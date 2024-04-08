package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*
import org.agoranomic.ruleset.history.*
import java.time.LocalDate

interface YamlProposalDataMap {
    fun dataFor(proposalSpecification: String, nameResolver: CauseNameResolver): ProposalData?
}

class ProposalDataParseException : Exception {
    companion object {
        private fun messageForSpecification(proposalSpecification: String) =
            "Error while parsing proposal $proposalSpecification"

        fun forProposalSpecification(specification: String) =
            ProposalDataParseException(messageForSpecification(specification))

        fun forProposalSpecification(specification: String, cause: Exception) =
            ProposalDataParseException(messageForSpecification(specification), cause)
    }

    constructor(message: String) : super(message)
    constructor(message: String, cause: Exception) : super(message, cause)
}

private fun parseMutabilityIndexYaml(index: String): HistoricalChanges.MutabilityIndex {
    if (index == "unanimity") return HistoricalChanges.MutabilityIndex.Unanimity
    return HistoricalChanges.MutabilityIndex.Numeric(index.toBigDecimal())
}

private fun parseHistoricalChangeYaml(changeNode: ParsedYamlNode.MapNode) =
    when (val changeType = changeNode.getContent("type")) {
        "enactment" -> HistoricalChanges.enactment()
        "initial" -> {
            val mutability = when (val mutability = changeNode.getContent("mutability")) {
                "mutable" -> HistoricalChanges.InitialRuleMutability.MUTABLE
                "immutable" -> HistoricalChanges.InitialRuleMutability.IMMUTABLE
                else -> throw IllegalArgumentException("unknown mutability $mutability")
            }

            val initialId = changeNode.getContent("id").toBigInteger()

            HistoricalChanges.initialRule(mutability = mutability, initialId = initialId)
        }
        "mutation" -> {
            val from = changeNode.getOptValue("old-mi")?.content?.let { parseMutabilityIndexYaml(it) }
            val to = changeNode.getOptValue("new-mi")?.content?.let { parseMutabilityIndexYaml(it) }

            HistoricalChanges.mutation(from = from, to = to)
        }
        "renumbering" -> HistoricalChanges.renumbering()
        "reenactment" -> {
            if (changeNode.containsKey("unchanged"))
                HistoricalChanges.unchangedReenactment()
            else
                HistoricalChanges.changedReenactment()
        }
        "amendment" -> {
            if (changeNode.getOptValue("uncounted")?.content?.lowercase()?.toBoolean() == true)
                HistoricalChanges.uncountedAmendment()
            else
                HistoricalChanges.countedAmendment()
        }
        "null-amendment" -> HistoricalChanges.nullAmendment()
        "infection-amendment" -> HistoricalChanges.infectionAmendment()
        "infection" -> HistoricalChanges.infection()
        "retitling" -> HistoricalChanges.retitling(
            oldTitle = changeNode.getOptValue("old-title")?.content,
            newTitle = changeNode.getOptValue("new-title")?.content,
        )
        "repeal" -> HistoricalChanges.repeal()
        "power-change" -> {
            val from = changeNode.getOptValue("old-power")?.content?.let { it.toBigDecimal() }
            val to = changeNode.getOptValue("new-power")?.content?.let { it.toBigDecimal() }

            HistoricalChanges.powerChange(from = from, to = to)
        }
        "committee-assignment" -> HistoricalChanges.committeeAssignment(changeNode.getContent("committee"))
        "authority-vanished" -> HistoricalChanges.authorityVanished()
        "unknown" -> HistoricalChanges.unknown()
        else -> throw IllegalArgumentException("Unknown rule change type $changeType")
    }

private fun parseHistoricalCauseYaml(
    topNode: ParsedYamlNode.MapNode,
    proposalDataMap: YamlProposalDataMap?,
    ruleNumberResolver: RuleNumberResolver,
    nameResolver: CauseNameResolver,
): HistoricalCause {
    val causeKind = topNode.keys.singleOrNull() ?: throw IllegalArgumentException("multiple cause kinds specified")
    val causeNode = topNode[causeKind] ?: error("")

    // Will throw if it doesn't exist
    val causeMap by lazy { causeNode.requireMap() }
    val causeContent by lazy { causeNode.requireValue().content }

    return when (causeKind) {
        "proposal" -> {
            if (proposalDataMap == null) {
                error("Attempt to use proposal cause where no proposal data is available")
            }

            HistoricalCauses.proposal(
                causeNode.requireValue().content.let { numberString ->
                    proposalDataMap.dataFor(proposalSpecification = numberString, nameResolver = nameResolver)
                        ?: throw IllegalArgumentException("No data for proposal $numberString")
                }
            )
        }

        "rule" -> HistoricalCauses.rule(ruleNumberResolver.resolve(causeContent))
        "convergence" -> HistoricalCauses.convergence(
            parseHistoricalCauseYaml(
                topNode = causeMap,
                proposalDataMap = proposalDataMap,
                ruleNumberResolver = ruleNumberResolver,
                nameResolver = nameResolver,
            ),
        )
        "cleaning" -> HistoricalCauses.cleaning(nameResolver.resolveInformalCauseName(causeMap.getContent("by")))
        "refiling" -> HistoricalCauses.refiling(nameResolver.resolveInformalCauseName(causeMap.getContent("by")))
        "ratification" -> HistoricalCauses.ratification(causeMap.getContent("document"))
        "decree" -> HistoricalCauses.decree(nameResolver.resolveInformalCauseName(causeNode.requireValue().content))
        "tournament_init" -> HistoricalCauses.tournamentInit(
            tournament = causeMap.getContent("name"),
            initiator = nameResolver.resolveInformalCauseName(causeMap.getContent("person")),
        )
        "tournament_change" -> HistoricalCauses.tournamentChange(
            tournament = causeMap.getContent("name"),
            agent = nameResolver.resolveInformalCauseName(causeMap.getContent("person")),
        )
        "tournament_end" -> HistoricalCauses.tournamentEnd(
            tournament = causeMap.getContent("name"),
            agent = nameResolver.resolveInformalCauseName(causeMap.getContent("person")),
        )
        "person" -> HistoricalCauses.person(nameResolver.resolveFormalCauseName(causeContent))
        "rulebending" -> HistoricalCauses.rulebending(
            magister = nameResolver.resolveInformalCauseName(causeMap.getContent("magister")),
        )
        "experiment" -> HistoricalCauses.deviceExperiment(
            id = causeMap.getContent("id").toInt(),
            madEngineer = nameResolver.resolveInformalCauseName(causeMap.getContent("engineer")),
        )
        else -> throw IllegalArgumentException("Unknown cause $causeKind")
    }
}

private fun parseHistoricalDate(topNode: ParsedYamlNode): HistoricalDate {
    return when (topNode) {
        is ParsedYamlNode.ValueNode -> {
            return if (topNode.content == "unknown")
                HistoricalDate.Unknown
            else
                HistoricalDate.Known(LocalDate.parse(topNode.content))
        }

        is ParsedYamlNode.MapNode -> {
            when {
                topNode.containsKey("around") -> HistoricalDate.Around(LocalDate.parse(topNode.getContent("around")))
                topNode.containsKey("between") -> HistoricalDate.Between(
                    LocalDate.parse(topNode.getContent("between")),
                    LocalDate.parse(
                        topNode.getOptContent("and")
                            ?: throw IllegalArgumentException("Date that contains a begin node should have an and node")
                    ),
                )
                else -> throw IllegalArgumentException("invalid date map: $topNode")
            }
        }

        is ParsedYamlNode.ListNode -> throw IllegalArgumentException("date cannot be a list")
        is ParsedYamlNode.NullNode -> throw error("NullNode should not have been passed into parseHistoricalDate")
    }
}

private fun parseHistoryEntryYaml(
    topNode: ParsedYamlNode.MapNode,
    proposalDataMap: YamlProposalDataMap?,
    ruleNumberResolver: RuleNumberResolver,
    nameResolver: CauseNameResolver,
): HistoricalEntry {
    val change = parseHistoricalChangeYaml(topNode.getMap("change"))

    val cause = topNode.getOptMap("agent")?.let {
        parseHistoricalCauseYaml(
            topNode = it,
            proposalDataMap = proposalDataMap,
            ruleNumberResolver = ruleNumberResolver,
            nameResolver = nameResolver,
        )
    }

    val date = parseHistoricalDate(topNode.getNode("date"))

    return HistoricalEntry(change, cause, date)
}

private fun parseCfjAnnotationNumber(number: String): CfjAnnotationNumber {
    val parts =
        number
            .split("-")
            .map { it.toBigIntegerOrNull() ?: throw IllegalArgumentException("Illegal CFJ number part: $it") }
            .map { CfjNumber(it) }

    return when (parts.size) {
        1 -> CfjAnnotationNumber.Single(parts.single())
        2 -> CfjAnnotationNumber.Range(parts[0], parts[1])
        else -> throw IllegalArgumentException("Invalid CFJ number: $number")
    }
}

private fun parseCfjAnnotationCaseBlock(topNode: ParsedYamlNode.MapNode): CfjAnnotationCaseBlock {
    return CfjAnnotationCaseBlock(
        number = parseCfjAnnotationNumber(topNode.getContent("id")),
        calledDate = topNode.getOptNode("called")?.let { parseHistoricalDate(it) },
    )
}

private fun parseRulesetAnnotationsYaml(topNode: ParsedYamlNode.ListNode): RuleAnnotations {
    return RuleAnnotations(topNode.values.map { it.requireMap() }.map { mapNode ->
        when {
            mapNode.containsKey("cfjs") -> {
                val cfjsNode = mapNode.getList("cfjs")
                val caseBlocks = cfjsNode.values.map { parseCfjAnnotationCaseBlock(it.requireMap()) }

                val text = mapNode.getContent("text")

                HistoricalCfjAnnotation(
                    blocks = caseBlocks,
                    finding = text.trim(),
                )
            }

            else -> throw IllegalArgumentException("Unknown annotation $topNode")
        }
    })
}

interface CauseNameResolver {
    /**
     * Returns the resolved name for an informal cause (such as the author of a proposal, or the person who did a
     * cleaning, or the person who gave a decree).
     */
    fun resolveInformalCauseName(name: String): String

    /**
     * Returns the resolved name for a formal cause (i.e. when using the person: cause attribute, such as for
     * regulations).
     */
    fun resolveFormalCauseName(name: String): String

    data object Identity : CauseNameResolver {
        override fun resolveInformalCauseName(name: String): String {
            return name
        }

        override fun resolveFormalCauseName(name: String): String {
            return name
        }
    }
}

fun parseRuleStateYaml(
    yaml: String,
    proposalDataMap: YamlProposalDataMap?,
    ruleNumberResolver: RuleNumberResolver,
    nameResolver: CauseNameResolver,
): RuleState {
    val topNode = parseRawYaml(yaml).requireMap()

    val id = ruleNumberResolver.resolve(topNode.getContent("id"))
    val title = topNode.getContent("name")

    val power =
        topNode
            .getOptContent("power")
            ?.let {
                it.toBigDecimalOrNull()
                    ?: throw IllegalArgumentException("Rule power should be a decimal number, got $it")
            }

    val text = topNode.getContent("text")

    val history = RuleHistory(
        topNode.getList("history").values.map {
            parseHistoryEntryYaml(
                topNode = it.requireMap(),
                proposalDataMap = proposalDataMap,
                ruleNumberResolver = ruleNumberResolver,
                nameResolver = nameResolver,
            )
        }
    )

    val annotations = topNode.getOptList("annotations")?.let { parseRulesetAnnotationsYaml(it) }

    return RuleState(
        id,
        title = title,
        power = power,
        text = text,
        history = history,
        annotations = annotations,
    )
}
