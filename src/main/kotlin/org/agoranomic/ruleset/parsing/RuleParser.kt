package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*
import org.agoranomic.ruleset.history.*
import java.time.LocalDate

interface YamlProposalDataMap {
    fun dataFor(proposalSpecification: String): ProposalData?
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
            if (changeNode.getOptValue("uncounted")?.content?.toLowerCase()?.toBoolean() == true)
                HistoricalChanges.uncountedAmendment()
            else
                HistoricalChanges.countedAmendment()
        }
        "infection-amendment" -> HistoricalChanges.infectionAmendment()
        "infection" -> HistoricalChanges.infection()
        "retitling" -> HistoricalChanges.retitling()
        "repeal" -> HistoricalChanges.repeal()
        "power-change" -> {
            val from = changeNode.getOptValue("old-power")?.content?.let { it.toBigDecimal() }
            val to = changeNode.getOptValue("new-power")?.content?.let { it.toBigDecimal() }

            HistoricalChanges.powerChange(from = from, to = to)
        }
        "committee-assignment" -> HistoricalChanges.committeeAssignment(changeNode.getContent("committee"))
        "unknown" -> HistoricalChanges.unknown()
        else -> throw IllegalArgumentException("Unknown rule change type $changeType")
    }

private fun parseHistoricalCauseYaml(
    topNode: ParsedYamlNode.MapNode,
    proposalDataMap: YamlProposalDataMap,
    ruleNumberResolver: RuleNumberResolver,
): HistoricalCause {
    val causeKind = topNode.keys.singleOrNull() ?: throw IllegalArgumentException("multiple cause kinds specified")
    val causeNode = topNode[causeKind] ?: error("")

    // Will throw if it doesn't exist
    val causeMap by lazy { causeNode.requireMap() }
    val causeContent by lazy { causeNode.requireValue().content }

    return when (causeKind) {
        "proposal" -> HistoricalCauses.proposal(
            causeNode.requireValue().content.let { numberString ->
                proposalDataMap.dataFor(numberString)
                    ?: throw IllegalArgumentException("No data for proposal $numberString")
            }
        )
        "rule" -> HistoricalCauses.rule(ruleNumberResolver.resolve(causeContent))
        "convergence" -> HistoricalCauses.convergence(
            parseHistoricalCauseYaml(causeMap, proposalDataMap, ruleNumberResolver)
        )
        "cleaning" -> HistoricalCauses.cleaning(causeMap.getContent("by"))
        "refiling" -> HistoricalCauses.refiling(causeMap.getContent("by"))
        "ratification" -> HistoricalCauses.ratification(causeMap.getContent("document"))
        "decree" -> HistoricalCauses.decree(causeNode.requireValue().content)
        "tournament_init" -> HistoricalCauses.tournamentInit(
            tournament = causeMap.getContent("name"),
            initiator = causeMap.getContent("person"),
        )
        "tournament_change" -> HistoricalCauses.tournamentChange(
            tournament = causeMap.getContent("name"),
            agent = causeMap.getContent("person"),
        )
        "tournament_end" -> HistoricalCauses.tournamentEnd(
            tournament = causeMap.getContent("name"),
            agent = causeMap.getContent("person"),
        )
        "person" -> HistoricalCauses.person(causeContent)
        "rulebending" -> HistoricalCauses.rulebending(magister = causeMap.getContent("magister"))
        else -> throw IllegalArgumentException("Unknown cause $causeKind")
    }
}

private fun parseHistoricalDate(topNode: ParsedYamlNode): HistoricalDate {
    return when (topNode) {
        is ParsedYamlNode.ValueNode -> {
            return HistoricalDate.Known(LocalDate.parse(topNode.content))
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
    proposalDataMap: YamlProposalDataMap,
    ruleNumberResolver: RuleNumberResolver,
): HistoricalEntry {
    val change = parseHistoricalChangeYaml(topNode.getMap("change"))

    val cause =
        topNode.getOptMap("agent")?.let { parseHistoricalCauseYaml(it, proposalDataMap, ruleNumberResolver) }

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

fun parseRuleStateYaml(
    yaml: String,
    proposalDataMap: YamlProposalDataMap,
    ruleNumberResolver: RuleNumberResolver,
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
            parseHistoryEntryYaml(it.requireMap(), proposalDataMap, ruleNumberResolver)
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

