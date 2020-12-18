package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*
import org.agoranomic.ruleset.history.*
import java.math.BigInteger
import java.time.LocalDate

interface ProposalDataMap {
    fun dataFor(proposalNumber: BigInteger): ProposalData?
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
    proposalDataMap: ProposalDataMap,
): HistoricalCause {
    val causeKind = topNode.keys.singleOrNull() ?: throw IllegalArgumentException("multiple cause kinds specified")
    val causeNode = topNode[causeKind] ?: error("")

    // Will throw if it doesn't exist
    val causeMap by lazy { causeNode.requireMap() }
    val causeContent by lazy { causeNode.requireValue().content }

    return when (causeKind) {
        "proposal" -> HistoricalCauses.proposal(
            causeNode.requireValue().content.toBigInteger().let {
                proposalDataMap.dataFor(it) ?: throw IllegalArgumentException("no data for proposal $it")
            }
        )
        "rule" -> HistoricalCauses.rule(RuleNumber(causeNode.requireValue().content.toBigInteger()))
        "convergence" -> HistoricalCauses.convergence(parseHistoricalCauseYaml(causeMap, proposalDataMap))
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
                    LocalDate.parse(topNode.getContent("and")),
                )
                else -> throw IllegalArgumentException("invalid date map: $topNode")
            }
        }

        is ParsedYamlNode.ListNode -> throw IllegalArgumentException("date cannot be a list")
        is ParsedYamlNode.NullNode -> throw IllegalArgumentException("date cannnot be null")
    }
}

private fun parseHistoryEntryYaml(topNode: ParsedYamlNode.MapNode, proposalDataMap: ProposalDataMap): HistoricalEntry {
    val change = parseHistoricalChangeYaml(topNode.getMap("change"))
    val cause = topNode.getOptMap("agent")?.let { parseHistoricalCauseYaml(it, proposalDataMap) }
    val date = parseHistoricalDate(topNode.getNode("date"))

    return HistoricalEntry(change, cause, date)
}

private fun parseCfjAnnotationNumber(number: String): CfjAnnotationNumber {
    val parts = number.split("-").map { it.toBigInteger() }.map { CfjNumber(it) }

    return when (parts.size) {
        1 -> CfjAnnotationNumber.Single(parts.single())
        2 -> CfjAnnotationNumber.Range(parts[0], parts[1])
        else -> throw IllegalArgumentException("Invalid CFJ number: $number")
    }
}

private fun parseRulesetAnnotationsYaml(topNode: ParsedYamlNode.ListNode): RuleAnnotations {
    return RuleAnnotations(topNode.values.map { it.requireMap() }.map { mapNode ->
        when {
            mapNode.containsKey("cfjs") -> {
                val cfjsNode = mapNode.getList("cfjs").values.single().requireMap()
                val text = mapNode.getContent("text")

                HistoricalCfjAnnotation(
                    number = parseCfjAnnotationNumber(cfjsNode.getContent("id")),
                    calledDate = run { // just to get the formatter to cooperate
                        cfjsNode
                            .getOptContent("called")
                            ?.let { it -> LocalDate.parse(it) }
                            ?.let { HistoricalDate.Known(it) }
                    },
                    finding = text,
                )
            }

            else -> throw IllegalArgumentException("Unknown annotation $topNode")
        }
    })
}

fun parseRuleStateYaml(yaml: String, proposalDataMap: ProposalDataMap): RuleState {
    val topNode = parseRawYaml(yaml).requireMap()
    val id = RuleNumber(topNode.getContent("id").toBigInteger())
    val title = topNode.getContent("name")
    val power = topNode.getContent("power").toBigDecimal()
    val text = topNode.getContent("text")

    val history = RuleHistory(
        topNode.getList("history").values.map { parseHistoryEntryYaml(it.requireMap(), proposalDataMap) }
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

