package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*
import org.agoranomic.ruleset.history.*
import java.math.BigInteger
import java.time.LocalDate
import kotlin.IllegalArgumentException

interface ProposalDataMap {
    fun dataFor(proposalNumber: BigInteger): ProposalData?
}

private fun parseMutabilityIndexYaml(index: String): MutabilityIndex {
    if (index == "unanimity") return MutabilityIndex.Unanimity
    return MutabilityIndex.Numeric(index.toBigDecimal())
}

private fun parseHistoricalChangeYaml(changeNode: ParsedYamlNode.MapNode) =
    when (val changeType = changeNode.getContent("type")) {
        "enacted" -> enactmentHistoricalChange()
        "initial" -> {
            val mutability = when (val mutability = changeNode.getContent("mutability")) {
                "mutable" -> InitialRuleMutability.MUTABLE
                "immutable" -> InitialRuleMutability.IMMUTABLE
                else -> throw IllegalArgumentException("unknown mutability $mutability")
            }

            val initialId = changeNode.getContent("id").toBigInteger()

            initialRuleHistoricalChange(mutability = mutability, initialId = initialId)
        }
        "mutation" -> {
            val from = changeNode.getOptValue("old-mi")?.content?.let { parseMutabilityIndexYaml(it) }
            val to = changeNode.getOptValue("new-mi")?.content?.let { parseMutabilityIndexYaml(it) }

            mutationHistoricalChange(from = from, to = to)
        }
        "renumbering" -> renumberingHistoricalChange()
        "re-enactment" -> {
            if (changeNode.containsKey("unchanged"))
                unchangedReenactmentHistoricalChange()
            else
                changedReenactmentHistoricalChange()
        }
        "amendment" -> {
            if (changeNode.getOptValue("uncounted")?.content?.toLowerCase()?.toBoolean() == true)
                uncountedAmendmentHistoricalChange()
            else
                countedAmendmentHistoricalChange()
        }
        "infection-amendment" -> infectionAmendmentHistoricalChange()
        "infection" -> infectionHistoricalChange()
        "retitling" -> retitilingHistoricalChange()
        "repeal" -> repealHistoricalChange()
        "power-change" -> {
            val from = changeNode.getOptValue("old-power")?.content?.let { it.toBigDecimal() }
            val to = changeNode.getOptValue("new-power")?.content?.let { it.toBigDecimal() }

            powerChangeHistoricalChange(from = from, to = to)
        }
        "committee-assignment" -> committeeAssignmentHistoricalChange(changeNode.getContent("committee"))
        "unknown" -> unknownHistoricalChange()
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
        "rule" -> HistoricalCauses.rule(causeNode.requireValue().content.toBigInteger())
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
    val parts = number.split("-").map { it.toBigInteger() }

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
    val id = topNode.getContent("id").toBigInteger()
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

