package org.agoranomic.ruleset.report

import kotlinx.collections.immutable.persistentListOf
import org.agoranomic.ruleset.*
import org.agoranomic.ruleset.history.HistoricalDate
import org.agoranomic.ruleset.history.ProposalNumber
import java.math.BigInteger
import java.time.LocalDate
import java.time.format.DateTimeFormatter

data class ProposalStatistics(
    val highestProposal: ProposalNumber,
)

data class ReadableReportConfig(
    val entityKind: String,
    val maxLineLength: Int,
    val includeHistory: Boolean,
    val includeAnnotations: Boolean,
)

private fun formatTableOfContents(ruleset: CategorizedRulesetState, entityKind: String): String {
    return ruleset
        .categories
        .map {
            it to ruleset.rulesIn(it.id)
        }
        .joinToString("") { (category, rules) ->
            rules.joinToString("\n", prefix = "${category.readableName}\n", postfix = "\n") {
                "   * $entityKind ${it.id.toString().padStart(4, ' ')}: ${it.title}"
            }
        }
}

private fun formatPowers(ruleset: RulesetState): String {
    val maxScale = ruleset.map { it.power }.maxOf { it.scale() }

    return ruleset
        .groupBy { it.power.setScale(maxScale).stripTrailingZeros() }
        .mapValues { (_, v) -> v.count() }
        .asIterable()
        .sortedBy { it.key }
        .let {
            val maxCountLength = it.maxOf { it.value.toString().length }

            it.joinToString("\n") { (power, count) ->
                "${count.toString().padEnd(maxCountLength, ' ')} with Power=$power"
            }
        }
}

private fun String.splitWordsToMaxLineLength(
    maxLineLength: Int,
    firstLineIndent: String,
    followingIndents: String,
): List<String> {
    require(maxLineLength > 0)

    return lineSequence()
        .joinToString(" ") { it.trim() }
        .split(" ")
        .fold(persistentListOf<String>()) { acc, next ->
            when {
                acc.isEmpty() -> acc.add(firstLineIndent + next)
                acc.last().length + next.length + 1 > maxLineLength -> acc.add(followingIndents + next)
                else -> acc.set(acc.size - 1, acc.last() + " " + next)
            }
        }
}

private fun String.splitWordsToMaxLineLength(maxLineLength: Int, indent: String) =
    splitWordsToMaxLineLength(maxLineLength = maxLineLength, firstLineIndent = indent, followingIndents = indent)

private fun String.splitWordsToMaxLineLength(maxLineLength: Int) =
    splitWordsToMaxLineLength(maxLineLength = maxLineLength, firstLineIndent = "", followingIndents = "")

private val AGORA_BIRTHDAY = LocalDate.of(1993, 6, 30)

private fun formatLocalDate(date: LocalDate): String {
    return if (date == AGORA_BIRTHDAY) "Agora's birth" else date.format(DateTimeFormatter.ofPattern("dd MMM uuuu"))
}

private fun formatDate(date: HistoricalDate): String {
    return when (date) {
        is HistoricalDate.Known -> formatLocalDate(date.date)
        is HistoricalDate.Around -> "around " + formatLocalDate(date.date)
        is HistoricalDate.Between -> "between ${formatLocalDate(date.start)} and ${formatLocalDate(date.end)}"
    }
}

private fun formatHistory(history: RuleHistory, maxLineLength: Int): String {
    val changeNumbers = history.entries
        .runningFold(1) { acc, next -> acc + next.change.changeCount }
        .dropLast(1)

    return changeNumbers
        .zip(history.entries) { baseChangeNumber, entry ->
            val raw = entry.change.formatEffect(baseChangeNumber) +
                    (entry.cause?.causeString?.let { " by $it" } ?: "") +
                    ", ${formatDate(entry.date)}"

            raw.splitWordsToMaxLineLength(maxLineLength, firstLineIndent = "", followingIndents = "   ")
        }
        .flatten()
        .joinToString("\n", postfix = "\n")
}

private fun formatAnnotations(
    annotations: RuleAnnotations,
    maxLineLength: Int,
): String {
    return annotations
        .map { annotation ->
            val raw = when (annotation) {
                is HistoricalCfjAnnotation -> {
                    annotation.blocks.joinToString(", ") { block ->
                        "CFJ " + when (val number = block.number) {
                            is CfjAnnotationNumber.Single -> number.number.toString()
                            is CfjAnnotationNumber.Range -> "${number.first}-${number.last}"
                        } + (block.calledDate?.let { " (called ${formatDate(it)})" } ?: "")
                    } + ": " + annotation.finding
                }
            }

            raw.splitWordsToMaxLineLength(maxLineLength, firstLineIndent = "", followingIndents = "   ")
        }
        .flatten()
        .joinToString("\n")
        .let { if (it.isNotEmpty()) it + "\n" else it }
}

private fun formatRuleset(
    rulesetState: CategorizedRulesetState,
    config: ReadableReportConfig,
): String {
    val equalLine = "=".repeat(config.maxLineLength)
    val dashLine = "-".repeat(config.maxLineLength)

    return rulesetState.categories.joinToString("") { category ->
        equalLine +
                "\n" +
                category.readableName +
                "\n" +
                category
                    .readableDescription
                    .splitWordsToMaxLineLength(config.maxLineLength, indent = "   ")
                    .joinToString("\n") +
                "\n" +
                dashLine +
                "\n" +
                rulesetState.rulesIn(category.id).joinToString("") { rule ->
                    val revisionNumber = rule.history.entries.sumBy { it.change.changeCount }

                    "${config.entityKind} ${rule.id}/$revisionNumber (Power=${rule.power})\n" +
                            rule.title +
                            "\n\n" +
                            rule
                                .text
                                .lines()
                                .dropLastWhile { it.isBlank() }
                                .joinToString("\n") { it.prependIndent("      ") } +
                            "\n" +
                            (if (config.includeHistory) {
                                "\nHistory:\n\n" + formatHistory(rule.history, maxLineLength = config.maxLineLength)
                            } else "") +
                            (if (config.includeAnnotations) {
                                "\nAnnotations:\n" + (rule.annotations?.let {
                                    formatAnnotations(it, maxLineLength = config.maxLineLength)
                                } ?: "")
                            } else "") +
                            "\n$dashLine\n"
                }
    }
}

private fun Iterable<RuleNumber>.requireIntegralMax(): BigInteger {
    return maxOf {
        when (it) {
            is RuleNumber.Integral -> it
            is RuleNumber.Textual -> {
                throw IllegalArgumentException(
                    "all rule numbers must be integral for statistics, but $it was not"
                )
            }
        }.value
    }
}

fun formatReadable(
    template: String,
    headerContent: String?,
    config: ReadableReportConfig,
    rulesetState: CategorizedRulesetState,
    proposalStatistics: ProposalStatistics?,
): String {
    val renderedRules = rulesetState.categorizedRules
    require(renderedRules.isNotEmpty())

    return template
        .let { string ->
            if (string.contains("{header}"))
                string.replace(
                    "{header}",
                    requireNotNull(headerContent) { "Header was used but not provided" }
                )
            else
                string
        }
        .replace("{num}", renderedRules.count().toString())
        .let { string ->
            if (string.contains("{her}"))
                string.replace("{her}", renderedRules.ruleNumbers.requireIntegralMax().toString())
            else
                string
        }
        .let { string ->
            if (string.contains("{hr}"))
                string.replace("{hr}", rulesetState.ruleset.ruleNumbers.requireIntegralMax().toString())
            else
                string
        }
        .let {
            if (it.contains("{hp}"))
                it.replace("{hp}", requireNotNull(proposalStatistics).highestProposal.readable)
            else
                it
        }
        .replace("{line}", "-".repeat(config.maxLineLength))
        .replace("{toc}", formatTableOfContents(rulesetState, entityKind = config.entityKind))
        .replace("{powers}", formatPowers(renderedRules))
        .replace("{ruleset}", formatRuleset(rulesetState, config))
}
