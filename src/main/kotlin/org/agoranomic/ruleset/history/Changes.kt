package org.agoranomic.ruleset.history

import java.math.BigDecimal
import java.math.BigInteger

interface HistoricalChange {
    val changeCount: Int
    fun formatEffect(baseChangeNumber: Int): String
}

private fun uncountedHistoricalChange(value: String): HistoricalChange {
    return object : HistoricalChange {
        override val changeCount: Int
            get() = 0

        override fun formatEffect(baseChangeNumber: Int): String {
            return value
        }
    }
}

private fun countedManyHistoricalChange(changeCount: Int, format: (List<Int>) -> String): HistoricalChange {
    require(changeCount > 0)

    return object : HistoricalChange {
        override val changeCount
            get() = changeCount

        override fun formatEffect(baseChangeNumber: Int): String {
            return format(List(changeCount) { baseChangeNumber + it })
        }
    }
}

private fun countedOnceHistoricalChange(format: (Int) -> String): HistoricalChange {
    return countedManyHistoricalChange(1) { list -> format(list.single()) }
}

fun enactmentHistoricalChange() = uncountedHistoricalChange("Enacted")

enum class InitialRuleMutability {
    MUTABLE, IMMUTABLE,
}

fun initialRuleHistoricalChange(mutability: InitialRuleMutability, initialId: BigInteger): HistoricalChange {
    val mutabilityString = when(mutability) {
        InitialRuleMutability.MUTABLE -> "mutable"
        InitialRuleMutability.IMMUTABLE -> "immutable"
    }

    return uncountedHistoricalChange("initial $mutabilityString rule $initialId")
}

fun countedAmendmentHistoricalChange() = countedOnceHistoricalChange { "Amended ($it)" }
fun uncountedAmendmentHistoricalChange() = uncountedHistoricalChange("Amended")

sealed class MutabilityIndex {
    data class Numeric(val value: BigDecimal) : MutabilityIndex() {
        override fun toString(): String {
            return value.toString()
        }
    }

    object Unanimity : MutabilityIndex() {
        override fun toString(): String {
            return "unanimity"
        }
    }
}

fun mutationHistoricalChange(
    from: MutabilityIndex?,
    to: MutabilityIndex?,
) = uncountedHistoricalChange("Mutated${from?.let { " from MI=$it" } ?: ""}${to?.let { " to MI=$it" } ?: ""}")

fun renumberingHistoricalChange() = uncountedHistoricalChange("Renumbered")

fun unchangedReenactmentHistoricalChange() = countedOnceHistoricalChange { "Re-enacted($it)" }
fun changedReenactmentHistoricalChange() = countedOnceHistoricalChange { "Re-enacted($it) and amended" }
fun infectionAmendmentHistoricalChange() = countedOnceHistoricalChange { "Infected and amended($it)" }
fun infectionHistoricalChange() = uncountedHistoricalChange("Infected")
fun retitilingHistoricalChange() = uncountedHistoricalChange("Retitled")
fun repealHistoricalChange() = uncountedHistoricalChange("Repeal")

fun powerChangeHistoricalChange(
    from: BigDecimal?,
    to: BigDecimal?,
) = uncountedHistoricalChange("Power changed${from?.let { " from $it" } ?: ""}${to?.let{ " to $it" } ?: ""}")

fun committeeAssignmentHistoricalChange(committee: String) = uncountedHistoricalChange("Assigned to the $committee")
fun unknownHistoricalChange() = uncountedHistoricalChange("History unknown...")
