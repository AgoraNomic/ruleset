package org.agoranomic.ruleset.history

import java.math.BigDecimal
import java.math.BigInteger

interface HistoricalChange {
    val changeCount: Int
    fun formatEffect(baseChangeNumber: Int): String
}

object HistoricalChanges {
    private fun uncountedChange(value: String): HistoricalChange {
        return object : HistoricalChange {
            override val changeCount: Int
                get() = 0

            override fun formatEffect(baseChangeNumber: Int): String {
                return value
            }
        }
    }

    private fun countedManyChange(changeCount: Int, format: (List<Int>) -> String): HistoricalChange {
        require(changeCount > 0)

        return object : HistoricalChange {
            override val changeCount
                get() = changeCount

            override fun formatEffect(baseChangeNumber: Int): String {
                return format(List(changeCount) { baseChangeNumber + it })
            }
        }
    }

    private fun countedOnceChange(format: (Int) -> String): HistoricalChange {
        return countedManyChange(1) { list -> format(list.single()) }
    }

    fun enactment() = uncountedChange("Enacted")

    enum class InitialRuleMutability {
        MUTABLE, IMMUTABLE,
    }

    fun initialRule(mutability: InitialRuleMutability, initialId: BigInteger): HistoricalChange {
        val mutabilityString = when (mutability) {
            InitialRuleMutability.MUTABLE -> "mutable"
            InitialRuleMutability.IMMUTABLE -> "immutable"
        }

        return uncountedChange("initial $mutabilityString rule $initialId")
    }

    fun countedAmendment() = countedOnceChange { "Amended ($it)" }
    fun uncountedAmendment() = uncountedChange("Amended")

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

    fun mutation(
        from: MutabilityIndex?,
        to: MutabilityIndex?,
    ) = uncountedChange("Mutated${from?.let { " from MI=$it" } ?: ""}${to?.let { " to MI=$it" } ?: ""}")

    fun renumbering() = uncountedChange("Renumbered")

    fun unchangedReenactment() = countedOnceChange { "Re-enacted($it)" }
    fun changedReenactment() = countedOnceChange { "Re-enacted($it) and amended" }
    fun infectionAmendment() = countedOnceChange { "Infected and amended($it)" }
    fun infection() = uncountedChange("Infected")
    fun retitiling() = uncountedChange("Retitled")
    fun repeal() = uncountedChange("Repeal")

    fun powerChange(
        from: BigDecimal?,
        to: BigDecimal?,
    ) = uncountedChange("Power changed${from?.let { " from $it" } ?: ""}${to?.let { " to $it" } ?: ""}")

    fun committeeAssignment(committee: String) = uncountedChange("Assigned to the $committee")
    fun unknown() = uncountedChange("History unknown...")
}
