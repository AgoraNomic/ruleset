package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableSet
import kotlinx.collections.immutable.persistentSetOf
import org.agoranomic.ruleset.history.HistoricalChangeEffect.*
import java.math.BigDecimal
import java.math.BigInteger

enum class HistoricalChangeEffect {
    ENACTMENT,
    REPEAL,
    TEXT_CHANGE,
    METADATA_CHANGE,
    UNKNOWN,
}

interface HistoricalChange {
    val changeCount: Int
    val effects: ImmutableSet<HistoricalChangeEffect>
    fun formatEffect(baseChangeNumber: Int): String
}

object HistoricalChanges {
    private fun effects(
        first: HistoricalChangeEffect,
        vararg rest: HistoricalChangeEffect,
    ): ImmutableSet<HistoricalChangeEffect> {
        return persistentSetOf(first, *rest)
    }

    private fun uncountedChange(
        value: String,
        effects: ImmutableSet<HistoricalChangeEffect>,
    ): HistoricalChange {
        return object : HistoricalChange {
            override val effects: ImmutableSet<HistoricalChangeEffect>
                get() = effects

            override val changeCount: Int
                get() = 0

            override fun formatEffect(baseChangeNumber: Int): String {
                return value
            }
        }
    }

    private fun countedManyChange(
        changeCount: Int,
        effects: ImmutableSet<HistoricalChangeEffect>,
        format: (List<Int>) -> String,
    ): HistoricalChange {
        require(changeCount > 0)

        return object : HistoricalChange {
            override val effects: ImmutableSet<HistoricalChangeEffect>
                get() = effects

            override val changeCount
                get() = changeCount

            override fun formatEffect(baseChangeNumber: Int): String {
                return format(List(changeCount) { baseChangeNumber + it })
            }
        }
    }

    private fun countedOnceChange(
        effects: ImmutableSet<HistoricalChangeEffect>,
        format: (Int) -> String,
    ): HistoricalChange {
        return countedManyChange(1, effects) { list -> format(list.single()) }
    }

    fun enactment() = uncountedChange("Enacted", effects(ENACTMENT))

    enum class InitialRuleMutability {
        MUTABLE, IMMUTABLE,
    }

    fun initialRule(mutability: InitialRuleMutability, initialId: BigInteger): HistoricalChange {
        val mutabilityString = when (mutability) {
            InitialRuleMutability.MUTABLE -> "mutable"
            InitialRuleMutability.IMMUTABLE -> "immutable"
        }

        return uncountedChange("initial $mutabilityString rule $initialId", effects(ENACTMENT))
    }

    fun countedAmendment() = countedOnceChange(effects(TEXT_CHANGE)) { "Amended ($it)" }
    fun uncountedAmendment() = uncountedChange("Amended", effects(TEXT_CHANGE))

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
    ) = uncountedChange(
        "Mutated${from?.let { " from MI=$it" } ?: ""}${to?.let { " to MI=$it" } ?: ""}",
        effects(TEXT_CHANGE),
    )

    fun renumbering() = uncountedChange("Renumbered", effects(METADATA_CHANGE))

    fun unchangedReenactment() = countedOnceChange(effects(ENACTMENT)) { "Re-enacted($it)" }
    fun changedReenactment() = countedOnceChange(effects(ENACTMENT, TEXT_CHANGE)) { "Re-enacted($it) and amended" }
    fun infectionAmendment() = countedOnceChange(effects(METADATA_CHANGE, TEXT_CHANGE)) { "Infected and amended($it)" }
    fun infection() = uncountedChange("Infected", effects(METADATA_CHANGE))
    fun retitiling() = uncountedChange("Retitled", effects(METADATA_CHANGE))
    fun repeal() = uncountedChange("Repeal", effects(REPEAL))

    fun powerChange(
        from: BigDecimal?,
        to: BigDecimal?,
    ) = uncountedChange(
        "Power changed${from?.let { " from $it" } ?: ""}${to?.let { " to $it" } ?: ""}",
        effects(METADATA_CHANGE),
    )

    fun committeeAssignment(committee: String) = uncountedChange(
        "Assigned to the $committee",
        effects(METADATA_CHANGE),
    )

    fun unknown() = uncountedChange("History unknown...", effects(UNKNOWN))
}
