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
    enum class InitialRuleMutability {
        MUTABLE, IMMUTABLE,
    }

    sealed class MutabilityIndex {
        data class Numeric(val value: BigDecimal) : MutabilityIndex() {
            override fun toString(): String {
                return value.toString()
            }
        }

        data object Unanimity : MutabilityIndex() {
            override fun toString(): String {
                return "unanimity"
            }
        }
    }

    fun initialRule(mutability: InitialRuleMutability, initialId: BigInteger): HistoricalChange =
        InitialRule(mutability = mutability, initialId = initialId)

    fun mutation(from: MutabilityIndex?, to: MutabilityIndex?): HistoricalChange = Mutation(from = from, to = to)
    fun powerChange(from: BigDecimal?, to: BigDecimal?): HistoricalChange = PowerChange(from = from, to = to)
    fun committeeAssignment(committee: String): HistoricalChange = CommitteeAssignment(committee = committee)

    fun enactment(): HistoricalChange = Enactment
    fun countedAmendment(): HistoricalChange = CountedAmendment
    fun uncountedAmendment(): HistoricalChange = UncountedAmendment
    fun nullAmendment(): HistoricalChange = NullAmendment
    fun renumbering(): HistoricalChange = Renumbering
    fun unchangedReenactment(): HistoricalChange = UnchangedReenactment
    fun changedReenactment(): HistoricalChange = ChangedReenactment
    fun infectionAmendment(): HistoricalChange = InfectionAmendment
    fun infection(): HistoricalChange = Infection

    fun retitling(oldTitle: String?, newTitle: String?): HistoricalChange =
        Retitling(oldTitle = oldTitle, newTitle = newTitle)

    fun repeal(): HistoricalChange = Repeal
    fun authorityVanished(): HistoricalChange = AuthorityVanished
    fun unknown(): HistoricalChange = Unknown

    private fun effects(
        first: HistoricalChangeEffect,
        vararg rest: HistoricalChangeEffect,
    ): ImmutableSet<HistoricalChangeEffect> {
        return persistentSetOf(first, *rest)
    }

    abstract class CountedOnceChange(
        private val formatter: (Int) -> String,
        final override val effects: ImmutableSet<HistoricalChangeEffect>,
    ) : HistoricalChange {
        final override val changeCount: Int
            get() = 1

        final override fun formatEffect(baseChangeNumber: Int): String {
            return formatter(baseChangeNumber)
        }
    }

    abstract class UncountedHistoricalChange(
        private val value: String,
        override val effects: ImmutableSet<HistoricalChangeEffect>,
    ) : HistoricalChange {
        override val changeCount: Int
            get() = 0

        override fun formatEffect(baseChangeNumber: Int): String {
            return value
        }
    }

    // The below must be types in order to ensure that equality works correctly

    private data class InitialRule(val mutability: InitialRuleMutability, val initialId: BigInteger) :
        UncountedHistoricalChange(
            when (mutability) {
                InitialRuleMutability.MUTABLE -> "mutable"
                InitialRuleMutability.IMMUTABLE -> "immutable"
            }.let { mutabilityString -> "Initial $mutabilityString rule $initialId" },
            effects(ENACTMENT),
        )

    data class Mutation(val from: MutabilityIndex?, val to: MutabilityIndex?) :
        UncountedHistoricalChange(
            "Mutated${from?.let { " from MI=$it" } ?: ""}${to?.let { " to MI=$it" } ?: ""}",
            effects(TEXT_CHANGE),
        )


    data class PowerChange(val from: BigDecimal?, val to: BigDecimal?) :
        UncountedHistoricalChange(
            "Power changed${from?.let { " from $it" } ?: ""}${to?.let { " to $it" } ?: ""}",
            effects(METADATA_CHANGE),
        )

    data class Retitling(val oldTitle: String?, val newTitle: String?) :
        UncountedHistoricalChange(
            "Retitled${oldTitle?.let { " from \'$it\'" } ?: ""}${newTitle?.let { " to \'$it\'" } ?: ""}",
            effects(METADATA_CHANGE),
        )

    private data class CommitteeAssignment(val committee: String) :
        UncountedHistoricalChange(
            "Assigned to the $committee",
            effects(METADATA_CHANGE),
        )

    private data object Enactment : UncountedHistoricalChange("Enacted", effects(ENACTMENT))
    private data object Renumbering : UncountedHistoricalChange("Renumbered", effects(METADATA_CHANGE))
    private data object Infection : UncountedHistoricalChange("Infected", effects(METADATA_CHANGE))
    private data object Repeal : UncountedHistoricalChange("Repealed", effects(REPEAL))
    private data object CountedAmendment : CountedOnceChange({ "Amended($it)" }, effects(TEXT_CHANGE))
    private data object UncountedAmendment : UncountedHistoricalChange("Amended", effects(TEXT_CHANGE))
    private data object NullAmendment : CountedOnceChange({ "Null-amended($it)" }, effects(TEXT_CHANGE))
    private data object Unknown : UncountedHistoricalChange("History unknown...", effects(UNKNOWN))
    private data object UnchangedReenactment : CountedOnceChange({ "Re-enacted($it)" }, effects(ENACTMENT))

    private data object ChangedReenactment :
        CountedOnceChange({ "Re-enacted($it) and amended" }, effects(ENACTMENT, TEXT_CHANGE))

    private data object InfectionAmendment :
        CountedOnceChange({ "Infected and amended($it)" }, effects(METADATA_CHANGE, TEXT_CHANGE))

    private data object AuthorityVanished : UncountedHistoricalChange("Disappeared as authority removed", effects(REPEAL))
}
