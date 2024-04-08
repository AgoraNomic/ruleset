package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableList
import org.agoranomic.ruleset.RuleHistory
import java.math.BigDecimal
import java.time.LocalDate

sealed class RuleHistoryValidationResult {
    data object Valid : RuleHistoryValidationResult()

    sealed class Invalid(val readableMessage: String) : RuleHistoryValidationResult() {
        data object EmptyHistory : Invalid("empty history")
        data object InitialNotEnactment : Invalid("initial change is not an enactment")
        data object Repealed : Invalid("rule was repealed and not re-enacted")
        data object DoubleEnactment : Invalid("two enactments in a row")
        data object NoReenactmentAfterRepeal : Invalid("rule was not re-enacted after a repeal")

        data class DescendingDate(val first: LocalDate, val second: LocalDate) : Invalid(
            "date $second is before date $first",
        )

        data class UnknownRulePower(val proposalNumber: ProposalNumber) : Invalid(
            "proposal ${proposalNumber.readable} applied to rule of unknown power"
        )

        data class InsufficientPower(
            val proposalNumber: ProposalNumber,
            val proposalPower: BigDecimal,
            val rulePower: BigDecimal
        ) : Invalid(
            "proposal ${proposalNumber.readable} of power $proposalPower applied to rule of greater power $rulePower"
        )

        data object UnknownTargetPower : Invalid("rule changed to have unknown power")

        data class InsufficientMutationPower(
            val proposalNumber: ProposalNumber,
            val proposalPower: BigDecimal,
            val newRulePower: BigDecimal
        ) : Invalid(
            "proposal ${proposalNumber.readable} of power $proposalPower cannot set power of rule to $newRulePower"
        )

        data class InconsistentPower(
            val expectedPowerBefore: BigDecimal,
            val actualPowerBefore: BigDecimal,
            val change: HistoricalChange,
        ) : Invalid(
            "change $change expected power to be $expectedPowerBefore but was $actualPowerBefore before application"
        )
    }
}

private fun Collection<HistoricalChangeEffect>.containsOrUnknown(effect: HistoricalChangeEffect): Boolean {
    return contains(effect) || contains(HistoricalChangeEffect.UNKNOWN)
}

fun validateHistory(
    history: RuleHistory,
    finalPower: BigDecimal?,
): RuleHistoryValidationResult {
    val entries = history.entries

    if (entries.isEmpty()) {
        return RuleHistoryValidationResult.Invalid.EmptyHistory
    }

    if (!entries.first().change.effects.containsOrUnknown(HistoricalChangeEffect.ENACTMENT)) {
        return RuleHistoryValidationResult.Invalid.InitialNotEnactment
    }

    if (entries.last().change.effects.contains(HistoricalChangeEffect.REPEAL)) {
        return RuleHistoryValidationResult.Invalid.Repealed
    }

    for ((first, second) in entries.map { it.date }.zipWithNext()) {
        if (first !is HistoricalDate.Known) continue
        if (second !is HistoricalDate.Known) continue

        val firstRaw = first.date
        val secondRaw = second.date

        if (secondRaw < firstRaw) return RuleHistoryValidationResult.Invalid.DescendingDate(firstRaw, secondRaw)
    }

    entries
        .asSequence()
        .zipWithNext()
        .forEach { (first, second) ->
            val firstEffects = first.change.effects
            val secondEffects = second.change.effects

            check(firstEffects.isNotEmpty())
            check(secondEffects.isNotEmpty())

            if (firstEffects.contains(HistoricalChangeEffect.UNKNOWN) || secondEffects.contains(HistoricalChangeEffect.UNKNOWN)) {
                return@forEach
            }

            if (firstEffects.contains(HistoricalChangeEffect.ENACTMENT)) {
                if (secondEffects.contains(HistoricalChangeEffect.ENACTMENT)) {
                    return RuleHistoryValidationResult.Invalid.DoubleEnactment
                }
            }

            if (firstEffects.contains(HistoricalChangeEffect.REPEAL)) {
                if (!secondEffects.contains(HistoricalChangeEffect.ENACTMENT)) {
                    return RuleHistoryValidationResult.Invalid.NoReenactmentAfterRepeal
                }
            }
        }

    val powerResult = validateRuleHistoryPower(
        initialPower = ruleInitialPower(entries, finalPower),
        history = history,
    )

    if (powerResult != null) return powerResult

    return RuleHistoryValidationResult.Valid
}

private sealed class PowerState {
    data object Unknown : PowerState()
    data object Repealed : PowerState()
    data class Known(val power: BigDecimal) : PowerState()
}

private fun validateRuleHistoryPower(
    initialPower: PowerState,
    history: RuleHistory,
): RuleHistoryValidationResult.Invalid? {
    var currentRulePower: PowerState = initialPower

    for (entry in history.entries) {
        val newPower = powerAfter(
            previousPower = currentRulePower,
            change = entry.change,
        )

        if (entry.cause is HistoricalCauses.Proposal) {
            val entryResult = validateRuleChangePower(
                currentRulePower = currentRulePower,
                cause = entry.cause,
                change = entry.change,
            )

            if (entryResult != null) return entryResult
        }

        currentRulePower = newPower
    }

    return null
}

private fun validatePower(previousPower: PowerState, change: HistoricalChange): RuleHistoryValidationResult {
    when (previousPower) {
        is PowerState.Unknown -> return RuleHistoryValidationResult.Valid

        is PowerState.Repealed -> {
            if (change.effects.containsOrUnknown(HistoricalChangeEffect.ENACTMENT)) {
                return RuleHistoryValidationResult.Valid
            }

            error("History inconsistency: PowerState is repealed but this should have been excluded previously")
        }

        is PowerState.Known -> {
            if (change is HistoricalChanges.PowerChange) {
                val from = change.from

                if (from != null && from.compareTo(previousPower.power) != 0) {
                    return RuleHistoryValidationResult.Invalid.InconsistentPower(
                        expectedPowerBefore = from,
                        actualPowerBefore = previousPower.power,
                        change = change,
                    )
                }
            }

            return RuleHistoryValidationResult.Valid
        }
    }
}

private fun validateRuleChangePower(
    currentRulePower: PowerState,
    cause: HistoricalCauses.Proposal,
    change: HistoricalChange,
): RuleHistoryValidationResult.Invalid? {
    if (cause.proposalData.power == null) return null

    if (currentRulePower is PowerState.Unknown) {
        return RuleHistoryValidationResult.Invalid.UnknownRulePower(cause.proposalData.number)
    }

    if (cause.proposalData.power.omnipotent) return null

    val consistencyResult = validatePower(currentRulePower, change)

    if (consistencyResult is RuleHistoryValidationResult.Invalid) {
        return consistencyResult
    }

    val proposalPower = cause.proposalData.power.rawPower

    if (currentRulePower is PowerState.Known) {
        if (proposalPower < currentRulePower.power) {
            return RuleHistoryValidationResult.Invalid.InsufficientPower(
                proposalNumber = cause.proposalData.number,
                proposalPower = proposalPower,
                rulePower = currentRulePower.power,
            )
        }
    }

    val newPower = powerAfter(previousPower = currentRulePower, change)

    if (newPower is PowerState.Unknown) {
        return RuleHistoryValidationResult.Invalid.UnknownTargetPower
    }

    if (newPower is PowerState.Known) {
        if (proposalPower < newPower.power) {
            return RuleHistoryValidationResult.Invalid.InsufficientMutationPower(
                proposalNumber = cause.proposalData.number,
                proposalPower = proposalPower,
                newRulePower = newPower.power,
            )
        }
    }

    return null
}

private fun powerAfter(previousPower: PowerState, change: HistoricalChange): PowerState {
    if (change is HistoricalChanges.Mutation) {
        return (change.to as? HistoricalChanges.MutabilityIndex.Numeric)?.value?.let(PowerState::Known) ?: PowerState.Unknown
    }

    if (change is HistoricalChanges.PowerChange) {
        return change.to?.let(PowerState::Known) ?: PowerState.Unknown
    }

    if (change is HistoricalChanges.Reenactment) {
        return change.power?.let(PowerState::Known) ?: PowerState.Unknown
    }

    if (change.effects.contains(HistoricalChangeEffect.REPEAL)) {
        return PowerState.Repealed
    }

    return previousPower
}

private fun ruleInitialPower(
    entries: ImmutableList<HistoricalEntry>,
    finalPower: BigDecimal?
): PowerState {
    for (entry in entries) {
        val change = entry.change

        if (change is HistoricalChanges.Mutation) {
            return (change.from as? HistoricalChanges.MutabilityIndex.Numeric)?.value?.let(PowerState::Known) ?: PowerState.Unknown
        }

        if (change is HistoricalChanges.PowerChange) {
            return change.from?.let(PowerState::Known) ?: PowerState.Unknown
        }

        val newPower = powerAfter(PowerState.Unknown, change)

        if (newPower !is PowerState.Unknown) {
            // If this change affects the power of the rule, and we can't determine the previous power from metadata
            // in the change, then the current power of the rule doesn't necessarily reflect the initial power.
            // So, give up.
            return PowerState.Unknown
        }
    }

    // No changes affected the power, so the initial power is the power now.
    return finalPower?.let(PowerState::Known) ?: PowerState.Unknown
}
