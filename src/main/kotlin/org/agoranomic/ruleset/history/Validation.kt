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
    }
}

fun validateHistory(
    history: RuleHistory,
    finalPower: BigDecimal?,
): RuleHistoryValidationResult {
    val entries = history.entries

    if (entries.isEmpty()) {
        return RuleHistoryValidationResult.Invalid.EmptyHistory
    }

    if (!entries.first().change.effects.contains(HistoricalChangeEffect.ENACTMENT)) {
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

private fun validateRuleHistoryPower(
    initialPower: BigDecimal?,
    history: RuleHistory,
): RuleHistoryValidationResult.Invalid? {
    var currentRulePower: BigDecimal? = initialPower

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

private fun validateRuleChangePower(
    currentRulePower: BigDecimal?,
    cause: HistoricalCauses.Proposal,
    change: HistoricalChange,
): RuleHistoryValidationResult.Invalid? {
    if (cause.proposalData.power == null) return null

    if (currentRulePower == null) {
        return RuleHistoryValidationResult.Invalid.UnknownRulePower(cause.proposalData.number)
    }

    if (cause.proposalData.power.omnipotent) return null

    val proposalPower = cause.proposalData.power.rawPower

    if (proposalPower < currentRulePower) {
        return RuleHistoryValidationResult.Invalid.InsufficientPower(
            proposalNumber = cause.proposalData.number,
            proposalPower = proposalPower,
            rulePower = currentRulePower,
        )
    }

    val newPower = powerAfter(previousPower = currentRulePower, change)
        ?: return RuleHistoryValidationResult.Invalid.UnknownTargetPower

    if (proposalPower < newPower) {
        return RuleHistoryValidationResult.Invalid.InsufficientMutationPower(
            proposalNumber = cause.proposalData.number,
            proposalPower = proposalPower,
            newRulePower = newPower,
        )
    }

    return null
}

private fun powerAfter(previousPower: BigDecimal?, change: HistoricalChange): BigDecimal? {
    if (change is HistoricalChanges.Mutation) {
        return (change.to as? HistoricalChanges.MutabilityIndex.Numeric)?.value
    }

    if (change is HistoricalChanges.PowerChange) {
        return change.to
    }

    if (change is HistoricalChanges.Reenactment) {
        val power = change.power

        if (power != null) {
            return power
        }
    }

    return previousPower
}

private fun ruleInitialPower(
    entries: ImmutableList<HistoricalEntry>,
    finalPower: BigDecimal?
): BigDecimal? {
    val powerChanges =
        entries.filter { it.change is HistoricalChanges.Mutation || it.change is HistoricalChanges.PowerChange }

    if (powerChanges.isEmpty()) {
        return finalPower
    }

    return when (val first = powerChanges.first().change) {
        is HistoricalChanges.Mutation -> {
            (first.from as? HistoricalChanges.MutabilityIndex.Numeric)?.value
        }

        is HistoricalChanges.PowerChange -> {
            first.from
        }

        else -> {
            error("unexpected type")
        }
    }
}
