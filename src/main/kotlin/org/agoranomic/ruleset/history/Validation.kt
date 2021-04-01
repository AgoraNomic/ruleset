package org.agoranomic.ruleset.history

import org.agoranomic.ruleset.RuleHistory
import java.time.LocalDate

sealed class RuleHistoryValidationResult {
    object Valid : RuleHistoryValidationResult()

    sealed class Invalid(val readableMessage: String) : RuleHistoryValidationResult() {
        object EmptyHistory : Invalid("empty history")
        object InitialNotEnactment : Invalid("initial change is not an enactment")
        object DoubleEnactment : Invalid("two enactments in a row")
        object NoReenactmentAfterRepeal : Invalid("rule was not re-enacted after a repeal")

        data class DescendingDate(val first: LocalDate, val second: LocalDate) : Invalid(
            "date $second is before date $first",
        )
    }
}

fun validateHistory(history: RuleHistory): RuleHistoryValidationResult {
    val entries = history.entries

    if (entries.isEmpty()) {
        return RuleHistoryValidationResult.Invalid.EmptyHistory
    }

    if (!entries.first().change.effects.contains(HistoricalChangeEffect.ENACTMENT)) {
        return RuleHistoryValidationResult.Invalid.InitialNotEnactment
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

    return RuleHistoryValidationResult.Valid
}
