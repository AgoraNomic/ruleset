package org.agoranomic.ruleset.history

import java.time.LocalDate

sealed class HistoricalDate {
    data object Unknown : HistoricalDate()
    data class Known(val date: LocalDate) : HistoricalDate()
    data class Around(val date: LocalDate) : HistoricalDate()
    data class Between(val start: LocalDate, val end: LocalDate) : HistoricalDate()
}

data class HistoricalEntry(
    val change: HistoricalChange,
    val cause: HistoricalCause?,
    val date: HistoricalDate,
)
