package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import org.agoranomic.ruleset.history.HistoricalDate
import org.agoranomic.ruleset.history.HistoricalEntry
import java.math.BigDecimal
import java.math.BigInteger

sealed class RuleAnnotation

sealed class CfjAnnotationNumber {
    data class Single(val number: BigInteger) : CfjAnnotationNumber()
    data class Range(val first: BigInteger, val last: BigInteger) : CfjAnnotationNumber()
}

data class HistoricalCfjAnnotation(
    val number: CfjAnnotationNumber,
    val calledDate: HistoricalDate?,
    val finding: String,
) : RuleAnnotation()

data class RuleHistory(val entries: ImmutableList<HistoricalEntry>) {
    // TODO: enforce constraints
    constructor(entries: List<HistoricalEntry>) : this(entries.toImmutableList())
}

data class RuleAnnotations(val annotations: ImmutableList<RuleAnnotation>) {
    constructor(annotations: List<RuleAnnotation>) : this(annotations.toImmutableList())
}

data class RuleState(
    val id: BigInteger,
    val title: String,
    val power: BigDecimal,
    val text: String,
    val history: RuleHistory,
    val annotations: RuleAnnotations?,
)
