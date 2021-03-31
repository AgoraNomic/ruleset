package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import org.agoranomic.ruleset.history.HistoricalDate
import java.math.BigInteger

inline class CfjNumber(val raw: BigInteger) : Comparable<CfjNumber> {
    override fun compareTo(other: CfjNumber): Int {
        return (this.raw).compareTo(other.raw)
    }

    override fun toString(): String {
        return raw.toString()
    }
}

sealed class RuleAnnotation

sealed class CfjAnnotationNumber {
    data class Single(val number: CfjNumber) : CfjAnnotationNumber()
    data class Range(val first: CfjNumber, val last: CfjNumber) : CfjAnnotationNumber()
}

data class CfjAnnotationCaseBlock(
    val number: CfjAnnotationNumber,
    val calledDate: HistoricalDate?,
)

data class HistoricalCfjAnnotation(
    val blocks: ImmutableList<CfjAnnotationCaseBlock>,
    val finding: String,
) : RuleAnnotation() {
    constructor(
        blocks: List<CfjAnnotationCaseBlock>,
        finding: String,
    ) : this(
        blocks.toImmutableList(),
        finding,
    )
}

data class RuleAnnotations(val annotations: ImmutableList<RuleAnnotation>) : Collection<RuleAnnotation> by annotations {
    constructor(annotations: List<RuleAnnotation>) : this(annotations.toImmutableList())
}
