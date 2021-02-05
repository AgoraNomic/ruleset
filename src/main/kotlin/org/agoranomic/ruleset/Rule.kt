package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.ImmutableMap
import kotlinx.collections.immutable.toImmutableList
import kotlinx.collections.immutable.toImmutableMap
import org.agoranomic.ruleset.history.HistoricalEntry
import java.math.BigDecimal
import java.math.BigInteger

data class RuleHistory(val entries: ImmutableList<HistoricalEntry>) {
    // TODO: enforce constraints
    constructor(entries: List<HistoricalEntry>) : this(entries.toImmutableList())
}

sealed class RuleNumber {
    data class Integral(val value: BigInteger) : RuleNumber(), Comparable<RuleNumber.Integral> {
        override fun compareTo(other: RuleNumber.Integral): Int {
            return (this.value).compareTo(other.value)
        }

        override fun toString(): String {
            return value.toString()
        }
    }

    data class Textual(val value: String) : RuleNumber() {
        override fun toString(): String {
            return value
        }
    }
}

interface RuleNumberResolver {
    fun resolve(textualNumber: String): RuleNumber
}

object TextualRuleNumberResolver : RuleNumberResolver {
    override fun resolve(textualNumber: String): RuleNumber {
        return RuleNumber.Textual(textualNumber)
    }
}

object RequireIntegralRuleNumberResolver : RuleNumberResolver {
    override fun resolve(textualNumber: String): RuleNumber {
        return RuleNumber.Integral(
            textualNumber.toBigIntegerOrNull()
                ?: throw IllegalArgumentException("Required integral rule number, but got $textualNumber")
        )
    }
}

object TryIntegralRuleNumberResolver : RuleNumberResolver {
    override fun resolve(textualNumber: String): RuleNumber {
        return textualNumber.toBigIntegerOrNull()?.let { RuleNumber.Integral(it) } ?: RuleNumber.Textual(textualNumber)
    }
}

data class RuleState(
    val id: RuleNumber,
    val title: String,
    val power: BigDecimal?,
    val text: String,
    val history: RuleHistory,
    val annotations: RuleAnnotations?,
)

data class RulesetState(private val rules: ImmutableList<RuleState>) : Collection<RuleState> {
    // This ensures that each number corresponds to exactly one equivalent rule
    private val rulesByNumber: ImmutableMap<RuleNumber, RuleState> =
        rules.associateByPrimaryKey { it.id }.toImmutableMap()

    constructor(rulesByNumber: List<RuleState>) : this(rulesByNumber.toImmutableList())

    override fun iterator(): Iterator<RuleState> {
        return rulesByNumber.values.iterator()
    }

    val ruleNumbers get() = rulesByNumber.keys

    fun ruleByNumber(id: RuleNumber): RuleState {
        return rulesByNumber.getValue(id)
    }

    fun rulesByNumbers(ids: Collection<RuleNumber>): RulesetState {
        return RulesetState(ids.map { ruleByNumber(it) })
    }

    override val size: Int
        get() = rulesByNumber.size

    override fun contains(element: RuleState): Boolean {
        return rulesByNumber.containsValue(element)
    }

    override fun containsAll(elements: Collection<RuleState>): Boolean {
        return rulesByNumber.values.containsAll(elements)
    }

    override fun isEmpty(): Boolean {
        return rulesByNumber.isEmpty()
    }

    fun distinct(): RulesetState = RulesetState(rules.distinct())
}
