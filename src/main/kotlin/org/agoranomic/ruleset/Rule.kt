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

inline class RuleNumber(val raw: BigInteger) : Comparable<RuleNumber> {
    override fun compareTo(other: RuleNumber): Int {
        return (this.raw).compareTo(other.raw)
    }

    override fun toString(): String {
        return raw.toString()
    }
}

data class RuleState(
    val id: RuleNumber,
    val title: String,
    val power: BigDecimal,
    val text: String,
    val history: RuleHistory,
    val annotations: RuleAnnotations?,
)

data class RulesetState(private val rulesByNumber: ImmutableMap<RuleNumber, RuleState>) : Collection<RuleState> {
    init {
        require(rulesByNumber.all { it.key == it.value.id })
    }

    constructor(rulesByNumber: Map<RuleNumber, RuleState>) : this(rulesByNumber.toImmutableMap())

    companion object {
        fun from(collection: Collection<RuleState>): RulesetState {
            return RulesetState(collection.associateByPrimaryKey { it.id })
        }
    }

    override fun iterator(): Iterator<RuleState> {
        return rulesByNumber.values.iterator()
    }

    val ruleNumbers get() = rulesByNumber.keys

    fun ruleByNumber(id: RuleNumber): RuleState {
        return rulesByNumber.getValue(id)
    }

    fun rulesByNumbers(ids: Collection<RuleNumber>): RulesetState {
        return from(ids.map { ruleByNumber(it) })
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
}
