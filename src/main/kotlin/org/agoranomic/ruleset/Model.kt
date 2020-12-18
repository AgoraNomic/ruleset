package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.ImmutableMap
import kotlinx.collections.immutable.toImmutableList
import kotlinx.collections.immutable.toImmutableMap
import org.agoranomic.ruleset.history.HistoricalDate
import org.agoranomic.ruleset.history.HistoricalEntry
import java.math.BigDecimal
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

data class RuleHistory(val entries: ImmutableList<HistoricalEntry>) {
    // TODO: enforce constraints
    constructor(entries: List<HistoricalEntry>) : this(entries.toImmutableList())
}

data class RuleAnnotations(val annotations: ImmutableList<RuleAnnotation>) {
    constructor(annotations: List<RuleAnnotation>) : this(annotations.toImmutableList())
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

data class RulesetState(private val rulesByNumber: ImmutableMap<RuleNumber, RuleState>) : Iterable<RuleState> {
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
}

inline class CategoryId(val raw: String) {
    override fun toString(): String {
        return raw
    }
}

data class CategorySpecification(
    val id: CategoryId,
    val readableName: String,
    val readableDescription: String,
)

data class CategorySpecificationSet(
    private val categoriesById: ImmutableMap<CategoryId, CategorySpecification>,
) : Iterable<CategorySpecification> {
    init {
        require(categoriesById.all { it.key == it.value.id })
    }

    constructor(categoriesById: Map<CategoryId, CategorySpecification>) : this(categoriesById.toImmutableMap())

    companion object {
        fun from(collection: Collection<CategorySpecification>): CategorySpecificationSet {
            return CategorySpecificationSet(collection.associateByPrimaryKey { it.id })
        }
    }

    override fun iterator(): Iterator<CategorySpecification> {
        return categoriesById.values.iterator()
    }

    val categoryIds get() = categoriesById.keys

    fun categoryById(id: CategoryId): CategorySpecification {
        return categoriesById.getValue(id)
    }
}

data class CategorizedRulesetState(
    val ruleset: RulesetState,
    val categories: CategorySpecificationSet,
    private val categoryMapping: ImmutableMap<RuleNumber, CategoryId>,
) {
    init {
        require(ruleset.ruleNumbers.containsAll(categoryMapping.keys))
        require(categories.categoryIds.containsAll(categoryMapping.values))
    }

    constructor(
        rulesetState: RulesetState,
        categories: CategorySpecificationSet,
        categoryMapping: Map<RuleNumber, CategoryId>,
    ) : this(
        rulesetState,
        categories,
        categoryMapping.toImmutableMap(),
    )

    val categorizedRuleNumbers get() = categoryMapping.keys
    val categorizedRules by lazy { categoryMapping.keys.map { ruleset.ruleByNumber(it) } }

    fun categoryOf(ruleNumber: RuleNumber): CategoryId {
        return categoryMapping.getValue(ruleNumber)
    }

    fun rulesIn(categoryId: CategoryId): Set<RuleNumber> {
        return categoryMapping.filter { it.value == categoryId }.map { it.key }.toSet()
    }
}
