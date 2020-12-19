package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableMap
import kotlinx.collections.immutable.toImmutableMap

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

data class RuleCategoryMapping(
    val categories: CategorySpecificationSet,
    private val categoryMapping: ImmutableMap<RuleNumber, CategoryId>,
) {
    init {
        require(categories.categoryIds.containsAll(categoryMapping.values))
    }

    constructor(
        categories: CategorySpecificationSet,
        categoryMapping: Map<RuleNumber, CategoryId>,
    ) : this(
        categories,
        categoryMapping.toImmutableMap(),
    )

    val categorizedRuleNumbers get() = categoryMapping.keys

    fun categoryIdOf(ruleNumber: RuleNumber): CategoryId {
        return categoryMapping.getValue(ruleNumber)
    }

    fun categoryOf(ruleNumber: RuleNumber): CategorySpecification {
        return categories.categoryById(categoryIdOf(ruleNumber))
    }

    fun ruleNumbersIn(categoryId: CategoryId): Set<RuleNumber> {
        return categoryMapping.filter { it.value == categoryId }.map { it.key }.toSet()
    }
}

data class CategorizedRulesetState(
    val ruleset: RulesetState,
    private val categoryMapping: RuleCategoryMapping,
) {
    init {
        require(ruleset.ruleNumbers.containsAll(categoryMapping.categorizedRuleNumbers))
    }

    val categorizedRuleNumbers get() = categoryMapping.categorizedRuleNumbers
    val categorizedRules by lazy { ruleset.rulesByNumbers(categorizedRuleNumbers) }
    val categories get() = categoryMapping.categories

    fun categoryIdOf(ruleNumber: RuleNumber): CategoryId {
        return categoryMapping.categoryIdOf(ruleNumber)
    }

    fun categoryOf(ruleNumber: RuleNumber): CategorySpecification {
        return categoryMapping.categoryOf(ruleNumber)
    }

    fun ruleNumbersIn(categoryId: CategoryId): Set<RuleNumber> {
        return categoryMapping.ruleNumbersIn(categoryId)
    }

    fun rulesIn(categoryId: CategoryId): RulesetState {
        return ruleset.rulesByNumbers(ruleNumbersIn(categoryId))
    }
}
