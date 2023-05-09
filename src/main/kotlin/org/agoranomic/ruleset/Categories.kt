package org.agoranomic.ruleset

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.ImmutableMap
import kotlinx.collections.immutable.toImmutableList
import kotlinx.collections.immutable.toImmutableMap

@JvmInline
value class CategoryId(val raw: String)

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
    private val categoryMapping: ImmutableMap<CategoryId, ImmutableList<RuleNumber>>,
) {
    companion object {
        fun empty() = RuleCategoryMapping(
            categories = CategorySpecificationSet(emptyMap()),
            categoryMapping = emptyMap(),
        )
    }

    init {
        require(categories.categoryIds.containsAll(categoryMapping.keys))
    }

    constructor(
        categories: CategorySpecificationSet,
        categoryMapping: Map<CategoryId, List<RuleNumber>>,
    ) : this(
        categories,
        categoryMapping.mapValues { (_, v) -> v.toImmutableList() }.toImmutableMap(),
    )

    val categorizedRuleNumbers by lazy { categoryMapping.values.flatten().toSet() }

    fun ruleNumbersIn(categoryId: CategoryId): ImmutableList<RuleNumber> {
        return categoryMapping.getValue(categoryId)
    }
}

data class CategorizedRulesetState(
    val categorizedRules: RulesetState,
    private val categoryMapping: RuleCategoryMapping,
) {
    companion object {
        fun selectCategorized(
            fullRulesetState: RulesetState,
            categoryMapping: RuleCategoryMapping,
        ): CategorizedRulesetState {
            return CategorizedRulesetState(
                categorizedRules = fullRulesetState.rulesetByNumbers(categoryMapping.categorizedRuleNumbers),
                categoryMapping = categoryMapping,
            )
        }
    }

    init {
        require(categorizedRules.ruleNumbers == categoryMapping.categorizedRuleNumbers)
    }

    val categories get() = categoryMapping.categories

    fun ruleNumbersIn(categoryId: CategoryId) = categoryMapping.ruleNumbersIn(categoryId)

    fun rulesIn(categoryId: CategoryId): List<RuleState> {
        return categorizedRules.rulesByNumbers(ruleNumbersIn(categoryId))
    }
}
