package org.agoranomic.ruleset.report

import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.*
import org.agoranomic.ruleset.RuleCategoryMapping
import org.agoranomic.ruleset.RulesetState

fun formatJsonIndex(
    fullRulesetState: RulesetState,
    categoryMapping: RuleCategoryMapping,
): String {
    return buildJsonObject {
        putJsonArray("known_rules") {
            for (rule in fullRulesetState) {
                addJsonObject {
                    put("title", rule.title)
                    put("id", rule.id.readable)
                    put("text", rule.text)
                    put("power", rule.power)
                }
            }
        }

        putJsonArray("enacted_rules") {
            for (ruleNumber in categoryMapping.categorizedRuleNumbers) {
                add(ruleNumber.readable)
            }
        }

        putJsonObject("category_names") {
            for (categoryId in categoryMapping.categories.categoryIds) {
                val category = categoryMapping.categories.categoryById(categoryId)

                putJsonObject(categoryId.raw) {
                    put("name", category.readableName)
                    put("description", category.readableDescription)
                }
            }
        }

        putJsonObject("enacted_rule_categories") {
            for (categoryId in categoryMapping.categories.categoryIds) {
                putJsonArray(categoryId.raw) {
                    for (ruleNumber in categoryMapping.ruleNumbersIn(categoryId)) {
                        add(ruleNumber.readable)
                    }
                }
            }
        }
    }.let {
        Json.encodeToString(it)
    }
}
