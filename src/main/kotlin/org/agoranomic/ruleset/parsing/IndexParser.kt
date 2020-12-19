package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*

fun parseIndexYaml(yaml: String): RuleCategoryMapping {
    val topNode = parseRawYaml(yaml).requireList()

    return topNode.values.map { it.requireMap() }.map { mapNode ->
        val name = mapNode.getContent("name")

        val category = CategorySpecification(
            id = CategoryId(name), // nothing better to use as the id in the existing YAML format
            readableName = name,
            readableDescription = mapNode.getContent("note").trim()
        )

        val rules =
            mapNode
                .getList("rules")
                .values
                .map { RuleNumber(it.requireValue().content.toBigInteger()) }

        category to rules
    }.let { list ->
        RuleCategoryMapping(
            CategorySpecificationSet.from(list.map { it.first }),
            list.flatMap { categoryRulesPair ->
                categoryRulesPair.second.map { it to categoryRulesPair.first.id }
            }.toMap()
        )
    }
}
