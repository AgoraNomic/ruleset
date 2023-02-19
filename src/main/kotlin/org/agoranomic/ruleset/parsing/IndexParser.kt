package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.*

fun parseIndexYaml(yaml: String, ruleNumberResolver: RuleNumberResolver): RuleCategoryMapping {
    val topNode = parseRawYaml(yaml).also {
        if (it is ParsedYamlNode.NullNode) {
            return RuleCategoryMapping.empty()
        }
    }.requireList()

    return topNode.values.map { it.requireMap() }.associate { mapNode ->
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
                .map { ruleNumberResolver.resolve(it.requireValue().content) }

        category to rules
    }.let { map ->
        RuleCategoryMapping(
            CategorySpecificationSet.from(map.keys),
            map.mapKeys { (k, _) -> k.id },
        )
    }
}
