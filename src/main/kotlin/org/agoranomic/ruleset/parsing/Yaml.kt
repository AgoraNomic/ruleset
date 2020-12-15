package org.agoranomic.ruleset.parsing

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.ImmutableMap
import kotlinx.collections.immutable.toImmutableList
import kotlinx.collections.immutable.toImmutableMap
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.resolver.Resolver

/**
 * A [Resolver] that treats everything as strings.
 */
private class StringResolver : Resolver() {
    override fun addImplicitResolvers() {
        addImplicitResolver(Tag.MERGE, MERGE, "<")
        addImplicitResolver(Tag.NULL, NULL, "~nN\u0000")
        addImplicitResolver(Tag.NULL, EMPTY, null)
    }
}

sealed class ParsedYamlNode {
    companion object {
        fun fromRaw(data: Any?): ParsedYamlNode {
            return when (data) {
                null -> NullNode
                is String -> ValueNode(data)
                is Map<*, *> -> MapNode.fromRaw(data)
                is List<*> -> ListNode.fromRaw(data)
                else -> throw IllegalArgumentException("Invalid type for YAML data: ${data::class}")
            }
        }
    }

    object NullNode : ParsedYamlNode()

    data class ValueNode(val content: String) : ParsedYamlNode()

    data class MapNode(private val mapping: ImmutableMap<String, ParsedYamlNode>) : ParsedYamlNode() {
        constructor(mapping: Map<String, ParsedYamlNode>) : this(mapping.toImmutableMap())

        companion object {
            fun fromRaw(data: Map<*, *>): MapNode {
                val safeData = data.toMap()

                require(safeData.keys.all { it is String })
                return MapNode(safeData.map { (k, v) -> (k as String) to fromRaw(v) }.toMap())
            }
        }

        operator fun get(key: String) = mapping.get(key)
        val keys get() = mapping.keys
    }

    data class ListNode(private val values: ImmutableList<ParsedYamlNode>) : ParsedYamlNode() {
        constructor(values: List<ParsedYamlNode>) : this(values.toImmutableList())

        companion object {
            fun fromRaw(data: List<*>): ListNode {
                val safeData = data.toList()
                return ListNode(safeData.map { fromRaw(it) })
            }
        }

        operator fun get(index: Int) = values.get(index)
        operator fun iterator() = values.iterator()
    }
}

fun ParsedYamlNode.requireMap(): ParsedYamlNode.MapNode {
    return this as? ParsedYamlNode.MapNode ?: throw IllegalArgumentException("Expected map, got $this")
}

// Opt -> Optional. If the node is a NullNode, returns null.

fun ParsedYamlNode.requireOptMap(): ParsedYamlNode.MapNode? {
    if (this is ParsedYamlNode.NullNode) return null
    return requireMap()
}

fun ParsedYamlNode.requireList(): ParsedYamlNode.ListNode {
    return this as? ParsedYamlNode.ListNode ?: throw IllegalArgumentException("Expected list, got $this")
}

fun ParsedYamlNode.requireOptList(): ParsedYamlNode.ListNode? {
    if (this is ParsedYamlNode.NullNode) return null
    return requireList()
}

fun ParsedYamlNode.requireValue(): ParsedYamlNode.ValueNode {
    return this as? ParsedYamlNode.ValueNode ?: throw IllegalArgumentException("Expected value, got $this")
}

fun ParsedYamlNode.requireOptValue(): ParsedYamlNode.ValueNode? {
    if (this is ParsedYamlNode.NullNode) return null
    return requireValue()
}

// Opt -> Optional. If the node does not exist, or is a null node, returns null

fun ParsedYamlNode.MapNode.getValue(key: String) =
    (this[key] ?: throw IllegalArgumentException("Expected value")).requireValue()

fun ParsedYamlNode.MapNode.getOptValue(key: String) = this[key]?.requireOptValue()

fun ParsedYamlNode.MapNode.getMap(key: String) =
    (this[key] ?: throw IllegalArgumentException("Expected map")).requireMap()

fun ParsedYamlNode.MapNode.getOptMap(key: String) = this[key]?.requireOptMap()

fun ParsedYamlNode.MapNode.getList(key: String) =
    (this[key] ?: throw IllegalArgumentException("Expected list")).requireList()

fun ParsedYamlNode.MapNode.getOptList(key: String) = this[key]?.requireOptList()
