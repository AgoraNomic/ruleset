package org.agoranomic.ruleset

import org.agoranomic.ruleset.parsing.ParsedYamlNode
import org.agoranomic.ruleset.parsing.ParsedYamlNode.*
import org.agoranomic.ruleset.parsing.parseRawYaml
import org.junit.jupiter.api.Nested
import kotlin.test.Test
import kotlin.test.assertEquals

class YamlTest {
    @Nested
    inner class RawYamlTest {
        private fun doTest(expectedNode: ParsedYamlNode, yaml: String) {
            assertEquals(expectedNode, parseRawYaml(yaml))
        }

        @Test
        fun `top level map`() {
            doTest(
                MapNode(
                    mapOf(
                        "key" to ValueNode("value"),
                    )
                ),
                "key: value"
            )
        }

        @Test
        fun `top level value`() {
            doTest(ValueNode("abc"), "abc")
            doTest(ValueNode("123"), "123")
            doTest(ValueNode("2020-11-12"), "2020-11-12")
        }

        @Test
        fun `top level list`() {
            doTest(ListNode(listOf(ValueNode("123"), ValueNode("abc"))), "- 123\n- abc\n")
        }

        @Test
        fun `list of varying types`() {
            doTest(
                ListNode(
                    listOf(
                        MapNode(
                            mapOf(
                                "a" to ValueNode("b"),
                                "c" to ValueNode("d"),
                            ),
                        ),
                        ValueNode("123"),
                        ListNode(
                            listOf(
                                ValueNode("x"),
                                ValueNode("y"),
                                ValueNode("z"),
                            )
                        )
                    ),
                ),
                """
                    - a: b
                      c: d
                    - 123
                    - - x
                      - y
                      - z
                """.trimIndent().trim(),
            )
        }

        @Test
        fun `map with null value`() {
            doTest(
                MapNode(
                    mapOf("a" to NullNode)
                ),
                "a: "
            )
        }

        @Test
        fun `list with null value`() {
            doTest(
                ListNode(
                    listOf(
                        MapNode(
                            mapOf(
                                "a" to ValueNode("b"),
                                "c" to NullNode,
                            )
                        ),
                        NullNode,
                        ValueNode("123")
                    )
                ),
                """
                    - a: b
                      c:
                    - 
                    - 123
                """.trimIndent()
            )
        }
    }
}
