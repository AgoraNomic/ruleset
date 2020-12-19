package org.agoranomic.ruleset

import kotlinx.collections.immutable.persistentListOf
import org.agoranomic.ruleset.history.*
import org.agoranomic.ruleset.parsing.ParsedYamlNode
import org.agoranomic.ruleset.parsing.ParsedYamlNode.*
import org.agoranomic.ruleset.parsing.ProposalDataMap
import org.agoranomic.ruleset.parsing.parseRawYaml
import org.agoranomic.ruleset.parsing.parseRuleStateYaml
import org.junit.jupiter.api.Nested
import java.math.BigDecimal
import java.math.BigInteger
import java.time.LocalDate
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

private object ThrowingProposalDataMap : ProposalDataMap {
    override fun dataFor(proposalNumber: ProposalNumber): ProposalData? {
        throw AssertionError("Unexpected proposal lookup: $proposalNumber")
    }
}

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

    @Nested
    inner class RuleYamlTest {
        private fun doTest(
            expected: RuleState,
            yaml: String,
            proposalDataMap: ProposalDataMap = ThrowingProposalDataMap,
        ) {
            assertEquals(expected, parseRuleStateYaml(yaml, proposalDataMap))
        }

        private fun proposaDataMapFor(vararg numbers: Int): ProposalDataMap {
            val bigNumbers = numbers.map { ProposalNumber.Integral(it.toBigInteger()) }

            return object : ProposalDataMap {
                override fun dataFor(proposalNumber: ProposalNumber): ProposalData? {
                    if (proposalNumber in bigNumbers) {
                        return ProposalData(
                            number = proposalNumber,
                            title = "Some title",
                            chamber = null,
                            isDisinterested = false,
                            authorship = ProposalAuthorship(null, null),
                        )
                    }

                    throw AssertionError("Unexpected proposal number lookup: $proposalNumber")
                }
            }
        }

        private fun doTestIAE(yaml: String, proposalDataMap: ProposalDataMap = ThrowingProposalDataMap) {
            assertFailsWith<IllegalArgumentException> {
                parseRuleStateYaml(yaml, proposalDataMap)
            }
        }

        @Test
        fun `all core parts are necessary`() {
            val parts = persistentListOf(
                "id: 101",
                "name: Test Rule",
                "power: 1.1",
                "text: Some Text",
                """
                history:
                - change:
                    type: enactment
                  date: 2020-10-11
                  agent:
                    proposal: 1
                """.trimIndent().trim(),
            )

            parts.indices.map { parts.removeAt(it).joinToString("\n") }.forEach { doTestIAE(it) }

            val dataMap = proposaDataMapFor(1)

            doTest(
                RuleState(
                    id = RuleNumber(101.toBigInteger()),
                    title = "Test Rule",
                    power = BigDecimal("1.1"),
                    text = "Some Text",
                    history = RuleHistory(
                        listOf(
                            HistoricalEntry(
                                HistoricalChanges.enactment(),
                                HistoricalCauses.proposal(dataMap.dataFor(ProposalNumber.Integral(BigInteger.ONE))!!),
                                HistoricalDate.Known(LocalDate.of(2020, 10, 11)),
                            ),
                        )
                    ),
                    annotations = null,
                ),
                parts.joinToString("\n"),
                dataMap,
            )
        }

        private val STD_PREFIX = """
            id: 101
            name: Test Rule
            power: 1.1
            text: Some Text
        """.trimIndent().trim() + "\n"

        private fun stdPrefixRule(
            history: RuleHistory,
            annotations: RuleAnnotations?,
        ): RuleState {
            return RuleState(
                id = RuleNumber(BigInteger("101")),
                title = "Test Rule",
                power = BigDecimal("1.1"),
                text = "Some Text",
                history = history,
                annotations = annotations,
            )
        }

        private val STD_HISTORY_PREFIX = STD_PREFIX + """
            history:
            - change:
                type: enactment
              date: 2020-10-11
              agent:
                proposal: "1"
        """.trimIndent() + "\n"

        private val STD_HISTORY_PROPOSAL_MAP = proposaDataMapFor(1)

        private val STD_HISTORY = RuleHistory(
            listOf(
                HistoricalEntry(
                    HistoricalChanges.enactment(),
                    HistoricalCauses.proposal(STD_HISTORY_PROPOSAL_MAP.dataFor(ProposalNumber.Integral(BigInteger.ONE))!!),
                    HistoricalDate.Known(LocalDate.of(2020, 10, 11)),
                ),
            ),
        )

        @Test
        fun `std rule`() {
            doTest(
                stdPrefixRule(
                    STD_HISTORY,
                    null,
                ),
                STD_HISTORY_PREFIX,
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `history around date`() {
            doTest(
                stdPrefixRule(
                    RuleHistory(
                        listOf(
                            HistoricalEntry(
                                HistoricalChanges.enactment(),
                                HistoricalCauses.proposal(STD_HISTORY_PROPOSAL_MAP.dataFor(ProposalNumber.Integral(
                                    BigInteger.ONE))!!),
                                HistoricalDate.Around(LocalDate.of(2020, 10, 11)),
                            ),
                        ),
                    ),
                    null,
                ),
                STD_PREFIX +
                        """
                            history:
                            - change:
                                type: enactment
                              date:
                                around: 2020-10-11
                              agent:
                                proposal: "1"
                        """.trimIndent().trim(),
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `history between date`() {
            doTest(
                stdPrefixRule(
                    RuleHistory(
                        listOf(
                            HistoricalEntry(
                                HistoricalChanges.enactment(),
                                HistoricalCauses.proposal(STD_HISTORY_PROPOSAL_MAP.dataFor(ProposalNumber.Integral(
                                    BigInteger.ONE))!!),
                                HistoricalDate.Between(
                                    LocalDate.of(2020, 10, 11),
                                    LocalDate.of(2020, 10, 20),
                                ),
                            ),
                        ),
                    ),
                    null,
                ),
                STD_PREFIX +
                        """
                            history:
                            - change:
                                type: enactment
                              date:
                                between: 2020-10-11
                                and: 2020-10-20
                              agent:
                                proposal: "1"
                        """.trimIndent().trim(),
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `null annotations`() {
            doTest(
                stdPrefixRule(STD_HISTORY, annotations = null),
                STD_HISTORY_PREFIX + "annotations:",
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `empty annotations`() {
            doTest(
                stdPrefixRule(STD_HISTORY, annotations = RuleAnnotations(emptyList())),
                STD_HISTORY_PREFIX + "annotations: []",
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `simple cfj annotations`() {
            doTest(
                stdPrefixRule(
                    STD_HISTORY,
                    annotations = RuleAnnotations(
                        listOf(
                            HistoricalCfjAnnotation(
                                listOf(
                                    CfjAnnotationCaseBlock(
                                        CfjAnnotationNumber.Single(CfjNumber(BigInteger("1001"))),
                                        calledDate = HistoricalDate.Known(LocalDate.of(2020, 10, 11)),
                                    ),
                                ),
                                "Some CFJ finding"
                            )
                        )
                    ),
                ),
                STD_HISTORY_PREFIX +
                        """
                            annotations:
                            - cfjs:
                              - id: '1001'
                                called: 2020-10-11
                              text: "Some CFJ finding"
                        """.trimIndent(),
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `cfj range annotations`() {
            doTest(
                stdPrefixRule(
                    STD_HISTORY,
                    annotations = RuleAnnotations(
                        listOf(
                            HistoricalCfjAnnotation(
                                listOf(
                                    CfjAnnotationCaseBlock(
                                        CfjAnnotationNumber.Range(
                                            CfjNumber(BigInteger("1001")),
                                            CfjNumber(BigInteger("1003")),
                                        ),
                                        calledDate = HistoricalDate.Known(LocalDate.of(2020, 10, 11)),
                                    ),
                                ),
                                "Some CFJ finding"
                            )
                        )
                    ),
                ),
                STD_HISTORY_PREFIX +
                        """
                            annotations:
                            - cfjs:
                              - id: 1001-1003
                                called: 2020-10-11
                              text: "Some CFJ finding"
                        """.trimIndent(),
                STD_HISTORY_PROPOSAL_MAP,
            )
        }

        @Test
        fun `cfj multi-block annotations`() {
            doTest(
                stdPrefixRule(
                    STD_HISTORY,
                    annotations = RuleAnnotations(
                        listOf(
                            HistoricalCfjAnnotation(
                                listOf(
                                    CfjAnnotationCaseBlock(
                                        CfjAnnotationNumber.Range(
                                            CfjNumber(BigInteger("1001")),
                                            CfjNumber(BigInteger("1003")),
                                        ),
                                        calledDate = HistoricalDate.Known(LocalDate.of(2020, 10, 11)),
                                    ),
                                    CfjAnnotationCaseBlock(
                                        CfjAnnotationNumber.Single(
                                            CfjNumber(BigInteger("2000")),
                                        ),
                                        calledDate = HistoricalDate.Known(LocalDate.of(4040, 10, 11)),
                                    ),
                                ),
                                "Some CFJ finding"
                            )
                        )
                    ),
                ),
                STD_HISTORY_PREFIX +
                        """
                            annotations:
                            - cfjs:
                              - id: 1001-1003
                                called: 2020-10-11
                              - id: 2000
                                called: 4040-10-11
                              text: "Some CFJ finding"
                        """.trimIndent(),
                STD_HISTORY_PROPOSAL_MAP,
            )
        }
    }
}
