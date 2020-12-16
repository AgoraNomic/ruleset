package org.agoranomic.ruleset

import org.agoranomic.ruleset.history.HistoricalCauses
import org.agoranomic.ruleset.history.ProposalAuthorship
import org.agoranomic.ruleset.history.ProposalData
import org.junit.jupiter.api.Nested
import java.math.BigInteger
import kotlin.test.Test
import kotlin.test.assertEquals

class CauseTest {
    @Nested
    inner class ProposalCauseTests {
        private fun doTest(
            expected: String,
            data: ProposalData,
        ) {
            assertEquals(expected, HistoricalCauses.proposal(data).causeString)
        }

        private fun doTest(
            expected: String,
            number: BigInteger,
            title: String? = null,
            chamber: String? = null,
            isDisinterested: Boolean = false,
            authorship: ProposalAuthorship? = null,
        ) {
            return doTest(expected, ProposalData(
                number = number,
                title = title,
                chamber = chamber,
                isDisinterested = isDisinterested,
                authorship = authorship,
            ))
        }

        @Test
        fun `no optional data`() {
            doTest(
                "P1",
                number = BigInteger.ONE,
            )
        }

        @Test
        fun `title only`() {
            doTest(
                "P1 'some title'",
                number = BigInteger.ONE,
                title = "some title",
            )
        }

        @Test
        fun `author only`() {
            doTest(
                "P1 (Alice)",
                number = BigInteger.ONE,
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = null
                ),
            )
        }

        @Test
        fun `author, empty coauthors`() {
            doTest(
                "P1 (Alice)",
                number = BigInteger.ONE,
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = emptyList(),
                ),
            )
        }

        @Test
        fun `title, author`() {
            doTest(
                "P1 'some proposal' (Alice)",
                number = BigInteger.ONE,
                title = "some proposal",
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = null,
                ),
            )
        }

        @Test
        fun `title, author, disi`() {
            doTest(
                "P1 'some proposal' [disi.] (Alice)",
                number = BigInteger.ONE,
                title = "some proposal",
                isDisinterested = true,
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = null,
                ),
            )
        }

        @Test
        fun `title, author, empty coauthors`() {
            doTest(
                "P1 'some proposal' (Alice)",
                number = BigInteger.ONE,
                title = "some proposal",
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = emptyList(),
                ),
            )
        }

        @Test
        fun `title, author, coauthors`() {
            doTest(
                "P1 'some proposal' (Alice, Bob, Charlie)",
                number = BigInteger.ONE,
                title = "some proposal",
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = listOf("Bob", "Charlie"),
                ),
            )
        }

        @Test
        fun `title, chamber, author`() {
            doTest(
                "P1 'some proposal' [some chamber] (Alice)",
                number = BigInteger.ONE,
                title = "some proposal",
                chamber = "some chamber",
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = null,
                ),
            )
        }

        @Test
        fun `title, chamber, disi, author`() {
            doTest(
                "P1 'some proposal' [some chamber, disi.] (Alice)",
                number = BigInteger.ONE,
                title = "some proposal",
                chamber = "some chamber",
                isDisinterested = true,
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = null,
                ),
            )
        }


        @Test
        fun `title, chamber, author, coauthors`() {
            doTest(
                "P1 'some proposal' [some chamber] (Alice, Bob, Charlie)",
                number = BigInteger.ONE,
                title = "some proposal",
                chamber = "some chamber",
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = listOf("Bob", "Charlie"),
                ),
            )
        }

        @Test
        fun `title, chamber, disi, author, coauthors`() {
            doTest(
                "P1 'some proposal' [some chamber, disi.] (Alice, Bob, Charlie)",
                number = BigInteger.ONE,
                title = "some proposal",
                chamber = "some chamber",
                isDisinterested = true,
                authorship = ProposalAuthorship(
                    author = "Alice",
                    coauthors = listOf("Bob", "Charlie"),
                ),
            )
        }
    }
}
