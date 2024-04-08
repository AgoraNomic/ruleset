package org.agoranomic.ruleset

import org.agoranomic.ruleset.history.*
import java.math.BigDecimal
import java.math.BigInteger
import java.time.LocalDate
import kotlin.test.Test
import kotlin.test.assertIs

class ValidationTest {
    companion object {
        private fun makeEntry(
            change: HistoricalChange,
            cause: HistoricalCause? = null,
            date: HistoricalDate = HistoricalDate.Unknown,
        ): HistoricalEntry {
            return HistoricalEntry(change = change, cause = cause, date = date)
        }

        private fun history(vararg entries: HistoricalEntry): RuleHistory {
            return RuleHistory(entries.toList())
        }

        private fun proposalCause(power: BigDecimal): HistoricalCause {
            return HistoricalCauses.proposal(
                ProposalData(
                    number = ProposalNumber.Integral(BigInteger.ONE),
                    title = null,
                    chamber = null,
                    isDisinterested = false,
                    authorship = ProposalAuthorship(author = null, coauthors = null),
                    power = ProposalPower(rawPower = power, omnipotent = false),
                )
            )
        }

        private fun omnipotentCause(): HistoricalCause {
            return HistoricalCauses.proposal(
                ProposalData(
                    number = ProposalNumber.Integral(BigInteger.ONE),
                    title = null,
                    chamber = null,
                    isDisinterested = false,
                    authorship = ProposalAuthorship(author = null, coauthors = null),
                    power = ProposalPower(rawPower = BigDecimal.valueOf(3), omnipotent = true),
                )
            )
        }
    }

    @Test
    fun `empty history`() {
        assertIs<RuleHistoryValidationResult.Invalid.EmptyHistory>(
            validateHistory(
                history = history(),
                finalPower = null,
            )
        )
    }

    @Test
    fun `unknown history`() {
        assertIs<RuleHistoryValidationResult.Valid>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.unknown()),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `initial not enactment`() {
        assertIs<RuleHistoryValidationResult.Invalid.InitialNotEnactment>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.countedAmendment()),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `ends with repeal`() {
        assertIs<RuleHistoryValidationResult.Invalid.Repealed>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.repeal()),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `consecutive enactments`() {
        assertIs<RuleHistoryValidationResult.Invalid.DoubleEnactment>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.enactment()),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `enactment then reenactment`() {
        assertIs<RuleHistoryValidationResult.Invalid.DoubleEnactment>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.unchangedReenactment(power = null)),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `nonconsecutive enactments, no repeal`() {
        assertIs<RuleHistoryValidationResult.Invalid.DoubleEnactment>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.countedAmendment()),
                    makeEntry(HistoricalChanges.unchangedReenactment(power = null)),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `enactment, unknown, enactment`() {
        assertIs<RuleHistoryValidationResult.Valid>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.unknown()),
                    makeEntry(HistoricalChanges.changedReenactment(power = null)),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `repeal then amendment`() {
        assertIs<RuleHistoryValidationResult.Invalid.NoReenactmentAfterRepeal>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.repeal()),
                    makeEntry(HistoricalChanges.countedAmendment()),
                    makeEntry(HistoricalChanges.unchangedReenactment(power = null)),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `descending date`() {
        assertIs<RuleHistoryValidationResult.Invalid.DescendingDate>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment(), date = HistoricalDate.Known(LocalDate.of(2024, 1, 2))),
                    makeEntry(HistoricalChanges.countedAmendment(), date = HistoricalDate.Known(LocalDate.of(2023, 11, 12))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `inconsistent power with power change`() {
        assertIs<RuleHistoryValidationResult.Invalid.InconsistentPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(2))),
                    makeEntry(HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(3))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `inconsistent power with reenactment`() {
        assertIs<RuleHistoryValidationResult.Invalid.InconsistentPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.repeal()),
                    makeEntry(HistoricalChanges.changedReenactment(power = BigDecimal.valueOf(2))),
                    makeEntry(HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(3))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `inconsistent power with mutation`() {
        assertIs<RuleHistoryValidationResult.Invalid.InconsistentPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.mutation(from = null, to = HistoricalChanges.MutabilityIndex.Numeric(BigDecimal("1.5")))),
                    makeEntry(HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(3))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `insufficient amendment power`() {
        assertIs<RuleHistoryValidationResult.Invalid.InsufficientPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.countedAmendment(), proposalCause(power = BigDecimal.valueOf(1))),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }

    @Test
    fun `insufficient power change power`() {
        assertIs<RuleHistoryValidationResult.Invalid.InsufficientMutationPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(
                        HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(2)),
                        proposalCause(power = BigDecimal("1.5")),
                    ),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }

    @Test
    fun `inconsistent final power`() {
        assertIs<RuleHistoryValidationResult.Invalid.InconsistentFinalPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = BigDecimal.valueOf(3))),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }

    @Test
    fun `known power before change`() {
        assertIs<RuleHistoryValidationResult.Valid>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.countedAmendment(), proposalCause(power = BigDecimal.valueOf(1))),
                    makeEntry(
                        HistoricalChanges.powerChange(from = BigDecimal.valueOf(0.5), to = BigDecimal.valueOf(2)),
                        proposalCause(power = BigDecimal.valueOf(3)),
                    ),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }

    @Test
    fun `unknown rule power on enactment`() {
        assertIs<RuleHistoryValidationResult.Invalid.UnknownRulePower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment(), proposalCause(power = BigDecimal.valueOf(1))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `unknown rule power on amendment`() {
        assertIs<RuleHistoryValidationResult.Invalid.UnknownRulePower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.countedAmendment(), proposalCause(power = BigDecimal.valueOf(1))),
                ),
                finalPower = null,
            )
        )
    }

    @Test
    fun `unknown rule power before power change`() {
        assertIs<RuleHistoryValidationResult.Invalid.UnknownRulePower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(HistoricalChanges.countedAmendment(), proposalCause(power = BigDecimal.valueOf(1))),
                    makeEntry(
                        HistoricalChanges.powerChange(from = null, to = BigDecimal.valueOf(2)),
                        proposalCause(power = BigDecimal.valueOf(3)),
                    ),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }

    @Test
    fun `unknown new power`() {
        assertIs<RuleHistoryValidationResult.Invalid.UnknownTargetPower>(
            validateHistory(
                history = history(
                    makeEntry(HistoricalChanges.enactment()),
                    makeEntry(
                        HistoricalChanges.powerChange(from = BigDecimal.valueOf(1), to = null),
                        proposalCause(power = BigDecimal.valueOf(3)),
                    ),
                ),
                finalPower = BigDecimal.valueOf(2),
            )
        )
    }
}
