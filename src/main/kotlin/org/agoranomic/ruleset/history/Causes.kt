package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import org.agoranomic.ruleset.RuleNumber
import java.math.BigInteger

interface HistoricalCause {
    val causeString: String
}

interface TaggedHistoricalCause : HistoricalCause {
    val tag: String
}

const val PROPOSAL_CAUSE_TAG = "proposal"

interface ProposalTaggedHistoricalCause : TaggedHistoricalCause {
    override val tag: String
        get() = PROPOSAL_CAUSE_TAG

    val proposalData: ProposalData
}

data class ProposalAuthorship(
    val author: String?,
    val coauthors: ImmutableList<String>?,
) {
    constructor(
        author: String?,
        coauthors: List<String>?,
    ) : this(
        author = author,
        coauthors = coauthors?.toImmutableList(),
    )
}

sealed class ProposalNumber {
    data class Integral(val number: BigInteger) : ProposalNumber()
    data class HistoricalOddity(val unparsed: String) : ProposalNumber()
}

private fun ProposalNumber.readable() = when (this) {
    is ProposalNumber.Integral -> number.toString()
    is ProposalNumber.HistoricalOddity -> unparsed
}

data class ProposalData(
    val number: ProposalNumber,
    val title: String?,
    val chamber: String?,
    val isDisinterested: Boolean,
    val authorship: ProposalAuthorship,
)

object HistoricalCauses {
    private fun stringCause(value: String) = object : HistoricalCause {
        override val causeString: String
            get() = value
    }

    private inline fun lazyStringCause(crossinline lazyString: () -> String) = object : HistoricalCause {
        override val causeString: String
            get() = lazyString()
    }

    private fun HistoricalCause.tagged(tag: String): TaggedHistoricalCause {
        val newTag = tag // to remove all ambiguity

        if (this is TaggedHistoricalCause) {
            require(this.tag == newTag)
            return this
        }

        return object : HistoricalCause by this, TaggedHistoricalCause {
            override val tag: String
                get() = newTag
        }
    }

    fun proposal(data: ProposalData): ProposalTaggedHistoricalCause {
        return object : ProposalTaggedHistoricalCause {
            override val causeString: String
                get() {
                    return "P${data.number.readable()}" +
                            (data.title?.let { " '$it'" } ?: "") +
                            (listOfNotNull(data.chamber, "disi.".takeIf { data.isDisinterested })
                                .takeIf { it.isNotEmpty() }
                                ?.joinToString(", ", prefix = " [", postfix = "]") ?: "") +
                            (data.authorship
                                .let { listOfNotNull(it.author) + (it.coauthors ?: emptyList()) }
                                .takeIf { it.isNotEmpty() }
                                ?.joinToString(", ", prefix = " (", postfix = ")") ?: "")
                }

            override val proposalData: ProposalData
                get() = data
        }
    }

    fun rule(ruleNumber: RuleNumber) = stringCause("R$ruleNumber").tagged("rule")

    fun convergence(cause: HistoricalCause) =
        lazyStringCause { "a convergence caused by ${cause.causeString}" }.tagged("convergence")

    fun cleaning(cause: String) = stringCause("cleaning ($cause)").tagged("cleaning")
    fun refiling(cause: String) = stringCause("refiling ($cause)").tagged("refiling")
    fun ratification(document: String) = stringCause("$document ratification").tagged("ratification")
    fun decree(agent: String) = stringCause("Decree give by $agent").tagged("decree")

    fun tournamentInit(tournament: String, initiator: String) =
        stringCause("initiation of $tournament by $initiator").tagged("tournament_init")

    fun tournamentChange(tournament: String, agent: String) =
        stringCause("$agent as part of $tournament").tagged("tournament_change")

    fun tournamentEnd(tournament: String, agent: String) =
        stringCause("$agent after end of $tournament").tagged("tournament_end")

    fun person(person: String) = stringCause(person).tagged("person")
    fun rulebending(magister: String) = stringCause("Rulebending Form demonstrated by $magister").tagged("rulebending")
}
