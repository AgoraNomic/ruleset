package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import org.agoranomic.ruleset.RuleNumber
import java.math.BigInteger

interface HistoricalCause {
    val causeString: String
}

data class ProposalAuthorship(
    val author: String,
    val coauthors: ImmutableList<String>?,
) {
    constructor(
        author: String,
        coauthors: List<String>?,
    ) : this(
        author = author,
        coauthors = coauthors?.toImmutableList(),
    )
}

data class ProposalData(
    val number: BigInteger,
    val title: String?,
    val chamber: String?,
    val isDisinterested: Boolean,
    val authorship: ProposalAuthorship?,
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

    fun proposal(data: ProposalData): HistoricalCause {
        return stringCause(
            "P${data.number}" +
                    (data.title?.let { " '$it'" } ?: "") +
                    (listOfNotNull(data.chamber, "disi.".takeIf { data.isDisinterested })
                        .takeIf { it.isNotEmpty() }
                        ?.joinToString(", ", prefix = " [", postfix = "]") ?: "") +
                    (data.authorship
                        ?.let { listOfNotNull(it.author) + (it.coauthors ?: emptyList()) }
                        ?.takeIf { it.isNotEmpty() }
                        ?.joinToString(", ", prefix = " (", postfix = ")") ?: "")
        )
    }

    fun rule(ruleNumber: RuleNumber) = stringCause("R$ruleNumber")
    fun convergence(cause: HistoricalCause) = lazyStringCause { "a convergence caused by ${cause.causeString}" }
    fun cleaning(cause: String) = stringCause("cleaning ($cause)")
    fun refiling(cause: String) = stringCause("refiling ($cause)")
    fun ratification(document: String) = stringCause("$document ratification")
    fun decree(agent: String) = stringCause("Decree give by $agent")
    fun tournamentInit(tournament: String, initiator: String) = stringCause("initiation of $tournament by $initiator")
    fun tournamentChange(tournament: String, agent: String) = stringCause("$agent as part of $tournament")
    fun tournamentEnd(tournament: String, agent: String) = stringCause("$agent after end of $tournament")
    fun person(person: String) = stringCause(person)
    fun rulebending(magister: String) = stringCause("Rulebending Form demonstrated by $magister")
}
