package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import java.math.BigInteger

interface HistoricalCause {
    val causeString: String
}

private fun stringCause(value: String) = object : HistoricalCause {
    override val causeString: String
        get() = value
}

private inline fun lazyStringCause(crossinline lazyString: () -> String) = object : HistoricalCause {
    override val causeString: String
        get() = lazyString()
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

fun proposalCause(data: ProposalData): HistoricalCause {
    return stringCause(
        "P${data.number}" +
                (data.title?.let { " '$it'" } ?: "") +
                (listOfNotNull(data.chamber, "disi.".takeIf { data.isDisinterested })
                    .takeIf { it.isNotEmpty() }
                    ?.let { it.joinToString(", ", prefix = " [", postfix = "]") } ?: "") +
                (data.authorship
                    ?.let { listOfNotNull(it.author) + (it.coauthors ?: emptyList()) }
                    ?.takeIf { it.isNotEmpty() }
                    ?.let { it.joinToString(", ", prefix = " (", postfix = ")") } ?: "")
    )
}

fun ruleCause(ruleNumber: BigInteger) = stringCause("R$ruleNumber")
fun convergenceCause(cause: HistoricalCause) = lazyStringCause { "a convergence caused by ${cause.causeString}" }
fun cleaningCause(cause: String) = stringCause("cleaning ($cause)")
fun refilingCause(cause: String) = stringCause("refiling ($cause)")
fun decreeCause(agent: String) = stringCause("Decree give by $agent")
fun tournamentInitCause(tournament: String, initiator: String) = stringCause("initiation of $tournament by $initiator")
fun tournamentChangeCause(tournament: String, agent: String) = stringCause("$agent as part of $tournament")
fun tournamentEndCause(tournament: String, agent: String) = stringCause("$agent after end of $tournament")
fun personCause(person: String) = stringCause(person)
fun rulebendingCause(magister: String) = stringCause("Rulebending Form demonstrated by $magister")
