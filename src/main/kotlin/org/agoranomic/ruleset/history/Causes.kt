package org.agoranomic.ruleset.history

import kotlinx.collections.immutable.ImmutableList
import kotlinx.collections.immutable.toImmutableList
import org.agoranomic.ruleset.RuleNumber
import java.math.BigDecimal
import java.math.BigInteger

interface HistoricalCause {
    val causeString: String
}

interface TaggedHistoricalCause : HistoricalCause {
    val tag: String
}

const val PROPOSAL_CAUSE_TAG = "proposal"

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
    data class Integral(val number: BigInteger) : ProposalNumber() {
        override val readable: String
            get() = number.toString()
    }

    data class HistoricalOddity(val unparsed: String) : ProposalNumber() {
        override val readable: String
            get() = unparsed
    }

    abstract val readable: String
}

data class ProposalPower(
    val rawPower: BigDecimal,
    val omnipotent: Boolean,
)

data class ProposalData(
    val number: ProposalNumber,
    val title: String?,
    val chamber: String?,
    val isDisinterested: Boolean,
    val authorship: ProposalAuthorship,
    val power: ProposalPower?,
)

object HistoricalCauses {
    fun proposal(data: ProposalData): HistoricalCause = Proposal(proposalData = data)
    fun rule(ruleNumber: RuleNumber): HistoricalCause = Rule(ruleNumber = ruleNumber)
    fun convergence(cause: HistoricalCause): HistoricalCause = Convergence(cause = cause)
    fun cleaning(cause: String): HistoricalCause = Cleaning(cause = cause)
    fun refiling(cause: String): HistoricalCause = Refiling(cause = cause)
    fun ratification(document: String): HistoricalCause = Ratification(document)
    fun decree(agent: String): HistoricalCause = Decree(agent = agent)
    fun person(person: String): HistoricalCause = Person(person = person)
    fun rulebending(magister: String): HistoricalCause = Rulebending(magister = magister)

    fun tournamentInit(tournament: String, initiator: String): HistoricalCause =
        TournamentInit(tournament = tournament, initiator = initiator)

    fun tournamentChange(tournament: String, agent: String): HistoricalCause =
        TournamentChange(tournament = tournament, agent = agent)

    fun tournamentEnd(tournament: String, agent: String): HistoricalCause =
        TournamentEnd(tournament = tournament, agent = agent)

    fun deviceExperiment(id: Int, madEngineer: String): HistoricalCause =
        DeviceExperiment(id = id, madEngineer = madEngineer)

    private abstract class BaseCause(
        override val tag: String,
        override val causeString: String,
    ) : TaggedHistoricalCause

    private data class Proposal(val proposalData: ProposalData) : BaseCause(
        PROPOSAL_CAUSE_TAG,
        "P${proposalData.number.readable}" +
                (proposalData.title?.let { " '$it'" } ?: "") +
                (listOfNotNull(proposalData.chamber, "disi.".takeIf { proposalData.isDisinterested })
                    .takeIf { it.isNotEmpty() }
                    ?.joinToString(", ", prefix = " [", postfix = "]") ?: "") +
                (proposalData.authorship
                    .let { listOfNotNull(it.author) + (it.coauthors ?: emptyList()) }
                    .takeIf { it.isNotEmpty() }
                    ?.joinToString(", ", prefix = " (", postfix = ")") ?: "")
    )

    private data class Convergence(val cause: HistoricalCause) : TaggedHistoricalCause {
        override val tag: String
            get() = "convergence"

        override val causeString: String
            get() = "a convergence caused by ${cause.causeString}"
    }

    private data class Rule(val ruleNumber: RuleNumber) : BaseCause("rule", "R${ruleNumber.readable}")
    private data class Cleaning(val cause: String) : BaseCause("cleaning", "cleaning ($cause)")
    private data class Refiling(val cause: String) : BaseCause("refiling", "refiling ($cause)")
    private data class Ratification(val document: String) : BaseCause("ratification", "$document ratification")
    private data class Decree(val agent: String) : BaseCause("decree", "decree given by $agent")
    private data class Person(val person: String) : BaseCause("person", person)

    private data class TournamentInit(val tournament: String, val initiator: String) :
        BaseCause("tournament_init", "initiation of $tournament by $initiator")

    private data class TournamentChange(val tournament: String, val agent: String) :
        BaseCause("tournament_change", "$agent as part of $tournament")

    private data class TournamentEnd(val tournament: String, val agent: String) :
        BaseCause("tournament_end", "$agent after end of $tournament")

    private data class Rulebending(val magister: String) :
        BaseCause("rulebending", "Rulebending Form demonstrated by $magister")

    private data class DeviceExperiment(val id: Int, val madEngineer: String) :
        BaseCause("device_experiment", "Experiment #$id under Mad Engineer $madEngineer") {
        init {
            require(id >= 1)
        }
    }
}
