package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.history.ProposalAuthorship
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.history.ProposalNumber
import org.agoranomic.ruleset.history.ProposalPower
import java.nio.file.Files
import java.nio.file.Path
import kotlin.streams.asSequence

data class DirectoryYamlProposalDataMap(val proposalsDir: Path) : YamlProposalDataMap {
    init {
        require(Files.isDirectory(proposalsDir))
    }

    override fun dataFor(proposalSpecification: String, nameResolver: CauseNameResolver): ProposalData? {
        val proposalFile = proposalsDir.resolve(proposalSpecification)

        if (Files.notExists(proposalFile)) return null

        require(proposalFile.normalize().toAbsolutePath().startsWith(proposalsDir.normalize().toAbsolutePath())) {
            "Attempt to navigate file system by proposal name"
        }

        try {
            val topNode = parseRawYaml(Files.readString(proposalFile, Charsets.UTF_8)).requireMap()

            return ProposalData(
                number = topNode.getContent("id").let { id ->
                    id.toBigIntegerOrNull()?.let { ProposalNumber.Integral(it) } ?: ProposalNumber.HistoricalOddity(id)
                },
                title = topNode.getOptContent("title"),
                authorship = ProposalAuthorship(
                    author = topNode
                        .getOptContent("author")
                        ?.let {
                            nameResolver.resolveInformalCauseName(it)
                        },
                    coauthors = topNode
                        .getOptList("coauthors")
                        ?.values
                        ?.map {
                            nameResolver.resolveInformalCauseName(it.requireValue().content)
                        },
                ),
                chamber = topNode.getOptContent("chamber"),
                isDisinterested = topNode.getOptContent("disinterested").toBoolean(),
                power = run {
                    val rawPower = topNode.getOptContent("power")
                    val omnipotent = topNode.getOptContent("omnipotent")

                    when {
                        (rawPower != null) && (omnipotent != null) -> ProposalPower(
                            rawPower = rawPower.toBigDecimal(),
                            omnipotent = omnipotent.toBoolean(),
                        )

                        (rawPower == null) && (omnipotent == null) -> null

                        else -> throw IllegalArgumentException("Expected both power and omnipotent or neither")
                    }
                },
            )
        } catch (error: Exception) {
            throw ProposalDataParseException.forProposalSpecification(
                specification = proposalSpecification,
                cause = error,
            )
        }
    }

    /**
     * Returns the highest integral number that is a direct descendant of the proposal directory (or null if no such
     * file exists).
     */
    fun maxProposalNumber(): ProposalNumber.Integral? {
        return Files.walk(proposalsDir, 1).use { stream ->
            stream
                .asSequence()
                .mapNotNull { it.fileName.toString().toBigIntegerOrNull() }
                .maxOrNull()
                ?.let { ProposalNumber.Integral(it) }
        }
    }
}
