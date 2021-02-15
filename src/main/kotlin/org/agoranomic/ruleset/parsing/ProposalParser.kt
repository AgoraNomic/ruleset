package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.history.ProposalAuthorship
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.history.ProposalNumber
import java.nio.file.Files
import java.nio.file.Path
import kotlin.streams.asSequence

data class DirectoryYamlProposalDataMap(val proposalsDir: Path) : YamlProposalDataMap {
    init {
        require(Files.isDirectory(proposalsDir))
    }

    override fun dataFor(proposalSpecification: String): ProposalData? {
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
                    author = topNode.getOptContent("author"),
                    coauthors = topNode.getOptList("coauthors")?.values?.map { it.requireValue().content },
                ),
                chamber = topNode.getOptContent("chamber"),
                isDisinterested = topNode.getOptContent("disinterested").toBoolean(),
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
