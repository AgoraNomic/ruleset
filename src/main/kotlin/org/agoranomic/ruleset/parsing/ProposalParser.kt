package org.agoranomic.ruleset.parsing

import org.agoranomic.ruleset.history.ProposalAuthorship
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.history.ProposalNumber
import java.nio.file.Files
import java.nio.file.Path

data class DirectoryYamlProposalDataMap(val dirPath: Path) : YamlProposalDataMap {
    init {
        require(Files.isDirectory(dirPath))
    }

    override fun dataFor(proposalSpecification: String): ProposalData? {
        val proposalFile = dirPath.resolve(proposalSpecification)

        if (Files.notExists(proposalFile)) return null

        require(proposalFile.normalize().toAbsolutePath().startsWith(dirPath.normalize().toAbsolutePath())) {
            "Attempt to navigate file system by proposal name"
        }

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
    }
}
