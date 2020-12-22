package org.agoranomic.ruleset

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.int
import com.github.ajalt.clikt.parameters.types.path
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.parsing.DirectoryYamlProposalDataMap
import org.agoranomic.ruleset.parsing.YamlProposalDataMap
import org.agoranomic.ruleset.parsing.parseIndexYaml
import org.agoranomic.ruleset.parsing.parseRuleStateYaml
import org.agoranomic.ruleset.report.ProposalStatistics
import org.agoranomic.ruleset.report.ReadableReportConfig
import org.agoranomic.ruleset.report.formatReadable
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import kotlin.streams.asSequence

private val FILE_CHARSET = Charsets.UTF_8
private const val STDOUT_OUT_FILE = "-"

private class RulekeeporCommand : CliktCommand() {
    val templateFile by option("--template-file", help = "file with ruleset template")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)
        .required()

    val indexFile by option("--index-file", help = "file with rule index")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)
        .required()

    val outFile by option("--out-file", help = "output file")
        .path(mustExist = false, canBeDir = false)
        .required()

    val proposalsDir by option("--proposals-dir", help = "proposals directory")
        .path(mustExist = true, canBeFile = false, mustBeReadable = true)

    val rulesDir by option("--rules-dir", help = "rules directory")
        .path(mustExist = true, canBeFile = false, mustBeReadable = true)
        .required()

    val maxLineLength by option("--max-line-length", help = "max line length").int().default(72)

    val includeHistory by option("--history", help = "whether to include rule history")
        .flag("--no-history", default = false)

    val includeAnnotations by option("--annotations", help = "whether to include rule annotations")
        .flag("--no-annotations", default = false)

    val entityKind by option("--entity-kind", help = "what to call the \"rules\" in the report")
        .default("Rule")

    val excludeRuleFiles by option("--exclude-rule-files", help = "file names of rules to exclude")
        .split(",")
        .default(emptyList())

    override fun run() {
        val (proposalDataMap, proposalStats) =
            proposalsDir
                ?.let { DirectoryYamlProposalDataMap(it) }
                ?.let { proposalMap -> proposalMap to proposalMap.maxProposalNumber()?.let { ProposalStatistics(it) } }
                ?: (object : YamlProposalDataMap {
                    override fun dataFor(proposalSpecification: String): ProposalData? {
                        throw IllegalArgumentException("Cannot use proposal when no proposals dir was specified")
                    }
                } to null)

        val rulesetState =
            Files
                .walk(rulesDir)
                .use { fileStream ->
                    fileStream
                        .asSequence()
                        .filter { !Files.isDirectory(it) }
                        .filter { it.fileName.toString() !in excludeRuleFiles }
                        .onEach { echo("Processing file: $it; fileName: ${it.fileName}", err = true) }
                        .map {
                            parseRuleStateYaml(
                                yaml = Files.readString(it, FILE_CHARSET),
                                proposalDataMap = proposalDataMap,
                                ruleNumberResolver = TryIntegralRuleNumberResolver,
                            )
                        }
                        .onEach { echo("Got rule ${it.id}", err = true) }
                        .toList()
                }
                .let { RulesetState.from(it) }

        val ruleCategoryMapping = parseIndexYaml(
            Files.readString(indexFile, FILE_CHARSET),
            ruleNumberResolver = TryIntegralRuleNumberResolver,
        )

        formatReadable(
            Files.readString(templateFile, FILE_CHARSET),
            ReadableReportConfig(
                entityKind = entityKind,
                maxLineLength = maxLineLength,
                includeHistory = includeHistory,
                includeAnnotations = includeAnnotations,
            ),
            CategorizedRulesetState(rulesetState, ruleCategoryMapping),
            proposalStatistics = proposalStats,
        ).let {
            Files.writeString(
                outFile,
                it,
                FILE_CHARSET,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING,
            )
        }
    }
}

fun main(args: Array<String>) {
    RulekeeporCommand().main(args)
}
