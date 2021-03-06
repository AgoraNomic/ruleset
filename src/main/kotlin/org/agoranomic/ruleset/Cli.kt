package org.agoranomic.ruleset

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.int
import com.github.ajalt.clikt.parameters.types.path
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.history.RuleHistoryValidationResult
import org.agoranomic.ruleset.history.validateHistory
import org.agoranomic.ruleset.parsing.DirectoryYamlProposalDataMap
import org.agoranomic.ruleset.parsing.YamlProposalDataMap
import org.agoranomic.ruleset.parsing.parseIndexYaml
import org.agoranomic.ruleset.parsing.parseRuleStateYaml
import org.agoranomic.ruleset.report.*
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import kotlin.system.exitProcess

private val FILE_CHARSET = Charsets.UTF_8
private const val STDOUT_OUT_FILE = "-"

class RuleParseException : Exception {
    constructor(message: String) : super(message)
    constructor(message: String, cause: Exception) : super(message, cause)
}

private class RulekeeporCommand : CliktCommand() {
    val templateFile by option("--template-file", help = "file with ruleset template")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)

    val indexFile by option("--index-file", help = "file with rule index")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)
        .required()

    val outFile by option("--out-file", help = "output file")
        .path(mustExist = false, canBeDir = false)

    val outDir by option("--out-dir", help = "output directory for individual rules")
        .path(mustExist = false, canBeFile = false)

    val outDirNameFormat by option("--out-dir-name-format",
        help = "name format for output files, replacing {} with rule number")
        .default("{}.txt")

    val headerPath by option("--header-file")
        .path(mustExist = false, canBeDir = false)

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

    val validateHistory by option("--validate-history", help = "validate history of each rule")
        .flag("--no-validate-history", default = true)

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

        val ruleCategoryMapping = parseIndexYaml(
            Files.readString(indexFile, FILE_CHARSET),
            ruleNumberResolver = TryIntegralRuleNumberResolver,
        )

        val rulesetState =
            ruleCategoryMapping
                .categorizedRuleNumbers
                .map {
                    it to rulesDir.resolve(it.toString())
                }
                .onEach { (_, path) -> echo("Processing file: $path") }
                .map { (number, path) ->
                    try {
                        parseRuleStateYaml(
                            yaml = Files.readString(path, FILE_CHARSET),
                            proposalDataMap = proposalDataMap,
                            ruleNumberResolver = TryIntegralRuleNumberResolver,
                        )
                    } catch (e: Exception) {
                        throw RuleParseException("Error while parsing rule $number", e)
                    }.also {
                        require(number == it.id) {
                            "Got disagreeing rule number in rule file and index: ${it.id} vs $number"
                        }

                        echo("Got rule ${it.id}")
                    }
                }
                .let { RulesetState(it.toSet()) }

        if (validateHistory) {
            for (rule in rulesetState) {
                @Suppress("UNUSED_VARIABLE")
                val ensureExhaustive = when (val validationResult = validateHistory(rule.history)) {
                    is RuleHistoryValidationResult.Valid -> {
                    }

                    is RuleHistoryValidationResult.Invalid -> {
                        echo(
                            "Error in history validation for rule ${rule.id}: ${validationResult.readableMessage}",
                            err = true,
                        )

                        exitProcess(1)
                    }
                }
            }
        }

        val reportConfig = ReadableReportConfig(
            entityKind = entityKind,
            maxLineLength = maxLineLength,
            includeHistory = includeHistory,
            includeAnnotations = includeAnnotations,
        )

        if (outFile != null) {
            val templateFile = templateFile

            if (templateFile == null) {
                echo("--template-file must be specified if full ruleset output is requested", err = true)
                exitProcess(1)
            }

            val templateWithHeaderReplaced = run {
                val originalTemplate = Files.readString(templateFile, FILE_CHARSET)
                val headerContent = headerPath?.let { Files.readString(it, FILE_CHARSET) }

                replaceHeaderInclusionWithOptionalHeader(
                    template = originalTemplate,
                    headerContent = headerContent,
                )
            }

            formatReadable(
                template = templateWithHeaderReplaced,
                config = reportConfig,
                rulesetState = CategorizedRulesetState(rulesetState, ruleCategoryMapping),
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

        val outDir = outDir
        if (outDir != null) {
            val outDirAbsolute = outDir.toAbsolutePath()
            Files.createDirectories(outDirAbsolute)

            for (rule in rulesetState) {
                val ruleOutFile =
                    outDirAbsolute.resolve(outDirNameFormat.replace("{}", rule.id.toString())).toAbsolutePath()

                // Avoid directory traversal
                require(ruleOutFile.startsWith(outDir))

                formatRule(
                    rule = rule,
                    config = reportConfig,
                ).let {
                    Files.writeString(
                        ruleOutFile,
                        it,
                        FILE_CHARSET,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.TRUNCATE_EXISTING,
                    )
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    RulekeeporCommand().main(args)
}
