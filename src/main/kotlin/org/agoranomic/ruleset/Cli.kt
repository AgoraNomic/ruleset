package org.agoranomic.ruleset

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.cooccurring
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.int
import com.github.ajalt.clikt.parameters.types.path
import kotlinx.collections.immutable.toImmutableMap
import org.agoranomic.ruleset.history.ProposalData
import org.agoranomic.ruleset.history.RuleHistoryValidationResult
import org.agoranomic.ruleset.history.validateHistory
import org.agoranomic.ruleset.parsing.*
import org.agoranomic.ruleset.report.*
import org.randomcat.util.requireDistinct
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import kotlin.io.path.readText
import kotlin.system.exitProcess

private val FILE_CHARSET = Charsets.UTF_8
private const val STDOUT_OUT_FILE = "-"

class RuleParseException : Exception {
    constructor(message: String) : super(message)
    constructor(message: String, cause: Exception) : super(message, cause)
}

private fun causeNameResolverForMap(nameReplacementMap: Map<String, String>): CauseNameResolver {
    val safeMap = nameReplacementMap.toImmutableMap()

    return object : CauseNameResolver {
        private fun resolveName(name: String): String {
            return safeMap[name] ?: name
        }

        override fun resolveInformalCauseName(name: String): String {
            return resolveName(name)
        }

        override fun resolveFormalCauseName(name: String): String {
            return resolveName(name)
        }
    }
}

private class SingleFileOutputGroup : OptionGroup() {
    val outFile by option("--out-file", help = "output file")
        .path(mustExist = false, canBeDir = false)
        .required()

    val templateFile by option("--template-file", help = "file with ruleset template")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)
        .required()

    val emptyRulesetPath by option("--empty-ruleset-file", help = "path to file containing content of report when no rules exist")
        .path(mustExist = false, canBeDir = false)
}

private class RulekeeporCommand : CliktCommand() {
    val indexFile by option("--index-file", help = "file with rule index")
        .path(mustExist = true, canBeDir = false, mustBeReadable = true)
        .required()

    val outFileGroup by SingleFileOutputGroup().cooccurring()

    val outDir by option("--out-dir", help = "output directory for individual rules")
        .path(mustExist = false, canBeFile = false)

    val outDirNameFormat by option("--out-dir-name-format",
        help = "name format for output files, replacing {} with rule number")
        .default("{}.txt")

    val generatedIndexOutFile by option("--generated-index-out-file", help = "where to write a generated JSON index")
        .path(mustExist = false, canBeDir = false)

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

    val nameReplacementFile by option("--name-replacement-file",
        help = "path to file with name replacements, one per line, of the form old:new")
        .path(mustExist = true, canBeDir = false)

    override fun run() {
        val (proposalDataMap, proposalStats) =
            proposalsDir
                ?.let { DirectoryYamlProposalDataMap(it) }
                ?.let { proposalMap -> proposalMap to proposalMap.maxProposalNumber()?.let { ProposalStatistics(it) } }
                ?: (object : YamlProposalDataMap {
                    override fun dataFor(
                        proposalSpecification: String,
                        nameResolver: CauseNameResolver,
                    ): ProposalData? {
                        throw IllegalArgumentException("Cannot use proposal when no proposals dir was specified")
                    }
                } to null)

        val ruleCategoryMapping = parseIndexYaml(
            Files.readString(indexFile, FILE_CHARSET),
            ruleNumberResolver = TryIntegralRuleNumberResolver,
        )

        val nameResolver = nameReplacementFile?.let { nameReplacementFile ->
            val nameReplacementText = nameReplacementFile.readText()

            val nameReplacementMappingEntries =
                nameReplacementText
                    .lines()
                    .filter { it.isNotBlank() }
                    .map { it.split(":") }
                    .onEach {
                        require(it.size == 2) { "Expected two colon separated parts, got $it" }
                    }
                    .map {
                        it[0] to it[1]
                    }

            nameReplacementMappingEntries.map { it.first }.requireDistinct()

            val nameReplacementMap = nameReplacementMappingEntries.toMap()

            causeNameResolverForMap(nameReplacementMap)
        } ?: CauseNameResolver.Identity

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
                            nameResolver = nameResolver,
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

        val outFileGroup = outFileGroup

        if (outFileGroup != null) {
            val templateFile = outFileGroup.templateFile

            val rulesetText = if (ruleCategoryMapping == RuleCategoryMapping.empty()) {
                val emptyRulesetPath = outFileGroup.emptyRulesetPath

                require(emptyRulesetPath != null) {
                    "If no rules exist, an empty ruleset file must be provided"
                }

                Files.readString(emptyRulesetPath, FILE_CHARSET)
            } else {
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
                    fullRulesetState = rulesetState,
                    categoryMapping = ruleCategoryMapping,
                    proposalStatistics = proposalStats,
                )
            }

            Files.writeString(
                outFileGroup.outFile,
                rulesetText,
                FILE_CHARSET,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING,
            )
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

        val generatedIndexOutFile = generatedIndexOutFile
        if (generatedIndexOutFile != null) {
            formatJsonIndex(
                fullRulesetState = rulesetState,
                categoryMapping = ruleCategoryMapping
            ).let {
                Files.writeString(
                    generatedIndexOutFile,
                    it,
                    FILE_CHARSET,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                )
            }
        }
    }
}

fun main(args: Array<String>) {
    RulekeeporCommand().main(args)
}
