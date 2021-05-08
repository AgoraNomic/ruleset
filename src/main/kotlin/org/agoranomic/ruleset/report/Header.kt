package org.agoranomic.ruleset.report

private const val HEADER_INCLUSION = "{header}"

private fun doReplaceHeader(
    template: String,
    headerContent: String,
): String {
    return template.replace(HEADER_INCLUSION, headerContent)
}

/**
 * If [template] does not container `{header}`, returns it unmodified.
 * Otherwise, if [headerContent] is null, throws [IllegalArgumentException].
 * Otherwise, replaces all instances of `{header}` in [template] with [headerContent].
 */
fun replaceHeaderInclusionWithOptionalHeader(
    template: String,
    headerContent: String?,
): String {
    return if (headerContent != null) {
        doReplaceHeader(template = template, headerContent = headerContent)
    } else {
        require(!template.contains(HEADER_INCLUSION)) {
            "Header was used but not provided"
        }

        template
    }
}
