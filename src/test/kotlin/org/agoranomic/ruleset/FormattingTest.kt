package org.agoranomic.ruleset

import org.agoranomic.ruleset.report.replaceHeaderInclusionWithOptionalHeader
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class HeaderTest {
    @Test
    fun `no header inclusion, null header`() {
        assertEquals(
            "blah",
            replaceHeaderInclusionWithOptionalHeader(
                template = "blah",
                headerContent = null,
            )
        )
    }

    @Test
    fun `no header inclusion, non-null header`() {
        assertEquals(
            "blah",
            replaceHeaderInclusionWithOptionalHeader(
                template = "blah",
                headerContent = "some header",
            )
        )
    }

    @Test
    fun `header inclusion, null header`() {
        assertFailsWith<IllegalArgumentException> {
            replaceHeaderInclusionWithOptionalHeader(
                template = "blah {header} blep",
                headerContent = null,
            )
        }
    }

    @Test
    fun `header inclusion, non-null header`() {
        assertEquals(
            "blah some header blep",
            replaceHeaderInclusionWithOptionalHeader(
                template = "blah {header} blep",
                headerContent = "some header",
            )
        )
    }
}
