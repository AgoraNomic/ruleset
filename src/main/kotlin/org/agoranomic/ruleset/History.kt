package org.agoranomic.ruleset

interface HistoricalChange {
    val changeCount: Int
    fun formatEffect(baseChangeNumber: Int): String
}

fun uncountedHistoricalChange(value: String): HistoricalChange {
    return object : HistoricalChange {
        override val changeCount: Int
            get() = 0

        override fun formatEffect(baseChangeNumber: Int): String {
            return value
        }
    }
}

fun countedManyHistoricalChange(changeCount: Int, format: (List<Int>) -> String): HistoricalChange {
    require(changeCount > 0)

    return object : HistoricalChange {
        override val changeCount
            get() = 1

        override fun formatEffect(baseChangeNumber: Int): String {
            return format(List(changeCount) { baseChangeNumber + it })
        }
    }
}

fun countedOnceHistoricalChange(format: (Int) -> String): HistoricalChange {
    return countedManyHistoricalChange(1) { list -> format(list.single()) }
}

fun enactmentHistoricalChange() = uncountedHistoricalChange("Enacted")
fun countedAmendmentHistoricalChange() = countedOnceHistoricalChange { "Amended ($it)" }
fun uncountedAmendmentHistoricalChange() = uncountedHistoricalChange("Amended")
