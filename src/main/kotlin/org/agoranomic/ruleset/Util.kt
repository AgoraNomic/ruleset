package org.agoranomic.ruleset

internal inline fun <T, K> Iterable<T>.associateByPrimaryKey(keySelector: (T) -> K): Map<K, T> {
    return groupBy(keySelector)
        .mapValues { (k, v) ->
            v.distinct().also {
                require(it.size == 1) { "Invalid primary key grouping (key=$k): $it" }
            }.single()
        }
}
