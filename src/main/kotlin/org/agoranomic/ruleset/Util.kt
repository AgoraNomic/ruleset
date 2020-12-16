package org.agoranomic.ruleset

internal inline fun <T, K> Iterable<T>.groupByPrimaryKey(keySelector: (T) -> K): Map<K, T> {
    return groupBy(keySelector).mapValues { (_, v) -> v.distinct().also { require(it.size == 1) }.single() }
}
