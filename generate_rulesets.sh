#!/usr/bin/env bash

set -euo pipefail

pushd . > /dev/null

OUT_DIR="$(realpath -- generated)"
SCRIPT_DIR="$(realpath -- "$(dirname -- "$0")")"
cd -- "$SCRIPT_DIR"

./gradlew install

RULEKEEPOR_EXEC="$SCRIPT_DIR/build/install/rulekeepor/bin/rulekeepor"
RULES_DATA_DIR="$SCRIPT_DIR/rules_data"
REGS_DATA_DIR="$SCRIPT_DIR/regs_data"

mkdir -p -- "$OUT_DIR"

COMMON_OPTIONS=(
  --name-replacement-file "$SCRIPT_DIR/name_replacement_data"
  --proposals-dir "$RULES_DATA_DIR/proposals"
)

COMMON_RULESET_OPTIONS=(
  --index-file "$RULES_DATA_DIR/config/index"
  --rules-dir "$RULES_DATA_DIR/rules"
  --header-file "$RULES_DATA_DIR/config/header"
)

"$RULEKEEPOR_EXEC" \
  "${COMMON_OPTIONS[@]}" \
  "${COMMON_RULESET_OPTIONS[@]}" \
  --template-file "$RULES_DATA_DIR/config/slr_format" \
  --out-file "$OUT_DIR/slr.txt" \
  --out-dir "$OUT_DIR/short_rules" \
  --out-dir-name-format "R{}.txt" \
  --no-history \
  --no-annotations

"$RULEKEEPOR_EXEC" \
  "${COMMON_OPTIONS[@]}" \
  "${COMMON_RULESET_OPTIONS[@]}" \
  --template-file "$RULES_DATA_DIR/config/flr_format" \
  --out-file "$OUT_DIR/flr.txt" \
  --out-dir "$OUT_DIR/full_rules" \
  --out-dir-name-format "R{}.txt" \
  --generated-index-out-file "$OUT_DIR/rule_index.json" \
  --history \
  --annotations

"$RULEKEEPOR_EXEC" \
  "${COMMON_OPTIONS[@]}" \
  --template-file "$REGS_DATA_DIR/config/flr_format" \
  --index-file "$REGS_DATA_DIR/config/index" \
  --rules-dir "$REGS_DATA_DIR/rules" \
  --header-file "$REGS_DATA_DIR/config/header" \
  --empty-ruleset-file "$REGS_DATA_DIR/config/empty_ruleset" \
  --out-file "$OUT_DIR/acorn.txt" \
  --history \
  --annotations \
  --entity-kind "Regulation"

popd > /dev/null
