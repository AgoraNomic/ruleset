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

"$RULEKEEPOR_EXEC" \
  --template-file "$RULES_DATA_DIR/config/slr_format" \
  --index-file "$RULES_DATA_DIR/config/index" \
  --proposals-dir "$RULES_DATA_DIR/proposals" \
  --rules-dir "$RULES_DATA_DIR/rules" \
  --header-file "$RULES_DATA_DIR/config/header" \
  --out-file "$OUT_DIR/slr.txt" \
  --out-dir "$OUT_DIR/short_rules" \
  --out-dir-name-format "R{}.txt" \
  --no-history \
  --no-annotations

"$RULEKEEPOR_EXEC" \
  --template-file "$RULES_DATA_DIR/config/flr_format" \
  --index-file "$RULES_DATA_DIR/config/index" \
  --proposals-dir "$RULES_DATA_DIR/proposals" \
  --rules-dir "$RULES_DATA_DIR/rules" \
  --header-file "$RULES_DATA_DIR/config/header" \
  --out-file "$OUT_DIR/flr.txt" \
  --out-dir "$OUT_DIR/full_rules" \
  --out-dir-name-format "R{}.txt" \
  --history \
  --annotations

"$RULEKEEPOR_EXEC" \
  --template-file "$REGS_DATA_DIR/config/flr_format" \
  --index-file "$REGS_DATA_DIR/config/index" \
  --rules-dir "$REGS_DATA_DIR/rules" \
  --header-file "$REGS_DATA_DIR/config/header" \
  --out-file "$OUT_DIR/acorn.txt" \
  --history \
  --annotations \
  --entity-kind "Regulation"

popd > /dev/null
