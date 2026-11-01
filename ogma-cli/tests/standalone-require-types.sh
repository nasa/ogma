#!/usr/bin/env bash
# End-to-end smoke test of standalone's unresolved external type contract.
set -euo pipefail

spec_unmapped=ogma-core/tests/fcs-untyped-sensor.json
spec_mapped=ogma-core/tests/fcs-typed-sensor.json
spec_two=ogma-core/tests/fcs-two-untyped-sensors.json
evidence_dir=ogma-cli/tests/out/standalone-require-types
mkdir -p "$evidence_dir"

# CI builds the CLI first. Running the binary directly keeps Cabal/compiler
# diagnostics out of the stderr assertions below. A bare `cabal list-bin`
# executable runs outside Cabal's packaged data-file environment, so point
# Paths_ogma_core at the checked-out Ogma resources explicitly.
ogma_data_dir="${OGMA_CORE_DATADIR:-$PWD/ogma-core}"
test -r "$ogma_data_dir/data/formats/fcs_smv"
test -d "$ogma_data_dir/templates/standalone"
export ogma_core_datadir="$ogma_data_dir"
ogma_bin="${OGMA_BIN:-$(cabal list-bin ogma-cli:exe:ogma)}"

run_standalone() {
  local scenario="$1"
  local fixture="$2"
  shift 2
  mkdir -p "$evidence_dir/$scenario"
  set +e
  "$ogma_bin" standalone \
    --input-format fcs --prop-format smv \
    --input-file "$fixture" \
    --target-dir "$evidence_dir/$scenario/generated" \
    "$@" > "$evidence_dir/$scenario/stdout.log" \
    2> "$evidence_dir/$scenario/stderr.log"
  local code=$?
  set -e
  echo "$code" > "$evidence_dir/$scenario/exit-code.txt"
  echo "$scenario exit: $code"
}

run_standalone default-unmapped "$spec_unmapped"
test "$(< "$evidence_dir/default-unmapped/exit-code.txt")" -eq 0
grep -q 'sensor_ok :: Stream (_)' "$evidence_dir/default-unmapped/generated/Copilot.hs"
grep -qi 'warning' "$evidence_dir/default-unmapped/stderr.log"
grep -q 'sensor_ok' "$evidence_dir/default-unmapped/stderr.log"

run_standalone default-two-unmapped "$spec_two"
test "$(< "$evidence_dir/default-two-unmapped/exit-code.txt")" -eq 0
test "$(grep -o 'sensor_ok' "$evidence_dir/default-two-unmapped/stderr.log" | wc -l)" -eq 1
test "$(grep -o 'link_ok' "$evidence_dir/default-two-unmapped/stderr.log" | wc -l)" -eq 1

run_standalone strict-unmapped "$spec_unmapped" --require-types
test "$(< "$evidence_dir/strict-unmapped/exit-code.txt")" -ne 0
grep -q 'sensor_ok' "$evidence_dir/strict-unmapped/stderr.log"
test ! -e "$evidence_dir/strict-unmapped/generated/Copilot.hs"

run_standalone strict-mapped "$spec_mapped" --require-types
test "$(< "$evidence_dir/strict-mapped/exit-code.txt")" -eq 0
grep -q 'sensor_ok :: Stream (Bool)' "$evidence_dir/strict-mapped/generated/Copilot.hs"
if grep -qi 'warning' "$evidence_dir/strict-mapped/stderr.log"; then
  echo 'mapped strict case unexpectedly emitted a warning' >&2
  exit 1
fi

run_standalone strict-two-unmapped "$spec_two" --require-types
test "$(< "$evidence_dir/strict-two-unmapped/exit-code.txt")" -ne 0
grep -q 'sensor_ok' "$evidence_dir/strict-two-unmapped/stderr.log"
grep -q 'link_ok' "$evidence_dir/strict-two-unmapped/stderr.log"
test ! -e "$evidence_dir/strict-two-unmapped/generated/Copilot.hs"

# Check that --require-types is propagated through the --project code path.
project_case="$evidence_dir/strict-project-unmapped"
mkdir -p "$project_case"
set +e
"$ogma_bin" standalone \
  --project ogma-core/tests/project-untyped-standalone.ogma \
  --target-dir "$project_case/generated" --require-types \
  > "$project_case/stdout.log" 2> "$project_case/stderr.log"
project_exit=$?
set -e
echo "$project_exit" > "$project_case/exit-code.txt"
test "$project_exit" -ne 0
grep -q 'sensor_ok' "$project_case/stderr.log"
test ! -e "$project_case/generated/Copilot.hs"

echo 'standalone unresolved-type smoke assertions passed'
