#!/usr/bin/env bash
# Interleaved A/B benchmark runner.
#
# usage: bench-ab.sh <rounds> <label>=<dir> [<label>=<dir>...]
#
# Each <dir> is a checkout of this repository. The criterion benches of every
# checkout are built first, then run interleaved for <rounds> rounds so that
# machine noise affects all variants equally. The fastest median per bench is
# kept and a markdown table is printed comparing every variant to the first
# one. Set BENCH_FEATURES_<label> to choose cargo features per variant
# (default: simd).
set -euo pipefail

rounds=$1
shift

out=$(mktemp -d)
labels=()

for pair in "$@"; do
    label=${pair%%=*}
    dir=${pair#*=}
    labels+=("$label")
    features_var="BENCH_FEATURES_${label}"
    features=${!features_var-simd}
    echo "::group::build $label ($dir, features: '${features}')" >&2
    (
        cd "$dir/bench"
        cargo bench --no-run --features "$features" --message-format=json \
            | jq -r 'select(.reason == "compiler-artifact" and .target.kind[0] == "bench" and .executable != null and (.target.name == "html_rendering" or .target.name == "markdown-it")) | .executable'
    ) >"$out/$label.bins"
    cat "$out/$label.bins" >&2
    echo "::endgroup::" >&2
done

# The markdown-it bench reads its inputs relative to the bench directory.
bench_dir=${1#*=}/bench

for round in $(seq 1 "$rounds"); do
    for label in "${labels[@]}"; do
        echo "round $round: $label" >&2
        while read -r bin; do
            (cd "$bench_dir" && CRITERION_HOME="$out/criterion-$label" "$bin" --bench --noplot \
                --warm-up-time 1 --measurement-time 3 2>/dev/null) \
                | awk '
                    NF == 1 { name = $1 }
                    /time:/ {
                        if ($1 != "time:") name = $1
                        for (i = 1; i <= NF; i++) if ($i == "time:") {
                            v = $(i + 3); u = $(i + 4)
                            sub(/\[/, "", v)
                            if (u == "ns") v /= 1000
                            if (u == "ms") v *= 1000
                            if (u == "s") v *= 1000000
                            print name, v
                        }
                    }' >>"$out/$label.txt"
        done <"$out/$label.bins"
    done
done

awk -v labels="${labels[*]}" '
    BEGIN { n = split(labels, L, " ") }
    FNR == 1 { f++ }
    {
        if (!($1 in seen)) { order[++m] = $1; seen[$1] = 1 }
        k = f SUBSEP $1
        if (!(k in t) || $2 < t[k]) t[k] = $2
    }
    END {
        printf "| bench |"
        for (i = 1; i <= n; i++) printf " %s (µs) |", L[i]
        for (i = 2; i <= n; i++) printf " %s vs %s |", L[i], L[1]
        printf "\n|---|"
        for (i = 1; i < 2 * n; i++) printf "---:|"
        printf "\n"
        for (j = 1; j <= m; j++) {
            b = order[j]
            printf "| %s |", b
            for (i = 1; i <= n; i++) printf " %.3f |", t[i SUBSEP b]
            for (i = 2; i <= n; i++) {
                d = (t[i SUBSEP b] / t[1 SUBSEP b] - 1) * 100
                printf " %+.1f%% |", d
            }
            printf "\n"
        }
    }' $(for label in "${labels[@]}"; do echo "$out/$label.txt"; done)
