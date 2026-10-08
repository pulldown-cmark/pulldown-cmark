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
# (default: simd). Labels must be valid shell identifiers.
set -euo pipefail

rounds=$1
shift

out=$(mktemp -d)
labels=()
dirs=()

for pair in "$@"; do
    label=${pair%%=*}
    dir=${pair#*=}
    if [[ ! $label =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
        echo "invalid label: $label" >&2
        exit 2
    fi
    labels+=("$label")
    dirs+=("$dir")
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

for round in $(seq 1 "$rounds"); do
    for i in "${!labels[@]}"; do
        label=${labels[$i]}
        echo "round $round: $label" >&2
        while read -r bin; do
            # The markdown-it bench reads its inputs relative to the bench directory.
            (cd "${dirs[$i]}/bench" && CRITERION_HOME="$out/criterion-$label" "$bin" --bench --noplot \
                --warm-up-time 1 --measurement-time 3 2>/dev/null) \
                | LC_ALL=C awk -v label="$label" '
                    NF == 1 { name = $1 }
                    /time:/ {
                        if ($1 != "time:") name = $1
                        for (i = 1; i <= NF; i++) if ($i == "time:") {
                            v = $(i + 3); u = $(i + 4)
                            sub(/\[/, "", v)
                            # units: ns, µs, ms, s (µs is multi-byte, so match by prefix)
                            if (u ~ /^ns/) v /= 1000
                            else if (u ~ /^ms/) v *= 1000
                            else if (u ~ /^s/) v *= 1000000
                            print label, name, v
                        }
                    }' >>"$out/results.txt"
        done <"$out/$label.bins"
    done
done

awk -v labels="${labels[*]}" '
    BEGIN { n = split(labels, L, " ") }
    {
        if (!($2 in seen)) { order[++m] = $2; seen[$2] = 1 }
        k = $1 SUBSEP $2
        if (!(k in t) || $3 < t[k]) t[k] = $3
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
            for (i = 1; i <= n; i++) {
                k = L[i] SUBSEP b
                if (k in t) printf " %.3f |", t[k]; else printf " - |"
            }
            for (i = 2; i <= n; i++) {
                k = L[i] SUBSEP b; k1 = L[1] SUBSEP b
                if ((k in t) && (k1 in t)) printf " %+.1f%% |", (t[k] / t[k1] - 1) * 100
                else printf " - |"
            }
            printf "\n"
        }
    }' "$out/results.txt"
