#!/bin/sh

set -e

if command -v cygpath >/dev/null 2>&1; then
  ocamltest_response="$(cygpath -u "${ocamltest_response:?}")"
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq could not be found" >> "${ocamltest_response:?}"
  exit "$TEST_SKIP"
fi

if [ "${ccomp_type:?}" = "cc" ]; then
  gnuc=$(${cxx:?} -E -P - <<'EOF'
__GNUC__
EOF
      )

  if [ "$gnuc" -lt 13 ]; then
    echo "GCC 13 or later is required for SARIF diagnostics" \
         >> "$ocamltest_response"
    exit "$TEST_SKIP"
  fi
elif [ "$ccomp_type" = "msvc" ]; then
  :
else
  echo "Unknown ccomp_type $ccomp_type" >> "$ocamltest_response"
  exit "$TEST_SKIP"
fi

exit "$TEST_PASS"
