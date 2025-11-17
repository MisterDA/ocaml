#!/bin/sh

if command -v cygpath >/dev/null 2>&1; then
  ocamltest_response="$(cygpath -u "${ocamltest_response:?}")"
  have_cygpath=true
else
  have_cygpath=false
fi

if [ "${ccomp_type:?}" = "cc" ]; then
  if "$have_cygpath"; then
    ocamlsrcdir="$(cygpath -u "${ocamlsrcdir:?}")"
    test_source_directory="$(cygpath -u "${test_source_directory:?}")"
  fi

  # TODO: use GCC 15 -fdiagnostics-add-output for stderr and SARIF output sinks
  if ${cxx:?} -Wall -Wextra -Wpedantic \
           -fdiagnostics-format=sarif-stderr \
           ${cppflags:?} ${cflags:?} \
           -I "${ocamlsrcdir:?}"/runtime \
           -I "$ocamlsrcdir"/otherlibs/runtime_events \
           -I "$ocamlsrcdir"/otherlibs/str \
           -I "$ocamlsrcdir"/otherlibs/systhreads \
           -I "$ocamlsrcdir"/otherlibs/unix \
           -o stubs.o \
           -c "${test_source_directory:?}"/stubs.cpp \
           2> stubs.sarif; then
    exit "$TEST_PASS"
  else
    cat <<'EOF' > filter.jq
.runs.[0].results.[]
| select(.message.text
        | contains("ISO C++ forbids flexible array member") | not)
| length
EOF
    length=$(jq -f filter.jq stubs.sarif)
    if [ -z "$length" ]; then
      exit "$TEST_PASS"
    fi
  fi

elif [ "$ccomp_type" = "msvc" ]; then
  if ${cxx:?} -W2 -permissive- \
          -experimental:log stubs \
          ${cppflags:?} ${cflags:?} \
          -I "${ocamlsrcdir:?}"\\runtime \
          -I "$ocamlsrcdir"\\otherlibs\\runtime_events \
          -I "$ocamlsrcdir"\\otherlibs\\str \
          -I "$ocamlsrcdir"\\otherlibs\\systhreads \
          -I "$ocamlsrcdir"\\otherlibs\\unix \
          -Fostubs.obj \
          -c "${test_source_directory:?}"\\stubs.cpp \
          >/dev/null 2>&1; then
    exit "$TEST_PASS"
  else
    # https://learn.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-levels-2-and-4-c4200
    cat <<'EOF' > filter.jq
.runs.[0].results.[]
| select(.ruleId
        | contains("C4200") | not)
| length
EOF
    length=$(jq -f filter.jq stubs.sarif)
    if [ -z "$length" ]; then
      exit "$TEST_PASS"
    fi
  fi
fi

# pretty-print
jq . stubs.sarif > stubs.sarif.tmp
mv stubs.sarif.tmp stubs.sarif

if [ "$GITHUB_ACTIONS" = true ]; then
  echo
  echo "::group::stubs.sarif content ($(wc -l stubs.sarif) lines)"
  cat stubs.sarif
  echo
  echo '::endgroup::'
elif [ -n "$CI" ]; then
  cat stubs.sarif
fi
echo "$cxx reported errors at ${test_build_directory:?}/stubs.sarif" \
     >> "${ocamltest_response:?}"
exit "$TEST_FAIL"
