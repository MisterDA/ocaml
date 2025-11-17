#!/bin/sh

if command -v cygpath >/dev/null 2>&1; then
  ocamlsrcdir="$(cygpath -u "${ocamlsrcdir:?}")"
  test_build_directory="$(cygpath -u "${test_build_directory:?}")"
fi

exec find "${ocamlsrcdir:?}"/runtime/caml "$ocamlsrcdir"/otherlibs/*/caml \
     -name '*.h' -not -name 'jumptbl.h' \
     -execdir echo '#include <caml/{}>' ';' \
     > "${test_build_directory:?}"/all-includes.h
