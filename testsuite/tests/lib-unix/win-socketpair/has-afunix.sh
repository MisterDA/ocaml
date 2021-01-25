#!/bin/sh

# Test if the OS runtime has afunix enabled.

if [ sc query afunix ]; then
  exit ${TEST_SKIP};
fi
exit ${TEST_PASS}
