#ifndef CAML_COUNTOF_H
#define CAML_COUNTOF_H

#if defined __has_include
#  if __has_include (<stdcountof.h>)
#    include <stdcountof.h>
#    define HAVE_COUNTOF 1
#  endif
#endif


#if !defined(HAVE_COUNTOF)
#define countof(a) (sizeof(a) / sizeof(*(a)))
#endif

#endif
