/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_STDBIT_H
#define CAML_STDBIT_H

#ifdef CAML_INTERNALS

#include "config.h"
#include <stdint.h>

#ifdef HAVE_STDBIT_H
#include <stdbit.h>
#endif

#ifndef HAVE_STDC_MEMREVERSE8
static inline void
stdc_memreverse8(size_t n, unsigned char *ptr)
{
  if (n > 0) {
    for (size_t i = 0, j = n-1; i < j; i++, j--) {
      unsigned char xi = ptr[i];
      unsigned char xj = ptr[j];
      ptr[j] = xi;
      ptr[i] = xj;
    }
  }
}
#endif

#endif /* CAML_INTERNALS */
#endif /* CAML_STDBIT_H */
