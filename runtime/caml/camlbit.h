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

#ifndef HAVE_STDC_LOAD8_LEU16
static inline uint_least16_t
stdc_load8_leu16(const unsigned char ptr[2])
{
  uint_fast16_t v0 = ptr[0];
  uint_fast16_t v1 = ptr[1];
  return (v0 << (8 * 0)) | (v1 << (8 * 1));
}
#endif

#ifndef HAVE_STDC_LOAD8_LEU32
static inline uint_least32_t
stdc_load8_leu32(const unsigned char ptr[4])
{
  uint_fast32_t v0 = ptr[0];
  uint_fast32_t v1 = ptr[1];
  uint_fast32_t v2 = ptr[2];
  uint_fast32_t v3 = ptr[3];
  return (v0 << (8 * 0)) | (v1 << (8 * 1)) | (v2 << (8 * 2)) | (v3 << (8 * 3));
}
#endif

#ifndef HAVE_STDC_LOAD8_LEU64
static inline uint_least64_t
stdc_load8_leu64(const unsigned char ptr[8])
{
  uint_fast64_t v0 = ptr[0];
  uint_fast64_t v1 = ptr[1];
  uint_fast64_t v2 = ptr[2];
  uint_fast64_t v3 = ptr[3];
  uint_fast64_t v4 = ptr[4];
  uint_fast64_t v5 = ptr[5];
  uint_fast64_t v6 = ptr[6];
  uint_fast64_t v7 = ptr[7];
  return ((v0 << (8 * 0)) | (v1 << (8 * 1))
          | (v2 << (8 * 2)) | (v3 << (8 * 3))
          | (v4 << (8 * 4)) | (v5 << (8 * 5))
          | (v6 << (8 * 6)) | (v7 << (8 * 7)));
}
#endif

#ifndef HAVE_STDC_STORE8_LEU16
static inline void
stdc_store8_leu16(uint_least16_t value, unsigned char ptr[2])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
}
#endif

#ifndef HAVE_STDC_STORE8_LEU32
static inline void
stdc_store8_leu32(uint_least32_t value, unsigned char ptr[4])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
  ptr[2] = (value >> 16) & 0xFFU;
  ptr[3] = (value >> 24) & 0xFFU;
}
#endif

#ifndef HAVE_STDC_STORE8_LEU64
static inline void
stdc_store8_leu64(uint_least64_t value, unsigned char ptr[8])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
  ptr[2] = (value >> 16) & 0xFFU;
  ptr[3] = (value >> 24) & 0xFFU;
  ptr[4] = (value >> 32) & 0xFFU;
  ptr[5] = (value >> 40) & 0xFFU;
  ptr[6] = (value >> 48) & 0xFFU;
  ptr[7] = (value >> 56) & 0xFFU;
}
#endif

#endif /* CAML_INTERNALS */
#endif /* CAML_STDBIT_H */
