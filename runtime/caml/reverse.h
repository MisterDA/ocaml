/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Swap byte-order in 16, 32, and 64-bit integers or floats */

#ifndef CAML_REVERSE_H
#define CAML_REVERSE_H

#ifdef CAML_INTERNALS

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#include <stdint.h>
#include <string.h>

#if __has_builtin(__builtin_bswap16) && __has_builtin(__builtin_bswap32) && \
    __has_builtin(__builtin_bswap64)
#define Reverse_N(N, dst, src) do {           \
  uint ## N ## _t x;                          \
  memcpy(&x, (src), sizeof(uint ## N ## _t)); \
  x = __builtin_bswap ## N (x);               \
  memcpy((dst), &x, sizeof(uint ## N ## _t)); \
} while (0)
#define Reverse_16(dst, src) Reverse_N(16, dst, src)
#define Reverse_32(dst, src) Reverse_N(32, dst, src)
#define Reverse_64(dst, src) Reverse_N(64, dst, src)

#elif defined(_MSC_VER)
#include <stdlib.h>
#define Reverse_N(N, name, dst, src) do {     \
  uint ## N ## _t x;                          \
  memcpy(&x, (src), sizeof(uint ## N ## _t)); \
  x = _byteswap_ ## name (x);                 \
  memcpy((dst), &x, sizeof(uint ## N ## _t)); \
} while(0)
#define Reverse_16(dst, src) Reverse_N(16, ushort, dst, src)
#define Reverse_32(dst, src) Reverse_N(32,  ulong, dst, src)
#define Reverse_64(dst, src) Reverse_N(64, uint64, dst, src)

#else
#define Reverse_16(dst,src) do {              \
  uint16_t x;                                 \
  memcpy(&x, (src), sizeof(uint16_t));        \
  x = x << 8 | x >> 8;                        \
  memcpy((dst), &x, sizeof(uint16_t));        \
} while (0)

#define Reverse_32(dst,src) do {                                    \
  uint32_t x;                                                       \
  memcpy(&x, (src), sizeof(uint32_t));                              \
  x = x >> 24 | (x >> 8 & 0xff00) | (x << 8 & 0xff0000) | x << 24;  \
  memcpy((dst), &x, sizeof(uint32_t));                              \
} while(0)

#define Reverse_64(dst,src) do {                                          \
  uint64_t x;                                                             \
  memcpy(&x, (src), sizeof(uint64_t));                                    \
  x = x << 56 | (x << 40 & UINT64_C(0xff) << 48) |                        \
    (x << 24 & UINT64_C(0xff) << 40) | (x <<  8 & UINT64_C(0xff) << 32) | \
    (x >>  8 & UINT64_C(0xff) << 24) | (x >> 24 & UINT64_C(0xff) << 16) | \
    (x >> 40 & UINT64_C(0xff) << 8)  |  x >> 56;                          \
  memcpy((dst), &x, sizeof(uint64_t));                                    \
} while (0)
#endif

#define Perm_index(perm,i) ((perm >> (i * 4)) & 0xF)

#define Permute_64(dst,perm_dst,src,perm_src) {                             \
  char * _p;                                                                \
  char _a, _b, _c, _d, _e, _f, _g, _h;                                      \
  _p = (char *) (src);                                                      \
  _a = _p[Perm_index(perm_src, 0)];                                         \
  _b = _p[Perm_index(perm_src, 1)];                                         \
  _c = _p[Perm_index(perm_src, 2)];                                         \
  _d = _p[Perm_index(perm_src, 3)];                                         \
  _e = _p[Perm_index(perm_src, 4)];                                         \
  _f = _p[Perm_index(perm_src, 5)];                                         \
  _g = _p[Perm_index(perm_src, 6)];                                         \
  _h = _p[Perm_index(perm_src, 7)];                                         \
  _p = (char *) (dst);                                                      \
  _p[Perm_index(perm_dst, 0)] = _a;                                         \
  _p[Perm_index(perm_dst, 1)] = _b;                                         \
  _p[Perm_index(perm_dst, 2)] = _c;                                         \
  _p[Perm_index(perm_dst, 3)] = _d;                                         \
  _p[Perm_index(perm_dst, 4)] = _e;                                         \
  _p[Perm_index(perm_dst, 5)] = _f;                                         \
  _p[Perm_index(perm_dst, 6)] = _g;                                         \
  _p[Perm_index(perm_dst, 7)] = _h;                                         \
}

#endif /* CAML_INTERNALS */

#endif /* CAML_REVERSE_H */
