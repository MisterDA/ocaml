/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Win32 implementation of the "st" interface */

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#include <windows.h>

#define NSEC_PER_MSEC UINT64_C(1000000)

/* from win32.c */
extern void caml_win32_nanosleep(uint64_t sec, uint64_t nsec);

Caml_inline void st_msleep(int msec)
{
  caml_win32_nanosleep(msec / NSEC_PER_MSEC, msec % NSEC_PER_MSEC);
}

#include "st_pthreads.h"
