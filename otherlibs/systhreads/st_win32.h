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

#define CAML_INTERNALS
#define WIN32_LEAN_AND_MEAN
#include <caml/misc.h>
#include <windows.h>

typedef DWORD st_timeout;

Caml_inline st_timeout st_timeout_of_msec(int msec)
{
  return msec;
}

Caml_inline void st_msleep(const st_timeout *timeout)
{
  DWORD msec = *timeout;
  const struct timespec req =
    { .tv_sec = msec / MSEC_PER_SEC, .tv_nsec = msec % MSEC_PER_SEC };
  caml_win32_nanosleep(&req, NULL);
}

#include "st_pthreads.h"
