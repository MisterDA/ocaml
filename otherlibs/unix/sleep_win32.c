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

#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "unixsupport.h"

extern void caml_win32_usleep(__int64 usecs); /* from win32.c */

CAMLprim value caml_unix_sleep(value duration)
{
  double d = Double_val(duration);
  caml_enter_blocking_section();
  caml_win32_usleep(d * 1e6);
  caml_leave_blocking_section();
  return Val_unit;
}
