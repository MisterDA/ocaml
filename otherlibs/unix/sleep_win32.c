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

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include "caml/unixsupport.h"
#include <math.h>

CAMLprim value caml_unix_sleep(value sec)
{
  const struct timespec req = caml_timespec_of_sec(Double_val(sec));
  caml_enter_blocking_section();
  caml_win32_nanosleep(&req, NULL);
  caml_leave_blocking_section();
  return Val_unit;
}
