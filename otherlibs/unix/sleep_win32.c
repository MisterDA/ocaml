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
#include "caml/unixsupport.h"
#include <math.h>

/* from win32.c */
extern void caml_win32_nanosleep(__int64 sec, __int64 nsec);

CAMLprim value caml_unix_sleep(value t)
{
  double dint, dfrac;
  dfrac = modf(Double_val(t), &dint);
  caml_enter_blocking_section();
  caml_win32_nanosleep(dint, dfrac * 1e9);
  caml_leave_blocking_section();
  return Val_unit;
}
