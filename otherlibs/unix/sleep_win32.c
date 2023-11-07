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

#define NSEC_PER_SEC UINT64_C(1000000000)

/* from win32.c */
extern void caml_win32_nanosleep(uint64_t sec, uint64_t nsec);

CAMLprim value caml_unix_sleep(value timeout_sec)
{
  double tmint_sec, tmfrac_sec;
  tmfrac_sec = modf(Double_val(timeout_sec), &tmint_sec);
  caml_enter_blocking_section();
  caml_win32_nanosleep((uint64_t)tmint_sec,
                       (uint64_t)(tmfrac_sec * NSEC_PER_SEC));
  caml_leave_blocking_section();
  return Val_unit;
}
