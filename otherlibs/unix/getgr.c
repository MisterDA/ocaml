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
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "caml/unixsupport.h"
#include <unistd.h>
#include <grp.h>

#if !defined(HAVE_GETGRNAM_R) || !defined(HAVE_GETGRID_R)
#include <errno.h>
#endif

static value alloc_group_entry(struct group *entry)
{
  CAMLparam0();
  CAMLlocal3(name, pass, mem);
  value res;

  name = caml_copy_string(entry->gr_name);
  /* on some platforms, namely Android, gr_passwd can be NULL,
     hence this workaround */
  pass = caml_copy_string(entry->gr_passwd ? entry->gr_passwd : "");
  mem = caml_copy_string_array((const char**)entry->gr_mem);
  res = caml_alloc_small(4, 0);
  Field(res,0) = name;
  Field(res,1) = pass;
  Field(res,2) = Val_int(entry->gr_gid);
  Field(res,3) = mem;
  CAMLreturn(res);
}

CAMLprim value caml_unix_getgrnam(value name)
{
  value res;
  struct group *resultp;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

#ifdef HAVE_GETGRNAM_R
  long int initlen = sysconf(_SC_GETGR_R_SIZE_MAX);
  size_t len = initlen == -1 ? /* default */ 1024 : (size_t) initlen;
  struct group result;
  char *buffer = caml_stat_alloc(len);
  int e;
  while ((e = getgrnam_r(String_val(name), &result, buffer, len, &resultp))
         == ERANGE) {
    len *= 2;
    buffer = caml_stat_resize(buffer, len);
  }
#else
  errno = 0;
  resultp = getgrnam(String_val(name));
#endif
  if (resultp == NULL) {
#if HAVE_GETGRNAM_R
    caml_stat_free(buffer);
    if (e == EINTR)
      caml_unix_error(e, "getgrnam_r", Nothing);
#else
    if (errno == EINTR)
      caml_uerror("getgrnam", Nothing);
#endif
    caml_raise_not_found();
  }
  res = alloc_group_entry(resultp);
#if HAVE_GETGRNAM_R
  caml_stat_free(buffer);
#endif
  return res;
}

CAMLprim value caml_unix_getgrgid(value gid)
{
  value res;
  struct group *resultp;

#ifdef HAVE_GETGRGID_R
  long int initlen = sysconf(_SC_GETGR_R_SIZE_MAX);
  size_t len = initlen == -1 ? /* default */ 1024 : (size_t) initlen;
  struct group result;
  char *buffer = caml_stat_alloc(len);
  int e;
  while ((e = getgrgid_r(Int_val(gid), &result, buffer, len, &resultp))
         == ERANGE) {
    len *= 2;
    buffer = caml_stat_resize(buffer, len);
  }
#else
  errno = 0;
  resultp = getgrgid(Int_val(gid));
#endif
  if (resultp == NULL) {
#if HAVE_GETGRGID_R
    caml_stat_free(buffer);
    if (e == EINTR)
      caml_unix_error(e, "getgrnam_r", Nothing);
#else
    if (errno == EINTR)
      caml_uerror("getgrnam", Nothing);
#endif
    caml_raise_not_found();
  }
  res = alloc_group_entry(resultp);
#if HAVE_GETGRGID_R
  caml_stat_free(buffer);
#endif
  return res;
}
