/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"

int socket_domain_table[] = {
  PF_UNIX, PF_INET,
#if defined(HAS_IPV6)
  PF_INET6
#else
  0
#endif
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

int socket_flags_table[] = {
  WSA_FLAG_OVERLAPPED
};

CAMLprim value unix_socket(value cloexec, value flags, value domain, value type,
                           value proto)
{
  CAMLparam5(cloexec, flags, domain, type, proto);
  CAMLlocal1(l);
  SOCKET s;
  DWORD dwFlags = 0;

  /* [flags] is a [socket_flags list option]. */
  if (Is_block(flags)) {
    l = Field(flags, 0);
    while (Is_block(l)) {
      dwFlags |= socket_flags_table[Int_val(Field(l, 0))];
      l = Field(l, 1);
    }
  }

  #ifndef HAS_IPV6
  /* IPv6 requires WinSock2, we must raise an error on PF_INET6 */
  if (Int_val(domain) >= sizeof(socket_domain_table)/sizeof(int)) {
    win32_maperr(WSAEPFNOSUPPORT);
    uerror("socket", Nothing);
  }
  #endif

  s = WSASocket(socket_domain_table[Int_val(domain)],
                socket_type_table[Int_val(type)],
                Int_val(proto),
                NULL,
                0,
                dwFlags);
  if (s == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  win_set_cloexec((HANDLE) s, cloexec);
  CAMLreturn(win_alloc_socket(s));
}
