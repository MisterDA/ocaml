/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2021 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include <errno.h>
#include <tchar.h>

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#include <ws2tcpip.h>

extern int socket_domain_table[]; /* from socket.c */
extern int socket_type_table[]; /* from socket.c */

#ifdef HAS_SOCKETPAIR

CAMLprim value unix_socketpair(value cloexec, value domain, value type,
                               value proto)
{
  CAMLparam4(cloexec, domain, type, proto);
  int sv[2];
  CAMLlocal3(res, s0, s1);

  if (socketpair(socket_domain_table[Int_val(domain)],
                 socket_type_table[Int_val(type)],
                 Int_val(proto), sv) == SOCKET_ERROR) {
    win32_maperr(WSAGetLastError());
    uerror("socketpair", Nothing);
  }

  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) sv[0],
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);
  SetHandleInformation((HANDLE) sv[1],
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  s0 = win_alloc_socket(sv[0]);
  s1 = win_alloc_socket(sv[1]);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = s0;
  Field(res, 1) = s1;
  CAMLreturn(res);
}

#else

CAMLprim value unix_socketpair(value cloexec, value domain, value type,
                               value proto)
{
  CAMLparam4(cloexec, domain, type, proto);

  TCHAR dirname[MAX_PATH], path[MAX_PATH];
  union sock_addr_union addr;
  socklen_param_type socklen;

  SOCKET listener = INVALID_SOCKET,
    server = INVALID_SOCKET,
    client = INVALID_SOCKET;

  fd_set writefds, exceptfds;
  u_long non_block, peerid = 0UL;

  char *bufferr;
  int saved_errno;
  DWORD drc;
  UINT urc;
  int rc;

  CAMLlocal3(res, s0, s1);
  errno = 0;

  if (socket_domain_table[Int_val(domain)] != PF_UNIX) {
    win32_maperr(WSAEPFNOSUPPORT);
    uerror("socketpair: only PF_UNIX is supported", Nothing);
  }

  if (socket_type_table[Int_val(type)] != SOCK_STREAM) {
    win32_maperr(WSAESOCKTNOSUPPORT);
    uerror("socketpair: only SOCK_STREAM is supported", Nothing);
  }

  drc = GetTempPath(sizeof(dirname), dirname);
  if (drc == 0) {
    win32_maperr(GetLastError());
    uerror("socketpair: get temporary path", Nothing);
  }

  urc = GetTempFileName(dirname, TEXT("osp"), 0U, path);
  if (urc == 0U) {
    win32_maperr(GetLastError());
    uerror("socketpair: get temporary file name", Nothing);
  }

  memset(&addr, 0, sizeof(addr));
  addr.s_unix.sun_family = PF_UNIX;
  socklen = sizeof(addr.s_unix);

#ifdef WINDOWS_UNICODE
  /* sun_path needs to be set in UTF-8 */
  rc = WideCharToMultiByte(CP_UTF8, 0, path, -1, NULL, 0, NULL, NULL);
  if (rc >= UNIX_PATH_MAX) {
    uerror("socketpair: path too long to fit in sun_path", Nothing);
  }
  rc = WideCharToMultiByte(CP_UTF8, 0, path, -1, addr.s_unix.sun_path, rc,
                           NULL, NULL);
  if (rc == 0) {
    win32_maperr(GetLastError());
    uerror("socketpair: convert to utf-8", Nothing);
  }
#endif

  listener = socket(socket_domain_table[Int_val(domain)],
                    socket_type_table[Int_val(type)],
                    Int_val(proto));
  if (listener == INVALID_SOCKET) {
    bufferr = "socketpair: listener socket";
    goto fail;
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) listener,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  if (DeleteFile(path) == 0) {
    win32_maperr(GetLastError());
    if (errno != ENOENT) {
      saved_errno = errno;
      closesocket(listener);
      unix_error(saved_errno, "socketpair: delete file", Nothing);
    }
  }

  rc = bind(listener, (struct sockaddr *)&addr, socklen);
  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: bind";
    goto fail;
  }

  rc = listen(listener, 1);
  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: listen";
    goto fail;
  }

  client = socket(socket_domain_table[Int_val(domain)],
                  socket_type_table[Int_val(type)],
                  Int_val(proto));
  if (client == INVALID_SOCKET) {
    bufferr = "socketpair: client socket";
    goto fail;
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) client,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  non_block = 1UL;
  if (ioctlsocket(client, FIONBIO, &non_block) == SOCKET_ERROR) {
    bufferr = "socketpair: set nonblock";
    goto fail;
  }

  rc = connect(client, (struct sockaddr *)&addr, socklen);
  if (rc == SOCKET_ERROR && WSAGetLastError() != WSAEWOULDBLOCK) {
    bufferr = "socketpair: connect";
    goto fail;
  }

  caml_enter_blocking_section();
  server = accept(listener, NULL, NULL);
  caml_leave_blocking_section();

  if (server == INVALID_SOCKET) {
    bufferr = "socketpair: set nonblock";
    goto fail;
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) server,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  closesocket(listener);
  listener = INVALID_SOCKET;

  FD_ZERO(&writefds);
  FD_SET(client, &writefds);
  FD_ZERO(&exceptfds);
  FD_SET(client, &exceptfds);

  caml_enter_blocking_section();
  rc = select(0, NULL, &writefds, &exceptfds, NULL /* blocking */);
  caml_leave_blocking_section();

  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: select";
    goto fail;
  }

  if (FD_ISSET(client, &exceptfds)) {
    int code, codelen = sizeof(int);
    rc = getsockopt(client, SOL_SOCKET, SO_ERROR, (char *)&code, &codelen);
    if (rc == SOCKET_ERROR) {
      bufferr = "socketpair: getsockopt";
      goto fail;
    } else {
      closesocket(server);
      closesocket(client);
      unix_error(code, "socketpair: select", Nothing);
    }
  }

  if (!FD_ISSET(client, &writefds)) {
    /* Should not happen. */
    bufferr = "socketpair: select";
    goto fail;
  }

  non_block = 0UL;
  if (ioctlsocket(client, FIONBIO, &non_block) == SOCKET_ERROR) {
    bufferr = "socketpair: clear nonblock";
    goto fail;
  }

  if (DeleteFile(path) == 0) {
    win32_maperr(GetLastError());
    if (errno != ENOENT) {
      saved_errno = errno;
      closesocket(server);
      closesocket(client);
      unix_error(saved_errno, "socketpair: delete file", Nothing);
    }
  }

  rc = WSAIoctl(client, SIO_AF_UNIX_GETPEERPID,
                NULL, 0U,
                &peerid, sizeof(peerid), &drc /* Windows bug: always 0 */,
                NULL, NULL);
  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: couldn't get peer id";
    goto fail;
  }

  if (peerid != GetCurrentProcessId()) {
    bufferr = "socketpair: another process took over the socket";
    goto fail;
  }

  s0 = win_alloc_socket(server);
  s1 = win_alloc_socket(client);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = s0;
  Field(res, 1) = s1;
  CAMLreturn(res);

fail:
  win32_maperr(WSAGetLastError());
  saved_errno = errno;
  if(listener != INVALID_SOCKET)
    closesocket(listener);
  if(client != INVALID_SOCKET)
    closesocket(client);
  if(server != INVALID_SOCKET)
    closesocket(server);
  unix_error(saved_errno, bufferr, Nothing);
}

#endif  /* HAS_SOCKETPAIR */

#endif  /* HAS_SOCKETS */
