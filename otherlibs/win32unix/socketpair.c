#include <caml/mlvalues.h>
#include "unixsupport.h"
#include <caml/io.h>

#ifdef HAS_SOCKETPAIR

extern int socket_domain_table[], socket_type_table[];

CAMLprim value unix_socketpair(value cloexec, value domain, value type,
                               value proto)
{
    CAMLparam4(cloexec, domain, type, proto);
    CAMLlocal1(res);
    int sv[2];
    if (socketpair(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto), sv) == -1) {
      win32_maperr(WSAGetLastError());
      uerror("socket", Nothing);
    }

    /* This is a best effort, not guaranteed to work, so don't fail on error */
    SetHandleInformation((HANDLE) sv[0],
                         HANDLE_FLAG_INHERIT,
                         unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);
    SetHandleInformation((HANDLE) sv[1],
                         HANDLE_FLAG_INHERIT,
                         unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

    res = caml_alloc_small(2, 0);
    Field(res,0) = win_alloc_socket(sv[0]);
    Field(res,1) = win_alloc_socket(sv[1]);
    CAMLreturn(res);
}

#elif defined(HAS_SOCKADDR_UN)

CAMLprim value unix_socketpair(value cloexec, value domain, value type,
                               value proto)
{
  CAMLparam4(cloexec, domain, type, proto);
  CAMLlocal(res);

  union sock_addr_union addr;
  TCHAR dirname[MAX_PATH];
  TCHAR path[MAX_PATH];
  size_t pathlen;

  SOCKET listener = INVALID_SOCKET,
    server = INVALID_SOCKET,
    client = INVALID_SOCKET;

  fd_set writefds, exceptfds;
  u_long non_block;

  char *bufferr;
  int saved_errno;
  DWORD drc;
  UINT urc;
  int rc;

  if (socket_domain_table[Int_val(domain)] != PF_UNIX) {
    uerror("socketpair: only PF_UNIX is supported", Nothing);
  }
  if (socket_type_table[Int_val(type)] != SOCK_STREAM) {
    uerror("socketpair: only SOCK_STREAM is supported", Nothing);
  }

  drc = GetTempPath(sizeof(dirname), dirname);
  if (drc == 0) {
     uerror("socketpair: get temporary path", Nothing);
  }

  urrc = GetTempFileName(dirname, TEXT("osp"), 0U, path);
  if (urc == 0U) {
    uerror("socketpair: get temporary file name", Nothing);
  }

  pathlen = strlen(path);
  if (pathlen >= sizeof(struct sockaddr_un) - offsetof(struct sockaddr_un, sun_path)) {
    uerror("socketpair: path too long to fit in sun_path", Nothing);
  }

  listener = socket(socket_domain_table[Int_val(domain)],
                    socket_type_table[Int_val(type)],
                    Int_val(proto));
  if (listener == INVALID_SOCKET) {
      win32_maperr(WSAGetLastError());
      uerror("socketpair: listener socket", Nothing);
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) listener,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  memset(addr, 0, sizeof(addr));
  addr.s_unix.sun_family = socket_domain_table[Int_val(domain)];
  memcpy(addr, path, pathlen);

  if (DeleteFile(path) == 0) {
    win32_maperr(GetLastError());
    if (errno != ENOENT) {
      saved_errno = errno;
      closesocket(listener);
      errno = saved_errno;
      uerror("socketpair: delete file", Nothing);
    }
  }

  rc = bind(listener, (const sockaddr *)addr.s_unix, sizeof(addr.s_unix));
  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: bind";
    goto fail_listener;
  }

  rc = listen(listener, 1);
  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: listen";
    goto fail_listener;
  }

  client = socket(socket_domain_table[Int_val(domain)],
                    socket_type_table[Int_val(type)],
                    Int_val(proto));
  if (client == INVALID_SOCKET) {
    bufferr = "socketpair: client socket";
    goto fail_listener;
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) client,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  non_block = 1UL;
  if (ioctlsocket(client, FIONBIO, &non_block) == SOCKET_ERRROR) {
    bufferr = "socketpair: set nonblock";
    goto fail_client;
  }

  rc = connect(client, (const struct sockaddr *)addr.s_unix, sizeof(addr.s_unix));
  if (rc == SOCKET_ERROR) {
    drc = WSAGetLastError();
    if (drc != WSAWOULDBLOCK) {
      win32_mapperr(drc);
      saved_errno = errno;
      closesocket(listener);
      closesocket(client);
      errno = saved_errno;
      uerror("socketpair: connect", Nothing);
    }
  }

  server = accept(listener, NULL, NULL);
  if (server == INVALID_SOCKET) {
    bufferr = "socketpair: set nonblock";
    goto fail_client;
  }
  /* This is a best effort, not guaranteed to work, so don't fail on error */
  SetHandleInformation((HANDLE) server,
                       HANDLE_FLAG_INHERIT,
                       unix_cloexec_p(cloexec) ? 0 : HANDLE_FLAG_INHERIT);

  closesocket(listener);

  FD_ZERO(&writefds);
  FD_SET(client, &writefds);
  FD_ZERO(&exceptfds);
  FD_SET(client, &exceptfds);

  caml_enter_blocking_section();
  rc = select(0, NULL, &writefds, &readfds, NULL /* blocking */);
  caml_leave_blocking_section();

  if (rc == SOCKET_ERROR) {
    bufferr = "socketpair: select";
    goto fail_pair;
  }

   if (FD_ISSET(client, &exceptfds)) {
    char buf[1024];
    rc = getsockopt(client, SOL_SOCKET, SO_ERROR, buf, sizeof(buf));
    if (rc == SOCKET_ERROR) {
      bufferr = "socketpair: getsockopt";
      goto fail_pair;
    } else {
      saved_errno = errno;
      closesocket(server);
      closesocket(client);
      errno = saved_errno;
      /* FIXME: a better way to report this error? */
      uerror(buf, Nothing);
    }
  }

   if (!FD_ISSET(client, &writefds)) {
     /* Should not happen. */
     bufferr = "socketpair: select";
     goto fail_pair;
   }

   non_block = 0UL;
   if (ioctlsocket(client, FIONBIO, &non_block) != SOCKET_ERROR) {
     bufferr = "socketpair: clear no block";
     goto fail_pair;
   }

   if (DeleteFile(path) == 0) {
     win32_maperr(GetLastError());
     if (errno != ENOENT) {
       saved_errno = errno;
       closesocket(server);
       closesocket(client);
       errno = saved_errno;
       uerror("socketpair: delete file", Nothing);
     }
   }

   res = caml_alloc_small(2, 0);
   Field(res, 0) = win_alloc_socket(server);
   Field(res, 1) = win_alloc_socket(client);
   CAMLreturn(res);

fail_listener:
   win32_maperr(WSAGetLastError());
   saved_errno = errno;
   closesocket(listener);
   errno = saved_errno;
   uerror(bufferr, Nothing);

fail_client:
   win32_maperr(WSAGetLastError());
   saved_errno = errno;
   closesocket(listener);
   closesocket(client);
   errno = saved_errno;
   uerror(bufferr, Nothing);

fail_pair:
   win32_maperr(WSAGetLastError());
   saved_errno = errno;
   closesocket(server);
   closesocket(client);
   errno = saved_errno;
   uerror(bufferr, Nothing);
}

#else

CAMLprim value unix_socketpair(value domain, value type, value proto)
{ caml_invalid_argument("socketpair not implemented"); }

#endif
