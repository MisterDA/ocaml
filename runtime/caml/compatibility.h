/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2024 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Definitions for compatibility with old identifiers. */

#ifndef CAML_COMPATIBILITY_H
#define CAML_COMPATIBILITY_H

#define NO_NAKED_POINTERS 1
#define CAML_SAFE_STRING 1

#ifdef WORDS_BIGENDIAN
#define ARCH_BIG_ENDIAN 1
#endif

#define HAS_STDINT_H 1 /* Deprecated since OCaml 5.3 */

/* HAS_NANOSECOND_STAT is deprecated since OCaml 5.3 */
#if defined(HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC)
#  define HAS_NANOSECOND_STAT 1
#elif defined(HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC)
#  define HAS_NANOSECOND_STAT 2
#elif defined(HAVE_STRUCT_STAT_ST_ATIMENSEC)
#  define HAS_NANOSECOND_STAT 3
#endif

#ifdef HAVE_AFUNIX_H
#define HAS_AFUNIX_H 1
#endif

#ifdef HAVE_DIRENT_H
#define HAS_DIRENT_H 1
#endif

#ifdef HAVE_PTHREAD_NP_H
#define HAS_PTHREAD_NP_H 1
#endif

#ifdef HAVE_SYS_MMAN_H
#define HAS_SYS_MMAN_H 1
#endif

#ifdef HAVE_SYS_SELECT_H
#define HAS_SYS_SELECT_H 1
#endif

#ifdef HAVE_SYS_SHM_H
#define HAS_SYS_SHM_H 1
#endif

#ifdef HAVE_UNISTD_H
#define HAS_UNISTD 1
#endif

#ifdef HAVE_ACCEPT4
#define HAS_ACCEPT4 1
#endif

#ifdef HAVE_DUP3
#define HAS_DUP3 1
#endif

#ifdef HAVE_EXECVPE
#define HAS_EXECVPE 1
#endif

#ifdef HAVE_GETAUXVAL
#define HAS_GETAUXVAL 1
#endif

#ifdef HAVE_GETCWD
#define HAS_GETCWD 1
#endif

#ifdef HAVE_GETENTROPY
#define HAS_GETENTROPY 1
#endif

#ifdef HAVE_GETGROUPS
#define HAS_GETGROUPS 1
#endif

#ifdef HAVE_GETRUSAGE
#define HAS_GETRUSAGE 1
#endif

#ifdef HAVE_GETHOSTNAME
#define HAS_GETHOSTNAME 1
#endif

#ifdef HAVE_GETTIMEOFDAY
#define HAS_GETTIMEOFDAY 1
#endif

#ifdef HAVE_INET_ATON
#define HAS_INET_ATON 1
#endif

#ifdef HAVE_INITGROUPS
#define HAS_INITGROUPS 1
#endif

#ifdef HAVE_ISSETUGID
#define HAS_ISSETUGID 1
#endif

#ifdef HAVE_LOCKF
#define HAS_LOCKF 1
#endif

#ifdef HAVE_MKFIFO
#define HAS_MKFIFO 1
#endif

#ifdef HAVE_MKSTEMP
#define HAS_MKSTEMP 1
#endif

#ifdef HAVE_MKTIME
#define HAS_MKTIME 1
#endif

#ifdef HAVE_NANOSLEEP
#define HAS_NANOSLEEP 1
#endif

#ifdef HAVE_NICE
#define HAVE_NICE 1
#endif

#ifdef HAVE_PIPE2
#define HAS_PIPE2 1
#endif

#ifdef HAVE_PUTENV
#define HAS_PUTENV 1
#endif

#ifdef HAVE_PWRITE
#define HAS_PWRITE 1
#endif

#ifdef HAVE_REALPATH
#define HAS_REALPATH 1
#endif

#ifdef HAVE_REWINDDIR
#define HAS_REWINDDIR 1
#endif

#ifdef HAVE_SECURE_GETENV
#define HAS_SECURE_GETENV 1
#endif

#ifdef HAVE___SECURE_GETENV
#define HAS___SECURE_GETENV 1
#endif

#ifdef HAVE_SETGROUPS
#define HAS_SETGROUPS 1
#endif

#ifdef HAVE_SETSID
#define HAS_SETSID 1
#endif

#ifdef HAVE_SETITIMER
#define HAS_SETITIMER 1
#endif

#ifdef HAVE_SHMAT
#define HAS_SHMAT 1
#endif

#ifdef HAVE_SIGWAIT
#define HAS_SIGWAIT 1
#endif

#ifdef HAVE_SOCKETPAIR
#define HAS_SOCKETPAIR 1
#endif

#ifdef HAVE_STRTOD_L
#define HAS_STRTOD_L 1
#endif

#ifdef HAVE_SYSTEM
#define HAS_SYSTEM 1
#endif

#ifdef HAVE_TIMES
#define HAS_TIMES 1
#endif

#ifdef HAVE_UNAME
#define HAS_UNAME 1
#endif

#ifdef HAVE_UTIMES
#define HAS_UTIMES 1
#endif

#ifdef HAVE_WAITPID
#define HAS_WAITPID 1
#endif

#ifdef HAVE_WAIT4
#define HAS_WAIT4 1
#endif

#ifdef HAVE_SOCKLEN_T
#define HAS_SOCKLEN_T 1
#endif

#ifdef SIZEOF_LONG_P
#define SIZEOF_PTR SIZEOF_LONG_P
#endif

#ifdef SIZEOF_LONG_LONG
#define SIZEOF_LONGLONG SIZEOF_LONG_LONG
#endif

#ifdef HAVE_SOCKETS
#define HAS_SOCKETS 1
#endif

#ifdef HAVE_IPV6
#define HAS_IPV6 1
#endif

#ifdef HAVE_BROKEN_PRINTF
#define HAS_BROKEN_PRINTF 1
#endif

#ifdef HAVE_ARCH_CODE32
#define HAS_ARCH_CODE32
#endif

#endif  /* CAML_COMPATIBILITY_H */
