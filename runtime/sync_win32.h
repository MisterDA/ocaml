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

/* Windows implementation of the user facing Mutex and Condition */
/* To be included in runtime/sync.c */

#ifndef CAML_SYNC_WIN32_H
#define CAML_SYNC_WIN32_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "caml/sync.h"
#include "caml/osdeps.h"

typedef int sync_retcode;

/* Mutexes */

Caml_inline int sync_mutex_create(sync_mutex * res)
{
  sync_mutex m = caml_stat_alloc_noexc(sizeof(SRWLOCK));
  if (m == NULL) return ENOMEM;
  InitializeSRWLock(m);
  *res = m;
  return 0;
}

Caml_inline int sync_mutex_destroy(sync_mutex m)
{
  caml_stat_free(m);
  return 0;
}

Caml_inline int sync_mutex_lock(sync_mutex m)
{
  AcquireSRWLockExclusive(m);
  return 0;
}

#define MUTEX_PREVIOUSLY_UNLOCKED 0
#define MUTEX_ALREADY_LOCKED EBUSY

Caml_inline int sync_mutex_trylock(sync_mutex m)
{
  return TryAcquireSRWLockExclusive(m) ?
    MUTEX_PREVIOUSLY_UNLOCKED : MUTEX_ALREADY_LOCKED;
}

Caml_inline int sync_mutex_unlock(sync_mutex m)
{
  ReleaseSRWLockExclusive(m);
  return 0;
}

/* Condition variables */

Caml_inline int sync_condvar_create(sync_condvar * res)
{
  sync_condvar c = caml_stat_alloc_noexc(sizeof(CONDITION_VARIABLE));
  if (c == NULL) return ENOMEM;
  InitializeConditionVariable(c);
  *res = c;
  return 0;
}

Caml_inline int sync_condvar_destroy(sync_condvar c)
{
  caml_stat_free(c);
  return 0;
}

Caml_inline int sync_condvar_signal(sync_condvar c)
{
  WakeConditionVariable(c);
  return 0;
}

Caml_inline int sync_condvar_broadcast(sync_condvar c)
{
  WakeAllConditionVariable(c);
  return 0;
}

Caml_inline int sync_condvar_wait(sync_condvar c, sync_mutex m)
{
  if (!SleepConditionVariableSRW(c, m, INFINITE, 0 /* exclusive */)) {
    int rc = caml_posixerr_of_win32err(GetLastError());
    return rc == 0 ? EINVAL : rc;
  }
  return 0;
}

#endif
