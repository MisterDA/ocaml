/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

typedef int st_retcode;

/* Variables used to stop "tick" threads */
static atomic_uintnat tick_thread_stop[Max_domains];
#define Tick_thread_stop tick_thread_stop[Caml_state->id]

/* OS-specific initialization */

static int st_initialize(void)
{
  atomic_store_release(&Tick_thread_stop, 0);
  return 0;
}

static uintnat st_masterlock_waiters(st_masterlock * m)
{
  return atomic_load_acquire(&m->waiters);
}

static void st_bt_lock_acquire(st_masterlock *m) {

  /* We do not want to signal the backup thread if it is not "working"
     as it may very well not be, because we could have just resumed
     execution from another thread right away. */
  if (caml_bt_is_in_blocking_section()) {
    caml_bt_enter_ocaml();
  }

  caml_acquire_domain_lock();

  return;
}

static void st_bt_lock_release(st_masterlock *m) {

  /* Here we do want to signal the backup thread iff there's
     no thread waiting to be scheduled, and the backup thread is currently
     idle. */
  if (st_masterlock_waiters(m) == 0 &&
      caml_bt_is_in_blocking_section() == 0) {
    caml_bt_exit_ocaml();
  }

  caml_release_domain_lock();

  return;
}

/* The tick thread: interrupt the domain periodically to force preemption  */

static void * caml_thread_tick(void * arg)
{
  int *domain_id = (int *) arg;

  caml_init_domain_self(*domain_id);
  caml_domain_state *domain = Caml_state;

  while(! atomic_load_acquire(&Tick_thread_stop)) {
    st_msleep(Thread_timeout);

    atomic_store_release(&domain->requested_external_interrupt, 1);
    caml_interrupt_self();
  }
  return NULL;
}
