#include <assert.h>
#include <stdatomic.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/tsan.h>

#define NSEC_PER_USEC UINT64_C(1000)
#define NSEC_PER_MSEC UINT64_C(1000000)
#define NSEC_PER_SEC  UINT64_C(1000000000)

#define MAX_WAITGROUP   8
#define SPIN_WAIT_NSEC  (10 /* msec */ * NSEC_PER_MSEC)

/* waitgroup inspired by Golang's `sync.WaitGroup`. This version does *not*
 * allow to restart a waitgroup. */
typedef struct {
  unsigned    limit; /* Number of threads participating in the checkpoint */
  atomic_uint count; /* Number of threads that have reached the checkpoint */
} waitgroup;

static waitgroup waitgroups[MAX_WAITGROUP] = { 0 };

static atomic_uint index = 0;

CAMLno_tsan static waitgroup* wg_get(unsigned idx)
{
  assert(idx < MAX_WAITGROUP);

  waitgroup* wg = &waitgroups[idx];
  return wg;
}

CAMLno_tsan value wg_create(value n)
{
  waitgroup* wg = wg_get(index);

  wg->limit = Int_val(n);
  wg->count = 0;
  return Val_int(index++);
}

CAMLno_tsan value wg_finish(value t)
{
  waitgroup* wg = wg_get(Int_val(t));

  wg->count += 1;
  return Val_unit;
}

CAMLno_tsan value wg_wait(value t)
{
  waitgroup* wg = wg_get(Int_val(t));

  /* Always sleep at least once, even for the last thread to reach the
   * checkpoint. This allows TSan to always generate a report with a
   * 'As if synchronized via sleep' section. */
  do {
#ifdef HAS_NANOSLEEP
    const struct timespec ts = {
      .tv_sec = SPIN_WAIT_NSEC / NSEC_PER_SEC,
      .tv_nsec = SPIN_WAIT_NSEC % NSEC_PER_SEC };
    nanosleep(&ts, NULL);
#else
    usleep(SPIN_WAIT_NS / NSEC_PER_USEC);
#endif
  }
  while (wg->count != wg->limit);
  return Val_unit;
}
