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

#define CAML_INTERNALS

/* The bytecode interpreter */
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/instrtrace.h"
#include "caml/instruct.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/signals.h"
#include "caml/fiber.h"
#include "caml/domain.h"
#include "caml/globroots.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"

/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
        Caml_state->trap_sp_off offset to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable Caml_state->current_stack->sp. */

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name:
#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#    define Jumptbl_base &&lbl_ACC0
#  else
#    define Jumptbl_base 0
#    define jumptbl_base ((char *) 0)
#  endif
#  ifdef DEBUG
#    define Next goto next_instr
#  else
#    define Next goto *(void *)(jumptbl_base + *pc++)
#  endif
#  define Fallthrough ((void) 0)
#else
#  define Instruct(name) case name:
#  define Next break
#  define Fallthrough fallthrough
#endif

/* GC interface */

#define Setup_for_gc \
  { sp -= 3; sp[0] = accu; sp[1] = env; sp[2] = (value)pc; \
    domain_state->current_stack->sp = sp; }
#define Restore_after_gc \
  { sp = domain_state->current_stack->sp; accu = sp[0]; env = sp[1]; sp += 3; }
/* Do call asynchronous callbacks from allocation functions */
#define Enter_gc(dom_st, wosize) do {                            \
    Setup_for_gc;                                                \
    Alloc_small_enter_GC_flags(CAML_DO_TRACK | CAML_FROM_CAML,   \
                               dom_st, wosize);                  \
    Restore_after_gc;                                            \
  } while (0)

/* We store [pc+1] in the stack so that, in case of an exception, the
   first backtrace slot points to the event following the C call
   instruction. */
#define Setup_for_c_call \
  { sp -= 2; sp[0] = env; sp[1] = (value)(pc + 1); \
    domain_state->current_stack->sp = sp; }
#define Restore_after_c_call \
  { sp = domain_state->current_stack->sp; env = *sp; sp += 2; \
    caml_update_young_limit_after_c_call(domain_state);       \
  }

/* For VM threads purposes, an event frame must look like accu + a
   C_CALL frame + a RETURN 1 frame.
   TODO: now that VM threads are gone, we could get rid of that. But
   we need to make sure that this is not used elsewhere. */
#define Setup_for_event \
  { sp -= 6; \
    sp[0] = accu; /* accu */ \
    sp[1] = Val_unit; /* C_CALL frame: dummy environment */ \
    sp[2] = Val_unit; /* RETURN frame: dummy local 0 */ \
    sp[3] = (value) pc; /* RETURN frame: saved return address */  \
    sp[4] = env; /* RETURN frame: saved environment */ \
    sp[5] = Val_long(extra_args); /* RETURN frame: saved extra args */ \
    domain_state->current_stack->sp = sp; }
#define Restore_after_event \
  { sp = domain_state->current_stack->sp; accu = sp[0]; \
    pc = (code_t) sp[3]; env = sp[4]; extra_args = Long_val(sp[5]); \
    sp += 6; }

/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     domain_state->current_stack->sp = sp; }
#define Restore_after_debugger \
   { CAMLassert(sp == domain_state->current_stack->sp); \
     CAMLassert(sp[0] == accu); \
     CAMLassert(sp[2] == env); \
     sp += 4; }

#ifdef THREADED_CODE
#define Restart_curr_instr \
  goto *((void*)(jumptbl_base + caml_debugger_saved_instruction(pc - 1)))
#else
#define Restart_curr_instr \
  curr_instr = caml_debugger_saved_instruction(pc - 1); \
  goto dispatch_instr
#endif

Caml_inline void check_trap_barrier_for_exception
  (caml_domain_state* domain_state)
{
  if (domain_state->current_stack->id == domain_state->trap_barrier_block
      && domain_state->trap_sp_off >= domain_state->trap_barrier_off)
    caml_debugger(TRAP_BARRIER, Val_unit);
}

Caml_inline void check_trap_barrier_for_effect
  (caml_domain_state* domain_state)
{
  if (domain_state->current_stack->id == domain_state->trap_barrier_block){
    caml_debugger(TRAP_BARRIER, Val_unit);
  }else{
    struct stack_info *parent_stack
      = domain_state->current_stack->handler->parent;
    if (parent_stack != NULL
        && parent_stack->id == domain_state->trap_barrier_block
        && parent_stack->sp + 2 - Stack_high (parent_stack)
              /* Note: +2 is the same constant as in the REQ_UP_FRAME
                 case in caml_debugger() in debugger.c */
           == domain_state->trap_barrier_off){
      caml_debugger(TRAP_BARRIER, Val_unit);
    }
  }
}

/* Register optimization.
   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#if defined(__GNUC__) && !defined(DEBUG) && !defined(__INTEL_COMPILER) \
    && !defined(__llvm__)
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define ACCU_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define ACCU_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define ACCU_REG asm("r11")
#define JUMPTBL_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define ACCU_REG asm("$11")
#define JUMPTBL_BASE_REG asm("$12")
#endif
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG
#endif
#if defined(__ppc__) || defined(__ppc64__)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define ACCU_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define ACCU_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a3")
#define SP_REG asm("a4")
#define ACCU_REG asm("d7")
#endif
/* PR#4953: these specific registers not available in Thumb mode */
#if defined (__arm__) && !defined(__thumb__)
#define PC_REG asm("r6")
#define SP_REG asm("r8")
#define ACCU_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define ACCU_REG asm("38")
#define JUMPTBL_BASE_REG asm("39")
#endif
#ifdef __x86_64__
#define PC_REG asm("%r15")
#define SP_REG asm("%r14")
#define ACCU_REG asm("%r13")
#endif
#ifdef __aarch64__
#define PC_REG asm("%x19")
#define SP_REG asm("%x20")
#define ACCU_REG asm("%x21")
#define JUMPTBL_BASE_REG asm("%x22")
#endif
#endif
#ifdef DEBUG
static CAMLthread_local intnat caml_bcodcount;
#endif

static value raise_unhandled_effect;

#ifdef HAVE_TAIL_CALL_INTERP
/* tc_ctx is defined fully in the TC section below; forward-declare here so
   tc_handler_t can reference it before caml_bytecode_interpreter(). */
struct tc_ctx;
/* Handler function type (preserve_none: all registers caller-saved).
   Defined fully again in the TC section to pick up TC_PARAMS/TC_ARGS. */
typedef CAMLpreserve_none
value (*tc_handler_t)(code_t pc, value * sp, value accu, value env,
                      intnat extra_args, caml_domain_state * domain_state,
                      struct tc_ctx * ctx, const char * tc_base);
/* Dispatch table — populated by tc_init_dispatch_table(). */
static tc_handler_t tc_dispatch_table[FIRST_UNIMPLEMENTED_OP];
static void tc_init_dispatch_table(void);
static value caml_bytecode_interpreter_tc(code_t prog, asize_t prog_size,
                                          value initial_env,
                                          intnat initial_extra_args);
#endif

/* ===================================================================== */
/* Abstract dispatch layer                                                */
/*                                                                        */
/* The bodies of all instructions and of the shared control-flow blocks   */
/* live once, in runtime/interp_instructions.h, expressed in terms of the */
/* macros below.  That file is included twice: once here, to build the     */
/* switch / threaded-code interpreter as a single function, and once in    */
/* the tail-call section, to build one function per instruction.           */
/* ===================================================================== */

/* Macros that do not depend on the interpreter flavour. */

#define CAML_METHOD_CACHE

#define Lookup(obj, lab) Field (Field (obj, 0), Int_val(lab))

#define Integer_comparison(typ,opname,tst)              \
    Instruct(opname) {                                  \
      accu = Val_int((typ) accu tst (typ) *sp++); Next; \
    }

#define Integer_branch_comparison(typ,opname,tst)       \
    Instruct(opname) {                                  \
      if ( *pc++ tst (typ) Long_val(accu)) {            \
        pc += *pc ;                                     \
      } else {                                          \
        pc++ ;                                          \
      } Next;                                           \
    }

/* Resume a continuation.  Inlined at each use site (rather than shared as a
   block) so that, in the tail-call flavour, every tail call keeps the same
   uniform arity.  [accu] holds the continuation to resume; the underscore
   prefixes avoid clashing with locals named cont_tail/cont_head at the use
   sites. */
#define Do_resume(resume_fn_val, resume_arg_val) do {                   \
    struct stack_info* _cont_tail = Ptr_val(accu);                      \
    if (_cont_tail == NULL) {                                           \
      Setup_for_c_call;                                                 \
      caml_raise_continuation_already_resumed();                        \
    }                                                                   \
    struct stack_info* _cont_head = Stack_parent(_cont_tail);           \
    if (_cont_head == NULL) {                                           \
      /* Freshly allocated stack; entering this computation for the     \
         first time */                                                  \
      _cont_head = _cont_tail;                                          \
    }                                                                   \
    Stack_parent(_cont_tail) = Caml_state->current_stack;              \
    domain_state->current_stack->sp = sp;                              \
    domain_state->current_stack = _cont_head;                          \
    sp = domain_state->current_stack->sp;                              \
    domain_state->trap_sp_off = Long_val(sp[0]);                       \
    sp[0] = (resume_arg_val);                                          \
    accu = (resume_fn_val);                                            \
    pc = Code_val(accu);                                               \
    env = accu;                                                        \
    extra_args = 0;                                                    \
    Goto_check_stacks();                                               \
  } while(0)

/* Switch / threaded-code flavour of the abstract dispatch layer.
   Instruct, Next, Fallthrough and Restart_curr_instr are defined near the
   top of this file; the remaining macros are defined here.  A shared block
   is a plain label, a "goto" is a plain goto and Fallthrough_to relies on
   textual fall-through to the next instruction.  The tail-call section
   redefines all of these. */

#define Helper(name) name:
#define Fallthrough_to(name) Fallthrough
#define Goto_check_stacks() goto check_stacks
#define Goto_do_return() goto do_return
#define Goto_raise_notrace() goto raise_notrace
#define Goto_raise_exception() goto raise_exception
#define Goto_process_signal() goto process_signal
#define Initial_external_raise initial_external_raise
#define Initial_trap_sp_off    initial_trap_sp_off
#define Initial_stack_words    initial_stack_words

/* The interpreter itself */

CAMLno_tsan /* No need to TSan-instrument this (and pay a slowdown) function as
               TSan is not supported for bytecode. */
value caml_bytecode_interpreter(code_t prog, asize_t prog_size,
                                value initial_env, intnat initial_extra_args)
{
#ifdef PC_REG
  register code_t pc PC_REG;
  register value * sp SP_REG;
  register value accu ACCU_REG;
#else
  register code_t pc;
  register value * sp;
  register value accu;
#endif
#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#ifdef JUMPTBL_BASE_REG
  register const char * jumptbl_base JUMPTBL_BASE_REG;
#else
  register const char * jumptbl_base;
#endif
#endif
  value env;
  intnat extra_args;
  struct caml_exception_context * initial_external_raise;
  int initial_stack_words;
  intnat initial_trap_sp_off;
  volatile value raise_exn_bucket = Val_unit;
  struct longjmp_buffer raise_buf;
  caml_domain_state* domain_state = Caml_state;
  struct caml_exception_context exception_ctx =
    { &raise_buf, domain_state->local_roots, &raise_exn_bucket};
#ifndef THREADED_CODE
  opcode_t curr_instr;
#endif

#ifdef THREADED_CODE
#define OPCODE_LABEL(name) &&lbl_ ## name,
  static const void * const jumptable[] = {
    CAML_ZINC_OPCODES(OPCODE_LABEL)
  };
#undef OPCODE_LABEL
#endif

  if (prog == NULL) {           /* Interpreter is initializing */
    static opcode_t raise_unhandled_effect_code[] = { ACC, 0, RAISE };
    value raise_unhandled_effect_closure;

    caml_register_code_fragment(
      (char *) raise_unhandled_effect_code,
      (char *) raise_unhandled_effect_code +
      sizeof(raise_unhandled_effect_code),
      DIGEST_IGNORE, NULL);
#ifdef THREADED_CODE
    caml_init_thread_code(jumptable, Jumptbl_base);
    caml_thread_code(raise_unhandled_effect_code,
                     sizeof(raise_unhandled_effect_code));
#endif
#ifdef HAVE_TAIL_CALL_INTERP
    tc_init_dispatch_table();
    caml_init_tc_thread_code((void * const *)tc_dispatch_table,
                             (const void *)tc_dispatch_table[0]);
    caml_tc_thread_code(raise_unhandled_effect_code,
                        sizeof(raise_unhandled_effect_code));
#endif
    raise_unhandled_effect_closure = caml_alloc_small (2, Closure_tag);
    Code_val(raise_unhandled_effect_closure) =
      (code_t)raise_unhandled_effect_code;
    Closinfo_val(raise_unhandled_effect_closure) = Make_closinfo(0, 2);
    raise_unhandled_effect = raise_unhandled_effect_closure;
    caml_register_generational_global_root(&raise_unhandled_effect);
    caml_register_generational_global_root(&caml_global_data);
    caml_init_callbacks();
    return Val_unit;
  }

#ifdef HAVE_TAIL_CALL_INTERP
  return caml_bytecode_interpreter_tc(prog, prog_size, initial_env,
                                      initial_extra_args);
#endif

#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  jumptbl_base = Jumptbl_base;
#endif
  initial_trap_sp_off = domain_state->trap_sp_off;
  initial_stack_words =
    Stack_high(domain_state->current_stack) - domain_state->current_stack->sp;
  initial_external_raise = domain_state->external_raise;

  if (sigsetjmp(raise_buf.buf, 0)) {
    /* no non-volatile local variables read here */
    sp = domain_state->current_stack->sp;
    accu = raise_exn_bucket;

    check_trap_barrier_for_exception (domain_state);
    if (domain_state->backtrace_active) {
         /* pc has already been pushed on the stack when calling the C
         function that raised the exception. No need to push it again
         here. */
      caml_stash_backtrace(accu, sp, 0);
    }
    goto raise_notrace;
  }
  domain_state->external_raise = &exception_ctx;

  domain_state->trap_sp_off = 1;

  sp = domain_state->current_stack->sp;
  pc = prog;
  extra_args = initial_extra_args;
  env = initial_env;
  accu = Val_int(0);

#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (caml_icount-- == 0) caml_stop_here ();
  CAMLassert(Stack_base(domain_state->current_stack) <= sp);
  CAMLassert(sp <= Stack_high(domain_state->current_stack));
#endif
  goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
#else
  while(1) {
#ifdef DEBUG
    caml_bcodcount++;
    if (caml_icount-- == 0) caml_stop_here ();
    if (caml_params->trace_level>1)
      printf("\n##%" CAML_PRIdNAT "\n", caml_bcodcount);
    if (caml_params->trace_level>0) caml_disasm_instr(pc);
    if (caml_params->event_trace>0) caml_event_trace(pc);
    if (caml_params->trace_level>1) {
      printf("env=");
      caml_trace_value_file(env,prog,prog_size,stdout);
      putchar('\n');
      caml_trace_accu_sp_file(accu,sp,prog,prog_size,stdout);
      fflush(stdout);
    };
    CAMLassert(Stack_base(domain_state->current_stack) <= sp);
    CAMLassert(sp <= Stack_high(domain_state->current_stack));

#endif
    curr_instr = *pc++;

  dispatch_instr:
    switch(curr_instr) {
#endif

/* Instruction and shared-block bodies (switch / threaded-code flavour). */
#include "interp_instructions.h"

#ifndef THREADED_CODE
    default:
#ifdef _MSC_VER
      CAMLunreachable();
#else
      caml_fatal_error("bad opcode (%" CAML_PRIxNAT ")", (uintnat) *(pc-1));
#endif
    }
  }
#endif
}

/*==========================================================================*/
/* Tail-call interpreter                                                      */
/* Uses the preserve_none calling convention so that all registers are        */
/* caller-saved. Each instruction is a top-level function; dispatch is a      */
/* tail call through tc_dispatch_table indexed by the raw opcode.             */
/*==========================================================================*/

#ifdef HAVE_TAIL_CALL_INTERP

/* Context holding values that are constant for an interpreter invocation.
   Passed as a pointer (7th parameter) to every handler. */
struct tc_ctx {
  struct caml_exception_context *initial_external_raise;
  intnat initial_trap_sp_off;
  int initial_stack_words;
};

/* tc_handler_t and tc_dispatch_table are declared earlier (before
   caml_bytecode_interpreter) so the prog==NULL init branch can access them. */

/* Shorthand for the uniform parameter list and argument list. */
#define TC_PARAMS \
  code_t pc, value * sp, value accu, value env, \
  intnat extra_args, caml_domain_state * domain_state, \
  struct tc_ctx * ctx, const char * tc_base
#define TC_ARGS pc, sp, accu, env, extra_args, domain_state, ctx, tc_base

/* Tail-call flavour of the abstract dispatch layer (see the switch flavour
   near the top of this file, and runtime/interp_instructions.h).  A shared
   block becomes a top-level function and every control transfer becomes a
   guaranteed tail call.  Read and increment pc before the call to avoid an
   unsequenced modification of pc. */

#undef Instruct
#undef Helper
#undef Next
#undef Fallthrough
#undef Fallthrough_to
#undef Restart_curr_instr
#undef Goto_check_stacks
#undef Goto_do_return
#undef Goto_raise_notrace
#undef Goto_raise_exception
#undef Goto_process_signal
#undef Initial_external_raise
#undef Initial_trap_sp_off
#undef Initial_stack_words

#define Instruct(name) \
  CAMLpreserve_none CAMLno_tsan static value tc_handler_##name(TC_PARAMS)
#define Helper(name) \
  CAMLpreserve_none CAMLno_tsan static value tc_##name(TC_PARAMS)
/* Dispatch: bytecode was rewritten by caml_tc_thread_code so *pc holds a
   32-bit signed offset from tc_base to the handler function.  Load the
   offset, add tc_base (in a register), and tail-call the result — one
   memory access instead of the two required by a dispatch-table lookup. */
#define Next \
  do { opcode_t _off = *pc++; \
       CAMLmusttail return \
         ((tc_handler_t)(tc_base + _off))(TC_ARGS); } while(0)
#define Fallthrough_to(name) CAMLmusttail return tc_handler_##name(TC_ARGS)
/* Debugger slow path: saved_instruction returns the offset that was in the
   bytecode slot (already rewritten), so use the same offset dispatch. */
#define Restart_curr_instr \
  do { opcode_t _off = caml_debugger_saved_instruction(pc - 1); \
       CAMLmusttail return \
         ((tc_handler_t)(tc_base + _off))(TC_ARGS); } while(0)
#define Goto_check_stacks()    CAMLmusttail return tc_check_stacks(TC_ARGS)
#define Goto_do_return()       CAMLmusttail return tc_do_return(TC_ARGS)
#define Goto_raise_notrace()   CAMLmusttail return tc_raise_notrace(TC_ARGS)
#define Goto_raise_exception() CAMLmusttail return tc_raise_exception(TC_ARGS)
#define Goto_process_signal()  CAMLmusttail return tc_process_signal(TC_ARGS)
#define Initial_external_raise (ctx->initial_external_raise)
#define Initial_trap_sp_off    (ctx->initial_trap_sp_off)
#define Initial_stack_words    (ctx->initial_stack_words)

/* ------------------------------------------------------------------ */
/* Forward declarations for the shared blocks and all handlers          */
/* ------------------------------------------------------------------ */

static CAMLpreserve_none value tc_do_return(TC_PARAMS);
static CAMLpreserve_none value tc_check_stacks(TC_PARAMS);
static CAMLpreserve_none value tc_raise_notrace(TC_PARAMS);
static CAMLpreserve_none value tc_raise_exception(TC_PARAMS);
static CAMLpreserve_none value tc_process_signal(TC_PARAMS);

#define TC_HANDLER_DECL(name) \
  static CAMLpreserve_none value tc_handler_##name(TC_PARAMS);
CAML_ZINC_OPCODES(TC_HANDLER_DECL)
#undef TC_HANDLER_DECL

/* ================================================================== */
/* Instruction and shared-block bodies (tail-call flavour)              */
/* ================================================================== */

#include "interp_instructions.h"

/* ================================================================== */
/* Dispatch table initialisation                                        */
/* ================================================================== */

static void tc_init_dispatch_table(void)
{
#define REGISTER(name) tc_dispatch_table[name] = tc_handler_##name;
  CAML_ZINC_OPCODES(REGISTER)
#undef REGISTER
}

/* ================================================================== */
/* Entry point for the tail-call interpreter                           */
/* ================================================================== */

CAMLno_tsan
static value caml_bytecode_interpreter_tc(code_t prog, asize_t prog_size,
                                           value initial_env,
                                           intnat initial_extra_args)
{
  /* These locals live on the C stack for the lifetime of the interpreter
     invocation.  We do NOT tail-call out of this frame: the non-tail call
     to tc_dispatch_table[...] below keeps the frame alive so that the
     sigsetjmp buffer in raise_buf remains valid throughout execution. */
  struct longjmp_buffer raise_buf;
  volatile value raise_exn_bucket = Val_unit;
  caml_domain_state * domain_state = Caml_state;
  struct caml_exception_context exception_ctx = {
    &raise_buf, domain_state->local_roots, (value *)&raise_exn_bucket
  };
  struct tc_ctx ctx;
  value * sp;
  code_t pc;
  /* tc_base is the base address used to recover handler pointers from the
     32-bit signed offsets stored in the rewritten bytecode.  Kept const so
     the compiler can hold it in a register and avoid stack spills. */
  const char * const tc_base = (const char *)tc_dispatch_table[0];

  ctx.initial_trap_sp_off   = domain_state->trap_sp_off;
  ctx.initial_stack_words   =
    Stack_high(domain_state->current_stack) - domain_state->current_stack->sp;
  ctx.initial_external_raise = domain_state->external_raise;

  if (sigsetjmp(raise_buf.buf, 0)) {
    /* An OCaml exception was raised through a C function.
       sp and accu are reloaded from domain state; pc/env/extra_args will be
       overwritten by tc_raise_notrace before they are used. */
    sp = domain_state->current_stack->sp;
    value accu = raise_exn_bucket;
    check_trap_barrier_for_exception(domain_state);
    if (domain_state->backtrace_active) {
      /* pc was already pushed on the stack by Setup_for_c_call. */
      caml_stash_backtrace(accu, sp, 0);
    }
    return tc_raise_notrace(NULL, sp, accu, Val_unit, 0, domain_state, &ctx,
                            tc_base);
  }

  domain_state->external_raise = &exception_ctx;
  domain_state->trap_sp_off = 1;

  sp  = domain_state->current_stack->sp;
  pc  = prog;

  /* The call below is intentionally a non-tail call.  The presence of
     sigsetjmp above prevents the compiler from optimising it into a tail call,
     so this frame (and therefore raise_buf) remains alive for the whole
     interpreter run. */
  { opcode_t _off = *pc++;
    return ((tc_handler_t)(tc_base + _off))(pc, sp, Val_int(0), initial_env,
                                            initial_extra_args, domain_state,
                                            &ctx, tc_base); }
}

#endif /* HAVE_TAIL_CALL_INTERP */
