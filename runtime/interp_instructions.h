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

/* Bodies of the bytecode interpreter instructions and of the shared
   control-flow blocks they jump to.

   This file is deliberately NOT protected by an include guard: it is
   included twice by interp.c, once per interpreter flavour.

   - The "switch" flavour (computed-goto threaded code or a plain switch)
     defines the macros below so that this file expands to the body of a
     single big function: every Instruct()/Helper() becomes a label (or a
     case), Next continues the dispatch loop, Fallthrough_to() relies on
     textual fall-through and the Goto_*() macros are plain gotos.

   - The "tail-call" flavour (preserve_none + musttail) defines the macros
     so that this file expands to many top-level functions: every
     Instruct()/Helper() becomes a function definition, Next and the
     Goto_*()/Fallthrough_to() macros become guaranteed tail calls.

   The following macros must be defined by the includer before including
   this file:

     Instruct(name)        declare the handler for opcode `name`
     Helper(name)          declare a shared (non-opcode) control-flow block
     Next                  decode and dispatch the next instruction
     Fallthrough_to(name)  continue into instruction `name`
     Goto_check_stacks()   jump to the `check_stacks` block
     Goto_do_return()      jump to the `do_return` block
     Goto_raise_notrace()  jump to the `raise_notrace` block
     Goto_raise_exception() jump to the `raise_exception` block
     Goto_process_signal() jump to the `process_signal` block
     Do_resume(fn, arg)    resume a continuation (inlined at each use site)
     Initial_external_raise, Initial_trap_sp_off, Initial_stack_words
                           access to the per-invocation initial machine state
     Restart_curr_instr    re-dispatch the current instruction (EVENT/BREAK)

   The abstract machine registers pc, sp, accu, env, extra_args and the
   pointer domain_state are likewise provided by the includer (as local
   variables in the switch flavour, as function parameters in the tail-call
   flavour). */

/* Basic stack operations */

    Instruct(ACC0) {
      accu = sp[0]; Next;
    }
    Instruct(ACC1) {
      accu = sp[1]; Next;
    }
    Instruct(ACC2) {
      accu = sp[2]; Next;
    }
    Instruct(ACC3) {
      accu = sp[3]; Next;
    }
    Instruct(ACC4) {
      accu = sp[4]; Next;
    }
    Instruct(ACC5) {
      accu = sp[5]; Next;
    }
    Instruct(ACC6) {
      accu = sp[6]; Next;
    }
    Instruct(ACC7) {
      accu = sp[7]; Next;
    }

    Instruct(PUSH) {
      *--sp = accu; Next;
    }
    Instruct(PUSHACC0) {
      *--sp = accu; Next;
    }
    Instruct(PUSHACC1) {
      *--sp = accu; accu = sp[1]; Next;
    }
    Instruct(PUSHACC2) {
      *--sp = accu; accu = sp[2]; Next;
    }
    Instruct(PUSHACC3) {
      *--sp = accu; accu = sp[3]; Next;
    }
    Instruct(PUSHACC4) {
      *--sp = accu; accu = sp[4]; Next;
    }
    Instruct(PUSHACC5) {
      *--sp = accu; accu = sp[5]; Next;
    }
    Instruct(PUSHACC6) {
      *--sp = accu; accu = sp[6]; Next;
    }
    Instruct(PUSHACC7) {
      *--sp = accu; accu = sp[7]; Next;
    }

    Instruct(PUSHACC) {
      *--sp = accu;
      Fallthrough_to(ACC);
    }
    Instruct(ACC) {
      accu = sp[*pc++];
      Next;
    }

    Instruct(POP) {
      sp += *pc++;
      Next;
    }
    Instruct(ASSIGN) {
      sp[*pc++] = accu;
      accu = Val_unit;
      Next;
    }

/* Access in heap-allocated environment */

    Instruct(ENVACC1) {
      accu = Field(env, 1); Next;
    }
    Instruct(ENVACC2) {
      accu = Field(env, 2); Next;
    }
    Instruct(ENVACC3) {
      accu = Field(env, 3); Next;
    }
    Instruct(ENVACC4) {
      accu = Field(env, 4); Next;
    }

    Instruct(PUSHENVACC1) {
      *--sp = accu; accu = Field(env, 1); Next;
    }
    Instruct(PUSHENVACC2) {
      *--sp = accu; accu = Field(env, 2); Next;
    }
    Instruct(PUSHENVACC3) {
      *--sp = accu; accu = Field(env, 3); Next;
    }
    Instruct(PUSHENVACC4) {
      *--sp = accu; accu = Field(env, 4); Next;
    }

    Instruct(PUSHENVACC) {
      *--sp = accu;
      Fallthrough_to(ENVACC);
    }
    Instruct(ENVACC) {
      accu = Field(env, *pc++);
      Next;
    }

/* Function application */

    Instruct(PUSH_RETADDR) {
      sp -= 3;
      sp[0] = (value) (pc + *pc);
      sp[1] = env;
      sp[2] = Val_long(extra_args);
      pc++;
      Next;
    }
    Instruct(APPLY) {
      extra_args = *pc - 1;
      pc = Code_val(accu);
      env = accu;
      Goto_check_stacks();
    }
    Instruct(APPLY1) {
      value arg1 = sp[0];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = (value)pc;
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      Goto_check_stacks();
    }
    Instruct(APPLY2) {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = (value)pc;
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 1;
      Goto_check_stacks();
    }
    Instruct(APPLY3) {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      sp[3] = (value)pc;
      sp[4] = env;
      sp[5] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 2;
      Goto_check_stacks();
    }

    Instruct(APPTERM) {
      int nargs = *pc++;
      int slotsize = *pc;
      value * newsp;
      /* Slide the nargs bottom words of the current frame to the top
         of the frame, and discard the remainder of the frame */
      newsp = sp + slotsize - nargs;
      for (int i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
      sp = newsp;
      pc = Code_val(accu);
      env = accu;
      extra_args += nargs - 1;
      Goto_check_stacks();
    }
    Instruct(APPTERM1) {
      value arg1 = sp[0];
      sp = sp + *pc - 1;
      sp[0] = arg1;
      pc = Code_val(accu);
      env = accu;
      Goto_check_stacks();
    }
    Instruct(APPTERM2) {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp = sp + *pc - 2;
      sp[0] = arg1;
      sp[1] = arg2;
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      Goto_check_stacks();
    }
    Instruct(APPTERM3) {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp = sp + *pc - 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      Goto_check_stacks();
    }

    Instruct(RETURN) {
      sp += *pc++;
      if (extra_args > 0) {
        extra_args--;
        pc = Code_val(accu);
        env = accu;
        Next;
      } else {
        Goto_do_return();
      }
    }

    Helper(do_return) {
      if (sp == Stack_high(domain_state->current_stack)) {
        /* return to parent stack */
        struct stack_info* old_stack = domain_state->current_stack;
        struct stack_info* parent_stack = Stack_parent(old_stack);
        value hval = Stack_handle_value(old_stack);
        CAMLassert(parent_stack != NULL);

        domain_state->current_stack = parent_stack;
        sp = domain_state->current_stack->sp;
        caml_free_stack(old_stack);

        domain_state->trap_sp_off = Long_val(sp[0]);
        extra_args = Long_val(sp[1]);
        sp++;
        sp[0] = accu;

        accu = hval;
        pc = Code_val(accu);
        env = accu;
        Goto_check_stacks();
      } else {
        /* return to callee, no stack switching */
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }

    Instruct(RESTART) {
      int num_args = Wosize_val(env) - 3;
      sp -= num_args;
      for (int i = 0; i < num_args; i++) sp[i] = Field(env, i + 3);
      env = Field(env, 2);
      extra_args += num_args;
      Next;
    }

    Instruct(GRAB) {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
        Next;
      } else {
        mlsize_t num_args;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 3, Closure_tag, Enter_gc);
        Field(accu, 2) = env;
        for (mlsize_t i = 0; i < num_args; i++) Field(accu, i + 3) = sp[i];
        Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
        Closinfo_val(accu) = Make_closinfo(0, 2);
        sp += num_args;
        Goto_do_return();
      }
    }
    Instruct(CLOSURE) {
      int nvars = *pc++;
      if (nvars > 0) *--sp = accu;
      if (nvars <= Max_young_wosize - 2) {
        /* nvars + 2 <= Max_young_wosize, can allocate in minor heap */
        Alloc_small(accu, 2 + nvars, Closure_tag, Enter_gc);
        for (int i = 0; i < nvars; i++) Field(accu, i + 2) = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(2 + nvars, Closure_tag);
        for (int i = 0; i < nvars; i++)
          caml_initialize(&Field(accu, i + 2), sp[i]);
      }
      /* The code pointer is not in the heap, so no need to go through
         caml_initialize. */
      Code_val(accu) = pc + *pc;
      Closinfo_val(accu) = Make_closinfo(0, 2);
      pc++;
      sp += nvars;
      Next;
    }

    Instruct(CLOSUREREC) {
      int nfuncs = *pc++;
      int nvars = *pc++;
      mlsize_t envofs = nfuncs * 3 - 1;
      mlsize_t blksize = envofs + nvars;
      volatile value * p;
      if (nvars > 0) *--sp = accu;
      if (blksize <= Max_young_wosize) {
        Alloc_small(accu, blksize, Closure_tag, Enter_gc);
        p = &Field(accu, envofs);
        for (int i = 0; i < nvars; i++, p++) *p = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(blksize, Closure_tag);
        p = &Field(accu, envofs);
        for (int i = 0; i < nvars; i++, p++) caml_initialize(p, sp[i]);
      }
      sp += nvars;
      /* The code pointers and infix headers are not in the heap,
         so no need to go through caml_initialize. */
      *--sp = accu;
      p = &Field(accu, 0);
      *p++ = (value) (pc + pc[0]);
      *p++ = Make_closinfo(0, envofs);
      for (int i = 1; i < nfuncs; i++) {
        *p++ = Make_header(i * 3, Infix_tag, 0); /* color irrelevant */
        *--sp = (value) p;
        *p++ = (value) (pc + pc[i]);
        envofs -= 3;
        *p++ = Make_closinfo(0, envofs);
      }
      pc += nfuncs;
      Next;
    }
    Instruct(PUSHOFFSETCLOSURE) {
      *--sp = accu; Fallthrough_to(OFFSETCLOSURE);
    }
    Instruct(OFFSETCLOSURE) {
      accu = env + *pc++ * sizeof(value); Next;
    }

    Instruct(PUSHOFFSETCLOSUREM3) {
      *--sp = accu; Fallthrough_to(OFFSETCLOSUREM3);
    }
    Instruct(OFFSETCLOSUREM3) {
      accu = env - 3 * sizeof(value); Next;
    }
    Instruct(PUSHOFFSETCLOSURE0) {
      *--sp = accu; Fallthrough_to(OFFSETCLOSURE0);
    }
    Instruct(OFFSETCLOSURE0) {
      accu = env; Next;
    }
    Instruct(PUSHOFFSETCLOSURE3) {
      *--sp = accu; Fallthrough_to(OFFSETCLOSURE3);
    }
    Instruct(OFFSETCLOSURE3) {
      accu = env + 3 * sizeof(value); Next;
    }


/* Access to global variables */

    Instruct(PUSHGETGLOBAL) {
      *--sp = accu;
      Fallthrough_to(GETGLOBAL);
    }
    Instruct(GETGLOBAL) {
      accu = Field(caml_global_data, *pc);
      pc++;
      Next;
    }

    Instruct(PUSHGETGLOBALFIELD) {
      *--sp = accu;
      Fallthrough_to(GETGLOBALFIELD);
    }
    Instruct(GETGLOBALFIELD) {
      accu = Field(caml_global_data, *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL) {
      caml_modify(&Field(caml_global_data, *pc), accu);
      accu = Val_unit;
      pc++;
      Next;
    }

/* Allocation of blocks */

    Instruct(PUSHATOM0) {
      *--sp = accu;
      Fallthrough_to(ATOM0);
    }
    Instruct(ATOM0) {
      accu = Atom(0); Next;
    }

    Instruct(PUSHATOM) {
      *--sp = accu;
      Fallthrough_to(ATOM);
    }
    Instruct(ATOM) {
      accu = Atom(*pc++); Next;
    }

    Instruct(MAKEBLOCK) {
      mlsize_t wosize = *pc++;
      tag_t tag = *pc++;
      value block;
      if (wosize <= Max_young_wosize) {
        Alloc_small(block, wosize, tag, Enter_gc);
        Field(block, 0) = accu;
        for (mlsize_t i = 1; i < wosize; i++) Field(block, i) = *sp++;
      } else {
        block = caml_alloc_shr(wosize, tag);
        caml_initialize(&Field(block, 0), accu);
        for (mlsize_t i = 1; i < wosize; i++)
          caml_initialize(&Field(block, i), *sp++);
      }
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1) {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag, Enter_gc);
      Field(block, 0) = accu;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2) {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag, Enter_gc);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3) {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag, Enter_gc);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      Field(block, 2) = sp[1];
      sp += 2;
      accu = block;
      Next;
    }
    Instruct(MAKEFLOATBLOCK) {
      mlsize_t size = *pc++;
      value block;
      if (size <= Max_young_wosize / Double_wosize) {
        Alloc_small(block, size * Double_wosize, Double_array_tag, Enter_gc);
      } else {
        block = caml_alloc_shr(size * Double_wosize, Double_array_tag);
      }
      Store_double_flat_field(block, 0, Double_val(accu));
      for (mlsize_t i = 1; i < size; i++){
        Store_double_flat_field(block, i, Double_val(*sp));
        ++ sp;
      }
      accu = block;
      Next;
    }

/* Access to components of blocks */

    Instruct(GETFIELD0) {
      accu = Field(accu, 0); Next;
    }
    Instruct(GETFIELD1) {
      accu = Field(accu, 1); Next;
    }
    Instruct(GETFIELD2) {
      accu = Field(accu, 2); Next;
    }
    Instruct(GETFIELD3) {
      accu = Field(accu, 3); Next;
    }
    Instruct(GETFIELD) {
      accu = Field(accu, *pc); pc++; Next;
    }
    Instruct(GETFLOATFIELD) {
      double d = Double_flat_field(accu, *pc++);
      Alloc_small(accu, Double_wosize, Double_tag, Enter_gc);
      Store_double_val(accu, d);
      Next;
    }
    Instruct(SETFIELD0) {
      caml_modify(&Field(accu, 0), *sp++);
      accu = Val_unit;
      Next;
    }
    Instruct(SETFIELD1) {
      caml_modify(&Field(accu, 1), *sp++);
      accu = Val_unit;
      Next;
    }
    Instruct(SETFIELD2) {
      caml_modify(&Field(accu, 2), *sp++);
      accu = Val_unit;
      Next;
    }
    Instruct(SETFIELD3) {
      caml_modify(&Field(accu, 3), *sp++);
      accu = Val_unit;
      Next;
    }
    Instruct(SETFIELD) {
      caml_modify(&Field(accu, *pc), *sp++);
      accu = Val_unit;
      pc++;
      Next;
    }
    Instruct(SETFLOATFIELD) {
      Store_double_flat_field(accu, *pc, Double_val(*sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;
    }

/* Array operations */

    Instruct(VECTLENGTH) {
      /* Todo: when FLAT_FLOAT_ARRAY is false, this instruction should
         be split into VECTLENGTH and FLOATVECTLENGTH because we know
         statically which one it is. */
      mlsize_t size = Wosize_val(accu);
      if (Tag_val(accu) == Double_array_tag) size = size / Double_wosize;
      accu = Val_long(size);
      Next;
    }
    Instruct(GETVECTITEM) {
      accu = Field(accu, Long_val(sp[0]));
      sp += 1;
      Next;
    }
    Instruct(SETVECTITEM) {
      caml_modify(&Field(accu, Long_val(sp[0])), sp[1]);
      accu = Val_unit;
      sp += 2;
      Next;
    }

/* Bytes/String operations */
    Instruct(GETSTRINGCHAR) {
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    }
    Instruct(GETBYTESCHAR) {
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    }
    Instruct(SETBYTESCHAR) {
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
      accu = Val_unit;
      Next;
    }

/* Branches and conditional branches */

    Instruct(BRANCH) {
      pc += *pc;
      Next;
    }
    Instruct(BRANCHIF) {
      if (accu != Val_false) pc += *pc; else pc++;
      Next;
    }
    Instruct(BRANCHIFNOT) {
      if (accu == Val_false) pc += *pc; else pc++;
      Next;
    }
    Instruct(SWITCH) {
      uint32_t sizes = *pc++;
      if (Is_block(accu)) {
        intnat index = Tag_val(accu);
        CAMLassert ((uintnat) index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        intnat index = Long_val(accu);
        CAMLassert ((uintnat) index < (sizes & 0xFFFF)) ;
        pc += pc[index];
      }
      Next;
    }
    Instruct(BOOLNOT) {
      accu = Val_not(accu);
      Next;
    }

/* Exceptions */

    Instruct(PUSHTRAP) {
      sp -= 4;
      Trap_pc(sp) = pc + *pc;
      Trap_link(sp) = Val_long(domain_state->trap_sp_off);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      domain_state->trap_sp_off = sp - Stack_high(domain_state->current_stack);
      pc++;
      Next;
    }

    Instruct(POPTRAP) {
      if (Caml_check_gc_interrupt(domain_state)) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        Goto_process_signal();
      }
      domain_state->trap_sp_off = Long_val(Trap_link(sp));
      sp += 4;
      Next;
    }

    Instruct(RAISE_NOTRACE) {
      check_trap_barrier_for_exception (domain_state);
      Goto_raise_notrace();
    }

    Instruct(RERAISE) {
      check_trap_barrier_for_exception (domain_state);
      if (domain_state->backtrace_active) {
        *--sp = (value)(pc - 1);
        caml_stash_backtrace(accu, sp, 1);
      }
      Goto_raise_notrace();
    }

    Instruct(RAISE) {
      Goto_raise_exception();
    }

    Helper(raise_exception) {
      check_trap_barrier_for_exception (domain_state);
      if (domain_state->backtrace_active) {
        *--sp = (value)(pc - 1);
        caml_stash_backtrace(accu, sp, 0);
      }
      Goto_raise_notrace();
    }

    Helper(raise_notrace) {
      if (domain_state->trap_sp_off > 0) {
        if (Stack_parent(domain_state->current_stack) == NULL) {
          domain_state->external_raise = Initial_external_raise;
          domain_state->trap_sp_off = Initial_trap_sp_off;
          domain_state->current_stack->sp =
            Stack_high(domain_state->current_stack) - Initial_stack_words ;
          return Make_exception_result(accu);
        } else {
          struct stack_info* old_stack = domain_state->current_stack;
          struct stack_info* parent_stack = Stack_parent(old_stack);
          value hexn = Stack_handle_exception(old_stack);
          old_stack->sp = sp;
          domain_state->current_stack = parent_stack;
          sp = domain_state->current_stack->sp;
          caml_free_stack(old_stack);

          domain_state->trap_sp_off = Long_val(sp[0]);
          extra_args = Long_val(sp[1]);
          sp++;
          sp[0] = accu;

          accu = hexn;
          pc = Code_val(accu);
          env = accu;
          Goto_check_stacks();
        }
      } else {
        sp =
           Stack_high(domain_state->current_stack) + domain_state->trap_sp_off;
        pc = Trap_pc(sp);
        domain_state->trap_sp_off = Long_val(Trap_link(sp));
        env = sp[2];
        extra_args = Long_val(sp[3]);
        sp += 4;
      }
      Next;
    }

/* Stack reallocation */

    Helper(check_stacks) {
      if (sp < Stack_threshold_ptr(domain_state->current_stack)) {
        domain_state->current_stack->sp = sp;
        if (!caml_try_realloc_stack(Stack_threshold_words)) {
          Setup_for_c_call; caml_raise_stack_overflow();
        }
        sp = domain_state->current_stack->sp;
      }
      /* Same as CHECK_SIGNALS below */
      if (Caml_check_gc_interrupt(domain_state)) Goto_process_signal();
      Next;
    }

/* Signal handling */

    Instruct(CHECK_SIGNALS) {     /* accu not preserved */
      if (Caml_check_gc_interrupt(domain_state)) Goto_process_signal();
      Next;
    }

    Helper(process_signal) {
      Setup_for_event;
      caml_process_pending_actions();
      Restore_after_event;
      Next;
    }

/* Calling C functions */

    Instruct(C_CALL1) {
      Setup_for_c_call;
      accu = Primitive1(*pc)(accu);
      Restore_after_c_call;
      pc++;
      Next;
    }
    Instruct(C_CALL2) {
      Setup_for_c_call;
      accu = Primitive2(*pc)(accu, sp[2]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    }
    Instruct(C_CALL3) {
      Setup_for_c_call;
      accu = Primitive3(*pc)(accu, sp[2], sp[3]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    }
    Instruct(C_CALL4) {
      Setup_for_c_call;
      accu = Primitive4(*pc)(accu, sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    }
    Instruct(C_CALL5) {
      Setup_for_c_call;
      accu = Primitive5(*pc)(accu, sp[2], sp[3], sp[4], sp[5]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    }
    Instruct(C_CALLN) {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = PrimitiveN(*pc)(sp + 2, nargs);
      Restore_after_c_call;
      sp += nargs;
      pc++;
      Next;
    }

/* Integer constants */

    Instruct(CONST0) {
      accu = Val_int(0); Next;
    }
    Instruct(CONST1) {
      accu = Val_int(1); Next;
    }
    Instruct(CONST2) {
      accu = Val_int(2); Next;
    }
    Instruct(CONST3) {
      accu = Val_int(3); Next;
    }

    Instruct(PUSHCONST0) {
      *--sp = accu; accu = Val_int(0); Next;
    }
    Instruct(PUSHCONST1) {
      *--sp = accu; accu = Val_int(1); Next;
    }
    Instruct(PUSHCONST2) {
      *--sp = accu; accu = Val_int(2); Next;
    }
    Instruct(PUSHCONST3) {
      *--sp = accu; accu = Val_int(3); Next;
    }

    Instruct(PUSHCONSTINT) {
      *--sp = accu;
      Fallthrough_to(CONSTINT);
    }
    Instruct(CONSTINT) {
      accu = Val_int(*pc);
      pc++;
      Next;
    }

/* Integer arithmetic */

    Instruct(NEGINT) {
      accu = (value)(2 - (intnat)accu); Next;
    }
    Instruct(ADDINT) {
      accu = (value)((intnat) accu + (intnat) *sp++ - 1); Next;
    }
    Instruct(SUBINT) {
      accu = (value)((intnat) accu - (intnat) *sp++ + 1); Next;
    }
    Instruct(MULINT) {
      accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;
    }

    Instruct(DIVINT) {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) / divisor);
      Next;
    }
    Instruct(MODINT) {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) % divisor);
      Next;
    }
    Instruct(ANDINT) {
      accu = (value)((intnat) accu & (intnat) *sp++); Next;
    }
    Instruct(ORINT) {
      accu = (value)((intnat) accu | (intnat) *sp++); Next;
    }
    Instruct(XORINT) {
      accu = (value)(((intnat) accu ^ (intnat) *sp++) | 1); Next;
    }
    Instruct(LSLINT) {
      accu = (value)((((intnat) accu - 1) << Long_val(*sp++)) + 1); Next;
    }
    Instruct(LSRINT) {
      accu = (value)((((uintnat) accu) >> Long_val(*sp++)) | 1); Next;
    }
    Instruct(ASRINT) {
      accu = (value)((((intnat) accu) >> Long_val(*sp++)) | 1); Next;
    }

    Integer_comparison(intnat,EQ, ==)
    Integer_comparison(intnat,NEQ, !=)
    Integer_comparison(intnat,LTINT, <)
    Integer_comparison(intnat,LEINT, <=)
    Integer_comparison(intnat,GTINT, >)
    Integer_comparison(intnat,GEINT, >=)
    Integer_comparison(uintnat,ULTINT, <)
    Integer_comparison(uintnat,UGEINT, >=)

    Integer_branch_comparison(intnat,BEQ, ==)
    Integer_branch_comparison(intnat,BNEQ, !=)
    Integer_branch_comparison(intnat,BLTINT, <)
    Integer_branch_comparison(intnat,BLEINT, <=)
    Integer_branch_comparison(intnat,BGTINT, >)
    Integer_branch_comparison(intnat,BGEINT, >=)
    Integer_branch_comparison(uintnat,BULTINT, <)
    Integer_branch_comparison(uintnat,BUGEINT, >=)

    Instruct(OFFSETINT) {
      accu += *pc << 1;
      pc++;
      Next;
    }
    Instruct(OFFSETREF) {
      Field(accu, 0) += *pc << 1;
      accu = Val_unit;
      pc++;
      Next;
    }
    Instruct(ISINT) {
      accu = Val_long(accu & 1);
      Next;
    }

/* Object-oriented operations */

    Instruct(GETMETHOD) {
      accu = Lookup(sp[0], accu);
      Next;
    }

#ifdef CAML_METHOD_CACHE
    Instruct(GETPUBMET) {
      /* accu == object, pc[0] == tag, pc[1] == cache */
      value meths = Field (accu, 0);
      value ofs;
#ifdef CAML_TEST_CACHE
      static int calls = 0, hits = 0;
      if (calls >= 10000000) {
        fprintf(stderr, "cache hit = %d%%\n", hits / 100000);
        calls = 0; hits = 0;
      }
      calls++;
#endif
      *--sp = accu;
      accu = Val_int(*pc++);
      /* We use relaxed atomic accesses to avoid racing with other domains
         updating the cache */
      ofs = atomic_load_relaxed((_Atomic opcode_t *)pc) & Field(meths,1);
      if (*(value*)(((char*)&Field(meths,3)) + ofs) == accu) {
#ifdef CAML_TEST_CACHE
        hits++;
#endif
        accu = *(value*)(((char*)&Field(meths,2)) + ofs);
      }
      else
      {
        int li = 3, hi = Field(meths,0), mi;
        while (li < hi) {
          mi = ((li+hi) >> 1) | 1;
          if (accu < Field(meths,mi)) hi = mi-2;
          else li = mi;
        }
        atomic_store_relaxed((_Atomic opcode_t *)pc, (li-3)*sizeof(value));
        accu = Field (meths, li-1);
      }
      pc++;
      Next;
    }
#else
    Instruct(GETPUBMET) {
      *--sp = accu;
      accu = Val_int(*pc);
      pc += 2;
      Fallthrough_to(GETDYNMET);
    }
#endif
    Instruct(GETDYNMET) {
      /* accu == tag, sp[0] == object, *pc == cache */
      value meths = Field (sp[0], 0);
      int li = 3, hi = Field(meths,0), mi;
      while (li < hi) {
        mi = ((li+hi) >> 1) | 1;
        if (accu < Field(meths,mi)) hi = mi-2;
        else li = mi;
      }
      accu = Field (meths, li-1);
      Next;
    }

/* Debugging and machine control */

    Instruct(STOP) {
      domain_state->external_raise = Initial_external_raise;
      domain_state->trap_sp_off = Initial_trap_sp_off;
      domain_state->current_stack->sp = sp;
      return accu;
    }

    Instruct(EVENT) {
      if (--caml_event_count == 0) {
        Setup_for_debugger;
        caml_debugger(EVENT_COUNT, Val_unit);
        Restore_after_debugger;
      }
      Restart_curr_instr;
    }

    Instruct(BREAK) {
      Setup_for_debugger;
      caml_debugger(BREAKPOINT, Val_unit);
      Restore_after_debugger;
      Restart_curr_instr;
    }

/* Context switching */

    Instruct(RESUME) {
      value resume_fn = sp[0];
      value resume_arg = sp[1];
      sp -= 3;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(0);
      sp[2] = (value)pc;
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      Do_resume(resume_fn, resume_arg);
    }

    Instruct(RESUMETERM) {
      value resume_fn = sp[0];
      value resume_arg = sp[1];
      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);
      Do_resume(resume_fn, resume_arg);
    }

    Instruct(PERFORM) {
      value cont;
      struct stack_info* old_stack = domain_state->current_stack;
      struct stack_info* parent_stack = Stack_parent(old_stack);

      check_trap_barrier_for_effect (domain_state);
      if (parent_stack == NULL) {
        Setup_for_c_call;
        accu = caml_make_unhandled_effect_exn(accu);
        Restore_after_c_call;
        Goto_raise_exception();
      }

      Alloc_small(cont, 1, Cont_tag, Enter_gc);

      sp -= 4;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = (value)pc;
      sp[2] = env;
      sp[3] = Val_long(extra_args);

      old_stack->sp = sp;
      domain_state->current_stack = parent_stack;
      sp = parent_stack->sp;
      Stack_parent(old_stack) = old_stack;
      Field(cont, 0) = Val_ptr(old_stack);

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp[0] = accu;
      sp[1] = cont;
      accu = Stack_handle_effect(old_stack);
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      Goto_check_stacks();
    }

    Instruct(REPERFORMTERM) {
      value eff = accu;
      value cont = sp[0];
      struct stack_info* cont_tail = Ptr_val(Field(cont, 0));
      struct stack_info* cont_head = Stack_parent(cont_tail);
      struct stack_info* self = domain_state->current_stack;
      struct stack_info* parent = Stack_parent(domain_state->current_stack);

      check_trap_barrier_for_effect (domain_state);
      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);

      if (parent == NULL) {
        /* Save cont across the allocation in caml_make_unhandled_effect_exn */
        sp -= 1;
        sp[0] = cont;
        Setup_for_c_call;
        value resume_arg = caml_make_unhandled_effect_exn(eff);
        Restore_after_c_call;
        cont = sp[0];
        sp += 1;
        Setup_for_c_call;
        accu = caml_continuation_use(cont);
        Restore_after_c_call;

        Do_resume(raise_unhandled_effect, resume_arg);
      }

      self->sp = sp;
      domain_state->current_stack = parent;
      sp = parent->sp;

      Stack_parent(self) = cont_head;
      Stack_parent(cont_tail) = self;
      Field(cont, 0) = Val_ptr(self);

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp[0] = eff;
      sp[1] = cont;
      accu = Stack_handle_effect(self);
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      Goto_check_stacks();
    }
