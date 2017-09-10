/*----------------------------------------------------------------------------*/
/*--- begin                                      host_generic_reg_alloc3.c ---*/
/*----------------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation framework.

   Copyright (C) 2017-2017 Ivo Raisr
      ivosh@ivosh.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main_util.h"
#include "host_generic_regs.h"

/* Set to 1 for lots of debugging output. */
#define DEBUG_REGALLOC 0

/* Set to 1 for sanity checking at every instruction.
   Set to 0 for sanity checking only every 17th one and the last one. */
#define SANITY_CHECKS_EVERY_INSTR 0


#define INVALID_INSTRNO (-2)
#define INSTRNO_TOTAL toShort(ii_chunk + chunk->ii_total_start)

#define MAX(x, y) ((x) > (y) ? (x) : (y))

/* Instruction numbering.
   One instruction has three numbers, depending on the context.
   Chunk = local within the RegAllocChunk.
   Vec   = index in the HInstrVec.
   Total = total ordering.

   Consider the following HInstrSB structure:
      insn1
      if (!cond) then fall-through {
         insn2
         insn3
      } else out-of-line {
         insn4
      }
      insn5
      insn6

   The resulting numbering is as follows:
               chunk    vec  total
       insn1       0      0      0
       insn2       0      0      1
       insn3       1      1      2
       insn4       0      0      1
       insn5       0      1      3
       insn6       1      2      4
*/

/* Register allocator state is kept in an array of VRegState's.
   There is an element for every virtual register (vreg).
   Elements are indexed [0 .. n_vregs-1].
   Records information about vreg live range (in total ordering) and its state.
 */
typedef
   struct {
      /* Live range, register class and spill offset are computed during the
         first register allocator pass and remain unchanged after that. */

      /* This vreg becomes live with this instruction (inclusive). Contains
         either an instruction number in total ordering or INVALID_INSTRNO. */
      Short live_after;
      /* This vreg becomes dead before this instruction (exclusive). Contains
         either an instruction number in total ordering or INVALID_INSTRNO. */
      Short dead_before;
      /* What kind of register this is. */
      HRegClass reg_class;

      /* What is its current disposition? */
      enum { Unallocated, /* Neither spilled nor assigned to a real reg. */
             Assigned,    /* Assigned to a real register, viz rreg. */
             Spilled      /* Spilled to the spill slot. */
           } disp;

      /* If .disp == Assigned, what rreg is it bound to? */
      HReg rreg;

      /* The "home" spill slot. The offset is relative to the beginning of
         the guest state. */
      UShort spill_offset;
   }
   VRegState;

/* Records information on a real-register live range, associated with
   a particular real register. Instruction numbers use chunk (local) numbering.
   Computed once; does not change. */
typedef
   struct {
      /* This rreg becomes live with this instruction (inclusive). Contains
         either an instruction number in chunk numbering or INVALID_INSTRNO. */
      Short live_after;
      /* This rreg becomes dead before this instruction (exclusive). Contains
         either an instruction number in chunk numbering or INVALID_INSTRNO. */
      Short dead_before;
   }
   RRegLR;

/* The allocator also maintains a redundant array of indexes (rreg_state) from
   rreg numbers back to entries in vreg_state. It is redundant because iff
   rreg_state[r] == v then hregNumber(vreg_state[v].rreg) == r -- that is, the
   two entries point at each other. The purpose of this is to speed up
   activities which involve looking for a particular rreg: there is no need to
   scan the vreg_state looking for it, just index directly into rreg_state.
   The FAQ "does this rreg already have an associated vreg" is the main
   beneficiary.
   The identity of the real register is not recorded here, because the index
   of this structure in |rreg_state| is the index number of the register, and
   the register itself can be extracted from the RRegUniverse (univ). */
typedef
   struct {
      /* What is its current disposition? */
      enum { Free,     /* Not bound to any vreg. */
             Bound,    /* Bound to a vreg, viz vreg. */
             Reserved  /* Reserved for an instruction. */
           } disp;

      /* If .disp == Bound, what vreg is it bound to? */
      HReg vreg;

      /* If .disp == Bound, has the associated vreg been reloaded from its spill
         slot recently and is this rreg still equal to that spill slot?
         Avoids unnecessary spilling that vreg later, when this rreg needs
         to be reserved. */
      Bool eq_spill_slot;
   }
   RRegState;

/* Live ranges for a single rreg and the current one.
   Live ranges are computed during the first register allocator pass and remain
   unchanged after that.
   The identity of the real register is not recorded here, because the index
   of this structure in RegAllocChunk->rreg_lr_state is the index number of the
   register, and the register itself can be extracted from the
   RRegUniverse (univ). */
typedef
   struct {
      RRegLR* lrs;
      UInt    lrs_size;
      UInt    lrs_used;

      /* Live range corresponding to the currently processed instruction.
         Points into |lrs| array. */
      RRegLR  *lr_current;
      UInt     lr_current_idx;
   }
   RRegLRState;

#define IS_VALID_VREGNO(v) ((v) >= 0 && (v) < n_vregs)
#define IS_VALID_RREGNO(r) ((r) >= 0 && (r) < n_rregs)

/* Represents register allocator state corresponding to one contiguous chunk
   of instructions. The chunk either continues with If-Then-Else legs or
   simply ends. */
typedef
   struct RegAllocChunk_ {
      /* Live ranges of real registers. Computed during the first register
         allocator pass and remain unchanged after that. Inherently local
         to every chunk. */
      RRegLRState* rreg_lr_state;
      UInt         n_rregs;

      /* Incoming contiguous chunk of instructions starting at |ii_vec_start|
         of size |ii_vec_len|. No HInstrIfThenElse is present here. */
      HInstrVec* instrs_in;
      Short      ii_vec_start;
      UShort     ii_vec_len; /* This is also ii_chunk_len. */
      Short      ii_total_start; /* Start index for insns in total ordering. */
      /* Register usage for the current instr chunk of size |ii_vec_len|. */
      HRegUsage* reg_usage;
      HInstrVec* instrs_out;

      /* Are If-Then-Legs present? */
      Bool isIfThenElse;
      struct {
         HCondCode              ccOOL; /* Condition code for the OOL branch. */
         struct RegAllocChunk_* fallThrough;
         struct RegAllocChunk_* outOfLine;
         HPhiNode*              phi_nodes;
         UInt                   n_phis;
      } IfThenElse;
      struct RegAllocChunk_* next; /* Next chunk, if any. */

      /* Possible combinations (x = allowed, - = not allowed):
                               If-Then-Else legs: | present | not present
         ----------------------------------------------------------------
         next chunk: not NULL                     |    x    |      -
                         NULL                     |    x    |      x
       */
   }
   RegAllocChunk;

static void init_rreg_lr_state(RRegLRState* rreg_lrs)
{
   rreg_lrs->lrs_size    = 4;
   rreg_lrs->lrs         = LibVEX_Alloc_inline(rreg_lrs->lrs_size
                                               * sizeof(RRegLR));
   rreg_lrs->lrs_used       = 0;
   rreg_lrs->lr_current     = &rreg_lrs->lrs[0];
   rreg_lrs->lr_current_idx = 0;
}

static RegAllocChunk* new_chunk(HInstrVec* instrs_in, UInt n_rregs)
{
   RegAllocChunk* chunk  = LibVEX_Alloc_inline(sizeof(RegAllocChunk));
   chunk->n_rregs        = n_rregs;
   chunk->rreg_lr_state  = LibVEX_Alloc_inline(chunk->n_rregs
                                               * sizeof(RRegLRState));
   for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
      init_rreg_lr_state(&chunk->rreg_lr_state[r_idx]);
   }
   chunk->instrs_in      = instrs_in;
   chunk->ii_vec_start   = INVALID_INSTRNO;
   chunk->ii_vec_len     = 0;
   chunk->ii_total_start = INVALID_INSTRNO;
   chunk->reg_usage      = NULL;
   chunk->instrs_out     = NULL;
   chunk->isIfThenElse   = False;
   chunk->next           = NULL;

   return chunk;
}

static void print_depth(UInt depth)
{
   for (UInt i = 0; i < depth; i++) {
      vex_printf("    ");
   }
}

#define WALK_CHUNKS(process_one_chunk, process_legs_fork,               \
                    process_fall_through_leg,                           \
                    process_out_of_line_leg, process_phi_nodes)         \
   do {                                                                 \
      while (chunk != NULL) {                                           \
         process_one_chunk;                                             \
         if (chunk->isIfThenElse) {                                     \
            process_legs_fork;                                          \
            if (DEBUG_REGALLOC) {                                       \
               print_depth(depth);                                      \
               vex_printf("if (!");                                     \
               con->ppCondCode(chunk->IfThenElse.ccOOL);                \
               vex_printf(") then fall-through {\n");                   \
            }                                                           \
            process_fall_through_leg;                                   \
            if (DEBUG_REGALLOC) {                                       \
               print_depth(depth);                                      \
               vex_printf("} else out-of-line {\n");                    \
            }                                                           \
            process_out_of_line_leg;                                    \
            if (DEBUG_REGALLOC) {                                       \
               print_depth(depth);                                      \
               vex_printf("}\n");                                       \
            }                                                           \
            if (chunk->IfThenElse.n_phis > 0) {                         \
               process_phi_nodes;                                       \
               if (DEBUG_REGALLOC) {                                    \
                  for (UInt p = 0; p < chunk->IfThenElse.n_phis; p++) { \
                     print_depth(depth);                                \
                     ppHPhiNode(&chunk->IfThenElse.phi_nodes[p]);       \
                     vex_printf("\n");                                  \
                  }                                                     \
               }                                                        \
            }                                                           \
         }                                                              \
         chunk = chunk->next;                                           \
      }                                                                 \
   } while (0)


/* Compute the index of the highest and lowest 1 in a ULong, respectively.
   Results are undefined if the argument is zero. Don't pass it zero :) */
static inline UInt ULong__maxIndex ( ULong w64 ) {
   return 63 - __builtin_clzll(w64);
}

static inline UInt ULong__minIndex ( ULong w64 ) {
   return __builtin_ctzll(w64);
}

static inline void enlarge_rreg_lrs(RRegLRState* rreg_lrs)
{
   vassert(rreg_lrs->lrs_used == rreg_lrs->lrs_size);

   RRegLR* lr2 = LibVEX_Alloc_inline(2 * rreg_lrs->lrs_used * sizeof(RRegLR));
   for (UInt l = 0; l < rreg_lrs->lrs_used; l++) {
      lr2[l] = rreg_lrs->lrs[l];
   }

   rreg_lrs->lrs = lr2;
   rreg_lrs->lrs_size = 2 * rreg_lrs->lrs_used;
}

#define PRINT_STATE(what)                                                \
   do {                                                                  \
      print_state(chunk, vreg_state, n_vregs, rreg_state, INSTRNO_TOTAL, \
                  depth, con, what);                                     \
   } while (0)

static inline void print_state(const RegAllocChunk* chunk,
   const VRegState* vreg_state, UInt n_vregs, const RRegState* rreg_state,
   Short ii_total_current, UInt depth, const RegAllocControl* con,
   const HChar* comment)
{
   print_depth(depth);
   vex_printf("%s (current instruction total #%d):\n",
              comment, ii_total_current);

   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      const VRegState* vreg = &vreg_state[v_idx];

      if (vreg->live_after == INVALID_INSTRNO) {
         continue; /* This is a dead vreg. Never comes into live. */
      }
      print_depth(depth);
      vex_printf("vreg_state[%3u] \t", v_idx);

      UInt written;
      switch (vreg->disp) {
      case Unallocated:
         written = vex_printf("unallocated");
         break;
      case Assigned:
         written = vex_printf("assigned to ");
         written += con->ppReg(vreg->rreg);
         break;
      case Spilled:
         written = vex_printf("spilled at offset %u", vreg->spill_offset);
         break;
      default:
         vassert(0);
      }

      for (Int w = 30 - written; w > 0; w--) {
         vex_printf(" ");
      }

      if (vreg->live_after > ii_total_current) {
         vex_printf("[not live yet]");
      } else if (ii_total_current >= vreg->dead_before) {
         vex_printf("[now dead]");
      } else {
         vex_printf("[live]");
      }
      vex_printf(" [%d - %d)\n", vreg->live_after, vreg->dead_before);
   }

   for (UInt r_idx = 0; r_idx < chunk->n_rregs; r_idx++) {
      const RRegState* rreg = &rreg_state[r_idx];
      const RRegLR*    lr   = chunk->rreg_lr_state[r_idx].lr_current;
      print_depth(depth);
      vex_printf("rreg_state[%2u] = ", r_idx);
      UInt written = con->ppReg(con->univ->regs[r_idx]);
      for (Int w = 10 - written; w > 0; w--) {
         vex_printf(" ");
      }

      switch (rreg->disp) {
      case Free:
         vex_printf("free\n");
         break;
      case Bound:
         vex_printf("bound for ");
         con->ppReg(rreg->vreg);
         if (rreg->eq_spill_slot) {
            vex_printf("    (equals to its spill slot)");
         }
         vex_printf("\n");
         break;
      case Reserved:
         vex_printf("reserved - live range [%d, %d)\n",
                    lr->live_after, lr->dead_before);
         break;
      }
   }
}

static inline void emit_instr(RegAllocChunk* chunk, HInstr* instr, UInt depth,
                              const RegAllocControl* con, const HChar* why)
{
   if (DEBUG_REGALLOC) {
      print_depth(depth);
      vex_printf("**  ");
      con->ppInstr(instr, con->mode64);
      if (why != NULL) {
         vex_printf("          (%s)", why);
      }
      vex_printf("\n\n");
   }

   addHInstr(chunk->instrs_out, instr);
}

/* Updates register allocator state after vreg has been spilled. */
static inline void mark_vreg_spilled(
   UInt v_idx, VRegState* vreg_state, UInt n_vregs,
   RRegState* rreg_state, UInt n_rregs)
{
   HReg rreg = vreg_state[v_idx].rreg;
   UInt r_idx = hregIndex(rreg);

   vreg_state[v_idx].disp          = Spilled;
   vreg_state[v_idx].rreg          = INVALID_HREG;
   rreg_state[r_idx].disp          = Free;
   rreg_state[r_idx].vreg          = INVALID_HREG;
   rreg_state[r_idx].eq_spill_slot = False;
}

/* Spills a vreg assigned to some rreg.
   The vreg is spilled and the rreg is freed.
   Returns rreg's index. */
static inline UInt spill_vreg(
   RegAllocChunk* chunk,
   VRegState* vreg_state, UInt n_vregs, RRegState* rreg_state,
   HReg vreg, UInt v_idx, Short ii_total_current,
   UInt depth, const RegAllocControl* con)
{
   UInt n_rregs = chunk->n_rregs;

   /* Check some invariants first. */
   vassert(IS_VALID_VREGNO((v_idx)));
   vassert(vreg_state[v_idx].disp == Assigned);
   HReg rreg = vreg_state[v_idx].rreg;
   UInt r_idx = hregIndex(rreg);
   vassert(IS_VALID_RREGNO(r_idx));
   vassert(hregClass(con->univ->regs[r_idx]) == hregClass(vreg));
   vassert(vreg_state[v_idx].dead_before > ii_total_current);
   vassert(vreg_state[v_idx].reg_class != HRcINVALID);

   /* Generate spill. */
   HInstr* spill1 = NULL;
   HInstr* spill2 = NULL;
   con->genSpill(&spill1, &spill2, rreg, vreg_state[v_idx].spill_offset,
                 con->mode64);
   vassert(spill1 != NULL || spill2 != NULL); /* cannot be both NULL */
   if (spill1 != NULL) {
      emit_instr(chunk, spill1, depth, con, "spill1");
   }
   if (spill2 != NULL) {
      emit_instr(chunk, spill2, depth, con, "spill2");
   }

   mark_vreg_spilled(v_idx, vreg_state, n_vregs, rreg_state, n_rregs);
   return r_idx;
}

/* Chooses a vreg to be spilled based on various criteria.
   The vreg must not be from the instruction being processed, that is, it must
   not be listed in reg_usage->vRegs. */
static inline HReg find_vreg_to_spill(
   const RegAllocChunk* chunk,
   const VRegState* vreg_state, UInt n_vregs, const RRegState* rreg_state,
   const HRegUsage* instr_regusage, HRegClass target_hregclass,
   Short ii_chunk_current, const RegAllocControl* con)
{
   Short scan_forward_start = ii_chunk_current + 1;
   Short scan_forward_max   = chunk->ii_vec_len - 1;

   /* Scan forwards a few instructions to find the most distant mentioned
      use of a vreg. We can scan in the range of (inclusive):
      - reg_usage[scan_forward_start]
      - reg_usage[scan_forward_end], where scan_forward_end
           = MIN(scan_forward_max, scan_forward_start + FEW_INSTRUCTIONS).
      reg_usage uses chunk instruction numbering. */
#  define FEW_INSTRUCTIONS 5
   Short scan_forward_end
      = (scan_forward_max <= scan_forward_start + FEW_INSTRUCTIONS) ?
        scan_forward_max : scan_forward_start + FEW_INSTRUCTIONS;
#  undef FEW_INSTRUCTIONS

   HReg vreg_found = INVALID_HREG;
   Short distance_so_far = 0;

   for (UInt r_idx = con->univ->allocable_start[target_hregclass];
        r_idx <= con->univ->allocable_end[target_hregclass]; r_idx++) {

      const RRegState* rreg = &rreg_state[r_idx];
      if (rreg->disp == Bound) {
         HReg vreg = rreg->vreg;
         if (! HRegUsage__contains(instr_regusage, vreg)) {
            Short ii_chunk = scan_forward_start;
            for ( ; ii_chunk <= scan_forward_end; ii_chunk++) {
               if (HRegUsage__contains(&chunk->reg_usage[ii_chunk], vreg)) {
                  break;
               }
            }

            if (ii_chunk - scan_forward_start > distance_so_far) {
               distance_so_far = ii_chunk - scan_forward_start;
               vreg_found = vreg;
               if (ii_chunk + distance_so_far == scan_forward_end) {
                  break; /* We are at the end. Nothing could be better. */
               }
            }
         }
      }
   }

   if (hregIsInvalid(vreg_found)) {
      vex_printf("registerAllocation: cannot find a register in class: ");
      ppHRegClass(target_hregclass);
      vex_printf("\n");
      vpanic("registerAllocation: cannot find a register.");
   }

   return vreg_found;
}

/* Find a free rreg of the correct class.
   Tries to find an rreg whose live range (if any) is as far ahead in the
   incoming instruction stream as possible. An ideal rreg candidate is
   a callee-save register because it won't be used for parameter passing
   around helper function calls. */
static Bool find_free_rreg(
   const RegAllocChunk* chunk,
   const VRegState* vreg_state, UInt n_vregs, const RRegState* rreg_state,
   Short ii_chunk_current, HRegClass target_hregclass,
   Bool reserve_phase, const RegAllocControl* con, UInt* r_idx_found)
{
   Bool found = False;
   Short distance_so_far = 0; /* running max for |live_after - current_ii| */

   for (UInt r_idx = con->univ->allocable_start[target_hregclass];
        r_idx <= con->univ->allocable_end[target_hregclass]; r_idx++) {
      const RRegState*   rreg     = &rreg_state[r_idx];
      const RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
      if (rreg->disp == Free) {
         if (rreg_lrs->lrs_used == 0) {
            found = True;
            *r_idx_found = r_idx;
            break; /* There could be nothing better, so break now. */
         } else {
            const RRegLR* lr = rreg_lrs->lr_current;
            if (lr->live_after > ii_chunk_current) {
               /* Not live, yet. */
               if ((lr->live_after - ii_chunk_current) > distance_so_far) {
                  distance_so_far = lr->live_after - ii_chunk_current;
                  found = True;
                  *r_idx_found = r_idx;
               }
            } else if (ii_chunk_current >= lr->dead_before) {
               /* Now dead. Effectively as if there is no LR now. */
               found = True;
               *r_idx_found = r_idx;
               break; /* There could be nothing better, so break now. */
            } else {
               /* Going live for this instruction. This could happen only when
                  rregs are being reserved en mass, for example before
                  a helper call. */
               vassert(reserve_phase);
            }
         }
      }
   }

   return found;
}


/* --- Stage 1. ---
   Determine total ordering of instructions and structure of HInstrIfThenElse.
   Build similar structure of RegAllocChunk's. */
static UInt stage1(HInstrVec* instrs_in, UInt ii_total_start, UInt n_rregs,
                   RegAllocChunk** first_chunk, const RegAllocControl* con)
{
   vassert(instrs_in->insns_used > 0);

   Short ii_vec_start = 0;

   RegAllocChunk* chunk  = new_chunk(instrs_in, n_rregs);
   chunk->ii_vec_start   = ii_vec_start;
   chunk->ii_total_start = ii_total_start;
   chunk->instrs_out     = newHInstrVec();
   *first_chunk = chunk;

   /* Now split incoming HInstrVec into chunks separated by HInstrIfThenElse. */
   for (Short ii_vec = 0; ii_vec < instrs_in->insns_used; ii_vec++) {
      HInstr* instr = instrs_in->insns[ii_vec];

      HInstrIfThenElse* hite = con->isIfThenElse(instr);
      if (LIKELY((hite == NULL) && (ii_vec < instrs_in->insns_used - 1))) {
         continue;
      }

      /* A chunk before HInstrIfThenElse or the last chunk of HInstrVec. */
      if (hite != NULL) {
         /* Omit HInstrIfThenElse. */
         chunk->ii_vec_len = ii_vec - ii_vec_start;
         ii_vec++;
      } else {
         chunk->ii_vec_len = (ii_vec - ii_vec_start) + 1;
      }
      ii_total_start += chunk->ii_vec_len;
      ii_vec_start = ii_vec;

      if (hite != NULL) {
         RegAllocChunk* chunk_fallThrough;
         UInt ii_total_fallThrough = stage1(hite->fallThrough, ii_total_start,
                                            n_rregs, &chunk_fallThrough, con);
         RegAllocChunk* chunk_outOfLine;
         UInt ii_total_outOfLine = stage1(hite->outOfLine, ii_total_start,
                                          n_rregs, &chunk_outOfLine, con);

         chunk->isIfThenElse           = True;
         chunk->IfThenElse.ccOOL       = hite->ccOOL;
         chunk->IfThenElse.fallThrough = chunk_fallThrough;
         chunk->IfThenElse.outOfLine   = chunk_outOfLine;
         chunk->IfThenElse.phi_nodes   = hite->phi_nodes;
         chunk->IfThenElse.n_phis      = hite->n_phis;

         ii_total_start = MAX(ii_total_fallThrough, ii_total_outOfLine);
      }

      if (ii_vec < instrs_in->insns_used - 1) {
         RegAllocChunk* previous = chunk;
         chunk                   = new_chunk(instrs_in, n_rregs);
         chunk->ii_vec_start     = ii_vec_start;
         chunk->ii_total_start   = toShort(ii_total_start);
         chunk->instrs_out       = (*first_chunk)->instrs_out;
         previous->next          = chunk;
      }
   }

   return ii_total_start;
}


/* --- Stage 2. ---
   Scan the incoming instructions.
   Note: vreg state is initially global (shared accross all chunks).
         rreg state is inherently local to every chunk. */
static void stage2_chunk(RegAllocChunk* chunk, VRegState* vreg_state,
             UInt n_vregs, UInt n_rregs, UInt depth, const RegAllocControl* con)
{
   /* Info on register usage in the incoming instructions. Computed once
      and remains unchanged, more or less; updated sometimes by the
      direct-reload optimisation. */
   chunk->reg_usage = LibVEX_Alloc_inline(sizeof(HRegUsage) * chunk->ii_vec_len);

#  define OFFENDING_VREG(_v_idx, _instr, _mode)                   \
   do {                                                           \
      vex_printf("\n\nOffending vreg = %u\n", (_v_idx));          \
      vex_printf("\nOffending instruction = ");                   \
      con->ppInstr((_instr), con->mode64);                        \
      vex_printf("\n");                                           \
      vpanic("registerAllocation: first event for vreg is "#_mode \
             " (should be Write)");                               \
   } while (0)

#  define OFFENDING_RREG(_r_idx, _instr, _mode)                    \
   do {                                                            \
      vex_printf("\n\nOffending rreg = ");                         \
      con->ppReg(con->univ->regs[(_r_idx)]);                       \
      vex_printf("\nOffending instruction = ");                    \
      con->ppInstr((_instr), con->mode64);                         \
      vex_printf("\n");                                            \
      vpanic("registerAllocation: first event for rreg is "#_mode" \
             (should be Write)");                                  \
   } while (0)

   Short ii_chunk = 0;
   for (Short ii_vec = chunk->ii_vec_start;
        ii_vec < chunk->ii_vec_start + chunk->ii_vec_len;
        ii_vec++, ii_chunk++) {
      const HInstr* instr = chunk->instrs_in->insns[ii_vec];

      con->getRegUsage(&chunk->reg_usage[ii_chunk], instr, con->mode64);

      if (0) {
         vex_printf("\n");
         print_depth(depth);
         vex_printf("stage 2: %d (chunk) %d (vec) %d (total) stage 2: ",
                    ii_chunk, ii_vec, INSTRNO_TOTAL);
         con->ppInstr(instr, con->mode64);
         vex_printf("\n");
         print_depth(depth);
         ppHRegUsage(con->univ, &chunk->reg_usage[ii_chunk]);
      }

      /* Process virtual registers mentioned in the instruction. */
      for (UInt j = 0; j < chunk->reg_usage[ii_chunk].n_vRegs; j++) {
         HReg vreg = chunk->reg_usage[ii_chunk].vRegs[j];
         vassert(hregIsVirtual(vreg));

         UInt v_idx = hregIndex(vreg);
         if (!IS_VALID_VREGNO(v_idx)) {
            vex_printf("\n");
            print_depth(depth);
            con->ppInstr(instr, con->mode64);
            vex_printf("\n");
            vex_printf("vreg %u (n_vregs %u)\n", v_idx, n_vregs);
            vpanic("registerAllocation (stage 2): out-of-range vreg");
         }

         /* Note the register class. */
         if (vreg_state[v_idx].reg_class == HRcINVALID) {
            /* First mention of this vreg. */
            vreg_state[v_idx].reg_class = hregClass(vreg);
         } else {
            /* Seen it before, so check for consistency. */
            vassert(vreg_state[v_idx].reg_class == hregClass(vreg));
         }

         /* Consider live ranges. */
         switch (chunk->reg_usage[ii_chunk].vMode[j]) {
         case HRmRead:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               OFFENDING_VREG(v_idx, instr, "Read");
            }
            break;
         case HRmWrite:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               vreg_state[v_idx].live_after = INSTRNO_TOTAL;
            } else if (vreg_state[v_idx].live_after > INSTRNO_TOTAL) {
               vreg_state[v_idx].live_after = INSTRNO_TOTAL;
            }
            break;
         case HRmModify:
            if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
               OFFENDING_VREG(v_idx, instr, "Modify");
            }
            break;
         default:
            vassert(0);
         }

         if (vreg_state[v_idx].dead_before < INSTRNO_TOTAL + 1) {
            vreg_state[v_idx].dead_before = INSTRNO_TOTAL + 1;
         }
      }

      /* Process real registers mentioned in the instruction. */
      const ULong rRead      = chunk->reg_usage[ii_chunk].rRead;
      const ULong rWritten   = chunk->reg_usage[ii_chunk].rWritten;
      const ULong rMentioned = rRead | rWritten;

      if (rMentioned != 0) {
         UInt rReg_minIndex = ULong__minIndex(rMentioned);
         UInt rReg_maxIndex = ULong__maxIndex(rMentioned);
         /* Don't bother to look at registers which are not available
            to the allocator such as the stack or guest state pointers. These
            are unavailable to the register allocator and so we never visit
            them. We asserted above that n_rregs > 0, so (n_rregs - 1) is
            safe. */
         if (rReg_maxIndex >= n_rregs) {
            rReg_maxIndex = n_rregs - 1;
         }

         for (UInt r_idx = rReg_minIndex; r_idx <= rReg_maxIndex; r_idx++) {
            const ULong jMask = 1ULL << r_idx;

            if (LIKELY((rMentioned & jMask) == 0)) {
               continue;
            }

            RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
            const Bool isR = (rRead    & jMask) != 0;
            const Bool isW = (rWritten & jMask) != 0;

            if (isW && !isR) {
               if (rreg_lrs->lrs_used == rreg_lrs->lrs_size) {
                  enlarge_rreg_lrs(rreg_lrs);
               }

               rreg_lrs->lrs[rreg_lrs->lrs_used].live_after = ii_chunk;
               rreg_lrs->lrs[rreg_lrs->lrs_used].dead_before = ii_chunk + 1;
               rreg_lrs->lrs_used += 1;
             } else if (!isW && isR) {
               if ((rreg_lrs->lrs_used == 0)
                   || (rreg_lrs->lrs[rreg_lrs->lrs_used - 1].live_after
                                                          == INVALID_INSTRNO)) {
                  OFFENDING_RREG(r_idx, instr, "Read");
               }
               rreg_lrs->lrs[rreg_lrs->lrs_used - 1].dead_before = ii_chunk + 1;
            } else {
               vassert(isR && isW);
               if ((rreg_lrs->lrs_used == 0)
                   || (rreg_lrs->lrs[rreg_lrs->lrs_used - 1].live_after
                                                          == INVALID_INSTRNO)) {
                  OFFENDING_RREG(r_idx, instr, "Modify");
               }
               rreg_lrs->lrs[rreg_lrs->lrs_used - 1].dead_before = ii_chunk + 1;
            }
         }
      }
   }
}

static void stage2_phi_nodes(RegAllocChunk* chunk, VRegState* vreg_state,
                           UInt n_vregs, UInt depth, const RegAllocControl* con)
{
   vassert(chunk->next != NULL);
   Short ii_total_next = chunk->next->ii_total_start;

   for (UInt p = 0; p < chunk->IfThenElse.n_phis; p++) {
      const HPhiNode* phi = &chunk->IfThenElse.phi_nodes[p];

      /* Extend dead-before of source vregs up to the first instruction
         after join from If-Then-Else. */
      UInt v_idx_fallThrough = hregIndex(phi->srcFallThrough);
      vassert(vreg_state[v_idx_fallThrough].live_after != INVALID_INSTRNO);
      if (vreg_state[v_idx_fallThrough].dead_before < ii_total_next + 1) {
         vreg_state[v_idx_fallThrough].dead_before = ii_total_next + 1;
      }

      UInt v_idx_outOfLine = hregIndex(phi->srcOutOfLine);
      vassert(vreg_state[v_idx_outOfLine].live_after != INVALID_INSTRNO);
      if (vreg_state[v_idx_outOfLine].dead_before < ii_total_next + 1) {
         vreg_state[v_idx_outOfLine].dead_before = ii_total_next + 1;
      }

      /* Live range for destination vreg begins here. */
      UInt v_idx_dst = hregIndex(phi->dst);
      vassert(vreg_state[v_idx_dst].live_after == INVALID_INSTRNO);
      vreg_state[v_idx_dst].live_after = ii_total_next;
      vreg_state[v_idx_dst].dead_before = ii_total_next + 1;
   }
}

static void stage2(RegAllocChunk* chunk, VRegState* vreg_state, UInt n_vregs,
                   UInt n_rregs, UInt depth, const RegAllocControl* con)
{
   WALK_CHUNKS(stage2_chunk(chunk, vreg_state, n_vregs, n_rregs, depth, con),
               ;,
               stage2(chunk->IfThenElse.fallThrough, vreg_state, n_vregs,
                      n_rregs, depth + 1, con),
               stage2(chunk->IfThenElse.outOfLine, vreg_state, n_vregs,
                      n_rregs, depth + 1, con),
               stage2_phi_nodes(chunk, vreg_state, n_vregs, depth, con));
}

static void stage2_debug_vregs(const VRegState* vreg_state, UInt n_vregs)
{
   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      vex_printf("vreg %3u:  [%3d, %3d)\n",
                 v_idx, vreg_state[v_idx].live_after,
                 vreg_state[v_idx].dead_before);
   }
}

static void stage2_debug_rregs_chunk(RegAllocChunk* chunk, UInt depth,
                                     const RegAllocControl* con)
{
   Bool any_lrs = False;
   for (UInt r_idx = 0; r_idx < chunk->n_rregs; r_idx++) {
      const RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
      if (rreg_lrs->lrs_used > 0) {
         any_lrs = True;
         print_depth(depth);
         vex_printf("rreg %2u (", r_idx);
         UInt written = con->ppReg(con->univ->regs[r_idx]);
         vex_printf("):");
         for (Int t = 15 - written; t > 0; t--) {
            vex_printf(" ");
         }

         for (UInt l = 0; l < rreg_lrs->lrs_used; l++) {
            vex_printf("[%3d, %3d) ",
                     rreg_lrs->lrs[l].live_after, rreg_lrs->lrs[l].dead_before);
         }
         vex_printf("\n");
      }
   }

   if (!any_lrs) {
      print_depth(depth);
      vex_printf("[no rreg live ranges for this chunk]\n");
   }
}

static void stage2_debug_rregs(RegAllocChunk* chunk, UInt depth,
                               const RegAllocControl* con)
{
   WALK_CHUNKS(stage2_debug_rregs_chunk(chunk, depth, con),
               ;,
               stage2_debug_rregs(chunk->IfThenElse.fallThrough, depth + 1, con),
               stage2_debug_rregs(chunk->IfThenElse.outOfLine, depth + 1, con),
               ;);
}

/* Allocates spill slots. Because VRegState is initiall global, also spill slots
   are initially global. This might have an adverse effect that spill slots will
   eventuall run out if there are too many nested If-Then-Else legs. In that
   case, VRegState must not be initially global but rather local to every leg;
   and vregs will need to eventually have extended their live ranges after legs
   merge. */
static void stage3(VRegState* vreg_state, UInt n_vregs,
                   const RegAllocControl* con)
{
#  define N_SPILL64S (LibVEX_N_SPILL_BYTES / 8)
   STATIC_ASSERT((N_SPILL64S % 2) == 0);
   STATIC_ASSERT((LibVEX_N_SPILL_BYTES % LibVEX_GUEST_STATE_ALIGN) == 0);

   Short ss_busy_until_before[N_SPILL64S];
   vex_bzero(&ss_busy_until_before, sizeof(ss_busy_until_before));

   /* Each spill slot is 8 bytes long. For vregs which take more than 64 bits
      to spill (for example classes Flt64 and Vec128), we have to allocate two
      consecutive spill slots. For 256 bit registers (class Vec256), we have to
      allocate four consecutive spill slots.

      For Vec128-class on PowerPC, the spill slot's actual address must be
      16-byte aligned. Since the spill slot's address is computed as an offset
      from the guest state pointer, and since the user of the generated code
      must set that pointer to a 32-byte aligned value, we have the residual
      obligation here of choosing a 16-byte aligned spill slot offset for
      Vec128-class values. Since each spill slot is 8 bytes long, that means for
      Vec128-class values we must allocate a spill slot number which is
      zero mod 2.

      Similarly, for Vec256 class on amd64, find a spill slot number which is
      zero mod 4. This guarantees it will be 32-byte aligned, which isn't
      actually necessary on amd64 (we use movUpd etc to spill), but seems like
      a good practice.

      Do a rank-based allocation of vregs to spill slot numbers. We put as few
      values as possible in spill slots, but nevertheless need to have a spill
      slot available for all vregs, just in case. */

   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      /* True iff this vreg is unused. In which case we also expect that the
         reg_class field for it has not been set.  */
      if (vreg_state[v_idx].live_after == INVALID_INSTRNO) {
         vassert(vreg_state[v_idx].reg_class == HRcINVALID);
         continue;
      }

      /* The spill slots are 64 bits in size.  As per the comment on definition
         of HRegClass in host_generic_regs.h, that means, to spill a vreg of
         class Flt64 or Vec128, we'll need to find two adjacent spill slots to
         use. For Vec256, we'll need to find four adjacent slots to use. Note,
         this logic needs to be kept in sync with the size info on the
         definition of HRegClass. */
      UInt ss_no;
      switch (vreg_state[v_idx].reg_class) {
         case HRcFlt64:
         case HRcVec128:
            /* Find two adjacent free slots which provide up to 128 bits to
               spill the vreg. Since we are trying to find an even:odd pair,
               move along in steps of 2 (slots). */
            for (ss_no = 0; ss_no < N_SPILL64S - 1; ss_no += 2) {
               if (ss_busy_until_before[ss_no + 0] <= vreg_state[v_idx].live_after
                 && ss_busy_until_before[ss_no + 1] <= vreg_state[v_idx].live_after)
                  break;
            }
            if (ss_no >= N_SPILL64S - 1) {
               vpanic("N_SPILL64S is too low in VEX. Increase and recompile.");
            }
            ss_busy_until_before[ss_no + 0] = vreg_state[v_idx].dead_before;
            ss_busy_until_before[ss_no + 1] = vreg_state[v_idx].dead_before;
            break;
         default:
            /* The ordinary case -- just find a single lowest-numbered spill
               slot which is available at the start point of this interval,
               and assign the interval to it. */
            for (ss_no = 0; ss_no < N_SPILL64S; ss_no++) {
               if (ss_busy_until_before[ss_no] <= vreg_state[v_idx].live_after)
                  break;
            }
            if (ss_no == N_SPILL64S) {
               vpanic("N_SPILL64S is too low in VEX. Increase and recompile.");
            }
            ss_busy_until_before[ss_no] = vreg_state[v_idx].dead_before;
            break;
      }

      /* This reflects VEX's hard-wired knowledge of the guest state layout:
         the guest state itself, then two equal sized areas following it for two
         sets of shadow state, and then the spill area. */
      vreg_state[v_idx].spill_offset
         = toShort(con->guest_sizeB * 3 + ss_no * 8);

      /* Independent check that we've made a sane choice of the slot. */
      switch (vreg_state[v_idx].reg_class) {
      case HRcVec128: case HRcFlt64:
         vassert((vreg_state[v_idx].spill_offset % 16) == 0);
         break;
      default:
         vassert((vreg_state[v_idx].spill_offset % 8) == 0);
         break;
      }
   }

   if (0) {
      vex_printf("\n\n");
      for (UInt v_idx = 0; v_idx < n_vregs; v_idx++)
         vex_printf("vreg %3u    --> spill offset %u\n",
                    v_idx, vreg_state[v_idx].spill_offset);
   }

#  undef N_SPILL64S
}


static void stage4_chunk(RegAllocChunk* chunk,
                     VRegState* vreg_state, UInt n_vregs, RRegState* rreg_state,
                     UInt depth, const RegAllocControl* con)
{
   UInt n_rregs = chunk->n_rregs;

/* Finds an rreg of the correct class.
   If a free rreg is not found, then spills a vreg not used by the current
   instruction and makes free the corresponding rreg. */
#  define FIND_OR_MAKE_FREE_RREG(_v_idx, _reg_class, _reserve_phase)           \
   ({                                                                          \
      UInt _r_free_idx = -1;                                                   \
      Bool free_rreg_found = find_free_rreg(chunk,                             \
                                    vreg_state, n_vregs, rreg_state,           \
                                    ii_chunk, (_reg_class), (_reserve_phase),  \
                                    con, &_r_free_idx);                        \
      if (!free_rreg_found) {                                                  \
         HReg vreg_to_spill = find_vreg_to_spill(chunk,                        \
                                    vreg_state, n_vregs, rreg_state,           \
                                    &chunk->reg_usage[ii_chunk], (_reg_class), \
                                    ii_chunk, con);                            \
         _r_free_idx = spill_vreg(chunk, vreg_state, n_vregs, rreg_state,      \
                                  vreg_to_spill, hregIndex(vreg_to_spill),     \
                                  INSTRNO_TOTAL, depth, con);                  \
      }                                                                        \
                                                                               \
      vassert(IS_VALID_RREGNO(_r_free_idx));                                   \
                                                                               \
      _r_free_idx;                                                             \
   })

   Short ii_chunk = 0;
   for (Short ii_vec = chunk->ii_vec_start;
        ii_vec < chunk->ii_vec_start + chunk->ii_vec_len;
        ii_vec++, ii_chunk++) {

      HInstr* instr = chunk->instrs_in->insns[ii_vec];
      HRegUsage* reg_usage = &chunk->reg_usage[ii_chunk];

      if (DEBUG_REGALLOC) {
         vex_printf("\n");
         print_depth(depth);
         vex_printf("====---- Instr: chunk %d, vec %d, total %d ----====\n",
                    ii_chunk, ii_vec, INSTRNO_TOTAL);
         print_depth(depth);
         vex_printf("---- ");
         con->ppInstr(chunk->instrs_in->insns[ii_vec], con->mode64);
         vex_printf("\n\n");
         PRINT_STATE("Initial Register Allocator state");
         vex_printf("\n");
      }

      /* ------------ Sanity checks ------------ */

      /* Sanity checks are relatively expensive. So they are done only once
         every 17 instructions, and just before the last instruction in every
         HInstrVec. */
      Bool do_sanity_check
         = toBool(
              SANITY_CHECKS_EVERY_INSTR
              || ii_vec == chunk->ii_vec_len - 1
              || (ii_chunk > 0 && (ii_chunk % 17) == 0)
           );

      if (do_sanity_check) {
         /* Sanity check: the vreg_state and rreg_state mutually-redundant
            mappings are consistent. If vreg_state[v].rreg points at some
            rreg_state entry then that rreg_state entry should point back at
            vreg_state[v]. */
         for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
            if (vreg_state[v_idx].disp == Assigned) {
               vassert(!hregIsVirtual(vreg_state[v_idx].rreg));

               UInt r_idx = hregIndex(vreg_state[v_idx].rreg);
               vassert(IS_VALID_RREGNO(r_idx));
               vassert(rreg_state[r_idx].disp == Bound);
               vassert(hregIndex(rreg_state[r_idx].vreg) == v_idx);

               vassert(hregClass(vreg_state[v_idx].rreg)
                       == hregClass(con->univ->regs[r_idx]));
            }
         }

         for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
            const RRegState* rreg = &rreg_state[r_idx];
            if (rreg->disp == Bound) {
               vassert(hregIsVirtual(rreg->vreg));

               UInt v_idx = hregIndex(rreg->vreg);
               vassert(IS_VALID_VREGNO(v_idx));
               vassert(vreg_state[v_idx].disp == Assigned);
               vassert(hregIndex(vreg_state[v_idx].rreg) == r_idx);
            } else {
               vassert(rreg_state[r_idx].eq_spill_slot == False);
            }
         }

         /* Sanity check: if rreg has been marked as Reserved, there must be
            a corresponding hard live range for it. */
         for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
            if (rreg_state[r_idx].disp == Reserved) {
               const RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
               vassert(rreg_lrs->lrs_used > 0);
               vassert(rreg_lrs->lr_current_idx < rreg_lrs->lrs_used);
               vassert(rreg_lrs->lr_current->live_after <= ii_chunk);
               vassert(ii_chunk < rreg_lrs->lr_current->dead_before);
            }
         }
      }


      /* --- MOV coalescing --- */
      /* Optimise register coalescing:
            MOV  v <-> v   coalescing (done here).
            MOV  v <-> r   coalescing (TODO: not yet). */
      /* If doing a reg-reg move between two vregs, and the src's live
         range ends here and the dst's live range starts here, bind the dst
         to the src's rreg, and that's all. */
      HReg vregS = INVALID_HREG;
      HReg vregD = INVALID_HREG;
      if (con->isMove(instr, &vregS, &vregD)) {
         if (hregIsVirtual(vregS) && hregIsVirtual(vregD)) {
            /* Check that |isMove| is not telling us a bunch of lies ... */
            vassert(hregClass(vregS) == hregClass(vregD));
            UInt vs_idx = hregIndex(vregS);
            UInt vd_idx = hregIndex(vregD);
            vassert(IS_VALID_VREGNO(vs_idx));
            vassert(IS_VALID_VREGNO(vd_idx));

            if ((vreg_state[vs_idx].dead_before == INSTRNO_TOTAL + 1)
                && (vreg_state[vd_idx].live_after == INSTRNO_TOTAL)
                && (vreg_state[vs_idx].disp == Assigned)) {

               /* Live ranges are adjacent and source vreg is bound.
                  Finally we can do the coalescing.  */
               HReg rreg = vreg_state[vs_idx].rreg;
               vreg_state[vd_idx].disp = Assigned;
               vreg_state[vd_idx].rreg = rreg;
               vreg_state[vs_idx].disp = Unallocated;
               vreg_state[vs_idx].rreg = INVALID_HREG;

               UInt r_idx = hregIndex(rreg);
               vassert(rreg_state[r_idx].disp == Bound);
               rreg_state[r_idx].vreg          = vregD;
               rreg_state[r_idx].eq_spill_slot = False;

               if (DEBUG_REGALLOC) {
                  print_depth(depth);
                  vex_printf("coalesced: ");
                  con->ppReg(vregS);
                  vex_printf(" -> ");
                  con->ppReg(vregD);
                  vex_printf("\n\n");
               }

               /* In rare cases it can happen that vregD's live range ends
                  here. Check and eventually free the vreg and rreg.
                  This effectively means that either the translated program
                  contained dead code (although VEX iropt passes are pretty good
                  at eliminating it) or the VEX backend generated dead code. */
               if (vreg_state[vd_idx].dead_before <= INSTRNO_TOTAL + 1) {
                  vreg_state[vd_idx].disp = Unallocated;
                  vreg_state[vd_idx].rreg = INVALID_HREG;
                  rreg_state[r_idx].disp          = Free;
                  rreg_state[r_idx].vreg          = INVALID_HREG;
                  rreg_state[r_idx].eq_spill_slot = False;
               }

               /* Move on to the next instruction. We skip the post-instruction
                  stuff because all required house-keeping was done here. */
               continue;
            }
         }
      }


      /* --- Reserve and free rregs if needed. --- */
      /* If the rreg enters its hard live range and is not free:
         1. If the corresponding vreg is not used by the instruction, spill it.
         2. If the corresponding vreg is used by the instruction, then:
         2a. If there are no free rregs, spill a vreg not used by this
             instruction.
         2b. Move the corresponding vreg to a free rreg. This is better than
             spilling it and immediatelly reloading it.
       */
      const ULong rRead      = reg_usage->rRead;
      const ULong rWritten   = reg_usage->rWritten;
      const ULong rMentioned = rRead | rWritten;

      if (rMentioned != 0) {
         UInt rReg_minIndex = ULong__minIndex(rMentioned);
         UInt rReg_maxIndex = ULong__maxIndex(rMentioned);
         if (rReg_maxIndex >= n_rregs) {
            rReg_maxIndex = n_rregs - 1;
         }

         for (UInt r_idx = rReg_minIndex; r_idx <= rReg_maxIndex; r_idx++) {
            const ULong jMask = 1ULL << r_idx;

            if (LIKELY((rMentioned & jMask) == 0)) {
               continue;
            }

            RRegState* rreg = &rreg_state[r_idx];
            const RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
            if (LIKELY(rreg_lrs->lrs_used == 0)) {
               continue;
            }
            if (rreg->disp == Reserved) {
               continue;
            }

            if ((rreg_lrs->lr_current->live_after <= ii_chunk)
                && (ii_chunk < rreg_lrs->lr_current->dead_before)) {

               switch (rreg->disp) {
               case Bound: {
                  /* Yes, there is an associated vreg. We need to deal with
                     it now somehow. */
                  HReg vreg = rreg->vreg;
                  UInt v_idx = hregIndex(vreg);

                  if (! HRegUsage__contains(reg_usage, vreg)) {
                     if (rreg->eq_spill_slot) {
                        mark_vreg_spilled(v_idx, vreg_state, n_vregs,
                                          rreg_state, n_rregs);
                     } else {
                        /* Spill the vreg. It is not used by this instruction.*/
                        spill_vreg(chunk, vreg_state, n_vregs, rreg_state,
                                   vreg, v_idx, INSTRNO_TOTAL, depth, con);
                     }
                  } else {
                     /* Find or make a free rreg where to move this vreg to. */
                     UInt r_free_idx = FIND_OR_MAKE_FREE_RREG(
                                  v_idx, vreg_state[v_idx].reg_class, True);

                     /* Generate "move" between real registers. */
                     HInstr* move = con->genMove(con->univ->regs[r_idx],
                                      con->univ->regs[r_free_idx], con->mode64);
                     vassert(move != NULL);
                     emit_instr(chunk, move, depth, con, "move");

                     /* Update the register allocator state. */
                     vassert(vreg_state[v_idx].disp == Assigned);
                     vreg_state[v_idx].rreg = con->univ->regs[r_free_idx];
                     rreg_state[r_free_idx].disp = Bound;
                     rreg_state[r_free_idx].vreg = vreg;
                     rreg_state[r_free_idx].eq_spill_slot = rreg->eq_spill_slot;
                     rreg->disp          = Free;
                     rreg->vreg          = INVALID_HREG;
                     rreg->eq_spill_slot = False;
                  }
                  break;
               }
               case Free:
                  break;
               default:
                  vassert(0);
               }

               /* Finally claim the rreg as reserved. */
               rreg->disp = Reserved;

               if (DEBUG_REGALLOC) {
                  print_depth(depth);
                  vex_printf("rreg has been reserved: ");
                  con->ppReg(con->univ->regs[r_idx]);
                  vex_printf("\n\n");
               }
            }
         }
      }


      /* --- Direct reload optimisation. --- */
      /* If the instruction reads exactly one vreg which is currently spilled,
         and this is the last use of that vreg, see if we can convert
         the instruction into one that reads directly from the spill slot.
         This is clearly only possible for x86 and amd64 targets, since ppc and
         arm are load-store architectures. If successful, replace
         instrs_in->insns[ii] with this new instruction, and recompute
         its reg_usage, so that the change is invisible to the standard-case
         handling that follows. */
      if ((con->directReload != NULL) && (reg_usage->n_vRegs <= 2)) {
         Bool debug_direct_reload = False;
         Bool nreads = 0;
         HReg vreg_found = INVALID_HREG;
         Short spill_offset = 0;

         for (UInt j = 0; j < reg_usage->n_vRegs; j++) {
            HReg vreg = reg_usage->vRegs[j];
            vassert(hregIsVirtual(vreg));

            if (reg_usage->vMode[j] == HRmRead) {
               nreads++;
               UInt v_idx = hregIndex(vreg);
               vassert(IS_VALID_VREGNO(v_idx));
               if (vreg_state[v_idx].disp == Spilled) {
                  /* Is this its last use? */
                  vassert(vreg_state[v_idx].dead_before >= INSTRNO_TOTAL + 1);
                  if ((vreg_state[v_idx].dead_before == INSTRNO_TOTAL + 1)
                      && hregIsInvalid(vreg_found)) {
                     vreg_found = vreg;
                     spill_offset = vreg_state[v_idx].spill_offset;
                  }
               }
            }
         }

         if (!hregIsInvalid(vreg_found) && (nreads == 1)) {
            if (reg_usage->n_vRegs == 2) {
               vassert(! sameHReg(reg_usage->vRegs[0], reg_usage->vRegs[1]));
            }

            HInstr* reloaded = con->directReload(
                     chunk->instrs_in->insns[ii_vec], vreg_found, spill_offset);
            if (debug_direct_reload && (reloaded != NULL)) {
               print_depth(depth);
               vex_printf("[%3d] ", spill_offset);
               ppHReg(vreg_found);
               vex_printf(": ");
               con->ppInstr(instr, con->mode64);
            }
            if (reloaded != NULL) {
               /* Update info about the instruction, so it looks as if it had
                  been in this form all along. */
               instr = reloaded;
               chunk->instrs_in->insns[ii_vec] = reloaded;
               con->getRegUsage(reg_usage, instr, con->mode64);
               if (debug_direct_reload) {
                  vex_printf("  -->  ");
                  con->ppInstr(reloaded, con->mode64);
               }
            }

            if (debug_direct_reload && (reloaded != NULL)) {
               vex_printf("\n");
            }
         }
      }


      /* The vreg -> rreg map constructed and then applied to each
         instruction. */
         HRegRemap remap;
         initHRegRemap(&remap);

      /* --- Allocate vregs used by the instruction. --- */
      /* Vregs used by the instruction can be in the following states:
         - Unallocated: vreg is entering its live range. Find a free rreg.
         - Assigned: we do nothing; rreg has been allocated previously.
         - Spilled: Find a free rreg and reload vreg into it.
         Naturally, finding a free rreg may involve spilling a vreg not used by
         the instruction. */
      for (UInt j = 0; j < reg_usage->n_vRegs; j++) {
         HReg vreg = reg_usage->vRegs[j];
         vassert(hregIsVirtual(vreg));

         if (0) {
            print_depth(depth);
            vex_printf("considering ");
            con->ppReg(vreg);
            vex_printf("\n");
         }

         UInt v_idx = hregIndex(vreg);
         vassert(IS_VALID_VREGNO(v_idx));
         HReg rreg = vreg_state[v_idx].rreg;
         UInt r_idx;
         if (vreg_state[v_idx].disp == Assigned) {
            r_idx = hregIndex(rreg);
            vassert(rreg_state[r_idx].disp == Bound);
            addToHRegRemap(&remap, vreg, rreg);
         } else {
            vassert(hregIsInvalid(rreg));

            /* Find or make a free rreg of the correct class. */
            r_idx = FIND_OR_MAKE_FREE_RREG(
                                 v_idx, vreg_state[v_idx].reg_class, False);
            rreg = con->univ->regs[r_idx];

            /* Generate reload only if the vreg is spilled and is about to being
               read or modified. If it is merely written than reloading it first
               would be pointless. */
            if ((vreg_state[v_idx].disp == Spilled)
                && (reg_usage->vMode[j] != HRmWrite)) {

               HInstr* reload1 = NULL;
               HInstr* reload2 = NULL;
               con->genReload(&reload1, &reload2, rreg,
                         vreg_state[v_idx].spill_offset, con->mode64);
               vassert(reload1 != NULL || reload2 != NULL);
               if (reload1 != NULL) {
                  emit_instr(chunk, reload1, depth, con, "reload1");
               }
               if (reload2 != NULL) {
                  emit_instr(chunk, reload2, depth, con, "reload2");
               }
            }

            rreg_state[r_idx].disp          = Bound;
            rreg_state[r_idx].vreg          = vreg;
            rreg_state[r_idx].eq_spill_slot = True;
            vreg_state[v_idx].disp = Assigned;
            vreg_state[v_idx].rreg = rreg;
            addToHRegRemap(&remap, vreg, rreg);
         }

         /* If this vreg is written or modified, mark it so. */
         if (reg_usage->vMode[j] != HRmRead) {
            rreg_state[r_idx].eq_spill_slot = False;
         }
      }

      con->mapRegs(&remap, instr, con->mode64);
      emit_instr(chunk, instr, depth, con, NULL);

      if (DEBUG_REGALLOC) {
         print_depth(depth);
         PRINT_STATE("Register Allocator state after dealing with"
                     " the current instruction");
         vex_printf("\n");
      }

      /* ------ Post-instruction actions. ------ */
      /* Free rregs which:
         - Have been reserved and whose hard live range ended.
         - Have been bound to vregs whose live range ended. */
      for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
         RRegState* rreg       = &rreg_state[r_idx];
         RRegLRState* rreg_lrs = &chunk->rreg_lr_state[r_idx];
         switch (rreg->disp) {
         case Free:
            break;
         case Reserved:
            if (rreg_lrs->lrs_used > 0) {
               /* Consider "dead before" the next instruction. */
               if (rreg_lrs->lr_current->dead_before <= ii_chunk + 1) {
                  rreg_state[r_idx].disp          = Free;
                  rreg_state[r_idx].vreg          = INVALID_HREG;
                  rreg_state[r_idx].eq_spill_slot = False;
                  if (rreg_lrs->lr_current_idx < rreg_lrs->lrs_used - 1) {
                     rreg_lrs->lr_current_idx += 1;
                     rreg_lrs->lr_current
                        = &rreg_lrs->lrs[rreg_lrs->lr_current_idx];
                  }
               }
            }
            break;
         case Bound: {
            UInt v_idx = hregIndex(rreg->vreg);
            /* Consider "dead before" the next instruction. */
            if (vreg_state[v_idx].dead_before <= INSTRNO_TOTAL + 1) {
               vreg_state[v_idx].disp = Unallocated;
               vreg_state[v_idx].rreg = INVALID_HREG;
               rreg_state[r_idx].disp          = Free;
               rreg_state[r_idx].vreg          = INVALID_HREG;
               rreg_state[r_idx].eq_spill_slot = False;
            }
            break;
         }
         default:
            vassert(0);
         }
      }
   }

#  undef FIND_OR_MAKE_FREE_RREG
}

static void stage4_emit_HInstrIfThenElse(RegAllocChunk* chunk, UInt depth,
                                         const RegAllocControl* con)
{
   vassert(chunk->isIfThenElse);

   HInstrIfThenElse* hite = newHInstrIfThenElse(
      chunk->IfThenElse.ccOOL,
      chunk->IfThenElse.phi_nodes,
      chunk->IfThenElse.n_phis);
   hite->fallThrough = chunk->IfThenElse.fallThrough->instrs_out;
   hite->outOfLine   = chunk->IfThenElse.outOfLine->instrs_out;

   emit_instr(chunk, con->genHInstrITE(hite), depth, con, "HInstrIfThenElse");

}

static void stage4(RegAllocChunk* chunk, VRegState* vreg_state, UInt n_vregs,
                  RRegState* rreg_state, UInt depth, const RegAllocControl* con)
{
   WALK_CHUNKS(stage4_chunk(chunk, vreg_state, n_vregs, rreg_state, depth, con),
               stage4_emit_HInstrIfThenElse(chunk, depth, con),
               stage4(chunk->IfThenElse.fallThrough, vreg_state, n_vregs,
                      rreg_state, depth + 1, con),
               stage4(chunk->IfThenElse.outOfLine, vreg_state, n_vregs,
                      rreg_state, depth + 1, con),
               ;);
}


/* A target-independent register allocator (v3). Requires various functions
   which it uses to deal abstractly with instructions and registers, since it
   cannot have any target-specific knowledge.

   Returns a new code block of instructions, which, as a result of the behaviour
   of mapRegs, will be in-place modifications of the original instructions.

   Requires that the incoming code has been generated using vreg numbers
   0, 1 .. n_vregs-1. Appearance of a vreg outside that range is a checked
   run-time error.

   Takes unallocated instructions and returns allocated instructions.
*/
HInstrSB* doRegisterAllocation(
   /* Incoming virtual-registerised code. */
   HInstrSB* sb_in,

   /* Register allocator controls to use. */
   const RegAllocControl* con
)
{
   vassert((con->guest_sizeB % LibVEX_GUEST_STATE_ALIGN) == 0);

   /* The main register allocator state. */
   UInt       n_vregs = sb_in->n_vregs;
   VRegState* vreg_state = NULL;
   if (n_vregs > 0) {
      vreg_state = LibVEX_Alloc_inline(n_vregs * sizeof(VRegState));
   }

   /* If this is not so, the universe we have is nonsensical. */
   UInt n_rregs = con->univ->allocable;
   vassert(n_rregs > 0);
   STATIC_ASSERT(N_RREGUNIVERSE_REGS == 64);

   /* --- Stage 0. --- */
   /* Initialize the vreg state. It is initially global. --- */
   for (UInt v_idx = 0; v_idx < n_vregs; v_idx++) {
      vreg_state[v_idx].live_after   = INVALID_INSTRNO;
      vreg_state[v_idx].dead_before  = INVALID_INSTRNO;
      vreg_state[v_idx].reg_class    = HRcINVALID;
      vreg_state[v_idx].disp         = Unallocated;
      vreg_state[v_idx].rreg         = INVALID_HREG;
      vreg_state[v_idx].spill_offset = 0;
   }

   /* Initialize redundant rreg -> vreg state. A snaphost is taken for
      every Out-Of-Line leg. */
   RRegState* rreg_state = LibVEX_Alloc_inline(n_rregs * sizeof(RRegState));
   for (UInt r_idx = 0; r_idx < n_rregs; r_idx++) {
      rreg_state[r_idx].disp          = Free;
      rreg_state[r_idx].vreg          = INVALID_HREG;
      rreg_state[r_idx].eq_spill_slot = False;
   }

   /* --- Stage 1. Determine total ordering of instructions and structure
      of HInstrIfThenElse. --- */
   RegAllocChunk* first_chunk;
   UInt ii_total_last = stage1(sb_in->insns, 0, n_rregs, &first_chunk, con);

   /* The live range numbers are signed shorts, and so limiting the number
      of instructions to 15000 comfortably guards against them
      overflowing 32k. */
   vassert(ii_total_last <= 15000);

   /* --- Stage 2. Scan the incoming instructions. --- */
   stage2(first_chunk, vreg_state, n_vregs, n_rregs, 0, con);
   if (DEBUG_REGALLOC) {
      vex_printf("\n\nInitial register allocator state:\n");
      stage2_debug_vregs(vreg_state, n_vregs);
      stage2_debug_rregs(first_chunk, 0, con);
   }

   /* --- Stage 3. Allocate spill slots. --- */
   stage3(vreg_state, n_vregs, con);

   /* --- Stage 4. Process the instructions and allocate registers. --- */
   stage4(first_chunk, vreg_state, n_vregs, rreg_state, 0, con);

   /* The output SB of instructions. */
   HInstrSB* sb_out = LibVEX_Alloc_inline(sizeof(HInstrSB));
   sb_out->n_vregs  = n_vregs;
   sb_out->insns    = first_chunk->instrs_out;
   return sb_out;
}

/*----------------------------------------------------------------------------*/
/*---                                            host_generic_reg_alloc3.c ---*/
/*----------------------------------------------------------------------------*/
