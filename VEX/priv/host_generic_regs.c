
/*---------------------------------------------------------------*/
/*--- begin                               host_generic_regs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
      info@open-works.net

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "main_util.h"
#include "host_generic_regs.h"


/*---------------------------------------------------------*/
/*--- Representing HOST REGISTERS                       ---*/
/*---------------------------------------------------------*/

void ppHRegClass ( HRegClass hrc )
{
   switch (hrc) {
      case HRcInt32:   vex_printf("HRcInt32"); break;
      case HRcInt64:   vex_printf("HRcInt64"); break;
      case HRcFlt32:   vex_printf("HRcFlt32"); break;
      case HRcFlt64:   vex_printf("HRcFlt64"); break;
      case HRcVec64:   vex_printf("HRcVec64"); break;
      case HRcVec128:  vex_printf("HRcVec128"); break;
      default: vpanic("ppHRegClass");
   }
}

/* Generic printing for registers. */
void ppHReg ( HReg r ) 
{
   if (hregIsInvalid(r)) {
      vex_printf("HReg_INVALID");
      return;
   }
   const Bool   isV     = hregIsVirtual(r);
   const HChar* maybe_v = isV ? "v" : "";
   const UInt   regNN   = isV ? hregIndex(r) : hregEncoding(r);
   /* For real registers, we show the encoding.  But the encoding is
      always zero for virtual registers, so that's pointless -- hence
      show the index number instead. */
   switch (hregClass(r)) {
      case HRcInt32:   vex_printf("%%%sr%u", maybe_v, regNN); return;
      case HRcInt64:   vex_printf("%%%sR%u", maybe_v, regNN); return;
      case HRcFlt32:   vex_printf("%%%sF%u", maybe_v, regNN); return;
      case HRcFlt64:   vex_printf("%%%sD%u", maybe_v, regNN); return;
      case HRcVec64:   vex_printf("%%%sv%u", maybe_v, regNN); return;
      case HRcVec128:  vex_printf("%%%sV%u", maybe_v, regNN); return;
      default: vpanic("ppHReg");
   }
}


/*---------------------------------------------------------*/
/*--- Real register Universes.                          ---*/
/*---------------------------------------------------------*/

void RRegUniverse__init ( /*OUT*/RRegUniverse* univ )
{
   *univ = (RRegUniverse){};
   univ->size      = 0;
   univ->allocable = 0;
   for (UInt i = 0; i < N_RREGUNIVERSE_REGS; i++) {
      univ->regs[i] = INVALID_HREG;
   }
}

void RRegUniverse__check_is_sane ( const RRegUniverse* univ )
{
   /* Check Real-Register-Universe invariants.  All of these are
      important. */
   vassert(univ->size > 0);
   vassert(univ->size <= N_RREGUNIVERSE_REGS);
   vassert(univ->allocable <= univ->size);
   for (UInt i = 0; i < univ->size; i++) {
      HReg reg = univ->regs[i];
      vassert(!hregIsInvalid(reg));
      vassert(!hregIsVirtual(reg));
      vassert(hregIndex(reg) == i);
   }
   for (UInt i = univ->size; i < N_RREGUNIVERSE_REGS; i++) {
      HReg reg = univ->regs[i];
      vassert(hregIsInvalid(reg));
   }
}


/*---------------------------------------------------------*/
/*--- Helpers for recording reg usage (for reg-alloc)   ---*/
/*---------------------------------------------------------*/

void ppHRegUsage ( const RRegUniverse* univ, HRegUsage* tab )
{
   /* This is going to fail miserably if N_RREGUNIVERSE_REGS exceeds
      64.  So let's cause it to fail in an obvious way. */
   vassert(N_RREGUNIVERSE_REGS == 64);

   vex_printf("HRegUsage {\n");
   /* First print the real regs */
   for (UInt i = 0; i < N_RREGUNIVERSE_REGS; i++) {
      Bool rRd = (tab->rRead    & (1ULL << i)) != 0;
      Bool rWr = (tab->rWritten & (1ULL << i)) != 0;
      const HChar* str = "Modify ";
      /**/ if (!rRd && !rWr) { continue; }
      else if ( rRd && !rWr) { str = "Read   "; }
      else if (!rRd &&  rWr) { str = "Write  "; }
      /* else "Modify" is correct */
      vex_printf("   %s ", str);
      ppHReg(univ->regs[i]);
      vex_printf("\n");
   }
   /* and now the virtual registers */
   for (UInt i = 0; i < tab->n_vRegs; i++) {
      const HChar* str = NULL;
      switch (tab->vMode[i]) {
         case HRmRead:   str = "Read   "; break;
         case HRmWrite:  str = "Write  "; break;
         case HRmModify: str = "Modify "; break;
         default: vpanic("ppHRegUsage");
      }
      vex_printf("   %s ", str);
      ppHReg(tab->vRegs[i]);
      vex_printf("\n");
   }
   vex_printf("}\n");
}


/* Add a register to a usage table.  Combines incoming read uses with
   existing write uses into a modify use, and vice versa.  Does not
   create duplicate entries -- each reg is only mentioned once.  
*/
void addHRegUse ( HRegUsage* tab, HRegMode mode, HReg reg )
{
   /* Because real and virtual registers are represented differently,
      they have completely different paths here. */
   if (LIKELY(hregIsVirtual(reg))) {
      /* Virtual register */
      UInt i;
      /* Find it ... */
      for (i = 0; i < tab->n_vRegs; i++)
         if (sameHReg(tab->vRegs[i], reg))
            break;
      if (i == tab->n_vRegs) {
         /* Not found, add new entry. */
         vassert(tab->n_vRegs < N_HREGUSAGE_VREGS);
         tab->vRegs[tab->n_vRegs] = reg;
         tab->vMode[tab->n_vRegs] = mode;
         tab->n_vRegs++;
      } else {
         /* Found: combine or ignore. */
         /* This is a greatest-lower-bound operation in the poset:

               R   W
                \ /
                 M

            Need to do: tab->mode[i] = GLB(tab->mode, mode).  In this
            case very simple -- if tab->mode[i] != mode then result must
            be M.
         */
         if (tab->vMode[i] == mode) {
            /* duplicate, ignore */
         } else {
            tab->vMode[i] = HRmModify;
         }
      }
   } else {
      /* Real register */
      UInt ix = hregIndex(reg);
      vassert(ix < N_RREGUNIVERSE_REGS);
      ULong mask = 1ULL << ix;
      switch (mode) {
         case HRmRead:   tab->rRead |= mask; break;
         case HRmWrite:  tab->rWritten |= mask; break;
         case HRmModify: tab->rRead |= mask; tab->rWritten |= mask; break;
         default: vassert(0);
      }
   }
}

Bool HRegUsage__contains ( const HRegUsage* tab, HReg reg )
{
   vassert(!hregIsInvalid(reg));
   if (hregIsVirtual(reg)) {
      for (UInt i = 0; i < tab->n_vRegs; i++) {
         if (sameHReg(reg, tab->vRegs[i]))
            return True;
      }
      return False;
   } else {
      UInt ix = hregIndex(reg);
      vassert(ix < N_RREGUNIVERSE_REGS);
      ULong mentioned = tab->rRead | tab->rWritten;
      return (mentioned & (1ULL << ix)) != 0;
   }
   /*NOTREACHED*/
}


/*---------------------------------------------------------*/
/*--- Indicating register remappings (for reg-alloc)    ---*/
/*---------------------------------------------------------*/

void ppHRegRemap ( HRegRemap* map )
{
   Int   i;
   vex_printf("HRegRemap {\n");
   for (i = 0; i < map->n_used; i++) {
      vex_printf("   ");
      ppHReg(map->orig[i]);
      vex_printf("  -->  ");
      ppHReg(map->replacement[i]);
      vex_printf("\n");
   }
   vex_printf("}\n");
}


void addToHRegRemap ( HRegRemap* map, HReg orig, HReg replacement )
{
   Int i;
   for (i = 0; i < map->n_used; i++)
      if (sameHReg(map->orig[i], orig))
         vpanic("addToHRegMap: duplicate entry");
   if (!hregIsVirtual(orig))
      vpanic("addToHRegMap: orig is not a vreg");
   if (hregIsVirtual(replacement))
      vpanic("addToHRegMap: replacement is a vreg");

   vassert(map->n_used+1 < N_HREG_REMAP);
   map->orig[map->n_used]        = orig;
   map->replacement[map->n_used] = replacement;
   map->n_used++;
}


HReg lookupHRegRemap ( HRegRemap* map, HReg orig )
{
   Int i;
   if (!hregIsVirtual(orig))
      return orig;
   for (i = 0; i < map->n_used; i++)
      if (sameHReg(map->orig[i], orig))
         return map->replacement[i];
   vpanic("lookupHRegRemap: not found");
}


/*---------------------------------------------------------*/
/*--- Abstract instructions                             ---*/
/*---------------------------------------------------------*/

HInstrVec* newHInstrVec(void)
{
   HInstrVec* hv  = LibVEX_Alloc_inline(sizeof(HInstrVec));
   hv->insns_size = 4;
   hv->insns_used = 0;
   hv->insns      = LibVEX_Alloc_inline(hv->insns_size * sizeof(HInstr*));
   return hv;
}

__attribute__((noinline))
void addHInstr_SLOW(HInstrVec* hv, HInstr* instr)
{
   vassert(hv->insns_used == hv->insns_size);
   HInstr** insns2 = LibVEX_Alloc_inline(hv->insns_size * 2 * sizeof(HInstr*));
   for (UInt i = 0; i < hv->insns_size; i++) {
      insns2[i] = hv->insns[i];
   }
   hv->insns_size *= 2;
   hv->insns = insns2;
   addHInstr(hv, instr);
}

HInstrIfThenElse* newHInstrIfThenElse(HCondCode condCode, HPhiNode* phi_nodes,
                                      UInt n_phis)
{
   HInstrIfThenElse* hite = LibVEX_Alloc_inline(sizeof(HInstrIfThenElse));
   hite->ccOOL            = condCode;
   hite->fallThrough      = newHInstrVec();
   hite->outOfLine        = newHInstrVec();
   hite->phi_nodes        = phi_nodes;
   hite->n_phis           = n_phis;
   return hite;
}

static void print_depth(UInt depth) {
   for (UInt i = 0; i < depth; i++) {
      vex_printf("    ");
   }
}

void ppHPhiNode(const HPhiNode* phi_node)
{
   ppHReg(phi_node->dst);
   vex_printf(" = phi(");
   ppHReg(phi_node->srcFallThrough);
   vex_printf(",");
   ppHReg(phi_node->srcOutOfLine);
   vex_printf(")");
}

static void ppHInstrVec(const HInstrVec* code,
                        HInstrIfThenElse* (*isIfThenElse)(const HInstr*),
                        void (*ppInstr)(const HInstr*, Bool),
                        void (*ppCondCode)(HCondCode),
                        Bool mode64, UInt depth, UInt *insn_num)
{
   for (UInt i = 0; i < code->insns_used; i++) {
      const HInstr* instr = code->insns[i];
      const HInstrIfThenElse* hite = isIfThenElse(instr);
      if (UNLIKELY(hite != NULL)) {
         print_depth(depth);
         vex_printf("      if (!");
         ppCondCode(hite->ccOOL);
         vex_printf(") then fall-through {\n");
         ppHInstrVec(hite->fallThrough, isIfThenElse, ppInstr, ppCondCode,
                     mode64, depth + 1, insn_num);
         print_depth(depth);
         vex_printf("      } else out-of-line {\n");
         ppHInstrVec(hite->outOfLine, isIfThenElse, ppInstr, ppCondCode,
                     mode64, depth + 1, insn_num);
         print_depth(depth);
         vex_printf("      }\n");

         for (UInt j = 0; j < hite->n_phis; j++) {
            print_depth(depth);
            vex_printf("      ");
            ppHPhiNode(&hite->phi_nodes[j]);
            vex_printf("\n");
         }
      } else {
         vex_printf("%3u   ", (*insn_num)++);
         print_depth(depth);
         ppInstr(instr, mode64);
         vex_printf("\n");
      }
   }
}

HInstrSB* newHInstrSB(void)
{
   HInstrSB* hsb = LibVEX_Alloc_inline(sizeof(HInstrSB));
   hsb->insns    = newHInstrVec();
   hsb->n_vregs  = 0;
   return hsb;
}

void ppHInstrSB(const HInstrSB* code,
                HInstrIfThenElse* (*isIfThenElse)(const HInstr*),
                void (*ppInstr)(const HInstr*, Bool),
                void (*ppCondCode)(HCondCode), Bool mode64)
{
   UInt insn_num = 0;
   ppHInstrVec(code->insns, isIfThenElse, ppInstr, ppCondCode, mode64, 0,
               &insn_num);
}


/*---------------------------------------------------------*/
/*--- C-Call return-location actions                    ---*/
/*---------------------------------------------------------*/

void ppRetLoc ( RetLoc ska )
{
   switch (ska.pri) {
      case RLPri_INVALID:
         vex_printf("RLPri_INVALID"); return;
      case RLPri_None:
         vex_printf("RLPri_None");    return;
      case RLPri_Int:
         vex_printf("RLPri_Int");     return;
      case RLPri_2Int:
         vex_printf("RLPri_2Int");    return;
      case RLPri_V128SpRel:
         vex_printf("RLPri_V128SpRel(%d)", ska.spOff); return;
      case RLPri_V256SpRel:
         vex_printf("RLPri_V256SpRel(%d)", ska.spOff); return;
      default:
         vpanic("ppRetLoc");
   }
}


/*---------------------------------------------------------------*/
/*--- end                                 host_generic_regs.c ---*/
/*---------------------------------------------------------------*/
