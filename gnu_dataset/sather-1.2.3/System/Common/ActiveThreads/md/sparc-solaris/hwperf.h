/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#pragma ident   "%Z%%M% %I%     %E% SMI"



/*
 * Hwperf header definitions
 */

/*
 * Spitfire PCR & PIC asr definition.
 */

#define	SPF_PCR_ASR		%asr16		/* ASR definitions */
#define	SPF_PIC_ASR		%asr17

/*
 * Spitfire PCR definition.
 */
#define	SPF_PCR_PRIV		0x0001		/* Privileged */
#define	SPF_PCR_ST		0x0002		/* System Trace */
#define	SPF_PCR_UT		0x0004		/* User Trace */

#define	SPF_PCR_SO_CC		0x0000		/* Cycle_cnt */
#define	SPF_PCR_SO_IC		0x0010		/* Instr_cnt */
#define	SPF_PCR_SO_DM		0x0020		/* Dispatch0_IC_miss */
#define	SPF_PCR_SO_DB		0x0030		/* Dispatch0_storeBuf */
#define	SPF_PCR_SO_IR		0x0080		/* IC_ref */
#define	SPF_PCR_SO_DR		0x0090		/* DC_rd */
#define	SPF_PCR_SO_DW		0x00A0		/* DC_wr */
#define	SPF_PCR_SO_LU		0x00B0		/* Load_use */
#define	SPF_PCR_SO_EC		0x00C0		/* EC_ref */
#define	SPF_PCR_S0_EW		0x00D0		/* EC_write_hit_RDO */
#define	SPF_PCR_S0_ES		0x00E0		/* EC_snoop_inv */
#define	SPF_PCR_S0_ER		0x00F0		/* EC_rd_hit */

#define	SPF_PCR_S1_CC		0x0000		/* Cycle_cnt */
#define	SPF_PCR_S1_IC		0x0800		/* Instr_cnt */
#define	SPF_PCR_S1_DM		0x1000		/* Dispatch0_mispred */
#define	SPF_PCR_S1_DU		0x1800		/* Dispatch0_FP_use */
#define	SPF_PCR_S1_IR		0x4000		/* IC_hit */
#define	SPF_PCR_S1_DR		0x4800		/* DC_rd_hit */
#define	SPF_PCR_S1_DW		0x5000		/* DC_wr_hit */
#define	SPF_PCR_S1_LU		0x5800		/* Load_use_RAW */
#define	SPF_PCR_S1_EC		0x6000		/* EC_hit */
#define	SPF_PCR_S1_EW		0x6800		/* EC_wb */
#define	SPF_PCR_S1_ES		0x7000		/* EC_snoop_cb */
#define	SPF_PCR_S1_EI		0x7800		/* EC_ic_hit */


/*
 * Ioctl Definitions
 */

#define	HW_GET_SPF_PCR		0x00
#define	HW_SET_SPF_PCR		0x01

#define	HW_GET_SPF_PIC0		0x02
#define	HW_GET_SPF_PIC1		0x03
#define	HW_GET_SPF_PIC01	0x04

#define	HW_SET_SPF_PIC01	0x05
#define	HW_CLR_SPF_PIC01	0x06

#define	HW_MAX_IOCTL		0x07


