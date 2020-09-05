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

/* Uncomment for migration */
/*#define AT_MIGRATION */

#ifdef AT_MIGRATION
#define MIG_STACK_SIZE 0x4000
#define MIG_STACKS_PER_CLUSTER 1000

#define BR_MIG_SUCCESS      0
#define BR_MIG_IN_PROGRESS  1
#define BR_MIG_FAILURE      2
#define BR_MIG_TERMINATION  3 
#define BR_MIG_IDLE         4

#define BR_MIG_CLUSTERS     3

/* Stuff used for statistics */
typedef struct BR_mig_stat {
  hrtime_t start;
  hrtime_t finish;
  hrtime_t total;
} BR_mig_stat_t;


void BR_mig_staks_init();
void *BR_mig_stack_get();
void BR_mig_stack_put(void *sto);
void BR_stack_mem_handler(BR_cluster_t cl, caddr_t a, size_t s, void *addr);
void BR_steal_req_h_2(BR_cluster_t src, BR_word_t mx, BR_word_t res);
void BR_steal_done_h_3(BR_cluster_t from, BR_word_t m, BR_word_t p, 
		       BR_word_t res);
void BR_push_handler(int dest, at_thread_t *lt);
void BR_push_on_completion_h(BR_cluster_t from, caddr_t addr, size_t s, 
			     void* arg);

extern volatile int BR_mig_from;
extern volatile int BR_mig_to;
extern volatile int BR_stack_stolen;
extern volatile int BR_stack_received;
extern volatile int BR_stack_pushed;
extern at_bundle_t *at_mig_bundle;

extern BR_mig_stat_t BR_push_self_stat;
/*-----------------------------------------------------------------*/
/* Public Interaface */
int BR_steal();
int BR_steal_from(BR_cluster_t from);
void BR_steal_term_loop();
void BR_steal_from_async(BR_cluster_t from);
int BR_push_to(BR_cluster_t to);
void  BR_push_self_to(BR_cluster_t to);
int BR_migrated_from();  /* Total threads migrated from current cluster*/
int BR_migrated_to();    /* Total threads migrated to calling cluster */
void BR_mig_start_time();
void BR_mig_finish_time();

/*-----------------------------------------------------------------*/

#endif
