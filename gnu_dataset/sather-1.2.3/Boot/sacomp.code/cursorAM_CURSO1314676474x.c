#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

typedef struct ELT_NI351013237_struct {/* layout for ELT_NIL{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI351013237;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct SE_CONTEXT_struct {/* layout for SE_CONTEXT */
 OB_HEADER header;
 struct FSETSIDE_EFFECT_struct *set;
 INT weight;
 struct SIG_struct *rsig;
 BOOL forks;
 BOOL has_arith_error;
 BOOL has_export;
 BOOL has_fatal_error;
 BOOL has_import;
 BOOL has_new;
 BOOL has_raise;
 BOOL has_yield_in_loc;
 BOOL is_full;
 BOOL may_block;
 BOOL unsafe;
 } *SE_CONTEXT;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SIDE_EFFECT_struct {/* layout for SIDE_EFFECT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct IDENT_struct name1;
 BOOL doeswrite;
 } *SIDE_EFFECT;

typedef struct SIG_struct {/* layout for SIG */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct dTP_struct *tp;
 struct ARRAYdTP_struct *src_tparams;
 struct ARRAYARG_struct *args;
 struct ARRAYBOOL_struct *hot;
 struct CONFIG_ROUT_struct *builtin_info;
 struct IDENT_struct name1;
 struct OPT_LOCAL_CALL_struct *opt_info;
 struct SE_CONTEXT_struct *se_context;
 struct SIG_struct *srcsig;
 STR str;
 BOOL am_created;
 BOOL got_builtin_info;
 BOOL is_attach_routin;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL needs_export;
 BOOL needs_import;
 BOOL opt_fork;
 BOOL returns_same;
 } *SIG;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

typedef struct AM_ATTR_EXPR_struct {/* layout for AM_ATTR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 BOOL secure1;
 } *AM_ATTR_EXPR;

typedef struct AM_CURSOR_struct {/* layout for AM_CURSOR */
 OB_HEADER header;
 struct dAM_struct *cur;
 struct dAM_struct *top1;
 struct A_STAC1244966098_struct *stack1;
 INT indent;
 INT loops;
 struct PROG_struct *prog;
 BOOL all_in_eval_orde;
 BOOL all_in_pre_order;
 BOOL assign_in_order;
 BOOL calls_in_eval_or;
 BOOL ignore_assert;
 BOOL ignore_next;
 BOOL ignore_post;
 BOOL ignore_pre;
 BOOL mark;
 BOOL started;
 BOOL with_side_effect;
 } *AM_CURSOR;

typedef struct AM_CUR1040670508_frame_struct {
 INT state;
 AM_CURSOR self;/* Formal argument: self */
 dAM ret_val;
 dAM n;
 } *AM_CUR1040670508_frame;

typedef struct AM_CURSOR_POS_struct {/* layout for AM_CURSOR_POS */
 OB_HEADER header;
 struct dAM_struct *stmt;
 INT branch;
 BOOL mark;
 } *AM_CURSOR_POS;

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

typedef struct AM_ITE1809135850_struct {/* layout for AM_ITER_CALL_EXPR */
 OB_HEADER header;
 struct dAM_STMT_struct *init;
 struct AM_LOOP_STMT_struct *lp;
 struct ARRAYSTR_struct *arg_list;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 STR uniq;
 BOOL init_before_loop;
 BOOL use_loop_index;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ITE1809135850;

typedef struct AM_ITE35020325_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE35020325_frame;

typedef struct AM_LOCAL_EXPR_struct {/* layout for AM_LOCAL_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL check_near;
 BOOL is_always_near;
 BOOL is_hot;
 BOOL is_volatile;
 BOOL needs_init;
 BOOL no_assign;
 BOOL not_in_frame;
 } *AM_LOCAL_EXPR;

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

typedef struct AM_ROU176159907_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU176159907_frame;

typedef struct AM_SHARED_EXPR_struct {/* layout for AM_SHARED_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_const;
 } *AM_SHARED_EXPR;

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct FLISTA1660160_frame_struct {
 INT state;
 FLISTA886630705 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val1;
 INT i;
 INT sz;
 FLISTA886630705 aget_self;
 INT aget_ind;
 AM_CURSOR_POS ret_val;
 } *FLISTA1660160_frame;

typedef struct FSETSIDE_EFFECT_struct {/* layout for FSET{SIDE_EFFECT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FSETSIDE_EFFECT;

typedef struct FSETSI2093978094_frame_struct {
 INT state;
 FSETSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val2;
 FSETSIDE_EFFECT L61;
 SIDE_EFFECT r;
 INT i;
 INT sz;
 INT L31;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSI2093978094_frame;

typedef struct PROG_struct {/* layout for PROG */
 OB_HEADER header;
 struct dBACK_END_struct *back_end;
 struct dBUILD508976674_struct *build_type_graph;
 struct dCHECK_AM_struct *check_am;
 struct dCHECK1877276428_struct *check_ifc_confor;
 struct dFIND_TYPES_struct *find_types;
 struct dGENERATE_AM_struct *generate_am;
 struct dGET_MAIN_SIG_struct *get_main_sig;
 struct dGET_OPTIONS_struct *get_options;
 struct dINLINE_struct *inliner;
 struct dOPTIMIZE_struct *optimizer;
 struct dPARSE_struct *parse;
 struct dSTAT_struct *stat1;
 struct ARRAYINT_struct *hotcounts;
 struct ARRAYINT_struct *itercounts;
 struct CHECK_GENERICS_struct *check_gen;
 struct CONFIG_TBL_struct *config;
 struct FIND_GENERICS_struct *find_generics;
 struct FLISTSFILE_ID_struct *err_list;
 struct FLISTSTR_struct *opt_debug_func;
 struct FLISTSTR_struct *platforms;
 struct FSETdTP_struct *tp_done;
 struct FSETSFILE_ID_struct *loops_seen;
 struct FSETSTR_struct *sather_files;
 INT genuine_platform;
 struct PROG_AS_TBL_struct *as_tbl;
 struct SFILE_ID_struct eloc;
 STR arch1;
 STR home;
 STR main_class;
 struct TP_GRAPH_struct *tp_graph;
 struct TP_GRAPH_ABS_DES_struct *tp_graph_abs_des;
 struct TP_TBL_struct *tp_tbl;
 BOOL all_reached;
 BOOL arith_checks;
 BOOL assert_checks;
 BOOL benchmark;
 BOOL boot;
 BOOL check_generics;
 BOOL chk_no_line_numb;
 BOOL debug_mangle;
 BOOL distributed;
 BOOL err_seen;
 BOOL fast_at1;
 BOOL fast_at2;
 BOOL fast_fork;
 BOOL find_generics_st;
 BOOL find_types_stage;
 BOOL generate_checked;
 BOOL hoist_const;
 BOOL hoist_iter_init;
 BOOL inline_iters;
 BOOL inline_routs;
 BOOL is_check_am;
 BOOL locks_on_stack;
 BOOL move_while;
 BOOL no_new_types;
 BOOL opt_debug;
 BOOL opt_verbose;
 BOOL post_checks;
 BOOL pre_checks;
 BOOL print_version;
 BOOL prolix;
 BOOL show_am;
 BOOL show_am_check;
 BOOL show_as_insert;
 BOOL show_checked_sig;
 BOOL show_generated_s;
 BOOL show_graphs;
 BOOL show_ifc;
 BOOL show_ifc_abs_cre;
 BOOL show_impl_create;
 BOOL show_include;
 BOOL show_main;
 BOOL show_parse_file;
 BOOL show_tr;
 BOOL show_types;
 BOOL temp_option_fast;
 BOOL threads;
 BOOL trace1;
 BOOL trace_generics;
 BOOL trace_tp_tables;
 BOOL verbose;
 BOOL void_checks;
 BOOL warnings;
 BOOL yields_in_locks;
 BOOL zones;
 } *PROG;

#include "tags.h"

/* Globals */

extern BOOL AM_CURSOR_debug;
extern INOUT_MODE MODES_inout_mode;
extern IN_MODE MODES_in_mode;
extern OUT_MODE MODES_out_mode;

/* Function declarations */


extern RETURNED_CONST AM_CALL_ARG (**dAM_CA967858872)(dAM_CALL_EXPR, INT);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST INT (**dAM_CA1128544926)(dAM_CALL_EXPR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);
AM_CALL_ARG AM_ITE35020325(AM_ITE35020325_frame);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AM_CURSOR_POS AM_CUR61708328(AM_CURSOR);
AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame);
AM_CURSOR_POS FLISTA860779230(FLISTA886630705);
AM_CURSOR_POS FLISTA971253650(FLISTA886630705);
BOOL AM_CUR1044001004(AM_CURSOR, AM_CURSOR, dTP, IDENT);
BOOL AM_CUR1093203002(AM_CURSOR, AM_CURSOR, AM_ROU1916046290);
BOOL AM_CUR1240489726(AM_CURSOR, AM_CURSOR, AM_LOCAL_EXPR);
BOOL AM_CUR1481630356(AM_CURSOR);
BOOL AM_CUR1587547281(AM_CURSOR, AM_CURSOR, dTP, IDENT);
BOOL AM_CUR176216201(AM_CURSOR, AM_CURSOR, dTP, IDENT);
BOOL AM_CUR1885691330(AM_CURSOR, AM_CURSOR);
BOOL AM_CUR1926902782(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR2013152412(AM_CURSOR);
BOOL AM_CUR45133320(AM_CURSOR, dAM);
BOOL AM_CUR537174465(AM_CURSOR, AM_CURSOR, AM_LOCAL_EXPR);
BOOL AM_CUR750559803(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR898853280(AM_CURSOR, AM_CURSOR, dTP, IDENT);
FLISTA886630705 FLISTA1475379054(FLISTA886630705, AM_CURSOR_POS);
INT FLISTA587993904(FLISTA886630705);
INT FSETSI2058428832(FSETSIDE_EFFECT);
INT INT_up418511718(INT_up418511718_frame);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame);
SIDE_EFFECT FSETSI937439554(FSETSIDE_EFFECT, SIDE_EFFECT);
STR SIDE_E1431483993(SIDE_EFFECT);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM AM_CUR52795524(AM_CURSOR);
void AM_CUR1754599450(AM_CURSOR);
void AM_CUR1769624888(AM_CURSOR);
void AM_CUR1869307717(AM_CURSOR);
void AM_OUT1439666796(AM_OUT, dAM);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void INT_timesb(INT_timesb_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_CURSOR AM_CUR1358945253(AM_CURSOR self, AM_CURSOR c) {
 AM_CURSOR ret_val;
 AM_CURSOR r;
 A_STAC1244966098 L11;
 A_STAC1244966098 create_self;
 A_STAC1244966098 ret_val1;
 A_STAC1244966098 res;
 FLISTA886630705 create_self1;
 FLISTA886630705 ret_val2;
 A_STAC1244966098 reverse_eltb_sel;
 AM_CURSOR_POS ret_val3;
 FLISTA886630705 L21;
 BOOL reverse_eltb_if_ = BOOL_zero;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 AM_CURSOR L3;
 OB L4;
 A_STAC1244966098 L5;
 OB L6;
 AM_CURSOR_POS L7;
 L4=ZALLOC(sizeof(struct AM_CURSOR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_CURSOR_tag;
 L3 = ((AM_CURSOR) L4);
 r = L3;
 create_self = ((A_STAC1244966098) NULL);
 L6=ZALLOC(sizeof(struct A_STAC1244966098_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=A_STAC1244966098_tag;
 L5 = ((A_STAC1244966098) L6);
 res = L5;
 create_self1 = ((FLISTA886630705) NULL);
 ret_val2 = ((FLISTA886630705) NULL);
 SATTR(res,s,ret_val2);
 ret_val1 = res;
 SATTR(r,stack1,ret_val1);
 SATTR(r,cur,ATTR(c,cur));
 SATTR(r,mark,ATTR(c,mark));
 SATTR(r,top1,ATTR(c,top1));
 SATTR(r,loops,ATTR(c,loops));
 SATTR(r,indent,ATTR(c,indent));
 AM_CURSOR_debug = AM_CURSOR_debug;
 SATTR(r,prog,ATTR(c,prog));
 SATTR(r,with_side_effect,ATTR(c,with_side_effect));
 SATTR(r,started,ATTR(c,started));
 SATTR(r,ignore_pre,ATTR(c,ignore_pre));
 SATTR(r,ignore_post,ATTR(c,ignore_post));
 SATTR(r,ignore_assert,ATTR(c,ignore_assert));
 SATTR(r,assign_in_order,ATTR(c,assign_in_order));
 SATTR(r,calls_in_eval_or,ATTR(c,calls_in_eval_or));
 SATTR(r,all_in_eval_orde,ATTR(c,all_in_eval_orde));
 SATTR(r,all_in_pre_order,ATTR(c,all_in_pre_order));
 {
  struct FLISTA1660160_frame_struct other1_0;
FLISTA1660160_frame noname1 = &other1_0;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::reverse_elt!:AM_CURSOR_POS */
  L11 = ATTR(c,stack1);
  reverse_eltb_sel = L11;
  L21 = ATTRs(reverse_eltb_sel,s,((FLISTA886630705) NULL));
  noname1->self = L21;
  noname1->state = 0;
  reverse_eltb_if_ = TRUE;
  while (1) {
   push_self = ATTR(r,stack1);
   L7 = FLISTA1660160(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   push_e = L7;
   SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CURSOR_POS AM_CUR61708328(AM_CURSOR self) {
 AM_CURSOR_POS ret_val;
 AM_CURSOR_POS s;
 dAM st;
 dAM tcur;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 INT L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 INT L11;
 INT L121_;
 size_self = ATTR(self,stack1);
 ret_val1 = FLISTA587993904(ATTR(size_self,s));
 L1 = ret_val1;
 L21_=(L1)==(0); 
 if (L21_) {
  SATTR(self,cur,((dAM) NULL));
  ret_val = ((AM_CURSOR_POS) NULL);
  return ret_val;
 }
 pop_self = ATTR(self,stack1);
 ret_val2 = FLISTA971253650(ATTR(pop_self,s));
 s = ret_val2;
 st = ATTR(s,stmt);
 L3 = ATTR(s,branch);
 L41_=(L3)<(999999); 
 if (L41_) {
  L5 = ATTR(self,indent);
  L61_=INTMINUS(L5,1); 
  SATTR(self,indent,L61_);
 }
 SATTR(self,cur,ATTR(s,stmt));
 SATTR(self,mark,ATTR(s,mark));
 if (st==NULL) {
  goto other235;
 } else
 switch (TAG(st)) {
  case AM_ASSERT_STMT_tag:
  case AM_ASSIGN_STMT_tag:
  case AM_ATTACH_STMT_tag:
  case AM_BREAK_STMT_tag:
  case AM_CASE_STMT_tag:
  case AM_COMMENT_STMT_tag:
  case AM_EXPR_STMT_tag:
  case AM_FORK_STMT_tag:
  case AM_IF_STMT_tag:
  case AM_INITIAL_STMT_tag:
  case AM_INV1167837230_tag:
  case AM_LOCK_STMT_tag:
  case AM_LOOP_STMT_tag:
  case AM_PAR_STMT_tag:
  case AM_POST_STMT_tag:
  case AM_PREFETCH_STMT_tag:
  case AM_PRE_STMT_tag:
  case AM_PROTECT_STMT_tag:
  case AM_RAISE_STMT_tag:
  case AM_RETURN_STMT_tag:
  case AM_SYNC_STMT_tag:
  case AM_TYPECASE_STMT_tag:
  case AM_UNLOCK_STMT_tag:
  case AM_WAITFOR_STMT_tag:
  case AM_WIT1996971603_tag:
  case AM_YIELD_STMT_tag:
   L7 = ATTR(s,branch);
   L81_=(L7)<(999999); 
   if (L81_) {
    tcur = ATTR(self,cur);
    if (tcur==NULL) {
     goto other236;
    } else
    switch (TAG(tcur)) {
     case AM_LOOP_STMT_tag:
      L9 = ATTR(s,branch);
      L101_=(1)<(L9); 
      if (L101_) {
       L11 = ATTR(self,loops);
       L121_=INTMINUS(L11,1); 
       SATTR(self,loops,L121_);
      } break;
     default: ;
     other236: ;
    }
   } break;
  default: ;
  other235: ;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1044001004(AM_CURSOR self, AM_CURSOR cr, dTP self_tp1, IDENT name111) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 dAM_EXPR d;
 dAM_EXPR d11;
 dAM_EXPR d21;
 SE_CONTEXT c;
 SIDE_EFFECT se;
 AM_ROU1916046290 L21;
 AM_CALL_ARG a;
 dAM_EXPR e;
 SE_CONTEXT c11;
 SIDE_EFFECT se1;
 AM_ITE1809135850 L31;
 AM_CALL_ARG a11;
 dAM_EXPR e1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val9;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 OUT create_self1;
 OUT ret_val13;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val14;
 FILE1 stdout_self5;
 FILE1 ret_val15;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val16;
 FILE1 stdout_self6;
 FILE1 ret_val17;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val18;
 FILE1 stdout_self7;
 FILE1 ret_val19;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val20;
 FILE1 stdout_self8;
 FILE1 ret_val21;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val22;
 FILE1 r9;
 AM_OUT AM_one_stmt_self1;
 dAM AM_one_stmt_a1;
 dAM_STMT n11;
 IDENT is_eq_self2 = IDENT_zero;
 IDENT is_eq_i2 = IDENT_zero;
 BOOL ret_val23 = BOOL_zero;
 OUT create_self2;
 OUT ret_val24;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val25;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val27;
 FILE1 stdout_self11;
 FILE1 ret_val28;
 FILE1 r11;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val29;
 FILE1 stdout_self12;
 FILE1 ret_val30;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val31;
 FILE1 stdout_self13;
 FILE1 ret_val32;
 FILE1 r13;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val33;
 FILE1 r14;
 AM_OUT AM_one_stmt_self2;
 dAM AM_one_stmt_a2;
 dAM_STMT n2;
 IDENT is_eq_self3 = IDENT_zero;
 IDENT is_eq_i3 = IDENT_zero;
 BOOL ret_val34 = BOOL_zero;
 OUT create_self3;
 OUT ret_val35;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val36;
 FILE1 stdout_self15;
 FILE1 ret_val37;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 OUT ret_val38;
 FILE1 stdout_self16;
 FILE1 ret_val39;
 FILE1 r16;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val40;
 FILE1 stdout_self17;
 FILE1 ret_val41;
 FILE1 r17;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val42;
 FILE1 stdout_self18;
 FILE1 ret_val43;
 FILE1 r18;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val44;
 FILE1 r19;
 AM_OUT AM_one_stmt_self3;
 dAM AM_one_stmt_a3;
 dAM_STMT n3;
 IDENT is_eq_self4 = IDENT_zero;
 IDENT is_eq_i4 = IDENT_zero;
 BOOL ret_val45 = BOOL_zero;
 OUT create_self4;
 OUT ret_val46;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val47;
 FILE1 stdout_self20;
 FILE1 ret_val48;
 FILE1 r20;
 OUT plus_self21;
 STR plus_s21;
 OUT ret_val49;
 FILE1 stdout_self21;
 FILE1 ret_val50;
 FILE1 r21;
 OUT plus_self22;
 STR plus_s22;
 OUT ret_val51;
 FILE1 stdout_self22;
 FILE1 ret_val52;
 FILE1 r22;
 OUT plus_self23;
 STR plus_s23;
 OUT ret_val53;
 FILE1 stdout_self23;
 FILE1 ret_val54;
 FILE1 r23;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val55;
 FILE1 r24;
 AM_OUT AM_one_stmt_self4;
 dAM AM_one_stmt_a4;
 dAM_STMT n4;
 SE_CONTEXT full_self;
 BOOL ret_val56 = BOOL_zero;
 OUT create_self5;
 OUT ret_val57;
 OUT plus_self25;
 STR plus_s25;
 OUT ret_val58;
 FILE1 stdout_self25;
 FILE1 ret_val59;
 FILE1 r25;
 OUT plus_self26;
 STR plus_s26;
 OUT ret_val60;
 FILE1 stdout_self26;
 FILE1 ret_val61;
 FILE1 r26;
 OUT plus_self27;
 STR plus_s27;
 FILE1 stdout_self27;
 FILE1 ret_val62;
 FILE1 r27;
 SE_CONTEXT se_attr_self;
 dTP se_attr_tp;
 IDENT se_attr_name = IDENT_zero;
 SIDE_EFFECT ret_val63;
 SIDE_EFFECT s;
 SIDE_EFFECT create_self6;
 SIDE_EFFECT ret_val64;
 OUT create_self7;
 OUT ret_val65;
 OUT plus_self28;
 STR plus_s28;
 OUT ret_val66;
 FILE1 stdout_self28;
 FILE1 ret_val67;
 FILE1 r28;
 OUT plus_self29;
 STR plus_s29;
 OUT ret_val68;
 FILE1 stdout_self29;
 FILE1 ret_val69;
 FILE1 r29;
 OUT plus_self30;
 STR plus_s30;
 OUT ret_val70;
 FILE1 stdout_self30;
 FILE1 ret_val71;
 FILE1 r30;
 OUT plus_self31;
 STR plus_s31;
 OUT ret_val72;
 FILE1 stdout_self31;
 FILE1 ret_val73;
 FILE1 r31;
 OUT plus_self32;
 STR plus_s32;
 OUT ret_val74;
 FILE1 stdout_self32;
 FILE1 ret_val75;
 FILE1 r32;
 OUT plus_self33;
 STR plus_s33;
 OUT ret_val76;
 FILE1 stdout_self33;
 FILE1 ret_val77;
 FILE1 r33;
 OUT plus_self34;
 STR plus_s34;
 FILE1 stdout_self34;
 FILE1 ret_val78;
 FILE1 r34;
 INT L41 = INT_zero;
 IDENT is_eq_self5 = IDENT_zero;
 IDENT is_eq_i5 = IDENT_zero;
 BOOL ret_val79 = BOOL_zero;
 OUT create_self8;
 OUT ret_val80;
 OUT plus_self35;
 STR plus_s35;
 OUT ret_val81;
 FILE1 stdout_self35;
 FILE1 ret_val82;
 FILE1 r35;
 OUT plus_self36;
 STR plus_s36;
 OUT ret_val83;
 FILE1 stdout_self36;
 FILE1 ret_val84;
 FILE1 r36;
 OUT plus_self37;
 STR plus_s37;
 FILE1 stdout_self37;
 FILE1 ret_val85;
 FILE1 r37;
 SE_CONTEXT full_self1;
 BOOL ret_val86 = BOOL_zero;
 OUT create_self9;
 OUT ret_val87;
 OUT plus_self38;
 STR plus_s38;
 OUT ret_val88;
 FILE1 stdout_self38;
 FILE1 ret_val89;
 FILE1 r38;
 OUT plus_self39;
 STR plus_s39;
 OUT ret_val90;
 FILE1 stdout_self39;
 FILE1 ret_val91;
 FILE1 r39;
 OUT plus_self40;
 STR plus_s40;
 FILE1 stdout_self40;
 FILE1 ret_val92;
 FILE1 r40;
 SE_CONTEXT se_attr_self1;
 dTP se_attr_tp1;
 IDENT se_attr_name1 = IDENT_zero;
 SIDE_EFFECT ret_val93;
 SIDE_EFFECT s1;
 SIDE_EFFECT create_self10;
 SIDE_EFFECT ret_val94;
 OUT create_self11;
 OUT ret_val95;
 OUT plus_self41;
 STR plus_s41;
 OUT ret_val96;
 FILE1 stdout_self41;
 FILE1 ret_val97;
 FILE1 r41;
 OUT plus_self42;
 STR plus_s42;
 OUT ret_val98;
 FILE1 stdout_self42;
 FILE1 ret_val99;
 FILE1 r42;
 OUT plus_self43;
 STR plus_s43;
 OUT ret_val100;
 FILE1 stdout_self43;
 FILE1 ret_val101;
 FILE1 r43;
 OUT plus_self44;
 STR plus_s44;
 OUT ret_val102;
 FILE1 stdout_self44;
 FILE1 ret_val103;
 FILE1 r44;
 OUT plus_self45;
 STR plus_s45;
 OUT ret_val104;
 FILE1 stdout_self45;
 FILE1 ret_val105;
 FILE1 r45;
 OUT plus_self46;
 STR plus_s46;
 OUT ret_val106;
 FILE1 stdout_self46;
 FILE1 ret_val107;
 FILE1 r46;
 OUT plus_self47;
 STR plus_s47;
 FILE1 stdout_self47;
 FILE1 ret_val108;
 FILE1 r47;
 INT L51 = INT_zero;
 IDENT is_eq_self6 = IDENT_zero;
 IDENT is_eq_i6 = IDENT_zero;
 BOOL ret_val109 = BOOL_zero;
 OUT create_self12;
 OUT ret_val110;
 OUT plus_self48;
 STR plus_s48;
 OUT ret_val1111;
 FILE1 stdout_self48;
 FILE1 ret_val112;
 FILE1 r48;
 OUT plus_self49;
 STR plus_s49;
 OUT ret_val113;
 FILE1 stdout_self49;
 FILE1 ret_val114;
 FILE1 r49;
 OUT plus_self50;
 STR plus_s50;
 FILE1 stdout_self50;
 FILE1 ret_val115;
 FILE1 r50;
 OUT create_self13;
 OUT ret_val116;
 OUT plus_self51;
 STR plus_s51;
 FILE1 stdout_self51;
 FILE1 ret_val117;
 FILE1 r51;
 OUT create_self14;
 OUT ret_val118;
 OUT plus_self52;
 STR plus_s52;
 FILE1 stdout_self52;
 FILE1 ret_val119;
 FILE1 r52;
 OUT create_self15;
 OUT ret_val120;
 OUT plus_self53;
 STR plus_s53;
 OUT ret_val121;
 FILE1 stdout_self53;
 FILE1 ret_val122;
 FILE1 r53;
 OUT plus_self54;
 STR plus_s54;
 OUT ret_val123;
 FILE1 stdout_self54;
 FILE1 ret_val124;
 FILE1 r54;
 OUT plus_self55;
 STR plus_s55;
 FILE1 stdout_self55;
 FILE1 ret_val125;
 FILE1 r55;
 dAM L6;
 BOOL L7;
 dTP L8;
 extern STR therei939610693;
 FILE1 L9;
 OB L10;
 dTP L13;
 FILE1 L14;
 OB L15;
 extern STR name34;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR namely1;
 FILE1 L24;
 OB L25;
 dAM_STMT L27;
 dAM_STMT L28;
 dAM_STMT L29;
 BOOL L30;
 dTP L32;
 extern STR therei939610693;
 FILE1 L33;
 OB L34;
 dTP L36;
 FILE1 L37;
 OB L38;
 extern STR name34;
 FILE1 L40;
 OB L42;
 FILE1 L44;
 OB L45;
 extern STR namely1;
 FILE1 L47;
 OB L48;
 dAM_STMT L50;
 dAM_STMT L52;
 dAM_STMT L53;
 BOOL L54;
 dTP L55;
 extern STR therei939610693;
 FILE1 L56;
 OB L57;
 dTP L59;
 FILE1 L60;
 OB L61;
 extern STR name34;
 FILE1 L63;
 OB L64;
 FILE1 L66;
 OB L67;
 extern STR namely1;
 FILE1 L69;
 OB L70;
 dAM_STMT L72;
 dAM_STMT L73;
 dAM_STMT L74;
 BOOL L75;
 dTP L76;
 extern STR therei939610693;
 FILE1 L77;
 OB L78;
 dTP L80;
 FILE1 L81;
 OB L82;
 extern STR name34;
 FILE1 L84;
 OB L85;
 FILE1 L87;
 OB L88;
 extern STR namely1;
 FILE1 L90;
 OB L91;
 dAM_STMT L93;
 dAM_STMT L94;
 dAM_STMT L95;
 BOOL L96;
 dTP L97;
 extern STR therei939610693;
 FILE1 L98;
 OB L99;
 dTP L101;
 FILE1 L102;
 OB L103;
 extern STR name34;
 FILE1 L105;
 OB L106;
 FILE1 L108;
 OB L109;
 extern STR namely1;
 FILE1 L111;
 OB L112;
 dAM_STMT L114;
 dAM_STMT L115;
 dAM_STMT L116;
 BOOL L117;
 BOOL L118;
 extern STR routine2;
 FILE1 L119;
 OB L120;
 FILE1 L123;
 OB L124;
 extern STR ismark127555749;
 FILE1 L126;
 OB L127;
 SIDE_EFFECT L129;
 OB L130;
 BOOL L131;
 BOOL L132;
 BOOL L1331_;
 extern STR routine2;
 FILE1 L134;
 OB L135;
 FILE1 L137;
 OB L138;
 extern STR writestotype;
 FILE1 L140;
 OB L141;
 dTP L143;
 FILE1 L144;
 OB L145;
 extern STR name34;
 FILE1 L147;
 OB L148;
 FILE1 L150;
 OB L151;
 extern STR name9;
 FILE1 L153;
 OB L154;
 INT L1561_br;
 AM_CALL_ARG aL1561_;
 AM_CALL_ARG L157;
 BOOL L158;
 dMODE L159;
 dMODE L160;
 BOOL L162;
 dTP L163;
 extern STR attris930377300;
 FILE1 L164;
 OB L165;
 FILE1 L167;
 OB L168;
 extern STR name9;
 FILE1 L170;
 OB L171;
 INT L1731_;
 BOOL L174;
 BOOL L175;
 extern STR iter2;
 FILE1 L176;
 OB L177;
 FILE1 L179;
 OB L180;
 extern STR ismark127555749;
 FILE1 L182;
 OB L183;
 SIDE_EFFECT L185;
 OB L186;
 BOOL L187;
 BOOL L188;
 BOOL L1891_;
 extern STR iter2;
 FILE1 L190;
 OB L192;
 FILE1 L194;
 OB L195;
 extern STR writestotype;
 FILE1 L197;
 OB L198;
 dTP L200;
 FILE1 L201;
 OB L202;
 extern STR name34;
 FILE1 L204;
 OB L205;
 FILE1 L207;
 OB L208;
 extern STR name9;
 FILE1 L210;
 OB L211;
 INT L2131_br;
 AM_CALL_ARG aL2131_;
 AM_CALL_ARG L214;
 BOOL L215;
 dMODE L216;
 dMODE L217;
 BOOL L218;
 dTP L219;
 extern STR attris607422243;
 FILE1 L220;
 OB L221;
 FILE1 L223;
 OB L224;
 extern STR name9;
 FILE1 L226;
 OB L227;
 INT L2291_;
 extern STR notcon1658466928;
 FILE1 L230;
 OB L232;
 extern STR notcon647555960;
 FILE1 L234;
 OB L235;
 extern STR notcon1880191200;
 FILE1 L237;
 OB L238;
 FILE1 L240;
 OB L241;
 extern STR name9;
 FILE1 L243;
 OB L244;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = cr;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L6 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L6;
   if (AM_CUR1885691330(cr, self)) {
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other239;
   } else
   switch (TAG(t)) {
    case AM_WAITFOR_STMT_tag:
     d = ATTR(((AM_WAITFOR_STMT) t),dest);
     if (d==NULL) {
      goto other240;
     } else
     switch (TAG(d)) {
      case AM_ATTR_EXPR_tag:
       L8 = ATTR(((AM_ATTR_EXPR) d),self_tp);
       if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) self_tp1))) {
        is_eq_self = ATTR(((AM_ATTR_EXPR) d),at);
        is_eq_i = name111;
        ret_val1 = (is_eq_self.str==is_eq_i.str);
        L7 = ret_val1;
       } else {
        L7 = FALSE;
       }
       if (L7) {
        if (AM_CURSOR_debug) {
         create_self = ((OUT) NULL);
         ret_val2 = create_self;
         plus_self = ret_val2;
         plus_s = ((STR) &therei939610693);
         stdout_self = ((FILE1) NULL);
         L10=ZALLOC(sizeof(struct FILE1_struct));
         if (L10==NULL) FATAL("Unable to allocate more memory");
         ((OB)L10)->header.tag=FILE1_tag;
         L9 = ((FILE1) L10);
         r = L9;
         
         SATTR(r,fp,stdout);
         ret_val4 = r;
         FILE_plus_STR(ret_val4, plus_s);
         ret_val3 = plus_self;
         plus_self1 = ret_val3;
         L13 = self_tp1;
         plus_s1 = (*dTP_strrSTR[TAG(L13)])(L13);
         stdout_self1 = ((FILE1) NULL);
         L15=ZALLOC(sizeof(struct FILE1_struct));
         if (L15==NULL) FATAL("Unable to allocate more memory");
         ((OB)L15)->header.tag=FILE1_tag;
         L14 = ((FILE1) L15);
         r1 = L14;
         
         SATTR(r1,fp,stdout);
         ret_val6 = r1;
         FILE_plus_STR(ret_val6, plus_s1);
         ret_val5 = plus_self1;
         plus_self2 = ret_val5;
         plus_s2 = ((STR) &name34);
         stdout_self2 = ((FILE1) NULL);
         L18=ZALLOC(sizeof(struct FILE1_struct));
         if (L18==NULL) FATAL("Unable to allocate more memory");
         ((OB)L18)->header.tag=FILE1_tag;
         L17 = ((FILE1) L18);
         r2 = L17;
         
         SATTR(r2,fp,stdout);
         ret_val8 = r2;
         FILE_plus_STR(ret_val8, plus_s2);
         ret_val7 = plus_self2;
         plus_self3 = ret_val7;
         plus_s3 = name111.str;
         stdout_self3 = ((FILE1) NULL);
         L22=ZALLOC(sizeof(struct FILE1_struct));
         if (L22==NULL) FATAL("Unable to allocate more memory");
         ((OB)L22)->header.tag=FILE1_tag;
         L20 = ((FILE1) L22);
         r3 = L20;
         
         SATTR(r3,fp,stdout);
         ret_val10 = r3;
         FILE_plus_STR(ret_val10, plus_s3);
         ret_val9 = plus_self3;
         plus_self4 = ret_val9;
         plus_s4 = ((STR) &namely1);
         stdout_self4 = ((FILE1) NULL);
         L25=ZALLOC(sizeof(struct FILE1_struct));
         if (L25==NULL) FATAL("Unable to allocate more memory");
         ((OB)L25)->header.tag=FILE1_tag;
         L24 = ((FILE1) L25);
         r4 = L24;
         
         SATTR(r4,fp,stdout);
         ret_val11 = r4;
         FILE_plus_STR(ret_val11, plus_s4);
         AM_one_stmt_self = ((AM_OUT) NULL);
         AM_one_stmt_a = t;
         switch (TAG(AM_one_stmt_a)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L27 = ((dAM_STMT) AM_one_stmt_a);
           n = (*dAM_ST775224435[TAG(L27)])(L27);
           L28 = ((dAM_STMT) AM_one_stmt_a);
           (*dAM_ST1372701974[TAG(L28)])(L28, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
           L29 = ((dAM_STMT) AM_one_stmt_a);
           (*dAM_ST1372701974[TAG(L29)])(L29, n); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other240: ;
     }
     d = ((dAM_EXPR) ATTR(((AM_WAITFOR_STMT) t),prefetch));
     if (d==NULL) {
      goto other241;
     } else
     switch (TAG(d)) {
      case AM_ATTR_EXPR_tag:
       L32 = ATTR(((AM_ATTR_EXPR) d),self_tp);
       if ((*dTP_is242312711[TAG(L32)])(L32, ((OB) self_tp1))) {
        is_eq_self1 = ATTR(((AM_ATTR_EXPR) d),at);
        is_eq_i1 = name111;
        ret_val12 = (is_eq_self1.str==is_eq_i1.str);
        L30 = ret_val12;
       } else {
        L30 = FALSE;
       }
       if (L30) {
        if (AM_CURSOR_debug) {
         create_self1 = ((OUT) NULL);
         ret_val13 = create_self1;
         plus_self5 = ret_val13;
         plus_s5 = ((STR) &therei939610693);
         stdout_self5 = ((FILE1) NULL);
         L34=ZALLOC(sizeof(struct FILE1_struct));
         if (L34==NULL) FATAL("Unable to allocate more memory");
         ((OB)L34)->header.tag=FILE1_tag;
         L33 = ((FILE1) L34);
         r5 = L33;
         
         SATTR(r5,fp,stdout);
         ret_val15 = r5;
         FILE_plus_STR(ret_val15, plus_s5);
         ret_val14 = plus_self5;
         plus_self6 = ret_val14;
         L36 = self_tp1;
         plus_s6 = (*dTP_strrSTR[TAG(L36)])(L36);
         stdout_self6 = ((FILE1) NULL);
         L38=ZALLOC(sizeof(struct FILE1_struct));
         if (L38==NULL) FATAL("Unable to allocate more memory");
         ((OB)L38)->header.tag=FILE1_tag;
         L37 = ((FILE1) L38);
         r6 = L37;
         
         SATTR(r6,fp,stdout);
         ret_val17 = r6;
         FILE_plus_STR(ret_val17, plus_s6);
         ret_val16 = plus_self6;
         plus_self7 = ret_val16;
         plus_s7 = ((STR) &name34);
         stdout_self7 = ((FILE1) NULL);
         L42=ZALLOC(sizeof(struct FILE1_struct));
         if (L42==NULL) FATAL("Unable to allocate more memory");
         ((OB)L42)->header.tag=FILE1_tag;
         L40 = ((FILE1) L42);
         r7 = L40;
         
         SATTR(r7,fp,stdout);
         ret_val19 = r7;
         FILE_plus_STR(ret_val19, plus_s7);
         ret_val18 = plus_self7;
         plus_self8 = ret_val18;
         plus_s8 = name111.str;
         stdout_self8 = ((FILE1) NULL);
         L45=ZALLOC(sizeof(struct FILE1_struct));
         if (L45==NULL) FATAL("Unable to allocate more memory");
         ((OB)L45)->header.tag=FILE1_tag;
         L44 = ((FILE1) L45);
         r8 = L44;
         
         SATTR(r8,fp,stdout);
         ret_val21 = r8;
         FILE_plus_STR(ret_val21, plus_s8);
         ret_val20 = plus_self8;
         plus_self9 = ret_val20;
         plus_s9 = ((STR) &namely1);
         stdout_self9 = ((FILE1) NULL);
         L48=ZALLOC(sizeof(struct FILE1_struct));
         if (L48==NULL) FATAL("Unable to allocate more memory");
         ((OB)L48)->header.tag=FILE1_tag;
         L47 = ((FILE1) L48);
         r9 = L47;
         
         SATTR(r9,fp,stdout);
         ret_val22 = r9;
         FILE_plus_STR(ret_val22, plus_s9);
         AM_one_stmt_self1 = ((AM_OUT) NULL);
         AM_one_stmt_a1 = t;
         switch (TAG(AM_one_stmt_a1)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L50 = ((dAM_STMT) AM_one_stmt_a1);
           n11 = (*dAM_ST775224435[TAG(L50)])(L50);
           L52 = ((dAM_STMT) AM_one_stmt_a1);
           (*dAM_ST1372701974[TAG(L52)])(L52, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
           L53 = ((dAM_STMT) AM_one_stmt_a1);
           (*dAM_ST1372701974[TAG(L53)])(L53, n11); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other241: ;
     } break;
    case AM_PREFETCH_STMT_tag:
     d11 = ATTR(((AM_PREFETCH_STMT) t),dest);
     if (d11==NULL) {
      goto other242;
     } else
     switch (TAG(d11)) {
      case AM_ATTR_EXPR_tag:
       L55 = ATTR(((AM_ATTR_EXPR) d11),self_tp);
       if ((*dTP_is242312711[TAG(L55)])(L55, ((OB) self_tp1))) {
        is_eq_self2 = ATTR(((AM_ATTR_EXPR) d11),at);
        is_eq_i2 = name111;
        ret_val23 = (is_eq_self2.str==is_eq_i2.str);
        L54 = ret_val23;
       } else {
        L54 = FALSE;
       }
       if (L54) {
        if (AM_CURSOR_debug) {
         create_self2 = ((OUT) NULL);
         ret_val24 = create_self2;
         plus_self10 = ret_val24;
         plus_s10 = ((STR) &therei939610693);
         stdout_self10 = ((FILE1) NULL);
         L57=ZALLOC(sizeof(struct FILE1_struct));
         if (L57==NULL) FATAL("Unable to allocate more memory");
         ((OB)L57)->header.tag=FILE1_tag;
         L56 = ((FILE1) L57);
         r10 = L56;
         
         SATTR(r10,fp,stdout);
         ret_val26 = r10;
         FILE_plus_STR(ret_val26, plus_s10);
         ret_val25 = plus_self10;
         plus_self11 = ret_val25;
         L59 = self_tp1;
         plus_s11 = (*dTP_strrSTR[TAG(L59)])(L59);
         stdout_self11 = ((FILE1) NULL);
         L61=ZALLOC(sizeof(struct FILE1_struct));
         if (L61==NULL) FATAL("Unable to allocate more memory");
         ((OB)L61)->header.tag=FILE1_tag;
         L60 = ((FILE1) L61);
         r11 = L60;
         
         SATTR(r11,fp,stdout);
         ret_val28 = r11;
         FILE_plus_STR(ret_val28, plus_s11);
         ret_val27 = plus_self11;
         plus_self12 = ret_val27;
         plus_s12 = ((STR) &name34);
         stdout_self12 = ((FILE1) NULL);
         L64=ZALLOC(sizeof(struct FILE1_struct));
         if (L64==NULL) FATAL("Unable to allocate more memory");
         ((OB)L64)->header.tag=FILE1_tag;
         L63 = ((FILE1) L64);
         r12 = L63;
         
         SATTR(r12,fp,stdout);
         ret_val30 = r12;
         FILE_plus_STR(ret_val30, plus_s12);
         ret_val29 = plus_self12;
         plus_self13 = ret_val29;
         plus_s13 = name111.str;
         stdout_self13 = ((FILE1) NULL);
         L67=ZALLOC(sizeof(struct FILE1_struct));
         if (L67==NULL) FATAL("Unable to allocate more memory");
         ((OB)L67)->header.tag=FILE1_tag;
         L66 = ((FILE1) L67);
         r13 = L66;
         
         SATTR(r13,fp,stdout);
         ret_val32 = r13;
         FILE_plus_STR(ret_val32, plus_s13);
         ret_val31 = plus_self13;
         plus_self14 = ret_val31;
         plus_s14 = ((STR) &namely1);
         stdout_self14 = ((FILE1) NULL);
         L70=ZALLOC(sizeof(struct FILE1_struct));
         if (L70==NULL) FATAL("Unable to allocate more memory");
         ((OB)L70)->header.tag=FILE1_tag;
         L69 = ((FILE1) L70);
         r14 = L69;
         
         SATTR(r14,fp,stdout);
         ret_val33 = r14;
         FILE_plus_STR(ret_val33, plus_s14);
         AM_one_stmt_self2 = ((AM_OUT) NULL);
         AM_one_stmt_a2 = t;
         switch (TAG(AM_one_stmt_a2)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L72 = ((dAM_STMT) AM_one_stmt_a2);
           n2 = (*dAM_ST775224435[TAG(L72)])(L72);
           L73 = ((dAM_STMT) AM_one_stmt_a2);
           (*dAM_ST1372701974[TAG(L73)])(L73, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
           L74 = ((dAM_STMT) AM_one_stmt_a2);
           (*dAM_ST1372701974[TAG(L74)])(L74, n2); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other242: ;
     }
     d11 = ((dAM_EXPR) ATTR(((AM_PREFETCH_STMT) t),prefetch));
     if (d11==NULL) {
      goto other243;
     } else
     switch (TAG(d11)) {
      case AM_ATTR_EXPR_tag:
       L76 = ATTR(((AM_ATTR_EXPR) d11),self_tp);
       if ((*dTP_is242312711[TAG(L76)])(L76, ((OB) self_tp1))) {
        is_eq_self3 = ATTR(((AM_ATTR_EXPR) d11),at);
        is_eq_i3 = name111;
        ret_val34 = (is_eq_self3.str==is_eq_i3.str);
        L75 = ret_val34;
       } else {
        L75 = FALSE;
       }
       if (L75) {
        if (AM_CURSOR_debug) {
         create_self3 = ((OUT) NULL);
         ret_val35 = create_self3;
         plus_self15 = ret_val35;
         plus_s15 = ((STR) &therei939610693);
         stdout_self15 = ((FILE1) NULL);
         L78=ZALLOC(sizeof(struct FILE1_struct));
         if (L78==NULL) FATAL("Unable to allocate more memory");
         ((OB)L78)->header.tag=FILE1_tag;
         L77 = ((FILE1) L78);
         r15 = L77;
         
         SATTR(r15,fp,stdout);
         ret_val37 = r15;
         FILE_plus_STR(ret_val37, plus_s15);
         ret_val36 = plus_self15;
         plus_self16 = ret_val36;
         L80 = self_tp1;
         plus_s16 = (*dTP_strrSTR[TAG(L80)])(L80);
         stdout_self16 = ((FILE1) NULL);
         L82=ZALLOC(sizeof(struct FILE1_struct));
         if (L82==NULL) FATAL("Unable to allocate more memory");
         ((OB)L82)->header.tag=FILE1_tag;
         L81 = ((FILE1) L82);
         r16 = L81;
         
         SATTR(r16,fp,stdout);
         ret_val39 = r16;
         FILE_plus_STR(ret_val39, plus_s16);
         ret_val38 = plus_self16;
         plus_self17 = ret_val38;
         plus_s17 = ((STR) &name34);
         stdout_self17 = ((FILE1) NULL);
         L85=ZALLOC(sizeof(struct FILE1_struct));
         if (L85==NULL) FATAL("Unable to allocate more memory");
         ((OB)L85)->header.tag=FILE1_tag;
         L84 = ((FILE1) L85);
         r17 = L84;
         
         SATTR(r17,fp,stdout);
         ret_val41 = r17;
         FILE_plus_STR(ret_val41, plus_s17);
         ret_val40 = plus_self17;
         plus_self18 = ret_val40;
         plus_s18 = name111.str;
         stdout_self18 = ((FILE1) NULL);
         L88=ZALLOC(sizeof(struct FILE1_struct));
         if (L88==NULL) FATAL("Unable to allocate more memory");
         ((OB)L88)->header.tag=FILE1_tag;
         L87 = ((FILE1) L88);
         r18 = L87;
         
         SATTR(r18,fp,stdout);
         ret_val43 = r18;
         FILE_plus_STR(ret_val43, plus_s18);
         ret_val42 = plus_self18;
         plus_self19 = ret_val42;
         plus_s19 = ((STR) &namely1);
         stdout_self19 = ((FILE1) NULL);
         L91=ZALLOC(sizeof(struct FILE1_struct));
         if (L91==NULL) FATAL("Unable to allocate more memory");
         ((OB)L91)->header.tag=FILE1_tag;
         L90 = ((FILE1) L91);
         r19 = L90;
         
         SATTR(r19,fp,stdout);
         ret_val44 = r19;
         FILE_plus_STR(ret_val44, plus_s19);
         AM_one_stmt_self3 = ((AM_OUT) NULL);
         AM_one_stmt_a3 = t;
         switch (TAG(AM_one_stmt_a3)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L93 = ((dAM_STMT) AM_one_stmt_a3);
           n3 = (*dAM_ST775224435[TAG(L93)])(L93);
           L94 = ((dAM_STMT) AM_one_stmt_a3);
           (*dAM_ST1372701974[TAG(L94)])(L94, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
           L95 = ((dAM_STMT) AM_one_stmt_a3);
           (*dAM_ST1372701974[TAG(L95)])(L95, n3); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other243: ;
     } break;
    case AM_ASSIGN_STMT_tag:
     d21 = ATTR(((AM_ASSIGN_STMT) t),dest);
     if (d21==NULL) {
      goto other244;
     } else
     switch (TAG(d21)) {
      case AM_ATTR_EXPR_tag:
       L97 = ATTR(((AM_ATTR_EXPR) d21),self_tp);
       if ((*dTP_is242312711[TAG(L97)])(L97, ((OB) self_tp1))) {
        is_eq_self4 = ATTR(((AM_ATTR_EXPR) d21),at);
        is_eq_i4 = name111;
        ret_val45 = (is_eq_self4.str==is_eq_i4.str);
        L96 = ret_val45;
       } else {
        L96 = FALSE;
       }
       if (L96) {
        if (AM_CURSOR_debug) {
         create_self4 = ((OUT) NULL);
         ret_val46 = create_self4;
         plus_self20 = ret_val46;
         plus_s20 = ((STR) &therei939610693);
         stdout_self20 = ((FILE1) NULL);
         L99=ZALLOC(sizeof(struct FILE1_struct));
         if (L99==NULL) FATAL("Unable to allocate more memory");
         ((OB)L99)->header.tag=FILE1_tag;
         L98 = ((FILE1) L99);
         r20 = L98;
         
         SATTR(r20,fp,stdout);
         ret_val48 = r20;
         FILE_plus_STR(ret_val48, plus_s20);
         ret_val47 = plus_self20;
         plus_self21 = ret_val47;
         L101 = self_tp1;
         plus_s21 = (*dTP_strrSTR[TAG(L101)])(L101);
         stdout_self21 = ((FILE1) NULL);
         L103=ZALLOC(sizeof(struct FILE1_struct));
         if (L103==NULL) FATAL("Unable to allocate more memory");
         ((OB)L103)->header.tag=FILE1_tag;
         L102 = ((FILE1) L103);
         r21 = L102;
         
         SATTR(r21,fp,stdout);
         ret_val50 = r21;
         FILE_plus_STR(ret_val50, plus_s21);
         ret_val49 = plus_self21;
         plus_self22 = ret_val49;
         plus_s22 = ((STR) &name34);
         stdout_self22 = ((FILE1) NULL);
         L106=ZALLOC(sizeof(struct FILE1_struct));
         if (L106==NULL) FATAL("Unable to allocate more memory");
         ((OB)L106)->header.tag=FILE1_tag;
         L105 = ((FILE1) L106);
         r22 = L105;
         
         SATTR(r22,fp,stdout);
         ret_val52 = r22;
         FILE_plus_STR(ret_val52, plus_s22);
         ret_val51 = plus_self22;
         plus_self23 = ret_val51;
         plus_s23 = name111.str;
         stdout_self23 = ((FILE1) NULL);
         L109=ZALLOC(sizeof(struct FILE1_struct));
         if (L109==NULL) FATAL("Unable to allocate more memory");
         ((OB)L109)->header.tag=FILE1_tag;
         L108 = ((FILE1) L109);
         r23 = L108;
         
         SATTR(r23,fp,stdout);
         ret_val54 = r23;
         FILE_plus_STR(ret_val54, plus_s23);
         ret_val53 = plus_self23;
         plus_self24 = ret_val53;
         plus_s24 = ((STR) &namely1);
         stdout_self24 = ((FILE1) NULL);
         L112=ZALLOC(sizeof(struct FILE1_struct));
         if (L112==NULL) FATAL("Unable to allocate more memory");
         ((OB)L112)->header.tag=FILE1_tag;
         L111 = ((FILE1) L112);
         r24 = L111;
         
         SATTR(r24,fp,stdout);
         ret_val55 = r24;
         FILE_plus_STR(ret_val55, plus_s24);
         AM_one_stmt_self4 = ((AM_OUT) NULL);
         AM_one_stmt_a4 = t;
         switch (TAG(AM_one_stmt_a4)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L114 = ((dAM_STMT) AM_one_stmt_a4);
           n4 = (*dAM_ST775224435[TAG(L114)])(L114);
           L115 = ((dAM_STMT) AM_one_stmt_a4);
           (*dAM_ST1372701974[TAG(L115)])(L115, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self4, AM_one_stmt_a4);
           L116 = ((dAM_STMT) AM_one_stmt_a4);
           (*dAM_ST1372701974[TAG(L116)])(L116, n4); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self4, AM_one_stmt_a4);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other244: ;
     } break;
    case AM_ROU1916046290_tag:
     if (ATTR(self,with_side_effect)) {
      c = SIG_ge642497605(ATTR(((AM_ROU1916046290) t),fun), ATTR(self,prog));
      full_self = c;
      if (ATTR(full_self,is_full)) {
       L118 = TRUE;
      } else {
       L118 = ATTR(full_self,has_import);
      }
      if (L118) {
       L117 = TRUE;
      } else {
       L117 = ATTR(full_self,has_export);
      }
      ret_val56 = L117;
      if (ret_val56) {
       if (AM_CURSOR_debug) {
        create_self5 = ((OUT) NULL);
        ret_val57 = create_self5;
        plus_self25 = ret_val57;
        plus_s25 = ((STR) &routine2);
        stdout_self25 = ((FILE1) NULL);
        L120=ZALLOC(sizeof(struct FILE1_struct));
        if (L120==NULL) FATAL("Unable to allocate more memory");
        ((OB)L120)->header.tag=FILE1_tag;
        L119 = ((FILE1) L120);
        r25 = L119;
        
        SATTR(r25,fp,stdout);
        ret_val59 = r25;
        FILE_plus_STR(ret_val59, plus_s25);
        ret_val58 = plus_self25;
        plus_self26 = ret_val58;
        plus_s26 = ATTR(ATTR(((AM_ROU1916046290) t),fun),str);
        stdout_self26 = ((FILE1) NULL);
        L124=ZALLOC(sizeof(struct FILE1_struct));
        if (L124==NULL) FATAL("Unable to allocate more memory");
        ((OB)L124)->header.tag=FILE1_tag;
        L123 = ((FILE1) L124);
        r26 = L123;
        
        SATTR(r26,fp,stdout);
        ret_val61 = r26;
        FILE_plus_STR(ret_val61, plus_s26);
        ret_val60 = plus_self26;
        plus_self27 = ret_val60;
        plus_s27 = ((STR) &ismark127555749);
        stdout_self27 = ((FILE1) NULL);
        L127=ZALLOC(sizeof(struct FILE1_struct));
        if (L127==NULL) FATAL("Unable to allocate more memory");
        ((OB)L127)->header.tag=FILE1_tag;
        L126 = ((FILE1) L127);
        r27 = L126;
        
        SATTR(r27,fp,stdout);
        ret_val62 = r27;
        FILE_plus_STR(ret_val62, plus_s27);
       }
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self = c;
      se_attr_tp = self_tp1;
      se_attr_name = name111;
      create_self6 = ((SIDE_EFFECT) NULL);
      L130=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L130==NULL) FATAL("Unable to allocate more memory");
      ((OB)L130)->header.tag=SIDE_EFFECT_tag;
      L129 = ((SIDE_EFFECT) L130);
      ret_val64 = L129;
      s = ret_val64;
      SATTR(s,tp,se_attr_tp);
      SATTR(s,name1,se_attr_name);
      ret_val63 = FSETSI937439554(ATTR(se_attr_self,set), s);
      se = ret_val63;
      L132 = (se==((SIDE_EFFECT) NULL));
      L1331_=!(L132); 
      if (L1331_) {
       L131 = ATTR(se,doeswrite);
      } else {
       L131 = FALSE;
      }
      if (L131) {
       if (AM_CURSOR_debug) {
        create_self7 = ((OUT) NULL);
        ret_val65 = create_self7;
        plus_self28 = ret_val65;
        plus_s28 = ((STR) &routine2);
        stdout_self28 = ((FILE1) NULL);
        L135=ZALLOC(sizeof(struct FILE1_struct));
        if (L135==NULL) FATAL("Unable to allocate more memory");
        ((OB)L135)->header.tag=FILE1_tag;
        L134 = ((FILE1) L135);
        r28 = L134;
        
        SATTR(r28,fp,stdout);
        ret_val67 = r28;
        FILE_plus_STR(ret_val67, plus_s28);
        ret_val66 = plus_self28;
        plus_self29 = ret_val66;
        plus_s29 = ATTR(ATTR(((AM_ROU1916046290) t),fun),str);
        stdout_self29 = ((FILE1) NULL);
        L138=ZALLOC(sizeof(struct FILE1_struct));
        if (L138==NULL) FATAL("Unable to allocate more memory");
        ((OB)L138)->header.tag=FILE1_tag;
        L137 = ((FILE1) L138);
        r29 = L137;
        
        SATTR(r29,fp,stdout);
        ret_val69 = r29;
        FILE_plus_STR(ret_val69, plus_s29);
        ret_val68 = plus_self29;
        plus_self30 = ret_val68;
        plus_s30 = ((STR) &writestotype);
        stdout_self30 = ((FILE1) NULL);
        L141=ZALLOC(sizeof(struct FILE1_struct));
        if (L141==NULL) FATAL("Unable to allocate more memory");
        ((OB)L141)->header.tag=FILE1_tag;
        L140 = ((FILE1) L141);
        r30 = L140;
        
        SATTR(r30,fp,stdout);
        ret_val71 = r30;
        FILE_plus_STR(ret_val71, plus_s30);
        ret_val70 = plus_self30;
        plus_self31 = ret_val70;
        L143 = self_tp1;
        plus_s31 = (*dTP_strrSTR[TAG(L143)])(L143);
        stdout_self31 = ((FILE1) NULL);
        L145=ZALLOC(sizeof(struct FILE1_struct));
        if (L145==NULL) FATAL("Unable to allocate more memory");
        ((OB)L145)->header.tag=FILE1_tag;
        L144 = ((FILE1) L145);
        r31 = L144;
        
        SATTR(r31,fp,stdout);
        ret_val73 = r31;
        FILE_plus_STR(ret_val73, plus_s31);
        ret_val72 = plus_self31;
        plus_self32 = ret_val72;
        plus_s32 = ((STR) &name34);
        stdout_self32 = ((FILE1) NULL);
        L148=ZALLOC(sizeof(struct FILE1_struct));
        if (L148==NULL) FATAL("Unable to allocate more memory");
        ((OB)L148)->header.tag=FILE1_tag;
        L147 = ((FILE1) L148);
        r32 = L147;
        
        SATTR(r32,fp,stdout);
        ret_val75 = r32;
        FILE_plus_STR(ret_val75, plus_s32);
        ret_val74 = plus_self32;
        plus_self33 = ret_val74;
        plus_s33 = name111.str;
        stdout_self33 = ((FILE1) NULL);
        L151=ZALLOC(sizeof(struct FILE1_struct));
        if (L151==NULL) FATAL("Unable to allocate more memory");
        ((OB)L151)->header.tag=FILE1_tag;
        L150 = ((FILE1) L151);
        r33 = L150;
        
        SATTR(r33,fp,stdout);
        ret_val77 = r33;
        FILE_plus_STR(ret_val77, plus_s33);
        ret_val76 = plus_self33;
        plus_self34 = ret_val76;
        plus_s34 = ((STR) &name9);
        stdout_self34 = ((FILE1) NULL);
        L154=ZALLOC(sizeof(struct FILE1_struct));
        if (L154==NULL) FATAL("Unable to allocate more memory");
        ((OB)L154)->header.tag=FILE1_tag;
        L153 = ((FILE1) L154);
        r34 = L153;
        
        SATTR(r34,fp,stdout);
        ret_val78 = r34;
        FILE_plus_STR(ret_val78, plus_s34);
       }
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     }
     {
      BOOL f_L1561_ = TRUE;
      /* loop index variable */
      L41 = 0;
      L21 = ((AM_ROU1916046290) t);
      L1561_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
      while (1) {
       if(L41>=L1561_br)  goto after_loop1; 
       aL1561_=ARR((AM_ROU1916046290)L21,L41); 
       a = aL1561_;
       L159 = ATTR(a,mode1);
       if ((*dMODE_814247358[TAG(L159)])(L159, ((dMODE) MODES_inout_mode))) {
        L158 = TRUE;
       } else {
        L160 = ATTR(a,mode1);
        L158 = (*dMODE_814247358[TAG(L160)])(L160, ((dMODE) MODES_out_mode));
       }
       if (L158) {
        e = ATTR(a,expr);
        if (e==NULL) {
         goto other245;
        } else
        switch (TAG(e)) {
         case AM_ATTR_EXPR_tag:
          L163 = ATTR(((AM_ATTR_EXPR) e),self_tp);
          if ((*dTP_is242312711[TAG(L163)])(L163, ((OB) self_tp1))) {
           is_eq_self5 = ATTR(((AM_ATTR_EXPR) e),at);
           is_eq_i5 = name111;
           ret_val79 = (is_eq_self5.str==is_eq_i5.str);
           L162 = ret_val79;
          } else {
           L162 = FALSE;
          }
          if (L162) {
           if (AM_CURSOR_debug) {
            create_self8 = ((OUT) NULL);
            ret_val80 = create_self8;
            plus_self35 = ret_val80;
            plus_s35 = ((STR) &attris930377300);
            stdout_self35 = ((FILE1) NULL);
            L165=ZALLOC(sizeof(struct FILE1_struct));
            if (L165==NULL) FATAL("Unable to allocate more memory");
            ((OB)L165)->header.tag=FILE1_tag;
            L164 = ((FILE1) L165);
            r35 = L164;
            
            SATTR(r35,fp,stdout);
            ret_val82 = r35;
            FILE_plus_STR(ret_val82, plus_s35);
            ret_val81 = plus_self35;
            plus_self36 = ret_val81;
            plus_s36 = ATTR(ATTR(((AM_ROU1916046290) t),fun),str);
            stdout_self36 = ((FILE1) NULL);
            L168=ZALLOC(sizeof(struct FILE1_struct));
            if (L168==NULL) FATAL("Unable to allocate more memory");
            ((OB)L168)->header.tag=FILE1_tag;
            L167 = ((FILE1) L168);
            r36 = L167;
            
            SATTR(r36,fp,stdout);
            ret_val84 = r36;
            FILE_plus_STR(ret_val84, plus_s36);
            ret_val83 = plus_self36;
            plus_self37 = ret_val83;
            plus_s37 = ((STR) &name9);
            stdout_self37 = ((FILE1) NULL);
            L171=ZALLOC(sizeof(struct FILE1_struct));
            if (L171==NULL) FATAL("Unable to allocate more memory");
            ((OB)L171)->header.tag=FILE1_tag;
            L170 = ((FILE1) L171);
            r37 = L170;
            
            SATTR(r37,fp,stdout);
            ret_val85 = r37;
            FILE_plus_STR(ret_val85, plus_s37);
           }
           ret_val = FALSE;
           return ret_val;
          } break;
         default: ;
         other245: ;
        }
       }
       L1731_=INTPLUS(L41,1); 
       L41 = L1731_;
      }
     }
     after_loop1: ; break;
    case AM_ITE1809135850_tag:
     if (ATTR(self,with_side_effect)) {
      c11 = SIG_ge642497605(ATTR(((AM_ITE1809135850) t),fun), ATTR(self,prog));
      full_self1 = c11;
      if (ATTR(full_self1,is_full)) {
       L175 = TRUE;
      } else {
       L175 = ATTR(full_self1,has_import);
      }
      if (L175) {
       L174 = TRUE;
      } else {
       L174 = ATTR(full_self1,has_export);
      }
      ret_val86 = L174;
      if (ret_val86) {
       if (AM_CURSOR_debug) {
        create_self9 = ((OUT) NULL);
        ret_val87 = create_self9;
        plus_self38 = ret_val87;
        plus_s38 = ((STR) &iter2);
        stdout_self38 = ((FILE1) NULL);
        L177=ZALLOC(sizeof(struct FILE1_struct));
        if (L177==NULL) FATAL("Unable to allocate more memory");
        ((OB)L177)->header.tag=FILE1_tag;
        L176 = ((FILE1) L177);
        r38 = L176;
        
        SATTR(r38,fp,stdout);
        ret_val89 = r38;
        FILE_plus_STR(ret_val89, plus_s38);
        ret_val88 = plus_self38;
        plus_self39 = ret_val88;
        plus_s39 = ATTR(ATTR(((AM_ITE1809135850) t),fun),str);
        stdout_self39 = ((FILE1) NULL);
        L180=ZALLOC(sizeof(struct FILE1_struct));
        if (L180==NULL) FATAL("Unable to allocate more memory");
        ((OB)L180)->header.tag=FILE1_tag;
        L179 = ((FILE1) L180);
        r39 = L179;
        
        SATTR(r39,fp,stdout);
        ret_val91 = r39;
        FILE_plus_STR(ret_val91, plus_s39);
        ret_val90 = plus_self39;
        plus_self40 = ret_val90;
        plus_s40 = ((STR) &ismark127555749);
        stdout_self40 = ((FILE1) NULL);
        L183=ZALLOC(sizeof(struct FILE1_struct));
        if (L183==NULL) FATAL("Unable to allocate more memory");
        ((OB)L183)->header.tag=FILE1_tag;
        L182 = ((FILE1) L183);
        r40 = L182;
        
        SATTR(r40,fp,stdout);
        ret_val92 = r40;
        FILE_plus_STR(ret_val92, plus_s40);
       }
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self1 = c11;
      se_attr_tp1 = self_tp1;
      se_attr_name1 = name111;
      create_self10 = ((SIDE_EFFECT) NULL);
      L186=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L186==NULL) FATAL("Unable to allocate more memory");
      ((OB)L186)->header.tag=SIDE_EFFECT_tag;
      L185 = ((SIDE_EFFECT) L186);
      ret_val94 = L185;
      s1 = ret_val94;
      SATTR(s1,tp,se_attr_tp1);
      SATTR(s1,name1,se_attr_name1);
      ret_val93 = FSETSI937439554(ATTR(se_attr_self1,set), s1);
      se1 = ret_val93;
      L188 = (se1==((SIDE_EFFECT) NULL));
      L1891_=!(L188); 
      if (L1891_) {
       L187 = ATTR(se1,doeswrite);
      } else {
       L187 = FALSE;
      }
      if (L187) {
       if (AM_CURSOR_debug) {
        create_self11 = ((OUT) NULL);
        ret_val95 = create_self11;
        plus_self41 = ret_val95;
        plus_s41 = ((STR) &iter2);
        stdout_self41 = ((FILE1) NULL);
        L192=ZALLOC(sizeof(struct FILE1_struct));
        if (L192==NULL) FATAL("Unable to allocate more memory");
        ((OB)L192)->header.tag=FILE1_tag;
        L190 = ((FILE1) L192);
        r41 = L190;
        
        SATTR(r41,fp,stdout);
        ret_val97 = r41;
        FILE_plus_STR(ret_val97, plus_s41);
        ret_val96 = plus_self41;
        plus_self42 = ret_val96;
        plus_s42 = ATTR(ATTR(((AM_ITE1809135850) t),fun),str);
        stdout_self42 = ((FILE1) NULL);
        L195=ZALLOC(sizeof(struct FILE1_struct));
        if (L195==NULL) FATAL("Unable to allocate more memory");
        ((OB)L195)->header.tag=FILE1_tag;
        L194 = ((FILE1) L195);
        r42 = L194;
        
        SATTR(r42,fp,stdout);
        ret_val99 = r42;
        FILE_plus_STR(ret_val99, plus_s42);
        ret_val98 = plus_self42;
        plus_self43 = ret_val98;
        plus_s43 = ((STR) &writestotype);
        stdout_self43 = ((FILE1) NULL);
        L198=ZALLOC(sizeof(struct FILE1_struct));
        if (L198==NULL) FATAL("Unable to allocate more memory");
        ((OB)L198)->header.tag=FILE1_tag;
        L197 = ((FILE1) L198);
        r43 = L197;
        
        SATTR(r43,fp,stdout);
        ret_val101 = r43;
        FILE_plus_STR(ret_val101, plus_s43);
        ret_val100 = plus_self43;
        plus_self44 = ret_val100;
        L200 = self_tp1;
        plus_s44 = (*dTP_strrSTR[TAG(L200)])(L200);
        stdout_self44 = ((FILE1) NULL);
        L202=ZALLOC(sizeof(struct FILE1_struct));
        if (L202==NULL) FATAL("Unable to allocate more memory");
        ((OB)L202)->header.tag=FILE1_tag;
        L201 = ((FILE1) L202);
        r44 = L201;
        
        SATTR(r44,fp,stdout);
        ret_val103 = r44;
        FILE_plus_STR(ret_val103, plus_s44);
        ret_val102 = plus_self44;
        plus_self45 = ret_val102;
        plus_s45 = ((STR) &name34);
        stdout_self45 = ((FILE1) NULL);
        L205=ZALLOC(sizeof(struct FILE1_struct));
        if (L205==NULL) FATAL("Unable to allocate more memory");
        ((OB)L205)->header.tag=FILE1_tag;
        L204 = ((FILE1) L205);
        r45 = L204;
        
        SATTR(r45,fp,stdout);
        ret_val105 = r45;
        FILE_plus_STR(ret_val105, plus_s45);
        ret_val104 = plus_self45;
        plus_self46 = ret_val104;
        plus_s46 = name111.str;
        stdout_self46 = ((FILE1) NULL);
        L208=ZALLOC(sizeof(struct FILE1_struct));
        if (L208==NULL) FATAL("Unable to allocate more memory");
        ((OB)L208)->header.tag=FILE1_tag;
        L207 = ((FILE1) L208);
        r46 = L207;
        
        SATTR(r46,fp,stdout);
        ret_val107 = r46;
        FILE_plus_STR(ret_val107, plus_s46);
        ret_val106 = plus_self46;
        plus_self47 = ret_val106;
        plus_s47 = ((STR) &name9);
        stdout_self47 = ((FILE1) NULL);
        L211=ZALLOC(sizeof(struct FILE1_struct));
        if (L211==NULL) FATAL("Unable to allocate more memory");
        ((OB)L211)->header.tag=FILE1_tag;
        L210 = ((FILE1) L211);
        r47 = L210;
        
        SATTR(r47,fp,stdout);
        ret_val108 = r47;
        FILE_plus_STR(ret_val108, plus_s47);
       }
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     }
     {
      BOOL f_L2131_ = TRUE;
      /* loop index variable */
      L51 = 0;
      L31 = ((AM_ITE1809135850) t);
      L2131_br=L31==NULL?0:ASIZE((AM_ITE1809135850)L31); 
      while (1) {
       if(L51>=L2131_br)  goto after_loop2; 
       aL2131_=ARR((AM_ITE1809135850)L31,L51); 
       a11 = aL2131_;
       L216 = ATTR(a11,mode1);
       if ((*dMODE_814247358[TAG(L216)])(L216, ((dMODE) MODES_inout_mode))) {
        L215 = TRUE;
       } else {
        L217 = ATTR(a11,mode1);
        L215 = (*dMODE_814247358[TAG(L217)])(L217, ((dMODE) MODES_out_mode));
       }
       if (L215) {
        e1 = ATTR(a11,expr);
        if (e1==NULL) {
         goto other246;
        } else
        switch (TAG(e1)) {
         case AM_ATTR_EXPR_tag:
          L219 = ATTR(((AM_ATTR_EXPR) e1),self_tp);
          if ((*dTP_is242312711[TAG(L219)])(L219, ((OB) self_tp1))) {
           is_eq_self6 = ATTR(((AM_ATTR_EXPR) e1),at);
           is_eq_i6 = name111;
           ret_val109 = (is_eq_self6.str==is_eq_i6.str);
           L218 = ret_val109;
          } else {
           L218 = FALSE;
          }
          if (L218) {
           if (AM_CURSOR_debug) {
            create_self12 = ((OUT) NULL);
            ret_val110 = create_self12;
            plus_self48 = ret_val110;
            plus_s48 = ((STR) &attris607422243);
            stdout_self48 = ((FILE1) NULL);
            L221=ZALLOC(sizeof(struct FILE1_struct));
            if (L221==NULL) FATAL("Unable to allocate more memory");
            ((OB)L221)->header.tag=FILE1_tag;
            L220 = ((FILE1) L221);
            r48 = L220;
            
            SATTR(r48,fp,stdout);
            ret_val112 = r48;
            FILE_plus_STR(ret_val112, plus_s48);
            ret_val1111 = plus_self48;
            plus_self49 = ret_val1111;
            plus_s49 = ATTR(ATTR(((AM_ITE1809135850) t),fun),str);
            stdout_self49 = ((FILE1) NULL);
            L224=ZALLOC(sizeof(struct FILE1_struct));
            if (L224==NULL) FATAL("Unable to allocate more memory");
            ((OB)L224)->header.tag=FILE1_tag;
            L223 = ((FILE1) L224);
            r49 = L223;
            
            SATTR(r49,fp,stdout);
            ret_val114 = r49;
            FILE_plus_STR(ret_val114, plus_s49);
            ret_val113 = plus_self49;
            plus_self50 = ret_val113;
            plus_s50 = ((STR) &name9);
            stdout_self50 = ((FILE1) NULL);
            L227=ZALLOC(sizeof(struct FILE1_struct));
            if (L227==NULL) FATAL("Unable to allocate more memory");
            ((OB)L227)->header.tag=FILE1_tag;
            L226 = ((FILE1) L227);
            r50 = L226;
            
            SATTR(r50,fp,stdout);
            ret_val115 = r50;
            FILE_plus_STR(ret_val115, plus_s50);
           }
           ret_val = FALSE;
           return ret_val;
          } break;
         default: ;
         other246: ;
        }
       }
       L2291_=INTPLUS(L51,1); 
       L51 = L2291_;
      }
     }
     after_loop2: ; break;
    case AM_BND260301329_tag:
     if (AM_CURSOR_debug) {
      create_self13 = ((OUT) NULL);
      ret_val116 = create_self13;
      plus_self51 = ret_val116;
      plus_s51 = ((STR) &notcon1658466928);
      stdout_self51 = ((FILE1) NULL);
      L232=ZALLOC(sizeof(struct FILE1_struct));
      if (L232==NULL) FATAL("Unable to allocate more memory");
      ((OB)L232)->header.tag=FILE1_tag;
      L230 = ((FILE1) L232);
      r51 = L230;
      
      SATTR(r51,fp,stdout);
      ret_val117 = r51;
      FILE_plus_STR(ret_val117, plus_s51);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_BND367211769_tag:
     if (AM_CURSOR_debug) {
      create_self14 = ((OUT) NULL);
      ret_val118 = create_self14;
      plus_self52 = ret_val118;
      plus_s52 = ((STR) &notcon647555960);
      stdout_self52 = ((FILE1) NULL);
      L235=ZALLOC(sizeof(struct FILE1_struct));
      if (L235==NULL) FATAL("Unable to allocate more memory");
      ((OB)L235)->header.tag=FILE1_tag;
      L234 = ((FILE1) L235);
      r52 = L234;
      
      SATTR(r52,fp,stdout);
      ret_val119 = r52;
      FILE_plus_STR(ret_val119, plus_s52);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_EXT_CALL_EXPR_tag:
     if (AM_CURSOR_debug) {
      create_self15 = ((OUT) NULL);
      ret_val120 = create_self15;
      plus_self53 = ret_val120;
      plus_s53 = ((STR) &notcon1880191200);
      stdout_self53 = ((FILE1) NULL);
      L238=ZALLOC(sizeof(struct FILE1_struct));
      if (L238==NULL) FATAL("Unable to allocate more memory");
      ((OB)L238)->header.tag=FILE1_tag;
      L237 = ((FILE1) L238);
      r53 = L237;
      
      SATTR(r53,fp,stdout);
      ret_val122 = r53;
      FILE_plus_STR(ret_val122, plus_s53);
      ret_val121 = plus_self53;
      plus_self54 = ret_val121;
      plus_s54 = ATTR(ATTR(((AM_EXT_CALL_EXPR) t),fun),str);
      stdout_self54 = ((FILE1) NULL);
      L241=ZALLOC(sizeof(struct FILE1_struct));
      if (L241==NULL) FATAL("Unable to allocate more memory");
      ((OB)L241)->header.tag=FILE1_tag;
      L240 = ((FILE1) L241);
      r54 = L240;
      
      SATTR(r54,fp,stdout);
      ret_val124 = r54;
      FILE_plus_STR(ret_val124, plus_s54);
      ret_val123 = plus_self54;
      plus_self55 = ret_val123;
      plus_s55 = ((STR) &name9);
      stdout_self55 = ((FILE1) NULL);
      L244=ZALLOC(sizeof(struct FILE1_struct));
      if (L244==NULL) FATAL("Unable to allocate more memory");
      ((OB)L244)->header.tag=FILE1_tag;
      L243 = ((FILE1) L244);
      r55 = L243;
      
      SATTR(r55,fp,stdout);
      ret_val125 = r55;
      FILE_plus_STR(ret_val125, plus_s55);
     }
     ret_val = FALSE;
     return ret_val; break;
    default: ;
    other239: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1093203002(AM_CURSOR self, AM_CURSOR c11, AM_ROU1916046290 am) {
 BOOL ret_val = BOOL_zero;
 SE_CONTEXT c;
 SE_CONTEXT L11;
 SIDE_EFFECT e;
 AM_ROU1916046290 L21;
 AM_ROU1916046290 L31;
 SE_CONTEXT full_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 SE_CONTEXT eltb_self;
 SIDE_EFFECT ret_val8;
 FSETSIDE_EFFECT L41;
 BOOL eltb_if_first = BOOL_zero;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val10;
 FILE1 stdout_self3;
 FILE1 ret_val11;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val12;
 FILE1 stdout_self4;
 FILE1 ret_val13;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val14;
 FILE1 r5;
 OUT create_self2;
 OUT ret_val15;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val16;
 FILE1 r6;
 OUT create_self3;
 OUT ret_val17;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val18;
 FILE1 r7;
 OUT create_self4;
 OUT ret_val19;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val20;
 FILE1 r8;
 OUT create_self5;
 OUT ret_val21;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val22;
 FILE1 r9;
 OUT create_self6;
 OUT ret_val23;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val24;
 FILE1 r10;
 OUT create_self7;
 OUT ret_val25;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val26;
 FILE1 stdout_self11;
 FILE1 ret_val27;
 FILE1 r11;
 OUT plus_self12;
 dSTR plus_s12;
 OUT ret_val28;
 FILE1 stdout_self12;
 FILE1 ret_val29;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val30;
 FILE1 r13;
 INT L51 = INT_zero;
 OUT create_self8;
 OUT ret_val31;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val32;
 FILE1 stdout_self14;
 FILE1 ret_val33;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val34;
 FILE1 stdout_self15;
 FILE1 ret_val35;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val36;
 FILE1 r16;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 extern STR foundf51920346;
 FILE1 L10;
 OB L12;
 FILE1 L14;
 OB L15;
 extern STR name9;
 FILE1 L17;
 OB L18;
 SIDE_EFFECT L20;
 BOOL L22;
 BOOL L23;
 extern STR routine2;
 FILE1 L24;
 OB L25;
 FILE1 L27;
 OB L28;
 extern STR attrtp;
 FILE1 L30;
 OB L32;
 extern STR void1;
 FILE1 L34;
 OB L35;
 dTP L37;
 FILE1 L38;
 OB L39;
 extern STR name59;
 FILE1 L42;
 OB L43;
 extern STR void1;
 FILE1 L45;
 OB L46;
 FILE1 L48;
 OB L49;
 extern STR doeswrite;
 FILE1 L52;
 OB L53;
 dSTR L55;
 OB L56;
 FILE1 L57;
 OB L58;
 extern STR name9;
 FILE1 L60;
 OB L61;
 AM_CURSOR L63;
 BOOL L64;
 BOOL L651_;
 INT L661_br;
 AM_CALL_ARG aL661_;
 INT L671_br;
INT i_L671_=0;
 AM_CALL_ARG aL671_;
 AM_CALL_ARG L68;
 AM_CALL_ARG L69;
 dMODE L70;
 BOOL L71;
 BOOL L721_;
 extern STR founda280474472;
 FILE1 L73;
 OB L74;
 FILE1 L76;
 OB L77;
 extern STR name9;
 FILE1 L79;
 OB L80;
 AM_CURSOR L82;
 AM_CALL_ARG L83;
 BOOL L84;
 BOOL L851_;
 INT L861_;
 if (ATTR(self,with_side_effect)) {
  c = SIG_ge642497605(ATTR(am,fun), ATTR(self,prog));
  full_self = c;
  if (ATTR(full_self,is_full)) {
   L9 = TRUE;
  } else {
   L9 = ATTR(full_self,has_import);
  }
  if (L9) {
   L8 = TRUE;
  } else {
   L8 = ATTR(full_self,has_export);
  }
  ret_val1 = L8;
  if (ret_val1) {
   L7 = TRUE;
  } else {
   L7 = ATTR(c,unsafe);
  }
  if (L7) {
   L6 = TRUE;
  } else {
   L6 = ATTR(c,has_raise);
  }
  if (L6) {
   if (AM_CURSOR_debug) {
    create_self = ((OUT) NULL);
    ret_val2 = create_self;
    plus_self = ret_val2;
    plus_s = ((STR) &foundf51920346);
    stdout_self = ((FILE1) NULL);
    L12=ZALLOC(sizeof(struct FILE1_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=FILE1_tag;
    L10 = ((FILE1) L12);
    r = L10;
    
    SATTR(r,fp,stdout);
    ret_val4 = r;
    FILE_plus_STR(ret_val4, plus_s);
    ret_val3 = plus_self;
    plus_self1 = ret_val3;
    plus_s1 = ATTR(ATTR(am,fun),str);
    stdout_self1 = ((FILE1) NULL);
    L15=ZALLOC(sizeof(struct FILE1_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=FILE1_tag;
    L14 = ((FILE1) L15);
    r1 = L14;
    
    SATTR(r1,fp,stdout);
    ret_val6 = r1;
    FILE_plus_STR(ret_val6, plus_s1);
    ret_val5 = plus_self1;
    plus_self2 = ret_val5;
    plus_s2 = ((STR) &name9);
    stdout_self2 = ((FILE1) NULL);
    L18=ZALLOC(sizeof(struct FILE1_struct));
    if (L18==NULL) FATAL("Unable to allocate more memory");
    ((OB)L18)->header.tag=FILE1_tag;
    L17 = ((FILE1) L18);
    r2 = L17;
    
    SATTR(r2,fp,stdout);
    ret_val7 = r2;
    FILE_plus_STR(ret_val7, plus_s2);
   }
   ret_val = FALSE;
   return ret_val;
  }
  {
   struct FSETSI2093978094_frame_struct other1_0;
FSETSI2093978094_frame noname1 = &other1_0;
   /* Initialization of inlined iter SE_CONTEXT::elt!:SIDE_EFFECT */
   L11 = c;
   eltb_self = L11;
   L41 = ATTRs(eltb_self,set,((FSETSIDE_EFFECT) NULL));
   noname1->self = L41;
   noname1->state = 0;
   eltb_if_first = TRUE;
   while (1) {
    L20 = FSETSI2093978094(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    e = L20;
    if (ATTR(e,doeswrite)) {
     L23 = TRUE;
    } else {
     L23 = (ATTR(e,tp)==((dTP) NULL));
    }
    if (L23) {
     L22 = TRUE;
    } else {
     L22 = (ATTR(e,name1).str==(STR)0);
    }
    if (L22) {
     if (AM_CURSOR_debug) {
      create_self1 = ((OUT) NULL);
      ret_val9 = create_self1;
      plus_self3 = ret_val9;
      plus_s3 = ((STR) &routine2);
      stdout_self3 = ((FILE1) NULL);
      L25=ZALLOC(sizeof(struct FILE1_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=FILE1_tag;
      L24 = ((FILE1) L25);
      r3 = L24;
      
      SATTR(r3,fp,stdout);
      ret_val11 = r3;
      FILE_plus_STR(ret_val11, plus_s3);
      ret_val10 = plus_self3;
      plus_self4 = ret_val10;
      plus_s4 = ATTR(ATTR(am,fun),str);
      stdout_self4 = ((FILE1) NULL);
      L28=ZALLOC(sizeof(struct FILE1_struct));
      if (L28==NULL) FATAL("Unable to allocate more memory");
      ((OB)L28)->header.tag=FILE1_tag;
      L27 = ((FILE1) L28);
      r4 = L27;
      
      SATTR(r4,fp,stdout);
      ret_val13 = r4;
      FILE_plus_STR(ret_val13, plus_s4);
      ret_val12 = plus_self4;
      plus_self5 = ret_val12;
      plus_s5 = ((STR) &attrtp);
      stdout_self5 = ((FILE1) NULL);
      L32=ZALLOC(sizeof(struct FILE1_struct));
      if (L32==NULL) FATAL("Unable to allocate more memory");
      ((OB)L32)->header.tag=FILE1_tag;
      L30 = ((FILE1) L32);
      r5 = L30;
      
      SATTR(r5,fp,stdout);
      ret_val14 = r5;
      FILE_plus_STR(ret_val14, plus_s5);
      if ((ATTR(e,tp)==((dTP) NULL))) {
       create_self2 = ((OUT) NULL);
       ret_val15 = create_self2;
       plus_self6 = ret_val15;
       plus_s6 = ((STR) &void1);
       stdout_self6 = ((FILE1) NULL);
       L35=ZALLOC(sizeof(struct FILE1_struct));
       if (L35==NULL) FATAL("Unable to allocate more memory");
       ((OB)L35)->header.tag=FILE1_tag;
       L34 = ((FILE1) L35);
       r6 = L34;
       
       SATTR(r6,fp,stdout);
       ret_val16 = r6;
       FILE_plus_STR(ret_val16, plus_s6);
      }
      else {
       create_self3 = ((OUT) NULL);
       ret_val17 = create_self3;
       plus_self7 = ret_val17;
       L37 = ATTR(e,tp);
       plus_s7 = (*dTP_strrSTR[TAG(L37)])(L37);
       stdout_self7 = ((FILE1) NULL);
       L39=ZALLOC(sizeof(struct FILE1_struct));
       if (L39==NULL) FATAL("Unable to allocate more memory");
       ((OB)L39)->header.tag=FILE1_tag;
       L38 = ((FILE1) L39);
       r7 = L38;
       
       SATTR(r7,fp,stdout);
       ret_val18 = r7;
       FILE_plus_STR(ret_val18, plus_s7);
      }
      create_self4 = ((OUT) NULL);
      ret_val19 = create_self4;
      plus_self8 = ret_val19;
      plus_s8 = ((STR) &name59);
      stdout_self8 = ((FILE1) NULL);
      L43=ZALLOC(sizeof(struct FILE1_struct));
      if (L43==NULL) FATAL("Unable to allocate more memory");
      ((OB)L43)->header.tag=FILE1_tag;
      L42 = ((FILE1) L43);
      r8 = L42;
      
      SATTR(r8,fp,stdout);
      ret_val20 = r8;
      FILE_plus_STR(ret_val20, plus_s8);
      if ((ATTR(e,name1).str==(STR)0)) {
       create_self5 = ((OUT) NULL);
       ret_val21 = create_self5;
       plus_self9 = ret_val21;
       plus_s9 = ((STR) &void1);
       stdout_self9 = ((FILE1) NULL);
       L46=ZALLOC(sizeof(struct FILE1_struct));
       if (L46==NULL) FATAL("Unable to allocate more memory");
       ((OB)L46)->header.tag=FILE1_tag;
       L45 = ((FILE1) L46);
       r9 = L45;
       
       SATTR(r9,fp,stdout);
       ret_val22 = r9;
       FILE_plus_STR(ret_val22, plus_s9);
      }
      else {
       create_self6 = ((OUT) NULL);
       ret_val23 = create_self6;
       plus_self10 = ret_val23;
       plus_s10 = ATTR(e,name1).str;
       stdout_self10 = ((FILE1) NULL);
       L49=ZALLOC(sizeof(struct FILE1_struct));
       if (L49==NULL) FATAL("Unable to allocate more memory");
       ((OB)L49)->header.tag=FILE1_tag;
       L48 = ((FILE1) L49);
       r10 = L48;
       
       SATTR(r10,fp,stdout);
       ret_val24 = r10;
       FILE_plus_STR(ret_val24, plus_s10);
      }
      create_self7 = ((OUT) NULL);
      ret_val25 = create_self7;
      plus_self11 = ret_val25;
      plus_s11 = ((STR) &doeswrite);
      stdout_self11 = ((FILE1) NULL);
      L53=ZALLOC(sizeof(struct FILE1_struct));
      if (L53==NULL) FATAL("Unable to allocate more memory");
      ((OB)L53)->header.tag=FILE1_tag;
      L52 = ((FILE1) L53);
      r11 = L52;
      
      SATTR(r11,fp,stdout);
      ret_val27 = r11;
      FILE_plus_STR(ret_val27, plus_s11);
      ret_val26 = plus_self11;
      plus_self12 = ret_val26;
      L56=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
      if (L56==NULL) FATAL("Unable to allocate more memory");
      memset(L56,0,sizeof(struct BOOL_boxed_struct));
      ((OB)L56)->header.tag=BOOL_tag;
      L55 = (dSTR)((BOOL_boxed) L56);
      ((BOOL_boxed) L55)->immutable_part = ATTR(e,doeswrite);
      plus_s12 = L55;
      stdout_self12 = ((FILE1) NULL);
      L58=ZALLOC(sizeof(struct FILE1_struct));
      if (L58==NULL) FATAL("Unable to allocate more memory");
      ((OB)L58)->header.tag=FILE1_tag;
      L57 = ((FILE1) L58);
      r12 = L57;
      
      SATTR(r12,fp,stdout);
      ret_val29 = r12;
      FILE_plus_dSTR(ret_val29, plus_s12);
      ret_val28 = plus_self12;
      plus_self13 = ret_val28;
      plus_s13 = ((STR) &name9);
      stdout_self13 = ((FILE1) NULL);
      L61=ZALLOC(sizeof(struct FILE1_struct));
      if (L61==NULL) FATAL("Unable to allocate more memory");
      ((OB)L61)->header.tag=FILE1_tag;
      L60 = ((FILE1) L61);
      r13 = L60;
      
      SATTR(r13,fp,stdout);
      ret_val30 = r13;
      FILE_plus_STR(ret_val30, plus_s13);
     }
     ret_val = FALSE;
     return ret_val;
    }
    else {
     L63 = AM_CUR1358945253(((AM_CURSOR) NULL), c11);
     L64 = AM_CUR1044001004(self, L63, ATTR(e,tp), ATTR(e,name1));
     L651_=!(L64); 
     if (L651_) {
      ret_val = FALSE;
      return ret_val;
     }
    }
   }
  }
  after_loop: ;
  {
   BOOL f_L661_ = TRUE;
   BOOL f_L671_ = TRUE;
   /* loop index variable */
   L51 = 0;
   L21 = am;
   L31 = am;
   L661_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
   L671_br=L31==NULL?0:ASIZE((AM_ROU1916046290)L31); 
   i_L671_=0;
   while (1) {
    if(L51>=L661_br)  goto after_loop1; 
    aL661_=ARR((AM_ROU1916046290)L21,L51); 
    L69=aL661_;
    L70 = ATTR(L69,mode1);
    L71 = (*dMODE_814247358[TAG(L70)])(L70, ((dMODE) MODES_in_mode));
    L721_=!(L71); 
    if (L721_) {
     if (AM_CURSOR_debug) {
      create_self8 = ((OUT) NULL);
      ret_val31 = create_self8;
      plus_self14 = ret_val31;
      plus_s14 = ((STR) &founda280474472);
      stdout_self14 = ((FILE1) NULL);
      L74=ZALLOC(sizeof(struct FILE1_struct));
      if (L74==NULL) FATAL("Unable to allocate more memory");
      ((OB)L74)->header.tag=FILE1_tag;
      L73 = ((FILE1) L74);
      r14 = L73;
      
      SATTR(r14,fp,stdout);
      ret_val33 = r14;
      FILE_plus_STR(ret_val33, plus_s14);
      ret_val32 = plus_self14;
      plus_self15 = ret_val32;
      plus_s15 = ATTR(ATTR(am,fun),str);
      stdout_self15 = ((FILE1) NULL);
      L77=ZALLOC(sizeof(struct FILE1_struct));
      if (L77==NULL) FATAL("Unable to allocate more memory");
      ((OB)L77)->header.tag=FILE1_tag;
      L76 = ((FILE1) L77);
      r15 = L76;
      
      SATTR(r15,fp,stdout);
      ret_val35 = r15;
      FILE_plus_STR(ret_val35, plus_s15);
      ret_val34 = plus_self15;
      plus_self16 = ret_val34;
      plus_s16 = ((STR) &name9);
      stdout_self16 = ((FILE1) NULL);
      L80=ZALLOC(sizeof(struct FILE1_struct));
      if (L80==NULL) FATAL("Unable to allocate more memory");
      ((OB)L80)->header.tag=FILE1_tag;
      L79 = ((FILE1) L80);
      r16 = L79;
      
      SATTR(r16,fp,stdout);
      ret_val36 = r16;
      FILE_plus_STR(ret_val36, plus_s16);
     }
     ret_val = FALSE;
     return ret_val;
    }
    L82 = AM_CUR1358945253(((AM_CURSOR) NULL), c11);
    if(i_L671_>=L671_br)  goto after_loop1; 
    aL671_=ARR((AM_ROU1916046290)L31,i_L671_); i_L671_++;
    L84 = AM_CUR750559803(self, L82, ((dAM) aL671_));
    L851_=!(L84); 
    if (L851_) {
     ret_val = FALSE;
     return ret_val;
    }
    L861_=INTPLUS(L51,1); 
    L51 = L861_;
   }
  }
  after_loop1: ;
  ret_val = TRUE;
  return ret_val;
 }
 else {
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1240489726(AM_CURSOR self, AM_CURSOR c, AM_LOCAL_EXPR am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 AM_CALL_ARG a;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val13;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n;
 OUT create_self5;
 OUT ret_val15;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT create_self6;
 OUT ret_val17;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 AM_OUT AM_one_stmt_self1;
 dAM AM_one_stmt_a1;
 dAM_STMT n11;
 OUT create_self7;
 OUT ret_val19;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 OUT create_self8;
 OUT ret_val21;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val22;
 FILE1 r10;
 AM_OUT AM_one_stmt_self2;
 dAM AM_one_stmt_a2;
 dAM_STMT n2;
 OUT create_self9;
 OUT ret_val23;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val24;
 FILE1 r11;
 OUT create_self10;
 OUT ret_val25;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val26;
 FILE1 r12;
 dAM L4;
 extern STR is_am_1835226159;
 FILE1 L5;
 OB L6;
 INT L81_;
 INT L9;
 STR L101_;
 FILE1 L12;
 OB L13;
 extern STR name9;
 FILE1 L15;
 OB L16;
 extern STR name9;
 FILE1 L18;
 OB L19;
 extern STR TRUE2;
 FILE1 L22;
 OB L23;
 extern STR therei679909862;
 FILE1 L25;
 OB L26;
 extern STR namely;
 FILE1 L28;
 OB L29;
 dAM_STMT L32;
 dAM_STMT L33;
 dAM_STMT L34;
 BOOL L35;
 extern STR therei679909862;
 FILE1 L36;
 OB L37;
 extern STR namely;
 FILE1 L39;
 OB L40;
 dAM_STMT L42;
 dAM_STMT L43;
 dAM_STMT L44;
 BOOL L45;
 extern STR therei679909862;
 FILE1 L46;
 OB L47;
 extern STR namely;
 FILE1 L49;
 OB L50;
 dAM_STMT L52;
 dAM_STMT L53;
 dAM_STMT L54;
 INT L56;
 dAM_CALL_EXPR L57;
 INT L58;
 INT L591_;
 INT L551_,L551_m;
 dAM_CALL_EXPR L60;
 BOOL L61;
 BOOL L62;
 dMODE L63;
 dMODE L64;
 extern STR thelocalvar;
 FILE1 L65;
 OB L66;
 extern STR isused1152025630;
 FILE1 L68;
 OB L69;
 if (ATTR(am,is_hot)) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L4 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L4;
   if (AM_CURSOR_debug) {
    create_self = ((OUT) NULL);
    ret_val1 = create_self;
    plus_self = ret_val1;
    plus_s = ((STR) &is_am_1835226159);
    stdout_self = ((FILE1) NULL);
    L6=ZALLOC(sizeof(struct FILE1_struct));
    if (L6==NULL) FATAL("Unable to allocate more memory");
    ((OB)L6)->header.tag=FILE1_tag;
    L5 = ((FILE1) L6);
    r = L5;
    
    SATTR(r,fp,stdout);
    ret_val3 = r;
    FILE_plus_STR(ret_val3, plus_s);
    ret_val2 = plus_self;
    plus_self1 = ret_val2;
    L81_=SYSTP(t); 
    L9 = L81_;
    L101_=SYSSTRFORTP(L9); 
    plus_s1 = L101_;
    stdout_self1 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r1 = L12;
    
    SATTR(r1,fp,stdout);
    ret_val5 = r1;
    FILE_plus_STR(ret_val5, plus_s1);
    ret_val4 = plus_self1;
    plus_self2 = ret_val4;
    plus_s2 = ((STR) &name9);
    stdout_self2 = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r2 = L15;
    
    SATTR(r2,fp,stdout);
    ret_val6 = r2;
    FILE_plus_STR(ret_val6, plus_s2);
    create_self1 = ((OUT) NULL);
    ret_val7 = create_self1;
    plus_self3 = ret_val7;
    plus_s3 = ((STR) &name9);
    stdout_self3 = ((FILE1) NULL);
    L19=ZALLOC(sizeof(struct FILE1_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=FILE1_tag;
    L18 = ((FILE1) L19);
    r3 = L18;
    
    SATTR(r3,fp,stdout);
    ret_val8 = r3;
    FILE_plus_STR(ret_val8, plus_s3);
   }
   if (AM_CUR1885691330(c, self)) {
    if (AM_CURSOR_debug) {
     create_self2 = ((OUT) NULL);
     ret_val9 = create_self2;
     plus_self4 = ret_val9;
     plus_s4 = ((STR) &TRUE2);
     stdout_self4 = ((FILE1) NULL);
     L23=ZALLOC(sizeof(struct FILE1_struct));
     if (L23==NULL) FATAL("Unable to allocate more memory");
     ((OB)L23)->header.tag=FILE1_tag;
     L22 = ((FILE1) L23);
     r4 = L22;
     
     SATTR(r4,fp,stdout);
     ret_val10 = r4;
     FILE_plus_STR(ret_val10, plus_s4);
    }
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other238;
   } else
   switch (TAG(t)) {
    case AM_CALL_ARG_tag: break;
    case AM_ASSIGN_STMT_tag:
     if (SYSOBEQ(((OB) ATTR(((AM_ASSIGN_STMT) t),dest)),((OB) am))) {
      if (AM_CURSOR_debug) {
       create_self3 = ((OUT) NULL);
       ret_val11 = create_self3;
       plus_self5 = ret_val11;
       plus_s5 = ((STR) &therei679909862);
       stdout_self5 = ((FILE1) NULL);
       L26=ZALLOC(sizeof(struct FILE1_struct));
       if (L26==NULL) FATAL("Unable to allocate more memory");
       ((OB)L26)->header.tag=FILE1_tag;
       L25 = ((FILE1) L26);
       r5 = L25;
       
       SATTR(r5,fp,stdout);
       ret_val12 = r5;
       FILE_plus_STR(ret_val12, plus_s5);
       AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
       create_self4 = ((OUT) NULL);
       ret_val13 = create_self4;
       plus_self6 = ret_val13;
       plus_s6 = ((STR) &namely);
       stdout_self6 = ((FILE1) NULL);
       L29=ZALLOC(sizeof(struct FILE1_struct));
       if (L29==NULL) FATAL("Unable to allocate more memory");
       ((OB)L29)->header.tag=FILE1_tag;
       L28 = ((FILE1) L29);
       r6 = L28;
       
       SATTR(r6,fp,stdout);
       ret_val14 = r6;
       FILE_plus_STR(ret_val14, plus_s6);
       AM_one_stmt_self = ((AM_OUT) NULL);
       AM_one_stmt_a = t;
       switch (TAG(AM_one_stmt_a)) {
        case AM_ASSERT_STMT_tag:
        case AM_ASSIGN_STMT_tag:
        case AM_ATTACH_STMT_tag:
        case AM_BREAK_STMT_tag:
        case AM_CASE_STMT_tag:
        case AM_COMMENT_STMT_tag:
        case AM_EXPR_STMT_tag:
        case AM_FORK_STMT_tag:
        case AM_IF_STMT_tag:
        case AM_INITIAL_STMT_tag:
        case AM_INV1167837230_tag:
        case AM_LOCK_STMT_tag:
        case AM_LOOP_STMT_tag:
        case AM_PAR_STMT_tag:
        case AM_POST_STMT_tag:
        case AM_PREFETCH_STMT_tag:
        case AM_PRE_STMT_tag:
        case AM_PROTECT_STMT_tag:
        case AM_RAISE_STMT_tag:
        case AM_RETURN_STMT_tag:
        case AM_SYNC_STMT_tag:
        case AM_TYPECASE_STMT_tag:
        case AM_UNLOCK_STMT_tag:
        case AM_WAITFOR_STMT_tag:
        case AM_WIT1996971603_tag:
        case AM_YIELD_STMT_tag:
         L32 = ((dAM_STMT) AM_one_stmt_a);
         n = (*dAM_ST775224435[TAG(L32)])(L32);
         L33 = ((dAM_STMT) AM_one_stmt_a);
         (*dAM_ST1372701974[TAG(L33)])(L33, ((dAM_STMT) NULL));
         AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
         L34 = ((dAM_STMT) AM_one_stmt_a);
         (*dAM_ST1372701974[TAG(L34)])(L34, n); break;
        default: ;
         AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
       }
      }
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_WAITFOR_STMT_tag:
     if (SYSOBEQ(((OB) ATTR(((AM_WAITFOR_STMT) t),dest)),((OB) am))) {
      L35 = TRUE;
     } else {
      L35 = (ATTR(((AM_WAITFOR_STMT) t),prefetch)==am);
     }
     if (L35) {
      if (AM_CURSOR_debug) {
       create_self5 = ((OUT) NULL);
       ret_val15 = create_self5;
       plus_self7 = ret_val15;
       plus_s7 = ((STR) &therei679909862);
       stdout_self7 = ((FILE1) NULL);
       L37=ZALLOC(sizeof(struct FILE1_struct));
       if (L37==NULL) FATAL("Unable to allocate more memory");
       ((OB)L37)->header.tag=FILE1_tag;
       L36 = ((FILE1) L37);
       r7 = L36;
       
       SATTR(r7,fp,stdout);
       ret_val16 = r7;
       FILE_plus_STR(ret_val16, plus_s7);
       AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
       create_self6 = ((OUT) NULL);
       ret_val17 = create_self6;
       plus_self8 = ret_val17;
       plus_s8 = ((STR) &namely);
       stdout_self8 = ((FILE1) NULL);
       L40=ZALLOC(sizeof(struct FILE1_struct));
       if (L40==NULL) FATAL("Unable to allocate more memory");
       ((OB)L40)->header.tag=FILE1_tag;
       L39 = ((FILE1) L40);
       r8 = L39;
       
       SATTR(r8,fp,stdout);
       ret_val18 = r8;
       FILE_plus_STR(ret_val18, plus_s8);
       AM_one_stmt_self1 = ((AM_OUT) NULL);
       AM_one_stmt_a1 = t;
       switch (TAG(AM_one_stmt_a1)) {
        case AM_ASSERT_STMT_tag:
        case AM_ASSIGN_STMT_tag:
        case AM_ATTACH_STMT_tag:
        case AM_BREAK_STMT_tag:
        case AM_CASE_STMT_tag:
        case AM_COMMENT_STMT_tag:
        case AM_EXPR_STMT_tag:
        case AM_FORK_STMT_tag:
        case AM_IF_STMT_tag:
        case AM_INITIAL_STMT_tag:
        case AM_INV1167837230_tag:
        case AM_LOCK_STMT_tag:
        case AM_LOOP_STMT_tag:
        case AM_PAR_STMT_tag:
        case AM_POST_STMT_tag:
        case AM_PREFETCH_STMT_tag:
        case AM_PRE_STMT_tag:
        case AM_PROTECT_STMT_tag:
        case AM_RAISE_STMT_tag:
        case AM_RETURN_STMT_tag:
        case AM_SYNC_STMT_tag:
        case AM_TYPECASE_STMT_tag:
        case AM_UNLOCK_STMT_tag:
        case AM_WAITFOR_STMT_tag:
        case AM_WIT1996971603_tag:
        case AM_YIELD_STMT_tag:
         L42 = ((dAM_STMT) AM_one_stmt_a1);
         n11 = (*dAM_ST775224435[TAG(L42)])(L42);
         L43 = ((dAM_STMT) AM_one_stmt_a1);
         (*dAM_ST1372701974[TAG(L43)])(L43, ((dAM_STMT) NULL));
         AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
         L44 = ((dAM_STMT) AM_one_stmt_a1);
         (*dAM_ST1372701974[TAG(L44)])(L44, n11); break;
        default: ;
         AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
       }
      }
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_PREFETCH_STMT_tag:
     if (SYSOBEQ(((OB) ATTR(((AM_PREFETCH_STMT) t),dest)),((OB) am))) {
      L45 = TRUE;
     } else {
      L45 = (ATTR(((AM_PREFETCH_STMT) t),prefetch)==am);
     }
     if (L45) {
      if (AM_CURSOR_debug) {
       create_self7 = ((OUT) NULL);
       ret_val19 = create_self7;
       plus_self9 = ret_val19;
       plus_s9 = ((STR) &therei679909862);
       stdout_self9 = ((FILE1) NULL);
       L47=ZALLOC(sizeof(struct FILE1_struct));
       if (L47==NULL) FATAL("Unable to allocate more memory");
       ((OB)L47)->header.tag=FILE1_tag;
       L46 = ((FILE1) L47);
       r9 = L46;
       
       SATTR(r9,fp,stdout);
       ret_val20 = r9;
       FILE_plus_STR(ret_val20, plus_s9);
       AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
       create_self8 = ((OUT) NULL);
       ret_val21 = create_self8;
       plus_self10 = ret_val21;
       plus_s10 = ((STR) &namely);
       stdout_self10 = ((FILE1) NULL);
       L50=ZALLOC(sizeof(struct FILE1_struct));
       if (L50==NULL) FATAL("Unable to allocate more memory");
       ((OB)L50)->header.tag=FILE1_tag;
       L49 = ((FILE1) L50);
       r10 = L49;
       
       SATTR(r10,fp,stdout);
       ret_val22 = r10;
       FILE_plus_STR(ret_val22, plus_s10);
       AM_one_stmt_self2 = ((AM_OUT) NULL);
       AM_one_stmt_a2 = t;
       switch (TAG(AM_one_stmt_a2)) {
        case AM_ASSERT_STMT_tag:
        case AM_ASSIGN_STMT_tag:
        case AM_ATTACH_STMT_tag:
        case AM_BREAK_STMT_tag:
        case AM_CASE_STMT_tag:
        case AM_COMMENT_STMT_tag:
        case AM_EXPR_STMT_tag:
        case AM_FORK_STMT_tag:
        case AM_IF_STMT_tag:
        case AM_INITIAL_STMT_tag:
        case AM_INV1167837230_tag:
        case AM_LOCK_STMT_tag:
        case AM_LOOP_STMT_tag:
        case AM_PAR_STMT_tag:
        case AM_POST_STMT_tag:
        case AM_PREFETCH_STMT_tag:
        case AM_PRE_STMT_tag:
        case AM_PROTECT_STMT_tag:
        case AM_RAISE_STMT_tag:
        case AM_RETURN_STMT_tag:
        case AM_SYNC_STMT_tag:
        case AM_TYPECASE_STMT_tag:
        case AM_UNLOCK_STMT_tag:
        case AM_WAITFOR_STMT_tag:
        case AM_WIT1996971603_tag:
        case AM_YIELD_STMT_tag:
         L52 = ((dAM_STMT) AM_one_stmt_a2);
         n2 = (*dAM_ST775224435[TAG(L52)])(L52);
         L53 = ((dAM_STMT) AM_one_stmt_a2);
         (*dAM_ST1372701974[TAG(L53)])(L53, ((dAM_STMT) NULL));
         AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
         L54 = ((dAM_STMT) AM_one_stmt_a2);
         (*dAM_ST1372701974[TAG(L54)])(L54, n2); break;
        default: ;
         AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
       }
      }
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_BND367211769_tag:
    case AM_BND260301329_tag:
    case AM_EXT_CALL_EXPR_tag:
    case AM_ITE1809135850_tag:
    case AM_ROU1916046290_tag:
     {
      BOOL f_L551_ = TRUE;
      while (1) {
       if (f_L551_) {
        f_L551_ = FALSE;
        L21 = 0;
        L57 = ((dAM_CALL_EXPR) t);
        L58 = (*dAM_CA1128544926[TAG(L57)])(L57);
        L591_=INTMINUS(L58,1); 
        L31 = L591_;
        L551_=L21-1;L551_m=L31; 
       }
       if(L551_++>=L551_m)  goto after_loop1; 
       L60 = ((dAM_CALL_EXPR) t);
       a = (*dAM_CA967858872[TAG(L60)])(L60, L551_);
       L63 = ATTR(a,mode1);
       if ((*dMODE_814247358[TAG(L63)])(L63, ((dMODE) MODES_out_mode))) {
        L62 = TRUE;
       } else {
        L64 = ATTR(a,mode1);
        L62 = (*dMODE_814247358[TAG(L64)])(L64, ((dMODE) MODES_inout_mode));
       }
       if (L62) {
        L61 = SYSOBEQ(((OB) ATTR(a,expr)),((OB) am));
       } else {
        L61 = FALSE;
       }
       if (L61) {
        if (AM_CURSOR_debug) {
         create_self9 = ((OUT) NULL);
         ret_val23 = create_self9;
         plus_self11 = ret_val23;
         plus_s11 = ((STR) &thelocalvar);
         stdout_self11 = ((FILE1) NULL);
         L66=ZALLOC(sizeof(struct FILE1_struct));
         if (L66==NULL) FATAL("Unable to allocate more memory");
         ((OB)L66)->header.tag=FILE1_tag;
         L65 = ((FILE1) L66);
         r11 = L65;
         
         SATTR(r11,fp,stdout);
         ret_val24 = r11;
         FILE_plus_STR(ret_val24, plus_s11);
         AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
         create_self10 = ((OUT) NULL);
         ret_val25 = create_self10;
         plus_self12 = ret_val25;
         plus_s12 = ((STR) &isused1152025630);
         stdout_self12 = ((FILE1) NULL);
         L69=ZALLOC(sizeof(struct FILE1_struct));
         if (L69==NULL) FATAL("Unable to allocate more memory");
         ((OB)L69)->header.tag=FILE1_tag;
         L68 = ((FILE1) L69);
         r12 = L68;
         
         SATTR(r12,fp,stdout);
         ret_val26 = r12;
         FILE_plus_STR(ret_val26, plus_s12);
         AM_OUT1439666796(((AM_OUT) NULL), t);
        }
        ret_val = FALSE;
        return ret_val;
       }
      }
     }
     after_loop1: ; break;
    default: ;
    other238: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1481630356(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR_POS p;
 dAM tcur;
 INT branch1 = INT_zero;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val2;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L81_;
 BOOL L91_;
 dMODE L10;
 BOOL L11;
 BOOL L121_;
 size_self = ATTR(self,stack1);
 ret_val1 = FLISTA587993904(ATTR(size_self,s));
 L1 = ret_val1;
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = TRUE;
  return ret_val;
 }
 top_self = ATTR(self,stack1);
 ret_val2 = FLISTA860779230(ATTR(top_self,s));
 p = ret_val2;
 tcur = ATTR(p,stmt);
 branch1 = ATTR(p,branch);
 if (tcur==NULL) {
  goto other229;
 } else
 switch (TAG(tcur)) {
  case AM_WAITFOR_STMT_tag:
   L41_=(branch1)==(1); 
   if (L41_) {
    L3 = TRUE;
   } else {
    L51_=(branch1)==(3); 
    L3 = L51_;
   }
   ret_val = L3;
   return ret_val; break;
  case AM_PREFETCH_STMT_tag:
   L71_=(branch1)==(1); 
   if (L71_) {
    L6 = TRUE;
   } else {
    L81_=(branch1)==(3); 
    L6 = L81_;
   }
   ret_val = L6;
   return ret_val; break;
  case AM_ASSIGN_STMT_tag:
   L91_=(branch1)==(1); 
   ret_val = L91_;
   return ret_val; break;
  case AM_CALL_ARG_tag:
   L10 = ATTR(((AM_CALL_ARG) tcur),mode1);
   L11 = (*dMODE_814247358[TAG(L10)])(L10, ((dMODE) MODES_in_mode));
   L121_=!(L11); 
   ret_val = L121_;
   return ret_val; break;
  default: ;
  other229: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1587547281(AM_CURSOR self, AM_CURSOR c11, dTP tp, IDENT name111) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 SE_CONTEXT c;
 SIDE_EFFECT se;
 SE_CONTEXT c12;
 SIDE_EFFECT se1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 SE_CONTEXT full_self;
 BOOL ret_val2 = BOOL_zero;
 SE_CONTEXT se_attr_self;
 dTP se_attr_tp;
 IDENT se_attr_name = IDENT_zero;
 SIDE_EFFECT ret_val3;
 SIDE_EFFECT s;
 SIDE_EFFECT create_self;
 SIDE_EFFECT ret_val4;
 OUT create_self1;
 OUT ret_val5;
 OUT plus_self;
 STR plus_s;
 OUT ret_val6;
 FILE1 stdout_self;
 FILE1 ret_val7;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val8;
 FILE1 stdout_self1;
 FILE1 ret_val9;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val10;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val12;
 FILE1 stdout_self3;
 FILE1 ret_val13;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val14;
 FILE1 r4;
 SE_CONTEXT full_self1;
 BOOL ret_val15 = BOOL_zero;
 SE_CONTEXT se_attr_self1;
 dTP se_attr_tp1;
 IDENT se_attr_name1 = IDENT_zero;
 SIDE_EFFECT ret_val16;
 SIDE_EFFECT s1;
 SIDE_EFFECT create_self2;
 SIDE_EFFECT ret_val17;
 OUT create_self3;
 OUT ret_val18;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val19;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val20;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val21;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val22;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val23;
 FILE1 stdout_self7;
 FILE1 ret_val24;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val25;
 FILE1 stdout_self8;
 FILE1 ret_val26;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val27;
 FILE1 r9;
 dAM L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L6;
 SIDE_EFFECT L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 extern STR foundd1562551159;
 FILE1 L12;
 OB L13;
 FILE1 L15;
 OB L16;
 extern STR in3;
 FILE1 L18;
 OB L19;
 FILE1 L21;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 BOOL L27;
 BOOL L28;
 SIDE_EFFECT L29;
 OB L30;
 BOOL L31;
 BOOL L321_;
 extern STR notunu421775452;
 FILE1 L33;
 OB L34;
 extern STR notunu589135516;
 FILE1 L36;
 OB L37;
 extern STR notunu1830346893;
 FILE1 L39;
 OB L40;
 FILE1 L42;
 OB L43;
 extern STR name9;
 FILE1 L45;
 OB L46;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c11;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L2;
   if (AM_CUR1885691330(c11, self)) {
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other233;
   } else
   switch (TAG(t)) {
    case AM_SHARED_EXPR_tag:
     L4 = ATTR(((AM_SHARED_EXPR) t),class_tp);
     if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) tp))) {
      is_eq_self = ATTR(((AM_SHARED_EXPR) t),name1);
      is_eq_i = name111;
      ret_val1 = (is_eq_self.str==is_eq_i.str);
      L3 = ret_val1;
     } else {
      L3 = FALSE;
     }
     if (L3) {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_ROU1916046290_tag:
     if (ATTR(self,with_side_effect)) {
      c = SIG_ge642497605(ATTR(((AM_ROU1916046290) t),fun), ATTR(self,prog));
      full_self = c;
      if (ATTR(full_self,is_full)) {
       L6 = TRUE;
      } else {
       L6 = ATTR(full_self,has_import);
      }
      if (L6) {
       L5 = TRUE;
      } else {
       L5 = ATTR(full_self,has_export);
      }
      ret_val2 = L5;
      if (ret_val2) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self = c;
      se_attr_tp = tp;
      se_attr_name = name111;
      create_self = ((SIDE_EFFECT) NULL);
      L8=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L8==NULL) FATAL("Unable to allocate more memory");
      ((OB)L8)->header.tag=SIDE_EFFECT_tag;
      L7 = ((SIDE_EFFECT) L8);
      ret_val4 = L7;
      s = ret_val4;
      SATTR(s,tp,se_attr_tp);
      SATTR(s,name1,se_attr_name);
      ret_val3 = FSETSI937439554(ATTR(se_attr_self,set), s);
      se = ret_val3;
      L9 = (se==((SIDE_EFFECT) NULL));
      L101_=!(L9); 
      if (L101_) {
       if (AM_CURSOR_debug) {
        create_self1 = ((OUT) NULL);
        ret_val5 = create_self1;
        plus_self = ret_val5;
        plus_s = ((STR) &foundd1562551159);
        stdout_self = ((FILE1) NULL);
        L13=ZALLOC(sizeof(struct FILE1_struct));
        if (L13==NULL) FATAL("Unable to allocate more memory");
        ((OB)L13)->header.tag=FILE1_tag;
        L12 = ((FILE1) L13);
        r = L12;
        
        SATTR(r,fp,stdout);
        ret_val7 = r;
        FILE_plus_STR(ret_val7, plus_s);
        ret_val6 = plus_self;
        plus_self1 = ret_val6;
        plus_s1 = SIDE_E1431483993(se);
        stdout_self1 = ((FILE1) NULL);
        L16=ZALLOC(sizeof(struct FILE1_struct));
        if (L16==NULL) FATAL("Unable to allocate more memory");
        ((OB)L16)->header.tag=FILE1_tag;
        L15 = ((FILE1) L16);
        r1 = L15;
        
        SATTR(r1,fp,stdout);
        ret_val9 = r1;
        FILE_plus_STR(ret_val9, plus_s1);
        ret_val8 = plus_self1;
        plus_self2 = ret_val8;
        plus_s2 = ((STR) &in3);
        stdout_self2 = ((FILE1) NULL);
        L19=ZALLOC(sizeof(struct FILE1_struct));
        if (L19==NULL) FATAL("Unable to allocate more memory");
        ((OB)L19)->header.tag=FILE1_tag;
        L18 = ((FILE1) L19);
        r2 = L18;
        
        SATTR(r2,fp,stdout);
        ret_val11 = r2;
        FILE_plus_STR(ret_val11, plus_s2);
        ret_val10 = plus_self2;
        plus_self3 = ret_val10;
        plus_s3 = ATTR(ATTR(((AM_ROU1916046290) t),fun),str);
        stdout_self3 = ((FILE1) NULL);
        L22=ZALLOC(sizeof(struct FILE1_struct));
        if (L22==NULL) FATAL("Unable to allocate more memory");
        ((OB)L22)->header.tag=FILE1_tag;
        L21 = ((FILE1) L22);
        r3 = L21;
        
        SATTR(r3,fp,stdout);
        ret_val13 = r3;
        FILE_plus_STR(ret_val13, plus_s3);
        ret_val12 = plus_self3;
        plus_self4 = ret_val12;
        plus_s4 = ((STR) &name9);
        stdout_self4 = ((FILE1) NULL);
        L25=ZALLOC(sizeof(struct FILE1_struct));
        if (L25==NULL) FATAL("Unable to allocate more memory");
        ((OB)L25)->header.tag=FILE1_tag;
        L24 = ((FILE1) L25);
        r4 = L24;
        
        SATTR(r4,fp,stdout);
        ret_val14 = r4;
        FILE_plus_STR(ret_val14, plus_s4);
       }
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_ITE1809135850_tag:
     if (ATTR(self,with_side_effect)) {
      c12 = SIG_ge642497605(ATTR(((AM_ITE1809135850) t),fun), ATTR(self,prog));
      full_self1 = c12;
      if (ATTR(full_self1,is_full)) {
       L28 = TRUE;
      } else {
       L28 = ATTR(full_self1,has_import);
      }
      if (L28) {
       L27 = TRUE;
      } else {
       L27 = ATTR(full_self1,has_export);
      }
      ret_val15 = L27;
      if (ret_val15) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self1 = c12;
      se_attr_tp1 = tp;
      se_attr_name1 = name111;
      create_self2 = ((SIDE_EFFECT) NULL);
      L30=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L30==NULL) FATAL("Unable to allocate more memory");
      ((OB)L30)->header.tag=SIDE_EFFECT_tag;
      L29 = ((SIDE_EFFECT) L30);
      ret_val17 = L29;
      s1 = ret_val17;
      SATTR(s1,tp,se_attr_tp1);
      SATTR(s1,name1,se_attr_name1);
      ret_val16 = FSETSI937439554(ATTR(se_attr_self1,set), s1);
      se1 = ret_val16;
      L31 = (se1==((SIDE_EFFECT) NULL));
      L321_=!(L31); 
      if (L321_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_BND260301329_tag:
     if (AM_CURSOR_debug) {
      create_self3 = ((OUT) NULL);
      ret_val18 = create_self3;
      plus_self5 = ret_val18;
      plus_s5 = ((STR) &notunu421775452);
      stdout_self5 = ((FILE1) NULL);
      L34=ZALLOC(sizeof(struct FILE1_struct));
      if (L34==NULL) FATAL("Unable to allocate more memory");
      ((OB)L34)->header.tag=FILE1_tag;
      L33 = ((FILE1) L34);
      r5 = L33;
      
      SATTR(r5,fp,stdout);
      ret_val19 = r5;
      FILE_plus_STR(ret_val19, plus_s5);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_BND367211769_tag:
     if (AM_CURSOR_debug) {
      create_self4 = ((OUT) NULL);
      ret_val20 = create_self4;
      plus_self6 = ret_val20;
      plus_s6 = ((STR) &notunu589135516);
      stdout_self6 = ((FILE1) NULL);
      L37=ZALLOC(sizeof(struct FILE1_struct));
      if (L37==NULL) FATAL("Unable to allocate more memory");
      ((OB)L37)->header.tag=FILE1_tag;
      L36 = ((FILE1) L37);
      r6 = L36;
      
      SATTR(r6,fp,stdout);
      ret_val21 = r6;
      FILE_plus_STR(ret_val21, plus_s6);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_EXT_CALL_EXPR_tag:
     if (AM_CURSOR_debug) {
      create_self5 = ((OUT) NULL);
      ret_val22 = create_self5;
      plus_self7 = ret_val22;
      plus_s7 = ((STR) &notunu1830346893);
      stdout_self7 = ((FILE1) NULL);
      L40=ZALLOC(sizeof(struct FILE1_struct));
      if (L40==NULL) FATAL("Unable to allocate more memory");
      ((OB)L40)->header.tag=FILE1_tag;
      L39 = ((FILE1) L40);
      r7 = L39;
      
      SATTR(r7,fp,stdout);
      ret_val24 = r7;
      FILE_plus_STR(ret_val24, plus_s7);
      ret_val23 = plus_self7;
      plus_self8 = ret_val23;
      plus_s8 = ATTR(ATTR(((AM_EXT_CALL_EXPR) t),fun),str);
      stdout_self8 = ((FILE1) NULL);
      L43=ZALLOC(sizeof(struct FILE1_struct));
      if (L43==NULL) FATAL("Unable to allocate more memory");
      ((OB)L43)->header.tag=FILE1_tag;
      L42 = ((FILE1) L43);
      r8 = L42;
      
      SATTR(r8,fp,stdout);
      ret_val26 = r8;
      FILE_plus_STR(ret_val26, plus_s8);
      ret_val25 = plus_self8;
      plus_self9 = ret_val25;
      plus_s9 = ((STR) &name9);
      stdout_self9 = ((FILE1) NULL);
      L46=ZALLOC(sizeof(struct FILE1_struct));
      if (L46==NULL) FATAL("Unable to allocate more memory");
      ((OB)L46)->header.tag=FILE1_tag;
      L45 = ((FILE1) L46);
      r9 = L45;
      
      SATTR(r9,fp,stdout);
      ret_val27 = r9;
      FILE_plus_STR(ret_val27, plus_s9);
     }
     ret_val = FALSE;
     return ret_val; break;
    default: ;
    other233: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR176216201(AM_CURSOR self, AM_CURSOR c11, dTP tp, IDENT name111) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 dAM_EXPR d;
 dAM_EXPR d11;
 dAM_EXPR d21;
 SE_CONTEXT c;
 SIDE_EFFECT se;
 AM_ROU1916046290 L21;
 AM_CALL_ARG a;
 dAM_EXPR e;
 SE_CONTEXT c12;
 SIDE_EFFECT se1;
 AM_ITE1809135850 L31;
 AM_CALL_ARG a11;
 dAM_EXPR e1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val9;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 OUT create_self1;
 OUT ret_val13;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val14;
 FILE1 stdout_self5;
 FILE1 ret_val15;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val16;
 FILE1 stdout_self6;
 FILE1 ret_val17;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val18;
 FILE1 stdout_self7;
 FILE1 ret_val19;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val20;
 FILE1 stdout_self8;
 FILE1 ret_val21;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val22;
 FILE1 r9;
 AM_OUT AM_one_stmt_self1;
 dAM AM_one_stmt_a1;
 dAM_STMT n11;
 IDENT is_eq_self2 = IDENT_zero;
 IDENT is_eq_i2 = IDENT_zero;
 BOOL ret_val23 = BOOL_zero;
 OUT create_self2;
 OUT ret_val24;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val25;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val27;
 FILE1 stdout_self11;
 FILE1 ret_val28;
 FILE1 r11;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val29;
 FILE1 stdout_self12;
 FILE1 ret_val30;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val31;
 FILE1 stdout_self13;
 FILE1 ret_val32;
 FILE1 r13;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val33;
 FILE1 r14;
 AM_OUT AM_one_stmt_self2;
 dAM AM_one_stmt_a2;
 dAM_STMT n2;
 IDENT is_eq_self3 = IDENT_zero;
 IDENT is_eq_i3 = IDENT_zero;
 BOOL ret_val34 = BOOL_zero;
 OUT create_self3;
 OUT ret_val35;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val36;
 FILE1 stdout_self15;
 FILE1 ret_val37;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 OUT ret_val38;
 FILE1 stdout_self16;
 FILE1 ret_val39;
 FILE1 r16;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val40;
 FILE1 stdout_self17;
 FILE1 ret_val41;
 FILE1 r17;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val42;
 FILE1 stdout_self18;
 FILE1 ret_val43;
 FILE1 r18;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val44;
 FILE1 r19;
 AM_OUT AM_one_stmt_self3;
 dAM AM_one_stmt_a3;
 dAM_STMT n3;
 IDENT is_eq_self4 = IDENT_zero;
 IDENT is_eq_i4 = IDENT_zero;
 BOOL ret_val45 = BOOL_zero;
 OUT create_self4;
 OUT ret_val46;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val47;
 FILE1 stdout_self20;
 FILE1 ret_val48;
 FILE1 r20;
 OUT plus_self21;
 STR plus_s21;
 OUT ret_val49;
 FILE1 stdout_self21;
 FILE1 ret_val50;
 FILE1 r21;
 OUT plus_self22;
 STR plus_s22;
 OUT ret_val51;
 FILE1 stdout_self22;
 FILE1 ret_val52;
 FILE1 r22;
 OUT plus_self23;
 STR plus_s23;
 OUT ret_val53;
 FILE1 stdout_self23;
 FILE1 ret_val54;
 FILE1 r23;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val55;
 FILE1 r24;
 AM_OUT AM_one_stmt_self4;
 dAM AM_one_stmt_a4;
 dAM_STMT n4;
 SE_CONTEXT full_self;
 BOOL ret_val56 = BOOL_zero;
 SE_CONTEXT se_attr_self;
 dTP se_attr_tp;
 IDENT se_attr_name = IDENT_zero;
 SIDE_EFFECT ret_val57;
 SIDE_EFFECT s;
 SIDE_EFFECT create_self5;
 SIDE_EFFECT ret_val58;
 INT L41 = INT_zero;
 IDENT is_eq_self5 = IDENT_zero;
 IDENT is_eq_i5 = IDENT_zero;
 BOOL ret_val59 = BOOL_zero;
 SE_CONTEXT full_self1;
 BOOL ret_val60 = BOOL_zero;
 SE_CONTEXT se_attr_self1;
 dTP se_attr_tp1;
 IDENT se_attr_name1 = IDENT_zero;
 SIDE_EFFECT ret_val61;
 SIDE_EFFECT s1;
 SIDE_EFFECT create_self6;
 SIDE_EFFECT ret_val62;
 OUT create_self7;
 OUT ret_val63;
 OUT plus_self25;
 STR plus_s25;
 OUT ret_val64;
 FILE1 stdout_self25;
 FILE1 ret_val65;
 FILE1 r25;
 OUT plus_self26;
 STR plus_s26;
 OUT ret_val66;
 FILE1 stdout_self26;
 FILE1 ret_val67;
 FILE1 r26;
 OUT plus_self27;
 STR plus_s27;
 OUT ret_val68;
 FILE1 stdout_self27;
 FILE1 ret_val69;
 FILE1 r27;
 OUT plus_self28;
 STR plus_s28;
 OUT ret_val70;
 FILE1 stdout_self28;
 FILE1 ret_val71;
 FILE1 r28;
 OUT plus_self29;
 STR plus_s29;
 OUT ret_val72;
 FILE1 stdout_self29;
 FILE1 ret_val73;
 FILE1 r29;
 OUT plus_self30;
 STR plus_s30;
 OUT ret_val74;
 FILE1 stdout_self30;
 FILE1 ret_val75;
 FILE1 r30;
 OUT plus_self31;
 STR plus_s31;
 FILE1 stdout_self31;
 FILE1 ret_val76;
 FILE1 r31;
 INT L51 = INT_zero;
 IDENT is_eq_self6 = IDENT_zero;
 IDENT is_eq_i6 = IDENT_zero;
 BOOL ret_val77 = BOOL_zero;
 OUT create_self8;
 OUT ret_val78;
 OUT plus_self32;
 STR plus_s32;
 FILE1 stdout_self32;
 FILE1 ret_val79;
 FILE1 r32;
 OUT create_self9;
 OUT ret_val80;
 OUT plus_self33;
 STR plus_s33;
 FILE1 stdout_self33;
 FILE1 ret_val81;
 FILE1 r33;
 OUT create_self10;
 OUT ret_val82;
 OUT plus_self34;
 STR plus_s34;
 OUT ret_val83;
 FILE1 stdout_self34;
 FILE1 ret_val84;
 FILE1 r34;
 OUT plus_self35;
 STR plus_s35;
 OUT ret_val85;
 FILE1 stdout_self35;
 FILE1 ret_val86;
 FILE1 r35;
 OUT plus_self36;
 STR plus_s36;
 FILE1 stdout_self36;
 FILE1 ret_val87;
 FILE1 r36;
 dAM L6;
 BOOL L7;
 dTP L8;
 extern STR therei939610693;
 FILE1 L9;
 OB L10;
 dTP L13;
 FILE1 L14;
 OB L15;
 extern STR name34;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR namely1;
 FILE1 L24;
 OB L25;
 dAM_STMT L27;
 dAM_STMT L28;
 dAM_STMT L29;
 BOOL L30;
 dTP L32;
 extern STR therei939610693;
 FILE1 L33;
 OB L34;
 dTP L36;
 FILE1 L37;
 OB L38;
 extern STR name34;
 FILE1 L40;
 OB L42;
 FILE1 L44;
 OB L45;
 extern STR namely1;
 FILE1 L47;
 OB L48;
 dAM_STMT L50;
 dAM_STMT L52;
 dAM_STMT L53;
 BOOL L54;
 dTP L55;
 extern STR therei939610693;
 FILE1 L56;
 OB L57;
 dTP L59;
 FILE1 L60;
 OB L61;
 extern STR name34;
 FILE1 L63;
 OB L64;
 FILE1 L66;
 OB L67;
 extern STR namely1;
 FILE1 L69;
 OB L70;
 dAM_STMT L72;
 dAM_STMT L73;
 dAM_STMT L74;
 BOOL L75;
 dTP L76;
 extern STR therei939610693;
 FILE1 L77;
 OB L78;
 dTP L80;
 FILE1 L81;
 OB L82;
 extern STR name34;
 FILE1 L84;
 OB L85;
 FILE1 L87;
 OB L88;
 extern STR namely1;
 FILE1 L90;
 OB L91;
 dAM_STMT L93;
 dAM_STMT L94;
 dAM_STMT L95;
 BOOL L96;
 dTP L97;
 extern STR therei939610693;
 FILE1 L98;
 OB L99;
 dTP L101;
 FILE1 L102;
 OB L103;
 extern STR name34;
 FILE1 L105;
 OB L106;
 FILE1 L108;
 OB L109;
 extern STR namely1;
 FILE1 L111;
 OB L112;
 dAM_STMT L114;
 dAM_STMT L115;
 dAM_STMT L116;
 BOOL L117;
 BOOL L118;
 SIDE_EFFECT L119;
 OB L120;
 BOOL L122;
 BOOL L123;
 BOOL L1241_;
 INT L1251_br;
 AM_CALL_ARG aL1251_;
 AM_CALL_ARG L126;
 BOOL L127;
 dMODE L128;
 dMODE L129;
 BOOL L130;
 dTP L131;
 INT L1321_;
 BOOL L133;
 BOOL L134;
 SIDE_EFFECT L135;
 OB L136;
 BOOL L137;
 BOOL L138;
 BOOL L1391_;
 extern STR iter2;
 FILE1 L140;
 OB L141;
 FILE1 L143;
 OB L144;
 extern STR writestotype;
 FILE1 L146;
 OB L147;
 dTP L149;
 FILE1 L150;
 OB L151;
 extern STR name34;
 FILE1 L153;
 OB L154;
 FILE1 L156;
 OB L157;
 extern STR name9;
 FILE1 L159;
 OB L160;
 INT L1631_br;
 AM_CALL_ARG aL1631_;
 AM_CALL_ARG L164;
 BOOL L165;
 dMODE L166;
 dMODE L167;
 BOOL L168;
 dTP L169;
 INT L1701_;
 extern STR notcon1658466928;
 FILE1 L171;
 OB L172;
 extern STR notcon647555960;
 FILE1 L174;
 OB L175;
 extern STR notcon1880191200;
 FILE1 L177;
 OB L178;
 FILE1 L180;
 OB L181;
 extern STR name9;
 FILE1 L183;
 OB L184;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c11;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L6 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L6;
   if (AM_CUR1885691330(c11, self)) {
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other247;
   } else
   switch (TAG(t)) {
    case AM_WAITFOR_STMT_tag:
     d = ATTR(((AM_WAITFOR_STMT) t),dest);
     if (d==NULL) {
      goto other248;
     } else
     switch (TAG(d)) {
      case AM_SHARED_EXPR_tag:
       L8 = ATTR(((AM_SHARED_EXPR) d),class_tp);
       if ((*dTP_is242312711[TAG(L8)])(L8, ((OB) tp))) {
        is_eq_self = ATTR(((AM_SHARED_EXPR) d),name1);
        is_eq_i = name111;
        ret_val1 = (is_eq_self.str==is_eq_i.str);
        L7 = ret_val1;
       } else {
        L7 = FALSE;
       }
       if (L7) {
        if (AM_CURSOR_debug) {
         create_self = ((OUT) NULL);
         ret_val2 = create_self;
         plus_self = ret_val2;
         plus_s = ((STR) &therei939610693);
         stdout_self = ((FILE1) NULL);
         L10=ZALLOC(sizeof(struct FILE1_struct));
         if (L10==NULL) FATAL("Unable to allocate more memory");
         ((OB)L10)->header.tag=FILE1_tag;
         L9 = ((FILE1) L10);
         r = L9;
         
         SATTR(r,fp,stdout);
         ret_val4 = r;
         FILE_plus_STR(ret_val4, plus_s);
         ret_val3 = plus_self;
         plus_self1 = ret_val3;
         L13 = tp;
         plus_s1 = (*dTP_strrSTR[TAG(L13)])(L13);
         stdout_self1 = ((FILE1) NULL);
         L15=ZALLOC(sizeof(struct FILE1_struct));
         if (L15==NULL) FATAL("Unable to allocate more memory");
         ((OB)L15)->header.tag=FILE1_tag;
         L14 = ((FILE1) L15);
         r1 = L14;
         
         SATTR(r1,fp,stdout);
         ret_val6 = r1;
         FILE_plus_STR(ret_val6, plus_s1);
         ret_val5 = plus_self1;
         plus_self2 = ret_val5;
         plus_s2 = ((STR) &name34);
         stdout_self2 = ((FILE1) NULL);
         L18=ZALLOC(sizeof(struct FILE1_struct));
         if (L18==NULL) FATAL("Unable to allocate more memory");
         ((OB)L18)->header.tag=FILE1_tag;
         L17 = ((FILE1) L18);
         r2 = L17;
         
         SATTR(r2,fp,stdout);
         ret_val8 = r2;
         FILE_plus_STR(ret_val8, plus_s2);
         ret_val7 = plus_self2;
         plus_self3 = ret_val7;
         plus_s3 = name111.str;
         stdout_self3 = ((FILE1) NULL);
         L22=ZALLOC(sizeof(struct FILE1_struct));
         if (L22==NULL) FATAL("Unable to allocate more memory");
         ((OB)L22)->header.tag=FILE1_tag;
         L20 = ((FILE1) L22);
         r3 = L20;
         
         SATTR(r3,fp,stdout);
         ret_val10 = r3;
         FILE_plus_STR(ret_val10, plus_s3);
         ret_val9 = plus_self3;
         plus_self4 = ret_val9;
         plus_s4 = ((STR) &namely1);
         stdout_self4 = ((FILE1) NULL);
         L25=ZALLOC(sizeof(struct FILE1_struct));
         if (L25==NULL) FATAL("Unable to allocate more memory");
         ((OB)L25)->header.tag=FILE1_tag;
         L24 = ((FILE1) L25);
         r4 = L24;
         
         SATTR(r4,fp,stdout);
         ret_val11 = r4;
         FILE_plus_STR(ret_val11, plus_s4);
         AM_one_stmt_self = ((AM_OUT) NULL);
         AM_one_stmt_a = t;
         switch (TAG(AM_one_stmt_a)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L27 = ((dAM_STMT) AM_one_stmt_a);
           n = (*dAM_ST775224435[TAG(L27)])(L27);
           L28 = ((dAM_STMT) AM_one_stmt_a);
           (*dAM_ST1372701974[TAG(L28)])(L28, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
           L29 = ((dAM_STMT) AM_one_stmt_a);
           (*dAM_ST1372701974[TAG(L29)])(L29, n); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other248: ;
     }
     d = ((dAM_EXPR) ATTR(((AM_WAITFOR_STMT) t),prefetch));
     if (d==NULL) {
      goto other249;
     } else
     switch (TAG(d)) {
      case AM_SHARED_EXPR_tag:
       L32 = ATTR(((AM_SHARED_EXPR) d),class_tp);
       if ((*dTP_is242312711[TAG(L32)])(L32, ((OB) tp))) {
        is_eq_self1 = ATTR(((AM_SHARED_EXPR) d),name1);
        is_eq_i1 = name111;
        ret_val12 = (is_eq_self1.str==is_eq_i1.str);
        L30 = ret_val12;
       } else {
        L30 = FALSE;
       }
       if (L30) {
        if (AM_CURSOR_debug) {
         create_self1 = ((OUT) NULL);
         ret_val13 = create_self1;
         plus_self5 = ret_val13;
         plus_s5 = ((STR) &therei939610693);
         stdout_self5 = ((FILE1) NULL);
         L34=ZALLOC(sizeof(struct FILE1_struct));
         if (L34==NULL) FATAL("Unable to allocate more memory");
         ((OB)L34)->header.tag=FILE1_tag;
         L33 = ((FILE1) L34);
         r5 = L33;
         
         SATTR(r5,fp,stdout);
         ret_val15 = r5;
         FILE_plus_STR(ret_val15, plus_s5);
         ret_val14 = plus_self5;
         plus_self6 = ret_val14;
         L36 = tp;
         plus_s6 = (*dTP_strrSTR[TAG(L36)])(L36);
         stdout_self6 = ((FILE1) NULL);
         L38=ZALLOC(sizeof(struct FILE1_struct));
         if (L38==NULL) FATAL("Unable to allocate more memory");
         ((OB)L38)->header.tag=FILE1_tag;
         L37 = ((FILE1) L38);
         r6 = L37;
         
         SATTR(r6,fp,stdout);
         ret_val17 = r6;
         FILE_plus_STR(ret_val17, plus_s6);
         ret_val16 = plus_self6;
         plus_self7 = ret_val16;
         plus_s7 = ((STR) &name34);
         stdout_self7 = ((FILE1) NULL);
         L42=ZALLOC(sizeof(struct FILE1_struct));
         if (L42==NULL) FATAL("Unable to allocate more memory");
         ((OB)L42)->header.tag=FILE1_tag;
         L40 = ((FILE1) L42);
         r7 = L40;
         
         SATTR(r7,fp,stdout);
         ret_val19 = r7;
         FILE_plus_STR(ret_val19, plus_s7);
         ret_val18 = plus_self7;
         plus_self8 = ret_val18;
         plus_s8 = name111.str;
         stdout_self8 = ((FILE1) NULL);
         L45=ZALLOC(sizeof(struct FILE1_struct));
         if (L45==NULL) FATAL("Unable to allocate more memory");
         ((OB)L45)->header.tag=FILE1_tag;
         L44 = ((FILE1) L45);
         r8 = L44;
         
         SATTR(r8,fp,stdout);
         ret_val21 = r8;
         FILE_plus_STR(ret_val21, plus_s8);
         ret_val20 = plus_self8;
         plus_self9 = ret_val20;
         plus_s9 = ((STR) &namely1);
         stdout_self9 = ((FILE1) NULL);
         L48=ZALLOC(sizeof(struct FILE1_struct));
         if (L48==NULL) FATAL("Unable to allocate more memory");
         ((OB)L48)->header.tag=FILE1_tag;
         L47 = ((FILE1) L48);
         r9 = L47;
         
         SATTR(r9,fp,stdout);
         ret_val22 = r9;
         FILE_plus_STR(ret_val22, plus_s9);
         AM_one_stmt_self1 = ((AM_OUT) NULL);
         AM_one_stmt_a1 = t;
         switch (TAG(AM_one_stmt_a1)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L50 = ((dAM_STMT) AM_one_stmt_a1);
           n11 = (*dAM_ST775224435[TAG(L50)])(L50);
           L52 = ((dAM_STMT) AM_one_stmt_a1);
           (*dAM_ST1372701974[TAG(L52)])(L52, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
           L53 = ((dAM_STMT) AM_one_stmt_a1);
           (*dAM_ST1372701974[TAG(L53)])(L53, n11); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other249: ;
     } break;
    case AM_PREFETCH_STMT_tag:
     d11 = ATTR(((AM_PREFETCH_STMT) t),dest);
     if (d11==NULL) {
      goto other250;
     } else
     switch (TAG(d11)) {
      case AM_SHARED_EXPR_tag:
       L55 = ATTR(((AM_SHARED_EXPR) d11),class_tp);
       if ((*dTP_is242312711[TAG(L55)])(L55, ((OB) tp))) {
        is_eq_self2 = ATTR(((AM_SHARED_EXPR) d11),name1);
        is_eq_i2 = name111;
        ret_val23 = (is_eq_self2.str==is_eq_i2.str);
        L54 = ret_val23;
       } else {
        L54 = FALSE;
       }
       if (L54) {
        if (AM_CURSOR_debug) {
         create_self2 = ((OUT) NULL);
         ret_val24 = create_self2;
         plus_self10 = ret_val24;
         plus_s10 = ((STR) &therei939610693);
         stdout_self10 = ((FILE1) NULL);
         L57=ZALLOC(sizeof(struct FILE1_struct));
         if (L57==NULL) FATAL("Unable to allocate more memory");
         ((OB)L57)->header.tag=FILE1_tag;
         L56 = ((FILE1) L57);
         r10 = L56;
         
         SATTR(r10,fp,stdout);
         ret_val26 = r10;
         FILE_plus_STR(ret_val26, plus_s10);
         ret_val25 = plus_self10;
         plus_self11 = ret_val25;
         L59 = tp;
         plus_s11 = (*dTP_strrSTR[TAG(L59)])(L59);
         stdout_self11 = ((FILE1) NULL);
         L61=ZALLOC(sizeof(struct FILE1_struct));
         if (L61==NULL) FATAL("Unable to allocate more memory");
         ((OB)L61)->header.tag=FILE1_tag;
         L60 = ((FILE1) L61);
         r11 = L60;
         
         SATTR(r11,fp,stdout);
         ret_val28 = r11;
         FILE_plus_STR(ret_val28, plus_s11);
         ret_val27 = plus_self11;
         plus_self12 = ret_val27;
         plus_s12 = ((STR) &name34);
         stdout_self12 = ((FILE1) NULL);
         L64=ZALLOC(sizeof(struct FILE1_struct));
         if (L64==NULL) FATAL("Unable to allocate more memory");
         ((OB)L64)->header.tag=FILE1_tag;
         L63 = ((FILE1) L64);
         r12 = L63;
         
         SATTR(r12,fp,stdout);
         ret_val30 = r12;
         FILE_plus_STR(ret_val30, plus_s12);
         ret_val29 = plus_self12;
         plus_self13 = ret_val29;
         plus_s13 = name111.str;
         stdout_self13 = ((FILE1) NULL);
         L67=ZALLOC(sizeof(struct FILE1_struct));
         if (L67==NULL) FATAL("Unable to allocate more memory");
         ((OB)L67)->header.tag=FILE1_tag;
         L66 = ((FILE1) L67);
         r13 = L66;
         
         SATTR(r13,fp,stdout);
         ret_val32 = r13;
         FILE_plus_STR(ret_val32, plus_s13);
         ret_val31 = plus_self13;
         plus_self14 = ret_val31;
         plus_s14 = ((STR) &namely1);
         stdout_self14 = ((FILE1) NULL);
         L70=ZALLOC(sizeof(struct FILE1_struct));
         if (L70==NULL) FATAL("Unable to allocate more memory");
         ((OB)L70)->header.tag=FILE1_tag;
         L69 = ((FILE1) L70);
         r14 = L69;
         
         SATTR(r14,fp,stdout);
         ret_val33 = r14;
         FILE_plus_STR(ret_val33, plus_s14);
         AM_one_stmt_self2 = ((AM_OUT) NULL);
         AM_one_stmt_a2 = t;
         switch (TAG(AM_one_stmt_a2)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L72 = ((dAM_STMT) AM_one_stmt_a2);
           n2 = (*dAM_ST775224435[TAG(L72)])(L72);
           L73 = ((dAM_STMT) AM_one_stmt_a2);
           (*dAM_ST1372701974[TAG(L73)])(L73, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
           L74 = ((dAM_STMT) AM_one_stmt_a2);
           (*dAM_ST1372701974[TAG(L74)])(L74, n2); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other250: ;
     }
     d11 = ((dAM_EXPR) ATTR(((AM_PREFETCH_STMT) t),prefetch));
     if (d11==NULL) {
      goto other251;
     } else
     switch (TAG(d11)) {
      case AM_SHARED_EXPR_tag:
       L76 = ATTR(((AM_SHARED_EXPR) d11),class_tp);
       if ((*dTP_is242312711[TAG(L76)])(L76, ((OB) tp))) {
        is_eq_self3 = ATTR(((AM_SHARED_EXPR) d11),name1);
        is_eq_i3 = name111;
        ret_val34 = (is_eq_self3.str==is_eq_i3.str);
        L75 = ret_val34;
       } else {
        L75 = FALSE;
       }
       if (L75) {
        if (AM_CURSOR_debug) {
         create_self3 = ((OUT) NULL);
         ret_val35 = create_self3;
         plus_self15 = ret_val35;
         plus_s15 = ((STR) &therei939610693);
         stdout_self15 = ((FILE1) NULL);
         L78=ZALLOC(sizeof(struct FILE1_struct));
         if (L78==NULL) FATAL("Unable to allocate more memory");
         ((OB)L78)->header.tag=FILE1_tag;
         L77 = ((FILE1) L78);
         r15 = L77;
         
         SATTR(r15,fp,stdout);
         ret_val37 = r15;
         FILE_plus_STR(ret_val37, plus_s15);
         ret_val36 = plus_self15;
         plus_self16 = ret_val36;
         L80 = tp;
         plus_s16 = (*dTP_strrSTR[TAG(L80)])(L80);
         stdout_self16 = ((FILE1) NULL);
         L82=ZALLOC(sizeof(struct FILE1_struct));
         if (L82==NULL) FATAL("Unable to allocate more memory");
         ((OB)L82)->header.tag=FILE1_tag;
         L81 = ((FILE1) L82);
         r16 = L81;
         
         SATTR(r16,fp,stdout);
         ret_val39 = r16;
         FILE_plus_STR(ret_val39, plus_s16);
         ret_val38 = plus_self16;
         plus_self17 = ret_val38;
         plus_s17 = ((STR) &name34);
         stdout_self17 = ((FILE1) NULL);
         L85=ZALLOC(sizeof(struct FILE1_struct));
         if (L85==NULL) FATAL("Unable to allocate more memory");
         ((OB)L85)->header.tag=FILE1_tag;
         L84 = ((FILE1) L85);
         r17 = L84;
         
         SATTR(r17,fp,stdout);
         ret_val41 = r17;
         FILE_plus_STR(ret_val41, plus_s17);
         ret_val40 = plus_self17;
         plus_self18 = ret_val40;
         plus_s18 = name111.str;
         stdout_self18 = ((FILE1) NULL);
         L88=ZALLOC(sizeof(struct FILE1_struct));
         if (L88==NULL) FATAL("Unable to allocate more memory");
         ((OB)L88)->header.tag=FILE1_tag;
         L87 = ((FILE1) L88);
         r18 = L87;
         
         SATTR(r18,fp,stdout);
         ret_val43 = r18;
         FILE_plus_STR(ret_val43, plus_s18);
         ret_val42 = plus_self18;
         plus_self19 = ret_val42;
         plus_s19 = ((STR) &namely1);
         stdout_self19 = ((FILE1) NULL);
         L91=ZALLOC(sizeof(struct FILE1_struct));
         if (L91==NULL) FATAL("Unable to allocate more memory");
         ((OB)L91)->header.tag=FILE1_tag;
         L90 = ((FILE1) L91);
         r19 = L90;
         
         SATTR(r19,fp,stdout);
         ret_val44 = r19;
         FILE_plus_STR(ret_val44, plus_s19);
         AM_one_stmt_self3 = ((AM_OUT) NULL);
         AM_one_stmt_a3 = t;
         switch (TAG(AM_one_stmt_a3)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L93 = ((dAM_STMT) AM_one_stmt_a3);
           n3 = (*dAM_ST775224435[TAG(L93)])(L93);
           L94 = ((dAM_STMT) AM_one_stmt_a3);
           (*dAM_ST1372701974[TAG(L94)])(L94, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
           L95 = ((dAM_STMT) AM_one_stmt_a3);
           (*dAM_ST1372701974[TAG(L95)])(L95, n3); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other251: ;
     } break;
    case AM_ASSIGN_STMT_tag:
     d21 = ATTR(((AM_ASSIGN_STMT) t),dest);
     if (d21==NULL) {
      goto other252;
     } else
     switch (TAG(d21)) {
      case AM_SHARED_EXPR_tag:
       L97 = ATTR(((AM_SHARED_EXPR) d21),class_tp);
       if ((*dTP_is242312711[TAG(L97)])(L97, ((OB) tp))) {
        is_eq_self4 = ATTR(((AM_SHARED_EXPR) d21),name1);
        is_eq_i4 = name111;
        ret_val45 = (is_eq_self4.str==is_eq_i4.str);
        L96 = ret_val45;
       } else {
        L96 = FALSE;
       }
       if (L96) {
        if (AM_CURSOR_debug) {
         create_self4 = ((OUT) NULL);
         ret_val46 = create_self4;
         plus_self20 = ret_val46;
         plus_s20 = ((STR) &therei939610693);
         stdout_self20 = ((FILE1) NULL);
         L99=ZALLOC(sizeof(struct FILE1_struct));
         if (L99==NULL) FATAL("Unable to allocate more memory");
         ((OB)L99)->header.tag=FILE1_tag;
         L98 = ((FILE1) L99);
         r20 = L98;
         
         SATTR(r20,fp,stdout);
         ret_val48 = r20;
         FILE_plus_STR(ret_val48, plus_s20);
         ret_val47 = plus_self20;
         plus_self21 = ret_val47;
         L101 = tp;
         plus_s21 = (*dTP_strrSTR[TAG(L101)])(L101);
         stdout_self21 = ((FILE1) NULL);
         L103=ZALLOC(sizeof(struct FILE1_struct));
         if (L103==NULL) FATAL("Unable to allocate more memory");
         ((OB)L103)->header.tag=FILE1_tag;
         L102 = ((FILE1) L103);
         r21 = L102;
         
         SATTR(r21,fp,stdout);
         ret_val50 = r21;
         FILE_plus_STR(ret_val50, plus_s21);
         ret_val49 = plus_self21;
         plus_self22 = ret_val49;
         plus_s22 = ((STR) &name34);
         stdout_self22 = ((FILE1) NULL);
         L106=ZALLOC(sizeof(struct FILE1_struct));
         if (L106==NULL) FATAL("Unable to allocate more memory");
         ((OB)L106)->header.tag=FILE1_tag;
         L105 = ((FILE1) L106);
         r22 = L105;
         
         SATTR(r22,fp,stdout);
         ret_val52 = r22;
         FILE_plus_STR(ret_val52, plus_s22);
         ret_val51 = plus_self22;
         plus_self23 = ret_val51;
         plus_s23 = name111.str;
         stdout_self23 = ((FILE1) NULL);
         L109=ZALLOC(sizeof(struct FILE1_struct));
         if (L109==NULL) FATAL("Unable to allocate more memory");
         ((OB)L109)->header.tag=FILE1_tag;
         L108 = ((FILE1) L109);
         r23 = L108;
         
         SATTR(r23,fp,stdout);
         ret_val54 = r23;
         FILE_plus_STR(ret_val54, plus_s23);
         ret_val53 = plus_self23;
         plus_self24 = ret_val53;
         plus_s24 = ((STR) &namely1);
         stdout_self24 = ((FILE1) NULL);
         L112=ZALLOC(sizeof(struct FILE1_struct));
         if (L112==NULL) FATAL("Unable to allocate more memory");
         ((OB)L112)->header.tag=FILE1_tag;
         L111 = ((FILE1) L112);
         r24 = L111;
         
         SATTR(r24,fp,stdout);
         ret_val55 = r24;
         FILE_plus_STR(ret_val55, plus_s24);
         AM_one_stmt_self4 = ((AM_OUT) NULL);
         AM_one_stmt_a4 = t;
         switch (TAG(AM_one_stmt_a4)) {
          case AM_ASSERT_STMT_tag:
          case AM_ASSIGN_STMT_tag:
          case AM_ATTACH_STMT_tag:
          case AM_BREAK_STMT_tag:
          case AM_CASE_STMT_tag:
          case AM_COMMENT_STMT_tag:
          case AM_EXPR_STMT_tag:
          case AM_FORK_STMT_tag:
          case AM_IF_STMT_tag:
          case AM_INITIAL_STMT_tag:
          case AM_INV1167837230_tag:
          case AM_LOCK_STMT_tag:
          case AM_LOOP_STMT_tag:
          case AM_PAR_STMT_tag:
          case AM_POST_STMT_tag:
          case AM_PREFETCH_STMT_tag:
          case AM_PRE_STMT_tag:
          case AM_PROTECT_STMT_tag:
          case AM_RAISE_STMT_tag:
          case AM_RETURN_STMT_tag:
          case AM_SYNC_STMT_tag:
          case AM_TYPECASE_STMT_tag:
          case AM_UNLOCK_STMT_tag:
          case AM_WAITFOR_STMT_tag:
          case AM_WIT1996971603_tag:
          case AM_YIELD_STMT_tag:
           L114 = ((dAM_STMT) AM_one_stmt_a4);
           n4 = (*dAM_ST775224435[TAG(L114)])(L114);
           L115 = ((dAM_STMT) AM_one_stmt_a4);
           (*dAM_ST1372701974[TAG(L115)])(L115, ((dAM_STMT) NULL));
           AM_OUT1439666796(AM_one_stmt_self4, AM_one_stmt_a4);
           L116 = ((dAM_STMT) AM_one_stmt_a4);
           (*dAM_ST1372701974[TAG(L116)])(L116, n4); break;
          default: ;
           AM_OUT1439666796(AM_one_stmt_self4, AM_one_stmt_a4);
         }
        }
        ret_val = FALSE;
        return ret_val;
       } break;
      default: ;
      other252: ;
     } break;
    case AM_ROU1916046290_tag:
     if (ATTR(self,with_side_effect)) {
      c = SIG_ge642497605(ATTR(((AM_ROU1916046290) t),fun), ATTR(self,prog));
      full_self = c;
      if (ATTR(full_self,is_full)) {
       L118 = TRUE;
      } else {
       L118 = ATTR(full_self,has_import);
      }
      if (L118) {
       L117 = TRUE;
      } else {
       L117 = ATTR(full_self,has_export);
      }
      ret_val56 = L117;
      if (ret_val56) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self = c;
      se_attr_tp = tp;
      se_attr_name = name111;
      create_self5 = ((SIDE_EFFECT) NULL);
      L120=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L120==NULL) FATAL("Unable to allocate more memory");
      ((OB)L120)->header.tag=SIDE_EFFECT_tag;
      L119 = ((SIDE_EFFECT) L120);
      ret_val58 = L119;
      s = ret_val58;
      SATTR(s,tp,se_attr_tp);
      SATTR(s,name1,se_attr_name);
      ret_val57 = FSETSI937439554(ATTR(se_attr_self,set), s);
      se = ret_val57;
      L123 = (se==((SIDE_EFFECT) NULL));
      L1241_=!(L123); 
      if (L1241_) {
       L122 = ATTR(se,doeswrite);
      } else {
       L122 = FALSE;
      }
      if (L122) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     }
     {
      BOOL f_L1251_ = TRUE;
      /* loop index variable */
      L41 = 0;
      L21 = ((AM_ROU1916046290) t);
      L1251_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
      while (1) {
       if(L41>=L1251_br)  goto after_loop1; 
       aL1251_=ARR((AM_ROU1916046290)L21,L41); 
       a = aL1251_;
       L128 = ATTR(a,mode1);
       if ((*dMODE_814247358[TAG(L128)])(L128, ((dMODE) MODES_inout_mode))) {
        L127 = TRUE;
       } else {
        L129 = ATTR(a,mode1);
        L127 = (*dMODE_814247358[TAG(L129)])(L129, ((dMODE) MODES_out_mode));
       }
       if (L127) {
        e = ATTR(a,expr);
        if (e==NULL) {
         goto other253;
        } else
        switch (TAG(e)) {
         case AM_SHARED_EXPR_tag:
          L131 = ATTR(((AM_SHARED_EXPR) e),class_tp);
          if ((*dTP_is242312711[TAG(L131)])(L131, ((OB) tp))) {
           is_eq_self5 = ATTR(((AM_SHARED_EXPR) e),name1);
           is_eq_i5 = name111;
           ret_val59 = (is_eq_self5.str==is_eq_i5.str);
           L130 = ret_val59;
          } else {
           L130 = FALSE;
          }
          if (L130) {
           ret_val = FALSE;
           return ret_val;
          } break;
         default: ;
         other253: ;
        }
       }
       L1321_=INTPLUS(L41,1); 
       L41 = L1321_;
      }
     }
     after_loop1: ; break;
    case AM_ITE1809135850_tag:
     if (ATTR(self,with_side_effect)) {
      c12 = SIG_ge642497605(ATTR(((AM_ITE1809135850) t),fun), ATTR(self,prog));
      full_self1 = c12;
      if (ATTR(full_self1,is_full)) {
       L134 = TRUE;
      } else {
       L134 = ATTR(full_self1,has_import);
      }
      if (L134) {
       L133 = TRUE;
      } else {
       L133 = ATTR(full_self1,has_export);
      }
      ret_val60 = L133;
      if (ret_val60) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self1 = c12;
      se_attr_tp1 = tp;
      se_attr_name1 = name111;
      create_self6 = ((SIDE_EFFECT) NULL);
      L136=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L136==NULL) FATAL("Unable to allocate more memory");
      ((OB)L136)->header.tag=SIDE_EFFECT_tag;
      L135 = ((SIDE_EFFECT) L136);
      ret_val62 = L135;
      s1 = ret_val62;
      SATTR(s1,tp,se_attr_tp1);
      SATTR(s1,name1,se_attr_name1);
      ret_val61 = FSETSI937439554(ATTR(se_attr_self1,set), s1);
      se1 = ret_val61;
      L138 = (se1==((SIDE_EFFECT) NULL));
      L1391_=!(L138); 
      if (L1391_) {
       L137 = ATTR(se1,doeswrite);
      } else {
       L137 = FALSE;
      }
      if (L137) {
       if (AM_CURSOR_debug) {
        create_self7 = ((OUT) NULL);
        ret_val63 = create_self7;
        plus_self25 = ret_val63;
        plus_s25 = ((STR) &iter2);
        stdout_self25 = ((FILE1) NULL);
        L141=ZALLOC(sizeof(struct FILE1_struct));
        if (L141==NULL) FATAL("Unable to allocate more memory");
        ((OB)L141)->header.tag=FILE1_tag;
        L140 = ((FILE1) L141);
        r25 = L140;
        
        SATTR(r25,fp,stdout);
        ret_val65 = r25;
        FILE_plus_STR(ret_val65, plus_s25);
        ret_val64 = plus_self25;
        plus_self26 = ret_val64;
        plus_s26 = ATTR(ATTR(((AM_ITE1809135850) t),fun),str);
        stdout_self26 = ((FILE1) NULL);
        L144=ZALLOC(sizeof(struct FILE1_struct));
        if (L144==NULL) FATAL("Unable to allocate more memory");
        ((OB)L144)->header.tag=FILE1_tag;
        L143 = ((FILE1) L144);
        r26 = L143;
        
        SATTR(r26,fp,stdout);
        ret_val67 = r26;
        FILE_plus_STR(ret_val67, plus_s26);
        ret_val66 = plus_self26;
        plus_self27 = ret_val66;
        plus_s27 = ((STR) &writestotype);
        stdout_self27 = ((FILE1) NULL);
        L147=ZALLOC(sizeof(struct FILE1_struct));
        if (L147==NULL) FATAL("Unable to allocate more memory");
        ((OB)L147)->header.tag=FILE1_tag;
        L146 = ((FILE1) L147);
        r27 = L146;
        
        SATTR(r27,fp,stdout);
        ret_val69 = r27;
        FILE_plus_STR(ret_val69, plus_s27);
        ret_val68 = plus_self27;
        plus_self28 = ret_val68;
        L149 = tp;
        plus_s28 = (*dTP_strrSTR[TAG(L149)])(L149);
        stdout_self28 = ((FILE1) NULL);
        L151=ZALLOC(sizeof(struct FILE1_struct));
        if (L151==NULL) FATAL("Unable to allocate more memory");
        ((OB)L151)->header.tag=FILE1_tag;
        L150 = ((FILE1) L151);
        r28 = L150;
        
        SATTR(r28,fp,stdout);
        ret_val71 = r28;
        FILE_plus_STR(ret_val71, plus_s28);
        ret_val70 = plus_self28;
        plus_self29 = ret_val70;
        plus_s29 = ((STR) &name34);
        stdout_self29 = ((FILE1) NULL);
        L154=ZALLOC(sizeof(struct FILE1_struct));
        if (L154==NULL) FATAL("Unable to allocate more memory");
        ((OB)L154)->header.tag=FILE1_tag;
        L153 = ((FILE1) L154);
        r29 = L153;
        
        SATTR(r29,fp,stdout);
        ret_val73 = r29;
        FILE_plus_STR(ret_val73, plus_s29);
        ret_val72 = plus_self29;
        plus_self30 = ret_val72;
        plus_s30 = name111.str;
        stdout_self30 = ((FILE1) NULL);
        L157=ZALLOC(sizeof(struct FILE1_struct));
        if (L157==NULL) FATAL("Unable to allocate more memory");
        ((OB)L157)->header.tag=FILE1_tag;
        L156 = ((FILE1) L157);
        r30 = L156;
        
        SATTR(r30,fp,stdout);
        ret_val75 = r30;
        FILE_plus_STR(ret_val75, plus_s30);
        ret_val74 = plus_self30;
        plus_self31 = ret_val74;
        plus_s31 = ((STR) &name9);
        stdout_self31 = ((FILE1) NULL);
        L160=ZALLOC(sizeof(struct FILE1_struct));
        if (L160==NULL) FATAL("Unable to allocate more memory");
        ((OB)L160)->header.tag=FILE1_tag;
        L159 = ((FILE1) L160);
        r31 = L159;
        
        SATTR(r31,fp,stdout);
        ret_val76 = r31;
        FILE_plus_STR(ret_val76, plus_s31);
       }
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     }
     {
      BOOL f_L1631_ = TRUE;
      /* loop index variable */
      L51 = 0;
      L31 = ((AM_ITE1809135850) t);
      L1631_br=L31==NULL?0:ASIZE((AM_ITE1809135850)L31); 
      while (1) {
       if(L51>=L1631_br)  goto after_loop2; 
       aL1631_=ARR((AM_ITE1809135850)L31,L51); 
       a11 = aL1631_;
       L166 = ATTR(a11,mode1);
       if ((*dMODE_814247358[TAG(L166)])(L166, ((dMODE) MODES_inout_mode))) {
        L165 = TRUE;
       } else {
        L167 = ATTR(a11,mode1);
        L165 = (*dMODE_814247358[TAG(L167)])(L167, ((dMODE) MODES_out_mode));
       }
       if (L165) {
        e1 = ATTR(a11,expr);
        if (e1==NULL) {
         goto other254;
        } else
        switch (TAG(e1)) {
         case AM_SHARED_EXPR_tag:
          L169 = ATTR(((AM_SHARED_EXPR) e1),class_tp);
          if ((*dTP_is242312711[TAG(L169)])(L169, ((OB) tp))) {
           is_eq_self6 = ATTR(((AM_SHARED_EXPR) e1),name1);
           is_eq_i6 = name111;
           ret_val77 = (is_eq_self6.str==is_eq_i6.str);
           L168 = ret_val77;
          } else {
           L168 = FALSE;
          }
          if (L168) {
           ret_val = FALSE;
           return ret_val;
          } break;
         default: ;
         other254: ;
        }
       }
       L1701_=INTPLUS(L51,1); 
       L51 = L1701_;
      }
     }
     after_loop2: ; break;
    case AM_BND260301329_tag:
     if (AM_CURSOR_debug) {
      create_self8 = ((OUT) NULL);
      ret_val78 = create_self8;
      plus_self32 = ret_val78;
      plus_s32 = ((STR) &notcon1658466928);
      stdout_self32 = ((FILE1) NULL);
      L172=ZALLOC(sizeof(struct FILE1_struct));
      if (L172==NULL) FATAL("Unable to allocate more memory");
      ((OB)L172)->header.tag=FILE1_tag;
      L171 = ((FILE1) L172);
      r32 = L171;
      
      SATTR(r32,fp,stdout);
      ret_val79 = r32;
      FILE_plus_STR(ret_val79, plus_s32);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_BND367211769_tag:
     if (AM_CURSOR_debug) {
      create_self9 = ((OUT) NULL);
      ret_val80 = create_self9;
      plus_self33 = ret_val80;
      plus_s33 = ((STR) &notcon647555960);
      stdout_self33 = ((FILE1) NULL);
      L175=ZALLOC(sizeof(struct FILE1_struct));
      if (L175==NULL) FATAL("Unable to allocate more memory");
      ((OB)L175)->header.tag=FILE1_tag;
      L174 = ((FILE1) L175);
      r33 = L174;
      
      SATTR(r33,fp,stdout);
      ret_val81 = r33;
      FILE_plus_STR(ret_val81, plus_s33);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_EXT_CALL_EXPR_tag:
     if (AM_CURSOR_debug) {
      create_self10 = ((OUT) NULL);
      ret_val82 = create_self10;
      plus_self34 = ret_val82;
      plus_s34 = ((STR) &notcon1880191200);
      stdout_self34 = ((FILE1) NULL);
      L178=ZALLOC(sizeof(struct FILE1_struct));
      if (L178==NULL) FATAL("Unable to allocate more memory");
      ((OB)L178)->header.tag=FILE1_tag;
      L177 = ((FILE1) L178);
      r34 = L177;
      
      SATTR(r34,fp,stdout);
      ret_val84 = r34;
      FILE_plus_STR(ret_val84, plus_s34);
      ret_val83 = plus_self34;
      plus_self35 = ret_val83;
      plus_s35 = ATTR(ATTR(((AM_EXT_CALL_EXPR) t),fun),str);
      stdout_self35 = ((FILE1) NULL);
      L181=ZALLOC(sizeof(struct FILE1_struct));
      if (L181==NULL) FATAL("Unable to allocate more memory");
      ((OB)L181)->header.tag=FILE1_tag;
      L180 = ((FILE1) L181);
      r35 = L180;
      
      SATTR(r35,fp,stdout);
      ret_val86 = r35;
      FILE_plus_STR(ret_val86, plus_s35);
      ret_val85 = plus_self35;
      plus_self36 = ret_val85;
      plus_s36 = ((STR) &name9);
      stdout_self36 = ((FILE1) NULL);
      L184=ZALLOC(sizeof(struct FILE1_struct));
      if (L184==NULL) FATAL("Unable to allocate more memory");
      ((OB)L184)->header.tag=FILE1_tag;
      L183 = ((FILE1) L184);
      r36 = L183;
      
      SATTR(r36,fp,stdout);
      ret_val87 = r36;
      FILE_plus_STR(ret_val87, plus_s36);
     }
     ret_val = FALSE;
     return ret_val; break;
    default: ;
    other247: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1926902782(AM_CURSOR self, AM_CURSOR c, dAM am) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 AM_CURSOR L2;
 if (am==NULL) {
  goto other234;
 } else
 switch (TAG(am)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = AM_CUR537174465(self, c, ((AM_LOCAL_EXPR) am));
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   L2 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
   if (AM_CUR898853280(self, L2, ATTR(((AM_ATTR_EXPR) am),self_tp), ATTR(((AM_ATTR_EXPR) am),at))) {
    L1 = AM_CUR1926902782(self, c, ((dAM) ATTR(((AM_ATTR_EXPR) am),ob)));
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = AM_CUR1587547281(self, c, ATTR(((AM_SHARED_EXPR) am),class_tp), ATTR(((AM_SHARED_EXPR) am),name1));
   return ret_val; break;
  default: ;
  other234: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR2013152412(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 dAM s;
 INT b1 = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val1;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val2;
 AM_CURSOR_POS L1;
 AM_CURSOR_POS L2;
 BOOL L31_;
 top_self = ATTR(self,stack1);
 ret_val1 = FLISTA860779230(ATTR(top_self,s));
 L1=ret_val1;
 s = ATTR(L1,stmt);
 top_self1 = ATTR(self,stack1);
 ret_val2 = FLISTA860779230(ATTR(top_self1,s));
 L2=ret_val2;
 b1 = ATTR(L2,branch);
 if (s==NULL) {
  goto other228;
 } else
 switch (TAG(s)) {
  case AM_AT_EXPR_tag:
   L31_=(b1)==(2); 
   if (L31_) {
    ret_val = TRUE;
    return ret_val;
   } break;
  default: ;
  other228: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR45133320(AM_CURSOR self, dAM am) {
 BOOL ret_val = BOOL_zero;
 SE_CONTEXT c;
 AM_ROU1916046290 L11;
 SE_CONTEXT size_self;
 INT ret_val1 = INT_zero;
 SE_CONTEXT full_self;
 BOOL ret_val2 = BOOL_zero;
 INT L21 = INT_zero;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L9;
 INT L101_br;
 AM_CALL_ARG aL101_;
 AM_CALL_ARG L12;
 AM_CALL_ARG L13;
 BOOL L14;
 BOOL L151_;
 INT L161_;
 if ((am==((dAM) NULL))) {
  ret_val = TRUE;
  return ret_val;
 }
 if (am==NULL) {
  goto other237;
 } else
 switch (TAG(am)) {
  case AM_ARR_CONST_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CONST_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_STR_CONST_tag:
  case AM_VOID_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_ROU1916046290_tag:
   if (ATTR(self,with_side_effect)) {
    c = SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ATTR(self,prog));
    size_self = c;
    ret_val1 = FSETSI2058428832(ATTR(size_self,set));
    L6 = ret_val1;
    L71_=(0)<(L6); 
    if (L71_) {
     L5 = TRUE;
    } else {
     full_self = c;
     if (ATTR(full_self,is_full)) {
      L9 = TRUE;
     } else {
      L9 = ATTR(full_self,has_import);
     }
     if (L9) {
      L8 = TRUE;
     } else {
      L8 = ATTR(full_self,has_export);
     }
     ret_val2 = L8;
     L5 = ret_val2;
    }
    if (L5) {
     L4 = TRUE;
    } else {
     L4 = ATTR(c,unsafe);
    }
    if (L4) {
     L3 = TRUE;
    } else {
     L3 = ATTR(c,has_raise);
    }
    if (L3) {
     ret_val = FALSE;
     return ret_val;
    }
    {
     BOOL f_L101_ = TRUE;
     /* loop index variable */
     L21 = 0;
     L11 = ((AM_ROU1916046290) am);
     L101_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
     while (1) {
      if(L21>=L101_br)  goto after_loop; 
      aL101_=ARR((AM_ROU1916046290)L11,L21); 
      L13=aL101_;
      L14 = AM_CUR45133320(self, ((dAM) ATTR(L13,expr)));
      L151_=!(L14); 
      if (L151_) {
       ret_val = FALSE;
       return ret_val;
      }
      L161_=INTPLUS(L21,1); 
      L21 = L161_;
     }
    }
    after_loop: ;
    ret_val = TRUE;
    return ret_val;
   }
   else {
    ret_val = FALSE;
    return ret_val;
   } break;
  case AM_SHARED_EXPR_tag:
   ret_val = ATTR(((AM_SHARED_EXPR) am),is_const);
   return ret_val; break;
  case AM_CLUSTER_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_HERE_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_CLU1286269335_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other237: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR537174465(AM_CURSOR self, AM_CURSOR c11, AM_LOCAL_EXPR am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val1 = BOOL_zero;
 dAM L2;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c11;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L2;
   if (AM_CUR1885691330(c11, self)) {
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other231;
   } else
   switch (TAG(t)) {
    case AM_LOCAL_EXPR_tag:
     is_eq_self = am;
     is_eq_a = ((OB) t);
     ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
     if (ret_val1) {
      ret_val = FALSE;
      return ret_val;
     } break;
    default: ;
    other231: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR898853280(AM_CURSOR self, AM_CURSOR c11, dTP tp, IDENT name111) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 SE_CONTEXT c;
 SIDE_EFFECT se;
 SE_CONTEXT c12;
 SIDE_EFFECT se1;
 AM_ATTR_EXPR tp_self;
 dTP ret_val1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 SE_CONTEXT full_self;
 BOOL ret_val3 = BOOL_zero;
 SE_CONTEXT se_attr_self;
 dTP se_attr_tp;
 IDENT se_attr_name = IDENT_zero;
 SIDE_EFFECT ret_val4;
 SIDE_EFFECT s;
 SIDE_EFFECT create_self;
 SIDE_EFFECT ret_val5;
 SE_CONTEXT full_self1;
 BOOL ret_val6 = BOOL_zero;
 SE_CONTEXT se_attr_self1;
 dTP se_attr_tp1;
 IDENT se_attr_name1 = IDENT_zero;
 SIDE_EFFECT ret_val7;
 SIDE_EFFECT s1;
 SIDE_EFFECT create_self1;
 SIDE_EFFECT ret_val8;
 OUT create_self2;
 OUT ret_val9;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val10;
 FILE1 r;
 OUT create_self3;
 OUT ret_val11;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val12;
 FILE1 r1;
 OUT create_self4;
 OUT ret_val13;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val14;
 FILE1 stdout_self2;
 FILE1 ret_val15;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val16;
 FILE1 stdout_self3;
 FILE1 ret_val17;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val18;
 FILE1 r4;
 dAM L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L6;
 SIDE_EFFECT L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 BOOL L13;
 SIDE_EFFECT L14;
 OB L15;
 BOOL L16;
 BOOL L171_;
 extern STR notunu421775452;
 FILE1 L18;
 OB L19;
 extern STR notunu589135516;
 FILE1 L21;
 OB L22;
 extern STR notunu1830346893;
 FILE1 L24;
 OB L25;
 FILE1 L27;
 OB L28;
 extern STR name9;
 FILE1 L30;
 OB L31;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c11;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L2;
   if (AM_CUR1885691330(c11, self)) {
    ret_val = TRUE;
    return ret_val;
   }
   if (t==NULL) {
    goto other232;
   } else
   switch (TAG(t)) {
    case AM_ATTR_EXPR_tag:
     tp_self = ((AM_ATTR_EXPR) t);
     ret_val1 = ATTR(tp_self,tp_at);
     L4 = ret_val1;
     if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) tp))) {
      is_eq_self = ATTR(((AM_ATTR_EXPR) t),at);
      is_eq_i = name111;
      ret_val2 = (is_eq_self.str==is_eq_i.str);
      L3 = ret_val2;
     } else {
      L3 = FALSE;
     }
     if (L3) {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_ROU1916046290_tag:
     if (ATTR(self,with_side_effect)) {
      c = SIG_ge642497605(ATTR(((AM_ROU1916046290) t),fun), ATTR(self,prog));
      full_self = c;
      if (ATTR(full_self,is_full)) {
       L6 = TRUE;
      } else {
       L6 = ATTR(full_self,has_import);
      }
      if (L6) {
       L5 = TRUE;
      } else {
       L5 = ATTR(full_self,has_export);
      }
      ret_val3 = L5;
      if (ret_val3) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self = c;
      se_attr_tp = tp;
      se_attr_name = name111;
      create_self = ((SIDE_EFFECT) NULL);
      L8=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L8==NULL) FATAL("Unable to allocate more memory");
      ((OB)L8)->header.tag=SIDE_EFFECT_tag;
      L7 = ((SIDE_EFFECT) L8);
      ret_val5 = L7;
      s = ret_val5;
      SATTR(s,tp,se_attr_tp);
      SATTR(s,name1,se_attr_name);
      ret_val4 = FSETSI937439554(ATTR(se_attr_self,set), s);
      se = ret_val4;
      L9 = (se==((SIDE_EFFECT) NULL));
      L101_=!(L9); 
      if (L101_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_ITE1809135850_tag:
     if (ATTR(self,with_side_effect)) {
      c12 = SIG_ge642497605(ATTR(((AM_ITE1809135850) t),fun), ATTR(self,prog));
      full_self1 = c12;
      if (ATTR(full_self1,is_full)) {
       L13 = TRUE;
      } else {
       L13 = ATTR(full_self1,has_import);
      }
      if (L13) {
       L12 = TRUE;
      } else {
       L12 = ATTR(full_self1,has_export);
      }
      ret_val6 = L12;
      if (ret_val6) {
       ret_val = FALSE;
       return ret_val;
      }
      se_attr_self1 = c12;
      se_attr_tp1 = tp;
      se_attr_name1 = name111;
      create_self1 = ((SIDE_EFFECT) NULL);
      L15=ZALLOC(sizeof(struct SIDE_EFFECT_struct));
      if (L15==NULL) FATAL("Unable to allocate more memory");
      ((OB)L15)->header.tag=SIDE_EFFECT_tag;
      L14 = ((SIDE_EFFECT) L15);
      ret_val8 = L14;
      s1 = ret_val8;
      SATTR(s1,tp,se_attr_tp1);
      SATTR(s1,name1,se_attr_name1);
      ret_val7 = FSETSI937439554(ATTR(se_attr_self1,set), s1);
      se1 = ret_val7;
      L16 = (se1==((SIDE_EFFECT) NULL));
      L171_=!(L16); 
      if (L171_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      ret_val = FALSE;
      return ret_val;
     } break;
    case AM_BND260301329_tag:
     if (AM_CURSOR_debug) {
      create_self2 = ((OUT) NULL);
      ret_val9 = create_self2;
      plus_self = ret_val9;
      plus_s = ((STR) &notunu421775452);
      stdout_self = ((FILE1) NULL);
      L19=ZALLOC(sizeof(struct FILE1_struct));
      if (L19==NULL) FATAL("Unable to allocate more memory");
      ((OB)L19)->header.tag=FILE1_tag;
      L18 = ((FILE1) L19);
      r = L18;
      
      SATTR(r,fp,stdout);
      ret_val10 = r;
      FILE_plus_STR(ret_val10, plus_s);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_BND367211769_tag:
     if (AM_CURSOR_debug) {
      create_self3 = ((OUT) NULL);
      ret_val11 = create_self3;
      plus_self1 = ret_val11;
      plus_s1 = ((STR) &notunu589135516);
      stdout_self1 = ((FILE1) NULL);
      L22=ZALLOC(sizeof(struct FILE1_struct));
      if (L22==NULL) FATAL("Unable to allocate more memory");
      ((OB)L22)->header.tag=FILE1_tag;
      L21 = ((FILE1) L22);
      r1 = L21;
      
      SATTR(r1,fp,stdout);
      ret_val12 = r1;
      FILE_plus_STR(ret_val12, plus_s1);
     }
     ret_val = FALSE;
     return ret_val; break;
    case AM_EXT_CALL_EXPR_tag:
     if (AM_CURSOR_debug) {
      create_self4 = ((OUT) NULL);
      ret_val13 = create_self4;
      plus_self2 = ret_val13;
      plus_s2 = ((STR) &notunu1830346893);
      stdout_self2 = ((FILE1) NULL);
      L25=ZALLOC(sizeof(struct FILE1_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=FILE1_tag;
      L24 = ((FILE1) L25);
      r2 = L24;
      
      SATTR(r2,fp,stdout);
      ret_val15 = r2;
      FILE_plus_STR(ret_val15, plus_s2);
      ret_val14 = plus_self2;
      plus_self3 = ret_val14;
      plus_s3 = ATTR(ATTR(((AM_EXT_CALL_EXPR) t),fun),str);
      stdout_self3 = ((FILE1) NULL);
      L28=ZALLOC(sizeof(struct FILE1_struct));
      if (L28==NULL) FATAL("Unable to allocate more memory");
      ((OB)L28)->header.tag=FILE1_tag;
      L27 = ((FILE1) L28);
      r3 = L27;
      
      SATTR(r3,fp,stdout);
      ret_val17 = r3;
      FILE_plus_STR(ret_val17, plus_s3);
      ret_val16 = plus_self3;
      plus_self4 = ret_val16;
      plus_s4 = ((STR) &name9);
      stdout_self4 = ((FILE1) NULL);
      L31=ZALLOC(sizeof(struct FILE1_struct));
      if (L31==NULL) FATAL("Unable to allocate more memory");
      ((OB)L31)->header.tag=FILE1_tag;
      L30 = ((FILE1) L31);
      r4 = L30;
      
      SATTR(r4,fp,stdout);
      ret_val18 = r4;
      FILE_plus_STR(ret_val18, plus_s4);
     }
     ret_val = FALSE;
     return ret_val; break;
    default: ;
    other232: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1754599450(AM_CURSOR self) {
 AM_CURSOR L11;
 dAM c;
 INT L21 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 INT L31 = INT_zero;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self3;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR name56;
 FILE1 L4;
 OB L5;
 dAM L7;
 extern STR name10;
 FILE1 L9;
 OB L10;
 INT L131_;
 INT L141_;
 INT L15;
 STR L161_;
 FILE1 L17;
 OB L18;
 extern STR name9;
 FILE1 L20;
 OB L22;
 extern STR name56;
 FILE1 L24;
 OB L25;
 AM_CUR1869307717(self);
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name56);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L7 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L7;
   {
    BOOL f_L81_ = TRUE;
    /* loop index variable */
    L31 = 0;
    L21 = ATTR(self,indent);
    while (1) {
     if(L31>=L21)  goto after_loop1; 
     ;
     create_self1 = ((OUT) NULL);
     ret_val2 = create_self1;
     plus_self1 = ret_val2;
     plus_s1 = ((STR) &name10);
     stdout_self1 = ((FILE1) NULL);
     L10=ZALLOC(sizeof(struct FILE1_struct));
     if (L10==NULL) FATAL("Unable to allocate more memory");
     ((OB)L10)->header.tag=FILE1_tag;
     L9 = ((FILE1) L10);
     r1 = L9;
     
     SATTR(r1,fp,stdout);
     ret_val3 = r1;
     FILE_plus_STR(ret_val3, plus_s1);
     L131_=INTPLUS(L31,1); 
     L31 = L131_;
    }
   }
   after_loop1: ;
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   L141_=SYSTP(c); 
   L15 = L141_;
   L161_=SYSSTRFORTP(L15); 
   plus_s2 = L161_;
   stdout_self2 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r2 = L17;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s2);
   ret_val5 = plus_self2;
   plus_self3 = ret_val5;
   plus_s3 = ((STR) &name9);
   stdout_self3 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r3 = L20;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   FILE_plus_STR(ret_val7, plus_s3);
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val8 = create_self3;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name56);
 stdout_self4 = ((FILE1) NULL);
 L25=ZALLOC(sizeof(struct FILE1_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=FILE1_tag;
 L24 = ((FILE1) L25);
 r4 = L24;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1769624888(AM_CURSOR self) {
 dAM tmcur;
 AM_CURSOR_POS s;
 dAM st;
 dAM tcur;
 A_STAC1244966098 size_self;
 INT ret_val = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val1;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 UNIX exit_self;
 INT exit_code = INT_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 INT L11;
 INT L121_;
 extern STR Intern614787477;
 FILE1 L13;
 OB L14;
 tmcur = ATTR(self,cur);
 while (1) {
  size_self = ATTR(self,stack1);
  ret_val = FLISTA587993904(ATTR(size_self,s));
  L1 = ret_val;
  L21_=(L1)==(0); 
  if (L21_) {
   SATTR(self,cur,((dAM) NULL));
   return;
  }
  pop_self = ATTR(self,stack1);
  ret_val1 = FLISTA971253650(ATTR(pop_self,s));
  s = ret_val1;
  st = ATTR(s,stmt);
  L3 = ATTR(s,branch);
  L41_=(L3)<(999999); 
  if (L41_) {
   L5 = ATTR(self,indent);
   L61_=INTMINUS(L5,1); 
   SATTR(self,indent,L61_);
  }
  if (st==NULL) {
   goto other230;
  } else
  switch (TAG(st)) {
   case AM_ASSERT_STMT_tag:
   case AM_ASSIGN_STMT_tag:
   case AM_ATTACH_STMT_tag:
   case AM_BREAK_STMT_tag:
   case AM_CASE_STMT_tag:
   case AM_COMMENT_STMT_tag:
   case AM_EXPR_STMT_tag:
   case AM_FORK_STMT_tag:
   case AM_IF_STMT_tag:
   case AM_INITIAL_STMT_tag:
   case AM_INV1167837230_tag:
   case AM_LOCK_STMT_tag:
   case AM_LOOP_STMT_tag:
   case AM_PAR_STMT_tag:
   case AM_POST_STMT_tag:
   case AM_PREFETCH_STMT_tag:
   case AM_PRE_STMT_tag:
   case AM_PROTECT_STMT_tag:
   case AM_RAISE_STMT_tag:
   case AM_RETURN_STMT_tag:
   case AM_SYNC_STMT_tag:
   case AM_TYPECASE_STMT_tag:
   case AM_UNLOCK_STMT_tag:
   case AM_WAITFOR_STMT_tag:
   case AM_WIT1996971603_tag:
   case AM_YIELD_STMT_tag:
    L7 = ATTR(s,branch);
    L81_=(L7)<(999999); 
    if (L81_) {
     SATTR(self,cur,ATTR(s,stmt));
     SATTR(self,mark,ATTR(s,mark));
     tcur = ATTR(self,cur);
     switch (TAG(tcur)) {
      case AM_LOOP_STMT_tag:
       L9 = ATTR(s,branch);
       L101_=(1)<(L9); 
       if (L101_) {
        L11 = ATTR(self,loops);
        L121_=INTMINUS(L11,1); 
        SATTR(self,loops,L121_);
       }
       return; break;
      case AM_ASSERT_STMT_tag:
      case AM_ASSIGN_STMT_tag:
      case AM_ATTACH_STMT_tag:
      case AM_BREAK_STMT_tag:
      case AM_CASE_STMT_tag:
      case AM_COMMENT_STMT_tag:
      case AM_EXPR_STMT_tag:
      case AM_FORK_STMT_tag:
      case AM_IF_STMT_tag:
      case AM_INITIAL_STMT_tag:
      case AM_INV1167837230_tag:
      case AM_LOCK_STMT_tag:
      case AM_PAR_STMT_tag:
      case AM_POST_STMT_tag:
      case AM_PREFETCH_STMT_tag:
      case AM_PRE_STMT_tag:
      case AM_PROTECT_STMT_tag:
      case AM_RAISE_STMT_tag:
      case AM_RETURN_STMT_tag:
      case AM_SYNC_STMT_tag:
      case AM_TYPECASE_STMT_tag:
      case AM_UNLOCK_STMT_tag:
      case AM_WAITFOR_STMT_tag:
      case AM_WIT1996971603_tag:
      case AM_YIELD_STMT_tag:
       return; break;
      default: ;
       FATAL("No applicable type in typecase\nin AM_CURSOR::surr_stmt\n./Back/cursor.sa:1535:23");
     }
     create_self = ((OUT) NULL);
     ret_val2 = create_self;
     plus_self = ret_val2;
     plus_s = ((STR) &Intern614787477);
     stdout_self = ((FILE1) NULL);
     L14=ZALLOC(sizeof(struct FILE1_struct));
     if (L14==NULL) FATAL("Unable to allocate more memory");
     ((OB)L14)->header.tag=FILE1_tag;
     L13 = ((FILE1) L14);
     r = L13;
     
     SATTR(r,fp,stdout);
     ret_val3 = r;
     FILE_plus_STR(ret_val3, plus_s);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    } break;
   default: ;
   other230: ;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

dAM AM_CUR1040670508(AM_CUR1040670508_frame frame) {
 dAM dummy = ((dAM) NULL);
 BOOL L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 if (ATTR(frame->self,started)) {
  L1 = (ATTR(frame->self,cur)==((dAM) NULL));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  AM_CUR1869307717(frame->self);
 }
 while (1) {
  frame->n = AM_CUR52795524(frame->self);
  if ((frame->n==((dAM) NULL))) {
   frame->state = -1;
   return;
  }
  else {
   frame->state = 1;
   return frame->n;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}
