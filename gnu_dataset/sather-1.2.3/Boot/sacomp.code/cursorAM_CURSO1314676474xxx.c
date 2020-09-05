#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

typedef struct AM_CASE_STMT_struct {/* layout for AM_CASE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTF7594819_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 } *AM_CASE_STMT;

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

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

typedef struct AM_INITIAL_STMT_struct {/* layout for AM_INITIAL_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct dAM_STMT_struct *stmts;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_INITIAL_STMT;

typedef struct AM_LOOP_STMT_struct {/* layout for AM_LOOP_STMT */
 OB_HEADER header;
 struct dAM_struct *first_while_move;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *init;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *loop_index_var;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTE1761212196_struct *hoisted;
 struct FLISTSTR_struct *ar_max_expr;
 INT breaks;
 struct SFILE_ID_struct source1;
 STR loop_index;
 BOOL has_init_stmt;
 BOOL has_yield;
 BOOL no_begin_loop;
 } *AM_LOOP_STMT;

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_PROTECT_STMT_struct {/* layout for AM_PROTECT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTdTP_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 } *AM_PROTECT_STMT;

typedef struct AM_STMT_EXPR_struct {/* layout for AM_STMT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dAM_EXPR_struct *replaced;
 struct dAM_STMT_struct *stmts;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTA725283029_struct *locals1;
 struct SFILE_ID_struct source1;
 } *AM_STMT_EXPR;

typedef struct AM_TYPECASE_STMT_struct {/* layout for AM_TYPECASE_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *test1;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTdTP_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL has_void_stmts;
 BOOL no_else;
 } *AM_TYPECASE_STMT;

typedef struct AM_WIT1996971603_struct {/* layout for AM_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_part;
 struct dAM_STMT_struct *near_part;
 struct dAM_STMT_struct *next;
 struct ARRAYdAM_EXPR_struct *objects1;
 struct SFILE_ID_struct source1;
 } *AM_WIT1996971603;

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

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

/* Function declarations */


extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AM_CURSOR_POS FLISTA860779230(FLISTA886630705);
AM_CURSOR_POS FLISTA971253650(FLISTA886630705);
AM_WIT1996971603 AM_CUR554772257(AM_CURSOR);
BOOL AM_CUR1778077145(AM_CURSOR);
BOOL AM_CUR1885691330(AM_CURSOR, AM_CURSOR);
BOOL AM_CUR1926902782(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR217486862(AM_CURSOR_POS, OB);
BOOL AM_CUR387225425(AM_CURSOR, dAM);
BOOL AM_CUR403450479(AM_CURSOR, dAM);
BOOL AM_CUR830576175(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR921612586(AM_CURSOR, dAM_STMT, INT, BOOL);
FLISTA886630705 FLISTA1475379054(FLISTA886630705, AM_CURSOR_POS);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AM_CUR1778373732(AM_CURSOR, AM_CURSOR);
INT FLISTA587993904(FLISTA886630705);
INT FLISTd355912233(FLISTdAM_STMT);
INT INT_do676118316(INT_do676118316_frame);
STR AM_CUR555854443(AM_CURSOR_POS);
STR AM_CUR720390882(AM_CURSOR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_pl1270664985(STR, dSTR);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM AM_CUR1623871191(AM_CURSOR);
dAM AM_CUR52795524(AM_CURSOR);
dAM AM_CUR710387089(AM_CURSOR, dAM, INT);
void AM_CUR1524773674(AM_CURSOR, dAM_STMT);
void AM_CUR1649762162(AM_CURSOR);
void AM_CUR1741858611(AM_CURSOR, dAM_STMT);
void AM_CUR1869307717(AM_CURSOR);
void AM_CUR2047729102(AM_CURSOR, dAM_STMT);
void AM_CUR315787800(AM_CURSOR, dAM, INT, dAM_STMT);
void AM_CUR413226357(AM_CURSOR, AM_CURSOR);
void AM_CUR915996644(AM_CURSOR, dAM_EXPR);
void AM_CUR997725387(AM_CURSOR);
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

AM_WIT1996971603 AM_CUR554772257(AM_CURSOR self) {
 AM_WIT1996971603 ret_val;
 A_STAC1244966098 L11;
 dAM s;
 A_STAC1244966098 eltb_self;
 AM_CURSOR_POS ret_val1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL eltb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val2;
 INT L5;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 AM_CURSOR_POS L81_;
 AM_CURSOR_POS L9;
 {
  BOOL f_L41_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::elt!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  eltb_self = L11;
  eltb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(eltb_self,s);
   if (f_L41_) {
    f_L41_ = FALSE;
    L6 = FLISTA587993904(ATTR(eltb_self,s));
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31 = 0;
    L41_=L21+1;L41_m=L31; 
   }
   if(L41_--<=L41_m)  goto after_loop; 
   aget_ind = L41_;
   L81_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val2 = L81_;
   L9=ret_val2;
   s = ATTR(L9,stmt);
   if (s==NULL) {
    goto other339;
   } else
   switch (TAG(s)) {
    case AM_WIT1996971603_tag:
     ret_val = ((AM_WIT1996971603) s);
     return ret_val; break;
    default: ;
    other339: ;
   }
  }
 }
 after_loop: ;
 ret_val = ((AM_WIT1996971603) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1778077145(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 dAM s;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val2;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val3;
 INT L1;
 BOOL L21_;
 AM_CURSOR_POS L3;
 AM_CURSOR_POS L4;
 INT L5;
 ARRAYdAM_EXPR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L111_;
 size_self = ATTR(self,stack1);
 ret_val1 = FLISTA587993904(ATTR(size_self,s));
 L1 = ret_val1;
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 top_self = ATTR(self,stack1);
 ret_val2 = FLISTA860779230(ATTR(top_self,s));
 L3=ret_val2;
 s = ATTR(L3,stmt);
 if (s==NULL) {
  goto other338;
 } else
 switch (TAG(s)) {
  case AM_WIT1996971603_tag:
   top_self1 = ATTR(self,stack1);
   ret_val3 = FLISTA860779230(ATTR(top_self1,s));
   L4=ret_val3;
   L5 = ATTR(L4,branch);
   L6 = ATTR(((AM_WIT1996971603) s),objects1);
   L71_=(L6)==NULL?0:ASIZE((ARRAYdAM_EXPR)L6); 
   L8 = L71_;
   L91_=INTPLUS(L8,2); 
   L10 = L91_;
   L111_=(L5)==(L10); 
   ret_val = L111_;
   return ret_val; break;
  default: ;
  other338: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR217486862(AM_CURSOR_POS self, OB arg) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR_POS is_eq_self;
 AM_CURSOR_POS is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 if (arg==NULL) {
  goto other159;
 } else
 switch (TAG(arg)) {
  case AM_CURSOR_POS_tag:
   is_eq_self = self;
   is_eq_t = ((AM_CURSOR_POS) arg);
   if (SYSOBEQ(((OB) ATTR(is_eq_self,stmt)),((OB) ATTR(is_eq_t,stmt)))) {
    L3 = ATTR(is_eq_self,branch);
    L4 = ATTR(is_eq_t,branch);
    L51_=(L3)==(L4); 
    L2 = L51_;
   } else {
    L2 = FALSE;
   }
   if (L2) {
    L6 = ATTR(is_eq_self,mark);
    L7 = ATTR(is_eq_t,mark);
    L81_=L6==L7; 
    L1 = L81_;
   } else {
    L1 = FALSE;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other159: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR387225425(AM_CURSOR self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR L11;
 AM_CURSOR L21;
 dAM L3;
 dAM L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 dAM L10;
 dAM L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L4 = L3;
   L51_=SYSID(L4); 
   L6 = L51_;
   L71_=SYSID(am); 
   L8 = L71_;
   L91_=(L6)==(L8); 
   if (L91_) {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
 after_loop: ;
 AM_CUR1869307717(self);
 {
  struct AM_CUR1040670508_frame_struct other2_0;
AM_CUR1040670508_frame noname2 = &other2_0;
  L21 = self;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L10 = AM_CUR1040670508(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   L12 = L10;
   L131_=SYSID(L12); 
   L14 = L131_;
   L151_=SYSID(am); 
   L16 = L151_;
   L171_=(L14)==(L16); 
   if (L171_) {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
 after_loop1: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR921612586(AM_CURSOR self, dAM_STMT am, INT weight, BOOL after_prefetch) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR cstore;
 BOOL first_try = BOOL_zero;
 AM_CURSOR L11;
 dAM n;
 INT L21 = INT_zero;
 AM_CURSOR L31;
 INT w = INT_zero;
 dAM tcur;
 dAM_STMT tcur21;
 dAM t;
 BOOL d = BOOL_zero;
 BOOL d11 = BOOL_zero;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT create_self1;
 OUT ret_val3;
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
 OUT create_self2;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n11;
 OUT create_self3;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val10;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT plus_self5;
 dSTR plus_s5;
 OUT ret_val12;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val14;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 OUT plus_self7;
 dSTR plus_s7;
 OUT ret_val16;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 OUT create_self4;
 OUT ret_val19;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 OUT create_self5;
 OUT ret_val21;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val22;
 FILE1 stdout_self10;
 FILE1 ret_val23;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val24;
 FILE1 r11;
 OUT create_self6;
 OUT ret_val25;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val26;
 FILE1 stdout_self12;
 FILE1 ret_val27;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val28;
 FILE1 r13;
 OUT create_self7;
 OUT ret_val29;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val30;
 FILE1 r14;
 INT L41 = INT_zero;
 OUT create_self8;
 OUT ret_val31;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val32;
 FILE1 r15;
 OUT create_self9;
 OUT ret_val33;
 OUT plus_self16;
 STR plus_s16;
 OUT ret_val34;
 FILE1 stdout_self16;
 FILE1 ret_val35;
 FILE1 r16;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val36;
 FILE1 r17;
 AM_CURSOR nextb_self;
 AM_CURSOR L51;
 dAM n2;
 BOOL nextb_if_first = BOOL_zero;
 OUT create_self10;
 OUT ret_val37;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val38;
 FILE1 stdout_self18;
 FILE1 ret_val39;
 FILE1 r18;
 OUT plus_self19;
 dSTR plus_s19;
 OUT ret_val40;
 FILE1 stdout_self19;
 FILE1 ret_val41;
 FILE1 r19;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val42;
 FILE1 r20;
 AM_CURSOR next_stmt_self;
 dAM d21;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val43;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val44;
 extern STR insert806313390;
 FILE1 L6;
 OB L7;
 extern STR self3;
 FILE1 L9;
 OB L10;
 FILE1 L13;
 OB L14;
 extern STR am1;
 FILE1 L16;
 OB L17;
 dAM_STMT L19;
 dAM_STMT L20;
 dAM_STMT L22;
 extern STR weight1;
 FILE1 L23;
 OB L24;
 dSTR L26;
 OB L27;
 FILE1 L28;
 OB L29;
 extern STR after_prefetch1;
 FILE1 L32;
 OB L33;
 dSTR L35;
 OB L36;
 FILE1 L37;
 OB L38;
 extern STR name9;
 FILE1 L40;
 OB L42;
 extern STR HAS_IT200349984;
 FILE1 L44;
 OB L45;
 extern STR LOOPself;
 FILE1 L47;
 OB L48;
 FILE1 L50;
 OB L52;
 BOOL L54;
 AM_CURSOR L55;
 extern STR LOOP2self;
 FILE1 L56;
 OB L57;
 FILE1 L59;
 OB L60;
 dAM L62;
 extern STR IAM;
 FILE1 L63;
 OB L64;
 extern STR name10;
 FILE1 L67;
 OB L68;
 INT L701_;
 INT L711_;
 INT L72;
 STR L731_;
 FILE1 L74;
 OB L75;
 extern STR name9;
 FILE1 L77;
 OB L78;
 dAM L80;
 extern STR WEIGHT;
 FILE1 L82;
 OB L83;
 dSTR L85;
 OB L86;
 FILE1 L87;
 OB L88;
 extern STR name9;
 FILE1 L90;
 OB L91;
 BOOL L931_;
 BOOL L94;
 BOOL L951_;
 dAM_STMT L96;
 dAM L97;
 INT L981_;
 INT L99;
 dAM L100;
 INT L1011_;
 INT L102;
 BOOL L1031_;
 BOOL L104;
 BOOL L1051_;
 AM_CURSOR_POS L106;
 dAM L107;
 INT L1081_;
 INT L109;
 AM_CURSOR_POS L110;
 dAM L111;
 INT L1121_;
 INT L113;
 BOOL L1141_;
 BOOL L115;
 BOOL L1161_;
 AM_CURSOR_debug = ATTR(ATTR(self,prog),opt_debug);
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &insert806313390);
  stdout_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r = L6;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  create_self1 = ((OUT) NULL);
  ret_val3 = create_self1;
  plus_self1 = ret_val3;
  plus_s1 = ((STR) &self3);
  stdout_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r1 = L9;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = AM_CUR720390882(self);
  stdout_self2 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r2 = L13;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
  create_self2 = ((OUT) NULL);
  ret_val7 = create_self2;
  plus_self3 = ret_val7;
  plus_s3 = ((STR) &am1);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  AM_one_stmt_self = ((AM_OUT) NULL);
  AM_one_stmt_a = ((dAM) am);
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
    L19 = ((dAM_STMT) AM_one_stmt_a);
    n11 = (*dAM_ST775224435[TAG(L19)])(L19);
    L20 = ((dAM_STMT) AM_one_stmt_a);
    (*dAM_ST1372701974[TAG(L20)])(L20, ((dAM_STMT) NULL));
    AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
    L22 = ((dAM_STMT) AM_one_stmt_a);
    (*dAM_ST1372701974[TAG(L22)])(L22, n11); break;
   default: ;
    AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
  }
  create_self3 = ((OUT) NULL);
  ret_val9 = create_self3;
  plus_self4 = ret_val9;
  plus_s4 = ((STR) &weight1);
  stdout_self4 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r4 = L23;
  
  SATTR(r4,fp,stdout);
  ret_val11 = r4;
  FILE_plus_STR(ret_val11, plus_s4);
  ret_val10 = plus_self4;
  plus_self5 = ret_val10;
  L27=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  memset(L27,0,sizeof(struct INT_boxed_struct));
  ((OB)L27)->header.tag=INT_tag;
  L26 = (dSTR)((INT_boxed) L27);
  ((INT_boxed) L26)->immutable_part = weight;
  plus_s5 = L26;
  stdout_self5 = ((FILE1) NULL);
  L29=ZALLOC(sizeof(struct FILE1_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=FILE1_tag;
  L28 = ((FILE1) L29);
  r5 = L28;
  
  SATTR(r5,fp,stdout);
  ret_val13 = r5;
  FILE_plus_dSTR(ret_val13, plus_s5);
  ret_val12 = plus_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &after_prefetch1);
  stdout_self6 = ((FILE1) NULL);
  L33=ZALLOC(sizeof(struct FILE1_struct));
  if (L33==NULL) FATAL("Unable to allocate more memory");
  ((OB)L33)->header.tag=FILE1_tag;
  L32 = ((FILE1) L33);
  r6 = L32;
  
  SATTR(r6,fp,stdout);
  ret_val15 = r6;
  FILE_plus_STR(ret_val15, plus_s6);
  ret_val14 = plus_self6;
  plus_self7 = ret_val14;
  L36=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  memset(L36,0,sizeof(struct BOOL_boxed_struct));
  ((OB)L36)->header.tag=BOOL_tag;
  L35 = (dSTR)((BOOL_boxed) L36);
  ((BOOL_boxed) L35)->immutable_part = after_prefetch;
  plus_s7 = L35;
  stdout_self7 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r7 = L37;
  
  SATTR(r7,fp,stdout);
  ret_val17 = r7;
  FILE_plus_dSTR(ret_val17, plus_s7);
  ret_val16 = plus_self7;
  plus_self8 = ret_val16;
  plus_s8 = ((STR) &name9);
  stdout_self8 = ((FILE1) NULL);
  L42=ZALLOC(sizeof(struct FILE1_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  ((OB)L42)->header.tag=FILE1_tag;
  L40 = ((FILE1) L42);
  r8 = L40;
  
  SATTR(r8,fp,stdout);
  ret_val18 = r8;
  FILE_plus_STR(ret_val18, plus_s8);
 }
 cstore = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 if (AM_CUR403450479(self, ((dAM) am))) {
  if (AM_CURSOR_debug) {
   create_self4 = ((OUT) NULL);
   ret_val19 = create_self4;
   plus_self9 = ret_val19;
   plus_s9 = ((STR) &HAS_IT200349984);
   stdout_self9 = ((FILE1) NULL);
   L45=ZALLOC(sizeof(struct FILE1_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   ((OB)L45)->header.tag=FILE1_tag;
   L44 = ((FILE1) L45);
   r9 = L44;
   
   SATTR(r9,fp,stdout);
   ret_val20 = r9;
   FILE_plus_STR(ret_val20, plus_s9);
  }
  AM_CUR997725387(self);
 }
 else {
  AM_CUR1649762162(self);
 }
 AM_CURSOR_debug = FALSE;
 first_try = TRUE;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  struct AM_CUR1040670508_frame_struct other1_1;
AM_CUR1040670508_frame noname2 = &other1_1;
  noname1->state = 0;
  noname2->state = 0;
  nextb_if_first = TRUE;
  while (1) {
   if (AM_CURSOR_debug) {
    create_self5 = ((OUT) NULL);
    ret_val21 = create_self5;
    plus_self10 = ret_val21;
    plus_s10 = ((STR) &LOOPself);
    stdout_self10 = ((FILE1) NULL);
    L48=ZALLOC(sizeof(struct FILE1_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=FILE1_tag;
    L47 = ((FILE1) L48);
    r10 = L47;
    
    SATTR(r10,fp,stdout);
    ret_val23 = r10;
    FILE_plus_STR(ret_val23, plus_s10);
    ret_val22 = plus_self10;
    plus_self11 = ret_val22;
    plus_s11 = AM_CUR720390882(self);
    stdout_self11 = ((FILE1) NULL);
    L52=ZALLOC(sizeof(struct FILE1_struct));
    if (L52==NULL) FATAL("Unable to allocate more memory");
    ((OB)L52)->header.tag=FILE1_tag;
    L50 = ((FILE1) L52);
    r11 = L50;
    
    SATTR(r11,fp,stdout);
    ret_val24 = r11;
    FILE_plus_STR(ret_val24, plus_s11);
   }
   if (AM_CUR1885691330(cstore, self)) {
    ret_val = FALSE;
    return ret_val;
   }
   AM_CURSOR_debug = ATTR(ATTR(self,prog),opt_debug);
   if (am==NULL) {
    goto other331;
   } else
   switch (TAG(am)) {
    case AM_PREFETCH_STMT_tag:
     if (AM_CUR830576175(cstore, self, ((dAM) ATTR(((AM_PREFETCH_STMT) am),src1)))) {
      L55 = AM_CUR1358945253(((AM_CURSOR) NULL), self);
      L54 = AM_CUR1926902782(cstore, L55, ((dAM) ATTR(((AM_PREFETCH_STMT) am),dest)));
     } else {
      L54 = FALSE;
     }
     if (L54) {
      goto after_loop;
     } break;
    default: ;
    other331: ;
     if (AM_CUR830576175(cstore, self, ((dAM) am))) {
      goto after_loop;
     }
   }
   AM_CURSOR_debug = FALSE;
   if (AM_CURSOR_debug) {
    create_self6 = ((OUT) NULL);
    ret_val25 = create_self6;
    plus_self12 = ret_val25;
    plus_s12 = ((STR) &LOOP2self);
    stdout_self12 = ((FILE1) NULL);
    L57=ZALLOC(sizeof(struct FILE1_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=FILE1_tag;
    L56 = ((FILE1) L57);
    r12 = L56;
    
    SATTR(r12,fp,stdout);
    ret_val27 = r12;
    FILE_plus_STR(ret_val27, plus_s12);
    ret_val26 = plus_self12;
    plus_self13 = ret_val26;
    plus_s13 = AM_CUR720390882(self);
    stdout_self13 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r13 = L59;
    
    SATTR(r13,fp,stdout);
    ret_val28 = r13;
    FILE_plus_STR(ret_val28, plus_s13);
    if (noname1->state == 0) {
     L11 = self;
     noname1->self = L11;
    }
    L62 = AM_CUR1040670508(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    n = L62;
    create_self7 = ((OUT) NULL);
    ret_val29 = create_self7;
    plus_self14 = ret_val29;
    plus_s14 = ((STR) &IAM);
    stdout_self14 = ((FILE1) NULL);
    L64=ZALLOC(sizeof(struct FILE1_struct));
    if (L64==NULL) FATAL("Unable to allocate more memory");
    ((OB)L64)->header.tag=FILE1_tag;
    L63 = ((FILE1) L64);
    r14 = L63;
    
    SATTR(r14,fp,stdout);
    ret_val30 = r14;
    FILE_plus_STR(ret_val30, plus_s14);
    {
     BOOL f_L661_ = TRUE;
     /* loop index variable */
     L41 = 0;
     L21 = ATTR(self,indent);
     while (1) {
      if(L41>=L21)  goto after_loop1; 
      ;
      create_self8 = ((OUT) NULL);
      ret_val31 = create_self8;
      plus_self15 = ret_val31;
      plus_s15 = ((STR) &name10);
      stdout_self15 = ((FILE1) NULL);
      L68=ZALLOC(sizeof(struct FILE1_struct));
      if (L68==NULL) FATAL("Unable to allocate more memory");
      ((OB)L68)->header.tag=FILE1_tag;
      L67 = ((FILE1) L68);
      r15 = L67;
      
      SATTR(r15,fp,stdout);
      ret_val32 = r15;
      FILE_plus_STR(ret_val32, plus_s15);
      L701_=INTPLUS(L41,1); 
      L41 = L701_;
     }
    }
    after_loop1: ;
    create_self9 = ((OUT) NULL);
    ret_val33 = create_self9;
    plus_self16 = ret_val33;
    L711_=SYSTP(n); 
    L72 = L711_;
    L731_=SYSSTRFORTP(L72); 
    plus_s16 = L731_;
    stdout_self16 = ((FILE1) NULL);
    L75=ZALLOC(sizeof(struct FILE1_struct));
    if (L75==NULL) FATAL("Unable to allocate more memory");
    ((OB)L75)->header.tag=FILE1_tag;
    L74 = ((FILE1) L75);
    r16 = L74;
    
    SATTR(r16,fp,stdout);
    ret_val35 = r16;
    FILE_plus_STR(ret_val35, plus_s16);
    ret_val34 = plus_self16;
    plus_self17 = ret_val34;
    plus_s17 = ((STR) &name9);
    stdout_self17 = ((FILE1) NULL);
    L78=ZALLOC(sizeof(struct FILE1_struct));
    if (L78==NULL) FATAL("Unable to allocate more memory");
    ((OB)L78)->header.tag=FILE1_tag;
    L77 = ((FILE1) L78);
    r17 = L77;
    
    SATTR(r17,fp,stdout);
    ret_val36 = r17;
    FILE_plus_STR(ret_val36, plus_s17);
   }
   else {
    if (nextb_if_first) {
     L31 = self;
     nextb_self = L31;
     nextb_if_first = FALSE;
    }
    if (noname2->state == 0) {
     L51 = nextb_self;
     noname2->self = L51;
    }
    L80 = AM_CUR1040670508(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    n2 = L80;
   }
   first_try = FALSE;
  }
 }
 after_loop: ;
 AM_CURSOR_debug = FALSE;
 w = AM_CUR1778373732(cstore, self);
 if (AM_CURSOR_debug) {
  create_self10 = ((OUT) NULL);
  ret_val37 = create_self10;
  plus_self18 = ret_val37;
  plus_s18 = ((STR) &WEIGHT);
  stdout_self18 = ((FILE1) NULL);
  L83=ZALLOC(sizeof(struct FILE1_struct));
  if (L83==NULL) FATAL("Unable to allocate more memory");
  ((OB)L83)->header.tag=FILE1_tag;
  L82 = ((FILE1) L83);
  r18 = L82;
  
  SATTR(r18,fp,stdout);
  ret_val39 = r18;
  FILE_plus_STR(ret_val39, plus_s18);
  ret_val38 = plus_self18;
  plus_self19 = ret_val38;
  L86=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L86==NULL) FATAL("Unable to allocate more memory");
  memset(L86,0,sizeof(struct INT_boxed_struct));
  ((OB)L86)->header.tag=INT_tag;
  L85 = (dSTR)((INT_boxed) L86);
  ((INT_boxed) L85)->immutable_part = w;
  plus_s19 = L85;
  stdout_self19 = ((FILE1) NULL);
  L88=ZALLOC(sizeof(struct FILE1_struct));
  if (L88==NULL) FATAL("Unable to allocate more memory");
  ((OB)L88)->header.tag=FILE1_tag;
  L87 = ((FILE1) L88);
  r19 = L87;
  
  SATTR(r19,fp,stdout);
  ret_val41 = r19;
  FILE_plus_dSTR(ret_val41, plus_s19);
  ret_val40 = plus_self19;
  plus_self20 = ret_val40;
  plus_s20 = ((STR) &name9);
  stdout_self20 = ((FILE1) NULL);
  L91=ZALLOC(sizeof(struct FILE1_struct));
  if (L91==NULL) FATAL("Unable to allocate more memory");
  ((OB)L91)->header.tag=FILE1_tag;
  L90 = ((FILE1) L91);
  r20 = L90;
  
  SATTR(r20,fp,stdout);
  ret_val42 = r20;
  FILE_plus_STR(ret_val42, plus_s20);
 }
 L931_=(w)<(weight); 
 L94 = L931_;
 L951_=!(L94); 
 if (L951_) {
  if (after_prefetch) {
   while (1) {
    tcur = ATTR(self,cur);
    if (tcur==NULL) {
     goto other332;
    } else
    switch (TAG(tcur)) {
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
      L96 = ((dAM_STMT) tcur);
      tcur21 = (*dAM_ST775224435[TAG(L96)])(L96);
      if (tcur21==NULL) {
       goto other333;
      } else
      switch (TAG(tcur21)) {
       case AM_PREFETCH_STMT_tag:
        next_stmt_self = self;
        d21 = AM_CUR1623871191(next_stmt_self); break;
       default: ;
       other333: ;
        goto after_loop2;
      } break;
     default: ;
     other332: ;
      goto after_loop2;
    }
   }
   after_loop2: ;
  }
  AM_CURSOR_debug = ATTR(ATTR(self,prog),opt_debug);
  t = ATTR(self,cur);
  if (t==NULL) {
   goto other334;
  } else
  switch (TAG(t)) {
   case AM_ASSIGN_STMT_tag:
    AM_CUR1741858611(self, am); break;
   case AM_ASSERT_STMT_tag:
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
    AM_CUR1524773674(self, am); break;
   default: ;
   other334: ;
    AM_CUR1741858611(self, am);
  }
  AM_CURSOR_debug = FALSE;
  L97 = ATTR(cstore,top1);
  L981_=SYSID(L97); 
  L99 = L981_;
  L100 = ATTR(self,top1);
  L1011_=SYSID(L100); 
  L102 = L1011_;
  L1031_=(L99)==(L102); 
  L104 = L1031_;
  L1051_=!(L104); 
  if (L1051_) {
   SATTR(cstore,top1,ATTR(self,top1));
  }
  d = AM_CUR387225425(self, ATTR(cstore,cur));
  top_self = ATTR(cstore,stack1);
  ret_val43 = FLISTA860779230(ATTR(top_self,s));
  L106=ret_val43;
  L107 = ATTR(L106,stmt);
  L1081_=SYSID(L107); 
  L109 = L1081_;
  top_self1 = ATTR(self,stack1);
  ret_val44 = FLISTA860779230(ATTR(top_self1,s));
  L110=ret_val44;
  L111 = ATTR(L110,stmt);
  L1121_=SYSID(L111); 
  L113 = L1121_;
  L1141_=(L109)==(L113); 
  L115 = L1141_;
  L1161_=!(L115); 
  if (L1161_) {
   d = AM_CUR387225425(self, ATTR(cstore,cur));
  }
  ret_val = TRUE;
  return ret_val;
 }
 d11 = AM_CUR387225425(self, ATTR(cstore,cur));
 AM_CUR413226357(self, cstore);
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR AM_CUR555854443(AM_CURSOR_POS self) {
 STR ret_val;
 STR ret;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 INT plus_arg = INT_zero;
 STR ret_val4;
 STR s;
 INT str_self = INT_zero;
 STR ret_val5;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val6;
 FSTR str_self1;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self6;
 INT plus_arg1 = INT_zero;
 STR ret_val10;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val11;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val12;
 FSTR str_self3;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val14;
 extern STR name1;
 extern STR stmt;
 dAM L1;
 INT L21_;
 INT L3;
 STR L41_;
 extern STR name14;
 dAM L5;
 INT L61_;
 BOOL L7;
 BOOL L81_;
 extern STR name7;
 extern STR branch;
 BOOL L9;
 BOOL L101_;
 extern STR mark;
 dSTR L11;
 OB L12;
 ret = ((STR) &name1);
 plus_self = ret;
 plus_sarg = ((STR) &stmt);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 L1 = ATTR(self,stmt);
 L21_=SYSTP(L1); 
 L3 = L21_;
 L41_=SYSSTRFORTP(L3); 
 plus_sarg1 = L41_;
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &name14);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 L5 = ATTR(self,stmt);
 L61_=SYSID(L5); 
 plus_arg = L61_;
 str_self = plus_arg;
 clear_self = buf1;
 L7 = (clear_self==((FSTR) NULL));
 L81_=!(L7); 
 if (L81_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val6 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val6;
 str_self1 = buf1;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val5 = ret_val7;
 s = ret_val5;
 ret_val4 = STR_ap1077157958(plus_self3, s, TRUE);
 plus_self4 = ret_val4;
 plus_sarg3 = ((STR) &name7);
 ret_val8 = STR_ap2004550586(plus_self4, plus_sarg3);
 ret = ret_val8;
 plus_self5 = ret;
 plus_sarg4 = ((STR) &branch);
 ret_val9 = STR_ap2004550586(plus_self5, plus_sarg4);
 plus_self6 = ret_val9;
 plus_arg1 = ATTR(self,branch);
 str_self2 = plus_arg1;
 clear_self1 = buf2;
 L9 = (clear_self1==((FSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val12 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val12;
 str_self3 = buf2;
 ret_val13 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val11 = ret_val13;
 s1 = ret_val11;
 ret_val10 = STR_ap1077157958(plus_self6, s1, TRUE);
 plus_self7 = ret_val10;
 plus_sarg5 = ((STR) &mark);
 ret_val14 = STR_ap2004550586(plus_self7, plus_sarg5);
 L12=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 memset(L12,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L12)->header.tag=BOOL_tag;
 L11 = (dSTR)((BOOL_boxed) L12);
 ((BOOL_boxed) L11)->immutable_part = ATTR(self,mark);
 ret = STR_pl1270664985(ret_val14, L11);
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1524773674(AM_CURSOR self, dAM_STMT am) {
 dAM tcur;
 AM_STMT_EXPR se;
 AM_STMT_EXPR se1;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 OUT ret_val1;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 AM_STMT_EXPR create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val4;
 AM_STMT_EXPR r2;
 AM_STMT_EXPR create_self2;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val5;
 AM_STMT_EXPR r3;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r4;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r5;
 extern STR CURSOR479438165;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 AM_STMT_EXPR L7;
 OB L8;
 AM_STMT_EXPR L9;
 OB L10;
 extern STR CURSOR570633082;
 FILE1 L11;
 OB L12;
 FILE1 L14;
 OB L15;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &CURSOR479438165);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = AM_CUR720390882(self);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 tcur = ATTR(self,cur);
 if (tcur==NULL) {
  goto other328;
 } else
 switch (TAG(tcur)) {
  case AM_CALL_ARG_tag:
   create_self1 = ((AM_STMT_EXPR) NULL);
   create_source = SFILE_ID_zero;
   L8=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=AM_STMT_EXPR_tag;
   L7 = ((AM_STMT_EXPR) L8);
   r2 = L7;
   SATTR(r2,source1,create_source);
   ret_val4 = r2;
   se = ret_val4;
   SATTR(se,stmts,am);
   SATTR(se,expr,ATTR(((AM_CALL_ARG) tcur),expr));
   SATTR(((AM_CALL_ARG) tcur),expr,((dAM_EXPR) se)); break;
  case AM_ANY_EXPR_tag:
  case AM_ARRAY_EXPR_tag:
  case AM_ARR_CONST_tag:
  case AM_ATTR_EXPR_tag:
  case AM_AT_EXPR_tag:
  case AM_BND1124877163_tag:
  case AM_BND367211769_tag:
  case AM_BND260301329_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CLUSTER_EXPR_tag:
  case AM_CLU1286269335_tag:
  case AM_CONST_tag:
  case AM_EXCEPT_EXPR_tag:
  case AM_EXT_CALL_EXPR_tag:
  case AM_FAR_EXPR_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_GLOBAL_EXPR_tag:
  case AM_HERE_EXPR_tag:
  case AM_IF_EXPR_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_IS_VOID_EXPR_tag:
  case AM_ITE1809135850_tag:
  case AM_LOCAL_EXPR_tag:
  case AM_NEAR_EXPR_tag:
  case AM_NEW_EXPR_tag:
  case AM_ROU1916046290_tag:
  case AM_SHARED_EXPR_tag:
  case AM_STMT_EXPR_tag:
  case AM_STR_CONST_tag:
  case AM_VAR744470097_tag:
  case AM_VAT319982528_tag:
  case AM_VOID_CONST_tag:
  case AM_WHERE_EXPR_tag:
   create_self2 = ((AM_STMT_EXPR) NULL);
   create_source1 = SFILE_ID_zero;
   L10=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=AM_STMT_EXPR_tag;
   L9 = ((AM_STMT_EXPR) L10);
   r3 = L9;
   SATTR(r3,source1,create_source1);
   ret_val5 = r3;
   se1 = ret_val5;
   SATTR(se1,stmts,am);
   SATTR(se1,expr,((dAM_EXPR) tcur));
   AM_CUR915996644(self, ((dAM_EXPR) se1));
   SATTR(self,cur,AM_CUR710387089(self, ((dAM) se1), 1));
   SATTR(self,mark,FALSE); break;
  default: ;
  other328: ;
   AM_CUR2047729102(self, am);
 }
 if (AM_CURSOR_debug) {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self2 = ret_val6;
  plus_s2 = ((STR) &CURSOR570633082);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L11 = ((FILE1) L12);
  r4 = L11;
  
  SATTR(r4,fp,stdout);
  ret_val8 = r4;
  FILE_plus_STR(ret_val8, plus_s2);
  ret_val7 = plus_self2;
  plus_self3 = ret_val7;
  plus_s3 = AM_CUR720390882(self);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r5 = L14;
  
  SATTR(r5,fp,stdout);
  ret_val9 = r5;
  FILE_plus_STR(ret_val9, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1741858611(AM_CURSOR self, dAM_STMT am) {
 dAM tcur = ((dAM) NULL);
 AM_CURSOR c;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AM_CURSOR next_self;
 dAM dummy1;
 AM_STMT_EXPR create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val6;
 AM_STMT_EXPR r3;
 AM_CURSOR next_self1;
 dAM dummy2;
 OUT create_self3;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r5;
 extern STR CURSOR1135707328;
 FILE1 L1;
 OB L2;
 extern STR beforeinserting;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 AM_STMT_EXPR L10;
 OB L11;
 extern STR CURSOR1056944728;
 FILE1 L12;
 OB L13;
 FILE1 L15;
 OB L16;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &CURSOR1135707328);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &beforeinserting);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = AM_CUR720390882(self);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 while (1) {
  tcur = ATTR(self,cur);
  if (tcur==NULL) {
   goto other329;
  } else
  switch (TAG(tcur)) {
   case AM_CALL_ARG_tag:
    next_self = self;
    dummy1 = AM_CUR52795524(next_self); break;
   default: ;
   other329: ;
    goto after_loop;
  }
 }
 after_loop: ;
 if (tcur==NULL) {
  goto other330;
 } else
 switch (TAG(tcur)) {
  case AM_ANY_EXPR_tag:
  case AM_ARRAY_EXPR_tag:
  case AM_ARR_CONST_tag:
  case AM_ATTR_EXPR_tag:
  case AM_AT_EXPR_tag:
  case AM_BND1124877163_tag:
  case AM_BND367211769_tag:
  case AM_BND260301329_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CLUSTER_EXPR_tag:
  case AM_CLU1286269335_tag:
  case AM_CONST_tag:
  case AM_EXCEPT_EXPR_tag:
  case AM_EXT_CALL_EXPR_tag:
  case AM_FAR_EXPR_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_GLOBAL_EXPR_tag:
  case AM_HERE_EXPR_tag:
  case AM_IF_EXPR_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_IS_VOID_EXPR_tag:
  case AM_ITE1809135850_tag:
  case AM_LOCAL_EXPR_tag:
  case AM_NEAR_EXPR_tag:
  case AM_NEW_EXPR_tag:
  case AM_ROU1916046290_tag:
  case AM_SHARED_EXPR_tag:
  case AM_STMT_EXPR_tag:
  case AM_STR_CONST_tag:
  case AM_VAR744470097_tag:
  case AM_VAT319982528_tag:
  case AM_VOID_CONST_tag:
  case AM_WHERE_EXPR_tag:
   c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
   create_self2 = ((AM_STMT_EXPR) NULL);
   create_source = SFILE_ID_zero;
   L11=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
   if (L11==NULL) FATAL("Unable to allocate more memory");
   ((OB)L11)->header.tag=AM_STMT_EXPR_tag;
   L10 = ((AM_STMT_EXPR) L11);
   r3 = L10;
   SATTR(r3,source1,create_source);
   ret_val6 = r3;
   SATTR(c,cur,((dAM) ret_val6));
   next_self1 = c;
   dummy2 = AM_CUR52795524(next_self1);
   AM_CUR1524773674(c, am); break;
  default: ;
  other330: ;
   AM_CUR315787800(self, ATTR(self,cur), 999999, am);
 }
 if (AM_CURSOR_debug) {
  create_self3 = ((OUT) NULL);
  ret_val7 = create_self3;
  plus_self3 = ret_val7;
  plus_s3 = ((STR) &CURSOR1056944728);
  stdout_self3 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r4 = L12;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s3);
  ret_val8 = plus_self3;
  plus_self4 = ret_val8;
  plus_s4 = AM_CUR720390882(self);
  stdout_self4 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r5 = L15;
  
  SATTR(r5,fp,stdout);
  ret_val10 = r5;
  FILE_plus_STR(ret_val10, plus_s4);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR2047729102(AM_CURSOR self, dAM_STMT am) {
 A_STAC1244966098 tst;
 INT branch1 = INT_zero;
 dAM tcur;
 dAM t;
 AM_CURSOR_POS s;
 AM_CURSOR_POS s1;
 dAM_STMT aml;
 AM_CURSOR_POS s2;
 A_STAC1244966098 create_self;
 A_STAC1244966098 ret_val;
 A_STAC1244966098 res;
 FLISTA886630705 create_self1;
 FLISTA886630705 ret_val1;
 AM_CURSOR_POS create_self2;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val2;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 A_STAC1244966098 size_self;
 INT ret_val3 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val4;
 AM_CURSOR_POS create_self3;
 dAM create_curr_stmt1;
 BOOL create_m1 = BOOL_zero;
 INT create_branch1 = INT_zero;
 AM_CURSOR_POS ret_val5;
 AM_CURSOR_POS r1;
 A_STAC1244966098 push_self1;
 AM_CURSOR_POS push_e1;
 A_STAC1244966098 size_self1;
 INT ret_val6 = INT_zero;
 A_STAC1244966098 pop_self1;
 AM_CURSOR_POS ret_val7;
 A_STAC1244966098 push_self2;
 AM_CURSOR_POS push_e2;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val8;
 AM_CURSOR_POS create_self4;
 dAM create_curr_stmt2;
 BOOL create_m2 = BOOL_zero;
 INT create_branch2 = INT_zero;
 AM_CURSOR_POS ret_val9;
 AM_CURSOR_POS r2;
 A_STAC1244966098 push_self3;
 AM_CURSOR_POS push_e3;
 A_STAC1244966098 size_self2;
 INT ret_val10 = INT_zero;
 A_STAC1244966098 pop_self2;
 AM_CURSOR_POS ret_val11;
 A_STAC1244966098 push_self4;
 AM_CURSOR_POS push_e4;
 A_STAC1244966098 pop_self3;
 AM_CURSOR_POS ret_val12;
 A_STAC1244966098 L1;
 OB L2;
 AM_CURSOR_POS L3;
 OB L4;
 INT L5;
 BOOL L61_;
 dAM_STMT L7;
 AM_CURSOR_POS L8;
 OB L9;
 INT L10;
 BOOL L111_;
 INT L121_;
 INT L13;
 AM_CURSOR_POS L14;
 dAM L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 AM_CURSOR_POS L21;
 OB L22;
 dAM_STMT L23;
 INT L24;
 BOOL L251_;
 create_self = ((A_STAC1244966098) NULL);
 L2=ZALLOC(sizeof(struct A_STAC1244966098_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=A_STAC1244966098_tag;
 L1 = ((A_STAC1244966098) L2);
 res = L1;
 create_self1 = ((FLISTA886630705) NULL);
 ret_val1 = ((FLISTA886630705) NULL);
 SATTR(res,s,ret_val1);
 ret_val = res;
 tst = ret_val;
 branch1 = 0;
 while (1) {
  tcur = ATTR(self,cur);
  if (tcur==NULL) {
   goto other327;
  } else
  switch (TAG(tcur)) {
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
    goto after_loop; break;
   default: ;
   other327: ;
  }
  push_self = tst;
  create_self2 = ((AM_CURSOR_POS) NULL);
  create_curr_stmt = ATTR(self,cur);
  create_m = ATTR(self,mark);
  create_branch = branch1;
  L4=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=AM_CURSOR_POS_tag;
  L3 = ((AM_CURSOR_POS) L4);
  r = L3;
  SATTR(r,stmt,create_curr_stmt);
  SATTR(r,branch,create_branch);
  SATTR(r,mark,create_m);
  ret_val2 = r;
  push_e = ret_val2;
  SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
  size_self = ATTR(self,stack1);
  ret_val3 = FLISTA587993904(ATTR(size_self,s));
  L5 = ret_val3;
  L61_=(L5)==(0); 
  if (L61_) {
   t = ATTR(self,top1);
   switch (TAG(t)) {
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
     L7 = am;
     (*dAM_ST1372701974[TAG(L7)])(L7, ((dAM_STMT) t)); break;
    default: ;
     FATAL("No applicable type in typecase\nin AM_CURSOR::insert_stmt_before_stmt($AM_STMT)\n./Back/cursor.sa:2416:20");
   }
   SATTR(self,top1,((dAM) am));
   AM_CUR1869307717(self);
   return;
  }
  pop_self = ATTR(self,stack1);
  ret_val4 = FLISTA971253650(ATTR(pop_self,s));
  s = ret_val4;
  SATTR(self,cur,ATTR(s,stmt));
  SATTR(self,mark,ATTR(s,mark));
  branch1 = ATTR(s,branch);
 }
 after_loop: ;
 push_self1 = tst;
 create_self3 = ((AM_CURSOR_POS) NULL);
 create_curr_stmt1 = ATTR(self,cur);
 create_m1 = ATTR(self,mark);
 create_branch1 = branch1;
 L9=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_CURSOR_POS_tag;
 L8 = ((AM_CURSOR_POS) L9);
 r1 = L8;
 SATTR(r1,stmt,create_curr_stmt1);
 SATTR(r1,branch,create_branch1);
 SATTR(r1,mark,create_m1);
 ret_val5 = r1;
 push_e1 = ret_val5;
 SATTR(push_self1,s,FLISTA1475379054(ATTR(push_self1,s), push_e1));
 size_self1 = ATTR(self,stack1);
 ret_val6 = FLISTA587993904(ATTR(size_self1,s));
 L10 = ret_val6;
 L111_=(L10)==(0); 
 if (L111_) {
  AM_CUR315787800(self, ((dAM) NULL), 0, am);
 }
 else {
  pop_self1 = ATTR(self,stack1);
  ret_val7 = FLISTA971253650(ATTR(pop_self1,s));
  s1 = ret_val7;
  AM_CUR315787800(self, ATTR(s1,stmt), ATTR(s1,branch), am);
  push_self2 = ATTR(self,stack1);
  push_e2 = s1;
  SATTR(push_self2,s,FLISTA1475379054(ATTR(push_self2,s), push_e2));
 }
 aml = am;
 while (1) {
  L121_=SYSID(aml); 
  L13 = L121_;
  top_self = tst;
  ret_val8 = FLISTA860779230(ATTR(top_self,s));
  L14=ret_val8;
  L15 = ATTR(L14,stmt);
  L161_=SYSID(L15); 
  L17 = L161_;
  L181_=(L13)==(L17); 
  L19 = L181_;
  L201_=!(L19); 
  if (L201_) {
  }
  else {
   goto after_loop1;
  }
  push_self3 = ATTR(self,stack1);
  create_self4 = ((AM_CURSOR_POS) NULL);
  create_curr_stmt2 = ((dAM) aml);
  create_m2 = FALSE;
  create_branch2 = 999999;
  L22=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=AM_CURSOR_POS_tag;
  L21 = ((AM_CURSOR_POS) L22);
  r2 = L21;
  SATTR(r2,stmt,create_curr_stmt2);
  SATTR(r2,branch,create_branch2);
  SATTR(r2,mark,create_m2);
  ret_val9 = r2;
  push_e3 = ret_val9;
  SATTR(push_self3,s,FLISTA1475379054(ATTR(push_self3,s), push_e3));
  L23 = aml;
  aml = (*dAM_ST775224435[TAG(L23)])(L23);
 }
 after_loop1: ;
 while (1) {
  size_self2 = tst;
  ret_val10 = FLISTA587993904(ATTR(size_self2,s));
  L24 = ret_val10;
  L251_=(0)<(L24); 
  if (L251_) {
  }
  else {
   goto after_loop2;
  }
  push_self4 = ATTR(self,stack1);
  pop_self2 = tst;
  ret_val11 = FLISTA971253650(ATTR(pop_self2,s));
  push_e4 = ret_val11;
  SATTR(push_self4,s,FLISTA1475379054(ATTR(push_self4,s), push_e4));
 }
 after_loop2: ;
 pop_self3 = ATTR(self,stack1);
 ret_val12 = FLISTA971253650(ATTR(pop_self3,s));
 s2 = ret_val12;
 SATTR(self,cur,ATTR(s2,stmt));
 SATTR(self,mark,ATTR(s2,mark));
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR315787800(AM_CURSOR self, dAM am, INT branch1, dAM_STMT w) {
 dAM_STMT last1;
 dAM tmp1;
 FLISTdAM_STMT aget_self;
 INT aget_ind = INT_zero;
 dAM_STMT ret_val;
 FLISTdAM_STMT aset_self;
 INT aset_ind = INT_zero;
 dAM_STMT aset_val;
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val1;
 FLISTdAM_STMT aset_self1;
 INT aset_ind1 = INT_zero;
 dAM_STMT aset_val1;
 FLISTdAM_STMT aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_STMT ret_val2;
 FLISTdAM_STMT aset_self2;
 INT aset_ind2 = INT_zero;
 dAM_STMT aset_val2;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT create_self1;
 OUT ret_val5;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val7;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT create_self3;
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
 OUT ret_val14;
 FILE1 stdout_self5;
 FILE1 ret_val15;
 FILE1 r5;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val16;
 FILE1 stdout_self6;
 FILE1 ret_val17;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val18;
 FILE1 r7;
 UNIX exit_self;
 INT exit_code = INT_zero;
 dAM_STMT L1;
 BOOL L2;
 BOOL L31_;
 dAM_STMT L4;
 dAM_STMT L5;
 BOOL L61_;
 dAM_STMT L7;
 dAM_STMT L8;
 dAM_STMT L9;
 BOOL L101_;
 dAM_STMT L11;
 BOOL L121_;
 dAM_STMT L13;
 BOOL L141_;
 dAM_STMT L15;
 BOOL L161_;
 dAM_STMT L17;
 INT L18;
 INT L191_;
 INT L20;
 BOOL L211_;
 dAM_STMT L22;
 INT L231_;
 dAM_STMT L241_;
 dAM_STMT L25;
 INT L261_;
 INT L28;
 INT L291_;
 INT L30;
 BOOL L321_;
 dAM_STMT L33;
 INT L341_;
 dAM_STMT L351_;
 dAM_STMT L36;
 INT L371_;
 BOOL L391_;
 dAM_STMT L40;
 BOOL L411_;
 dAM_STMT L42;
 INT L43;
 INT L441_;
 INT L45;
 BOOL L461_;
 dAM_STMT L47;
 INT L481_;
 dAM_STMT L491_;
 dAM_STMT L50;
 INT L511_;
 BOOL L531_;
 dAM_STMT L54;
 extern STR Trying1677464978;
 FILE1 L55;
 OB L56;
 extern STR Statem239123203;
 FILE1 L58;
 OB L59;
 extern STR Trying914151009;
 FILE1 L62;
 OB L63;
 extern STR dAM1;
 FILE1 L65;
 OB L66;
 INT L681_;
 INT L69;
 STR L701_;
 FILE1 L71;
 OB L72;
 extern STR branch2;
 FILE1 L74;
 OB L75;
 dSTR L77;
 OB L78;
 FILE1 L79;
 OB L80;
 extern STR name9;
 FILE1 L82;
 OB L83;
 last1 = w;
 while (1) {
  L1 = last1;
  L2 = ((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL));
  L31_=!(L2); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L4 = last1;
  last1 = (*dAM_ST775224435[TAG(L4)])(L4);
 }
 after_loop: ;
 if ((am==((dAM) NULL))) {
  tmp1 = ATTR(self,top1);
  switch (TAG(tmp1)) {
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
    L5 = last1;
    (*dAM_ST1372701974[TAG(L5)])(L5, ((dAM_STMT) tmp1)); break;
   default: ;
    FATAL("No applicable type in typecase\nin AM_CURSOR::insert_stmt($AM,INT,$AM_STMT)\n./Back/cursor.sa:2336:17");
  }
  SATTR(self,top1,((dAM) w));
  return;
 }
 if (am==NULL) {
  goto other325;
 } else
 switch (TAG(am)) {
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
   L61_=(branch1)==(999999); 
   if (L61_) {
    L7 = ((dAM_STMT) am);
    L8 = last1;
    (*dAM_ST1372701974[TAG(L8)])(L8, (*dAM_ST775224435[TAG(L7)])(L7));
    L9 = ((dAM_STMT) am);
    (*dAM_ST1372701974[TAG(L9)])(L9, w);
    return;
   } break;
  default: ;
  other325: ;
 }
 if (am==NULL) {
  goto other326;
 } else
 switch (TAG(am)) {
  case AM_IF_STMT_tag:
   L101_=(branch1)==(2); 
   if (L101_) {
    L11 = last1;
    (*dAM_ST1372701974[TAG(L11)])(L11, ATTR(((AM_IF_STMT) am),if_true));
    SATTR(((AM_IF_STMT) am),if_true,w);
   }
   L121_=(branch1)==(3); 
   if (L121_) {
    L13 = last1;
    (*dAM_ST1372701974[TAG(L13)])(L13, ATTR(((AM_IF_STMT) am),if_false));
    SATTR(((AM_IF_STMT) am),if_false,w);
   } break;
  case AM_LOOP_STMT_tag:
   L141_=(branch1)==(1); 
   if (L141_) {
    L15 = last1;
    (*dAM_ST1372701974[TAG(L15)])(L15, ATTR(((AM_LOOP_STMT) am),init));
    SATTR(((AM_LOOP_STMT) am),init,w);
   }
   L161_=(branch1)==(2); 
   if (L161_) {
    L17 = last1;
    (*dAM_ST1372701974[TAG(L17)])(L17, ATTR(((AM_LOOP_STMT) am),body));
    SATTR(((AM_LOOP_STMT) am),body,w);
   } break;
  case AM_CASE_STMT_tag:
   L18 = FLISTd355912233(ATTR(((AM_CASE_STMT) am),stmts));
   L191_=INTPLUS(L18,2); 
   L20 = L191_;
   L211_=(branch1)==(L20); 
   if (L211_) {
    L22 = last1;
    (*dAM_ST1372701974[TAG(L22)])(L22, ATTR(((AM_CASE_STMT) am),else_stmts));
    SATTR(((AM_CASE_STMT) am),else_stmts,w);
   }
   else {
    aget_self = ATTR(((AM_CASE_STMT) am),stmts);
    L231_=INTMINUS(branch1,2); 
    aget_ind = L231_;
    L241_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self,aget_ind); 
    ret_val = L241_;
    L25 = last1;
    (*dAM_ST1372701974[TAG(L25)])(L25, ret_val);
    aset_self = ATTR(((AM_CASE_STMT) am),stmts);
    L261_=INTMINUS(branch1,2); 
    aset_ind = L261_;
    aset_val = w;
    SARR((FLISTdAM_STMT)aset_self,aset_ind,(dAM_STMT)aset_val); 
    ;
   } break;
  case AM_TYPECASE_STMT_tag:
   L28 = FLISTd355912233(ATTR(((AM_TYPECASE_STMT) am),stmts));
   L291_=INTPLUS(L28,2); 
   L30 = L291_;
   L321_=(branch1)==(L30); 
   if (L321_) {
    L33 = last1;
    (*dAM_ST1372701974[TAG(L33)])(L33, ATTR(((AM_TYPECASE_STMT) am),else_stmts));
    SATTR(((AM_TYPECASE_STMT) am),else_stmts,w);
   }
   else {
    aget_self1 = ATTR(((AM_TYPECASE_STMT) am),stmts);
    L341_=INTMINUS(branch1,2); 
    aget_ind1 = L341_;
    L351_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
    ret_val1 = L351_;
    L36 = last1;
    (*dAM_ST1372701974[TAG(L36)])(L36, ret_val1);
    aset_self1 = ATTR(((AM_TYPECASE_STMT) am),stmts);
    L371_=INTMINUS(branch1,2); 
    aset_ind1 = L371_;
    aset_val1 = w;
    SARR((FLISTdAM_STMT)aset_self1,aset_ind1,(dAM_STMT)aset_val1); 
    ;
   } break;
  case AM_INITIAL_STMT_tag:
   L391_=(branch1)==(1); 
   if (L391_) {
    L40 = last1;
    (*dAM_ST1372701974[TAG(L40)])(L40, ATTR(((AM_INITIAL_STMT) am),stmts));
    SATTR(((AM_INITIAL_STMT) am),stmts,w);
   } break;
  case AM_PROTECT_STMT_tag:
   L411_=(branch1)==(1); 
   if (L411_) {
    L42 = last1;
    (*dAM_ST1372701974[TAG(L42)])(L42, ATTR(((AM_PROTECT_STMT) am),body));
    SATTR(((AM_PROTECT_STMT) am),body,w);
   }
   else {
    L43 = FLISTd355912233(ATTR(((AM_PROTECT_STMT) am),stmts));
    L441_=INTPLUS(L43,2); 
    L45 = L441_;
    L461_=(branch1)==(L45); 
    if (L461_) {
     L47 = last1;
     (*dAM_ST1372701974[TAG(L47)])(L47, ATTR(((AM_PROTECT_STMT) am),else_stmts));
     SATTR(((AM_PROTECT_STMT) am),else_stmts,w);
    }
    else {
     aget_self2 = ATTR(((AM_PROTECT_STMT) am),stmts);
     L481_=INTMINUS(branch1,2); 
     aget_ind2 = L481_;
     L491_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self2,aget_ind2); 
     ret_val2 = L491_;
     L50 = last1;
     (*dAM_ST1372701974[TAG(L50)])(L50, ret_val2);
     aset_self2 = ATTR(((AM_PROTECT_STMT) am),stmts);
     L511_=INTMINUS(branch1,2); 
     aset_ind2 = L511_;
     aset_val2 = w;
     SARR((FLISTdAM_STMT)aset_self2,aset_ind2,(dAM_STMT)aset_val2); 
     ;
    }
   } break;
  case AM_STMT_EXPR_tag:
   L531_=(branch1)==(1); 
   if (L531_) {
    L54 = last1;
    (*dAM_ST1372701974[TAG(L54)])(L54, ATTR(((AM_STMT_EXPR) am),stmts));
    SATTR(((AM_STMT_EXPR) am),stmts,w);
   } break;
  default: ;
  other326: ;
   create_self = ((OUT) NULL);
   ret_val3 = create_self;
   plus_self = ret_val3;
   plus_s = ((STR) &Trying1677464978);
   stdout_self = ((FILE1) NULL);
   L56=ZALLOC(sizeof(struct FILE1_struct));
   if (L56==NULL) FATAL("Unable to allocate more memory");
   ((OB)L56)->header.tag=FILE1_tag;
   L55 = ((FILE1) L56);
   r = L55;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   create_self1 = ((OUT) NULL);
   ret_val5 = create_self1;
   plus_self1 = ret_val5;
   plus_s1 = ((STR) &Statem239123203);
   stdout_self1 = ((FILE1) NULL);
   L59=ZALLOC(sizeof(struct FILE1_struct));
   if (L59==NULL) FATAL("Unable to allocate more memory");
   ((OB)L59)->header.tag=FILE1_tag;
   L58 = ((FILE1) L59);
   r1 = L58;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   AM_OUT1439666796(((AM_OUT) NULL), ((dAM) w));
   create_self2 = ((OUT) NULL);
   ret_val7 = create_self2;
   plus_self2 = ret_val7;
   plus_s2 = ((STR) &Trying914151009);
   stdout_self2 = ((FILE1) NULL);
   L63=ZALLOC(sizeof(struct FILE1_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   ((OB)L63)->header.tag=FILE1_tag;
   L62 = ((FILE1) L63);
   r2 = L62;
   
   SATTR(r2,fp,stdout);
   ret_val8 = r2;
   FILE_plus_STR(ret_val8, plus_s2);
   create_self3 = ((OUT) NULL);
   ret_val9 = create_self3;
   plus_self3 = ret_val9;
   plus_s3 = ((STR) &dAM1);
   stdout_self3 = ((FILE1) NULL);
   L66=ZALLOC(sizeof(struct FILE1_struct));
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=FILE1_tag;
   L65 = ((FILE1) L66);
   r3 = L65;
   
   SATTR(r3,fp,stdout);
   ret_val11 = r3;
   FILE_plus_STR(ret_val11, plus_s3);
   ret_val10 = plus_self3;
   plus_self4 = ret_val10;
   L681_=SYSTP(am); 
   L69 = L681_;
   L701_=SYSSTRFORTP(L69); 
   plus_s4 = L701_;
   stdout_self4 = ((FILE1) NULL);
   L72=ZALLOC(sizeof(struct FILE1_struct));
   if (L72==NULL) FATAL("Unable to allocate more memory");
   ((OB)L72)->header.tag=FILE1_tag;
   L71 = ((FILE1) L72);
   r4 = L71;
   
   SATTR(r4,fp,stdout);
   ret_val13 = r4;
   FILE_plus_STR(ret_val13, plus_s4);
   ret_val12 = plus_self4;
   plus_self5 = ret_val12;
   plus_s5 = ((STR) &branch2);
   stdout_self5 = ((FILE1) NULL);
   L75=ZALLOC(sizeof(struct FILE1_struct));
   if (L75==NULL) FATAL("Unable to allocate more memory");
   ((OB)L75)->header.tag=FILE1_tag;
   L74 = ((FILE1) L75);
   r5 = L74;
   
   SATTR(r5,fp,stdout);
   ret_val15 = r5;
   FILE_plus_STR(ret_val15, plus_s5);
   ret_val14 = plus_self5;
   plus_self6 = ret_val14;
   L78=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L78==NULL) FATAL("Unable to allocate more memory");
   memset(L78,0,sizeof(struct INT_boxed_struct));
   ((OB)L78)->header.tag=INT_tag;
   L77 = (dSTR)((INT_boxed) L78);
   ((INT_boxed) L77)->immutable_part = branch1;
   plus_s6 = L77;
   stdout_self6 = ((FILE1) NULL);
   L80=ZALLOC(sizeof(struct FILE1_struct));
   if (L80==NULL) FATAL("Unable to allocate more memory");
   ((OB)L80)->header.tag=FILE1_tag;
   L79 = ((FILE1) L80);
   r6 = L79;
   
   SATTR(r6,fp,stdout);
   ret_val17 = r6;
   FILE_plus_dSTR(ret_val17, plus_s6);
   ret_val16 = plus_self6;
   plus_self7 = ret_val16;
   plus_s7 = ((STR) &name9);
   stdout_self7 = ((FILE1) NULL);
   L83=ZALLOC(sizeof(struct FILE1_struct));
   if (L83==NULL) FATAL("Unable to allocate more memory");
   ((OB)L83)->header.tag=FILE1_tag;
   L82 = ((FILE1) L83);
   r7 = L82;
   
   SATTR(r7,fp,stdout);
   ret_val18 = r7;
   FILE_plus_STR(ret_val18, plus_s7);
   AM_OUT1439666796(((AM_OUT) NULL), am);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR413226357(AM_CURSOR self, AM_CURSOR c) {
 A_STAC1244966098 L11;
 AM_CURSOR_POS orig;
 A_STAC1244966098 L21;
 AM_CURSOR_POS sf;
 A_STAC1244966098 topb_self;
 AM_CURSOR_POS ret_val;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 BOOL topb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val1;
 A_STAC1244966098 topb_self1;
 AM_CURSOR_POS ret_val2;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 BOOL topb_if_first1 = BOOL_zero;
 FLISTA886630705 aget_self1;
 INT aget_ind1 = INT_zero;
 AM_CURSOR_POS ret_val3;
 INT L8;
 INT L9;
 INT L101_;
 INT L71_,L71_m;
 AM_CURSOR_POS L121_;
 INT L14;
 INT L15;
 INT L161_;
 INT L131_,L131_m;
 AM_CURSOR_POS L171_;
 BOOL L18;
 dAM L19;
 INT L201_;
 INT L22;
 dAM L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 INT L27;
 INT L28;
 BOOL L291_;
 {
  BOOL f_L71_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
  L11 = ATTR(c,stack1);
  topb_self = L11;
  topb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(topb_self,s);
   if (f_L71_) {
    f_L71_ = FALSE;
    L9 = FLISTA587993904(ATTR(topb_self,s));
    L101_=INTMINUS(L9,1); 
    L31 = L101_;
    L41 = 0;
    L71_=L31+1;L71_m=L41; 
   }
   if(L71_--<=L71_m)  goto after_loop; 
   aget_ind = L71_;
   L121_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val1 = L121_;
   orig = ret_val1;
   {
    BOOL f_L131_ = TRUE;
    /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
    L21 = ATTR(self,stack1);
    topb_self1 = L21;
    topb_if_first1 = TRUE;
    while (1) {
     aget_self1 = ATTR(topb_self1,s);
     if (f_L131_) {
      f_L131_ = FALSE;
      L15 = FLISTA587993904(ATTR(topb_self1,s));
      L161_=INTMINUS(L15,1); 
      L51 = L161_;
      L61 = 0;
      L131_=L51+1;L131_m=L61; 
     }
     if(L131_--<=L131_m)  goto after_loop1; 
     aget_ind1 = L131_;
     L171_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self1,aget_ind1); 
     ret_val3 = L171_;
     sf = ret_val3;
     L19 = ATTR(sf,stmt);
     L201_=SYSID(L19); 
     L22 = L201_;
     L23 = ATTR(orig,stmt);
     L241_=SYSID(L23); 
     L25 = L241_;
     L261_=(L22)==(L25); 
     if (L261_) {
      L27 = ATTR(sf,branch);
      L28 = ATTR(orig,branch);
      L291_=(L27)==(L28); 
      L18 = L291_;
     } else {
      L18 = FALSE;
     }
     if (L18) {
      SATTR(sf,mark,ATTR(orig,mark));
      goto after_loop1;
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
}
