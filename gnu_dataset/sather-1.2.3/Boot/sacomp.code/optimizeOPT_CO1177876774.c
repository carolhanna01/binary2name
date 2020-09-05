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

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct EXPR_HOISTED_struct {/* layout for EXPR_HOISTED */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct AM_LOCAL_EXPR_struct *local1;
 BOOL is_safe;
 } *EXPR_HOISTED;

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

typedef struct OPT_CONST_struct {/* layout for OPT_CONST */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_CONST;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

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

typedef struct AM_EXPR_STMT_struct {/* layout for AM_EXPR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *expr;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_EXPR_STMT;

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

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

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

typedef struct AM_ROUT_DEF_struct {/* layout for AM_ROUT_DEF */
 OB_HEADER header;
 struct dAM_STMT_struct *code1;
 struct AM_LOCAL_EXPR_struct *rres;
 struct AM_LOCAL_EXPR_struct *specul_prefetch;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FLISTA725283029_struct *locals1;
 INT num_yields;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 BOOL is_abstract;
 BOOL is_attach_routin;
 BOOL is_clean;
 BOOL is_external;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL n_export_call;
 BOOL n_export_locals;
 BOOL n_import_call;
 BOOL n_import_locals;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *AM_ROUT_DEF;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTE1761212196_struct {/* layout for FLIST{EXPR_HOISTED} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct EXPR_HOISTED_struct *arr_part[1];
 } *FLISTE1761212196;

typedef struct FLISTE18407748_frame_struct {
 INT state;
 FLISTE1761212196 self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTE18407748_frame;

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
extern PROG OPT_CO233462019;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST SFILE_ID (**dAM_EX2051891691)(dAM_EXPR);

extern RETURNED_CONST SFILE_ID (**dAM_ST1805574914)(dAM_STMT);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AM_LOOP_STMT AM_CUR153953493(AM_CURSOR);
BOOL AM_CUR1097584212(AM_CURSOR);
BOOL AM_CUR1481630356(AM_CURSOR);
BOOL AM_CUR1926902782(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR2013152412(AM_CURSOR);
BOOL AM_CUR2066246826(AM_CURSOR);
BOOL AM_CUR2135357807(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR262177482(AM_CURSOR);
BOOL AM_CUR750559803(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR762735158(AM_CURSOR, dAM);
BOOL AM_CUR954266477(AM_CURSOR);
BOOL OPT_CO1865909003(OPT_CONST, AM_ROUT_DEF, INT);
BOOL OPT_CO189407210(OPT_CONST, dAM_EXPR, AM_CURSOR, BOOL, BOOL);
BOOL OPT_CO197759827(OPT_CONST, dAM);
BOOL OPT_CO2013054955(OPT_CONST, dAM);
BOOL OPT_CO372535654(OPT_CONST, AM_ASSIGN_STMT, AM_CURSOR, BOOL, BOOL);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTE1761212196 FLISTE1031401542(FLISTE1761212196, EXPR_HOISTED);
INT FLISTE18407748(FLISTE18407748_frame);
INT STR_sizerINT(STR);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap2004550586(STR, STR);
STR STR_pl1270664985(STR, dSTR);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
void AM_CUR1223064464(AM_CURSOR);
void AM_CUR191948285(AM_CURSOR);
void AM_CUR1968472392(AM_CURSOR, dAM_STMT);
void AM_CUR352782528(AM_CURSOR, dAM_STMT);
void AM_CUR897865267(AM_CURSOR);
void AM_CUR915996644(AM_CURSOR, dAM_EXPR);
void AM_OUT1439666796(AM_OUT, dAM);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void OPT_CO160961835(OPT_CONST, dAM);
void OPT_CO43809499(OPT_CONST, dAM);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL OPT_CO1865909003(OPT_CONST self, AM_ROUT_DEF func, INT pass1) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 BOOL res = BOOL_zero;
 ARRAYBOOL after_loop_break;
 AM_CURSOR L11;
 dAM am;
 dAM_STMT b1;
 dAM_STMT br1 = ((dAM_STMT) NULL);
 AM_IF_STMT nif;
 dAM_STMT last1;
 BOOL hoisted = BOOL_zero;
 BOOL in_conditional = BOOL_zero;
 BOOL need_safe_attr = BOOL_zero;
 AM_EXPR_STMT se;
 FLISTE1761212196 e;
 BOOL found_expr = BOOL_zero;
 FLISTE1761212196 L21;
 INT i = INT_zero;
 AM_LOCAL_EXPR l;
 AM_ASSIGN_STMT as;
 AM_COMMENT_STMT comment1;
 BOOL in_conditional1 = BOOL_zero;
 BOOL need_safe_attr1 = BOOL_zero;
 BOOL hoisted1 = BOOL_zero;
 BOOL in_conditional2 = BOOL_zero;
 BOOL need_safe_attr2 = BOOL_zero;
 AM_COMMENT_STMT comment2;
 dAM tam;
 OPT_CONST prog_self;
 PROG ret_val1;
 ARRAYBOOL create_self;
 INT create_n = INT_zero;
 ARRAYBOOL ret_val2;
 OPT_CONST prog_self1;
 PROG ret_val3;
 AM_IF_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_STMT ret_val4;
 AM_IF_STMT r;
 OPT_CONST prog_self2;
 PROG ret_val5;
 OPT_CONST prog_self3;
 PROG ret_val6;
 OUT create_self2;
 OUT ret_val7;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val8;
 FILE1 r1;
 OPT_CONST prog_self4;
 PROG ret_val9;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val10 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val11 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 OPT_CONST prog_self5;
 PROG ret_val13;
 OPT_CONST prog_self6;
 PROG ret_val14;
 AM_EXPR_STMT create_self3;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val15;
 AM_EXPR_STMT r2;
 OPT_CONST prog_self7;
 PROG ret_val16;
 OUT create_self4;
 OUT ret_val17;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val18;
 FILE1 r3;
 OUT create_self5;
 OUT ret_val19;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val20;
 FILE1 stdout_self2;
 FILE1 ret_val21;
 FILE1 r4;
 OUT plus_self3;
 dSTR plus_s3;
 OUT ret_val22;
 FILE1 stdout_self3;
 FILE1 ret_val23;
 FILE1 r5;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val24;
 FILE1 r6;
 FLISTE1761212196 aget_self;
 INT aget_ind = INT_zero;
 EXPR_HOISTED ret_val25;
 OPT_CONST prog_self8;
 PROG ret_val26;
 OPT_CONST prog_self9;
 PROG ret_val27;
 OUT create_self6;
 OUT ret_val28;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val29;
 FILE1 r7;
 OUT create_self7;
 OUT ret_val30;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val31;
 FILE1 stdout_self6;
 FILE1 ret_val32;
 FILE1 r8;
 OUT plus_self7;
 dSTR plus_s7;
 OUT ret_val33;
 FILE1 stdout_self7;
 FILE1 ret_val34;
 FILE1 r9;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val35;
 FILE1 r10;
 FLISTE1761212196 aget_self1;
 INT aget_ind1 = INT_zero;
 EXPR_HOISTED ret_val36;
 FLISTE1761212196 aget_self2;
 INT aget_ind2 = INT_zero;
 EXPR_HOISTED ret_val37;
 FLISTE1761212196 aget_self3;
 INT aget_ind3 = INT_zero;
 EXPR_HOISTED ret_val38;
 OPT_CONST new_local_self;
 AM_ROUT_DEF new_local_func;
 dTP new_local_tp;
 AM_LOCAL_EXPR ret_val39;
 AM_LOCAL_EXPR lc;
 AM_LOCAL_EXPR create_self8;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val40;
 AM_LOCAL_EXPR r11;
 AM_ASSIGN_STMT create_self9;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val41;
 AM_ASSIGN_STMT r12;
 AM_COMMENT_STMT create_self10;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val42;
 AM_COMMENT_STMT r13;
 STR plus_self9;
 STR plus_sarg;
 STR ret_val43;
 EXPR_HOISTED create_self11;
 dAM_EXPR create_e;
 BOOL create_s = BOOL_zero;
 AM_LOCAL_EXPR create_l;
 EXPR_HOISTED ret_val44;
 EXPR_HOISTED r14;
 OPT_CONST prog_self10;
 PROG ret_val45;
 OUT create_self12;
 OUT ret_val46;
 OUT plus_self10;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val47;
 FILE1 r15;
 OUT create_self13;
 OUT ret_val48;
 OUT plus_self11;
 STR plus_s10;
 OUT ret_val49;
 FILE1 stdout_self10;
 FILE1 ret_val50;
 FILE1 r16;
 OUT plus_self12;
 dSTR plus_s11;
 OUT ret_val51;
 FILE1 stdout_self11;
 FILE1 ret_val52;
 FILE1 r17;
 OUT plus_self13;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val53;
 FILE1 r18;
 OPT_CONST prog_self11;
 PROG ret_val54;
 OPT_CONST prog_self12;
 PROG ret_val55;
 OPT_CONST prog_self13;
 PROG ret_val56;
 OUT create_self14;
 OUT ret_val57;
 OUT plus_self14;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val58;
 FILE1 r19;
 OUT create_self15;
 OUT ret_val59;
 OUT plus_self15;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val60;
 FILE1 r20;
 OUT create_self16;
 OUT ret_val61;
 OUT plus_self16;
 STR plus_s15;
 OUT ret_val62;
 FILE1 stdout_self15;
 FILE1 ret_val63;
 FILE1 r21;
 OUT plus_self17;
 dSTR plus_s16;
 OUT ret_val64;
 FILE1 stdout_self16;
 FILE1 ret_val65;
 FILE1 r22;
 OUT plus_self18;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val66;
 FILE1 r23;
 OUT create_self17;
 OUT ret_val67;
 OUT plus_self19;
 STR plus_s18;
 OUT ret_val68;
 FILE1 stdout_self18;
 FILE1 ret_val69;
 FILE1 r24;
 OUT plus_self20;
 dSTR plus_s19;
 OUT ret_val70;
 FILE1 stdout_self19;
 FILE1 ret_val71;
 FILE1 r25;
 OUT plus_self21;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val72;
 FILE1 r26;
 OUT create_self18;
 OUT ret_val73;
 OUT plus_self22;
 STR plus_s21;
 OUT ret_val74;
 FILE1 stdout_self21;
 FILE1 ret_val75;
 FILE1 r27;
 OPT_CONST prog_self14;
 PROG ret_val76;
 OUT plus_self23;
 dSTR plus_s22;
 OUT ret_val77;
 FILE1 stdout_self22;
 FILE1 ret_val78;
 FILE1 r28;
 OUT plus_self24;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val79;
 FILE1 r29;
 OUT create_self19;
 OUT ret_val80;
 STR plus_self25;
 STR plus_sarg1;
 STR ret_val81;
 STR plus_self26;
 STR plus_sarg2;
 STR ret_val82;
 OUT plus_self27;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val83;
 FILE1 r30;
 OPT_CONST prog_self15;
 PROG ret_val84;
 AM_ROUT_DEF is_iter_self3;
 BOOL ret_val85 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val86 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val87 = BOOL_zero;
 OPT_CONST prog_self16;
 PROG ret_val88;
 OPT_CONST prog_self17;
 PROG ret_val89;
 OUT create_self20;
 OUT ret_val90;
 OUT plus_self28;
 STR plus_s25;
 OUT ret_val91;
 FILE1 stdout_self25;
 FILE1 ret_val92;
 FILE1 r31;
 OUT plus_self29;
 dSTR plus_s26;
 OUT ret_val93;
 FILE1 stdout_self26;
 FILE1 ret_val94;
 FILE1 r32;
 OUT plus_self30;
 STR plus_s27;
 FILE1 stdout_self27;
 FILE1 ret_val95;
 FILE1 r33;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n;
 AM_COMMENT_STMT create_self21;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val96;
 AM_COMMENT_STMT r34;
 STR plus_self31;
 STR plus_sarg3;
 STR ret_val97;
 OPT_CONST prog_self18;
 PROG ret_val98;
 OPT_CONST prog_self19;
 PROG ret_val99;
 OUT create_self22;
 OUT ret_val100;
 OUT plus_self32;
 STR plus_s28;
 OUT ret_val101;
 FILE1 stdout_self28;
 FILE1 ret_val102;
 FILE1 r35;
 OUT plus_self33;
 dSTR plus_s29;
 OUT ret_val103;
 FILE1 stdout_self29;
 FILE1 ret_val104;
 FILE1 r36;
 OUT plus_self34;
 STR plus_s30;
 FILE1 stdout_self30;
 FILE1 ret_val105;
 FILE1 r37;
 OUT create_self23;
 OUT ret_val106;
 OUT plus_self35;
 STR plus_s31;
 OUT ret_val107;
 FILE1 stdout_self31;
 FILE1 ret_val108;
 FILE1 r38;
 OPT_CONST prog_self20;
 PROG ret_val109;
 OUT plus_self36;
 dSTR plus_s32;
 FILE1 stdout_self32;
 FILE1 ret_val110;
 FILE1 r39;
 OUT create_self24;
 OUT ret_val1111;
 OUT plus_self37;
 STR plus_s33;
 OUT ret_val112;
 FILE1 stdout_self33;
 FILE1 ret_val113;
 FILE1 r40;
 OUT plus_self38;
 dSTR plus_s34;
 OUT ret_val114;
 FILE1 stdout_self34;
 FILE1 ret_val115;
 FILE1 r41;
 OUT plus_self39;
 STR plus_s35;
 FILE1 stdout_self35;
 FILE1 ret_val116;
 FILE1 r42;
 OUT create_self25;
 OUT ret_val117;
 OUT plus_self40;
 STR plus_s36;
 OUT ret_val118;
 FILE1 stdout_self36;
 FILE1 ret_val119;
 FILE1 r43;
 OUT plus_self41;
 dSTR plus_s37;
 OUT ret_val120;
 FILE1 stdout_self37;
 FILE1 ret_val121;
 FILE1 r44;
 OUT plus_self42;
 STR plus_s38;
 OUT ret_val122;
 FILE1 stdout_self38;
 FILE1 ret_val123;
 FILE1 r45;
 OUT plus_self43;
 dSTR plus_s39;
 OUT ret_val124;
 FILE1 stdout_self39;
 FILE1 ret_val125;
 FILE1 r46;
 OUT plus_self44;
 STR plus_s40;
 FILE1 stdout_self40;
 FILE1 ret_val126;
 FILE1 r47;
 AM_OUT AM_one_stmt_self1;
 dAM AM_one_stmt_a1;
 dAM_STMT n11;
 ARRAYBOOL L3;
 INT L4;
 OB L5;
 INT L6;
 dAM L7;
 INT L8;
 INT L10;
 INT L13;
 INT L15;
 INT L17;
 INT L181_;
 INT L19;
 BOOL L22;
 PROG L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 BOOL L27;
 BOOL L28;
 BOOL L29;
 BOOL L30;
 dAM L31;
 INT L321_;
 INT L33;
 INT L341_;
 INT L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 AM_IF_STMT L39;
 OB L40;
 dAM_EXPR L41;
 dAM_STMT L42;
 dAM_STMT L43;
 BOOL L44;
 BOOL L451_;
 dAM_STMT L46;
 dAM_STMT L47;
 PROG L48;
 extern STR Oofwhi603346421;
 dSTAT L49;
 PROG L50;
 extern STR moveda1993889901;
 FILE1 L51;
 OB L52;
 BOOL L54;
 BOOL L55;
 BOOL L56;
 BOOL L57;
 BOOL L58;
 INT L59;
 BOOL L601_;
 PROG L61;
 BOOL L62;
 BOOL L63;
 BOOL L641_;
 BOOL L65;
 BOOL L66;
 BOOL L671_;
 STR L68;
 INT L69;
 INT L701_;
 INT L71;
 CHAR L721_;
 CHAR L73;
 BOOL L741_;
 BOOL L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 BOOL L79;
 BOOL L801_;
 BOOL L81;
 BOOL L821_;
 BOOL L83;
 INT L84;
 BOOL L851_;
 BOOL L86;
 BOOL L87;
 BOOL L88;
 BOOL L891_;
 PROG L90;
 BOOL L92;
 BOOL L931_;
 BOOL L94;
 BOOL L951_;
 PROG L96;
 extern STR Oofexp1845456289;
 dSTAT L97;
 dAM_EXPR L98;
 dAM_EXPR L99;
 AM_EXPR_STMT L100;
 OB L101;
 PROG L102;
 extern STR expres2094954656;
 FILE1 L103;
 OB L104;
 extern STR need_safe_attr11;
 FILE1 L106;
 OB L107;
 dSTR L109;
 OB L110;
 FILE1 L111;
 OB L112;
 extern STR name50;
 FILE1 L114;
 OB L115;
 AM_LOOP_STMT L117;
 BOOL L118;
 BOOL L1191_;
 INT L120;
 EXPR_HOISTED L1221_;
 EXPR_HOISTED L123;
 dAM_EXPR L124;
 PROG L125;
 extern STR Oofdou1217912041;
 dSTAT L126;
 PROG L127;
 extern STR double1994202846;
 FILE1 L128;
 OB L129;
 extern STR need_safe_attr11;
 FILE1 L131;
 OB L132;
 dSTR L134;
 OB L135;
 FILE1 L136;
 OB L137;
 extern STR name50;
 FILE1 L139;
 OB L140;
 BOOL L143;
 EXPR_HOISTED L1441_;
 EXPR_HOISTED L145;
 BOOL L1461_;
 EXPR_HOISTED L1471_;
 EXPR_HOISTED L148;
 EXPR_HOISTED L1491_;
 EXPR_HOISTED L150;
 BOOL L1511_;
 dAM_EXPR L152;
 dAM_STMT L153;
 AM_LOCAL_EXPR L154;
 OB L155;
 dAM_EXPR L156;
 AM_ASSIGN_STMT L157;
 OB L158;
 dAM_EXPR L159;
 AM_COMMENT_STMT L160;
 OB L162;
 extern STR hoiste1504429436;
 dAM_EXPR L163;
 EXPR_HOISTED L164;
 OB L165;
 PROG L166;
 extern STR expres1398214494;
 FILE1 L167;
 OB L168;
 extern STR need_safe_attr11;
 FILE1 L170;
 OB L171;
 dSTR L173;
 OB L174;
 FILE1 L175;
 OB L176;
 extern STR name50;
 FILE1 L178;
 OB L179;
 INT L182;
 BOOL L183;
 BOOL L184;
 INT L185;
 BOOL L1861_;
 BOOL L187;
 BOOL L1881_;
 BOOL L189;
 BOOL L190;
 BOOL L1911_;
 SE_CONTEXT L192;
 BOOL L193;
 BOOL L195;
 BOOL L196;
 PROG L197;
 BOOL L1981_;
 INT L199;
 BOOL L2001_;
 extern STR thefol1298497786;
 FILE1 L202;
 OB L203;
 extern STR name9;
 FILE1 L205;
 OB L206;
 BOOL L208;
 BOOL L2091_;
 BOOL L210;
 INT L211;
 BOOL L2121_;
 extern STR in_conditional11;
 FILE1 L213;
 OB L214;
 dSTR L216;
 OB L217;
 FILE1 L218;
 OB L219;
 extern STR name60;
 FILE1 L221;
 OB L222;
 extern STR need_safe_attr21;
 FILE1 L224;
 OB L225;
 dSTR L227;
 OB L228;
 FILE1 L229;
 OB L230;
 extern STR name60;
 FILE1 L232;
 OB L233;
 extern STR progvoid_checks;
 FILE1 L235;
 OB L236;
 PROG L238;
 dSTR L239;
 OB L240;
 FILE1 L242;
 OB L243;
 extern STR name9;
 FILE1 L245;
 OB L246;
 extern STR has_attr_expram;
 dSTR L248;
 OB L249;
 extern STR hoistable;
 STR L250;
 dSTR L251;
 OB L252;
 extern STR name9;
 FILE1 L253;
 OB L254;
 BOOL L256;
 BOOL L257;
 BOOL L258;
 INT L259;
 BOOL L2601_;
 PROG L262;
 BOOL L263;
 BOOL L264;
 BOOL L2651_;
 BOOL L266;
 BOOL L267;
 BOOL L2681_;
 STR L269;
 INT L270;
 INT L2711_;
 INT L272;
 CHAR L2731_;
 CHAR L274;
 BOOL L2751_;
 BOOL L276;
 BOOL L2771_;
 BOOL L278;
 BOOL L2791_;
 BOOL L280;
 BOOL L2811_;
 BOOL L282;
 INT L283;
 BOOL L2841_;
 BOOL L285;
 BOOL L286;
 BOOL L287;
 BOOL L288;
 BOOL L2891_;
 PROG L290;
 BOOL L291;
 BOOL L2921_;
 BOOL L293;
 BOOL L2941_;
 PROG L295;
 extern STR assign138801694;
 FILE1 L296;
 OB L297;
 dSTR L299;
 OB L300;
 FILE1 L301;
 OB L302;
 extern STR name50;
 FILE1 L304;
 OB L305;
 dAM_STMT L307;
 dAM_STMT L308;
 dAM_STMT L309;
 AM_COMMENT_STMT L310;
 OB L311;
 extern STR hoiste87020047;
 PROG L312;
 extern STR Oofass325928632;
 dSTAT L313;
 PROG L314;
 extern STR assign1949641357;
 FILE1 L315;
 OB L316;
 dSTR L318;
 OB L319;
 FILE1 L320;
 OB L322;
 extern STR name60;
 FILE1 L324;
 OB L325;
 extern STR progvoid_checks;
 FILE1 L327;
 OB L328;
 PROG L330;
 dSTR L331;
 OB L332;
 FILE1 L333;
 OB L334;
 extern STR has_attr_expr;
 FILE1 L336;
 OB L337;
 dSTR L339;
 OB L340;
 FILE1 L342;
 OB L343;
 extern STR name9;
 FILE1 L345;
 OB L346;
 extern STR hoistable_dest;
 FILE1 L348;
 OB L349;
 dSTR L351;
 OB L352;
 FILE1 L353;
 OB L354;
 extern STR hoistable1;
 FILE1 L356;
 OB L357;
 dSTR L359;
 OB L360;
 FILE1 L362;
 OB L363;
 extern STR name9;
 FILE1 L365;
 OB L366;
 dAM_STMT L368;
 dAM_STMT L369;
 dAM_STMT L370;
 BOOL L371;
 BOOL L3721_;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val1, ((dAM) ATTR(func,code1)));
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 res = FALSE;
 create_self = ((ARRAYBOOL) NULL);
 create_n = 30;
 L4 = create_n;
 L6=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L4)*sizeof(BOOL);
 L5=ZALLOC_LEAF_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,L6);
 ((OB)L5)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((ARRAYBOOL) L5);
 L3->asize = L4;
 ret_val2 = L3;
 after_loop_break = ret_val2;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L7 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L7;
   if (am==NULL) {
    goto other281;
   } else
   switch (TAG(am)) {
    case AM_RAISE_STMT_tag:
     L8 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L8,TRUE); 
     ; break;
    case AM_BREAK_STMT_tag:
     L10 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L10,TRUE); 
     ; break;
    case AM_ITE1809135850_tag:
     L13 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L13,TRUE); 
     ; break;
    case AM_RETURN_STMT_tag:
     L15 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L15,TRUE); 
     ; break;
    case AM_LOOP_STMT_tag:
     L17 = ATTR(c,loops);
     L181_=INTPLUS(L17,1); 
     L19 = L181_;
     SARR((ARRAYBOOL)after_loop_break,L19,FALSE); 
     ;
     prog_self1 = self;
     ret_val3 = OPT_CO233462019;
     L23=ret_val3;
     if (ATTR(L23,move_while)) {
      L241_=(2)<(pass1); 
      L25 = L241_;
      L261_=!(L25); 
      L22 = L261_;
     } else {
      L22 = FALSE;
     }
     if (L22) {
      b1 = ATTR(((AM_LOOP_STMT) am),body);
      if (b1==NULL) {
       goto other282;
      } else
      switch (TAG(b1)) {
       case AM_IF_STMT_tag:
        if ((ATTR(((AM_IF_STMT) b1),if_true)==((dAM_STMT) NULL))) {
         L29 = TRUE;
        } else {
         L29 = (ATTR(((AM_IF_STMT) b1),if_false)==((dAM_STMT) NULL));
        }
        if (L29) {
         L28 = OPT_CO2013054955(self, ((dAM) ATTR(((AM_IF_STMT) b1),test1)));
        } else {
         L28 = FALSE;
        }
        if (L28) {
         if ((ATTR(((AM_LOOP_STMT) am),first_while_move)==((dAM) NULL))) {
          L30 = TRUE;
         } else {
          L31 = ATTR(((AM_LOOP_STMT) am),first_while_move);
          L321_=SYSID(L31); 
          L33 = L321_;
          L341_=SYSID(b1); 
          L35 = L341_;
          L361_=(L33)==(L35); 
          L37 = L361_;
          L381_=!(L37); 
          L30 = L381_;
         }
         L27 = L30;
        } else {
         L27 = FALSE;
        }
        if (L27) {
         if ((ATTR(((AM_LOOP_STMT) am),first_while_move)==((dAM) NULL))) {
          SATTR(((AM_LOOP_STMT) am),first_while_move,((dAM) b1));
         }
         if ((ATTR(((AM_IF_STMT) b1),if_true)==((dAM_STMT) NULL))) {
          br1 = ATTR(((AM_IF_STMT) b1),if_false);
         }
         else {
          br1 = ATTR(((AM_IF_STMT) b1),if_true);
         }
         if (br1==NULL) {
          goto other283;
         } else
         switch (TAG(br1)) {
          case AM_BREAK_STMT_tag:
           create_self1 = ((AM_IF_STMT) NULL);
           create_source = ATTR(((AM_LOOP_STMT) am),source1);
           L40=ZALLOC(sizeof(struct AM_IF_STMT_struct));
           if (L40==NULL) FATAL("Unable to allocate more memory");
           ((OB)L40)->header.tag=AM_IF_STMT_tag;
           L39 = ((AM_IF_STMT) L40);
           r = L39;
           SATTR(r,source1,create_source);
           ret_val4 = r;
           nif = ret_val4;
           L41 = ATTR(((AM_IF_STMT) b1),test1);
           SATTR(nif,test1,(*dAM_EX1830308833[TAG(L41)])(L41));
           AM_CUR1968472392(c, ((dAM_STMT) nif));
           if ((ATTR(((AM_IF_STMT) b1),if_true)==((dAM_STMT) NULL))) {
            SATTR(nif,if_true,((dAM_STMT) am));
           }
           else {
            SATTR(nif,if_false,((dAM_STMT) am));
           }
           SATTR(((AM_LOOP_STMT) am),next,((dAM_STMT) NULL));
           L42 = ATTR(((AM_LOOP_STMT) am),body);
           SATTR(((AM_LOOP_STMT) am),body,(*dAM_ST775224435[TAG(L42)])(L42));
           last1 = ATTR(((AM_LOOP_STMT) am),body);
           if ((last1==((dAM_STMT) NULL))) {
            SATTR(((AM_LOOP_STMT) am),body,b1);
           }
           else {
            while (1) {
             L43 = last1;
             L44 = ((*dAM_ST775224435[TAG(L43)])(L43)==((dAM_STMT) NULL));
             L451_=!(L44); 
             if (L451_) {
             }
             else {
              goto after_loop1;
             }
             L46 = last1;
             last1 = (*dAM_ST775224435[TAG(L46)])(L46);
            }
            after_loop1: ;
            L47 = last1;
            (*dAM_ST1372701974[TAG(L47)])(L47, b1);
           }
           SATTR(((AM_IF_STMT) b1),next,((dAM_STMT) NULL));
           prog_self2 = self;
           ret_val5 = OPT_CO233462019;
           L48=ret_val5;
           L49 = ATTR(L48,stat1);
           (*dSTAT_incr_STR[TAG(L49)])(L49, ((STR) &Oofwhi603346421));
           res = TRUE;
           prog_self3 = self;
           ret_val6 = OPT_CO233462019;
           L50=ret_val6;
           if (ATTR(L50,opt_debug)) {
            create_self2 = ((OUT) NULL);
            ret_val7 = create_self2;
            plus_self = ret_val7;
            plus_s = ((STR) &moveda1993889901);
            stdout_self = ((FILE1) NULL);
            L52=ZALLOC(sizeof(struct FILE1_struct));
            if (L52==NULL) FATAL("Unable to allocate more memory");
            ((OB)L52)->header.tag=FILE1_tag;
            L51 = ((FILE1) L52);
            r1 = L51;
            
            SATTR(r1,fp,stdout);
            ret_val8 = r1;
            FILE_plus_STR(ret_val8, plus_s);
           } break;
          default: ;
          other283: ;
         }
        } break;
       default: ;
       other282: ;
      }
     } break;
    case AM_LOCAL_EXPR_tag: break;
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
    case AM_VOID_CONST_tag: break;
    case AM_SHARED_EXPR_tag: break;
    case AM_ANY_EXPR_tag:
    case AM_ARRAY_EXPR_tag:
    case AM_ATTR_EXPR_tag:
    case AM_AT_EXPR_tag:
    case AM_BND1124877163_tag:
    case AM_BND367211769_tag:
    case AM_BND260301329_tag:
    case AM_CLUSTER_EXPR_tag:
    case AM_CLU1286269335_tag:
    case AM_EXCEPT_EXPR_tag:
    case AM_EXT_CALL_EXPR_tag:
    case AM_FAR_EXPR_tag:
    case AM_GLOBAL_EXPR_tag:
    case AM_HERE_EXPR_tag:
    case AM_IF_EXPR_tag:
    case AM_IS_VOID_EXPR_tag:
    case AM_NEAR_EXPR_tag:
    case AM_NEW_EXPR_tag:
    case AM_ROU1916046290_tag:
    case AM_STMT_EXPR_tag:
    case AM_VAR744470097_tag:
    case AM_VAT319982528_tag:
    case AM_WHERE_EXPR_tag:
     hoisted = FALSE;
     L59 = ATTR(c,loops);
     L601_=(0)<(L59); 
     if (L601_) {
      prog_self4 = self;
      ret_val9 = OPT_CO233462019;
      L61=ret_val9;
      L58 = ATTR(L61,hoist_const);
     } else {
      L58 = FALSE;
     }
     if (L58) {
      L57 = AM_CUR1097584212(c);
     } else {
      L57 = FALSE;
     }
     if (L57) {
      L56 = AM_CUR954266477(c);
     } else {
      L56 = FALSE;
     }
     if (L56) {
      L641_=(1)<(pass1); 
      if (L641_) {
       is_iter_self = func;
       is_iter_self1 = ATTR(is_iter_self,sig1);
       is_iter_self2 = ATTR(is_iter_self1,name1);
       L66 = (is_iter_self2.str==((STR) NULL));
       L671_=!(L66); 
       if (L671_) {
        L68 = is_iter_self2.str;
        L69 = STR_sizerINT(is_iter_self2.str);
        L701_=INTMINUS(L69,1); 
        L71 = L701_;
        L721_=ARR((STR)L68,L71); 
        L73 = L721_;
        L741_=L73=='!'; 
        L65 = L741_;
       } else {
        L65 = FALSE;
       }
       ret_val12 = L65;
       ret_val11 = ret_val12;
       ret_val10 = ret_val11;
       L75 = ret_val10;
       L761_=!(L75); 
       L63 = L761_;
      } else {
       L63 = FALSE;
      }
      if (L63) {
       L62 = TRUE;
      } else {
       L77 = AM_CUR2066246826(c);
       L781_=!(L77); 
       L62 = L781_;
      }
      L55 = L62;
     } else {
      L55 = FALSE;
     }
     if (L55) {
      L79 = AM_CUR2013152412(c);
      L801_=!(L79); 
      L54 = L801_;
     } else {
      L54 = FALSE;
     }
     if (L54) {
      L81 = AM_CUR262177482(c);
      L821_=!(L81); 
      in_conditional = L821_;
      L84 = ATTR(c,loops);
      L851_=ARR((ARRAYBOOL)after_loop_break,L84); 
      if (L851_) {
       L83 = TRUE;
      } else {
       L83 = in_conditional;
      }
      need_safe_attr = L83;
      L891_=!(need_safe_attr); 
      if (L891_) {
       L88 = TRUE;
      } else {
       prog_self5 = self;
       ret_val13 = OPT_CO233462019;
       L90=ret_val13;
       L92 = ATTR(L90,void_checks);
       L931_=!(L92); 
       L88 = L931_;
      }
      if (L88) {
       L87 = TRUE;
      } else {
       L94 = OPT_CO197759827(self, am);
       L951_=!(L94); 
       L87 = L951_;
      }
      if (L87) {
       L86 = OPT_CO189407210(self, ((dAM_EXPR) am), c, need_safe_attr, in_conditional);
      } else {
       L86 = FALSE;
      }
      if (L86) {
       prog_self6 = self;
       ret_val14 = OPT_CO233462019;
       L96=ret_val14;
       L97 = ATTR(L96,stat1);
       (*dSTAT_incr_STR[TAG(L97)])(L97, ((STR) &Oofexp1845456289));
       hoisted = TRUE;
       res = TRUE;
       L98 = ((dAM_EXPR) am);
       if (((*dAM_EXPR_tprdTP[TAG(L98)])(L98)==((dTP) NULL))) {
        create_self3 = ((AM_EXPR_STMT) NULL);
        L99 = ((dAM_EXPR) am);
        create_source1 = (*dAM_EX2051891691[TAG(L99)])(L99);
        L101=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
        if (L101==NULL) FATAL("Unable to allocate more memory");
        ((OB)L101)->header.tag=AM_EXPR_STMT_tag;
        L100 = ((AM_EXPR_STMT) L101);
        r2 = L100;
        SATTR(r2,source1,create_source1);
        ret_val15 = r2;
        se = ret_val15;
        SATTR(se,expr,((dAM_EXPR) am));
        AM_CUR191948285(c);
        if (need_safe_attr) {
         OPT_CO160961835(self, ((dAM) se));
        }
        AM_CUR352782528(c, ((dAM_STMT) se));
        prog_self7 = self;
        ret_val16 = OPT_CO233462019;
        L102=ret_val16;
        if (ATTR(L102,opt_debug)) {
         create_self4 = ((OUT) NULL);
         ret_val17 = create_self4;
         plus_self1 = ret_val17;
         plus_s1 = ((STR) &expres2094954656);
         stdout_self1 = ((FILE1) NULL);
         L104=ZALLOC(sizeof(struct FILE1_struct));
         if (L104==NULL) FATAL("Unable to allocate more memory");
         ((OB)L104)->header.tag=FILE1_tag;
         L103 = ((FILE1) L104);
         r3 = L103;
         
         SATTR(r3,fp,stdout);
         ret_val18 = r3;
         FILE_plus_STR(ret_val18, plus_s1);
         AM_OUT1439666796(((AM_OUT) NULL), am);
         create_self5 = ((OUT) NULL);
         ret_val19 = create_self5;
         plus_self2 = ret_val19;
         plus_s2 = ((STR) &need_safe_attr11);
         stdout_self2 = ((FILE1) NULL);
         L107=ZALLOC(sizeof(struct FILE1_struct));
         if (L107==NULL) FATAL("Unable to allocate more memory");
         ((OB)L107)->header.tag=FILE1_tag;
         L106 = ((FILE1) L107);
         r4 = L106;
         
         SATTR(r4,fp,stdout);
         ret_val21 = r4;
         FILE_plus_STR(ret_val21, plus_s2);
         ret_val20 = plus_self2;
         plus_self3 = ret_val20;
         L110=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
         if (L110==NULL) FATAL("Unable to allocate more memory");
         memset(L110,0,sizeof(struct BOOL_boxed_struct));
         ((OB)L110)->header.tag=BOOL_tag;
         L109 = (dSTR)((BOOL_boxed) L110);
         ((BOOL_boxed) L109)->immutable_part = need_safe_attr;
         plus_s3 = L109;
         stdout_self3 = ((FILE1) NULL);
         L112=ZALLOC(sizeof(struct FILE1_struct));
         if (L112==NULL) FATAL("Unable to allocate more memory");
         ((OB)L112)->header.tag=FILE1_tag;
         L111 = ((FILE1) L112);
         r5 = L111;
         
         SATTR(r5,fp,stdout);
         ret_val23 = r5;
         FILE_plus_dSTR(ret_val23, plus_s3);
         ret_val22 = plus_self3;
         plus_self4 = ret_val22;
         plus_s4 = ((STR) &name50);
         stdout_self4 = ((FILE1) NULL);
         L115=ZALLOC(sizeof(struct FILE1_struct));
         if (L115==NULL) FATAL("Unable to allocate more memory");
         ((OB)L115)->header.tag=FILE1_tag;
         L114 = ((FILE1) L115);
         r6 = L114;
         
         SATTR(r6,fp,stdout);
         ret_val24 = r6;
         FILE_plus_STR(ret_val24, plus_s4);
        }
       }
       else {
        L117=AM_CUR153953493(c);
        e = ATTR(L117,hoisted);
        found_expr = FALSE;
        L118 = (e==((FLISTE1761212196) NULL));
        L1191_=!(L118); 
        if (L1191_) {
         {
          struct FLISTE18407748_frame_struct other3_0;
FLISTE18407748_frame noname2 = &other3_0;
          L21 = e;
          noname2->self = L21;
          noname2->state = 0;
          while (1) {
           L120 = FLISTE18407748(noname2);
           if (noname2->state == -1) {
            goto after_loop2;
           }
           i = L120;
           aget_self = e;
           aget_ind = i;
           L1221_=(EXPR_HOISTED)ARR((FLISTE1761212196)aget_self,aget_ind); 
           ret_val25 = L1221_;
           L123=ret_val25;
           L124 = ((dAM_EXPR) am);
           if ((*dAM_EX2004971809[TAG(L124)])(L124, ((OB) ATTR(L123,expr)))) {
            prog_self8 = self;
            ret_val26 = OPT_CO233462019;
            L125=ret_val26;
            L126 = ATTR(L125,stat1);
            (*dSTAT_incr_STR[TAG(L126)])(L126, ((STR) &Oofdou1217912041));
            prog_self9 = self;
            ret_val27 = OPT_CO233462019;
            L127=ret_val27;
            if (ATTR(L127,opt_debug)) {
             create_self6 = ((OUT) NULL);
             ret_val28 = create_self6;
             plus_self5 = ret_val28;
             plus_s5 = ((STR) &double1994202846);
             stdout_self5 = ((FILE1) NULL);
             L129=ZALLOC(sizeof(struct FILE1_struct));
             if (L129==NULL) FATAL("Unable to allocate more memory");
             ((OB)L129)->header.tag=FILE1_tag;
             L128 = ((FILE1) L129);
             r7 = L128;
             
             SATTR(r7,fp,stdout);
             ret_val29 = r7;
             FILE_plus_STR(ret_val29, plus_s5);
             AM_OUT1439666796(((AM_OUT) NULL), am);
             create_self7 = ((OUT) NULL);
             ret_val30 = create_self7;
             plus_self6 = ret_val30;
             plus_s6 = ((STR) &need_safe_attr11);
             stdout_self6 = ((FILE1) NULL);
             L132=ZALLOC(sizeof(struct FILE1_struct));
             if (L132==NULL) FATAL("Unable to allocate more memory");
             ((OB)L132)->header.tag=FILE1_tag;
             L131 = ((FILE1) L132);
             r8 = L131;
             
             SATTR(r8,fp,stdout);
             ret_val32 = r8;
             FILE_plus_STR(ret_val32, plus_s6);
             ret_val31 = plus_self6;
             plus_self7 = ret_val31;
             L135=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
             if (L135==NULL) FATAL("Unable to allocate more memory");
             memset(L135,0,sizeof(struct BOOL_boxed_struct));
             ((OB)L135)->header.tag=BOOL_tag;
             L134 = (dSTR)((BOOL_boxed) L135);
             ((BOOL_boxed) L134)->immutable_part = need_safe_attr;
             plus_s7 = L134;
             stdout_self7 = ((FILE1) NULL);
             L137=ZALLOC(sizeof(struct FILE1_struct));
             if (L137==NULL) FATAL("Unable to allocate more memory");
             ((OB)L137)->header.tag=FILE1_tag;
             L136 = ((FILE1) L137);
             r9 = L136;
             
             SATTR(r9,fp,stdout);
             ret_val34 = r9;
             FILE_plus_dSTR(ret_val34, plus_s7);
             ret_val33 = plus_self7;
             plus_self8 = ret_val33;
             plus_s8 = ((STR) &name50);
             stdout_self8 = ((FILE1) NULL);
             L140=ZALLOC(sizeof(struct FILE1_struct));
             if (L140==NULL) FATAL("Unable to allocate more memory");
             ((OB)L140)->header.tag=FILE1_tag;
             L139 = ((FILE1) L140);
             r10 = L139;
             
             SATTR(r10,fp,stdout);
             ret_val35 = r10;
             FILE_plus_STR(ret_val35, plus_s8);
            }
            res = TRUE;
            found_expr = TRUE;
            aget_self1 = e;
            aget_ind1 = i;
            L1441_=(EXPR_HOISTED)ARR((FLISTE1761212196)aget_self1,aget_ind1); 
            ret_val36 = L1441_;
            L145=ret_val36;
            if (ATTR(L145,is_safe)) {
             L1461_=!(need_safe_attr); 
             L143 = L1461_;
            } else {
             L143 = FALSE;
            }
            if (L143) {
             aget_self2 = e;
             aget_ind2 = i;
             L1471_=(EXPR_HOISTED)ARR((FLISTE1761212196)aget_self2,aget_ind2); 
             ret_val37 = L1471_;
             L148=ret_val37;
             OPT_CO43809499(self, ((dAM) ATTR(L148,expr)));
            }
            aget_self3 = e;
            aget_ind3 = i;
            L1491_=(EXPR_HOISTED)ARR((FLISTE1761212196)aget_self3,aget_ind3); 
            ret_val38 = L1491_;
            L150=ret_val38;
            AM_CUR915996644(c, ((dAM_EXPR) ATTR(L150,local1)));
            goto after_loop2;
           }
          }
         }
         after_loop2: ;
        }
        L1511_=!(found_expr); 
        if (L1511_) {
         new_local_self = self;
         new_local_func = func;
         L152 = ((dAM_EXPR) am);
         new_local_tp = (*dAM_EXPR_tprdTP[TAG(L152)])(L152);
         create_self8 = ((AM_LOCAL_EXPR) NULL);
         L153 = ATTR(new_local_func,code1);
         create_src = (*dAM_ST1805574914[TAG(L153)])(L153);
         create_name = IDENT_zero;
         create_tp = new_local_tp;
         L155=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
         if (L155==NULL) FATAL("Unable to allocate more memory");
         ((OB)L155)->header.tag=AM_LOCAL_EXPR_tag;
         L154 = ((AM_LOCAL_EXPR) L155);
         r11 = L154;
         SATTR(r11,source1,create_src);
         SATTR(r11,name1,create_name);
         SATTR(r11,tp_at,create_tp);
         ret_val40 = r11;
         lc = ret_val40;
         SATTR(new_local_func,locals1,FLISTA1817671564(ATTR(new_local_func,locals1), lc));
         ret_val39 = lc;
         l = ret_val39;
         create_self9 = ((AM_ASSIGN_STMT) NULL);
         L156 = ((dAM_EXPR) am);
         create_source2 = (*dAM_EX2051891691[TAG(L156)])(L156);
         L158=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
         if (L158==NULL) FATAL("Unable to allocate more memory");
         ((OB)L158)->header.tag=AM_ASSIGN_STMT_tag;
         L157 = ((AM_ASSIGN_STMT) L158);
         r12 = L157;
         SATTR(r12,source1,create_source2);
         ret_val41 = r12;
         as = ret_val41;
         SATTR(as,dest,((dAM_EXPR) l));
         SATTR(as,src1,((dAM_EXPR) am));
         create_self10 = ((AM_COMMENT_STMT) NULL);
         L159 = ((dAM_EXPR) am);
         create_source3 = (*dAM_EX2051891691[TAG(L159)])(L159);
         L162=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
         if (L162==NULL) FATAL("Unable to allocate more memory");
         ((OB)L162)->header.tag=AM_COMMENT_STMT_tag;
         L160 = ((AM_COMMENT_STMT) L162);
         r13 = L160;
         SATTR(r13,source1,create_source3);
         ret_val42 = r13;
         comment1 = ret_val42;
         plus_self9 = ((STR) &hoiste1504429436);
         L163 = ((dAM_EXPR) am);
         plus_sarg = SFILE_ID_strrSTR((*dAM_EX2051891691[TAG(L163)])(L163));
         ret_val43 = STR_ap2004550586(plus_self9, plus_sarg);
         SATTR(comment1,comment1,ret_val43);
         if (need_safe_attr) {
          OPT_CO160961835(self, am);
         }
         AM_CUR352782528(c, ((dAM_STMT) comment1));
         AM_CUR352782528(c, ((dAM_STMT) as));
         AM_CUR915996644(c, ((dAM_EXPR) l));
         create_self11 = ((EXPR_HOISTED) NULL);
         create_e = ((dAM_EXPR) am);
         create_s = need_safe_attr;
         create_l = l;
         L165=ZALLOC(sizeof(struct EXPR_HOISTED_struct));
         if (L165==NULL) FATAL("Unable to allocate more memory");
         ((OB)L165)->header.tag=EXPR_HOISTED_tag;
         L164 = ((EXPR_HOISTED) L165);
         r14 = L164;
         SATTR(r14,expr,create_e);
         SATTR(r14,is_safe,create_s);
         SATTR(r14,local1,create_l);
         ret_val44 = r14;
         SATTR(AM_CUR153953493(c),hoisted,FLISTE1031401542(e, ret_val44));
         prog_self10 = self;
         ret_val45 = OPT_CO233462019;
         L166=ret_val45;
         if (ATTR(L166,opt_debug)) {
          create_self12 = ((OUT) NULL);
          ret_val46 = create_self12;
          plus_self10 = ret_val46;
          plus_s9 = ((STR) &expres1398214494);
          stdout_self9 = ((FILE1) NULL);
          L168=ZALLOC(sizeof(struct FILE1_struct));
          if (L168==NULL) FATAL("Unable to allocate more memory");
          ((OB)L168)->header.tag=FILE1_tag;
          L167 = ((FILE1) L168);
          r15 = L167;
          
          SATTR(r15,fp,stdout);
          ret_val47 = r15;
          FILE_plus_STR(ret_val47, plus_s9);
          AM_OUT1439666796(((AM_OUT) NULL), am);
          create_self13 = ((OUT) NULL);
          ret_val48 = create_self13;
          plus_self11 = ret_val48;
          plus_s10 = ((STR) &need_safe_attr11);
          stdout_self10 = ((FILE1) NULL);
          L171=ZALLOC(sizeof(struct FILE1_struct));
          if (L171==NULL) FATAL("Unable to allocate more memory");
          ((OB)L171)->header.tag=FILE1_tag;
          L170 = ((FILE1) L171);
          r16 = L170;
          
          SATTR(r16,fp,stdout);
          ret_val50 = r16;
          FILE_plus_STR(ret_val50, plus_s10);
          ret_val49 = plus_self11;
          plus_self12 = ret_val49;
          L174=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
          if (L174==NULL) FATAL("Unable to allocate more memory");
          memset(L174,0,sizeof(struct BOOL_boxed_struct));
          ((OB)L174)->header.tag=BOOL_tag;
          L173 = (dSTR)((BOOL_boxed) L174);
          ((BOOL_boxed) L173)->immutable_part = need_safe_attr;
          plus_s11 = L173;
          stdout_self11 = ((FILE1) NULL);
          L176=ZALLOC(sizeof(struct FILE1_struct));
          if (L176==NULL) FATAL("Unable to allocate more memory");
          ((OB)L176)->header.tag=FILE1_tag;
          L175 = ((FILE1) L176);
          r17 = L175;
          
          SATTR(r17,fp,stdout);
          ret_val52 = r17;
          FILE_plus_dSTR(ret_val52, plus_s11);
          ret_val51 = plus_self12;
          plus_self13 = ret_val51;
          plus_s12 = ((STR) &name50);
          stdout_self12 = ((FILE1) NULL);
          L179=ZALLOC(sizeof(struct FILE1_struct));
          if (L179==NULL) FATAL("Unable to allocate more memory");
          ((OB)L179)->header.tag=FILE1_tag;
          L178 = ((FILE1) L179);
          r18 = L178;
          
          SATTR(r18,fp,stdout);
          ret_val53 = r18;
          FILE_plus_STR(ret_val53, plus_s12);
         }
        }
       }
      }
     }
     if (am==NULL) {
      goto other284;
     } else
     switch (TAG(am)) {
      case AM_ROU1916046290_tag:
       L182 = ATTR(c,loops);
       L185 = ATTR(c,loops);
       L1861_=ARR((ARRAYBOOL)after_loop_break,L185); 
       if (L1861_) {
        L184 = TRUE;
       } else {
        L187 = ATTR(c,with_side_effect);
        L1881_=!(L187); 
        L184 = L1881_;
       }
       if (L184) {
        L183 = TRUE;
       } else {
        prog_self11 = self;
        ret_val54 = OPT_CO233462019;
        L190 = (SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val54)==((SE_CONTEXT) NULL));
        L1911_=!(L190); 
        if (L1911_) {
         prog_self12 = self;
         ret_val55 = OPT_CO233462019;
         L192=SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val55);
         L189 = ATTR(L192,has_raise);
        } else {
         L189 = FALSE;
        }
        L183 = L189;
       }
       L193 = L183;
       SARR((ARRAYBOOL)after_loop_break,L182,L193); 
       ; break;
      default: ;
      other284: ;
     }
     prog_self13 = self;
     ret_val56 = OPT_CO233462019;
     L197=ret_val56;
     if (ATTR(L197,opt_debug)) {
      L1981_=!(hoisted); 
      L196 = L1981_;
     } else {
      L196 = FALSE;
     }
     if (L196) {
      L199 = ATTR(c,loops);
      L2001_=(0)<(L199); 
      L195 = L2001_;
     } else {
      L195 = FALSE;
     }
     if (L195) {
      if (am==NULL) {
       goto other285;
      } else
      switch (TAG(am)) {
       case AM_ATTR_EXPR_tag:
        AM_CURSOR_debug = TRUE;
        create_self14 = ((OUT) NULL);
        ret_val57 = create_self14;
        plus_self14 = ret_val57;
        plus_s13 = ((STR) &thefol1298497786);
        stdout_self13 = ((FILE1) NULL);
        L203=ZALLOC(sizeof(struct FILE1_struct));
        if (L203==NULL) FATAL("Unable to allocate more memory");
        ((OB)L203)->header.tag=FILE1_tag;
        L202 = ((FILE1) L203);
        r19 = L202;
        
        SATTR(r19,fp,stdout);
        ret_val58 = r19;
        FILE_plus_STR(ret_val58, plus_s13);
        AM_OUT1439666796(((AM_OUT) NULL), am);
        create_self15 = ((OUT) NULL);
        ret_val59 = create_self15;
        plus_self15 = ret_val59;
        plus_s14 = ((STR) &name9);
        stdout_self14 = ((FILE1) NULL);
        L206=ZALLOC(sizeof(struct FILE1_struct));
        if (L206==NULL) FATAL("Unable to allocate more memory");
        ((OB)L206)->header.tag=FILE1_tag;
        L205 = ((FILE1) L206);
        r20 = L205;
        
        SATTR(r20,fp,stdout);
        ret_val60 = r20;
        FILE_plus_STR(ret_val60, plus_s14);
        L208 = AM_CUR262177482(c);
        L2091_=!(L208); 
        in_conditional1 = L2091_;
        L211 = ATTR(c,loops);
        L2121_=ARR((ARRAYBOOL)after_loop_break,L211); 
        if (L2121_) {
         L210 = TRUE;
        } else {
         L210 = in_conditional1;
        }
        need_safe_attr1 = L210;
        create_self16 = ((OUT) NULL);
        ret_val61 = create_self16;
        plus_self16 = ret_val61;
        plus_s15 = ((STR) &in_conditional11);
        stdout_self15 = ((FILE1) NULL);
        L214=ZALLOC(sizeof(struct FILE1_struct));
        if (L214==NULL) FATAL("Unable to allocate more memory");
        ((OB)L214)->header.tag=FILE1_tag;
        L213 = ((FILE1) L214);
        r21 = L213;
        
        SATTR(r21,fp,stdout);
        ret_val63 = r21;
        FILE_plus_STR(ret_val63, plus_s15);
        ret_val62 = plus_self16;
        plus_self17 = ret_val62;
        L217=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L217==NULL) FATAL("Unable to allocate more memory");
        memset(L217,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L217)->header.tag=BOOL_tag;
        L216 = (dSTR)((BOOL_boxed) L217);
        ((BOOL_boxed) L216)->immutable_part = in_conditional1;
        plus_s16 = L216;
        stdout_self16 = ((FILE1) NULL);
        L219=ZALLOC(sizeof(struct FILE1_struct));
        if (L219==NULL) FATAL("Unable to allocate more memory");
        ((OB)L219)->header.tag=FILE1_tag;
        L218 = ((FILE1) L219);
        r22 = L218;
        
        SATTR(r22,fp,stdout);
        ret_val65 = r22;
        FILE_plus_dSTR(ret_val65, plus_s16);
        ret_val64 = plus_self17;
        plus_self18 = ret_val64;
        plus_s17 = ((STR) &name60);
        stdout_self17 = ((FILE1) NULL);
        L222=ZALLOC(sizeof(struct FILE1_struct));
        if (L222==NULL) FATAL("Unable to allocate more memory");
        ((OB)L222)->header.tag=FILE1_tag;
        L221 = ((FILE1) L222);
        r23 = L221;
        
        SATTR(r23,fp,stdout);
        ret_val66 = r23;
        FILE_plus_STR(ret_val66, plus_s17);
        create_self17 = ((OUT) NULL);
        ret_val67 = create_self17;
        plus_self19 = ret_val67;
        plus_s18 = ((STR) &need_safe_attr21);
        stdout_self18 = ((FILE1) NULL);
        L225=ZALLOC(sizeof(struct FILE1_struct));
        if (L225==NULL) FATAL("Unable to allocate more memory");
        ((OB)L225)->header.tag=FILE1_tag;
        L224 = ((FILE1) L225);
        r24 = L224;
        
        SATTR(r24,fp,stdout);
        ret_val69 = r24;
        FILE_plus_STR(ret_val69, plus_s18);
        ret_val68 = plus_self19;
        plus_self20 = ret_val68;
        L228=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L228==NULL) FATAL("Unable to allocate more memory");
        memset(L228,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L228)->header.tag=BOOL_tag;
        L227 = (dSTR)((BOOL_boxed) L228);
        ((BOOL_boxed) L227)->immutable_part = need_safe_attr1;
        plus_s19 = L227;
        stdout_self19 = ((FILE1) NULL);
        L230=ZALLOC(sizeof(struct FILE1_struct));
        if (L230==NULL) FATAL("Unable to allocate more memory");
        ((OB)L230)->header.tag=FILE1_tag;
        L229 = ((FILE1) L230);
        r25 = L229;
        
        SATTR(r25,fp,stdout);
        ret_val71 = r25;
        FILE_plus_dSTR(ret_val71, plus_s19);
        ret_val70 = plus_self20;
        plus_self21 = ret_val70;
        plus_s20 = ((STR) &name60);
        stdout_self20 = ((FILE1) NULL);
        L233=ZALLOC(sizeof(struct FILE1_struct));
        if (L233==NULL) FATAL("Unable to allocate more memory");
        ((OB)L233)->header.tag=FILE1_tag;
        L232 = ((FILE1) L233);
        r26 = L232;
        
        SATTR(r26,fp,stdout);
        ret_val72 = r26;
        FILE_plus_STR(ret_val72, plus_s20);
        create_self18 = ((OUT) NULL);
        ret_val73 = create_self18;
        plus_self22 = ret_val73;
        plus_s21 = ((STR) &progvoid_checks);
        stdout_self21 = ((FILE1) NULL);
        L236=ZALLOC(sizeof(struct FILE1_struct));
        if (L236==NULL) FATAL("Unable to allocate more memory");
        ((OB)L236)->header.tag=FILE1_tag;
        L235 = ((FILE1) L236);
        r27 = L235;
        
        SATTR(r27,fp,stdout);
        ret_val75 = r27;
        FILE_plus_STR(ret_val75, plus_s21);
        ret_val74 = plus_self22;
        plus_self23 = ret_val74;
        prog_self14 = self;
        ret_val76 = OPT_CO233462019;
        L238=ret_val76;
        L240=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L240==NULL) FATAL("Unable to allocate more memory");
        memset(L240,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L240)->header.tag=BOOL_tag;
        L239 = (dSTR)((BOOL_boxed) L240);
        ((BOOL_boxed) L239)->immutable_part = ATTR(L238,void_checks);
        plus_s22 = L239;
        stdout_self22 = ((FILE1) NULL);
        L243=ZALLOC(sizeof(struct FILE1_struct));
        if (L243==NULL) FATAL("Unable to allocate more memory");
        ((OB)L243)->header.tag=FILE1_tag;
        L242 = ((FILE1) L243);
        r28 = L242;
        
        SATTR(r28,fp,stdout);
        ret_val78 = r28;
        FILE_plus_dSTR(ret_val78, plus_s22);
        ret_val77 = plus_self23;
        plus_self24 = ret_val77;
        plus_s23 = ((STR) &name9);
        stdout_self23 = ((FILE1) NULL);
        L246=ZALLOC(sizeof(struct FILE1_struct));
        if (L246==NULL) FATAL("Unable to allocate more memory");
        ((OB)L246)->header.tag=FILE1_tag;
        L245 = ((FILE1) L246);
        r29 = L245;
        
        SATTR(r29,fp,stdout);
        ret_val79 = r29;
        FILE_plus_STR(ret_val79, plus_s23);
        create_self19 = ((OUT) NULL);
        ret_val80 = create_self19;
        plus_self27 = ret_val80;
        L249=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L249==NULL) FATAL("Unable to allocate more memory");
        memset(L249,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L249)->header.tag=BOOL_tag;
        L248 = (dSTR)((BOOL_boxed) L249);
        ((BOOL_boxed) L248)->immutable_part = OPT_CO197759827(self, am);
        plus_self25 = STR_pl1270664985(((STR) &has_attr_expram), L248);
        plus_sarg1 = ((STR) &hoistable);
        ret_val81 = STR_ap2004550586(plus_self25, plus_sarg1);
        L250 = ret_val81;
        L252=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L252==NULL) FATAL("Unable to allocate more memory");
        memset(L252,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L252)->header.tag=BOOL_tag;
        L251 = (dSTR)((BOOL_boxed) L252);
        ((BOOL_boxed) L251)->immutable_part = OPT_CO189407210(self, ((dAM_EXPR) am), c, need_safe_attr1, in_conditional1);
        plus_self26 = STR_pl1270664985(L250, L251);
        plus_sarg2 = ((STR) &name9);
        ret_val82 = STR_ap2004550586(plus_self26, plus_sarg2);
        plus_s24 = ret_val82;
        stdout_self24 = ((FILE1) NULL);
        L254=ZALLOC(sizeof(struct FILE1_struct));
        if (L254==NULL) FATAL("Unable to allocate more memory");
        ((OB)L254)->header.tag=FILE1_tag;
        L253 = ((FILE1) L254);
        r30 = L253;
        
        SATTR(r30,fp,stdout);
        ret_val83 = r30;
        FILE_plus_STR(ret_val83, plus_s24);
        AM_CURSOR_debug = FALSE; break;
       default: ;
       other285: ;
      }
     } break;
    case AM_ASSIGN_STMT_tag:
     hoisted1 = FALSE;
     L259 = ATTR(c,loops);
     L2601_=(0)<(L259); 
     if (L2601_) {
      prog_self15 = self;
      ret_val84 = OPT_CO233462019;
      L262=ret_val84;
      L258 = ATTR(L262,hoist_const);
     } else {
      L258 = FALSE;
     }
     if (L258) {
      L257 = AM_CUR1097584212(c);
     } else {
      L257 = FALSE;
     }
     if (L257) {
      L2651_=(1)<(pass1); 
      if (L2651_) {
       is_iter_self3 = func;
       is_iter_self4 = ATTR(is_iter_self3,sig1);
       is_iter_self5 = ATTR(is_iter_self4,name1);
       L267 = (is_iter_self5.str==((STR) NULL));
       L2681_=!(L267); 
       if (L2681_) {
        L269 = is_iter_self5.str;
        L270 = STR_sizerINT(is_iter_self5.str);
        L2711_=INTMINUS(L270,1); 
        L272 = L2711_;
        L2731_=ARR((STR)L269,L272); 
        L274 = L2731_;
        L2751_=L274=='!'; 
        L266 = L2751_;
       } else {
        L266 = FALSE;
       }
       ret_val87 = L266;
       ret_val86 = ret_val87;
       ret_val85 = ret_val86;
       L276 = ret_val85;
       L2771_=!(L276); 
       L264 = L2771_;
      } else {
       L264 = FALSE;
      }
      if (L264) {
       L263 = TRUE;
      } else {
       L278 = AM_CUR2066246826(c);
       L2791_=!(L278); 
       L263 = L2791_;
      }
      L256 = L263;
     } else {
      L256 = FALSE;
     }
     if (L256) {
      L280 = AM_CUR262177482(c);
      L2811_=!(L280); 
      in_conditional2 = L2811_;
      L283 = ATTR(c,loops);
      L2841_=ARR((ARRAYBOOL)after_loop_break,L283); 
      if (L2841_) {
       L282 = TRUE;
      } else {
       L282 = in_conditional2;
      }
      need_safe_attr2 = L282;
      L2891_=!(need_safe_attr2); 
      if (L2891_) {
       L288 = TRUE;
      } else {
       prog_self16 = self;
       ret_val88 = OPT_CO233462019;
       L290=ret_val88;
       L291 = ATTR(L290,void_checks);
       L2921_=!(L291); 
       L288 = L2921_;
      }
      if (L288) {
       L287 = TRUE;
      } else {
       L293 = OPT_CO197759827(self, am);
       L2941_=!(L293); 
       L287 = L2941_;
      }
      if (L287) {
       L286 = OPT_CO372535654(self, ((AM_ASSIGN_STMT) am), c, need_safe_attr2, in_conditional2);
      } else {
       L286 = FALSE;
      }
      if (L286) {
       L285 = OPT_CO189407210(self, ATTR(((AM_ASSIGN_STMT) am),src1), c, need_safe_attr2, in_conditional2);
      } else {
       L285 = FALSE;
      }
      if (L285) {
       if (need_safe_attr2) {
        OPT_CO160961835(self, ((dAM) ATTR(((AM_ASSIGN_STMT) am),src1)));
       }
       hoisted1 = TRUE;
       prog_self17 = self;
       ret_val89 = OPT_CO233462019;
       L295=ret_val89;
       if (ATTR(L295,opt_debug)) {
        create_self20 = ((OUT) NULL);
        ret_val90 = create_self20;
        plus_self28 = ret_val90;
        plus_s25 = ((STR) &assign138801694);
        stdout_self25 = ((FILE1) NULL);
        L297=ZALLOC(sizeof(struct FILE1_struct));
        if (L297==NULL) FATAL("Unable to allocate more memory");
        ((OB)L297)->header.tag=FILE1_tag;
        L296 = ((FILE1) L297);
        r31 = L296;
        
        SATTR(r31,fp,stdout);
        ret_val92 = r31;
        FILE_plus_STR(ret_val92, plus_s25);
        ret_val91 = plus_self28;
        plus_self29 = ret_val91;
        L300=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L300==NULL) FATAL("Unable to allocate more memory");
        memset(L300,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L300)->header.tag=BOOL_tag;
        L299 = (dSTR)((BOOL_boxed) L300);
        ((BOOL_boxed) L299)->immutable_part = need_safe_attr2;
        plus_s26 = L299;
        stdout_self26 = ((FILE1) NULL);
        L302=ZALLOC(sizeof(struct FILE1_struct));
        if (L302==NULL) FATAL("Unable to allocate more memory");
        ((OB)L302)->header.tag=FILE1_tag;
        L301 = ((FILE1) L302);
        r32 = L301;
        
        SATTR(r32,fp,stdout);
        ret_val94 = r32;
        FILE_plus_dSTR(ret_val94, plus_s26);
        ret_val93 = plus_self29;
        plus_self30 = ret_val93;
        plus_s27 = ((STR) &name50);
        stdout_self27 = ((FILE1) NULL);
        L305=ZALLOC(sizeof(struct FILE1_struct));
        if (L305==NULL) FATAL("Unable to allocate more memory");
        ((OB)L305)->header.tag=FILE1_tag;
        L304 = ((FILE1) L305);
        r33 = L304;
        
        SATTR(r33,fp,stdout);
        ret_val95 = r33;
        FILE_plus_STR(ret_val95, plus_s27);
        AM_one_stmt_self = ((AM_OUT) NULL);
        AM_one_stmt_a = am;
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
          L307 = ((dAM_STMT) AM_one_stmt_a);
          n = (*dAM_ST775224435[TAG(L307)])(L307);
          L308 = ((dAM_STMT) AM_one_stmt_a);
          (*dAM_ST1372701974[TAG(L308)])(L308, ((dAM_STMT) NULL));
          AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
          L309 = ((dAM_STMT) AM_one_stmt_a);
          (*dAM_ST1372701974[TAG(L309)])(L309, n); break;
         default: ;
          AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
        }
       }
       AM_CUR191948285(c);
       create_self21 = ((AM_COMMENT_STMT) NULL);
       create_source4 = ATTR(((AM_ASSIGN_STMT) am),source1);
       L311=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
       if (L311==NULL) FATAL("Unable to allocate more memory");
       ((OB)L311)->header.tag=AM_COMMENT_STMT_tag;
       L310 = ((AM_COMMENT_STMT) L311);
       r34 = L310;
       SATTR(r34,source1,create_source4);
       ret_val96 = r34;
       comment2 = ret_val96;
       plus_self31 = ((STR) &hoiste87020047);
       plus_sarg3 = SFILE_ID_strrSTR(ATTR(((AM_ASSIGN_STMT) am),source1));
       ret_val97 = STR_ap2004550586(plus_self31, plus_sarg3);
       SATTR(comment2,comment1,ret_val97);
       AM_CUR352782528(c, ((dAM_STMT) comment2));
       AM_CUR352782528(c, ((dAM_STMT) am));
       prog_self18 = self;
       ret_val98 = OPT_CO233462019;
       L312=ret_val98;
       L313 = ATTR(L312,stat1);
       (*dSTAT_incr_STR[TAG(L313)])(L313, ((STR) &Oofass325928632));
       res = TRUE;
      }
      else {
       prog_self19 = self;
       ret_val99 = OPT_CO233462019;
       L314=ret_val99;
       if (ATTR(L314,opt_debug)) {
        create_self22 = ((OUT) NULL);
        ret_val100 = create_self22;
        plus_self32 = ret_val100;
        plus_s28 = ((STR) &assign1949641357);
        stdout_self28 = ((FILE1) NULL);
        L316=ZALLOC(sizeof(struct FILE1_struct));
        if (L316==NULL) FATAL("Unable to allocate more memory");
        ((OB)L316)->header.tag=FILE1_tag;
        L315 = ((FILE1) L316);
        r35 = L315;
        
        SATTR(r35,fp,stdout);
        ret_val102 = r35;
        FILE_plus_STR(ret_val102, plus_s28);
        ret_val101 = plus_self32;
        plus_self33 = ret_val101;
        L319=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L319==NULL) FATAL("Unable to allocate more memory");
        memset(L319,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L319)->header.tag=BOOL_tag;
        L318 = (dSTR)((BOOL_boxed) L319);
        ((BOOL_boxed) L318)->immutable_part = need_safe_attr2;
        plus_s29 = L318;
        stdout_self29 = ((FILE1) NULL);
        L322=ZALLOC(sizeof(struct FILE1_struct));
        if (L322==NULL) FATAL("Unable to allocate more memory");
        ((OB)L322)->header.tag=FILE1_tag;
        L320 = ((FILE1) L322);
        r36 = L320;
        
        SATTR(r36,fp,stdout);
        ret_val104 = r36;
        FILE_plus_dSTR(ret_val104, plus_s29);
        ret_val103 = plus_self33;
        plus_self34 = ret_val103;
        plus_s30 = ((STR) &name60);
        stdout_self30 = ((FILE1) NULL);
        L325=ZALLOC(sizeof(struct FILE1_struct));
        if (L325==NULL) FATAL("Unable to allocate more memory");
        ((OB)L325)->header.tag=FILE1_tag;
        L324 = ((FILE1) L325);
        r37 = L324;
        
        SATTR(r37,fp,stdout);
        ret_val105 = r37;
        FILE_plus_STR(ret_val105, plus_s30);
        create_self23 = ((OUT) NULL);
        ret_val106 = create_self23;
        plus_self35 = ret_val106;
        plus_s31 = ((STR) &progvoid_checks);
        stdout_self31 = ((FILE1) NULL);
        L328=ZALLOC(sizeof(struct FILE1_struct));
        if (L328==NULL) FATAL("Unable to allocate more memory");
        ((OB)L328)->header.tag=FILE1_tag;
        L327 = ((FILE1) L328);
        r38 = L327;
        
        SATTR(r38,fp,stdout);
        ret_val108 = r38;
        FILE_plus_STR(ret_val108, plus_s31);
        ret_val107 = plus_self35;
        plus_self36 = ret_val107;
        prog_self20 = self;
        ret_val109 = OPT_CO233462019;
        L330=ret_val109;
        L332=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L332==NULL) FATAL("Unable to allocate more memory");
        memset(L332,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L332)->header.tag=BOOL_tag;
        L331 = (dSTR)((BOOL_boxed) L332);
        ((BOOL_boxed) L331)->immutable_part = ATTR(L330,void_checks);
        plus_s32 = L331;
        stdout_self32 = ((FILE1) NULL);
        L334=ZALLOC(sizeof(struct FILE1_struct));
        if (L334==NULL) FATAL("Unable to allocate more memory");
        ((OB)L334)->header.tag=FILE1_tag;
        L333 = ((FILE1) L334);
        r39 = L333;
        
        SATTR(r39,fp,stdout);
        ret_val110 = r39;
        FILE_plus_dSTR(ret_val110, plus_s32);
        create_self24 = ((OUT) NULL);
        ret_val1111 = create_self24;
        plus_self37 = ret_val1111;
        plus_s33 = ((STR) &has_attr_expr);
        stdout_self33 = ((FILE1) NULL);
        L337=ZALLOC(sizeof(struct FILE1_struct));
        if (L337==NULL) FATAL("Unable to allocate more memory");
        ((OB)L337)->header.tag=FILE1_tag;
        L336 = ((FILE1) L337);
        r40 = L336;
        
        SATTR(r40,fp,stdout);
        ret_val113 = r40;
        FILE_plus_STR(ret_val113, plus_s33);
        ret_val112 = plus_self37;
        plus_self38 = ret_val112;
        L340=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L340==NULL) FATAL("Unable to allocate more memory");
        memset(L340,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L340)->header.tag=BOOL_tag;
        L339 = (dSTR)((BOOL_boxed) L340);
        ((BOOL_boxed) L339)->immutable_part = OPT_CO197759827(self, am);
        plus_s34 = L339;
        stdout_self34 = ((FILE1) NULL);
        L343=ZALLOC(sizeof(struct FILE1_struct));
        if (L343==NULL) FATAL("Unable to allocate more memory");
        ((OB)L343)->header.tag=FILE1_tag;
        L342 = ((FILE1) L343);
        r41 = L342;
        
        SATTR(r41,fp,stdout);
        ret_val115 = r41;
        FILE_plus_dSTR(ret_val115, plus_s34);
        ret_val114 = plus_self38;
        plus_self39 = ret_val114;
        plus_s35 = ((STR) &name9);
        stdout_self35 = ((FILE1) NULL);
        L346=ZALLOC(sizeof(struct FILE1_struct));
        if (L346==NULL) FATAL("Unable to allocate more memory");
        ((OB)L346)->header.tag=FILE1_tag;
        L345 = ((FILE1) L346);
        r42 = L345;
        
        SATTR(r42,fp,stdout);
        ret_val116 = r42;
        FILE_plus_STR(ret_val116, plus_s35);
        create_self25 = ((OUT) NULL);
        ret_val117 = create_self25;
        plus_self40 = ret_val117;
        plus_s36 = ((STR) &hoistable_dest);
        stdout_self36 = ((FILE1) NULL);
        L349=ZALLOC(sizeof(struct FILE1_struct));
        if (L349==NULL) FATAL("Unable to allocate more memory");
        ((OB)L349)->header.tag=FILE1_tag;
        L348 = ((FILE1) L349);
        r43 = L348;
        
        SATTR(r43,fp,stdout);
        ret_val119 = r43;
        FILE_plus_STR(ret_val119, plus_s36);
        ret_val118 = plus_self40;
        plus_self41 = ret_val118;
        L352=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L352==NULL) FATAL("Unable to allocate more memory");
        memset(L352,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L352)->header.tag=BOOL_tag;
        L351 = (dSTR)((BOOL_boxed) L352);
        ((BOOL_boxed) L351)->immutable_part = OPT_CO372535654(self, ((AM_ASSIGN_STMT) am), c, need_safe_attr2, in_conditional2);
        plus_s37 = L351;
        stdout_self37 = ((FILE1) NULL);
        L354=ZALLOC(sizeof(struct FILE1_struct));
        if (L354==NULL) FATAL("Unable to allocate more memory");
        ((OB)L354)->header.tag=FILE1_tag;
        L353 = ((FILE1) L354);
        r44 = L353;
        
        SATTR(r44,fp,stdout);
        ret_val121 = r44;
        FILE_plus_dSTR(ret_val121, plus_s37);
        ret_val120 = plus_self41;
        plus_self42 = ret_val120;
        plus_s38 = ((STR) &hoistable1);
        stdout_self38 = ((FILE1) NULL);
        L357=ZALLOC(sizeof(struct FILE1_struct));
        if (L357==NULL) FATAL("Unable to allocate more memory");
        ((OB)L357)->header.tag=FILE1_tag;
        L356 = ((FILE1) L357);
        r45 = L356;
        
        SATTR(r45,fp,stdout);
        ret_val123 = r45;
        FILE_plus_STR(ret_val123, plus_s38);
        ret_val122 = plus_self42;
        plus_self43 = ret_val122;
        L360=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
        if (L360==NULL) FATAL("Unable to allocate more memory");
        memset(L360,0,sizeof(struct BOOL_boxed_struct));
        ((OB)L360)->header.tag=BOOL_tag;
        L359 = (dSTR)((BOOL_boxed) L360);
        ((BOOL_boxed) L359)->immutable_part = OPT_CO189407210(self, ATTR(((AM_ASSIGN_STMT) am),src1), c, need_safe_attr2, in_conditional2);
        plus_s39 = L359;
        stdout_self39 = ((FILE1) NULL);
        L363=ZALLOC(sizeof(struct FILE1_struct));
        if (L363==NULL) FATAL("Unable to allocate more memory");
        ((OB)L363)->header.tag=FILE1_tag;
        L362 = ((FILE1) L363);
        r46 = L362;
        
        SATTR(r46,fp,stdout);
        ret_val125 = r46;
        FILE_plus_dSTR(ret_val125, plus_s39);
        ret_val124 = plus_self43;
        plus_self44 = ret_val124;
        plus_s40 = ((STR) &name9);
        stdout_self40 = ((FILE1) NULL);
        L366=ZALLOC(sizeof(struct FILE1_struct));
        if (L366==NULL) FATAL("Unable to allocate more memory");
        ((OB)L366)->header.tag=FILE1_tag;
        L365 = ((FILE1) L366);
        r47 = L365;
        
        SATTR(r47,fp,stdout);
        ret_val126 = r47;
        FILE_plus_STR(ret_val126, plus_s40);
        AM_one_stmt_self1 = ((AM_OUT) NULL);
        AM_one_stmt_a1 = am;
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
          L368 = ((dAM_STMT) AM_one_stmt_a1);
          n11 = (*dAM_ST775224435[TAG(L368)])(L368);
          L369 = ((dAM_STMT) AM_one_stmt_a1);
          (*dAM_ST1372701974[TAG(L369)])(L369, ((dAM_STMT) NULL));
          AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
          L370 = ((dAM_STMT) AM_one_stmt_a1);
          (*dAM_ST1372701974[TAG(L370)])(L370, n11); break;
         default: ;
          AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
        }
       }
      }
     } break;
    default: ;
    other281: ;
   }
  }
 }
 after_loop: ;
 L371 = (ATTR(c,top1)==((dAM) NULL));
 L3721_=!(L371); 
 if (L3721_) {
  tam = ATTR(c,top1);
  switch (TAG(tam)) {
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
    SATTR(func,code1,((dAM_STMT) tam)); break;
   default: ;
    FATAL("No applicable type in typecase\nin OPT_CONST::const_hoisting(AM_ROUT_DEF,INT):BOOL\n./Back/o_const.sa:216:17");
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_CO189407210(OPT_CONST self, dAM_EXPR src11, AM_CURSOR c, BOOL need_safe_attr, BOOL in_conditional) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR is_non_fatal_and;
 dAM is_non_fatal_and1;
 BOOL ret_val1 = BOOL_zero;
 AM_CURSOR is_non_fatal_and2;
 dAM is_non_fatal_and3;
 BOOL ret_val2 = BOOL_zero;
 AM_CURSOR is_const_in_loop;
 dAM is_const_in_loop1;
 BOOL ret_val3 = BOOL_zero;
 AM_LOOP_STMT L1;
 AM_CURSOR L2;
 AM_LOOP_STMT L3;
 AM_CURSOR L4;
 AM_LOOP_STMT L5;
 AM_CURSOR L6;
 if (AM_CUR1481630356(c)) {
  ret_val = FALSE;
  return ret_val;
 }
 if (in_conditional) {
  is_non_fatal_and = c;
  is_non_fatal_and1 = ((dAM) src11);
  L1=AM_CUR153953493(is_non_fatal_and);
  L2 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_non_fatal_and,prog), ((dAM) ATTR(L1,body)));
  ret_val1 = AM_CUR2135357807(is_non_fatal_and, L2, is_non_fatal_and1);
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  if (need_safe_attr) {
   is_non_fatal_and2 = c;
   is_non_fatal_and3 = ((dAM) src11);
   L3=AM_CUR153953493(is_non_fatal_and2);
   L4 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_non_fatal_and2,prog), ((dAM) ATTR(L3,body)));
   ret_val2 = AM_CUR2135357807(is_non_fatal_and2, L4, is_non_fatal_and3);
   ret_val = ret_val2;
   return ret_val;
  }
  else {
   is_const_in_loop = c;
   is_const_in_loop1 = ((dAM) src11);
   L5=AM_CUR153953493(is_const_in_loop);
   L6 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_const_in_loop,prog), ((dAM) ATTR(L5,body)));
   ret_val3 = AM_CUR750559803(is_const_in_loop, L6, is_const_in_loop1);
   ret_val = ret_val3;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_CO197759827(OPT_CONST self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM a;
 OPT_CONST prog_self;
 PROG ret_val1;
 dAM L2;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val1, am);
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   a = L2;
   if (a==NULL) {
    goto other270;
   } else
   switch (TAG(a)) {
    case AM_ATTR_EXPR_tag:
     ret_val = TRUE;
     return ret_val; break;
    default: ;
    other270: ;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_CO2013054955(OPT_CONST self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM a;
 OPT_CONST prog_self;
 PROG ret_val1;
 dAM L2;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val1, am);
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   a = L2;
   if (a==NULL) {
    goto other264;
   } else
   switch (TAG(a)) {
    case AM_ITE1809135850_tag:
     ret_val = FALSE;
     return ret_val; break;
    default: ;
    other264: ;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_CO372535654(OPT_CONST self, AM_ASSIGN_STMT desta, AM_CURSOR c, BOOL need_safe_attr, BOOL in_conditional) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR dest1;
 BOOL r = BOOL_zero;
 AM_CURSOR is_const_in_loop;
 dAM is_const_in_loop1;
 BOOL ret_val1 = BOOL_zero;
 AM_CURSOR is_not_used_in_l;
 dAM is_not_used_in_l1;
 BOOL ret_val2 = BOOL_zero;
 AM_CURSOR c11;
 BOOL L1;
 AM_LOOP_STMT L2;
 AM_CURSOR L3;
 BOOL L4;
 dest1 = ATTR(desta,dest);
 if (in_conditional) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  SATTR(desta,dest,((dAM_EXPR) NULL));
  is_const_in_loop = c;
  is_const_in_loop1 = ((dAM) dest1);
  L2=AM_CUR153953493(is_const_in_loop);
  L3 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_const_in_loop,prog), ((dAM) ATTR(L2,body)));
  ret_val1 = AM_CUR750559803(is_const_in_loop, L3, is_const_in_loop1);
  if (ret_val1) {
   is_not_used_in_l = c;
   is_not_used_in_l1 = ((dAM) dest1);
   c11 = AM_CUR1358945253(((AM_CURSOR) NULL), is_not_used_in_l);
   AM_CUR1223064464(c11);
   AM_CUR897865267(c11);
   ret_val2 = AM_CUR1926902782(is_not_used_in_l, c11, is_not_used_in_l1);
   L1 = ret_val2;
  } else {
   L1 = FALSE;
  }
  r = L1;
  SATTR(desta,dest,dest1);
  if (r) {
   L4 = need_safe_attr;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   if (dest1==NULL) {
    goto other280;
   } else
   switch (TAG(dest1)) {
    case AM_LOCAL_EXPR_tag:
     ret_val = AM_CUR762735158(c, ((dAM) dest1));
     return ret_val; break;
    default: ;
    other280: ;
     ret_val = FALSE;
     return ret_val;
   }
  }
  ret_val = r;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPT_CO160961835(OPT_CONST self, dAM a) {
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM am;
 OPT_CONST prog_self;
 PROG ret_val;
 dAM L2;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, a);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L2;
   if (am==NULL) {
    goto other274;
   } else
   switch (TAG(am)) {
    case AM_ATTR_EXPR_tag:
     SATTR(((AM_ATTR_EXPR) am),secure1,TRUE); break;
    default: ;
    other274: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_CO43809499(OPT_CONST self, dAM a) {
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM am;
 OPT_CONST prog_self;
 PROG ret_val;
 dAM L2;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, a);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L2;
   if (am==NULL) {
    goto other275;
   } else
   switch (TAG(am)) {
    case AM_ATTR_EXPR_tag:
     SATTR(((AM_ATTR_EXPR) am),secure1,FALSE); break;
    default: ;
    other275: ;
   }
  }
 }
 after_loop: ;
}
