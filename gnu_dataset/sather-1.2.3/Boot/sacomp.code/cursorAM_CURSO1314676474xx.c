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

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

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

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ASSERT_STMT_struct {/* layout for AM_ASSERT_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_ASSERT_STMT;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

typedef struct AM_ATTACH_STMT_struct {/* layout for AM_ATTACH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *gate;
 struct AM_LOCAL_EXPR_struct *helper;
 struct SFILE_ID_struct source1;
 struct SIG_struct *rout;
 } *AM_ATTACH_STMT;

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

typedef struct AM_AT_EXPR_struct {/* layout for AM_AT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *e;
 struct SFILE_ID_struct source1;
 } *AM_AT_EXPR;

typedef struct AM_BND1124877163_struct {/* layout for AM_BND_CREATE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct ARRAYINT_struct *bnd_args;
 struct ARRAYINT_struct *unbnd_args;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 STR clst;
 BOOL is_remote;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND1124877163;

typedef struct AM_BND367211769_struct {/* layout for AM_BND_ITER_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *bi;
 struct dAM_STMT_struct *init;
 struct AM_LOOP_STMT_struct *lp;
 struct SFILE_ID_struct source1;
 struct TP_ITER_struct *bi_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND367211769;

typedef struct AM_BND260301329_struct {/* layout for AM_BND_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *br;
 struct SFILE_ID_struct source1;
 struct TP_ROUT_struct *br_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND260301329;

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

typedef struct AM_CURSOR_POS_struct {/* layout for AM_CURSOR_POS */
 OB_HEADER header;
 struct dAM_struct *stmt;
 INT branch;
 BOOL mark;
 } *AM_CURSOR_POS;

typedef struct AM_EXCEPT_EXPR_struct {/* layout for AM_EXCEPT_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_EXCEPT_EXPR;

typedef struct AM_EXPR_STMT_struct {/* layout for AM_EXPR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *expr;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_EXPR_STMT;

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

typedef struct AM_IF_EXPR_struct {/* layout for AM_IF_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *if_false;
 struct dAM_EXPR_struct *if_true;
 struct dAM_EXPR_struct *test1;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IF_EXPR;

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

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

typedef struct AM_LOCK_STMT_struct {/* layout for AM_LOCK_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_EXPR_struct *guards;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTA1327600838_struct *locks1;
 struct SFILE_ID_struct source1;
 BOOL manual_unlock;
 } *AM_LOCK_STMT;

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

typedef struct AM_NEAR_EXPR_struct {/* layout for AM_NEAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_NEAR_EXPR;

typedef struct AM_NEW_EXPR_struct {/* layout for AM_NEW_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *asz;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_NEW_EXPR;

typedef struct AM_PAR_STMT_struct {/* layout for AM_PAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_PAR_STMT;

typedef struct AM_POST_STMT_struct {/* layout for AM_POST_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_POST_STMT;

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_PRE_STMT_struct {/* layout for AM_PRE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_PRE_STMT;

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

typedef struct AM_RAISE_STMT_struct {/* layout for AM_RAISE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RAISE_STMT;

typedef struct AM_RETURN_STMT_struct {/* layout for AM_RETURN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RETURN_STMT;

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

typedef struct AM_UNLOCK_STMT_struct {/* layout for AM_UNLOCK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *lock_ob;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_UNLOCK_STMT;

typedef struct AM_VAR744470097_struct {/* layout for AM_VARR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ind;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_VAR744470097;

typedef struct AM_VAT319982528_struct {/* layout for AM_VATTR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct dTP_struct *real_tp;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 } *AM_VAT319982528;

typedef struct AM_VOID_CONST_struct {/* layout for AM_VOID_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_VOID_CONST;

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

typedef struct AM_WIT1996971603_struct {/* layout for AM_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_part;
 struct dAM_STMT_struct *near_part;
 struct dAM_STMT_struct *next;
 struct ARRAYdAM_EXPR_struct *objects1;
 struct SFILE_ID_struct source1;
 } *AM_WIT1996971603;

typedef struct AM_YIELD_STMT_struct {/* layout for AM_YIELD_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 INT ret;
 struct SFILE_ID_struct source1;
 } *AM_YIELD_STMT;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

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

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FLISTA1857087356_frame_struct {
 INT state;
 FLISTA1327600838 self;/* Formal argument: self */
 ARRAYdAM_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA1327600838 aget_self;
 INT aget_ind;
 ARRAYdAM_EXPR ret_val;
 } *FLISTA1857087356_frame;

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
extern IN_MODE MODES_in_mode;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AM_CURSOR_POS AM_CUR61708328(AM_CURSOR);
AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame);
AM_CURSOR_POS FLISTA860779230(FLISTA886630705);
AM_CURSOR_POS FLISTA971253650(FLISTA886630705);
AM_LOCK_STMT AM_CUR201931527(AM_CURSOR);
AM_LOOP_STMT AM_CUR153953493(AM_CURSOR);
AM_PREFETCH_STMT AM_CUR141764973(AM_CURSOR);
AM_WAITFOR_STMT AM_CUR1250801636(AM_CURSOR);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
BOOL AM_CUR1044001004(AM_CURSOR, AM_CURSOR, dTP, IDENT);
BOOL AM_CUR1093203002(AM_CURSOR, AM_CURSOR, AM_ROU1916046290);
BOOL AM_CUR1097584212(AM_CURSOR);
BOOL AM_CUR1106436467(AM_CURSOR);
BOOL AM_CUR1240489726(AM_CURSOR, AM_CURSOR, AM_LOCAL_EXPR);
BOOL AM_CUR1481630356(AM_CURSOR);
BOOL AM_CUR176216201(AM_CURSOR, AM_CURSOR, dTP, IDENT);
BOOL AM_CUR1885691330(AM_CURSOR, AM_CURSOR);
BOOL AM_CUR1926902782(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR1972821118(AM_CURSOR, dAM);
BOOL AM_CUR2066246826(AM_CURSOR);
BOOL AM_CUR2135357807(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR262177482(AM_CURSOR);
BOOL AM_CUR403450479(AM_CURSOR, dAM);
BOOL AM_CUR45133320(AM_CURSOR, dAM);
BOOL AM_CUR473137964(AM_CURSOR, AM_CURSOR, AM_ROU1916046290);
BOOL AM_CUR750559803(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR762735158(AM_CURSOR, dAM);
BOOL AM_CUR830576175(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR954266477(AM_CURSOR);
FLISTA886630705 FLISTA1475379054(FLISTA886630705, AM_CURSOR_POS);
INT AM_CUR1778373732(AM_CURSOR, AM_CURSOR);
INT FLISTA587993904(FLISTA886630705);
INT FLISTd1406591968(FLISTdAM_EXPR);
INT FLISTd355912233(FLISTdAM_STMT);
INT INT_do676118316(INT_do676118316_frame);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame);
STR AM_CUR720390882(AM_CURSOR);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM AM_CUR1623871191(AM_CURSOR);
dAM AM_CUR710387089(AM_CURSOR, dAM, INT);
dAM AM_CUR938531823(AM_CURSOR, INT);
dAM_STMT AM_CUR1430796545(AM_CURSOR);
dAM_STMT AM_CUR643033494(AM_CURSOR);
void AM_CUR1083119301(AM_CURSOR);
void AM_CUR1119852682(AM_CURSOR);
void AM_CUR1127255611(AM_CURSOR);
void AM_CUR1223064464(AM_CURSOR);
void AM_CUR1389299115(AM_CURSOR);
void AM_CUR1649762162(AM_CURSOR);
void AM_CUR1769624888(AM_CURSOR);
void AM_CUR1869307717(AM_CURSOR);
void AM_CUR191948285(AM_CURSOR);
void AM_CUR1966744225(AM_CURSOR, dAM_STMT);
void AM_CUR1968472392(AM_CURSOR, dAM_STMT);
void AM_CUR352782528(AM_CURSOR, dAM_STMT);
void AM_CUR897865267(AM_CURSOR);
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

AM_LOCK_STMT AM_CUR201931527(AM_CURSOR self) {
 AM_LOCK_STMT ret_val;
 dAM a;
 AM_LOCK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCK_STMT ret_val1;
 AM_LOCK_STMT r;
 AM_LOCK_STMT L1;
 OB L2;
 AM_LOCK_STMT L3;
 INT L41_;
 create_self = ((AM_LOCK_STMT) NULL);
 create_source = SFILE_ID_zero;
 L2=ZALLOC(sizeof(struct AM_LOCK_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_LOCK_STMT_tag;
 L1 = ((AM_LOCK_STMT) L2);
 r = L1;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 L3 = ret_val1;
 L41_=SYSTP(L3); 
 a = AM_CUR938531823(self, L41_);
 if ((a==((dAM) NULL))) {
  ret_val = ((AM_LOCK_STMT) NULL);
  return ret_val;
 }
 switch (TAG(a)) {
  case AM_LOCK_STMT_tag:
   ret_val = ((AM_LOCK_STMT) a);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_CURSOR::lock_stmt:AM_LOCK_STMT\n./Back/cursor.sa:1972:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

AM_LOOP_STMT AM_CUR153953493(AM_CURSOR self) {
 AM_LOOP_STMT ret_val;
 A_STAC1244966098 L11;
 AM_CURSOR_POS a;
 dAM s;
 A_STAC1244966098 topb_self;
 AM_CURSOR_POS ret_val1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL topb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val2;
 INT L5;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 AM_CURSOR_POS L81_;
 BOOL L9;
 INT L10;
 BOOL L121_;
 INT L13;
 BOOL L141_;
 {
  BOOL f_L41_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  topb_self = L11;
  topb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(topb_self,s);
   if (f_L41_) {
    f_L41_ = FALSE;
    L6 = FLISTA587993904(ATTR(topb_self,s));
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31 = 0;
    L41_=L21+1;L41_m=L31; 
   }
   if(L41_--<=L41_m)  goto after_loop; 
   aget_ind = L41_;
   L81_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val2 = L81_;
   a = ret_val2;
   s = ATTR(a,stmt);
   if (s==NULL) {
    goto other271;
   } else
   switch (TAG(s)) {
    case AM_LOOP_STMT_tag:
     L10 = ATTR(a,branch);
     L121_=(1)<(L10); 
     if (L121_) {
      L13 = ATTR(a,branch);
      L141_=(L13)<(999999); 
      L9 = L141_;
     } else {
      L9 = FALSE;
     }
     if (L9) {
      ret_val = ((AM_LOOP_STMT) s);
      return ret_val;
     } break;
    default: ;
    other271: ;
   }
  }
 }
 after_loop: ;
 ret_val = ((AM_LOOP_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_PREFETCH_STMT AM_CUR141764973(AM_CURSOR self) {
 AM_PREFETCH_STMT ret_val;
 dAM a;
 AM_PREFETCH_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PREFETCH_STMT ret_val1;
 AM_PREFETCH_STMT r;
 AM_PREFETCH_STMT L1;
 OB L2;
 AM_PREFETCH_STMT L3;
 INT L41_;
 create_self = ((AM_PREFETCH_STMT) NULL);
 create_source = SFILE_ID_zero;
 L2=ZALLOC(sizeof(struct AM_PREFETCH_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_PREFETCH_STMT_tag;
 L1 = ((AM_PREFETCH_STMT) L2);
 r = L1;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 L3 = ret_val1;
 L41_=SYSTP(L3); 
 a = AM_CUR938531823(self, L41_);
 if ((a==((dAM) NULL))) {
  ret_val = ((AM_PREFETCH_STMT) NULL);
  return ret_val;
 }
 switch (TAG(a)) {
  case AM_PREFETCH_STMT_tag:
   ret_val = ((AM_PREFETCH_STMT) a);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_CURSOR::prefetch_stmt:AM_PREFETCH_STMT\n./Back/cursor.sa:1966:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

AM_WAITFOR_STMT AM_CUR1250801636(AM_CURSOR self) {
 AM_WAITFOR_STMT ret_val;
 dAM a;
 AM_WAITFOR_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WAITFOR_STMT ret_val1;
 AM_WAITFOR_STMT r;
 AM_WAITFOR_STMT L1;
 OB L2;
 AM_WAITFOR_STMT L3;
 INT L41_;
 create_self = ((AM_WAITFOR_STMT) NULL);
 create_source = SFILE_ID_zero;
 L2=ZALLOC(sizeof(struct AM_WAITFOR_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_WAITFOR_STMT_tag;
 L1 = ((AM_WAITFOR_STMT) L2);
 r = L1;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 L3 = ret_val1;
 L41_=SYSTP(L3); 
 a = AM_CUR938531823(self, L41_);
 if ((a==((dAM) NULL))) {
  ret_val = ((AM_WAITFOR_STMT) NULL);
  return ret_val;
 }
 switch (TAG(a)) {
  case AM_WAITFOR_STMT_tag:
   ret_val = ((AM_WAITFOR_STMT) a);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_CURSOR::waitfor_stmt:AM_WAITFOR_STMT\n./Back/cursor.sa:1960:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1097584212(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 INT branch1 = INT_zero;
 dAM tcur;
 AM_CURSOR_POS p;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 BOOL L11_;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 branch1 = 0;
 tcur = ATTR(c,cur);
 while (1) {
  if (tcur==NULL) {
   goto other266;
  } else
  switch (TAG(tcur)) {
   case AM_LOOP_STMT_tag:
    ret_val = TRUE;
    return ret_val; break;
   case AM_CASE_STMT_tag:
    L11_=(1)<(branch1); 
    if (L11_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_TYPECASE_STMT_tag:
    L21_=(1)<(branch1); 
    if (L21_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   default: ;
   other266: ;
  }
  size_self = ATTR(c,stack1);
  ret_val1 = FLISTA587993904(ATTR(size_self,s));
  L3 = ret_val1;
  L41_=(L3)==(0); 
  if (L41_) {
   goto after_loop;
  }
  pop_self = ATTR(c,stack1);
  ret_val2 = FLISTA971253650(ATTR(pop_self,s));
  p = ret_val2;
  tcur = ATTR(p,stmt);
  branch1 = ATTR(p,branch);
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR1106436467(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 AM_CURSOR_POS p;
 dAM s1;
 INT b1 = INT_zero;
 dAM s2;
 INT b2 = INT_zero;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 A_STAC1244966098 pop_self1;
 AM_CURSOR_POS ret_val3;
 INT L1;
 BOOL L21_;
 BOOL L31_;
 BOOL L41_;
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 size_self = ATTR(c,stack1);
 ret_val1 = FLISTA587993904(ATTR(size_self,s));
 L1 = ret_val1;
 L21_=(L1)<(2); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 pop_self = ATTR(c,stack1);
 ret_val2 = FLISTA971253650(ATTR(pop_self,s));
 p = ret_val2;
 s1 = ATTR(p,stmt);
 b1 = ATTR(p,branch);
 if (s1==NULL) {
  goto other287;
 } else
 switch (TAG(s1)) {
  case AM_EXPR_STMT_tag:
   L31_=(b1)==(1); 
   if (L31_) {
    pop_self1 = ATTR(c,stack1);
    ret_val3 = FLISTA971253650(ATTR(pop_self1,s));
    p = ret_val3;
    s2 = ATTR(p,stmt);
    b2 = ATTR(p,branch);
    if (s2==NULL) {
     goto other288;
    } else
    switch (TAG(s2)) {
     case AM_FORK_STMT_tag:
      L41_=(b2)==(2); 
      if (L41_) {
       ret_val = TRUE;
       return ret_val;
      } break;
     default: ;
     other288: ;
    }
   } break;
  default: ;
  other287: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR2066246826(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 INT branch1 = INT_zero;
 dAM tcur;
 AM_CURSOR_POS p;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 BOOL L11_;
 INT L2;
 BOOL L31_;
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 branch1 = 0;
 tcur = ATTR(c,cur);
 while (1) {
  if (tcur==NULL) {
   goto other268;
  } else
  switch (TAG(tcur)) {
   case AM_ITE1809135850_tag:
    L11_=(branch1)==(1); 
    if (L11_) {
     ret_val = TRUE;
     return ret_val;
    } break;
   default: ;
   other268: ;
  }
  size_self = ATTR(c,stack1);
  ret_val1 = FLISTA587993904(ATTR(size_self,s));
  L2 = ret_val1;
  L31_=(L2)==(0); 
  if (L31_) {
   goto after_loop;
  }
  pop_self = ATTR(c,stack1);
  ret_val2 = FLISTA971253650(ATTR(pop_self,s));
  p = ret_val2;
  tcur = ATTR(p,stmt);
  branch1 = ATTR(p,branch);
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR2135357807(AM_CURSOR self, AM_CURSOR c, dAM am) {
 BOOL ret_val = BOOL_zero;
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
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val10;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 BOOL L1;
 AM_CURSOR L2;
 BOOL L3;
 BOOL L4;
 AM_CURSOR L5;
 AM_CURSOR L6;
 BOOL L7;
 BOOL L81_;
 AM_CURSOR L9;
 extern STR thefol1378826944;
 FILE1 L10;
 OB L11;
 INT L131_;
 INT L14;
 STR L151_;
 FILE1 L16;
 OB L17;
 extern STR name9;
 FILE1 L19;
 OB L20;
 extern STR thefol1378826944;
 FILE1 L22;
 OB L23;
 INT L251_;
 INT L26;
 STR L271_;
 FILE1 L28;
 OB L29;
 extern STR name9;
 FILE1 L31;
 OB L32;
 if (AM_CUR45133320(self, am)) {
  ret_val = TRUE;
  return ret_val;
 }
 if (am==NULL) {
  goto other272;
 } else
 switch (TAG(am)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = AM_CUR1240489726(self, c, ((AM_LOCAL_EXPR) am));
   return ret_val; break;
  case AM_ROU1916046290_tag:
   ret_val = AM_CUR473137964(self, c, ((AM_ROU1916046290) am));
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   L2 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
   if (AM_CUR1044001004(self, L2, ATTR(((AM_ATTR_EXPR) am),self_tp), ATTR(((AM_ATTR_EXPR) am),at))) {
    L1 = AM_CUR2135357807(self, c, ((dAM) ATTR(((AM_ATTR_EXPR) am),ob)));
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = AM_CUR176216201(self, c, ATTR(((AM_SHARED_EXPR) am),class_tp), ATTR(((AM_SHARED_EXPR) am),name1));
   return ret_val; break;
  case AM_IS_VOID_EXPR_tag:
   ret_val = AM_CUR2135357807(self, c, ((dAM) ATTR(((AM_IS_VOID_EXPR) am),arg)));
   return ret_val; break;
  case AM_CALL_ARG_tag:
   ret_val = AM_CUR2135357807(self, c, ((dAM) ATTR(((AM_CALL_ARG) am),expr)));
   return ret_val; break;
  case AM_IF_EXPR_tag:
   L5 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
   if (AM_CUR2135357807(self, L5, ((dAM) ATTR(((AM_IF_EXPR) am),test1)))) {
    L6 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
    L4 = AM_CUR2135357807(self, L6, ((dAM) ATTR(((AM_IF_EXPR) am),if_true)));
   } else {
    L4 = FALSE;
   }
   if (L4) {
    L3 = AM_CUR2135357807(self, c, ((dAM) ATTR(((AM_IF_EXPR) am),if_false)));
   } else {
    L3 = FALSE;
   }
   ret_val = L3;
   return ret_val; break;
  case AM_STMT_EXPR_tag:
   L7 = (ATTR(((AM_STMT_EXPR) am),replaced)==((dAM_EXPR) NULL));
   L81_=!(L7); 
   if (L81_) {
    L9 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
    ret_val = AM_CUR2135357807(self, L9, ((dAM) ATTR(((AM_STMT_EXPR) am),replaced)));
    return ret_val;
   }
   else {
    if (AM_CURSOR_debug) {
     create_self = ((OUT) NULL);
     ret_val1 = create_self;
     plus_self = ret_val1;
     plus_s = ((STR) &thefol1378826944);
     stdout_self = ((FILE1) NULL);
     L11=ZALLOC(sizeof(struct FILE1_struct));
     if (L11==NULL) FATAL("Unable to allocate more memory");
     ((OB)L11)->header.tag=FILE1_tag;
     L10 = ((FILE1) L11);
     r = L10;
     
     SATTR(r,fp,stdout);
     ret_val3 = r;
     FILE_plus_STR(ret_val3, plus_s);
     ret_val2 = plus_self;
     plus_self1 = ret_val2;
     L131_=SYSTP(am); 
     L14 = L131_;
     L151_=SYSSTRFORTP(L14); 
     plus_s1 = L151_;
     stdout_self1 = ((FILE1) NULL);
     L17=ZALLOC(sizeof(struct FILE1_struct));
     if (L17==NULL) FATAL("Unable to allocate more memory");
     ((OB)L17)->header.tag=FILE1_tag;
     L16 = ((FILE1) L17);
     r1 = L16;
     
     SATTR(r1,fp,stdout);
     ret_val5 = r1;
     FILE_plus_STR(ret_val5, plus_s1);
     ret_val4 = plus_self1;
     plus_self2 = ret_val4;
     plus_s2 = ((STR) &name9);
     stdout_self2 = ((FILE1) NULL);
     L20=ZALLOC(sizeof(struct FILE1_struct));
     if (L20==NULL) FATAL("Unable to allocate more memory");
     ((OB)L20)->header.tag=FILE1_tag;
     L19 = ((FILE1) L20);
     r2 = L19;
     
     SATTR(r2,fp,stdout);
     ret_val6 = r2;
     FILE_plus_STR(ret_val6, plus_s2);
    }
    ret_val = FALSE;
    return ret_val;
   } break;
  default: ;
  other272: ;
   if (AM_CURSOR_debug) {
    create_self1 = ((OUT) NULL);
    ret_val7 = create_self1;
    plus_self3 = ret_val7;
    plus_s3 = ((STR) &thefol1378826944);
    stdout_self3 = ((FILE1) NULL);
    L23=ZALLOC(sizeof(struct FILE1_struct));
    if (L23==NULL) FATAL("Unable to allocate more memory");
    ((OB)L23)->header.tag=FILE1_tag;
    L22 = ((FILE1) L23);
    r3 = L22;
    
    SATTR(r3,fp,stdout);
    ret_val9 = r3;
    FILE_plus_STR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    L251_=SYSTP(am); 
    L26 = L251_;
    L271_=SYSSTRFORTP(L26); 
    plus_s4 = L271_;
    stdout_self4 = ((FILE1) NULL);
    L29=ZALLOC(sizeof(struct FILE1_struct));
    if (L29==NULL) FATAL("Unable to allocate more memory");
    ((OB)L29)->header.tag=FILE1_tag;
    L28 = ((FILE1) L29);
    r4 = L28;
    
    SATTR(r4,fp,stdout);
    ret_val11 = r4;
    FILE_plus_STR(ret_val11, plus_s4);
    ret_val10 = plus_self4;
    plus_self5 = ret_val10;
    plus_s5 = ((STR) &name9);
    stdout_self5 = ((FILE1) NULL);
    L32=ZALLOC(sizeof(struct FILE1_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    ((OB)L32)->header.tag=FILE1_tag;
    L31 = ((FILE1) L32);
    r5 = L31;
    
    SATTR(r5,fp,stdout);
    ret_val12 = r5;
    FILE_plus_STR(ret_val12, plus_s5);
   }
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR262177482(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 INT branch1 = INT_zero;
 dAM tcur;
 AM_CURSOR_POS p;
 A_STAC1244966098 size_self;
 INT ret_val1 = INT_zero;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val2;
 BOOL L11_;
 BOOL L21_;
 BOOL L31_;
 BOOL L41_;
 INT L5;
 BOOL L61_;
 ARRAYdAM_EXPR L7;
 INT L81_;
 INT L9;
 BOOL L101_;
 INT L12;
 BOOL L131_;
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 branch1 = 0;
 tcur = ATTR(c,cur);
 while (1) {
  if (tcur==NULL) {
   goto other269;
  } else
  switch (TAG(tcur)) {
   case AM_LOOP_STMT_tag:
    ret_val = TRUE;
    return ret_val; break;
   case AM_IF_STMT_tag:
    L11_=(1)<(branch1); 
    if (L11_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_IF_EXPR_tag:
    L21_=(1)<(branch1); 
    if (L21_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_TYPECASE_STMT_tag:
    L31_=(1)<(branch1); 
    if (L31_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_CASE_STMT_tag:
    L41_=(1)<(branch1); 
    if (L41_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_LOCK_STMT_tag:
    L5 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) tcur),guards));
    L61_=(L5)<(branch1); 
    if (L61_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   case AM_WIT1996971603_tag:
    L7 = ATTR(((AM_WIT1996971603) tcur),objects1);
    L81_=(L7)==NULL?0:ASIZE((ARRAYdAM_EXPR)L7); 
    L9 = L81_;
    L101_=(L9)<(branch1); 
    if (L101_) {
     ret_val = FALSE;
     return ret_val;
    } break;
   default: ;
   other269: ;
  }
  size_self = ATTR(c,stack1);
  ret_val1 = FLISTA587993904(ATTR(size_self,s));
  L12 = ret_val1;
  L131_=(L12)==(0); 
  if (L131_) {
   goto after_loop;
  }
  pop_self = ATTR(c,stack1);
  ret_val2 = FLISTA971253650(ATTR(pop_self,s));
  p = ret_val2;
  tcur = ATTR(p,stmt);
  branch1 = ATTR(p,branch);
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR403450479(AM_CURSOR self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM t;
 dAM L2;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), am);
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
   t = L2;
   if (t==NULL) {
    goto other322;
   } else
   switch (TAG(t)) {
    case AM_ITE1809135850_tag:
     ret_val = TRUE;
     return ret_val; break;
    case AM_BND367211769_tag:
     ret_val = TRUE;
     return ret_val; break;
    default: ;
    other322: ;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR473137964(AM_CURSOR self, AM_CURSOR c11, AM_ROU1916046290 am) {
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
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 extern STR foundf149178985;
 FILE1 L15;
 OB L16;
 FILE1 L18;
 OB L19;
 extern STR name9;
 FILE1 L22;
 OB L23;
 SIDE_EFFECT L25;
 BOOL L26;
 BOOL L27;
 extern STR routine2;
 FILE1 L28;
 OB L29;
 FILE1 L32;
 OB L33;
 extern STR attrtp;
 FILE1 L35;
 OB L36;
 extern STR void1;
 FILE1 L38;
 OB L39;
 dTP L42;
 FILE1 L43;
 OB L44;
 extern STR name59;
 FILE1 L46;
 OB L47;
 extern STR void1;
 FILE1 L49;
 OB L50;
 FILE1 L53;
 OB L54;
 extern STR doeswrite;
 FILE1 L56;
 OB L57;
 dSTR L59;
 OB L60;
 FILE1 L61;
 OB L62;
 extern STR name9;
 FILE1 L64;
 OB L65;
 AM_CURSOR L67;
 BOOL L68;
 BOOL L691_;
 INT L701_br;
 AM_CALL_ARG aL701_;
 INT L711_br;
INT i_L711_=0;
 AM_CALL_ARG aL711_;
 AM_CALL_ARG L72;
 AM_CALL_ARG L73;
 dMODE L74;
 BOOL L75;
 BOOL L761_;
 AM_CURSOR L77;
 AM_CALL_ARG L78;
 BOOL L79;
 BOOL L801_;
 INT L821_;
 if (ATTR(self,with_side_effect)) {
  L7 = ATTR(ATTR(self,prog),arith_checks);
  L81_=!(L7); 
  L6 = L81_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  c = SIG_ge642497605(ATTR(am,fun), ATTR(self,prog));
  full_self = c;
  if (ATTR(full_self,is_full)) {
   L14 = TRUE;
  } else {
   L14 = ATTR(full_self,has_import);
  }
  if (L14) {
   L13 = TRUE;
  } else {
   L13 = ATTR(full_self,has_export);
  }
  ret_val1 = L13;
  if (ret_val1) {
   L12 = TRUE;
  } else {
   L12 = ATTR(c,unsafe);
  }
  if (L12) {
   L10 = TRUE;
  } else {
   L10 = ATTR(c,has_raise);
  }
  if (L10) {
   L9 = TRUE;
  } else {
   L9 = ATTR(c,has_fatal_error);
  }
  if (L9) {
   if (AM_CURSOR_debug) {
    create_self = ((OUT) NULL);
    ret_val2 = create_self;
    plus_self = ret_val2;
    plus_s = ((STR) &foundf149178985);
    stdout_self = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r = L15;
    
    SATTR(r,fp,stdout);
    ret_val4 = r;
    FILE_plus_STR(ret_val4, plus_s);
    ret_val3 = plus_self;
    plus_self1 = ret_val3;
    plus_s1 = ATTR(ATTR(am,fun),str);
    stdout_self1 = ((FILE1) NULL);
    L19=ZALLOC(sizeof(struct FILE1_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=FILE1_tag;
    L18 = ((FILE1) L19);
    r1 = L18;
    
    SATTR(r1,fp,stdout);
    ret_val6 = r1;
    FILE_plus_STR(ret_val6, plus_s1);
    ret_val5 = plus_self1;
    plus_self2 = ret_val5;
    plus_s2 = ((STR) &name9);
    stdout_self2 = ((FILE1) NULL);
    L23=ZALLOC(sizeof(struct FILE1_struct));
    if (L23==NULL) FATAL("Unable to allocate more memory");
    ((OB)L23)->header.tag=FILE1_tag;
    L22 = ((FILE1) L23);
    r2 = L22;
    
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
    L25 = FSETSI2093978094(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    e = L25;
    if (ATTR(e,doeswrite)) {
     L27 = TRUE;
    } else {
     L27 = (ATTR(e,tp)==((dTP) NULL));
    }
    if (L27) {
     L26 = TRUE;
    } else {
     L26 = (ATTR(e,name1).str==(STR)0);
    }
    if (L26) {
     if (AM_CURSOR_debug) {
      create_self1 = ((OUT) NULL);
      ret_val9 = create_self1;
      plus_self3 = ret_val9;
      plus_s3 = ((STR) &routine2);
      stdout_self3 = ((FILE1) NULL);
      L29=ZALLOC(sizeof(struct FILE1_struct));
      if (L29==NULL) FATAL("Unable to allocate more memory");
      ((OB)L29)->header.tag=FILE1_tag;
      L28 = ((FILE1) L29);
      r3 = L28;
      
      SATTR(r3,fp,stdout);
      ret_val11 = r3;
      FILE_plus_STR(ret_val11, plus_s3);
      ret_val10 = plus_self3;
      plus_self4 = ret_val10;
      plus_s4 = ATTR(ATTR(am,fun),str);
      stdout_self4 = ((FILE1) NULL);
      L33=ZALLOC(sizeof(struct FILE1_struct));
      if (L33==NULL) FATAL("Unable to allocate more memory");
      ((OB)L33)->header.tag=FILE1_tag;
      L32 = ((FILE1) L33);
      r4 = L32;
      
      SATTR(r4,fp,stdout);
      ret_val13 = r4;
      FILE_plus_STR(ret_val13, plus_s4);
      ret_val12 = plus_self4;
      plus_self5 = ret_val12;
      plus_s5 = ((STR) &attrtp);
      stdout_self5 = ((FILE1) NULL);
      L36=ZALLOC(sizeof(struct FILE1_struct));
      if (L36==NULL) FATAL("Unable to allocate more memory");
      ((OB)L36)->header.tag=FILE1_tag;
      L35 = ((FILE1) L36);
      r5 = L35;
      
      SATTR(r5,fp,stdout);
      ret_val14 = r5;
      FILE_plus_STR(ret_val14, plus_s5);
      if ((ATTR(e,tp)==((dTP) NULL))) {
       create_self2 = ((OUT) NULL);
       ret_val15 = create_self2;
       plus_self6 = ret_val15;
       plus_s6 = ((STR) &void1);
       stdout_self6 = ((FILE1) NULL);
       L39=ZALLOC(sizeof(struct FILE1_struct));
       if (L39==NULL) FATAL("Unable to allocate more memory");
       ((OB)L39)->header.tag=FILE1_tag;
       L38 = ((FILE1) L39);
       r6 = L38;
       
       SATTR(r6,fp,stdout);
       ret_val16 = r6;
       FILE_plus_STR(ret_val16, plus_s6);
      }
      else {
       create_self3 = ((OUT) NULL);
       ret_val17 = create_self3;
       plus_self7 = ret_val17;
       L42 = ATTR(e,tp);
       plus_s7 = (*dTP_strrSTR[TAG(L42)])(L42);
       stdout_self7 = ((FILE1) NULL);
       L44=ZALLOC(sizeof(struct FILE1_struct));
       if (L44==NULL) FATAL("Unable to allocate more memory");
       ((OB)L44)->header.tag=FILE1_tag;
       L43 = ((FILE1) L44);
       r7 = L43;
       
       SATTR(r7,fp,stdout);
       ret_val18 = r7;
       FILE_plus_STR(ret_val18, plus_s7);
      }
      create_self4 = ((OUT) NULL);
      ret_val19 = create_self4;
      plus_self8 = ret_val19;
      plus_s8 = ((STR) &name59);
      stdout_self8 = ((FILE1) NULL);
      L47=ZALLOC(sizeof(struct FILE1_struct));
      if (L47==NULL) FATAL("Unable to allocate more memory");
      ((OB)L47)->header.tag=FILE1_tag;
      L46 = ((FILE1) L47);
      r8 = L46;
      
      SATTR(r8,fp,stdout);
      ret_val20 = r8;
      FILE_plus_STR(ret_val20, plus_s8);
      if ((ATTR(e,name1).str==(STR)0)) {
       create_self5 = ((OUT) NULL);
       ret_val21 = create_self5;
       plus_self9 = ret_val21;
       plus_s9 = ((STR) &void1);
       stdout_self9 = ((FILE1) NULL);
       L50=ZALLOC(sizeof(struct FILE1_struct));
       if (L50==NULL) FATAL("Unable to allocate more memory");
       ((OB)L50)->header.tag=FILE1_tag;
       L49 = ((FILE1) L50);
       r9 = L49;
       
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
       L54=ZALLOC(sizeof(struct FILE1_struct));
       if (L54==NULL) FATAL("Unable to allocate more memory");
       ((OB)L54)->header.tag=FILE1_tag;
       L53 = ((FILE1) L54);
       r10 = L53;
       
       SATTR(r10,fp,stdout);
       ret_val24 = r10;
       FILE_plus_STR(ret_val24, plus_s10);
      }
      create_self7 = ((OUT) NULL);
      ret_val25 = create_self7;
      plus_self11 = ret_val25;
      plus_s11 = ((STR) &doeswrite);
      stdout_self11 = ((FILE1) NULL);
      L57=ZALLOC(sizeof(struct FILE1_struct));
      if (L57==NULL) FATAL("Unable to allocate more memory");
      ((OB)L57)->header.tag=FILE1_tag;
      L56 = ((FILE1) L57);
      r11 = L56;
      
      SATTR(r11,fp,stdout);
      ret_val27 = r11;
      FILE_plus_STR(ret_val27, plus_s11);
      ret_val26 = plus_self11;
      plus_self12 = ret_val26;
      L60=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
      if (L60==NULL) FATAL("Unable to allocate more memory");
      memset(L60,0,sizeof(struct BOOL_boxed_struct));
      ((OB)L60)->header.tag=BOOL_tag;
      L59 = (dSTR)((BOOL_boxed) L60);
      ((BOOL_boxed) L59)->immutable_part = ATTR(e,doeswrite);
      plus_s12 = L59;
      stdout_self12 = ((FILE1) NULL);
      L62=ZALLOC(sizeof(struct FILE1_struct));
      if (L62==NULL) FATAL("Unable to allocate more memory");
      ((OB)L62)->header.tag=FILE1_tag;
      L61 = ((FILE1) L62);
      r12 = L61;
      
      SATTR(r12,fp,stdout);
      ret_val29 = r12;
      FILE_plus_dSTR(ret_val29, plus_s12);
      ret_val28 = plus_self12;
      plus_self13 = ret_val28;
      plus_s13 = ((STR) &name9);
      stdout_self13 = ((FILE1) NULL);
      L65=ZALLOC(sizeof(struct FILE1_struct));
      if (L65==NULL) FATAL("Unable to allocate more memory");
      ((OB)L65)->header.tag=FILE1_tag;
      L64 = ((FILE1) L65);
      r13 = L64;
      
      SATTR(r13,fp,stdout);
      ret_val30 = r13;
      FILE_plus_STR(ret_val30, plus_s13);
     }
     ret_val = FALSE;
     return ret_val;
    }
    else {
     L67 = AM_CUR1358945253(((AM_CURSOR) NULL), c11);
     L68 = AM_CUR1044001004(self, L67, ATTR(e,tp), ATTR(e,name1));
     L691_=!(L68); 
     if (L691_) {
      ret_val = FALSE;
      return ret_val;
     }
    }
   }
  }
  after_loop: ;
  {
   BOOL f_L701_ = TRUE;
   BOOL f_L711_ = TRUE;
   /* loop index variable */
   L51 = 0;
   L21 = am;
   L31 = am;
   L701_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
   L711_br=L31==NULL?0:ASIZE((AM_ROU1916046290)L31); 
   i_L711_=0;
   while (1) {
    if(L51>=L701_br)  goto after_loop1; 
    aL701_=ARR((AM_ROU1916046290)L21,L51); 
    L73=aL701_;
    L74 = ATTR(L73,mode1);
    L75 = (*dMODE_814247358[TAG(L74)])(L74, ((dMODE) MODES_in_mode));
    L761_=!(L75); 
    if (L761_) {
     ret_val = FALSE;
     return ret_val;
    }
    L77 = AM_CUR1358945253(((AM_CURSOR) NULL), c11);
    if(i_L711_>=L711_br)  goto after_loop1; 
    aL711_=ARR((AM_ROU1916046290)L31,i_L711_); i_L711_++;
    L79 = AM_CUR2135357807(self, L77, ((dAM) aL711_));
    L801_=!(L79); 
    if (L801_) {
     ret_val = FALSE;
     return ret_val;
    }
    L821_=INTPLUS(L51,1); 
    L51 = L821_;
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

BOOL AM_CUR750559803(AM_CURSOR self, AM_CURSOR c, dAM am) {
 BOOL ret_val = BOOL_zero;
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
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self1;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
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
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT create_self2;
 OUT ret_val17;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val18;
 FILE1 stdout_self8;
 FILE1 ret_val19;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val20;
 FILE1 stdout_self9;
 FILE1 ret_val21;
 FILE1 r9;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val22;
 FILE1 r10;
 OUT create_self3;
 OUT ret_val23;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val24;
 FILE1 stdout_self11;
 FILE1 ret_val25;
 FILE1 r11;
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
 extern STR is_const_inself;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR c211;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L11;
 extern STR am1;
 FILE1 L13;
 OB L14;
 extern STR is_const_in;
 FILE1 L16;
 OB L17;
 INT L191_;
 INT L20;
 STR L211_;
 FILE1 L22;
 OB L23;
 extern STR name9;
 FILE1 L25;
 OB L26;
 BOOL L28;
 AM_CURSOR L29;
 BOOL L30;
 BOOL L32;
 AM_CURSOR L33;
 AM_CURSOR L34;
 BOOL L35;
 BOOL L361_;
 AM_CURSOR L37;
 extern STR thefol1378826944;
 FILE1 L38;
 OB L39;
 INT L411_;
 INT L42;
 STR L431_;
 FILE1 L44;
 OB L45;
 extern STR name9;
 FILE1 L47;
 OB L48;
 extern STR thefol1378826944;
 FILE1 L50;
 OB L51;
 INT L531_;
 INT L54;
 STR L551_;
 FILE1 L56;
 OB L57;
 extern STR name9;
 FILE1 L59;
 OB L60;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &is_const_inself);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = AM_CUR720390882(self);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &c211);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val7 = r2;
  FILE_plus_STR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = AM_CUR720390882(c);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val9 = r3;
  FILE_plus_STR(ret_val9, plus_s3);
  ret_val8 = plus_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &am1);
  stdout_self4 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r4 = L13;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  AM_OUT1439666796(((AM_OUT) NULL), am);
 }
 if (AM_CUR45133320(self, am)) {
  ret_val = TRUE;
  return ret_val;
 }
 if (AM_CURSOR_debug) {
  create_self1 = ((OUT) NULL);
  ret_val11 = create_self1;
  plus_self5 = ret_val11;
  plus_s5 = ((STR) &is_const_in);
  stdout_self5 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r5 = L16;
  
  SATTR(r5,fp,stdout);
  ret_val13 = r5;
  FILE_plus_STR(ret_val13, plus_s5);
  ret_val12 = plus_self5;
  plus_self6 = ret_val12;
  L191_=SYSTP(am); 
  L20 = L191_;
  L211_=SYSSTRFORTP(L20); 
  plus_s6 = L211_;
  stdout_self6 = ((FILE1) NULL);
  L23=ZALLOC(sizeof(struct FILE1_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=FILE1_tag;
  L22 = ((FILE1) L23);
  r6 = L22;
  
  SATTR(r6,fp,stdout);
  ret_val15 = r6;
  FILE_plus_STR(ret_val15, plus_s6);
  ret_val14 = plus_self6;
  plus_self7 = ret_val14;
  plus_s7 = ((STR) &name9);
  stdout_self7 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r7 = L25;
  
  SATTR(r7,fp,stdout);
  ret_val16 = r7;
  FILE_plus_STR(ret_val16, plus_s7);
 }
 if (am==NULL) {
  goto other255;
 } else
 switch (TAG(am)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = AM_CUR1240489726(self, c, ((AM_LOCAL_EXPR) am));
   return ret_val; break;
  case AM_ROU1916046290_tag:
   ret_val = AM_CUR1093203002(self, c, ((AM_ROU1916046290) am));
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   L29 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
   if (AM_CUR1044001004(self, L29, ATTR(((AM_ATTR_EXPR) am),self_tp), ATTR(((AM_ATTR_EXPR) am),at))) {
    L28 = AM_CUR750559803(self, c, ((dAM) ATTR(((AM_ATTR_EXPR) am),ob)));
   } else {
    L28 = FALSE;
   }
   ret_val = L28;
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = AM_CUR176216201(self, c, ATTR(((AM_SHARED_EXPR) am),class_tp), ATTR(((AM_SHARED_EXPR) am),name1));
   return ret_val; break;
  case AM_IS_VOID_EXPR_tag:
   ret_val = AM_CUR750559803(self, c, ((dAM) ATTR(((AM_IS_VOID_EXPR) am),arg)));
   return ret_val; break;
  case AM_CALL_ARG_tag:
   ret_val = AM_CUR750559803(self, c, ((dAM) ATTR(((AM_CALL_ARG) am),expr)));
   return ret_val; break;
  case AM_IF_EXPR_tag:
   L33 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
   if (AM_CUR750559803(self, L33, ((dAM) ATTR(((AM_IF_EXPR) am),test1)))) {
    L34 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
    L32 = AM_CUR750559803(self, L34, ((dAM) ATTR(((AM_IF_EXPR) am),if_true)));
   } else {
    L32 = FALSE;
   }
   if (L32) {
    L30 = AM_CUR750559803(self, c, ((dAM) ATTR(((AM_IF_EXPR) am),if_false)));
   } else {
    L30 = FALSE;
   }
   ret_val = L30;
   return ret_val; break;
  case AM_STMT_EXPR_tag:
   L35 = (ATTR(((AM_STMT_EXPR) am),replaced)==((dAM_EXPR) NULL));
   L361_=!(L35); 
   if (L361_) {
    L37 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
    ret_val = AM_CUR750559803(self, L37, ((dAM) ATTR(((AM_STMT_EXPR) am),replaced)));
    return ret_val;
   }
   else {
    if (AM_CURSOR_debug) {
     create_self2 = ((OUT) NULL);
     ret_val17 = create_self2;
     plus_self8 = ret_val17;
     plus_s8 = ((STR) &thefol1378826944);
     stdout_self8 = ((FILE1) NULL);
     L39=ZALLOC(sizeof(struct FILE1_struct));
     if (L39==NULL) FATAL("Unable to allocate more memory");
     ((OB)L39)->header.tag=FILE1_tag;
     L38 = ((FILE1) L39);
     r8 = L38;
     
     SATTR(r8,fp,stdout);
     ret_val19 = r8;
     FILE_plus_STR(ret_val19, plus_s8);
     ret_val18 = plus_self8;
     plus_self9 = ret_val18;
     L411_=SYSTP(am); 
     L42 = L411_;
     L431_=SYSSTRFORTP(L42); 
     plus_s9 = L431_;
     stdout_self9 = ((FILE1) NULL);
     L45=ZALLOC(sizeof(struct FILE1_struct));
     if (L45==NULL) FATAL("Unable to allocate more memory");
     ((OB)L45)->header.tag=FILE1_tag;
     L44 = ((FILE1) L45);
     r9 = L44;
     
     SATTR(r9,fp,stdout);
     ret_val21 = r9;
     FILE_plus_STR(ret_val21, plus_s9);
     ret_val20 = plus_self9;
     plus_self10 = ret_val20;
     plus_s10 = ((STR) &name9);
     stdout_self10 = ((FILE1) NULL);
     L48=ZALLOC(sizeof(struct FILE1_struct));
     if (L48==NULL) FATAL("Unable to allocate more memory");
     ((OB)L48)->header.tag=FILE1_tag;
     L47 = ((FILE1) L48);
     r10 = L47;
     
     SATTR(r10,fp,stdout);
     ret_val22 = r10;
     FILE_plus_STR(ret_val22, plus_s10);
    }
    ret_val = FALSE;
    return ret_val;
   } break;
  default: ;
  other255: ;
   if (AM_CURSOR_debug) {
    create_self3 = ((OUT) NULL);
    ret_val23 = create_self3;
    plus_self11 = ret_val23;
    plus_s11 = ((STR) &thefol1378826944);
    stdout_self11 = ((FILE1) NULL);
    L51=ZALLOC(sizeof(struct FILE1_struct));
    if (L51==NULL) FATAL("Unable to allocate more memory");
    ((OB)L51)->header.tag=FILE1_tag;
    L50 = ((FILE1) L51);
    r11 = L50;
    
    SATTR(r11,fp,stdout);
    ret_val25 = r11;
    FILE_plus_STR(ret_val25, plus_s11);
    ret_val24 = plus_self11;
    plus_self12 = ret_val24;
    L531_=SYSTP(am); 
    L54 = L531_;
    L551_=SYSSTRFORTP(L54); 
    plus_s12 = L551_;
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
    plus_s13 = ((STR) &name9);
    stdout_self13 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r13 = L59;
    
    SATTR(r13,fp,stdout);
    ret_val28 = r13;
    FILE_plus_STR(ret_val28, plus_s13);
   }
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR762735158(AM_CURSOR self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_LOOP_STMT lp;
 AM_CURSOR c11;
 AM_CURSOR L11;
 dAM t;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val1 = BOOL_zero;
 dAM L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 if (am==NULL) {
  goto other278;
 } else
 switch (TAG(am)) {
  case AM_LOCAL_EXPR_tag:
   lp = AM_CUR153953493(self);
   c11 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ATTR(self,top1));
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
     if (t==NULL) {
      goto other279;
     } else
     switch (TAG(t)) {
      case AM_LOCAL_EXPR_tag:
       is_eq_self = ((AM_LOCAL_EXPR) am);
       is_eq_a = ((OB) t);
       ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
       if (ret_val1) {
        ret_val = FALSE;
        return ret_val;
       } break;
      case AM_LOOP_STMT_tag:
       L31_=SYSID(lp); 
       L4 = L31_;
       L51_=SYSID(t); 
       L6 = L51_;
       L71_=(L4)==(L6); 
       if (L71_) {
        AM_CUR1083119301(c11);
       } break;
      default: ;
      other279: ;
     }
    }
   }
   after_loop: ;
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other278: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR830576175(AM_CURSOR self, AM_CURSOR pafter, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR dummy_cursor;
 AM_CURSOR after1 = ((AM_CURSOR) NULL);
 dAM_EXPR old_dest = ((dAM_EXPR) NULL);
 AM_ASSIGN_STMT old_as = ((AM_ASSIGN_STMT) NULL);
 AM_CURSOR a;
 dAM s;
 dAM as;
 AM_CURSOR cx;
 AM_CURSOR L11;
 AM_CURSOR c;
 INT branch1 = INT_zero;
 dAM tcur;
 BOOL no_cond = BOOL_zero;
 INT a_branch = INT_zero;
 dAM a_tcur;
 BOOL in_same_branch = BOOL_zero;
 A_STAC1244966098 L21;
 AM_CURSOR_POS ts;
 A_STAC1244966098 L31;
 AM_CURSOR_POS tself;
 dAM s1;
 AM_CURSOR cx1;
 AM_CURSOR L41;
 dAM x;
 AM_CURSOR_POS p;
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
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
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
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n;
 OUT create_self4;
 OUT ret_val13;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
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
 OUT create_self7;
 OUT ret_val19;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 AM_VOID_CONST create_self8;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VOID_CONST ret_val21;
 AM_VOID_CONST r10;
 OUT create_self9;
 OUT ret_val22;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val23;
 FILE1 stdout_self10;
 FILE1 ret_val24;
 FILE1 r11;
 OUT plus_self11;
 dSTR plus_s11;
 OUT ret_val25;
 FILE1 stdout_self11;
 FILE1 ret_val26;
 FILE1 r12;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val27;
 FILE1 r13;
 AM_OUT AM_one_stmt_self1;
 dAM AM_one_stmt_a1;
 dAM_STMT n11;
 OUT create_self10;
 OUT ret_val28;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val29;
 FILE1 r14;
 A_STAC1244966098 size_self;
 INT ret_val30 = INT_zero;
 OUT create_self11;
 OUT ret_val31;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val32;
 FILE1 r15;
 OUT create_self12;
 OUT ret_val33;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val34;
 FILE1 r16;
 AM_OUT AM_one_stmt_self2;
 dAM AM_one_stmt_a2;
 dAM_STMT n2;
 OUT create_self13;
 OUT ret_val35;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val36;
 FILE1 r17;
 OUT create_self14;
 OUT ret_val37;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val38;
 FILE1 stdout_self17;
 FILE1 ret_val39;
 FILE1 r18;
 OUT plus_self18;
 dSTR plus_s18;
 OUT ret_val40;
 FILE1 stdout_self18;
 FILE1 ret_val41;
 FILE1 r19;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val42;
 FILE1 r20;
 A_STAC1244966098 reverse_eltb_sel;
 AM_CURSOR_POS ret_val43;
 FLISTA886630705 L51;
 BOOL reverse_eltb_if_ = BOOL_zero;
 A_STAC1244966098 reverse_eltb_sel1;
 AM_CURSOR_POS ret_val44;
 FLISTA886630705 L61;
 BOOL reverse_eltb_if_1 = BOOL_zero;
 OUT create_self15;
 OUT ret_val45;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val46;
 FILE1 r21;
 AM_OUT AM_one_stmt_self3;
 dAM AM_one_stmt_a3;
 dAM_STMT n3;
 OUT create_self16;
 OUT ret_val47;
 OUT plus_self21;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val48;
 FILE1 r22;
 OUT create_self17;
 OUT ret_val49;
 OUT plus_self22;
 STR plus_s22;
 FILE1 stdout_self22;
 FILE1 ret_val50;
 FILE1 r23;
 OUT create_self18;
 OUT ret_val51;
 OUT plus_self23;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val52;
 FILE1 r24;
 OUT create_self19;
 OUT ret_val53;
 OUT plus_self24;
 STR plus_s24;
 OUT ret_val54;
 FILE1 stdout_self24;
 FILE1 ret_val55;
 FILE1 r25;
 OUT plus_self25;
 dSTR plus_s25;
 OUT ret_val56;
 FILE1 stdout_self25;
 FILE1 ret_val57;
 FILE1 r26;
 OUT plus_self26;
 STR plus_s26;
 FILE1 stdout_self26;
 FILE1 ret_val58;
 FILE1 r27;
 OUT create_self20;
 OUT ret_val59;
 OUT plus_self27;
 STR plus_s27;
 FILE1 stdout_self27;
 FILE1 ret_val60;
 FILE1 r28;
 OUT create_self21;
 OUT ret_val61;
 OUT plus_self28;
 STR plus_s28;
 FILE1 stdout_self28;
 FILE1 ret_val62;
 FILE1 r29;
 A_STAC1244966098 size_self1;
 INT ret_val63 = INT_zero;
 OUT create_self22;
 OUT ret_val64;
 OUT plus_self29;
 STR plus_s29;
 FILE1 stdout_self29;
 FILE1 ret_val65;
 FILE1 r30;
 OUT create_self23;
 OUT ret_val66;
 OUT plus_self30;
 STR plus_s30;
 FILE1 stdout_self30;
 FILE1 ret_val67;
 FILE1 r31;
 extern STR is_exe861833386;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L12;
 extern STR after11;
 FILE1 L14;
 OB L15;
 FILE1 L17;
 OB L18;
 extern STR am2;
 FILE1 L20;
 OB L22;
 extern STR is_not123263715;
 FILE1 L24;
 OB L25;
 dAM_STMT L27;
 dAM_STMT L28;
 dAM_STMT L29;
 extern STR expressiototest;
 FILE1 L30;
 OB L32;
 extern STR name9;
 FILE1 L34;
 OB L35;
 AM_CURSOR L37;
 BOOL L38;
 BOOL L391_;
 extern STR RESULTfalse;
 FILE1 L40;
 OB L42;
 extern STR RESULTfalse;
 FILE1 L44;
 OB L45;
 BOOL L47;
 dAM L48;
 dAM L49;
 INT L501_;
 INT L52;
 INT L531_;
 INT L54;
 BOOL L551_;
 AM_VOID_CONST L56;
 OB L57;
 extern STR no_con1660239222;
 FILE1 L58;
 OB L59;
 dSTR L62;
 OB L63;
 FILE1 L64;
 OB L65;
 extern STR tcur11;
 FILE1 L67;
 OB L68;
 dAM_STMT L70;
 dAM_STMT L71;
 dAM_STMT L72;
 extern STR name9;
 FILE1 L73;
 OB L74;
 BOOL L76;
 BOOL L771_;
 BOOL L781_;
 BOOL L791_;
 BOOL L80;
 BOOL L811_;
 BOOL L821_;
 BOOL L83;
 BOOL L841_;
 BOOL L851_;
 BOOL L86;
 INT L87;
 BOOL L881_;
 BOOL L891_;
 BOOL L90;
 ARRAYdAM_EXPR L92;
 INT L931_;
 INT L94;
 BOOL L951_;
 BOOL L961_;
 INT L97;
 BOOL L981_;
 AM_CURSOR_POS L99;
 AM_CURSOR L100;
 BOOL L101;
 BOOL L1021_;
 extern STR is_exe758215276;
 FILE1 L103;
 OB L104;
 BOOL L106;
 BOOL L1071_;
 extern STR tcur2;
 FILE1 L108;
 OB L109;
 dAM_STMT L111;
 dAM_STMT L112;
 dAM_STMT L113;
 extern STR name9;
 FILE1 L114;
 OB L115;
 extern STR no_cond1;
 FILE1 L117;
 OB L118;
 dSTR L120;
 OB L121;
 FILE1 L122;
 OB L123;
 extern STR name9;
 FILE1 L125;
 OB L126;
 BOOL L128;
 BOOL L1291_;
 AM_CURSOR_POS L130;
 AM_CURSOR_POS L132;
 INT L133;
 INT L134;
 BOOL L1351_;
 BOOL L1361_;
 INT L137;
 BOOL L1381_;
 dAM L139;
 INT L1401_;
 INT L141;
 dAM L142;
 INT L1431_;
 INT L144;
 BOOL L1451_;
 extern STR a_tcur1;
 FILE1 L146;
 OB L147;
 dAM_STMT L149;
 dAM_STMT L150;
 dAM_STMT L151;
 extern STR name9;
 FILE1 L152;
 OB L153;
 BOOL L155;
 BOOL L1561_;
 INT L1571_;
 INT L158;
 INT L1591_;
 INT L160;
 BOOL L1621_;
 BOOL L163;
 BOOL L1641_;
 BOOL L165;
 BOOL L1661_;
 extern STR is_exe377520954;
 FILE1 L167;
 OB L168;
 extern STR theexp686419287;
 FILE1 L170;
 OB L171;
 BOOL L1731_;
 extern STR branch11;
 FILE1 L174;
 OB L175;
 dSTR L177;
 OB L178;
 FILE1 L179;
 OB L180;
 extern STR testin1363464486;
 FILE1 L182;
 OB L183;
 extern STR LOOP;
 FILE1 L185;
 OB L186;
 BOOL L188;
 BOOL L1891_;
 AM_CURSOR L190;
 BOOL L192;
 BOOL L1931_;
 extern STR is_exe1111937962;
 FILE1 L194;
 OB L195;
 INT L197;
 BOOL L1981_;
 BOOL L199;
 BOOL L2001_;
 extern STR is_exe377520954;
 FILE1 L201;
 OB L202;
 extern STR theexp686419287;
 FILE1 L204;
 OB L205;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &is_exe861833386);
  stdout_self = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r = L7;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  create_self1 = ((OUT) NULL);
  ret_val3 = create_self1;
  plus_self1 = ret_val3;
  plus_s1 = AM_CUR720390882(self);
  stdout_self1 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L10 = ((FILE1) L12);
  r1 = L10;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &after11);
  stdout_self2 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r2 = L14;
  
  SATTR(r2,fp,stdout);
  ret_val7 = r2;
  FILE_plus_STR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = AM_CUR720390882(pafter);
  stdout_self3 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r3 = L17;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  create_self2 = ((OUT) NULL);
  ret_val9 = create_self2;
  plus_self4 = ret_val9;
  plus_s4 = ((STR) &am2);
  stdout_self4 = ((FILE1) NULL);
  L22=ZALLOC(sizeof(struct FILE1_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=FILE1_tag;
  L20 = ((FILE1) L22);
  r4 = L20;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  AM_OUT1439666796(((AM_OUT) NULL), am);
 }
 dummy_cursor = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ((dAM) NULL));
 if (AM_CUR1481630356(self)) {
  ret_val = FALSE;
  return ret_val;
 }
 a = AM_CUR1358945253(((AM_CURSOR) NULL), pafter);
 while (1) {
  AM_CUR1769624888(a);
  s = ATTR(a,cur);
  if ((s==((dAM) NULL))) {
   goto after_loop;
  }
  if (s==NULL) {
   goto other256;
  } else
  switch (TAG(s)) {
   case AM_ASSIGN_STMT_tag:
    if (ATTR(ATTR(self,prog),opt_debug)) {
     create_self3 = ((OUT) NULL);
     ret_val11 = create_self3;
     plus_self5 = ret_val11;
     plus_s5 = ((STR) &is_not123263715);
     stdout_self5 = ((FILE1) NULL);
     L25=ZALLOC(sizeof(struct FILE1_struct));
     if (L25==NULL) FATAL("Unable to allocate more memory");
     ((OB)L25)->header.tag=FILE1_tag;
     L24 = ((FILE1) L25);
     r5 = L24;
     
     SATTR(r5,fp,stdout);
     ret_val12 = r5;
     FILE_plus_STR(ret_val12, plus_s5);
     AM_one_stmt_self = ((AM_OUT) NULL);
     AM_one_stmt_a = s;
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
     create_self4 = ((OUT) NULL);
     ret_val13 = create_self4;
     plus_self6 = ret_val13;
     plus_s6 = ((STR) &expressiototest);
     stdout_self6 = ((FILE1) NULL);
     L32=ZALLOC(sizeof(struct FILE1_struct));
     if (L32==NULL) FATAL("Unable to allocate more memory");
     ((OB)L32)->header.tag=FILE1_tag;
     L30 = ((FILE1) L32);
     r6 = L30;
     
     SATTR(r6,fp,stdout);
     ret_val14 = r6;
     FILE_plus_STR(ret_val14, plus_s6);
     AM_OUT1439666796(((AM_OUT) NULL), am);
     create_self5 = ((OUT) NULL);
     ret_val15 = create_self5;
     plus_self7 = ret_val15;
     plus_s7 = ((STR) &name9);
     stdout_self7 = ((FILE1) NULL);
     L35=ZALLOC(sizeof(struct FILE1_struct));
     if (L35==NULL) FATAL("Unable to allocate more memory");
     ((OB)L35)->header.tag=FILE1_tag;
     L34 = ((FILE1) L35);
     r7 = L34;
     
     SATTR(r7,fp,stdout);
     ret_val16 = r7;
     FILE_plus_STR(ret_val16, plus_s7);
    }
    L37 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ((dAM) ATTR(((AM_ASSIGN_STMT) s),dest)));
    L38 = AM_CUR1926902782(self, L37, am);
    L391_=!(L38); 
    if (L391_) {
     if (ATTR(ATTR(self,prog),opt_debug)) {
      create_self6 = ((OUT) NULL);
      ret_val17 = create_self6;
      plus_self8 = ret_val17;
      plus_s8 = ((STR) &RESULTfalse);
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
     ret_val = FALSE;
     return ret_val;
    }
    if (ATTR(ATTR(self,prog),opt_debug)) {
     create_self7 = ((OUT) NULL);
     ret_val19 = create_self7;
     plus_self9 = ret_val19;
     plus_s9 = ((STR) &RESULTfalse);
     stdout_self9 = ((FILE1) NULL);
     L45=ZALLOC(sizeof(struct FILE1_struct));
     if (L45==NULL) FATAL("Unable to allocate more memory");
     ((OB)L45)->header.tag=FILE1_tag;
     L44 = ((FILE1) L45);
     r9 = L44;
     
     SATTR(r9,fp,stdout);
     ret_val20 = r9;
     FILE_plus_STR(ret_val20, plus_s9);
    } break;
   default: ;
   other256: ;
  }
 }
 after_loop: ;
 if (ATTR(pafter,assign_in_order)) {
  L47 = AM_CUR1481630356(pafter);
 } else {
  L47 = FALSE;
 }
 if (L47) {
  after1 = AM_CUR1358945253(((AM_CURSOR) NULL), pafter);
  AM_CUR1769624888(after1);
  as = ATTR(after1,cur);
  if (as==NULL) {
   goto other257;
  } else
  switch (TAG(as)) {
   case AM_ASSIGN_STMT_tag:
    cx = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ((dAM) ATTR(((AM_ASSIGN_STMT) as),src1)));
    {
     struct AM_CUR1040670508_frame_struct other2_0;
AM_CUR1040670508_frame noname1 = &other2_0;
     L11 = cx;
     noname1->self = L11;
     noname1->state = 0;
     while (1) {
      L48 = AM_CUR1040670508(noname1);
      if (noname1->state == -1) {
       goto after_loop1;
      }
      L49 = L48;
      L501_=SYSID(L49); 
      L52 = L501_;
      L531_=SYSID(am); 
      L54 = L531_;
      L551_=(L52)==(L54); 
      if (L551_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
    }
    after_loop1: ;
    old_dest = ATTR(((AM_ASSIGN_STMT) as),dest);
    old_as = ((AM_ASSIGN_STMT) as);
    create_self8 = ((AM_VOID_CONST) NULL);
    create_source = SFILE_ID_zero;
    L57=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=AM_VOID_CONST_tag;
    L56 = ((AM_VOID_CONST) L57);
    r10 = L56;
    SATTR(r10,source1,create_source);
    ret_val21 = r10;
    SATTR(((AM_ASSIGN_STMT) as),dest,((dAM_EXPR) ret_val21)); break;
   default: ;
   other257: ;
  }
 }
 else {
  after1 = pafter;
 }
 c = AM_CUR1358945253(((AM_CURSOR) NULL), after1);
 branch1 = 0;
 tcur = ATTR(c,cur);
 no_cond = FALSE;
 while (1) {
  if (AM_CURSOR_debug) {
   create_self9 = ((OUT) NULL);
   ret_val22 = create_self9;
   plus_self10 = ret_val22;
   plus_s10 = ((STR) &no_con1660239222);
   stdout_self10 = ((FILE1) NULL);
   L59=ZALLOC(sizeof(struct FILE1_struct));
   if (L59==NULL) FATAL("Unable to allocate more memory");
   ((OB)L59)->header.tag=FILE1_tag;
   L58 = ((FILE1) L59);
   r11 = L58;
   
   SATTR(r11,fp,stdout);
   ret_val24 = r11;
   FILE_plus_STR(ret_val24, plus_s10);
   ret_val23 = plus_self10;
   plus_self11 = ret_val23;
   L63=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   memset(L63,0,sizeof(struct INT_boxed_struct));
   ((OB)L63)->header.tag=INT_tag;
   L62 = (dSTR)((INT_boxed) L63);
   ((INT_boxed) L62)->immutable_part = branch1;
   plus_s11 = L62;
   stdout_self11 = ((FILE1) NULL);
   L65=ZALLOC(sizeof(struct FILE1_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=FILE1_tag;
   L64 = ((FILE1) L65);
   r12 = L64;
   
   SATTR(r12,fp,stdout);
   ret_val26 = r12;
   FILE_plus_dSTR(ret_val26, plus_s11);
   ret_val25 = plus_self11;
   plus_self12 = ret_val25;
   plus_s12 = ((STR) &tcur11);
   stdout_self12 = ((FILE1) NULL);
   L68=ZALLOC(sizeof(struct FILE1_struct));
   if (L68==NULL) FATAL("Unable to allocate more memory");
   ((OB)L68)->header.tag=FILE1_tag;
   L67 = ((FILE1) L68);
   r13 = L67;
   
   SATTR(r13,fp,stdout);
   ret_val27 = r13;
   FILE_plus_STR(ret_val27, plus_s12);
   AM_one_stmt_self1 = ((AM_OUT) NULL);
   AM_one_stmt_a1 = tcur;
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
     L70 = ((dAM_STMT) AM_one_stmt_a1);
     n11 = (*dAM_ST775224435[TAG(L70)])(L70);
     L71 = ((dAM_STMT) AM_one_stmt_a1);
     (*dAM_ST1372701974[TAG(L71)])(L71, ((dAM_STMT) NULL));
     AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
     L72 = ((dAM_STMT) AM_one_stmt_a1);
     (*dAM_ST1372701974[TAG(L72)])(L72, n11); break;
    default: ;
     AM_OUT1439666796(AM_one_stmt_self1, AM_one_stmt_a1);
   }
   create_self10 = ((OUT) NULL);
   ret_val28 = create_self10;
   plus_self13 = ret_val28;
   plus_s13 = ((STR) &name9);
   stdout_self13 = ((FILE1) NULL);
   L74=ZALLOC(sizeof(struct FILE1_struct));
   if (L74==NULL) FATAL("Unable to allocate more memory");
   ((OB)L74)->header.tag=FILE1_tag;
   L73 = ((FILE1) L74);
   r14 = L73;
   
   SATTR(r14,fp,stdout);
   ret_val29 = r14;
   FILE_plus_STR(ret_val29, plus_s13);
  }
  if (tcur==NULL) {
   goto other258;
  } else
  switch (TAG(tcur)) {
   case AM_IF_STMT_tag:
    L771_=(1)<(branch1); 
    if (L771_) {
     L781_=(branch1)<(999999); 
     L76 = L781_;
    } else {
     L76 = FALSE;
    }
    if (L76) {
     goto after_loop2;
    } break;
   case AM_IF_EXPR_tag:
    L791_=(1)<(branch1); 
    if (L791_) {
     goto after_loop2;
    } break;
   case AM_TYPECASE_STMT_tag:
    L811_=(1)<(branch1); 
    if (L811_) {
     L821_=(branch1)<(999999); 
     L80 = L821_;
    } else {
     L80 = FALSE;
    }
    if (L80) {
     goto after_loop2;
    } break;
   case AM_CASE_STMT_tag:
    L841_=(1)<(branch1); 
    if (L841_) {
     L851_=(branch1)<(999999); 
     L83 = L851_;
    } else {
     L83 = FALSE;
    }
    if (L83) {
     goto after_loop2;
    } break;
   case AM_LOCK_STMT_tag:
    L87 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) tcur),guards));
    L881_=(L87)<(branch1); 
    if (L881_) {
     L891_=(branch1)<(999999); 
     L86 = L891_;
    } else {
     L86 = FALSE;
    }
    if (L86) {
     goto after_loop2;
    } break;
   case AM_WIT1996971603_tag:
    L92 = ATTR(((AM_WIT1996971603) tcur),objects1);
    L931_=(L92)==NULL?0:ASIZE((ARRAYdAM_EXPR)L92); 
    L94 = L931_;
    L951_=(L94)<(branch1); 
    if (L951_) {
     L961_=(branch1)<(999999); 
     L90 = L961_;
    } else {
     L90 = FALSE;
    }
    if (L90) {
     goto after_loop2;
    } break;
   default: ;
   other258: ;
  }
  size_self = ATTR(c,stack1);
  ret_val30 = FLISTA587993904(ATTR(size_self,s));
  L97 = ret_val30;
  L981_=(L97)==(0); 
  if (L981_) {
   no_cond = TRUE;
   goto after_loop2;
  }
  L99=AM_CUR61708328(c);
  branch1 = ATTR(L99,branch);
  tcur = ATTR(c,cur);
 }
 after_loop2: ;
 L100 = AM_CUR1358945253(((AM_CURSOR) NULL), after1);
 L101 = AM_CUR750559803(self, L100, am);
 L1021_=!(L101); 
 if (L1021_) {
  if (AM_CURSOR_debug) {
   create_self11 = ((OUT) NULL);
   ret_val31 = create_self11;
   plus_self14 = ret_val31;
   plus_s14 = ((STR) &is_exe758215276);
   stdout_self14 = ((FILE1) NULL);
   L104=ZALLOC(sizeof(struct FILE1_struct));
   if (L104==NULL) FATAL("Unable to allocate more memory");
   ((OB)L104)->header.tag=FILE1_tag;
   L103 = ((FILE1) L104);
   r15 = L103;
   
   SATTR(r15,fp,stdout);
   ret_val32 = r15;
   FILE_plus_STR(ret_val32, plus_s14);
  }
  L106 = (old_as==((AM_ASSIGN_STMT) NULL));
  L1071_=!(L106); 
  if (L1071_) {
   SATTR(old_as,dest,old_dest);
  }
  ret_val = FALSE;
  return ret_val;
 }
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 a_branch = 0;
 a_tcur = ATTR(c,cur);
 if (AM_CURSOR_debug) {
  create_self12 = ((OUT) NULL);
  ret_val33 = create_self12;
  plus_self15 = ret_val33;
  plus_s15 = ((STR) &tcur2);
  stdout_self15 = ((FILE1) NULL);
  L109=ZALLOC(sizeof(struct FILE1_struct));
  if (L109==NULL) FATAL("Unable to allocate more memory");
  ((OB)L109)->header.tag=FILE1_tag;
  L108 = ((FILE1) L109);
  r16 = L108;
  
  SATTR(r16,fp,stdout);
  ret_val34 = r16;
  FILE_plus_STR(ret_val34, plus_s15);
  AM_one_stmt_self2 = ((AM_OUT) NULL);
  AM_one_stmt_a2 = tcur;
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
    L111 = ((dAM_STMT) AM_one_stmt_a2);
    n2 = (*dAM_ST775224435[TAG(L111)])(L111);
    L112 = ((dAM_STMT) AM_one_stmt_a2);
    (*dAM_ST1372701974[TAG(L112)])(L112, ((dAM_STMT) NULL));
    AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
    L113 = ((dAM_STMT) AM_one_stmt_a2);
    (*dAM_ST1372701974[TAG(L113)])(L113, n2); break;
   default: ;
    AM_OUT1439666796(AM_one_stmt_self2, AM_one_stmt_a2);
  }
  create_self13 = ((OUT) NULL);
  ret_val35 = create_self13;
  plus_self16 = ret_val35;
  plus_s16 = ((STR) &name9);
  stdout_self16 = ((FILE1) NULL);
  L115=ZALLOC(sizeof(struct FILE1_struct));
  if (L115==NULL) FATAL("Unable to allocate more memory");
  ((OB)L115)->header.tag=FILE1_tag;
  L114 = ((FILE1) L115);
  r17 = L114;
  
  SATTR(r17,fp,stdout);
  ret_val36 = r17;
  FILE_plus_STR(ret_val36, plus_s16);
  create_self14 = ((OUT) NULL);
  ret_val37 = create_self14;
  plus_self17 = ret_val37;
  plus_s17 = ((STR) &no_cond1);
  stdout_self17 = ((FILE1) NULL);
  L118=ZALLOC(sizeof(struct FILE1_struct));
  if (L118==NULL) FATAL("Unable to allocate more memory");
  ((OB)L118)->header.tag=FILE1_tag;
  L117 = ((FILE1) L118);
  r18 = L117;
  
  SATTR(r18,fp,stdout);
  ret_val39 = r18;
  FILE_plus_STR(ret_val39, plus_s17);
  ret_val38 = plus_self17;
  plus_self18 = ret_val38;
  L121=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
  if (L121==NULL) FATAL("Unable to allocate more memory");
  memset(L121,0,sizeof(struct BOOL_boxed_struct));
  ((OB)L121)->header.tag=BOOL_tag;
  L120 = (dSTR)((BOOL_boxed) L121);
  ((BOOL_boxed) L120)->immutable_part = no_cond;
  plus_s18 = L120;
  stdout_self18 = ((FILE1) NULL);
  L123=ZALLOC(sizeof(struct FILE1_struct));
  if (L123==NULL) FATAL("Unable to allocate more memory");
  ((OB)L123)->header.tag=FILE1_tag;
  L122 = ((FILE1) L123);
  r19 = L122;
  
  SATTR(r19,fp,stdout);
  ret_val41 = r19;
  FILE_plus_dSTR(ret_val41, plus_s18);
  ret_val40 = plus_self18;
  plus_self19 = ret_val40;
  plus_s19 = ((STR) &name9);
  stdout_self19 = ((FILE1) NULL);
  L126=ZALLOC(sizeof(struct FILE1_struct));
  if (L126==NULL) FATAL("Unable to allocate more memory");
  ((OB)L126)->header.tag=FILE1_tag;
  L125 = ((FILE1) L126);
  r20 = L125;
  
  SATTR(r20,fp,stdout);
  ret_val42 = r20;
  FILE_plus_STR(ret_val42, plus_s19);
 }
 L128 = (old_as==((AM_ASSIGN_STMT) NULL));
 L1291_=!(L128); 
 if (L1291_) {
  SATTR(old_as,dest,old_dest);
 }
 in_same_branch = TRUE;
 {
  struct FLISTA1660160_frame_struct other4_0;
FLISTA1660160_frame noname2 = &other4_0;
  struct FLISTA1660160_frame_struct other4_1;
FLISTA1660160_frame noname3 = &other4_1;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::reverse_elt!:AM_CURSOR_POS */
  L21 = ATTR(pafter,stack1);
  reverse_eltb_sel = L21;
  L51 = ATTRs(reverse_eltb_sel,s,((FLISTA886630705) NULL));
  noname2->self = L51;
  noname2->state = 0;
  noname3->state = 0;
  reverse_eltb_if_ = TRUE;
  reverse_eltb_if_1 = TRUE;
  while (1) {
   L130 = FLISTA1660160(noname2);
   if (noname2->state == -1) {
    goto after_loop3;
   }
   ts = L130;
   if (in_same_branch) {
    if (reverse_eltb_if_1) {
     L31 = ATTR(self,stack1);
     reverse_eltb_sel1 = L31;
     reverse_eltb_if_1 = FALSE;
    }
    if (noname3->state == 0) {
     L61 = ATTR(reverse_eltb_sel1,s);
     noname3->self = L61;
    }
    L132 = FLISTA1660160(noname3);
    if (noname3->state == -1) {
     goto after_loop3;
    }
    tself = L132;
    L133 = ATTR(ts,branch);
    L134 = ATTR(tself,branch);
    L1351_=(L133)==(L134); 
    in_same_branch = L1351_;
   }
   L1361_=!(in_same_branch); 
   if (L1361_) {
    s1 = ATTR(ts,stmt);
    if (s1==NULL) {
     goto other259;
    } else
    switch (TAG(s1)) {
     case AM_LOOP_STMT_tag:
      L137 = ATTR(ts,branch);
      L1381_=(L137)==(2); 
      if (L1381_) {
       cx1 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ((dAM) ATTR(((AM_LOOP_STMT) s1),body)));
       {
        struct AM_CUR1040670508_frame_struct other5_0;
AM_CUR1040670508_frame noname4 = &other5_0;
        L41 = cx1;
        noname4->self = L41;
        noname4->state = 0;
        while (1) {
         L139 = AM_CUR1040670508(noname4);
         if (noname4->state == -1) {
          goto after_loop4;
         }
         x = L139;
         L1401_=SYSID(x); 
         L141 = L1401_;
         L142 = ATTR(pafter,cur);
         L1431_=SYSID(L142); 
         L144 = L1431_;
         L1451_=(L141)==(L144); 
         if (L1451_) {
          goto after_loop4;
         }
         if (x==NULL) {
          goto other260;
         } else
         switch (TAG(x)) {
          case AM_ITE1809135850_tag:
           ret_val = FALSE;
           return ret_val; break;
          case AM_BND367211769_tag:
           ret_val = FALSE;
           return ret_val; break;
          case AM_BREAK_STMT_tag:
           ret_val = FALSE;
           return ret_val; break;
          default: ;
          other260: ;
         }
        }
       }
       after_loop4: ;
      }
      goto after_loop3; break;
     default: ;
     other259: ;
    }
   }
  }
 }
 after_loop3: ;
 while (1) {
  if (AM_CURSOR_debug) {
   create_self15 = ((OUT) NULL);
   ret_val45 = create_self15;
   plus_self20 = ret_val45;
   plus_s20 = ((STR) &a_tcur1);
   stdout_self20 = ((FILE1) NULL);
   L147=ZALLOC(sizeof(struct FILE1_struct));
   if (L147==NULL) FATAL("Unable to allocate more memory");
   ((OB)L147)->header.tag=FILE1_tag;
   L146 = ((FILE1) L147);
   r21 = L146;
   
   SATTR(r21,fp,stdout);
   ret_val46 = r21;
   FILE_plus_STR(ret_val46, plus_s20);
   AM_one_stmt_self3 = ((AM_OUT) NULL);
   AM_one_stmt_a3 = a_tcur;
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
     L149 = ((dAM_STMT) AM_one_stmt_a3);
     n3 = (*dAM_ST775224435[TAG(L149)])(L149);
     L150 = ((dAM_STMT) AM_one_stmt_a3);
     (*dAM_ST1372701974[TAG(L150)])(L150, ((dAM_STMT) NULL));
     AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
     L151 = ((dAM_STMT) AM_one_stmt_a3);
     (*dAM_ST1372701974[TAG(L151)])(L151, n3); break;
    default: ;
     AM_OUT1439666796(AM_one_stmt_self3, AM_one_stmt_a3);
   }
   create_self16 = ((OUT) NULL);
   ret_val47 = create_self16;
   plus_self21 = ret_val47;
   plus_s21 = ((STR) &name9);
   stdout_self21 = ((FILE1) NULL);
   L153=ZALLOC(sizeof(struct FILE1_struct));
   if (L153==NULL) FATAL("Unable to allocate more memory");
   ((OB)L153)->header.tag=FILE1_tag;
   L152 = ((FILE1) L153);
   r22 = L152;
   
   SATTR(r22,fp,stdout);
   ret_val48 = r22;
   FILE_plus_STR(ret_val48, plus_s21);
  }
  L1561_=!(no_cond); 
  if (L1561_) {
   L1571_=SYSID(tcur); 
   L158 = L1571_;
   L1591_=SYSID(a_tcur); 
   L160 = L1591_;
   L1621_=(L158)==(L160); 
   L155 = L1621_;
  } else {
   L155 = FALSE;
  }
  if (L155) {
   L1641_=(branch1)==(a_branch); 
   L165 = L1641_;
   L1661_=!(L165); 
   if (L1661_) {
    L163 = AM_CURSOR_debug;
   } else {
    L163 = FALSE;
   }
   if (L163) {
    create_self17 = ((OUT) NULL);
    ret_val49 = create_self17;
    plus_self22 = ret_val49;
    plus_s22 = ((STR) &is_exe377520954);
    stdout_self22 = ((FILE1) NULL);
    L168=ZALLOC(sizeof(struct FILE1_struct));
    if (L168==NULL) FATAL("Unable to allocate more memory");
    ((OB)L168)->header.tag=FILE1_tag;
    L167 = ((FILE1) L168);
    r23 = L167;
    
    SATTR(r23,fp,stdout);
    ret_val50 = r23;
    FILE_plus_STR(ret_val50, plus_s22);
    create_self18 = ((OUT) NULL);
    ret_val51 = create_self18;
    plus_self23 = ret_val51;
    plus_s23 = ((STR) &theexp686419287);
    stdout_self23 = ((FILE1) NULL);
    L171=ZALLOC(sizeof(struct FILE1_struct));
    if (L171==NULL) FATAL("Unable to allocate more memory");
    ((OB)L171)->header.tag=FILE1_tag;
    L170 = ((FILE1) L171);
    r24 = L170;
    
    SATTR(r24,fp,stdout);
    ret_val52 = r24;
    FILE_plus_STR(ret_val52, plus_s23);
   }
   L1731_=(branch1)==(a_branch); 
   ret_val = L1731_;
   return ret_val;
  }
  if (a_tcur==NULL) {
   goto other261;
  } else
  switch (TAG(a_tcur)) {
   case AM_LOOP_STMT_tag:
    if (AM_CURSOR_debug) {
     create_self19 = ((OUT) NULL);
     ret_val53 = create_self19;
     plus_self24 = ret_val53;
     plus_s24 = ((STR) &branch11);
     stdout_self24 = ((FILE1) NULL);
     L175=ZALLOC(sizeof(struct FILE1_struct));
     if (L175==NULL) FATAL("Unable to allocate more memory");
     ((OB)L175)->header.tag=FILE1_tag;
     L174 = ((FILE1) L175);
     r25 = L174;
     
     SATTR(r25,fp,stdout);
     ret_val55 = r25;
     FILE_plus_STR(ret_val55, plus_s24);
     ret_val54 = plus_self24;
     plus_self25 = ret_val54;
     L178=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L178==NULL) FATAL("Unable to allocate more memory");
     memset(L178,0,sizeof(struct INT_boxed_struct));
     ((OB)L178)->header.tag=INT_tag;
     L177 = (dSTR)((INT_boxed) L178);
     ((INT_boxed) L177)->immutable_part = branch1;
     plus_s25 = L177;
     stdout_self25 = ((FILE1) NULL);
     L180=ZALLOC(sizeof(struct FILE1_struct));
     if (L180==NULL) FATAL("Unable to allocate more memory");
     ((OB)L180)->header.tag=FILE1_tag;
     L179 = ((FILE1) L180);
     r26 = L179;
     
     SATTR(r26,fp,stdout);
     ret_val57 = r26;
     FILE_plus_dSTR(ret_val57, plus_s25);
     ret_val56 = plus_self25;
     plus_self26 = ret_val56;
     plus_s26 = ((STR) &testin1363464486);
     stdout_self26 = ((FILE1) NULL);
     L183=ZALLOC(sizeof(struct FILE1_struct));
     if (L183==NULL) FATAL("Unable to allocate more memory");
     ((OB)L183)->header.tag=FILE1_tag;
     L182 = ((FILE1) L183);
     r27 = L182;
     
     SATTR(r27,fp,stdout);
     ret_val58 = r27;
     FILE_plus_STR(ret_val58, plus_s26);
     AM_OUT1439666796(((AM_OUT) NULL), am);
     create_self20 = ((OUT) NULL);
     ret_val59 = create_self20;
     plus_self27 = ret_val59;
     plus_s27 = ((STR) &LOOP);
     stdout_self27 = ((FILE1) NULL);
     L186=ZALLOC(sizeof(struct FILE1_struct));
     if (L186==NULL) FATAL("Unable to allocate more memory");
     ((OB)L186)->header.tag=FILE1_tag;
     L185 = ((FILE1) L186);
     r28 = L185;
     
     SATTR(r28,fp,stdout);
     ret_val60 = r28;
     FILE_plus_STR(ret_val60, plus_s27);
     AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(((AM_LOOP_STMT) a_tcur),body)));
    }
    L1891_=(1)<(branch1); 
    if (L1891_) {
     L190 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(self,prog), ((dAM) ATTR(((AM_LOOP_STMT) a_tcur),body)));
     L192 = AM_CUR750559803(dummy_cursor, L190, am);
     L1931_=!(L192); 
     L188 = L1931_;
    } else {
     L188 = FALSE;
    }
    if (L188) {
     if (AM_CURSOR_debug) {
      create_self21 = ((OUT) NULL);
      ret_val61 = create_self21;
      plus_self28 = ret_val61;
      plus_s28 = ((STR) &is_exe1111937962);
      stdout_self28 = ((FILE1) NULL);
      L195=ZALLOC(sizeof(struct FILE1_struct));
      if (L195==NULL) FATAL("Unable to allocate more memory");
      ((OB)L195)->header.tag=FILE1_tag;
      L194 = ((FILE1) L195);
      r29 = L194;
      
      SATTR(r29,fp,stdout);
      ret_val62 = r29;
      FILE_plus_STR(ret_val62, plus_s28);
     }
     ret_val = FALSE;
     return ret_val;
    } break;
   default: ;
   other261: ;
  }
  size_self1 = ATTR(c,stack1);
  ret_val63 = FLISTA587993904(ATTR(size_self1,s));
  L197 = ret_val63;
  L1981_=(L197)==(0); 
  if (L1981_) {
   if (AM_CURSOR_debug) {
    L2001_=!(no_cond); 
    L199 = L2001_;
   } else {
    L199 = FALSE;
   }
   if (L199) {
    create_self22 = ((OUT) NULL);
    ret_val64 = create_self22;
    plus_self29 = ret_val64;
    plus_s29 = ((STR) &is_exe377520954);
    stdout_self29 = ((FILE1) NULL);
    L202=ZALLOC(sizeof(struct FILE1_struct));
    if (L202==NULL) FATAL("Unable to allocate more memory");
    ((OB)L202)->header.tag=FILE1_tag;
    L201 = ((FILE1) L202);
    r30 = L201;
    
    SATTR(r30,fp,stdout);
    ret_val65 = r30;
    FILE_plus_STR(ret_val65, plus_s29);
    create_self23 = ((OUT) NULL);
    ret_val66 = create_self23;
    plus_self30 = ret_val66;
    plus_s30 = ((STR) &theexp686419287);
    stdout_self30 = ((FILE1) NULL);
    L205=ZALLOC(sizeof(struct FILE1_struct));
    if (L205==NULL) FATAL("Unable to allocate more memory");
    ((OB)L205)->header.tag=FILE1_tag;
    L204 = ((FILE1) L205);
    r31 = L204;
    
    SATTR(r31,fp,stdout);
    ret_val67 = r31;
    FILE_plus_STR(ret_val67, plus_s30);
   }
   ret_val = no_cond;
   return ret_val;
  }
  p = AM_CUR61708328(c);
  a_tcur = ATTR(p,stmt);
  a_branch = ATTR(p,branch);
 }
 after_loop5: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CUR954266477(AM_CURSOR self) {
 BOOL ret_val = BOOL_zero;
 dAM s;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS L1;
 top_self = ATTR(self,stack1);
 ret_val1 = FLISTA860779230(ATTR(top_self,s));
 L1=ret_val1;
 s = ATTR(L1,stmt);
 if (s==NULL) {
  goto other267;
 } else
 switch (TAG(s)) {
  case AM_PREFETCH_STMT_tag:
   ret_val = FALSE;
   return ret_val; break;
  default: ;
  other267: ;
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_CUR1778373732(AM_CURSOR self, AM_CURSOR cc) {
 INT ret_val = INT_zero;
 AM_CURSOR c;
 INT w = INT_zero;
 AM_CURSOR L11;
 dAM am;
 INT L21 = INT_zero;
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
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 INT L31 = INT_zero;
 OUT create_self2;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT create_self3;
 OUT ret_val13;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val14;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val16;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 extern STR WEIGHT586643509;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR c311;
 FILE1 L10;
 OB L12;
 FILE1 L14;
 OB L15;
 dAM L17;
 extern STR WW;
 FILE1 L18;
 OB L19;
 extern STR name10;
 FILE1 L23;
 OB L24;
 INT L261_;
 INT L271_;
 INT L28;
 STR L291_;
 FILE1 L30;
 OB L32;
 extern STR name9;
 FILE1 L34;
 OB L35;
 FILE1 L37;
 OB L38;
 SE_CONTEXT L40;
 INT L41;
 INT L421_;
 SE_CONTEXT L43;
 INT L44;
 INT L451_;
 INT L461_;
 INT L471_;
 INT L481_;
 INT L491_;
 INT L501_;
 INT L511_;
 INT L521_;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &WEIGHT586643509);
  stdout_self = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r = L4;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = AM_CUR720390882(self);
  stdout_self1 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r1 = L7;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &c311);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L10 = ((FILE1) L12);
  r2 = L10;
  
  SATTR(r2,fp,stdout);
  ret_val7 = r2;
  FILE_plus_STR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = AM_CUR720390882(cc);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
 }
 c = AM_CUR1358945253(((AM_CURSOR) NULL), cc);
 w = 0;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   if (AM_CUR1885691330(c, self)) {
    goto after_loop;
   }
   L17 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L17;
   if (AM_CURSOR_debug) {
    create_self1 = ((OUT) NULL);
    ret_val9 = create_self1;
    plus_self4 = ret_val9;
    plus_s4 = ((STR) &WW);
    stdout_self4 = ((FILE1) NULL);
    L19=ZALLOC(sizeof(struct FILE1_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=FILE1_tag;
    L18 = ((FILE1) L19);
    r4 = L18;
    
    SATTR(r4,fp,stdout);
    ret_val10 = r4;
    FILE_plus_STR(ret_val10, plus_s4);
    {
     BOOL f_L221_ = TRUE;
     /* loop index variable */
     L31 = 0;
     L21 = ATTR(c,indent);
     while (1) {
      if(L31>=L21)  goto after_loop1; 
      ;
      create_self2 = ((OUT) NULL);
      ret_val11 = create_self2;
      plus_self5 = ret_val11;
      plus_s5 = ((STR) &name10);
      stdout_self5 = ((FILE1) NULL);
      L24=ZALLOC(sizeof(struct FILE1_struct));
      if (L24==NULL) FATAL("Unable to allocate more memory");
      ((OB)L24)->header.tag=FILE1_tag;
      L23 = ((FILE1) L24);
      r5 = L23;
      
      SATTR(r5,fp,stdout);
      ret_val12 = r5;
      FILE_plus_STR(ret_val12, plus_s5);
      L261_=INTPLUS(L31,1); 
      L31 = L261_;
     }
    }
    after_loop1: ;
    create_self3 = ((OUT) NULL);
    ret_val13 = create_self3;
    plus_self6 = ret_val13;
    L271_=SYSTP(am); 
    L28 = L271_;
    L291_=SYSSTRFORTP(L28); 
    plus_s6 = L291_;
    stdout_self6 = ((FILE1) NULL);
    L32=ZALLOC(sizeof(struct FILE1_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    ((OB)L32)->header.tag=FILE1_tag;
    L30 = ((FILE1) L32);
    r6 = L30;
    
    SATTR(r6,fp,stdout);
    ret_val15 = r6;
    FILE_plus_STR(ret_val15, plus_s6);
    ret_val14 = plus_self6;
    plus_self7 = ret_val14;
    plus_s7 = ((STR) &name9);
    stdout_self7 = ((FILE1) NULL);
    L35=ZALLOC(sizeof(struct FILE1_struct));
    if (L35==NULL) FATAL("Unable to allocate more memory");
    ((OB)L35)->header.tag=FILE1_tag;
    L34 = ((FILE1) L35);
    r7 = L34;
    
    SATTR(r7,fp,stdout);
    ret_val17 = r7;
    FILE_plus_STR(ret_val17, plus_s7);
    ret_val16 = plus_self7;
    plus_self8 = ret_val16;
    plus_s8 = AM_CUR720390882(c);
    stdout_self8 = ((FILE1) NULL);
    L38=ZALLOC(sizeof(struct FILE1_struct));
    if (L38==NULL) FATAL("Unable to allocate more memory");
    ((OB)L38)->header.tag=FILE1_tag;
    L37 = ((FILE1) L38);
    r8 = L37;
    
    SATTR(r8,fp,stdout);
    ret_val18 = r8;
    FILE_plus_STR(ret_val18, plus_s8);
   }
   if (am==NULL) {
    goto other324;
   } else
   switch (TAG(am)) {
    case AM_ROU1916046290_tag:
     L40=SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ATTR(self,prog));
     L41 = ATTR(L40,weight);
     L421_=INTPLUS(w,L41); 
     w = L421_; break;
    case AM_ITE1809135850_tag:
     L43=SIG_ge642497605(ATTR(((AM_ITE1809135850) am),fun), ATTR(self,prog));
     L44 = ATTR(L43,weight);
     L451_=INTPLUS(w,L44); 
     w = L451_; break;
    case AM_BND260301329_tag:
     L461_=INTPLUS(w,100); 
     w = L461_; break;
    case AM_BND367211769_tag:
     L471_=INTPLUS(w,100); 
     w = L471_; break;
    case AM_LOOP_STMT_tag:
     L481_=INTPLUS(w,10); 
     w = L481_; break;
    case AM_WAITFOR_STMT_tag:
     L491_=INTPLUS(w,10); 
     w = L491_; break;
    case AM_PREFETCH_STMT_tag:
     L501_=INTPLUS(w,10); 
     w = L501_; break;
    case AM_AT_EXPR_tag:
     L511_=INTPLUS(w,2); 
     w = L511_; break;
    default: ;
    other324: ;
   }
   L521_=(w<100000)?w:100000; 
   w = L521_;
  }
 }
 after_loop: ;
 ret_val = w;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM AM_CUR1623871191(AM_CURSOR self) {
 dAM ret_val;
 dAM tcur;
 AM_CURSOR_POS create_self;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r1;
 UNIX exit_self;
 INT exit_code = INT_zero;
 dAM_STMT L1;
 AM_CURSOR_POS L2;
 OB L3;
 dAM_STMT L4;
 extern STR callin1037273329;
 FILE1 L5;
 OB L6;
 if ((ATTR(self,cur)==((dAM) NULL))) {
  ret_val = ((dAM) NULL);
  return ret_val;
 }
 tcur = ATTR(self,cur);
 if (tcur==NULL) {
  goto other301;
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
   L1 = ((dAM_STMT) tcur);
   if (((*dAM_ST775224435[TAG(L1)])(L1)==((dAM_STMT) NULL))) {
    AM_CUR1769624888(self);
    ret_val = AM_CUR1623871191(self);
    return ret_val;
   }
   else {
    push_self = ATTR(self,stack1);
    create_self = ((AM_CURSOR_POS) NULL);
    create_curr_stmt = ATTR(self,cur);
    create_m = ATTR(self,mark);
    create_branch = 999999;
    L3=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L3==NULL) FATAL("Unable to allocate more memory");
    ((OB)L3)->header.tag=AM_CURSOR_POS_tag;
    L2 = ((AM_CURSOR_POS) L3);
    r = L2;
    SATTR(r,stmt,create_curr_stmt);
    SATTR(r,branch,create_branch);
    SATTR(r,mark,create_m);
    ret_val1 = r;
    push_e = ret_val1;
    SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
    L4 = ((dAM_STMT) tcur);
    SATTR(self,cur,((dAM) (*dAM_ST775224435[TAG(L4)])(L4)));
    SATTR(self,mark,FALSE);
    ret_val = ATTR(self,cur);
    return ret_val;
   } break;
  default: ;
  other301: ;
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self = ret_val2;
   plus_s = ((STR) &callin1037273329);
   stdout_self = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r1 = L5;
   
   SATTR(r1,fp,stdout);
   ret_val3 = r1;
   FILE_plus_STR(ret_val3, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
   ret_val = ((dAM) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM AM_CUR938531823(AM_CURSOR self, INT t) {
 dAM ret_val;
 A_STAC1244966098 L11;
 AM_CURSOR_POS a;
 A_STAC1244966098 topb_self;
 AM_CURSOR_POS ret_val1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL topb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val2;
 INT L5;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 AM_CURSOR_POS L81_;
 BOOL L9;
 INT L10;
 BOOL L121_;
 dAM L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 {
  BOOL f_L41_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  topb_self = L11;
  topb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(topb_self,s);
   if (f_L41_) {
    f_L41_ = FALSE;
    L6 = FLISTA587993904(ATTR(topb_self,s));
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31 = 0;
    L41_=L21+1;L41_m=L31; 
   }
   if(L41_--<=L41_m)  goto after_loop; 
   aget_ind = L41_;
   L81_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val2 = L81_;
   a = ret_val2;
   L10 = ATTR(a,branch);
   L121_=(L10)<(999999); 
   if (L121_) {
    L13 = ATTR(a,stmt);
    L141_=SYSTP(L13); 
    L15 = L141_;
    L161_=(L15)==(t); 
    L9 = L161_;
   } else {
    L9 = FALSE;
   }
   if (L9) {
    ret_val = ATTR(a,stmt);
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = ((dAM) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_CUR1430796545(AM_CURSOR self) {
 dAM_STMT ret_val;
 AM_CURSOR c;
 dAM t;
 c = AM_CUR1358945253(((AM_CURSOR) NULL), self);
 AM_CUR1769624888(c);
 t = ATTR(c,cur);
 if (t==NULL) {
  goto other298;
 } else
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
   ret_val = ((dAM_STMT) t);
   return ret_val; break;
  default: ;
  other298: ;
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT AM_CUR643033494(AM_CURSOR self) {
 dAM_STMT ret_val;
 A_STAC1244966098 L11;
 AM_CURSOR_POS a;
 dAM s;
 A_STAC1244966098 topb_self;
 AM_CURSOR_POS ret_val1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL topb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val2;
 INT L5;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 AM_CURSOR_POS L81_;
 INT L9;
 BOOL L101_;
 INT L12;
 BOOL L131_;
 {
  BOOL f_L41_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  topb_self = L11;
  topb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(topb_self,s);
   if (f_L41_) {
    f_L41_ = FALSE;
    L6 = FLISTA587993904(ATTR(topb_self,s));
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31 = 0;
    L41_=L21+1;L41_m=L31; 
   }
   if(L41_--<=L41_m)  goto after_loop; 
   aget_ind = L41_;
   L81_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val2 = L81_;
   a = ret_val2;
   s = ATTR(a,stmt);
   L9 = ATTR(a,branch);
   L101_=(L9)<(999999); 
   if (L101_) {
    if (s==NULL) {
     goto other293;
    } else
    switch (TAG(s)) {
     case AM_LOOP_STMT_tag:
      L12 = ATTR(a,branch);
      L131_=(1)<(L12); 
      if (L131_) {
       ret_val = ((dAM_STMT) s);
       return ret_val;
      } break;
     case AM_LOCK_STMT_tag:
      ret_val = ((dAM_STMT) s);
      return ret_val; break;
     default: ;
     other293: ;
    }
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1083119301(AM_CURSOR self) {
 dAM tcur;
 AM_CURSOR_POS create_self;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 AM_CURSOR_POS create_self1;
 dAM create_curr_stmt1;
 BOOL create_m1 = BOOL_zero;
 INT create_branch1 = INT_zero;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS r1;
 A_STAC1244966098 push_self1;
 AM_CURSOR_POS push_e1;
 AM_COMMENT_STMT create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val2;
 AM_COMMENT_STMT r2;
 OUT create_self3;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r3;
 UNIX exit_self;
 INT exit_code = INT_zero;
 AM_CURSOR_POS L1;
 OB L2;
 AM_CURSOR_POS L3;
 OB L4;
 INT L5;
 INT L61_;
 AM_COMMENT_STMT L7;
 OB L8;
 extern STR Intern419679125;
 FILE1 L9;
 OB L10;
 tcur = ATTR(self,cur);
 if (tcur==NULL) {
  goto other277;
 } else
 switch (TAG(tcur)) {
  case AM_LOOP_STMT_tag:
   if ((ATTR(((AM_LOOP_STMT) tcur),body)==((dAM_STMT) NULL))) {
    if ((ATTR(((AM_LOOP_STMT) tcur),init)==((dAM_STMT) NULL))) {
     return;
    }
    push_self = ATTR(self,stack1);
    create_self = ((AM_CURSOR_POS) NULL);
    create_curr_stmt = ATTR(self,cur);
    create_m = ATTR(self,mark);
    create_branch = 1;
    L2=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L2==NULL) FATAL("Unable to allocate more memory");
    ((OB)L2)->header.tag=AM_CURSOR_POS_tag;
    L1 = ((AM_CURSOR_POS) L2);
    r = L1;
    SATTR(r,stmt,create_curr_stmt);
    SATTR(r,branch,create_branch);
    SATTR(r,mark,create_m);
    ret_val = r;
    push_e = ret_val;
    SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
   }
   else {
    push_self1 = ATTR(self,stack1);
    create_self1 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt1 = ATTR(self,cur);
    create_m1 = ATTR(self,mark);
    create_branch1 = 2;
    L4=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    ((OB)L4)->header.tag=AM_CURSOR_POS_tag;
    L3 = ((AM_CURSOR_POS) L4);
    r1 = L3;
    SATTR(r1,stmt,create_curr_stmt1);
    SATTR(r1,branch,create_branch1);
    SATTR(r1,mark,create_m1);
    ret_val1 = r1;
    push_e1 = ret_val1;
    SATTR(push_self1,s,FLISTA1475379054(ATTR(push_self1,s), push_e1));
   }
   L5 = ATTR(self,indent);
   L61_=INTPLUS(L5,1); 
   SATTR(self,indent,L61_);
   create_self2 = ((AM_COMMENT_STMT) NULL);
   create_source = SFILE_ID_zero;
   L8=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=AM_COMMENT_STMT_tag;
   L7 = ((AM_COMMENT_STMT) L8);
   r2 = L7;
   SATTR(r2,source1,create_source);
   ret_val2 = r2;
   SATTR(self,cur,((dAM) ret_val2));
   SATTR(self,mark,FALSE); break;
  default: ;
  other277: ;
   create_self3 = ((OUT) NULL);
   ret_val3 = create_self3;
   plus_self = ret_val3;
   plus_s = ((STR) &Intern419679125);
   stdout_self = ((FILE1) NULL);
   L10=ZALLOC(sizeof(struct FILE1_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=FILE1_tag;
   L9 = ((FILE1) L10);
   r3 = L9;
   
   SATTR(r3,fp,stdout);
   ret_val4 = r3;
   FILE_plus_STR(ret_val4, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1119852682(AM_CURSOR self) {
 A_STAC1244966098 L11;
 AM_CURSOR_POS a;
 A_STAC1244966098 topb_self;
 AM_CURSOR_POS ret_val;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 BOOL topb_if_first = BOOL_zero;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val1;
 INT L5;
 INT L6;
 INT L71_;
 INT L41_,L41_m;
 AM_CURSOR_POS L81_;
 INT L9;
 BOOL L101_;
 {
  BOOL f_L41_ = TRUE;
  /* Initialization of inlined iter A_STACK{AM_CURSOR_POS}::top!:AM_CURSOR_POS */
  L11 = ATTR(self,stack1);
  topb_self = L11;
  topb_if_first = TRUE;
  while (1) {
   aget_self = ATTR(topb_self,s);
   if (f_L41_) {
    f_L41_ = FALSE;
    L6 = FLISTA587993904(ATTR(topb_self,s));
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31 = 0;
    L41_=L21+1;L41_m=L31; 
   }
   if(L41_--<=L41_m)  goto after_loop; 
   aget_ind = L41_;
   L81_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
   ret_val1 = L81_;
   a = ret_val1;
   if (ATTR(a,mark)) {
    goto after_loop;
   }
   L9 = ATTR(a,branch);
   L101_=(L9)<(999999); 
   if (L101_) {
    SATTR(a,mark,TRUE);
   }
  }
 }
 after_loop: ;
 SATTR(self,mark,TRUE);
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1127255611(AM_CURSOR self) {
 dAM tcur;
 AM_CURSOR_POS create_self;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 AM_CURSOR_POS create_self1;
 dAM create_curr_stmt1;
 BOOL create_m1 = BOOL_zero;
 INT create_branch1 = INT_zero;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS r1;
 A_STAC1244966098 push_self1;
 AM_CURSOR_POS push_e1;
 AM_CURSOR_POS create_self2;
 dAM create_curr_stmt2;
 BOOL create_m2 = BOOL_zero;
 INT create_branch2 = INT_zero;
 AM_CURSOR_POS ret_val2;
 AM_CURSOR_POS r2;
 A_STAC1244966098 push_self2;
 AM_CURSOR_POS push_e2;
 AM_COMMENT_STMT create_self3;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val3;
 AM_COMMENT_STMT r3;
 OUT create_self4;
 OUT ret_val4;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 AM_CURSOR_POS L1;
 OB L2;
 AM_CURSOR_POS L3;
 OB L4;
 AM_CURSOR_POS L5;
 OB L6;
 AM_COMMENT_STMT L7;
 OB L8;
 INT L9;
 INT L101_;
 extern STR Intern419679125;
 FILE1 L11;
 OB L12;
 tcur = ATTR(self,cur);
 if (tcur==NULL) {
  goto other302;
 } else
 switch (TAG(tcur)) {
  case AM_IF_STMT_tag:
   if ((ATTR(((AM_IF_STMT) tcur),if_false)==((dAM_STMT) NULL))) {
    if ((ATTR(((AM_IF_STMT) tcur),if_true)==((dAM_STMT) NULL))) {
     if ((ATTR(((AM_IF_STMT) tcur),test1)==((dAM_EXPR) NULL))) {
      return;
     }
     else {
      push_self = ATTR(self,stack1);
      create_self = ((AM_CURSOR_POS) NULL);
      create_curr_stmt = ATTR(self,cur);
      create_m = ATTR(self,mark);
      create_branch = 1;
      L2=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
      if (L2==NULL) FATAL("Unable to allocate more memory");
      ((OB)L2)->header.tag=AM_CURSOR_POS_tag;
      L1 = ((AM_CURSOR_POS) L2);
      r = L1;
      SATTR(r,stmt,create_curr_stmt);
      SATTR(r,branch,create_branch);
      SATTR(r,mark,create_m);
      ret_val = r;
      push_e = ret_val;
      SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
     }
    }
    else {
     push_self1 = ATTR(self,stack1);
     create_self1 = ((AM_CURSOR_POS) NULL);
     create_curr_stmt1 = ATTR(self,cur);
     create_m1 = ATTR(self,mark);
     create_branch1 = 2;
     L4=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
     if (L4==NULL) FATAL("Unable to allocate more memory");
     ((OB)L4)->header.tag=AM_CURSOR_POS_tag;
     L3 = ((AM_CURSOR_POS) L4);
     r1 = L3;
     SATTR(r1,stmt,create_curr_stmt1);
     SATTR(r1,branch,create_branch1);
     SATTR(r1,mark,create_m1);
     ret_val1 = r1;
     push_e1 = ret_val1;
     SATTR(push_self1,s,FLISTA1475379054(ATTR(push_self1,s), push_e1));
    }
   }
   else {
    push_self2 = ATTR(self,stack1);
    create_self2 = ((AM_CURSOR_POS) NULL);
    create_curr_stmt2 = ATTR(self,cur);
    create_m2 = ATTR(self,mark);
    create_branch2 = 3;
    L6=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
    if (L6==NULL) FATAL("Unable to allocate more memory");
    ((OB)L6)->header.tag=AM_CURSOR_POS_tag;
    L5 = ((AM_CURSOR_POS) L6);
    r2 = L5;
    SATTR(r2,stmt,create_curr_stmt2);
    SATTR(r2,branch,create_branch2);
    SATTR(r2,mark,create_m2);
    ret_val2 = r2;
    push_e2 = ret_val2;
    SATTR(push_self2,s,FLISTA1475379054(ATTR(push_self2,s), push_e2));
   }
   create_self3 = ((AM_COMMENT_STMT) NULL);
   create_source = SFILE_ID_zero;
   L8=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=AM_COMMENT_STMT_tag;
   L7 = ((AM_COMMENT_STMT) L8);
   r3 = L7;
   SATTR(r3,source1,create_source);
   ret_val3 = r3;
   SATTR(self,cur,((dAM) ret_val3));
   SATTR(self,mark,FALSE);
   L9 = ATTR(self,indent);
   L101_=INTPLUS(L9,1); 
   SATTR(self,indent,L101_); break;
  default: ;
  other302: ;
   create_self4 = ((OUT) NULL);
   ret_val4 = create_self4;
   plus_self = ret_val4;
   plus_s = ((STR) &Intern419679125);
   stdout_self = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L11 = ((FILE1) L12);
   r4 = L11;
   
   SATTR(r4,fp,stdout);
   ret_val5 = r4;
   FILE_plus_STR(ret_val5, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1223064464(AM_CURSOR self) {
 INT cur_loop = INT_zero;
 dAM am;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 UNIX exit_self;
 INT exit_code = INT_zero;
 extern STR oopsse1358845011;
 FILE1 L1;
 OB L2;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 cur_loop = ATTR(self,loops);
 while (1) {
  AM_CUR1769624888(self);
  am = ATTR(self,cur);
  if ((am==((dAM) NULL))) {
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &oopsse1358845011);
   stdout_self = ((FILE1) NULL);
   L2=ZALLOC(sizeof(struct FILE1_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=FILE1_tag;
   L1 = ((FILE1) L2);
   r = L1;
   
   SATTR(r,fp,stdout);
   ret_val1 = r;
   FILE_plus_STR(ret_val1, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
  L4 = ATTR(self,loops);
  L51_=(cur_loop)==(L4); 
  L6 = L51_;
  L71_=!(L6); 
  if (L71_) {
   return;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1389299115(AM_CURSOR self) {
 dAM n;
 AM_CURSOR_POS d;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val;
 A_STAC1244966098 pop_self;
 AM_CURSOR_POS ret_val1;
 AM_CURSOR_POS L1;
 INT L2;
 BOOL L31_;
 while (1) {
  if (AM_CUR1972821118(self, ATTR(self,cur))) {
  }
  else {
   goto after_loop;
  }
  n = AM_CUR710387089(self, ATTR(self,cur), 0);
  if ((n==((dAM) NULL))) {
   return;
  }
  top_self = ATTR(self,stack1);
  ret_val = FLISTA860779230(ATTR(top_self,s));
  L1=ret_val;
  L2 = ATTR(L1,branch);
  L31_=(L2)==(999999); 
  if (L31_) {
   pop_self = ATTR(self,stack1);
   ret_val1 = FLISTA971253650(ATTR(pop_self,s));
   d = ret_val1;
   return;
  }
  SATTR(self,cur,n);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1649762162(AM_CURSOR self) {
 AM_CURSOR_POS a;
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
 A_STAC1244966098 size_self;
 INT ret_val4 = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val5;
 AM_CURSOR pop_stack_self;
 AM_CURSOR_POS d;
 A_STAC1244966098 size_self1;
 INT ret_val6 = INT_zero;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val8;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r3;
 extern STR before1421983347;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 INT L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 extern STR aftere1148777451;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &before1421983347);
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
 if (ATTR(self,mark)) {
  return;
 }
 while (1) {
  size_self = ATTR(self,stack1);
  ret_val4 = FLISTA587993904(ATTR(size_self,s));
  L7 = ret_val4;
  L81_=(0)<(L7); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  top_self = ATTR(self,stack1);
  ret_val5 = FLISTA860779230(ATTR(top_self,s));
  a = ret_val5;
  if (ATTR(a,mark)) {
   goto after_loop;
  }
  pop_stack_self = self;
  d = AM_CUR61708328(pop_stack_self);
  SATTR(self,mark,FALSE);
 }
 after_loop: ;
 size_self1 = ATTR(self,stack1);
 ret_val6 = FLISTA587993904(ATTR(size_self1,s));
 L9 = ret_val6;
 L101_=(L9)==(0); 
 if (L101_) {
  AM_CUR1869307717(self);
 }
 else {
  AM_CUR1389299115(self);
 }
 L11 = (ATTR(self,cur)==((dAM) NULL));
 L121_=!(L11); 
 SATTR(self,ignore_next,L121_);
 if (AM_CURSOR_debug) {
  create_self1 = ((OUT) NULL);
  ret_val7 = create_self1;
  plus_self2 = ret_val7;
  plus_s2 = ((STR) &aftere1148777451);
  stdout_self2 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r2 = L13;
  
  SATTR(r2,fp,stdout);
  ret_val9 = r2;
  FILE_plus_STR(ret_val9, plus_s2);
  ret_val8 = plus_self2;
  plus_self3 = ret_val8;
  plus_s3 = AM_CUR720390882(self);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val10 = r3;
  FILE_plus_STR(ret_val10, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR191948285(AM_CURSOR self) {
 dAM tcur;
 dAM_STMT t;
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
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self2;
 OUT ret_val6;
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
 OUT create_self4;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 OUT create_self5;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val13;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 extern STR CURSOR581910373;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 dAM_STMT L7;
 dAM_STMT L8;
 extern STR objecttodelete;
 FILE1 L9;
 OB L10;
 extern STR objecttodelete;
 FILE1 L12;
 OB L13;
 extern STR name9;
 FILE1 L15;
 OB L16;
 extern STR Intern22142674;
 FILE1 L18;
 OB L19;
 extern STR CURSOR2000469882;
 FILE1 L21;
 OB L22;
 FILE1 L24;
 OB L25;
 tcur = ATTR(self,cur);
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &CURSOR581910373);
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
  AM_OUT1439666796(((AM_OUT) NULL), ATTR(self,top1));
 }
 if (tcur==NULL) {
  goto other273;
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
   L7 = ((dAM_STMT) tcur);
   t = (*dAM_ST775224435[TAG(L7)])(L7);
   L8 = ((dAM_STMT) tcur);
   (*dAM_ST1372701974[TAG(L8)])(L8, ((dAM_STMT) NULL));
   if (AM_CURSOR_debug) {
    create_self1 = ((OUT) NULL);
    ret_val4 = create_self1;
    plus_self2 = ret_val4;
    plus_s2 = ((STR) &objecttodelete);
    stdout_self2 = ((FILE1) NULL);
    L10=ZALLOC(sizeof(struct FILE1_struct));
    if (L10==NULL) FATAL("Unable to allocate more memory");
    ((OB)L10)->header.tag=FILE1_tag;
    L9 = ((FILE1) L10);
    r2 = L9;
    
    SATTR(r2,fp,stdout);
    ret_val5 = r2;
    FILE_plus_STR(ret_val5, plus_s2);
    AM_OUT1439666796(((AM_OUT) NULL), tcur);
   }
   AM_CUR1968472392(self, t); break;
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
   if (AM_CURSOR_debug) {
    create_self2 = ((OUT) NULL);
    ret_val6 = create_self2;
    plus_self3 = ret_val6;
    plus_s3 = ((STR) &objecttodelete);
    stdout_self3 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r3 = L12;
    
    SATTR(r3,fp,stdout);
    ret_val7 = r3;
    FILE_plus_STR(ret_val7, plus_s3);
    AM_OUT1439666796(((AM_OUT) NULL), tcur);
    create_self3 = ((OUT) NULL);
    ret_val8 = create_self3;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &name9);
    stdout_self4 = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r4 = L15;
    
    SATTR(r4,fp,stdout);
    ret_val9 = r4;
    FILE_plus_STR(ret_val9, plus_s4);
   }
   AM_CUR915996644(self, ((dAM_EXPR) NULL)); break;
  default: ;
  other273: ;
   create_self4 = ((OUT) NULL);
   ret_val10 = create_self4;
   plus_self5 = ret_val10;
   plus_s5 = ((STR) &Intern22142674);
   stdout_self5 = ((FILE1) NULL);
   L19=ZALLOC(sizeof(struct FILE1_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=FILE1_tag;
   L18 = ((FILE1) L19);
   r5 = L18;
   
   SATTR(r5,fp,stdout);
   ret_val11 = r5;
   FILE_plus_STR(ret_val11, plus_s5);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
 }
 if (AM_CURSOR_debug) {
  create_self5 = ((OUT) NULL);
  ret_val12 = create_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &CURSOR2000469882);
  stdout_self6 = ((FILE1) NULL);
  L22=ZALLOC(sizeof(struct FILE1_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=FILE1_tag;
  L21 = ((FILE1) L22);
  r6 = L21;
  
  SATTR(r6,fp,stdout);
  ret_val14 = r6;
  FILE_plus_STR(ret_val14, plus_s6);
  ret_val13 = plus_self6;
  plus_self7 = ret_val13;
  plus_s7 = AM_CUR720390882(self);
  stdout_self7 = ((FILE1) NULL);
  L25=ZALLOC(sizeof(struct FILE1_struct));
  if (L25==NULL) FATAL("Unable to allocate more memory");
  ((OB)L25)->header.tag=FILE1_tag;
  L24 = ((FILE1) L25);
  r7 = L24;
  
  SATTR(r7,fp,stdout);
  ret_val15 = r7;
  FILE_plus_STR(ret_val15, plus_s7);
  AM_OUT1439666796(((AM_OUT) NULL), ATTR(self,top1));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1966744225(AM_CURSOR self, dAM_STMT am) {
 dAM_STMT lp;
 AM_LOOP_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 dAM_STMT L4;
 dAM_STMT L5;
 L1=AM_CUR153953493(self);
 lp = ATTR(L1,body);
 L2 = am;
 (*dAM_ST1372701974[TAG(L2)])(L2, ((dAM_STMT) NULL));
 if ((lp==((dAM_STMT) NULL))) {
  SATTR(AM_CUR153953493(self),body,am);
 }
 else {
  while (1) {
   L3 = lp;
   if (((*dAM_ST775224435[TAG(L3)])(L3)==((dAM_STMT) NULL))) {
    goto after_loop;
   }
   L4 = lp;
   lp = (*dAM_ST775224435[TAG(L4)])(L4);
  }
  after_loop: ;
  L5 = lp;
  (*dAM_ST1372701974[TAG(L5)])(L5, am);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR1968472392(AM_CURSOR self, dAM_STMT am) {
 dAM_STMT tcur = ((dAM_STMT) NULL);
 dAM tc;
 dAM_STMT last1 = ((dAM_STMT) NULL);
 dAM f;
 INT branch1 = INT_zero;
 INT b1 = INT_zero;
 FLISTA1327600838 L11;
 ARRAYdAM_EXPR l;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 UNIX exit_self;
 INT exit_code = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val2;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val3;
 FLISTdAM_STMT aget_self;
 INT aget_ind = INT_zero;
 dAM_STMT ret_val4;
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val5;
 FLISTdAM_STMT aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_STMT ret_val6;
 FLISTdAM_STMT aset_self;
 INT aset_ind = INT_zero;
 dAM_STMT aset_val;
 FLISTdAM_STMT aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_STMT ret_val7;
 FLISTdAM_STMT aget_self4;
 INT aget_ind4 = INT_zero;
 dAM_STMT ret_val8;
 FLISTdAM_STMT aget_self5;
 INT aget_ind5 = INT_zero;
 dAM_STMT ret_val9;
 FLISTdAM_STMT aset_self1;
 INT aset_ind1 = INT_zero;
 dAM_STMT aset_val1;
 FLISTdAM_STMT aget_self6;
 INT aget_ind6 = INT_zero;
 dAM_STMT ret_val10;
 FLISTdAM_STMT aget_self7;
 INT aget_ind7 = INT_zero;
 dAM_STMT ret_val11;
 FLISTdAM_STMT aget_self8;
 INT aget_ind8 = INT_zero;
 dAM_STMT ret_val12;
 FLISTdAM_STMT aset_self2;
 INT aset_ind2 = INT_zero;
 dAM_STMT aset_val2;
 FLISTdAM_STMT aget_self9;
 INT aget_ind9 = INT_zero;
 dAM_STMT ret_val13;
 FLISTdAM_STMT aget_self10;
 INT aget_ind10 = INT_zero;
 dAM_STMT ret_val14;
 FLISTdAM_STMT aset_self3;
 INT aset_ind3 = INT_zero;
 dAM_STMT aset_val3;
 FLISTdAM_STMT aget_self11;
 INT aget_ind11 = INT_zero;
 dAM_STMT ret_val15;
 FLISTdAM_STMT aset_self4;
 INT aset_ind4 = INT_zero;
 dAM_STMT aset_val4;
 AM_COMMENT_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val16;
 AM_COMMENT_STMT r1;
 extern STR Errori172290981;
 FILE1 L2;
 OB L3;
 BOOL L5;
 BOOL L61_;
 dAM_STMT L7;
 BOOL L8;
 BOOL L91_;
 dAM_STMT L10;
 dAM_STMT L12;
 dAM_STMT L13;
 dAM L14;
 INT L151_;
 INT L16;
 dAM L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 dAM_STMT L21;
 AM_CURSOR_POS L22;
 AM_CURSOR_POS L23;
 BOOL L241_;
 dAM_STMT L25;
 dAM_STMT L26;
 dAM_STMT L27;
 dAM_STMT L28;
 BOOL L291_;
 dAM_STMT L30;
 BOOL L311_;
 dAM_STMT L32;
 BOOL L331_;
 dAM_STMT L34;
 BOOL L351_;
 dAM_STMT L36;
 INT L37;
 INT L381_;
 INT L39;
 BOOL L401_;
 dAM_STMT L42;
 INT L431_;
 dAM_STMT L441_;
 dAM_STMT L45;
 INT L461_;
 dAM_STMT L471_;
 dAM_STMT L48;
 dAM_STMT L49;
 INT L501_;
 dAM_STMT L511_;
 INT L521_;
 INT L54;
 INT L551_;
 INT L56;
 BOOL L571_;
 dAM_STMT L58;
 INT L591_;
 dAM_STMT L601_;
 dAM_STMT L62;
 INT L631_;
 dAM_STMT L641_;
 dAM_STMT L65;
 dAM_STMT L66;
 INT L671_;
 dAM_STMT L681_;
 INT L691_;
 BOOL L711_;
 dAM_STMT L72;
 INT L73;
 INT L741_;
 INT L75;
 BOOL L761_;
 dAM_STMT L77;
 INT L781_;
 dAM_STMT L791_;
 dAM_STMT L80;
 INT L811_;
 dAM_STMT L821_;
 dAM_STMT L83;
 dAM_STMT L84;
 INT L851_;
 dAM_STMT L861_;
 INT L871_;
 BOOL L891_;
 dAM_STMT L90;
 BOOL L921_;
 dAM_STMT L93;
 BOOL L941_;
 dAM_STMT L95;
 INT L96;
 INT L971_;
 INT L98;
 INT L991_;
 ARRAYdAM_EXPR L100;
 INT L1011_;
 INT L102;
 INT L1031_;
 INT L104;
 BOOL L1051_;
 dAM_STMT L1061_;
 BOOL L107;
 BOOL L1081_;
 dAM_STMT L1091_;
 dAM_STMT L110;
 dAM_STMT L1121_;
 INT L114;
 INT L1151_;
 BOOL L116;
 BOOL L1171_;
 BOOL L118;
 BOOL L1191_;
 dAM_STMT L120;
 ARRAYdAM_EXPR L121;
 INT L1221_;
 INT L123;
 INT L1241_;
 INT L125;
 BOOL L1261_;
 dAM_STMT L127;
 ARRAYdAM_EXPR L128;
 INT L1291_;
 INT L130;
 INT L1311_;
 INT L132;
 BOOL L1331_;
 dAM_STMT L134;
 BOOL L1351_;
 dAM_STMT L136;
 BOOL L1371_;
 dAM_STMT L138;
 AM_COMMENT_STMT L139;
 OB L140;
 tc = ATTR(self,cur);
 if (tc==NULL) {
  goto other265;
 } else
 switch (TAG(tc)) {
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
   tcur = ((dAM_STMT) tc); break;
  default: ;
  other265: ;
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &Errori172290981);
   stdout_self = ((FILE1) NULL);
   L3=ZALLOC(sizeof(struct FILE1_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=FILE1_tag;
   L2 = ((FILE1) L3);
   r = L2;
   
   SATTR(r,fp,stdout);
   ret_val1 = r;
   FILE_plus_STR(ret_val1, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
 }
 L5 = (am==((dAM_STMT) NULL));
 L61_=!(L5); 
 if (L61_) {
  last1 = am;
  while (1) {
   L7 = last1;
   L8 = ((*dAM_ST775224435[TAG(L7)])(L7)==((dAM_STMT) NULL));
   L91_=!(L8); 
   if (L91_) {
   }
   else {
    goto after_loop;
   }
   L10 = last1;
   last1 = (*dAM_ST775224435[TAG(L10)])(L10);
  }
  after_loop: ;
  L12 = tcur;
  L13 = last1;
  (*dAM_ST1372701974[TAG(L13)])(L13, (*dAM_ST775224435[TAG(L12)])(L12));
 }
 L14 = ATTR(self,top1);
 L151_=SYSID(L14); 
 L16 = L151_;
 L17 = ATTR(self,cur);
 L181_=SYSID(L17); 
 L19 = L181_;
 L201_=(L16)==(L19); 
 if (L201_) {
  if ((am==((dAM_STMT) NULL))) {
   L21 = tcur;
   SATTR(self,top1,((dAM) (*dAM_ST775224435[TAG(L21)])(L21)));
  }
  else {
   SATTR(self,top1,((dAM) am));
  }
  AM_CUR1869307717(self);
  return;
 }
 SATTR(self,cur,((dAM) am));
 SATTR(self,ignore_next,TRUE);
 top_self = ATTR(self,stack1);
 ret_val2 = FLISTA860779230(ATTR(top_self,s));
 L22=ret_val2;
 f = ATTR(L22,stmt);
 top_self1 = ATTR(self,stack1);
 ret_val3 = FLISTA860779230(ATTR(top_self1,s));
 L23=ret_val3;
 branch1 = ATTR(L23,branch);
 L241_=(branch1)==(999999); 
 if (L241_) {
  switch (TAG(f)) {
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
    if ((am==((dAM_STMT) NULL))) {
     L25 = ((dAM_STMT) f);
     L26 = (*dAM_ST775224435[TAG(L25)])(L25);
     L27 = ((dAM_STMT) f);
     (*dAM_ST1372701974[TAG(L27)])(L27, (*dAM_ST775224435[TAG(L26)])(L26));
    }
    else {
     L28 = ((dAM_STMT) f);
     (*dAM_ST1372701974[TAG(L28)])(L28, am);
    }
    return; break;
   default: ;
    FATAL("No applicable type in typecase\nin AM_CURSOR::replace_stmt($AM_STMT)\n./Back/cursor.sa:2028:17");
  }
 }
 SATTR(self,mark,FALSE);
 switch (TAG(f)) {
  case AM_IF_STMT_tag:
   L291_=(branch1)==(2); 
   if (L291_) {
    if ((am==((dAM_STMT) NULL))) {
     L30 = ATTR(((AM_IF_STMT) f),if_true);
     SATTR(((AM_IF_STMT) f),if_true,(*dAM_ST775224435[TAG(L30)])(L30));
     SATTR(self,cur,((dAM) ATTR(((AM_IF_STMT) f),if_true)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_IF_STMT) f),if_true,am);
    }
   }
   else {
    L311_=(branch1)==(3); 
    if (L311_) {
     if ((am==((dAM_STMT) NULL))) {
      L32 = ATTR(((AM_IF_STMT) f),if_false);
      SATTR(((AM_IF_STMT) f),if_false,(*dAM_ST775224435[TAG(L32)])(L32));
      SATTR(self,cur,((dAM) ATTR(((AM_IF_STMT) f),if_false)));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      SATTR(((AM_IF_STMT) f),if_false,am);
     }
    }
   } break;
  case AM_LOOP_STMT_tag:
   L331_=(branch1)==(1); 
   if (L331_) {
    if ((am==((dAM_STMT) NULL))) {
     L34 = ATTR(((AM_LOOP_STMT) f),init);
     SATTR(((AM_LOOP_STMT) f),init,(*dAM_ST775224435[TAG(L34)])(L34));
     SATTR(self,cur,((dAM) ATTR(((AM_LOOP_STMT) f),init)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_LOOP_STMT) f),init,am);
    }
   }
   else {
    L351_=(branch1)==(2); 
    if (L351_) {
     if ((am==((dAM_STMT) NULL))) {
      L36 = ATTR(((AM_LOOP_STMT) f),body);
      SATTR(((AM_LOOP_STMT) f),body,(*dAM_ST775224435[TAG(L36)])(L36));
      SATTR(self,cur,((dAM) ATTR(((AM_LOOP_STMT) f),body)));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      SATTR(((AM_LOOP_STMT) f),body,am);
     }
    }
   } break;
  case AM_CASE_STMT_tag:
   L37 = FLISTd355912233(ATTR(((AM_CASE_STMT) f),stmts));
   L381_=INTPLUS(L37,2); 
   L39 = L381_;
   L401_=(branch1)==(L39); 
   if (L401_) {
    if ((am==((dAM_STMT) NULL))) {
     L42 = ATTR(((AM_CASE_STMT) f),else_stmts);
     SATTR(((AM_CASE_STMT) f),else_stmts,(*dAM_ST775224435[TAG(L42)])(L42));
     SATTR(self,cur,((dAM) ATTR(((AM_CASE_STMT) f),else_stmts)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_CASE_STMT) f),else_stmts,am);
    }
   }
   else {
    if ((am==((dAM_STMT) NULL))) {
     aget_self = ATTR(((AM_CASE_STMT) f),stmts);
     L431_=INTMINUS(branch1,2); 
     aget_ind = L431_;
     L441_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self,aget_ind); 
     ret_val4 = L441_;
     L45 = ret_val4;
     aget_self1 = ATTR(((AM_CASE_STMT) f),stmts);
     L461_=INTMINUS(branch1,2); 
     aget_ind1 = L461_;
     L471_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
     ret_val5 = L471_;
     L48 = ret_val5;
     L49 = L45;
     (*dAM_ST1372701974[TAG(L49)])(L49, (*dAM_ST775224435[TAG(L48)])(L48));
     aget_self2 = ATTR(((AM_CASE_STMT) f),stmts);
     L501_=INTMINUS(branch1,2); 
     aget_ind2 = L501_;
     L511_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self2,aget_ind2); 
     ret_val6 = L511_;
     SATTR(self,cur,((dAM) ret_val6));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     aset_self = ATTR(((AM_CASE_STMT) f),stmts);
     L521_=INTMINUS(branch1,2); 
     aset_ind = L521_;
     aset_val = am;
     SARR((FLISTdAM_STMT)aset_self,aset_ind,(dAM_STMT)aset_val); 
     ;
    }
   } break;
  case AM_TYPECASE_STMT_tag:
   L54 = FLISTd355912233(ATTR(((AM_TYPECASE_STMT) f),stmts));
   L551_=INTPLUS(L54,2); 
   L56 = L551_;
   L571_=(branch1)==(L56); 
   if (L571_) {
    if ((am==((dAM_STMT) NULL))) {
     L58 = ATTR(((AM_TYPECASE_STMT) f),else_stmts);
     SATTR(((AM_TYPECASE_STMT) f),else_stmts,(*dAM_ST775224435[TAG(L58)])(L58));
     SATTR(self,cur,((dAM) ATTR(((AM_TYPECASE_STMT) f),else_stmts)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_TYPECASE_STMT) f),else_stmts,am);
    }
   }
   else {
    if ((am==((dAM_STMT) NULL))) {
     aget_self3 = ATTR(((AM_TYPECASE_STMT) f),stmts);
     L591_=INTMINUS(branch1,2); 
     aget_ind3 = L591_;
     L601_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self3,aget_ind3); 
     ret_val7 = L601_;
     L62 = ret_val7;
     aget_self4 = ATTR(((AM_TYPECASE_STMT) f),stmts);
     L631_=INTMINUS(branch1,2); 
     aget_ind4 = L631_;
     L641_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self4,aget_ind4); 
     ret_val8 = L641_;
     L65 = ret_val8;
     L66 = L62;
     (*dAM_ST1372701974[TAG(L66)])(L66, (*dAM_ST775224435[TAG(L65)])(L65));
     aget_self5 = ATTR(((AM_TYPECASE_STMT) f),stmts);
     L671_=INTMINUS(branch1,2); 
     aget_ind5 = L671_;
     L681_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self5,aget_ind5); 
     ret_val9 = L681_;
     SATTR(self,cur,((dAM) ret_val9));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     aset_self1 = ATTR(((AM_TYPECASE_STMT) f),stmts);
     L691_=INTMINUS(branch1,2); 
     aset_ind1 = L691_;
     aset_val1 = am;
     SARR((FLISTdAM_STMT)aset_self1,aset_ind1,(dAM_STMT)aset_val1); 
     ;
    }
   } break;
  case AM_PROTECT_STMT_tag:
   L711_=(branch1)==(1); 
   if (L711_) {
    if ((am==((dAM_STMT) NULL))) {
     L72 = ATTR(((AM_PROTECT_STMT) f),body);
     SATTR(((AM_PROTECT_STMT) f),body,(*dAM_ST775224435[TAG(L72)])(L72));
     SATTR(self,cur,((dAM) ATTR(((AM_PROTECT_STMT) f),body)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_PROTECT_STMT) f),body,am);
    }
   }
   else {
    L73 = FLISTd355912233(ATTR(((AM_PROTECT_STMT) f),stmts));
    L741_=INTPLUS(L73,2); 
    L75 = L741_;
    L761_=(branch1)==(L75); 
    if (L761_) {
     if ((am==((dAM_STMT) NULL))) {
      L77 = ATTR(((AM_PROTECT_STMT) f),else_stmts);
      SATTR(((AM_PROTECT_STMT) f),else_stmts,(*dAM_ST775224435[TAG(L77)])(L77));
      SATTR(self,cur,((dAM) ATTR(((AM_PROTECT_STMT) f),else_stmts)));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      SATTR(((AM_PROTECT_STMT) f),else_stmts,am);
     }
    }
    else {
     if ((am==((dAM_STMT) NULL))) {
      aget_self6 = ATTR(((AM_PROTECT_STMT) f),stmts);
      L781_=INTMINUS(branch1,2); 
      aget_ind6 = L781_;
      L791_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self6,aget_ind6); 
      ret_val10 = L791_;
      L80 = ret_val10;
      aget_self7 = ATTR(((AM_PROTECT_STMT) f),stmts);
      L811_=INTMINUS(branch1,2); 
      aget_ind7 = L811_;
      L821_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self7,aget_ind7); 
      ret_val11 = L821_;
      L83 = ret_val11;
      L84 = L80;
      (*dAM_ST1372701974[TAG(L84)])(L84, (*dAM_ST775224435[TAG(L83)])(L83));
      aget_self8 = ATTR(((AM_PROTECT_STMT) f),stmts);
      L851_=INTMINUS(branch1,2); 
      aget_ind8 = L851_;
      L861_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self8,aget_ind8); 
      ret_val12 = L861_;
      SATTR(self,cur,((dAM) ret_val12));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      aset_self2 = ATTR(((AM_PROTECT_STMT) f),stmts);
      L871_=INTMINUS(branch1,2); 
      aset_ind2 = L871_;
      aset_val2 = am;
      SARR((FLISTdAM_STMT)aset_self2,aset_ind2,(dAM_STMT)aset_val2); 
      ;
     }
    }
   } break;
  case AM_ITE1809135850_tag:
   L891_=(branch1)==(1); 
   if (L891_) {
    if ((am==((dAM_STMT) NULL))) {
     L90 = ATTR(((AM_ITE1809135850) f),init);
     SATTR(((AM_ITE1809135850) f),init,(*dAM_ST775224435[TAG(L90)])(L90));
     SATTR(self,cur,((dAM) ATTR(((AM_ITE1809135850) f),init)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_ITE1809135850) f),init,am);
    }
   } break;
  case AM_BND367211769_tag:
   L921_=(branch1)==(2); 
   if (L921_) {
    if ((am==((dAM_STMT) NULL))) {
     L93 = ATTR(((AM_BND367211769) f),init);
     SATTR(((AM_BND367211769) f),init,(*dAM_ST775224435[TAG(L93)])(L93));
     SATTR(self,cur,((dAM) ATTR(((AM_BND367211769) f),init)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_BND367211769) f),init,am);
    }
   } break;
  case AM_STMT_EXPR_tag:
   L941_=(branch1)==(1); 
   if (L941_) {
    if ((am==((dAM_STMT) NULL))) {
     L95 = ATTR(((AM_STMT_EXPR) f),stmts);
     SATTR(((AM_STMT_EXPR) f),stmts,(*dAM_ST775224435[TAG(L95)])(L95));
    }
    else {
     SATTR(((AM_STMT_EXPR) f),stmts,am);
    }
   } break;
  case AM_LOCK_STMT_tag:
   b1 = branch1;
   L96 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) f),guards));
   L971_=INTMINUS(b1,L96); 
   L98 = L971_;
   L991_=INTMINUS(L98,1); 
   b1 = L991_;
   {
    struct FLISTA1857087356_frame_struct other2_0;
FLISTA1857087356_frame noname1 = &other2_0;
    L11 = ATTR(((AM_LOCK_STMT) f),locks1);
    noname1->self = L11;
    noname1->state = 0;
    while (1) {
     L100 = FLISTA1857087356(noname1);
     if (noname1->state == -1) {
      goto after_loop1;
     }
     l = L100;
     L1011_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
     L102 = L1011_;
     L1031_=INTMINUS(b1,L102); 
     b1 = L1031_;
    }
   }
   after_loop1: ;
   L104 = FLISTd355912233(ATTR(((AM_LOCK_STMT) f),stmts));
   L1051_=(b1)<(L104); 
   if (L1051_) {
    aget_self9 = ATTR(((AM_LOCK_STMT) f),stmts);
    aget_ind9 = b1;
    L1061_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self9,aget_ind9); 
    ret_val13 = L1061_;
    L107 = (ret_val13==((dAM_STMT) NULL));
    L1081_=!(L107); 
    if (L1081_) {
     if ((am==((dAM_STMT) NULL))) {
      aset_self3 = ATTR(((AM_LOCK_STMT) f),stmts);
      aset_ind3 = b1;
      aget_self10 = ATTR(((AM_LOCK_STMT) f),stmts);
      aget_ind10 = b1;
      L1091_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self10,aget_ind10); 
      ret_val14 = L1091_;
      L110 = ret_val14;
      aset_val3 = (*dAM_ST775224435[TAG(L110)])(L110);
      SARR((FLISTdAM_STMT)aset_self3,aset_ind3,(dAM_STMT)aset_val3); 
      ;
      aget_self11 = ATTR(((AM_LOCK_STMT) f),stmts);
      aget_ind11 = b1;
      L1121_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self11,aget_ind11); 
      ret_val15 = L1121_;
      SATTR(self,cur,((dAM) ret_val15));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      aset_self4 = ATTR(((AM_LOCK_STMT) f),stmts);
      aset_ind4 = b1;
      aset_val4 = am;
      SARR((FLISTdAM_STMT)aset_self4,aset_ind4,(dAM_STMT)aset_val4); 
      ;
     }
    }
   }
   L114 = FLISTd355912233(ATTR(((AM_LOCK_STMT) f),stmts));
   L1151_=INTMINUS(b1,L114); 
   b1 = L1151_;
   L1171_=(b1)==(0); 
   if (L1171_) {
    L118 = (ATTR(((AM_LOCK_STMT) f),else_stmts)==((dAM_STMT) NULL));
    L1191_=!(L118); 
    L116 = L1191_;
   } else {
    L116 = FALSE;
   }
   if (L116) {
    if ((am==((dAM_STMT) NULL))) {
     L120 = ATTR(((AM_LOCK_STMT) f),else_stmts);
     SATTR(((AM_LOCK_STMT) f),else_stmts,(*dAM_ST775224435[TAG(L120)])(L120));
     SATTR(self,cur,((dAM) ATTR(((AM_LOCK_STMT) f),else_stmts)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_LOCK_STMT) f),else_stmts,am);
    }
   } break;
  case AM_WIT1996971603_tag:
   L121 = ATTR(((AM_WIT1996971603) f),objects1);
   L1221_=(L121)==NULL?0:ASIZE((ARRAYdAM_EXPR)L121); 
   L123 = L1221_;
   L1241_=INTPLUS(L123,1); 
   L125 = L1241_;
   L1261_=(branch1)==(L125); 
   if (L1261_) {
    if ((am==((dAM_STMT) NULL))) {
     L127 = ATTR(((AM_WIT1996971603) f),near_part);
     SATTR(((AM_WIT1996971603) f),near_part,(*dAM_ST775224435[TAG(L127)])(L127));
     SATTR(self,cur,((dAM) ATTR(((AM_WIT1996971603) f),near_part)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_WIT1996971603) f),near_part,am);
    }
   }
   else {
    L128 = ATTR(((AM_WIT1996971603) f),objects1);
    L1291_=(L128)==NULL?0:ASIZE((ARRAYdAM_EXPR)L128); 
    L130 = L1291_;
    L1311_=INTPLUS(L130,2); 
    L132 = L1311_;
    L1331_=(branch1)==(L132); 
    if (L1331_) {
     if ((am==((dAM_STMT) NULL))) {
      L134 = ATTR(((AM_WIT1996971603) f),else_part);
      SATTR(((AM_WIT1996971603) f),else_part,(*dAM_ST775224435[TAG(L134)])(L134));
      SATTR(self,cur,((dAM) ATTR(((AM_WIT1996971603) f),else_part)));
      SATTR(self,ignore_next,TRUE);
     }
     else {
      SATTR(((AM_WIT1996971603) f),else_part,am);
     }
    }
   } break;
  case AM_FORK_STMT_tag:
   L1351_=(branch1)==(2); 
   if (L1351_) {
    if ((am==((dAM_STMT) NULL))) {
     L136 = ATTR(((AM_FORK_STMT) f),body);
     SATTR(((AM_FORK_STMT) f),body,(*dAM_ST775224435[TAG(L136)])(L136));
     SATTR(self,cur,((dAM) ATTR(((AM_FORK_STMT) f),body)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_FORK_STMT) f),body,am);
    }
   } break;
  case AM_PAR_STMT_tag:
   L1371_=(branch1)==(1); 
   if (L1371_) {
    if ((am==((dAM_STMT) NULL))) {
     L138 = ATTR(((AM_PAR_STMT) f),body);
     SATTR(((AM_PAR_STMT) f),body,(*dAM_ST775224435[TAG(L138)])(L138));
     SATTR(self,cur,((dAM) ATTR(((AM_PAR_STMT) f),body)));
     SATTR(self,ignore_next,TRUE);
    }
    else {
     SATTR(((AM_PAR_STMT) f),body,am);
    }
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_CURSOR::replace_stmt($AM_STMT)\n./Back/cursor.sa:2039:14");
 }
 if ((ATTR(self,cur)==((dAM) NULL))) {
  create_self1 = ((AM_COMMENT_STMT) NULL);
  create_source = SFILE_ID_zero;
  L140=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
  if (L140==NULL) FATAL("Unable to allocate more memory");
  ((OB)L140)->header.tag=AM_COMMENT_STMT_tag;
  L139 = ((AM_COMMENT_STMT) L140);
  r1 = L139;
  SATTR(r1,source1,create_source);
  ret_val16 = r1;
  SATTR(self,cur,((dAM) ret_val16));
  SATTR(self,ignore_next,FALSE);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR352782528(AM_CURSOR self, dAM_STMT am) {
 AM_LOOP_STMT lp;
 AM_LOOP_STMT L1;
 AM_LOOP_STMT L2;
 dAM_STMT L3;
 lp = AM_CUR153953493(self);
 L1=AM_CUR153953493(self);
 if ((ATTR(L1,init)==((dAM_STMT) NULL))) {
  SATTR(AM_CUR153953493(self),init,am);
  return;
 }
 else {
  L2=AM_CUR153953493(self);
  L3 = ATTR(L2,init);
  (*dAM_ST2020072576[TAG(L3)])(L3, am);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR897865267(AM_CURSOR self) {
 dAM tcur;
 AM_CURSOR_POS create_self;
 dAM create_curr_stmt;
 BOOL create_m = BOOL_zero;
 INT create_branch = INT_zero;
 AM_CURSOR_POS ret_val;
 AM_CURSOR_POS r;
 A_STAC1244966098 push_self;
 AM_CURSOR_POS push_e;
 AM_COMMENT_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val1;
 AM_COMMENT_STMT r1;
 OUT create_self2;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 AM_CURSOR_POS L1;
 OB L2;
 INT L3;
 INT L41_;
 AM_COMMENT_STMT L5;
 OB L6;
 extern STR Intern419679144;
 FILE1 L7;
 OB L8;
 tcur = ATTR(self,cur);
 if (tcur==NULL) {
  goto other276;
 } else
 switch (TAG(tcur)) {
  case AM_LOOP_STMT_tag:
   if ((ATTR(((AM_LOOP_STMT) tcur),init)==((dAM_STMT) NULL))) {
    return;
   }
   push_self = ATTR(self,stack1);
   create_self = ((AM_CURSOR_POS) NULL);
   create_curr_stmt = ATTR(self,cur);
   create_m = ATTR(self,mark);
   create_branch = 1;
   L2=ZALLOC(sizeof(struct AM_CURSOR_POS_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=AM_CURSOR_POS_tag;
   L1 = ((AM_CURSOR_POS) L2);
   r = L1;
   SATTR(r,stmt,create_curr_stmt);
   SATTR(r,branch,create_branch);
   SATTR(r,mark,create_m);
   ret_val = r;
   push_e = ret_val;
   SATTR(push_self,s,FLISTA1475379054(ATTR(push_self,s), push_e));
   L3 = ATTR(self,indent);
   L41_=INTPLUS(L3,1); 
   SATTR(self,indent,L41_);
   create_self1 = ((AM_COMMENT_STMT) NULL);
   create_source = SFILE_ID_zero;
   L6=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AM_COMMENT_STMT_tag;
   L5 = ((AM_COMMENT_STMT) L6);
   r1 = L5;
   SATTR(r1,source1,create_source);
   ret_val1 = r1;
   SATTR(self,cur,((dAM) ret_val1));
   SATTR(self,mark,FALSE); break;
  default: ;
  other276: ;
   create_self2 = ((OUT) NULL);
   ret_val2 = create_self2;
   plus_self = ret_val2;
   plus_s = ((STR) &Intern419679144);
   stdout_self = ((FILE1) NULL);
   L8=ZALLOC(sizeof(struct FILE1_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=FILE1_tag;
   L7 = ((FILE1) L8);
   r2 = L7;
   
   SATTR(r2,fp,stdout);
   ret_val3 = r2;
   FILE_plus_STR(ret_val3, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR915996644(AM_CURSOR self, dAM_EXPR w) {
 dAM am;
 INT branch1 = INT_zero;
 INT b1 = INT_zero;
 FLISTA1327600838 L11;
 ARRAYdAM_EXPR l;
 dTP t = ((dTP) NULL);
 A_STAC1244966098 size_self;
 INT ret_val = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val1;
 A_STAC1244966098 top_self1;
 AM_CURSOR_POS ret_val2;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 OUT ret_val4;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val6;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 FLISTdAM_EXPR aset_self;
 INT aset_ind = INT_zero;
 dAM_EXPR aset_val;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r3;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 AM_EXCEPT_EXPR create_self2;
 dTP create_tp;
 AM_EXCEPT_EXPR ret_val11;
 AM_EXCEPT_EXPR r4;
 BOOL L2;
 INT L3;
 BOOL L41_;
 AM_CURSOR_POS L5;
 AM_CURSOR_POS L6;
 BOOL L71_;
 BOOL L81_;
 BOOL L91_;
 BOOL L101_;
 BOOL L121_;
 BOOL L131_;
 BOOL L141_;
 BOOL L151_;
 BOOL L161_;
 BOOL L171_;
 BOOL L181_;
 BOOL L191_;
 BOOL L201_;
 extern STR intern948720431;
 FILE1 L21;
 OB L22;
 INT L241_;
 INT L25;
 STR L261_;
 FILE1 L27;
 OB L28;
 extern STR name9;
 FILE1 L30;
 OB L31;
 BOOL L331_;
 BOOL L341_;
 BOOL L351_;
 BOOL L361_;
 BOOL L371_;
 INT L38;
 BOOL L391_;
 BOOL L40;
 BOOL L421_;
 INT L431_;
 INT L45;
 INT L461_;
 INT L47;
 INT L481_;
 ARRAYdAM_EXPR L49;
 INT L501_;
 INT L51;
 BOOL L521_;
 INT L541_;
 INT L55;
 INT L561_;
 BOOL L571_;
 BOOL L581_;
 BOOL L591_;
 ARRAYdAM_EXPR L60;
 INT L611_;
 INT L62;
 BOOL L631_;
 BOOL L64;
 BOOL L651_;
 ARRAYdAM_EXPR L66;
 INT L671_;
 INT L68;
 BOOL L701_;
 INT L721_;
 INT L73;
 AM_CALL_ARG L741_;
 INT L751_;
 INT L76;
 AM_CALL_ARG L771_;
 INT L781_;
 INT L79;
 AM_CALL_ARG L801_;
 BOOL L821_;
 INT L831_;
 INT L84;
 AM_CALL_ARG L851_;
 BOOL L861_;
 INT L871_;
 INT L88;
 AM_CALL_ARG L891_;
 INT L901_;
 INT L92;
 INT L941_;
 INT L95;
 AM_CALL_ARG L961_;
 BOOL L971_;
 BOOL L981_;
 BOOL L991_;
 BOOL L1001_;
 BOOL L1021_;
 BOOL L1031_;
 BOOL L1041_;
 BOOL L1051_;
 BOOL L1061_;
 BOOL L1071_;
 BOOL L1081_;
 BOOL L1091_;
 extern STR Intern1442877807;
 FILE1 L110;
 OB L111;
 BOOL L1131_;
 BOOL L1141_;
 BOOL L1151_;
 BOOL L1161_;
 BOOL L1171_;
 BOOL L1181_;
 AM_EXCEPT_EXPR L119;
 OB L120;
 if ((ATTR(self,stack1)==((A_STAC1244966098) NULL))) {
  L2 = TRUE;
 } else {
  size_self = ATTR(self,stack1);
  ret_val = FLISTA587993904(ATTR(size_self,s));
  L3 = ret_val;
  L41_=(L3)==(0); 
  L2 = L41_;
 }
 if (L2) {
  SATTR(self,top1,((dAM) w));
  SATTR(self,cur,((dAM) w));
  return;
 }
 top_self = ATTR(self,stack1);
 ret_val1 = FLISTA860779230(ATTR(top_self,s));
 L5=ret_val1;
 am = ATTR(L5,stmt);
 top_self1 = ATTR(self,stack1);
 ret_val2 = FLISTA860779230(ATTR(top_self1,s));
 L6=ret_val2;
 branch1 = ATTR(L6,branch);
 SATTR(self,cur,((dAM) w));
 SATTR(self,mark,FALSE);
 switch (TAG(am)) {
  case AM_WAITFOR_STMT_tag:
   L71_=(branch1)==(1); 
   if (L71_) {
    SATTR(((AM_WAITFOR_STMT) am),dest,w);
   }
   L81_=(branch1)==(2); 
   if (L81_) {
    SATTR(((AM_WAITFOR_STMT) am),src1,w);
   }
   L91_=(branch1)==(3); 
   if (L91_) {
    switch (TAG(w)) {
     case AM_LOCAL_EXPR_tag:
      SATTR(((AM_WAITFOR_STMT) am),prefetch,((AM_LOCAL_EXPR) w)); break;
     default: ;
      FATAL("No applicable type in typecase\nin AM_CURSOR::replace_expr($AM_EXPR)\n./Back/cursor.sa:2155:20");
    }
   } break;
  case AM_PREFETCH_STMT_tag:
   L101_=(branch1)==(1); 
   if (L101_) {
    SATTR(((AM_PREFETCH_STMT) am),dest,w);
   }
   L121_=(branch1)==(2); 
   if (L121_) {
    SATTR(((AM_PREFETCH_STMT) am),src1,w);
   }
   L131_=(branch1)==(3); 
   if (L131_) {
    switch (TAG(w)) {
     case AM_LOCAL_EXPR_tag:
      SATTR(((AM_PREFETCH_STMT) am),prefetch,((AM_LOCAL_EXPR) w)); break;
     default: ;
      FATAL("No applicable type in typecase\nin AM_CURSOR::replace_expr($AM_EXPR)\n./Back/cursor.sa:2161:20");
    }
   } break;
  case AM_ASSIGN_STMT_tag:
   L141_=(branch1)==(1); 
   if (L141_) {
    SATTR(((AM_ASSIGN_STMT) am),dest,w);
   }
   L151_=(branch1)==(2); 
   if (L151_) {
    SATTR(((AM_ASSIGN_STMT) am),src1,w);
   } break;
  case AM_IF_STMT_tag:
   L161_=(branch1)==(1); 
   if (L161_) {
    SATTR(((AM_IF_STMT) am),test1,w);
   } break;
  case AM_RETURN_STMT_tag:
   L171_=(branch1)==(1); 
   if (L171_) {
    SATTR(((AM_RETURN_STMT) am),val1,w);
   } break;
  case AM_YIELD_STMT_tag:
   L181_=(branch1)==(1); 
   if (L181_) {
    SATTR(((AM_YIELD_STMT) am),val1,w);
   } break;
  case AM_CASE_STMT_tag:
   L191_=(branch1)==(1); 
   if (L191_) {
    SATTR(((AM_CASE_STMT) am),test1,w);
   } break;
  case AM_TYPECASE_STMT_tag:
   L201_=(branch1)==(1); 
   if (L201_) {
    if (w==NULL) {
     goto other262;
    } else
    switch (TAG(w)) {
     case AM_LOCAL_EXPR_tag:
      SATTR(((AM_TYPECASE_STMT) am),test1,((AM_LOCAL_EXPR) w)); break;
     default: ;
     other262: ;
      create_self = ((OUT) NULL);
      ret_val3 = create_self;
      plus_self = ret_val3;
      plus_s = ((STR) &intern948720431);
      stdout_self = ((FILE1) NULL);
      L22=ZALLOC(sizeof(struct FILE1_struct));
      if (L22==NULL) FATAL("Unable to allocate more memory");
      ((OB)L22)->header.tag=FILE1_tag;
      L21 = ((FILE1) L22);
      r = L21;
      
      SATTR(r,fp,stdout);
      ret_val5 = r;
      FILE_plus_STR(ret_val5, plus_s);
      ret_val4 = plus_self;
      plus_self1 = ret_val4;
      L241_=SYSTP(w); 
      L25 = L241_;
      L261_=SYSSTRFORTP(L25); 
      plus_s1 = L261_;
      stdout_self1 = ((FILE1) NULL);
      L28=ZALLOC(sizeof(struct FILE1_struct));
      if (L28==NULL) FATAL("Unable to allocate more memory");
      ((OB)L28)->header.tag=FILE1_tag;
      L27 = ((FILE1) L28);
      r1 = L27;
      
      SATTR(r1,fp,stdout);
      ret_val7 = r1;
      FILE_plus_STR(ret_val7, plus_s1);
      ret_val6 = plus_self1;
      plus_self2 = ret_val6;
      plus_s2 = ((STR) &name9);
      stdout_self2 = ((FILE1) NULL);
      L31=ZALLOC(sizeof(struct FILE1_struct));
      if (L31==NULL) FATAL("Unable to allocate more memory");
      ((OB)L31)->header.tag=FILE1_tag;
      L30 = ((FILE1) L31);
      r2 = L30;
      
      SATTR(r2,fp,stdout);
      ret_val8 = r2;
      FILE_plus_STR(ret_val8, plus_s2);
      exit_self = ((UNIX) NULL);
      exit_code = 1;
      exit(exit_code);
    }
   } break;
  case AM_PRE_STMT_tag:
   L331_=(branch1)==(1); 
   if (L331_) {
    SATTR(((AM_PRE_STMT) am),test1,w);
   } break;
  case AM_POST_STMT_tag:
   L341_=(branch1)==(1); 
   if (L341_) {
    SATTR(((AM_POST_STMT) am),test1,w);
   } break;
  case AM_ASSERT_STMT_tag:
   L351_=(branch1)==(1); 
   if (L351_) {
    SATTR(((AM_ASSERT_STMT) am),test1,w);
   } break;
  case AM_RAISE_STMT_tag:
   L361_=(branch1)==(1); 
   if (L361_) {
    SATTR(((AM_RAISE_STMT) am),val1,w);
   } break;
  case AM_EXPR_STMT_tag:
   L371_=(branch1)==(1); 
   if (L371_) {
    SATTR(((AM_EXPR_STMT) am),expr,w);
   } break;
  case AM_LOCK_STMT_tag:
   b1 = branch1;
   L38 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) am),guards));
   L391_=(L38)<(branch1); 
   L40 = L391_;
   L421_=!(L40); 
   if (L421_) {
    aset_self = ATTR(((AM_LOCK_STMT) am),guards);
    L431_=INTMINUS(branch1,1); 
    aset_ind = L431_;
    aset_val = w;
    SARR((FLISTdAM_EXPR)aset_self,aset_ind,(dAM_EXPR)aset_val); 
    ;
   }
   else {
    L45 = FLISTd1406591968(ATTR(((AM_LOCK_STMT) am),guards));
    L461_=INTMINUS(b1,L45); 
    L47 = L461_;
    L481_=INTMINUS(L47,1); 
    b1 = L481_;
    {
     struct FLISTA1857087356_frame_struct other1_0;
FLISTA1857087356_frame noname1 = &other1_0;
     L11 = ATTR(((AM_LOCK_STMT) am),locks1);
     noname1->self = L11;
     noname1->state = 0;
     while (1) {
      L49 = FLISTA1857087356(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      l = L49;
      L501_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
      L51 = L501_;
      L521_=(b1)<(L51); 
      if (L521_) {
       SARR((ARRAYdAM_EXPR)l,b1,(dAM_EXPR)w); 
       ;
       goto after_loop;
      }
      L541_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
      L55 = L541_;
      L561_=INTMINUS(b1,L55); 
      b1 = L561_;
     }
    }
    after_loop: ;
   } break;
  case AM_ATTACH_STMT_tag:
   L571_=(branch1)==(1); 
   if (L571_) {
    SATTR(((AM_ATTACH_STMT) am),at,w);
   } break;
  case AM_FORK_STMT_tag:
   L581_=(branch1)==(1); 
   if (L581_) {
    SATTR(((AM_FORK_STMT) am),at,w);
   } break;
  case AM_UNLOCK_STMT_tag:
   L591_=(branch1)==(1); 
   if (L591_) {
    SATTR(((AM_UNLOCK_STMT) am),lock_ob,w);
   } break;
  case AM_WIT1996971603_tag:
   L60 = ATTR(((AM_WIT1996971603) am),objects1);
   L611_=(L60)==NULL?0:ASIZE((ARRAYdAM_EXPR)L60); 
   L62 = L611_;
   L631_=(L62)<(branch1); 
   L64 = L631_;
   L651_=!(L64); 
   if (L651_) {
    L66 = ATTR(((AM_WIT1996971603) am),objects1);
    L671_=INTMINUS(branch1,1); 
    L68 = L671_;
    SARR((ARRAYdAM_EXPR)L66,L68,(dAM_EXPR)w); 
    ;
   } break;
  case AM_CALL_ARG_tag:
   L701_=(branch1)==(1); 
   if (L701_) {
    SATTR(((AM_CALL_ARG) am),expr,w);
   } break;
  case AM_ROU1916046290_tag:
   L721_=INTMINUS(branch1,1); 
   L73 = L721_;
   L741_=(AM_CALL_ARG)ARR((AM_ROU1916046290)am,L73); 
   SATTR(L741_,expr,w); break;
  case AM_EXT_CALL_EXPR_tag:
   L751_=INTMINUS(branch1,1); 
   L76 = L751_;
   L771_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)am,L76); 
   SATTR(L771_,expr,w); break;
  case AM_ITE1809135850_tag:
   L781_=INTMINUS(branch1,2); 
   L79 = L781_;
   L801_=(AM_CALL_ARG)ARR((AM_ITE1809135850)am,L79); 
   SATTR(L801_,expr,w); break;
  case AM_BND260301329_tag:
   L821_=(branch1)==(1); 
   if (L821_) {
    SATTR(((AM_BND260301329) am),br,w);
   }
   else {
    L831_=INTMINUS(branch1,2); 
    L84 = L831_;
    L851_=(AM_CALL_ARG)ARR((AM_BND260301329)am,L84); 
    SATTR(L851_,expr,w);
   } break;
  case AM_BND367211769_tag:
   L861_=(branch1)==(1); 
   if (L861_) {
    SATTR(((AM_BND367211769) am),bi,w);
   }
   else {
    L871_=INTMINUS(branch1,3); 
    L88 = L871_;
    L891_=(AM_CALL_ARG)ARR((AM_BND367211769)am,L88); 
    SATTR(L891_,expr,w);
   } break;
  case AM_ARRAY_EXPR_tag:
   L901_=INTMINUS(branch1,1); 
   L92 = L901_;
   SARR((AM_ARRAY_EXPR)am,L92,(dAM_EXPR)w); 
   ; break;
  case AM_BND1124877163_tag:
   L941_=INTMINUS(branch1,1); 
   L95 = L941_;
   L961_=(AM_CALL_ARG)ARR((AM_BND1124877163)am,L95); 
   SATTR(L961_,expr,w); break;
  case AM_IF_EXPR_tag:
   L971_=(branch1)==(1); 
   if (L971_) {
    SATTR(((AM_IF_EXPR) am),test1,w);
   }
   L981_=(branch1)==(2); 
   if (L981_) {
    SATTR(((AM_IF_EXPR) am),if_true,w);
   }
   L991_=(branch1)==(3); 
   if (L991_) {
    SATTR(((AM_IF_EXPR) am),if_false,w);
   } break;
  case AM_NEW_EXPR_tag:
   L1001_=(branch1)==(1); 
   if (L1001_) {
    SATTR(((AM_NEW_EXPR) am),asz,w);
   } break;
  case AM_IS_VOID_EXPR_tag:
   L1021_=(branch1)==(1); 
   if (L1021_) {
    SATTR(((AM_IS_VOID_EXPR) am),arg,w);
   } break;
  case AM_SHARED_EXPR_tag: break;
  case AM_ATTR_EXPR_tag:
   L1031_=(branch1)==(1); 
   if (L1031_) {
    SATTR(((AM_ATTR_EXPR) am),ob,w);
   } break;
  case AM_VAT319982528_tag:
   L1041_=(branch1)==(1); 
   if (L1041_) {
    SATTR(((AM_VAT319982528) am),ob,w);
   }
   L1051_=(branch1)==(2); 
   if (L1051_) {
    SATTR(((AM_VAT319982528) am),val1,w);
   } break;
  case AM_VAR744470097_tag:
   L1061_=(branch1)==(1); 
   if (L1061_) {
    SATTR(((AM_VAR744470097) am),ob,w);
   }
   L1071_=(branch1)==(2); 
   if (L1071_) {
    SATTR(((AM_VAR744470097) am),ind,w);
   }
   L1081_=(branch1)==(3); 
   if (L1081_) {
    SATTR(((AM_VAR744470097) am),val1,w);
   } break;
  case AM_STMT_EXPR_tag:
   L1091_=(branch1)==(1); 
   if (L1091_) {
    create_self1 = ((OUT) NULL);
    ret_val9 = create_self1;
    plus_self3 = ret_val9;
    plus_s3 = ((STR) &Intern1442877807);
    stdout_self3 = ((FILE1) NULL);
    L111=ZALLOC(sizeof(struct FILE1_struct));
    if (L111==NULL) FATAL("Unable to allocate more memory");
    ((OB)L111)->header.tag=FILE1_tag;
    L110 = ((FILE1) L111);
    r3 = L110;
    
    SATTR(r3,fp,stdout);
    ret_val10 = r3;
    FILE_plus_STR(ret_val10, plus_s3);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = 1;
    exit(exit_code1);
   }
   L1131_=(branch1)==(2); 
   if (L1131_) {
    SATTR(((AM_STMT_EXPR) am),expr,w);
   } break;
  case AM_WHERE_EXPR_tag:
   L1141_=(branch1)==(1); 
   if (L1141_) {
    SATTR(((AM_WHERE_EXPR) am),arg,w);
   } break;
  case AM_FAR_EXPR_tag:
   L1151_=(branch1)==(1); 
   if (L1151_) {
    SATTR(((AM_FAR_EXPR) am),arg,w);
   } break;
  case AM_NEAR_EXPR_tag:
   L1161_=(branch1)==(1); 
   if (L1161_) {
    SATTR(((AM_NEAR_EXPR) am),arg,w);
   } break;
  case AM_AT_EXPR_tag:
   L1171_=(branch1)==(1); 
   if (L1171_) {
    SATTR(((AM_AT_EXPR) am),at,w);
   }
   L1181_=(branch1)==(2); 
   if (L1181_) {
    SATTR(((AM_AT_EXPR) am),e,w);
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_CURSOR::replace_expr($AM_EXPR)\n./Back/cursor.sa:2150:14");
 }
 if ((ATTR(self,cur)==((dAM) NULL))) {
  create_self2 = ((AM_EXCEPT_EXPR) NULL);
  create_tp = t;
  L120=ZALLOC(sizeof(struct AM_EXCEPT_EXPR_struct));
  if (L120==NULL) FATAL("Unable to allocate more memory");
  ((OB)L120)->header.tag=AM_EXCEPT_EXPR_tag;
  L119 = ((AM_EXCEPT_EXPR) L120);
  r4 = L119;
  SATTR(r4,tp_at,create_tp);
  ret_val11 = r4;
  SATTR(self,cur,((dAM) ret_val11));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_CUR997725387(AM_CURSOR self) {
 AM_CURSOR_POS a;
 dAM s;
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
 A_STAC1244966098 size_self;
 INT ret_val4 = INT_zero;
 A_STAC1244966098 top_self;
 AM_CURSOR_POS ret_val5;
 AM_CURSOR pop_stack_self;
 AM_CURSOR_POS d;
 A_STAC1244966098 size_self1;
 INT ret_val6 = INT_zero;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val8;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val10;
 FILE1 stdout_self3;
 FILE1 ret_val11;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val12;
 FILE1 r4;
 extern STR before837382382;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 INT L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 extern STR aftere1148777451;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 extern STR name61;
 FILE1 L19;
 OB L20;
 if (AM_CURSOR_debug) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &before837382382);
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
 if (ATTR(self,mark)) {
  return;
 }
 while (1) {
  size_self = ATTR(self,stack1);
  ret_val4 = FLISTA587993904(ATTR(size_self,s));
  L7 = ret_val4;
  L81_=(0)<(L7); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  top_self = ATTR(self,stack1);
  ret_val5 = FLISTA860779230(ATTR(top_self,s));
  a = ret_val5;
  s = ATTR(a,stmt);
  if (s==NULL) {
   goto other323;
  } else
  switch (TAG(s)) {
   case AM_LOOP_STMT_tag:
    goto after_loop; break;
   default: ;
   other323: ;
  }
  if (ATTR(a,mark)) {
   goto after_loop;
  }
  pop_stack_self = self;
  d = AM_CUR61708328(pop_stack_self);
  SATTR(self,mark,FALSE);
 }
 after_loop: ;
 size_self1 = ATTR(self,stack1);
 ret_val6 = FLISTA587993904(ATTR(size_self1,s));
 L9 = ret_val6;
 L101_=(L9)==(0); 
 if (L101_) {
  AM_CUR1869307717(self);
 }
 else {
  AM_CUR1389299115(self);
 }
 L11 = (ATTR(self,cur)==((dAM) NULL));
 L121_=!(L11); 
 SATTR(self,ignore_next,L121_);
 if (AM_CURSOR_debug) {
  create_self1 = ((OUT) NULL);
  ret_val7 = create_self1;
  plus_self2 = ret_val7;
  plus_s2 = ((STR) &aftere1148777451);
  stdout_self2 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r2 = L13;
  
  SATTR(r2,fp,stdout);
  ret_val9 = r2;
  FILE_plus_STR(ret_val9, plus_s2);
  ret_val8 = plus_self2;
  plus_self3 = ret_val8;
  plus_s3 = AM_CUR720390882(self);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val11 = r3;
  FILE_plus_STR(ret_val11, plus_s3);
  ret_val10 = plus_self3;
  plus_self4 = ret_val10;
  plus_s4 = ((STR) &name61);
  stdout_self4 = ((FILE1) NULL);
  L20=ZALLOC(sizeof(struct FILE1_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=FILE1_tag;
  L19 = ((FILE1) L20);
  r4 = L19;
  
  SATTR(r4,fp,stdout);
  ret_val12 = r4;
  FILE_plus_STR(ret_val12, plus_s4);
 }
}
