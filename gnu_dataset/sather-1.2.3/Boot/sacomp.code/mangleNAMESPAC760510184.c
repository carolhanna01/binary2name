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

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dAS_STMT_struct {
 OB_HEADER header;
 } *dAS_STMT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct CSE_DOUBLE_EXPRS_struct {/* layout for CSE_DOUBLE_EXPRS */
 OB_HEADER header;
 struct FLISTdAM_EXPR_struct *expr;
 struct FLISTAM_CURSOR_struct *pos;
 struct FLISTA725283029_struct *locals1;
 } *CSE_DOUBLE_EXPRS;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

typedef struct MODES_struct {/* layout for MODES */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODES;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

typedef struct OPT_CSE_struct {/* layout for OPT_CSE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_CSE;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

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

typedef struct TUPAM_927437481_struct {/* layout for TUP{AM_CURSOR,$AM_EXPR,$AM_STMT} */
 struct dAM_EXPR_struct *t2;
 struct dAM_STMT_struct *t3;
 struct AM_CURSOR_struct *t1;
 } TUPAM_927437481;
static TUPAM_927437481 TUPAM_927437481_zero;

typedef struct TUPAM_927437481_boxed_struct {
 OB_HEADER header;
 TUPAM_927437481 immutable_part;
 } *TUPAM_927437481_boxed;

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

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

typedef struct AS_AND_EXPR_struct {/* layout for AS_AND_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AND_EXPR;

typedef struct AS_ANY_EXPR_struct {/* layout for AS_ANY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ANY_EXPR;

typedef struct AS_ARG_DEC_struct {/* layout for AS_ARG_DEC */
 OB_HEADER header;
 struct AS_ARG_DEC_struct *next;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_ARG_DEC;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

typedef struct AS_ARRAY_EXPR_struct {/* layout for AS_ARRAY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ARRAY_EXPR;

typedef struct AS_ASSERT_STMT_struct {/* layout for AS_ASSERT_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_ASSERT_STMT;

typedef struct AS_ASSIGN_STMT_struct {/* layout for AS_ASSIGN_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *lhs_expr;
 struct dAS_EXPR_struct *rhs;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_ASSIGN_STMT;

typedef struct AS_ATTACH_STMT_struct {/* layout for AS_ATTACH_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *lhs;
 struct dAS_EXPR_struct *rhs;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL dont_print;
 BOOL transformed;
 } *AS_ATTACH_STMT;

typedef struct AS_ATTR_DEF_struct {/* layout for AS_ATTR_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_ATTR_DEF;

typedef struct AS_AT_EXPR_struct {/* layout for AS_AT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AT_EXPR;

typedef struct AS_BOOL_LIT_EXPR_struct {/* layout for AS_BOOL_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AS_BOOL_LIT_EXPR;

typedef struct AS_BOU14269336_struct {/* layout for AS_BOUND_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct AS_CALL_EXPR_struct *call1;
 struct AS_TYPE_SPEC_struct *ret;
 struct SFILE_ID_struct source1;
 BOOL is_f_rout;
 BOOL is_iter;
 } *AS_BOU14269336;

typedef struct AS_BREAK_EXPR_struct {/* layout for AS_BREAK_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_BREAK_EXPR;

typedef struct AS_CALL_EXPR_struct {/* layout for AS_CALL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *args;
 struct dAS_EXPR_struct *next;
 struct dAS_EXPR_struct *ob;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL flip;
 BOOL is_array;
 } *AS_CALL_EXPR;

typedef struct AS_CASE_STMT_struct {/* layout for AS_CASE_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_CASE_WHEN_struct *when_part;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_CASE_STMT;

typedef struct AS_CASE_WHEN_struct {/* layout for AS_CASE_WHEN */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct AS_CASE_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 } *AS_CASE_WHEN;

typedef struct AS_CHAR_LIT_EXPR_struct {/* layout for AS_CHAR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT val1;
 struct SFILE_ID_struct source1;
 } *AS_CHAR_LIT_EXPR;

typedef struct AS_CLASS_DEF_struct {/* layout for AS_CLASS_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *body;
 struct AS_CLASS_DEF_struct *next;
 struct AS_PARAM_DEC_struct *params;
 struct AS_TYPE_SPEC_struct *over;
 struct AS_TYPE_SPEC_struct *under;
 struct IDENT_struct name1;
 INT kind;
 struct SFILE_ID_struct source1;
 } *AS_CLASS_DEF;

typedef struct AS_CLASS_ELT_struct {/* layout for AS_CLASS_ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_CLASS_ELT;

typedef struct AS_CLUSTER_EXPR_struct {/* layout for AS_CLUSTER_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLUSTER_EXPR;

typedef struct AS_CLU553570599_struct {/* layout for AS_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLU553570599;

typedef struct AS_COHORT_EXPR_struct {/* layout for AS_COHORT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct IDENT_struct name_after_pSath;
 struct SFILE_ID_struct source1;
 } *AS_COHORT_EXPR;

typedef struct AS_CONST_DEF_struct {/* layout for AS_CONST_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_CONST_DEF;

typedef struct AS_CREATE_EXPR_struct {/* layout for AS_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_CREATE_EXPR;

typedef struct AS_DEC_STMT_struct {/* layout for AS_DEC_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_DEC_STMT;

typedef struct AS_EXCEPT_EXPR_struct {/* layout for AS_EXCEPT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_EXCEPT_EXPR;

typedef struct AS_EXPR_struct {/* layout for AS_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_EXPR;

typedef struct AS_EXPR_STMT_struct {/* layout for AS_EXPR_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_EXPR_STMT;

typedef struct AS_FAR_EXPR_struct {/* layout for AS_FAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_FAR_EXPR;

typedef struct AS_FEAT_MOD_struct {/* layout for AS_FEAT_MOD */
 OB_HEADER header;
 struct AS_FEAT_MOD_struct *next;
 struct IDENT_struct name1;
 struct IDENT_struct new_name;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_FEAT_MOD;

typedef struct AS_FLT_LIT_EXPR_struct {/* layout for AS_FLT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT tp;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AS_FLT_LIT_EXPR;

typedef struct AS_FORK_STMT_struct {/* layout for AS_FORK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_FORK_STMT;

typedef struct AS_GLOBAL_EXPR_struct {/* layout for AS_GLOBAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_GLOBAL_EXPR;

typedef struct AS_HERE_EXPR_struct {/* layout for AS_HERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_HERE_EXPR;

typedef struct AS_IDENT_LIST_struct {/* layout for AS_IDENT_LIST */
 OB_HEADER header;
 struct AS_IDENT_LIST_struct *next;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_IDENT_LIST;

typedef struct AS_IF_STMT_struct {/* layout for AS_IF_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_IF_STMT;

typedef struct AS_INC1416917001_struct {/* layout for AS_INCLUDE_CLAUSE */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_FEAT_MOD_struct *mods;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_INC1416917001;

typedef struct AS_INITIAL_EXPR_struct {/* layout for AS_INITIAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_INITIAL_EXPR;

typedef struct AS_INT2031239268_struct {/* layout for AS_INTERF_ATTACH_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct gatename;
 struct IDENT_struct helpername;
 struct IDENT_struct routname;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_INT2031239268;

typedef struct AS_INT_LIT_EXPR_struct {/* layout for AS_INT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 BOOL is_inti;
 } *AS_INT_LIT_EXPR;

typedef struct AS_IS_VOID_EXPR_struct {/* layout for AS_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *arg;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_IS_VOID_EXPR;

typedef struct AS_LOCK_IF_WHEN_struct {/* layout for AS_LOCK_IF_WHEN */
 OB_HEADER header;
 struct dAS_EXPR_struct *e_list;
 struct dAS_EXPR_struct *val1;
 struct AS_LOCK_IF_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 } *AS_LOCK_IF_WHEN;

typedef struct AS_LOCK_STMT_struct {/* layout for AS_LOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e_list;
 struct dAS_STMT_struct *next;
 struct AS_LOCK_IF_WHEN_struct *if_when_part;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_LOCK_STMT;

typedef struct AS_LOOP_STMT_struct {/* layout for AS_LOOP_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_LOOP_STMT;

typedef struct AS_NEAR_EXPR_struct {/* layout for AS_NEAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEAR_EXPR;

typedef struct AS_NEW_EXPR_struct {/* layout for AS_NEW_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *arg;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEW_EXPR;

typedef struct AS_OR_EXPR_struct {/* layout for AS_OR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_OR_EXPR;

typedef struct AS_PARAM_DEC_struct {/* layout for AS_PARAM_DEC */
 OB_HEADER header;
 struct AS_PARAM_DEC_struct *next;
 struct AS_TYPE_SPEC_struct *type_constraint;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_PARAM_DEC;

typedef struct AS_PAR_STMT_struct {/* layout for AS_PAR_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_PAR_STMT;

typedef struct AS_PROTECT_STMT_struct {/* layout for AS_PROTECT_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_PROTECT_WHEN_struct *when_part;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_PROTECT_STMT;

typedef struct AS_PROTECT_WHEN_struct {/* layout for AS_PROTECT_WHEN */
 OB_HEADER header;
 struct AS_PROTECT_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_PROTECT_WHEN;

typedef struct AS_QUIT_STMT_struct {/* layout for AS_QUIT_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_QUIT_STMT;

typedef struct AS_RAISE_STMT_struct {/* layout for AS_RAISE_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_RAISE_STMT;

typedef struct AS_RESULT_EXPR_struct {/* layout for AS_RESULT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_RESULT_EXPR;

typedef struct AS_RETURN_STMT_struct {/* layout for AS_RETURN_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_RETURN_STMT;

typedef struct AS_ROUT_DEF_struct {/* layout for AS_ROUT_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *post_e;
 struct dAS_EXPR_struct *pre_e;
 struct AS_ARG_DEC_struct *args_dec;
 struct AS_STMT_LIST_struct *body;
 struct AS_TYPE_SPEC_struct *ret_dec;
 struct IDENT_struct builtin_name;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_abstract;
 BOOL is_attach_routin;
 BOOL is_builtin;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL is_private;
 } *AS_ROUT_DEF;

typedef struct AS_SELF_EXPR_struct {/* layout for AS_SELF_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_SELF_EXPR;

typedef struct AS_SHARED_DEF_struct {/* layout for AS_SHARED_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_SHARED_DEF;

typedef struct AS_STMT_struct {/* layout for AS_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_STMT;

typedef struct AS_STMT_LIST_struct {/* layout for AS_STMT_LIST */
 OB_HEADER header;
 struct dAS_STMT_struct *stmts;
 struct SFILE_ID_struct source1;
 } *AS_STMT_LIST;

typedef struct AS_STR_LIT_EXPR_struct {/* layout for AS_STR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 STR s;
 } *AS_STR_LIT_EXPR;

typedef struct AS_SYNC_STMT_struct {/* layout for AS_SYNC_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_SYNC_STMT;

typedef struct AS_TYPECASE_STMT_struct {/* layout for AS_TYPECASE_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPECASE_WHEN_struct *when_part;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_TYPECASE_STMT;

typedef struct AS_TYPECASE_WHEN_struct {/* layout for AS_TYPECASE_WHEN */
 OB_HEADER header;
 struct AS_STMT_LIST_struct *then_part;
 struct AS_TYPECASE_WHEN_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_TYPECASE_WHEN;

typedef struct AS_TYPE_SPEC_struct {/* layout for AS_TYPE_SPEC */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *next;
 struct AS_TYPE_SPEC_struct *params;
 struct AS_TYPE_SPEC_struct *ret;
 struct IDENT_struct name1;
 INT kind;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_TYPE_SPEC;

typedef struct AS_UND152986614_struct {/* layout for AS_UNDERSCORE_ARG */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_UND152986614;

typedef struct AS_UNLOCK_STMT_struct {/* layout for AS_UNLOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_UNLOCK_STMT;

typedef struct AS_VOID_EXPR_struct {/* layout for AS_VOID_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_VOID_EXPR;

typedef struct AS_WHERE_EXPR_struct {/* layout for AS_WHERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_WHERE_EXPR;

typedef struct AS_WIT1063437351_struct {/* layout for AS_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_IDENT_LIST_struct *idents;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *near_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 INT elts_size;
 struct SFILE_ID_struct source1;
 BOOL self_occurred;
 BOOL transformed;
 } *AS_WIT1063437351;

typedef struct AS_YIELD_STMT_struct {/* layout for AS_YIELD_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_YIELD_STMT;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTAM_CURSOR_struct {/* layout for FLIST{AM_CURSOR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_struct *arr_part[1];
 } *FLISTAM_CURSOR;

typedef struct FLISTA1878650141_frame_struct {
 INT state;
 FLISTAM_CURSOR self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTA1878650141_frame;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTT1145236345_struct {/* layout for FLIST{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_927437481_struct arr_part[1];
 } *FLISTT1145236345;

typedef struct FLISTT709128269_frame_struct {
 INT state;
 FLISTT1145236345 self;/* Formal argument: self */
 TUPAM_927437481 ret_val1;
 INT i;
 INT sz;
 FLISTT1145236345 aget_self;
 INT aget_ind;
 TUPAM_927437481 ret_val;
 } *FLISTT709128269_frame;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FSETCS1561418284_struct {/* layout for FSET{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FSETCS1561418284;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

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

extern INOUT_MODE MODES_inout_mode;
extern IN_MODE MODES_in_mode;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;
extern PROG OPT_CO233462019;

/* Function declarations */


extern RETURNED_CONST INT (**dAS_EX1952576959)(dAS_EXPR);

extern RETURNED_CONST SFILE_ID (**dAM_EX2051891691)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dAS_CLASS_ELT (**dAS_CL760354380)(dAS_CLASS_ELT);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAS_CL1804429169)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_EX1809609236)(dAS_EXPR, dAS_EXPR);

extern RETURNED_CONST void (**dAS_ST1687647066)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AS_STMT_LIST AS_ASS1748452704(AS_ASSERT_STMT);
AS_STMT_LIST AS_ASS369354997(AS_ASSIGN_STMT);
AS_STMT_LIST AS_ATT1199964935(AS_ATTACH_STMT);
AS_STMT_LIST AS_CAS422199583(AS_CASE_STMT);
AS_STMT_LIST AS_DEC1951684792(AS_DEC_STMT);
AS_STMT_LIST AS_EXP80096096(AS_EXPR_STMT);
AS_STMT_LIST AS_FOR1300129919(AS_FORK_STMT);
AS_STMT_LIST AS_IF_1377965621(AS_IF_STMT);
AS_STMT_LIST AS_INT1290825245(AS_INT2031239268);
AS_STMT_LIST AS_LOC1425522258(AS_LOCK_STMT);
AS_STMT_LIST AS_LOO1823271233(AS_LOOP_STMT);
AS_STMT_LIST AS_PAR380674305(AS_PAR_STMT);
AS_STMT_LIST AS_PRO1473178483(AS_PROTECT_STMT);
AS_STMT_LIST AS_QUI1856043734(AS_QUIT_STMT);
AS_STMT_LIST AS_RAI1834286773(AS_RAISE_STMT);
AS_STMT_LIST AS_RET1323242550(AS_RETURN_STMT);
AS_STMT_LIST AS_STM87227529(AS_STMT);
AS_STMT_LIST AS_SYN1536258462(AS_SYNC_STMT);
AS_STMT_LIST AS_TYP2083833657(AS_TYPECASE_STMT);
AS_STMT_LIST AS_UNL900516002(AS_UNLOCK_STMT);
AS_STMT_LIST AS_WIT1668425008(AS_WIT1063437351);
AS_STMT_LIST AS_YIE913253602(AS_YIELD_STMT);
BOOL AM_CUR1481630356(AM_CURSOR);
BOOL AM_CUR2013152412(AM_CURSOR);
BOOL AM_CUR830576175(AM_CURSOR, AM_CURSOR, dAM);
BOOL AS_ASS1377838817(AS_ASSERT_STMT);
BOOL AS_ASS1755466016(AS_ASSIGN_STMT);
BOOL AS_ATT1182933590(AS_ATTACH_STMT);
BOOL AS_ATT2034251932(AS_ATTR_DEF);
BOOL AS_CAS964680792(AS_CASE_STMT);
BOOL AS_CON1915749157(AS_CONST_DEF);
BOOL AS_DEC2043607416(AS_DEC_STMT);
BOOL AS_EXP1274701909(AS_EXPR_STMT);
BOOL AS_FOR1720691150(AS_FORK_STMT);
BOOL AS_IF_117934348(AS_IF_STMT);
BOOL AS_INC827145130(AS_INC1416917001);
BOOL AS_INT130358855(AS_INT2031239268);
BOOL AS_LOC802751445(AS_LOCK_STMT);
BOOL AS_LOO612242568(AS_LOOP_STMT);
BOOL AS_PAR818790093(AS_PAR_STMT);
BOOL AS_PRO122180587(AS_PROTECT_STMT);
BOOL AS_QUI1489742669(AS_QUIT_STMT);
BOOL AS_RAI201810971(AS_RAISE_STMT);
BOOL AS_RET863344353(AS_RETURN_STMT);
BOOL AS_ROU1543294757(AS_ROUT_DEF);
BOOL AS_SHA952781759(AS_SHARED_DEF);
BOOL AS_STM1645255095(AS_STMT);
BOOL AS_SYN1201341531(AS_SYNC_STMT);
BOOL AS_TYP1265860748(AS_TYPECASE_STMT);
BOOL AS_UNL1126435291(AS_UNLOCK_STMT);
BOOL AS_WIT1566004798(AS_WIT1063437351);
BOOL AS_YIE1515612652(AS_YIELD_STMT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL INOUT_1534195007(INOUT_MODE);
BOOL INOUT_394279242(INOUT_MODE, dMODE);
BOOL IN_MOD1604254699(IN_MODE, dMODE);
BOOL IN_MOD851634165(IN_MODE);
BOOL ONCE_M1658631798(ONCE_MODE, dMODE);
BOOL ONCE_M1884799084(ONCE_MODE);
BOOL OUT_MO1200475098(OUT_MODE, dMODE);
BOOL OUT_MO898624407(OUT_MODE);
CSE_DOUBLE_EXPRS FSETCS1814959(FSETCS1561418284, CSE_DOUBLE_EXPRS);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTAM_CURSOR FLISTA122884859(FLISTAM_CURSOR, AM_CURSOR);
FLISTT1145236345 FLISTT596895593(FLISTT1145236345, TUPAM_927437481);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FMAPSTRINT FMAPST951578658(FMAPSTRINT, STR, INT);
FSETCS1561418284 FSETCS1791208015(FSETCS1561418284, CSE_DOUBLE_EXPRS);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INOUT_MODE INOUT_2024898585(INOUT_MODE);
INOUT_MODE MODES_969646796(MODES);
INT AS_AND1958569903(AS_AND_EXPR);
INT AS_ANY1507835814(AS_ANY_EXPR);
INT AS_ARG1759480338(AS_ARG_DEC);
INT AS_ARR1547513756(AS_ARRAY_EXPR);
INT AS_ARR1847317313(AS_ARRAY_EXPR);
INT AS_AT_355764446(AS_AT_EXPR);
INT AS_BOO711205340(AS_BOOL_LIT_EXPR);
INT AS_BOU1634505419(AS_BOU14269336);
INT AS_BRE836708546(AS_BREAK_EXPR);
INT AS_CAL1853175953(AS_CALL_EXPR);
INT AS_CAL755875642(AS_CALL_EXPR);
INT AS_CHA1908653268(AS_CHAR_LIT_EXPR);
INT AS_CLU1234120202(AS_CLUSTER_EXPR);
INT AS_CLU889538930(AS_CLU553570599);
INT AS_COH1949131218(AS_COHORT_EXPR);
INT AS_CRE1921695223(AS_CREATE_EXPR);
INT AS_CRE943264723(AS_CREATE_EXPR);
INT AS_EXC629960794(AS_EXCEPT_EXPR);
INT AS_EXPR_sizerINT(AS_EXPR);
INT AS_FAR1350508667(AS_FAR_EXPR);
INT AS_FEA1447190869(AS_FEAT_MOD);
INT AS_FLT374280066(AS_FLT_LIT_EXPR);
INT AS_GLO113092240(AS_GLOBAL_EXPR);
INT AS_HER1196006492(AS_HERE_EXPR);
INT AS_INI414727320(AS_INITIAL_EXPR);
INT AS_INT228236077(AS_INT_LIT_EXPR);
INT AS_IS_1954421395(AS_IS_VOID_EXPR);
INT AS_LOC1840648359(AS_LOCK_IF_WHEN);
INT AS_NEA1735606560(AS_NEAR_EXPR);
INT AS_NEW1836119458(AS_NEW_EXPR);
INT AS_OR_1113601798(AS_OR_EXPR);
INT AS_PAR401840644(AS_PARAM_DEC);
INT AS_RES32147922(AS_RESULT_EXPR);
INT AS_SEL1486205964(AS_SELF_EXPR);
INT AS_STR1375497755(AS_STR_LIT_EXPR);
INT AS_TYP2145685956(AS_TYPE_SPEC);
INT AS_UND1736379304(AS_UND152986614);
INT AS_VOI1355443380(AS_VOID_EXPR);
INT AS_WHE140356038(AS_WHERE_EXPR);
INT FLISTA1878650141(FLISTA1878650141_frame);
INT FMAPST1755966813(FMAPSTRINT, STR);
IN_MODE IN_MOD403789248(IN_MODE);
IN_MODE MODES_65249541(MODES);
ONCE_MODE MODES_1570711429(MODES);
ONCE_MODE ONCE_M704734922(ONCE_MODE);
OUT_MODE MODES_1166542234(MODES);
OUT_MODE OUT_MO1658580595(OUT_MODE);
SFILE_ID AS_AND749298802(AS_AND_EXPR);
SFILE_ID AS_ANY2075595295(AS_ANY_EXPR);
SFILE_ID AS_ARG265670232(AS_ARG_DEC);
SFILE_ID AS_ARR542631033(AS_ARRAY_EXPR);
SFILE_ID AS_ASS40608582(AS_ASSERT_STMT);
SFILE_ID AS_ASS964687339(AS_ASSIGN_STMT);
SFILE_ID AS_ATT1647669388(AS_ATTR_DEF);
SFILE_ID AS_ATT2064808605(AS_ATTACH_STMT);
SFILE_ID AS_AT_1597668772(AS_AT_EXPR);
SFILE_ID AS_BOO291537060(AS_BOOL_LIT_EXPR);
SFILE_ID AS_BOU1793757293(AS_BOU14269336);
SFILE_ID AS_BRE1173994939(AS_BREAK_EXPR);
SFILE_ID AS_CAL82770762(AS_CALL_EXPR);
SFILE_ID AS_CAS1215719733(AS_CASE_WHEN);
SFILE_ID AS_CAS986885563(AS_CASE_STMT);
SFILE_ID AS_CHA1542077308(AS_CHAR_LIT_EXPR);
SFILE_ID AS_CLA374248818(AS_CLASS_DEF);
SFILE_ID AS_CLU1205555915(AS_CLUSTER_EXPR);
SFILE_ID AS_CLU243953950(AS_CLU553570599);
SFILE_ID AS_COH931029852(AS_COHORT_EXPR);
SFILE_ID AS_CON1302703181(AS_CONST_DEF);
SFILE_ID AS_CRE1835137079(AS_CREATE_EXPR);
SFILE_ID AS_DEC1054605514(AS_DEC_STMT);
SFILE_ID AS_EXC2142015156(AS_EXCEPT_EXPR);
SFILE_ID AS_EXP353102998(AS_EXPR_STMT);
SFILE_ID AS_FAR1861383798(AS_FAR_EXPR);
SFILE_ID AS_FEA630208390(AS_FEAT_MOD);
SFILE_ID AS_FLT235098865(AS_FLT_LIT_EXPR);
SFILE_ID AS_FOR393448377(AS_FORK_STMT);
SFILE_ID AS_GLO435400634(AS_GLOBAL_EXPR);
SFILE_ID AS_HER667171168(AS_HERE_EXPR);
SFILE_ID AS_IDE48340397(AS_IDENT_LIST);
SFILE_ID AS_IF_1168370869(AS_IF_STMT);
SFILE_ID AS_INC214099154(AS_INC1416917001);
SFILE_ID AS_INI295049889(AS_INITIAL_EXPR);
SFILE_ID AS_INT1381353727(AS_INT2031239268);
SFILE_ID AS_INT6023736(AS_INT_LIT_EXPR);
SFILE_ID AS_IS_2018065350(AS_IS_VOID_EXPR);
SFILE_ID AS_LOC1119913724(AS_LOCK_STMT);
SFILE_ID AS_LOC1864862121(AS_LOCK_IF_WHEN);
SFILE_ID AS_LOO1129940507(AS_LOOP_STMT);
SFILE_ID AS_NEA708839628(AS_NEAR_EXPR);
SFILE_ID AS_NEW1623017607(AS_NEW_EXPR);
SFILE_ID AS_OR_1363025812(AS_OR_EXPR);
SFILE_ID AS_PAR158230952(AS_PARAM_DEC);
SFILE_ID AS_PAR666967763(AS_PAR_STMT);
SFILE_ID AS_PRO264390719(AS_PROTECT_STMT);
SFILE_ID AS_PRO493224889(AS_PROTECT_WHEN);
SFILE_ID AS_QUI1176752812(AS_QUIT_STMT);
SFILE_ID AS_RAI1765876403(AS_RAISE_STMT);
SFILE_ID AS_RES951018660(AS_RESULT_EXPR);
SFILE_ID AS_RET600804304(AS_RETURN_STMT);
SFILE_ID AS_ROU2138626563(AS_ROUT_DEF);
SFILE_ID AS_SEL957265064(AS_SELF_EXPR);
SFILE_ID AS_SHA1565827735(AS_SHARED_DEF);
SFILE_ID AS_STM1237803845(AS_STMT);
SFILE_ID AS_STR1687611758(AS_STR_LIT_EXPR);
SFILE_ID AS_SYN1451443212(AS_SYNC_STMT);
SFILE_ID AS_TYP1164969549(AS_TYPECASE_STMT);
SFILE_ID AS_TYP1615798168(AS_TYPE_SPEC);
SFILE_ID AS_TYP936135379(AS_TYPECASE_WHEN);
SFILE_ID AS_UND1334198639(AS_UND152986614);
SFILE_ID AS_UNL1835784100(AS_UNLOCK_STMT);
SFILE_ID AS_VOI1131745048(AS_VOID_EXPR);
SFILE_ID AS_WHE1480938151(AS_WHERE_EXPR);
SFILE_ID AS_WIT337771262(AS_WIT1063437351);
SFILE_ID AS_YIE1856267120(AS_YIELD_STMT);
STR INOUT_1907813337(INOUT_MODE);
STR IN_MODE_strrSTR(IN_MODE);
STR NAMESP1003761134(NAMESPACE, STR);
STR NAMESP180237388(NAMESPACE, STR);
STR NAMESP8552102(NAMESPACE, STR);
STR ONCE_M1994645546(ONCE_MODE);
STR OUT_MODE_strrSTR(OUT_MODE);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_fr1097270334(STR, FSTR);
TUPAM_927437481 FLISTT709128269(FLISTT709128269_frame);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAS_CLASS_ELT AS_ATT97460488(AS_ATTR_DEF);
dAS_CLASS_ELT AS_CON306298556(AS_CONST_DEF);
dAS_CLASS_ELT AS_INC1775598384(AS_INC1416917001);
dAS_CLASS_ELT AS_ROU123442773(AS_ROUT_DEF);
dAS_CLASS_ELT AS_SHA297921449(AS_SHARED_DEF);
dAS_EXPR AS_AND244040648(AS_AND_EXPR);
dAS_EXPR AS_ANY1960655325(AS_ANY_EXPR);
dAS_EXPR AS_ARR1675309363(AS_ARRAY_EXPR);
dAS_EXPR AS_AT_246103478(AS_AT_EXPR);
dAS_EXPR AS_BOO1496152570(AS_BOOL_LIT_EXPR);
dAS_EXPR AS_BOU110275841(AS_BOU14269336);
dAS_EXPR AS_BRE631040475(AS_BREAK_EXPR);
dAS_EXPR AS_CAL1709269196(AS_CALL_EXPR);
dAS_EXPR AS_CHA1807865070(AS_CHAR_LIT_EXPR);
dAS_EXPR AS_CLU1420620999(AS_CLUSTER_EXPR);
dAS_EXPR AS_CLU792721656(AS_CLU553570599);
dAS_EXPR AS_COH1869044806(AS_COHORT_EXPR);
dAS_EXPR AS_CRE1115272673(AS_CREATE_EXPR);
dAS_EXPR AS_EXC1501278546(AS_EXCEPT_EXPR);
dAS_EXPR AS_FAR2096957028(AS_FAR_EXPR);
dAS_EXPR AS_FLT1204717277(AS_FLT_LIT_EXPR);
dAS_EXPR AS_GLO1872829932(AS_GLOBAL_EXPR);
dAS_EXPR AS_HER846597914(AS_HERE_EXPR);
dAS_EXPR AS_INI1732476001(AS_INITIAL_EXPR);
dAS_EXPR AS_INT896052382(AS_INT_LIT_EXPR);
dAS_EXPR AS_IS_1211675728(AS_IS_VOID_EXPR);
dAS_EXPR AS_NEA160870810(AS_NEAR_EXPR);
dAS_EXPR AS_NEW162891781(AS_NEW_EXPR);
dAS_EXPR AS_OR_261789378(AS_OR_EXPR);
dAS_EXPR AS_RES945429434(AS_RESULT_EXPR);
dAS_EXPR AS_SEL1160430562(AS_SELF_EXPR);
dAS_EXPR AS_STR70439144(AS_STR_LIT_EXPR);
dAS_EXPR AS_UND1982396421(AS_UND152986614);
dAS_EXPR AS_VOI1856591394(AS_VOID_EXPR);
dAS_EXPR AS_WHE2011036209(AS_WHERE_EXPR);
dAS_STMT AS_ASS255483717(AS_ASSERT_STMT);
dAS_STMT AS_ASS384416990(AS_ASSIGN_STMT);
dAS_STMT AS_ATT428959228(AS_ATTACH_STMT);
dAS_STMT AS_CAS568284252(AS_CASE_STMT);
dAS_STMT AS_DEC1501678601(AS_DEC_STMT);
dAS_STMT AS_EXP516982707(AS_EXPR_STMT);
dAS_STMT AS_FOR721306794(AS_FORK_STMT);
dAS_STMT AS_IF_192647666(AS_IF_STMT);
dAS_STMT AS_INT1461683196(AS_INT2031239268);
dAS_STMT AS_LOC1353882229(AS_LOCK_STMT);
dAS_STMT AS_LOO1639447996(AS_LOOP_STMT);
dAS_STMT AS_PAR1281243032(AS_PAR_STMT);
dAS_STMT AS_PRO1410999444(AS_PROTECT_STMT);
dAS_STMT AS_QUI758322419(AS_QUIT_STMT);
dAS_STMT AS_RAI287018298(AS_RAISE_STMT);
dAS_STMT AS_RET732932159(AS_RETURN_STMT);
dAS_STMT AS_STM1163003696(AS_STMT);
dAS_STMT AS_SYN57982213(AS_SYNC_STMT);
dAS_STMT AS_TYP1323758084(AS_TYPECASE_STMT);
dAS_STMT AS_UNL712558323(AS_UNLOCK_STMT);
dAS_STMT AS_WIT1968509975(AS_WIT1063437351);
dAS_STMT AS_YIE1402665211(AS_YIELD_STMT);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
void AM_CUR1769624888(AM_CURSOR);
void AM_CUR915996644(AM_CURSOR, dAM_EXPR);
void AS_AND1244275737(AS_AND_EXPR, dAS_EXPR);
void AS_AND1398627607(AS_AND_EXPR, SFILE_ID);
void AS_AND1903000239(AS_AND_EXPR, dAS_EXPR);
void AS_ANY1724395066(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY187271084(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY72331114(AS_ANY_EXPR, SFILE_ID);
void AS_ARG2059479532(AS_ARG_DEC, AS_ARG_DEC);
void AS_ARR1604409854(AS_ARRAY_EXPR, SFILE_ID);
void AS_ARR471731524(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ARR47654098(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ASS1183239070(AS_ASSIGN_STMT, SFILE_ID);
void AS_ASS1330792936(AS_ASSERT_STMT, BOOL);
void AS_ASS1461460287(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1708420135(AS_ASSIGN_STMT, BOOL);
void AS_ASS1762623897(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1892442692(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS2029454919(AS_ASSERT_STMT, AS_STMT_LIST);
void AS_ASS2107317827(AS_ASSERT_STMT, SFILE_ID);
void AS_ASS537381530(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS650357212(AS_ASSIGN_STMT, AS_STMT_LIST);
void AS_ATT1135887709(AS_ATTACH_STMT, BOOL);
void AS_ATT1141535277(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT1480967150(AS_ATTACH_STMT, AS_STMT_LIST);
void AS_ATT1718081659(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1733385743(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1987206051(AS_ATTR_DEF, BOOL);
void AS_ATT2024939759(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT83117804(AS_ATTACH_STMT, SFILE_ID);
void AS_AT_1901822931(AS_AT_EXPR, dAS_EXPR);
void AS_AT_2092645707(AS_AT_EXPR, dAS_EXPR);
void AS_AT_550257637(AS_AT_EXPR, SFILE_ID);
void AS_BOO1855503827(AS_BOOL_LIT_EXPR, SFILE_ID);
void AS_BOO203439875(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOO650888317(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOU2006233068(AS_BOU14269336, dAS_EXPR);
void AS_BOU2036765046(AS_BOU14269336, dAS_EXPR);
void AS_BOU354169116(AS_BOU14269336, SFILE_ID);
void AS_BRE1516885934(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE679018004(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE973045948(AS_BREAK_EXPR, SFILE_ID);
void AS_CAL2065155647(AS_CALL_EXPR, SFILE_ID);
void AS_CAL437771691(AS_CALL_EXPR, dAS_EXPR);
void AS_CAL577747697(AS_CALL_EXPR, dAS_EXPR);
void AS_CAS1011726673(AS_CASE_STMT, BOOL);
void AS_CAS1160155324(AS_CASE_STMT, SFILE_ID);
void AS_CAS1219409394(AS_CASE_WHEN, AS_CASE_WHEN);
void AS_CAS1578756635(AS_CASE_STMT, dAS_STMT);
void AS_CAS490112615(AS_CASE_STMT, dAS_STMT);
void AS_CAS703201798(AS_CASE_STMT, AS_STMT_LIST);
void AS_CHA1047100373(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA340061339(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA604963579(AS_CHAR_LIT_EXPR, SFILE_ID);
void AS_CLA1288691224(AS_CLASS_DEF, AS_CLASS_DEF);
void AS_CLA129549718(AS_CLASS_ELT, dAS_CLASS_ELT);
void AS_CLU1354319231(AS_CLU553570599, dAS_EXPR);
void AS_CLU1700532850(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU1903972459(AS_CLU553570599, SFILE_ID);
void AS_CLU726419888(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU738930885(AS_CLU553570599, dAS_EXPR);
void AS_CLU942370494(AS_CLUSTER_EXPR, SFILE_ID);
void AS_COH1216011035(AS_COHORT_EXPR, SFILE_ID);
void AS_COH278881603(AS_COHORT_EXPR, dAS_EXPR);
void AS_COH436052917(AS_COHORT_EXPR, dAS_EXPR);
void AS_CON1753066199(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CON1962795038(AS_CONST_DEF, BOOL);
void AS_CON737776233(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CRE1032653736(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE1964853282(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE312789330(AS_CREATE_EXPR, SFILE_ID);
void AS_DEC1092435373(AS_DEC_STMT, SFILE_ID);
void AS_DEC1996561535(AS_DEC_STMT, BOOL);
void AS_DEC2062280289(AS_DEC_STMT, AS_STMT_LIST);
void AS_DEC557832566(AS_DEC_STMT, dAS_STMT);
void AS_DEC646247808(AS_DEC_STMT, dAS_STMT);
void AS_EXC1647038221(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXC5025731(AS_EXCEPT_EXPR, SFILE_ID);
void AS_EXC645762341(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXP1321747790(AS_EXPR_STMT, BOOL);
void AS_EXP1630058180(AS_EXPR_STMT, dAS_STMT);
void AS_EXP1794823411(AS_EXPR_STMT, SFILE_ID);
void AS_EXP200906119(AS_EXPR_STMT, AS_STMT_LIST);
void AS_EXP2124685825(AS_EXPR, dAS_EXPR);
void AS_EXP849875946(AS_EXPR_STMT, dAS_STMT);
void AS_FAR1938606563(AS_FAR_EXPR, dAS_EXPR);
void AS_FAR286542611(AS_FAR_EXPR, SFILE_ID);
void AS_FAR50083859(AS_FAR_EXPR, dAS_EXPR);
void AS_FEA814213709(AS_FEAT_MOD, AS_FEAT_MOD);
void AS_FLT1911942022(AS_FLT_LIT_EXPR, SFILE_ID);
void AS_FLT259878070(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FLT943209132(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FOR1019127704(AS_FORK_STMT, AS_STMT_LIST);
void AS_FOR103324571(AS_FORK_STMT, dAS_STMT);
void AS_FOR1425734093(AS_FORK_STMT, dAS_STMT);
void AS_FOR1673645269(AS_FORK_STMT, BOOL);
void AS_FOR1753592510(AS_FORK_STMT, SFILE_ID);
void AS_GLO1712525775(AS_GLOBAL_EXPR, SFILE_ID);
void AS_GLO275096477(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_GLO930377569(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_HER1301328495(AS_HERE_EXPR, dAS_EXPR);
void AS_HER1479869719(AS_HERE_EXPR, SFILE_ID);
void AS_HER172194233(AS_HERE_EXPR, dAS_EXPR);
void AS_IDE1407507383(AS_IDENT_LIST, AS_IDENT_LIST);
void AS_IF_164980229(AS_IF_STMT, BOOL);
void AS_IF_1658967836(AS_IF_STMT, AS_STMT_LIST);
void AS_IF_1955278743(AS_IF_STMT, dAS_STMT);
void AS_IF_671597921(AS_IF_STMT, dAS_STMT);
void AS_IF_978670018(AS_IF_STMT, SFILE_ID);
void AS_INC1475294123(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC1807545759(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC874191011(AS_INC1416917001, BOOL);
void AS_INI1852876520(AS_INITIAL_EXPR, SFILE_ID);
void AS_INI414564886(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INI790026824(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INT1009823030(AS_INT2031239268, AS_STMT_LIST);
void AS_INT1251874027(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT2141902673(AS_INT_LIT_EXPR, SFILE_ID);
void AS_INT501000671(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT685357691(AS_INT2031239268, dAS_STMT);
void AS_INT765687160(AS_INT2031239268, SFILE_ID);
void AS_INT83312974(AS_INT2031239268, BOOL);
void AS_INT884580779(AS_INT2031239268, dAS_STMT);
void AS_IS_128975537(AS_IS_VOID_EXPR, SFILE_ID);
void AS_IS_1523088415(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_IS_936250681(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_LOC1027127163(AS_LOCK_STMT, SFILE_ID);
void AS_LOC1037771083(AS_LOCK_IF_WHEN, AS_LOCK_IF_WHEN);
void AS_LOC1144520043(AS_LOCK_STMT, AS_STMT_LIST);
void AS_LOC623140776(AS_LOCK_STMT, dAS_STMT);
void AS_LOC755705564(AS_LOCK_STMT, BOOL);
void AS_LOC793158658(AS_LOCK_STMT, dAS_STMT);
void AS_LOO1017100380(AS_LOOP_STMT, SFILE_ID);
void AS_LOO1542269018(AS_LOOP_STMT, AS_STMT_LIST);
void AS_LOO507592891(AS_LOOP_STMT, dAS_STMT);
void AS_LOO565196687(AS_LOOP_STMT, BOOL);
void AS_LOO633167559(AS_LOOP_STMT, dAS_STMT);
void AS_NEA1203816563(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEA1439086781(AS_NEAR_EXPR, SFILE_ID);
void AS_NEA1986170077(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEW1985034628(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW2117994542(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW524908802(AS_NEW_EXPR, SFILE_ID);
void AS_OR_1885251509(AS_OR_EXPR, dAS_EXPR);
void AS_OR_784015075(AS_OR_EXPR, SFILE_ID);
void AS_OR_868048877(AS_OR_EXPR, dAS_EXPR);
void AS_PAR1480073124(AS_PAR_STMT, SFILE_ID);
void AS_PAR170194815(AS_PAR_STMT, dAS_STMT);
void AS_PAR769375844(AS_PARAM_DEC, AS_PARAM_DEC);
void AS_PAR771744212(AS_PAR_STMT, BOOL);
void AS_PAR865797855(AS_PAR_STMT, dAS_STMT);
void AS_PAR99672090(AS_PAR_STMT, AS_STMT_LIST);
void AS_PRO1103329709(AS_PROTECT_WHEN, AS_PROTECT_WHEN);
void AS_PRO169226468(AS_PROTECT_STMT, BOOL);
void AS_PRO1754180698(AS_PROTECT_STMT, AS_STMT_LIST);
void AS_PRO1882650168(AS_PROTECT_STMT, SFILE_ID);
void AS_PRO232382229(AS_PROTECT_STMT, dAS_STMT);
void AS_PRO736041443(AS_PROTECT_STMT, dAS_STMT);
void AS_QUI1389603990(AS_QUIT_STMT, dAS_STMT);
void AS_QUI1442696788(AS_QUIT_STMT, BOOL);
void AS_QUI1673525760(AS_QUIT_STMT, dAS_STMT);
void AS_QUI2137045949(AS_QUIT_STMT, AS_STMT_LIST);
void AS_QUI971173597(AS_QUIT_STMT, SFILE_ID);
void AS_RAI1860022589(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2032317945(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2115288988(AS_RAISE_STMT, AS_STMT_LIST);
void AS_RAI248856852(AS_RAISE_STMT, BOOL);
void AS_RAI382050006(AS_RAISE_STMT, SFILE_ID);
void AS_RES1196907749(AS_RESULT_EXPR, SFILE_ID);
void AS_RES1201611453(AS_RESULT_EXPR, dAS_EXPR);
void AS_RES1445995595(AS_RESULT_EXPR, dAS_EXPR);
void AS_RET1042240335(AS_RETURN_STMT, AS_STMT_LIST);
void AS_RET1097577252(AS_RETURN_STMT, dAS_STMT);
void AS_RET1414994250(AS_RETURN_STMT, dAS_STMT);
void AS_RET1547122105(AS_RETURN_STMT, SFILE_ID);
void AS_RET910390234(AS_RETURN_STMT, BOOL);
void AS_ROU1496248876(AS_ROUT_DEF, BOOL);
void AS_ROU166638356(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_ROU920632016(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_SEL1190661345(AS_SELF_EXPR, SFILE_ID);
void AS_SEL1452241999(AS_SELF_EXPR, dAS_EXPR);
void AS_SEL987495847(AS_SELF_EXPR, dAS_EXPR);
void AS_SHA1271069808(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA746153340(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA905735878(AS_SHARED_DEF, BOOL);
void AS_STM1692300976(AS_STMT, BOOL);
void AS_STM1734576793(AS_STMT, dAS_STMT);
void AS_STM193774686(AS_STMT, AS_STMT_LIST);
void AS_STM910122564(AS_STMT, SFILE_ID);
void AS_STM984922713(AS_STMT, dAS_STMT);
void AS_STR1192634823(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR2077487265(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR459429129(AS_STR_LIT_EXPR, SFILE_ID);
void AS_SYN1248387412(AS_SYNC_STMT, BOOL);
void AS_SYN1817260677(AS_SYNC_STMT, AS_STMT_LIST);
void AS_SYN2089058674(AS_SYNC_STMT, dAS_STMT);
void AS_SYN695597675(AS_SYNC_STMT, SFILE_ID);
void AS_SYN954670264(AS_SYNC_STMT, dAS_STMT);
void AS_TYP1027254244(AS_TYPE_SPEC, AS_TYPE_SPEC);
void AS_TYP1218814867(AS_TYPECASE_STMT, BOOL);
void AS_TYP1661742497(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP1802831442(AS_TYPECASE_STMT, AS_STMT_LIST);
void AS_TYP1805639138(AS_TYPECASE_WHEN, AS_TYPECASE_WHEN);
void AS_TYP823282803(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP982956860(AS_TYPECASE_STMT, SFILE_ID);
void AS_UND165529988(AS_UND152986614, dAS_EXPR);
void AS_UND1829175574(AS_UND152986614, dAS_EXPR);
void AS_UND813727770(AS_UND152986614, SFILE_ID);
void AS_UNL1079389410(AS_UNLOCK_STMT, BOOL);
void AS_UNL1435368086(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL1962410248(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL312142309(AS_UNLOCK_STMT, SFILE_ID);
void AS_UNL619513787(AS_UNLOCK_STMT, AS_STMT_LIST);
void AS_VOI1015295839(AS_VOID_EXPR, SFILE_ID);
void AS_VOI291335015(AS_VOID_EXPR, dAS_EXPR);
void AS_VOI636768113(AS_VOID_EXPR, dAS_EXPR);
void AS_WHE136004678(AS_WHERE_EXPR, dAS_EXPR);
void AS_WHE666102736(AS_WHERE_EXPR, SFILE_ID);
void AS_WHE985961216(AS_WHERE_EXPR, dAS_EXPR);
void AS_WIT1613050679(AS_WIT1063437351, BOOL);
void AS_WIT178530912(AS_WIT1063437351, dAS_STMT);
void AS_WIT1810155147(AS_WIT1063437351, SFILE_ID);
void AS_WIT1949427223(AS_WIT1063437351, AS_STMT_LIST);
void AS_WIT834544210(AS_WIT1063437351, dAS_STMT);
void AS_YIE1194255817(AS_YIELD_STMT, AS_STMT_LIST);
void AS_YIE1468566771(AS_YIELD_STMT, BOOL);
void AS_YIE1941927228(AS_YIELD_STMT, dAS_STMT);
void AS_YIE291659289(AS_YIELD_STMT, SFILE_ID);
void AS_YIE745261198(AS_YIELD_STMT, dAS_STMT);
void OPT_CS641813603(OPT_CSE, AM_ROUT_DEF);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_ASS1748452704(AS_ASSERT_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_ASS369354997(AS_ASSIGN_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_ATT1199964935(AS_ATTACH_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_CAS422199583(AS_CASE_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_DEC1951684792(AS_DEC_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_EXP80096096(AS_EXPR_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_FOR1300129919(AS_FORK_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_IF_1377965621(AS_IF_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_INT1290825245(AS_INT2031239268 self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_LOC1425522258(AS_LOCK_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_LOO1823271233(AS_LOOP_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_PAR380674305(AS_PAR_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_PRO1473178483(AS_PROTECT_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_QUI1856043734(AS_QUIT_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_RAI1834286773(AS_RAISE_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_RET1323242550(AS_RETURN_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_STM87227529(AS_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_SYN1536258462(AS_SYNC_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_TYP2083833657(AS_TYPECASE_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_UNL900516002(AS_UNLOCK_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_WIT1668425008(AS_WIT1063437351 self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST AS_YIE913253602(AS_YIELD_STMT self) {
 return ATTR(self,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_ASS1377838817(AS_ASSERT_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_ASS1755466016(AS_ASSIGN_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_ATT1182933590(AS_ATTACH_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_ATT2034251932(AS_ATTR_DEF self) {
 return ATTR(self,is_private);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_CAS964680792(AS_CASE_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_CON1915749157(AS_CONST_DEF self) {
 return ATTR(self,is_private);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_DEC2043607416(AS_DEC_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_EXP1274701909(AS_EXPR_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_FOR1720691150(AS_FORK_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_IF_117934348(AS_IF_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_INC827145130(AS_INC1416917001 self) {
 return ATTR(self,is_private);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_INT130358855(AS_INT2031239268 self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_LOC802751445(AS_LOCK_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_LOO612242568(AS_LOOP_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_PAR818790093(AS_PAR_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_PRO122180587(AS_PROTECT_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_QUI1489742669(AS_QUIT_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_RAI201810971(AS_RAISE_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_RET863344353(AS_RETURN_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_ROU1543294757(AS_ROUT_DEF self) {
 return ATTR(self,is_private);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_SHA952781759(AS_SHARED_DEF self) {
 return ATTR(self,is_private);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_STM1645255095(AS_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_SYN1201341531(AS_SYNC_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_TYP1265860748(AS_TYPECASE_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_UNL1126435291(AS_UNLOCK_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_WIT1566004798(AS_WIT1063437351 self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL AS_YIE1515612652(AS_YIELD_STMT self) {
 return ATTR(self,transformed);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INOUT_1534195007(INOUT_MODE self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INOUT_394279242(INOUT_MODE self, dMODE m) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),((OB) m));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL IN_MOD1604254699(IN_MODE self, dMODE m) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),((OB) m));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL IN_MOD851634165(IN_MODE self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL ONCE_M1658631798(ONCE_MODE self, dMODE m) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),((OB) m));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL ONCE_M1884799084(ONCE_MODE self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OUT_MO1200475098(OUT_MODE self, dMODE m) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),((OB) m));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OUT_MO898624407(OUT_MODE self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INOUT_MODE INOUT_2024898585(INOUT_MODE self) {
 INOUT_MODE ret_val;
 INOUT_MODE res;
 INOUT_MODE L1;
 OB L2;
 if ((MODES_969646796(((MODES) NULL))==((INOUT_MODE) NULL))) {
  L2=ZALLOC_LEAF(sizeof(struct INOUT_MODE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  memset(L2,0,sizeof(struct INOUT_MODE_struct));
  ((OB)L2)->header.tag=INOUT_MODE_tag;
  L1 = ((INOUT_MODE) L2);
  res = L1;
  ret_val = res;
  return ret_val;
 }
 else {
  ret_val = MODES_969646796(((MODES) NULL));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INOUT_MODE MODES_969646796(MODES self) {
 return MODES_inout_mode;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_AND1958569903(AS_AND_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_AND_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_ANY1507835814(AS_ANY_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_ANY_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_ARG1759480338(AS_ARG_DEC self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 AS_ARG_DEC n;
 INT L11_;
 if ((self==((AS_ARG_DEC) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((AS_ARG_DEC) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  n = ATTR(n,next);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_ARR1547513756(AS_ARRAY_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_ARRAY_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_ARR1847317313(AS_ARRAY_EXPR self) {
 INT ret_val = INT_zero;
 dAS_EXPR L1;
 if ((ATTR(self,elts)==((dAS_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L1 = ATTR(self,elts);
  ret_val = (*dAS_EX1952576959[TAG(L1)])(L1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AS_AT_355764446(AS_AT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_AT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_BOO711205340(AS_BOOL_LIT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_BOOL_LIT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_BOU1634505419(AS_BOU14269336 self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_BOU14269336) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_BRE836708546(AS_BREAK_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_BREAK_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CAL1853175953(AS_CALL_EXPR self) {
 INT ret_val = INT_zero;
 dAS_EXPR L1;
 if ((ATTR(self,args)==((dAS_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L1 = ATTR(self,args);
  ret_val = (*dAS_EX1952576959[TAG(L1)])(L1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CAL755875642(AS_CALL_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_CALL_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CHA1908653268(AS_CHAR_LIT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_CHAR_LIT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CLU1234120202(AS_CLUSTER_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_CLUSTER_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CLU889538930(AS_CLU553570599 self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_CLU553570599) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_COH1949131218(AS_COHORT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_COHORT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CRE1921695223(AS_CREATE_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_CREATE_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_CRE943264723(AS_CREATE_EXPR self) {
 INT ret_val = INT_zero;
 dAS_EXPR L1;
 if ((ATTR(self,elts)==((dAS_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L1 = ATTR(self,elts);
  ret_val = (*dAS_EX1952576959[TAG(L1)])(L1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AS_EXC629960794(AS_EXCEPT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_EXCEPT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_EXPR_sizerINT(AS_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_FAR1350508667(AS_FAR_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_FAR_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_FEA1447190869(AS_FEAT_MOD self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 AS_FEAT_MOD n;
 INT L11_;
 if ((self==((AS_FEAT_MOD) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((AS_FEAT_MOD) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  n = ATTR(n,next);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_FLT374280066(AS_FLT_LIT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_FLT_LIT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_GLO113092240(AS_GLOBAL_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_GLOBAL_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_HER1196006492(AS_HERE_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_HERE_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_INI414727320(AS_INITIAL_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_INITIAL_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_INT228236077(AS_INT_LIT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_INT_LIT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_IS_1954421395(AS_IS_VOID_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_IS_VOID_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_LOC1840648359(AS_LOCK_IF_WHEN self) {
 INT ret_val = INT_zero;
 dAS_EXPR L1;
 if ((ATTR(self,e_list)==((dAS_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  L1 = ATTR(self,e_list);
  ret_val = (*dAS_EX1952576959[TAG(L1)])(L1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AS_NEA1735606560(AS_NEAR_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_NEAR_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_NEW1836119458(AS_NEW_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_NEW_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_OR_1113601798(AS_OR_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_OR_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_PAR401840644(AS_PARAM_DEC self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 AS_PARAM_DEC n;
 INT L11_;
 if ((self==((AS_PARAM_DEC) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((AS_PARAM_DEC) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  n = ATTR(n,next);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_RES32147922(AS_RESULT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_RESULT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_SEL1486205964(AS_SELF_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_SELF_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_STR1375497755(AS_STR_LIT_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_STR_LIT_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_TYP2145685956(AS_TYPE_SPEC self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 AS_TYPE_SPEC n;
 INT L11_;
 if ((self==((AS_TYPE_SPEC) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((AS_TYPE_SPEC) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  n = ATTR(n,next);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_UND1736379304(AS_UND152986614 self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_UND152986614) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_VOI1355443380(AS_VOID_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_VOID_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AS_WHE140356038(AS_WHERE_EXPR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR n;
 INT L11_;
 dAS_EXPR L2;
 if ((self==((AS_WHERE_EXPR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 r = 1;
 n = ATTR(self,next);
 while (1) {
  if ((n==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L11_=INTPLUS(r,1); 
  r = L11_;
  L2 = n;
  n = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IN_MODE IN_MOD403789248(IN_MODE self) {
 IN_MODE ret_val;
 IN_MODE res;
 IN_MODE L1;
 OB L2;
 if ((MODES_65249541(((MODES) NULL))==((IN_MODE) NULL))) {
  L2=ZALLOC_LEAF(sizeof(struct IN_MODE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  memset(L2,0,sizeof(struct IN_MODE_struct));
  ((OB)L2)->header.tag=IN_MODE_tag;
  L1 = ((IN_MODE) L2);
  res = L1;
  ret_val = res;
  return ret_val;
 }
 else {
  ret_val = MODES_65249541(((MODES) NULL));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

IN_MODE MODES_65249541(MODES self) {
 return MODES_in_mode;
}


#undef IS_ITER
#define IS_ITER 0

ONCE_MODE MODES_1570711429(MODES self) {
 return MODES_once_mode;
}


#undef IS_ITER
#define IS_ITER 0

ONCE_MODE ONCE_M704734922(ONCE_MODE self) {
 ONCE_MODE ret_val;
 ONCE_MODE res;
 ONCE_MODE L1;
 OB L2;
 if ((MODES_1570711429(((MODES) NULL))==((ONCE_MODE) NULL))) {
  L2=ZALLOC_LEAF(sizeof(struct ONCE_MODE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  memset(L2,0,sizeof(struct ONCE_MODE_struct));
  ((OB)L2)->header.tag=ONCE_MODE_tag;
  L1 = ((ONCE_MODE) L2);
  res = L1;
  ret_val = res;
  return ret_val;
 }
 else {
  ret_val = MODES_1570711429(((MODES) NULL));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

OUT_MODE MODES_1166542234(MODES self) {
 return MODES_out_mode;
}


#undef IS_ITER
#define IS_ITER 0

OUT_MODE OUT_MO1658580595(OUT_MODE self) {
 OUT_MODE ret_val;
 OUT_MODE res;
 OUT_MODE L1;
 OB L2;
 if ((MODES_1166542234(((MODES) NULL))==((OUT_MODE) NULL))) {
  L2=ZALLOC_LEAF(sizeof(struct OUT_MODE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  memset(L2,0,sizeof(struct OUT_MODE_struct));
  ((OB)L2)->header.tag=OUT_MODE_tag;
  L1 = ((OUT_MODE) L2);
  res = L1;
  ret_val = res;
  return ret_val;
 }
 else {
  ret_val = MODES_1166542234(((MODES) NULL));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_AND749298802(AS_AND_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ANY2075595295(AS_ANY_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ARG265670232(AS_ARG_DEC self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ARR542631033(AS_ARRAY_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ASS40608582(AS_ASSERT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ASS964687339(AS_ASSIGN_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ATT1647669388(AS_ATTR_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ATT2064808605(AS_ATTACH_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_AT_1597668772(AS_AT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_BOO291537060(AS_BOOL_LIT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_BOU1793757293(AS_BOU14269336 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_BRE1173994939(AS_BREAK_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CAL82770762(AS_CALL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CAS1215719733(AS_CASE_WHEN self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CAS986885563(AS_CASE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CHA1542077308(AS_CHAR_LIT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CLA374248818(AS_CLASS_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CLU1205555915(AS_CLUSTER_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CLU243953950(AS_CLU553570599 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_COH931029852(AS_COHORT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CON1302703181(AS_CONST_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_CRE1835137079(AS_CREATE_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_DEC1054605514(AS_DEC_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_EXC2142015156(AS_EXCEPT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_EXP353102998(AS_EXPR_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_FAR1861383798(AS_FAR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_FEA630208390(AS_FEAT_MOD self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_FLT235098865(AS_FLT_LIT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_FOR393448377(AS_FORK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_GLO435400634(AS_GLOBAL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_HER667171168(AS_HERE_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_IDE48340397(AS_IDENT_LIST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_IF_1168370869(AS_IF_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_INC214099154(AS_INC1416917001 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_INI295049889(AS_INITIAL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_INT1381353727(AS_INT2031239268 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_INT6023736(AS_INT_LIT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_IS_2018065350(AS_IS_VOID_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_LOC1119913724(AS_LOCK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_LOC1864862121(AS_LOCK_IF_WHEN self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_LOO1129940507(AS_LOOP_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_NEA708839628(AS_NEAR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_NEW1623017607(AS_NEW_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_OR_1363025812(AS_OR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_PAR158230952(AS_PARAM_DEC self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_PAR666967763(AS_PAR_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_PRO264390719(AS_PROTECT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_PRO493224889(AS_PROTECT_WHEN self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_QUI1176752812(AS_QUIT_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_RAI1765876403(AS_RAISE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_RES951018660(AS_RESULT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_RET600804304(AS_RETURN_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_ROU2138626563(AS_ROUT_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_SEL957265064(AS_SELF_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_SHA1565827735(AS_SHARED_DEF self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_STM1237803845(AS_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_STR1687611758(AS_STR_LIT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_SYN1451443212(AS_SYNC_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_TYP1164969549(AS_TYPECASE_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_TYP1615798168(AS_TYPE_SPEC self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_TYP936135379(AS_TYPECASE_WHEN self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_UND1334198639(AS_UND152986614 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_UNL1835784100(AS_UNLOCK_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_VOI1131745048(AS_VOID_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_WHE1480938151(AS_WHERE_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_WIT337771262(AS_WIT1063437351 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_YIE1856267120(AS_YIELD_STMT self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR INOUT_1907813337(INOUT_MODE self) {
 STR ret_val;
 extern STR inout;
 ret_val = ((STR) &inout);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR IN_MODE_strrSTR(IN_MODE self) {
 STR ret_val;
 extern STR name1;
 ret_val = ((STR) &name1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR NAMESP1003761134(NAMESPACE self, STR s) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val1;
 STR s1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 INT L1;
 INT L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 while (1) {
  L1 = ATTR(self,counter);
  L21_=INTPLUS(L1,1); 
  SATTR(self,counter,L21_);
  plus_self = s;
  plus_arg = ATTR(self,counter);
  str_self = plus_arg;
  clear_self = buf1;
  L3 = (clear_self==((FSTR) NULL));
  L41_=!(L3); 
  if (L41_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val3;
  str_self1 = buf1;
  ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val2 = ret_val4;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  res = ret_val1;
  L5 = FSETST1025458804(ATTR(self,set), res);
  L61_=!(L5); 
  if (L61_) {
   goto after_loop;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR NAMESP180237388(NAMESPACE self, STR s) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val1;
 STR s1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 INT L1;
 INT L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 while (1) {
  L1 = ATTR(self,counter);
  L21_=INTPLUS(L1,1); 
  SATTR(self,counter,L21_);
  plus_self = s;
  plus_arg = ATTR(self,counter);
  str_self = plus_arg;
  clear_self = buf1;
  L3 = (clear_self==((FSTR) NULL));
  L41_=!(L3); 
  if (L41_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val3;
  str_self1 = buf1;
  ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val2 = ret_val4;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  res = ret_val1;
  L5 = FSETST1025458804(ATTR(self,set), res);
  L61_=!(L5); 
  if (L61_) {
   goto after_loop;
  }
 }
 after_loop: ;
 SATTR(self,set,FSETST1404644833(ATTR(self,set), res));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR NAMESP8552102(NAMESPACE self, STR s) {
 STR ret_val;
 STR res = ((STR) NULL);
 INT i = INT_zero;
 STR plus_self;
 INT plus_arg = INT_zero;
 STR ret_val1;
 STR s1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 INT L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 i = FMAPST1755966813(ATTR(self,uniques), s);
 if (INT_IS_VOID(i)) {
  i = 0;
 }
 while (1) {
  L11_=INTPLUS(i,1); 
  i = L11_;
  plus_self = s;
  plus_arg = i;
  str_self = plus_arg;
  clear_self = buf1;
  L2 = (clear_self==((FSTR) NULL));
  L31_=!(L2); 
  if (L31_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val3;
  str_self1 = buf1;
  ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val2 = ret_val4;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  res = ret_val1;
  L4 = FSETST1025458804(ATTR(self,set), res);
  L51_=!(L4); 
  if (L51_) {
   goto after_loop;
  }
 }
 after_loop: ;
 SATTR(self,uniques,FMAPST951578658(ATTR(self,uniques), s, i));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ONCE_M1994645546(ONCE_MODE self) {
 STR ret_val;
 extern STR once;
 ret_val = ((STR) &once);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR OUT_MODE_strrSTR(OUT_MODE self) {
 STR ret_val;
 extern STR out1;
 ret_val = ((STR) &out1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT AS_ATT97460488(AS_ATTR_DEF self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT AS_CON306298556(AS_CONST_DEF self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT AS_INC1775598384(AS_INC1416917001 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT AS_ROU123442773(AS_ROUT_DEF self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT AS_SHA297921449(AS_SHARED_DEF self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_AND244040648(AS_AND_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_ANY1960655325(AS_ANY_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_ARR1675309363(AS_ARRAY_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_AT_246103478(AS_AT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_BOO1496152570(AS_BOOL_LIT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_BOU110275841(AS_BOU14269336 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_BRE631040475(AS_BREAK_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_CAL1709269196(AS_CALL_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_CHA1807865070(AS_CHAR_LIT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_CLU1420620999(AS_CLUSTER_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_CLU792721656(AS_CLU553570599 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_COH1869044806(AS_COHORT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_CRE1115272673(AS_CREATE_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_EXC1501278546(AS_EXCEPT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_FAR2096957028(AS_FAR_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_FLT1204717277(AS_FLT_LIT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_GLO1872829932(AS_GLOBAL_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_HER846597914(AS_HERE_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_INI1732476001(AS_INITIAL_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_INT896052382(AS_INT_LIT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_IS_1211675728(AS_IS_VOID_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_NEA160870810(AS_NEAR_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_NEW162891781(AS_NEW_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_OR_261789378(AS_OR_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_RES945429434(AS_RESULT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_SEL1160430562(AS_SELF_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_STR70439144(AS_STR_LIT_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_UND1982396421(AS_UND152986614 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_VOI1856591394(AS_VOID_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR AS_WHE2011036209(AS_WHERE_EXPR self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_ASS255483717(AS_ASSERT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_ASS384416990(AS_ASSIGN_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_ATT428959228(AS_ATTACH_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_CAS568284252(AS_CASE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_DEC1501678601(AS_DEC_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_EXP516982707(AS_EXPR_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_FOR721306794(AS_FORK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_IF_192647666(AS_IF_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_INT1461683196(AS_INT2031239268 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_LOC1353882229(AS_LOCK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_LOO1639447996(AS_LOOP_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_PAR1281243032(AS_PAR_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_PRO1410999444(AS_PROTECT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_QUI758322419(AS_QUIT_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_RAI287018298(AS_RAISE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_RET732932159(AS_RETURN_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_STM1163003696(AS_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_SYN57982213(AS_SYNC_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_TYP1323758084(AS_TYPECASE_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_UNL712558323(AS_UNLOCK_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_WIT1968509975(AS_WIT1063437351 self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT AS_YIE1402665211(AS_YIELD_STMT self) {
 return ATTR(self,next);
}


#undef IS_ITER
#define IS_ITER 0

dMODE MODE_c998271775(MODE self, AS_ARG_MODE as_mode) {
 dMODE ret_val;
 INT L11 = INT_zero;
 L11 = ATTR(as_mode,mod);
 switch (L11) {
  case 1: 
   ret_val = ((dMODE) MODES_in_mode);
   return ret_val;
   break;
  case 2: 
   ret_val = ((dMODE) MODES_out_mode);
   return ret_val;
   break;
  case 3: 
   ret_val = ((dMODE) MODES_inout_mode);
   return ret_val;
   break;
  case 4: 
   ret_val = ((dMODE) MODES_once_mode);
   return ret_val;
   break;
  default: ;
   FATAL("No applicable target in case statement\nin MODE::create_from_as(AS_ARG_MODE):$MODE\n./Representation/mode.sa:55:24");
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_AND1244275737(AS_AND_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_AND1398627607(AS_AND_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_AND1903000239(AS_AND_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ANY1724395066(AS_ANY_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ANY187271084(AS_ANY_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ANY72331114(AS_ANY_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ARG2059479532(AS_ARG_DEC self, AS_ARG_DEC l) {
 AS_ARG_DEC last1;
 if ((ATTR(self,next)==((AS_ARG_DEC) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_ARG_DEC) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ARR1604409854(AS_ARRAY_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ARR471731524(AS_ARRAY_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ARR47654098(AS_ARRAY_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1183239070(AS_ASSIGN_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1330792936(AS_ASSERT_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1461460287(AS_ASSIGN_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1708420135(AS_ASSIGN_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1762623897(AS_ASSIGN_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS1892442692(AS_ASSERT_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS2029454919(AS_ASSERT_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS2107317827(AS_ASSERT_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS537381530(AS_ASSERT_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ASS650357212(AS_ASSIGN_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1135887709(AS_ATTACH_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1141535277(AS_ATTR_DEF self, dAS_CLASS_ELT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1480967150(AS_ATTACH_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1718081659(AS_ATTACH_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1733385743(AS_ATTACH_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT1987206051(AS_ATTR_DEF self, BOOL is_private) {
 SATTR(self,is_private,is_private);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT2024939759(AS_ATTR_DEF self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ATT83117804(AS_ATTACH_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_AT_1901822931(AS_AT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_AT_2092645707(AS_AT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_AT_550257637(AS_AT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOO1855503827(AS_BOOL_LIT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOO203439875(AS_BOOL_LIT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOO650888317(AS_BOOL_LIT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOU2006233068(AS_BOU14269336 self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOU2036765046(AS_BOU14269336 self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BOU354169116(AS_BOU14269336 self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BRE1516885934(AS_BREAK_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BRE679018004(AS_BREAK_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_BRE973045948(AS_BREAK_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAL2065155647(AS_CALL_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAL437771691(AS_CALL_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAL577747697(AS_CALL_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS1011726673(AS_CASE_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS1160155324(AS_CASE_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS1219409394(AS_CASE_WHEN self, AS_CASE_WHEN l) {
 AS_CASE_WHEN last1;
 if ((ATTR(self,next)==((AS_CASE_WHEN) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_CASE_WHEN) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS1578756635(AS_CASE_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS490112615(AS_CASE_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CAS703201798(AS_CASE_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CHA1047100373(AS_CHAR_LIT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CHA340061339(AS_CHAR_LIT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CHA604963579(AS_CHAR_LIT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLA1288691224(AS_CLASS_DEF self, AS_CLASS_DEF l) {
 AS_CLASS_DEF last1;
 if ((ATTR(self,next)==((AS_CLASS_DEF) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_CLASS_DEF) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLA129549718(AS_CLASS_ELT self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU1354319231(AS_CLU553570599 self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU1700532850(AS_CLUSTER_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU1903972459(AS_CLU553570599 self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU726419888(AS_CLUSTER_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU738930885(AS_CLU553570599 self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CLU942370494(AS_CLUSTER_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_COH1216011035(AS_COHORT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_COH278881603(AS_COHORT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_COH436052917(AS_COHORT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CON1753066199(AS_CONST_DEF self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CON1962795038(AS_CONST_DEF self, BOOL is_private) {
 SATTR(self,is_private,is_private);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CON737776233(AS_CONST_DEF self, dAS_CLASS_ELT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CRE1032653736(AS_CREATE_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CRE1964853282(AS_CREATE_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_CRE312789330(AS_CREATE_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_DEC1092435373(AS_DEC_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_DEC1996561535(AS_DEC_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_DEC2062280289(AS_DEC_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_DEC557832566(AS_DEC_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_DEC646247808(AS_DEC_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXC1647038221(AS_EXCEPT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXC5025731(AS_EXCEPT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXC645762341(AS_EXCEPT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP1321747790(AS_EXPR_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP1630058180(AS_EXPR_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP1794823411(AS_EXPR_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP200906119(AS_EXPR_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP2124685825(AS_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_EXP849875946(AS_EXPR_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FAR1938606563(AS_FAR_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FAR286542611(AS_FAR_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FAR50083859(AS_FAR_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FEA814213709(AS_FEAT_MOD self, AS_FEAT_MOD l) {
 AS_FEAT_MOD last1;
 if ((ATTR(self,next)==((AS_FEAT_MOD) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_FEAT_MOD) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FLT1911942022(AS_FLT_LIT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FLT259878070(AS_FLT_LIT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FLT943209132(AS_FLT_LIT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FOR1019127704(AS_FORK_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FOR103324571(AS_FORK_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FOR1425734093(AS_FORK_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FOR1673645269(AS_FORK_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_FOR1753592510(AS_FORK_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_GLO1712525775(AS_GLOBAL_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_GLO275096477(AS_GLOBAL_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_GLO930377569(AS_GLOBAL_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_HER1301328495(AS_HERE_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_HER1479869719(AS_HERE_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_HER172194233(AS_HERE_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IDE1407507383(AS_IDENT_LIST self, AS_IDENT_LIST l) {
 AS_IDENT_LIST last1;
 if ((ATTR(self,next)==((AS_IDENT_LIST) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_IDENT_LIST) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IF_164980229(AS_IF_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IF_1658967836(AS_IF_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IF_1955278743(AS_IF_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IF_671597921(AS_IF_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IF_978670018(AS_IF_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INC1475294123(AS_INC1416917001 self, dAS_CLASS_ELT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INC1807545759(AS_INC1416917001 self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INC874191011(AS_INC1416917001 self, BOOL is_private) {
 SATTR(self,is_private,is_private);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INI1852876520(AS_INITIAL_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INI414564886(AS_INITIAL_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INI790026824(AS_INITIAL_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT1009823030(AS_INT2031239268 self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT1251874027(AS_INT_LIT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT2141902673(AS_INT_LIT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT501000671(AS_INT_LIT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT685357691(AS_INT2031239268 self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT765687160(AS_INT2031239268 self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT83312974(AS_INT2031239268 self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_INT884580779(AS_INT2031239268 self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IS_128975537(AS_IS_VOID_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IS_1523088415(AS_IS_VOID_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_IS_936250681(AS_IS_VOID_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC1027127163(AS_LOCK_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC1037771083(AS_LOCK_IF_WHEN self, AS_LOCK_IF_WHEN l) {
 AS_LOCK_IF_WHEN last1;
 if ((ATTR(self,next)==((AS_LOCK_IF_WHEN) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_LOCK_IF_WHEN) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC1144520043(AS_LOCK_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC623140776(AS_LOCK_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC755705564(AS_LOCK_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOC793158658(AS_LOCK_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOO1017100380(AS_LOOP_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOO1542269018(AS_LOOP_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOO507592891(AS_LOOP_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOO565196687(AS_LOOP_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_LOO633167559(AS_LOOP_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEA1203816563(AS_NEAR_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEA1439086781(AS_NEAR_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEA1986170077(AS_NEAR_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEW1985034628(AS_NEW_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEW2117994542(AS_NEW_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_NEW524908802(AS_NEW_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OR_1885251509(AS_OR_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OR_784015075(AS_OR_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OR_868048877(AS_OR_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR1480073124(AS_PAR_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR170194815(AS_PAR_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR769375844(AS_PARAM_DEC self, AS_PARAM_DEC l) {
 AS_PARAM_DEC last1;
 if ((ATTR(self,next)==((AS_PARAM_DEC) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_PARAM_DEC) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR771744212(AS_PAR_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR865797855(AS_PAR_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PAR99672090(AS_PAR_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO1103329709(AS_PROTECT_WHEN self, AS_PROTECT_WHEN l) {
 AS_PROTECT_WHEN last1;
 if ((ATTR(self,next)==((AS_PROTECT_WHEN) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_PROTECT_WHEN) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO169226468(AS_PROTECT_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO1754180698(AS_PROTECT_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO1882650168(AS_PROTECT_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO232382229(AS_PROTECT_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_PRO736041443(AS_PROTECT_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_QUI1389603990(AS_QUIT_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_QUI1442696788(AS_QUIT_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_QUI1673525760(AS_QUIT_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_QUI2137045949(AS_QUIT_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_QUI971173597(AS_QUIT_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RAI1860022589(AS_RAISE_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RAI2032317945(AS_RAISE_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RAI2115288988(AS_RAISE_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RAI248856852(AS_RAISE_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RAI382050006(AS_RAISE_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RES1196907749(AS_RESULT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RES1201611453(AS_RESULT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RES1445995595(AS_RESULT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RET1042240335(AS_RETURN_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RET1097577252(AS_RETURN_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RET1414994250(AS_RETURN_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RET1547122105(AS_RETURN_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_RET910390234(AS_RETURN_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ROU1496248876(AS_ROUT_DEF self, BOOL is_private) {
 SATTR(self,is_private,is_private);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ROU166638356(AS_ROUT_DEF self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_ROU920632016(AS_ROUT_DEF self, dAS_CLASS_ELT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SEL1190661345(AS_SELF_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SEL1452241999(AS_SELF_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SEL987495847(AS_SELF_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SHA1271069808(AS_SHARED_DEF self, dAS_CLASS_ELT l) {
 dAS_CLASS_ELT last1;
 dAS_CLASS_ELT L1;
 dAS_CLASS_ELT L2;
 dAS_CLASS_ELT L3;
 if ((ATTR(self,next)==((dAS_CLASS_ELT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_CL760354380[TAG(L1)])(L1)==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_CL760354380[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_CL1804429169[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SHA746153340(AS_SHARED_DEF self, dAS_CLASS_ELT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SHA905735878(AS_SHARED_DEF self, BOOL is_private) {
 SATTR(self,is_private,is_private);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STM1692300976(AS_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STM1734576793(AS_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STM193774686(AS_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STM910122564(AS_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STM984922713(AS_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STR1192634823(AS_STR_LIT_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STR2077487265(AS_STR_LIT_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_STR459429129(AS_STR_LIT_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SYN1248387412(AS_SYNC_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SYN1817260677(AS_SYNC_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SYN2089058674(AS_SYNC_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SYN695597675(AS_SYNC_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_SYN954670264(AS_SYNC_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP1027254244(AS_TYPE_SPEC self, AS_TYPE_SPEC l) {
 AS_TYPE_SPEC last1;
 if ((ATTR(self,next)==((AS_TYPE_SPEC) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_TYPE_SPEC) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP1218814867(AS_TYPECASE_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP1661742497(AS_TYPECASE_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP1802831442(AS_TYPECASE_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP1805639138(AS_TYPECASE_WHEN self, AS_TYPECASE_WHEN l) {
 AS_TYPECASE_WHEN last1;
 if ((ATTR(self,next)==((AS_TYPECASE_WHEN) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_TYPECASE_WHEN) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP823282803(AS_TYPECASE_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_TYP982956860(AS_TYPECASE_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UND165529988(AS_UND152986614 self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UND1829175574(AS_UND152986614 self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UND813727770(AS_UND152986614 self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UNL1079389410(AS_UNLOCK_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UNL1435368086(AS_UNLOCK_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UNL1962410248(AS_UNLOCK_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UNL312142309(AS_UNLOCK_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_UNL619513787(AS_UNLOCK_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_VOI1015295839(AS_VOID_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_VOI291335015(AS_VOID_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_VOI636768113(AS_VOID_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WHE136004678(AS_WHERE_EXPR self, dAS_EXPR next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WHE666102736(AS_WHERE_EXPR self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WHE985961216(AS_WHERE_EXPR self, dAS_EXPR l) {
 dAS_EXPR last1;
 dAS_EXPR L1;
 dAS_EXPR L2;
 dAS_EXPR L3;
 if ((ATTR(self,next)==((dAS_EXPR) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_EX337431651[TAG(L1)])(L1)==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_EX337431651[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_EX1809609236[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WIT1613050679(AS_WIT1063437351 self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WIT178530912(AS_WIT1063437351 self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WIT1810155147(AS_WIT1063437351 self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WIT1949427223(AS_WIT1063437351 self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_WIT834544210(AS_WIT1063437351 self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_YIE1194255817(AS_YIELD_STMT self, AS_STMT_LIST surr_stmt_list) {
 SATTR(self,surr_stmt_list,surr_stmt_list);
}


#undef IS_ITER
#define IS_ITER 0

void AS_YIE1468566771(AS_YIELD_STMT self, BOOL transformed) {
 SATTR(self,transformed,transformed);
}


#undef IS_ITER
#define IS_ITER 0

void AS_YIE1941927228(AS_YIELD_STMT self, dAS_STMT l) {
 dAS_STMT last1;
 dAS_STMT L1;
 dAS_STMT L2;
 dAS_STMT L3;
 if ((ATTR(self,next)==((dAS_STMT) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  L1 = last1;
  if (((*dAS_ST460279343[TAG(L1)])(L1)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L2 = last1;
  last1 = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 L3 = last1;
 (*dAS_ST1687647066[TAG(L3)])(L3, l);
}


#undef IS_ITER
#define IS_ITER 0

void AS_YIE291659289(AS_YIELD_STMT self, SFILE_ID source1) {
 SATTR(self,source1,source1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_YIE745261198(AS_YIELD_STMT self, dAS_STMT next) {
 SATTR(self,next,next);
}


#undef IS_ITER
#define IS_ITER 0

void OPT_CS641813603(OPT_CSE self, AM_ROUT_DEF func) {
 AM_CURSOR c;
 FSETCS1561418284 exprs = ((FSETCS1561418284) NULL);
 FLISTT1145236345 replacements = ((FLISTT1145236345) NULL);
 BOOL found_double = BOOL_zero;
 AM_CURSOR L11;
 dAM am;
 CSE_DOUBLE_EXPRS e;
 CSE_DOUBLE_EXPRS ee;
 BOOL done = BOOL_zero;
 FLISTAM_CURSOR L21;
 INT i = INT_zero;
 AM_ASSIGN_STMT as;
 AM_STMT_EXPR st;
 FLISTT1145236345 L31;
 TUPAM_927437481 tp = TUPAM_927437481_zero;
 dAM cur;
 OPT_CSE prog_self;
 PROG ret_val;
 CSE_DOUBLE_EXPRS create_self;
 dAM_EXPR create_e;
 AM_CURSOR create_c;
 CSE_DOUBLE_EXPRS ret_val1;
 CSE_DOUBLE_EXPRS r;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val2;
 FLISTAM_CURSOR aget_self1;
 INT aget_ind1 = INT_zero;
 AM_CURSOR ret_val3;
 FLISTA725283029 aget_self2;
 INT aget_ind2 = INT_zero;
 AM_LOCAL_EXPR ret_val4;
 OPT_CSE new_local_self;
 AM_ROUT_DEF new_local_func;
 dTP new_local_tp;
 SFILE_ID new_local_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val5;
 AM_LOCAL_EXPR lc;
 AM_LOCAL_EXPR create_self1;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val6;
 AM_LOCAL_EXPR r1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 FLISTA725283029 aget_self3;
 INT aget_ind3 = INT_zero;
 AM_LOCAL_EXPR ret_val7;
 AM_ASSIGN_STMT create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val8;
 AM_ASSIGN_STMT r2;
 FLISTAM_CURSOR aget_self4;
 INT aget_ind4 = INT_zero;
 AM_CURSOR ret_val9;
 FLISTdAM_EXPR aget_self5;
 INT aget_ind5 = INT_zero;
 dAM_EXPR ret_val10;
 FLISTA725283029 aget_self6;
 INT aget_ind6 = INT_zero;
 AM_LOCAL_EXPR ret_val11;
 FLISTAM_CURSOR aget_self7;
 INT aget_ind7 = INT_zero;
 AM_CURSOR ret_val12;
 FLISTA725283029 aget_self8;
 INT aget_ind8 = INT_zero;
 AM_LOCAL_EXPR ret_val13;
 TUPAM_927437481 create_self3 = TUPAM_927437481_zero;
 AM_CURSOR create_at1;
 dAM_EXPR create_at2;
 dAM_STMT create_at3;
 TUPAM_927437481 ret_val14 = TUPAM_927437481_zero;
 FLISTA725283029 aget_self9;
 INT aget_ind9 = INT_zero;
 AM_LOCAL_EXPR ret_val15;
 FLISTdAM_EXPR aget_self10;
 INT aget_ind10 = INT_zero;
 dAM_EXPR ret_val16;
 FLISTA725283029 aget_self11;
 INT aget_ind11 = INT_zero;
 AM_LOCAL_EXPR ret_val17;
 AM_STMT_EXPR create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val18;
 AM_STMT_EXPR r3;
 FLISTA725283029 aget_self12;
 INT aget_ind12 = INT_zero;
 AM_LOCAL_EXPR ret_val19;
 FLISTAM_CURSOR aget_self13;
 INT aget_ind13 = INT_zero;
 AM_CURSOR ret_val20;
 TUPAM_927437481 create_self5 = TUPAM_927437481_zero;
 AM_CURSOR create_at11;
 dAM_EXPR create_at21;
 dAM_STMT create_at31;
 TUPAM_927437481 ret_val21 = TUPAM_927437481_zero;
 OPT_CSE prog_self1;
 PROG ret_val22;
 FLISTA725283029 aget_self14;
 INT aget_ind14 = INT_zero;
 AM_LOCAL_EXPR ret_val23;
 TUPAM_927437481 create_self6 = TUPAM_927437481_zero;
 AM_CURSOR create_at12;
 dAM_EXPR create_at22;
 dAM_STMT create_at32;
 TUPAM_927437481 ret_val24 = TUPAM_927437481_zero;
 CSE_DOUBLE_EXPRS add_loc_self;
 dAM_EXPR add_loc_e;
 AM_CURSOR add_loc_c;
 dAM L4;
 BOOL L5;
 dAM_EXPR L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 CSE_DOUBLE_EXPRS L12;
 OB L13;
 FLISTAM_CURSOR L14;
 BOOL L15;
 INT L16;
 BOOL L17;
 dAM_EXPR L181_;
 dAM_EXPR L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 AM_CURSOR L281_;
 AM_LOCAL_EXPR L291_;
 dAM_EXPR L30;
 dAM_EXPR L32;
 AM_LOCAL_EXPR L33;
 OB L34;
 AM_LOCAL_EXPR L361_;
 AM_LOCAL_EXPR L37;
 AM_ASSIGN_STMT L38;
 OB L39;
 AM_CURSOR L401_;
 dAM_EXPR L411_;
 AM_LOCAL_EXPR L421_;
 AM_CURSOR L431_;
 AM_LOCAL_EXPR L441_;
 TUPAM_927437481 L45;
 TUPAM_927437481 L46;
 TUPAM_927437481 L47;
 AM_LOCAL_EXPR L481_;
 dAM_EXPR L491_;
 AM_LOCAL_EXPR L501_;
 AM_LOCAL_EXPR L51;
 AM_STMT_EXPR L52;
 OB L53;
 AM_LOCAL_EXPR L541_;
 AM_CURSOR L551_;
 TUPAM_927437481 L56;
 TUPAM_927437481 L57;
 TUPAM_927437481 L58;
 PROG L59;
 extern STR Oofcse;
 dSTAT L60;
 AM_LOCAL_EXPR L611_;
 TUPAM_927437481 L62;
 TUPAM_927437481 L63;
 TUPAM_927437481 L64;
 BOOL L651_;
 FLISTAM_CURSOR L66;
 BOOL L67;
 BOOL L681_;
 TUPAM_927437481 L69;
 BOOL L70;
 BOOL L711_;
 dAM_STMT L72;
 dAM_STMT L73;
 dAM_STMT L74;
 dAM_STMT L75;
 BOOL L77;
 BOOL L781_;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, ((dAM) ATTR(func,code1)));
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 SATTR(c,assign_in_order,TRUE);
 found_double = FALSE;
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
   am = L4;
   if (am==NULL) {
    goto other263;
   } else
   switch (TAG(am)) {
    case AM_LOCAL_EXPR_tag: break;
    case AM_SHARED_EXPR_tag: break;
    case AM_CLUSTER_EXPR_tag: break;
    case AM_HERE_EXPR_tag: break;
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
    case AM_ANY_EXPR_tag:
    case AM_ARRAY_EXPR_tag:
    case AM_ATTR_EXPR_tag:
    case AM_AT_EXPR_tag:
    case AM_BND1124877163_tag:
    case AM_BND367211769_tag:
    case AM_BND260301329_tag:
    case AM_CLU1286269335_tag:
    case AM_EXCEPT_EXPR_tag:
    case AM_EXT_CALL_EXPR_tag:
    case AM_FAR_EXPR_tag:
    case AM_GLOBAL_EXPR_tag:
    case AM_IF_EXPR_tag:
    case AM_IS_VOID_EXPR_tag:
    case AM_ITE1809135850_tag:
    case AM_NEAR_EXPR_tag:
    case AM_NEW_EXPR_tag:
    case AM_ROU1916046290_tag:
    case AM_STMT_EXPR_tag:
    case AM_VAR744470097_tag:
    case AM_VAT319982528_tag:
    case AM_WHERE_EXPR_tag:
     L6 = ((dAM_EXPR) am);
     L7 = ((*dAM_EXPR_tprdTP[TAG(L6)])(L6)==((dTP) NULL));
     L81_=!(L7); 
     if (L81_) {
      L9 = AM_CUR2013152412(c);
      L101_=!(L9); 
      L5 = L101_;
     } else {
      L5 = FALSE;
     }
     if (L5) {
      create_self = ((CSE_DOUBLE_EXPRS) NULL);
      create_e = ((dAM_EXPR) am);
      create_c = c;
      L13=ZALLOC(sizeof(struct CSE_DOUBLE_EXPRS_struct));
      if (L13==NULL) FATAL("Unable to allocate more memory");
      ((OB)L13)->header.tag=CSE_DOUBLE_EXPRS_tag;
      L12 = ((CSE_DOUBLE_EXPRS) L13);
      r = L12;
      SATTR(r,expr,FLISTd805769956(ATTR(r,expr), create_e));
      L14 = ATTR(r,pos);
      SATTR(r,pos,FLISTA122884859(L14, AM_CUR1358945253(((AM_CURSOR) NULL), create_c)));
      SATTR(r,locals1,FLISTA1817671564(ATTR(r,locals1), ((AM_LOCAL_EXPR) NULL)));
      ret_val1 = r;
      e = ret_val1;
      ee = FSETCS1814959(exprs, e);
      if ((ee==((CSE_DOUBLE_EXPRS) NULL))) {
       L15 = TRUE;
      } else {
       L15 = AM_CUR1481630356(c);
      }
      if (L15) {
       exprs = FSETCS1791208015(exprs, e);
      }
      else {
       e = ee;
       done = FALSE;
       {
        struct FLISTA1878650141_frame_struct other2_0;
FLISTA1878650141_frame noname2 = &other2_0;
        L21 = ATTR(e,pos);
        noname2->self = L21;
        noname2->state = 0;
        while (1) {
         L16 = FLISTA1878650141(noname2);
         if (noname2->state == -1) {
          goto after_loop1;
         }
         i = L16;
         aget_self = ATTR(e,expr);
         aget_ind = i;
         L181_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
         ret_val2 = L181_;
         L19 = ret_val2;
         L201_=SYSID(L19); 
         L22 = L201_;
         L231_=SYSID(am); 
         L24 = L231_;
         L251_=(L22)==(L24); 
         L26 = L251_;
         L271_=!(L26); 
         if (L271_) {
          aget_self1 = ATTR(e,pos);
          aget_ind1 = i;
          L281_=(AM_CURSOR)ARR((FLISTAM_CURSOR)aget_self1,aget_ind1); 
          ret_val3 = L281_;
          L17 = AM_CUR830576175(c, ret_val3, am);
         } else {
          L17 = FALSE;
         }
         if (L17) {
          aget_self2 = ATTR(e,locals1);
          aget_ind2 = i;
          L291_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self2,aget_ind2); 
          ret_val4 = L291_;
          if ((ret_val4==((AM_LOCAL_EXPR) NULL))) {
           aset_self = ATTR(e,locals1);
           aset_ind = i;
           new_local_self = self;
           new_local_func = func;
           L30 = ((dAM_EXPR) am);
           new_local_tp = (*dAM_EXPR_tprdTP[TAG(L30)])(L30);
           L32 = ((dAM_EXPR) am);
           new_local_source = (*dAM_EX2051891691[TAG(L32)])(L32);
           create_self1 = ((AM_LOCAL_EXPR) NULL);
           create_src = new_local_source;
           create_name = IDENT_zero;
           create_tp = new_local_tp;
           L34=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
           if (L34==NULL) FATAL("Unable to allocate more memory");
           ((OB)L34)->header.tag=AM_LOCAL_EXPR_tag;
           L33 = ((AM_LOCAL_EXPR) L34);
           r1 = L33;
           SATTR(r1,source1,create_src);
           SATTR(r1,name1,create_name);
           SATTR(r1,tp_at,create_tp);
           ret_val6 = r1;
           lc = ret_val6;
           SATTR(new_local_func,locals1,FLISTA1817671564(ATTR(new_local_func,locals1), lc));
           ret_val5 = lc;
           aset_val = ret_val5;
           SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
           ;
           create_self2 = ((AM_ASSIGN_STMT) NULL);
           aget_self3 = ATTR(e,locals1);
           aget_ind3 = i;
           L361_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self3,aget_ind3); 
           ret_val7 = L361_;
           L37=ret_val7;
           create_source = ATTR(L37,source1);
           L39=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
           if (L39==NULL) FATAL("Unable to allocate more memory");
           ((OB)L39)->header.tag=AM_ASSIGN_STMT_tag;
           L38 = ((AM_ASSIGN_STMT) L39);
           r2 = L38;
           SATTR(r2,source1,create_source);
           ret_val8 = r2;
           as = ret_val8;
           aget_self4 = ATTR(e,pos);
           aget_ind4 = i;
           L401_=(AM_CURSOR)ARR((FLISTAM_CURSOR)aget_self4,aget_ind4); 
           ret_val9 = L401_;
           if (AM_CUR1481630356(ret_val9)) {
            aget_self5 = ATTR(e,expr);
            aget_ind5 = i;
            L411_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self5,aget_ind5); 
            ret_val10 = L411_;
            SATTR(as,dest,ret_val10);
            aget_self6 = ATTR(e,locals1);
            aget_ind6 = i;
            L421_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self6,aget_ind6); 
            ret_val11 = L421_;
            SATTR(as,src1,((dAM_EXPR) ret_val11));
            create_self3 = TUPAM_927437481_zero;
            aget_self7 = ATTR(e,pos);
            aget_ind7 = i;
            L431_=(AM_CURSOR)ARR((FLISTAM_CURSOR)aget_self7,aget_ind7); 
            ret_val12 = L431_;
            create_at1 = ret_val12;
            aget_self8 = ATTR(e,locals1);
            aget_ind8 = i;
            L441_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self8,aget_ind8); 
            ret_val13 = L441_;
            create_at2 = ((dAM_EXPR) ret_val13);
            create_at3 = ((dAM_STMT) as);
            L45 = create_self3;
            L45.t1 = create_at1;
            L46 = L45;
            L46.t2 = create_at2;
            L47 = L46;
            L47.t3 = create_at3;
            ret_val14 = L47;
            replacements = FLISTT596895593(replacements, ret_val14);
           }
           else {
            aget_self9 = ATTR(e,locals1);
            aget_ind9 = i;
            L481_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self9,aget_ind9); 
            ret_val15 = L481_;
            SATTR(as,dest,((dAM_EXPR) ret_val15));
            aget_self10 = ATTR(e,expr);
            aget_ind10 = i;
            L491_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self10,aget_ind10); 
            ret_val16 = L491_;
            SATTR(as,src1,ret_val16);
            create_self4 = ((AM_STMT_EXPR) NULL);
            aget_self11 = ATTR(e,locals1);
            aget_ind11 = i;
            L501_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self11,aget_ind11); 
            ret_val17 = L501_;
            L51=ret_val17;
            create_source1 = ATTR(L51,source1);
            L53=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
            if (L53==NULL) FATAL("Unable to allocate more memory");
            ((OB)L53)->header.tag=AM_STMT_EXPR_tag;
            L52 = ((AM_STMT_EXPR) L53);
            r3 = L52;
            SATTR(r3,source1,create_source1);
            ret_val18 = r3;
            st = ret_val18;
            SATTR(st,stmts,((dAM_STMT) as));
            aget_self12 = ATTR(e,locals1);
            aget_ind12 = i;
            L541_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self12,aget_ind12); 
            ret_val19 = L541_;
            SATTR(st,expr,((dAM_EXPR) ret_val19));
            create_self5 = TUPAM_927437481_zero;
            aget_self13 = ATTR(e,pos);
            aget_ind13 = i;
            L551_=(AM_CURSOR)ARR((FLISTAM_CURSOR)aget_self13,aget_ind13); 
            ret_val20 = L551_;
            create_at11 = ret_val20;
            create_at21 = ((dAM_EXPR) st);
            create_at31 = ((dAM_STMT) NULL);
            L56 = create_self5;
            L56.t1 = create_at11;
            L57 = L56;
            L57.t2 = create_at21;
            L58 = L57;
            L58.t3 = create_at31;
            ret_val21 = L58;
            replacements = FLISTT596895593(replacements, ret_val21);
           }
          }
          prog_self1 = self;
          ret_val22 = OPT_CO233462019;
          L59=ret_val22;
          L60 = ATTR(L59,stat1);
          (*dSTAT_incr_STR[TAG(L60)])(L60, ((STR) &Oofcse));
          create_self6 = TUPAM_927437481_zero;
          create_at12 = AM_CUR1358945253(((AM_CURSOR) NULL), c);
          aget_self14 = ATTR(e,locals1);
          aget_ind14 = i;
          L611_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self14,aget_ind14); 
          ret_val23 = L611_;
          create_at22 = ((dAM_EXPR) ret_val23);
          create_at32 = ((dAM_STMT) NULL);
          L62 = create_self6;
          L62.t1 = create_at12;
          L63 = L62;
          L63.t2 = create_at22;
          L64 = L63;
          L64.t3 = create_at32;
          ret_val24 = L64;
          replacements = FLISTT596895593(replacements, ret_val24);
          done = TRUE;
          goto after_loop1;
         }
        }
       }
       after_loop1: ;
       L651_=!(done); 
       if (L651_) {
        add_loc_self = e;
        add_loc_e = ((dAM_EXPR) am);
        add_loc_c = c;
        SATTR(add_loc_self,expr,FLISTd805769956(ATTR(add_loc_self,expr), add_loc_e));
        L66 = ATTR(add_loc_self,pos);
        SATTR(add_loc_self,pos,FLISTA122884859(L66, AM_CUR1358945253(((AM_CURSOR) NULL), add_loc_c)));
        SATTR(add_loc_self,locals1,FLISTA1817671564(ATTR(add_loc_self,locals1), ((AM_LOCAL_EXPR) NULL)));
       }
      }
     } break;
    default: ;
    other263: ;
   }
  }
 }
 after_loop: ;
 L67 = (replacements==((FLISTT1145236345) NULL));
 L681_=!(L67); 
 if (L681_) {
  {
   struct FLISTT709128269_frame_struct other3_0;
FLISTT709128269_frame noname3 = &other3_0;
   L31 = replacements;
   noname3->self = L31;
   noname3->state = 0;
   while (1) {
    L69 = FLISTT709128269(noname3);
    if (noname3->state == -1) {
     goto after_loop2;
    }
    tp = L69;
    AM_CUR915996644(tp.t1, tp.t2);
    L70 = (tp.t3==((dAM_STMT) NULL));
    L711_=!(L70); 
    if (L711_) {
     AM_CUR1769624888(tp.t1);
     cur = ATTR(tp.t1,cur);
     switch (TAG(cur)) {
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
       L72 = tp.t3;
       L73 = ((dAM_STMT) cur);
       L74 = L72;
       (*dAM_ST1372701974[TAG(L74)])(L74, (*dAM_ST775224435[TAG(L73)])(L73));
       L75 = ((dAM_STMT) cur);
       (*dAM_ST1372701974[TAG(L75)])(L75, tp.t3); break;
      default: ;
       FATAL("No applicable type in typecase\nin OPT_CSE::remove_cse(AM_ROUT_DEF)\n./Back/o_cse.sa:141:23");
     }
    }
   }
  }
  after_loop2: ;
  SYSDESTROY(replacements); 
  ;
 }
 L77 = (exprs==((FSETCS1561418284) NULL));
 L781_=!(L77); 
 if (L781_) {
  SYSDESTROY(exprs); 
  ;
 }
}
