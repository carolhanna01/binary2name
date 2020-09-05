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

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYd1329429954_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1329429954_frame;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

typedef struct CONFIG_ROUT_struct {/* layout for CONFIG_ROUT */
 OB_HEADER header;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *near_depends_on;
 struct ARRAYSTR_struct *break1;
 struct ARRAYSTR_struct *declare;
 struct ARRAYSTR_struct *exec;
 struct ARRAYSTR_struct *f_break;
 struct ARRAYSTR_struct *f_declare;
 struct ARRAYSTR_struct *f_exec;
 struct ARRAYSTR_struct *f_init;
 struct ARRAYSTR_struct *f_iter;
 struct ARRAYSTR_struct *f_temp;
 struct ARRAYSTR_struct *f_var;
 struct ARRAYSTR_struct *init;
 struct ARRAYSTR_struct *iter;
 struct ARRAYSTR_struct *raises;
 struct ARRAYSTR_struct *reads;
 struct ARRAYSTR_struct *temp1;
 struct ARRAYSTR_struct *var;
 struct ARRAYSTR_struct *writes;
 STR name1;
 BOOL arith;
 BOOL block1;
 BOOL does_export;
 BOOL does_import;
 BOOL fragile;
 BOOL is_near;
 BOOL no_post;
 BOOL no_pre;
 BOOL raises_any;
 BOOL reads_any;
 BOOL use_index;
 BOOL volatile1;
 BOOL writes_any;
 } *CONFIG_ROUT;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_EQ947319992_struct {/* layout for ELT_EQ{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ947319992;

typedef struct ELT_EQ826090414_struct {/* layout for ELT_EQ{BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ826090414;

typedef struct ELT_EQ960775407_struct {/* layout for ELT_EQ{BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ960775407;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct IMPL_struct {/* layout for IMPL */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ELT_TBL_struct *elts;
 struct IFC_struct *ifc;
 struct TP_CLASS_struct *arr;
 BOOL is_ref_cache;
 BOOL use_ref_cache;
 } *IMPL;

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

typedef struct INTI1_struct {/* layout for INTI */
 OB_HEADER header;
 INT len1;
 INT asize;
 INT arr_part[1];
 } *INTI1;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

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

typedef struct TP_CLASS_struct {/* layout for TP_CLASS */
 OB_HEADER header;
 struct ARRAYdTP_struct *params;
 struct AS_CLASS_DEF_struct *my_as_tree;
 struct IDENT_struct name1;
 struct IFC_struct *my_ifc;
 struct IMPL_struct *my_impl;
 INT kind_cache;
 struct PROG_struct *prog;
 STR sas_cache;
 struct TP_CONTEXT_struct *my_context;
 BOOL is_atomic;
 BOOL is_builtin;
 BOOL is_dummy_class;
 BOOL is_generic;
 } *TP_CLASS;

typedef struct TP_CLASS_TBL_struct {/* layout for TP_CLASS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_CLASS_TBL;

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

typedef struct TP_TBL_struct {/* layout for TP_TBL */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CLASS_TBL_struct *class_tbl;
 struct TP_CLASS_TBL_struct *included_classes;
 struct TP_GEN_TBL_struct *gen_tbl;
 struct TP_ITER_TBL_struct *iter_tbl;
 struct TP_ROUT_TBL_struct *f_rout_tbl;
 struct TP_ROUT_TBL_struct *rout_tbl;
 } *TP_TBL;

typedef struct TRANS_struct {/* layout for TRANS */
 OB_HEADER header;
 struct dAM_STMT_struct *cur_lock;
 struct dAM_STMT_struct *init_stmts;
 struct dTP_struct *ex_tp;
 struct AM_INV1167837230_struct *inv_stmt;
 struct AM_LOCAL_EXPR_struct *cur_cohort;
 struct AM_LOCAL_EXPR_struct *cur_par_ob;
 struct AM_LOCAL_EXPR_struct *cur_param_ob;
 struct AM_LOOP_STMT_struct *cur_loop;
 struct AM_PAR_STMT_struct *cur_simple_par;
 struct AM_POST_STMT_struct *post_stmt;
 struct AM_ROUT_DEF_struct *cur_rout;
 struct FLISTA725283029_struct *active_locals;
 struct IMPL_struct *impl;
 INT cur_yield_ind;
 struct PROG_struct *prog;
 struct SE_CONTEXT_struct *cur_se;
 struct TP_CONTEXT_struct *tp_con;
 BOOL chk_assert;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL in_constant;
 BOOL in_external;
 BOOL in_initial;
 BOOL in_invariant;
 BOOL in_post;
 BOOL in_pre;
 BOOL in_protect_body;
 BOOL in_protect_but_n;
 BOOL in_protect_then;
 BOOL in_simple_par;
 BOOL is_in_par;
 } *TRANS;

typedef struct TUPAM_304458649_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1007407331_struct *t2;
 } TUPAM_304458649;
static TUPAM_304458649 TUPAM_304458649_zero;

typedef struct TUPAM_304458649_boxed_struct {
 OB_HEADER header;
 TUPAM_304458649 immutable_part;
 } *TUPAM_304458649_boxed;

typedef struct TUPAM_667892060_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1677815802_struct *t2;
 } TUPAM_667892060;
static TUPAM_667892060 TUPAM_667892060_zero;

typedef struct TUPAM_667892060_boxed_struct {
 OB_HEADER header;
 TUPAM_667892060 immutable_part;
 } *TUPAM_667892060_boxed;

typedef struct TUPIDENTARRAYdTP_struct {/* layout for TUP{IDENT,ARRAY{$TP}} */
 struct ARRAYdTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTARRAYdTP;
static TUPIDENTARRAYdTP TUPIDENTARRAYdTP_zero;

typedef struct TUPIDENTARRAYdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTARRAYdTP immutable_part;
 } *TUPIDENTARRAYdTP_boxed;

typedef struct AM_ATTACH_STMT_struct {/* layout for AM_ATTACH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *gate;
 struct AM_LOCAL_EXPR_struct *helper;
 struct SFILE_ID_struct source1;
 struct SIG_struct *rout;
 } *AM_ATTACH_STMT;

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

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

typedef struct AM_INV1167837230_struct {/* layout for AM_INVARIANT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 } *AM_INV1167837230;

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

typedef struct AM_PRE_STMT_struct {/* layout for AM_PRE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_PRE_STMT;

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

typedef struct AM_SYNC_STMT_struct {/* layout for AM_SYNC_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_SYNC_STMT;

typedef struct AM_UNLOCK_STMT_struct {/* layout for AM_UNLOCK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *lock_ob;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_UNLOCK_STMT;

typedef struct AM_WIT1996971603_struct {/* layout for AM_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_part;
 struct dAM_STMT_struct *near_part;
 struct dAM_STMT_struct *next;
 struct ARRAYdAM_EXPR_struct *objects1;
 struct SFILE_ID_struct source1;
 } *AM_WIT1996971603;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct AS_EXPR_STMT_struct {/* layout for AS_EXPR_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_EXPR_STMT;

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

typedef struct AS_STMT_LIST_struct {/* layout for AS_STMT_LIST */
 OB_HEADER header;
 struct dAS_STMT_struct *stmts;
 struct SFILE_ID_struct source1;
 } *AS_STMT_LIST;

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

typedef struct AS_UNLOCK_STMT_struct {/* layout for AS_UNLOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_UNLOCK_STMT;

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

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

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

typedef struct TRANS_1349638895_frame_struct {
 INT state;
 TRANS self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val;
 AM_ROUT_DEF L81;
 INT L91;
 FLISTA725283029 L151;
 AM_LOCAL_EXPR loc;
 INT L51;
 BOOL f_L61_; /* used by builtin iter */
 INT L61_c,L61_s; /* used by builtin iter */
 INT i_L61_; /* used by builtin iter */
 struct FLISTA2119642552_frame_struct *nested2; /* nested iter frame */
 } *TRANS_1349638895_frame;

#include "tags.h"

/* Globals */

extern BOOL TRANS_1975351488;
extern BOOL TRANS_2136297923;
extern IDENT IDENT_1909070998;
extern IDENT IDENT_2063376192;
extern IDENT IDENT_985337352;
extern INT AS_ARG1300937332;
extern INT IDENT_tmp_count;
extern INT TP_KIND_ref_tp;
extern INT TP_KIND_spr_tp;
extern INT TRANS_1924362320;
extern INT TRANS_194341863;
extern INT TRANS_att_code;
extern INT TRANS_frk_code;
extern INT TRANS_par_code;
extern TP_CLASS TP_BUI1813155122;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_zone;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST AS_STMT_LIST (**dAS_ST445865822)(dAS_STMT);

extern RETURNED_CONST AS_TYPE_SPEC (**dTP_as1220129267)(dTP);

extern RETURNED_CONST BOOL (**dAS_ST1616688325)(dAS_STMT);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST SFILE_ID (**dAS_ST957383256)(dAS_STMT);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAS_ST1454156204)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dAS_ST1569642444)(dAS_STMT, BOOL);

extern RETURNED_CONST void (**dAS_ST1687647066)(dAS_STMT, dAS_STMT);
AM_LOCAL_EXPR FLISTA912130454(FLISTA725283029);
AM_LOCAL_EXPR TRANS_1349638895(TRANS_1349638895_frame);
AM_LOCAL_EXPR TRANS_1496987093(TRANS, dAS_STMT, IDENT, IDENT, AS_CLASS_DEF, ARRAYdTP);
AM_LOCAL_EXPR TRANS_1711874080(TRANS, IDENT);
AM_ROUT_DEF TRANS_1291044671(TRANS, ELT, AS_SHARED_DEF);
AM_ROUT_DEF TRANS_1584821941(TRANS, ELT, AS_ATTR_DEF);
AM_ROUT_DEF TRANS_1746053264(TRANS, ELT, AS_CONST_DEF);
AM_ROUT_DEF TRANS_281283203(TRANS, ELT, AS_ROUT_DEF);
AM_ROUT_DEF TRANS_503579921(TRANS, ELT);
AS_ASSIGN_STMT TRANS_1390141743(TRANS, IDENT, IDENT, AS_CLASS_DEF, AS_STMT_LIST, SFILE_ID);
AS_ASSIGN_STMT TRANS_2064676398(TRANS, IDENT, AS_STMT_LIST, SFILE_ID);
AS_CALL_EXPR TRANS_1849744987(TRANS, IDENT, AM_LOCAL_EXPR, SFILE_ID);
AS_ROUT_DEF TRANS_725662504(TRANS, IDENT, IDENT, IDENT, AM_LOCAL_EXPR, AS_CLASS_DEF, TP_CLASS, IDENT, AS_TYPE_SPEC, AS_STMT_LIST, INT, SFILE_ID);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA513015767(TP_CLASS, dCALL_TP);
BOOL TRANS_1229338887(TRANS);
BOOL TRANS_157729662(TRANS, IDENT, AM_LOCAL_EXPR);
BOOL TRANS_1615967248(TRANS, AS_FORK_STMT);
BOOL TRANS_1865855576(TRANS, dAM_EXPR);
BOOL TRANS_2091440962(TRANS);
BOOL TRANS_309326235(TRANS, dTP);
BOOL TRANS_883636795(TRANS, AS_PAR_STMT);
BOOL TUPAM_1278179299(TUPAM_667892060, TUPAM_667892060);
BOOL TUPAM_2013149714(TUPAM_304458649, OB);
BOOL TUPAM_2045709861(TUPAM_667892060, OB);
BOOL TUPAM_540366523(TUPAM_304458649, TUPAM_304458649);
FLISTA1327600838 FLISTA475813755(FLISTA1327600838, ARRAYdAM_EXPR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT AS_ARG1759480338(AS_ARG_DEC);
INT AS_LOC1840648359(AS_LOCK_IF_WHEN);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FLISTA1151473122(FLISTA725283029);
INT INT_upbrINT(INT_upbrINT_frame);
INT TUPAM_1877096499(TUPAM_304458649);
INT TUPAM_460226510(TUPAM_667892060);
INTI1 INTI_c1588968505(INTI1, INT);
IN_MODE IN_MOD403789248(IN_MODE);
SIG IMPL_i1943552357(IMPL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR TUPAM_1128362628(TUPAM_304458649);
STR TUPAM_827739609(TUPAM_667892060);
TP_CLASS TP_CLA420352644(TP_CLASS_TBL, TUPIDENTARRAYdTP);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
TRANS TRANS_214014142(TRANS, ELT);
dAM_EXPR TRANS_1705639890(TRANS, AS_CALL_EXPR, dTP, BOOL);
dAM_EXPR TRANS_693526804(TRANS, dAS_EXPR, dTP);
dAM_STMT TRANS_1055378524(TRANS, AS_SYNC_STMT);
dAM_STMT TRANS_1066292461(TRANS, AS_YIELD_STMT);
dAM_STMT TRANS_1157876950(TRANS, AS_ASSIGN_STMT);
dAM_STMT TRANS_1226584966(TRANS, AS_ASSERT_STMT);
dAM_STMT TRANS_1261916410(TRANS, AS_IF_STMT);
dAM_STMT TRANS_1403806496(TRANS, AS_FORK_STMT);
dAM_STMT TRANS_1563748848(TRANS, AS_STMT_LIST);
dAM_STMT TRANS_1573138211(TRANS, AS_DEC_STMT);
dAM_STMT TRANS_1603674908(TRANS, AS_LOCK_STMT);
dAM_STMT TRANS_17409519(TRANS, AS_INT2031239268);
dAM_STMT TRANS_1762825757(TRANS, AS_RAISE_STMT);
dAM_STMT TRANS_1801074742(TRANS, dAS_STMT);
dAM_STMT TRANS_1847483442(TRANS, AS_RETURN_STMT);
dAM_STMT TRANS_1853773828(TRANS, AS_QUIT_STMT);
dAM_STMT TRANS_1999552456(TRANS, AS_CASE_STMT);
dAM_STMT TRANS_2091674770(TRANS, AS_ATTACH_STMT);
dAM_STMT TRANS_338440751(TRANS, AS_WIT1063437351);
dAM_STMT TRANS_342518622(TRANS, AS_UNLOCK_STMT);
dAM_STMT TRANS_565673800(TRANS, AS_LOOP_STMT);
dAM_STMT TRANS_637628566(TRANS, dAS_STMT);
dAM_STMT TRANS_741871011(TRANS, AS_PROTECT_STMT);
dAM_STMT TRANS_747994499(TRANS, AS_PAR_STMT);
dAM_STMT TRANS_774746956(TRANS, AS_TYPECASE_STMT);
dAM_STMT TRANS_905208628(TRANS, AS_EXPR_STMT);
dAS_STMT TRANS_120708644(TRANS, INT, AS_STMT_LIST, SFILE_ID);
dAS_STMT TRANS_166911016(TRANS, INT, AS_STMT_LIST, SFILE_ID);
dAS_STMT TRANS_954470687(TRANS, dAS_STMT);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void ARRAYd1329429954(ARRAYd1329429954_frame);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_message_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);
void SE_CON412941612(SE_CONTEXT, dAM);
void SE_CON621544985(SE_CONTEXT);
void TRANS_1069298803(TRANS, TP_CLASS, AS_ROUT_DEF);
void TRANS_1456624412(TRANS, dAS_STMT);
void TRANS_673475073(TRANS, AS_ROUT_DEF);
void TRANS_75192950(TRANS, AS_ROUT_DEF);
void TRANS_866431519(TRANS, AS_STMT_LIST, BOOL);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF TRANS_281283203(TRANS self, ELT e, AS_ROUT_DEF as) {
 AM_ROUT_DEF ret_val;
 AM_ROUT_DEF r;
 AM_LOCAL_EXPR sl;
 INT i = INT_zero;
 AS_ARG_DEC na;
 AM_LOCAL_EXPR l;
 AM_PRE_STMT pres = ((AM_PRE_STMT) NULL);
 SIG isig;
 AM_ROU1916046290 icall;
 dAM_STMT code11 = ((dAM_STMT) NULL);
 ELT is_invariant_sel;
 BOOL ret_val1 = BOOL_zero;
 SIG is_invariant_sel1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT tp_self;
 dTP ret_val5;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 AM_ROUT_DEF create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROUT_DEF ret_val8;
 AM_ROUT_DEF r1;
 ELT tp_self1;
 dTP ret_val9;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val10;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self1;
 AS_TYPE_SPEC ret_val11;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val12;
 AM_LOCAL_EXPR r2;
 AM_FORMAL_ARG create_self3;
 AM_LOCAL_EXPR create_e;
 AM_FORMAL_ARG ret_val13;
 AM_FORMAL_ARG res1;
 SIG has_ret_self;
 BOOL ret_val14 = BOOL_zero;
 ELT ret_self;
 dTP ret_val15;
 AM_LOCAL_EXPR create_self4;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AM_LOCAL_EXPR ret_val16;
 AM_LOCAL_EXPR r3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val17;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val18;
 AM_LOCAL_EXPR create_self5;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name2 = IDENT_zero;
 dTP create_tp2;
 AS_TYPE_SPEC create_as_tp1;
 AM_LOCAL_EXPR ret_val19;
 AM_LOCAL_EXPR r4;
 AM_FORMAL_ARG create_self6;
 AM_LOCAL_EXPR create_e1;
 dMODE create_m;
 AM_FORMAL_ARG ret_val20;
 AM_FORMAL_ARG res2;
 PROG psather_self;
 BOOL ret_val21 = BOOL_zero;
 AM_PRE_STMT create_self7;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_PRE_STMT ret_val22;
 AM_PRE_STMT r5;
 AM_POST_STMT create_self8;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_POST_STMT ret_val23;
 AM_POST_STMT r6;
 AM_INV1167837230 create_self9;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_INV1167837230 ret_val24;
 AM_INV1167837230 r7;
 AM_ROU1916046290 create_self10;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val25;
 AM_ROU1916046290 r8;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 dTP L7;
 extern STR Compil1234648972;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 ARRAYARG L10;
 INT L111_;
 INT L12;
 INT L131_;
 AM_ROUT_DEF L14;
 INT L15;
 OB L16;
 INT L17;
 BOOL L18;
 BOOL L191_;
 AS_TYPE_SPEC L20;
 OB L21;
 extern STR INT1;
 AM_LOCAL_EXPR L22;
 OB L23;
 AM_FORMAL_ARG L24;
 OB L25;
 AM_FORMAL_ARG L26;
 BOOL L28;
 BOOL L291_;
 AM_LOCAL_EXPR L30;
 OB L31;
 INT L32;
 ARRAYARG L33;
 INT L341_;
 INT L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 extern STR Compil564774712;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 ARRAYARG L41;
 INT L421_;
 INT L43;
 BOOL L441_;
 ARRAYARG L45;
 ARG L461_;
 ARG L47;
 AM_LOCAL_EXPR L48;
 OB L49;
 INT L501_;
 INT L52;
 AM_FORMAL_ARG L53;
 OB L54;
 AM_FORMAL_ARG L55;
 INT L571_;
 BOOL L58;
 BOOL L59;
 BOOL L601_;
 BOOL L61;
 BOOL L62;
 BOOL L631_;
 AM_PRE_STMT L64;
 OB L65;
 BOOL L66;
 BOOL L671_;
 BOOL L68;
 BOOL L69;
 BOOL L701_;
 AM_POST_STMT L71;
 OB L72;
 BOOL L73;
 BOOL L74;
 BOOL L751_;
 BOOL L76;
 BOOL L771_;
 BOOL L78;
 BOOL L791_;
 AM_INV1167837230 L80;
 OB L81;
 AM_ROU1916046290 L82;
 INT L83;
 OB L84;
 INT L85;
 dAM_STMT L86;
 dAM_STMT L87;
 dAM_STMT L88;
 dAM_STMT L89;
 is_invariant_sel = e;
 is_invariant_sel1 = ATTR(is_invariant_sel,sig1);
 if ((ATTR(is_invariant_sel1,args)==((ARRAYARG) NULL))) {
  L4 = (ATTR(is_invariant_sel1,ret)==((dTP) NULL));
  L51_=!(L4); 
  L3 = L51_;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  is_eq_self = TP_BUILTIN_bool;
  is_eq_t = ((OB) ATTR(is_invariant_sel1,ret));
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  L2 = ret_val3;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  is_eq_self1 = ATTR(is_invariant_sel1,name1);
  is_eq_i = IDENT_2063376192;
  ret_val4 = (is_eq_self1.str==is_eq_i.str);
  L1 = ret_val4;
 } else {
  L1 = FALSE;
 }
 ret_val2 = L1;
 ret_val1 = ret_val2;
 if (ret_val1) {
  SATTR(self,in_invariant,TRUE);
 }
 else {
  SATTR(self,in_invariant,FALSE);
 }
 if (ATTR(as,is_abstract)) {
  if (ATTR(e,is_external)) {
   L6 = TRUE;
  } else {
   tp_self = e;
   ret_val5 = ATTR(ATTR(tp_self,sig1),tp);
   L7 = ret_val5;
   L6 = (*dTP_is1553335918[TAG(L7)])(L7);
  }
  if (L6) {
   ret_val = ((AM_ROUT_DEF) NULL);
   return ret_val;
  }
  else {
   err_loc_self = self;
   err_loc_t = ((dPROG_ERR) as);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = self;
   err_s = ((STR) &Compil1234648972);
   if ((err_self==((TRANS) NULL))) {
    L9 = TRUE;
   } else {
    L9 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L9) {
    L8 = TRUE;
   } else {
    L8 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L8) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val6;
    plus_sarg1 = err_s;
    ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val7;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((AM_ROUT_DEF) NULL);
   return ret_val;
  }
 }
 TRANS_673475073(self, as);
 create_self = ((AM_ROUT_DEF) NULL);
 L10 = ATTR(ATTR(e,sig1),args);
 L111_=(L10)==NULL?0:ASIZE((ARRAYARG)L10); 
 L12 = L111_;
 L131_=INTPLUS(1,L12); 
 create_nargs = L131_;
 create_source = ATTR(as,source1);
 L15 = create_nargs;
 L17=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L15)*sizeof(AM_FORMAL_ARG);
 L16=ZALLOC_BIG(L17);
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

   ((OB)L16)->header.destroyed=0;
#endif

 L14 = ((AM_ROUT_DEF) L16);
 L14->asize = L15;
 r1 = L14;
 SATTR(r1,source1,create_source);
 ret_val8 = r1;
 r = ret_val8;
 SATTR(r,srcsig,ATTR(e,srcsig));
 SATTR(r,sig1,ATTR(e,sig1));
 SATTR(ATTR(r,sig1),srcsig,ATTR(e,srcsig));
 if (ATTR(e,is_external)) {
  SATTR(r,is_external,TRUE);
 }
 create_self2 = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(as,source1);
 create_name = IDENT_985337352;
 tp_self1 = e;
 ret_val9 = ATTR(ATTR(tp_self1,sig1),tp);
 create_tp = ret_val9;
 as_tp_self = e;
 clelt = ATTR(as_tp_self,as);
 switch (TAG(clelt)) {
  case AS_CONST_DEF_tag:
   L18 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
   L191_=!(L18); 
   if (L191_) {
    res = ATTR(((AS_CONST_DEF) clelt),tp);
   }
   else {
    create_self1 = ((AS_TYPE_SPEC) NULL);
    L21=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
    if (L21==NULL) FATAL("Unable to allocate more memory");
    ((OB)L21)->header.tag=AS_TYPE_SPEC_tag;
    L20 = ((AS_TYPE_SPEC) L21);
    ret_val11 = L20;
    res = ret_val11;
    SATTR(res,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
   } break;
  case AS_SHARED_DEF_tag:
   res = ATTR(((AS_SHARED_DEF) clelt),tp); break;
  case AS_ATTR_DEF_tag:
   res = ATTR(((AS_ATTR_DEF) clelt),tp); break;
  default: ;
 }
 ret_val10 = res;
 create_as_tp = ret_val10;
 L23=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=AM_LOCAL_EXPR_tag;
 L22 = ((AM_LOCAL_EXPR) L23);
 r2 = L22;
 SATTR(r2,source1,create_src);
 SATTR(r2,name1,create_name);
 SATTR(r2,tp_at,create_tp);
 SATTR(r2,as_type,create_as_tp);
 ret_val12 = r2;
 sl = ret_val12;
 create_self3 = ((AM_FORMAL_ARG) NULL);
 create_e = sl;
 L25=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=AM_FORMAL_ARG_tag;
 L24 = ((AM_FORMAL_ARG) L25);
 res1 = L24;
 SATTR(res1,expr,create_e);
 SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
 ret_val13 = res1;
 L26 = ret_val13;
 SARR((AM_ROUT_DEF)r,0,(AM_FORMAL_ARG)L26); 
 ;
 has_ret_self = ATTR(e,sig1);
 L28 = (ATTR(has_ret_self,ret)==((dTP) NULL));
 L291_=!(L28); 
 ret_val14 = L291_;
 if (ret_val14) {
  create_self4 = ((AM_LOCAL_EXPR) NULL);
  create_src1 = ATTR(as,source1);
  create_name1 = IDENT_1909070998;
  ret_self = e;
  ret_val15 = ATTR(ATTR(ret_self,sig1),ret);
  create_tp1 = ret_val15;
  L31=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=AM_LOCAL_EXPR_tag;
  L30 = ((AM_LOCAL_EXPR) L31);
  r3 = L30;
  SATTR(r3,source1,create_src1);
  SATTR(r3,name1,create_name1);
  SATTR(r3,tp_at,create_tp1);
  ret_val16 = r3;
  SATTR(r,rres,ret_val16);
  SATTR(r,locals1,FLISTA1817671564(ATTR(r,locals1), ATTR(r,rres)));
 }
 i = 0;
 na = ATTR(as,args_dec);
 L32 = AS_ARG1759480338(na);
 L33 = ATTR(ATTR(e,sig1),args);
 L341_=(L33)==NULL?0:ASIZE((ARRAYARG)L33); 
 L35 = L341_;
 L361_=(L32)==(L35); 
 L37 = L361_;
 L381_=!(L37); 
 if (L381_) {
  err_loc_self1 = self;
  err_loc_t1 = ((dPROG_ERR) as);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = self;
  err_s1 = ((STR) &Compil564774712);
  if ((err_self1==((TRANS) NULL))) {
   L40 = TRUE;
  } else {
   L40 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L40) {
   L39 = TRUE;
  } else {
   L39 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L39) {
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  else {
   plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
   plus_sarg2 = ((STR) &name21);
   ret_val17 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val17;
   plus_sarg3 = err_s1;
   ret_val18 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s1 = ret_val18;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  ret_val = ((AM_ROUT_DEF) NULL);
  return ret_val;
 }
 while (1) {
  L41 = ATTR(ATTR(e,sig1),args);
  L421_=(L41)==NULL?0:ASIZE((ARRAYARG)L41); 
  L43 = L421_;
  L441_=(i)<(L43); 
  if (L441_) {
  }
  else {
   goto after_loop;
  }
  create_self5 = ((AM_LOCAL_EXPR) NULL);
  create_src2 = ATTR(as,source1);
  create_name2 = ATTR(na,name1);
  L45 = ATTR(ATTR(e,sig1),args);
  L461_=(ARG)ARR((ARRAYARG)L45,i); 
  L47=L461_;
  create_tp2 = ATTR(L47,tp);
  create_as_tp1 = ATTR(na,tp);
  L49=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L49==NULL) FATAL("Unable to allocate more memory");
  ((OB)L49)->header.tag=AM_LOCAL_EXPR_tag;
  L48 = ((AM_LOCAL_EXPR) L49);
  r4 = L48;
  SATTR(r4,source1,create_src2);
  SATTR(r4,name1,create_name2);
  SATTR(r4,tp_at,create_tp2);
  SATTR(r4,as_type,create_as_tp1);
  ret_val19 = r4;
  l = ret_val19;
  L501_=INTPLUS(i,1); 
  L52 = L501_;
  create_self6 = ((AM_FORMAL_ARG) NULL);
  create_e1 = l;
  create_m = MODE_c998271775(((MODE) NULL), ATTR(na,mode1));
  L54=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L54==NULL) FATAL("Unable to allocate more memory");
  ((OB)L54)->header.tag=AM_FORMAL_ARG_tag;
  L53 = ((AM_FORMAL_ARG) L54);
  res2 = L53;
  SATTR(res2,expr,create_e1);
  SATTR(res2,mode1,create_m);
  ret_val20 = res2;
  L55 = ret_val20;
  SARR((AM_ROUT_DEF)r,L52,(AM_FORMAL_ARG)L55); 
  ;
  L571_=INTPLUS(i,1); 
  i = L571_;
  na = ATTR(na,next);
 }
 after_loop: ;
 SATTR(self,cur_rout,r);
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L58 = TRUE;
 } else {
  L58 = ATTR(psather_self,distributed);
 }
 ret_val21 = L58;
 if (ret_val21) {
  TRANS_75192950(self, as);
 }
 if (ATTR(self,chk_pre)) {
  L59 = (ATTR(as,pre_e)==((dAS_EXPR) NULL));
  L601_=!(L59); 
  if (L601_) {
   if (ATTR(ATTR(self,prog),pre_checks)) {
    L62 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
    L631_=!(L62); 
    L61 = L631_;
   } else {
    L61 = FALSE;
   }
   if (L61) {
    SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
   }
   SATTR(self,in_pre,TRUE);
   create_self7 = ((AM_PRE_STMT) NULL);
   create_source1 = ATTR(as,source1);
   L65=ZALLOC(sizeof(struct AM_PRE_STMT_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=AM_PRE_STMT_tag;
   L64 = ((AM_PRE_STMT) L65);
   r5 = L64;
   SATTR(r5,source1,create_source1);
   ret_val22 = r5;
   pres = ret_val22;
   SATTR(pres,tp,ATTR(ATTR(self,impl),tp));
   SATTR(pres,test1,TRANS_693526804(self, ATTR(as,pre_e), ((dTP) TP_BUILTIN_bool)));
   if ((ATTR(pres,test1)==((dAM_EXPR) NULL))) {
    pres = ((AM_PRE_STMT) NULL);
   }
   SATTR(self,in_pre,FALSE);
  }
 }
 SATTR(self,post_stmt,((AM_POST_STMT) NULL));
 if (ATTR(self,chk_post)) {
  L66 = (ATTR(as,post_e)==((dAS_EXPR) NULL));
  L671_=!(L66); 
  if (L671_) {
   SATTR(self,in_post,TRUE);
   if (ATTR(ATTR(self,prog),post_checks)) {
    L69 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
    L701_=!(L69); 
    L68 = L701_;
   } else {
    L68 = FALSE;
   }
   if (L68) {
    SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
   }
   create_self8 = ((AM_POST_STMT) NULL);
   create_source2 = ATTR(as,source1);
   L72=ZALLOC(sizeof(struct AM_POST_STMT_struct));
   if (L72==NULL) FATAL("Unable to allocate more memory");
   ((OB)L72)->header.tag=AM_POST_STMT_tag;
   L71 = ((AM_POST_STMT) L72);
   r6 = L71;
   SATTR(r6,source1,create_source2);
   ret_val23 = r6;
   SATTR(self,post_stmt,ret_val23);
   SATTR(ATTR(self,post_stmt),tp,ATTR(ATTR(self,impl),tp));
   SATTR(ATTR(self,post_stmt),test1,TRANS_693526804(self, ATTR(as,post_e), ((dTP) TP_BUILTIN_bool)));
   if ((ATTR(ATTR(self,post_stmt),test1)==((dAM_EXPR) NULL))) {
    SATTR(self,post_stmt,((AM_POST_STMT) NULL));
   }
   SATTR(self,in_post,FALSE);
  }
 }
 SATTR(self,inv_stmt,((AM_INV1167837230) NULL));
 if (ATTR(self,chk_invariant)) {
  L74 = ATTR(e,is_private);
  L751_=!(L74); 
  if (L751_) {
   L76 = ATTR(self,in_invariant);
   L771_=!(L76); 
   L73 = L771_;
  } else {
   L73 = FALSE;
  }
  if (L73) {
   isig = IMPL_i1943552357(ATTR(self,impl));
   L78 = (isig==((SIG) NULL));
   L791_=!(L78); 
   if (L791_) {
    create_self9 = ((AM_INV1167837230) NULL);
    create_source3 = ATTR(as,source1);
    L81=ZALLOC(sizeof(struct AM_INV1167837230_struct));
    if (L81==NULL) FATAL("Unable to allocate more memory");
    ((OB)L81)->header.tag=AM_INV1167837230_tag;
    L80 = ((AM_INV1167837230) L81);
    r7 = L80;
    SATTR(r7,source1,create_source3);
    ret_val24 = r7;
    SATTR(self,inv_stmt,ret_val24);
    SATTR(ATTR(self,inv_stmt),sig1,isig);
    create_self10 = ((AM_ROU1916046290) NULL);
    create_nargs1 = 1;
    create_source4 = ATTR(as,source1);
    L83 = create_nargs1;
    L85=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L83)*sizeof(AM_CALL_ARG);
    L84=ZALLOC_BIG(L85);
    if (L84==NULL) FATAL("Unable to allocate more memory");
    ((OB)L84)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

      ((OB)L84)->header.destroyed=0;
#endif

    L82 = ((AM_ROU1916046290) L84);
    L82->asize = L83;
    r8 = L82;
    SATTR(r8,source1,create_source4);
    ret_val25 = r8;
    icall = ret_val25;
    SATTR(icall,fun,isig);
    SE_CON412941612(ATTR(self,cur_se), ((dAM) icall));
    SATTR(r,calls1,FLISTd805769956(ATTR(r,calls1), ((dAM_EXPR) icall)));
   }
  }
 }
 code11 = TRANS_1563748848(self, ATTR(as,body));
 SATTR(r,code1,ATTR(self,init_stmts));
 if ((ATTR(r,code1)==((dAM_STMT) NULL))) {
  SATTR(r,code1,((dAM_STMT) pres));
 }
 else {
  L86 = ATTR(r,code1);
  (*dAM_ST2020072576[TAG(L86)])(L86, ((dAM_STMT) pres));
 }
 if ((ATTR(r,code1)==((dAM_STMT) NULL))) {
  SATTR(r,code1,code11);
 }
 else {
  L87 = ATTR(r,code1);
  (*dAM_ST2020072576[TAG(L87)])(L87, code11);
 }
 if ((ATTR(as,ret_dec)==((AS_TYPE_SPEC) NULL))) {
  if ((ATTR(r,code1)==((dAM_STMT) NULL))) {
   SATTR(r,code1,((dAM_STMT) ATTR(self,post_stmt)));
  }
  else {
   L88 = ATTR(r,code1);
   (*dAM_ST2020072576[TAG(L88)])(L88, ((dAM_STMT) ATTR(self,post_stmt)));
  }
  if ((ATTR(r,code1)==((dAM_STMT) NULL))) {
   SATTR(r,code1,((dAM_STMT) ATTR(self,inv_stmt)));
  }
  else {
   L89 = ATTR(r,code1);
   (*dAM_ST2020072576[TAG(L89)])(L89, ((dAM_STMT) ATTR(self,inv_stmt)));
  }
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF TRANS_503579921(TRANS self, ELT e) {
 AM_ROUT_DEF ret_val;
 TRANS t;
 dAS_CLASS_ELT as;
 AM_ROUT_DEF r = ((AM_ROUT_DEF) NULL);
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 t = TRANS_214014142(((TRANS) NULL), e);
 if ((t==((TRANS) NULL))) {
  ret_val = ((AM_ROUT_DEF) NULL);
  return ret_val;
 }
 as = ATTR(e,as);
 switch (TAG(as)) {
  case AS_CONST_DEF_tag:
   r = TRANS_1746053264(t, e, ((AS_CONST_DEF) as)); break;
  case AS_SHARED_DEF_tag:
   r = TRANS_1291044671(t, e, ((AS_SHARED_DEF) as)); break;
  case AS_ATTR_DEF_tag:
   r = TRANS_1584821941(t, e, ((AS_ATTR_DEF) as)); break;
  case AS_ROUT_DEF_tag:
   r = TRANS_281283203(t, e, ((AS_ROUT_DEF) as));
   psather_self = ATTR(t,prog);
   if (ATTR(psather_self,threads)) {
    L2 = TRUE;
   } else {
    L2 = ATTR(psather_self,distributed);
   }
   ret_val1 = L2;
   if (ret_val1) {
    L3 = (ATTR(t,cur_se)==((SE_CONTEXT) NULL));
    L41_=!(L3); 
    L1 = L41_;
   } else {
    L1 = FALSE;
   }
   if (L1) {
    if (ATTR(ATTR(t,cur_se),has_import)) {
     L5 = TRUE;
    } else {
     if (SIG_is418491101(ATTR(r,sig1))) {
      L6 = ATTR(ATTR(ATTR(r,sig1),builtin_info),does_import);
     } else {
      L6 = FALSE;
     }
     L5 = L6;
    }
    SATTR(ATTR(t,cur_se),has_import,L5);
    if (ATTR(ATTR(t,cur_se),has_export)) {
     L7 = TRUE;
    } else {
     if (SIG_is418491101(ATTR(r,sig1))) {
      L8 = ATTR(ATTR(ATTR(r,sig1),builtin_info),does_export);
     } else {
      L8 = FALSE;
     }
     L7 = L8;
    }
    SATTR(ATTR(t,cur_se),has_export,L7);
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::transform_elt(ELT):AM_ROUT_DEF\n./Middle/trans.sa:226:14");
 }
 SE_CON621544985(ATTR(t,cur_se));
 SYSDESTROY(t); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_309326235(TRANS self, dTP tp) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTARRAYdTP create_self = TUPIDENTARRAYdTP_zero;
 IDENT create_at1 = IDENT_zero;
 ARRAYdTP create_at2;
 TUPIDENTARRAYdTP ret_val1 = TUPIDENTARRAYdTP_zero;
 extern STR dATTACH;
 TUPIDENTARRAYdTP L1;
 TUPIDENTARRAYdTP L2;
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   create_self = TUPIDENTARRAYdTP_zero;
   create_at1 = IDENT_1150413730(IDENT_zero, ((STR) &dATTACH));
   create_at2 = ATTR(((TP_CLASS) tp),params);
   L1 = create_self;
   L1.t1 = create_at1;
   L2 = L1;
   L2.t2 = create_at2;
   ret_val1 = L2;
   ret_val = TP_CLA513015767(((TP_CLASS) tp), ((dCALL_TP) TP_CLA420352644(ATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl), ret_val1)));
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::is_of_type_attach($TP):BOOL\n./Middle/trans.sa:5568:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1278179299(TUPAM_667892060 self, TUPAM_667892060 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_667892060 elt_eq_self = TUPAM_667892060_zero;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 TUPAM_667892060 elt_eq2_self = TUPAM_667892060_zero;
 BOUND_1677815802 elt_eq2_e1;
 BOUND_1677815802 elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ960775407 elt_eq_self2;
 BOUND_1677815802 elt_eq_e12;
 BOUND_1677815802 elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ947319992) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_a = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ960775407) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_2013149714(TUPAM_304458649 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_304458649 L1;
 if (arg==NULL) {
  goto other186;
 } else
 switch (TAG(arg)) {
  case TUPAM_304458649_tag:
   L1 = ((TUPAM_304458649_boxed) arg)->immutable_part;
   ret_val = TUPAM_540366523(self, L1);
   return ret_val; break;
  default: ;
  other186: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_2045709861(TUPAM_667892060 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_667892060 L1;
 if (arg==NULL) {
  goto other187;
 } else
 switch (TAG(arg)) {
  case TUPAM_667892060_tag:
   L1 = ((TUPAM_667892060_boxed) arg)->immutable_part;
   ret_val = TUPAM_1278179299(self, L1);
   return ret_val; break;
  default: ;
  other187: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_540366523(TUPAM_304458649 self, TUPAM_304458649 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_304458649 elt_eq_self = TUPAM_304458649_zero;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 TUPAM_304458649 elt_eq2_self = TUPAM_304458649_zero;
 BOUND_1007407331 elt_eq2_e1;
 BOUND_1007407331 elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ826090414 elt_eq_self2;
 BOUND_1007407331 elt_eq_e12;
 BOUND_1007407331 elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ947319992) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_a = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ826090414) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_1877096499(TUPAM_304458649 self) {
 INT ret_val = INT_zero;
 TUPAM_304458649 elt_hash_self = TUPAM_304458649_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_304458649 elt_hash2_self = TUPAM_304458649_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_460226510(TUPAM_667892060 self) {
 INT ret_val = INT_zero;
 TUPAM_667892060 elt_hash_self = TUPAM_667892060_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_667892060 elt_hash2_self = TUPAM_667892060_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_1128362628(TUPAM_304458649 self) {
 STR ret_val;
 FSTR res;
 AM_BND1124877163 lt1;
 BOUND_1007407331 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_827739609(TUPAM_667892060 self) {
 STR ret_val;
 FSTR res;
 AM_BND1124877163 lt1;
 BOUND_1677815802 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1055378524(TRANS self, AS_SYNC_STMT s) {
 dAM_STMT ret_val;
 dAM_STMT ret = ((dAM_STMT) NULL);
 dAS_STMT prev;
 dAS_STMT export11;
 AM_SYNC_STMT r;
 dAS_STMT import11;
 AM_SYNC_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_SYNC_STMT ret_val1;
 AM_SYNC_STMT r1;
 BOOL L1;
 BOOL L21_;
 dAS_STMT L3;
 dAS_STMT L4;
 AM_SYNC_STMT L5;
 OB L6;
 dAM_STMT L7;
 BOOL L8;
 BOOL L91_;
 dAS_STMT L10;
 L1 = ATTR(s,transformed);
 L21_=!(L1); 
 if (L21_) {
  prev = TRANS_954470687(self, ((dAS_STMT) s));
  export11 = TRANS_166911016(self, TRANS_194341863, ATTR(s,surr_stmt_list), ATTR(s,source1));
  L3 = export11;
  (*dAS_ST1687647066[TAG(L3)])(L3, ((dAS_STMT) s));
  if ((prev==((dAS_STMT) NULL))) {
   SATTR(ATTR(s,surr_stmt_list),stmts,export11);
  }
  else {
   L4 = prev;
   (*dAS_ST1687647066[TAG(L4)])(L4, export11);
  }
  ret = TRANS_1801074742(self, export11);
 }
 create_self = ((AM_SYNC_STMT) NULL);
 create_source = ATTR(s,source1);
 L6=ZALLOC(sizeof(struct AM_SYNC_STMT_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_SYNC_STMT_tag;
 L5 = ((AM_SYNC_STMT) L6);
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 if ((ret==((dAM_STMT) NULL))) {
  ret = ((dAM_STMT) r);
 }
 else {
  L7 = ret;
  (*dAM_ST2020072576[TAG(L7)])(L7, ((dAM_STMT) r));
 }
 L8 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L91_=!(L8); 
 if (L91_) {
  SATTR(ATTR(self,cur_se),may_block,TRUE);
 }
 import11 = TRANS_166911016(self, TRANS_1924362320, ATTR(s,surr_stmt_list), ATTR(s,source1));
 L10 = import11;
 (*dAS_ST1687647066[TAG(L10)])(L10, ATTR(s,next));
 SATTR(s,next,import11);
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1403806496(TRANS self, AS_FORK_STMT s) {
 dAM_STMT ret_val;
 dAS_STMT ret_as = ((dAS_STMT) NULL);
 dTP orig_cl_tp;
 TP_CLASS orig_class_tp = ((TP_CLASS) NULL);
 AS_CLASS_DEF orig_class_as = ((AS_CLASS_DEF) NULL);
 ARRAYdTP orig_cl_params = ((ARRAYdTP) NULL);
 INT num1 = INT_zero;
 dTP current_tp;
 TP_CLASS current_class_tp = ((TP_CLASS) NULL);
 TP_CLASS dummy1;
 IDENT newidfork = IDENT_zero;
 IDENT newidparams_cl = IDENT_zero;
 IDENT newidparams_ob = IDENT_zero;
 AM_LOCAL_EXPR helper1;
 AS_ASSIGN_STMT chos;
 TRANS L11;
 AM_LOCAL_EXPR loc;
 AS_ASSIGN_STMT exp1;
 AS_CALL_EXPR rhs;
 AS_INT2031239268 att8;
 TRANS fork_outside_par;
 AS_FORK_STMT fork_outside_par1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS fast_fork_messag;
 AS_FORK_STMT fast_fork_messag1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS message_self;
 STR message_s;
 TRANS transform_simple;
 AS_FORK_STMT transform_simple1;
 dAM_STMT ret_val3;
 AM_FORK_STMT r;
 AM_FORK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_FORK_STMT ret_val4;
 AM_FORK_STMT r1;
 TRANS expensive_fork_w;
 AS_FORK_STMT expensive_fork_w1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS warning_self;
 STR warning_s;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 AM_FORMAL_ARG tp_self;
 dTP ret_val7;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val8;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 TRANS add_helper_to_tb;
 IDENT add_helper_to_tb1 = IDENT_zero;
 ARRAYdTP add_helper_to_tb2;
 TP_CLASS ret_val11;
 TP_CLASS tp;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val12;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val14;
 IDENT next_tmp_self = IDENT_zero;
 STR next_tmp_oldiden;
 STR ret_val15;
 STR ret;
 STR create_self1;
 STR ret_val16;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val17;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val18;
 INT str_self = INT_zero;
 STR ret_val19;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val20;
 FSTR str_self1;
 STR ret_val21;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val22;
 IDENT next_tmp_self1 = IDENT_zero;
 STR next_tmp_oldiden1;
 STR ret_val23;
 STR ret11;
 STR create_self2;
 STR ret_val24;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val25;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val26;
 INT str_self2 = INT_zero;
 STR ret_val27;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val28;
 FSTR str_self3;
 STR ret_val29;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val30;
 IDENT next_tmp_self2 = IDENT_zero;
 STR next_tmp_oldiden2;
 STR ret_val31;
 STR ret2;
 STR create_self3;
 STR ret_val32;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val33;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val34;
 INT str_self4 = INT_zero;
 STR ret_val35;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val36;
 FSTR str_self5;
 STR ret_val37;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val38;
 AS_ASSIGN_STMT create_self4;
 AS_ASSIGN_STMT ret_val39;
 AS_CALL_EXPR create_self5;
 AS_CALL_EXPR ret_val40;
 AS_INT2031239268 create_self6;
 AS_INT2031239268 ret_val41;
 BOOL L2;
 BOOL L31_;
 extern STR forkst2003855832;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 BOOL L6;
 BOOL L71_;
 extern STR Fastfo578173173;
 AM_FORK_STMT L8;
 OB L9;
 extern STR Expens1195807252;
 BOOL L10;
 BOOL L121_;
 INT L131_;
 dPARSE L14;
 extern STR Compil1402030697;
 BOOL L15;
 BOOL L16;
 extern STR name21;
 AM_ROUT_DEF L17;
 AM_FORMAL_ARG L181_;
 extern STR Compil1510538391;
 BOOL L19;
 BOOL L20;
 extern STR name21;
 extern STR Compil163952783;
 BOOL L21;
 BOOL L22;
 extern STR name21;
 BOOL L23;
 BOOL L241_;
 extern STR fork11;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L25;
 BOOL L261_;
 INT L27;
 INT L281_;
 extern STR params_cl;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L29;
 BOOL L301_;
 INT L32;
 INT L331_;
 extern STR params_ob;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L34;
 BOOL L351_;
 INT L36;
 INT L371_;
 dAS_STMT L38;
 dAS_STMT L39;
 AM_LOCAL_EXPR L40;
 BOOL L41;
 STR L42;
 extern STR S_;
 BOOL L43;
 BOOL L441_;
 BOOL L45;
 BOOL L461_;
 AS_ASSIGN_STMT L47;
 OB L48;
 AS_CALL_EXPR L49;
 OB L50;
 dAS_STMT L51;
 dAS_STMT L52;
 AS_INT2031239268 L53;
 OB L54;
 dAS_STMT L55;
 dAS_STMT L56;
 L2 = TRANS_2091440962(self);
 L31_=!(L2); 
 if (L31_) {
  fork_outside_par = self;
  fork_outside_par1 = s;
  err_loc_self = fork_outside_par;
  err_loc_t = ((dPROG_ERR) fork_outside_par1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = fork_outside_par;
  err_s = ((STR) &forkst2003855832);
  if ((err_self==((TRANS) NULL))) {
   L5 = TRUE;
  } else {
   L5 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L5) {
   L4 = TRUE;
  } else {
   L4 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L4) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = err_s;
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L6 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L71_=!(L6); 
 if (L71_) {
  SATTR(ATTR(self,cur_se),forks,TRUE);
 }
 if (ATTR(ATTR(self,prog),fast_fork)) {
  if (TRANS_1615967248(self, s)) {
   fast_fork_messag = self;
   fast_fork_messag1 = s;
   err_loc_self1 = fast_fork_messag;
   err_loc_t1 = ((dPROG_ERR) fast_fork_messag1);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   message_self = fast_fork_messag;
   message_s = ((STR) &Fastfo578173173);
   PROG_message_STR(ATTR(message_self,prog), message_s);
   transform_simple = self;
   transform_simple1 = s;
   create_self = ((AM_FORK_STMT) NULL);
   create_source = ATTR(transform_simple1,source1);
   L9=ZALLOC(sizeof(struct AM_FORK_STMT_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=AM_FORK_STMT_tag;
   L8 = ((AM_FORK_STMT) L9);
   r1 = L8;
   SATTR(r1,source1,create_source);
   ret_val4 = r1;
   r = ret_val4;
   SATTR(r,body,TRANS_1563748848(transform_simple, ATTR(transform_simple1,body)));
   SATTR(r,at,TRANS_693526804(transform_simple, ATTR(transform_simple1,at), ((dTP) TP_BUILTIN_int)));
   SATTR(r,par_stmt,ATTR(transform_simple,cur_simple_par));
   ret_val3 = ((dAM_STMT) r);
   ret_val = ret_val3;
   return ret_val;
  }
  else {
   expensive_fork_w = self;
   expensive_fork_w1 = s;
   err_loc_self2 = expensive_fork_w;
   err_loc_t2 = ((dPROG_ERR) expensive_fork_w1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   warning_self = expensive_fork_w;
   warning_s = ((STR) &Expens1195807252);
   PROG_warning_STR(ATTR(warning_self,prog), warning_s);
  }
 }
 orig_cl_tp = ATTR(ATTR(ATTR(self,cur_rout),srcsig),tp);
 if (orig_cl_tp==NULL) {
  goto other448;
 } else
 switch (TAG(orig_cl_tp)) {
  case TP_CLASS_tag:
   orig_class_tp = ((TP_CLASS) orig_cl_tp);
   orig_cl_params = ATTR(orig_class_tp,params);
   num1 = 0;
   L10 = (orig_cl_params==((ARRAYdTP) NULL));
   L121_=!(L10); 
   if (L121_) {
    L131_=(orig_cl_params)==NULL?0:ASIZE((ARRAYdTP)orig_cl_params); 
    num1 = L131_;
   }
   L14 = ATTR(ATTR(self,prog),parse);
   orig_class_as = (*dPARSE1581792481[TAG(L14)])(L14, ATTR(orig_class_tp,name1), num1); break;
  default: ;
  other448: ;
   err_self1 = self;
   err_s1 = ((STR) &Compil1402030697);
   if ((err_self1==((TRANS) NULL))) {
    L16 = TRUE;
   } else {
    L16 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L16) {
    L15 = TRUE;
   } else {
    L15 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L15) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val5;
    plus_sarg3 = err_s1;
    ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val6;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
 }
 L17 = ATTR(self,cur_rout);
 L181_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L17,0); 
 tp_self = L181_;
 tp_self1 = ATTR(tp_self,expr);
 ret_val8 = ATTR(tp_self1,tp_at);
 ret_val7 = ret_val8;
 current_tp = ret_val7;
 if (current_tp==NULL) {
  goto other449;
 } else
 switch (TAG(current_tp)) {
  case TP_CLASS_tag:
   current_class_tp = ((TP_CLASS) current_tp); break;
  default: ;
  other449: ;
   err_self2 = self;
   err_s2 = ((STR) &Compil1510538391);
   if ((err_self2==((TRANS) NULL))) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L19) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val9;
    plus_sarg5 = err_s2;
    ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val10;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
 }
 if (ATTR(s,transformed)) {
  add_helper_to_tb = self;
  add_helper_to_tb1 = ATTR(s,helper_class);
  add_helper_to_tb2 = orig_cl_params;
  tp_class_for_sel = ATTR(ATTR(add_helper_to_tb,prog),tp_tbl);
  tp_class_for_nam = add_helper_to_tb1;
  tp_class_for_par = add_helper_to_tb2;
  ret_val12 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
  tp = ret_val12;
  if ((tp==((TP_CLASS) NULL))) {
   err_self3 = add_helper_to_tb;
   err_s3 = ((STR) &Compil163952783);
   if ((err_self3==((TRANS) NULL))) {
    L22 = TRUE;
   } else {
    L22 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L22) {
    L21 = TRUE;
   } else {
    L21 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L21) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg6 = ((STR) &name21);
    ret_val13 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val13;
    plus_sarg7 = err_s3;
    ret_val14 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s3 = ret_val14;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
  }
  L23 = FSETdT1764379880(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp));
  L241_=!(L23); 
  if (L241_) {
   SATTR(ATTR(add_helper_to_tb,prog),tp_done,FSETdT1016814448(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp)));
  }
  ret_val11 = tp;
  dummy1 = ret_val11;
  TRANS_1069298803(self, current_class_tp, ATTR(s,rout));
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 next_tmp_self = IDENT_zero;
 next_tmp_oldiden = ((STR) &fork11);
 create_self1 = ((STR) NULL);
 ret_val16 = ((STR) &name1);
 ret = ret_val16;
 plus_self8 = ((STR) &S_pS_);
 plus_sarg8 = next_tmp_oldiden;
 ret_val17 = STR_ap2004550586(plus_self8, plus_sarg8);
 plus_self9 = ret_val17;
 plus_sarg9 = ((STR) &S_);
 ret_val18 = STR_ap2004550586(plus_self9, plus_sarg9);
 plus_self10 = ret_val18;
 str_self = IDENT_tmp_count;
 clear_self = buf1;
 L25 = (clear_self==((FSTR) NULL));
 L261_=!(L25); 
 if (L261_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val20 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val20;
 str_self1 = buf1;
 ret_val21 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val19 = ret_val21;
 plus_sarg10 = ret_val19;
 ret_val22 = STR_ap2004550586(plus_self10, plus_sarg10);
 ret = ret_val22;
 L27 = IDENT_tmp_count;
 L281_=INTPLUS(L27,1); 
 IDENT_tmp_count = L281_;
 ret_val15 = ret;
 newidfork = IDENT_1150413730(IDENT_zero, ret_val15);
 next_tmp_self1 = IDENT_zero;
 next_tmp_oldiden1 = ((STR) &params_cl);
 create_self2 = ((STR) NULL);
 ret_val24 = ((STR) &name1);
 ret11 = ret_val24;
 plus_self11 = ((STR) &S_pS_);
 plus_sarg11 = next_tmp_oldiden1;
 ret_val25 = STR_ap2004550586(plus_self11, plus_sarg11);
 plus_self12 = ret_val25;
 plus_sarg12 = ((STR) &S_);
 ret_val26 = STR_ap2004550586(plus_self12, plus_sarg12);
 plus_self13 = ret_val26;
 str_self2 = IDENT_tmp_count;
 clear_self1 = buf2;
 L29 = (clear_self1==((FSTR) NULL));
 L301_=!(L29); 
 if (L301_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val28 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val28;
 str_self3 = buf2;
 ret_val29 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val27 = ret_val29;
 plus_sarg13 = ret_val27;
 ret_val30 = STR_ap2004550586(plus_self13, plus_sarg13);
 ret11 = ret_val30;
 L32 = IDENT_tmp_count;
 L331_=INTPLUS(L32,1); 
 IDENT_tmp_count = L331_;
 ret_val23 = ret11;
 newidparams_cl = IDENT_1150413730(IDENT_zero, ret_val23);
 next_tmp_self2 = IDENT_zero;
 next_tmp_oldiden2 = ((STR) &params_ob);
 create_self3 = ((STR) NULL);
 ret_val32 = ((STR) &name1);
 ret2 = ret_val32;
 plus_self14 = ((STR) &S_pS_);
 plus_sarg14 = next_tmp_oldiden2;
 ret_val33 = STR_ap2004550586(plus_self14, plus_sarg14);
 plus_self15 = ret_val33;
 plus_sarg15 = ((STR) &S_);
 ret_val34 = STR_ap2004550586(plus_self15, plus_sarg15);
 plus_self16 = ret_val34;
 str_self4 = IDENT_tmp_count;
 clear_self2 = buf3;
 L34 = (clear_self2==((FSTR) NULL));
 L351_=!(L34); 
 if (L351_) {
  SATTR(clear_self2,loc,0);
 }
 str_in_self2 = str_self4;
 str_in_s2 = buf3;
 ret_val36 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
 buf3 = ret_val36;
 str_self5 = buf3;
 ret_val37 = STR_fr1097270334(((STR) NULL), str_self5);
 ret_val35 = ret_val37;
 plus_sarg16 = ret_val35;
 ret_val38 = STR_ap2004550586(plus_self16, plus_sarg16);
 ret2 = ret_val38;
 L36 = IDENT_tmp_count;
 L371_=INTPLUS(L36,1); 
 IDENT_tmp_count = L371_;
 ret_val31 = ret2;
 newidparams_ob = IDENT_1150413730(IDENT_zero, ret_val31);
 SATTR(s,helper_class,newidparams_cl);
 helper1 = TRANS_1496987093(self, ((dAS_STMT) s), newidparams_ob, newidparams_cl, orig_class_as, orig_cl_params);
 chos = TRANS_1390141743(self, newidparams_ob, newidparams_cl, orig_class_as, ATTR(s,surr_stmt_list), ATTR(s,source1));
 if ((ret_as==((dAS_STMT) NULL))) {
  ret_as = ((dAS_STMT) chos);
 }
 else {
  L38 = ret_as;
  (*dAS_ST1454156204[TAG(L38)])(L38, ((dAS_STMT) chos));
 }
 L39 = ret_as;
 (*dAS_ST1454156204[TAG(L39)])(L39, ((dAS_STMT) TRANS_2064676398(self, newidparams_ob, ATTR(s,surr_stmt_list), ATTR(s,source1))));
 {
  struct TRANS_1349638895_frame_struct other1_0;
TRANS_1349638895_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L40 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   loc = L40;
   L42 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L43 = STR_is111530248(L42, ((STR) &S_));
   L441_=!(L43); 
   if (L441_) {
    L45 = TRANS_157729662(self, ATTR(loc,name1), ATTR(self,cur_param_ob));
    L461_=!(L45); 
    L41 = L461_;
   } else {
    L41 = FALSE;
   }
   if (L41) {
    create_self4 = ((AS_ASSIGN_STMT) NULL);
    L48=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=AS_ASSIGN_STMT_tag;
    L47 = ((AS_ASSIGN_STMT) L48);
    ret_val39 = L47;
    exp1 = ret_val39;
    SATTR(exp1,source1,ATTR(s,source1));
    SATTR(exp1,surr_stmt_list,ATTR(s,surr_stmt_list));
    SATTR(exp1,lhs_expr,((dAS_EXPR) TRANS_1849744987(self, ATTR(loc,name1), helper1, ATTR(s,source1))));
    create_self5 = ((AS_CALL_EXPR) NULL);
    L50=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L50==NULL) FATAL("Unable to allocate more memory");
    ((OB)L50)->header.tag=AS_CALL_EXPR_tag;
    L49 = ((AS_CALL_EXPR) L50);
    ret_val40 = L49;
    rhs = ret_val40;
    SATTR(rhs,source1,ATTR(s,source1));
    SATTR(rhs,name1,ATTR(loc,name1));
    SATTR(exp1,rhs,((dAS_EXPR) rhs));
    L51 = ret_as;
    (*dAS_ST1454156204[TAG(L51)])(L51, ((dAS_STMT) exp1));
   }
  }
 }
 after_loop: ;
 L52 = ret_as;
 (*dAS_ST1454156204[TAG(L52)])(L52, TRANS_166911016(self, TRANS_194341863, ATTR(s,surr_stmt_list), ATTR(s,source1)));
 SATTR(s,rout,TRANS_725662504(self, newidfork, newidparams_ob, newidparams_cl, helper1, orig_class_as, current_class_tp, ATTR(ATTR(self,cur_cohort),name1), ((AS_TYPE_SPEC) NULL), ATTR(s,body), TRANS_frk_code, ATTR(s,source1)));
 SATTR(s,body,((AS_STMT_LIST) NULL));
 create_self6 = ((AS_INT2031239268) NULL);
 L54=ZALLOC(sizeof(struct AS_INT2031239268_struct));
 if (L54==NULL) FATAL("Unable to allocate more memory");
 ((OB)L54)->header.tag=AS_INT2031239268_tag;
 L53 = ((AS_INT2031239268) L54);
 ret_val41 = L53;
 att8 = ret_val41;
 SATTR(att8,source1,ATTR(s,source1));
 SATTR(att8,surr_stmt_list,ATTR(s,surr_stmt_list));
 SATTR(att8,routname,newidfork);
 SATTR(att8,helpername,newidparams_ob);
 SATTR(att8,gatename,ATTR(ATTR(self,cur_cohort),name1));
 SATTR(att8,at,ATTR(s,at));
 L55 = ret_as;
 (*dAS_ST1454156204[TAG(L55)])(L55, ((dAS_STMT) att8));
 L56 = ret_as;
 (*dAS_ST1454156204[TAG(L56)])(L56, ATTR(s,next));
 SATTR(s,next,ret_as);
 ret_val = ((dAM_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1563748848(TRANS self, AS_STMT_LIST l) {
 dAM_STMT ret_val;
 dAS_STMT s;
 INT osize = INT_zero;
 dAM_STMT r = ((dAM_STMT) NULL);
 dAS_STMT last1 = ((dAS_STMT) NULL);
 dAM_STMT t;
 dAS_STMT export11;
 dAS_STMT sys_exp;
 BOOL imp_local_pendin = BOOL_zero;
 BOOL imp_call_pending = BOOL_zero;
 AS_STMT_LIST exp_stmt_list;
 dAM_STMT exp1;
 dAS_STMT sys_imp;
 AM_LOCAL_EXPR ignore1;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 AM_ROUT_DEF export_locals_pe;
 BOOL ret_val2 = BOOL_zero;
 AM_ROUT_DEF export_call_pend;
 BOOL ret_val3 = BOOL_zero;
 AM_ROUT_DEF export_done_self;
 AM_ROUT_DEF import_locals_pe;
 BOOL ret_val4 = BOOL_zero;
 AM_ROUT_DEF import_call_pend;
 BOOL ret_val5 = BOOL_zero;
 AS_STMT_LIST create_self;
 AS_STMT_LIST ret_val6;
 AM_ROUT_DEF needs_import_loc;
 SIG is_forked_self;
 BOOL ret_val7 = BOOL_zero;
 AM_ROUT_DEF needs_import_cal;
 PROG psather_self1;
 BOOL ret_val8 = BOOL_zero;
 AM_ROUT_DEF import_locals_pe1;
 BOOL ret_val9 = BOOL_zero;
 AM_ROUT_DEF import_call_pend1;
 BOOL ret_val10 = BOOL_zero;
 AM_ROUT_DEF import_done_self;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 dAS_STMT L6;
 BOOL L7;
 BOOL L81_;
 INT L9;
 dAS_STMT L10;
 INT L11;
 dAS_STMT L12;
 dAS_STMT L13;
 dAS_STMT L14;
 BOOL L15;
 BOOL L161_;
 AS_STMT_LIST L17;
 OB L18;
 dAS_STMT L19;
 dAS_STMT L20;
 BOOL L22;
 BOOL L231_;
 dAM_STMT L24;
 BOOL L25;
 BOOL L26;
 BOOL L27;
 BOOL L281_;
 dAM_STMT L29;
 BOOL L30;
 dAS_STMT L31;
 BOOL L32;
 BOOL L331_;
 INT L34;
 dAS_STMT L35;
 dAS_STMT L36;
 dAS_STMT L37;
 dAS_STMT L38;
 dAS_STMT L39;
 dAS_STMT L40;
 dAS_STMT L42;
 BOOL L43;
 BOOL L441_;
 INT L45;
 BOOL L461_;
 if ((l==((AS_STMT_LIST) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 s = ATTR(l,stmts);
 if ((s==((dAS_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L1 = (ATTR(self,active_locals)==((FLISTA725283029) NULL));
 L21_=!(L1); 
 if (L21_) {
  osize = FLISTA1151473122(ATTR(self,active_locals));
 }
 while (1) {
  L3 = (s==((dAS_STMT) NULL));
  L41_=!(L3); 
  if (L41_) {
  }
  else {
   goto after_loop;
  }
  t = TRANS_1801074742(self, s);
  psather_self = ATTR(self,prog);
  if (ATTR(psather_self,threads)) {
   L5 = TRUE;
  } else {
   L5 = ATTR(psather_self,distributed);
  }
  ret_val1 = L5;
  if (ret_val1) {
   export11 = ((dAS_STMT) NULL);
   L6 = s;
   L7 = (*dAS_ST1616688325[TAG(L6)])(L6);
   L81_=!(L7); 
   if (L81_) {
    export_locals_pe = ATTR(self,cur_rout);
    ret_val2 = ATTR(export_locals_pe,n_export_locals);
    if (ret_val2) {
     if (TRANS_2136297923) {
      L9 = TRANS_194341863;
      L10 = s;
      export11 = TRANS_120708644(self, L9, l, (*dAS_ST957383256[TAG(L10)])(L10));
     }
    }
    export_call_pend = ATTR(self,cur_rout);
    ret_val3 = ATTR(export_call_pend,n_export_call);
    if (ret_val3) {
     L11 = TRANS_194341863;
     L12 = s;
     sys_exp = TRANS_166911016(self, L11, l, (*dAS_ST957383256[TAG(L12)])(L12));
     L13 = sys_exp;
     (*dAS_ST1569642444[TAG(L13)])(L13, TRUE);
     if ((export11==((dAS_STMT) NULL))) {
      export11 = sys_exp;
     }
     else {
      L14 = export11;
      (*dAS_ST1454156204[TAG(L14)])(L14, sys_exp);
     }
    }
   }
   export_done_self = ATTR(self,cur_rout);
   SATTR(export_done_self,n_export_locals,FALSE);
   SATTR(export_done_self,n_export_call,FALSE);
   L15 = (export11==((dAS_STMT) NULL));
   L161_=!(L15); 
   if (L161_) {
    import_locals_pe = ATTR(self,cur_rout);
    ret_val4 = ATTR(import_locals_pe,n_import_locals);
    imp_local_pendin = ret_val4;
    import_call_pend = ATTR(self,cur_rout);
    ret_val5 = ATTR(import_call_pend,n_import_call);
    imp_call_pending = ret_val5;
    create_self = ((AS_STMT_LIST) NULL);
    L18=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
    if (L18==NULL) FATAL("Unable to allocate more memory");
    ((OB)L18)->header.tag=AS_STMT_LIST_tag;
    L17 = ((AS_STMT_LIST) L18);
    ret_val6 = L17;
    exp_stmt_list = ret_val6;
    SATTR(exp_stmt_list,stmts,export11);
    exp1 = TRANS_1563748848(self, exp_stmt_list);
    L19 = export11;
    (*dAS_ST1454156204[TAG(L19)])(L19, s);
    if ((last1==((dAS_STMT) NULL))) {
     SATTR(l,stmts,export11);
    }
    else {
     L20 = last1;
     (*dAS_ST1687647066[TAG(L20)])(L20, export11);
    }
    L22 = (exp1==((dAM_STMT) NULL));
    L231_=!(L22); 
    if (L231_) {
     L24 = exp1;
     (*dAM_ST2020072576[TAG(L24)])(L24, t);
     t = exp1;
    }
    if (imp_local_pendin) {
     needs_import_loc = ATTR(self,cur_rout);
     is_forked_self = ATTR(needs_import_loc,sig1);
     if (ATTR(is_forked_self,is_par_routine)) {
      L26 = TRUE;
     } else {
      L26 = ATTR(is_forked_self,is_fork_routine);
     }
     if (L26) {
      L25 = TRUE;
     } else {
      L25 = ATTR(is_forked_self,is_attach_routin);
     }
     ret_val7 = L25;
     L27 = ret_val7;
     L281_=!(L27); 
     SATTR(ATTR(needs_import_loc,sig1),needs_import,L281_);
     SATTR(needs_import_loc,n_import_locals,TRUE);
    }
    if (imp_call_pending) {
     needs_import_cal = ATTR(self,cur_rout);
     SATTR(needs_import_cal,n_import_call,TRUE);
    }
   }
  }
  if ((r==((dAM_STMT) NULL))) {
   r = t;
  }
  else {
   L29 = r;
   (*dAM_ST2020072576[TAG(L29)])(L29, t);
  }
  psather_self1 = ATTR(self,prog);
  if (ATTR(psather_self1,threads)) {
   L30 = TRUE;
  } else {
   L30 = ATTR(psather_self1,distributed);
  }
  ret_val8 = L30;
  if (ret_val8) {
   L31 = s;
   L32 = (*dAS_ST1616688325[TAG(L31)])(L31);
   L331_=!(L32); 
   if (L331_) {
    import_locals_pe1 = ATTR(self,cur_rout);
    ret_val9 = ATTR(import_locals_pe1,n_import_locals);
    if (ret_val9) {
     TRANS_1456624412(self, s);
    }
    import_call_pend1 = ATTR(self,cur_rout);
    ret_val10 = ATTR(import_call_pend1,n_import_call);
    if (ret_val10) {
     if (t==NULL) {
      goto other452;
     } else
     switch (TAG(t)) {
      default: ;
      other452: ;
       L34 = TRANS_1924362320;
       L35 = s;
       sys_imp = TRANS_166911016(self, L34, l, (*dAS_ST957383256[TAG(L35)])(L35));
       L36 = sys_imp;
       (*dAS_ST1569642444[TAG(L36)])(L36, TRUE);
       L37 = s;
       L38 = sys_imp;
       (*dAS_ST1454156204[TAG(L38)])(L38, (*dAS_ST460279343[TAG(L37)])(L37));
       L39 = s;
       (*dAS_ST1687647066[TAG(L39)])(L39, sys_imp);
     }
    }
   }
   import_done_self = ATTR(self,cur_rout);
   SATTR(import_done_self,n_import_locals,FALSE);
   SATTR(import_done_self,n_import_call,FALSE);
  }
  L40 = s;
  (*dAS_ST1569642444[TAG(L40)])(L40, TRUE);
  last1 = s;
  L42 = s;
  s = (*dAS_ST460279343[TAG(L42)])(L42);
 }
 after_loop: ;
 L43 = (ATTR(self,active_locals)==((FLISTA725283029) NULL));
 L441_=!(L43); 
 if (L441_) {
  while (1) {
   L45 = FLISTA1151473122(ATTR(self,active_locals));
   L461_=(osize)<(L45); 
   if (L461_) {
   }
   else {
    goto after_loop1;
   }
   ignore1 = FLISTA912130454(ATTR(self,active_locals));
  }
  after_loop1: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1603674908(TRANS self, AS_LOCK_STMT s) {
 dAM_STMT ret_val;
 AS_LOCK_IF_WHEN iwp;
 dAS_STMT import11;
 dAS_STMT import2;
 AM_LOCK_STMT r;
 dAM_STMT old_cur_lock;
 AS_LOCK_IF_WHEN iwp1;
 dAM_EXPR guardt;
 ARRAYdAM_EXPR locks1;
 dAS_EXPR lck1;
 INT L11 = INT_zero;
 INT idx = INT_zero;
 dAM_STMT body11;
 AS_STMT_LIST create_self;
 AS_STMT_LIST ret_val1;
 AM_LOCK_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCK_STMT ret_val2;
 AM_LOCK_STMT r1;
 ARRAYdAM_EXPR create_self2;
 INT create_n = INT_zero;
 ARRAYdAM_EXPR ret_val3;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 AS_STMT_LIST L6;
 OB L7;
 dAS_STMT L8;
 BOOL L9;
 BOOL L101_;
 dAS_STMT L12;
 AM_LOCK_STMT L13;
 OB L14;
 BOOL L15;
 BOOL L161_;
 ARRAYdAM_EXPR L17;
 INT L18;
 OB L19;
 INT L20;
 INT L211_;
 BOOL L22;
 BOOL L231_;
 INT L24;
 dAM_EXPR L25;
 dAS_EXPR L27;
 BOOL L28;
 BOOL L291_;
 L2 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L31_=!(L2); 
 if (L31_) {
  SATTR(ATTR(self,cur_se),may_block,TRUE);
 }
 L4 = ATTR(s,transformed);
 L51_=!(L4); 
 if (L51_) {
  iwp = ATTR(s,if_when_part);
  while (1) {
   if ((iwp==((AS_LOCK_IF_WHEN) NULL))) {
    goto after_loop;
   }
   if ((ATTR(iwp,then_part)==((AS_STMT_LIST) NULL))) {
    create_self = ((AS_STMT_LIST) NULL);
    L7=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=AS_STMT_LIST_tag;
    L6 = ((AS_STMT_LIST) L7);
    ret_val1 = L6;
    SATTR(iwp,then_part,ret_val1);
    SATTR(ATTR(iwp,then_part),source1,ATTR(s,source1));
   }
   import11 = TRANS_166911016(self, TRANS_1924362320, ATTR(iwp,then_part), ATTR(s,source1));
   if ((ATTR(ATTR(iwp,then_part),stmts)==((dAS_STMT) NULL))) {
    SATTR(ATTR(iwp,then_part),stmts,import11);
   }
   else {
    L8 = import11;
    (*dAS_ST1687647066[TAG(L8)])(L8, ATTR(ATTR(iwp,then_part),stmts));
    SATTR(ATTR(iwp,then_part),stmts,import11);
   }
   TRANS_866431519(self, ATTR(iwp,then_part), FALSE);
   iwp = ATTR(iwp,next);
  }
  after_loop: ;
  L9 = (ATTR(s,else_part)==((AS_STMT_LIST) NULL));
  L101_=!(L9); 
  if (L101_) {
   import2 = TRANS_166911016(self, TRANS_1924362320, ATTR(s,else_part), ATTR(s,source1));
   if ((ATTR(ATTR(s,else_part),stmts)==((dAS_STMT) NULL))) {
    SATTR(ATTR(s,else_part),stmts,import2);
   }
   else {
    L12 = import2;
    (*dAS_ST1687647066[TAG(L12)])(L12, ATTR(ATTR(s,else_part),stmts));
    SATTR(ATTR(s,else_part),stmts,import2);
   }
   TRANS_866431519(self, ATTR(s,else_part), FALSE);
  }
 }
 if ((s==((AS_LOCK_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self1 = ((AM_LOCK_STMT) NULL);
 create_source = ATTR(s,source1);
 L14=ZALLOC(sizeof(struct AM_LOCK_STMT_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_LOCK_STMT_tag;
 L13 = ((AM_LOCK_STMT) L14);
 r1 = L13;
 SATTR(r1,source1,create_source);
 ret_val2 = r1;
 r = ret_val2;
 old_cur_lock = ATTR(self,cur_lock);
 SATTR(self,cur_lock,((dAM_STMT) r));
 iwp1 = ATTR(s,if_when_part);
 while (1) {
  L15 = (iwp1==((AS_LOCK_IF_WHEN) NULL));
  L161_=!(L15); 
  if (L161_) {
  }
  else {
   goto after_loop1;
  }
  guardt = TRANS_693526804(self, ATTR(iwp1,val1), ((dTP) TP_BUILTIN_bool));
  create_self2 = ((ARRAYdAM_EXPR) NULL);
  create_n = AS_LOC1840648359(iwp1);
  L18 = create_n;
  L20=(sizeof(struct ARRAYdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L18)*sizeof(dAM_EXPR);
  L19=ZALLOC_BIG(L20);
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=ARRAYdAM_EXPR_tag;
#ifdef DESTROY_CHK

    ((OB)L19)->header.destroyed=0;
#endif

  L17 = ((ARRAYdAM_EXPR) L19);
  L17->asize = L18;
  ret_val3 = L17;
  locks1 = ret_val3;
  lck1 = ATTR(iwp1,e_list);
  {
   BOOL f_L211_ = TRUE;
   L11 = 0;
   L211_=L11-1; 
   while (1) {
    L22 = (lck1==((dAS_EXPR) NULL));
    L231_=!(L22); 
    if (L231_) {
    }
    else {
     goto after_loop2;
    }
    L211_++; 
    idx = L211_;
    L25 = TRANS_693526804(self, lck1, ((dTP) TP_BUI1813155122));
    SARR((ARRAYdAM_EXPR)locks1,idx,(dAM_EXPR)L25); 
    ;
    L27 = lck1;
    lck1 = (*dAS_EX337431651[TAG(L27)])(L27);
   }
  }
  after_loop2: ;
  body11 = TRANS_1563748848(self, ATTR(iwp1,then_part));
  SATTR(r,guards,FLISTd805769956(ATTR(r,guards), guardt));
  SATTR(r,locks1,FLISTA475813755(ATTR(r,locks1), locks1));
  SATTR(r,stmts,FLISTd506668277(ATTR(r,stmts), body11));
  iwp1 = ATTR(iwp1,next);
 }
 after_loop1: ;
 L28 = ATTR(s,no_else);
 L291_=!(L28); 
 if (L291_) {
  SATTR(r,else_stmts,TRANS_1563748848(self, ATTR(s,else_part)));
 }
 SATTR(self,cur_lock,old_cur_lock);
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_17409519(TRANS self, AS_INT2031239268 s) {
 dAM_STMT ret_val;
 AM_ATTACH_STMT r;
 AS_CALL_EXPR callexpr;
 AS_ARG_MODE mod1;
 AS_CALL_EXPR arg11;
 AS_ARG_MODE mod2;
 AS_CALL_EXPR arg2;
 AS_ARG_MODE mod3;
 AS_INT_LIT_EXPR arg31;
 dAM_EXPR callam;
 AM_ATTACH_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ATTACH_STMT ret_val1;
 AM_ATTACH_STMT r1;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val2;
 AS_ARG_MODE create_self2;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val3;
 AS_ARG_MODE res;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val4;
 AS_ARG_MODE create_self4;
 INT create_m1 = INT_zero;
 AS_ARG_MODE ret_val5;
 AS_ARG_MODE res1;
 AS_CALL_EXPR create_self5;
 AS_CALL_EXPR ret_val6;
 AS_ARG_MODE create_self6;
 INT create_m2 = INT_zero;
 AS_ARG_MODE ret_val7;
 AS_ARG_MODE res2;
 AS_INT_LIT_EXPR create_self7;
 AS_INT_LIT_EXPR ret_val8;
 AM_FORMAL_ARG tp_self;
 dTP ret_val9;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val10;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val11;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val12;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val13;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val14;
 AM_ATTACH_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 AS_CALL_EXPR L5;
 OB L6;
 AS_ARG_MODE L7;
 OB L8;
 AS_CALL_EXPR L9;
 OB L10;
 AS_ARG_MODE L11;
 OB L12;
 AS_CALL_EXPR L13;
 OB L14;
 AS_ARG_MODE L15;
 OB L16;
 AS_INT_LIT_EXPR L17;
 OB L18;
 AM_ROUT_DEF L19;
 AM_FORMAL_ARG L201_;
 extern STR Compil628753182;
 BOOL L21;
 BOOL L22;
 extern STR name21;
 extern STR Compil832122242;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 create_self = ((AM_ATTACH_STMT) NULL);
 create_source = ATTR(s,source1);
 L2=ZALLOC(sizeof(struct AM_ATTACH_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_ATTACH_STMT_tag;
 L1 = ((AM_ATTACH_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,helper,TRANS_1711874080(self, ATTR(s,helpername)));
 SATTR(r,gate,TRANS_1711874080(self, ATTR(s,gatename)));
 L3 = (ATTR(s,at)==((dAS_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  if (ATTR(ATTR(self,prog),zones)) {
   SATTR(r,at,TRANS_693526804(self, ATTR(s,at), ((dTP) TP_BUILTIN_zone)));
  }
  else {
   SATTR(r,at,TRANS_693526804(self, ATTR(s,at), ((dTP) TP_BUILTIN_int)));
  }
 }
 create_self1 = ((AS_CALL_EXPR) NULL);
 L6=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AS_CALL_EXPR_tag;
 L5 = ((AS_CALL_EXPR) L6);
 ret_val2 = L5;
 callexpr = ret_val2;
 SATTR(callexpr,source1,ATTR(s,source1));
 SATTR(callexpr,name1,ATTR(s,routname));
 create_self2 = ((AS_ARG_MODE) NULL);
 create_m = AS_ARG1300937332;
 L8=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AS_ARG_MODE_tag;
 L7 = ((AS_ARG_MODE) L8);
 res = L7;
 SATTR(res,mod,create_m);
 ret_val3 = res;
 mod1 = ret_val3;
 create_self3 = ((AS_CALL_EXPR) NULL);
 L10=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AS_CALL_EXPR_tag;
 L9 = ((AS_CALL_EXPR) L10);
 ret_val4 = L9;
 arg11 = ret_val4;
 SATTR(arg11,source1,ATTR(s,source1));
 SATTR(arg11,name1,ATTR(s,helpername));
 create_self4 = ((AS_ARG_MODE) NULL);
 create_m1 = AS_ARG1300937332;
 L12=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AS_ARG_MODE_tag;
 L11 = ((AS_ARG_MODE) L12);
 res1 = L11;
 SATTR(res1,mod,create_m1);
 ret_val5 = res1;
 mod2 = ret_val5;
 create_self5 = ((AS_CALL_EXPR) NULL);
 L14=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AS_CALL_EXPR_tag;
 L13 = ((AS_CALL_EXPR) L14);
 ret_val6 = L13;
 arg2 = ret_val6;
 SATTR(arg2,source1,ATTR(s,source1));
 SATTR(arg2,name1,ATTR(s,gatename));
 create_self6 = ((AS_ARG_MODE) NULL);
 create_m2 = AS_ARG1300937332;
 L16=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=AS_ARG_MODE_tag;
 L15 = ((AS_ARG_MODE) L16);
 res2 = L15;
 SATTR(res2,mod,create_m2);
 ret_val7 = res2;
 mod3 = ret_val7;
 create_self7 = ((AS_INT_LIT_EXPR) NULL);
 L18=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=AS_INT_LIT_EXPR_tag;
 L17 = ((AS_INT_LIT_EXPR) L18);
 ret_val8 = L17;
 arg31 = ret_val8;
 SATTR(arg31,source1,ATTR(s,source1));
 SATTR(arg31,val1,INTI_c1588968505(((INTI1) NULL), 0));
 SATTR(arg11,next,((dAS_EXPR) arg2));
 SATTR(mod1,next,mod2);
 SATTR(arg2,next,((dAS_EXPR) arg31));
 SATTR(mod2,next,mod3);
 SATTR(callexpr,args,((dAS_EXPR) arg11));
 SATTR(callexpr,modes1,mod1);
 L19 = ATTR(self,cur_rout);
 L201_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L19,0); 
 tp_self = L201_;
 tp_self1 = ATTR(tp_self,expr);
 ret_val10 = ATTR(tp_self1,tp_at);
 ret_val9 = ret_val10;
 callam = TRANS_1705639890(self, callexpr, ret_val9, FALSE);
 if ((callam==((dAM_EXPR) NULL))) {
  err_self = self;
  err_s = ((STR) &Compil628753182);
  if ((err_self==((TRANS) NULL))) {
   L22 = TRUE;
  } else {
   L22 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L22) {
   L21 = TRUE;
  } else {
   L21 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L21) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val11 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val11;
   plus_sarg1 = err_s;
   ret_val12 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val12;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
 }
 else {
  if (callam==NULL) {
   goto other439;
  } else
  switch (TAG(callam)) {
   case AM_ROU1916046290_tag:
    SATTR(r,rout,ATTR(((AM_ROU1916046290) callam),fun)); break;
   default: ;
   other439: ;
    err_self1 = self;
    err_s1 = ((STR) &Compil832122242);
    if ((err_self1==((TRANS) NULL))) {
     L24 = TRUE;
    } else {
     L24 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L24) {
     L23 = TRUE;
    } else {
     L23 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L23) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg2 = ((STR) &name21);
     ret_val13 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val13;
     plus_sarg3 = err_s1;
     ret_val14 = STR_ap2004550586(plus_self3, plus_sarg3);
     err_s1 = ret_val14;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
  }
  SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), callam));
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1801074742(TRANS self, dAS_STMT s) {
 dAM_STMT ret_val;
 dAM_STMT r = ((dAM_STMT) NULL);
 if ((s==((dAS_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if (s==NULL) {
  goto other450;
 } else
 switch (TAG(s)) {
  case AS_DEC_STMT_tag:
   r = TRANS_1573138211(self, ((AS_DEC_STMT) s)); break;
  case AS_ASSIGN_STMT_tag:
   r = TRANS_1157876950(self, ((AS_ASSIGN_STMT) s)); break;
  case AS_IF_STMT_tag:
   r = TRANS_1261916410(self, ((AS_IF_STMT) s)); break;
  case AS_LOOP_STMT_tag:
   r = TRANS_565673800(self, ((AS_LOOP_STMT) s)); break;
  case AS_RETURN_STMT_tag:
   r = TRANS_1847483442(self, ((AS_RETURN_STMT) s)); break;
  case AS_YIELD_STMT_tag:
   r = TRANS_1066292461(self, ((AS_YIELD_STMT) s)); break;
  case AS_QUIT_STMT_tag:
   r = TRANS_1853773828(self, ((AS_QUIT_STMT) s)); break;
  case AS_CASE_STMT_tag:
   r = TRANS_1999552456(self, ((AS_CASE_STMT) s)); break;
  case AS_TYPECASE_STMT_tag:
   r = TRANS_774746956(self, ((AS_TYPECASE_STMT) s)); break;
  case AS_ASSERT_STMT_tag:
   r = TRANS_1226584966(self, ((AS_ASSERT_STMT) s)); break;
  case AS_PROTECT_STMT_tag:
   r = TRANS_741871011(self, ((AS_PROTECT_STMT) s)); break;
  case AS_RAISE_STMT_tag:
   r = TRANS_1762825757(self, ((AS_RAISE_STMT) s)); break;
  case AS_EXPR_STMT_tag:
   r = TRANS_905208628(self, ((AS_EXPR_STMT) s)); break;
  default: ;
  other450: ;
   r = TRANS_637628566(self, s);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_2091674770(TRANS self, AS_ATTACH_STMT s) {
 dAM_STMT ret_val;
 dAM_EXPR lhs_am;
 dTP lhs_tp;
 dTP rhs_tp;
 AS_TYPE_SPEC lhs_as;
 dAS_EXPR rhs;
 dAS_EXPR at = ((dAS_EXPR) NULL);
 AS_AT_EXPR atexpr = ((AS_AT_EXPR) NULL);
 dAS_EXPR rhsexpr;
 dAM_EXPR rhs_am = ((dAM_EXPR) NULL);
 dAS_STMT ret_as = ((dAS_STMT) NULL);
 dTP orig_cl_tp;
 TP_CLASS orig_class_tp = ((TP_CLASS) NULL);
 AS_CLASS_DEF orig_class_as = ((AS_CLASS_DEF) NULL);
 ARRAYdTP orig_cl_params = ((ARRAYdTP) NULL);
 INT num1 = INT_zero;
 dTP current_tp;
 TP_CLASS current_class_tp = ((TP_CLASS) NULL);
 TP_CLASS dummy1;
 IDENT newidattach = IDENT_zero;
 IDENT newidattach_cl = IDENT_zero;
 IDENT newidattach_ob = IDENT_zero;
 IDENT newidgate = IDENT_zero;
 AM_LOCAL_EXPR helper1;
 AS_ASSIGN_STMT as1;
 TRANS L11;
 AM_LOCAL_EXPR loc;
 AS_ASSIGN_STMT exp1;
 AS_CALL_EXPR rhsx;
 AS_STMT_LIST attachbody;
 AS_EXPR_STMT call11;
 AS_CALL_EXPR callexpr;
 AS_CALL_EXPR callexprob;
 AS_EXPR_STMT exprstmt;
 AS_EXPR_STMT call21;
 AS_CALL_EXPR callexpr1;
 AS_CALL_EXPR callexprob1;
 AS_INT2031239268 att8;
 TRANS attach_without_a;
 AS_ATTACH_STMT attach_without_a1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS attach_without_a2;
 AS_ATTACH_STMT attach_without_a3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS attach_without_a4;
 AS_ATTACH_STMT attach_without_a5;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val7;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 AM_FORMAL_ARG tp_self;
 dTP ret_val10;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val11;
 TRANS err_self4;
 STR err_s4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 TRANS add_helper_to_tb;
 IDENT add_helper_to_tb1 = IDENT_zero;
 ARRAYdTP add_helper_to_tb2;
 TP_CLASS ret_val14;
 TP_CLASS tp;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val15;
 TRANS err_self5;
 STR err_s5;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val16;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val17;
 IDENT next_tmp_self = IDENT_zero;
 STR next_tmp_oldiden;
 STR ret_val18;
 STR ret;
 STR create_self;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val20;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val21;
 INT str_self = INT_zero;
 STR ret_val22;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val23;
 FSTR str_self1;
 STR ret_val24;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val25;
 IDENT next_tmp_self1 = IDENT_zero;
 STR next_tmp_oldiden1;
 STR ret_val26;
 STR ret11;
 STR create_self1;
 STR ret_val27;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val28;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val29;
 INT str_self2 = INT_zero;
 STR ret_val30;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val31;
 FSTR str_self3;
 STR ret_val32;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val33;
 IDENT next_tmp_self2 = IDENT_zero;
 STR next_tmp_oldiden2;
 STR ret_val34;
 STR ret2;
 STR create_self2;
 STR ret_val35;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val36;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val37;
 INT str_self4 = INT_zero;
 STR ret_val38;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val39;
 FSTR str_self5;
 STR ret_val40;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val41;
 IDENT next_tmp_self3 = IDENT_zero;
 STR next_tmp_oldiden3;
 STR ret_val42;
 STR ret3;
 STR create_self3;
 STR ret_val43;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val44;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val45;
 INT str_self6 = INT_zero;
 STR ret_val46;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val47;
 FSTR str_self7;
 STR ret_val48;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val49;
 AS_ASSIGN_STMT create_self4;
 AS_ASSIGN_STMT ret_val50;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val51 = BOOL_zero;
 AS_ASSIGN_STMT create_self5;
 AS_ASSIGN_STMT ret_val52;
 AS_CALL_EXPR create_self6;
 AS_CALL_EXPR ret_val53;
 AS_STMT_LIST create_self7;
 AS_STMT_LIST ret_val54;
 AS_EXPR_STMT create_self8;
 AS_EXPR_STMT ret_val55;
 AS_CALL_EXPR create_self9;
 AS_CALL_EXPR ret_val56;
 AS_CALL_EXPR create_self10;
 AS_CALL_EXPR ret_val57;
 AS_ARG_MODE create_self11;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val58;
 AS_ARG_MODE res;
 AS_EXPR_STMT create_self12;
 AS_EXPR_STMT ret_val59;
 AS_EXPR_STMT create_self13;
 AS_EXPR_STMT ret_val60;
 AS_CALL_EXPR create_self14;
 AS_CALL_EXPR ret_val61;
 AS_CALL_EXPR create_self15;
 AS_CALL_EXPR ret_val62;
 AS_INT2031239268 create_self16;
 AS_INT2031239268 ret_val63;
 extern STR statem1515885938;
 BOOL L2;
 BOOL L3;
 extern STR name21;
 dAM_EXPR L4;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 extern STR statem1515885938;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 dAM_EXPR L10;
 dTP L12;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 extern STR statem1515885938;
 BOOL L19;
 BOOL L20;
 extern STR name21;
 BOOL L21;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 INT L251_;
 dPARSE L26;
 extern STR Compil608171113;
 BOOL L27;
 BOOL L28;
 extern STR name21;
 AM_ROUT_DEF L29;
 AM_FORMAL_ARG L301_;
 extern STR Compil151053839;
 BOOL L31;
 BOOL L32;
 extern STR name21;
 extern STR Compil163952783;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 BOOL L35;
 BOOL L361_;
 extern STR attach;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L37;
 BOOL L381_;
 INT L39;
 INT L401_;
 extern STR attach_cl;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L41;
 BOOL L421_;
 INT L43;
 INT L441_;
 extern STR attach_ob;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L45;
 BOOL L461_;
 INT L47;
 INT L481_;
 extern STR gate11;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L49;
 BOOL L501_;
 INT L51;
 INT L521_;
 AS_ASSIGN_STMT L53;
 OB L54;
 dAS_STMT L55;
 dAS_STMT L56;
 dAS_STMT L57;
 AM_LOCAL_EXPR L58;
 BOOL L59;
 BOOL L60;
 STR L61;
 extern STR S_;
 BOOL L62;
 BOOL L631_;
 BOOL L64;
 BOOL L65;
 BOOL L661_;
 BOOL L67;
 BOOL L68;
 BOOL L691_;
 AS_ASSIGN_STMT L70;
 OB L72;
 AS_CALL_EXPR L73;
 OB L74;
 dAS_STMT L75;
 dAS_STMT L76;
 AS_STMT_LIST L77;
 OB L78;
 BOOL L79;
 BOOL L801_;
 AS_EXPR_STMT L81;
 OB L82;
 AS_CALL_EXPR L83;
 OB L84;
 extern STR death;
 AS_CALL_EXPR L85;
 OB L86;
 AS_ARG_MODE L87;
 OB L88;
 AS_EXPR_STMT L89;
 OB L90;
 AS_EXPR_STMT L91;
 OB L92;
 AS_CALL_EXPR L93;
 OB L94;
 extern STR death;
 AS_CALL_EXPR L95;
 OB L96;
 dAS_STMT L97;
 AS_INT2031239268 L98;
 OB L99;
 dAS_STMT L100;
 dAS_STMT L101;
 if ((ATTR(s,lhs)==((dAS_EXPR) NULL))) {
  attach_without_a = self;
  attach_without_a1 = s;
  err_loc_self = attach_without_a;
  err_loc_t = ((dPROG_ERR) attach_without_a1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = attach_without_a;
  err_s = ((STR) &statem1515885938);
  if ((err_self==((TRANS) NULL))) {
   L3 = TRUE;
  } else {
   L3 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L3) {
   L2 = TRUE;
  } else {
   L2 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L2) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = err_s;
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 lhs_am = TRANS_693526804(self, ATTR(s,lhs), ((dTP) NULL));
 if ((lhs_am==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L4 = lhs_am;
 lhs_tp = (*dAM_EXPR_tprdTP[TAG(L4)])(L4);
 rhs_tp = ((dTP) NULL);
 L5 = lhs_am;
 L6 = TRANS_309326235(self, (*dAM_EXPR_tprdTP[TAG(L5)])(L5));
 L71_=!(L6); 
 if (L71_) {
  attach_without_a2 = self;
  attach_without_a3 = s;
  err_loc_self1 = attach_without_a2;
  err_loc_t1 = ((dPROG_ERR) attach_without_a3);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = attach_without_a2;
  err_s1 = ((STR) &statem1515885938);
  if ((err_self1==((TRANS) NULL))) {
   L9 = TRUE;
  } else {
   L9 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L9) {
   L8 = TRUE;
  } else {
   L8 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L8) {
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  else {
   plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
   plus_sarg2 = ((STR) &name21);
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = err_s1;
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s1 = ret_val4;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
 }
 L10 = lhs_am;
 L12 = (*dAM_EXPR_tprdTP[TAG(L10)])(L10);
 lhs_as = (*dTP_as1220129267[TAG(L12)])(L12);
 if (lhs_am==NULL) {
  goto other443;
 } else
 switch (TAG(lhs_am)) {
  case AM_LOCAL_EXPR_tag:
   L13 = (ATTR(((AM_LOCAL_EXPR) lhs_am),as_type)==((AS_TYPE_SPEC) NULL));
   L141_=!(L13); 
   if (L141_) {
    lhs_as = ATTR(((AM_LOCAL_EXPR) lhs_am),as_type);
   } break;
  default: ;
  other443: ;
 }
 L15 = (ATTR(lhs_as,params)==((AS_TYPE_SPEC) NULL));
 L161_=!(L15); 
 if (L161_) {
  L17 = (ATTR(ATTR(lhs_as,params),next)==((AS_TYPE_SPEC) NULL));
  L181_=!(L17); 
  if (L181_) {
   attach_without_a4 = self;
   attach_without_a5 = s;
   err_loc_self2 = attach_without_a4;
   err_loc_t2 = ((dPROG_ERR) attach_without_a5);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = attach_without_a4;
   err_s2 = ((STR) &statem1515885938);
   if ((err_self2==((TRANS) NULL))) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L19) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val5;
    plus_sarg5 = err_s2;
    ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val6;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  tp_of_self = self;
  tp_of_t = ATTR(lhs_as,params);
  ret_val7 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  rhs_tp = ret_val7;
 }
 rhs = ATTR(s,rhs);
 rhsexpr = ATTR(s,rhs);
 while (1) {
  if (rhsexpr==NULL) {
   goto other444;
  } else
  switch (TAG(rhsexpr)) {
   case AS_AT_EXPR_tag:
    atexpr = ((AS_AT_EXPR) rhsexpr);
    if ((at==((dAS_EXPR) NULL))) {
     at = ATTR(atexpr,at);
    } break;
   default: ;
   other444: ;
    goto after_loop;
  }
  rhsexpr = ATTR(atexpr,e);
 }
 after_loop: ;
 rhs = rhsexpr;
 if (rhs==NULL) {
  goto other445;
 } else
 switch (TAG(rhs)) {
  case AS_CALL_EXPR_tag:
   L21 = (rhs_tp==((dTP) NULL));
   L221_=!(L21); 
   rhs_am = TRANS_1705639890(self, ((AS_CALL_EXPR) rhs), rhs_tp, L221_); break;
  default: ;
  other445: ;
   rhs_am = TRANS_693526804(self, rhs, rhs_tp);
 }
 if ((rhs_am==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 orig_cl_tp = ATTR(ATTR(ATTR(self,cur_rout),srcsig),tp);
 if (orig_cl_tp==NULL) {
  goto other446;
 } else
 switch (TAG(orig_cl_tp)) {
  case TP_CLASS_tag:
   orig_class_tp = ((TP_CLASS) orig_cl_tp);
   orig_cl_params = ATTR(orig_class_tp,params);
   num1 = 0;
   L23 = (orig_cl_params==((ARRAYdTP) NULL));
   L241_=!(L23); 
   if (L241_) {
    L251_=(orig_cl_params)==NULL?0:ASIZE((ARRAYdTP)orig_cl_params); 
    num1 = L251_;
   }
   L26 = ATTR(ATTR(self,prog),parse);
   orig_class_as = (*dPARSE1581792481[TAG(L26)])(L26, ATTR(orig_class_tp,name1), num1); break;
  default: ;
  other446: ;
   err_self3 = self;
   err_s3 = ((STR) &Compil608171113);
   if ((err_self3==((TRANS) NULL))) {
    L28 = TRUE;
   } else {
    L28 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L28) {
    L27 = TRUE;
   } else {
    L27 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L27) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg6 = ((STR) &name21);
    ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val8;
    plus_sarg7 = err_s3;
    ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s3 = ret_val9;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
 }
 L29 = ATTR(self,cur_rout);
 L301_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L29,0); 
 tp_self = L301_;
 tp_self1 = ATTR(tp_self,expr);
 ret_val11 = ATTR(tp_self1,tp_at);
 ret_val10 = ret_val11;
 current_tp = ret_val10;
 if (current_tp==NULL) {
  goto other447;
 } else
 switch (TAG(current_tp)) {
  case TP_CLASS_tag:
   current_class_tp = ((TP_CLASS) current_tp); break;
  default: ;
  other447: ;
   err_self4 = self;
   err_s4 = ((STR) &Compil151053839);
   if ((err_self4==((TRANS) NULL))) {
    L32 = TRUE;
   } else {
    L32 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L32) {
    L31 = TRUE;
   } else {
    L31 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L31) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val12;
    plus_sarg9 = err_s4;
    ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s4 = ret_val13;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
 }
 if (ATTR(s,transformed)) {
  add_helper_to_tb = self;
  add_helper_to_tb1 = ATTR(s,helper_class);
  add_helper_to_tb2 = orig_cl_params;
  tp_class_for_sel = ATTR(ATTR(add_helper_to_tb,prog),tp_tbl);
  tp_class_for_nam = add_helper_to_tb1;
  tp_class_for_par = add_helper_to_tb2;
  ret_val15 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
  tp = ret_val15;
  if ((tp==((TP_CLASS) NULL))) {
   err_self5 = add_helper_to_tb;
   err_s5 = ((STR) &Compil163952783);
   if ((err_self5==((TRANS) NULL))) {
    L34 = TRUE;
   } else {
    L34 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L34) {
    L33 = TRUE;
   } else {
    L33 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
   }
   if (L33) {
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val16 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val16;
    plus_sarg11 = err_s5;
    ret_val17 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s5 = ret_val17;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
  }
  L35 = FSETdT1764379880(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp));
  L361_=!(L35); 
  if (L361_) {
   SATTR(ATTR(add_helper_to_tb,prog),tp_done,FSETdT1016814448(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp)));
  }
  ret_val14 = tp;
  dummy1 = ret_val14;
  TRANS_1069298803(self, current_class_tp, ATTR(s,rout));
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 next_tmp_self = IDENT_zero;
 next_tmp_oldiden = ((STR) &attach);
 create_self = ((STR) NULL);
 ret_val19 = ((STR) &name1);
 ret = ret_val19;
 plus_self12 = ((STR) &S_pS_);
 plus_sarg12 = next_tmp_oldiden;
 ret_val20 = STR_ap2004550586(plus_self12, plus_sarg12);
 plus_self13 = ret_val20;
 plus_sarg13 = ((STR) &S_);
 ret_val21 = STR_ap2004550586(plus_self13, plus_sarg13);
 plus_self14 = ret_val21;
 str_self = IDENT_tmp_count;
 clear_self = buf1;
 L37 = (clear_self==((FSTR) NULL));
 L381_=!(L37); 
 if (L381_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val23 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val23;
 str_self1 = buf1;
 ret_val24 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val22 = ret_val24;
 plus_sarg14 = ret_val22;
 ret_val25 = STR_ap2004550586(plus_self14, plus_sarg14);
 ret = ret_val25;
 L39 = IDENT_tmp_count;
 L401_=INTPLUS(L39,1); 
 IDENT_tmp_count = L401_;
 ret_val18 = ret;
 newidattach = IDENT_1150413730(IDENT_zero, ret_val18);
 next_tmp_self1 = IDENT_zero;
 next_tmp_oldiden1 = ((STR) &attach_cl);
 create_self1 = ((STR) NULL);
 ret_val27 = ((STR) &name1);
 ret11 = ret_val27;
 plus_self15 = ((STR) &S_pS_);
 plus_sarg15 = next_tmp_oldiden1;
 ret_val28 = STR_ap2004550586(plus_self15, plus_sarg15);
 plus_self16 = ret_val28;
 plus_sarg16 = ((STR) &S_);
 ret_val29 = STR_ap2004550586(plus_self16, plus_sarg16);
 plus_self17 = ret_val29;
 str_self2 = IDENT_tmp_count;
 clear_self1 = buf2;
 L41 = (clear_self1==((FSTR) NULL));
 L421_=!(L41); 
 if (L421_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val31 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val31;
 str_self3 = buf2;
 ret_val32 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val30 = ret_val32;
 plus_sarg17 = ret_val30;
 ret_val33 = STR_ap2004550586(plus_self17, plus_sarg17);
 ret11 = ret_val33;
 L43 = IDENT_tmp_count;
 L441_=INTPLUS(L43,1); 
 IDENT_tmp_count = L441_;
 ret_val26 = ret11;
 newidattach_cl = IDENT_1150413730(IDENT_zero, ret_val26);
 next_tmp_self2 = IDENT_zero;
 next_tmp_oldiden2 = ((STR) &attach_ob);
 create_self2 = ((STR) NULL);
 ret_val35 = ((STR) &name1);
 ret2 = ret_val35;
 plus_self18 = ((STR) &S_pS_);
 plus_sarg18 = next_tmp_oldiden2;
 ret_val36 = STR_ap2004550586(plus_self18, plus_sarg18);
 plus_self19 = ret_val36;
 plus_sarg19 = ((STR) &S_);
 ret_val37 = STR_ap2004550586(plus_self19, plus_sarg19);
 plus_self20 = ret_val37;
 str_self4 = IDENT_tmp_count;
 clear_self2 = buf3;
 L45 = (clear_self2==((FSTR) NULL));
 L461_=!(L45); 
 if (L461_) {
  SATTR(clear_self2,loc,0);
 }
 str_in_self2 = str_self4;
 str_in_s2 = buf3;
 ret_val39 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
 buf3 = ret_val39;
 str_self5 = buf3;
 ret_val40 = STR_fr1097270334(((STR) NULL), str_self5);
 ret_val38 = ret_val40;
 plus_sarg20 = ret_val38;
 ret_val41 = STR_ap2004550586(plus_self20, plus_sarg20);
 ret2 = ret_val41;
 L47 = IDENT_tmp_count;
 L481_=INTPLUS(L47,1); 
 IDENT_tmp_count = L481_;
 ret_val34 = ret2;
 newidattach_ob = IDENT_1150413730(IDENT_zero, ret_val34);
 next_tmp_self3 = IDENT_zero;
 next_tmp_oldiden3 = ((STR) &gate11);
 create_self3 = ((STR) NULL);
 ret_val43 = ((STR) &name1);
 ret3 = ret_val43;
 plus_self21 = ((STR) &S_pS_);
 plus_sarg21 = next_tmp_oldiden3;
 ret_val44 = STR_ap2004550586(plus_self21, plus_sarg21);
 plus_self22 = ret_val44;
 plus_sarg22 = ((STR) &S_);
 ret_val45 = STR_ap2004550586(plus_self22, plus_sarg22);
 plus_self23 = ret_val45;
 str_self6 = IDENT_tmp_count;
 clear_self3 = buf4;
 L49 = (clear_self3==((FSTR) NULL));
 L501_=!(L49); 
 if (L501_) {
  SATTR(clear_self3,loc,0);
 }
 str_in_self3 = str_self6;
 str_in_s3 = buf4;
 ret_val47 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
 buf4 = ret_val47;
 str_self7 = buf4;
 ret_val48 = STR_fr1097270334(((STR) NULL), str_self7);
 ret_val46 = ret_val48;
 plus_sarg23 = ret_val46;
 ret_val49 = STR_ap2004550586(plus_self23, plus_sarg23);
 ret3 = ret_val49;
 L51 = IDENT_tmp_count;
 L521_=INTPLUS(L51,1); 
 IDENT_tmp_count = L521_;
 ret_val42 = ret3;
 newidgate = IDENT_1150413730(IDENT_zero, ret_val42);
 SATTR(s,helper_class,newidattach_cl);
 helper1 = TRANS_1496987093(self, ((dAS_STMT) s), newidattach_ob, newidattach_cl, orig_class_as, orig_cl_params);
 create_self4 = ((AS_ASSIGN_STMT) NULL);
 L54=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
 if (L54==NULL) FATAL("Unable to allocate more memory");
 ((OB)L54)->header.tag=AS_ASSIGN_STMT_tag;
 L53 = ((AS_ASSIGN_STMT) L54);
 ret_val50 = L53;
 as1 = ret_val50;
 SATTR(as1,source1,ATTR(s,source1));
 SATTR(as1,surr_stmt_list,ATTR(s,surr_stmt_list));
 SATTR(as1,name1,newidgate);
 SATTR(as1,rhs,ATTR(s,lhs));
 if ((ret_as==((dAS_STMT) NULL))) {
  ret_as = ((dAS_STMT) as1);
 }
 else {
  L55 = ret_as;
  (*dAS_ST1454156204[TAG(L55)])(L55, ((dAS_STMT) as1));
 }
 L56 = ret_as;
 (*dAS_ST1454156204[TAG(L56)])(L56, ((dAS_STMT) TRANS_1390141743(self, newidattach_ob, newidattach_cl, orig_class_as, ATTR(s,surr_stmt_list), ATTR(s,source1))));
 L57 = ret_as;
 (*dAS_ST1454156204[TAG(L57)])(L57, ((dAS_STMT) TRANS_2064676398(self, newidattach_ob, ATTR(s,surr_stmt_list), ATTR(s,source1))));
 {
  struct TRANS_1349638895_frame_struct other2_0;
TRANS_1349638895_frame noname1 = &other2_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L58 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   loc = L58;
   L61 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L62 = STR_is111530248(L61, ((STR) &S_));
   L631_=!(L62); 
   if (L631_) {
    L60 = TRUE;
   } else {
    L65 = (ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL));
    L661_=!(L65); 
    if (L661_) {
     is_eq_self = ATTR(loc,name1);
     is_eq_i = ATTR(ATTR(self,cur_cohort),name1);
     ret_val51 = (is_eq_self.str==is_eq_i.str);
     L64 = ret_val51;
    } else {
     L64 = FALSE;
    }
    L60 = L64;
   }
   if (L60) {
    if ((ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL))) {
     L67 = TRUE;
    } else {
     L68 = TRANS_157729662(self, ATTR(loc,name1), ATTR(self,cur_param_ob));
     L691_=!(L68); 
     L67 = L691_;
    }
    L59 = L67;
   } else {
    L59 = FALSE;
   }
   if (L59) {
    create_self5 = ((AS_ASSIGN_STMT) NULL);
    L72=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
    if (L72==NULL) FATAL("Unable to allocate more memory");
    ((OB)L72)->header.tag=AS_ASSIGN_STMT_tag;
    L70 = ((AS_ASSIGN_STMT) L72);
    ret_val52 = L70;
    exp1 = ret_val52;
    SATTR(exp1,source1,ATTR(s,source1));
    SATTR(exp1,surr_stmt_list,ATTR(s,surr_stmt_list));
    SATTR(exp1,lhs_expr,((dAS_EXPR) TRANS_1849744987(self, ATTR(loc,name1), helper1, ATTR(s,source1))));
    create_self6 = ((AS_CALL_EXPR) NULL);
    L74=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L74==NULL) FATAL("Unable to allocate more memory");
    ((OB)L74)->header.tag=AS_CALL_EXPR_tag;
    L73 = ((AS_CALL_EXPR) L74);
    ret_val53 = L73;
    rhsx = ret_val53;
    SATTR(rhsx,source1,ATTR(s,source1));
    SATTR(rhsx,name1,ATTR(loc,name1));
    SATTR(exp1,rhs,((dAS_EXPR) rhsx));
    L75 = ret_as;
    (*dAS_ST1454156204[TAG(L75)])(L75, ((dAS_STMT) exp1));
   }
  }
 }
 after_loop1: ;
 L76 = ret_as;
 (*dAS_ST1454156204[TAG(L76)])(L76, TRANS_166911016(self, TRANS_194341863, ATTR(s,surr_stmt_list), ATTR(s,source1)));
 create_self7 = ((AS_STMT_LIST) NULL);
 L78=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L78==NULL) FATAL("Unable to allocate more memory");
 ((OB)L78)->header.tag=AS_STMT_LIST_tag;
 L77 = ((AS_STMT_LIST) L78);
 ret_val54 = L77;
 attachbody = ret_val54;
 SATTR(attachbody,source1,ATTR(s,source1));
 L79 = (rhs_tp==((dTP) NULL));
 L801_=!(L79); 
 if (L801_) {
  create_self8 = ((AS_EXPR_STMT) NULL);
  L82=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
  if (L82==NULL) FATAL("Unable to allocate more memory");
  ((OB)L82)->header.tag=AS_EXPR_STMT_tag;
  L81 = ((AS_EXPR_STMT) L82);
  ret_val55 = L81;
  call11 = ret_val55;
  SATTR(call11,source1,ATTR(s,source1));
  SATTR(call11,surr_stmt_list,attachbody);
  create_self9 = ((AS_CALL_EXPR) NULL);
  L84=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L84==NULL) FATAL("Unable to allocate more memory");
  ((OB)L84)->header.tag=AS_CALL_EXPR_tag;
  L83 = ((AS_CALL_EXPR) L84);
  ret_val56 = L83;
  callexpr = ret_val56;
  SATTR(callexpr,source1,ATTR(s,source1));
  SATTR(callexpr,name1,IDENT_1150413730(IDENT_zero, ((STR) &death)));
  create_self10 = ((AS_CALL_EXPR) NULL);
  L86=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L86==NULL) FATAL("Unable to allocate more memory");
  ((OB)L86)->header.tag=AS_CALL_EXPR_tag;
  L85 = ((AS_CALL_EXPR) L86);
  ret_val57 = L85;
  callexprob = ret_val57;
  SATTR(callexprob,source1,ATTR(s,source1));
  SATTR(callexprob,name1,newidgate);
  SATTR(callexpr,ob,((dAS_EXPR) callexprob));
  SATTR(callexpr,args,rhs);
  create_self11 = ((AS_ARG_MODE) NULL);
  create_m = AS_ARG1300937332;
  L88=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
  if (L88==NULL) FATAL("Unable to allocate more memory");
  ((OB)L88)->header.tag=AS_ARG_MODE_tag;
  L87 = ((AS_ARG_MODE) L88);
  res = L87;
  SATTR(res,mod,create_m);
  ret_val58 = res;
  SATTR(callexpr,modes1,ret_val58);
  SATTR(call11,e,((dAS_EXPR) callexpr));
  SATTR(call11,transformed,TRUE);
  SATTR(attachbody,stmts,((dAS_STMT) call11));
 }
 else {
  create_self12 = ((AS_EXPR_STMT) NULL);
  L90=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
  if (L90==NULL) FATAL("Unable to allocate more memory");
  ((OB)L90)->header.tag=AS_EXPR_STMT_tag;
  L89 = ((AS_EXPR_STMT) L90);
  ret_val59 = L89;
  exprstmt = ret_val59;
  SATTR(exprstmt,source1,ATTR(s,source1));
  SATTR(exprstmt,surr_stmt_list,attachbody);
  SATTR(exprstmt,e,rhs);
  SATTR(attachbody,stmts,((dAS_STMT) exprstmt));
  create_self13 = ((AS_EXPR_STMT) NULL);
  L92=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
  if (L92==NULL) FATAL("Unable to allocate more memory");
  ((OB)L92)->header.tag=AS_EXPR_STMT_tag;
  L91 = ((AS_EXPR_STMT) L92);
  ret_val60 = L91;
  call21 = ret_val60;
  SATTR(call21,source1,ATTR(s,source1));
  SATTR(call21,surr_stmt_list,attachbody);
  create_self14 = ((AS_CALL_EXPR) NULL);
  L94=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L94==NULL) FATAL("Unable to allocate more memory");
  ((OB)L94)->header.tag=AS_CALL_EXPR_tag;
  L93 = ((AS_CALL_EXPR) L94);
  ret_val61 = L93;
  callexpr1 = ret_val61;
  SATTR(callexpr1,source1,ATTR(s,source1));
  SATTR(callexpr1,name1,IDENT_1150413730(IDENT_zero, ((STR) &death)));
  create_self15 = ((AS_CALL_EXPR) NULL);
  L96=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L96==NULL) FATAL("Unable to allocate more memory");
  ((OB)L96)->header.tag=AS_CALL_EXPR_tag;
  L95 = ((AS_CALL_EXPR) L96);
  ret_val62 = L95;
  callexprob1 = ret_val62;
  SATTR(callexprob1,source1,ATTR(s,source1));
  SATTR(callexprob1,name1,newidgate);
  SATTR(callexpr1,ob,((dAS_EXPR) callexprob1));
  SATTR(call21,e,((dAS_EXPR) callexpr1));
  SATTR(call21,transformed,TRUE);
  L97 = ATTR(attachbody,stmts);
  (*dAS_ST1454156204[TAG(L97)])(L97, ((dAS_STMT) call21));
 }
 SATTR(s,rout,TRANS_725662504(self, newidattach, newidattach_ob, newidattach_cl, helper1, orig_class_as, current_class_tp, newidgate, lhs_as, attachbody, TRANS_att_code, ATTR(s,source1)));
 create_self16 = ((AS_INT2031239268) NULL);
 L99=ZALLOC(sizeof(struct AS_INT2031239268_struct));
 if (L99==NULL) FATAL("Unable to allocate more memory");
 ((OB)L99)->header.tag=AS_INT2031239268_tag;
 L98 = ((AS_INT2031239268) L99);
 ret_val63 = L98;
 att8 = ret_val63;
 SATTR(att8,source1,ATTR(s,source1));
 SATTR(att8,surr_stmt_list,ATTR(s,surr_stmt_list));
 SATTR(att8,routname,newidattach);
 SATTR(att8,helpername,newidattach_ob);
 SATTR(att8,gatename,newidgate);
 SATTR(att8,at,at);
 L100 = ret_as;
 (*dAS_ST1454156204[TAG(L100)])(L100, ((dAS_STMT) att8));
 L101 = ret_as;
 (*dAS_ST1454156204[TAG(L101)])(L101, ATTR(s,next));
 SATTR(s,next,ret_as);
 SATTR(s,dont_print,TRUE);
 ret_val = ((dAM_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_338440751(TRANS self, AS_WIT1063437351 s) {
 dAM_STMT ret_val;
 AM_WIT1996971603 r;
 AS_IDENT_LIST obj1;
 AM_LOCAL_EXPR l;
 INT k = INT_zero;
 ARRAYdAM_EXPR L11;
 AM_WIT1996971603 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WIT1996971603 ret_val1;
 AM_WIT1996971603 r1;
 ARRAYdAM_EXPR create_self1;
 INT create_n = INT_zero;
 ARRAYdAM_EXPR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 TRANS with_near_locals;
 AS_IDENT_LIST with_near_locals1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val9;
 TRANS with_near_non_re;
 AS_IDENT_LIST with_near_non_re1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 AM_ROUT_DEF self_local_self;
 AM_LOCAL_EXPR ret_val14;
 AM_WIT1996971603 L2;
 OB L3;
 ARRAYdAM_EXPR L4;
 INT L5;
 OB L6;
 INT L7;
 extern STR Compil1673539783;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 BOOL L12;
 BOOL L131_;
 extern STR withnearcantuse;
 extern STR Locals698775910;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 dTP L16;
 BOOL L17;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L211_;
 INT L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 extern STR withnearcantuse1;
 extern STR Refere2000259175;
 BOOL L26;
 BOOL L27;
 extern STR name21;
 INT L101_br;
INT i_L101_=0;
 ARRAYdAM_EXPR L28;
 INT L29;
 INT L301_;
 INT L31;
 AM_FORMAL_ARG L321_;
 AM_FORMAL_ARG L33;
 AM_LOCAL_EXPR L34;
 create_self = ((AM_WIT1996971603) NULL);
 create_source = ATTR(s,source1);
 L3=ZALLOC(sizeof(struct AM_WIT1996971603_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AM_WIT1996971603_tag;
 L2 = ((AM_WIT1996971603) L3);
 r1 = L2;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 create_self1 = ((ARRAYdAM_EXPR) NULL);
 create_n = ATTR(s,elts_size);
 L5 = create_n;
 L7=(sizeof(struct ARRAYdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L5)*sizeof(dAM_EXPR);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYdAM_EXPR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYdAM_EXPR) L6);
 L4->asize = L5;
 ret_val2 = L4;
 SATTR(r,objects1,ret_val2);
 obj1 = ATTR(s,idents);
 if ((ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL))) {
  err_self = self;
  err_s = ((STR) &Compil1673539783);
  if ((err_self==((TRANS) NULL))) {
   L9 = TRUE;
  } else {
   L9 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L9) {
   L8 = TRUE;
  } else {
   L8 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L8) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val3;
   plus_sarg1 = err_s;
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val4;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
 }
 {
  BOOL f_L101_ = TRUE;
  while (1) {
   L12 = (obj1==((AS_IDENT_LIST) NULL));
   L131_=!(L12); 
   if (L131_) {
   }
   else {
    goto after_loop;
   }
   l = TRANS_1711874080(self, ATTR(obj1,name1));
   if ((l==((AM_LOCAL_EXPR) NULL))) {
    with_near_locals = self;
    with_near_locals1 = obj1;
    err_loc_self = with_near_locals;
    err_loc_t = ((dPROG_ERR) with_near_locals1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self1 = with_near_locals;
    plus_self2 = ((STR) &withnearcantuse);
    plus_sarg2 = ATTR(with_near_locals1,name1).str;
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val5;
    plus_sarg3 = ((STR) &Locals698775910);
    ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val6;
    if ((err_self1==((TRANS) NULL))) {
     L15 = TRUE;
    } else {
     L15 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L15) {
     L14 = TRUE;
    } else {
     L14 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L14) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self4 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val7;
     plus_sarg5 = err_s1;
     ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s1 = ret_val8;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   }
   tp_self = l;
   ret_val9 = ATTR(tp_self,tp_at);
   L16 = ret_val9;
   k = (*dTP_kindrINT[TAG(L16)])(L16);
   L18 = TP_KIND_ref_tp;
   L191_=(k)==(L18); 
   L20 = L191_;
   L211_=!(L20); 
   if (L211_) {
    L22 = TP_KIND_spr_tp;
    L231_=(k)==(L22); 
    L24 = L231_;
    L251_=!(L24); 
    L17 = L251_;
   } else {
    L17 = FALSE;
   }
   if (L17) {
    with_near_non_re = self;
    with_near_non_re1 = obj1;
    err_loc_self1 = with_near_non_re;
    err_loc_t1 = ((dPROG_ERR) with_near_non_re1);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self2 = with_near_non_re;
    plus_self6 = ((STR) &withnearcantuse1);
    plus_sarg6 = ATTR(with_near_non_re1,name1).str;
    ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val10;
    plus_sarg7 = ((STR) &Refere2000259175);
    ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s2 = ret_val11;
    if ((err_self2==((TRANS) NULL))) {
     L27 = TRUE;
    } else {
     L27 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L27) {
     L26 = TRUE;
    } else {
     L26 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L26) {
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    else {
     plus_self8 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
     plus_sarg8 = ((STR) &name21);
     ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
     plus_self9 = ret_val12;
     plus_sarg9 = err_s2;
     ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
     err_s2 = ret_val13;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   }
   if (f_L101_) {
    f_L101_ = FALSE;
    L11 = ATTR(r,objects1);
    L101_br=L11==NULL?0:ASIZE((ARRAYdAM_EXPR)L11); 
    i_L101_=0;
   }
   if(i_L101_>=L101_br)  goto after_loop; 
   SARR((ARRAYdAM_EXPR)L11,i_L101_,(dAM_EXPR)l); i_L101_++;
   ;
   obj1 = ATTR(obj1,next);
  }
 }
 after_loop: ;
 if (ATTR(s,self_occurred)) {
  L28 = ATTR(r,objects1);
  L29 = ATTR(s,elts_size);
  L301_=INTMINUS(L29,1); 
  L31 = L301_;
  self_local_self = ATTR(self,cur_rout);
  L321_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)self_local_self,0); 
  L33=L321_;
  ret_val14 = ATTR(L33,expr);
  L34 = ret_val14;
  SARR((ARRAYdAM_EXPR)L28,L31,(dAM_EXPR)L34); 
  ;
 }
 SATTR(r,near_part,TRANS_1563748848(self, ATTR(s,near_part)));
 SATTR(r,else_part,TRANS_1563748848(self, ATTR(s,else_part)));
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_342518622(TRANS self, AS_UNLOCK_STMT s) {
 dAM_STMT ret_val;
 dAM_STMT ret = ((dAM_STMT) NULL);
 dAS_STMT prev;
 dAS_STMT export11;
 AM_UNLOCK_STMT r;
 TRANS unlock_in_par_fo;
 AS_UNLOCK_STMT unlock_in_par_fo1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS unlock_outside_l;
 AS_UNLOCK_STMT unlock_outside_l1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 AM_UNLOCK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_UNLOCK_STMT ret_val5;
 AM_UNLOCK_STMT r1;
 BOOL L1;
 BOOL L21_;
 extern STR unlock800726374;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR unlock1640206679;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 BOOL L7;
 BOOL L81_;
 dAS_STMT L9;
 dAS_STMT L10;
 AM_UNLOCK_STMT L11;
 OB L12;
 dAM_STMT L13;
 L1 = TRANS_1229338887(self);
 L21_=!(L1); 
 if (L21_) {
  if (TRANS_2091440962(self)) {
   unlock_in_par_fo = self;
   unlock_in_par_fo1 = s;
   err_loc_self = unlock_in_par_fo;
   err_loc_t = ((dPROG_ERR) unlock_in_par_fo1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = unlock_in_par_fo;
   err_s = ((STR) &unlock800726374);
   if ((err_self==((TRANS) NULL))) {
    L4 = TRUE;
   } else {
    L4 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L4) {
    L3 = TRUE;
   } else {
    L3 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L3) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = err_s;
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val2;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
  }
  else {
   unlock_outside_l = self;
   unlock_outside_l1 = s;
   err_loc_self1 = unlock_outside_l;
   err_loc_t1 = ((dPROG_ERR) unlock_outside_l1);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   err_self1 = unlock_outside_l;
   err_s1 = ((STR) &unlock1640206679);
   if ((err_self1==((TRANS) NULL))) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L6) {
    L5 = TRUE;
   } else {
    L5 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L5) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s1;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val4;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L7 = ATTR(s,transformed);
 L81_=!(L7); 
 if (L81_) {
  prev = TRANS_954470687(self, ((dAS_STMT) s));
  export11 = TRANS_166911016(self, TRANS_194341863, ATTR(s,surr_stmt_list), ATTR(s,source1));
  L9 = export11;
  (*dAS_ST1687647066[TAG(L9)])(L9, ((dAS_STMT) s));
  if ((prev==((dAS_STMT) NULL))) {
   SATTR(ATTR(s,surr_stmt_list),stmts,export11);
  }
  else {
   L10 = prev;
   (*dAS_ST1687647066[TAG(L10)])(L10, export11);
  }
  ret = TRANS_1801074742(self, export11);
 }
 create_self = ((AM_UNLOCK_STMT) NULL);
 create_source = ATTR(s,source1);
 L12=ZALLOC(sizeof(struct AM_UNLOCK_STMT_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_UNLOCK_STMT_tag;
 L11 = ((AM_UNLOCK_STMT) L12);
 r1 = L11;
 SATTR(r1,source1,create_source);
 ret_val5 = r1;
 r = ret_val5;
 if ((ret==((dAM_STMT) NULL))) {
  ret = ((dAM_STMT) r);
 }
 else {
  L13 = ret;
  (*dAM_ST2020072576[TAG(L13)])(L13, ((dAM_STMT) r));
 }
 SATTR(r,lock_ob,TRANS_693526804(self, ATTR(s,e), ((dTP) TP_BUI1813155122)));
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_637628566(TRANS self, dAS_STMT s) {
 dAM_STMT ret_val;
 dAM_STMT r = ((dAM_STMT) NULL);
 BOOL was1 = BOOL_zero;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 TRANS fork_outside_par;
 AS_FORK_STMT fork_outside_par1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS warning_self;
 STR warning_s;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR forkst2003855832;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 BOOL L6;
 BOOL L71_;
 extern STR Emitti760829686;
 if ((s==((dAS_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(psather_self,distributed);
 }
 ret_val1 = L1;
 if (ret_val1) {
  switch (TAG(s)) {
   case AS_PAR_STMT_tag:
    r = TRANS_747994499(self, ((AS_PAR_STMT) s)); break;
   case AS_INT2031239268_tag:
    r = TRANS_17409519(self, ((AS_INT2031239268) s)); break;
   case AS_LOCK_STMT_tag:
    r = TRANS_1603674908(self, ((AS_LOCK_STMT) s)); break;
   case AS_UNLOCK_STMT_tag:
    r = TRANS_342518622(self, ((AS_UNLOCK_STMT) s)); break;
   case AS_WIT1063437351_tag:
    r = TRANS_338440751(self, ((AS_WIT1063437351) s)); break;
   case AS_ATTACH_STMT_tag:
    r = TRANS_2091674770(self, ((AS_ATTACH_STMT) s)); break;
   case AS_FORK_STMT_tag:
    r = TRANS_1403806496(self, ((AS_FORK_STMT) s)); break;
   case AS_SYNC_STMT_tag:
    r = TRANS_1055378524(self, ((AS_SYNC_STMT) s)); break;
   default: ;
    FATAL("No applicable type in typecase\nin TRANS::transform_pSather_stmt($AS_STMT):$AM_STMT\n./Middle/trans.sa:3976:17");
  }
 }
 else {
  switch (TAG(s)) {
   case AS_PAR_STMT_tag:
    was1 = ATTR(self,is_in_par);
    SATTR(self,is_in_par,TRUE);
    r = TRANS_1563748848(self, ATTR(((AS_PAR_STMT) s),body));
    SATTR(self,is_in_par,was1); break;
   case AS_FORK_STMT_tag:
    L2 = ATTR(self,is_in_par);
    L31_=!(L2); 
    if (L31_) {
     fork_outside_par = self;
     fork_outside_par1 = ((AS_FORK_STMT) s);
     err_loc_self = fork_outside_par;
     err_loc_t = ((dPROG_ERR) fork_outside_par1);
     PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
     err_self = fork_outside_par;
     err_s = ((STR) &forkst2003855832);
     if ((err_self==((TRANS) NULL))) {
      L5 = TRUE;
     } else {
      L5 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L5) {
      L4 = TRUE;
     } else {
      L4 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
     }
     if (L4) {
      PROG_err_STR(ATTR(err_self,prog), err_s);
     }
     else {
      plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
      plus_sarg = ((STR) &name21);
      ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
      plus_self1 = ret_val2;
      plus_sarg1 = err_s;
      ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
      err_s = ret_val3;
      PROG_err_STR(ATTR(err_self,prog), err_s);
     }
     ret_val = ((dAM_STMT) NULL);
     return ret_val;
    }
    L6 = (ATTR(((AS_FORK_STMT) s),at)==((dAS_EXPR) NULL));
    L71_=!(L6); 
    if (L71_) {
     err_loc_self1 = self;
     err_loc_t1 = ((dPROG_ERR) s);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     warning_self = self;
     warning_s = ((STR) &Emitti760829686);
     PROG_warning_STR(ATTR(warning_self,prog), warning_s);
    }
    r = TRANS_1563748848(self, ATTR(((AS_FORK_STMT) s),body)); break;
   default: ;
    FATAL("No applicable type in typecase\nin TRANS::transform_pSather_stmt($AS_STMT):$AM_STMT\n./Middle/trans.sa:3990:17");
  }
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_747994499(TRANS self, AS_PAR_STMT s) {
 dAM_STMT ret_val;
 BOOL in_simple_par_ol = BOOL_zero;
 dAM_STMT res;
 dAS_STMT ret_as = ((dAS_STMT) NULL);
 dTP orig_cl_tp;
 TP_CLASS orig_class_tp = ((TP_CLASS) NULL);
 AS_CLASS_DEF orig_class_as = ((AS_CLASS_DEF) NULL);
 ARRAYdTP orig_cl_params = ((ARRAYdTP) NULL);
 INT num1 = INT_zero;
 dTP current_tp;
 TP_CLASS current_class_tp = ((TP_CLASS) NULL);
 TP_CLASS dummy1;
 IDENT newidpar = IDENT_zero;
 IDENT newidpar_cl = IDENT_zero;
 IDENT newidpar_ob = IDENT_zero;
 IDENT newidcohort = IDENT_zero;
 AM_LOCAL_EXPR helper1;
 AS_ASSIGN_STMT asc;
 AS_TYPE_SPEC tpc;
 AS_CREATE_EXPR rhsc;
 TRANS L11;
 AM_LOCAL_EXPR loc;
 AS_ASSIGN_STMT exp1;
 AS_CALL_EXPR rhs;
 AS_INT2031239268 att8;
 AS_LOCK_STMT aslck;
 AS_CALL_EXPR nthr;
 AS_CALL_EXPR nthrob;
 AS_BOOL_LIT_EXPR tru;
 TRANS fast_par_message;
 AS_PAR_STMT fast_par_message1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS message_self;
 STR message_s;
 TRANS transform_simple;
 AS_PAR_STMT transform_simple1;
 dAM_STMT ret_val1;
 AM_PAR_STMT r;
 AM_PAR_STMT old_simple_par;
 AM_PAR_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PAR_STMT ret_val2;
 AM_PAR_STMT r1;
 TRANS expensive_par_wa;
 AS_PAR_STMT expensive_par_wa1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS warning_self;
 STR warning_s;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 AM_FORMAL_ARG tp_self;
 dTP ret_val5;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val6;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 TRANS add_helper_to_tb;
 IDENT add_helper_to_tb1 = IDENT_zero;
 ARRAYdTP add_helper_to_tb2;
 TP_CLASS ret_val9;
 TP_CLASS tp;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val10;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val12;
 IDENT next_tmp_self = IDENT_zero;
 STR next_tmp_oldiden;
 STR ret_val13;
 STR ret;
 STR create_self1;
 STR ret_val14;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val15;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val16;
 INT str_self = INT_zero;
 STR ret_val17;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val18;
 FSTR str_self1;
 STR ret_val19;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val20;
 IDENT next_tmp_self1 = IDENT_zero;
 STR next_tmp_oldiden1;
 STR ret_val21;
 STR ret11;
 STR create_self2;
 STR ret_val22;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val23;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val24;
 INT str_self2 = INT_zero;
 STR ret_val25;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val26;
 FSTR str_self3;
 STR ret_val27;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val28;
 IDENT next_tmp_self2 = IDENT_zero;
 STR next_tmp_oldiden2;
 STR ret_val29;
 STR ret2;
 STR create_self3;
 STR ret_val30;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val32;
 INT str_self4 = INT_zero;
 STR ret_val33;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val34;
 FSTR str_self5;
 STR ret_val35;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val36;
 IDENT next_tmp_self3 = IDENT_zero;
 STR next_tmp_oldiden3;
 STR ret_val37;
 STR ret3;
 STR create_self4;
 STR ret_val38;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val39;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val40;
 INT str_self6 = INT_zero;
 STR ret_val41;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val42;
 FSTR str_self7;
 STR ret_val43;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val44;
 AS_ASSIGN_STMT create_self5;
 AS_ASSIGN_STMT ret_val45;
 AS_TYPE_SPEC create_self6;
 AS_TYPE_SPEC ret_val46;
 AS_CREATE_EXPR create_self7;
 AS_CREATE_EXPR ret_val47;
 AS_ASSIGN_STMT create_self8;
 AS_ASSIGN_STMT ret_val48;
 AS_CALL_EXPR create_self9;
 AS_CALL_EXPR ret_val49;
 AS_INT2031239268 create_self10;
 AS_INT2031239268 ret_val50;
 AS_LOCK_STMT create_self11;
 AS_LOCK_STMT ret_val51;
 AS_CALL_EXPR create_self12;
 AS_CALL_EXPR ret_val52;
 AS_CALL_EXPR create_self13;
 AS_CALL_EXPR ret_val53;
 AS_BOOL_LIT_EXPR create_self14;
 AS_BOOL_LIT_EXPR ret_val54;
 AS_LOCK_IF_WHEN create_self15;
 AS_LOCK_IF_WHEN ret_val55;
 extern STR Fastpa760727994;
 AM_PAR_STMT L2;
 OB L3;
 extern STR Expens429369331;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 dPARSE L12;
 extern STR Compil151053839;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 AM_ROUT_DEF L15;
 AM_FORMAL_ARG L161_;
 extern STR Compil151053839;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 extern STR Compil163952783;
 BOOL L19;
 BOOL L20;
 extern STR name21;
 BOOL L21;
 BOOL L221_;
 extern STR par;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L23;
 BOOL L241_;
 INT L25;
 INT L261_;
 extern STR par_cl;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L27;
 BOOL L281_;
 INT L29;
 INT L301_;
 extern STR par_ob1;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L31;
 BOOL L321_;
 INT L33;
 INT L341_;
 extern STR cohort;
 extern STR name1;
 extern STR S_pS_;
 extern STR S_;
 BOOL L35;
 BOOL L361_;
 INT L37;
 INT L381_;
 AS_ASSIGN_STMT L39;
 OB L40;
 AS_TYPE_SPEC L41;
 OB L42;
 extern STR PAR_ATTACH;
 AS_CREATE_EXPR L43;
 OB L44;
 dAS_STMT L45;
 dAS_STMT L46;
 AM_LOCAL_EXPR L47;
 BOOL L48;
 STR L49;
 extern STR S_;
 BOOL L50;
 BOOL L521_;
 BOOL L53;
 BOOL L54;
 BOOL L551_;
 AS_ASSIGN_STMT L56;
 OB L57;
 AS_CALL_EXPR L58;
 OB L59;
 dAS_STMT L60;
 dAS_STMT L61;
 AS_INT2031239268 L62;
 OB L63;
 dAS_STMT L64;
 AS_LOCK_STMT L65;
 OB L66;
 AS_CALL_EXPR L67;
 OB L68;
 AS_CALL_EXPR L69;
 OB L70;
 extern STR no_threads;
 AS_BOOL_LIT_EXPR L72;
 OB L73;
 AS_LOCK_IF_WHEN L74;
 OB L75;
 dAS_STMT L76;
 dAS_STMT L77;
 dAS_STMT L78;
 in_simple_par_ol = ATTR(self,in_simple_par);
 if (ATTR(ATTR(self,prog),fast_fork)) {
  SATTR(self,in_simple_par,TRANS_883636795(self, s));
  if (ATTR(self,in_simple_par)) {
   fast_par_message = self;
   fast_par_message1 = s;
   err_loc_self = fast_par_message;
   err_loc_t = ((dPROG_ERR) fast_par_message1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   message_self = fast_par_message;
   message_s = ((STR) &Fastpa760727994);
   PROG_message_STR(ATTR(message_self,prog), message_s);
   transform_simple = self;
   transform_simple1 = s;
   create_self = ((AM_PAR_STMT) NULL);
   create_source = ATTR(transform_simple1,source1);
   L3=ZALLOC(sizeof(struct AM_PAR_STMT_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=AM_PAR_STMT_tag;
   L2 = ((AM_PAR_STMT) L3);
   r1 = L2;
   SATTR(r1,source1,create_source);
   ret_val2 = r1;
   r = ret_val2;
   old_simple_par = ATTR(transform_simple,cur_simple_par);
   SATTR(transform_simple,cur_simple_par,r);
   SATTR(r,body,TRANS_1563748848(transform_simple, ATTR(transform_simple1,body)));
   SATTR(transform_simple,cur_simple_par,old_simple_par);
   ret_val1 = ((dAM_STMT) r);
   res = ret_val1;
   SATTR(self,in_simple_par,in_simple_par_ol);
   ret_val = res;
   return ret_val;
  }
  else {
   expensive_par_wa = self;
   expensive_par_wa1 = s;
   err_loc_self1 = expensive_par_wa;
   err_loc_t1 = ((dPROG_ERR) expensive_par_wa1);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   warning_self = expensive_par_wa;
   warning_s = ((STR) &Expens429369331);
   PROG_warning_STR(ATTR(warning_self,prog), warning_s);
  }
 }
 L4 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L51_=!(L4); 
 if (L51_) {
  SATTR(ATTR(self,cur_se),may_block,TRUE);
 }
 L6 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L71_=!(L6); 
 if (L71_) {
  SATTR(ATTR(self,cur_se),forks,TRUE);
 }
 orig_cl_tp = ATTR(ATTR(ATTR(self,cur_rout),srcsig),tp);
 if (orig_cl_tp==NULL) {
  goto other437;
 } else
 switch (TAG(orig_cl_tp)) {
  case TP_CLASS_tag:
   orig_class_tp = ((TP_CLASS) orig_cl_tp);
   orig_cl_params = ATTR(orig_class_tp,params);
   num1 = 0;
   L8 = (orig_cl_params==((ARRAYdTP) NULL));
   L91_=!(L8); 
   if (L91_) {
    L101_=(orig_cl_params)==NULL?0:ASIZE((ARRAYdTP)orig_cl_params); 
    num1 = L101_;
   }
   L12 = ATTR(ATTR(self,prog),parse);
   orig_class_as = (*dPARSE1581792481[TAG(L12)])(L12, ATTR(orig_class_tp,name1), num1); break;
  default: ;
  other437: ;
   err_self = self;
   err_s = ((STR) &Compil151053839);
   if ((err_self==((TRANS) NULL))) {
    L14 = TRUE;
   } else {
    L14 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L14) {
    L13 = TRUE;
   } else {
    L13 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L13) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val3;
    plus_sarg1 = err_s;
    ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val4;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
 }
 L15 = ATTR(self,cur_rout);
 L161_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L15,0); 
 tp_self = L161_;
 tp_self1 = ATTR(tp_self,expr);
 ret_val6 = ATTR(tp_self1,tp_at);
 ret_val5 = ret_val6;
 current_tp = ret_val5;
 if (current_tp==NULL) {
  goto other438;
 } else
 switch (TAG(current_tp)) {
  case TP_CLASS_tag:
   current_class_tp = ((TP_CLASS) current_tp); break;
  default: ;
  other438: ;
   err_self1 = self;
   err_s1 = ((STR) &Compil151053839);
   if ((err_self1==((TRANS) NULL))) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L17) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val7;
    plus_sarg3 = err_s1;
    ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val8;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
 }
 if (ATTR(s,transformed)) {
  add_helper_to_tb = self;
  add_helper_to_tb1 = ATTR(s,helper_class);
  add_helper_to_tb2 = orig_cl_params;
  tp_class_for_sel = ATTR(ATTR(add_helper_to_tb,prog),tp_tbl);
  tp_class_for_nam = add_helper_to_tb1;
  tp_class_for_par = add_helper_to_tb2;
  ret_val10 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
  tp = ret_val10;
  if ((tp==((TP_CLASS) NULL))) {
   err_self2 = add_helper_to_tb;
   err_s2 = ((STR) &Compil163952783);
   if ((err_self2==((TRANS) NULL))) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L19) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val11 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val11;
    plus_sarg5 = err_s2;
    ret_val12 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val12;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
  }
  L21 = FSETdT1764379880(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp));
  L221_=!(L21); 
  if (L221_) {
   SATTR(ATTR(add_helper_to_tb,prog),tp_done,FSETdT1016814448(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp)));
  }
  ret_val9 = tp;
  dummy1 = ret_val9;
  TRANS_1069298803(self, current_class_tp, ATTR(s,rout));
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 next_tmp_self = IDENT_zero;
 next_tmp_oldiden = ((STR) &par);
 create_self1 = ((STR) NULL);
 ret_val14 = ((STR) &name1);
 ret = ret_val14;
 plus_self6 = ((STR) &S_pS_);
 plus_sarg6 = next_tmp_oldiden;
 ret_val15 = STR_ap2004550586(plus_self6, plus_sarg6);
 plus_self7 = ret_val15;
 plus_sarg7 = ((STR) &S_);
 ret_val16 = STR_ap2004550586(plus_self7, plus_sarg7);
 plus_self8 = ret_val16;
 str_self = IDENT_tmp_count;
 clear_self = buf1;
 L23 = (clear_self==((FSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val18 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val18;
 str_self1 = buf1;
 ret_val19 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val17 = ret_val19;
 plus_sarg8 = ret_val17;
 ret_val20 = STR_ap2004550586(plus_self8, plus_sarg8);
 ret = ret_val20;
 L25 = IDENT_tmp_count;
 L261_=INTPLUS(L25,1); 
 IDENT_tmp_count = L261_;
 ret_val13 = ret;
 newidpar = IDENT_1150413730(IDENT_zero, ret_val13);
 next_tmp_self1 = IDENT_zero;
 next_tmp_oldiden1 = ((STR) &par_cl);
 create_self2 = ((STR) NULL);
 ret_val22 = ((STR) &name1);
 ret11 = ret_val22;
 plus_self9 = ((STR) &S_pS_);
 plus_sarg9 = next_tmp_oldiden1;
 ret_val23 = STR_ap2004550586(plus_self9, plus_sarg9);
 plus_self10 = ret_val23;
 plus_sarg10 = ((STR) &S_);
 ret_val24 = STR_ap2004550586(plus_self10, plus_sarg10);
 plus_self11 = ret_val24;
 str_self2 = IDENT_tmp_count;
 clear_self1 = buf2;
 L27 = (clear_self1==((FSTR) NULL));
 L281_=!(L27); 
 if (L281_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val26 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val26;
 str_self3 = buf2;
 ret_val27 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val25 = ret_val27;
 plus_sarg11 = ret_val25;
 ret_val28 = STR_ap2004550586(plus_self11, plus_sarg11);
 ret11 = ret_val28;
 L29 = IDENT_tmp_count;
 L301_=INTPLUS(L29,1); 
 IDENT_tmp_count = L301_;
 ret_val21 = ret11;
 newidpar_cl = IDENT_1150413730(IDENT_zero, ret_val21);
 next_tmp_self2 = IDENT_zero;
 next_tmp_oldiden2 = ((STR) &par_ob1);
 create_self3 = ((STR) NULL);
 ret_val30 = ((STR) &name1);
 ret2 = ret_val30;
 plus_self12 = ((STR) &S_pS_);
 plus_sarg12 = next_tmp_oldiden2;
 ret_val31 = STR_ap2004550586(plus_self12, plus_sarg12);
 plus_self13 = ret_val31;
 plus_sarg13 = ((STR) &S_);
 ret_val32 = STR_ap2004550586(plus_self13, plus_sarg13);
 plus_self14 = ret_val32;
 str_self4 = IDENT_tmp_count;
 clear_self2 = buf3;
 L31 = (clear_self2==((FSTR) NULL));
 L321_=!(L31); 
 if (L321_) {
  SATTR(clear_self2,loc,0);
 }
 str_in_self2 = str_self4;
 str_in_s2 = buf3;
 ret_val34 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
 buf3 = ret_val34;
 str_self5 = buf3;
 ret_val35 = STR_fr1097270334(((STR) NULL), str_self5);
 ret_val33 = ret_val35;
 plus_sarg14 = ret_val33;
 ret_val36 = STR_ap2004550586(plus_self14, plus_sarg14);
 ret2 = ret_val36;
 L33 = IDENT_tmp_count;
 L341_=INTPLUS(L33,1); 
 IDENT_tmp_count = L341_;
 ret_val29 = ret2;
 newidpar_ob = IDENT_1150413730(IDENT_zero, ret_val29);
 next_tmp_self3 = IDENT_zero;
 next_tmp_oldiden3 = ((STR) &cohort);
 create_self4 = ((STR) NULL);
 ret_val38 = ((STR) &name1);
 ret3 = ret_val38;
 plus_self15 = ((STR) &S_pS_);
 plus_sarg15 = next_tmp_oldiden3;
 ret_val39 = STR_ap2004550586(plus_self15, plus_sarg15);
 plus_self16 = ret_val39;
 plus_sarg16 = ((STR) &S_);
 ret_val40 = STR_ap2004550586(plus_self16, plus_sarg16);
 plus_self17 = ret_val40;
 str_self6 = IDENT_tmp_count;
 clear_self3 = buf4;
 L35 = (clear_self3==((FSTR) NULL));
 L361_=!(L35); 
 if (L361_) {
  SATTR(clear_self3,loc,0);
 }
 str_in_self3 = str_self6;
 str_in_s3 = buf4;
 ret_val42 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
 buf4 = ret_val42;
 str_self7 = buf4;
 ret_val43 = STR_fr1097270334(((STR) NULL), str_self7);
 ret_val41 = ret_val43;
 plus_sarg17 = ret_val41;
 ret_val44 = STR_ap2004550586(plus_self17, plus_sarg17);
 ret3 = ret_val44;
 L37 = IDENT_tmp_count;
 L381_=INTPLUS(L37,1); 
 IDENT_tmp_count = L381_;
 ret_val37 = ret3;
 newidcohort = IDENT_1150413730(IDENT_zero, ret_val37);
 SATTR(s,helper_class,newidpar_cl);
 helper1 = TRANS_1496987093(self, ((dAS_STMT) s), newidpar_ob, newidpar_cl, orig_class_as, orig_cl_params);
 create_self5 = ((AS_ASSIGN_STMT) NULL);
 L40=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=AS_ASSIGN_STMT_tag;
 L39 = ((AS_ASSIGN_STMT) L40);
 ret_val45 = L39;
 asc = ret_val45;
 SATTR(asc,source1,ATTR(s,source1));
 SATTR(asc,name1,newidcohort);
 SATTR(asc,surr_stmt_list,ATTR(s,surr_stmt_list));
 create_self6 = ((AS_TYPE_SPEC) NULL);
 L42=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L42==NULL) FATAL("Unable to allocate more memory");
 ((OB)L42)->header.tag=AS_TYPE_SPEC_tag;
 L41 = ((AS_TYPE_SPEC) L42);
 ret_val46 = L41;
 tpc = ret_val46;
 SATTR(tpc,source1,ATTR(s,source1));
 SATTR(tpc,name1,IDENT_1150413730(IDENT_zero, ((STR) &PAR_ATTACH)));
 create_self7 = ((AS_CREATE_EXPR) NULL);
 L44=ZALLOC(sizeof(struct AS_CREATE_EXPR_struct));
 if (L44==NULL) FATAL("Unable to allocate more memory");
 ((OB)L44)->header.tag=AS_CREATE_EXPR_tag;
 L43 = ((AS_CREATE_EXPR) L44);
 ret_val47 = L43;
 rhsc = ret_val47;
 SATTR(rhsc,source1,ATTR(s,source1));
 SATTR(rhsc,tp,tpc);
 SATTR(asc,rhs,((dAS_EXPR) rhsc));
 ret_as = ((dAS_STMT) asc);
 L45 = ret_as;
 (*dAS_ST1454156204[TAG(L45)])(L45, ((dAS_STMT) TRANS_1390141743(self, newidpar_ob, newidpar_cl, orig_class_as, ATTR(s,surr_stmt_list), ATTR(s,source1))));
 L46 = ret_as;
 (*dAS_ST1454156204[TAG(L46)])(L46, ((dAS_STMT) TRANS_2064676398(self, newidpar_ob, ATTR(s,surr_stmt_list), ATTR(s,source1))));
 {
  struct TRANS_1349638895_frame_struct other1_0;
TRANS_1349638895_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L47 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   loc = L47;
   L49 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L50 = STR_is111530248(L49, ((STR) &S_));
   L521_=!(L50); 
   if (L521_) {
    if ((ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL))) {
     L53 = TRUE;
    } else {
     L54 = TRANS_157729662(self, ATTR(loc,name1), ATTR(self,cur_param_ob));
     L551_=!(L54); 
     L53 = L551_;
    }
    L48 = L53;
   } else {
    L48 = FALSE;
   }
   if (L48) {
    create_self8 = ((AS_ASSIGN_STMT) NULL);
    L57=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=AS_ASSIGN_STMT_tag;
    L56 = ((AS_ASSIGN_STMT) L57);
    ret_val48 = L56;
    exp1 = ret_val48;
    SATTR(exp1,source1,ATTR(s,source1));
    SATTR(exp1,surr_stmt_list,ATTR(s,surr_stmt_list));
    SATTR(exp1,lhs_expr,((dAS_EXPR) TRANS_1849744987(self, ATTR(loc,name1), helper1, ATTR(s,source1))));
    create_self9 = ((AS_CALL_EXPR) NULL);
    L59=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L59==NULL) FATAL("Unable to allocate more memory");
    ((OB)L59)->header.tag=AS_CALL_EXPR_tag;
    L58 = ((AS_CALL_EXPR) L59);
    ret_val49 = L58;
    rhs = ret_val49;
    SATTR(rhs,source1,ATTR(s,source1));
    SATTR(rhs,name1,ATTR(loc,name1));
    SATTR(exp1,rhs,((dAS_EXPR) rhs));
    L60 = ret_as;
    (*dAS_ST1454156204[TAG(L60)])(L60, ((dAS_STMT) exp1));
   }
  }
 }
 after_loop: ;
 L61 = ret_as;
 (*dAS_ST1454156204[TAG(L61)])(L61, TRANS_166911016(self, TRANS_194341863, ATTR(s,surr_stmt_list), ATTR(s,source1)));
 SATTR(s,rout,TRANS_725662504(self, newidpar, newidpar_ob, newidpar_cl, helper1, orig_class_as, current_class_tp, newidcohort, ((AS_TYPE_SPEC) NULL), ATTR(s,body), TRANS_par_code, ATTR(s,source1)));
 SATTR(s,body,((AS_STMT_LIST) NULL));
 create_self10 = ((AS_INT2031239268) NULL);
 L63=ZALLOC(sizeof(struct AS_INT2031239268_struct));
 if (L63==NULL) FATAL("Unable to allocate more memory");
 ((OB)L63)->header.tag=AS_INT2031239268_tag;
 L62 = ((AS_INT2031239268) L63);
 ret_val50 = L62;
 att8 = ret_val50;
 SATTR(att8,source1,ATTR(s,source1));
 SATTR(att8,surr_stmt_list,ATTR(s,surr_stmt_list));
 SATTR(att8,routname,newidpar);
 SATTR(att8,helpername,newidpar_ob);
 SATTR(att8,gatename,newidcohort);
 SATTR(att8,at,((dAS_EXPR) NULL));
 L64 = ret_as;
 (*dAS_ST1454156204[TAG(L64)])(L64, ((dAS_STMT) att8));
 create_self11 = ((AS_LOCK_STMT) NULL);
 L66=ZALLOC(sizeof(struct AS_LOCK_STMT_struct));
 if (L66==NULL) FATAL("Unable to allocate more memory");
 ((OB)L66)->header.tag=AS_LOCK_STMT_tag;
 L65 = ((AS_LOCK_STMT) L66);
 ret_val51 = L65;
 aslck = ret_val51;
 SATTR(aslck,source1,ATTR(s,source1));
 SATTR(aslck,surr_stmt_list,ATTR(s,surr_stmt_list));
 create_self12 = ((AS_CALL_EXPR) NULL);
 L68=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L68==NULL) FATAL("Unable to allocate more memory");
 ((OB)L68)->header.tag=AS_CALL_EXPR_tag;
 L67 = ((AS_CALL_EXPR) L68);
 ret_val52 = L67;
 nthr = ret_val52;
 SATTR(nthr,source1,ATTR(s,source1));
 create_self13 = ((AS_CALL_EXPR) NULL);
 L70=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L70==NULL) FATAL("Unable to allocate more memory");
 ((OB)L70)->header.tag=AS_CALL_EXPR_tag;
 L69 = ((AS_CALL_EXPR) L70);
 ret_val53 = L69;
 nthrob = ret_val53;
 SATTR(nthrob,source1,ATTR(s,source1));
 SATTR(nthrob,name1,newidcohort);
 SATTR(nthr,ob,((dAS_EXPR) nthrob));
 SATTR(nthr,name1,IDENT_1150413730(IDENT_zero, ((STR) &no_threads)));
 create_self14 = ((AS_BOOL_LIT_EXPR) NULL);
 L73=ZALLOC(sizeof(struct AS_BOOL_LIT_EXPR_struct));
 if (L73==NULL) FATAL("Unable to allocate more memory");
 ((OB)L73)->header.tag=AS_BOOL_LIT_EXPR_tag;
 L72 = ((AS_BOOL_LIT_EXPR) L73);
 ret_val54 = L72;
 tru = ret_val54;
 SATTR(tru,val1,TRUE);
 create_self15 = ((AS_LOCK_IF_WHEN) NULL);
 L75=ZALLOC(sizeof(struct AS_LOCK_IF_WHEN_struct));
 if (L75==NULL) FATAL("Unable to allocate more memory");
 ((OB)L75)->header.tag=AS_LOCK_IF_WHEN_tag;
 L74 = ((AS_LOCK_IF_WHEN) L75);
 ret_val55 = L74;
 SATTR(aslck,if_when_part,ret_val55);
 SATTR(ATTR(aslck,if_when_part),val1,((dAS_EXPR) tru));
 SATTR(ATTR(aslck,if_when_part),e_list,((dAS_EXPR) nthr));
 SATTR(aslck,else_part,((AS_STMT_LIST) NULL));
 SATTR(aslck,no_else,TRUE);
 SATTR(aslck,transformed,TRUE);
 L76 = ret_as;
 (*dAS_ST1454156204[TAG(L76)])(L76, ((dAS_STMT) aslck));
 L77 = ret_as;
 (*dAS_ST1454156204[TAG(L77)])(L77, TRANS_166911016(self, TRANS_1924362320, ATTR(s,surr_stmt_list), ATTR(s,source1)));
 L78 = ret_as;
 (*dAS_ST1454156204[TAG(L78)])(L78, ATTR(s,next));
 SATTR(s,next,ret_as);
 ret_val = ((dAM_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT TRANS_954470687(TRANS self, dAS_STMT s) {
 dAS_STMT ret_val;
 AS_STMT_LIST body11;
 dAS_STMT curr;
 dAS_STMT prev = ((dAS_STMT) NULL);
 dAS_STMT L1;
 dAS_STMT L2;
 L1 = s;
 body11 = (*dAS_ST445865822[TAG(L1)])(L1);
 curr = ATTR(body11,stmts);
 while (1) {
  if ((curr==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  if (SYSOBEQ(((OB) curr),((OB) s))) {
   ret_val = prev;
   return ret_val;
  }
  prev = curr;
  L2 = curr;
  curr = (*dAS_ST460279343[TAG(L2)])(L2);
 }
 after_loop: ;
 ret_val = ((dAS_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_1456624412(TRANS self, dAS_STMT t) {
 dAS_STMT import11 = ((dAS_STMT) NULL);
 INT L1;
 dAS_STMT L2;
 AS_STMT_LIST L3;
 dAS_STMT L4;
 BOOL L5;
 BOOL L61_;
 dAS_STMT L7;
 dAS_STMT L8;
 dAS_STMT L9;
 if (t==NULL) {
  goto other451;
 } else
 switch (TAG(t)) {
  case AS_RETURN_STMT_tag:
   return; break;
  case AS_RAISE_STMT_tag:
   return; break;
  case AS_QUIT_STMT_tag:
   return; break;
  default: ;
  other451: ;
 }
 if (TRANS_1975351488) {
  L1 = TRANS_1924362320;
  L2 = t;
  L3 = (*dAS_ST445865822[TAG(L2)])(L2);
  L4 = t;
  import11 = TRANS_120708644(self, L1, L3, (*dAS_ST957383256[TAG(L4)])(L4));
 }
 L5 = (import11==((dAS_STMT) NULL));
 L61_=!(L5); 
 if (L61_) {
  L7 = t;
  L8 = import11;
  (*dAS_ST1454156204[TAG(L8)])(L8, (*dAS_ST460279343[TAG(L7)])(L7));
  L9 = t;
  (*dAS_ST1687647066[TAG(L9)])(L9, import11);
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_866431519(TRANS self, AS_STMT_LIST l, BOOL on_exit_only) {
 dAS_STMT s;
 dAS_STMT export11;
 dAS_STMT p;
 dAS_EXPR e;
 AS_CASE_WHEN wp;
 AS_TYPECASE_WHEN wp1;
 AS_LOCK_IF_WHEN iwp;
 dAS_EXPR lck1;
 dAS_STMT export21;
 TRANS is_in_loop_and_c;
 dAS_EXPR is_in_loop_and_c1;
 BOOL ret_val = BOOL_zero;
 TRANS prev_export_self;
 dAS_STMT prev_export_p;
 dAS_STMT prev_export_s;
 AS_STMT_LIST prev_export_l;
 dAS_STMT export3;
 TRANS is_in_loop_and_c2;
 dAS_EXPR is_in_loop_and_c3;
 BOOL ret_val1 = BOOL_zero;
 TRANS prev_export_self1;
 dAS_STMT prev_export_p1;
 dAS_STMT prev_export_s1;
 AS_STMT_LIST prev_export_l1;
 dAS_STMT export4;
 TRANS prev_export_self2;
 dAS_STMT prev_export_p2;
 dAS_STMT prev_export_s2;
 AS_STMT_LIST prev_export_l2;
 dAS_STMT export5;
 TRANS prev_export_self3;
 dAS_STMT prev_export_p3;
 dAS_STMT prev_export_s3;
 AS_STMT_LIST prev_export_l3;
 dAS_STMT export6;
 TRANS is_in_loop_and_c4;
 dAS_EXPR is_in_loop_and_c5;
 BOOL ret_val2 = BOOL_zero;
 TRANS is_in_loop_and_c6;
 dAS_EXPR is_in_loop_and_c7;
 BOOL ret_val3 = BOOL_zero;
 TRANS prev_export_self4;
 dAS_STMT prev_export_p4;
 dAS_STMT prev_export_s4;
 AS_STMT_LIST prev_export_l4;
 dAS_STMT export7;
 TRANS is_in_loop_and_c8;
 dAS_EXPR is_in_loop_and_c9;
 BOOL ret_val4 = BOOL_zero;
 TRANS prev_export_self5;
 dAS_STMT prev_export_p5;
 dAS_STMT prev_export_s5;
 AS_STMT_LIST prev_export_l5;
 dAS_STMT export8;
 TRANS is_in_loop_and_c10;
 dAS_EXPR is_in_loop_and_c11;
 BOOL ret_val5 = BOOL_zero;
 TRANS prev_export_self6;
 dAS_STMT prev_export_p6;
 dAS_STMT prev_export_s6;
 AS_STMT_LIST prev_export_l6;
 dAS_STMT export9;
 TRANS is_in_loop_and_c12;
 dAS_EXPR is_in_loop_and_c13;
 BOOL ret_val6 = BOOL_zero;
 TRANS prev_export_self7;
 dAS_STMT prev_export_p7;
 dAS_STMT prev_export_s7;
 AS_STMT_LIST prev_export_l7;
 dAS_STMT export10;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 INT L4;
 dAS_STMT L5;
 dAS_STMT L6;
 dAS_STMT L7;
 BOOL L8;
 BOOL L9;
 BOOL L101_;
 INT L11;
 dAS_STMT L12;
 dAS_STMT L13;
 dAS_STMT L14;
 INT L15;
 dAS_STMT L16;
 dAS_STMT L17;
 dAS_STMT L18;
 BOOL L19;
 BOOL L20;
 BOOL L211_;
 INT L22;
 dAS_STMT L23;
 dAS_STMT L24;
 dAS_STMT L25;
 BOOL L26;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L32;
 BOOL L331_;
 INT L34;
 dAS_STMT L35;
 dAS_STMT L36;
 dAS_STMT L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 INT L41;
 dAS_STMT L42;
 dAS_STMT L43;
 dAS_STMT L44;
 BOOL L45;
 BOOL L46;
 BOOL L471_;
 INT L48;
 dAS_STMT L49;
 dAS_STMT L50;
 dAS_STMT L51;
 BOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 BOOL L56;
 BOOL L571_;
 BOOL L58;
 BOOL L591_;
 BOOL L60;
 BOOL L611_;
 BOOL L62;
 BOOL L63;
 BOOL L641_;
 INT L65;
 dAS_STMT L66;
 dAS_STMT L67;
 dAS_STMT L68;
 dAS_EXPR L69;
 dAS_STMT L70;
 dAS_STMT L71;
 BOOL L721_;
 INT L73;
 dAS_STMT L74;
 dAS_STMT L75;
 if ((l==((AS_STMT_LIST) NULL))) {
  return;
 }
 s = ATTR(l,stmts);
 if ((s==((dAS_STMT) NULL))) {
  export11 = TRANS_166911016(self, TRANS_194341863, l, ATTR(l,source1));
  SATTR(l,stmts,export11);
  return;
 }
 p = ((dAS_STMT) NULL);
 while (1) {
  if (TRANS_1229338887(self)) {
   if (s==NULL) {
    goto other440;
   } else
   switch (TAG(s)) {
    case AS_ASSIGN_STMT_tag:
     is_in_loop_and_c = self;
     is_in_loop_and_c1 = ATTR(((AS_ASSIGN_STMT) s),rhs);
     L2 = (ATTR(is_in_loop_and_c,cur_loop)==((AM_LOOP_STMT) NULL));
     L31_=!(L2); 
     if (L31_) {
      L1 = TRANS_1865855576(is_in_loop_and_c, TRANS_693526804(is_in_loop_and_c, is_in_loop_and_c1, ((dTP) NULL)));
     } else {
      L1 = FALSE;
     }
     ret_val = L1;
     if (ret_val) {
      prev_export_self = self;
      prev_export_p = p;
      prev_export_s = s;
      prev_export_l = l;
      L4 = TRANS_194341863;
      L5 = prev_export_s;
      export3 = TRANS_166911016(prev_export_self, L4, prev_export_l, (*dAS_ST957383256[TAG(L5)])(L5));
      L6 = export3;
      (*dAS_ST1687647066[TAG(L6)])(L6, prev_export_s);
      if ((prev_export_p==((dAS_STMT) NULL))) {
       SATTR(prev_export_l,stmts,export3);
      }
      else {
       L7 = prev_export_p;
       (*dAS_ST1687647066[TAG(L7)])(L7, export3);
      }
     } break;
    case AS_ASSERT_STMT_tag:
     is_in_loop_and_c2 = self;
     is_in_loop_and_c3 = ATTR(((AS_ASSERT_STMT) s),test1);
     L9 = (ATTR(is_in_loop_and_c2,cur_loop)==((AM_LOOP_STMT) NULL));
     L101_=!(L9); 
     if (L101_) {
      L8 = TRANS_1865855576(is_in_loop_and_c2, TRANS_693526804(is_in_loop_and_c2, is_in_loop_and_c3, ((dTP) NULL)));
     } else {
      L8 = FALSE;
     }
     ret_val1 = L8;
     if (ret_val1) {
      prev_export_self1 = self;
      prev_export_p1 = p;
      prev_export_s1 = s;
      prev_export_l1 = l;
      L11 = TRANS_194341863;
      L12 = prev_export_s1;
      export4 = TRANS_166911016(prev_export_self1, L11, prev_export_l1, (*dAS_ST957383256[TAG(L12)])(L12));
      L13 = export4;
      (*dAS_ST1687647066[TAG(L13)])(L13, prev_export_s1);
      if ((prev_export_p1==((dAS_STMT) NULL))) {
       SATTR(prev_export_l1,stmts,export4);
      }
      else {
       L14 = prev_export_p1;
       (*dAS_ST1687647066[TAG(L14)])(L14, export4);
      }
     } break;
    case AS_EXPR_STMT_tag:
     e = ATTR(((AS_EXPR_STMT) s),e);
     if (e==NULL) {
      goto other441;
     } else
     switch (TAG(e)) {
      case AS_BREAK_EXPR_tag:
       prev_export_self2 = self;
       prev_export_p2 = p;
       prev_export_s2 = s;
       prev_export_l2 = l;
       L15 = TRANS_194341863;
       L16 = prev_export_s2;
       export5 = TRANS_166911016(prev_export_self2, L15, prev_export_l2, (*dAS_ST957383256[TAG(L16)])(L16));
       L17 = export5;
       (*dAS_ST1687647066[TAG(L17)])(L17, prev_export_s2);
       if ((prev_export_p2==((dAS_STMT) NULL))) {
        SATTR(prev_export_l2,stmts,export5);
       }
       else {
        L18 = prev_export_p2;
        (*dAS_ST1687647066[TAG(L18)])(L18, export5);
       } break;
      case AS_CALL_EXPR_tag:
       L20 = (ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL));
       L211_=!(L20); 
       if (L211_) {
        L19 = TRANS_1865855576(self, TRANS_1705639890(self, ((AS_CALL_EXPR) e), ((dTP) NULL), FALSE));
       } else {
        L19 = FALSE;
       }
       if (L19) {
        prev_export_self3 = self;
        prev_export_p3 = p;
        prev_export_s3 = s;
        prev_export_l3 = l;
        L22 = TRANS_194341863;
        L23 = prev_export_s3;
        export6 = TRANS_166911016(prev_export_self3, L22, prev_export_l3, (*dAS_ST957383256[TAG(L23)])(L23));
        L24 = export6;
        (*dAS_ST1687647066[TAG(L24)])(L24, prev_export_s3);
        if ((prev_export_p3==((dAS_STMT) NULL))) {
         SATTR(prev_export_l3,stmts,export6);
        }
        else {
         L25 = prev_export_p3;
         (*dAS_ST1687647066[TAG(L25)])(L25, export6);
        }
       } break;
      case AS_AT_EXPR_tag:
       is_in_loop_and_c4 = self;
       is_in_loop_and_c5 = ATTR(((AS_AT_EXPR) e),e);
       L28 = (ATTR(is_in_loop_and_c4,cur_loop)==((AM_LOOP_STMT) NULL));
       L291_=!(L28); 
       if (L291_) {
        L27 = TRANS_1865855576(is_in_loop_and_c4, TRANS_693526804(is_in_loop_and_c4, is_in_loop_and_c5, ((dTP) NULL)));
       } else {
        L27 = FALSE;
       }
       ret_val2 = L27;
       if (ret_val2) {
        L26 = TRUE;
       } else {
        is_in_loop_and_c6 = self;
        is_in_loop_and_c7 = ATTR(((AS_AT_EXPR) e),at);
        L32 = (ATTR(is_in_loop_and_c6,cur_loop)==((AM_LOOP_STMT) NULL));
        L331_=!(L32); 
        if (L331_) {
         L30 = TRANS_1865855576(is_in_loop_and_c6, TRANS_693526804(is_in_loop_and_c6, is_in_loop_and_c7, ((dTP) NULL)));
        } else {
         L30 = FALSE;
        }
        ret_val3 = L30;
        L26 = ret_val3;
       }
       if (L26) {
        prev_export_self4 = self;
        prev_export_p4 = p;
        prev_export_s4 = s;
        prev_export_l4 = l;
        L34 = TRANS_194341863;
        L35 = prev_export_s4;
        export7 = TRANS_166911016(prev_export_self4, L34, prev_export_l4, (*dAS_ST957383256[TAG(L35)])(L35));
        L36 = export7;
        (*dAS_ST1687647066[TAG(L36)])(L36, prev_export_s4);
        if ((prev_export_p4==((dAS_STMT) NULL))) {
         SATTR(prev_export_l4,stmts,export7);
        }
        else {
         L37 = prev_export_p4;
         (*dAS_ST1687647066[TAG(L37)])(L37, export7);
        }
       } break;
      default: ;
      other441: ;
     } break;
    case AS_IF_STMT_tag:
     is_in_loop_and_c8 = self;
     is_in_loop_and_c9 = ATTR(((AS_IF_STMT) s),test1);
     L39 = (ATTR(is_in_loop_and_c8,cur_loop)==((AM_LOOP_STMT) NULL));
     L401_=!(L39); 
     if (L401_) {
      L38 = TRANS_1865855576(is_in_loop_and_c8, TRANS_693526804(is_in_loop_and_c8, is_in_loop_and_c9, ((dTP) NULL)));
     } else {
      L38 = FALSE;
     }
     ret_val4 = L38;
     if (ret_val4) {
      prev_export_self5 = self;
      prev_export_p5 = p;
      prev_export_s5 = s;
      prev_export_l5 = l;
      L41 = TRANS_194341863;
      L42 = prev_export_s5;
      export8 = TRANS_166911016(prev_export_self5, L41, prev_export_l5, (*dAS_ST957383256[TAG(L42)])(L42));
      L43 = export8;
      (*dAS_ST1687647066[TAG(L43)])(L43, prev_export_s5);
      if ((prev_export_p5==((dAS_STMT) NULL))) {
       SATTR(prev_export_l5,stmts,export8);
      }
      else {
       L44 = prev_export_p5;
       (*dAS_ST1687647066[TAG(L44)])(L44, export8);
      }
     }
     TRANS_866431519(self, ATTR(((AS_IF_STMT) s),then_part), TRUE);
     TRANS_866431519(self, ATTR(((AS_IF_STMT) s),else_part), TRUE); break;
    case AS_LOOP_STMT_tag:
     TRANS_866431519(self, ATTR(((AS_LOOP_STMT) s),body), TRUE); break;
    case AS_CASE_STMT_tag:
     is_in_loop_and_c10 = self;
     is_in_loop_and_c11 = ATTR(((AS_CASE_STMT) s),test1);
     L46 = (ATTR(is_in_loop_and_c10,cur_loop)==((AM_LOOP_STMT) NULL));
     L471_=!(L46); 
     if (L471_) {
      L45 = TRANS_1865855576(is_in_loop_and_c10, TRANS_693526804(is_in_loop_and_c10, is_in_loop_and_c11, ((dTP) NULL)));
     } else {
      L45 = FALSE;
     }
     ret_val5 = L45;
     if (ret_val5) {
      prev_export_self6 = self;
      prev_export_p6 = p;
      prev_export_s6 = s;
      prev_export_l6 = l;
      L48 = TRANS_194341863;
      L49 = prev_export_s6;
      export9 = TRANS_166911016(prev_export_self6, L48, prev_export_l6, (*dAS_ST957383256[TAG(L49)])(L49));
      L50 = export9;
      (*dAS_ST1687647066[TAG(L50)])(L50, prev_export_s6);
      if ((prev_export_p6==((dAS_STMT) NULL))) {
       SATTR(prev_export_l6,stmts,export9);
      }
      else {
       L51 = prev_export_p6;
       (*dAS_ST1687647066[TAG(L51)])(L51, export9);
      }
     }
     L52 = ATTR(((AS_CASE_STMT) s),no_else);
     L531_=!(L52); 
     if (L531_) {
      TRANS_866431519(self, ATTR(((AS_CASE_STMT) s),else_part), TRUE);
     }
     wp = ATTR(((AS_CASE_STMT) s),when_part);
     while (1) {
      L54 = (wp==((AS_CASE_WHEN) NULL));
      L551_=!(L54); 
      if (L551_) {
      }
      else {
       goto after_loop1;
      }
      TRANS_866431519(self, ATTR(wp,then_part), TRUE);
      wp = ATTR(wp,next);
     }
     after_loop1: ; break;
    case AS_TYPECASE_STMT_tag:
     L56 = ATTR(((AS_TYPECASE_STMT) s),no_else);
     L571_=!(L56); 
     if (L571_) {
      TRANS_866431519(self, ATTR(((AS_TYPECASE_STMT) s),else_part), TRUE);
     }
     wp1 = ATTR(((AS_TYPECASE_STMT) s),when_part);
     while (1) {
      L58 = (wp1==((AS_TYPECASE_WHEN) NULL));
      L591_=!(L58); 
      if (L591_) {
      }
      else {
       goto after_loop2;
      }
      TRANS_866431519(self, ATTR(wp1,then_part), TRUE);
      wp1 = ATTR(wp1,next);
     }
     after_loop2: ; break;
    case AS_WIT1063437351_tag:
     TRANS_866431519(self, ATTR(((AS_WIT1063437351) s),near_part), TRUE);
     TRANS_866431519(self, ATTR(((AS_WIT1063437351) s),else_part), TRUE); break;
    case AS_LOCK_STMT_tag:
     iwp = ATTR(((AS_LOCK_STMT) s),if_when_part);
     while (1) {
      if ((iwp==((AS_LOCK_IF_WHEN) NULL))) {
       goto after_loop3;
      }
      lck1 = ATTR(iwp,e_list);
      while (1) {
       L60 = (lck1==((dAS_EXPR) NULL));
       L611_=!(L60); 
       if (L611_) {
       }
       else {
        goto after_loop4;
       }
       is_in_loop_and_c12 = self;
       is_in_loop_and_c13 = lck1;
       L63 = (ATTR(is_in_loop_and_c12,cur_loop)==((AM_LOOP_STMT) NULL));
       L641_=!(L63); 
       if (L641_) {
        L62 = TRANS_1865855576(is_in_loop_and_c12, TRANS_693526804(is_in_loop_and_c12, is_in_loop_and_c13, ((dTP) NULL)));
       } else {
        L62 = FALSE;
       }
       ret_val6 = L62;
       if (ret_val6) {
        prev_export_self7 = self;
        prev_export_p7 = p;
        prev_export_s7 = s;
        prev_export_l7 = l;
        L65 = TRANS_194341863;
        L66 = prev_export_s7;
        export10 = TRANS_166911016(prev_export_self7, L65, prev_export_l7, (*dAS_ST957383256[TAG(L66)])(L66));
        L67 = export10;
        (*dAS_ST1687647066[TAG(L67)])(L67, prev_export_s7);
        if ((prev_export_p7==((dAS_STMT) NULL))) {
         SATTR(prev_export_l7,stmts,export10);
        }
        else {
         L68 = prev_export_p7;
         (*dAS_ST1687647066[TAG(L68)])(L68, export10);
        }
       }
       L69 = lck1;
       lck1 = (*dAS_EX337431651[TAG(L69)])(L69);
      }
      after_loop4: ;
      iwp = ATTR(iwp,next);
     }
     after_loop3: ; break;
    default: ;
    other440: ;
   }
  }
  L70 = s;
  if (((*dAS_ST460279343[TAG(L70)])(L70)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  p = s;
  L71 = s;
  s = (*dAS_ST460279343[TAG(L71)])(L71);
 }
 after_loop: ;
 L721_=!(on_exit_only); 
 if (L721_) {
  if (s==NULL) {
   goto other442;
  } else
  switch (TAG(s)) {
   case AS_RETURN_STMT_tag:
    return; break;
   case AS_YIELD_STMT_tag:
    return; break;
   case AS_QUIT_STMT_tag:
    return; break;
   case AS_RAISE_STMT_tag:
    return; break;
   default: ;
   other442: ;
    L73 = TRANS_194341863;
    L74 = s;
    export21 = TRANS_166911016(self, L73, l, (*dAS_ST957383256[TAG(L74)])(L74));
    L75 = s;
    (*dAS_ST1454156204[TAG(L75)])(L75, export21);
  }
 }
}
