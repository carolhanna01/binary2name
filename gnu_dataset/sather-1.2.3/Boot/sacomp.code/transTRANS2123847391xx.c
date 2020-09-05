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

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_NILELT_struct {/* layout for ELT_NIL{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILELT;

typedef struct GENERATE_AM_struct {/* layout for GENERATE_AM */
 OB_HEADER header;
 struct FLISTSIG_struct *sig_list;
 struct FSETSIG_struct *sig_inprocout;
 struct FSETSIG_struct *sig_recursive;
 struct GLOBAL_TBL_struct *global_tbl;
 struct PROG_struct *prog;
 BOOL only_check;
 BOOL optimize;
 } *GENERATE_AM;

typedef struct GLOBAL_TBL_struct {/* layout for GLOBAL_TBL */
 OB_HEADER header;
 struct FLISTA112326087_struct *top_sort;
 struct FMAPdT953816637_struct *tbl;
 struct PROG_struct *prog;
 } *GLOBAL_TBL;

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

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

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

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

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

typedef struct TUPdTP1710233051_struct {/* layout for TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 struct dTP_struct *t1;
 struct FMAPID739086519_struct *t2;
 } TUPdTP1710233051;
static TUPdTP1710233051 TUPdTP1710233051_zero;

typedef struct TUPdTP1710233051_boxed_struct {
 OB_HEADER header;
 TUPdTP1710233051 immutable_part;
 } *TUPdTP1710233051_boxed;

typedef struct TUPIDE641654124_struct {/* layout for TUP{IDENT,AM_SHARED_EXPR} */
 struct AM_SHARED_EXPR_struct *t2;
 struct IDENT_struct t1;
 } TUPIDE641654124;
static TUPIDE641654124 TUPIDE641654124_zero;

typedef struct TUPIDE641654124_boxed_struct {
 OB_HEADER header;
 TUPIDE641654124 immutable_part;
 } *TUPIDE641654124_boxed;

typedef struct AM_ANY_EXPR_struct {/* layout for AM_ANY_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_ANY_EXPR;

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

typedef struct AM_AT_EXPR_struct {/* layout for AM_AT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *e;
 struct SFILE_ID_struct source1;
 } *AM_AT_EXPR;

typedef struct AM_CLUSTER_EXPR_struct {/* layout for AM_CLUSTER_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLUSTER_EXPR;

typedef struct AM_CLU1286269335_struct {/* layout for AM_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLU1286269335;

typedef struct AM_EXPR_STMT_struct {/* layout for AM_EXPR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *expr;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_EXPR_STMT;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

typedef struct AM_GLOBAL_EXPR_struct {/* layout for AM_GLOBAL_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_GLOBAL_EXPR;

typedef struct AM_HERE_EXPR_struct {/* layout for AM_HERE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_HERE_EXPR;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

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

typedef struct AM_NEAR_EXPR_struct {/* layout for AM_NEAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_NEAR_EXPR;

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

typedef struct AM_VAT319982528_struct {/* layout for AM_VATTR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct dTP_struct *real_tp;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 } *AM_VAT319982528;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct AS_FAR_EXPR_struct {/* layout for AS_FAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_FAR_EXPR;

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

typedef struct AS_INITIAL_EXPR_struct {/* layout for AS_INITIAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_INITIAL_EXPR;

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

typedef struct AS_RESULT_EXPR_struct {/* layout for AS_RESULT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_RESULT_EXPR;

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

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct ELT_TBL_eltbrELT_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 ELT ret_val2;
 ELT_TBL L61;
 ELT r;
 INT L31;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *ELT_TBL_eltbrELT_frame;

typedef struct ELT_TB431570775_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: q */
 ELT ret_val6;
 INT h;
 ELT e;
 ELT e1;
 ELT_TBL query_hash_self;
 IDENT query_hash_i;
 INT ret_val;
 IDENT hash_self;
 INT ret_val1;
 INT x;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val2;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val3;
 ELT_TBL is_elt_nil_self2;
 ELT is_elt_nil_e2;
 BOOL ret_val4;
 ELT_NILELT is_elt_nil_self3;
 ELT is_elt_nil_e3;
 BOOL ret_val5;
 } *ELT_TB431570775_frame;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA112326087_struct {/* layout for FLIST{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FLISTA112326087;

typedef struct FLISTIDENT_struct {/* layout for FLIST{IDENT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *FLISTIDENT;

typedef struct FLISTI1709766432_frame_struct {
 INT state;
 FLISTIDENT self;/* Formal argument: self */
 IDENT ret_val1;
 INT i;
 INT sz;
 FLISTIDENT aget_self;
 INT aget_ind;
 IDENT ret_val;
 } *FLISTI1709766432_frame;

typedef struct FMAPdT953816637_struct {/* layout for FMAP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FMAPdT953816637;

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

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

extern BOOL TRANS_89465360;
extern IDENT IDENT_667688077;
extern IDENT IDENT_985337352;
extern INT AS_ARG1300937332;
extern INT AS_TYP1853679690;
extern INT TP_KIN138039446;
extern INT TP_KIND_val_tp;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUI191368816;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_sys;
extern TP_CLASS TP_BUILTIN_zone;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAS_EX1809609236)(dAS_EXPR, dAS_EXPR);
AM_LOCAL_EXPR TRANS_1711874080(TRANS, IDENT);
AM_LOCAL_EXPR TRANS_2133809761(TRANS);
AM_LOCAL_EXPR TRANS_647716465(TRANS);
AM_ROUT_DEF TRANS_1291044671(TRANS, ELT, AS_SHARED_DEF);
AM_ROUT_DEF TRANS_1584821941(TRANS, ELT, AS_ATTR_DEF);
AM_ROUT_DEF TRANS_1746053264(TRANS, ELT, AS_CONST_DEF);
AM_SHARED_EXPR GLOBAL499677090(GLOBAL_TBL, IDENT, dTP);
AS_CALL_EXPR TRANS_1849744987(TRANS, IDENT, AM_LOCAL_EXPR, SFILE_ID);
BOOL ELT_is1936066670(ELT);
BOOL ELT_is380037919(ELT);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA513015767(TP_CLASS, dCALL_TP);
BOOL TRANS_157729662(TRANS, IDENT, AM_LOCAL_EXPR);
BOOL TRANS_2091440962(TRANS);
ELT ELT_TB431570775(ELT_TB431570775_frame);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
FLISTA112326087 FLISTA327355535(FLISTA112326087, AM_SHARED_EXPR);
FLISTIDENT FLISTI370551816(FLISTIDENT, IDENT);
FLISTIDENT TRANS_1609601492(TRANS, IDENT, AM_LOCAL_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FMAPID739086519 FMAPID1512695779(FMAPID739086519, IDENT, AM_SHARED_EXPR);
FMAPID739086519 FMAPdT183078507(FMAPdT953816637, dTP);
FMAPdT953816637 FMAPdT1221772946(FMAPdT953816637, dTP, FMAPID739086519);
GENERATE_AM TRANS_1867726633(TRANS);
IDENT FLISTI1709766432(FLISTI1709766432_frame);
IDENT IDENT_1150413730(IDENT, STR);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTI1328284422(FLISTIDENT);
INT STR_sizerINT(STR);
INTI1 INTI_c1588968505(INTI1, INT);
IN_MODE IN_MOD403789248(IN_MODE);
SIG IMPL_i1943552357(IMPL);
STR STR_ap2004550586(STR, STR);
STR STR_head_INTrSTR(STR, INT);
dAM_EXPR TRANS_1006089529(TRANS, AS_AND_EXPR, dTP);
dAM_EXPR TRANS_1164784471(TRANS, AS_ANY_EXPR, dTP);
dAM_EXPR TRANS_1168851617(TRANS, AS_IS_VOID_EXPR, dTP);
dAM_EXPR TRANS_1173176875(TRANS, AS_INT_LIT_EXPR, dTP);
dAM_EXPR TRANS_1230152132(TRANS, AS_GLOBAL_EXPR, dTP);
dAM_EXPR TRANS_1269501442(TRANS, AS_CHAR_LIT_EXPR, dTP);
dAM_EXPR TRANS_1346628084(TRANS, AS_RESULT_EXPR, dTP);
dAM_EXPR TRANS_1475346849(TRANS, AS_FLT_LIT_EXPR, dTP);
dAM_EXPR TRANS_1502802970(TRANS, AS_HERE_EXPR, dTP);
dAM_EXPR TRANS_1546520066(TRANS, AS_BOU14269336, dTP);
dAM_EXPR TRANS_1556853519(TRANS, AS_STR_LIT_EXPR, dTP);
dAM_EXPR TRANS_1627619307(TRANS, AS_INITIAL_EXPR, dTP);
dAM_EXPR TRANS_1668850900(TRANS, AS_EXCEPT_EXPR, dTP);
dAM_EXPR TRANS_1703484959(TRANS, AS_WHERE_EXPR, dTP);
dAM_EXPR TRANS_1705639890(TRANS, AS_CALL_EXPR, dTP, BOOL);
dAM_EXPR TRANS_1761370920(TRANS, AS_AT_EXPR, dTP);
dAM_EXPR TRANS_1783297694(TRANS, AS_BOOL_LIT_EXPR, dTP);
dAM_EXPR TRANS_196236678(TRANS, AS_VOID_EXPR, dTP);
dAM_EXPR TRANS_266129750(TRANS, AS_NEAR_EXPR, dTP);
dAM_EXPR TRANS_31851033(TRANS, AS_CLUSTER_EXPR, dTP);
dAM_EXPR TRANS_361080840(TRANS, AS_OR_EXPR, dTP);
dAM_EXPR TRANS_593837946(TRANS, AS_SELF_EXPR, dTP);
dAM_EXPR TRANS_595522723(TRANS, AS_ARRAY_EXPR, dTP);
dAM_EXPR TRANS_637585(TRANS, AS_FAR_EXPR, dTP);
dAM_EXPR TRANS_693526804(TRANS, dAS_EXPR, dTP);
dAM_EXPR TRANS_700113784(TRANS, AS_CREATE_EXPR, dTP);
dAM_EXPR TRANS_809530892(TRANS, AS_COHORT_EXPR, dTP);
dAM_EXPR TRANS_921970742(TRANS, AS_CLU553570599, dTP);
dAM_EXPR TRANS_962243143(TRANS, AS_NEW_EXPR, dTP);
dAM_STMT TRANS_1207936294(TRANS, AS_CALL_EXPR, AS_ASSIGN_STMT);
dAM_STMT TRANS_1573138211(TRANS, AS_DEC_STMT);
dAM_STMT TRANS_1920518219(TRANS, AS_ASSIGN_STMT);
dAM_STMT TRANS_471579016(TRANS, AS_CALL_EXPR, AS_ASSIGN_STMT);
dAM_STMT TRANS_496546257(TRANS, AM_LOCAL_EXPR, AS_ASSIGN_STMT);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void AS_ARG1328804463(AS_ARG_MODE, AS_ARG_MODE);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void SE_CON1222696490(SE_CONTEXT, dAM, BOOL);
void SE_CON412941612(SE_CONTEXT, dAM);
void TRANS_1191040045(TRANS, dAS_STMT);
void TRANS_1611318779(TRANS, AS_STMT_LIST);
void TRANS_423329532(TRANS, AM_LOCAL_EXPR, AS_ASSIGN_STMT);
void TRANS_548661555(TRANS, AS_ASSIGN_STMT);
void TRANS_626807411(TRANS, AM_LOCAL_EXPR);
void TRANS_673475073(TRANS, AS_ROUT_DEF);
void TRANS_74748323(TRANS, AS_TYPE_SPEC);
void TRANS_75192950(TRANS, AS_ROUT_DEF);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR TRANS_2133809761(TRANS self) {
 AM_LOCAL_EXPR ret_val;
 INT idx = INT_zero;
 AM_LOCAL_EXPR loc;
 AM_LOCAL_EXPR loc1;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 BOOL L11_;
 INT L21_;
 AM_LOCAL_EXPR L31_;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 STR L16;
 extern STR S_pS_par_ob;
 AM_ROUT_DEF L17;
 INT L181_;
 BOOL L191_;
 INT L201_;
 AM_ROUT_DEF L22;
 AM_FORMAL_ARG L231_;
 AM_FORMAL_ARG L24;
 BOOL L25;
 BOOL L26;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L321_;
 INT L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 STR L37;
 extern STR S_pS_par_ob;
 idx = FLISTA1151473122(ATTR(self,active_locals));
 while (1) {
  L11_=(0)<(idx); 
  if (L11_) {
  }
  else {
   goto after_loop;
  }
  L21_=INTMINUS(idx,1); 
  idx = L21_;
  aget_self = ATTR(self,active_locals);
  aget_ind = idx;
  L31_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
  ret_val1 = L31_;
  loc = ret_val1;
  L7 = (ATTR(loc,name1).str==(STR)0);
  L81_=!(L7); 
  if (L81_) {
   L9 = (ATTR(loc,tp_at)==((dTP) NULL));
   L101_=!(L9); 
   L6 = L101_;
  } else {
   L6 = FALSE;
  }
  if (L6) {
   L12 = STR_sizerINT(ATTR(loc,name1).str);
   L131_=(L12)<(10); 
   L14 = L131_;
   L151_=!(L14); 
   L5 = L151_;
  } else {
   L5 = FALSE;
  }
  if (L5) {
   L16 = STR_head_INTrSTR(ATTR(loc,name1).str, 10);
   L4 = STR_is111530248(L16, ((STR) &S_pS_par_ob));
  } else {
   L4 = FALSE;
  }
  if (L4) {
   ret_val = loc;
   return ret_val;
  }
 }
 after_loop: ;
 L17 = ATTR(self,cur_rout);
 L181_=(L17)==NULL?0:ASIZE((AM_ROUT_DEF)L17); 
 idx = L181_;
 while (1) {
  L191_=(0)<(idx); 
  if (L191_) {
  }
  else {
   goto after_loop1;
  }
  L201_=INTMINUS(idx,1); 
  idx = L201_;
  L22 = ATTR(self,cur_rout);
  L231_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L22,idx); 
  L24=L231_;
  loc1 = ATTR(L24,expr);
  L28 = (ATTR(loc1,name1).str==(STR)0);
  L291_=!(L28); 
  if (L291_) {
   L30 = (ATTR(loc1,tp_at)==((dTP) NULL));
   L321_=!(L30); 
   L27 = L321_;
  } else {
   L27 = FALSE;
  }
  if (L27) {
   L33 = STR_sizerINT(ATTR(loc1,name1).str);
   L341_=(L33)<(10); 
   L35 = L341_;
   L361_=!(L35); 
   L26 = L361_;
  } else {
   L26 = FALSE;
  }
  if (L26) {
   L37 = STR_head_INTrSTR(ATTR(loc1,name1).str, 10);
   L25 = STR_is111530248(L37, ((STR) &S_pS_par_ob));
  } else {
   L25 = FALSE;
  }
  if (L25) {
   ret_val = loc1;
   return ret_val;
  }
 }
 after_loop1: ;
 ret_val = ((AM_LOCAL_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR TRANS_647716465(TRANS self) {
 AM_LOCAL_EXPR ret_val;
 INT idx = INT_zero;
 AM_LOCAL_EXPR loc;
 AM_LOCAL_EXPR loc1;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 BOOL L11_;
 INT L21_;
 AM_LOCAL_EXPR L31_;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 STR L17;
 extern STR S_pS_par;
 STR L18;
 extern STR S_pS_att;
 AM_ROUT_DEF L19;
 INT L201_;
 BOOL L221_;
 INT L231_;
 AM_ROUT_DEF L24;
 AM_FORMAL_ARG L251_;
 AM_FORMAL_ARG L26;
 BOOL L27;
 BOOL L28;
 BOOL L29;
 BOOL L30;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 INT L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 BOOL L39;
 STR L40;
 extern STR S_pS_par;
 STR L41;
 extern STR S_pS_att;
 idx = FLISTA1151473122(ATTR(self,active_locals));
 while (1) {
  L11_=(0)<(idx); 
  if (L11_) {
  }
  else {
   goto after_loop;
  }
  L21_=INTMINUS(idx,1); 
  idx = L21_;
  aget_self = ATTR(self,active_locals);
  aget_ind = idx;
  L31_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
  ret_val1 = L31_;
  loc = ret_val1;
  L7 = (ATTR(loc,name1).str==(STR)0);
  L81_=!(L7); 
  if (L81_) {
   L9 = (ATTR(loc,tp_at)==((dTP) NULL));
   L101_=!(L9); 
   L6 = L101_;
  } else {
   L6 = FALSE;
  }
  if (L6) {
   L12 = STR_sizerINT(ATTR(loc,name1).str);
   L131_=(L12)<(7); 
   L14 = L131_;
   L151_=!(L14); 
   L5 = L151_;
  } else {
   L5 = FALSE;
  }
  if (L5) {
   L17 = STR_head_INTrSTR(ATTR(loc,name1).str, 7);
   if (STR_is111530248(L17, ((STR) &S_pS_par))) {
    L16 = TRUE;
   } else {
    L18 = STR_head_INTrSTR(ATTR(loc,name1).str, 7);
    L16 = STR_is111530248(L18, ((STR) &S_pS_att));
   }
   L4 = L16;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   ret_val = loc;
   return ret_val;
  }
 }
 after_loop: ;
 L19 = ATTR(self,cur_rout);
 L201_=(L19)==NULL?0:ASIZE((AM_ROUT_DEF)L19); 
 idx = L201_;
 while (1) {
  L221_=(0)<(idx); 
  if (L221_) {
  }
  else {
   goto after_loop1;
  }
  L231_=INTMINUS(idx,1); 
  idx = L231_;
  L24 = ATTR(self,cur_rout);
  L251_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L24,idx); 
  L26=L251_;
  loc1 = ATTR(L26,expr);
  L30 = (ATTR(loc1,name1).str==(STR)0);
  L321_=!(L30); 
  if (L321_) {
   L33 = (ATTR(loc1,tp_at)==((dTP) NULL));
   L341_=!(L33); 
   L29 = L341_;
  } else {
   L29 = FALSE;
  }
  if (L29) {
   L35 = STR_sizerINT(ATTR(loc1,name1).str);
   L361_=(L35)<(7); 
   L37 = L361_;
   L381_=!(L37); 
   L28 = L381_;
  } else {
   L28 = FALSE;
  }
  if (L28) {
   L40 = STR_head_INTrSTR(ATTR(loc1,name1).str, 7);
   if (STR_is111530248(L40, ((STR) &S_pS_par))) {
    L39 = TRUE;
   } else {
    L41 = STR_head_INTrSTR(ATTR(loc1,name1).str, 7);
    L39 = STR_is111530248(L41, ((STR) &S_pS_att));
   }
   L27 = L39;
  } else {
   L27 = FALSE;
  }
  if (L27) {
   ret_val = loc1;
   return ret_val;
  }
 }
 after_loop1: ;
 ret_val = ((AM_LOCAL_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF TRANS_1291044671(TRANS self, ELT e, AS_SHARED_DEF as) {
 AM_ROUT_DEF ret_val;
 AM_LOCAL_EXPR sl = ((AM_LOCAL_EXPR) NULL);
 AM_LOCAL_EXPR l1 = ((AM_LOCAL_EXPR) NULL);
 AM_ROUT_DEF r;
 AM_SHARED_EXPR g;
 AM_RETURN_STMT ar;
 AM_ROUT_DEF r1;
 AM_SHARED_EXPR g11;
 AM_ASSIGN_STMT ar1;
 AM_INV1167837230 inv = ((AM_INV1167837230) NULL);
 SIG isig;
 AM_ROU1916046290 icall;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val1;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 AM_ROUT_DEF create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF r2;
 ELT tp_self;
 dTP ret_val5;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val6;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self1;
 AS_TYPE_SPEC ret_val7;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val8;
 AM_LOCAL_EXPR r3;
 AM_FORMAL_ARG create_self3;
 AM_LOCAL_EXPR create_e;
 AM_FORMAL_ARG ret_val9;
 AM_FORMAL_ARG res1;
 ELT name_self;
 IDENT ret_val10 = IDENT_zero;
 AM_SHARED_EXPR create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val11;
 AM_SHARED_EXPR r4;
 ELT name_self1;
 IDENT ret_val12 = IDENT_zero;
 ELT as_tp_self1;
 AS_TYPE_SPEC ret_val13;
 dAS_CLASS_ELT clelt1;
 AS_TYPE_SPEC res2 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self5;
 AS_TYPE_SPEC ret_val14;
 ELT ret_self;
 dTP ret_val15;
 AM_SHARED_EXPR tp_self1;
 dTP ret_val16;
 GLOBAL_TBL insert_self;
 AM_SHARED_EXPR insert_g;
 FMAPID739086519 m;
 AM_RETURN_STMT create_self6;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_RETURN_STMT ret_val17;
 AM_RETURN_STMT r5;
 AM_ROUT_DEF create_self7;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_ROUT_DEF ret_val18;
 AM_ROUT_DEF r6;
 ELT tp_self2;
 dTP ret_val19;
 ELT as_tp_self2;
 AS_TYPE_SPEC ret_val20;
 dAS_CLASS_ELT clelt2;
 AS_TYPE_SPEC res3 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self8;
 AS_TYPE_SPEC ret_val21;
 AM_LOCAL_EXPR create_self9;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AS_TYPE_SPEC create_as_tp1;
 AM_LOCAL_EXPR ret_val22;
 AM_LOCAL_EXPR r7;
 AM_FORMAL_ARG create_self10;
 AM_LOCAL_EXPR create_e1;
 AM_FORMAL_ARG ret_val23;
 AM_FORMAL_ARG res4;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val24;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val25;
 ELT name_self2;
 IDENT ret_val26 = IDENT_zero;
 AM_LOCAL_EXPR create_self11;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name2 = IDENT_zero;
 dTP create_tp2;
 AM_LOCAL_EXPR ret_val27;
 AM_LOCAL_EXPR r8;
 AM_FORMAL_ARG create_self12;
 AM_LOCAL_EXPR create_e2;
 AM_FORMAL_ARG ret_val28;
 AM_FORMAL_ARG res5;
 ELT name_self3;
 IDENT ret_val29 = IDENT_zero;
 AM_SHARED_EXPR create_self13;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val30;
 AM_SHARED_EXPR r9;
 ELT name_self4;
 IDENT ret_val31 = IDENT_zero;
 ELT as_tp_self3;
 AS_TYPE_SPEC ret_val32;
 dAS_CLASS_ELT clelt3;
 AS_TYPE_SPEC res6 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self14;
 AS_TYPE_SPEC ret_val33;
 AM_SHARED_EXPR tp_self3;
 dTP ret_val34;
 GLOBAL_TBL insert_self1;
 AM_SHARED_EXPR insert_g1;
 FMAPID739086519 m1;
 AM_ASSIGN_STMT create_self15;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val35;
 AM_ASSIGN_STMT r10;
 AM_INV1167837230 create_self16;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_INV1167837230 ret_val36;
 AM_INV1167837230 r11;
 AM_ROU1916046290 create_self17;
 INT create_nargs2 = INT_zero;
 SFILE_ID create_source7 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val37;
 AM_ROU1916046290 r12;
 BOOL L1;
 BOOL L2;
 dTP L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR Partia2053653834;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 AM_ROUT_DEF L13;
 INT L14;
 OB L15;
 INT L16;
 BOOL L17;
 BOOL L181_;
 AS_TYPE_SPEC L19;
 OB L20;
 extern STR INT1;
 AM_LOCAL_EXPR L21;
 OB L22;
 AM_FORMAL_ARG L23;
 OB L24;
 AM_FORMAL_ARG L25;
 GENERATE_AM L27;
 AM_SHARED_EXPR L28;
 OB L29;
 BOOL L30;
 BOOL L311_;
 AS_TYPE_SPEC L32;
 OB L33;
 extern STR INT1;
 GENERATE_AM L34;
 AM_RETURN_STMT L35;
 OB L36;
 AM_ROUT_DEF L37;
 INT L38;
 OB L39;
 INT L40;
 BOOL L41;
 BOOL L421_;
 AS_TYPE_SPEC L43;
 OB L44;
 extern STR INT1;
 AM_LOCAL_EXPR L45;
 OB L46;
 AM_FORMAL_ARG L47;
 OB L48;
 AM_FORMAL_ARG L49;
 extern STR Compil1435418303;
 BOOL L51;
 BOOL L52;
 extern STR name21;
 ARRAYARG L53;
 ARG L541_;
 ARG L55;
 AM_LOCAL_EXPR L56;
 OB L57;
 AM_FORMAL_ARG L58;
 OB L59;
 AM_FORMAL_ARG L60;
 GENERATE_AM L63;
 AM_SHARED_EXPR L64;
 OB L65;
 BOOL L66;
 BOOL L671_;
 AS_TYPE_SPEC L68;
 OB L69;
 extern STR INT1;
 GENERATE_AM L70;
 ARRAYARG L71;
 ARG L721_;
 ARG L73;
 AM_ASSIGN_STMT L74;
 OB L75;
 AM_FORMAL_ARG L761_;
 AM_FORMAL_ARG L77;
 BOOL L78;
 BOOL L79;
 BOOL L801_;
 BOOL L82;
 BOOL L831_;
 BOOL L84;
 BOOL L851_;
 AM_INV1167837230 L86;
 OB L87;
 AM_ROU1916046290 L88;
 INT L89;
 OB L90;
 INT L91;
 dAM_STMT L92;
 tp_of_self = self;
 tp_of_t = ATTR(as,tp);
 ret_val1 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
 L3 = ret_val1;
 if ((*dTP_is1553335918[TAG(L3)])(L3)) {
  L4 = ATTR(ATTR(as,tp),kind);
  L5 = AS_TYP1853679690;
  L61_=(L4)==(L5); 
  L7 = L61_;
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L9 = ATTR(ATTR(self,prog),is_check_am);
  L101_=!(L9); 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  err_self = self;
  err_s = ((STR) &Partia2053653834);
  if ((err_self==((TRANS) NULL))) {
   L12 = TRUE;
  } else {
   L12 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L12) {
   L11 = TRUE;
  } else {
   L11 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L11) {
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
 }
 TRANS_74748323(self, ATTR(as,tp));
 if (ELT_is1936066670(e)) {
  create_self = ((AM_ROUT_DEF) NULL);
  create_nargs = 1;
  create_source = ATTR(as,source1);
  L14 = create_nargs;
  L16=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L14)*sizeof(AM_FORMAL_ARG);
  L15=ZALLOC_BIG(L16);
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

    ((OB)L15)->header.destroyed=0;
#endif

  L13 = ((AM_ROUT_DEF) L15);
  L13->asize = L14;
  r2 = L13;
  SATTR(r2,source1,create_source);
  ret_val4 = r2;
  r = ret_val4;
  SATTR(self,cur_rout,r);
  SATTR(r,srcsig,ATTR(e,srcsig));
  create_self2 = ((AM_LOCAL_EXPR) NULL);
  create_src = ATTR(as,source1);
  create_name = IDENT_985337352;
  tp_self = e;
  ret_val5 = ATTR(ATTR(tp_self,sig1),tp);
  create_tp = ret_val5;
  as_tp_self = e;
  clelt = ATTR(as_tp_self,as);
  switch (TAG(clelt)) {
   case AS_CONST_DEF_tag:
    L17 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
    L181_=!(L17); 
    if (L181_) {
     res = ATTR(((AS_CONST_DEF) clelt),tp);
    }
    else {
     create_self1 = ((AS_TYPE_SPEC) NULL);
     L20=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L20==NULL) FATAL("Unable to allocate more memory");
     ((OB)L20)->header.tag=AS_TYPE_SPEC_tag;
     L19 = ((AS_TYPE_SPEC) L20);
     ret_val7 = L19;
     res = ret_val7;
     SATTR(res,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res = ATTR(((AS_SHARED_DEF) clelt),tp); break;
   case AS_ATTR_DEF_tag:
    res = ATTR(((AS_ATTR_DEF) clelt),tp); break;
   default: ;
  }
  ret_val6 = res;
  create_as_tp = ret_val6;
  L22=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=AM_LOCAL_EXPR_tag;
  L21 = ((AM_LOCAL_EXPR) L22);
  r3 = L21;
  SATTR(r3,source1,create_src);
  SATTR(r3,name1,create_name);
  SATTR(r3,tp_at,create_tp);
  SATTR(r3,as_type,create_as_tp);
  ret_val8 = r3;
  sl = ret_val8;
  create_self3 = ((AM_FORMAL_ARG) NULL);
  create_e = sl;
  L24=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=AM_FORMAL_ARG_tag;
  L23 = ((AM_FORMAL_ARG) L24);
  res1 = L23;
  SATTR(res1,expr,create_e);
  SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val9 = res1;
  L25 = ret_val9;
  SARR((AM_ROUT_DEF)r,0,(AM_FORMAL_ARG)L25); 
  ;
  SATTR(r,sig1,ATTR(e,sig1));
  SATTR(ATTR(r,sig1),srcsig,ATTR(e,srcsig));
  L27=TRANS_1867726633(self);
  name_self = e;
  ret_val10 = ATTR(ATTR(name_self,sig1),name1);
  g = GLOBAL499677090(ATTR(L27,global_tbl), ret_val10, ATTR(ATTR(self,impl),tp));
  if ((g==((AM_SHARED_EXPR) NULL))) {
   create_self4 = ((AM_SHARED_EXPR) NULL);
   create_source1 = ATTR(as,source1);
   L29=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=AM_SHARED_EXPR_tag;
   L28 = ((AM_SHARED_EXPR) L29);
   r4 = L28;
   SATTR(r4,source1,create_source1);
   ret_val11 = r4;
   g = ret_val11;
   name_self1 = e;
   ret_val12 = ATTR(ATTR(name_self1,sig1),name1);
   SATTR(g,name1,ret_val12);
   as_tp_self1 = e;
   clelt1 = ATTR(as_tp_self1,as);
   switch (TAG(clelt1)) {
    case AS_CONST_DEF_tag:
     L30 = (ATTR(((AS_CONST_DEF) clelt1),tp)==((AS_TYPE_SPEC) NULL));
     L311_=!(L30); 
     if (L311_) {
      res2 = ATTR(((AS_CONST_DEF) clelt1),tp);
     }
     else {
      create_self5 = ((AS_TYPE_SPEC) NULL);
      L33=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
      if (L33==NULL) FATAL("Unable to allocate more memory");
      ((OB)L33)->header.tag=AS_TYPE_SPEC_tag;
      L32 = ((AS_TYPE_SPEC) L33);
      ret_val14 = L32;
      res2 = ret_val14;
      SATTR(res2,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
     } break;
    case AS_SHARED_DEF_tag:
     res2 = ATTR(((AS_SHARED_DEF) clelt1),tp); break;
    case AS_ATTR_DEF_tag:
     res2 = ATTR(((AS_ATTR_DEF) clelt1),tp); break;
    default: ;
   }
   ret_val13 = res2;
   SATTR(g,as_type,ret_val13);
   ret_self = e;
   ret_val15 = ATTR(ATTR(ret_self,sig1),ret);
   SATTR(g,tp_at,ret_val15);
   SATTR(g,class_tp,ATTR(ATTR(self,impl),tp));
   SATTR(self,in_constant,TRUE);
   tp_self1 = g;
   ret_val16 = ATTR(tp_self1,tp_at);
   SATTR(g,init,TRANS_693526804(self, ATTR(as,init), ret_val16));
   SATTR(self,in_constant,FALSE);
   L34=TRANS_1867726633(self);
   insert_self = ATTR(L34,global_tbl);
   insert_g = g;
   m = FMAPdT183078507(ATTR(insert_self,tbl), ATTR(insert_g,class_tp));
   m = FMAPID1512695779(m, ATTR(insert_g,name1), insert_g);
   SATTR(insert_self,tbl,FMAPdT1221772946(ATTR(insert_self,tbl), ATTR(insert_g,class_tp), m));
   SATTR(insert_self,top_sort,FLISTA327355535(ATTR(insert_self,top_sort), insert_g));
  }
  SATTR(g,tp_at,ATTR(ATTR(e,sig1),ret));
  SE_CON1222696490(ATTR(self,cur_se), ((dAM) g), FALSE);
  create_self6 = ((AM_RETURN_STMT) NULL);
  create_source2 = ATTR(as,source1);
  L36=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  ((OB)L36)->header.tag=AM_RETURN_STMT_tag;
  L35 = ((AM_RETURN_STMT) L36);
  r5 = L35;
  SATTR(r5,source1,create_source2);
  ret_val17 = r5;
  ar = ret_val17;
  SATTR(ar,val1,((dAM_EXPR) g));
  SATTR(r,code1,((dAM_STMT) ar));
  SATTR(r,is_clean,TRUE);
  ret_val = r;
  return ret_val;
 }
 else {
  create_self7 = ((AM_ROUT_DEF) NULL);
  create_nargs1 = 2;
  create_source3 = ATTR(as,source1);
  L38 = create_nargs1;
  L40=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L38)*sizeof(AM_FORMAL_ARG);
  L39=ZALLOC_BIG(L40);
  if (L39==NULL) FATAL("Unable to allocate more memory");
  ((OB)L39)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

    ((OB)L39)->header.destroyed=0;
#endif

  L37 = ((AM_ROUT_DEF) L39);
  L37->asize = L38;
  r6 = L37;
  SATTR(r6,source1,create_source3);
  ret_val18 = r6;
  r1 = ret_val18;
  SATTR(self,cur_rout,r1);
  SATTR(r1,srcsig,ATTR(e,srcsig));
  create_self9 = ((AM_LOCAL_EXPR) NULL);
  create_src1 = ATTR(as,source1);
  create_name1 = IDENT_985337352;
  tp_self2 = e;
  ret_val19 = ATTR(ATTR(tp_self2,sig1),tp);
  create_tp1 = ret_val19;
  as_tp_self2 = e;
  clelt2 = ATTR(as_tp_self2,as);
  switch (TAG(clelt2)) {
   case AS_CONST_DEF_tag:
    L41 = (ATTR(((AS_CONST_DEF) clelt2),tp)==((AS_TYPE_SPEC) NULL));
    L421_=!(L41); 
    if (L421_) {
     res3 = ATTR(((AS_CONST_DEF) clelt2),tp);
    }
    else {
     create_self8 = ((AS_TYPE_SPEC) NULL);
     L44=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L44==NULL) FATAL("Unable to allocate more memory");
     ((OB)L44)->header.tag=AS_TYPE_SPEC_tag;
     L43 = ((AS_TYPE_SPEC) L44);
     ret_val21 = L43;
     res3 = ret_val21;
     SATTR(res3,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res3 = ATTR(((AS_SHARED_DEF) clelt2),tp); break;
   case AS_ATTR_DEF_tag:
    res3 = ATTR(((AS_ATTR_DEF) clelt2),tp); break;
   default: ;
  }
  ret_val20 = res3;
  create_as_tp1 = ret_val20;
  L46=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L46==NULL) FATAL("Unable to allocate more memory");
  ((OB)L46)->header.tag=AM_LOCAL_EXPR_tag;
  L45 = ((AM_LOCAL_EXPR) L46);
  r7 = L45;
  SATTR(r7,source1,create_src1);
  SATTR(r7,name1,create_name1);
  SATTR(r7,tp_at,create_tp1);
  SATTR(r7,as_type,create_as_tp1);
  ret_val22 = r7;
  sl = ret_val22;
  create_self10 = ((AM_FORMAL_ARG) NULL);
  create_e1 = sl;
  L48=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L48==NULL) FATAL("Unable to allocate more memory");
  ((OB)L48)->header.tag=AM_FORMAL_ARG_tag;
  L47 = ((AM_FORMAL_ARG) L48);
  res4 = L47;
  SATTR(res4,expr,create_e1);
  SATTR(res4,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val23 = res4;
  L49 = ret_val23;
  SARR((AM_ROUT_DEF)r1,0,(AM_FORMAL_ARG)L49); 
  ;
  if ((ATTR(ATTR(e,sig1),args)==((ARRAYARG) NULL))) {
   err_self1 = self;
   err_s1 = ((STR) &Compil1435418303);
   if ((err_self1==((TRANS) NULL))) {
    L52 = TRUE;
   } else {
    L52 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L52) {
    L51 = TRUE;
   } else {
    L51 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L51) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val24 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val24;
    plus_sarg3 = err_s1;
    ret_val25 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val25;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   ret_val = ((AM_ROUT_DEF) NULL);
   return ret_val;
  }
  create_self11 = ((AM_LOCAL_EXPR) NULL);
  create_src2 = ATTR(as,source1);
  name_self2 = e;
  ret_val26 = ATTR(ATTR(name_self2,sig1),name1);
  create_name2 = ret_val26;
  L53 = ATTR(ATTR(e,sig1),args);
  L541_=(ARG)ARR((ARRAYARG)L53,0); 
  L55=L541_;
  create_tp2 = ATTR(L55,tp);
  L57=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L57==NULL) FATAL("Unable to allocate more memory");
  ((OB)L57)->header.tag=AM_LOCAL_EXPR_tag;
  L56 = ((AM_LOCAL_EXPR) L57);
  r8 = L56;
  SATTR(r8,source1,create_src2);
  SATTR(r8,name1,create_name2);
  SATTR(r8,tp_at,create_tp2);
  ret_val27 = r8;
  l1 = ret_val27;
  create_self12 = ((AM_FORMAL_ARG) NULL);
  create_e2 = l1;
  L59=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L59==NULL) FATAL("Unable to allocate more memory");
  ((OB)L59)->header.tag=AM_FORMAL_ARG_tag;
  L58 = ((AM_FORMAL_ARG) L59);
  res5 = L58;
  SATTR(res5,expr,create_e2);
  SATTR(res5,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val28 = res5;
  L60 = ret_val28;
  SARR((AM_ROUT_DEF)r1,1,(AM_FORMAL_ARG)L60); 
  ;
  SATTR(r1,sig1,ATTR(e,sig1));
  SATTR(ATTR(r1,sig1),srcsig,ATTR(e,srcsig));
  L63=TRANS_1867726633(self);
  name_self3 = e;
  ret_val29 = ATTR(ATTR(name_self3,sig1),name1);
  g11 = GLOBAL499677090(ATTR(L63,global_tbl), ret_val29, ATTR(ATTR(self,impl),tp));
  if ((g11==((AM_SHARED_EXPR) NULL))) {
   create_self13 = ((AM_SHARED_EXPR) NULL);
   create_source4 = ATTR(as,source1);
   L65=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=AM_SHARED_EXPR_tag;
   L64 = ((AM_SHARED_EXPR) L65);
   r9 = L64;
   SATTR(r9,source1,create_source4);
   ret_val30 = r9;
   g11 = ret_val30;
   name_self4 = e;
   ret_val31 = ATTR(ATTR(name_self4,sig1),name1);
   SATTR(g11,name1,ret_val31);
   as_tp_self3 = e;
   clelt3 = ATTR(as_tp_self3,as);
   switch (TAG(clelt3)) {
    case AS_CONST_DEF_tag:
     L66 = (ATTR(((AS_CONST_DEF) clelt3),tp)==((AS_TYPE_SPEC) NULL));
     L671_=!(L66); 
     if (L671_) {
      res6 = ATTR(((AS_CONST_DEF) clelt3),tp);
     }
     else {
      create_self14 = ((AS_TYPE_SPEC) NULL);
      L69=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
      if (L69==NULL) FATAL("Unable to allocate more memory");
      ((OB)L69)->header.tag=AS_TYPE_SPEC_tag;
      L68 = ((AS_TYPE_SPEC) L69);
      ret_val33 = L68;
      res6 = ret_val33;
      SATTR(res6,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
     } break;
    case AS_SHARED_DEF_tag:
     res6 = ATTR(((AS_SHARED_DEF) clelt3),tp); break;
    case AS_ATTR_DEF_tag:
     res6 = ATTR(((AS_ATTR_DEF) clelt3),tp); break;
    default: ;
   }
   ret_val32 = res6;
   SATTR(g11,as_type,ret_val32);
   SATTR(g11,class_tp,ATTR(ATTR(self,impl),tp));
   SATTR(self,in_constant,TRUE);
   tp_self3 = g11;
   ret_val34 = ATTR(tp_self3,tp_at);
   SATTR(g11,init,TRANS_693526804(self, ATTR(as,init), ret_val34));
   SATTR(self,in_constant,FALSE);
   L70=TRANS_1867726633(self);
   insert_self1 = ATTR(L70,global_tbl);
   insert_g1 = g11;
   m1 = FMAPdT183078507(ATTR(insert_self1,tbl), ATTR(insert_g1,class_tp));
   m1 = FMAPID1512695779(m1, ATTR(insert_g1,name1), insert_g1);
   SATTR(insert_self1,tbl,FMAPdT1221772946(ATTR(insert_self1,tbl), ATTR(insert_g1,class_tp), m1));
   SATTR(insert_self1,top_sort,FLISTA327355535(ATTR(insert_self1,top_sort), insert_g1));
  }
  L71 = ATTR(ATTR(e,sig1),args);
  L721_=(ARG)ARR((ARRAYARG)L71,0); 
  L73=L721_;
  SATTR(g11,tp_at,ATTR(L73,tp));
  SE_CON1222696490(ATTR(self,cur_se), ((dAM) g11), TRUE);
  create_self15 = ((AM_ASSIGN_STMT) NULL);
  create_source5 = ATTR(as,source1);
  L75=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L75==NULL) FATAL("Unable to allocate more memory");
  ((OB)L75)->header.tag=AM_ASSIGN_STMT_tag;
  L74 = ((AM_ASSIGN_STMT) L75);
  r10 = L74;
  SATTR(r10,source1,create_source5);
  ret_val35 = r10;
  ar1 = ret_val35;
  SATTR(ar1,dest,((dAM_EXPR) g11));
  L761_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r1,1); 
  L77=L761_;
  SATTR(ar1,src1,((dAM_EXPR) ATTR(L77,expr)));
  L79 = ATTR(e,is_private);
  L801_=!(L79); 
  if (L801_) {
   L82 = ATTR(self,in_invariant);
   L831_=!(L82); 
   L78 = L831_;
  } else {
   L78 = FALSE;
  }
  if (L78) {
   isig = IMPL_i1943552357(ATTR(self,impl));
   L84 = (isig==((SIG) NULL));
   L851_=!(L84); 
   if (L851_) {
    create_self16 = ((AM_INV1167837230) NULL);
    create_source6 = ATTR(as,source1);
    L87=ZALLOC(sizeof(struct AM_INV1167837230_struct));
    if (L87==NULL) FATAL("Unable to allocate more memory");
    ((OB)L87)->header.tag=AM_INV1167837230_tag;
    L86 = ((AM_INV1167837230) L87);
    r11 = L86;
    SATTR(r11,source1,create_source6);
    ret_val36 = r11;
    inv = ret_val36;
    SATTR(inv,sig1,isig);
    create_self17 = ((AM_ROU1916046290) NULL);
    create_nargs2 = 1;
    create_source7 = ATTR(as,source1);
    L89 = create_nargs2;
    L91=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L89)*sizeof(AM_CALL_ARG);
    L90=ZALLOC_BIG(L91);
    if (L90==NULL) FATAL("Unable to allocate more memory");
    ((OB)L90)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

      ((OB)L90)->header.destroyed=0;
#endif

    L88 = ((AM_ROU1916046290) L90);
    L88->asize = L89;
    r12 = L88;
    SATTR(r12,source1,create_source7);
    ret_val37 = r12;
    icall = ret_val37;
    SATTR(icall,fun,isig);
    SE_CON412941612(ATTR(self,cur_se), ((dAM) icall));
    SATTR(r1,calls1,FLISTd805769956(ATTR(r1,calls1), ((dAM_EXPR) icall)));
   }
  }
  SATTR(r1,code1,((dAM_STMT) ar1));
  if ((ATTR(r1,code1)==((dAM_STMT) NULL))) {
   SATTR(r1,code1,((dAM_STMT) inv));
  }
  else {
   L92 = ATTR(r1,code1);
   (*dAM_ST2020072576[TAG(L92)])(L92, ((dAM_STMT) inv));
  }
  SATTR(r1,is_clean,FALSE);
  ret_val = r1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF TRANS_1584821941(TRANS self, ELT e, AS_ATTR_DEF as) {
 AM_ROUT_DEF ret_val;
 AM_LOCAL_EXPR sl = ((AM_LOCAL_EXPR) NULL);
 AM_LOCAL_EXPR l1 = ((AM_LOCAL_EXPR) NULL);
 AM_LOCAL_EXPR l2 = ((AM_LOCAL_EXPR) NULL);
 AM_ROUT_DEF r = ((AM_ROUT_DEF) NULL);
 AM_ATTR_EXPR ae;
 AM_RETURN_STMT ar;
 AM_VAT319982528 av;
 AM_RETURN_STMT ar1;
 AM_ATTR_EXPR ae1;
 AM_ASSIGN_STMT ar2;
 AM_INV1167837230 inv = ((AM_INV1167837230) NULL);
 SIG isig;
 AM_ROU1916046290 icall;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val1;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 AM_ROUT_DEF create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF r1;
 ELT tp_self;
 dTP ret_val5;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val6;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self1;
 AS_TYPE_SPEC ret_val7;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val8;
 AM_LOCAL_EXPR r2;
 AM_FORMAL_ARG create_self3;
 AM_LOCAL_EXPR create_e;
 AM_FORMAL_ARG ret_val9;
 AM_FORMAL_ARG res1;
 AM_ATTR_EXPR create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val10;
 AM_ATTR_EXPR r3;
 ELT as_tp_self1;
 AS_TYPE_SPEC ret_val11;
 dAS_CLASS_ELT clelt1;
 AS_TYPE_SPEC res2 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self5;
 AS_TYPE_SPEC ret_val12;
 ELT name_self;
 IDENT ret_val13 = IDENT_zero;
 TRANS tp_of_self1;
 AS_TYPE_SPEC tp_of_t1;
 dTP ret_val14;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val15;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val16;
 AM_RETURN_STMT create_self6;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_RETURN_STMT ret_val17;
 AM_RETURN_STMT r4;
 AM_ROUT_DEF create_self7;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_ROUT_DEF ret_val18;
 AM_ROUT_DEF r5;
 ELT tp_self1;
 dTP ret_val19;
 ELT as_tp_self2;
 AS_TYPE_SPEC ret_val20;
 dAS_CLASS_ELT clelt2;
 AS_TYPE_SPEC res3 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self8;
 AS_TYPE_SPEC ret_val21;
 AM_LOCAL_EXPR create_self9;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AS_TYPE_SPEC create_as_tp1;
 AM_LOCAL_EXPR ret_val22;
 AM_LOCAL_EXPR r6;
 AM_FORMAL_ARG create_self10;
 AM_LOCAL_EXPR create_e1;
 AM_FORMAL_ARG ret_val23;
 AM_FORMAL_ARG res4;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val24;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val25;
 ELT name_self1;
 IDENT ret_val26 = IDENT_zero;
 AM_LOCAL_EXPR create_self11;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name2 = IDENT_zero;
 dTP create_tp2;
 AM_LOCAL_EXPR ret_val27;
 AM_LOCAL_EXPR r7;
 AM_FORMAL_ARG create_self12;
 AM_LOCAL_EXPR create_e2;
 AM_FORMAL_ARG ret_val28;
 AM_FORMAL_ARG res5;
 TP_CLASS kind_self;
 INT ret_val29 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val30;
 AM_VAT319982528 create_self13;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_VAT319982528 ret_val31;
 AM_VAT319982528 r8;
 ELT name_self2;
 IDENT ret_val32 = IDENT_zero;
 TRANS tp_of_self2;
 AS_TYPE_SPEC tp_of_t2;
 dTP ret_val33;
 AM_RETURN_STMT create_self14;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_RETURN_STMT ret_val34;
 AM_RETURN_STMT r9;
 AM_ATTR_EXPR create_self15;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val35;
 AM_ATTR_EXPR r10;
 ELT as_tp_self3;
 AS_TYPE_SPEC ret_val36;
 dAS_CLASS_ELT clelt3;
 AS_TYPE_SPEC res6 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self16;
 AS_TYPE_SPEC ret_val37;
 ELT name_self3;
 IDENT ret_val38 = IDENT_zero;
 TRANS tp_of_self3;
 AS_TYPE_SPEC tp_of_t3;
 dTP ret_val39;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val40;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val41;
 AM_ASSIGN_STMT create_self17;
 SFILE_ID create_source7 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val42;
 AM_ASSIGN_STMT r11;
 AM_INV1167837230 create_self18;
 SFILE_ID create_source8 = SFILE_ID_zero;
 AM_INV1167837230 ret_val43;
 AM_INV1167837230 r12;
 AM_ROU1916046290 create_self19;
 INT create_nargs2 = INT_zero;
 SFILE_ID create_source9 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val44;
 AM_ROU1916046290 r13;
 BOOL L1;
 BOOL L2;
 dTP L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR Partia2053653834;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 AM_ROUT_DEF L13;
 INT L14;
 OB L15;
 INT L16;
 BOOL L17;
 BOOL L181_;
 AS_TYPE_SPEC L19;
 OB L20;
 extern STR INT1;
 AM_LOCAL_EXPR L21;
 OB L22;
 AM_FORMAL_ARG L23;
 OB L24;
 AM_FORMAL_ARG L25;
 AM_ATTR_EXPR L27;
 OB L28;
 BOOL L29;
 BOOL L301_;
 AS_TYPE_SPEC L31;
 OB L32;
 extern STR INT1;
 AM_FORMAL_ARG L331_;
 AM_FORMAL_ARG L34;
 dAM_EXPR L35;
 extern STR Cannot457229582;
 BOOL L36;
 BOOL L37;
 extern STR name21;
 BOOL L38;
 BOOL L391_;
 AM_RETURN_STMT L40;
 OB L41;
 AM_ROUT_DEF L42;
 INT L43;
 OB L44;
 INT L45;
 BOOL L46;
 BOOL L471_;
 AS_TYPE_SPEC L48;
 OB L49;
 extern STR INT1;
 AM_LOCAL_EXPR L50;
 OB L51;
 AM_FORMAL_ARG L52;
 OB L53;
 AM_FORMAL_ARG L54;
 extern STR Compil1678661998;
 BOOL L56;
 BOOL L57;
 extern STR name21;
 ARRAYARG L58;
 ARG L591_;
 ARG L60;
 AM_LOCAL_EXPR L62;
 OB L63;
 AM_FORMAL_ARG L64;
 OB L65;
 AM_FORMAL_ARG L66;
 INT L68;
 INT L69;
 BOOL L701_;
 dPARSE L71;
 IDENT L72;
 ARRAYdTP L73;
 INT L741_;
 dPARSE L75;
 BOOL L76;
 BOOL L771_;
 INT L78;
 INT L79;
 BOOL L801_;
 AM_VAT319982528 L82;
 OB L83;
 AM_FORMAL_ARG L841_;
 AM_FORMAL_ARG L85;
 AM_FORMAL_ARG L861_;
 AM_FORMAL_ARG L87;
 BOOL L88;
 BOOL L891_;
 AM_RETURN_STMT L90;
 OB L91;
 AM_ATTR_EXPR L92;
 OB L93;
 BOOL L94;
 BOOL L951_;
 AS_TYPE_SPEC L96;
 OB L97;
 extern STR INT1;
 AM_FORMAL_ARG L981_;
 AM_FORMAL_ARG L99;
 dAM_EXPR L100;
 extern STR Cannot457229582;
 BOOL L102;
 BOOL L103;
 extern STR name21;
 BOOL L104;
 BOOL L1051_;
 AM_ASSIGN_STMT L106;
 OB L107;
 AM_FORMAL_ARG L1081_;
 AM_FORMAL_ARG L109;
 BOOL L110;
 BOOL L111;
 BOOL L1121_;
 BOOL L113;
 BOOL L1141_;
 BOOL L115;
 BOOL L1161_;
 AM_INV1167837230 L117;
 OB L118;
 AM_ROU1916046290 L119;
 INT L120;
 OB L121;
 INT L122;
 dAM_STMT L123;
 tp_of_self = self;
 tp_of_t = ATTR(as,tp);
 ret_val1 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
 L3 = ret_val1;
 if ((*dTP_is1553335918[TAG(L3)])(L3)) {
  L4 = ATTR(ATTR(as,tp),kind);
  L5 = AS_TYP1853679690;
  L61_=(L4)==(L5); 
  L7 = L61_;
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L9 = ATTR(ATTR(self,prog),is_check_am);
  L101_=!(L9); 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  err_self = self;
  err_s = ((STR) &Partia2053653834);
  if ((err_self==((TRANS) NULL))) {
   L12 = TRUE;
  } else {
   L12 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L12) {
   L11 = TRUE;
  } else {
   L11 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L11) {
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
 }
 TRANS_74748323(self, ATTR(as,tp));
 if (ELT_is380037919(e)) {
  create_self = ((AM_ROUT_DEF) NULL);
  create_nargs = 1;
  create_source = ATTR(as,source1);
  L14 = create_nargs;
  L16=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L14)*sizeof(AM_FORMAL_ARG);
  L15=ZALLOC_BIG(L16);
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

    ((OB)L15)->header.destroyed=0;
#endif

  L13 = ((AM_ROUT_DEF) L15);
  L13->asize = L14;
  r1 = L13;
  SATTR(r1,source1,create_source);
  ret_val4 = r1;
  r = ret_val4;
  SATTR(r,srcsig,ATTR(e,srcsig));
  create_self2 = ((AM_LOCAL_EXPR) NULL);
  create_src = ATTR(as,source1);
  create_name = IDENT_985337352;
  tp_self = e;
  ret_val5 = ATTR(ATTR(tp_self,sig1),tp);
  create_tp = ret_val5;
  as_tp_self = e;
  clelt = ATTR(as_tp_self,as);
  switch (TAG(clelt)) {
   case AS_CONST_DEF_tag:
    L17 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
    L181_=!(L17); 
    if (L181_) {
     res = ATTR(((AS_CONST_DEF) clelt),tp);
    }
    else {
     create_self1 = ((AS_TYPE_SPEC) NULL);
     L20=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L20==NULL) FATAL("Unable to allocate more memory");
     ((OB)L20)->header.tag=AS_TYPE_SPEC_tag;
     L19 = ((AS_TYPE_SPEC) L20);
     ret_val7 = L19;
     res = ret_val7;
     SATTR(res,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res = ATTR(((AS_SHARED_DEF) clelt),tp); break;
   case AS_ATTR_DEF_tag:
    res = ATTR(((AS_ATTR_DEF) clelt),tp); break;
   default: ;
  }
  ret_val6 = res;
  create_as_tp = ret_val6;
  L22=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=AM_LOCAL_EXPR_tag;
  L21 = ((AM_LOCAL_EXPR) L22);
  r2 = L21;
  SATTR(r2,source1,create_src);
  SATTR(r2,name1,create_name);
  SATTR(r2,tp_at,create_tp);
  SATTR(r2,as_type,create_as_tp);
  ret_val8 = r2;
  sl = ret_val8;
  create_self3 = ((AM_FORMAL_ARG) NULL);
  create_e = sl;
  L24=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=AM_FORMAL_ARG_tag;
  L23 = ((AM_FORMAL_ARG) L24);
  res1 = L23;
  SATTR(res1,expr,create_e);
  SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val9 = res1;
  L25 = ret_val9;
  SARR((AM_ROUT_DEF)r,0,(AM_FORMAL_ARG)L25); 
  ;
  SATTR(r,sig1,ATTR(e,sig1));
  SATTR(ATTR(r,sig1),srcsig,ATTR(e,srcsig));
  create_self4 = ((AM_ATTR_EXPR) NULL);
  create_source1 = ATTR(as,source1);
  L28=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=AM_ATTR_EXPR_tag;
  L27 = ((AM_ATTR_EXPR) L28);
  r3 = L27;
  SATTR(r3,source1,create_source1);
  ret_val10 = r3;
  ae = ret_val10;
  as_tp_self1 = e;
  clelt1 = ATTR(as_tp_self1,as);
  switch (TAG(clelt1)) {
   case AS_CONST_DEF_tag:
    L29 = (ATTR(((AS_CONST_DEF) clelt1),tp)==((AS_TYPE_SPEC) NULL));
    L301_=!(L29); 
    if (L301_) {
     res2 = ATTR(((AS_CONST_DEF) clelt1),tp);
    }
    else {
     create_self5 = ((AS_TYPE_SPEC) NULL);
     L32=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L32==NULL) FATAL("Unable to allocate more memory");
     ((OB)L32)->header.tag=AS_TYPE_SPEC_tag;
     L31 = ((AS_TYPE_SPEC) L32);
     ret_val12 = L31;
     res2 = ret_val12;
     SATTR(res2,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res2 = ATTR(((AS_SHARED_DEF) clelt1),tp); break;
   case AS_ATTR_DEF_tag:
    res2 = ATTR(((AS_ATTR_DEF) clelt1),tp); break;
   default: ;
  }
  ret_val11 = res2;
  SATTR(ae,as_type,ret_val11);
  L331_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r,0); 
  L34=L331_;
  SATTR(ae,ob,((dAM_EXPR) ATTR(L34,expr)));
  L35 = ATTR(ae,ob);
  SATTR(ae,self_tp,(*dAM_EXPR_tprdTP[TAG(L35)])(L35));
  name_self = e;
  ret_val13 = ATTR(ATTR(name_self,sig1),name1);
  SATTR(ae,at,ret_val13);
  tp_of_self1 = self;
  tp_of_t1 = ATTR(as,tp);
  ret_val14 = TP_CON1800432689(ATTR(tp_of_self1,tp_con), tp_of_t1);
  SATTR(ae,tp_at,ret_val14);
  if ((ATTR(ae,tp_at)==((dTP) NULL))) {
   err_loc_self = self;
   err_loc_t = ((dPROG_ERR) ATTR(as,tp));
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self1 = self;
   err_s1 = ((STR) &Cannot457229582);
   if ((err_self1==((TRANS) NULL))) {
    L37 = TRUE;
   } else {
    L37 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L37) {
    L36 = TRUE;
   } else {
    L36 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L36) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val15 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val15;
    plus_sarg3 = err_s1;
    ret_val16 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val16;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   ret_val = ((AM_ROUT_DEF) NULL);
   return ret_val;
  }
  SE_CON1222696490(ATTR(self,cur_se), ((dAM) ae), FALSE);
  L38 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
  L391_=!(L38); 
  if (L391_) {
   SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
  }
  create_self6 = ((AM_RETURN_STMT) NULL);
  create_source2 = ATTR(as,source1);
  L41=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
  if (L41==NULL) FATAL("Unable to allocate more memory");
  ((OB)L41)->header.tag=AM_RETURN_STMT_tag;
  L40 = ((AM_RETURN_STMT) L41);
  r4 = L40;
  SATTR(r4,source1,create_source2);
  ret_val17 = r4;
  ar = ret_val17;
  SATTR(ar,val1,((dAM_EXPR) ae));
  SATTR(r,code1,((dAM_STMT) ar));
  SATTR(r,is_clean,TRUE);
 }
 else {
  create_self7 = ((AM_ROUT_DEF) NULL);
  create_nargs1 = 2;
  create_source3 = ATTR(as,source1);
  L43 = create_nargs1;
  L45=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L43)*sizeof(AM_FORMAL_ARG);
  L44=ZALLOC_BIG(L45);
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

    ((OB)L44)->header.destroyed=0;
#endif

  L42 = ((AM_ROUT_DEF) L44);
  L42->asize = L43;
  r5 = L42;
  SATTR(r5,source1,create_source3);
  ret_val18 = r5;
  r = ret_val18;
  SATTR(r,srcsig,ATTR(e,srcsig));
  create_self9 = ((AM_LOCAL_EXPR) NULL);
  create_src1 = ATTR(as,source1);
  create_name1 = IDENT_985337352;
  tp_self1 = e;
  ret_val19 = ATTR(ATTR(tp_self1,sig1),tp);
  create_tp1 = ret_val19;
  as_tp_self2 = e;
  clelt2 = ATTR(as_tp_self2,as);
  switch (TAG(clelt2)) {
   case AS_CONST_DEF_tag:
    L46 = (ATTR(((AS_CONST_DEF) clelt2),tp)==((AS_TYPE_SPEC) NULL));
    L471_=!(L46); 
    if (L471_) {
     res3 = ATTR(((AS_CONST_DEF) clelt2),tp);
    }
    else {
     create_self8 = ((AS_TYPE_SPEC) NULL);
     L49=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L49==NULL) FATAL("Unable to allocate more memory");
     ((OB)L49)->header.tag=AS_TYPE_SPEC_tag;
     L48 = ((AS_TYPE_SPEC) L49);
     ret_val21 = L48;
     res3 = ret_val21;
     SATTR(res3,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res3 = ATTR(((AS_SHARED_DEF) clelt2),tp); break;
   case AS_ATTR_DEF_tag:
    res3 = ATTR(((AS_ATTR_DEF) clelt2),tp); break;
   default: ;
  }
  ret_val20 = res3;
  create_as_tp1 = ret_val20;
  L51=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L51==NULL) FATAL("Unable to allocate more memory");
  ((OB)L51)->header.tag=AM_LOCAL_EXPR_tag;
  L50 = ((AM_LOCAL_EXPR) L51);
  r6 = L50;
  SATTR(r6,source1,create_src1);
  SATTR(r6,name1,create_name1);
  SATTR(r6,tp_at,create_tp1);
  SATTR(r6,as_type,create_as_tp1);
  ret_val22 = r6;
  sl = ret_val22;
  create_self10 = ((AM_FORMAL_ARG) NULL);
  create_e1 = sl;
  L53=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L53==NULL) FATAL("Unable to allocate more memory");
  ((OB)L53)->header.tag=AM_FORMAL_ARG_tag;
  L52 = ((AM_FORMAL_ARG) L53);
  res4 = L52;
  SATTR(res4,expr,create_e1);
  SATTR(res4,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val23 = res4;
  L54 = ret_val23;
  SARR((AM_ROUT_DEF)r,0,(AM_FORMAL_ARG)L54); 
  ;
  if ((ATTR(ATTR(e,sig1),args)==((ARRAYARG) NULL))) {
   err_self2 = self;
   err_s2 = ((STR) &Compil1678661998);
   if ((err_self2==((TRANS) NULL))) {
    L57 = TRUE;
   } else {
    L57 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L57) {
    L56 = TRUE;
   } else {
    L56 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L56) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val24 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val24;
    plus_sarg5 = err_s2;
    ret_val25 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val25;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((AM_ROUT_DEF) NULL);
   return ret_val;
  }
  create_self11 = ((AM_LOCAL_EXPR) NULL);
  create_src2 = ATTR(as,source1);
  name_self1 = e;
  ret_val26 = ATTR(ATTR(name_self1,sig1),name1);
  create_name2 = ret_val26;
  L58 = ATTR(ATTR(e,sig1),args);
  L591_=(ARG)ARR((ARRAYARG)L58,0); 
  L60=L591_;
  create_tp2 = ATTR(L60,tp);
  L63=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L63==NULL) FATAL("Unable to allocate more memory");
  ((OB)L63)->header.tag=AM_LOCAL_EXPR_tag;
  L62 = ((AM_LOCAL_EXPR) L63);
  r7 = L62;
  SATTR(r7,source1,create_src2);
  SATTR(r7,name1,create_name2);
  SATTR(r7,tp_at,create_tp2);
  ret_val27 = r7;
  l1 = ret_val27;
  create_self12 = ((AM_FORMAL_ARG) NULL);
  create_e2 = l1;
  L65=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
  if (L65==NULL) FATAL("Unable to allocate more memory");
  ((OB)L65)->header.tag=AM_FORMAL_ARG_tag;
  L64 = ((AM_FORMAL_ARG) L65);
  res5 = L64;
  SATTR(res5,expr,create_e2);
  SATTR(res5,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val28 = res5;
  L66 = ret_val28;
  SARR((AM_ROUT_DEF)r,1,(AM_FORMAL_ARG)L66); 
  ;
  SATTR(r,sig1,ATTR(e,sig1));
  SATTR(ATTR(r,sig1),srcsig,ATTR(e,srcsig));
  kind_self = ATTR(ATTR(self,tp_con),same1);
  L68 = ATTR(kind_self,kind_cache);
  L69 = TP_KIN138039446;
  L701_=(L68)==(L69); 
  if (L701_) {
   as_tree_self = kind_self;
   if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
    L71 = ATTR(ATTR(as_tree_self,prog),parse);
    L72 = ATTR(as_tree_self,name1);
    L73 = ATTR(as_tree_self,params);
    L741_=(L73)==NULL?0:ASIZE((ARRAYdTP)L73); 
    L75 = L71;
    SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L75)])(L75, L72, L741_));
   }
   ret_val30 = ATTR(as_tree_self,my_as_tree);
   L76 = (ret_val30==((AS_CLASS_DEF) NULL));
   L771_=!(L76); 
   if (L771_) {
    SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
   }
  }
  ret_val29 = ATTR(kind_self,kind_cache);
  L78 = ret_val29;
  L79 = TP_KIND_val_tp;
  L801_=(L78)==(L79); 
  if (L801_) {
   create_self13 = ((AM_VAT319982528) NULL);
   create_source4 = ATTR(as,source1);
   L83=ZALLOC(sizeof(struct AM_VAT319982528_struct));
   if (L83==NULL) FATAL("Unable to allocate more memory");
   ((OB)L83)->header.tag=AM_VAT319982528_tag;
   L82 = ((AM_VAT319982528) L83);
   r8 = L82;
   SATTR(r8,source1,create_source4);
   ret_val31 = r8;
   av = ret_val31;
   L841_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r,0); 
   L85=L841_;
   SATTR(av,ob,((dAM_EXPR) ATTR(L85,expr)));
   name_self2 = e;
   ret_val32 = ATTR(ATTR(name_self2,sig1),name1);
   SATTR(av,at,ret_val32);
   L861_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r,1); 
   L87=L861_;
   SATTR(av,val1,((dAM_EXPR) ATTR(L87,expr)));
   tp_of_self2 = self;
   tp_of_t2 = ATTR(as,tp);
   ret_val33 = TP_CON1800432689(ATTR(tp_of_self2,tp_con), tp_of_t2);
   SATTR(av,real_tp,ret_val33);
   SE_CON1222696490(ATTR(self,cur_se), ((dAM) av), TRUE);
   L88 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
   L891_=!(L88); 
   if (L891_) {
    SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
   }
   create_self14 = ((AM_RETURN_STMT) NULL);
   create_source5 = ATTR(as,source1);
   L91=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
   if (L91==NULL) FATAL("Unable to allocate more memory");
   ((OB)L91)->header.tag=AM_RETURN_STMT_tag;
   L90 = ((AM_RETURN_STMT) L91);
   r9 = L90;
   SATTR(r9,source1,create_source5);
   ret_val34 = r9;
   ar1 = ret_val34;
   SATTR(ar1,val1,((dAM_EXPR) av));
   SATTR(r,code1,((dAM_STMT) ar1));
  }
  else {
   create_self15 = ((AM_ATTR_EXPR) NULL);
   create_source6 = ATTR(as,source1);
   L93=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
   if (L93==NULL) FATAL("Unable to allocate more memory");
   ((OB)L93)->header.tag=AM_ATTR_EXPR_tag;
   L92 = ((AM_ATTR_EXPR) L93);
   r10 = L92;
   SATTR(r10,source1,create_source6);
   ret_val35 = r10;
   ae1 = ret_val35;
   as_tp_self3 = e;
   clelt3 = ATTR(as_tp_self3,as);
   switch (TAG(clelt3)) {
    case AS_CONST_DEF_tag:
     L94 = (ATTR(((AS_CONST_DEF) clelt3),tp)==((AS_TYPE_SPEC) NULL));
     L951_=!(L94); 
     if (L951_) {
      res6 = ATTR(((AS_CONST_DEF) clelt3),tp);
     }
     else {
      create_self16 = ((AS_TYPE_SPEC) NULL);
      L97=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
      if (L97==NULL) FATAL("Unable to allocate more memory");
      ((OB)L97)->header.tag=AS_TYPE_SPEC_tag;
      L96 = ((AS_TYPE_SPEC) L97);
      ret_val37 = L96;
      res6 = ret_val37;
      SATTR(res6,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
     } break;
    case AS_SHARED_DEF_tag:
     res6 = ATTR(((AS_SHARED_DEF) clelt3),tp); break;
    case AS_ATTR_DEF_tag:
     res6 = ATTR(((AS_ATTR_DEF) clelt3),tp); break;
    default: ;
   }
   ret_val36 = res6;
   SATTR(ae1,as_type,ret_val36);
   L981_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r,0); 
   L99=L981_;
   SATTR(ae1,ob,((dAM_EXPR) ATTR(L99,expr)));
   L100 = ATTR(ae1,ob);
   SATTR(ae1,self_tp,(*dAM_EXPR_tprdTP[TAG(L100)])(L100));
   name_self3 = e;
   ret_val38 = ATTR(ATTR(name_self3,sig1),name1);
   SATTR(ae1,at,ret_val38);
   tp_of_self3 = self;
   tp_of_t3 = ATTR(as,tp);
   ret_val39 = TP_CON1800432689(ATTR(tp_of_self3,tp_con), tp_of_t3);
   SATTR(ae1,tp_at,ret_val39);
   if ((ATTR(ae1,tp_at)==((dTP) NULL))) {
    err_loc_self1 = self;
    err_loc_t1 = ((dPROG_ERR) ATTR(as,tp));
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self3 = self;
    err_s3 = ((STR) &Cannot457229582);
    if ((err_self3==((TRANS) NULL))) {
     L103 = TRUE;
    } else {
     L103 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L103) {
     L102 = TRUE;
    } else {
     L102 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
    }
    if (L102) {
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    else {
     plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val40 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val40;
     plus_sarg7 = err_s3;
     ret_val41 = STR_ap2004550586(plus_self7, plus_sarg7);
     err_s3 = ret_val41;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    ret_val = ((AM_ROUT_DEF) NULL);
    return ret_val;
   }
   SE_CON1222696490(ATTR(self,cur_se), ((dAM) ae1), TRUE);
   L104 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
   L1051_=!(L104); 
   if (L1051_) {
    SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
   }
   create_self17 = ((AM_ASSIGN_STMT) NULL);
   create_source7 = ATTR(as,source1);
   L107=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
   if (L107==NULL) FATAL("Unable to allocate more memory");
   ((OB)L107)->header.tag=AM_ASSIGN_STMT_tag;
   L106 = ((AM_ASSIGN_STMT) L107);
   r11 = L106;
   SATTR(r11,source1,create_source7);
   ret_val42 = r11;
   ar2 = ret_val42;
   SATTR(ar2,dest,((dAM_EXPR) ae1));
   L1081_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)r,1); 
   L109=L1081_;
   SATTR(ar2,src1,((dAM_EXPR) ATTR(L109,expr)));
   SATTR(r,code1,((dAM_STMT) ar2));
  }
  L111 = ATTR(e,is_private);
  L1121_=!(L111); 
  if (L1121_) {
   L113 = ATTR(self,in_invariant);
   L1141_=!(L113); 
   L110 = L1141_;
  } else {
   L110 = FALSE;
  }
  if (L110) {
   isig = IMPL_i1943552357(ATTR(self,impl));
   L115 = (isig==((SIG) NULL));
   L1161_=!(L115); 
   if (L1161_) {
    create_self18 = ((AM_INV1167837230) NULL);
    create_source8 = ATTR(as,source1);
    L118=ZALLOC(sizeof(struct AM_INV1167837230_struct));
    if (L118==NULL) FATAL("Unable to allocate more memory");
    ((OB)L118)->header.tag=AM_INV1167837230_tag;
    L117 = ((AM_INV1167837230) L118);
    r12 = L117;
    SATTR(r12,source1,create_source8);
    ret_val43 = r12;
    inv = ret_val43;
    SATTR(inv,sig1,isig);
    create_self19 = ((AM_ROU1916046290) NULL);
    create_nargs2 = 1;
    create_source9 = ATTR(as,source1);
    L120 = create_nargs2;
    L122=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L120)*sizeof(AM_CALL_ARG);
    L121=ZALLOC_BIG(L122);
    if (L121==NULL) FATAL("Unable to allocate more memory");
    ((OB)L121)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

      ((OB)L121)->header.destroyed=0;
#endif

    L119 = ((AM_ROU1916046290) L121);
    L119->asize = L120;
    r13 = L119;
    SATTR(r13,source1,create_source9);
    ret_val44 = r13;
    icall = ret_val44;
    SATTR(icall,fun,isig);
    SE_CON412941612(ATTR(self,cur_se), ((dAM) icall));
    SATTR(r,calls1,FLISTd805769956(ATTR(r,calls1), ((dAM_EXPR) icall)));
   }
  }
  if ((ATTR(r,code1)==((dAM_STMT) NULL))) {
   SATTR(r,code1,((dAM_STMT) inv));
  }
  else {
   L123 = ATTR(r,code1);
   (*dAM_ST2020072576[TAG(L123)])(L123, ((dAM_STMT) inv));
  }
  SATTR(r,is_clean,FALSE);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF TRANS_1746053264(TRANS self, ELT e, AS_CONST_DEF as) {
 AM_ROUT_DEF ret_val;
 AM_ROUT_DEF r;
 AM_LOCAL_EXPR sl;
 AM_SHARED_EXPR g;
 AM_RETURN_STMT ar;
 AM_ROUT_DEF create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROUT_DEF ret_val1;
 AM_ROUT_DEF r1;
 ELT tp_self;
 dTP ret_val2;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val3;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self1;
 AS_TYPE_SPEC ret_val4;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val5;
 AM_LOCAL_EXPR r2;
 AM_FORMAL_ARG create_self3;
 AM_LOCAL_EXPR create_e;
 AM_FORMAL_ARG ret_val6;
 AM_FORMAL_ARG res1;
 ELT name_self;
 IDENT ret_val7 = IDENT_zero;
 AM_SHARED_EXPR create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val8;
 AM_SHARED_EXPR r3;
 ELT name_self1;
 IDENT ret_val9 = IDENT_zero;
 ELT as_tp_self1;
 AS_TYPE_SPEC ret_val10;
 dAS_CLASS_ELT clelt1;
 AS_TYPE_SPEC res2 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self5;
 AS_TYPE_SPEC ret_val11;
 ELT ret_self;
 dTP ret_val12;
 GLOBAL_TBL insert_self;
 AM_SHARED_EXPR insert_g;
 FMAPID739086519 m;
 AM_RETURN_STMT create_self6;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_RETURN_STMT ret_val13;
 AM_RETURN_STMT r4;
 AM_ROUT_DEF L1;
 INT L2;
 OB L3;
 INT L4;
 BOOL L5;
 BOOL L61_;
 AS_TYPE_SPEC L7;
 OB L8;
 extern STR INT1;
 AM_LOCAL_EXPR L9;
 OB L10;
 AM_FORMAL_ARG L11;
 OB L12;
 AM_FORMAL_ARG L13;
 GENERATE_AM L15;
 AM_SHARED_EXPR L16;
 OB L17;
 BOOL L18;
 BOOL L191_;
 AS_TYPE_SPEC L20;
 OB L21;
 extern STR INT1;
 GENERATE_AM L22;
 AM_RETURN_STMT L23;
 OB L24;
 TRANS_74748323(self, ATTR(as,tp));
 create_self = ((AM_ROUT_DEF) NULL);
 create_nargs = 1;
 create_source = ATTR(as,source1);
 L2 = create_nargs;
 L4=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L2)*sizeof(AM_FORMAL_ARG);
 L3=ZALLOC_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((AM_ROUT_DEF) L3);
 L1->asize = L2;
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(self,cur_rout,r);
 SATTR(r,srcsig,ATTR(e,srcsig));
 SATTR(r,sig1,ATTR(e,sig1));
 SATTR(ATTR(r,sig1),srcsig,ATTR(e,srcsig));
 create_self2 = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(as,source1);
 create_name = IDENT_985337352;
 tp_self = e;
 ret_val2 = ATTR(ATTR(tp_self,sig1),tp);
 create_tp = ret_val2;
 as_tp_self = e;
 clelt = ATTR(as_tp_self,as);
 switch (TAG(clelt)) {
  case AS_CONST_DEF_tag:
   L5 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
   L61_=!(L5); 
   if (L61_) {
    res = ATTR(((AS_CONST_DEF) clelt),tp);
   }
   else {
    create_self1 = ((AS_TYPE_SPEC) NULL);
    L8=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
    if (L8==NULL) FATAL("Unable to allocate more memory");
    ((OB)L8)->header.tag=AS_TYPE_SPEC_tag;
    L7 = ((AS_TYPE_SPEC) L8);
    ret_val4 = L7;
    res = ret_val4;
    SATTR(res,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
   } break;
  case AS_SHARED_DEF_tag:
   res = ATTR(((AS_SHARED_DEF) clelt),tp); break;
  case AS_ATTR_DEF_tag:
   res = ATTR(((AS_ATTR_DEF) clelt),tp); break;
  default: ;
 }
 ret_val3 = res;
 create_as_tp = ret_val3;
 L10=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_LOCAL_EXPR_tag;
 L9 = ((AM_LOCAL_EXPR) L10);
 r2 = L9;
 SATTR(r2,source1,create_src);
 SATTR(r2,name1,create_name);
 SATTR(r2,tp_at,create_tp);
 SATTR(r2,as_type,create_as_tp);
 ret_val5 = r2;
 sl = ret_val5;
 create_self3 = ((AM_FORMAL_ARG) NULL);
 create_e = sl;
 L12=ZALLOC(sizeof(struct AM_FORMAL_ARG_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_FORMAL_ARG_tag;
 L11 = ((AM_FORMAL_ARG) L12);
 res1 = L11;
 SATTR(res1,expr,create_e);
 SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
 ret_val6 = res1;
 L13 = ret_val6;
 SARR((AM_ROUT_DEF)r,0,(AM_FORMAL_ARG)L13); 
 ;
 L15=TRANS_1867726633(self);
 name_self = e;
 ret_val7 = ATTR(ATTR(name_self,sig1),name1);
 g = GLOBAL499677090(ATTR(L15,global_tbl), ret_val7, ATTR(ATTR(self,impl),tp));
 if ((g==((AM_SHARED_EXPR) NULL))) {
  create_self4 = ((AM_SHARED_EXPR) NULL);
  create_source1 = ATTR(as,source1);
  L17=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=AM_SHARED_EXPR_tag;
  L16 = ((AM_SHARED_EXPR) L17);
  r3 = L16;
  SATTR(r3,source1,create_source1);
  ret_val8 = r3;
  g = ret_val8;
  name_self1 = e;
  ret_val9 = ATTR(ATTR(name_self1,sig1),name1);
  SATTR(g,name1,ret_val9);
  as_tp_self1 = e;
  clelt1 = ATTR(as_tp_self1,as);
  switch (TAG(clelt1)) {
   case AS_CONST_DEF_tag:
    L18 = (ATTR(((AS_CONST_DEF) clelt1),tp)==((AS_TYPE_SPEC) NULL));
    L191_=!(L18); 
    if (L191_) {
     res2 = ATTR(((AS_CONST_DEF) clelt1),tp);
    }
    else {
     create_self5 = ((AS_TYPE_SPEC) NULL);
     L21=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
     if (L21==NULL) FATAL("Unable to allocate more memory");
     ((OB)L21)->header.tag=AS_TYPE_SPEC_tag;
     L20 = ((AS_TYPE_SPEC) L21);
     ret_val11 = L20;
     res2 = ret_val11;
     SATTR(res2,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
    } break;
   case AS_SHARED_DEF_tag:
    res2 = ATTR(((AS_SHARED_DEF) clelt1),tp); break;
   case AS_ATTR_DEF_tag:
    res2 = ATTR(((AS_ATTR_DEF) clelt1),tp); break;
   default: ;
  }
  ret_val10 = res2;
  SATTR(g,as_type,ret_val10);
  ret_self = e;
  ret_val12 = ATTR(ATTR(ret_self,sig1),ret);
  SATTR(g,tp_at,ret_val12);
  SATTR(g,class_tp,ATTR(ATTR(self,impl),tp));
  SATTR(g,is_const,TRUE);
  SATTR(self,in_constant,TRUE);
  SATTR(g,init,TRANS_693526804(self, ATTR(as,init), ATTR(g,tp_at)));
  SATTR(self,in_constant,FALSE);
  L22=TRANS_1867726633(self);
  insert_self = ATTR(L22,global_tbl);
  insert_g = g;
  m = FMAPdT183078507(ATTR(insert_self,tbl), ATTR(insert_g,class_tp));
  m = FMAPID1512695779(m, ATTR(insert_g,name1), insert_g);
  SATTR(insert_self,tbl,FMAPdT1221772946(ATTR(insert_self,tbl), ATTR(insert_g,class_tp), m));
  SATTR(insert_self,top_sort,FLISTA327355535(ATTR(insert_self,top_sort), insert_g));
 }
 create_self6 = ((AM_RETURN_STMT) NULL);
 create_source2 = ATTR(as,source1);
 L24=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=AM_RETURN_STMT_tag;
 L23 = ((AM_RETURN_STMT) L24);
 r4 = L23;
 SATTR(r4,source1,create_source2);
 ret_val13 = r4;
 ar = ret_val13;
 SATTR(ar,val1,((dAM_EXPR) g));
 SATTR(r,code1,((dAM_STMT) ar));
 SATTR(r,is_clean,TRUE);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_CALL_EXPR TRANS_1849744987(TRANS self, IDENT local11, AM_LOCAL_EXPR helper1, SFILE_ID source1) {
 AS_CALL_EXPR ret_val;
 FLISTIDENT names1;
 AS_CALL_EXPR as = ((AS_CALL_EXPR) NULL);
 FLISTIDENT L11;
 IDENT name111 = IDENT_zero;
 AS_CALL_EXPR as_new;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val2;
 IDENT L2;
 AS_CALL_EXPR L3;
 OB L4;
 AS_CALL_EXPR L5;
 OB L6;
 names1 = TRANS_1609601492(self, local11, helper1);
 {
  struct FLISTI1709766432_frame_struct other1_0;
FLISTI1709766432_frame noname1 = &other1_0;
  L11 = names1;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTI1709766432(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   name111 = L2;
   if ((as==((AS_CALL_EXPR) NULL))) {
    create_self = ((AS_CALL_EXPR) NULL);
    L4=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    ((OB)L4)->header.tag=AS_CALL_EXPR_tag;
    L3 = ((AS_CALL_EXPR) L4);
    ret_val1 = L3;
    as = ret_val1;
    SATTR(as,source1,source1);
   }
   else {
    create_self1 = ((AS_CALL_EXPR) NULL);
    L6=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L6==NULL) FATAL("Unable to allocate more memory");
    ((OB)L6)->header.tag=AS_CALL_EXPR_tag;
    L5 = ((AS_CALL_EXPR) L6);
    ret_val2 = L5;
    as_new = ret_val2;
    SATTR(as_new,source1,source1);
    SATTR(as_new,ob,((dAS_EXPR) as));
    as = as_new;
   }
   SATTR(as,name1,name111);
  }
 }
 after_loop: ;
 ret_val = as;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_157729662(TRANS self, IDENT local11, AM_LOCAL_EXPR helper1) {
 BOOL ret_val = BOOL_zero;
 FLISTIDENT names1;
 FLISTIDENT aget_self;
 INT aget_ind = INT_zero;
 IDENT ret_val1 = IDENT_zero;
 FLISTIDENT aget_self1;
 INT aget_ind1 = INT_zero;
 IDENT ret_val2 = IDENT_zero;
 BOOL L1;
 INT L2;
 BOOL L31_;
 BOOL L4;
 INT L5;
 INT L61_;
 IDENT L71_;
 IDENT L8;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 INT L13;
 INT L141_;
 IDENT L151_;
 IDENT L16;
 STR L17;
 extern STR S_pS_par_ob;
 names1 = TRANS_1609601492(self, local11, helper1);
 if ((names1==((FLISTIDENT) NULL))) {
  L1 = TRUE;
 } else {
  L2 = FLISTI1328284422(names1);
  L31_=(L2)<(2); 
  L1 = L31_;
 }
 if (L1) {
  ret_val = FALSE;
  return ret_val;
 }
 aget_self = names1;
 L5 = FLISTI1328284422(names1);
 L61_=INTMINUS(L5,2); 
 aget_ind = L61_;
 L71_=ARR((FLISTIDENT)aget_self,aget_ind); 
 ret_val1 = L71_;
 L8=ret_val1;
 L9 = STR_sizerINT(L8.str);
 L101_=(L9)<(10); 
 L11 = L101_;
 L121_=!(L11); 
 if (L121_) {
  aget_self1 = names1;
  L13 = FLISTI1328284422(names1);
  L141_=INTMINUS(L13,2); 
  aget_ind1 = L141_;
  L151_=ARR((FLISTIDENT)aget_self1,aget_ind1); 
  ret_val2 = L151_;
  L16=ret_val2;
  L17 = STR_head_INTrSTR(L16.str, 10);
  L4 = STR_is111530248(L17, ((STR) &S_pS_par_ob));
 } else {
  L4 = FALSE;
 }
 ret_val = L4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_2091440962(TRANS self) {
 BOOL ret_val = BOOL_zero;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L5;
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(psather_self,distributed);
 }
 ret_val1 = L1;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = ATTR(self,is_in_par);
  return ret_val;
 }
 else {
  if (ATTR(ATTR(self,cur_rout),is_fork_routine)) {
   L5 = TRUE;
  } else {
   L5 = ATTR(ATTR(self,cur_rout),is_par_routine);
  }
  if (L5) {
   L4 = TRUE;
  } else {
   L4 = ATTR(self,in_simple_par);
  }
  ret_val = L4;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTIDENT TRANS_1609601492(TRANS self, IDENT local11, AM_LOCAL_EXPR helper1) {
 FLISTIDENT ret_val;
 FLISTIDENT names1;
 dTP tp;
 ELT_TBL elts1;
 BOOL found1 = BOOL_zero;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT elt1;
 ELT_TBL L31;
 ELT elt2;
 FLISTIDENT create_self;
 FLISTIDENT ret_val1;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val2;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT name_self;
 IDENT ret_val4 = IDENT_zero;
 ELT name_self1;
 IDENT ret_val5 = IDENT_zero;
 ELT ret_self;
 dTP ret_val6;
 ELT ret_self1;
 dTP ret_val7;
 ELT name_self2;
 IDENT ret_val8 = IDENT_zero;
 dTP L4;
 IMPL L5;
 ELT L6;
 ELT L7;
 BOOL L8;
 IDENT L9;
 INT L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 IDENT L15;
 STR L16;
 extern STR S_pS_par;
 BOOL L17;
 BOOL L181_;
 BOOL L191_;
 create_self = ((FLISTIDENT) NULL);
 ret_val1 = ((FLISTIDENT) NULL);
 names1 = ret_val1;
 tp_self = helper1;
 ret_val2 = ATTR(tp_self,tp_at);
 tp = ret_val2;
 names1 = FLISTI370551816(names1, ATTR(helper1,name1));
 is_eq_self = ATTR(helper1,name1);
 is_eq_i = local11;
 ret_val3 = (is_eq_self.str==is_eq_i.str);
 if (ret_val3) {
  ret_val = names1;
  return ret_val;
 }
 while (1) {
  L4 = tp;
  L5=(*dTP_implrIMPL[TAG(L4)])(L4);
  elts1 = ATTR(L5,elts);
  found1 = FALSE;
  {
   struct ELT_TB431570775_frame_struct other2_0;
ELT_TB431570775_frame noname1 = &other2_0;
   L11 = elts1;
   L21 = local11;
   noname1->self = L11;
   noname1->arg1 = L21;
   noname1->state = 0;
   while (1) {
    L6 = ELT_TB431570775(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    elt1 = L6;
    found1 = TRUE;
    names1 = FLISTI370551816(names1, local11);
    goto after_loop1;
   }
  }
  after_loop1: ;
  if (found1) {
   goto after_loop;
  }
  found1 = FALSE;
  {
   struct ELT_TBL_eltbrELT_frame_struct other3_0;
ELT_TBL_eltbrELT_frame noname2 = &other3_0;
   L31 = elts1;
   noname2->self = L31;
   noname2->state = 0;
   while (1) {
    L7 = ELT_TBL_eltbrELT(noname2);
    if (noname2->state == -1) {
     goto after_loop2;
    }
    elt2 = L7;
    name_self = elt2;
    ret_val4 = ATTR(ATTR(name_self,sig1),name1);
    L9=ret_val4;
    L10 = STR_sizerINT(L9.str);
    L121_=(L10)<(7); 
    L13 = L121_;
    L141_=!(L13); 
    if (L141_) {
     name_self1 = elt2;
     ret_val5 = ATTR(ATTR(name_self1,sig1),name1);
     L15=ret_val5;
     L16 = STR_head_INTrSTR(L15.str, 7);
     L8 = STR_is111530248(L16, ((STR) &S_pS_par));
    } else {
     L8 = FALSE;
    }
    if (L8) {
     ret_self = elt2;
     ret_val6 = ATTR(ATTR(ret_self,sig1),ret);
     L17 = (ret_val6==((dTP) NULL));
     L181_=!(L17); 
     if (L181_) {
      found1 = TRUE;
      ret_self1 = elt2;
      ret_val7 = ATTR(ATTR(ret_self1,sig1),ret);
      tp = ret_val7;
      name_self2 = elt2;
      ret_val8 = ATTR(ATTR(name_self2,sig1),name1);
      names1 = FLISTI370551816(names1, ret_val8);
      goto after_loop2;
     }
    }
   }
  }
  after_loop2: ;
  L191_=!(found1); 
  if (L191_) {
   ret_val = ((FLISTIDENT) NULL);
   return ret_val;
  }
 }
 after_loop: ;
 ret_val = names1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1164784471(TRANS self, AS_ANY_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_ANY_EXPR r;
 TRANS any_context_err_;
 AS_ANY_EXPR any_context_err_1;
 dTP any_context_err_2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 AM_ANY_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_ANY_EXPR ret_val5;
 AM_ANY_EXPR r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR anyINT40680072;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_ANY_EXPR L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_int, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   any_context_err_ = self;
   any_context_err_1 = e;
   any_context_err_2 = tp;
   err_loc_self = any_context_err_;
   err_loc_t = ((dPROG_ERR) any_context_err_1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = any_context_err_;
   plus_self = ((STR) &anyINT40680072);
   L5 = any_context_err_2;
   plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &name18);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   if ((err_self==((TRANS) NULL))) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L7) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L6) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s = ret_val4;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 create_self = ((AM_ANY_EXPR) NULL);
 create_src = ATTR(e,source1);
 L9=ZALLOC_LEAF(sizeof(struct AM_ANY_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 memset(L9,0,sizeof(struct AM_ANY_EXPR_struct));
 ((OB)L9)->header.tag=AM_ANY_EXPR_tag;
 L8 = ((AM_ANY_EXPR) L9);
 r1 = L8;
 SATTR(r1,source1,create_src);
 ret_val5 = r1;
 r = ret_val5;
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1230152132(TRANS self, AS_GLOBAL_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 TRANS global_context_e;
 AS_GLOBAL_EXPR global_context_e1;
 dTP global_context_e2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 AM_GLOBAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_GLOBAL_EXPR ret_val5;
 AM_GLOBAL_EXPR r;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR global307478914;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_GLOBAL_EXPR L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_zone, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   global_context_e = self;
   global_context_e1 = e;
   global_context_e2 = tp;
   err_loc_self = global_context_e;
   err_loc_t = ((dPROG_ERR) global_context_e1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = global_context_e;
   plus_self = ((STR) &global307478914);
   L5 = global_context_e2;
   plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &name18);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   if ((err_self==((TRANS) NULL))) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L7) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L6) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s = ret_val4;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 create_self = ((AM_GLOBAL_EXPR) NULL);
 create_src = ATTR(e,source1);
 L9=ZALLOC_LEAF(sizeof(struct AM_GLOBAL_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 memset(L9,0,sizeof(struct AM_GLOBAL_EXPR_struct));
 ((OB)L9)->header.tag=AM_GLOBAL_EXPR_tag;
 L8 = ((AM_GLOBAL_EXPR) L9);
 r = L8;
 SATTR(r,source1,create_src);
 ret_val5 = r;
 ret_val = ((dAM_EXPR) ret_val5);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1502802970(TRANS self, AS_HERE_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_HERE_EXPR r;
 TRANS here_context_zon;
 AS_HERE_EXPR here_context_zon1;
 dTP here_context_zon2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS here_context_int;
 AS_HERE_EXPR here_context_int1;
 dTP here_context_int2;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 AM_HERE_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 BOOL create_zones = BOOL_zero;
 AM_HERE_EXPR ret_val9;
 AM_HERE_EXPR r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR hereZO804547672;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 BOOL L8;
 BOOL L91_;
 extern STR hereIN194332449;
 dTP L10;
 extern STR name18;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 AM_HERE_EXPR L13;
 OB L14;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(ATTR(self,prog),zones)) {
   L3 = TP_CLA513015767(TP_BUILTIN_zone, ((dCALL_TP) tp));
   L41_=!(L3); 
   if (L41_) {
    here_context_zon = self;
    here_context_zon1 = e;
    here_context_zon2 = tp;
    err_loc_self = here_context_zon;
    err_loc_t = ((dPROG_ERR) here_context_zon1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = here_context_zon;
    plus_self = ((STR) &hereZO804547672);
    L5 = here_context_zon2;
    plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = ((STR) &name18);
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val2;
    if ((err_self==((TRANS) NULL))) {
     L7 = TRUE;
    } else {
     L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L7) {
     L6 = TRUE;
    } else {
     L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
    }
    if (L6) {
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    else {
     plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
     plus_sarg2 = ((STR) &name21);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val3;
     plus_sarg3 = err_s;
     ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
     err_s = ret_val4;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
  else {
   L8 = TP_CLA513015767(TP_BUILTIN_int, ((dCALL_TP) tp));
   L91_=!(L8); 
   if (L91_) {
    here_context_int = self;
    here_context_int1 = e;
    here_context_int2 = tp;
    err_loc_self1 = here_context_int;
    err_loc_t1 = ((dPROG_ERR) here_context_int1);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self1 = here_context_int;
    plus_self4 = ((STR) &hereIN194332449);
    L10 = here_context_int2;
    plus_sarg4 = (*dTP_strrSTR[TAG(L10)])(L10);
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val5;
    plus_sarg5 = ((STR) &name18);
    ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s1 = ret_val6;
    if ((err_self1==((TRANS) NULL))) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L12) {
     L11 = TRUE;
    } else {
     L11 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L11) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val7;
     plus_sarg7 = err_s1;
     ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
     err_s1 = ret_val8;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
 }
 create_self = ((AM_HERE_EXPR) NULL);
 create_src = ATTR(e,source1);
 create_zones = ATTR(ATTR(self,prog),zones);
 L14=ZALLOC(sizeof(struct AM_HERE_EXPR_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_HERE_EXPR_tag;
 L13 = ((AM_HERE_EXPR) L14);
 r1 = L13;
 SATTR(r1,source1,create_src);
 if (create_zones) {
  SATTR(r1,tp,((dTP) TP_BUILTIN_zone));
 }
 else {
  SATTR(r1,tp,((dTP) TP_BUILTIN_int));
 }
 ret_val9 = r1;
 r = ret_val9;
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1703484959(TRANS self, AS_WHERE_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_WHERE_EXPR r;
 TRANS where_context_zo;
 AS_WHERE_EXPR where_context_zo1;
 dTP where_context_zo2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS where_context_in;
 AS_WHERE_EXPR where_context_in1;
 dTP where_context_in2;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 AM_WHERE_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 BOOL create_zones = BOOL_zero;
 AM_WHERE_EXPR ret_val9;
 AM_WHERE_EXPR r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR whereZ1301005922;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 BOOL L8;
 BOOL L91_;
 extern STR whereI1277205312;
 dTP L10;
 extern STR name18;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 AM_WHERE_EXPR L13;
 OB L14;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(ATTR(self,prog),zones)) {
   L3 = TP_CLA513015767(TP_BUILTIN_zone, ((dCALL_TP) tp));
   L41_=!(L3); 
   if (L41_) {
    where_context_zo = self;
    where_context_zo1 = e;
    where_context_zo2 = tp;
    err_loc_self = where_context_zo;
    err_loc_t = ((dPROG_ERR) where_context_zo1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = where_context_zo;
    plus_self = ((STR) &whereZ1301005922);
    L5 = where_context_zo2;
    plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = ((STR) &name18);
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val2;
    if ((err_self==((TRANS) NULL))) {
     L7 = TRUE;
    } else {
     L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L7) {
     L6 = TRUE;
    } else {
     L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
    }
    if (L6) {
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    else {
     plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
     plus_sarg2 = ((STR) &name21);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val3;
     plus_sarg3 = err_s;
     ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
     err_s = ret_val4;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
  else {
   L8 = TP_CLA513015767(TP_BUILTIN_int, ((dCALL_TP) tp));
   L91_=!(L8); 
   if (L91_) {
    where_context_in = self;
    where_context_in1 = e;
    where_context_in2 = tp;
    err_loc_self1 = where_context_in;
    err_loc_t1 = ((dPROG_ERR) where_context_in1);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self1 = where_context_in;
    plus_self4 = ((STR) &whereI1277205312);
    L10 = where_context_in2;
    plus_sarg4 = (*dTP_strrSTR[TAG(L10)])(L10);
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val5;
    plus_sarg5 = ((STR) &name18);
    ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s1 = ret_val6;
    if ((err_self1==((TRANS) NULL))) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L12) {
     L11 = TRUE;
    } else {
     L11 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L11) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val7;
     plus_sarg7 = err_s1;
     ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
     err_s1 = ret_val8;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
 }
 create_self = ((AM_WHERE_EXPR) NULL);
 create_src = ATTR(e,source1);
 create_zones = ATTR(ATTR(self,prog),zones);
 L14=ZALLOC(sizeof(struct AM_WHERE_EXPR_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_WHERE_EXPR_tag;
 L13 = ((AM_WHERE_EXPR) L14);
 r1 = L13;
 SATTR(r1,source1,create_src);
 if (create_zones) {
  SATTR(r1,tp,((dTP) TP_BUILTIN_zone));
 }
 else {
  SATTR(r1,tp,((dTP) TP_BUILTIN_int));
 }
 ret_val9 = r1;
 r = ret_val9;
 SATTR(r,arg,TRANS_693526804(self, ATTR(e,e), ((dTP) TP_BUI1325635093)));
 if ((ATTR(r,arg)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1761370920(TRANS self, AS_AT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_AT_EXPR r;
 AM_AT_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_AT_EXPR ret_val1;
 AM_AT_EXPR r1;
 AM_AT_EXPR L1;
 OB L2;
 create_self = ((AM_AT_EXPR) NULL);
 create_src = ATTR(e,source1);
 L2=ZALLOC(sizeof(struct AM_AT_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_AT_EXPR_tag;
 L1 = ((AM_AT_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,e,TRANS_693526804(self, ATTR(e,e), tp));
 if ((ATTR(r,e)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if (ATTR(ATTR(self,prog),zones)) {
  SATTR(r,at,TRANS_693526804(self, ATTR(e,at), ((dTP) TP_BUILTIN_zone)));
 }
 else {
  SATTR(r,at,TRANS_693526804(self, ATTR(e,at), ((dTP) TP_BUILTIN_int)));
 }
 if ((ATTR(r,at)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_266129750(TRANS self, AS_NEAR_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_NEAR_EXPR r;
 TRANS near_context_err;
 AS_NEAR_EXPR near_context_err1;
 dTP near_context_err2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 AM_NEAR_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_NEAR_EXPR ret_val5;
 AM_NEAR_EXPR r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR nearIN473408029;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_NEAR_EXPR L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   near_context_err = self;
   near_context_err1 = e;
   near_context_err2 = tp;
   err_loc_self = near_context_err;
   err_loc_t = ((dPROG_ERR) near_context_err1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = near_context_err;
   plus_self = ((STR) &nearIN473408029);
   L5 = near_context_err2;
   plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &name18);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   if ((err_self==((TRANS) NULL))) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L7) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L6) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s = ret_val4;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 create_self = ((AM_NEAR_EXPR) NULL);
 create_src = ATTR(e,source1);
 L9=ZALLOC(sizeof(struct AM_NEAR_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_NEAR_EXPR_tag;
 L8 = ((AM_NEAR_EXPR) L9);
 r1 = L8;
 SATTR(r1,source1,create_src);
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,arg,TRANS_693526804(self, ATTR(e,e), ((dTP) TP_BUI1325635093)));
 if ((ATTR(r,arg)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_31851033(TRANS self, AS_CLUSTER_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AS_INT_LIT_EXPR asint;
 AM_INT_CONST r;
 AM_CLUSTER_EXPR r1;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 AS_INT_LIT_EXPR create_self;
 AS_INT_LIT_EXPR ret_val3;
 AM_INT_CONST create_self1;
 AS_INT_LIT_EXPR create_t;
 AM_INT_CONST ret_val4;
 AM_INT_CONST r2;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 TRANS cluster_context_;
 AS_CLUSTER_EXPR cluster_context_1;
 dTP cluster_context_2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 AM_CLUSTER_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_CLUSTER_EXPR ret_val11;
 AM_CLUSTER_EXPR r3;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 dTP L5;
 extern STR builti1654399671;
 AS_INT_LIT_EXPR L6;
 OB L7;
 AM_INT_CONST L8;
 OB L9;
 extern STR cluste1354883291;
 BOOL L10;
 BOOL L11;
 extern STR name21;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR cluste1966397776;
 dTP L16;
 extern STR name18;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 AM_CLUSTER_EXPR L19;
 OB L20;
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(psather_self,distributed);
 }
 ret_val1 = L1;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  L5 = ATTR(ATTR(ATTR(self,cur_rout),sig1),tp);
  if ((*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUILTIN_sys))) {
   is_eq_self = ATTR(ATTR(ATTR(self,cur_rout),sig1),name1);
   is_eq_i = IDENT_1150413730(IDENT_zero, ((STR) &builti1654399671));
   ret_val2 = (is_eq_self.str==is_eq_i.str);
   L4 = ret_val2;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   create_self = ((AS_INT_LIT_EXPR) NULL);
   L7=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
   if (L7==NULL) FATAL("Unable to allocate more memory");
   ((OB)L7)->header.tag=AS_INT_LIT_EXPR_tag;
   L6 = ((AS_INT_LIT_EXPR) L7);
   ret_val3 = L6;
   asint = ret_val3;
   SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), 1));
   create_self1 = ((AM_INT_CONST) NULL);
   create_t = asint;
   L9=ZALLOC(sizeof(struct AM_INT_CONST_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=AM_INT_CONST_tag;
   L8 = ((AM_INT_CONST) L9);
   r2 = L8;
   SATTR(r2,source1,ATTR(create_t,source1));
   SATTR(r2,val1,ATTR(create_t,val1));
   ret_val4 = r2;
   r = ret_val4;
   ret_val = ((dAM_EXPR) r);
   return ret_val;
  }
  else {
   err_self = self;
   err_s = ((STR) &cluste1354883291);
   if ((err_self==((TRANS) NULL))) {
    L11 = TRUE;
   } else {
    L11 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L11) {
    L10 = TRUE;
   } else {
    L10 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L10) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val5;
    plus_sarg1 = err_s;
    ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val6;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 else {
  L12 = (tp==((dTP) NULL));
  L131_=!(L12); 
  if (L131_) {
   L14 = TP_CLA513015767(TP_BUILTIN_int, ((dCALL_TP) tp));
   L151_=!(L14); 
   if (L151_) {
    cluster_context_ = self;
    cluster_context_1 = e;
    cluster_context_2 = tp;
    err_loc_self = cluster_context_;
    err_loc_t = ((dPROG_ERR) cluster_context_1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self1 = cluster_context_;
    plus_self2 = ((STR) &cluste1966397776);
    L16 = cluster_context_2;
    plus_sarg2 = (*dTP_strrSTR[TAG(L16)])(L16);
    ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val7;
    plus_sarg3 = ((STR) &name18);
    ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val8;
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
     plus_self4 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val9;
     plus_sarg5 = err_s1;
     ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s1 = ret_val10;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
  create_self2 = ((AM_CLUSTER_EXPR) NULL);
  create_src = ATTR(e,source1);
  L20=ZALLOC_LEAF(sizeof(struct AM_CLUSTER_EXPR_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  memset(L20,0,sizeof(struct AM_CLUSTER_EXPR_struct));
  ((OB)L20)->header.tag=AM_CLUSTER_EXPR_tag;
  L19 = ((AM_CLUSTER_EXPR) L20);
  r3 = L19;
  SATTR(r3,source1,create_src);
  ret_val11 = r3;
  r1 = ret_val11;
  ret_val = ((dAM_EXPR) r1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_637585(TRANS self, AS_FAR_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_FAR_EXPR r;
 TRANS far_context_err_;
 AS_FAR_EXPR far_context_err_1;
 dTP far_context_err_2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 AM_FAR_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_FAR_EXPR ret_val5;
 AM_FAR_EXPR r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR farINT1183426989;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_FAR_EXPR L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   far_context_err_ = self;
   far_context_err_1 = e;
   far_context_err_2 = tp;
   err_loc_self = far_context_err_;
   err_loc_t = ((dPROG_ERR) far_context_err_1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = far_context_err_;
   plus_self = ((STR) &farINT1183426989);
   L5 = far_context_err_2;
   plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &name18);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val2;
   if ((err_self==((TRANS) NULL))) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L7) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L6) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s = ret_val4;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 create_self = ((AM_FAR_EXPR) NULL);
 create_src = ATTR(e,source1);
 L9=ZALLOC(sizeof(struct AM_FAR_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_FAR_EXPR_tag;
 L8 = ((AM_FAR_EXPR) L9);
 r1 = L8;
 SATTR(r1,source1,create_src);
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,arg,TRANS_693526804(self, ATTR(e,e), ((dTP) TP_BUI1325635093)));
 if ((ATTR(r,arg)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_693526804(TRANS self, dAS_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dAM_EXPR r = ((dAM_EXPR) NULL);
 TRANS transform_break_;
 AS_BREAK_EXPR transform_break_1;
 dTP transform_break_2;
 dAM_EXPR ret_val1;
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
 extern STR breakb816053051;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 if ((e==((dAS_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 switch (TAG(e)) {
  case AS_SELF_EXPR_tag:
   r = TRANS_593837946(self, ((AS_SELF_EXPR) e), tp); break;
  case AS_CALL_EXPR_tag:
   r = TRANS_1705639890(self, ((AS_CALL_EXPR) e), tp, TRUE); break;
  case AS_VOID_EXPR_tag:
   r = TRANS_196236678(self, ((AS_VOID_EXPR) e), tp); break;
  case AS_IS_VOID_EXPR_tag:
   r = TRANS_1168851617(self, ((AS_IS_VOID_EXPR) e), tp); break;
  case AS_ARRAY_EXPR_tag:
   r = TRANS_595522723(self, ((AS_ARRAY_EXPR) e), tp); break;
  case AS_CREATE_EXPR_tag:
   r = TRANS_700113784(self, ((AS_CREATE_EXPR) e), tp); break;
  case AS_BOU14269336_tag:
   r = TRANS_1546520066(self, ((AS_BOU14269336) e), tp); break;
  case AS_AND_EXPR_tag:
   r = TRANS_1006089529(self, ((AS_AND_EXPR) e), tp); break;
  case AS_OR_EXPR_tag:
   r = TRANS_361080840(self, ((AS_OR_EXPR) e), tp); break;
  case AS_EXCEPT_EXPR_tag:
   r = TRANS_1668850900(self, ((AS_EXCEPT_EXPR) e), tp); break;
  case AS_NEW_EXPR_tag:
   r = TRANS_962243143(self, ((AS_NEW_EXPR) e), tp); break;
  case AS_INITIAL_EXPR_tag:
   r = TRANS_1627619307(self, ((AS_INITIAL_EXPR) e), tp); break;
  case AS_BREAK_EXPR_tag:
   transform_break_ = self;
   transform_break_1 = ((AS_BREAK_EXPR) e);
   transform_break_2 = tp;
   err_loc_self = transform_break_;
   err_loc_t = ((dPROG_ERR) transform_break_1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = transform_break_;
   err_s = ((STR) &breakb816053051);
   if ((err_self==((TRANS) NULL))) {
    L2 = TRUE;
   } else {
    L2 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L2) {
    L1 = TRUE;
   } else {
    L1 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L1) {
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
   ret_val1 = ((dAM_EXPR) NULL);
   r = ret_val1; break;
  case AS_RESULT_EXPR_tag:
   r = TRANS_1346628084(self, ((AS_RESULT_EXPR) e), tp); break;
  case AS_BOOL_LIT_EXPR_tag:
   r = TRANS_1783297694(self, ((AS_BOOL_LIT_EXPR) e), tp); break;
  case AS_CHAR_LIT_EXPR_tag:
   r = TRANS_1269501442(self, ((AS_CHAR_LIT_EXPR) e), tp); break;
  case AS_STR_LIT_EXPR_tag:
   r = TRANS_1556853519(self, ((AS_STR_LIT_EXPR) e), tp); break;
  case AS_INT_LIT_EXPR_tag:
   r = TRANS_1173176875(self, ((AS_INT_LIT_EXPR) e), tp); break;
  case AS_FLT_LIT_EXPR_tag:
   r = TRANS_1475346849(self, ((AS_FLT_LIT_EXPR) e), tp); break;
  case AS_CLUSTER_EXPR_tag:
   r = TRANS_31851033(self, ((AS_CLUSTER_EXPR) e), tp); break;
  case AS_CLU553570599_tag:
   r = TRANS_921970742(self, ((AS_CLU553570599) e), tp); break;
  case AS_GLOBAL_EXPR_tag:
   r = TRANS_1230152132(self, ((AS_GLOBAL_EXPR) e), tp); break;
  case AS_AT_EXPR_tag:
   r = TRANS_1761370920(self, ((AS_AT_EXPR) e), tp); break;
  case AS_HERE_EXPR_tag:
   r = TRANS_1502802970(self, ((AS_HERE_EXPR) e), tp); break;
  case AS_WHERE_EXPR_tag:
   r = TRANS_1703484959(self, ((AS_WHERE_EXPR) e), tp); break;
  case AS_ANY_EXPR_tag:
   r = TRANS_1164784471(self, ((AS_ANY_EXPR) e), tp); break;
  case AS_COHORT_EXPR_tag:
   r = TRANS_809530892(self, ((AS_COHORT_EXPR) e), tp); break;
  case AS_NEAR_EXPR_tag:
   r = TRANS_266129750(self, ((AS_NEAR_EXPR) e), tp); break;
  case AS_FAR_EXPR_tag:
   r = TRANS_637585(self, ((AS_FAR_EXPR) e), tp); break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::transform_expr($AS_EXPR,$TP):$AM_EXPR\n./Middle/trans.sa:1522:14");
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_809530892(TRANS self, AS_COHORT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_LOCAL_EXPR r;
 TRANS cohort_outside_p;
 AS_COHORT_EXPR cohort_outside_p1;
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
 TRANS cohort_context_e;
 AS_COHORT_EXPR cohort_context_e1;
 dTP cohort_context_e2;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 TRANS err_self2;
 STR err_s2;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR cohort1947905067;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR cohort228120;
 dTP L10;
 extern STR name18;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 extern STR CompEr1768856194;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 L2 = TRANS_2091440962(self);
 L31_=!(L2); 
 if (L31_) {
  L1 = (ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  cohort_outside_p = self;
  cohort_outside_p1 = e;
  err_loc_self = cohort_outside_p;
  err_loc_t = ((dPROG_ERR) cohort_outside_p1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = cohort_outside_p;
  err_s = ((STR) &cohort1947905067);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L6 = (tp==((dTP) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = TP_CLA513015767(TP_BUI191368816, ((dCALL_TP) tp));
  L91_=!(L8); 
  if (L91_) {
   cohort_context_e = self;
   cohort_context_e1 = e;
   cohort_context_e2 = tp;
   err_loc_self1 = cohort_context_e;
   err_loc_t1 = ((dPROG_ERR) cohort_context_e1);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   err_self1 = cohort_context_e;
   plus_self2 = ((STR) &cohort228120);
   L10 = cohort_context_e2;
   plus_sarg2 = (*dTP_strrSTR[TAG(L10)])(L10);
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = ((STR) &name18);
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s1 = ret_val4;
   if ((err_self1==((TRANS) NULL))) {
    L12 = TRUE;
   } else {
    L12 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L12) {
    L11 = TRUE;
   } else {
    L11 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L11) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val5;
    plus_sarg5 = err_s1;
    ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s1 = ret_val6;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 r = TRANS_1711874080(self, ATTR(ATTR(self,cur_cohort),name1));
 if ((r==((AM_LOCAL_EXPR) NULL))) {
  err_self2 = self;
  err_s2 = ((STR) &CompEr1768856194);
  if ((err_self2==((TRANS) NULL))) {
   L14 = TRUE;
  } else {
   L14 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L14) {
   L13 = TRUE;
  } else {
   L13 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
  }
  if (L13) {
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
  else {
   plus_self6 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
   plus_sarg6 = ((STR) &name21);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = err_s2;
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s2 = ret_val8;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 SATTR(r,source1,ATTR(e,source1));
 SATTR(e,name_after_pSath,ATTR(ATTR(self,cur_cohort),name1));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_921970742(TRANS self, AS_CLU553570599 e, dTP tp) {
 dAM_EXPR ret_val;
 AM_CLU1286269335 r;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 TRANS cluster_size_con;
 AS_CLU553570599 cluster_size_con1;
 dTP cluster_size_con2;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 AM_CLU1286269335 create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_CLU1286269335 ret_val8;
 AM_CLU1286269335 r1;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR cluste448825028;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR cluste1354019123;
 dTP L10;
 extern STR name18;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 AM_CLU1286269335 L13;
 OB L14;
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(psather_self,distributed);
 }
 ret_val1 = L1;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  err_self = self;
  err_s = ((STR) &cluste448825028);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 else {
  L6 = (tp==((dTP) NULL));
  L71_=!(L6); 
  if (L71_) {
   L8 = TP_CLA513015767(TP_BUILTIN_int, ((dCALL_TP) tp));
   L91_=!(L8); 
   if (L91_) {
    cluster_size_con = self;
    cluster_size_con1 = e;
    cluster_size_con2 = tp;
    err_loc_self = cluster_size_con;
    err_loc_t = ((dPROG_ERR) cluster_size_con1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self1 = cluster_size_con;
    plus_self2 = ((STR) &cluste1354019123);
    L10 = cluster_size_con2;
    plus_sarg2 = (*dTP_strrSTR[TAG(L10)])(L10);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val4;
    plus_sarg3 = ((STR) &name18);
    ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val5;
    if ((err_self1==((TRANS) NULL))) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L12) {
     L11 = TRUE;
    } else {
     L11 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L11) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self4 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val6;
     plus_sarg5 = err_s1;
     ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s1 = ret_val7;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
  create_self = ((AM_CLU1286269335) NULL);
  create_src = ATTR(e,source1);
  L14=ZALLOC_LEAF(sizeof(struct AM_CLU1286269335_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  memset(L14,0,sizeof(struct AM_CLU1286269335_struct));
  ((OB)L14)->header.tag=AM_CLU1286269335_tag;
  L13 = ((AM_CLU1286269335) L14);
  r1 = L13;
  SATTR(r1,source1,create_src);
  ret_val8 = r1;
  r = ret_val8;
  ret_val = ((dAM_EXPR) r);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1207936294(TRANS self, AS_CALL_EXPR l, AS_ASSIGN_STMT s) {
 dAM_STMT ret_val;
 AM_EXPR_STMT r;
 dAS_EXPR a;
 AS_ARG_MODE mode1;
 dAS_EXPR lst;
 AS_ARG_MODE modes1;
 dAS_EXPR added_args;
 AS_ARG_MODE added_modes = ((AS_ARG_MODE) NULL);
 AS_ARG_MODE mode2;
 AM_EXPR_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_EXPR_STMT ret_val1;
 AM_EXPR_STMT r1;
 AS_ARG_MODE create_self1;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val2;
 AS_ARG_MODE res;
 AS_ARG_MODE create_self2;
 INT create_m1 = INT_zero;
 AS_ARG_MODE ret_val3;
 AS_ARG_MODE res1;
 AM_EXPR_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 AS_ARG_MODE L5;
 OB L6;
 dAS_EXPR L7;
 dAS_EXPR L8;
 dAS_EXPR L9;
 BOOL L10;
 BOOL L111_;
 AS_ARG_MODE L12;
 OB L13;
 dAS_EXPR L14;
 dAS_EXPR L15;
 dAS_EXPR L16;
 BOOL L17;
 BOOL L181_;
 create_self = ((AM_EXPR_STMT) NULL);
 create_source = ATTR(l,source1);
 L2=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_EXPR_STMT_tag;
 L1 = ((AM_EXPR_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(l,name1,IDENT_667688077);
 SATTR(l,is_array,FALSE);
 if ((ATTR(l,args)==((dAS_EXPR) NULL))) {
  SATTR(l,args,ATTR(s,rhs));
  a = ATTR(l,args);
  while (1) {
   L3 = (a==((dAS_EXPR) NULL));
   L41_=!(L3); 
   if (L41_) {
   }
   else {
    goto after_loop;
   }
   create_self1 = ((AS_ARG_MODE) NULL);
   create_m = AS_ARG1300937332;
   L6=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AS_ARG_MODE_tag;
   L5 = ((AS_ARG_MODE) L6);
   res = L5;
   SATTR(res,mod,create_m);
   ret_val2 = res;
   mode1 = ret_val2;
   if ((ATTR(l,modes1)==((AS_ARG_MODE) NULL))) {
    SATTR(l,modes1,mode1);
   }
   else {
    AS_ARG1328804463(ATTR(l,modes1), mode1);
   }
   L7 = a;
   a = (*dAS_EX337431651[TAG(L7)])(L7);
  }
  after_loop: ;
  SATTR(r,expr,TRANS_1705639890(self, l, ((dTP) NULL), FALSE));
  SATTR(l,args,((dAS_EXPR) NULL));
  SATTR(l,modes1,((AS_ARG_MODE) NULL));
 }
 else {
  lst = ATTR(l,args);
  modes1 = ATTR(l,modes1);
  while (1) {
   L8 = lst;
   if (((*dAS_EX337431651[TAG(L8)])(L8)==((dAS_EXPR) NULL))) {
    goto after_loop1;
   }
   L9 = lst;
   lst = (*dAS_EX337431651[TAG(L9)])(L9);
   modes1 = ATTR(modes1,next);
  }
  after_loop1: ;
  added_args = ATTR(s,rhs);
  while (1) {
   L10 = (added_args==((dAS_EXPR) NULL));
   L111_=!(L10); 
   if (L111_) {
   }
   else {
    goto after_loop2;
   }
   create_self2 = ((AS_ARG_MODE) NULL);
   create_m1 = AS_ARG1300937332;
   L13=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=AS_ARG_MODE_tag;
   L12 = ((AS_ARG_MODE) L13);
   res1 = L12;
   SATTR(res1,mod,create_m1);
   ret_val3 = res1;
   mode2 = ret_val3;
   if ((added_modes==((AS_ARG_MODE) NULL))) {
    added_modes = mode2;
   }
   else {
    AS_ARG1328804463(added_modes, mode2);
   }
   L14 = added_args;
   added_args = (*dAS_EX337431651[TAG(L14)])(L14);
  }
  after_loop2: ;
  L15 = lst;
  (*dAS_EX1809609236[TAG(L15)])(L15, ATTR(s,rhs));
  SATTR(modes1,next,added_modes);
  SATTR(r,expr,TRANS_1705639890(self, l, ((dTP) NULL), FALSE));
  L16 = lst;
  (*dAS_EX1809609236[TAG(L16)])(L16, ((dAS_EXPR) NULL));
 }
 SATTR(l,name1,IDENT_zero);
 SATTR(l,is_array,TRUE);
 L17 = (ATTR(r,expr)==((dAM_EXPR) NULL));
 L181_=!(L17); 
 if (L181_) {
  ret_val = ((dAM_STMT) r);
  return ret_val;
 }
 else {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1573138211(TRANS self, AS_DEC_STMT s) {
 dAM_STMT ret_val;
 AM_LOCAL_EXPR l;
 dTP the_tp;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
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
 STR plus_sarg3;
 STR ret_val4;
 TRANS err_self;
 STR err_s;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val7;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val10;
 AM_LOCAL_EXPR r;
 BOOL L1;
 BOOL L21_;
 extern STR Thislo1121838955;
 extern STR name21;
 dTP L3;
 extern STR whichh1927473166;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 BOOL L6;
 BOOL L7;
 dTP L8;
 INT L9;
 INT L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR Partia2053653834;
 BOOL L16;
 BOOL L17;
 extern STR name21;
 AM_LOCAL_EXPR L18;
 OB L19;
 l = TRANS_1711874080(self, ATTR(s,name1));
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) s);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 L1 = (l==((AM_LOCAL_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  err_self = self;
  plus_self = ((STR) &Thislo1121838955);
  plus_sarg = ATTR(l,name1).str;
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name21);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val2;
  L3 = ATTR(l,tp_at);
  plus_sarg2 = (*dTP_strrSTR[TAG(L3)])(L3);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &whichh1927473166);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  err_s = ret_val4;
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
   plus_self4 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg4 = ((STR) &name21);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = err_s;
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s = ret_val6;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 tp_of_self = self;
 tp_of_t = ATTR(s,tp);
 ret_val7 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
 the_tp = ret_val7;
 L8 = the_tp;
 if ((*dTP_is1553335918[TAG(L8)])(L8)) {
  L9 = ATTR(ATTR(s,tp),kind);
  L10 = AS_TYP1853679690;
  L111_=(L9)==(L10); 
  L12 = L111_;
  L131_=!(L12); 
  L7 = L131_;
 } else {
  L7 = FALSE;
 }
 if (L7) {
  L14 = ATTR(ATTR(self,prog),is_check_am);
  L151_=!(L14); 
  L6 = L151_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  err_self1 = self;
  err_s1 = ((STR) &Partia2053653834);
  if ((err_self1==((TRANS) NULL))) {
   L17 = TRUE;
  } else {
   L17 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L17) {
   L16 = TRUE;
  } else {
   L16 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L16) {
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  else {
   plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
   plus_sarg6 = ((STR) &name21);
   ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val8;
   plus_sarg7 = err_s1;
   ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s1 = ret_val9;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
 }
 TRANS_74748323(self, ATTR(s,tp));
 create_self = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(s,source1);
 create_name = ATTR(s,name1);
 create_tp = the_tp;
 create_as_tp = ATTR(s,tp);
 L19=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=AM_LOCAL_EXPR_tag;
 L18 = ((AM_LOCAL_EXPR) L19);
 r = L18;
 SATTR(r,source1,create_src);
 SATTR(r,name1,create_name);
 SATTR(r,tp_at,create_tp);
 SATTR(r,as_type,create_as_tp);
 ret_val10 = r;
 l = ret_val10;
 SATTR(l,needs_init,TRUE);
 TRANS_626807411(self, l);
 ret_val = ((dAM_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1920518219(TRANS self, AS_ASSIGN_STMT s) {
 dAM_STMT ret_val;
 AM_LOCAL_EXPR l;
 AM_ASSIGN_STMT r = ((AM_ASSIGN_STMT) NULL);
 dAS_EXPR rhs;
 dAM_EXPR rhsam;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
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
 STR plus_sarg3;
 STR ret_val4;
 TRANS err_self;
 STR err_s;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val7;
 AM_LOCAL_EXPR r1;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val8;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val10;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val11;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val12;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val13;
 AM_ASSIGN_STMT r2;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val14;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self3;
 STR err_s3;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val16;
 TRANS err_self4;
 STR err_s4;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val18;
 TRANS tp_of_self1;
 AS_TYPE_SPEC tp_of_t1;
 dTP ret_val19;
 TRANS err_self5;
 STR err_s5;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val20;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val21;
 TRANS err_self6;
 STR err_s6;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val22;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val23;
 AM_ASSIGN_STMT create_self2;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val24;
 AM_ASSIGN_STMT r3;
 PROG psather_self;
 BOOL ret_val25 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 extern STR Thislo1121838955;
 extern STR name21;
 dTP L3;
 extern STR whichh1927473166;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 AM_LOCAL_EXPR L6;
 OB L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L11;
 dTP L12;
 INT L13;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 extern STR Partia2053653834;
 BOOL L20;
 BOOL L22;
 extern STR name21;
 extern STR Compil1546367601;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 AM_ASSIGN_STMT L25;
 OB L26;
 extern STR Therig1406749556;
 BOOL L27;
 BOOL L28;
 extern STR name21;
 extern STR Creati1591834403;
 BOOL L29;
 BOOL L30;
 extern STR name21;
 BOOL L31;
 BOOL L32;
 dTP L33;
 INT L34;
 INT L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 extern STR Partia2053653834;
 BOOL L41;
 BOOL L42;
 extern STR name21;
 extern STR Therig416988915;
 BOOL L43;
 BOOL L44;
 extern STR name21;
 AM_ASSIGN_STMT L45;
 OB L46;
 dAM_EXPR L47;
 BOOL L48;
 if ((s==((AS_ASSIGN_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 l = TRANS_1711874080(self, ATTR(s,name1));
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) s);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 L1 = (l==((AM_LOCAL_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  err_self = self;
  plus_self = ((STR) &Thislo1121838955);
  plus_sarg = ATTR(l,name1).str;
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name21);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val2;
  L3 = ATTR(l,tp_at);
  plus_sarg2 = (*dTP_strrSTR[TAG(L3)])(L3);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &whichh1927473166);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  err_s = ret_val4;
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
   plus_self4 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg4 = ((STR) &name21);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = err_s;
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s = ret_val6;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(s,source1);
 create_name = ATTR(s,name1);
 create_tp = ((dTP) NULL);
 L7=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_LOCAL_EXPR_tag;
 L6 = ((AM_LOCAL_EXPR) L7);
 r1 = L6;
 SATTR(r1,source1,create_src);
 SATTR(r1,name1,create_name);
 SATTR(r1,tp_at,create_tp);
 ret_val7 = r1;
 l = ret_val7;
 if (ATTR(self,in_protect_body)) {
  SATTR(l,is_volatile,TRUE);
 }
 L8 = (ATTR(s,tp)==((AS_TYPE_SPEC) NULL));
 L91_=!(L8); 
 if (L91_) {
  tp_of_self = self;
  tp_of_t = ATTR(s,tp);
  ret_val8 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  SATTR(l,tp_at,ret_val8);
  L12 = ATTR(l,tp_at);
  if ((*dTP_is1553335918[TAG(L12)])(L12)) {
   L13 = ATTR(ATTR(s,tp),kind);
   L14 = AS_TYP1853679690;
   L151_=(L13)==(L14); 
   L16 = L151_;
   L171_=!(L16); 
   L11 = L171_;
  } else {
   L11 = FALSE;
  }
  if (L11) {
   L18 = ATTR(ATTR(self,prog),is_check_am);
   L191_=!(L18); 
   L10 = L191_;
  } else {
   L10 = FALSE;
  }
  if (L10) {
   err_self1 = self;
   err_s1 = ((STR) &Partia2053653834);
   if ((err_self1==((TRANS) NULL))) {
    L22 = TRUE;
   } else {
    L22 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L22) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L20) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg6 = ((STR) &name21);
    ret_val9 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val9;
    plus_sarg7 = err_s1;
    ret_val10 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s1 = ret_val10;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  TRANS_74748323(self, ATTR(s,tp));
  SATTR(l,as_type,ATTR(s,tp));
  if ((ATTR(l,tp_at)==((dTP) NULL))) {
   err_loc_self1 = self;
   err_loc_t1 = ((dPROG_ERR) s);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   err_self2 = self;
   err_s2 = ((STR) &Compil1546367601);
   if ((err_self2==((TRANS) NULL))) {
    L24 = TRUE;
   } else {
    L24 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L24) {
    L23 = TRUE;
   } else {
    L23 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L23) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val11 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val11;
    plus_sarg9 = err_s2;
    ret_val12 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s2 = ret_val12;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  TRANS_626807411(self, l);
  create_self1 = ((AM_ASSIGN_STMT) NULL);
  create_source = ATTR(s,source1);
  L26=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=AM_ASSIGN_STMT_tag;
  L25 = ((AM_ASSIGN_STMT) L26);
  r2 = L25;
  SATTR(r2,source1,create_source);
  ret_val13 = r2;
  r = ret_val13;
  SATTR(r,dest,((dAM_EXPR) l));
  tp_self = l;
  ret_val14 = ATTR(tp_self,tp_at);
  SATTR(r,src1,TRANS_693526804(self, ATTR(s,rhs), ret_val14));
  if ((ATTR(r,src1)==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  ret_val = ((dAM_STMT) r);
  return ret_val;
 }
 rhs = ATTR(s,rhs);
 err_loc_self2 = self;
 err_loc_t2 = ((dPROG_ERR) ATTR(s,rhs));
 PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
 if (rhs==NULL) {
  goto other425;
 } else
 switch (TAG(rhs)) {
  case AS_VOID_EXPR_tag:
   err_self3 = self;
   err_s3 = ((STR) &Therig1406749556);
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
    plus_self10 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val15 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val15;
    plus_sarg11 = err_s3;
    ret_val16 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s3 = ret_val16;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val; break;
  case AS_CREATE_EXPR_tag:
   if ((ATTR(((AS_CREATE_EXPR) rhs),tp)==((AS_TYPE_SPEC) NULL))) {
    err_self4 = self;
    err_s4 = ((STR) &Creati1591834403);
    if ((err_self4==((TRANS) NULL))) {
     L30 = TRUE;
    } else {
     L30 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L30) {
     L29 = TRUE;
    } else {
     L29 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
    }
    if (L29) {
     PROG_err_STR(ATTR(err_self4,prog), err_s4);
    }
    else {
     plus_self12 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
     plus_sarg12 = ((STR) &name21);
     ret_val17 = STR_ap2004550586(plus_self12, plus_sarg12);
     plus_self13 = ret_val17;
     plus_sarg13 = err_s4;
     ret_val18 = STR_ap2004550586(plus_self13, plus_sarg13);
     err_s4 = ret_val18;
     PROG_err_STR(ATTR(err_self4,prog), err_s4);
    }
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   }
   tp_of_self1 = self;
   tp_of_t1 = ATTR(((AS_CREATE_EXPR) rhs),tp);
   ret_val19 = TP_CON1800432689(ATTR(tp_of_self1,tp_con), tp_of_t1);
   L33 = ret_val19;
   if ((*dTP_is1553335918[TAG(L33)])(L33)) {
    L34 = ATTR(ATTR(((AS_CREATE_EXPR) rhs),tp),kind);
    L35 = AS_TYP1853679690;
    L361_=(L34)==(L35); 
    L37 = L361_;
    L381_=!(L37); 
    L32 = L381_;
   } else {
    L32 = FALSE;
   }
   if (L32) {
    L39 = ATTR(ATTR(self,prog),is_check_am);
    L401_=!(L39); 
    L31 = L401_;
   } else {
    L31 = FALSE;
   }
   if (L31) {
    err_self5 = self;
    err_s5 = ((STR) &Partia2053653834);
    if ((err_self5==((TRANS) NULL))) {
     L42 = TRUE;
    } else {
     L42 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L42) {
     L41 = TRUE;
    } else {
     L41 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
    }
    if (L41) {
     PROG_err_STR(ATTR(err_self5,prog), err_s5);
    }
    else {
     plus_self14 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
     plus_sarg14 = ((STR) &name21);
     ret_val20 = STR_ap2004550586(plus_self14, plus_sarg14);
     plus_self15 = ret_val20;
     plus_sarg15 = err_s5;
     ret_val21 = STR_ap2004550586(plus_self15, plus_sarg15);
     err_s5 = ret_val21;
     PROG_err_STR(ATTR(err_self5,prog), err_s5);
    }
   } break;
  case AS_ARRAY_EXPR_tag:
   err_self6 = self;
   err_s6 = ((STR) &Therig416988915);
   if ((err_self6==((TRANS) NULL))) {
    L44 = TRUE;
   } else {
    L44 = (ATTR(err_self6,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L44) {
    L43 = TRUE;
   } else {
    L43 = (ATTR(ATTR(err_self6,cur_rout),sig1)==((SIG) NULL));
   }
   if (L43) {
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   else {
    plus_self16 = ATTR(ATTR(ATTR(err_self6,cur_rout),sig1),str);
    plus_sarg16 = ((STR) &name21);
    ret_val22 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self17 = ret_val22;
    plus_sarg17 = err_s6;
    ret_val23 = STR_ap2004550586(plus_self17, plus_sarg17);
    err_s6 = ret_val23;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val; break;
  default: ;
  other425: ;
 }
 create_self2 = ((AM_ASSIGN_STMT) NULL);
 create_source1 = ATTR(s,source1);
 L46=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L46==NULL) FATAL("Unable to allocate more memory");
 ((OB)L46)->header.tag=AM_ASSIGN_STMT_tag;
 L45 = ((AM_ASSIGN_STMT) L46);
 r3 = L45;
 SATTR(r3,source1,create_source1);
 ret_val24 = r3;
 r = ret_val24;
 SATTR(r,dest,((dAM_EXPR) l));
 SATTR(r,src1,TRANS_693526804(self, ATTR(s,rhs), ((dTP) NULL)));
 if ((ATTR(r,src1)==((dAM_EXPR) NULL))) {
  SATTR(l,tp_at,((dTP) TP_BUI1325635093));
  TRANS_626807411(self, l);
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L47 = ATTR(r,src1);
 SATTR(l,tp_at,(*dAM_EXPR_tprdTP[TAG(L47)])(L47));
 TRANS_626807411(self, l);
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L48 = TRUE;
 } else {
  L48 = ATTR(psather_self,distributed);
 }
 ret_val25 = L48;
 if (ret_val25) {
  rhsam = ATTR(r,src1);
  if (rhsam==NULL) {
   goto other426;
  } else
  switch (TAG(rhsam)) {
   case AM_LOCAL_EXPR_tag:
    SATTR(l,as_type,ATTR(((AM_LOCAL_EXPR) rhsam),as_type)); break;
   case AM_ROU1916046290_tag:
    SATTR(l,as_type,ATTR(((AM_ROU1916046290) rhsam),as_type)); break;
   case AM_SHARED_EXPR_tag:
    SATTR(l,as_type,ATTR(((AM_SHARED_EXPR) rhsam),as_type)); break;
   case AM_ATTR_EXPR_tag:
    SATTR(l,as_type,ATTR(((AM_ATTR_EXPR) rhsam),as_type)); break;
   default: ;
   other426: ;
  }
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_471579016(TRANS self, AS_CALL_EXPR l, AS_ASSIGN_STMT s) {
 dAM_STMT ret_val;
 AM_LOCAL_EXPR loc;
 dAM_STMT r;
 dAS_EXPR a;
 AS_ARG_MODE mode1;
 AM_EXPR_STMT r1;
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
 AS_ARG_MODE create_self;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val3;
 AS_ARG_MODE res;
 AM_EXPR_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_EXPR_STMT ret_val4;
 AM_EXPR_STMT r2;
 BOOL L1;
 BOOL L21_;
 extern STR Itisil1885405538;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 AS_ARG_MODE L10;
 OB L11;
 dAS_EXPR L12;
 AM_EXPR_STMT L13;
 OB L14;
 BOOL L15;
 BOOL L161_;
 L1 = (ATTR(l,args)==((dAS_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) l);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = self;
  err_s = ((STR) &Itisil1885405538);
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
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if ((ATTR(l,ob)==((dAS_EXPR) NULL))) {
  L5 = (ATTR(l,tp)==((AS_TYPE_SPEC) NULL));
 } else {
  L5 = FALSE;
 }
 if (L5) {
  loc = TRANS_1711874080(self, ATTR(l,name1));
  L6 = (loc==((AM_LOCAL_EXPR) NULL));
  L71_=!(L6); 
  if (L71_) {
   r = TRANS_496546257(self, loc, s);
   ret_val = r;
   return ret_val;
  }
 }
 SATTR(l,args,ATTR(s,rhs));
 a = ATTR(l,args);
 while (1) {
  L8 = (a==((dAS_EXPR) NULL));
  L91_=!(L8); 
  if (L91_) {
  }
  else {
   goto after_loop;
  }
  create_self = ((AS_ARG_MODE) NULL);
  create_m = AS_ARG1300937332;
  L11=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=AS_ARG_MODE_tag;
  L10 = ((AS_ARG_MODE) L11);
  res = L10;
  SATTR(res,mod,create_m);
  ret_val3 = res;
  mode1 = ret_val3;
  if ((ATTR(l,modes1)==((AS_ARG_MODE) NULL))) {
   SATTR(l,modes1,mode1);
  }
  else {
   AS_ARG1328804463(ATTR(l,modes1), mode1);
  }
  L12 = a;
  a = (*dAS_EX337431651[TAG(L12)])(L12);
 }
 after_loop: ;
 create_self1 = ((AM_EXPR_STMT) NULL);
 create_source = ATTR(l,source1);
 L14=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_EXPR_STMT_tag;
 L13 = ((AM_EXPR_STMT) L14);
 r2 = L13;
 SATTR(r2,source1,create_source);
 ret_val4 = r2;
 r1 = ret_val4;
 SATTR(r1,expr,TRANS_1705639890(self, l, ((dTP) NULL), FALSE));
 SATTR(l,args,((dAS_EXPR) NULL));
 L15 = (ATTR(r1,expr)==((dAM_EXPR) NULL));
 L161_=!(L15); 
 if (L161_) {
  ret_val = ((dAM_STMT) r1);
  return ret_val;
 }
 else {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_496546257(TRANS self, AM_LOCAL_EXPR loc, AS_ASSIGN_STMT s) {
 dAM_STMT ret_val;
 AM_ASSIGN_STMT r;
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
 AM_ASSIGN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val3;
 AM_ASSIGN_STMT r1;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val4;
 PROG psather_self;
 BOOL ret_val5 = BOOL_zero;
 extern STR Itisil905536373;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 AM_ASSIGN_STMT L3;
 OB L4;
 BOOL L5;
 if (ATTR(loc,no_assign)) {
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) s);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = self;
  err_s = ((STR) &Itisil905536373);
  if ((err_self==((TRANS) NULL))) {
   L2 = TRUE;
  } else {
   L2 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L2) {
   L1 = TRUE;
  } else {
   L1 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L1) {
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
 create_self = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(s,source1);
 L4=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_ASSIGN_STMT_tag;
 L3 = ((AM_ASSIGN_STMT) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val3 = r1;
 r = ret_val3;
 SATTR(r,dest,((dAM_EXPR) loc));
 if (ATTR(self,in_protect_body)) {
  SATTR(loc,is_volatile,TRUE);
 }
 tp_self = loc;
 ret_val4 = ATTR(tp_self,tp_at);
 SATTR(r,src1,TRANS_693526804(self, ATTR(s,rhs), ret_val4));
 if ((ATTR(r,src1)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L5 = TRUE;
 } else {
  L5 = ATTR(psather_self,distributed);
 }
 ret_val5 = L5;
 if (ret_val5) {
  TRANS_423329532(self, loc, s);
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_1191040045(TRANS self, dAS_STMT t) {
 dAS_STMT s;
 AS_LOCK_IF_WHEN iwp;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS return_err_self;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 TRANS return_err_self1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Routin1457291481;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 BOOL L7;
 BOOL L81_;
 extern STR Routin1457291481;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 s = t;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) s);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 switch (TAG(s)) {
  case AS_PAR_STMT_tag:
   TRANS_1611318779(self, ATTR(((AS_PAR_STMT) s),body)); break;
  case AS_LOCK_STMT_tag:
   L1 = ATTR(((AS_LOCK_STMT) s),no_else);
   L21_=!(L1); 
   if (L21_) {
    TRANS_1611318779(self, ATTR(((AS_LOCK_STMT) s),else_part));
   }
   iwp = ATTR(((AS_LOCK_STMT) s),if_when_part);
   while (1) {
    L3 = (iwp==((AS_LOCK_IF_WHEN) NULL));
    L41_=!(L3); 
    if (L41_) {
    }
    else {
     goto after_loop;
    }
    TRANS_1611318779(self, ATTR(iwp,then_part));
    iwp = ATTR(iwp,next);
   }
   after_loop: ; break;
  case AS_UNLOCK_STMT_tag:
   return_err_self = self;
   err_self = return_err_self;
   err_s = ((STR) &Routin1457291481);
   if ((err_self==((TRANS) NULL))) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L6) {
    L5 = TRUE;
   } else {
    L5 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L5) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val;
    plus_sarg1 = err_s;
    ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val1;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   } break;
  case AS_WIT1063437351_tag:
   TRANS_1611318779(self, ATTR(((AS_WIT1063437351) s),near_part));
   L7 = (ATTR(((AS_WIT1063437351) s),else_part)==((AS_STMT_LIST) NULL));
   L81_=!(L7); 
   if (L81_) {
    TRANS_1611318779(self, ATTR(((AS_WIT1063437351) s),else_part));
   } break;
  case AS_ATTACH_STMT_tag:
   return_err_self1 = self;
   err_self1 = return_err_self1;
   err_s1 = ((STR) &Routin1457291481);
   if ((err_self1==((TRANS) NULL))) {
    L10 = TRUE;
   } else {
    L10 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L10) {
    L9 = TRUE;
   } else {
    L9 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L9) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val2;
    plus_sarg3 = err_s1;
    ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val3;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   } break;
  case AS_FORK_STMT_tag:
   TRANS_1611318779(self, ATTR(((AS_FORK_STMT) s),body)); break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::check_pSather_stmt_for_return($AS_STMT)\n./Middle/trans.sa:6278:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_1611318779(TRANS self, AS_STMT_LIST l) {
 dAS_STMT t;
 dAS_STMT s;
 AS_CASE_WHEN wp;
 AS_TYPECASE_WHEN wp1;
 AS_PROTECT_WHEN wp2;
 TRANS return_err_self;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS return_err_self1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 TRANS return_err_self2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val4;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val5;
 TRANS return_err_self3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val6;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val7;
 TRANS return_err_self4;
 TRANS err_self4;
 STR err_s4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val8;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val9;
 TRANS return_err_self5;
 TRANS err_self5;
 STR err_s5;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val10;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val11;
 TRANS return_err_self6;
 TRANS err_self6;
 STR err_s6;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val12;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val13;
 extern STR Routin1457291481;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 dAS_STMT L3;
 dAS_STMT L4;
 extern STR Routin1457291481;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 extern STR Routin1457291481;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 BOOL L9;
 BOOL L101_;
 extern STR Routin1457291481;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 extern STR Routin1457291481;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 BOOL L21;
 BOOL L221_;
 extern STR Routin1457291481;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 BOOL L25;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 extern STR Routin1457291481;
 BOOL L29;
 BOOL L30;
 extern STR name21;
 t = ATTR(l,stmts);
 if ((t==((dAS_STMT) NULL))) {
  return_err_self = self;
  err_self = return_err_self;
  err_s = ((STR) &Routin1457291481);
  if ((err_self==((TRANS) NULL))) {
   L2 = TRUE;
  } else {
   L2 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L2) {
   L1 = TRUE;
  } else {
   L1 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L1) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val;
   plus_sarg1 = err_s;
   ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val1;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  return;
 }
 s = t;
 while (1) {
  L3 = s;
  if (((*dAS_ST460279343[TAG(L3)])(L3)==((dAS_STMT) NULL))) {
   goto after_loop;
  }
  L4 = s;
  s = (*dAS_ST460279343[TAG(L4)])(L4);
 }
 after_loop: ;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) s);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 if (s==NULL) {
  goto other424;
 } else
 switch (TAG(s)) {
  case AS_DEC_STMT_tag:
   return_err_self1 = self;
   err_self1 = return_err_self1;
   err_s1 = ((STR) &Routin1457291481);
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
    ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val2;
    plus_sarg3 = err_s1;
    ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val3;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   } break;
  case AS_ASSIGN_STMT_tag:
   return_err_self2 = self;
   err_self2 = return_err_self2;
   err_s2 = ((STR) &Routin1457291481);
   if ((err_self2==((TRANS) NULL))) {
    L8 = TRUE;
   } else {
    L8 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L8) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L7) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val4;
    plus_sarg5 = err_s2;
    ret_val5 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val5;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   } break;
  case AS_IF_STMT_tag:
   TRANS_1611318779(self, ATTR(((AS_IF_STMT) s),then_part));
   L9 = (ATTR(((AS_IF_STMT) s),else_part)==((AS_STMT_LIST) NULL));
   L101_=!(L9); 
   if (L101_) {
    TRANS_1611318779(self, ATTR(((AS_IF_STMT) s),else_part));
   } break;
  case AS_LOOP_STMT_tag: break;
  case AS_RETURN_STMT_tag: break;
  case AS_YIELD_STMT_tag:
   return_err_self3 = self;
   err_self3 = return_err_self3;
   err_s3 = ((STR) &Routin1457291481);
   if ((err_self3==((TRANS) NULL))) {
    L12 = TRUE;
   } else {
    L12 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L12) {
    L11 = TRUE;
   } else {
    L11 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L11) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg6 = ((STR) &name21);
    ret_val6 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val6;
    plus_sarg7 = err_s3;
    ret_val7 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s3 = ret_val7;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   } break;
  case AS_QUIT_STMT_tag:
   return_err_self4 = self;
   err_self4 = return_err_self4;
   err_s4 = ((STR) &Routin1457291481);
   if ((err_self4==((TRANS) NULL))) {
    L14 = TRUE;
   } else {
    L14 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L14) {
    L13 = TRUE;
   } else {
    L13 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L13) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val8 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val8;
    plus_sarg9 = err_s4;
    ret_val9 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s4 = ret_val9;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   } break;
  case AS_CASE_STMT_tag:
   L15 = ATTR(((AS_CASE_STMT) s),no_else);
   L161_=!(L15); 
   if (L161_) {
    TRANS_1611318779(self, ATTR(((AS_CASE_STMT) s),else_part));
   }
   wp = ATTR(((AS_CASE_STMT) s),when_part);
   while (1) {
    L17 = (wp==((AS_CASE_WHEN) NULL));
    L181_=!(L17); 
    if (L181_) {
    }
    else {
     goto after_loop1;
    }
    TRANS_1611318779(self, ATTR(wp,then_part));
    wp = ATTR(wp,next);
   }
   after_loop1: ; break;
  case AS_TYPECASE_STMT_tag:
   L19 = ATTR(((AS_TYPECASE_STMT) s),no_else);
   L201_=!(L19); 
   if (L201_) {
    TRANS_1611318779(self, ATTR(((AS_TYPECASE_STMT) s),else_part));
   }
   wp1 = ATTR(((AS_TYPECASE_STMT) s),when_part);
   while (1) {
    L21 = (wp1==((AS_TYPECASE_WHEN) NULL));
    L221_=!(L21); 
    if (L221_) {
    }
    else {
     goto after_loop2;
    }
    TRANS_1611318779(self, ATTR(wp1,then_part));
    wp1 = ATTR(wp1,next);
   }
   after_loop2: ; break;
  case AS_ASSERT_STMT_tag:
   return_err_self5 = self;
   err_self5 = return_err_self5;
   err_s5 = ((STR) &Routin1457291481);
   if ((err_self5==((TRANS) NULL))) {
    L24 = TRUE;
   } else {
    L24 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L24) {
    L23 = TRUE;
   } else {
    L23 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
   }
   if (L23) {
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val10;
    plus_sarg11 = err_s5;
    ret_val11 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s5 = ret_val11;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   } break;
  case AS_PROTECT_STMT_tag:
   L25 = ATTR(((AS_PROTECT_STMT) s),no_else);
   L261_=!(L25); 
   if (L261_) {
    TRANS_1611318779(self, ATTR(((AS_PROTECT_STMT) s),else_part));
   }
   wp2 = ATTR(((AS_PROTECT_STMT) s),when_part);
   while (1) {
    L27 = (wp2==((AS_PROTECT_WHEN) NULL));
    L281_=!(L27); 
    if (L281_) {
    }
    else {
     goto after_loop3;
    }
    TRANS_1611318779(self, ATTR(wp2,then_part));
    wp2 = ATTR(wp2,next);
   }
   after_loop3: ; break;
  case AS_RAISE_STMT_tag: break;
  case AS_EXPR_STMT_tag:
   return_err_self6 = self;
   err_self6 = return_err_self6;
   err_s6 = ((STR) &Routin1457291481);
   if ((err_self6==((TRANS) NULL))) {
    L30 = TRUE;
   } else {
    L30 = (ATTR(err_self6,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L30) {
    L29 = TRUE;
   } else {
    L29 = (ATTR(ATTR(err_self6,cur_rout),sig1)==((SIG) NULL));
   }
   if (L29) {
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   else {
    plus_self12 = ATTR(ATTR(ATTR(err_self6,cur_rout),sig1),str);
    plus_sarg12 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val12;
    plus_sarg13 = err_s6;
    ret_val13 = STR_ap2004550586(plus_self13, plus_sarg13);
    err_s6 = ret_val13;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   } break;
  default: ;
  other424: ;
   TRANS_1191040045(self, s);
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_423329532(TRANS self, AM_LOCAL_EXPR loc, AS_ASSIGN_STMT s) {
 AS_ASSIGN_STMT as;
 AM_LOCAL_EXPR last_helper;
 AM_LOCAL_EXPR rel_helper = ((AM_LOCAL_EXPR) NULL);
 AS_CALL_EXPR locvar;
 AS_CALL_EXPR helploc;
 AS_ASSIGN_STMT create_self;
 AS_ASSIGN_STMT ret_val;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val1;
 BOOL L1;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 STR L6;
 extern STR S_pS_cohort;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 AS_ASSIGN_STMT L11;
 OB L12;
 AS_CALL_EXPR L13;
 OB L14;
 L2 = STR_sizerINT(ATTR(loc,name1).str);
 L31_=(L2)<(10); 
 L4 = L31_;
 L51_=!(L4); 
 if (L51_) {
  L6 = STR_head_INTrSTR(ATTR(loc,name1).str, 10);
  L1 = STR_is111530248(L6, ((STR) &S_pS_cohort));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  if ((ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL))) {
   SATTR(self,cur_cohort,loc);
  }
 }
 L7 = ATTR(s,transformed);
 L81_=!(L7); 
 if (L81_) {
  SATTR(s,transformed,TRUE);
  L9 = TRANS_89465360;
  L101_=!(L9); 
  if (L101_) {
   return;
  }
  as = ((AS_ASSIGN_STMT) NULL);
  last_helper = TRANS_647716465(self);
  if ((last_helper==((AM_LOCAL_EXPR) NULL))) {
   return;
  }
  if ((ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL))) {
   rel_helper = last_helper;
  }
  else {
   rel_helper = TRANS_2133809761(self);
   if ((rel_helper==((AM_LOCAL_EXPR) NULL))) {
    rel_helper = last_helper;
   }
  }
  if (TRANS_157729662(self, ATTR(loc,name1), rel_helper)) {
   create_self = ((AS_ASSIGN_STMT) NULL);
   L12=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=AS_ASSIGN_STMT_tag;
   L11 = ((AS_ASSIGN_STMT) L12);
   ret_val = L11;
   as = ret_val;
   SATTR(as,source1,ATTR(s,source1));
   SATTR(as,surr_stmt_list,ATTR(s,surr_stmt_list));
   create_self1 = ((AS_CALL_EXPR) NULL);
   L14=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L14==NULL) FATAL("Unable to allocate more memory");
   ((OB)L14)->header.tag=AS_CALL_EXPR_tag;
   L13 = ((AS_CALL_EXPR) L14);
   ret_val1 = L13;
   locvar = ret_val1;
   SATTR(locvar,source1,ATTR(s,source1));
   SATTR(locvar,name1,ATTR(loc,name1));
   helploc = TRANS_1849744987(self, ATTR(loc,name1), rel_helper, ATTR(s,source1));
   SATTR(as,lhs_expr,((dAS_EXPR) helploc));
   SATTR(as,rhs,((dAS_EXPR) locvar));
   SATTR(as,next,ATTR(s,next));
   SATTR(s,next,((dAS_STMT) as));
   return;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_548661555(TRANS self, AS_ASSIGN_STMT s) {
 dAS_EXPR lhs;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val4;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val5;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val6;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val7;
 TRANS err_self4;
 STR err_s4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val8;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val9;
 TRANS err_self5;
 STR err_s5;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val10;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val11;
 TRANS err_self6;
 STR err_s6;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val12;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val13;
 extern STR Itisil1607700999;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 extern STR Itisil187901097;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR Itisil325536867;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 extern STR Itisil1048791346;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 extern STR Itisil1608792715;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 extern STR Itisil1021252333;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 extern STR Itisil1815917697;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 lhs = ATTR(s,lhs_expr);
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) lhs);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 switch (TAG(lhs)) {
  case AS_HERE_EXPR_tag:
   err_self = self;
   err_s = ((STR) &Itisil1607700999);
   if ((err_self==((TRANS) NULL))) {
    L2 = TRUE;
   } else {
    L2 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L2) {
    L1 = TRUE;
   } else {
    L1 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L1) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val;
    plus_sarg1 = err_s;
    ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val1;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   } break;
  case AS_ANY_EXPR_tag:
   err_self1 = self;
   err_s1 = ((STR) &Itisil187901097);
   if ((err_self1==((TRANS) NULL))) {
    L4 = TRUE;
   } else {
    L4 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L4) {
    L3 = TRUE;
   } else {
    L3 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L3) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val2;
    plus_sarg3 = err_s1;
    ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val3;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   } break;
  case AS_COHORT_EXPR_tag:
   err_self2 = self;
   err_s2 = ((STR) &Itisil325536867);
   if ((err_self2==((TRANS) NULL))) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L6) {
    L5 = TRUE;
   } else {
    L5 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L5) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val4;
    plus_sarg5 = err_s2;
    ret_val5 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val5;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   } break;
  case AS_WHERE_EXPR_tag:
   err_self3 = self;
   err_s3 = ((STR) &Itisil1048791346);
   if ((err_self3==((TRANS) NULL))) {
    L8 = TRUE;
   } else {
    L8 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L8) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L7) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg6 = ((STR) &name21);
    ret_val6 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val6;
    plus_sarg7 = err_s3;
    ret_val7 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s3 = ret_val7;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   } break;
  case AS_NEAR_EXPR_tag:
   err_self4 = self;
   err_s4 = ((STR) &Itisil1608792715);
   if ((err_self4==((TRANS) NULL))) {
    L10 = TRUE;
   } else {
    L10 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L10) {
    L9 = TRUE;
   } else {
    L9 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L9) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val8 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val8;
    plus_sarg9 = err_s4;
    ret_val9 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s4 = ret_val9;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   } break;
  case AS_FAR_EXPR_tag:
   err_self5 = self;
   err_s5 = ((STR) &Itisil1021252333);
   if ((err_self5==((TRANS) NULL))) {
    L12 = TRUE;
   } else {
    L12 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L12) {
    L11 = TRUE;
   } else {
    L11 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
   }
   if (L11) {
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val10;
    plus_sarg11 = err_s5;
    ret_val11 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s5 = ret_val11;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   } break;
  case AS_AT_EXPR_tag:
   err_self6 = self;
   err_s6 = ((STR) &Itisil1815917697);
   if ((err_self6==((TRANS) NULL))) {
    L14 = TRUE;
   } else {
    L14 = (ATTR(err_self6,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L14) {
    L13 = TRUE;
   } else {
    L13 = (ATTR(ATTR(err_self6,cur_rout),sig1)==((SIG) NULL));
   }
   if (L13) {
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   else {
    plus_self12 = ATTR(ATTR(ATTR(err_self6,cur_rout),sig1),str);
    plus_sarg12 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val12;
    plus_sarg13 = err_s6;
    ret_val13 = STR_ap2004550586(plus_self13, plus_sarg13);
    err_s6 = ret_val13;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::transform_pSather_assign_stmt_err(AS_ASSIGN_STMT)\n./Middle/trans.sa:4015:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_673475073(TRANS self, AS_ROUT_DEF t) {
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val = BOOL_zero;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 STR L4;
 INT L5;
 INT L61_;
 INT L7;
 CHAR L81_;
 CHAR L9;
 BOOL L101_;
 if ((t==((AS_ROUT_DEF) NULL))) {
  return;
 }
 if ((ATTR(t,ret_dec)==((AS_TYPE_SPEC) NULL))) {
  return;
 }
 is_iter_self = ATTR(t,name1);
 L2 = (is_iter_self.str==((STR) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = is_iter_self.str;
  L5 = STR_sizerINT(is_iter_self.str);
  L61_=INTMINUS(L5,1); 
  L7 = L61_;
  L81_=ARR((STR)L4,L7); 
  L9 = L81_;
  L101_=L9=='!'; 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 if (ret_val) {
  return;
 }
 if (ATTR(t,is_builtin)) {
  return;
 }
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) t);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 TRANS_1611318779(self, ATTR(t,body));
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_75192950(TRANS self, AS_ROUT_DEF as) {
 SIG is_forked_self;
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 AM_ROUT_DEF L3;
 AM_FORMAL_ARG L41_;
 AM_FORMAL_ARG L5;
 AM_ROUT_DEF L6;
 AM_FORMAL_ARG L71_;
 AM_FORMAL_ARG L8;
 BOOL L9;
 BOOL L101_;
 AM_ROUT_DEF L11;
 AM_FORMAL_ARG L121_;
 AM_FORMAL_ARG L13;
 SATTR(ATTR(self,cur_rout),is_par_routine,ATTR(as,is_par_routine));
 SATTR(ATTR(self,cur_rout),is_fork_routine,ATTR(as,is_fork_routine));
 SATTR(ATTR(self,cur_rout),is_attach_routin,ATTR(as,is_attach_routin));
 SATTR(ATTR(ATTR(self,cur_rout),sig1),is_par_routine,ATTR(ATTR(self,cur_rout),is_par_routine));
 SATTR(ATTR(ATTR(self,cur_rout),sig1),is_fork_routine,ATTR(ATTR(self,cur_rout),is_fork_routine));
 SATTR(ATTR(ATTR(self,cur_rout),sig1),is_attach_routin,ATTR(ATTR(self,cur_rout),is_attach_routin));
 is_forked_self = ATTR(ATTR(self,cur_rout),sig1);
 if (ATTR(is_forked_self,is_par_routine)) {
  L2 = TRUE;
 } else {
  L2 = ATTR(is_forked_self,is_fork_routine);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L1 = ATTR(is_forked_self,is_attach_routin);
 }
 ret_val = L1;
 if (ret_val) {
  L3 = ATTR(self,cur_rout);
  L41_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L3,1); 
  L5=L41_;
  SATTR(self,cur_param_ob,ATTR(L5,expr));
  if (ATTR(ATTR(self,cur_rout),is_par_routine)) {
   L6 = ATTR(self,cur_rout);
   L71_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L6,1); 
   L8=L71_;
   SATTR(self,cur_par_ob,ATTR(L8,expr));
  }
  L9 = ATTR(ATTR(self,cur_rout),is_attach_routin);
  L101_=!(L9); 
  if (L101_) {
   L11 = ATTR(self,cur_rout);
   L121_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)L11,2); 
   L13=L121_;
   SATTR(self,cur_cohort,ATTR(L13,expr));
  }
 }
}
