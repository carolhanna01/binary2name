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

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

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

typedef struct ARRAYCALL_ARG_struct {/* layout for ARRAY{CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct CALL_ARG_struct *arr_part[1];
 } *ARRAYCALL_ARG;

typedef struct CALL_ARG_struct {/* layout for CALL_ARG */
 OB_HEADER header;
 struct dCALL_TP_struct *tp;
 struct dMODE_struct *mode1;
 } *CALL_ARG;

typedef struct CALL_T666389079_struct {/* layout for CALL_TP_UNDERSCORE */
 OB_HEADER header;
 struct dTP_struct *tp;
 } *CALL_T666389079;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct IFC_struct {/* layout for IFC */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SIG_TBL_struct *sigs;
 } *IFC;

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

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

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

typedef struct TP_ITER_struct {/* layout for TP_ITER */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct ARRAYARG_struct *args;
 struct ARRAYBOOL_struct *hot;
 struct IFC_struct *my_ifc;
 struct PROG_struct *prog;
 STR sas_cache;
 BOOL is_generic;
 } *TP_ITER;

typedef struct TP_ITER_TBL_struct {/* layout for TP_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_ITER_struct *arr_part[1];
 } *TP_ITER_TBL;

typedef struct TP_ROUT_struct {/* layout for TP_ROUT */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct ARRAYARG_struct *args;
 struct IFC_struct *my_ifc;
 struct PROG_struct *prog;
 STR sas_cache;
 BOOL is_fortran;
 BOOL is_generic;
 BOOL is_remote;
 } *TP_ROUT;

typedef struct TP_ROUT_TBL_struct {/* layout for TP_ROUT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_ROUT_struct *arr_part[1];
 } *TP_ROUT_TBL;

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

typedef struct TUPdAM_EXPRdTP_struct {/* layout for TUP{$AM_EXPR,$TP} */
 struct dAM_EXPR_struct *t1;
 struct dTP_struct *t2;
 } TUPdAM_EXPRdTP;
static TUPdAM_EXPRdTP TUPdAM_EXPRdTP_zero;

typedef struct TUPdAM_EXPRdTP_boxed_struct {
 OB_HEADER header;
 TUPdAM_EXPRdTP immutable_part;
 } *TUPdAM_EXPRdTP_boxed;

typedef struct TUPARRAYARGdTP_struct {/* layout for TUP{ARRAY{ARG},$TP} */
 struct dTP_struct *t2;
 struct ARRAYARG_struct *t1;
 } TUPARRAYARGdTP;
static TUPARRAYARGdTP TUPARRAYARGdTP_zero;

typedef struct TUPARRAYARGdTP_boxed_struct {
 OB_HEADER header;
 TUPARRAYARGdTP immutable_part;
 } *TUPARRAYARGdTP_boxed;

typedef struct TUPARR1759987776_struct {/* layout for TUP{ARRAY{ARG},ARRAY{BOOL},$TP} */
 struct dTP_struct *t3;
 struct ARRAYARG_struct *t1;
 struct ARRAYBOOL_struct *t2;
 } TUPARR1759987776;
static TUPARR1759987776 TUPARR1759987776_zero;

typedef struct TUPARR1759987776_boxed_struct {
 OB_HEADER header;
 TUPARR1759987776 immutable_part;
 } *TUPARR1759987776_boxed;

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

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

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

typedef struct AM_CHAR_CONST_struct {/* layout for AM_CHAR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 CHAR bval;
 } *AM_CHAR_CONST;

typedef struct AM_EXCEPT_EXPR_struct {/* layout for AM_EXCEPT_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_EXCEPT_EXPR;

typedef struct AM_FLTDX_CONST_struct {/* layout for AM_FLTDX_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTDX_CONST;

typedef struct AM_FLTD_CONST_struct {/* layout for AM_FLTD_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTD_CONST;

typedef struct AM_FLTI_CONST_struct {/* layout for AM_FLTI_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTI_CONST;

typedef struct AM_FLTX_CONST_struct {/* layout for AM_FLTX_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTX_CONST;

typedef struct AM_FLT_CONST_struct {/* layout for AM_FLT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLT_CONST;

typedef struct AM_IF_EXPR_struct {/* layout for AM_IF_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *if_false;
 struct dAM_EXPR_struct *if_true;
 struct dAM_EXPR_struct *test1;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IF_EXPR;

typedef struct AM_INITIAL_STMT_struct {/* layout for AM_INITIAL_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct dAM_STMT_struct *stmts;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_INITIAL_STMT;

typedef struct AM_INTI_CONST_struct {/* layout for AM_INTI_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INTI_CONST;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

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

typedef struct AM_NEW_EXPR_struct {/* layout for AM_NEW_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *asz;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_NEW_EXPR;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

typedef struct AM_VOID_CONST_struct {/* layout for AM_VOID_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_VOID_CONST;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYA409651007_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA409651007_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYB1390856792_frame_struct {
 INT state;
 ARRAYBOOL self;/* Formal argument: self */
 BOOL ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB1390856792_frame;

typedef struct ARRAYB741586158_frame_struct {
 INT state;
 ARRAYBOOL self;/* Formal argument: self */
 BOOL arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB741586158_frame;

typedef struct ARRAYC1058209184_frame_struct {
 INT state;
 ARRAYCALL_ARG self;/* Formal argument: self */
 CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1058209184_frame;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

typedef struct ARRAYI69049459_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI69049459_frame;

typedef struct AS_AND_EXPR_struct {/* layout for AS_AND_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AND_EXPR;

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

typedef struct AS_CHAR_LIT_EXPR_struct {/* layout for AS_CHAR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT val1;
 struct SFILE_ID_struct source1;
 } *AS_CHAR_LIT_EXPR;

typedef struct AS_CREATE_EXPR_struct {/* layout for AS_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_CREATE_EXPR;

typedef struct AS_EXCEPT_EXPR_struct {/* layout for AS_EXCEPT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_EXCEPT_EXPR;

typedef struct AS_FLT_LIT_EXPR_struct {/* layout for AS_FLT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT tp;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AS_FLT_LIT_EXPR;

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

typedef struct AS_RESULT_EXPR_struct {/* layout for AS_RESULT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_RESULT_EXPR;

typedef struct AS_STR_LIT_EXPR_struct {/* layout for AS_STR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 STR s;
 } *AS_STR_LIT_EXPR;

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

typedef struct AS_VOID_EXPR_struct {/* layout for AS_VOID_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_VOID_EXPR;

typedef struct CALL_SIG_struct {/* layout for CALL_SIG */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ARRAYCALL_ARG_struct *args;
 struct IDENT_struct name1;
 BOOL has_ret;
 BOOL unknown_ret;
 } *CALL_SIG;

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

extern IDENT IDENT_190278447;
extern IDENT IDENT_458929583;
extern INT AS_TYP1853679690;
extern INT TP_KIND_part_tp;
extern INT TP_KIND_ref_tp;
extern INT TP_KIND_spr_tp;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_flt;
extern TP_CLASS TP_BUILTIN_fltd;
extern TP_CLASS TP_BUILTIN_fltdx;
extern TP_CLASS TP_BUILTIN_flti;
extern TP_CLASS TP_BUILTIN_fltx;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_inti;
extern TP_CLASS TP_BUILTIN_str;

/* Function declarations */


extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is411624931)(dTP);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dINLIN970826022)(dINLINE, AM_ROU1916046290);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);
AM_LOCAL_EXPR TRANS_1711874080(TRANS, IDENT);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYINT TRANS_2014167366(TRANS, INT, AS_BOU14269336);
ARRAYINT TRANS_80510940(TRANS, INT, AS_BOU14269336);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL SIG_is275586960(SIG);
BOOL TP_CLA513015767(TP_CLASS, dCALL_TP);
BOOL TRANS_1865855576(TRANS, dAM_EXPR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA811588375(FLISTA725283029, FLISTA725283029);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT AS_ARR1847317313(AS_ARRAY_EXPR);
INT AS_CAL1853175953(AS_CALL_EXPR);
INT AS_CRE943264723(AS_CREATE_EXPR);
INT STR_sizerINT(STR);
INT TRANS_844294361(TRANS, dAM_EXPR, AS_BOU14269336);
IN_MODE IN_MOD403789248(IN_MODE);
ONCE_MODE ONCE_M704734922(ONCE_MODE);
SIG CALL_S2120587481(CALL_SIG, BOOL);
SIG IFC_si178998979(IFC, CALL_SIG);
SIG IMPL_s773282892(IMPL, CALL_SIG);
SIG TRANS_2093630221(TRANS, AS_BOU14269336, dTP, dTP);
STR STR_ap2004550586(STR, STR);
TP_ITER TP_ITE1350567874(TP_ITER, ARRAYARG, ARRAYBOOL, dTP, PROG);
TP_ITER TP_ITE990128539(TP_ITER_TBL, TUPARR1759987776);
TP_ITER_TBL TP_ITE992470229(TP_ITER_TBL, TP_ITER);
TP_ROUT TP_ROU52430373(TP_ROUT, ARRAYARG, dTP, PROG);
TP_ROUT TP_ROU581494640(TP_ROUT_TBL, TUPARRAYARGdTP);
TP_ROUT_TBL TP_ROU1247655203(TP_ROUT_TBL, TP_ROUT);
TUPdAM_EXPRdTP TRANS_962473634(TRANS, AS_BOU14269336);
dAM_EXPR TRANS_1006089529(TRANS, AS_AND_EXPR, dTP);
dAM_EXPR TRANS_1168851617(TRANS, AS_IS_VOID_EXPR, dTP);
dAM_EXPR TRANS_1173176875(TRANS, AS_INT_LIT_EXPR, dTP);
dAM_EXPR TRANS_1269501442(TRANS, AS_CHAR_LIT_EXPR, dTP);
dAM_EXPR TRANS_1346628084(TRANS, AS_RESULT_EXPR, dTP);
dAM_EXPR TRANS_1475346849(TRANS, AS_FLT_LIT_EXPR, dTP);
dAM_EXPR TRANS_1546520066(TRANS, AS_BOU14269336, dTP);
dAM_EXPR TRANS_1556853519(TRANS, AS_STR_LIT_EXPR, dTP);
dAM_EXPR TRANS_1627619307(TRANS, AS_INITIAL_EXPR, dTP);
dAM_EXPR TRANS_1668850900(TRANS, AS_EXCEPT_EXPR, dTP);
dAM_EXPR TRANS_1783297694(TRANS, AS_BOOL_LIT_EXPR, dTP);
dAM_EXPR TRANS_196236678(TRANS, AS_VOID_EXPR, dTP);
dAM_EXPR TRANS_361080840(TRANS, AS_OR_EXPR, dTP);
dAM_EXPR TRANS_595522723(TRANS, AS_ARRAY_EXPR, dTP);
dAM_EXPR TRANS_693526804(TRANS, dAS_EXPR, dTP);
dAM_EXPR TRANS_700113784(TRANS, AS_CREATE_EXPR, dTP);
dAM_EXPR TRANS_962243143(TRANS, AS_NEW_EXPR, dTP);
dCALL_TP TRANS_68137339(TRANS, dAS_EXPR);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void ARRAYA409651007(ARRAYA409651007_frame);
void ARRAYB741586158(ARRAYB741586158_frame);
void ARRAYC1058209184(ARRAYC1058209184_frame);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void SE_CON412941612(SE_CONTEXT, dAM);
void SE_CON435970114(SE_CONTEXT);
void TRANS_1147317392(TRANS, AS_BOU14269336, AM_BND1124877163);
void TRANS_74748323(TRANS, AS_TYPE_SPEC);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

ARRAYINT TRANS_2014167366(TRANS self, INT nbnd, AS_BOU14269336 e) {
 ARRAYINT ret_val;
 INT nr_unbound_arg = INT_zero;
 ARRAYINT r = ((ARRAYINT) NULL);
 INT rind = INT_zero;
 dAS_EXPR st;
 INT aind = INT_zero;
 dAS_EXPR a;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 ARRAYINT L8;
 INT L9;
 OB L10;
 INT L11;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 INT L161_;
 INT L181_;
 dAS_EXPR L19;
 L1 = AS_CAL1853175953(ATTR(e,call1));
 L21_=INTPLUS(1,L1); 
 L3 = L21_;
 L41_=INTMINUS(L3,nbnd); 
 nr_unbound_arg = L41_;
 L51_=(nr_unbound_arg)==(0); 
 L6 = L51_;
 L71_=!(L6); 
 if (L71_) {
  create_self = ((ARRAYINT) NULL);
  create_n = nr_unbound_arg;
  L9 = create_n;
  L11=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L9)*sizeof(INT);
  L10=ZALLOC_LEAF_BIG(L11);
  if (L10==NULL) FATAL("Unable to allocate more memory");
  memset(L10,0,L11);
  ((OB)L10)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

    ((OB)L10)->header.destroyed=0;
#endif

  L8 = ((ARRAYINT) L10);
  L8->asize = L9;
  ret_val1 = L8;
  r = ret_val1;
 }
 rind = 0;
 st = ATTR(ATTR(e,call1),ob);
 if (st==NULL) {
  goto other421;
 } else
 switch (TAG(st)) {
  case AS_UND152986614_tag:
   SARR((ARRAYINT)r,rind,0); 
   ;
   L131_=INTPLUS(rind,1); 
   rind = L131_; break;
  default: ;
  other421: ;
 }
 aind = 0;
 a = ATTR(ATTR(e,call1),args);
 while (1) {
  L14 = (a==((dAS_EXPR) NULL));
  L151_=!(L14); 
  if (L151_) {
  }
  else {
   goto after_loop;
  }
  L161_=INTPLUS(aind,1); 
  aind = L161_;
  if (a==NULL) {
   goto other422;
  } else
  switch (TAG(a)) {
   case AS_UND152986614_tag:
    SARR((ARRAYINT)r,rind,aind); 
    ;
    L181_=INTPLUS(rind,1); 
    rind = L181_; break;
   default: ;
   other422: ;
  }
  L19 = a;
  a = (*dAS_EX337431651[TAG(L19)])(L19);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYINT TRANS_80510940(TRANS self, INT nbnd, AS_BOU14269336 e) {
 ARRAYINT ret_val;
 ARRAYINT r = ((ARRAYINT) NULL);
 INT rind = INT_zero;
 dAS_EXPR st;
 INT aind = INT_zero;
 dAS_EXPR a;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val1;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 ARRAYINT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L91_;
 BOOL L10;
 BOOL L121_;
 INT L131_;
 INT L151_;
 dAS_EXPR L16;
 L11_=(nbnd)==(0); 
 L2 = L11_;
 L31_=!(L2); 
 if (L31_) {
  create_self = ((ARRAYINT) NULL);
  create_n = nbnd;
  L5 = create_n;
  L7=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L5)*sizeof(INT);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((ARRAYINT) L6);
  L4->asize = L5;
  ret_val1 = L4;
  r = ret_val1;
 }
 rind = 0;
 st = ATTR(ATTR(e,call1),ob);
 if (st==NULL) {
  goto other419;
 } else
 switch (TAG(st)) {
  case AS_UND152986614_tag: break;
  default: ;
  other419: ;
   SARR((ARRAYINT)r,rind,0); 
   ;
   L91_=INTPLUS(rind,1); 
   rind = L91_;
 }
 aind = 0;
 a = ATTR(ATTR(e,call1),args);
 while (1) {
  L10 = (a==((dAS_EXPR) NULL));
  L121_=!(L10); 
  if (L121_) {
  }
  else {
   goto after_loop;
  }
  L131_=INTPLUS(aind,1); 
  aind = L131_;
  if (a==NULL) {
   goto other420;
  } else
  switch (TAG(a)) {
   case AS_UND152986614_tag: break;
   default: ;
   other420: ;
    SARR((ARRAYINT)r,rind,aind); 
    ;
    L151_=INTPLUS(rind,1); 
    rind = L151_;
  }
  L16 = a;
  a = (*dAS_EX337431651[TAG(L16)])(L16);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TRANS_844294361(TRANS self, dAM_EXPR self_val, AS_BOU14269336 e) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 dAS_EXPR a;
 BOOL L1;
 BOOL L21_;
 INT L31_;
 dAS_EXPR L4;
 if ((self_val==((dAM_EXPR) NULL))) {
  r = 0;
 }
 else {
  r = 1;
 }
 a = ATTR(ATTR(e,call1),args);
 while (1) {
  L1 = (a==((dAS_EXPR) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  if (a==NULL) {
   goto other416;
  } else
  switch (TAG(a)) {
   case AS_UND152986614_tag: break;
   default: ;
   other416: ;
    L31_=INTPLUS(r,1); 
    r = L31_;
  }
  L4 = a;
  a = (*dAS_EX337431651[TAG(L4)])(L4);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG TRANS_2093630221(TRANS self, AS_BOU14269336 e, dTP self_tp1, dTP ctp) {
 SIG ret_val;
 ARRAYARG con_args = ((ARRAYARG) NULL);
 dTP con_ret = ((dTP) NULL);
 CALL_SIG call_sig;
 dAS_EXPR ca;
 AS_ARG_MODE cm;
 INT i = INT_zero;
 dAS_EXPR ob;
 dAS_EXPR arg;
 INT unbound_args = INT_zero;
 dCALL_TP atp;
 dCALL_TP atp1;
 ARRAYCALL_ARG L11;
 SIG s;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CALL_SIG create_self;
 CALL_SIG ret_val3;
 ARRAYCALL_ARG create_self1;
 INT create_n = INT_zero;
 ARRAYCALL_ARG ret_val4;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 CALL_ARG create_self2;
 dCALL_TP create_t;
 dMODE create_m;
 CALL_ARG ret_val9;
 CALL_ARG res;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 ARG L61_;
 ARG L7;
 extern STR Failur531548896;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 CALL_SIG L10;
 OB L12;
 ARRAYCALL_ARG L13;
 INT L14;
 OB L15;
 INT L16;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L20;
 BOOL L211_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 INT L261_;
 dAS_EXPR L27;
 INT L281_;
 INT L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 INT L341_;
 INT L35;
 BOOL L361_;
 extern STR Toofew1923659380;
 BOOL L37;
 BOOL L38;
 extern STR name21;
 extern STR Tooman1400108149;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 BOOL L42;
 BOOL L431_;
 dAM_EXPR L44;
 BOOL L45;
 BOOL L461_;
 ARG L471_;
 ARG L48;
 INT L491_;
 INT L411_br;
INT i_L411_=0;
 CALL_ARG L50;
 OB L52;
 CALL_ARG L53;
 dAS_EXPR L54;
 dTP L55;
 L2 = (ctp==((dTP) NULL));
 L31_=!(L2); 
 if (L31_) {
  switch (TAG(ctp)) {
   case TP_ROUT_tag:
    con_args = ATTR(((TP_ROUT) ctp),args);
    con_ret = ATTR(((TP_ROUT) ctp),ret); break;
   case TP_ITER_tag:
    con_args = ATTR(((TP_ITER) ctp),args);
    con_ret = ATTR(((TP_ITER) ctp),ret); break;
   default: ;
    FATAL("No applicable type in typecase\nin TRANS::bound_create_sig(AS_BOUND_CREATE_EXPR,$TP,$TP):SIG\n./Middle/trans.sa:2778:17");
  }
 }
 if ((self_tp1==((dTP) NULL))) {
  L4 = (con_args==((ARRAYARG) NULL));
  L51_=!(L4); 
  if (L51_) {
   L61_=(ARG)ARR((ARRAYARG)con_args,0); 
   L7=L61_;
   self_tp1 = ATTR(L7,tp);
  }
  else {
   err_self = self;
   err_s = ((STR) &Failur531548896);
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
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = err_s;
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val2;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((SIG) NULL);
   return ret_val;
  }
 }
 create_self = ((CALL_SIG) NULL);
 L12=ZALLOC(sizeof(struct CALL_SIG_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=CALL_SIG_tag;
 L10 = ((CALL_SIG) L12);
 ret_val3 = L10;
 call_sig = ret_val3;
 SATTR(call_sig,tp,self_tp1);
 SATTR(call_sig,name1,ATTR(ATTR(e,call1),name1));
 create_self1 = ((ARRAYCALL_ARG) NULL);
 create_n = AS_CAL1853175953(ATTR(e,call1));
 L14 = create_n;
 L16=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L14)*sizeof(CALL_ARG);
 L15=ZALLOC_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((ARRAYCALL_ARG) L15);
 L13->asize = L14;
 ret_val4 = L13;
 SATTR(call_sig,args,ret_val4);
 L17 = (con_ret==((dTP) NULL));
 L181_=!(L17); 
 if (L181_) {
  SATTR(call_sig,has_ret,TRUE);
 }
 if ((ctp==((dTP) NULL))) {
  L19 = (ATTR(e,ret)==((AS_TYPE_SPEC) NULL));
 } else {
  L19 = FALSE;
 }
 if (L19) {
  SATTR(call_sig,unknown_ret,TRUE);
 }
 ca = ATTR(ATTR(e,call1),args);
 cm = ATTR(ATTR(e,call1),modes1);
 ob = ATTR(ATTR(e,call1),ob);
 if (ob==NULL) {
  goto other417;
 } else
 switch (TAG(ob)) {
  case AS_UND152986614_tag:
   i = 1; break;
  default: ;
  other417: ;
   i = 0;
 }
 L20 = (con_args==((ARRAYARG) NULL));
 L211_=!(L20); 
 if (L211_) {
  arg = ca;
  unbound_args = i;
  while (1) {
   L22 = (arg==((dAS_EXPR) NULL));
   L231_=!(L22); 
   if (L231_) {
   }
   else {
    goto after_loop;
   }
   atp = TRANS_68137339(self, arg);
   L24 = (atp==((dCALL_TP) NULL));
   L251_=!(L24); 
   if (L251_) {
    switch (TAG(atp)) {
     case CALL_T666389079_tag:
      L261_=INTPLUS(unbound_args,1); 
      unbound_args = L261_; break;
     default: ;
      FATAL("No applicable type in typecase\nin TRANS::bound_create_sig(AS_BOUND_CREATE_EXPR,$TP,$TP):SIG\n./Middle/trans.sa:2826:23");
    }
   }
   L27 = arg;
   arg = (*dAS_EX337431651[TAG(L27)])(L27);
  }
  after_loop: ;
  L281_=(con_args)==NULL?0:ASIZE((ARRAYARG)con_args); 
  L29 = L281_;
  L301_=(unbound_args)==(L29); 
  L32 = L301_;
  L331_=!(L32); 
  if (L331_) {
   L341_=(con_args)==NULL?0:ASIZE((ARRAYARG)con_args); 
   L35 = L341_;
   L361_=(unbound_args)<(L35); 
   if (L361_) {
    err_self1 = self;
    err_s1 = ((STR) &Toofew1923659380);
    if ((err_self1==((TRANS) NULL))) {
     L38 = TRUE;
    } else {
     L38 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L38) {
     L37 = TRUE;
    } else {
     L37 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L37) {
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
   else {
    err_self2 = self;
    err_s2 = ((STR) &Tooman1400108149);
    if ((err_self2==((TRANS) NULL))) {
     L40 = TRUE;
    } else {
     L40 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L40) {
     L39 = TRUE;
    } else {
     L39 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L39) {
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    else {
     plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val7;
     plus_sarg5 = err_s2;
     ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s2 = ret_val8;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
   }
   ret_val = ((SIG) NULL);
   return ret_val;
  }
 }
 {
  BOOL f_L411_ = TRUE;
  while (1) {
   L42 = (ca==((dAS_EXPR) NULL));
   L431_=!(L42); 
   if (L431_) {
   }
   else {
    goto after_loop1;
   }
   atp1 = TRANS_68137339(self, ca);
   if ((atp1==((dCALL_TP) NULL))) {
    L44 = TRANS_693526804(self, ca, ((dTP) NULL));
    atp1 = ((dCALL_TP) (*dAM_EXPR_tprdTP[TAG(L44)])(L44));
   }
   else {
    L45 = (con_args==((ARRAYARG) NULL));
    L461_=!(L45); 
    if (L461_) {
     if (atp1==NULL) {
      goto other418;
     } else
     switch (TAG(atp1)) {
      case CALL_T666389079_tag:
       if ((ATTR(((CALL_T666389079) atp1),tp)==((dTP) NULL))) {
        L471_=(ARG)ARR((ARRAYARG)con_args,i); 
        L48=L471_;
        SATTR(((CALL_T666389079) atp1),tp,ATTR(L48,tp));
       }
       L491_=INTPLUS(i,1); 
       i = L491_; break;
      default: ;
      other418: ;
     }
    }
   }
   if (f_L411_) {
    f_L411_ = FALSE;
    L11 = ATTR(call_sig,args);
    L411_br=L11==NULL?0:ASIZE((ARRAYCALL_ARG)L11); 
    i_L411_=0;
   }
   if(i_L411_>=L411_br)  goto after_loop1; 
   create_self2 = ((CALL_ARG) NULL);
   create_t = atp1;
   create_m = MODE_c998271775(((MODE) NULL), cm);
   L52=ZALLOC(sizeof(struct CALL_ARG_struct));
   if (L52==NULL) FATAL("Unable to allocate more memory");
   ((OB)L52)->header.tag=CALL_ARG_tag;
   L50 = ((CALL_ARG) L52);
   res = L50;
   SATTR(res,tp,create_t);
   SATTR(res,mode1,create_m);
   ret_val9 = res;
   L53 = ret_val9;
   SARR((ARRAYCALL_ARG)L11,i_L411_,(CALL_ARG)L53); i_L411_++;
   ;
   L54 = ca;
   ca = (*dAS_EX337431651[TAG(L54)])(L54);
   cm = ATTR(cm,next);
  }
 }
 after_loop1: ;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) e);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 L55 = self_tp1;
 s = CALL_S2120587481(call_sig, (*dTP_is242312711[TAG(L55)])(L55, ((OB) ATTR(ATTR(self,tp_con),same1))));
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TUPdAM_EXPRdTP TRANS_962473634(TRANS self, AS_BOU14269336 e) {
 TUPdAM_EXPRdTP ret_val = TUPdAM_EXPRdTP_zero;
 AS_CALL_EXPR call11;
 dAS_EXPR self_as;
 dAM_EXPR self_val = ((dAM_EXPR) NULL);
 AM_VOID_CONST res;
 AM_LOCAL_EXPR l;
 TRANS bound_create_sel;
 AS_BOU14269336 bound_create_sel1;
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
 TUPdAM_EXPRdTP create_self = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at1;
 dTP create_at2;
 TUPdAM_EXPRdTP ret_val3 = TUPdAM_EXPRdTP_zero;
 TRANS bound_create_sel2;
 AS_BOU14269336 bound_create_sel3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 TUPdAM_EXPRdTP create_self1 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at11;
 dTP create_at21;
 TUPdAM_EXPRdTP ret_val6 = TUPdAM_EXPRdTP_zero;
 TUPdAM_EXPRdTP create_self2 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at12;
 dTP create_at22;
 TUPdAM_EXPRdTP ret_val7 = TUPdAM_EXPRdTP_zero;
 TRANS bound_create_sel4;
 AS_BOU14269336 bound_create_sel5;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 TUPdAM_EXPRdTP create_self3 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at13;
 dTP create_at23;
 TUPdAM_EXPRdTP ret_val10 = TUPdAM_EXPRdTP_zero;
 TUPdAM_EXPRdTP create_self4 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at14;
 dTP create_at24;
 TUPdAM_EXPRdTP ret_val11 = TUPdAM_EXPRdTP_zero;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val12;
 TUPdAM_EXPRdTP create_self5 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at15;
 dTP create_at25;
 TUPdAM_EXPRdTP ret_val13 = TUPdAM_EXPRdTP_zero;
 TUPdAM_EXPRdTP create_self6 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at16;
 dTP create_at26;
 TUPdAM_EXPRdTP ret_val14 = TUPdAM_EXPRdTP_zero;
 AM_VOID_CONST create_self7;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VOID_CONST ret_val15;
 AM_VOID_CONST r;
 TRANS tp_of_self1;
 AS_TYPE_SPEC tp_of_t1;
 dTP ret_val16;
 TUPdAM_EXPRdTP create_self8 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at17;
 dTP create_at27;
 TUPdAM_EXPRdTP ret_val17 = TUPdAM_EXPRdTP_zero;
 TRANS bound_create_sel6;
 AS_BOU14269336 bound_create_sel7;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val18;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val19;
 TUPdAM_EXPRdTP create_self9 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at18;
 dTP create_at28;
 TUPdAM_EXPRdTP ret_val20 = TUPdAM_EXPRdTP_zero;
 AM_ROUT_DEF self_local_self;
 AM_LOCAL_EXPR ret_val21;
 TUPdAM_EXPRdTP create_self10 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at19;
 dTP create_at29;
 TUPdAM_EXPRdTP ret_val22 = TUPdAM_EXPRdTP_zero;
 BOOL L1;
 BOOL L21_;
 extern STR Boundc88095547;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 TUPdAM_EXPRdTP L5;
 TUPdAM_EXPRdTP L6;
 extern STR Boundc1977295681;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 TUPdAM_EXPRdTP L9;
 TUPdAM_EXPRdTP L10;
 dAM_EXPR L11;
 TUPdAM_EXPRdTP L12;
 TUPdAM_EXPRdTP L13;
 extern STR Boundc1895589567;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 TUPdAM_EXPRdTP L16;
 TUPdAM_EXPRdTP L17;
 TUPdAM_EXPRdTP L18;
 TUPdAM_EXPRdTP L19;
 TUPdAM_EXPRdTP L20;
 TUPdAM_EXPRdTP L22;
 dAM_EXPR L23;
 TUPdAM_EXPRdTP L24;
 TUPdAM_EXPRdTP L25;
 BOOL L26;
 BOOL L271_;
 AM_VOID_CONST L28;
 OB L29;
 TUPdAM_EXPRdTP L30;
 TUPdAM_EXPRdTP L31;
 BOOL L32;
 BOOL L331_;
 extern STR Boundc1055142586;
 BOOL L34;
 BOOL L35;
 extern STR name21;
 TUPdAM_EXPRdTP L36;
 TUPdAM_EXPRdTP L37;
 AM_FORMAL_ARG L381_;
 AM_FORMAL_ARG L39;
 dAM_EXPR L40;
 TUPdAM_EXPRdTP L41;
 TUPdAM_EXPRdTP L42;
 call11 = ATTR(e,call1);
 self_as = ATTR(call11,ob);
 L1 = (self_as==((dAS_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (self_as==NULL) {
   goto other415;
  } else
  switch (TAG(self_as)) {
   case AS_VOID_EXPR_tag:
    bound_create_sel = self;
    bound_create_sel1 = e;
    err_loc_self = bound_create_sel;
    err_loc_t = ((dPROG_ERR) bound_create_sel1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = bound_create_sel;
    err_s = ((STR) &Boundc88095547);
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
    create_self = TUPdAM_EXPRdTP_zero;
    create_at1 = ((dAM_EXPR) NULL);
    create_at2 = ((dTP) NULL);
    L5 = create_self;
    L5.t1 = create_at1;
    L6 = L5;
    L6.t2 = create_at2;
    ret_val3 = L6;
    ret_val = ret_val3;
    return ret_val; break;
   case AS_CREATE_EXPR_tag:
    if ((ATTR(((AS_CREATE_EXPR) self_as),tp)==((AS_TYPE_SPEC) NULL))) {
     bound_create_sel2 = self;
     bound_create_sel3 = e;
     err_loc_self1 = bound_create_sel2;
     err_loc_t1 = ((dPROG_ERR) bound_create_sel3);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     err_self1 = bound_create_sel2;
     err_s1 = ((STR) &Boundc1977295681);
     if ((err_self1==((TRANS) NULL))) {
      L8 = TRUE;
     } else {
      L8 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L8) {
      L7 = TRUE;
     } else {
      L7 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
     }
     if (L7) {
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     else {
      plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
      plus_sarg2 = ((STR) &name21);
      ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
      plus_self3 = ret_val4;
      plus_sarg3 = err_s1;
      ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
      err_s1 = ret_val5;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     create_self1 = TUPdAM_EXPRdTP_zero;
     create_at11 = ((dAM_EXPR) NULL);
     create_at21 = ((dTP) NULL);
     L9 = create_self1;
     L9.t1 = create_at11;
     L10 = L9;
     L10.t2 = create_at21;
     ret_val6 = L10;
     ret_val = ret_val6;
     return ret_val;
    }
    else {
     self_val = TRANS_693526804(self, self_as, ((dTP) NULL));
     create_self2 = TUPdAM_EXPRdTP_zero;
     create_at12 = self_val;
     L11 = self_val;
     create_at22 = (*dAM_EXPR_tprdTP[TAG(L11)])(L11);
     L12 = create_self2;
     L12.t1 = create_at12;
     L13 = L12;
     L13.t2 = create_at22;
     ret_val7 = L13;
     ret_val = ret_val7;
     return ret_val;
    } break;
   case AS_ARRAY_EXPR_tag:
    bound_create_sel4 = self;
    bound_create_sel5 = e;
    err_loc_self2 = bound_create_sel4;
    err_loc_t2 = ((dPROG_ERR) bound_create_sel5);
    PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
    err_self2 = bound_create_sel4;
    err_s2 = ((STR) &Boundc1895589567);
    if ((err_self2==((TRANS) NULL))) {
     L15 = TRUE;
    } else {
     L15 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L15) {
     L14 = TRUE;
    } else {
     L14 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L14) {
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    else {
     plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val8;
     plus_sarg5 = err_s2;
     ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s2 = ret_val9;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    create_self3 = TUPdAM_EXPRdTP_zero;
    create_at13 = ((dAM_EXPR) NULL);
    create_at23 = ((dTP) NULL);
    L16 = create_self3;
    L16.t1 = create_at13;
    L17 = L16;
    L17.t2 = create_at23;
    ret_val10 = L17;
    ret_val = ret_val10;
    return ret_val; break;
   case AS_UND152986614_tag:
    if ((ATTR(((AS_UND152986614) self_as),tp)==((AS_TYPE_SPEC) NULL))) {
     create_self4 = TUPdAM_EXPRdTP_zero;
     create_at14 = ((dAM_EXPR) NULL);
     create_at24 = ((dTP) NULL);
     L18 = create_self4;
     L18.t1 = create_at14;
     L19 = L18;
     L19.t2 = create_at24;
     ret_val11 = L19;
     ret_val = ret_val11;
     return ret_val;
    }
    else {
     create_self5 = TUPdAM_EXPRdTP_zero;
     create_at15 = ((dAM_EXPR) NULL);
     tp_of_self = self;
     tp_of_t = ATTR(((AS_UND152986614) self_as),tp);
     ret_val12 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
     create_at25 = ret_val12;
     L20 = create_self5;
     L20.t1 = create_at15;
     L22 = L20;
     L22.t2 = create_at25;
     ret_val13 = L22;
     ret_val = ret_val13;
     return ret_val;
    } break;
   default: ;
   other415: ;
    self_val = TRANS_693526804(self, self_as, ((dTP) NULL));
    create_self6 = TUPdAM_EXPRdTP_zero;
    create_at16 = self_val;
    L23 = self_val;
    create_at26 = (*dAM_EXPR_tprdTP[TAG(L23)])(L23);
    L24 = create_self6;
    L24.t1 = create_at16;
    L25 = L24;
    L25.t2 = create_at26;
    ret_val14 = L25;
    ret_val = ret_val14;
    return ret_val;
  }
 }
 else {
  L26 = (ATTR(call11,tp)==((AS_TYPE_SPEC) NULL));
  L271_=!(L26); 
  if (L271_) {
   create_self7 = ((AM_VOID_CONST) NULL);
   create_source = ATTR(call11,source1);
   L29=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=AM_VOID_CONST_tag;
   L28 = ((AM_VOID_CONST) L29);
   r = L28;
   SATTR(r,source1,create_source);
   ret_val15 = r;
   res = ret_val15;
   tp_of_self1 = self;
   tp_of_t1 = ATTR(call11,tp);
   ret_val16 = TP_CON1800432689(ATTR(tp_of_self1,tp_con), tp_of_t1);
   SATTR(res,tp_at,ret_val16);
   create_self8 = TUPdAM_EXPRdTP_zero;
   create_at17 = ((dAM_EXPR) res);
   create_at27 = ATTR(res,tp_at);
   L30 = create_self8;
   L30.t1 = create_at17;
   L31 = L30;
   L31.t2 = create_at27;
   ret_val17 = L31;
   ret_val = ret_val17;
   return ret_val;
  }
  else {
   if ((ATTR(call11,args)==((dAS_EXPR) NULL))) {
    l = TRANS_1711874080(self, ATTR(call11,name1));
    L32 = (l==((AM_LOCAL_EXPR) NULL));
    L331_=!(L32); 
    if (L331_) {
     bound_create_sel6 = self;
     bound_create_sel7 = e;
     err_loc_self3 = bound_create_sel6;
     err_loc_t3 = ((dPROG_ERR) bound_create_sel7);
     PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
     err_self3 = bound_create_sel6;
     err_s3 = ((STR) &Boundc1055142586);
     if ((err_self3==((TRANS) NULL))) {
      L35 = TRUE;
     } else {
      L35 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L35) {
      L34 = TRUE;
     } else {
      L34 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
     }
     if (L34) {
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
     else {
      plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
      plus_sarg6 = ((STR) &name21);
      ret_val18 = STR_ap2004550586(plus_self6, plus_sarg6);
      plus_self7 = ret_val18;
      plus_sarg7 = err_s3;
      ret_val19 = STR_ap2004550586(plus_self7, plus_sarg7);
      err_s3 = ret_val19;
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
     create_self9 = TUPdAM_EXPRdTP_zero;
     create_at18 = ((dAM_EXPR) NULL);
     create_at28 = ((dTP) NULL);
     L36 = create_self9;
     L36.t1 = create_at18;
     L37 = L36;
     L37.t2 = create_at28;
     ret_val20 = L37;
     ret_val = ret_val20;
     return ret_val;
    }
   }
   self_local_self = ATTR(self,cur_rout);
   L381_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)self_local_self,0); 
   L39=L381_;
   ret_val21 = ATTR(L39,expr);
   self_val = ((dAM_EXPR) ret_val21);
   create_self10 = TUPdAM_EXPRdTP_zero;
   create_at19 = self_val;
   L40 = self_val;
   create_at29 = (*dAM_EXPR_tprdTP[TAG(L40)])(L40);
   L41 = create_self10;
   L41.t1 = create_at19;
   L42 = L41;
   L42.t2 = create_at29;
   ret_val22 = L42;
   ret_val = ret_val22;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1006089529(TRANS self, AS_AND_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dAM_EXPR e1;
 dAM_EXPR e2;
 AM_IF_EXPR r;
 AM_BOOL_CONST abc;
 TRANS and_context_err_;
 AS_AND_EXPR and_context_err_1;
 dTP and_context_err_2;
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
 TRANS err_self;
 STR err_s;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 AM_IF_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_EXPR ret_val6;
 AM_IF_EXPR r1;
 AM_BOOL_CONST create_self1;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_BOOL_CONST ret_val7;
 AM_BOOL_CONST r2;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Andexp1703022664;
 extern STR notsubtypesof;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 BOOL L8;
 AM_IF_EXPR L9;
 OB L10;
 AM_BOOL_CONST L11;
 OB L12;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   and_context_err_ = self;
   and_context_err_1 = e;
   and_context_err_2 = tp;
   err_loc_self = and_context_err_;
   err_loc_t = ((dPROG_ERR) and_context_err_1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = and_context_err_;
   plus_self = ((STR) &Andexp1703022664);
   plus_sarg = ((STR) &notsubtypesof);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   L5 = and_context_err_2;
   plus_sarg1 = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val2;
   plus_sarg2 = ((STR) &name18);
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   err_s = ret_val3;
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
    plus_self3 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg3 = ((STR) &name21);
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val4;
    plus_sarg4 = err_s;
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    err_s = ret_val5;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 e1 = TRANS_693526804(self, ATTR(e,e1), ((dTP) TP_BUILTIN_bool));
 e2 = TRANS_693526804(self, ATTR(e,e2), ((dTP) TP_BUILTIN_bool));
 if ((e1==((dAM_EXPR) NULL))) {
  L8 = TRUE;
 } else {
  L8 = (e2==((dAM_EXPR) NULL));
 }
 if (L8) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_IF_EXPR) NULL);
 create_source = ATTR(e,source1);
 L10=ZALLOC(sizeof(struct AM_IF_EXPR_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_IF_EXPR_tag;
 L9 = ((AM_IF_EXPR) L10);
 r1 = L9;
 SATTR(r1,source1,create_source);
 ret_val6 = r1;
 r = ret_val6;
 SATTR(r,test1,e1);
 SATTR(r,if_true,e2);
 create_self1 = ((AM_BOOL_CONST) NULL);
 create_src = ATTR(e,source1);
 L12=ZALLOC(sizeof(struct AM_BOOL_CONST_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_BOOL_CONST_tag;
 L11 = ((AM_BOOL_CONST) L12);
 r2 = L11;
 SATTR(r2,source1,create_src);
 ret_val7 = r2;
 abc = ret_val7;
 SATTR(abc,val1,FALSE);
 SATTR(r,if_false,((dAM_EXPR) abc));
 SATTR(r,tp_at,((dTP) TP_BUILTIN_bool));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1168851617(TRANS self, AS_IS_VOID_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_IS_VOID_EXPR r;
 dAS_EXPR earg;
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
 AM_IS_VOID_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IS_VOID_EXPR ret_val5;
 AM_IS_VOID_EXPR r1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 TRANS err_self2;
 STR err_s2;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 TRANS err_self3;
 STR err_s3;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val10;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val11;
 TRANS err_self4;
 STR err_s4;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val12;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val13;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Voidte1174064897;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_IS_VOID_EXPR L8;
 OB L9;
 extern STR voidvo530295945;
 BOOL L10;
 BOOL L11;
 extern STR name21;
 extern STR voidon651986877;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 extern STR voidon2114174781;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 extern STR void_isillegal;
 BOOL L16;
 BOOL L17;
 extern STR name21;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   err_loc_self = self;
   err_loc_t = ((dPROG_ERR) e);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = self;
   plus_self = ((STR) &Voidte1174064897);
   L5 = tp;
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
 create_self = ((AM_IS_VOID_EXPR) NULL);
 create_source = ATTR(e,source1);
 L9=ZALLOC(sizeof(struct AM_IS_VOID_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_IS_VOID_EXPR_tag;
 L8 = ((AM_IS_VOID_EXPR) L9);
 r1 = L8;
 SATTR(r1,source1,create_source);
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,tp_at,((dTP) TP_BUILTIN_bool));
 err_loc_self1 = self;
 err_loc_t1 = ((dPROG_ERR) ATTR(e,arg));
 PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
 earg = ATTR(e,arg);
 if (earg==NULL) {
  goto other412;
 } else
 switch (TAG(earg)) {
  case AS_VOID_EXPR_tag:
   err_self1 = self;
   err_s1 = ((STR) &voidvo530295945);
   if ((err_self1==((TRANS) NULL))) {
    L11 = TRUE;
   } else {
    L11 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L11) {
    L10 = TRUE;
   } else {
    L10 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L10) {
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
   return ret_val; break;
  case AS_CREATE_EXPR_tag:
   if ((ATTR(((AS_CREATE_EXPR) earg),tp)==((AS_TYPE_SPEC) NULL))) {
    err_self2 = self;
    err_s2 = ((STR) &voidon651986877);
    if ((err_self2==((TRANS) NULL))) {
     L13 = TRUE;
    } else {
     L13 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L13) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L12) {
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    else {
     plus_self6 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val8;
     plus_sarg7 = err_s2;
     ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
     err_s2 = ret_val9;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   } break;
  case AS_ARRAY_EXPR_tag:
   err_self3 = self;
   err_s3 = ((STR) &voidon2114174781);
   if ((err_self3==((TRANS) NULL))) {
    L15 = TRUE;
   } else {
    L15 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L15) {
    L14 = TRUE;
   } else {
    L14 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L14) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val10;
    plus_sarg9 = err_s3;
    ret_val11 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s3 = ret_val11;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val; break;
  case AS_UND152986614_tag:
   err_self4 = self;
   err_s4 = ((STR) &void_isillegal);
   if ((err_self4==((TRANS) NULL))) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L17) {
    L16 = TRUE;
   } else {
    L16 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L16) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val12;
    plus_sarg11 = err_s4;
    ret_val13 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s4 = ret_val13;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val; break;
  default: ;
  other412: ;
 }
 SATTR(r,arg,TRANS_693526804(self, ATTR(e,arg), ((dTP) NULL)));
 if ((ATTR(r,arg)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1173176875(TRANS self, AS_INT_LIT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_INTI_CONST ri;
 AM_INT_CONST r;
 AM_INTI_CONST create_self;
 AS_INT_LIT_EXPR create_t;
 AM_INTI_CONST ret_val1;
 AM_INTI_CONST r1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 AM_INT_CONST create_self1;
 AS_INT_LIT_EXPR create_t1;
 AM_INT_CONST ret_val6;
 AM_INT_CONST r2;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val10;
 AM_INTI_CONST L1;
 OB L2;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 extern STR Thetyp857837218;
 dTP L6;
 extern STR isnota1914231676;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 AM_INT_CONST L9;
 OB L10;
 dTP L11;
 BOOL L12;
 BOOL L131_;
 extern STR Thetyp857837218;
 dTP L14;
 extern STR isnota1029505609;
 BOOL L15;
 BOOL L16;
 extern STR name21;
 if (ATTR(e,is_inti)) {
  create_self = ((AM_INTI_CONST) NULL);
  create_t = e;
  L2=ZALLOC(sizeof(struct AM_INTI_CONST_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=AM_INTI_CONST_tag;
  L1 = ((AM_INTI_CONST) L2);
  r1 = L1;
  SATTR(r1,source1,ATTR(create_t,source1));
  SATTR(r1,val1,ATTR(create_t,val1));
  ret_val1 = r1;
  ri = ret_val1;
  SATTR(ri,tp_at,((dTP) TP_BUILTIN_inti));
  if ((tp==((dTP) NULL))) {
   ret_val = ((dAM_EXPR) ri);
   return ret_val;
  }
  else {
   L3 = ATTR(ri,tp_at);
   L4 = (*dTP_is1999456142[TAG(L3)])(L3, tp);
   L51_=!(L4); 
   if (L51_) {
    err_loc_self = self;
    err_loc_t = ((dPROG_ERR) e);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = self;
    plus_self = ((STR) &Thetyp857837218);
    L6 = tp;
    plus_sarg = (*dTP_strrSTR[TAG(L6)])(L6);
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val2;
    plus_sarg1 = ((STR) &isnota1914231676);
    ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
    err_s = ret_val3;
    if ((err_self==((TRANS) NULL))) {
     L8 = TRUE;
    } else {
     L8 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L8) {
     L7 = TRUE;
    } else {
     L7 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
    }
    if (L7) {
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    else {
     plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
     plus_sarg2 = ((STR) &name21);
     ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val4;
     plus_sarg3 = err_s;
     ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
     err_s = ret_val5;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   else {
    ret_val = ((dAM_EXPR) ri);
    return ret_val;
   }
  }
 }
 else {
  create_self1 = ((AM_INT_CONST) NULL);
  create_t1 = e;
  L10=ZALLOC(sizeof(struct AM_INT_CONST_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=AM_INT_CONST_tag;
  L9 = ((AM_INT_CONST) L10);
  r2 = L9;
  SATTR(r2,source1,ATTR(create_t1,source1));
  SATTR(r2,val1,ATTR(create_t1,val1));
  ret_val6 = r2;
  r = ret_val6;
  SATTR(r,tp_at,((dTP) TP_BUILTIN_int));
  if ((tp==((dTP) NULL))) {
   ret_val = ((dAM_EXPR) r);
   return ret_val;
  }
  else {
   L11 = ATTR(r,tp_at);
   L12 = (*dTP_is1999456142[TAG(L11)])(L11, tp);
   L131_=!(L12); 
   if (L131_) {
    err_loc_self1 = self;
    err_loc_t1 = ((dPROG_ERR) e);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self1 = self;
    plus_self4 = ((STR) &Thetyp857837218);
    L14 = tp;
    plus_sarg4 = (*dTP_strrSTR[TAG(L14)])(L14);
    ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val7;
    plus_sarg5 = ((STR) &isnota1029505609);
    ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s1 = ret_val8;
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
     plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val9 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val9;
     plus_sarg7 = err_s1;
     ret_val10 = STR_ap2004550586(plus_self7, plus_sarg7);
     err_s1 = ret_val10;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   else {
    ret_val = ((dAM_EXPR) r);
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1269501442(TRANS self, AS_CHAR_LIT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_CHAR_CONST r;
 TRANS char_lit_context;
 AS_CHAR_LIT_EXPR char_lit_context1;
 dTP char_lit_context2;
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
 AM_CHAR_CONST create_self;
 AS_CHAR_LIT_EXPR create_t;
 AM_CHAR_CONST ret_val5;
 AM_CHAR_CONST r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Charac693849448;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_CHAR_CONST L8;
 OB L9;
 INT L10;
 CHAR L111_;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_char, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   char_lit_context = self;
   char_lit_context1 = e;
   char_lit_context2 = tp;
   err_loc_self = char_lit_context;
   err_loc_t = ((dPROG_ERR) char_lit_context1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = char_lit_context;
   plus_self = ((STR) &Charac693849448);
   L5 = char_lit_context2;
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
 create_self = ((AM_CHAR_CONST) NULL);
 create_t = e;
 L9=ZALLOC(sizeof(struct AM_CHAR_CONST_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_CHAR_CONST_tag;
 L8 = ((AM_CHAR_CONST) L9);
 r1 = L8;
 SATTR(r1,source1,ATTR(create_t,source1));
 L10 = ATTR(create_t,val1);
 L111_=(CHAR)L10; 
 SATTR(r1,bval,L111_);
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,tp_at,((dTP) TP_BUILTIN_char));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1346628084(TRANS self, AS_RESULT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_LOCAL_EXPR r;
 TRANS result_out_of_po;
 AS_RESULT_EXPR result_out_of_po1;
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
 TRANS result_in_initia;
 AS_RESULT_EXPR result_in_initia1;
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
 TRANS result_and_no_re;
 AS_RESULT_EXPR result_and_no_re1;
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
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val7;
 AM_LOCAL_EXPR r1;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val8;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val9;
 TRANS result_context_e;
 AS_RESULT_EXPR result_context_e1;
 dTP result_context_e2;
 dTP result_context_e3;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 TRANS err_self3;
 STR err_s3;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val14;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val15;
 BOOL L1;
 BOOL L21_;
 extern STR result202247682;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 BOOL L5;
 BOOL L61_;
 extern STR result1689554699;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 extern STR result1629600363;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 AM_LOCAL_EXPR L11;
 OB L12;
 BOOL L13;
 BOOL L141_;
 dTP L15;
 BOOL L16;
 BOOL L171_;
 extern STR Thetyp849634942;
 dTP L18;
 extern STR isnotasubtypeof;
 dTP L19;
 extern STR name18;
 BOOL L20;
 BOOL L22;
 extern STR name21;
 L1 = ATTR(self,in_post);
 L21_=L1==FALSE; 
 if (L21_) {
  result_out_of_po = self;
  result_out_of_po1 = e;
  err_loc_self = result_out_of_po;
  err_loc_t = ((dPROG_ERR) result_out_of_po1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = result_out_of_po;
  err_s = ((STR) &result202247682);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L5 = ATTR(self,in_initial);
 L61_=L5==TRUE; 
 if (L61_) {
  result_in_initia = self;
  result_in_initia1 = e;
  err_loc_self1 = result_in_initia;
  err_loc_t1 = ((dPROG_ERR) result_in_initia1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = result_in_initia;
  err_s1 = ((STR) &result1689554699);
  if ((err_self1==((TRANS) NULL))) {
   L8 = TRUE;
  } else {
   L8 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L8) {
   L7 = TRUE;
  } else {
   L7 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L7) {
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if ((ATTR(ATTR(self,cur_rout),rres)==((AM_LOCAL_EXPR) NULL))) {
  if ((ATTR(ATTR(ATTR(self,cur_rout),sig1),ret)==((dTP) NULL))) {
   result_and_no_re = self;
   result_and_no_re1 = e;
   err_loc_self2 = result_and_no_re;
   err_loc_t2 = ((dPROG_ERR) result_and_no_re1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = result_and_no_re;
   err_s2 = ((STR) &result1629600363);
   if ((err_self2==((TRANS) NULL))) {
    L10 = TRUE;
   } else {
    L10 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L10) {
    L9 = TRUE;
   } else {
    L9 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L9) {
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
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
  create_self = ((AM_LOCAL_EXPR) NULL);
  create_src = ATTR(e,source1);
  create_name = IDENT_zero;
  create_tp = ATTR(ATTR(ATTR(self,cur_rout),sig1),ret);
  L12=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=AM_LOCAL_EXPR_tag;
  L11 = ((AM_LOCAL_EXPR) L12);
  r1 = L11;
  SATTR(r1,source1,create_src);
  SATTR(r1,name1,create_name);
  SATTR(r1,tp_at,create_tp);
  ret_val7 = r1;
  SATTR(ATTR(self,cur_rout),rres,ret_val7);
 }
 L13 = (tp==((dTP) NULL));
 L141_=!(L13); 
 if (L141_) {
  tp_self = ATTR(ATTR(self,cur_rout),rres);
  ret_val8 = ATTR(tp_self,tp_at);
  L15 = ret_val8;
  L16 = (*dTP_is1999456142[TAG(L15)])(L15, tp);
  L171_=!(L16); 
  if (L171_) {
   result_context_e = self;
   result_context_e1 = e;
   tp_self1 = ATTR(ATTR(self,cur_rout),rres);
   ret_val9 = ATTR(tp_self1,tp_at);
   result_context_e2 = ret_val9;
   result_context_e3 = tp;
   err_loc_self3 = result_context_e;
   err_loc_t3 = ((dPROG_ERR) result_context_e1);
   PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
   err_self3 = result_context_e;
   plus_self6 = ((STR) &Thetyp849634942);
   L18 = result_context_e2;
   plus_sarg6 = (*dTP_strrSTR[TAG(L18)])(L18);
   ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val10;
   plus_sarg7 = ((STR) &isnotasubtypeof);
   ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
   plus_self8 = ret_val11;
   L19 = result_context_e3;
   plus_sarg8 = (*dTP_strrSTR[TAG(L19)])(L19);
   ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val12;
   plus_sarg9 = ((STR) &name18);
   ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
   err_s3 = ret_val13;
   if ((err_self3==((TRANS) NULL))) {
    L22 = TRUE;
   } else {
    L22 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L22) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L20) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val14 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val14;
    plus_sarg11 = err_s3;
    ret_val15 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s3 = ret_val15;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 r = ATTR(ATTR(self,cur_rout),rres);
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1475346849(TRANS self, AS_FLT_LIT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 INT L11 = INT_zero;
 AM_FLT_CONST rf;
 AM_FLTD_CONST rfd;
 AM_FLTX_CONST rfx;
 AM_FLTDX_CONST rfdx;
 AM_FLTI_CONST rfi;
 AM_FLT_CONST create_self;
 AS_FLT_LIT_EXPR create_t;
 AM_FLT_CONST ret_val1;
 AM_FLT_CONST r;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 TRANS err_self;
 STR err_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 AM_FLTD_CONST create_self1;
 AS_FLT_LIT_EXPR create_t1;
 AM_FLTD_CONST ret_val6;
 AM_FLTD_CONST r1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val10;
 AM_FLTX_CONST create_self2;
 AS_FLT_LIT_EXPR create_t2;
 AM_FLTX_CONST ret_val11;
 AM_FLTX_CONST r2;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 TRANS err_self2;
 STR err_s2;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val14;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val15;
 AM_FLTDX_CONST create_self3;
 AS_FLT_LIT_EXPR create_t3;
 AM_FLTDX_CONST ret_val16;
 AM_FLTDX_CONST r3;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val18;
 TRANS err_self3;
 STR err_s3;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val19;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val20;
 AM_FLTI_CONST create_self4;
 AS_FLT_LIT_EXPR create_t4;
 AM_FLTI_CONST ret_val21;
 AM_FLTI_CONST r4;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val22;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val23;
 TRANS err_self4;
 STR err_s4;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val24;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val25;
 AM_FLT_CONST L2;
 OB L3;
 dTP L4;
 BOOL L5;
 BOOL L61_;
 extern STR Thetyp857837218;
 dTP L7;
 extern STR isnota1029526908;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 AM_FLTD_CONST L10;
 OB L12;
 dTP L13;
 BOOL L14;
 BOOL L151_;
 extern STR Thetyp857837218;
 dTP L16;
 extern STR isnota1913826900;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 AM_FLTX_CONST L19;
 OB L20;
 dTP L21;
 BOOL L22;
 BOOL L231_;
 extern STR Thetyp857837218;
 dTP L24;
 extern STR isnota1913827280;
 BOOL L25;
 BOOL L26;
 extern STR name21;
 AM_FLTDX_CONST L27;
 OB L28;
 dTP L29;
 BOOL L30;
 BOOL L311_;
 extern STR Thetyp857837218;
 dTP L32;
 extern STR isnota2002974803;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 AM_FLTI_CONST L35;
 OB L36;
 dTP L37;
 BOOL L38;
 BOOL L391_;
 extern STR Thetyp857837218;
 dTP L40;
 extern STR isnota1913826995;
 BOOL L41;
 BOOL L42;
 extern STR name21;
 L11 = ATTR(e,tp);
 switch (L11) {
  case 0: 
   create_self = ((AM_FLT_CONST) NULL);
   create_t = e;
   L3=ZALLOC(sizeof(struct AM_FLT_CONST_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=AM_FLT_CONST_tag;
   L2 = ((AM_FLT_CONST) L3);
   r = L2;
   SATTR(r,source1,ATTR(create_t,source1));
   SATTR(r,val1,ATTR(create_t,val1));
   ret_val1 = r;
   rf = ret_val1;
   SATTR(rf,tp_at,((dTP) TP_BUILTIN_flt));
   if ((tp==((dTP) NULL))) {
    ret_val = ((dAM_EXPR) rf);
    return ret_val;
   }
   else {
    L4 = ATTR(rf,tp_at);
    L5 = (*dTP_is1999456142[TAG(L4)])(L4, tp);
    L61_=!(L5); 
    if (L61_) {
     err_loc_self = self;
     err_loc_t = ((dPROG_ERR) e);
     PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
     err_self = self;
     plus_self = ((STR) &Thetyp857837218);
     L7 = tp;
     plus_sarg = (*dTP_strrSTR[TAG(L7)])(L7);
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val2;
     plus_sarg1 = ((STR) &isnota1029526908);
     ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
     err_s = ret_val3;
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
      plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
      plus_sarg2 = ((STR) &name21);
      ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
      plus_self3 = ret_val4;
      plus_sarg3 = err_s;
      ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
      err_s = ret_val5;
      PROG_err_STR(ATTR(err_self,prog), err_s);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    else {
     ret_val = ((dAM_EXPR) rf);
     return ret_val;
    }
   }
   break;
  case 1: 
   create_self1 = ((AM_FLTD_CONST) NULL);
   create_t1 = e;
   L12=ZALLOC(sizeof(struct AM_FLTD_CONST_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=AM_FLTD_CONST_tag;
   L10 = ((AM_FLTD_CONST) L12);
   r1 = L10;
   SATTR(r1,source1,ATTR(create_t1,source1));
   SATTR(r1,val1,ATTR(create_t1,val1));
   ret_val6 = r1;
   rfd = ret_val6;
   SATTR(rfd,tp_at,((dTP) TP_BUILTIN_fltd));
   if ((tp==((dTP) NULL))) {
    ret_val = ((dAM_EXPR) rfd);
    return ret_val;
   }
   else {
    L13 = ATTR(rfd,tp_at);
    L14 = (*dTP_is1999456142[TAG(L13)])(L13, tp);
    L151_=!(L14); 
    if (L151_) {
     err_loc_self1 = self;
     err_loc_t1 = ((dPROG_ERR) e);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     err_self1 = self;
     plus_self4 = ((STR) &Thetyp857837218);
     L16 = tp;
     plus_sarg4 = (*dTP_strrSTR[TAG(L16)])(L16);
     ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val7;
     plus_sarg5 = ((STR) &isnota1913826900);
     ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
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
      plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
      plus_sarg6 = ((STR) &name21);
      ret_val9 = STR_ap2004550586(plus_self6, plus_sarg6);
      plus_self7 = ret_val9;
      plus_sarg7 = err_s1;
      ret_val10 = STR_ap2004550586(plus_self7, plus_sarg7);
      err_s1 = ret_val10;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    else {
     ret_val = ((dAM_EXPR) rfd);
     return ret_val;
    }
   }
   break;
  case 2: 
   create_self2 = ((AM_FLTX_CONST) NULL);
   create_t2 = e;
   L20=ZALLOC(sizeof(struct AM_FLTX_CONST_struct));
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=AM_FLTX_CONST_tag;
   L19 = ((AM_FLTX_CONST) L20);
   r2 = L19;
   SATTR(r2,source1,ATTR(create_t2,source1));
   SATTR(r2,val1,ATTR(create_t2,val1));
   ret_val11 = r2;
   rfx = ret_val11;
   SATTR(rfx,tp_at,((dTP) TP_BUILTIN_fltx));
   if ((tp==((dTP) NULL))) {
    ret_val = ((dAM_EXPR) rfx);
    return ret_val;
   }
   else {
    L21 = ATTR(rfx,tp_at);
    L22 = (*dTP_is1999456142[TAG(L21)])(L21, tp);
    L231_=!(L22); 
    if (L231_) {
     err_loc_self2 = self;
     err_loc_t2 = ((dPROG_ERR) e);
     PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
     err_self2 = self;
     plus_self8 = ((STR) &Thetyp857837218);
     L24 = tp;
     plus_sarg8 = (*dTP_strrSTR[TAG(L24)])(L24);
     ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
     plus_self9 = ret_val12;
     plus_sarg9 = ((STR) &isnota1913827280);
     ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
     err_s2 = ret_val13;
     if ((err_self2==((TRANS) NULL))) {
      L26 = TRUE;
     } else {
      L26 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L26) {
      L25 = TRUE;
     } else {
      L25 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
     }
     if (L25) {
      PROG_err_STR(ATTR(err_self2,prog), err_s2);
     }
     else {
      plus_self10 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
      plus_sarg10 = ((STR) &name21);
      ret_val14 = STR_ap2004550586(plus_self10, plus_sarg10);
      plus_self11 = ret_val14;
      plus_sarg11 = err_s2;
      ret_val15 = STR_ap2004550586(plus_self11, plus_sarg11);
      err_s2 = ret_val15;
      PROG_err_STR(ATTR(err_self2,prog), err_s2);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    else {
     ret_val = ((dAM_EXPR) rfx);
     return ret_val;
    }
   }
   break;
  case 3: 
   create_self3 = ((AM_FLTDX_CONST) NULL);
   create_t3 = e;
   L28=ZALLOC(sizeof(struct AM_FLTDX_CONST_struct));
   if (L28==NULL) FATAL("Unable to allocate more memory");
   ((OB)L28)->header.tag=AM_FLTDX_CONST_tag;
   L27 = ((AM_FLTDX_CONST) L28);
   r3 = L27;
   SATTR(r3,source1,ATTR(create_t3,source1));
   SATTR(r3,val1,ATTR(create_t3,val1));
   ret_val16 = r3;
   rfdx = ret_val16;
   SATTR(rfdx,tp_at,((dTP) TP_BUILTIN_fltdx));
   if ((tp==((dTP) NULL))) {
    ret_val = ((dAM_EXPR) rfdx);
    return ret_val;
   }
   else {
    L29 = ATTR(rfdx,tp_at);
    L30 = (*dTP_is1999456142[TAG(L29)])(L29, tp);
    L311_=!(L30); 
    if (L311_) {
     err_loc_self3 = self;
     err_loc_t3 = ((dPROG_ERR) e);
     PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
     err_self3 = self;
     plus_self12 = ((STR) &Thetyp857837218);
     L32 = tp;
     plus_sarg12 = (*dTP_strrSTR[TAG(L32)])(L32);
     ret_val17 = STR_ap2004550586(plus_self12, plus_sarg12);
     plus_self13 = ret_val17;
     plus_sarg13 = ((STR) &isnota2002974803);
     ret_val18 = STR_ap2004550586(plus_self13, plus_sarg13);
     err_s3 = ret_val18;
     if ((err_self3==((TRANS) NULL))) {
      L34 = TRUE;
     } else {
      L34 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L34) {
      L33 = TRUE;
     } else {
      L33 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
     }
     if (L33) {
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
     else {
      plus_self14 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
      plus_sarg14 = ((STR) &name21);
      ret_val19 = STR_ap2004550586(plus_self14, plus_sarg14);
      plus_self15 = ret_val19;
      plus_sarg15 = err_s3;
      ret_val20 = STR_ap2004550586(plus_self15, plus_sarg15);
      err_s3 = ret_val20;
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    else {
     ret_val = ((dAM_EXPR) rfdx);
     return ret_val;
    }
   }
   break;
  case 4: 
   create_self4 = ((AM_FLTI_CONST) NULL);
   create_t4 = e;
   L36=ZALLOC(sizeof(struct AM_FLTI_CONST_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   ((OB)L36)->header.tag=AM_FLTI_CONST_tag;
   L35 = ((AM_FLTI_CONST) L36);
   r4 = L35;
   SATTR(r4,source1,ATTR(create_t4,source1));
   SATTR(r4,val1,ATTR(create_t4,val1));
   ret_val21 = r4;
   rfi = ret_val21;
   SATTR(rfi,tp_at,((dTP) TP_BUILTIN_flti));
   if ((tp==((dTP) NULL))) {
    ret_val = ((dAM_EXPR) rfi);
    return ret_val;
   }
   else {
    L37 = ATTR(rfi,tp_at);
    L38 = (*dTP_is1999456142[TAG(L37)])(L37, tp);
    L391_=!(L38); 
    if (L391_) {
     err_loc_self4 = self;
     err_loc_t4 = ((dPROG_ERR) e);
     PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
     err_self4 = self;
     plus_self16 = ((STR) &Thetyp857837218);
     L40 = tp;
     plus_sarg16 = (*dTP_strrSTR[TAG(L40)])(L40);
     ret_val22 = STR_ap2004550586(plus_self16, plus_sarg16);
     plus_self17 = ret_val22;
     plus_sarg17 = ((STR) &isnota1913826995);
     ret_val23 = STR_ap2004550586(plus_self17, plus_sarg17);
     err_s4 = ret_val23;
     if ((err_self4==((TRANS) NULL))) {
      L42 = TRUE;
     } else {
      L42 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L42) {
      L41 = TRUE;
     } else {
      L41 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
     }
     if (L41) {
      PROG_err_STR(ATTR(err_self4,prog), err_s4);
     }
     else {
      plus_self18 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
      plus_sarg18 = ((STR) &name21);
      ret_val24 = STR_ap2004550586(plus_self18, plus_sarg18);
      plus_self19 = ret_val24;
      plus_sarg19 = err_s4;
      ret_val25 = STR_ap2004550586(plus_self19, plus_sarg19);
      err_s4 = ret_val25;
      PROG_err_STR(ATTR(err_self4,prog), err_s4);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    else {
     ret_val = ((dAM_EXPR) rfi);
     return ret_val;
    }
   }
   break;
  default: ;
   FATAL("No applicable target in case statement\nin TRANS::transform_flt_lit_expr(AS_FLT_LIT_EXPR,$TP):$AM_EXPR\n./Middle/trans.sa:3327:26");
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1546520066(TRANS self, AS_BOU14269336 e, dTP tp) {
 dAM_EXPR ret_val;
 TUPdAM_EXPRdTP st = TUPdAM_EXPRdTP_zero;
 dAM_EXPR self_val;
 dTP self_tp1;
 INT nbnd = INT_zero;
 AM_BND1124877163 r;
 INT b_ind = INT_zero;
 BOOL hot = BOOL_zero;
 dAS_EXPR a;
 AS_ARG_MODE m;
 ARRAYARG L11;
 dTP atp;
 ARRAYBOOL L21;
 TRANS bound_create_in_;
 AS_BOU14269336 bound_create_in_1;
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
 AM_BND1124877163 create_self;
 INT create_n = INT_zero;
 AM_BND1124877163 ret_val3;
 SIG is_iter_self;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 TRANS bound_create_not;
 AS_BOU14269336 bound_create_not1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 SIG is_iter_self2;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 TRANS bound_create_ite;
 AS_BOU14269336 bound_create_ite1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val11;
 AM_CALL_ARG create_self1;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val12;
 AM_CALL_ARG res;
 TRANS bound_create_sel;
 AS_BOU14269336 bound_create_sel1;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val14;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e1;
 dMODE create_m;
 AM_CALL_ARG ret_val15;
 AM_CALL_ARG res1;
 TRANS bound_create_ite2;
 dAS_EXPR bound_create_ite3;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 TRANS err_self4;
 STR err_s4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val16;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val17;
 AM_BND1124877163 tp_self;
 dTP ret_val18;
 AM_BND1124877163 tp_self1;
 dTP ret_val19;
 TRANS bound_create_con;
 AS_BOU14269336 bound_create_con1;
 dTP bound_create_con2;
 dTP bound_create_con3;
 TRANS err_loc_self5;
 dPROG_ERR err_loc_t5;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val20;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val21;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val22;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val23;
 TRANS err_self5;
 STR err_s5;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val25;
 AM_BND1124877163 tp_self2;
 dTP ret_val26;
 TRANS f_rout_create_si;
 AS_BOU14269336 f_rout_create_si1;
 TRANS err_loc_self6;
 dPROG_ERR err_loc_t6;
 TRANS err_self6;
 STR err_s6;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val27;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val28;
 TRANS f_rout_create_bo;
 AS_BOU14269336 f_rout_create_bo1;
 TRANS err_loc_self7;
 dPROG_ERR err_loc_t7;
 TRANS err_self7;
 STR err_s7;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val29;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val30;
 extern STR Boundc1335562444;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 AM_BND1124877163 L5;
 INT L6;
 OB L7;
 INT L8;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 BOOL L131_;
 STR L14;
 INT L15;
 INT L161_;
 INT L17;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 extern STR Boundi1405270156;
 BOOL L24;
 BOOL L25;
 extern STR name21;
 BOOL L26;
 BOOL L27;
 BOOL L281_;
 BOOL L29;
 BOOL L30;
 BOOL L311_;
 STR L32;
 INT L33;
 INT L341_;
 INT L35;
 CHAR L361_;
 CHAR L37;
 BOOL L381_;
 extern STR Boundr2070341545;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 BOOL L41;
 BOOL L421_;
 AM_CALL_ARG L43;
 OB L44;
 AM_CALL_ARG L45;
 INT L471_;
 BOOL L48;
 extern STR Theexp628757468;
 BOOL L49;
 BOOL L50;
 extern STR name21;
 BOOL L53;
 BOOL L541_;
 ARG L55;
 INT L511_br;
INT i_L511_=0;
 ARG aL511_;
 ARG L56;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 INT L521_br;
INT i_L521_=0;
 BOOL aL521_;
 AM_CALL_ARG L60;
 OB L61;
 AM_CALL_ARG L62;
 AM_CALL_ARG L641_;
 AM_CALL_ARG L65;
 BOOL L66;
 BOOL L67;
 BOOL L681_;
 AM_CALL_ARG L691_;
 AM_CALL_ARG L70;
 extern STR Oncear1787080910;
 BOOL L71;
 BOOL L72;
 extern STR name21;
 INT L731_;
 dAS_EXPR L74;
 BOOL L75;
 BOOL L76;
 BOOL L771_;
 dTP L78;
 BOOL L79;
 BOOL L801_;
 extern STR Thetyp1564090783;
 dTP L81;
 extern STR isnotasubtypeof;
 dTP L82;
 extern STR name18;
 BOOL L83;
 BOOL L84;
 extern STR name21;
 dTP L85;
 BOOL L86;
 BOOL L871_;
 extern STR Illega1957842819;
 BOOL L88;
 BOOL L89;
 extern STR name21;
 ARRAYINT L90;
 INT L911_;
 INT L92;
 BOOL L931_;
 BOOL L94;
 ARRAYINT L95;
 INT L961_;
 INT L97;
 BOOL L981_;
 ARRAYINT L99;
 INT L1001_;
 INT L101;
 BOOL L1021_;
 BOOL L103;
 BOOL L1041_;
 extern STR Allarg773350235;
 BOOL L105;
 BOOL L106;
 extern STR name21;
 if (ATTR(self,in_constant)) {
  bound_create_in_ = self;
  bound_create_in_1 = e;
  err_loc_self = bound_create_in_;
  err_loc_t = ((dPROG_ERR) bound_create_in_1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = bound_create_in_;
  err_s = ((STR) &Boundc1335562444);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 st = TRANS_962473634(self, e);
 self_val = st.t1;
 self_tp1 = st.t2;
 nbnd = TRANS_844294361(self, self_val, e);
 create_self = ((AM_BND1124877163) NULL);
 create_n = nbnd;
 L6 = create_n;
 L8=(sizeof(struct AM_BND1124877163_struct)-sizeof(AM_CALL_ARG))+(L6)*sizeof(AM_CALL_ARG);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_BND1124877163_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((AM_BND1124877163) L7);
 L5->asize = L6;
 ret_val3 = L5;
 r = ret_val3;
 SATTR(r,fun,TRANS_2093630221(self, e, self_tp1, tp));
 if ((ATTR(r,fun)==((SIG) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if (ATTR(e,is_iter)) {
  is_iter_self = ATTR(r,fun);
  is_iter_self1 = ATTR(is_iter_self,name1);
  L12 = (is_iter_self1.str==((STR) NULL));
  L131_=!(L12); 
  if (L131_) {
   L14 = is_iter_self1.str;
   L15 = STR_sizerINT(is_iter_self1.str);
   L161_=INTMINUS(L15,1); 
   L17 = L161_;
   L181_=ARR((STR)L14,L17); 
   L19 = L181_;
   L201_=L19=='!'; 
   L10 = L201_;
  } else {
   L10 = FALSE;
  }
  ret_val5 = L10;
  ret_val4 = ret_val5;
  L22 = ret_val4;
  L231_=!(L22); 
  L9 = L231_;
 } else {
  L9 = FALSE;
 }
 if (L9) {
  bound_create_not = self;
  bound_create_not1 = e;
  err_loc_self1 = bound_create_not;
  err_loc_t1 = ((dPROG_ERR) bound_create_not1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = bound_create_not;
  err_s1 = ((STR) &Boundi1405270156);
  if ((err_self1==((TRANS) NULL))) {
   L25 = TRUE;
  } else {
   L25 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L25) {
   L24 = TRUE;
  } else {
   L24 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L24) {
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  else {
   plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
   plus_sarg2 = ((STR) &name21);
   ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val6;
   plus_sarg3 = err_s1;
   ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s1 = ret_val7;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 else {
  L27 = ATTR(e,is_iter);
  L281_=!(L27); 
  if (L281_) {
   is_iter_self2 = ATTR(r,fun);
   is_iter_self3 = ATTR(is_iter_self2,name1);
   L30 = (is_iter_self3.str==((STR) NULL));
   L311_=!(L30); 
   if (L311_) {
    L32 = is_iter_self3.str;
    L33 = STR_sizerINT(is_iter_self3.str);
    L341_=INTMINUS(L33,1); 
    L35 = L341_;
    L361_=ARR((STR)L32,L35); 
    L37 = L361_;
    L381_=L37=='!'; 
    L29 = L381_;
   } else {
    L29 = FALSE;
   }
   ret_val9 = L29;
   ret_val8 = ret_val9;
   L26 = ret_val8;
  } else {
   L26 = FALSE;
  }
  if (L26) {
   bound_create_ite = self;
   bound_create_ite1 = e;
   err_loc_self2 = bound_create_ite;
   err_loc_t2 = ((dPROG_ERR) bound_create_ite1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = bound_create_ite;
   err_s2 = ((STR) &Boundr2070341545);
   if ((err_self2==((TRANS) NULL))) {
    L40 = TRUE;
   } else {
    L40 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L40) {
    L39 = TRUE;
   } else {
    L39 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L39) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg4 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val10;
    plus_sarg5 = err_s2;
    ret_val11 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val11;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 SATTR(r,bnd_args,TRANS_80510940(self, nbnd, e));
 SATTR(r,unbnd_args,TRANS_2014167366(self, nbnd, e));
 b_ind = 0;
 L41 = (self_val==((dAM_EXPR) NULL));
 L421_=!(L41); 
 if (L421_) {
  create_self1 = ((AM_CALL_ARG) NULL);
  create_e = self_val;
  L44=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=AM_CALL_ARG_tag;
  L43 = ((AM_CALL_ARG) L44);
  res = L43;
  SATTR(res,expr,create_e);
  SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val12 = res;
  L45 = ret_val12;
  SARR((AM_BND1124877163)r,b_ind,(AM_CALL_ARG)L45); 
  ;
  L471_=INTPLUS(b_ind,1); 
  b_ind = L471_;
  if (ATTR(e,is_iter)) {
   L48 = TRANS_1865855576(self, self_val);
  } else {
   L48 = FALSE;
  }
  if (L48) {
   bound_create_sel = self;
   bound_create_sel1 = e;
   err_loc_self3 = bound_create_sel;
   err_loc_t3 = ((dPROG_ERR) bound_create_sel1);
   PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
   err_self3 = bound_create_sel;
   err_s3 = ((STR) &Theexp628757468);
   if ((err_self3==((TRANS) NULL))) {
    L50 = TRUE;
   } else {
    L50 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L50) {
    L49 = TRUE;
   } else {
    L49 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L49) {
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
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 a = ATTR(ATTR(e,call1),args);
 m = ATTR(ATTR(e,call1),modes1);
 {
  BOOL f_L511_ = TRUE;
  BOOL f_L521_ = TRUE;
  while (1) {
   L53 = (a==((dAS_EXPR) NULL));
   L541_=!(L53); 
   if (L541_) {
   }
   else {
    goto after_loop;
   }
   if (f_L511_) {
    f_L511_ = FALSE;
    L11 = ATTR(ATTR(r,fun),args);
    L511_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
    i_L511_=0;
   }
   if(i_L511_>=L511_br)  goto after_loop; 
   aL511_=ARR((ARRAYARG)L11,i_L511_); i_L511_++;
   L56=aL511_;
   atp = ATTR(L56,tp);
   L57 = (ATTR(ATTR(r,fun),hot)==((ARRAYBOOL) NULL));
   L581_=!(L57); 
   if (L581_) {
    if (f_L521_) {
     f_L521_ = FALSE;
     L21 = ATTR(ATTR(r,fun),hot);
     L521_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
     i_L521_=0;
    }
    if(i_L521_>=L521_br)  goto after_loop; 
    aL521_=ARR((ARRAYBOOL)L21,i_L521_); i_L521_++;
    hot = aL521_;
   }
   if (a==NULL) {
    goto other423;
   } else
   switch (TAG(a)) {
    case AS_UND152986614_tag: break;
    default: ;
    other423: ;
     create_self2 = ((AM_CALL_ARG) NULL);
     create_e1 = TRANS_693526804(self, a, atp);
     create_m = MODE_c998271775(((MODE) NULL), m);
     L61=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L61==NULL) FATAL("Unable to allocate more memory");
     ((OB)L61)->header.tag=AM_CALL_ARG_tag;
     L60 = ((AM_CALL_ARG) L61);
     res1 = L60;
     SATTR(res1,expr,create_e1);
     SATTR(res1,mode1,create_m);
     ret_val15 = res1;
     L62 = ret_val15;
     SARR((AM_BND1124877163)r,b_ind,(AM_CALL_ARG)L62); 
     ;
     L641_=(AM_CALL_ARG)ARR((AM_BND1124877163)r,b_ind); 
     L65=L641_;
     if ((ATTR(L65,expr)==((dAM_EXPR) NULL))) {
      ret_val = ((dAM_EXPR) NULL);
      return ret_val;
     }
     if (ATTR(e,is_iter)) {
      L681_=!(hot); 
      L67 = L681_;
     } else {
      L67 = FALSE;
     }
     if (L67) {
      L691_=(AM_CALL_ARG)ARR((AM_BND1124877163)r,b_ind); 
      L70=L691_;
      L66 = TRANS_1865855576(self, ATTR(L70,expr));
     } else {
      L66 = FALSE;
     }
     if (L66) {
      bound_create_ite2 = self;
      bound_create_ite3 = a;
      err_loc_self4 = bound_create_ite2;
      err_loc_t4 = ((dPROG_ERR) bound_create_ite3);
      PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
      err_self4 = bound_create_ite2;
      err_s4 = ((STR) &Oncear1787080910);
      if ((err_self4==((TRANS) NULL))) {
       L72 = TRUE;
      } else {
       L72 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
      }
      if (L72) {
       L71 = TRUE;
      } else {
       L71 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
      }
      if (L71) {
       PROG_err_STR(ATTR(err_self4,prog), err_s4);
      }
      else {
       plus_self8 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
       plus_sarg8 = ((STR) &name21);
       ret_val16 = STR_ap2004550586(plus_self8, plus_sarg8);
       plus_self9 = ret_val16;
       plus_sarg9 = err_s4;
       ret_val17 = STR_ap2004550586(plus_self9, plus_sarg9);
       err_s4 = ret_val17;
       PROG_err_STR(ATTR(err_self4,prog), err_s4);
      }
      ret_val = ((dAM_EXPR) NULL);
      return ret_val;
     }
     L731_=INTPLUS(b_ind,1); 
     b_ind = L731_;
   }
   L74 = a;
   a = (*dAS_EX337431651[TAG(L74)])(L74);
  }
 }
 after_loop: ;
 TRANS_1147317392(self, e, r);
 L76 = (tp==((dTP) NULL));
 L771_=!(L76); 
 if (L771_) {
  tp_self = r;
  ret_val18 = ATTR(tp_self,tp_at);
  L78 = ret_val18;
  L79 = (*dTP_is1999456142[TAG(L78)])(L78, tp);
  L801_=!(L79); 
  L75 = L801_;
 } else {
  L75 = FALSE;
 }
 if (L75) {
  bound_create_con = self;
  bound_create_con1 = e;
  tp_self1 = r;
  ret_val19 = ATTR(tp_self1,tp_at);
  bound_create_con2 = ret_val19;
  bound_create_con3 = tp;
  err_loc_self5 = bound_create_con;
  err_loc_t5 = ((dPROG_ERR) bound_create_con1);
  PROG_e176405615(ATTR(err_loc_self5,prog), err_loc_t5);
  err_self5 = bound_create_con;
  plus_self10 = ((STR) &Thetyp1564090783);
  L81 = bound_create_con2;
  plus_sarg10 = (*dTP_strrSTR[TAG(L81)])(L81);
  ret_val20 = STR_ap2004550586(plus_self10, plus_sarg10);
  plus_self11 = ret_val20;
  plus_sarg11 = ((STR) &isnotasubtypeof);
  ret_val21 = STR_ap2004550586(plus_self11, plus_sarg11);
  plus_self12 = ret_val21;
  L82 = bound_create_con3;
  plus_sarg12 = (*dTP_strrSTR[TAG(L82)])(L82);
  ret_val22 = STR_ap2004550586(plus_self12, plus_sarg12);
  plus_self13 = ret_val22;
  plus_sarg13 = ((STR) &name18);
  ret_val23 = STR_ap2004550586(plus_self13, plus_sarg13);
  err_s5 = ret_val23;
  if ((err_self5==((TRANS) NULL))) {
   L84 = TRUE;
  } else {
   L84 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L84) {
   L83 = TRUE;
  } else {
   L83 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
  }
  if (L83) {
   PROG_err_STR(ATTR(err_self5,prog), err_s5);
  }
  else {
   plus_self14 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
   plus_sarg14 = ((STR) &name21);
   ret_val24 = STR_ap2004550586(plus_self14, plus_sarg14);
   plus_self15 = ret_val24;
   plus_sarg15 = err_s5;
   ret_val25 = STR_ap2004550586(plus_self15, plus_sarg15);
   err_s5 = ret_val25;
   PROG_err_STR(ATTR(err_self5,prog), err_s5);
  }
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), ((dAM_EXPR) r)));
 tp_self2 = r;
 ret_val26 = ATTR(tp_self2,tp_at);
 L85 = ret_val26;
 if ((*dTP_is411624931[TAG(L85)])(L85)) {
  L86 = SIG_is275586960(ATTR(r,fun));
  L871_=!(L86); 
  if (L871_) {
   f_rout_create_si = self;
   f_rout_create_si1 = e;
   err_loc_self6 = f_rout_create_si;
   err_loc_t6 = ((dPROG_ERR) f_rout_create_si1);
   PROG_e176405615(ATTR(err_loc_self6,prog), err_loc_t6);
   err_self6 = f_rout_create_si;
   err_s6 = ((STR) &Illega1957842819);
   if ((err_self6==((TRANS) NULL))) {
    L89 = TRUE;
   } else {
    L89 = (ATTR(err_self6,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L89) {
    L88 = TRUE;
   } else {
    L88 = (ATTR(ATTR(err_self6,cur_rout),sig1)==((SIG) NULL));
   }
   if (L88) {
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   else {
    plus_self16 = ATTR(ATTR(ATTR(err_self6,cur_rout),sig1),str);
    plus_sarg16 = ((STR) &name21);
    ret_val27 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self17 = ret_val27;
    plus_sarg17 = err_s6;
    ret_val28 = STR_ap2004550586(plus_self17, plus_sarg17);
    err_s6 = ret_val28;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
  }
  L90 = ATTR(r,bnd_args);
  L911_=(L90)==NULL?0:ASIZE((ARRAYINT)L90); 
  L92 = L911_;
  L931_=(0)<(L92); 
  if (L931_) {
   L95 = ATTR(r,bnd_args);
   L961_=(L95)==NULL?0:ASIZE((ARRAYINT)L95); 
   L97 = L961_;
   L981_=(L97)==(1); 
   if (L981_) {
    L99 = ATTR(r,bnd_args);
    L1001_=ARR((ARRAYINT)L99,0); 
    L101 = L1001_;
    L1021_=(L101)==(0); 
    L94 = L1021_;
   } else {
    L94 = FALSE;
   }
   L103 = L94;
   L1041_=!(L103); 
   if (L1041_) {
    f_rout_create_bo = self;
    f_rout_create_bo1 = e;
    err_loc_self7 = f_rout_create_bo;
    err_loc_t7 = ((dPROG_ERR) f_rout_create_bo1);
    PROG_e176405615(ATTR(err_loc_self7,prog), err_loc_t7);
    err_self7 = f_rout_create_bo;
    err_s7 = ((STR) &Allarg773350235);
    if ((err_self7==((TRANS) NULL))) {
     L106 = TRUE;
    } else {
     L106 = (ATTR(err_self7,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L106) {
     L105 = TRUE;
    } else {
     L105 = (ATTR(ATTR(err_self7,cur_rout),sig1)==((SIG) NULL));
    }
    if (L105) {
     PROG_err_STR(ATTR(err_self7,prog), err_s7);
    }
    else {
     plus_self18 = ATTR(ATTR(ATTR(err_self7,cur_rout),sig1),str);
     plus_sarg18 = ((STR) &name21);
     ret_val29 = STR_ap2004550586(plus_self18, plus_sarg18);
     plus_self19 = ret_val29;
     plus_sarg19 = err_s7;
     ret_val30 = STR_ap2004550586(plus_self19, plus_sarg19);
     err_s7 = ret_val30;
     PROG_err_STR(ATTR(err_self7,prog), err_s7);
    }
   }
  }
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1556853519(TRANS self, AS_STR_LIT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_STR_CONST r;
 TRANS str_lit_context_;
 AS_STR_LIT_EXPR str_lit_context_1;
 dTP str_lit_context_2;
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
 AM_STR_CONST create_self;
 AS_STR_LIT_EXPR create_t;
 AM_STR_CONST ret_val5;
 AM_STR_CONST r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR String1518861743;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_STR_CONST L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_str, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   str_lit_context_ = self;
   str_lit_context_1 = e;
   str_lit_context_2 = tp;
   err_loc_self = str_lit_context_;
   err_loc_t = ((dPROG_ERR) str_lit_context_1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = str_lit_context_;
   plus_self = ((STR) &String1518861743);
   L5 = str_lit_context_2;
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
 create_self = ((AM_STR_CONST) NULL);
 create_t = e;
 L9=ZALLOC(sizeof(struct AM_STR_CONST_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_STR_CONST_tag;
 L8 = ((AM_STR_CONST) L9);
 r1 = L8;
 SATTR(r1,source1,ATTR(create_t,source1));
 SATTR(r1,bval,ATTR(create_t,s));
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,tp_at,((dTP) TP_BUILTIN_str));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1627619307(TRANS self, AS_INITIAL_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dAM_EXPR te;
 AM_LOCAL_EXPR v;
 AM_ASSIGN_STMT as;
 AM_INITIAL_STMT inst;
 TRANS initial_out_of_p;
 AS_INITIAL_EXPR initial_out_of_p1;
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
 TRANS nested_initial_e;
 AS_INITIAL_EXPR nested_initial_e1;
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
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val5;
 AM_LOCAL_EXPR r;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val6;
 AM_ASSIGN_STMT r1;
 AM_INITIAL_STMT create_self2;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_INITIAL_STMT ret_val7;
 AM_INITIAL_STMT r2;
 BOOL L1;
 BOOL L21_;
 extern STR initia552865977;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR initia1858005947;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 dAM_EXPR L7;
 AM_LOCAL_EXPR L8;
 OB L9;
 AM_ASSIGN_STMT L10;
 OB L11;
 AM_INITIAL_STMT L12;
 OB L13;
 dAM_STMT L14;
 L1 = ATTR(self,in_post);
 L21_=!(L1); 
 if (L21_) {
  initial_out_of_p = self;
  initial_out_of_p1 = e;
  err_loc_self = initial_out_of_p;
  err_loc_t = ((dPROG_ERR) initial_out_of_p1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = initial_out_of_p;
  err_s = ((STR) &initia552865977);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if (ATTR(self,in_initial)) {
  nested_initial_e = self;
  nested_initial_e1 = e;
  err_loc_self1 = nested_initial_e;
  err_loc_t1 = ((dPROG_ERR) nested_initial_e1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = nested_initial_e;
  err_s1 = ((STR) &initia1858005947);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 SATTR(self,in_initial,TRUE);
 te = TRANS_693526804(self, ATTR(e,e), tp);
 SATTR(self,in_initial,FALSE);
 if ((te==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(e,source1);
 create_name = IDENT_zero;
 L7 = te;
 create_tp = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
 L9=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_LOCAL_EXPR_tag;
 L8 = ((AM_LOCAL_EXPR) L9);
 r = L8;
 SATTR(r,source1,create_src);
 SATTR(r,name1,create_name);
 SATTR(r,tp_at,create_tp);
 ret_val5 = r;
 v = ret_val5;
 SATTR(ATTR(self,cur_rout),locals1,FLISTA1817671564(ATTR(ATTR(self,cur_rout),locals1), v));
 create_self1 = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(e,source1);
 L11=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AM_ASSIGN_STMT_tag;
 L10 = ((AM_ASSIGN_STMT) L11);
 r1 = L10;
 SATTR(r1,source1,create_source);
 ret_val6 = r1;
 as = ret_val6;
 SATTR(as,src1,te);
 SATTR(as,dest,((dAM_EXPR) v));
 create_self2 = ((AM_INITIAL_STMT) NULL);
 create_source1 = ATTR(e,source1);
 L13=ZALLOC(sizeof(struct AM_INITIAL_STMT_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=AM_INITIAL_STMT_tag;
 L12 = ((AM_INITIAL_STMT) L13);
 r2 = L12;
 SATTR(r2,source1,create_source1);
 ret_val7 = r2;
 inst = ret_val7;
 SATTR(inst,tp,ATTR(ATTR(self,impl),tp));
 SATTR(inst,stmts,((dAM_STMT) as));
 if ((ATTR(self,init_stmts)==((dAM_STMT) NULL))) {
  SATTR(self,init_stmts,((dAM_STMT) inst));
 }
 else {
  L14 = ATTR(self,init_stmts);
  (*dAM_ST2020072576[TAG(L14)])(L14, ((dAM_STMT) inst));
 }
 ret_val = ((dAM_EXPR) v);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1668850900(TRANS self, AS_EXCEPT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_EXCEPT_EXPR r;
 TRANS except_const_err;
 AS_EXCEPT_EXPR except_const_err1;
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
 TRANS except_loc_err_s;
 AS_EXCEPT_EXPR except_loc_err_e;
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
 AM_EXCEPT_EXPR create_self;
 dTP create_tp;
 AM_EXCEPT_EXPR ret_val5;
 AM_EXCEPT_EXPR r1;
 AM_EXCEPT_EXPR tp_self;
 dTP ret_val6;
 AM_EXCEPT_EXPR tp_self1;
 dTP ret_val7;
 TRANS except_context_e;
 AS_EXCEPT_EXPR except_context_e1;
 dTP except_context_e2;
 dTP except_context_e3;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
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
 extern STR except680562044;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 BOOL L3;
 BOOL L41_;
 extern STR except1173160488;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 AM_EXCEPT_EXPR L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 dTP L11;
 BOOL L12;
 BOOL L131_;
 extern STR Thetyp1640230519;
 dTP L14;
 extern STR isnotasubtypeof;
 dTP L15;
 extern STR name18;
 BOOL L16;
 BOOL L17;
 extern STR name21;
 if (ATTR(self,in_constant)) {
  except_const_err = self;
  except_const_err1 = e;
  err_loc_self = except_const_err;
  err_loc_t = ((dPROG_ERR) except_const_err1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = except_const_err;
  err_s = ((STR) &except680562044);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L3 = ATTR(self,in_protect_then);
 L41_=L3==FALSE; 
 if (L41_) {
  except_loc_err_s = self;
  except_loc_err_e = e;
  err_loc_self1 = except_loc_err_s;
  err_loc_t1 = ((dPROG_ERR) except_loc_err_e);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = except_loc_err_s;
  err_s1 = ((STR) &except1173160488);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_EXCEPT_EXPR) NULL);
 create_tp = ATTR(self,ex_tp);
 L8=ZALLOC(sizeof(struct AM_EXCEPT_EXPR_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AM_EXCEPT_EXPR_tag;
 L7 = ((AM_EXCEPT_EXPR) L8);
 r1 = L7;
 SATTR(r1,tp_at,create_tp);
 ret_val5 = r1;
 r = ret_val5;
 L9 = (tp==((dTP) NULL));
 L101_=!(L9); 
 if (L101_) {
  tp_self = r;
  ret_val6 = ATTR(tp_self,tp_at);
  L11 = ret_val6;
  L12 = (*dTP_is1999456142[TAG(L11)])(L11, tp);
  L131_=!(L12); 
  if (L131_) {
   except_context_e = self;
   except_context_e1 = e;
   tp_self1 = r;
   ret_val7 = ATTR(tp_self1,tp_at);
   except_context_e2 = ret_val7;
   except_context_e3 = tp;
   err_loc_self2 = except_context_e;
   err_loc_t2 = ((dPROG_ERR) except_context_e1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = except_context_e;
   plus_self4 = ((STR) &Thetyp1640230519);
   L14 = except_context_e2;
   plus_sarg4 = (*dTP_strrSTR[TAG(L14)])(L14);
   ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val8;
   plus_sarg5 = ((STR) &isnotasubtypeof);
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val9;
   L15 = except_context_e3;
   plus_sarg6 = (*dTP_strrSTR[TAG(L15)])(L15);
   ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val10;
   plus_sarg7 = ((STR) &name18);
   ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s2 = ret_val11;
   if ((err_self2==((TRANS) NULL))) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L17) {
    L16 = TRUE;
   } else {
    L16 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L16) {
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
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1783297694(TRANS self, AS_BOOL_LIT_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_BOOL_CONST r;
 TRANS bool_lit_context;
 AS_BOOL_LIT_EXPR bool_lit_context1;
 dTP bool_lit_context2;
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
 AM_BOOL_CONST create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_BOOL_CONST ret_val5;
 AM_BOOL_CONST r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Boolea1401042582;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 AM_BOOL_CONST L8;
 OB L9;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   bool_lit_context = self;
   bool_lit_context1 = e;
   bool_lit_context2 = tp;
   err_loc_self = bool_lit_context;
   err_loc_t = ((dPROG_ERR) bool_lit_context1);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = bool_lit_context;
   plus_self = ((STR) &Boolea1401042582);
   L5 = bool_lit_context2;
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
 create_self = ((AM_BOOL_CONST) NULL);
 create_src = ATTR(e,source1);
 L9=ZALLOC(sizeof(struct AM_BOOL_CONST_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_BOOL_CONST_tag;
 L8 = ((AM_BOOL_CONST) L9);
 r1 = L8;
 SATTR(r1,source1,create_src);
 ret_val5 = r1;
 r = ret_val5;
 SATTR(r,tp_at,((dTP) TP_BUILTIN_bool));
 SATTR(r,val1,ATTR(e,val1));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_196236678(TRANS self, AS_VOID_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_VOID_CONST r;
 AM_VOID_CONST create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VOID_CONST ret_val1;
 AM_VOID_CONST r1;
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
 AM_VOID_CONST L1;
 OB L2;
 extern STR Compil1678608282;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 create_self = ((AM_VOID_CONST) NULL);
 create_source = ATTR(e,source1);
 L2=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_VOID_CONST_tag;
 L1 = ((AM_VOID_CONST) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 if ((tp==((dTP) NULL))) {
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) e);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = self;
  err_s = ((STR) &Compil1678608282);
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
 SATTR(r,tp_at,tp);
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_361080840(TRANS self, AS_OR_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dAM_EXPR e1;
 dAM_EXPR e2;
 AM_IF_EXPR r;
 AM_BOOL_CONST abc;
 TRANS or_context_err_s;
 AS_OR_EXPR or_context_err_e;
 dTP or_context_err_t;
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
 TRANS err_self;
 STR err_s;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 AM_IF_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_EXPR ret_val6;
 AM_IF_EXPR r1;
 AM_BOOL_CONST create_self1;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_BOOL_CONST ret_val7;
 AM_BOOL_CONST r2;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR Orexpr1074242364;
 extern STR notsubtypesof;
 dTP L5;
 extern STR name18;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 BOOL L8;
 AM_IF_EXPR L9;
 OB L10;
 AM_BOOL_CONST L11;
 OB L12;
 L1 = (tp==((dTP) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = TP_CLA513015767(TP_BUILTIN_bool, ((dCALL_TP) tp));
  L41_=!(L3); 
  if (L41_) {
   or_context_err_s = self;
   or_context_err_e = e;
   or_context_err_t = tp;
   err_loc_self = or_context_err_s;
   err_loc_t = ((dPROG_ERR) or_context_err_e);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self = or_context_err_s;
   plus_self = ((STR) &Orexpr1074242364);
   plus_sarg = ((STR) &notsubtypesof);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   L5 = or_context_err_t;
   plus_sarg1 = (*dTP_strrSTR[TAG(L5)])(L5);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val2;
   plus_sarg2 = ((STR) &name18);
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   err_s = ret_val3;
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
    plus_self3 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg3 = ((STR) &name21);
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val4;
    plus_sarg4 = err_s;
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    err_s = ret_val5;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 e1 = TRANS_693526804(self, ATTR(e,e1), ((dTP) TP_BUILTIN_bool));
 e2 = TRANS_693526804(self, ATTR(e,e2), ((dTP) TP_BUILTIN_bool));
 if ((e1==((dAM_EXPR) NULL))) {
  L8 = TRUE;
 } else {
  L8 = (e2==((dAM_EXPR) NULL));
 }
 if (L8) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_IF_EXPR) NULL);
 create_source = ATTR(e,source1);
 L10=ZALLOC(sizeof(struct AM_IF_EXPR_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_IF_EXPR_tag;
 L9 = ((AM_IF_EXPR) L10);
 r1 = L9;
 SATTR(r1,source1,create_source);
 ret_val6 = r1;
 r = ret_val6;
 SATTR(r,test1,e1);
 SATTR(r,if_false,e2);
 create_self1 = ((AM_BOOL_CONST) NULL);
 create_src = ATTR(e,source1);
 L12=ZALLOC(sizeof(struct AM_BOOL_CONST_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_BOOL_CONST_tag;
 L11 = ((AM_BOOL_CONST) L12);
 r2 = L11;
 SATTR(r2,source1,create_src);
 ret_val7 = r2;
 abc = ret_val7;
 SATTR(abc,val1,TRUE);
 SATTR(r,if_true,((dAM_EXPR) abc));
 SATTR(r,tp_at,((dTP) TP_BUILTIN_bool));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_595522723(TRANS self, AS_ARRAY_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dTP pt = ((dTP) NULL);
 AM_ARRAY_EXPR r;
 dAS_EXPR ae;
 INT i = INT_zero;
 dAM_EXPR tae;
 TRANS array_tp_void_er;
 AS_ARRAY_EXPR array_tp_void_er1;
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
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 TRANS array_wrong_tp_e;
 AS_ARRAY_EXPR array_wrong_tp_e1;
 dTP array_wrong_tp_e2;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 TRANS err_self1;
 STR err_s1;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 TRANS array_wrong_tp_e3;
 AS_ARRAY_EXPR array_wrong_tp_e4;
 dTP array_wrong_tp_e5;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val10;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val11;
 TRANS err_self2;
 STR err_s2;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val12;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val13;
 AM_ARRAY_EXPR create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ARRAY_EXPR ret_val14;
 AM_ARRAY_EXPR r1;
 extern STR Thetyp1295754401;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 ARRAYdTP L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 extern STR Theinferredtype;
 dTP L12;
 extern STR forthisarray;
 extern STR creati960341370;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 ARRAYdTP L15;
 dTP L161_;
 extern STR Theinferredtype;
 dTP L17;
 extern STR forthisarray;
 extern STR creati960341370;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 AM_ARRAY_EXPR L20;
 INT L21;
 OB L22;
 INT L23;
 BOOL L24;
 BOOL L251_;
 dAS_EXPR L27;
 INT L281_;
 if ((tp==((dTP) NULL))) {
  array_tp_void_er = self;
  array_tp_void_er1 = e;
  err_loc_self = array_tp_void_er;
  err_loc_t = ((dPROG_ERR) array_tp_void_er1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = array_tp_void_er;
  err_s = ((STR) &Thetyp1295754401);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if (tp==NULL) {
  goto other413;
 } else
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   is_eq_self = ATTR(((TP_CLASS) tp),name1);
   is_eq_i = IDENT_190278447;
   ret_val3 = (is_eq_self.str==is_eq_i.str);
   L4 = ret_val3;
   L51_=!(L4); 
   if (L51_) {
    L3 = TRUE;
   } else {
    L6 = ATTR(((TP_CLASS) tp),params);
    L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
    L8 = L71_;
    L91_=(L8)==(1); 
    L10 = L91_;
    L111_=!(L10); 
    L3 = L111_;
   }
   if (L3) {
    array_wrong_tp_e = self;
    array_wrong_tp_e1 = e;
    array_wrong_tp_e2 = tp;
    err_loc_self1 = array_wrong_tp_e;
    err_loc_t1 = ((dPROG_ERR) array_wrong_tp_e1);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self1 = array_wrong_tp_e;
    plus_self2 = ((STR) &Theinferredtype);
    L12 = array_wrong_tp_e2;
    plus_sarg2 = (*dTP_strrSTR[TAG(L12)])(L12);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val4;
    plus_sarg3 = ((STR) &forthisarray);
    ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val5;
    plus_sarg4 = ((STR) &creati960341370);
    ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
    err_s1 = ret_val6;
    if ((err_self1==((TRANS) NULL))) {
     L14 = TRUE;
    } else {
     L14 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L14) {
     L13 = TRUE;
    } else {
     L13 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L13) {
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    else {
     plus_self5 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
     plus_sarg5 = ((STR) &name21);
     ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
     plus_self6 = ret_val7;
     plus_sarg6 = err_s1;
     ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
     err_s1 = ret_val8;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   L15 = ATTR(((TP_CLASS) tp),params);
   L161_=(dTP)ARR((ARRAYdTP)L15,0); 
   pt = L161_; break;
  default: ;
  other413: ;
   array_wrong_tp_e3 = self;
   array_wrong_tp_e4 = e;
   array_wrong_tp_e5 = tp;
   err_loc_self2 = array_wrong_tp_e3;
   err_loc_t2 = ((dPROG_ERR) array_wrong_tp_e4);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = array_wrong_tp_e3;
   plus_self7 = ((STR) &Theinferredtype);
   L17 = array_wrong_tp_e5;
   plus_sarg7 = (*dTP_strrSTR[TAG(L17)])(L17);
   ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
   plus_self8 = ret_val9;
   plus_sarg8 = ((STR) &forthisarray);
   ret_val10 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val10;
   plus_sarg9 = ((STR) &creati960341370);
   ret_val11 = STR_ap2004550586(plus_self9, plus_sarg9);
   err_s2 = ret_val11;
   if ((err_self2==((TRANS) NULL))) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L19) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L18) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val12;
    plus_sarg11 = err_s2;
    ret_val13 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s2 = ret_val13;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
 }
 create_self = ((AM_ARRAY_EXPR) NULL);
 create_nargs = AS_ARR1847317313(e);
 create_source = ATTR(e,source1);
 L21 = create_nargs;
 L23=(sizeof(struct AM_ARRAY_EXPR_struct)-sizeof(dAM_EXPR))+(L21)*sizeof(dAM_EXPR);
 L22=ZALLOC_BIG(L23);
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=AM_ARRAY_EXPR_tag;
#ifdef DESTROY_CHK

   ((OB)L22)->header.destroyed=0;
#endif

 L20 = ((AM_ARRAY_EXPR) L22);
 L20->asize = L21;
 r1 = L20;
 SATTR(r1,source1,create_source);
 ret_val14 = r1;
 r = ret_val14;
 SATTR(r,tp_at,tp);
 ae = ATTR(e,elts);
 i = 0;
 while (1) {
  L24 = (ae==((dAS_EXPR) NULL));
  L251_=!(L24); 
  if (L251_) {
  }
  else {
   goto after_loop;
  }
  tae = TRANS_693526804(self, ae, pt);
  if ((tae==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
  SARR((AM_ARRAY_EXPR)r,i,(dAM_EXPR)tae); 
  ;
  L27 = ae;
  ae = (*dAS_EX337431651[TAG(L27)])(L27);
  L281_=INTPLUS(i,1); 
  i = L281_;
 }
 after_loop: ;
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_700113784(TRANS self, AS_CREATE_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dTP at = ((dTP) NULL);
 BOOL dummyclass = BOOL_zero;
 INT na = INT_zero;
 AM_ROU1916046290 r;
 AM_VOID_CONST av;
 CALL_SIG cs;
 dAS_EXPR ce;
 INT i = INT_zero;
 AS_ARG_MODE m;
 dAM_EXPR r2;
 TRANS create_const_err;
 AS_CREATE_EXPR create_const_err1;
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
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val3;
 TRANS create_context_e;
 AS_CREATE_EXPR create_context_e1;
 dTP create_context_e2;
 dTP create_context_e3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 TRANS create_tp_spec_e;
 AS_CREATE_EXPR create_tp_spec_e1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val10;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val11;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val12;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val13;
 TRANS err_self4;
 STR err_s4;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val14;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val15;
 AM_ROU1916046290 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROU1916046290 ret_val16;
 AM_ROU1916046290 r1;
 AM_VOID_CONST create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_VOID_CONST ret_val17;
 AM_VOID_CONST r3;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val18;
 AM_CALL_ARG res;
 CALL_SIG create_self3;
 CALL_SIG ret_val19;
 ARRAYCALL_ARG create_self4;
 INT create_n = INT_zero;
 ARRAYCALL_ARG ret_val20;
 CALL_ARG create_self5;
 dCALL_TP create_t;
 dMODE create_m;
 CALL_ARG ret_val21;
 CALL_ARG res1;
 AM_CALL_ARG create_self6;
 dAM_EXPR create_e1;
 dMODE create_m1;
 AM_CALL_ARG ret_val22;
 AM_CALL_ARG res2;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 AM_CALL_ARG create_self7;
 dAM_EXPR create_e2;
 dMODE create_m2;
 AM_CALL_ARG ret_val23;
 AM_CALL_ARG res3;
 TRANS create_bad_retur;
 AS_CREATE_EXPR create_bad_retur1;
 dTP create_bad_retur2;
 dTP create_bad_retur3;
 TRANS err_loc_self5;
 dPROG_ERR err_loc_t5;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val24;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val25;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val26;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val27;
 TRANS err_self5;
 STR err_s5;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val28;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val29;
 TRANS special_inline_s;
 AM_ROU1916046290 special_inline_c;
 dAM_EXPR ret_val30;
 dAM_EXPR am;
 extern STR Creati989582580;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 dTP L7;
 BOOL L8;
 BOOL L91_;
 extern STR Thetyp1424918217;
 dTP L10;
 extern STR isnotasubtypeof;
 dTP L11;
 extern STR name18;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 extern STR Thiscr2087058747;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 dTP L16;
 BOOL L171_;
 extern STR Creati2137576634;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 BOOL L20;
 BOOL L21;
 dTP L22;
 INT L23;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 extern STR Partia2053653834;
 BOOL L30;
 BOOL L31;
 extern STR name21;
 INT L321_;
 AM_ROU1916046290 L33;
 INT L34;
 OB L35;
 INT L36;
 AM_VOID_CONST L37;
 OB L38;
 AM_CALL_ARG L39;
 OB L40;
 AM_CALL_ARG L42;
 CALL_SIG L44;
 OB L45;
 BOOL L461_;
 ARRAYCALL_ARG L47;
 INT L48;
 OB L49;
 INT L50;
 BOOL L51;
 BOOL L521_;
 ARRAYCALL_ARG L53;
 CALL_ARG L54;
 OB L55;
 CALL_ARG L56;
 ARRAYCALL_ARG L58;
 CALL_ARG L591_;
 CALL_ARG L60;
 INT L621_;
 INT L63;
 AM_CALL_ARG L64;
 OB L65;
 AM_CALL_ARG L66;
 INT L681_;
 INT L69;
 AM_CALL_ARG L701_;
 AM_CALL_ARG L71;
 ARRAYCALL_ARG L72;
 CALL_ARG L731_;
 INT L741_;
 INT L75;
 AM_CALL_ARG L761_;
 AM_CALL_ARG L77;
 dAM_EXPR L78;
 ARRAYCALL_ARG L79;
 CALL_ARG L801_;
 INT L811_;
 INT L82;
 AM_CALL_ARG L831_;
 AM_CALL_ARG L84;
 dAS_EXPR L85;
 INT L861_;
 dTP L87;
 dTP L88;
 IMPL L89;
 dTP L90;
 IFC L92;
 BOOL L93;
 BOOL L941_;
 INT L951_;
 INT L96;
 AM_CALL_ARG L971_;
 INT L981_;
 INT L99;
 ARRAYARG L100;
 ARG L1011_;
 ARG L102;
 ARRAYARG L103;
 ARG L1041_;
 ARG L105;
 AM_CALL_ARG L106;
 OB L107;
 AM_CALL_ARG L108;
 INT L1101_;
 INT L111;
 AM_CALL_ARG L1121_;
 AM_CALL_ARG L113;
 dAS_EXPR L114;
 INT L1151_;
 dTP L116;
 BOOL L117;
 BOOL L1181_;
 extern STR Thiscr666336343;
 dTP L119;
 extern STR ratherthan;
 dTP L120;
 extern STR asitmust;
 BOOL L121;
 BOOL L122;
 extern STR name21;
 BOOL L123;
 BOOL L1241_;
 dGENERATE_AM L125;
 dINLINE L126;
 if (ATTR(self,in_constant)) {
  create_const_err = self;
  create_const_err1 = e;
  err_loc_self = create_const_err;
  err_loc_t = ((dPROG_ERR) create_const_err1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = create_const_err;
  err_s = ((STR) &Creati989582580);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L3 = (ATTR(e,tp)==((AS_TYPE_SPEC) NULL));
 L41_=!(L3); 
 if (L41_) {
  tp_of_self = self;
  tp_of_t = ATTR(e,tp);
  ret_val3 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  at = ret_val3;
  L5 = (tp==((dTP) NULL));
  L61_=!(L5); 
  if (L61_) {
   L7 = at;
   L8 = (*dTP_is1999456142[TAG(L7)])(L7, tp);
   L91_=!(L8); 
   if (L91_) {
    create_context_e = self;
    create_context_e1 = e;
    create_context_e2 = at;
    create_context_e3 = tp;
    err_loc_self1 = create_context_e;
    err_loc_t1 = ((dPROG_ERR) create_context_e1);
    PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
    err_self1 = create_context_e;
    plus_self2 = ((STR) &Thetyp1424918217);
    L10 = create_context_e2;
    plus_sarg2 = (*dTP_strrSTR[TAG(L10)])(L10);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val4;
    plus_sarg3 = ((STR) &isnotasubtypeof);
    ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val5;
    L11 = create_context_e3;
    plus_sarg4 = (*dTP_strrSTR[TAG(L11)])(L11);
    ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val6;
    plus_sarg5 = ((STR) &name18);
    ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s1 = ret_val7;
    if ((err_self1==((TRANS) NULL))) {
     L13 = TRUE;
    } else {
     L13 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L13) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
    }
    if (L12) {
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
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
 }
 else {
  if ((tp==((dTP) NULL))) {
   create_tp_spec_e = self;
   create_tp_spec_e1 = e;
   err_loc_self2 = create_tp_spec_e;
   err_loc_t2 = ((dPROG_ERR) create_tp_spec_e1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = create_tp_spec_e;
   err_s2 = ((STR) &Thiscr2087058747);
   if ((err_self2==((TRANS) NULL))) {
    L15 = TRUE;
   } else {
    L15 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L15) {
    L14 = TRUE;
   } else {
    L14 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L14) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val10;
    plus_sarg9 = err_s2;
    ret_val11 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s2 = ret_val11;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
  else {
   at = tp;
  }
 }
 L16 = at;
 if ((*dTP_is1811059018[TAG(L16)])(L16)) {
  dummyclass = FALSE;
  if (ATTR(ATTR(self,prog),check_generics)) {
   if (at==NULL) {
    goto other414;
   } else
   switch (TAG(at)) {
    case TP_CLASS_tag:
     dummyclass = ATTR(((TP_CLASS) at),is_dummy_class); break;
    default: ;
    other414: ;
   }
  }
  L171_=!(dummyclass); 
  if (L171_) {
   err_loc_self3 = self;
   err_loc_t3 = ((dPROG_ERR) e);
   PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
   err_self3 = self;
   err_s3 = ((STR) &Creati2137576634);
   if ((err_self3==((TRANS) NULL))) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L19) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L18) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val12 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val12;
    plus_sarg11 = err_s3;
    ret_val13 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s3 = ret_val13;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 L22 = at;
 if ((*dTP_is1553335918[TAG(L22)])(L22)) {
  L23 = ATTR(ATTR(e,tp),kind);
  L24 = AS_TYP1853679690;
  L251_=(L23)==(L24); 
  L26 = L251_;
  L271_=!(L26); 
  L21 = L271_;
 } else {
  L21 = FALSE;
 }
 if (L21) {
  L28 = ATTR(ATTR(self,prog),is_check_am);
  L291_=!(L28); 
  L20 = L291_;
 } else {
  L20 = FALSE;
 }
 if (L20) {
  err_self4 = self;
  err_s4 = ((STR) &Partia2053653834);
  if ((err_self4==((TRANS) NULL))) {
   L31 = TRUE;
  } else {
   L31 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L31) {
   L30 = TRUE;
  } else {
   L30 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
  }
  if (L30) {
   PROG_err_STR(ATTR(err_self4,prog), err_s4);
  }
  else {
   plus_self12 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
   plus_sarg12 = ((STR) &name21);
   ret_val14 = STR_ap2004550586(plus_self12, plus_sarg12);
   plus_self13 = ret_val14;
   plus_sarg13 = err_s4;
   ret_val15 = STR_ap2004550586(plus_self13, plus_sarg13);
   err_s4 = ret_val15;
   PROG_err_STR(ATTR(err_self4,prog), err_s4);
  }
 }
 TRANS_74748323(self, ATTR(e,tp));
 na = AS_CRE943264723(e);
 create_self = ((AM_ROU1916046290) NULL);
 L321_=INTPLUS(na,1); 
 create_nargs = L321_;
 create_source = ATTR(e,source1);
 L34 = create_nargs;
 L36=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L34)*sizeof(AM_CALL_ARG);
 L35=ZALLOC_BIG(L36);
 if (L35==NULL) FATAL("Unable to allocate more memory");
 ((OB)L35)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

   ((OB)L35)->header.destroyed=0;
#endif

 L33 = ((AM_ROU1916046290) L35);
 L33->asize = L34;
 r1 = L33;
 SATTR(r1,source1,create_source);
 ret_val16 = r1;
 r = ret_val16;
 SATTR(r,as_type,ATTR(e,tp));
 create_self1 = ((AM_VOID_CONST) NULL);
 create_source1 = ATTR(e,source1);
 L38=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
 if (L38==NULL) FATAL("Unable to allocate more memory");
 ((OB)L38)->header.tag=AM_VOID_CONST_tag;
 L37 = ((AM_VOID_CONST) L38);
 r3 = L37;
 SATTR(r3,source1,create_source1);
 ret_val17 = r3;
 av = ret_val17;
 SATTR(av,tp_at,at);
 create_self2 = ((AM_CALL_ARG) NULL);
 create_e = ((dAM_EXPR) av);
 L40=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=AM_CALL_ARG_tag;
 L39 = ((AM_CALL_ARG) L40);
 res = L39;
 SATTR(res,expr,create_e);
 SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
 ret_val18 = res;
 L42 = ret_val18;
 SARR((AM_ROU1916046290)r,0,(AM_CALL_ARG)L42); 
 ;
 create_self3 = ((CALL_SIG) NULL);
 L45=ZALLOC(sizeof(struct CALL_SIG_struct));
 if (L45==NULL) FATAL("Unable to allocate more memory");
 ((OB)L45)->header.tag=CALL_SIG_tag;
 L44 = ((CALL_SIG) L45);
 ret_val19 = L44;
 cs = ret_val19;
 L461_=(0)<(na); 
 if (L461_) {
  create_self4 = ((ARRAYCALL_ARG) NULL);
  create_n = na;
  L48 = create_n;
  L50=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L48)*sizeof(CALL_ARG);
  L49=ZALLOC_BIG(L50);
  if (L49==NULL) FATAL("Unable to allocate more memory");
  ((OB)L49)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

    ((OB)L49)->header.destroyed=0;
#endif

  L47 = ((ARRAYCALL_ARG) L49);
  L47->asize = L48;
  ret_val20 = L47;
  SATTR(cs,args,ret_val20);
 }
 SATTR(cs,tp,at);
 SATTR(cs,name1,IDENT_458929583);
 SATTR(cs,has_ret,TRUE);
 ce = ATTR(e,elts);
 i = 0;
 m = ATTR(e,modes1);
 while (1) {
  L51 = (ce==((dAS_EXPR) NULL));
  L521_=!(L51); 
  if (L521_) {
  }
  else {
   goto after_loop;
  }
  L53 = ATTR(cs,args);
  create_self5 = ((CALL_ARG) NULL);
  create_t = TRANS_68137339(self, ce);
  create_m = MODE_c998271775(((MODE) NULL), m);
  L55=ZALLOC(sizeof(struct CALL_ARG_struct));
  if (L55==NULL) FATAL("Unable to allocate more memory");
  ((OB)L55)->header.tag=CALL_ARG_tag;
  L54 = ((CALL_ARG) L55);
  res1 = L54;
  SATTR(res1,tp,create_t);
  SATTR(res1,mode1,create_m);
  ret_val21 = res1;
  L56 = ret_val21;
  SARR((ARRAYCALL_ARG)L53,i,(CALL_ARG)L56); 
  ;
  L58 = ATTR(cs,args);
  L591_=(CALL_ARG)ARR((ARRAYCALL_ARG)L58,i); 
  L60=L591_;
  if ((ATTR(L60,tp)==((dCALL_TP) NULL))) {
   L621_=INTPLUS(i,1); 
   L63 = L621_;
   create_self6 = ((AM_CALL_ARG) NULL);
   create_e1 = TRANS_693526804(self, ce, ((dTP) NULL));
   create_m1 = MODE_c998271775(((MODE) NULL), m);
   L65=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=AM_CALL_ARG_tag;
   L64 = ((AM_CALL_ARG) L65);
   res2 = L64;
   SATTR(res2,expr,create_e1);
   SATTR(res2,mode1,create_m1);
   ret_val22 = res2;
   L66 = ret_val22;
   SARR((AM_ROU1916046290)r,L63,(AM_CALL_ARG)L66); 
   ;
   L681_=INTPLUS(i,1); 
   L69 = L681_;
   L701_=(AM_CALL_ARG)ARR((AM_ROU1916046290)r,L69); 
   L71=L701_;
   if ((ATTR(L71,expr)==((dAM_EXPR) NULL))) {
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   L72 = ATTR(cs,args);
   L731_=(CALL_ARG)ARR((ARRAYCALL_ARG)L72,i); 
   L741_=INTPLUS(i,1); 
   L75 = L741_;
   L761_=(AM_CALL_ARG)ARR((AM_ROU1916046290)r,L75); 
   L77=L761_;
   L78 = ATTR(L77,expr);
   SATTR(L731_,tp,((dCALL_TP) (*dAM_EXPR_tprdTP[TAG(L78)])(L78)));
   L79 = ATTR(cs,args);
   L801_=(CALL_ARG)ARR((ARRAYCALL_ARG)L79,i); 
   L811_=INTPLUS(i,1); 
   L82 = L811_;
   L831_=(AM_CALL_ARG)ARR((AM_ROU1916046290)r,L82); 
   L84=L831_;
   SATTR(L801_,mode1,ATTR(L84,mode1));
  }
  L85 = ce;
  ce = (*dAS_EX337431651[TAG(L85)])(L85);
  m = ATTR(m,next);
  L861_=INTPLUS(i,1); 
  i = L861_;
 }
 after_loop: ;
 err_loc_self4 = self;
 err_loc_t4 = ((dPROG_ERR) e);
 PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
 L87 = at;
 if ((*dTP_is242312711[TAG(L87)])(L87, ((OB) ATTR(ATTR(self,tp_con),same1)))) {
  L88 = at;
  L89 = (*dTP_implrIMPL[TAG(L88)])(L88);
  SATTR(r,fun,IMPL_s773282892(L89, cs));
 }
 else {
  L90 = at;
  L92 = (*dTP_ifcrIFC[TAG(L90)])(L90);
  SATTR(r,fun,IFC_si178998979(L92, cs));
 }
 if ((ATTR(r,fun)==((SIG) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 ce = ATTR(e,elts);
 i = 0;
 while (1) {
  L93 = (ce==((dAS_EXPR) NULL));
  L941_=!(L93); 
  if (L941_) {
  }
  else {
   goto after_loop1;
  }
  L951_=INTPLUS(i,1); 
  L96 = L951_;
  L971_=(AM_CALL_ARG)ARR((AM_ROU1916046290)r,L96); 
  if ((L971_==((AM_CALL_ARG) NULL))) {
   L981_=INTPLUS(i,1); 
   L99 = L981_;
   create_self7 = ((AM_CALL_ARG) NULL);
   L100 = ATTR(ATTR(r,fun),args);
   L1011_=(ARG)ARR((ARRAYARG)L100,i); 
   L102=L1011_;
   create_e2 = TRANS_693526804(self, ce, ATTR(L102,tp));
   L103 = ATTR(ATTR(r,fun),args);
   L1041_=(ARG)ARR((ARRAYARG)L103,i); 
   L105=L1041_;
   create_m2 = ATTR(L105,mode1);
   L107=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L107==NULL) FATAL("Unable to allocate more memory");
   ((OB)L107)->header.tag=AM_CALL_ARG_tag;
   L106 = ((AM_CALL_ARG) L107);
   res3 = L106;
   SATTR(res3,expr,create_e2);
   SATTR(res3,mode1,create_m2);
   ret_val23 = res3;
   L108 = ret_val23;
   SARR((AM_ROU1916046290)r,L99,(AM_CALL_ARG)L108); 
   ;
   L1101_=INTPLUS(i,1); 
   L111 = L1101_;
   L1121_=(AM_CALL_ARG)ARR((AM_ROU1916046290)r,L111); 
   L113=L1121_;
   if ((ATTR(L113,expr)==((dAM_EXPR) NULL))) {
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
  }
  L114 = ce;
  ce = (*dAS_EX337431651[TAG(L114)])(L114);
  L1151_=INTPLUS(i,1); 
  i = L1151_;
 }
 after_loop1: ;
 L116 = ATTR(ATTR(r,fun),ret);
 L117 = (*dTP_is242312711[TAG(L116)])(L116, ((OB) at));
 L1181_=!(L117); 
 if (L1181_) {
  create_bad_retur = self;
  create_bad_retur1 = e;
  create_bad_retur2 = ATTR(ATTR(r,fun),ret);
  create_bad_retur3 = at;
  err_loc_self5 = create_bad_retur;
  err_loc_t5 = ((dPROG_ERR) create_bad_retur1);
  PROG_e176405615(ATTR(err_loc_self5,prog), err_loc_t5);
  err_self5 = create_bad_retur;
  plus_self14 = ((STR) &Thiscr666336343);
  L119 = create_bad_retur2;
  plus_sarg14 = (*dTP_strrSTR[TAG(L119)])(L119);
  ret_val24 = STR_ap2004550586(plus_self14, plus_sarg14);
  plus_self15 = ret_val24;
  plus_sarg15 = ((STR) &ratherthan);
  ret_val25 = STR_ap2004550586(plus_self15, plus_sarg15);
  plus_self16 = ret_val25;
  L120 = create_bad_retur3;
  plus_sarg16 = (*dTP_strrSTR[TAG(L120)])(L120);
  ret_val26 = STR_ap2004550586(plus_self16, plus_sarg16);
  plus_self17 = ret_val26;
  plus_sarg17 = ((STR) &asitmust);
  ret_val27 = STR_ap2004550586(plus_self17, plus_sarg17);
  err_s5 = ret_val27;
  if ((err_self5==((TRANS) NULL))) {
   L122 = TRUE;
  } else {
   L122 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L122) {
   L121 = TRUE;
  } else {
   L121 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
  }
  if (L121) {
   PROG_err_STR(ATTR(err_self5,prog), err_s5);
  }
  else {
   plus_self18 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
   plus_sarg18 = ((STR) &name21);
   ret_val28 = STR_ap2004550586(plus_self18, plus_sarg18);
   plus_self19 = ret_val28;
   plus_sarg19 = err_s5;
   ret_val29 = STR_ap2004550586(plus_self19, plus_sarg19);
   err_s5 = ret_val29;
   PROG_err_STR(ATTR(err_self5,prog), err_s5);
  }
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), ((dAM_EXPR) r)));
 SE_CON412941612(ATTR(self,cur_se), ((dAM) r));
 special_inline_s = self;
 special_inline_c = r;
 L123 = ATTR(ATTR(special_inline_s,prog),all_reached);
 L1241_=!(L123); 
 if (L1241_) {
  L125 = ATTR(ATTR(special_inline_s,prog),generate_am);
  (*dGENER651759373[TAG(L125)])(L125, ATTR(special_inline_c,fun));
 }
 L126 = ATTR(ATTR(special_inline_s,prog),inliner);
 am = (*dINLIN970826022[TAG(L126)])(L126, special_inline_c);
 switch (TAG(am)) {
  case AM_STMT_EXPR_tag:
   SATTR(ATTR(special_inline_s,cur_rout),calls1,FLISTd1364738838(ATTR(ATTR(special_inline_s,cur_rout),calls1), ATTR(((AM_STMT_EXPR) am),calls1)));
   SATTR(ATTR(special_inline_s,cur_rout),locals1,FLISTA811588375(ATTR(ATTR(special_inline_s,cur_rout),locals1), ATTR(((AM_STMT_EXPR) am),locals1))); break;
  default: ;
 }
 ret_val30 = am;
 r2 = ret_val30;
 ret_val = r2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_962243143(TRANS self, AS_NEW_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 dTP t;
 INT k = INT_zero;
 AM_NEW_EXPR r = ((AM_NEW_EXPR) NULL);
 TRANS new_const_err_se;
 AS_NEW_EXPR new_const_err_e;
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
 TRANS new_in_non_ref_o;
 AS_NEW_EXPR new_in_non_ref_o1;
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
 TRANS new_context_err_;
 AS_NEW_EXPR new_context_err_1;
 dTP new_context_err_2;
 dTP new_context_err_3;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 TRANS new_arg_no_array;
 AS_NEW_EXPR new_arg_no_array1;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val11;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val12;
 AM_NEW_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_NEW_EXPR ret_val13;
 AM_NEW_EXPR r1;
 TRANS new_no_arg_array;
 AS_NEW_EXPR new_no_arg_array1;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 TRANS err_self4;
 STR err_s4;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val14;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val15;
 AM_NEW_EXPR create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_NEW_EXPR ret_val16;
 AM_NEW_EXPR r2;
 extern STR newexp2094845250;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 dTP L3;
 BOOL L4;
 BOOL L5;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR newexp646232754;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 BOOL L20;
 BOOL L211_;
 BOOL L22;
 BOOL L231_;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 extern STR Thetyp249438757;
 dTP L27;
 extern STR isnotasubtypeof;
 dTP L28;
 extern STR name18;
 BOOL L29;
 BOOL L30;
 extern STR name21;
 BOOL L31;
 BOOL L321_;
 extern STR newexp1686257053;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 AM_NEW_EXPR L35;
 OB L36;
 BOOL L37;
 BOOL L381_;
 extern STR newexp1208830217;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 AM_NEW_EXPR L41;
 OB L42;
 if (ATTR(self,in_constant)) {
  new_const_err_se = self;
  new_const_err_e = e;
  err_loc_self = new_const_err_se;
  err_loc_t = ((dPROG_ERR) new_const_err_e);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = new_const_err_se;
  err_s = ((STR) &newexp2094845250);
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 t = ATTR(ATTR(self,impl),tp);
 L3 = t;
 k = (*dTP_kindrINT[TAG(L3)])(L3);
 L6 = TP_KIND_ref_tp;
 L71_=(k)==(L6); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = TP_KIND_spr_tp;
  L111_=(k)==(L10); 
  L12 = L111_;
  L131_=!(L12); 
  L5 = L131_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  L14 = TP_KIND_part_tp;
  L151_=(k)==(L14); 
  L16 = L151_;
  L171_=!(L16); 
  L4 = L171_;
 } else {
  L4 = FALSE;
 }
 if (L4) {
  new_in_non_ref_o = self;
  new_in_non_ref_o1 = e;
  err_loc_self1 = new_in_non_ref_o;
  err_loc_t1 = ((dPROG_ERR) new_in_non_ref_o1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = new_in_non_ref_o;
  err_s1 = ((STR) &newexp646232754);
  if ((err_self1==((TRANS) NULL))) {
   L19 = TRUE;
  } else {
   L19 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L19) {
   L18 = TRUE;
  } else {
   L18 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L18) {
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
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 SE_CON435970114(ATTR(self,cur_se));
 L20 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L211_=!(L20); 
 if (L211_) {
  SATTR(ATTR(self,cur_se),has_new,TRUE);
 }
 L22 = (tp==((dTP) NULL));
 L231_=!(L22); 
 if (L231_) {
  L24 = t;
  L25 = (*dTP_is1999456142[TAG(L24)])(L24, tp);
  L261_=!(L25); 
  if (L261_) {
   new_context_err_ = self;
   new_context_err_1 = e;
   new_context_err_2 = t;
   new_context_err_3 = tp;
   err_loc_self2 = new_context_err_;
   err_loc_t2 = ((dPROG_ERR) new_context_err_1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = new_context_err_;
   plus_self4 = ((STR) &Thetyp249438757);
   L27 = new_context_err_2;
   plus_sarg4 = (*dTP_strrSTR[TAG(L27)])(L27);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = ((STR) &isnotasubtypeof);
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val6;
   L28 = new_context_err_3;
   plus_sarg6 = (*dTP_strrSTR[TAG(L28)])(L28);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = ((STR) &name18);
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s2 = ret_val8;
   if ((err_self2==((TRANS) NULL))) {
    L30 = TRUE;
   } else {
    L30 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L30) {
    L29 = TRUE;
   } else {
    L29 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L29) {
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   else {
    plus_self8 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val9;
    plus_sarg9 = err_s2;
    ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s2 = ret_val10;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 L31 = (ATTR(e,arg)==((dAS_EXPR) NULL));
 L321_=!(L31); 
 if (L321_) {
  if ((ATTR(ATTR(self,impl),arr)==((TP_CLASS) NULL))) {
   new_arg_no_array = self;
   new_arg_no_array1 = e;
   err_loc_self3 = new_arg_no_array;
   err_loc_t3 = ((dPROG_ERR) new_arg_no_array1);
   PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
   err_self3 = new_arg_no_array;
   err_s3 = ((STR) &newexp1686257053);
   if ((err_self3==((TRANS) NULL))) {
    L34 = TRUE;
   } else {
    L34 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L34) {
    L33 = TRUE;
   } else {
    L33 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
   }
   if (L33) {
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val11 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val11;
    plus_sarg11 = err_s3;
    ret_val12 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s3 = ret_val12;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
  create_self = ((AM_NEW_EXPR) NULL);
  create_source = ATTR(e,source1);
  L36=ZALLOC(sizeof(struct AM_NEW_EXPR_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  ((OB)L36)->header.tag=AM_NEW_EXPR_tag;
  L35 = ((AM_NEW_EXPR) L36);
  r1 = L35;
  SATTR(r1,source1,create_source);
  ret_val13 = r1;
  r = ret_val13;
  SATTR(r,tp_at,t);
  SATTR(r,asz,TRANS_693526804(self, ATTR(e,arg), ((dTP) TP_BUILTIN_int)));
  if ((ATTR(r,asz)==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 else {
  L37 = (ATTR(ATTR(self,impl),arr)==((TP_CLASS) NULL));
  L381_=!(L37); 
  if (L381_) {
   new_no_arg_array = self;
   new_no_arg_array1 = e;
   err_loc_self4 = new_no_arg_array;
   err_loc_t4 = ((dPROG_ERR) new_no_arg_array1);
   PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
   err_self4 = new_no_arg_array;
   err_s4 = ((STR) &newexp1208830217);
   if ((err_self4==((TRANS) NULL))) {
    L40 = TRUE;
   } else {
    L40 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L40) {
    L39 = TRUE;
   } else {
    L39 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L39) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self12 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg12 = ((STR) &name21);
    ret_val14 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val14;
    plus_sarg13 = err_s4;
    ret_val15 = STR_ap2004550586(plus_self13, plus_sarg13);
    err_s4 = ret_val15;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
  create_self1 = ((AM_NEW_EXPR) NULL);
  create_source1 = ATTR(e,source1);
  L42=ZALLOC(sizeof(struct AM_NEW_EXPR_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  ((OB)L42)->header.tag=AM_NEW_EXPR_tag;
  L41 = ((AM_NEW_EXPR) L42);
  r2 = L41;
  SATTR(r2,source1,create_source1);
  ret_val16 = r2;
  r = ret_val16;
  SATTR(r,tp_at,t);
 }
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_1147317392(TRANS self, AS_BOU14269336 as, AM_BND1124877163 r) {
 SIG fun11;
 ARRAYARG args;
 dTP t = ((dTP) NULL);
 dMODE m = ((dMODE) NULL);
 BOOL h = BOOL_zero;
 ARRAYBOOL hot = ((ARRAYBOOL) NULL);
 BOOL one_is_hot = BOOL_zero;
 ARRAYINT L11;
 INT i = INT_zero;
 ARRAYARG L21;
 ARRAYBOOL L31;
 ARRAYINT L41;
 INT i1 = INT_zero;
 ARRAYARG L51;
 ARRAYARG create_self;
 INT create_n = INT_zero;
 ARRAYARG ret_val;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 ARRAYBOOL create_self1;
 INT create_n1 = INT_zero;
 ARRAYBOOL ret_val3;
 INT L61 = INT_zero;
 SIG is_iter_self2;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 ARG create_self2;
 dTP create_t;
 dMODE create_m;
 ARG ret_val6;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val7;
 TP_TBL tp_iter_for_self;
 ARRAYARG tp_iter_for_args;
 ARRAYBOOL tp_iter_for_hot;
 dTP tp_iter_for_ret;
 TP_ITER ret_val8;
 TP_ITER r1;
 TUPARR1759987776 create_self3 = TUPARR1759987776_zero;
 ARRAYARG create_at1;
 ARRAYBOOL create_at2;
 dTP create_at3;
 TUPARR1759987776 ret_val9 = TUPARR1759987776_zero;
 INT L71 = INT_zero;
 ARG create_self4;
 dTP create_t1;
 dMODE create_m1;
 ARG ret_val10;
 ARG init_self1;
 dTP init_t1;
 dMODE init_m1;
 BOOL init_same1 = BOOL_zero;
 ARG ret_val11;
 TP_TBL tp_f_rout_for_se;
 ARRAYARG tp_f_rout_for_ar;
 dTP tp_f_rout_for_re;
 TP_ROUT ret_val12;
 TP_ROUT r2;
 TUPARRAYARGdTP create_self5 = TUPARRAYARGdTP_zero;
 ARRAYARG create_at11;
 dTP create_at21;
 TUPARRAYARGdTP ret_val13 = TUPARRAYARGdTP_zero;
 TP_TBL tp_rout_for_self;
 ARRAYARG tp_rout_for_args;
 dTP tp_rout_for_ret;
 TP_ROUT ret_val14;
 TP_ROUT r3;
 TUPARRAYARGdTP create_self6 = TUPARRAYARGdTP_zero;
 ARRAYARG create_at12;
 dTP create_at22;
 TUPARRAYARGdTP ret_val15 = TUPARRAYARGdTP_zero;
 ARRAYINT L8;
 INT L91_;
 ARRAYARG L10;
 INT L12;
 OB L13;
 INT L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 STR L18;
 INT L19;
 INT L201_;
 INT L22;
 CHAR L231_;
 CHAR L24;
 BOOL L251_;
 ARRAYINT L26;
 INT L271_;
 ARRAYBOOL L28;
 INT L29;
 OB L30;
 INT L32;
 INT L331_br;
 INT aL331_;
 INT L341_br;
INT i_L341_=0;
 INT L351_br;
INT i_L351_=0;
 INT L36;
 BOOL L371_;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 STR L42;
 INT L43;
 INT L441_;
 INT L45;
 CHAR L461_;
 CHAR L47;
 BOOL L481_;
 ARRAYINT L49;
 INT L501_;
 INT L52;
 BOOL L531_;
 ARRAYARG L54;
 INT L551_;
 INT L56;
 ARG L571_;
 ARG L58;
 ARRAYARG L59;
 INT L601_;
 INT L62;
 ARG L631_;
 ARG L64;
 ARG L65;
 OB L66;
 ARG L67;
 BOOL L68;
 BOOL L691_;
 BOOL L701_;
 ARRAYBOOL L72;
 INT L731_;
 INT L74;
 BOOL L751_;
 BOOL L76;
 INT L771_;
 BOOL L781_;
 TUPARR1759987776 L79;
 TUPARR1759987776 L80;
 TUPARR1759987776 L81;
 INT L821_br;
 INT aL821_;
 INT L831_br;
INT i_L831_=0;
 INT L84;
 BOOL L851_;
 ARRAYARG L86;
 INT L871_;
 INT L88;
 ARG L891_;
 ARG L90;
 ARRAYARG L92;
 INT L931_;
 INT L94;
 ARG L951_;
 ARG L96;
 ARG L97;
 OB L98;
 ARG L99;
 INT L1001_;
 TUPARRAYARGdTP L101;
 TUPARRAYARGdTP L102;
 TUPARRAYARGdTP L103;
 TUPARRAYARGdTP L104;
 fun11 = ATTR(r,fun);
 create_self = ((ARRAYARG) NULL);
 L8 = ATTR(r,unbnd_args);
 L91_=(L8)==NULL?0:ASIZE((ARRAYINT)L8); 
 create_n = L91_;
 L12 = create_n;
 L14=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L12)*sizeof(ARG);
 L13=ZALLOC_BIG(L14);
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L13)->header.destroyed=0;
#endif

 L10 = ((ARRAYARG) L13);
 L10->asize = L12;
 ret_val = L10;
 args = ret_val;
 is_iter_self = fun11;
 is_iter_self1 = ATTR(is_iter_self,name1);
 L16 = (is_iter_self1.str==((STR) NULL));
 L171_=!(L16); 
 if (L171_) {
  L18 = is_iter_self1.str;
  L19 = STR_sizerINT(is_iter_self1.str);
  L201_=INTMINUS(L19,1); 
  L22 = L201_;
  L231_=ARR((STR)L18,L22); 
  L24 = L231_;
  L251_=L24=='!'; 
  L15 = L251_;
 } else {
  L15 = FALSE;
 }
 ret_val2 = L15;
 ret_val1 = ret_val2;
 if (ret_val1) {
  one_is_hot = FALSE;
  if ((ATTR(fun11,hot)==((ARRAYBOOL) NULL))) {
   hot = ((ARRAYBOOL) NULL);
  }
  else {
   create_self1 = ((ARRAYBOOL) NULL);
   L26 = ATTR(r,unbnd_args);
   L271_=(L26)==NULL?0:ASIZE((ARRAYINT)L26); 
   create_n1 = L271_;
   L29 = create_n1;
   L32=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L29)*sizeof(BOOL);
   L30=ZALLOC_LEAF_BIG(L32);
   if (L30==NULL) FATAL("Unable to allocate more memory");
   memset(L30,0,L32);
   ((OB)L30)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

     ((OB)L30)->header.destroyed=0;
#endif

   L28 = ((ARRAYBOOL) L30);
   L28->asize = L29;
   ret_val3 = L28;
   hot = ret_val3;
  }
  {
   BOOL f_L331_ = TRUE;
   BOOL f_L341_ = TRUE;
   BOOL f_L351_ = TRUE;
   /* loop index variable */
   L61 = 0;
   L11 = ATTR(r,unbnd_args);
   L21 = args;
   L31 = hot;
   L331_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
   L341_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
   i_L341_=0;
   L351_br=L31==NULL?0:ASIZE((ARRAYBOOL)L31); 
   i_L351_=0;
   while (1) {
    if(L61>=L331_br)  goto after_loop; 
    aL331_=ARR((ARRAYINT)L11,L61); 
    i = aL331_;
    L371_=(i)==(0); 
    if (L371_) {
     t = ATTR(fun11,tp);
     is_iter_self2 = fun11;
     is_iter_self3 = ATTR(is_iter_self2,name1);
     L39 = (is_iter_self3.str==((STR) NULL));
     L401_=!(L39); 
     if (L401_) {
      L42 = is_iter_self3.str;
      L43 = STR_sizerINT(is_iter_self3.str);
      L441_=INTMINUS(L43,1); 
      L45 = L441_;
      L461_=ARR((STR)L42,L45); 
      L47 = L461_;
      L481_=L47=='!'; 
      L38 = L481_;
     } else {
      L38 = FALSE;
     }
     ret_val5 = L38;
     ret_val4 = ret_val5;
     if (ret_val4) {
      L49 = ATTR(r,unbnd_args);
      L501_=ARR((ARRAYINT)L49,0); 
      L52 = L501_;
      L531_=(L52)==(0); 
      if (L531_) {
       m = ((dMODE) ONCE_M704734922(((ONCE_MODE) NULL)));
      }
     }
     else {
      m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
     }
    }
    else {
     L54 = ATTR(fun11,args);
     L551_=INTMINUS(i,1); 
     L56 = L551_;
     L571_=(ARG)ARR((ARRAYARG)L54,L56); 
     L58=L571_;
     t = ATTR(L58,tp);
     L59 = ATTR(fun11,args);
     L601_=INTMINUS(i,1); 
     L62 = L601_;
     L631_=(ARG)ARR((ARRAYARG)L59,L62); 
     L64=L631_;
     m = ATTR(L64,mode1);
    }
    if(i_L341_>=L341_br)  goto after_loop; 
    create_self2 = ((ARG) NULL);
    create_t = t;
    create_m = m;
    L66=ZALLOC(sizeof(struct ARG_struct));
    if (L66==NULL) FATAL("Unable to allocate more memory");
    ((OB)L66)->header.tag=ARG_tag;
    L65 = ((ARG) L66);
    init_self = L65;
    init_t = create_t;
    init_m = create_m;
    init_same = FALSE;
    SATTR(init_self,tp,init_t);
    SATTR(init_self,mode1,init_m);
    SATTR(init_self,issame,init_same);
    ret_val7 = init_self;
    ret_val6 = ret_val7;
    L67 = ret_val6;
    SARR((ARRAYARG)L21,i_L341_,(ARG)L67); i_L341_++;
    ;
    L68 = (ATTR(fun11,hot)==((ARRAYBOOL) NULL));
    L691_=!(L68); 
    if (L691_) {
     L701_=(i)==(0); 
     if (L701_) {
      h = FALSE;
     }
     else {
      L72 = ATTR(fun11,hot);
      L731_=INTMINUS(i,1); 
      L74 = L731_;
      L751_=ARR((ARRAYBOOL)L72,L74); 
      h = L751_;
     }
     if(i_L351_>=L351_br)  goto after_loop; 
     SARR((ARRAYBOOL)L31,i_L351_,h); i_L351_++;
     ;
     if (one_is_hot) {
      L76 = TRUE;
     } else {
      L76 = h;
     }
     one_is_hot = L76;
    }
    L771_=INTPLUS(L61,1); 
    L61 = L771_;
   }
  }
  after_loop: ;
  L781_=!(one_is_hot); 
  if (L781_) {
   hot = ((ARRAYBOOL) NULL);
  }
  tp_iter_for_self = ATTR(ATTR(self,prog),tp_tbl);
  tp_iter_for_args = args;
  tp_iter_for_hot = hot;
  tp_iter_for_ret = ATTR(fun11,ret);
  create_self3 = TUPARR1759987776_zero;
  create_at1 = tp_iter_for_args;
  create_at2 = tp_iter_for_hot;
  create_at3 = tp_iter_for_ret;
  L79 = create_self3;
  L79.t1 = create_at1;
  L80 = L79;
  L80.t2 = create_at2;
  L81 = L80;
  L81.t3 = create_at3;
  ret_val9 = L81;
  r1 = TP_ITE990128539(ATTR(tp_iter_for_self,iter_tbl), ret_val9);
  if ((r1==((TP_ITER) NULL))) {
   r1 = TP_ITE1350567874(((TP_ITER) NULL), tp_iter_for_args, tp_iter_for_hot, tp_iter_for_ret, ATTR(tp_iter_for_self,prog));
   SATTR(tp_iter_for_self,iter_tbl,TP_ITE992470229(ATTR(tp_iter_for_self,iter_tbl), r1));
  }
  ret_val8 = r1;
  SATTR(r,tp_at,((dTP) ret_val8));
 }
 else {
  {
   BOOL f_L821_ = TRUE;
   BOOL f_L831_ = TRUE;
   /* loop index variable */
   L71 = 0;
   L41 = ATTR(r,unbnd_args);
   L51 = args;
   L821_br=L41==NULL?0:ASIZE((ARRAYINT)L41); 
   L831_br=L51==NULL?0:ASIZE((ARRAYARG)L51); 
   i_L831_=0;
   while (1) {
    if(L71>=L821_br)  goto after_loop1; 
    aL821_=ARR((ARRAYINT)L41,L71); 
    i1 = aL821_;
    L851_=(i1)==(0); 
    if (L851_) {
     t = ATTR(fun11,tp);
     m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
    }
    else {
     L86 = ATTR(fun11,args);
     L871_=INTMINUS(i1,1); 
     L88 = L871_;
     L891_=(ARG)ARR((ARRAYARG)L86,L88); 
     L90=L891_;
     t = ATTR(L90,tp);
     L92 = ATTR(fun11,args);
     L931_=INTMINUS(i1,1); 
     L94 = L931_;
     L951_=(ARG)ARR((ARRAYARG)L92,L94); 
     L96=L951_;
     m = ATTR(L96,mode1);
    }
    if(i_L831_>=L831_br)  goto after_loop1; 
    create_self4 = ((ARG) NULL);
    create_t1 = t;
    create_m1 = m;
    L98=ZALLOC(sizeof(struct ARG_struct));
    if (L98==NULL) FATAL("Unable to allocate more memory");
    ((OB)L98)->header.tag=ARG_tag;
    L97 = ((ARG) L98);
    init_self1 = L97;
    init_t1 = create_t1;
    init_m1 = create_m1;
    init_same1 = FALSE;
    SATTR(init_self1,tp,init_t1);
    SATTR(init_self1,mode1,init_m1);
    SATTR(init_self1,issame,init_same1);
    ret_val11 = init_self1;
    ret_val10 = ret_val11;
    L99 = ret_val10;
    SARR((ARRAYARG)L51,i_L831_,(ARG)L99); i_L831_++;
    ;
    L1001_=INTPLUS(L71,1); 
    L71 = L1001_;
   }
  }
  after_loop1: ;
  if (ATTR(as,is_f_rout)) {
   tp_f_rout_for_se = ATTR(ATTR(self,prog),tp_tbl);
   tp_f_rout_for_ar = args;
   tp_f_rout_for_re = ATTR(fun11,ret);
   create_self5 = TUPARRAYARGdTP_zero;
   create_at11 = tp_f_rout_for_ar;
   create_at21 = tp_f_rout_for_re;
   L101 = create_self5;
   L101.t1 = create_at11;
   L102 = L101;
   L102.t2 = create_at21;
   ret_val13 = L102;
   r2 = TP_ROU581494640(ATTR(tp_f_rout_for_se,f_rout_tbl), ret_val13);
   if ((r2==((TP_ROUT) NULL))) {
    r2 = TP_ROU52430373(((TP_ROUT) NULL), tp_f_rout_for_ar, tp_f_rout_for_re, ATTR(tp_f_rout_for_se,prog));
    SATTR(r2,is_fortran,TRUE);
    SATTR(tp_f_rout_for_se,f_rout_tbl,TP_ROU1247655203(ATTR(tp_f_rout_for_se,f_rout_tbl), r2));
   }
   ret_val12 = r2;
   SATTR(r,tp_at,((dTP) ret_val12));
  }
  else {
   tp_rout_for_self = ATTR(ATTR(self,prog),tp_tbl);
   tp_rout_for_args = args;
   tp_rout_for_ret = ATTR(fun11,ret);
   create_self6 = TUPARRAYARGdTP_zero;
   create_at12 = tp_rout_for_args;
   create_at22 = tp_rout_for_ret;
   L103 = create_self6;
   L103.t1 = create_at12;
   L104 = L103;
   L104.t2 = create_at22;
   ret_val15 = L104;
   r3 = TP_ROU581494640(ATTR(tp_rout_for_self,rout_tbl), ret_val15);
   if ((r3==((TP_ROUT) NULL))) {
    r3 = TP_ROU52430373(((TP_ROUT) NULL), tp_rout_for_args, tp_rout_for_ret, ATTR(tp_rout_for_self,prog));
    SATTR(tp_rout_for_self,rout_tbl,TP_ROU1247655203(ATTR(tp_rout_for_self,rout_tbl), r3));
   }
   ret_val14 = r3;
   SATTR(r,tp_at,((dTP) ret_val14));
  }
 }
}
