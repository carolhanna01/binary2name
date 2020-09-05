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

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

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

typedef struct ARRAYAM_CALL_ARG_struct {/* layout for ARRAY{AM_CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *ARRAYAM_CALL_ARG;

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

typedef struct CALL_TP_ARRAY_struct {/* layout for CALL_TP_ARRAY */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_ARRAY;

typedef struct CALL_T561792367_struct {/* layout for CALL_TP_BOUND_CREATE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_T561792367;

typedef struct CALL_TP_CREATE_struct {/* layout for CALL_TP_CREATE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_CREATE;

typedef struct CALL_T666389079_struct {/* layout for CALL_TP_UNDERSCORE */
 OB_HEADER header;
 struct dTP_struct *tp;
 } *CALL_T666389079;

typedef struct CALL_TP_VOID_struct {/* layout for CALL_TP_VOID */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_VOID;

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

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

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

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

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

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

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

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ARR535890289_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR535890289_frame;

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

typedef struct AM_BND1502113605_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1502113605_frame;

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

typedef struct AM_BND956869000_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND956869000_frame;

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

typedef struct AM_EXT150734657_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT150734657_frame;

typedef struct AM_IF_EXPR_struct {/* layout for AM_IF_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *if_false;
 struct dAM_EXPR_struct *if_true;
 struct dAM_EXPR_struct *test1;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IF_EXPR;

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

typedef struct AM_ROU176159907_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU176159907_frame;

typedef struct AM_ROU545684847_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU545684847_frame;

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

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

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

typedef struct ARRAYA537529312_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA537529312_frame;

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYC1058209184_frame_struct {
 INT state;
 ARRAYCALL_ARG self;/* Formal argument: self */
 CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1058209184_frame;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

typedef struct AS_ATTR_DEF_struct {/* layout for AS_ATTR_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_ATTR_DEF;

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

typedef struct CALL_SIG_struct {/* layout for CALL_SIG */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ARRAYCALL_ARG_struct *args;
 struct IDENT_struct name1;
 BOOL has_ret;
 BOOL unknown_ret;
 } *CALL_SIG;

typedef struct CS_OPTIONS_struct {/* layout for CS_OPTIONS */
 OB_HEADER header;
 struct ARRAYSTR_struct *arg_locations;
 struct ARRAYSTR_struct *args;
 struct CGEN_struct *cgen;
 struct CS_struct *cs;
 struct FMAPSTRSTR_struct *module_inclusion;
 struct FSETSTR_struct *arith_in;
 struct FSETSTR_struct *arith_out;
 struct FSETSTR_struct *assert_in;
 struct FSETSTR_struct *assert_out;
 struct FSETSTR_struct *bounds_in;
 struct FSETSTR_struct *bounds_out;
 struct FSETSTR_struct *classes1;
 struct FSETSTR_struct *invariant_in;
 struct FSETSTR_struct *invariant_out;
 struct FSETSTR_struct *paths;
 struct FSETSTR_struct *post_in;
 struct FSETSTR_struct *post_out;
 struct FSETSTR_struct *pre_in;
 struct FSETSTR_struct *pre_out;
 struct FSETSTR_struct *return_in;
 struct FSETSTR_struct *return_out;
 struct FSETSTR_struct *void_in;
 struct FSETSTR_struct *void_out;
 struct FSETSTR_struct *when_in;
 struct FSETSTR_struct *when_out;
 struct INLINE_struct *inliner;
 INT cache_size;
 INT cache_slot_size;
 INT local_call_acces;
 INT local_call_dynam;
 INT next;
 INT prefetch_weight;
 INT remote_call_acce;
 struct PARSE_struct *parser;
 struct PROG_struct *prog;
 BOOL all1;
 BOOL arith_all;
 BOOL assert_all;
 BOOL bounds_all;
 BOOL cache1;
 BOOL cse;
 BOOL cse_debug;
 BOOL debug_C_seen;
 BOOL debug_source_see;
 BOOL destroy_chk;
 BOOL got_libs;
 BOOL invariant_all;
 BOOL local_call;
 BOOL local_call_dynam1;
 BOOL local_opt;
 BOOL loop_poll;
 BOOL loop_prefetch;
 BOOL parloops;
 BOOL post_all;
 BOOL post_write;
 BOOL pre_all;
 BOOL prefetch;
 BOOL psather_chk;
 BOOL psather_stats;
 BOOL psather_trace;
 BOOL remote_call;
 BOOL remote_call_crea;
 BOOL return_all;
 BOOL side_debug;
 BOOL side_effects;
 BOOL specul_prefetch;
 BOOL stats;
 BOOL void_all;
 BOOL when_all;
 } *CS_OPTIONS;

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTA1062334999_struct {/* layout for FLIST{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *FLISTA1062334999;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA2119642552_frame_struct {
 INT state;
 FLISTA725283029 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA725283029 aget_self;
 INT aget_ind;
 AM_LOCAL_EXPR ret_val;
 } *FLISTA2119642552_frame;

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

extern CALL_T561792367 CALL_T714618297;
extern CALL_TP_ARRAY CALL_T1969158839;
extern CALL_TP_CREATE CALL_T1560699295;
extern CALL_TP_VOID CALL_T2135634286;
extern IDENT IDENT_1140439567;
extern IDENT IDENT_667688077;
extern INOUT_MODE MODES_inout_mode;
extern INT AS_ARG1872943786;
extern INT AS_ARG685418392;
extern INT AS_TYP1853679690;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_ext_c_tp;
extern OUT_MODE MODES_out_mode;

/* Function declarations */


extern RETURNED_CONST AM_CALL_ARG (**dAM_CA967858872)(dAM_CALL_EXPR, INT);

extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST INT (**dAM_CA1128544926)(dAM_CALL_EXPR);

extern RETURNED_CONST SFILE_ID (**dAM_CA1298533540)(dAM_CALL_EXPR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_EXPR (**dINLIN17214190)(dINLINE, AM_ITE1809135850);

extern RETURNED_CONST dAM_EXPR (**dINLIN970826022)(dINLINE, AM_ROU1916046290);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dTP (**dAM_CA564811416)(dAM_CALL_EXPR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);
AM_BND367211769 TRANS_745553885(TRANS, AM_BND367211769, SIG);
AM_CALL_ARG AM_BND1502113605(AM_BND1502113605_frame);
AM_CALL_ARG AM_BND956869000(AM_BND956869000_frame);
AM_CALL_ARG AM_CAL1030878588(AM_CALL_ARG);
AM_CALL_ARG AM_EXT150734657(AM_EXT150734657_frame);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_ITE1809135850 TRANS_1528108774(TRANS, AM_ITE1809135850);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_LOCAL_EXPR TRANS_1711874080(TRANS, IDENT);
AM_LOCAL_EXPR TRANS_598855940(TRANS, AS_CALL_EXPR, dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
BOOL ELT_is1808162451(ELT);
BOOL ELT_is1907475257(ELT);
BOOL ELT_is1936066670(ELT);
BOOL ELT_is380037919(ELT);
BOOL ELT_is507942138(ELT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL SIG_is1739708917(SIG);
BOOL SIG_is275586960(SIG);
BOOL SIG_is418491101(SIG);
BOOL SIG_is600379186(SIG);
BOOL TRANS_1319121910(TRANS, SIG);
BOOL TRANS_1865855576(TRANS, dAM_EXPR);
BOOL TRANS_1951149652(TRANS, dAM_CALL_EXPR);
BOOL TRANS_292955251(TRANS, SIG);
BOOL TRANS_792319540(TRANS, SIG);
ELT ELT_TB1949891380(ELT_TBL, SIG);
FLISTA1062334999 FLISTA572736043(FLISTA1062334999, FLISTA1062334999);
FLISTA1062334999 FLISTA943034044(FLISTA1062334999, AM_ITE1809135850);
FLISTA1409846210 FLISTA1236996208(FLISTA1409846210, FLISTA1409846210);
FLISTA1409846210 FLISTA1715286767(FLISTA1409846210, AM_BND367211769);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA811588375(FLISTA725283029, FLISTA725283029);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
GENERATE_AM TRANS_1867726633(TRANS);
IDENT IDENT_1150413730(IDENT, STR);
IDENT TRANS_1721028300(TRANS, ELT);
IDENT TRANS_885329442(TRANS, AS_CALL_EXPR);
IMPL ELT_implrIMPL(ELT);
IMPL TP_CLA1109727233(TP_CLASS);
INT ARRAYA537529312(ARRAYA537529312_frame);
INT AS_CAL1853175953(AS_CALL_EXPR);
INT INT_up418511718(INT_up418511718_frame);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
SE_CONTEXT SE_CON32319044(SE_CONTEXT, PROG, SIG);
SIG CALL_S2120587481(CALL_SIG, BOOL);
SIG IFC_si178998979(IFC, CALL_SIG);
SIG TRANS_1831541612(TRANS, AS_CALL_EXPR, CALL_SIG, ARRAYAM_CALL_ARG, BOOL);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
TRANS TRANS_214014142(TRANS, ELT);
TUPdAM_EXPRdTP TRANS_331125130(TRANS, AS_CALL_EXPR);
dAM_EXPR AM_ARR535890289(AM_ARR535890289_frame);
dAM_EXPR TRANS_1705639890(TRANS, AS_CALL_EXPR, dTP, BOOL);
dAM_EXPR TRANS_2076826648(TRANS, AM_ITE1809135850);
dAM_EXPR TRANS_593837946(TRANS, AS_SELF_EXPR, dTP);
dAM_EXPR TRANS_693526804(TRANS, dAS_EXPR, dTP);
dAM_EXPR TRANS_711092593(TRANS, dAM_CALL_EXPR);
dCALL_TP TRANS_68137339(TRANS, dAS_EXPR);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void AM_ROU545684847(AM_ROU545684847_frame);
void ARRAYC1058209184(ARRAYC1058209184_frame);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void SE_CON1222696490(SE_CONTEXT, dAM, BOOL);
void SE_CON412941612(SE_CONTEXT, dAM);
void TRANS_317173573(TRANS, dAM_EXPR);
void TRANS_350528331(TRANS);
void TRANS_507974720(TRANS, AS_CALL_EXPR);
void TRANS_626807411(TRANS, AM_LOCAL_EXPR);
void TRANS_74748323(TRANS, AS_TYPE_SPEC);
void TRANS_81467988(TRANS, SIG);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_BND367211769 TRANS_745553885(TRANS self, AM_BND367211769 bir, SIG sig1) {
 AM_BND367211769 ret_val;
 INT i = INT_zero;
 BOOL aonce = BOOL_zero;
 AM_LOCAL_EXPR nl1;
 AM_ASSIGN_STMT ass;
 TRANS bnd_iter_call_co;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS bnd_iter_call_ou;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self4;
 INT plus_arg = INT_zero;
 STR ret_val5;
 STR s;
 INT str_self = INT_zero;
 STR ret_val6;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val7;
 FSTR str_self1;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val9;
 TRANS err_self2;
 STR err_s2;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val11;
 TRANS bnd_iter_call_in;
 INT bnd_iter_call_in1 = INT_zero;
 STR plus_self8;
 INT plus_arg1 = INT_zero;
 STR ret_val12;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val13;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val14;
 FSTR str_self3;
 STR ret_val15;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val16;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val17;
 TRANS err_self3;
 STR err_s3;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val18;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val19;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val20;
 AM_LOCAL_EXPR r;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val21;
 AM_ASSIGN_STMT r1;
 BOOL L1;
 extern STR Boundi122936614;
 BOOL L2;
 BOOL L3;
 extern STR name21;
 extern STR Boundi238890286;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 INT L61_;
 INT L7;
 BOOL L81_;
 AM_CALL_ARG L91_;
 AM_CALL_ARG L10;
 extern STR Compil1718354347;
 BOOL L11;
 BOOL L121_;
 extern STR exprvoid;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 ARRAYBOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 AM_CALL_ARG L191_;
 AM_CALL_ARG L20;
 extern STR Argument;
 BOOL L21;
 BOOL L221_;
 extern STR ofthis353885841;
 extern STR aoncea731344550;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 AM_CALL_ARG L251_;
 AM_CALL_ARG L26;
 dAM_EXPR L27;
 AM_LOCAL_EXPR L28;
 OB L29;
 AM_ASSIGN_STMT L30;
 OB L31;
 AM_CALL_ARG L321_;
 AM_CALL_ARG L33;
 AM_CALL_ARG L341_;
 dAM_STMT L35;
 INT L361_;
 if ((bir==((AM_BND367211769) NULL))) {
  L1 = TRUE;
 } else {
  L1 = (sig1==((SIG) NULL));
 }
 if (L1) {
  ret_val = ((AM_BND367211769) NULL);
  return ret_val;
 }
 if (ATTR(self,in_constant)) {
  bnd_iter_call_co = self;
  err_self = bnd_iter_call_co;
  err_s = ((STR) &Boundi122936614);
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
  ret_val = ((AM_BND367211769) NULL);
  return ret_val;
 }
 if ((ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL))) {
  bnd_iter_call_ou = self;
  err_self1 = bnd_iter_call_ou;
  err_s1 = ((STR) &Boundi238890286);
  if ((err_self1==((TRANS) NULL))) {
   L5 = TRUE;
  } else {
   L5 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L5) {
   L4 = TRUE;
  } else {
   L4 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
  }
  if (L4) {
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
  ret_val = ((AM_BND367211769) NULL);
  return ret_val;
 }
 SATTR(bir,lp,ATTR(self,cur_loop));
 i = 0;
 while (1) {
  L61_=(bir)==NULL?0:ASIZE((AM_BND367211769)bir); 
  L7 = L61_;
  L81_=(i)<(L7); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  L91_=(AM_CALL_ARG)ARR((AM_BND367211769)bir,i); 
  L10=L91_;
  if ((ATTR(L10,expr)==((dAM_EXPR) NULL))) {
   err_loc_self = self;
   err_loc_t = ((dPROG_ERR) bir);
   PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
   err_self2 = self;
   plus_self4 = ((STR) &Compil1718354347);
   plus_arg = i;
   str_self = plus_arg;
   clear_self = buf1;
   L11 = (clear_self==((FSTR) NULL));
   L121_=!(L11); 
   if (L121_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val7 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val7;
   str_self1 = buf1;
   ret_val8 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val6 = ret_val8;
   s = ret_val6;
   ret_val5 = STR_ap1077157958(plus_self4, s, TRUE);
   plus_self5 = ret_val5;
   plus_sarg4 = ((STR) &exprvoid);
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg4);
   err_s2 = ret_val9;
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
    plus_sarg5 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self6, plus_sarg5);
    plus_self7 = ret_val10;
    plus_sarg6 = err_s2;
    ret_val11 = STR_ap2004550586(plus_self7, plus_sarg6);
    err_s2 = ret_val11;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   ret_val = ((AM_BND367211769) NULL);
   return ret_val;
  }
  aonce = FALSE;
  if ((ATTR(sig1,hot)==((ARRAYBOOL) NULL))) {
   aonce = TRUE;
  }
  else {
   L15 = ATTR(sig1,hot);
   L161_=ARR((ARRAYBOOL)L15,i); 
   L17 = L161_;
   L181_=!(L17); 
   if (L181_) {
    aonce = TRUE;
   }
  }
  if (aonce) {
   L191_=(AM_CALL_ARG)ARR((AM_BND367211769)bir,i); 
   L20=L191_;
   if (TRANS_1865855576(self, ATTR(L20,expr))) {
    bnd_iter_call_in = self;
    bnd_iter_call_in1 = i;
    err_self3 = bnd_iter_call_in;
    plus_self8 = ((STR) &Argument);
    plus_arg1 = bnd_iter_call_in1;
    str_self2 = plus_arg1;
    clear_self1 = buf2;
    L21 = (clear_self1==((FSTR) NULL));
    L221_=!(L21); 
    if (L221_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self2;
    str_in_s1 = buf2;
    ret_val14 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val14;
    str_self3 = buf2;
    ret_val15 = STR_fr1097270334(((STR) NULL), str_self3);
    ret_val13 = ret_val15;
    s1 = ret_val13;
    ret_val12 = STR_ap1077157958(plus_self8, s1, TRUE);
    plus_self9 = ret_val12;
    plus_sarg7 = ((STR) &ofthis353885841);
    ret_val16 = STR_ap2004550586(plus_self9, plus_sarg7);
    plus_self10 = ret_val16;
    plus_sarg8 = ((STR) &aoncea731344550);
    ret_val17 = STR_ap2004550586(plus_self10, plus_sarg8);
    err_s3 = ret_val17;
    if ((err_self3==((TRANS) NULL))) {
     L24 = TRUE;
    } else {
     L24 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L24) {
     L23 = TRUE;
    } else {
     L23 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
    }
    if (L23) {
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    else {
     plus_self11 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
     plus_sarg9 = ((STR) &name21);
     ret_val18 = STR_ap2004550586(plus_self11, plus_sarg9);
     plus_self12 = ret_val18;
     plus_sarg10 = err_s3;
     ret_val19 = STR_ap2004550586(plus_self12, plus_sarg10);
     err_s3 = ret_val19;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    ret_val = ((AM_BND367211769) NULL);
    return ret_val;
   }
   create_self = ((AM_LOCAL_EXPR) NULL);
   create_src = ATTR(bir,source1);
   create_name = IDENT_zero;
   L251_=(AM_CALL_ARG)ARR((AM_BND367211769)bir,i); 
   L26=L251_;
   L27 = ATTR(L26,expr);
   create_tp = (*dAM_EXPR_tprdTP[TAG(L27)])(L27);
   L29=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=AM_LOCAL_EXPR_tag;
   L28 = ((AM_LOCAL_EXPR) L29);
   r = L28;
   SATTR(r,source1,create_src);
   SATTR(r,name1,create_name);
   SATTR(r,tp_at,create_tp);
   ret_val20 = r;
   nl1 = ret_val20;
   TRANS_626807411(self, nl1);
   create_self1 = ((AM_ASSIGN_STMT) NULL);
   create_source = ATTR(bir,source1);
   L31=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
   if (L31==NULL) FATAL("Unable to allocate more memory");
   ((OB)L31)->header.tag=AM_ASSIGN_STMT_tag;
   L30 = ((AM_ASSIGN_STMT) L31);
   r1 = L30;
   SATTR(r1,source1,create_source);
   ret_val21 = r1;
   ass = ret_val21;
   SATTR(ass,dest,((dAM_EXPR) nl1));
   L321_=(AM_CALL_ARG)ARR((AM_BND367211769)bir,i); 
   L33=L321_;
   SATTR(ass,src1,ATTR(L33,expr));
   L341_=(AM_CALL_ARG)ARR((AM_BND367211769)bir,i); 
   SATTR(L341_,expr,((dAM_EXPR) nl1));
   if ((ATTR(bir,init)==((dAM_STMT) NULL))) {
    SATTR(bir,init,((dAM_STMT) ass));
   }
   else {
    L35 = ATTR(bir,init);
    (*dAM_ST2020072576[TAG(L35)])(L35, ((dAM_STMT) ass));
   }
  }
  L361_=INTPLUS(i,1); 
  i = L361_;
 }
 after_loop: ;
 SATTR(ATTR(self,cur_loop),bits1,FLISTA1715286767(ATTR(ATTR(self,cur_loop),bits1), bir));
 ret_val = bir;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ITE1809135850 TRANS_1528108774(TRANS self, AM_ITE1809135850 ir) {
 AM_ITE1809135850 ret_val;
 AM_LOCAL_EXPR nl1;
 AM_ASSIGN_STMT ass;
 INT i = INT_zero;
 BOOL aonce = BOOL_zero;
 TRANS iter_call_const_;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self2;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT create_self1;
 OUT ret_val5;
 OUT plus_self3;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 TRANS iter_call_in_onc;
 INT iter_call_in_onc1 = INT_zero;
 TRANS err_self1;
 STR err_s1;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val8;
 STR plus_self6;
 INT plus_arg = INT_zero;
 STR ret_val9;
 STR s;
 INT str_self = INT_zero;
 STR ret_val10;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val11;
 FSTR str_self1;
 STR ret_val12;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val13;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val14;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val15;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val16;
 AM_LOCAL_EXPR r2;
 AM_ASSIGN_STMT create_self3;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val17;
 AM_ASSIGN_STMT r3;
 OUT create_self4;
 OUT ret_val18;
 OUT plus_self10;
 STR plus_s2;
 OUT ret_val19;
 FILE1 stdout_self2;
 FILE1 ret_val20;
 FILE1 r4;
 OUT plus_self11;
 dSTR plus_s3;
 OUT ret_val21;
 FILE1 stdout_self3;
 FILE1 ret_val22;
 FILE1 r5;
 OUT plus_self12;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val23;
 FILE1 r6;
 TRANS iter_call_in_onc2;
 INT iter_call_in_onc3 = INT_zero;
 TRANS err_self3;
 STR err_s3;
 STR plus_self13;
 STR plus_sarg7;
 STR ret_val24;
 STR plus_self14;
 STR plus_sarg8;
 STR ret_val25;
 STR plus_self15;
 INT plus_arg1 = INT_zero;
 STR ret_val26;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val27;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val28;
 FSTR str_self3;
 STR ret_val29;
 STR plus_self16;
 STR plus_sarg9;
 STR ret_val30;
 TRANS err_self4;
 STR err_s4;
 STR plus_self17;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self18;
 STR plus_sarg11;
 STR ret_val32;
 AM_LOCAL_EXPR create_self5;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AM_LOCAL_EXPR ret_val33;
 AM_LOCAL_EXPR r7;
 AM_ASSIGN_STMT create_self6;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val34;
 AM_ASSIGN_STMT r8;
 extern STR Iterca2012547742;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 AM_CALL_ARG L31_;
 AM_CALL_ARG L4;
 extern STR Compil1396259414;
 FILE1 L5;
 OB L6;
 extern STR Compil1779756918;
 FILE1 L8;
 OB L9;
 AM_CALL_ARG L111_;
 AM_CALL_ARG L12;
 BOOL L131_;
 extern STR Theexp627865128;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 extern STR Theexp1372574939;
 BOOL L16;
 BOOL L171_;
 extern STR inthis1097895141;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 AM_CALL_ARG L201_;
 AM_CALL_ARG L21;
 dAM_EXPR L22;
 AM_LOCAL_EXPR L23;
 OB L24;
 AM_ASSIGN_STMT L25;
 OB L26;
 AM_CALL_ARG L271_;
 AM_CALL_ARG L28;
 AM_CALL_ARG L291_;
 INT L301_;
 INT L32;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 AM_CALL_ARG L381_;
 AM_CALL_ARG L39;
 extern STR Compil2017674967;
 FILE1 L40;
 OB L41;
 INT L431_;
 dSTR L44;
 OB L45;
 FILE1 L46;
 OB L47;
 extern STR exprvoid;
 FILE1 L49;
 OB L50;
 ARRAYBOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 INT L561_;
 INT L57;
 AM_CALL_ARG L581_;
 AM_CALL_ARG L59;
 INT L601_;
 BOOL L611_;
 extern STR Theexp627865128;
 BOOL L62;
 BOOL L63;
 extern STR name21;
 extern STR Theexp1372574939;
 BOOL L64;
 BOOL L651_;
 extern STR inthis1097895141;
 BOOL L66;
 BOOL L67;
 extern STR name21;
 INT L681_;
 INT L69;
 AM_CALL_ARG L701_;
 AM_CALL_ARG L72;
 dAM_EXPR L73;
 AM_LOCAL_EXPR L74;
 OB L75;
 AM_ASSIGN_STMT L76;
 OB L77;
 INT L781_;
 INT L79;
 AM_CALL_ARG L801_;
 AM_CALL_ARG L81;
 INT L821_;
 INT L83;
 AM_CALL_ARG L841_;
 dAM_STMT L85;
 INT L861_;
 if (ATTR(self,in_constant)) {
  iter_call_const_ = self;
  err_self = iter_call_const_;
  err_s = ((STR) &Iterca2012547742);
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
  ret_val = ((AM_ITE1809135850) NULL);
  return ret_val;
 }
 if ((ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL))) {
  TRANS_350528331(self);
  ret_val = ((AM_ITE1809135850) NULL);
  return ret_val;
 }
 SATTR(ir,lp,ATTR(self,cur_loop));
 L31_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,0); 
 L4=L31_;
 if ((ATTR(L4,expr)==((dAM_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val3 = create_self;
  plus_self2 = ret_val3;
  plus_s = ((STR) &Compil1396259414);
  stdout_self = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r = L5;
  
  SATTR(r,fp,stdout);
  ret_val4 = r;
  FILE_plus_STR(ret_val4, plus_s);
  ret_val = ((AM_ITE1809135850) NULL);
  return ret_val;
 }
 if ((ATTR(ir,fun)==((SIG) NULL))) {
  create_self1 = ((OUT) NULL);
  ret_val5 = create_self1;
  plus_self3 = ret_val5;
  plus_s1 = ((STR) &Compil1779756918);
  stdout_self1 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stdout);
  ret_val6 = r1;
  FILE_plus_STR(ret_val6, plus_s1);
  ret_val = ((AM_ITE1809135850) NULL);
  return ret_val;
 }
 L111_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,0); 
 L12=L111_;
 if (TRANS_1865855576(self, ATTR(L12,expr))) {
  iter_call_in_onc = self;
  iter_call_in_onc1 = 0;
  L131_=(iter_call_in_onc1)==(0); 
  if (L131_) {
   err_self1 = iter_call_in_onc;
   err_s1 = ((STR) &Theexp627865128);
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
    plus_sarg2 = ((STR) &name21);
    ret_val7 = STR_ap2004550586(plus_self4, plus_sarg2);
    plus_self5 = ret_val7;
    plus_sarg3 = err_s1;
    ret_val8 = STR_ap2004550586(plus_self5, plus_sarg3);
    err_s1 = ret_val8;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  else {
   err_self2 = iter_call_in_onc;
   plus_self6 = ((STR) &Theexp1372574939);
   plus_arg = iter_call_in_onc1;
   str_self = plus_arg;
   clear_self = buf1;
   L16 = (clear_self==((FSTR) NULL));
   L171_=!(L16); 
   if (L171_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val11 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val11;
   str_self1 = buf1;
   ret_val12 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val10 = ret_val12;
   s = ret_val10;
   ret_val9 = STR_ap1077157958(plus_self6, s, TRUE);
   plus_self7 = ret_val9;
   plus_sarg4 = ((STR) &inthis1097895141);
   ret_val13 = STR_ap2004550586(plus_self7, plus_sarg4);
   err_s2 = ret_val13;
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
    plus_self8 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
    plus_sarg5 = ((STR) &name21);
    ret_val14 = STR_ap2004550586(plus_self8, plus_sarg5);
    plus_self9 = ret_val14;
    plus_sarg6 = err_s2;
    ret_val15 = STR_ap2004550586(plus_self9, plus_sarg6);
    err_s2 = ret_val15;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
  }
  ret_val = ((AM_ITE1809135850) NULL);
  return ret_val;
 }
 create_self2 = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(ir,source1);
 create_name = IDENT_zero;
 L201_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,0); 
 L21=L201_;
 L22 = ATTR(L21,expr);
 create_tp = (*dAM_EXPR_tprdTP[TAG(L22)])(L22);
 L24=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=AM_LOCAL_EXPR_tag;
 L23 = ((AM_LOCAL_EXPR) L24);
 r2 = L23;
 SATTR(r2,source1,create_src);
 SATTR(r2,name1,create_name);
 SATTR(r2,tp_at,create_tp);
 ret_val16 = r2;
 nl1 = ret_val16;
 TRANS_626807411(self, nl1);
 create_self3 = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(ir,source1);
 L26=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 ((OB)L26)->header.tag=AM_ASSIGN_STMT_tag;
 L25 = ((AM_ASSIGN_STMT) L26);
 r3 = L25;
 SATTR(r3,source1,create_source);
 ret_val17 = r3;
 ass = ret_val17;
 SATTR(ass,dest,((dAM_EXPR) nl1));
 L271_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,0); 
 L28=L271_;
 SATTR(ass,src1,ATTR(L28,expr));
 L291_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,0); 
 SATTR(L291_,expr,((dAM_EXPR) nl1));
 SATTR(ir,init,((dAM_STMT) ass));
 i = 0;
 while (1) {
  L301_=(ir)==NULL?0:ASIZE((AM_ITE1809135850)ir); 
  L32 = L301_;
  L331_=INTMINUS(L32,1); 
  L34 = L331_;
  L351_=(i)<(L34); 
  if (L351_) {
  }
  else {
   goto after_loop;
  }
  L361_=INTPLUS(i,1); 
  L37 = L361_;
  L381_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,L37); 
  L39=L381_;
  if ((ATTR(L39,expr)==((dAM_EXPR) NULL))) {
   create_self4 = ((OUT) NULL);
   ret_val18 = create_self4;
   plus_self10 = ret_val18;
   plus_s2 = ((STR) &Compil2017674967);
   stdout_self2 = ((FILE1) NULL);
   L41=ZALLOC(sizeof(struct FILE1_struct));
   if (L41==NULL) FATAL("Unable to allocate more memory");
   ((OB)L41)->header.tag=FILE1_tag;
   L40 = ((FILE1) L41);
   r4 = L40;
   
   SATTR(r4,fp,stdout);
   ret_val20 = r4;
   FILE_plus_STR(ret_val20, plus_s2);
   ret_val19 = plus_self10;
   plus_self11 = ret_val19;
   L431_=INTPLUS(i,1); 
   L45=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   memset(L45,0,sizeof(struct INT_boxed_struct));
   ((OB)L45)->header.tag=INT_tag;
   L44 = (dSTR)((INT_boxed) L45);
   ((INT_boxed) L44)->immutable_part = L431_;
   plus_s3 = L44;
   stdout_self3 = ((FILE1) NULL);
   L47=ZALLOC(sizeof(struct FILE1_struct));
   if (L47==NULL) FATAL("Unable to allocate more memory");
   ((OB)L47)->header.tag=FILE1_tag;
   L46 = ((FILE1) L47);
   r5 = L46;
   
   SATTR(r5,fp,stdout);
   ret_val22 = r5;
   FILE_plus_dSTR(ret_val22, plus_s3);
   ret_val21 = plus_self11;
   plus_self12 = ret_val21;
   plus_s4 = ((STR) &exprvoid);
   stdout_self4 = ((FILE1) NULL);
   L50=ZALLOC(sizeof(struct FILE1_struct));
   if (L50==NULL) FATAL("Unable to allocate more memory");
   ((OB)L50)->header.tag=FILE1_tag;
   L49 = ((FILE1) L50);
   r6 = L49;
   
   SATTR(r6,fp,stdout);
   ret_val23 = r6;
   FILE_plus_STR(ret_val23, plus_s4);
   ret_val = ((AM_ITE1809135850) NULL);
   return ret_val;
  }
  aonce = FALSE;
  if ((ATTR(ATTR(ir,fun),hot)==((ARRAYBOOL) NULL))) {
   aonce = TRUE;
  }
  else {
   L52 = ATTR(ATTR(ir,fun),hot);
   L531_=ARR((ARRAYBOOL)L52,i); 
   L54 = L531_;
   L551_=!(L54); 
   if (L551_) {
    aonce = TRUE;
   }
  }
  if (aonce) {
   L561_=INTPLUS(i,1); 
   L57 = L561_;
   L581_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,L57); 
   L59=L581_;
   if (TRANS_1865855576(self, ATTR(L59,expr))) {
    iter_call_in_onc2 = self;
    L601_=INTPLUS(i,1); 
    iter_call_in_onc3 = L601_;
    L611_=(iter_call_in_onc3)==(0); 
    if (L611_) {
     err_self3 = iter_call_in_onc2;
     err_s3 = ((STR) &Theexp627865128);
     if ((err_self3==((TRANS) NULL))) {
      L63 = TRUE;
     } else {
      L63 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L63) {
      L62 = TRUE;
     } else {
      L62 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
     }
     if (L62) {
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
     else {
      plus_self13 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
      plus_sarg7 = ((STR) &name21);
      ret_val24 = STR_ap2004550586(plus_self13, plus_sarg7);
      plus_self14 = ret_val24;
      plus_sarg8 = err_s3;
      ret_val25 = STR_ap2004550586(plus_self14, plus_sarg8);
      err_s3 = ret_val25;
      PROG_err_STR(ATTR(err_self3,prog), err_s3);
     }
    }
    else {
     err_self4 = iter_call_in_onc2;
     plus_self15 = ((STR) &Theexp1372574939);
     plus_arg1 = iter_call_in_onc3;
     str_self2 = plus_arg1;
     clear_self1 = buf2;
     L64 = (clear_self1==((FSTR) NULL));
     L651_=!(L64); 
     if (L651_) {
      SATTR(clear_self1,loc,0);
     }
     str_in_self1 = str_self2;
     str_in_s1 = buf2;
     ret_val28 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
     buf2 = ret_val28;
     str_self3 = buf2;
     ret_val29 = STR_fr1097270334(((STR) NULL), str_self3);
     ret_val27 = ret_val29;
     s1 = ret_val27;
     ret_val26 = STR_ap1077157958(plus_self15, s1, TRUE);
     plus_self16 = ret_val26;
     plus_sarg9 = ((STR) &inthis1097895141);
     ret_val30 = STR_ap2004550586(plus_self16, plus_sarg9);
     err_s4 = ret_val30;
     if ((err_self4==((TRANS) NULL))) {
      L67 = TRUE;
     } else {
      L67 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L67) {
      L66 = TRUE;
     } else {
      L66 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
     }
     if (L66) {
      PROG_err_STR(ATTR(err_self4,prog), err_s4);
     }
     else {
      plus_self17 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
      plus_sarg10 = ((STR) &name21);
      ret_val31 = STR_ap2004550586(plus_self17, plus_sarg10);
      plus_self18 = ret_val31;
      plus_sarg11 = err_s4;
      ret_val32 = STR_ap2004550586(plus_self18, plus_sarg11);
      err_s4 = ret_val32;
      PROG_err_STR(ATTR(err_self4,prog), err_s4);
     }
    }
    ret_val = ((AM_ITE1809135850) NULL);
    return ret_val;
   }
   create_self5 = ((AM_LOCAL_EXPR) NULL);
   create_src1 = ATTR(ir,source1);
   create_name1 = IDENT_zero;
   L681_=INTPLUS(i,1); 
   L69 = L681_;
   L701_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,L69); 
   L72=L701_;
   L73 = ATTR(L72,expr);
   create_tp1 = (*dAM_EXPR_tprdTP[TAG(L73)])(L73);
   L75=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L75==NULL) FATAL("Unable to allocate more memory");
   ((OB)L75)->header.tag=AM_LOCAL_EXPR_tag;
   L74 = ((AM_LOCAL_EXPR) L75);
   r7 = L74;
   SATTR(r7,source1,create_src1);
   SATTR(r7,name1,create_name1);
   SATTR(r7,tp_at,create_tp1);
   ret_val33 = r7;
   nl1 = ret_val33;
   TRANS_626807411(self, nl1);
   create_self6 = ((AM_ASSIGN_STMT) NULL);
   create_source1 = ATTR(ir,source1);
   L77=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
   if (L77==NULL) FATAL("Unable to allocate more memory");
   ((OB)L77)->header.tag=AM_ASSIGN_STMT_tag;
   L76 = ((AM_ASSIGN_STMT) L77);
   r8 = L76;
   SATTR(r8,source1,create_source1);
   ret_val34 = r8;
   ass = ret_val34;
   SATTR(ass,dest,((dAM_EXPR) nl1));
   L781_=INTPLUS(i,1); 
   L79 = L781_;
   L801_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,L79); 
   L81=L801_;
   SATTR(ass,src1,ATTR(L81,expr));
   L821_=INTPLUS(i,1); 
   L83 = L821_;
   L841_=(AM_CALL_ARG)ARR((AM_ITE1809135850)ir,L83); 
   SATTR(L841_,expr,((dAM_EXPR) nl1));
   if ((ATTR(ir,init)==((dAM_STMT) NULL))) {
    SATTR(ir,init,((dAM_STMT) ass));
   }
   else {
    L85 = ATTR(ir,init);
    (*dAM_ST2020072576[TAG(L85)])(L85, ((dAM_STMT) ass));
   }
  }
  L861_=INTPLUS(i,1); 
  i = L861_;
 }
 after_loop: ;
 ret_val = ir;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR TRANS_1711874080(TRANS self, IDENT n) {
 AM_LOCAL_EXPR ret_val;
 AM_ROUT_DEF L11;
 AM_FORMAL_ARG r;
 FLISTA725283029 L21;
 AM_LOCAL_EXPR r1;
 INT L31 = INT_zero;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r2;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r3;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 INT L41_br;
 AM_FORMAL_ARG aL41_;
 AM_FORMAL_ARG L5;
 extern STR Compil816288381;
 FILE1 L6;
 OB L7;
 INT L91_;
 AM_LOCAL_EXPR L10;
 extern STR Compil816288381;
 FILE1 L12;
 OB L13;
 {
  BOOL f_L41_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,cur_rout);
  L41_br=L11==NULL?0:ASIZE((AM_ROUT_DEF)L11); 
  while (1) {
   if(L31>=L41_br)  goto after_loop; 
   aL41_=ARR((AM_ROUT_DEF)L11,L31); 
   r = aL41_;
   if ((r==((AM_FORMAL_ARG) NULL))) {
    create_self = ((OUT) NULL);
    ret_val1 = create_self;
    plus_self = ret_val1;
    plus_s = ((STR) &Compil816288381);
    stdout_self = ((FILE1) NULL);
    L7=ZALLOC(sizeof(struct FILE1_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=FILE1_tag;
    L6 = ((FILE1) L7);
    r2 = L6;
    
    SATTR(r2,fp,stdout);
    ret_val2 = r2;
    FILE_plus_STR(ret_val2, plus_s);
    ret_val = ((AM_LOCAL_EXPR) NULL);
    return ret_val;
   }
   is_eq_self = ATTR(ATTR(r,expr),name1);
   is_eq_i = n;
   ret_val3 = (is_eq_self.str==is_eq_i.str);
   if (ret_val3) {
    ret_val = ATTR(r,expr);
    return ret_val;
   }
   L91_=INTPLUS(L31,1); 
   L31 = L91_;
  }
 }
 after_loop: ;
 {
  struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname1 = &other2_0;
  L21 = ATTR(self,active_locals);
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   L10 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   r1 = L10;
   if ((r1==((AM_LOCAL_EXPR) NULL))) {
    create_self1 = ((OUT) NULL);
    ret_val4 = create_self1;
    plus_self1 = ret_val4;
    plus_s1 = ((STR) &Compil816288381);
    stdout_self1 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r3 = L12;
    
    SATTR(r3,fp,stdout);
    ret_val5 = r3;
    FILE_plus_STR(ret_val5, plus_s1);
    ret_val = ((AM_LOCAL_EXPR) NULL);
    return ret_val;
   }
   is_eq_self1 = ATTR(r1,name1);
   is_eq_i1 = n;
   ret_val6 = (is_eq_self1.str==is_eq_i1.str);
   if (ret_val6) {
    ret_val = r1;
    return ret_val;
   }
  }
 }
 after_loop1: ;
 ret_val = ((AM_LOCAL_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR TRANS_598855940(TRANS self, AS_CALL_EXPR e, dTP tp) {
 AM_LOCAL_EXPR ret_val;
 dAS_EXPR self_as;
 AM_LOCAL_EXPR r = ((AM_LOCAL_EXPR) NULL);
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val3;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val4;
 TRANS call_local_conte;
 AS_CALL_EXPR call_local_conte1;
 dTP call_local_conte2;
 dTP call_local_conte3;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
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
 extern STR Compil483515429;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 dTP L12;
 BOOL L13;
 BOOL L141_;
 extern STR Thetyp1357282984;
 dTP L15;
 extern STR isnotasubtypeof;
 dTP L16;
 extern STR name18;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 if (ATTR(self,in_constant)) {
  ret_val = ((AM_LOCAL_EXPR) NULL);
  return ret_val;
 }
 if ((e==((AS_CALL_EXPR) NULL))) {
  err_self = self;
  err_s = ((STR) &Compil483515429);
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
  ret_val = ((AM_LOCAL_EXPR) NULL);
  return ret_val;
 }
 self_as = ATTR(e,ob);
 if ((self_as==((dAS_EXPR) NULL))) {
  L5 = (ATTR(e,tp)==((AS_TYPE_SPEC) NULL));
 } else {
  L5 = FALSE;
 }
 if (L5) {
  L4 = (ATTR(e,args)==((dAS_EXPR) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L6 = ATTR(e,is_array);
  L71_=L6==FALSE; 
  L3 = L71_;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  r = TRANS_1711874080(self, ATTR(e,name1));
  L8 = (r==((AM_LOCAL_EXPR) NULL));
  L91_=!(L8); 
  if (L91_) {
   L10 = (tp==((dTP) NULL));
   L111_=!(L10); 
   if (L111_) {
    tp_self = r;
    ret_val3 = ATTR(tp_self,tp_at);
    L12 = ret_val3;
    L13 = (*dTP_is1999456142[TAG(L12)])(L12, tp);
    L141_=!(L13); 
    if (L141_) {
     call_local_conte = self;
     call_local_conte1 = e;
     tp_self1 = r;
     ret_val4 = ATTR(tp_self1,tp_at);
     call_local_conte2 = ret_val4;
     call_local_conte3 = tp;
     err_loc_self = call_local_conte;
     err_loc_t = ((dPROG_ERR) call_local_conte1);
     PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
     err_self1 = call_local_conte;
     plus_self2 = ((STR) &Thetyp1357282984);
     L15 = call_local_conte2;
     plus_sarg2 = (*dTP_strrSTR[TAG(L15)])(L15);
     ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val5;
     plus_sarg3 = ((STR) &isnotasubtypeof);
     ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self4 = ret_val6;
     L16 = call_local_conte3;
     plus_sarg4 = (*dTP_strrSTR[TAG(L16)])(L16);
     ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val7;
     plus_sarg5 = ((STR) &name18);
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
     ret_val = ((AM_LOCAL_EXPR) NULL);
     return ret_val;
    }
   }
  }
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1319121910(TRANS self, SIG s) {
 BOOL ret_val = BOOL_zero;
 dTP stp;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 ARRAYdTP L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 if ((s==((SIG) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 stp = ATTR(s,tp);
 if (stp==NULL) {
  goto other400;
 } else
 switch (TAG(stp)) {
  case TP_CLASS_tag:
   L1 = (ATTR(((TP_CLASS) stp),params)==((ARRAYdTP) NULL));
   L21_=!(L1); 
   if (L21_) {
    L3 = ATTR(((TP_CLASS) stp),params);
    L41_=(L3)==NULL?0:ASIZE((ARRAYdTP)L3); 
    L5 = L41_;
    L61_=(L5)==(1); 
    L7 = L61_;
    L81_=!(L7); 
    if (L81_) {
     ret_val = FALSE;
     return ret_val;
    }
   }
   is_eq_self = ATTR(s,name1);
   is_eq_i = IDENT_1140439567;
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   L9 = ret_val1;
   L101_=!(L9); 
   if (L101_) {
    ret_val = FALSE;
    return ret_val;
   }
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other400: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1865855576(TRANS self, dAM_EXPR e) {
 BOOL ret_val = BOOL_zero;
 AM_ROU1916046290 L11;
 AM_ARRAY_EXPR L21;
 AM_BND1124877163 L31;
 AM_BND260301329 L41;
 AM_EXT_CALL_EXPR L51;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT L121_br;
 AM_CALL_ARG aL121_;
 AM_CALL_ARG L13;
 AM_CALL_ARG L14;
 INT L151_;
 INT L161_br;
 dAM_EXPR aL161_;
 dAM_EXPR L17;
 INT L181_;
 INT L191_br;
 AM_CALL_ARG aL191_;
 AM_CALL_ARG L20;
 AM_CALL_ARG L22;
 INT L231_;
 INT L241_br;
 AM_CALL_ARG aL241_;
 AM_CALL_ARG L25;
 AM_CALL_ARG L26;
 INT L271_;
 BOOL L28;
 BOOL L29;
 INT L301_br;
 AM_CALL_ARG aL301_;
 AM_CALL_ARG L32;
 AM_CALL_ARG L33;
 INT L341_;
 if ((e==((dAM_EXPR) NULL))) {
  ret_val = BOOL_zero;
  return ret_val;
 }
 if (e==NULL) {
  goto other399;
 } else
 switch (TAG(e)) {
  case AM_ROU1916046290_tag:
   {
    BOOL f_L121_ = TRUE;
    /* loop index variable */
    L61 = 0;
    L11 = ((AM_ROU1916046290) e);
    L121_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
    while (1) {
     if(L61>=L121_br)  goto after_loop; 
     aL121_=ARR((AM_ROU1916046290)L11,L61); 
     L14=aL121_;
     if (TRANS_1865855576(self, ATTR(L14,expr))) {
      ret_val = TRUE;
      return ret_val;
     }
     L151_=INTPLUS(L61,1); 
     L61 = L151_;
    }
   }
   after_loop: ; break;
  case AM_ITE1809135850_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_ARRAY_EXPR_tag:
   {
    BOOL f_L161_ = TRUE;
    /* loop index variable */
    L71 = 0;
    L21 = ((AM_ARRAY_EXPR) e);
    L161_br=L21==NULL?0:ASIZE((AM_ARRAY_EXPR)L21); 
    while (1) {
     if(L71>=L161_br)  goto after_loop1; 
     aL161_=ARR((AM_ARRAY_EXPR)L21,L71); 
     if (TRANS_1865855576(self, aL161_)) {
      ret_val = TRUE;
      return ret_val;
     }
     L181_=INTPLUS(L71,1); 
     L71 = L181_;
    }
   }
   after_loop1: ; break;
  case AM_BND1124877163_tag:
   {
    BOOL f_L191_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L31 = ((AM_BND1124877163) e);
    L191_br=L31==NULL?0:ASIZE((AM_BND1124877163)L31); 
    while (1) {
     if(L81>=L191_br)  goto after_loop2; 
     aL191_=ARR((AM_BND1124877163)L31,L81); 
     L22=aL191_;
     if (TRANS_1865855576(self, ATTR(L22,expr))) {
      ret_val = TRUE;
      return ret_val;
     }
     L231_=INTPLUS(L81,1); 
     L81 = L231_;
    }
   }
   after_loop2: ; break;
  case AM_BND260301329_tag:
   {
    BOOL f_L241_ = TRUE;
    /* loop index variable */
    L91 = 0;
    L41 = ((AM_BND260301329) e);
    L241_br=L41==NULL?0:ASIZE((AM_BND260301329)L41); 
    while (1) {
     if(L91>=L241_br)  goto after_loop3; 
     aL241_=ARR((AM_BND260301329)L41,L91); 
     L26=aL241_;
     if (TRANS_1865855576(self, ATTR(L26,expr))) {
      ret_val = TRUE;
      return ret_val;
     }
     L271_=INTPLUS(L91,1); 
     L91 = L271_;
    }
   }
   after_loop3: ; break;
  case AM_BND367211769_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_IF_EXPR_tag:
   if (TRANS_1865855576(self, ATTR(((AM_IF_EXPR) e),test1))) {
    L29 = TRUE;
   } else {
    L29 = TRANS_1865855576(self, ATTR(((AM_IF_EXPR) e),if_true));
   }
   if (L29) {
    L28 = TRUE;
   } else {
    L28 = TRANS_1865855576(self, ATTR(((AM_IF_EXPR) e),if_false));
   }
   if (L28) {
    ret_val = TRUE;
    return ret_val;
   } break;
  case AM_IS_VOID_EXPR_tag:
   if (TRANS_1865855576(self, ATTR(((AM_IS_VOID_EXPR) e),arg))) {
    ret_val = TRUE;
    return ret_val;
   } break;
  case AM_NEW_EXPR_tag:
   if (TRANS_1865855576(self, ATTR(((AM_NEW_EXPR) e),asz))) {
    ret_val = TRUE;
    return ret_val;
   } break;
  case AM_ATTR_EXPR_tag:
   if (TRANS_1865855576(self, ATTR(((AM_ATTR_EXPR) e),ob))) {
    ret_val = TRUE;
    return ret_val;
   } break;
  case AM_EXT_CALL_EXPR_tag:
   {
    BOOL f_L301_ = TRUE;
    /* loop index variable */
    L101 = 0;
    L51 = ((AM_EXT_CALL_EXPR) e);
    L301_br=L51==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L51); 
    while (1) {
     if(L101>=L301_br)  goto after_loop4; 
     aL301_=ARR((AM_EXT_CALL_EXPR)L51,L101); 
     L33=aL301_;
     if (TRANS_1865855576(self, ATTR(L33,expr))) {
      ret_val = TRUE;
      return ret_val;
     }
     L341_=INTPLUS(L101,1); 
     L101 = L341_;
    }
   }
   after_loop4: ; break;
  default: ;
  other399: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1951149652(TRANS self, dAM_CALL_EXPR cr) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 AM_CALL_ARG a;
 dAM_EXPR ae;
 SIG is_reader_sig_se;
 BOOL ret_val1 = BOOL_zero;
 INT L4;
 dAM_CALL_EXPR L5;
 INT L6;
 INT L71_;
 INT L31_,L31_m;
 dAM_CALL_EXPR L8;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 {
  BOOL f_L31_ = TRUE;
  while (1) {
   if (f_L31_) {
    f_L31_ = FALSE;
    L11 = 0;
    L5 = cr;
    L6 = (*dAM_CA1128544926[TAG(L5)])(L5);
    L71_=INTMINUS(L6,1); 
    L21 = L71_;
    L31_=L11-1;L31_m=L21; 
   }
   if(L31_++>=L31_m)  goto after_loop; 
   i = L31_;
   L8 = cr;
   a = (*dAM_CA967858872[TAG(L8)])(L8, i);
   ae = ATTR(a,expr);
   if (ae==NULL) {
    goto other401;
   } else
   switch (TAG(ae)) {
    case AM_ROU1916046290_tag:
     if (SYSOBEQ(((OB) ATTR(a,mode1)),((OB) MODES_out_mode))) {
      L9 = TRUE;
     } else {
      L9 = SYSOBEQ(((OB) ATTR(a,mode1)),((OB) MODES_inout_mode));
     }
     if (L9) {
      if (TRANS_1319121910(self, ATTR(((AM_ROU1916046290) ae),fun))) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_reader_sig_se = ATTR(((AM_ROU1916046290) ae),fun);
       if ((ATTR(is_reader_sig_se,args)==((ARRAYARG) NULL))) {
        L12 = (ATTR(is_reader_sig_se,ret)==((dTP) NULL));
        L131_=!(L12); 
        L10 = L131_;
       } else {
        L10 = FALSE;
       }
       ret_val1 = L10;
       if (ret_val1) {
        ret_val = TRUE;
        return ret_val;
       }
      }
     } break;
    case AM_ATTR_EXPR_tag:
     if (SYSOBEQ(((OB) ATTR(a,mode1)),((OB) MODES_out_mode))) {
      L14 = TRUE;
     } else {
      L14 = SYSOBEQ(((OB) ATTR(a,mode1)),((OB) MODES_inout_mode));
     }
     if (L14) {
      ret_val = TRUE;
      return ret_val;
     } break;
    default: ;
    other401: ;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_292955251(TRANS self, SIG s) {
 BOOL ret_val = BOOL_zero;
 dTP tp;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L1;
 extern STR SYS1;
 extern STR import1;
 tp = ATTR(s,tp);
 if (tp==NULL) {
  goto other396;
 } else
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   is_eq_self = ATTR(((TP_CLASS) tp),name1);
   is_eq_i = IDENT_1150413730(IDENT_zero, ((STR) &SYS1));
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   if (ret_val1) {
    is_eq_self1 = ATTR(s,name1);
    is_eq_i1 = IDENT_1150413730(IDENT_zero, ((STR) &import1));
    ret_val2 = (is_eq_self1.str==is_eq_i1.str);
    L1 = ret_val2;
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  default: ;
  other396: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_792319540(TRANS self, SIG s) {
 BOOL ret_val = BOOL_zero;
 dTP tp;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L1;
 extern STR SYS1;
 extern STR export2;
 tp = ATTR(s,tp);
 if (tp==NULL) {
  goto other397;
 } else
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   is_eq_self = ATTR(((TP_CLASS) tp),name1);
   is_eq_i = IDENT_1150413730(IDENT_zero, ((STR) &SYS1));
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   if (ret_val1) {
    is_eq_self1 = ATTR(s,name1);
    is_eq_i1 = IDENT_1150413730(IDENT_zero, ((STR) &export2));
    ret_val2 = (is_eq_self1.str==is_eq_i1.str);
    L1 = ret_val2;
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  default: ;
  other397: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

GENERATE_AM TRANS_1867726633(TRANS self) {
 GENERATE_AM ret_val;
 dGENERATE_AM gen;
 gen = ATTR(ATTR(self,prog),generate_am);
 switch (TAG(gen)) {
  case GENERATE_AM_tag:
   ret_val = ((GENERATE_AM) gen);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::gen:GENERATE_AM\n./Middle/trans.sa:213:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

IDENT TRANS_1721028300(TRANS self, ELT el) {
 IDENT ret_val = IDENT_zero;
 ELT tp_self;
 dTP ret_val1;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val5;
 BOOL L1;
 BOOL L2;
 dTP L3;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 if (ELT_is1907475257(el)) {
  L2 = TRUE;
 } else {
  L2 = SIG_is275586960(ATTR(el,sig1));
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L1 = SIG_is600379186(ATTR(el,sig1));
 }
 if (L1) {
  ret_val = ATTR(ATTR(el,sig1),name1);
  return ret_val;
 }
 else {
  tp_self = el;
  ret_val1 = ATTR(ATTR(tp_self,sig1),tp);
  L3 = ret_val1;
  plus_self = (*dTP_strrSTR[TAG(L3)])(L3);
  plus_arg = '_';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L5 = 1;
  L7=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((STR) L6);
  L4->asize = L5;
  r = L4;
  SARR((STR)r,0,create_c); 
  ;
  ret_val4 = r;
  ret_val3 = ret_val4;
  s = ret_val3;
  ret_val2 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val2;
  plus_sarg = ATTR(ATTR(el,sig1),name1).str;
  ret_val5 = STR_ap2004550586(plus_self1, plus_sarg);
  ret_val = IDENT_1150413730(IDENT_zero, ret_val5);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

IDENT TRANS_885329442(TRANS self, AS_CALL_EXPR e) {
 IDENT ret_val = IDENT_zero;
 if (ATTR(e,is_array)) {
  ret_val = IDENT_1140439567;
  return ret_val;
 }
 else {
  ret_val = ATTR(e,name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SIG TRANS_1831541612(TRANS self, AS_CALL_EXPR e, CALL_SIG call_sig, ARRAYAM_CALL_ARG args, BOOL in_class) {
 SIG ret_val;
 dAS_EXPR a;
 AS_ARG_MODE m;
 INT i = INT_zero;
 dAM_EXPR ce = ((dAM_EXPR) NULL);
 dMODE md = ((dMODE) NULL);
 dCALL_TP ct;
 SIG r;
 dTP at;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 CALL_ARG create_self;
 dCALL_TP create_t;
 dMODE create_m;
 CALL_ARG ret_val5;
 CALL_ARG res;
 AM_CALL_ARG create_self1;
 dAM_EXPR create_e;
 dMODE create_m1;
 AM_CALL_ARG ret_val6;
 AM_CALL_ARG res1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e1;
 dMODE create_m2;
 AM_CALL_ARG ret_val9;
 AM_CALL_ARG res2;
 BOOL L1;
 BOOL L21_;
 extern STR Compil534809471;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 BOOL L5;
 INT L61_;
 INT L7;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 ARRAYCALL_ARG L12;
 INT L131_;
 INT L14;
 INT L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 extern STR Compil534809471;
 BOOL L19;
 BOOL L20;
 extern STR name21;
 BOOL L22;
 BOOL L231_;
 dAM_EXPR L24;
 ARRAYCALL_ARG L25;
 CALL_ARG L26;
 OB L27;
 CALL_ARG L28;
 AM_CALL_ARG L30;
 OB L31;
 AM_CALL_ARG L32;
 dAS_EXPR L34;
 INT L351_;
 ARRAYARG L36;
 INT L371_;
 INT L38;
 INT L39;
 BOOL L401_;
 BOOL L41;
 BOOL L421_;
 extern STR Compil1237838286;
 BOOL L43;
 BOOL L44;
 extern STR name21;
 BOOL L45;
 BOOL L461_;
 AM_CALL_ARG L471_;
 AM_CALL_ARG L48;
 ARRAYARG L49;
 ARG L501_;
 ARG L51;
 AM_CALL_ARG L52;
 OB L53;
 AM_CALL_ARG L54;
 dAS_EXPR L56;
 INT L571_;
 if ((args==((ARRAYAM_CALL_ARG) NULL))) {
  L1 = (ATTR(e,args)==((dAS_EXPR) NULL));
  L21_=!(L1); 
  if (L21_) {
   err_self = self;
   err_s = ((STR) &Compil534809471);
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
   ret_val = ((SIG) NULL);
   return ret_val;
  }
 }
 else {
  L61_=(args)==NULL?0:ASIZE((ARRAYAM_CALL_ARG)args); 
  L7 = L61_;
  L8 = AS_CAL1853175953(e);
  L91_=(L7)==(L8); 
  L10 = L91_;
  L111_=!(L10); 
  if (L111_) {
   L5 = TRUE;
  } else {
   L12 = ATTR(call_sig,args);
   L131_=(L12)==NULL?0:ASIZE((ARRAYCALL_ARG)L12); 
   L14 = L131_;
   L15 = AS_CAL1853175953(e);
   L161_=(L14)==(L15); 
   L17 = L161_;
   L181_=!(L17); 
   L5 = L181_;
  }
  if (L5) {
   err_self1 = self;
   err_s1 = ((STR) &Compil534809471);
   if ((err_self1==((TRANS) NULL))) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L19) {
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
   ret_val = ((SIG) NULL);
   return ret_val;
  }
 }
 a = ATTR(e,args);
 m = ATTR(e,modes1);
 i = 0;
 while (1) {
  L22 = (a==((dAS_EXPR) NULL));
  L231_=!(L22); 
  if (L231_) {
  }
  else {
   goto after_loop;
  }
  ct = TRANS_68137339(self, a);
  if ((ct==((dCALL_TP) NULL))) {
   ce = TRANS_693526804(self, a, ((dTP) NULL));
   if ((ce==((dAM_EXPR) NULL))) {
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   else {
    L24 = ce;
    ct = ((dCALL_TP) (*dAM_EXPR_tprdTP[TAG(L24)])(L24));
   }
  }
  else {
   ce = ((dAM_EXPR) NULL);
  }
  md = MODE_c998271775(((MODE) NULL), m);
  L25 = ATTR(call_sig,args);
  create_self = ((CALL_ARG) NULL);
  create_t = ct;
  create_m = md;
  L27=ZALLOC(sizeof(struct CALL_ARG_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=CALL_ARG_tag;
  L26 = ((CALL_ARG) L27);
  res = L26;
  SATTR(res,tp,create_t);
  SATTR(res,mode1,create_m);
  ret_val5 = res;
  L28 = ret_val5;
  SARR((ARRAYCALL_ARG)L25,i,(CALL_ARG)L28); 
  ;
  create_self1 = ((AM_CALL_ARG) NULL);
  create_e = ce;
  create_m1 = md;
  L31=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=AM_CALL_ARG_tag;
  L30 = ((AM_CALL_ARG) L31);
  res1 = L30;
  SATTR(res1,expr,create_e);
  SATTR(res1,mode1,create_m1);
  ret_val6 = res1;
  L32 = ret_val6;
  SARR((ARRAYAM_CALL_ARG)args,i,(AM_CALL_ARG)L32); 
  ;
  L34 = a;
  a = (*dAS_EX337431651[TAG(L34)])(L34);
  m = ATTR(m,next);
  L351_=INTPLUS(i,1); 
  i = L351_;
 }
 after_loop: ;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) e);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 r = CALL_S2120587481(call_sig, in_class);
 if ((r==((SIG) NULL))) {
  ret_val = ((SIG) NULL);
  return ret_val;
 }
 L36 = ATTR(r,args);
 L371_=(L36)==NULL?0:ASIZE((ARRAYARG)L36); 
 L38 = L371_;
 L39 = AS_CAL1853175953(e);
 L401_=(L38)==(L39); 
 L41 = L401_;
 L421_=!(L41); 
 if (L421_) {
  err_self2 = self;
  err_s2 = ((STR) &Compil1237838286);
  if ((err_self2==((TRANS) NULL))) {
   L44 = TRUE;
  } else {
   L44 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L44) {
   L43 = TRUE;
  } else {
   L43 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
  }
  if (L43) {
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
  ret_val = ((SIG) NULL);
  return ret_val;
 }
 a = ATTR(e,args);
 m = ATTR(e,modes1);
 i = 0;
 while (1) {
  L45 = (a==((dAS_EXPR) NULL));
  L461_=!(L45); 
  if (L461_) {
  }
  else {
   goto after_loop1;
  }
  L471_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i); 
  L48=L471_;
  ce = ATTR(L48,expr);
  L49 = ATTR(r,args);
  L501_=(ARG)ARR((ARRAYARG)L49,i); 
  L51=L501_;
  at = ATTR(L51,tp);
  if ((ce==((dAM_EXPR) NULL))) {
   ce = TRANS_693526804(self, a, at);
  }
  if ((ce==((dAM_EXPR) NULL))) {
   ret_val = ((SIG) NULL);
   return ret_val;
  }
  create_self2 = ((AM_CALL_ARG) NULL);
  create_e1 = ce;
  create_m2 = MODE_c998271775(((MODE) NULL), m);
  L53=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
  if (L53==NULL) FATAL("Unable to allocate more memory");
  ((OB)L53)->header.tag=AM_CALL_ARG_tag;
  L52 = ((AM_CALL_ARG) L53);
  res2 = L52;
  SATTR(res2,expr,create_e1);
  SATTR(res2,mode1,create_m2);
  ret_val9 = res2;
  L54 = ret_val9;
  SARR((ARRAYAM_CALL_ARG)args,i,(AM_CALL_ARG)L54); 
  ;
  L56 = a;
  a = (*dAS_EX337431651[TAG(L56)])(L56);
  m = ATTR(m,next);
  L571_=INTPLUS(i,1); 
  i = L571_;
 }
 after_loop1: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TRANS TRANS_214014142(TRANS self, ELT e) {
 TRANS ret_val;
 TRANS r;
 ELT prog_self;
 PROG ret_val1;
 TRANS set_chks_self;
 STR n;
 dGET_OPTIONS options1;
 CS_OPTIONS pre_chk_self;
 STR pre_chk_n;
 BOOL ret_val2 = BOOL_zero;
 CS_OPTIONS post_chk_self;
 STR post_chk_n;
 BOOL ret_val3 = BOOL_zero;
 CS_OPTIONS invariant_chk_se;
 STR invariant_chk_n;
 BOOL ret_val4 = BOOL_zero;
 CS_OPTIONS assert_chk_self;
 STR assert_chk_n;
 BOOL ret_val5 = BOOL_zero;
 TRANS L1;
 OB L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L18;
 BOOL L19;
 BOOL L201_;
 L2=ZALLOC(sizeof(struct TRANS_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=TRANS_tag;
 L1 = ((TRANS) L2);
 r = L1;
 prog_self = e;
 ret_val1 = ATTR(ATTR(prog_self,con),prog);
 SATTR(r,prog,ret_val1);
 SATTR(r,impl,ELT_implrIMPL(e));
 SATTR(r,tp_con,ATTR(e,con));
 SATTR(r,cur_se,SE_CON32319044(((SE_CONTEXT) NULL), ATTR(r,prog), ATTR(e,sig1)));
 if ((ATTR(r,impl)==((IMPL) NULL))) {
  L3 = TRUE;
 } else {
  L3 = (ATTR(r,tp_con)==((TP_CONTEXT) NULL));
 }
 if (L3) {
  ret_val = ((TRANS) NULL);
  return ret_val;
 }
 set_chks_self = r;
 L4 = ATTR(ATTR(set_chks_self,impl),tp);
 n = (*dTP_strrSTR[TAG(L4)])(L4);
 options1 = ATTR(ATTR(set_chks_self,prog),get_options);
 switch (TAG(options1)) {
  case CS_OPTIONS_tag:
   pre_chk_self = ((CS_OPTIONS) options1);
   pre_chk_n = n;
   if (ATTR(pre_chk_self,pre_all)) {
    L6 = TRUE;
   } else {
    L6 = FSETST1025458804(ATTR(pre_chk_self,pre_in), pre_chk_n);
   }
   if (L6) {
    L7 = FSETST1025458804(ATTR(pre_chk_self,pre_out), pre_chk_n);
    L81_=!(L7); 
    L5 = L81_;
   } else {
    L5 = FALSE;
   }
   ret_val2 = L5;
   SATTR(set_chks_self,chk_pre,ret_val2);
   post_chk_self = ((CS_OPTIONS) options1);
   post_chk_n = n;
   if (ATTR(post_chk_self,post_all)) {
    L10 = TRUE;
   } else {
    L10 = FSETST1025458804(ATTR(post_chk_self,post_in), post_chk_n);
   }
   if (L10) {
    L11 = FSETST1025458804(ATTR(post_chk_self,post_out), post_chk_n);
    L121_=!(L11); 
    L9 = L121_;
   } else {
    L9 = FALSE;
   }
   ret_val3 = L9;
   SATTR(set_chks_self,chk_post,ret_val3);
   invariant_chk_se = ((CS_OPTIONS) options1);
   invariant_chk_n = n;
   if (ATTR(invariant_chk_se,invariant_all)) {
    L14 = TRUE;
   } else {
    L14 = FSETST1025458804(ATTR(invariant_chk_se,invariant_in), invariant_chk_n);
   }
   if (L14) {
    L15 = FSETST1025458804(ATTR(invariant_chk_se,invariant_out), invariant_chk_n);
    L161_=!(L15); 
    L13 = L161_;
   } else {
    L13 = FALSE;
   }
   ret_val4 = L13;
   SATTR(set_chks_self,chk_invariant,ret_val4);
   assert_chk_self = ((CS_OPTIONS) options1);
   assert_chk_n = n;
   if (ATTR(assert_chk_self,assert_all)) {
    L18 = TRUE;
   } else {
    L18 = FSETST1025458804(ATTR(assert_chk_self,assert_in), assert_chk_n);
   }
   if (L18) {
    L19 = FSETST1025458804(ATTR(assert_chk_self,assert_out), assert_chk_n);
    L201_=!(L19); 
    L17 = L201_;
   } else {
    L17 = FALSE;
   }
   ret_val5 = L17;
   SATTR(set_chks_self,chk_assert,ret_val5); break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::create(ELT):SAME\n./Middle/trans.sa:89:14");
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TUPdAM_EXPRdTP TRANS_331125130(TRANS self, AS_CALL_EXPR e) {
 TUPdAM_EXPRdTP ret_val = TUPdAM_EXPRdTP_zero;
 dAS_EXPR self_as;
 dAM_EXPR self_val;
 dAM_EXPR self_val1;
 AM_VOID_CONST av;
 dAM_EXPR self_val2 = ((dAM_EXPR) NULL);
 AM_VOID_CONST av1;
 TRANS call_self_void_e;
 AS_CALL_EXPR call_self_void_e1;
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
 TRANS call_self_create;
 AS_CALL_EXPR call_self_create1;
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
 TUPdAM_EXPRdTP create_self = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at1;
 dTP create_at2;
 TUPdAM_EXPRdTP ret_val5 = TUPdAM_EXPRdTP_zero;
 TRANS call_self_array_;
 AS_CALL_EXPR call_self_array_1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 TRANS call_self_unders;
 AS_CALL_EXPR call_self_unders1;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 TUPdAM_EXPRdTP create_self1 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at11;
 dTP create_at21;
 TUPdAM_EXPRdTP ret_val10 = TUPdAM_EXPRdTP_zero;
 AM_VOID_CONST create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VOID_CONST ret_val11;
 AM_VOID_CONST r;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val12;
 TUPdAM_EXPRdTP create_self3 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at12;
 dTP create_at22;
 TUPdAM_EXPRdTP ret_val13 = TUPdAM_EXPRdTP_zero;
 AM_VOID_CONST create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_VOID_CONST ret_val14;
 AM_VOID_CONST r1;
 AM_ROUT_DEF self_local_self;
 AM_LOCAL_EXPR ret_val15;
 OUT create_self5;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val17;
 FILE1 r2;
 TUPdAM_EXPRdTP create_self6 = TUPdAM_EXPRdTP_zero;
 dAM_EXPR create_at13;
 dTP create_at23;
 TUPdAM_EXPRdTP ret_val18 = TUPdAM_EXPRdTP_zero;
 BOOL L1;
 BOOL L21_;
 extern STR Callsm1564266979;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR Callsm1819579645;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 dAM_EXPR L7;
 TUPdAM_EXPRdTP L8;
 TUPdAM_EXPRdTP L9;
 extern STR Callsm1636158155;
 BOOL L10;
 BOOL L11;
 extern STR name21;
 extern STR Unders445425447;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 dAM_EXPR L14;
 TUPdAM_EXPRdTP L15;
 TUPdAM_EXPRdTP L16;
 BOOL L17;
 BOOL L181_;
 AM_VOID_CONST L19;
 OB L20;
 TUPdAM_EXPRdTP L22;
 TUPdAM_EXPRdTP L23;
 AM_VOID_CONST L24;
 OB L25;
 AM_FORMAL_ARG L261_;
 AM_FORMAL_ARG L27;
 extern STR Compil321489027;
 FILE1 L28;
 OB L29;
 dAM_EXPR L31;
 TUPdAM_EXPRdTP L32;
 TUPdAM_EXPRdTP L33;
 self_as = ATTR(e,ob);
 L1 = (self_as==((dAS_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (self_as==NULL) {
   goto other390;
  } else
  switch (TAG(self_as)) {
   case AS_VOID_EXPR_tag:
    call_self_void_e = self;
    call_self_void_e1 = e;
    err_loc_self = call_self_void_e;
    err_loc_t = ((dPROG_ERR) call_self_void_e1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = call_self_void_e;
    err_s = ((STR) &Callsm1564266979);
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
    ret_val = TUPdAM_EXPRdTP_zero;
    return ret_val; break;
   case AS_CREATE_EXPR_tag:
    if ((ATTR(((AS_CREATE_EXPR) self_as),tp)==((AS_TYPE_SPEC) NULL))) {
     call_self_create = self;
     call_self_create1 = e;
     err_loc_self1 = call_self_create;
     err_loc_t1 = ((dPROG_ERR) call_self_create1);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     err_self1 = call_self_create;
     err_s1 = ((STR) &Callsm1819579645);
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
     ret_val = TUPdAM_EXPRdTP_zero;
     return ret_val;
    }
    else {
     self_val = TRANS_693526804(self, self_as, ((dTP) NULL));
     if ((self_val==((dAM_EXPR) NULL))) {
      ret_val = TUPdAM_EXPRdTP_zero;
      return ret_val;
     }
     create_self = TUPdAM_EXPRdTP_zero;
     create_at1 = self_val;
     L7 = self_val;
     create_at2 = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
     L8 = create_self;
     L8.t1 = create_at1;
     L9 = L8;
     L9.t2 = create_at2;
     ret_val5 = L9;
     ret_val = ret_val5;
     return ret_val;
    } break;
   case AS_ARRAY_EXPR_tag:
    call_self_array_ = self;
    call_self_array_1 = e;
    err_loc_self2 = call_self_array_;
    err_loc_t2 = ((dPROG_ERR) call_self_array_1);
    PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
    err_self2 = call_self_array_;
    err_s2 = ((STR) &Callsm1636158155);
    if ((err_self2==((TRANS) NULL))) {
     L11 = TRUE;
    } else {
     L11 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L11) {
     L10 = TRUE;
    } else {
     L10 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L10) {
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    else {
     plus_self4 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
     plus_sarg4 = ((STR) &name21);
     ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val6;
     plus_sarg5 = err_s2;
     ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
     err_s2 = ret_val7;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    ret_val = TUPdAM_EXPRdTP_zero;
    return ret_val; break;
   case AS_UND152986614_tag:
    call_self_unders = self;
    call_self_unders1 = e;
    err_loc_self3 = call_self_unders;
    err_loc_t3 = ((dPROG_ERR) call_self_unders1);
    PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
    err_self3 = call_self_unders;
    err_s3 = ((STR) &Unders445425447);
    if ((err_self3==((TRANS) NULL))) {
     L13 = TRUE;
    } else {
     L13 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L13) {
     L12 = TRUE;
    } else {
     L12 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
    }
    if (L12) {
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
    ret_val = TUPdAM_EXPRdTP_zero;
    return ret_val; break;
   default: ;
   other390: ;
    self_val1 = TRANS_693526804(self, self_as, ((dTP) NULL));
    if ((self_val1==((dAM_EXPR) NULL))) {
     ret_val = TUPdAM_EXPRdTP_zero;
     return ret_val;
    }
    create_self1 = TUPdAM_EXPRdTP_zero;
    create_at11 = self_val1;
    L14 = self_val1;
    create_at21 = (*dAM_EXPR_tprdTP[TAG(L14)])(L14);
    L15 = create_self1;
    L15.t1 = create_at11;
    L16 = L15;
    L16.t2 = create_at21;
    ret_val10 = L16;
    ret_val = ret_val10;
    return ret_val;
  }
 }
 else {
  L17 = (ATTR(e,tp)==((AS_TYPE_SPEC) NULL));
  L181_=!(L17); 
  if (L181_) {
   create_self2 = ((AM_VOID_CONST) NULL);
   create_source = ATTR(e,source1);
   L20=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=AM_VOID_CONST_tag;
   L19 = ((AM_VOID_CONST) L20);
   r = L19;
   SATTR(r,source1,create_source);
   ret_val11 = r;
   av = ret_val11;
   tp_of_self = self;
   tp_of_t = ATTR(e,tp);
   ret_val12 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
   SATTR(av,tp_at,ret_val12);
   create_self3 = TUPdAM_EXPRdTP_zero;
   create_at12 = ((dAM_EXPR) av);
   create_at22 = ATTR(av,tp_at);
   L22 = create_self3;
   L22.t1 = create_at12;
   L23 = L22;
   L23.t2 = create_at22;
   ret_val13 = L23;
   ret_val = ret_val13;
   return ret_val;
  }
  else {
   if (ATTR(self,in_constant)) {
    create_self4 = ((AM_VOID_CONST) NULL);
    create_source1 = ATTR(e,source1);
    L25=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
    if (L25==NULL) FATAL("Unable to allocate more memory");
    ((OB)L25)->header.tag=AM_VOID_CONST_tag;
    L24 = ((AM_VOID_CONST) L25);
    r1 = L24;
    SATTR(r1,source1,create_source1);
    ret_val14 = r1;
    av1 = ret_val14;
    SATTR(av1,tp_at,((dTP) ATTR(ATTR(self,tp_con),same1)));
    self_val2 = ((dAM_EXPR) av1);
   }
   else {
    self_local_self = ATTR(self,cur_rout);
    L261_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)self_local_self,0); 
    L27=L261_;
    ret_val15 = ATTR(L27,expr);
    self_val2 = ((dAM_EXPR) ret_val15);
   }
   if ((self_val2==((dAM_EXPR) NULL))) {
    create_self5 = ((OUT) NULL);
    ret_val16 = create_self5;
    plus_self8 = ret_val16;
    plus_s = ((STR) &Compil321489027);
    stdout_self = ((FILE1) NULL);
    L29=ZALLOC(sizeof(struct FILE1_struct));
    if (L29==NULL) FATAL("Unable to allocate more memory");
    ((OB)L29)->header.tag=FILE1_tag;
    L28 = ((FILE1) L29);
    r2 = L28;
    
    SATTR(r2,fp,stdout);
    ret_val17 = r2;
    FILE_plus_STR(ret_val17, plus_s);
    ret_val = TUPdAM_EXPRdTP_zero;
    return ret_val;
   }
   create_self6 = TUPdAM_EXPRdTP_zero;
   create_at13 = self_val2;
   L31 = self_val2;
   create_at23 = (*dAM_EXPR_tprdTP[TAG(L31)])(L31);
   L32 = create_self6;
   L32.t1 = create_at13;
   L33 = L32;
   L33.t2 = create_at23;
   ret_val18 = L33;
   ret_val = ret_val18;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_1705639890(TRANS self, AS_CALL_EXPR e, dTP tp, BOOL has_ret) {
 dAM_EXPR ret_val;
 dAM_EXPR r = ((dAM_EXPR) NULL);
 dTP stp = ((dTP) NULL);
 TUPdAM_EXPRdTP stup = TUPdAM_EXPRdTP_zero;
 dAM_EXPR self_val;
 dTP self_tp1;
 BOOL in_class = BOOL_zero;
 CALL_SIG call_sig;
 ARRAYAM_CALL_ARG args = ((ARRAYAM_CALL_ARG) NULL);
 INT nargs = INT_zero;
 SIG sig1;
 dAM_CALL_EXPR cr = ((dAM_CALL_EXPR) NULL);
 AM_EXT_CALL_EXPR er = ((AM_EXT_CALL_EXPR) NULL);
 AM_ITE1809135850 ir = ((AM_ITE1809135850) NULL);
 AM_ROU1916046290 rr = ((AM_ROU1916046290) NULL);
 AM_BND260301329 brr = ((AM_BND260301329) NULL);
 AM_BND367211769 bir = ((AM_BND367211769) NULL);
 BOOL external_call = BOOL_zero;
 ELT elt1;
 IMPL im;
 ELT el;
 INT i = INT_zero;
 INT i1 = INT_zero;
 INT i2 = INT_zero;
 AM_CALL_ARG t;
 IMPL im1;
 ELT el1;
 ARRAYAM_CALL_ARG L11;
 INT i3 = INT_zero;
 INT i4 = INT_zero;
 dAM_EXPR ncr;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i5 = INT_zero;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r1;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val3;
 TRANS err_self;
 STR err_s;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 CALL_SIG create_self1;
 CALL_SIG ret_val6;
 ARRAYAM_CALL_ARG create_self2;
 INT create_n = INT_zero;
 ARRAYAM_CALL_ARG ret_val7;
 ARRAYCALL_ARG create_self3;
 INT create_n1 = INT_zero;
 ARRAYCALL_ARG ret_val8;
 PROG psather_self;
 BOOL ret_val9 = BOOL_zero;
 TP_CLASS is_external_self;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val11 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val12;
 TP_CLASS kind_self1;
 INT ret_val13 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val14;
 TP_CLASS kind_self2;
 INT ret_val15 = INT_zero;
 TP_CLASS as_tree_self2;
 AS_CLASS_DEF ret_val16;
 TP_CLASS kind_self3;
 INT ret_val17 = INT_zero;
 TP_CLASS as_tree_self3;
 AS_CLASS_DEF ret_val18;
 TP_CLASS kind_self4;
 INT ret_val19 = INT_zero;
 TP_CLASS as_tree_self4;
 AS_CLASS_DEF ret_val20;
 TP_CLASS kind_self5;
 INT ret_val21 = INT_zero;
 TP_CLASS as_tree_self5;
 AS_CLASS_DEF ret_val22;
 SIG is_reader_sig_se;
 BOOL ret_val23 = BOOL_zero;
 TRANS ext_call_const_e;
 AS_CALL_EXPR ext_call_const_e1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 TRANS err_self1;
 STR err_s1;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val24;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val25;
 OUT create_self4;
 OUT ret_val26;
 OUT plus_self5;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val27;
 FILE1 r2;
 IMPL elt_with_sig_sel;
 SIG elt_with_sig_s;
 ELT ret_val28;
 OUT create_self5;
 OUT ret_val29;
 OUT plus_self6;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val30;
 FILE1 r3;
 AM_EXT_CALL_EXPR create_self6;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 IDENT create_nm = IDENT_zero;
 AM_EXT_CALL_EXPR ret_val31;
 AM_EXT_CALL_EXPR r4;
 AM_EXT_CALL_EXPR create_self7;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_EXT_CALL_EXPR ret_val32;
 AM_EXT_CALL_EXPR r5;
 AM_CALL_ARG create_self8;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val33;
 AM_CALL_ARG res;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val34 = BOOL_zero;
 TRANS iter_in_protect_;
 AS_CALL_EXPR iter_in_protect_1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val35;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val36;
 AM_ITE1809135850 create_self9;
 INT create_nargs2 = INT_zero;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ITE1809135850 ret_val37;
 AM_ITE1809135850 r6;
 AM_CALL_ARG create_self10;
 dAM_EXPR create_e1;
 AM_CALL_ARG ret_val38;
 AM_CALL_ARG res1;
 AM_ROU1916046290 create_self11;
 INT create_nargs3 = INT_zero;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val39;
 AM_ROU1916046290 r7;
 AM_CALL_ARG create_self12;
 dAM_EXPR create_e2;
 AM_CALL_ARG ret_val40;
 AM_CALL_ARG res2;
 PROG psather_self1;
 BOOL ret_val41 = BOOL_zero;
 OUT create_self13;
 OUT ret_val42;
 OUT plus_self9;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val43;
 FILE1 r8;
 IMPL elt_with_sig_sel1;
 SIG elt_with_sig_s1;
 ELT ret_val44;
 OUT create_self14;
 OUT ret_val45;
 OUT plus_self10;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val46;
 FILE1 r9;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val47;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res3 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self15;
 AS_TYPE_SPEC ret_val48;
 TRANS bnd_rout_call_co;
 TRANS err_self3;
 STR err_s3;
 STR plus_self11;
 STR plus_sarg6;
 STR ret_val49;
 STR plus_self12;
 STR plus_sarg7;
 STR ret_val50;
 AM_BND260301329 create_self16;
 INT create_nargs4 = INT_zero;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_BND260301329 ret_val51;
 AM_BND260301329 r10;
 INT L41 = INT_zero;
 TRANS iter_in_protect_2;
 AS_CALL_EXPR iter_in_protect_3;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self4;
 STR err_s4;
 STR plus_self13;
 STR plus_sarg8;
 STR ret_val52;
 STR plus_self14;
 STR plus_sarg9;
 STR ret_val53;
 AM_BND367211769 create_self17;
 INT create_nargs5 = INT_zero;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_BND367211769 ret_val54;
 AM_BND367211769 r11;
 TRANS call_context_err;
 AS_CALL_EXPR call_context_err1;
 dTP call_context_err2;
 dTP call_context_err3;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 STR plus_self15;
 STR plus_sarg10;
 STR ret_val55;
 STR plus_self16;
 STR plus_sarg11;
 STR ret_val56;
 STR plus_self17;
 STR plus_sarg12;
 STR ret_val57;
 STR plus_self18;
 STR plus_sarg13;
 STR ret_val58;
 TRANS err_self5;
 STR err_s5;
 STR plus_self19;
 STR plus_sarg14;
 STR ret_val59;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val60;
 TRANS special_inline_s;
 AM_ROU1916046290 special_inline_c;
 dAM_EXPR ret_val61;
 dAM_EXPR am;
 PROG psather_self2;
 BOOL ret_val62 = BOOL_zero;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 extern STR Compil29290199;
 FILE1 L8;
 OB L9;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 dTP L15;
 extern STR Nocall611387935;
 BOOL L16;
 BOOL L17;
 extern STR name21;
 BOOL L18;
 BOOL L191_;
 dTP L20;
 CALL_SIG L22;
 OB L23;
 BOOL L24;
 BOOL L251_;
 ARRAYAM_CALL_ARG L26;
 INT L27;
 OB L28;
 INT L29;
 ARRAYCALL_ARG L30;
 INT L32;
 OB L33;
 INT L34;
 BOOL L35;
 BOOL L36;
 BOOL L37;
 INT L38;
 INT L39;
 BOOL L401_;
 dPARSE L42;
 IDENT L43;
 ARRAYdTP L44;
 INT L451_;
 dPARSE L46;
 BOOL L47;
 BOOL L481_;
 INT L49;
 INT L50;
 BOOL L511_;
 INT L52;
 INT L53;
 BOOL L541_;
 dPARSE L55;
 IDENT L56;
 ARRAYdTP L57;
 INT L581_;
 dPARSE L59;
 BOOL L60;
 BOOL L611_;
 INT L62;
 INT L63;
 BOOL L641_;
 BOOL L65;
 BOOL L66;
 INT L67;
 INT L68;
 BOOL L691_;
 dPARSE L70;
 IDENT L72;
 ARRAYdTP L73;
 INT L741_;
 dPARSE L75;
 BOOL L76;
 BOOL L771_;
 INT L78;
 INT L79;
 BOOL L801_;
 BOOL L81;
 BOOL L821_;
 BOOL L83;
 INT L84;
 INT L85;
 BOOL L861_;
 dPARSE L87;
 IDENT L88;
 ARRAYdTP L89;
 INT L901_;
 dPARSE L91;
 BOOL L92;
 BOOL L931_;
 INT L94;
 INT L95;
 BOOL L961_;
 BOOL L97;
 INT L98;
 INT L99;
 BOOL L1001_;
 dPARSE L102;
 IDENT L103;
 ARRAYdTP L104;
 INT L1051_;
 dPARSE L106;
 BOOL L107;
 BOOL L1081_;
 INT L109;
 INT L110;
 BOOL L1111_;
 BOOL L112;
 INT L113;
 INT L114;
 BOOL L1151_;
 dPARSE L116;
 IDENT L117;
 ARRAYdTP L118;
 INT L1191_;
 dPARSE L120;
 BOOL L121;
 BOOL L1221_;
 INT L123;
 INT L124;
 BOOL L1251_;
 BOOL L126;
 BOOL L127;
 BOOL L128;
 BOOL L1291_;
 IMPL L130;
 BOOL L132;
 BOOL L1331_;
 BOOL L134;
 BOOL L135;
 BOOL L136;
 extern STR Extern977010393;
 BOOL L137;
 BOOL L138;
 extern STR name21;
 extern STR Compil998589335;
 FILE1 L139;
 OB L140;
 extern STR Compil807929712;
 FILE1 L142;
 OB L143;
 INT L1451_;
 AM_EXT_CALL_EXPR L146;
 INT L147;
 OB L148;
 INT L149;
 AM_CALL_ARG L150;
 OB L151;
 AM_CALL_ARG L152;
 BOOL L154;
 BOOL L1551_;
 BOOL L1561_;
 INT L1571_;
 INT L158;
 AM_CALL_ARG L1591_;
 AM_CALL_ARG L160;
 INT L1621_;
 BOOL L163;
 BOOL L164;
 BOOL L1651_;
 STR L166;
 INT L167;
 INT L1681_;
 INT L169;
 CHAR L1701_;
 CHAR L171;
 BOOL L1721_;
 extern STR Aniter1062309569;
 BOOL L173;
 BOOL L174;
 extern STR name21;
 INT L1751_;
 AM_ITE1809135850 L176;
 INT L177;
 OB L178;
 INT L179;
 AM_CALL_ARG L180;
 OB L181;
 AM_CALL_ARG L182;
 BOOL L184;
 BOOL L1851_;
 BOOL L1861_;
 INT L1871_;
 INT L188;
 AM_CALL_ARG L1891_;
 AM_CALL_ARG L190;
 INT L1931_;
 INT L1941_;
 AM_ROU1916046290 L195;
 INT L196;
 OB L197;
 INT L198;
 AM_CALL_ARG L199;
 OB L200;
 AM_CALL_ARG L201;
 BOOL L203;
 BOOL L2041_;
 BOOL L2051_;
 INT L2061_;
 INT L207;
 AM_CALL_ARG L2081_;
 AM_CALL_ARG L209;
 INT L2111_;
 AM_CALL_ARG L2121_;
 AM_CALL_ARG L2131_;
 AM_CALL_ARG L214;
 BOOL L217;
 BOOL L218;
 extern STR Compil998589335;
 FILE1 L219;
 OB L220;
 extern STR Compil807929712;
 FILE1 L222;
 OB L223;
 BOOL L225;
 BOOL L2261_;
 AS_TYPE_SPEC L227;
 OB L228;
 extern STR INT1;
 extern STR Boundr821212056;
 BOOL L229;
 BOOL L230;
 extern STR name21;
 AM_BND260301329 L231;
 INT L232;
 OB L233;
 INT L234;
 BOOL L235;
 BOOL L2361_;
 INT L2371_br;
 INT rL2371_;
 INT L238;
 AM_CALL_ARG L2391_;
 AM_CALL_ARG L240;
 INT L2421_;
 extern STR Aniter1062309569;
 BOOL L243;
 BOOL L244;
 extern STR name21;
 AM_BND367211769 L245;
 INT L246;
 OB L247;
 INT L248;
 BOOL L249;
 BOOL L2501_;
 BOOL L2521_;
 AM_CALL_ARG L2531_;
 AM_CALL_ARG L254;
 INT L2561_;
 BOOL L257;
 BOOL L258;
 BOOL L2591_;
 dAM_CALL_EXPR L260;
 BOOL L261;
 BOOL L2621_;
 dAM_CALL_EXPR L263;
 dTP L264;
 dTP L265;
 BOOL L266;
 BOOL L2671_;
 dAM_CALL_EXPR L268;
 extern STR Thetypeofthecall;
 dTP L269;
 extern STR isnotasubtypeof;
 dTP L270;
 extern STR name18;
 BOOL L271;
 BOOL L272;
 extern STR name21;
 BOOL L273;
 BOOL L2741_;
 dGENERATE_AM L275;
 dINLINE L276;
 BOOL L277;
 BOOL L278;
 BOOL L2791_;
 INT L281;
 dAM_CALL_EXPR L282;
 INT L283;
 INT L2841_;
 INT L2801_,L2801_m;
 BOOL L285;
 dAM_CALL_EXPR L286;
 AM_CALL_ARG L287;
 dMODE L288;
 dAM_CALL_EXPR L289;
 AM_CALL_ARG L290;
 dMODE L291;
 dAM_CALL_EXPR L292;
 AM_CALL_ARG L293;
 if ((ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL))) {
  L6 = ATTR(self,in_constant);
  L71_=!(L6); 
  L5 = L71_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Compil29290199);
  stdout_self = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stdout);
  ret_val2 = r1;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 if ((e==((AS_CALL_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) e);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 L12 = (ATTR(e,tp)==((AS_TYPE_SPEC) NULL));
 L131_=!(L12); 
 if (L131_) {
  tp_of_self = self;
  tp_of_t = ATTR(e,tp);
  ret_val3 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  stp = ret_val3;
  if ((ATTR(e,ob)==((dAS_EXPR) NULL))) {
   L15 = stp;
   L14 = (*dTP_is1553335918[TAG(L15)])(L15);
  } else {
   L14 = FALSE;
  }
  if (L14) {
   err_self = self;
   err_s = ((STR) &Nocall611387935);
   if ((err_self==((TRANS) NULL))) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L17) {
    L16 = TRUE;
   } else {
    L16 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
   }
   if (L16) {
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    plus_self1 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
    plus_sarg = ((STR) &name21);
    ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
    plus_self2 = ret_val4;
    plus_sarg1 = err_s;
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
    err_s = ret_val5;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
  }
 }
 r = ((dAM_EXPR) TRANS_598855940(self, e, tp));
 L18 = (r==((dAM_EXPR) NULL));
 L191_=!(L18); 
 if (L191_) {
  ret_val = r;
  return ret_val;
 }
 stup = TRANS_331125130(self, e);
 if ((stup.t1==(dAM_EXPR)0)&&(stup.t2==(dTP)0)) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 self_val = stup.t1;
 self_tp1 = stup.t2;
 if ((self_tp1==((dTP) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L20 = self_tp1;
 if ((*dTP_is242312711[TAG(L20)])(L20, ((OB) ATTR(ATTR(self,tp_con),same1)))) {
  in_class = TRUE;
 }
 create_self1 = ((CALL_SIG) NULL);
 L23=ZALLOC(sizeof(struct CALL_SIG_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=CALL_SIG_tag;
 L22 = ((CALL_SIG) L23);
 ret_val6 = L22;
 call_sig = ret_val6;
 SATTR(call_sig,has_ret,has_ret);
 SATTR(call_sig,name1,TRANS_885329442(self, e));
 SATTR(call_sig,tp,self_tp1);
 nargs = AS_CAL1853175953(e);
 L24 = (ATTR(e,args)==((dAS_EXPR) NULL));
 L251_=!(L24); 
 if (L251_) {
  create_self2 = ((ARRAYAM_CALL_ARG) NULL);
  create_n = nargs;
  L27 = create_n;
  L29=(sizeof(struct ARRAYAM_CALL_ARG_struct)-sizeof(AM_CALL_ARG))+(L27)*sizeof(AM_CALL_ARG);
  L28=ZALLOC_BIG(L29);
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=ARRAYAM_CALL_ARG_tag;
#ifdef DESTROY_CHK

    ((OB)L28)->header.destroyed=0;
#endif

  L26 = ((ARRAYAM_CALL_ARG) L28);
  L26->asize = L27;
  ret_val7 = L26;
  args = ret_val7;
  create_self3 = ((ARRAYCALL_ARG) NULL);
  create_n1 = nargs;
  L32 = create_n1;
  L34=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L32)*sizeof(CALL_ARG);
  L33=ZALLOC_BIG(L34);
  if (L33==NULL) FATAL("Unable to allocate more memory");
  ((OB)L33)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

    ((OB)L33)->header.destroyed=0;
#endif

  L30 = ((ARRAYCALL_ARG) L33);
  L30->asize = L32;
  ret_val8 = L30;
  SATTR(call_sig,args,ret_val8);
 }
 sig1 = TRANS_1831541612(self, e, call_sig, args, in_class);
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L35 = TRUE;
 } else {
  L35 = ATTR(psather_self,distributed);
 }
 ret_val9 = L35;
 if (ret_val9) {
  TRANS_81467988(self, sig1);
 }
 if ((sig1==((SIG) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 TRANS_507974720(self, e);
 switch (TAG(self_tp1)) {
  case TP_CLASS_tag:
   is_external_self = ((TP_CLASS) self_tp1);
   kind_self = is_external_self;
   L38 = ATTR(kind_self,kind_cache);
   L39 = TP_KIN138039446;
   L401_=(L38)==(L39); 
   if (L401_) {
    as_tree_self = kind_self;
    if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L42 = ATTR(ATTR(as_tree_self,prog),parse);
     L43 = ATTR(as_tree_self,name1);
     L44 = ATTR(as_tree_self,params);
     L451_=(L44)==NULL?0:ASIZE((ARRAYdTP)L44); 
     L46 = L42;
     SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L46)])(L46, L43, L451_));
    }
    ret_val12 = ATTR(as_tree_self,my_as_tree);
    L47 = (ret_val12==((AS_CLASS_DEF) NULL));
    L481_=!(L47); 
    if (L481_) {
     SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
    }
   }
   ret_val11 = ATTR(kind_self,kind_cache);
   L49 = ret_val11;
   L50 = TP_KIND_ext_c_tp;
   L511_=(L49)==(L50); 
   if (L511_) {
    L37 = TRUE;
   } else {
    kind_self1 = is_external_self;
    L52 = ATTR(kind_self1,kind_cache);
    L53 = TP_KIN138039446;
    L541_=(L52)==(L53); 
    if (L541_) {
     as_tree_self1 = kind_self1;
     if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L55 = ATTR(ATTR(as_tree_self1,prog),parse);
      L56 = ATTR(as_tree_self1,name1);
      L57 = ATTR(as_tree_self1,params);
      L581_=(L57)==NULL?0:ASIZE((ARRAYdTP)L57); 
      L59 = L55;
      SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L59)])(L59, L56, L581_));
     }
     ret_val14 = ATTR(as_tree_self1,my_as_tree);
     L60 = (ret_val14==((AS_CLASS_DEF) NULL));
     L611_=!(L60); 
     if (L611_) {
      SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
     }
    }
    ret_val13 = ATTR(kind_self1,kind_cache);
    L62 = ret_val13;
    L63 = TP_KIN462481068;
    L641_=(L62)==(L63); 
    L37 = L641_;
   }
   ret_val10 = L37;
   if (ret_val10) {
    kind_self2 = ((TP_CLASS) self_tp1);
    L67 = ATTR(kind_self2,kind_cache);
    L68 = TP_KIN138039446;
    L691_=(L67)==(L68); 
    if (L691_) {
     as_tree_self2 = kind_self2;
     if ((ATTR(as_tree_self2,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L70 = ATTR(ATTR(as_tree_self2,prog),parse);
      L72 = ATTR(as_tree_self2,name1);
      L73 = ATTR(as_tree_self2,params);
      L741_=(L73)==NULL?0:ASIZE((ARRAYdTP)L73); 
      L75 = L70;
      SATTR(as_tree_self2,my_as_tree,(*dPARSE1581792481[TAG(L75)])(L75, L72, L741_));
     }
     ret_val16 = ATTR(as_tree_self2,my_as_tree);
     L76 = (ret_val16==((AS_CLASS_DEF) NULL));
     L771_=!(L76); 
     if (L771_) {
      SATTR(kind_self2,kind_cache,ATTR(ATTR(kind_self2,my_as_tree),kind));
     }
    }
    ret_val15 = ATTR(kind_self2,kind_cache);
    L78 = ret_val15;
    L79 = TP_KIN462481068;
    L801_=(L78)==(L79); 
    L81 = L801_;
    L821_=!(L81); 
    if (L821_) {
     L66 = TRUE;
    } else {
     kind_self3 = ((TP_CLASS) self_tp1);
     L84 = ATTR(kind_self3,kind_cache);
     L85 = TP_KIN138039446;
     L861_=(L84)==(L85); 
     if (L861_) {
      as_tree_self3 = kind_self3;
      if ((ATTR(as_tree_self3,my_as_tree)==((AS_CLASS_DEF) NULL))) {
       L87 = ATTR(ATTR(as_tree_self3,prog),parse);
       L88 = ATTR(as_tree_self3,name1);
       L89 = ATTR(as_tree_self3,params);
       L901_=(L89)==NULL?0:ASIZE((ARRAYdTP)L89); 
       L91 = L87;
       SATTR(as_tree_self3,my_as_tree,(*dPARSE1581792481[TAG(L91)])(L91, L88, L901_));
      }
      ret_val18 = ATTR(as_tree_self3,my_as_tree);
      L92 = (ret_val18==((AS_CLASS_DEF) NULL));
      L931_=!(L92); 
      if (L931_) {
       SATTR(kind_self3,kind_cache,ATTR(ATTR(kind_self3,my_as_tree),kind));
      }
     }
     ret_val17 = ATTR(kind_self3,kind_cache);
     L94 = ret_val17;
     L95 = TP_KIN462481068;
     L961_=(L94)==(L95); 
     if (L961_) {
      L83 = SIG_is275586960(sig1);
     } else {
      L83 = FALSE;
     }
     L66 = L83;
    }
    if (L66) {
     L65 = TRUE;
    } else {
     kind_self4 = ((TP_CLASS) self_tp1);
     L98 = ATTR(kind_self4,kind_cache);
     L99 = TP_KIN138039446;
     L1001_=(L98)==(L99); 
     if (L1001_) {
      as_tree_self4 = kind_self4;
      if ((ATTR(as_tree_self4,my_as_tree)==((AS_CLASS_DEF) NULL))) {
       L102 = ATTR(ATTR(as_tree_self4,prog),parse);
       L103 = ATTR(as_tree_self4,name1);
       L104 = ATTR(as_tree_self4,params);
       L1051_=(L104)==NULL?0:ASIZE((ARRAYdTP)L104); 
       L106 = L102;
       SATTR(as_tree_self4,my_as_tree,(*dPARSE1581792481[TAG(L106)])(L106, L103, L1051_));
      }
      ret_val20 = ATTR(as_tree_self4,my_as_tree);
      L107 = (ret_val20==((AS_CLASS_DEF) NULL));
      L1081_=!(L107); 
      if (L1081_) {
       SATTR(kind_self4,kind_cache,ATTR(ATTR(kind_self4,my_as_tree),kind));
      }
     }
     ret_val19 = ATTR(kind_self4,kind_cache);
     L109 = ret_val19;
     L110 = TP_KIND_ext_c_tp;
     L1111_=(L109)==(L110); 
     if (L1111_) {
      L97 = SIG_is600379186(sig1);
     } else {
      L97 = FALSE;
     }
     L65 = L97;
    }
    L36 = L65;
   } else {
    L36 = FALSE;
   }
   external_call = L36;
   kind_self5 = ((TP_CLASS) self_tp1);
   L113 = ATTR(kind_self5,kind_cache);
   L114 = TP_KIN138039446;
   L1151_=(L113)==(L114); 
   if (L1151_) {
    as_tree_self5 = kind_self5;
    if ((ATTR(as_tree_self5,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L116 = ATTR(ATTR(as_tree_self5,prog),parse);
     L117 = ATTR(as_tree_self5,name1);
     L118 = ATTR(as_tree_self5,params);
     L1191_=(L118)==NULL?0:ASIZE((ARRAYdTP)L118); 
     L120 = L116;
     SATTR(as_tree_self5,my_as_tree,(*dPARSE1581792481[TAG(L120)])(L120, L117, L1191_));
    }
    ret_val22 = ATTR(as_tree_self5,my_as_tree);
    L121 = (ret_val22==((AS_CLASS_DEF) NULL));
    L1221_=!(L121); 
    if (L1221_) {
     SATTR(kind_self5,kind_cache,ATTR(ATTR(kind_self5,my_as_tree),kind));
    }
   }
   ret_val21 = ATTR(kind_self5,kind_cache);
   L123 = ret_val21;
   L124 = TP_KIND_ext_c_tp;
   L1251_=(L123)==(L124); 
   if (L1251_) {
    L112 = SIG_is600379186(sig1);
   } else {
    L112 = FALSE;
   }
   if (L112) {
    is_reader_sig_se = sig1;
    if ((ATTR(is_reader_sig_se,args)==((ARRAYARG) NULL))) {
     L128 = (ATTR(is_reader_sig_se,ret)==((dTP) NULL));
     L1291_=!(L128); 
     L127 = L1291_;
    } else {
     L127 = FALSE;
    }
    ret_val23 = L127;
    if (ret_val23) {
     L126 = TRUE;
    } else {
     L126 = SIG_is1739708917(sig1);
    }
    if (L126) {
     L130=TP_CLA1109727233(((TP_CLASS) self_tp1));
     elt1 = ELT_TB1949891380(ATTR(L130,elts), sig1);
     L132 = (elt1==((ELT) NULL));
     L1331_=!(L132); 
     if (L1331_) {
      if (ELT_is380037919(elt1)) {
       L136 = TRUE;
      } else {
       L136 = ELT_is1808162451(elt1);
      }
      if (L136) {
       L135 = TRUE;
      } else {
       L135 = ELT_is1936066670(elt1);
      }
      if (L135) {
       L134 = TRUE;
      } else {
       L134 = ELT_is507942138(elt1);
      }
      if (L134) {
       external_call = FALSE;
      }
     }
    }
   }
   if (external_call) {
    if (ATTR(self,in_constant)) {
     ext_call_const_e = self;
     ext_call_const_e1 = e;
     err_loc_self1 = ext_call_const_e;
     err_loc_t1 = ((dPROG_ERR) ext_call_const_e1);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     err_self1 = ext_call_const_e;
     err_s1 = ((STR) &Extern977010393);
     if ((err_self1==((TRANS) NULL))) {
      L138 = TRUE;
     } else {
      L138 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L138) {
      L137 = TRUE;
     } else {
      L137 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
     }
     if (L137) {
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     else {
      plus_self3 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
      plus_sarg2 = ((STR) &name21);
      ret_val24 = STR_ap2004550586(plus_self3, plus_sarg2);
      plus_self4 = ret_val24;
      plus_sarg3 = err_s1;
      ret_val25 = STR_ap2004550586(plus_self4, plus_sarg3);
      err_s1 = ret_val25;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    im = TP_CLA1109727233(((TP_CLASS) self_tp1));
    if ((im==((IMPL) NULL))) {
     create_self4 = ((OUT) NULL);
     ret_val26 = create_self4;
     plus_self5 = ret_val26;
     plus_s1 = ((STR) &Compil998589335);
     stdout_self1 = ((FILE1) NULL);
     L140=ZALLOC(sizeof(struct FILE1_struct));
     if (L140==NULL) FATAL("Unable to allocate more memory");
     ((OB)L140)->header.tag=FILE1_tag;
     L139 = ((FILE1) L140);
     r2 = L139;
     
     SATTR(r2,fp,stdout);
     ret_val27 = r2;
     FILE_plus_STR(ret_val27, plus_s1);
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    elt_with_sig_sel = im;
    elt_with_sig_s = sig1;
    ret_val28 = ELT_TB1949891380(ATTR(elt_with_sig_sel,elts), elt_with_sig_s);
    el = ret_val28;
    if ((el==((ELT) NULL))) {
     create_self5 = ((OUT) NULL);
     ret_val29 = create_self5;
     plus_self6 = ret_val29;
     plus_s2 = ((STR) &Compil807929712);
     stdout_self2 = ((FILE1) NULL);
     L143=ZALLOC(sizeof(struct FILE1_struct));
     if (L143==NULL) FATAL("Unable to allocate more memory");
     ((OB)L143)->header.tag=FILE1_tag;
     L142 = ((FILE1) L143);
     r3 = L142;
     
     SATTR(r3,fp,stdout);
     ret_val30 = r3;
     FILE_plus_STR(ret_val30, plus_s2);
     ret_val = ((dAM_EXPR) NULL);
     return ret_val;
    }
    create_self6 = ((AM_EXT_CALL_EXPR) NULL);
    L1451_=INTPLUS(nargs,1); 
    create_nargs = L1451_;
    create_source = ATTR(e,source1);
    create_nm = TRANS_1721028300(self, el);
    create_self7 = create_self6;
    create_nargs1 = create_nargs;
    create_source1 = create_source;
    L147 = create_nargs1;
    L149=(sizeof(struct AM_EXT_CALL_EXPR_struct)-sizeof(AM_CALL_ARG))+(L147)*sizeof(AM_CALL_ARG);
    L148=ZALLOC_BIG(L149);
    if (L148==NULL) FATAL("Unable to allocate more memory");
    ((OB)L148)->header.tag=AM_EXT_CALL_EXPR_tag;
#ifdef DESTROY_CHK

      ((OB)L148)->header.destroyed=0;
#endif

    L146 = ((AM_EXT_CALL_EXPR) L148);
    L146->asize = L147;
    r5 = L146;
    SATTR(r5,source1,create_source1);
    ret_val32 = r5;
    r4 = ret_val32;
    SATTR(r4,nm,create_nm);
    ret_val31 = r4;
    er = ret_val31;
    create_self8 = ((AM_CALL_ARG) NULL);
    create_e = self_val;
    L151=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
    if (L151==NULL) FATAL("Unable to allocate more memory");
    ((OB)L151)->header.tag=AM_CALL_ARG_tag;
    L150 = ((AM_CALL_ARG) L151);
    res = L150;
    SATTR(res,expr,create_e);
    SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
    ret_val33 = res;
    L152 = ret_val33;
    SARR((AM_EXT_CALL_EXPR)er,0,(AM_CALL_ARG)L152); 
    ;
    SATTR(er,fun,sig1);
    L154 = (args==((ARRAYAM_CALL_ARG) NULL));
    L1551_=!(L154); 
    if (L1551_) {
     i = 0;
     while (1) {
      L1561_=(i)<(nargs); 
      if (L1561_) {
      }
      else {
       goto after_loop;
      }
      L1571_=INTPLUS(i,1); 
      L158 = L1571_;
      L1591_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i); 
      L160 = L1591_;
      SARR((AM_EXT_CALL_EXPR)er,L158,(AM_CALL_ARG)L160); 
      ;
      L1621_=INTPLUS(i,1); 
      i = L1621_;
     }
     after_loop: ;
    }
    cr = ((dAM_CALL_EXPR) er);
   }
   else {
    is_iter_self = ATTR(e,name1);
    L164 = (is_iter_self.str==((STR) NULL));
    L1651_=!(L164); 
    if (L1651_) {
     L166 = is_iter_self.str;
     L167 = STR_sizerINT(is_iter_self.str);
     L1681_=INTMINUS(L167,1); 
     L169 = L1681_;
     L1701_=ARR((STR)L166,L169); 
     L171 = L1701_;
     L1721_=L171=='!'; 
     L163 = L1721_;
    } else {
     L163 = FALSE;
    }
    ret_val34 = L163;
    if (ret_val34) {
     if (ATTR(self,in_protect_but_n)) {
      iter_in_protect_ = self;
      iter_in_protect_1 = e;
      err_loc_self2 = iter_in_protect_;
      err_loc_t2 = ((dPROG_ERR) iter_in_protect_1);
      PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
      err_self2 = iter_in_protect_;
      err_s2 = ((STR) &Aniter1062309569);
      if ((err_self2==((TRANS) NULL))) {
       L174 = TRUE;
      } else {
       L174 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
      }
      if (L174) {
       L173 = TRUE;
      } else {
       L173 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
      }
      if (L173) {
       PROG_err_STR(ATTR(err_self2,prog), err_s2);
      }
      else {
       plus_self7 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
       plus_sarg4 = ((STR) &name21);
       ret_val35 = STR_ap2004550586(plus_self7, plus_sarg4);
       plus_self8 = ret_val35;
       plus_sarg5 = err_s2;
       ret_val36 = STR_ap2004550586(plus_self8, plus_sarg5);
       err_s2 = ret_val36;
       PROG_err_STR(ATTR(err_self2,prog), err_s2);
      }
      ret_val = ((dAM_EXPR) NULL);
      return ret_val;
     }
     create_self9 = ((AM_ITE1809135850) NULL);
     L1751_=INTPLUS(nargs,1); 
     create_nargs2 = L1751_;
     create_source2 = ATTR(e,source1);
     L177 = create_nargs2;
     L179=(sizeof(struct AM_ITE1809135850_struct)-sizeof(AM_CALL_ARG))+(L177)*sizeof(AM_CALL_ARG);
     L178=ZALLOC_BIG(L179);
     if (L178==NULL) FATAL("Unable to allocate more memory");
     ((OB)L178)->header.tag=AM_ITE1809135850_tag;
#ifdef DESTROY_CHK

       ((OB)L178)->header.destroyed=0;
#endif

     L176 = ((AM_ITE1809135850) L178);
     L176->asize = L177;
     r6 = L176;
     SATTR(r6,source1,create_source2);
     ret_val37 = r6;
     ir = ret_val37;
     create_self10 = ((AM_CALL_ARG) NULL);
     create_e1 = self_val;
     L181=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L181==NULL) FATAL("Unable to allocate more memory");
     ((OB)L181)->header.tag=AM_CALL_ARG_tag;
     L180 = ((AM_CALL_ARG) L181);
     res1 = L180;
     SATTR(res1,expr,create_e1);
     SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
     ret_val38 = res1;
     L182 = ret_val38;
     SARR((AM_ITE1809135850)ir,0,(AM_CALL_ARG)L182); 
     ;
     SATTR(ir,fun,sig1);
     L184 = (args==((ARRAYAM_CALL_ARG) NULL));
     L1851_=!(L184); 
     if (L1851_) {
      i1 = 0;
      while (1) {
       L1861_=(i1)<(nargs); 
       if (L1861_) {
       }
       else {
        goto after_loop1;
       }
       L1871_=INTPLUS(i1,1); 
       L188 = L1871_;
       L1891_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i1); 
       L190 = L1891_;
       SARR((AM_ITE1809135850)ir,L188,(AM_CALL_ARG)L190); 
       ;
       L1931_=INTPLUS(i1,1); 
       i1 = L1931_;
      }
      after_loop1: ;
     }
     cr = ((dAM_CALL_EXPR) TRANS_1528108774(self, ir));
    }
    else {
     create_self11 = ((AM_ROU1916046290) NULL);
     L1941_=INTPLUS(nargs,1); 
     create_nargs3 = L1941_;
     create_source3 = ATTR(e,source1);
     L196 = create_nargs3;
     L198=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L196)*sizeof(AM_CALL_ARG);
     L197=ZALLOC_BIG(L198);
     if (L197==NULL) FATAL("Unable to allocate more memory");
     ((OB)L197)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

       ((OB)L197)->header.destroyed=0;
#endif

     L195 = ((AM_ROU1916046290) L197);
     L195->asize = L196;
     r7 = L195;
     SATTR(r7,source1,create_source3);
     ret_val39 = r7;
     rr = ret_val39;
     create_self12 = ((AM_CALL_ARG) NULL);
     create_e2 = self_val;
     L200=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L200==NULL) FATAL("Unable to allocate more memory");
     ((OB)L200)->header.tag=AM_CALL_ARG_tag;
     L199 = ((AM_CALL_ARG) L200);
     res2 = L199;
     SATTR(res2,expr,create_e2);
     SATTR(res2,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
     ret_val40 = res2;
     L201 = ret_val40;
     SARR((AM_ROU1916046290)rr,0,(AM_CALL_ARG)L201); 
     ;
     SATTR(rr,fun,sig1);
     L203 = (args==((ARRAYAM_CALL_ARG) NULL));
     L2041_=!(L203); 
     if (L2041_) {
      i2 = 0;
      while (1) {
       L2051_=(i2)<(nargs); 
       if (L2051_) {
       }
       else {
        goto after_loop2;
       }
       L2061_=INTPLUS(i2,1); 
       L207 = L2061_;
       L2081_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i2); 
       L209 = L2081_;
       SARR((AM_ROU1916046290)rr,L207,(AM_CALL_ARG)L209); 
       ;
       L2111_=INTPLUS(i2,1); 
       i2 = L2111_;
      }
      after_loop2: ;
      if (ATTR(e,flip)) {
       L2121_=(AM_CALL_ARG)ARR((AM_ROU1916046290)rr,0); 
       t = L2121_;
       L2131_=(AM_CALL_ARG)ARR((AM_ROU1916046290)rr,1); 
       L214 = L2131_;
       SARR((AM_ROU1916046290)rr,0,(AM_CALL_ARG)L214); 
       ;
       SARR((AM_ROU1916046290)rr,1,(AM_CALL_ARG)t); 
       ;
      }
     }
     cr = ((dAM_CALL_EXPR) rr);
     psather_self1 = ATTR(self,prog);
     if (ATTR(psather_self1,threads)) {
      L218 = TRUE;
     } else {
      L218 = ATTR(psather_self1,distributed);
     }
     ret_val41 = L218;
     if (ret_val41) {
      L217 = in_class;
     } else {
      L217 = FALSE;
     }
     if (L217) {
      im1 = TP_CLA1109727233(((TP_CLASS) self_tp1));
      if ((im1==((IMPL) NULL))) {
       create_self13 = ((OUT) NULL);
       ret_val42 = create_self13;
       plus_self9 = ret_val42;
       plus_s3 = ((STR) &Compil998589335);
       stdout_self3 = ((FILE1) NULL);
       L220=ZALLOC(sizeof(struct FILE1_struct));
       if (L220==NULL) FATAL("Unable to allocate more memory");
       ((OB)L220)->header.tag=FILE1_tag;
       L219 = ((FILE1) L220);
       r8 = L219;
       
       SATTR(r8,fp,stdout);
       ret_val43 = r8;
       FILE_plus_STR(ret_val43, plus_s3);
       ret_val = ((dAM_EXPR) NULL);
       return ret_val;
      }
      elt_with_sig_sel1 = im1;
      elt_with_sig_s1 = sig1;
      ret_val44 = ELT_TB1949891380(ATTR(elt_with_sig_sel1,elts), elt_with_sig_s1);
      el1 = ret_val44;
      if ((el1==((ELT) NULL))) {
       create_self14 = ((OUT) NULL);
       ret_val45 = create_self14;
       plus_self10 = ret_val45;
       plus_s4 = ((STR) &Compil807929712);
       stdout_self4 = ((FILE1) NULL);
       L223=ZALLOC(sizeof(struct FILE1_struct));
       if (L223==NULL) FATAL("Unable to allocate more memory");
       ((OB)L223)->header.tag=FILE1_tag;
       L222 = ((FILE1) L223);
       r9 = L222;
       
       SATTR(r9,fp,stdout);
       ret_val46 = r9;
       FILE_plus_STR(ret_val46, plus_s4);
       ret_val = ((dAM_EXPR) NULL);
       return ret_val;
      }
      as_tp_self = el1;
      clelt = ATTR(as_tp_self,as);
      switch (TAG(clelt)) {
       case AS_CONST_DEF_tag:
        L225 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
        L2261_=!(L225); 
        if (L2261_) {
         res3 = ATTR(((AS_CONST_DEF) clelt),tp);
        }
        else {
         create_self15 = ((AS_TYPE_SPEC) NULL);
         L228=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
         if (L228==NULL) FATAL("Unable to allocate more memory");
         ((OB)L228)->header.tag=AS_TYPE_SPEC_tag;
         L227 = ((AS_TYPE_SPEC) L228);
         ret_val48 = L227;
         res3 = ret_val48;
         SATTR(res3,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
        } break;
       case AS_SHARED_DEF_tag:
        res3 = ATTR(((AS_SHARED_DEF) clelt),tp); break;
       case AS_ATTR_DEF_tag:
        res3 = ATTR(((AS_ATTR_DEF) clelt),tp); break;
       default: ;
      }
      ret_val47 = res3;
      SATTR(rr,as_type,ret_val47);
     }
    }
   } break;
  case TP_ROUT_tag:
   if (ATTR(self,in_constant)) {
    bnd_rout_call_co = self;
    err_self3 = bnd_rout_call_co;
    err_s3 = ((STR) &Boundr821212056);
    if ((err_self3==((TRANS) NULL))) {
     L230 = TRUE;
    } else {
     L230 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L230) {
     L229 = TRUE;
    } else {
     L229 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
    }
    if (L229) {
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    else {
     plus_self11 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
     plus_sarg6 = ((STR) &name21);
     ret_val49 = STR_ap2004550586(plus_self11, plus_sarg6);
     plus_self12 = ret_val49;
     plus_sarg7 = err_s3;
     ret_val50 = STR_ap2004550586(plus_self12, plus_sarg7);
     err_s3 = ret_val50;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   create_self16 = ((AM_BND260301329) NULL);
   create_nargs4 = nargs;
   create_source4 = ATTR(e,source1);
   L232 = create_nargs4;
   L234=(sizeof(struct AM_BND260301329_struct)-sizeof(AM_CALL_ARG))+(L232)*sizeof(AM_CALL_ARG);
   L233=ZALLOC_BIG(L234);
   if (L233==NULL) FATAL("Unable to allocate more memory");
   ((OB)L233)->header.tag=AM_BND260301329_tag;
#ifdef DESTROY_CHK

     ((OB)L233)->header.destroyed=0;
#endif

   L231 = ((AM_BND260301329) L233);
   L231->asize = L232;
   r10 = L231;
   SATTR(r10,source1,create_source4);
   ret_val51 = r10;
   brr = ret_val51;
   SATTR(brr,br,self_val);
   SATTR(brr,br_tp,((TP_ROUT) self_tp1));
   L235 = (args==((ARRAYAM_CALL_ARG) NULL));
   L2361_=!(L235); 
   if (L2361_) {
    {
     BOOL f_L2371_ = TRUE;
     /* loop index variable */
     L41 = 0;
     L11 = args;
     L2371_br=L11==NULL?0:ASIZE((ARRAYAM_CALL_ARG)L11); 
     while (1) {
      if(L41>=L2371_br)  goto after_loop3; 
      rL2371_=L41; 
      i3 = rL2371_;
      L2391_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i3); 
      L240 = L2391_;
      SARR((AM_BND260301329)brr,i3,(AM_CALL_ARG)L240); 
      ;
      L2421_=INTPLUS(L41,1); 
      L41 = L2421_;
     }
    }
    after_loop3: ;
   }
   cr = ((dAM_CALL_EXPR) brr); break;
  case TP_ITER_tag:
   if (ATTR(self,in_protect_but_n)) {
    iter_in_protect_2 = self;
    iter_in_protect_3 = e;
    err_loc_self3 = iter_in_protect_2;
    err_loc_t3 = ((dPROG_ERR) iter_in_protect_3);
    PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
    err_self4 = iter_in_protect_2;
    err_s4 = ((STR) &Aniter1062309569);
    if ((err_self4==((TRANS) NULL))) {
     L244 = TRUE;
    } else {
     L244 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L244) {
     L243 = TRUE;
    } else {
     L243 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
    }
    if (L243) {
     PROG_err_STR(ATTR(err_self4,prog), err_s4);
    }
    else {
     plus_self13 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
     plus_sarg8 = ((STR) &name21);
     ret_val52 = STR_ap2004550586(plus_self13, plus_sarg8);
     plus_self14 = ret_val52;
     plus_sarg9 = err_s4;
     ret_val53 = STR_ap2004550586(plus_self14, plus_sarg9);
     err_s4 = ret_val53;
     PROG_err_STR(ATTR(err_self4,prog), err_s4);
    }
    ret_val = ((dAM_EXPR) NULL);
    return ret_val;
   }
   create_self17 = ((AM_BND367211769) NULL);
   create_nargs5 = nargs;
   create_source5 = ATTR(e,source1);
   L246 = create_nargs5;
   L248=(sizeof(struct AM_BND367211769_struct)-sizeof(AM_CALL_ARG))+(L246)*sizeof(AM_CALL_ARG);
   L247=ZALLOC_BIG(L248);
   if (L247==NULL) FATAL("Unable to allocate more memory");
   ((OB)L247)->header.tag=AM_BND367211769_tag;
#ifdef DESTROY_CHK

     ((OB)L247)->header.destroyed=0;
#endif

   L245 = ((AM_BND367211769) L247);
   L245->asize = L246;
   r11 = L245;
   SATTR(r11,source1,create_source5);
   ret_val54 = r11;
   bir = ret_val54;
   SATTR(bir,bi,self_val);
   SATTR(bir,bi_tp,((TP_ITER) self_tp1));
   L249 = (args==((ARRAYAM_CALL_ARG) NULL));
   L2501_=!(L249); 
   if (L2501_) {
    i4 = 0;
    while (1) {
     L2521_=(i4)<(nargs); 
     if (L2521_) {
     }
     else {
      goto after_loop4;
     }
     L2531_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i4); 
     L254 = L2531_;
     SARR((AM_BND367211769)bir,i4,(AM_CALL_ARG)L254); 
     ;
     L2561_=INTPLUS(i4,1); 
     i4 = L2561_;
    }
    after_loop4: ;
   }
   cr = ((dAM_CALL_EXPR) TRANS_745553885(self, bir, sig1)); break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::transform_call_expr(AS_CALL_EXPR,$TP,BOOL):$AM_EXPR\n./Middle/trans.sa:1643:14");
 }
 if ((cr==((dAM_CALL_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 L258 = (tp==((dTP) NULL));
 L2591_=!(L258); 
 if (L2591_) {
  L260 = cr;
  L261 = ((*dAM_CA564811416[TAG(L260)])(L260)==((dTP) NULL));
  L2621_=!(L261); 
  L257 = L2621_;
 } else {
  L257 = FALSE;
 }
 if (L257) {
  L263 = cr;
  L264 = (*dAM_CA564811416[TAG(L263)])(L263);
  L265 = L264;
  L266 = (*dTP_is1999456142[TAG(L265)])(L265, tp);
  L2671_=!(L266); 
  if (L2671_) {
   call_context_err = self;
   call_context_err1 = e;
   L268 = cr;
   call_context_err2 = (*dAM_CA564811416[TAG(L268)])(L268);
   call_context_err3 = tp;
   err_loc_self4 = call_context_err;
   err_loc_t4 = ((dPROG_ERR) call_context_err1);
   PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
   err_self5 = call_context_err;
   plus_self15 = ((STR) &Thetypeofthecall);
   L269 = call_context_err2;
   plus_sarg10 = (*dTP_strrSTR[TAG(L269)])(L269);
   ret_val55 = STR_ap2004550586(plus_self15, plus_sarg10);
   plus_self16 = ret_val55;
   plus_sarg11 = ((STR) &isnotasubtypeof);
   ret_val56 = STR_ap2004550586(plus_self16, plus_sarg11);
   plus_self17 = ret_val56;
   L270 = call_context_err3;
   plus_sarg12 = (*dTP_strrSTR[TAG(L270)])(L270);
   ret_val57 = STR_ap2004550586(plus_self17, plus_sarg12);
   plus_self18 = ret_val57;
   plus_sarg13 = ((STR) &name18);
   ret_val58 = STR_ap2004550586(plus_self18, plus_sarg13);
   err_s5 = ret_val58;
   if ((err_self5==((TRANS) NULL))) {
    L272 = TRUE;
   } else {
    L272 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L272) {
    L271 = TRUE;
   } else {
    L271 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
   }
   if (L271) {
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   else {
    plus_self19 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
    plus_sarg14 = ((STR) &name21);
    ret_val59 = STR_ap2004550586(plus_self19, plus_sarg14);
    plus_self20 = ret_val59;
    plus_sarg15 = err_s5;
    ret_val60 = STR_ap2004550586(plus_self20, plus_sarg15);
    err_s5 = ret_val60;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 ncr = ((dAM_EXPR) cr);
 if (TRANS_1951149652(self, cr)) {
  ncr = TRANS_711092593(self, cr);
 }
 else {
  SE_CON412941612(ATTR(self,cur_se), ((dAM) cr));
  if (cr==NULL) {
   goto other409;
  } else
  switch (TAG(cr)) {
   case AM_ROU1916046290_tag:
    special_inline_s = self;
    special_inline_c = ((AM_ROU1916046290) cr);
    L273 = ATTR(ATTR(special_inline_s,prog),all_reached);
    L2741_=!(L273); 
    if (L2741_) {
     L275 = ATTR(ATTR(special_inline_s,prog),generate_am);
     (*dGENER651759373[TAG(L275)])(L275, ATTR(special_inline_c,fun));
    }
    L276 = ATTR(ATTR(special_inline_s,prog),inliner);
    am = (*dINLIN970826022[TAG(L276)])(L276, special_inline_c);
    switch (TAG(am)) {
     case AM_STMT_EXPR_tag:
      SATTR(ATTR(special_inline_s,cur_rout),calls1,FLISTd1364738838(ATTR(ATTR(special_inline_s,cur_rout),calls1), ATTR(((AM_STMT_EXPR) am),calls1)));
      SATTR(ATTR(special_inline_s,cur_rout),locals1,FLISTA811588375(ATTR(ATTR(special_inline_s,cur_rout),locals1), ATTR(((AM_STMT_EXPR) am),locals1))); break;
     default: ;
    }
    ret_val61 = am;
    ncr = ret_val61; break;
   case AM_ITE1809135850_tag:
    ncr = TRANS_2076826648(self, ((AM_ITE1809135850) cr));
    if (ncr==NULL) {
     goto other410;
    } else
    switch (TAG(ncr)) {
     case AM_ITE1809135850_tag:
      SATTR(ATTR(self,cur_loop),its1,FLISTA943034044(ATTR(ATTR(self,cur_loop),its1), ((AM_ITE1809135850) ncr))); break;
     default: ;
     other410: ;
    } break;
   default: ;
   other409: ;
  }
 }
 psather_self2 = ATTR(self,prog);
 if (ATTR(psather_self2,threads)) {
  L277 = TRUE;
 } else {
  L277 = ATTR(psather_self2,distributed);
 }
 ret_val62 = L277;
 if (ret_val62) {
  TRANS_317173573(self, ((dAM_EXPR) cr));
 }
 L278 = (ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL));
 L2791_=!(L278); 
 if (L2791_) {
  if (ncr==NULL) {
   goto other411;
  } else
  switch (TAG(ncr)) {
   case AM_BND367211769_tag:
   case AM_BND260301329_tag:
   case AM_EXT_CALL_EXPR_tag:
   case AM_ITE1809135850_tag:
   case AM_ROU1916046290_tag:
    SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), ncr));
    {
     BOOL f_L2801_ = TRUE;
     while (1) {
      if (f_L2801_) {
       f_L2801_ = FALSE;
       L21 = 0;
       L282 = ((dAM_CALL_EXPR) ncr);
       L283 = (*dAM_CA1128544926[TAG(L282)])(L282);
       L2841_=INTMINUS(L283,1); 
       L31 = L2841_;
       L2801_=L21-1;L2801_m=L31; 
      }
      if(L2801_++>=L2801_m)  goto after_loop5; 
      i5 = L2801_;
      L286 = ((dAM_CALL_EXPR) ncr);
      L287=(*dAM_CA967858872[TAG(L286)])(L286, i5);
      L288 = ATTR(L287,mode1);
      if ((*dMODE_814247358[TAG(L288)])(L288, ((dMODE) MODES_out_mode))) {
       L285 = TRUE;
      } else {
       L289 = ((dAM_CALL_EXPR) ncr);
       L290=(*dAM_CA967858872[TAG(L289)])(L289, i5);
       L291 = ATTR(L290,mode1);
       L285 = (*dMODE_814247358[TAG(L291)])(L291, ((dMODE) MODES_inout_mode));
      }
      if (L285) {
       L292 = ((dAM_CALL_EXPR) ncr);
       L293=(*dAM_CA967858872[TAG(L292)])(L292, i5);
       SE_CON1222696490(ATTR(self,cur_se), ((dAM) ATTR(L293,expr)), TRUE);
      }
     }
    }
    after_loop5: ; break;
   default: ;
   other411: ;
  }
 }
 ret_val = ncr;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_2076826648(TRANS self, AM_ITE1809135850 call11) {
 dAM_EXPR ret_val;
 dAM_EXPR am;
 BOOL L1;
 BOOL L21_;
 dGENERATE_AM L3;
 dINLINE L4;
 L1 = ATTR(ATTR(self,prog),all_reached);
 L21_=!(L1); 
 if (L21_) {
  L3 = ATTR(ATTR(self,prog),generate_am);
  (*dGENER651759373[TAG(L3)])(L3, ATTR(call11,fun));
 }
 L4 = ATTR(ATTR(self,prog),inliner);
 am = (*dINLIN17214190[TAG(L4)])(L4, call11);
 if (am==NULL) {
  goto other403;
 } else
 switch (TAG(am)) {
  case AM_STMT_EXPR_tag:
   SATTR(ATTR(self,cur_rout),calls1,FLISTd1364738838(ATTR(ATTR(self,cur_rout),calls1), ATTR(((AM_STMT_EXPR) am),calls1)));
   SATTR(ATTR(self,cur_rout),locals1,FLISTA811588375(ATTR(ATTR(self,cur_rout),locals1), ATTR(((AM_STMT_EXPR) am),locals1)));
   SATTR(ATTR(self,cur_loop),its1,FLISTA572736043(ATTR(ATTR(self,cur_loop),its1), ATTR(((AM_STMT_EXPR) am),its1)));
   SATTR(ATTR(self,cur_loop),bits1,FLISTA1236996208(ATTR(ATTR(self,cur_loop),bits1), ATTR(((AM_STMT_EXPR) am),bits1)));
   SATTR(ATTR(self,cur_loop),firsts,FLISTA811588375(ATTR(ATTR(self,cur_loop),firsts), ATTR(((AM_STMT_EXPR) am),firsts))); break;
  default: ;
  other403: ;
 }
 ret_val = am;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_593837946(TRANS self, AS_SELF_EXPR e, dTP tp) {
 dAM_EXPR ret_val;
 AM_LOCAL_EXPR sl;
 TRANS self_const_err_s;
 AS_SELF_EXPR self_const_err_e;
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
 AM_ROUT_DEF self_local_self;
 AM_LOCAL_EXPR ret_val3;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val4;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val5;
 TRANS self_context_err;
 AS_SELF_EXPR self_context_err1;
 dTP self_context_err2;
 dTP self_context_err3;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 TRANS err_self1;
 STR err_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 extern STR selfma1628479826;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 AM_FORMAL_ARG L31_;
 AM_FORMAL_ARG L4;
 BOOL L5;
 BOOL L61_;
 dTP L7;
 BOOL L8;
 BOOL L91_;
 extern STR Thetypeofself;
 dTP L10;
 extern STR isnotasubtypeof;
 dTP L11;
 extern STR name18;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 if (ATTR(self,in_constant)) {
  self_const_err_s = self;
  self_const_err_e = e;
  err_loc_self = self_const_err_s;
  err_loc_t = ((dPROG_ERR) self_const_err_e);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = self_const_err_s;
  err_s = ((STR) &selfma1628479826);
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
 self_local_self = ATTR(self,cur_rout);
 L31_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)self_local_self,0); 
 L4=L31_;
 ret_val3 = ATTR(L4,expr);
 sl = ret_val3;
 L5 = (tp==((dTP) NULL));
 L61_=!(L5); 
 if (L61_) {
  tp_self = sl;
  ret_val4 = ATTR(tp_self,tp_at);
  L7 = ret_val4;
  L8 = (*dTP_is1999456142[TAG(L7)])(L7, tp);
  L91_=!(L8); 
  if (L91_) {
   self_context_err = self;
   self_context_err1 = e;
   tp_self1 = sl;
   ret_val5 = ATTR(tp_self1,tp_at);
   self_context_err2 = ret_val5;
   self_context_err3 = tp;
   err_loc_self1 = self_context_err;
   err_loc_t1 = ((dPROG_ERR) self_context_err1);
   PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
   err_self1 = self_context_err;
   plus_self2 = ((STR) &Thetypeofself);
   L10 = self_context_err2;
   plus_sarg2 = (*dTP_strrSTR[TAG(L10)])(L10);
   ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val6;
   plus_sarg3 = ((STR) &isnotasubtypeof);
   ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val7;
   L11 = self_context_err3;
   plus_sarg4 = (*dTP_strrSTR[TAG(L11)])(L11);
   ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val8;
   plus_sarg5 = ((STR) &name18);
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s1 = ret_val9;
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
    ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val10;
    plus_sarg7 = err_s1;
    ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s1 = ret_val11;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   ret_val = ((dAM_EXPR) NULL);
   return ret_val;
  }
 }
 ret_val = ((dAM_EXPR) sl);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR TRANS_711092593(TRANS self, dAM_CALL_EXPR cr) {
 dAM_EXPR ret_val;
 AM_STMT_EXPR res = ((AM_STMT_EXPR) NULL);
 ARRAYAM_CALL_ARG args = ((ARRAYAM_CALL_ARG) NULL);
 INT start1 = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 AM_CALL_ARG a;
 dAM_EXPR ae;
 AM_LOCAL_EXPR l;
 AM_ASSIGN_STMT stmt1;
 dAM_STMT new_call_stmt = ((dAM_STMT) NULL);
 AM_LOCAL_EXPR new_res = ((AM_LOCAL_EXPR) NULL);
 dTP ret_tp = ((dTP) NULL);
 dAM_EXPR ncr = ((dAM_EXPR) NULL);
 AM_ROU1916046290 nr;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 AM_ITE1809135850 ni;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT i2 = INT_zero;
 AM_BND260301329 nb;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT i3 = INT_zero;
 AM_BND367211769 nbi;
 AM_EXT_CALL_EXPR ne;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT i4 = INT_zero;
 AM_EXPR_STMT expr_stmt;
 AM_ASSIGN_STMT ass_stmt;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 INT i5 = INT_zero;
 AM_CALL_ARG a11;
 dAM_EXPR ae1;
 AM_ROU1916046290 aset_call;
 CALL_SIG aset_call_sig;
 ARRAYARG L141;
 ARRAYCALL_ARG L151;
 AM_ROU1916046290 L161;
 AM_ROU1916046290 L171;
 AM_EXPR_STMT expr_stmt1;
 AM_ROU1916046290 writer_call;
 CALL_SIG writer_call_sig;
 AM_EXPR_STMT expr_stmt2;
 AM_ASSIGN_STMT stmt2 = ((AM_ASSIGN_STMT) NULL);
 AM_STMT_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val1;
 AM_STMT_EXPR r;
 ARRAYAM_CALL_ARG create_self1;
 INT create_n = INT_zero;
 ARRAYAM_CALL_ARG ret_val2;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val3;
 AM_LOCAL_EXPR r1;
 AM_ASSIGN_STMT create_self3;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val4;
 AM_ASSIGN_STMT r2;
 AM_CALL_ARG create_self4;
 dAM_EXPR create_e;
 dMODE create_m;
 AM_CALL_ARG ret_val5;
 AM_CALL_ARG res1;
 AM_ROU1916046290 create_self5;
 INT create_n1 = INT_zero;
 AM_ROU1916046290 ret_val6;
 TRANS special_inline_s;
 AM_ROU1916046290 special_inline_c;
 dAM_EXPR ret_val7;
 dAM_EXPR am;
 AM_ITE1809135850 create_self6;
 INT create_n2 = INT_zero;
 AM_ITE1809135850 ret_val8;
 AM_BND260301329 create_self7;
 INT create_n3 = INT_zero;
 AM_BND260301329 ret_val9;
 AM_BND260301329 tp_self;
 dTP ret_val10;
 AM_BND367211769 copy_self;
 AM_BND367211769 ret_val11;
 OUT create_self8;
 OUT ret_val12;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val13;
 FILE1 r3;
 AM_EXT_CALL_EXPR create_self9;
 INT create_n4 = INT_zero;
 AM_EXT_CALL_EXPR ret_val14;
 AM_EXPR_STMT create_self10;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val15;
 AM_EXPR_STMT r4;
 AM_ASSIGN_STMT create_self11;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val16;
 AM_ASSIGN_STMT r5;
 AM_LOCAL_EXPR create_self12;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AM_LOCAL_EXPR ret_val17;
 AM_LOCAL_EXPR r6;
 TRANS err_self;
 STR err_s;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val18;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val19;
 AM_ROU1916046290 create_self13;
 INT create_n5 = INT_zero;
 AM_ROU1916046290 ret_val20;
 CALL_SIG create_self14;
 CALL_SIG ret_val21;
 ARRAYCALL_ARG create_self15;
 INT create_n6 = INT_zero;
 ARRAYCALL_ARG ret_val22;
 INT L181 = INT_zero;
 CALL_ARG create_self16;
 dCALL_TP create_t;
 CALL_ARG ret_val23;
 CALL_ARG res2;
 CALL_ARG create_self17;
 dCALL_TP create_t1;
 CALL_ARG ret_val24;
 CALL_ARG res3;
 TRANS err_self1;
 STR err_s1;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val25;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val26;
 INT L191 = INT_zero;
 AM_CALL_ARG create_self18;
 dAM_EXPR create_e1;
 AM_CALL_ARG ret_val27;
 AM_CALL_ARG res4;
 AM_EXPR_STMT create_self19;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val28;
 AM_EXPR_STMT r7;
 SIG is_reader_sig_se;
 BOOL ret_val29 = BOOL_zero;
 TRANS err_self2;
 STR err_s2;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val30;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val31;
 AM_ROU1916046290 create_self20;
 INT create_n7 = INT_zero;
 AM_ROU1916046290 ret_val32;
 CALL_SIG create_self21;
 CALL_SIG ret_val33;
 ARRAYCALL_ARG create_self22;
 INT create_n8 = INT_zero;
 ARRAYCALL_ARG ret_val34;
 CALL_ARG create_self23;
 dCALL_TP create_t2;
 CALL_ARG ret_val35;
 CALL_ARG res5;
 TRANS err_self3;
 STR err_s3;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val36;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val37;
 AM_CALL_ARG create_self24;
 dAM_EXPR create_e2;
 AM_CALL_ARG ret_val38;
 AM_CALL_ARG res6;
 AM_EXPR_STMT create_self25;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val39;
 AM_EXPR_STMT r8;
 AM_ASSIGN_STMT create_self26;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val40;
 AM_ASSIGN_STMT r9;
 dAM_CALL_EXPR L20;
 AM_STMT_EXPR L22;
 OB L23;
 dAM_CALL_EXPR L24;
 ARRAYAM_CALL_ARG L25;
 INT L26;
 OB L27;
 INT L28;
 AM_CALL_ARG L291_;
 AM_CALL_ARG L30;
 AM_CALL_ARG L331_;
 AM_CALL_ARG L34;
 INT L37;
 dAM_CALL_EXPR L38;
 INT L39;
 INT L401_;
 INT L361_,L361_m;
 dAM_CALL_EXPR L42;
 dAM_CALL_EXPR L44;
 dAM_EXPR L45;
 AM_LOCAL_EXPR L46;
 OB L47;
 dAM_CALL_EXPR L48;
 AM_ASSIGN_STMT L49;
 OB L50;
 dAM_STMT L52;
 AM_CALL_ARG L53;
 OB L54;
 AM_CALL_ARG L55;
 INT L571_;
 AM_ROU1916046290 L58;
 INT L59;
 OB L60;
 INT L62;
 INT L64;
 INT L651_;
 INT L66;
 INT L671_;
 INT L631_,L631_m;
 AM_CALL_ARG L681_;
 AM_CALL_ARG L69;
 BOOL L72;
 BOOL L731_;
 dGENERATE_AM L74;
 dINLINE L75;
 INT L761_;
 AM_ITE1809135850 L77;
 INT L78;
 OB L79;
 INT L80;
 INT L83;
 INT L841_;
 INT L85;
 INT L861_;
 INT L821_,L821_m;
 AM_CALL_ARG L871_;
 AM_CALL_ARG L88;
 INT L901_;
 AM_BND260301329 L92;
 INT L93;
 OB L94;
 INT L95;
 INT L97;
 INT L981_;
 INT L99;
 INT L1001_;
 INT L961_,L961_m;
 AM_CALL_ARG L1021_;
 AM_CALL_ARG L103;
 extern STR Itisno2078445089;
 FILE1 L105;
 OB L106;
 INT L1081_;
 AM_EXT_CALL_EXPR L109;
 INT L110;
 OB L111;
 INT L112;
 INT L1141_;
 INT L115;
 INT L1161_;
 INT L1131_,L1131_m;
 INT L117;
 AM_CALL_ARG L1181_;
 AM_CALL_ARG L119;
 dAM_CALL_EXPR L122;
 AM_EXPR_STMT L123;
 OB L124;
 dAM_CALL_EXPR L125;
 AM_ASSIGN_STMT L126;
 OB L127;
 dAM_CALL_EXPR L128;
 AM_LOCAL_EXPR L129;
 OB L130;
 dAM_STMT L132;
 INT L134;
 dAM_CALL_EXPR L135;
 INT L136;
 INT L1371_;
 INT L1331_,L1331_m;
 dAM_CALL_EXPR L138;
 BOOL L139;
 AM_CALL_ARG L1401_;
 AM_CALL_ARG L142;
 dAM_EXPR L143;
 dTP L144;
 extern STR Itisil593397214;
 BOOL L145;
 BOOL L146;
 extern STR name21;
 INT L1471_;
 INT L148;
 INT L1491_;
 AM_ROU1916046290 L150;
 INT L152;
 OB L153;
 INT L154;
 CALL_SIG L155;
 OB L156;
 INT L1571_;
 ARRAYCALL_ARG L158;
 INT L159;
 OB L160;
 INT L162;
 INT L1631_br;
 ARG aL1631_;
 INT L1641_br;
 ARG L165;
 ARG L166;
 CALL_ARG L167;
 OB L168;
 CALL_ARG L169;
 INT L1701_;
 ARRAYCALL_ARG L172;
 INT L1731_;
 INT L174;
 INT L1751_;
 INT L176;
 CALL_ARG L177;
 OB L178;
 CALL_ARG L179;
 dTP L182;
 IFC L183;
 extern STR Passin645655410;
 BOOL L184;
 BOOL L185;
 extern STR name21;
 INT L1861_br;
 AM_CALL_ARG aL1861_;
 INT L1871_br;
 AM_CALL_ARG L188;
 AM_CALL_ARG L189;
 INT L1901_;
 INT L1921_;
 INT L193;
 INT L1941_;
 INT L195;
 AM_CALL_ARG L1961_;
 AM_CALL_ARG L197;
 AM_CALL_ARG L198;
 OB L199;
 AM_CALL_ARG L200;
 dAM_CALL_EXPR L202;
 AM_EXPR_STMT L203;
 OB L204;
 dAM_STMT L205;
 BOOL L206;
 BOOL L207;
 BOOL L2081_;
 AM_CALL_ARG L2091_;
 AM_CALL_ARG L210;
 dAM_EXPR L211;
 dTP L212;
 extern STR Itisil1872847488;
 BOOL L213;
 BOOL L214;
 extern STR name21;
 AM_ROU1916046290 L215;
 INT L216;
 OB L217;
 INT L218;
 CALL_SIG L219;
 OB L220;
 ARRAYCALL_ARG L221;
 INT L222;
 OB L223;
 INT L224;
 ARRAYCALL_ARG L225;
 CALL_ARG L226;
 OB L227;
 CALL_ARG L228;
 dTP L230;
 IFC L231;
 extern STR Passin247568571;
 BOOL L232;
 BOOL L233;
 extern STR name21;
 AM_CALL_ARG L2341_;
 AM_CALL_ARG L235;
 AM_CALL_ARG L2371_;
 AM_CALL_ARG L238;
 dAM_EXPR L239;
 AM_CALL_ARG L240;
 OB L241;
 AM_CALL_ARG L242;
 dAM_CALL_EXPR L244;
 AM_EXPR_STMT L245;
 OB L246;
 dAM_STMT L247;
 dAM_CALL_EXPR L248;
 AM_ASSIGN_STMT L249;
 OB L250;
 AM_CALL_ARG L2511_;
 AM_CALL_ARG L252;
 dAM_EXPR L253;
 dAM_CALL_EXPR L254;
 AM_CALL_ARG L255;
 dAM_EXPR L256;
 dAM_STMT L257;
 create_self = ((AM_STMT_EXPR) NULL);
 L20 = cr;
 create_source = (*dAM_CA1298533540[TAG(L20)])(L20);
 L23=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=AM_STMT_EXPR_tag;
 L22 = ((AM_STMT_EXPR) L23);
 r = L22;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 res = ret_val1;
 create_self1 = ((ARRAYAM_CALL_ARG) NULL);
 L24 = cr;
 create_n = (*dAM_CA1128544926[TAG(L24)])(L24);
 L26 = create_n;
 L28=(sizeof(struct ARRAYAM_CALL_ARG_struct)-sizeof(AM_CALL_ARG))+(L26)*sizeof(AM_CALL_ARG);
 L27=ZALLOC_BIG(L28);
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=ARRAYAM_CALL_ARG_tag;
#ifdef DESTROY_CHK

   ((OB)L27)->header.destroyed=0;
#endif

 L25 = ((ARRAYAM_CALL_ARG) L27);
 L25->asize = L26;
 ret_val2 = L25;
 args = ret_val2;
 start1 = 0;
 if (cr==NULL) {
  goto other404;
 } else
 switch (TAG(cr)) {
  case AM_ROU1916046290_tag:
   start1 = 1;
   L291_=(AM_CALL_ARG)ARR((AM_ROU1916046290)cr,0); 
   L30 = AM_CAL1030878588(L291_);
   SARR((ARRAYAM_CALL_ARG)args,0,(AM_CALL_ARG)L30); 
   ; break;
  case AM_ITE1809135850_tag:
   start1 = 1;
   L331_=(AM_CALL_ARG)ARR((AM_ITE1809135850)cr,0); 
   L34 = AM_CAL1030878588(L331_);
   SARR((ARRAYAM_CALL_ARG)args,0,(AM_CALL_ARG)L34); 
   ; break;
  default: ;
  other404: ;
 }
 {
  BOOL f_L361_ = TRUE;
  while (1) {
   if (f_L361_) {
    f_L361_ = FALSE;
    L11 = start1;
    L38 = cr;
    L39 = (*dAM_CA1128544926[TAG(L38)])(L38);
    L401_=INTMINUS(L39,1); 
    L21 = L401_;
    L361_=L11-1;L361_m=L21; 
   }
   if(L361_++>=L361_m)  goto after_loop; 
   i = L361_;
   L42 = cr;
   a = AM_CAL1030878588((*dAM_CA967858872[TAG(L42)])(L42, i));
   ae = ATTR(a,expr);
   if (ae==NULL) {
    goto other405;
   } else
   switch (TAG(ae)) {
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
     SARR((ARRAYAM_CALL_ARG)args,i,(AM_CALL_ARG)a); 
     ; break;
    default: ;
    other405: ;
     create_self2 = ((AM_LOCAL_EXPR) NULL);
     L44 = cr;
     create_src = (*dAM_CA1298533540[TAG(L44)])(L44);
     create_name = IDENT_zero;
     L45 = ae;
     create_tp = (*dAM_EXPR_tprdTP[TAG(L45)])(L45);
     L47=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
     if (L47==NULL) FATAL("Unable to allocate more memory");
     ((OB)L47)->header.tag=AM_LOCAL_EXPR_tag;
     L46 = ((AM_LOCAL_EXPR) L47);
     r1 = L46;
     SATTR(r1,source1,create_src);
     SATTR(r1,name1,create_name);
     SATTR(r1,tp_at,create_tp);
     ret_val3 = r1;
     l = ret_val3;
     create_self3 = ((AM_ASSIGN_STMT) NULL);
     L48 = cr;
     create_source1 = (*dAM_CA1298533540[TAG(L48)])(L48);
     L50=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
     if (L50==NULL) FATAL("Unable to allocate more memory");
     ((OB)L50)->header.tag=AM_ASSIGN_STMT_tag;
     L49 = ((AM_ASSIGN_STMT) L50);
     r2 = L49;
     SATTR(r2,source1,create_source1);
     ret_val4 = r2;
     stmt1 = ret_val4;
     SATTR(stmt1,src1,ae);
     TRANS_626807411(self, l);
     SATTR(stmt1,dest,((dAM_EXPR) l));
     if ((ATTR(res,stmts)==((dAM_STMT) NULL))) {
      SATTR(res,stmts,((dAM_STMT) stmt1));
     }
     else {
      L52 = ATTR(res,stmts);
      (*dAM_ST2020072576[TAG(L52)])(L52, ((dAM_STMT) stmt1));
     }
     create_self4 = ((AM_CALL_ARG) NULL);
     create_e = ((dAM_EXPR) l);
     create_m = ATTR(a,mode1);
     L54=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L54==NULL) FATAL("Unable to allocate more memory");
     ((OB)L54)->header.tag=AM_CALL_ARG_tag;
     L53 = ((AM_CALL_ARG) L54);
     res1 = L53;
     SATTR(res1,expr,create_e);
     SATTR(res1,mode1,create_m);
     ret_val5 = res1;
     L55 = ret_val5;
     SARR((ARRAYAM_CALL_ARG)args,i,(AM_CALL_ARG)L55); 
     ;
   }
  }
 }
 after_loop: ;
 switch (TAG(cr)) {
  case AM_ROU1916046290_tag:
   create_self5 = ((AM_ROU1916046290) NULL);
   L571_=ASIZE((AM_ROU1916046290)cr); 
   create_n1 = L571_;
   L59 = create_n1;
   L62=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L59)*sizeof(AM_CALL_ARG);
   L60=ZALLOC_BIG(L62);
   if (L60==NULL) FATAL("Unable to allocate more memory");
   ((OB)L60)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

     ((OB)L60)->header.destroyed=0;
#endif

   L58 = ((AM_ROU1916046290) L60);
   L58->asize = L59;
   ret_val6 = L58;
   nr = ret_val6;
   SATTR(nr,fun,ATTR(((AM_ROU1916046290) cr),fun));
   {
    BOOL f_L631_ = TRUE;
    while (1) {
     if (f_L631_) {
      f_L631_ = FALSE;
      L31 = 0;
      L651_=ASIZE((AM_ROU1916046290)cr); 
      L66 = L651_;
      L671_=INTMINUS(L66,1); 
      L41 = L671_;
      L631_=L31-1;L631_m=L41; 
     }
     if(L631_++>=L631_m)  goto after_loop1; 
     i1 = L631_;
     L681_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i1); 
     L69 = L681_;
     SARR((AM_ROU1916046290)nr,i1,(AM_CALL_ARG)L69); 
     ;
    }
   }
   after_loop1: ;
   ret_tp = ATTR(ATTR(nr,fun),ret);
   SE_CON412941612(ATTR(self,cur_se), ((dAM) nr));
   special_inline_s = self;
   special_inline_c = nr;
   L72 = ATTR(ATTR(special_inline_s,prog),all_reached);
   L731_=!(L72); 
   if (L731_) {
    L74 = ATTR(ATTR(special_inline_s,prog),generate_am);
    (*dGENER651759373[TAG(L74)])(L74, ATTR(special_inline_c,fun));
   }
   L75 = ATTR(ATTR(special_inline_s,prog),inliner);
   am = (*dINLIN970826022[TAG(L75)])(L75, special_inline_c);
   switch (TAG(am)) {
    case AM_STMT_EXPR_tag:
     SATTR(ATTR(special_inline_s,cur_rout),calls1,FLISTd1364738838(ATTR(ATTR(special_inline_s,cur_rout),calls1), ATTR(((AM_STMT_EXPR) am),calls1)));
     SATTR(ATTR(special_inline_s,cur_rout),locals1,FLISTA811588375(ATTR(ATTR(special_inline_s,cur_rout),locals1), ATTR(((AM_STMT_EXPR) am),locals1))); break;
    default: ;
   }
   ret_val7 = am;
   ncr = ret_val7; break;
  case AM_ITE1809135850_tag:
   create_self6 = ((AM_ITE1809135850) NULL);
   L761_=ASIZE((AM_ITE1809135850)cr); 
   create_n2 = L761_;
   L78 = create_n2;
   L80=(sizeof(struct AM_ITE1809135850_struct)-sizeof(AM_CALL_ARG))+(L78)*sizeof(AM_CALL_ARG);
   L79=ZALLOC_BIG(L80);
   if (L79==NULL) FATAL("Unable to allocate more memory");
   ((OB)L79)->header.tag=AM_ITE1809135850_tag;
#ifdef DESTROY_CHK

     ((OB)L79)->header.destroyed=0;
#endif

   L77 = ((AM_ITE1809135850) L79);
   L77->asize = L78;
   ret_val8 = L77;
   ni = ret_val8;
   SATTR(ni,fun,ATTR(((AM_ITE1809135850) cr),fun));
   SATTR(ni,init,ATTR(((AM_ITE1809135850) cr),init));
   SATTR(ni,lp,ATTR(((AM_ITE1809135850) cr),lp));
   SATTR(ni,init_before_loop,ATTR(((AM_ITE1809135850) cr),init_before_loop));
   {
    BOOL f_L821_ = TRUE;
    while (1) {
     if (f_L821_) {
      f_L821_ = FALSE;
      L51 = 0;
      L841_=ASIZE((AM_ITE1809135850)cr); 
      L85 = L841_;
      L861_=INTMINUS(L85,1); 
      L61 = L861_;
      L821_=L51-1;L821_m=L61; 
     }
     if(L821_++>=L821_m)  goto after_loop2; 
     i2 = L821_;
     L871_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i2); 
     L88 = L871_;
     SARR((AM_ITE1809135850)ni,i2,(AM_CALL_ARG)L88); 
     ;
    }
   }
   after_loop2: ;
   ret_tp = ATTR(ATTR(ni,fun),ret);
   SE_CON412941612(ATTR(self,cur_se), ((dAM) ni));
   ncr = TRANS_2076826648(self, ni);
   if (ncr==NULL) {
    goto other406;
   } else
   switch (TAG(ncr)) {
    case AM_ITE1809135850_tag:
     SATTR(ATTR(self,cur_loop),its1,FLISTA943034044(ATTR(ATTR(self,cur_loop),its1), ((AM_ITE1809135850) ncr))); break;
    default: ;
    other406: ;
   } break;
  case AM_BND260301329_tag:
   create_self7 = ((AM_BND260301329) NULL);
   L901_=ASIZE((AM_BND260301329)cr); 
   create_n3 = L901_;
   L93 = create_n3;
   L95=(sizeof(struct AM_BND260301329_struct)-sizeof(AM_CALL_ARG))+(L93)*sizeof(AM_CALL_ARG);
   L94=ZALLOC_BIG(L95);
   if (L94==NULL) FATAL("Unable to allocate more memory");
   ((OB)L94)->header.tag=AM_BND260301329_tag;
#ifdef DESTROY_CHK

     ((OB)L94)->header.destroyed=0;
#endif

   L92 = ((AM_BND260301329) L94);
   L92->asize = L93;
   ret_val9 = L92;
   nb = ret_val9;
   SATTR(nb,br,ATTR(((AM_BND260301329) cr),br));
   SATTR(nb,br_tp,ATTR(((AM_BND260301329) cr),br_tp));
   {
    BOOL f_L961_ = TRUE;
    while (1) {
     if (f_L961_) {
      f_L961_ = FALSE;
      L71 = 0;
      L981_=ASIZE((AM_BND260301329)cr); 
      L99 = L981_;
      L1001_=INTMINUS(L99,1); 
      L81 = L1001_;
      L961_=L71-1;L961_m=L81; 
     }
     if(L961_++>=L961_m)  goto after_loop3; 
     i3 = L961_;
     L1021_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i3); 
     L103 = L1021_;
     SARR((AM_BND260301329)nb,i3,(AM_CALL_ARG)L103); 
     ;
    }
   }
   after_loop3: ;
   ncr = ((dAM_EXPR) nb);
   tp_self = nb;
   ret_val10 = ATTR(ATTR(tp_self,br_tp),ret);
   ret_tp = ret_val10; break;
  case AM_BND367211769_tag:
   copy_self = ((AM_BND367211769) cr);
   create_self8 = ((OUT) NULL);
   ret_val12 = create_self8;
   plus_self = ret_val12;
   plus_s = ((STR) &Itisno2078445089);
   stdout_self = ((FILE1) NULL);
   L106=ZALLOC(sizeof(struct FILE1_struct));
   if (L106==NULL) FATAL("Unable to allocate more memory");
   ((OB)L106)->header.tag=FILE1_tag;
   L105 = ((FILE1) L106);
   r3 = L105;
   
   SATTR(r3,fp,stdout);
   ret_val13 = r3;
   FILE_plus_STR(ret_val13, plus_s);
   ret_val11 = ((AM_BND367211769) NULL);
   nbi = ret_val11; break;
  case AM_EXT_CALL_EXPR_tag:
   create_self9 = ((AM_EXT_CALL_EXPR) NULL);
   L1081_=ASIZE((AM_EXT_CALL_EXPR)cr); 
   create_n4 = L1081_;
   L110 = create_n4;
   L112=(sizeof(struct AM_EXT_CALL_EXPR_struct)-sizeof(AM_CALL_ARG))+(L110)*sizeof(AM_CALL_ARG);
   L111=ZALLOC_BIG(L112);
   if (L111==NULL) FATAL("Unable to allocate more memory");
   ((OB)L111)->header.tag=AM_EXT_CALL_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L111)->header.destroyed=0;
#endif

   L109 = ((AM_EXT_CALL_EXPR) L111);
   L109->asize = L110;
   ret_val14 = L109;
   ne = ret_val14;
   SATTR(ne,fun,ATTR(((AM_EXT_CALL_EXPR) cr),fun));
   SATTR(ne,nm,ATTR(((AM_EXT_CALL_EXPR) cr),nm));
   {
    BOOL f_L1131_ = TRUE;
    L91 = 0;
    L1141_=ASIZE((AM_EXT_CALL_EXPR)cr); 
    L115 = L1141_;
    L1161_=INTMINUS(L115,1); 
    L101 = L1161_;
    L1131_=L91-1;L1131_m=L101; 
    while (1) {
     if(L1131_++>=L1131_m)  goto after_loop4; 
     i4 = L1131_;
     L1181_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i4); 
     L119 = L1181_;
     SARR((AM_EXT_CALL_EXPR)ne,i4,(AM_CALL_ARG)L119); 
     ;
    }
   }
   after_loop4: ;
   ncr = ((dAM_EXPR) ne);
   ret_tp = ATTR(ATTR(ne,fun),ret); break;
  default: ;
   FATAL("No applicable type in typecase\nin TRANS::call_fix_out_args($AM_CALL_EXPR):$AM_EXPR\n./Middle/trans.sa:1904:14");
 }
 if ((ret_tp==((dTP) NULL))) {
  create_self10 = ((AM_EXPR_STMT) NULL);
  L122 = cr;
  create_source2 = (*dAM_CA1298533540[TAG(L122)])(L122);
  L124=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
  if (L124==NULL) FATAL("Unable to allocate more memory");
  ((OB)L124)->header.tag=AM_EXPR_STMT_tag;
  L123 = ((AM_EXPR_STMT) L124);
  r4 = L123;
  SATTR(r4,source1,create_source2);
  ret_val15 = r4;
  expr_stmt = ret_val15;
  SATTR(expr_stmt,expr,ncr);
  new_call_stmt = ((dAM_STMT) expr_stmt);
 }
 else {
  create_self11 = ((AM_ASSIGN_STMT) NULL);
  L125 = cr;
  create_source3 = (*dAM_CA1298533540[TAG(L125)])(L125);
  L127=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L127==NULL) FATAL("Unable to allocate more memory");
  ((OB)L127)->header.tag=AM_ASSIGN_STMT_tag;
  L126 = ((AM_ASSIGN_STMT) L127);
  r5 = L126;
  SATTR(r5,source1,create_source3);
  ret_val16 = r5;
  ass_stmt = ret_val16;
  create_self12 = ((AM_LOCAL_EXPR) NULL);
  L128 = cr;
  create_src1 = (*dAM_CA1298533540[TAG(L128)])(L128);
  create_name1 = IDENT_zero;
  create_tp1 = ret_tp;
  L130=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L130==NULL) FATAL("Unable to allocate more memory");
  ((OB)L130)->header.tag=AM_LOCAL_EXPR_tag;
  L129 = ((AM_LOCAL_EXPR) L130);
  r6 = L129;
  SATTR(r6,source1,create_src1);
  SATTR(r6,name1,create_name1);
  SATTR(r6,tp_at,create_tp1);
  ret_val17 = r6;
  new_res = ret_val17;
  TRANS_626807411(self, new_res);
  SATTR(ass_stmt,src1,ncr);
  SATTR(ass_stmt,dest,((dAM_EXPR) new_res));
  SATTR(res,expr,((dAM_EXPR) new_res));
  new_call_stmt = ((dAM_STMT) ass_stmt);
 }
 L132 = ATTR(res,stmts);
 (*dAM_ST2020072576[TAG(L132)])(L132, new_call_stmt);
 {
  BOOL f_L1331_ = TRUE;
  while (1) {
   if (f_L1331_) {
    f_L1331_ = FALSE;
    L121 = start1;
    L135 = cr;
    L136 = (*dAM_CA1128544926[TAG(L135)])(L135);
    L1371_=INTMINUS(L136,1); 
    L131 = L1371_;
    L1331_=L121-1;L1331_m=L131; 
   }
   if(L1331_++>=L1331_m)  goto after_loop5; 
   i5 = L1331_;
   L138 = cr;
   a11 = AM_CAL1030878588((*dAM_CA967858872[TAG(L138)])(L138, i5));
   ae1 = ATTR(a11,expr);
   if (SYSOBEQ(((OB) ATTR(a11,mode1)),((OB) MODES_out_mode))) {
    L139 = TRUE;
   } else {
    L139 = SYSOBEQ(((OB) ATTR(a11,mode1)),((OB) MODES_inout_mode));
   }
   if (L139) {
    if (ae1==NULL) {
     goto other407;
    } else
    switch (TAG(ae1)) {
     case AM_ROU1916046290_tag:
      if (TRANS_1319121910(self, ATTR(((AM_ROU1916046290) ae1),fun))) {
       L1401_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ae1,0); 
       L142=L1401_;
       L143 = ATTR(L142,expr);
       L144 = (*dAM_EXPR_tprdTP[TAG(L143)])(L143);
       if ((*dTP_is1334578382[TAG(L144)])(L144)) {
        err_self = self;
        err_s = ((STR) &Itisil593397214);
        if ((err_self==((TRANS) NULL))) {
         L146 = TRUE;
        } else {
         L146 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
        }
        if (L146) {
         L145 = TRUE;
        } else {
         L145 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
        }
        if (L145) {
         PROG_err_STR(ATTR(err_self,prog), err_s);
        }
        else {
         plus_self1 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
         plus_sarg = ((STR) &name21);
         ret_val18 = STR_ap2004550586(plus_self1, plus_sarg);
         plus_self2 = ret_val18;
         plus_sarg1 = err_s;
         ret_val19 = STR_ap2004550586(plus_self2, plus_sarg1);
         err_s = ret_val19;
         PROG_err_STR(ATTR(err_self,prog), err_s);
        }
       }
       create_self13 = ((AM_ROU1916046290) NULL);
       L1471_=(ae1)==NULL?0:ASIZE((AM_ROU1916046290)ae1); 
       L148 = L1471_;
       L1491_=INTPLUS(L148,1); 
       create_n5 = L1491_;
       L152 = create_n5;
       L154=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L152)*sizeof(AM_CALL_ARG);
       L153=ZALLOC_BIG(L154);
       if (L153==NULL) FATAL("Unable to allocate more memory");
       ((OB)L153)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

         ((OB)L153)->header.destroyed=0;
#endif

       L150 = ((AM_ROU1916046290) L153);
       L150->asize = L152;
       ret_val20 = L150;
       aset_call = ret_val20;
       create_self14 = ((CALL_SIG) NULL);
       L156=ZALLOC(sizeof(struct CALL_SIG_struct));
       if (L156==NULL) FATAL("Unable to allocate more memory");
       ((OB)L156)->header.tag=CALL_SIG_tag;
       L155 = ((CALL_SIG) L156);
       ret_val21 = L155;
       aset_call_sig = ret_val21;
       create_self15 = ((ARRAYCALL_ARG) NULL);
       L1571_=(ae1)==NULL?0:ASIZE((AM_ROU1916046290)ae1); 
       create_n6 = L1571_;
       L159 = create_n6;
       L162=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L159)*sizeof(CALL_ARG);
       L160=ZALLOC_BIG(L162);
       if (L160==NULL) FATAL("Unable to allocate more memory");
       ((OB)L160)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

         ((OB)L160)->header.destroyed=0;
#endif

       L158 = ((ARRAYCALL_ARG) L160);
       L158->asize = L159;
       ret_val22 = L158;
       SATTR(aset_call_sig,args,ret_val22);
       SATTR(aset_call_sig,tp,ATTR(ATTR(((AM_ROU1916046290) ae1),fun),tp));
       SATTR(aset_call_sig,name1,IDENT_667688077);
       SATTR(aset_call_sig,has_ret,FALSE);
       {
        BOOL f_L1631_ = TRUE;
        BOOL f_L1641_ = TRUE;
        /* loop index variable */
        L181 = 0;
        L141 = ATTR(ATTR(((AM_ROU1916046290) ae1),fun),args);
        L1631_br=L141==NULL?0:ASIZE((ARRAYARG)L141); 
        while (1) {
         if (f_L1641_) {
          f_L1641_ = FALSE;
          L151 = ATTR(aset_call_sig,args);
          L1641_br=L151==NULL?0:ASIZE((ARRAYCALL_ARG)L151); 
         }
         if(L181>=L1641_br)  goto after_loop6; 
         create_self16 = ((CALL_ARG) NULL);
         if(L181>=L1631_br)  goto after_loop6; 
         aL1631_=ARR((ARRAYARG)L141,L181); 
         L166=aL1631_;
         create_t = ((dCALL_TP) ATTR(L166,tp));
         L168=ZALLOC(sizeof(struct CALL_ARG_struct));
         if (L168==NULL) FATAL("Unable to allocate more memory");
         ((OB)L168)->header.tag=CALL_ARG_tag;
         L167 = ((CALL_ARG) L168);
         res2 = L167;
         SATTR(res2,tp,create_t);
         SATTR(res2,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
         ret_val23 = res2;
         L169 = ret_val23;
         SARR((ARRAYCALL_ARG)L151,L181,(CALL_ARG)L169); 
         ;
         L1701_=INTPLUS(L181,1); 
         L181 = L1701_;
        }
       }
       after_loop6: ;
       L172 = ATTR(aset_call_sig,args);
       L1731_=(ae1)==NULL?0:ASIZE((AM_ROU1916046290)ae1); 
       L174 = L1731_;
       L1751_=INTMINUS(L174,1); 
       L176 = L1751_;
       create_self17 = ((CALL_ARG) NULL);
       create_t1 = ((dCALL_TP) ATTR(ATTR(((AM_ROU1916046290) ae1),fun),ret));
       L178=ZALLOC(sizeof(struct CALL_ARG_struct));
       if (L178==NULL) FATAL("Unable to allocate more memory");
       ((OB)L178)->header.tag=CALL_ARG_tag;
       L177 = ((CALL_ARG) L178);
       res3 = L177;
       SATTR(res3,tp,create_t1);
       SATTR(res3,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
       ret_val24 = res3;
       L179 = ret_val24;
       SARR((ARRAYCALL_ARG)L172,L176,(CALL_ARG)L179); 
       ;
       L182 = ATTR(ATTR(((AM_ROU1916046290) ae1),fun),tp);
       L183 = (*dTP_ifcrIFC[TAG(L182)])(L182);
       SATTR(aset_call,fun,IFC_si178998979(L183, aset_call_sig));
       if ((ATTR(aset_call,fun)==((SIG) NULL))) {
        err_self1 = self;
        err_s1 = ((STR) &Passin645655410);
        if ((err_self1==((TRANS) NULL))) {
         L185 = TRUE;
        } else {
         L185 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
        }
        if (L185) {
         L184 = TRUE;
        } else {
         L184 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
        }
        if (L184) {
         PROG_err_STR(ATTR(err_self1,prog), err_s1);
        }
        else {
         plus_self3 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
         plus_sarg2 = ((STR) &name21);
         ret_val25 = STR_ap2004550586(plus_self3, plus_sarg2);
         plus_self4 = ret_val25;
         plus_sarg3 = err_s1;
         ret_val26 = STR_ap2004550586(plus_self4, plus_sarg3);
         err_s1 = ret_val26;
         PROG_err_STR(ATTR(err_self1,prog), err_s1);
        }
       }
       else {
        {
         BOOL f_L1861_ = TRUE;
         BOOL f_L1871_ = TRUE;
         /* loop index variable */
         L191 = 0;
         L161 = ((AM_ROU1916046290) ae1);
         L171 = aset_call;
         L1861_br=L161==NULL?0:ASIZE((AM_ROU1916046290)L161); 
         L1871_br=L171==NULL?0:ASIZE((AM_ROU1916046290)L171); 
         while (1) {
          if(L191>=L1871_br)  goto after_loop7; 
          if(L191>=L1861_br)  goto after_loop7; 
          aL1861_=ARR((AM_ROU1916046290)L161,L191); 
          L189 = aL1861_;
          SARR((AM_ROU1916046290)L171,L191,(AM_CALL_ARG)L189); 
          ;
          L1901_=INTPLUS(L191,1); 
          L191 = L1901_;
         }
        }
        after_loop7: ;
        L1921_=(aset_call)==NULL?0:ASIZE((AM_ROU1916046290)aset_call); 
        L193 = L1921_;
        L1941_=INTMINUS(L193,1); 
        L195 = L1941_;
        create_self18 = ((AM_CALL_ARG) NULL);
        L1961_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i5); 
        L197=L1961_;
        create_e1 = ATTR(L197,expr);
        L199=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
        if (L199==NULL) FATAL("Unable to allocate more memory");
        ((OB)L199)->header.tag=AM_CALL_ARG_tag;
        L198 = ((AM_CALL_ARG) L199);
        res4 = L198;
        SATTR(res4,expr,create_e1);
        SATTR(res4,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
        ret_val27 = res4;
        L200 = ret_val27;
        SARR((AM_ROU1916046290)aset_call,L195,(AM_CALL_ARG)L200); 
        ;
        SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), ((dAM_EXPR) aset_call)));
        create_self19 = ((AM_EXPR_STMT) NULL);
        L202 = cr;
        create_source4 = (*dAM_CA1298533540[TAG(L202)])(L202);
        L204=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
        if (L204==NULL) FATAL("Unable to allocate more memory");
        ((OB)L204)->header.tag=AM_EXPR_STMT_tag;
        L203 = ((AM_EXPR_STMT) L204);
        r7 = L203;
        SATTR(r7,source1,create_source4);
        ret_val28 = r7;
        expr_stmt1 = ret_val28;
        SATTR(expr_stmt1,expr,((dAM_EXPR) aset_call));
        L205 = ATTR(res,stmts);
        (*dAM_ST2020072576[TAG(L205)])(L205, ((dAM_STMT) expr_stmt1));
       }
      }
      else {
       is_reader_sig_se = ATTR(((AM_ROU1916046290) ae1),fun);
       if ((ATTR(is_reader_sig_se,args)==((ARRAYARG) NULL))) {
        L207 = (ATTR(is_reader_sig_se,ret)==((dTP) NULL));
        L2081_=!(L207); 
        L206 = L2081_;
       } else {
        L206 = FALSE;
       }
       ret_val29 = L206;
       if (ret_val29) {
        L2091_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ae1,0); 
        L210=L2091_;
        L211 = ATTR(L210,expr);
        L212 = (*dAM_EXPR_tprdTP[TAG(L211)])(L211);
        if ((*dTP_is1334578382[TAG(L212)])(L212)) {
         err_self2 = self;
         err_s2 = ((STR) &Itisil1872847488);
         if ((err_self2==((TRANS) NULL))) {
          L214 = TRUE;
         } else {
          L214 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
         }
         if (L214) {
          L213 = TRUE;
         } else {
          L213 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
         }
         if (L213) {
          PROG_err_STR(ATTR(err_self2,prog), err_s2);
         }
         else {
          plus_self5 = ATTR(ATTR(ATTR(err_self2,cur_rout),sig1),str);
          plus_sarg4 = ((STR) &name21);
          ret_val30 = STR_ap2004550586(plus_self5, plus_sarg4);
          plus_self6 = ret_val30;
          plus_sarg5 = err_s2;
          ret_val31 = STR_ap2004550586(plus_self6, plus_sarg5);
          err_s2 = ret_val31;
          PROG_err_STR(ATTR(err_self2,prog), err_s2);
         }
        }
        create_self20 = ((AM_ROU1916046290) NULL);
        create_n7 = 2;
        L216 = create_n7;
        L218=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L216)*sizeof(AM_CALL_ARG);
        L217=ZALLOC_BIG(L218);
        if (L217==NULL) FATAL("Unable to allocate more memory");
        ((OB)L217)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

          ((OB)L217)->header.destroyed=0;
#endif

        L215 = ((AM_ROU1916046290) L217);
        L215->asize = L216;
        ret_val32 = L215;
        writer_call = ret_val32;
        create_self21 = ((CALL_SIG) NULL);
        L220=ZALLOC(sizeof(struct CALL_SIG_struct));
        if (L220==NULL) FATAL("Unable to allocate more memory");
        ((OB)L220)->header.tag=CALL_SIG_tag;
        L219 = ((CALL_SIG) L220);
        ret_val33 = L219;
        writer_call_sig = ret_val33;
        create_self22 = ((ARRAYCALL_ARG) NULL);
        create_n8 = 1;
        L222 = create_n8;
        L224=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L222)*sizeof(CALL_ARG);
        L223=ZALLOC_BIG(L224);
        if (L223==NULL) FATAL("Unable to allocate more memory");
        ((OB)L223)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

          ((OB)L223)->header.destroyed=0;
#endif

        L221 = ((ARRAYCALL_ARG) L223);
        L221->asize = L222;
        ret_val34 = L221;
        SATTR(writer_call_sig,args,ret_val34);
        SATTR(writer_call_sig,tp,ATTR(ATTR(((AM_ROU1916046290) ae1),fun),tp));
        SATTR(writer_call_sig,name1,ATTR(ATTR(((AM_ROU1916046290) ae1),fun),name1));
        SATTR(writer_call_sig,has_ret,FALSE);
        L225 = ATTR(writer_call_sig,args);
        create_self23 = ((CALL_ARG) NULL);
        create_t2 = ((dCALL_TP) ATTR(ATTR(((AM_ROU1916046290) ae1),fun),ret));
        L227=ZALLOC(sizeof(struct CALL_ARG_struct));
        if (L227==NULL) FATAL("Unable to allocate more memory");
        ((OB)L227)->header.tag=CALL_ARG_tag;
        L226 = ((CALL_ARG) L227);
        res5 = L226;
        SATTR(res5,tp,create_t2);
        SATTR(res5,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
        ret_val35 = res5;
        L228 = ret_val35;
        SARR((ARRAYCALL_ARG)L225,0,(CALL_ARG)L228); 
        ;
        L230 = ATTR(ATTR(((AM_ROU1916046290) ae1),fun),tp);
        L231 = (*dTP_ifcrIFC[TAG(L230)])(L230);
        SATTR(writer_call,fun,IFC_si178998979(L231, writer_call_sig));
        if ((ATTR(writer_call,fun)==((SIG) NULL))) {
         err_self3 = self;
         err_s3 = ((STR) &Passin247568571);
         if ((err_self3==((TRANS) NULL))) {
          L233 = TRUE;
         } else {
          L233 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
         }
         if (L233) {
          L232 = TRUE;
         } else {
          L232 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
         }
         if (L232) {
          PROG_err_STR(ATTR(err_self3,prog), err_s3);
         }
         else {
          plus_self7 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
          plus_sarg6 = ((STR) &name21);
          ret_val36 = STR_ap2004550586(plus_self7, plus_sarg6);
          plus_self8 = ret_val36;
          plus_sarg7 = err_s3;
          ret_val37 = STR_ap2004550586(plus_self8, plus_sarg7);
          err_s3 = ret_val37;
          PROG_err_STR(ATTR(err_self3,prog), err_s3);
         }
        }
        else {
         L2341_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ae1,0); 
         L235 = L2341_;
         SARR((AM_ROU1916046290)writer_call,0,(AM_CALL_ARG)L235); 
         ;
         create_self24 = ((AM_CALL_ARG) NULL);
         L2371_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i5); 
         L238=L2371_;
         L239 = ATTR(L238,expr);
         create_e2 = (*dAM_EX1830308833[TAG(L239)])(L239);
         L241=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
         if (L241==NULL) FATAL("Unable to allocate more memory");
         ((OB)L241)->header.tag=AM_CALL_ARG_tag;
         L240 = ((AM_CALL_ARG) L241);
         res6 = L240;
         SATTR(res6,expr,create_e2);
         SATTR(res6,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
         ret_val38 = res6;
         L242 = ret_val38;
         SARR((AM_ROU1916046290)writer_call,1,(AM_CALL_ARG)L242); 
         ;
         SATTR(ATTR(self,cur_rout),calls1,FLISTd805769956(ATTR(ATTR(self,cur_rout),calls1), ((dAM_EXPR) writer_call)));
         create_self25 = ((AM_EXPR_STMT) NULL);
         L244 = cr;
         create_source5 = (*dAM_CA1298533540[TAG(L244)])(L244);
         L246=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
         if (L246==NULL) FATAL("Unable to allocate more memory");
         ((OB)L246)->header.tag=AM_EXPR_STMT_tag;
         L245 = ((AM_EXPR_STMT) L246);
         r8 = L245;
         SATTR(r8,source1,create_source5);
         ret_val39 = r8;
         expr_stmt2 = ret_val39;
         SATTR(expr_stmt2,expr,((dAM_EXPR) writer_call));
         L247 = ATTR(res,stmts);
         (*dAM_ST2020072576[TAG(L247)])(L247, ((dAM_STMT) expr_stmt2));
        }
       }
      } break;
     default: ;
     other407: ;
      create_self26 = ((AM_ASSIGN_STMT) NULL);
      L248 = cr;
      create_source6 = (*dAM_CA1298533540[TAG(L248)])(L248);
      L250=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
      if (L250==NULL) FATAL("Unable to allocate more memory");
      ((OB)L250)->header.tag=AM_ASSIGN_STMT_tag;
      L249 = ((AM_ASSIGN_STMT) L250);
      r9 = L249;
      SATTR(r9,source1,create_source6);
      ret_val40 = r9;
      stmt2 = ret_val40;
      L2511_=(AM_CALL_ARG)ARR((ARRAYAM_CALL_ARG)args,i5); 
      L252=L2511_;
      L253 = ATTR(L252,expr);
      SATTR(stmt2,src1,(*dAM_EX1830308833[TAG(L253)])(L253));
      L254 = cr;
      L255=(*dAM_CA967858872[TAG(L254)])(L254, i5);
      L256 = ATTR(L255,expr);
      SATTR(stmt2,dest,(*dAM_EX1830308833[TAG(L256)])(L256));
      L257 = ATTR(res,stmts);
      (*dAM_ST2020072576[TAG(L257)])(L257, ((dAM_STMT) stmt2));
    }
   }
  }
 }
 after_loop5: ;
 ret_val = ((dAM_EXPR) res);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dCALL_TP TRANS_68137339(TRANS self, dAS_EXPR e) {
 dCALL_TP ret_val;
 CALL_T666389079 tua;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 CALL_TP_VOID create_self1;
 CALL_TP_VOID ret_val3;
 CALL_TP_CREATE create_self2;
 CALL_TP_CREATE ret_val4;
 CALL_T561792367 create_self3;
 CALL_T561792367 ret_val5;
 CALL_TP_ARRAY create_self4;
 CALL_TP_ARRAY ret_val6;
 CALL_T666389079 create_self5;
 CALL_T666389079 ret_val7;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val8;
 extern STR Compil204150738;
 FILE1 L1;
 OB L2;
 CALL_TP_VOID L4;
 OB L5;
 CALL_TP_CREATE L6;
 OB L7;
 CALL_T561792367 L8;
 OB L9;
 CALL_TP_ARRAY L10;
 OB L11;
 CALL_T666389079 L12;
 OB L13;
 BOOL L14;
 BOOL L151_;
 if ((e==((dAS_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Compil204150738);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val = ((dCALL_TP) NULL);
  return ret_val;
 }
 if (e==NULL) {
  goto other391;
 } else
 switch (TAG(e)) {
  case AS_VOID_EXPR_tag:
   create_self1 = ((CALL_TP_VOID) NULL);
   if ((CALL_T2135634286==((CALL_TP_VOID) NULL))) {
    L5=ZALLOC_LEAF(sizeof(struct CALL_TP_VOID_struct));
    if (L5==NULL) FATAL("Unable to allocate more memory");
    memset(L5,0,sizeof(struct CALL_TP_VOID_struct));
    ((OB)L5)->header.tag=CALL_TP_VOID_tag;
    L4 = ((CALL_TP_VOID) L5);
    CALL_T2135634286 = L4;
   }
   ret_val3 = CALL_T2135634286;
   ret_val = ((dCALL_TP) ret_val3);
   return ret_val; break;
  case AS_CREATE_EXPR_tag:
   if ((ATTR(((AS_CREATE_EXPR) e),tp)==((AS_TYPE_SPEC) NULL))) {
    create_self2 = ((CALL_TP_CREATE) NULL);
    if ((CALL_T1560699295==((CALL_TP_CREATE) NULL))) {
     L7=ZALLOC_LEAF(sizeof(struct CALL_TP_CREATE_struct));
     if (L7==NULL) FATAL("Unable to allocate more memory");
     memset(L7,0,sizeof(struct CALL_TP_CREATE_struct));
     ((OB)L7)->header.tag=CALL_TP_CREATE_tag;
     L6 = ((CALL_TP_CREATE) L7);
     CALL_T1560699295 = L6;
    }
    ret_val4 = CALL_T1560699295;
    ret_val = ((dCALL_TP) ret_val4);
    return ret_val;
   }
   else {
    ret_val = ((dCALL_TP) NULL);
    return ret_val;
   } break;
  case AS_BOU14269336_tag:
   create_self3 = ((CALL_T561792367) NULL);
   if ((CALL_T714618297==((CALL_T561792367) NULL))) {
    L9=ZALLOC_LEAF(sizeof(struct CALL_T561792367_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    memset(L9,0,sizeof(struct CALL_T561792367_struct));
    ((OB)L9)->header.tag=CALL_T561792367_tag;
    L8 = ((CALL_T561792367) L9);
    CALL_T714618297 = L8;
   }
   ret_val5 = CALL_T714618297;
   ret_val = ((dCALL_TP) ret_val5);
   return ret_val; break;
  case AS_ARRAY_EXPR_tag:
   create_self4 = ((CALL_TP_ARRAY) NULL);
   if ((CALL_T1969158839==((CALL_TP_ARRAY) NULL))) {
    L11=ZALLOC_LEAF(sizeof(struct CALL_TP_ARRAY_struct));
    if (L11==NULL) FATAL("Unable to allocate more memory");
    memset(L11,0,sizeof(struct CALL_TP_ARRAY_struct));
    ((OB)L11)->header.tag=CALL_TP_ARRAY_tag;
    L10 = ((CALL_TP_ARRAY) L11);
    CALL_T1969158839 = L10;
   }
   ret_val6 = CALL_T1969158839;
   ret_val = ((dCALL_TP) ret_val6);
   return ret_val; break;
  case AS_UND152986614_tag:
   create_self5 = ((CALL_T666389079) NULL);
   L13=ZALLOC(sizeof(struct CALL_T666389079_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=CALL_T666389079_tag;
   L12 = ((CALL_T666389079) L13);
   ret_val7 = L12;
   tua = ret_val7;
   L14 = (ATTR(((AS_UND152986614) e),tp)==((AS_TYPE_SPEC) NULL));
   L151_=!(L14); 
   if (L151_) {
    tp_of_self = self;
    tp_of_t = ATTR(((AS_UND152986614) e),tp);
    ret_val8 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
    SATTR(tua,tp,ret_val8);
   }
   ret_val = ((dCALL_TP) tua);
   return ret_val; break;
  default: ;
  other391: ;
   ret_val = ((dCALL_TP) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_317173573(TRANS self, dAM_EXPR ncs) {
 AM_ROU1916046290 needs_import_sel;
 BOOL ret_val = BOOL_zero;
 AM_ROUT_DEF needs_import_loc;
 SIG is_forked_self;
 BOOL ret_val1 = BOOL_zero;
 AM_ROU1916046290 needs_export_sel;
 BOOL ret_val2 = BOOL_zero;
 AM_ROUT_DEF needs_export_loc;
 SIG is_forked_self1;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 L2 = (ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = (ncs==((dAM_EXPR) NULL));
  L51_=!(L4); 
  L1 = L51_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  if (ncs==NULL) {
   goto other408;
  } else
  switch (TAG(ncs)) {
   case AM_ROU1916046290_tag:
    needs_import_sel = ((AM_ROU1916046290) ncs);
    ret_val = ATTR(ATTR(needs_import_sel,fun),needs_import);
    if (ret_val) {
     needs_import_loc = ATTR(self,cur_rout);
     is_forked_self = ATTR(needs_import_loc,sig1);
     if (ATTR(is_forked_self,is_par_routine)) {
      L7 = TRUE;
     } else {
      L7 = ATTR(is_forked_self,is_fork_routine);
     }
     if (L7) {
      L6 = TRUE;
     } else {
      L6 = ATTR(is_forked_self,is_attach_routin);
     }
     ret_val1 = L6;
     L8 = ret_val1;
     L91_=!(L8); 
     SATTR(ATTR(needs_import_loc,sig1),needs_import,L91_);
     SATTR(needs_import_loc,n_import_locals,TRUE);
    }
    needs_export_sel = ((AM_ROU1916046290) ncs);
    ret_val2 = ATTR(ATTR(needs_export_sel,fun),needs_export);
    if (ret_val2) {
     needs_export_loc = ATTR(self,cur_rout);
     is_forked_self1 = ATTR(needs_export_loc,sig1);
     if (ATTR(is_forked_self1,is_par_routine)) {
      L11 = TRUE;
     } else {
      L11 = ATTR(is_forked_self1,is_fork_routine);
     }
     if (L11) {
      L10 = TRUE;
     } else {
      L10 = ATTR(is_forked_self1,is_attach_routin);
     }
     ret_val3 = L10;
     L12 = ret_val3;
     L131_=!(L12); 
     SATTR(ATTR(needs_export_loc,sig1),needs_export,L131_);
     SATTR(needs_export_loc,n_export_locals,TRUE);
    } break;
   default: ;
   other408: ;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_350528331(TRANS self) {
 PROG psather_self;
 BOOL ret_val = BOOL_zero;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 TRANS err_self3;
 STR err_s3;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR Itersm2063802943;
 BOOL L4;
 BOOL L5;
 extern STR name21;
 extern STR Itersm2063802943;
 BOOL L6;
 BOOL L7;
 extern STR name21;
 extern STR Itersm1658204009;
 BOOL L8;
 BOOL L9;
 extern STR name21;
 extern STR Itersn1426326843;
 extern STR parandparloop;
 BOOL L10;
 BOOL L11;
 extern STR name21;
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(psather_self,distributed);
 }
 ret_val = L1;
 L2 = ret_val;
 L31_=!(L2); 
 if (L31_) {
  err_self = self;
  err_s = ((STR) &Itersm2063802943);
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
 }
 else {
  if ((ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL))) {
   err_self1 = self;
   err_s1 = ((STR) &Itersm2063802943);
   if ((err_self1==((TRANS) NULL))) {
    L7 = TRUE;
   } else {
    L7 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L7) {
    L6 = TRUE;
   } else {
    L6 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L6) {
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
  else {
   if (ATTR(ATTR(self,cur_rout),is_attach_routin)) {
    err_self2 = self;
    err_s2 = ((STR) &Itersm1658204009);
    if ((err_self2==((TRANS) NULL))) {
     L9 = TRUE;
    } else {
     L9 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L9) {
     L8 = TRUE;
    } else {
     L8 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
    }
    if (L8) {
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
   }
   else {
    err_self3 = self;
    plus_self6 = ((STR) &Itersn1426326843);
    plus_sarg6 = ((STR) &parandparloop);
    ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
    err_s3 = ret_val7;
    if ((err_self3==((TRANS) NULL))) {
     L11 = TRUE;
    } else {
     L11 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L11) {
     L10 = TRUE;
    } else {
     L10 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
    }
    if (L10) {
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    else {
     plus_self7 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
     plus_sarg7 = ((STR) &name21);
     ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
     plus_self8 = ret_val8;
     plus_sarg8 = err_s3;
     ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
     err_s3 = ret_val9;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_507974720(TRANS self, AS_CALL_EXPR as_call) {
 dAS_EXPR e = ((dAS_EXPR) NULL);
 AS_ARG_MODE m = ((AS_ARG_MODE) NULL);
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
 TRANS err_self7;
 STR err_s7;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val14;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val15;
 TRANS err_self8;
 STR err_s8;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val16;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val17;
 TRANS err_self9;
 STR err_s9;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val18;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val19;
 TRANS err_self10;
 STR err_s10;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val20;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val21;
 TRANS err_self11;
 STR err_s11;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val22;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val23;
 TRANS err_self12;
 STR err_s12;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val24;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val25;
 TRANS err_self13;
 STR err_s13;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val26;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val27;
 TRANS err_self14;
 STR err_s14;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val28;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val29;
 TRANS err_self15;
 STR err_s15;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val30;
 STR plus_self31;
 STR plus_sarg31;
 STR ret_val31;
 TRANS err_self16;
 STR err_s16;
 STR plus_self32;
 STR plus_sarg32;
 STR ret_val32;
 STR plus_self33;
 STR plus_sarg33;
 STR ret_val33;
 TRANS err_self17;
 STR err_s17;
 STR plus_self34;
 STR plus_sarg34;
 STR ret_val34;
 STR plus_self35;
 STR plus_sarg35;
 STR ret_val35;
 TRANS err_self18;
 STR err_s18;
 STR plus_self36;
 STR plus_sarg36;
 STR ret_val36;
 STR plus_self37;
 STR plus_sarg37;
 STR ret_val37;
 TRANS err_self19;
 STR err_s19;
 STR plus_self38;
 STR plus_sarg38;
 STR ret_val38;
 STR plus_self39;
 STR plus_sarg39;
 STR ret_val39;
 TRANS err_self20;
 STR err_s20;
 STR plus_self40;
 STR plus_sarg40;
 STR ret_val40;
 STR plus_self41;
 STR plus_sarg41;
 STR ret_val41;
 TRANS err_self21;
 STR err_s21;
 STR plus_self42;
 STR plus_sarg42;
 STR ret_val42;
 STR plus_self43;
 STR plus_sarg43;
 STR ret_val43;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 INT L5;
 BOOL L61_;
 INT L7;
 INT L8;
 BOOL L91_;
 extern STR Itisil162874430;
 BOOL L10;
 BOOL L11;
 extern STR name21;
 extern STR Itisil1456121986;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 extern STR Itisil483697597;
 BOOL L14;
 BOOL L15;
 extern STR name21;
 extern STR Itisil842621925;
 BOOL L16;
 BOOL L17;
 extern STR name21;
 extern STR Itisil159494944;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 extern STR Itisil781657708;
 BOOL L20;
 BOOL L22;
 extern STR name21;
 extern STR Itisil2145524732;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 extern STR Itisil419355605;
 BOOL L25;
 BOOL L26;
 extern STR name21;
 extern STR Itisil1672895968;
 BOOL L27;
 BOOL L28;
 extern STR name21;
 extern STR Itisil1936949134;
 BOOL L29;
 BOOL L30;
 extern STR name21;
 extern STR Itisil27243933;
 BOOL L31;
 BOOL L32;
 extern STR name21;
 extern STR Itisil100141197;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 extern STR Itisil1159445535;
 BOOL L35;
 BOOL L36;
 extern STR name21;
 extern STR Itisil1083364558;
 BOOL L37;
 BOOL L38;
 extern STR name21;
 extern STR Itisil1874953554;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 extern STR Itisil1213919924;
 BOOL L41;
 BOOL L42;
 extern STR name21;
 extern STR Itisil1114767838;
 BOOL L43;
 BOOL L44;
 extern STR name21;
 extern STR Itisil676454093;
 BOOL L45;
 BOOL L46;
 extern STR name21;
 extern STR Itisil205696450;
 BOOL L47;
 BOOL L48;
 extern STR name21;
 extern STR Itisil1998665541;
 BOOL L49;
 BOOL L50;
 extern STR name21;
 extern STR Itisil401642417;
 BOOL L51;
 BOOL L52;
 extern STR name21;
 BOOL L53;
 BOOL L54;
 BOOL L551_;
 BOOL L56;
 BOOL L571_;
 extern STR Itisil264579487;
 BOOL L58;
 BOOL L59;
 extern STR name21;
 dAS_EXPR L60;
 e = ATTR(as_call,args);
 m = ATTR(as_call,modes1);
 while (1) {
  L1 = (e==((dAS_EXPR) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  L4 = ATTR(m,mod);
  L5 = AS_ARG685418392;
  L61_=(L4)==(L5); 
  if (L61_) {
   L3 = TRUE;
  } else {
   L7 = ATTR(m,mod);
   L8 = AS_ARG1872943786;
   L91_=(L7)==(L8); 
   L3 = L91_;
  }
  if (L3) {
   if (e==NULL) {
    goto other398;
   } else
   switch (TAG(e)) {
    case AS_SELF_EXPR_tag:
     err_self = self;
     err_s = ((STR) &Itisil162874430);
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
      ret_val = STR_ap2004550586(plus_self, plus_sarg);
      plus_self1 = ret_val;
      plus_sarg1 = err_s;
      ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
      err_s = ret_val1;
      PROG_err_STR(ATTR(err_self,prog), err_s);
     } break;
    case AS_VOID_EXPR_tag:
     err_self1 = self;
     err_s1 = ((STR) &Itisil1456121986);
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
      plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
      plus_sarg2 = ((STR) &name21);
      ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
      plus_self3 = ret_val2;
      plus_sarg3 = err_s1;
      ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
      err_s1 = ret_val3;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     } break;
    case AS_IS_VOID_EXPR_tag:
     err_self2 = self;
     err_s2 = ((STR) &Itisil483697597);
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
      ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
      plus_self5 = ret_val4;
      plus_sarg5 = err_s2;
      ret_val5 = STR_ap2004550586(plus_self5, plus_sarg5);
      err_s2 = ret_val5;
      PROG_err_STR(ATTR(err_self2,prog), err_s2);
     } break;
    case AS_ARRAY_EXPR_tag:
     err_self3 = self;
     err_s3 = ((STR) &Itisil842621925);
     if ((err_self3==((TRANS) NULL))) {
      L17 = TRUE;
     } else {
      L17 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L17) {
      L16 = TRUE;
     } else {
      L16 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
     }
     if (L16) {
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
    case AS_CREATE_EXPR_tag:
     err_self4 = self;
     err_s4 = ((STR) &Itisil159494944);
     if ((err_self4==((TRANS) NULL))) {
      L19 = TRUE;
     } else {
      L19 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L19) {
      L18 = TRUE;
     } else {
      L18 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
     }
     if (L18) {
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
    case AS_BOU14269336_tag:
     err_self5 = self;
     err_s5 = ((STR) &Itisil781657708);
     if ((err_self5==((TRANS) NULL))) {
      L22 = TRUE;
     } else {
      L22 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L22) {
      L20 = TRUE;
     } else {
      L20 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
     }
     if (L20) {
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
    case AS_AND_EXPR_tag:
     err_self6 = self;
     err_s6 = ((STR) &Itisil2145524732);
     if ((err_self6==((TRANS) NULL))) {
      L24 = TRUE;
     } else {
      L24 = (ATTR(err_self6,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L24) {
      L23 = TRUE;
     } else {
      L23 = (ATTR(ATTR(err_self6,cur_rout),sig1)==((SIG) NULL));
     }
     if (L23) {
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
    case AS_OR_EXPR_tag:
     err_self7 = self;
     err_s7 = ((STR) &Itisil419355605);
     if ((err_self7==((TRANS) NULL))) {
      L26 = TRUE;
     } else {
      L26 = (ATTR(err_self7,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L26) {
      L25 = TRUE;
     } else {
      L25 = (ATTR(ATTR(err_self7,cur_rout),sig1)==((SIG) NULL));
     }
     if (L25) {
      PROG_err_STR(ATTR(err_self7,prog), err_s7);
     }
     else {
      plus_self14 = ATTR(ATTR(ATTR(err_self7,cur_rout),sig1),str);
      plus_sarg14 = ((STR) &name21);
      ret_val14 = STR_ap2004550586(plus_self14, plus_sarg14);
      plus_self15 = ret_val14;
      plus_sarg15 = err_s7;
      ret_val15 = STR_ap2004550586(plus_self15, plus_sarg15);
      err_s7 = ret_val15;
      PROG_err_STR(ATTR(err_self7,prog), err_s7);
     } break;
    case AS_EXCEPT_EXPR_tag:
     err_self8 = self;
     err_s8 = ((STR) &Itisil1672895968);
     if ((err_self8==((TRANS) NULL))) {
      L28 = TRUE;
     } else {
      L28 = (ATTR(err_self8,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L28) {
      L27 = TRUE;
     } else {
      L27 = (ATTR(ATTR(err_self8,cur_rout),sig1)==((SIG) NULL));
     }
     if (L27) {
      PROG_err_STR(ATTR(err_self8,prog), err_s8);
     }
     else {
      plus_self16 = ATTR(ATTR(ATTR(err_self8,cur_rout),sig1),str);
      plus_sarg16 = ((STR) &name21);
      ret_val16 = STR_ap2004550586(plus_self16, plus_sarg16);
      plus_self17 = ret_val16;
      plus_sarg17 = err_s8;
      ret_val17 = STR_ap2004550586(plus_self17, plus_sarg17);
      err_s8 = ret_val17;
      PROG_err_STR(ATTR(err_self8,prog), err_s8);
     } break;
    case AS_NEW_EXPR_tag:
     err_self9 = self;
     err_s9 = ((STR) &Itisil1936949134);
     if ((err_self9==((TRANS) NULL))) {
      L30 = TRUE;
     } else {
      L30 = (ATTR(err_self9,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L30) {
      L29 = TRUE;
     } else {
      L29 = (ATTR(ATTR(err_self9,cur_rout),sig1)==((SIG) NULL));
     }
     if (L29) {
      PROG_err_STR(ATTR(err_self9,prog), err_s9);
     }
     else {
      plus_self18 = ATTR(ATTR(ATTR(err_self9,cur_rout),sig1),str);
      plus_sarg18 = ((STR) &name21);
      ret_val18 = STR_ap2004550586(plus_self18, plus_sarg18);
      plus_self19 = ret_val18;
      plus_sarg19 = err_s9;
      ret_val19 = STR_ap2004550586(plus_self19, plus_sarg19);
      err_s9 = ret_val19;
      PROG_err_STR(ATTR(err_self9,prog), err_s9);
     } break;
    case AS_INITIAL_EXPR_tag:
     err_self10 = self;
     err_s10 = ((STR) &Itisil27243933);
     if ((err_self10==((TRANS) NULL))) {
      L32 = TRUE;
     } else {
      L32 = (ATTR(err_self10,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L32) {
      L31 = TRUE;
     } else {
      L31 = (ATTR(ATTR(err_self10,cur_rout),sig1)==((SIG) NULL));
     }
     if (L31) {
      PROG_err_STR(ATTR(err_self10,prog), err_s10);
     }
     else {
      plus_self20 = ATTR(ATTR(ATTR(err_self10,cur_rout),sig1),str);
      plus_sarg20 = ((STR) &name21);
      ret_val20 = STR_ap2004550586(plus_self20, plus_sarg20);
      plus_self21 = ret_val20;
      plus_sarg21 = err_s10;
      ret_val21 = STR_ap2004550586(plus_self21, plus_sarg21);
      err_s10 = ret_val21;
      PROG_err_STR(ATTR(err_self10,prog), err_s10);
     } break;
    case AS_BREAK_EXPR_tag:
     err_self11 = self;
     err_s11 = ((STR) &Itisil100141197);
     if ((err_self11==((TRANS) NULL))) {
      L34 = TRUE;
     } else {
      L34 = (ATTR(err_self11,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L34) {
      L33 = TRUE;
     } else {
      L33 = (ATTR(ATTR(err_self11,cur_rout),sig1)==((SIG) NULL));
     }
     if (L33) {
      PROG_err_STR(ATTR(err_self11,prog), err_s11);
     }
     else {
      plus_self22 = ATTR(ATTR(ATTR(err_self11,cur_rout),sig1),str);
      plus_sarg22 = ((STR) &name21);
      ret_val22 = STR_ap2004550586(plus_self22, plus_sarg22);
      plus_self23 = ret_val22;
      plus_sarg23 = err_s11;
      ret_val23 = STR_ap2004550586(plus_self23, plus_sarg23);
      err_s11 = ret_val23;
      PROG_err_STR(ATTR(err_self11,prog), err_s11);
     } break;
    case AS_RESULT_EXPR_tag:
     err_self12 = self;
     err_s12 = ((STR) &Itisil1159445535);
     if ((err_self12==((TRANS) NULL))) {
      L36 = TRUE;
     } else {
      L36 = (ATTR(err_self12,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L36) {
      L35 = TRUE;
     } else {
      L35 = (ATTR(ATTR(err_self12,cur_rout),sig1)==((SIG) NULL));
     }
     if (L35) {
      PROG_err_STR(ATTR(err_self12,prog), err_s12);
     }
     else {
      plus_self24 = ATTR(ATTR(ATTR(err_self12,cur_rout),sig1),str);
      plus_sarg24 = ((STR) &name21);
      ret_val24 = STR_ap2004550586(plus_self24, plus_sarg24);
      plus_self25 = ret_val24;
      plus_sarg25 = err_s12;
      ret_val25 = STR_ap2004550586(plus_self25, plus_sarg25);
      err_s12 = ret_val25;
      PROG_err_STR(ATTR(err_self12,prog), err_s12);
     } break;
    case AS_BOOL_LIT_EXPR_tag:
     err_self13 = self;
     err_s13 = ((STR) &Itisil1083364558);
     if ((err_self13==((TRANS) NULL))) {
      L38 = TRUE;
     } else {
      L38 = (ATTR(err_self13,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L38) {
      L37 = TRUE;
     } else {
      L37 = (ATTR(ATTR(err_self13,cur_rout),sig1)==((SIG) NULL));
     }
     if (L37) {
      PROG_err_STR(ATTR(err_self13,prog), err_s13);
     }
     else {
      plus_self26 = ATTR(ATTR(ATTR(err_self13,cur_rout),sig1),str);
      plus_sarg26 = ((STR) &name21);
      ret_val26 = STR_ap2004550586(plus_self26, plus_sarg26);
      plus_self27 = ret_val26;
      plus_sarg27 = err_s13;
      ret_val27 = STR_ap2004550586(plus_self27, plus_sarg27);
      err_s13 = ret_val27;
      PROG_err_STR(ATTR(err_self13,prog), err_s13);
     } break;
    case AS_CHAR_LIT_EXPR_tag:
     err_self14 = self;
     err_s14 = ((STR) &Itisil1874953554);
     if ((err_self14==((TRANS) NULL))) {
      L40 = TRUE;
     } else {
      L40 = (ATTR(err_self14,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L40) {
      L39 = TRUE;
     } else {
      L39 = (ATTR(ATTR(err_self14,cur_rout),sig1)==((SIG) NULL));
     }
     if (L39) {
      PROG_err_STR(ATTR(err_self14,prog), err_s14);
     }
     else {
      plus_self28 = ATTR(ATTR(ATTR(err_self14,cur_rout),sig1),str);
      plus_sarg28 = ((STR) &name21);
      ret_val28 = STR_ap2004550586(plus_self28, plus_sarg28);
      plus_self29 = ret_val28;
      plus_sarg29 = err_s14;
      ret_val29 = STR_ap2004550586(plus_self29, plus_sarg29);
      err_s14 = ret_val29;
      PROG_err_STR(ATTR(err_self14,prog), err_s14);
     } break;
    case AS_STR_LIT_EXPR_tag:
     err_self15 = self;
     err_s15 = ((STR) &Itisil1213919924);
     if ((err_self15==((TRANS) NULL))) {
      L42 = TRUE;
     } else {
      L42 = (ATTR(err_self15,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L42) {
      L41 = TRUE;
     } else {
      L41 = (ATTR(ATTR(err_self15,cur_rout),sig1)==((SIG) NULL));
     }
     if (L41) {
      PROG_err_STR(ATTR(err_self15,prog), err_s15);
     }
     else {
      plus_self30 = ATTR(ATTR(ATTR(err_self15,cur_rout),sig1),str);
      plus_sarg30 = ((STR) &name21);
      ret_val30 = STR_ap2004550586(plus_self30, plus_sarg30);
      plus_self31 = ret_val30;
      plus_sarg31 = err_s15;
      ret_val31 = STR_ap2004550586(plus_self31, plus_sarg31);
      err_s15 = ret_val31;
      PROG_err_STR(ATTR(err_self15,prog), err_s15);
     } break;
    case AS_INT_LIT_EXPR_tag:
     err_self16 = self;
     err_s16 = ((STR) &Itisil1114767838);
     if ((err_self16==((TRANS) NULL))) {
      L44 = TRUE;
     } else {
      L44 = (ATTR(err_self16,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L44) {
      L43 = TRUE;
     } else {
      L43 = (ATTR(ATTR(err_self16,cur_rout),sig1)==((SIG) NULL));
     }
     if (L43) {
      PROG_err_STR(ATTR(err_self16,prog), err_s16);
     }
     else {
      plus_self32 = ATTR(ATTR(ATTR(err_self16,cur_rout),sig1),str);
      plus_sarg32 = ((STR) &name21);
      ret_val32 = STR_ap2004550586(plus_self32, plus_sarg32);
      plus_self33 = ret_val32;
      plus_sarg33 = err_s16;
      ret_val33 = STR_ap2004550586(plus_self33, plus_sarg33);
      err_s16 = ret_val33;
      PROG_err_STR(ATTR(err_self16,prog), err_s16);
     } break;
    case AS_FLT_LIT_EXPR_tag:
     err_self17 = self;
     err_s17 = ((STR) &Itisil676454093);
     if ((err_self17==((TRANS) NULL))) {
      L46 = TRUE;
     } else {
      L46 = (ATTR(err_self17,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L46) {
      L45 = TRUE;
     } else {
      L45 = (ATTR(ATTR(err_self17,cur_rout),sig1)==((SIG) NULL));
     }
     if (L45) {
      PROG_err_STR(ATTR(err_self17,prog), err_s17);
     }
     else {
      plus_self34 = ATTR(ATTR(ATTR(err_self17,cur_rout),sig1),str);
      plus_sarg34 = ((STR) &name21);
      ret_val34 = STR_ap2004550586(plus_self34, plus_sarg34);
      plus_self35 = ret_val34;
      plus_sarg35 = err_s17;
      ret_val35 = STR_ap2004550586(plus_self35, plus_sarg35);
      err_s17 = ret_val35;
      PROG_err_STR(ATTR(err_self17,prog), err_s17);
     } break;
    case AS_CLUSTER_EXPR_tag:
     err_self18 = self;
     err_s18 = ((STR) &Itisil205696450);
     if ((err_self18==((TRANS) NULL))) {
      L48 = TRUE;
     } else {
      L48 = (ATTR(err_self18,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L48) {
      L47 = TRUE;
     } else {
      L47 = (ATTR(ATTR(err_self18,cur_rout),sig1)==((SIG) NULL));
     }
     if (L47) {
      PROG_err_STR(ATTR(err_self18,prog), err_s18);
     }
     else {
      plus_self36 = ATTR(ATTR(ATTR(err_self18,cur_rout),sig1),str);
      plus_sarg36 = ((STR) &name21);
      ret_val36 = STR_ap2004550586(plus_self36, plus_sarg36);
      plus_self37 = ret_val36;
      plus_sarg37 = err_s18;
      ret_val37 = STR_ap2004550586(plus_self37, plus_sarg37);
      err_s18 = ret_val37;
      PROG_err_STR(ATTR(err_self18,prog), err_s18);
     } break;
    case AS_CLU553570599_tag:
     err_self19 = self;
     err_s19 = ((STR) &Itisil1998665541);
     if ((err_self19==((TRANS) NULL))) {
      L50 = TRUE;
     } else {
      L50 = (ATTR(err_self19,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L50) {
      L49 = TRUE;
     } else {
      L49 = (ATTR(ATTR(err_self19,cur_rout),sig1)==((SIG) NULL));
     }
     if (L49) {
      PROG_err_STR(ATTR(err_self19,prog), err_s19);
     }
     else {
      plus_self38 = ATTR(ATTR(ATTR(err_self19,cur_rout),sig1),str);
      plus_sarg38 = ((STR) &name21);
      ret_val38 = STR_ap2004550586(plus_self38, plus_sarg38);
      plus_self39 = ret_val38;
      plus_sarg39 = err_s19;
      ret_val39 = STR_ap2004550586(plus_self39, plus_sarg39);
      err_s19 = ret_val39;
      PROG_err_STR(ATTR(err_self19,prog), err_s19);
     } break;
    case AS_GLOBAL_EXPR_tag:
     err_self20 = self;
     err_s20 = ((STR) &Itisil401642417);
     if ((err_self20==((TRANS) NULL))) {
      L52 = TRUE;
     } else {
      L52 = (ATTR(err_self20,cur_rout)==((AM_ROUT_DEF) NULL));
     }
     if (L52) {
      L51 = TRUE;
     } else {
      L51 = (ATTR(ATTR(err_self20,cur_rout),sig1)==((SIG) NULL));
     }
     if (L51) {
      PROG_err_STR(ATTR(err_self20,prog), err_s20);
     }
     else {
      plus_self40 = ATTR(ATTR(ATTR(err_self20,cur_rout),sig1),str);
      plus_sarg40 = ((STR) &name21);
      ret_val40 = STR_ap2004550586(plus_self40, plus_sarg40);
      plus_self41 = ret_val40;
      plus_sarg41 = err_s20;
      ret_val41 = STR_ap2004550586(plus_self41, plus_sarg41);
      err_s20 = ret_val41;
      PROG_err_STR(ATTR(err_self20,prog), err_s20);
     } break;
    case AS_CALL_EXPR_tag:
     L54 = ATTR(((AS_CALL_EXPR) e),is_array);
     L551_=L54==FALSE; 
     if (L551_) {
      L56 = (ATTR(((AS_CALL_EXPR) e),args)==((dAS_EXPR) NULL));
      L571_=!(L56); 
      L53 = L571_;
     } else {
      L53 = FALSE;
     }
     if (L53) {
      err_self21 = self;
      err_s21 = ((STR) &Itisil264579487);
      if ((err_self21==((TRANS) NULL))) {
       L59 = TRUE;
      } else {
       L59 = (ATTR(err_self21,cur_rout)==((AM_ROUT_DEF) NULL));
      }
      if (L59) {
       L58 = TRUE;
      } else {
       L58 = (ATTR(ATTR(err_self21,cur_rout),sig1)==((SIG) NULL));
      }
      if (L58) {
       PROG_err_STR(ATTR(err_self21,prog), err_s21);
      }
      else {
       plus_self42 = ATTR(ATTR(ATTR(err_self21,cur_rout),sig1),str);
       plus_sarg42 = ((STR) &name21);
       ret_val42 = STR_ap2004550586(plus_self42, plus_sarg42);
       plus_self43 = ret_val42;
       plus_sarg43 = err_s21;
       ret_val43 = STR_ap2004550586(plus_self43, plus_sarg43);
       err_s21 = ret_val43;
       PROG_err_STR(ATTR(err_self21,prog), err_s21);
      }
     } break;
    default: ;
    other398: ;
   }
  }
  L60 = e;
  e = (*dAS_EX337431651[TAG(L60)])(L60);
  m = ATTR(m,next);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_626807411(TRANS self, AM_LOCAL_EXPR l) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 extern STR Compil1357616012;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 if ((ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Compil1357616012);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 SATTR(ATTR(self,cur_rout),locals1,FLISTA1817671564(ATTR(ATTR(self,cur_rout),locals1), l));
 L4 = (ATTR(l,name1).str==(STR)0);
 L51_=!(L4); 
 if (L51_) {
  SATTR(self,active_locals,FLISTA1817671564(ATTR(self,active_locals), l));
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_74748323(TRANS self, AS_TYPE_SPEC t) {
 AS_TYPE_SPEC param1;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val;
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
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L6;
 dTP L7;
 INT L8;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR Partia838698083;
 BOOL L15;
 BOOL L16;
 extern STR name21;
 L1 = (t==((AS_TYPE_SPEC) NULL));
 L21_=!(L1); 
 if (L21_) {
  param1 = ATTR(t,params);
  while (1) {
   L3 = (param1==((AS_TYPE_SPEC) NULL));
   L41_=!(L3); 
   if (L41_) {
   }
   else {
    goto after_loop;
   }
   tp_of_self = self;
   tp_of_t = param1;
   ret_val = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
   L7 = ret_val;
   if ((*dTP_is1553335918[TAG(L7)])(L7)) {
    L8 = ATTR(param1,kind);
    L9 = AS_TYP1853679690;
    L101_=(L8)==(L9); 
    L11 = L101_;
    L121_=!(L11); 
    L6 = L121_;
   } else {
    L6 = FALSE;
   }
   if (L6) {
    L13 = ATTR(ATTR(self,prog),is_check_am);
    L141_=!(L13); 
    L5 = L141_;
   } else {
    L5 = FALSE;
   }
   if (L5) {
    err_loc_self = self;
    err_loc_t = ((dPROG_ERR) t);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = self;
    err_s = ((STR) &Partia838698083);
    if ((err_self==((TRANS) NULL))) {
     L16 = TRUE;
    } else {
     L16 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L16) {
     L15 = TRUE;
    } else {
     L15 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
    }
    if (L15) {
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
   param1 = ATTR(param1,next);
  }
  after_loop: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_81467988(TRANS self, SIG sig1) {
 TRANS has_import_self;
 SIG has_import_s;
 BOOL ret_val = BOOL_zero;
 AM_ROUT_DEF needs_import_loc;
 SIG is_forked_self;
 BOOL ret_val1 = BOOL_zero;
 AM_ROUT_DEF needs_import_cal;
 TRANS has_export_self;
 SIG has_export_s;
 BOOL ret_val2 = BOOL_zero;
 AM_ROUT_DEF needs_export_loc;
 SIG is_forked_self1;
 BOOL ret_val3 = BOOL_zero;
 AM_ROUT_DEF needs_export_cal;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L17;
 BOOL L18;
 BOOL L19;
 BOOL L201_;
 BOOL L21;
 BOOL L221_;
 L3 = (ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = (sig1==((SIG) NULL));
  L61_=!(L5); 
  L2 = L61_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L7 = (ATTR(sig1,tp)==((dTP) NULL));
  L81_=!(L7); 
  L1 = L81_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  has_import_self = self;
  has_import_s = sig1;
  if (SIG_is418491101(has_import_s)) {
   L9 = ATTR(ATTR(has_import_s,builtin_info),does_import);
  } else {
   L9 = FALSE;
  }
  ret_val = L9;
  if (ret_val) {
   needs_import_loc = ATTR(self,cur_rout);
   is_forked_self = ATTR(needs_import_loc,sig1);
   if (ATTR(is_forked_self,is_par_routine)) {
    L11 = TRUE;
   } else {
    L11 = ATTR(is_forked_self,is_fork_routine);
   }
   if (L11) {
    L10 = TRUE;
   } else {
    L10 = ATTR(is_forked_self,is_attach_routin);
   }
   ret_val1 = L10;
   L12 = ret_val1;
   L131_=!(L12); 
   SATTR(ATTR(needs_import_loc,sig1),needs_import,L131_);
   SATTR(needs_import_loc,n_import_locals,TRUE);
   L14 = TRANS_292955251(self, sig1);
   L151_=!(L14); 
   if (L151_) {
    needs_import_cal = ATTR(self,cur_rout);
    SATTR(needs_import_cal,n_import_call,TRUE);
   }
  }
  has_export_self = self;
  has_export_s = sig1;
  if (SIG_is418491101(has_export_s)) {
   L16 = ATTR(ATTR(has_export_s,builtin_info),does_export);
  } else {
   L16 = FALSE;
  }
  ret_val2 = L16;
  if (ret_val2) {
   needs_export_loc = ATTR(self,cur_rout);
   is_forked_self1 = ATTR(needs_export_loc,sig1);
   if (ATTR(is_forked_self1,is_par_routine)) {
    L18 = TRUE;
   } else {
    L18 = ATTR(is_forked_self1,is_fork_routine);
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = ATTR(is_forked_self1,is_attach_routin);
   }
   ret_val3 = L17;
   L19 = ret_val3;
   L201_=!(L19); 
   SATTR(ATTR(needs_export_loc,sig1),needs_export,L201_);
   SATTR(needs_export_loc,n_export_locals,TRUE);
   L21 = TRANS_792319540(self, sig1);
   L221_=!(L21); 
   if (L221_) {
    needs_export_cal = ATTR(self,cur_rout);
    SATTR(needs_export_cal,n_export_call,TRUE);
   }
  }
 }
}
