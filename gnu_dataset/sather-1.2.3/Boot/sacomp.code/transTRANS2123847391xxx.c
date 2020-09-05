#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

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

typedef struct dAS_NODE_struct {
 OB_HEADER header;
 } *dAS_NODE;

typedef struct dAS_STMT_struct {
 OB_HEADER header;
 } *dAS_STMT;

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

typedef struct AS_STMT_CURSOR_struct {/* layout for AS_STMT_CURSOR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_STMT_CURSOR;

typedef struct AS_STM1504735434_frame_struct {
 INT state;
 AS_STMT_CURSOR self;/* Formal argument: self */
 dAS_NODE arg1;/* Formal argument: cur */
 dAS_STMT ret_val;
 dAS_NODE as;
 AS_STMT_CURSOR L21;
 AS_STMT_CURSOR L41;
 AS_STMT_CURSOR L61;
 AS_STMT_CURSOR L81;
 AS_STMT_CURSOR L101;
 AS_STMT_CURSOR L121;
 AS_STMT_CURSOR L141;
 AS_STMT_CURSOR L161;
 AS_STMT_CURSOR L181;
 AS_STMT_CURSOR L201;
 AS_STMT_CURSOR L231;
 AS_STMT_CURSOR L251;
 AS_STMT_CURSOR L271;
 AS_STMT_CURSOR L291;
 AS_STMT_CURSOR L311;
 AS_STMT_CURSOR L331;
 AS_STMT_CURSOR L351;
 AS_STMT_CURSOR L371;
 AS_STMT_CURSOR L391;
 AS_STMT_CURSOR L421;
 AS_STMT_CURSOR L441;
 AS_STMT_CURSOR L461;
 AS_STMT_CURSOR L481;
 AS_STMT_CURSOR L501;
 AS_STMT_CURSOR L521;
 AS_STMT_CURSOR L541;
 AS_STMT_CURSOR L561;
 struct AS_STM1504735434_frame_struct *nested1; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested2; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested3; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested4; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested5; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested6; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested7; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested8; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested9; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested10; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested11; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested12; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested13; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested14; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested15; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested16; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested17; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested18; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested19; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested20; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested21; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested22; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested23; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested24; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested25; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested26; /* nested iter frame */
 struct AS_STM1504735434_frame_struct *nested27; /* nested iter frame */
 } *AS_STM1504735434_frame;

typedef struct CALL_ARG_struct {/* layout for CALL_ARG */
 OB_HEADER header;
 struct dCALL_TP_struct *tp;
 struct dMODE_struct *mode1;
 } *CALL_ARG;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct PROG_AS_TBL_struct {/* layout for PROG_AS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct AS_CLASS_DEF_struct *arr_part[1];
 } *PROG_AS_TBL;

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

typedef struct TP_GRAPH_struct {/* layout for TP_GRAPH */
 OB_HEADER header;
 struct FMAPTP291739594_struct *child_tbl;
 struct FMAPTP291739594_struct *des_tbl;
 struct FMAPTP775297600_struct *anc_tbl;
 struct FMAPTP775297600_struct *par_tbl;
 struct FSETTUPIDENTINT_struct *cur;
 struct PROG_struct *prog;
 } *TP_GRAPH;

typedef struct TP_GRAPH_ABS_DES_struct {/* layout for TP_GRAPH_ABS_DES */
 OB_HEADER header;
 struct FMAPTP291739594_struct *tbl;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *dollar_lock;
 struct TP_CLASS_struct *gate;
 struct TP_CLASS_struct *mutex;
 } *TP_GRAPH_ABS_DES;

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

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

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

typedef struct AM_AT_EXPR_struct {/* layout for AM_AT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *e;
 struct SFILE_ID_struct source1;
 } *AM_AT_EXPR;

typedef struct AM_BREAK_STMT_struct {/* layout for AM_BREAK_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_BREAK_STMT;

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

typedef struct AM_INV1167837230_struct {/* layout for AM_INVARIANT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 } *AM_INV1167837230;

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

typedef struct AM_POST_STMT_struct {/* layout for AM_POST_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_POST_STMT;

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

typedef struct AM_ROU1235468764_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1235468764_frame;

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

typedef struct AM_YIELD_STMT_struct {/* layout for AM_YIELD_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 INT ret;
 struct SFILE_ID_struct source1;
 } *AM_YIELD_STMT;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

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

typedef struct AS_LOOP_STMT_struct {/* layout for AS_LOOP_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_LOOP_STMT;

typedef struct AS_NEW_EXPR_struct {/* layout for AS_NEW_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *arg;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEW_EXPR;

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

typedef struct AS_WHERE_EXPR_struct {/* layout for AS_WHERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_WHERE_EXPR;

typedef struct AS_YIELD_STMT_struct {/* layout for AS_YIELD_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_YIELD_STMT;

typedef struct CALL_SIG_struct {/* layout for CALL_SIG */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ARRAYCALL_ARG_struct *args;
 struct IDENT_struct name1;
 BOOL has_ret;
 BOOL unknown_ret;
 } *CALL_SIG;

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

typedef struct FLISTdAM_CONST_struct {/* layout for FLIST{$AM_CONST} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *FLISTdAM_CONST;

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

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

typedef struct FLISTA1062334999_struct {/* layout for FLIST{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *FLISTA1062334999;

typedef struct FLISTA20621028_frame_struct {
 INT state;
 FLISTA1062334999 self;/* Formal argument: self */
 AM_ITE1809135850 ret_val1;
 INT i;
 INT sz;
 FLISTA1062334999 aget_self;
 INT aget_ind;
 AM_ITE1809135850 ret_val;
 } *FLISTA20621028_frame;

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

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FMAPTP291739594_struct {/* layout for FMAP{TP_CLASS,FSET{$TP}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FMAPTP291739594;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETSFILE_ID_struct {/* layout for FSET{SFILE_ID} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FSETSFILE_ID;

typedef struct FSETTP_CLASS_struct {/* layout for FSET{TP_CLASS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FSETTP_CLASS;

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
extern IDENT IDENT_1728630987;
extern INT AS_ARG1300937332;
extern INT AS_CLASS_DEF_ref;
extern INT AS_TYP1853679690;
extern INT TRANS_1924362320;
extern INT TRANS_194341863;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_zone;

/* Function declarations */


extern RETURNED_CONST AS_TYPE_SPEC (**dTP_as1220129267)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST SFILE_ID (**dAS_ST957383256)(dAS_STMT);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dINLIN970826022)(dINLINE, AM_ROU1916046290);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_ST1454156204)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dAS_ST1569642444)(dAS_STMT, BOOL);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);
AM_FORMAL_ARG AM_ROU1235468764(AM_ROU1235468764_frame);
AM_INV1167837230 AM_INV1353049642(AM_INV1167837230);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_LOCAL_EXPR TRANS_1349638895(TRANS_1349638895_frame);
AM_LOCAL_EXPR TRANS_1496987093(TRANS, dAS_STMT, IDENT, IDENT, AS_CLASS_DEF, ARRAYdTP);
AM_LOCAL_EXPR TRANS_1711874080(TRANS, IDENT);
AM_LOCAL_EXPR TRANS_2133809761(TRANS);
AM_LOCAL_EXPR TRANS_647716465(TRANS);
AM_POST_STMT AM_POS1078136755(AM_POST_STMT);
AS_ASSIGN_STMT TRANS_1390141743(TRANS, IDENT, IDENT, AS_CLASS_DEF, AS_STMT_LIST, SFILE_ID);
AS_ASSIGN_STMT TRANS_2064676398(TRANS, IDENT, AS_STMT_LIST, SFILE_ID);
AS_CALL_EXPR TRANS_1849744987(TRANS, IDENT, AM_LOCAL_EXPR, SFILE_ID);
AS_ROUT_DEF TRANS_725662504(TRANS, IDENT, IDENT, IDENT, AM_LOCAL_EXPR, AS_CLASS_DEF, TP_CLASS, IDENT, AS_TYPE_SPEC, AS_STMT_LIST, INT, SFILE_ID);
BOOL AM_ROU389945790(AM_ROUT_DEF, AM_LOCAL_EXPR);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL FSETSF406940155(FSETSFILE_ID, SFILE_ID);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL STR_is111530248(STR, STR);
BOOL TRANS_1229338887(TRANS);
BOOL TRANS_157729662(TRANS, IDENT, AM_LOCAL_EXPR);
BOOL TRANS_1615967248(TRANS, AS_FORK_STMT);
BOOL TRANS_1864518471(TRANS, dAS_EXPR);
BOOL TRANS_2091440962(TRANS);
BOOL TRANS_883636795(TRANS, AS_PAR_STMT);
BOOL TRANS_900561657(TRANS);
ELT ELT_TB954957059(ELT_TBL, ELT);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
ELT_TBL ELT_TB1703146017(ELT_TBL, ELT);
FLISTA725283029 FLISTA811588375(FLISTA725283029, FLISTA725283029);
FLISTF7594819 FLISTF1203220157(FLISTF7594819, FLISTdAM_CONST);
FLISTdAM_CONST FLISTF471519751(FLISTF7594819);
FLISTdAM_CONST FLISTd246699990(FLISTdAM_CONST, dAM_CONST);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FLISTdTP FLISTd1953924847(FLISTdTP, dTP);
FMAPTP291739594 FMAPTP551118428(FMAPTP291739594, TP_CLASS, FSETdTP);
FSETSFILE_ID FSETSF663191272(FSETSFILE_ID, SFILE_ID);
FSETTP_CLASS TP_GRA583006971(TP_GRAPH, TP_CLASS);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSETdTP TP_GRA2060761681(TP_GRAPH, TP_CLASS);
IDENT IDENT_1150413730(IDENT, STR);
IMPL TP_CLA1109727233(TP_CLASS);
INT AS_CAL1853175953(AS_CALL_EXPR);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTA1962912942(FLISTA1062334999);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
PROG_AS_TBL PROG_A2047269051(PROG_AS_TBL, AS_CLASS_DEF);
SIG CALL_S2120587481(CALL_SIG, BOOL);
SIG SIG_ro1276623596(SIG, AS_ROUT_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
STR STR_ap2004550586(STR, STR);
STR STR_head_INTrSTR(STR, INT);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
dAM_CONST TRANS_1139222136(TRANS, dAM_EXPR);
dAM_EXPR TRANS_1705639890(TRANS, AS_CALL_EXPR, dTP, BOOL);
dAM_EXPR TRANS_693526804(TRANS, dAS_EXPR, dTP);
dAM_STMT TRANS_1066292461(TRANS, AS_YIELD_STMT);
dAM_STMT TRANS_1157876950(TRANS, AS_ASSIGN_STMT);
dAM_STMT TRANS_1207936294(TRANS, AS_CALL_EXPR, AS_ASSIGN_STMT);
dAM_STMT TRANS_1226584966(TRANS, AS_ASSERT_STMT);
dAM_STMT TRANS_1261916410(TRANS, AS_IF_STMT);
dAM_STMT TRANS_1563748848(TRANS, AS_STMT_LIST);
dAM_STMT TRANS_1630043848(TRANS, AS_CASE_STMT, AS_CASE_WHEN, AM_LOCAL_EXPR);
dAM_STMT TRANS_1762825757(TRANS, AS_RAISE_STMT);
dAM_STMT TRANS_1847483442(TRANS, AS_RETURN_STMT);
dAM_STMT TRANS_1853773828(TRANS, AS_QUIT_STMT);
dAM_STMT TRANS_1920518219(TRANS, AS_ASSIGN_STMT);
dAM_STMT TRANS_1999552456(TRANS, AS_CASE_STMT);
dAM_STMT TRANS_471579016(TRANS, AS_CALL_EXPR, AS_ASSIGN_STMT);
dAM_STMT TRANS_565673800(TRANS, AS_LOOP_STMT);
dAM_STMT TRANS_741871011(TRANS, AS_PROTECT_STMT);
dAM_STMT TRANS_774746956(TRANS, AS_TYPECASE_STMT);
dAM_STMT TRANS_905208628(TRANS, AS_EXPR_STMT);
dAS_STMT AS_STM1504735434(AS_STM1504735434_frame);
dAS_STMT TRANS_120708644(TRANS, INT, AS_STMT_LIST, SFILE_ID);
dAS_STMT TRANS_166911016(TRANS, INT, AS_STMT_LIST, SFILE_ID);
dCALL_TP TRANS_68137339(TRANS, dAS_EXPR);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void AS_ARG2059479532(AS_ARG_DEC, AS_ARG_DEC);
void AS_PAR769375844(AS_PARAM_DEC, AS_PARAM_DEC);
void AS_TYP1027254244(AS_TYPE_SPEC, AS_TYPE_SPEC);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);
void SE_CON412941612(SE_CONTEXT, dAM);
void TRANS_1069298803(TRANS, TP_CLASS, AS_ROUT_DEF);
void TRANS_548661555(TRANS, AS_ASSIGN_STMT);
void TRANS_626807411(TRANS, AM_LOCAL_EXPR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR TRANS_1496987093(TRANS self, dAS_STMT s, IDENT newid_ob, IDENT newid_cl, AS_CLASS_DEF orig_class_as, ARRAYdTP orig_cl_params) {
 AM_LOCAL_EXPR ret_val;
 AS_CLASS_DEF as;
 AS_PARAM_DEC class_param;
 AS_PARAM_DEC pardec;
 TP_CLASS tp;
 TRANS L11;
 AM_LOCAL_EXPR loc;
 AS_ATTR_DEF var;
 AS_TYPE_SPEC tps = ((AS_TYPE_SPEC) NULL);
 AS_ATTR_DEF var11;
 AS_TYPE_SPEC tps1;
 AS_ATTR_DEF var21;
 AS_TYPE_SPEC tps2 = ((AS_TYPE_SPEC) NULL);
 ELT_TBL elts1;
 ELT_TBL L21;
 ELT elt1;
 AS_ATTR_DEF var3;
 AS_TYPE_SPEC tps3 = ((AS_TYPE_SPEC) NULL);
 AS_ROUT_DEF crt;
 AS_TYPE_SPEC ret_dec;
 AS_RETURN_STMT crtbdy;
 AS_NEW_EXPR crtnew;
 IMPL impl;
 TP_CLASS dob;
 FSETTP_CLASS ip1;
 FSETdTP ip2;
 AS_CLASS_DEF create_self;
 AS_CLASS_DEF ret_val1;
 AS_PARAM_DEC create_self1;
 AS_PARAM_DEC ret_val2;
 TRANS add_helper_to_tb;
 IDENT add_helper_to_tb1 = IDENT_zero;
 ARRAYdTP add_helper_to_tb2;
 TP_CLASS ret_val3;
 TP_CLASS tp1;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val4;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 AS_ATTR_DEF create_self2;
 AS_ATTR_DEF ret_val7;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val8;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val9;
 TRANS err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val10;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val11;
 AS_ATTR_DEF create_self3;
 AS_ATTR_DEF ret_val12;
 AS_TYPE_SPEC create_self4;
 AS_TYPE_SPEC ret_val13;
 AS_ATTR_DEF create_self5;
 AS_ATTR_DEF ret_val14;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val15;
 AM_LOCAL_EXPR tp_self3;
 dTP ret_val16;
 ELT name_self;
 IDENT ret_val17 = IDENT_zero;
 ELT ret_self;
 dTP ret_val18;
 ELT name_self1;
 IDENT ret_val19 = IDENT_zero;
 ELT name_self2;
 IDENT ret_val20 = IDENT_zero;
 AS_ATTR_DEF create_self6;
 AS_ATTR_DEF ret_val21;
 ELT name_self3;
 IDENT ret_val22 = IDENT_zero;
 ELT as_tp_self;
 AS_TYPE_SPEC ret_val23;
 dAS_CLASS_ELT clelt;
 AS_TYPE_SPEC res = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self7;
 AS_TYPE_SPEC ret_val24;
 ELT as_tp_self1;
 AS_TYPE_SPEC ret_val25;
 dAS_CLASS_ELT clelt1;
 AS_TYPE_SPEC res1 = ((AS_TYPE_SPEC) NULL);
 AS_TYPE_SPEC create_self8;
 AS_TYPE_SPEC ret_val26;
 ELT ret_self1;
 dTP ret_val27;
 AS_ROUT_DEF create_self9;
 AS_ROUT_DEF ret_val28;
 AS_TYPE_SPEC create_self10;
 AS_TYPE_SPEC ret_val29;
 AS_RETURN_STMT create_self11;
 AS_RETURN_STMT ret_val30;
 AS_NEW_EXPR create_self12;
 AS_NEW_EXPR ret_val31;
 AS_STMT_LIST create_self13;
 AS_STMT_LIST ret_val32;
 TP_GRAPH_ABS_DES add_self;
 TP_CLASS add_at;
 dTP add_ct;
 FSETdTP s1;
 AM_LOCAL_EXPR create_self14;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val33;
 AM_LOCAL_EXPR r;
 AS_CLASS_DEF L3;
 OB L4;
 dAS_STMT L5;
 AS_PARAM_DEC L6;
 OB L7;
 dAS_STMT L8;
 extern STR Compil163952783;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 BOOL L12;
 BOOL L131_;
 AM_LOCAL_EXPR L14;
 BOOL L15;
 STR L16;
 extern STR S_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L20;
 BOOL L221_;
 AS_ATTR_DEF L23;
 OB L24;
 dAS_STMT L25;
 BOOL L26;
 BOOL L271_;
 INT L28;
 INT L29;
 BOOL L301_;
 BOOL L31;
 BOOL L321_;
 dTP L33;
 dTP L34;
 dAS_CLASS_ELT L35;
 BOOL L36;
 STR L37;
 extern STR S_pS_att;
 extern STR Compil1359403594;
 BOOL L38;
 BOOL L39;
 extern STR name21;
 AS_ATTR_DEF L40;
 OB L41;
 dAS_STMT L42;
 AS_TYPE_SPEC L43;
 OB L44;
 dAS_STMT L45;
 extern STR PAR_ATTACH;
 dAS_CLASS_ELT L46;
 BOOL L47;
 BOOL L481_;
 AS_ATTR_DEF L49;
 OB L50;
 dAS_STMT L51;
 BOOL L52;
 BOOL L531_;
 dTP L54;
 dAS_CLASS_ELT L55;
 BOOL L56;
 BOOL L571_;
 dTP L58;
 IMPL L59;
 ELT L60;
 IDENT L61;
 STR L62;
 extern STR S_;
 BOOL L63;
 BOOL L641_;
 BOOL L65;
 IDENT L66;
 INT L67;
 BOOL L681_;
 BOOL L69;
 BOOL L701_;
 IDENT L71;
 STR L72;
 extern STR S_pS_par_;
 AS_ATTR_DEF L73;
 OB L74;
 dAS_STMT L75;
 BOOL L76;
 BOOL L771_;
 AS_TYPE_SPEC L78;
 OB L79;
 extern STR INT1;
 BOOL L80;
 BOOL L811_;
 BOOL L82;
 BOOL L831_;
 AS_TYPE_SPEC L84;
 OB L85;
 extern STR INT1;
 dTP L86;
 dAS_CLASS_ELT L87;
 AS_ROUT_DEF L88;
 OB L89;
 dAS_STMT L90;
 extern STR create1;
 AS_TYPE_SPEC L91;
 OB L92;
 dAS_STMT L93;
 AS_RETURN_STMT L94;
 OB L95;
 dAS_STMT L96;
 AS_NEW_EXPR L97;
 OB L98;
 dAS_STMT L99;
 AS_STMT_LIST L100;
 OB L101;
 dAS_STMT L102;
 dAS_CLASS_ELT L103;
 dAS_STMT L104;
 AM_LOCAL_EXPR L105;
 OB L106;
 create_self = ((AS_CLASS_DEF) NULL);
 L4=ZALLOC(sizeof(struct AS_CLASS_DEF_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_CLASS_DEF_tag;
 L3 = ((AS_CLASS_DEF) L4);
 ret_val1 = L3;
 as = ret_val1;
 L5 = s;
 SATTR(as,source1,(*dAS_ST957383256[TAG(L5)])(L5));
 SATTR(as,kind,AS_CLASS_DEF_ref);
 SATTR(as,name1,newid_cl);
 class_param = ATTR(orig_class_as,params);
 while (1) {
  if ((class_param==((AS_PARAM_DEC) NULL))) {
   goto after_loop;
  }
  create_self1 = ((AS_PARAM_DEC) NULL);
  L7=ZALLOC(sizeof(struct AS_PARAM_DEC_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=AS_PARAM_DEC_tag;
  L6 = ((AS_PARAM_DEC) L7);
  ret_val2 = L6;
  pardec = ret_val2;
  L8 = s;
  SATTR(pardec,source1,(*dAS_ST957383256[TAG(L8)])(L8));
  SATTR(pardec,name1,ATTR(class_param,name1));
  if ((ATTR(as,params)==((AS_PARAM_DEC) NULL))) {
   SATTR(as,params,pardec);
  }
  else {
   AS_PAR769375844(ATTR(as,params), pardec);
  }
  class_param = ATTR(class_param,next);
 }
 after_loop: ;
 SATTR(ATTR(self,prog),as_tbl,PROG_A2047269051(ATTR(ATTR(self,prog),as_tbl), as));
 add_helper_to_tb = self;
 add_helper_to_tb1 = newid_cl;
 add_helper_to_tb2 = orig_cl_params;
 tp_class_for_sel = ATTR(ATTR(add_helper_to_tb,prog),tp_tbl);
 tp_class_for_nam = add_helper_to_tb1;
 tp_class_for_par = add_helper_to_tb2;
 ret_val4 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
 tp1 = ret_val4;
 if ((tp1==((TP_CLASS) NULL))) {
  err_self = add_helper_to_tb;
  err_s = ((STR) &Compil163952783);
  if ((err_self==((TRANS) NULL))) {
   L10 = TRUE;
  } else {
   L10 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L10) {
   L9 = TRUE;
  } else {
   L9 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L9) {
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
 }
 L12 = FSETdT1764379880(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp1));
 L131_=!(L12); 
 if (L131_) {
  SATTR(ATTR(add_helper_to_tb,prog),tp_done,FSETdT1016814448(ATTR(ATTR(add_helper_to_tb,prog),tp_done), ((dTP) tp1)));
 }
 ret_val3 = tp1;
 tp = ret_val3;
 {
  struct TRANS_1349638895_frame_struct other2_0;
TRANS_1349638895_frame noname1 = &other2_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L14 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   loc = L14;
   L16 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L17 = STR_is111530248(L16, ((STR) &S_));
   L181_=!(L17); 
   if (L181_) {
    if ((ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL))) {
     L19 = TRUE;
    } else {
     L20 = TRANS_157729662(self, ATTR(loc,name1), ATTR(self,cur_param_ob));
     L221_=!(L20); 
     L19 = L221_;
    }
    L15 = L19;
   } else {
    L15 = FALSE;
   }
   if (L15) {
    create_self2 = ((AS_ATTR_DEF) NULL);
    L24=ZALLOC(sizeof(struct AS_ATTR_DEF_struct));
    if (L24==NULL) FATAL("Unable to allocate more memory");
    ((OB)L24)->header.tag=AS_ATTR_DEF_tag;
    L23 = ((AS_ATTR_DEF) L24);
    ret_val7 = L23;
    var = ret_val7;
    L25 = s;
    SATTR(var,source1,(*dAS_ST957383256[TAG(L25)])(L25));
    SATTR(var,name1,ATTR(loc,name1));
    L26 = (ATTR(loc,as_type)==((AS_TYPE_SPEC) NULL));
    L271_=!(L26); 
    if (L271_) {
     L28 = ATTR(ATTR(loc,as_type),kind);
     L29 = AS_TYP1853679690;
     L301_=(L28)==(L29); 
     L31 = L301_;
     L321_=!(L31); 
     if (L321_) {
      tps = ATTR(loc,as_type);
     }
     else {
      tp_self = loc;
      ret_val8 = ATTR(tp_self,tp_at);
      L33 = ret_val8;
      tps = (*dTP_as1220129267[TAG(L33)])(L33);
     }
    }
    else {
     tp_self1 = loc;
     ret_val9 = ATTR(tp_self1,tp_at);
     L34 = ret_val9;
     tps = (*dTP_as1220129267[TAG(L34)])(L34);
    }
    SATTR(var,tp,tps);
    if ((ATTR(as,body)==((dAS_CLASS_ELT) NULL))) {
     SATTR(as,body,((dAS_CLASS_ELT) var));
    }
    else {
     L35 = ATTR(as,body);
     (*dAS_CL1056554733[TAG(L35)])(L35, ((dAS_CLASS_ELT) var));
    }
   }
  }
 }
 after_loop1: ;
 if (TRANS_2091440962(self)) {
  L37 = STR_head_INTrSTR(newid_cl.str, 7);
  L36 = STR_is111530248(L37, ((STR) &S_pS_att));
 } else {
  L36 = FALSE;
 }
 if (L36) {
  if ((ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL))) {
   err_self1 = self;
   err_s1 = ((STR) &Compil1359403594);
   if ((err_self1==((TRANS) NULL))) {
    L39 = TRUE;
   } else {
    L39 = (ATTR(err_self1,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L39) {
    L38 = TRUE;
   } else {
    L38 = (ATTR(ATTR(err_self1,cur_rout),sig1)==((SIG) NULL));
   }
   if (L38) {
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   else {
    plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
    plus_sarg2 = ((STR) &name21);
    ret_val10 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val10;
    plus_sarg3 = err_s1;
    ret_val11 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val11;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  create_self3 = ((AS_ATTR_DEF) NULL);
  L41=ZALLOC(sizeof(struct AS_ATTR_DEF_struct));
  if (L41==NULL) FATAL("Unable to allocate more memory");
  ((OB)L41)->header.tag=AS_ATTR_DEF_tag;
  L40 = ((AS_ATTR_DEF) L41);
  ret_val12 = L40;
  var11 = ret_val12;
  L42 = s;
  SATTR(var11,source1,(*dAS_ST957383256[TAG(L42)])(L42));
  SATTR(var11,name1,ATTR(ATTR(self,cur_cohort),name1));
  create_self4 = ((AS_TYPE_SPEC) NULL);
  L44=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=AS_TYPE_SPEC_tag;
  L43 = ((AS_TYPE_SPEC) L44);
  ret_val13 = L43;
  tps1 = ret_val13;
  L45 = s;
  SATTR(tps1,source1,(*dAS_ST957383256[TAG(L45)])(L45));
  SATTR(tps1,name1,IDENT_1150413730(IDENT_zero, ((STR) &PAR_ATTACH)));
  SATTR(var11,tp,tps1);
  if ((ATTR(as,body)==((dAS_CLASS_ELT) NULL))) {
   SATTR(as,body,((dAS_CLASS_ELT) var11));
  }
  else {
   L46 = ATTR(as,body);
   (*dAS_CL1056554733[TAG(L46)])(L46, ((dAS_CLASS_ELT) var11));
  }
 }
 L47 = (ATTR(self,cur_par_ob)==((AM_LOCAL_EXPR) NULL));
 L481_=!(L47); 
 if (L481_) {
  create_self5 = ((AS_ATTR_DEF) NULL);
  L50=ZALLOC(sizeof(struct AS_ATTR_DEF_struct));
  if (L50==NULL) FATAL("Unable to allocate more memory");
  ((OB)L50)->header.tag=AS_ATTR_DEF_tag;
  L49 = ((AS_ATTR_DEF) L50);
  ret_val14 = L49;
  var21 = ret_val14;
  L51 = s;
  SATTR(var21,source1,(*dAS_ST957383256[TAG(L51)])(L51));
  SATTR(var21,name1,ATTR(ATTR(self,cur_par_ob),name1));
  L52 = (ATTR(ATTR(self,cur_par_ob),as_type)==((AS_TYPE_SPEC) NULL));
  L531_=!(L52); 
  if (L531_) {
   tps2 = ATTR(ATTR(self,cur_par_ob),as_type);
  }
  else {
   tp_self2 = ATTR(self,cur_par_ob);
   ret_val15 = ATTR(tp_self2,tp_at);
   L54 = ret_val15;
   tps2 = (*dTP_as1220129267[TAG(L54)])(L54);
  }
  SATTR(var21,tp,tps2);
  if ((ATTR(as,body)==((dAS_CLASS_ELT) NULL))) {
   SATTR(as,body,((dAS_CLASS_ELT) var21));
  }
  else {
   L55 = ATTR(as,body);
   (*dAS_CL1056554733[TAG(L55)])(L55, ((dAS_CLASS_ELT) var21));
  }
 }
 else {
  L56 = (ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL));
  L571_=!(L56); 
  if (L571_) {
   tp_self3 = ATTR(self,cur_param_ob);
   ret_val16 = ATTR(tp_self3,tp_at);
   L58 = ret_val16;
   L59=(*dTP_implrIMPL[TAG(L58)])(L58);
   elts1 = ATTR(L59,elts);
   {
    struct ELT_TBL_eltbrELT_frame_struct other3_0;
ELT_TBL_eltbrELT_frame noname2 = &other3_0;
    L21 = elts1;
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L60 = ELT_TBL_eltbrELT(noname2);
     if (noname2->state == -1) {
      goto after_loop2;
     }
     elt1 = L60;
     name_self = elt1;
     ret_val17 = ATTR(ATTR(name_self,sig1),name1);
     L61=ret_val17;
     L62 = STR_head_INTrSTR(L61.str, 1);
     if (STR_is111530248(L62, ((STR) &S_))) {
      ret_self = elt1;
      ret_val18 = ATTR(ATTR(ret_self,sig1),ret);
      L63 = (ret_val18==((dTP) NULL));
      L641_=!(L63); 
      if (L641_) {
       name_self1 = elt1;
       ret_val19 = ATTR(ATTR(name_self1,sig1),name1);
       L66=ret_val19;
       L67 = STR_sizerINT(L66.str);
       L681_=(L67)<(8); 
       L69 = L681_;
       L701_=!(L69); 
       if (L701_) {
        name_self2 = elt1;
        ret_val20 = ATTR(ATTR(name_self2,sig1),name1);
        L71=ret_val20;
        L72 = STR_head_INTrSTR(L71.str, 8);
        L65 = STR_is111530248(L72, ((STR) &S_pS_par_));
       } else {
        L65 = FALSE;
       }
       if (L65) {
        create_self6 = ((AS_ATTR_DEF) NULL);
        L74=ZALLOC(sizeof(struct AS_ATTR_DEF_struct));
        if (L74==NULL) FATAL("Unable to allocate more memory");
        ((OB)L74)->header.tag=AS_ATTR_DEF_tag;
        L73 = ((AS_ATTR_DEF) L74);
        ret_val21 = L73;
        var3 = ret_val21;
        L75 = s;
        SATTR(var3,source1,(*dAS_ST957383256[TAG(L75)])(L75));
        name_self3 = elt1;
        ret_val22 = ATTR(ATTR(name_self3,sig1),name1);
        SATTR(var3,name1,ret_val22);
        as_tp_self = elt1;
        clelt = ATTR(as_tp_self,as);
        switch (TAG(clelt)) {
         case AS_CONST_DEF_tag:
          L76 = (ATTR(((AS_CONST_DEF) clelt),tp)==((AS_TYPE_SPEC) NULL));
          L771_=!(L76); 
          if (L771_) {
           res = ATTR(((AS_CONST_DEF) clelt),tp);
          }
          else {
           create_self7 = ((AS_TYPE_SPEC) NULL);
           L79=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
           if (L79==NULL) FATAL("Unable to allocate more memory");
           ((OB)L79)->header.tag=AS_TYPE_SPEC_tag;
           L78 = ((AS_TYPE_SPEC) L79);
           ret_val24 = L78;
           res = ret_val24;
           SATTR(res,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
          } break;
         case AS_SHARED_DEF_tag:
          res = ATTR(((AS_SHARED_DEF) clelt),tp); break;
         case AS_ATTR_DEF_tag:
          res = ATTR(((AS_ATTR_DEF) clelt),tp); break;
         default: ;
        }
        ret_val23 = res;
        L80 = (ret_val23==((AS_TYPE_SPEC) NULL));
        L811_=!(L80); 
        if (L811_) {
         as_tp_self1 = elt1;
         clelt1 = ATTR(as_tp_self1,as);
         switch (TAG(clelt1)) {
          case AS_CONST_DEF_tag:
           L82 = (ATTR(((AS_CONST_DEF) clelt1),tp)==((AS_TYPE_SPEC) NULL));
           L831_=!(L82); 
           if (L831_) {
            res1 = ATTR(((AS_CONST_DEF) clelt1),tp);
           }
           else {
            create_self8 = ((AS_TYPE_SPEC) NULL);
            L85=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
            if (L85==NULL) FATAL("Unable to allocate more memory");
            ((OB)L85)->header.tag=AS_TYPE_SPEC_tag;
            L84 = ((AS_TYPE_SPEC) L85);
            ret_val26 = L84;
            res1 = ret_val26;
            SATTR(res1,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
           } break;
          case AS_SHARED_DEF_tag:
           res1 = ATTR(((AS_SHARED_DEF) clelt1),tp); break;
          case AS_ATTR_DEF_tag:
           res1 = ATTR(((AS_ATTR_DEF) clelt1),tp); break;
          default: ;
         }
         ret_val25 = res1;
         tps3 = ret_val25;
        }
        else {
         ret_self1 = elt1;
         ret_val27 = ATTR(ATTR(ret_self1,sig1),ret);
         L86 = ret_val27;
         tps3 = (*dTP_as1220129267[TAG(L86)])(L86);
        }
        SATTR(var3,tp,tps3);
        if ((ATTR(as,body)==((dAS_CLASS_ELT) NULL))) {
         SATTR(as,body,((dAS_CLASS_ELT) var3));
        }
        else {
         L87 = ATTR(as,body);
         (*dAS_CL1056554733[TAG(L87)])(L87, ((dAS_CLASS_ELT) var3));
        }
        goto after_loop2;
       }
      }
     }
    }
   }
   after_loop2: ;
  }
 }
 create_self9 = ((AS_ROUT_DEF) NULL);
 L89=ZALLOC(sizeof(struct AS_ROUT_DEF_struct));
 if (L89==NULL) FATAL("Unable to allocate more memory");
 ((OB)L89)->header.tag=AS_ROUT_DEF_tag;
 L88 = ((AS_ROUT_DEF) L89);
 ret_val28 = L88;
 crt = ret_val28;
 L90 = s;
 SATTR(crt,source1,(*dAS_ST957383256[TAG(L90)])(L90));
 SATTR(crt,name1,IDENT_1150413730(IDENT_zero, ((STR) &create1)));
 create_self10 = ((AS_TYPE_SPEC) NULL);
 L92=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L92==NULL) FATAL("Unable to allocate more memory");
 ((OB)L92)->header.tag=AS_TYPE_SPEC_tag;
 L91 = ((AS_TYPE_SPEC) L92);
 ret_val29 = L91;
 ret_dec = ret_val29;
 L93 = s;
 SATTR(ret_dec,source1,(*dAS_ST957383256[TAG(L93)])(L93));
 SATTR(ret_dec,kind,AS_TYP1853679690);
 SATTR(crt,ret_dec,ret_dec);
 create_self11 = ((AS_RETURN_STMT) NULL);
 L95=ZALLOC(sizeof(struct AS_RETURN_STMT_struct));
 if (L95==NULL) FATAL("Unable to allocate more memory");
 ((OB)L95)->header.tag=AS_RETURN_STMT_tag;
 L94 = ((AS_RETURN_STMT) L95);
 ret_val30 = L94;
 crtbdy = ret_val30;
 L96 = s;
 SATTR(crtbdy,source1,(*dAS_ST957383256[TAG(L96)])(L96));
 create_self12 = ((AS_NEW_EXPR) NULL);
 L98=ZALLOC(sizeof(struct AS_NEW_EXPR_struct));
 if (L98==NULL) FATAL("Unable to allocate more memory");
 ((OB)L98)->header.tag=AS_NEW_EXPR_tag;
 L97 = ((AS_NEW_EXPR) L98);
 ret_val31 = L97;
 crtnew = ret_val31;
 L99 = s;
 SATTR(crtnew,source1,(*dAS_ST957383256[TAG(L99)])(L99));
 SATTR(crtbdy,val1,((dAS_EXPR) crtnew));
 create_self13 = ((AS_STMT_LIST) NULL);
 L101=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L101==NULL) FATAL("Unable to allocate more memory");
 ((OB)L101)->header.tag=AS_STMT_LIST_tag;
 L100 = ((AS_STMT_LIST) L101);
 ret_val32 = L100;
 SATTR(crt,body,ret_val32);
 L102 = s;
 SATTR(ATTR(crt,body),source1,(*dAS_ST957383256[TAG(L102)])(L102));
 SATTR(crtbdy,surr_stmt_list,ATTR(crt,body));
 SATTR(crtbdy,surr_stmt_list,ATTR(crt,body));
 SATTR(ATTR(crt,body),stmts,((dAS_STMT) crtbdy));
 if ((ATTR(as,body)==((dAS_CLASS_ELT) NULL))) {
  SATTR(as,body,((dAS_CLASS_ELT) crt));
 }
 else {
  L103 = ATTR(as,body);
  (*dAS_CL1056554733[TAG(L103)])(L103, ((dAS_CLASS_ELT) crt));
 }
 impl = TP_CLA1109727233(tp);
 dob = TP_BUI1325635093;
 add_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
 add_at = dob;
 add_ct = ((dTP) tp);
 s1 = FMAPTP861983338(ATTR(add_self,tbl), add_at);
 s1 = FSETdT1016814448(s1, add_ct);
 SATTR(add_self,tbl,FMAPTP551118428(ATTR(add_self,tbl), add_at, s1));
 ip1 = TP_GRA583006971(ATTR(ATTR(self,prog),tp_graph), tp);
 ip2 = TP_GRA2060761681(ATTR(ATTR(self,prog),tp_graph), tp);
 create_self14 = ((AM_LOCAL_EXPR) NULL);
 L104 = s;
 create_src = (*dAS_ST957383256[TAG(L104)])(L104);
 create_name = newid_ob;
 create_tp = ((dTP) tp);
 L106=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L106==NULL) FATAL("Unable to allocate more memory");
 ((OB)L106)->header.tag=AM_LOCAL_EXPR_tag;
 L105 = ((AM_LOCAL_EXPR) L106);
 r = L105;
 SATTR(r,source1,create_src);
 SATTR(r,name1,create_name);
 SATTR(r,tp_at,create_tp);
 ret_val33 = r;
 ret_val = ret_val33;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ASSIGN_STMT TRANS_1390141743(TRANS self, IDENT newid_ob, IDENT newid_cl, AS_CLASS_DEF orig_class_as, AS_STMT_LIST body11, SFILE_ID source1) {
 AS_ASSIGN_STMT ret_val;
 AS_ASSIGN_STMT as3;
 AS_TYPE_SPEC tp3;
 AS_PARAM_DEC class_param;
 AS_TYPE_SPEC pardec;
 AS_CREATE_EXPR rhs3;
 AS_ASSIGN_STMT create_self;
 AS_ASSIGN_STMT ret_val1;
 AS_TYPE_SPEC create_self1;
 AS_TYPE_SPEC ret_val2;
 AS_TYPE_SPEC create_self2;
 AS_TYPE_SPEC ret_val3;
 AS_CREATE_EXPR create_self3;
 AS_CREATE_EXPR ret_val4;
 AS_ASSIGN_STMT L1;
 OB L2;
 AS_TYPE_SPEC L3;
 OB L4;
 AS_TYPE_SPEC L5;
 OB L6;
 AS_CREATE_EXPR L7;
 OB L8;
 create_self = ((AS_ASSIGN_STMT) NULL);
 L2=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_ASSIGN_STMT_tag;
 L1 = ((AS_ASSIGN_STMT) L2);
 ret_val1 = L1;
 as3 = ret_val1;
 SATTR(as3,source1,source1);
 SATTR(as3,name1,newid_ob);
 SATTR(as3,surr_stmt_list,body11);
 create_self1 = ((AS_TYPE_SPEC) NULL);
 L4=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_TYPE_SPEC_tag;
 L3 = ((AS_TYPE_SPEC) L4);
 ret_val2 = L3;
 tp3 = ret_val2;
 SATTR(tp3,source1,source1);
 SATTR(tp3,name1,newid_cl);
 class_param = ATTR(orig_class_as,params);
 while (1) {
  if ((class_param==((AS_PARAM_DEC) NULL))) {
   goto after_loop;
  }
  create_self2 = ((AS_TYPE_SPEC) NULL);
  L6=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=AS_TYPE_SPEC_tag;
  L5 = ((AS_TYPE_SPEC) L6);
  ret_val3 = L5;
  pardec = ret_val3;
  SATTR(pardec,source1,source1);
  SATTR(pardec,name1,ATTR(class_param,name1));
  if ((ATTR(tp3,params)==((AS_TYPE_SPEC) NULL))) {
   SATTR(tp3,params,pardec);
  }
  else {
   AS_TYP1027254244(ATTR(tp3,params), pardec);
  }
  class_param = ATTR(class_param,next);
 }
 after_loop: ;
 create_self3 = ((AS_CREATE_EXPR) NULL);
 L8=ZALLOC(sizeof(struct AS_CREATE_EXPR_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AS_CREATE_EXPR_tag;
 L7 = ((AS_CREATE_EXPR) L8);
 ret_val4 = L7;
 rhs3 = ret_val4;
 SATTR(rhs3,source1,source1);
 SATTR(rhs3,tp,tp3);
 SATTR(as3,rhs,((dAS_EXPR) rhs3));
 ret_val = as3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ASSIGN_STMT TRANS_2064676398(TRANS self, IDENT newid_ob, AS_STMT_LIST body11, SFILE_ID source1) {
 AS_ASSIGN_STMT ret_val;
 AS_ASSIGN_STMT exp1 = ((AS_ASSIGN_STMT) NULL);
 AS_CALL_EXPR lhs;
 AS_CALL_EXPR lhsob;
 AS_CALL_EXPR rhs;
 ELT_TBL elts1;
 IDENT par_ob = IDENT_zero;
 ELT_TBL L11;
 ELT elt1;
 AS_CALL_EXPR rhs1;
 AS_CALL_EXPR lhs1;
 AS_CALL_EXPR lhsob1;
 AS_ASSIGN_STMT create_self;
 AS_ASSIGN_STMT ret_val1;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val2;
 AS_CALL_EXPR create_self2;
 AS_CALL_EXPR ret_val3;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val4;
 AS_ASSIGN_STMT create_self4;
 AS_ASSIGN_STMT ret_val5;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val6;
 ELT name_self;
 IDENT ret_val7 = IDENT_zero;
 ELT ret_self;
 dTP ret_val8;
 ELT name_self1;
 IDENT ret_val9 = IDENT_zero;
 ELT name_self2;
 IDENT ret_val10 = IDENT_zero;
 ELT name_self3;
 IDENT ret_val11 = IDENT_zero;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val12;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val13;
 AS_CALL_EXPR create_self5;
 AS_CALL_EXPR ret_val14;
 AS_CALL_EXPR create_self6;
 AS_CALL_EXPR ret_val15;
 BOOL L2;
 BOOL L31_;
 AS_ASSIGN_STMT L4;
 OB L5;
 AS_CALL_EXPR L6;
 OB L7;
 AS_CALL_EXPR L8;
 OB L9;
 AS_CALL_EXPR L10;
 OB L12;
 BOOL L13;
 BOOL L141_;
 AS_ASSIGN_STMT L15;
 OB L16;
 dTP L17;
 IMPL L18;
 ELT L19;
 IDENT L20;
 STR L21;
 extern STR S_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 IDENT L25;
 INT L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 IDENT L30;
 STR L32;
 extern STR S_pS_par_;
 extern STR Compil862630323;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 AS_CALL_EXPR L35;
 OB L36;
 AS_CALL_EXPR L37;
 OB L38;
 L2 = (ATTR(self,cur_par_ob)==((AM_LOCAL_EXPR) NULL));
 L31_=!(L2); 
 if (L31_) {
  create_self = ((AS_ASSIGN_STMT) NULL);
  L5=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=AS_ASSIGN_STMT_tag;
  L4 = ((AS_ASSIGN_STMT) L5);
  ret_val1 = L4;
  exp1 = ret_val1;
  SATTR(exp1,source1,source1);
  SATTR(exp1,surr_stmt_list,body11);
  create_self1 = ((AS_CALL_EXPR) NULL);
  L7=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=AS_CALL_EXPR_tag;
  L6 = ((AS_CALL_EXPR) L7);
  ret_val2 = L6;
  lhs = ret_val2;
  SATTR(lhs,source1,source1);
  create_self2 = ((AS_CALL_EXPR) NULL);
  L9=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=AS_CALL_EXPR_tag;
  L8 = ((AS_CALL_EXPR) L9);
  ret_val3 = L8;
  lhsob = ret_val3;
  SATTR(lhsob,source1,source1);
  SATTR(lhsob,name1,newid_ob);
  SATTR(lhs,ob,((dAS_EXPR) lhsob));
  SATTR(lhs,name1,ATTR(ATTR(self,cur_par_ob),name1));
  SATTR(exp1,lhs_expr,((dAS_EXPR) lhs));
  create_self3 = ((AS_CALL_EXPR) NULL);
  L12=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=AS_CALL_EXPR_tag;
  L10 = ((AS_CALL_EXPR) L12);
  ret_val4 = L10;
  rhs = ret_val4;
  SATTR(rhs,source1,source1);
  SATTR(rhs,name1,ATTR(ATTR(self,cur_par_ob),name1));
  SATTR(exp1,rhs,((dAS_EXPR) rhs));
 }
 else {
  L13 = (ATTR(self,cur_param_ob)==((AM_LOCAL_EXPR) NULL));
  L141_=!(L13); 
  if (L141_) {
   create_self4 = ((AS_ASSIGN_STMT) NULL);
   L16=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
   if (L16==NULL) FATAL("Unable to allocate more memory");
   ((OB)L16)->header.tag=AS_ASSIGN_STMT_tag;
   L15 = ((AS_ASSIGN_STMT) L16);
   ret_val5 = L15;
   exp1 = ret_val5;
   SATTR(exp1,source1,source1);
   SATTR(exp1,surr_stmt_list,body11);
   tp_self = ATTR(self,cur_param_ob);
   ret_val6 = ATTR(tp_self,tp_at);
   L17 = ret_val6;
   L18=(*dTP_implrIMPL[TAG(L17)])(L17);
   elts1 = ATTR(L18,elts);
   {
    struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
    L11 = elts1;
    noname1->self = L11;
    noname1->state = 0;
    while (1) {
     L19 = ELT_TBL_eltbrELT(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     elt1 = L19;
     name_self = elt1;
     ret_val7 = ATTR(ATTR(name_self,sig1),name1);
     L20=ret_val7;
     L21 = STR_head_INTrSTR(L20.str, 1);
     if (STR_is111530248(L21, ((STR) &S_))) {
      ret_self = elt1;
      ret_val8 = ATTR(ATTR(ret_self,sig1),ret);
      L22 = (ret_val8==((dTP) NULL));
      L231_=!(L22); 
      if (L231_) {
       name_self1 = elt1;
       ret_val9 = ATTR(ATTR(name_self1,sig1),name1);
       L25=ret_val9;
       L26 = STR_sizerINT(L25.str);
       L271_=(L26)<(8); 
       L28 = L271_;
       L291_=!(L28); 
       if (L291_) {
        name_self2 = elt1;
        ret_val10 = ATTR(ATTR(name_self2,sig1),name1);
        L30=ret_val10;
        L32 = STR_head_INTrSTR(L30.str, 8);
        L24 = STR_is111530248(L32, ((STR) &S_pS_par_));
       } else {
        L24 = FALSE;
       }
       if (L24) {
        name_self3 = elt1;
        ret_val11 = ATTR(ATTR(name_self3,sig1),name1);
        par_ob = ret_val11;
        goto after_loop;
       }
      }
     }
    }
   }
   after_loop: ;
   if ((par_ob.str==(STR)0)) {
    err_self = self;
    err_s = ((STR) &Compil862630323);
    if ((err_self==((TRANS) NULL))) {
     L34 = TRUE;
    } else {
     L34 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
    }
    if (L34) {
     L33 = TRUE;
    } else {
     L33 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
    }
    if (L33) {
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    else {
     plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
     plus_sarg = ((STR) &name21);
     ret_val12 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val12;
     plus_sarg1 = err_s;
     ret_val13 = STR_ap2004550586(plus_self1, plus_sarg1);
     err_s = ret_val13;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
   }
   rhs1 = TRANS_1849744987(self, par_ob, ATTR(self,cur_param_ob), source1);
   create_self5 = ((AS_CALL_EXPR) NULL);
   L36=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   ((OB)L36)->header.tag=AS_CALL_EXPR_tag;
   L35 = ((AS_CALL_EXPR) L36);
   ret_val14 = L35;
   lhs1 = ret_val14;
   SATTR(lhs1,source1,source1);
   create_self6 = ((AS_CALL_EXPR) NULL);
   L38=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=AS_CALL_EXPR_tag;
   L37 = ((AS_CALL_EXPR) L38);
   ret_val15 = L37;
   lhsob1 = ret_val15;
   SATTR(lhsob1,source1,source1);
   SATTR(lhsob1,name1,newid_ob);
   SATTR(lhs1,ob,((dAS_EXPR) lhsob1));
   SATTR(lhs1,name1,par_ob);
   SATTR(exp1,lhs_expr,((dAS_EXPR) lhs1));
   SATTR(exp1,rhs,((dAS_EXPR) rhs1));
  }
 }
 ret_val = exp1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ROUT_DEF TRANS_725662504(TRANS self, IDENT routname, IDENT helper_ob, IDENT helper_cl, AM_LOCAL_EXPR helper1, AS_CLASS_DEF orig_class_as, TP_CLASS current_class_tp, IDENT gate1, AS_TYPE_SPEC orig_gate_tp, AS_STMT_LIST body11, INT code11, SFILE_ID source1) {
 AS_ROUT_DEF ret_val;
 AS_TYPE_SPEC gate_tp;
 AS_ROUT_DEF newrout;
 INT L11 = INT_zero;
 AS_ARG_DEC argdec0;
 AS_TYPE_SPEC argdec0tp;
 AS_PARAM_DEC class_param;
 AS_TYPE_SPEC pardec;
 AS_ARG_DEC argdec1;
 AS_ARG_DEC argdec2;
 AS_TYPE_SPEC argdec2tp;
 dAS_STMT decl_stmts = ((dAS_STMT) NULL);
 AS_DEC_STMT asdec = ((AS_DEC_STMT) NULL);
 TRANS L21;
 AM_LOCAL_EXPR loc;
 dAS_STMT copy_in_stmts;
 AS_ASSIGN_STMT asin1 = ((AS_ASSIGN_STMT) NULL);
 TRANS L31;
 AM_LOCAL_EXPR loc1;
 AS_CALL_EXPR asinlhs;
 dAS_STMT copy_out_stmts = ((dAS_STMT) NULL);
 AS_EXPR_STMT call11;
 AS_AT_EXPR atexpr;
 AS_CALL_EXPR callexpr;
 AS_CALL_EXPR callexprob;
 AS_WHERE_EXPR whereexpr;
 AS_CALL_EXPR whereobexpr;
 AS_ROUT_DEF create_self;
 AS_ROUT_DEF ret_val1;
 AS_ARG_DEC create_self1;
 AS_ARG_DEC ret_val2;
 AS_ARG_MODE create_self2;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val3;
 AS_ARG_MODE res;
 AS_TYPE_SPEC create_self3;
 AS_TYPE_SPEC ret_val4;
 AS_TYPE_SPEC create_self4;
 AS_TYPE_SPEC ret_val5;
 AS_ARG_DEC create_self5;
 AS_ARG_DEC ret_val6;
 AS_ARG_MODE create_self6;
 INT create_m1 = INT_zero;
 AS_ARG_MODE ret_val7;
 AS_ARG_MODE res1;
 AS_TYPE_SPEC create_self7;
 AS_TYPE_SPEC ret_val8;
 AS_ARG_DEC create_self8;
 AS_ARG_DEC ret_val9;
 AS_ARG_MODE create_self9;
 INT create_m2 = INT_zero;
 AS_ARG_MODE ret_val10;
 AS_ARG_MODE res2;
 AS_TYPE_SPEC create_self10;
 AS_TYPE_SPEC ret_val11;
 AS_STMT_LIST create_self11;
 AS_STMT_LIST ret_val12;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 AS_DEC_STMT create_self12;
 AS_DEC_STMT ret_val14;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val15;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val16 = BOOL_zero;
 AS_ASSIGN_STMT create_self13;
 AS_ASSIGN_STMT ret_val17;
 AS_CALL_EXPR create_self14;
 AS_CALL_EXPR ret_val18;
 AS_EXPR_STMT create_self15;
 AS_EXPR_STMT ret_val19;
 AS_AT_EXPR create_self16;
 AS_AT_EXPR ret_val20;
 AS_CALL_EXPR create_self17;
 AS_CALL_EXPR ret_val21;
 AS_CALL_EXPR create_self18;
 AS_CALL_EXPR ret_val22;
 AS_ARG_MODE create_self19;
 INT create_m3 = INT_zero;
 AS_ARG_MODE ret_val23;
 AS_ARG_MODE res3;
 AS_WHERE_EXPR create_self20;
 AS_WHERE_EXPR ret_val24;
 AS_CALL_EXPR create_self21;
 AS_CALL_EXPR ret_val25;
 AS_ROUT_DEF L4;
 OB L5;
 AS_ARG_DEC L6;
 OB L7;
 AS_ARG_MODE L8;
 OB L9;
 AS_TYPE_SPEC L10;
 OB L12;
 AS_TYPE_SPEC L13;
 OB L14;
 AS_ARG_DEC L15;
 OB L16;
 AS_ARG_MODE L17;
 OB L18;
 AS_TYPE_SPEC L19;
 OB L20;
 extern STR PAR_ATTACH;
 AS_ARG_DEC L22;
 OB L23;
 AS_ARG_MODE L24;
 OB L25;
 extern STR S_pS_at;
 AS_TYPE_SPEC L26;
 OB L27;
 extern STR INT1;
 AS_STMT_LIST L28;
 OB L29;
 dAS_CLASS_ELT L30;
 AM_LOCAL_EXPR L32;
 BOOL L33;
 STR L34;
 extern STR S_;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 AS_DEC_STMT L41;
 OB L42;
 BOOL L43;
 BOOL L441_;
 dTP L45;
 dAS_STMT L46;
 AM_LOCAL_EXPR L47;
 BOOL L48;
 BOOL L49;
 STR L50;
 extern STR S_;
 BOOL L51;
 BOOL L521_;
 BOOL L53;
 BOOL L54;
 BOOL L55;
 BOOL L561_;
 BOOL L57;
 BOOL L581_;
 AS_ASSIGN_STMT L59;
 OB L60;
 AS_CALL_EXPR L61;
 OB L62;
 dAS_STMT L63;
 AS_EXPR_STMT L64;
 OB L65;
 AS_AT_EXPR L66;
 OB L67;
 AS_CALL_EXPR L68;
 OB L69;
 extern STR death;
 AS_CALL_EXPR L70;
 OB L71;
 AS_ARG_MODE L72;
 OB L73;
 AS_WHERE_EXPR L74;
 OB L75;
 AS_CALL_EXPR L76;
 OB L77;
 dAS_STMT L78;
 BOOL L79;
 BOOL L801_;
 BOOL L81;
 BOOL L821_;
 dAS_STMT L83;
 dAS_STMT L84;
 dAS_STMT L85;
 gate_tp = orig_gate_tp;
 create_self = ((AS_ROUT_DEF) NULL);
 L5=ZALLOC(sizeof(struct AS_ROUT_DEF_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AS_ROUT_DEF_tag;
 L4 = ((AS_ROUT_DEF) L5);
 ret_val1 = L4;
 newrout = ret_val1;
 SATTR(newrout,source1,source1);
 SATTR(newrout,name1,routname);
 L11 = code11;
 switch (L11) {
  case 12: 
   SATTR(newrout,is_par_routine,TRUE);
   break;
  case 11: 
   SATTR(newrout,is_fork_routine,TRUE);
   break;
  case 13: 
   SATTR(newrout,is_attach_routin,TRUE);
   break;
  default: ;
   FATAL("No applicable target in case statement\nin TRANS::turn_into_routine(IDENT,IDENT,IDENT,AM_LOCAL_EXPR,AS_CLASS_DEF,TP_CLASS,IDENT,AS_TYPE_SPEC,AS_STMT_LIST,INT,SFILE_ID):AS_ROUT_DEF\n./Middle/trans.sa:4381:19");
 }
 create_self1 = ((AS_ARG_DEC) NULL);
 L7=ZALLOC(sizeof(struct AS_ARG_DEC_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AS_ARG_DEC_tag;
 L6 = ((AS_ARG_DEC) L7);
 ret_val2 = L6;
 argdec0 = ret_val2;
 SATTR(argdec0,source1,source1);
 SATTR(argdec0,name1,helper_ob);
 create_self2 = ((AS_ARG_MODE) NULL);
 create_m = AS_ARG1300937332;
 L9=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AS_ARG_MODE_tag;
 L8 = ((AS_ARG_MODE) L9);
 res = L8;
 SATTR(res,mod,create_m);
 ret_val3 = res;
 SATTR(argdec0,mode1,ret_val3);
 create_self3 = ((AS_TYPE_SPEC) NULL);
 L12=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AS_TYPE_SPEC_tag;
 L10 = ((AS_TYPE_SPEC) L12);
 ret_val4 = L10;
 argdec0tp = ret_val4;
 SATTR(argdec0tp,source1,source1);
 SATTR(argdec0tp,name1,helper_cl);
 class_param = ATTR(orig_class_as,params);
 while (1) {
  if ((class_param==((AS_PARAM_DEC) NULL))) {
   goto after_loop;
  }
  create_self4 = ((AS_TYPE_SPEC) NULL);
  L14=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=AS_TYPE_SPEC_tag;
  L13 = ((AS_TYPE_SPEC) L14);
  ret_val5 = L13;
  pardec = ret_val5;
  SATTR(pardec,source1,source1);
  SATTR(pardec,name1,ATTR(class_param,name1));
  if ((ATTR(argdec0tp,params)==((AS_TYPE_SPEC) NULL))) {
   SATTR(argdec0tp,params,pardec);
  }
  else {
   AS_TYP1027254244(ATTR(argdec0tp,params), pardec);
  }
  class_param = ATTR(class_param,next);
 }
 after_loop: ;
 SATTR(argdec0,tp,argdec0tp);
 create_self5 = ((AS_ARG_DEC) NULL);
 L16=ZALLOC(sizeof(struct AS_ARG_DEC_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=AS_ARG_DEC_tag;
 L15 = ((AS_ARG_DEC) L16);
 ret_val6 = L15;
 argdec1 = ret_val6;
 SATTR(argdec1,source1,source1);
 SATTR(argdec1,name1,gate1);
 create_self6 = ((AS_ARG_MODE) NULL);
 create_m1 = AS_ARG1300937332;
 L18=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=AS_ARG_MODE_tag;
 L17 = ((AS_ARG_MODE) L18);
 res1 = L17;
 SATTR(res1,mod,create_m1);
 ret_val7 = res1;
 SATTR(argdec1,mode1,ret_val7);
 if ((gate_tp==((AS_TYPE_SPEC) NULL))) {
  create_self7 = ((AS_TYPE_SPEC) NULL);
  L20=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=AS_TYPE_SPEC_tag;
  L19 = ((AS_TYPE_SPEC) L20);
  ret_val8 = L19;
  gate_tp = ret_val8;
  SATTR(gate_tp,source1,source1);
  SATTR(gate_tp,name1,IDENT_1150413730(IDENT_zero, ((STR) &PAR_ATTACH)));
 }
 SATTR(argdec1,tp,gate_tp);
 create_self8 = ((AS_ARG_DEC) NULL);
 L23=ZALLOC(sizeof(struct AS_ARG_DEC_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=AS_ARG_DEC_tag;
 L22 = ((AS_ARG_DEC) L23);
 ret_val9 = L22;
 argdec2 = ret_val9;
 create_self9 = ((AS_ARG_MODE) NULL);
 create_m2 = AS_ARG1300937332;
 L25=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=AS_ARG_MODE_tag;
 L24 = ((AS_ARG_MODE) L25);
 res2 = L24;
 SATTR(res2,mod,create_m2);
 ret_val10 = res2;
 SATTR(argdec2,mode1,ret_val10);
 SATTR(argdec2,source1,source1);
 SATTR(argdec2,name1,IDENT_1150413730(IDENT_zero, ((STR) &S_pS_at)));
 create_self10 = ((AS_TYPE_SPEC) NULL);
 L27=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=AS_TYPE_SPEC_tag;
 L26 = ((AS_TYPE_SPEC) L27);
 ret_val11 = L26;
 argdec2tp = ret_val11;
 SATTR(argdec2tp,source1,source1);
 SATTR(argdec2tp,name1,IDENT_1150413730(IDENT_zero, ((STR) &INT1)));
 SATTR(argdec2,tp,argdec2tp);
 AS_ARG2059479532(argdec0, argdec1);
 AS_ARG2059479532(argdec0, argdec2);
 SATTR(newrout,args_dec,argdec0);
 SATTR(newrout,is_private,TRUE);
 create_self11 = ((AS_STMT_LIST) NULL);
 L29=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=AS_STMT_LIST_tag;
 L28 = ((AS_STMT_LIST) L29);
 ret_val12 = L28;
 SATTR(newrout,body,ret_val12);
 SATTR(ATTR(newrout,body),source1,source1);
 L30 = ATTR(orig_class_as,body);
 (*dAS_CL1056554733[TAG(L30)])(L30, ((dAS_CLASS_ELT) newrout));
 TRANS_1069298803(self, current_class_tp, newrout);
 {
  struct TRANS_1349638895_frame_struct other2_0;
TRANS_1349638895_frame noname1 = &other2_0;
  L21 = self;
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   L32 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   loc = L32;
   L34 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L35 = STR_is111530248(L34, ((STR) &S_));
   L361_=!(L35); 
   if (L361_) {
    L33 = TRUE;
   } else {
    if (ATTR(newrout,is_attach_routin)) {
     L39 = (ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL));
     L401_=!(L39); 
     L38 = L401_;
    } else {
     L38 = FALSE;
    }
    if (L38) {
     is_eq_self = ATTR(loc,name1);
     is_eq_i = ATTR(ATTR(self,cur_cohort),name1);
     ret_val13 = (is_eq_self.str==is_eq_i.str);
     L37 = ret_val13;
    } else {
     L37 = FALSE;
    }
    L33 = L37;
   }
   if (L33) {
    create_self12 = ((AS_DEC_STMT) NULL);
    L42=ZALLOC(sizeof(struct AS_DEC_STMT_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=AS_DEC_STMT_tag;
    L41 = ((AS_DEC_STMT) L42);
    ret_val14 = L41;
    asdec = ret_val14;
    SATTR(asdec,source1,source1);
    SATTR(asdec,name1,ATTR(loc,name1));
    SATTR(asdec,surr_stmt_list,ATTR(newrout,body));
    L43 = (ATTR(loc,as_type)==((AS_TYPE_SPEC) NULL));
    L441_=!(L43); 
    if (L441_) {
     SATTR(asdec,tp,ATTR(loc,as_type));
    }
    else {
     tp_self = loc;
     ret_val15 = ATTR(tp_self,tp_at);
     L45 = ret_val15;
     SATTR(asdec,tp,(*dTP_as1220129267[TAG(L45)])(L45));
    }
    if ((decl_stmts==((dAS_STMT) NULL))) {
     decl_stmts = ((dAS_STMT) asdec);
    }
    else {
     L46 = decl_stmts;
     (*dAS_ST1454156204[TAG(L46)])(L46, ((dAS_STMT) asdec));
    }
   }
  }
 }
 after_loop1: ;
 copy_in_stmts = TRANS_166911016(self, TRANS_1924362320, ATTR(newrout,body), source1);
 {
  struct TRANS_1349638895_frame_struct other3_0;
TRANS_1349638895_frame noname2 = &other3_0;
  L31 = self;
  noname2->self = L31;
  noname2->state = 0;
  while (1) {
   L47 = TRANS_1349638895(noname2);
   if (noname2->state == -1) {
    goto after_loop2;
   }
   loc1 = L47;
   L50 = STR_head_INTrSTR(ATTR(loc1,name1).str, 1);
   L51 = STR_is111530248(L50, ((STR) &S_));
   L521_=!(L51); 
   if (L521_) {
    L49 = TRUE;
   } else {
    if (ATTR(newrout,is_attach_routin)) {
     L55 = (ATTR(self,cur_cohort)==((AM_LOCAL_EXPR) NULL));
     L561_=!(L55); 
     L54 = L561_;
    } else {
     L54 = FALSE;
    }
    if (L54) {
     is_eq_self1 = ATTR(loc1,name1);
     is_eq_i1 = ATTR(ATTR(self,cur_cohort),name1);
     ret_val16 = (is_eq_self1.str==is_eq_i1.str);
     L53 = ret_val16;
    } else {
     L53 = FALSE;
    }
    L49 = L53;
   }
   if (L49) {
    L57 = TRANS_157729662(self, ATTR(loc1,name1), helper1);
    L581_=!(L57); 
    L48 = L581_;
   } else {
    L48 = FALSE;
   }
   if (L48) {
    create_self13 = ((AS_ASSIGN_STMT) NULL);
    L60=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=AS_ASSIGN_STMT_tag;
    L59 = ((AS_ASSIGN_STMT) L60);
    ret_val17 = L59;
    asin1 = ret_val17;
    SATTR(asin1,source1,source1);
    SATTR(asin1,transformed,TRUE);
    create_self14 = ((AS_CALL_EXPR) NULL);
    L62=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L62==NULL) FATAL("Unable to allocate more memory");
    ((OB)L62)->header.tag=AS_CALL_EXPR_tag;
    L61 = ((AS_CALL_EXPR) L62);
    ret_val18 = L61;
    asinlhs = ret_val18;
    SATTR(asinlhs,source1,source1);
    SATTR(asinlhs,name1,ATTR(loc1,name1));
    SATTR(asin1,lhs_expr,((dAS_EXPR) asinlhs));
    SATTR(asin1,rhs,((dAS_EXPR) TRANS_1849744987(self, ATTR(loc1,name1), helper1, source1)));
    SATTR(asin1,surr_stmt_list,ATTR(newrout,body));
    L63 = copy_in_stmts;
    (*dAS_ST1454156204[TAG(L63)])(L63, ((dAS_STMT) asin1));
   }
  }
 }
 after_loop2: ;
 if ((orig_gate_tp==((AS_TYPE_SPEC) NULL))) {
  create_self15 = ((AS_EXPR_STMT) NULL);
  L65=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
  if (L65==NULL) FATAL("Unable to allocate more memory");
  ((OB)L65)->header.tag=AS_EXPR_STMT_tag;
  L64 = ((AS_EXPR_STMT) L65);
  ret_val19 = L64;
  call11 = ret_val19;
  SATTR(call11,source1,source1);
  SATTR(call11,surr_stmt_list,body11);
  create_self16 = ((AS_AT_EXPR) NULL);
  L67=ZALLOC(sizeof(struct AS_AT_EXPR_struct));
  if (L67==NULL) FATAL("Unable to allocate more memory");
  ((OB)L67)->header.tag=AS_AT_EXPR_tag;
  L66 = ((AS_AT_EXPR) L67);
  ret_val20 = L66;
  atexpr = ret_val20;
  SATTR(atexpr,source1,source1);
  create_self17 = ((AS_CALL_EXPR) NULL);
  L69=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L69==NULL) FATAL("Unable to allocate more memory");
  ((OB)L69)->header.tag=AS_CALL_EXPR_tag;
  L68 = ((AS_CALL_EXPR) L69);
  ret_val21 = L68;
  callexpr = ret_val21;
  SATTR(callexpr,source1,source1);
  SATTR(callexpr,name1,IDENT_1150413730(IDENT_zero, ((STR) &death)));
  create_self18 = ((AS_CALL_EXPR) NULL);
  L71=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L71==NULL) FATAL("Unable to allocate more memory");
  ((OB)L71)->header.tag=AS_CALL_EXPR_tag;
  L70 = ((AS_CALL_EXPR) L71);
  ret_val22 = L70;
  callexprob = ret_val22;
  SATTR(callexprob,source1,source1);
  SATTR(callexprob,name1,gate1);
  SATTR(callexpr,ob,((dAS_EXPR) callexprob));
  SATTR(callexpr,args,((dAS_EXPR) NULL));
  create_self19 = ((AS_ARG_MODE) NULL);
  create_m3 = AS_ARG1300937332;
  L73=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
  if (L73==NULL) FATAL("Unable to allocate more memory");
  ((OB)L73)->header.tag=AS_ARG_MODE_tag;
  L72 = ((AS_ARG_MODE) L73);
  res3 = L72;
  SATTR(res3,mod,create_m3);
  ret_val23 = res3;
  SATTR(callexpr,modes1,ret_val23);
  create_self20 = ((AS_WHERE_EXPR) NULL);
  L75=ZALLOC(sizeof(struct AS_WHERE_EXPR_struct));
  if (L75==NULL) FATAL("Unable to allocate more memory");
  ((OB)L75)->header.tag=AS_WHERE_EXPR_tag;
  L74 = ((AS_WHERE_EXPR) L75);
  ret_val24 = L74;
  whereexpr = ret_val24;
  SATTR(whereexpr,source1,source1);
  create_self21 = ((AS_CALL_EXPR) NULL);
  L77=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L77==NULL) FATAL("Unable to allocate more memory");
  ((OB)L77)->header.tag=AS_CALL_EXPR_tag;
  L76 = ((AS_CALL_EXPR) L77);
  ret_val25 = L76;
  whereobexpr = ret_val25;
  SATTR(whereobexpr,source1,source1);
  SATTR(whereobexpr,name1,gate1);
  SATTR(whereexpr,e,((dAS_EXPR) whereobexpr));
  SATTR(atexpr,e,((dAS_EXPR) callexpr));
  SATTR(atexpr,at,((dAS_EXPR) whereexpr));
  SATTR(call11,e,((dAS_EXPR) atexpr));
  SATTR(call11,transformed,TRUE);
  copy_out_stmts = ((dAS_STMT) call11);
 }
 else {
  copy_out_stmts = ((dAS_STMT) NULL);
 }
 if ((copy_out_stmts==((dAS_STMT) NULL))) {
  copy_out_stmts = TRANS_166911016(self, TRANS_194341863, ATTR(newrout,body), source1);
 }
 else {
  L78 = copy_out_stmts;
  (*dAS_ST1454156204[TAG(L78)])(L78, TRANS_166911016(self, TRANS_194341863, ATTR(newrout,body), source1));
 }
 L79 = (ATTR(body11,stmts)==((dAS_STMT) NULL));
 L801_=!(L79); 
 if (L801_) {
  L81 = (copy_in_stmts==((dAS_STMT) NULL));
  L821_=!(L81); 
  if (L821_) {
   L83 = copy_in_stmts;
   (*dAS_ST1454156204[TAG(L83)])(L83, ATTR(body11,stmts));
   SATTR(body11,stmts,copy_in_stmts);
  }
  L84 = ATTR(body11,stmts);
  (*dAS_ST1454156204[TAG(L84)])(L84, copy_out_stmts);
 }
 SATTR(ATTR(newrout,body),stmts,decl_stmts);
 if ((decl_stmts==((dAS_STMT) NULL))) {
  SATTR(ATTR(newrout,body),stmts,ATTR(body11,stmts));
 }
 else {
  L85 = ATTR(ATTR(newrout,body),stmts);
  (*dAS_ST1454156204[TAG(L85)])(L85, ATTR(body11,stmts));
 }
 ret_val = newrout;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1229338887(TRANS self) {
 BOOL ret_val = BOOL_zero;
 PROG psather_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
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
  ret_val = FALSE;
  return ret_val;
 }
 else {
  L4 = (ATTR(self,cur_lock)==((dAM_STMT) NULL));
  L51_=!(L4); 
  ret_val = L51_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1615967248(TRANS self, AS_FORK_STMT fork_stmt) {
 BOOL ret_val = BOOL_zero;
 dAS_STMT s = ((dAS_STMT) NULL);
 dAS_EXPR e;
 AS_ARG_MODE m;
 dAS_EXPR ae;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L5;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 dAS_EXPR L14;
 if ((ATTR(fork_stmt,body)==((AS_STMT_LIST) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 s = ATTR(ATTR(fork_stmt,body),stmts);
 if (s==NULL) {
  goto other434;
 } else
 switch (TAG(s)) {
  case AS_EXPR_STMT_tag:
   e = ATTR(((AS_EXPR_STMT) s),e);
   if (e==NULL) {
    goto other435;
   } else
   switch (TAG(e)) {
    case AS_CALL_EXPR_tag:
     if ((ATTR(((AS_EXPR_STMT) s),next)==((dAS_STMT) NULL))) {
      L1 = AS_CAL1853175953(((AS_CALL_EXPR) e));
      L21_=(2)<(L1); 
      if (L21_) {
       ret_val = FALSE;
       return ret_val;
      }
      m = ATTR(((AS_CALL_EXPR) e),modes1);
      while (1) {
       L3 = (m==((AS_ARG_MODE) NULL));
       L41_=!(L3); 
       if (L41_) {
       }
       else {
        goto after_loop;
       }
       L5 = ATTR(m,mod);
       L6 = AS_ARG1300937332;
       L71_=(L5)==(L6); 
       L8 = L71_;
       L91_=!(L8); 
       if (L91_) {
        ret_val = FALSE;
        return ret_val;
       }
       m = ATTR(m,next);
      }
      after_loop: ;
      ae = ATTR(((AS_CALL_EXPR) e),args);
      while (1) {
       L10 = (ae==((dAS_EXPR) NULL));
       L111_=!(L10); 
       if (L111_) {
       }
       else {
        goto after_loop1;
       }
       L12 = TRANS_1864518471(self, ae);
       L131_=!(L12); 
       if (L131_) {
        ret_val = FALSE;
        return ret_val;
       }
       L14 = ae;
       ae = (*dAS_EX337431651[TAG(L14)])(L14);
      }
      after_loop1: ;
      ret_val = TRUE;
      return ret_val;
     }
     else {
      ret_val = FALSE;
      return ret_val;
     } break;
    default: ;
    other435: ;
     ret_val = FALSE;
     return ret_val;
   } break;
  default: ;
  other434: ;
   ret_val = FALSE;
   return ret_val;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_1864518471(TRANS self, dAS_EXPR ae) {
 BOOL ret_val = BOOL_zero;
 dAS_EXPR e;
 INT L1;
 BOOL L21_;
 e = ae;
 if (e==NULL) {
  goto other433;
 } else
 switch (TAG(e)) {
  case AS_SELF_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_VOID_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_ARRAY_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_BOOL_LIT_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_CHAR_LIT_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_STR_LIT_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_INT_LIT_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_FLT_LIT_EXPR_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AS_CALL_EXPR_tag:
   L1 = AS_CAL1853175953(((AS_CALL_EXPR) e));
   L21_=(L1)==(0); 
   if (L21_) {
    ret_val = TRUE;
    return ret_val;
   } break;
  default: ;
  other433: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_883636795(TRANS self, AS_PAR_STMT par_stmt) {
 BOOL ret_val = BOOL_zero;
 AS_STMT_CURSOR cursor;
 BOOL simple_par = BOOL_zero;
 dAS_STMT stmt1 = ((dAS_STMT) NULL);
 AS_STMT_CURSOR L11;
 AS_STMT_CURSOR create_self;
 AS_STMT_CURSOR ret_val1;
 AS_STMT_CURSOR r;
 AS_STMT_CURSOR L2;
 OB L3;
 dAS_STMT L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 create_self = ((AS_STMT_CURSOR) NULL);
 L3=ZALLOC_LEAF(sizeof(struct AS_STMT_CURSOR_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,sizeof(struct AS_STMT_CURSOR_struct));
 ((OB)L3)->header.tag=AS_STMT_CURSOR_tag;
 L2 = ((AS_STMT_CURSOR) L3);
 r = L2;
 ret_val1 = r;
 cursor = ret_val1;
 simple_par = TRUE;
 {
  struct AS_STM1504735434_frame_struct other1_0;
AS_STM1504735434_frame noname1 = &other1_0;
  L11 = cursor;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   noname1->arg1 = ((dAS_NODE) ATTR(ATTR(par_stmt,body),stmts));
   L4 = AS_STM1504735434(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   stmt1 = L4;
   if (stmt1==NULL) {
    goto other436;
   } else
   switch (TAG(stmt1)) {
    case AS_FORK_STMT_tag:
     L5 = TRANS_1615967248(self, ((AS_FORK_STMT) stmt1));
     L61_=!(L5); 
     if (L61_) {
      simple_par = FALSE;
      goto after_loop;
     } break;
    case AS_PAR_STMT_tag:
     L7 = TRANS_883636795(self, ((AS_PAR_STMT) stmt1));
     L81_=!(L7); 
     if (L81_) {
      simple_par = FALSE;
      goto after_loop;
     } break;
    default: ;
    other436: ;
   }
  }
 }
 after_loop: ;
 ret_val = simple_par;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TRANS_900561657(TRANS self) {
 BOOL ret_val = BOOL_zero;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val2 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
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
 if ((ATTR(self,cur_rout)==((AM_ROUT_DEF) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_iter_self = ATTR(self,cur_rout);
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L2 = (is_iter_self2.str==((STR) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = is_iter_self2.str;
  L5 = STR_sizerINT(is_iter_self2.str);
  L61_=INTMINUS(L5,1); 
  L7 = L61_;
  L81_=ARR((STR)L4,L7); 
  L9 = L81_;
  L101_=L9=='!'; 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 ret_val3 = L1;
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_CONST TRANS_1139222136(TRANS self, dAM_EXPR e) {
 dAM_CONST ret_val;
 if ((e==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_CONST) NULL);
  return ret_val;
 }
 if (e==NULL) {
  goto other428;
 } else
 switch (TAG(e)) {
  case AM_CHAR_CONST_tag:
   ret_val = ((dAM_CONST) e);
   return ret_val; break;
  case AM_INT_CONST_tag:
   ret_val = ((dAM_CONST) e);
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   ret_val = TRANS_1139222136(self, ATTR(((AM_SHARED_EXPR) e),init));
   return ret_val; break;
  default: ;
  other428: ;
   ret_val = ((dAM_CONST) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1066292461(TRANS self, AS_YIELD_STMT s) {
 dAM_STMT ret_val;
 dTP rtp;
 AM_YIELD_STMT y;
 AM_YIELD_STMT r;
 TRANS yield_in_rout_er;
 AS_YIELD_STMT yield_in_rout_er1;
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
 TRANS yield_in_protect;
 AS_YIELD_STMT yield_in_protect1;
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
 TRANS yield_in_protect2;
 AS_YIELD_STMT yield_in_protect3;
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
 TRANS yield_in_par_for;
 AS_YIELD_STMT yield_in_par_for1;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 TRANS missing_yield_va;
 AS_YIELD_STMT missing_yield_va1;
 dTP missing_yield_va2;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 TRANS err_self4;
 STR err_s4;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val11;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val12;
 AM_YIELD_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_YIELD_STMT ret_val13;
 AM_YIELD_STMT r1;
 TRANS extra_yield_valu;
 AS_YIELD_STMT extra_yield_valu1;
 SIG extra_yield_valu2;
 TRANS err_loc_self5;
 dPROG_ERR err_loc_t5;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val14;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val15;
 TRANS err_self5;
 STR err_s5;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val16;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val17;
 AM_YIELD_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_YIELD_STMT ret_val18;
 AM_YIELD_STMT r2;
 BOOL L1;
 BOOL L21_;
 extern STR yields57855310;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR yields1884499596;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 extern STR yields1884499596;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 extern STR yields560152216;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR Ayield1696661686;
 dTP L16;
 extern STR mustbespecified;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 AM_YIELD_STMT L19;
 OB L20;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 BOOL L26;
 BOOL L271_;
 extern STR Noyiel2003661345;
 extern STR name18;
 BOOL L28;
 BOOL L29;
 extern STR name21;
 AM_YIELD_STMT L30;
 OB L31;
 INT L32;
 INT L331_;
 INT L34;
 INT L351_;
 BOOL L36;
 BOOL L371_;
 L1 = TRANS_900561657(self);
 L21_=!(L1); 
 if (L21_) {
  yield_in_rout_er = self;
  yield_in_rout_er1 = s;
  err_loc_self = yield_in_rout_er;
  err_loc_t = ((dPROG_ERR) yield_in_rout_er1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = yield_in_rout_er;
  err_s = ((STR) &yields57855310);
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
 if (ATTR(self,in_protect_body)) {
  yield_in_protect = self;
  yield_in_protect1 = s;
  err_loc_self1 = yield_in_protect;
  err_loc_t1 = ((dPROG_ERR) yield_in_protect1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = yield_in_protect;
  err_s1 = ((STR) &yields1884499596);
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
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if (ATTR(self,in_protect_then)) {
  yield_in_protect2 = self;
  yield_in_protect3 = s;
  err_loc_self2 = yield_in_protect2;
  err_loc_t2 = ((dPROG_ERR) yield_in_protect3);
  PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
  err_self2 = yield_in_protect2;
  err_s2 = ((STR) &yields1884499596);
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
 if (TRANS_2091440962(self)) {
  yield_in_par_for = self;
  yield_in_par_for1 = s;
  err_loc_self3 = yield_in_par_for;
  err_loc_t3 = ((dPROG_ERR) yield_in_par_for1);
  PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
  err_self3 = yield_in_par_for;
  err_s3 = ((STR) &yields560152216);
  if ((err_self3==((TRANS) NULL))) {
   L10 = TRUE;
  } else {
   L10 = (ATTR(err_self3,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L10) {
   L9 = TRUE;
  } else {
   L9 = (ATTR(ATTR(err_self3,cur_rout),sig1)==((SIG) NULL));
  }
  if (L9) {
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
  }
  else {
   plus_self6 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
   plus_sarg6 = ((STR) &name21);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = err_s3;
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s3 = ret_val8;
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if (TRANS_1229338887(self)) {
  L12 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
  L131_=!(L12); 
  L11 = L131_;
 } else {
  L11 = FALSE;
 }
 if (L11) {
  SATTR(ATTR(self,cur_se),has_yield_in_loc,TRUE);
 }
 rtp = ATTR(ATTR(ATTR(self,cur_rout),sig1),ret);
 if ((ATTR(s,val1)==((dAS_EXPR) NULL))) {
  L14 = (rtp==((dTP) NULL));
  L151_=!(L14); 
  if (L151_) {
   missing_yield_va = self;
   missing_yield_va1 = s;
   missing_yield_va2 = rtp;
   err_loc_self4 = missing_yield_va;
   err_loc_t4 = ((dPROG_ERR) missing_yield_va1);
   PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
   err_self4 = missing_yield_va;
   plus_self8 = ((STR) &Ayield1696661686);
   L16 = missing_yield_va2;
   plus_sarg8 = (*dTP_strrSTR[TAG(L16)])(L16);
   ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val9;
   plus_sarg9 = ((STR) &mustbespecified);
   ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
   err_s4 = ret_val10;
   if ((err_self4==((TRANS) NULL))) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(err_self4,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(ATTR(err_self4,cur_rout),sig1)==((SIG) NULL));
   }
   if (L17) {
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   else {
    plus_self10 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg10 = ((STR) &name21);
    ret_val11 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val11;
    plus_sarg11 = err_s4;
    ret_val12 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s4 = ret_val12;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  create_self = ((AM_YIELD_STMT) NULL);
  create_source = ATTR(s,source1);
  L20=ZALLOC(sizeof(struct AM_YIELD_STMT_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=AM_YIELD_STMT_tag;
  L19 = ((AM_YIELD_STMT) L20);
  r1 = L19;
  SATTR(r1,source1,create_source);
  ret_val13 = r1;
  y = ret_val13;
  L22 = ATTR(self,cur_yield_ind);
  L231_=INTPLUS(L22,1); 
  SATTR(self,cur_yield_ind,L231_);
  SATTR(y,ret,ATTR(self,cur_yield_ind));
  L24 = ATTR(ATTR(self,cur_rout),num_yields);
  L251_=INTPLUS(L24,1); 
  SATTR(ATTR(self,cur_rout),num_yields,L251_);
  L26 = (ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL));
  L271_=!(L26); 
  if (L271_) {
   SATTR(ATTR(self,cur_loop),has_yield,TRUE);
  }
  ret_val = ((dAM_STMT) y);
  return ret_val;
 }
 else {
  if ((rtp==((dTP) NULL))) {
   extra_yield_valu = self;
   extra_yield_valu1 = s;
   extra_yield_valu2 = ATTR(ATTR(self,cur_rout),sig1);
   err_loc_self5 = extra_yield_valu;
   err_loc_t5 = ((dPROG_ERR) extra_yield_valu1);
   PROG_e176405615(ATTR(err_loc_self5,prog), err_loc_t5);
   err_self5 = extra_yield_valu;
   plus_self12 = ((STR) &Noyiel2003661345);
   plus_sarg12 = ATTR(extra_yield_valu2,str);
   ret_val14 = STR_ap2004550586(plus_self12, plus_sarg12);
   plus_self13 = ret_val14;
   plus_sarg13 = ((STR) &name18);
   ret_val15 = STR_ap2004550586(plus_self13, plus_sarg13);
   err_s5 = ret_val15;
   if ((err_self5==((TRANS) NULL))) {
    L29 = TRUE;
   } else {
    L29 = (ATTR(err_self5,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L29) {
    L28 = TRUE;
   } else {
    L28 = (ATTR(ATTR(err_self5,cur_rout),sig1)==((SIG) NULL));
   }
   if (L28) {
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   else {
    plus_self14 = ATTR(ATTR(ATTR(err_self5,cur_rout),sig1),str);
    plus_sarg14 = ((STR) &name21);
    ret_val16 = STR_ap2004550586(plus_self14, plus_sarg14);
    plus_self15 = ret_val16;
    plus_sarg15 = err_s5;
    ret_val17 = STR_ap2004550586(plus_self15, plus_sarg15);
    err_s5 = ret_val17;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  create_self1 = ((AM_YIELD_STMT) NULL);
  create_source1 = ATTR(s,source1);
  L31=ZALLOC(sizeof(struct AM_YIELD_STMT_struct));
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=AM_YIELD_STMT_tag;
  L30 = ((AM_YIELD_STMT) L31);
  r2 = L30;
  SATTR(r2,source1,create_source1);
  ret_val18 = r2;
  r = ret_val18;
  SATTR(r,val1,TRANS_693526804(self, ATTR(s,val1), rtp));
  if ((ATTR(r,val1)==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  L32 = ATTR(self,cur_yield_ind);
  L331_=INTPLUS(L32,1); 
  SATTR(self,cur_yield_ind,L331_);
  SATTR(r,ret,ATTR(self,cur_yield_ind));
  L34 = ATTR(ATTR(self,cur_rout),num_yields);
  L351_=INTPLUS(L34,1); 
  SATTR(ATTR(self,cur_rout),num_yields,L351_);
  L36 = (ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL));
  L371_=!(L36); 
  if (L371_) {
   SATTR(ATTR(self,cur_loop),has_yield,TRUE);
  }
  ret_val = ((dAM_STMT) r);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1157876950(TRANS self, AS_ASSIGN_STMT s) {
 dAM_STMT ret_val;
 dAM_STMT r;
 dAS_EXPR lhs;
 dAM_STMT r1;
 dAM_STMT r2;
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
 TRANS err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 TRANS err_self4;
 STR err_s4;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 TRANS err_self5;
 STR err_s5;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val11;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val12;
 TRANS err_self6;
 STR err_s6;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val13;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val14;
 TRANS err_self7;
 STR err_s7;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val15;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val16;
 TRANS err_self8;
 STR err_s8;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val17;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val18;
 TRANS err_self9;
 STR err_s9;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val19;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val20;
 TRANS err_self10;
 STR err_s10;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val21;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val22;
 TRANS err_self11;
 STR err_s11;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val23;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val24;
 TRANS err_self12;
 STR err_s12;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val25;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val26;
 TRANS err_self13;
 STR err_s13;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val27;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val28;
 TRANS err_self14;
 STR err_s14;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val29;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val30;
 TRANS err_self15;
 STR err_s15;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val31;
 STR plus_self31;
 STR plus_sarg31;
 STR ret_val32;
 TRANS err_self16;
 STR err_s16;
 STR plus_self32;
 STR plus_sarg32;
 STR ret_val33;
 STR plus_self33;
 STR plus_sarg33;
 STR ret_val34;
 TRANS err_self17;
 STR err_s17;
 STR plus_self34;
 STR plus_sarg34;
 STR ret_val35;
 STR plus_self35;
 STR plus_sarg35;
 STR ret_val36;
 TRANS err_self18;
 STR err_s18;
 STR plus_self36;
 STR plus_sarg36;
 STR ret_val37;
 STR plus_self37;
 STR plus_sarg37;
 STR ret_val38;
 TRANS err_self19;
 STR err_s19;
 STR plus_self38;
 STR plus_sarg38;
 STR ret_val39;
 STR plus_self39;
 STR plus_sarg39;
 STR ret_val40;
 TRANS err_self20;
 STR err_s20;
 STR plus_self40;
 STR plus_sarg40;
 STR ret_val41;
 STR plus_self41;
 STR plus_sarg41;
 STR ret_val42;
 PROG psather_self;
 BOOL ret_val43 = BOOL_zero;
 extern STR Itisil1956015094;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 extern STR Itisil1956473526;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 extern STR Itisil1408694951;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 extern STR Itisil578632473;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 extern STR Itisil9134278;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 extern STR Itisil2092671248;
 BOOL L11;
 BOOL L12;
 extern STR name21;
 extern STR Itisil2136244190;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 extern STR Itisil258160423;
 BOOL L15;
 BOOL L16;
 extern STR name21;
 extern STR Itisil815302075;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 extern STR Itisil333018310;
 BOOL L19;
 BOOL L20;
 extern STR name21;
 extern STR Itisil1384984645;
 BOOL L21;
 BOOL L22;
 extern STR name21;
 extern STR Itisil1353104731;
 BOOL L23;
 BOOL L24;
 extern STR name21;
 extern STR Itisil1603144605;
 BOOL L25;
 BOOL L26;
 extern STR name21;
 extern STR Itisil1717990552;
 BOOL L27;
 BOOL L28;
 extern STR name21;
 extern STR Itisil1097711340;
 BOOL L29;
 BOOL L30;
 extern STR name21;
 extern STR Itisil1921741040;
 BOOL L31;
 BOOL L32;
 extern STR name21;
 extern STR Itisil87353010;
 BOOL L33;
 BOOL L34;
 extern STR name21;
 extern STR Itisil598792827;
 BOOL L35;
 BOOL L36;
 extern STR name21;
 extern STR Itisil173827897;
 BOOL L37;
 BOOL L38;
 extern STR name21;
 extern STR Itisil1765426125;
 BOOL L39;
 BOOL L40;
 extern STR name21;
 extern STR Itisil515121697;
 BOOL L41;
 BOOL L42;
 extern STR name21;
 BOOL L43;
 if ((ATTR(s,lhs_expr)==((dAS_EXPR) NULL))) {
  r = TRANS_1920518219(self, s);
  ret_val = r;
  return ret_val;
 }
 lhs = ATTR(s,lhs_expr);
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) lhs);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 if (lhs==NULL) {
  goto other427;
 } else
 switch (TAG(lhs)) {
  case AS_CALL_EXPR_tag:
   if (ATTR(((AS_CALL_EXPR) lhs),is_array)) {
    r1 = TRANS_1207936294(self, ((AS_CALL_EXPR) lhs), s);
    ret_val = r1;
    return ret_val;
   }
   else {
    r2 = TRANS_471579016(self, ((AS_CALL_EXPR) lhs), s);
    ret_val = r2;
    return ret_val;
   } break;
  case AS_SELF_EXPR_tag:
   err_self = self;
   err_s = ((STR) &Itisil1956015094);
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
   } break;
  case AS_VOID_EXPR_tag:
   err_self1 = self;
   err_s1 = ((STR) &Itisil1956473526);
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
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    plus_sarg3 = err_s1;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    err_s1 = ret_val4;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   } break;
  case AS_IS_VOID_EXPR_tag:
   err_self2 = self;
   err_s2 = ((STR) &Itisil1408694951);
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
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val5;
    plus_sarg5 = err_s2;
    ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
    err_s2 = ret_val6;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   } break;
  case AS_ARRAY_EXPR_tag:
   err_self3 = self;
   err_s3 = ((STR) &Itisil578632473);
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
    ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val7;
    plus_sarg7 = err_s3;
    ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
    err_s3 = ret_val8;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   } break;
  case AS_CREATE_EXPR_tag:
   err_self4 = self;
   err_s4 = ((STR) &Itisil9134278);
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
    ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val9;
    plus_sarg9 = err_s4;
    ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s4 = ret_val10;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   } break;
  case AS_BOU14269336_tag:
   err_self5 = self;
   err_s5 = ((STR) &Itisil2092671248);
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
    ret_val11 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val11;
    plus_sarg11 = err_s5;
    ret_val12 = STR_ap2004550586(plus_self11, plus_sarg11);
    err_s5 = ret_val12;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   } break;
  case AS_AND_EXPR_tag:
   err_self6 = self;
   err_s6 = ((STR) &Itisil2136244190);
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
    ret_val13 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val13;
    plus_sarg13 = err_s6;
    ret_val14 = STR_ap2004550586(plus_self13, plus_sarg13);
    err_s6 = ret_val14;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   } break;
  case AS_OR_EXPR_tag:
   err_self7 = self;
   err_s7 = ((STR) &Itisil258160423);
   if ((err_self7==((TRANS) NULL))) {
    L16 = TRUE;
   } else {
    L16 = (ATTR(err_self7,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L16) {
    L15 = TRUE;
   } else {
    L15 = (ATTR(ATTR(err_self7,cur_rout),sig1)==((SIG) NULL));
   }
   if (L15) {
    PROG_err_STR(ATTR(err_self7,prog), err_s7);
   }
   else {
    plus_self14 = ATTR(ATTR(ATTR(err_self7,cur_rout),sig1),str);
    plus_sarg14 = ((STR) &name21);
    ret_val15 = STR_ap2004550586(plus_self14, plus_sarg14);
    plus_self15 = ret_val15;
    plus_sarg15 = err_s7;
    ret_val16 = STR_ap2004550586(plus_self15, plus_sarg15);
    err_s7 = ret_val16;
    PROG_err_STR(ATTR(err_self7,prog), err_s7);
   } break;
  case AS_EXCEPT_EXPR_tag:
   err_self8 = self;
   err_s8 = ((STR) &Itisil815302075);
   if ((err_self8==((TRANS) NULL))) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(err_self8,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(ATTR(err_self8,cur_rout),sig1)==((SIG) NULL));
   }
   if (L17) {
    PROG_err_STR(ATTR(err_self8,prog), err_s8);
   }
   else {
    plus_self16 = ATTR(ATTR(ATTR(err_self8,cur_rout),sig1),str);
    plus_sarg16 = ((STR) &name21);
    ret_val17 = STR_ap2004550586(plus_self16, plus_sarg16);
    plus_self17 = ret_val17;
    plus_sarg17 = err_s8;
    ret_val18 = STR_ap2004550586(plus_self17, plus_sarg17);
    err_s8 = ret_val18;
    PROG_err_STR(ATTR(err_self8,prog), err_s8);
   } break;
  case AS_NEW_EXPR_tag:
   err_self9 = self;
   err_s9 = ((STR) &Itisil333018310);
   if ((err_self9==((TRANS) NULL))) {
    L20 = TRUE;
   } else {
    L20 = (ATTR(err_self9,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L19 = (ATTR(ATTR(err_self9,cur_rout),sig1)==((SIG) NULL));
   }
   if (L19) {
    PROG_err_STR(ATTR(err_self9,prog), err_s9);
   }
   else {
    plus_self18 = ATTR(ATTR(ATTR(err_self9,cur_rout),sig1),str);
    plus_sarg18 = ((STR) &name21);
    ret_val19 = STR_ap2004550586(plus_self18, plus_sarg18);
    plus_self19 = ret_val19;
    plus_sarg19 = err_s9;
    ret_val20 = STR_ap2004550586(plus_self19, plus_sarg19);
    err_s9 = ret_val20;
    PROG_err_STR(ATTR(err_self9,prog), err_s9);
   } break;
  case AS_INITIAL_EXPR_tag:
   err_self10 = self;
   err_s10 = ((STR) &Itisil1384984645);
   if ((err_self10==((TRANS) NULL))) {
    L22 = TRUE;
   } else {
    L22 = (ATTR(err_self10,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L22) {
    L21 = TRUE;
   } else {
    L21 = (ATTR(ATTR(err_self10,cur_rout),sig1)==((SIG) NULL));
   }
   if (L21) {
    PROG_err_STR(ATTR(err_self10,prog), err_s10);
   }
   else {
    plus_self20 = ATTR(ATTR(ATTR(err_self10,cur_rout),sig1),str);
    plus_sarg20 = ((STR) &name21);
    ret_val21 = STR_ap2004550586(plus_self20, plus_sarg20);
    plus_self21 = ret_val21;
    plus_sarg21 = err_s10;
    ret_val22 = STR_ap2004550586(plus_self21, plus_sarg21);
    err_s10 = ret_val22;
    PROG_err_STR(ATTR(err_self10,prog), err_s10);
   } break;
  case AS_BREAK_EXPR_tag:
   err_self11 = self;
   err_s11 = ((STR) &Itisil1353104731);
   if ((err_self11==((TRANS) NULL))) {
    L24 = TRUE;
   } else {
    L24 = (ATTR(err_self11,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L24) {
    L23 = TRUE;
   } else {
    L23 = (ATTR(ATTR(err_self11,cur_rout),sig1)==((SIG) NULL));
   }
   if (L23) {
    PROG_err_STR(ATTR(err_self11,prog), err_s11);
   }
   else {
    plus_self22 = ATTR(ATTR(ATTR(err_self11,cur_rout),sig1),str);
    plus_sarg22 = ((STR) &name21);
    ret_val23 = STR_ap2004550586(plus_self22, plus_sarg22);
    plus_self23 = ret_val23;
    plus_sarg23 = err_s11;
    ret_val24 = STR_ap2004550586(plus_self23, plus_sarg23);
    err_s11 = ret_val24;
    PROG_err_STR(ATTR(err_self11,prog), err_s11);
   } break;
  case AS_RESULT_EXPR_tag:
   err_self12 = self;
   err_s12 = ((STR) &Itisil1603144605);
   if ((err_self12==((TRANS) NULL))) {
    L26 = TRUE;
   } else {
    L26 = (ATTR(err_self12,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L26) {
    L25 = TRUE;
   } else {
    L25 = (ATTR(ATTR(err_self12,cur_rout),sig1)==((SIG) NULL));
   }
   if (L25) {
    PROG_err_STR(ATTR(err_self12,prog), err_s12);
   }
   else {
    plus_self24 = ATTR(ATTR(ATTR(err_self12,cur_rout),sig1),str);
    plus_sarg24 = ((STR) &name21);
    ret_val25 = STR_ap2004550586(plus_self24, plus_sarg24);
    plus_self25 = ret_val25;
    plus_sarg25 = err_s12;
    ret_val26 = STR_ap2004550586(plus_self25, plus_sarg25);
    err_s12 = ret_val26;
    PROG_err_STR(ATTR(err_self12,prog), err_s12);
   } break;
  case AS_BOOL_LIT_EXPR_tag:
   err_self13 = self;
   err_s13 = ((STR) &Itisil1717990552);
   if ((err_self13==((TRANS) NULL))) {
    L28 = TRUE;
   } else {
    L28 = (ATTR(err_self13,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L28) {
    L27 = TRUE;
   } else {
    L27 = (ATTR(ATTR(err_self13,cur_rout),sig1)==((SIG) NULL));
   }
   if (L27) {
    PROG_err_STR(ATTR(err_self13,prog), err_s13);
   }
   else {
    plus_self26 = ATTR(ATTR(ATTR(err_self13,cur_rout),sig1),str);
    plus_sarg26 = ((STR) &name21);
    ret_val27 = STR_ap2004550586(plus_self26, plus_sarg26);
    plus_self27 = ret_val27;
    plus_sarg27 = err_s13;
    ret_val28 = STR_ap2004550586(plus_self27, plus_sarg27);
    err_s13 = ret_val28;
    PROG_err_STR(ATTR(err_self13,prog), err_s13);
   } break;
  case AS_CHAR_LIT_EXPR_tag:
   err_self14 = self;
   err_s14 = ((STR) &Itisil1097711340);
   if ((err_self14==((TRANS) NULL))) {
    L30 = TRUE;
   } else {
    L30 = (ATTR(err_self14,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L30) {
    L29 = TRUE;
   } else {
    L29 = (ATTR(ATTR(err_self14,cur_rout),sig1)==((SIG) NULL));
   }
   if (L29) {
    PROG_err_STR(ATTR(err_self14,prog), err_s14);
   }
   else {
    plus_self28 = ATTR(ATTR(ATTR(err_self14,cur_rout),sig1),str);
    plus_sarg28 = ((STR) &name21);
    ret_val29 = STR_ap2004550586(plus_self28, plus_sarg28);
    plus_self29 = ret_val29;
    plus_sarg29 = err_s14;
    ret_val30 = STR_ap2004550586(plus_self29, plus_sarg29);
    err_s14 = ret_val30;
    PROG_err_STR(ATTR(err_self14,prog), err_s14);
   } break;
  case AS_STR_LIT_EXPR_tag:
   err_self15 = self;
   err_s15 = ((STR) &Itisil1921741040);
   if ((err_self15==((TRANS) NULL))) {
    L32 = TRUE;
   } else {
    L32 = (ATTR(err_self15,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L32) {
    L31 = TRUE;
   } else {
    L31 = (ATTR(ATTR(err_self15,cur_rout),sig1)==((SIG) NULL));
   }
   if (L31) {
    PROG_err_STR(ATTR(err_self15,prog), err_s15);
   }
   else {
    plus_self30 = ATTR(ATTR(ATTR(err_self15,cur_rout),sig1),str);
    plus_sarg30 = ((STR) &name21);
    ret_val31 = STR_ap2004550586(plus_self30, plus_sarg30);
    plus_self31 = ret_val31;
    plus_sarg31 = err_s15;
    ret_val32 = STR_ap2004550586(plus_self31, plus_sarg31);
    err_s15 = ret_val32;
    PROG_err_STR(ATTR(err_self15,prog), err_s15);
   } break;
  case AS_INT_LIT_EXPR_tag:
   err_self16 = self;
   err_s16 = ((STR) &Itisil87353010);
   if ((err_self16==((TRANS) NULL))) {
    L34 = TRUE;
   } else {
    L34 = (ATTR(err_self16,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L34) {
    L33 = TRUE;
   } else {
    L33 = (ATTR(ATTR(err_self16,cur_rout),sig1)==((SIG) NULL));
   }
   if (L33) {
    PROG_err_STR(ATTR(err_self16,prog), err_s16);
   }
   else {
    plus_self32 = ATTR(ATTR(ATTR(err_self16,cur_rout),sig1),str);
    plus_sarg32 = ((STR) &name21);
    ret_val33 = STR_ap2004550586(plus_self32, plus_sarg32);
    plus_self33 = ret_val33;
    plus_sarg33 = err_s16;
    ret_val34 = STR_ap2004550586(plus_self33, plus_sarg33);
    err_s16 = ret_val34;
    PROG_err_STR(ATTR(err_self16,prog), err_s16);
   } break;
  case AS_FLT_LIT_EXPR_tag:
   err_self17 = self;
   err_s17 = ((STR) &Itisil598792827);
   if ((err_self17==((TRANS) NULL))) {
    L36 = TRUE;
   } else {
    L36 = (ATTR(err_self17,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L36) {
    L35 = TRUE;
   } else {
    L35 = (ATTR(ATTR(err_self17,cur_rout),sig1)==((SIG) NULL));
   }
   if (L35) {
    PROG_err_STR(ATTR(err_self17,prog), err_s17);
   }
   else {
    plus_self34 = ATTR(ATTR(ATTR(err_self17,cur_rout),sig1),str);
    plus_sarg34 = ((STR) &name21);
    ret_val35 = STR_ap2004550586(plus_self34, plus_sarg34);
    plus_self35 = ret_val35;
    plus_sarg35 = err_s17;
    ret_val36 = STR_ap2004550586(plus_self35, plus_sarg35);
    err_s17 = ret_val36;
    PROG_err_STR(ATTR(err_self17,prog), err_s17);
   } break;
  case AS_CLUSTER_EXPR_tag:
   err_self18 = self;
   err_s18 = ((STR) &Itisil173827897);
   if ((err_self18==((TRANS) NULL))) {
    L38 = TRUE;
   } else {
    L38 = (ATTR(err_self18,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L38) {
    L37 = TRUE;
   } else {
    L37 = (ATTR(ATTR(err_self18,cur_rout),sig1)==((SIG) NULL));
   }
   if (L37) {
    PROG_err_STR(ATTR(err_self18,prog), err_s18);
   }
   else {
    plus_self36 = ATTR(ATTR(ATTR(err_self18,cur_rout),sig1),str);
    plus_sarg36 = ((STR) &name21);
    ret_val37 = STR_ap2004550586(plus_self36, plus_sarg36);
    plus_self37 = ret_val37;
    plus_sarg37 = err_s18;
    ret_val38 = STR_ap2004550586(plus_self37, plus_sarg37);
    err_s18 = ret_val38;
    PROG_err_STR(ATTR(err_self18,prog), err_s18);
   } break;
  case AS_CLU553570599_tag:
   err_self19 = self;
   err_s19 = ((STR) &Itisil1765426125);
   if ((err_self19==((TRANS) NULL))) {
    L40 = TRUE;
   } else {
    L40 = (ATTR(err_self19,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L40) {
    L39 = TRUE;
   } else {
    L39 = (ATTR(ATTR(err_self19,cur_rout),sig1)==((SIG) NULL));
   }
   if (L39) {
    PROG_err_STR(ATTR(err_self19,prog), err_s19);
   }
   else {
    plus_self38 = ATTR(ATTR(ATTR(err_self19,cur_rout),sig1),str);
    plus_sarg38 = ((STR) &name21);
    ret_val39 = STR_ap2004550586(plus_self38, plus_sarg38);
    plus_self39 = ret_val39;
    plus_sarg39 = err_s19;
    ret_val40 = STR_ap2004550586(plus_self39, plus_sarg39);
    err_s19 = ret_val40;
    PROG_err_STR(ATTR(err_self19,prog), err_s19);
   } break;
  case AS_GLOBAL_EXPR_tag:
   err_self20 = self;
   err_s20 = ((STR) &Itisil515121697);
   if ((err_self20==((TRANS) NULL))) {
    L42 = TRUE;
   } else {
    L42 = (ATTR(err_self20,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L42) {
    L41 = TRUE;
   } else {
    L41 = (ATTR(ATTR(err_self20,cur_rout),sig1)==((SIG) NULL));
   }
   if (L41) {
    PROG_err_STR(ATTR(err_self20,prog), err_s20);
   }
   else {
    plus_self40 = ATTR(ATTR(ATTR(err_self20,cur_rout),sig1),str);
    plus_sarg40 = ((STR) &name21);
    ret_val41 = STR_ap2004550586(plus_self40, plus_sarg40);
    plus_self41 = ret_val41;
    plus_sarg41 = err_s20;
    ret_val42 = STR_ap2004550586(plus_self41, plus_sarg41);
    err_s20 = ret_val42;
    PROG_err_STR(ATTR(err_self20,prog), err_s20);
   } break;
  default: ;
  other427: ;
   psather_self = ATTR(self,prog);
   if (ATTR(psather_self,threads)) {
    L43 = TRUE;
   } else {
    L43 = ATTR(psather_self,distributed);
   }
   ret_val43 = L43;
   if (ret_val43) {
    TRANS_548661555(self, s);
   }
 }
 ret_val = ((dAM_STMT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1226584966(TRANS self, AS_ASSERT_STMT s) {
 dAM_STMT ret_val;
 AM_ASSERT_STMT r;
 AM_ASSERT_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSERT_STMT ret_val1;
 AM_ASSERT_STMT r1;
 BOOL L1;
 BOOL L21_;
 AM_ASSERT_STMT L3;
 OB L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 L1 = ATTR(self,chk_assert);
 L21_=!(L1); 
 if (L21_) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_ASSERT_STMT) NULL);
 create_source = ATTR(s,source1);
 L4=ZALLOC(sizeof(struct AM_ASSERT_STMT_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_ASSERT_STMT_tag;
 L3 = ((AM_ASSERT_STMT) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,test1,TRANS_693526804(self, ATTR(s,test1), ((dTP) TP_BUILTIN_bool)));
 if ((ATTR(r,test1)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if (ATTR(ATTR(self,prog),assert_checks)) {
  L6 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
  L71_=!(L6); 
  L5 = L71_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1261916410(TRANS self, AS_IF_STMT s) {
 dAM_STMT ret_val;
 AM_IF_STMT r;
 dAS_STMT sys_imp;
 dAS_STMT sys_imp1;
 AM_IF_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_STMT ret_val1;
 AM_IF_STMT r1;
 PROG psather_self;
 BOOL ret_val2 = BOOL_zero;
 AM_ROUT_DEF import_locals_pe;
 BOOL ret_val3 = BOOL_zero;
 TRANS insert_import_se;
 AS_STMT_LIST insert_import_l;
 SFILE_ID insert_import_so = SFILE_ID_zero;
 dAS_STMT import11 = ((dAS_STMT) NULL);
 TRANS insert_import_se1;
 AS_STMT_LIST insert_import_l1;
 SFILE_ID insert_import_so1 = SFILE_ID_zero;
 dAS_STMT import2 = ((dAS_STMT) NULL);
 AM_ROUT_DEF import_call_pend;
 BOOL ret_val4 = BOOL_zero;
 AM_ROUT_DEF import_done_self;
 AM_IF_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 dAS_STMT L12;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 dAS_STMT L19;
 BOOL L20;
 BOOL L211_;
 dAS_STMT L22;
 dAS_STMT L23;
 BOOL L24;
 BOOL L251_;
 dAS_STMT L26;
 dAS_STMT L27;
 create_self = ((AM_IF_STMT) NULL);
 create_source = ATTR(s,source1);
 L2=ZALLOC(sizeof(struct AM_IF_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_IF_STMT_tag;
 L1 = ((AM_IF_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,test1,TRANS_693526804(self, ATTR(s,test1), ((dTP) TP_BUILTIN_bool)));
 if ((ATTR(r,test1)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L3 = TRUE;
 } else {
  L3 = ATTR(psather_self,distributed);
 }
 ret_val2 = L3;
 if (ret_val2) {
  L4 = ATTR(s,transformed);
  L51_=!(L4); 
  if (L51_) {
   import_locals_pe = ATTR(self,cur_rout);
   ret_val3 = ATTR(import_locals_pe,n_import_locals);
   if (ret_val3) {
    L6 = (ATTR(s,then_part)==((AS_STMT_LIST) NULL));
    L71_=!(L6); 
    if (L71_) {
     insert_import_se = self;
     insert_import_l = ATTR(s,then_part);
     insert_import_so = ATTR(s,source1);
     if (TRANS_1975351488) {
      import11 = TRANS_120708644(insert_import_se, TRANS_1924362320, insert_import_l, insert_import_so);
     }
     L8 = (import11==((dAS_STMT) NULL));
     L91_=!(L8); 
     if (L91_) {
      L10 = (insert_import_l==((AS_STMT_LIST) NULL));
      L111_=!(L10); 
      if (L111_) {
       L12 = import11;
       (*dAS_ST1454156204[TAG(L12)])(L12, ATTR(insert_import_l,stmts));
      }
      SATTR(insert_import_l,stmts,import11);
     }
    }
    L13 = (ATTR(s,else_part)==((AS_STMT_LIST) NULL));
    L141_=!(L13); 
    if (L141_) {
     insert_import_se1 = self;
     insert_import_l1 = ATTR(s,else_part);
     insert_import_so1 = ATTR(s,source1);
     if (TRANS_1975351488) {
      import2 = TRANS_120708644(insert_import_se1, TRANS_1924362320, insert_import_l1, insert_import_so1);
     }
     L15 = (import2==((dAS_STMT) NULL));
     L161_=!(L15); 
     if (L161_) {
      L17 = (insert_import_l1==((AS_STMT_LIST) NULL));
      L181_=!(L17); 
      if (L181_) {
       L19 = import2;
       (*dAS_ST1454156204[TAG(L19)])(L19, ATTR(insert_import_l1,stmts));
      }
      SATTR(insert_import_l1,stmts,import2);
     }
    }
   }
   import_call_pend = ATTR(self,cur_rout);
   ret_val4 = ATTR(import_call_pend,n_import_call);
   if (ret_val4) {
    L20 = (ATTR(s,then_part)==((AS_STMT_LIST) NULL));
    L211_=!(L20); 
    if (L211_) {
     sys_imp = TRANS_166911016(self, TRANS_1924362320, ATTR(s,then_part), ATTR(s,source1));
     L22 = sys_imp;
     (*dAS_ST1569642444[TAG(L22)])(L22, TRUE);
     L23 = sys_imp;
     (*dAS_ST1454156204[TAG(L23)])(L23, ATTR(ATTR(s,then_part),stmts));
     SATTR(ATTR(s,then_part),stmts,sys_imp);
    }
    L24 = (ATTR(s,else_part)==((AS_STMT_LIST) NULL));
    L251_=!(L24); 
    if (L251_) {
     sys_imp1 = TRANS_166911016(self, TRANS_1924362320, ATTR(s,else_part), ATTR(s,source1));
     L26 = sys_imp1;
     (*dAS_ST1569642444[TAG(L26)])(L26, TRUE);
     L27 = sys_imp1;
     (*dAS_ST1454156204[TAG(L27)])(L27, ATTR(ATTR(s,else_part),stmts));
     SATTR(ATTR(s,else_part),stmts,sys_imp1);
    }
   }
  }
  import_done_self = ATTR(self,cur_rout);
  SATTR(import_done_self,n_import_locals,FALSE);
  SATTR(import_done_self,n_import_call,FALSE);
 }
 SATTR(r,if_true,TRANS_1563748848(self, ATTR(s,then_part)));
 SATTR(r,if_false,TRANS_1563748848(self, ATTR(s,else_part)));
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1630043848(TRANS self, AS_CASE_STMT s, AS_CASE_WHEN cw, AM_LOCAL_EXPR l) {
 dAM_STMT ret_val;
 AM_CASE_STMT r;
 dAM_STMT r1;
 dCALL_TP ct;
 dAM_EXPR v = ((dAM_EXPR) NULL);
 dAM_CONST cv;
 AM_CASE_STMT r2;
 dAS_STMT last_then;
 FLISTdAM_CONST ls = ((FLISTdAM_CONST) NULL);
 CALL_SIG cs;
 SIG sig1;
 AM_ROU1916046290 arc;
 AM_IF_STMT r3;
 AM_CASE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_CASE_STMT ret_val1;
 AM_CASE_STMT r4;
 TRANS err_loc_self;
 dPROG_ERR err_loc_t;
 AM_CASE_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_CASE_STMT ret_val2;
 AM_CASE_STMT r5;
 CALL_SIG create_self2;
 CALL_SIG ret_val3;
 ARRAYCALL_ARG create_self3;
 INT create_n = INT_zero;
 ARRAYCALL_ARG ret_val4;
 CALL_ARG create_self4;
 dCALL_TP create_t;
 CALL_ARG ret_val5;
 CALL_ARG res;
 CALL_ARG create_self5;
 dCALL_TP create_t1;
 CALL_ARG ret_val6;
 CALL_ARG res1;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val7 = BOOL_zero;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val8;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val9;
 AM_ROU1916046290 create_self6;
 INT create_nargs = INT_zero;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val10;
 AM_ROU1916046290 r6;
 AM_CALL_ARG create_self7;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val11;
 AM_CALL_ARG res2;
 AM_CALL_ARG create_self8;
 dAM_EXPR create_e1;
 AM_CALL_ARG ret_val12;
 AM_CALL_ARG res3;
 AM_IF_STMT create_self9;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_IF_STMT ret_val13;
 AM_IF_STMT r7;
 TRANS special_inline_s;
 AM_ROU1916046290 special_inline_c;
 dAM_EXPR ret_val14;
 dAM_EXPR am;
 AM_CASE_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 AM_CASE_STMT L5;
 OB L6;
 FLISTdAM_STMT L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 FLISTdAM_STMT L12;
 CALL_SIG L13;
 OB L14;
 ARRAYCALL_ARG L15;
 INT L16;
 OB L17;
 INT L18;
 BOOL L19;
 BOOL L201_;
 ARRAYCALL_ARG L21;
 CALL_ARG L22;
 OB L23;
 CALL_ARG L24;
 ARRAYCALL_ARG L26;
 dAM_EXPR L27;
 CALL_ARG L28;
 OB L29;
 CALL_ARG L30;
 dTP L32;
 BOOL L33;
 BOOL L341_;
 extern STR Theis_1675105926;
 BOOL L35;
 BOOL L36;
 extern STR name21;
 ARRAYARG L37;
 ARG L381_;
 ARG L39;
 AM_ROU1916046290 L40;
 INT L42;
 OB L43;
 INT L44;
 AM_CALL_ARG L45;
 OB L46;
 AM_CALL_ARG L47;
 AM_CALL_ARG L49;
 OB L50;
 AM_CALL_ARG L51;
 AM_IF_STMT L53;
 OB L54;
 BOOL L55;
 BOOL L561_;
 dGENERATE_AM L57;
 dINLINE L58;
 if ((cw==((AS_CASE_WHEN) NULL))) {
  if (ATTR(s,no_else)) {
   create_self = ((AM_CASE_STMT) NULL);
   create_source = ATTR(s,source1);
   L2=ZALLOC(sizeof(struct AM_CASE_STMT_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=AM_CASE_STMT_tag;
   L1 = ((AM_CASE_STMT) L2);
   r4 = L1;
   SATTR(r4,source1,create_source);
   ret_val1 = r4;
   r = ret_val1;
   SATTR(r,test1,((dAM_EXPR) l));
   SATTR(r,no_else,TRUE);
   ret_val = ((dAM_STMT) r);
   return ret_val;
  }
  else {
   r1 = TRANS_1563748848(self, ATTR(s,else_part));
   ret_val = r1;
   return ret_val;
  }
 }
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) cw);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 ct = TRANS_68137339(self, ATTR(cw,val1));
 if ((ct==((dCALL_TP) NULL))) {
  v = TRANS_693526804(self, ATTR(cw,val1), ((dTP) NULL));
  if ((v==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  cv = TRANS_1139222136(self, v);
  L3 = (cv==((dAM_CONST) NULL));
  L41_=!(L3); 
  if (L41_) {
   create_self1 = ((AM_CASE_STMT) NULL);
   create_source1 = ATTR(cw,source1);
   L6=ZALLOC(sizeof(struct AM_CASE_STMT_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AM_CASE_STMT_tag;
   L5 = ((AM_CASE_STMT) L6);
   r5 = L5;
   SATTR(r5,source1,create_source1);
   ret_val2 = r5;
   r2 = ret_val2;
   SATTR(r2,test1,((dAM_EXPR) l));
   last_then = ATTR(ATTR(cw,then_part),stmts);
   ls = FLISTd246699990(ls, cv);
   SATTR(r2,tgts,FLISTF1203220157(ATTR(r2,tgts), ls));
   L7 = ATTR(r2,stmts);
   SATTR(r2,stmts,FLISTd506668277(L7, TRANS_1563748848(self, ATTR(cw,then_part))));
   while (1) {
    cw = ATTR(cw,next);
    if ((cw==((AS_CASE_WHEN) NULL))) {
     if (ATTR(s,no_else)) {
      SATTR(r2,no_else,TRUE);
     }
     else {
      SATTR(r2,else_stmts,TRANS_1563748848(self, ATTR(s,else_part)));
     }
     ret_val = ((dAM_STMT) r2);
     return ret_val;
    }
    L8 = (TRANS_68137339(self, ATTR(cw,val1))==((dCALL_TP) NULL));
    L91_=!(L8); 
    if (L91_) {
     SATTR(r2,else_stmts,TRANS_1630043848(self, s, cw, l));
     ret_val = ((dAM_STMT) r2);
     return ret_val;
    }
    v = TRANS_693526804(self, ATTR(cw,val1), ((dTP) NULL));
    if ((v==((dAM_EXPR) NULL))) {
     ret_val = ((dAM_STMT) NULL);
     return ret_val;
    }
    cv = TRANS_1139222136(self, v);
    L10 = (cv==((dAM_CONST) NULL));
    L111_=!(L10); 
    if (L111_) {
     if (SYSOBEQ(((OB) last_then),((OB) ATTR(cw,then_part)))) {
      ls = FLISTF471519751(ATTR(r2,tgts));
      ls = FLISTd246699990(ls, cv);
      SATTR(r2,tgts,FLISTF1203220157(ATTR(r2,tgts), ls));
     }
     else {
      ls = ((FLISTdAM_CONST) NULL);
      ls = FLISTd246699990(ls, cv);
      SATTR(r2,tgts,FLISTF1203220157(ATTR(r2,tgts), ls));
      L12 = ATTR(r2,stmts);
      SATTR(r2,stmts,FLISTd506668277(L12, TRANS_1563748848(self, ATTR(cw,then_part))));
     }
    }
    else {
     SATTR(r2,else_stmts,TRANS_1630043848(self, s, cw, l));
     ret_val = ((dAM_STMT) r2);
     return ret_val;
    }
   }
   after_loop: ;
  }
 }
 create_self2 = ((CALL_SIG) NULL);
 L14=ZALLOC(sizeof(struct CALL_SIG_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=CALL_SIG_tag;
 L13 = ((CALL_SIG) L14);
 ret_val3 = L13;
 cs = ret_val3;
 SATTR(cs,tp,ATTR(l,tp_at));
 SATTR(cs,name1,IDENT_1728630987);
 SATTR(cs,has_ret,TRUE);
 create_self3 = ((ARRAYCALL_ARG) NULL);
 create_n = 1;
 L16 = create_n;
 L18=(sizeof(struct ARRAYCALL_ARG_struct)-sizeof(CALL_ARG))+(L16)*sizeof(CALL_ARG);
 L17=ZALLOC_BIG(L18);
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=ARRAYCALL_ARG_tag;
#ifdef DESTROY_CHK

   ((OB)L17)->header.destroyed=0;
#endif

 L15 = ((ARRAYCALL_ARG) L17);
 L15->asize = L16;
 ret_val4 = L15;
 SATTR(cs,args,ret_val4);
 L19 = (ct==((dCALL_TP) NULL));
 L201_=!(L19); 
 if (L201_) {
  L21 = ATTR(cs,args);
  create_self4 = ((CALL_ARG) NULL);
  create_t = ct;
  L23=ZALLOC(sizeof(struct CALL_ARG_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=CALL_ARG_tag;
  L22 = ((CALL_ARG) L23);
  res = L22;
  SATTR(res,tp,create_t);
  SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val5 = res;
  L24 = ret_val5;
  SARR((ARRAYCALL_ARG)L21,0,(CALL_ARG)L24); 
  ;
 }
 else {
  L26 = ATTR(cs,args);
  create_self5 = ((CALL_ARG) NULL);
  L27 = v;
  create_t1 = ((dCALL_TP) (*dAM_EXPR_tprdTP[TAG(L27)])(L27));
  L29=ZALLOC(sizeof(struct CALL_ARG_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=CALL_ARG_tag;
  L28 = ((CALL_ARG) L29);
  res1 = L28;
  SATTR(res1,tp,create_t1);
  SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val6 = res1;
  L30 = ret_val6;
  SARR((ARRAYCALL_ARG)L26,0,(CALL_ARG)L30); 
  ;
 }
 is_eq_self = ATTR(ATTR(self,tp_con),same1);
 is_eq_t = ((OB) ATTR(cs,tp));
 ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 sig1 = CALL_S2120587481(cs, ret_val7);
 if ((sig1==((SIG) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L32 = ATTR(sig1,ret);
 L33 = (*dTP_is242312711[TAG(L32)])(L32, ((OB) TP_BUILTIN_bool));
 L341_=!(L33); 
 if (L341_) {
  err_self = self;
  err_s = ((STR) &Theis_1675105926);
  if ((err_self==((TRANS) NULL))) {
   L36 = TRUE;
  } else {
   L36 = (ATTR(err_self,cur_rout)==((AM_ROUT_DEF) NULL));
  }
  if (L36) {
   L35 = TRUE;
  } else {
   L35 = (ATTR(ATTR(err_self,cur_rout),sig1)==((SIG) NULL));
  }
  if (L35) {
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   plus_self = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg = ((STR) &name21);
   ret_val8 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val8;
   plus_sarg1 = err_s;
   ret_val9 = STR_ap2004550586(plus_self1, plus_sarg1);
   err_s = ret_val9;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if ((v==((dAM_EXPR) NULL))) {
  L37 = ATTR(sig1,args);
  L381_=(ARG)ARR((ARRAYARG)L37,0); 
  L39=L381_;
  v = TRANS_693526804(self, ATTR(cw,val1), ATTR(L39,tp));
 }
 if ((v==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self6 = ((AM_ROU1916046290) NULL);
 create_nargs = 2;
 create_source2 = ATTR(cw,source1);
 L42 = create_nargs;
 L44=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L42)*sizeof(AM_CALL_ARG);
 L43=ZALLOC_BIG(L44);
 if (L43==NULL) FATAL("Unable to allocate more memory");
 ((OB)L43)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

   ((OB)L43)->header.destroyed=0;
#endif

 L40 = ((AM_ROU1916046290) L43);
 L40->asize = L42;
 r6 = L40;
 SATTR(r6,source1,create_source2);
 ret_val10 = r6;
 arc = ret_val10;
 SATTR(arc,fun,sig1);
 create_self7 = ((AM_CALL_ARG) NULL);
 create_e = ((dAM_EXPR) l);
 L46=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
 if (L46==NULL) FATAL("Unable to allocate more memory");
 ((OB)L46)->header.tag=AM_CALL_ARG_tag;
 L45 = ((AM_CALL_ARG) L46);
 res2 = L45;
 SATTR(res2,expr,create_e);
 SATTR(res2,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
 ret_val11 = res2;
 L47 = ret_val11;
 SARR((AM_ROU1916046290)arc,0,(AM_CALL_ARG)L47); 
 ;
 create_self8 = ((AM_CALL_ARG) NULL);
 create_e1 = v;
 L50=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
 if (L50==NULL) FATAL("Unable to allocate more memory");
 ((OB)L50)->header.tag=AM_CALL_ARG_tag;
 L49 = ((AM_CALL_ARG) L50);
 res3 = L49;
 SATTR(res3,expr,create_e1);
 SATTR(res3,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
 ret_val12 = res3;
 L51 = ret_val12;
 SARR((AM_ROU1916046290)arc,1,(AM_CALL_ARG)L51); 
 ;
 create_self9 = ((AM_IF_STMT) NULL);
 create_source3 = ATTR(cw,source1);
 L54=ZALLOC(sizeof(struct AM_IF_STMT_struct));
 if (L54==NULL) FATAL("Unable to allocate more memory");
 ((OB)L54)->header.tag=AM_IF_STMT_tag;
 L53 = ((AM_IF_STMT) L54);
 r7 = L53;
 SATTR(r7,source1,create_source3);
 ret_val13 = r7;
 r3 = ret_val13;
 SE_CON412941612(ATTR(self,cur_se), ((dAM) arc));
 special_inline_s = self;
 special_inline_c = arc;
 L55 = ATTR(ATTR(special_inline_s,prog),all_reached);
 L561_=!(L55); 
 if (L561_) {
  L57 = ATTR(ATTR(special_inline_s,prog),generate_am);
  (*dGENER651759373[TAG(L57)])(L57, ATTR(special_inline_c,fun));
 }
 L58 = ATTR(ATTR(special_inline_s,prog),inliner);
 am = (*dINLIN970826022[TAG(L58)])(L58, special_inline_c);
 switch (TAG(am)) {
  case AM_STMT_EXPR_tag:
   SATTR(ATTR(special_inline_s,cur_rout),calls1,FLISTd1364738838(ATTR(ATTR(special_inline_s,cur_rout),calls1), ATTR(((AM_STMT_EXPR) am),calls1)));
   SATTR(ATTR(special_inline_s,cur_rout),locals1,FLISTA811588375(ATTR(ATTR(special_inline_s,cur_rout),locals1), ATTR(((AM_STMT_EXPR) am),locals1))); break;
  default: ;
 }
 ret_val14 = am;
 SATTR(r3,test1,ret_val14);
 SATTR(r3,if_true,TRANS_1563748848(self, ATTR(cw,then_part)));
 SATTR(r3,if_false,TRANS_1630043848(self, s, ATTR(cw,next), l));
 ret_val = ((dAM_STMT) r3);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1762825757(TRANS self, AS_RAISE_STMT s) {
 dAM_STMT ret_val;
 AM_RAISE_STMT r;
 TRANS stmts_after_rais;
 AS_RAISE_STMT stmts_after_rais1;
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
 TRANS unprotected_rais;
 AS_RAISE_STMT unprotected_rais1;
 TRANS err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 TRANS warning_self;
 STR warning_s;
 AM_RAISE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_RAISE_STMT ret_val4;
 AM_RAISE_STMT r1;
 BOOL L1;
 BOOL L21_;
 extern STR Nostat1502650915;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 extern STR Inside1737175246;
 extern STR should96003773;
 AM_RAISE_STMT L8;
 OB L9;
 BOOL L10;
 BOOL L111_;
 L1 = (ATTR(s,next)==((dAS_STMT) NULL));
 L21_=!(L1); 
 if (L21_) {
  stmts_after_rais = self;
  stmts_after_rais1 = s;
  err_loc_self = stmts_after_rais;
  err_loc_t = ((dPROG_ERR) stmts_after_rais1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = stmts_after_rais;
  err_s = ((STR) &Nostat1502650915);
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
 if (TRANS_2091440962(self)) {
  L6 = ATTR(self,in_protect_body);
  L71_=!(L6); 
  L5 = L71_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  unprotected_rais = self;
  unprotected_rais1 = s;
  err_loc_self1 = unprotected_rais;
  err_loc_t1 = ((dPROG_ERR) unprotected_rais1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  warning_self = unprotected_rais;
  plus_self2 = ((STR) &Inside1737175246);
  plus_sarg2 = ((STR) &should96003773);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  warning_s = ret_val3;
  PROG_warning_STR(ATTR(warning_self,prog), warning_s);
 }
 create_self = ((AM_RAISE_STMT) NULL);
 create_source = ATTR(s,source1);
 L9=ZALLOC(sizeof(struct AM_RAISE_STMT_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AM_RAISE_STMT_tag;
 L8 = ((AM_RAISE_STMT) L9);
 r1 = L8;
 SATTR(r1,source1,create_source);
 ret_val4 = r1;
 r = ret_val4;
 L10 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L111_=!(L10); 
 if (L111_) {
  SATTR(ATTR(self,cur_se),has_raise,TRUE);
 }
 SATTR(r,val1,TRANS_693526804(self, ATTR(s,val1), ((dTP) NULL)));
 if ((ATTR(r,val1)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1847483442(TRANS self, AS_RETURN_STMT s) {
 dAM_STMT ret_val;
 dTP rtp;
 dAM_STMT res = ((dAM_STMT) NULL);
 AM_ASSIGN_STMT a;
 AM_RETURN_STMT r;
 AM_INV1167837230 stmt1;
 AM_POST_STMT stmt2;
 TRANS return_in_iter_e;
 AS_RETURN_STMT return_in_iter_e1;
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
 TRANS return_in_par_fo;
 AS_RETURN_STMT return_in_par_fo1;
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
 TRANS stmts_after_retu;
 AS_RETURN_STMT stmts_after_retu1;
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
 TRANS missing_return_v;
 AS_RETURN_STMT missing_return_v1;
 dTP missing_return_v2;
 TRANS err_loc_self3;
 dPROG_ERR err_loc_t3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 TRANS err_self3;
 STR err_s3;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 AM_RETURN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_RETURN_STMT ret_val11;
 AM_RETURN_STMT r1;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val12;
 AM_ASSIGN_STMT r2;
 TRANS extra_return_val;
 AS_RETURN_STMT extra_return_val1;
 SIG extra_return_val2;
 TRANS err_loc_self4;
 dPROG_ERR err_loc_t4;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val13;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val14;
 TRANS err_self4;
 STR err_s4;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val15;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val16;
 AM_RETURN_STMT create_self2;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_RETURN_STMT ret_val17;
 AM_RETURN_STMT r3;
 extern STR return448605028;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 extern STR return401915800;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 BOOL L5;
 BOOL L61_;
 extern STR Nostat2057009120;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 BOOL L9;
 BOOL L101_;
 extern STR Aretur964039722;
 dTP L11;
 extern STR mustbespecified;
 BOOL L12;
 BOOL L13;
 extern STR name21;
 AM_RETURN_STMT L14;
 OB L15;
 AM_ASSIGN_STMT L16;
 OB L17;
 extern STR Noretu261930975;
 extern STR name18;
 BOOL L18;
 BOOL L19;
 extern STR name21;
 AM_RETURN_STMT L20;
 OB L21;
 BOOL L22;
 BOOL L231_;
 dAM_STMT L24;
 dAM_STMT L25;
 BOOL L26;
 BOOL L271_;
 dAM_STMT L28;
 dAM_STMT L29;
 if (TRANS_900561657(self)) {
  return_in_iter_e = self;
  return_in_iter_e1 = s;
  err_loc_self = return_in_iter_e;
  err_loc_t = ((dPROG_ERR) return_in_iter_e1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = return_in_iter_e;
  err_s = ((STR) &return448605028);
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
 if (TRANS_2091440962(self)) {
  return_in_par_fo = self;
  return_in_par_fo1 = s;
  err_loc_self1 = return_in_par_fo;
  err_loc_t1 = ((dPROG_ERR) return_in_par_fo1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = return_in_par_fo;
  err_s1 = ((STR) &return401915800);
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
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = err_s1;
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s1 = ret_val4;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L5 = (ATTR(s,next)==((dAS_STMT) NULL));
 L61_=!(L5); 
 if (L61_) {
  stmts_after_retu = self;
  stmts_after_retu1 = s;
  err_loc_self2 = stmts_after_retu;
  err_loc_t2 = ((dPROG_ERR) stmts_after_retu1);
  PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
  err_self2 = stmts_after_retu;
  err_s2 = ((STR) &Nostat2057009120);
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
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = err_s2;
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s2 = ret_val6;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
 }
 rtp = ATTR(ATTR(ATTR(self,cur_rout),sig1),ret);
 if ((ATTR(s,val1)==((dAS_EXPR) NULL))) {
  L9 = (rtp==((dTP) NULL));
  L101_=!(L9); 
  if (L101_) {
   missing_return_v = self;
   missing_return_v1 = s;
   missing_return_v2 = rtp;
   err_loc_self3 = missing_return_v;
   err_loc_t3 = ((dPROG_ERR) missing_return_v1);
   PROG_e176405615(ATTR(err_loc_self3,prog), err_loc_t3);
   err_self3 = missing_return_v;
   plus_self6 = ((STR) &Aretur964039722);
   L11 = missing_return_v2;
   plus_sarg6 = (*dTP_strrSTR[TAG(L11)])(L11);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = ((STR) &mustbespecified);
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s3 = ret_val8;
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
    plus_self8 = ATTR(ATTR(ATTR(err_self3,cur_rout),sig1),str);
    plus_sarg8 = ((STR) &name21);
    ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val9;
    plus_sarg9 = err_s3;
    ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
    err_s3 = ret_val10;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  create_self = ((AM_RETURN_STMT) NULL);
  create_source = ATTR(s,source1);
  L15=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=AM_RETURN_STMT_tag;
  L14 = ((AM_RETURN_STMT) L15);
  r1 = L14;
  SATTR(r1,source1,create_source);
  ret_val11 = r1;
  res = ((dAM_STMT) ret_val11);
 }
 else {
  create_self1 = ((AM_ASSIGN_STMT) NULL);
  create_source1 = ATTR(s,source1);
  L17=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=AM_ASSIGN_STMT_tag;
  L16 = ((AM_ASSIGN_STMT) L17);
  r2 = L16;
  SATTR(r2,source1,create_source1);
  ret_val12 = r2;
  a = ret_val12;
  SATTR(a,dest,((dAM_EXPR) ATTR(ATTR(self,cur_rout),rres)));
  if ((rtp==((dTP) NULL))) {
   extra_return_val = self;
   extra_return_val1 = s;
   extra_return_val2 = ATTR(ATTR(self,cur_rout),sig1);
   err_loc_self4 = extra_return_val;
   err_loc_t4 = ((dPROG_ERR) extra_return_val1);
   PROG_e176405615(ATTR(err_loc_self4,prog), err_loc_t4);
   err_self4 = extra_return_val;
   plus_self10 = ((STR) &Noretu261930975);
   plus_sarg10 = ATTR(extra_return_val2,str);
   ret_val13 = STR_ap2004550586(plus_self10, plus_sarg10);
   plus_self11 = ret_val13;
   plus_sarg11 = ((STR) &name18);
   ret_val14 = STR_ap2004550586(plus_self11, plus_sarg11);
   err_s4 = ret_val14;
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
    plus_self12 = ATTR(ATTR(ATTR(err_self4,cur_rout),sig1),str);
    plus_sarg12 = ((STR) &name21);
    ret_val15 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val15;
    plus_sarg13 = err_s4;
    ret_val16 = STR_ap2004550586(plus_self13, plus_sarg13);
    err_s4 = ret_val16;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  create_self2 = ((AM_RETURN_STMT) NULL);
  create_source2 = ATTR(s,source1);
  L21=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
  if (L21==NULL) FATAL("Unable to allocate more memory");
  ((OB)L21)->header.tag=AM_RETURN_STMT_tag;
  L20 = ((AM_RETURN_STMT) L21);
  r3 = L20;
  SATTR(r3,source1,create_source2);
  ret_val17 = r3;
  r = ret_val17;
  SATTR(a,next,((dAM_STMT) r));
  SATTR(r,val1,((dAM_EXPR) ATTR(ATTR(self,cur_rout),rres)));
  SATTR(a,src1,TRANS_693526804(self, ATTR(s,val1), rtp));
  if ((ATTR(a,src1)==((dAM_EXPR) NULL))) {
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
  }
  res = ((dAM_STMT) a);
 }
 L22 = (ATTR(self,inv_stmt)==((AM_INV1167837230) NULL));
 L231_=!(L22); 
 if (L231_) {
  stmt1 = AM_INV1353049642(ATTR(self,inv_stmt));
  L24 = res;
  SATTR(stmt1,next,(*dAM_ST775224435[TAG(L24)])(L24));
  L25 = res;
  (*dAM_ST1372701974[TAG(L25)])(L25, ((dAM_STMT) stmt1));
 }
 L26 = (ATTR(self,post_stmt)==((AM_POST_STMT) NULL));
 L271_=!(L26); 
 if (L271_) {
  stmt2 = AM_POS1078136755(ATTR(self,post_stmt));
  L28 = res;
  SATTR(stmt2,next,(*dAM_ST775224435[TAG(L28)])(L28));
  L29 = res;
  (*dAM_ST1372701974[TAG(L29)])(L29, ((dAM_STMT) stmt2));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1853773828(TRANS self, AS_QUIT_STMT s) {
 dAM_STMT ret_val;
 AM_RETURN_STMT r;
 TRANS quit_in_par_fork;
 AS_QUIT_STMT quit_in_par_fork1;
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
 TRANS quit_in_rout_err;
 AS_QUIT_STMT quit_in_rout_err1;
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
 TRANS stmts_after_quit;
 AS_QUIT_STMT stmts_after_quit1;
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
 AM_RETURN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_RETURN_STMT ret_val7;
 AM_RETURN_STMT r1;
 extern STR quitst277111570;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 BOOL L3;
 BOOL L41_;
 extern STR quitst2023839416;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 BOOL L7;
 BOOL L81_;
 extern STR Nostat1064653964;
 BOOL L9;
 BOOL L10;
 extern STR name21;
 AM_RETURN_STMT L11;
 OB L12;
 if (TRANS_2091440962(self)) {
  quit_in_par_fork = self;
  quit_in_par_fork1 = s;
  err_loc_self = quit_in_par_fork;
  err_loc_t = ((dPROG_ERR) quit_in_par_fork1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = quit_in_par_fork;
  err_s = ((STR) &quitst277111570);
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
 L3 = TRANS_900561657(self);
 L41_=!(L3); 
 if (L41_) {
  quit_in_rout_err = self;
  quit_in_rout_err1 = s;
  err_loc_self1 = quit_in_rout_err;
  err_loc_t1 = ((dPROG_ERR) quit_in_rout_err1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = quit_in_rout_err;
  err_s1 = ((STR) &quitst2023839416);
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
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 L7 = (ATTR(s,next)==((dAS_STMT) NULL));
 L81_=!(L7); 
 if (L81_) {
  stmts_after_quit = self;
  stmts_after_quit1 = s;
  err_loc_self2 = stmts_after_quit;
  err_loc_t2 = ((dPROG_ERR) stmts_after_quit1);
  PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
  err_self2 = stmts_after_quit;
  err_s2 = ((STR) &Nostat1064653964);
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
 }
 create_self = ((AM_RETURN_STMT) NULL);
 create_source = ATTR(s,source1);
 L12=ZALLOC(sizeof(struct AM_RETURN_STMT_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_RETURN_STMT_tag;
 L11 = ((AM_RETURN_STMT) L12);
 r1 = L11;
 SATTR(r1,source1,create_source);
 ret_val7 = r1;
 r = ret_val7;
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_1999552456(TRANS self, AS_CASE_STMT s) {
 dAM_STMT ret_val;
 AM_ASSIGN_STMT r;
 AM_LOCAL_EXPR l;
 AS_CASE_WHEN wp;
 AS_CASE_WHEN wp1;
 dAS_STMT sys_imp;
 dAS_STMT sys_imp1;
 AM_ASSIGN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val1;
 AM_ASSIGN_STMT r1;
 AM_LOCAL_EXPR create_self1;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val2;
 AM_LOCAL_EXPR r2;
 PROG psather_self;
 BOOL ret_val3 = BOOL_zero;
 AM_ROUT_DEF import_locals_pe;
 BOOL ret_val4 = BOOL_zero;
 TRANS insert_import_se;
 AS_STMT_LIST insert_import_l;
 SFILE_ID insert_import_so = SFILE_ID_zero;
 dAS_STMT import11 = ((dAS_STMT) NULL);
 TRANS insert_import_se1;
 AS_STMT_LIST insert_import_l1;
 SFILE_ID insert_import_so1 = SFILE_ID_zero;
 dAS_STMT import2 = ((dAS_STMT) NULL);
 AM_ROUT_DEF import_call_pend;
 BOOL ret_val5 = BOOL_zero;
 AM_ROUT_DEF import_done_self;
 AM_ASSIGN_STMT L1;
 OB L2;
 dAM_EXPR L3;
 AM_LOCAL_EXPR L4;
 OB L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 dAS_STMT L13;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 dAS_STMT L20;
 dAS_STMT L21;
 dAS_STMT L22;
 BOOL L23;
 BOOL L241_;
 dAS_STMT L25;
 dAS_STMT L26;
 if ((s==((AS_CASE_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(s,source1);
 L2=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_ASSIGN_STMT_tag;
 L1 = ((AM_ASSIGN_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,src1,TRANS_693526804(self, ATTR(s,test1), ((dTP) NULL)));
 if ((ATTR(r,src1)==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 create_self1 = ((AM_LOCAL_EXPR) NULL);
 create_src = ATTR(s,source1);
 create_name = IDENT_zero;
 L3 = ATTR(r,src1);
 create_tp = (*dAM_EXPR_tprdTP[TAG(L3)])(L3);
 L5=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AM_LOCAL_EXPR_tag;
 L4 = ((AM_LOCAL_EXPR) L5);
 r2 = L4;
 SATTR(r2,source1,create_src);
 SATTR(r2,name1,create_name);
 SATTR(r2,tp_at,create_tp);
 ret_val2 = r2;
 l = ret_val2;
 TRANS_626807411(self, l);
 SATTR(r,dest,((dAM_EXPR) l));
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L6 = TRUE;
 } else {
  L6 = ATTR(psather_self,distributed);
 }
 ret_val3 = L6;
 if (ret_val3) {
  L7 = ATTR(s,transformed);
  L81_=!(L7); 
  if (L81_) {
   import_locals_pe = ATTR(self,cur_rout);
   ret_val4 = ATTR(import_locals_pe,n_import_locals);
   if (ret_val4) {
    wp = ATTR(s,when_part);
    while (1) {
     if ((wp==((AS_CASE_WHEN) NULL))) {
      goto after_loop;
     }
     insert_import_se = self;
     insert_import_l = ATTR(wp,then_part);
     insert_import_so = ATTR(s,source1);
     if (TRANS_1975351488) {
      import11 = TRANS_120708644(insert_import_se, TRANS_1924362320, insert_import_l, insert_import_so);
     }
     L9 = (import11==((dAS_STMT) NULL));
     L101_=!(L9); 
     if (L101_) {
      L11 = (insert_import_l==((AS_STMT_LIST) NULL));
      L121_=!(L11); 
      if (L121_) {
       L13 = import11;
       (*dAS_ST1454156204[TAG(L13)])(L13, ATTR(insert_import_l,stmts));
      }
      SATTR(insert_import_l,stmts,import11);
     }
     wp = ATTR(wp,next);
    }
    after_loop: ;
    L14 = (ATTR(s,else_part)==((AS_STMT_LIST) NULL));
    L151_=!(L14); 
    if (L151_) {
     insert_import_se1 = self;
     insert_import_l1 = ATTR(s,else_part);
     insert_import_so1 = ATTR(s,source1);
     if (TRANS_1975351488) {
      import2 = TRANS_120708644(insert_import_se1, TRANS_1924362320, insert_import_l1, insert_import_so1);
     }
     L16 = (import2==((dAS_STMT) NULL));
     L171_=!(L16); 
     if (L171_) {
      L18 = (insert_import_l1==((AS_STMT_LIST) NULL));
      L191_=!(L18); 
      if (L191_) {
       L20 = import2;
       (*dAS_ST1454156204[TAG(L20)])(L20, ATTR(insert_import_l1,stmts));
      }
      SATTR(insert_import_l1,stmts,import2);
     }
    }
   }
   import_call_pend = ATTR(self,cur_rout);
   ret_val5 = ATTR(import_call_pend,n_import_call);
   if (ret_val5) {
    wp1 = ATTR(s,when_part);
    while (1) {
     if ((wp1==((AS_CASE_WHEN) NULL))) {
      goto after_loop1;
     }
     sys_imp = TRANS_166911016(self, TRANS_1924362320, ATTR(wp1,then_part), ATTR(s,source1));
     L21 = sys_imp;
     (*dAS_ST1569642444[TAG(L21)])(L21, TRUE);
     L22 = sys_imp;
     (*dAS_ST1454156204[TAG(L22)])(L22, ATTR(ATTR(wp1,then_part),stmts));
     SATTR(ATTR(wp1,then_part),stmts,sys_imp);
     wp1 = ATTR(wp1,next);
    }
    after_loop1: ;
    L23 = (ATTR(s,else_part)==((AS_STMT_LIST) NULL));
    L241_=!(L23); 
    if (L241_) {
     sys_imp1 = TRANS_166911016(self, TRANS_1924362320, ATTR(s,else_part), ATTR(s,source1));
     L25 = sys_imp1;
     (*dAS_ST1569642444[TAG(L25)])(L25, TRUE);
     L26 = sys_imp1;
     (*dAS_ST1454156204[TAG(L26)])(L26, ATTR(ATTR(s,else_part),stmts));
     SATTR(ATTR(s,else_part),stmts,sys_imp1);
    }
   }
  }
  import_done_self = ATTR(self,cur_rout);
  SATTR(import_done_self,n_import_locals,FALSE);
  SATTR(import_done_self,n_import_call,FALSE);
 }
 SATTR(r,next,TRANS_1630043848(self, s, ATTR(s,when_part), l));
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_565673800(TRANS self, AS_LOOP_STMT s) {
 dAM_STMT ret_val;
 AM_LOOP_STMT ol;
 AM_LOOP_STMT r;
 BOOL old_in_protect_b = BOOL_zero;
 INT num1 = INT_zero;
 INT hots = INT_zero;
 FLISTA1062334999 L11;
 SIG sig1;
 ARRAYBOOL L21;
 AM_LOOP_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOOP_STMT ret_val1;
 AM_LOOP_STMT r1;
 INT L31 = INT_zero;
 ARRAYINT create_self1;
 INT create_n = INT_zero;
 ARRAYINT ret_val2;
 ARRAYINT create_self2;
 INT create_n1 = INT_zero;
 ARRAYINT ret_val3;
 AM_LOOP_STMT L4;
 OB L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 AM_ITE1809135850 L16;
 AM_ITE1809135850 L17;
 BOOL L18;
 BOOL L191_;
 INT L201_br;
 BOOL aL201_;
 BOOL L22;
 INT L231_;
 INT L241_;
 ARRAYINT L25;
 INT L26;
 OB L27;
 INT L28;
 ARRAYINT L29;
 INT L30;
 OB L32;
 INT L33;
 ARRAYINT L34;
 ARRAYINT L35;
 INT L361_;
 INT L37;
 INT L381_;
 INT L39;
 ARRAYINT L41;
 ARRAYINT L42;
 INT L431_;
 INT L44;
 INT L451_;
 INT L46;
 BOOL L48;
 BOOL L491_;
 INT L50;
 INT L511_;
 INT L52;
 INT L531_;
 ol = ATTR(self,cur_loop);
 create_self = ((AM_LOOP_STMT) NULL);
 create_source = ATTR(s,source1);
 L5=ZALLOC(sizeof(struct AM_LOOP_STMT_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AM_LOOP_STMT_tag;
 L4 = ((AM_LOOP_STMT) L5);
 r1 = L4;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(self,cur_loop,r);
 old_in_protect_b = ATTR(self,in_protect_but_n);
 SATTR(self,in_protect_but_n,FALSE);
 SATTR(r,body,TRANS_1563748848(self, ATTR(s,body)));
 SATTR(self,in_protect_but_n,old_in_protect_b);
 L7 = (ol==((AM_LOOP_STMT) NULL));
 L81_=!(L7); 
 if (L81_) {
  L9 = (r==((AM_LOOP_STMT) NULL));
  L101_=!(L9); 
  L6 = L101_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  if (ATTR(ol,has_yield)) {
   L12 = TRUE;
  } else {
   L12 = ATTR(r,has_yield);
  }
  SATTR(ol,has_yield,L12);
 }
 SATTR(self,cur_loop,ol);
 if (ATTR(ATTR(self,prog),prolix)) {
  L14 = FSETSF406940155(ATTR(ATTR(self,prog),loops_seen), ATTR(s,source1));
  L151_=!(L14); 
  L13 = L151_;
 } else {
  L13 = FALSE;
 }
 if (L13) {
  num1 = FLISTA1962912942(ATTR(r,its1));
  hots = 0;
  {
   struct FLISTA20621028_frame_struct other1_0;
FLISTA20621028_frame noname1 = &other1_0;
   L11 = ATTR(r,its1);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L16 = FLISTA20621028(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    L17=L16;
    sig1 = ATTR(L17,fun);
    L18 = (ATTR(sig1,hot)==((ARRAYBOOL) NULL));
    L191_=!(L18); 
    if (L191_) {
     {
      BOOL f_L201_ = TRUE;
      /* loop index variable */
      L31 = 0;
      L21 = ATTR(sig1,hot);
      L201_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
      while (1) {
       if(L31>=L201_br)  goto after_loop1; 
       aL201_=ARR((ARRAYBOOL)L21,L31); 
       if (aL201_) {
        L231_=INTPLUS(hots,1); 
        hots = L231_;
        goto after_loop1;
       }
       L241_=INTPLUS(L31,1); 
       L31 = L241_;
      }
     }
     after_loop1: ;
    }
   }
  }
  after_loop: ;
  if ((ATTR(ATTR(self,prog),itercounts)==((ARRAYINT) NULL))) {
   create_self1 = ((ARRAYINT) NULL);
   create_n = 10;
   L26 = create_n;
   L28=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L26)*sizeof(INT);
   L27=ZALLOC_LEAF_BIG(L28);
   if (L27==NULL) FATAL("Unable to allocate more memory");
   memset(L27,0,L28);
   ((OB)L27)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

     ((OB)L27)->header.destroyed=0;
#endif

   L25 = ((ARRAYINT) L27);
   L25->asize = L26;
   ret_val2 = L25;
   SATTR(ATTR(self,prog),itercounts,ret_val2);
  }
  if ((ATTR(ATTR(self,prog),hotcounts)==((ARRAYINT) NULL))) {
   create_self2 = ((ARRAYINT) NULL);
   create_n1 = 10;
   L30 = create_n1;
   L33=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L30)*sizeof(INT);
   L32=ZALLOC_LEAF_BIG(L33);
   if (L32==NULL) FATAL("Unable to allocate more memory");
   memset(L32,0,L33);
   ((OB)L32)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

     ((OB)L32)->header.destroyed=0;
#endif

   L29 = ((ARRAYINT) L32);
   L29->asize = L30;
   ret_val3 = L29;
   SATTR(ATTR(self,prog),hotcounts,ret_val3);
  }
  L34 = ATTR(ATTR(self,prog),itercounts);
  L35 = ATTR(ATTR(self,prog),itercounts);
  L361_=ARR((ARRAYINT)L35,num1); 
  L37 = L361_;
  L381_=INTPLUS(L37,1); 
  L39 = L381_;
  SARR((ARRAYINT)L34,num1,L39); 
  ;
  L41 = ATTR(ATTR(self,prog),hotcounts);
  L42 = ATTR(ATTR(self,prog),hotcounts);
  L431_=ARR((ARRAYINT)L42,hots); 
  L44 = L431_;
  L451_=INTPLUS(L44,1); 
  L46 = L451_;
  SARR((ARRAYINT)L41,hots,L46); 
  ;
  SATTR(ATTR(self,prog),loops_seen,FSETSF663191272(ATTR(ATTR(self,prog),loops_seen), ATTR(s,source1)));
 }
 L48 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L491_=!(L48); 
 if (L491_) {
  L50 = ATTR(ATTR(self,cur_se),weight);
  L511_=INTPLUS(L50,10); 
  L52 = L511_;
  L531_=(L52<1000000)?L52:1000000; 
  SATTR(ATTR(self,cur_se),weight,L531_);
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_741871011(TRANS self, AS_PROTECT_STMT s) {
 dAM_STMT ret_val;
 AM_PROTECT_STMT r;
 BOOL old_in_protect_b = BOOL_zero;
 BOOL old_in_protect_b1 = BOOL_zero;
 AS_PROTECT_WHEN wp;
 dTP tp;
 dTP oex_tp;
 BOOL old_in_protect_t = BOOL_zero;
 dTP oex_tp1;
 BOOL old_in_protect_t1 = BOOL_zero;
 AM_PROTECT_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PROTECT_STMT ret_val1;
 AM_PROTECT_STMT r1;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val2;
 AM_PROTECT_STMT L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 FLISTdAM_STMT L5;
 create_self = ((AM_PROTECT_STMT) NULL);
 create_source = ATTR(s,source1);
 L2=ZALLOC(sizeof(struct AM_PROTECT_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_PROTECT_STMT_tag;
 L1 = ((AM_PROTECT_STMT) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 old_in_protect_b = ATTR(self,in_protect_body);
 SATTR(self,in_protect_body,TRUE);
 old_in_protect_b1 = ATTR(self,in_protect_but_n);
 SATTR(self,in_protect_but_n,TRUE);
 SATTR(r,body,TRANS_1563748848(self, ATTR(s,body)));
 SATTR(self,in_protect_body,old_in_protect_b);
 wp = ATTR(s,when_part);
 while (1) {
  L3 = (wp==((AS_PROTECT_WHEN) NULL));
  L41_=!(L3); 
  if (L41_) {
  }
  else {
   goto after_loop;
  }
  tp_of_self = self;
  tp_of_t = ATTR(wp,tp);
  ret_val2 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  tp = ret_val2;
  oex_tp = ATTR(self,ex_tp);
  SATTR(self,ex_tp,tp);
  old_in_protect_t = ATTR(self,in_protect_then);
  SATTR(self,in_protect_then,TRUE);
  SATTR(r,tgts,FLISTd1953924847(ATTR(r,tgts), tp));
  L5 = ATTR(r,stmts);
  SATTR(r,stmts,FLISTd506668277(L5, TRANS_1563748848(self, ATTR(wp,then_part))));
  SATTR(self,in_protect_then,old_in_protect_t);
  SATTR(self,ex_tp,oex_tp);
  wp = ATTR(wp,next);
 }
 after_loop: ;
 if (ATTR(s,no_else)) {
  SATTR(r,no_else,TRUE);
 }
 else {
  oex_tp1 = ATTR(self,ex_tp);
  SATTR(self,ex_tp,((dTP) TP_BUI1325635093));
  old_in_protect_t1 = ATTR(self,in_protect_then);
  SATTR(self,in_protect_then,TRUE);
  SATTR(r,else_stmts,TRANS_1563748848(self, ATTR(s,else_part)));
  SATTR(self,in_protect_then,old_in_protect_t1);
  SATTR(self,ex_tp,oex_tp1);
 }
 SATTR(self,in_protect_but_n,old_in_protect_b1);
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_774746956(TRANS self, AS_TYPECASE_STMT s) {
 dAM_STMT ret_val;
 AM_LOCAL_EXPR l;
 BOOL old_no_assign = BOOL_zero;
 dTP ltp;
 AS_TYPE_SPEC las_type;
 AM_TYPECASE_STMT r;
 AS_TYPECASE_WHEN wp;
 dTP tp;
 TRANS typecase_local_e;
 AS_TYPECASE_STMT typecase_local_e1;
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
 TRANS typecase_hot_loc;
 AS_TYPECASE_STMT typecase_hot_loc1;
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
 AM_LOCAL_EXPR tp_self;
 dTP ret_val9;
 AM_TYPECASE_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_TYPECASE_STMT ret_val10;
 AM_TYPECASE_STMT r1;
 TRANS tp_of_self;
 AS_TYPE_SPEC tp_of_t;
 dTP ret_val11;
 TRANS typecase_no_bran;
 AS_TYPECASE_STMT typecase_no_bran1;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 extern STR Thename;
 extern STR isntal923684775;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 extern STR Thetyp933302920;
 extern STR mustno562683704;
 BOOL L3;
 BOOL L4;
 extern STR name21;
 AM_TYPECASE_STMT L5;
 OB L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 dTP L11;
 dTP L12;
 FLISTdAM_STMT L13;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 extern STR Therea944368619;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 l = TRANS_1711874080(self, ATTR(s,name1));
 if ((l==((AM_LOCAL_EXPR) NULL))) {
  typecase_local_e = self;
  typecase_local_e1 = s;
  err_loc_self = typecase_local_e;
  err_loc_t = ((dPROG_ERR) typecase_local_e1);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  err_self = typecase_local_e;
  plus_self = ((STR) &Thename);
  plus_sarg = ATTR(typecase_local_e1,name1).str;
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &isntal923684775);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  err_s = ret_val2;
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
   plus_self2 = ATTR(ATTR(ATTR(err_self,cur_rout),sig1),str);
   plus_sarg2 = ((STR) &name21);
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = err_s;
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   err_s = ret_val4;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 if (AM_ROU389945790(ATTR(self,cur_rout), l)) {
  typecase_hot_loc = self;
  typecase_hot_loc1 = s;
  err_loc_self1 = typecase_hot_loc;
  err_loc_t1 = ((dPROG_ERR) typecase_hot_loc1);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  err_self1 = typecase_hot_loc;
  plus_self4 = ((STR) &Thetyp933302920);
  plus_sarg4 = ATTR(typecase_hot_loc1,name1).str;
  ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
  plus_self5 = ret_val5;
  plus_sarg5 = ((STR) &mustno562683704);
  ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
  err_s1 = ret_val6;
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
   plus_self6 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
   plus_sarg6 = ((STR) &name21);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = err_s1;
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s1 = ret_val8;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 old_no_assign = ATTR(l,no_assign);
 SATTR(l,no_assign,TRUE);
 tp_self = l;
 ret_val9 = ATTR(tp_self,tp_at);
 ltp = ret_val9;
 las_type = ATTR(l,as_type);
 create_self = ((AM_TYPECASE_STMT) NULL);
 create_source = ATTR(s,source1);
 L6=ZALLOC(sizeof(struct AM_TYPECASE_STMT_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_TYPECASE_STMT_tag;
 L5 = ((AM_TYPECASE_STMT) L6);
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val10 = r1;
 r = ret_val10;
 SATTR(r,test1,l);
 SATTR(r,no_else,TRUE);
 SATTR(r,has_void_stmts,FALSE);
 wp = ATTR(s,when_part);
 L7 = (ATTR(self,cur_se)==((SE_CONTEXT) NULL));
 L81_=!(L7); 
 if (L81_) {
  SATTR(ATTR(self,cur_se),has_fatal_error,TRUE);
 }
 while (1) {
  L9 = (wp==((AS_TYPECASE_WHEN) NULL));
  L101_=!(L9); 
  if (L101_) {
  }
  else {
   goto after_loop;
  }
  tp_of_self = self;
  tp_of_t = ATTR(wp,tp);
  ret_val11 = TP_CON1800432689(ATTR(tp_of_self,tp_con), tp_of_t);
  tp = ret_val11;
  L11 = ltp;
  if ((*dTP_is1999456142[TAG(L11)])(L11, tp)) {
   SATTR(r,no_else,FALSE);
   SATTR(r,else_stmts,TRANS_1563748848(self, ATTR(wp,then_part)));
   SATTR(r,has_void_stmts,TRUE);
   goto after_loop;
  }
  else {
   L12 = ltp;
   if ((*dTP_is1811059018[TAG(L12)])(L12)) {
    SATTR(l,tp_at,tp);
    SATTR(l,as_type,ATTR(wp,tp));
    SATTR(r,tgts,FLISTd1953924847(ATTR(r,tgts), tp));
    L13 = ATTR(r,stmts);
    SATTR(r,stmts,FLISTd506668277(L13, TRANS_1563748848(self, ATTR(wp,then_part))));
    SATTR(l,tp_at,ltp);
    SATTR(l,as_type,las_type);
   }
  }
  wp = ATTR(wp,next);
 }
 after_loop: ;
 SATTR(l,no_assign,old_no_assign);
 if (ATTR(r,no_else)) {
  L15 = ATTR(s,no_else);
  L161_=!(L15); 
  L14 = L161_;
 } else {
  L14 = FALSE;
 }
 if (L14) {
  SATTR(r,no_else,FALSE);
  SATTR(r,else_stmts,TRANS_1563748848(self, ATTR(s,else_part)));
  SATTR(r,has_void_stmts,TRUE);
 }
 if ((ATTR(r,stmts)==((FLISTdAM_STMT) NULL))) {
  if (ATTR(r,no_else)) {
   typecase_no_bran = self;
   typecase_no_bran1 = s;
   err_loc_self2 = typecase_no_bran;
   err_loc_t2 = ((dPROG_ERR) typecase_no_bran1);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = typecase_no_bran;
   err_s2 = ((STR) &Therea944368619);
   if ((err_self2==((TRANS) NULL))) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L17) {
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
   ret_val = ((dAM_STMT) r);
   return ret_val;
  }
  else {
   ret_val = ATTR(r,else_stmts);
   return ret_val;
  }
 }
 ret_val = ((dAM_STMT) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT TRANS_905208628(TRANS self, AS_EXPR_STMT s) {
 dAM_STMT ret_val;
 dAS_EXPR e;
 AM_BREAK_STMT r;
 AM_EXPR_STMT r1;
 AM_AT_EXPR r2;
 dAS_EXPR ce;
 AM_EXPR_STMT r21;
 TRANS break_not_in_loo;
 AS_EXPR_STMT break_not_in_loo1;
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
 AM_BREAK_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_BREAK_STMT ret_val3;
 AM_BREAK_STMT r3;
 AM_EXPR_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val4;
 AM_EXPR_STMT r4;
 AM_AT_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_AT_EXPR ret_val5;
 AM_AT_EXPR r5;
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
 AM_EXPR_STMT create_self3;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val8;
 AM_EXPR_STMT r6;
 TRANS expr_stmt_err_se;
 AS_EXPR_STMT expr_stmt_err_s;
 TRANS err_loc_self2;
 dPROG_ERR err_loc_t2;
 TRANS err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 extern STR breakb17360986;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 AM_BREAK_STMT L3;
 OB L4;
 AM_EXPR_STMT L5;
 OB L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 AM_AT_EXPR L11;
 OB L12;
 extern STR Lhsofi493151537;
 BOOL L13;
 BOOL L14;
 extern STR name21;
 AM_EXPR_STMT L15;
 OB L16;
 extern STR Expres1823750894;
 BOOL L17;
 BOOL L18;
 extern STR name21;
 e = ATTR(s,e);
 if (e==NULL) {
  goto other429;
 } else
 switch (TAG(e)) {
  case AS_BREAK_EXPR_tag:
   if ((ATTR(self,cur_loop)==((AM_LOOP_STMT) NULL))) {
    break_not_in_loo = self;
    break_not_in_loo1 = s;
    err_loc_self = break_not_in_loo;
    err_loc_t = ((dPROG_ERR) break_not_in_loo1);
    PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
    err_self = break_not_in_loo;
    err_s = ((STR) &breakb17360986);
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
   create_self = ((AM_BREAK_STMT) NULL);
   create_source = ATTR(s,source1);
   L4=ZALLOC(sizeof(struct AM_BREAK_STMT_struct));
   if (L4==NULL) FATAL("Unable to allocate more memory");
   ((OB)L4)->header.tag=AM_BREAK_STMT_tag;
   L3 = ((AM_BREAK_STMT) L4);
   r3 = L3;
   SATTR(r3,source1,create_source);
   ret_val3 = r3;
   r = ret_val3;
   ret_val = ((dAM_STMT) r);
   return ret_val; break;
  case AS_CALL_EXPR_tag:
   create_self1 = ((AM_EXPR_STMT) NULL);
   create_source1 = ATTR(s,source1);
   L6=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AM_EXPR_STMT_tag;
   L5 = ((AM_EXPR_STMT) L6);
   r4 = L5;
   SATTR(r4,source1,create_source1);
   ret_val4 = r4;
   r1 = ret_val4;
   SATTR(r1,expr,TRANS_1705639890(self, ((AS_CALL_EXPR) e), ((dTP) NULL), FALSE));
   L7 = (ATTR(s,at)==((dAS_EXPR) NULL));
   L81_=!(L7); 
   if (L81_) {
    if (ATTR(ATTR(self,prog),zones)) {
     SATTR(r1,at,TRANS_693526804(self, ATTR(s,at), ((dTP) TP_BUILTIN_zone)));
    }
    else {
     SATTR(r1,at,TRANS_693526804(self, ATTR(s,at), ((dTP) TP_BUILTIN_int)));
    }
   }
   L9 = (ATTR(r1,expr)==((dAM_EXPR) NULL));
   L101_=!(L9); 
   if (L101_) {
    ret_val = ((dAM_STMT) r1);
    return ret_val;
   }
   else {
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   } break;
  case AS_AT_EXPR_tag:
   create_self2 = ((AM_AT_EXPR) NULL);
   create_src = ATTR(((AS_AT_EXPR) e),source1);
   L12=ZALLOC(sizeof(struct AM_AT_EXPR_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=AM_AT_EXPR_tag;
   L11 = ((AM_AT_EXPR) L12);
   r5 = L11;
   SATTR(r5,source1,create_src);
   ret_val5 = r5;
   r2 = ret_val5;
   ce = ATTR(((AS_AT_EXPR) e),e);
   if (ce==NULL) {
    goto other430;
   } else
   switch (TAG(ce)) {
    case AS_CALL_EXPR_tag:
     SATTR(r2,e,TRANS_1705639890(self, ((AS_CALL_EXPR) ce), ((dTP) NULL), FALSE)); break;
    default: ;
    other430: ;
     err_loc_self1 = self;
     err_loc_t1 = ((dPROG_ERR) s);
     PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
     err_self1 = self;
     err_s1 = ((STR) &Lhsofi493151537);
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
      plus_self2 = ATTR(ATTR(ATTR(err_self1,cur_rout),sig1),str);
      plus_sarg2 = ((STR) &name21);
      ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
      plus_self3 = ret_val6;
      plus_sarg3 = err_s1;
      ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
      err_s1 = ret_val7;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
   }
   if ((ATTR(r2,e)==((dAM_EXPR) NULL))) {
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   }
   if (ATTR(ATTR(self,prog),zones)) {
    SATTR(r2,at,TRANS_693526804(self, ATTR(((AS_AT_EXPR) e),at), ((dTP) TP_BUILTIN_zone)));
   }
   else {
    SATTR(r2,at,TRANS_693526804(self, ATTR(((AS_AT_EXPR) e),at), ((dTP) TP_BUILTIN_int)));
   }
   if ((ATTR(r2,at)==((dAM_EXPR) NULL))) {
    ret_val = ((dAM_STMT) NULL);
    return ret_val;
   }
   create_self3 = ((AM_EXPR_STMT) NULL);
   create_source2 = ATTR(s,source1);
   L16=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
   if (L16==NULL) FATAL("Unable to allocate more memory");
   ((OB)L16)->header.tag=AM_EXPR_STMT_tag;
   L15 = ((AM_EXPR_STMT) L16);
   r6 = L15;
   SATTR(r6,source1,create_source2);
   ret_val8 = r6;
   r21 = ret_val8;
   SATTR(r21,expr,((dAM_EXPR) r2));
   ret_val = ((dAM_STMT) r21);
   return ret_val; break;
  default: ;
  other429: ;
   expr_stmt_err_se = self;
   expr_stmt_err_s = s;
   err_loc_self2 = expr_stmt_err_se;
   err_loc_t2 = ((dPROG_ERR) expr_stmt_err_s);
   PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
   err_self2 = expr_stmt_err_se;
   err_s2 = ((STR) &Expres1823750894);
   if ((err_self2==((TRANS) NULL))) {
    L18 = TRUE;
   } else {
    L18 = (ATTR(err_self2,cur_rout)==((AM_ROUT_DEF) NULL));
   }
   if (L18) {
    L17 = TRUE;
   } else {
    L17 = (ATTR(ATTR(err_self2,cur_rout),sig1)==((SIG) NULL));
   }
   if (L17) {
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
   ret_val = ((dAM_STMT) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT TRANS_120708644(TRANS self, INT imporexp, AS_STMT_LIST body11, SFILE_ID source1) {
 dAS_STMT ret_val;
 AS_ASSIGN_STMT as = ((AS_ASSIGN_STMT) NULL);
 dAS_STMT code11;
 AM_LOCAL_EXPR last_helper;
 AM_LOCAL_EXPR rel_helper = ((AM_LOCAL_EXPR) NULL);
 TRANS L11;
 AM_LOCAL_EXPR loc;
 AS_CALL_EXPR locvar;
 AS_CALL_EXPR helploc;
 INT L21 = INT_zero;
 AS_ASSIGN_STMT create_self;
 AS_ASSIGN_STMT ret_val1;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val2;
 AM_LOCAL_EXPR L3;
 BOOL L4;
 STR L5;
 extern STR S_;
 BOOL L6;
 BOOL L71_;
 AS_ASSIGN_STMT L8;
 OB L9;
 AS_CALL_EXPR L10;
 OB L12;
 dAS_STMT L13;
 code11 = ((dAS_STMT) NULL);
 last_helper = TRANS_647716465(self);
 if ((last_helper==((AM_LOCAL_EXPR) NULL))) {
  ret_val = code11;
  return ret_val;
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
 if ((rel_helper==((AM_LOCAL_EXPR) NULL))) {
  ret_val = code11;
  return ret_val;
 }
 {
  struct TRANS_1349638895_frame_struct other1_0;
TRANS_1349638895_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = TRANS_1349638895(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   loc = L3;
   L5 = STR_head_INTrSTR(ATTR(loc,name1).str, 1);
   L6 = STR_is111530248(L5, ((STR) &S_));
   L71_=!(L6); 
   if (L71_) {
    L4 = TRANS_157729662(self, ATTR(loc,name1), rel_helper);
   } else {
    L4 = FALSE;
   }
   if (L4) {
    create_self = ((AS_ASSIGN_STMT) NULL);
    L9=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=AS_ASSIGN_STMT_tag;
    L8 = ((AS_ASSIGN_STMT) L9);
    ret_val1 = L8;
    as = ret_val1;
    SATTR(as,source1,source1);
    SATTR(as,surr_stmt_list,body11);
    SATTR(as,transformed,TRUE);
    create_self1 = ((AS_CALL_EXPR) NULL);
    L12=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=AS_CALL_EXPR_tag;
    L10 = ((AS_CALL_EXPR) L12);
    ret_val2 = L10;
    locvar = ret_val2;
    SATTR(locvar,source1,source1);
    SATTR(locvar,name1,ATTR(loc,name1));
    helploc = TRANS_1849744987(self, ATTR(loc,name1), rel_helper, source1);
    L21 = imporexp;
    switch (L21) {
     case 1: 
      SATTR(as,lhs_expr,((dAS_EXPR) locvar));
      SATTR(as,rhs,((dAS_EXPR) helploc));
      break;
     case 2: 
      SATTR(as,lhs_expr,((dAS_EXPR) helploc));
      SATTR(as,rhs,((dAS_EXPR) locvar));
      break;
     default: ;
      FATAL("No applicable target in case statement\nin TRANS::as_for_import_export(INT,AS_STMT_LIST,SFILE_ID):$AS_STMT\n./Middle/trans.sa:5067:28");
    }
    if ((code11==((dAS_STMT) NULL))) {
     code11 = ((dAS_STMT) as);
    }
    else {
     L13 = code11;
     (*dAS_ST1454156204[TAG(L13)])(L13, ((dAS_STMT) as));
    }
   }
  }
 }
 after_loop: ;
 ret_val = code11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT TRANS_166911016(TRANS self, INT imporexp, AS_STMT_LIST body11, SFILE_ID source1) {
 dAS_STMT ret_val;
 AS_EXPR_STMT impexp;
 AS_CALL_EXPR impexpe;
 AS_TYPE_SPEC impexpetp;
 INT L11 = INT_zero;
 AS_EXPR_STMT create_self;
 AS_EXPR_STMT ret_val1;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val2;
 AS_TYPE_SPEC create_self2;
 AS_TYPE_SPEC ret_val3;
 AS_EXPR_STMT L2;
 OB L3;
 AS_CALL_EXPR L4;
 OB L5;
 AS_TYPE_SPEC L6;
 OB L7;
 extern STR SYS1;
 extern STR import1;
 extern STR export2;
 create_self = ((AS_EXPR_STMT) NULL);
 L3=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AS_EXPR_STMT_tag;
 L2 = ((AS_EXPR_STMT) L3);
 ret_val1 = L2;
 impexp = ret_val1;
 SATTR(impexp,source1,source1);
 SATTR(impexp,surr_stmt_list,body11);
 create_self1 = ((AS_CALL_EXPR) NULL);
 L5=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AS_CALL_EXPR_tag;
 L4 = ((AS_CALL_EXPR) L5);
 ret_val2 = L4;
 impexpe = ret_val2;
 SATTR(impexpe,source1,source1);
 create_self2 = ((AS_TYPE_SPEC) NULL);
 L7=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AS_TYPE_SPEC_tag;
 L6 = ((AS_TYPE_SPEC) L7);
 ret_val3 = L6;
 impexpetp = ret_val3;
 SATTR(impexpetp,source1,source1);
 SATTR(impexpetp,name1,IDENT_1150413730(IDENT_zero, ((STR) &SYS1)));
 SATTR(impexpe,tp,impexpetp);
 L11 = imporexp;
 switch (L11) {
  case 1: 
   SATTR(impexpe,name1,IDENT_1150413730(IDENT_zero, ((STR) &import1)));
   break;
  case 2: 
   SATTR(impexpe,name1,IDENT_1150413730(IDENT_zero, ((STR) &export2)));
   break;
  default: ;
   FATAL("No applicable target in case statement\nin TRANS::as_for_sys_import_export(INT,AS_STMT_LIST,SFILE_ID):$AS_STMT\n./Middle/trans.sa:5013:22");
 }
 SATTR(impexp,e,((dAS_EXPR) impexpe));
 ret_val = ((dAS_STMT) impexp);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void TRANS_1069298803(TRANS self, TP_CLASS cur_class_tp, AS_ROUT_DEF newrout) {
 SIG sig1;
 ELT elt1;
 IMPL impl;
 ELT f;
 TRANS err_self;
 STR err_s;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 ELT create_self;
 SIG create_sig;
 SIG create_srcsig;
 dAS_CLASS_ELT create_as;
 TP_CONTEXT create_con;
 BOOL create_is_privat = BOOL_zero;
 ELT ret_val2;
 ELT r;
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
 extern STR Compil372096575;
 BOOL L1;
 BOOL L2;
 extern STR name21;
 ELT L3;
 OB L4;
 extern STR Compil372096556;
 BOOL L5;
 BOOL L6;
 extern STR name21;
 extern STR Compil372096537;
 BOOL L7;
 BOOL L8;
 extern STR name21;
 sig1 = SIG_ro1276623596(((SIG) NULL), newrout, ATTR(newrout,name1), ATTR(ATTR(self,tp_con),ptypes), ATTR(self,tp_con));
 if ((sig1==((SIG) NULL))) {
  err_self = self;
  err_s = ((STR) &Compil372096575);
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
 }
 create_self = ((ELT) NULL);
 create_sig = sig1;
 create_srcsig = sig1;
 create_as = ((dAS_CLASS_ELT) newrout);
 create_con = ATTR(self,tp_con);
 create_is_privat = ATTR(newrout,is_private);
 L4=ZALLOC(sizeof(struct ELT_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=ELT_tag;
 L3 = ((ELT) L4);
 r = L3;
 SATTR(r,sig1,create_sig);
 SATTR(r,srcsig,create_srcsig);
 SATTR(r,as,create_as);
 SATTR(r,con,create_con);
 SATTR(r,is_private,create_is_privat);
 ret_val2 = r;
 elt1 = ret_val2;
 if ((elt1==((ELT) NULL))) {
  err_self1 = self;
  err_s1 = ((STR) &Compil372096556);
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
 impl = TP_CLA1109727233(cur_class_tp);
 if ((impl==((IMPL) NULL))) {
  err_self2 = self;
  err_s2 = ((STR) &Compil372096537);
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
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = err_s2;
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s2 = ret_val6;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
 }
 f = ELT_TB954957059(ATTR(impl,elts), elt1);
 if ((f==((ELT) NULL))) {
  SATTR(impl,elts,ELT_TB1703146017(ATTR(impl,elts), elt1));
 }
}


#undef IS_ITER
#define IS_ITER 1

AM_LOCAL_EXPR TRANS_1349638895(TRANS_1349638895_frame frame) {
 AM_LOCAL_EXPR dummy = ((AM_LOCAL_EXPR) NULL);
 AM_ROUT_DEF L1;
 INT L21_;
 INT L3;
 BOOL L41_;
 AM_FORMAL_ARG L7;
 AM_FORMAL_ARG aL61_;
 AM_FORMAL_ARG L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 AM_LOCAL_EXPR L14;
 BOOL L16;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 if ((ATTR(frame->self,cur_rout)==((AM_ROUT_DEF) NULL))) {
  frame->state = -1;
  return;
 }
 L1 = ATTR(frame->self,cur_rout);
 L21_=ASIZE((AM_ROUT_DEF)L1); 
 L3 = L21_;
 L41_=(1)<(L3); 
 if (L41_) {
  {
   /* loop index variable */
   frame->L51 = 0;
   frame->f_L61_ = TRUE;
   while (1) {
    frame->state = 1;
    if (frame->f_L61_) {
     frame->f_L61_ = FALSE;
     frame->L81 = ATTR(frame->self,cur_rout);
     frame->L91 = 1;
     frame->L61_c=frame->L91-1;frame->L61_s=frame->L81==0?0:ASIZE((AM_ROUT_DEF)frame->L81); 
    }
    if(++frame->L61_c>=frame->L61_s)  goto after_loop; aL61_=ARR((AM_ROUT_DEF)frame->L81,frame->L61_c); 
    L10=aL61_;
    return ATTR(L10,expr);
    state1:;
    L111_=INTPLUS(frame->L51,1); 
    frame->L51 = L111_;
   }
  }
  after_loop: ;
 }
 L12 = FLISTA1151473122(ATTR(frame->self,active_locals));
 L131_=(0)<(L12); 
 if (L131_) {
  {
   frame->nested2 = OBALLOC(FLISTA2119642552_frame);
   frame->nested2->state = 0;
   while (1) {
    if (frame->nested2->state == 0) {
     frame->L151 = ATTR(frame->self,active_locals);
     frame->nested2->self = frame->L151;
    }
    L14 = FLISTA2119642552(frame->nested2);
    if (frame->nested2->state == -1) {
     goto after_loop1;
    }
    frame->loc = L14;
    L17 = (ATTR(frame->loc,name1).str==(STR)0);
    L181_=!(L17); 
    if (L181_) {
     L19 = (ATTR(frame->loc,tp_at)==((dTP) NULL));
     L201_=!(L19); 
     L16 = L201_;
    } else {
     L16 = FALSE;
    }
    if (L16) {
     frame->state = 2;
     return frame->loc;
     state2:;
    }
   }
  }
  after_loop1: ;
  ZFREE(frame->nested2); frame->nested2 = NULL;
 }
 frame->state = -1;
 return dummy;
}
