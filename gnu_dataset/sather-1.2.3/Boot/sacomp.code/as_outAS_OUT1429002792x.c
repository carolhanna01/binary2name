#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

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

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

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

typedef struct AS_OUT_struct {/* layout for AS_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_OUT;

typedef struct BUILD_TYPE_GRAPH_struct {/* layout for BUILD_TYPE_GRAPH */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *BUILD_TYPE_GRAPH;

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

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

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

typedef struct INT_timesbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesbrINT_frame;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

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

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

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

typedef struct AM_ROU2113525054_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU2113525054_frame;

typedef struct AM_ROU545684847_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU545684847_frame;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYC494195798_frame_struct {
 INT state;
 ARRAYCALL_ARG self;/* Formal argument: self */
 CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC494195798_frame;

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

typedef struct AS_ARG_DEC_struct {/* layout for AS_ARG_DEC */
 OB_HEADER header;
 struct AS_ARG_DEC_struct *next;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_ARG_DEC;

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

typedef struct AS_EXPR_STMT_struct {/* layout for AS_EXPR_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_EXPR_STMT;

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

typedef struct CALL_SIG_struct {/* layout for CALL_SIG */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ARRAYCALL_ARG_struct *args;
 struct IDENT_struct name1;
 BOOL has_ret;
 BOOL unknown_ret;
 } *CALL_SIG;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FMAPTP291739594_struct {/* layout for FMAP{TP_CLASS,FSET{$TP}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FMAPTP291739594;

typedef struct FMAPTP1465407463_frame_struct {
 INT state;
 FMAPTP291739594 self;/* Formal argument: self */
 TUPTP_858321267 ret_val2;
 FMAPTP291739594 L61;
 TUPTP_858321267 r;
 INT L31;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPTP1465407463_frame;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETdTP_eltbrdTP_frame_struct {
 INT state;
 FSETdTP self;/* Formal argument: self */
 dTP ret_val1;
 FSETdTP L61;
 dTP r;
 INT i;
 INT sz;
 INT L31;
 FSETdTP is_elt_nil_self;
 dTP is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETdTP_eltbrdTP_frame;

typedef struct FSETTP_CLASS_struct {/* layout for FSET{TP_CLASS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FSETTP_CLASS;

typedef struct FSETTP340014621_frame_struct {
 INT state;
 FSETTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val2;
 FSETTP_CLASS L61;
 TP_CLASS r;
 INT i;
 INT sz;
 INT L31;
 FSETTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTP340014621_frame;

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

extern IDENT IDENT_2110206590;
extern IDENT IDENT_327685657;
extern INOUT_MODE MODES_inout_mode;
extern INT AS_OUT_indent;
extern IN_MODE MODES_in_mode;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;

/* Function declarations */


extern RETURNED_CONST AS_STMT_LIST (**dAS_ST445865822)(dAS_STMT);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST BOOL (**dCALL_358880146)(dCALL_TP, dTP);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST BOOL (**dTP_is762676118)(dTP, dCALL_TP);

extern RETURNED_CONST FLISTA725283029 (**dAM_EX1656723676)(dAM_EXPR);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dAM_EX1001627434)(dAM_EXPR);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST PROG (**dTP_progrPROG)(dTP);

extern RETURNED_CONST SFILE_ID (**dAM_EX2051891691)(dAM_EXPR);

extern RETURNED_CONST STR (**dCALL_TP_strrSTR)(dCALL_TP);

extern RETURNED_CONST STR (**dMODE_strrSTR)(dMODE);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);
AM_CALL_ARG AM_CAL1030878588(AM_CALL_ARG);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_ROU1916046290 AM_ROU960965786(AM_ROU1916046290);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYINT SIG_ne271652128(SIG);
BOOL AM_ROU2096608001(AM_ROU1916046290);
BOOL AM_ROU774362080(AM_ROU1916046290, OB);
BOOL BOOL_f112211388(BOOL, STR);
BOOL BOOL_i1290536107(BOOL, OB);
BOOL CALL_S1637743358(CALL_SIG, SIG);
BOOL CALL_T148273370(CALL_T561792367, dTP);
BOOL CALL_T16429326(CALL_TP_ARRAY, dTP);
BOOL CALL_T1680302827(CALL_TP_VOID, dTP);
BOOL CALL_T1822200530(CALL_T666389079, dTP);
BOOL CALL_T640058966(CALL_TP_CREATE, dTP);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
CALL_ARG ARRAYC494195798(ARRAYC494195798_frame);
FLISTA725283029 AM_ROU518743835(AM_ROU1916046290);
FLISTA725283029 FLISTA189055531(FLISTA725283029, FLISTA725283029);
FSETTP_CLASS TP_GRA583006971(TP_GRAPH, TP_CLASS);
FSETdTP TP_GRA2060761681(TP_GRAPH, TP_CLASS);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT AM_ROU2113525054(AM_ROU2113525054_frame);
INT AM_ROU67895179(AM_ROU1916046290);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT INT_hashrINT(INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
SFILE_ID AM_CAL1587798028(AM_CALL_ARG);
SFILE_ID AM_FOR97554330(AM_FORMAL_ARG);
SFILE_ID AM_ROU145143149(AM_ROU1916046290);
SIG CALL_S2120587481(CALL_SIG, BOOL);
SIG IFC_si178998979(IFC, CALL_SIG);
SIG IMPL_s773282892(IMPL, CALL_SIG);
SIG SIG_ma1442297600(SIG);
STR AS_OUT422425760(AS_OUT, STR);
STR BOOL_strrSTR(BOOL);
STR CALL_SIG_strrSTR(CALL_SIG);
STR CALL_T152540990(CALL_TP_ARRAY);
STR CALL_T1577548690(CALL_TP_CREATE);
STR CALL_T1666519150(CALL_T561792367);
STR CALL_T184446387(CALL_TP_VOID);
STR CALL_T47412966(CALL_T666389079);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_pl1270664985(STR, dSTR);
STR STR_se405450305(STR_se405450305_frame);
STR STR_upperrSTR(STR);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_ROUT_strrSTR(TP_ROUT);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TUPTP_858321267 FMAPTP1465407463(FMAPTP1465407463_frame);
dTP AM_ROU885465471(AM_ROU1916046290);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void AM_ROU545684847(AM_ROU545684847_frame);
void AS_OUT1019461868(AS_OUT, AS_VOID_EXPR);
void AS_OUT1070397350(AS_OUT, AS_RESULT_EXPR);
void AS_OUT1080486727(AS_OUT, AS_FEAT_MOD);
void AS_OUT1171738231(AS_OUT, dAS_NODE);
void AS_OUT1189782454(AS_OUT, AS_RETURN_STMT);
void AS_OUT1198591538(AS_OUT, AS_ARG_DEC);
void AS_OUT1208804252(AS_OUT, AS_CLASS_DEF);
void AS_OUT1211286518(AS_OUT, AS_CREATE_EXPR);
void AS_OUT1240003398(AS_OUT, AS_IF_STMT);
void AS_OUT1255290752(AS_OUT, AS_BOOL_LIT_EXPR);
void AS_OUT1317811396(AS_OUT, AS_QUIT_STMT);
void AS_OUT1331819376(AS_OUT, AS_LOOP_STMT);
void AS_OUT1347508463(AS_OUT, AS_INITIAL_EXPR);
void AS_OUT1365061583(AS_OUT, dAS_STMT);
void AS_OUT1372724668(AS_OUT, AS_TYPECASE_STMT);
void AS_OUT1434191198(AS_OUT, AS_OR_EXPR);
void AS_OUT148919342(AS_OUT, AS_ATTACH_STMT);
void AS_OUT1495750296(AS_OUT, AS_FORK_STMT);
void AS_OUT1499224135(AS_OUT, AS_PAR_STMT);
void AS_OUT1538624491(AS_OUT, AS_RAISE_STMT);
void AS_OUT1577197339(AS_OUT, AS_PROTECT_WHEN);
void AS_OUT160973005(AS_OUT, AS_INT_LIT_EXPR);
void AS_OUT1737761882(AS_OUT, AS_UNLOCK_STMT);
void AS_OUT1804680247(AS_OUT, AS_INT2031239268);
void AS_OUT1983800340(AS_OUT, AS_CALL_EXPR);
void AS_OUT2042091545(AS_OUT, AS_DEC_STMT);
void AS_OUT2138884099(AS_OUT, AS_UND152986614);
void AS_OUT2139428914(AS_OUT, dAS_CLASS_ELT);
void AS_OUT229230569(AS_OUT, AS_WIT1063437351);
void AS_OUT26552353(AS_OUT, AS_BREAK_EXPR);
void AS_OUT359892079(AS_OUT, AS_AND_EXPR);
void AS_OUT387909983(AS_OUT, AS_STR_LIT_EXPR);
void AS_OUT419203785(AS_OUT, AS_INC1416917001);
void AS_OUT422363112(AS_OUT, AS_CASE_WHEN);
void AS_OUT423700572(AS_OUT, AS_TYPECASE_WHEN);
void AS_OUT454374187(AS_OUT, AS_FLT_LIT_EXPR);
void AS_OUT460718931(AS_OUT, AS_SHARED_DEF);
void AS_OUT474809072(AS_OUT, AS_CASE_STMT);
void AS_OUT48907654(AS_OUT, AS_ASSIGN_STMT);
void AS_OUT516254387(AS_OUT, AS_ARRAY_EXPR);
void AS_OUT57117984(AS_OUT, AS_CHAR_LIT_EXPR);
void AS_OUT5759708(AS_OUT, AS_SYNC_STMT);
void AS_OUT599192453(AS_OUT, AS_NEW_EXPR);
void AS_OUT600758956(AS_OUT, AS_TYPE_SPEC);
void AS_OUT620392985(AS_OUT, AS_ROUT_DEF);
void AS_OUT662922118(AS_OUT, AS_EXCEPT_EXPR);
void AS_OUT749907512(AS_OUT, AS_STMT_LIST);
void AS_OUT767507057(AS_OUT, AS_IS_VOID_EXPR);
void AS_OUT774387720(AS_OUT, AS_CONST_DEF);
void AS_OUT778273489(AS_OUT, AS_PROTECT_STMT);
void AS_OUT804297708(AS_OUT, AS_SELF_EXPR);
void AS_OUT842936047(AS_OUT, AS_IDENT_LIST);
void AS_OUT849427428(AS_OUT, AS_LOCK_STMT);
void AS_OUT851761245(AS_OUT, dAS_EXPR);
void AS_OUT898752312(AS_OUT, AS_BOU14269336);
void AS_OUT916258175(AS_OUT, AS_LOCK_IF_WHEN);
void AS_OUT918185922(AS_OUT, AS_ASSERT_STMT);
void AS_OUT962799085(AS_OUT, AS_YIELD_STMT);
void AS_OUT964805355(AS_OUT, AS_ATTR_DEF);
void AS_OUT966193580(AS_OUT, AS_PARAM_DEC);
void AS_OUT995003644(AS_OUT, AS_EXPR_STMT);
void AS_OUT_ind(AS_OUT);
void BUILD_1202586967(BUILD_TYPE_GRAPH);
void BUILD_1386674981(BUILD_TYPE_GRAPH);
void BUILD_1583735792(BUILD_TYPE_GRAPH);
void BUILD_413411632(BUILD_TYPE_GRAPH);
void CALL_S371511472(CALL_SIG, dSTR, dSTR, dSTR, dSTR);
void CALL_S407987990(CALL_SIG, dSTR, dSTR, dSTR, dSTR, dSTR, dSTR);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PROG_err_STR(PROG, STR);
void TP_GRA1175712875(TP_GRAPH_ABS_DES);
void TP_GRA1175730507(TP_GRAPH_ABS_DES);
void TP_GRA1187239150(TP_GRAPH_ABS_DES);
void TP_GRA628060055(TP_GRAPH);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_CALL_ARG AM_CAL1030878588(AM_CALL_ARG self) {
 AM_CALL_ARG ret_val;
 AM_CALL_ARG r;
 AM_CALL_ARG L1;
 OB L2;
 dAM_EXPR L3;
 if ((self==((AM_CALL_ARG) NULL))) {
  ret_val = ((AM_CALL_ARG) NULL);
  return ret_val;
 }
 L2=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_CALL_ARG_tag;
 L1 = ((AM_CALL_ARG) L2);
 r = L1;
 L3 = ATTR(self,expr);
 SATTR(r,expr,(*dAM_EX1830308833[TAG(L3)])(L3));
 SATTR(r,mode1,ATTR(self,mode1));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ROU1916046290 AM_ROU960965786(AM_ROU1916046290 self) {
 AM_ROU1916046290 ret_val;
 AM_ROU1916046290 r;
 AM_ROU1916046290 L11;
 AM_CALL_ARG x;
 AM_ROU1916046290 L21;
 AM_ROU1916046290 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROU1916046290 ret_val1;
 AM_ROU1916046290 r1;
 INT L31 = INT_zero;
 INT L41_;
 AM_ROU1916046290 L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 AM_CALL_ARG aL91_;
 INT L101_br;
INT i_L101_=0;
 AM_CALL_ARG L12;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 if ((self==((AM_ROU1916046290) NULL))) {
  ret_val = ((AM_ROU1916046290) NULL);
  return ret_val;
 }
 create_self = ((AM_ROU1916046290) NULL);
 L41_=(self)==NULL?0:ASIZE((AM_ROU1916046290)self); 
 create_nargs = L41_;
 create_source = ATTR(self,source1);
 L6 = create_nargs;
 L8=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L6)*sizeof(AM_CALL_ARG);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((AM_ROU1916046290) L7);
 L5->asize = L6;
 r1 = L5;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,fun,ATTR(self,fun));
 SATTR(r,as_type,ATTR(self,as_type));
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = r;
  L91_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
  L101_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
  i_L101_=0;
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   aL91_=ARR((AM_ROU1916046290)L11,L31); 
   x = aL91_;
   L13 = (x==((AM_CALL_ARG) NULL));
   L141_=!(L13); 
   if (L141_) {
    x = AM_CAL1030878588(x);
   }
   if(i_L101_>=L101_br)  goto after_loop; 
   SARR((AM_ROU1916046290)L21,i_L101_,(AM_CALL_ARG)x); i_L101_++;
   ;
   L151_=INTPLUS(L31,1); 
   L31 = L151_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ROU2096608001(AM_ROU1916046290 self) {
 BOOL ret_val = BOOL_zero;
 SIG is_near_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 is_near_self = ATTR(self,fun);
 if (SIG_is418491101(is_near_self)) {
  L3 = ATTR(ATTR(is_near_self,builtin_info),is_near);
 } else {
  L3 = FALSE;
 }
 if (L3) {
  L2 = TRUE;
 } else {
  L4 = ATTR(is_near_self,ret);
  L2 = (*dTP_is1334578382[TAG(L4)])(L4);
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L6 = (ATTR(is_near_self,opt_info)==((OPT_LOCAL_CALL) NULL));
  L71_=!(L6); 
  if (L71_) {
   L5 = ATTR(ATTR(is_near_self,opt_info),is_near);
  } else {
   L5 = FALSE;
  }
  L1 = L5;
 }
 ret_val1 = L1;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ROU774362080(AM_ROU1916046290 self, OB a) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 INT i = INT_zero;
 dAM_EXPR x;
 dAM_EXPR y;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 INT L21 = INT_zero;
 AM_CALL_ARG is_eq_self1;
 AM_CALL_ARG is_eq_a;
 BOOL ret_val2 = BOOL_zero;
 BOOL L3;
 BOOL L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 INT L14;
 INT L151_;
 INT aL131_;
 dTP L16;
 STR L17;
 extern STR OPT_DEBUG;
 AM_CALL_ARG L181_;
 AM_CALL_ARG L19;
 AM_CALL_ARG L201_;
 AM_CALL_ARG L22;
 AM_CALL_ARG L231_;
 AM_CALL_ARG L241_;
 BOOL L25;
 dAM_EXPR L26;
 dMODE L27;
 BOOL L28;
 BOOL L291_;
 INT L301_;
 if (a==NULL) {
  goto other152;
 } else
 switch (TAG(a)) {
  case AM_ROU1916046290_tag:
   is_eq_self = ATTR(self,fun);
   is_eq_s = ATTR(((AM_ROU1916046290) a),fun);
   ret_val1 = (is_eq_self==is_eq_s);
   L3 = ret_val1;
   L41_=!(L3); 
   if (L41_) {
    ret_val = FALSE;
    return ret_val;
   }
   L51_=ASIZE((AM_ROU1916046290)self); 
   L6 = L51_;
   L71_=ASIZE((AM_ROU1916046290)a); 
   L8 = L71_;
   L91_=(L6)==(L8); 
   L10 = L91_;
   L121_=!(L10); 
   if (L121_) {
    ret_val = FALSE;
    return ret_val;
   }
   {
    BOOL f_L131_ = TRUE;
    /* loop index variable */
    L21 = 0;
    while (1) {
     if (f_L131_) {
      f_L131_ = FALSE;
      L151_=ASIZE((AM_ROU1916046290)self); 
      L11 = L151_;
     }
     if(L21>=L11)  goto after_loop; 
     aL131_=L21; 
     i = aL131_;
     x = ((dAM_EXPR) NULL);
     L16 = ATTR(ATTR(self,fun),tp);
     L17 = (*dTP_strrSTR[TAG(L16)])(L16);
     if (STR_is111530248(L17, ((STR) &OPT_DEBUG))) {
      L181_=(AM_CALL_ARG)ARR((AM_ROU1916046290)self,i); 
      L19=L181_;
      x = ATTR(L19,expr);
     }
     if (x==NULL) {
      goto other153;
     } else
     switch (TAG(x)) {
      case AM_INT_CONST_tag:
       L201_=(AM_CALL_ARG)ARR((AM_ROU1916046290)a,i); 
       L22=L201_;
       y = ATTR(L22,expr);
       if (y==NULL) {
        goto other154;
       } else
       switch (TAG(y)) {
        case AM_INT_CONST_tag: break;
        default: ;
        other154: ;
         ret_val = FALSE;
         return ret_val;
       } break;
      default: ;
      other153: ;
       L231_=(AM_CALL_ARG)ARR((AM_ROU1916046290)self,i); 
       is_eq_self1 = L231_;
       L241_=(AM_CALL_ARG)ARR((AM_ROU1916046290)a,i); 
       is_eq_a = L241_;
       L26 = ATTR(is_eq_self1,expr);
       if ((*dAM_EX2004971809[TAG(L26)])(L26, ((OB) ATTR(is_eq_a,expr)))) {
        L27 = ATTR(is_eq_self1,mode1);
        L25 = (*dMODE_814247358[TAG(L27)])(L27, ATTR(is_eq_a,mode1));
       } else {
        L25 = FALSE;
       }
       ret_val2 = L25;
       L28 = ret_val2;
       L291_=!(L28); 
       if (L291_) {
        ret_val = FALSE;
        return ret_val;
       }
     }
     L301_=INTPLUS(L21,1); 
     L21 = L301_;
    }
   }
   after_loop: ;
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other152: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL BOOL_f112211388(BOOL self, STR s) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 extern STR true1;
 extern STR t111;
 extern STR True1;
 extern STR T;
 extern STR TRUE1;
 extern STR false;
 extern STR f11;
 extern STR False1;
 extern STR F1;
 extern STR FALSE1;
 extern STR Cantin268249159;
 L11 = s;
 if (STR_is111530248(L11, ((STR) &true1))) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  if (STR_is111530248(L11, ((STR) &t111))) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   if (STR_is111530248(L11, ((STR) &True1))) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    if (STR_is111530248(L11, ((STR) &T))) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     if (STR_is111530248(L11, ((STR) &TRUE1))) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      if (STR_is111530248(L11, ((STR) &false))) {
       ret_val = FALSE;
       return ret_val;
      }
      else {
       if (STR_is111530248(L11, ((STR) &f11))) {
        ret_val = FALSE;
        return ret_val;
       }
       else {
        if (STR_is111530248(L11, ((STR) &False1))) {
         ret_val = FALSE;
         return ret_val;
        }
        else {
         if (STR_is111530248(L11, ((STR) &F1))) {
          ret_val = FALSE;
          return ret_val;
         }
         else {
          if (STR_is111530248(L11, ((STR) &FALSE1))) {
           ret_val = FALSE;
           return ret_val;
          }
          else {
           plus_self = ((STR) &Cantin268249159);
           plus_sarg = s;
           ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
           RAISE(((OB) ret_val1));
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL BOOL_i1290536107(BOOL self, OB arg) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 if (arg==NULL) {
  goto other161;
 } else
 switch (TAG(arg)) {
  case BOOL_tag:
   L1 = ((BOOL_boxed) arg)->immutable_part;
   L21_=self==L1; 
   ret_val = L21_;
   return ret_val; break;
  default: ;
  other161: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_S1637743358(CALL_SIG self, SIG s) {
 BOOL ret_val = BOOL_zero;
 ARRAYCALL_ARG L11;
 CALL_ARG car;
 ARRAYARG L21;
 ARG sar;
 ARRAYCALL_ARG L31;
 CALL_ARG ca;
 ARRAYARG L41;
 ARG sa1;
 dCALL_TP call_tp;
 dMODE ca_mode;
 dMODE sa_mode;
 dMODE L51;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 CALL_SIG prog_self;
 PROG ret_val2;
 BOOL L8;
 dTP L9;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 ARRAYCALL_ARG L19;
 INT L201_;
 INT L22;
 ARRAYARG L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 INT L291_br;
 CALL_ARG aL291_;
 INT L301_br;
 ARG aL301_;
 CALL_ARG L32;
 ARG L33;
 dMODE L34;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 dMODE L38;
 dMODE L39;
 INT L401_;
 INT L421_br;
 CALL_ARG aL421_;
 INT L431_br;
 ARG aL431_;
 CALL_ARG L44;
 ARG L45;
 dTP L46;
 dTP L47;
 BOOL L48;
 BOOL L491_;
 dTP L50;
 extern STR Dispat1077094544;
 dMODE L52;
 dCALL_TP L53;
 BOOL L54;
 BOOL L551_;
 dMODE L56;
 dTP L57;
 BOOL L58;
 BOOL L591_;
 dMODE L60;
 BOOL L62;
 BOOL L631_;
 dTP L64;
 BOOL L65;
 BOOL L661_;
 dTP L67;
 BOOL L68;
 BOOL L691_;
 dMODE L70;
 dCALL_TP L72;
 BOOL L73;
 BOOL L741_;
 INT L751_;
 if ((self==((CALL_SIG) NULL))) {
  L8 = TRUE;
 } else {
  L8 = (s==((SIG) NULL));
 }
 if (L8) {
  ret_val = FALSE;
  return ret_val;
 }
 L9 = ATTR(self,tp);
 L10 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) ATTR(s,tp)));
 L121_=!(L10); 
 if (L121_) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = ATTR(self,name1);
 is_eq_i = ATTR(s,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L13 = ret_val1;
 L141_=!(L13); 
 if (L141_) {
  ret_val = FALSE;
  return ret_val;
 }
 L15 = ATTR(self,unknown_ret);
 L161_=!(L15); 
 if (L161_) {
  if (ATTR(self,has_ret)) {
   if ((ATTR(s,ret)==((dTP) NULL))) {
    ret_val = FALSE;
    return ret_val;
   }
  }
  else {
   L17 = (ATTR(s,ret)==((dTP) NULL));
   L181_=!(L17); 
   if (L181_) {
    ret_val = FALSE;
    return ret_val;
   }
  }
 }
 L19 = ATTR(self,args);
 L201_=(L19)==NULL?0:ASIZE((ARRAYCALL_ARG)L19); 
 L22 = L201_;
 L23 = ATTR(s,args);
 L241_=(L23)==NULL?0:ASIZE((ARRAYARG)L23); 
 L25 = L241_;
 L261_=(L22)==(L25); 
 L27 = L261_;
 L281_=!(L27); 
 if (L281_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L291_ = TRUE;
  BOOL f_L301_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L11 = ATTR(self,args);
  L21 = ATTRs(s,args,((ARRAYARG) NULL));
  L291_br=L11==NULL?0:ASIZE((ARRAYCALL_ARG)L11); 
  L301_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L61>=L291_br)  goto after_loop; 
   aL291_=ARR((ARRAYCALL_ARG)L11,L61); 
   car = aL291_;
   if(L61>=L301_br)  goto after_loop; 
   aL301_=ARR((ARRAYARG)L21,L61); 
   sar = aL301_;
   L34 = ATTR(car,mode1);
   L35 = (*dMODE_814247358[TAG(L34)])(L34, ATTR(sar,mode1));
   L361_=!(L35); 
   if (L361_) {
    L38 = ATTR(car,mode1);
    if ((*dMODE_814247358[TAG(L38)])(L38, ((dMODE) MODES_in_mode))) {
     L39 = ATTR(sar,mode1);
     L37 = (*dMODE_814247358[TAG(L39)])(L39, ((dMODE) MODES_once_mode));
    } else {
     L37 = FALSE;
    }
    if (L37) {
     SATTR(car,mode1,ATTR(sar,mode1));
    }
    else {
     ret_val = FALSE;
     return ret_val;
    }
   }
   L401_=INTPLUS(L61,1); 
   L61 = L401_;
  }
 }
 after_loop: ;
 {
  BOOL f_L421_ = TRUE;
  BOOL f_L431_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L31 = ATTR(self,args);
  L41 = ATTRs(s,args,((ARRAYARG) NULL));
  L421_br=L31==NULL?0:ASIZE((ARRAYCALL_ARG)L31); 
  L431_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
  while (1) {
   if(L71>=L421_br)  goto after_loop1; 
   aL421_=ARR((ARRAYCALL_ARG)L31,L71); 
   ca = aL421_;
   if(L71>=L431_br)  goto after_loop1; 
   aL431_=ARR((ARRAYARG)L41,L71); 
   sa1 = aL431_;
   call_tp = ATTR(ca,tp);
   L46 = ATTR(sa1,tp);
   if ((*dTP_is735947028[TAG(L46)])(L46)) {
    if (call_tp==NULL) {
     goto other393;
    } else
    switch (TAG(call_tp)) {
     case TP_CLASS_tag:
      L47 = ATTR(sa1,tp);
      L48 = (*dTP_is242312711[TAG(L47)])(L47, ((OB) call_tp));
      L491_=!(L48); 
      if (L491_) {
       prog_self = self;
       L50 = ATTR(prog_self,tp);
       ret_val2 = (*dTP_progrPROG[TAG(L50)])(L50);
       PROG_err_STR(ret_val2, ((STR) &Dispat1077094544));
      } break;
     default: ;
     other393: ;
    }
   }
   ca_mode = ATTR(ca,mode1);
   sa_mode = ATTR(sa1,mode1);
   L51 = sa_mode;
   L52 = L51;
   if ((*dMODE_814247358[TAG(L52)])(L52, ((dMODE) MODES_in_mode))) {
    L53 = ATTR(ca,tp);
    L54 = (*dCALL_358880146[TAG(L53)])(L53, ATTR(sa1,tp));
    L551_=!(L54); 
    if (L551_) {
     ret_val = FALSE;
     return ret_val;
    }
   }
   else {
    L56 = L51;
    if ((*dMODE_814247358[TAG(L56)])(L56, ((dMODE) MODES_out_mode))) {
     L57 = ATTR(sa1,tp);
     L58 = (*dTP_is762676118[TAG(L57)])(L57, ATTR(ca,tp));
     L591_=!(L58); 
     if (L591_) {
      ret_val = FALSE;
      return ret_val;
     }
    }
    else {
     L60 = L51;
     if ((*dMODE_814247358[TAG(L60)])(L60, ((dMODE) MODES_inout_mode))) {
      call_tp = ATTR(ca,tp);
      if (call_tp==NULL) {
       goto other394;
      } else
      switch (TAG(call_tp)) {
       case CALL_T666389079_tag:
        L62 = (ATTR(((CALL_T666389079) call_tp),tp)==((dTP) NULL));
        L631_=!(L62); 
        if (L631_) {
         L64 = ATTR(((CALL_T666389079) call_tp),tp);
         L65 = (*dTP_is242312711[TAG(L64)])(L64, ((OB) ATTR(sa1,tp)));
         L661_=!(L65); 
         if (L661_) {
          ret_val = FALSE;
          return ret_val;
         }
        } break;
       default: ;
       other394: ;
        L67 = ATTR(sa1,tp);
        L68 = (*dTP_is242312711[TAG(L67)])(L67, ((OB) ATTR(ca,tp)));
        L691_=!(L68); 
        if (L691_) {
         ret_val = FALSE;
         return ret_val;
        }
      }
     }
     else {
      L70 = L51;
      if ((*dMODE_814247358[TAG(L70)])(L70, ((dMODE) MODES_once_mode))) {
       L72 = ATTR(ca,tp);
       L73 = (*dCALL_358880146[TAG(L72)])(L72, ATTR(sa1,tp));
       L741_=!(L73); 
       if (L741_) {
        ret_val = FALSE;
        return ret_val;
       }
      }
      else {
       FATAL("No applicable target in case statement\nin CALL_SIG::conforms_to(SIG):BOOL\n./Representation/call.sa:324:16");
      }
     }
    }
   }
   L751_=INTPLUS(L71,1); 
   L71 = L751_;
  }
 }
 after_loop1: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_T148273370(CALL_T561792367 self, dTP t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 dTP L2;
 L2 = t;
 L11 = (*dTP_kindrINT[TAG(L2)])(L2);
 switch (L11) {
  case 1: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 2: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 4: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 3: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 5: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 6: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 7: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 8: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 9: 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   FATAL("No applicable target in case statement\nin CALL_TP_BOUND_CREATE::is_subtype($TP):BOOL\n./Representation/call.sa:117:18");
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_T16429326(CALL_TP_ARRAY self, dTP t) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 extern STR ARRAY;
 if (t==NULL) {
  goto other392;
 } else
 switch (TAG(t)) {
  case TP_CLASS_tag:
   is_eq_self = ATTR(((TP_CLASS) t),name1);
   is_eq_i = IDENT_1150413730(IDENT_zero, ((STR) &ARRAY));
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other392: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_T1680302827(CALL_TP_VOID self, dTP t) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_T1822200530(CALL_T666389079 self, dTP t) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 if ((ATTR(self,tp)==((dTP) NULL))) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  L1 = ATTR(self,tp);
  ret_val = (*dTP_is1999456142[TAG(L1)])(L1, t);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CALL_T640058966(CALL_TP_CREATE self, dTP t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 dTP L2;
 L2 = t;
 L11 = (*dTP_kindrINT[TAG(L2)])(L2);
 switch (L11) {
  case 1: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 2: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 4: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 3: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 5: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 6: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 7: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 8: 
   ret_val = FALSE;
   return ret_val;
   break;
  case 9: 
   ret_val = FALSE;
   return ret_val;
   break;
  default: ;
   FATAL("No applicable target in case statement\nin CALL_TP_CREATE::is_subtype($TP):BOOL\n./Representation/call.sa:85:18");
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ROU518743835(AM_ROU1916046290 self) {
 FLISTA725283029 ret_val;
 ARRAYINT na;
 FLISTA725283029 nd = ((FLISTA725283029) NULL);
 ARRAYINT L11;
 AM_CALL_ARG argd;
 ARRAYINT L21;
 AM_ROU1916046290 is_near_self;
 BOOL ret_val1 = BOOL_zero;
 SIG is_near_self1;
 BOOL ret_val2 = BOOL_zero;
 INT L31 = INT_zero;
 AM_CALL_ARG near_depends_on_;
 FLISTA725283029 ret_val3;
 AM_CALL_ARG is_near_self2;
 BOOL ret_val4 = BOOL_zero;
 AM_CALL_ARG near_depends_on_1;
 FLISTA725283029 ret_val5;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 dTP L8;
 BOOL L9;
 BOOL L10;
 BOOL L121_;
 INT L131_br;
 INT aL131_;
 INT L141_br;
INT i_L141_=0;
 INT aL141_;
 INT L15;
 INT L16;
 AM_CALL_ARG L171_;
 BOOL L18;
 dAM_EXPR L19;
 dAM_EXPR L20;
 BOOL L22;
 BOOL L231_;
 INT L24;
 INT L25;
 AM_CALL_ARG L261_;
 dAM_EXPR L27;
 INT L281_;
 na = SIG_ne271652128(ATTR(self,fun));
 if ((na==((ARRAYINT) NULL))) {
  L4 = TRUE;
 } else {
  is_near_self = self;
  is_near_self1 = ATTR(is_near_self,fun);
  if (SIG_is418491101(is_near_self1)) {
   L7 = ATTR(ATTR(is_near_self1,builtin_info),is_near);
  } else {
   L7 = FALSE;
  }
  if (L7) {
   L6 = TRUE;
  } else {
   L8 = ATTR(is_near_self1,ret);
   L6 = (*dTP_is1334578382[TAG(L8)])(L8);
  }
  if (L6) {
   L5 = TRUE;
  } else {
   L10 = (ATTR(is_near_self1,opt_info)==((OPT_LOCAL_CALL) NULL));
   L121_=!(L10); 
   if (L121_) {
    L9 = ATTR(ATTR(is_near_self1,opt_info),is_near);
   } else {
    L9 = FALSE;
   }
   L5 = L9;
  }
  ret_val2 = L5;
  ret_val1 = ret_val2;
  L4 = ret_val1;
 }
 if (L4) {
  ret_val = ((FLISTA725283029) NULL);
  return ret_val;
 }
 {
  BOOL f_L131_ = TRUE;
  BOOL f_L141_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = na;
  L21 = na;
  L131_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  L141_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  i_L141_=0;
  while (1) {
   if(L31>=L131_br)  goto after_loop; 
   aL131_=ARR((ARRAYINT)L11,L31); 
   L16 = aL131_;
   L171_=(AM_CALL_ARG)ARR((AM_ROU1916046290)self,L16); 
   argd = L171_;
   near_depends_on_ = argd;
   L19 = ATTR(near_depends_on_,expr);
   ret_val3 = (*dAM_EX1656723676[TAG(L19)])(L19);
   if ((ret_val3==((FLISTA725283029) NULL))) {
    is_near_self2 = argd;
    L20 = ATTR(is_near_self2,expr);
    ret_val4 = (*dAM_EX1117610980[TAG(L20)])(L20);
    L22 = ret_val4;
    L231_=!(L22); 
    L18 = L231_;
   } else {
    L18 = FALSE;
   }
   if (L18) {
    ret_val = ((FLISTA725283029) NULL);
    return ret_val;
   }
   if(i_L141_>=L141_br)  goto after_loop; 
   aL141_=ARR((ARRAYINT)L21,i_L141_); i_L141_++;
   L25 = aL141_;
   L261_=(AM_CALL_ARG)ARR((AM_ROU1916046290)self,L25); 
   near_depends_on_1 = L261_;
   L27 = ATTR(near_depends_on_1,expr);
   ret_val5 = (*dAM_EX1656723676[TAG(L27)])(L27);
   nd = FLISTA189055531(nd, ret_val5);
   L281_=INTPLUS(L31,1); 
   L31 = L281_;
  }
 }
 after_loop: ;
 ret_val = nd;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ROU67895179(AM_ROU1916046290 self) {
 INT ret_val = INT_zero;
 INT htemp = INT_zero;
 INT L11 = INT_zero;
 INT i = INT_zero;
 SIG hash_self;
 INT ret_val1 = INT_zero;
 INT L21 = INT_zero;
 INT L31_;
 INT L5;
 INT L61_;
 INT aL41_;
 AM_CALL_ARG L71_;
 AM_CALL_ARG L8;
 dAM_EXPR L9;
 INT L10;
 INT L121_;
 INT L131_;
 hash_self = ATTR(self,fun);
 L31_=SYSID(hash_self); 
 ret_val1 = INT_hashrINT(L31_);
 htemp = ret_val1;
 {
  BOOL f_L41_ = TRUE;
  /* loop index variable */
  L21 = 0;
  while (1) {
   if (f_L41_) {
    f_L41_ = FALSE;
    L61_=ASIZE((AM_ROU1916046290)self); 
    L11 = L61_;
   }
   if(L21>=L11)  goto after_loop; 
   aL41_=L21; 
   i = aL41_;
   htemp = INT_hashrINT(htemp);
   L71_=(AM_CALL_ARG)ARR((AM_ROU1916046290)self,i); 
   L8=L71_;
   L9 = ATTR(L8,expr);
   L10 = (*dAM_EX1001627434[TAG(L9)])(L9);
   L121_=htemp^L10; 
   htemp = L121_;
   L131_=INTPLUS(L21,1); 
   L21 = L131_;
  }
 }
 after_loop: ;
 ret_val = htemp;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CAL1587798028(AM_CALL_ARG self) {
 SFILE_ID ret_val = SFILE_ID_zero;
 dAM_EXPR L1;
 L1 = ATTR(self,expr);
 ret_val = (*dAM_EX2051891691[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FOR97554330(AM_FORMAL_ARG self) {
 SFILE_ID ret_val = SFILE_ID_zero;
 ret_val = ATTR(ATTR(self,expr),source1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ROU145143149(AM_ROU1916046290 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SIG CALL_S2120587481(CALL_SIG self, BOOL in_class) {
 SIG ret_val;
 dTP st;
 ARRAYCALL_ARG L11;
 CALL_ARG arg;
 ARRAYARG L21;
 ARG br_arg;
 dMODE arg_mode;
 dMODE br_arg_mode;
 dMODE L31;
 ARRAYCALL_ARG L41;
 CALL_ARG arg11;
 ARRAYARG L51;
 ARG br_arg1;
 dMODE arg_mode1;
 dMODE br_arg_mode1;
 dMODE L61;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CALL_SIG prog_self;
 PROG ret_val2;
 CALL_SIG prog_self1;
 PROG ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 CALL_SIG prog_self2;
 PROG ret_val6;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 CALL_SIG prog_self3;
 PROG ret_val9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val12;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val13;
 INT L71 = INT_zero;
 SIG bound_routine_ca;
 TP_ROUT bound_routine_ca1;
 SIG ret_val14;
 SIG r;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val15 = BOOL_zero;
 CALL_SIG prog_self4;
 PROG ret_val16;
 CALL_SIG prog_self5;
 PROG ret_val17;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val19;
 CALL_SIG prog_self6;
 PROG ret_val20;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val21;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val22;
 CALL_SIG prog_self7;
 PROG ret_val23;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val24;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val25;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val26;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val27;
 INT L81 = INT_zero;
 SIG bound_iter_call_;
 TP_ITER bound_iter_call_1;
 SIG ret_val28;
 SIG r1;
 dTP L9;
 IMPL L10;
 dTP L12;
 IFC L13;
 BOOL L14;
 BOOL L151_;
 dTP L16;
 extern STR Onlyca1521382942;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 dTP L20;
 extern STR Theboundroutine;
 extern STR hasnor813490359;
 BOOL L22;
 BOOL L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 dTP L27;
 extern STR Theboundroutine;
 extern STR hasare2086446435;
 ARRAYARG L28;
 INT L291_;
 INT L30;
 ARRAYCALL_ARG L32;
 INT L331_;
 INT L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 dTP L38;
 extern STR Thecall;
 extern STR hasthe1782842755;
 extern STR name18;
 INT L391_br;
 CALL_ARG aL391_;
 INT L401_br;
 ARG aL401_;
 CALL_ARG L42;
 ARG L43;
 dMODE L44;
 BOOL L45;
 BOOL L461_;
 STR L47;
 dCALL_TP L48;
 STR L49;
 dMODE L50;
 STR L52;
 STR L53;
 dTP L54;
 STR L55;
 dMODE L56;
 dMODE L57;
 dCALL_TP L58;
 BOOL L59;
 BOOL L601_;
 STR L62;
 dCALL_TP L63;
 STR L64;
 STR L65;
 dTP L66;
 dMODE L67;
 dTP L68;
 BOOL L69;
 BOOL L701_;
 STR L72;
 dCALL_TP L73;
 STR L74;
 STR L75;
 dTP L76;
 dMODE L77;
 dTP L78;
 BOOL L79;
 BOOL L801_;
 STR L82;
 dCALL_TP L83;
 STR L84;
 STR L85;
 dTP L86;
 dMODE L87;
 dCALL_TP L88;
 BOOL L89;
 BOOL L901_;
 STR L91;
 dCALL_TP L92;
 STR L93;
 STR L94;
 dTP L95;
 INT L961_;
 SIG L97;
 OB L98;
 BOOL L99;
 BOOL L1001_;
 dTP L101;
 extern STR Onlyca1297144567;
 BOOL L102;
 BOOL L1031_;
 BOOL L104;
 dTP L105;
 extern STR Thebounditer;
 extern STR hasnor813490359;
 BOOL L106;
 BOOL L107;
 BOOL L1081_;
 BOOL L109;
 BOOL L1101_;
 dTP L111;
 extern STR Thebounditer;
 extern STR hasare2086446435;
 ARRAYARG L112;
 INT L1131_;
 INT L114;
 ARRAYCALL_ARG L115;
 INT L1161_;
 INT L117;
 BOOL L1181_;
 BOOL L119;
 BOOL L1201_;
 dTP L121;
 extern STR Thecall;
 extern STR hasthe1782842755;
 extern STR name18;
 INT L1221_br;
 CALL_ARG aL1221_;
 INT L1231_br;
 ARG aL1231_;
 CALL_ARG L124;
 ARG L125;
 dMODE L126;
 BOOL L127;
 BOOL L1281_;
 BOOL L129;
 dMODE L130;
 dMODE L131;
 STR L132;
 dCALL_TP L133;
 STR L134;
 dMODE L135;
 STR L136;
 STR L137;
 dTP L138;
 STR L139;
 dMODE L140;
 dMODE L141;
 dCALL_TP L142;
 BOOL L143;
 BOOL L1441_;
 STR L145;
 dCALL_TP L146;
 STR L147;
 STR L148;
 dTP L149;
 dMODE L150;
 dTP L152;
 BOOL L153;
 BOOL L1541_;
 STR L155;
 dCALL_TP L156;
 STR L157;
 STR L158;
 dTP L159;
 dMODE L160;
 dTP L161;
 BOOL L162;
 BOOL L1631_;
 STR L164;
 dCALL_TP L165;
 STR L166;
 STR L167;
 dTP L168;
 dMODE L169;
 dCALL_TP L170;
 BOOL L171;
 BOOL L1721_;
 STR L173;
 dCALL_TP L174;
 STR L175;
 STR L176;
 dTP L177;
 INT L1781_;
 SIG L179;
 OB L180;
 st = ATTR(self,tp);
 switch (TAG(st)) {
  case TP_CLASS_tag:
   if (in_class) {
    L9 = ATTR(self,tp);
    L10 = (*dTP_implrIMPL[TAG(L9)])(L9);
    ret_val = IMPL_s773282892(L10, self);
    return ret_val;
   }
   else {
    L12 = ATTR(self,tp);
    L13 = (*dTP_ifcrIFC[TAG(L12)])(L12);
    ret_val = IFC_si178998979(L13, self);
    return ret_val;
   } break;
  case TP_ROUT_tag:
   is_eq_self = ATTR(self,name1);
   is_eq_i = IDENT_2110206590;
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   L14 = ret_val1;
   L151_=!(L14); 
   if (L151_) {
    prog_self = self;
    L16 = ATTR(prog_self,tp);
    ret_val2 = (*dTP_progrPROG[TAG(L16)])(L16);
    PROG_err_STR(ret_val2, ((STR) &Onlyca1521382942));
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   L17 = ATTR(self,unknown_ret);
   L181_=!(L17); 
   if (L181_) {
    if (ATTR(self,has_ret)) {
     L19 = (ATTR(((TP_ROUT) st),ret)==((dTP) NULL));
    } else {
     L19 = FALSE;
    }
    if (L19) {
     prog_self1 = self;
     L20 = ATTR(prog_self1,tp);
     ret_val3 = (*dTP_progrPROG[TAG(L20)])(L20);
     plus_self = ((STR) &Theboundroutine);
     plus_sarg = TP_ROUT_strrSTR(((TP_ROUT) st));
     ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val4;
     plus_sarg1 = ((STR) &hasnor813490359);
     ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
     PROG_err_STR(ret_val3, ret_val5);
     ret_val = ((SIG) NULL);
     return ret_val;
    }
    else {
     L23 = ATTR(self,has_ret);
     L241_=!(L23); 
     if (L241_) {
      L25 = (ATTR(((TP_ROUT) st),ret)==((dTP) NULL));
      L261_=!(L25); 
      L22 = L261_;
     } else {
      L22 = FALSE;
     }
     if (L22) {
      prog_self2 = self;
      L27 = ATTR(prog_self2,tp);
      ret_val6 = (*dTP_progrPROG[TAG(L27)])(L27);
      plus_self2 = ((STR) &Theboundroutine);
      plus_sarg2 = TP_ROUT_strrSTR(((TP_ROUT) st));
      ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
      plus_self3 = ret_val7;
      plus_sarg3 = ((STR) &hasare2086446435);
      ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
      PROG_err_STR(ret_val6, ret_val8);
      ret_val = ((SIG) NULL);
      return ret_val;
     }
    }
   }
   L28 = ATTR(((TP_ROUT) st),args);
   L291_=(L28)==NULL?0:ASIZE((ARRAYARG)L28); 
   L30 = L291_;
   L32 = ATTR(self,args);
   L331_=(L32)==NULL?0:ASIZE((ARRAYCALL_ARG)L32); 
   L34 = L331_;
   L351_=(L30)==(L34); 
   L36 = L351_;
   L371_=!(L36); 
   if (L371_) {
    prog_self3 = self;
    L38 = ATTR(prog_self3,tp);
    ret_val9 = (*dTP_progrPROG[TAG(L38)])(L38);
    plus_self4 = ((STR) &Thecall);
    plus_sarg4 = CALL_SIG_strrSTR(self);
    ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val10;
    plus_sarg5 = ((STR) &hasthe1782842755);
    ret_val11 = STR_ap2004550586(plus_self5, plus_sarg5);
    plus_self6 = ret_val11;
    plus_sarg6 = TP_ROUT_strrSTR(((TP_ROUT) st));
    ret_val12 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val12;
    plus_sarg7 = ((STR) &name18);
    ret_val13 = STR_ap2004550586(plus_self7, plus_sarg7);
    PROG_err_STR(ret_val9, ret_val13);
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   {
    BOOL f_L391_ = TRUE;
    BOOL f_L401_ = TRUE;
    /* loop index variable */
    L71 = 0;
    L11 = ATTR(self,args);
    L21 = ATTRs(((TP_ROUT) st),args,((ARRAYARG) NULL));
    L391_br=L11==NULL?0:ASIZE((ARRAYCALL_ARG)L11); 
    L401_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
    while (1) {
     if(L71>=L391_br)  goto after_loop; 
     aL391_=ARR((ARRAYCALL_ARG)L11,L71); 
     arg = aL391_;
     if(L71>=L401_br)  goto after_loop; 
     aL401_=ARR((ARRAYARG)L21,L71); 
     br_arg = aL401_;
     arg_mode = ATTR(arg,mode1);
     br_arg_mode = ATTR(br_arg,mode1);
     L44 = arg_mode;
     L45 = (*dMODE_814247358[TAG(L44)])(L44, br_arg_mode);
     L461_=!(L45); 
     if (L461_) {
      L47 = CALL_SIG_strrSTR(self);
      L48 = ATTR(arg,tp);
      L49 = (*dCALL_TP_strrSTR[TAG(L48)])(L48);
      L50 = ATTR(arg,mode1);
      L52 = (*dMODE_strrSTR[TAG(L50)])(L50);
      L53 = TP_ROUT_strrSTR(((TP_ROUT) st));
      L54 = ATTR(br_arg,tp);
      L55 = (*dTP_strrSTR[TAG(L54)])(L54);
      L56 = ATTR(br_arg,mode1);
      CALL_S407987990(self, ((dSTR) L47), ((dSTR) L49), ((dSTR) L52), ((dSTR) L53), ((dSTR) L55), ((dSTR) (*dMODE_strrSTR[TAG(L56)])(L56)));
      ret_val = ((SIG) NULL);
      return ret_val;
     }
     L31 = arg_mode;
     L57 = L31;
     if ((*dMODE_814247358[TAG(L57)])(L57, ((dMODE) MODES_in_mode))) {
      L58 = ATTR(arg,tp);
      L59 = (*dCALL_358880146[TAG(L58)])(L58, ATTR(br_arg,tp));
      L601_=!(L59); 
      if (L601_) {
       L62 = CALL_SIG_strrSTR(self);
       L63 = ATTR(arg,tp);
       L64 = (*dCALL_TP_strrSTR[TAG(L63)])(L63);
       L65 = TP_ROUT_strrSTR(((TP_ROUT) st));
       L66 = ATTR(br_arg,tp);
       CALL_S371511472(self, ((dSTR) L62), ((dSTR) L64), ((dSTR) L65), ((dSTR) (*dTP_strrSTR[TAG(L66)])(L66)));
       ret_val = ((SIG) NULL);
       return ret_val;
      }
     }
     else {
      L67 = L31;
      if ((*dMODE_814247358[TAG(L67)])(L67, ((dMODE) MODES_out_mode))) {
       L68 = ATTR(br_arg,tp);
       L69 = (*dTP_is762676118[TAG(L68)])(L68, ATTR(arg,tp));
       L701_=!(L69); 
       if (L701_) {
        L72 = CALL_SIG_strrSTR(self);
        L73 = ATTR(arg,tp);
        L74 = (*dCALL_TP_strrSTR[TAG(L73)])(L73);
        L75 = TP_ROUT_strrSTR(((TP_ROUT) st));
        L76 = ATTR(br_arg,tp);
        CALL_S371511472(self, ((dSTR) L72), ((dSTR) L74), ((dSTR) L75), ((dSTR) (*dTP_strrSTR[TAG(L76)])(L76)));
        ret_val = ((SIG) NULL);
        return ret_val;
       }
      }
      else {
       L77 = L31;
       if ((*dMODE_814247358[TAG(L77)])(L77, ((dMODE) MODES_inout_mode))) {
        L78 = ATTR(br_arg,tp);
        L79 = (*dTP_is242312711[TAG(L78)])(L78, ((OB) ATTR(arg,tp)));
        L801_=!(L79); 
        if (L801_) {
         L82 = CALL_SIG_strrSTR(self);
         L83 = ATTR(arg,tp);
         L84 = (*dCALL_TP_strrSTR[TAG(L83)])(L83);
         L85 = TP_ROUT_strrSTR(((TP_ROUT) st));
         L86 = ATTR(br_arg,tp);
         CALL_S371511472(self, ((dSTR) L82), ((dSTR) L84), ((dSTR) L85), ((dSTR) (*dTP_strrSTR[TAG(L86)])(L86)));
         ret_val = ((SIG) NULL);
         return ret_val;
        }
       }
       else {
        L87 = L31;
        if ((*dMODE_814247358[TAG(L87)])(L87, ((dMODE) MODES_once_mode))) {
         L88 = ATTR(arg,tp);
         L89 = (*dCALL_358880146[TAG(L88)])(L88, ATTR(br_arg,tp));
         L901_=!(L89); 
         if (L901_) {
          L91 = CALL_SIG_strrSTR(self);
          L92 = ATTR(arg,tp);
          L93 = (*dCALL_TP_strrSTR[TAG(L92)])(L92);
          L94 = TP_ROUT_strrSTR(((TP_ROUT) st));
          L95 = ATTR(br_arg,tp);
          CALL_S371511472(self, ((dSTR) L91), ((dSTR) L93), ((dSTR) L94), ((dSTR) (*dTP_strrSTR[TAG(L95)])(L95)));
          ret_val = ((SIG) NULL);
          return ret_val;
         }
        }
        else {
         FATAL("No applicable target in case statement\nin CALL_SIG::lookup(BOOL):SIG\n./Representation/call.sa:426:18");
        }
       }
      }
     }
     L961_=INTPLUS(L71,1); 
     L71 = L961_;
    }
   }
   after_loop: ;
   bound_routine_ca = ((SIG) NULL);
   bound_routine_ca1 = ((TP_ROUT) st);
   L98=ZALLOC(sizeof(struct SIG_struct));
   if (L98==NULL) FATAL("Unable to allocate more memory");
   ((OB)L98)->header.tag=SIG_tag;
   L97 = ((SIG) L98);
   r = L97;
   SATTR(r,tp,((dTP) bound_routine_ca1));
   SATTR(r,name1,IDENT_2110206590);
   SATTR(r,args,ATTR(bound_routine_ca1,args));
   SATTR(r,ret,ATTR(bound_routine_ca1,ret));
   ret_val14 = SIG_ma1442297600(r);
   ret_val = ret_val14;
   return ret_val; break;
  case TP_ITER_tag:
   is_eq_self1 = ATTR(self,name1);
   is_eq_i1 = IDENT_327685657;
   ret_val15 = (is_eq_self1.str==is_eq_i1.str);
   L99 = ret_val15;
   L1001_=!(L99); 
   if (L1001_) {
    prog_self4 = self;
    L101 = ATTR(prog_self4,tp);
    ret_val16 = (*dTP_progrPROG[TAG(L101)])(L101);
    PROG_err_STR(ret_val16, ((STR) &Onlyca1297144567));
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   L102 = ATTR(self,unknown_ret);
   L1031_=!(L102); 
   if (L1031_) {
    if (ATTR(self,has_ret)) {
     L104 = (ATTR(((TP_ITER) st),ret)==((dTP) NULL));
    } else {
     L104 = FALSE;
    }
    if (L104) {
     prog_self5 = self;
     L105 = ATTR(prog_self5,tp);
     ret_val17 = (*dTP_progrPROG[TAG(L105)])(L105);
     plus_self8 = ((STR) &Thebounditer);
     plus_sarg8 = TP_ITER_strrSTR(((TP_ITER) st));
     ret_val18 = STR_ap2004550586(plus_self8, plus_sarg8);
     plus_self9 = ret_val18;
     plus_sarg9 = ((STR) &hasnor813490359);
     ret_val19 = STR_ap2004550586(plus_self9, plus_sarg9);
     PROG_err_STR(ret_val17, ret_val19);
     ret_val = ((SIG) NULL);
     return ret_val;
    }
    else {
     L107 = ATTR(self,has_ret);
     L1081_=!(L107); 
     if (L1081_) {
      L109 = (ATTR(((TP_ITER) st),ret)==((dTP) NULL));
      L1101_=!(L109); 
      L106 = L1101_;
     } else {
      L106 = FALSE;
     }
     if (L106) {
      prog_self6 = self;
      L111 = ATTR(prog_self6,tp);
      ret_val20 = (*dTP_progrPROG[TAG(L111)])(L111);
      plus_self10 = ((STR) &Thebounditer);
      plus_sarg10 = TP_ITER_strrSTR(((TP_ITER) st));
      ret_val21 = STR_ap2004550586(plus_self10, plus_sarg10);
      plus_self11 = ret_val21;
      plus_sarg11 = ((STR) &hasare2086446435);
      ret_val22 = STR_ap2004550586(plus_self11, plus_sarg11);
      PROG_err_STR(ret_val20, ret_val22);
      ret_val = ((SIG) NULL);
      return ret_val;
     }
    }
   }
   L112 = ATTR(((TP_ITER) st),args);
   L1131_=(L112)==NULL?0:ASIZE((ARRAYARG)L112); 
   L114 = L1131_;
   L115 = ATTR(self,args);
   L1161_=(L115)==NULL?0:ASIZE((ARRAYCALL_ARG)L115); 
   L117 = L1161_;
   L1181_=(L114)==(L117); 
   L119 = L1181_;
   L1201_=!(L119); 
   if (L1201_) {
    prog_self7 = self;
    L121 = ATTR(prog_self7,tp);
    ret_val23 = (*dTP_progrPROG[TAG(L121)])(L121);
    plus_self12 = ((STR) &Thecall);
    plus_sarg12 = CALL_SIG_strrSTR(self);
    ret_val24 = STR_ap2004550586(plus_self12, plus_sarg12);
    plus_self13 = ret_val24;
    plus_sarg13 = ((STR) &hasthe1782842755);
    ret_val25 = STR_ap2004550586(plus_self13, plus_sarg13);
    plus_self14 = ret_val25;
    plus_sarg14 = TP_ITER_strrSTR(((TP_ITER) st));
    ret_val26 = STR_ap2004550586(plus_self14, plus_sarg14);
    plus_self15 = ret_val26;
    plus_sarg15 = ((STR) &name18);
    ret_val27 = STR_ap2004550586(plus_self15, plus_sarg15);
    PROG_err_STR(ret_val23, ret_val27);
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   {
    BOOL f_L1221_ = TRUE;
    BOOL f_L1231_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L41 = ATTR(self,args);
    L51 = ATTRs(((TP_ITER) st),args,((ARRAYARG) NULL));
    L1221_br=L41==NULL?0:ASIZE((ARRAYCALL_ARG)L41); 
    L1231_br=L51==NULL?0:ASIZE((ARRAYARG)L51); 
    while (1) {
     if(L81>=L1221_br)  goto after_loop1; 
     aL1221_=ARR((ARRAYCALL_ARG)L41,L81); 
     arg11 = aL1221_;
     if(L81>=L1231_br)  goto after_loop1; 
     aL1231_=ARR((ARRAYARG)L51,L81); 
     br_arg1 = aL1231_;
     arg_mode1 = ATTR(arg11,mode1);
     br_arg_mode1 = ATTR(br_arg1,mode1);
     L126 = arg_mode1;
     L127 = (*dMODE_814247358[TAG(L126)])(L126, br_arg_mode1);
     L1281_=!(L127); 
     if (L1281_) {
      L130 = arg_mode1;
      if ((*dMODE_814247358[TAG(L130)])(L130, ((dMODE) MODES_in_mode))) {
       L131 = br_arg_mode1;
       L129 = (*dMODE_814247358[TAG(L131)])(L131, ((dMODE) MODES_once_mode));
      } else {
       L129 = FALSE;
      }
      if (L129) {
       SATTR(arg11,mode1,br_arg_mode1);
      }
      else {
       L132 = CALL_SIG_strrSTR(self);
       L133 = ATTR(arg11,tp);
       L134 = (*dCALL_TP_strrSTR[TAG(L133)])(L133);
       L135 = ATTR(arg11,mode1);
       L136 = (*dMODE_strrSTR[TAG(L135)])(L135);
       L137 = TP_ITER_strrSTR(((TP_ITER) st));
       L138 = ATTR(br_arg1,tp);
       L139 = (*dTP_strrSTR[TAG(L138)])(L138);
       L140 = ATTR(br_arg1,mode1);
       CALL_S407987990(self, ((dSTR) L132), ((dSTR) L134), ((dSTR) L136), ((dSTR) L137), ((dSTR) L139), ((dSTR) (*dMODE_strrSTR[TAG(L140)])(L140)));
       ret_val = ((SIG) NULL);
       return ret_val;
      }
     }
     L61 = arg_mode1;
     L141 = L61;
     if ((*dMODE_814247358[TAG(L141)])(L141, ((dMODE) MODES_in_mode))) {
      L142 = ATTR(arg11,tp);
      L143 = (*dCALL_358880146[TAG(L142)])(L142, ATTR(br_arg1,tp));
      L1441_=!(L143); 
      if (L1441_) {
       L145 = CALL_SIG_strrSTR(self);
       L146 = ATTR(arg11,tp);
       L147 = (*dCALL_TP_strrSTR[TAG(L146)])(L146);
       L148 = TP_ITER_strrSTR(((TP_ITER) st));
       L149 = ATTR(br_arg1,tp);
       CALL_S371511472(self, ((dSTR) L145), ((dSTR) L147), ((dSTR) L148), ((dSTR) (*dTP_strrSTR[TAG(L149)])(L149)));
       ret_val = ((SIG) NULL);
       return ret_val;
      }
     }
     else {
      L150 = L61;
      if ((*dMODE_814247358[TAG(L150)])(L150, ((dMODE) MODES_out_mode))) {
       L152 = ATTR(br_arg1,tp);
       L153 = (*dTP_is762676118[TAG(L152)])(L152, ATTR(arg11,tp));
       L1541_=!(L153); 
       if (L1541_) {
        L155 = CALL_SIG_strrSTR(self);
        L156 = ATTR(arg11,tp);
        L157 = (*dCALL_TP_strrSTR[TAG(L156)])(L156);
        L158 = TP_ITER_strrSTR(((TP_ITER) st));
        L159 = ATTR(br_arg1,tp);
        CALL_S371511472(self, ((dSTR) L155), ((dSTR) L157), ((dSTR) L158), ((dSTR) (*dTP_strrSTR[TAG(L159)])(L159)));
        ret_val = ((SIG) NULL);
        return ret_val;
       }
      }
      else {
       L160 = L61;
       if ((*dMODE_814247358[TAG(L160)])(L160, ((dMODE) MODES_inout_mode))) {
        L161 = ATTR(br_arg1,tp);
        L162 = (*dTP_is242312711[TAG(L161)])(L161, ((OB) ATTR(arg11,tp)));
        L1631_=!(L162); 
        if (L1631_) {
         L164 = CALL_SIG_strrSTR(self);
         L165 = ATTR(arg11,tp);
         L166 = (*dCALL_TP_strrSTR[TAG(L165)])(L165);
         L167 = TP_ITER_strrSTR(((TP_ITER) st));
         L168 = ATTR(br_arg1,tp);
         CALL_S371511472(self, ((dSTR) L164), ((dSTR) L166), ((dSTR) L167), ((dSTR) (*dTP_strrSTR[TAG(L168)])(L168)));
         ret_val = ((SIG) NULL);
         return ret_val;
        }
       }
       else {
        L169 = L61;
        if ((*dMODE_814247358[TAG(L169)])(L169, ((dMODE) MODES_once_mode))) {
         L170 = ATTR(arg11,tp);
         L171 = (*dCALL_358880146[TAG(L170)])(L170, ATTR(br_arg1,tp));
         L1721_=!(L171); 
         if (L1721_) {
          L173 = CALL_SIG_strrSTR(self);
          L174 = ATTR(arg11,tp);
          L175 = (*dCALL_TP_strrSTR[TAG(L174)])(L174);
          L176 = TP_ITER_strrSTR(((TP_ITER) st));
          L177 = ATTR(br_arg1,tp);
          CALL_S371511472(self, ((dSTR) L173), ((dSTR) L175), ((dSTR) L176), ((dSTR) (*dTP_strrSTR[TAG(L177)])(L177)));
          ret_val = ((SIG) NULL);
          return ret_val;
         }
        }
        else {
         FATAL("No applicable target in case statement\nin CALL_SIG::lookup(BOOL):SIG\n./Representation/call.sa:487:18");
        }
       }
      }
     }
     L1781_=INTPLUS(L81,1); 
     L81 = L1781_;
    }
   }
   after_loop1: ;
   bound_iter_call_ = ((SIG) NULL);
   bound_iter_call_1 = ((TP_ITER) st);
   L180=ZALLOC(sizeof(struct SIG_struct));
   if (L180==NULL) FATAL("Unable to allocate more memory");
   ((OB)L180)->header.tag=SIG_tag;
   L179 = ((SIG) L180);
   r1 = L179;
   SATTR(r1,tp,((dTP) bound_iter_call_1));
   SATTR(r1,name1,IDENT_327685657);
   SATTR(r1,args,ATTR(bound_iter_call_1,args));
   SATTR(r1,hot,ATTR(bound_iter_call_1,hot));
   SATTR(r1,ret,ATTR(bound_iter_call_1,ret));
   ret_val28 = SIG_ma1442297600(r1);
   ret_val = ret_val28;
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin CALL_SIG::lookup(BOOL):SIG\n./Representation/call.sa:396:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

STR BOOL_strrSTR(BOOL self) {
 STR ret_val;
 extern STR true1;
 extern STR false;
 if (self) {
  ret_val = ((STR) &true1);
  return ret_val;
 }
 else {
  ret_val = ((STR) &false);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_SIG_strrSTR(CALL_SIG self) {
 STR ret_val;
 FSTR s;
 ARRAYCALL_ARG L11;
 CALL_ARG arg;
 STR L21;
 FSTR create_self;
 FSTR ret_val1;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 INT L31 = INT_zero;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val3;
 FSTR plus_self2;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val4;
 FSTR str_self;
 STR ret_val5;
 extern STR void1;
 FSTR L4;
 INT L5;
 OB L6;
 INT L7;
 FSTR L8;
 dTP L9;
 FSTR L10;
 extern STR name34;
 FSTR L12;
 BOOL L13;
 BOOL L141_;
 extern STR name3;
 INT L151_br;
 CALL_ARG aL151_;
 CALL_ARG L16;
 STR L17;
 dMODE L18;
 dCALL_TP L19;
 INT L201_;
 extern STR name150;
 extern STR S_1;
 if ((self==((CALL_SIG) NULL))) {
  ret_val = ((STR) &void1);
  return ret_val;
 }
 create_self = ((FSTR) NULL);
 L5 = 16;
 L7=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((FSTR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 L8 = ret_val1;
 L9 = ATTR(self,tp);
 L10 = FSTR_p1752847026(L8, (*dTP_strrSTR[TAG(L9)])(L9));
 L12 = FSTR_p1752847026(L10, ((STR) &name34));
 s = FSTR_p1752847026(L12, ATTR(self,name1).str);
 L13 = (ATTR(self,args)==((ARRAYCALL_ARG) NULL));
 L141_=!(L13); 
 if (L141_) {
  plus_self = s;
  plus_c = '(';
  ret_val2 = FSTR_p399773021(plus_self, plus_c);
  s = ret_val2;
  {
   BOOL f_L151_ = TRUE;
   struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
   /* loop index variable */
   L31 = 0;
   L11 = ATTR(self,args);
   L21 = ((STR) &name3);
   L151_br=L11==NULL?0:ASIZE((ARRAYCALL_ARG)L11); 
   noname1->self = L21;
   noname1->state = 0;
   while (1) {
    if(L31>=L151_br)  goto after_loop; 
    aL151_=ARR((ARRAYCALL_ARG)L11,L31); 
    arg = aL151_;
    L18 = ATTR(arg,mode1);
    plus_self1 = (*dMODE_strrSTR[TAG(L18)])(L18);
    L19 = ATTR(arg,tp);
    plus_sarg = (*dCALL_TP_strrSTR[TAG(L19)])(L19);
    ret_val3 = STR_ap2004550586(plus_self1, plus_sarg);
    noname1->arg1 = ((dSTR) ret_val3);
    L17 = STR_se405450305(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    s = FSTR_p1752847026(s, L17);
    L201_=INTPLUS(L31,1); 
    L31 = L201_;
   }
  }
  after_loop: ;
  plus_self2 = s;
  plus_c1 = ')';
  ret_val4 = FSTR_p399773021(plus_self2, plus_c1);
  s = ret_val4;
 }
 if (ATTR(self,unknown_ret)) {
  s = FSTR_p1752847026(s, ((STR) &name150));
 }
 else {
  if (ATTR(self,has_ret)) {
   s = FSTR_p1752847026(s, ((STR) &S_1));
  }
 }
 str_self = s;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_T152540990(CALL_TP_ARRAY self) {
 STR ret_val;
 extern STR arrayexpression1;
 ret_val = ((STR) &arrayexpression1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_T1577548690(CALL_TP_CREATE self) {
 STR ret_val;
 extern STR createexpression1;
 ret_val = ((STR) &createexpression1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_T1666519150(CALL_T561792367 self) {
 STR ret_val;
 extern STR boundc12314396721;
 ret_val = ((STR) &boundc12314396721);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_T184446387(CALL_TP_VOID self) {
 STR ret_val;
 extern STR voidexpression;
 ret_val = ((STR) &voidexpression);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CALL_T47412966(CALL_T666389079 self) {
 STR ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 extern STR unders1197384132;
 extern STR unders11973841321;
 dTP L1;
 if ((ATTR(self,tp)==((dTP) NULL))) {
  ret_val = ((STR) &unders1197384132);
  return ret_val;
 }
 else {
  plus_self = ((STR) &unders11973841321);
  L1 = ATTR(self,tp);
  plus_sarg = (*dTP_strrSTR[TAG(L1)])(L1);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val = ret_val1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ROU885465471(AM_ROU1916046290 self) {
 dTP ret_val;
 ret_val = ATTR(ATTR(self,fun),ret);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1080486727(AS_OUT self, AS_FEAT_MOD t) {
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
 OUT create_self5;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name157;
 FILE1 L7;
 OB L8;
 extern STR private3;
 FILE1 L10;
 OB L11;
 extern STR readonly2;
 FILE1 L13;
 OB L14;
 BOOL L16;
 BOOL L171_;
 FILE1 L18;
 OB L19;
 BOOL L21;
 BOOL L221_;
 extern STR name158;
 FILE1 L23;
 OB L24;
 if ((t==((AS_FEAT_MOD) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
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
 plus_s2 = ((STR) &name157);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 if (ATTR(t,is_private)) {
  create_self2 = ((OUT) NULL);
  ret_val6 = create_self2;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &private3);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 if (ATTR(t,is_readonly)) {
  create_self3 = ((OUT) NULL);
  ret_val8 = create_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &readonly2);
  stdout_self4 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r4 = L13;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
 }
 L16 = (ATTR(t,new_name).str==(STR)0);
 L171_=!(L16); 
 if (L171_) {
  create_self4 = ((OUT) NULL);
  ret_val10 = create_self4;
  plus_self5 = ret_val10;
  plus_s5 = AS_OUT422425760(self, ATTR(t,new_name).str);
  stdout_self5 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r5 = L18;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 L21 = (ATTR(t,next)==((AS_FEAT_MOD) NULL));
 L221_=!(L21); 
 if (L221_) {
  create_self5 = ((OUT) NULL);
  ret_val12 = create_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name158);
  stdout_self6 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r6 = L23;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AS_OUT1080486727(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1171738231(AS_OUT self, dAS_NODE t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 extern STR dAS_NODEvoid;
 FILE1 L1;
 OB L2;
 if ((t==((dAS_NODE) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &dAS_NODEvoid);
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
 switch (TAG(t)) {
  case AS_CLASS_DEF_tag:
   AS_OUT1208804252(self, ((AS_CLASS_DEF) t)); break;
  case AS_CONST_DEF_tag:
   AS_OUT774387720(self, ((AS_CONST_DEF) t)); break;
  case AS_SHARED_DEF_tag:
   AS_OUT460718931(self, ((AS_SHARED_DEF) t)); break;
  case AS_ATTR_DEF_tag:
   AS_OUT964805355(self, ((AS_ATTR_DEF) t)); break;
  case AS_ROUT_DEF_tag:
   AS_OUT620392985(self, ((AS_ROUT_DEF) t)); break;
  case AS_INC1416917001_tag:
   AS_OUT419203785(self, ((AS_INC1416917001) t)); break;
  case AS_STMT_LIST_tag:
   AS_OUT749907512(self, ((AS_STMT_LIST) t)); break;
  case AS_DEC_STMT_tag:
   AS_OUT2042091545(self, ((AS_DEC_STMT) t)); break;
  case AS_ASSIGN_STMT_tag:
   AS_OUT48907654(self, ((AS_ASSIGN_STMT) t)); break;
  case AS_IF_STMT_tag:
   AS_OUT1240003398(self, ((AS_IF_STMT) t)); break;
  case AS_LOOP_STMT_tag:
   AS_OUT1331819376(self, ((AS_LOOP_STMT) t)); break;
  case AS_RETURN_STMT_tag:
   AS_OUT1189782454(self, ((AS_RETURN_STMT) t)); break;
  case AS_YIELD_STMT_tag:
   AS_OUT962799085(self, ((AS_YIELD_STMT) t)); break;
  case AS_QUIT_STMT_tag:
   AS_OUT1317811396(self, ((AS_QUIT_STMT) t)); break;
  case AS_CASE_STMT_tag:
   AS_OUT474809072(self, ((AS_CASE_STMT) t)); break;
  case AS_TYPECASE_STMT_tag:
   AS_OUT1372724668(self, ((AS_TYPECASE_STMT) t)); break;
  case AS_ASSERT_STMT_tag:
   AS_OUT918185922(self, ((AS_ASSERT_STMT) t)); break;
  case AS_PROTECT_STMT_tag:
   AS_OUT778273489(self, ((AS_PROTECT_STMT) t)); break;
  case AS_RAISE_STMT_tag:
   AS_OUT1538624491(self, ((AS_RAISE_STMT) t)); break;
  case AS_EXPR_STMT_tag:
   AS_OUT995003644(self, ((AS_EXPR_STMT) t)); break;
  case AS_SELF_EXPR_tag:
   AS_OUT804297708(self, ((AS_SELF_EXPR) t)); break;
  case AS_CALL_EXPR_tag:
   AS_OUT1983800340(self, ((AS_CALL_EXPR) t)); break;
  case AS_VOID_EXPR_tag:
   AS_OUT1019461868(self, ((AS_VOID_EXPR) t)); break;
  case AS_IS_VOID_EXPR_tag:
   AS_OUT767507057(self, ((AS_IS_VOID_EXPR) t)); break;
  case AS_ARRAY_EXPR_tag:
   AS_OUT516254387(self, ((AS_ARRAY_EXPR) t)); break;
  case AS_CREATE_EXPR_tag:
   AS_OUT1211286518(self, ((AS_CREATE_EXPR) t)); break;
  case AS_BOU14269336_tag:
   AS_OUT898752312(self, ((AS_BOU14269336) t)); break;
  case AS_UND152986614_tag:
   AS_OUT2138884099(self, ((AS_UND152986614) t)); break;
  case AS_AND_EXPR_tag:
   AS_OUT359892079(self, ((AS_AND_EXPR) t)); break;
  case AS_OR_EXPR_tag:
   AS_OUT1434191198(self, ((AS_OR_EXPR) t)); break;
  case AS_EXCEPT_EXPR_tag:
   AS_OUT662922118(self, ((AS_EXCEPT_EXPR) t)); break;
  case AS_NEW_EXPR_tag:
   AS_OUT599192453(self, ((AS_NEW_EXPR) t)); break;
  case AS_INITIAL_EXPR_tag:
   AS_OUT1347508463(self, ((AS_INITIAL_EXPR) t)); break;
  case AS_BREAK_EXPR_tag:
   AS_OUT26552353(self, ((AS_BREAK_EXPR) t)); break;
  case AS_RESULT_EXPR_tag:
   AS_OUT1070397350(self, ((AS_RESULT_EXPR) t)); break;
  case AS_BOOL_LIT_EXPR_tag:
   AS_OUT1255290752(self, ((AS_BOOL_LIT_EXPR) t)); break;
  case AS_CHAR_LIT_EXPR_tag:
   AS_OUT57117984(self, ((AS_CHAR_LIT_EXPR) t)); break;
  case AS_STR_LIT_EXPR_tag:
   AS_OUT387909983(self, ((AS_STR_LIT_EXPR) t)); break;
  case AS_INT_LIT_EXPR_tag:
   AS_OUT160973005(self, ((AS_INT_LIT_EXPR) t)); break;
  case AS_FLT_LIT_EXPR_tag:
   AS_OUT454374187(self, ((AS_FLT_LIT_EXPR) t)); break;
  case AS_PARAM_DEC_tag:
   AS_OUT966193580(self, ((AS_PARAM_DEC) t)); break;
  case AS_TYPE_SPEC_tag:
   AS_OUT600758956(self, ((AS_TYPE_SPEC) t)); break;
  case AS_ARG_DEC_tag:
   AS_OUT1198591538(self, ((AS_ARG_DEC) t)); break;
  case AS_FEAT_MOD_tag:
   AS_OUT1080486727(self, ((AS_FEAT_MOD) t)); break;
  case AS_CASE_WHEN_tag:
   AS_OUT422363112(self, ((AS_CASE_WHEN) t)); break;
  case AS_TYPECASE_WHEN_tag:
   AS_OUT423700572(self, ((AS_TYPECASE_WHEN) t)); break;
  case AS_PROTECT_WHEN_tag:
   AS_OUT1577197339(self, ((AS_PROTECT_WHEN) t)); break;
  case AS_LOCK_IF_WHEN_tag:
   AS_OUT916258175(self, ((AS_LOCK_IF_WHEN) t)); break;
  case AS_PAR_STMT_tag:
   AS_OUT1499224135(self, ((AS_PAR_STMT) t)); break;
  case AS_INT2031239268_tag:
   AS_OUT1804680247(self, ((AS_INT2031239268) t)); break;
  case AS_LOCK_STMT_tag:
   AS_OUT849427428(self, ((AS_LOCK_STMT) t)); break;
  case AS_UNLOCK_STMT_tag:
   AS_OUT1737761882(self, ((AS_UNLOCK_STMT) t)); break;
  case AS_WIT1063437351_tag:
   AS_OUT229230569(self, ((AS_WIT1063437351) t)); break;
  case AS_ATTACH_STMT_tag:
   AS_OUT148919342(self, ((AS_ATTACH_STMT) t)); break;
  case AS_FORK_STMT_tag:
   AS_OUT1495750296(self, ((AS_FORK_STMT) t)); break;
  case AS_SYNC_STMT_tag:
   AS_OUT5759708(self, ((AS_SYNC_STMT) t)); break;
  default: ;
   FATAL("No applicable type in typecase\nin AS_OUT::AS_NODE_out($AS_NODE)\n./Abstract_Syntax/as_out.sa:39:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1208804252(AS_OUT self, AS_CLASS_DEF t) {
 INT L11 = INT_zero;
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self7;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val15;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT plus_self8;
 dSTR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self8;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self9;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 OUT create_self10;
 OUT ret_val22;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val23;
 FILE1 r11;
 OUT create_self11;
 OUT ret_val24;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self12;
 OUT ret_val26;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val27;
 FILE1 r13;
 AS_OUT iinc_self;
 OUT create_self13;
 OUT ret_val28;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val29;
 FILE1 stdout_self14;
 FILE1 ret_val30;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val31;
 FILE1 stdout_self15;
 FILE1 ret_val32;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val33;
 FILE1 r16;
 extern STR AS_CLASS_DEFvoid;
 FILE1 L2;
 OB L3;
 extern STR immutableclass1;
 FILE1 L5;
 OB L6;
 extern STR class3;
 FILE1 L8;
 OB L9;
 extern STR type3;
 FILE1 L12;
 OB L13;
 extern STR externalCclass;
 FILE1 L15;
 OB L16;
 extern STR extern2035574927;
 FILE1 L18;
 OB L19;
 extern STR spreadclass;
 FILE1 L21;
 OB L22;
 FILE1 L24;
 OB L25;
 dSTR L27;
 OB L28;
 FILE1 L29;
 OB L30;
 BOOL L32;
 BOOL L331_;
 extern STR name2;
 FILE1 L34;
 OB L35;
 extern STR name4;
 FILE1 L37;
 OB L38;
 BOOL L40;
 BOOL L421_;
 extern STR name151;
 FILE1 L43;
 OB L44;
 BOOL L46;
 BOOL L471_;
 extern STR name159;
 FILE1 L48;
 OB L49;
 extern STR is3;
 FILE1 L51;
 OB L52;
 INT L54;
 INT L551_;
 extern STR endof1;
 FILE1 L56;
 OB L57;
 FILE1 L59;
 OB L60;
 extern STR name9;
 FILE1 L62;
 OB L63;
 AS_OUT_indent = 0;
 if ((t==((AS_CLASS_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_CLASS_DEFvoid);
  stdout_self = ((FILE1) NULL);
  L3=ZALLOC(sizeof(struct FILE1_struct));
  if (L3==NULL) FATAL("Unable to allocate more memory");
  ((OB)L3)->header.tag=FILE1_tag;
  L2 = ((FILE1) L3);
  r = L2;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 L11 = ATTR(t,kind);
 switch (L11) {
  case 1: 
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self1 = ret_val2;
   plus_s1 = ((STR) &immutableclass1);
   stdout_self1 = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r1 = L5;
   
   SATTR(r1,fp,stdout);
   ret_val3 = r1;
   FILE_plus_STR(ret_val3, plus_s1);
   break;
  case 2: 
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   plus_s2 = ((STR) &class3);
   stdout_self2 = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r2 = L8;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
   break;
  case 3: 
   create_self3 = ((OUT) NULL);
   ret_val6 = create_self3;
   plus_self3 = ret_val6;
   plus_s3 = ((STR) &type3);
   stdout_self3 = ((FILE1) NULL);
   L13=ZALLOC(sizeof(struct FILE1_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=FILE1_tag;
   L12 = ((FILE1) L13);
   r3 = L12;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   FILE_plus_STR(ret_val7, plus_s3);
   break;
  case 5: 
   create_self4 = ((OUT) NULL);
   ret_val8 = create_self4;
   plus_self4 = ret_val8;
   plus_s4 = ((STR) &externalCclass);
   stdout_self4 = ((FILE1) NULL);
   L16=ZALLOC(sizeof(struct FILE1_struct));
   if (L16==NULL) FATAL("Unable to allocate more memory");
   ((OB)L16)->header.tag=FILE1_tag;
   L15 = ((FILE1) L16);
   r4 = L15;
   
   SATTR(r4,fp,stdout);
   ret_val9 = r4;
   FILE_plus_STR(ret_val9, plus_s4);
   break;
  case 6: 
   create_self5 = ((OUT) NULL);
   ret_val10 = create_self5;
   plus_self5 = ret_val10;
   plus_s5 = ((STR) &extern2035574927);
   stdout_self5 = ((FILE1) NULL);
   L19=ZALLOC(sizeof(struct FILE1_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=FILE1_tag;
   L18 = ((FILE1) L19);
   r5 = L18;
   
   SATTR(r5,fp,stdout);
   ret_val11 = r5;
   FILE_plus_STR(ret_val11, plus_s5);
   break;
  case 7: 
   create_self6 = ((OUT) NULL);
   ret_val12 = create_self6;
   plus_self6 = ret_val12;
   plus_s6 = ((STR) &spreadclass);
   stdout_self6 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L21 = ((FILE1) L22);
   r6 = L21;
   
   SATTR(r6,fp,stdout);
   ret_val13 = r6;
   FILE_plus_STR(ret_val13, plus_s6);
   break;
  default: ;
   FATAL("No applicable target in case statement\nin AS_OUT::AS_CLASS_DEF_out(AS_CLASS_DEF)\n./Abstract_Syntax/as_out.sa:241:12");
 }
 create_self7 = ((OUT) NULL);
 ret_val14 = create_self7;
 plus_self7 = ret_val14;
 plus_s7 = AS_OUT422425760(self, STR_upperrSTR(ATTR(t,name1).str));
 stdout_self7 = ((FILE1) NULL);
 L25=ZALLOC(sizeof(struct FILE1_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=FILE1_tag;
 L24 = ((FILE1) L25);
 r7 = L24;
 
 SATTR(r7,fp,stdout);
 ret_val16 = r7;
 FILE_plus_STR(ret_val16, plus_s7);
 ret_val15 = plus_self7;
 plus_self8 = ret_val15;
 L28=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 memset(L28,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L28)->header.tag=CHAR_tag;
 L27 = (dSTR)((CHAR_boxed) L28);
 ((CHAR_boxed) L27)->immutable_part = ' ';
 plus_s8 = L27;
 stdout_self8 = ((FILE1) NULL);
 L30=ZALLOC(sizeof(struct FILE1_struct));
 if (L30==NULL) FATAL("Unable to allocate more memory");
 ((OB)L30)->header.tag=FILE1_tag;
 L29 = ((FILE1) L30);
 r8 = L29;
 
 SATTR(r8,fp,stdout);
 ret_val17 = r8;
 FILE_plus_dSTR(ret_val17, plus_s8);
 L32 = (ATTR(t,params)==((AS_PARAM_DEC) NULL));
 L331_=!(L32); 
 if (L331_) {
  create_self8 = ((OUT) NULL);
  ret_val18 = create_self8;
  plus_self9 = ret_val18;
  plus_s9 = ((STR) &name2);
  stdout_self9 = ((FILE1) NULL);
  L35=ZALLOC(sizeof(struct FILE1_struct));
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=FILE1_tag;
  L34 = ((FILE1) L35);
  r9 = L34;
  
  SATTR(r9,fp,stdout);
  ret_val19 = r9;
  FILE_plus_STR(ret_val19, plus_s9);
  AS_OUT966193580(self, ATTR(t,params));
  create_self9 = ((OUT) NULL);
  ret_val20 = create_self9;
  plus_self10 = ret_val20;
  plus_s10 = ((STR) &name4);
  stdout_self10 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r10 = L37;
  
  SATTR(r10,fp,stdout);
  ret_val21 = r10;
  FILE_plus_STR(ret_val21, plus_s10);
 }
 L40 = (ATTR(t,under)==((AS_TYPE_SPEC) NULL));
 L421_=!(L40); 
 if (L421_) {
  create_self10 = ((OUT) NULL);
  ret_val22 = create_self10;
  plus_self11 = ret_val22;
  plus_s11 = ((STR) &name151);
  stdout_self11 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r11 = L43;
  
  SATTR(r11,fp,stdout);
  ret_val23 = r11;
  FILE_plus_STR(ret_val23, plus_s11);
  AS_OUT600758956(self, ATTR(t,under));
 }
 L46 = (ATTR(t,over)==((AS_TYPE_SPEC) NULL));
 L471_=!(L46); 
 if (L471_) {
  create_self11 = ((OUT) NULL);
  ret_val24 = create_self11;
  plus_self12 = ret_val24;
  plus_s12 = ((STR) &name159);
  stdout_self12 = ((FILE1) NULL);
  L49=ZALLOC(sizeof(struct FILE1_struct));
  if (L49==NULL) FATAL("Unable to allocate more memory");
  ((OB)L49)->header.tag=FILE1_tag;
  L48 = ((FILE1) L49);
  r12 = L48;
  
  SATTR(r12,fp,stdout);
  ret_val25 = r12;
  FILE_plus_STR(ret_val25, plus_s12);
  AS_OUT600758956(self, ATTR(t,over));
 }
 create_self12 = ((OUT) NULL);
 ret_val26 = create_self12;
 plus_self13 = ret_val26;
 plus_s13 = ((STR) &is3);
 stdout_self13 = ((FILE1) NULL);
 L52=ZALLOC(sizeof(struct FILE1_struct));
 if (L52==NULL) FATAL("Unable to allocate more memory");
 ((OB)L52)->header.tag=FILE1_tag;
 L51 = ((FILE1) L52);
 r13 = L51;
 
 SATTR(r13,fp,stdout);
 ret_val27 = r13;
 FILE_plus_STR(ret_val27, plus_s13);
 iinc_self = self;
 L54 = AS_OUT_indent;
 L551_=INTPLUS(L54,3); 
 AS_OUT_indent = L551_;
 AS_OUT2139428914(self, ATTR(t,body));
 create_self13 = ((OUT) NULL);
 ret_val28 = create_self13;
 plus_self14 = ret_val28;
 plus_s14 = ((STR) &endof1);
 stdout_self14 = ((FILE1) NULL);
 L57=ZALLOC(sizeof(struct FILE1_struct));
 if (L57==NULL) FATAL("Unable to allocate more memory");
 ((OB)L57)->header.tag=FILE1_tag;
 L56 = ((FILE1) L57);
 r14 = L56;
 
 SATTR(r14,fp,stdout);
 ret_val30 = r14;
 FILE_plus_STR(ret_val30, plus_s14);
 ret_val29 = plus_self14;
 plus_self15 = ret_val29;
 plus_s15 = AS_OUT422425760(self, STR_upperrSTR(ATTR(t,name1).str));
 stdout_self15 = ((FILE1) NULL);
 L60=ZALLOC(sizeof(struct FILE1_struct));
 if (L60==NULL) FATAL("Unable to allocate more memory");
 ((OB)L60)->header.tag=FILE1_tag;
 L59 = ((FILE1) L60);
 r15 = L59;
 
 SATTR(r15,fp,stdout);
 ret_val32 = r15;
 FILE_plus_STR(ret_val32, plus_s15);
 ret_val31 = plus_self15;
 plus_self16 = ret_val31;
 plus_s16 = ((STR) &name9);
 stdout_self16 = ((FILE1) NULL);
 L63=ZALLOC(sizeof(struct FILE1_struct));
 if (L63==NULL) FATAL("Unable to allocate more memory");
 ((OB)L63)->header.tag=FILE1_tag;
 L62 = ((FILE1) L63);
 r16 = L62;
 
 SATTR(r16,fp,stdout);
 ret_val33 = r16;
 FILE_plus_STR(ret_val33, plus_s16);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1365061583(AS_OUT self, dAS_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 dAS_STMT L1;
 extern STR voidsurr_ptr;
 FILE1 L2;
 OB L3;
 dAS_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAS_STMT L8;
 if ((t==((dAS_STMT) NULL))) {
  return;
 }
 L1 = t;
 if (((*dAS_ST445865822[TAG(L1)])(L1)==((AS_STMT_LIST) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidsurr_ptr);
  stdout_self = ((FILE1) NULL);
  L3=ZALLOC(sizeof(struct FILE1_struct));
  if (L3==NULL) FATAL("Unable to allocate more memory");
  ((OB)L3)->header.tag=FILE1_tag;
  L2 = ((FILE1) L3);
  r = L2;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
 }
 switch (TAG(t)) {
  case AS_DEC_STMT_tag:
   AS_OUT2042091545(self, ((AS_DEC_STMT) t)); break;
  case AS_ASSIGN_STMT_tag:
   AS_OUT48907654(self, ((AS_ASSIGN_STMT) t)); break;
  case AS_IF_STMT_tag:
   AS_OUT1240003398(self, ((AS_IF_STMT) t)); break;
  case AS_LOOP_STMT_tag:
   AS_OUT1331819376(self, ((AS_LOOP_STMT) t)); break;
  case AS_RETURN_STMT_tag:
   AS_OUT1189782454(self, ((AS_RETURN_STMT) t)); break;
  case AS_YIELD_STMT_tag:
   AS_OUT962799085(self, ((AS_YIELD_STMT) t)); break;
  case AS_QUIT_STMT_tag:
   AS_OUT1317811396(self, ((AS_QUIT_STMT) t)); break;
  case AS_CASE_STMT_tag:
   AS_OUT474809072(self, ((AS_CASE_STMT) t)); break;
  case AS_TYPECASE_STMT_tag:
   AS_OUT1372724668(self, ((AS_TYPECASE_STMT) t)); break;
  case AS_ASSERT_STMT_tag:
   AS_OUT918185922(self, ((AS_ASSERT_STMT) t)); break;
  case AS_PROTECT_STMT_tag:
   AS_OUT778273489(self, ((AS_PROTECT_STMT) t)); break;
  case AS_RAISE_STMT_tag:
   AS_OUT1538624491(self, ((AS_RAISE_STMT) t)); break;
  case AS_EXPR_STMT_tag:
   AS_OUT995003644(self, ((AS_EXPR_STMT) t)); break;
  case AS_PAR_STMT_tag:
   AS_OUT1499224135(self, ((AS_PAR_STMT) t)); break;
  case AS_INT2031239268_tag:
   AS_OUT1804680247(self, ((AS_INT2031239268) t)); break;
  case AS_LOCK_STMT_tag:
   AS_OUT849427428(self, ((AS_LOCK_STMT) t)); break;
  case AS_UNLOCK_STMT_tag:
   AS_OUT1737761882(self, ((AS_UNLOCK_STMT) t)); break;
  case AS_WIT1063437351_tag:
   AS_OUT229230569(self, ((AS_WIT1063437351) t)); break;
  case AS_ATTACH_STMT_tag:
   AS_OUT148919342(self, ((AS_ATTACH_STMT) t)); break;
  case AS_FORK_STMT_tag:
   AS_OUT1495750296(self, ((AS_FORK_STMT) t)); break;
  case AS_SYNC_STMT_tag:
   AS_OUT5759708(self, ((AS_SYNC_STMT) t)); break;
  default: ;
   FATAL("No applicable type in typecase\nin AS_OUT::AS_STMT_out($AS_STMT)\n./Abstract_Syntax/as_out.sa:116:14");
 }
 L5 = t;
 L6 = ((*dAS_ST460279343[TAG(L5)])(L5)==((dAS_STMT) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = t;
  AS_OUT1365061583(self, (*dAS_ST460279343[TAG(L8)])(L8));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT148919342(AS_OUT self, AS_ATTACH_STMT t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR attach1;
 FILE1 L4;
 OB L5;
 extern STR name156;
 FILE1 L7;
 OB L8;
 extern STR name13;
 FILE1 L10;
 OB L11;
 if ((t==((AS_ATTACH_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 if (ATTR(t,dont_print)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &attach1);
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
 else {
  AS_OUT851761245(self, ATTR(t,lhs));
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name156);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT851761245(self, ATTR(t,rhs));
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name13);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1495750296(AS_OUT self, AS_FORK_STMT t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR fork3;
 FILE1 L4;
 OB L5;
 extern STR name9;
 FILE1 L7;
 OB L8;
 BOOL L10;
 BOOL L111_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 extern STR endoffork;
 FILE1 L16;
 OB L17;
 if ((t==((AS_FORK_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &fork3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,at));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L10 = (ATTR(t,body)==((AS_STMT_LIST) NULL));
 L111_=!(L10); 
 if (L111_) {
  iinc_self = self;
  L12 = AS_OUT_indent;
  L131_=INTPLUS(L12,3); 
  AS_OUT_indent = L131_;
  AS_OUT749907512(self, ATTR(t,body));
  idec_self = self;
  L14 = AS_OUT_indent;
  L151_=INTMINUS(L14,3); 
  AS_OUT_indent = L151_;
  AS_OUT_ind(self);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &endoffork);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1737761882(AS_OUT self, AS_UNLOCK_STMT t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR unlock2;
 FILE1 L4;
 OB L5;
 extern STR name13;
 FILE1 L7;
 OB L8;
 if ((t==((AS_UNLOCK_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &unlock2);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT2139428914(AS_OUT self, dAS_CLASS_ELT t) {
 if ((t==((dAS_CLASS_ELT) NULL))) {
  return;
 }
 switch (TAG(t)) {
  case AS_CONST_DEF_tag:
   AS_OUT774387720(self, ((AS_CONST_DEF) t)); break;
  case AS_SHARED_DEF_tag:
   AS_OUT460718931(self, ((AS_SHARED_DEF) t)); break;
  case AS_ATTR_DEF_tag:
   AS_OUT964805355(self, ((AS_ATTR_DEF) t)); break;
  case AS_ROUT_DEF_tag:
   AS_OUT620392985(self, ((AS_ROUT_DEF) t)); break;
  case AS_INC1416917001_tag:
   AS_OUT419203785(self, ((AS_INC1416917001) t)); break;
  default: ;
   FATAL("No applicable type in typecase\nin AS_OUT::AS_CLASS_ELT_out($AS_CLASS_ELT)\n./Abstract_Syntax/as_out.sa:100:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT229230569(AS_OUT self, AS_WIT1063437351 t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 AS_OUT iinc_self1;
 AS_OUT idec_self1;
 OUT create_self6;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR with3;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name29;
 FILE1 L9;
 OB L10;
 extern STR self1;
 FILE1 L12;
 OB L13;
 extern STR near5;
 FILE1 L15;
 OB L16;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 BOOL L22;
 BOOL L231_;
 extern STR else8;
 FILE1 L24;
 OB L25;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 extern STR endofwithnear;
 FILE1 L32;
 OB L33;
 if ((t==((AS_WIT1063437351) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &with3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT842936047(self, ATTR(t,idents));
 if (ATTR(t,self_occurred)) {
  L7 = (ATTR(t,idents)==((AS_IDENT_LIST) NULL));
  L81_=!(L7); 
  if (L81_) {
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   plus_s2 = ((STR) &name29);
   stdout_self2 = ((FILE1) NULL);
   L10=ZALLOC(sizeof(struct FILE1_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=FILE1_tag;
   L9 = ((FILE1) L10);
   r2 = L9;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
  }
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &self1);
  stdout_self3 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r3 = L12;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &near5);
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 iinc_self = self;
 L18 = AS_OUT_indent;
 L191_=INTPLUS(L18,3); 
 AS_OUT_indent = L191_;
 AS_OUT749907512(self, ATTR(t,near_part));
 idec_self = self;
 L20 = AS_OUT_indent;
 L211_=INTMINUS(L20,3); 
 AS_OUT_indent = L211_;
 L22 = (ATTR(t,else_part)==((AS_STMT_LIST) NULL));
 L231_=!(L22); 
 if (L231_) {
  AS_OUT_ind(self);
  create_self5 = ((OUT) NULL);
  ret_val10 = create_self5;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &else8);
  stdout_self5 = ((FILE1) NULL);
  L25=ZALLOC(sizeof(struct FILE1_struct));
  if (L25==NULL) FATAL("Unable to allocate more memory");
  ((OB)L25)->header.tag=FILE1_tag;
  L24 = ((FILE1) L25);
  r5 = L24;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  iinc_self1 = self;
  L27 = AS_OUT_indent;
  L281_=INTPLUS(L27,3); 
  AS_OUT_indent = L281_;
  AS_OUT749907512(self, ATTR(t,else_part));
  idec_self1 = self;
  L29 = AS_OUT_indent;
  L301_=INTMINUS(L29,3); 
  AS_OUT_indent = L301_;
 }
 AS_OUT_ind(self);
 create_self6 = ((OUT) NULL);
 ret_val12 = create_self6;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &endofwithnear);
 stdout_self6 = ((FILE1) NULL);
 L33=ZALLOC(sizeof(struct FILE1_struct));
 if (L33==NULL) FATAL("Unable to allocate more memory");
 ((OB)L33)->header.tag=FILE1_tag;
 L32 = ((FILE1) L33);
 r6 = L32;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT419203785(AS_OUT self, AS_INC1416917001 t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 AS_OUT iinc_self;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 AS_OUT idec_self;
 extern STR AS_INC530055583;
 FILE1 L1;
 OB L2;
 extern STR private3;
 FILE1 L4;
 OB L5;
 extern STR include4;
 FILE1 L7;
 OB L8;
 BOOL L10;
 BOOL L111_;
 extern STR name9;
 FILE1 L12;
 OB L13;
 INT L15;
 INT L161_;
 extern STR name13;
 FILE1 L17;
 OB L18;
 BOOL L20;
 BOOL L211_;
 INT L22;
 INT L231_;
 BOOL L24;
 BOOL L251_;
 if ((t==((AS_INC1416917001) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_INC530055583);
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
 AS_OUT_ind(self);
 if (ATTR(t,is_private)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &private3);
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
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &include4);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 AS_OUT600758956(self, ATTR(t,tp));
 L10 = (ATTR(t,mods)==((AS_FEAT_MOD) NULL));
 L111_=!(L10); 
 if (L111_) {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name9);
  stdout_self3 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r3 = L12;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  iinc_self = self;
  L15 = AS_OUT_indent;
  L161_=INTPLUS(L15,3); 
  AS_OUT_indent = L161_;
  AS_OUT1080486727(self, ATTR(t,mods));
 }
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name13);
 stdout_self4 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r4 = L17;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L20 = (ATTR(t,mods)==((AS_FEAT_MOD) NULL));
 L211_=!(L20); 
 if (L211_) {
  idec_self = self;
  L22 = AS_OUT_indent;
  L231_=INTMINUS(L22,3); 
  AS_OUT_indent = L231_;
 }
 L24 = (ATTR(t,next)==((dAS_CLASS_ELT) NULL));
 L251_=!(L24); 
 if (L251_) {
  AS_OUT2139428914(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT5759708(AS_OUT self, AS_SYNC_STMT t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR sync3;
 FILE1 L4;
 OB L5;
 if ((t==((AS_SYNC_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &sync3);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT620392985(AS_OUT self, AS_ROUT_DEF t) {
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
 dSTR plus_s3;
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
 OUT create_self5;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self6;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self7;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self8;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self9;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 OUT create_self10;
 OUT ret_val22;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val23;
 FILE1 r11;
 OUT create_self11;
 OUT ret_val24;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self12;
 OUT ret_val26;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val27;
 FILE1 stdout_self13;
 FILE1 ret_val28;
 FILE1 r13;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val29;
 FILE1 stdout_self14;
 FILE1 ret_val30;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val31;
 FILE1 r15;
 OUT create_self13;
 OUT ret_val32;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val33;
 FILE1 r16;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self14;
 OUT ret_val34;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val35;
 FILE1 stdout_self17;
 FILE1 ret_val36;
 FILE1 r17;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val37;
 FILE1 stdout_self18;
 FILE1 ret_val38;
 FILE1 r18;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val39;
 FILE1 r19;
 extern STR AS_ROUT_DEFvoid;
 FILE1 L1;
 OB L2;
 extern STR private3;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 dSTR L10;
 OB L11;
 FILE1 L12;
 OB L13;
 BOOL L15;
 BOOL L161_;
 extern STR name24;
 FILE1 L17;
 OB L18;
 extern STR name26;
 FILE1 L20;
 OB L21;
 BOOL L23;
 BOOL L241_;
 extern STR name20;
 FILE1 L25;
 OB L26;
 BOOL L28;
 BOOL L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 extern STR name9;
 FILE1 L34;
 OB L35;
 BOOL L37;
 BOOL L381_;
 extern STR pre2;
 FILE1 L39;
 OB L40;
 extern STR name9;
 FILE1 L42;
 OB L43;
 BOOL L45;
 BOOL L461_;
 extern STR post2;
 FILE1 L47;
 OB L48;
 extern STR name9;
 FILE1 L50;
 OB L51;
 extern STR name13;
 FILE1 L53;
 OB L54;
 extern STR builtin2;
 FILE1 L56;
 OB L57;
 FILE1 L59;
 OB L60;
 extern STR name13;
 FILE1 L63;
 OB L64;
 extern STR is;
 FILE1 L66;
 OB L67;
 INT L69;
 INT L701_;
 INT L71;
 INT L721_;
 extern STR endof;
 FILE1 L73;
 OB L74;
 FILE1 L76;
 OB L77;
 extern STR name9;
 FILE1 L79;
 OB L80;
 BOOL L82;
 BOOL L831_;
 if ((t==((AS_ROUT_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_ROUT_DEFvoid);
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
 AS_OUT_ind(self);
 if (ATTR(t,is_private)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &private3);
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
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L11=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 memset(L11,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L11)->header.tag=CHAR_tag;
 L10 = (dSTR)((CHAR_boxed) L11);
 ((CHAR_boxed) L10)->immutable_part = ' ';
 plus_s3 = L10;
 stdout_self3 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r3 = L12;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_dSTR(ret_val7, plus_s3);
 L15 = (ATTR(t,args_dec)==((AS_ARG_DEC) NULL));
 L161_=!(L15); 
 if (L161_) {
  create_self3 = ((OUT) NULL);
  ret_val8 = create_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name24);
  stdout_self4 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r4 = L17;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
  AS_OUT1198591538(self, ATTR(t,args_dec));
  create_self4 = ((OUT) NULL);
  ret_val10 = create_self4;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &name26);
  stdout_self5 = ((FILE1) NULL);
  L21=ZALLOC(sizeof(struct FILE1_struct));
  if (L21==NULL) FATAL("Unable to allocate more memory");
  ((OB)L21)->header.tag=FILE1_tag;
  L20 = ((FILE1) L21);
  r5 = L20;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 L23 = (ATTR(t,ret_dec)==((AS_TYPE_SPEC) NULL));
 L241_=!(L23); 
 if (L241_) {
  create_self5 = ((OUT) NULL);
  ret_val12 = create_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name20);
  stdout_self6 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r6 = L25;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AS_OUT600758956(self, ATTR(t,ret_dec));
  L29 = (ATTR(t,pre_e)==((dAS_EXPR) NULL));
  L301_=!(L29); 
  if (L301_) {
   L28 = TRUE;
  } else {
   L32 = (ATTR(t,post_e)==((dAS_EXPR) NULL));
   L331_=!(L32); 
   L28 = L331_;
  }
  if (L28) {
   create_self6 = ((OUT) NULL);
   ret_val14 = create_self6;
   plus_self7 = ret_val14;
   plus_s7 = ((STR) &name9);
   stdout_self7 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   r7 = L34;
   
   SATTR(r7,fp,stdout);
   ret_val15 = r7;
   FILE_plus_STR(ret_val15, plus_s7);
   AS_OUT_ind(self);
  }
 }
 L37 = (ATTR(t,pre_e)==((dAS_EXPR) NULL));
 L381_=!(L37); 
 if (L381_) {
  create_self7 = ((OUT) NULL);
  ret_val16 = create_self7;
  plus_self8 = ret_val16;
  plus_s8 = ((STR) &pre2);
  stdout_self8 = ((FILE1) NULL);
  L40=ZALLOC(sizeof(struct FILE1_struct));
  if (L40==NULL) FATAL("Unable to allocate more memory");
  ((OB)L40)->header.tag=FILE1_tag;
  L39 = ((FILE1) L40);
  r8 = L39;
  
  SATTR(r8,fp,stdout);
  ret_val17 = r8;
  FILE_plus_STR(ret_val17, plus_s8);
  AS_OUT851761245(self, ATTR(t,pre_e));
  create_self8 = ((OUT) NULL);
  ret_val18 = create_self8;
  plus_self9 = ret_val18;
  plus_s9 = ((STR) &name9);
  stdout_self9 = ((FILE1) NULL);
  L43=ZALLOC(sizeof(struct FILE1_struct));
  if (L43==NULL) FATAL("Unable to allocate more memory");
  ((OB)L43)->header.tag=FILE1_tag;
  L42 = ((FILE1) L43);
  r9 = L42;
  
  SATTR(r9,fp,stdout);
  ret_val19 = r9;
  FILE_plus_STR(ret_val19, plus_s9);
  AS_OUT_ind(self);
 }
 L45 = (ATTR(t,post_e)==((dAS_EXPR) NULL));
 L461_=!(L45); 
 if (L461_) {
  create_self9 = ((OUT) NULL);
  ret_val20 = create_self9;
  plus_self10 = ret_val20;
  plus_s10 = ((STR) &post2);
  stdout_self10 = ((FILE1) NULL);
  L48=ZALLOC(sizeof(struct FILE1_struct));
  if (L48==NULL) FATAL("Unable to allocate more memory");
  ((OB)L48)->header.tag=FILE1_tag;
  L47 = ((FILE1) L48);
  r10 = L47;
  
  SATTR(r10,fp,stdout);
  ret_val21 = r10;
  FILE_plus_STR(ret_val21, plus_s10);
  AS_OUT851761245(self, ATTR(t,post_e));
  create_self10 = ((OUT) NULL);
  ret_val22 = create_self10;
  plus_self11 = ret_val22;
  plus_s11 = ((STR) &name9);
  stdout_self11 = ((FILE1) NULL);
  L51=ZALLOC(sizeof(struct FILE1_struct));
  if (L51==NULL) FATAL("Unable to allocate more memory");
  ((OB)L51)->header.tag=FILE1_tag;
  L50 = ((FILE1) L51);
  r11 = L50;
  
  SATTR(r11,fp,stdout);
  ret_val23 = r11;
  FILE_plus_STR(ret_val23, plus_s11);
  AS_OUT_ind(self);
 }
 if (ATTR(t,is_abstract)) {
  create_self11 = ((OUT) NULL);
  ret_val24 = create_self11;
  plus_self12 = ret_val24;
  plus_s12 = ((STR) &name13);
  stdout_self12 = ((FILE1) NULL);
  L54=ZALLOC(sizeof(struct FILE1_struct));
  if (L54==NULL) FATAL("Unable to allocate more memory");
  ((OB)L54)->header.tag=FILE1_tag;
  L53 = ((FILE1) L54);
  r12 = L53;
  
  SATTR(r12,fp,stdout);
  ret_val25 = r12;
  FILE_plus_STR(ret_val25, plus_s12);
 }
 else {
  if (ATTR(t,is_builtin)) {
   create_self12 = ((OUT) NULL);
   ret_val26 = create_self12;
   plus_self13 = ret_val26;
   plus_s13 = ((STR) &builtin2);
   stdout_self13 = ((FILE1) NULL);
   L57=ZALLOC(sizeof(struct FILE1_struct));
   if (L57==NULL) FATAL("Unable to allocate more memory");
   ((OB)L57)->header.tag=FILE1_tag;
   L56 = ((FILE1) L57);
   r13 = L56;
   
   SATTR(r13,fp,stdout);
   ret_val28 = r13;
   FILE_plus_STR(ret_val28, plus_s13);
   ret_val27 = plus_self13;
   plus_self14 = ret_val27;
   plus_s14 = ATTR(t,builtin_name).str;
   stdout_self14 = ((FILE1) NULL);
   L60=ZALLOC(sizeof(struct FILE1_struct));
   if (L60==NULL) FATAL("Unable to allocate more memory");
   ((OB)L60)->header.tag=FILE1_tag;
   L59 = ((FILE1) L60);
   r14 = L59;
   
   SATTR(r14,fp,stdout);
   ret_val30 = r14;
   FILE_plus_STR(ret_val30, plus_s14);
   ret_val29 = plus_self14;
   plus_self15 = ret_val29;
   plus_s15 = ((STR) &name13);
   stdout_self15 = ((FILE1) NULL);
   L64=ZALLOC(sizeof(struct FILE1_struct));
   if (L64==NULL) FATAL("Unable to allocate more memory");
   ((OB)L64)->header.tag=FILE1_tag;
   L63 = ((FILE1) L64);
   r15 = L63;
   
   SATTR(r15,fp,stdout);
   ret_val31 = r15;
   FILE_plus_STR(ret_val31, plus_s15);
  }
  else {
   create_self13 = ((OUT) NULL);
   ret_val32 = create_self13;
   plus_self16 = ret_val32;
   plus_s16 = ((STR) &is);
   stdout_self16 = ((FILE1) NULL);
   L67=ZALLOC(sizeof(struct FILE1_struct));
   if (L67==NULL) FATAL("Unable to allocate more memory");
   ((OB)L67)->header.tag=FILE1_tag;
   L66 = ((FILE1) L67);
   r16 = L66;
   
   SATTR(r16,fp,stdout);
   ret_val33 = r16;
   FILE_plus_STR(ret_val33, plus_s16);
   iinc_self = self;
   L69 = AS_OUT_indent;
   L701_=INTPLUS(L69,3); 
   AS_OUT_indent = L701_;
   AS_OUT749907512(self, ATTR(t,body));
   idec_self = self;
   L71 = AS_OUT_indent;
   L721_=INTMINUS(L71,3); 
   AS_OUT_indent = L721_;
   AS_OUT_ind(self);
   create_self14 = ((OUT) NULL);
   ret_val34 = create_self14;
   plus_self17 = ret_val34;
   plus_s17 = ((STR) &endof);
   stdout_self17 = ((FILE1) NULL);
   L74=ZALLOC(sizeof(struct FILE1_struct));
   if (L74==NULL) FATAL("Unable to allocate more memory");
   ((OB)L74)->header.tag=FILE1_tag;
   L73 = ((FILE1) L74);
   r17 = L73;
   
   SATTR(r17,fp,stdout);
   ret_val36 = r17;
   FILE_plus_STR(ret_val36, plus_s17);
   ret_val35 = plus_self17;
   plus_self18 = ret_val35;
   plus_s18 = AS_OUT422425760(self, ATTR(t,name1).str);
   stdout_self18 = ((FILE1) NULL);
   L77=ZALLOC(sizeof(struct FILE1_struct));
   if (L77==NULL) FATAL("Unable to allocate more memory");
   ((OB)L77)->header.tag=FILE1_tag;
   L76 = ((FILE1) L77);
   r18 = L76;
   
   SATTR(r18,fp,stdout);
   ret_val38 = r18;
   FILE_plus_STR(ret_val38, plus_s18);
   ret_val37 = plus_self18;
   plus_self19 = ret_val37;
   plus_s19 = ((STR) &name9);
   stdout_self19 = ((FILE1) NULL);
   L80=ZALLOC(sizeof(struct FILE1_struct));
   if (L80==NULL) FATAL("Unable to allocate more memory");
   ((OB)L80)->header.tag=FILE1_tag;
   L79 = ((FILE1) L80);
   r19 = L79;
   
   SATTR(r19,fp,stdout);
   ret_val39 = r19;
   FILE_plus_STR(ret_val39, plus_s19);
  }
 }
 L82 = (ATTR(t,next)==((dAS_CLASS_ELT) NULL));
 L831_=!(L82); 
 if (L831_) {
  AS_OUT2139428914(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT749907512(AS_OUT self, AS_STMT_LIST t) {
 if ((t==((AS_STMT_LIST) NULL))) {
  return;
 }
 AS_OUT1365061583(self, ATTR(t,stmts));
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT842936047(AS_OUT self, AS_IDENT_LIST t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name29;
 FILE1 L9;
 OB L10;
 if ((t==((AS_IDENT_LIST) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,next)==((AS_IDENT_LIST) NULL));
 L81_=!(L7); 
 if (L81_) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name29);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT842936047(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT849427428(AS_OUT self, AS_LOCK_STMT t) {
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
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 AS_OUT iinc_self;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT idec_self;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR lock3;
 FILE1 L4;
 OB L5;
 INT L7;
 INT L81_;
 BOOL L9;
 BOOL L101_;
 extern STR else8;
 FILE1 L11;
 OB L12;
 INT L14;
 INT L151_;
 extern STR endoflock;
 FILE1 L16;
 OB L17;
 if ((t==((AS_LOCK_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
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
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &lock3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 iinc_self = self;
 L7 = AS_OUT_indent;
 L81_=INTPLUS(L7,3); 
 AS_OUT_indent = L81_;
 AS_OUT916258175(self, ATTR(t,if_when_part));
 L9 = ATTR(t,no_else);
 L101_=!(L9); 
 if (L101_) {
  AS_OUT_ind(self);
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &else8);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L11 = ((FILE1) L12);
  r2 = L11;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT749907512(self, ATTR(t,else_part));
 }
 idec_self = self;
 L14 = AS_OUT_indent;
 L151_=INTMINUS(L14,3); 
 AS_OUT_indent = L151_;
 AS_OUT_ind(self);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &endoflock);
 stdout_self3 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r3 = L16;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void BUILD_1202586967(BUILD_TYPE_GRAPH self) {
 dTP tp;
 FSETdTP L11;
 FSETTP_CLASS ig1;
 FSETdTP ig2;
 TP_GRAPH_ABS_DES do_tbl_self;
 dTP L2;
 TP_GRA628060055(ATTR(ATTR(self,prog),tp_graph));
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,prog),tp_done);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L2;
   if (tp==NULL) {
    goto other115;
   } else
   switch (TAG(tp)) {
    case TP_CLASS_tag:
     ig1 = TP_GRA583006971(ATTR(ATTR(self,prog),tp_graph), ((TP_CLASS) tp));
     ig2 = TP_GRA2060761681(ATTR(ATTR(self,prog),tp_graph), ((TP_CLASS) tp)); break;
    default: ;
    other115: ;
   }
  }
 }
 after_loop: ;
 do_tbl_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
 TP_GRA1187239150(do_tbl_self);
 TP_GRA1175730507(do_tbl_self);
 TP_GRA1175712875(do_tbl_self);
 if (ATTR(ATTR(self,prog),show_graphs)) {
  BUILD_413411632(self);
  BUILD_1583735792(self);
  BUILD_1386674981(self);
 }
}


#undef IS_ITER
#define IS_ITER 0

void BUILD_1386674981(BUILD_TYPE_GRAPH self) {
 FMAPTP291739594 L11;
 TUPTP_858321267 p = TUPTP_858321267_zero;
 FSETdTP L21;
 STR L31;
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
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 TUPTP_858321267 L4;
 extern STR Theabstracttype;
 FILE1 L5;
 OB L6;
 FILE1 L8;
 OB L9;
 extern STR hascon610228982;
 FILE1 L12;
 OB L13;
 extern STR name10;
 STR L15;
 dTP L16;
 dTP L17;
 FILE1 L18;
 OB L19;
 extern STR name9;
 FILE1 L22;
 OB L23;
 extern STR name9;
 FILE1 L25;
 OB L26;
 {
  struct FMAPTP1465407463_frame_struct other1_0;
FMAPTP1465407463_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,prog),tp_graph_abs_des),tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L4 = FMAPTP1465407463(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   p = L4;
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &Theabstracttype);
   stdout_self = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r = L5;
   
   SATTR(r,fp,stdout);
   ret_val2 = r;
   FILE_plus_STR(ret_val2, plus_s);
   ret_val1 = plus_self;
   plus_self1 = ret_val1;
   plus_s1 = TP_CLASS_strrSTR(p.t1);
   stdout_self1 = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r1 = L8;
   
   SATTR(r1,fp,stdout);
   ret_val4 = r1;
   FILE_plus_STR(ret_val4, plus_s1);
   ret_val3 = plus_self1;
   plus_self2 = ret_val3;
   plus_s2 = ((STR) &hascon610228982);
   stdout_self2 = ((FILE1) NULL);
   L13=ZALLOC(sizeof(struct FILE1_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=FILE1_tag;
   L12 = ((FILE1) L13);
   r2 = L12;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
   {
    struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname2 = &other2_0;
    struct STR_se405450305_frame_struct other2_1;
STR_se405450305_frame noname3 = &other2_1;
    L21 = p.t2;
    L31 = ((STR) &name10);
    noname2->self = L21;
    noname2->state = 0;
    noname3->self = L31;
    noname3->state = 0;
    while (1) {
     create_self1 = ((OUT) NULL);
     ret_val6 = create_self1;
     plus_self3 = ret_val6;
     L16 = FSETdTP_eltbrdTP(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     L17 = L16;
     noname3->arg1 = ((dSTR) (*dTP_strrSTR[TAG(L17)])(L17));
     L15 = STR_se405450305(noname3);
     if (noname3->state == -1) {
      goto after_loop1;
     }
     plus_s3 = L15;
     stdout_self3 = ((FILE1) NULL);
     L19=ZALLOC(sizeof(struct FILE1_struct));
     if (L19==NULL) FATAL("Unable to allocate more memory");
     ((OB)L19)->header.tag=FILE1_tag;
     L18 = ((FILE1) L19);
     r3 = L18;
     
     SATTR(r3,fp,stdout);
     ret_val7 = r3;
     FILE_plus_STR(ret_val7, plus_s3);
    }
   }
   after_loop1: ;
   create_self2 = ((OUT) NULL);
   ret_val8 = create_self2;
   plus_self4 = ret_val8;
   plus_s4 = ((STR) &name9);
   stdout_self4 = ((FILE1) NULL);
   L23=ZALLOC(sizeof(struct FILE1_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=FILE1_tag;
   L22 = ((FILE1) L23);
   r4 = L22;
   
   SATTR(r4,fp,stdout);
   ret_val9 = r4;
   FILE_plus_STR(ret_val9, plus_s4);
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name9);
 stdout_self5 = ((FILE1) NULL);
 L26=ZALLOC(sizeof(struct FILE1_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 ((OB)L26)->header.tag=FILE1_tag;
 L25 = ((FILE1) L26);
 r5 = L25;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void BUILD_1583735792(BUILD_TYPE_GRAPH self) {
 dTP tp;
 FSETdTP L11;
 FSETdTP at;
 FSETdTP L21;
 STR L31;
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
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 dTP L4;
 BOOL L5;
 BOOL L61_;
 extern STR Descendantsof;
 FILE1 L7;
 OB L8;
 dTP L10;
 FILE1 L12;
 OB L13;
 extern STR name33;
 FILE1 L15;
 OB L16;
 extern STR name10;
 STR L18;
 dTP L19;
 dTP L20;
 FILE1 L22;
 OB L23;
 extern STR name9;
 FILE1 L25;
 OB L26;
 extern STR name9;
 FILE1 L28;
 OB L29;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,prog),tp_done);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L4 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L4;
   at = ((FSETdTP) NULL);
   if (tp==NULL) {
    goto other114;
   } else
   switch (TAG(tp)) {
    case TP_CLASS_tag:
     at = TP_GRA2060761681(ATTR(ATTR(self,prog),tp_graph), ((TP_CLASS) tp)); break;
    default: ;
    other114: ;
   }
   L5 = (at==((FSETdTP) NULL));
   L61_=!(L5); 
   if (L61_) {
    create_self = ((OUT) NULL);
    ret_val = create_self;
    plus_self = ret_val;
    plus_s = ((STR) &Descendantsof);
    stdout_self = ((FILE1) NULL);
    L8=ZALLOC(sizeof(struct FILE1_struct));
    if (L8==NULL) FATAL("Unable to allocate more memory");
    ((OB)L8)->header.tag=FILE1_tag;
    L7 = ((FILE1) L8);
    r = L7;
    
    SATTR(r,fp,stdout);
    ret_val2 = r;
    FILE_plus_STR(ret_val2, plus_s);
    ret_val1 = plus_self;
    plus_self1 = ret_val1;
    L10 = tp;
    plus_s1 = (*dTP_strrSTR[TAG(L10)])(L10);
    stdout_self1 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r1 = L12;
    
    SATTR(r1,fp,stdout);
    ret_val4 = r1;
    FILE_plus_STR(ret_val4, plus_s1);
    ret_val3 = plus_self1;
    plus_self2 = ret_val3;
    plus_s2 = ((STR) &name33);
    stdout_self2 = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r2 = L15;
    
    SATTR(r2,fp,stdout);
    ret_val5 = r2;
    FILE_plus_STR(ret_val5, plus_s2);
    {
     struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname2 = &other2_0;
     struct STR_se405450305_frame_struct other2_1;
STR_se405450305_frame noname3 = &other2_1;
     L21 = at;
     L31 = ((STR) &name10);
     noname2->self = L21;
     noname2->state = 0;
     noname3->self = L31;
     noname3->state = 0;
     while (1) {
      create_self1 = ((OUT) NULL);
      ret_val6 = create_self1;
      plus_self3 = ret_val6;
      L19 = FSETdTP_eltbrdTP(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      L20 = L19;
      noname3->arg1 = ((dSTR) (*dTP_strrSTR[TAG(L20)])(L20));
      L18 = STR_se405450305(noname3);
      if (noname3->state == -1) {
       goto after_loop1;
      }
      plus_s3 = L18;
      stdout_self3 = ((FILE1) NULL);
      L23=ZALLOC(sizeof(struct FILE1_struct));
      if (L23==NULL) FATAL("Unable to allocate more memory");
      ((OB)L23)->header.tag=FILE1_tag;
      L22 = ((FILE1) L23);
      r3 = L22;
      
      SATTR(r3,fp,stdout);
      ret_val7 = r3;
      FILE_plus_STR(ret_val7, plus_s3);
     }
    }
    after_loop1: ;
    create_self2 = ((OUT) NULL);
    ret_val8 = create_self2;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &name9);
    stdout_self4 = ((FILE1) NULL);
    L26=ZALLOC(sizeof(struct FILE1_struct));
    if (L26==NULL) FATAL("Unable to allocate more memory");
    ((OB)L26)->header.tag=FILE1_tag;
    L25 = ((FILE1) L26);
    r4 = L25;
    
    SATTR(r4,fp,stdout);
    ret_val9 = r4;
    FILE_plus_STR(ret_val9, plus_s4);
   }
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name9);
 stdout_self5 = ((FILE1) NULL);
 L29=ZALLOC(sizeof(struct FILE1_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=FILE1_tag;
 L28 = ((FILE1) L29);
 r5 = L28;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void BUILD_413411632(BUILD_TYPE_GRAPH self) {
 dTP tp;
 FSETdTP L11;
 FSETTP_CLASS at;
 FSETTP_CLASS L21;
 STR L31;
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
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 dTP L4;
 BOOL L5;
 BOOL L61_;
 extern STR Ancestorsof;
 FILE1 L7;
 OB L8;
 dTP L10;
 FILE1 L12;
 OB L13;
 extern STR name33;
 FILE1 L15;
 OB L16;
 extern STR name10;
 STR L18;
 TP_CLASS L19;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 extern STR name9;
 FILE1 L27;
 OB L28;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,prog),tp_done);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L4 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L4;
   at = ((FSETTP_CLASS) NULL);
   if (tp==NULL) {
    goto other113;
   } else
   switch (TAG(tp)) {
    case TP_CLASS_tag:
     at = TP_GRA583006971(ATTR(ATTR(self,prog),tp_graph), ((TP_CLASS) tp)); break;
    default: ;
    other113: ;
   }
   L5 = (at==((FSETTP_CLASS) NULL));
   L61_=!(L5); 
   if (L61_) {
    create_self = ((OUT) NULL);
    ret_val = create_self;
    plus_self = ret_val;
    plus_s = ((STR) &Ancestorsof);
    stdout_self = ((FILE1) NULL);
    L8=ZALLOC(sizeof(struct FILE1_struct));
    if (L8==NULL) FATAL("Unable to allocate more memory");
    ((OB)L8)->header.tag=FILE1_tag;
    L7 = ((FILE1) L8);
    r = L7;
    
    SATTR(r,fp,stdout);
    ret_val2 = r;
    FILE_plus_STR(ret_val2, plus_s);
    ret_val1 = plus_self;
    plus_self1 = ret_val1;
    L10 = tp;
    plus_s1 = (*dTP_strrSTR[TAG(L10)])(L10);
    stdout_self1 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r1 = L12;
    
    SATTR(r1,fp,stdout);
    ret_val4 = r1;
    FILE_plus_STR(ret_val4, plus_s1);
    ret_val3 = plus_self1;
    plus_self2 = ret_val3;
    plus_s2 = ((STR) &name33);
    stdout_self2 = ((FILE1) NULL);
    L16=ZALLOC(sizeof(struct FILE1_struct));
    if (L16==NULL) FATAL("Unable to allocate more memory");
    ((OB)L16)->header.tag=FILE1_tag;
    L15 = ((FILE1) L16);
    r2 = L15;
    
    SATTR(r2,fp,stdout);
    ret_val5 = r2;
    FILE_plus_STR(ret_val5, plus_s2);
    {
     struct FSETTP340014621_frame_struct other2_0;
FSETTP340014621_frame noname2 = &other2_0;
     struct STR_se405450305_frame_struct other2_1;
STR_se405450305_frame noname3 = &other2_1;
     L21 = at;
     L31 = ((STR) &name10);
     noname2->self = L21;
     noname2->state = 0;
     noname3->self = L31;
     noname3->state = 0;
     while (1) {
      create_self1 = ((OUT) NULL);
      ret_val6 = create_self1;
      plus_self3 = ret_val6;
      L19 = FSETTP340014621(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      noname3->arg1 = ((dSTR) TP_CLASS_strrSTR(L19));
      L18 = STR_se405450305(noname3);
      if (noname3->state == -1) {
       goto after_loop1;
      }
      plus_s3 = L18;
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
    after_loop1: ;
    create_self2 = ((OUT) NULL);
    ret_val8 = create_self2;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &name9);
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
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name9);
 stdout_self5 = ((FILE1) NULL);
 L28=ZALLOC(sizeof(struct FILE1_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 ((OB)L28)->header.tag=FILE1_tag;
 L27 = ((FILE1) L28);
 r5 = L27;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void CALL_S371511472(CALL_SIG self, dSTR call11, dSTR arg, dSTR br1, dSTR br_arg) {
 CALL_SIG prog_self;
 PROG ret_val;
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
 dTP L1;
 extern STR Theargumenttype;
 extern STR inthecall;
 extern STR doesntconformto;
 extern STR intheb890863962;
 extern STR name18;
 prog_self = self;
 L1 = ATTR(prog_self,tp);
 ret_val = (*dTP_progrPROG[TAG(L1)])(L1);
 plus_self = STR_pl1270664985(((STR) &Theargumenttype), arg);
 plus_sarg = ((STR) &inthecall);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = STR_pl1270664985(ret_val1, call11);
 plus_sarg1 = ((STR) &doesntconformto);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = STR_pl1270664985(ret_val2, br_arg);
 plus_sarg2 = ((STR) &intheb890863962);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = STR_pl1270664985(ret_val3, br1);
 plus_sarg3 = ((STR) &name18);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 PROG_err_STR(ret_val, ret_val4);
}


#undef IS_ITER
#define IS_ITER 0

void CALL_S407987990(CALL_SIG self, dSTR call11, dSTR arg, dSTR mode1, dSTR br1, dSTR br_arg, dSTR br_mode) {
 CALL_SIG prog_self;
 PROG ret_val;
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
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 dTP L1;
 extern STR Theargumentmode;
 extern STR name10;
 extern STR inthecall;
 extern STR doesntconformto;
 extern STR name10;
 extern STR intheb890863962;
 extern STR name18;
 prog_self = self;
 L1 = ATTR(prog_self,tp);
 ret_val = (*dTP_progrPROG[TAG(L1)])(L1);
 plus_self = STR_pl1270664985(((STR) &Theargumentmode), mode1);
 plus_sarg = ((STR) &name10);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = STR_pl1270664985(ret_val1, arg);
 plus_sarg1 = ((STR) &inthecall);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = STR_pl1270664985(ret_val2, call11);
 plus_sarg2 = ((STR) &doesntconformto);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = STR_pl1270664985(ret_val3, br_mode);
 plus_sarg3 = ((STR) &name10);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = STR_pl1270664985(ret_val4, br_arg);
 plus_sarg4 = ((STR) &intheb890863962);
 ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
 plus_self5 = STR_pl1270664985(ret_val5, br1);
 plus_sarg5 = ((STR) &name18);
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 PROG_err_STR(ret_val, ret_val6);
}


#undef IS_ITER
#define IS_ITER 1

AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 AM_CALL_ARG aI_u_I;
 AM_CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ROU1916046290)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((AM_ROU1916046290)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT AM_ROU2113525054(AM_ROU2113525054_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ROU1916046290)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   rI_u_I=frame->i_I_u_I; frame->i_I_u_I++;
   return rI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void AM_ROU545684847(AM_ROU545684847_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((AM_ROU1916046290)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((AM_ROU1916046290)frame->self,frame->i_I_u_I,(AM_CALL_ARG)frame->arg1); frame->i_I_u_I++;
   ;
   frame->state = 1;
   return;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return;
}
