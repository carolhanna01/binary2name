#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

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

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYd1413132360_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1413132360_frame;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYd1730019671_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1730019671_frame;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ARRAYARRAYSTR_struct {/* layout for ARRAY{ARRAY{STR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *ARRAYARRAYSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

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

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

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

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

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

typedef struct MANGLE_struct {/* layout for MANGLE */
 OB_HEADER header;
 struct FMAPdOBNAMESPACE_struct *namespaces;
 struct FSETSTR_struct *forbidden;
 struct FSETSTR_struct *used_by_local;
 INT max_identifier_l;
 struct NAMESPACE_struct *global_space;
 struct PROG_struct *prog;
 BOOL append_hash_to_i;
 } *MANGLE;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct PRINT_OB_struct {/* layout for PRINT_OB */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *PRINT_OB;

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

typedef struct SIG_TBL_struct {/* layout for SIG_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *SIG_TBL;

typedef struct SIG_TB788365833_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: q */
 SIG ret_val6;
 INT h;
 SIG e;
 SIG e1;
 SIG_TBL query_hash_self;
 IDENT query_hash_i;
 INT ret_val;
 IDENT hash_self;
 INT ret_val1;
 INT x;
 SIG_TBL is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val3;
 SIG_TBL is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val4;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val5;
 } *SIG_TB788365833_frame;

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

typedef struct TUPdOBNAMESPACE_struct {/* layout for TUP{$OB,NAMESPACE} */
 OB t1;
 struct NAMESPACE_struct *t2;
 } TUPdOBNAMESPACE;
static TUPdOBNAMESPACE TUPdOBNAMESPACE_zero;

typedef struct TUPdOBNAMESPACE_boxed_struct {
 OB_HEADER header;
 TUPdOBNAMESPACE immutable_part;
 } *TUPdOBNAMESPACE_boxed;

typedef struct TUPdOBSTR_struct {/* layout for TUP{$OB,STR} */
 OB t1;
 STR t2;
 } TUPdOBSTR;
static TUPdOBSTR TUPdOBSTR_zero;

typedef struct TUPdOBSTR_boxed_struct {
 OB_HEADER header;
 TUPdOBSTR immutable_part;
 } *TUPdOBSTR_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

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

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

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

typedef struct AM_SYNC_STMT_struct {/* layout for AM_SYNC_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_SYNC_STMT;

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

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYd244347909_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 INT arg2;/* Formal argument: num */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Im; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd244347909_frame;

typedef struct ARRAYA712604839_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA712604839_frame;

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

typedef struct ARRAYS1828691793_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1828691793_frame;

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

typedef struct CGEN_struct {/* layout for CGEN */
 OB_HEADER header;
 struct AM_ROUT_DEF_struct *current_am_rout_;
 struct ARRAYSTR_struct *current_arg_list;
 struct ARRAYSTR_struct *current_c_arg_li;
 struct ARRAYSTR_struct *current_fortran_;
 struct CODE_FILE_struct *code_c;
 struct CODE_FILE_struct *comp_options;
 struct CODE_FILE_struct *dispatch_c;
 struct CODE_FILE_struct *globals_c;
 struct CODE_FILE_struct *link_options;
 struct CODE_FILE_struct *makefile;
 struct CODE_FILE_struct *print_c;
 struct CODE_FILE_struct *sather_h;
 struct CODE_FILE_struct *strings_c;
 struct CODE_FILE_struct *system_c;
 struct CODE_FILE_struct *tags_h;
 struct CODE_FILE_struct *unbox_c;
 struct CS_OPTIONS_struct *options1;
 struct FLISTA1893461511_struct *bnd_iter_creates;
 struct FLISTA1893461511_struct *bnd_rout_creates;
 struct FLISTA1893461511_struct *bnd_rout_creates1;
 struct FLISTA725283029_struct *current_iter_out;
 struct FLISTAM_ROUT_DEF_struct *abstract_iters;
 struct FLISTAM_ROUT_DEF_struct *abstract_routs;
 struct FLISTSTR_struct *archive_files;
 struct FLISTSTR_struct *c_flags;
 struct FMAPdTPCODE_FILE_struct *code_files;
 struct FMAPdTPINT_struct *tags;
 struct FMAPAM1244483504_struct *nested_bits;
 struct FMAPAM1787628573_struct *nested_its;
 struct FMAPSI518162669_struct *itersig_map;
 struct FMAPSI518162669_struct *not_emitted;
 struct FMAPSIGSTR_struct *emitted_dispatch;
 struct FMAPSTRFSETSTR_struct *externals;
 struct FMAPSTRSTR_struct *string_constants;
 struct FSETdTP_struct *needs_tag;
 struct FSETAM_ROUT_DEF_struct *emitted_leftover;
 struct FSETAM_ROUT_DEF_struct *leftovers;
 struct FSETSIG_struct *arg_frames;
 struct FSETSIG_struct *emitted_iter_all;
 struct FSETSTR_struct *c_files;
 struct FSETSTR_struct *force_routines;
 struct FSETSTR_struct *object_files;
 struct FSTR_struct *routine_code;
 struct GENERATE_AM_struct *gen;
 INT abstract_calls;
 INT builtin_cntr_n;
 INT concrete_calls;
 INT current_loop_ex_;
 INT ex_nesting;
 INT indent;
 INT inlined_iter_cou;
 INT inlined_pointer_;
 INT inlined_value_ob;
 INT last_lineno;
 INT routine_count;
 INT state_counter;
 INT str_count;
 INT threshold;
 struct MANGLE_struct *mangler;
 struct PROG_struct *prog;
 struct SIG_struct *current_sig;
 struct SIG_struct *main_sig;
 STR code_dir;
 STR comp_home;
 STR current_function;
 STR current_loop;
 STR executable1;
 STR fortran_append_u;
 STR fortran_prepend_;
 STR func_name_commen;
 STR func_name_suffix;
 STR last_file;
 BOOL chk_arith;
 BOOL chk_assert;
 BOOL chk_bounds;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL chk_return;
 BOOL chk_void;
 BOOL chk_when;
 BOOL debug1;
 BOOL deterministic1;
 BOOL force_all;
 BOOL fortran_bind_fun;
 BOOL func_tables;
 BOOL gen_c;
 BOOL ignore_lock;
 BOOL in_bnd_iter_call;
 BOOL in_bnd_rout_call;
 BOOL in_constant;
 BOOL manual_loop_unlo;
 BOOL manual_unlock;
 BOOL no_destroy;
 BOOL no_destroy_lock;
 BOOL null_segfaults;
 BOOL only_C;
 BOOL only_check;
 BOOL optimize;
 BOOL polling;
 BOOL pretty;
 BOOL print_ob;
 BOOL print_ob_g;
 BOOL saw_outer_return;
 BOOL siva;
 BOOL type_tables;
 } *CGEN;

typedef struct CODE_FILE_struct {/* layout for CODE_FILE */
 OB_HEADER header;
 struct FLISTA1893461511_struct *bnd_iter_creates;
 struct FLISTFSTR_struct *text1;
 struct FSETdLAYOUT_struct *layouts;
 struct FSETAM1032778315_struct *bnd_rout_creates;
 struct FSETAM1542678947_struct *globals;
 struct FSETSIG_struct *decs;
 struct FSETSIG_struct *iters;
 struct FSETSTR_struct *externs1;
 struct FSETTUPSIGSIG_struct *unboxes;
 struct FSTR_struct *ntext;
 INT length1;
 STR name1;
 BOOL chk_arith;
 BOOL chk_assert;
 BOOL chk_bounds;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL chk_return;
 BOOL chk_void;
 BOOL chk_when;
 BOOL do_not_merge;
 BOOL is_c_code;
 } *CODE_FILE;

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

typedef struct FLISTd2122660729_frame_struct {
 INT state;
 FLISTdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTdAM_EXPR aget_self;
 INT aget_ind;
 dAM_EXPR ret_val;
 } *FLISTd2122660729_frame;

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTd1374595573_frame_struct {
 INT state;
 FLISTdAM_STMT self;/* Formal argument: self */
 dAM_STMT ret_val1;
 INT i;
 INT sz;
 FLISTdAM_STMT aget_self;
 INT aget_ind;
 dAM_STMT ret_val;
 } *FLISTd1374595573_frame;

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

typedef struct FLISTd1990200313_frame_struct {
 INT state;
 FLISTdTP self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTd1990200313_frame;

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

typedef struct FLISTBOOL_struct {/* layout for FLIST{BOOL} */
 OB_HEADER header;
 INT loc;
 INT asize;
 BOOL arr_part[1];
 } *FLISTBOOL;

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

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

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

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

typedef struct NAMEMAP_struct {/* layout for NAMEMAP */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *NAMEMAP;

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

extern TP_CLASS TP_BUI1325635093;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_2144735300)(dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST SFILE_ID (**dAM_ST1805574914)(dAM_STMT);

extern RETURNED_CONST SFILE_ID (**dAM_so1457869664)(dAM);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
ARRAYSTR CGEN_e1933193154(CGEN, dAM_CALL_EXPR);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
BOOL FLISTB466509411(FLISTBOOL);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA1568240525(TP_CLASS);
ELT ELT_TB431570775(ELT_TB431570775_frame);
FLISTBOOL FLISTB1991913204(FLISTBOOL, BOOL);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSETdTP FSETdT1854975287(FSETdTP, FSETdTP);
FSETdTP FSETdT217460096(FSETdTP);
FSETdTP FSETdT653600961(FSETdTP, FSETdTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
IMPL TP_CLA1109727233(TP_CLASS);
INT ARRAYA712604839(ARRAYA712604839_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT FLISTA1531002815(FLISTA1327600838);
INT FLISTF873512073(FLISTF7594819);
INT FLISTd1855368276(FLISTdAM_CONST);
INT FLISTd1990200313(FLISTd1990200313_frame);
INT FSETdTP_sizerINT(FSETdTP);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
SIG SIG_TB788365833(SIG_TB788365833_frame);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e1835933683(CGEN, SIG, dAM_EXPR, ARRAYSTR);
STR CGEN_f280967020(CGEN, STR, AM_LOCK_STMT);
STR MANGLE119219986(MANGLE, OB, OB);
STR NAMESP8552102(NAMESPACE, STR);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1182453786(STR, STR, STR, STR, STR, STR, STR);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
TP_CLASS TP_TBL1484824239(TP_TBL, STR, INT*);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
dAM_EXPR FLISTd2122660729(FLISTd2122660729_frame);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dTP ARRAYd244347909(ARRAYd244347909_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void AM_ROU545684847(AM_ROU545684847_frame);
void ARRAYd1239612935(ARRAYdTP, INT, INT);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void CGEN_a1893106143(CGEN, SFILE_ID);
void CGEN_c9785167(CGEN);
void CGEN_d695831906(CGEN);
void CGEN_e101922820(CGEN, AM_RETURN_STMT);
void CGEN_e1197535054(CGEN, AM_LOOP_STMT);
void CGEN_e1270633199(CGEN, AM_PAR_STMT);
void CGEN_e1378643769(CGEN, AM_INV1167837230);
void CGEN_e1389302728(CGEN, AM_ASSIGN_STMT);
void CGEN_e1441236469(CGEN, AM_YIELD_STMT);
void CGEN_e1553420074(CGEN, AM_LOCK_STMT);
void CGEN_e1669826382(CGEN, AM_EXPR_STMT);
void CGEN_e1809841360(CGEN, AM_ATTACH_STMT);
void CGEN_e1959168490(CGEN, AM_FORK_STMT);
void CGEN_e2137132086(CGEN, dAM_EXPR, FLISTdTP, FLISTdAM_STMT);
void CGEN_e2141826530(CGEN, AM_TYPECASE_STMT);
void CGEN_e286683954(CGEN, AM_CASE_STMT);
void CGEN_e290817711(CGEN, AM_WIT1996971603);
void CGEN_e398507978(CGEN, AM_PREFETCH_STMT);
void CGEN_e494460490(CGEN, dAM_STMT);
void CGEN_e520888267(CGEN, AM_ROUT_DEF);
void CGEN_e638028220(CGEN, AM_IF_STMT);
void CGEN_e881275767(CGEN, AM_PROTECT_STMT);
void CGEN_g628546529(CGEN, AM_ROUT_DEF, ARRAYSTR*, ARRAYSTR*);
void CGEN_newline(CGEN);
void CGEN_r878823253(CGEN, STR, dAM);
void CODE_F1291349628(CODE_FILE, SIG);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PRINT_721582855(PRINT_OB, PROG, CGEN, CODE_FILE, AM_ROUT_DEF, FLISTA725283029);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

STR CGEN_f280967020(CGEN self, STR ident1, AM_LOCK_STMT arg) {
 STR ret_val;
 AM_ROU1916046290 am_call;
 ARRAYdAM_EXPR L11;
 AM_ROU1916046290 L21;
 dTP tp;
 TP_CLASS lock_tp;
 IDENT method_id = IDENT_zero;
 ELT_TBL L31;
 IDENT L41 = IDENT_zero;
 SIG_TBL L51;
 IDENT L61 = IDENT_zero;
 FLISTA1327600838 aget_self;
 INT aget_ind = INT_zero;
 ARRAYdAM_EXPR ret_val1;
 AM_ROU1916046290 create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROU1916046290 ret_val2;
 AM_ROU1916046290 r;
 FLISTA1327600838 aget_self1;
 INT aget_ind1 = INT_zero;
 ARRAYdAM_EXPR ret_val3;
 INT L71 = INT_zero;
 AM_CALL_ARG create_self1;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val4;
 AM_CALL_ARG res;
 FLISTA1327600838 aget_self2;
 INT aget_ind2 = INT_zero;
 ARRAYdAM_EXPR ret_val5;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 UNIX exit_self;
 INT exit_code = INT_zero;
 TP_TBL tp_class_for_sel;
 STR tp_class_for_s;
 TP_CLASS ret_val7;
 INT i = INT_zero;
 ARRAYdAM_EXPR L81_;
 ARRAYdAM_EXPR L9;
 INT L101_;
 AM_ROU1916046290 L12;
 INT L13;
 OB L14;
 INT L15;
 INT L171_br;
 dAM_EXPR L18;
 ARRAYdAM_EXPR L191_;
 INT L161_br;
 dAM_EXPR aL161_;
 AM_CALL_ARG L20;
 OB L22;
 AM_CALL_ARG L23;
 INT L241_;
 ARRAYdAM_EXPR L251_;
 ARRAYdAM_EXPR L26;
 dAM_EXPR L271_;
 dAM_EXPR L28;
 extern STR Badtyp271192016;
 extern STR Intern28965746;
 BOOL L29;
 BOOL L301_;
 ELT L32;
 IMPL L33;
 ELT L34;
 extern STR dLOCK;
 INT* L35;
 TP_CLASS L36;
 SIG L37;
 IFC L38;
 SIG L39;
 create_self = ((AM_ROU1916046290) NULL);
 aget_self = ATTR(arg,locks1);
 aget_ind = 0;
 L81_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)aget_self,aget_ind); 
 ret_val1 = L81_;
 L9 = ret_val1;
 L101_=ASIZE((ARRAYdAM_EXPR)L9); 
 create_nargs = L101_;
 create_source = ATTR(arg,source1);
 L13 = create_nargs;
 L15=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L13)*sizeof(AM_CALL_ARG);
 L14=ZALLOC_BIG(L15);
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

   ((OB)L14)->header.destroyed=0;
#endif

 L12 = ((AM_ROU1916046290) L14);
 L12->asize = L13;
 r = L12;
 SATTR(r,source1,create_source);
 ret_val2 = r;
 am_call = ret_val2;
 {
  BOOL f_L161_ = TRUE;
  BOOL f_L171_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L21 = am_call;
  L171_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
  while (1) {
   if(L71>=L171_br)  goto after_loop; 
   create_self1 = ((AM_CALL_ARG) NULL);
   if (f_L161_) {
    f_L161_ = FALSE;
    aget_self1 = ATTR(arg,locks1);
    aget_ind1 = 0;
    L191_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)aget_self1,aget_ind1); 
    ret_val3 = L191_;
    L11 = ret_val3;
    L161_br=L11==NULL?0:ASIZE((ARRAYdAM_EXPR)L11); 
   }
   if(L71>=L161_br)  goto after_loop; 
   aL161_=ARR((ARRAYdAM_EXPR)L11,L71); 
   create_e = aL161_;
   L22=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=AM_CALL_ARG_tag;
   L20 = ((AM_CALL_ARG) L22);
   res = L20;
   SATTR(res,expr,create_e);
   SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
   ret_val4 = res;
   L23 = ret_val4;
   SARR((AM_ROU1916046290)L21,L71,(AM_CALL_ARG)L23); 
   ;
   L241_=INTPLUS(L71,1); 
   L71 = L241_;
  }
 }
 after_loop: ;
 aget_self2 = ATTR(arg,locks1);
 aget_ind2 = 0;
 L251_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)aget_self2,aget_ind2); 
 ret_val5 = L251_;
 L26 = ret_val5;
 L271_=(dAM_EXPR)ARR((ARRAYdAM_EXPR)L26,0); 
 L28 = L271_;
 tp = (*dAM_EXPR_tprdTP[TAG(L28)])(L28);
 lock_tp = ((TP_CLASS) NULL);
 switch (TAG(tp)) {
  case TP_CLASS_tag:
   lock_tp = ((TP_CLASS) tp); break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::find_and_emit_rout_call(STR,AM_LOCK_STMT):STR\n./Back/cgen.sa:4029:14");
 }
 if ((lock_tp==((TP_CLASS) NULL))) {
  barf_self = self;
  barf_msg = ((STR) &Badtyp271192016);
  barf_self1 = ATTR(barf_self,prog);
  barf_msg1 = barf_msg;
  barf_at_self = barf_self1;
  barf_at_msg = barf_msg1;
  barf_at_at = ((dPROG_ERR) NULL);
  PROG_e176405615(barf_at_self, barf_at_at);
  plus_self = ((STR) &Intern28965746);
  plus_sarg = barf_at_msg;
  ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
  PROG_err_STR(barf_at_self, ret_val6);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 method_id = IDENT_1150413730(IDENT_zero, ident1);
 L29 = TP_CLA1568240525(lock_tp);
 L301_=!(L29); 
 if (L301_) {
  {
   struct ELT_TB431570775_frame_struct other2_0;
ELT_TB431570775_frame noname1 = &other2_0;
   noname1->state = 0;
   while (1) {
    if (noname1->state == 0) {
     L33=TP_CLA1109727233(lock_tp);
     L31 = ATTR(L33,elts);
     L41 = method_id;
     noname1->self = L31;
     noname1->arg1 = L41;
    }
    L32 = ELT_TB431570775(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    L34=L32;
    SATTR(am_call,fun,ATTR(L34,sig1));
    goto after_loop1;
   }
  }
  after_loop1: ;
 }
 else {
  tp_class_for_sel = ATTR(ATTR(self,prog),tp_tbl);
  tp_class_for_s = ((STR) &dLOCK);
  i = 0;
  L35 = &i;
  L36 = TP_TBL1484824239(tp_class_for_sel, tp_class_for_s, L35);
  ret_val7 = L36;
  lock_tp = ret_val7;
  {
   struct SIG_TB788365833_frame_struct other3_0;
SIG_TB788365833_frame noname2 = &other3_0;
   noname2->state = 0;
   while (1) {
    if (noname2->state == 0) {
     L38=TP_CLASS_ifcrIFC(lock_tp);
     L51 = ATTR(L38,sigs);
     L61 = method_id;
     noname2->self = L51;
     noname2->arg1 = L61;
    }
    L37 = SIG_TB788365833(noname2);
    if (noname2->state == -1) {
     goto after_loop2;
    }
    SATTR(am_call,fun,L37);
    goto after_loop2;
   }
  }
  after_loop2: ;
 }
 L39 = ATTR(am_call,fun);
 ret_val = CGEN_e1835933683(self, L39, ((dAM_EXPR) am_call), CGEN_e1933193154(self, ((dAM_CALL_EXPR) am_call)));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1270633199(CGEN self, AM_PAR_STMT stmt1) {
 CGEN comment_self;
 STR comment_com;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val3;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val6;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN move_out_self;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN comment_self2;
 STR comment_com2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 CGEN ndefer_self7;
 STR ndefer_s7;
 extern STR PARstart;
 extern STR name54;
 extern STR name79;
 extern STR do5;
 INT L1;
 INT L21_;
 extern STR PAR_DEC;
 extern STR name76;
 extern STR PAR_START;
 extern STR name76;
 extern STR PARbody;
 extern STR name54;
 extern STR name79;
 extern STR PAR_END;
 extern STR name76;
 INT L3;
 INT L41_;
 extern STR while02;
 extern STR PARend;
 extern STR name54;
 extern STR name79;
 comment_self = self;
 comment_com = ((STR) &PARstart);
 if (ATTR(comment_self,pretty)) {
  ndefer_self = comment_self;
  plus_self = ((STR) &name54);
  plus_sarg = comment_com;
  ret_val = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val;
  plus_sarg1 = ((STR) &name79);
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s = ret_val1;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 ndefer_self1 = self;
 ndefer_s1 = ((STR) &do5);
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 in_self = self;
 L1 = ATTR(in_self,indent);
 L21_=INTPLUS(L1,1); 
 SATTR(in_self,indent,L21_);
 ndefer_self2 = self;
 mang_self = self;
 mang_ob = ((OB) stmt1);
 ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 ndefer_s2 = STR_ap1693864410(((STR) &PAR_DEC), ret_val2, ((STR) &name76));
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 ndefer_self3 = self;
 mang_self1 = self;
 mang_ob1 = ((OB) stmt1);
 ret_val3 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 ndefer_s3 = STR_ap1693864410(((STR) &PAR_START), ret_val3, ((STR) &name76));
 CGEN_d695831906(ndefer_self3);
 SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 comment_self1 = self;
 comment_com1 = ((STR) &PARbody);
 if (ATTR(comment_self1,pretty)) {
  ndefer_self4 = comment_self1;
  plus_self2 = ((STR) &name54);
  plus_sarg2 = comment_com1;
  ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val4;
  plus_sarg3 = ((STR) &name79);
  ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
  ndefer_s4 = ret_val5;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 CGEN_e494460490(self, ATTR(stmt1,body));
 ndefer_self5 = self;
 mang_self2 = self;
 mang_ob2 = ((OB) stmt1);
 ret_val6 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 ndefer_s5 = STR_ap1693864410(((STR) &PAR_END), ret_val6, ((STR) &name76));
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 move_out_self = self;
 L3 = ATTR(move_out_self,indent);
 L41_=INTMINUS(L3,1); 
 SATTR(move_out_self,indent,L41_);
 ndefer_self6 = self;
 ndefer_s6 = ((STR) &while02);
 CGEN_d695831906(ndefer_self6);
 SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 comment_self2 = self;
 comment_com2 = ((STR) &PARend);
 if (ATTR(comment_self2,pretty)) {
  ndefer_self7 = comment_self2;
  plus_self4 = ((STR) &name54);
  plus_sarg4 = comment_com2;
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
  plus_self5 = ret_val7;
  plus_sarg5 = ((STR) &name79);
  ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
  ndefer_s7 = ret_val8;
  CGEN_d695831906(ndefer_self7);
  SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1378643769(CGEN self, AM_INV1167837230 arg) {
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val3;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val4;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val5;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN in_self;
 CGEN move_out_self;
 CGEN ndefer_self4;
 STR ndefer_s4;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 STR L6;
 INT L7;
 INT L81_;
 INT L9;
 CHAR L101_;
 CHAR L11;
 BOOL L121_;
 extern STR ifb;
 extern STR frameselfFF;
 extern STR ifb;
 extern STR frameself;
 extern STR ifb;
 extern STR selfFF;
 extern STR ifb;
 extern STR self7;
 INT L13;
 INT L141_;
 extern STR Failedinvariant;
 STR L15;
 INT L16;
 INT L171_;
 extern STR name4;
 if (ATTR(self,chk_invariant)) {
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(arg,sig1);
  ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
  L1 = (ard==((AM_ROUT_DEF) NULL));
  L21_=!(L1); 
  if (L21_) {
   SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
   SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
  }
  is_iter_self = ATTR(self,current_sig);
  is_iter_self1 = ATTR(is_iter_self,name1);
  L4 = (is_iter_self1.str==((STR) NULL));
  L51_=!(L4); 
  if (L51_) {
   L6 = is_iter_self1.str;
   L7 = STR_sizerINT(is_iter_self1.str);
   L81_=INTMINUS(L7,1); 
   L9 = L81_;
   L101_=ARR((STR)L6,L9); 
   L11 = L101_;
   L121_=L11=='!'; 
   L3 = L121_;
  } else {
   L3 = FALSE;
  }
  ret_val1 = L3;
  ret_val = ret_val1;
  if (ret_val) {
   if (ATTR(self,func_tables)) {
    ndefer_self = self;
    mang_self = self;
    mang_ob = ((OB) ATTR(arg,sig1));
    ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
    ndefer_s = STR_ap1693864410(((STR) &ifb), ret_val2, ((STR) &frameselfFF));
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   else {
    ndefer_self1 = self;
    mang_self1 = self;
    mang_ob1 = ((OB) ATTR(arg,sig1));
    ret_val3 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    ndefer_s1 = STR_ap1693864410(((STR) &ifb), ret_val3, ((STR) &frameself));
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
  }
  else {
   if (ATTR(self,func_tables)) {
    ndefer_self2 = self;
    mang_self2 = self;
    mang_ob2 = ((OB) ATTR(arg,sig1));
    ret_val4 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    ndefer_s2 = STR_ap1693864410(((STR) &ifb), ret_val4, ((STR) &selfFF));
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   }
   else {
    ndefer_self3 = self;
    mang_self3 = self;
    mang_ob3 = ((OB) ATTR(arg,sig1));
    ret_val5 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
    ndefer_s3 = STR_ap1693864410(((STR) &ifb), ret_val5, ((STR) &self7));
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   }
  }
  in_self = self;
  L13 = ATTR(in_self,indent);
  L141_=INTPLUS(L13,1); 
  SATTR(in_self,indent,L141_);
  L15 = STR_ap2004550586(((STR) &Failedinvariant), ATTR(ATTR(arg,sig1),str));
  CGEN_r878823253(self, L15, ((dAM) arg));
  move_out_self = self;
  L16 = ATTR(move_out_self,indent);
  L171_=INTMINUS(L16,1); 
  SATTR(move_out_self,indent,L171_);
  ndefer_self4 = self;
  ndefer_s4 = ((STR) &name4);
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1441236469(CGEN self, AM_YIELD_STMT arg) {
 INT str_self = INT_zero;
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 PROG psather_self;
 BOOL ret_val3 = BOOL_zero;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val4;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 INT str_self2 = INT_zero;
 STR ret_val5;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val6;
 FSTR str_self3;
 STR ret_val7;
 CGEN ndefer_self4;
 STR ndefer_s4;
 extern STR framestate;
 BOOL L1;
 BOOL L21_;
 extern STR name16;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 SE_CONTEXT L11;
 extern STR framee1759213052;
 BOOL L12;
 BOOL L131_;
 extern STR return4;
 dAM_EXPR L14;
 extern STR name16;
 extern STR return5;
 extern STR state2;
 BOOL L15;
 BOOL L161_;
 extern STR name138;
 INT L17;
 INT L181_;
 ndefer_self = self;
 str_self = ATTR(arg,ret);
 clear_self = buf1;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val1 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val1;
 str_self1 = buf1;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 ndefer_s = STR_ap1693864410(((STR) &framestate), ret_val, ((STR) &name16));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L4 = TRUE;
 } else {
  L4 = ATTR(psather_self,distributed);
 }
 ret_val3 = L4;
 if (ret_val3) {
  L7 = ATTR(ATTR(self,prog),yields_in_locks);
  L81_=!(L7); 
  if (L81_) {
   L6 = TRUE;
  } else {
   L9 = ATTR(ATTR(self,options1),side_effects);
   L101_=!(L9); 
   L6 = L101_;
  }
  if (L6) {
   L5 = TRUE;
  } else {
   L11=SIG_ge642497605(ATTR(self,current_sig), ATTR(self,prog));
   L5 = ATTR(L11,has_yield_in_loc);
  }
  L3 = L5;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  ndefer_self1 = self;
  ndefer_s1 = ((STR) &framee1759213052);
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 }
 CGEN_c9785167(self);
 L12 = (ATTR(arg,val1)==((dAM_EXPR) NULL));
 L131_=!(L12); 
 if (L131_) {
  ndefer_self2 = self;
  cast_self = self;
  cast_dest_tp = ATTR(ATTR(self,current_sig),ret);
  L14 = ATTR(arg,val1);
  cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L14)])(L14);
  cast_expr = CGEN_e1639504572(self, ATTR(arg,val1));
  cast_only_boxing = FALSE;
  ret_val4 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  ndefer_s2 = STR_ap1693864410(((STR) &return4), ret_val4, ((STR) &name16));
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 else {
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &return5);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 ndefer_self4 = self;
 str_self2 = ATTR(self,state_counter);
 clear_self1 = buf2;
 L15 = (clear_self1==((FSTR) NULL));
 L161_=!(L15); 
 if (L161_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val6 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val6;
 str_self3 = buf2;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val5 = ret_val7;
 ndefer_s4 = STR_ap1693864410(((STR) &state2), ret_val5, ((STR) &name138));
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 L17 = ATTR(self,state_counter);
 L181_=INTPLUS(L17,1); 
 SATTR(self,state_counter,L181_);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1553420074(CGEN self, AM_LOCK_STMT arg) {
 INT branches1 = INT_zero;
 INT cols = INT_zero;
 FLISTA1327600838 L11;
 STR stmt1 = ((STR) NULL);
 STR this_lock_str;
 STR stmt2;
 INT L21 = INT_zero;
 INT c = INT_zero;
 FLISTA1327600838 L31;
 ARRAYdAM_EXPR l;
 FLISTdAM_EXPR L41;
 dAM_EXPR g;
 INT L51 = INT_zero;
 INT ln = INT_zero;
 ARRAYdAM_EXPR L61;
 FLISTdAM_STMT L71;
 dAM_STMT stt;
 INT L81 = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 FLISTA1327600838 aget_self2;
 INT aget_ind2 = INT_zero;
 ARRAYdAM_EXPR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN in_self;
 FLISTdAM_STMT aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_STMT ret_val8;
 CGEN move_out_self;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 CGEN ndefer_self5;
 STR ndefer_s5;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 CGEN ndefer_self6;
 STR ndefer_s6;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val14;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val15;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN in_self1;
 FLISTdAM_STMT aget_self4;
 INT aget_ind4 = INT_zero;
 dAM_STMT ret_val16;
 CGEN move_out_self1;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val18;
 CGEN ndefer_self8;
 STR ndefer_s8;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val19;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN in_self2;
 CGEN move_out_self2;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN ndefer_self12;
 STR ndefer_s12;
 STR plus_self15;
 INT plus_arg = INT_zero;
 STR ret_val20;
 STR s;
 INT str_self = INT_zero;
 STR ret_val21;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val22;
 FSTR str_self1;
 STR ret_val23;
 STR plus_self16;
 STR plus_sarg15;
 STR ret_val24;
 STR plus_self17;
 INT plus_arg1 = INT_zero;
 STR ret_val25;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val26;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val27;
 FSTR str_self3;
 STR ret_val28;
 STR plus_self18;
 STR plus_sarg16;
 STR ret_val29;
 STR plus_self19;
 STR plus_sarg17;
 STR ret_val30;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val31;
 STR plus_self21;
 STR plus_sarg19;
 STR ret_val32;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self22;
 STR plus_sarg20;
 STR ret_val33;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self23;
 STR plus_sarg21;
 STR ret_val34;
 STR plus_self24;
 STR plus_sarg22;
 STR ret_val35;
 STR plus_self25;
 STR plus_sarg23;
 STR ret_val36;
 STR plus_self26;
 STR plus_sarg24;
 STR ret_val37;
 STR plus_self27;
 INT plus_arg2 = INT_zero;
 STR ret_val38;
 STR s2;
 INT str_self4 = INT_zero;
 STR ret_val39;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val40;
 FSTR str_self5;
 STR ret_val41;
 STR plus_self28;
 STR plus_sarg25;
 STR ret_val42;
 STR plus_self29;
 INT plus_arg3 = INT_zero;
 STR ret_val43;
 STR s3;
 INT str_self6 = INT_zero;
 STR ret_val44;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val45;
 FSTR str_self7;
 STR ret_val46;
 STR plus_self30;
 STR plus_sarg26;
 STR ret_val47;
 INT L91 = INT_zero;
 STR plus_self31;
 STR plus_sarg27;
 STR ret_val48;
 STR plus_self32;
 STR plus_sarg28;
 STR ret_val49;
 STR plus_self33;
 STR plus_sarg29;
 STR ret_val50;
 CGEN ndefer_self14;
 STR ndefer_s14;
 CGEN ndefer_self15;
 STR ndefer_s15;
 CGEN ndefer_self16;
 STR ndefer_s16;
 CGEN in_self3;
 FLISTdAM_STMT aget_self5;
 INT aget_ind5 = INT_zero;
 dAM_STMT ret_val51;
 CGEN move_out_self3;
 CGEN ndefer_self17;
 STR ndefer_s17;
 CGEN ndefer_self18;
 STR ndefer_s18;
 CGEN ndefer_self19;
 STR ndefer_s19;
 CGEN ndefer_self20;
 STR ndefer_s20;
 CGEN in_self4;
 FLISTdAM_STMT aget_self6;
 INT aget_ind6 = INT_zero;
 dAM_STMT ret_val52;
 CGEN move_out_self4;
 CGEN ndefer_self21;
 STR ndefer_s21;
 CGEN ndefer_self22;
 STR ndefer_s22;
 CGEN ndefer_self23;
 STR ndefer_s23;
 CGEN in_self5;
 CGEN move_out_self5;
 CGEN ndefer_self24;
 STR ndefer_s24;
 CGEN ndefer_self25;
 STR ndefer_s25;
 CGEN ndefer_self26;
 STR ndefer_s26;
 CGEN in_self6;
 FLISTdAM_STMT aget_self7;
 INT aget_ind7 = INT_zero;
 dAM_STMT ret_val53;
 CGEN move_out_self6;
 CGEN ndefer_self27;
 STR ndefer_s27;
 CGEN ndefer_self28;
 STR ndefer_s28;
 CGEN ndefer_self29;
 STR ndefer_s29;
 CGEN ndefer_self30;
 STR ndefer_s30;
 CGEN in_self7;
 FLISTdAM_STMT aget_self8;
 INT aget_ind8 = INT_zero;
 dAM_STMT ret_val54;
 CGEN move_out_self7;
 CGEN ndefer_self31;
 STR ndefer_s31;
 CGEN ndefer_self32;
 STR ndefer_s32;
 CGEN ndefer_self33;
 STR ndefer_s33;
 CGEN in_self8;
 CGEN move_out_self8;
 CGEN ndefer_self34;
 STR ndefer_s34;
 CGEN ndefer_self35;
 STR ndefer_s35;
 STR plus_self34;
 INT plus_arg4 = INT_zero;
 STR ret_val55;
 STR s4;
 INT str_self8 = INT_zero;
 STR ret_val56;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val57;
 FSTR str_self9;
 STR ret_val58;
 STR plus_self35;
 STR plus_sarg30;
 STR ret_val59;
 CGEN ndefer_self36;
 STR ndefer_s36;
 CGEN ndefer_self37;
 STR ndefer_s37;
 CGEN in_self9;
 CGEN move_out_self9;
 CGEN ndefer_self38;
 STR ndefer_s38;
 CGEN ndefer_self39;
 STR ndefer_s39;
 CGEN ndefer_self40;
 STR ndefer_s40;
 CGEN in_self10;
 CGEN move_out_self10;
 CGEN ndefer_self41;
 STR ndefer_s41;
 CGEN ndefer_self42;
 STR ndefer_s42;
 extern STR Oofloc443792109;
 dSTAT L10;
 ARRAYdAM_EXPR L12;
 ARRAYdAM_EXPR L13;
 INT L141_;
 INT L15;
 INT L161_;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 BOOL L201_;
 dAM_EXPR L221_;
 BOOL L23;
 BOOL L241_;
 extern STR if4;
 dAM_EXPR L251_;
 extern STR name84;
 extern STR name2;
 ARRAYdAM_EXPR L261_;
 ARRAYdAM_EXPR L27;
 dAM_EXPR L281_;
 extern STR access753243259;
 extern STR name16;
 extern STR voidpp1939092441;
 extern STR name16;
 INT L29;
 INT L301_;
 dAM_STMT L321_;
 INT L33;
 INT L341_;
 extern STR locked_lock;
 extern STR name16;
 extern STR access1838956836;
 extern STR name102;
 extern STR if3;
 extern STR access955627674;
 extern STR name26;
 extern STR voidpp1939092441;
 extern STR name16;
 INT L35;
 INT L361_;
 dAM_STMT L371_;
 INT L38;
 INT L391_;
 extern STR locked_lock;
 extern STR name16;
 extern STR access1838956836;
 extern STR name16;
 extern STR else5;
 INT L40;
 INT L421_;
 INT L43;
 INT L441_;
 extern STR name4;
 extern STR name4;
 extern STR DECLARE_LOCK1;
 BOOL L45;
 BOOL L461_;
 extern STR name3;
 BOOL L47;
 BOOL L481_;
 extern STR S05;
 BOOL L49;
 BOOL L501_;
 extern STR LOCK_WITH_ELSE1;
 extern STR LOCK_NO_STACK;
 extern STR name26;
 INT L521_;
 INT L53;
 ARRAYdAM_EXPR L54;
 dAM_EXPR L55;
 INT L561_;
 INT L57;
 BOOL L581_;
 extern STR Youcan1429763738;
 extern STR Intern28965746;
 extern STR name1;
 BOOL L59;
 BOOL L601_;
 extern STR if4;
 extern STR name84;
 INT L621_;
 INT L631_br;
 dAM_EXPR aL631_;
 INT L64;
 extern STR ADD_LOCK;
 BOOL L65;
 BOOL L661_;
 extern STR name3;
 BOOL L67;
 BOOL L681_;
 extern STR name3;
 dAM_EXPR L69;
 extern STR name76;
 INT L701_;
 BOOL L72;
 BOOL L731_;
 extern STR name139;
 BOOL L74;
 BOOL L751_;
 INT L76;
 INT L771_;
 BOOL L781_;
 BOOL L791_;
 extern STR LM_ACQ898286306;
 BOOL L80;
 BOOL L821_;
 extern STR LM_PUS2132113989;
 INT L83;
 INT L841_;
 dAM_STMT L851_;
 INT L86;
 INT L871_;
 BOOL L88;
 BOOL L891_;
 INT L90;
 INT L921_;
 extern STR LM_REL804480895;
 BOOL L93;
 BOOL L941_;
 extern STR LM_POP1734626182;
 extern STR LM_TRY563339091;
 BOOL L95;
 BOOL L961_;
 extern STR LM_PUS2132113989;
 INT L97;
 INT L981_;
 dAM_STMT L991_;
 INT L100;
 INT L1011_;
 BOOL L102;
 BOOL L1031_;
 INT L104;
 INT L1051_;
 extern STR LM_REL804480895;
 BOOL L106;
 BOOL L1071_;
 extern STR LM_POP1734626182;
 extern STR else5;
 INT L108;
 INT L1091_;
 INT L110;
 INT L1111_;
 extern STR name4;
 extern STR LM_ACQ1821457010;
 BOOL L112;
 BOOL L1131_;
 extern STR LM_PUS2132113989;
 INT L114;
 INT L1151_;
 dAM_STMT L1161_;
 INT L117;
 INT L1181_;
 BOOL L119;
 BOOL L1201_;
 INT L121;
 INT L1221_;
 extern STR LM_REL1765693927;
 BOOL L123;
 BOOL L1241_;
 extern STR LM_POP1734626182;
 extern STR LM_TRY1921320459;
 BOOL L125;
 BOOL L1261_;
 extern STR LM_PUS2132113989;
 INT L127;
 INT L1281_;
 dAM_STMT L1291_;
 INT L130;
 INT L1311_;
 BOOL L132;
 BOOL L1331_;
 INT L134;
 INT L1351_;
 extern STR LM_REL1765693927;
 BOOL L136;
 BOOL L1371_;
 extern STR LM_POP1734626182;
 extern STR else5;
 INT L138;
 INT L1391_;
 INT L140;
 INT L1421_;
 extern STR name4;
 extern STR LM_SEL917075861;
 INT L1431_;
 dAM_STMT L144;
 extern STR LM_LOCK_CLAUSE;
 INT L145;
 BOOL L146;
 BOOL L1471_;
 extern STR name26;
 BOOL L148;
 BOOL L1491_;
 extern STR LM_PUS2132113989;
 INT L150;
 INT L1511_;
 INT L152;
 INT L1531_;
 extern STR LM_REL1415431597;
 BOOL L154;
 BOOL L1551_;
 extern STR LM_POP1734626182;
 BOOL L156;
 BOOL L1571_;
 INT L158;
 INT L1591_;
 BOOL L160;
 BOOL L1621_;
 extern STR LM_ELSE_CLAUSE;
 INT L163;
 INT L1641_;
 INT L165;
 INT L1661_;
 extern STR LM_END629067339;
 extern STR LM_CLO1780161327;
 if (ATTR(self,ignore_lock)) {
  SATTR(self,ignore_lock,FALSE);
  return;
 }
 if (ATTR(arg,manual_unlock)) {
  L10 = ATTR(ATTR(self,prog),stat1);
  (*dSTAT_incr_STR[TAG(L10)])(L10, ((STR) &Oofloc443792109));
  SATTR(self,manual_unlock,TRUE);
  SATTR(self,manual_loop_unlo,TRUE);
 }
 branches1 = FLISTA1531002815(ATTR(arg,locks1));
 cols = 0;
 {
  struct FLISTA1857087356_frame_struct other1_0;
FLISTA1857087356_frame noname1 = &other1_0;
  L11 = ATTR(arg,locks1);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L12 = FLISTA1857087356(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L13 = L12;
   L141_=(L13)==NULL?0:ASIZE((ARRAYdAM_EXPR)L13); 
   L15 = L141_;
   L161_=(cols>L15)?cols:L15; 
   cols = L161_;
  }
 }
 after_loop: ;
 if (ATTR(self,manual_unlock)) {
  L191_=(branches1)==(1); 
  L18 = L191_;
 } else {
  L18 = FALSE;
 }
 if (L18) {
  L201_=(cols)==(1); 
  L17 = L201_;
 } else {
  L17 = FALSE;
 }
 if (L17) {
  aget_self = ATTR(arg,guards);
  aget_ind = 0;
  L221_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val = L221_;
  L23 = (ret_val==((dAM_EXPR) NULL));
  L241_=!(L23); 
  if (L241_) {
   ndefer_self = self;
   plus_self = ((STR) &if4);
   aget_self1 = ATTR(arg,guards);
   aget_ind1 = 0;
   L251_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
   ret_val1 = L251_;
   plus_sarg = CGEN_e1639504572(self, ret_val1);
   ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val2;
   plus_sarg1 = ((STR) &name84);
   ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
   ndefer_s = ret_val3;
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  }
  else {
   ndefer_self1 = self;
   ndefer_s1 = ((STR) &name2);
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  SATTR(self,no_destroy_lock,TRUE);
  aget_self2 = ATTR(arg,locks1);
  aget_ind2 = 0;
  L261_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)aget_self2,aget_ind2); 
  ret_val4 = L261_;
  L27 = ret_val4;
  L281_=(dAM_EXPR)ARR((ARRAYdAM_EXPR)L27,0); 
  this_lock_str = CGEN_e1639504572(self, L281_);
  SATTR(self,no_destroy_lock,FALSE);
  if ((ATTR(arg,else_stmts)==((dAM_STMT) NULL))) {
   SATTR(self,no_destroy_lock,TRUE);
   ndefer_self2 = self;
   plus_self2 = CGEN_f280967020(self, ((STR) &access753243259), arg);
   plus_sarg2 = ((STR) &name16);
   ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
   ndefer_s2 = ret_val5;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   SATTR(self,no_destroy_lock,FALSE);
   ndefer_self3 = self;
   plus_self3 = ((STR) &voidpp1939092441);
   plus_sarg3 = this_lock_str;
   ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val6;
   plus_sarg4 = ((STR) &name16);
   ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
   ndefer_s3 = ret_val7;
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   in_self = self;
   L29 = ATTR(in_self,indent);
   L301_=INTPLUS(L29,1); 
   SATTR(in_self,indent,L301_);
   aget_self3 = ATTR(arg,stmts);
   aget_ind3 = 0;
   L321_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self3,aget_ind3); 
   ret_val8 = L321_;
   CGEN_e494460490(self, ret_val8);
   move_out_self = self;
   L33 = ATTR(move_out_self,indent);
   L341_=INTMINUS(L33,1); 
   SATTR(move_out_self,indent,L341_);
   ndefer_self4 = self;
   plus_self5 = ((STR) &locked_lock);
   plus_sarg5 = this_lock_str;
   ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val9;
   plus_sarg6 = ((STR) &name16);
   ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
   ndefer_s4 = ret_val10;
   CGEN_d695831906(ndefer_self4);
   SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
   ndefer_self5 = self;
   plus_self7 = CGEN_f280967020(self, ((STR) &access1838956836), arg);
   plus_sarg7 = ((STR) &name102);
   ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
   ndefer_s5 = ret_val11;
   CGEN_d695831906(ndefer_self5);
   SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
   SATTR(self,manual_unlock,FALSE);
   SATTR(self,manual_loop_unlo,FALSE);
  }
  else {
   ndefer_self6 = self;
   plus_self8 = ((STR) &if3);
   plus_sarg8 = CGEN_f280967020(self, ((STR) &access955627674), arg);
   ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val12;
   plus_sarg9 = ((STR) &name26);
   ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
   ndefer_s6 = ret_val13;
   CGEN_d695831906(ndefer_self6);
   SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
   ndefer_self7 = self;
   plus_self10 = ((STR) &voidpp1939092441);
   plus_sarg10 = this_lock_str;
   ret_val14 = STR_ap2004550586(plus_self10, plus_sarg10);
   plus_self11 = ret_val14;
   plus_sarg11 = ((STR) &name16);
   ret_val15 = STR_ap2004550586(plus_self11, plus_sarg11);
   ndefer_s7 = ret_val15;
   CGEN_d695831906(ndefer_self7);
   SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
   in_self1 = self;
   L35 = ATTR(in_self1,indent);
   L361_=INTPLUS(L35,1); 
   SATTR(in_self1,indent,L361_);
   aget_self4 = ATTR(arg,stmts);
   aget_ind4 = 0;
   L371_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self4,aget_ind4); 
   ret_val16 = L371_;
   CGEN_e494460490(self, ret_val16);
   move_out_self1 = self;
   L38 = ATTR(move_out_self1,indent);
   L391_=INTMINUS(L38,1); 
   SATTR(move_out_self1,indent,L391_);
   ndefer_self8 = self;
   plus_self12 = ((STR) &locked_lock);
   plus_sarg12 = this_lock_str;
   ret_val17 = STR_ap2004550586(plus_self12, plus_sarg12);
   plus_self13 = ret_val17;
   plus_sarg13 = ((STR) &name16);
   ret_val18 = STR_ap2004550586(plus_self13, plus_sarg13);
   ndefer_s8 = ret_val18;
   CGEN_d695831906(ndefer_self8);
   SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
   ndefer_self9 = self;
   plus_self14 = CGEN_f280967020(self, ((STR) &access1838956836), arg);
   plus_sarg14 = ((STR) &name16);
   ret_val19 = STR_ap2004550586(plus_self14, plus_sarg14);
   ndefer_s9 = ret_val19;
   CGEN_d695831906(ndefer_self9);
   SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
   SATTR(self,manual_unlock,FALSE);
   SATTR(self,manual_loop_unlo,FALSE);
   ndefer_self10 = self;
   ndefer_s10 = ((STR) &else5);
   CGEN_d695831906(ndefer_self10);
   SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
   in_self2 = self;
   L40 = ATTR(in_self2,indent);
   L421_=INTPLUS(L40,1); 
   SATTR(in_self2,indent,L421_);
   CGEN_e494460490(self, ATTR(arg,else_stmts));
   move_out_self2 = self;
   L43 = ATTR(move_out_self2,indent);
   L441_=INTMINUS(L43,1); 
   SATTR(move_out_self2,indent,L441_);
   ndefer_self11 = self;
   ndefer_s11 = ((STR) &name4);
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
  }
  ndefer_self12 = self;
  ndefer_s12 = ((STR) &name4);
  CGEN_d695831906(ndefer_self12);
  SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
 }
 else {
  plus_self15 = ((STR) &DECLARE_LOCK1);
  plus_arg = branches1;
  str_self = plus_arg;
  clear_self = buf1;
  L45 = (clear_self==((FSTR) NULL));
  L461_=!(L45); 
  if (L461_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val22 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val22;
  str_self1 = buf1;
  ret_val23 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val21 = ret_val23;
  s = ret_val21;
  ret_val20 = STR_ap1077157958(plus_self15, s, TRUE);
  plus_self16 = ret_val20;
  plus_sarg15 = ((STR) &name3);
  ret_val24 = STR_ap2004550586(plus_self16, plus_sarg15);
  plus_self17 = ret_val24;
  plus_arg1 = cols;
  str_self2 = plus_arg1;
  clear_self1 = buf2;
  L47 = (clear_self1==((FSTR) NULL));
  L481_=!(L47); 
  if (L481_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self2;
  str_in_s1 = buf2;
  ret_val27 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val27;
  str_self3 = buf2;
  ret_val28 = STR_fr1097270334(((STR) NULL), str_self3);
  ret_val26 = ret_val28;
  s1 = ret_val26;
  ret_val25 = STR_ap1077157958(plus_self17, s1, TRUE);
  plus_self18 = ret_val25;
  plus_sarg16 = ((STR) &S05);
  ret_val29 = STR_ap2004550586(plus_self18, plus_sarg16);
  stmt2 = ret_val29;
  L49 = (ATTR(arg,else_stmts)==((dAM_STMT) NULL));
  L501_=!(L49); 
  if (L501_) {
   plus_self19 = stmt2;
   plus_sarg17 = ((STR) &LOCK_WITH_ELSE1);
   ret_val30 = STR_ap2004550586(plus_self19, plus_sarg17);
   stmt2 = ret_val30;
  }
  if (ATTR(arg,manual_unlock)) {
   plus_self20 = stmt2;
   plus_sarg18 = ((STR) &LOCK_NO_STACK);
   ret_val31 = STR_ap2004550586(plus_self20, plus_sarg18);
   stmt2 = ret_val31;
  }
  plus_self21 = stmt2;
  plus_sarg19 = ((STR) &name26);
  ret_val32 = STR_ap2004550586(plus_self21, plus_sarg19);
  stmt2 = ret_val32;
  ndefer_self13 = self;
  ndefer_s13 = stmt2;
  CGEN_d695831906(ndefer_self13);
  SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
  {
   BOOL f_L521_ = TRUE;
   struct FLISTA1857087356_frame_struct other2_0;
FLISTA1857087356_frame noname2 = &other2_0;
   struct FLISTd2122660729_frame_struct other2_1;
FLISTd2122660729_frame noname3 = &other2_1;
   L21 = 0;
   L31 = ATTRs(arg,locks1,((FLISTA1327600838) NULL));
   L41 = ATTRs(arg,guards,((FLISTdAM_EXPR) NULL));
   L521_=L21-1; 
   noname2->self = L31;
   noname2->state = 0;
   noname3->self = L41;
   noname3->state = 0;
   while (1) {
    L521_++; 
    c = L521_;
    L54 = FLISTA1857087356(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    l = L54;
    L55 = FLISTd2122660729(noname3);
    if (noname3->state == -1) {
     goto after_loop1;
    }
    g = L55;
    L561_=(l)==NULL?0:ASIZE((ARRAYdAM_EXPR)l); 
    L57 = L561_;
    L581_=(15)<(L57); 
    if (L581_) {
     barf_self = self;
     barf_msg = ((STR) &Youcan1429763738);
     barf_self1 = ATTR(barf_self,prog);
     barf_msg1 = barf_msg;
     barf_at_self = barf_self1;
     barf_at_msg = barf_msg1;
     barf_at_at = ((dPROG_ERR) NULL);
     PROG_e176405615(barf_at_self, barf_at_at);
     plus_self22 = ((STR) &Intern28965746);
     plus_sarg20 = barf_at_msg;
     ret_val33 = STR_ap2004550586(plus_self22, plus_sarg20);
     PROG_err_STR(barf_at_self, ret_val33);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    }
    stmt2 = ((STR) &name1);
    L59 = (g==((dAM_EXPR) NULL));
    L601_=!(L59); 
    if (L601_) {
     plus_self23 = stmt2;
     plus_sarg21 = ((STR) &if4);
     ret_val34 = STR_ap2004550586(plus_self23, plus_sarg21);
     plus_self24 = ret_val34;
     plus_sarg22 = CGEN_e1639504572(self, g);
     ret_val35 = STR_ap2004550586(plus_self24, plus_sarg22);
     plus_self25 = ret_val35;
     plus_sarg23 = ((STR) &name84);
     ret_val36 = STR_ap2004550586(plus_self25, plus_sarg23);
     stmt2 = ret_val36;
    }
    {
     BOOL f_L621_ = TRUE;
     BOOL f_L631_ = TRUE;
     L51 = 0;
     /* loop index variable */
     L91 = 0;
     L61 = l;
     L621_=L51-1; 
     L631_br=L61==NULL?0:ASIZE((ARRAYdAM_EXPR)L61); 
     while (1) {
      L621_++; 
      ln = L621_;
      plus_self26 = stmt2;
      plus_sarg24 = ((STR) &ADD_LOCK);
      ret_val37 = STR_ap2004550586(plus_self26, plus_sarg24);
      plus_self27 = ret_val37;
      plus_arg2 = c;
      str_self4 = plus_arg2;
      clear_self2 = buf3;
      L65 = (clear_self2==((FSTR) NULL));
      L661_=!(L65); 
      if (L661_) {
       SATTR(clear_self2,loc,0);
      }
      str_in_self2 = str_self4;
      str_in_s2 = buf3;
      ret_val40 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
      buf3 = ret_val40;
      str_self5 = buf3;
      ret_val41 = STR_fr1097270334(((STR) NULL), str_self5);
      ret_val39 = ret_val41;
      s2 = ret_val39;
      ret_val38 = STR_ap1077157958(plus_self27, s2, TRUE);
      plus_self28 = ret_val38;
      plus_sarg25 = ((STR) &name3);
      ret_val42 = STR_ap2004550586(plus_self28, plus_sarg25);
      plus_self29 = ret_val42;
      plus_arg3 = ln;
      str_self6 = plus_arg3;
      clear_self3 = buf4;
      L67 = (clear_self3==((FSTR) NULL));
      L681_=!(L67); 
      if (L681_) {
       SATTR(clear_self3,loc,0);
      }
      str_in_self3 = str_self6;
      str_in_s3 = buf4;
      ret_val45 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
      buf4 = ret_val45;
      str_self7 = buf4;
      ret_val46 = STR_fr1097270334(((STR) NULL), str_self7);
      ret_val44 = ret_val46;
      s3 = ret_val44;
      ret_val43 = STR_ap1077157958(plus_self29, s3, TRUE);
      plus_self30 = ret_val43;
      plus_sarg26 = ((STR) &name3);
      ret_val47 = STR_ap2004550586(plus_self30, plus_sarg26);
      plus_self31 = ret_val47;
      if(L91>=L631_br)  goto after_loop2; 
      aL631_=ARR((ARRAYdAM_EXPR)L61,L91); 
      plus_sarg27 = CGEN_e1639504572(self, aL631_);
      ret_val48 = STR_ap2004550586(plus_self31, plus_sarg27);
      plus_self32 = ret_val48;
      plus_sarg28 = ((STR) &name76);
      ret_val49 = STR_ap2004550586(plus_self32, plus_sarg28);
      stmt2 = ret_val49;
      L701_=INTPLUS(L91,1); 
      L91 = L701_;
     }
    }
    after_loop2: ;
    L72 = (g==((dAM_EXPR) NULL));
    L731_=!(L72); 
    if (L731_) {
     plus_self33 = stmt2;
     plus_sarg29 = ((STR) &name139);
     ret_val50 = STR_ap2004550586(plus_self33, plus_sarg29);
     stmt2 = ret_val50;
    }
    ndefer_self14 = self;
    ndefer_s14 = stmt2;
    CGEN_d695831906(ndefer_self14);
    SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
   }
  }
  after_loop1: ;
  L74 = ATTR(self,manual_unlock);
  L751_=!(L74); 
  if (L751_) {
   L76 = ATTR(self,ex_nesting);
   L771_=INTPLUS(L76,1); 
   SATTR(self,ex_nesting,L771_);
  }
  L781_=(branches1)==(1); 
  if (L781_) {
   L791_=(cols)==(1); 
   if (L791_) {
    if ((ATTR(arg,else_stmts)==((dAM_STMT) NULL))) {
     ndefer_self15 = self;
     ndefer_s15 = ((STR) &LM_ACQ898286306);
     CGEN_d695831906(ndefer_self15);
     SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
     L80 = ATTR(self,manual_unlock);
     L821_=!(L80); 
     if (L821_) {
      ndefer_self16 = self;
      ndefer_s16 = ((STR) &LM_PUS2132113989);
      CGEN_d695831906(ndefer_self16);
      SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
     }
     in_self3 = self;
     L83 = ATTR(in_self3,indent);
     L841_=INTPLUS(L83,1); 
     SATTR(in_self3,indent,L841_);
     aget_self5 = ATTR(arg,stmts);
     aget_ind5 = 0;
     L851_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self5,aget_ind5); 
     ret_val51 = L851_;
     CGEN_e494460490(self, ret_val51);
     move_out_self3 = self;
     L86 = ATTR(move_out_self3,indent);
     L871_=INTMINUS(L86,1); 
     SATTR(move_out_self3,indent,L871_);
     L88 = ATTR(self,manual_unlock);
     L891_=!(L88); 
     if (L891_) {
      L90 = ATTR(self,ex_nesting);
      L921_=INTMINUS(L90,1); 
      SATTR(self,ex_nesting,L921_);
     }
     ndefer_self17 = self;
     ndefer_s17 = ((STR) &LM_REL804480895);
     CGEN_d695831906(ndefer_self17);
     SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
     L93 = ATTR(self,manual_unlock);
     L941_=!(L93); 
     if (L941_) {
      ndefer_self18 = self;
      ndefer_s18 = ((STR) &LM_POP1734626182);
      CGEN_d695831906(ndefer_self18);
      SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
     }
     SATTR(self,manual_unlock,FALSE);
     SATTR(self,manual_loop_unlo,FALSE);
    }
    else {
     ndefer_self19 = self;
     ndefer_s19 = ((STR) &LM_TRY563339091);
     CGEN_d695831906(ndefer_self19);
     SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
     L95 = ATTR(self,manual_unlock);
     L961_=!(L95); 
     if (L961_) {
      ndefer_self20 = self;
      ndefer_s20 = ((STR) &LM_PUS2132113989);
      CGEN_d695831906(ndefer_self20);
      SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
     }
     in_self4 = self;
     L97 = ATTR(in_self4,indent);
     L981_=INTPLUS(L97,1); 
     SATTR(in_self4,indent,L981_);
     aget_self6 = ATTR(arg,stmts);
     aget_ind6 = 0;
     L991_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self6,aget_ind6); 
     ret_val52 = L991_;
     CGEN_e494460490(self, ret_val52);
     move_out_self4 = self;
     L100 = ATTR(move_out_self4,indent);
     L1011_=INTMINUS(L100,1); 
     SATTR(move_out_self4,indent,L1011_);
     L102 = ATTR(self,manual_unlock);
     L1031_=!(L102); 
     if (L1031_) {
      L104 = ATTR(self,ex_nesting);
      L1051_=INTMINUS(L104,1); 
      SATTR(self,ex_nesting,L1051_);
     }
     ndefer_self21 = self;
     ndefer_s21 = ((STR) &LM_REL804480895);
     CGEN_d695831906(ndefer_self21);
     SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
     L106 = ATTR(self,manual_unlock);
     L1071_=!(L106); 
     if (L1071_) {
      ndefer_self22 = self;
      ndefer_s22 = ((STR) &LM_POP1734626182);
      CGEN_d695831906(ndefer_self22);
      SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
     }
     SATTR(self,manual_unlock,FALSE);
     SATTR(self,manual_loop_unlo,FALSE);
     ndefer_self23 = self;
     ndefer_s23 = ((STR) &else5);
     CGEN_d695831906(ndefer_self23);
     SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
     in_self5 = self;
     L108 = ATTR(in_self5,indent);
     L1091_=INTPLUS(L108,1); 
     SATTR(in_self5,indent,L1091_);
     CGEN_e494460490(self, ATTR(arg,else_stmts));
     move_out_self5 = self;
     L110 = ATTR(move_out_self5,indent);
     L1111_=INTMINUS(L110,1); 
     SATTR(move_out_self5,indent,L1111_);
     ndefer_self24 = self;
     ndefer_s24 = ((STR) &name4);
     CGEN_d695831906(ndefer_self24);
     SATTR(ndefer_self24,routine_code,FSTR_p1752847026(ATTR(ndefer_self24,routine_code), ndefer_s24));
    }
   }
   else {
    if ((ATTR(arg,else_stmts)==((dAM_STMT) NULL))) {
     ndefer_self25 = self;
     ndefer_s25 = ((STR) &LM_ACQ1821457010);
     CGEN_d695831906(ndefer_self25);
     SATTR(ndefer_self25,routine_code,FSTR_p1752847026(ATTR(ndefer_self25,routine_code), ndefer_s25));
     L112 = ATTR(self,manual_unlock);
     L1131_=!(L112); 
     if (L1131_) {
      ndefer_self26 = self;
      ndefer_s26 = ((STR) &LM_PUS2132113989);
      CGEN_d695831906(ndefer_self26);
      SATTR(ndefer_self26,routine_code,FSTR_p1752847026(ATTR(ndefer_self26,routine_code), ndefer_s26));
     }
     in_self6 = self;
     L114 = ATTR(in_self6,indent);
     L1151_=INTPLUS(L114,1); 
     SATTR(in_self6,indent,L1151_);
     aget_self7 = ATTR(arg,stmts);
     aget_ind7 = 0;
     L1161_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self7,aget_ind7); 
     ret_val53 = L1161_;
     CGEN_e494460490(self, ret_val53);
     move_out_self6 = self;
     L117 = ATTR(move_out_self6,indent);
     L1181_=INTMINUS(L117,1); 
     SATTR(move_out_self6,indent,L1181_);
     L119 = ATTR(self,manual_unlock);
     L1201_=!(L119); 
     if (L1201_) {
      L121 = ATTR(self,ex_nesting);
      L1221_=INTMINUS(L121,1); 
      SATTR(self,ex_nesting,L1221_);
     }
     ndefer_self27 = self;
     ndefer_s27 = ((STR) &LM_REL1765693927);
     CGEN_d695831906(ndefer_self27);
     SATTR(ndefer_self27,routine_code,FSTR_p1752847026(ATTR(ndefer_self27,routine_code), ndefer_s27));
     L123 = ATTR(self,manual_unlock);
     L1241_=!(L123); 
     if (L1241_) {
      ndefer_self28 = self;
      ndefer_s28 = ((STR) &LM_POP1734626182);
      CGEN_d695831906(ndefer_self28);
      SATTR(ndefer_self28,routine_code,FSTR_p1752847026(ATTR(ndefer_self28,routine_code), ndefer_s28));
     }
     SATTR(self,manual_unlock,FALSE);
     SATTR(self,manual_loop_unlo,FALSE);
    }
    else {
     ndefer_self29 = self;
     ndefer_s29 = ((STR) &LM_TRY1921320459);
     CGEN_d695831906(ndefer_self29);
     SATTR(ndefer_self29,routine_code,FSTR_p1752847026(ATTR(ndefer_self29,routine_code), ndefer_s29));
     L125 = ATTR(self,manual_unlock);
     L1261_=!(L125); 
     if (L1261_) {
      ndefer_self30 = self;
      ndefer_s30 = ((STR) &LM_PUS2132113989);
      CGEN_d695831906(ndefer_self30);
      SATTR(ndefer_self30,routine_code,FSTR_p1752847026(ATTR(ndefer_self30,routine_code), ndefer_s30));
     }
     in_self7 = self;
     L127 = ATTR(in_self7,indent);
     L1281_=INTPLUS(L127,1); 
     SATTR(in_self7,indent,L1281_);
     aget_self8 = ATTR(arg,stmts);
     aget_ind8 = 0;
     L1291_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self8,aget_ind8); 
     ret_val54 = L1291_;
     CGEN_e494460490(self, ret_val54);
     move_out_self7 = self;
     L130 = ATTR(move_out_self7,indent);
     L1311_=INTMINUS(L130,1); 
     SATTR(move_out_self7,indent,L1311_);
     L132 = ATTR(self,manual_unlock);
     L1331_=!(L132); 
     if (L1331_) {
      L134 = ATTR(self,ex_nesting);
      L1351_=INTMINUS(L134,1); 
      SATTR(self,ex_nesting,L1351_);
     }
     ndefer_self31 = self;
     ndefer_s31 = ((STR) &LM_REL1765693927);
     CGEN_d695831906(ndefer_self31);
     SATTR(ndefer_self31,routine_code,FSTR_p1752847026(ATTR(ndefer_self31,routine_code), ndefer_s31));
     L136 = ATTR(self,manual_unlock);
     L1371_=!(L136); 
     if (L1371_) {
      ndefer_self32 = self;
      ndefer_s32 = ((STR) &LM_POP1734626182);
      CGEN_d695831906(ndefer_self32);
      SATTR(ndefer_self32,routine_code,FSTR_p1752847026(ATTR(ndefer_self32,routine_code), ndefer_s32));
     }
     SATTR(self,manual_unlock,FALSE);
     SATTR(self,manual_loop_unlo,FALSE);
     ndefer_self33 = self;
     ndefer_s33 = ((STR) &else5);
     CGEN_d695831906(ndefer_self33);
     SATTR(ndefer_self33,routine_code,FSTR_p1752847026(ATTR(ndefer_self33,routine_code), ndefer_s33));
     in_self8 = self;
     L138 = ATTR(in_self8,indent);
     L1391_=INTPLUS(L138,1); 
     SATTR(in_self8,indent,L1391_);
     CGEN_e494460490(self, ATTR(arg,else_stmts));
     move_out_self8 = self;
     L140 = ATTR(move_out_self8,indent);
     L1421_=INTMINUS(L140,1); 
     SATTR(move_out_self8,indent,L1421_);
     ndefer_self34 = self;
     ndefer_s34 = ((STR) &name4);
     CGEN_d695831906(ndefer_self34);
     SATTR(ndefer_self34,routine_code,FSTR_p1752847026(ATTR(ndefer_self34,routine_code), ndefer_s34));
    }
   }
  }
  else {
   ndefer_self35 = self;
   ndefer_s35 = ((STR) &LM_SEL917075861);
   CGEN_d695831906(ndefer_self35);
   SATTR(ndefer_self35,routine_code,FSTR_p1752847026(ATTR(ndefer_self35,routine_code), ndefer_s35));
   {
    struct FLISTd1374595573_frame_struct other4_0;
FLISTd1374595573_frame noname4 = &other4_0;
    BOOL f_L1431_ = TRUE;
    L71 = ATTR(arg,stmts);
    L81 = 0;
    noname4->self = L71;
    noname4->state = 0;
    L1431_=L81-1; 
    while (1) {
     L144 = FLISTd1374595573(noname4);
     if (noname4->state == -1) {
      goto after_loop3;
     }
     stt = L144;
     ndefer_self36 = self;
     plus_self34 = ((STR) &LM_LOCK_CLAUSE);
     L1431_++; 
     plus_arg4 = L1431_;
     str_self8 = plus_arg4;
     clear_self4 = buf5;
     L146 = (clear_self4==((FSTR) NULL));
     L1471_=!(L146); 
     if (L1471_) {
      SATTR(clear_self4,loc,0);
     }
     str_in_self4 = str_self8;
     str_in_s4 = buf5;
     ret_val57 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
     buf5 = ret_val57;
     str_self9 = buf5;
     ret_val58 = STR_fr1097270334(((STR) NULL), str_self9);
     ret_val56 = ret_val58;
     s4 = ret_val56;
     ret_val55 = STR_ap1077157958(plus_self34, s4, TRUE);
     plus_self35 = ret_val55;
     plus_sarg30 = ((STR) &name26);
     ret_val59 = STR_ap2004550586(plus_self35, plus_sarg30);
     ndefer_s36 = ret_val59;
     CGEN_d695831906(ndefer_self36);
     SATTR(ndefer_self36,routine_code,FSTR_p1752847026(ATTR(ndefer_self36,routine_code), ndefer_s36));
     L148 = ATTR(self,manual_unlock);
     L1491_=!(L148); 
     if (L1491_) {
      ndefer_self37 = self;
      ndefer_s37 = ((STR) &LM_PUS2132113989);
      CGEN_d695831906(ndefer_self37);
      SATTR(ndefer_self37,routine_code,FSTR_p1752847026(ATTR(ndefer_self37,routine_code), ndefer_s37));
     }
     in_self9 = self;
     L150 = ATTR(in_self9,indent);
     L1511_=INTPLUS(L150,1); 
     SATTR(in_self9,indent,L1511_);
     CGEN_e494460490(self, stt);
     move_out_self9 = self;
     L152 = ATTR(move_out_self9,indent);
     L1531_=INTMINUS(L152,1); 
     SATTR(move_out_self9,indent,L1531_);
     ndefer_self38 = self;
     ndefer_s38 = ((STR) &LM_REL1415431597);
     CGEN_d695831906(ndefer_self38);
     SATTR(ndefer_self38,routine_code,FSTR_p1752847026(ATTR(ndefer_self38,routine_code), ndefer_s38));
     L154 = ATTR(self,manual_unlock);
     L1551_=!(L154); 
     if (L1551_) {
      ndefer_self39 = self;
      ndefer_s39 = ((STR) &LM_POP1734626182);
      CGEN_d695831906(ndefer_self39);
      SATTR(ndefer_self39,routine_code,FSTR_p1752847026(ATTR(ndefer_self39,routine_code), ndefer_s39));
     }
    }
   }
   after_loop3: ;
   L156 = ATTR(self,manual_unlock);
   L1571_=!(L156); 
   if (L1571_) {
    L158 = ATTR(self,ex_nesting);
    L1591_=INTMINUS(L158,1); 
    SATTR(self,ex_nesting,L1591_);
   }
   SATTR(self,manual_unlock,FALSE);
   SATTR(self,manual_loop_unlo,FALSE);
   L160 = (ATTR(arg,else_stmts)==((dAM_STMT) NULL));
   L1621_=!(L160); 
   if (L1621_) {
    ndefer_self40 = self;
    ndefer_s40 = ((STR) &LM_ELSE_CLAUSE);
    CGEN_d695831906(ndefer_self40);
    SATTR(ndefer_self40,routine_code,FSTR_p1752847026(ATTR(ndefer_self40,routine_code), ndefer_s40));
    in_self10 = self;
    L163 = ATTR(in_self10,indent);
    L1641_=INTPLUS(L163,1); 
    SATTR(in_self10,indent,L1641_);
    CGEN_e494460490(self, ATTR(arg,else_stmts));
    move_out_self10 = self;
    L165 = ATTR(move_out_self10,indent);
    L1661_=INTMINUS(L165,1); 
    SATTR(move_out_self10,indent,L1661_);
   }
   ndefer_self41 = self;
   ndefer_s41 = ((STR) &LM_END629067339);
   CGEN_d695831906(ndefer_self41);
   SATTR(ndefer_self41,routine_code,FSTR_p1752847026(ATTR(ndefer_self41,routine_code), ndefer_s41));
  }
  ndefer_self42 = self;
  ndefer_s42 = ((STR) &LM_CLO1780161327);
  CGEN_d695831906(ndefer_self42);
  SATTR(ndefer_self42,routine_code,FSTR_p1752847026(ATTR(ndefer_self42,routine_code), ndefer_s42));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1669826382(CGEN self, AM_EXPR_STMT arg) {
 STR s1;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val1;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val2;
 STR r;
 CGEN ndefer_self1;
 STR ndefer_s1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 extern STR void9;
 extern STR name16;
 STR L8;
 INT L9;
 OB L10;
 INT L11;
 L1 = (ATTR(arg,expr)==((dAM_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  s1 = CGEN_e1639504572(self, ATTR(arg,expr));
  L3 = (s1==((STR) NULL));
  L41_=!(L3); 
  if (L41_) {
   L5 = ATTR(arg,expr);
   L6 = ((*dAM_EXPR_tprdTP[TAG(L5)])(L5)==((dTP) NULL));
   L71_=!(L6); 
   if (L71_) {
    ndefer_self = self;
    ndefer_s = STR_ap1693864410(((STR) &void9), s1, ((STR) &name16));
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   else {
    ndefer_self1 = self;
    plus_self = s1;
    plus_arg = ';';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L9 = 1;
    L11=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
    L10=ZALLOC_LEAF_BIG(L11);
    if (L10==NULL) FATAL("Unable to allocate more memory");
    memset(L10,0,L11);
    ((OB)L10)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L10)->header.destroyed=0;
#endif

    L8 = ((STR) L10);
    L8->asize = L9;
    r = L8;
    SARR((STR)r,0,create_c); 
    ;
    ret_val2 = r;
    ret_val1 = ret_val2;
    s = ret_val1;
    ret_val = STR_ap1077157958(plus_self, s, TRUE);
    ndefer_s1 = ret_val;
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1809841360(CGEN self, AM_ATTACH_STMT arg) {
 STR stmt1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
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
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val10;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val11;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val12;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val13;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val14;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val15;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val16;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val17;
 CGEN ndefer_self;
 STR ndefer_s;
 extern STR name1;
 extern STR PARLOOP_;
 BOOL L1;
 extern STR PARLOOP_FORK_;
 BOOL L2;
 extern STR FORK_;
 extern STR ATTACH1;
 extern STR name29;
 extern STR self1;
 extern STR name29;
 extern STR name29;
 BOOL L3;
 BOOL L41_;
 extern STR name29;
 extern STR SI_HEREcluster;
 extern STR HERE2;
 extern STR name76;
 extern STR forked;
 extern STR attached;
 extern STR par2;
 CODE_F1291349628(ATTR(self,code_c), ATTR(arg,rout));
 stmt1 = ((STR) &name1);
 if (ATTR(ATTR(arg,rout),opt_fork)) {
  if (ATTR(ATTR(arg,rout),is_par_routine)) {
   stmt1 = ((STR) &PARLOOP_);
   SATTR(self,ignore_lock,TRUE);
  }
  else {
   if (ATTR(ATTR(arg,rout),is_fork_routine)) {
    L1 = ATTR(ATTR(self,current_sig),opt_fork);
   } else {
    L1 = FALSE;
   }
   if (L1) {
    stmt1 = ((STR) &PARLOOP_FORK_);
   }
  }
 }
 else {
  if (ATTR(ATTR(arg,rout),is_fork_routine)) {
   L2 = TRUE;
  } else {
   L2 = ATTR(ATTR(arg,rout),is_par_routine);
  }
  if (L2) {
   stmt1 = ((STR) &FORK_);
  }
 }
 plus_self = stmt1;
 plus_sarg = ((STR) &ATTACH1);
 ret_val = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val;
 mang_self = self;
 mang_ob = ((OB) ATTR(arg,rout));
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_sarg1 = ret_val1;
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &name29);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 cast_self = self;
 cast_dest_tp = ((dTP) TP_BUI1325635093);
 cast_src_tp = ATTR(ATTR(arg,rout),tp);
 cast_expr = ((STR) &self1);
 cast_only_boxing = FALSE;
 ret_val4 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
 plus_sarg3 = ret_val4;
 ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val5;
 plus_sarg4 = ((STR) &name29);
 ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
 plus_self5 = ret_val6;
 plus_sarg5 = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(arg,helper)));
 ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
 plus_self6 = ret_val7;
 plus_sarg6 = ((STR) &name29);
 ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
 plus_self7 = ret_val8;
 plus_sarg7 = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(arg,gate)));
 ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
 stmt1 = ret_val9;
 L3 = (ATTR(arg,at)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  plus_self8 = stmt1;
  plus_sarg8 = ((STR) &name29);
  ret_val10 = STR_ap2004550586(plus_self8, plus_sarg8);
  plus_self9 = ret_val10;
  plus_sarg9 = CGEN_e1639504572(self, ATTR(arg,at));
  ret_val11 = STR_ap2004550586(plus_self9, plus_sarg9);
  stmt1 = ret_val11;
 }
 else {
  if (ATTR(ATTR(self,prog),zones)) {
   plus_self10 = stmt1;
   plus_sarg10 = ((STR) &SI_HEREcluster);
   ret_val12 = STR_ap2004550586(plus_self10, plus_sarg10);
   stmt1 = ret_val12;
  }
  else {
   plus_self11 = stmt1;
   plus_sarg11 = ((STR) &HERE2);
   ret_val13 = STR_ap2004550586(plus_self11, plus_sarg11);
   stmt1 = ret_val13;
  }
 }
 plus_self12 = stmt1;
 plus_sarg12 = ((STR) &name76);
 ret_val14 = STR_ap2004550586(plus_self12, plus_sarg12);
 stmt1 = ret_val14;
 if (ATTR(ATTR(arg,rout),is_fork_routine)) {
  plus_self13 = stmt1;
  plus_sarg13 = ((STR) &forked);
  ret_val15 = STR_ap2004550586(plus_self13, plus_sarg13);
  stmt1 = ret_val15;
 }
 if (ATTR(ATTR(arg,rout),is_attach_routin)) {
  plus_self14 = stmt1;
  plus_sarg14 = ((STR) &attached);
  ret_val16 = STR_ap2004550586(plus_self14, plus_sarg14);
  stmt1 = ret_val16;
 }
 if (ATTR(ATTR(arg,rout),is_par_routine)) {
  plus_self15 = stmt1;
  plus_sarg15 = ((STR) &par2);
  ret_val17 = STR_ap2004550586(plus_self15, plus_sarg15);
  stmt1 = ret_val17;
 }
 ndefer_self = self;
 ndefer_s = stmt1;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e1959168490(CGEN self, AM_FORK_STMT arg) {
 dAM_STMT s = ((dAM_STMT) NULL);
 dAM_EXPR e;
 STR dest1 = ((STR) NULL);
 STR handler1;
 ARRAYSTR arg_list;
 INT num_args = INT_zero;
 STR fork_str;
 STR arg_str = ((STR) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG sa1;
 STR decl;
 ARRAYSTR L31;
 STR arg_str1;
 AM_ROU1916046290 L41;
 ARRAYSTR L51;
 STR L61;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val5;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L71 = INT_zero;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 CODE_FILE plus_self5;
 dSTR plus_s;
 CODE_FILE plus_self6;
 dSTR plus_s1;
 CODE_FILE plus_self7;
 dSTR plus_s2;
 CODE_FILE ret_val11;
 CODE_FILE plus_self8;
 dSTR plus_s3;
 CODE_FILE plus_self9;
 dSTR plus_s4;
 CODE_FILE plus_self10;
 dSTR plus_s5;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val12;
 CODE_FILE plus_self11;
 dSTR plus_s6;
 INT L81 = INT_zero;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val13;
 CODE_FILE plus_self12;
 dSTR plus_s7;
 CODE_FILE plus_self13;
 dSTR plus_s8;
 CODE_FILE plus_self14;
 dSTR plus_s9;
 CODE_FILE plus_self15;
 dSTR plus_s10;
 CODE_FILE plus_self16;
 dSTR plus_s11;
 OUT create_self;
 OUT ret_val14;
 OUT plus_self17;
 STR plus_s12;
 FILE1 stdout_self;
 FILE1 ret_val15;
 FILE1 r;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 OUT create_self1;
 OUT ret_val16;
 OUT plus_self18;
 STR plus_s13;
 FILE1 stdout_self1;
 FILE1 ret_val17;
 FILE1 r1;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 BOOL L9;
 BOOL L101_;
 extern STR BR_HERE1;
 extern STR PAR_INCREMENT;
 extern STR name76;
 INT L121_;
 INT L13;
 INT L141_;
 extern STR BR_FORK_;
 BOOL L15;
 BOOL L161_;
 extern STR name24;
 extern STR name3;
 extern STR name3;
 extern STR BR_word_t4;
 extern STR name3;
 dTP L17;
 dTP L18;
 BOOL L19;
 BOOL L201_;
 extern STR nonato57333472;
 extern STR Intern28965746;
 extern STR BR_word_t3;
 STR L221_;
 extern STR BR_word_tSENDFOB1;
 STR L231_;
 STR L24;
 extern STR name3;
 extern STR name26;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L251_,L251_m;
 ARRAYARG L30;
 INT L321_;
 INT L33;
 ARG L341_;
 dTP L35;
 dTP L36;
 BOOL L37;
 BOOL L381_;
 extern STR nonato2046226648;
 extern STR Intern28965746;
 extern STR BR_word_t3;
 STR L391_;
 extern STR BR_word_t3;
 extern STR SENDFOB1;
 STR L401_;
 STR L42;
 extern STR name3;
 extern STR name26;
 extern STR name3;
 extern STR name76;
 extern STR void6;
 extern STR BR_clu1677588829;
 INT L431_br;
 STR aL431_;
 STR L44;
 extern STR BR_word_t1;
 extern STR arg_;
 INT L451_;
 extern STR name26;
 extern STR extern1;
 extern STR name13;
 FSTR L46;
 dSTR L47;
 extern STR Forkhandler;
 FSTR L48;
 dSTR L49;
 BOOL L50;
 BOOL L521_;
 FSTR L53;
 dSTR L54;
 extern STR name98;
 FSTR L55;
 dSTR L56;
 extern STR struct88687480;
 FSTR L57;
 dSTR L58;
 extern STR INIT_T802300136;
 FSTR L59;
 dSTR L60;
 extern STR name10;
 extern STR name24;
 FSTR L62;
 dSTR L63;
 extern STR name3;
 INT L641_br;
 AM_CALL_ARG aL641_;
 INT L651_br;
 STR aL651_;
 STR L66;
 extern STR name24;
 AM_CALL_ARG L67;
 AM_CALL_ARG L68;
 dAM_EXPR L69;
 STR L70;
 extern STR name26;
 extern STR arg_;
 STR L72;
 FSTR L73;
 dSTR L74;
 INT L751_;
 extern STR FF_NULL;
 FSTR L76;
 dSTR L77;
 extern STR name127;
 FSTR L78;
 dSTR L79;
 extern STR PAR_DE1693558485;
 FSTR L80;
 dSTR L82;
 extern STR name32;
 FSTR L83;
 dSTR L84;
 extern STR Errore497477243;
 FILE1 L85;
 OB L86;
 extern STR ErrorS66315690;
 FILE1 L88;
 OB L89;
 s = ATTR(arg,body);
 if (s==NULL) {
  goto other382;
 } else
 switch (TAG(s)) {
  case AM_EXPR_STMT_tag:
   e = ATTR(((AM_EXPR_STMT) s),expr);
   if (e==NULL) {
    goto other383;
   } else
   switch (TAG(e)) {
    case AM_ROU1916046290_tag:
     L9 = (ATTR(arg,at)==((dAM_EXPR) NULL));
     L101_=!(L9); 
     if (L101_) {
      dest1 = CGEN_e1639504572(self, ATTR(arg,at));
     }
     else {
      dest1 = ((STR) &BR_HERE1);
     }
     ndefer_self = self;
     mang_self = self;
     mang_ob = ((OB) ATTR(arg,par_stmt));
     ret_val = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
     ndefer_s = STR_ap1693864410(((STR) &PAR_INCREMENT), ret_val, ((STR) &name76));
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     mang_self1 = self;
     mang_ob1 = ((OB) arg);
     ret_val1 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
     handler1 = ret_val1;
     arg_list = CGEN_e1933193154(self, ((dAM_CALL_EXPR) e));
     L121_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
     L13 = L121_;
     L141_=INTPLUS(L13,1); 
     num_args = L141_;
     str_self = num_args;
     clear_self = buf1;
     L15 = (clear_self==((FSTR) NULL));
     L161_=!(L15); 
     if (L161_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val3;
     str_self1 = buf1;
     ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val2 = ret_val4;
     fork_str = STR_ap1182453786(((STR) &BR_FORK_), ret_val2, ((STR) &name24), dest1, ((STR) &name3), handler1, ((STR) &name3));
     mang_self2 = self;
     mang_ob2 = ((OB) ATTR(arg,par_stmt));
     ret_val5 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
     fork_str = STR_ap1417971546(fork_str, ((STR) &BR_word_t4), ret_val5, ((STR) &name3));
     L17 = ATTR(ATTR(((AM_ROU1916046290) e),fun),tp);
     if ((*dTP_is1334578382[TAG(L17)])(L17)) {
      L18 = ATTR(ATTR(((AM_ROU1916046290) e),fun),tp);
      L19 = (*dTP_is418055720[TAG(L18)])(L18);
      L201_=!(L19); 
      if (L201_) {
       barf_self = self;
       barf_msg = ((STR) &nonato57333472);
       barf_self1 = ATTR(barf_self,prog);
       barf_msg1 = barf_msg;
       barf_at_self = barf_self1;
       barf_at_msg = barf_msg1;
       barf_at_at = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self, barf_at_at);
       plus_self = ((STR) &Intern28965746);
       plus_sarg = barf_at_msg;
       ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
       PROG_err_STR(barf_at_self, ret_val6);
       exit_self = ((UNIX) NULL);
       exit_code = 1;
       exit(exit_code);
      }
      L221_=(STR)ARR((ARRAYSTR)arg_list,0); 
      fork_str = STR_ap1693864410(fork_str, ((STR) &BR_word_t3), L221_);
     }
     else {
      L231_=(STR)ARR((ARRAYSTR)arg_list,0); 
      L24 = L231_;
      fork_str = STR_ap1925291910(fork_str, ((STR) &BR_word_tSENDFOB1), L24, ((STR) &name3), dest1, ((STR) &name26));
     }
     {
      BOOL f_L251_ = TRUE;
      while (1) {
       if (f_L251_) {
        f_L251_ = FALSE;
        L11 = 1;
        L271_=ASIZE((AM_ROU1916046290)e); 
        L28 = L271_;
        L291_=INTMINUS(L28,1); 
        L21 = L291_;
        L251_=L11-1;L251_m=L21; 
       }
       if(L251_++>=L251_m)  goto after_loop; 
       i = L251_;
       L30 = ATTR(ATTR(((AM_ROU1916046290) e),fun),args);
       L321_=INTMINUS(i,1); 
       L33 = L321_;
       L341_=(ARG)ARR((ARRAYARG)L30,L33); 
       sa1 = L341_;
       L35 = ATTR(sa1,tp);
       if ((*dTP_is1334578382[TAG(L35)])(L35)) {
        L36 = ATTR(sa1,tp);
        L37 = (*dTP_is418055720[TAG(L36)])(L36);
        L381_=!(L37); 
        if (L381_) {
         barf_self2 = self;
         barf_msg2 = ((STR) &nonato2046226648);
         barf_self3 = ATTR(barf_self2,prog);
         barf_msg3 = barf_msg2;
         barf_at_self1 = barf_self3;
         barf_at_msg1 = barf_msg3;
         barf_at_at1 = ((dPROG_ERR) NULL);
         PROG_e176405615(barf_at_self1, barf_at_at1);
         plus_self1 = ((STR) &Intern28965746);
         plus_sarg1 = barf_at_msg1;
         ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
         PROG_err_STR(barf_at_self1, ret_val7);
         exit_self1 = ((UNIX) NULL);
         exit_code1 = 1;
         exit(exit_code1);
        }
        plus_self2 = ((STR) &BR_word_t3);
        L391_=(STR)ARR((ARRAYSTR)arg_list,i); 
        plus_sarg2 = L391_;
        ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
        arg_str = ret_val8;
       }
       else {
        L401_=(STR)ARR((ARRAYSTR)arg_list,i); 
        L42 = L401_;
        arg_str = STR_ap1925291910(((STR) &BR_word_t3), ((STR) &SENDFOB1), L42, ((STR) &name3), dest1, ((STR) &name26));
       }
       fork_str = STR_ap1693864410(fork_str, ((STR) &name3), arg_str);
      }
     }
     after_loop: ;
     plus_self3 = fork_str;
     plus_sarg3 = ((STR) &name76);
     ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
     fork_str = ret_val9;
     ndefer_self1 = self;
     ndefer_s1 = fork_str;
     CGEN_d695831906(ndefer_self1);
     SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
     decl = STR_ap1693864410(((STR) &void6), handler1, ((STR) &BR_clu1677588829));
     {
      BOOL f_L431_ = TRUE;
      /* loop index variable */
      L71 = 0;
      L31 = arg_list;
      L431_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
      while (1) {
       if(L71>=L431_br)  goto after_loop1; 
       aL431_=ARR((ARRAYSTR)L31,L71); 
       arg_str1 = aL431_;
       decl = STR_ap1417971546(decl, ((STR) &BR_word_t1), ((STR) &arg_), arg_str1);
       L451_=INTPLUS(L71,1); 
       L71 = L451_;
      }
     }
     after_loop1: ;
     plus_self4 = decl;
     plus_sarg4 = ((STR) &name26);
     ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
     decl = ret_val10;
     plus_self5 = ATTR(self,code_c);
     plus_s = ((dSTR) STR_ap1693864410(((STR) &extern1), decl, ((STR) &name13)));
     L46 = ATTR(plus_self5,ntext);
     L47 = plus_s;
     SATTR(plus_self5,ntext,FSTR_p1752847026(L46, (*dSTR_strrSTR[TAG(L47)])(L47)));
     plus_self6 = ATTR(self,unbox_c);
     plus_s1 = ((dSTR) ((STR) &Forkhandler));
     L48 = ATTR(plus_self6,ntext);
     L49 = plus_s1;
     SATTR(plus_self6,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
     plus_self7 = ATTR(self,unbox_c);
     plus_s2 = ((dSTR) decl);
     L50 = (plus_s2==((dSTR) NULL));
     L521_=!(L50); 
     if (L521_) {
      L53 = ATTR(plus_self7,ntext);
      L54 = plus_s2;
      SATTR(plus_self7,ntext,FSTR_p1752847026(L53, (*dSTR_strrSTR[TAG(L54)])(L54)));
     }
     ret_val11 = plus_self7;
     plus_self8 = ret_val11;
     plus_s3 = ((dSTR) ((STR) &name98));
     L55 = ATTR(plus_self8,ntext);
     L56 = plus_s3;
     SATTR(plus_self8,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
     if (ATTR(self,func_tables)) {
      plus_self9 = ATTR(self,unbox_c);
      plus_s4 = ((dSTR) ((STR) &struct88687480));
      L57 = ATTR(plus_self9,ntext);
      L58 = plus_s4;
      SATTR(plus_self9,ntext,FSTR_p1752847026(L57, (*dSTR_strrSTR[TAG(L58)])(L58)));
     }
     plus_self10 = ATTR(self,unbox_c);
     plus_s5 = ((dSTR) ((STR) &INIT_T802300136));
     L59 = ATTR(plus_self10,ntext);
     L60 = plus_s5;
     SATTR(plus_self10,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
     plus_self11 = ATTR(self,unbox_c);
     mang_self3 = self;
     mang_ob3 = ((OB) ATTR(((AM_ROU1916046290) e),fun));
     ret_val12 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_s6 = ((dSTR) STR_ap1693864410(((STR) &name10), ret_val12, ((STR) &name24)));
     L62 = ATTR(plus_self11,ntext);
     L63 = plus_s6;
     SATTR(plus_self11,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
     {
      BOOL f_L641_ = TRUE;
      BOOL f_L651_ = TRUE;
      struct STR_se405450305_frame_struct other3_0;
STR_se405450305_frame noname1 = &other3_0;
      /* loop index variable */
      L81 = 0;
      L41 = ((AM_ROU1916046290) e);
      L51 = arg_list;
      L61 = ((STR) &name3);
      L641_br=L41==NULL?0:ASIZE((AM_ROU1916046290)L41); 
      L651_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
      noname1->self = L61;
      noname1->state = 0;
      while (1) {
       plus_self12 = ATTR(self,unbox_c);
       mang_self4 = self;
       if(L81>=L641_br)  goto after_loop2; 
       aL641_=ARR((AM_ROU1916046290)L41,L81); 
       L68=aL641_;
       L69 = ATTR(L68,expr);
       mang_ob4 = ((OB) (*dAM_EXPR_tprdTP[TAG(L69)])(L69));
       ret_val13 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
       L70 = ret_val13;
       if(L81>=L651_br)  goto after_loop2; 
       aL651_=ARR((ARRAYSTR)L51,L81); 
       noname1->arg1 = ((dSTR) STR_ap44172742(((STR) &name24), L70, ((STR) &name26), ((STR) &arg_), aL651_));
       L66 = STR_se405450305(noname1);
       if (noname1->state == -1) {
        goto after_loop2;
       }
       plus_s7 = ((dSTR) L66);
       L73 = ATTR(plus_self12,ntext);
       L74 = plus_s7;
       SATTR(plus_self12,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
       L751_=INTPLUS(L81,1); 
       L81 = L751_;
      }
     }
     after_loop2: ;
     if (ATTR(self,func_tables)) {
      plus_self13 = ATTR(self,unbox_c);
      plus_s8 = ((dSTR) ((STR) &FF_NULL));
      L76 = ATTR(plus_self13,ntext);
      L77 = plus_s8;
      SATTR(plus_self13,ntext,FSTR_p1752847026(L76, (*dSTR_strrSTR[TAG(L77)])(L77)));
     }
     plus_self14 = ATTR(self,unbox_c);
     plus_s9 = ((dSTR) ((STR) &name127));
     L78 = ATTR(plus_self14,ntext);
     L79 = plus_s9;
     SATTR(plus_self14,ntext,FSTR_p1752847026(L78, (*dSTR_strrSTR[TAG(L79)])(L79)));
     plus_self15 = ATTR(self,unbox_c);
     plus_s10 = ((dSTR) ((STR) &PAR_DE1693558485));
     L80 = ATTR(plus_self15,ntext);
     L82 = plus_s10;
     SATTR(plus_self15,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L82)])(L82)));
     plus_self16 = ATTR(self,unbox_c);
     plus_s11 = ((dSTR) ((STR) &name32));
     L83 = ATTR(plus_self16,ntext);
     L84 = plus_s11;
     SATTR(plus_self16,ntext,FSTR_p1752847026(L83, (*dSTR_strrSTR[TAG(L84)])(L84)));
     CODE_F1291349628(ATTR(self,unbox_c), ATTR(((AM_ROU1916046290) e),fun)); break;
    default: ;
    other383: ;
     create_self = ((OUT) NULL);
     ret_val14 = create_self;
     plus_self17 = ret_val14;
     plus_s12 = ((STR) &Errore497477243);
     stdout_self = ((FILE1) NULL);
     L86=ZALLOC(sizeof(struct FILE1_struct));
     if (L86==NULL) FATAL("Unable to allocate more memory");
     ((OB)L86)->header.tag=FILE1_tag;
     L85 = ((FILE1) L86);
     r = L85;
     
     SATTR(r,fp,stdout);
     ret_val15 = r;
     FILE_plus_STR(ret_val15, plus_s12);
     exit_self2 = ((UNIX) NULL);
     exit_code2 = 1;
     exit(exit_code2);
   } break;
  default: ;
  other382: ;
   create_self1 = ((OUT) NULL);
   ret_val16 = create_self1;
   plus_self18 = ret_val16;
   plus_s13 = ((STR) &ErrorS66315690);
   stdout_self1 = ((FILE1) NULL);
   L89=ZALLOC(sizeof(struct FILE1_struct));
   if (L89==NULL) FATAL("Unable to allocate more memory");
   ((OB)L89)->header.tag=FILE1_tag;
   L88 = ((FILE1) L89);
   r1 = L88;
   
   SATTR(r1,fp,stdout);
   ret_val17 = r1;
   FILE_plus_STR(ret_val17, plus_s13);
   exit_self3 = ((UNIX) NULL);
   exit_code3 = 1;
   exit(exit_code3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e2137132086(CGEN self, dAM_EXPR test_expr, FLISTdTP tgts, FLISTdAM_STMT stmts11) {
 dTP etp;
 FSETdTP consider = ((FSETdTP) NULL);
 STR test1;
 ARRAYdTP case_arr;
 INT case_num = INT_zero;
 FLISTdTP L11;
 INT i = INT_zero;
 dTP tp;
 FSETdTP this_branch = ((FSETdTP) NULL);
 FSETdTP L21;
 ARRAYdTP L31;
 ARRAYdTP L41;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 dTP t;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self;
 ARRAYdTP create_self;
 INT create_n = INT_zero;
 ARRAYdTP ret_val5;
 FLISTdTP aget_self;
 INT aget_ind = INT_zero;
 dTP ret_val6;
 TP_GRAPH_ABS_DES des_of_self1;
 TP_CLASS des_of_tp1;
 FSETdTP ret_val7;
 INT L71 = INT_zero;
 FSETdTP create_self1;
 FSETdTP ret_val8;
 INT L81 = INT_zero;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val9;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val10;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val12;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self1;
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val13;
 CGEN defer_self;
 STR defer_s;
 CGEN move_out_self;
 CGEN ndefer_self3;
 STR ndefer_s3;
 dAM_EXPR L9;
 extern STR switchF_TAG;
 extern STR name105;
 extern STR switchTAG;
 extern STR name105;
 INT L10;
 INT L121_;
 BOOL L13;
 BOOL L141_;
 ARRAYdTP L15;
 INT L16;
 OB L17;
 INT L18;
 INT L19;
 dTP L201_;
 dTP L22;
 BOOL L231_;
 INT L241_br;
 dTP L25;
 dTP L26;
 INT L271_;
 INT L281_;
 BOOL L301_;
 INT L321_c,L321_m;
 dTP aL321_;
 dTP L33;
 extern STR case3;
 extern STR S_tag;
 extern STR name21;
 INT L341_;
 INT L35;
 INT L361_;
 dAM_STMT L371_;
 extern STR break5;
 INT L38;
 INT L391_;
 extern STR default2;
 L9 = test_expr;
 etp = (*dAM_EXPR_tprdTP[TAG(L9)])(L9);
 switch (TAG(etp)) {
  case TP_CLASS_tag:
   des_of_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
   des_of_tp = ((TP_CLASS) etp);
   ret_val = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
   consider = FSETdT217460096(ret_val); break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_typeswitch($AM_EXPR,FLIST{$TP},FLIST{$AM_STMT})\n./Back/cgen.sa:3779:14");
 }
 test1 = CGEN_e1639504572(self, test_expr);
 if (ATTR(ATTR(self,prog),distributed)) {
  ndefer_self = self;
  plus_self = ((STR) &switchF_TAG);
  plus_sarg = test1;
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name105);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s = ret_val2;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 else {
  ndefer_self1 = self;
  plus_self2 = ((STR) &switchTAG);
  plus_sarg2 = test1;
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &name105);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  ndefer_s1 = ret_val4;
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 }
 in_self = self;
 L10 = ATTR(in_self,indent);
 L121_=INTPLUS(L10,1); 
 SATTR(in_self,indent,L121_);
 L13 = (tgts==((FLISTdTP) NULL));
 L141_=!(L13); 
 if (L141_) {
  create_self = ((ARRAYdTP) NULL);
  create_n = FSETdTP_sizerINT(consider);
  L16 = create_n;
  L18=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L16)*sizeof(dTP);
  L17=ZALLOC_BIG(L18);
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

    ((OB)L17)->header.destroyed=0;
#endif

  L15 = ((ARRAYdTP) L17);
  L15->asize = L16;
  ret_val5 = L15;
  case_arr = ret_val5;
  {
   struct FLISTd1990200313_frame_struct other1_0;
FLISTd1990200313_frame noname1 = &other1_0;
   L11 = tgts;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L19 = FLISTd1990200313(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    i = L19;
    aget_self = tgts;
    aget_ind = i;
    L201_=(dTP)ARR((FLISTdTP)aget_self,aget_ind); 
    ret_val6 = L201_;
    tp = ret_val6;
    L22 = tp;
    if ((*dTP_is1811059018[TAG(L22)])(L22)) {
     switch (TAG(tp)) {
      case TP_CLASS_tag:
       des_of_self1 = ATTR(ATTR(self,prog),tp_graph_abs_des);
       des_of_tp1 = ((TP_CLASS) tp);
       ret_val7 = FMAPTP861983338(ATTR(des_of_self1,tbl), des_of_tp1);
       this_branch = FSETdT653600961(ret_val7, consider); break;
      default: ;
       FATAL("No applicable type in typecase\nin CGEN::emit_typeswitch($AM_EXPR,FLIST{$TP},FLIST{$AM_STMT})\n./Back/cgen.sa:3803:23");
     }
     case_num = FSETdTP_sizerINT(this_branch);
     L231_=(0)<(case_num); 
     if (L231_) {
      {
       struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname2 = &other2_0;
       BOOL f_L241_ = TRUE;
       L21 = this_branch;
       /* loop index variable */
       L71 = 0;
       L31 = case_arr;
       noname2->self = L21;
       noname2->state = 0;
       L241_br=L31==NULL?0:ASIZE((ARRAYdTP)L31); 
       while (1) {
        if(L71>=L241_br)  goto after_loop1; 
        L25 = FSETdTP_eltbrdTP(noname2);
        if (noname2->state == -1) {
         goto after_loop1;
        }
        L26 = L25;
        SARR((ARRAYdTP)L31,L71,(dTP)L26); 
        ;
        L271_=INTPLUS(L71,1); 
        L71 = L271_;
       }
      }
      after_loop1: ;
      L281_=INTMINUS(case_num,1); 
      ARRAYd1239612935(case_arr, 0, L281_);
     }
    }
    else {
     if (FSETdT1764379880(consider, tp)) {
      create_self1 = ((FSETdTP) NULL);
      ret_val8 = ((FSETdTP) NULL);
      this_branch = FSETdT1016814448(ret_val8, tp);
      case_num = 1;
      SARR((ARRAYdTP)case_arr,0,(dTP)tp); 
      ;
     }
     else {
      case_num = 0;
     }
    }
    L301_=(0)<(case_num); 
    if (L301_) {
     {
      BOOL f_L321_ = TRUE;
      /* loop index variable */
      L81 = 0;
      L41 = case_arr;
      L51 = 0;
      L61 = case_num;
      L321_c=L51-1;L321_m=L51+L61; 
      while (1) {
       if(++L321_c>=L321_m)  goto after_loop2; aL321_=ARR((ARRAYdTP)L41,L321_c); 
       t = aL321_;
       ndefer_self2 = self;
       plus_self4 = ((STR) &case3);
       tag_for_self = self;
       tag_for_tp = t;
       SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
       mang_self = tag_for_self;
       mang_ob = ((OB) tag_for_tp);
       ret_val10 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
       res = STR_ap2004550586(ret_val10, ((STR) &S_tag));
       forbid_self = tag_for_self;
       forbid_s = res;
       forbid_self1 = ATTR(forbid_self,mangler);
       forbid_s1 = forbid_s;
       SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
       ret_val9 = res;
       plus_sarg4 = ret_val9;
       ret_val11 = STR_ap2004550586(plus_self4, plus_sarg4);
       plus_self5 = ret_val11;
       plus_sarg5 = ((STR) &name21);
       ret_val12 = STR_ap2004550586(plus_self5, plus_sarg5);
       ndefer_s2 = ret_val12;
       CGEN_d695831906(ndefer_self2);
       SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
       L341_=INTPLUS(L81,1); 
       L81 = L341_;
      }
     }
     after_loop2: ;
     in_self1 = self;
     L35 = ATTR(in_self1,indent);
     L361_=INTPLUS(L35,1); 
     SATTR(in_self1,indent,L361_);
     aget_self1 = stmts11;
     aget_ind1 = i;
     L371_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
     ret_val13 = L371_;
     CGEN_e494460490(self, ret_val13);
     defer_self = self;
     defer_s = ((STR) &break5);
     SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
     move_out_self = self;
     L38 = ATTR(move_out_self,indent);
     L391_=INTMINUS(L38,1); 
     SATTR(move_out_self,indent,L391_);
     consider = FSETdT1854975287(consider, this_branch);
    }
   }
  }
  after_loop: ;
 }
 ndefer_self3 = self;
 ndefer_s3 = ((STR) &default2);
 CGEN_d695831906(ndefer_self3);
 SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e2141826530(CGEN self, AM_TYPECASE_STMT arg) {
 STR label = ((STR) NULL);
 AM_LOCAL_EXPR is_near_self;
 BOOL ret_val = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self;
 MANGLE genother_self;
 OB genother_ns;
 STR ret_val1;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val2;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val3;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val4;
 FSETSTR create_self2;
 FSETSTR ret_val5;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val6;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN move_out_self;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN in_self1;
 CGEN move_out_self1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val9;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN in_self2;
 CGEN move_out_self2;
 CGEN move_out_self3;
 CGEN ndefer_self5;
 STR ndefer_s5;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 dTP L4;
 BOOL L5;
 BOOL L61_;
 extern STR ifFVOID;
 STR L7;
 extern STR name105;
 extern STR if3;
 STR L8;
 extern STR NULL3;
 INT L9;
 INT L101_;
 NAMESPACE L11;
 OB L12;
 extern STR other359;
 extern STR goto1;
 extern STR name16;
 extern STR Voidob1946594598;
 INT L13;
 INT L141_;
 extern STR else7;
 INT L15;
 INT L161_;
 extern STR Noappl834681518;
 INT L17;
 INT L181_;
 BOOL L19;
 BOOL L201_;
 extern STR name136;
 INT L21;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 INT L261_;
 extern STR name4;
 if (ATTR(arg,has_void_stmts)) {
  L1 = TRUE;
 } else {
  L1 = ATTR(self,chk_when);
 }
 if (L1) {
  if (ATTR(ATTR(self,prog),distributed)) {
   is_near_self = ATTR(arg,test1);
   if (ATTR(is_near_self,is_always_near)) {
    L3 = TRUE;
   } else {
    L4 = ATTR(is_near_self,tp_at);
    L3 = (*dTP_is1334578382[TAG(L4)])(L4);
   }
   ret_val = L3;
   L5 = ret_val;
   L61_=!(L5); 
   L2 = L61_;
  } else {
   L2 = FALSE;
  }
  if (L2) {
   ndefer_self = self;
   L7 = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(arg,test1)));
   ndefer_s = STR_ap1693864410(((STR) &ifFVOID), L7, ((STR) &name105));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  }
  else {
   ndefer_self1 = self;
   L8 = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(arg,test1)));
   ndefer_s1 = STR_ap1693864410(((STR) &if3), L8, ((STR) &NULL3));
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  in_self = self;
  L9 = ATTR(in_self,indent);
  L101_=INTPLUS(L9,1); 
  SATTR(in_self,indent,L101_);
  if (ATTR(arg,has_void_stmts)) {
   genother_self = ATTR(self,mangler);
   genother_ns = ((OB) NULL);
   space_self = genother_self;
   space_ns = genother_ns;
   if ((space_ns==((OB) NULL))) {
    space_ns = ((OB) space_self);
   }
   r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
   if ((r==((NAMESPACE) NULL))) {
    create_self = ((NAMESPACE) NULL);
    L12=ZALLOC(sizeof(struct NAMESPACE_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=NAMESPACE_tag;
    L11 = ((NAMESPACE) L12);
    r1 = L11;
    create_self1 = ((NAMEMAP) NULL);
    ret_val4 = ((NAMEMAP) NULL);
    SATTR(r1,map,ret_val4);
    create_self2 = ((FSETSTR) NULL);
    ret_val5 = ((FSETSTR) NULL);
    SATTR(r1,set,ret_val5);
    SATTR(r1,counter,0);
    create_self3 = ((FMAPSTRINT) NULL);
    ret_val6 = ((FMAPSTRINT) NULL);
    SATTR(r1,uniques,ret_val6);
    ret_val3 = r1;
    r = ret_val3;
    SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
   }
   ret_val2 = r;
   ret_val1 = NAMESP8552102(ret_val2, ((STR) &other359));
   label = ret_val1;
   ndefer_self2 = self;
   plus_self = ((STR) &goto1);
   plus_sarg = label;
   ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val7;
   plus_sarg1 = ((STR) &name16);
   ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
   ndefer_s2 = ret_val8;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  }
  else {
   CGEN_r878823253(self, ((STR) &Voidob1946594598), ((dAM) arg));
  }
  move_out_self = self;
  L13 = ATTR(move_out_self,indent);
  L141_=INTMINUS(L13,1); 
  SATTR(move_out_self,indent,L141_);
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &else7);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 CGEN_e2137132086(self, ((dAM_EXPR) ATTR(arg,test1)), ATTR(arg,tgts), ATTR(arg,stmts));
 if (ATTR(arg,no_else)) {
  in_self1 = self;
  L15 = ATTR(in_self1,indent);
  L161_=INTPLUS(L15,1); 
  SATTR(in_self1,indent,L161_);
  CGEN_r878823253(self, ((STR) &Noappl834681518), ((dAM) arg));
  move_out_self1 = self;
  L17 = ATTR(move_out_self1,indent);
  L181_=INTMINUS(L17,1); 
  SATTR(move_out_self1,indent,L181_);
 }
 else {
  L19 = (label==((STR) NULL));
  L201_=!(L19); 
  if (L201_) {
   ndefer_self4 = self;
   plus_self2 = label;
   plus_sarg2 = ((STR) &name136);
   ret_val9 = STR_ap2004550586(plus_self2, plus_sarg2);
   ndefer_s4 = ret_val9;
   CGEN_d695831906(ndefer_self4);
   SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
  }
  in_self2 = self;
  L21 = ATTR(in_self2,indent);
  L221_=INTPLUS(L21,1); 
  SATTR(in_self2,indent,L221_);
  CGEN_e494460490(self, ATTR(arg,else_stmts));
  move_out_self2 = self;
  L23 = ATTR(move_out_self2,indent);
  L241_=INTMINUS(L23,1); 
  SATTR(move_out_self2,indent,L241_);
 }
 move_out_self3 = self;
 L25 = ATTR(move_out_self3,indent);
 L261_=INTMINUS(L25,1); 
 SATTR(move_out_self3,indent,L261_);
 ndefer_self5 = self;
 ndefer_s5 = ((STR) &name4);
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e286683954(CGEN self, AM_CASE_STMT arg) {
 ARRAYARRAYSTR targets = ((ARRAYARRAYSTR) NULL);
 STR test1;
 ARRAYARRAYSTR L11;
 INT i = INT_zero;
 ARRAYSTR L21;
 INT j = INT_zero;
 ARRAYARRAYSTR L31;
 INT i1 = INT_zero;
 BOOL emit_case = BOOL_zero;
 STR case_str = ((STR) NULL);
 ARRAYSTR L41;
 INT j11 = INT_zero;
 BOOL already_emitted = BOOL_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT k = INT_zero;
 INT l = INT_zero;
 ARRAYSTR L71;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 STR L101;
 ARRAYARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYARRAYSTR ret_val;
 INT L121 = INT_zero;
 FLISTF7594819 aget_self;
 INT aget_ind = INT_zero;
 FLISTdAM_CONST ret_val1;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val2;
 INT L131 = INT_zero;
 FLISTF7594819 aget_self1;
 INT aget_ind1 = INT_zero;
 FLISTdAM_CONST ret_val3;
 FLISTdAM_CONST aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_CONST ret_val4;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN comment_self;
 STR comment_com;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L141 = INT_zero;
 INT L151 = INT_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self1;
 FLISTdAM_STMT aget_self3;
 INT aget_ind3 = INT_zero;
 dAM_STMT ret_val9;
 CGEN move_out_self;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN in_self2;
 CGEN move_out_self1;
 CGEN move_out_self2;
 CGEN ndefer_self5;
 STR ndefer_s5;
 BOOL L16;
 BOOL L171_;
 ARRAYARRAYSTR L18;
 INT L19;
 OB L20;
 INT L22;
 INT L231_br;
 INT rL231_;
 INT L24;
 FLISTdAM_CONST L251_;
 ARRAYSTR L26;
 INT L27;
 OB L28;
 INT L29;
 ARRAYSTR L30;
 INT L34;
 ARRAYSTR L351_;
 INT L331_br;
 INT rL331_;
 ARRAYSTR L361_;
 ARRAYSTR L37;
 FLISTdAM_CONST L381_;
 dAM_CONST L391_;
 STR L40;
 INT L431_;
 INT L441_;
 extern STR switch2;
 extern STR name84;
 INT L45;
 INT L461_;
 extern STR casestatement;
 extern STR name54;
 extern STR name79;
 INT L471_br;
 INT rL471_;
 INT L48;
 extern STR name3;
 INT L50;
 ARRAYSTR L521_;
 INT L491_br;
 INT rL491_;
 extern STR case3;
 INT L531_,L531_m;
 INT L54;
 INT L571_;
 INT L561_,L561_m;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 INT L62;
 ARRAYSTR L631_;
 INT L551_br;
INT i_L551_=0;
 INT rL551_;
 INT L64;
 ARRAYSTR L651_;
 ARRAYSTR L66;
 STR L671_;
 STR L68;
 ARRAYSTR L691_;
 ARRAYSTR L70;
 STR L721_;
 BOOL L731_;
 STR L74;
 ARRAYSTR L751_;
 ARRAYSTR L76;
 STR L771_;
 INT L781_;
 extern STR name20;
 INT L79;
 INT L801_;
 dAM_STMT L821_;
 INT L83;
 INT L841_;
 extern STR break5;
 INT L851_;
 extern STR default2;
 INT L86;
 INT L871_;
 extern STR Noappl1426784379;
 INT L88;
 INT L891_;
 INT L90;
 INT L921_;
 extern STR name4;
 extern STR Noappl1426784379;
 test1 = CGEN_e1639504572(self, ATTR(arg,test1));
 L16 = (ATTR(arg,tgts)==((FLISTF7594819) NULL));
 L171_=!(L16); 
 if (L171_) {
  create_self = ((ARRAYARRAYSTR) NULL);
  create_n = FLISTF873512073(ATTR(arg,tgts));
  L19 = create_n;
  L22=(sizeof(struct ARRAYARRAYSTR_struct)-sizeof(ARRAYSTR))+(L19)*sizeof(ARRAYSTR);
  L20=ZALLOC_BIG(L22);
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=ARRAYARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L20)->header.destroyed=0;
#endif

  L18 = ((ARRAYARRAYSTR) L20);
  L18->asize = L19;
  ret_val = L18;
  targets = ret_val;
  {
   BOOL f_L231_ = TRUE;
   /* loop index variable */
   L121 = 0;
   L11 = targets;
   L231_br=L11==NULL?0:ASIZE((ARRAYARRAYSTR)L11); 
   while (1) {
    if(L121>=L231_br)  goto after_loop; 
    rL231_=L121; 
    i = rL231_;
    create_self1 = ((ARRAYSTR) NULL);
    aget_self = ATTR(arg,tgts);
    aget_ind = i;
    L251_=(FLISTdAM_CONST)ARR((FLISTF7594819)aget_self,aget_ind); 
    ret_val1 = L251_;
    create_n1 = FLISTd1855368276(ret_val1);
    L27 = create_n1;
    L29=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L27)*sizeof(STR);
    L28=ZALLOC_BIG(L29);
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

      ((OB)L28)->header.destroyed=0;
#endif

    L26 = ((ARRAYSTR) L28);
    L26->asize = L27;
    ret_val2 = L26;
    L30 = ret_val2;
    SARR((ARRAYARRAYSTR)targets,i,(ARRAYSTR)L30); 
    ;
    {
     BOOL f_L331_ = TRUE;
     /* loop index variable */
     L131 = 0;
     while (1) {
      if (f_L331_) {
       f_L331_ = FALSE;
       L351_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,i); 
       L21 = L351_;
       L331_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
      }
      if(L131>=L331_br)  goto after_loop1; 
      rL331_=L131; 
      j = rL331_;
      L361_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,i); 
      L37 = L361_;
      aget_self1 = ATTR(arg,tgts);
      aget_ind1 = i;
      L381_=(FLISTdAM_CONST)ARR((FLISTF7594819)aget_self1,aget_ind1); 
      ret_val3 = L381_;
      aget_self2 = ret_val3;
      aget_ind2 = j;
      L391_=(dAM_CONST)ARR((FLISTdAM_CONST)aget_self2,aget_ind2); 
      ret_val4 = L391_;
      L40 = CGEN_e1639504572(self, ((dAM_EXPR) ret_val4));
      SARR((ARRAYSTR)L37,j,(STR)L40); 
      ;
      L431_=INTPLUS(L131,1); 
      L131 = L431_;
     }
    }
    after_loop1: ;
    L441_=INTPLUS(L121,1); 
    L121 = L441_;
   }
  }
  after_loop: ;
  ndefer_self = self;
  ndefer_s = STR_ap1693864410(((STR) &switch2), test1, ((STR) &name84));
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  in_self = self;
  L45 = ATTR(in_self,indent);
  L461_=INTPLUS(L45,1); 
  SATTR(in_self,indent,L461_);
  comment_self = self;
  comment_com = ((STR) &casestatement);
  if (ATTR(comment_self,pretty)) {
   ndefer_self1 = comment_self;
   plus_self = ((STR) &name54);
   plus_sarg = comment_com;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val5;
   plus_sarg1 = ((STR) &name79);
   ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
   ndefer_s1 = ret_val6;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  {
   BOOL f_L471_ = TRUE;
   /* loop index variable */
   L141 = 0;
   L31 = targets;
   L471_br=L31==NULL?0:ASIZE((ARRAYARRAYSTR)L31); 
   while (1) {
    if(L141>=L471_br)  goto after_loop2; 
    rL471_=L141; 
    i1 = rL471_;
    emit_case = FALSE;
    {
     BOOL f_L491_ = TRUE;
     struct STR_se405450305_frame_struct other4_0;
STR_se405450305_frame noname1 = &other4_0;
     /* loop index variable */
     L151 = 0;
     L101 = ((STR) &name3);
     noname1->self = L101;
     noname1->state = 0;
     while (1) {
      if (f_L491_) {
       f_L491_ = FALSE;
       L521_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,i1); 
       L41 = L521_;
       L491_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
      }
      if(L151>=L491_br)  goto after_loop3; 
      rL491_=L151; 
      j11 = rL491_;
      case_str = ((STR) &case3);
      already_emitted = FALSE;
      {
       BOOL f_L531_ = TRUE;
       L51 = 0;
       L61 = i1;
       L531_=L51-1;L531_m=L61; 
       while (1) {
        if(L531_++>=L531_m)  goto after_loop4; 
        k = L531_;
        {
         BOOL f_L551_ = TRUE;
         BOOL f_L561_ = TRUE;
         L81 = 0;
         L571_=INTMINUS(j11,1); 
         L91 = L571_;
         L561_=L81-1;L561_m=L91; 
         while (1) {
          L581_=(k)==(i1); 
          L59 = L581_;
          L601_=!(L59); 
          if (L601_) {
           if (f_L551_) {
            f_L551_ = FALSE;
            L631_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,k); 
            L71 = L631_;
            L551_br=L71==NULL?0:ASIZE((ARRAYSTR)L71); 
            i_L551_=0;
           }
           if(i_L551_>=L551_br)  goto after_loop5; 
           rL551_=i_L551_; i_L551_++;
           l = rL551_;
          }
          else {
           if(L561_++>=L561_m)  goto after_loop5; 
           l = L561_;
          }
          L651_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,k); 
          L66 = L651_;
          L671_=(STR)ARR((ARRAYSTR)L66,l); 
          L68 = L671_;
          L691_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,i1); 
          L70 = L691_;
          L721_=(STR)ARR((ARRAYSTR)L70,j11); 
          if (STR_is111530248(L68, L721_)) {
           already_emitted = TRUE;
          }
         }
        }
        after_loop5: ;
       }
      }
      after_loop4: ;
      L731_=!(already_emitted); 
      if (L731_) {
       plus_self2 = case_str;
       L751_=(ARRAYSTR)ARR((ARRAYARRAYSTR)targets,i1); 
       L76 = L751_;
       L771_=(STR)ARR((ARRAYSTR)L76,j11); 
       noname1->arg1 = ((dSTR) L771_);
       L74 = STR_se405450305(noname1);
       if (noname1->state == -1) {
        goto after_loop3;
       }
       plus_sarg2 = L74;
       ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
       case_str = ret_val7;
       emit_case = TRUE;
      }
      L781_=INTPLUS(L151,1); 
      L151 = L781_;
     }
    }
    after_loop3: ;
    if (emit_case) {
     ndefer_self2 = self;
     plus_self3 = case_str;
     plus_sarg3 = ((STR) &name20);
     ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
     ndefer_s2 = ret_val8;
     CGEN_d695831906(ndefer_self2);
     SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     in_self1 = self;
     L79 = ATTR(in_self1,indent);
     L801_=INTPLUS(L79,1); 
     SATTR(in_self1,indent,L801_);
     aget_self3 = ATTR(arg,stmts);
     aget_ind3 = i1;
     L821_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self3,aget_ind3); 
     ret_val9 = L821_;
     CGEN_e494460490(self, ret_val9);
     move_out_self = self;
     L83 = ATTR(move_out_self,indent);
     L841_=INTMINUS(L83,1); 
     SATTR(move_out_self,indent,L841_);
     ndefer_self3 = self;
     ndefer_s3 = ((STR) &break5);
     CGEN_d695831906(ndefer_self3);
     SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
    }
    L851_=INTPLUS(L141,1); 
    L141 = L851_;
   }
  }
  after_loop2: ;
  ndefer_self4 = self;
  ndefer_s4 = ((STR) &default2);
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
  in_self2 = self;
  L86 = ATTR(in_self2,indent);
  L871_=INTPLUS(L86,1); 
  SATTR(in_self2,indent,L871_);
  if (ATTR(arg,no_else)) {
   CGEN_r878823253(self, ((STR) &Noappl1426784379), ((dAM) arg));
  }
  else {
   CGEN_e494460490(self, ATTR(arg,else_stmts));
  }
  move_out_self1 = self;
  L88 = ATTR(move_out_self1,indent);
  L891_=INTMINUS(L88,1); 
  SATTR(move_out_self1,indent,L891_);
  move_out_self2 = self;
  L90 = ATTR(move_out_self2,indent);
  L921_=INTMINUS(L90,1); 
  SATTR(move_out_self2,indent,L921_);
  ndefer_self5 = self;
  ndefer_s5 = ((STR) &name4);
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 }
 else {
  CGEN_r878823253(self, ((STR) &Noappl1426784379), ((dAM) arg));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e290817711(CGEN self, AM_WIT1996971603 arg) {
 STR s = ((STR) NULL);
 ARRAYdAM_EXPR L11;
 dAM_EXPR a;
 STR t = ((STR) NULL);
 ARRAYdAM_EXPR L21;
 STR L31;
 FLISTBOOL nearobj;
 ARRAYdAM_EXPR L41;
 dAM_EXPR a11;
 ARRAYdAM_EXPR L51;
 dAM_EXPR a21;
 INT L61 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val4;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 FLISTBOOL create_self;
 FLISTBOOL ret_val5;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 CGEN move_out_self;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self1;
 CGEN move_out_self1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 extern STR name88;
 INT L91_br;
 dAM_EXPR aL91_;
 dAM_EXPR L12;
 dAM_EXPR L13;
 dTP L14;
 extern STR S01;
 extern STR PS_NEAR_OR_VOID;
 dAM_EXPR L15;
 INT L101_br;
INT i_L101_=0;
 dAM_EXPR aL101_;
 extern STR name26;
 STR L16;
 INT L171_;
 extern STR if4;
 extern STR name84;
 INT L18;
 INT L191_;
 INT L201_br;
 dAM_EXPR aL201_;
 dAM_EXPR L22;
 FLISTBOOL L23;
 INT L241_;
 INT L251_br;
 dAM_EXPR aL251_;
 dAM_EXPR L26;
 INT L271_;
 INT L28;
 INT L291_;
 extern STR else5;
 INT L30;
 INT L321_;
 INT L33;
 INT L341_;
 extern STR name4;
 {
  BOOL f_L91_ = TRUE;
  BOOL f_L101_ = TRUE;
  struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
  /* loop index variable */
  L61 = 0;
  L11 = ATTR(arg,objects1);
  L31 = ((STR) &name88);
  L91_br=L11==NULL?0:ASIZE((ARRAYdAM_EXPR)L11); 
  noname1->self = L31;
  noname1->state = 0;
  while (1) {
   if(L61>=L91_br)  goto after_loop; 
   aL91_=ARR((ARRAYdAM_EXPR)L11,L61); 
   a = aL91_;
   L13 = a;
   L14 = (*dAM_EXPR_tprdTP[TAG(L13)])(L13);
   if ((*dTP_is1334578382[TAG(L14)])(L14)) {
    t = ((STR) &S01);
   }
   else {
    plus_self = ((STR) &PS_NEAR_OR_VOID);
    if (f_L101_) {
     f_L101_ = FALSE;
     L21 = ATTR(arg,objects1);
     L101_br=L21==NULL?0:ASIZE((ARRAYdAM_EXPR)L21); 
     i_L101_=0;
    }
    if(i_L101_>=L101_br)  goto after_loop; 
    aL101_=ARR((ARRAYdAM_EXPR)L21,i_L101_); i_L101_++;
    plus_sarg = CGEN_e1639504572(self, aL101_);
    ret_val = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val;
    plus_sarg1 = ((STR) &name26);
    ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
    t = ret_val1;
   }
   plus_self2 = s;
   noname1->arg1 = ((dSTR) t);
   L16 = STR_se405450305(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg2 = L16;
   ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
   s = ret_val2;
   L171_=INTPLUS(L61,1); 
   L61 = L171_;
  }
 }
 after_loop: ;
 ndefer_self = self;
 plus_self3 = ((STR) &if4);
 plus_sarg3 = s;
 ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val3;
 plus_sarg4 = ((STR) &name84);
 ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
 ndefer_s = ret_val4;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 in_self = self;
 L18 = ATTR(in_self,indent);
 L191_=INTPLUS(L18,1); 
 SATTR(in_self,indent,L191_);
 create_self = ((FLISTBOOL) NULL);
 ret_val5 = ((FLISTBOOL) NULL);
 nearobj = ret_val5;
 {
  BOOL f_L201_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L41 = ATTR(arg,objects1);
  L201_br=L41==NULL?0:ASIZE((ARRAYdAM_EXPR)L41); 
  while (1) {
   if(L71>=L201_br)  goto after_loop1; 
   aL201_=ARR((ARRAYdAM_EXPR)L41,L71); 
   a11 = aL201_;
   if (a11==NULL) {
    goto other384;
   } else
   switch (TAG(a11)) {
    case AM_LOCAL_EXPR_tag:
     L23 = FLISTB1991913204(nearobj, ATTR(((AM_LOCAL_EXPR) a11),check_near));
     nearobj = FLISTB1991913204(L23, ATTR(((AM_LOCAL_EXPR) a11),is_always_near));
     SATTR(((AM_LOCAL_EXPR) a11),check_near,TRUE);
     SATTR(((AM_LOCAL_EXPR) a11),is_always_near,TRUE); break;
    default: ;
    other384: ;
   }
   L241_=INTPLUS(L71,1); 
   L71 = L241_;
  }
 }
 after_loop1: ;
 CGEN_e494460490(self, ATTR(arg,near_part));
 {
  BOOL f_L251_ = TRUE;
  /* loop index variable */
  L81 = 0;
  L51 = ATTR(arg,objects1);
  L251_br=L51==NULL?0:ASIZE((ARRAYdAM_EXPR)L51); 
  while (1) {
   if(L81>=L251_br)  goto after_loop2; 
   aL251_=ARR((ARRAYdAM_EXPR)L51,L81); 
   a21 = aL251_;
   if (a21==NULL) {
    goto other385;
   } else
   switch (TAG(a21)) {
    case AM_LOCAL_EXPR_tag:
     SATTR(((AM_LOCAL_EXPR) a21),check_near,FLISTB466509411(nearobj));
     SATTR(((AM_LOCAL_EXPR) a21),is_always_near,FLISTB466509411(nearobj)); break;
    default: ;
    other385: ;
   }
   L271_=INTPLUS(L81,1); 
   L81 = L271_;
  }
 }
 after_loop2: ;
 move_out_self = self;
 L28 = ATTR(move_out_self,indent);
 L291_=INTMINUS(L28,1); 
 SATTR(move_out_self,indent,L291_);
 ndefer_self1 = self;
 ndefer_s1 = ((STR) &else5);
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 in_self1 = self;
 L30 = ATTR(in_self1,indent);
 L321_=INTPLUS(L30,1); 
 SATTR(in_self1,indent,L321_);
 CGEN_e494460490(self, ATTR(arg,else_part));
 move_out_self1 = self;
 L33 = ATTR(move_out_self1,indent);
 L341_=INTMINUS(L33,1); 
 SATTR(move_out_self1,indent,L341_);
 ndefer_self2 = self;
 ndefer_s2 = ((STR) &name4);
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e398507978(CGEN self, AM_PREFETCH_STMT arg) {
 dAM_EXPR r;
 dAM_EXPR l;
 STR ax;
 STR loc;
 STR pr;
 STR attrname;
 STR sp;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val;
 AM_ATTR_EXPR tp_self;
 dTP ret_val1;
 AM_ATTR_EXPR tp_self1;
 dTP ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 AM_ATTR_EXPR tp_self2;
 dTP ret_val7;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val11;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val12;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val14;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val15;
 CGEN ndefer_self2;
 STR ndefer_s2;
 AM_ASSIGN_STMT create_self;
 AM_PREFETCH_STMT create_c;
 AM_ASSIGN_STMT ret_val16;
 AM_ASSIGN_STMT r1;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val17;
 AM_ASSIGN_STMT r2;
 BOOL L1;
 dAM_EXPR L2;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 dAM_EXPR L6;
 BOOL L7;
 BOOL L81_;
 OB L9;
 OB L10;
 extern STR PREFETCHNULL;
 BOOL L11;
 BOOL L121_;
 dTP L13;
 dTP L14;
 extern STR PRE_VA_RATTR_NA;
 extern STR name3;
 STR L15;
 extern STR name3;
 STR L16;
 extern STR name3;
 STR L17;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR PRE_V_RATTR_LP;
 extern STR name3;
 STR L18;
 extern STR name3;
 extern STR name3;
 STR L19;
 extern STR name3;
 STR L20;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 extern STR PRE_R_RATTR_NA;
 extern STR name3;
 STR L21;
 extern STR name3;
 STR L22;
 extern STR name3;
 STR L23;
 extern STR name3;
 extern STR name3;
 extern STR name76;
 AM_ASSIGN_STMT L24;
 OB L25;
 r = ATTR(arg,src1);
 l = ATTR(arg,dest);
 if (l==NULL) {
  goto other386;
 } else
 switch (TAG(l)) {
  case AM_LOCAL_EXPR_tag:
   if (r==NULL) {
    goto other387;
   } else
   switch (TAG(r)) {
    case AM_ATTR_EXPR_tag:
     L2 = ATTR(((AM_ATTR_EXPR) r),ob);
     L3 = (*dAM_EXPR_tprdTP[TAG(L2)])(L2);
     L4 = (*dTP_is1334578382[TAG(L3)])(L3);
     L51_=!(L4); 
     if (L51_) {
      L6 = ATTR(((AM_ATTR_EXPR) r),ob);
      L7 = (*dAM_EX1117610980[TAG(L6)])(L6);
      L81_=!(L7); 
      L1 = L81_;
     } else {
      L1 = FALSE;
     }
     if (L1) {
      ax = CGEN_e1639504572(self, ATTR(((AM_ATTR_EXPR) r),ob));
      loc = CGEN_e1639504572(self, l);
      pr = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(arg,prefetch)));
      SATTR(ATTR(arg,prefetch),is_hot,TRUE);
      mang_self = self;
      L10=ZALLOC(sizeof(struct IDENT_boxed_struct));
      if (L10==NULL) FATAL("Unable to allocate more memory");
      ((OB)L10)->header.tag=IDENT_tag;
      L9 = (OB)((IDENT_boxed) L10);
      ((IDENT_boxed) L9)->immutable_part = ATTR(((AM_ATTR_EXPR) r),at);
      mang_ob = L9;
      mang_ns = ((OB) ATTR(((AM_ATTR_EXPR) r),self_tp));
      ret_val = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
      attrname = ret_val;
      sp = ((STR) &PREFETCHNULL);
      L11 = (ATTR(ATTR(self,current_am_rout_),specul_prefetch)==((AM_LOCAL_EXPR) NULL));
      L121_=!(L11); 
      if (L121_) {
       sp = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(ATTR(self,current_am_rout_),specul_prefetch)));
      }
      tp_self = ((AM_ATTR_EXPR) r);
      ret_val1 = ATTR(tp_self,tp_at);
      L13 = ret_val1;
      if ((*dTP_is1334578382[TAG(L13)])(L13)) {
       tp_self1 = ((AM_ATTR_EXPR) r);
       ret_val2 = ATTR(tp_self1,tp_at);
       L14 = ret_val2;
       if ((*dTP_is418055720[TAG(L14)])(L14)) {
        ndefer_self = self;
        plus_self = ((STR) &name3);
        plus_sarg = sp;
        ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
        L15 = ret_val3;
        plus_self1 = ((STR) &name3);
        plus_sarg1 = loc;
        ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
        L16 = ret_val4;
        mang_self1 = self;
        mang_ob1 = ((OB) ATTR(((AM_ATTR_EXPR) r),self_tp));
        ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
        L17 = ret_val5;
        ndefer_s = STR_ap1182453786(((STR) &PRE_VA_RATTR_NA), pr, L15, L16, ((STR) &name3), L17, STR_ap44172742(((STR) &name3), ax, ((STR) &name3), attrname, ((STR) &name76)));
        CGEN_d695831906(ndefer_self);
        SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
       }
       else {
        ndefer_self1 = self;
        plus_self2 = ((STR) &name3);
        plus_sarg2 = sp;
        ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
        L18 = ret_val6;
        plus_self3 = ((STR) &name3);
        mang_self2 = self;
        tp_self2 = ((AM_ATTR_EXPR) r);
        ret_val7 = ATTR(tp_self2,tp_at);
        mang_ob2 = ((OB) ret_val7);
        ret_val8 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
        plus_sarg3 = ret_val8;
        ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
        plus_self4 = ret_val9;
        plus_sarg4 = ((STR) &name3);
        ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
        plus_self5 = ret_val10;
        plus_sarg5 = loc;
        ret_val11 = STR_ap2004550586(plus_self5, plus_sarg5);
        L19 = ret_val11;
        mang_self3 = self;
        mang_ob3 = ((OB) ATTR(((AM_ATTR_EXPR) r),self_tp));
        ret_val12 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
        L20 = ret_val12;
        ndefer_s1 = STR_ap1182453786(((STR) &PRE_V_RATTR_LP), pr, L18, L19, ((STR) &name3), L20, STR_ap44172742(((STR) &name3), ax, ((STR) &name3), attrname, ((STR) &name76)));
        CGEN_d695831906(ndefer_self1);
        SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
       }
      }
      else {
       ndefer_self2 = self;
       plus_self6 = ((STR) &name3);
       plus_sarg6 = sp;
       ret_val13 = STR_ap2004550586(plus_self6, plus_sarg6);
       L21 = ret_val13;
       plus_self7 = ((STR) &name3);
       plus_sarg7 = loc;
       ret_val14 = STR_ap2004550586(plus_self7, plus_sarg7);
       L22 = ret_val14;
       mang_self4 = self;
       mang_ob4 = ((OB) ATTR(((AM_ATTR_EXPR) r),self_tp));
       ret_val15 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
       L23 = ret_val15;
       ndefer_s2 = STR_ap1182453786(((STR) &PRE_R_RATTR_NA), pr, L21, L22, ((STR) &name3), L23, STR_ap44172742(((STR) &name3), ax, ((STR) &name3), attrname, ((STR) &name76)));
       CGEN_d695831906(ndefer_self2);
       SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
      }
      return;
     } break;
    default: ;
    other387: ;
   } break;
  default: ;
  other386: ;
 }
 create_self = ((AM_ASSIGN_STMT) NULL);
 create_c = arg;
 create_self1 = ((AM_ASSIGN_STMT) NULL);
 create_source = ATTR(create_c,source1);
 L25=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=AM_ASSIGN_STMT_tag;
 L24 = ((AM_ASSIGN_STMT) L25);
 r2 = L24;
 SATTR(r2,source1,create_source);
 ret_val17 = r2;
 r1 = ret_val17;
 SATTR(r1,dest,ATTR(create_c,dest));
 SATTR(r1,src1,ATTR(create_c,src1));
 SATTR(r1,next,((dAM_STMT) NULL));
 ret_val16 = r1;
 CGEN_e1389302728(self, ret_val16);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e494460490(CGEN self, dAM_STMT arg) {
 STR s1 = ((STR) NULL);
 STR s2 = ((STR) NULL);
 dAM_STMT next;
 CGEN emit_am_break_st;
 AM_BREAK_STMT emit_am_break_st1;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN pop_exceptions_s;
 INT pop_exceptions_n = INT_zero;
 INT L11 = INT_zero;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT str_self = INT_zero;
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN emit_am_comment_;
 AM_COMMENT_STMT emit_am_comment_1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN emit_am_pre_stmt;
 AM_PRE_STMT emit_am_pre_stmt1;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN in_self;
 CGEN move_out_self;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN emit_am_post_stm;
 AM_POST_STMT emit_am_post_stm1;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN in_self1;
 CGEN move_out_self1;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN emit_am_initial_;
 AM_INITIAL_STMT emit_am_initial_1;
 CGEN emit_am_assert_s;
 AM_ASSERT_STMT emit_am_assert_s1;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN in_self2;
 CGEN move_out_self2;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN emit_am_raise_st;
 AM_RAISE_STMT emit_am_raise_st1;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val5;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN emit_am_unlock_s;
 AM_UNLOCK_STMT emit_am_unlock_s1;
 CGEN ndefer_self12;
 STR ndefer_s12;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN emit_am_sync_stm;
 AM_SYNC_STMT emit_am_sync_stm1;
 CGEN ndefer_self14;
 STR ndefer_s14;
 CGEN emit_am_waitfor_;
 AM_WAITFOR_STMT emit_am_waitfor_1;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 CGEN ndefer_self15;
 STR ndefer_s15;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 UNIX exit_self;
 INT exit_code = INT_zero;
 dAM_STMT L2;
 extern STR LM_LOC1545209594;
 INT L3;
 INT L4;
 INT L51_;
 extern STR POP_EXCEPTION12;
 extern STR POP_EXCEPTION11;
 BOOL L6;
 BOOL L71_;
 extern STR name76;
 extern STR goto1;
 extern STR name16;
 extern STR name54;
 extern STR name79;
 extern STR ifb1;
 STR L8;
 extern STR name105;
 INT L9;
 INT L101_;
 extern STR Violat835114313;
 INT L12;
 INT L131_;
 extern STR name4;
 extern STR ifb1;
 STR L14;
 extern STR name105;
 INT L15;
 INT L161_;
 extern STR Violat32627033;
 INT L17;
 INT L181_;
 extern STR name4;
 extern STR ifb1;
 STR L19;
 extern STR name105;
 INT L20;
 INT L211_;
 extern STR Violat167861416;
 INT L22;
 INT L231_;
 extern STR name4;
 extern STR RAISE1;
 dAM_EXPR L24;
 extern STR name76;
 extern STR Should515168967;
 extern STR LM_UNLOCK;
 extern STR name76;
 extern STR SYNC1;
 extern STR PREFETCH_WAIT;
 extern STR name76;
 extern STR Thebac1831246981;
 INT L251_;
 INT L26;
 STR L271_;
 extern STR Intern28965746;
 dAM_STMT L28;
 BOOL L29;
 BOOL L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 while (1) {
  if ((arg==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = arg;
  CGEN_a1893106143(self, (*dAM_ST1805574914[TAG(L2)])(L2));
  if (arg==NULL) {
   goto other388;
  } else
  switch (TAG(arg)) {
   case AM_ASSIGN_STMT_tag:
    CGEN_e1389302728(self, ((AM_ASSIGN_STMT) arg)); break;
   case AM_IF_STMT_tag:
    CGEN_e638028220(self, ((AM_IF_STMT) arg)); break;
   case AM_LOOP_STMT_tag:
    CGEN_e1197535054(self, ((AM_LOOP_STMT) arg)); break;
   case AM_BREAK_STMT_tag:
    emit_am_break_st = self;
    emit_am_break_st1 = ((AM_BREAK_STMT) arg);
    if (ATTR(emit_am_break_st,manual_loop_unlo)) {
     ndefer_self = emit_am_break_st;
     ndefer_s = ((STR) &LM_LOC1545209594);
     CGEN_d695831906(ndefer_self);
     SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
    }
    pop_exceptions_s = emit_am_break_st;
    L3 = ATTR(emit_am_break_st,ex_nesting);
    L4 = ATTR(emit_am_break_st,current_loop_ex_);
    L51_=INTMINUS(L3,L4); 
    pop_exceptions_n = L51_;
    L11 = pop_exceptions_n;
    switch (L11) {
     case 0: 
      break;
     case 1: 
      ndefer_self1 = pop_exceptions_s;
      ndefer_s1 = ((STR) &POP_EXCEPTION12);
      CGEN_d695831906(ndefer_self1);
      SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      break;
     default: ;
      ndefer_self2 = pop_exceptions_s;
      str_self = pop_exceptions_n;
      clear_self = buf1;
      L6 = (clear_self==((FSTR) NULL));
      L71_=!(L6); 
      if (L71_) {
       SATTR(clear_self,loc,0);
      }
      str_in_self = str_self;
      str_in_s = buf1;
      ret_val1 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
      buf1 = ret_val1;
      str_self1 = buf1;
      ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
      ret_val = ret_val2;
      ndefer_s2 = STR_ap1693864410(((STR) &POP_EXCEPTION11), ret_val, ((STR) &name76));
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
    }
    ndefer_self3 = emit_am_break_st;
    ndefer_s3 = STR_ap1693864410(((STR) &goto1), ATTR(emit_am_break_st,current_loop), ((STR) &name16));
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3)); break;
   case AM_RETURN_STMT_tag:
    CGEN_e101922820(self, ((AM_RETURN_STMT) arg)); break;
   case AM_COMMENT_STMT_tag:
    emit_am_comment_ = self;
    emit_am_comment_1 = ((AM_COMMENT_STMT) arg);
    ndefer_self4 = emit_am_comment_;
    plus_self = ((STR) &name54);
    plus_sarg = ATTR(emit_am_comment_1,comment1);
    ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val3;
    plus_sarg1 = ((STR) &name79);
    ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
    ndefer_s4 = ret_val4;
    CGEN_d695831906(ndefer_self4);
    SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4)); break;
   case AM_EXPR_STMT_tag:
    CGEN_e1669826382(self, ((AM_EXPR_STMT) arg)); break;
   case AM_YIELD_STMT_tag:
    CGEN_e1441236469(self, ((AM_YIELD_STMT) arg)); break;
   case AM_CASE_STMT_tag:
    CGEN_e286683954(self, ((AM_CASE_STMT) arg)); break;
   case AM_PRE_STMT_tag:
    emit_am_pre_stmt = self;
    emit_am_pre_stmt1 = ((AM_PRE_STMT) arg);
    if (ATTR(emit_am_pre_stmt,chk_pre)) {
     ndefer_self5 = emit_am_pre_stmt;
     L8 = CGEN_e1639504572(emit_am_pre_stmt, ATTR(emit_am_pre_stmt1,test1));
     ndefer_s5 = STR_ap1693864410(((STR) &ifb1), L8, ((STR) &name105));
     CGEN_d695831906(ndefer_self5);
     SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
     in_self = emit_am_pre_stmt;
     L9 = ATTR(in_self,indent);
     L101_=INTPLUS(L9,1); 
     SATTR(in_self,indent,L101_);
     CGEN_r878823253(emit_am_pre_stmt, ((STR) &Violat835114313), ((dAM) emit_am_pre_stmt1));
     move_out_self = emit_am_pre_stmt;
     L12 = ATTR(move_out_self,indent);
     L131_=INTMINUS(L12,1); 
     SATTR(move_out_self,indent,L131_);
     ndefer_self6 = emit_am_pre_stmt;
     ndefer_s6 = ((STR) &name4);
     CGEN_d695831906(ndefer_self6);
     SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
    } break;
   case AM_POST_STMT_tag:
    emit_am_post_stm = self;
    emit_am_post_stm1 = ((AM_POST_STMT) arg);
    if (ATTR(emit_am_post_stm,chk_post)) {
     ndefer_self7 = emit_am_post_stm;
     L14 = CGEN_e1639504572(emit_am_post_stm, ATTR(emit_am_post_stm1,test1));
     ndefer_s7 = STR_ap1693864410(((STR) &ifb1), L14, ((STR) &name105));
     CGEN_d695831906(ndefer_self7);
     SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
     in_self1 = emit_am_post_stm;
     L15 = ATTR(in_self1,indent);
     L161_=INTPLUS(L15,1); 
     SATTR(in_self1,indent,L161_);
     CGEN_r878823253(emit_am_post_stm, ((STR) &Violat32627033), ((dAM) emit_am_post_stm1));
     move_out_self1 = emit_am_post_stm;
     L17 = ATTR(move_out_self1,indent);
     L181_=INTMINUS(L17,1); 
     SATTR(move_out_self1,indent,L181_);
     ndefer_self8 = emit_am_post_stm;
     ndefer_s8 = ((STR) &name4);
     CGEN_d695831906(ndefer_self8);
     SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
    } break;
   case AM_INITIAL_STMT_tag:
    emit_am_initial_ = self;
    emit_am_initial_1 = ((AM_INITIAL_STMT) arg);
    if (ATTR(emit_am_initial_,chk_post)) {
     CGEN_e494460490(emit_am_initial_, ATTR(emit_am_initial_1,stmts));
    } break;
   case AM_ASSERT_STMT_tag:
    emit_am_assert_s = self;
    emit_am_assert_s1 = ((AM_ASSERT_STMT) arg);
    if (ATTR(emit_am_assert_s,chk_assert)) {
     ndefer_self9 = emit_am_assert_s;
     L19 = CGEN_e1639504572(emit_am_assert_s, ATTR(emit_am_assert_s1,test1));
     ndefer_s9 = STR_ap1693864410(((STR) &ifb1), L19, ((STR) &name105));
     CGEN_d695831906(ndefer_self9);
     SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
     in_self2 = emit_am_assert_s;
     L20 = ATTR(in_self2,indent);
     L211_=INTPLUS(L20,1); 
     SATTR(in_self2,indent,L211_);
     CGEN_r878823253(emit_am_assert_s, ((STR) &Violat167861416), ((dAM) emit_am_assert_s1));
     move_out_self2 = emit_am_assert_s;
     L22 = ATTR(move_out_self2,indent);
     L231_=INTMINUS(L22,1); 
     SATTR(move_out_self2,indent,L231_);
     ndefer_self10 = emit_am_assert_s;
     ndefer_s10 = ((STR) &name4);
     CGEN_d695831906(ndefer_self10);
     SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
    } break;
   case AM_TYPECASE_STMT_tag:
    CGEN_e2141826530(self, ((AM_TYPECASE_STMT) arg)); break;
   case AM_RAISE_STMT_tag:
    emit_am_raise_st = self;
    emit_am_raise_st1 = ((AM_RAISE_STMT) arg);
    ndefer_self11 = emit_am_raise_st;
    cast_self = emit_am_raise_st;
    cast_dest_tp = ((dTP) TP_BUI1325635093);
    L24 = ATTR(emit_am_raise_st1,val1);
    cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L24)])(L24);
    cast_expr = CGEN_e1639504572(emit_am_raise_st, ATTR(emit_am_raise_st1,val1));
    cast_only_boxing = FALSE;
    ret_val5 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
    ndefer_s11 = STR_ap1693864410(((STR) &RAISE1), ret_val5, ((STR) &name76));
    CGEN_d695831906(ndefer_self11);
    SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11)); break;
   case AM_INV1167837230_tag:
    CGEN_e1378643769(self, ((AM_INV1167837230) arg)); break;
   case AM_PROTECT_STMT_tag:
    CGEN_e881275767(self, ((AM_PROTECT_STMT) arg)); break;
   case AM_ATTACH_STMT_tag:
    CGEN_e1809841360(self, ((AM_ATTACH_STMT) arg)); break;
   case AM_FORK_STMT_tag:
    CGEN_e1959168490(self, ((AM_FORK_STMT) arg)); break;
   case AM_PAR_STMT_tag:
    CGEN_e1270633199(self, ((AM_PAR_STMT) arg)); break;
   case AM_LOCK_STMT_tag:
    CGEN_e1553420074(self, ((AM_LOCK_STMT) arg)); break;
   case AM_UNLOCK_STMT_tag:
    emit_am_unlock_s = self;
    emit_am_unlock_s1 = ((AM_UNLOCK_STMT) arg);
    ndefer_self12 = emit_am_unlock_s;
    ndefer_s12 = ((STR) &Should515168967);
    CGEN_d695831906(ndefer_self12);
    SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
    ndefer_self13 = emit_am_unlock_s;
    plus_self2 = ((STR) &LM_UNLOCK);
    plus_sarg2 = CGEN_e1639504572(emit_am_unlock_s, ATTR(emit_am_unlock_s1,lock_ob));
    ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val6;
    plus_sarg3 = ((STR) &name76);
    ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
    ndefer_s13 = ret_val7;
    CGEN_d695831906(ndefer_self13);
    SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13)); break;
   case AM_WIT1996971603_tag:
    CGEN_e290817711(self, ((AM_WIT1996971603) arg)); break;
   case AM_SYNC_STMT_tag:
    emit_am_sync_stm = self;
    emit_am_sync_stm1 = ((AM_SYNC_STMT) arg);
    ndefer_self14 = emit_am_sync_stm;
    ndefer_s14 = ((STR) &SYNC1);
    CGEN_d695831906(ndefer_self14);
    SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14)); break;
   case AM_PREFETCH_STMT_tag:
    CGEN_e398507978(self, ((AM_PREFETCH_STMT) arg)); break;
   case AM_WAITFOR_STMT_tag:
    emit_am_waitfor_ = self;
    emit_am_waitfor_1 = ((AM_WAITFOR_STMT) arg);
    if (ATTR(ATTR(emit_am_waitfor_1,prefetch),is_hot)) {
     ndefer_self15 = emit_am_waitfor_;
     plus_self4 = ((STR) &PREFETCH_WAIT);
     plus_sarg4 = CGEN_e1639504572(emit_am_waitfor_, ((dAM_EXPR) ATTR(emit_am_waitfor_1,prefetch)));
     ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val8;
     plus_sarg5 = ((STR) &name76);
     ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
     ndefer_s15 = ret_val9;
     CGEN_d695831906(ndefer_self15);
     SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
    } break;
   default: ;
   other388: ;
    barf_self = self;
    plus_self6 = ((STR) &Thebac1831246981);
    L251_=SYSTP(arg); 
    L26 = L251_;
    L271_=SYSSTRFORTP(L26); 
    plus_sarg6 = L271_;
    ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
    barf_msg = ret_val10;
    barf_self1 = ATTR(barf_self,prog);
    barf_msg1 = barf_msg;
    barf_at_self = barf_self1;
    barf_at_msg = barf_msg1;
    barf_at_at = ((dPROG_ERR) NULL);
    PROG_e176405615(barf_at_self, barf_at_at);
    plus_self7 = ((STR) &Intern28965746);
    plus_sarg7 = barf_at_msg;
    ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
    PROG_err_STR(barf_at_self, ret_val11);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
  }
  L28 = arg;
  next = (*dAM_ST775224435[TAG(L28)])(L28);
  if (arg==NULL) {
   goto other389;
  } else
  switch (TAG(arg)) {
   case AM_LOOP_STMT_tag: break;
   case AM_PAR_STMT_tag: break;
   case AM_FORK_STMT_tag: break;
   default: ;
   other389: ;
    L30 = ATTR(self,no_destroy);
    L311_=!(L30); 
    if (L311_) {
     L32 = ATTR(self,no_destroy_lock);
     L331_=!(L32); 
     L29 = L331_;
    } else {
     L29 = FALSE;
    }
    if (L29) {
     SYSDESTROY(arg); 
     ;
    }
  }
  arg = next;
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e520888267(CGEN self, AM_ROUT_DEF f) {
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 CGEN ndefer_self;
 STR ndefer_s;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val2 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 CODE_FILE plus_self2;
 dSTR plus_s;
 AM_ROUT_DEF is_iter_self3;
 BOOL ret_val5 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val6 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CGEN move_out_self;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 OUT create_self;
 OUT ret_val8;
 OUT plus_self7;
 STR plus_s5;
 OUT ret_val9;
 FILE1 stdout_self;
 FILE1 ret_val10;
 FILE1 r;
 OUT plus_self8;
 STR plus_s6;
 OUT ret_val11;
 FILE1 stdout_self1;
 FILE1 ret_val12;
 FILE1 r1;
 OUT plus_self9;
 STR plus_s7;
 OUT ret_val13;
 FILE1 stdout_self2;
 FILE1 ret_val14;
 FILE1 r2;
 OUT plus_self10;
 dSTR plus_s8;
 OUT ret_val15;
 FILE1 stdout_self3;
 FILE1 ret_val16;
 FILE1 r3;
 OUT plus_self11;
 STR plus_s9;
 FILE1 stdout_self4;
 FILE1 ret_val17;
 FILE1 r4;
 BOOL L1;
 BOOL L21_;
 extern STR PREFET1824934344;
 extern STR name76;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L11;
 BOOL L121_;
 STR L13;
 INT L14;
 INT L151_;
 INT L16;
 CHAR L171_;
 CHAR L18;
 BOOL L191_;
 BOOL L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 extern STR Lastst301842070;
 FSTR L25;
 dSTR L26;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 STR L30;
 INT L31;
 INT L321_;
 INT L33;
 CHAR L341_;
 CHAR L35;
 BOOL L361_;
 extern STR framestate1;
 FSTR L37;
 dSTR L38;
 BOOL L39;
 BOOL L401_;
 extern STR returndummy;
 FSTR L42;
 dSTR L43;
 extern STR return5;
 FSTR L44;
 dSTR L45;
 INT L46;
 INT L471_;
 extern STR name4;
 FSTR L48;
 dSTR L49;
 INT L50;
 BOOL L511_;
 BOOL L52;
 BOOL L531_;
 extern STR WARNINGfunction;
 FILE1 L54;
 OB L55;
 FILE1 L57;
 OB L58;
 extern STR haswrongindent;
 FILE1 L60;
 OB L61;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 extern STR name9;
 FILE1 L68;
 OB L69;
 L1 = (ATTR(ATTR(self,current_am_rout_),specul_prefetch)==((AM_LOCAL_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  ndefer_self = self;
  plus_self = ((STR) &PREFET1824934344);
  plus_sarg = CGEN_e1639504572(self, ((dAM_EXPR) ATTR(ATTR(self,current_am_rout_),specul_prefetch)));
  ret_val = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val;
  plus_sarg1 = ((STR) &name76);
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s = ret_val1;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 L3 = ATTR(self,saw_outer_return);
 L41_=!(L3); 
 if (L41_) {
  CGEN_c9785167(self);
 }
 if (ATTR(self,chk_return)) {
  L8 = ATTR(self,saw_outer_return);
  L91_=!(L8); 
  L7 = L91_;
 } else {
  L7 = FALSE;
 }
 if (L7) {
  is_iter_self = f;
  is_iter_self1 = ATTR(is_iter_self,sig1);
  is_iter_self2 = ATTR(is_iter_self1,name1);
  L11 = (is_iter_self2.str==((STR) NULL));
  L121_=!(L11); 
  if (L121_) {
   L13 = is_iter_self2.str;
   L14 = STR_sizerINT(is_iter_self2.str);
   L151_=INTMINUS(L14,1); 
   L16 = L151_;
   L171_=ARR((STR)L13,L16); 
   L18 = L171_;
   L191_=L18=='!'; 
   L10 = L191_;
  } else {
   L10 = FALSE;
  }
  ret_val4 = L10;
  ret_val3 = ret_val4;
  ret_val2 = ret_val3;
  L20 = ret_val2;
  L221_=!(L20); 
  L6 = L221_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  L23 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
  L241_=!(L23); 
  L5 = L241_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  CGEN_r878823253(self, ((STR) &Lastst301842070), ((dAM) f));
 }
 plus_self2 = ATTR(self,code_c);
 plus_s = ((dSTR) ATTR(self,routine_code));
 L25 = ATTR(plus_self2,ntext);
 L26 = plus_s;
 SATTR(plus_self2,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 is_iter_self3 = f;
 is_iter_self4 = ATTR(is_iter_self3,sig1);
 is_iter_self5 = ATTR(is_iter_self4,name1);
 L28 = (is_iter_self5.str==((STR) NULL));
 L291_=!(L28); 
 if (L291_) {
  L30 = is_iter_self5.str;
  L31 = STR_sizerINT(is_iter_self5.str);
  L321_=INTMINUS(L31,1); 
  L33 = L321_;
  L341_=ARR((STR)L30,L33); 
  L35 = L341_;
  L361_=L35=='!'; 
  L27 = L361_;
 } else {
  L27 = FALSE;
 }
 ret_val7 = L27;
 ret_val6 = ret_val7;
 ret_val5 = ret_val6;
 if (ret_val5) {
  CGEN_newline(self);
  plus_self3 = ATTR(self,code_c);
  plus_s1 = ((dSTR) ((STR) &framestate1));
  L37 = ATTR(plus_self3,ntext);
  L38 = plus_s1;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
  L39 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
  L401_=!(L39); 
  if (L401_) {
   CGEN_newline(self);
   plus_self4 = ATTR(self,code_c);
   plus_s2 = ((dSTR) ((STR) &returndummy));
   L42 = ATTR(plus_self4,ntext);
   L43 = plus_s2;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L42, (*dSTR_strrSTR[TAG(L43)])(L43)));
  }
  else {
   CGEN_newline(self);
   plus_self5 = ATTR(self,code_c);
   plus_s3 = ((dSTR) ((STR) &return5));
   L44 = ATTR(plus_self5,ntext);
   L45 = plus_s3;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L44, (*dSTR_strrSTR[TAG(L45)])(L45)));
  }
 }
 move_out_self = self;
 L46 = ATTR(move_out_self,indent);
 L471_=INTMINUS(L46,1); 
 SATTR(move_out_self,indent,L471_);
 CGEN_newline(self);
 plus_self6 = ATTR(self,code_c);
 plus_s4 = ((dSTR) ((STR) &name4));
 L48 = ATTR(plus_self6,ntext);
 L49 = plus_s4;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
 L50 = ATTR(self,indent);
 L511_=(L50)==(0); 
 L52 = L511_;
 L531_=!(L52); 
 if (L531_) {
  create_self = ((OUT) NULL);
  ret_val8 = create_self;
  plus_self7 = ret_val8;
  plus_s5 = ((STR) &WARNINGfunction);
  stdout_self = ((FILE1) NULL);
  L55=ZALLOC(sizeof(struct FILE1_struct));
  if (L55==NULL) FATAL("Unable to allocate more memory");
  ((OB)L55)->header.tag=FILE1_tag;
  L54 = ((FILE1) L55);
  r = L54;
  
  SATTR(r,fp,stdout);
  ret_val10 = r;
  FILE_plus_STR(ret_val10, plus_s5);
  ret_val9 = plus_self7;
  plus_self8 = ret_val9;
  plus_s6 = ATTR(ATTR(f,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L58=ZALLOC(sizeof(struct FILE1_struct));
  if (L58==NULL) FATAL("Unable to allocate more memory");
  ((OB)L58)->header.tag=FILE1_tag;
  L57 = ((FILE1) L58);
  r1 = L57;
  
  SATTR(r1,fp,stdout);
  ret_val12 = r1;
  FILE_plus_STR(ret_val12, plus_s6);
  ret_val11 = plus_self8;
  plus_self9 = ret_val11;
  plus_s7 = ((STR) &haswrongindent);
  stdout_self2 = ((FILE1) NULL);
  L61=ZALLOC(sizeof(struct FILE1_struct));
  if (L61==NULL) FATAL("Unable to allocate more memory");
  ((OB)L61)->header.tag=FILE1_tag;
  L60 = ((FILE1) L61);
  r2 = L60;
  
  SATTR(r2,fp,stdout);
  ret_val14 = r2;
  FILE_plus_STR(ret_val14, plus_s7);
  ret_val13 = plus_self9;
  plus_self10 = ret_val13;
  L64=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L64==NULL) FATAL("Unable to allocate more memory");
  memset(L64,0,sizeof(struct INT_boxed_struct));
  ((OB)L64)->header.tag=INT_tag;
  L63 = (dSTR)((INT_boxed) L64);
  ((INT_boxed) L63)->immutable_part = ATTR(self,indent);
  plus_s8 = L63;
  stdout_self3 = ((FILE1) NULL);
  L66=ZALLOC(sizeof(struct FILE1_struct));
  if (L66==NULL) FATAL("Unable to allocate more memory");
  ((OB)L66)->header.tag=FILE1_tag;
  L65 = ((FILE1) L66);
  r3 = L65;
  
  SATTR(r3,fp,stdout);
  ret_val16 = r3;
  FILE_plus_dSTR(ret_val16, plus_s8);
  ret_val15 = plus_self10;
  plus_self11 = ret_val15;
  plus_s9 = ((STR) &name9);
  stdout_self4 = ((FILE1) NULL);
  L69=ZALLOC(sizeof(struct FILE1_struct));
  if (L69==NULL) FATAL("Unable to allocate more memory");
  ((OB)L69)->header.tag=FILE1_tag;
  L68 = ((FILE1) L69);
  r4 = L68;
  
  SATTR(r4,fp,stdout);
  ret_val17 = r4;
  FILE_plus_STR(ret_val17, plus_s9);
 }
 if (ATTR(self,func_tables)) {
  PRINT_721582855(((PRINT_OB) NULL), ATTR(self,prog), self, ATTR(self,code_c), f, ATTR(self,current_iter_out));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e881275767(CGEN self, AM_PROTECT_STMT arg) {
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN move_out_self;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self1;
 AM_EXCEPT_EXPR create_self;
 dTP create_tp;
 AM_EXCEPT_EXPR ret_val;
 AM_EXCEPT_EXPR r;
 CGEN in_self2;
 CGEN move_out_self1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN move_out_self2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN move_out_self3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 extern STR PROTECT_BEGIN1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 extern STR PROTECT_WHEN1;
 INT L9;
 INT L101_;
 AM_EXCEPT_EXPR L11;
 OB L12;
 BOOL L13;
 BOOL L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 extern STR RAISEEXCEPTION;
 INT L19;
 INT L201_;
 extern STR name4;
 INT L22;
 INT L231_;
 extern STR PROTECT_END1;
 ndefer_self = self;
 ndefer_s = ((STR) &PROTECT_BEGIN1);
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 in_self = self;
 L1 = ATTR(in_self,indent);
 L21_=INTPLUS(L1,1); 
 SATTR(in_self,indent,L21_);
 L3 = ATTR(self,ex_nesting);
 L41_=INTPLUS(L3,1); 
 SATTR(self,ex_nesting,L41_);
 CGEN_e494460490(self, ATTR(arg,body));
 L5 = ATTR(self,ex_nesting);
 L61_=INTMINUS(L5,1); 
 SATTR(self,ex_nesting,L61_);
 move_out_self = self;
 L7 = ATTR(move_out_self,indent);
 L81_=INTMINUS(L7,1); 
 SATTR(move_out_self,indent,L81_);
 ndefer_self1 = self;
 ndefer_s1 = ((STR) &PROTECT_WHEN1);
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 in_self1 = self;
 L9 = ATTR(in_self1,indent);
 L101_=INTPLUS(L9,1); 
 SATTR(in_self1,indent,L101_);
 create_self = ((AM_EXCEPT_EXPR) NULL);
 create_tp = ((dTP) TP_BUI1325635093);
 L12=ZALLOC(sizeof(struct AM_EXCEPT_EXPR_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_EXCEPT_EXPR_tag;
 L11 = ((AM_EXCEPT_EXPR) L12);
 r = L11;
 SATTR(r,tp_at,create_tp);
 ret_val = r;
 CGEN_e2137132086(self, ((dAM_EXPR) ret_val), ATTR(arg,tgts), ATTR(arg,stmts));
 L13 = ATTR(arg,no_else);
 L141_=!(L13); 
 if (L141_) {
  in_self2 = self;
  L15 = ATTR(in_self2,indent);
  L161_=INTPLUS(L15,1); 
  SATTR(in_self2,indent,L161_);
  CGEN_e494460490(self, ATTR(arg,else_stmts));
  move_out_self1 = self;
  L17 = ATTR(move_out_self1,indent);
  L181_=INTMINUS(L17,1); 
  SATTR(move_out_self1,indent,L181_);
 }
 else {
  ndefer_self2 = self;
  ndefer_s2 = ((STR) &RAISEEXCEPTION);
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 move_out_self2 = self;
 L19 = ATTR(move_out_self2,indent);
 L201_=INTMINUS(L19,1); 
 SATTR(move_out_self2,indent,L201_);
 ndefer_self3 = self;
 ndefer_s3 = ((STR) &name4);
 CGEN_d695831906(ndefer_self3);
 SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 move_out_self3 = self;
 L22 = ATTR(move_out_self3,indent);
 L231_=INTMINUS(L22,1); 
 SATTR(move_out_self3,indent,L231_);
 ndefer_self4 = self;
 ndefer_s4 = ((STR) &PROTECT_END1);
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g628546529(CGEN self, AM_ROUT_DEF rout_def, ARRAYSTR* arg_list, ARRAYSTR* type_list) {
 ARRAYSTR L1 = ((ARRAYSTR) NULL);/*Local for arg*/
 ARRAYSTR L2 = ((ARRAYSTR) NULL);/*Local for arg*/
 INT num_args = INT_zero;
 SIG sig1;
 INT arg_pos = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i = INT_zero;
 AM_FORMAL_ARG arg;
 STR arg_name = ((STR) NULL);
 AM_LOCAL_EXPR le;
 dTP tp;
 AM_LOCAL_EXPR expr11;
 AM_LOCAL_EXPR lv;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val1;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val2;
 CGEN c_type_for_arg_s;
 AM_FORMAL_ARG c_type_for_arg_a;
 STR ret_val3;
 STR str;
 AM_FORMAL_ARG tp_self1;
 dTP ret_val4;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val5;
 CGEN c_type_for_arg_s1;
 dTP c_type_for_arg_t;
 STR ret_val6;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val7;
 STR plus_self;
 STR plus_sarg;
 STR ret_val8;
 AM_LOCAL_EXPR tp_self3;
 dTP ret_val9;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val10;
 AM_LOCAL_EXPR r;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns;
 STR ret_val11;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns1;
 STR ret_val12;
 ARRAYARG L5;
 INT L61_;
 ARRAYSTR L7;
 INT L8;
 OB L9;
 INT L10;
 ARRAYSTR L11;
 INT L12;
 OB L13;
 INT L14;
 BOOL L151_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L161_,L161_m;
 AM_FORMAL_ARG L211_;
 AM_FORMAL_ARG L221_;
 AM_FORMAL_ARG L23;
 dMODE L24;
 extern STR name58;
 STR L25;
 dMODE L27;
 AM_LOCAL_EXPR L28;
 OB L29;
 STR L30;
 STR L33;
 INT L351_;
 sig1 = ATTR(rout_def,sig1);
 L5 = ATTR(sig1,args);
 L61_=(L5)==NULL?0:ASIZE((ARRAYARG)L5); 
 num_args = L61_;
 create_self = ((ARRAYSTR) NULL);
 create_n = num_args;
 L8 = create_n;
 L10=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L8)*sizeof(STR);
 L9=ZALLOC_BIG(L10);
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L9)->header.destroyed=0;
#endif

 L7 = ((ARRAYSTR) L9);
 L7->asize = L8;
 ret_val = L7;
 L1 = ret_val;
 create_self1 = ((ARRAYSTR) NULL);
 create_n1 = num_args;
 L12 = create_n1;
 L14=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L12)*sizeof(STR);
 L13=ZALLOC_BIG(L14);
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L13)->header.destroyed=0;
#endif

 L11 = ((ARRAYSTR) L13);
 L11->asize = L12;
 ret_val1 = L11;
 L2 = ret_val1;
 arg_pos = 0;
 L151_=(0)<(num_args); 
 if (L151_) {
  {
   BOOL f_L161_ = TRUE;
   while (1) {
    if (f_L161_) {
     f_L161_ = FALSE;
     L31 = 1;
     L181_=(rout_def)==NULL?0:ASIZE((AM_ROUT_DEF)rout_def); 
     L19 = L181_;
     L201_=INTMINUS(L19,1); 
     L41 = L201_;
     L161_=L31-1;L161_m=L41; 
    }
    if(L161_++>=L161_m)  goto after_loop; 
    i = L161_;
    L211_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)rout_def,i); 
    arg = L211_;
    le = ATTR(arg,expr);
    arg_name = ATTR(le,name1).str;
    L221_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)rout_def,i); 
    L23=L221_;
    tp_self = ATTR(L23,expr);
    ret_val2 = ATTR(tp_self,tp_at);
    tp = ret_val2;
    c_type_for_arg_s = self;
    c_type_for_arg_a = arg;
    c_type_for_arg_s1 = c_type_for_arg_s;
    tp_self1 = c_type_for_arg_a;
    tp_self2 = ATTR(tp_self1,expr);
    ret_val5 = ATTR(tp_self2,tp_at);
    ret_val4 = ret_val5;
    c_type_for_arg_t = ret_val4;
    mang_self = c_type_for_arg_s1;
    mang_ob = ((OB) c_type_for_arg_t);
    ret_val7 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
    ret_val6 = ret_val7;
    str = ret_val6;
    L24 = ATTR(c_type_for_arg_a,mode1);
    if ((*dMODE_2144735300[TAG(L24)])(L24)) {
     plus_self = str;
     plus_sarg = ((STR) &name58);
     ret_val8 = STR_ap2004550586(plus_self, plus_sarg);
     str = ret_val8;
    }
    ret_val3 = str;
    L25 = ret_val3;
    SARR((ARRAYSTR)L2,arg_pos,(STR)L25); 
    ;
    L27 = ATTR(arg,mode1);
    if ((*dMODE_2144735300[TAG(L27)])(L27)) {
     expr11 = ATTR(arg,expr);
     create_self2 = ((AM_LOCAL_EXPR) NULL);
     create_src = ATTR(expr11,source1);
     create_name = ATTR(expr11,name1);
     tp_self3 = expr11;
     ret_val9 = ATTR(tp_self3,tp_at);
     create_tp = ret_val9;
     create_as_tp = ATTR(expr11,as_type);
     L29=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
     if (L29==NULL) FATAL("Unable to allocate more memory");
     ((OB)L29)->header.tag=AM_LOCAL_EXPR_tag;
     L28 = ((AM_LOCAL_EXPR) L29);
     r = L28;
     SATTR(r,source1,create_src);
     SATTR(r,name1,create_name);
     SATTR(r,tp_at,create_tp);
     SATTR(r,as_type,create_as_tp);
     ret_val10 = r;
     lv = ret_val10;
     mang_self1 = self;
     mang_ob1 = ((OB) lv);
     mang_ns = ((OB) sig1);
     ret_val11 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns);
     L30 = ret_val11;
     SARR((ARRAYSTR)L1,arg_pos,(STR)L30); 
     ;
    }
    else {
     mang_self2 = self;
     mang_ob2 = ((OB) ATTR(arg,expr));
     mang_ns1 = ((OB) sig1);
     ret_val12 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns1);
     L33 = ret_val12;
     SARR((ARRAYSTR)L1,arg_pos,(STR)L33); 
     ;
    }
    L351_=INTPLUS(arg_pos,1); 
    arg_pos = L351_;
   }
  }
  after_loop: ;
 }
 *arg_list = L1;
 *type_list = L2;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_r878823253(CGEN self, STR s, dAM am) {
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 CGEN runtime_error_se;
 STR runtime_error_s;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val4;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val5;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val6;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val7;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val8;
 CGEN runtime_error_se1;
 STR runtime_error_s1;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val9;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val10;
 CGEN ndefer_self1;
 STR ndefer_s1;
 extern STR nin;
 extern STR n21;
 extern STR FATAL1;
 extern STR name95;
 extern STR nin;
 extern STR n21;
 dAM L1;
 extern STR FATAL1;
 extern STR name95;
 if (ATTR(ATTR(self,prog),chk_no_line_numb)) {
  runtime_error_se = self;
  plus_self = s;
  plus_sarg = ((STR) &nin);
  ret_val = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val;
  plus_sarg1 = ATTR(self,current_function);
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val1;
  plus_sarg2 = ((STR) &n21);
  ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
  runtime_error_s = ret_val2;
  ndefer_self = runtime_error_se;
  plus_self3 = ((STR) &FATAL1);
  plus_sarg3 = runtime_error_s;
  ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val3;
  plus_sarg4 = ((STR) &name95);
  ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
  ndefer_s = ret_val4;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 else {
  runtime_error_se1 = self;
  plus_self5 = s;
  plus_sarg5 = ((STR) &nin);
  ret_val5 = STR_ap2004550586(plus_self5, plus_sarg5);
  plus_self6 = ret_val5;
  plus_sarg6 = ATTR(self,current_function);
  ret_val6 = STR_ap2004550586(plus_self6, plus_sarg6);
  plus_self7 = ret_val6;
  plus_sarg7 = ((STR) &n21);
  ret_val7 = STR_ap2004550586(plus_self7, plus_sarg7);
  plus_self8 = ret_val7;
  L1 = am;
  plus_sarg8 = SFILE_ID_strrSTR((*dAM_so1457869664[TAG(L1)])(L1));
  ret_val8 = STR_ap2004550586(plus_self8, plus_sarg8);
  runtime_error_s1 = ret_val8;
  ndefer_self1 = runtime_error_se1;
  plus_self9 = ((STR) &FATAL1);
  plus_sarg9 = runtime_error_s1;
  ret_val9 = STR_ap2004550586(plus_self9, plus_sarg9);
  plus_self10 = ret_val9;
  plus_sarg10 = ((STR) &name95);
  ret_val10 = STR_ap2004550586(plus_self10, plus_sarg10);
  ndefer_s1 = ret_val10;
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 }
}
