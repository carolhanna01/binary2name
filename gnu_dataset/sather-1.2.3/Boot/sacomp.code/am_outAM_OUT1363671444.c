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

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct ARRAYdAM_CONST_struct {/* layout for ARRAY{$AM_CONST} */
 OB_HEADER header;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *ARRAYdAM_CONST;

typedef struct ARRAYd198808957_frame_struct {
 INT state;
 ARRAYdAM_CONST self;/* Formal argument: self */
 dAM_CONST ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd198808957_frame;

typedef struct ELT_NILIDENT_struct {/* layout for ELT_NIL{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILIDENT;

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

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct TUPIDENTdTP_struct {/* layout for TUP{IDENT,$TP} */
 struct dTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTdTP;
static TUPIDENTdTP TUPIDENTdTP_zero;

typedef struct TUPIDENTdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTdTP immutable_part;
 } *TUPIDENTdTP_boxed;

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

typedef struct AM_ARR1990213312_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR1990213312_frame;

typedef struct AM_ARR_CONST_struct {/* layout for AM_ARR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct ARRAYdAM_CONST_struct *elts;
 struct SFILE_ID_struct source1;
 struct TP_CLASS_struct *elt_tp;
 } *AM_ARR_CONST;

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

typedef struct AM_BND1640868266_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1640868266_frame;

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

typedef struct AM_BND745688768_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND745688768_frame;

typedef struct AM_BND83643009_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND83643009_frame;

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

typedef struct AM_BND1037254841_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1037254841_frame;

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

typedef struct AM_EXT150734657_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT150734657_frame;

typedef struct AM_EXT1469221262_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT1469221262_frame;

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

typedef struct AM_ITE35020325_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE35020325_frame;

typedef struct AM_ITE1159913222_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE1159913222_frame;

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

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

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

typedef struct AM_ROU2113525054_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU2113525054_frame;

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

typedef struct AM_YIELD_STMT_struct {/* layout for AM_YIELD_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 INT ret;
 struct SFILE_ID_struct source1;
 } *AM_YIELD_STMT;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

typedef struct ARRAYI180735351_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI180735351_frame;

typedef struct FLISTdAM_CONST_struct {/* layout for FLIST{$AM_CONST} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *FLISTdAM_CONST;

typedef struct FLISTd617209260_frame_struct {
 INT state;
 FLISTdAM_CONST self;/* Formal argument: self */
 dAM_CONST ret_val1;
 INT i;
 INT sz;
 FLISTdAM_CONST aget_self;
 INT aget_ind;
 dAM_CONST ret_val;
 } *FLISTd617209260_frame;

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

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPID1741388334_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val2;
 FMAPIDENTdTP L61;
 TUPIDENTdTP r;
 INT L31;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID1741388334_frame;

#include "tags.h"

/* Globals */


/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST STR (**dMODE_strrSTR)(dMODE);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
AM_CALL_ARG AM_BND745688768(AM_BND745688768_frame);
AM_CALL_ARG AM_BND956869000(AM_BND956869000_frame);
AM_CALL_ARG AM_EXT150734657(AM_EXT150734657_frame);
AM_CALL_ARG AM_ITE35020325(AM_ITE35020325_frame);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
INT AM_ARR1990213312(AM_ARR1990213312_frame);
INT AM_BND1037254841(AM_BND1037254841_frame);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT AM_BND83643009(AM_BND83643009_frame);
INT AM_EXT1469221262(AM_EXT1469221262_frame);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT AM_ROU2113525054(AM_ROU2113525054_frame);
INT ARRAYI180735351(ARRAYI180735351_frame);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTF873512073(FLISTF7594819);
INT FLISTd895185163(FLISTdTP);
INT INT_up418511718(INT_up418511718_frame);
STR STR_se405450305(STR_se405450305_frame);
STR TP_CLASS_strrSTR(TP_CLASS);
TUPIDENTdTP FMAPID1741388334(FMAPID1741388334_frame);
dAM_CONST ARRAYd198808957(ARRAYd198808957_frame);
dAM_CONST FLISTd617209260(FLISTd617209260_frame);
dAM_EXPR AM_ARR535890289(AM_ARR535890289_frame);
void AM_OUT101602186(AM_OUT, AM_LOOP_STMT, INT, INT);
void AM_OUT1191840136(AM_OUT, AM_ASSERT_STMT, INT, INT);
void AM_OUT1250601520(AM_OUT, AM_IF_STMT, INT, INT);
void AM_OUT1341727046(AM_OUT, AM_POST_STMT, INT, INT);
void AM_OUT1365214533(AM_OUT, AM_FORMAL_ARG, INT, INT);
void AM_OUT1399375684(AM_OUT, AM_ASSIGN_STMT, INT, INT);
void AM_OUT1439666796(AM_OUT, dAM);
void AM_OUT1468881370(AM_OUT, AM_STMT_EXPR, INT, INT);
void AM_OUT1496755235(AM_OUT, AM_YIELD_STMT, INT, INT);
void AM_OUT1555944202(AM_OUT, AM_CASE_STMT, INT, INT);
void AM_OUT1614972293(AM_OUT, AM_BND260301329, INT, INT);
void AM_OUT1623210178(AM_OUT, AM_TYPECASE_STMT, INT, INT);
void AM_OUT1677535099(AM_OUT, AM_LOCAL_EXPR, INT, INT);
void AM_OUT1813430155(AM_OUT, AM_BND367211769, INT, INT);
void AM_OUT1924306007(AM_OUT, AM_ARRAY_EXPR, INT, INT);
void AM_OUT1997333258(AM_OUT, AM_ATTR_EXPR, INT, INT);
void AM_OUT2087454067(AM_OUT, INT);
void AM_OUT244025178(AM_OUT, AM_VAT319982528, INT, INT);
void AM_OUT347621831(AM_OUT, AM_VAR744470097, INT, INT);
void AM_OUT359547257(AM_OUT, AM_PRE_STMT, INT, INT);
void AM_OUT376898766(AM_OUT, AM_EXT_CALL_EXPR, INT, INT);
void AM_OUT391785759(AM_OUT, AM_OB_DEF, INT, INT);
void AM_OUT41050653(AM_OUT, AM_ROU1916046290, INT, INT);
void AM_OUT435235740(AM_OUT, AM_ATTACH_STMT, INT, INT);
void AM_OUT523288876(AM_OUT, AM_BND1124877163, INT, INT);
void AM_OUT600226455(AM_OUT, AM_CALL_ARG, INT, INT);
void AM_OUT671570827(AM_OUT, AM_PROTECT_STMT, INT, INT);
void AM_OUT726258064(AM_OUT, AM_SHARED_EXPR, INT, INT);
void AM_OUT732629843(AM_OUT, AM_ITE1809135850, INT, INT);
void AM_OUT786574336(AM_OUT, AM_RETURN_STMT, INT, INT);
void AM_OUT796864830(AM_OUT, AM_ARR_CONST, INT, INT);
void AM_OUT797583920(AM_OUT, AM_EXCEPT_EXPR, INT, INT);
void AM_OUT860401336(AM_OUT, dAM, INT, INT);
void AM_OUT930898334(AM_OUT, AM_EXPR_STMT, INT, INT);
void AM_OUT959054340(AM_OUT, AM_IF_EXPR, INT, INT);
void AM_OUT991874317(AM_OUT, AM_ROUT_DEF, INT, INT);
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

void AM_OUT101602186(AM_OUT self, AM_LOOP_STMT a, INT first1, INT indent11) {
 FLISTA1062334999 L11;
 STR L21;
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
 extern STR has_yieldtrue;
 FILE1 L3;
 OB L4;
 extern STR has_yieldfalse;
 FILE1 L6;
 OB L7;
 extern STR name9;
 FILE1 L9;
 OB L10;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 extern STR its1;
 FILE1 L16;
 OB L17;
 extern STR name3;
 STR L19;
 AM_ITE1809135850 L20;
 AM_ITE1809135850 L22;
 FILE1 L23;
 OB L24;
 extern STR name9;
 FILE1 L26;
 OB L27;
 INT L291_;
 extern STR init2;
 FILE1 L30;
 OB L31;
 dAM_STMT L33;
 INT L341_;
 dAM_STMT L35;
 INT L361_;
 INT L37;
 INT L381_;
 if (ATTR(a,has_yield)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &has_yieldtrue);
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
 }
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &has_yieldfalse);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L13 = (ATTR(a,its1)==((FLISTA1062334999) NULL));
 L141_=!(L13); 
 if (L141_) {
  L151_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L151_);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &its1);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  {
   struct FLISTA20621028_frame_struct other1_0;
FLISTA20621028_frame noname1 = &other1_0;
   struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
   L11 = ATTR(a,its1);
   L21 = ((STR) &name3);
   noname1->self = L11;
   noname1->state = 0;
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    create_self4 = ((OUT) NULL);
    ret_val8 = create_self4;
    plus_self4 = ret_val8;
    L20 = FLISTA20621028(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    L22=L20;
    noname2->arg1 = ((dSTR) ATTR(ATTR(L22,fun),str));
    L19 = STR_se405450305(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    plus_s4 = L19;
    stdout_self4 = ((FILE1) NULL);
    L24=ZALLOC(sizeof(struct FILE1_struct));
    if (L24==NULL) FATAL("Unable to allocate more memory");
    ((OB)L24)->header.tag=FILE1_tag;
    L23 = ((FILE1) L24);
    r4 = L23;
    
    SATTR(r4,fp,stdout);
    ret_val9 = r4;
    FILE_plus_STR(ret_val9, plus_s4);
   }
  }
  after_loop: ;
  create_self5 = ((OUT) NULL);
  ret_val10 = create_self5;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &name9);
  stdout_self5 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r5 = L26;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 L291_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L291_);
 create_self6 = ((OUT) NULL);
 ret_val12 = create_self6;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &init2);
 stdout_self6 = ((FILE1) NULL);
 L31=ZALLOC(sizeof(struct FILE1_struct));
 if (L31==NULL) FATAL("Unable to allocate more memory");
 ((OB)L31)->header.tag=FILE1_tag;
 L30 = ((FILE1) L31);
 r6 = L30;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 L33 = ATTR(a,init);
 L341_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L33), 0, L341_);
 L35 = ATTR(a,body);
 L361_=INTPLUS(indent11,3); 
 L37 = L361_;
 L381_=INTPLUS(indent11,3); 
 AM_OUT860401336(self, ((dAM) L35), L37, L381_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1191840136(AM_OUT self, AM_ASSERT_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR test2;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &test2);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,test1);
 L91_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1250601520(AM_OUT self, AM_IF_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR test11;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 INT L131_;
 extern STR if_true;
 FILE1 L14;
 OB L15;
 dAM_STMT L17;
 INT L181_;
 INT L191_;
 extern STR if_false;
 FILE1 L20;
 OB L21;
 dAM_STMT L23;
 INT L241_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &test11);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,test1);
 L91_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &if_true);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L17 = ATTR(a,if_true);
 L181_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L17), 0, L181_);
 L191_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L191_);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &if_false);
 stdout_self4 = ((FILE1) NULL);
 L21=ZALLOC(sizeof(struct FILE1_struct));
 if (L21==NULL) FATAL("Unable to allocate more memory");
 ((OB)L21)->header.tag=FILE1_tag;
 L20 = ((FILE1) L21);
 r4 = L20;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L23 = ATTR(a,if_false);
 L241_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L23), 0, L241_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1341727046(AM_OUT self, AM_POST_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR test2;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &test2);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,test1);
 L91_=INTPLUS(indent11,9); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1365214533(AM_OUT self, AM_FORMAL_ARG a, INT first1, INT indent11) {
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
 extern STR name52;
 FILE1 L1;
 OB L2;
 extern STR mode11;
 FILE1 L4;
 OB L5;
 dMODE L7;
 FILE1 L8;
 OB L9;
 extern STR expr2;
 FILE1 L11;
 OB L12;
 AM_LOCAL_EXPR L14;
 INT L151_;
 extern STR name26;
 FILE1 L16;
 OB L17;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name52);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &mode11);
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
 L7 = ATTR(a,mode1);
 plus_s2 = (*dMODE_strrSTR[TAG(L7)])(L7);
 stdout_self2 = ((FILE1) NULL);
 L9=ZALLOC(sizeof(struct FILE1_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=FILE1_tag;
 L8 = ((FILE1) L9);
 r2 = L8;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &expr2);
 stdout_self3 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r3 = L11;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L14 = ATTR(a,expr);
 L151_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L14), 0, L151_);
 create_self3 = ((OUT) NULL);
 ret_val8 = create_self3;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name26);
 stdout_self4 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r4 = L16;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1399375684(AM_OUT self, AM_ASSIGN_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR dest;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 INT L131_;
 extern STR src1;
 FILE1 L14;
 OB L15;
 dAM_EXPR L17;
 INT L181_;
 extern STR name9;
 FILE1 L19;
 OB L20;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &dest);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,dest);
 L91_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &src1);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L17 = ATTR(a,src1);
 L181_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L17), 0, L181_);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r4 = L19;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1468881370(AM_OUT self, AM_STMT_EXPR a, INT first1, INT indent11) {
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
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 AM_STMT_EXPR is_near_self;
 BOOL ret_val11 = BOOL_zero;
 OUT plus_self5;
 dSTR plus_s5;
 OUT ret_val12;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR voidstmts;
 FILE1 L5;
 OB L6;
 extern STR stmts1;
 FILE1 L8;
 OB L9;
 dAM_STMT L11;
 INT L121_;
 INT L131_;
 extern STR voidexpr;
 FILE1 L14;
 OB L15;
 extern STR is_near4;
 FILE1 L17;
 OB L18;
 dAM_EXPR L20;
 dSTR L21;
 OB L22;
 FILE1 L23;
 OB L24;
 extern STR name9;
 FILE1 L26;
 OB L27;
 INT L291_;
 extern STR expr1;
 FILE1 L30;
 OB L32;
 dAM_EXPR L34;
 INT L351_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 if ((ATTR(a,stmts)==((dAM_STMT) NULL))) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &voidstmts);
  stdout_self1 = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r1 = L5;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 else {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &stmts1);
  stdout_self2 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r2 = L8;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  L11 = ATTR(a,stmts);
  L121_=INTPLUS(indent11,9); 
  AM_OUT860401336(self, ((dAM) L11), 0, L121_);
 }
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 if ((ATTR(a,expr)==((dAM_EXPR) NULL))) {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &voidexpr);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 else {
  create_self4 = ((OUT) NULL);
  ret_val8 = create_self4;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &is_near4);
  stdout_self4 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r4 = L17;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  ret_val9 = plus_self4;
  plus_self5 = ret_val9;
  is_near_self = a;
  L20 = ATTR(is_near_self,expr);
  ret_val11 = (*dAM_EX1117610980[TAG(L20)])(L20);
  L22=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  memset(L22,0,sizeof(struct BOOL_boxed_struct));
  ((OB)L22)->header.tag=BOOL_tag;
  L21 = (dSTR)((BOOL_boxed) L22);
  ((BOOL_boxed) L21)->immutable_part = ret_val11;
  plus_s5 = L21;
  stdout_self5 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r5 = L23;
  
  SATTR(r5,fp,stdout);
  ret_val13 = r5;
  FILE_plus_dSTR(ret_val13, plus_s5);
  ret_val12 = plus_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name9);
  stdout_self6 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r6 = L26;
  
  SATTR(r6,fp,stdout);
  ret_val14 = r6;
  FILE_plus_STR(ret_val14, plus_s6);
  L291_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L291_);
  create_self5 = ((OUT) NULL);
  ret_val15 = create_self5;
  plus_self7 = ret_val15;
  plus_s7 = ((STR) &expr1);
  stdout_self7 = ((FILE1) NULL);
  L32=ZALLOC(sizeof(struct FILE1_struct));
  if (L32==NULL) FATAL("Unable to allocate more memory");
  ((OB)L32)->header.tag=FILE1_tag;
  L30 = ((FILE1) L32);
  r7 = L30;
  
  SATTR(r7,fp,stdout);
  ret_val16 = r7;
  FILE_plus_STR(ret_val16, plus_s7);
  L34 = ATTR(a,expr);
  L351_=INTPLUS(indent11,9); 
  AM_OUT860401336(self, ((dAM) L34), 0, L351_);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1496755235(AM_OUT self, AM_YIELD_STMT a, INT first1, INT indent11) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 OUT ret_val1;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT plus_self1;
 dSTR plus_s1;
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
 INT L11_;
 extern STR ret1;
 FILE1 L2;
 OB L3;
 dSTR L5;
 OB L6;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L12;
 INT L141_;
 extern STR val1;
 FILE1 L15;
 OB L16;
 dAM_EXPR L18;
 INT L191_;
 extern STR name9;
 FILE1 L20;
 OB L21;
 L11_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L11_);
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &ret1);
 stdout_self = ((FILE1) NULL);
 L3=ZALLOC(sizeof(struct FILE1_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=FILE1_tag;
 L2 = ((FILE1) L3);
 r = L2;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 L6=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,sizeof(struct INT_boxed_struct));
 ((OB)L6)->header.tag=INT_tag;
 L5 = (dSTR)((INT_boxed) L6);
 ((INT_boxed) L5)->immutable_part = ATTR(a,ret);
 plus_s1 = L5;
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
 create_self1 = ((OUT) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L10 = ((FILE1) L12);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L141_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L141_);
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &val1);
 stdout_self3 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r3 = L15;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L18 = ATTR(a,val1);
 L191_=INTPLUS(indent11,7); 
 AM_OUT860401336(self, ((dAM) L18), 0, L191_);
 create_self3 = ((OUT) NULL);
 ret_val8 = create_self3;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L21=ZALLOC(sizeof(struct FILE1_struct));
 if (L21==NULL) FATAL("Unable to allocate more memory");
 ((OB)L21)->header.tag=FILE1_tag;
 L20 = ((FILE1) L21);
 r4 = L20;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1555944202(AM_OUT self, AM_CASE_STMT a, INT first1, INT indent11) {
 INT i = INT_zero;
 INT j = INT_zero;
 FLISTdAM_CONST L11;
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
 FLISTF7594819 aget_self;
 INT aget_ind = INT_zero;
 FLISTdAM_CONST ret_val8;
 OUT create_self4;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val11;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val13;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val15;
 OUT create_self7;
 OUT ret_val16;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 extern STR test2;
 FILE1 L2;
 OB L3;
 dAM_EXPR L5;
 INT L61_;
 extern STR name9;
 FILE1 L7;
 OB L8;
 INT L101_;
 extern STR branches;
 FILE1 L12;
 OB L13;
 INT L15;
 BOOL L161_;
 INT L171_;
 extern STR cond;
 FILE1 L18;
 OB L19;
 dAM_CONST L21;
 FLISTdAM_CONST L221_;
 extern STR name29;
 FILE1 L23;
 OB L24;
 extern STR name9;
 FILE1 L26;
 OB L27;
 INT L291_;
 extern STR code3;
 FILE1 L30;
 OB L31;
 dAM_STMT L331_;
 dAM_STMT L34;
 INT L351_;
 INT L361_;
 INT L371_;
 extern STR else_stmts;
 FILE1 L38;
 OB L39;
 dAM_STMT L42;
 INT L431_;
 AM_OUT2087454067(self, indent11);
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &test2);
 stdout_self = ((FILE1) NULL);
 L3=ZALLOC(sizeof(struct FILE1_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=FILE1_tag;
 L2 = ((FILE1) L3);
 r = L2;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L5 = ATTR(a,test1);
 L61_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L5), 0, L61_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name9);
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L101_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L101_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &branches);
 stdout_self2 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r2 = L12;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 i = 0;
 while (1) {
  L15 = FLISTF873512073(ATTR(a,tgts));
  L161_=(i)<(L15); 
  if (L161_) {
  }
  else {
   goto after_loop;
  }
  L171_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L171_);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &cond);
  stdout_self3 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r3 = L18;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  j = 0;
  {
   struct FLISTd617209260_frame_struct other2_0;
FLISTd617209260_frame noname1 = &other2_0;
   noname1->state = 0;
   while (1) {
    if (noname1->state == 0) {
     aget_self = ATTR(a,tgts);
     aget_ind = i;
     L221_=(FLISTdAM_CONST)ARR((FLISTF7594819)aget_self,aget_ind); 
     ret_val8 = L221_;
     L11 = ret_val8;
     noname1->self = L11;
    }
    L21 = FLISTd617209260(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    AM_OUT1439666796(((AM_OUT) NULL), ((dAM) L21));
    create_self4 = ((OUT) NULL);
    ret_val9 = create_self4;
    plus_self4 = ret_val9;
    plus_s4 = ((STR) &name29);
    stdout_self4 = ((FILE1) NULL);
    L24=ZALLOC(sizeof(struct FILE1_struct));
    if (L24==NULL) FATAL("Unable to allocate more memory");
    ((OB)L24)->header.tag=FILE1_tag;
    L23 = ((FILE1) L24);
    r4 = L23;
    
    SATTR(r4,fp,stdout);
    ret_val10 = r4;
    FILE_plus_STR(ret_val10, plus_s4);
   }
  }
  after_loop1: ;
  create_self5 = ((OUT) NULL);
  ret_val11 = create_self5;
  plus_self5 = ret_val11;
  plus_s5 = ((STR) &name9);
  stdout_self5 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r5 = L26;
  
  SATTR(r5,fp,stdout);
  ret_val12 = r5;
  FILE_plus_STR(ret_val12, plus_s5);
  L291_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L291_);
  create_self6 = ((OUT) NULL);
  ret_val13 = create_self6;
  plus_self6 = ret_val13;
  plus_s6 = ((STR) &code3);
  stdout_self6 = ((FILE1) NULL);
  L31=ZALLOC(sizeof(struct FILE1_struct));
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=FILE1_tag;
  L30 = ((FILE1) L31);
  r6 = L30;
  
  SATTR(r6,fp,stdout);
  ret_val14 = r6;
  FILE_plus_STR(ret_val14, plus_s6);
  aget_self1 = ATTR(a,stmts);
  aget_ind1 = i;
  L331_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
  ret_val15 = L331_;
  L34 = ret_val15;
  L351_=INTPLUS(indent11,10); 
  AM_OUT860401336(self, ((dAM) L34), 0, L351_);
  L361_=INTPLUS(i,1); 
  i = L361_;
 }
 after_loop: ;
 L371_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L371_);
 create_self7 = ((OUT) NULL);
 ret_val16 = create_self7;
 plus_self7 = ret_val16;
 plus_s7 = ((STR) &else_stmts);
 stdout_self7 = ((FILE1) NULL);
 L39=ZALLOC(sizeof(struct FILE1_struct));
 if (L39==NULL) FATAL("Unable to allocate more memory");
 ((OB)L39)->header.tag=FILE1_tag;
 L38 = ((FILE1) L39);
 r7 = L38;
 
 SATTR(r7,fp,stdout);
 ret_val17 = r7;
 FILE_plus_STR(ret_val17, plus_s7);
 L42 = ATTR(a,else_stmts);
 L431_=INTPLUS(indent11,15); 
 AM_OUT860401336(self, ((dAM) L42), 0, L431_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1614972293(AM_OUT self, AM_BND260301329 a, INT first1, INT indent11) {
 AM_BND260301329 L11;
 INT i = INT_zero;
 AM_BND260301329 L21;
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
 INT L31 = INT_zero;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 INT L41_;
 extern STR br;
 FILE1 L5;
 OB L6;
 extern STR name9;
 FILE1 L8;
 OB L9;
 INT L121_;
 extern STR args1;
 FILE1 L13;
 OB L14;
 INT L161_br;
 INT rL161_;
 INT L171_br;
INT i_L171_=0;
 AM_CALL_ARG aL171_;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L221_;
 INT L231_;
 AM_CALL_ARG L24;
 AM_CALL_ARG L25;
 INT L261_;
 extern STR name9;
 FILE1 L27;
 OB L28;
 INT L301_;
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &br);
 stdout_self = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r = L5;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 AM_OUT1439666796(self, ((dAM) ATTR(a,br)));
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name9);
 stdout_self1 = ((FILE1) NULL);
 L9=ZALLOC(sizeof(struct FILE1_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=FILE1_tag;
 L8 = ((FILE1) L9);
 r1 = L8;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L121_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L121_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &args1);
 stdout_self2 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r2 = L13;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 {
  BOOL f_L161_ = TRUE;
  BOOL f_L171_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L161_br=L11==NULL?0:ASIZE((AM_BND260301329)L11); 
  L171_br=L21==NULL?0:ASIZE((AM_BND260301329)L21); 
  i_L171_=0;
  while (1) {
   if(L31>=L161_br)  goto after_loop; 
   rL161_=L31; 
   i = rL161_;
   L191_=(i)==(0); 
   L20 = L191_;
   L221_=!(L20); 
   if (L221_) {
    L231_=INTPLUS(indent11,8); 
    AM_OUT2087454067(self, L231_);
   }
   if(i_L171_>=L171_br)  goto after_loop; 
   aL171_=ARR((AM_BND260301329)L21,i_L171_); i_L171_++;
   L25 = aL171_;
   L261_=INTPLUS(indent11,8); 
   AM_OUT860401336(self, ((dAM) L25), 0, L261_);
   create_self3 = ((OUT) NULL);
   ret_val6 = create_self3;
   plus_self3 = ret_val6;
   plus_s3 = ((STR) &name9);
   stdout_self3 = ((FILE1) NULL);
   L28=ZALLOC(sizeof(struct FILE1_struct));
   if (L28==NULL) FATAL("Unable to allocate more memory");
   ((OB)L28)->header.tag=FILE1_tag;
   L27 = ((FILE1) L28);
   r3 = L27;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   FILE_plus_STR(ret_val7, plus_s3);
   L301_=INTPLUS(L31,1); 
   L31 = L301_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1623210178(AM_OUT self, AM_TYPECASE_STMT a, INT first1, INT indent11) {
 INT i = INT_zero;
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
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 FLISTdTP aget_self;
 INT aget_ind = INT_zero;
 dTP ret_val9;
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
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val13;
 OUT create_self4;
 OUT ret_val14;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 INT L11_;
 extern STR test2;
 FILE1 L2;
 OB L3;
 AM_LOCAL_EXPR L5;
 INT L61_;
 extern STR name9;
 FILE1 L7;
 OB L8;
 INT L101_;
 extern STR branches;
 FILE1 L12;
 OB L13;
 INT L15;
 BOOL L161_;
 INT L171_;
 extern STR type2;
 FILE1 L18;
 OB L19;
 dTP L211_;
 dTP L22;
 FILE1 L23;
 OB L24;
 extern STR name9;
 FILE1 L26;
 OB L27;
 dAM_STMT L291_;
 dAM_STMT L30;
 INT L311_;
 INT L32;
 INT L331_;
 INT L341_;
 INT L351_;
 extern STR else_stmts1;
 FILE1 L36;
 OB L37;
 dAM_STMT L39;
 INT L401_;
 L11_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L11_);
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &test2);
 stdout_self = ((FILE1) NULL);
 L3=ZALLOC(sizeof(struct FILE1_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=FILE1_tag;
 L2 = ((FILE1) L3);
 r = L2;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L5 = ATTR(a,test1);
 L61_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L5), 0, L61_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name9);
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L101_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L101_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &branches);
 stdout_self2 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r2 = L12;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 i = 0;
 while (1) {
  L15 = FLISTd895185163(ATTR(a,tgts));
  L161_=(i)<(L15); 
  if (L161_) {
  }
  else {
   goto after_loop;
  }
  L171_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L171_);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &type2);
  stdout_self3 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r3 = L18;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  ret_val7 = plus_self3;
  plus_self4 = ret_val7;
  aget_self = ATTR(a,tgts);
  aget_ind = i;
  L211_=(dTP)ARR((FLISTdTP)aget_self,aget_ind); 
  ret_val9 = L211_;
  L22 = ret_val9;
  plus_s4 = (*dTP_strrSTR[TAG(L22)])(L22);
  stdout_self4 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r4 = L23;
  
  SATTR(r4,fp,stdout);
  ret_val11 = r4;
  FILE_plus_STR(ret_val11, plus_s4);
  ret_val10 = plus_self4;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &name9);
  stdout_self5 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r5 = L26;
  
  SATTR(r5,fp,stdout);
  ret_val12 = r5;
  FILE_plus_STR(ret_val12, plus_s5);
  aget_self1 = ATTR(a,stmts);
  aget_ind1 = i;
  L291_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
  ret_val13 = L291_;
  L30 = ret_val13;
  L311_=INTPLUS(indent11,6); 
  L32 = L311_;
  L331_=INTPLUS(indent11,6); 
  AM_OUT860401336(self, ((dAM) L30), L32, L331_);
  L341_=INTPLUS(i,1); 
  i = L341_;
 }
 after_loop: ;
 L351_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L351_);
 create_self4 = ((OUT) NULL);
 ret_val14 = create_self4;
 plus_self6 = ret_val14;
 plus_s6 = ((STR) &else_stmts1);
 stdout_self6 = ((FILE1) NULL);
 L37=ZALLOC(sizeof(struct FILE1_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=FILE1_tag;
 L36 = ((FILE1) L37);
 r6 = L36;
 
 SATTR(r6,fp,stdout);
 ret_val15 = r6;
 FILE_plus_STR(ret_val15, plus_s6);
 L39 = ATTR(a,else_stmts);
 L401_=INTPLUS(indent11,14); 
 AM_OUT860401336(self, ((dAM) L39), 0, L401_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1677535099(AM_OUT self, AM_LOCAL_EXPR a, INT first1, INT indent11) {
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
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val11;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
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
 OUT create_self4;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val17;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 AM_LOCAL_EXPR is_near_self;
 BOOL ret_val19 = BOOL_zero;
 OUT plus_self9;
 dSTR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 BOOL L1;
 extern STR nonamenotype;
 FILE1 L2;
 OB L3;
 extern STR noname;
 FILE1 L5;
 OB L6;
 dTP L8;
 FILE1 L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR notype;
 FILE1 L15;
 OB L16;
 FILE1 L18;
 OB L19;
 extern STR name21;
 FILE1 L21;
 OB L22;
 dTP L24;
 FILE1 L25;
 OB L26;
 extern STR is_near2;
 FILE1 L28;
 OB L29;
 BOOL L31;
 dTP L32;
 dSTR L33;
 OB L34;
 FILE1 L35;
 OB L36;
 if ((ATTR(a,name1).str==(STR)0)) {
  L1 = (ATTR(a,tp_at)==((dTP) NULL));
 } else {
  L1 = FALSE;
 }
 if (L1) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &nonamenotype);
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
 else {
  if ((ATTR(a,name1).str==(STR)0)) {
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self1 = ret_val2;
   plus_s1 = ((STR) &noname);
   stdout_self1 = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r1 = L5;
   
   SATTR(r1,fp,stdout);
   ret_val4 = r1;
   FILE_plus_STR(ret_val4, plus_s1);
   ret_val3 = plus_self1;
   plus_self2 = ret_val3;
   L8 = ATTR(a,tp_at);
   plus_s2 = (*dTP_strrSTR[TAG(L8)])(L8);
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
  else {
   if ((ATTR(a,tp_at)==((dTP) NULL))) {
    create_self2 = ((OUT) NULL);
    ret_val6 = create_self2;
    plus_self3 = ret_val6;
    plus_s3 = ATTR(a,name1).str;
    stdout_self3 = ((FILE1) NULL);
    L13=ZALLOC(sizeof(struct FILE1_struct));
    if (L13==NULL) FATAL("Unable to allocate more memory");
    ((OB)L13)->header.tag=FILE1_tag;
    L12 = ((FILE1) L13);
    r3 = L12;
    
    SATTR(r3,fp,stdout);
    ret_val8 = r3;
    FILE_plus_STR(ret_val8, plus_s3);
    ret_val7 = plus_self3;
    plus_self4 = ret_val7;
    plus_s4 = ((STR) &notype);
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
   else {
    create_self3 = ((OUT) NULL);
    ret_val10 = create_self3;
    plus_self5 = ret_val10;
    plus_s5 = ATTR(a,name1).str;
    stdout_self5 = ((FILE1) NULL);
    L19=ZALLOC(sizeof(struct FILE1_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=FILE1_tag;
    L18 = ((FILE1) L19);
    r5 = L18;
    
    SATTR(r5,fp,stdout);
    ret_val12 = r5;
    FILE_plus_STR(ret_val12, plus_s5);
    ret_val11 = plus_self5;
    plus_self6 = ret_val11;
    plus_s6 = ((STR) &name21);
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
    L24 = ATTR(a,tp_at);
    plus_s7 = (*dTP_strrSTR[TAG(L24)])(L24);
    stdout_self7 = ((FILE1) NULL);
    L26=ZALLOC(sizeof(struct FILE1_struct));
    if (L26==NULL) FATAL("Unable to allocate more memory");
    ((OB)L26)->header.tag=FILE1_tag;
    L25 = ((FILE1) L26);
    r7 = L25;
    
    SATTR(r7,fp,stdout);
    ret_val15 = r7;
    FILE_plus_STR(ret_val15, plus_s7);
   }
  }
 }
 create_self4 = ((OUT) NULL);
 ret_val16 = create_self4;
 plus_self8 = ret_val16;
 plus_s8 = ((STR) &is_near2);
 stdout_self8 = ((FILE1) NULL);
 L29=ZALLOC(sizeof(struct FILE1_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=FILE1_tag;
 L28 = ((FILE1) L29);
 r8 = L28;
 
 SATTR(r8,fp,stdout);
 ret_val18 = r8;
 FILE_plus_STR(ret_val18, plus_s8);
 ret_val17 = plus_self8;
 plus_self9 = ret_val17;
 is_near_self = a;
 if (ATTR(is_near_self,is_always_near)) {
  L31 = TRUE;
 } else {
  L32 = ATTR(is_near_self,tp_at);
  L31 = (*dTP_is1334578382[TAG(L32)])(L32);
 }
 ret_val19 = L31;
 L34=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L34==NULL) FATAL("Unable to allocate more memory");
 memset(L34,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L34)->header.tag=BOOL_tag;
 L33 = (dSTR)((BOOL_boxed) L34);
 ((BOOL_boxed) L33)->immutable_part = ret_val19;
 plus_s9 = L33;
 stdout_self9 = ((FILE1) NULL);
 L36=ZALLOC(sizeof(struct FILE1_struct));
 if (L36==NULL) FATAL("Unable to allocate more memory");
 ((OB)L36)->header.tag=FILE1_tag;
 L35 = ((FILE1) L36);
 r9 = L35;
 
 SATTR(r9,fp,stdout);
 ret_val20 = r9;
 FILE_plus_dSTR(ret_val20, plus_s9);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1813430155(AM_OUT self, AM_BND367211769 a, INT first1, INT indent11) {
 AM_BND367211769 L11;
 INT i = INT_zero;
 AM_BND367211769 L21;
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
 INT L31 = INT_zero;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR bi;
 FILE1 L4;
 OB L5;
 dAM_EXPR L7;
 INT L81_;
 extern STR name9;
 FILE1 L9;
 OB L10;
 INT L131_;
 extern STR init2;
 FILE1 L14;
 OB L15;
 dAM_STMT L17;
 INT L181_;
 INT L191_;
 extern STR args1;
 FILE1 L20;
 OB L22;
 INT L241_br;
 INT rL241_;
 INT L251_br;
INT i_L251_=0;
 AM_CALL_ARG aL251_;
 INT L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 INT L301_;
 AM_CALL_ARG L32;
 AM_CALL_ARG L33;
 INT L341_;
 extern STR name9;
 FILE1 L35;
 OB L36;
 INT L381_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &bi);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L7 = ATTR(a,bi);
 L81_=INTPLUS(indent11,25); 
 AM_OUT860401336(self, ((dAM) L7), 0, L81_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name9);
 stdout_self1 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r1 = L9;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &init2);
 stdout_self2 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r2 = L14;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L17 = ATTR(a,init);
 L181_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L17), 0, L181_);
 L191_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L191_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &args1);
 stdout_self3 = ((FILE1) NULL);
 L22=ZALLOC(sizeof(struct FILE1_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=FILE1_tag;
 L20 = ((FILE1) L22);
 r3 = L20;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 {
  BOOL f_L241_ = TRUE;
  BOOL f_L251_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L241_br=L11==NULL?0:ASIZE((AM_BND367211769)L11); 
  L251_br=L21==NULL?0:ASIZE((AM_BND367211769)L21); 
  i_L251_=0;
  while (1) {
   if(L31>=L241_br)  goto after_loop; 
   rL241_=L31; 
   i = rL241_;
   L271_=(i)==(0); 
   L28 = L271_;
   L291_=!(L28); 
   if (L291_) {
    L301_=INTPLUS(indent11,8); 
    AM_OUT2087454067(self, L301_);
   }
   if(i_L251_>=L251_br)  goto after_loop; 
   aL251_=ARR((AM_BND367211769)L21,i_L251_); i_L251_++;
   L33 = aL251_;
   L341_=INTPLUS(indent11,8); 
   AM_OUT860401336(self, ((dAM) L33), 0, L341_);
   create_self4 = ((OUT) NULL);
   ret_val8 = create_self4;
   plus_self4 = ret_val8;
   plus_s4 = ((STR) &name9);
   stdout_self4 = ((FILE1) NULL);
   L36=ZALLOC(sizeof(struct FILE1_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   ((OB)L36)->header.tag=FILE1_tag;
   L35 = ((FILE1) L36);
   r4 = L35;
   
   SATTR(r4,fp,stdout);
   ret_val9 = r4;
   FILE_plus_STR(ret_val9, plus_s4);
   L381_=INTPLUS(L31,1); 
   L31 = L381_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1924306007(AM_OUT self, AM_ARRAY_EXPR a, INT first1, INT indent11) {
 AM_ARRAY_EXPR L11;
 INT i = INT_zero;
 AM_ARRAY_EXPR L21;
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
 INT L31 = INT_zero;
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
 extern STR tp21;
 FILE1 L4;
 OB L5;
 dTP L7;
 FILE1 L8;
 OB L9;
 extern STR name47;
 FILE1 L12;
 OB L13;
 INT L151_br;
 INT rL151_;
 INT L161_br;
INT i_L161_=0;
 dAM_EXPR aL161_;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 extern STR name48;
 FILE1 L22;
 OB L23;
 extern STR name24;
 FILE1 L25;
 OB L26;
 dAM_EXPR L28;
 dAM_EXPR L29;
 INT L301_;
 extern STR name49;
 FILE1 L32;
 OB L33;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &tp21);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 L7 = ATTR(a,tp_at);
 plus_s1 = (*dTP_strrSTR[TAG(L7)])(L7);
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
 plus_s2 = ((STR) &name47);
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
  BOOL f_L151_ = TRUE;
  BOOL f_L161_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L151_br=L11==NULL?0:ASIZE((AM_ARRAY_EXPR)L11); 
  L161_br=L21==NULL?0:ASIZE((AM_ARRAY_EXPR)L21); 
  i_L161_=0;
  while (1) {
   if(L31>=L151_br)  goto after_loop; 
   rL151_=L31; 
   i = rL151_;
   L181_=(i)==(0); 
   L19 = L181_;
   L201_=!(L19); 
   if (L201_) {
    create_self1 = ((OUT) NULL);
    ret_val6 = create_self1;
    plus_self3 = ret_val6;
    plus_s3 = ((STR) &name48);
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
   else {
    create_self2 = ((OUT) NULL);
    ret_val8 = create_self2;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &name24);
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
   if(i_L161_>=L161_br)  goto after_loop; 
   aL161_=ARR((AM_ARRAY_EXPR)L21,i_L161_); i_L161_++;
   L29 = aL161_;
   AM_OUT860401336(self, ((dAM) L29), 0, 0);
   L301_=INTPLUS(L31,1); 
   L31 = L301_;
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name49);
 stdout_self5 = ((FILE1) NULL);
 L33=ZALLOC(sizeof(struct FILE1_struct));
 if (L33==NULL) FATAL("Unable to allocate more memory");
 ((OB)L33)->header.tag=FILE1_tag;
 L32 = ((FILE1) L33);
 r5 = L32;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1997333258(AM_OUT self, AM_ATTR_EXPR a, INT first1, INT indent11) {
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
 dSTR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self2;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val11;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self4;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val15;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self5;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val19;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 AM_ATTR_EXPR is_near_self;
 BOOL ret_val21 = BOOL_zero;
 AM_ATTR_EXPR tp_self;
 dTP ret_val22;
 OUT plus_self10;
 dSTR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val23;
 FILE1 r10;
 extern STR ob1;
 FILE1 L1;
 OB L2;
 extern STR ob_is_near;
 FILE1 L4;
 OB L5;
 dAM_EXPR L7;
 dSTR L8;
 OB L9;
 FILE1 L10;
 OB L11;
 extern STR at1;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 extern STR self_tp;
 FILE1 L19;
 OB L20;
 dTP L22;
 FILE1 L23;
 OB L24;
 extern STR tp21;
 FILE1 L26;
 OB L27;
 dTP L29;
 FILE1 L30;
 OB L32;
 extern STR is_near3;
 FILE1 L34;
 OB L35;
 dTP L37;
 dSTR L38;
 OB L39;
 FILE1 L40;
 OB L41;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &ob1);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 AM_OUT860401336(self, ((dAM) ATTR(a,ob)), 0, 0);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &ob_is_near);
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
 L7 = ATTR(a,ob);
 L9=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 memset(L9,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L9)->header.tag=BOOL_tag;
 L8 = (dSTR)((BOOL_boxed) L9);
 ((BOOL_boxed) L8)->immutable_part = (*dAM_EX1117610980[TAG(L7)])(L7);
 plus_s2 = L8;
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_dSTR(ret_val5, plus_s2);
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &at1);
 stdout_self3 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r3 = L13;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ATTR(a,at).str;
 stdout_self4 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r4 = L16;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &self_tp);
 stdout_self5 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r5 = L19;
 
 SATTR(r5,fp,stdout);
 ret_val12 = r5;
 FILE_plus_STR(ret_val12, plus_s5);
 ret_val11 = plus_self5;
 plus_self6 = ret_val11;
 L22 = ATTR(a,self_tp);
 plus_s6 = (*dTP_strrSTR[TAG(L22)])(L22);
 stdout_self6 = ((FILE1) NULL);
 L24=ZALLOC(sizeof(struct FILE1_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=FILE1_tag;
 L23 = ((FILE1) L24);
 r6 = L23;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 create_self4 = ((OUT) NULL);
 ret_val14 = create_self4;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &tp21);
 stdout_self7 = ((FILE1) NULL);
 L27=ZALLOC(sizeof(struct FILE1_struct));
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=FILE1_tag;
 L26 = ((FILE1) L27);
 r7 = L26;
 
 SATTR(r7,fp,stdout);
 ret_val16 = r7;
 FILE_plus_STR(ret_val16, plus_s7);
 ret_val15 = plus_self7;
 plus_self8 = ret_val15;
 L29 = ATTR(a,tp_at);
 plus_s8 = (*dTP_strrSTR[TAG(L29)])(L29);
 stdout_self8 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r8 = L30;
 
 SATTR(r8,fp,stdout);
 ret_val17 = r8;
 FILE_plus_STR(ret_val17, plus_s8);
 create_self5 = ((OUT) NULL);
 ret_val18 = create_self5;
 plus_self9 = ret_val18;
 plus_s9 = ((STR) &is_near3);
 stdout_self9 = ((FILE1) NULL);
 L35=ZALLOC(sizeof(struct FILE1_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 ((OB)L35)->header.tag=FILE1_tag;
 L34 = ((FILE1) L35);
 r9 = L34;
 
 SATTR(r9,fp,stdout);
 ret_val20 = r9;
 FILE_plus_STR(ret_val20, plus_s9);
 ret_val19 = plus_self9;
 plus_self10 = ret_val19;
 is_near_self = a;
 tp_self = is_near_self;
 ret_val22 = ATTR(tp_self,tp_at);
 L37 = ret_val22;
 ret_val21 = (*dTP_is1334578382[TAG(L37)])(L37);
 L39=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L39==NULL) FATAL("Unable to allocate more memory");
 memset(L39,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L39)->header.tag=BOOL_tag;
 L38 = (dSTR)((BOOL_boxed) L39);
 ((BOOL_boxed) L38)->immutable_part = ret_val21;
 plus_s10 = L38;
 stdout_self10 = ((FILE1) NULL);
 L41=ZALLOC(sizeof(struct FILE1_struct));
 if (L41==NULL) FATAL("Unable to allocate more memory");
 ((OB)L41)->header.tag=FILE1_tag;
 L40 = ((FILE1) L41);
 r10 = L40;
 
 SATTR(r10,fp,stdout);
 ret_val23 = r10;
 FILE_plus_dSTR(ret_val23, plus_s10);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT2087454067(AM_OUT self, INT i) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 extern STR name10;
 FILE1 L4;
 OB L5;
 INT L71_;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = i;
  while (1) {
   if(L21>=L11)  goto after_loop; 
   ;
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &name10);
   stdout_self = ((FILE1) NULL);
   L5=ZALLOC(sizeof(struct FILE1_struct));
   if (L5==NULL) FATAL("Unable to allocate more memory");
   ((OB)L5)->header.tag=FILE1_tag;
   L4 = ((FILE1) L5);
   r = L4;
   
   SATTR(r,fp,stdout);
   ret_val1 = r;
   FILE_plus_STR(ret_val1, plus_s);
   L71_=INTPLUS(L21,1); 
   L21 = L71_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT244025178(AM_OUT self, AM_VAT319982528 a, INT first1, INT indent11) {
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
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
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
 extern STR voidob;
 FILE1 L1;
 OB L2;
 extern STR ob1;
 FILE1 L4;
 OB L5;
 extern STR voidat;
 FILE1 L7;
 OB L8;
 extern STR at1;
 FILE1 L10;
 OB L11;
 FILE1 L13;
 OB L14;
 extern STR voidval;
 FILE1 L16;
 OB L17;
 extern STR val2;
 FILE1 L19;
 OB L20;
 if ((ATTR(a,ob)==((dAM_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidob);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
 }
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &ob1);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  AM_OUT860401336(self, ((dAM) ATTR(a,ob)), 0, 0);
 }
 if ((ATTR(a,at).str==(STR)0)) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &voidat);
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
 else {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &at1);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  ret_val7 = plus_self3;
  plus_self4 = ret_val7;
  plus_s4 = ATTR(a,at).str;
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
 if ((ATTR(a,val1)==((dAM_EXPR) NULL))) {
  create_self4 = ((OUT) NULL);
  ret_val10 = create_self4;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &voidval);
  stdout_self5 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r5 = L16;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 else {
  create_self5 = ((OUT) NULL);
  ret_val12 = create_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &val2);
  stdout_self6 = ((FILE1) NULL);
  L20=ZALLOC(sizeof(struct FILE1_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=FILE1_tag;
  L19 = ((FILE1) L20);
  r6 = L19;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AM_OUT860401336(self, ((dAM) ATTR(a,val1)), 0, 0);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT347621831(AM_OUT self, AM_VAR744470097 a, INT first1, INT indent11) {
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
 extern STR voidob;
 FILE1 L1;
 OB L2;
 extern STR ob1;
 FILE1 L4;
 OB L5;
 extern STR voidind;
 FILE1 L7;
 OB L8;
 extern STR ind;
 FILE1 L10;
 OB L11;
 extern STR voidval;
 FILE1 L13;
 OB L14;
 extern STR val2;
 FILE1 L16;
 OB L17;
 if ((ATTR(a,ob)==((dAM_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidob);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
 }
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &ob1);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  AM_OUT860401336(self, ((dAM) ATTR(a,ob)), 0, 0);
 }
 if ((ATTR(a,ind)==((dAM_EXPR) NULL))) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &voidind);
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
 else {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &ind);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  AM_OUT860401336(self, ((dAM) ATTR(a,ind)), 0, 0);
 }
 if ((ATTR(a,val1)==((dAM_EXPR) NULL))) {
  create_self4 = ((OUT) NULL);
  ret_val8 = create_self4;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &voidval);
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
 else {
  create_self5 = ((OUT) NULL);
  ret_val10 = create_self5;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &val2);
  stdout_self5 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r5 = L16;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  AM_OUT860401336(self, ((dAM) ATTR(a,val1)), 0, 0);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT359547257(AM_OUT self, AM_PRE_STMT a, INT first1, INT indent11) {
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
 extern STR voidAM_PRE_STMT;
 FILE1 L1;
 OB L2;
 extern STR AM_PRE_STMT1;
 FILE1 L4;
 OB L5;
 INT L71_;
 extern STR test2;
 FILE1 L8;
 OB L9;
 dAM_EXPR L11;
 INT L121_;
 extern STR name9;
 FILE1 L13;
 OB L14;
 if ((a==((AM_PRE_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidAM_PRE_STMT);
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
 plus_s1 = ((STR) &AM_PRE_STMT1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L71_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L71_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &test2);
 stdout_self2 = ((FILE1) NULL);
 L9=ZALLOC(sizeof(struct FILE1_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=FILE1_tag;
 L8 = ((FILE1) L9);
 r2 = L8;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L11 = ATTR(a,test1);
 L121_=INTPLUS(indent11,9); 
 AM_OUT860401336(self, ((dAM) L11), 0, L121_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name9);
 stdout_self3 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r3 = L13;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT376898766(AM_OUT self, AM_EXT_CALL_EXPR a, INT first1, INT indent11) {
 AM_EXT_CALL_EXPR L11;
 INT i = INT_zero;
 AM_EXT_CALL_EXPR L21;
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
 INT L31 = INT_zero;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR fun;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L12;
 INT L141_;
 extern STR args1;
 FILE1 L15;
 OB L16;
 INT L181_br;
 INT rL181_;
 INT L191_br;
INT i_L191_=0;
 AM_CALL_ARG aL191_;
 INT L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 INT L251_;
 AM_CALL_ARG L26;
 AM_CALL_ARG L27;
 INT L281_;
 extern STR name9;
 FILE1 L29;
 OB L30;
 INT L331_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &fun);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 plus_s1 = ATTR(ATTR(a,fun),str);
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L10 = ((FILE1) L12);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L141_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L141_);
 create_self1 = ((OUT) NULL);
 ret_val6 = create_self1;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &args1);
 stdout_self3 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r3 = L15;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 {
  BOOL f_L181_ = TRUE;
  BOOL f_L191_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L181_br=L11==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L11); 
  L191_br=L21==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L21); 
  i_L191_=0;
  while (1) {
   if(L31>=L181_br)  goto after_loop; 
   rL181_=L31; 
   i = rL181_;
   L221_=(i)==(0); 
   L23 = L221_;
   L241_=!(L23); 
   if (L241_) {
    L251_=INTPLUS(indent11,8); 
    AM_OUT2087454067(self, L251_);
   }
   if(i_L191_>=L191_br)  goto after_loop; 
   aL191_=ARR((AM_EXT_CALL_EXPR)L21,i_L191_); i_L191_++;
   L27 = aL191_;
   L281_=INTPLUS(indent11,8); 
   AM_OUT860401336(self, ((dAM) L27), 0, L281_);
   create_self2 = ((OUT) NULL);
   ret_val8 = create_self2;
   plus_self4 = ret_val8;
   plus_s4 = ((STR) &name9);
   stdout_self4 = ((FILE1) NULL);
   L30=ZALLOC(sizeof(struct FILE1_struct));
   if (L30==NULL) FATAL("Unable to allocate more memory");
   ((OB)L30)->header.tag=FILE1_tag;
   L29 = ((FILE1) L30);
   r4 = L29;
   
   SATTR(r4,fp,stdout);
   ret_val9 = r4;
   FILE_plus_STR(ret_val9, plus_s4);
   L331_=INTPLUS(L31,1); 
   L31 = L331_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT391785759(AM_OUT self, AM_OB_DEF a, INT first1, INT indent11) {
 FMAPIDENTdTP L11;
 TUPIDENTdTP p = TUPIDENTdTP_zero;
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
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
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
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self6;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val17;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val19;
 FILE1 stdout_self9;
 FILE1 ret_val20;
 FILE1 r9;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val21;
 FILE1 stdout_self10;
 FILE1 ret_val22;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val23;
 FILE1 stdout_self11;
 FILE1 ret_val24;
 FILE1 r11;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self7;
 OUT ret_val26;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val27;
 FILE1 r13;
 OUT create_self8;
 OUT ret_val28;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val29;
 FILE1 r14;
 OUT create_self9;
 OUT ret_val30;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val31;
 FILE1 r15;
 OUT create_self10;
 OUT ret_val32;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val33;
 FILE1 r16;
 OUT create_self11;
 OUT ret_val34;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val35;
 FILE1 r17;
 extern STR voidtp;
 FILE1 L2;
 OB L3;
 extern STR tp11;
 FILE1 L5;
 OB L6;
 dTP L8;
 FILE1 L9;
 OB L10;
 extern STR name10;
 FILE1 L13;
 OB L14;
 extern STR name9;
 FILE1 L16;
 OB L17;
 extern STR voidattributes;
 FILE1 L19;
 OB L20;
 extern STR name9;
 FILE1 L22;
 OB L23;
 INT L251_;
 extern STR attributes1;
 FILE1 L26;
 OB L27;
 TUPIDENTdTP L29;
 extern STR name24;
 FILE1 L30;
 OB L31;
 FILE1 L33;
 OB L34;
 extern STR name35;
 FILE1 L36;
 OB L37;
 dTP L39;
 FILE1 L40;
 OB L42;
 extern STR name23;
 FILE1 L44;
 OB L45;
 extern STR name9;
 FILE1 L47;
 OB L48;
 INT L501_;
 extern STR arraytype;
 FILE1 L51;
 OB L52;
 extern STR void1;
 FILE1 L54;
 OB L55;
 dTP L57;
 FILE1 L58;
 OB L59;
 extern STR name9;
 FILE1 L61;
 OB L62;
 if ((ATTR(a,tp)==((dTP) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidtp);
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
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &tp11);
  stdout_self1 = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r1 = L5;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  L8 = ATTR(a,tp);
  plus_s2 = (*dTP_strrSTR[TAG(L8)])(L8);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
  ret_val5 = plus_self2;
  plus_self3 = ret_val5;
  plus_s3 = ((STR) &name10);
  stdout_self3 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r3 = L13;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 create_self2 = ((OUT) NULL);
 ret_val8 = create_self2;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r4 = L16;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 if ((ATTR(a,at)==((FMAPIDENTdTP) NULL))) {
  create_self3 = ((OUT) NULL);
  ret_val10 = create_self3;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &voidattributes);
  stdout_self5 = ((FILE1) NULL);
  L20=ZALLOC(sizeof(struct FILE1_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=FILE1_tag;
  L19 = ((FILE1) L20);
  r5 = L19;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 else {
  create_self4 = ((OUT) NULL);
  ret_val12 = create_self4;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name9);
  stdout_self6 = ((FILE1) NULL);
  L23=ZALLOC(sizeof(struct FILE1_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=FILE1_tag;
  L22 = ((FILE1) L23);
  r6 = L22;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  L251_=INTPLUS(indent11,11); 
  AM_OUT2087454067(self, L251_);
  create_self5 = ((OUT) NULL);
  ret_val14 = create_self5;
  plus_self7 = ret_val14;
  plus_s7 = ((STR) &attributes1);
  stdout_self7 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r7 = L26;
  
  SATTR(r7,fp,stdout);
  ret_val15 = r7;
  FILE_plus_STR(ret_val15, plus_s7);
  {
   struct FMAPID1741388334_frame_struct other1_0;
FMAPID1741388334_frame noname1 = &other1_0;
   L11 = ATTR(a,at);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L29 = FMAPID1741388334(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    p = L29;
    create_self6 = ((OUT) NULL);
    ret_val16 = create_self6;
    plus_self8 = ret_val16;
    plus_s8 = ((STR) &name24);
    stdout_self8 = ((FILE1) NULL);
    L31=ZALLOC(sizeof(struct FILE1_struct));
    if (L31==NULL) FATAL("Unable to allocate more memory");
    ((OB)L31)->header.tag=FILE1_tag;
    L30 = ((FILE1) L31);
    r8 = L30;
    
    SATTR(r8,fp,stdout);
    ret_val18 = r8;
    FILE_plus_STR(ret_val18, plus_s8);
    ret_val17 = plus_self8;
    plus_self9 = ret_val17;
    plus_s9 = p.t1.str;
    stdout_self9 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r9 = L33;
    
    SATTR(r9,fp,stdout);
    ret_val20 = r9;
    FILE_plus_STR(ret_val20, plus_s9);
    ret_val19 = plus_self9;
    plus_self10 = ret_val19;
    plus_s10 = ((STR) &name35);
    stdout_self10 = ((FILE1) NULL);
    L37=ZALLOC(sizeof(struct FILE1_struct));
    if (L37==NULL) FATAL("Unable to allocate more memory");
    ((OB)L37)->header.tag=FILE1_tag;
    L36 = ((FILE1) L37);
    r10 = L36;
    
    SATTR(r10,fp,stdout);
    ret_val22 = r10;
    FILE_plus_STR(ret_val22, plus_s10);
    ret_val21 = plus_self10;
    plus_self11 = ret_val21;
    L39 = p.t2;
    plus_s11 = (*dTP_strrSTR[TAG(L39)])(L39);
    stdout_self11 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L40 = ((FILE1) L42);
    r11 = L40;
    
    SATTR(r11,fp,stdout);
    ret_val24 = r11;
    FILE_plus_STR(ret_val24, plus_s11);
    ret_val23 = plus_self11;
    plus_self12 = ret_val23;
    plus_s12 = ((STR) &name23);
    stdout_self12 = ((FILE1) NULL);
    L45=ZALLOC(sizeof(struct FILE1_struct));
    if (L45==NULL) FATAL("Unable to allocate more memory");
    ((OB)L45)->header.tag=FILE1_tag;
    L44 = ((FILE1) L45);
    r12 = L44;
    
    SATTR(r12,fp,stdout);
    ret_val25 = r12;
    FILE_plus_STR(ret_val25, plus_s12);
   }
  }
  after_loop: ;
 }
 create_self7 = ((OUT) NULL);
 ret_val26 = create_self7;
 plus_self13 = ret_val26;
 plus_s13 = ((STR) &name9);
 stdout_self13 = ((FILE1) NULL);
 L48=ZALLOC(sizeof(struct FILE1_struct));
 if (L48==NULL) FATAL("Unable to allocate more memory");
 ((OB)L48)->header.tag=FILE1_tag;
 L47 = ((FILE1) L48);
 r13 = L47;
 
 SATTR(r13,fp,stdout);
 ret_val27 = r13;
 FILE_plus_STR(ret_val27, plus_s13);
 L501_=INTPLUS(indent11,11); 
 AM_OUT2087454067(self, L501_);
 create_self8 = ((OUT) NULL);
 ret_val28 = create_self8;
 plus_self14 = ret_val28;
 plus_s14 = ((STR) &arraytype);
 stdout_self14 = ((FILE1) NULL);
 L52=ZALLOC(sizeof(struct FILE1_struct));
 if (L52==NULL) FATAL("Unable to allocate more memory");
 ((OB)L52)->header.tag=FILE1_tag;
 L51 = ((FILE1) L52);
 r14 = L51;
 
 SATTR(r14,fp,stdout);
 ret_val29 = r14;
 FILE_plus_STR(ret_val29, plus_s14);
 if ((ATTR(a,arr)==((dTP) NULL))) {
  create_self9 = ((OUT) NULL);
  ret_val30 = create_self9;
  plus_self15 = ret_val30;
  plus_s15 = ((STR) &void1);
  stdout_self15 = ((FILE1) NULL);
  L55=ZALLOC(sizeof(struct FILE1_struct));
  if (L55==NULL) FATAL("Unable to allocate more memory");
  ((OB)L55)->header.tag=FILE1_tag;
  L54 = ((FILE1) L55);
  r15 = L54;
  
  SATTR(r15,fp,stdout);
  ret_val31 = r15;
  FILE_plus_STR(ret_val31, plus_s15);
 }
 else {
  create_self10 = ((OUT) NULL);
  ret_val32 = create_self10;
  plus_self16 = ret_val32;
  L57 = ATTR(a,arr);
  plus_s16 = (*dTP_strrSTR[TAG(L57)])(L57);
  stdout_self16 = ((FILE1) NULL);
  L59=ZALLOC(sizeof(struct FILE1_struct));
  if (L59==NULL) FATAL("Unable to allocate more memory");
  ((OB)L59)->header.tag=FILE1_tag;
  L58 = ((FILE1) L59);
  r16 = L58;
  
  SATTR(r16,fp,stdout);
  ret_val33 = r16;
  FILE_plus_STR(ret_val33, plus_s16);
 }
 create_self11 = ((OUT) NULL);
 ret_val34 = create_self11;
 plus_self17 = ret_val34;
 plus_s17 = ((STR) &name9);
 stdout_self17 = ((FILE1) NULL);
 L62=ZALLOC(sizeof(struct FILE1_struct));
 if (L62==NULL) FATAL("Unable to allocate more memory");
 ((OB)L62)->header.tag=FILE1_tag;
 L61 = ((FILE1) L62);
 r17 = L61;
 
 SATTR(r17,fp,stdout);
 ret_val35 = r17;
 FILE_plus_STR(ret_val35, plus_s17);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT41050653(AM_OUT self, AM_ROU1916046290 a, INT first1, INT indent11) {
 AM_ROU1916046290 L11;
 INT i = INT_zero;
 AM_ROU1916046290 L21;
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
 INT L31 = INT_zero;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR fun1;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L12;
 INT L141_;
 extern STR args1;
 FILE1 L15;
 OB L16;
 INT L181_br;
 INT rL181_;
 INT L191_br;
INT i_L191_=0;
 AM_CALL_ARG aL191_;
 INT L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 INT L251_;
 AM_CALL_ARG L26;
 AM_CALL_ARG L27;
 INT L281_;
 extern STR name9;
 FILE1 L29;
 OB L30;
 INT L331_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &fun1);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 plus_s1 = ATTR(ATTR(a,fun),str);
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L10 = ((FILE1) L12);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L141_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L141_);
 create_self1 = ((OUT) NULL);
 ret_val6 = create_self1;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &args1);
 stdout_self3 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r3 = L15;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 {
  BOOL f_L181_ = TRUE;
  BOOL f_L191_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L181_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
  L191_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
  i_L191_=0;
  while (1) {
   if(L31>=L181_br)  goto after_loop; 
   rL181_=L31; 
   i = rL181_;
   L221_=(i)==(0); 
   L23 = L221_;
   L241_=!(L23); 
   if (L241_) {
    L251_=INTPLUS(indent11,8); 
    AM_OUT2087454067(self, L251_);
   }
   if(i_L191_>=L191_br)  goto after_loop; 
   aL191_=ARR((AM_ROU1916046290)L21,i_L191_); i_L191_++;
   L27 = aL191_;
   L281_=INTPLUS(indent11,8); 
   AM_OUT860401336(self, ((dAM) L27), 0, L281_);
   create_self2 = ((OUT) NULL);
   ret_val8 = create_self2;
   plus_self4 = ret_val8;
   plus_s4 = ((STR) &name9);
   stdout_self4 = ((FILE1) NULL);
   L30=ZALLOC(sizeof(struct FILE1_struct));
   if (L30==NULL) FATAL("Unable to allocate more memory");
   ((OB)L30)->header.tag=FILE1_tag;
   L29 = ((FILE1) L30);
   r4 = L29;
   
   SATTR(r4,fp,stdout);
   ret_val9 = r4;
   FILE_plus_STR(ret_val9, plus_s4);
   L331_=INTPLUS(L31,1); 
   L31 = L331_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT435235740(AM_OUT self, AM_ATTACH_STMT a, INT first1, INT indent11) {
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
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
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
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self6;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self7;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self8;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR routine1;
 FILE1 L5;
 OB L6;
 FILE1 L8;
 OB L9;
 extern STR name9;
 FILE1 L11;
 OB L12;
 INT L141_;
 extern STR helper;
 FILE1 L15;
 OB L16;
 AM_LOCAL_EXPR L18;
 INT L191_;
 extern STR name9;
 FILE1 L20;
 OB L21;
 INT L231_;
 extern STR gate;
 FILE1 L24;
 OB L25;
 AM_LOCAL_EXPR L27;
 INT L281_;
 extern STR name9;
 FILE1 L29;
 OB L30;
 INT L331_;
 extern STR at2;
 FILE1 L34;
 OB L35;
 extern STR any3;
 FILE1 L37;
 OB L38;
 dAM_EXPR L40;
 INT L421_;
 extern STR name9;
 FILE1 L43;
 OB L44;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &routine1);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ATTR(ATTR(a,rout),str);
 stdout_self2 = ((FILE1) NULL);
 L9=ZALLOC(sizeof(struct FILE1_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=FILE1_tag;
 L8 = ((FILE1) L9);
 r2 = L8;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 plus_s3 = ((STR) &name9);
 stdout_self3 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r3 = L11;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L141_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L141_);
 create_self2 = ((OUT) NULL);
 ret_val8 = create_self2;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &helper);
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L18 = ATTR(a,helper);
 L191_=INTPLUS(indent11,11); 
 AM_OUT860401336(self, ((dAM) L18), 0, L191_);
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name9);
 stdout_self5 = ((FILE1) NULL);
 L21=ZALLOC(sizeof(struct FILE1_struct));
 if (L21==NULL) FATAL("Unable to allocate more memory");
 ((OB)L21)->header.tag=FILE1_tag;
 L20 = ((FILE1) L21);
 r5 = L20;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
 L231_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L231_);
 create_self4 = ((OUT) NULL);
 ret_val12 = create_self4;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &gate);
 stdout_self6 = ((FILE1) NULL);
 L25=ZALLOC(sizeof(struct FILE1_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=FILE1_tag;
 L24 = ((FILE1) L25);
 r6 = L24;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 L27 = ATTR(a,gate);
 L281_=INTPLUS(indent11,11); 
 AM_OUT860401336(self, ((dAM) L27), 0, L281_);
 create_self5 = ((OUT) NULL);
 ret_val14 = create_self5;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &name9);
 stdout_self7 = ((FILE1) NULL);
 L30=ZALLOC(sizeof(struct FILE1_struct));
 if (L30==NULL) FATAL("Unable to allocate more memory");
 ((OB)L30)->header.tag=FILE1_tag;
 L29 = ((FILE1) L30);
 r7 = L29;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
 L331_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L331_);
 create_self6 = ((OUT) NULL);
 ret_val16 = create_self6;
 plus_self8 = ret_val16;
 plus_s8 = ((STR) &at2);
 stdout_self8 = ((FILE1) NULL);
 L35=ZALLOC(sizeof(struct FILE1_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 ((OB)L35)->header.tag=FILE1_tag;
 L34 = ((FILE1) L35);
 r8 = L34;
 
 SATTR(r8,fp,stdout);
 ret_val17 = r8;
 FILE_plus_STR(ret_val17, plus_s8);
 if ((ATTR(a,at)==((dAM_EXPR) NULL))) {
  create_self7 = ((OUT) NULL);
  ret_val18 = create_self7;
  plus_self9 = ret_val18;
  plus_s9 = ((STR) &any3);
  stdout_self9 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r9 = L37;
  
  SATTR(r9,fp,stdout);
  ret_val19 = r9;
  FILE_plus_STR(ret_val19, plus_s9);
 }
 else {
  L40 = ATTR(a,at);
  L421_=INTPLUS(indent11,11); 
  AM_OUT860401336(self, ((dAM) L40), 0, L421_);
 }
 create_self8 = ((OUT) NULL);
 ret_val20 = create_self8;
 plus_self10 = ret_val20;
 plus_s10 = ((STR) &name9);
 stdout_self10 = ((FILE1) NULL);
 L44=ZALLOC(sizeof(struct FILE1_struct));
 if (L44==NULL) FATAL("Unable to allocate more memory");
 ((OB)L44)->header.tag=FILE1_tag;
 L43 = ((FILE1) L44);
 r10 = L43;
 
 SATTR(r10,fp,stdout);
 ret_val21 = r10;
 FILE_plus_STR(ret_val21, plus_s10);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT523288876(AM_OUT self, AM_BND1124877163 a, INT first1, INT indent11) {
 AM_BND1124877163 L11;
 INT i = INT_zero;
 ARRAYINT L21;
 INT j = INT_zero;
 ARRAYINT L31;
 INT j11 = INT_zero;
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
 INT L41 = INT_zero;
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
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val11;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 INT L51 = INT_zero;
 OUT create_self6;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self7;
 OUT ret_val18;
 OUT plus_self9;
 dSTR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self8;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 OUT create_self9;
 OUT ret_val22;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val23;
 FILE1 r11;
 INT L61 = INT_zero;
 OUT create_self10;
 OUT ret_val24;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self11;
 OUT ret_val26;
 OUT plus_self13;
 dSTR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val27;
 FILE1 r13;
 OUT create_self12;
 OUT ret_val28;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val29;
 FILE1 r14;
 OUT create_self13;
 OUT ret_val30;
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
 extern STR name9;
 FILE1 L7;
 OB L8;
 INT L101_;
 extern STR args2;
 FILE1 L12;
 OB L13;
 INT L151_br;
 INT rL151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 extern STR name10;
 FILE1 L20;
 OB L22;
 AM_CALL_ARG L241_;
 INT L251_;
 extern STR name50;
 FILE1 L26;
 OB L27;
 INT L291_;
 extern STR fun;
 FILE1 L30;
 OB L32;
 FILE1 L34;
 OB L35;
 extern STR name9;
 FILE1 L37;
 OB L38;
 extern STR bnd_args;
 FILE1 L40;
 OB L42;
 INT L441_br;
 INT rL441_;
 INT L45;
 BOOL L461_;
 BOOL L47;
 BOOL L481_;
 extern STR name10;
 FILE1 L49;
 OB L50;
 ARRAYINT L53;
 INT L541_;
 dSTR L55;
 OB L56;
 FILE1 L57;
 OB L58;
 INT L601_;
 extern STR name9;
 FILE1 L62;
 OB L63;
 INT L651_;
 extern STR unbnd_args;
 FILE1 L66;
 OB L67;
 INT L691_br;
 INT rL691_;
 INT L70;
 BOOL L711_;
 BOOL L72;
 BOOL L731_;
 extern STR name10;
 FILE1 L74;
 OB L75;
 ARRAYINT L77;
 INT L781_;
 dSTR L79;
 OB L80;
 FILE1 L81;
 OB L82;
 INT L841_;
 extern STR name9;
 FILE1 L85;
 OB L86;
 INT L881_;
 extern STR tp_at;
 FILE1 L89;
 OB L90;
 dTP L93;
 FILE1 L94;
 OB L95;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r = L7;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L101_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L101_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &args2);
 stdout_self1 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r1 = L12;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 {
  BOOL f_L151_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = a;
  L151_br=L11==NULL?0:ASIZE((AM_BND1124877163)L11); 
  while (1) {
   if(L41>=L151_br)  goto after_loop; 
   rL151_=L41; 
   i = rL151_;
   L171_=(i)==(0); 
   L18 = L171_;
   L191_=!(L18); 
   if (L191_) {
    create_self2 = ((OUT) NULL);
    ret_val4 = create_self2;
    plus_self2 = ret_val4;
    plus_s2 = ((STR) &name10);
    stdout_self2 = ((FILE1) NULL);
    L22=ZALLOC(sizeof(struct FILE1_struct));
    if (L22==NULL) FATAL("Unable to allocate more memory");
    ((OB)L22)->header.tag=FILE1_tag;
    L20 = ((FILE1) L22);
    r2 = L20;
    
    SATTR(r2,fp,stdout);
    ret_val5 = r2;
    FILE_plus_STR(ret_val5, plus_s2);
   }
   L241_=(AM_CALL_ARG)ARR((AM_BND1124877163)a,i); 
   AM_OUT1439666796(self, ((dAM) L241_));
   L251_=INTPLUS(L41,1); 
   L41 = L251_;
  }
 }
 after_loop: ;
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name50);
 stdout_self3 = ((FILE1) NULL);
 L27=ZALLOC(sizeof(struct FILE1_struct));
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=FILE1_tag;
 L26 = ((FILE1) L27);
 r3 = L26;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L291_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L291_);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &fun);
 stdout_self4 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r4 = L30;
 
 SATTR(r4,fp,stdout);
 ret_val10 = r4;
 FILE_plus_STR(ret_val10, plus_s4);
 ret_val9 = plus_self4;
 plus_self5 = ret_val9;
 plus_s5 = ATTR(ATTR(a,fun),str);
 stdout_self5 = ((FILE1) NULL);
 L35=ZALLOC(sizeof(struct FILE1_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 ((OB)L35)->header.tag=FILE1_tag;
 L34 = ((FILE1) L35);
 r5 = L34;
 
 SATTR(r5,fp,stdout);
 ret_val12 = r5;
 FILE_plus_STR(ret_val12, plus_s5);
 ret_val11 = plus_self5;
 plus_self6 = ret_val11;
 plus_s6 = ((STR) &name9);
 stdout_self6 = ((FILE1) NULL);
 L38=ZALLOC(sizeof(struct FILE1_struct));
 if (L38==NULL) FATAL("Unable to allocate more memory");
 ((OB)L38)->header.tag=FILE1_tag;
 L37 = ((FILE1) L38);
 r6 = L37;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 create_self5 = ((OUT) NULL);
 ret_val14 = create_self5;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &bnd_args);
 stdout_self7 = ((FILE1) NULL);
 L42=ZALLOC(sizeof(struct FILE1_struct));
 if (L42==NULL) FATAL("Unable to allocate more memory");
 ((OB)L42)->header.tag=FILE1_tag;
 L40 = ((FILE1) L42);
 r7 = L40;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
 {
  BOOL f_L441_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L21 = ATTR(a,bnd_args);
  L441_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
  while (1) {
   if(L51>=L441_br)  goto after_loop1; 
   rL441_=L51; 
   j = rL441_;
   L461_=(j)==(0); 
   L47 = L461_;
   L481_=!(L47); 
   if (L481_) {
    create_self6 = ((OUT) NULL);
    ret_val16 = create_self6;
    plus_self8 = ret_val16;
    plus_s8 = ((STR) &name10);
    stdout_self8 = ((FILE1) NULL);
    L50=ZALLOC(sizeof(struct FILE1_struct));
    if (L50==NULL) FATAL("Unable to allocate more memory");
    ((OB)L50)->header.tag=FILE1_tag;
    L49 = ((FILE1) L50);
    r8 = L49;
    
    SATTR(r8,fp,stdout);
    ret_val17 = r8;
    FILE_plus_STR(ret_val17, plus_s8);
   }
   create_self7 = ((OUT) NULL);
   ret_val18 = create_self7;
   plus_self9 = ret_val18;
   L53 = ATTR(a,bnd_args);
   L541_=ARR((ARRAYINT)L53,j); 
   L56=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L56==NULL) FATAL("Unable to allocate more memory");
   memset(L56,0,sizeof(struct INT_boxed_struct));
   ((OB)L56)->header.tag=INT_tag;
   L55 = (dSTR)((INT_boxed) L56);
   ((INT_boxed) L55)->immutable_part = L541_;
   plus_s9 = L55;
   stdout_self9 = ((FILE1) NULL);
   L58=ZALLOC(sizeof(struct FILE1_struct));
   if (L58==NULL) FATAL("Unable to allocate more memory");
   ((OB)L58)->header.tag=FILE1_tag;
   L57 = ((FILE1) L58);
   r9 = L57;
   
   SATTR(r9,fp,stdout);
   ret_val19 = r9;
   FILE_plus_dSTR(ret_val19, plus_s9);
   L601_=INTPLUS(L51,1); 
   L51 = L601_;
  }
 }
 after_loop1: ;
 create_self8 = ((OUT) NULL);
 ret_val20 = create_self8;
 plus_self10 = ret_val20;
 plus_s10 = ((STR) &name9);
 stdout_self10 = ((FILE1) NULL);
 L63=ZALLOC(sizeof(struct FILE1_struct));
 if (L63==NULL) FATAL("Unable to allocate more memory");
 ((OB)L63)->header.tag=FILE1_tag;
 L62 = ((FILE1) L63);
 r10 = L62;
 
 SATTR(r10,fp,stdout);
 ret_val21 = r10;
 FILE_plus_STR(ret_val21, plus_s10);
 L651_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L651_);
 create_self9 = ((OUT) NULL);
 ret_val22 = create_self9;
 plus_self11 = ret_val22;
 plus_s11 = ((STR) &unbnd_args);
 stdout_self11 = ((FILE1) NULL);
 L67=ZALLOC(sizeof(struct FILE1_struct));
 if (L67==NULL) FATAL("Unable to allocate more memory");
 ((OB)L67)->header.tag=FILE1_tag;
 L66 = ((FILE1) L67);
 r11 = L66;
 
 SATTR(r11,fp,stdout);
 ret_val23 = r11;
 FILE_plus_STR(ret_val23, plus_s11);
 {
  BOOL f_L691_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L31 = ATTR(a,unbnd_args);
  L691_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
  while (1) {
   if(L61>=L691_br)  goto after_loop2; 
   rL691_=L61; 
   j11 = rL691_;
   L711_=(j11)==(0); 
   L72 = L711_;
   L731_=!(L72); 
   if (L731_) {
    create_self10 = ((OUT) NULL);
    ret_val24 = create_self10;
    plus_self12 = ret_val24;
    plus_s12 = ((STR) &name10);
    stdout_self12 = ((FILE1) NULL);
    L75=ZALLOC(sizeof(struct FILE1_struct));
    if (L75==NULL) FATAL("Unable to allocate more memory");
    ((OB)L75)->header.tag=FILE1_tag;
    L74 = ((FILE1) L75);
    r12 = L74;
    
    SATTR(r12,fp,stdout);
    ret_val25 = r12;
    FILE_plus_STR(ret_val25, plus_s12);
   }
   create_self11 = ((OUT) NULL);
   ret_val26 = create_self11;
   plus_self13 = ret_val26;
   L77 = ATTR(a,unbnd_args);
   L781_=ARR((ARRAYINT)L77,j11); 
   L80=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L80==NULL) FATAL("Unable to allocate more memory");
   memset(L80,0,sizeof(struct INT_boxed_struct));
   ((OB)L80)->header.tag=INT_tag;
   L79 = (dSTR)((INT_boxed) L80);
   ((INT_boxed) L79)->immutable_part = L781_;
   plus_s13 = L79;
   stdout_self13 = ((FILE1) NULL);
   L82=ZALLOC(sizeof(struct FILE1_struct));
   if (L82==NULL) FATAL("Unable to allocate more memory");
   ((OB)L82)->header.tag=FILE1_tag;
   L81 = ((FILE1) L82);
   r13 = L81;
   
   SATTR(r13,fp,stdout);
   ret_val27 = r13;
   FILE_plus_dSTR(ret_val27, plus_s13);
   L841_=INTPLUS(L61,1); 
   L61 = L841_;
  }
 }
 after_loop2: ;
 create_self12 = ((OUT) NULL);
 ret_val28 = create_self12;
 plus_self14 = ret_val28;
 plus_s14 = ((STR) &name9);
 stdout_self14 = ((FILE1) NULL);
 L86=ZALLOC(sizeof(struct FILE1_struct));
 if (L86==NULL) FATAL("Unable to allocate more memory");
 ((OB)L86)->header.tag=FILE1_tag;
 L85 = ((FILE1) L86);
 r14 = L85;
 
 SATTR(r14,fp,stdout);
 ret_val29 = r14;
 FILE_plus_STR(ret_val29, plus_s14);
 L881_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L881_);
 create_self13 = ((OUT) NULL);
 ret_val30 = create_self13;
 plus_self15 = ret_val30;
 plus_s15 = ((STR) &tp_at);
 stdout_self15 = ((FILE1) NULL);
 L90=ZALLOC(sizeof(struct FILE1_struct));
 if (L90==NULL) FATAL("Unable to allocate more memory");
 ((OB)L90)->header.tag=FILE1_tag;
 L89 = ((FILE1) L90);
 r15 = L89;
 
 SATTR(r15,fp,stdout);
 ret_val32 = r15;
 FILE_plus_STR(ret_val32, plus_s15);
 ret_val31 = plus_self15;
 plus_self16 = ret_val31;
 L93 = ATTR(a,tp_at);
 plus_s16 = (*dTP_strrSTR[TAG(L93)])(L93);
 stdout_self16 = ((FILE1) NULL);
 L95=ZALLOC(sizeof(struct FILE1_struct));
 if (L95==NULL) FATAL("Unable to allocate more memory");
 ((OB)L95)->header.tag=FILE1_tag;
 L94 = ((FILE1) L95);
 r16 = L94;
 
 SATTR(r16,fp,stdout);
 ret_val33 = r16;
 FILE_plus_STR(ret_val33, plus_s16);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT600226455(AM_OUT self, AM_CALL_ARG a, INT first1, INT indent11) {
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
 extern STR name52;
 FILE1 L1;
 OB L2;
 extern STR mode11;
 FILE1 L4;
 OB L5;
 dMODE L7;
 FILE1 L8;
 OB L9;
 extern STR expr2;
 FILE1 L11;
 OB L12;
 dAM_EXPR L14;
 INT L151_;
 extern STR name26;
 FILE1 L16;
 OB L17;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name52);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &mode11);
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
 L7 = ATTR(a,mode1);
 plus_s2 = (*dMODE_strrSTR[TAG(L7)])(L7);
 stdout_self2 = ((FILE1) NULL);
 L9=ZALLOC(sizeof(struct FILE1_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=FILE1_tag;
 L8 = ((FILE1) L9);
 r2 = L8;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &expr2);
 stdout_self3 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r3 = L11;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L14 = ATTR(a,expr);
 L151_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L14), 0, L151_);
 create_self3 = ((OUT) NULL);
 ret_val8 = create_self3;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name26);
 stdout_self4 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r4 = L16;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT671570827(AM_OUT self, AM_PROTECT_STMT a, INT first1, INT indent11) {
 INT i = INT_zero;
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
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 FLISTdTP aget_self;
 INT aget_ind = INT_zero;
 dTP ret_val9;
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
 FLISTdAM_STMT aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_STMT ret_val13;
 OUT create_self4;
 OUT ret_val14;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR body;
 FILE1 L5;
 OB L6;
 dAM_STMT L8;
 INT L91_;
 INT L101_;
 extern STR branches;
 FILE1 L11;
 OB L12;
 INT L14;
 BOOL L151_;
 INT L161_;
 extern STR type2;
 FILE1 L17;
 OB L18;
 dTP L201_;
 dTP L21;
 FILE1 L22;
 OB L23;
 extern STR name9;
 FILE1 L25;
 OB L26;
 dAM_STMT L281_;
 dAM_STMT L29;
 INT L301_;
 INT L32;
 INT L331_;
 INT L341_;
 INT L351_;
 extern STR else_stmts1;
 FILE1 L36;
 OB L37;
 dAM_STMT L39;
 INT L401_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &body);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,body);
 L91_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 L101_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L101_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &branches);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r2 = L11;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 i = 0;
 while (1) {
  L14 = FLISTd895185163(ATTR(a,tgts));
  L151_=(i)<(L14); 
  if (L151_) {
  }
  else {
   goto after_loop;
  }
  L161_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L161_);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &type2);
  stdout_self3 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r3 = L17;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  ret_val7 = plus_self3;
  plus_self4 = ret_val7;
  aget_self = ATTR(a,tgts);
  aget_ind = i;
  L201_=(dTP)ARR((FLISTdTP)aget_self,aget_ind); 
  ret_val9 = L201_;
  L21 = ret_val9;
  plus_s4 = (*dTP_strrSTR[TAG(L21)])(L21);
  stdout_self4 = ((FILE1) NULL);
  L23=ZALLOC(sizeof(struct FILE1_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=FILE1_tag;
  L22 = ((FILE1) L23);
  r4 = L22;
  
  SATTR(r4,fp,stdout);
  ret_val11 = r4;
  FILE_plus_STR(ret_val11, plus_s4);
  ret_val10 = plus_self4;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &name9);
  stdout_self5 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r5 = L25;
  
  SATTR(r5,fp,stdout);
  ret_val12 = r5;
  FILE_plus_STR(ret_val12, plus_s5);
  aget_self1 = ATTR(a,stmts);
  aget_ind1 = i;
  L281_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self1,aget_ind1); 
  ret_val13 = L281_;
  L29 = ret_val13;
  L301_=INTPLUS(indent11,6); 
  L32 = L301_;
  L331_=INTPLUS(indent11,6); 
  AM_OUT860401336(self, ((dAM) L29), L32, L331_);
  L341_=INTPLUS(i,1); 
  i = L341_;
 }
 after_loop: ;
 L351_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L351_);
 create_self4 = ((OUT) NULL);
 ret_val14 = create_self4;
 plus_self6 = ret_val14;
 plus_s6 = ((STR) &else_stmts1);
 stdout_self6 = ((FILE1) NULL);
 L37=ZALLOC(sizeof(struct FILE1_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=FILE1_tag;
 L36 = ((FILE1) L37);
 r6 = L36;
 
 SATTR(r6,fp,stdout);
 ret_val15 = r6;
 FILE_plus_STR(ret_val15, plus_s6);
 L39 = ATTR(a,else_stmts);
 L401_=INTPLUS(indent11,14); 
 AM_OUT860401336(self, ((dAM) L39), 0, L401_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT726258064(AM_OUT self, AM_SHARED_EXPR a, INT first1, INT indent11) {
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
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self8;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self9;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self10;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 extern STR name51;
 FILE1 L1;
 OB L2;
 extern STR void1;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR tp21;
 FILE1 L10;
 OB L11;
 extern STR void1;
 FILE1 L13;
 OB L14;
 dTP L16;
 FILE1 L17;
 OB L18;
 extern STR class_tp;
 FILE1 L20;
 OB L21;
 extern STR void1;
 FILE1 L23;
 OB L24;
 dTP L26;
 FILE1 L27;
 OB L28;
 extern STR name9;
 FILE1 L30;
 OB L32;
 INT L341_;
 extern STR init2;
 FILE1 L35;
 OB L36;
 dAM_EXPR L38;
 INT L391_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name51);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 if ((ATTR(a,name1).str==(STR)0)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &void1);
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
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ATTR(a,name1).str;
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
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &tp21);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 if ((ATTR(a,tp_at)==((dTP) NULL))) {
  create_self4 = ((OUT) NULL);
  ret_val8 = create_self4;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &void1);
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
 else {
  create_self5 = ((OUT) NULL);
  ret_val10 = create_self5;
  plus_self5 = ret_val10;
  L16 = ATTR(a,tp_at);
  plus_s5 = (*dTP_strrSTR[TAG(L16)])(L16);
  stdout_self5 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r5 = L17;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 create_self6 = ((OUT) NULL);
 ret_val12 = create_self6;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &class_tp);
 stdout_self6 = ((FILE1) NULL);
 L21=ZALLOC(sizeof(struct FILE1_struct));
 if (L21==NULL) FATAL("Unable to allocate more memory");
 ((OB)L21)->header.tag=FILE1_tag;
 L20 = ((FILE1) L21);
 r6 = L20;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 if ((ATTR(a,class_tp)==((dTP) NULL))) {
  create_self7 = ((OUT) NULL);
  ret_val14 = create_self7;
  plus_self7 = ret_val14;
  plus_s7 = ((STR) &void1);
  stdout_self7 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r7 = L23;
  
  SATTR(r7,fp,stdout);
  ret_val15 = r7;
  FILE_plus_STR(ret_val15, plus_s7);
 }
 else {
  create_self8 = ((OUT) NULL);
  ret_val16 = create_self8;
  plus_self8 = ret_val16;
  L26 = ATTR(a,class_tp);
  plus_s8 = (*dTP_strrSTR[TAG(L26)])(L26);
  stdout_self8 = ((FILE1) NULL);
  L28=ZALLOC(sizeof(struct FILE1_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=FILE1_tag;
  L27 = ((FILE1) L28);
  r8 = L27;
  
  SATTR(r8,fp,stdout);
  ret_val17 = r8;
  FILE_plus_STR(ret_val17, plus_s8);
 }
 create_self9 = ((OUT) NULL);
 ret_val18 = create_self9;
 plus_self9 = ret_val18;
 plus_s9 = ((STR) &name9);
 stdout_self9 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r9 = L30;
 
 SATTR(r9,fp,stdout);
 ret_val19 = r9;
 FILE_plus_STR(ret_val19, plus_s9);
 L341_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L341_);
 create_self10 = ((OUT) NULL);
 ret_val20 = create_self10;
 plus_self10 = ret_val20;
 plus_s10 = ((STR) &init2);
 stdout_self10 = ((FILE1) NULL);
 L36=ZALLOC(sizeof(struct FILE1_struct));
 if (L36==NULL) FATAL("Unable to allocate more memory");
 ((OB)L36)->header.tag=FILE1_tag;
 L35 = ((FILE1) L36);
 r10 = L35;
 
 SATTR(r10,fp,stdout);
 ret_val21 = r10;
 FILE_plus_STR(ret_val21, plus_s10);
 L38 = ATTR(a,init);
 L391_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L38), 0, L391_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT732629843(AM_OUT self, AM_ITE1809135850 a, INT first1, INT indent11) {
 AM_ITE1809135850 L11;
 INT i = INT_zero;
 AM_ITE1809135850 L21;
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
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 dSTR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self1;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self2;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 INT L31 = INT_zero;
 OUT create_self3;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 extern STR fun;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR init_before_loop;
 FILE1 L10;
 OB L12;
 dSTR L14;
 OB L15;
 FILE1 L16;
 OB L17;
 extern STR name9;
 FILE1 L19;
 OB L20;
 INT L231_;
 extern STR init2;
 FILE1 L24;
 OB L25;
 dAM_STMT L27;
 INT L281_;
 INT L291_;
 extern STR args1;
 FILE1 L30;
 OB L32;
 INT L341_br;
 INT rL341_;
 INT L351_br;
INT i_L351_=0;
 AM_CALL_ARG aL351_;
 INT L36;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 INT L401_;
 AM_CALL_ARG L41;
 AM_CALL_ARG L42;
 INT L431_;
 extern STR name9;
 FILE1 L44;
 OB L45;
 INT L471_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &fun);
 stdout_self = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r = L4;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 plus_s1 = ATTR(ATTR(a,fun),str);
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ((STR) &init_before_loop);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L10 = ((FILE1) L12);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L15=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 memset(L15,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L15)->header.tag=BOOL_tag;
 L14 = (dSTR)((BOOL_boxed) L15);
 ((BOOL_boxed) L14)->immutable_part = ATTR(a,init_before_loop);
 plus_s3 = L14;
 stdout_self3 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r3 = L16;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_dSTR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r4 = L19;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L231_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L231_);
 create_self1 = ((OUT) NULL);
 ret_val10 = create_self1;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &init2);
 stdout_self5 = ((FILE1) NULL);
 L25=ZALLOC(sizeof(struct FILE1_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=FILE1_tag;
 L24 = ((FILE1) L25);
 r5 = L24;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
 L27 = ATTR(a,init);
 L281_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L27), 0, L281_);
 L291_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L291_);
 create_self2 = ((OUT) NULL);
 ret_val12 = create_self2;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &args1);
 stdout_self6 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r6 = L30;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 {
  BOOL f_L341_ = TRUE;
  BOOL f_L351_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = a;
  L21 = a;
  L341_br=L11==NULL?0:ASIZE((AM_ITE1809135850)L11); 
  L351_br=L21==NULL?0:ASIZE((AM_ITE1809135850)L21); 
  i_L351_=0;
  while (1) {
   if(L31>=L341_br)  goto after_loop; 
   rL341_=L31; 
   i = rL341_;
   L371_=(i)==(0); 
   L38 = L371_;
   L391_=!(L38); 
   if (L391_) {
    L401_=INTPLUS(indent11,8); 
    AM_OUT2087454067(self, L401_);
   }
   if(i_L351_>=L351_br)  goto after_loop; 
   aL351_=ARR((AM_ITE1809135850)L21,i_L351_); i_L351_++;
   L42 = aL351_;
   L431_=INTPLUS(indent11,8); 
   AM_OUT860401336(self, ((dAM) L42), 0, L431_);
   create_self3 = ((OUT) NULL);
   ret_val14 = create_self3;
   plus_self7 = ret_val14;
   plus_s7 = ((STR) &name9);
   stdout_self7 = ((FILE1) NULL);
   L45=ZALLOC(sizeof(struct FILE1_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   ((OB)L45)->header.tag=FILE1_tag;
   L44 = ((FILE1) L45);
   r7 = L44;
   
   SATTR(r7,fp,stdout);
   ret_val15 = r7;
   FILE_plus_STR(ret_val15, plus_s7);
   L471_=INTPLUS(L31,1); 
   L31 = L471_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT786574336(AM_OUT self, AM_RETURN_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR val1;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &val1);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,val1);
 L91_=INTPLUS(indent11,7); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT796864830(AM_OUT self, AM_ARR_CONST a, INT first1, INT indent11) {
 ARRAYdAM_CONST L11;
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
 INT L21 = INT_zero;
 extern STR elt_tp;
 FILE1 L3;
 OB L4;
 FILE1 L6;
 OB L7;
 extern STR name9;
 FILE1 L9;
 OB L10;
 INT L131_;
 extern STR elts;
 FILE1 L14;
 OB L15;
 INT L171_br;
 dAM_CONST aL171_;
 dAM_CONST L18;
 INT L191_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &elt_tp);
 stdout_self = ((FILE1) NULL);
 L4=ZALLOC(sizeof(struct FILE1_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=FILE1_tag;
 L3 = ((FILE1) L4);
 r = L3;
 
 SATTR(r,fp,stdout);
 ret_val2 = r;
 FILE_plus_STR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 plus_s1 = TP_CLASS_strrSTR(ATTR(a,elt_tp));
 stdout_self1 = ((FILE1) NULL);
 L7=ZALLOC(sizeof(struct FILE1_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=FILE1_tag;
 L6 = ((FILE1) L7);
 r1 = L6;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 create_self1 = ((OUT) NULL);
 ret_val6 = create_self1;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &elts);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 {
  BOOL f_L171_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(a,elts);
  L171_br=L11==NULL?0:ASIZE((ARRAYdAM_CONST)L11); 
  while (1) {
   if(L21>=L171_br)  goto after_loop; 
   aL171_=ARR((ARRAYdAM_CONST)L11,L21); 
   AM_OUT1439666796(self, ((dAM) aL171_));
   L191_=INTPLUS(L21,1); 
   L21 = L191_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT797583920(AM_OUT self, AM_EXCEPT_EXPR a, INT first1, INT indent11) {
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT930898334(AM_OUT self, AM_EXPR_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR expr;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &expr);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,expr);
 L91_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT959054340(AM_OUT self, AM_IF_EXPR a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR test11;
 FILE1 L5;
 OB L6;
 dAM_EXPR L8;
 INT L91_;
 extern STR name9;
 FILE1 L10;
 OB L11;
 INT L131_;
 extern STR if_true;
 FILE1 L14;
 OB L15;
 dAM_EXPR L17;
 INT L181_;
 extern STR name9;
 FILE1 L19;
 OB L20;
 INT L221_;
 extern STR if_false;
 FILE1 L23;
 OB L24;
 dAM_EXPR L26;
 INT L271_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &test11);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,test1);
 L91_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L131_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L131_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &if_true);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L17 = ATTR(a,if_true);
 L181_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L17), 0, L181_);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name9);
 stdout_self4 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r4 = L19;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L221_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L221_);
 create_self5 = ((OUT) NULL);
 ret_val10 = create_self5;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &if_false);
 stdout_self5 = ((FILE1) NULL);
 L24=ZALLOC(sizeof(struct FILE1_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=FILE1_tag;
 L23 = ((FILE1) L24);
 r5 = L23;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
 L26 = ATTR(a,if_false);
 L271_=INTPLUS(indent11,12); 
 AM_OUT860401336(self, ((dAM) L26), 0, L271_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT991874317(AM_OUT self, AM_ROUT_DEF a, INT first1, INT indent11) {
 INT i = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i1 = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
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
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val24;
 FLISTA725283029 aget_self1;
 INT aget_ind1 = INT_zero;
 AM_LOCAL_EXPR ret_val25;
 OUT create_self11;
 OUT ret_val26;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val27;
 FILE1 r12;
 OUT create_self12;
 OUT ret_val28;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val29;
 FILE1 r13;
 extern STR voidsig;
 FILE1 L5;
 OB L6;
 extern STR sig11;
 FILE1 L8;
 OB L9;
 FILE1 L12;
 OB L13;
 extern STR needs_import;
 FILE1 L15;
 OB L16;
 extern STR needs_export;
 FILE1 L18;
 OB L19;
 extern STR isabstract;
 FILE1 L22;
 OB L23;
 extern STR name9;
 FILE1 L25;
 OB L26;
 INT L281_;
 INT L29;
 BOOL L301_;
 INT L321_;
 extern STR args1;
 FILE1 L33;
 OB L34;
 AM_FORMAL_ARG L361_;
 AM_FORMAL_ARG L37;
 INT L381_;
 INT L40;
 INT L421_;
 INT L43;
 INT L441_;
 INT L391_,L391_m;
 INT L45;
 AM_FORMAL_ARG L461_;
 AM_FORMAL_ARG L47;
 INT L481_;
 INT L49;
 INT L501_;
 INT L511_;
 BOOL L52;
 BOOL L531_;
 extern STR prefetch;
 FILE1 L54;
 OB L55;
 AM_LOCAL_EXPR L57;
 INT L581_;
 INT L59;
 INT L601_;
 extern STR name9;
 FILE1 L61;
 OB L62;
 INT L641_;
 extern STR voidlocals;
 FILE1 L65;
 OB L66;
 extern STR locals1;
 FILE1 L68;
 OB L69;
 AM_LOCAL_EXPR L721_;
 AM_LOCAL_EXPR L73;
 INT L741_;
 INT L75;
 INT L761_;
 INT L77;
 BOOL L781_;
 INT L80;
 INT L811_;
 INT L791_,L791_m;
 INT L82;
 AM_LOCAL_EXPR L831_;
 AM_LOCAL_EXPR L84;
 INT L851_;
 INT L86;
 INT L871_;
 INT L881_;
 extern STR voidcode;
 FILE1 L89;
 OB L90;
 extern STR code2;
 FILE1 L92;
 OB L93;
 dAM_STMT L95;
 INT L961_;
 if ((ATTR(a,sig1)==((SIG) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voidsig);
  stdout_self = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r = L5;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
 }
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &sig11);
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
  plus_s2 = ATTR(ATTR(a,sig1),str);
  stdout_self2 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r2 = L12;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 if (ATTR(ATTR(a,sig1),needs_import)) {
  create_self2 = ((OUT) NULL);
  ret_val6 = create_self2;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &needs_import);
  stdout_self3 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r3 = L15;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 if (ATTR(ATTR(a,sig1),needs_export)) {
  create_self3 = ((OUT) NULL);
  ret_val8 = create_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &needs_export);
  stdout_self4 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r4 = L18;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
 }
 if (ATTR(a,is_abstract)) {
  create_self4 = ((OUT) NULL);
  ret_val10 = create_self4;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &isabstract);
  stdout_self5 = ((FILE1) NULL);
  L23=ZALLOC(sizeof(struct FILE1_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=FILE1_tag;
  L22 = ((FILE1) L23);
  r5 = L22;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  return;
 }
 create_self5 = ((OUT) NULL);
 ret_val12 = create_self5;
 plus_self6 = ret_val12;
 plus_s6 = ((STR) &name9);
 stdout_self6 = ((FILE1) NULL);
 L26=ZALLOC(sizeof(struct FILE1_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 ((OB)L26)->header.tag=FILE1_tag;
 L25 = ((FILE1) L26);
 r6 = L25;
 
 SATTR(r6,fp,stdout);
 ret_val13 = r6;
 FILE_plus_STR(ret_val13, plus_s6);
 L281_=(a)==NULL?0:ASIZE((AM_ROUT_DEF)a); 
 L29 = L281_;
 L301_=(1)<(L29); 
 if (L301_) {
  L321_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L321_);
  create_self6 = ((OUT) NULL);
  ret_val14 = create_self6;
  plus_self7 = ret_val14;
  plus_s7 = ((STR) &args1);
  stdout_self7 = ((FILE1) NULL);
  L34=ZALLOC(sizeof(struct FILE1_struct));
  if (L34==NULL) FATAL("Unable to allocate more memory");
  ((OB)L34)->header.tag=FILE1_tag;
  L33 = ((FILE1) L34);
  r7 = L33;
  
  SATTR(r7,fp,stdout);
  ret_val15 = r7;
  FILE_plus_STR(ret_val15, plus_s7);
  L361_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)a,1); 
  L37 = L361_;
  L381_=INTPLUS(indent11,8); 
  AM_OUT860401336(self, ((dAM) L37), 0, L381_);
  i = 2;
  {
   BOOL f_L391_ = TRUE;
   while (1) {
    if (f_L391_) {
     f_L391_ = FALSE;
     L11 = 2;
     L421_=ASIZE((AM_ROUT_DEF)a); 
     L43 = L421_;
     L441_=INTMINUS(L43,1); 
     L21 = L441_;
     L391_=L11-1;L391_m=L21; 
    }
    if(L391_++>=L391_m)  goto after_loop; 
    L45 = L391_;
    L461_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)a,L45); 
    L47 = L461_;
    L481_=INTPLUS(indent11,8); 
    L49 = L481_;
    L501_=INTPLUS(indent11,8); 
    AM_OUT860401336(self, ((dAM) L47), L49, L501_);
   }
  }
  after_loop: ;
 }
 L511_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L511_);
 L52 = (ATTR(a,specul_prefetch)==((AM_LOCAL_EXPR) NULL));
 L531_=!(L52); 
 if (L531_) {
  create_self7 = ((OUT) NULL);
  ret_val16 = create_self7;
  plus_self8 = ret_val16;
  plus_s8 = ((STR) &prefetch);
  stdout_self8 = ((FILE1) NULL);
  L55=ZALLOC(sizeof(struct FILE1_struct));
  if (L55==NULL) FATAL("Unable to allocate more memory");
  ((OB)L55)->header.tag=FILE1_tag;
  L54 = ((FILE1) L55);
  r8 = L54;
  
  SATTR(r8,fp,stdout);
  ret_val17 = r8;
  FILE_plus_STR(ret_val17, plus_s8);
  L57 = ATTR(a,specul_prefetch);
  L581_=INTPLUS(indent11,10); 
  L59 = L581_;
  L601_=INTPLUS(indent11,10); 
  AM_OUT860401336(self, ((dAM) L57), L59, L601_);
  create_self8 = ((OUT) NULL);
  ret_val18 = create_self8;
  plus_self9 = ret_val18;
  plus_s9 = ((STR) &name9);
  stdout_self9 = ((FILE1) NULL);
  L62=ZALLOC(sizeof(struct FILE1_struct));
  if (L62==NULL) FATAL("Unable to allocate more memory");
  ((OB)L62)->header.tag=FILE1_tag;
  L61 = ((FILE1) L62);
  r9 = L61;
  
  SATTR(r9,fp,stdout);
  ret_val19 = r9;
  FILE_plus_STR(ret_val19, plus_s9);
  L641_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L641_);
 }
 if ((ATTR(a,locals1)==((FLISTA725283029) NULL))) {
  create_self9 = ((OUT) NULL);
  ret_val20 = create_self9;
  plus_self10 = ret_val20;
  plus_s10 = ((STR) &voidlocals);
  stdout_self10 = ((FILE1) NULL);
  L66=ZALLOC(sizeof(struct FILE1_struct));
  if (L66==NULL) FATAL("Unable to allocate more memory");
  ((OB)L66)->header.tag=FILE1_tag;
  L65 = ((FILE1) L66);
  r10 = L65;
  
  SATTR(r10,fp,stdout);
  ret_val21 = r10;
  FILE_plus_STR(ret_val21, plus_s10);
 }
 else {
  create_self10 = ((OUT) NULL);
  ret_val22 = create_self10;
  plus_self11 = ret_val22;
  plus_s11 = ((STR) &locals1);
  stdout_self11 = ((FILE1) NULL);
  L69=ZALLOC(sizeof(struct FILE1_struct));
  if (L69==NULL) FATAL("Unable to allocate more memory");
  ((OB)L69)->header.tag=FILE1_tag;
  L68 = ((FILE1) L69);
  r11 = L68;
  
  SATTR(r11,fp,stdout);
  ret_val23 = r11;
  FILE_plus_STR(ret_val23, plus_s11);
  i1 = 0;
  aget_self = ATTR(a,locals1);
  aget_ind = 0;
  L721_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
  ret_val24 = L721_;
  L73 = ret_val24;
  L741_=INTPLUS(indent11,10); 
  L75 = L741_;
  L761_=INTPLUS(indent11,10); 
  AM_OUT860401336(self, ((dAM) L73), L75, L761_);
  L77 = FLISTA1151473122(ATTR(a,locals1));
  L781_=(1)<(L77); 
  if (L781_) {
   {
    BOOL f_L791_ = TRUE;
    L31 = 1;
    L80 = FLISTA1151473122(ATTR(a,locals1));
    L811_=INTMINUS(L80,1); 
    L41 = L811_;
    L791_=L31-1;L791_m=L41; 
    while (1) {
     aget_self1 = ATTR(a,locals1);
     if(L791_++>=L791_m)  goto after_loop1; 
     aget_ind1 = L791_;
     L831_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self1,aget_ind1); 
     ret_val25 = L831_;
     L84 = ret_val25;
     L851_=INTPLUS(indent11,10); 
     L86 = L851_;
     L871_=INTPLUS(indent11,10); 
     AM_OUT860401336(self, ((dAM) L84), L86, L871_);
    }
   }
   after_loop1: ;
  }
 }
 L881_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L881_);
 if ((ATTR(a,code1)==((dAM_STMT) NULL))) {
  create_self11 = ((OUT) NULL);
  ret_val26 = create_self11;
  plus_self12 = ret_val26;
  plus_s12 = ((STR) &voidcode);
  stdout_self12 = ((FILE1) NULL);
  L90=ZALLOC(sizeof(struct FILE1_struct));
  if (L90==NULL) FATAL("Unable to allocate more memory");
  ((OB)L90)->header.tag=FILE1_tag;
  L89 = ((FILE1) L90);
  r12 = L89;
  
  SATTR(r12,fp,stdout);
  ret_val27 = r12;
  FILE_plus_STR(ret_val27, plus_s12);
 }
 else {
  create_self12 = ((OUT) NULL);
  ret_val28 = create_self12;
  plus_self13 = ret_val28;
  plus_s13 = ((STR) &code2);
  stdout_self13 = ((FILE1) NULL);
  L93=ZALLOC(sizeof(struct FILE1_struct));
  if (L93==NULL) FATAL("Unable to allocate more memory");
  ((OB)L93)->header.tag=FILE1_tag;
  L92 = ((FILE1) L93);
  r13 = L92;
  
  SATTR(r13,fp,stdout);
  ret_val29 = r13;
  FILE_plus_STR(ret_val29, plus_s13);
  L95 = ATTR(a,code1);
  L961_=INTPLUS(indent11,8); 
  AM_OUT860401336(self, ((dAM) L95), 0, L961_);
 }
}
