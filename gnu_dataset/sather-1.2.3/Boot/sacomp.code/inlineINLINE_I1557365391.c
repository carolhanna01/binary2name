#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

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

typedef struct CHANGE_VARS_struct {/* layout for CHANGE_VARS */
 OB_HEADER header;
 struct dAM_STMT_struct *init_hot;
 struct dAM_STMT_struct *init_nonhot;
 struct dAM_STMT_struct *init_parms;
 struct dAM_STMT_struct *init_vars;
 struct ARRAYINT_struct *pind_hot;
 struct ARRAYINT_struct *pind_nonhot;
 struct FLISTA725283029_struct *new_vars;
 struct FLISTA725283029_struct *saved_vars;
 struct FMAPAM1145008811_struct *new_subst;
 struct FMAPAM1145008811_struct *saved_subst;
 } *CHANGE_VARS;

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

typedef struct INLINE_INT_FOLD_struct {/* layout for INLINE_INT_FOLD */
 OB_HEADER header;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE_INT_FOLD;

typedef struct INLINE_ITER_struct {/* layout for INLINE_ITER */
 OB_HEADER header;
 struct dAM_EXPR_struct *yield_cond;
 struct dAM_EXPR_struct *yield_val;
 struct dAM_STMT_struct *BEFLOOP_code;
 struct dAM_STMT_struct *IINIT_code;
 struct dAM_STMT_struct *ITERM_code;
 struct dAM_STMT_struct *WNOY_code;
 struct dAM_STMT_struct *WYAFT_code;
 struct dAM_STMT_struct *WYBEF_code;
 struct CHANGE_VARS_struct *change_vars;
 struct FLISTA725283029_struct *loop_firsts;
 struct SIG_struct *signature;
 BOOL flat_yield;
 BOOL is_special;
 } *INLINE_ITER;

typedef struct INLINE_ROUT_struct {/* layout for INLINE_ROUT */
 OB_HEADER header;
 struct dAM_EXPR_struct *return_val;
 struct dAM_STMT_struct *code1;
 struct CHANGE_VARS_struct *change_vars;
 struct SIG_struct *signature;
 BOOL is_special;
 } *INLINE_ROUT;

typedef struct INLINE1164904870_struct {/* layout for INLINE_VATTR_WRITE */
 OB_HEADER header;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE1164904870;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct INT_fo686245905_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_fo686245905_frame;

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct INT_timesbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesbrINT_frame;

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

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

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

typedef struct TUPAM_1519040877_struct {/* layout for TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct AM_LOCAL_EXPR_struct *t2;
 } TUPAM_1519040877;
static TUPAM_1519040877 TUPAM_1519040877_zero;

typedef struct TUPAM_1519040877_boxed_struct {
 OB_HEADER header;
 TUPAM_1519040877 immutable_part;
 } *TUPAM_1519040877_boxed;

typedef struct WEIGH_CODE_struct {/* layout for WEIGH_CODE */
 OB_HEADER header;
 struct dAM_STMT_struct *badstmt;
 struct AM_IF_STMT_struct *enclosing_if;
 INT level1;
 INT max_weight;
 INT weight;
 BOOL true_branch;
 } *WEIGH_CODE;

typedef struct XFORM_CODE_struct {/* layout for XFORM_CODE */
 OB_HEADER header;
 struct dAM_EXPR_struct *return_val;
 struct dAM_EXPR_struct *yield_val;
 struct dAM_STMT_struct *after_yield;
 struct AM_IF_STMT_struct *enclosing_if;
 struct AM_IF_STMT_struct *if_encl_yield;
 struct AM_LOOP_STMT_struct *loop_stmt;
 struct AM_LOOP_STMT_struct *new_loop_stmt;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FMAPAM1145008811_struct *subst;
 BOOL record_calls;
 BOOL true_br_yield;
 BOOL true_branch;
 } *XFORM_CODE;

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

typedef struct AM_ARR2088295271_frame_struct {
 INT state;
 AM_ARRAY_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ARR2088295271_frame;

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

typedef struct AM_BND1502113605_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1502113605_frame;

typedef struct AM_BND2071008937_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND2071008937_frame;

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

typedef struct AM_BND23844014_frame_struct {
 INT state;
 AM_BND367211769 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND23844014_frame;

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

typedef struct AM_BND235024246_frame_struct {
 INT state;
 AM_BND260301329 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND235024246_frame;

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

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

typedef struct AM_EXT872579411_frame_struct {
 INT state;
 AM_EXT_CALL_EXPR self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_EXT872579411_frame;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

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

typedef struct AM_INITIAL_STMT_struct {/* layout for AM_INITIAL_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct dAM_STMT_struct *stmts;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_INITIAL_STMT;

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

typedef struct AM_ITE35020325_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE35020325_frame;

typedef struct AM_ITE756865079_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE756865079_frame;

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

typedef struct AM_VOID_CONST_struct {/* layout for AM_VOID_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_VOID_CONST;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

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

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FMAPAM1145008811_struct {/* layout for FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FMAPAM1145008811;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

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

extern IDENT IDENT_1837767448;
extern INOUT_MODE MODES_inout_mode;
extern INT INLINE1232309177;
extern INT INLINE1301544351;
extern INT INLINE1752510002;
extern INT INLINE1950426034;
extern INT INT_asize;
extern INT WEIGH_1480681287;
extern INT WEIGH_743880746;
extern INT WEIGH_803917570;
extern INT WEIGH_879105801;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUILTIN_bool;

/* Function declarations */


extern RETURNED_CONST FSETSIG (**dGENER354174443)(dGENERATE_AM);

extern RETURNED_CONST SFILE_ID (**dAM_EX2051891691)(dAM_EXPR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);
AM_CALL_ARG AM_BND1502113605(AM_BND1502113605_frame);
AM_CALL_ARG AM_BND745688768(AM_BND745688768_frame);
AM_CALL_ARG AM_BND956869000(AM_BND956869000_frame);
AM_CALL_ARG AM_EXT150734657(AM_EXT150734657_frame);
AM_CALL_ARG AM_ITE35020325(AM_ITE35020325_frame);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_LOCAL_EXPR FMAPAM1614132657(FMAPAM1145008811, AM_LOCAL_EXPR);
BOOL FSETSI722431140(FSETSIG, SIG);
BOOL INLINE1537257477(INLINE_ROUT, AM_ROUT_DEF, INT, PROG);
BOOL INLINE1554137872(INLINE_ROUT);
BOOL INLINE1738748633(INLINE1164904870);
BOOL INLINE1799378917(INLINE_INT_FOLD);
BOOL INLINE182665064(INLINE_ITER);
BOOL INLINE803584675(INLINE_ITER, AM_ROUT_DEF, INT, PROG);
BOOL INT_is240310584(INT, OB);
BOOL INT_is_nilrBOOL(INT);
BOOL STR_is111530248(STR, STR);
CHANGE_VARS CHANGE1489663732(CHANGE_VARS, AM_ROUT_DEF);
FLISTA1062334999 FLISTA943034044(FLISTA1062334999, AM_ITE1809135850);
FLISTA1409846210 FLISTA1715286767(FLISTA1409846210, AM_BND367211769);
FLISTA725283029 FLISTA1446031755(FLISTA725283029, INT);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FLISTdAM_STMT FLISTd846796316(FLISTdAM_STMT, INT);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTd355912233(FLISTdAM_STMT);
INT FSTR_sizerINT(FSTR);
INT INT_do676118316(INT_do676118316_frame);
INT INT_fo686245905(INT_fo686245905_frame);
INT INT_hashrINT(INT);
INT INT_hi740923800(INT);
INT INT_nilrINT(INT);
INT INT_pow_INTrINT(INT, INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INTI1 INTI_p1769163857(INTI1, INTI1);
IN_MODE IN_MOD403789248(IN_MODE);
SIG INLINE1199394416(INLINE_ITER);
SIG INLINE1334459112(INLINE_ROUT);
SIG INLINE1848276099(INLINE_INT_FOLD);
SIG INLINE1854659937(INLINE1164904870);
SIG SIG_ma1442297600(SIG);
STR INT_oc2110318348(INT);
STR INT_strrSTR(INT);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
dAM_EXPR AM_ARR535890289(AM_ARR535890289_frame);
dAM_EXPR INLINE1324370258(INLINE_INT_FOLD, AM_ROU1916046290);
dAM_EXPR INLINE1736265130(INLINE1164904870, AM_ROU1916046290);
dAM_EXPR INLINE1949170047(INLINE_ROUT, AM_ROU1916046290);
dAM_EXPR INLINE986164897(INLINE_ITER, AM_ITE1809135850);
dAM_EXPR XFORM_69815265(XFORM_CODE, dAM_EXPR);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dAM_STMT XFORM_1427763452(XFORM_CODE, dAM_STMT);
dAM_STMT XFORM_1958700984(XFORM_CODE, dAM_STMT);
void AM_ARR2088295271(AM_ARR2088295271_frame);
void AM_ASS1281039639(AM_ASSIGN_STMT, dAM_STMT);
void AM_BND2071008937(AM_BND2071008937_frame);
void AM_BND235024246(AM_BND235024246_frame);
void AM_BND23844014(AM_BND23844014_frame);
void AM_EXT872579411(AM_EXT872579411_frame);
void AM_ITE756865079(AM_ITE756865079_frame);
void AM_ROU545684847(AM_ROU545684847_frame);
void CHANGE1122717432(CHANGE_VARS, SFILE_ID);
void FILE_plus_STR(FILE1, STR);
void FSTR_to_reverse(FSTR);
void INLINE15806555(INLINE_ITER, AM_ROUT_DEF);
void INT_timesb(INT_timesb_frame);
void WEIGH_1152361671(WEIGH_CODE, dAM_STMT);
void WEIGH_1472719854(WEIGH_CODE, dAM_EXPR);
void WEIGH_1908424532(WEIGH_CODE, dAM_STMT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL INLINE1537257477(INLINE_ROUT self, AM_ROUT_DEF rout, INT threshold1, PROG prog) {
 BOOL ret_val = BOOL_zero;
 WEIGH_CODE sb;
 dAM_STMT badstmt;
 WEIGH_CODE create_self;
 INT create_threshold = INT_zero;
 WEIGH_CODE ret_val1;
 WEIGH_CODE r;
 WEIGH_CODE clear_badstmt_se;
 dGENERATE_AM L1;
 FSETSIG L2;
 WEIGH_CODE L3;
 OB L4;
 INT L5;
 BOOL L61_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L111_;
 INT L13;
 BOOL L141_;
 L1 = ATTR(prog,generate_am);
 L2 = (*dGENER354174443[TAG(L1)])(L1);
 if (FSETSI722431140(L2, ATTR(rout,sig1))) {
  ret_val = FALSE;
  return ret_val;
 }
 create_self = ((WEIGH_CODE) NULL);
 create_threshold = threshold1;
 L4=ZALLOC(sizeof(struct WEIGH_CODE_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=WEIGH_CODE_tag;
 L3 = ((WEIGH_CODE) L4);
 r = L3;
 SATTR(r,max_weight,create_threshold);
 SATTR(r,weight,0);
 SATTR(r,level1,0);
 ret_val1 = r;
 sb = ret_val1;
 WEIGH_1152361671(sb, ATTR(rout,code1));
 L5 = ATTR(sb,weight);
 L61_=(threshold1)<(L5); 
 if (L61_) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 badstmt = ATTR(sb,badstmt);
 clear_badstmt_se = sb;
 SATTR(clear_badstmt_se,badstmt,((dAM_STMT) NULL));
 L8 = (badstmt==((dAM_STMT) NULL));
 L91_=!(L8); 
 if (L91_) {
  if (badstmt==NULL) {
   goto other457;
  } else
  switch (TAG(badstmt)) {
   case AM_RETURN_STMT_tag:
    L10 = ATTR(sb,level1);
    L111_=(1)<(L10); 
    if (L111_) {
     SYSDESTROY(sb); 
     ;
     ret_val = FALSE;
     return ret_val;
    }
    WEIGH_1472719854(sb, ATTR(((AM_RETURN_STMT) badstmt),val1));
    L13 = ATTR(sb,weight);
    L141_=(threshold1)<(L13); 
    if (L141_) {
     SYSDESTROY(sb); 
     ;
     ret_val = FALSE;
     return ret_val;
    } break;
   default: ;
   other457: ;
    SYSDESTROY(sb); 
    ;
    ret_val = FALSE;
    return ret_val;
  }
 }
 SYSDESTROY(sb); 
 ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE1554137872(INLINE_ROUT self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE1738748633(INLINE1164904870 self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE1799378917(INLINE_INT_FOLD self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE182665064(INLINE_ITER self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE803584675(INLINE_ITER self, AM_ROUT_DEF rout, INT threshold1, PROG prog) {
 BOOL ret_val = BOOL_zero;
 WEIGH_CODE sb;
 AM_LOOP_STMT loopstmt = ((AM_LOOP_STMT) NULL);
 dAM_STMT badstmt;
 AM_YIELD_STMT yieldstmt = ((AM_YIELD_STMT) NULL);
 WEIGH_CODE create_self;
 INT create_threshold = INT_zero;
 WEIGH_CODE ret_val1;
 WEIGH_CODE r;
 WEIGH_CODE clear_badstmt_se;
 WEIGH_CODE clear_badstmt_se1;
 WEIGH_CODE uplevel_self;
 WEIGH_CODE uplevel_self1;
 WEIGH_CODE uplevel_self2;
 dGENERATE_AM L1;
 FSETSIG L2;
 WEIGH_CODE L3;
 OB L4;
 INT L5;
 BOOL L61_;
 BOOL L8;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L17;
 BOOL L181_;
 INT L20;
 BOOL L211_;
 BOOL L22;
 BOOL L231_;
 INT L26;
 BOOL L271_;
 BOOL L29;
 BOOL L30;
 BOOL L311_;
 INT L32;
 BOOL L331_;
 INT L35;
 INT L361_;
 INT L37;
 BOOL L381_;
 BOOL L39;
 BOOL L40;
 BOOL L411_;
 INT L42;
 BOOL L431_;
 BOOL L45;
 BOOL L46;
 BOOL L471_;
 INT L48;
 BOOL L491_;
 INT L51;
 INT L521_;
 BOOL L53;
 BOOL L541_;
 INT L56;
 INT L571_;
 L1 = ATTR(prog,generate_am);
 L2 = (*dGENER354174443[TAG(L1)])(L1);
 if (FSETSI722431140(L2, ATTR(rout,sig1))) {
  ret_val = FALSE;
  return ret_val;
 }
 create_self = ((WEIGH_CODE) NULL);
 create_threshold = threshold1;
 L4=ZALLOC(sizeof(struct WEIGH_CODE_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=WEIGH_CODE_tag;
 L3 = ((WEIGH_CODE) L4);
 r = L3;
 SATTR(r,max_weight,create_threshold);
 SATTR(r,weight,0);
 SATTR(r,level1,0);
 ret_val1 = r;
 sb = ret_val1;
 WEIGH_1152361671(sb, ATTR(rout,code1));
 L5 = ATTR(sb,weight);
 L61_=(threshold1)<(L5); 
 if (L61_) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 badstmt = ATTR(sb,badstmt);
 clear_badstmt_se = sb;
 SATTR(clear_badstmt_se,badstmt,((dAM_STMT) NULL));
 if (badstmt==NULL) {
  goto other454;
 } else
 switch (TAG(badstmt)) {
  case AM_LOOP_STMT_tag:
   L9 = ATTR(sb,level1);
   L101_=(L9)==(1); 
   L11 = L101_;
   L121_=!(L11); 
   if (L121_) {
    L8 = TRUE;
   } else {
    L13 = ATTR(((AM_LOOP_STMT) badstmt),has_yield);
    L141_=!(L13); 
    L8 = L141_;
   }
   if (L8) {
    SYSDESTROY(sb); 
    ;
    ret_val = FALSE;
    return ret_val;
   }
   else {
    loopstmt = ((AM_LOOP_STMT) badstmt);
   } break;
  default: ;
  other454: ;
   SYSDESTROY(sb); 
   ;
   ret_val = FALSE;
   return ret_val;
 }
 WEIGH_1152361671(sb, ATTR(loopstmt,body));
 L17 = ATTR(sb,weight);
 L181_=(threshold1)<(L17); 
 if (L181_) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 badstmt = ATTR(sb,badstmt);
 clear_badstmt_se1 = sb;
 SATTR(clear_badstmt_se1,badstmt,((dAM_STMT) NULL));
 if (badstmt==NULL) {
  goto other455;
 } else
 switch (TAG(badstmt)) {
  case AM_YIELD_STMT_tag:
   L20 = ATTR(sb,level1);
   L211_=(L20)==(2); 
   L22 = L211_;
   L231_=!(L22); 
   if (L231_) {
    SYSDESTROY(sb); 
    ;
    ret_val = FALSE;
    return ret_val;
   }
   else {
    yieldstmt = ((AM_YIELD_STMT) badstmt);
   } break;
  default: ;
  other455: ;
   SYSDESTROY(sb); 
   ;
   ret_val = FALSE;
   return ret_val;
 }
 WEIGH_1472719854(sb, ATTR(yieldstmt,val1));
 L26 = ATTR(sb,weight);
 L271_=(threshold1)<(L26); 
 if (L271_) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 WEIGH_1152361671(sb, ATTR(yieldstmt,next));
 L30 = (ATTR(sb,badstmt)==((dAM_STMT) NULL));
 L311_=!(L30); 
 if (L311_) {
  L29 = TRUE;
 } else {
  L32 = ATTR(sb,weight);
  L331_=(threshold1)<(L32); 
  L29 = L331_;
 }
 if (L29) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 uplevel_self = sb;
 L35 = ATTR(uplevel_self,level1);
 L361_=INTMINUS(L35,1); 
 SATTR(uplevel_self,level1,L361_);
 L37 = ATTR(sb,level1);
 L381_=(L37)==(2); 
 if (L381_) {
  if (ATTR(sb,true_branch)) {
   WEIGH_1152361671(sb, ATTR(ATTR(sb,enclosing_if),if_false));
  }
  else {
   WEIGH_1152361671(sb, ATTR(ATTR(sb,enclosing_if),if_true));
  }
  L40 = (ATTR(sb,badstmt)==((dAM_STMT) NULL));
  L411_=!(L40); 
  if (L411_) {
   L39 = TRUE;
  } else {
   L42 = ATTR(sb,weight);
   L431_=(threshold1)<(L42); 
   L39 = L431_;
  }
  if (L39) {
   SYSDESTROY(sb); 
   ;
   ret_val = FALSE;
   return ret_val;
  }
  WEIGH_1152361671(sb, ATTR(ATTR(sb,enclosing_if),next));
  L46 = (ATTR(sb,badstmt)==((dAM_STMT) NULL));
  L471_=!(L46); 
  if (L471_) {
   L45 = TRUE;
  } else {
   L48 = ATTR(sb,weight);
   L491_=(threshold1)<(L48); 
   L45 = L491_;
  }
  if (L45) {
   SYSDESTROY(sb); 
   ;
   ret_val = FALSE;
   return ret_val;
  }
  uplevel_self1 = sb;
  L51 = ATTR(uplevel_self1,level1);
  L521_=INTMINUS(L51,1); 
  SATTR(uplevel_self1,level1,L521_);
 }
 L53 = (ATTR(loopstmt,next)==((dAM_STMT) NULL));
 L541_=!(L53); 
 if (L541_) {
  SYSDESTROY(sb); 
  ;
  ret_val = FALSE;
  return ret_val;
 }
 uplevel_self2 = sb;
 L56 = ATTR(uplevel_self2,level1);
 L571_=INTMINUS(L56,1); 
 SATTR(uplevel_self2,level1,L571_);
 SYSDESTROY(sb); 
 ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INT_is240310584(INT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 INT L1;
 BOOL L21_;
 if (arg==NULL) {
  goto other168;
 } else
 switch (TAG(arg)) {
  case INT_tag:
   L1 = ((INT_boxed) arg)->immutable_part;
   L21_=(self)==(L1); 
   ret_val = L21_;
   return ret_val; break;
  default: ;
  other168: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL INT_is_nilrBOOL(INT self) {
 BOOL ret_val = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 L1 = INT_asize;
 L21_=INTMINUS(L1,1); 
 L3 = L21_;
 L41_=((unsigned)1)<<((unsigned)L3); 
 L5 = L41_;
 L61_=(self)==(L5); 
 ret_val = L61_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR INT_st367594495(INT self, FSTR s, INT n, INT b1, CHAR f) {
 FSTR ret_val;
 INT x = INT_zero;
 INT divid = INT_zero;
 INT rem1 = INT_zero;
 INT rem = INT_zero;
 CHAR first_char = CHAR_zero;
 INT i = INT_zero;
 INT j = INT_zero;
 CHAR ch = CHAR_zero;
 INT x1 = INT_zero;
 INT i1 = INT_zero;
 INT j11 = INT_zero;
 CHAR ch1 = CHAR_zero;
 INT is_nil_self = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 INT digit_char_self = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR length_self;
 INT ret_val3 = INT_zero;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val4;
 INT digit_char_self1 = INT_zero;
 CHAR ret_val5 = CHAR_zero;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val6;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val7;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val8;
 FSTR length_self1;
 INT ret_val9 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val10 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aset_self;
 INT aset_ind = INT_zero;
 CHAR aset_val = CHAR_zero;
 FSTR aset_self1;
 INT aset_ind1 = INT_zero;
 CHAR aset_val1 = CHAR_zero;
 FSTR length_self2;
 INT ret_val12 = INT_zero;
 INT digit_char_self2 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR plus_self4;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val14;
 FSTR plus_self5;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val15;
 FSTR plus_self6;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val16;
 FSTR length_self3;
 INT ret_val17 = INT_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val18 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val19 = CHAR_zero;
 FSTR aset_self2;
 INT aset_ind2 = INT_zero;
 CHAR aset_val2 = CHAR_zero;
 FSTR aset_self3;
 INT aset_ind3 = INT_zero;
 CHAR aset_val3 = CHAR_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 INT L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 INT L131_;
 BOOL L141_;
 INT L151_;
 INT L161_;
 INT L171_;
 extern STR S012341320448133;
 CHAR L181_;
 INT L191_;
 INT L201_;
 INT L221_;
 extern STR S012341320448133;
 CHAR L231_;
 INT L241_;
 INT L251_;
 BOOL L261_;
 INT L271_;
 BOOL L281_;
 INT L291_;
 INT L30;
 INT L311_;
 BOOL L321_;
 CHAR L331_;
 CHAR L341_;
 INT L371_;
 INT L381_;
 INT L391_;
 INT L401_;
 extern STR S012341320448133;
 CHAR L421_;
 INT L431_;
 INT L441_;
 BOOL L451_;
 BOOL L461_;
 INT L471_;
 BOOL L481_;
 INT L491_;
 INT L50;
 INT L511_;
 BOOL L521_;
 CHAR L531_;
 CHAR L541_;
 INT L571_;
 INT L581_;
 is_nil_self = self;
 L1 = INT_asize;
 L21_=INTMINUS(L1,1); 
 L3 = L21_;
 L41_=((unsigned)1)<<((unsigned)L3); 
 L5 = L41_;
 L61_=(is_nil_self)==(L5); 
 ret_val1 = L61_;
 if (ret_val1) {
  x = self;
  L71_=INTDIV(x,b1); 
  divid = L71_;
  L81_=INTMINUS(b1,1); 
  L9 = L81_;
  L101_=INTTIMES(divid,L9); 
  L11 = L101_;
  L121_=INTMINUS(x,L11); 
  rem1 = L121_;
  L131_=INTMINUS(rem1,divid); 
  rem = L131_;
  L141_=(0)<(rem); 
  if (L141_) {
   L151_=INTMINUS(rem,b1); 
   rem = L151_;
   L161_=INTPLUS(divid,1); 
   divid = L161_;
  }
  L171_=(rem<0)?-rem:rem; 
  digit_char_self = L171_;
  L181_=ARR((STR)((STR) &S012341320448133),digit_char_self); 
  ret_val2 = L181_;
  first_char = ret_val2;
  L191_=(divid<0)?-divid:divid; 
  x = L191_;
  length_self = s;
  ret_val3 = FSTR_sizerINT(length_self);
  i = ret_val3;
  plus_self = s;
  plus_c = first_char;
  ret_val4 = FSTR_p399773021(plus_self, plus_c);
  s = ret_val4;
  L201_=INTMINUS(n,1); 
  n = L201_;
  while (1) {
   plus_self1 = s;
   L221_=INTMOD(x,b1); 
   digit_char_self1 = L221_;
   L231_=ARR((STR)((STR) &S012341320448133),digit_char_self1); 
   ret_val5 = L231_;
   plus_c1 = ret_val5;
   ret_val6 = FSTR_p399773021(plus_self1, plus_c1);
   s = ret_val6;
   L241_=INTDIV(x,b1); 
   x = L241_;
   L251_=INTMINUS(n,1); 
   n = L251_;
   L261_=(x)==(0); 
   if (L261_) {
    goto after_loop;
   }
  }
  after_loop: ;
  plus_self2 = s;
  plus_c2 = '-';
  ret_val7 = FSTR_p399773021(plus_self2, plus_c2);
  s = ret_val7;
  L271_=INTMINUS(n,1); 
  n = L271_;
  while (1) {
   L281_=(0)<(n); 
   if (L281_) {
   }
   else {
    goto after_loop1;
   }
   plus_self3 = s;
   plus_c3 = f;
   ret_val8 = FSTR_p399773021(plus_self3, plus_c3);
   s = ret_val8;
   L291_=INTMINUS(n,1); 
   n = L291_;
  }
  after_loop1: ;
  length_self1 = s;
  ret_val9 = FSTR_sizerINT(length_self1);
  L30 = ret_val9;
  L311_=INTMINUS(L30,1); 
  j = L311_;
  while (1) {
   L321_=(i)<(j); 
   if (L321_) {
   }
   else {
    goto after_loop2;
   }
   aget_self = s;
   aget_ind = i;
   L331_=ARR((FSTR)aget_self,aget_ind); 
   ret_val10 = L331_;
   ch = ret_val10;
   aset_self = s;
   aset_ind = i;
   aget_self1 = s;
   aget_ind1 = j;
   L341_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val11 = L341_;
   aset_val = ret_val11;
   SARR((FSTR)aset_self,aset_ind,aset_val); 
   ;
   aset_self1 = s;
   aset_ind1 = j;
   aset_val1 = ch;
   SARR((FSTR)aset_self1,aset_ind1,aset_val1); 
   ;
   L371_=INTPLUS(i,1); 
   i = L371_;
   L381_=INTMINUS(j,1); 
   j = L381_;
  }
  after_loop2: ;
  ret_val = s;
  return ret_val;
 }
 else {
  L391_=(self<0)?-self:self; 
  x1 = L391_;
  length_self2 = s;
  ret_val12 = FSTR_sizerINT(length_self2);
  i1 = ret_val12;
  while (1) {
   plus_self4 = s;
   L401_=INTMOD(x1,b1); 
   digit_char_self2 = L401_;
   L421_=ARR((STR)((STR) &S012341320448133),digit_char_self2); 
   ret_val13 = L421_;
   plus_c4 = ret_val13;
   ret_val14 = FSTR_p399773021(plus_self4, plus_c4);
   s = ret_val14;
   L431_=INTDIV(x1,b1); 
   x1 = L431_;
   L441_=INTMINUS(n,1); 
   n = L441_;
   L451_=(x1)==(0); 
   if (L451_) {
    goto after_loop3;
   }
  }
  after_loop3: ;
  L461_=(self)<(0); 
  if (L461_) {
   plus_self5 = s;
   plus_c5 = '-';
   ret_val15 = FSTR_p399773021(plus_self5, plus_c5);
   s = ret_val15;
   L471_=INTMINUS(n,1); 
   n = L471_;
  }
  while (1) {
   L481_=(0)<(n); 
   if (L481_) {
   }
   else {
    goto after_loop4;
   }
   plus_self6 = s;
   plus_c6 = f;
   ret_val16 = FSTR_p399773021(plus_self6, plus_c6);
   s = ret_val16;
   L491_=INTMINUS(n,1); 
   n = L491_;
  }
  after_loop4: ;
  length_self3 = s;
  ret_val17 = FSTR_sizerINT(length_self3);
  L50 = ret_val17;
  L511_=INTMINUS(L50,1); 
  j11 = L511_;
  while (1) {
   L521_=(i1)<(j11); 
   if (L521_) {
   }
   else {
    goto after_loop5;
   }
   aget_self2 = s;
   aget_ind2 = i1;
   L531_=ARR((FSTR)aget_self2,aget_ind2); 
   ret_val18 = L531_;
   ch1 = ret_val18;
   aset_self2 = s;
   aset_ind2 = i1;
   aget_self3 = s;
   aget_ind3 = j11;
   L541_=ARR((FSTR)aget_self3,aget_ind3); 
   ret_val19 = L541_;
   aset_val2 = ret_val19;
   SARR((FSTR)aset_self2,aset_ind2,aset_val2); 
   ;
   aset_self3 = s;
   aset_ind3 = j11;
   aset_val3 = ch1;
   SARR((FSTR)aset_self3,aset_ind3,aset_val3); 
   ;
   L571_=INTPLUS(i1,1); 
   i1 = L571_;
   L581_=INTMINUS(j11,1); 
   j11 = L581_;
  }
  after_loop5: ;
  ret_val = s;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT INT_hashrINT(INT self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L41_;
 INT L5;
 INT L61_;
 INT L71_;
 INT L8;
 INT L91_;
 INT L101_;
 INT L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 r = self;
 L11_=((unsigned)r)<<((unsigned)17); 
 L2 = L11_;
 L31_=r^L2; 
 r = L31_;
 L41_=((unsigned)r)>>((unsigned)15); 
 L5 = L41_;
 L61_=r^L5; 
 r = L61_;
 L71_=((unsigned)r)<<((unsigned)17); 
 L8 = L71_;
 L91_=r^L8; 
 r = L91_;
 L101_=((unsigned)r)>>((unsigned)15); 
 L12 = L101_;
 L131_=r^L12; 
 r = L131_;
 L141_=((unsigned)r)<<((unsigned)17); 
 L15 = L141_;
 L161_=r^L15; 
 r = L161_;
 L171_=((unsigned)r)>>((unsigned)15); 
 L18 = L171_;
 L191_=r^L18; 
 r = L191_;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT INT_hi740923800(INT self) {
 INT ret_val = INT_zero;
 INT x = INT_zero;
 INT z = INT_zero;
 INT r = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 BOOL L31_;
 INT L4;
 BOOL L51_;
 INT L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 INT L121_;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 INT L161_;
 INT L171_;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 INT L221_;
 INT L231_;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 INT L271_;
 INT L281_;
 BOOL L291_;
 BOOL L30;
 BOOL L321_;
 INT L331_;
 INT L35;
 INT L361_;
 INT L341_,L341_m;
 INT L37;
 INT L381_;
 INT L39;
 BOOL L401_;
 INT L411_;
 INT L42;
 INT L431_;
 L31_=(self)==(0); 
 if (L31_) {
  ret_val = -1;
  return ret_val;
 }
 L4 = INT_asize;
 L51_=(L4)==(32); 
 if (L51_) {
  x = self;
  L61_=((unsigned)x)>>((unsigned)16); 
  z = L61_;
  L71_=(z)==(0); 
  L8 = L71_;
  L91_=!(L8); 
  if (L91_) {
   x = z;
   L101_=INTPLUS(r,16); 
   r = L101_;
  }
  L121_=((unsigned)x)>>((unsigned)8); 
  z = L121_;
  L131_=(z)==(0); 
  L14 = L131_;
  L151_=!(L14); 
  if (L151_) {
   x = z;
   L161_=INTPLUS(r,8); 
   r = L161_;
  }
  L171_=((unsigned)x)>>((unsigned)4); 
  z = L171_;
  L181_=(z)==(0); 
  L19 = L181_;
  L201_=!(L19); 
  if (L201_) {
   x = z;
   L221_=INTPLUS(r,4); 
   r = L221_;
  }
  L231_=((unsigned)x)>>((unsigned)2); 
  z = L231_;
  L241_=(z)==(0); 
  L25 = L241_;
  L261_=!(L25); 
  if (L261_) {
   x = z;
   L271_=INTPLUS(r,2); 
   r = L271_;
  }
  L281_=((unsigned)x)>>((unsigned)1); 
  z = L281_;
  L291_=(z)==(0); 
  L30 = L291_;
  L321_=!(L30); 
  if (L321_) {
   x = z;
   L331_=INTPLUS(r,1); 
   r = L331_;
  }
  ret_val = r;
  return ret_val;
 }
 else {
  {
   BOOL f_L341_ = TRUE;
   L11 = 1;
   L35 = INT_asize;
   L361_=INTMINUS(L35,1); 
   L21 = L361_;
   L341_=L11-1;L341_m=L21; 
   while (1) {
    if(L341_++>=L341_m)  goto after_loop; 
    i = L341_;
    L381_=INTRSHIFT(self,i); 
    L39 = L381_;
    L401_=(L39)==(0); 
    if (L401_) {
     L411_=INTMINUS(i,1); 
     ret_val = L411_;
     return ret_val;
    }
   }
  }
  after_loop: ;
  L42 = INT_asize;
  L431_=INTMINUS(L42,1); 
  ret_val = L431_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT INT_nilrINT(INT self) {
 INT ret_val = INT_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 L1 = INT_asize;
 L21_=INTMINUS(L1,1); 
 L3 = L21_;
 L41_=((unsigned)1)<<((unsigned)L3); 
 ret_val = L41_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT INT_pow_INTrINT(INT self, INT i) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 INT L11 = INT_zero;
 INT x = INT_zero;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L61_;
 INT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L121_;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L241_;
 INT L251_;
 INT L261_;
 INT L271_;
 INT L281_;
 INT L291_;
 INT L30;
 INT L321_;
 INT L331_;
 INT L341_;
 INT L35;
 INT L361_;
 INT L371_;
 BOOL L381_;
 INT L391_;
 INT L401_;
 BOOL L411_;
 INT L421_;
 L11 = i;
 switch (L11) {
  case 0: 
   ret_val = 1;
   return ret_val;
   break;
  case 1: 
   ret_val = self;
   return ret_val;
   break;
  case 2: 
   L21_=INTTIMES(self,self); 
   ret_val = L21_;
   return ret_val;
   break;
  case 3: 
   L31_=INTTIMES(self,self); 
   L4 = L31_;
   L51_=INTTIMES(L4,self); 
   ret_val = L51_;
   return ret_val;
   break;
  case 4: 
   L61_=INTTIMES(self,self); 
   r = L61_;
   L71_=INTTIMES(r,r); 
   ret_val = L71_;
   return ret_val;
   break;
  case 5: 
   L81_=INTTIMES(self,self); 
   r = L81_;
   L91_=INTTIMES(self,r); 
   L10 = L91_;
   L121_=INTTIMES(L10,r); 
   ret_val = L121_;
   return ret_val;
   break;
  case 6: 
   L131_=INTTIMES(self,self); 
   r = L131_;
   L141_=INTTIMES(r,r); 
   L15 = L141_;
   L161_=INTTIMES(L15,r); 
   ret_val = L161_;
   return ret_val;
   break;
  case 7: 
   L171_=INTTIMES(self,self); 
   r = L171_;
   L181_=INTTIMES(self,r); 
   L19 = L181_;
   L201_=INTTIMES(L19,r); 
   L22 = L201_;
   L231_=INTTIMES(L22,r); 
   ret_val = L231_;
   return ret_val;
   break;
  case 8: 
   L241_=INTTIMES(self,self); 
   r = L241_;
   L251_=INTTIMES(r,r); 
   r = L251_;
   L261_=INTTIMES(r,r); 
   ret_val = L261_;
   return ret_val;
   break;
  case 9: 
   L271_=INTTIMES(self,self); 
   r = L271_;
   L281_=INTTIMES(r,r); 
   r = L281_;
   L291_=INTTIMES(self,r); 
   L30 = L291_;
   L321_=INTTIMES(L30,r); 
   ret_val = L321_;
   return ret_val;
   break;
  case 10: 
   L331_=INTTIMES(self,self); 
   r = L331_;
   L341_=INTTIMES(self,r); 
   L35 = L341_;
   L361_=INTTIMES(L35,r); 
   r = L361_;
   L371_=INTTIMES(r,r); 
   ret_val = L371_;
   return ret_val;
   break;
  default: ;
   x = self;
   r = 1;
   while (1) {
    L381_=(i&1)!=0; 
    if (L381_) {
     L391_=INTTIMES(r,x); 
     r = L391_;
    }
    L401_=INTRSHIFT(i,1); 
    i = L401_;
    L411_=(0)<(i); 
    if (L411_) {
    }
    else {
     goto after_loop;
    }
    L421_=INTTIMES(x,x); 
    x = L421_;
   }
   after_loop: ;
   ret_val = r;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1199394416(INLINE_ITER self) {
 SIG ret_val;
 ret_val = ATTR(self,signature);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1334459112(INLINE_ROUT self) {
 SIG ret_val;
 ret_val = ATTR(self,signature);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1848276099(INLINE_INT_FOLD self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1854659937(INLINE1164904870 self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

STR INT_oc2110318348(INT self) {
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 INT i = INT_zero;
 FSTR clear_self;
 INT digit_char_self = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 FSTR str_self;
 STR ret_val3;
 BOOL L1;
 BOOL L21_;
 INT L31_;
 extern STR S012341320448133;
 CHAR L41_;
 INT L51_;
 BOOL L61_;
 extern STR o0;
 clear_self = buf1;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 i = self;
 while (1) {
  plus_self = buf1;
  L31_=i&7; 
  digit_char_self = L31_;
  L41_=ARR((STR)((STR) &S012341320448133),digit_char_self); 
  ret_val1 = L41_;
  plus_c = ret_val1;
  ret_val2 = FSTR_p399773021(plus_self, plus_c);
  buf1 = ret_val2;
  L51_=((unsigned)i)>>((unsigned)3); 
  i = L51_;
  L61_=(i)==(0); 
  if (L61_) {
   goto after_loop;
  }
 }
 after_loop: ;
 buf1 = FSTR_p1752847026(buf1, ((STR) &o0));
 FSTR_to_reverse(buf1);
 str_self = buf1;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR INT_strrSTR(INT self) {
 STR ret_val;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val1;
 FSTR str_self;
 STR ret_val2;
 BOOL L1;
 BOOL L21_;
 clear_self = buf1;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = self;
 str_in_s = buf1;
 ret_val1 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val1;
 str_self = buf1;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1324370258(INLINE_INT_FOLD self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 dAM_EXPR arg11;
 dAM_EXPR arg2;
 AM_INT_CONST r;
 AM_INT_CONST create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_INT_CONST ret_val1;
 AM_INT_CONST r1;
 AM_CALL_ARG L11_;
 AM_CALL_ARG L2;
 AM_CALL_ARG L31_;
 AM_CALL_ARG L4;
 AM_INT_CONST L5;
 OB L6;
 INT L7;
 INT L81_;
 L11_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,0); 
 L2=L11_;
 arg11 = ATTR(L2,expr);
 L31_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,1); 
 L4=L31_;
 arg2 = ATTR(L4,expr);
 if (arg11==NULL) {
  goto other289;
 } else
 switch (TAG(arg11)) {
  case AM_INT_CONST_tag:
   if (arg2==NULL) {
    goto other290;
   } else
   switch (TAG(arg2)) {
    case AM_INT_CONST_tag:
     create_self = ((AM_INT_CONST) NULL);
     create_source = ATTR(((AM_INT_CONST) arg11),source1);
     L6=ZALLOC(sizeof(struct AM_INT_CONST_struct));
     if (L6==NULL) FATAL("Unable to allocate more memory");
     ((OB)L6)->header.tag=AM_INT_CONST_tag;
     L5 = ((AM_INT_CONST) L6);
     r1 = L5;
     SATTR(r1,source1,create_source);
     ret_val1 = r1;
     r = ret_val1;
     SATTR(r,val1,INTI_p1769163857(ATTR(((AM_INT_CONST) arg11),val1), ATTR(((AM_INT_CONST) arg2),val1)));
     SATTR(r,tp_at,ATTR(((AM_INT_CONST) arg11),tp_at));
     L7 = INLINE1950426034;
     L81_=INTPLUS(L7,1); 
     INLINE1950426034 = L81_;
     ret_val = ((dAM_EXPR) r);
     return ret_val; break;
    default: ;
    other290: ;
   } break;
  default: ;
  other289: ;
 }
 ret_val = ((dAM_EXPR) call11);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1736265130(INLINE1164904870 self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_VAT319982528 r;
 AM_VAT319982528 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VAT319982528 ret_val1;
 AM_VAT319982528 r1;
 INT L1;
 INT L21_;
 AM_VAT319982528 L3;
 OB L4;
 AM_CALL_ARG L51_;
 AM_CALL_ARG L6;
 AM_CALL_ARG L71_;
 AM_CALL_ARG L8;
 ARRAYARG L9;
 ARG L101_;
 ARG L11;
 L1 = INLINE1752510002;
 L21_=INTPLUS(L1,1); 
 INLINE1752510002 = L21_;
 create_self = ((AM_VAT319982528) NULL);
 create_source = ATTR(call11,source1);
 L4=ZALLOC(sizeof(struct AM_VAT319982528_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_VAT319982528_tag;
 L3 = ((AM_VAT319982528) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L51_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,0); 
 L6=L51_;
 SATTR(r,ob,ATTR(L6,expr));
 L71_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,1); 
 L8=L71_;
 SATTR(r,val1,ATTR(L8,expr));
 SATTR(r,at,ATTR(self,at));
 L9 = ATTR(ATTR(call11,fun),args);
 L101_=(ARG)ARR((ARRAYARG)L9,0); 
 L11=L101_;
 SATTR(r,real_tp,ATTR(L11,tp));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1949170047(INLINE_ROUT self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_STMT_EXPR se;
 XFORM_CODE xc;
 dAM_STMT init_parms;
 dAM_STMT s;
 INT i = INT_zero;
 dTP stp = ((dTP) NULL);
 AM_ROU1916046290 L11;
 AM_CALL_ARG ca;
 AM_VOID_CONST vconst;
 AM_ROU1916046290 L21;
 AM_CALL_ARG ca1;
 AM_ASSIGN_STMT cp_stmt;
 AM_STMT_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val1;
 AM_STMT_EXPR r;
 XFORM_CODE create_self1;
 FMAPAM1145008811 create_subst;
 XFORM_CODE ret_val2;
 XFORM_CODE xc1;
 AM_CALL_ARG source_self;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 AM_VOID_CONST create_self2;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_VOID_CONST ret_val4;
 AM_VOID_CONST r1;
 AM_ASSIGN_STMT create_self3;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val5;
 AM_ASSIGN_STMT r2;
 CHANGE_VARS after_one_use_se;
 INT L3;
 INT L41_;
 AM_STMT_EXPR L5;
 OB L6;
 XFORM_CODE L7;
 OB L8;
 AM_CALL_ARG L10;
 INT L91_br;
INT i_L91_=0;
 AM_CALL_ARG aL91_;
 BOOL L121_;
 ARRAYARG L13;
 ARG L141_;
 ARG L15;
 INT L161_;
 BOOL L17;
 BOOL L181_;
 dAM_EXPR L19;
 AM_VOID_CONST L20;
 OB L22;
 dAM_STMT L23;
 BOOL L24;
 BOOL L251_;
 dAM_STMT L26;
 INT L271_br;
INT i_L271_=0;
 AM_CALL_ARG aL271_;
 AM_CALL_ARG L28;
 BOOL L29;
 AM_ASSIGN_STMT L30;
 OB L31;
 dAM_STMT L32;
 dAM_STMT L33;
 BOOL L34;
 BOOL L351_;
 FMAPAM1145008811 L37;
 L3 = INLINE1232309177;
 L41_=INTPLUS(L3,1); 
 INLINE1232309177 = L41_;
 create_self = ((AM_STMT_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L6=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_STMT_EXPR_tag;
 L5 = ((AM_STMT_EXPR) L6);
 r = L5;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 se = ret_val1;
 CHANGE1122717432(ATTR(self,change_vars), ATTR(call11,source1));
 SATTR(se,locals1,ATTR(ATTR(self,change_vars),new_vars));
 create_self1 = ((XFORM_CODE) NULL);
 create_subst = ATTR(ATTR(self,change_vars),new_subst);
 L8=ZALLOC(sizeof(struct XFORM_CODE_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=XFORM_CODE_tag;
 L7 = ((XFORM_CODE) L8);
 xc1 = L7;
 SATTR(xc1,subst,create_subst);
 ret_val2 = xc1;
 xc = ret_val2;
 SATTR(xc,record_calls,FALSE);
 init_parms = XFORM_1427763452(xc, ATTR(ATTR(self,change_vars),init_parms));
 s = init_parms;
 i = -1;
 {
  BOOL f_L91_ = TRUE;
  while (1) {
   if ((s==((dAM_STMT) NULL))) {
    goto after_loop;
   }
   switch (TAG(s)) {
    case AM_ASSIGN_STMT_tag:
     if ((ATTR(((AM_ASSIGN_STMT) s),src1)==((dAM_EXPR) NULL))) {
      if (f_L91_) {
       f_L91_ = FALSE;
       L11 = call11;
       L91_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
       i_L91_=0;
      }
      if(i_L91_>=L91_br)  goto after_loop; 
      aL91_=ARR((AM_ROU1916046290)L11,i_L91_); i_L91_++;
      ca = aL91_;
      L121_=(i)==(-1); 
      if (L121_) {
       stp = ATTR(ATTR(call11,fun),tp);
      }
      else {
       L13 = ATTR(ATTR(call11,fun),args);
       L141_=(ARG)ARR((ARRAYARG)L13,i); 
       L15=L141_;
       stp = ATTR(L15,tp);
      }
      L161_=INTPLUS(i,1); 
      i = L161_;
      L17 = SYSOBEQ(((OB) ATTR(ca,mode1)),((OB) MODES_out_mode));
      L181_=!(L17); 
      if (L181_) {
       SATTR(((AM_ASSIGN_STMT) s),src1,ATTR(ca,expr));
      }
      else {
       create_self2 = ((AM_VOID_CONST) NULL);
       source_self = ca;
       L19 = ATTR(source_self,expr);
       ret_val3 = (*dAM_EX2051891691[TAG(L19)])(L19);
       create_source1 = ret_val3;
       L22=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
       if (L22==NULL) FATAL("Unable to allocate more memory");
       ((OB)L22)->header.tag=AM_VOID_CONST_tag;
       L20 = ((AM_VOID_CONST) L22);
       r1 = L20;
       SATTR(r1,source1,create_source1);
       ret_val4 = r1;
       vconst = ret_val4;
       SATTR(vconst,tp_at,stp);
       SATTR(((AM_ASSIGN_STMT) s),src1,((dAM_EXPR) vconst));
      }
     } break;
    default: ;
     FATAL("No applicable type in typecase\nin INLINE_ROUT::inline(AM_ROUT_CALL_EXPR):$AM_EXPR\n./Middle/inline.sa:1611:17");
   }
   L23 = s;
   s = (*dAM_ST775224435[TAG(L23)])(L23);
  }
 }
 after_loop: ;
 SATTR(xc,record_calls,TRUE);
 SATTR(se,stmts,XFORM_1427763452(xc, ATTR(self,code1)));
 L24 = (init_parms==((dAM_STMT) NULL));
 L251_=!(L24); 
 if (L251_) {
  L26 = init_parms;
  (*dAM_ST2020072576[TAG(L26)])(L26, ATTR(se,stmts));
  SATTR(se,stmts,init_parms);
 }
 s = init_parms;
 {
  BOOL f_L271_ = TRUE;
  L21 = call11;
  L271_br=L21==NULL?0:ASIZE((AM_ROU1916046290)L21); 
  i_L271_=0;
  while (1) {
   if ((s==((dAM_STMT) NULL))) {
    goto after_loop1;
   }
   if(i_L271_>=L271_br)  goto after_loop1; 
   aL271_=ARR((AM_ROU1916046290)L21,i_L271_); i_L271_++;
   ca1 = aL271_;
   if (SYSOBEQ(((OB) ATTR(ca1,mode1)),((OB) MODES_out_mode))) {
    L29 = TRUE;
   } else {
    L29 = SYSOBEQ(((OB) ATTR(ca1,mode1)),((OB) MODES_inout_mode));
   }
   if (L29) {
    switch (TAG(s)) {
     case AM_ASSIGN_STMT_tag:
      create_self3 = ((AM_ASSIGN_STMT) NULL);
      create_source2 = ATTR(call11,source1);
      L31=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
      if (L31==NULL) FATAL("Unable to allocate more memory");
      ((OB)L31)->header.tag=AM_ASSIGN_STMT_tag;
      L30 = ((AM_ASSIGN_STMT) L31);
      r2 = L30;
      SATTR(r2,source1,create_source2);
      ret_val5 = r2;
      cp_stmt = ret_val5;
      SATTR(cp_stmt,dest,ATTR(ca1,expr));
      SATTR(cp_stmt,src1,ATTR(((AM_ASSIGN_STMT) s),dest));
      L32 = ATTR(se,stmts);
      (*dAM_ST2020072576[TAG(L32)])(L32, ((dAM_STMT) cp_stmt)); break;
     default: ;
      FATAL("No applicable type in typecase\nin INLINE_ROUT::inline(AM_ROUT_CALL_EXPR):$AM_EXPR\n./Middle/inline.sa:1647:23");
    }
   }
   L33 = s;
   s = (*dAM_ST775224435[TAG(L33)])(L33);
  }
 }
 after_loop1: ;
 L34 = (ATTR(self,return_val)==((dAM_EXPR) NULL));
 L351_=!(L34); 
 if (L351_) {
  SATTR(se,expr,XFORM_69815265(xc, ATTR(self,return_val)));
 }
 SATTR(se,calls1,ATTR(xc,calls1));
 SYSDESTROY(xc); 
 ;
 after_one_use_se = ATTR(self,change_vars);
 L37 = ATTR(after_one_use_se,new_subst);
 SYSDESTROY(L37); 
 ;
 ret_val = ((dAM_EXPR) se);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE986164897(INLINE_ITER self, AM_ITE1809135850 call11) {
 dAM_EXPR ret_val;
 AM_BOOL_CONST bcfalse;
 AM_BOOL_CONST bctrue;
 AM_STMT_EXPR se;
 XFORM_CODE xc;
 AM_LOOP_STMT proxy_loop_stmt;
 dAM_STMT init_nonhot;
 dAM_STMT s;
 ARRAYINT L11;
 dAM_STMT init_hot;
 ARRAYINT L21;
 INT ind1 = INT_zero;
 AM_CALL_ARG ca;
 dTP stp;
 AM_VOID_CONST vconst;
 dAM_STMT ftm1;
 AM_LOCAL_EXPR if_first;
 AM_IF_STMT iffstmt1;
 dAM_STMT init_vars;
 dAM_STMT ftm2;
 dAM_STMT BEFLOOP;
 AM_ASSIGN_STMT ass;
 dAM_STMT nftm2 = ((dAM_STMT) NULL);
 dAM_STMT ITERM;
 AM_IF_STMT iffstmt2;
 AM_LOCAL_EXPR if_yld;
 AM_LOOP_STMT loopstmt;
 dAM_STMT lbd;
 AM_IF_STMT ifcstmt;
 AM_IF_STMT ifzstmt;
 ARRAYINT L31;
 AM_CALL_ARG ca1;
 AM_ASSIGN_STMT cp_stmt;
 FLISTA725283029 L41;
 AM_BOOL_CONST create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_BOOL_CONST ret_val1;
 AM_BOOL_CONST r;
 AM_BOOL_CONST create_self1;
 SFILE_ID create_src1 = SFILE_ID_zero;
 AM_BOOL_CONST ret_val2;
 AM_BOOL_CONST r1;
 AM_STMT_EXPR create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val3;
 AM_STMT_EXPR r2;
 XFORM_CODE create_self3;
 FMAPAM1145008811 create_subst;
 XFORM_CODE ret_val4;
 XFORM_CODE xc1;
 AM_LOOP_STMT create_self4;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_LOOP_STMT ret_val5;
 AM_LOOP_STMT r3;
 AM_CALL_ARG source_self;
 SFILE_ID ret_val6 = SFILE_ID_zero;
 AM_VOID_CONST create_self5;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_VOID_CONST ret_val7;
 AM_VOID_CONST r4;
 INLINE_ITER sig_self;
 SIG ret_val8;
 STR plus_self;
 STR plus_sarg;
 STR ret_val9;
 AM_LOCAL_EXPR create_self6;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val10;
 AM_LOCAL_EXPR r5;
 AM_IF_STMT create_self7;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_IF_STMT ret_val11;
 AM_IF_STMT r6;
 AM_ASSIGN_STMT create_self8;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val12;
 AM_ASSIGN_STMT r7;
 AM_IF_STMT create_self9;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_IF_STMT ret_val13;
 AM_IF_STMT r8;
 INLINE_ITER sig_self1;
 SIG ret_val14;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val15;
 AM_LOCAL_EXPR create_self10;
 SFILE_ID create_src3 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AM_LOCAL_EXPR ret_val16;
 AM_LOCAL_EXPR r9;
 AM_ASSIGN_STMT create_self11;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val17;
 AM_ASSIGN_STMT r10;
 AM_LOOP_STMT create_self12;
 SFILE_ID create_source7 = SFILE_ID_zero;
 AM_LOOP_STMT ret_val18;
 AM_LOOP_STMT r11;
 AM_ASSIGN_STMT create_self13;
 SFILE_ID create_source8 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val19;
 AM_ASSIGN_STMT r12;
 AM_BREAK_STMT create_self14;
 SFILE_ID create_source9 = SFILE_ID_zero;
 AM_BREAK_STMT ret_val20;
 AM_BREAK_STMT r13;
 AM_IF_STMT create_self15;
 SFILE_ID create_source10 = SFILE_ID_zero;
 AM_IF_STMT ret_val21;
 AM_IF_STMT r14;
 AM_IF_STMT create_self16;
 SFILE_ID create_source11 = SFILE_ID_zero;
 AM_IF_STMT ret_val22;
 AM_IF_STMT r15;
 AM_BREAK_STMT create_self17;
 SFILE_ID create_source12 = SFILE_ID_zero;
 AM_BREAK_STMT ret_val23;
 AM_BREAK_STMT r16;
 AM_ASSIGN_STMT create_self18;
 SFILE_ID create_source13 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val24;
 AM_ASSIGN_STMT r17;
 CHANGE_VARS after_one_use_se;
 INT L5;
 INT L61_;
 AM_BOOL_CONST L7;
 OB L8;
 AM_BOOL_CONST L9;
 OB L10;
 AM_STMT_EXPR L12;
 OB L13;
 XFORM_CODE L14;
 OB L15;
 AM_LOOP_STMT L16;
 OB L17;
 INT L19;
 INT L181_br;
INT i_L181_=0;
 INT aL181_;
 INT L20;
 AM_CALL_ARG L221_;
 AM_CALL_ARG L23;
 dAM_STMT L24;
 INT L26;
 INT L251_br;
INT i_L251_=0;
 INT aL251_;
 AM_CALL_ARG L271_;
 ARRAYARG L28;
 INT L291_;
 INT L30;
 ARG L321_;
 ARG L33;
 BOOL L34;
 BOOL L351_;
 dAM_EXPR L36;
 AM_VOID_CONST L37;
 OB L38;
 dAM_STMT L39;
 BOOL L40;
 BOOL L421_;
 dAM_STMT L43;
 SIG L44;
 extern STR S_if_first;
 AM_LOCAL_EXPR L45;
 OB L46;
 AM_IF_STMT L47;
 OB L48;
 BOOL L49;
 BOOL L501_;
 dAM_STMT L51;
 AM_ASSIGN_STMT L52;
 OB L53;
 BOOL L54;
 BOOL L551_;
 dAM_STMT L56;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 dAM_STMT L62;
 BOOL L63;
 BOOL L641_;
 dAM_STMT L65;
 AM_IF_STMT L66;
 OB L67;
 dAM_STMT L68;
 BOOL L69;
 BOOL L701_;
 dAM_STMT L71;
 BOOL L72;
 BOOL L731_;
 SIG L74;
 extern STR S_if_yld;
 AM_LOCAL_EXPR L75;
 OB L76;
 AM_ASSIGN_STMT L77;
 OB L78;
 dAM_STMT L79;
 AM_LOOP_STMT L80;
 OB L81;
 AM_ASSIGN_STMT L82;
 OB L83;
 AM_BREAK_STMT L84;
 OB L85;
 AM_IF_STMT L86;
 OB L87;
 BOOL L88;
 BOOL L891_;
 dAM_STMT L90;
 dAM_STMT L91;
 dAM_STMT L92;
 dAM_STMT L93;
 AM_IF_STMT L94;
 OB L95;
 AM_BREAK_STMT L96;
 OB L97;
 dAM_STMT L98;
 dAM_STMT L99;
 dAM_STMT L100;
 BOOL L101;
 BOOL L1021_;
 INT L104;
 INT L1031_br;
INT i_L1031_=0;
 INT aL1031_;
 INT L105;
 AM_CALL_ARG L1061_;
 BOOL L107;
 AM_ASSIGN_STMT L108;
 OB L109;
 dAM_STMT L110;
 dAM_STMT L111;
 INT L112;
 INT L1131_;
 FLISTA725283029 L114;
 FMAPAM1145008811 L115;
 AM_LOCAL_EXPR L116;
 FMAPAM1145008811 L118;
 L5 = INLINE1301544351;
 L61_=INTPLUS(L5,1); 
 INLINE1301544351 = L61_;
 create_self = ((AM_BOOL_CONST) NULL);
 create_src = ATTR(call11,source1);
 L8=ZALLOC(sizeof(struct AM_BOOL_CONST_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AM_BOOL_CONST_tag;
 L7 = ((AM_BOOL_CONST) L8);
 r = L7;
 SATTR(r,source1,create_src);
 ret_val1 = r;
 bcfalse = ret_val1;
 SATTR(bcfalse,tp_at,((dTP) TP_BUILTIN_bool));
 SATTR(bcfalse,val1,FALSE);
 create_self1 = ((AM_BOOL_CONST) NULL);
 create_src1 = ATTR(call11,source1);
 L10=ZALLOC(sizeof(struct AM_BOOL_CONST_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_BOOL_CONST_tag;
 L9 = ((AM_BOOL_CONST) L10);
 r1 = L9;
 SATTR(r1,source1,create_src1);
 ret_val2 = r1;
 bctrue = ret_val2;
 SATTR(bctrue,tp_at,((dTP) TP_BUILTIN_bool));
 SATTR(bctrue,val1,TRUE);
 create_self2 = ((AM_STMT_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L13=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=AM_STMT_EXPR_tag;
 L12 = ((AM_STMT_EXPR) L13);
 r2 = L12;
 SATTR(r2,source1,create_source);
 ret_val3 = r2;
 se = ret_val3;
 CHANGE1122717432(ATTR(self,change_vars), ATTR(call11,source1));
 SATTR(se,locals1,ATTR(ATTR(self,change_vars),new_vars));
 create_self3 = ((XFORM_CODE) NULL);
 create_subst = ATTR(ATTR(self,change_vars),new_subst);
 L15=ZALLOC(sizeof(struct XFORM_CODE_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=XFORM_CODE_tag;
 L14 = ((XFORM_CODE) L15);
 xc1 = L14;
 SATTR(xc1,subst,create_subst);
 ret_val4 = xc1;
 xc = ret_val4;
 SATTR(xc,record_calls,TRUE);
 create_self4 = ((AM_LOOP_STMT) NULL);
 create_source1 = ATTR(call11,source1);
 L17=ZALLOC(sizeof(struct AM_LOOP_STMT_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=AM_LOOP_STMT_tag;
 L16 = ((AM_LOOP_STMT) L17);
 r3 = L16;
 SATTR(r3,source1,create_source1);
 ret_val5 = r3;
 proxy_loop_stmt = ret_val5;
 SATTR(xc,new_loop_stmt,proxy_loop_stmt);
 init_nonhot = XFORM_1427763452(xc, ATTR(ATTR(self,change_vars),init_nonhot));
 s = init_nonhot;
 {
  BOOL f_L181_ = TRUE;
  while (1) {
   if ((s==((dAM_STMT) NULL))) {
    goto after_loop;
   }
   switch (TAG(s)) {
    case AM_ASSIGN_STMT_tag:
     if ((ATTR(((AM_ASSIGN_STMT) s),src1)==((dAM_EXPR) NULL))) {
      if (f_L181_) {
       f_L181_ = FALSE;
       L11 = ATTR(ATTR(self,change_vars),pind_nonhot);
       L181_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
       i_L181_=0;
      }
      if(i_L181_>=L181_br)  goto after_loop; 
      aL181_=ARR((ARRAYINT)L11,i_L181_); i_L181_++;
      L20 = aL181_;
      L221_=(AM_CALL_ARG)ARR((AM_ITE1809135850)call11,L20); 
      L23=L221_;
      SATTR(((AM_ASSIGN_STMT) s),src1,ATTR(L23,expr));
     } break;
    default: ;
     FATAL("No applicable type in typecase\nin INLINE_ITER::inline(AM_ITER_CALL_EXPR):$AM_EXPR\n./Middle/inline.sa:1359:20");
   }
   L24 = s;
   s = (*dAM_ST775224435[TAG(L24)])(L24);
  }
 }
 after_loop: ;
 init_hot = XFORM_1427763452(xc, ATTR(ATTR(self,change_vars),init_hot));
 s = init_hot;
 {
  BOOL f_L251_ = TRUE;
  while (1) {
   if ((s==((dAM_STMT) NULL))) {
    goto after_loop1;
   }
   switch (TAG(s)) {
    case AM_ASSIGN_STMT_tag:
     if ((ATTR(((AM_ASSIGN_STMT) s),src1)==((dAM_EXPR) NULL))) {
      if (f_L251_) {
       f_L251_ = FALSE;
       L21 = ATTR(ATTR(self,change_vars),pind_hot);
       L251_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
       i_L251_=0;
      }
      if(i_L251_>=L251_br)  goto after_loop1; 
      aL251_=ARR((ARRAYINT)L21,i_L251_); i_L251_++;
      ind1 = aL251_;
      L271_=(AM_CALL_ARG)ARR((AM_ITE1809135850)call11,ind1); 
      ca = L271_;
      L28 = ATTR(ATTR(call11,fun),args);
      L291_=INTMINUS(ind1,1); 
      L30 = L291_;
      L321_=(ARG)ARR((ARRAYARG)L28,L30); 
      L33=L321_;
      stp = ATTR(L33,tp);
      L34 = SYSOBEQ(((OB) ATTR(ca,mode1)),((OB) MODES_out_mode));
      L351_=!(L34); 
      if (L351_) {
       SATTR(((AM_ASSIGN_STMT) s),src1,ATTR(ca,expr));
      }
      else {
       create_self5 = ((AM_VOID_CONST) NULL);
       source_self = ca;
       L36 = ATTR(source_self,expr);
       ret_val6 = (*dAM_EX2051891691[TAG(L36)])(L36);
       create_source2 = ret_val6;
       L38=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
       if (L38==NULL) FATAL("Unable to allocate more memory");
       ((OB)L38)->header.tag=AM_VOID_CONST_tag;
       L37 = ((AM_VOID_CONST) L38);
       r4 = L37;
       SATTR(r4,source1,create_source2);
       ret_val7 = r4;
       vconst = ret_val7;
       SATTR(vconst,tp_at,stp);
       SATTR(((AM_ASSIGN_STMT) s),src1,((dAM_EXPR) vconst));
      }
     } break;
    default: ;
     FATAL("No applicable type in typecase\nin INLINE_ITER::inline(AM_ITER_CALL_EXPR):$AM_EXPR\n./Middle/inline.sa:1372:20");
   }
   L39 = s;
   s = (*dAM_ST775224435[TAG(L39)])(L39);
  }
 }
 after_loop1: ;
 ftm1 = ATTR(call11,init);
 L40 = (ftm1==((dAM_STMT) NULL));
 L421_=!(L40); 
 if (L421_) {
  L43 = ftm1;
  (*dAM_ST2020072576[TAG(L43)])(L43, init_nonhot);
 }
 else {
  ftm1 = init_nonhot;
 }
 create_self6 = ((AM_LOCAL_EXPR) NULL);
 create_src2 = ATTR(call11,source1);
 sig_self = self;
 ret_val8 = ATTR(sig_self,signature);
 L44=ret_val8;
 plus_self = ATTR(L44,name1).str;
 plus_sarg = ((STR) &S_if_first);
 ret_val9 = STR_ap2004550586(plus_self, plus_sarg);
 create_name = IDENT_1150413730(IDENT_zero, ret_val9);
 create_tp = ((dTP) TP_BUILTIN_bool);
 L46=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L46==NULL) FATAL("Unable to allocate more memory");
 ((OB)L46)->header.tag=AM_LOCAL_EXPR_tag;
 L45 = ((AM_LOCAL_EXPR) L46);
 r5 = L45;
 SATTR(r5,source1,create_src2);
 SATTR(r5,name1,create_name);
 SATTR(r5,tp_at,create_tp);
 ret_val10 = r5;
 if_first = ret_val10;
 SATTR(se,locals1,FLISTA1817671564(ATTR(se,locals1), if_first));
 create_self7 = ((AM_IF_STMT) NULL);
 create_source3 = ATTR(call11,source1);
 L48=ZALLOC(sizeof(struct AM_IF_STMT_struct));
 if (L48==NULL) FATAL("Unable to allocate more memory");
 ((OB)L48)->header.tag=AM_IF_STMT_tag;
 L47 = ((AM_IF_STMT) L48);
 r6 = L47;
 SATTR(r6,source1,create_source3);
 ret_val11 = r6;
 iffstmt1 = ret_val11;
 SATTR(iffstmt1,test1,((dAM_EXPR) if_first));
 SATTR(iffstmt1,if_true,ftm1);
 SATTR(iffstmt1,if_false,((dAM_STMT) NULL));
 SATTR(se,stmts,((dAM_STMT) iffstmt1));
 init_vars = XFORM_1427763452(xc, ATTR(ATTR(self,change_vars),init_vars));
 ftm2 = init_vars;
 BEFLOOP = XFORM_1427763452(xc, ATTR(self,BEFLOOP_code));
 L49 = (ftm2==((dAM_STMT) NULL));
 L501_=!(L49); 
 if (L501_) {
  L51 = ftm2;
  (*dAM_ST2020072576[TAG(L51)])(L51, BEFLOOP);
 }
 else {
  ftm2 = BEFLOOP;
 }
 create_self8 = ((AM_ASSIGN_STMT) NULL);
 create_source4 = ATTR(call11,source1);
 L53=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L53==NULL) FATAL("Unable to allocate more memory");
 ((OB)L53)->header.tag=AM_ASSIGN_STMT_tag;
 L52 = ((AM_ASSIGN_STMT) L53);
 r7 = L52;
 SATTR(r7,source1,create_source4);
 ret_val12 = r7;
 ass = ret_val12;
 SATTR(ass,dest,((dAM_EXPR) if_first));
 SATTR(ass,src1,((dAM_EXPR) bcfalse));
 L54 = (ftm2==((dAM_STMT) NULL));
 L551_=!(L54); 
 if (L551_) {
  L56 = ftm2;
  (*dAM_ST2020072576[TAG(L56)])(L56, ((dAM_STMT) ass));
 }
 else {
  ftm2 = ((dAM_STMT) ass);
 }
 L57 = ATTR(self,flat_yield);
 L581_=!(L57); 
 if (L581_) {
  nftm2 = XFORM_1427763452(xc, ATTR(self,WYAFT_code));
 }
 ITERM = XFORM_1427763452(xc, ATTR(self,ITERM_code));
 L59 = (nftm2==((dAM_STMT) NULL));
 L601_=!(L59); 
 if (L601_) {
  L62 = nftm2;
  (*dAM_ST2020072576[TAG(L62)])(L62, ITERM);
 }
 else {
  nftm2 = ITERM;
 }
 L63 = (init_hot==((dAM_STMT) NULL));
 L641_=!(L63); 
 if (L641_) {
  L65 = ATTR(se,stmts);
  (*dAM_ST2020072576[TAG(L65)])(L65, init_hot);
  create_self9 = ((AM_IF_STMT) NULL);
  create_source5 = ATTR(call11,source1);
  L67=ZALLOC(sizeof(struct AM_IF_STMT_struct));
  if (L67==NULL) FATAL("Unable to allocate more memory");
  ((OB)L67)->header.tag=AM_IF_STMT_tag;
  L66 = ((AM_IF_STMT) L67);
  r8 = L66;
  SATTR(r8,source1,create_source5);
  ret_val13 = r8;
  iffstmt2 = ret_val13;
  SATTR(iffstmt2,test1,((dAM_EXPR) if_first));
  SATTR(iffstmt2,if_true,ftm2);
  SATTR(iffstmt2,if_false,nftm2);
  L68 = ATTR(se,stmts);
  (*dAM_ST2020072576[TAG(L68)])(L68, ((dAM_STMT) iffstmt2));
 }
 else {
  L69 = (ATTR(iffstmt1,if_true)==((dAM_STMT) NULL));
  L701_=!(L69); 
  if (L701_) {
   L71 = ATTR(iffstmt1,if_true);
   (*dAM_ST2020072576[TAG(L71)])(L71, ftm2);
  }
  else {
   SATTR(iffstmt1,if_true,ftm2);
  }
  SATTR(iffstmt1,if_false,nftm2);
 }
 L72 = ATTR(self,flat_yield);
 L731_=!(L72); 
 if (L731_) {
  create_self10 = ((AM_LOCAL_EXPR) NULL);
  create_src3 = ATTR(call11,source1);
  sig_self1 = self;
  ret_val14 = ATTR(sig_self1,signature);
  L74=ret_val14;
  plus_self1 = ATTR(L74,name1).str;
  plus_sarg1 = ((STR) &S_if_yld);
  ret_val15 = STR_ap2004550586(plus_self1, plus_sarg1);
  create_name1 = IDENT_1150413730(IDENT_zero, ret_val15);
  create_tp1 = ((dTP) TP_BUILTIN_bool);
  L76=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L76==NULL) FATAL("Unable to allocate more memory");
  ((OB)L76)->header.tag=AM_LOCAL_EXPR_tag;
  L75 = ((AM_LOCAL_EXPR) L76);
  r9 = L75;
  SATTR(r9,source1,create_src3);
  SATTR(r9,name1,create_name1);
  SATTR(r9,tp_at,create_tp1);
  ret_val16 = r9;
  if_yld = ret_val16;
  SATTR(se,locals1,FLISTA1817671564(ATTR(se,locals1), if_yld));
  create_self11 = ((AM_ASSIGN_STMT) NULL);
  create_source6 = ATTR(call11,source1);
  L78=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L78==NULL) FATAL("Unable to allocate more memory");
  ((OB)L78)->header.tag=AM_ASSIGN_STMT_tag;
  L77 = ((AM_ASSIGN_STMT) L78);
  r10 = L77;
  SATTR(r10,source1,create_source6);
  ret_val17 = r10;
  ass = ret_val17;
  SATTR(ass,dest,((dAM_EXPR) if_yld));
  SATTR(ass,src1,((dAM_EXPR) bcfalse));
  L79 = ATTR(se,stmts);
  (*dAM_ST2020072576[TAG(L79)])(L79, ((dAM_STMT) ass));
  create_self12 = ((AM_LOOP_STMT) NULL);
  create_source7 = ATTR(call11,source1);
  L81=ZALLOC(sizeof(struct AM_LOOP_STMT_struct));
  if (L81==NULL) FATAL("Unable to allocate more memory");
  ((OB)L81)->header.tag=AM_LOOP_STMT_tag;
  L80 = ((AM_LOOP_STMT) L81);
  r11 = L80;
  SATTR(r11,source1,create_source7);
  ret_val18 = r11;
  loopstmt = ret_val18;
  SATTR(loopstmt,has_yield,FALSE);
  lbd = XFORM_1427763452(xc, ATTR(self,IINIT_code));
  create_self13 = ((AM_ASSIGN_STMT) NULL);
  create_source8 = ATTR(call11,source1);
  L83=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L83==NULL) FATAL("Unable to allocate more memory");
  ((OB)L83)->header.tag=AM_ASSIGN_STMT_tag;
  L82 = ((AM_ASSIGN_STMT) L83);
  r12 = L82;
  SATTR(r12,source1,create_source8);
  ret_val19 = r12;
  ass = ret_val19;
  SATTR(ass,dest,((dAM_EXPR) if_yld));
  SATTR(ass,src1,((dAM_EXPR) bctrue));
  create_self14 = ((AM_BREAK_STMT) NULL);
  create_source9 = ATTR(call11,source1);
  L85=ZALLOC(sizeof(struct AM_BREAK_STMT_struct));
  if (L85==NULL) FATAL("Unable to allocate more memory");
  ((OB)L85)->header.tag=AM_BREAK_STMT_tag;
  L84 = ((AM_BREAK_STMT) L85);
  r13 = L84;
  SATTR(r13,source1,create_source9);
  ret_val20 = r13;
  AM_ASS1281039639(ass, ((dAM_STMT) ret_val20));
  create_self15 = ((AM_IF_STMT) NULL);
  create_source10 = ATTR(call11,source1);
  L87=ZALLOC(sizeof(struct AM_IF_STMT_struct));
  if (L87==NULL) FATAL("Unable to allocate more memory");
  ((OB)L87)->header.tag=AM_IF_STMT_tag;
  L86 = ((AM_IF_STMT) L87);
  r14 = L86;
  SATTR(r14,source1,create_source10);
  ret_val21 = r14;
  ifcstmt = ret_val21;
  SATTR(ifcstmt,test1,XFORM_69815265(xc, ATTR(self,yield_cond)));
  SATTR(ifcstmt,if_true,((dAM_STMT) ass));
  SATTR(ifcstmt,if_false,((dAM_STMT) NULL));
  L88 = (lbd==((dAM_STMT) NULL));
  L891_=!(L88); 
  if (L891_) {
   L90 = lbd;
   (*dAM_ST2020072576[TAG(L90)])(L90, ((dAM_STMT) ifcstmt));
  }
  else {
   lbd = ((dAM_STMT) ifcstmt);
  }
  L91 = lbd;
  (*dAM_ST2020072576[TAG(L91)])(L91, XFORM_1427763452(xc, ATTR(self,WNOY_code)));
  L92 = lbd;
  (*dAM_ST2020072576[TAG(L92)])(L92, XFORM_1427763452(xc, ATTR(self,ITERM_code)));
  SATTR(loopstmt,body,lbd);
  L93 = ATTR(se,stmts);
  (*dAM_ST2020072576[TAG(L93)])(L93, ((dAM_STMT) loopstmt));
  create_self16 = ((AM_IF_STMT) NULL);
  create_source11 = ATTR(call11,source1);
  L95=ZALLOC(sizeof(struct AM_IF_STMT_struct));
  if (L95==NULL) FATAL("Unable to allocate more memory");
  ((OB)L95)->header.tag=AM_IF_STMT_tag;
  L94 = ((AM_IF_STMT) L95);
  r15 = L94;
  SATTR(r15,source1,create_source11);
  ret_val22 = r15;
  ifzstmt = ret_val22;
  SATTR(ifzstmt,test1,((dAM_EXPR) if_yld));
  SATTR(ifzstmt,if_true,XFORM_1427763452(xc, ATTR(self,WYBEF_code)));
  create_self17 = ((AM_BREAK_STMT) NULL);
  create_source12 = ATTR(call11,source1);
  L97=ZALLOC(sizeof(struct AM_BREAK_STMT_struct));
  if (L97==NULL) FATAL("Unable to allocate more memory");
  ((OB)L97)->header.tag=AM_BREAK_STMT_tag;
  L96 = ((AM_BREAK_STMT) L97);
  r16 = L96;
  SATTR(r16,source1,create_source12);
  ret_val23 = r16;
  SATTR(ifzstmt,if_false,((dAM_STMT) ret_val23));
  L98 = ATTR(se,stmts);
  (*dAM_ST2020072576[TAG(L98)])(L98, ((dAM_STMT) ifzstmt));
 }
 else {
  L99 = ATTR(se,stmts);
  L100 = L99;
  (*dAM_ST2020072576[TAG(L100)])(L100, XFORM_1427763452(xc, ATTR(self,IINIT_code)));
 }
 L101 = (ATTR(self,yield_val)==((dAM_EXPR) NULL));
 L1021_=!(L101); 
 if (L1021_) {
  SATTR(se,expr,XFORM_69815265(xc, ATTR(self,yield_val)));
 }
 s = init_hot;
 {
  BOOL f_L1031_ = TRUE;
  while (1) {
   if ((s==((dAM_STMT) NULL))) {
    goto after_loop2;
   }
   if (f_L1031_) {
    f_L1031_ = FALSE;
    L31 = ATTR(ATTR(self,change_vars),pind_hot);
    L1031_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
    i_L1031_=0;
   }
   if(i_L1031_>=L1031_br)  goto after_loop2; 
   aL1031_=ARR((ARRAYINT)L31,i_L1031_); i_L1031_++;
   L105 = aL1031_;
   L1061_=(AM_CALL_ARG)ARR((AM_ITE1809135850)call11,L105); 
   ca1 = L1061_;
   if (SYSOBEQ(((OB) ATTR(ca1,mode1)),((OB) MODES_out_mode))) {
    L107 = TRUE;
   } else {
    L107 = SYSOBEQ(((OB) ATTR(ca1,mode1)),((OB) MODES_inout_mode));
   }
   if (L107) {
    switch (TAG(s)) {
     case AM_ASSIGN_STMT_tag:
      create_self18 = ((AM_ASSIGN_STMT) NULL);
      create_source13 = ATTR(call11,source1);
      L109=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
      if (L109==NULL) FATAL("Unable to allocate more memory");
      ((OB)L109)->header.tag=AM_ASSIGN_STMT_tag;
      L108 = ((AM_ASSIGN_STMT) L109);
      r17 = L108;
      SATTR(r17,source1,create_source13);
      ret_val24 = r17;
      cp_stmt = ret_val24;
      SATTR(cp_stmt,dest,ATTR(ca1,expr));
      SATTR(cp_stmt,src1,ATTR(((AM_ASSIGN_STMT) s),dest));
      L110 = ATTR(se,stmts);
      (*dAM_ST2020072576[TAG(L110)])(L110, ((dAM_STMT) cp_stmt)); break;
     default: ;
      FATAL("No applicable type in typecase\nin INLINE_ITER::inline(AM_ITER_CALL_EXPR):$AM_EXPR\n./Middle/inline.sa:1487:28");
    }
   }
   L111 = s;
   s = (*dAM_ST775224435[TAG(L111)])(L111);
  }
 }
 after_loop2: ;
 SATTR(se,calls1,ATTR(xc,calls1));
 SATTR(se,its1,ATTR(proxy_loop_stmt,its1));
 SATTR(se,bits1,ATTR(proxy_loop_stmt,bits1));
 L112 = FLISTA1151473122(ATTR(self,loop_firsts));
 L1131_=INTPLUS(L112,1); 
 SATTR(se,firsts,FLISTA1446031755(((FLISTA725283029) NULL), L1131_));
 SATTR(se,firsts,FLISTA1817671564(ATTR(se,firsts), if_first));
 {
  struct FLISTA2119642552_frame_struct other4_0;
FLISTA2119642552_frame noname1 = &other4_0;
  L41 = ATTR(self,loop_firsts);
  noname1->self = L41;
  noname1->state = 0;
  while (1) {
   L114 = ATTR(se,firsts);
   L115 = ATTR(ATTR(self,change_vars),new_subst);
   L116 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop3;
   }
   SATTR(se,firsts,FLISTA1817671564(L114, FMAPAM1614132657(L115, L116)));
  }
 }
 after_loop3: ;
 SYSDESTROY(proxy_loop_stmt); 
 ;
 after_one_use_se = ATTR(self,change_vars);
 L118 = ATTR(after_one_use_se,new_subst);
 SYSDESTROY(L118); 
 ;
 SYSDESTROY(xc); 
 ;
 ret_val = ((dAM_EXPR) se);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR XFORM_69815265(XFORM_CODE self, dAM_EXPR expr11) {
 dAM_EXPR ret_val;
 AM_CALL_ARG arg = ((AM_CALL_ARG) NULL);
 AM_ARRAY_EXPR enew;
 AM_ARRAY_EXPR L11;
 AM_ARRAY_EXPR L21;
 AM_BND1124877163 enew1;
 AM_BND1124877163 L31;
 AM_BND1124877163 L41;
 AM_IF_EXPR enew2;
 AM_NEW_EXPR enew3;
 AM_IS_VOID_EXPR enew4;
 AM_SHARED_EXPR enew5;
 AM_ATTR_EXPR enew6;
 AM_VAT319982528 enew7;
 AM_VAR744470097 enew8;
 AM_WHERE_EXPR enew9;
 AM_NEAR_EXPR enew10;
 AM_FAR_EXPR enew11;
 AM_AT_EXPR enew12;
 AM_ROU1916046290 enew13;
 AM_ROU1916046290 L51;
 AM_ROU1916046290 L61;
 AM_ITE1809135850 enew14;
 AM_ITE1809135850 L71;
 AM_ITE1809135850 L81;
 AM_BND260301329 enew15;
 AM_BND260301329 L91;
 AM_BND260301329 L101;
 AM_BND367211769 enew16;
 AM_BND367211769 L121;
 AM_BND367211769 L131;
 AM_EXT_CALL_EXPR enew17;
 AM_EXT_CALL_EXPR L141;
 AM_EXT_CALL_EXPR L151;
 AM_STMT_EXPR enew18;
 FLISTdAM_EXPR save_calls;
 FLISTA725283029 L161;
 AM_ARRAY_EXPR create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ARRAY_EXPR ret_val1;
 AM_ARRAY_EXPR r;
 INT L171 = INT_zero;
 AM_BND1124877163 create_self1;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_BND1124877163 ret_val2;
 AM_BND1124877163 r1;
 INT L181 = INT_zero;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e;
 dMODE create_m;
 AM_CALL_ARG ret_val3;
 AM_CALL_ARG res;
 AM_IF_EXPR create_self3;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_IF_EXPR ret_val4;
 AM_IF_EXPR r2;
 AM_NEW_EXPR create_self4;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_NEW_EXPR ret_val5;
 AM_NEW_EXPR r3;
 AM_IS_VOID_EXPR create_self5;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_IS_VOID_EXPR ret_val6;
 AM_IS_VOID_EXPR r4;
 AM_SHARED_EXPR create_self6;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val7;
 AM_SHARED_EXPR r5;
 AM_ATTR_EXPR create_self7;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val8;
 AM_ATTR_EXPR r6;
 AM_VAT319982528 create_self8;
 SFILE_ID create_source7 = SFILE_ID_zero;
 AM_VAT319982528 ret_val9;
 AM_VAT319982528 r7;
 AM_VAR744470097 create_self9;
 SFILE_ID create_source8 = SFILE_ID_zero;
 AM_VAR744470097 ret_val10;
 AM_VAR744470097 r8;
 AM_WHERE_EXPR create_self10;
 SFILE_ID create_source9 = SFILE_ID_zero;
 AM_WHERE_EXPR ret_val11;
 AM_WHERE_EXPR r9;
 AM_NEAR_EXPR create_self11;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_NEAR_EXPR ret_val12;
 AM_NEAR_EXPR r10;
 AM_FAR_EXPR create_self12;
 SFILE_ID create_src1 = SFILE_ID_zero;
 AM_FAR_EXPR ret_val13;
 AM_FAR_EXPR r11;
 AM_AT_EXPR create_self13;
 SFILE_ID create_src2 = SFILE_ID_zero;
 AM_AT_EXPR ret_val14;
 AM_AT_EXPR r12;
 AM_ROU1916046290 create_self14;
 INT create_nargs2 = INT_zero;
 SFILE_ID create_source10 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val15;
 AM_ROU1916046290 r13;
 INT L191 = INT_zero;
 AM_CALL_ARG create_self15;
 dAM_EXPR create_e1;
 dMODE create_m1;
 AM_CALL_ARG ret_val16;
 AM_CALL_ARG res1;
 AM_ITE1809135850 create_self16;
 INT create_nargs3 = INT_zero;
 SFILE_ID create_source11 = SFILE_ID_zero;
 AM_ITE1809135850 ret_val17;
 AM_ITE1809135850 r14;
 INT L201 = INT_zero;
 AM_CALL_ARG create_self17;
 dAM_EXPR create_e2;
 dMODE create_m2;
 AM_CALL_ARG ret_val18;
 AM_CALL_ARG res2;
 AM_BND260301329 create_self18;
 INT create_nargs4 = INT_zero;
 SFILE_ID create_source12 = SFILE_ID_zero;
 AM_BND260301329 ret_val19;
 AM_BND260301329 r15;
 INT L221 = INT_zero;
 AM_CALL_ARG create_self19;
 dAM_EXPR create_e3;
 dMODE create_m3;
 AM_CALL_ARG ret_val20;
 AM_CALL_ARG res3;
 AM_BND367211769 create_self20;
 INT create_nargs5 = INT_zero;
 SFILE_ID create_source13 = SFILE_ID_zero;
 AM_BND367211769 ret_val21;
 AM_BND367211769 r16;
 INT L231 = INT_zero;
 AM_CALL_ARG create_self21;
 dAM_EXPR create_e4;
 dMODE create_m4;
 AM_CALL_ARG ret_val22;
 AM_CALL_ARG res4;
 AM_EXT_CALL_EXPR create_self22;
 INT create_nargs6 = INT_zero;
 SFILE_ID create_source14 = SFILE_ID_zero;
 IDENT create_nm = IDENT_zero;
 AM_EXT_CALL_EXPR ret_val23;
 AM_EXT_CALL_EXPR r17;
 AM_EXT_CALL_EXPR create_self23;
 INT create_nargs7 = INT_zero;
 SFILE_ID create_source15 = SFILE_ID_zero;
 AM_EXT_CALL_EXPR ret_val24;
 AM_EXT_CALL_EXPR r18;
 INT L241 = INT_zero;
 AM_CALL_ARG create_self24;
 dAM_EXPR create_e5;
 dMODE create_m5;
 AM_CALL_ARG ret_val25;
 AM_CALL_ARG res5;
 AM_STMT_EXPR create_self25;
 SFILE_ID create_source16 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val26;
 AM_STMT_EXPR r19;
 INT L251_;
 AM_ARRAY_EXPR L26;
 INT L27;
 OB L28;
 INT L29;
 INT L301_br;
 dAM_EXPR aL301_;
 INT L321_br;
 dAM_EXPR L33;
 dAM_EXPR L34;
 INT L351_;
 INT L361_;
 AM_BND1124877163 L37;
 INT L38;
 OB L39;
 INT L40;
 INT L421_br;
 AM_CALL_ARG aL421_;
 INT L431_br;
 AM_CALL_ARG L44;
 AM_CALL_ARG L45;
 OB L46;
 AM_CALL_ARG L47;
 INT L481_;
 AM_IF_EXPR L49;
 OB L50;
 AM_NEW_EXPR L52;
 OB L53;
 AM_IS_VOID_EXPR L54;
 OB L55;
 AM_SHARED_EXPR L56;
 OB L57;
 AM_ATTR_EXPR L58;
 OB L59;
 AM_VAT319982528 L60;
 OB L62;
 AM_VAR744470097 L63;
 OB L64;
 AM_WHERE_EXPR L65;
 OB L66;
 AM_NEAR_EXPR L67;
 OB L68;
 AM_FAR_EXPR L69;
 OB L70;
 AM_AT_EXPR L72;
 OB L73;
 INT L741_;
 AM_ROU1916046290 L75;
 INT L76;
 OB L77;
 INT L78;
 INT L791_br;
 AM_CALL_ARG aL791_;
 INT L801_br;
 AM_CALL_ARG L82;
 AM_CALL_ARG L83;
 OB L84;
 AM_CALL_ARG L85;
 INT L861_;
 INT L871_;
 AM_ITE1809135850 L88;
 INT L89;
 OB L90;
 INT L92;
 INT L931_br;
 AM_CALL_ARG aL931_;
 INT L941_br;
 AM_CALL_ARG L95;
 AM_CALL_ARG L96;
 OB L97;
 AM_CALL_ARG L98;
 INT L991_;
 INT L1001_;
 AM_BND260301329 L102;
 INT L103;
 OB L104;
 INT L105;
 INT L1061_br;
 AM_CALL_ARG aL1061_;
 INT L1071_br;
 AM_CALL_ARG L108;
 AM_CALL_ARG L109;
 OB L110;
 AM_CALL_ARG L111;
 INT L1121_;
 INT L1131_;
 AM_BND367211769 L114;
 INT L115;
 OB L116;
 INT L117;
 INT L1181_br;
 AM_CALL_ARG aL1181_;
 INT L1191_br;
 AM_CALL_ARG L120;
 AM_CALL_ARG L122;
 OB L123;
 AM_CALL_ARG L124;
 INT L1251_;
 INT L1261_;
 AM_EXT_CALL_EXPR L127;
 INT L128;
 OB L129;
 INT L130;
 INT L1321_br;
 AM_CALL_ARG aL1321_;
 INT L1331_br;
 AM_CALL_ARG L134;
 AM_CALL_ARG L135;
 OB L136;
 AM_CALL_ARG L137;
 INT L1381_;
 AM_STMT_EXPR L139;
 OB L140;
 FLISTA725283029 L142;
 FMAPAM1145008811 L143;
 AM_LOCAL_EXPR L144;
 if ((expr11==((dAM_EXPR) NULL))) {
  ret_val = ((dAM_EXPR) NULL);
  return ret_val;
 }
 switch (TAG(expr11)) {
  case AM_LOCAL_EXPR_tag:
   ret_val = ((dAM_EXPR) FMAPAM1614132657(ATTR(self,subst), ((AM_LOCAL_EXPR) expr11)));
   return ret_val; break;
  case AM_ARRAY_EXPR_tag:
   create_self = ((AM_ARRAY_EXPR) NULL);
   L251_=ASIZE((AM_ARRAY_EXPR)expr11); 
   create_nargs = L251_;
   create_source = ATTR(((AM_ARRAY_EXPR) expr11),source1);
   L27 = create_nargs;
   L29=(sizeof(struct AM_ARRAY_EXPR_struct)-sizeof(dAM_EXPR))+(L27)*sizeof(dAM_EXPR);
   L28=ZALLOC_BIG(L29);
   if (L28==NULL) FATAL("Unable to allocate more memory");
   ((OB)L28)->header.tag=AM_ARRAY_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L28)->header.destroyed=0;
#endif

   L26 = ((AM_ARRAY_EXPR) L28);
   L26->asize = L27;
   r = L26;
   SATTR(r,source1,create_source);
   ret_val1 = r;
   enew = ret_val1;
   SATTR(enew,tp_at,ATTR(((AM_ARRAY_EXPR) expr11),tp_at));
   {
    BOOL f_L301_ = TRUE;
    BOOL f_L321_ = TRUE;
    /* loop index variable */
    L171 = 0;
    L11 = ((AM_ARRAY_EXPR) expr11);
    L21 = enew;
    L301_br=L11==NULL?0:ASIZE((AM_ARRAY_EXPR)L11); 
    L321_br=L21==NULL?0:ASIZE((AM_ARRAY_EXPR)L21); 
    while (1) {
     if(L171>=L321_br)  goto after_loop; 
     if(L171>=L301_br)  goto after_loop; 
     aL301_=ARR((AM_ARRAY_EXPR)L11,L171); 
     L34 = XFORM_69815265(self, aL301_);
     SARR((AM_ARRAY_EXPR)L21,L171,(dAM_EXPR)L34); 
     ;
     L351_=INTPLUS(L171,1); 
     L171 = L351_;
    }
   }
   after_loop: ;
   ret_val = ((dAM_EXPR) enew);
   return ret_val; break;
  case AM_BND1124877163_tag:
   create_self1 = ((AM_BND1124877163) NULL);
   L361_=ASIZE((AM_BND1124877163)expr11); 
   create_nargs1 = L361_;
   create_source1 = ATTR(((AM_BND1124877163) expr11),source1);
   L38 = create_nargs1;
   L40=(sizeof(struct AM_BND1124877163_struct)-sizeof(AM_CALL_ARG))+(L38)*sizeof(AM_CALL_ARG);
   L39=ZALLOC_BIG(L40);
   if (L39==NULL) FATAL("Unable to allocate more memory");
   ((OB)L39)->header.tag=AM_BND1124877163_tag;
#ifdef DESTROY_CHK

     ((OB)L39)->header.destroyed=0;
#endif

   L37 = ((AM_BND1124877163) L39);
   L37->asize = L38;
   r1 = L37;
   SATTR(r1,source1,create_source1);
   ret_val2 = r1;
   enew1 = ret_val2;
   SATTR(enew1,fun,ATTR(((AM_BND1124877163) expr11),fun));
   SATTR(enew1,bnd_args,ATTR(((AM_BND1124877163) expr11),bnd_args));
   SATTR(enew1,unbnd_args,ATTR(((AM_BND1124877163) expr11),unbnd_args));
   SATTR(enew1,tp_at,ATTR(((AM_BND1124877163) expr11),tp_at));
   {
    BOOL f_L421_ = TRUE;
    BOOL f_L431_ = TRUE;
    /* loop index variable */
    L181 = 0;
    L31 = ((AM_BND1124877163) expr11);
    L41 = enew1;
    L421_br=L31==NULL?0:ASIZE((AM_BND1124877163)L31); 
    L431_br=L41==NULL?0:ASIZE((AM_BND1124877163)L41); 
    while (1) {
     if(L181>=L421_br)  goto after_loop1; 
     aL421_=ARR((AM_BND1124877163)L31,L181); 
     arg = aL421_;
     if(L181>=L431_br)  goto after_loop1; 
     create_self2 = ((AM_CALL_ARG) NULL);
     create_e = XFORM_69815265(self, ATTR(arg,expr));
     create_m = ATTR(arg,mode1);
     L46=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L46==NULL) FATAL("Unable to allocate more memory");
     ((OB)L46)->header.tag=AM_CALL_ARG_tag;
     L45 = ((AM_CALL_ARG) L46);
     res = L45;
     SATTR(res,expr,create_e);
     SATTR(res,mode1,create_m);
     ret_val3 = res;
     L47 = ret_val3;
     SARR((AM_BND1124877163)L41,L181,(AM_CALL_ARG)L47); 
     ;
     L481_=INTPLUS(L181,1); 
     L181 = L481_;
    }
   }
   after_loop1: ;
   ret_val = ((dAM_EXPR) enew1);
   return ret_val; break;
  case AM_IF_EXPR_tag:
   create_self3 = ((AM_IF_EXPR) NULL);
   create_source2 = ATTR(((AM_IF_EXPR) expr11),source1);
   L50=ZALLOC(sizeof(struct AM_IF_EXPR_struct));
   if (L50==NULL) FATAL("Unable to allocate more memory");
   ((OB)L50)->header.tag=AM_IF_EXPR_tag;
   L49 = ((AM_IF_EXPR) L50);
   r2 = L49;
   SATTR(r2,source1,create_source2);
   ret_val4 = r2;
   enew2 = ret_val4;
   SATTR(enew2,test1,XFORM_69815265(self, ATTR(((AM_IF_EXPR) expr11),test1)));
   SATTR(enew2,if_true,XFORM_69815265(self, ATTR(((AM_IF_EXPR) expr11),if_true)));
   SATTR(enew2,if_false,XFORM_69815265(self, ATTR(((AM_IF_EXPR) expr11),if_false)));
   SATTR(enew2,tp_at,ATTR(((AM_IF_EXPR) expr11),tp_at));
   ret_val = ((dAM_EXPR) enew2);
   return ret_val; break;
  case AM_NEW_EXPR_tag:
   create_self4 = ((AM_NEW_EXPR) NULL);
   create_source3 = ATTR(((AM_NEW_EXPR) expr11),source1);
   L53=ZALLOC(sizeof(struct AM_NEW_EXPR_struct));
   if (L53==NULL) FATAL("Unable to allocate more memory");
   ((OB)L53)->header.tag=AM_NEW_EXPR_tag;
   L52 = ((AM_NEW_EXPR) L53);
   r3 = L52;
   SATTR(r3,source1,create_source3);
   ret_val5 = r3;
   enew3 = ret_val5;
   SATTR(enew3,tp_at,ATTR(((AM_NEW_EXPR) expr11),tp_at));
   SATTR(enew3,asz,XFORM_69815265(self, ATTR(((AM_NEW_EXPR) expr11),asz)));
   ret_val = ((dAM_EXPR) enew3);
   return ret_val; break;
  case AM_IS_VOID_EXPR_tag:
   create_self5 = ((AM_IS_VOID_EXPR) NULL);
   create_source4 = ATTR(((AM_IS_VOID_EXPR) expr11),source1);
   L55=ZALLOC(sizeof(struct AM_IS_VOID_EXPR_struct));
   if (L55==NULL) FATAL("Unable to allocate more memory");
   ((OB)L55)->header.tag=AM_IS_VOID_EXPR_tag;
   L54 = ((AM_IS_VOID_EXPR) L55);
   r4 = L54;
   SATTR(r4,source1,create_source4);
   ret_val6 = r4;
   enew4 = ret_val6;
   SATTR(enew4,tp_at,ATTR(((AM_IS_VOID_EXPR) expr11),tp_at));
   SATTR(enew4,arg,XFORM_69815265(self, ATTR(((AM_IS_VOID_EXPR) expr11),arg)));
   ret_val = ((dAM_EXPR) enew4);
   return ret_val; break;
  case AM_SHARED_EXPR_tag:
   create_self6 = ((AM_SHARED_EXPR) NULL);
   create_source5 = ATTR(((AM_SHARED_EXPR) expr11),source1);
   L57=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
   if (L57==NULL) FATAL("Unable to allocate more memory");
   ((OB)L57)->header.tag=AM_SHARED_EXPR_tag;
   L56 = ((AM_SHARED_EXPR) L57);
   r5 = L56;
   SATTR(r5,source1,create_source5);
   ret_val7 = r5;
   enew5 = ret_val7;
   SATTR(enew5,tp_at,ATTR(((AM_SHARED_EXPR) expr11),tp_at));
   SATTR(enew5,name1,ATTR(((AM_SHARED_EXPR) expr11),name1));
   SATTR(enew5,class_tp,ATTR(((AM_SHARED_EXPR) expr11),class_tp));
   SATTR(enew5,init,ATTR(((AM_SHARED_EXPR) expr11),init));
   SATTR(enew5,is_const,ATTR(((AM_SHARED_EXPR) expr11),is_const));
   SATTR(enew5,as_type,ATTR(((AM_SHARED_EXPR) expr11),as_type));
   ret_val = ((dAM_EXPR) enew5);
   return ret_val; break;
  case AM_ATTR_EXPR_tag:
   create_self7 = ((AM_ATTR_EXPR) NULL);
   create_source6 = ATTR(((AM_ATTR_EXPR) expr11),source1);
   L59=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
   if (L59==NULL) FATAL("Unable to allocate more memory");
   ((OB)L59)->header.tag=AM_ATTR_EXPR_tag;
   L58 = ((AM_ATTR_EXPR) L59);
   r6 = L58;
   SATTR(r6,source1,create_source6);
   ret_val8 = r6;
   enew6 = ret_val8;
   SATTR(enew6,ob,XFORM_69815265(self, ATTR(((AM_ATTR_EXPR) expr11),ob)));
   SATTR(enew6,self_tp,ATTR(((AM_ATTR_EXPR) expr11),self_tp));
   SATTR(enew6,at,ATTR(((AM_ATTR_EXPR) expr11),at));
   SATTR(enew6,tp_at,ATTR(((AM_ATTR_EXPR) expr11),tp_at));
   SATTR(enew6,as_type,ATTR(((AM_ATTR_EXPR) expr11),as_type));
   ret_val = ((dAM_EXPR) enew6);
   return ret_val; break;
  case AM_VAT319982528_tag:
   create_self8 = ((AM_VAT319982528) NULL);
   create_source7 = ATTR(((AM_VAT319982528) expr11),source1);
   L62=ZALLOC(sizeof(struct AM_VAT319982528_struct));
   if (L62==NULL) FATAL("Unable to allocate more memory");
   ((OB)L62)->header.tag=AM_VAT319982528_tag;
   L60 = ((AM_VAT319982528) L62);
   r7 = L60;
   SATTR(r7,source1,create_source7);
   ret_val9 = r7;
   enew7 = ret_val9;
   SATTR(enew7,ob,XFORM_69815265(self, ATTR(((AM_VAT319982528) expr11),ob)));
   SATTR(enew7,at,ATTR(((AM_VAT319982528) expr11),at));
   SATTR(enew7,val1,XFORM_69815265(self, ATTR(((AM_VAT319982528) expr11),val1)));
   SATTR(enew7,real_tp,ATTR(((AM_VAT319982528) expr11),real_tp));
   ret_val = ((dAM_EXPR) enew7);
   return ret_val; break;
  case AM_VAR744470097_tag:
   create_self9 = ((AM_VAR744470097) NULL);
   create_source8 = ATTR(((AM_VAR744470097) expr11),source1);
   L64=ZALLOC(sizeof(struct AM_VAR744470097_struct));
   if (L64==NULL) FATAL("Unable to allocate more memory");
   ((OB)L64)->header.tag=AM_VAR744470097_tag;
   L63 = ((AM_VAR744470097) L64);
   r8 = L63;
   SATTR(r8,source1,create_source8);
   ret_val10 = r8;
   enew8 = ret_val10;
   SATTR(enew8,ob,XFORM_69815265(self, ATTR(((AM_VAR744470097) expr11),ob)));
   SATTR(enew8,ind,XFORM_69815265(self, ATTR(((AM_VAR744470097) expr11),ind)));
   SATTR(enew8,val1,XFORM_69815265(self, ATTR(((AM_VAR744470097) expr11),val1)));
   ret_val = ((dAM_EXPR) enew8);
   return ret_val; break;
  case AM_EXCEPT_EXPR_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_HERE_EXPR_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_GLOBAL_EXPR_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_ANY_EXPR_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_CLU1286269335_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_CLUSTER_EXPR_tag:
   ret_val = expr11;
   return ret_val; break;
  case AM_WHERE_EXPR_tag:
   create_self10 = ((AM_WHERE_EXPR) NULL);
   create_source9 = ATTR(((AM_WHERE_EXPR) expr11),source1);
   L66=ZALLOC(sizeof(struct AM_WHERE_EXPR_struct));
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=AM_WHERE_EXPR_tag;
   L65 = ((AM_WHERE_EXPR) L66);
   r9 = L65;
   SATTR(r9,source1,create_source9);
   ret_val11 = r9;
   enew9 = ret_val11;
   SATTR(enew9,arg,XFORM_69815265(self, ATTR(((AM_WHERE_EXPR) expr11),arg)));
   ret_val = ((dAM_EXPR) enew9);
   return ret_val; break;
  case AM_NEAR_EXPR_tag:
   create_self11 = ((AM_NEAR_EXPR) NULL);
   create_src = ATTR(((AM_NEAR_EXPR) expr11),source1);
   L68=ZALLOC(sizeof(struct AM_NEAR_EXPR_struct));
   if (L68==NULL) FATAL("Unable to allocate more memory");
   ((OB)L68)->header.tag=AM_NEAR_EXPR_tag;
   L67 = ((AM_NEAR_EXPR) L68);
   r10 = L67;
   SATTR(r10,source1,create_src);
   ret_val12 = r10;
   enew10 = ret_val12;
   SATTR(enew10,arg,XFORM_69815265(self, ATTR(((AM_NEAR_EXPR) expr11),arg)));
   ret_val = ((dAM_EXPR) enew10);
   return ret_val; break;
  case AM_FAR_EXPR_tag:
   create_self12 = ((AM_FAR_EXPR) NULL);
   create_src1 = ATTR(((AM_FAR_EXPR) expr11),source1);
   L70=ZALLOC(sizeof(struct AM_FAR_EXPR_struct));
   if (L70==NULL) FATAL("Unable to allocate more memory");
   ((OB)L70)->header.tag=AM_FAR_EXPR_tag;
   L69 = ((AM_FAR_EXPR) L70);
   r11 = L69;
   SATTR(r11,source1,create_src1);
   ret_val13 = r11;
   enew11 = ret_val13;
   SATTR(enew11,arg,XFORM_69815265(self, ATTR(((AM_FAR_EXPR) expr11),arg)));
   ret_val = ((dAM_EXPR) enew11);
   return ret_val; break;
  case AM_AT_EXPR_tag:
   create_self13 = ((AM_AT_EXPR) NULL);
   create_src2 = ATTR(((AM_AT_EXPR) expr11),source1);
   L73=ZALLOC(sizeof(struct AM_AT_EXPR_struct));
   if (L73==NULL) FATAL("Unable to allocate more memory");
   ((OB)L73)->header.tag=AM_AT_EXPR_tag;
   L72 = ((AM_AT_EXPR) L73);
   r12 = L72;
   SATTR(r12,source1,create_src2);
   ret_val14 = r12;
   enew12 = ret_val14;
   SATTR(enew12,at,XFORM_69815265(self, ATTR(((AM_AT_EXPR) expr11),at)));
   SATTR(enew12,e,XFORM_69815265(self, ATTR(((AM_AT_EXPR) expr11),e)));
   ret_val = ((dAM_EXPR) enew12);
   return ret_val; break;
  case AM_ROU1916046290_tag:
   create_self14 = ((AM_ROU1916046290) NULL);
   L741_=ASIZE((AM_ROU1916046290)expr11); 
   create_nargs2 = L741_;
   create_source10 = ATTR(((AM_ROU1916046290) expr11),source1);
   L76 = create_nargs2;
   L78=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L76)*sizeof(AM_CALL_ARG);
   L77=ZALLOC_BIG(L78);
   if (L77==NULL) FATAL("Unable to allocate more memory");
   ((OB)L77)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

     ((OB)L77)->header.destroyed=0;
#endif

   L75 = ((AM_ROU1916046290) L77);
   L75->asize = L76;
   r13 = L75;
   SATTR(r13,source1,create_source10);
   ret_val15 = r13;
   enew13 = ret_val15;
   SATTR(enew13,fun,ATTR(((AM_ROU1916046290) expr11),fun));
   SATTR(enew13,as_type,ATTR(((AM_ROU1916046290) expr11),as_type));
   {
    BOOL f_L791_ = TRUE;
    BOOL f_L801_ = TRUE;
    /* loop index variable */
    L191 = 0;
    L51 = ((AM_ROU1916046290) expr11);
    L61 = enew13;
    L791_br=L51==NULL?0:ASIZE((AM_ROU1916046290)L51); 
    L801_br=L61==NULL?0:ASIZE((AM_ROU1916046290)L61); 
    while (1) {
     if(L191>=L791_br)  goto after_loop2; 
     aL791_=ARR((AM_ROU1916046290)L51,L191); 
     arg = aL791_;
     if(L191>=L801_br)  goto after_loop2; 
     create_self15 = ((AM_CALL_ARG) NULL);
     create_e1 = XFORM_69815265(self, ATTR(arg,expr));
     create_m1 = ATTR(arg,mode1);
     L84=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L84==NULL) FATAL("Unable to allocate more memory");
     ((OB)L84)->header.tag=AM_CALL_ARG_tag;
     L83 = ((AM_CALL_ARG) L84);
     res1 = L83;
     SATTR(res1,expr,create_e1);
     SATTR(res1,mode1,create_m1);
     ret_val16 = res1;
     L85 = ret_val16;
     SARR((AM_ROU1916046290)L61,L191,(AM_CALL_ARG)L85); 
     ;
     L861_=INTPLUS(L191,1); 
     L191 = L861_;
    }
   }
   after_loop2: ;
   if (ATTR(self,record_calls)) {
    SATTR(self,calls1,FLISTd805769956(ATTR(self,calls1), ((dAM_EXPR) enew13)));
   }
   ret_val = ((dAM_EXPR) enew13);
   return ret_val; break;
  case AM_ITE1809135850_tag:
   create_self16 = ((AM_ITE1809135850) NULL);
   L871_=ASIZE((AM_ITE1809135850)expr11); 
   create_nargs3 = L871_;
   create_source11 = ATTR(((AM_ITE1809135850) expr11),source1);
   L89 = create_nargs3;
   L92=(sizeof(struct AM_ITE1809135850_struct)-sizeof(AM_CALL_ARG))+(L89)*sizeof(AM_CALL_ARG);
   L90=ZALLOC_BIG(L92);
   if (L90==NULL) FATAL("Unable to allocate more memory");
   ((OB)L90)->header.tag=AM_ITE1809135850_tag;
#ifdef DESTROY_CHK

     ((OB)L90)->header.destroyed=0;
#endif

   L88 = ((AM_ITE1809135850) L90);
   L88->asize = L89;
   r14 = L88;
   SATTR(r14,source1,create_source11);
   ret_val17 = r14;
   enew14 = ret_val17;
   SATTR(enew14,fun,ATTR(((AM_ITE1809135850) expr11),fun));
   SATTR(enew14,init,XFORM_1427763452(self, ATTR(((AM_ITE1809135850) expr11),init)));
   SATTR(enew14,lp,ATTR(self,new_loop_stmt));
   SATTR(ATTR(self,new_loop_stmt),its1,FLISTA943034044(ATTR(ATTR(self,new_loop_stmt),its1), enew14));
   {
    BOOL f_L931_ = TRUE;
    BOOL f_L941_ = TRUE;
    /* loop index variable */
    L201 = 0;
    L71 = ((AM_ITE1809135850) expr11);
    L81 = enew14;
    L931_br=L71==NULL?0:ASIZE((AM_ITE1809135850)L71); 
    L941_br=L81==NULL?0:ASIZE((AM_ITE1809135850)L81); 
    while (1) {
     if(L201>=L931_br)  goto after_loop3; 
     aL931_=ARR((AM_ITE1809135850)L71,L201); 
     arg = aL931_;
     if(L201>=L941_br)  goto after_loop3; 
     create_self17 = ((AM_CALL_ARG) NULL);
     create_e2 = XFORM_69815265(self, ATTR(arg,expr));
     create_m2 = ATTR(arg,mode1);
     L97=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L97==NULL) FATAL("Unable to allocate more memory");
     ((OB)L97)->header.tag=AM_CALL_ARG_tag;
     L96 = ((AM_CALL_ARG) L97);
     res2 = L96;
     SATTR(res2,expr,create_e2);
     SATTR(res2,mode1,create_m2);
     ret_val18 = res2;
     L98 = ret_val18;
     SARR((AM_ITE1809135850)L81,L201,(AM_CALL_ARG)L98); 
     ;
     L991_=INTPLUS(L201,1); 
     L201 = L991_;
    }
   }
   after_loop3: ;
   if (ATTR(self,record_calls)) {
    SATTR(self,calls1,FLISTd805769956(ATTR(self,calls1), ((dAM_EXPR) enew14)));
   }
   ret_val = ((dAM_EXPR) enew14);
   return ret_val; break;
  case AM_BND260301329_tag:
   create_self18 = ((AM_BND260301329) NULL);
   L1001_=ASIZE((AM_BND260301329)expr11); 
   create_nargs4 = L1001_;
   create_source12 = ATTR(((AM_BND260301329) expr11),source1);
   L103 = create_nargs4;
   L105=(sizeof(struct AM_BND260301329_struct)-sizeof(AM_CALL_ARG))+(L103)*sizeof(AM_CALL_ARG);
   L104=ZALLOC_BIG(L105);
   if (L104==NULL) FATAL("Unable to allocate more memory");
   ((OB)L104)->header.tag=AM_BND260301329_tag;
#ifdef DESTROY_CHK

     ((OB)L104)->header.destroyed=0;
#endif

   L102 = ((AM_BND260301329) L104);
   L102->asize = L103;
   r15 = L102;
   SATTR(r15,source1,create_source12);
   ret_val19 = r15;
   enew15 = ret_val19;
   SATTR(enew15,br,XFORM_69815265(self, ATTR(((AM_BND260301329) expr11),br)));
   SATTR(enew15,br_tp,ATTR(((AM_BND260301329) expr11),br_tp));
   {
    BOOL f_L1061_ = TRUE;
    BOOL f_L1071_ = TRUE;
    /* loop index variable */
    L221 = 0;
    L91 = ((AM_BND260301329) expr11);
    L101 = enew15;
    L1061_br=L91==NULL?0:ASIZE((AM_BND260301329)L91); 
    L1071_br=L101==NULL?0:ASIZE((AM_BND260301329)L101); 
    while (1) {
     if(L221>=L1061_br)  goto after_loop4; 
     aL1061_=ARR((AM_BND260301329)L91,L221); 
     arg = aL1061_;
     if(L221>=L1071_br)  goto after_loop4; 
     create_self19 = ((AM_CALL_ARG) NULL);
     create_e3 = XFORM_69815265(self, ATTR(arg,expr));
     create_m3 = ATTR(arg,mode1);
     L110=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L110==NULL) FATAL("Unable to allocate more memory");
     ((OB)L110)->header.tag=AM_CALL_ARG_tag;
     L109 = ((AM_CALL_ARG) L110);
     res3 = L109;
     SATTR(res3,expr,create_e3);
     SATTR(res3,mode1,create_m3);
     ret_val20 = res3;
     L111 = ret_val20;
     SARR((AM_BND260301329)L101,L221,(AM_CALL_ARG)L111); 
     ;
     L1121_=INTPLUS(L221,1); 
     L221 = L1121_;
    }
   }
   after_loop4: ;
   if (ATTR(self,record_calls)) {
    SATTR(self,calls1,FLISTd805769956(ATTR(self,calls1), ((dAM_EXPR) enew15)));
   }
   ret_val = ((dAM_EXPR) enew15);
   return ret_val; break;
  case AM_BND367211769_tag:
   create_self20 = ((AM_BND367211769) NULL);
   L1131_=ASIZE((AM_BND367211769)expr11); 
   create_nargs5 = L1131_;
   create_source13 = ATTR(((AM_BND367211769) expr11),source1);
   L115 = create_nargs5;
   L117=(sizeof(struct AM_BND367211769_struct)-sizeof(AM_CALL_ARG))+(L115)*sizeof(AM_CALL_ARG);
   L116=ZALLOC_BIG(L117);
   if (L116==NULL) FATAL("Unable to allocate more memory");
   ((OB)L116)->header.tag=AM_BND367211769_tag;
#ifdef DESTROY_CHK

     ((OB)L116)->header.destroyed=0;
#endif

   L114 = ((AM_BND367211769) L116);
   L114->asize = L115;
   r16 = L114;
   SATTR(r16,source1,create_source13);
   ret_val21 = r16;
   enew16 = ret_val21;
   SATTR(enew16,bi,XFORM_69815265(self, ATTR(((AM_BND367211769) expr11),bi)));
   SATTR(enew16,bi_tp,ATTR(((AM_BND367211769) expr11),bi_tp));
   SATTR(enew16,init,XFORM_1427763452(self, ATTR(((AM_BND367211769) expr11),init)));
   SATTR(enew16,lp,ATTR(self,new_loop_stmt));
   SATTR(ATTR(self,new_loop_stmt),bits1,FLISTA1715286767(ATTR(ATTR(self,new_loop_stmt),bits1), enew16));
   {
    BOOL f_L1181_ = TRUE;
    BOOL f_L1191_ = TRUE;
    /* loop index variable */
    L231 = 0;
    L121 = ((AM_BND367211769) expr11);
    L131 = enew16;
    L1181_br=L121==NULL?0:ASIZE((AM_BND367211769)L121); 
    L1191_br=L131==NULL?0:ASIZE((AM_BND367211769)L131); 
    while (1) {
     if(L231>=L1181_br)  goto after_loop5; 
     aL1181_=ARR((AM_BND367211769)L121,L231); 
     arg = aL1181_;
     if(L231>=L1191_br)  goto after_loop5; 
     create_self21 = ((AM_CALL_ARG) NULL);
     create_e4 = XFORM_69815265(self, ATTR(arg,expr));
     create_m4 = ATTR(arg,mode1);
     L123=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L123==NULL) FATAL("Unable to allocate more memory");
     ((OB)L123)->header.tag=AM_CALL_ARG_tag;
     L122 = ((AM_CALL_ARG) L123);
     res4 = L122;
     SATTR(res4,expr,create_e4);
     SATTR(res4,mode1,create_m4);
     ret_val22 = res4;
     L124 = ret_val22;
     SARR((AM_BND367211769)L131,L231,(AM_CALL_ARG)L124); 
     ;
     L1251_=INTPLUS(L231,1); 
     L231 = L1251_;
    }
   }
   after_loop5: ;
   if (ATTR(self,record_calls)) {
    SATTR(self,calls1,FLISTd805769956(ATTR(self,calls1), ((dAM_EXPR) enew16)));
   }
   ret_val = ((dAM_EXPR) enew16);
   return ret_val; break;
  case AM_EXT_CALL_EXPR_tag:
   create_self22 = ((AM_EXT_CALL_EXPR) NULL);
   L1261_=ASIZE((AM_EXT_CALL_EXPR)expr11); 
   create_nargs6 = L1261_;
   create_source14 = ATTR(((AM_EXT_CALL_EXPR) expr11),source1);
   create_nm = ATTR(((AM_EXT_CALL_EXPR) expr11),nm);
   create_self23 = create_self22;
   create_nargs7 = create_nargs6;
   create_source15 = create_source14;
   L128 = create_nargs7;
   L130=(sizeof(struct AM_EXT_CALL_EXPR_struct)-sizeof(AM_CALL_ARG))+(L128)*sizeof(AM_CALL_ARG);
   L129=ZALLOC_BIG(L130);
   if (L129==NULL) FATAL("Unable to allocate more memory");
   ((OB)L129)->header.tag=AM_EXT_CALL_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L129)->header.destroyed=0;
#endif

   L127 = ((AM_EXT_CALL_EXPR) L129);
   L127->asize = L128;
   r18 = L127;
   SATTR(r18,source1,create_source15);
   ret_val24 = r18;
   r17 = ret_val24;
   SATTR(r17,nm,create_nm);
   ret_val23 = r17;
   enew17 = ret_val23;
   SATTR(enew17,fun,ATTR(((AM_EXT_CALL_EXPR) expr11),fun));
   {
    BOOL f_L1321_ = TRUE;
    BOOL f_L1331_ = TRUE;
    /* loop index variable */
    L241 = 0;
    L141 = ((AM_EXT_CALL_EXPR) expr11);
    L151 = enew17;
    L1321_br=L141==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L141); 
    L1331_br=L151==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L151); 
    while (1) {
     if(L241>=L1321_br)  goto after_loop6; 
     aL1321_=ARR((AM_EXT_CALL_EXPR)L141,L241); 
     arg = aL1321_;
     if(L241>=L1331_br)  goto after_loop6; 
     create_self24 = ((AM_CALL_ARG) NULL);
     create_e5 = XFORM_69815265(self, ATTR(arg,expr));
     create_m5 = ATTR(arg,mode1);
     L136=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L136==NULL) FATAL("Unable to allocate more memory");
     ((OB)L136)->header.tag=AM_CALL_ARG_tag;
     L135 = ((AM_CALL_ARG) L136);
     res5 = L135;
     SATTR(res5,expr,create_e5);
     SATTR(res5,mode1,create_m5);
     ret_val25 = res5;
     L137 = ret_val25;
     SARR((AM_EXT_CALL_EXPR)L151,L241,(AM_CALL_ARG)L137); 
     ;
     L1381_=INTPLUS(L241,1); 
     L241 = L1381_;
    }
   }
   after_loop6: ;
   if (ATTR(self,record_calls)) {
    SATTR(self,calls1,FLISTd805769956(ATTR(self,calls1), ((dAM_EXPR) enew17)));
   }
   ret_val = ((dAM_EXPR) enew17);
   return ret_val; break;
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
   ret_val = expr11;
   return ret_val; break;
  case AM_STMT_EXPR_tag:
   create_self25 = ((AM_STMT_EXPR) NULL);
   create_source16 = ATTR(((AM_STMT_EXPR) expr11),source1);
   L140=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
   if (L140==NULL) FATAL("Unable to allocate more memory");
   ((OB)L140)->header.tag=AM_STMT_EXPR_tag;
   L139 = ((AM_STMT_EXPR) L140);
   r19 = L139;
   SATTR(r19,source1,create_source16);
   ret_val26 = r19;
   enew18 = ret_val26;
   save_calls = ATTR(self,calls1);
   SATTR(self,calls1,((FLISTdAM_EXPR) NULL));
   SATTR(enew18,stmts,XFORM_1427763452(self, ATTR(((AM_STMT_EXPR) expr11),stmts)));
   SATTR(enew18,calls1,ATTR(self,calls1));
   SATTR(self,calls1,FLISTd1364738838(save_calls, ATTR(self,calls1)));
   SATTR(enew18,locals1,FLISTA1446031755(((FLISTA725283029) NULL), FLISTA1151473122(ATTR(((AM_STMT_EXPR) expr11),locals1))));
   {
    struct FLISTA2119642552_frame_struct other8_0;
FLISTA2119642552_frame noname1 = &other8_0;
    noname1->state = 0;
    while (1) {
     L142 = ATTR(enew18,locals1);
     L143 = ATTR(self,subst);
     if (noname1->state == 0) {
      L161 = ATTR(((AM_STMT_EXPR) expr11),locals1);
      noname1->self = L161;
     }
     L144 = FLISTA2119642552(noname1);
     if (noname1->state == -1) {
      goto after_loop7;
     }
     SATTR(enew18,locals1,FLISTA1817671564(L142, FMAPAM1614132657(L143, L144)));
    }
   }
   after_loop7: ;
   SATTR(enew18,expr,XFORM_69815265(self, ATTR(((AM_STMT_EXPR) expr11),expr)));
   ret_val = ((dAM_EXPR) enew18);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin XFORM_CODE::xform_expr($AM_EXPR):$AM_EXPR\n./Middle/inline.sa:770:16");
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT XFORM_1427763452(XFORM_CODE self, dAM_STMT code11) {
 dAM_STMT ret_val;
 dAM_STMT temp11;
 dAM_STMT copy;
 dAM_STMT L1;
 dAM_STMT L2;
 dAM_STMT L3;
 if ((code11==((dAM_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 temp11 = XFORM_1958700984(self, code11);
 if ((temp11==((dAM_STMT) NULL))) {
  ret_val = ((dAM_STMT) NULL);
  return ret_val;
 }
 copy = temp11;
 while (1) {
  L1 = code11;
  code11 = (*dAM_ST775224435[TAG(L1)])(L1);
  if ((code11==((dAM_STMT) NULL))) {
   goto after_loop;
  }
  L2 = temp11;
  (*dAM_ST1372701974[TAG(L2)])(L2, XFORM_1958700984(self, code11));
  L3 = temp11;
  temp11 = (*dAM_ST775224435[TAG(L3)])(L3);
  if ((temp11==((dAM_STMT) NULL))) {
   goto after_loop;
  }
 }
 after_loop: ;
 ret_val = copy;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_STMT XFORM_1958700984(XFORM_CODE self, dAM_STMT stmt1) {
 dAM_STMT ret_val;
 AM_ASSIGN_STMT snew;
 AM_PRE_STMT snew1;
 AM_POST_STMT snew2;
 AM_INITIAL_STMT snew3;
 AM_ASSERT_STMT snew4;
 AM_INV1167837230 snew5;
 AM_EXPR_STMT snew6;
 AM_IF_STMT snew7;
 AM_IF_STMT old_eif;
 AM_CASE_STMT snew8;
 FLISTdAM_STMT L11;
 AM_TYPECASE_STMT snew9;
 FLISTdAM_STMT L21;
 AM_ASSIGN_STMT create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val1;
 AM_ASSIGN_STMT r;
 AM_PRE_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_PRE_STMT ret_val2;
 AM_PRE_STMT r1;
 AM_POST_STMT create_self2;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_POST_STMT ret_val3;
 AM_POST_STMT r2;
 AM_INITIAL_STMT create_self3;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_INITIAL_STMT ret_val4;
 AM_INITIAL_STMT r3;
 AM_ASSERT_STMT create_self4;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_ASSERT_STMT ret_val5;
 AM_ASSERT_STMT r4;
 AM_INV1167837230 create_self5;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_INV1167837230 ret_val6;
 AM_INV1167837230 r5;
 AM_EXPR_STMT create_self6;
 SFILE_ID create_source6 = SFILE_ID_zero;
 AM_EXPR_STMT ret_val7;
 AM_EXPR_STMT r6;
 AM_IF_STMT create_self7;
 SFILE_ID create_source7 = SFILE_ID_zero;
 AM_IF_STMT ret_val8;
 AM_IF_STMT r7;
 AM_CASE_STMT create_self8;
 SFILE_ID create_source8 = SFILE_ID_zero;
 AM_CASE_STMT ret_val9;
 AM_CASE_STMT r8;
 AM_TYPECASE_STMT create_self9;
 SFILE_ID create_source9 = SFILE_ID_zero;
 AM_TYPECASE_STMT ret_val10;
 AM_TYPECASE_STMT r9;
 AM_BREAK_STMT create_self10;
 SFILE_ID create_source10 = SFILE_ID_zero;
 AM_BREAK_STMT ret_val11;
 AM_BREAK_STMT r10;
 AM_ASSIGN_STMT L3;
 OB L4;
 AM_PRE_STMT L5;
 OB L6;
 AM_POST_STMT L7;
 OB L8;
 AM_INITIAL_STMT L9;
 OB L10;
 AM_ASSERT_STMT L12;
 OB L13;
 AM_INV1167837230 L14;
 OB L15;
 AM_EXPR_STMT L16;
 OB L17;
 AM_IF_STMT L18;
 OB L19;
 AM_CASE_STMT L20;
 OB L22;
 FLISTdAM_STMT L23;
 dAM_STMT L24;
 BOOL L25;
 BOOL L261_;
 AM_TYPECASE_STMT L27;
 OB L28;
 FLISTdAM_STMT L29;
 dAM_STMT L30;
 BOOL L31;
 BOOL L321_;
 AM_BREAK_STMT L33;
 OB L34;
 switch (TAG(stmt1)) {
  case AM_ASSIGN_STMT_tag:
   create_self = ((AM_ASSIGN_STMT) NULL);
   create_source = ATTR(((AM_ASSIGN_STMT) stmt1),source1);
   L4=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
   if (L4==NULL) FATAL("Unable to allocate more memory");
   ((OB)L4)->header.tag=AM_ASSIGN_STMT_tag;
   L3 = ((AM_ASSIGN_STMT) L4);
   r = L3;
   SATTR(r,source1,create_source);
   ret_val1 = r;
   snew = ret_val1;
   SATTR(snew,dest,XFORM_69815265(self, ATTR(((AM_ASSIGN_STMT) stmt1),dest)));
   SATTR(snew,src1,XFORM_69815265(self, ATTR(((AM_ASSIGN_STMT) stmt1),src1)));
   ret_val = ((dAM_STMT) snew);
   return ret_val; break;
  case AM_PRE_STMT_tag:
   create_self1 = ((AM_PRE_STMT) NULL);
   create_source1 = ATTR(((AM_PRE_STMT) stmt1),source1);
   L6=ZALLOC(sizeof(struct AM_PRE_STMT_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AM_PRE_STMT_tag;
   L5 = ((AM_PRE_STMT) L6);
   r1 = L5;
   SATTR(r1,source1,create_source1);
   ret_val2 = r1;
   snew1 = ret_val2;
   SATTR(snew1,tp,ATTR(((AM_PRE_STMT) stmt1),tp));
   SATTR(snew1,test1,XFORM_69815265(self, ATTR(((AM_PRE_STMT) stmt1),test1)));
   ret_val = ((dAM_STMT) snew1);
   return ret_val; break;
  case AM_POST_STMT_tag:
   create_self2 = ((AM_POST_STMT) NULL);
   create_source2 = ATTR(((AM_POST_STMT) stmt1),source1);
   L8=ZALLOC(sizeof(struct AM_POST_STMT_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=AM_POST_STMT_tag;
   L7 = ((AM_POST_STMT) L8);
   r2 = L7;
   SATTR(r2,source1,create_source2);
   ret_val3 = r2;
   snew2 = ret_val3;
   SATTR(snew2,tp,ATTR(((AM_POST_STMT) stmt1),tp));
   SATTR(snew2,test1,XFORM_69815265(self, ATTR(((AM_POST_STMT) stmt1),test1)));
   ret_val = ((dAM_STMT) snew2);
   return ret_val; break;
  case AM_INITIAL_STMT_tag:
   create_self3 = ((AM_INITIAL_STMT) NULL);
   create_source3 = ATTR(((AM_INITIAL_STMT) stmt1),source1);
   L10=ZALLOC(sizeof(struct AM_INITIAL_STMT_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=AM_INITIAL_STMT_tag;
   L9 = ((AM_INITIAL_STMT) L10);
   r3 = L9;
   SATTR(r3,source1,create_source3);
   ret_val4 = r3;
   snew3 = ret_val4;
   SATTR(snew3,tp,ATTR(((AM_INITIAL_STMT) stmt1),tp));
   SATTR(snew3,stmts,XFORM_1427763452(self, ATTR(((AM_INITIAL_STMT) stmt1),stmts)));
   ret_val = ((dAM_STMT) snew3);
   return ret_val; break;
  case AM_ASSERT_STMT_tag:
   create_self4 = ((AM_ASSERT_STMT) NULL);
   create_source4 = ATTR(((AM_ASSERT_STMT) stmt1),source1);
   L13=ZALLOC(sizeof(struct AM_ASSERT_STMT_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=AM_ASSERT_STMT_tag;
   L12 = ((AM_ASSERT_STMT) L13);
   r4 = L12;
   SATTR(r4,source1,create_source4);
   ret_val5 = r4;
   snew4 = ret_val5;
   SATTR(snew4,tp,ATTR(((AM_ASSERT_STMT) stmt1),tp));
   SATTR(snew4,test1,XFORM_69815265(self, ATTR(((AM_ASSERT_STMT) stmt1),test1)));
   ret_val = ((dAM_STMT) snew4);
   return ret_val; break;
  case AM_INV1167837230_tag:
   create_self5 = ((AM_INV1167837230) NULL);
   create_source5 = ATTR(((AM_INV1167837230) stmt1),source1);
   L15=ZALLOC(sizeof(struct AM_INV1167837230_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=AM_INV1167837230_tag;
   L14 = ((AM_INV1167837230) L15);
   r5 = L14;
   SATTR(r5,source1,create_source5);
   ret_val6 = r5;
   snew5 = ret_val6;
   SATTR(snew5,sig1,ATTR(((AM_INV1167837230) stmt1),sig1));
   ret_val = ((dAM_STMT) snew5);
   return ret_val; break;
  case AM_EXPR_STMT_tag:
   create_self6 = ((AM_EXPR_STMT) NULL);
   create_source6 = ATTR(((AM_EXPR_STMT) stmt1),source1);
   L17=ZALLOC(sizeof(struct AM_EXPR_STMT_struct));
   if (L17==NULL) FATAL("Unable to allocate more memory");
   ((OB)L17)->header.tag=AM_EXPR_STMT_tag;
   L16 = ((AM_EXPR_STMT) L17);
   r6 = L16;
   SATTR(r6,source1,create_source6);
   ret_val7 = r6;
   snew6 = ret_val7;
   SATTR(snew6,expr,XFORM_69815265(self, ATTR(((AM_EXPR_STMT) stmt1),expr)));
   ret_val = ((dAM_STMT) snew6);
   return ret_val; break;
  case AM_IF_STMT_tag:
   create_self7 = ((AM_IF_STMT) NULL);
   create_source7 = ATTR(((AM_IF_STMT) stmt1),source1);
   L19=ZALLOC(sizeof(struct AM_IF_STMT_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=AM_IF_STMT_tag;
   L18 = ((AM_IF_STMT) L19);
   r7 = L18;
   SATTR(r7,source1,create_source7);
   ret_val8 = r7;
   snew7 = ret_val8;
   old_eif = ATTR(self,enclosing_if);
   SATTR(self,enclosing_if,snew7);
   SATTR(snew7,test1,XFORM_69815265(self, ATTR(((AM_IF_STMT) stmt1),test1)));
   SATTR(self,true_branch,TRUE);
   SATTR(snew7,if_true,XFORM_1427763452(self, ATTR(((AM_IF_STMT) stmt1),if_true)));
   SATTR(self,true_branch,FALSE);
   SATTR(snew7,if_false,XFORM_1427763452(self, ATTR(((AM_IF_STMT) stmt1),if_false)));
   SATTR(self,enclosing_if,old_eif);
   ret_val = ((dAM_STMT) snew7);
   return ret_val; break;
  case AM_CASE_STMT_tag:
   create_self8 = ((AM_CASE_STMT) NULL);
   create_source8 = ATTR(((AM_CASE_STMT) stmt1),source1);
   L22=ZALLOC(sizeof(struct AM_CASE_STMT_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=AM_CASE_STMT_tag;
   L20 = ((AM_CASE_STMT) L22);
   r8 = L20;
   SATTR(r8,source1,create_source8);
   ret_val9 = r8;
   snew8 = ret_val9;
   SATTR(snew8,test1,XFORM_69815265(self, ATTR(((AM_CASE_STMT) stmt1),test1)));
   SATTR(snew8,tgts,ATTR(((AM_CASE_STMT) stmt1),tgts));
   SATTR(snew8,stmts,FLISTd846796316(((FLISTdAM_STMT) NULL), FLISTd355912233(ATTR(((AM_CASE_STMT) stmt1),stmts))));
   {
    struct FLISTd1374595573_frame_struct other1_0;
FLISTd1374595573_frame noname1 = &other1_0;
    noname1->state = 0;
    while (1) {
     L23 = ATTR(snew8,stmts);
     if (noname1->state == 0) {
      L11 = ATTR(((AM_CASE_STMT) stmt1),stmts);
      noname1->self = L11;
     }
     L24 = FLISTd1374595573(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     SATTR(snew8,stmts,FLISTd506668277(L23, XFORM_1427763452(self, L24)));
    }
   }
   after_loop: ;
   SATTR(snew8,no_else,ATTR(((AM_CASE_STMT) stmt1),no_else));
   L25 = ATTR(snew8,no_else);
   L261_=!(L25); 
   if (L261_) {
    SATTR(snew8,else_stmts,XFORM_1427763452(self, ATTR(((AM_CASE_STMT) stmt1),else_stmts)));
   }
   ret_val = ((dAM_STMT) snew8);
   return ret_val; break;
  case AM_TYPECASE_STMT_tag:
   create_self9 = ((AM_TYPECASE_STMT) NULL);
   create_source9 = ATTR(((AM_TYPECASE_STMT) stmt1),source1);
   L28=ZALLOC(sizeof(struct AM_TYPECASE_STMT_struct));
   if (L28==NULL) FATAL("Unable to allocate more memory");
   ((OB)L28)->header.tag=AM_TYPECASE_STMT_tag;
   L27 = ((AM_TYPECASE_STMT) L28);
   r9 = L27;
   SATTR(r9,source1,create_source9);
   ret_val10 = r9;
   snew9 = ret_val10;
   SATTR(snew9,test1,FMAPAM1614132657(ATTR(self,subst), ATTR(((AM_TYPECASE_STMT) stmt1),test1)));
   SATTR(snew9,tgts,ATTR(((AM_TYPECASE_STMT) stmt1),tgts));
   SATTR(snew9,stmts,FLISTd846796316(((FLISTdAM_STMT) NULL), FLISTd355912233(ATTR(((AM_TYPECASE_STMT) stmt1),stmts))));
   {
    struct FLISTd1374595573_frame_struct other2_0;
FLISTd1374595573_frame noname2 = &other2_0;
    noname2->state = 0;
    while (1) {
     L29 = ATTR(snew9,stmts);
     if (noname2->state == 0) {
      L21 = ATTR(((AM_TYPECASE_STMT) stmt1),stmts);
      noname2->self = L21;
     }
     L30 = FLISTd1374595573(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     SATTR(snew9,stmts,FLISTd506668277(L29, XFORM_1427763452(self, L30)));
    }
   }
   after_loop1: ;
   SATTR(snew9,no_else,ATTR(((AM_TYPECASE_STMT) stmt1),no_else));
   L31 = ATTR(snew9,no_else);
   L321_=!(L31); 
   if (L321_) {
    SATTR(snew9,else_stmts,XFORM_1427763452(self, ATTR(((AM_TYPECASE_STMT) stmt1),else_stmts)));
   }
   ret_val = ((dAM_STMT) snew9);
   return ret_val; break;
  case AM_RETURN_STMT_tag:
   SATTR(self,return_val,XFORM_69815265(self, ATTR(((AM_RETURN_STMT) stmt1),val1)));
   ret_val = ((dAM_STMT) NULL);
   return ret_val; break;
  case AM_YIELD_STMT_tag:
   SATTR(self,yield_val,XFORM_69815265(self, ATTR(((AM_YIELD_STMT) stmt1),val1)));
   SATTR(self,if_encl_yield,ATTR(self,enclosing_if));
   SATTR(self,true_br_yield,ATTR(self,true_branch));
   SATTR(self,after_yield,XFORM_1427763452(self, ATTR(((AM_YIELD_STMT) stmt1),next)));
   ret_val = ((dAM_STMT) NULL);
   return ret_val; break;
  case AM_BREAK_STMT_tag:
   create_self10 = ((AM_BREAK_STMT) NULL);
   create_source10 = ATTR(((AM_BREAK_STMT) stmt1),source1);
   L34=ZALLOC(sizeof(struct AM_BREAK_STMT_struct));
   if (L34==NULL) FATAL("Unable to allocate more memory");
   ((OB)L34)->header.tag=AM_BREAK_STMT_tag;
   L33 = ((AM_BREAK_STMT) L34);
   r10 = L33;
   SATTR(r10,source1,create_source10);
   ret_val11 = r10;
   ret_val = ((dAM_STMT) ret_val11);
   return ret_val; break;
  case AM_LOOP_STMT_tag:
   SATTR(self,loop_stmt,((AM_LOOP_STMT) stmt1));
   ret_val = ((dAM_STMT) NULL);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin XFORM_CODE::xform_stmt($AM_STMT):$AM_STMT\n./Middle/inline.sa:676:16");
 }
}


#undef IS_ITER
#define IS_ITER 0

void INLINE15806555(INLINE_ITER self, AM_ROUT_DEF rout) {
 XFORM_CODE xc;
 AM_LOOP_STMT proxy_loop_stmt;
 FLISTA725283029 L11;
 AM_IF_STMT iey;
 dAM_STMT s1;
 TP_CLASS bool_tp;
 SIG notsig;
 AM_ROU1916046290 yc;
 XFORM_CODE create_self;
 FMAPAM1145008811 create_subst;
 XFORM_CODE ret_val;
 XFORM_CODE xc1;
 AM_LOOP_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOOP_STMT ret_val1;
 AM_LOOP_STMT r;
 CHANGE_VARS after_saving_sel;
 OUT create_self2;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val9;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r5;
 ARRAYdTP create_self3;
 INT create_n = INT_zero;
 ARRAYdTP ret_val12;
 SIG create_self4;
 dTP create_ntp;
 ARRAYdTP create_tparr;
 IDENT create_nname = IDENT_zero;
 ARRAYARG create_nargs;
 dTP create_nret;
 BOOL create_nsame = BOOL_zero;
 SIG ret_val13;
 SIG r6;
 AM_ROU1916046290 create_self5;
 INT create_nargs1 = INT_zero;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val14;
 AM_ROU1916046290 r7;
 AM_CALL_ARG create_self6;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val15;
 AM_CALL_ARG res;
 CHANGE_VARS after_saving_sel1;
 XFORM_CODE L2;
 OB L3;
 AM_LOOP_STMT L4;
 OB L5;
 FMAPAM1145008811 L6;
 FLISTA725283029 L9;
 FMAPAM1145008811 L10;
 AM_LOCAL_EXPR L12;
 dAM_STMT L13;
 dAM_STMT L14;
 BOOL L15;
 BOOL L161_;
 extern STR foundc1834005908;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L21;
 extern STR name52;
 FILE1 L23;
 OB L24;
 FILE1 L26;
 OB L27;
 extern STR name50;
 FILE1 L29;
 OB L30;
 ARRAYdTP L32;
 INT L33;
 OB L34;
 INT L35;
 SIG L36;
 OB L37;
 dAM_EXPR L38;
 AM_ROU1916046290 L39;
 INT L40;
 OB L41;
 INT L42;
 AM_CALL_ARG L43;
 OB L44;
 AM_CALL_ARG L45;
 FMAPAM1145008811 L48;
 SATTR(self,change_vars,CHANGE1489663732(((CHANGE_VARS) NULL), rout));
 create_self = ((XFORM_CODE) NULL);
 create_subst = ATTR(ATTR(self,change_vars),saved_subst);
 L3=ZALLOC(sizeof(struct XFORM_CODE_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=XFORM_CODE_tag;
 L2 = ((XFORM_CODE) L3);
 xc1 = L2;
 SATTR(xc1,subst,create_subst);
 ret_val = xc1;
 xc = ret_val;
 SATTR(xc,record_calls,FALSE);
 create_self1 = ((AM_LOOP_STMT) NULL);
 create_source = SFILE_ID_zero;
 L5=ZALLOC(sizeof(struct AM_LOOP_STMT_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AM_LOOP_STMT_tag;
 L4 = ((AM_LOOP_STMT) L5);
 r = L4;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 proxy_loop_stmt = ret_val1;
 SATTR(xc,new_loop_stmt,proxy_loop_stmt);
 SATTR(self,BEFLOOP_code,XFORM_1427763452(xc, ATTR(rout,code1)));
 if ((ATTR(xc,loop_stmt)==((AM_LOOP_STMT) NULL))) {
  SATTR(self,flat_yield,TRUE);
  after_saving_sel = ATTR(self,change_vars);
  L6 = ATTR(after_saving_sel,saved_subst);
  SYSDESTROY(L6); 
  ;
  SYSDESTROY(xc); 
  ;
  return;
 }
 SATTR(self,loop_firsts,FLISTA1446031755(((FLISTA725283029) NULL), FLISTA1151473122(ATTR(ATTR(xc,loop_stmt),firsts))));
 {
  struct FLISTA2119642552_frame_struct other1_0;
FLISTA2119642552_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(xc,loop_stmt),firsts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = ATTR(self,loop_firsts);
   L10 = ATTR(ATTR(self,change_vars),saved_subst);
   L12 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   SATTR(self,loop_firsts,FLISTA1817671564(L9, FMAPAM1614132657(L10, L12)));
  }
 }
 after_loop: ;
 SATTR(self,IINIT_code,XFORM_1427763452(xc, ATTR(ATTR(xc,loop_stmt),body)));
 iey = ATTR(xc,if_encl_yield);
 SATTR(self,flat_yield,(iey==((AM_IF_STMT) NULL)));
 if (ATTR(self,flat_yield)) {
  SATTR(self,ITERM_code,ATTR(xc,after_yield));
 }
 else {
  s1 = ATTR(self,IINIT_code);
  if (SYSOBEQ(((OB) s1),((OB) iey))) {
   SATTR(self,IINIT_code,((dAM_STMT) NULL));
  }
  else {
   while (1) {
    L13 = s1;
    if (SYSOBEQ(((OB) (*dAM_ST775224435[TAG(L13)])(L13)),((OB) iey))) {
     goto after_loop1;
    }
   }
   after_loop1: ;
   L14 = s1;
   (*dAM_ST1372701974[TAG(L14)])(L14, ((dAM_STMT) NULL));
  }
  SATTR(self,ITERM_code,ATTR(iey,next));
 }
 SATTR(self,yield_val,ATTR(xc,yield_val));
 L15 = ATTR(self,flat_yield);
 L161_=!(L15); 
 if (L161_) {
  create_self2 = ((OUT) NULL);
  ret_val2 = create_self2;
  plus_self = ret_val2;
  plus_s = ((STR) &foundc1834005908);
  stdout_self = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r1 = L17;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s);
  ret_val3 = plus_self;
  plus_self1 = ret_val3;
  plus_s1 = ATTR(ATTR(rout,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L21=ZALLOC(sizeof(struct FILE1_struct));
  if (L21==NULL) FATAL("Unable to allocate more memory");
  ((OB)L21)->header.tag=FILE1_tag;
  L20 = ((FILE1) L21);
  r2 = L20;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s1);
  ret_val5 = plus_self1;
  plus_self2 = ret_val5;
  plus_s2 = ((STR) &name52);
  stdout_self2 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r3 = L23;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s2);
  ret_val7 = plus_self2;
  plus_self3 = ret_val7;
  plus_s3 = ATTR(ATTR(ATTR(rout,sig1),srcsig),str);
  stdout_self3 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r4 = L26;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s3);
  ret_val9 = plus_self3;
  plus_self4 = ret_val9;
  plus_s4 = ((STR) &name50);
  stdout_self4 = ((FILE1) NULL);
  L30=ZALLOC(sizeof(struct FILE1_struct));
  if (L30==NULL) FATAL("Unable to allocate more memory");
  ((OB)L30)->header.tag=FILE1_tag;
  L29 = ((FILE1) L30);
  r5 = L29;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s4);
  if (ATTR(xc,true_br_yield)) {
   SATTR(self,yield_cond,ATTR(iey,test1));
   SATTR(self,WYBEF_code,ATTR(iey,if_true));
   SATTR(self,WYAFT_code,ATTR(xc,after_yield));
   SATTR(self,WNOY_code,ATTR(iey,if_false));
  }
  else {
   bool_tp = TP_BUILTIN_bool;
   create_self4 = ((SIG) NULL);
   create_ntp = ((dTP) bool_tp);
   create_self3 = ((ARRAYdTP) NULL);
   create_n = 0;
   L33 = create_n;
   L35=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L33)*sizeof(dTP);
   L34=ZALLOC_BIG(L35);
   if (L34==NULL) FATAL("Unable to allocate more memory");
   ((OB)L34)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

     ((OB)L34)->header.destroyed=0;
#endif

   L32 = ((ARRAYdTP) L34);
   L32->asize = L33;
   ret_val12 = L32;
   create_tparr = ret_val12;
   create_nname = IDENT_1837767448;
   create_nargs = ((ARRAYARG) NULL);
   create_nret = ((dTP) bool_tp);
   create_nsame = FALSE;
   L37=ZALLOC(sizeof(struct SIG_struct));
   if (L37==NULL) FATAL("Unable to allocate more memory");
   ((OB)L37)->header.tag=SIG_tag;
   L36 = ((SIG) L37);
   r6 = L36;
   SATTR(r6,tp,create_ntp);
   SATTR(r6,src_tparams,create_tparr);
   SATTR(r6,name1,create_nname);
   SATTR(r6,args,create_nargs);
   SATTR(r6,ret,create_nret);
   SATTR(r6,returns_same,create_nsame);
   ret_val13 = SIG_ma1442297600(r6);
   notsig = ret_val13;
   create_self5 = ((AM_ROU1916046290) NULL);
   create_nargs1 = 1;
   L38 = ATTR(iey,test1);
   create_source1 = (*dAM_EX2051891691[TAG(L38)])(L38);
   L40 = create_nargs1;
   L42=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L40)*sizeof(AM_CALL_ARG);
   L41=ZALLOC_BIG(L42);
   if (L41==NULL) FATAL("Unable to allocate more memory");
   ((OB)L41)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

     ((OB)L41)->header.destroyed=0;
#endif

   L39 = ((AM_ROU1916046290) L41);
   L39->asize = L40;
   r7 = L39;
   SATTR(r7,source1,create_source1);
   ret_val14 = r7;
   yc = ret_val14;
   SATTR(yc,fun,notsig);
   create_self6 = ((AM_CALL_ARG) NULL);
   create_e = ATTR(iey,test1);
   L44=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L44==NULL) FATAL("Unable to allocate more memory");
   ((OB)L44)->header.tag=AM_CALL_ARG_tag;
   L43 = ((AM_CALL_ARG) L44);
   res = L43;
   SATTR(res,expr,create_e);
   SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
   ret_val15 = res;
   L45 = ret_val15;
   SARR((AM_ROU1916046290)yc,0,(AM_CALL_ARG)L45); 
   ;
   SATTR(self,yield_cond,((dAM_EXPR) yc));
   SATTR(self,WYBEF_code,ATTR(iey,if_false));
   SATTR(self,WYAFT_code,ATTR(xc,after_yield));
   SATTR(self,WNOY_code,ATTR(iey,if_true));
  }
 }
 SYSDESTROY(proxy_loop_stmt); 
 ;
 after_saving_sel1 = ATTR(self,change_vars);
 L48 = ATTR(after_saving_sel1,saved_subst);
 SYSDESTROY(L48); 
 ;
 SYSDESTROY(xc); 
 ;
}


#undef IS_ITER
#define IS_ITER 0

void WEIGH_1152361671(WEIGH_CODE self, dAM_STMT code11) {
 INT L1;
 INT L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 INT L8;
 INT L9;
 BOOL L101_;
 dAM_STMT L11;
 INT L12;
 INT L131_;
 L1 = ATTR(self,level1);
 L21_=INTPLUS(L1,1); 
 SATTR(self,level1,L21_);
 while (1) {
  L3 = (code11==((dAM_STMT) NULL));
  L41_=!(L3); 
  if (L41_) {
  }
  else {
   goto after_loop;
  }
  WEIGH_1908424532(self, code11);
  L6 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
  L71_=!(L6); 
  if (L71_) {
   L5 = TRUE;
  } else {
   L8 = ATTR(self,max_weight);
   L9 = ATTR(self,weight);
   L101_=(L8)<(L9); 
   L5 = L101_;
  }
  if (L5) {
   return;
  }
  L11 = code11;
  code11 = (*dAM_ST775224435[TAG(L11)])(L11);
 }
 after_loop: ;
 L12 = ATTR(self,level1);
 L131_=INTMINUS(L12,1); 
 SATTR(self,level1,L131_);
}


#undef IS_ITER
#define IS_ITER 0

void WEIGH_1472719854(WEIGH_CODE self, dAM_EXPR expr11) {
 AM_ARRAY_EXPR L11;
 AM_BND1124877163 L21;
 AM_ROU1916046290 L31;
 AM_ITE1809135850 L41;
 AM_BND260301329 L51;
 AM_EXT_CALL_EXPR L61;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 BOOL L14;
 BOOL L151_;
 INT L16;
 INT L17;
 INT L181_;
 INT L19;
 INT L20;
 BOOL L221_;
 INT L231_br;
 dAM_EXPR aL231_;
 dAM_EXPR L24;
 BOOL L25;
 BOOL L26;
 BOOL L271_;
 INT L28;
 INT L29;
 BOOL L301_;
 INT L321_;
 INT L331_br;
 AM_CALL_ARG aL331_;
 AM_CALL_ARG L34;
 AM_CALL_ARG L35;
 BOOL L36;
 BOOL L37;
 BOOL L381_;
 INT L39;
 INT L40;
 BOOL L421_;
 INT L431_;
 INT L44;
 INT L45;
 INT L461_;
 INT L47;
 INT L48;
 BOOL L491_;
 BOOL L50;
 BOOL L52;
 BOOL L531_;
 INT L54;
 INT L55;
 BOOL L561_;
 BOOL L57;
 BOOL L58;
 BOOL L591_;
 INT L60;
 INT L62;
 BOOL L631_;
 INT L64;
 INT L65;
 INT L661_;
 INT L67;
 INT L68;
 INT L691_;
 INT L70;
 INT L72;
 INT L731_;
 INT L74;
 INT L75;
 BOOL L761_;
 BOOL L77;
 BOOL L78;
 BOOL L791_;
 INT L80;
 INT L82;
 BOOL L831_;
 BOOL L84;
 BOOL L85;
 BOOL L861_;
 INT L87;
 INT L88;
 BOOL L891_;
 INT L90;
 INT L921_;
 INT L93;
 INT L941_;
 BOOL L95;
 BOOL L96;
 BOOL L971_;
 INT L98;
 INT L99;
 BOOL L1001_;
 BOOL L102;
 dTP L103;
 STR L104;
 extern STR SYS1;
 extern STR inlined_C;
 INT L105;
 INT L1061_;
 INT L107;
 INT L108;
 INT L1091_;
 INT L110;
 INT L111;
 BOOL L1121_;
 INT L1131_br;
 AM_CALL_ARG aL1131_;
 AM_CALL_ARG L114;
 AM_CALL_ARG L115;
 BOOL L116;
 BOOL L117;
 BOOL L1181_;
 INT L119;
 INT L120;
 BOOL L1221_;
 INT L1231_;
 INT L124;
 INT L125;
 INT L1261_;
 INT L127;
 INT L128;
 BOOL L1291_;
 INT L1301_br;
 AM_CALL_ARG aL1301_;
 AM_CALL_ARG L132;
 AM_CALL_ARG L133;
 BOOL L134;
 BOOL L135;
 BOOL L1361_;
 INT L137;
 INT L138;
 BOOL L1391_;
 INT L1401_;
 INT L141;
 INT L1421_;
 INT L143;
 INT L1441_;
 INT L145;
 INT L146;
 INT L1471_;
 INT L148;
 INT L149;
 BOOL L1501_;
 INT L1521_br;
 AM_CALL_ARG aL1521_;
 AM_CALL_ARG L153;
 AM_CALL_ARG L154;
 BOOL L155;
 BOOL L156;
 BOOL L1571_;
 INT L158;
 INT L159;
 BOOL L1601_;
 INT L1611_;
 INT L162;
 INT L1631_;
 INT L164;
 INT L165;
 INT L1661_;
 INT L167;
 INT L168;
 BOOL L1691_;
 INT L1701_br;
 AM_CALL_ARG aL1701_;
 AM_CALL_ARG L171;
 AM_CALL_ARG L172;
 BOOL L173;
 BOOL L174;
 BOOL L1751_;
 INT L176;
 INT L177;
 BOOL L1781_;
 INT L1791_;
 BOOL L180;
 BOOL L182;
 BOOL L1831_;
 INT L184;
 INT L185;
 BOOL L1861_;
 L14 = (expr11==((dAM_EXPR) NULL));
 L151_=!(L14); 
 if (L151_) {
  switch (TAG(expr11)) {
   case AM_LOCAL_EXPR_tag: break;
   case AM_ARRAY_EXPR_tag:
    L16 = ATTR(self,weight);
    L17 = WEIGH_803917570;
    L181_=INTPLUS(L16,L17); 
    SATTR(self,weight,L181_);
    L19 = ATTR(self,max_weight);
    L20 = ATTR(self,weight);
    L221_=(L19)<(L20); 
    if (L221_) {
     return;
    }
    {
     BOOL f_L231_ = TRUE;
     /* loop index variable */
     L71 = 0;
     L11 = ((AM_ARRAY_EXPR) expr11);
     L231_br=L11==NULL?0:ASIZE((AM_ARRAY_EXPR)L11); 
     while (1) {
      if(L71>=L231_br)  goto after_loop; 
      aL231_=ARR((AM_ARRAY_EXPR)L11,L71); 
      WEIGH_1472719854(self, aL231_);
      L26 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L271_=!(L26); 
      if (L271_) {
       L25 = TRUE;
      } else {
       L28 = ATTR(self,max_weight);
       L29 = ATTR(self,weight);
       L301_=(L28)<(L29); 
       L25 = L301_;
      }
      if (L25) {
       return;
      }
      L321_=INTPLUS(L71,1); 
      L71 = L321_;
     }
    }
    after_loop: ; break;
   case AM_BND1124877163_tag:
    {
     BOOL f_L331_ = TRUE;
     /* loop index variable */
     L81 = 0;
     L21 = ((AM_BND1124877163) expr11);
     L331_br=L21==NULL?0:ASIZE((AM_BND1124877163)L21); 
     while (1) {
      if(L81>=L331_br)  goto after_loop1; 
      aL331_=ARR((AM_BND1124877163)L21,L81); 
      L35=aL331_;
      WEIGH_1472719854(self, ATTR(L35,expr));
      L37 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L381_=!(L37); 
      if (L381_) {
       L36 = TRUE;
      } else {
       L39 = ATTR(self,max_weight);
       L40 = ATTR(self,weight);
       L421_=(L39)<(L40); 
       L36 = L421_;
      }
      if (L36) {
       return;
      }
      L431_=INTPLUS(L81,1); 
      L81 = L431_;
     }
    }
    after_loop1: ; break;
   case AM_IF_EXPR_tag:
    L44 = ATTR(self,weight);
    L45 = WEIGH_1480681287;
    L461_=INTPLUS(L44,L45); 
    SATTR(self,weight,L461_);
    L47 = ATTR(self,max_weight);
    L48 = ATTR(self,weight);
    L491_=(L47)<(L48); 
    if (L491_) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_IF_EXPR) expr11),test1));
    L52 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L531_=!(L52); 
    if (L531_) {
     L50 = TRUE;
    } else {
     L54 = ATTR(self,max_weight);
     L55 = ATTR(self,weight);
     L561_=(L54)<(L55); 
     L50 = L561_;
    }
    if (L50) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_IF_EXPR) expr11),if_true));
    L58 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L591_=!(L58); 
    if (L591_) {
     L57 = TRUE;
    } else {
     L60 = ATTR(self,max_weight);
     L62 = ATTR(self,weight);
     L631_=(L60)<(L62); 
     L57 = L631_;
    }
    if (L57) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_IF_EXPR) expr11),if_false)); break;
   case AM_NEW_EXPR_tag:
    L64 = ATTR(self,weight);
    L65 = WEIGH_803917570;
    L661_=INTPLUS(L64,L65); 
    SATTR(self,weight,L661_); break;
   case AM_IS_VOID_EXPR_tag:
    L67 = ATTR(self,weight);
    L68 = WEIGH_1480681287;
    L691_=INTPLUS(L67,L68); 
    SATTR(self,weight,L691_); break;
   case AM_SHARED_EXPR_tag: break;
   case AM_ATTR_EXPR_tag:
    WEIGH_1472719854(self, ATTR(((AM_ATTR_EXPR) expr11),ob)); break;
   case AM_VAT319982528_tag:
    L70 = ATTR(self,weight);
    L72 = WEIGH_879105801;
    L731_=INTPLUS(L70,L72); 
    SATTR(self,weight,L731_);
    L74 = ATTR(self,max_weight);
    L75 = ATTR(self,weight);
    L761_=(L74)<(L75); 
    if (L761_) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_VAT319982528) expr11),ob));
    L78 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L791_=!(L78); 
    if (L791_) {
     L77 = TRUE;
    } else {
     L80 = ATTR(self,max_weight);
     L82 = ATTR(self,weight);
     L831_=(L80)<(L82); 
     L77 = L831_;
    }
    if (L77) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_VAT319982528) expr11),val1)); break;
   case AM_EXCEPT_EXPR_tag: break;
   case AM_STMT_EXPR_tag:
    WEIGH_1472719854(self, ATTR(((AM_STMT_EXPR) expr11),expr));
    L85 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L861_=!(L85); 
    if (L861_) {
     L84 = TRUE;
    } else {
     L87 = ATTR(self,max_weight);
     L88 = ATTR(self,weight);
     L891_=(L87)<(L88); 
     L84 = L891_;
    }
    if (L84) {
     return;
    }
    L90 = ATTR(self,level1);
    L921_=INTMINUS(L90,1); 
    SATTR(self,level1,L921_);
    WEIGH_1152361671(self, ATTR(((AM_STMT_EXPR) expr11),stmts));
    L93 = ATTR(self,level1);
    L941_=INTPLUS(L93,1); 
    SATTR(self,level1,L941_);
    L96 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L971_=!(L96); 
    if (L971_) {
     L95 = TRUE;
    } else {
     L98 = ATTR(self,max_weight);
     L99 = ATTR(self,weight);
     L1001_=(L98)<(L99); 
     L95 = L1001_;
    }
    if (L95) {
     return;
    } break;
   case AM_ROU1916046290_tag:
    L103 = ATTR(ATTR(((AM_ROU1916046290) expr11),fun),tp);
    L104 = (*dTP_strrSTR[TAG(L103)])(L103);
    if (STR_is111530248(L104, ((STR) &SYS1))) {
     L102 = STR_is111530248(ATTR(ATTR(((AM_ROU1916046290) expr11),fun),name1).str, ((STR) &inlined_C));
    } else {
     L102 = FALSE;
    }
    if (L102) {
     L105 = ATTR(self,max_weight);
     L1061_=INTPLUS(L105,1); 
     SATTR(self,weight,L1061_);
     return;
    }
    L107 = ATTR(self,weight);
    L108 = WEIGH_803917570;
    L1091_=INTPLUS(L107,L108); 
    SATTR(self,weight,L1091_);
    L110 = ATTR(self,max_weight);
    L111 = ATTR(self,weight);
    L1121_=(L110)<(L111); 
    if (L1121_) {
     return;
    }
    {
     BOOL f_L1131_ = TRUE;
     /* loop index variable */
     L91 = 0;
     L31 = ((AM_ROU1916046290) expr11);
     L1131_br=L31==NULL?0:ASIZE((AM_ROU1916046290)L31); 
     while (1) {
      if(L91>=L1131_br)  goto after_loop2; 
      aL1131_=ARR((AM_ROU1916046290)L31,L91); 
      L115=aL1131_;
      WEIGH_1472719854(self, ATTR(L115,expr));
      L117 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L1181_=!(L117); 
      if (L1181_) {
       L116 = TRUE;
      } else {
       L119 = ATTR(self,max_weight);
       L120 = ATTR(self,weight);
       L1221_=(L119)<(L120); 
       L116 = L1221_;
      }
      if (L116) {
       return;
      }
      L1231_=INTPLUS(L91,1); 
      L91 = L1231_;
     }
    }
    after_loop2: ; break;
   case AM_ITE1809135850_tag:
    L124 = ATTR(self,weight);
    L125 = WEIGH_743880746;
    L1261_=INTPLUS(L124,L125); 
    SATTR(self,weight,L1261_);
    L127 = ATTR(self,max_weight);
    L128 = ATTR(self,weight);
    L1291_=(L127)<(L128); 
    if (L1291_) {
     return;
    }
    {
     BOOL f_L1301_ = TRUE;
     /* loop index variable */
     L101 = 0;
     L41 = ((AM_ITE1809135850) expr11);
     L1301_br=L41==NULL?0:ASIZE((AM_ITE1809135850)L41); 
     while (1) {
      if(L101>=L1301_br)  goto after_loop3; 
      aL1301_=ARR((AM_ITE1809135850)L41,L101); 
      L133=aL1301_;
      WEIGH_1472719854(self, ATTR(L133,expr));
      L135 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L1361_=!(L135); 
      if (L1361_) {
       L134 = TRUE;
      } else {
       L137 = ATTR(self,max_weight);
       L138 = ATTR(self,weight);
       L1391_=(L137)<(L138); 
       L134 = L1391_;
      }
      if (L134) {
       return;
      }
      L1401_=INTPLUS(L101,1); 
      L101 = L1401_;
     }
    }
    after_loop3: ;
    L141 = ATTR(self,level1);
    L1421_=INTMINUS(L141,1); 
    SATTR(self,level1,L1421_);
    WEIGH_1152361671(self, ATTR(((AM_ITE1809135850) expr11),init));
    L143 = ATTR(self,level1);
    L1441_=INTPLUS(L143,1); 
    SATTR(self,level1,L1441_); break;
   case AM_BND260301329_tag:
    L145 = ATTR(self,weight);
    L146 = WEIGH_803917570;
    L1471_=INTPLUS(L145,L146); 
    SATTR(self,weight,L1471_);
    L148 = ATTR(self,max_weight);
    L149 = ATTR(self,weight);
    L1501_=(L148)<(L149); 
    if (L1501_) {
     return;
    }
    {
     BOOL f_L1521_ = TRUE;
     /* loop index variable */
     L121 = 0;
     L51 = ((AM_BND260301329) expr11);
     L1521_br=L51==NULL?0:ASIZE((AM_BND260301329)L51); 
     while (1) {
      if(L121>=L1521_br)  goto after_loop4; 
      aL1521_=ARR((AM_BND260301329)L51,L121); 
      L154=aL1521_;
      WEIGH_1472719854(self, ATTR(L154,expr));
      L156 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L1571_=!(L156); 
      if (L1571_) {
       L155 = TRUE;
      } else {
       L158 = ATTR(self,max_weight);
       L159 = ATTR(self,weight);
       L1601_=(L158)<(L159); 
       L155 = L1601_;
      }
      if (L155) {
       return;
      }
      L1611_=INTPLUS(L121,1); 
      L121 = L1611_;
     }
    }
    after_loop4: ;
    WEIGH_1472719854(self, ATTR(((AM_BND260301329) expr11),br)); break;
   case AM_BND367211769_tag:
    L162 = ATTR(self,max_weight);
    L1631_=INTPLUS(L162,1); 
    SATTR(self,weight,L1631_);
    return; break;
   case AM_EXT_CALL_EXPR_tag:
    L164 = ATTR(self,weight);
    L165 = WEIGH_803917570;
    L1661_=INTPLUS(L164,L165); 
    SATTR(self,weight,L1661_);
    L167 = ATTR(self,max_weight);
    L168 = ATTR(self,weight);
    L1691_=(L167)<(L168); 
    if (L1691_) {
     return;
    }
    {
     BOOL f_L1701_ = TRUE;
     /* loop index variable */
     L131 = 0;
     L61 = ((AM_EXT_CALL_EXPR) expr11);
     L1701_br=L61==NULL?0:ASIZE((AM_EXT_CALL_EXPR)L61); 
     while (1) {
      if(L131>=L1701_br)  goto after_loop5; 
      aL1701_=ARR((AM_EXT_CALL_EXPR)L61,L131); 
      L172=aL1701_;
      WEIGH_1472719854(self, ATTR(L172,expr));
      L174 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
      L1751_=!(L174); 
      if (L1751_) {
       L173 = TRUE;
      } else {
       L176 = ATTR(self,max_weight);
       L177 = ATTR(self,weight);
       L1781_=(L176)<(L177); 
       L173 = L1781_;
      }
      if (L173) {
       return;
      }
      L1791_=INTPLUS(L131,1); 
      L131 = L1791_;
     }
    }
    after_loop5: ; break;
   case AM_GLOBAL_EXPR_tag: break;
   case AM_HERE_EXPR_tag: break;
   case AM_ANY_EXPR_tag: break;
   case AM_CLUSTER_EXPR_tag: break;
   case AM_CLU1286269335_tag: break;
   case AM_WHERE_EXPR_tag: break;
   case AM_NEAR_EXPR_tag: break;
   case AM_FAR_EXPR_tag: break;
   case AM_AT_EXPR_tag:
    WEIGH_1472719854(self, ATTR(((AM_AT_EXPR) expr11),at));
    L182 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
    L1831_=!(L182); 
    if (L1831_) {
     L180 = TRUE;
    } else {
     L184 = ATTR(self,max_weight);
     L185 = ATTR(self,weight);
     L1861_=(L184)<(L185); 
     L180 = L1861_;
    }
    if (L180) {
     return;
    }
    WEIGH_1472719854(self, ATTR(((AM_AT_EXPR) expr11),e)); break;
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
   default: ;
    FATAL("No applicable type in typecase\nin WEIGH_CODE::calc_expr_weight($AM_EXPR)\n./Middle/inline.sa:493:20");
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void WEIGH_1908424532(WEIGH_CODE self, dAM_STMT stmt1) {
 AM_IF_STMT old_eif;
 FLISTdAM_STMT L11;
 FLISTdAM_STMT L21;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L121_;
 INT L13;
 INT L14;
 BOOL L151_;
 INT L16;
 INT L17;
 INT L181_;
 INT L19;
 INT L20;
 INT L221_;
 INT L23;
 INT L24;
 BOOL L251_;
 INT L26;
 INT L27;
 INT L281_;
 INT L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L33;
 BOOL L341_;
 INT L35;
 INT L36;
 BOOL L371_;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 INT L41;
 INT L42;
 BOOL L431_;
 BOOL L44;
 BOOL L45;
 BOOL L461_;
 INT L47;
 INT L48;
 BOOL L491_;
 BOOL L50;
 BOOL L52;
 BOOL L531_;
 INT L54;
 INT L55;
 BOOL L561_;
 INT L57;
 INT L58;
 INT L591_;
 INT L60;
 INT L61;
 BOOL L621_;
 dAM_STMT L63;
 BOOL L64;
 BOOL L65;
 BOOL L661_;
 INT L67;
 INT L68;
 BOOL L691_;
 dAM_STMT L70;
 BOOL L71;
 BOOL L72;
 BOOL L731_;
 INT L74;
 INT L75;
 BOOL L761_;
 INT L77;
 INT L78;
 INT L791_;
 INT L80;
 INT L82;
 BOOL L831_;
 BOOL L84;
 BOOL L85;
 BOOL L861_;
 INT L87;
 INT L88;
 BOOL L891_;
 if (stmt1==NULL) {
  goto other453;
 } else
 switch (TAG(stmt1)) {
  case AM_ASSIGN_STMT_tag:
   L3 = ATTR(self,weight);
   L4 = WEIGH_879105801;
   L51_=INTPLUS(L3,L4); 
   SATTR(self,weight,L51_);
   L6 = ATTR(self,max_weight);
   L7 = ATTR(self,weight);
   L81_=(L6)<(L7); 
   if (L81_) {
    return;
   }
   WEIGH_1472719854(self, ATTR(((AM_ASSIGN_STMT) stmt1),dest));
   L10 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L121_=!(L10); 
   if (L121_) {
    L9 = TRUE;
   } else {
    L13 = ATTR(self,max_weight);
    L14 = ATTR(self,weight);
    L151_=(L13)<(L14); 
    L9 = L151_;
   }
   if (L9) {
    return;
   }
   WEIGH_1472719854(self, ATTR(((AM_ASSIGN_STMT) stmt1),src1)); break;
  case AM_PRE_STMT_tag:
   WEIGH_1472719854(self, ATTR(((AM_PRE_STMT) stmt1),test1)); break;
  case AM_POST_STMT_tag:
   WEIGH_1472719854(self, ATTR(((AM_POST_STMT) stmt1),test1)); break;
  case AM_INITIAL_STMT_tag:
   WEIGH_1152361671(self, ATTR(((AM_INITIAL_STMT) stmt1),stmts)); break;
  case AM_ASSERT_STMT_tag:
   WEIGH_1472719854(self, ATTR(((AM_ASSERT_STMT) stmt1),test1)); break;
  case AM_INV1167837230_tag:
   L16 = ATTR(self,weight);
   L17 = WEIGH_803917570;
   L181_=INTPLUS(L16,L17); 
   L19 = L181_;
   L20 = WEIGH_1480681287;
   L221_=INTPLUS(L19,L20); 
   SATTR(self,weight,L221_);
   L23 = ATTR(self,max_weight);
   L24 = ATTR(self,weight);
   L251_=(L23)<(L24); 
   if (L251_) {
    return;
   } break;
  case AM_EXPR_STMT_tag:
   WEIGH_1472719854(self, ATTR(((AM_EXPR_STMT) stmt1),expr)); break;
  case AM_BREAK_STMT_tag: break;
  case AM_IF_STMT_tag:
   L26 = ATTR(self,weight);
   L27 = WEIGH_1480681287;
   L281_=INTPLUS(L26,L27); 
   SATTR(self,weight,L281_);
   L29 = ATTR(self,max_weight);
   L30 = ATTR(self,weight);
   L311_=(L29)<(L30); 
   if (L311_) {
    return;
   }
   WEIGH_1472719854(self, ATTR(((AM_IF_STMT) stmt1),test1));
   L33 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L341_=!(L33); 
   if (L341_) {
    L32 = TRUE;
   } else {
    L35 = ATTR(self,max_weight);
    L36 = ATTR(self,weight);
    L371_=(L35)<(L36); 
    L32 = L371_;
   }
   if (L32) {
    return;
   }
   old_eif = ATTR(self,enclosing_if);
   SATTR(self,enclosing_if,((AM_IF_STMT) stmt1));
   SATTR(self,true_branch,TRUE);
   WEIGH_1152361671(self, ATTR(((AM_IF_STMT) stmt1),if_true));
   L39 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L401_=!(L39); 
   if (L401_) {
    L38 = TRUE;
   } else {
    L41 = ATTR(self,max_weight);
    L42 = ATTR(self,weight);
    L431_=(L41)<(L42); 
    L38 = L431_;
   }
   if (L38) {
    return;
   }
   SATTR(self,true_branch,FALSE);
   WEIGH_1152361671(self, ATTR(((AM_IF_STMT) stmt1),if_false));
   L45 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L461_=!(L45); 
   if (L461_) {
    L44 = TRUE;
   } else {
    L47 = ATTR(self,max_weight);
    L48 = ATTR(self,weight);
    L491_=(L47)<(L48); 
    L44 = L491_;
   }
   if (L44) {
    return;
   }
   SATTR(self,enclosing_if,old_eif); break;
  case AM_CASE_STMT_tag:
   WEIGH_1472719854(self, ATTR(((AM_CASE_STMT) stmt1),test1));
   L52 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L531_=!(L52); 
   if (L531_) {
    L50 = TRUE;
   } else {
    L54 = ATTR(self,max_weight);
    L55 = ATTR(self,weight);
    L561_=(L54)<(L55); 
    L50 = L561_;
   }
   if (L50) {
    return;
   }
   {
    struct FLISTd1374595573_frame_struct other1_0;
FLISTd1374595573_frame noname1 = &other1_0;
    noname1->state = 0;
    while (1) {
     L57 = ATTR(self,weight);
     L58 = WEIGH_1480681287;
     L591_=INTPLUS(L57,L58); 
     SATTR(self,weight,L591_);
     L60 = ATTR(self,max_weight);
     L61 = ATTR(self,weight);
     L621_=(L60)<(L61); 
     if (L621_) {
      return;
     }
     if (noname1->state == 0) {
      L11 = ATTR(((AM_CASE_STMT) stmt1),stmts);
      noname1->self = L11;
     }
     L63 = FLISTd1374595573(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     WEIGH_1152361671(self, L63);
     L65 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
     L661_=!(L65); 
     if (L661_) {
      L64 = TRUE;
     } else {
      L67 = ATTR(self,max_weight);
      L68 = ATTR(self,weight);
      L691_=(L67)<(L68); 
      L64 = L691_;
     }
     if (L64) {
      return;
     }
    }
   }
   after_loop: ;
   WEIGH_1152361671(self, ATTR(((AM_CASE_STMT) stmt1),else_stmts)); break;
  case AM_TYPECASE_STMT_tag:
   {
    struct FLISTd1374595573_frame_struct other2_0;
FLISTd1374595573_frame noname2 = &other2_0;
    L21 = ATTR(((AM_TYPECASE_STMT) stmt1),stmts);
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L70 = FLISTd1374595573(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     WEIGH_1152361671(self, L70);
     L72 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
     L731_=!(L72); 
     if (L731_) {
      L71 = TRUE;
     } else {
      L74 = ATTR(self,max_weight);
      L75 = ATTR(self,weight);
      L761_=(L74)<(L75); 
      L71 = L761_;
     }
     if (L71) {
      return;
     }
    }
   }
   after_loop1: ;
   WEIGH_1152361671(self, ATTR(((AM_TYPECASE_STMT) stmt1),else_stmts)); break;
  case AM_WAITFOR_STMT_tag: break;
  case AM_PREFETCH_STMT_tag:
   L77 = ATTR(self,weight);
   L78 = WEIGH_879105801;
   L791_=INTPLUS(L77,L78); 
   SATTR(self,weight,L791_);
   L80 = ATTR(self,max_weight);
   L82 = ATTR(self,weight);
   L831_=(L80)<(L82); 
   if (L831_) {
    return;
   }
   WEIGH_1472719854(self, ATTR(((AM_PREFETCH_STMT) stmt1),dest));
   L85 = (ATTR(self,badstmt)==((dAM_STMT) NULL));
   L861_=!(L85); 
   if (L861_) {
    L84 = TRUE;
   } else {
    L87 = ATTR(self,max_weight);
    L88 = ATTR(self,weight);
    L891_=(L87)<(L88); 
    L84 = L891_;
   }
   if (L84) {
    return;
   }
   WEIGH_1472719854(self, ATTR(((AM_PREFETCH_STMT) stmt1),src1)); break;
  default: ;
  other453: ;
   SATTR(self,badstmt,stmt1);
 }
}


#undef IS_ITER
#define IS_ITER 1

INT INT_do676118316(INT_do676118316_frame frame) {
 INT dummy = INT_zero;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_I=frame->self+1;frame->I_u_Im=frame->arg1; 
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->I_u_I--<=frame->I_u_Im)  goto after_loop; 
   return frame->I_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT INT_fo686245905(INT_fo686245905_frame frame) {
 INT dummy = INT_zero;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_I=frame->self-1;frame->I_u_Im=frame->arg1; 
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if((frame->I_u_Im--)<=0)  goto after_loop;frame->I_u_I++; 
   return frame->I_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT INT_timesbrINT(INT_timesbrINT_frame frame) {
 INT dummy = INT_zero;
 INT aI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->self)  goto after_loop; 
   aI_u_I=frame->i_I_u_I; frame->i_I_u_I++;
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

INT INT_up418511718(INT_up418511718_frame frame) {
 INT dummy = INT_zero;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_I=frame->self-1;frame->I_u_Im=frame->arg1; 
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->I_u_I++>=frame->I_u_Im)  goto after_loop; 
   return frame->I_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT INT_upbrINT(INT_upbrINT_frame frame) {
 INT dummy = INT_zero;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_I=frame->self-1; 
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   frame->I_u_I++; 
   return frame->I_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void INT_timesb(INT_timesb_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->self)  goto after_loop; 
   frame->i_I_u_I++;
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
