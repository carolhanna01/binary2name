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

typedef struct dINLINE_ITER_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ITER_SIG;

typedef struct dINLINE_ROUT_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ROUT_SIG;

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

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

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

typedef struct IMPL_CREATE_struct {/* layout for IMPL_CREATE */
 OB_HEADER header;
 struct AS_CLASS_DEF_struct *as;
 struct ELT_TBL_struct *class_elts;
 struct FLISTI1895665739_struct *incs;
 struct TP_CLASS_struct *tp;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 } *IMPL_CREATE;

typedef struct IMPL_INCLUDE_struct {/* layout for IMPL_INCLUDE */
 OB_HEADER header;
 struct AS_INC1416917001_struct *as;
 struct ELT_TBL_struct *elt_tbl;
 struct FSETAS_FEAT_MOD_struct *used_mods;
 struct IMPL_struct *impl;
 struct TP_CLASS_struct *itp;
 struct TP_CLASS_struct *tp;
 struct TP_CONTEXT_struct *con;
 } *IMPL_INCLUDE;

typedef struct INLINE_ATTR_READ_struct {/* layout for INLINE_ATTR_READ */
 OB_HEADER header;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE_ATTR_READ;

typedef struct INLINE1596311811_struct {/* layout for INLINE_ATTR_WRITE */
 OB_HEADER header;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct IDENT_struct at;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE1596311811;

typedef struct INLINE1433668901_struct {/* layout for INLINE_GLOBAL_READ */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SIG_struct *sig1;
 BOOL is_const;
 BOOL is_special;
 } *INLINE1433668901;

typedef struct INLINE1455768374_struct {/* layout for INLINE_GLOBAL_WRITE */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SIG_struct *sig1;
 BOOL is_const;
 BOOL is_special;
 } *INLINE1455768374;

typedef struct INLINE_INT_FOLD_struct {/* layout for INLINE_INT_FOLD */
 OB_HEADER header;
 struct SIG_struct *sig1;
 BOOL is_special;
 } *INLINE_INT_FOLD;

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

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

typedef struct SIG_TBL_struct {/* layout for SIG_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *SIG_TBL;

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

typedef struct TUPAM_1519040877_struct {/* layout for TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct AM_LOCAL_EXPR_struct *t2;
 } TUPAM_1519040877;
static TUPAM_1519040877 TUPAM_1519040877_zero;

typedef struct TUPAM_1519040877_boxed_struct {
 OB_HEADER header;
 TUPAM_1519040877 immutable_part;
 } *TUPAM_1519040877_boxed;

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

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYIDENT_struct {/* layout for ARRAY{IDENT} */
 OB_HEADER header;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *ARRAYIDENT;

typedef struct ARRAYINT_struct {/* layout for ARRAY{INT} */
 OB_HEADER header;
 INT asize;
 INT arr_part[1];
 } *ARRAYINT;

typedef struct ARRAYI486979141_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI486979141_frame;

typedef struct AS_ATTR_DEF_struct {/* layout for AS_ATTR_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_ATTR_DEF;

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

typedef struct AS_FEAT_MOD_struct {/* layout for AS_FEAT_MOD */
 OB_HEADER header;
 struct AS_FEAT_MOD_struct *next;
 struct IDENT_struct name1;
 struct IDENT_struct new_name;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_FEAT_MOD;

typedef struct AS_INC1416917001_struct {/* layout for AS_INCLUDE_CLAUSE */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_FEAT_MOD_struct *mods;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_INC1416917001;

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

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTA1446310868_frame_struct {
 INT state;
 FLISTA1409846210 self;/* Formal argument: self */
 AM_BND367211769 ret_val1;
 INT i;
 INT sz;
 FLISTA1409846210 aget_self;
 INT aget_ind;
 AM_BND367211769 ret_val;
 } *FLISTA1446310868_frame;

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

typedef struct FLISTI1895665739_struct {/* layout for FLIST{IMPL_INCLUDE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IMPL_INCLUDE_struct *arr_part[1];
 } *FLISTI1895665739;

typedef struct FLISTI381429157_frame_struct {
 INT state;
 FLISTI1895665739 self;/* Formal argument: self */
 IMPL_INCLUDE ret_val1;
 INT i;
 INT sz;
 FLISTI1895665739 aget_self;
 INT aget_ind;
 IMPL_INCLUDE ret_val;
 } *FLISTI381429157_frame;

typedef struct FMAPAM1145008811_struct {/* layout for FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FMAPAM1145008811;

typedef struct FSETAS_FEAT_MOD_struct {/* layout for FSET{AS_FEAT_MOD} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FSETAS_FEAT_MOD;

typedef struct INLINE_struct {/* layout for INLINE */
 OB_HEADER header;
 struct INLINE_ITER_TBL_struct *itbl;
 struct INLINE_ROUT_TBL_struct *rtbl;
 INT iter_thres;
 INT rout_thres;
 struct PROG_struct *prog;
 BOOL inline_iters;
 BOOL inline_routs;
 } *INLINE;

typedef struct INLINE_ITER_TBL_struct {/* layout for INLINE_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ITER_SIG_struct *arr_part[1];
 } *INLINE_ITER_TBL;

typedef struct INLINE_ROUT_TBL_struct {/* layout for INLINE_ROUT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ROUT_SIG_struct *arr_part[1];
 } *INLINE_ROUT_TBL;

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

extern IDENT IDENT_1485568624;
extern IDENT IDENT_1666445894;
extern IDENT IDENT_2085547498;
extern INT INLINE1173790597;
extern INT INLINE1181209094;
extern INT INLINE1258550073;
extern INT INLINE1306635001;
extern INT INLINE520918393;
extern INT INLINE545315961;
extern INT INLINE957944682;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_ext_c_tp;
extern INT TP_KIND_part_tp;
extern INT TP_KIND_ref_tp;
extern INT TP_KIND_spr_tp;
extern INT TP_KIND_val_tp;
extern TP_CLASS TP_BUILTIN_int;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dAS_CL1087157762)(dAS_CLASS_ELT);

extern RETURNED_CONST BOOL (**dINLIN1181425367)(dINLINE_ROUT_SIG);

extern RETURNED_CONST BOOL (**dINLIN2120180991)(dINLINE_ITER_SIG);

extern RETURNED_CONST dAM_EXPR (**dINLIN104160276)(dINLINE_ITER_SIG, AM_ITE1809135850);

extern RETURNED_CONST dAM_EXPR (**dINLIN516975284)(dINLINE_ROUT_SIG, AM_ROU1916046290);

extern RETURNED_CONST dAS_CLASS_ELT (**dAS_CL760354380)(dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);
AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AS_FEAT_MOD IMPL_I36886340(IMPL_INCLUDE, IDENT);
BOOL ELT_is1300999700(ELT);
BOOL ELT_is1808162451(ELT);
BOOL ELT_is1907475257(ELT);
BOOL ELT_is507942138(ELT);
BOOL FSETAS456491640(FSETAS_FEAT_MOD, AS_FEAT_MOD);
BOOL IMPL_I1019512980(IMPL_INCLUDE);
BOOL IMPL_I1756162892(IMPL_INCLUDE);
BOOL INLINE1676073295(INLINE_ATTR_READ);
BOOL INLINE182457744(INLINE1596311811);
BOOL INLINE600822962(INLINE1433668901);
BOOL INLINE76438420(INLINE, SIG);
BOOL INLINE862622371(INLINE1455768374);
BOOL SIG_is1444519892(SIG);
BOOL SIG_is152200802(SIG);
BOOL SIG_is1739708917(SIG);
BOOL SIG_is418491101(SIG);
BOOL SIG_is683883765(SIG);
BOOL TP_CLA1568240525(TP_CLASS);
BOOL TP_CLA727826289(TP_CLASS);
BOOL TP_CON1882075561(TP_CONTEXT, AS_TYPE_SPEC);
CHANGE_VARS CHANGE1489663732(CHANGE_VARS, AM_ROUT_DEF);
ELT ELT_TB1201943681(ELT_TBL, ELT);
ELT ELT_TB431570775(ELT_TB431570775_frame);
ELT ELT_TB954957059(ELT_TBL, ELT);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
ELT IMPL_C144337270(IMPL_CREATE, dAS_CLASS_ELT);
ELT IMPL_C872226022(IMPL_CREATE, dAS_CLASS_ELT);
ELT IMPL_I954938038(IMPL_INCLUDE, ELT);
ELT_TBL ELT_TB1573869061(ELT_TBL, ELT);
ELT_TBL ELT_TB1703146017(ELT_TBL, ELT);
ELT_TBL IMPL_C1114329143(IMPL_CREATE);
ELT_TBL IMPL_C1426629505(IMPL_CREATE);
ELT_TBL IMPL_I1774855049(IMPL_INCLUDE);
FLISTA1062334999 FLISTA572736043(FLISTA1062334999, FLISTA1062334999);
FLISTA1409846210 FLISTA1236996208(FLISTA1409846210, FLISTA1409846210);
FLISTA725283029 FLISTA1446031755(FLISTA725283029, INT);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA811588375(FLISTA725283029, FLISTA725283029);
FLISTI1895665739 FLISTI1378884285(FLISTI1895665739, IMPL_INCLUDE);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FMAPAM1145008811 FMAPAM714369621(FMAPAM1145008811, INT);
FMAPAM1145008811 FMAPAM876857188(FMAPAM1145008811, AM_LOCAL_EXPR, AM_LOCAL_EXPR);
FSETAS_FEAT_MOD FSETAS559779810(FSETAS_FEAT_MOD, AS_FEAT_MOD);
IDENT IDENT_1150413730(IDENT, STR);
IFC ELT_TB462529002(ELT_TBL);
IMPL IMPL_C12545525(IMPL_CREATE, TP_CLASS, TP_CONTEXT);
IMPL IMPL_C629829570(IMPL_CREATE, TP_CLASS);
IMPL_INCLUDE FLISTI381429157(FLISTI381429157_frame);
IMPL_INCLUDE FLISTI961811375(FLISTI1895665739);
IMPL_INCLUDE IMPL_I1190749135(IMPL_INCLUDE, TP_CLASS, AS_INC1416917001, TP_CONTEXT);
INLINE1433668901 INLINE433490271(INLINE1433668901, AM_ROUT_DEF);
INLINE1455768374 INLINE1362941708(INLINE1455768374, AM_ROUT_DEF);
INLINE_ITER_TBL INLINE263789299(INLINE_ITER_TBL, AM_ROUT_DEF, BOOL, INT, PROG);
INLINE_ROUT_TBL INLINE180230301(INLINE_ROUT_TBL, AM_ROUT_DEF, BOOL, INT, PROG);
INLINE_ROUT_TBL INLINE525216690(INLINE_ROUT_TBL, dINLINE_ROUT_SIG);
INT AS_FEA1447190869(AS_FEAT_MOD);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTA23038851(FLISTA1062334999, AM_ITE1809135850);
INT FLISTI377878565(FLISTI1895665739);
INT FLISTd1086621942(FLISTdAM_EXPR, dAM_EXPR);
INT FSETAS77357633(FSETAS_FEAT_MOD);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
SIG INLINE1060094617(INLINE_ATTR_READ);
SIG INLINE1204485066(INLINE1433668901);
SIG INLINE1283359029(INLINE1455768374);
SIG INLINE1360700008(INLINE1596311811);
SIG SIG_at1749337727(SIG, AS_ATTR_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_co2044018076(SIG, AS_CONST_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_ma1442297600(SIG);
SIG SIG_ro1276623596(SIG, AS_ROUT_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_sh1677498347(SIG, AS_SHARED_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_CON702863562(TP_CONTEXT);
TP_CLASS IMPL_C639524789(IMPL_CREATE);
TP_CLASS IMPL_I517033114(IMPL_INCLUDE);
TP_CONTEXT TP_CLA821242177(TP_CLASS);
dAM_EXPR INLINE1060127472(INLINE, AM_ROUT_DEF, AM_LOOP_STMT, AM_ITE1809135850);
dAM_EXPR INLINE1426016646(INLINE, AM_ITE1809135850);
dAM_EXPR INLINE1475043475(INLINE1433668901, AM_ROU1916046290);
dAM_EXPR INLINE1797911459(INLINE, AM_ROUT_DEF, AM_ROU1916046290);
dAM_EXPR INLINE2023716438(INLINE1455768374, AM_ROU1916046290);
dAM_EXPR INLINE46712374(INLINE_ATTR_READ, AM_ROU1916046290);
dAM_EXPR INLINE472404814(INLINE, AM_ROU1916046290);
dAM_EXPR INLINE655229257(INLINE1596311811, AM_ROU1916046290);
dINLINE_ITER_SIG INLINE1838056742(INLINE_ITER_TBL, SIG);
dINLINE_ROUT_SIG INLINE2019550598(INLINE_ROUT_TBL, SIG);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON2076339407(TP_CONTEXT, AS_TYPE_SPEC, BOOL);
void AM_OUT1439666796(AM_OUT, dAM);
void ARRAYI486979141(ARRAYI486979141_frame);
void CHANGE1122717432(CHANGE_VARS, SFILE_ID);
void FILE_plus_STR(FILE1, STR);
void FLISTA739122510(FLISTA1062334999, INT);
void FLISTd1252754628(FLISTdAM_EXPR, INT);
void IMPL_C170915365(IMPL_CREATE, ELT, ELT, IMPL_INCLUDE);
void IMPL_C1750830100(IMPL_CREATE, IMPL_INCLUDE, IMPL_INCLUDE, ELT, ELT);
void IMPL_C1885190518(IMPL_CREATE);
void IMPL_C228209176(IMPL_CREATE, IMPL_INCLUDE, TP_CLASS, TP_CLASS);
void INLINE1765310158(INLINE, AM_ROUT_DEF);
void INLINE_init(INLINE);
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

AS_FEAT_MOD IMPL_I36886340(IMPL_INCLUDE self, IDENT i) {
 AS_FEAT_MOD ret_val;
 AS_FEAT_MOD e;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 e = ATTR(ATTR(self,as),mods);
 while (1) {
  if ((e==((AS_FEAT_MOD) NULL))) {
   goto after_loop;
  }
  is_eq_self = ATTR(e,name1);
  is_eq_i = i;
  ret_val1 = (is_eq_self.str==is_eq_i.str);
  if (ret_val1) {
   SATTR(self,used_mods,FSETAS559779810(ATTR(self,used_mods), e));
   ret_val = e;
   return ret_val;
  }
  e = ATTR(e,next);
 }
 after_loop: ;
 ret_val = ((AS_FEAT_MOD) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL IMPL_I1019512980(IMPL_INCLUDE self) {
 BOOL ret_val = BOOL_zero;
 INT k = INT_zero;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 IMPL_INCLUDE prog_self;
 PROG ret_val4;
 IMPL_INCLUDE prog_self1;
 PROG ret_val5;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val9;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val10;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val11;
 STR r;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 IMPL_INCLUDE prog_self2;
 PROG ret_val13;
 IMPL_INCLUDE prog_self3;
 PROG ret_val14;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val15;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val16;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val17;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val18;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val19;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val20;
 STR r1;
 INT L1;
 INT L2;
 BOOL L31_;
 dPARSE L4;
 IDENT L5;
 ARRAYdTP L6;
 INT L71_;
 dPARSE L8;
 BOOL L9;
 BOOL L101_;
 BOOL L11;
 INT L12;
 BOOL L131_;
 extern STR Thevaluetype;
 extern STR maynot514500303;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 BOOL L19;
 BOOL L20;
 INT L21;
 BOOL L221_;
 INT L23;
 BOOL L241_;
 extern STR Thereferencetype;
 extern STR maynot1240126487;
 STR L25;
 INT L26;
 OB L27;
 INT L28;
 kind_self = ATTR(self,tp);
 L1 = ATTR(kind_self,kind_cache);
 L2 = TP_KIN138039446;
 L31_=(L1)==(L2); 
 if (L31_) {
  as_tree_self = kind_self;
  if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L4 = ATTR(ATTR(as_tree_self,prog),parse);
   L5 = ATTR(as_tree_self,name1);
   L6 = ATTR(as_tree_self,params);
   L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
   L8 = L4;
   SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L8)])(L8, L5, L71_));
  }
  ret_val2 = ATTR(as_tree_self,my_as_tree);
  L9 = (ret_val2==((AS_CLASS_DEF) NULL));
  L101_=!(L9); 
  if (L101_) {
   SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
  }
 }
 ret_val1 = ATTR(kind_self,kind_cache);
 k = ret_val1;
 if ((ATTR(ATTR(self,impl),arr)==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L12 = TP_KIND_val_tp;
 L131_=(k)==(L12); 
 if (L131_) {
  is_eq_self = ATTR(ATTR(ATTR(self,impl),arr),name1);
  is_eq_i = IDENT_1666445894;
  ret_val3 = (is_eq_self.str==is_eq_i.str);
  L11 = ret_val3;
 } else {
  L11 = FALSE;
 }
 if (L11) {
  prog_self = self;
  ret_val4 = ATTR(ATTR(prog_self,tp),prog);
  PROG_e176405615(ret_val4, ((dPROG_ERR) ATTR(self,as)));
  prog_self1 = self;
  ret_val5 = ATTR(ATTR(prog_self1,tp),prog);
  plus_self = ((STR) &Thevaluetype);
  plus_sarg = TP_CLASS_strrSTR(ATTR(self,tp));
  ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val6;
  plus_sarg1 = ((STR) &maynot514500303);
  ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val7;
  plus_sarg2 = TP_CLASS_strrSTR(ATTR(ATTR(self,impl),arr));
  ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val8;
  plus_arg = '.';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L15 = 1;
  L17=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L15)*sizeof(CHAR);
  L16=ZALLOC_LEAF_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  memset(L16,0,L17);
  ((OB)L16)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((STR) L16);
  L14->asize = L15;
  r = L14;
  SARR((STR)r,0,create_c); 
  ;
  ret_val11 = r;
  ret_val10 = ret_val11;
  s = ret_val10;
  ret_val9 = STR_ap1077157958(plus_self3, s, TRUE);
  PROG_err_STR(ret_val5, ret_val9);
  ret_val = TRUE;
  return ret_val;
 }
 else {
  L21 = TP_KIND_ref_tp;
  L221_=(k)==(L21); 
  if (L221_) {
   L20 = TRUE;
  } else {
   L23 = TP_KIND_spr_tp;
   L241_=(k)==(L23); 
   L20 = L241_;
  }
  if (L20) {
   is_eq_self1 = ATTR(ATTR(ATTR(self,impl),arr),name1);
   is_eq_i1 = IDENT_1485568624;
   ret_val12 = (is_eq_self1.str==is_eq_i1.str);
   L19 = ret_val12;
  } else {
   L19 = FALSE;
  }
  if (L19) {
   prog_self2 = self;
   ret_val13 = ATTR(ATTR(prog_self2,tp),prog);
   PROG_e176405615(ret_val13, ((dPROG_ERR) ATTR(self,as)));
   prog_self3 = self;
   ret_val14 = ATTR(ATTR(prog_self3,tp),prog);
   plus_self4 = ((STR) &Thereferencetype);
   plus_sarg3 = TP_CLASS_strrSTR(ATTR(self,tp));
   ret_val15 = STR_ap2004550586(plus_self4, plus_sarg3);
   plus_self5 = ret_val15;
   plus_sarg4 = ((STR) &maynot1240126487);
   ret_val16 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val16;
   plus_sarg5 = TP_CLASS_strrSTR(ATTR(ATTR(self,impl),arr));
   ret_val17 = STR_ap2004550586(plus_self6, plus_sarg5);
   plus_self7 = ret_val17;
   plus_arg1 = '.';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L26 = 1;
   L28=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L26)*sizeof(CHAR);
   L27=ZALLOC_LEAF_BIG(L28);
   if (L27==NULL) FATAL("Unable to allocate more memory");
   memset(L27,0,L28);
   ((OB)L27)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L27)->header.destroyed=0;
#endif

   L25 = ((STR) L27);
   L25->asize = L26;
   r1 = L25;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val20 = r1;
   ret_val19 = ret_val20;
   s1 = ret_val19;
   ret_val18 = STR_ap1077157958(plus_self7, s1, TRUE);
   PROG_err_STR(ret_val14, ret_val18);
   ret_val = TRUE;
   return ret_val;
  }
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL IMPL_I1756162892(IMPL_INCLUDE self) {
 BOOL ret_val = BOOL_zero;
 AS_FEAT_MOD m1;
 AS_FEAT_MOD m2;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IMPL_INCLUDE prog_self;
 PROG ret_val2;
 IMPL_INCLUDE prog_self1;
 PROG ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR Therea727930475;
 extern STR name18;
 m1 = ATTR(ATTR(self,as),mods);
 while (1) {
  if ((m1==((AS_FEAT_MOD) NULL))) {
   goto after_loop;
  }
  m2 = ATTR(ATTR(self,as),mods);
  while (1) {
   if ((m2==((AS_FEAT_MOD) NULL))) {
    goto after_loop1;
   }
   L2 = (m1==m2);
   L31_=!(L2); 
   if (L31_) {
    is_eq_self = ATTR(m1,name1);
    is_eq_i = ATTR(m2,name1);
    ret_val1 = (is_eq_self.str==is_eq_i.str);
    L1 = ret_val1;
   } else {
    L1 = FALSE;
   }
   if (L1) {
    prog_self = self;
    ret_val2 = ATTR(ATTR(prog_self,tp),prog);
    PROG_e176405615(ret_val2, ((dPROG_ERR) m1));
    prog_self1 = self;
    ret_val3 = ATTR(ATTR(prog_self1,tp),prog);
    plus_self = ((STR) &Therea727930475);
    plus_sarg = ATTR(m1,name1).str;
    ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val4;
    plus_sarg1 = ((STR) &name18);
    ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
    PROG_err_STR(ret_val3, ret_val5);
    ret_val = TRUE;
    return ret_val;
   }
   m2 = ATTR(m2,next);
  }
  after_loop1: ;
  m1 = ATTR(m1,next);
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE1676073295(INLINE_ATTR_READ self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE182457744(INLINE1596311811 self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE600822962(INLINE1433668901 self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE76438420(INLINE self, SIG s) {
 BOOL ret_val = BOOL_zero;
 dINLINE_ITER_SIG inl;
 dINLINE_ROUT_SIG inl1;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
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
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 is_iter_self = s;
 is_iter_self1 = ATTR(is_iter_self,name1);
 L2 = (is_iter_self1.str==((STR) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = is_iter_self1.str;
  L5 = STR_sizerINT(is_iter_self1.str);
  L61_=INTMINUS(L5,1); 
  L7 = L61_;
  L81_=ARR((STR)L4,L7); 
  L9 = L81_;
  L101_=L9=='!'; 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 ret_val2 = L1;
 ret_val1 = ret_val2;
 if (ret_val1) {
  inl = INLINE1838056742(ATTR(self,itbl), s);
  L11 = (inl==((dINLINE_ITER_SIG) NULL));
  L121_=!(L11); 
  ret_val = L121_;
  return ret_val;
 }
 else {
  inl1 = INLINE2019550598(ATTR(self,rtbl), s);
  L13 = (inl1==((dINLINE_ROUT_SIG) NULL));
  L141_=!(L13); 
  ret_val = L141_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL INLINE862622371(INLINE1455768374 self) {
 return ATTR(self,is_special);
}


#undef IS_ITER
#define IS_ITER 0

CHANGE_VARS CHANGE1489663732(CHANGE_VARS self, AM_ROUT_DEF rout) {
 CHANGE_VARS ret_val;
 CHANGE_VARS r;
 AM_ROUT_DEF L11;
 AM_LOCAL_EXPR l;
 AM_LOCAL_EXPR m;
 AM_ASSIGN_STMT as;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARRAYINT L31;
 ARRAYINT L41;
 AM_ASSIGN_STMT as1;
 FLISTA725283029 L51;
 AM_LOCAL_EXPR l1;
 AM_LOCAL_EXPR m1;
 AM_ASSIGN_STMT as2;
 AM_VOID_CONST vconst;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val2 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val4;
 ARRAYINT create_self1;
 INT create_n1 = INT_zero;
 ARRAYINT ret_val5;
 INT L61 = INT_zero;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val6;
 AM_LOCAL_EXPR r1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 AM_ROUT_DEF is_iter_self3;
 BOOL ret_val9 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 AM_ASSIGN_STMT create_self3;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val12;
 AM_ASSIGN_STMT r2;
 AM_ASSIGN_STMT create_self4;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val13;
 AM_ASSIGN_STMT r3;
 AM_LOCAL_EXPR create_self5;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val14;
 AM_LOCAL_EXPR r4;
 AM_ASSIGN_STMT create_self6;
 SFILE_ID create_source4 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val15;
 AM_ASSIGN_STMT r5;
 AM_VOID_CONST create_self7;
 SFILE_ID create_source5 = SFILE_ID_zero;
 AM_VOID_CONST ret_val16;
 AM_VOID_CONST r6;
 CHANGE_VARS L7;
 OB L8;
 INT L91_;
 INT L10;
 INT L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L16;
 INT L171_;
 BOOL L18;
 BOOL L19;
 BOOL L201_;
 STR L22;
 INT L23;
 INT L241_;
 INT L25;
 CHAR L261_;
 CHAR L27;
 BOOL L281_;
 INT L291_;
 ARRAYINT L30;
 INT L32;
 OB L33;
 INT L34;
 INT L351_;
 ARRAYINT L36;
 INT L37;
 OB L38;
 INT L39;
 INT L401_br;
 AM_FORMAL_ARG aL401_;
 INT L421_;
 AM_FORMAL_ARG L45;
 AM_FORMAL_ARG L46;
 AM_LOCAL_EXPR L47;
 OB L48;
 extern STR S_;
 BOOL L49;
 BOOL L50;
 BOOL L521_;
 STR L53;
 INT L54;
 INT L551_;
 INT L56;
 CHAR L571_;
 CHAR L58;
 BOOL L591_;
 AM_ASSIGN_STMT L60;
 OB L62;
 INT L63;
 BOOL L64;
 BOOL L65;
 BOOL L661_;
 ARRAYBOOL L67;
 INT L681_;
 INT L69;
 BOOL L701_;
 BOOL L71;
 BOOL L721_;
 dAM_STMT L73;
 INT L431_br;
INT i_L431_=0;
 dAM_STMT L74;
 INT L441_br;
INT i_L441_=0;
 AM_ASSIGN_STMT L75;
 OB L76;
 dAM_STMT L77;
 INT L781_;
 AM_LOCAL_EXPR L79;
 AM_LOCAL_EXPR L80;
 OB L81;
 AM_ASSIGN_STMT L82;
 OB L83;
 AM_VOID_CONST L84;
 OB L85;
 dAM_STMT L86;
 L8=ZALLOC(sizeof(struct CHANGE_VARS_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=CHANGE_VARS_tag;
 L7 = ((CHANGE_VARS) L8);
 r = L7;
 L91_=(rout)==NULL?0:ASIZE((AM_ROUT_DEF)rout); 
 L10 = L91_;
 L12 = FLISTA1151473122(ATTR(rout,locals1));
 L131_=INTPLUS(L10,L12); 
 SATTR(r,saved_subst,FMAPAM714369621(((FMAPAM1145008811) NULL), L131_));
 L141_=(rout)==NULL?0:ASIZE((AM_ROUT_DEF)rout); 
 L15 = L141_;
 L16 = FLISTA1151473122(ATTR(rout,locals1));
 L171_=INTPLUS(L15,L16); 
 SATTR(r,saved_vars,FLISTA1446031755(((FLISTA725283029) NULL), L171_));
 is_iter_self = rout;
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L19 = (is_iter_self2.str==((STR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L22 = is_iter_self2.str;
  L23 = STR_sizerINT(is_iter_self2.str);
  L241_=INTMINUS(L23,1); 
  L25 = L241_;
  L261_=ARR((STR)L22,L25); 
  L27 = L261_;
  L281_=L27=='!'; 
  L18 = L281_;
 } else {
  L18 = FALSE;
 }
 ret_val3 = L18;
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 if (ret_val1) {
  SATTR(r,init_hot,((dAM_STMT) NULL));
  create_self = ((ARRAYINT) NULL);
  L291_=(rout)==NULL?0:ASIZE((AM_ROUT_DEF)rout); 
  create_n = L291_;
  L32 = create_n;
  L34=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L32)*sizeof(INT);
  L33=ZALLOC_LEAF_BIG(L34);
  if (L33==NULL) FATAL("Unable to allocate more memory");
  memset(L33,0,L34);
  ((OB)L33)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

    ((OB)L33)->header.destroyed=0;
#endif

  L30 = ((ARRAYINT) L33);
  L30->asize = L32;
  ret_val4 = L30;
  SATTR(r,pind_hot,ret_val4);
  SATTR(r,init_nonhot,((dAM_STMT) NULL));
  create_self1 = ((ARRAYINT) NULL);
  L351_=(rout)==NULL?0:ASIZE((AM_ROUT_DEF)rout); 
  create_n1 = L351_;
  L37 = create_n1;
  L39=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L37)*sizeof(INT);
  L38=ZALLOC_LEAF_BIG(L39);
  if (L38==NULL) FATAL("Unable to allocate more memory");
  memset(L38,0,L39);
  ((OB)L38)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

    ((OB)L38)->header.destroyed=0;
#endif

  L36 = ((ARRAYINT) L38);
  L36->asize = L37;
  ret_val5 = L36;
  SATTR(r,pind_nonhot,ret_val5);
 }
 else {
  SATTR(r,init_parms,((dAM_STMT) NULL));
 }
 SATTR(r,init_vars,((dAM_STMT) NULL));
 {
  BOOL f_L401_ = TRUE;
  BOOL f_L421_ = TRUE;
  BOOL f_L431_ = TRUE;
  BOOL f_L441_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L11 = rout;
  L21 = 0;
  L401_br=L11==NULL?0:ASIZE((AM_ROUT_DEF)L11); 
  L421_=L21-1; 
  while (1) {
   if(L61>=L401_br)  goto after_loop; 
   aL401_=ARR((AM_ROUT_DEF)L11,L61); 
   L46=aL401_;
   l = ATTR(L46,expr);
   create_self2 = ((AM_LOCAL_EXPR) NULL);
   create_source = ATTR(rout,source1);
   L48=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L48==NULL) FATAL("Unable to allocate more memory");
   ((OB)L48)->header.tag=AM_LOCAL_EXPR_tag;
   L47 = ((AM_LOCAL_EXPR) L48);
   r1 = L47;
   SATTR(r1,source1,create_source);
   ret_val6 = r1;
   m = ret_val6;
   SATTR(m,is_volatile,ATTR(l,is_volatile));
   plus_self = ATTR(ATTR(rout,sig1),name1).str;
   plus_sarg = ((STR) &S_);
   ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val7;
   plus_sarg1 = ATTR(l,name1).str;
   ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
   SATTR(m,name1,IDENT_1150413730(IDENT_zero, ret_val8));
   SATTR(m,tp_at,ATTR(l,tp_at));
   SATTR(m,needs_init,ATTR(l,needs_init));
   is_iter_self3 = rout;
   is_iter_self4 = ATTR(is_iter_self3,sig1);
   is_iter_self5 = ATTR(is_iter_self4,name1);
   L50 = (is_iter_self5.str==((STR) NULL));
   L521_=!(L50); 
   if (L521_) {
    L53 = is_iter_self5.str;
    L54 = STR_sizerINT(is_iter_self5.str);
    L551_=INTMINUS(L54,1); 
    L56 = L551_;
    L571_=ARR((STR)L53,L56); 
    L58 = L571_;
    L591_=L58=='!'; 
    L49 = L591_;
   } else {
    L49 = FALSE;
   }
   ret_val11 = L49;
   ret_val10 = ret_val11;
   ret_val9 = ret_val10;
   if (ret_val9) {
    create_self3 = ((AM_ASSIGN_STMT) NULL);
    create_source1 = ATTR(rout,source1);
    L62=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
    if (L62==NULL) FATAL("Unable to allocate more memory");
    ((OB)L62)->header.tag=AM_ASSIGN_STMT_tag;
    L60 = ((AM_ASSIGN_STMT) L62);
    r2 = L60;
    SATTR(r2,source1,create_source1);
    ret_val12 = r2;
    as = ret_val12;
    SATTR(as,dest,((dAM_EXPR) m));
    SATTR(as,src1,((dAM_EXPR) NULL));
    L421_++; 
    i = L421_;
    if ((ATTR(ATTR(rout,sig1),hot)==((ARRAYBOOL) NULL))) {
     L65 = TRUE;
    } else {
     L661_=(i)==(0); 
     L65 = L661_;
    }
    if (L65) {
     L64 = TRUE;
    } else {
     L67 = ATTR(ATTR(rout,sig1),hot);
     L681_=INTMINUS(i,1); 
     L69 = L681_;
     L701_=ARR((ARRAYBOOL)L67,L69); 
     L71 = L701_;
     L721_=!(L71); 
     L64 = L721_;
    }
    if (L64) {
     if ((ATTR(r,init_nonhot)==((dAM_STMT) NULL))) {
      SATTR(r,init_nonhot,((dAM_STMT) as));
     }
     else {
      L73 = ATTR(r,init_nonhot);
      (*dAM_ST2020072576[TAG(L73)])(L73, ((dAM_STMT) as));
     }
     if (f_L431_) {
      f_L431_ = FALSE;
      L31 = ATTR(r,pind_nonhot);
      L431_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
      i_L431_=0;
     }
     if(i_L431_>=L431_br)  goto after_loop; 
     SARR((ARRAYINT)L31,i_L431_,i); i_L431_++;
     ;
    }
    else {
     if ((ATTR(r,init_hot)==((dAM_STMT) NULL))) {
      SATTR(r,init_hot,((dAM_STMT) as));
     }
     else {
      L74 = ATTR(r,init_hot);
      (*dAM_ST2020072576[TAG(L74)])(L74, ((dAM_STMT) as));
     }
     if (f_L441_) {
      f_L441_ = FALSE;
      L41 = ATTR(r,pind_hot);
      L441_br=L41==NULL?0:ASIZE((ARRAYINT)L41); 
      i_L441_=0;
     }
     if(i_L441_>=L441_br)  goto after_loop; 
     SARR((ARRAYINT)L41,i_L441_,i); i_L441_++;
     ;
    }
   }
   else {
    create_self4 = ((AM_ASSIGN_STMT) NULL);
    create_source2 = ATTR(rout,source1);
    L76=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
    if (L76==NULL) FATAL("Unable to allocate more memory");
    ((OB)L76)->header.tag=AM_ASSIGN_STMT_tag;
    L75 = ((AM_ASSIGN_STMT) L76);
    r3 = L75;
    SATTR(r3,source1,create_source2);
    ret_val13 = r3;
    as1 = ret_val13;
    SATTR(as1,dest,((dAM_EXPR) m));
    SATTR(as1,src1,((dAM_EXPR) NULL));
    if ((ATTR(r,init_parms)==((dAM_STMT) NULL))) {
     SATTR(r,init_parms,((dAM_STMT) as1));
    }
    else {
     L77 = ATTR(r,init_parms);
     (*dAM_ST2020072576[TAG(L77)])(L77, ((dAM_STMT) as1));
    }
   }
   SATTR(m,no_assign,ATTR(l,no_assign));
   SATTR(r,saved_subst,FMAPAM876857188(ATTR(r,saved_subst), l, m));
   SATTR(r,saved_vars,FLISTA1817671564(ATTR(r,saved_vars), m));
   L781_=INTPLUS(L61,1); 
   L61 = L781_;
  }
 }
 after_loop: ;
 {
  struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname1 = &other2_0;
  L51 = ATTR(rout,locals1);
  noname1->self = L51;
  noname1->state = 0;
  while (1) {
   L79 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   l1 = L79;
   create_self5 = ((AM_LOCAL_EXPR) NULL);
   create_source3 = ATTR(rout,source1);
   L81=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L81==NULL) FATAL("Unable to allocate more memory");
   ((OB)L81)->header.tag=AM_LOCAL_EXPR_tag;
   L80 = ((AM_LOCAL_EXPR) L81);
   r4 = L80;
   SATTR(r4,source1,create_source3);
   ret_val14 = r4;
   m1 = ret_val14;
   SATTR(m1,is_volatile,ATTR(l1,is_volatile));
   SATTR(m1,name1,ATTR(l1,name1));
   SATTR(m1,tp_at,ATTR(l1,tp_at));
   SATTR(m1,needs_init,ATTR(l1,needs_init));
   if (ATTR(m1,needs_init)) {
    create_self6 = ((AM_ASSIGN_STMT) NULL);
    create_source4 = ATTR(rout,source1);
    L83=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
    if (L83==NULL) FATAL("Unable to allocate more memory");
    ((OB)L83)->header.tag=AM_ASSIGN_STMT_tag;
    L82 = ((AM_ASSIGN_STMT) L83);
    r5 = L82;
    SATTR(r5,source1,create_source4);
    ret_val15 = r5;
    as2 = ret_val15;
    SATTR(as2,dest,((dAM_EXPR) m1));
    create_self7 = ((AM_VOID_CONST) NULL);
    create_source5 = ATTR(rout,source1);
    L85=ZALLOC(sizeof(struct AM_VOID_CONST_struct));
    if (L85==NULL) FATAL("Unable to allocate more memory");
    ((OB)L85)->header.tag=AM_VOID_CONST_tag;
    L84 = ((AM_VOID_CONST) L85);
    r6 = L84;
    SATTR(r6,source1,create_source5);
    ret_val16 = r6;
    vconst = ret_val16;
    SATTR(vconst,tp_at,ATTR(m1,tp_at));
    SATTR(as2,src1,((dAM_EXPR) vconst));
    if ((ATTR(r,init_vars)==((dAM_STMT) NULL))) {
     SATTR(r,init_vars,((dAM_STMT) as2));
    }
    else {
     L86 = ATTR(r,init_vars);
     (*dAM_ST2020072576[TAG(L86)])(L86, ((dAM_STMT) as2));
    }
   }
   SATTR(m1,no_assign,ATTR(l1,no_assign));
   SATTR(r,saved_subst,FMAPAM876857188(ATTR(r,saved_subst), l1, m1));
   SATTR(r,saved_vars,FLISTA1817671564(ATTR(r,saved_vars), m1));
  }
 }
 after_loop1: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT IMPL_C144337270(IMPL_CREATE self, dAS_CLASS_ELT t) {
 ELT ret_val;
 SIG sig1 = ((SIG) NULL);
 ELT r = ((ELT) NULL);
 IMPL_CREATE prog_self;
 PROG ret_val1;
 IMPL_CREATE prog_self1;
 PROG ret_val2;
 IMPL_CREATE prog_self2;
 PROG ret_val3;
 IMPL_CREATE prog_self3;
 PROG ret_val4;
 SIG shared_reader_si;
 AS_SHARED_DEF shared_reader_si1;
 IDENT shared_reader_si2 = IDENT_zero;
 ARRAYdTP shared_reader_si3;
 TP_CONTEXT shared_reader_si4;
 SIG ret_val5;
 SIG r1;
 IMPL_CREATE prog_self4;
 PROG ret_val6;
 IMPL_CREATE prog_self5;
 PROG ret_val7;
 SIG attr_reader_sig_;
 AS_ATTR_DEF attr_reader_sig_1;
 IDENT attr_reader_sig_2 = IDENT_zero;
 ARRAYdTP attr_reader_sig_3;
 TP_CONTEXT attr_reader_sig_4;
 SIG ret_val8;
 SIG r2;
 ELT create_self;
 SIG create_sig;
 SIG create_srcsig;
 dAS_CLASS_ELT create_as;
 TP_CONTEXT create_con;
 BOOL create_is_privat = BOOL_zero;
 ELT ret_val9;
 ELT r3;
 TP_CLASS is_partial_self;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val11 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val12;
 IMPL_CREATE prog_self6;
 PROG ret_val13;
 IMPL_CREATE prog_self7;
 PROG ret_val14;
 ELT create_self1;
 SIG create_sig1;
 SIG create_srcsig1;
 dAS_CLASS_ELT create_as1;
 TP_CONTEXT create_con1;
 BOOL create_is_privat1 = BOOL_zero;
 ELT ret_val15;
 ELT r4;
 ELT create_self2;
 SIG create_sig2;
 SIG create_srcsig2;
 dAS_CLASS_ELT create_as2;
 TP_CONTEXT create_con2;
 BOOL create_is_privat2 = BOOL_zero;
 ELT ret_val16;
 ELT r5;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 extern STR Extern1091872787;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 extern STR Extern1571001135;
 SIG L7;
 OB L8;
 BOOL L9;
 BOOL L10;
 BOOL L111_;
 extern STR Extern490213281;
 SIG L12;
 OB L13;
 BOOL L14;
 BOOL L151_;
 ELT L16;
 OB L17;
 INT L18;
 INT L19;
 BOOL L201_;
 dPARSE L21;
 IDENT L22;
 ARRAYdTP L23;
 INT L241_;
 dPARSE L25;
 BOOL L26;
 BOOL L271_;
 INT L28;
 INT L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 extern STR Stubfe715112180;
 BOOL L34;
 BOOL L351_;
 ELT L36;
 OB L37;
 dAS_CLASS_ELT L38;
 ELT L39;
 OB L40;
 if (t==NULL) {
  goto other125;
 } else
 switch (TAG(t)) {
  case AS_CONST_DEF_tag:
   if (ATTR(self,is_external)) {
    L2 = TP_CLA727826289(ATTR(self,tp));
    L31_=!(L2); 
    L1 = L31_;
   } else {
    L1 = FALSE;
   }
   if (L1) {
    prog_self = self;
    ret_val1 = ATTR(ATTR(prog_self,tp),prog);
    PROG_e176405615(ret_val1, ((dPROG_ERR) t));
    prog_self1 = self;
    ret_val2 = ATTR(ATTR(prog_self1,tp),prog);
    PROG_err_STR(ret_val2, ((STR) &Extern1091872787));
    ret_val = ((ELT) NULL);
    return ret_val;
   }
   sig1 = SIG_co2044018076(((SIG) NULL), ((AS_CONST_DEF) t), ATTR(((AS_CONST_DEF) t),name1), ATTR(ATTR(self,con),ptypes), ATTR(self,con)); break;
  case AS_SHARED_DEF_tag:
   if (ATTR(self,is_external)) {
    L5 = TP_CLA727826289(ATTR(self,tp));
    L61_=!(L5); 
    L4 = L61_;
   } else {
    L4 = FALSE;
   }
   if (L4) {
    prog_self2 = self;
    ret_val3 = ATTR(ATTR(prog_self2,tp),prog);
    PROG_e176405615(ret_val3, ((dPROG_ERR) t));
    prog_self3 = self;
    ret_val4 = ATTR(ATTR(prog_self3,tp),prog);
    PROG_err_STR(ret_val4, ((STR) &Extern1571001135));
    ret_val = ((ELT) NULL);
    return ret_val;
   }
   shared_reader_si = ((SIG) NULL);
   shared_reader_si1 = ((AS_SHARED_DEF) t);
   shared_reader_si2 = ATTR(((AS_SHARED_DEF) t),name1);
   shared_reader_si3 = ATTR(ATTR(self,con),ptypes);
   shared_reader_si4 = ATTR(self,con);
   L8=ZALLOC(sizeof(struct SIG_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=SIG_tag;
   L7 = ((SIG) L8);
   r1 = L7;
   SATTR(r1,tp,((dTP) ATTR(shared_reader_si4,same1)));
   SATTR(r1,name1,shared_reader_si2);
   SATTR(r1,src_tparams,shared_reader_si3);
   SATTR(r1,ret,TP_CON1800432689(shared_reader_si4, ATTR(shared_reader_si1,tp)));
   ret_val5 = SIG_ma1442297600(r1);
   sig1 = ret_val5; break;
  case AS_ATTR_DEF_tag:
   if (ATTR(self,is_external)) {
    L10 = TP_CLA727826289(ATTR(self,tp));
    L111_=!(L10); 
    L9 = L111_;
   } else {
    L9 = FALSE;
   }
   if (L9) {
    prog_self4 = self;
    ret_val6 = ATTR(ATTR(prog_self4,tp),prog);
    PROG_e176405615(ret_val6, ((dPROG_ERR) t));
    prog_self5 = self;
    ret_val7 = ATTR(ATTR(prog_self5,tp),prog);
    PROG_err_STR(ret_val7, ((STR) &Extern490213281));
    ret_val = ((ELT) NULL);
    return ret_val;
   }
   attr_reader_sig_ = ((SIG) NULL);
   attr_reader_sig_1 = ((AS_ATTR_DEF) t);
   attr_reader_sig_2 = ATTR(((AS_ATTR_DEF) t),name1);
   attr_reader_sig_3 = ATTR(ATTR(self,con),ptypes);
   attr_reader_sig_4 = ATTR(self,con);
   L13=ZALLOC(sizeof(struct SIG_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=SIG_tag;
   L12 = ((SIG) L13);
   r2 = L12;
   SATTR(r2,tp,((dTP) ATTR(attr_reader_sig_4,same1)));
   SATTR(r2,name1,attr_reader_sig_2);
   SATTR(r2,src_tparams,attr_reader_sig_3);
   SATTR(r2,ret,TP_CON1800432689(attr_reader_sig_4, ATTR(attr_reader_sig_1,tp)));
   ret_val8 = SIG_ma1442297600(r2);
   sig1 = ret_val8; break;
  case AS_ROUT_DEF_tag:
   sig1 = SIG_ro1276623596(((SIG) NULL), ((AS_ROUT_DEF) t), ATTR(((AS_ROUT_DEF) t),name1), ATTR(ATTR(self,con),ptypes), ATTR(self,con));
   if (ATTR(((AS_ROUT_DEF) t),is_abstract)) {
    if (ATTR(self,is_external)) {
     L14 = SIG_is683883765(sig1);
     L151_=!(L14); 
     if (L151_) {
      ret_val = ((ELT) NULL);
      return ret_val;
     }
     create_self = ((ELT) NULL);
     create_sig = sig1;
     create_srcsig = sig1;
     create_as = t;
     create_con = ATTR(self,con);
     create_is_privat = ATTR(((AS_ROUT_DEF) t),is_private);
     L17=ZALLOC(sizeof(struct ELT_struct));
     if (L17==NULL) FATAL("Unable to allocate more memory");
     ((OB)L17)->header.tag=ELT_tag;
     L16 = ((ELT) L17);
     r3 = L16;
     SATTR(r3,sig1,create_sig);
     SATTR(r3,srcsig,create_srcsig);
     SATTR(r3,as,create_as);
     SATTR(r3,con,create_con);
     SATTR(r3,is_private,create_is_privat);
     ret_val9 = r3;
     r = ret_val9;
     SATTR(r,is_external,TRUE);
     ret_val = r;
     return ret_val;
    }
    else {
     is_partial_self = ATTR(self,tp);
     kind_self = is_partial_self;
     L18 = ATTR(kind_self,kind_cache);
     L19 = TP_KIN138039446;
     L201_=(L18)==(L19); 
     if (L201_) {
      as_tree_self = kind_self;
      if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
       L21 = ATTR(ATTR(as_tree_self,prog),parse);
       L22 = ATTR(as_tree_self,name1);
       L23 = ATTR(as_tree_self,params);
       L241_=(L23)==NULL?0:ASIZE((ARRAYdTP)L23); 
       L25 = L21;
       SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L25)])(L25, L22, L241_));
      }
      ret_val12 = ATTR(as_tree_self,my_as_tree);
      L26 = (ret_val12==((AS_CLASS_DEF) NULL));
      L271_=!(L26); 
      if (L271_) {
       SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
      }
     }
     ret_val11 = ATTR(kind_self,kind_cache);
     L28 = ret_val11;
     L29 = TP_KIND_part_tp;
     L301_=(L28)==(L29); 
     ret_val10 = L301_;
     L32 = ret_val10;
     L331_=!(L32); 
     if (L331_) {
      prog_self6 = self;
      ret_val13 = ATTR(ATTR(prog_self6,tp),prog);
      PROG_e176405615(ret_val13, ((dPROG_ERR) t));
      prog_self7 = self;
      ret_val14 = ATTR(ATTR(prog_self7,tp),prog);
      PROG_err_STR(ret_val14, ((STR) &Stubfe715112180));
      ret_val = ((ELT) NULL);
      return ret_val;
     }
    }
   }
   else {
    if (ATTR(self,is_external)) {
     L34 = SIG_is1444519892(sig1);
     L351_=!(L34); 
     if (L351_) {
      ret_val = ((ELT) NULL);
      return ret_val;
     }
     create_self1 = ((ELT) NULL);
     create_sig1 = sig1;
     create_srcsig1 = sig1;
     create_as1 = t;
     create_con1 = ATTR(self,con);
     create_is_privat1 = ATTR(((AS_ROUT_DEF) t),is_private);
     L37=ZALLOC(sizeof(struct ELT_struct));
     if (L37==NULL) FATAL("Unable to allocate more memory");
     ((OB)L37)->header.tag=ELT_tag;
     L36 = ((ELT) L37);
     r4 = L36;
     SATTR(r4,sig1,create_sig1);
     SATTR(r4,srcsig,create_srcsig1);
     SATTR(r4,as,create_as1);
     SATTR(r4,con,create_con1);
     SATTR(r4,is_private,create_is_privat1);
     ret_val15 = r4;
     r = ret_val15;
     SATTR(r,is_external,TRUE);
     ret_val = r;
     return ret_val;
    }
   } break;
  default: ;
  other125: ;
   ret_val = ((ELT) NULL);
   return ret_val;
 }
 if ((sig1==((SIG) NULL))) {
  ret_val = ((ELT) NULL);
  return ret_val;
 }
 create_self2 = ((ELT) NULL);
 create_sig2 = sig1;
 create_srcsig2 = sig1;
 create_as2 = t;
 create_con2 = ATTR(self,con);
 L38 = t;
 create_is_privat2 = (*dAS_CL1087157762[TAG(L38)])(L38);
 L40=ZALLOC(sizeof(struct ELT_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=ELT_tag;
 L39 = ((ELT) L40);
 r5 = L39;
 SATTR(r5,sig1,create_sig2);
 SATTR(r5,srcsig,create_srcsig2);
 SATTR(r5,as,create_as2);
 SATTR(r5,con,create_con2);
 SATTR(r5,is_private,create_is_privat2);
 ret_val16 = r5;
 r = ret_val16;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT IMPL_C872226022(IMPL_CREATE self, dAS_CLASS_ELT t) {
 ELT ret_val;
 SIG sig1 = ((SIG) NULL);
 BOOL pri1 = BOOL_zero;
 ELT create_self;
 SIG create_sig;
 SIG create_srcsig;
 dAS_CLASS_ELT create_as;
 TP_CONTEXT create_con;
 BOOL create_is_privat = BOOL_zero;
 ELT ret_val1;
 ELT r;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L5;
 ELT L6;
 OB L7;
 if (ATTR(self,is_external)) {
  L2 = TP_CLA727826289(ATTR(self,tp));
  L31_=!(L2); 
  L1 = L31_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  ret_val = ((ELT) NULL);
  return ret_val;
 }
 if (t==NULL) {
  goto other126;
 } else
 switch (TAG(t)) {
  case AS_SHARED_DEF_tag:
   sig1 = SIG_sh1677498347(((SIG) NULL), ((AS_SHARED_DEF) t), ATTR(((AS_SHARED_DEF) t),name1), ATTR(ATTR(self,con),ptypes), ATTR(self,con));
   if (ATTR(((AS_SHARED_DEF) t),is_private)) {
    L4 = TRUE;
   } else {
    L4 = ATTR(((AS_SHARED_DEF) t),is_readonly);
   }
   pri1 = L4; break;
  case AS_ATTR_DEF_tag:
   sig1 = SIG_at1749337727(((SIG) NULL), ((AS_ATTR_DEF) t), ATTR(((AS_ATTR_DEF) t),name1), ATTR(ATTR(self,con),ptypes), ATTR(self,con));
   if (ATTR(((AS_ATTR_DEF) t),is_private)) {
    L5 = TRUE;
   } else {
    L5 = ATTR(((AS_ATTR_DEF) t),is_readonly);
   }
   pri1 = L5; break;
  default: ;
  other126: ;
   ret_val = ((ELT) NULL);
   return ret_val;
 }
 if ((sig1==((SIG) NULL))) {
  ret_val = ((ELT) NULL);
  return ret_val;
 }
 create_self = ((ELT) NULL);
 create_sig = sig1;
 create_srcsig = sig1;
 create_as = t;
 create_con = ATTR(self,con);
 create_is_privat = pri1;
 L7=ZALLOC(sizeof(struct ELT_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=ELT_tag;
 L6 = ((ELT) L7);
 r = L6;
 SATTR(r,sig1,create_sig);
 SATTR(r,srcsig,create_srcsig);
 SATTR(r,as,create_as);
 SATTR(r,con,create_con);
 SATTR(r,is_private,create_is_privat);
 ret_val1 = r;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT IMPL_I954938038(IMPL_INCLUDE self, ELT e) {
 ELT ret_val;
 TP_CONTEXT con;
 IDENT name111 = IDENT_zero;
 BOOL read_pri = BOOL_zero;
 BOOL write_pri = BOOL_zero;
 AS_FEAT_MOD m;
 ARRAYdTP srcparams;
 BOOL has_reader = BOOL_zero;
 BOOL has_writer = BOOL_zero;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT elt1;
 SIG es;
 SIG sig1 = ((SIG) NULL);
 BOOL pri1 = BOOL_zero;
 dAS_CLASS_ELT eas;
 ELT name_self;
 IDENT ret_val1 = IDENT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT name_self1;
 IDENT ret_val3 = IDENT_zero;
 SIG is_reader_sig_se;
 BOOL ret_val4 = BOOL_zero;
 IMPL_INCLUDE include_readonly;
 dAS_CLASS_ELT include_readonly1;
 IMPL_INCLUDE prog_self;
 PROG ret_val5;
 IMPL_INCLUDE prog_self1;
 PROG ret_val6;
 ELT name_self2;
 IDENT ret_val7 = IDENT_zero;
 SIG shared_reader_si;
 AS_SHARED_DEF shared_reader_si1;
 IDENT shared_reader_si2 = IDENT_zero;
 ARRAYdTP shared_reader_si3;
 TP_CONTEXT shared_reader_si4;
 SIG ret_val8;
 SIG r;
 SIG attr_reader_sig_;
 AS_ATTR_DEF attr_reader_sig_1;
 IDENT attr_reader_sig_2 = IDENT_zero;
 ARRAYdTP attr_reader_sig_3;
 TP_CONTEXT attr_reader_sig_4;
 SIG ret_val9;
 SIG r1;
 SIG is_reader_sig_se1;
 BOOL ret_val10 = BOOL_zero;
 ELT create_self;
 SIG create_sig;
 SIG create_srcsig;
 dAS_CLASS_ELT create_as;
 TP_CONTEXT create_con;
 BOOL create_is_privat = BOOL_zero;
 ELT ret_val11;
 ELT r2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 ELT L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L12;
 BOOL L13;
 BOOL L141_;
 extern STR Bothre938930097;
 BOOL L15;
 SIG L16;
 OB L17;
 SIG L18;
 OB L19;
 BOOL L20;
 BOOL L221_;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 ELT L28;
 OB L29;
 con = ATTR(e,con);
 name_self = e;
 ret_val1 = ATTR(ATTR(name_self,sig1),name1);
 m = IMPL_I36886340(self, ret_val1);
 srcparams = ATTR(ATTR(e,srcsig),src_tparams);
 L3 = (m==((AS_FEAT_MOD) NULL));
 L41_=!(L3); 
 if (L41_) {
  is_eq_self = ATTR(m,new_name);
  is_eq_i = IDENT_1150413730(IDENT_zero, ((STR) NULL));
  ret_val2 = (is_eq_self.str==is_eq_i.str);
  if (ret_val2) {
   ret_val = ((ELT) NULL);
   return ret_val;
  }
  name111 = ATTR(m,new_name);
  read_pri = ATTR(m,is_private);
  if (ATTR(m,is_private)) {
   L5 = TRUE;
  } else {
   L5 = ATTR(m,is_readonly);
  }
  write_pri = L5;
  if (ATTR(m,is_readonly)) {
   has_reader = FALSE;
   has_writer = FALSE;
   {
    struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
    L11 = ATTR(ATTR(self,impl),elts);
    name_self1 = e;
    ret_val3 = ATTR(ATTR(name_self1,sig1),name1);
    L21 = ret_val3;
    noname1->self = L11;
    noname1->arg1 = L21;
    noname1->state = 0;
    while (1) {
     L6 = ELT_TB431570775(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     elt1 = L6;
     es = ATTR(elt1,sig1);
     is_reader_sig_se = es;
     if ((ATTR(is_reader_sig_se,args)==((ARRAYARG) NULL))) {
      L8 = (ATTR(is_reader_sig_se,ret)==((dTP) NULL));
      L91_=!(L8); 
      L7 = L91_;
     } else {
      L7 = FALSE;
     }
     ret_val4 = L7;
     if (ret_val4) {
      has_reader = TRUE;
     }
     else {
      if (SIG_is1739708917(es)) {
       L10 = TRUE;
      } else {
       L10 = SIG_is152200802(es);
      }
      if (L10) {
       has_writer = TRUE;
      }
     }
    }
   }
   after_loop: ;
   if (has_reader) {
    L12 = has_writer;
   } else {
    L12 = FALSE;
   }
   L13 = L12;
   L141_=!(L13); 
   if (L141_) {
    include_readonly = self;
    include_readonly1 = ((dAS_CLASS_ELT) ATTR(self,as));
    prog_self = include_readonly;
    ret_val5 = ATTR(ATTR(prog_self,tp),prog);
    PROG_e176405615(ret_val5, ((dPROG_ERR) include_readonly1));
    prog_self1 = include_readonly;
    ret_val6 = ATTR(ATTR(prog_self1,tp),prog);
    PROG_err_STR(ret_val6, ((STR) &Bothre938930097));
    ret_val = ((ELT) NULL);
    return ret_val;
   }
  }
 }
 else {
  name_self2 = e;
  ret_val7 = ATTR(ATTR(name_self2,sig1),name1);
  name111 = ret_val7;
  if (ATTR(ATTR(self,as),is_private)) {
   L15 = TRUE;
  } else {
   L15 = ATTR(e,is_private);
  }
  read_pri = L15;
  write_pri = read_pri;
 }
 eas = ATTR(e,as);
 switch (TAG(eas)) {
  case AS_CONST_DEF_tag:
   sig1 = SIG_co2044018076(((SIG) NULL), ((AS_CONST_DEF) eas), name111, srcparams, con);
   pri1 = read_pri; break;
  case AS_SHARED_DEF_tag:
   if (ELT_is507942138(e)) {
    sig1 = SIG_sh1677498347(((SIG) NULL), ((AS_SHARED_DEF) eas), name111, srcparams, con);
    pri1 = write_pri;
   }
   else {
    shared_reader_si = ((SIG) NULL);
    shared_reader_si1 = ((AS_SHARED_DEF) eas);
    shared_reader_si2 = name111;
    shared_reader_si3 = srcparams;
    shared_reader_si4 = con;
    L17=ZALLOC(sizeof(struct SIG_struct));
    if (L17==NULL) FATAL("Unable to allocate more memory");
    ((OB)L17)->header.tag=SIG_tag;
    L16 = ((SIG) L17);
    r = L16;
    SATTR(r,tp,((dTP) ATTR(shared_reader_si4,same1)));
    SATTR(r,name1,shared_reader_si2);
    SATTR(r,src_tparams,shared_reader_si3);
    SATTR(r,ret,TP_CON1800432689(shared_reader_si4, ATTR(shared_reader_si1,tp)));
    ret_val8 = SIG_ma1442297600(r);
    sig1 = ret_val8;
    pri1 = read_pri;
   } break;
  case AS_ATTR_DEF_tag:
   if (ELT_is1808162451(e)) {
    sig1 = SIG_at1749337727(((SIG) NULL), ((AS_ATTR_DEF) eas), name111, srcparams, con);
    pri1 = write_pri;
   }
   else {
    attr_reader_sig_ = ((SIG) NULL);
    attr_reader_sig_1 = ((AS_ATTR_DEF) eas);
    attr_reader_sig_2 = name111;
    attr_reader_sig_3 = srcparams;
    attr_reader_sig_4 = con;
    L19=ZALLOC(sizeof(struct SIG_struct));
    if (L19==NULL) FATAL("Unable to allocate more memory");
    ((OB)L19)->header.tag=SIG_tag;
    L18 = ((SIG) L19);
    r1 = L18;
    SATTR(r1,tp,((dTP) ATTR(attr_reader_sig_4,same1)));
    SATTR(r1,name1,attr_reader_sig_2);
    SATTR(r1,src_tparams,attr_reader_sig_3);
    SATTR(r1,ret,TP_CON1800432689(attr_reader_sig_4, ATTR(attr_reader_sig_1,tp)));
    ret_val9 = SIG_ma1442297600(r1);
    sig1 = ret_val9;
    pri1 = read_pri;
   } break;
  case AS_ROUT_DEF_tag:
   sig1 = SIG_ro1276623596(((SIG) NULL), ((AS_ROUT_DEF) eas), name111, srcparams, con);
   pri1 = read_pri;
   L20 = (m==((AS_FEAT_MOD) NULL));
   L221_=!(L20); 
   if (L221_) {
    if (ATTR(m,is_readonly)) {
     is_reader_sig_se1 = sig1;
     if ((ATTR(is_reader_sig_se1,args)==((ARRAYARG) NULL))) {
      L24 = (ATTR(is_reader_sig_se1,ret)==((dTP) NULL));
      L251_=!(L24); 
      L23 = L251_;
     } else {
      L23 = FALSE;
     }
     ret_val10 = L23;
     L26 = ret_val10;
     L271_=!(L26); 
     if (L271_) {
      pri1 = write_pri;
     }
    }
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin IMPL_INCLUDE::modify_elt(ELT):ELT\n./Representation/impl.sa:457:26");
 }
 if ((sig1==((SIG) NULL))) {
  ret_val = ((ELT) NULL);
  return ret_val;
 }
 create_self = ((ELT) NULL);
 create_sig = sig1;
 create_srcsig = ATTR(e,srcsig);
 create_as = ATTR(e,as);
 create_con = con;
 create_is_privat = pri1;
 L29=ZALLOC(sizeof(struct ELT_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=ELT_tag;
 L28 = ((ELT) L29);
 r2 = L28;
 SATTR(r2,sig1,create_sig);
 SATTR(r2,srcsig,create_srcsig);
 SATTR(r2,as,create_as);
 SATTR(r2,con,create_con);
 SATTR(r2,is_private,create_is_privat);
 ret_val11 = r2;
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL IMPL_C1114329143(IMPL_CREATE self) {
 ELT_TBL ret_val;
 dAS_CLASS_ELT t;
 ELT_TBL r = ((ELT_TBL) NULL);
 ELT er;
 ELT f;
 ELT f1;
 ELT ew;
 ELT f2;
 IMPL_CREATE ext_conflict_err;
 ELT ext_conflict_err1;
 ELT ext_conflict_err2;
 IMPL_CREATE prog_self;
 PROG ret_val1;
 IMPL_CREATE prog_self1;
 PROG ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 IMPL_CREATE reader_conflict_;
 ELT reader_conflict_1;
 ELT reader_conflict_2;
 IMPL_CREATE prog_self2;
 PROG ret_val7;
 IMPL_CREATE prog_self3;
 PROG ret_val8;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val11;
 STR plus_self7;
 CHAR plus_arg = CHAR_zero;
 STR ret_val12;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val13;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val14;
 STR r1;
 IMPL_CREATE writer_conflict_;
 ELT writer_conflict_1;
 ELT writer_conflict_2;
 IMPL_CREATE prog_self4;
 PROG ret_val15;
 IMPL_CREATE prog_self5;
 PROG ret_val16;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val17;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self10;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self11;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val20;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val21;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val22;
 STR r2;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR Thesignature1;
 extern STR hasthe192443696;
 extern STR inanex715602695;
 BOOL L7;
 BOOL L81_;
 extern STR Thesignature1;
 extern STR ofther153420789;
 STR L9;
 INT L10;
 OB L11;
 INT L12;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR Thesignature1;
 extern STR ofthew1419133383;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 dAS_CLASS_ELT L24;
 t = ATTR(ATTR(self,as),body);
 while (1) {
  L1 = (t==((dAS_CLASS_ELT) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  er = IMPL_C144337270(self, t);
  L3 = (er==((ELT) NULL));
  L41_=!(L3); 
  if (L41_) {
   if (ATTR(self,is_external)) {
    f = ELT_TB954957059(r, er);
    L5 = (f==((ELT) NULL));
    L61_=!(L5); 
    if (L61_) {
     ext_conflict_err = self;
     ext_conflict_err1 = er;
     ext_conflict_err2 = f;
     prog_self = ext_conflict_err;
     ret_val1 = ATTR(ATTR(prog_self,tp),prog);
     PROG_e176405615(ret_val1, ((dPROG_ERR) ATTR(ext_conflict_err1,as)));
     prog_self1 = ext_conflict_err;
     ret_val2 = ATTR(ATTR(prog_self1,tp),prog);
     plus_self = ((STR) &Thesignature1);
     plus_sarg = ATTR(ATTR(ext_conflict_err1,sig1),str);
     ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val3;
     plus_sarg1 = ((STR) &hasthe192443696);
     ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val4;
     plus_sarg2 = ATTR(ATTR(ext_conflict_err2,sig1),str);
     ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val5;
     plus_sarg3 = ((STR) &inanex715602695);
     ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(ret_val2, ret_val6);
    }
    else {
     r = ELT_TB1703146017(r, er);
    }
   }
   else {
    f1 = ELT_TB1201943681(r, er);
    L7 = (f1==((ELT) NULL));
    L81_=!(L7); 
    if (L81_) {
     reader_conflict_ = self;
     reader_conflict_1 = er;
     reader_conflict_2 = f1;
     prog_self2 = reader_conflict_;
     ret_val7 = ATTR(ATTR(prog_self2,tp),prog);
     PROG_e176405615(ret_val7, ((dPROG_ERR) ATTR(reader_conflict_1,as)));
     prog_self3 = reader_conflict_;
     ret_val8 = ATTR(ATTR(prog_self3,tp),prog);
     plus_self4 = ((STR) &Thesignature1);
     plus_sarg4 = ATTR(ATTR(reader_conflict_1,sig1),str);
     ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val9;
     plus_sarg5 = ((STR) &ofther153420789);
     ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
     plus_self6 = ret_val10;
     plus_sarg6 = ATTR(ATTR(reader_conflict_2,sig1),str);
     ret_val11 = STR_ap2004550586(plus_self6, plus_sarg6);
     plus_self7 = ret_val11;
     plus_arg = '.';
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
     L10 = 1;
     L12=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L10)*sizeof(CHAR);
     L11=ZALLOC_LEAF_BIG(L12);
     if (L11==NULL) FATAL("Unable to allocate more memory");
     memset(L11,0,L12);
     ((OB)L11)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L11)->header.destroyed=0;
#endif

     L9 = ((STR) L11);
     L9->asize = L10;
     r1 = L9;
     SARR((STR)r1,0,create_c); 
     ;
     ret_val14 = r1;
     ret_val13 = ret_val14;
     s = ret_val13;
     ret_val12 = STR_ap1077157958(plus_self7, s, TRUE);
     PROG_err_STR(ret_val8, ret_val12);
    }
    else {
     r = ELT_TB1703146017(r, er);
    }
   }
  }
  ew = IMPL_C872226022(self, t);
  L14 = (ew==((ELT) NULL));
  L151_=!(L14); 
  if (L151_) {
   f2 = ELT_TB1201943681(r, ew);
   L16 = (f2==((ELT) NULL));
   L171_=!(L16); 
   if (L171_) {
    writer_conflict_ = self;
    writer_conflict_1 = ew;
    writer_conflict_2 = f2;
    prog_self4 = writer_conflict_;
    ret_val15 = ATTR(ATTR(prog_self4,tp),prog);
    PROG_e176405615(ret_val15, ((dPROG_ERR) ATTR(writer_conflict_1,as)));
    prog_self5 = writer_conflict_;
    ret_val16 = ATTR(ATTR(prog_self5,tp),prog);
    plus_self8 = ((STR) &Thesignature1);
    plus_sarg7 = ATTR(ATTR(writer_conflict_1,sig1),str);
    ret_val17 = STR_ap2004550586(plus_self8, plus_sarg7);
    plus_self9 = ret_val17;
    plus_sarg8 = ((STR) &ofthew1419133383);
    ret_val18 = STR_ap2004550586(plus_self9, plus_sarg8);
    plus_self10 = ret_val18;
    plus_sarg9 = ATTR(ATTR(writer_conflict_2,sig1),str);
    ret_val19 = STR_ap2004550586(plus_self10, plus_sarg9);
    plus_self11 = ret_val19;
    plus_arg1 = '.';
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
    L19 = 1;
    L22=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L19)*sizeof(CHAR);
    L20=ZALLOC_LEAF_BIG(L22);
    if (L20==NULL) FATAL("Unable to allocate more memory");
    memset(L20,0,L22);
    ((OB)L20)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L20)->header.destroyed=0;
#endif

    L18 = ((STR) L20);
    L18->asize = L19;
    r2 = L18;
    SARR((STR)r2,0,create_c1); 
    ;
    ret_val22 = r2;
    ret_val21 = ret_val22;
    s1 = ret_val21;
    ret_val20 = STR_ap1077157958(plus_self11, s1, TRUE);
    PROG_err_STR(ret_val16, ret_val20);
   }
   else {
    r = ELT_TB1703146017(r, ew);
   }
  }
  L24 = t;
  t = (*dAS_CL760354380[TAG(L24)])(L24);
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL IMPL_C1426629505(IMPL_CREATE self) {
 ELT_TBL ret_val;
 ELT_TBL r = ((ELT_TBL) NULL);
 ELT_TBL L11;
 IMPL_INCLUDE inc;
 ELT_TBL L21;
 ELT e;
 ELT f;
 FLISTI1895665739 L31;
 IMPL_INCLUDE inc2;
 ELT g;
 ELT_TBL L41;
 ELT e1;
 FLISTI1895665739 is_empty_self;
 BOOL ret_val1 = BOOL_zero;
 TP_CLASS is_partial_self;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val3 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val4;
 TP_CLASS is_partial_self1;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS kind_self1;
 INT ret_val6 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val7;
 TP_CLASS is_partial_self2;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS kind_self2;
 INT ret_val9 = INT_zero;
 TP_CLASS as_tree_self2;
 AS_CLASS_DEF ret_val10;
 TP_CLASS is_partial_self3;
 BOOL ret_val11 = BOOL_zero;
 TP_CLASS kind_self3;
 INT ret_val12 = INT_zero;
 TP_CLASS as_tree_self3;
 AS_CLASS_DEF ret_val13;
 TP_CLASS is_partial_self4;
 BOOL ret_val14 = BOOL_zero;
 TP_CLASS kind_self4;
 INT ret_val15 = INT_zero;
 TP_CLASS as_tree_self4;
 AS_CLASS_DEF ret_val16;
 TP_CLASS is_partial_self5;
 BOOL ret_val17 = BOOL_zero;
 TP_CLASS kind_self5;
 INT ret_val18 = INT_zero;
 TP_CLASS as_tree_self5;
 AS_CLASS_DEF ret_val19;
 TP_CLASS is_external_self;
 BOOL ret_val20 = BOOL_zero;
 TP_CLASS kind_self6;
 INT ret_val21 = INT_zero;
 TP_CLASS as_tree_self6;
 AS_CLASS_DEF ret_val22;
 TP_CLASS kind_self7;
 INT ret_val23 = INT_zero;
 TP_CLASS as_tree_self7;
 AS_CLASS_DEF ret_val24;
 IMPL_CREATE undef_stub_err_s;
 ELT undef_stub_err_e;
 IMPL_CREATE prog_self;
 PROG ret_val25;
 IMPL_CREATE prog_self1;
 PROG ret_val26;
 STR plus_self;
 STR plus_sarg;
 STR ret_val27;
 ELT L5;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 ELT L10;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L15;
 INT L16;
 INT L17;
 BOOL L181_;
 dPARSE L19;
 IDENT L20;
 ARRAYdTP L22;
 INT L231_;
 dPARSE L24;
 BOOL L25;
 BOOL L261_;
 INT L27;
 INT L28;
 BOOL L291_;
 BOOL L30;
 INT L32;
 INT L33;
 BOOL L341_;
 dPARSE L35;
 IDENT L36;
 ARRAYdTP L37;
 INT L381_;
 dPARSE L39;
 BOOL L40;
 BOOL L421_;
 INT L43;
 INT L44;
 BOOL L451_;
 BOOL L46;
 BOOL L471_;
 BOOL L48;
 BOOL L49;
 BOOL L50;
 BOOL L511_;
 BOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L55;
 BOOL L56;
 BOOL L57;
 BOOL L581_;
 INT L59;
 INT L60;
 BOOL L611_;
 dPARSE L62;
 IDENT L63;
 ARRAYdTP L64;
 INT L651_;
 dPARSE L66;
 BOOL L67;
 BOOL L681_;
 INT L69;
 INT L70;
 BOOL L721_;
 IMPL_INCLUDE L73;
 BOOL L74;
 BOOL L751_;
 BOOL L76;
 BOOL L77;
 INT L78;
 INT L79;
 BOOL L801_;
 dPARSE L81;
 IDENT L82;
 ARRAYdTP L83;
 INT L841_;
 dPARSE L85;
 BOOL L86;
 BOOL L871_;
 INT L88;
 INT L89;
 BOOL L901_;
 BOOL L92;
 INT L93;
 INT L94;
 BOOL L951_;
 dPARSE L96;
 IDENT L97;
 ARRAYdTP L98;
 INT L991_;
 dPARSE L100;
 BOOL L101;
 BOOL L1021_;
 INT L103;
 INT L104;
 BOOL L1051_;
 BOOL L106;
 BOOL L1071_;
 BOOL L108;
 BOOL L1091_;
 BOOL L110;
 BOOL L1111_;
 BOOL L112;
 BOOL L113;
 BOOL L1141_;
 BOOL L115;
 BOOL L116;
 INT L117;
 INT L118;
 BOOL L1191_;
 dPARSE L120;
 IDENT L121;
 ARRAYdTP L122;
 INT L1231_;
 dPARSE L124;
 BOOL L125;
 BOOL L1261_;
 INT L127;
 INT L128;
 BOOL L1291_;
 BOOL L130;
 BOOL L1321_;
 BOOL L133;
 BOOL L1341_;
 BOOL L135;
 INT L136;
 INT L137;
 BOOL L1381_;
 dPARSE L139;
 IDENT L140;
 ARRAYdTP L141;
 INT L1421_;
 dPARSE L143;
 BOOL L144;
 BOOL L1451_;
 INT L146;
 INT L147;
 BOOL L1481_;
 INT L149;
 INT L150;
 BOOL L1511_;
 dPARSE L152;
 IDENT L153;
 ARRAYdTP L154;
 INT L1551_;
 dPARSE L156;
 BOOL L157;
 BOOL L1581_;
 INT L159;
 INT L160;
 BOOL L1611_;
 BOOL L162;
 BOOL L1631_;
 ELT L164;
 extern STR Unimpl1918572407;
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = ATTR(self,class_elts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L5 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = ELT_TB1703146017(r, L5);
  }
 }
 after_loop: ;
 while (1) {
  is_empty_self = ATTR(self,incs);
  L6 = FLISTI377878565(is_empty_self);
  L71_=(L6)==(0); 
  ret_val1 = L71_;
  L8 = ret_val1;
  L91_=!(L8); 
  if (L91_) {
  }
  else {
   goto after_loop1;
  }
  inc = FLISTI961811375(ATTR(self,incs));
  {
   struct ELT_TBL_eltbrELT_frame_struct other3_0;
ELT_TBL_eltbrELT_frame noname2 = &other3_0;
   L21 = ATTR(inc,elt_tbl);
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    L10 = ELT_TBL_eltbrELT(noname2);
    if (noname2->state == -1) {
     goto after_loop2;
    }
    e = L10;
    f = ELT_TB1201943681(r, e);
    L12 = (f==((ELT) NULL));
    L131_=!(L12); 
    if (L131_) {
     is_partial_self = ATTR(inc,itp);
     kind_self = is_partial_self;
     L16 = ATTR(kind_self,kind_cache);
     L17 = TP_KIN138039446;
     L181_=(L16)==(L17); 
     if (L181_) {
      as_tree_self = kind_self;
      if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
       L19 = ATTR(ATTR(as_tree_self,prog),parse);
       L20 = ATTR(as_tree_self,name1);
       L22 = ATTR(as_tree_self,params);
       L231_=(L22)==NULL?0:ASIZE((ARRAYdTP)L22); 
       L24 = L19;
       SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L24)])(L24, L20, L231_));
      }
      ret_val4 = ATTR(as_tree_self,my_as_tree);
      L25 = (ret_val4==((AS_CLASS_DEF) NULL));
      L261_=!(L25); 
      if (L261_) {
       SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
      }
     }
     ret_val3 = ATTR(kind_self,kind_cache);
     L27 = ret_val3;
     L28 = TP_KIND_part_tp;
     L291_=(L27)==(L28); 
     ret_val2 = L291_;
     if (ret_val2) {
      L15 = ELT_is1907475257(e);
     } else {
      L15 = FALSE;
     }
     if (L15) {
      L14 = TRUE;
     } else {
      is_partial_self1 = ATTR(self,tp);
      kind_self1 = is_partial_self1;
      L32 = ATTR(kind_self1,kind_cache);
      L33 = TP_KIN138039446;
      L341_=(L32)==(L33); 
      if (L341_) {
       as_tree_self1 = kind_self1;
       if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
        L35 = ATTR(ATTR(as_tree_self1,prog),parse);
        L36 = ATTR(as_tree_self1,name1);
        L37 = ATTR(as_tree_self1,params);
        L381_=(L37)==NULL?0:ASIZE((ARRAYdTP)L37); 
        L39 = L35;
        SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L39)])(L39, L36, L381_));
       }
       ret_val7 = ATTR(as_tree_self1,my_as_tree);
       L40 = (ret_val7==((AS_CLASS_DEF) NULL));
       L421_=!(L40); 
       if (L421_) {
        SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
       }
      }
      ret_val6 = ATTR(kind_self1,kind_cache);
      L43 = ret_val6;
      L44 = TP_KIND_part_tp;
      L451_=(L43)==(L44); 
      ret_val5 = L451_;
      if (ret_val5) {
       L30 = ELT_is1907475257(f);
      } else {
       L30 = FALSE;
      }
      L14 = L30;
     }
     L46 = L14;
     L471_=!(L46); 
     if (L471_) {
      if (ELT_is1300999700(e)) {
       L50 = ELT_is1300999700(f);
       L511_=!(L50); 
       L49 = L511_;
      } else {
       L49 = FALSE;
      }
      if (L49) {
       L52 = ELT_is1907475257(f);
       L531_=!(L52); 
       L48 = L531_;
      } else {
       L48 = FALSE;
      }
      if (L48) {
       IMPL_C170915365(self, e, f, inc);
      }
     }
    }
    if ((f==((ELT) NULL))) {
     L57 = ELT_is1907475257(e);
     L581_=!(L57); 
     if (L581_) {
      L56 = TRUE;
     } else {
      L56 = TP_CLA1568240525(ATTR(self,tp));
     }
     if (L56) {
      L55 = TRUE;
     } else {
      is_partial_self2 = ATTR(inc,itp);
      kind_self2 = is_partial_self2;
      L59 = ATTR(kind_self2,kind_cache);
      L60 = TP_KIN138039446;
      L611_=(L59)==(L60); 
      if (L611_) {
       as_tree_self2 = kind_self2;
       if ((ATTR(as_tree_self2,my_as_tree)==((AS_CLASS_DEF) NULL))) {
        L62 = ATTR(ATTR(as_tree_self2,prog),parse);
        L63 = ATTR(as_tree_self2,name1);
        L64 = ATTR(as_tree_self2,params);
        L651_=(L64)==NULL?0:ASIZE((ARRAYdTP)L64); 
        L66 = L62;
        SATTR(as_tree_self2,my_as_tree,(*dPARSE1581792481[TAG(L66)])(L66, L63, L651_));
       }
       ret_val10 = ATTR(as_tree_self2,my_as_tree);
       L67 = (ret_val10==((AS_CLASS_DEF) NULL));
       L681_=!(L67); 
       if (L681_) {
        SATTR(kind_self2,kind_cache,ATTR(ATTR(kind_self2,my_as_tree),kind));
       }
      }
      ret_val9 = ATTR(kind_self2,kind_cache);
      L69 = ret_val9;
      L70 = TP_KIND_part_tp;
      L721_=(L69)==(L70); 
      ret_val8 = L721_;
      L55 = ret_val8;
     }
     L54 = L55;
    } else {
     L54 = FALSE;
    }
    if (L54) {
     {
      struct FLISTI381429157_frame_struct other4_0;
FLISTI381429157_frame noname3 = &other4_0;
      L31 = ATTR(self,incs);
      noname3->self = L31;
      noname3->state = 0;
      while (1) {
       L73 = FLISTI381429157(noname3);
       if (noname3->state == -1) {
        goto after_loop3;
       }
       inc2 = L73;
       g = ELT_TB1201943681(ATTR(inc2,elt_tbl), e);
       L74 = (g==((ELT) NULL));
       L751_=!(L74); 
       if (L751_) {
        is_partial_self3 = ATTR(inc,itp);
        kind_self3 = is_partial_self3;
        L78 = ATTR(kind_self3,kind_cache);
        L79 = TP_KIN138039446;
        L801_=(L78)==(L79); 
        if (L801_) {
         as_tree_self3 = kind_self3;
         if ((ATTR(as_tree_self3,my_as_tree)==((AS_CLASS_DEF) NULL))) {
          L81 = ATTR(ATTR(as_tree_self3,prog),parse);
          L82 = ATTR(as_tree_self3,name1);
          L83 = ATTR(as_tree_self3,params);
          L841_=(L83)==NULL?0:ASIZE((ARRAYdTP)L83); 
          L85 = L81;
          SATTR(as_tree_self3,my_as_tree,(*dPARSE1581792481[TAG(L85)])(L85, L82, L841_));
         }
         ret_val13 = ATTR(as_tree_self3,my_as_tree);
         L86 = (ret_val13==((AS_CLASS_DEF) NULL));
         L871_=!(L86); 
         if (L871_) {
          SATTR(kind_self3,kind_cache,ATTR(ATTR(kind_self3,my_as_tree),kind));
         }
        }
        ret_val12 = ATTR(kind_self3,kind_cache);
        L88 = ret_val12;
        L89 = TP_KIND_part_tp;
        L901_=(L88)==(L89); 
        ret_val11 = L901_;
        if (ret_val11) {
         L77 = ELT_is1907475257(e);
        } else {
         L77 = FALSE;
        }
        if (L77) {
         L76 = TRUE;
        } else {
         is_partial_self4 = ATTR(inc2,itp);
         kind_self4 = is_partial_self4;
         L93 = ATTR(kind_self4,kind_cache);
         L94 = TP_KIN138039446;
         L951_=(L93)==(L94); 
         if (L951_) {
          as_tree_self4 = kind_self4;
          if ((ATTR(as_tree_self4,my_as_tree)==((AS_CLASS_DEF) NULL))) {
           L96 = ATTR(ATTR(as_tree_self4,prog),parse);
           L97 = ATTR(as_tree_self4,name1);
           L98 = ATTR(as_tree_self4,params);
           L991_=(L98)==NULL?0:ASIZE((ARRAYdTP)L98); 
           L100 = L96;
           SATTR(as_tree_self4,my_as_tree,(*dPARSE1581792481[TAG(L100)])(L100, L97, L991_));
          }
          ret_val16 = ATTR(as_tree_self4,my_as_tree);
          L101 = (ret_val16==((AS_CLASS_DEF) NULL));
          L1021_=!(L101); 
          if (L1021_) {
           SATTR(kind_self4,kind_cache,ATTR(ATTR(kind_self4,my_as_tree),kind));
          }
         }
         ret_val15 = ATTR(kind_self4,kind_cache);
         L103 = ret_val15;
         L104 = TP_KIND_part_tp;
         L1051_=(L103)==(L104); 
         ret_val14 = L1051_;
         if (ret_val14) {
          L92 = ELT_is1907475257(g);
         } else {
          L92 = FALSE;
         }
         L76 = L92;
        }
        L106 = L76;
        L1071_=!(L106); 
        if (L1071_) {
         IMPL_C1750830100(self, inc, inc2, e, g);
        }
       }
      }
     }
     after_loop3: ;
     r = ELT_TB1703146017(r, e);
    }
    L108 = TP_CLA1568240525(ATTR(self,tp));
    L1091_=!(L108); 
    if (L1091_) {
     L110 = (f==((ELT) NULL));
     L1111_=!(L110); 
     if (L1111_) {
      if (ELT_is1907475257(f)) {
       L113 = ELT_is1907475257(e);
       L1141_=!(L113); 
       L112 = L1141_;
      } else {
       L112 = FALSE;
      }
      if (L112) {
       r = ELT_TB1573869061(r, f);
       r = ELT_TB1703146017(r, e);
      }
     }
    }
   }
  }
  after_loop2: ;
 }
 after_loop1: ;
 is_partial_self5 = ATTR(self,tp);
 kind_self5 = is_partial_self5;
 L117 = ATTR(kind_self5,kind_cache);
 L118 = TP_KIN138039446;
 L1191_=(L117)==(L118); 
 if (L1191_) {
  as_tree_self5 = kind_self5;
  if ((ATTR(as_tree_self5,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L120 = ATTR(ATTR(as_tree_self5,prog),parse);
   L121 = ATTR(as_tree_self5,name1);
   L122 = ATTR(as_tree_self5,params);
   L1231_=(L122)==NULL?0:ASIZE((ARRAYdTP)L122); 
   L124 = L120;
   SATTR(as_tree_self5,my_as_tree,(*dPARSE1581792481[TAG(L124)])(L124, L121, L1231_));
  }
  ret_val19 = ATTR(as_tree_self5,my_as_tree);
  L125 = (ret_val19==((AS_CLASS_DEF) NULL));
  L1261_=!(L125); 
  if (L1261_) {
   SATTR(kind_self5,kind_cache,ATTR(ATTR(kind_self5,my_as_tree),kind));
  }
 }
 ret_val18 = ATTR(kind_self5,kind_cache);
 L127 = ret_val18;
 L128 = TP_KIND_part_tp;
 L1291_=(L127)==(L128); 
 ret_val17 = L1291_;
 L130 = ret_val17;
 L1321_=!(L130); 
 if (L1321_) {
  L133 = TP_CLA1568240525(ATTR(self,tp));
  L1341_=!(L133); 
  L116 = L1341_;
 } else {
  L116 = FALSE;
 }
 if (L116) {
  is_external_self = ATTR(self,tp);
  kind_self6 = is_external_self;
  L136 = ATTR(kind_self6,kind_cache);
  L137 = TP_KIN138039446;
  L1381_=(L136)==(L137); 
  if (L1381_) {
   as_tree_self6 = kind_self6;
   if ((ATTR(as_tree_self6,my_as_tree)==((AS_CLASS_DEF) NULL))) {
    L139 = ATTR(ATTR(as_tree_self6,prog),parse);
    L140 = ATTR(as_tree_self6,name1);
    L141 = ATTR(as_tree_self6,params);
    L1421_=(L141)==NULL?0:ASIZE((ARRAYdTP)L141); 
    L143 = L139;
    SATTR(as_tree_self6,my_as_tree,(*dPARSE1581792481[TAG(L143)])(L143, L140, L1421_));
   }
   ret_val22 = ATTR(as_tree_self6,my_as_tree);
   L144 = (ret_val22==((AS_CLASS_DEF) NULL));
   L1451_=!(L144); 
   if (L1451_) {
    SATTR(kind_self6,kind_cache,ATTR(ATTR(kind_self6,my_as_tree),kind));
   }
  }
  ret_val21 = ATTR(kind_self6,kind_cache);
  L146 = ret_val21;
  L147 = TP_KIND_ext_c_tp;
  L1481_=(L146)==(L147); 
  if (L1481_) {
   L135 = TRUE;
  } else {
   kind_self7 = is_external_self;
   L149 = ATTR(kind_self7,kind_cache);
   L150 = TP_KIN138039446;
   L1511_=(L149)==(L150); 
   if (L1511_) {
    as_tree_self7 = kind_self7;
    if ((ATTR(as_tree_self7,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L152 = ATTR(ATTR(as_tree_self7,prog),parse);
     L153 = ATTR(as_tree_self7,name1);
     L154 = ATTR(as_tree_self7,params);
     L1551_=(L154)==NULL?0:ASIZE((ARRAYdTP)L154); 
     L156 = L152;
     SATTR(as_tree_self7,my_as_tree,(*dPARSE1581792481[TAG(L156)])(L156, L153, L1551_));
    }
    ret_val24 = ATTR(as_tree_self7,my_as_tree);
    L157 = (ret_val24==((AS_CLASS_DEF) NULL));
    L1581_=!(L157); 
    if (L1581_) {
     SATTR(kind_self7,kind_cache,ATTR(ATTR(kind_self7,my_as_tree),kind));
    }
   }
   ret_val23 = ATTR(kind_self7,kind_cache);
   L159 = ret_val23;
   L160 = TP_KIN462481068;
   L1611_=(L159)==(L160); 
   L135 = L1611_;
  }
  ret_val20 = L135;
  L162 = ret_val20;
  L1631_=!(L162); 
  L115 = L1631_;
 } else {
  L115 = FALSE;
 }
 if (L115) {
  {
   struct ELT_TBL_eltbrELT_frame_struct other5_0;
ELT_TBL_eltbrELT_frame noname4 = &other5_0;
   L41 = r;
   noname4->self = L41;
   noname4->state = 0;
   while (1) {
    L164 = ELT_TBL_eltbrELT(noname4);
    if (noname4->state == -1) {
     goto after_loop4;
    }
    e1 = L164;
    if (ELT_is1907475257(e1)) {
     undef_stub_err_s = self;
     undef_stub_err_e = e1;
     prog_self = undef_stub_err_s;
     ret_val25 = ATTR(ATTR(prog_self,tp),prog);
     PROG_e176405615(ret_val25, ((dPROG_ERR) ATTR(undef_stub_err_s,as)));
     prog_self1 = undef_stub_err_s;
     ret_val26 = ATTR(ATTR(prog_self1,tp),prog);
     plus_self = ((STR) &Unimpl1918572407);
     plus_sarg = ATTR(ATTR(undef_stub_err_e,sig1),str);
     ret_val27 = STR_ap2004550586(plus_self, plus_sarg);
     PROG_err_STR(ret_val26, ret_val27);
    }
   }
  }
  after_loop4: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL IMPL_I1774855049(IMPL_INCLUDE self) {
 ELT_TBL ret_val;
 ELT_TBL r = ((ELT_TBL) NULL);
 ELT_TBL L11;
 ELT e;
 ELT en;
 ELT f;
 AS_FEAT_MOD m;
 IMPL_INCLUDE include_conflict;
 ELT include_conflict1;
 ELT include_conflict2;
 IMPL_INCLUDE prog_self;
 PROG ret_val1;
 IMPL_INCLUDE prog_self1;
 PROG ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 IMPL_INCLUDE unused_mod_err_s;
 AS_FEAT_MOD unused_mod_err_t;
 IMPL_INCLUDE prog_self2;
 PROG ret_val7;
 IMPL_INCLUDE prog_self3;
 PROG ret_val8;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 ELT L2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR Twooft290023832;
 extern STR and3;
 extern STR name18;
 INT L7;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR Therea1644237726;
 extern STR name18;
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,impl),elts);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L2;
   en = IMPL_I954938038(self, e);
   L3 = (en==((ELT) NULL));
   L41_=!(L3); 
   if (L41_) {
    f = ELT_TB1201943681(r, en);
    L5 = (f==((ELT) NULL));
    L61_=!(L5); 
    if (L61_) {
     include_conflict = self;
     include_conflict1 = en;
     include_conflict2 = f;
     prog_self = include_conflict;
     ret_val1 = ATTR(ATTR(prog_self,tp),prog);
     PROG_e176405615(ret_val1, ((dPROG_ERR) ATTR(include_conflict,as)));
     prog_self1 = include_conflict;
     ret_val2 = ATTR(ATTR(prog_self1,tp),prog);
     plus_self = ((STR) &Twooft290023832);
     plus_sarg = ATTR(ATTR(include_conflict1,sig1),str);
     ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val3;
     plus_sarg1 = ((STR) &and3);
     ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val4;
     plus_sarg2 = ATTR(ATTR(include_conflict2,sig1),str);
     ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val5;
     plus_sarg3 = ((STR) &name18);
     ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(ret_val2, ret_val6);
    }
    else {
     r = ELT_TB1703146017(r, en);
    }
   }
  }
 }
 after_loop: ;
 L7 = FSETAS77357633(ATTR(self,used_mods));
 L8 = AS_FEA1447190869(ATTR(ATTR(self,as),mods));
 L91_=(L7)==(L8); 
 L10 = L91_;
 L121_=!(L10); 
 if (L121_) {
  m = ATTR(ATTR(self,as),mods);
  while (1) {
   if ((m==((AS_FEAT_MOD) NULL))) {
    goto after_loop1;
   }
   L13 = FSETAS456491640(ATTR(self,used_mods), m);
   L141_=!(L13); 
   if (L141_) {
    unused_mod_err_s = self;
    unused_mod_err_t = m;
    prog_self2 = unused_mod_err_s;
    ret_val7 = ATTR(ATTR(prog_self2,tp),prog);
    PROG_e176405615(ret_val7, ((dPROG_ERR) unused_mod_err_t));
    prog_self3 = unused_mod_err_s;
    ret_val8 = ATTR(ATTR(prog_self3,tp),prog);
    plus_self4 = ((STR) &Therea1644237726);
    plus_sarg4 = ATTR(unused_mod_err_t,name1).str;
    ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val9;
    plus_sarg5 = ((STR) &name18);
    ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
    PROG_err_STR(ret_val8, ret_val10);
   }
   m = ATTR(m,next);
  }
  after_loop1: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL IMPL_C12545525(IMPL_CREATE self, TP_CLASS t, TP_CONTEXT context) {
 IMPL ret_val;
 IMPL_CREATE ic;
 IMPL r;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r2;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r5;
 TP_CLASS is_external_self;
 BOOL ret_val11 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val12 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val13;
 TP_CLASS kind_self1;
 INT ret_val14 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val15;
 TP_CLASS as_tree_self2;
 AS_CLASS_DEF ret_val16;
 IMPL create_self1;
 IMPL ret_val17;
 IFC create_self2;
 SIG_TBL create_sigs;
 dTP create_tp;
 IFC ret_val18;
 IFC r6;
 extern STR Implcreate;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR Context;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L11;
 extern STR name9;
 FILE1 L13;
 OB L14;
 IMPL_CREATE L16;
 OB L17;
 BOOL L18;
 INT L19;
 INT L20;
 BOOL L211_;
 dPARSE L22;
 IDENT L23;
 ARRAYdTP L24;
 INT L251_;
 dPARSE L26;
 BOOL L27;
 BOOL L281_;
 INT L29;
 INT L30;
 BOOL L321_;
 INT L33;
 INT L34;
 BOOL L351_;
 dPARSE L36;
 IDENT L37;
 ARRAYdTP L38;
 INT L391_;
 dPARSE L40;
 BOOL L41;
 BOOL L421_;
 INT L43;
 INT L44;
 BOOL L451_;
 dPARSE L46;
 IDENT L47;
 ARRAYdTP L48;
 INT L491_;
 dPARSE L50;
 IMPL L51;
 OB L52;
 IFC L53;
 OB L54;
 if (ATTR(ATTR(t,prog),show_impl_create)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Implcreate);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r1 = L1;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = TP_CLASS_strrSTR(t);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r2 = L4;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &Context);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r3 = L7;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = TP_CON702863562(context);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r4 = L10;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s3);
  ret_val8 = plus_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name9);
  stdout_self4 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r5 = L13;
  
  SATTR(r5,fp,stdout);
  ret_val10 = r5;
  FILE_plus_STR(ret_val10, plus_s4);
 }
 L17=ZALLOC(sizeof(struct IMPL_CREATE_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=IMPL_CREATE_tag;
 L16 = ((IMPL_CREATE) L17);
 ic = L16;
 SATTR(ic,tp,t);
 is_external_self = t;
 kind_self = is_external_self;
 L19 = ATTR(kind_self,kind_cache);
 L20 = TP_KIN138039446;
 L211_=(L19)==(L20); 
 if (L211_) {
  as_tree_self = kind_self;
  if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L22 = ATTR(ATTR(as_tree_self,prog),parse);
   L23 = ATTR(as_tree_self,name1);
   L24 = ATTR(as_tree_self,params);
   L251_=(L24)==NULL?0:ASIZE((ARRAYdTP)L24); 
   L26 = L22;
   SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L26)])(L26, L23, L251_));
  }
  ret_val13 = ATTR(as_tree_self,my_as_tree);
  L27 = (ret_val13==((AS_CLASS_DEF) NULL));
  L281_=!(L27); 
  if (L281_) {
   SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
  }
 }
 ret_val12 = ATTR(kind_self,kind_cache);
 L29 = ret_val12;
 L30 = TP_KIND_ext_c_tp;
 L321_=(L29)==(L30); 
 if (L321_) {
  L18 = TRUE;
 } else {
  kind_self1 = is_external_self;
  L33 = ATTR(kind_self1,kind_cache);
  L34 = TP_KIN138039446;
  L351_=(L33)==(L34); 
  if (L351_) {
   as_tree_self1 = kind_self1;
   if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
    L36 = ATTR(ATTR(as_tree_self1,prog),parse);
    L37 = ATTR(as_tree_self1,name1);
    L38 = ATTR(as_tree_self1,params);
    L391_=(L38)==NULL?0:ASIZE((ARRAYdTP)L38); 
    L40 = L36;
    SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L40)])(L40, L37, L391_));
   }
   ret_val15 = ATTR(as_tree_self1,my_as_tree);
   L41 = (ret_val15==((AS_CLASS_DEF) NULL));
   L421_=!(L41); 
   if (L421_) {
    SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
   }
  }
  ret_val14 = ATTR(kind_self1,kind_cache);
  L43 = ret_val14;
  L44 = TP_KIN462481068;
  L451_=(L43)==(L44); 
  L18 = L451_;
 }
 ret_val11 = L18;
 SATTR(ic,is_external,ret_val11);
 SATTR(ic,con,context);
 if ((ATTR(ic,con)==((TP_CONTEXT) NULL))) {
  ret_val = ((IMPL) NULL);
  return ret_val;
 }
 as_tree_self2 = t;
 if ((ATTR(as_tree_self2,my_as_tree)==((AS_CLASS_DEF) NULL))) {
  L46 = ATTR(ATTR(as_tree_self2,prog),parse);
  L47 = ATTR(as_tree_self2,name1);
  L48 = ATTR(as_tree_self2,params);
  L491_=(L48)==NULL?0:ASIZE((ARRAYdTP)L48); 
  L50 = L46;
  SATTR(as_tree_self2,my_as_tree,(*dPARSE1581792481[TAG(L50)])(L50, L47, L491_));
 }
 ret_val16 = ATTR(as_tree_self2,my_as_tree);
 SATTR(ic,as,ret_val16);
 if ((ATTR(ic,as)==((AS_CLASS_DEF) NULL))) {
  ret_val = ((IMPL) NULL);
  return ret_val;
 }
 SATTR(ic,class_elts,IMPL_C1114329143(ic));
 IMPL_C1885190518(ic);
 create_self1 = ((IMPL) NULL);
 L52=ZALLOC(sizeof(struct IMPL_struct));
 if (L52==NULL) FATAL("Unable to allocate more memory");
 ((OB)L52)->header.tag=IMPL_tag;
 L51 = ((IMPL) L52);
 ret_val17 = L51;
 r = ret_val17;
 SATTR(r,tp,((dTP) t));
 SATTR(r,arr,IMPL_C639524789(ic));
 SATTR(r,elts,IMPL_C1426629505(ic));
 SATTR(r,ifc,ELT_TB462529002(ATTR(r,elts)));
 if ((ATTR(r,ifc)==((IFC) NULL))) {
  create_self2 = ((IFC) NULL);
  create_sigs = ((SIG_TBL) NULL);
  create_tp = ((dTP) t);
  L54=ZALLOC(sizeof(struct IFC_struct));
  if (L54==NULL) FATAL("Unable to allocate more memory");
  ((OB)L54)->header.tag=IFC_tag;
  L53 = ((IFC) L54);
  r6 = L53;
  SATTR(r6,sigs,create_sigs);
  SATTR(r6,tp,create_tp);
  ret_val18 = r6;
  SATTR(r,ifc,ret_val18);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL IMPL_C629829570(IMPL_CREATE self, TP_CLASS t) {
 IMPL ret_val;
 if (TP_CLA1568240525(t)) {
  ret_val = ((IMPL) NULL);
  return ret_val;
 }
 ret_val = IMPL_C12545525(self, t, TP_CLA821242177(t));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL_INCLUDE IMPL_I1190749135(IMPL_INCLUDE self, TP_CLASS tp, AS_INC1416917001 as, TP_CONTEXT context) {
 IMPL_INCLUDE ret_val;
 IMPL_INCLUDE r;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r2;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r5;
 IMPL_INCLUDE included_impl_se;
 IMPL ret_val11;
 TP_CONTEXT itp_context;
 TP_CONTEXT subcon;
 IMPL_INCLUDE prog_self;
 PROG ret_val12;
 IMPL_INCLUDE prog_self1;
 PROG ret_val13;
 TP_CONTEXT create_self1;
 TP_CLASS create_same;
 ARRAYIDENT create_pnames;
 ARRAYdTP create_ptypes;
 PROG create_prog;
 TP_CONTEXT ret_val14;
 TP_CONTEXT r6;
 IMPL_INCLUDE L1;
 OB L2;
 extern STR Including;
 FILE1 L3;
 OB L4;
 FILE1 L6;
 OB L7;
 extern STR in;
 FILE1 L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR name23;
 FILE1 L15;
 OB L16;
 TP_CONTEXT L18;
 OB L19;
 L2=ZALLOC(sizeof(struct IMPL_INCLUDE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IMPL_INCLUDE_tag;
 L1 = ((IMPL_INCLUDE) L2);
 r = L1;
 SATTR(r,tp,tp);
 SATTR(r,as,as);
 SATTR(r,con,context);
 SATTR(r,itp,IMPL_I517033114(r));
 if ((ATTR(r,itp)==((TP_CLASS) NULL))) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 if (ATTR(ATTR(tp,prog),show_include)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Including);
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r1 = L3;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = TP_CLASS_strrSTR(ATTR(r,itp));
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r2 = L6;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &in);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r3 = L9;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = TP_CLASS_strrSTR(tp);
  stdout_self3 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r4 = L12;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s3);
  ret_val8 = plus_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name23);
  stdout_self4 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r5 = L15;
  
  SATTR(r5,fp,stdout);
  ret_val10 = r5;
  FILE_plus_STR(ret_val10, plus_s4);
 }
 included_impl_se = r;
 prog_self = included_impl_se;
 ret_val12 = ATTR(ATTR(prog_self,tp),prog);
 PROG_e176405615(ret_val12, ((dPROG_ERR) ATTR(included_impl_se,as)));
 itp_context = TP_CLA821242177(ATTR(included_impl_se,itp));
 create_self1 = ((TP_CONTEXT) NULL);
 create_same = ATTR(ATTR(included_impl_se,con),same1);
 create_pnames = ATTR(itp_context,pnames);
 create_ptypes = ATTR(itp_context,ptypes);
 prog_self1 = included_impl_se;
 ret_val13 = ATTR(ATTR(prog_self1,tp),prog);
 create_prog = ret_val13;
 L19=ZALLOC(sizeof(struct TP_CONTEXT_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=TP_CONTEXT_tag;
 L18 = ((TP_CONTEXT) L19);
 r6 = L18;
 SATTR(r6,same1,create_same);
 SATTR(r6,pnames,create_pnames);
 SATTR(r6,ptypes,create_ptypes);
 SATTR(r6,prog,create_prog);
 ret_val14 = r6;
 subcon = ret_val14;
 ret_val11 = IMPL_C12545525(((IMPL_CREATE) NULL), ATTR(included_impl_se,itp), subcon);
 SATTR(r,impl,ret_val11);
 if ((ATTR(r,impl)==((IMPL) NULL))) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 if (IMPL_I1019512980(r)) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 if (IMPL_I1756162892(r)) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 SATTR(r,elt_tbl,IMPL_I1774855049(r));
 if ((ATTR(r,elt_tbl)==((ELT_TBL) NULL))) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE1433668901 INLINE433490271(INLINE1433668901 self, AM_ROUT_DEF am) {
 INLINE1433668901 ret_val;
 INLINE1433668901 r;
 dAM_STMT stmt1;
 dAM_EXPR val11;
 INLINE1433668901 L1;
 OB L2;
 INT L3;
 INT L41_;
 L2=ZALLOC(sizeof(struct INLINE1433668901_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=INLINE1433668901_tag;
 L1 = ((INLINE1433668901) L2);
 r = L1;
 SATTR(r,sig1,ATTR(am,sig1));
 SATTR(r,is_special,TRUE);
 stmt1 = ATTR(am,code1);
 switch (TAG(stmt1)) {
  case AM_RETURN_STMT_tag:
   val11 = ATTR(((AM_RETURN_STMT) stmt1),val1);
   switch (TAG(val11)) {
    case AM_SHARED_EXPR_tag:
     SATTR(r,tp_at,ATTR(((AM_SHARED_EXPR) val11),tp_at));
     SATTR(r,name1,ATTR(((AM_SHARED_EXPR) val11),name1));
     SATTR(r,class_tp,ATTR(((AM_SHARED_EXPR) val11),class_tp));
     SATTR(r,init,ATTR(((AM_SHARED_EXPR) val11),init));
     SATTR(r,is_const,ATTR(((AM_SHARED_EXPR) val11),is_const));
     SATTR(r,as_type,ATTR(((AM_SHARED_EXPR) val11),as_type)); break;
    default: ;
     FATAL("No applicable type in typecase\nin INLINE_GLOBAL_READ::create(AM_ROUT_DEF):SAME\n./Middle/inline.sa:205:20");
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin INLINE_GLOBAL_READ::create(AM_ROUT_DEF):SAME\n./Middle/inline.sa:203:16");
 }
 L3 = INLINE520918393;
 L41_=INTPLUS(L3,1); 
 INLINE520918393 = L41_;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INLINE1455768374 INLINE1362941708(INLINE1455768374 self, AM_ROUT_DEF am) {
 INLINE1455768374 ret_val;
 INLINE1455768374 r;
 dAM_STMT stmt1;
 dAM_EXPR dest1;
 INLINE1455768374 L1;
 OB L2;
 INT L3;
 INT L41_;
 L2=ZALLOC(sizeof(struct INLINE1455768374_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=INLINE1455768374_tag;
 L1 = ((INLINE1455768374) L2);
 r = L1;
 SATTR(r,sig1,ATTR(am,sig1));
 SATTR(r,is_special,TRUE);
 stmt1 = ATTR(am,code1);
 switch (TAG(stmt1)) {
  case AM_ASSIGN_STMT_tag:
   dest1 = ATTR(((AM_ASSIGN_STMT) stmt1),dest);
   switch (TAG(dest1)) {
    case AM_SHARED_EXPR_tag:
     SATTR(r,tp_at,ATTR(((AM_SHARED_EXPR) dest1),tp_at));
     SATTR(r,name1,ATTR(((AM_SHARED_EXPR) dest1),name1));
     SATTR(r,class_tp,ATTR(((AM_SHARED_EXPR) dest1),class_tp));
     SATTR(r,init,ATTR(((AM_SHARED_EXPR) dest1),init));
     SATTR(r,is_const,ATTR(((AM_SHARED_EXPR) dest1),is_const));
     SATTR(r,as_type,ATTR(((AM_SHARED_EXPR) dest1),as_type)); break;
    default: ;
     FATAL("No applicable type in typecase\nin INLINE_GLOBAL_WRITE::create(AM_ROUT_DEF):SAME\n./Middle/inline.sa:261:20");
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin INLINE_GLOBAL_WRITE::create(AM_ROUT_DEF):SAME\n./Middle/inline.sa:259:16");
 }
 L3 = INLINE545315961;
 L41_=INTPLUS(L3,1); 
 INLINE545315961 = L41_;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1060094617(INLINE_ATTR_READ self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1204485066(INLINE1433668901 self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1283359029(INLINE1455768374 self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

SIG INLINE1360700008(INLINE1596311811 self) {
 return ATTR(self,sig1);
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS IMPL_C639524789(IMPL_CREATE self) {
 TP_CLASS ret_val;
 TP_CLASS r = ((TP_CLASS) NULL);
 FLISTI1895665739 L11;
 IMPL_INCLUDE inc;
 TP_CLASS a;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self2;
 OB is_eq_t;
 BOOL ret_val3 = BOOL_zero;
 BOOL L2;
 ARRAYdTP L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 BOOL L7;
 ARRAYdTP L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 IMPL_INCLUDE L13;
 BOOL L14;
 BOOL L151_;
 is_eq_self = ATTR(ATTR(self,tp),name1);
 is_eq_i = IDENT_1666445894;
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 if (ret_val1) {
  L3 = ATTR(ATTR(self,tp),params);
  L41_=(L3)==NULL?0:ASIZE((ARRAYdTP)L3); 
  L5 = L41_;
  L61_=(L5)==(1); 
  L2 = L61_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  ret_val = ATTR(self,tp);
  return ret_val;
 }
 is_eq_self1 = ATTR(ATTR(self,tp),name1);
 is_eq_i1 = IDENT_1485568624;
 ret_val2 = (is_eq_self1.str==is_eq_i1.str);
 if (ret_val2) {
  L8 = ATTR(ATTR(self,tp),params);
  L91_=(L8)==NULL?0:ASIZE((ARRAYdTP)L8); 
  L10 = L91_;
  L121_=(L10)==(1); 
  L7 = L121_;
 } else {
  L7 = FALSE;
 }
 if (L7) {
  ret_val = ATTR(self,tp);
  return ret_val;
 }
 {
  struct FLISTI381429157_frame_struct other1_0;
FLISTI381429157_frame noname1 = &other1_0;
  L11 = ATTR(self,incs);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L13 = FLISTI381429157(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   inc = L13;
   a = ATTR(ATTR(inc,impl),arr);
   L14 = (a==((TP_CLASS) NULL));
   L151_=!(L14); 
   if (L151_) {
    if ((r==((TP_CLASS) NULL))) {
     r = a;
    }
    else {
     is_eq_self2 = r;
     is_eq_t = ((OB) a);
     ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t);
     if (ret_val3) {
     }
     else {
      IMPL_C228209176(self, inc, r, a);
     }
    }
   }
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS IMPL_I517033114(IMPL_INCLUDE self) {
 TP_CLASS ret_val;
 dTP r;
 IMPL_INCLUDE param_include_er;
 dAS_CLASS_ELT param_include_er1;
 IMPL_INCLUDE prog_self;
 PROG ret_val1;
 IMPL_INCLUDE prog_self1;
 PROG ret_val2;
 IMPL_INCLUDE prog_self2;
 PROG ret_val3;
 IMPL_INCLUDE prog_self3;
 PROG ret_val4;
 TP_CLASS is_external_self;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val6 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val7;
 TP_CLASS kind_self1;
 INT ret_val8 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val9;
 IMPL_INCLUDE include_ext_err_;
 dAS_CLASS_ELT include_ext_err_1;
 IMPL_INCLUDE prog_self4;
 PROG ret_val10;
 IMPL_INCLUDE prog_self5;
 PROG ret_val11;
 IMPL_INCLUDE include_bound_er;
 dAS_CLASS_ELT include_bound_er1;
 IMPL_INCLUDE prog_self6;
 PROG ret_val12;
 IMPL_INCLUDE prog_self7;
 PROG ret_val13;
 IMPL_INCLUDE include_bound_er2;
 dAS_CLASS_ELT include_bound_er3;
 IMPL_INCLUDE prog_self8;
 PROG ret_val14;
 IMPL_INCLUDE prog_self9;
 PROG ret_val15;
 extern STR Typesp941653765;
 extern STR Classe21075321;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 dPARSE L5;
 IDENT L6;
 ARRAYdTP L7;
 INT L81_;
 dPARSE L9;
 BOOL L10;
 BOOL L111_;
 INT L12;
 INT L13;
 BOOL L141_;
 INT L15;
 INT L16;
 BOOL L171_;
 dPARSE L18;
 IDENT L19;
 ARRAYdTP L20;
 INT L211_;
 dPARSE L22;
 BOOL L23;
 BOOL L241_;
 INT L25;
 INT L26;
 BOOL L271_;
 extern STR Classe1914147250;
 extern STR Classe1173452030;
 extern STR Classe1173452030;
 if (TP_CON1882075561(ATTR(self,con), ATTR(ATTR(self,as),tp))) {
  param_include_er = self;
  param_include_er1 = ((dAS_CLASS_ELT) ATTR(self,as));
  prog_self = param_include_er;
  ret_val1 = ATTR(ATTR(prog_self,tp),prog);
  PROG_e176405615(ret_val1, ((dPROG_ERR) param_include_er1));
  prog_self1 = param_include_er;
  ret_val2 = ATTR(ATTR(prog_self1,tp),prog);
  PROG_err_STR(ret_val2, ((STR) &Typesp941653765));
  ret_val = ((TP_CLASS) NULL);
  return ret_val;
 }
 r = TP_CON2076339407(ATTR(self,con), ATTR(ATTR(self,as),tp), FALSE);
 switch (TAG(r)) {
  case TP_CLASS_tag:
   if (TP_CLA1568240525(((TP_CLASS) r))) {
    prog_self2 = self;
    ret_val3 = ATTR(ATTR(prog_self2,tp),prog);
    PROG_e176405615(ret_val3, ((dPROG_ERR) ATTR(self,as)));
    prog_self3 = self;
    ret_val4 = ATTR(ATTR(prog_self3,tp),prog);
    PROG_err_STR(ret_val4, ((STR) &Classe21075321));
    ret_val = ((TP_CLASS) NULL);
    return ret_val;
   }
   is_external_self = ((TP_CLASS) r);
   kind_self = is_external_self;
   L2 = ATTR(kind_self,kind_cache);
   L3 = TP_KIN138039446;
   L41_=(L2)==(L3); 
   if (L41_) {
    as_tree_self = kind_self;
    if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L5 = ATTR(ATTR(as_tree_self,prog),parse);
     L6 = ATTR(as_tree_self,name1);
     L7 = ATTR(as_tree_self,params);
     L81_=(L7)==NULL?0:ASIZE((ARRAYdTP)L7); 
     L9 = L5;
     SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L9)])(L9, L6, L81_));
    }
    ret_val7 = ATTR(as_tree_self,my_as_tree);
    L10 = (ret_val7==((AS_CLASS_DEF) NULL));
    L111_=!(L10); 
    if (L111_) {
     SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
    }
   }
   ret_val6 = ATTR(kind_self,kind_cache);
   L12 = ret_val6;
   L13 = TP_KIND_ext_c_tp;
   L141_=(L12)==(L13); 
   if (L141_) {
    L1 = TRUE;
   } else {
    kind_self1 = is_external_self;
    L15 = ATTR(kind_self1,kind_cache);
    L16 = TP_KIN138039446;
    L171_=(L15)==(L16); 
    if (L171_) {
     as_tree_self1 = kind_self1;
     if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L18 = ATTR(ATTR(as_tree_self1,prog),parse);
      L19 = ATTR(as_tree_self1,name1);
      L20 = ATTR(as_tree_self1,params);
      L211_=(L20)==NULL?0:ASIZE((ARRAYdTP)L20); 
      L22 = L18;
      SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L22)])(L22, L19, L211_));
     }
     ret_val9 = ATTR(as_tree_self1,my_as_tree);
     L23 = (ret_val9==((AS_CLASS_DEF) NULL));
     L241_=!(L23); 
     if (L241_) {
      SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
     }
    }
    ret_val8 = ATTR(kind_self1,kind_cache);
    L25 = ret_val8;
    L26 = TP_KIN462481068;
    L271_=(L25)==(L26); 
    L1 = L271_;
   }
   ret_val5 = L1;
   if (ret_val5) {
    include_ext_err_ = self;
    include_ext_err_1 = ((dAS_CLASS_ELT) ATTR(self,as));
    prog_self4 = include_ext_err_;
    ret_val10 = ATTR(ATTR(prog_self4,tp),prog);
    PROG_e176405615(ret_val10, ((dPROG_ERR) include_ext_err_1));
    prog_self5 = include_ext_err_;
    ret_val11 = ATTR(ATTR(prog_self5,tp),prog);
    PROG_err_STR(ret_val11, ((STR) &Classe1914147250));
    ret_val = ((TP_CLASS) NULL);
    return ret_val;
   }
   ret_val = ((TP_CLASS) r);
   return ret_val; break;
  case TP_ROUT_tag:
   include_bound_er = self;
   include_bound_er1 = ((dAS_CLASS_ELT) ATTR(self,as));
   prog_self6 = include_bound_er;
   ret_val12 = ATTR(ATTR(prog_self6,tp),prog);
   PROG_e176405615(ret_val12, ((dPROG_ERR) include_bound_er1));
   prog_self7 = include_bound_er;
   ret_val13 = ATTR(ATTR(prog_self7,tp),prog);
   PROG_err_STR(ret_val13, ((STR) &Classe1173452030));
   ret_val = ((TP_CLASS) NULL);
   return ret_val; break;
  case TP_ITER_tag:
   include_bound_er2 = self;
   include_bound_er3 = ((dAS_CLASS_ELT) ATTR(self,as));
   prog_self8 = include_bound_er2;
   ret_val14 = ATTR(ATTR(prog_self8,tp),prog);
   PROG_e176405615(ret_val14, ((dPROG_ERR) include_bound_er3));
   prog_self9 = include_bound_er2;
   ret_val15 = ATTR(ATTR(prog_self9,tp),prog);
   PROG_err_STR(ret_val15, ((STR) &Classe1173452030));
   ret_val = ((TP_CLASS) NULL);
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin IMPL_INCLUDE::included_tp:TP_CLASS\n./Representation/impl.sa:290:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1060127472(INLINE self, AM_ROUT_DEF rout, AM_LOOP_STMT lp, AM_ITE1809135850 call11) {
 dAM_EXPR ret_val;
 dINLINE_ITER_SIG inl;
 dAM_EXPR n;
 FLISTA1062334999 L11;
 FLISTA1409846210 L21;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 FLISTA1062334999 delete_elt_order;
 AM_ITE1809135850 delete_elt_order1;
 OUT create_self2;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val10;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val12;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val14;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val16;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val18;
 FILE1 r8;
 FLISTdAM_EXPR delete_elt_order2;
 dAM_EXPR delete_elt_order3;
 dINLINE_ITER_SIG L3;
 BOOL L4;
 BOOL L51_;
 dINLINE_ITER_SIG L6;
 AM_ITE1809135850 L7;
 AM_BND367211769 L8;
 INT L9;
 BOOL L101_;
 extern STR WARNINGiter;
 FILE1 L12;
 OB L13;
 FILE1 L15;
 OB L16;
 extern STR isnotr1517381936;
 FILE1 L18;
 OB L19;
 extern STR END_OF_LOOP;
 FILE1 L22;
 OB L23;
 INT L25;
 BOOL L261_;
 extern STR WARNINGiter;
 FILE1 L27;
 OB L28;
 FILE1 L30;
 OB L31;
 extern STR isnotr1464605876;
 FILE1 L33;
 OB L34;
 FILE1 L36;
 OB L37;
 extern STR name9;
 FILE1 L39;
 OB L40;
 inl = INLINE1838056742(ATTR(self,itbl), ATTR(call11,fun));
 if ((inl==((dINLINE_ITER_SIG) NULL))) {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
 L3 = inl;
 L4 = (*dINLIN2120180991[TAG(L3)])(L3);
 L51_=!(L4); 
 if (L51_) {
  L6 = inl;
  n = (*dINLIN104160276[TAG(L6)])(L6, call11);
  if (n==NULL) {
   goto other296;
  } else
  switch (TAG(n)) {
   case AM_STMT_EXPR_tag:
    SATTR(rout,calls1,FLISTd1364738838(ATTR(rout,calls1), ATTR(((AM_STMT_EXPR) n),calls1)));
    SATTR(rout,locals1,FLISTA811588375(ATTR(rout,locals1), ATTR(((AM_STMT_EXPR) n),locals1)));
    {
     struct FLISTA20621028_frame_struct other1_0;
FLISTA20621028_frame noname1 = &other1_0;
     L11 = ATTR(((AM_STMT_EXPR) n),its1);
     noname1->self = L11;
     noname1->state = 0;
     while (1) {
      L7 = FLISTA20621028(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      SATTR(L7,lp,lp);
     }
    }
    after_loop: ;
    {
     struct FLISTA1446310868_frame_struct other2_0;
FLISTA1446310868_frame noname2 = &other2_0;
     L21 = ATTR(((AM_STMT_EXPR) n),bits1);
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L8 = FLISTA1446310868(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      SATTR(L8,lp,lp);
     }
    }
    after_loop1: ;
    SATTR(lp,its1,FLISTA572736043(ATTR(lp,its1), ATTR(((AM_STMT_EXPR) n),its1)));
    SATTR(lp,bits1,FLISTA1236996208(ATTR(lp,bits1), ATTR(((AM_STMT_EXPR) n),bits1)));
    SATTR(lp,firsts,FLISTA811588375(ATTR(lp,firsts), ATTR(((AM_STMT_EXPR) n),firsts)));
    L9 = FLISTA23038851(ATTR(lp,its1), call11);
    L101_=(L9)<(0); 
    if (L101_) {
     if (ATTR(ATTR(self,prog),opt_debug)) {
      create_self = ((OUT) NULL);
      ret_val1 = create_self;
      plus_self = ret_val1;
      plus_s = ((STR) &WARNINGiter);
      stdout_self = ((FILE1) NULL);
      L13=ZALLOC(sizeof(struct FILE1_struct));
      if (L13==NULL) FATAL("Unable to allocate more memory");
      ((OB)L13)->header.tag=FILE1_tag;
      L12 = ((FILE1) L13);
      r = L12;
      
      SATTR(r,fp,stdout);
      ret_val3 = r;
      FILE_plus_STR(ret_val3, plus_s);
      ret_val2 = plus_self;
      plus_self1 = ret_val2;
      plus_s1 = ATTR(ATTR(call11,fun),str);
      stdout_self1 = ((FILE1) NULL);
      L16=ZALLOC(sizeof(struct FILE1_struct));
      if (L16==NULL) FATAL("Unable to allocate more memory");
      ((OB)L16)->header.tag=FILE1_tag;
      L15 = ((FILE1) L16);
      r1 = L15;
      
      SATTR(r1,fp,stdout);
      ret_val5 = r1;
      FILE_plus_STR(ret_val5, plus_s1);
      ret_val4 = plus_self1;
      plus_self2 = ret_val4;
      plus_s2 = ((STR) &isnotr1517381936);
      stdout_self2 = ((FILE1) NULL);
      L19=ZALLOC(sizeof(struct FILE1_struct));
      if (L19==NULL) FATAL("Unable to allocate more memory");
      ((OB)L19)->header.tag=FILE1_tag;
      L18 = ((FILE1) L19);
      r2 = L18;
      
      SATTR(r2,fp,stdout);
      ret_val6 = r2;
      FILE_plus_STR(ret_val6, plus_s2);
      AM_OUT1439666796(((AM_OUT) NULL), ((dAM) lp));
      create_self1 = ((OUT) NULL);
      ret_val7 = create_self1;
      plus_self3 = ret_val7;
      plus_s3 = ((STR) &END_OF_LOOP);
      stdout_self3 = ((FILE1) NULL);
      L23=ZALLOC(sizeof(struct FILE1_struct));
      if (L23==NULL) FATAL("Unable to allocate more memory");
      ((OB)L23)->header.tag=FILE1_tag;
      L22 = ((FILE1) L23);
      r3 = L22;
      
      SATTR(r3,fp,stdout);
      ret_val8 = r3;
      FILE_plus_STR(ret_val8, plus_s3);
     }
    }
    else {
     delete_elt_order = ATTR(lp,its1);
     delete_elt_order1 = call11;
     FLISTA739122510(delete_elt_order, FLISTA23038851(delete_elt_order, delete_elt_order1));
    }
    L25 = FLISTd1086621942(ATTR(rout,calls1), ((dAM_EXPR) call11));
    L261_=(L25)<(0); 
    if (L261_) {
     if (ATTR(ATTR(self,prog),opt_debug)) {
      create_self2 = ((OUT) NULL);
      ret_val9 = create_self2;
      plus_self4 = ret_val9;
      plus_s4 = ((STR) &WARNINGiter);
      stdout_self4 = ((FILE1) NULL);
      L28=ZALLOC(sizeof(struct FILE1_struct));
      if (L28==NULL) FATAL("Unable to allocate more memory");
      ((OB)L28)->header.tag=FILE1_tag;
      L27 = ((FILE1) L28);
      r4 = L27;
      
      SATTR(r4,fp,stdout);
      ret_val11 = r4;
      FILE_plus_STR(ret_val11, plus_s4);
      ret_val10 = plus_self4;
      plus_self5 = ret_val10;
      plus_s5 = ATTR(ATTR(call11,fun),str);
      stdout_self5 = ((FILE1) NULL);
      L31=ZALLOC(sizeof(struct FILE1_struct));
      if (L31==NULL) FATAL("Unable to allocate more memory");
      ((OB)L31)->header.tag=FILE1_tag;
      L30 = ((FILE1) L31);
      r5 = L30;
      
      SATTR(r5,fp,stdout);
      ret_val13 = r5;
      FILE_plus_STR(ret_val13, plus_s5);
      ret_val12 = plus_self5;
      plus_self6 = ret_val12;
      plus_s6 = ((STR) &isnotr1464605876);
      stdout_self6 = ((FILE1) NULL);
      L34=ZALLOC(sizeof(struct FILE1_struct));
      if (L34==NULL) FATAL("Unable to allocate more memory");
      ((OB)L34)->header.tag=FILE1_tag;
      L33 = ((FILE1) L34);
      r6 = L33;
      
      SATTR(r6,fp,stdout);
      ret_val15 = r6;
      FILE_plus_STR(ret_val15, plus_s6);
      ret_val14 = plus_self6;
      plus_self7 = ret_val14;
      plus_s7 = ATTR(ATTR(rout,sig1),str);
      stdout_self7 = ((FILE1) NULL);
      L37=ZALLOC(sizeof(struct FILE1_struct));
      if (L37==NULL) FATAL("Unable to allocate more memory");
      ((OB)L37)->header.tag=FILE1_tag;
      L36 = ((FILE1) L37);
      r7 = L36;
      
      SATTR(r7,fp,stdout);
      ret_val17 = r7;
      FILE_plus_STR(ret_val17, plus_s7);
      ret_val16 = plus_self7;
      plus_self8 = ret_val16;
      plus_s8 = ((STR) &name9);
      stdout_self8 = ((FILE1) NULL);
      L40=ZALLOC(sizeof(struct FILE1_struct));
      if (L40==NULL) FATAL("Unable to allocate more memory");
      ((OB)L40)->header.tag=FILE1_tag;
      L39 = ((FILE1) L40);
      r8 = L39;
      
      SATTR(r8,fp,stdout);
      ret_val18 = r8;
      FILE_plus_STR(ret_val18, plus_s8);
     }
    }
    else {
     delete_elt_order2 = ATTR(rout,calls1);
     delete_elt_order3 = ((dAM_EXPR) call11);
     FLISTd1252754628(delete_elt_order2, FLISTd1086621942(delete_elt_order2, delete_elt_order3));
    } break;
   default: ;
   other296: ;
  }
  ret_val = n;
  return ret_val;
 }
 else {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1426016646(INLINE self, AM_ITE1809135850 call11) {
 dAM_EXPR ret_val;
 dINLINE_ITER_SIG inl;
 dINLINE_ITER_SIG L1;
 dINLINE_ITER_SIG L2;
 inl = INLINE1838056742(ATTR(self,itbl), ATTR(call11,fun));
 if ((inl==((dINLINE_ITER_SIG) NULL))) {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
 L1 = inl;
 if ((*dINLIN2120180991[TAG(L1)])(L1)) {
  L2 = inl;
  ret_val = (*dINLIN104160276[TAG(L2)])(L2, call11);
  return ret_val;
 }
 else {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1475043475(INLINE1433668901 self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_SHARED_EXPR r;
 AM_SHARED_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val1;
 AM_SHARED_EXPR r1;
 INT L1;
 INT L21_;
 AM_SHARED_EXPR L3;
 OB L4;
 L1 = INLINE1306635001;
 L21_=INTPLUS(L1,1); 
 INLINE1306635001 = L21_;
 create_self = ((AM_SHARED_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L4=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_SHARED_EXPR_tag;
 L3 = ((AM_SHARED_EXPR) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,tp_at,ATTR(self,tp_at));
 SATTR(r,name1,ATTR(self,name1));
 SATTR(r,class_tp,ATTR(self,class_tp));
 SATTR(r,init,ATTR(self,init));
 SATTR(r,is_const,ATTR(self,is_const));
 SATTR(r,as_type,ATTR(self,as_type));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE1797911459(INLINE self, AM_ROUT_DEF rout, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 dINLINE_ROUT_SIG in11;
 dAM_EXPR n;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 FLISTdAM_EXPR delete_elt_order;
 dAM_EXPR delete_elt_order1;
 dINLINE_ROUT_SIG L1;
 BOOL L2;
 BOOL L31_;
 dINLINE_ROUT_SIG L4;
 INT L5;
 BOOL L61_;
 extern STR WARNINGfunction;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L11;
 extern STR isnotr1464605876;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 extern STR name9;
 FILE1 L19;
 OB L20;
 in11 = INLINE2019550598(ATTR(self,rtbl), ATTR(call11,fun));
 if ((in11==((dINLINE_ROUT_SIG) NULL))) {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
 L1 = in11;
 L2 = (*dINLIN1181425367[TAG(L1)])(L1);
 L31_=!(L2); 
 if (L31_) {
  L4 = in11;
  n = (*dINLIN516975284[TAG(L4)])(L4, call11);
  if (n==NULL) {
   goto other291;
  } else
  switch (TAG(n)) {
   case AM_STMT_EXPR_tag:
    SATTR(rout,calls1,FLISTd1364738838(ATTR(rout,calls1), ATTR(((AM_STMT_EXPR) n),calls1)));
    SATTR(rout,locals1,FLISTA811588375(ATTR(rout,locals1), ATTR(((AM_STMT_EXPR) n),locals1)));
    L5 = FLISTd1086621942(ATTR(rout,calls1), ((dAM_EXPR) call11));
    L61_=(L5)<(0); 
    if (L61_) {
     if (ATTR(ATTR(self,prog),opt_debug)) {
      create_self = ((OUT) NULL);
      ret_val1 = create_self;
      plus_self = ret_val1;
      plus_s = ((STR) &WARNINGfunction);
      stdout_self = ((FILE1) NULL);
      L8=ZALLOC(sizeof(struct FILE1_struct));
      if (L8==NULL) FATAL("Unable to allocate more memory");
      ((OB)L8)->header.tag=FILE1_tag;
      L7 = ((FILE1) L8);
      r = L7;
      
      SATTR(r,fp,stdout);
      ret_val3 = r;
      FILE_plus_STR(ret_val3, plus_s);
      ret_val2 = plus_self;
      plus_self1 = ret_val2;
      plus_s1 = ATTR(ATTR(call11,fun),str);
      stdout_self1 = ((FILE1) NULL);
      L11=ZALLOC(sizeof(struct FILE1_struct));
      if (L11==NULL) FATAL("Unable to allocate more memory");
      ((OB)L11)->header.tag=FILE1_tag;
      L10 = ((FILE1) L11);
      r1 = L10;
      
      SATTR(r1,fp,stdout);
      ret_val5 = r1;
      FILE_plus_STR(ret_val5, plus_s1);
      ret_val4 = plus_self1;
      plus_self2 = ret_val4;
      plus_s2 = ((STR) &isnotr1464605876);
      stdout_self2 = ((FILE1) NULL);
      L14=ZALLOC(sizeof(struct FILE1_struct));
      if (L14==NULL) FATAL("Unable to allocate more memory");
      ((OB)L14)->header.tag=FILE1_tag;
      L13 = ((FILE1) L14);
      r2 = L13;
      
      SATTR(r2,fp,stdout);
      ret_val7 = r2;
      FILE_plus_STR(ret_val7, plus_s2);
      ret_val6 = plus_self2;
      plus_self3 = ret_val6;
      plus_s3 = ATTR(ATTR(rout,sig1),str);
      stdout_self3 = ((FILE1) NULL);
      L17=ZALLOC(sizeof(struct FILE1_struct));
      if (L17==NULL) FATAL("Unable to allocate more memory");
      ((OB)L17)->header.tag=FILE1_tag;
      L16 = ((FILE1) L17);
      r3 = L16;
      
      SATTR(r3,fp,stdout);
      ret_val9 = r3;
      FILE_plus_STR(ret_val9, plus_s3);
      ret_val8 = plus_self3;
      plus_self4 = ret_val8;
      plus_s4 = ((STR) &name9);
      stdout_self4 = ((FILE1) NULL);
      L20=ZALLOC(sizeof(struct FILE1_struct));
      if (L20==NULL) FATAL("Unable to allocate more memory");
      ((OB)L20)->header.tag=FILE1_tag;
      L19 = ((FILE1) L20);
      r4 = L19;
      
      SATTR(r4,fp,stdout);
      ret_val10 = r4;
      FILE_plus_STR(ret_val10, plus_s4);
     }
    }
    else {
     delete_elt_order = ATTR(rout,calls1);
     delete_elt_order1 = ((dAM_EXPR) call11);
     FLISTd1252754628(delete_elt_order, FLISTd1086621942(delete_elt_order, delete_elt_order1));
    } break;
   default: ;
   other291: ;
  }
  ret_val = n;
  return ret_val;
 }
 else {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE2023716438(INLINE1455768374 self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_SHARED_EXPR g;
 AM_ASSIGN_STMT stmt1;
 AM_STMT_EXPR r;
 AM_SHARED_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val1;
 AM_SHARED_EXPR r1;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val2;
 AM_ASSIGN_STMT r2;
 AM_STMT_EXPR create_self2;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val3;
 AM_STMT_EXPR r3;
 INT L1;
 INT L21_;
 AM_SHARED_EXPR L3;
 OB L4;
 AM_ASSIGN_STMT L5;
 OB L6;
 AM_CALL_ARG L71_;
 AM_CALL_ARG L8;
 AM_STMT_EXPR L9;
 OB L10;
 L1 = INLINE1181209094;
 L21_=INTPLUS(L1,1); 
 INLINE1181209094 = L21_;
 create_self = ((AM_SHARED_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L4=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_SHARED_EXPR_tag;
 L3 = ((AM_SHARED_EXPR) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 g = ret_val1;
 SATTR(g,tp_at,ATTR(self,tp_at));
 SATTR(g,name1,ATTR(self,name1));
 SATTR(g,class_tp,ATTR(self,class_tp));
 SATTR(g,init,ATTR(self,init));
 SATTR(g,is_const,ATTR(self,is_const));
 SATTR(g,as_type,ATTR(self,as_type));
 create_self1 = ((AM_ASSIGN_STMT) NULL);
 create_source1 = ATTR(call11,source1);
 L6=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_ASSIGN_STMT_tag;
 L5 = ((AM_ASSIGN_STMT) L6);
 r2 = L5;
 SATTR(r2,source1,create_source1);
 ret_val2 = r2;
 stmt1 = ret_val2;
 SATTR(stmt1,dest,((dAM_EXPR) g));
 L71_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,1); 
 L8=L71_;
 SATTR(stmt1,src1,ATTR(L8,expr));
 create_self2 = ((AM_STMT_EXPR) NULL);
 create_source2 = ATTR(call11,source1);
 L10=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_STMT_EXPR_tag;
 L9 = ((AM_STMT_EXPR) L10);
 r3 = L9;
 SATTR(r3,source1,create_source2);
 ret_val3 = r3;
 r = ret_val3;
 SATTR(r,stmts,((dAM_STMT) stmt1));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE46712374(INLINE_ATTR_READ self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_ATTR_EXPR r;
 AM_ATTR_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val1;
 AM_ATTR_EXPR r1;
 INT L1;
 INT L21_;
 AM_ATTR_EXPR L3;
 OB L4;
 AM_CALL_ARG L51_;
 AM_CALL_ARG L6;
 L1 = INLINE957944682;
 L21_=INTPLUS(L1,1); 
 INLINE957944682 = L21_;
 create_self = ((AM_ATTR_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L4=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_ATTR_EXPR_tag;
 L3 = ((AM_ATTR_EXPR) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L51_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,0); 
 L6=L51_;
 SATTR(r,ob,ATTR(L6,expr));
 SATTR(r,self_tp,ATTR(self,self_tp));
 SATTR(r,at,ATTR(self,at));
 SATTR(r,tp_at,ATTR(self,tp_at));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE472404814(INLINE self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 dINLINE_ROUT_SIG in11;
 dINLINE_ROUT_SIG L1;
 dINLINE_ROUT_SIG L2;
 in11 = INLINE2019550598(ATTR(self,rtbl), ATTR(call11,fun));
 if ((in11==((dINLINE_ROUT_SIG) NULL))) {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
 L1 = in11;
 if ((*dINLIN1181425367[TAG(L1)])(L1)) {
  L2 = in11;
  ret_val = (*dINLIN516975284[TAG(L2)])(L2, call11);
  return ret_val;
 }
 else {
  ret_val = ((dAM_EXPR) call11);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dAM_EXPR INLINE655229257(INLINE1596311811 self, AM_ROU1916046290 call11) {
 dAM_EXPR ret_val;
 AM_ATTR_EXPR a;
 AM_ASSIGN_STMT as;
 AM_STMT_EXPR r;
 AM_ATTR_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val1;
 AM_ATTR_EXPR r1;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val2;
 AM_ASSIGN_STMT r2;
 AM_STMT_EXPR create_self2;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val3;
 AM_STMT_EXPR r3;
 INT L1;
 INT L21_;
 AM_ATTR_EXPR L3;
 OB L4;
 AM_CALL_ARG L51_;
 AM_CALL_ARG L6;
 AM_ASSIGN_STMT L7;
 OB L8;
 AM_CALL_ARG L91_;
 AM_CALL_ARG L10;
 AM_STMT_EXPR L11;
 OB L12;
 L1 = INLINE1258550073;
 L21_=INTPLUS(L1,1); 
 INLINE1258550073 = L21_;
 create_self = ((AM_ATTR_EXPR) NULL);
 create_source = ATTR(call11,source1);
 L4=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AM_ATTR_EXPR_tag;
 L3 = ((AM_ATTR_EXPR) L4);
 r1 = L3;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 a = ret_val1;
 L51_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,0); 
 L6=L51_;
 SATTR(a,ob,ATTR(L6,expr));
 SATTR(a,self_tp,ATTR(self,self_tp));
 SATTR(a,at,ATTR(self,at));
 SATTR(a,tp_at,ATTR(self,tp_at));
 create_self1 = ((AM_ASSIGN_STMT) NULL);
 create_source1 = ATTR(call11,source1);
 L8=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AM_ASSIGN_STMT_tag;
 L7 = ((AM_ASSIGN_STMT) L8);
 r2 = L7;
 SATTR(r2,source1,create_source1);
 ret_val2 = r2;
 as = ret_val2;
 SATTR(as,dest,((dAM_EXPR) a));
 L91_=(AM_CALL_ARG)ARR((AM_ROU1916046290)call11,1); 
 L10=L91_;
 SATTR(as,src1,ATTR(L10,expr));
 create_self2 = ((AM_STMT_EXPR) NULL);
 create_source2 = ATTR(call11,source1);
 L12=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AM_STMT_EXPR_tag;
 L11 = ((AM_STMT_EXPR) L12);
 r3 = L11;
 SATTR(r3,source1,create_source2);
 ret_val3 = r3;
 r = ret_val3;
 SATTR(r,stmts,((dAM_STMT) as));
 ret_val = ((dAM_EXPR) r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CHANGE1122717432(CHANGE_VARS self, SFILE_ID source1) {
 FLISTA725283029 L11;
 AM_LOCAL_EXPR l;
 AM_LOCAL_EXPR m;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val;
 AM_LOCAL_EXPR r;
 AM_LOCAL_EXPR L2;
 AM_LOCAL_EXPR L3;
 OB L4;
 SATTR(self,new_subst,FMAPAM714369621(((FMAPAM1145008811) NULL), FLISTA1151473122(ATTR(self,saved_vars))));
 SATTR(self,new_vars,FLISTA1446031755(((FLISTA725283029) NULL), FLISTA1151473122(ATTR(self,saved_vars))));
 {
  struct FLISTA2119642552_frame_struct other1_0;
FLISTA2119642552_frame noname1 = &other1_0;
  L11 = ATTR(self,saved_vars);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   l = L2;
   create_self = ((AM_LOCAL_EXPR) NULL);
   create_source = source1;
   L4=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L4==NULL) FATAL("Unable to allocate more memory");
   ((OB)L4)->header.tag=AM_LOCAL_EXPR_tag;
   L3 = ((AM_LOCAL_EXPR) L4);
   r = L3;
   SATTR(r,source1,create_source);
   ret_val = r;
   m = ret_val;
   SATTR(m,is_volatile,ATTR(l,is_volatile));
   SATTR(m,name1,ATTR(l,name1));
   SATTR(m,tp_at,ATTR(l,tp_at));
   SATTR(m,needs_init,ATTR(l,needs_init));
   SATTR(m,no_assign,ATTR(l,no_assign));
   SATTR(self,new_subst,FMAPAM876857188(ATTR(self,new_subst), l, m));
   SATTR(self,new_vars,FLISTA1817671564(ATTR(self,new_vars), m));
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void IMPL_C170915365(IMPL_CREATE self, ELT e, ELT f, IMPL_INCLUDE inc) {
 IMPL_CREATE prog_self;
 PROG ret_val;
 IMPL_CREATE prog_self1;
 PROG ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 extern STR Thisex1415772183;
 extern STR whichi1312142318;
 extern STR name18;
 prog_self = self;
 ret_val = ATTR(ATTR(prog_self,tp),prog);
 PROG_e176405615(ret_val, ((dPROG_ERR) ATTR(f,as)));
 prog_self1 = self;
 ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
 plus_self = ((STR) &Thisex1415772183);
 plus_sarg = ATTR(ATTR(e,sig1),str);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val2;
 plus_sarg1 = ((STR) &whichi1312142318);
 ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val3;
 plus_sarg2 = TP_CLASS_strrSTR(ATTR(inc,itp));
 ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val4;
 plus_sarg3 = ((STR) &name18);
 ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
 PROG_err_STR(ret_val1, ret_val5);
}


#undef IS_ITER
#define IS_ITER 0

void IMPL_C1750830100(IMPL_CREATE self, IMPL_INCLUDE inc, IMPL_INCLUDE inc2, ELT e, ELT g) {
 IMPL_CREATE prog_self;
 PROG ret_val;
 IMPL_CREATE prog_self1;
 PROG ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
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
 extern STR Thisin312642855;
 extern STR whichc598506611;
 extern STR whichi13121423181;
 extern STR name18;
 prog_self = self;
 ret_val = ATTR(ATTR(prog_self,tp),prog);
 PROG_e176405615(ret_val, ((dPROG_ERR) ATTR(inc,as)));
 prog_self1 = self;
 ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
 plus_self = ((STR) &Thisin312642855);
 plus_sarg = ATTR(ATTR(e,sig1),str);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val2;
 plus_sarg1 = ((STR) &whichc598506611);
 ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val3;
 plus_sarg2 = ATTR(ATTR(g,sig1),str);
 ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val4;
 plus_sarg3 = ((STR) &whichi13121423181);
 ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val5;
 plus_sarg4 = TP_CLASS_strrSTR(ATTR(inc2,itp));
 ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
 plus_self5 = ret_val6;
 plus_sarg5 = ((STR) &name18);
 ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
 PROG_err_STR(ret_val1, ret_val7);
}


#undef IS_ITER
#define IS_ITER 0

void IMPL_C1885190518(IMPL_CREATE self) {
 dAS_CLASS_ELT e;
 IMPL_INCLUDE ii;
 IMPL_CREATE prog_self;
 PROG ret_val;
 IMPL_CREATE prog_self1;
 PROG ret_val1;
 BOOL L1;
 BOOL L21_;
 extern STR Extern1737961284;
 BOOL L3;
 BOOL L41_;
 dAS_CLASS_ELT L5;
 e = ATTR(ATTR(self,as),body);
 while (1) {
  L1 = (e==((dAS_CLASS_ELT) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  if (e==NULL) {
   goto other129;
  } else
  switch (TAG(e)) {
   case AS_INC1416917001_tag:
    if (ATTR(self,is_external)) {
     prog_self = self;
     ret_val = ATTR(ATTR(prog_self,tp),prog);
     PROG_e176405615(ret_val, ((dPROG_ERR) e));
     prog_self1 = self;
     ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
     PROG_err_STR(ret_val1, ((STR) &Extern1737961284));
    }
    else {
     ii = IMPL_I1190749135(((IMPL_INCLUDE) NULL), ATTR(self,tp), ((AS_INC1416917001) e), ATTR(self,con));
     L3 = (ii==((IMPL_INCLUDE) NULL));
     L41_=!(L3); 
     if (L41_) {
      SATTR(self,incs,FLISTI1378884285(ATTR(self,incs), ii));
     }
    } break;
   default: ;
   other129: ;
  }
  L5 = e;
  e = (*dAS_CL760354380[TAG(L5)])(L5);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void IMPL_C228209176(IMPL_CREATE self, IMPL_INCLUDE inc, TP_CLASS a11, TP_CLASS a21) {
 IMPL_CREATE prog_self;
 PROG ret_val;
 IMPL_CREATE prog_self1;
 PROG ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 extern STR Thiscl1001874673;
 extern STR and3;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 prog_self = self;
 ret_val = ATTR(ATTR(prog_self,tp),prog);
 PROG_e176405615(ret_val, ((dPROG_ERR) ATTR(inc,as)));
 prog_self1 = self;
 ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
 plus_self = ((STR) &Thiscl1001874673);
 plus_sarg = TP_CLASS_strrSTR(a11);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val2;
 plus_sarg1 = ((STR) &and3);
 ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val3;
 plus_sarg2 = TP_CLASS_strrSTR(a21);
 ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val4;
 plus_arg = '.';
 str_self = plus_arg;
 create_self = ((STR) NULL);
 create_c = str_self;
 L2 = 1;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 SARR((STR)r,0,create_c); 
 ;
 ret_val7 = r;
 ret_val6 = ret_val7;
 s = ret_val6;
 ret_val5 = STR_ap1077157958(plus_self3, s, TRUE);
 PROG_err_STR(ret_val1, ret_val5);
}


#undef IS_ITER
#define IS_ITER 0

void INLINE1765310158(INLINE self, AM_ROUT_DEF am) {
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
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
 L1 = SIG_is418491101(ATTR(am,sig1));
 L21_=!(L1); 
 if (L21_) {
  is_iter_self = am;
  is_iter_self1 = ATTR(is_iter_self,sig1);
  is_iter_self2 = ATTR(is_iter_self1,name1);
  L4 = (is_iter_self2.str==((STR) NULL));
  L51_=!(L4); 
  if (L51_) {
   L6 = is_iter_self2.str;
   L7 = STR_sizerINT(is_iter_self2.str);
   L81_=INTMINUS(L7,1); 
   L9 = L81_;
   L101_=ARR((STR)L6,L9); 
   L11 = L101_;
   L121_=L11=='!'; 
   L3 = L121_;
  } else {
   L3 = FALSE;
  }
  ret_val2 = L3;
  ret_val1 = ret_val2;
  ret_val = ret_val1;
  if (ret_val) {
   SATTR(self,itbl,INLINE263789299(ATTR(self,itbl), am, ATTR(self,inline_iters), ATTR(self,iter_thres), ATTR(self,prog)));
  }
  else {
   SATTR(self,rtbl,INLINE180230301(ATTR(self,rtbl), am, ATTR(self,inline_routs), ATTR(self,rout_thres), ATTR(self,prog)));
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void INLINE_init(INLINE self) {
 TP_CLASS int_tp;
 ARRAYARG ar;
 SIG ipiis;
 ARRAYARG create_self;
 INT create_n = INT_zero;
 ARRAYARG ret_val;
 ARG create_self1;
 dTP create_t;
 ARG ret_val1;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val2;
 ARRAYdTP create_self2;
 INT create_n1 = INT_zero;
 ARRAYdTP ret_val3;
 SIG create_self3;
 dTP create_ntp;
 ARRAYdTP create_tparr;
 IDENT create_nname = IDENT_zero;
 ARRAYARG create_nargs;
 dTP create_nret;
 BOOL create_nsame = BOOL_zero;
 SIG ret_val4;
 SIG r;
 INLINE_INT_FOLD create_self4;
 SIG create_s;
 INLINE_INT_FOLD ret_val5;
 INLINE_INT_FOLD r1;
 ARRAYARG L1;
 INT L2;
 OB L3;
 INT L4;
 ARG L5;
 OB L6;
 ARG L7;
 ARRAYdTP L9;
 INT L10;
 OB L11;
 INT L12;
 SIG L13;
 OB L14;
 INLINE_INT_FOLD L15;
 OB L16;
 INT L17;
 INT L181_;
 int_tp = TP_BUILTIN_int;
 create_self = ((ARRAYARG) NULL);
 create_n = 1;
 L2 = create_n;
 L4=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L2)*sizeof(ARG);
 L3=ZALLOC_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((ARRAYARG) L3);
 L1->asize = L2;
 ret_val = L1;
 ar = ret_val;
 create_self1 = ((ARG) NULL);
 create_t = ((dTP) int_tp);
 L6=ZALLOC(sizeof(struct ARG_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARG_tag;
 L5 = ((ARG) L6);
 init_self = L5;
 init_t = create_t;
 init_m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
 init_same = FALSE;
 SATTR(init_self,tp,init_t);
 SATTR(init_self,mode1,init_m);
 SATTR(init_self,issame,init_same);
 ret_val2 = init_self;
 ret_val1 = ret_val2;
 L7 = ret_val1;
 SARR((ARRAYARG)ar,0,(ARG)L7); 
 ;
 create_self3 = ((SIG) NULL);
 create_ntp = ((dTP) int_tp);
 create_self2 = ((ARRAYdTP) NULL);
 create_n1 = 0;
 L10 = create_n1;
 L12=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L10)*sizeof(dTP);
 L11=ZALLOC_BIG(L12);
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L11)->header.destroyed=0;
#endif

 L9 = ((ARRAYdTP) L11);
 L9->asize = L10;
 ret_val3 = L9;
 create_tparr = ret_val3;
 create_nname = IDENT_2085547498;
 create_nargs = ar;
 create_nret = ((dTP) int_tp);
 create_nsame = TRUE;
 L14=ZALLOC(sizeof(struct SIG_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=SIG_tag;
 L13 = ((SIG) L14);
 r = L13;
 SATTR(r,tp,create_ntp);
 SATTR(r,src_tparams,create_tparr);
 SATTR(r,name1,create_nname);
 SATTR(r,args,create_nargs);
 SATTR(r,ret,create_nret);
 SATTR(r,returns_same,create_nsame);
 ret_val4 = SIG_ma1442297600(r);
 ipiis = ret_val4;
 create_self4 = ((INLINE_INT_FOLD) NULL);
 create_s = ipiis;
 L16=ZALLOC(sizeof(struct INLINE_INT_FOLD_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=INLINE_INT_FOLD_tag;
 L15 = ((INLINE_INT_FOLD) L16);
 r1 = L15;
 SATTR(r1,sig1,create_s);
 SATTR(r1,is_special,TRUE);
 L17 = INLINE1173790597;
 L181_=INTPLUS(L17,1); 
 INLINE1173790597 = L181_;
 ret_val5 = r1;
 SATTR(self,rtbl,INLINE525216690(ATTR(self,rtbl), ((dINLINE_ROUT_SIG) ret_val5)));
}
