#include "sather.h"

/* Layouts */

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYA1369744492_struct {/* layout for ARRAY{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *ARRAYA1369744492;

typedef struct ARRAYA1575632848_struct {/* layout for ARRAY{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *ARRAYA1575632848;

typedef struct ARRAYAM_ROUT_DEF_struct {/* layout for ARRAY{AM_ROUT_DEF} */
 OB_HEADER header;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *ARRAYAM_ROUT_DEF;

typedef struct ARRAYA1135548710_struct {/* layout for ARRAY{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *ARRAYA1135548710;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ARRAYA477251019_struct {/* layout for ARRAY{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *ARRAYA477251019;

typedef struct ARRAYA289850317_frame_struct {
 INT state;
 ARRAYA477251019 self;/* Formal argument: self */
 ARRAYdAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA289850317_frame;

typedef struct ARRAYARRAYBOOL_struct {/* layout for ARRAY{ARRAY{BOOL}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYBOOL_struct *arr_part[1];
 } *ARRAYARRAYBOOL;

typedef struct ARRAYARRAYSTR_struct {/* layout for ARRAY{ARRAY{STR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *ARRAYARRAYSTR;

typedef struct ARRAYAS_FEAT_MOD_struct {/* layout for ARRAY{AS_FEAT_MOD} */
 OB_HEADER header;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *ARRAYAS_FEAT_MOD;

typedef struct ARRAYATTRSORT_struct {/* layout for ARRAY{ATTRSORT} */
 OB_HEADER header;
 INT asize;
 struct ATTRSORT_struct *arr_part[1];
 } *ARRAYATTRSORT;

typedef struct ARRAYB1234765325_struct {/* layout for ARRAY{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *ARRAYB1234765325;

typedef struct ARRAYB1925477728_struct {/* layout for ARRAY{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *ARRAYB1925477728;

typedef struct ARRAYB1039347268_struct {/* layout for ARRAY{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *ARRAYB1039347268;

typedef struct ARRAYCALL_ARG_struct {/* layout for ARRAY{CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct CALL_ARG_struct *arr_part[1];
 } *ARRAYCALL_ARG;

typedef struct ARRAYC1438655838_struct {/* layout for ARRAY{CLASS_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *ARRAYC1438655838;

typedef struct ARRAYCODE_FILE_struct {/* layout for ARRAY{CODE_FILE} */
 OB_HEADER header;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *ARRAYCODE_FILE;

typedef struct ARRAYC1788421899_struct {/* layout for ARRAY{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *ARRAYC1788421899;

typedef struct ARRAYE2126408533_struct {/* layout for ARRAY{EXPR_HOISTED} */
 OB_HEADER header;
 INT asize;
 struct EXPR_HOISTED_struct *arr_part[1];
 } *ARRAYE2126408533;

typedef struct ARRAYE406487263_struct {/* layout for ARRAY{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *ARRAYE406487263;

typedef struct ARRAYF1030817442_struct {/* layout for ARRAY{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *ARRAYF1030817442;

typedef struct ARRAYFLISTSTR_struct {/* layout for ARRAY{FLIST{STR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTSTR_struct *arr_part[1];
 } *ARRAYFLISTSTR;

typedef struct ARRAYFSTR_struct {/* layout for ARRAY{FSTR} */
 OB_HEADER header;
 INT asize;
 struct FSTR_struct *arr_part[1];
 } *ARRAYFSTR;

typedef struct ARRAYIFC_struct {/* layout for ARRAY{IFC} */
 OB_HEADER header;
 INT asize;
 struct IFC_struct *arr_part[1];
 } *ARRAYIFC;

typedef struct ARRAYI1327367692_struct {/* layout for ARRAY{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *ARRAYI1327367692;

typedef struct ARRAYI1991954990_struct {/* layout for ARRAY{IMPL_INCLUDE} */
 OB_HEADER header;
 INT asize;
 struct IMPL_INCLUDE_struct *arr_part[1];
 } *ARRAYI1991954990;

typedef struct ARRAYO570594127_struct {/* layout for ARRAY{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct OPT_LOCAL_EXPR_struct *arr_part[1];
 } *ARRAYO570594127;

typedef struct ARRAYSIDE_EFFECT_struct {/* layout for ARRAY{SIDE_EFFECT} */
 OB_HEADER header;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *ARRAYSIDE_EFFECT;

typedef struct ARRAYSIG_struct {/* layout for ARRAY{SIG} */
 OB_HEADER header;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *ARRAYSIG;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct ARRAYTP_CLASS_struct {/* layout for ARRAY{TP_CLASS} */
 OB_HEADER header;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *ARRAYTP_CLASS;

typedef struct ARRAYTP_LAYOUT_struct {/* layout for ARRAY{TP_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct TP_LAYOUT_struct *arr_part[1];
 } *ARRAYTP_LAYOUT;

typedef struct ATTRSORT_struct {/* layout for ATTRSORT */
 OB_HEADER header;
 struct ELT_struct *elt1;
 STR name1;
 } *ATTRSORT;

typedef struct BOUND_2140373124_struct {/* layout for BOUND_ITER_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ITER_struct *tp;
 } *BOUND_2140373124;

typedef struct BOUND_809390774_struct {/* layout for BOUND_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ROUT_struct *tp;
 } *BOUND_809390774;

typedef struct BUILTIN_LAYOUT_struct {/* layout for BUILTIN_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *BUILTIN_LAYOUT;

typedef struct CALL_ARG_struct {/* layout for CALL_ARG */
 OB_HEADER header;
 struct dCALL_TP_struct *tp;
 struct dMODE_struct *mode1;
 } *CALL_ARG;

typedef struct CLASS_LAYOUT_struct {/* layout for CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *CLASS_LAYOUT;

typedef struct CSE_DOUBLE_EXPRS_struct {/* layout for CSE_DOUBLE_EXPRS */
 OB_HEADER header;
 struct FLISTdAM_EXPR_struct *expr;
 struct FLISTAM_CURSOR_struct *pos;
 struct FLISTA725283029_struct *locals1;
 } *CSE_DOUBLE_EXPRS;

typedef struct ELT_EQBOOL_struct {/* layout for ELT_EQ{BOOL} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQBOOL;

typedef struct ELT_EQIDENT_struct {/* layout for ELT_EQ{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQIDENT;

typedef struct ELT_LTATTRSORT_struct {/* layout for ELT_LT{ATTRSORT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_LTATTRSORT;

typedef struct ELT_LTFSTR_struct {/* layout for ELT_LT{FSTR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_LTFSTR;

typedef struct ELT_LTIDENT_struct {/* layout for ELT_LT{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_LTIDENT;

typedef struct ELT_LTSTR_struct {/* layout for ELT_LT{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_LTSTR;

typedef struct EXPR_HOISTED_struct {/* layout for EXPR_HOISTED */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct AM_LOCAL_EXPR_struct *local1;
 BOOL is_safe;
 } *EXPR_HOISTED;

typedef struct EXTERNAL_LAYOUT_struct {/* layout for EXTERNAL_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct PROG_struct *prog;
 STR str;
 } *EXTERNAL_LAYOUT;

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

typedef struct IMMUTA421759893_struct {/* layout for IMMUTABLE_CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *IMMUTA421759893;

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

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct OPT_LOCAL_EXPR_struct {/* layout for OPT_LOCAL_EXPR */
 OB_HEADER header;
 struct AM_LOCAL_EXPR_struct *local1;
 INT attr_access;
 INT index1;
 BOOL is_arg;
 BOOL is_sometimes_far;
 BOOL near_arg;
 } *OPT_LOCAL_EXPR;

typedef struct RND_struct {/* layout for RND */
 OB_HEADER header;
 FLTD norm_cache;
 struct MS_RANDOM_GEN_struct *own_gen;
 BOOL norm_cache_valid;
 } *RND;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SIDE_EFFECT_struct {/* layout for SIDE_EFFECT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct IDENT_struct name1;
 BOOL doeswrite;
 } *SIDE_EFFECT;

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

typedef struct TP_LAYOUT_struct {/* layout for TP_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *TP_LAYOUT;

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

typedef struct TUPAM_927437481_struct {/* layout for TUP{AM_CURSOR,$AM_EXPR,$AM_STMT} */
 struct dAM_EXPR_struct *t2;
 struct dAM_STMT_struct *t3;
 struct AM_CURSOR_struct *t1;
 } TUPAM_927437481;
static TUPAM_927437481 TUPAM_927437481_zero;

typedef struct TUPAM_927437481_boxed_struct {
 OB_HEADER header;
 TUPAM_927437481 immutable_part;
 } *TUPAM_927437481_boxed;

typedef struct TUPAM_1519040877_struct {/* layout for TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct AM_LOCAL_EXPR_struct *t2;
 } TUPAM_1519040877;
static TUPAM_1519040877 TUPAM_1519040877_zero;

typedef struct TUPAM_1519040877_boxed_struct {
 OB_HEADER header;
 TUPAM_1519040877 immutable_part;
 } *TUPAM_1519040877_boxed;

typedef struct TUPAM_504653531_struct {/* layout for TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct OPT_LOCAL_EXPR_struct *t2;
 } TUPAM_504653531;
static TUPAM_504653531 TUPAM_504653531_zero;

typedef struct TUPAM_504653531_boxed_struct {
 OB_HEADER header;
 TUPAM_504653531 immutable_part;
 } *TUPAM_504653531_boxed;

typedef struct TUPAM_1024994801_struct {/* layout for TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 struct AM_ROUT_DEF_struct *t1;
 struct FLISTA1409846210_struct *t2;
 } TUPAM_1024994801;
static TUPAM_1024994801 TUPAM_1024994801_zero;

typedef struct TUPAM_1024994801_boxed_struct {
 OB_HEADER header;
 TUPAM_1024994801 immutable_part;
 } *TUPAM_1024994801_boxed;

typedef struct TUPAM_653469574_struct {/* layout for TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
 struct AM_ROUT_DEF_struct *t1;
 struct FLISTA1062334999_struct *t2;
 } TUPAM_653469574;
static TUPAM_653469574 TUPAM_653469574_zero;

typedef struct TUPAM_653469574_boxed_struct {
 OB_HEADER header;
 TUPAM_653469574 immutable_part;
 } *TUPAM_653469574_boxed;

typedef struct TUPIDENTdTP_struct {/* layout for TUP{IDENT,$TP} */
 struct dTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTdTP;
static TUPIDENTdTP TUPIDENTdTP_zero;

typedef struct TUPIDENTdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTdTP immutable_part;
 } *TUPIDENTdTP_boxed;

typedef struct TUPIDE641654124_struct {/* layout for TUP{IDENT,AM_SHARED_EXPR} */
 struct AM_SHARED_EXPR_struct *t2;
 struct IDENT_struct t1;
 } TUPIDE641654124;
static TUPIDE641654124 TUPIDE641654124_zero;

typedef struct TUPIDE641654124_boxed_struct {
 OB_HEADER header;
 TUPIDE641654124 immutable_part;
 } *TUPIDE641654124_boxed;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA494342579_frame_struct {
 INT state;
 ARRAYA1369744492 self;/* Formal argument: self */
 AM_ITE1809135850 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA494342579_frame;

typedef struct ARRAYA27295121_frame_struct {
 INT state;
 ARRAYA1575632848 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA27295121_frame;

typedef struct ARRAYA702716235_frame_struct {
 INT state;
 ARRAYA1575632848 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA702716235_frame;

typedef struct ARRAYA1253289773_frame_struct {
 INT state;
 ARRAYAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1253289773_frame;

typedef struct ARRAYA1830087990_frame_struct {
 INT state;
 ARRAYA1135548710 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1830087990_frame;

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYA1077419174_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1077419174_frame;

typedef struct ARRAYA409651007_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA409651007_frame;

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

typedef struct ARRAYA712604839_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA712604839_frame;

typedef struct ARRAYA1660316962_frame_struct {
 INT state;
 ARRAYATTRSORT self;/* Formal argument: self */
 ATTRSORT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1660316962_frame;

typedef struct ARRAYA107911980_frame_struct {
 INT state;
 ARRAYATTRSORT self;/* Formal argument: self */
 ATTRSORT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA107911980_frame;

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

typedef struct ARRAYB1475407144_frame_struct {
 INT state;
 ARRAYB1234765325 self;/* Formal argument: self */
 BOUND_2140373124 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB1475407144_frame;

typedef struct ARRAYB1420960227_frame_struct {
 INT state;
 ARRAYB1925477728 self;/* Formal argument: self */
 BOUND_809390774 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB1420960227_frame;

typedef struct ARRAYB1615560602_frame_struct {
 INT state;
 ARRAYB1039347268 self;/* Formal argument: self */
 BUILTIN_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB1615560602_frame;

typedef struct ARRAYC494195798_frame_struct {
 INT state;
 ARRAYCALL_ARG self;/* Formal argument: self */
 CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC494195798_frame;

typedef struct ARRAYC1058209184_frame_struct {
 INT state;
 ARRAYCALL_ARG self;/* Formal argument: self */
 CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1058209184_frame;

typedef struct ARRAYCHAR_struct {/* layout for ARRAY{CHAR} */
 OB_HEADER header;
 INT asize;
 CHAR arr_part[1];
 } *ARRAYCHAR;

typedef struct ARRAYC1494984744_frame_struct {
 INT state;
 ARRAYCHAR self;/* Formal argument: self */
 CHAR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1494984744_frame;

typedef struct ARRAYC818595214_frame_struct {
 INT state;
 ARRAYC1438655838 self;/* Formal argument: self */
 CLASS_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC818595214_frame;

typedef struct ARRAYC1440407068_frame_struct {
 INT state;
 ARRAYC1788421899 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1440407068_frame;

typedef struct ARRAYE1693383532_frame_struct {
 INT state;
 ARRAYE2126408533 self;/* Formal argument: self */
 EXPR_HOISTED ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYE1693383532_frame;

typedef struct ARRAYE1687022731_frame_struct {
 INT state;
 ARRAYE406487263 self;/* Formal argument: self */
 EXTERNAL_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYE1687022731_frame;

typedef struct ARRAYIDENT_struct {/* layout for ARRAY{IDENT} */
 OB_HEADER header;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *ARRAYIDENT;

typedef struct ARRAYI1725945879_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 IDENT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1725945879_frame;

typedef struct ARRAYI789953351_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI789953351_frame;

typedef struct ARRAYI1177186037_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1177186037_frame;

typedef struct ARRAYI2003604369_frame_struct {
 INT state;
 ARRAYIFC self;/* Formal argument: self */
 IFC ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI2003604369_frame;

typedef struct ARRAYI1264630278_frame_struct {
 INT state;
 ARRAYI1327367692 self;/* Formal argument: self */
 IMMUTA421759893 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1264630278_frame;

typedef struct ARRAYI1719886982_frame_struct {
 INT state;
 ARRAYI1991954990 self;/* Formal argument: self */
 IMPL_INCLUDE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1719886982_frame;

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

typedef struct ARRAYI180735351_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI180735351_frame;

typedef struct ARRAYI486979141_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI486979141_frame;

typedef struct ARRAYO335724112_frame_struct {
 INT state;
 ARRAYO570594127 self;/* Formal argument: self */
 OPT_LOCAL_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYO335724112_frame;

typedef struct ARRAYSFILE_ID_struct {/* layout for ARRAY{SFILE_ID} */
 OB_HEADER header;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *ARRAYSFILE_ID;

typedef struct ARRAYS1372094358_frame_struct {
 INT state;
 ARRAYSFILE_ID self;/* Formal argument: self */
 SFILE_ID ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1372094358_frame;

typedef struct ARRAYS2023743485_frame_struct {
 INT state;
 ARRAYSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS2023743485_frame;

typedef struct ARRAYS1481566101_frame_struct {
 INT state;
 ARRAYSIG self;/* Formal argument: self */
 SIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1481566101_frame;

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

typedef struct ARRAYS1161048019_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1161048019_frame;

typedef struct ARRAYT1851819274_frame_struct {
 INT state;
 ARRAYTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1851819274_frame;

typedef struct ARRAYT1372201847_frame_struct {
 INT state;
 ARRAYTP_LAYOUT self;/* Formal argument: self */
 TP_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1372201847_frame;

typedef struct ARRAYT1275404834_struct {/* layout for ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *ARRAYT1275404834;

typedef struct ARRAYT310960754_frame_struct {
 INT state;
 ARRAYT1275404834 self;/* Formal argument: self */
 TUPAM_304458649 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT310960754_frame;

typedef struct ARRAYT1146239647_struct {/* layout for ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *ARRAYT1146239647;

typedef struct ARRAYT2108331328_frame_struct {
 INT state;
 ARRAYT1146239647 self;/* Formal argument: self */
 TUPAM_667892060 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT2108331328_frame;

typedef struct ARRAYT2065045042_struct {/* layout for ARRAY{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_927437481_struct arr_part[1];
 } *ARRAYT2065045042;

typedef struct ARRAYT1360634798_frame_struct {
 INT state;
 ARRAYT2065045042 self;/* Formal argument: self */
 TUPAM_927437481 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1360634798_frame;

typedef struct ARRAYT1753333425_struct {/* layout for ARRAY{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *ARRAYT1753333425;

typedef struct ARRAYT684852235_frame_struct {
 INT state;
 ARRAYT1753333425 self;/* Formal argument: self */
 TUPAM_1519040877 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT684852235_frame;

typedef struct ARRAYT1744744076_struct {/* layout for ARRAY{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *ARRAYT1744744076;

typedef struct ARRAYT342187846_frame_struct {
 INT state;
 ARRAYT1744744076 self;/* Formal argument: self */
 TUPAM_504653531 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT342187846_frame;

typedef struct ARRAYT546647214_struct {/* layout for ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *ARRAYT546647214;

typedef struct ARRAYT1758179858_frame_struct {
 INT state;
 ARRAYT546647214 self;/* Formal argument: self */
 TUPAM_1024994801 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1758179858_frame;

typedef struct ARRAYT1968212795_struct {/* layout for ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *ARRAYT1968212795;

typedef struct ARRAYT1101967660_frame_struct {
 INT state;
 ARRAYT1968212795 self;/* Formal argument: self */
 TUPAM_653469574 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1101967660_frame;

typedef struct ARRAYTUPIDENTdTP_struct {/* layout for ARRAY{TUP{IDENT,$TP}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *ARRAYTUPIDENTdTP;

typedef struct ARRAYT849390229_frame_struct {
 INT state;
 ARRAYTUPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT849390229_frame;

typedef struct ARRAYT263953675_struct {/* layout for ARRAY{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *ARRAYT263953675;

typedef struct ARRAYT451883912_frame_struct {
 INT state;
 ARRAYT263953675 self;/* Formal argument: self */
 TUPIDE641654124 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT451883912_frame;

typedef struct AS_FEAT_MOD_struct {/* layout for AS_FEAT_MOD */
 OB_HEADER header;
 struct AS_FEAT_MOD_struct *next;
 struct IDENT_struct name1;
 struct IDENT_struct new_name;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_FEAT_MOD;

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

typedef struct FLISTdAM_CONST_struct {/* layout for FLIST{$AM_CONST} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *FLISTdAM_CONST;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct ARRAYA277860273_frame_struct {
 INT state;
 ARRAYARRAYBOOL self;/* Formal argument: self */
 ARRAYBOOL ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA277860273_frame;

typedef struct ARRAYA1522064985_frame_struct {
 INT state;
 ARRAYAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1522064985_frame;

typedef struct ARRAYC1531429675_frame_struct {
 INT state;
 ARRAYCODE_FILE self;/* Formal argument: self */
 CODE_FILE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYC1531429675_frame;

typedef struct ARRAYF1657157762_frame_struct {
 INT state;
 ARRAYF1030817442 self;/* Formal argument: self */
 FLISTdAM_CONST ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF1657157762_frame;

typedef struct ARRAYF1006928322_frame_struct {
 INT state;
 ARRAYFLISTSTR self;/* Formal argument: self */
 FLISTSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF1006928322_frame;

typedef struct ARRAYF254484098_frame_struct {
 INT state;
 ARRAYFSTR self;/* Formal argument: self */
 FSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF254484098_frame;

typedef struct ARRAYF394786536_frame_struct {
 INT state;
 ARRAYFSTR self;/* Formal argument: self */
 FSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF394786536_frame;

#include "tags.h"

/* Globals */

extern INT ARRAYA876812706;
extern INT ARRAYF881388824;
extern INT ARRAYI56037128;
extern INT ARRAYS852413528;

/* Function declarations */

AM_ITE1809135850 ARRAYA494342579(ARRAYA494342579_frame);
AM_LOCAL_EXPR ARRAYA27295121(ARRAYA27295121_frame);
AM_ROUT_DEF ARRAYA1253289773(ARRAYA1253289773_frame);
AM_SHARED_EXPR ARRAYA1830087990(ARRAYA1830087990_frame);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYATTRSORT ARRAYA1114043836(ARRAYATTRSORT, INT);
ARRAYBOOL ARRAYA277860273(ARRAYA277860273_frame);
ARRAYINT ARRAYI413484363(ARRAYINT);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
ARRAYSTR ARRAYS1312593303(ARRAYSTR);
ARRAYSTR ARRAYS417083844(ARRAYSTR, ARRAYSTR);
ARRAYdAM_EXPR ARRAYA289850317(ARRAYA289850317_frame);
AS_FEAT_MOD ARRAYA1522064985(ARRAYA1522064985_frame);
ATTRSORT ARRAYA1660316962(ARRAYA1660316962_frame);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL ARRAYB651176924(ARRAYBOOL, BOOL);
BOOL ARRAYI801160309(ARRAYIDENT, IDENT);
BOOL FSTR_i673953987(FSTR, FSTR);
BOOL STR_is171745744(STR, STR);
BOUND_2140373124 ARRAYB1475407144(ARRAYB1475407144_frame);
BOUND_809390774 ARRAYB1420960227(ARRAYB1420960227_frame);
BUILTIN_LAYOUT ARRAYB1615560602(ARRAYB1615560602_frame);
CALL_ARG ARRAYC494195798(ARRAYC494195798_frame);
CHAR ARRAYC1494984744(ARRAYC1494984744_frame);
CLASS_LAYOUT ARRAYC818595214(ARRAYC818595214_frame);
CODE_FILE ARRAYC1531429675(ARRAYC1531429675_frame);
CSE_DOUBLE_EXPRS ARRAYC1440407068(ARRAYC1440407068_frame);
EXPR_HOISTED ARRAYE1693383532(ARRAYE1693383532_frame);
EXTERNAL_LAYOUT ARRAYE1687022731(ARRAYE1687022731_frame);
FLISTSTR ARRAYF1006928322(ARRAYF1006928322_frame);
FLISTdAM_CONST ARRAYF1657157762(ARRAYF1657157762_frame);
FSTR ARRAYF254484098(ARRAYF254484098_frame);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IFC ARRAYI2003604369(ARRAYI2003604369_frame);
IMMUTA421759893 ARRAYI1264630278(ARRAYI1264630278_frame);
IMPL_INCLUDE ARRAYI1719886982(ARRAYI1719886982_frame);
INT ARRAYA1077419174(ARRAYA1077419174_frame);
INT ARRAYA702716235(ARRAYA702716235_frame);
INT ARRAYA712604839(ARRAYA712604839_frame);
INT ARRAYI180735351(ARRAYI180735351_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT ARRAYI789953351(ARRAYI789953351_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT INT_do676118316(INT_do676118316_frame);
INT INT_up418511718(INT_up418511718_frame);
INT RND_in1575450710(RND, INT, INT);
OPT_LOCAL_EXPR ARRAYO335724112(ARRAYO335724112_frame);
SFILE_ID ARRAYS1372094358(ARRAYS1372094358_frame);
SIDE_EFFECT ARRAYS2023743485(ARRAYS2023743485_frame);
SIG ARRAYS1481566101(ARRAYS1481566101_frame);
STR ARRAYA1329352756(ARRAYARRAYSTR);
STR ARRAYA1368915033(ARRAYAM_ROUT_DEF);
STR ARRAYA1523920654(ARRAYA477251019);
STR ARRAYA1607624311(ARRAYATTRSORT);
STR ARRAYA208544369(ARRAYA1369744492);
STR ARRAYA449689863(ARRAYA1135548710);
STR ARRAYA565856506(ARRAYAS_FEAT_MOD);
STR ARRAYA704584979(ARRAYA1575632848);
STR ARRAYA881422470(ARRAYARRAYBOOL);
STR ARRAYARG_strrSTR(ARRAYARG);
STR ARRAYB1034562007(ARRAYB1039347268);
STR ARRAYB1997509424(ARRAYBOOL);
STR ARRAYB422115773(ARRAYB1925477728);
STR ARRAYB799780672(ARRAYB1234765325);
STR ARRAYC1188196456(ARRAYCHAR);
STR ARRAYC1509950809(ARRAYCODE_FILE);
STR ARRAYC15432117(ARRAYCALL_ARG);
STR ARRAYC1557608670(ARRAYC1788421899);
STR ARRAYC861356497(ARRAYC1438655838);
STR ARRAYE1256334724(ARRAYE406487263);
STR ARRAYE1673119298(ARRAYE2126408533);
STR ARRAYF1350792049(ARRAYFLISTSTR);
STR ARRAYF378827521(ARRAYFSTR);
STR ARRAYF852228085(ARRAYF1030817442);
STR ARRAYI1079246087(ARRAYI1327367692);
STR ARRAYI1551332689(ARRAYIDENT);
STR ARRAYI1980470469(ARRAYI1991954990);
STR ARRAYIFC_strrSTR(ARRAYIFC);
STR ARRAYINT_strrSTR(ARRAYINT);
STR ARRAYO2133079268(ARRAYO570594127);
STR ARRAYS1170645309(ARRAYSFILE_ID);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR ARRAYS2043945973(ARRAYSIDE_EFFECT);
STR ARRAYSIG_strrSTR(ARRAYSIG);
STR ARRAYSTR_strrSTR(ARRAYSTR);
STR ARRAYT1037471825(ARRAYT1275404834);
STR ARRAYT1054394674(ARRAYT1968212795);
STR ARRAYT1238370919(ARRAYTUPIDENTdTP);
STR ARRAYT1589477172(ARRAYTP_LAYOUT);
STR ARRAYT1626625951(ARRAYT546647214);
STR ARRAYT1671874055(ARRAYT1744744076);
STR ARRAYT1733874469(ARRAYTP_CLASS);
STR ARRAYT1803904833(ARRAYT2065045042);
STR ARRAYT1810565556(ARRAYT1753333425);
STR ARRAYT1926215796(ARRAYT1146239647);
STR ARRAYT568533288(ARRAYT263953675);
STR ARRAYd2023686048(ARRAYdAM_EXPR);
STR BOOL_strrSTR(BOOL);
STR FLISTSTR_strrSTR(FLISTSTR);
STR FLISTd1757835884(FLISTdAM_CONST);
STR IFC_strrSTR(IFC);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TUPAM_1128362628(TUPAM_304458649);
STR TUPAM_1694719258(TUPAM_1519040877);
STR TUPAM_489161612(TUPAM_927437481);
STR TUPAM_504028066(TUPAM_504653531);
STR TUPAM_525468809(TUPAM_653469574);
STR TUPAM_827739609(TUPAM_667892060);
STR TUPAM_85614060(TUPAM_1024994801);
STR TUPIDE1477036401(TUPIDE641654124);
STR TUPIDE2057908249(TUPIDENTdTP);
TP_CLASS ARRAYT1851819274(ARRAYT1851819274_frame);
TP_LAYOUT ARRAYT1372201847(ARRAYT1372201847_frame);
TUPAM_1024994801 ARRAYT1758179858(ARRAYT1758179858_frame);
TUPAM_1519040877 ARRAYT684852235(ARRAYT684852235_frame);
TUPAM_304458649 ARRAYT310960754(ARRAYT310960754_frame);
TUPAM_504653531 ARRAYT342187846(ARRAYT342187846_frame);
TUPAM_653469574 ARRAYT1101967660(ARRAYT1101967660_frame);
TUPAM_667892060 ARRAYT2108331328(ARRAYT2108331328_frame);
TUPAM_927437481 ARRAYT1360634798(ARRAYT1360634798_frame);
TUPIDE641654124 ARRAYT451883912(ARRAYT451883912_frame);
TUPIDENTdTP ARRAYT849390229(ARRAYT849390229_frame);
void ARRAYA107911980(ARRAYA107911980_frame);
void ARRAYA409651007(ARRAYA409651007_frame);
void ARRAYA487581188(ARRAYATTRSORT, INT, INT);
void ARRAYA555964192(ARRAYATTRSORT, INT, INT);
void ARRAYB741586158(ARRAYB741586158_frame);
void ARRAYC1058209184(ARRAYC1058209184_frame);
void ARRAYF1076245134(ARRAYFSTR, INT, INT);
void ARRAYF1978661014(ARRAYFSTR, INT, INT);
void ARRAYF394786536(ARRAYF394786536_frame);
void ARRAYI1050890878(ARRAYIDENT, INT, INT);
void ARRAYI1177186037(ARRAYI1177186037_frame);
void ARRAYI2092236385(ARRAYIDENT, INT, INT);
void ARRAYI486979141(ARRAYI486979141_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void ARRAYS37144603(ARRAYSTR, INT, INT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

ARRAYATTRSORT ARRAYA1114043836(ARRAYATTRSORT self, INT n) {
 ARRAYATTRSORT ret_val;
 ARRAYATTRSORT res;
 ARRAYATTRSORT L11;
 ARRAYATTRSORT L21;
 ARRAYATTRSORT create_self;
 INT create_n = INT_zero;
 ARRAYATTRSORT ret_val1;
 INT L31 = INT_zero;
 ARRAYATTRSORT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_br;
 ATTRSORT aL81_;
 INT L91_br;
 ATTRSORT L10;
 ATTRSORT L12;
 INT L131_;
 create_self = ((ARRAYATTRSORT) NULL);
 create_n = n;
 L5 = create_n;
 L7=(sizeof(struct ARRAYATTRSORT_struct)-sizeof(ATTRSORT))+(L5)*sizeof(ATTRSORT);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYATTRSORT_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYATTRSORT) L6);
 L4->asize = L5;
 ret_val1 = L4;
 res = ret_val1;
 {
  BOOL f_L81_ = TRUE;
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = self;
  L21 = res;
  L81_br=L11==NULL?0:ASIZE((ARRAYATTRSORT)L11); 
  L91_br=L21==NULL?0:ASIZE((ARRAYATTRSORT)L21); 
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   if(L31>=L81_br)  goto after_loop; 
   aL81_=ARR((ARRAYATTRSORT)L11,L31); 
   L12 = aL81_;
   SARR((ARRAYATTRSORT)L21,L31,(ATTRSORT)L12); 
   ;
   L131_=INTPLUS(L31,1); 
   L31 = L131_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYINT ARRAYI413484363(ARRAYINT self) {
 ARRAYINT ret_val;
 ARRAYINT r;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val1;
 INT L11_;
 ARRAYINT L2;
 INT L3;
 OB L4;
 INT L5;
 if ((self==((ARRAYINT) NULL))) {
  ret_val = ((ARRAYINT) NULL);
  return ret_val;
 }
 create_self = self;
 L11_=(self)==NULL?0:ASIZE((ARRAYINT)self); 
 create_n = L11_;
 L3 = create_n;
 L5=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L3)*sizeof(INT);
 L4=ZALLOC_LEAF_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 memset(L4,0,L5);
 ((OB)L4)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L2 = ((ARRAYINT) L4);
 L2->asize = L3;
 ret_val1 = L2;
 r = ret_val1;
 if(r!=NULL && self!=NULL) AREFACOPY(r,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR ARRAYS1312593303(ARRAYSTR self) {
 ARRAYSTR ret_val;
 ARRAYSTR r;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 INT L11_;
 ARRAYSTR L2;
 INT L3;
 OB L4;
 INT L5;
 if ((self==((ARRAYSTR) NULL))) {
  ret_val = ((ARRAYSTR) NULL);
  return ret_val;
 }
 create_self = self;
 L11_=(self)==NULL?0:ASIZE((ARRAYSTR)self); 
 create_n = L11_;
 L3 = create_n;
 L5=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L3)*sizeof(STR);
 L4=ZALLOC_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L2 = ((ARRAYSTR) L4);
 L2->asize = L3;
 ret_val1 = L2;
 r = ret_val1;
 if(r!=NULL && self!=NULL) AREFACOPY(r,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR ARRAYS417083844(ARRAYSTR self, ARRAYSTR a) {
 ARRAYSTR ret_val;
 ARRAYSTR r;
 ARRAYSTR L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 OB L8;
 INT L9;
 INT L111_;
 INT L12;
 if ((self==((ARRAYSTR) NULL))) {
  ret_val = ARRAYS1312593303(a);
  return ret_val;
 }
 else {
  if ((a==((ARRAYSTR) NULL))) {
   ret_val = ARRAYS1312593303(self);
   return ret_val;
  }
  else {
   L31_=(self)==NULL?0:ASIZE((ARRAYSTR)self); 
   L4 = L31_;
   L51_=(a)==NULL?0:ASIZE((ARRAYSTR)a); 
   L6 = L51_;
   L71_=INTPLUS(L4,L6); 
   L2 = L71_;
   L9=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L2)*sizeof(STR);
   L8=ZALLOC_BIG(L9);
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L8)->header.destroyed=0;
#endif

   L1 = ((ARRAYSTR) L8);
   L1->asize = L2;
   r = L1;
   if(r!=NULL && self!=NULL) AREFACOPY(r,self); 
   ;
   L111_=(self)==NULL?0:ASIZE((ARRAYSTR)self); 
   L12 = L111_;
   if(a!=NULL) AREFACOPYB(r,L12,a); 
   ;
   ret_val = r;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ARRAYB651176924(ARRAYBOOL self, BOOL e) {
 BOOL ret_val = BOOL_zero;
 ARRAYBOOL L11;
 INT L21 = INT_zero;
 ARRAYBOOL elt_eq_self;
 BOOL elt_eq_e1 = BOOL_zero;
 BOOL elt_eq_e2 = BOOL_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQBOOL elt_eq_self1;
 BOOL elt_eq_e11 = BOOL_zero;
 BOOL elt_eq_e21 = BOOL_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L31_br;
 BOOL aL31_;
 BOOL L4;
 BOOL L51_;
 INT L61_;
 if ((self==((ARRAYBOOL) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = self;
  L31_br=L11==NULL?0:ASIZE((ARRAYBOOL)L11); 
  while (1) {
   elt_eq_self = self;
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYBOOL)L11,L21); 
   elt_eq_e1 = aL31_;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQBOOL) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   L51_=elt_eq_e11==elt_eq_e21; 
   ret_val2 = L51_;
   ret_val1 = ret_val2;
   if (ret_val1) {
    ret_val = TRUE;
    return ret_val;
   }
   L61_=INTPLUS(L21,1); 
   L21 = L61_;
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL ARRAYI801160309(ARRAYIDENT self, IDENT e) {
 BOOL ret_val = BOOL_zero;
 ARRAYIDENT L11;
 INT L21 = INT_zero;
 ARRAYIDENT elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 INT L31_br;
 IDENT aL31_;
 IDENT L4;
 INT L51_;
 if ((self==((ARRAYIDENT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = self;
  L31_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
  while (1) {
   elt_eq_self = self;
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYIDENT)L11,L21); 
   elt_eq_e1 = aL31_;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQIDENT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_i = elt_eq_e21;
   ret_val3 = (is_eq_self.str==is_eq_i.str);
   ret_val2 = ret_val3;
   ret_val1 = ret_val2;
   if (ret_val1) {
    ret_val = TRUE;
    return ret_val;
   }
   L51_=INTPLUS(L21,1); 
   L21 = L51_;
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1329352756(ARRAYARRAYSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ARRAYSTR e;
 ARRAYARRAYSTR elt_str_self;
 ARRAYSTR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYARRAYSTR elt_str_self1;
 ARRAYSTR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ARRAYSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYARRAYSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ARRAYSTR)ARR((ARRAYARRAYSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ARRAYSTR_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ARRAYSTR_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1368915033(ARRAYAM_ROUT_DEF self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_ROUT_DEF e;
 ARRAYAM_ROUT_DEF elt_str_self;
 AM_ROUT_DEF elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYAM_ROUT_DEF elt_str_self1;
 AM_ROUT_DEF elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_ROUT_DEF L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYAM_ROUT_DEF)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_ROUT_DEF)ARR((ARRAYAM_ROUT_DEF)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1523920654(ARRAYA477251019 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ARRAYdAM_EXPR e;
 ARRAYA477251019 elt_str_self;
 ARRAYdAM_EXPR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYA477251019 elt_str_self1;
 ARRAYdAM_EXPR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ARRAYdAM_EXPR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA477251019)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ARRAYdAM_EXPR)ARR((ARRAYA477251019)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ARRAYd2023686048(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ARRAYd2023686048(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA1607624311(ARRAYATTRSORT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ATTRSORT e;
 ARRAYATTRSORT elt_str_self;
 ATTRSORT elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYATTRSORT elt_str_self1;
 ATTRSORT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ATTRSORT L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYATTRSORT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ATTRSORT)ARR((ARRAYATTRSORT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA208544369(ARRAYA1369744492 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_ITE1809135850 e;
 ARRAYA1369744492 elt_str_self;
 AM_ITE1809135850 elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA1369744492 elt_str_self1;
 AM_ITE1809135850 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_ITE1809135850 L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA1369744492)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_ITE1809135850)ARR((ARRAYA1369744492)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA449689863(ARRAYA1135548710 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_SHARED_EXPR e;
 ARRAYA1135548710 elt_str_self;
 AM_SHARED_EXPR elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA1135548710 elt_str_self1;
 AM_SHARED_EXPR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_SHARED_EXPR L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA1135548710)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_SHARED_EXPR)ARR((ARRAYA1135548710)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA565856506(ARRAYAS_FEAT_MOD self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AS_FEAT_MOD e;
 ARRAYAS_FEAT_MOD elt_str_self;
 AS_FEAT_MOD elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYAS_FEAT_MOD elt_str_self1;
 AS_FEAT_MOD elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AS_FEAT_MOD L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYAS_FEAT_MOD)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AS_FEAT_MOD)ARR((ARRAYAS_FEAT_MOD)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA704584979(ARRAYA1575632848 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 AM_LOCAL_EXPR e;
 ARRAYA1575632848 elt_str_self;
 AM_LOCAL_EXPR elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYA1575632848 elt_str_self1;
 AM_LOCAL_EXPR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 AM_LOCAL_EXPR L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYA1575632848)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYA881422470(ARRAYARRAYBOOL self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ARRAYBOOL e;
 ARRAYARRAYBOOL elt_str_self;
 ARRAYBOOL elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYARRAYBOOL elt_str_self1;
 ARRAYBOOL elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ARRAYBOOL L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYARRAYBOOL)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ARRAYBOOL)ARR((ARRAYARRAYBOOL)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ARRAYB1997509424(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ARRAYB1997509424(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYARG_strrSTR(ARRAYARG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 ARG e;
 ARRAYARG elt_str_self;
 ARG elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYARG elt_str_self1;
 ARG elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 ARG L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYARG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(ARG)ARR((ARRAYARG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYB1034562007(ARRAYB1039347268 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 BUILTIN_LAYOUT e;
 ARRAYB1039347268 elt_str_self;
 BUILTIN_LAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYB1039347268 elt_str_self1;
 BUILTIN_LAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 BUILTIN_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYB1039347268)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(BUILTIN_LAYOUT)ARR((ARRAYB1039347268)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYB1997509424(ARRAYBOOL self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 BOOL e = BOOL_zero;
 ARRAYBOOL elt_str_self;
 BOOL elt_str_e = BOOL_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYBOOL elt_str_self1;
 BOOL elt_str_e1 = BOOL_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 BOOL L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYBOOL)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYBOOL)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = BOOL_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = BOOL_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYB422115773(ARRAYB1925477728 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 BOUND_809390774 e;
 ARRAYB1925477728 elt_str_self;
 BOUND_809390774 elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYB1925477728 elt_str_self1;
 BOUND_809390774 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 BOUND_809390774 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYB1925477728)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(BOUND_809390774)ARR((ARRAYB1925477728)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYB799780672(ARRAYB1234765325 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 BOUND_2140373124 e;
 ARRAYB1234765325 elt_str_self;
 BOUND_2140373124 elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYB1234765325 elt_str_self1;
 BOUND_2140373124 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 BOUND_2140373124 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYB1234765325)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(BOUND_2140373124)ARR((ARRAYB1234765325)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYC1188196456(ARRAYCHAR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 CHAR e = CHAR_zero;
 ARRAYCHAR elt_str_self;
 CHAR elt_str_e = CHAR_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 ARRAYCHAR elt_str_self1;
 CHAR elt_str_e1 = CHAR_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val4;
 CHAR str_self1 = CHAR_zero;
 STR ret_val5;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val6;
 STR r1;
 FSTR str_self2;
 STR ret_val7;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 CHAR L41_;
 BOOL L51_;
 STR L6;
 INT L7;
 OB L8;
 INT L9;
 extern STR name3;
 FSTR L12;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 INT L181_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYCHAR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYCHAR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   str_self = elt_str_e;
   create_self = ((STR) NULL);
   create_c = str_self;
   L7 = 1;
   L9=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L7)*sizeof(CHAR);
   L8=ZALLOC_LEAF_BIG(L9);
   if (L8==NULL) FATAL("Unable to allocate more memory");
   memset(L8,0,L9);
   ((OB)L8)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L8)->header.destroyed=0;
#endif

   L6 = ((STR) L8);
   L6->asize = L7;
   r = L6;
   SARR((STR)r,0,create_c); 
   ;
   ret_val3 = r;
   ret_val2 = ret_val3;
   ret_val1 = ret_val2;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L12 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   str_self1 = elt_str_e1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L14 = 1;
   L16=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L14)*sizeof(CHAR);
   L15=ZALLOC_LEAF_BIG(L16);
   if (L15==NULL) FATAL("Unable to allocate more memory");
   memset(L15,0,L16);
   ((OB)L15)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L15)->header.destroyed=0;
#endif

   L13 = ((STR) L15);
   L13->asize = L14;
   r1 = L13;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val6 = r1;
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   res = FSTR_p1752847026(L12, ret_val4);
  }
  L181_=INTPLUS(i,1); 
  i = L181_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYC1509950809(ARRAYCODE_FILE self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 CODE_FILE e;
 ARRAYCODE_FILE elt_str_self;
 CODE_FILE elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYCODE_FILE elt_str_self1;
 CODE_FILE elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 CODE_FILE L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYCODE_FILE)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(CODE_FILE)ARR((ARRAYCODE_FILE)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYC15432117(ARRAYCALL_ARG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 CALL_ARG e;
 ARRAYCALL_ARG elt_str_self;
 CALL_ARG elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYCALL_ARG elt_str_self1;
 CALL_ARG elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 CALL_ARG L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYCALL_ARG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(CALL_ARG)ARR((ARRAYCALL_ARG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYC1557608670(ARRAYC1788421899 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 CSE_DOUBLE_EXPRS e;
 ARRAYC1788421899 elt_str_self;
 CSE_DOUBLE_EXPRS elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYC1788421899 elt_str_self1;
 CSE_DOUBLE_EXPRS elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 CSE_DOUBLE_EXPRS L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYC1788421899)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(CSE_DOUBLE_EXPRS)ARR((ARRAYC1788421899)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYC861356497(ARRAYC1438655838 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 CLASS_LAYOUT e;
 ARRAYC1438655838 elt_str_self;
 CLASS_LAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYC1438655838 elt_str_self1;
 CLASS_LAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 CLASS_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYC1438655838)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(CLASS_LAYOUT)ARR((ARRAYC1438655838)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYE1256334724(ARRAYE406487263 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 EXTERNAL_LAYOUT e;
 ARRAYE406487263 elt_str_self;
 EXTERNAL_LAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYE406487263 elt_str_self1;
 EXTERNAL_LAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 EXTERNAL_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYE406487263)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(EXTERNAL_LAYOUT)ARR((ARRAYE406487263)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYE1673119298(ARRAYE2126408533 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 EXPR_HOISTED e;
 ARRAYE2126408533 elt_str_self;
 EXPR_HOISTED elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYE2126408533 elt_str_self1;
 EXPR_HOISTED elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 EXPR_HOISTED L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYE2126408533)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(EXPR_HOISTED)ARR((ARRAYE2126408533)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYF1350792049(ARRAYFLISTSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 FLISTSTR e;
 ARRAYFLISTSTR elt_str_self;
 FLISTSTR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYFLISTSTR elt_str_self1;
 FLISTSTR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 FLISTSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYFLISTSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(FLISTSTR)ARR((ARRAYFLISTSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = FLISTSTR_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = FLISTSTR_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYF378827521(ARRAYFSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 FSTR e;
 ARRAYFSTR elt_str_self;
 FSTR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 FSTR str_self;
 STR ret_val2;
 ARRAYFSTR elt_str_self1;
 FSTR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 FSTR str_self2;
 STR ret_val5;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 FSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYFSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(FSTR)ARR((ARRAYFSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   str_self = elt_str_e;
   ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
   ret_val1 = ret_val2;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   str_self1 = elt_str_e1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val3 = ret_val4;
   res = FSTR_p1752847026(L6, ret_val3);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYF852228085(ARRAYF1030817442 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 FLISTdAM_CONST e;
 ARRAYF1030817442 elt_str_self;
 FLISTdAM_CONST elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYF1030817442 elt_str_self1;
 FLISTdAM_CONST elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 FLISTdAM_CONST L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYF1030817442)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(FLISTdAM_CONST)ARR((ARRAYF1030817442)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = FLISTd1757835884(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = FLISTd1757835884(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYI1079246087(ARRAYI1327367692 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 IMMUTA421759893 e;
 ARRAYI1327367692 elt_str_self;
 IMMUTA421759893 elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYI1327367692 elt_str_self1;
 IMMUTA421759893 elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 IMMUTA421759893 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYI1327367692)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(IMMUTA421759893)ARR((ARRAYI1327367692)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYI1551332689(ARRAYIDENT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 IDENT e = IDENT_zero;
 ARRAYIDENT elt_str_self;
 IDENT elt_str_e = IDENT_zero;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYIDENT elt_str_self1;
 IDENT elt_str_e1 = IDENT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 IDENT L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYIDENT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYIDENT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYI1980470469(ARRAYI1991954990 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 IMPL_INCLUDE e;
 ARRAYI1991954990 elt_str_self;
 IMPL_INCLUDE elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYI1991954990 elt_str_self1;
 IMPL_INCLUDE elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 IMPL_INCLUDE L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYI1991954990)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(IMPL_INCLUDE)ARR((ARRAYI1991954990)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYIFC_strrSTR(ARRAYIFC self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 IFC e;
 ARRAYIFC elt_str_self;
 IFC elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYIFC elt_str_self1;
 IFC elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 IFC L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYIFC)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(IFC)ARR((ARRAYIFC)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = IFC_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = IFC_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYINT_strrSTR(ARRAYINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 INT e = INT_zero;
 ARRAYINT elt_str_self;
 INT elt_str_e = INT_zero;
 INT elt_str_i = INT_zero;
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
 ARRAYINT elt_str_self1;
 INT elt_str_e1 = INT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val5;
 INT str_self2 = INT_zero;
 STR ret_val6;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val7;
 FSTR str_self3;
 STR ret_val8;
 FSTR str_self4;
 STR ret_val9;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 INT L41_;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYINT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   str_self = elt_str_e;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   ret_val1 = ret_val2;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   str_self2 = elt_str_e1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val7 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val7;
   str_self3 = buf2;
   ret_val8 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val6 = ret_val8;
   ret_val5 = ret_val6;
   res = FSTR_p1752847026(L8, ret_val5);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val9 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val9;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYO2133079268(ARRAYO570594127 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 OPT_LOCAL_EXPR e;
 ARRAYO570594127 elt_str_self;
 OPT_LOCAL_EXPR elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYO570594127 elt_str_self1;
 OPT_LOCAL_EXPR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 OPT_LOCAL_EXPR L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYO570594127)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYS1170645309(ARRAYSFILE_ID self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 SFILE_ID e = SFILE_ID_zero;
 ARRAYSFILE_ID elt_str_self;
 SFILE_ID elt_str_e = SFILE_ID_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYSFILE_ID elt_str_self1;
 SFILE_ID elt_str_e1 = SFILE_ID_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 SFILE_ID L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYSFILE_ID)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYSFILE_ID)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = SFILE_ID_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = SFILE_ID_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYS2043945973(ARRAYSIDE_EFFECT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 SIDE_EFFECT e;
 ARRAYSIDE_EFFECT elt_str_self;
 SIDE_EFFECT elt_str_e;
 INT elt_str_i = INT_zero;
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
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 ARRAYSIDE_EFFECT elt_str_self1;
 SIDE_EFFECT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val6;
 INT str_self2 = INT_zero;
 STR ret_val7;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val8;
 FSTR str_self3;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 FSTR str_self4;
 STR ret_val11;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 SIDE_EFFECT L41_;
 BOOL L51_;
 extern STR Unprintable;
 BOOL L6;
 BOOL L71_;
 extern STR name3;
 FSTR L8;
 extern STR Unprintable;
 BOOL L9;
 BOOL L101_;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYSIDE_EFFECT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(SIDE_EFFECT)ARR((ARRAYSIDE_EFFECT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   plus_self = ((STR) &Unprintable);
   str_self = elt_str_i;
   clear_self = buf1;
   L6 = (clear_self==((FSTR) NULL));
   L71_=!(L6); 
   if (L71_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val1 = ret_val5;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L8 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   plus_self1 = ((STR) &Unprintable);
   str_self2 = elt_str_i1;
   clear_self1 = buf2;
   L9 = (clear_self1==((FSTR) NULL));
   L101_=!(L9); 
   if (L101_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val8 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val8;
   str_self3 = buf2;
   ret_val9 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val7 = ret_val9;
   plus_sarg1 = ret_val7;
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   ret_val6 = ret_val10;
   res = FSTR_p1752847026(L8, ret_val6);
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val11 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYSIG_strrSTR(ARRAYSIG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 SIG e;
 ARRAYSIG elt_str_self;
 SIG elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYSIG elt_str_self1;
 SIG elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 SIG L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYSIG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(SIG)ARR((ARRAYSIG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYSTR_strrSTR(ARRAYSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 STR e;
 ARRAYSTR elt_str_self;
 STR elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 STR str_self;
 STR ret_val2;
 ARRAYSTR elt_str_self1;
 STR elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val3;
 STR str_self1;
 STR ret_val4;
 FSTR str_self2;
 STR ret_val5;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 STR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(STR)ARR((ARRAYSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   str_self = elt_str_e;
   ret_val2 = str_self;
   ret_val1 = ret_val2;
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   str_self1 = elt_str_e1;
   ret_val4 = str_self1;
   ret_val3 = ret_val4;
   res = FSTR_p1752847026(L6, ret_val3);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1037471825(ARRAYT1275404834 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_304458649 e = TUPAM_304458649_zero;
 ARRAYT1275404834 elt_str_self;
 TUPAM_304458649 elt_str_e = TUPAM_304458649_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1275404834 elt_str_self1;
 TUPAM_304458649 elt_str_e1 = TUPAM_304458649_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_304458649 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1275404834)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1275404834)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_1128362628(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_1128362628(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1054394674(ARRAYT1968212795 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_653469574 e = TUPAM_653469574_zero;
 ARRAYT1968212795 elt_str_self;
 TUPAM_653469574 elt_str_e = TUPAM_653469574_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1968212795 elt_str_self1;
 TUPAM_653469574 elt_str_e1 = TUPAM_653469574_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_653469574 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1968212795)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1968212795)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_525468809(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_525468809(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1238370919(ARRAYTUPIDENTdTP self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPIDENTdTP e = TUPIDENTdTP_zero;
 ARRAYTUPIDENTdTP elt_str_self;
 TUPIDENTdTP elt_str_e = TUPIDENTdTP_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPIDENTdTP elt_str_self1;
 TUPIDENTdTP elt_str_e1 = TUPIDENTdTP_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPIDENTdTP L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPIDENTdTP)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPIDENTdTP)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPIDE2057908249(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPIDE2057908249(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1589477172(ARRAYTP_LAYOUT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TP_LAYOUT e;
 ARRAYTP_LAYOUT elt_str_self;
 TP_LAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTP_LAYOUT elt_str_self1;
 TP_LAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TP_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTP_LAYOUT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(TP_LAYOUT)ARR((ARRAYTP_LAYOUT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = ATTR(elt_str_e,str);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = ATTR(elt_str_e1,str);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1626625951(ARRAYT546647214 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_1024994801 e = TUPAM_1024994801_zero;
 ARRAYT546647214 elt_str_self;
 TUPAM_1024994801 elt_str_e = TUPAM_1024994801_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT546647214 elt_str_self1;
 TUPAM_1024994801 elt_str_e1 = TUPAM_1024994801_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_1024994801 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT546647214)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT546647214)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_85614060(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_85614060(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1671874055(ARRAYT1744744076 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_504653531 e = TUPAM_504653531_zero;
 ARRAYT1744744076 elt_str_self;
 TUPAM_504653531 elt_str_e = TUPAM_504653531_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1744744076 elt_str_self1;
 TUPAM_504653531 elt_str_e1 = TUPAM_504653531_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_504653531 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1744744076)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1744744076)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_504028066(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_504028066(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1733874469(ARRAYTP_CLASS self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TP_CLASS e;
 ARRAYTP_CLASS elt_str_self;
 TP_CLASS elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTP_CLASS elt_str_self1;
 TP_CLASS elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TP_CLASS L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTP_CLASS)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(TP_CLASS)ARR((ARRAYTP_CLASS)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TP_CLASS_strrSTR(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TP_CLASS_strrSTR(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1803904833(ARRAYT2065045042 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_927437481 e = TUPAM_927437481_zero;
 ARRAYT2065045042 elt_str_self;
 TUPAM_927437481 elt_str_e = TUPAM_927437481_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT2065045042 elt_str_self1;
 TUPAM_927437481 elt_str_e1 = TUPAM_927437481_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_927437481 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT2065045042)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT2065045042)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_489161612(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_489161612(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1810565556(ARRAYT1753333425 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_1519040877 e = TUPAM_1519040877_zero;
 ARRAYT1753333425 elt_str_self;
 TUPAM_1519040877 elt_str_e = TUPAM_1519040877_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1753333425 elt_str_self1;
 TUPAM_1519040877 elt_str_e1 = TUPAM_1519040877_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_1519040877 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1753333425)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1753333425)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_1694719258(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_1694719258(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT1926215796(ARRAYT1146239647 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_667892060 e = TUPAM_667892060_zero;
 ARRAYT1146239647 elt_str_self;
 TUPAM_667892060 elt_str_e = TUPAM_667892060_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1146239647 elt_str_self1;
 TUPAM_667892060 elt_str_e1 = TUPAM_667892060_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_667892060 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1146239647)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1146239647)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPAM_827739609(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPAM_827739609(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT568533288(ARRAYT263953675 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPIDE641654124 e = TUPIDE641654124_zero;
 ARRAYT263953675 elt_str_self;
 TUPIDE641654124 elt_str_e = TUPIDE641654124_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT263953675 elt_str_self1;
 TUPIDE641654124 elt_str_e1 = TUPIDE641654124_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPIDE641654124 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT263953675)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT263953675)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPIDE1477036401(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPIDE1477036401(elt_str_e1);
   res = FSTR_p1752847026(L6, ret_val2);
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYA487581188(ARRAYATTRSORT self, INT l, INT u) {
 INT r = INT_zero;
 ATTRSORT t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ATTRSORT s;
 ARRAYATTRSORT elt_lt_self;
 ATTRSORT elt_lt_e1;
 ATTRSORT elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTATTRSORT elt_lt_self1;
 ATTRSORT elt_lt_e11;
 ATTRSORT elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 ATTRSORT is_lt_self;
 ATTRSORT is_lt_b;
 BOOL ret_val2 = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 ATTRSORT L71_;
 ATTRSORT L81_;
 ATTRSORT L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 ATTRSORT L161_;
 INT L171_;
 ATTRSORT L181_;
 ATTRSORT L191_;
 ATTRSORT L20;
 ATTRSORT L241_;
 ATTRSORT L251_;
 ATTRSORT L26;
 INT L291_;
 INT L30;
 BOOL L311_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 L3 = ARRAYA876812706;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(ATTRSORT)ARR((ARRAYATTRSORT)self,r); 
  t = L71_;
  L81_=(ATTRSORT)ARR((ARRAYATTRSORT)self,l); 
  L9 = L81_;
  SARR((ARRAYATTRSORT)self,r,(ATTRSORT)L9); 
  ;
  SARR((ARRAYATTRSORT)self,l,(ATTRSORT)t); 
  ;
  m = l;
  {
   BOOL f_L131_ = TRUE;
   L141_=INTPLUS(l,1); 
   L11 = L141_;
   L21 = u;
   L131_=L11-1;L131_m=L21; 
   while (1) {
    if(L131_++>=L131_m)  goto after_loop; 
    i = L131_;
    elt_lt_self = self;
    L161_=(ATTRSORT)ARR((ARRAYATTRSORT)self,i); 
    elt_lt_e1 = L161_;
    elt_lt_e2 = t;
    elt_lt_self1 = ((ELT_LTATTRSORT) NULL);
    elt_lt_e11 = elt_lt_e1;
    elt_lt_e21 = elt_lt_e2;
    is_lt_self = elt_lt_e11;
    is_lt_b = elt_lt_e21;
    ret_val2 = STR_is171745744(ATTR(is_lt_self,name1), ATTR(is_lt_b,name1));
    ret_val1 = ret_val2;
    ret_val = ret_val1;
    if (ret_val) {
     L171_=INTPLUS(m,1); 
     m = L171_;
     L181_=(ATTRSORT)ARR((ARRAYATTRSORT)self,m); 
     s = L181_;
     L191_=(ATTRSORT)ARR((ARRAYATTRSORT)self,i); 
     L20 = L191_;
     SARR((ARRAYATTRSORT)self,m,(ATTRSORT)L20); 
     ;
     SARR((ARRAYATTRSORT)self,i,(ATTRSORT)s); 
     ;
    }
   }
  }
  after_loop: ;
  L241_=(ATTRSORT)ARR((ARRAYATTRSORT)self,l); 
  t = L241_;
  L251_=(ATTRSORT)ARR((ARRAYATTRSORT)self,m); 
  L26 = L251_;
  SARR((ARRAYATTRSORT)self,l,(ATTRSORT)L26); 
  ;
  SARR((ARRAYATTRSORT)self,m,(ATTRSORT)t); 
  ;
  L291_=INTMINUS(m,1); 
  L30 = L291_;
  L311_=(l)<(L30); 
  if (L311_) {
   L321_=INTMINUS(m,1); 
   ARRAYA487581188(self, l, L321_);
  }
  L331_=INTPLUS(m,1); 
  L34 = L331_;
  L351_=(L34)<(u); 
  if (L351_) {
   L361_=INTPLUS(m,1); 
   L37 = L361_;
   ARRAYA487581188(self, L37, u);
  }
 }
 else {
  ARRAYA555964192(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYA555964192(ARRAYATTRSORT self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ATTRSORT e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 ARRAYATTRSORT elt_lt_self;
 ATTRSORT elt_lt_e1;
 ATTRSORT elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTATTRSORT elt_lt_self1;
 ATTRSORT elt_lt_e11;
 ATTRSORT elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 ATTRSORT is_lt_self;
 ATTRSORT is_lt_b;
 BOOL ret_val2 = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 ATTRSORT L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 ATTRSORT L161_;
 INT L171_;
 INT L18;
 INT L201_;
 INT L22;
 ATTRSORT L231_;
 ATTRSORT L24;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(ATTRSORT)ARR((ARRAYATTRSORT)self,i); 
   e = L81_;
   {
    BOOL f_L91_ = TRUE;
    L101_=INTMINUS(i,1); 
    L31 = L101_;
    L121_=INTMINUS(l,1); 
    L41 = L121_;
    L91_=L31+1;L91_m=L41; 
    while (1) {
     if(L91_--<=L91_m)  goto after_loop1; 
     j = L91_;
     L141_=(j)<(l); 
     if (L141_) {
      SARR((ARRAYATTRSORT)self,l,(ATTRSORT)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(ATTRSORT)ARR((ARRAYATTRSORT)self,j); 
      elt_lt_e1 = L161_;
      elt_lt_e2 = e;
      elt_lt_self1 = ((ELT_LTATTRSORT) NULL);
      elt_lt_e11 = elt_lt_e1;
      elt_lt_e21 = elt_lt_e2;
      is_lt_self = elt_lt_e11;
      is_lt_b = elt_lt_e21;
      ret_val2 = STR_is171745744(ATTR(is_lt_self,name1), ATTR(is_lt_b,name1));
      ret_val1 = ret_val2;
      ret_val = ret_val1;
      if (ret_val) {
       L171_=INTPLUS(j,1); 
       L18 = L171_;
       SARR((ARRAYATTRSORT)self,L18,(ATTRSORT)e); 
       ;
       goto after_loop1;
      }
      else {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       L231_=(ATTRSORT)ARR((ARRAYATTRSORT)self,j); 
       L24 = L231_;
       SARR((ARRAYATTRSORT)self,L22,(ATTRSORT)L24); 
       ;
      }
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYF1076245134(ARRAYFSTR self, INT l, INT u) {
 INT r = INT_zero;
 FSTR t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 FSTR s;
 ARRAYFSTR elt_lt_self;
 FSTR elt_lt_e1;
 FSTR elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTFSTR elt_lt_self1;
 FSTR elt_lt_e11;
 FSTR elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 FSTR L71_;
 FSTR L81_;
 FSTR L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 FSTR L161_;
 INT L171_;
 FSTR L181_;
 FSTR L191_;
 FSTR L20;
 FSTR L241_;
 FSTR L251_;
 FSTR L26;
 INT L291_;
 INT L30;
 BOOL L311_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 L3 = ARRAYF881388824;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(FSTR)ARR((ARRAYFSTR)self,r); 
  t = L71_;
  L81_=(FSTR)ARR((ARRAYFSTR)self,l); 
  L9 = L81_;
  SARR((ARRAYFSTR)self,r,(FSTR)L9); 
  ;
  SARR((ARRAYFSTR)self,l,(FSTR)t); 
  ;
  m = l;
  {
   BOOL f_L131_ = TRUE;
   L141_=INTPLUS(l,1); 
   L11 = L141_;
   L21 = u;
   L131_=L11-1;L131_m=L21; 
   while (1) {
    if(L131_++>=L131_m)  goto after_loop; 
    i = L131_;
    elt_lt_self = self;
    L161_=(FSTR)ARR((ARRAYFSTR)self,i); 
    elt_lt_e1 = L161_;
    elt_lt_e2 = t;
    elt_lt_self1 = ((ELT_LTFSTR) NULL);
    elt_lt_e11 = elt_lt_e1;
    elt_lt_e21 = elt_lt_e2;
    ret_val1 = FSTR_i673953987(elt_lt_e11, elt_lt_e21);
    ret_val = ret_val1;
    if (ret_val) {
     L171_=INTPLUS(m,1); 
     m = L171_;
     L181_=(FSTR)ARR((ARRAYFSTR)self,m); 
     s = L181_;
     L191_=(FSTR)ARR((ARRAYFSTR)self,i); 
     L20 = L191_;
     SARR((ARRAYFSTR)self,m,(FSTR)L20); 
     ;
     SARR((ARRAYFSTR)self,i,(FSTR)s); 
     ;
    }
   }
  }
  after_loop: ;
  L241_=(FSTR)ARR((ARRAYFSTR)self,l); 
  t = L241_;
  L251_=(FSTR)ARR((ARRAYFSTR)self,m); 
  L26 = L251_;
  SARR((ARRAYFSTR)self,l,(FSTR)L26); 
  ;
  SARR((ARRAYFSTR)self,m,(FSTR)t); 
  ;
  L291_=INTMINUS(m,1); 
  L30 = L291_;
  L311_=(l)<(L30); 
  if (L311_) {
   L321_=INTMINUS(m,1); 
   ARRAYF1076245134(self, l, L321_);
  }
  L331_=INTPLUS(m,1); 
  L34 = L331_;
  L351_=(L34)<(u); 
  if (L351_) {
   L361_=INTPLUS(m,1); 
   L37 = L361_;
   ARRAYF1076245134(self, L37, u);
  }
 }
 else {
  ARRAYF1978661014(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYF1978661014(ARRAYFSTR self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 FSTR e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 ARRAYFSTR elt_lt_self;
 FSTR elt_lt_e1;
 FSTR elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTFSTR elt_lt_self1;
 FSTR elt_lt_e11;
 FSTR elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 FSTR L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 FSTR L161_;
 INT L171_;
 INT L18;
 INT L201_;
 INT L22;
 FSTR L231_;
 FSTR L24;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(FSTR)ARR((ARRAYFSTR)self,i); 
   e = L81_;
   {
    BOOL f_L91_ = TRUE;
    L101_=INTMINUS(i,1); 
    L31 = L101_;
    L121_=INTMINUS(l,1); 
    L41 = L121_;
    L91_=L31+1;L91_m=L41; 
    while (1) {
     if(L91_--<=L91_m)  goto after_loop1; 
     j = L91_;
     L141_=(j)<(l); 
     if (L141_) {
      SARR((ARRAYFSTR)self,l,(FSTR)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(FSTR)ARR((ARRAYFSTR)self,j); 
      elt_lt_e1 = L161_;
      elt_lt_e2 = e;
      elt_lt_self1 = ((ELT_LTFSTR) NULL);
      elt_lt_e11 = elt_lt_e1;
      elt_lt_e21 = elt_lt_e2;
      ret_val1 = FSTR_i673953987(elt_lt_e11, elt_lt_e21);
      ret_val = ret_val1;
      if (ret_val) {
       L171_=INTPLUS(j,1); 
       L18 = L171_;
       SARR((ARRAYFSTR)self,L18,(FSTR)e); 
       ;
       goto after_loop1;
      }
      else {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       L231_=(FSTR)ARR((ARRAYFSTR)self,j); 
       L24 = L231_;
       SARR((ARRAYFSTR)self,L22,(FSTR)L24); 
       ;
      }
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYI1050890878(ARRAYIDENT self, INT l, INT u) {
 INT r = INT_zero;
 IDENT t = IDENT_zero;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 IDENT s = IDENT_zero;
 ARRAYIDENT elt_lt_self;
 IDENT elt_lt_e1 = IDENT_zero;
 IDENT elt_lt_e2 = IDENT_zero;
 BOOL ret_val = BOOL_zero;
 ELT_LTIDENT elt_lt_self1;
 IDENT elt_lt_e11 = IDENT_zero;
 IDENT elt_lt_e21 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_lt_self = IDENT_zero;
 IDENT is_lt_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 IDENT L71_;
 IDENT L81_;
 IDENT L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 IDENT L161_;
 INT L171_;
 IDENT L181_;
 IDENT L191_;
 IDENT L20;
 IDENT L241_;
 IDENT L251_;
 IDENT L26;
 INT L291_;
 INT L30;
 BOOL L311_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 L3 = ARRAYI56037128;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=ARR((ARRAYIDENT)self,r); 
  t = L71_;
  L81_=ARR((ARRAYIDENT)self,l); 
  L9 = L81_;
  SARR((ARRAYIDENT)self,r,L9); 
  ;
  SARR((ARRAYIDENT)self,l,t); 
  ;
  m = l;
  {
   BOOL f_L131_ = TRUE;
   L141_=INTPLUS(l,1); 
   L11 = L141_;
   L21 = u;
   L131_=L11-1;L131_m=L21; 
   while (1) {
    if(L131_++>=L131_m)  goto after_loop; 
    i = L131_;
    elt_lt_self = self;
    L161_=ARR((ARRAYIDENT)self,i); 
    elt_lt_e1 = L161_;
    elt_lt_e2 = t;
    elt_lt_self1 = ((ELT_LTIDENT) NULL);
    elt_lt_e11 = elt_lt_e1;
    elt_lt_e21 = elt_lt_e2;
    is_lt_self = elt_lt_e11;
    is_lt_i = elt_lt_e21;
    ret_val2 = STR_is171745744(is_lt_self.str, is_lt_i.str);
    ret_val1 = ret_val2;
    ret_val = ret_val1;
    if (ret_val) {
     L171_=INTPLUS(m,1); 
     m = L171_;
     L181_=ARR((ARRAYIDENT)self,m); 
     s = L181_;
     L191_=ARR((ARRAYIDENT)self,i); 
     L20 = L191_;
     SARR((ARRAYIDENT)self,m,L20); 
     ;
     SARR((ARRAYIDENT)self,i,s); 
     ;
    }
   }
  }
  after_loop: ;
  L241_=ARR((ARRAYIDENT)self,l); 
  t = L241_;
  L251_=ARR((ARRAYIDENT)self,m); 
  L26 = L251_;
  SARR((ARRAYIDENT)self,l,L26); 
  ;
  SARR((ARRAYIDENT)self,m,t); 
  ;
  L291_=INTMINUS(m,1); 
  L30 = L291_;
  L311_=(l)<(L30); 
  if (L311_) {
   L321_=INTMINUS(m,1); 
   ARRAYI1050890878(self, l, L321_);
  }
  L331_=INTPLUS(m,1); 
  L34 = L331_;
  L351_=(L34)<(u); 
  if (L351_) {
   L361_=INTPLUS(m,1); 
   L37 = L361_;
   ARRAYI1050890878(self, L37, u);
  }
 }
 else {
  ARRAYI2092236385(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYI2092236385(ARRAYIDENT self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 IDENT e = IDENT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 ARRAYIDENT elt_lt_self;
 IDENT elt_lt_e1 = IDENT_zero;
 IDENT elt_lt_e2 = IDENT_zero;
 BOOL ret_val = BOOL_zero;
 ELT_LTIDENT elt_lt_self1;
 IDENT elt_lt_e11 = IDENT_zero;
 IDENT elt_lt_e21 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_lt_self = IDENT_zero;
 IDENT is_lt_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 IDENT L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 IDENT L161_;
 INT L171_;
 INT L18;
 INT L201_;
 INT L22;
 IDENT L231_;
 IDENT L24;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=ARR((ARRAYIDENT)self,i); 
   e = L81_;
   {
    BOOL f_L91_ = TRUE;
    L101_=INTMINUS(i,1); 
    L31 = L101_;
    L121_=INTMINUS(l,1); 
    L41 = L121_;
    L91_=L31+1;L91_m=L41; 
    while (1) {
     if(L91_--<=L91_m)  goto after_loop1; 
     j = L91_;
     L141_=(j)<(l); 
     if (L141_) {
      SARR((ARRAYIDENT)self,l,e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=ARR((ARRAYIDENT)self,j); 
      elt_lt_e1 = L161_;
      elt_lt_e2 = e;
      elt_lt_self1 = ((ELT_LTIDENT) NULL);
      elt_lt_e11 = elt_lt_e1;
      elt_lt_e21 = elt_lt_e2;
      is_lt_self = elt_lt_e11;
      is_lt_i = elt_lt_e21;
      ret_val2 = STR_is171745744(is_lt_self.str, is_lt_i.str);
      ret_val1 = ret_val2;
      ret_val = ret_val1;
      if (ret_val) {
       L171_=INTPLUS(j,1); 
       L18 = L171_;
       SARR((ARRAYIDENT)self,L18,e); 
       ;
       goto after_loop1;
      }
      else {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       L231_=ARR((ARRAYIDENT)self,j); 
       L24 = L231_;
       SARR((ARRAYIDENT)self,L22,L24); 
       ;
      }
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYS1450857938(ARRAYSTR self, INT l, INT u) {
 INT r = INT_zero;
 STR t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR s;
 ARRAYSTR elt_lt_self;
 STR elt_lt_e1;
 STR elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTSTR elt_lt_self1;
 STR elt_lt_e11;
 STR elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 STR L71_;
 STR L81_;
 STR L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 STR L161_;
 INT L171_;
 STR L181_;
 STR L191_;
 STR L20;
 STR L241_;
 STR L251_;
 STR L26;
 INT L291_;
 INT L30;
 BOOL L311_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 L3 = ARRAYS852413528;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(STR)ARR((ARRAYSTR)self,r); 
  t = L71_;
  L81_=(STR)ARR((ARRAYSTR)self,l); 
  L9 = L81_;
  SARR((ARRAYSTR)self,r,(STR)L9); 
  ;
  SARR((ARRAYSTR)self,l,(STR)t); 
  ;
  m = l;
  {
   BOOL f_L131_ = TRUE;
   L141_=INTPLUS(l,1); 
   L11 = L141_;
   L21 = u;
   L131_=L11-1;L131_m=L21; 
   while (1) {
    if(L131_++>=L131_m)  goto after_loop; 
    i = L131_;
    elt_lt_self = self;
    L161_=(STR)ARR((ARRAYSTR)self,i); 
    elt_lt_e1 = L161_;
    elt_lt_e2 = t;
    elt_lt_self1 = ((ELT_LTSTR) NULL);
    elt_lt_e11 = elt_lt_e1;
    elt_lt_e21 = elt_lt_e2;
    ret_val1 = STR_is171745744(elt_lt_e11, elt_lt_e21);
    ret_val = ret_val1;
    if (ret_val) {
     L171_=INTPLUS(m,1); 
     m = L171_;
     L181_=(STR)ARR((ARRAYSTR)self,m); 
     s = L181_;
     L191_=(STR)ARR((ARRAYSTR)self,i); 
     L20 = L191_;
     SARR((ARRAYSTR)self,m,(STR)L20); 
     ;
     SARR((ARRAYSTR)self,i,(STR)s); 
     ;
    }
   }
  }
  after_loop: ;
  L241_=(STR)ARR((ARRAYSTR)self,l); 
  t = L241_;
  L251_=(STR)ARR((ARRAYSTR)self,m); 
  L26 = L251_;
  SARR((ARRAYSTR)self,l,(STR)L26); 
  ;
  SARR((ARRAYSTR)self,m,(STR)t); 
  ;
  L291_=INTMINUS(m,1); 
  L30 = L291_;
  L311_=(l)<(L30); 
  if (L311_) {
   L321_=INTMINUS(m,1); 
   ARRAYS1450857938(self, l, L321_);
  }
  L331_=INTPLUS(m,1); 
  L34 = L331_;
  L351_=(L34)<(u); 
  if (L351_) {
   L361_=INTPLUS(m,1); 
   L37 = L361_;
   ARRAYS1450857938(self, L37, u);
  }
 }
 else {
  ARRAYS37144603(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYS37144603(ARRAYSTR self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 ARRAYSTR elt_lt_self;
 STR elt_lt_e1;
 STR elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTSTR elt_lt_self1;
 STR elt_lt_e11;
 STR elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 STR L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 STR L161_;
 INT L171_;
 INT L18;
 INT L201_;
 INT L22;
 STR L231_;
 STR L24;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(STR)ARR((ARRAYSTR)self,i); 
   e = L81_;
   {
    BOOL f_L91_ = TRUE;
    L101_=INTMINUS(i,1); 
    L31 = L101_;
    L121_=INTMINUS(l,1); 
    L41 = L121_;
    L91_=L31+1;L91_m=L41; 
    while (1) {
     if(L91_--<=L91_m)  goto after_loop1; 
     j = L91_;
     L141_=(j)<(l); 
     if (L141_) {
      SARR((ARRAYSTR)self,l,(STR)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(STR)ARR((ARRAYSTR)self,j); 
      elt_lt_e1 = L161_;
      elt_lt_e2 = e;
      elt_lt_self1 = ((ELT_LTSTR) NULL);
      elt_lt_e11 = elt_lt_e1;
      elt_lt_e21 = elt_lt_e2;
      ret_val1 = STR_is171745744(elt_lt_e11, elt_lt_e21);
      ret_val = ret_val1;
      if (ret_val) {
       L171_=INTPLUS(j,1); 
       L18 = L171_;
       SARR((ARRAYSTR)self,L18,(STR)e); 
       ;
       goto after_loop1;
      }
      else {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       L231_=(STR)ARR((ARRAYSTR)self,j); 
       L24 = L231_;
       SARR((ARRAYSTR)self,L22,(STR)L24); 
       ;
      }
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

AM_ITE1809135850 ARRAYA494342579(ARRAYA494342579_frame frame) {
 AM_ITE1809135850 dummy = ((AM_ITE1809135850) NULL);
 AM_ITE1809135850 aI_u_I;
 AM_ITE1809135850 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1369744492)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA1369744492)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_LOCAL_EXPR ARRAYA27295121(ARRAYA27295121_frame frame) {
 AM_LOCAL_EXPR dummy = ((AM_LOCAL_EXPR) NULL);
 AM_LOCAL_EXPR aI_u_I;
 AM_LOCAL_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1575632848)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA1575632848)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_ROUT_DEF ARRAYA1253289773(ARRAYA1253289773_frame frame) {
 AM_ROUT_DEF dummy = ((AM_ROUT_DEF) NULL);
 AM_ROUT_DEF aI_u_I;
 AM_ROUT_DEF L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYAM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYAM_ROUT_DEF)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AM_SHARED_EXPR ARRAYA1830087990(ARRAYA1830087990_frame frame) {
 AM_SHARED_EXPR dummy = ((AM_SHARED_EXPR) NULL);
 AM_SHARED_EXPR aI_u_I;
 AM_SHARED_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1135548710)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA1135548710)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ARG ARRAYA827580689(ARRAYA827580689_frame frame) {
 ARG dummy = ((ARG) NULL);
 ARG aI_u_I;
 ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYARG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ARRAYBOOL ARRAYA277860273(ARRAYA277860273_frame frame) {
 ARRAYBOOL dummy = ((ARRAYBOOL) NULL);
 ARRAYBOOL aI_u_I;
 ARRAYBOOL L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARRAYBOOL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYARRAYBOOL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame frame) {
 ARRAYSTR dummy = ((ARRAYSTR) NULL);
 ARRAYSTR aI_u_I;
 ARRAYSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARRAYSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYARRAYSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ARRAYdAM_EXPR ARRAYA289850317(ARRAYA289850317_frame frame) {
 ARRAYdAM_EXPR dummy = ((ARRAYdAM_EXPR) NULL);
 ARRAYdAM_EXPR aI_u_I;
 ARRAYdAM_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA477251019)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYA477251019)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AS_FEAT_MOD ARRAYA1522064985(ARRAYA1522064985_frame frame) {
 AS_FEAT_MOD dummy = ((AS_FEAT_MOD) NULL);
 AS_FEAT_MOD aI_u_I;
 AS_FEAT_MOD L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYAS_FEAT_MOD)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYAS_FEAT_MOD)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ATTRSORT ARRAYA1660316962(ARRAYA1660316962_frame frame) {
 ATTRSORT dummy = ((ATTRSORT) NULL);
 ATTRSORT aI_u_I;
 ATTRSORT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYATTRSORT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYATTRSORT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BOOL ARRAYB1390856792(ARRAYB1390856792_frame frame) {
 BOOL dummy = BOOL_zero;
 BOOL aI_u_I;
 BOOL L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYBOOL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYBOOL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BOUND_2140373124 ARRAYB1475407144(ARRAYB1475407144_frame frame) {
 BOUND_2140373124 dummy = ((BOUND_2140373124) NULL);
 BOUND_2140373124 aI_u_I;
 BOUND_2140373124 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYB1234765325)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYB1234765325)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BOUND_809390774 ARRAYB1420960227(ARRAYB1420960227_frame frame) {
 BOUND_809390774 dummy = ((BOUND_809390774) NULL);
 BOUND_809390774 aI_u_I;
 BOUND_809390774 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYB1925477728)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYB1925477728)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

BUILTIN_LAYOUT ARRAYB1615560602(ARRAYB1615560602_frame frame) {
 BUILTIN_LAYOUT dummy = ((BUILTIN_LAYOUT) NULL);
 BUILTIN_LAYOUT aI_u_I;
 BUILTIN_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYB1039347268)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYB1039347268)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CALL_ARG ARRAYC494195798(ARRAYC494195798_frame frame) {
 CALL_ARG dummy = ((CALL_ARG) NULL);
 CALL_ARG aI_u_I;
 CALL_ARG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYCALL_ARG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYCALL_ARG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CHAR ARRAYC1494984744(ARRAYC1494984744_frame frame) {
 CHAR dummy = CHAR_zero;
 CHAR aI_u_I;
 CHAR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYCHAR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYCHAR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CLASS_LAYOUT ARRAYC818595214(ARRAYC818595214_frame frame) {
 CLASS_LAYOUT dummy = ((CLASS_LAYOUT) NULL);
 CLASS_LAYOUT aI_u_I;
 CLASS_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYC1438655838)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYC1438655838)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CODE_FILE ARRAYC1531429675(ARRAYC1531429675_frame frame) {
 CODE_FILE dummy = ((CODE_FILE) NULL);
 CODE_FILE aI_u_I;
 CODE_FILE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYCODE_FILE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYCODE_FILE)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

CSE_DOUBLE_EXPRS ARRAYC1440407068(ARRAYC1440407068_frame frame) {
 CSE_DOUBLE_EXPRS dummy = ((CSE_DOUBLE_EXPRS) NULL);
 CSE_DOUBLE_EXPRS aI_u_I;
 CSE_DOUBLE_EXPRS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYC1788421899)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYC1788421899)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

EXPR_HOISTED ARRAYE1693383532(ARRAYE1693383532_frame frame) {
 EXPR_HOISTED dummy = ((EXPR_HOISTED) NULL);
 EXPR_HOISTED aI_u_I;
 EXPR_HOISTED L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYE2126408533)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYE2126408533)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

EXTERNAL_LAYOUT ARRAYE1687022731(ARRAYE1687022731_frame frame) {
 EXTERNAL_LAYOUT dummy = ((EXTERNAL_LAYOUT) NULL);
 EXTERNAL_LAYOUT aI_u_I;
 EXTERNAL_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYE406487263)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYE406487263)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

FLISTSTR ARRAYF1006928322(ARRAYF1006928322_frame frame) {
 FLISTSTR dummy = ((FLISTSTR) NULL);
 FLISTSTR aI_u_I;
 FLISTSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYFLISTSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYFLISTSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

FLISTdAM_CONST ARRAYF1657157762(ARRAYF1657157762_frame frame) {
 FLISTdAM_CONST dummy = ((FLISTdAM_CONST) NULL);
 FLISTdAM_CONST aI_u_I;
 FLISTdAM_CONST L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYF1030817442)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYF1030817442)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

FSTR ARRAYF254484098(ARRAYF254484098_frame frame) {
 FSTR dummy = ((FSTR) NULL);
 FSTR aI_u_I;
 FSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYFSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYFSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IDENT ARRAYI1725945879(ARRAYI1725945879_frame frame) {
 IDENT dummy = IDENT_zero;
 IDENT aI_u_I;
 IDENT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYIDENT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYIDENT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IFC ARRAYI2003604369(ARRAYI2003604369_frame frame) {
 IFC dummy = ((IFC) NULL);
 IFC aI_u_I;
 IFC L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYIFC)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYIFC)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IMMUTA421759893 ARRAYI1264630278(ARRAYI1264630278_frame frame) {
 IMMUTA421759893 dummy = ((IMMUTA421759893) NULL);
 IMMUTA421759893 aI_u_I;
 IMMUTA421759893 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYI1327367692)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYI1327367692)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IMPL_INCLUDE ARRAYI1719886982(ARRAYI1719886982_frame frame) {
 IMPL_INCLUDE dummy = ((IMPL_INCLUDE) NULL);
 IMPL_INCLUDE aI_u_I;
 IMPL_INCLUDE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYI1991954990)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYI1991954990)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

INT ARRAYA1077419174(ARRAYA1077419174_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARG)frame->self); 
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

INT ARRAYA702716235(ARRAYA702716235_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYA1575632848)frame->self); 
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

INT ARRAYA712604839(ARRAYA712604839_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARRAYSTR)frame->self); 
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

INT ARRAYI180735351(ARRAYI180735351_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYINT)frame->self); 
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

INT ARRAYI69049459(ARRAYI69049459_frame frame) {
 INT dummy = INT_zero;
 INT aI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

INT ARRAYI789953351(ARRAYI789953351_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYIDENT)frame->self); 
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

INT ARRAYS1828691793(ARRAYS1828691793_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSTR)frame->self); 
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

OPT_LOCAL_EXPR ARRAYO335724112(ARRAYO335724112_frame frame) {
 OPT_LOCAL_EXPR dummy = ((OPT_LOCAL_EXPR) NULL);
 OPT_LOCAL_EXPR aI_u_I;
 OPT_LOCAL_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYO570594127)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYO570594127)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SFILE_ID ARRAYS1372094358(ARRAYS1372094358_frame frame) {
 SFILE_ID dummy = SFILE_ID_zero;
 SFILE_ID aI_u_I;
 SFILE_ID L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSFILE_ID)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYSFILE_ID)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SIDE_EFFECT ARRAYS2023743485(ARRAYS2023743485_frame frame) {
 SIDE_EFFECT dummy = ((SIDE_EFFECT) NULL);
 SIDE_EFFECT aI_u_I;
 SIDE_EFFECT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSIDE_EFFECT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYSIDE_EFFECT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SIG ARRAYS1481566101(ARRAYS1481566101_frame frame) {
 SIG dummy = ((SIG) NULL);
 SIG aI_u_I;
 SIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

STR ARRAYS1578977701(ARRAYS1578977701_frame frame) {
 STR dummy = ((STR) NULL);
 STR aI_u_I;
 STR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TP_CLASS ARRAYT1851819274(ARRAYT1851819274_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 TP_CLASS aI_u_I;
 TP_CLASS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTP_CLASS)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTP_CLASS)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TP_LAYOUT ARRAYT1372201847(ARRAYT1372201847_frame frame) {
 TP_LAYOUT dummy = ((TP_LAYOUT) NULL);
 TP_LAYOUT aI_u_I;
 TP_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTP_LAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTP_LAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_1024994801 ARRAYT1758179858(ARRAYT1758179858_frame frame) {
 TUPAM_1024994801 dummy = TUPAM_1024994801_zero;
 TUPAM_1024994801 aI_u_I;
 TUPAM_1024994801 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT546647214)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT546647214)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_1519040877 ARRAYT684852235(ARRAYT684852235_frame frame) {
 TUPAM_1519040877 dummy = TUPAM_1519040877_zero;
 TUPAM_1519040877 aI_u_I;
 TUPAM_1519040877 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1753333425)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1753333425)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_304458649 ARRAYT310960754(ARRAYT310960754_frame frame) {
 TUPAM_304458649 dummy = TUPAM_304458649_zero;
 TUPAM_304458649 aI_u_I;
 TUPAM_304458649 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1275404834)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1275404834)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_504653531 ARRAYT342187846(ARRAYT342187846_frame frame) {
 TUPAM_504653531 dummy = TUPAM_504653531_zero;
 TUPAM_504653531 aI_u_I;
 TUPAM_504653531 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1744744076)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1744744076)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_653469574 ARRAYT1101967660(ARRAYT1101967660_frame frame) {
 TUPAM_653469574 dummy = TUPAM_653469574_zero;
 TUPAM_653469574 aI_u_I;
 TUPAM_653469574 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1968212795)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1968212795)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_667892060 ARRAYT2108331328(ARRAYT2108331328_frame frame) {
 TUPAM_667892060 dummy = TUPAM_667892060_zero;
 TUPAM_667892060 aI_u_I;
 TUPAM_667892060 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1146239647)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1146239647)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_927437481 ARRAYT1360634798(ARRAYT1360634798_frame frame) {
 TUPAM_927437481 dummy = TUPAM_927437481_zero;
 TUPAM_927437481 aI_u_I;
 TUPAM_927437481 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT2065045042)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT2065045042)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDE641654124 ARRAYT451883912(ARRAYT451883912_frame frame) {
 TUPIDE641654124 dummy = TUPIDE641654124_zero;
 TUPIDE641654124 aI_u_I;
 TUPIDE641654124 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT263953675)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT263953675)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDENTdTP ARRAYT849390229(ARRAYT849390229_frame frame) {
 TUPIDENTdTP dummy = TUPIDENTdTP_zero;
 TUPIDENTdTP aI_u_I;
 TUPIDENTdTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPIDENTdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPIDENTdTP)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void ARRAYA107911980(ARRAYA107911980_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYATTRSORT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYATTRSORT)frame->self,frame->i_I_u_I,(ATTRSORT)frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYA409651007(ARRAYA409651007_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYARG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYARG)frame->self,frame->i_I_u_I,(ARG)frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYB741586158(ARRAYB741586158_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYBOOL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYBOOL)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYC1058209184(ARRAYC1058209184_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYCALL_ARG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYCALL_ARG)frame->self,frame->i_I_u_I,(CALL_ARG)frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYF394786536(ARRAYF394786536_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYFSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYFSTR)frame->self,frame->i_I_u_I,(FSTR)frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYI1177186037(ARRAYI1177186037_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYIDENT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYIDENT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYI486979141(ARRAYI486979141_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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


#undef IS_ITER
#define IS_ITER 1

void ARRAYS1161048019(ARRAYS1161048019_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYSTR)frame->self,frame->i_I_u_I,(STR)frame->arg1); frame->i_I_u_I++;
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
