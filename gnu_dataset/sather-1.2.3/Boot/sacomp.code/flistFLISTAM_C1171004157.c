#include "sather.h"

/* Layouts */

typedef struct dELTAM_CALL_ARG_struct {
 OB_HEADER header;
 } *dELTAM_CALL_ARG;

typedef struct dELTAM373291377_struct {
 OB_HEADER header;
 } *dELTAM373291377;

typedef struct dELTAM_CURSOR_struct {
 OB_HEADER header;
 } *dELTAM_CURSOR;

typedef struct dELTAM937720280_struct {
 OB_HEADER header;
 } *dELTAM937720280;

typedef struct dELTAM938692099_struct {
 OB_HEADER header;
 } *dELTAM938692099;

typedef struct dELTAM1985205111_struct {
 OB_HEADER header;
 } *dELTAM1985205111;

typedef struct dELTAM_ROUT_DEF_struct {
 OB_HEADER header;
 } *dELTAM_ROUT_DEF;

typedef struct dELTAM1943610286_struct {
 OB_HEADER header;
 } *dELTAM1943610286;

typedef struct dELTARG_struct {
 OB_HEADER header;
 } *dELTARG;

typedef struct dELTAR67678756_struct {
 OB_HEADER header;
 } *dELTAR67678756;

typedef struct dELTARRAYBOOL_struct {
 OB_HEADER header;
 } *dELTARRAYBOOL;

typedef struct dELTARRAYSTR_struct {
 OB_HEADER header;
 } *dELTARRAYSTR;

typedef struct dELTAS_FEAT_MOD_struct {
 OB_HEADER header;
 } *dELTAS_FEAT_MOD;

typedef struct dELTATTRSORT_struct {
 OB_HEADER header;
 } *dELTATTRSORT;

typedef struct dELTBOOL_struct {
 OB_HEADER header;
 } *dELTBOOL;

typedef struct dELTBO2026178203_struct {
 OB_HEADER header;
 } *dELTBO2026178203;

typedef struct dELTBO61052977_struct {
 OB_HEADER header;
 } *dELTBO61052977;

typedef struct dELTBU1847408844_struct {
 OB_HEADER header;
 } *dELTBU1847408844;

typedef struct dELTCALL_ARG_struct {
 OB_HEADER header;
 } *dELTCALL_ARG;

typedef struct dELTCHAR_struct {
 OB_HEADER header;
 } *dELTCHAR;

typedef struct dELTCLASS_LAYOUT_struct {
 OB_HEADER header;
 } *dELTCLASS_LAYOUT;

typedef struct dELTCODE_FILE_struct {
 OB_HEADER header;
 } *dELTCODE_FILE;

typedef struct dELTCS1440874327_struct {
 OB_HEADER header;
 } *dELTCS1440874327;

typedef struct dELTEXPR_HOISTED_struct {
 OB_HEADER header;
 } *dELTEXPR_HOISTED;

typedef struct dELTEX2061780774_struct {
 OB_HEADER header;
 } *dELTEX2061780774;

typedef struct dELTFL1838879018_struct {
 OB_HEADER header;
 } *dELTFL1838879018;

typedef struct dELTFLISTSTR_struct {
 OB_HEADER header;
 } *dELTFLISTSTR;

typedef struct dELTFSTR_struct {
 OB_HEADER header;
 } *dELTFSTR;

typedef struct dELTIDENT_struct {
 OB_HEADER header;
 } *dELTIDENT;

typedef struct dELTIFC_struct {
 OB_HEADER header;
 } *dELTIFC;

typedef struct dELTIM293343924_struct {
 OB_HEADER header;
 } *dELTIM293343924;

typedef struct dELTIMPL_INCLUDE_struct {
 OB_HEADER header;
 } *dELTIMPL_INCLUDE;

typedef struct dELTINT_struct {
 OB_HEADER header;
 } *dELTINT;

typedef struct dELTOP237467449_struct {
 OB_HEADER header;
 } *dELTOP237467449;

typedef struct dELTSFILE_ID_struct {
 OB_HEADER header;
 } *dELTSFILE_ID;

typedef struct dELTSIDE_EFFECT_struct {
 OB_HEADER header;
 } *dELTSIDE_EFFECT;

typedef struct dELTSIG_struct {
 OB_HEADER header;
 } *dELTSIG;

typedef struct dELTSTR_struct {
 OB_HEADER header;
 } *dELTSTR;

typedef struct dELTTP_CLASS_struct {
 OB_HEADER header;
 } *dELTTP_CLASS;

typedef struct dELTTP_LAYOUT_struct {
 OB_HEADER header;
 } *dELTTP_LAYOUT;

typedef struct dELTTU1558822270_struct {
 OB_HEADER header;
 } *dELTTU1558822270;

typedef struct dELTTU847844253_struct {
 OB_HEADER header;
 } *dELTTU847844253;

typedef struct dELTTU789674962_struct {
 OB_HEADER header;
 } *dELTTU789674962;

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

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

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

typedef struct ELT_AL1348837573_struct {/* layout for ELT_ALG{AM_CALL_ARG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1348837573;

typedef struct ELT_AL92619562_struct {/* layout for ELT_ALG{AM_CURSOR_POS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL92619562;

typedef struct ELT_ALGAM_CURSOR_struct {/* layout for ELT_ALG{AM_CURSOR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGAM_CURSOR;

typedef struct ELT_AL1403631219_struct {/* layout for ELT_ALG{AM_FORMAL_ARG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1403631219;

typedef struct ELT_AL911461552_struct {/* layout for ELT_ALG{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL911461552;

typedef struct ELT_AL1519294172_struct {/* layout for ELT_ALG{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1519294172;

typedef struct ELT_AL1523214528_struct {/* layout for ELT_ALG{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1523214528;

typedef struct ELT_AL2088983704_struct {/* layout for ELT_ALG{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2088983704;

typedef struct ELT_ALGARG_struct {/* layout for ELT_ALG{ARG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGARG;

typedef struct ELT_AL533589695_struct {/* layout for ELT_ALG{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL533589695;

typedef struct ELT_ALGARRAYBOOL_struct {/* layout for ELT_ALG{ARRAY{BOOL}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGARRAYBOOL;

typedef struct ELT_ALGARRAYSTR_struct {/* layout for ELT_ALG{ARRAY{STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGARRAYSTR;

typedef struct ELT_AL454519361_struct {/* layout for ELT_ALG{AS_FEAT_MOD} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL454519361;

typedef struct ELT_ALGATTRSORT_struct {/* layout for ELT_ALG{ATTRSORT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGATTRSORT;

typedef struct ELT_ALGBOOL_struct {/* layout for ELT_ALG{BOOL} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGBOOL;

typedef struct ELT_AL1115246319_struct {/* layout for ELT_ALG{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1115246319;

typedef struct ELT_AL88283524_struct {/* layout for ELT_ALG{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL88283524;

typedef struct ELT_AL2109782150_struct {/* layout for ELT_ALG{BUILTIN_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2109782150;

typedef struct ELT_ALGCALL_ARG_struct {/* layout for ELT_ALG{CALL_ARG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGCALL_ARG;

typedef struct ELT_ALGCHAR_struct {/* layout for ELT_ALG{CHAR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGCHAR;

typedef struct ELT_AL1044938984_struct {/* layout for ELT_ALG{CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1044938984;

typedef struct ELT_ALGCODE_FILE_struct {/* layout for ELT_ALG{CODE_FILE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGCODE_FILE;

typedef struct ELT_AL1668358421_struct {/* layout for ELT_ALG{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1668358421;

typedef struct ELT_AL315036059_struct {/* layout for ELT_ALG{EXPR_HOISTED} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL315036059;

typedef struct ELT_AL1543060947_struct {/* layout for ELT_ALG{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1543060947;

typedef struct ELT_AL2101252324_struct {/* layout for ELT_ALG{FLIST{$AM_CONST}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2101252324;

typedef struct ELT_ALGFLISTSTR_struct {/* layout for ELT_ALG{FLIST{STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGFLISTSTR;

typedef struct ELT_ALGFSTR_struct {/* layout for ELT_ALG{FSTR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGFSTR;

typedef struct ELT_ALGIDENT_struct {/* layout for ELT_ALG{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGIDENT;

typedef struct ELT_ALGIFC_struct {/* layout for ELT_ALG{IFC} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGIFC;

typedef struct ELT_AL1446886698_struct {/* layout for ELT_ALG{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1446886698;

typedef struct ELT_AL180582516_struct {/* layout for ELT_ALG{IMPL_INCLUDE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL180582516;

typedef struct ELT_ALGINT_struct {/* layout for ELT_ALG{INT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGINT;

typedef struct ELT_AL499840755_struct {/* layout for ELT_ALG{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL499840755;

typedef struct ELT_ALGSFILE_ID_struct {/* layout for ELT_ALG{SFILE_ID} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGSFILE_ID;

typedef struct ELT_AL569350364_struct {/* layout for ELT_ALG{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL569350364;

typedef struct ELT_ALGSIG_struct {/* layout for ELT_ALG{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGSIG;

typedef struct ELT_ALGSTR_struct {/* layout for ELT_ALG{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGSTR;

typedef struct ELT_ALGTP_CLASS_struct {/* layout for ELT_ALG{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTP_CLASS;

typedef struct ELT_ALGTP_LAYOUT_struct {/* layout for ELT_ALG{TP_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTP_LAYOUT;

typedef struct ELT_AL420149736_struct {/* layout for ELT_ALG{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL420149736;

typedef struct ELT_AL1230514763_struct {/* layout for ELT_ALG{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1230514763;

typedef struct ELT_AL2090369736_struct {/* layout for ELT_ALG{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2090369736;

typedef struct ELT_EQ334385345_struct {/* layout for ELT_EQ{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ334385345;

typedef struct ELT_EQ1704040611_struct {/* layout for ELT_EQ{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1704040611;

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

typedef struct AM_CURSOR_struct {/* layout for AM_CURSOR */
 OB_HEADER header;
 struct dAM_struct *cur;
 struct dAM_struct *top1;
 struct A_STAC1244966098_struct *stack1;
 INT indent;
 INT loops;
 struct PROG_struct *prog;
 BOOL all_in_eval_orde;
 BOOL all_in_pre_order;
 BOOL assign_in_order;
 BOOL calls_in_eval_or;
 BOOL ignore_assert;
 BOOL ignore_next;
 BOOL ignore_post;
 BOOL ignore_pre;
 BOOL mark;
 BOOL started;
 BOOL with_side_effect;
 } *AM_CURSOR;

typedef struct AM_CURSOR_POS_struct {/* layout for AM_CURSOR_POS */
 OB_HEADER header;
 struct dAM_struct *stmt;
 INT branch;
 BOOL mark;
 } *AM_CURSOR_POS;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYS1161048019_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1161048019_frame;

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

typedef struct FLISTAM_CALL_ARG_struct {/* layout for FLIST{AM_CALL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *FLISTAM_CALL_ARG;

typedef struct FLISTA964597258_frame_struct {
 INT state;
 FLISTAM_CALL_ARG self;/* Formal argument: self */
 AM_CALL_ARG ret_val1;
 INT i;
 INT sz;
 FLISTAM_CALL_ARG aget_self;
 INT aget_ind;
 AM_CALL_ARG ret_val;
 } *FLISTA964597258_frame;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct FLISTA259226942_frame_struct {
 INT state;
 FLISTA886630705 self;/* Formal argument: self */
 AM_CURSOR_POS arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA259226942_frame;

typedef struct FLISTA1660160_frame_struct {
 INT state;
 FLISTA886630705 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val1;
 INT i;
 INT sz;
 FLISTA886630705 aget_self;
 INT aget_ind;
 AM_CURSOR_POS ret_val;
 } *FLISTA1660160_frame;

typedef struct FLISTAM_CURSOR_struct {/* layout for FLIST{AM_CURSOR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_struct *arr_part[1];
 } *FLISTAM_CURSOR;

typedef struct FLISTA1933262168_frame_struct {
 INT state;
 FLISTAM_CURSOR self;/* Formal argument: self */
 AM_CURSOR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA1933262168_frame;

typedef struct FLISTA84883712_frame_struct {
 INT state;
 FLISTAM_CURSOR self;/* Formal argument: self */
 AM_CURSOR ret_val1;
 INT i;
 INT sz;
 FLISTAM_CURSOR aget_self;
 INT aget_ind;
 AM_CURSOR ret_val;
 } *FLISTA84883712_frame;

typedef struct FLISTA1878650141_frame_struct {
 INT state;
 FLISTAM_CURSOR self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTA1878650141_frame;

typedef struct FLISTA2097324934_struct {/* layout for FLIST{AM_FORMAL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *FLISTA2097324934;

typedef struct FLISTA831183340_frame_struct {
 INT state;
 FLISTA2097324934 self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val1;
 INT i;
 INT sz;
 FLISTA2097324934 aget_self;
 INT aget_ind;
 AM_FORMAL_ARG ret_val;
 } *FLISTA831183340_frame;

typedef struct FLISTA1062334999_struct {/* layout for FLIST{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ITE1809135850_struct *arr_part[1];
 } *FLISTA1062334999;

typedef struct FLISTA443212798_frame_struct {
 INT state;
 FLISTA1062334999 self;/* Formal argument: self */
 AM_ITE1809135850 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA443212798_frame;

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

typedef struct FLISTA1237039204_frame_struct {
 INT state;
 FLISTA1062334999 self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTA1237039204_frame;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA340495062_frame_struct {
 INT state;
 FLISTA725283029 self;/* Formal argument: self */
 AM_LOCAL_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA340495062_frame;

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

typedef struct FLISTA56457972_frame_struct {
 INT state;
 FLISTA725283029 self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTA56457972_frame;

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

typedef struct FLISTA496550642_frame_struct {
 INT state;
 FLISTAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA496550642_frame;

typedef struct FLISTA2121868826_frame_struct {
 INT state;
 FLISTAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val1;
 INT i;
 INT sz;
 FLISTAM_ROUT_DEF aget_self;
 INT aget_ind;
 AM_ROUT_DEF ret_val;
 } *FLISTA2121868826_frame;

typedef struct FLISTA112326087_struct {/* layout for FLIST{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FLISTA112326087;

typedef struct FLISTA1482204797_frame_struct {
 INT state;
 FLISTA112326087 self;/* Formal argument: self */
 AM_SHARED_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA1482204797_frame;

typedef struct FLISTA120298683_frame_struct {
 INT state;
 FLISTA112326087 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA112326087 aget_self;
 INT aget_ind;
 AM_SHARED_EXPR ret_val;
 } *FLISTA120298683_frame;

typedef struct FLISTARG_struct {/* layout for FLIST{ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *FLISTARG;

typedef struct FLISTA420234122_frame_struct {
 INT state;
 FLISTARG self;/* Formal argument: self */
 ARG ret_val1;
 INT i;
 INT sz;
 FLISTARG aget_self;
 INT aget_ind;
 ARG ret_val;
 } *FLISTA420234122_frame;

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FLISTA1349709692_frame_struct {
 INT state;
 FLISTA1327600838 self;/* Formal argument: self */
 ARRAYdAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA1349709692_frame;

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

typedef struct FLISTARRAYBOOL_struct {/* layout for FLIST{ARRAY{BOOL}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYBOOL_struct *arr_part[1];
 } *FLISTARRAYBOOL;

typedef struct FLISTA538157944_frame_struct {
 INT state;
 FLISTARRAYBOOL self;/* Formal argument: self */
 ARRAYBOOL ret_val1;
 INT i;
 INT sz;
 FLISTARRAYBOOL aget_self;
 INT aget_ind;
 ARRAYBOOL ret_val;
 } *FLISTA538157944_frame;

typedef struct FLISTARRAYSTR_struct {/* layout for FLIST{ARRAY{STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *FLISTARRAYSTR;

typedef struct FLISTA970595569_frame_struct {
 INT state;
 FLISTARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val1;
 INT i;
 INT sz;
 FLISTARRAYSTR aget_self;
 INT aget_ind;
 ARRAYSTR ret_val;
 } *FLISTA970595569_frame;

typedef struct FLISTAS_FEAT_MOD_struct {/* layout for FLIST{AS_FEAT_MOD} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FLISTAS_FEAT_MOD;

typedef struct FLISTA1853093614_frame_struct {
 INT state;
 FLISTAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD ret_val1;
 INT i;
 INT sz;
 FLISTAS_FEAT_MOD aget_self;
 INT aget_ind;
 AS_FEAT_MOD ret_val;
 } *FLISTA1853093614_frame;

typedef struct FLISTATTRSORT_struct {/* layout for FLIST{ATTRSORT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ATTRSORT_struct *arr_part[1];
 } *FLISTATTRSORT;

typedef struct FLISTA901142755_frame_struct {
 INT state;
 FLISTATTRSORT self;/* Formal argument: self */
 ATTRSORT ret_val1;
 INT i;
 INT sz;
 FLISTATTRSORT aget_self;
 INT aget_ind;
 ATTRSORT ret_val;
 } *FLISTA901142755_frame;

typedef struct FLISTBOOL_struct {/* layout for FLIST{BOOL} */
 OB_HEADER header;
 INT loc;
 INT asize;
 BOOL arr_part[1];
 } *FLISTBOOL;

typedef struct FLISTB2086724459_frame_struct {
 INT state;
 FLISTBOOL self;/* Formal argument: self */
 BOOL arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTB2086724459_frame;

typedef struct FLISTB1880887881_frame_struct {
 INT state;
 FLISTBOOL self;/* Formal argument: self */
 BOOL ret_val1;
 INT i;
 INT sz;
 FLISTBOOL aget_self;
 INT aget_ind;
 BOOL ret_val;
 } *FLISTB1880887881_frame;

typedef struct FLISTB475591118_struct {/* layout for FLIST{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FLISTB475591118;

typedef struct FLISTB455167321_frame_struct {
 INT state;
 FLISTB475591118 self;/* Formal argument: self */
 BOUND_2140373124 ret_val1;
 INT i;
 INT sz;
 FLISTB475591118 aget_self;
 INT aget_ind;
 BOUND_2140373124 ret_val;
 } *FLISTB455167321_frame;

typedef struct FLISTB2062080075_struct {/* layout for FLIST{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FLISTB2062080075;

typedef struct FLISTB905996620_frame_struct {
 INT state;
 FLISTB2062080075 self;/* Formal argument: self */
 BOUND_809390774 ret_val1;
 INT i;
 INT sz;
 FLISTB2062080075 aget_self;
 INT aget_ind;
 BOUND_809390774 ret_val;
 } *FLISTB905996620_frame;

typedef struct FLISTB16124645_struct {/* layout for FLIST{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FLISTB16124645;

typedef struct FLISTB334826071_frame_struct {
 INT state;
 FLISTB16124645 self;/* Formal argument: self */
 BUILTIN_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTB16124645 aget_self;
 INT aget_ind;
 BUILTIN_LAYOUT ret_val;
 } *FLISTB334826071_frame;

typedef struct FLISTCALL_ARG_struct {/* layout for FLIST{CALL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CALL_ARG_struct *arr_part[1];
 } *FLISTCALL_ARG;

typedef struct FLISTC264978409_frame_struct {
 INT state;
 FLISTCALL_ARG self;/* Formal argument: self */
 CALL_ARG ret_val1;
 INT i;
 INT sz;
 FLISTCALL_ARG aget_self;
 INT aget_ind;
 CALL_ARG ret_val;
 } *FLISTC264978409_frame;

typedef struct FLISTCHAR_struct {/* layout for FLIST{CHAR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FLISTCHAR;

typedef struct FLISTC1776759929_frame_struct {
 INT state;
 FLISTCHAR self;/* Formal argument: self */
 CHAR ret_val1;
 INT i;
 INT sz;
 FLISTCHAR aget_self;
 INT aget_ind;
 CHAR ret_val;
 } *FLISTC1776759929_frame;

typedef struct FLISTC1031309271_struct {/* layout for FLIST{CLASS_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FLISTC1031309271;

typedef struct FLISTC519862611_frame_struct {
 INT state;
 FLISTC1031309271 self;/* Formal argument: self */
 CLASS_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTC1031309271 aget_self;
 INT aget_ind;
 CLASS_LAYOUT ret_val;
 } *FLISTC519862611_frame;

typedef struct FLISTCODE_FILE_struct {/* layout for FLIST{CODE_FILE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *FLISTCODE_FILE;

typedef struct FLISTC542105842_frame_struct {
 INT state;
 FLISTCODE_FILE self;/* Formal argument: self */
 CODE_FILE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTC542105842_frame;

typedef struct FLISTC1947519404_frame_struct {
 INT state;
 FLISTCODE_FILE self;/* Formal argument: self */
 CODE_FILE ret_val1;
 INT i;
 INT sz;
 FLISTCODE_FILE aget_self;
 INT aget_ind;
 CODE_FILE ret_val;
 } *FLISTC1947519404_frame;

typedef struct FLISTC1772242452_struct {/* layout for FLIST{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FLISTC1772242452;

typedef struct FLISTC1263372315_frame_struct {
 INT state;
 FLISTC1772242452 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS ret_val1;
 INT i;
 INT sz;
 FLISTC1772242452 aget_self;
 INT aget_ind;
 CSE_DOUBLE_EXPRS ret_val;
 } *FLISTC1263372315_frame;

typedef struct FLISTE1761212196_struct {/* layout for FLIST{EXPR_HOISTED} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct EXPR_HOISTED_struct *arr_part[1];
 } *FLISTE1761212196;

typedef struct FLISTE1675330331_frame_struct {
 INT state;
 FLISTE1761212196 self;/* Formal argument: self */
 EXPR_HOISTED arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTE1675330331_frame;

typedef struct FLISTE354925707_frame_struct {
 INT state;
 FLISTE1761212196 self;/* Formal argument: self */
 EXPR_HOISTED ret_val1;
 INT i;
 INT sz;
 FLISTE1761212196 aget_self;
 INT aget_ind;
 EXPR_HOISTED ret_val;
 } *FLISTE354925707_frame;

typedef struct FLISTE18407748_frame_struct {
 INT state;
 FLISTE1761212196 self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTE18407748_frame;

typedef struct FLISTE1627119380_struct {/* layout for FLIST{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FLISTE1627119380;

typedef struct FLISTE1972070322_frame_struct {
 INT state;
 FLISTE1627119380 self;/* Formal argument: self */
 EXTERNAL_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTE1627119380 aget_self;
 INT aget_ind;
 EXTERNAL_LAYOUT ret_val;
 } *FLISTE1972070322_frame;

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

typedef struct FLISTF2058849421_frame_struct {
 INT state;
 FLISTF7594819 self;/* Formal argument: self */
 FLISTdAM_CONST arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTF2058849421_frame;

typedef struct FLISTF687422861_frame_struct {
 INT state;
 FLISTF7594819 self;/* Formal argument: self */
 FLISTdAM_CONST ret_val1;
 INT i;
 INT sz;
 FLISTF7594819 aget_self;
 INT aget_ind;
 FLISTdAM_CONST ret_val;
 } *FLISTF687422861_frame;

typedef struct FLISTFLISTSTR_struct {/* layout for FLIST{FLIST{STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTSTR_struct *arr_part[1];
 } *FLISTFLISTSTR;

typedef struct FLISTFSTR_struct {/* layout for FLIST{FSTR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FSTR_struct *arr_part[1];
 } *FLISTFSTR;

typedef struct FLISTIDENT_struct {/* layout for FLIST{IDENT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *FLISTIDENT;

typedef struct FLISTI747041326_frame_struct {
 INT state;
 FLISTIDENT self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTI747041326_frame;

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

typedef struct FLISTIFC_struct {/* layout for FLIST{IFC} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IFC_struct *arr_part[1];
 } *FLISTIFC;

typedef struct FLISTI1211273540_frame_struct {
 INT state;
 FLISTIFC self;/* Formal argument: self */
 IFC arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTI1211273540_frame;

typedef struct FLISTI1596257802_frame_struct {
 INT state;
 FLISTIFC self;/* Formal argument: self */
 IFC ret_val1;
 INT i;
 INT sz;
 FLISTIFC aget_self;
 INT aget_ind;
 IFC ret_val;
 } *FLISTI1596257802_frame;

typedef struct FLISTI2086541899_struct {/* layout for FLIST{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FLISTI2086541899;

typedef struct FLISTI244390455_frame_struct {
 INT state;
 FLISTI2086541899 self;/* Formal argument: self */
 IMMUTA421759893 ret_val1;
 INT i;
 INT sz;
 FLISTI2086541899 aget_self;
 INT aget_ind;
 IMMUTA421759893 ret_val;
 } *FLISTI244390455_frame;

typedef struct FLISTI1895665739_struct {/* layout for FLIST{IMPL_INCLUDE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IMPL_INCLUDE_struct *arr_part[1];
 } *FLISTI1895665739;

typedef struct FLISTI304092359_frame_struct {
 INT state;
 FLISTI1895665739 self;/* Formal argument: self */
 IMPL_INCLUDE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTI304092359_frame;

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

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FLISTI485498674_frame_struct {
 INT state;
 FLISTINT self;/* Formal argument: self */
 INT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTI485498674_frame;

typedef struct FLISTI476396026_frame_struct {
 INT state;
 FLISTINT self;/* Formal argument: self */
 INT ret_val1;
 INT i;
 INT sz;
 FLISTINT aget_self;
 INT aget_ind;
 INT ret_val;
 } *FLISTI476396026_frame;

typedef struct FLISTO1593816750_struct {/* layout for FLIST{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct OPT_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTO1593816750;

typedef struct FLISTO2008856511_frame_struct {
 INT state;
 FLISTO1593816750 self;/* Formal argument: self */
 OPT_LOCAL_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTO1593816750 aget_self;
 INT aget_ind;
 OPT_LOCAL_EXPR ret_val;
 } *FLISTO2008856511_frame;

typedef struct FLISTSFILE_ID_struct {/* layout for FLIST{SFILE_ID} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FLISTSFILE_ID;

typedef struct FLISTS1763978443_frame_struct {
 INT state;
 FLISTSFILE_ID self;/* Formal argument: self */
 SFILE_ID arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTS1763978443_frame;

typedef struct FLISTS2131268565_frame_struct {
 INT state;
 FLISTSFILE_ID self;/* Formal argument: self */
 SFILE_ID ret_val1;
 INT i;
 INT sz;
 FLISTSFILE_ID aget_self;
 INT aget_ind;
 SFILE_ID ret_val;
 } *FLISTS2131268565_frame;

typedef struct FLISTSIDE_EFFECT_struct {/* layout for FLIST{SIDE_EFFECT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FLISTSIDE_EFFECT;

typedef struct FLISTS1351415114_frame_struct {
 INT state;
 FLISTSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val1;
 INT i;
 INT sz;
 FLISTSIDE_EFFECT aget_self;
 INT aget_ind;
 SIDE_EFFECT ret_val;
 } *FLISTS1351415114_frame;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

typedef struct FLISTS118774442_frame_struct {
 INT state;
 FLISTSIG self;/* Formal argument: self */
 SIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTS118774442_frame;

typedef struct FLISTS1074219534_frame_struct {
 INT state;
 FLISTSIG self;/* Formal argument: self */
 SIG ret_val1;
 INT i;
 INT sz;
 FLISTSIG aget_self;
 INT aget_ind;
 SIG ret_val;
 } *FLISTS1074219534_frame;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FLISTS1731970718_frame_struct {
 INT state;
 FLISTSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTS1731970718_frame;

typedef struct FLISTS1171631134_frame_struct {
 INT state;
 FLISTSTR self;/* Formal argument: self */
 STR ret_val1;
 INT i;
 INT sz;
 FLISTSTR aget_self;
 INT aget_ind;
 STR ret_val;
 } *FLISTS1171631134_frame;

typedef struct FLISTTP_CLASS_struct {/* layout for FLIST{TP_CLASS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FLISTTP_CLASS;

typedef struct FLISTT1683973815_frame_struct {
 INT state;
 FLISTTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val1;
 INT i;
 INT sz;
 FLISTTP_CLASS aget_self;
 INT aget_ind;
 TP_CLASS ret_val;
 } *FLISTT1683973815_frame;

typedef struct FLISTTP_LAYOUT_struct {/* layout for FLIST{TP_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TP_LAYOUT_struct *arr_part[1];
 } *FLISTTP_LAYOUT;

typedef struct FLISTT2106747232_frame_struct {
 INT state;
 FLISTTP_LAYOUT self;/* Formal argument: self */
 TP_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTTP_LAYOUT aget_self;
 INT aget_ind;
 TP_LAYOUT ret_val;
 } *FLISTT2106747232_frame;

typedef struct FLISTT1311334615_struct {/* layout for FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FLISTT1311334615;

typedef struct FLISTT726119853_frame_struct {
 INT state;
 FLISTT1311334615 self;/* Formal argument: self */
 TUPAM_304458649 ret_val1;
 INT i;
 INT sz;
 FLISTT1311334615 aget_self;
 INT aget_ind;
 TUPAM_304458649 ret_val;
 } *FLISTT726119853_frame;

typedef struct FLISTT1661203254_struct {/* layout for FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FLISTT1661203254;

typedef struct FLISTT193552257_frame_struct {
 INT state;
 FLISTT1661203254 self;/* Formal argument: self */
 TUPAM_667892060 ret_val1;
 INT i;
 INT sz;
 FLISTT1661203254 aget_self;
 INT aget_ind;
 TUPAM_667892060 ret_val;
 } *FLISTT193552257_frame;

typedef struct FLISTT1145236345_struct {/* layout for FLIST{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_927437481_struct arr_part[1];
 } *FLISTT1145236345;

typedef struct FLISTT854840857_frame_struct {
 INT state;
 FLISTT1145236345 self;/* Formal argument: self */
 TUPAM_927437481 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTT854840857_frame;

typedef struct FLISTT709128269_frame_struct {
 INT state;
 FLISTT1145236345 self;/* Formal argument: self */
 TUPAM_927437481 ret_val1;
 INT i;
 INT sz;
 FLISTT1145236345 aget_self;
 INT aget_ind;
 TUPAM_927437481 ret_val;
 } *FLISTT709128269_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct FLISTF206425427_frame_struct {
 INT state;
 FLISTFLISTSTR self;/* Formal argument: self */
 FLISTSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTF206425427_frame;

typedef struct FLISTF1766102529_frame_struct {
 INT state;
 FLISTFLISTSTR self;/* Formal argument: self */
 FLISTSTR ret_val1;
 INT i;
 INT sz;
 FLISTFLISTSTR aget_self;
 INT aget_ind;
 FLISTSTR ret_val;
 } *FLISTF1766102529_frame;

typedef struct FLISTF1980391255_frame_struct {
 INT state;
 FLISTFSTR self;/* Formal argument: self */
 FSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTF1980391255_frame;

typedef struct FLISTF1277706721_frame_struct {
 INT state;
 FLISTFSTR self;/* Formal argument: self */
 FSTR ret_val1;
 INT i;
 INT sz;
 FLISTFSTR aget_self;
 INT aget_ind;
 FSTR ret_val;
 } *FLISTF1277706721_frame;

#include "tags.h"

/* Globals */


/* Function declarations */

AM_CALL_ARG FLISTA964597258(FLISTA964597258_frame);
AM_CURSOR FLISTA84883712(FLISTA84883712_frame);
AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame);
AM_CURSOR_POS FLISTA860779230(FLISTA886630705);
AM_CURSOR_POS FLISTA971253650(FLISTA886630705);
AM_FORMAL_ARG FLISTA831183340(FLISTA831183340_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_LOCAL_EXPR FLISTA912130454(FLISTA725283029);
AM_ROUT_DEF FLISTA2121868826(FLISTA2121868826_frame);
AM_SHARED_EXPR FLISTA120298683(FLISTA120298683_frame);
ARG FLISTA420234122(FLISTA420234122_frame);
ARRAYBOOL FLISTA538157944(FLISTA538157944_frame);
ARRAYSTR FLISTA970595569(FLISTA970595569_frame);
ARRAYSTR FLISTS533235694(FLISTSTR);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
AS_FEAT_MOD FLISTA1853093614(FLISTA1853093614_frame);
ATTRSORT FLISTA901142755(FLISTA901142755_frame);
BOOL FLISTB1880887881(FLISTB1880887881_frame);
BOOL FLISTB466509411(FLISTBOOL);
BOUND_2140373124 FLISTB455167321(FLISTB455167321_frame);
BOUND_809390774 FLISTB905996620(FLISTB905996620_frame);
BUILTIN_LAYOUT FLISTB334826071(FLISTB334826071_frame);
CALL_ARG FLISTC264978409(FLISTC264978409_frame);
CHAR FLISTC1776759929(FLISTC1776759929_frame);
CLASS_LAYOUT FLISTC519862611(FLISTC519862611_frame);
CODE_FILE FLISTC1947519404(FLISTC1947519404_frame);
CSE_DOUBLE_EXPRS FLISTC1263372315(FLISTC1263372315_frame);
EXPR_HOISTED FLISTE354925707(FLISTE354925707_frame);
EXTERNAL_LAYOUT FLISTE1972070322(FLISTE1972070322_frame);
FLISTA1062334999 FLISTA1106467315(FLISTA1062334999, INT);
FLISTA1062334999 FLISTA1752107307(FLISTA1062334999, INT);
FLISTA1062334999 FLISTA572736043(FLISTA1062334999, FLISTA1062334999);
FLISTA1062334999 FLISTA943034044(FLISTA1062334999, AM_ITE1809135850);
FLISTA112326087 FLISTA327355535(FLISTA112326087, AM_SHARED_EXPR);
FLISTA1327600838 FLISTA475813755(FLISTA1327600838, ARRAYdAM_EXPR);
FLISTA725283029 FLISTA1446031755(FLISTA725283029, INT);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA189055531(FLISTA725283029, FLISTA725283029);
FLISTA725283029 FLISTA2134789053(FLISTA725283029, INT);
FLISTA725283029 FLISTA528857227(FLISTA725283029);
FLISTA725283029 FLISTA811588375(FLISTA725283029, FLISTA725283029);
FLISTA886630705 FLISTA1475379054(FLISTA886630705, AM_CURSOR_POS);
FLISTAM_CURSOR FLISTA122884859(FLISTAM_CURSOR, AM_CURSOR);
FLISTAM_ROUT_DEF FLISTA1773009698(FLISTAM_ROUT_DEF, AM_ROUT_DEF);
FLISTBOOL FLISTB1991913204(FLISTBOOL, BOOL);
FLISTCODE_FILE FLISTC1532120756(FLISTCODE_FILE, CODE_FILE);
FLISTE1761212196 FLISTE1031401542(FLISTE1761212196, EXPR_HOISTED);
FLISTF7594819 FLISTF1203220157(FLISTF7594819, FLISTdAM_CONST);
FLISTFLISTSTR FLISTF630314943(FLISTFLISTSTR, FLISTSTR);
FLISTFSTR FLISTF1280148353(FLISTFSTR, FSTR);
FLISTFSTR FLISTF524229941(FLISTFSTR, FLISTFSTR);
FLISTFSTR FLISTF575242064(FLISTFSTR, INT);
FLISTI1895665739 FLISTI1378884285(FLISTI1895665739, IMPL_INCLUDE);
FLISTIDENT FLISTI370551816(FLISTIDENT, IDENT);
FLISTIFC FLISTI985525641(FLISTIFC, IFC);
FLISTINT FLISTI1369678776(FLISTINT, INT);
FLISTINT FLISTI55819225(FLISTINT, INT);
FLISTSFILE_ID FLISTS1155895163(FLISTSFILE_ID, SFILE_ID);
FLISTSIG FLISTS69519306(FLISTSIG, SIG);
FLISTSTR FLISTF1766102529(FLISTF1766102529_frame);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTSTR FLISTS1900495285(FLISTSTR, INT);
FLISTT1145236345 FLISTT596895593(FLISTT1145236345, TUPAM_927437481);
FLISTdAM_CONST FLISTF471519751(FLISTF7594819);
FLISTdAM_CONST FLISTF687422861(FLISTF687422861_frame);
FSTR FLISTF1277706721(FLISTF1277706721_frame);
IDENT FLISTI1709766432(FLISTI1709766432_frame);
IFC FLISTI1596257802(FLISTI1596257802_frame);
IFC FLISTIFC_poprIFC(FLISTIFC);
IMMUTA421759893 FLISTI244390455(FLISTI244390455_frame);
IMPL_INCLUDE FLISTI381429157(FLISTI381429157_frame);
IMPL_INCLUDE FLISTI961811375(FLISTI1895665739);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTA1237039204(FLISTA1237039204_frame);
INT FLISTA1370145561(FLISTA725283029, AM_LOCAL_EXPR);
INT FLISTA1531002815(FLISTA1327600838);
INT FLISTA1878650141(FLISTA1878650141_frame);
INT FLISTA1962912942(FLISTA1062334999);
INT FLISTA23038851(FLISTA1062334999, AM_ITE1809135850);
INT FLISTA56457972(FLISTA56457972_frame);
INT FLISTA587993904(FLISTA886630705);
INT FLISTB746188588(FLISTBOOL);
INT FLISTC1064619074(FLISTCODE_FILE);
INT FLISTE18407748(FLISTE18407748_frame);
INT FLISTF1097166371(FLISTFLISTSTR);
INT FLISTF1348049713(FLISTFSTR);
INT FLISTF873512073(FLISTF7594819);
INT FLISTI1321626366(FLISTINT);
INT FLISTI1328284422(FLISTIDENT);
INT FLISTI377878565(FLISTI1895665739);
INT FLISTI476396026(FLISTI476396026_frame);
INT FLISTI751030673(FLISTIFC);
INT FLISTINT_poprINT(FLISTINT);
INT FLISTS228922656(FLISTSIG);
INT FLISTS326330076(FLISTSTR);
INT FLISTS434496825(FLISTSFILE_ID);
INT INT_up418511718(INT_up418511718_frame);
OPT_LOCAL_EXPR FLISTO2008856511(FLISTO2008856511_frame);
SFILE_ID FLISTS2131268565(FLISTS2131268565_frame);
SIDE_EFFECT FLISTS1351415114(FLISTS1351415114_frame);
SIG FLISTS1074219534(FLISTS1074219534_frame);
SIG FLISTSIG_poprSIG(FLISTSIG);
SIG FLISTSIG_toprSIG(FLISTSIG);
STR ELT_AL101060479(ELT_AL2101252324, dELTFL1838879018);
STR ELT_AL1010662471(ELT_AL1044938984, dELTCLASS_LAYOUT);
STR ELT_AL1024390623(ELT_ALGARRAYSTR, dELTARRAYSTR);
STR ELT_AL1033027484(ELT_ALGTP_LAYOUT, dELTTP_LAYOUT);
STR ELT_AL1033451287(ELT_AL180582516, dELTIMPL_INCLUDE);
STR ELT_AL1065029381(ELT_ALGCHAR, dELTCHAR);
STR ELT_AL1119786764(ELT_ALGAM_CURSOR, dELTAM_CURSOR);
STR ELT_AL127733498(ELT_AL1519294172, dELTAM1985205111);
STR ELT_AL130847997(ELT_AL1115246319, dELTBO2026178203);
STR ELT_AL1399860630(ELT_AL88283524, dELTBO61052977);
STR ELT_AL1417078085(ELT_AL499840755, dELTOP237467449);
STR ELT_AL1425453107(ELT_ALGSFILE_ID, dELTSFILE_ID);
STR ELT_AL1455618083(ELT_ALGATTRSORT, dELTATTRSORT);
STR ELT_AL150680983(ELT_AL2090369736, dELTTU789674962);
STR ELT_AL1523154184(ELT_AL533589695, dELTAR67678756);
STR ELT_AL1534167666(ELT_ALGIDENT, dELTIDENT);
STR ELT_AL1544151106(ELT_AL1523214528, dELTAM_ROUT_DEF);
STR ELT_AL1563649328(ELT_AL454519361, dELTAS_FEAT_MOD);
STR ELT_AL1598639248(ELT_ALGIFC, dELTIFC);
STR ELT_AL1688888374(ELT_AL569350364, dELTSIDE_EFFECT);
STR ELT_AL1717624110(ELT_ALGSTR, dELTSTR);
STR ELT_AL1734287228(ELT_AL1348837573, dELTAM_CALL_ARG);
STR ELT_AL1852369832(ELT_ALGARRAYBOOL, dELTARRAYBOOL);
STR ELT_AL1886121199(ELT_AL1668358421, dELTCS1440874327);
STR ELT_AL1906929438(ELT_AL911461552, dELTAM938692099);
STR ELT_AL1973394827(ELT_ALGTP_CLASS, dELTTP_CLASS);
STR ELT_AL2059371083(ELT_ALGCALL_ARG, dELTCALL_ARG);
STR ELT_AL2071496711(ELT_AL2109782150, dELTBU1847408844);
STR ELT_AL2090488585(ELT_ALGFSTR, dELTFSTR);
STR ELT_AL288668417(ELT_AL1446886698, dELTIM293343924);
STR ELT_AL360284389(ELT_ALGBOOL, dELTBOOL);
STR ELT_AL378639519(ELT_AL1230514763, dELTTU847844253);
STR ELT_AL4458855(ELT_AL420149736, dELTTU1558822270);
STR ELT_AL479013642(ELT_ALGINT, dELTINT);
STR ELT_AL481240086(ELT_ALGSIG, dELTSIG);
STR ELT_AL519275506(ELT_ALGCODE_FILE, dELTCODE_FILE);
STR ELT_AL541085071(ELT_AL2088983704, dELTAM1943610286);
STR ELT_AL550997356(ELT_AL1543060947, dELTEX2061780774);
STR ELT_AL741225494(ELT_AL92619562, dELTAM373291377);
STR ELT_AL755347248(ELT_ALGARG, dELTARG);
STR ELT_AL769058240(ELT_AL1403631219, dELTAM937720280);
STR ELT_AL808485957(ELT_AL315036059, dELTEXPR_HOISTED);
STR ELT_AL854472091(ELT_ALGFLISTSTR, dELTFLISTSTR);
STR FLISTA1208864070(FLISTA112326087);
STR FLISTA1228646807(FLISTA1327600838);
STR FLISTA128466217(FLISTAS_FEAT_MOD);
STR FLISTA1345532203(FLISTARRAYSTR);
STR FLISTA1366495389(FLISTA2097324934);
STR FLISTA1598488884(FLISTA886630705);
STR FLISTA1623803758(FLISTATTRSORT);
STR FLISTA1774499089(FLISTAM_CALL_ARG);
STR FLISTA1884067430(FLISTA1062334999);
STR FLISTA2047982482(FLISTA725283029);
STR FLISTA2143887329(FLISTAM_CURSOR);
STR FLISTA574012977(FLISTARRAYBOOL);
STR FLISTA674592310(FLISTAM_ROUT_DEF);
STR FLISTARG_strrSTR(FLISTARG);
STR FLISTB1590162857(FLISTBOOL);
STR FLISTB2097638834(FLISTB2062080075);
STR FLISTB2138238497(FLISTB475591118);
STR FLISTB275387800(FLISTB16124645);
STR FLISTC1168586346(FLISTC1031309271);
STR FLISTC1817360302(FLISTCODE_FILE);
STR FLISTC1921340409(FLISTC1772242452);
STR FLISTC31611564(FLISTCALL_ARG);
STR FLISTC780849889(FLISTCHAR);
STR FLISTE1365889449(FLISTE1761212196);
STR FLISTE283073321(FLISTE1627119380);
STR FLISTF1366971496(FLISTFLISTSTR);
STR FLISTF786174088(FLISTFSTR);
STR FLISTF93053878(FLISTF7594819);
STR FLISTI1877263384(FLISTI2086541899);
STR FLISTI2007266978(FLISTI1895665739);
STR FLISTI700982870(FLISTIDENT);
STR FLISTIFC_strrSTR(FLISTIFC);
STR FLISTINT_strrSTR(FLISTINT);
STR FLISTO1373905061(FLISTO1593816750);
STR FLISTS1154465862(FLISTSFILE_ID);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FLISTS1349623250(FLISTSIDE_EFFECT);
STR FLISTSIG_strrSTR(FLISTSIG);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_strrSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR FLISTT1518857242(FLISTT1145236345);
STR FLISTT1750053916(FLISTTP_CLASS);
STR FLISTT1896886665(FLISTTP_LAYOUT);
STR FLISTT2098083178(FLISTT1311334615);
STR FLISTT709853181(FLISTT1661203254);
TP_CLASS FLISTT1683973815(FLISTT1683973815_frame);
TP_LAYOUT FLISTT2106747232(FLISTT2106747232_frame);
TUPAM_304458649 FLISTT726119853(FLISTT726119853_frame);
TUPAM_667892060 FLISTT193552257(FLISTT193552257_frame);
TUPAM_927437481 FLISTT709128269(FLISTT709128269_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void FLISTA1349709692(FLISTA1349709692_frame);
void FLISTA1482204797(FLISTA1482204797_frame);
void FLISTA1933262168(FLISTA1933262168_frame);
void FLISTA259226942(FLISTA259226942_frame);
void FLISTA340495062(FLISTA340495062_frame);
void FLISTA443212798(FLISTA443212798_frame);
void FLISTA496550642(FLISTA496550642_frame);
void FLISTA739122510(FLISTA1062334999, INT);
void FLISTB2086724459(FLISTB2086724459_frame);
void FLISTC542105842(FLISTC542105842_frame);
void FLISTE1675330331(FLISTE1675330331_frame);
void FLISTF1980391255(FLISTF1980391255_frame);
void FLISTF2058849421(FLISTF2058849421_frame);
void FLISTF206425427(FLISTF206425427_frame);
void FLISTI1211273540(FLISTI1211273540_frame);
void FLISTI304092359(FLISTI304092359_frame);
void FLISTI485498674(FLISTI485498674_frame);
void FLISTI747041326(FLISTI747041326_frame);
void FLISTS118774442(FLISTS118774442_frame);
void FLISTS1731970718(FLISTS1731970718_frame);
void FLISTS1763978443(FLISTS1763978443_frame);
void FLISTT854840857(FLISTT854840857_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_CURSOR_POS FLISTA860779230(FLISTA886630705 self) {
 AM_CURSOR_POS ret_val;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val1;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 AM_CURSOR_POS L51_;
 L1 = FLISTA587993904(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((AM_CURSOR_POS) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
 ret_val1 = L51_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CURSOR_POS FLISTA971253650(FLISTA886630705 self) {
 AM_CURSOR_POS ret_val;
 AM_CURSOR_POS r;
 FLISTA886630705 aget_self;
 INT aget_ind = INT_zero;
 AM_CURSOR_POS ret_val1;
 FLISTA886630705 aset_self;
 INT aset_ind = INT_zero;
 AM_CURSOR_POS aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 AM_CURSOR_POS L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTA587993904(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((AM_CURSOR_POS) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(AM_CURSOR_POS)ARR((FLISTA886630705)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((AM_CURSOR_POS) NULL);
 SARR((FLISTA886630705)aset_self,aset_ind,(AM_CURSOR_POS)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR FLISTA912130454(FLISTA725283029 self) {
 AM_LOCAL_EXPR ret_val;
 AM_LOCAL_EXPR r;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 AM_LOCAL_EXPR L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTA1151473122(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((AM_LOCAL_EXPR) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((AM_LOCAL_EXPR) NULL);
 SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR FLISTS533235694(FLISTSTR self) {
 ARRAYSTR ret_val;
 ARRAYSTR r;
 FLISTSTR L11;
 ARRAYSTR L21;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 INT L31 = INT_zero;
 ARRAYSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_br;
 STR L9;
 STR L10;
 INT L121_;
 if ((self==((FLISTSTR) NULL))) {
  ret_val = ((ARRAYSTR) NULL);
  return ret_val;
 }
 create_self = ((ARRAYSTR) NULL);
 create_n = ATTR(self,loc);
 L5 = create_n;
 L7=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L5)*sizeof(STR);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYSTR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 r = ret_val1;
 {
  struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
  BOOL f_L81_ = TRUE;
  L11 = self;
  /* loop index variable */
  L31 = 0;
  L21 = r;
  noname1->self = L11;
  noname1->state = 0;
  L81_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  while (1) {
   if(L31>=L81_br)  goto after_loop; 
   L9 = FLISTS1171631134(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L10 = L9;
   SARR((ARRAYSTR)L21,L31,(STR)L10); 
   ;
   L121_=INTPLUS(L31,1); 
   L31 = L121_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLISTB466509411(FLISTBOOL self) {
 BOOL ret_val = BOOL_zero;
 BOOL r = BOOL_zero;
 FLISTBOOL aget_self;
 INT aget_ind = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 FLISTBOOL aset_self;
 INT aset_ind = INT_zero;
 BOOL aset_val = BOOL_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 BOOL L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTB746188588(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = BOOL_zero;
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=ARR((FLISTBOOL)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = BOOL_zero;
 SARR((FLISTBOOL)aset_self,aset_ind,aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1062334999 FLISTA1106467315(FLISTA1062334999 self, INT n) {
 FLISTA1062334999 ret_val;
 BOOL L11_;
 FLISTA1062334999 L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTA1062334999) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTA1062334999_struct)-sizeof(AM_ITE1809135850))+(L3)*sizeof(AM_ITE1809135850);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA1062334999_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTA1062334999) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1062334999 FLISTA1752107307(FLISTA1062334999 self, INT new_size) {
 FLISTA1062334999 ret_val;
 FLISTA1062334999 r = ((FLISTA1062334999) NULL);
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTA1062334999 aget_self;
 INT aget_ind = INT_zero;
 AM_ITE1809135850 ret_val1;
 FLISTA1062334999 aset_self;
 INT aset_ind = INT_zero;
 AM_ITE1809135850 aset_val;
 FLISTA1062334999 L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 FLISTA1062334999 L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 AM_ITE1809135850 L211_;
 INT L231_;
 if ((self==((FLISTA1062334999) NULL))) {
  L31_=(5>new_size)?5:new_size; 
  L2 = L31_;
  L5=(sizeof(struct FLISTA1062334999_struct)-sizeof(AM_ITE1809135850))+(L2)*sizeof(AM_ITE1809135850);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA1062334999_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L1 = ((FLISTA1062334999) L4);
  L1->asize = L2;
  r = L1;
 }
 else {
  L61_=ASIZE((FLISTA1062334999)self); 
  L7 = L61_;
  L81_=(L7)<(new_size); 
  L9 = L81_;
  L101_=!(L9); 
  if (L101_) {
   r = self;
  }
  else {
   L131_=ASIZE((FLISTA1062334999)self); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L16 = L151_;
   L171_=(L16>new_size)?L16:new_size; 
   L12 = L171_;
   L19=(sizeof(struct FLISTA1062334999_struct)-sizeof(AM_ITE1809135850))+(L12)*sizeof(AM_ITE1809135850);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1062334999_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L11 = ((FLISTA1062334999) L18);
   L11->asize = L12;
   r = L11;
   SATTR(r,loc,ATTR(self,loc));
   i = 0;
   sz = FLISTA1962912942(self);
   while (1) {
    L201_=(i)==(sz); 
    if (L201_) {
     goto after_loop;
    }
    aset_self = r;
    aset_ind = i;
    aget_self = self;
    aget_ind = i;
    L211_=(AM_ITE1809135850)ARR((FLISTA1062334999)aget_self,aget_ind); 
    ret_val1 = L211_;
    aset_val = ret_val1;
    SARR((FLISTA1062334999)aset_self,aset_ind,(AM_ITE1809135850)aset_val); 
    ;
    L231_=INTPLUS(i,1); 
    i = L231_;
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 SATTR(r,loc,new_size);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1062334999 FLISTA572736043(FLISTA1062334999 self, FLISTA1062334999 l) {
 FLISTA1062334999 ret_val;
 FLISTA1062334999 res;
 INT oldsize = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT resi = INT_zero;
 FLISTA1062334999 aget_self;
 INT aget_ind = INT_zero;
 AM_ITE1809135850 ret_val1;
 FLISTA1062334999 aset_self;
 INT aset_ind = INT_zero;
 AM_ITE1809135850 aset_val;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 BOOL L61_;
 AM_ITE1809135850 L71_;
 INT L91_;
 INT L101_;
 res = self;
 L1 = (l==((FLISTA1062334999) NULL));
 L21_=!(L1); 
 if (L21_) {
  oldsize = FLISTA1962912942(self);
  L3 = FLISTA1962912942(self);
  L4 = FLISTA1962912942(l);
  L51_=INTPLUS(L3,L4); 
  res = FLISTA1752107307(res, L51_);
  i = 0;
  sz = FLISTA1962912942(l);
  resi = oldsize;
  while (1) {
   L61_=(i)==(sz); 
   if (L61_) {
    goto after_loop;
   }
   aset_self = res;
   aset_ind = resi;
   aget_self = l;
   aget_ind = i;
   L71_=(AM_ITE1809135850)ARR((FLISTA1062334999)aget_self,aget_ind); 
   ret_val1 = L71_;
   aset_val = ret_val1;
   SARR((FLISTA1062334999)aset_self,aset_ind,(AM_ITE1809135850)aset_val); 
   ;
   L91_=INTPLUS(i,1); 
   i = L91_;
   L101_=INTPLUS(resi,1); 
   resi = L101_;
  }
  after_loop: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1062334999 FLISTA943034044(FLISTA1062334999 self, AM_ITE1809135850 e) {
 FLISTA1062334999 ret_val;
 FLISTA1062334999 r = ((FLISTA1062334999) NULL);
 FLISTA1062334999 L11;
 FLISTA1062334999 L21;
 INT L31 = INT_zero;
 FLISTA1062334999 aset_self;
 INT aset_ind = INT_zero;
 AM_ITE1809135850 aset_val;
 FLISTA1062334999 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA1062334999 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_ITE1809135850 L22;
 AM_ITE1809135850 L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA1062334999) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA1062334999_struct)-sizeof(AM_ITE1809135850))+(L5)*sizeof(AM_ITE1809135850);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA1062334999_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA1062334999) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA1062334999)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA1062334999)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA1062334999_struct)-sizeof(AM_ITE1809135850))+(L14)*sizeof(AM_ITE1809135850);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1062334999_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA1062334999) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA20621028_frame_struct other1_0;
FLISTA20621028_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA1062334999)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA20621028(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA1062334999)L21,L31,(AM_ITE1809135850)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA1062334999)aset_self,aset_ind,(AM_ITE1809135850)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA112326087 FLISTA327355535(FLISTA112326087 self, AM_SHARED_EXPR e) {
 FLISTA112326087 ret_val;
 FLISTA112326087 r = ((FLISTA112326087) NULL);
 FLISTA112326087 L11;
 FLISTA112326087 L21;
 INT L31 = INT_zero;
 FLISTA112326087 aset_self;
 INT aset_ind = INT_zero;
 AM_SHARED_EXPR aset_val;
 FLISTA112326087 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA112326087 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_SHARED_EXPR L22;
 AM_SHARED_EXPR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA112326087) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA112326087_struct)-sizeof(AM_SHARED_EXPR))+(L5)*sizeof(AM_SHARED_EXPR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA112326087_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA112326087) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA112326087)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA112326087)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA112326087_struct)-sizeof(AM_SHARED_EXPR))+(L14)*sizeof(AM_SHARED_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA112326087_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA112326087) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA120298683_frame_struct other1_0;
FLISTA120298683_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA112326087)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA120298683(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA112326087)L21,L31,(AM_SHARED_EXPR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA112326087)aset_self,aset_ind,(AM_SHARED_EXPR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1327600838 FLISTA475813755(FLISTA1327600838 self, ARRAYdAM_EXPR e) {
 FLISTA1327600838 ret_val;
 FLISTA1327600838 r = ((FLISTA1327600838) NULL);
 FLISTA1327600838 L11;
 FLISTA1327600838 L21;
 INT L31 = INT_zero;
 FLISTA1327600838 aset_self;
 INT aset_ind = INT_zero;
 ARRAYdAM_EXPR aset_val;
 FLISTA1327600838 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA1327600838 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 ARRAYdAM_EXPR L22;
 ARRAYdAM_EXPR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA1327600838) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA1327600838_struct)-sizeof(ARRAYdAM_EXPR))+(L5)*sizeof(ARRAYdAM_EXPR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA1327600838_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA1327600838) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA1327600838)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA1327600838)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA1327600838_struct)-sizeof(ARRAYdAM_EXPR))+(L14)*sizeof(ARRAYdAM_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1327600838_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA1327600838) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA1857087356_frame_struct other1_0;
FLISTA1857087356_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA1327600838)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA1857087356(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA1327600838)L21,L31,(ARRAYdAM_EXPR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA1327600838)aset_self,aset_ind,(ARRAYdAM_EXPR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA1446031755(FLISTA725283029 self, INT n) {
 FLISTA725283029 ret_val;
 BOOL L11_;
 FLISTA725283029 L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTA725283029) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L3)*sizeof(AM_LOCAL_EXPR);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTA725283029) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA1817671564(FLISTA725283029 self, AM_LOCAL_EXPR e) {
 FLISTA725283029 ret_val;
 FLISTA725283029 r = ((FLISTA725283029) NULL);
 FLISTA725283029 L11;
 FLISTA725283029 L21;
 INT L31 = INT_zero;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 FLISTA725283029 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA725283029 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_LOCAL_EXPR L22;
 AM_LOCAL_EXPR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA725283029) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L5)*sizeof(AM_LOCAL_EXPR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA725283029) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA725283029)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA725283029)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L14)*sizeof(AM_LOCAL_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA725283029) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA2119642552_frame_struct other1_0;
FLISTA2119642552_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA725283029)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA2119642552(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA725283029)L21,L31,(AM_LOCAL_EXPR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA189055531(FLISTA725283029 self, FLISTA725283029 l) {
 FLISTA725283029 ret_val;
 FLISTA725283029 r;
 INT old_size = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT li = INT_zero;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 BOOL L61_;
 AM_LOCAL_EXPR L71_;
 INT L91_;
 INT L101_;
 r = FLISTA528857227(self);
 old_size = FLISTA1151473122(self);
 L1 = FLISTA1151473122(self);
 L2 = FLISTA1151473122(l);
 L31_=INTPLUS(L1,L2); 
 r = FLISTA2134789053(r, L31_);
 i = old_size;
 L4 = FLISTA1151473122(l);
 L51_=INTPLUS(old_size,L4); 
 sz = L51_;
 li = 0;
 while (1) {
  L61_=(i)==(sz); 
  if (L61_) {
   goto after_loop;
  }
  aset_self = r;
  aset_ind = i;
  aget_self = l;
  aget_ind = li;
  L71_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
  ret_val1 = L71_;
  aset_val = ret_val1;
  SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
  ;
  L91_=INTPLUS(li,1); 
  li = L91_;
  L101_=INTPLUS(i,1); 
  i = L101_;
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA2134789053(FLISTA725283029 self, INT new_size) {
 FLISTA725283029 ret_val;
 FLISTA725283029 r = ((FLISTA725283029) NULL);
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 FLISTA725283029 L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 FLISTA725283029 L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 AM_LOCAL_EXPR L211_;
 INT L231_;
 if ((self==((FLISTA725283029) NULL))) {
  L31_=(5>new_size)?5:new_size; 
  L2 = L31_;
  L5=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L2)*sizeof(AM_LOCAL_EXPR);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L1 = ((FLISTA725283029) L4);
  L1->asize = L2;
  r = L1;
 }
 else {
  L61_=ASIZE((FLISTA725283029)self); 
  L7 = L61_;
  L81_=(L7)<(new_size); 
  L9 = L81_;
  L101_=!(L9); 
  if (L101_) {
   r = self;
  }
  else {
   L131_=ASIZE((FLISTA725283029)self); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L16 = L151_;
   L171_=(L16>new_size)?L16:new_size; 
   L12 = L171_;
   L19=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L12)*sizeof(AM_LOCAL_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L11 = ((FLISTA725283029) L18);
   L11->asize = L12;
   r = L11;
   SATTR(r,loc,ATTR(self,loc));
   i = 0;
   sz = FLISTA1151473122(self);
   while (1) {
    L201_=(i)==(sz); 
    if (L201_) {
     goto after_loop;
    }
    aset_self = r;
    aset_ind = i;
    aget_self = self;
    aget_ind = i;
    L211_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
    ret_val1 = L211_;
    aset_val = ret_val1;
    SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
    ;
    L231_=INTPLUS(i,1); 
    i = L231_;
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 SATTR(r,loc,new_size);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA528857227(FLISTA725283029 self) {
 FLISTA725283029 ret_val;
 FLISTA725283029 r;
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 FLISTA725283029 L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 BOOL L61_;
 AM_LOCAL_EXPR L71_;
 INT L91_;
 if ((self==((FLISTA725283029) NULL))) {
  ret_val = ((FLISTA725283029) NULL);
  return ret_val;
 }
 L31_=ASIZE((FLISTA725283029)self); 
 L2 = L31_;
 L5=(sizeof(struct FLISTA725283029_struct)-sizeof(AM_LOCAL_EXPR))+(L2)*sizeof(AM_LOCAL_EXPR);
 L4=ZALLOC_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=FLISTA725283029_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L1 = ((FLISTA725283029) L4);
 L1->asize = L2;
 r = L1;
 i = 0;
 sz = ATTR(self,loc);
 SATTR(r,loc,ATTR(self,loc));
 while (1) {
  L61_=(i)==(sz); 
  if (L61_) {
   goto after_loop;
  }
  aset_self = r;
  aset_ind = i;
  aget_self = self;
  aget_ind = i;
  L71_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
  ret_val1 = L71_;
  aset_val = ret_val1;
  SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
  ;
  L91_=INTPLUS(i,1); 
  i = L91_;
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 FLISTA811588375(FLISTA725283029 self, FLISTA725283029 l) {
 FLISTA725283029 ret_val;
 FLISTA725283029 res;
 INT oldsize = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT resi = INT_zero;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 aset_self;
 INT aset_ind = INT_zero;
 AM_LOCAL_EXPR aset_val;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 BOOL L61_;
 AM_LOCAL_EXPR L71_;
 INT L91_;
 INT L101_;
 res = self;
 L1 = (l==((FLISTA725283029) NULL));
 L21_=!(L1); 
 if (L21_) {
  oldsize = FLISTA1151473122(self);
  L3 = FLISTA1151473122(self);
  L4 = FLISTA1151473122(l);
  L51_=INTPLUS(L3,L4); 
  res = FLISTA2134789053(res, L51_);
  i = 0;
  sz = FLISTA1151473122(l);
  resi = oldsize;
  while (1) {
   L61_=(i)==(sz); 
   if (L61_) {
    goto after_loop;
   }
   aset_self = res;
   aset_ind = resi;
   aget_self = l;
   aget_ind = i;
   L71_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
   ret_val1 = L71_;
   aset_val = ret_val1;
   SARR((FLISTA725283029)aset_self,aset_ind,(AM_LOCAL_EXPR)aset_val); 
   ;
   L91_=INTPLUS(i,1); 
   i = L91_;
   L101_=INTPLUS(resi,1); 
   resi = L101_;
  }
  after_loop: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA886630705 FLISTA1475379054(FLISTA886630705 self, AM_CURSOR_POS e) {
 FLISTA886630705 ret_val;
 FLISTA886630705 r = ((FLISTA886630705) NULL);
 FLISTA886630705 L11;
 FLISTA886630705 L21;
 INT L31 = INT_zero;
 FLISTA886630705 aset_self;
 INT aset_ind = INT_zero;
 AM_CURSOR_POS aset_val;
 FLISTA886630705 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA886630705 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_CURSOR_POS L22;
 AM_CURSOR_POS L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA886630705) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA886630705_struct)-sizeof(AM_CURSOR_POS))+(L5)*sizeof(AM_CURSOR_POS);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA886630705_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA886630705) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA886630705)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA886630705)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA886630705_struct)-sizeof(AM_CURSOR_POS))+(L14)*sizeof(AM_CURSOR_POS);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA886630705_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA886630705) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA1660160_frame_struct other1_0;
FLISTA1660160_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA886630705)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA1660160(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA886630705)L21,L31,(AM_CURSOR_POS)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA886630705)aset_self,aset_ind,(AM_CURSOR_POS)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTAM_CURSOR FLISTA122884859(FLISTAM_CURSOR self, AM_CURSOR e) {
 FLISTAM_CURSOR ret_val;
 FLISTAM_CURSOR r = ((FLISTAM_CURSOR) NULL);
 FLISTAM_CURSOR L11;
 FLISTAM_CURSOR L21;
 INT L31 = INT_zero;
 FLISTAM_CURSOR aset_self;
 INT aset_ind = INT_zero;
 AM_CURSOR aset_val;
 FLISTAM_CURSOR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTAM_CURSOR L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_CURSOR L22;
 AM_CURSOR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTAM_CURSOR) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTAM_CURSOR_struct)-sizeof(AM_CURSOR))+(L5)*sizeof(AM_CURSOR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTAM_CURSOR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTAM_CURSOR) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTAM_CURSOR)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTAM_CURSOR)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTAM_CURSOR_struct)-sizeof(AM_CURSOR))+(L14)*sizeof(AM_CURSOR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTAM_CURSOR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTAM_CURSOR) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA84883712_frame_struct other1_0;
FLISTA84883712_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTAM_CURSOR)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA84883712(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTAM_CURSOR)L21,L31,(AM_CURSOR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTAM_CURSOR)aset_self,aset_ind,(AM_CURSOR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTAM_ROUT_DEF FLISTA1773009698(FLISTAM_ROUT_DEF self, AM_ROUT_DEF e) {
 FLISTAM_ROUT_DEF ret_val;
 FLISTAM_ROUT_DEF r = ((FLISTAM_ROUT_DEF) NULL);
 FLISTAM_ROUT_DEF L11;
 FLISTAM_ROUT_DEF L21;
 INT L31 = INT_zero;
 FLISTAM_ROUT_DEF aset_self;
 INT aset_ind = INT_zero;
 AM_ROUT_DEF aset_val;
 FLISTAM_ROUT_DEF L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTAM_ROUT_DEF L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_ROUT_DEF L22;
 AM_ROUT_DEF L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTAM_ROUT_DEF) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTAM_ROUT_DEF_struct)-sizeof(AM_ROUT_DEF))+(L5)*sizeof(AM_ROUT_DEF);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTAM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTAM_ROUT_DEF) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTAM_ROUT_DEF)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTAM_ROUT_DEF)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTAM_ROUT_DEF_struct)-sizeof(AM_ROUT_DEF))+(L14)*sizeof(AM_ROUT_DEF);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTAM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTAM_ROUT_DEF) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA2121868826_frame_struct other1_0;
FLISTA2121868826_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTAM_ROUT_DEF)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA2121868826(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTAM_ROUT_DEF)L21,L31,(AM_ROUT_DEF)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTAM_ROUT_DEF)aset_self,aset_ind,(AM_ROUT_DEF)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTBOOL FLISTB1991913204(FLISTBOOL self, BOOL e) {
 FLISTBOOL ret_val;
 FLISTBOOL r = ((FLISTBOOL) NULL);
 FLISTBOOL L11;
 FLISTBOOL L21;
 INT L31 = INT_zero;
 FLISTBOOL aset_self;
 INT aset_ind = INT_zero;
 BOOL aset_val = BOOL_zero;
 FLISTBOOL L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTBOOL L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 BOOL L22;
 BOOL L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTBOOL) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTBOOL_struct)+1-sizeof(BOOL))+(L5)*sizeof(BOOL);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=FLISTBOOL_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTBOOL) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTBOOL)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTBOOL)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTBOOL_struct)+1-sizeof(BOOL))+(L14)*sizeof(BOOL);
   L18=ZALLOC_LEAF_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   memset(L18,0,L19);
   ((OB)L18)->header.tag=FLISTBOOL_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTBOOL) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTB1880887881_frame_struct other1_0;
FLISTB1880887881_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTBOOL)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTB1880887881(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTBOOL)L21,L31,L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTBOOL)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTCODE_FILE FLISTC1532120756(FLISTCODE_FILE self, CODE_FILE e) {
 FLISTCODE_FILE ret_val;
 FLISTCODE_FILE r = ((FLISTCODE_FILE) NULL);
 FLISTCODE_FILE L11;
 FLISTCODE_FILE L21;
 INT L31 = INT_zero;
 FLISTCODE_FILE aset_self;
 INT aset_ind = INT_zero;
 CODE_FILE aset_val;
 FLISTCODE_FILE L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTCODE_FILE L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 CODE_FILE L22;
 CODE_FILE L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTCODE_FILE) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTCODE_FILE_struct)-sizeof(CODE_FILE))+(L5)*sizeof(CODE_FILE);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTCODE_FILE_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTCODE_FILE) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTCODE_FILE)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTCODE_FILE)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTCODE_FILE_struct)-sizeof(CODE_FILE))+(L14)*sizeof(CODE_FILE);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTCODE_FILE_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTCODE_FILE) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTC1947519404_frame_struct other1_0;
FLISTC1947519404_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTCODE_FILE)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTC1947519404(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTCODE_FILE)L21,L31,(CODE_FILE)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTCODE_FILE)aset_self,aset_ind,(CODE_FILE)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTE1761212196 FLISTE1031401542(FLISTE1761212196 self, EXPR_HOISTED e) {
 FLISTE1761212196 ret_val;
 FLISTE1761212196 r = ((FLISTE1761212196) NULL);
 FLISTE1761212196 L11;
 FLISTE1761212196 L21;
 INT L31 = INT_zero;
 FLISTE1761212196 aset_self;
 INT aset_ind = INT_zero;
 EXPR_HOISTED aset_val;
 FLISTE1761212196 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTE1761212196 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 EXPR_HOISTED L22;
 EXPR_HOISTED L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTE1761212196) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTE1761212196_struct)-sizeof(EXPR_HOISTED))+(L5)*sizeof(EXPR_HOISTED);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTE1761212196_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTE1761212196) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTE1761212196)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTE1761212196)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTE1761212196_struct)-sizeof(EXPR_HOISTED))+(L14)*sizeof(EXPR_HOISTED);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTE1761212196_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTE1761212196) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTE354925707_frame_struct other1_0;
FLISTE354925707_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTE1761212196)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTE354925707(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTE1761212196)L21,L31,(EXPR_HOISTED)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTE1761212196)aset_self,aset_ind,(EXPR_HOISTED)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTF7594819 FLISTF1203220157(FLISTF7594819 self, FLISTdAM_CONST e) {
 FLISTF7594819 ret_val;
 FLISTF7594819 r = ((FLISTF7594819) NULL);
 FLISTF7594819 L11;
 FLISTF7594819 L21;
 INT L31 = INT_zero;
 FLISTF7594819 aset_self;
 INT aset_ind = INT_zero;
 FLISTdAM_CONST aset_val;
 FLISTF7594819 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTF7594819 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 FLISTdAM_CONST L22;
 FLISTdAM_CONST L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTF7594819) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTF7594819_struct)-sizeof(FLISTdAM_CONST))+(L5)*sizeof(FLISTdAM_CONST);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTF7594819_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTF7594819) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTF7594819)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTF7594819)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTF7594819_struct)-sizeof(FLISTdAM_CONST))+(L14)*sizeof(FLISTdAM_CONST);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTF7594819_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTF7594819) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTF687422861_frame_struct other1_0;
FLISTF687422861_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTF7594819)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTF687422861(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTF7594819)L21,L31,(FLISTdAM_CONST)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTF7594819)aset_self,aset_ind,(FLISTdAM_CONST)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTFLISTSTR FLISTF630314943(FLISTFLISTSTR self, FLISTSTR e) {
 FLISTFLISTSTR ret_val;
 FLISTFLISTSTR r = ((FLISTFLISTSTR) NULL);
 FLISTFLISTSTR L11;
 FLISTFLISTSTR L21;
 INT L31 = INT_zero;
 FLISTFLISTSTR aset_self;
 INT aset_ind = INT_zero;
 FLISTSTR aset_val;
 FLISTFLISTSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTFLISTSTR L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 FLISTSTR L22;
 FLISTSTR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTFLISTSTR) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTFLISTSTR_struct)-sizeof(FLISTSTR))+(L5)*sizeof(FLISTSTR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTFLISTSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTFLISTSTR) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTFLISTSTR)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTFLISTSTR)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTFLISTSTR_struct)-sizeof(FLISTSTR))+(L14)*sizeof(FLISTSTR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTFLISTSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTFLISTSTR) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTF1766102529_frame_struct other1_0;
FLISTF1766102529_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTFLISTSTR)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTF1766102529(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTFLISTSTR)L21,L31,(FLISTSTR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTFLISTSTR)aset_self,aset_ind,(FLISTSTR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTFSTR FLISTF1280148353(FLISTFSTR self, FSTR e) {
 FLISTFSTR ret_val;
 FLISTFSTR r = ((FLISTFSTR) NULL);
 FLISTFSTR L11;
 FLISTFSTR L21;
 INT L31 = INT_zero;
 FLISTFSTR aset_self;
 INT aset_ind = INT_zero;
 FSTR aset_val;
 FLISTFSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTFSTR L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 FSTR L22;
 FSTR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTFSTR) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTFSTR_struct)-sizeof(FSTR))+(L5)*sizeof(FSTR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTFSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTFSTR) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTFSTR)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTFSTR)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTFSTR_struct)-sizeof(FSTR))+(L14)*sizeof(FSTR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTFSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTFSTR) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTF1277706721_frame_struct other1_0;
FLISTF1277706721_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTFSTR)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTF1277706721(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTFSTR)L21,L31,(FSTR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTFSTR)aset_self,aset_ind,(FSTR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTFSTR FLISTF524229941(FLISTFSTR self, FLISTFSTR l) {
 FLISTFSTR ret_val;
 FLISTFSTR res;
 INT oldsize = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT resi = INT_zero;
 FLISTFSTR aget_self;
 INT aget_ind = INT_zero;
 FSTR ret_val1;
 FLISTFSTR aset_self;
 INT aset_ind = INT_zero;
 FSTR aset_val;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 BOOL L61_;
 FSTR L71_;
 INT L91_;
 INT L101_;
 res = self;
 L1 = (l==((FLISTFSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  oldsize = FLISTF1348049713(self);
  L3 = FLISTF1348049713(self);
  L4 = FLISTF1348049713(l);
  L51_=INTPLUS(L3,L4); 
  res = FLISTF575242064(res, L51_);
  i = 0;
  sz = FLISTF1348049713(l);
  resi = oldsize;
  while (1) {
   L61_=(i)==(sz); 
   if (L61_) {
    goto after_loop;
   }
   aset_self = res;
   aset_ind = resi;
   aget_self = l;
   aget_ind = i;
   L71_=(FSTR)ARR((FLISTFSTR)aget_self,aget_ind); 
   ret_val1 = L71_;
   aset_val = ret_val1;
   SARR((FLISTFSTR)aset_self,aset_ind,(FSTR)aset_val); 
   ;
   L91_=INTPLUS(i,1); 
   i = L91_;
   L101_=INTPLUS(resi,1); 
   resi = L101_;
  }
  after_loop: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTFSTR FLISTF575242064(FLISTFSTR self, INT new_size) {
 FLISTFSTR ret_val;
 FLISTFSTR r = ((FLISTFSTR) NULL);
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTFSTR aget_self;
 INT aget_ind = INT_zero;
 FSTR ret_val1;
 FLISTFSTR aset_self;
 INT aset_ind = INT_zero;
 FSTR aset_val;
 FLISTFSTR L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 FLISTFSTR L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 FSTR L211_;
 INT L231_;
 if ((self==((FLISTFSTR) NULL))) {
  L31_=(5>new_size)?5:new_size; 
  L2 = L31_;
  L5=(sizeof(struct FLISTFSTR_struct)-sizeof(FSTR))+(L2)*sizeof(FSTR);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTFSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L1 = ((FLISTFSTR) L4);
  L1->asize = L2;
  r = L1;
 }
 else {
  L61_=ASIZE((FLISTFSTR)self); 
  L7 = L61_;
  L81_=(L7)<(new_size); 
  L9 = L81_;
  L101_=!(L9); 
  if (L101_) {
   r = self;
  }
  else {
   L131_=ASIZE((FLISTFSTR)self); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L16 = L151_;
   L171_=(L16>new_size)?L16:new_size; 
   L12 = L171_;
   L19=(sizeof(struct FLISTFSTR_struct)-sizeof(FSTR))+(L12)*sizeof(FSTR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTFSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L11 = ((FLISTFSTR) L18);
   L11->asize = L12;
   r = L11;
   SATTR(r,loc,ATTR(self,loc));
   i = 0;
   sz = FLISTF1348049713(self);
   while (1) {
    L201_=(i)==(sz); 
    if (L201_) {
     goto after_loop;
    }
    aset_self = r;
    aset_ind = i;
    aget_self = self;
    aget_ind = i;
    L211_=(FSTR)ARR((FLISTFSTR)aget_self,aget_ind); 
    ret_val1 = L211_;
    aset_val = ret_val1;
    SARR((FLISTFSTR)aset_self,aset_ind,(FSTR)aset_val); 
    ;
    L231_=INTPLUS(i,1); 
    i = L231_;
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 SATTR(r,loc,new_size);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTI1895665739 FLISTI1378884285(FLISTI1895665739 self, IMPL_INCLUDE e) {
 FLISTI1895665739 ret_val;
 FLISTI1895665739 r = ((FLISTI1895665739) NULL);
 FLISTI1895665739 L11;
 FLISTI1895665739 L21;
 INT L31 = INT_zero;
 FLISTI1895665739 aset_self;
 INT aset_ind = INT_zero;
 IMPL_INCLUDE aset_val;
 FLISTI1895665739 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTI1895665739 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 IMPL_INCLUDE L22;
 IMPL_INCLUDE L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTI1895665739) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTI1895665739_struct)-sizeof(IMPL_INCLUDE))+(L5)*sizeof(IMPL_INCLUDE);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTI1895665739_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTI1895665739) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTI1895665739)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTI1895665739)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTI1895665739_struct)-sizeof(IMPL_INCLUDE))+(L14)*sizeof(IMPL_INCLUDE);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTI1895665739_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTI1895665739) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTI381429157_frame_struct other1_0;
FLISTI381429157_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTI1895665739)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTI381429157(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTI1895665739)L21,L31,(IMPL_INCLUDE)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTI1895665739)aset_self,aset_ind,(IMPL_INCLUDE)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTIDENT FLISTI370551816(FLISTIDENT self, IDENT e) {
 FLISTIDENT ret_val;
 FLISTIDENT r = ((FLISTIDENT) NULL);
 FLISTIDENT L11;
 FLISTIDENT L21;
 INT L31 = INT_zero;
 FLISTIDENT aset_self;
 INT aset_ind = INT_zero;
 IDENT aset_val = IDENT_zero;
 FLISTIDENT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTIDENT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 IDENT L22;
 IDENT L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTIDENT) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTIDENT_struct)-sizeof(IDENT))+(L5)*sizeof(IDENT);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTIDENT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTIDENT) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTIDENT)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTIDENT)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTIDENT_struct)-sizeof(IDENT))+(L14)*sizeof(IDENT);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTIDENT_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTIDENT) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTI1709766432_frame_struct other1_0;
FLISTI1709766432_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTIDENT)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTI1709766432(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTIDENT)L21,L31,L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTIDENT)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTIFC FLISTI985525641(FLISTIFC self, IFC e) {
 FLISTIFC ret_val;
 FLISTIFC r = ((FLISTIFC) NULL);
 FLISTIFC L11;
 FLISTIFC L21;
 INT L31 = INT_zero;
 FLISTIFC aset_self;
 INT aset_ind = INT_zero;
 IFC aset_val;
 FLISTIFC L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTIFC L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 IFC L22;
 IFC L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTIFC) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTIFC_struct)-sizeof(IFC))+(L5)*sizeof(IFC);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTIFC_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTIFC) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTIFC)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTIFC)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTIFC_struct)-sizeof(IFC))+(L14)*sizeof(IFC);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTIFC_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTIFC) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTI1596257802_frame_struct other1_0;
FLISTI1596257802_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTIFC)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTI1596257802(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTIFC)L21,L31,(IFC)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTIFC)aset_self,aset_ind,(IFC)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTINT FLISTI1369678776(FLISTINT self, INT e) {
 FLISTINT ret_val;
 FLISTINT r = ((FLISTINT) NULL);
 FLISTINT L11;
 FLISTINT L21;
 INT L31 = INT_zero;
 FLISTINT aset_self;
 INT aset_ind = INT_zero;
 INT aset_val = INT_zero;
 FLISTINT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTINT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 INT L22;
 INT L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTINT) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTINT_struct)+1-sizeof(INT))+(L5)*sizeof(INT);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=FLISTINT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTINT) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTINT)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTINT)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTINT_struct)+1-sizeof(INT))+(L14)*sizeof(INT);
   L18=ZALLOC_LEAF_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   memset(L18,0,L19);
   ((OB)L18)->header.tag=FLISTINT_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTINT) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTI476396026_frame_struct other1_0;
FLISTI476396026_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTINT)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTI476396026(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTINT)L21,L31,L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTINT)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTINT FLISTI55819225(FLISTINT self, INT n) {
 FLISTINT ret_val;
 BOOL L11_;
 FLISTINT L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTINT) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTINT_struct)+1-sizeof(INT))+(L3)*sizeof(INT);
  L4=ZALLOC_LEAF_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  memset(L4,0,L5);
  ((OB)L4)->header.tag=FLISTINT_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTINT) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTSFILE_ID FLISTS1155895163(FLISTSFILE_ID self, SFILE_ID e) {
 FLISTSFILE_ID ret_val;
 FLISTSFILE_ID r = ((FLISTSFILE_ID) NULL);
 FLISTSFILE_ID L11;
 FLISTSFILE_ID L21;
 INT L31 = INT_zero;
 FLISTSFILE_ID aset_self;
 INT aset_ind = INT_zero;
 SFILE_ID aset_val = SFILE_ID_zero;
 FLISTSFILE_ID L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTSFILE_ID L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 SFILE_ID L22;
 SFILE_ID L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTSFILE_ID) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTSFILE_ID_struct)+1-sizeof(SFILE_ID))+(L5)*sizeof(SFILE_ID);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=FLISTSFILE_ID_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTSFILE_ID) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTSFILE_ID)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTSFILE_ID)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTSFILE_ID_struct)+1-sizeof(SFILE_ID))+(L14)*sizeof(SFILE_ID);
   L18=ZALLOC_LEAF_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   memset(L18,0,L19);
   ((OB)L18)->header.tag=FLISTSFILE_ID_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTSFILE_ID) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTS2131268565_frame_struct other1_0;
FLISTS2131268565_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTSFILE_ID)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTS2131268565(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTSFILE_ID)L21,L31,L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTSFILE_ID)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSIG FLISTS69519306(FLISTSIG self, SIG e) {
 FLISTSIG ret_val;
 FLISTSIG r = ((FLISTSIG) NULL);
 FLISTSIG L11;
 FLISTSIG L21;
 INT L31 = INT_zero;
 FLISTSIG aset_self;
 INT aset_ind = INT_zero;
 SIG aset_val;
 FLISTSIG L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTSIG L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 SIG L22;
 SIG L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTSIG) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTSIG_struct)-sizeof(SIG))+(L5)*sizeof(SIG);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTSIG_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTSIG) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTSIG)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTSIG)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTSIG_struct)-sizeof(SIG))+(L14)*sizeof(SIG);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTSIG_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTSIG) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTS1074219534_frame_struct other1_0;
FLISTS1074219534_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTSIG)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTS1074219534(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTSIG)L21,L31,(SIG)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTSIG)aset_self,aset_ind,(SIG)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR FLISTS1425610062(FLISTSTR self, STR e) {
 FLISTSTR ret_val;
 FLISTSTR r = ((FLISTSTR) NULL);
 FLISTSTR L11;
 FLISTSTR L21;
 INT L31 = INT_zero;
 FLISTSTR aset_self;
 INT aset_ind = INT_zero;
 STR aset_val;
 FLISTSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTSTR L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 STR L22;
 STR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTSTR) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTSTR_struct)-sizeof(STR))+(L5)*sizeof(STR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTSTR) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTSTR)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTSTR)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTSTR_struct)-sizeof(STR))+(L14)*sizeof(STR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTSTR) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTSTR)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTS1171631134(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTSTR)L21,L31,(STR)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTSTR)aset_self,aset_ind,(STR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR FLISTS1900495285(FLISTSTR self, INT n) {
 FLISTSTR ret_val;
 BOOL L11_;
 FLISTSTR L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTSTR) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTSTR_struct)-sizeof(STR))+(L3)*sizeof(STR);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTSTR) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTT1145236345 FLISTT596895593(FLISTT1145236345 self, TUPAM_927437481 e) {
 FLISTT1145236345 ret_val;
 FLISTT1145236345 r = ((FLISTT1145236345) NULL);
 FLISTT1145236345 L11;
 FLISTT1145236345 L21;
 INT L31 = INT_zero;
 FLISTT1145236345 aset_self;
 INT aset_ind = INT_zero;
 TUPAM_927437481 aset_val = TUPAM_927437481_zero;
 FLISTT1145236345 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTT1145236345 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 TUPAM_927437481 L22;
 TUPAM_927437481 L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTT1145236345) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTT1145236345_struct)-sizeof(TUPAM_927437481))+(L5)*sizeof(TUPAM_927437481);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTT1145236345_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTT1145236345) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTT1145236345)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTT1145236345)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTT1145236345_struct)-sizeof(TUPAM_927437481))+(L14)*sizeof(TUPAM_927437481);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTT1145236345_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTT1145236345) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTT709128269_frame_struct other1_0;
FLISTT709128269_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTT1145236345)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTT709128269(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTT1145236345)L21,L31,L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTT1145236345)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdAM_CONST FLISTF471519751(FLISTF7594819 self) {
 FLISTdAM_CONST ret_val;
 FLISTdAM_CONST r;
 FLISTF7594819 aget_self;
 INT aget_ind = INT_zero;
 FLISTdAM_CONST ret_val1;
 FLISTF7594819 aset_self;
 INT aset_ind = INT_zero;
 FLISTdAM_CONST aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 FLISTdAM_CONST L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTF873512073(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((FLISTdAM_CONST) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(FLISTdAM_CONST)ARR((FLISTF7594819)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((FLISTdAM_CONST) NULL);
 SARR((FLISTF7594819)aset_self,aset_ind,(FLISTdAM_CONST)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC FLISTIFC_poprIFC(FLISTIFC self) {
 IFC ret_val;
 IFC r;
 FLISTIFC aget_self;
 INT aget_ind = INT_zero;
 IFC ret_val1;
 FLISTIFC aset_self;
 INT aset_ind = INT_zero;
 IFC aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 IFC L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTI751030673(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((IFC) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(IFC)ARR((FLISTIFC)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((IFC) NULL);
 SARR((FLISTIFC)aset_self,aset_ind,(IFC)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL_INCLUDE FLISTI961811375(FLISTI1895665739 self) {
 IMPL_INCLUDE ret_val;
 IMPL_INCLUDE r;
 FLISTI1895665739 aget_self;
 INT aget_ind = INT_zero;
 IMPL_INCLUDE ret_val1;
 FLISTI1895665739 aset_self;
 INT aset_ind = INT_zero;
 IMPL_INCLUDE aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 IMPL_INCLUDE L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTI377878565(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((IMPL_INCLUDE) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(IMPL_INCLUDE)ARR((FLISTI1895665739)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((IMPL_INCLUDE) NULL);
 SARR((FLISTI1895665739)aset_self,aset_ind,(IMPL_INCLUDE)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA1151473122(FLISTA725283029 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA725283029) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA1370145561(FLISTA725283029 self, AM_LOCAL_EXPR e) {
 INT ret_val = INT_zero;
 FLISTA725283029 L11;
 INT r = INT_zero;
 FLISTA725283029 aget_self;
 INT aget_ind = INT_zero;
 AM_LOCAL_EXPR ret_val1;
 FLISTA725283029 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val4 = BOOL_zero;
 BOOL L2;
 BOOL L31_;
 INT L4;
 AM_LOCAL_EXPR L51_;
 L2 = (self==((FLISTA725283029) NULL));
 L31_=!(L2); 
 if (L31_) {
  {
   struct FLISTA56457972_frame_struct other1_0;
FLISTA56457972_frame noname1 = &other1_0;
   L11 = self;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L4 = FLISTA56457972(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    r = L4;
    elt_eq_self = self;
    elt_eq_e1 = e;
    aget_self = self;
    aget_ind = r;
    L51_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)aget_self,aget_ind); 
    ret_val1 = L51_;
    elt_eq_e2 = ret_val1;
    elt_eq_self1 = ((ELT_EQ1704040611) NULL);
    elt_eq_e11 = elt_eq_e1;
    elt_eq_e21 = elt_eq_e2;
    is_eq_self = elt_eq_e11;
    is_eq_a = ((OB) elt_eq_e21);
    ret_val4 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
    ret_val3 = ret_val4;
    ret_val2 = ret_val3;
    if (ret_val2) {
     ret_val = r;
     return ret_val;
    }
   }
  }
  after_loop: ;
 }
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA1531002815(FLISTA1327600838 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA1327600838) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA1962912942(FLISTA1062334999 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA1062334999) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA23038851(FLISTA1062334999 self, AM_ITE1809135850 e) {
 INT ret_val = INT_zero;
 FLISTA1062334999 L11;
 INT r = INT_zero;
 FLISTA1062334999 aget_self;
 INT aget_ind = INT_zero;
 AM_ITE1809135850 ret_val1;
 FLISTA1062334999 elt_eq_self;
 AM_ITE1809135850 elt_eq_e1;
 AM_ITE1809135850 elt_eq_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQ334385345 elt_eq_self1;
 AM_ITE1809135850 elt_eq_e11;
 AM_ITE1809135850 elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 AM_ITE1809135850 is_eq_self;
 OB is_eq_a;
 BOOL ret_val4 = BOOL_zero;
 BOOL L2;
 BOOL L31_;
 INT L4;
 AM_ITE1809135850 L51_;
 L2 = (self==((FLISTA1062334999) NULL));
 L31_=!(L2); 
 if (L31_) {
  {
   struct FLISTA1237039204_frame_struct other1_0;
FLISTA1237039204_frame noname1 = &other1_0;
   L11 = self;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L4 = FLISTA1237039204(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    r = L4;
    elt_eq_self = self;
    elt_eq_e1 = e;
    aget_self = self;
    aget_ind = r;
    L51_=(AM_ITE1809135850)ARR((FLISTA1062334999)aget_self,aget_ind); 
    ret_val1 = L51_;
    elt_eq_e2 = ret_val1;
    elt_eq_self1 = ((ELT_EQ334385345) NULL);
    elt_eq_e11 = elt_eq_e1;
    elt_eq_e21 = elt_eq_e2;
    is_eq_self = elt_eq_e11;
    is_eq_a = ((OB) elt_eq_e21);
    ret_val4 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
    ret_val3 = ret_val4;
    ret_val2 = ret_val3;
    if (ret_val2) {
     ret_val = r;
     return ret_val;
    }
   }
  }
  after_loop: ;
 }
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA587993904(FLISTA886630705 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA886630705) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTB746188588(FLISTBOOL self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTBOOL) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTC1064619074(FLISTCODE_FILE self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTCODE_FILE) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTF1097166371(FLISTFLISTSTR self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTFLISTSTR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTF1348049713(FLISTFSTR self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTFSTR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTF873512073(FLISTF7594819 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTF7594819) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTI1321626366(FLISTINT self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTINT) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTI1328284422(FLISTIDENT self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTIDENT) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTI377878565(FLISTI1895665739 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTI1895665739) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTI751030673(FLISTIFC self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTIFC) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTINT_poprINT(FLISTINT self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 FLISTINT aget_self;
 INT aget_ind = INT_zero;
 INT ret_val1 = INT_zero;
 FLISTINT aset_self;
 INT aset_ind = INT_zero;
 INT aset_val = INT_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTI1321626366(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = INT_zero;
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=ARR((FLISTINT)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = INT_zero;
 SARR((FLISTINT)aset_self,aset_ind,aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTS228922656(FLISTSIG self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTSIG) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTS326330076(FLISTSTR self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTSTR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTS434496825(FLISTSFILE_ID self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTSFILE_ID) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SIG FLISTSIG_poprSIG(FLISTSIG self) {
 SIG ret_val;
 SIG r;
 FLISTSIG aget_self;
 INT aget_ind = INT_zero;
 SIG ret_val1;
 FLISTSIG aset_self;
 INT aset_ind = INT_zero;
 SIG aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 SIG L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTS228922656(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((SIG) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(SIG)ARR((FLISTSIG)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((SIG) NULL);
 SARR((FLISTSIG)aset_self,aset_ind,(SIG)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG FLISTSIG_toprSIG(FLISTSIG self) {
 SIG ret_val;
 FLISTSIG aget_self;
 INT aget_ind = INT_zero;
 SIG ret_val1;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 SIG L51_;
 L1 = FLISTS228922656(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((SIG) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(SIG)ARR((FLISTSIG)aget_self,aget_ind); 
 ret_val1 = L51_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1208864070(FLISTA112326087 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA112326087) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL541085071(((ELT_AL2088983704) NULL), ((dELTAM1943610286) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1228646807(FLISTA1327600838 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA1327600838) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1523154184(((ELT_AL533589695) NULL), ((dELTAR67678756) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA128466217(FLISTAS_FEAT_MOD self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTAS_FEAT_MOD) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1563649328(((ELT_AL454519361) NULL), ((dELTAS_FEAT_MOD) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1345532203(FLISTARRAYSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTARRAYSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1024390623(((ELT_ALGARRAYSTR) NULL), ((dELTARRAYSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1366495389(FLISTA2097324934 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA2097324934) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL769058240(((ELT_AL1403631219) NULL), ((dELTAM937720280) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1598488884(FLISTA886630705 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA886630705) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL741225494(((ELT_AL92619562) NULL), ((dELTAM373291377) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1623803758(FLISTATTRSORT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTATTRSORT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1455618083(((ELT_ALGATTRSORT) NULL), ((dELTATTRSORT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1774499089(FLISTAM_CALL_ARG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTAM_CALL_ARG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1734287228(((ELT_AL1348837573) NULL), ((dELTAM_CALL_ARG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1884067430(FLISTA1062334999 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA1062334999) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1906929438(((ELT_AL911461552) NULL), ((dELTAM938692099) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA2047982482(FLISTA725283029 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA725283029) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL127733498(((ELT_AL1519294172) NULL), ((dELTAM1985205111) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA2143887329(FLISTAM_CURSOR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTAM_CURSOR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1119786764(((ELT_ALGAM_CURSOR) NULL), ((dELTAM_CURSOR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA574012977(FLISTARRAYBOOL self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTARRAYBOOL) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1852369832(((ELT_ALGARRAYBOOL) NULL), ((dELTARRAYBOOL) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA674592310(FLISTAM_ROUT_DEF self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTAM_ROUT_DEF) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1544151106(((ELT_AL1523214528) NULL), ((dELTAM_ROUT_DEF) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTARG_strrSTR(FLISTARG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTARG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL755347248(((ELT_ALGARG) NULL), ((dELTARG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTB1590162857(FLISTBOOL self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTBOOL) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL360284389(((ELT_ALGBOOL) NULL), ((dELTBOOL) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTB2097638834(FLISTB2062080075 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTB2062080075) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1399860630(((ELT_AL88283524) NULL), ((dELTBO61052977) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTB2138238497(FLISTB475591118 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTB475591118) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL130847997(((ELT_AL1115246319) NULL), ((dELTBO2026178203) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTB275387800(FLISTB16124645 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTB16124645) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2071496711(((ELT_AL2109782150) NULL), ((dELTBU1847408844) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTC1168586346(FLISTC1031309271 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTC1031309271) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1010662471(((ELT_AL1044938984) NULL), ((dELTCLASS_LAYOUT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTC1817360302(FLISTCODE_FILE self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTCODE_FILE) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL519275506(((ELT_ALGCODE_FILE) NULL), ((dELTCODE_FILE) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTC1921340409(FLISTC1772242452 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTC1772242452) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1886121199(((ELT_AL1668358421) NULL), ((dELTCS1440874327) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTC31611564(FLISTCALL_ARG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTCALL_ARG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2059371083(((ELT_ALGCALL_ARG) NULL), ((dELTCALL_ARG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTC780849889(FLISTCHAR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTCHAR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1065029381(((ELT_ALGCHAR) NULL), ((dELTCHAR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTE1365889449(FLISTE1761212196 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTE1761212196) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL808485957(((ELT_AL315036059) NULL), ((dELTEXPR_HOISTED) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTE283073321(FLISTE1627119380 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTE1627119380) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL550997356(((ELT_AL1543060947) NULL), ((dELTEX2061780774) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTF1366971496(FLISTFLISTSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTFLISTSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL854472091(((ELT_ALGFLISTSTR) NULL), ((dELTFLISTSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTF786174088(FLISTFSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTFSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2090488585(((ELT_ALGFSTR) NULL), ((dELTFSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTF93053878(FLISTF7594819 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTF7594819) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL101060479(((ELT_AL2101252324) NULL), ((dELTFL1838879018) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTI1877263384(FLISTI2086541899 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTI2086541899) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL288668417(((ELT_AL1446886698) NULL), ((dELTIM293343924) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTI2007266978(FLISTI1895665739 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTI1895665739) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1033451287(((ELT_AL180582516) NULL), ((dELTIMPL_INCLUDE) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTI700982870(FLISTIDENT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTIDENT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1534167666(((ELT_ALGIDENT) NULL), ((dELTIDENT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTIFC_strrSTR(FLISTIFC self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTIFC) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1598639248(((ELT_ALGIFC) NULL), ((dELTIFC) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTINT_strrSTR(FLISTINT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTINT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL479013642(((ELT_ALGINT) NULL), ((dELTINT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTO1373905061(FLISTO1593816750 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTO1593816750) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1417078085(((ELT_AL499840755) NULL), ((dELTOP237467449) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTS1154465862(FLISTSFILE_ID self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTSFILE_ID) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1425453107(((ELT_ALGSFILE_ID) NULL), ((dELTSFILE_ID) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTS1349623250(FLISTSIDE_EFFECT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTSIDE_EFFECT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1688888374(((ELT_AL569350364) NULL), ((dELTSIDE_EFFECT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTSIG_strrSTR(FLISTSIG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTSIG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL481240086(((ELT_ALGSIG) NULL), ((dELTSIG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTSTR_poprSTR(FLISTSTR self) {
 STR ret_val;
 STR r;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 FLISTSTR aset_self;
 INT aset_ind = INT_zero;
 STR aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 STR L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTS326330076(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((STR) NULL);
 SARR((FLISTSTR)aset_self,aset_ind,(STR)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTSTR_strrSTR(FLISTSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1717624110(((ELT_ALGSTR) NULL), ((dELTSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTSTR_toprSTR(FLISTSTR self) {
 STR ret_val;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 STR L51_;
 L1 = FLISTS326330076(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
 ret_val1 = L51_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1518857242(FLISTT1145236345 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1145236345) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL150680983(((ELT_AL2090369736) NULL), ((dELTTU789674962) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1750053916(FLISTTP_CLASS self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTP_CLASS) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1973394827(((ELT_ALGTP_CLASS) NULL), ((dELTTP_CLASS) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1896886665(FLISTTP_LAYOUT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTP_LAYOUT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1033027484(((ELT_ALGTP_LAYOUT) NULL), ((dELTTP_LAYOUT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT2098083178(FLISTT1311334615 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1311334615) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL4458855(((ELT_AL420149736) NULL), ((dELTTU1558822270) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT709853181(FLISTT1661203254 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1661203254) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL378639519(((ELT_AL1230514763) NULL), ((dELTTU847844253) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void FLISTA739122510(FLISTA1062334999 self, INT ind1) {
 INT i = INT_zero;
 FLISTA1062334999 aget_self;
 INT aget_ind = INT_zero;
 AM_ITE1809135850 ret_val;
 FLISTA1062334999 aset_self;
 INT aset_ind = INT_zero;
 AM_ITE1809135850 aset_val;
 INT L11_;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 INT L61_;
 AM_ITE1809135850 L71_;
 INT L91_;
 INT L10;
 INT L121_;
 L11_=INTPLUS(ind1,1); 
 i = L11_;
 while (1) {
  L2 = FLISTA1962912942(self);
  L31_=(i)<(L2); 
  L4 = L31_;
  L51_=!(L4); 
  if (L51_) {
   goto after_loop;
  }
  aset_self = self;
  L61_=INTMINUS(i,1); 
  aset_ind = L61_;
  aget_self = self;
  aget_ind = i;
  L71_=(AM_ITE1809135850)ARR((FLISTA1062334999)aget_self,aget_ind); 
  ret_val = L71_;
  aset_val = ret_val;
  SARR((FLISTA1062334999)aset_self,aset_ind,(AM_ITE1809135850)aset_val); 
  ;
  L91_=INTPLUS(i,1); 
  i = L91_;
 }
 after_loop: ;
 L10 = ATTR(self,loc);
 L121_=INTMINUS(L10,1); 
 SATTR(self,loc,L121_);
}


#undef IS_ITER
#define IS_ITER 1

AM_CALL_ARG FLISTA964597258(FLISTA964597258_frame frame) {
 AM_CALL_ARG dummy = ((AM_CALL_ARG) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_CALL_ARG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTAM_CALL_ARG) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_CALL_ARG)ARR((FLISTAM_CALL_ARG)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_CURSOR FLISTA84883712(FLISTA84883712_frame frame) {
 AM_CURSOR dummy = ((AM_CURSOR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_CURSOR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTAM_CURSOR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_CURSOR)ARR((FLISTAM_CURSOR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame frame) {
 AM_CURSOR_POS dummy = ((AM_CURSOR_POS) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_CURSOR_POS L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA886630705) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_CURSOR_POS)ARR((FLISTA886630705)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_FORMAL_ARG FLISTA831183340(FLISTA831183340_frame frame) {
 AM_FORMAL_ARG dummy = ((AM_FORMAL_ARG) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_FORMAL_ARG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA2097324934) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_FORMAL_ARG)ARR((FLISTA2097324934)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame frame) {
 AM_ITE1809135850 dummy = ((AM_ITE1809135850) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_ITE1809135850 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA1062334999) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_ITE1809135850)ARR((FLISTA1062334999)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame frame) {
 AM_LOCAL_EXPR dummy = ((AM_LOCAL_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_LOCAL_EXPR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA725283029) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_LOCAL_EXPR)ARR((FLISTA725283029)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_ROUT_DEF FLISTA2121868826(FLISTA2121868826_frame frame) {
 AM_ROUT_DEF dummy = ((AM_ROUT_DEF) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_ROUT_DEF L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTAM_ROUT_DEF) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_ROUT_DEF)ARR((FLISTAM_ROUT_DEF)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_SHARED_EXPR FLISTA120298683(FLISTA120298683_frame frame) {
 AM_SHARED_EXPR dummy = ((AM_SHARED_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_SHARED_EXPR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA112326087) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_SHARED_EXPR)ARR((FLISTA112326087)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ARG FLISTA420234122(FLISTA420234122_frame frame) {
 ARG dummy = ((ARG) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ARG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTARG) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ARG)ARR((FLISTARG)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ARRAYBOOL FLISTA538157944(FLISTA538157944_frame frame) {
 ARRAYBOOL dummy = ((ARRAYBOOL) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ARRAYBOOL L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTARRAYBOOL) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ARRAYBOOL)ARR((FLISTARRAYBOOL)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ARRAYSTR FLISTA970595569(FLISTA970595569_frame frame) {
 ARRAYSTR dummy = ((ARRAYSTR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ARRAYSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTARRAYSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ARRAYSTR)ARR((FLISTARRAYSTR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame frame) {
 ARRAYdAM_EXPR dummy = ((ARRAYdAM_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ARRAYdAM_EXPR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA1327600838) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AS_FEAT_MOD FLISTA1853093614(FLISTA1853093614_frame frame) {
 AS_FEAT_MOD dummy = ((AS_FEAT_MOD) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AS_FEAT_MOD L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTAS_FEAT_MOD) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AS_FEAT_MOD)ARR((FLISTAS_FEAT_MOD)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ATTRSORT FLISTA901142755(FLISTA901142755_frame frame) {
 ATTRSORT dummy = ((ATTRSORT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ATTRSORT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTATTRSORT) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ATTRSORT)ARR((FLISTATTRSORT)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BOOL FLISTB1880887881(FLISTB1880887881_frame frame) {
 BOOL dummy = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOOL L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTBOOL) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTBOOL)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BOUND_2140373124 FLISTB455167321(FLISTB455167321_frame frame) {
 BOUND_2140373124 dummy = ((BOUND_2140373124) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOUND_2140373124 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTB475591118) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(BOUND_2140373124)ARR((FLISTB475591118)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BOUND_809390774 FLISTB905996620(FLISTB905996620_frame frame) {
 BOUND_809390774 dummy = ((BOUND_809390774) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOUND_809390774 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTB2062080075) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(BOUND_809390774)ARR((FLISTB2062080075)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BUILTIN_LAYOUT FLISTB334826071(FLISTB334826071_frame frame) {
 BUILTIN_LAYOUT dummy = ((BUILTIN_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BUILTIN_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTB16124645) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(BUILTIN_LAYOUT)ARR((FLISTB16124645)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

CALL_ARG FLISTC264978409(FLISTC264978409_frame frame) {
 CALL_ARG dummy = ((CALL_ARG) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CALL_ARG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTCALL_ARG) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(CALL_ARG)ARR((FLISTCALL_ARG)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

CHAR FLISTC1776759929(FLISTC1776759929_frame frame) {
 CHAR dummy = CHAR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CHAR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTCHAR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTCHAR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

CLASS_LAYOUT FLISTC519862611(FLISTC519862611_frame frame) {
 CLASS_LAYOUT dummy = ((CLASS_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CLASS_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTC1031309271) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(CLASS_LAYOUT)ARR((FLISTC1031309271)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

CODE_FILE FLISTC1947519404(FLISTC1947519404_frame frame) {
 CODE_FILE dummy = ((CODE_FILE) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CODE_FILE L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTCODE_FILE) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(CODE_FILE)ARR((FLISTCODE_FILE)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

CSE_DOUBLE_EXPRS FLISTC1263372315(FLISTC1263372315_frame frame) {
 CSE_DOUBLE_EXPRS dummy = ((CSE_DOUBLE_EXPRS) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CSE_DOUBLE_EXPRS L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTC1772242452) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(CSE_DOUBLE_EXPRS)ARR((FLISTC1772242452)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

EXPR_HOISTED FLISTE354925707(FLISTE354925707_frame frame) {
 EXPR_HOISTED dummy = ((EXPR_HOISTED) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 EXPR_HOISTED L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTE1761212196) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(EXPR_HOISTED)ARR((FLISTE1761212196)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

EXTERNAL_LAYOUT FLISTE1972070322(FLISTE1972070322_frame frame) {
 EXTERNAL_LAYOUT dummy = ((EXTERNAL_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 EXTERNAL_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTE1627119380) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(EXTERNAL_LAYOUT)ARR((FLISTE1627119380)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

FLISTSTR FLISTF1766102529(FLISTF1766102529_frame frame) {
 FLISTSTR dummy = ((FLISTSTR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 FLISTSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTFLISTSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(FLISTSTR)ARR((FLISTFLISTSTR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

FLISTdAM_CONST FLISTF687422861(FLISTF687422861_frame frame) {
 FLISTdAM_CONST dummy = ((FLISTdAM_CONST) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 FLISTdAM_CONST L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTF7594819) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(FLISTdAM_CONST)ARR((FLISTF7594819)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

FSTR FLISTF1277706721(FLISTF1277706721_frame frame) {
 FSTR dummy = ((FSTR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 FSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTFSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(FSTR)ARR((FLISTFSTR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

IDENT FLISTI1709766432(FLISTI1709766432_frame frame) {
 IDENT dummy = IDENT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 IDENT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTIDENT) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTIDENT)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

IFC FLISTI1596257802(FLISTI1596257802_frame frame) {
 IFC dummy = ((IFC) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 IFC L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTIFC) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(IFC)ARR((FLISTIFC)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

IMMUTA421759893 FLISTI244390455(FLISTI244390455_frame frame) {
 IMMUTA421759893 dummy = ((IMMUTA421759893) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 IMMUTA421759893 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTI2086541899) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(IMMUTA421759893)ARR((FLISTI2086541899)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

IMPL_INCLUDE FLISTI381429157(FLISTI381429157_frame frame) {
 IMPL_INCLUDE dummy = ((IMPL_INCLUDE) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 IMPL_INCLUDE L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTI1895665739) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(IMPL_INCLUDE)ARR((FLISTI1895665739)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT FLISTA1237039204(FLISTA1237039204_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 INT L4;
 INT L7;
 INT L81_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA1062334999) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   frame->f_L31_ = TRUE;
   while (1) {
    frame->state = 1;
    if (frame->f_L31_) {
     frame->f_L31_ = FALSE;
     frame->L51 = 0;
     L7 = ATTR(frame->self,loc);
     L81_=INTMINUS(L7,1); 
     frame->L61 = L81_;
     frame->L31_=frame->L51-1;frame->L31_m=frame->L61; 
    }
    if(frame->L31_++>=frame->L31_m)  goto after_loop; 
    return frame->L31_;
    state1:;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT FLISTA1878650141(FLISTA1878650141_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 INT L4;
 INT L7;
 INT L81_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTAM_CURSOR) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   frame->f_L31_ = TRUE;
   while (1) {
    frame->state = 1;
    if (frame->f_L31_) {
     frame->f_L31_ = FALSE;
     frame->L51 = 0;
     L7 = ATTR(frame->self,loc);
     L81_=INTMINUS(L7,1); 
     frame->L61 = L81_;
     frame->L31_=frame->L51-1;frame->L31_m=frame->L61; 
    }
    if(frame->L31_++>=frame->L31_m)  goto after_loop; 
    return frame->L31_;
    state1:;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT FLISTA56457972(FLISTA56457972_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 INT L4;
 INT L7;
 INT L81_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA725283029) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   frame->f_L31_ = TRUE;
   while (1) {
    frame->state = 1;
    if (frame->f_L31_) {
     frame->f_L31_ = FALSE;
     frame->L51 = 0;
     L7 = ATTR(frame->self,loc);
     L81_=INTMINUS(L7,1); 
     frame->L61 = L81_;
     frame->L31_=frame->L51-1;frame->L31_m=frame->L61; 
    }
    if(frame->L31_++>=frame->L31_m)  goto after_loop; 
    return frame->L31_;
    state1:;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT FLISTE18407748(FLISTE18407748_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 INT L4;
 INT L7;
 INT L81_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTE1761212196) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   frame->f_L31_ = TRUE;
   while (1) {
    frame->state = 1;
    if (frame->f_L31_) {
     frame->f_L31_ = FALSE;
     frame->L51 = 0;
     L7 = ATTR(frame->self,loc);
     L81_=INTMINUS(L7,1); 
     frame->L61 = L81_;
     frame->L31_=frame->L51-1;frame->L31_m=frame->L61; 
    }
    if(frame->L31_++>=frame->L31_m)  goto after_loop; 
    return frame->L31_;
    state1:;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT FLISTI476396026(FLISTI476396026_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 INT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTINT) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTINT)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

OPT_LOCAL_EXPR FLISTO2008856511(FLISTO2008856511_frame frame) {
 OPT_LOCAL_EXPR dummy = ((OPT_LOCAL_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 OPT_LOCAL_EXPR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTO1593816750) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(OPT_LOCAL_EXPR)ARR((FLISTO1593816750)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SFILE_ID FLISTS2131268565(FLISTS2131268565_frame frame) {
 SFILE_ID dummy = SFILE_ID_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 SFILE_ID L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTSFILE_ID) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTSFILE_ID)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SIDE_EFFECT FLISTS1351415114(FLISTS1351415114_frame frame) {
 SIDE_EFFECT dummy = ((SIDE_EFFECT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 SIDE_EFFECT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTSIDE_EFFECT) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(SIDE_EFFECT)ARR((FLISTSIDE_EFFECT)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SIG FLISTS1074219534(FLISTS1074219534_frame frame) {
 SIG dummy = ((SIG) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 SIG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTSIG) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(SIG)ARR((FLISTSIG)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR FLISTS1171631134(FLISTS1171631134_frame frame) {
 STR dummy = ((STR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 STR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(STR)ARR((FLISTSTR)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS FLISTT1683973815(FLISTT1683973815_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TP_CLASS L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTP_CLASS) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(TP_CLASS)ARR((FLISTTP_CLASS)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_LAYOUT FLISTT2106747232(FLISTT2106747232_frame frame) {
 TP_LAYOUT dummy = ((TP_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TP_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTP_LAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(TP_LAYOUT)ARR((FLISTTP_LAYOUT)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TUPAM_304458649 FLISTT726119853(FLISTT726119853_frame frame) {
 TUPAM_304458649 dummy = TUPAM_304458649_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_304458649 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1311334615) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTT1311334615)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TUPAM_667892060 FLISTT193552257(FLISTT193552257_frame frame) {
 TUPAM_667892060 dummy = TUPAM_667892060_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_667892060 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1661203254) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTT1661203254)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TUPAM_927437481 FLISTT709128269(FLISTT709128269_frame frame) {
 TUPAM_927437481 dummy = TUPAM_927437481_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_927437481 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1145236345) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=ARR((FLISTT1145236345)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void FLISTA1349709692(FLISTA1349709692_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA1327600838)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA1327600838)frame->self,frame->i_I_u_I,(ARRAYdAM_EXPR)frame->arg1); frame->i_I_u_I++;
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

void FLISTA1482204797(FLISTA1482204797_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA112326087)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA112326087)frame->self,frame->i_I_u_I,(AM_SHARED_EXPR)frame->arg1); frame->i_I_u_I++;
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

void FLISTA1933262168(FLISTA1933262168_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTAM_CURSOR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTAM_CURSOR)frame->self,frame->i_I_u_I,(AM_CURSOR)frame->arg1); frame->i_I_u_I++;
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

void FLISTA259226942(FLISTA259226942_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA886630705)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA886630705)frame->self,frame->i_I_u_I,(AM_CURSOR_POS)frame->arg1); frame->i_I_u_I++;
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

void FLISTA340495062(FLISTA340495062_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA725283029)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA725283029)frame->self,frame->i_I_u_I,(AM_LOCAL_EXPR)frame->arg1); frame->i_I_u_I++;
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

void FLISTA443212798(FLISTA443212798_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA1062334999)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA1062334999)frame->self,frame->i_I_u_I,(AM_ITE1809135850)frame->arg1); frame->i_I_u_I++;
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

void FLISTA496550642(FLISTA496550642_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTAM_ROUT_DEF)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTAM_ROUT_DEF)frame->self,frame->i_I_u_I,(AM_ROUT_DEF)frame->arg1); frame->i_I_u_I++;
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

void FLISTB2086724459(FLISTB2086724459_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTBOOL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTBOOL)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FLISTC542105842(FLISTC542105842_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTCODE_FILE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTCODE_FILE)frame->self,frame->i_I_u_I,(CODE_FILE)frame->arg1); frame->i_I_u_I++;
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

void FLISTE1675330331(FLISTE1675330331_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTE1761212196)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTE1761212196)frame->self,frame->i_I_u_I,(EXPR_HOISTED)frame->arg1); frame->i_I_u_I++;
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

void FLISTF1980391255(FLISTF1980391255_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTFSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTFSTR)frame->self,frame->i_I_u_I,(FSTR)frame->arg1); frame->i_I_u_I++;
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

void FLISTF2058849421(FLISTF2058849421_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTF7594819)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTF7594819)frame->self,frame->i_I_u_I,(FLISTdAM_CONST)frame->arg1); frame->i_I_u_I++;
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

void FLISTF206425427(FLISTF206425427_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTFLISTSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTFLISTSTR)frame->self,frame->i_I_u_I,(FLISTSTR)frame->arg1); frame->i_I_u_I++;
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

void FLISTI1211273540(FLISTI1211273540_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTIFC)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTIFC)frame->self,frame->i_I_u_I,(IFC)frame->arg1); frame->i_I_u_I++;
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

void FLISTI304092359(FLISTI304092359_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTI1895665739)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTI1895665739)frame->self,frame->i_I_u_I,(IMPL_INCLUDE)frame->arg1); frame->i_I_u_I++;
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

void FLISTI485498674(FLISTI485498674_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FLISTI747041326(FLISTI747041326_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTIDENT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTIDENT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FLISTS118774442(FLISTS118774442_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTSIG)frame->self,frame->i_I_u_I,(SIG)frame->arg1); frame->i_I_u_I++;
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

void FLISTS1731970718(FLISTS1731970718_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTSTR)frame->self,frame->i_I_u_I,(STR)frame->arg1); frame->i_I_u_I++;
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

void FLISTS1763978443(FLISTS1763978443_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTSFILE_ID)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTSFILE_ID)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FLISTT854840857(FLISTT854840857_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTT1145236345)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTT1145236345)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
