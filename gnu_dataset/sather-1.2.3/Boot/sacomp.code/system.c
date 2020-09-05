#include "sather.h"

/* Layouts */

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ABSTRA151498155_struct {/* layout for ABSTRACT_FRAME_LAYOUT */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *sig1;
 STR str;
 } *ABSTRA151498155;

typedef struct ABSTRACT_LAYOUT_struct {/* layout for ABSTRACT_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *ABSTRACT_LAYOUT;

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

typedef struct AREFFL1698233817_struct {/* layout for AREF{FLIST{AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTA725283029_struct *arr_part[1];
 } *AREFFL1698233817;

typedef struct ARG_LAYOUT_struct {/* layout for ARG_LAYOUT */
 OB_HEADER header;
 struct SIG_struct *sig1;
 STR str;
 } *ARG_LAYOUT;

typedef struct ARRAYdAM_CONST_struct {/* layout for ARRAY{$AM_CONST} */
 OB_HEADER header;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *ARRAYdAM_CONST;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYdAM_STMT_struct {/* layout for ARRAY{$AM_STMT} */
 OB_HEADER header;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *ARRAYdAM_STMT;

typedef struct ARRAYdLAYOUT_struct {/* layout for ARRAY{$LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *ARRAYdLAYOUT;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYA1870808380_struct {/* layout for ARRAY{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *ARRAYA1870808380;

typedef struct ARRAYA855692714_struct {/* layout for ARRAY{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *ARRAYA855692714;

typedef struct ARRAYA132553625_struct {/* layout for ARRAY{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *ARRAYA132553625;

typedef struct ARRAYAM_CALL_ARG_struct {/* layout for ARRAY{AM_CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *ARRAYAM_CALL_ARG;

typedef struct ARRAYA36280886_struct {/* layout for ARRAY{AM_CURSOR_POS} */
 OB_HEADER header;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *ARRAYA36280886;

typedef struct ARRAYAM_CURSOR_struct {/* layout for ARRAY{AM_CURSOR} */
 OB_HEADER header;
 INT asize;
 struct AM_CURSOR_struct *arr_part[1];
 } *ARRAYAM_CURSOR;

typedef struct ARRAYA1347292543_struct {/* layout for ARRAY{AM_FORMAL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *ARRAYA1347292543;

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

typedef struct AS_STMT_CURSOR_struct {/* layout for AS_STMT_CURSOR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_STMT_CURSOR;

typedef struct ATTRSORT_struct {/* layout for ATTRSORT */
 OB_HEADER header;
 struct ELT_struct *elt1;
 STR name1;
 } *ATTRSORT;

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct BOUND_2140373124_struct {/* layout for BOUND_ITER_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ITER_struct *tp;
 } *BOUND_2140373124;

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

typedef struct BOUND_809390774_struct {/* layout for BOUND_TYPE_LAYOUT */
 OB_HEADER header;
 STR str;
 struct TP_ROUT_struct *tp;
 } *BOUND_809390774;

typedef struct BUILD_TYPE_GRAPH_struct {/* layout for BUILD_TYPE_GRAPH */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *BUILD_TYPE_GRAPH;

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

typedef struct CHECK_AM_struct {/* layout for CHECK_AM */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_AM;

typedef struct CHECK_GENERICS_struct {/* layout for CHECK_GENERICS */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_GENERICS;

typedef struct CHECK_1174552075_struct {/* layout for CHECK_IFC_CONFORMANCE */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_1174552075;

typedef struct CLASS_LAYOUT_struct {/* layout for CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *CLASS_LAYOUT;

typedef struct CODE_FILE_ARRAY_struct {/* layout for CODE_FILE_ARRAY */
 OB_HEADER header;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *CODE_FILE_ARRAY;

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

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

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct CS_struct {/* layout for CS */
 OB_HEADER header;
 BOOL only_parse;
 BOOL only_reachable;
 } *CS;

typedef struct CSE_DOUBLE_EXPRS_struct {/* layout for CSE_DOUBLE_EXPRS */
 OB_HEADER header;
 struct FLISTdAM_EXPR_struct *expr;
 struct FLISTAM_CURSOR_struct *pos;
 struct FLISTA725283029_struct *locals1;
 } *CSE_DOUBLE_EXPRS;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

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

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct FIND_GENERICS_struct {/* layout for FIND_GENERICS */
 OB_HEADER header;
 struct FSETTP_CLASS_struct *tp_todo;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *generic_class;
 struct TP_CONTEXT_struct *con;
 } *FIND_GENERICS;

typedef struct FIND_TYPES_struct {/* layout for FIND_TYPES */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CONTEXT_struct *con;
 } *FIND_TYPES;

typedef struct FRAME_LAYOUT_struct {/* layout for FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_ROUT_DEF_struct *f;
 struct PROG_struct *prog;
 STR str;
 } *FRAME_LAYOUT;

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

typedef struct GET_MAIN_SIG_struct {/* layout for GET_MAIN_SIG */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *main_sig;
 } *GET_MAIN_SIG;

typedef struct GLOBAL_TBL_struct {/* layout for GLOBAL_TBL */
 OB_HEADER header;
 struct FLISTA112326087_struct *top_sort;
 struct FMAPdT953816637_struct *tbl;
 struct PROG_struct *prog;
 } *GLOBAL_TBL;

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

typedef struct IFC_ABS_CREATE_struct {/* layout for IFC_ABS_CREATE */
 OB_HEADER header;
 struct AS_CLASS_DEF_struct *tr;
 struct FLISTIFC_struct *supers;
 struct SIG_TBL_struct *class_sigs;
 struct TP_CLASS_struct *tp;
 struct TP_CONTEXT_struct *con;
 } *IFC_ABS_CREATE;

typedef struct IMMUTA421759893_struct {/* layout for IMMUTABLE_CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *IMMUTA421759893;

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

typedef struct LAYOUT_ARRAY_struct {/* layout for LAYOUT_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *LAYOUT_ARRAY;

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

typedef struct MS_RANDOM_GEN_struct {/* layout for MS_RANDOM_GEN */
 OB_HEADER header;
 INT seed;
 } *MS_RANDOM_GEN;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

typedef struct NULL_STAT_struct {/* layout for NULL_STAT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *NULL_STAT;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

typedef struct OPTIMIZE_struct {/* layout for OPTIMIZE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPTIMIZE;

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

typedef struct OPT_LOCAL_EXPR_struct {/* layout for OPT_LOCAL_EXPR */
 OB_HEADER header;
 struct AM_LOCAL_EXPR_struct *local1;
 INT attr_access;
 INT index1;
 BOOL is_arg;
 BOOL is_sometimes_far;
 BOOL near_arg;
 } *OPT_LOCAL_EXPR;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

typedef struct PARSE_struct {/* layout for PARSE */
 OB_HEADER header;
 struct FMAPSTRSTR_struct *has;
 struct FSETSTR_struct *convert_files;
 struct FSETSTR_struct *known_files;
 struct FSETSTR_struct *missing_classes;
 struct FSETSTR_struct *parsed;
 struct PROG_struct *prog;
 BOOL convert_all;
 BOOL version_1_0;
 } *PARSE;

typedef struct PRINT_OB_struct {/* layout for PRINT_OB */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *PRINT_OB;

typedef struct PROG_AS_TBL_struct {/* layout for PROG_AS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct AS_CLASS_DEF_struct *arr_part[1];
 } *PROG_AS_TBL;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

typedef struct SCANNER_struct {/* layout for SCANNER */
 OB_HEADER header;
 struct FSTR_struct *buf1;
 struct IDENT_struct lex_value;
 INT value_type;
 struct PROG_struct *prog;
 struct RAT_struct num_value;
 CHAR char_value;
 CHAR last_char;
 CHAR next;
 BOOL backed_up;
 BOOL pSather;
 BOOL zones;
 } *SCANNER;

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

typedef struct SIG_TBL_struct {/* layout for SIG_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *SIG_TBL;

typedef struct STAT1_struct {/* layout for STAT */
 OB_HEADER header;
 struct FMAPSTRINT_struct *map;
 } *STAT1;

typedef struct STR_CURSOR_struct {/* layout for STR_CURSOR */
 OB_HEADER header;
 INT error1;
 INT index1;
 INT line_no;
 STR buf1;
 CHAR comment_char1;
 CHAR comment_char2;
 BOOL is_done;
 } *STR_CURSOR;

typedef struct SYSTEM_LEX_struct {/* layout for SYSTEM_LEX */
 OB_HEADER header;
 struct FSTR_struct *buf1;
 INT lex_state;
 INT pos;
 STR name1;
 } *SYSTEM_LEX;

typedef struct TOKEN_struct {/* layout for TOKEN */
 INT val1;
 } TOKEN;
static TOKEN TOKEN_zero;

typedef struct TOKEN_boxed_struct {
 OB_HEADER header;
 TOKEN immutable_part;
 } *TOKEN_boxed;

typedef struct TP_ARRAY_struct {/* layout for TP_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *TP_ARRAY;

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

typedef struct TP_CLASS_TBL_struct {/* layout for TP_CLASS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_CLASS_TBL;

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

typedef struct TP_GEN_TBL_struct {/* layout for TP_GEN_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_GEN_TBL;

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

typedef struct TP_ITER_TBL_struct {/* layout for TP_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_ITER_struct *arr_part[1];
 } *TP_ITER_TBL;

typedef struct TP_LAYOUT_struct {/* layout for TP_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *TP_LAYOUT;

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

typedef struct TUPdAS268679779_struct {/* layout for TUP{$AS_EXPR,AS_ARG_MODE} */
 struct dAS_EXPR_struct *t1;
 struct AS_ARG_MODE_struct *t2;
 } TUPdAS268679779;
static TUPdAS268679779 TUPdAS268679779_zero;

typedef struct TUPdAS268679779_boxed_struct {
 OB_HEADER header;
 TUPdAS268679779 immutable_part;
 } *TUPdAS268679779_boxed;

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

typedef struct TUPdTPdLAYOUT_struct {/* layout for TUP{$TP,$LAYOUT} */
 struct dLAYOUT_struct *t2;
 struct dTP_struct *t1;
 } TUPdTPdLAYOUT;
static TUPdTPdLAYOUT TUPdTPdLAYOUT_zero;

typedef struct TUPdTPdLAYOUT_boxed_struct {
 OB_HEADER header;
 TUPdTPdLAYOUT immutable_part;
 } *TUPdTPdLAYOUT_boxed;

typedef struct TUPdTPCODE_FILE_struct {/* layout for TUP{$TP,CODE_FILE} */
 struct dTP_struct *t1;
 struct CODE_FILE_struct *t2;
 } TUPdTPCODE_FILE;
static TUPdTPCODE_FILE TUPdTPCODE_FILE_zero;

typedef struct TUPdTPCODE_FILE_boxed_struct {
 OB_HEADER header;
 TUPdTPCODE_FILE immutable_part;
 } *TUPdTPCODE_FILE_boxed;

typedef struct TUPdTP1710233051_struct {/* layout for TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 struct dTP_struct *t1;
 struct FMAPID739086519_struct *t2;
 } TUPdTP1710233051;
static TUPdTP1710233051 TUPdTP1710233051_zero;

typedef struct TUPdTP1710233051_boxed_struct {
 OB_HEADER header;
 TUPdTP1710233051 immutable_part;
 } *TUPdTP1710233051_boxed;

typedef struct TUPdTPINT_struct {/* layout for TUP{$TP,INT} */
 struct dTP_struct *t1;
 INT t2;
 } TUPdTPINT;
static TUPdTPINT TUPdTPINT_zero;

typedef struct TUPdTPINT_boxed_struct {
 OB_HEADER header;
 TUPdTPINT immutable_part;
 } *TUPdTPINT_boxed;

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

typedef struct TUPBOO1392923241_struct {/* layout for TUP{BOOL,INTI,INT,BOOL} */
 INT t3;
 struct INTI1_struct *t2;
 BOOL t1;
 BOOL t4;
 } TUPBOO1392923241;
static TUPBOO1392923241 TUPBOO1392923241_zero;

typedef struct TUPBOO1392923241_boxed_struct {
 OB_HEADER header;
 TUPBOO1392923241 immutable_part;
 } *TUPBOO1392923241_boxed;

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

typedef struct TUPIDENTARRAYdTP_struct {/* layout for TUP{IDENT,ARRAY{$TP}} */
 struct ARRAYdTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTARRAYdTP;
static TUPIDENTARRAYdTP TUPIDENTARRAYdTP_zero;

typedef struct TUPIDENTARRAYdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTARRAYdTP immutable_part;
 } *TUPIDENTARRAYdTP_boxed;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

typedef struct TUPINTINT_struct {/* layout for TUP{INT,INT} */
 INT t1;
 INT t2;
 } TUPINTINT;
static TUPINTINT TUPINTINT_zero;

typedef struct TUPINTINT_boxed_struct {
 OB_HEADER header;
 TUPINTINT immutable_part;
 } *TUPINTINT_boxed;

typedef struct TUPSIG1754078736_struct {/* layout for TUP{SIG,ABSTRACT_FRAME_LAYOUT} */
 struct ABSTRA151498155_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG1754078736;
static TUPSIG1754078736 TUPSIG1754078736_zero;

typedef struct TUPSIG1754078736_boxed_struct {
 OB_HEADER header;
 TUPSIG1754078736 immutable_part;
 } *TUPSIG1754078736_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct TUPSIGARG_LAYOUT_struct {/* layout for TUP{SIG,ARG_LAYOUT} */
 struct ARG_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIGARG_LAYOUT;
static TUPSIGARG_LAYOUT TUPSIGARG_LAYOUT_zero;

typedef struct TUPSIGARG_LAYOUT_boxed_struct {
 OB_HEADER header;
 TUPSIGARG_LAYOUT immutable_part;
 } *TUPSIGARG_LAYOUT_boxed;

typedef struct TUPSIG2023533247_struct {/* layout for TUP{SIG,FRAME_LAYOUT} */
 struct FRAME_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG2023533247;
static TUPSIG2023533247 TUPSIG2023533247_zero;

typedef struct TUPSIG2023533247_boxed_struct {
 OB_HEADER header;
 TUPSIG2023533247 immutable_part;
 } *TUPSIG2023533247_boxed;

typedef struct TUPSIGINT_struct {/* layout for TUP{SIG,INT} */
 INT t2;
 struct SIG_struct *t1;
 } TUPSIGINT;
static TUPSIGINT TUPSIGINT_zero;

typedef struct TUPSIGINT_boxed_struct {
 OB_HEADER header;
 TUPSIGINT immutable_part;
 } *TUPSIGINT_boxed;

typedef struct TUPSIGSIG_struct {/* layout for TUP{SIG,SIG} */
 struct SIG_struct *t1;
 struct SIG_struct *t2;
 } TUPSIGSIG;
static TUPSIGSIG TUPSIGSIG_zero;

typedef struct TUPSIGSIG_boxed_struct {
 OB_HEADER header;
 TUPSIGSIG immutable_part;
 } *TUPSIGSIG_boxed;

typedef struct TUPSIGSTR_struct {/* layout for TUP{SIG,STR} */
 struct SIG_struct *t1;
 STR t2;
 } TUPSIGSTR;
static TUPSIGSTR TUPSIGSTR_zero;

typedef struct TUPSIGSTR_boxed_struct {
 OB_HEADER header;
 TUPSIGSTR immutable_part;
 } *TUPSIGSTR_boxed;

typedef struct TUPSTRdCONFIG_struct {/* layout for TUP{STR,$CONFIG} */
 struct dCONFIG_struct *t2;
 STR t1;
 } TUPSTRdCONFIG;
static TUPSTRdCONFIG TUPSTRdCONFIG_zero;

typedef struct TUPSTRdCONFIG_boxed_struct {
 OB_HEADER header;
 TUPSTRdCONFIG immutable_part;
 } *TUPSTRdCONFIG_boxed;

typedef struct TUPSTRFSETSTR_struct {/* layout for TUP{STR,FSET{STR}} */
 struct FSETSTR_struct *t2;
 STR t1;
 } TUPSTRFSETSTR;
static TUPSTRFSETSTR TUPSTRFSETSTR_zero;

typedef struct TUPSTRFSETSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRFSETSTR immutable_part;
 } *TUPSTRFSETSTR_boxed;

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct TUPSTRSIG_struct {/* layout for TUP{STR,SIG} */
 struct SIG_struct *t2;
 STR t1;
 } TUPSTRSIG;
static TUPSTRSIG TUPSTRSIG_zero;

typedef struct TUPSTRSIG_boxed_struct {
 OB_HEADER header;
 TUPSTRSIG immutable_part;
 } *TUPSTRSIG_boxed;

typedef struct TUPSTRSTR_struct {/* layout for TUP{STR,STR} */
 STR t1;
 STR t2;
 } TUPSTRSTR;
static TUPSTRSTR TUPSTRSTR_zero;

typedef struct TUPSTRSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRSTR immutable_part;
 } *TUPSTRSTR_boxed;

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

typedef struct TUPTP_1668454172_struct {/* layout for TUP{TP_CLASS,FSET{TP_CLASS}} */
 struct FSETTP_CLASS_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_1668454172;
static TUPTP_1668454172 TUPTP_1668454172_zero;

typedef struct TUPTP_1668454172_boxed_struct {
 OB_HEADER header;
 TUPTP_1668454172 immutable_part;
 } *TUPTP_1668454172_boxed;

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

typedef struct AM_ANY_EXPR_struct {/* layout for AM_ANY_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_ANY_EXPR;

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

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

typedef struct AM_CHAR_CONST_struct {/* layout for AM_CHAR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 CHAR bval;
 } *AM_CHAR_CONST;

typedef struct AM_CLUSTER_EXPR_struct {/* layout for AM_CLUSTER_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLUSTER_EXPR;

typedef struct AM_CLU1286269335_struct {/* layout for AM_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLU1286269335;

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

typedef struct AM_CONST_struct {/* layout for AM_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_CONST;

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

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

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

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

typedef struct AM_GLOBAL_EXPR_struct {/* layout for AM_GLOBAL_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_GLOBAL_EXPR;

typedef struct AM_HERE_EXPR_struct {/* layout for AM_HERE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_HERE_EXPR;

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

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

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

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

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

typedef struct ARRAY2BOOL_struct {/* layout for ARRAY2{BOOL} */
 OB_HEADER header;
 INT size1;
 INT size2;
 INT asize;
 BOOL arr_part[1];
 } *ARRAY2BOOL;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYCHAR_struct {/* layout for ARRAY{CHAR} */
 OB_HEADER header;
 INT asize;
 CHAR arr_part[1];
 } *ARRAYCHAR;

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

typedef struct ARRAYSFILE_ID_struct {/* layout for ARRAY{SFILE_ID} */
 OB_HEADER header;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *ARRAYSFILE_ID;

typedef struct ARRAYT747572380_struct {/* layout for ARRAY{TUP{$OB,NAMESPACE}} */
 OB_HEADER header;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *ARRAYT747572380;

typedef struct ARRAYTUPdOBSTR_struct {/* layout for ARRAY{TUP{$OB,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *ARRAYTUPdOBSTR;

typedef struct ARRAYT1418209173_struct {/* layout for ARRAY{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *ARRAYT1418209173;

typedef struct ARRAYT1153101058_struct {/* layout for ARRAY{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *ARRAYT1153101058;

typedef struct ARRAYT1562141251_struct {/* layout for ARRAY{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *ARRAYT1562141251;

typedef struct ARRAYTUPdTPINT_struct {/* layout for ARRAY{TUP{$TP,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *ARRAYTUPdTPINT;

typedef struct ARRAYT1275404834_struct {/* layout for ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *ARRAYT1275404834;

typedef struct ARRAYT1146239647_struct {/* layout for ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *ARRAYT1146239647;

typedef struct ARRAYT2065045042_struct {/* layout for ARRAY{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_927437481_struct arr_part[1];
 } *ARRAYT2065045042;

typedef struct ARRAYT1753333425_struct {/* layout for ARRAY{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *ARRAYT1753333425;

typedef struct ARRAYT1744744076_struct {/* layout for ARRAY{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *ARRAYT1744744076;

typedef struct ARRAYT546647214_struct {/* layout for ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *ARRAYT546647214;

typedef struct ARRAYT1968212795_struct {/* layout for ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *ARRAYT1968212795;

typedef struct ARRAYTUPIDENTdTP_struct {/* layout for ARRAY{TUP{IDENT,$TP}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *ARRAYTUPIDENTdTP;

typedef struct ARRAYT263953675_struct {/* layout for ARRAY{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *ARRAYT263953675;

typedef struct ARRAYTUPIDENTINT_struct {/* layout for ARRAY{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *ARRAYTUPIDENTINT;

typedef struct ARRAYT348931282_struct {/* layout for ARRAY{TUP{SIG,ABSTRACT_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *ARRAYT348931282;

typedef struct ARRAYT537394765_struct {/* layout for ARRAY{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *ARRAYT537394765;

typedef struct ARRAYT1759168384_struct {/* layout for ARRAY{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *ARRAYT1759168384;

typedef struct ARRAYT1754348798_struct {/* layout for ARRAY{TUP{SIG,FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *ARRAYT1754348798;

typedef struct ARRAYTUPSIGINT_struct {/* layout for ARRAY{TUP{SIG,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGINT_struct arr_part[1];
 } *ARRAYTUPSIGINT;

typedef struct ARRAYTUPSIGSIG_struct {/* layout for ARRAY{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *ARRAYTUPSIGSIG;

typedef struct ARRAYTUPSIGSTR_struct {/* layout for ARRAY{TUP{SIG,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *ARRAYTUPSIGSTR;

typedef struct ARRAYT61112132_struct {/* layout for ARRAY{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *ARRAYT61112132;

typedef struct ARRAYT378612147_struct {/* layout for ARRAY{TUP{STR,FSET{STR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *ARRAYT378612147;

typedef struct ARRAYTUPSTRINT_struct {/* layout for ARRAY{TUP{STR,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *ARRAYTUPSTRINT;

typedef struct ARRAYTUPSTRSIG_struct {/* layout for ARRAY{TUP{STR,SIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *ARRAYTUPSTRSIG;

typedef struct ARRAYTUPSTRSTR_struct {/* layout for ARRAY{TUP{STR,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *ARRAYTUPSTRSTR;

typedef struct ARRAYT1127505716_struct {/* layout for ARRAY{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *ARRAYT1127505716;

typedef struct ARRAYT1641775270_struct {/* layout for ARRAY{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *ARRAYT1641775270;

typedef struct AS_AND_EXPR_struct {/* layout for AS_AND_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AND_EXPR;

typedef struct AS_ANY_EXPR_struct {/* layout for AS_ANY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ANY_EXPR;

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

typedef struct AS_AT_EXPR_struct {/* layout for AS_AT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AT_EXPR;

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

typedef struct AS_CLUSTER_EXPR_struct {/* layout for AS_CLUSTER_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLUSTER_EXPR;

typedef struct AS_CLU553570599_struct {/* layout for AS_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLU553570599;

typedef struct AS_COHORT_EXPR_struct {/* layout for AS_COHORT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct IDENT_struct name_after_pSath;
 struct SFILE_ID_struct source1;
 } *AS_COHORT_EXPR;

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

typedef struct AS_FAR_EXPR_struct {/* layout for AS_FAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_FAR_EXPR;

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

typedef struct AS_GLOBAL_EXPR_struct {/* layout for AS_GLOBAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_GLOBAL_EXPR;

typedef struct AS_HERE_EXPR_struct {/* layout for AS_HERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_HERE_EXPR;

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

typedef struct AS_NEAR_EXPR_struct {/* layout for AS_NEAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEAR_EXPR;

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

typedef struct AS_STMT_struct {/* layout for AS_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_STMT;

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

typedef struct AS_WHERE_EXPR_struct {/* layout for AS_WHERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_WHERE_EXPR;

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

typedef struct CONFIG_SCANNER_struct {/* layout for CONFIG_SCANNER */
 OB_HEADER header;
 struct FSTR_struct *fstr;
 INT line1;
 INT next;
 STR file1;
 } *CONFIG_SCANNER;

typedef struct CONVERT_struct {/* layout for CONVERT */
 OB_HEADER header;
 struct BFILE_struct *file1;
 INT end_pos;
 INT start_pos;
 BOOL need_to_convert;
 } *CONVERT;

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

typedef struct FLISTdLAYOUT_struct {/* layout for FLIST{$LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FLISTdLAYOUT;

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

typedef struct FLISTA162798263_struct {/* layout for FLIST{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FLISTA162798263;

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTAM_CALL_ARG_struct {/* layout for FLIST{AM_CALL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *FLISTAM_CALL_ARG;

typedef struct FLISTA886630705_struct {/* layout for FLIST{AM_CURSOR_POS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_POS_struct *arr_part[1];
 } *FLISTA886630705;

typedef struct FLISTAM_CURSOR_struct {/* layout for FLIST{AM_CURSOR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_CURSOR_struct *arr_part[1];
 } *FLISTAM_CURSOR;

typedef struct FLISTA2097324934_struct {/* layout for FLIST{AM_FORMAL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *FLISTA2097324934;

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

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

typedef struct FLISTA112326087_struct {/* layout for FLIST{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FLISTA112326087;

typedef struct FLISTARG_struct {/* layout for FLIST{ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *FLISTARG;

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FLISTARRAYBOOL_struct {/* layout for FLIST{ARRAY{BOOL}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYBOOL_struct *arr_part[1];
 } *FLISTARRAYBOOL;

typedef struct FLISTARRAYSTR_struct {/* layout for FLIST{ARRAY{STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *FLISTARRAYSTR;

typedef struct FLISTAS_FEAT_MOD_struct {/* layout for FLIST{AS_FEAT_MOD} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FLISTAS_FEAT_MOD;

typedef struct FLISTATTRSORT_struct {/* layout for FLIST{ATTRSORT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ATTRSORT_struct *arr_part[1];
 } *FLISTATTRSORT;

typedef struct FLISTBOOL_struct {/* layout for FLIST{BOOL} */
 OB_HEADER header;
 INT loc;
 INT asize;
 BOOL arr_part[1];
 } *FLISTBOOL;

typedef struct FLISTB475591118_struct {/* layout for FLIST{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FLISTB475591118;

typedef struct FLISTB2062080075_struct {/* layout for FLIST{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FLISTB2062080075;

typedef struct FLISTB16124645_struct {/* layout for FLIST{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FLISTB16124645;

typedef struct FLISTCALL_ARG_struct {/* layout for FLIST{CALL_ARG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CALL_ARG_struct *arr_part[1];
 } *FLISTCALL_ARG;

typedef struct FLISTCHAR_struct {/* layout for FLIST{CHAR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FLISTCHAR;

typedef struct FLISTC1031309271_struct {/* layout for FLIST{CLASS_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FLISTC1031309271;

typedef struct FLISTCODE_FILE_struct {/* layout for FLIST{CODE_FILE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *FLISTCODE_FILE;

typedef struct FLISTC1772242452_struct {/* layout for FLIST{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FLISTC1772242452;

typedef struct FLISTE1761212196_struct {/* layout for FLIST{EXPR_HOISTED} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct EXPR_HOISTED_struct *arr_part[1];
 } *FLISTE1761212196;

typedef struct FLISTE1627119380_struct {/* layout for FLIST{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FLISTE1627119380;

typedef struct FLISTF7594819_struct {/* layout for FLIST{FLIST{$AM_CONST}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTdAM_CONST_struct *arr_part[1];
 } *FLISTF7594819;

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

typedef struct FLISTIFC_struct {/* layout for FLIST{IFC} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IFC_struct *arr_part[1];
 } *FLISTIFC;

typedef struct FLISTI2086541899_struct {/* layout for FLIST{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FLISTI2086541899;

typedef struct FLISTI1895665739_struct {/* layout for FLIST{IMPL_INCLUDE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IMPL_INCLUDE_struct *arr_part[1];
 } *FLISTI1895665739;

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FLISTO1593816750_struct {/* layout for FLIST{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct OPT_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTO1593816750;

typedef struct FLISTSFILE_ID_struct {/* layout for FLIST{SFILE_ID} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FLISTSFILE_ID;

typedef struct FLISTSIDE_EFFECT_struct {/* layout for FLIST{SIDE_EFFECT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FLISTSIDE_EFFECT;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FLISTTP_CLASS_struct {/* layout for FLIST{TP_CLASS} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FLISTTP_CLASS;

typedef struct FLISTTP_LAYOUT_struct {/* layout for FLIST{TP_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TP_LAYOUT_struct *arr_part[1];
 } *FLISTTP_LAYOUT;

typedef struct FLISTT1513788273_struct {/* layout for FLIST{TUP{$OB,NAMESPACE}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FLISTT1513788273;

typedef struct FLISTTUPdOBSTR_struct {/* layout for FLIST{TUP{$OB,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *FLISTTUPdOBSTR;

typedef struct FLISTT567859354_struct {/* layout for FLIST{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FLISTT567859354;

typedef struct FLISTT880505585_struct {/* layout for FLIST{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *FLISTT880505585;

typedef struct FLISTT1858637310_struct {/* layout for FLIST{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FLISTT1858637310;

typedef struct FLISTTUPdTPINT_struct {/* layout for FLIST{TUP{$TP,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FLISTTUPdTPINT;

typedef struct FLISTT1311334615_struct {/* layout for FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FLISTT1311334615;

typedef struct FLISTT1661203254_struct {/* layout for FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FLISTT1661203254;

typedef struct FLISTT1145236345_struct {/* layout for FLIST{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_927437481_struct arr_part[1];
 } *FLISTT1145236345;

typedef struct FLISTT2049829484_struct {/* layout for FLIST{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FLISTT2049829484;

typedef struct FLISTT406286251_struct {/* layout for FLIST{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *FLISTT406286251;

typedef struct FLISTT31683607_struct {/* layout for FLIST{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *FLISTT31683607;

typedef struct FLISTT1683165204_struct {/* layout for FLIST{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *FLISTT1683165204;

typedef struct FLISTTUPIDENTdTP_struct {/* layout for FLIST{TUP{IDENT,$TP}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FLISTTUPIDENTdTP;

typedef struct FLISTT1023127882_struct {/* layout for FLIST{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FLISTT1023127882;

typedef struct FLISTTUPIDENTINT_struct {/* layout for FLIST{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FLISTTUPIDENTINT;

typedef struct FLISTT1075494997_struct {/* layout for FLIST{TUP{SIG,ABSTRACT_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FLISTT1075494997;

typedef struct FLISTT844804258_struct {/* layout for FLIST{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FLISTT844804258;

typedef struct FLISTT1742988937_struct {/* layout for FLIST{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FLISTT1742988937;

typedef struct FLISTT994805427_struct {/* layout for FLIST{TUP{SIG,FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FLISTT994805427;

typedef struct FLISTTUPSIGINT_struct {/* layout for FLIST{TUP{SIG,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGINT_struct arr_part[1];
 } *FLISTTUPSIGINT;

typedef struct FLISTTUPSIGSIG_struct {/* layout for FLIST{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FLISTTUPSIGSIG;

typedef struct FLISTTUPSIGSTR_struct {/* layout for FLIST{TUP{SIG,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *FLISTTUPSIGSTR;

typedef struct FLISTT789237687_struct {/* layout for FLIST{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *FLISTT789237687;

typedef struct FLISTT471737672_struct {/* layout for FLIST{TUP{STR,FSET{STR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FLISTT471737672;

typedef struct FLISTTUPSTRINT_struct {/* layout for FLIST{TUP{STR,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FLISTTUPSTRINT;

typedef struct FLISTTUPSTRSIG_struct {/* layout for FLIST{TUP{STR,SIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FLISTTUPSTRSIG;

typedef struct FLISTTUPSTRSTR_struct {/* layout for FLIST{TUP{STR,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FLISTTUPSTRSTR;

typedef struct FLISTT418307355_struct {/* layout for FLIST{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FLISTT418307355;

typedef struct FLISTT102367225_struct {/* layout for FLIST{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *FLISTT102367225;

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPdTPdLAYOUT_struct {/* layout for FMAP{$TP,$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FMAPdTPdLAYOUT;

typedef struct FMAPdTPCODE_FILE_struct {/* layout for FMAP{$TP,CODE_FILE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *FMAPdTPCODE_FILE;

typedef struct FMAPdT953816637_struct {/* layout for FMAP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FMAPdT953816637;

typedef struct FMAPdTPINT_struct {/* layout for FMAP{$TP,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FMAPdTPINT;

typedef struct FMAPAM339652544_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FMAPAM339652544;

typedef struct FMAPAM1357596931_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FMAPAM1357596931;

typedef struct FMAPAM1145008811_struct {/* layout for FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FMAPAM1145008811;

typedef struct FMAPAM418009930_struct {/* layout for FMAP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *FMAPAM418009930;

typedef struct FMAPAM1244483504_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *FMAPAM1244483504;

typedef struct FMAPAM1787628573_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *FMAPAM1787628573;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

typedef struct FMAPSI2103621588_struct {/* layout for FMAP{SIG,ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FMAPSI2103621588;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSI1349988702_struct {/* layout for FMAP{SIG,ARG_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FMAPSI1349988702;

typedef struct FMAPSI1121373188_struct {/* layout for FMAP{SIG,FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FMAPSI1121373188;

typedef struct FMAPSIGSTR_struct {/* layout for FMAP{SIG,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *FMAPSIGSTR;

typedef struct FMAPSTRdCONFIG_struct {/* layout for FMAP{STR,$CONFIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *FMAPSTRdCONFIG;

typedef struct FMAPSTRFSETSTR_struct {/* layout for FMAP{STR,FSET{STR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FMAPSTRFSETSTR;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FMAPSTRSIG_struct {/* layout for FMAP{STR,SIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FMAPSTRSIG;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

typedef struct FMAPTP291739594_struct {/* layout for FMAP{TP_CLASS,FSET{$TP}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FMAPTP291739594;

typedef struct FMAPTP775297600_struct {/* layout for FMAP{TP_CLASS,FSET{TP_CLASS}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *FMAPTP775297600;

typedef struct FMT_ERROR_struct {/* layout for FMT_ERROR */
 INT error1;
 STR str;
 } FMT_ERROR;
static FMT_ERROR FMT_ERROR_zero;

typedef struct FMT_ERROR_boxed_struct {
 OB_HEADER header;
 FMT_ERROR immutable_part;
 } *FMT_ERROR_boxed;

typedef struct FSETdLAYOUT_struct {/* layout for FSET{$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FSETdLAYOUT;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETAB1569698808_struct {/* layout for FSET{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FSETAB1569698808;

typedef struct FSETAM1032778315_struct {/* layout for FSET{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FSETAM1032778315;

typedef struct FSETAM1158405984_struct {/* layout for FSET{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FSETAM1158405984;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

typedef struct FSETAM1542678947_struct {/* layout for FSET{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FSETAM1542678947;

typedef struct FSETAS_FEAT_MOD_struct {/* layout for FSET{AS_FEAT_MOD} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FSETAS_FEAT_MOD;

typedef struct FSETBO1548397318_struct {/* layout for FSET{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FSETBO1548397318;

typedef struct FSETBO1147931784_struct {/* layout for FSET{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FSETBO1147931784;

typedef struct FSETBU1446477505_struct {/* layout for FSET{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FSETBU1446477505;

typedef struct FSETCLASS_LAYOUT_struct {/* layout for FSET{CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FSETCLASS_LAYOUT;

typedef struct FSETCS1561418284_struct {/* layout for FSET{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FSETCS1561418284;

typedef struct FSETEX1260947371_struct {/* layout for FSET{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FSETEX1260947371;

typedef struct FSETIM184436961_struct {/* layout for FSET{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FSETIM184436961;

typedef struct FSETSFILE_ID_struct {/* layout for FSET{SFILE_ID} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FSETSFILE_ID;

typedef struct FSETSIDE_EFFECT_struct {/* layout for FSET{SIDE_EFFECT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FSETSIDE_EFFECT;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

typedef struct FSETTP_CLASS_struct {/* layout for FSET{TP_CLASS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FSETTP_CLASS;

typedef struct FSETTUPIDENTINT_struct {/* layout for FSET{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FSETTUPIDENTINT;

typedef struct FSETTUPSIGSIG_struct {/* layout for FSET{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FSETTUPSIGSIG;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct IDENT_TBL_struct {/* layout for IDENT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *IDENT_TBL;

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

typedef struct LAYOUT_ARR_struct {/* layout for LAYOUT_ARR */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *LAYOUT_ARR;

typedef struct NAMEMAP_struct {/* layout for NAMEMAP */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *NAMEMAP;

typedef struct PARSER_struct {/* layout for PARSER */
 OB_HEADER header;
 struct CONVERT_struct *convert;
 struct FLISTSTR_struct *entered;
 struct PROG_struct *prog;
 struct SCANNER_struct *scanner;
 struct TOKEN_struct next;
 BOOL version_1_0;
 } *PARSER;

typedef struct bound1_ob_struct {
 OB_HEADER header;
 BOOL (*funcptr)(struct bound1_ob_struct *, TUPSIGINT, TUPSIGINT);
 PRINT_OB bound_arg0;
 } *bound1_ob;

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

typedef struct ROUTTU467247893_struct {
 OB_HEADER header;
 BOOL (*funcptr)(void *, TUPSIGINT, TUPSIGINT);
} *ROUTTU467247893;

#include "tags.h"

/* Globals */

extern AM_INT_CONST OPT_CO150672939;
extern AM_INT_CONST OPT_CO150710673;
extern AM_INT_CONST OPT_CO232246741;
extern AM_INT_CONST OPT_CO238309264;
extern AM_INT_CONST OPT_CO238346998;
extern ARRAYTP_CLASS TP_BUI1236844942;
extern ARRAYTP_CLASS TP_BUI1236844961;
extern ARRAYTP_CLASS TP_BUI1517259338;
extern BOOL AM_CURSOR_debug;
extern BOOL FSETAB1854241901;
extern BOOL FSETAB582100884;
extern BOOL FSETAM1208812133;
extern BOOL FSETAM1441852029;
extern BOOL FSETAM1817646066;
extern BOOL FSETAM1935294117;
extern BOOL FSETAM40978445;
extern BOOL FSETAM501048668;
extern BOOL FSETAM649812378;
extern BOOL FSETAM994490756;
extern BOOL FSETAS1812528589;
extern BOOL FSETAS46095922;
extern BOOL FSETBO1572491060;
extern BOOL FSETBO2074935273;
extern BOOL FSETBO216310762;
extern BOOL FSETBO286133451;
extern BOOL FSETBU1017592727;
extern BOOL FSETBU841031784;
extern BOOL FSETCL2058817423;
extern BOOL FSETCL377525362;
extern BOOL FSETCS1600255829;
extern BOOL FSETCS836086956;
extern BOOL FSETEX1520806408;
extern BOOL FSETEX915536377;
extern BOOL FSETIM1693735320;
extern BOOL FSETIM742607465;
extern BOOL FSETSF1336677965;
extern BOOL FSETSF521946546;
extern BOOL FSETSI1125125409;
extern BOOL FSETSI1311217376;
extern BOOL FSETSI2024459664;
extern BOOL FSETSI411883121;
extern BOOL FSETST1011828476;
extern BOOL FSETST846796035;
extern BOOL FSETTP138222400;
extern BOOL FSETTP1720402111;
extern BOOL FSETTU1642694154;
extern BOOL FSETTU1862000339;
extern BOOL FSETTU215930357;
extern BOOL FSETTU574342446;
extern BOOL FSETdL1534870640;
extern BOOL FSETdL901472145;
extern BOOL FSETdT1052485343;
extern BOOL FSETdT806139168;
extern BOOL SFILE_ID_newline;
extern BOOL TRANS_1975351488;
extern BOOL TRANS_2136297923;
extern BOOL TRANS_89465360;
extern CALL_T561792367 CALL_T714618297;
extern CALL_TP_ARRAY CALL_T1969158839;
extern CALL_TP_CREATE CALL_T1560699295;
extern CALL_TP_VOID CALL_T2135634286;
extern CGEN CODE_FILE_cgen;
extern CGEN LAYOUT_TBL_cgen;
extern CHAR SFILE_446927505;
extern CS_OPTIONS OPT_CO1299251581;
extern CS_OPTIONS SE_CON1373267262;
extern FLISTCODE_FILE CODE_FILE_all;
extern FLISTCODE_FILE CODE_FILE_todo;
extern FLISTINT SFILE_ID_lines;
extern FLISTSIG PRINT_OB_funcs;
extern FLISTSTR CODE_F1806827531;
extern FLISTSTR SFILE_ID_files;
extern FLISTTUPIDENTINT FIND_G1866376849;
extern FLISTTUPIDENTINT FIND_T1407637984;
extern FLISTdLAYOUT CODE_F19188927;
extern FLT FLT_log2_e;
extern FLTD MS_RAN1117060533;
extern FLTD MS_RAN393309279;
extern FLTD MS_RAN393309507;
extern FLTD TIME_t854034019;
extern FMAPAM1357596931 BOUND_2051717332;
extern FMAPAM339652544 BOUND_982289559;
extern FMAPSI1121373188 FRAME_1643038396;
extern FMAPSI1349988702 ARG_LA2092120054;
extern FMAPSI2103621588 ABSTRA854284101;
extern FMAPSI518162669 CODE_F508943357;
extern FMAPSTRSIG SIG_sigs;
extern FMAPSTRSTR CODE_F1403139677;
extern FMAPdTPdLAYOUT LAYOUT1191461457;
extern FSETAB1569698808 ABSTRA8148400;
extern FSETBO1147931784 BOUND_2075179152;
extern FSETBO1548397318 BOUND_210358778;
extern FSETBU1446477505 BUILTI898649695;
extern FSETCLASS_LAYOUT CLASS_368803185;
extern FSETEX1260947371 EXTERN1555805875;
extern FSETIM184436961 IMMUTA489824193;
extern FSETSTR FILE_M683825354;
extern FSETTUPIDENTINT IFC_abs_cur;
extern FSTR FAST_I1322544971;
extern FSTR INTI_buf;
extern FSTR SFILE_ID_source;
extern FSTR STR_buf;
extern IDENT IDENT_1061837561;
extern IDENT IDENT_1067669005;
extern IDENT IDENT_1129727818;
extern IDENT IDENT_1137952589;
extern IDENT IDENT_1140439567;
extern IDENT IDENT_12128454;
extern IDENT IDENT_1280657180;
extern IDENT IDENT_1295303664;
extern IDENT IDENT_1418293603;
extern IDENT IDENT_1485568624;
extern IDENT IDENT_1616879092;
extern IDENT IDENT_1644121920;
extern IDENT IDENT_1666445894;
extern IDENT IDENT_1728630987;
extern IDENT IDENT_1834849979;
extern IDENT IDENT_1837767448;
extern IDENT IDENT_1891587148;
extern IDENT IDENT_190278447;
extern IDENT IDENT_1909070998;
extern IDENT IDENT_1924458840;
extern IDENT IDENT_196328811;
extern IDENT IDENT_1974148927;
extern IDENT IDENT_2063376192;
extern IDENT IDENT_2085547498;
extern IDENT IDENT_2108657069;
extern IDENT IDENT_2110206590;
extern IDENT IDENT_228725606;
extern IDENT IDENT_265373817;
extern IDENT IDENT_291726180;
extern IDENT IDENT_327685657;
extern IDENT IDENT_386785441;
extern IDENT IDENT_421158771;
extern IDENT IDENT_428696930;
extern IDENT IDENT_458929583;
extern IDENT IDENT_461800478;
extern IDENT IDENT_475003333;
extern IDENT IDENT_51715464;
extern IDENT IDENT_5303930;
extern IDENT IDENT_531080196;
extern IDENT IDENT_58546263;
extern IDENT IDENT_604723504;
extern IDENT IDENT_667688077;
extern IDENT IDENT_673807638;
extern IDENT IDENT_685437142;
extern IDENT IDENT_692534448;
extern IDENT IDENT_853591262;
extern IDENT IDENT_857412936;
extern IDENT IDENT_869381517;
extern IDENT IDENT_959790938;
extern IDENT IDENT_985337352;
extern IDENT TP_BUI1031701849;
extern IDENT TP_BUI1247087282;
extern IDENT TP_BUI1322220574;
extern IDENT TP_BUI1336772878;
extern IDENT TP_BUI1390125097;
extern IDENT TP_BUI1459211256;
extern IDENT TP_BUI1516264549;
extern IDENT TP_BUI1586744698;
extern IDENT TP_BUI1597109090;
extern IDENT TP_BUI1626092738;
extern IDENT TP_BUI1636890711;
extern IDENT TP_BUI1825617811;
extern IDENT TP_BUI1845282323;
extern IDENT TP_BUI1928257270;
extern IDENT TP_BUI2068715177;
extern IDENT TP_BUI2144653755;
extern IDENT TP_BUI222951651;
extern IDENT TP_BUI238610581;
extern IDENT TP_BUI243806180;
extern IDENT TP_BUI345188431;
extern IDENT TP_BUI354250336;
extern IDENT TP_BUI370100142;
extern IDENT TP_BUI388121286;
extern IDENT TP_BUI391975957;
extern IDENT TP_BUI436262224;
extern IDENT TP_BUI459983896;
extern IDENT TP_BUI482029710;
extern IDENT TP_BUI505750453;
extern IDENT TP_BUI567029074;
extern IDENT TP_BUI57672398;
extern IDENT TP_BUI591808532;
extern IDENT TP_BUI620054994;
extern IDENT TP_BUI640672586;
extern IDENT TP_BUI659858568;
extern IDENT TP_BUI66959111;
extern IDENT TP_BUI726964134;
extern IDENT TP_BUI732557391;
extern IDENT TP_BUI757402305;
extern IDENT TP_BUI764451214;
extern IDENT TP_BUI819938711;
extern IDENT TP_BUI881541400;
extern IDENT TP_BUI895836687;
extern IDENT TP_BUI979458576;
extern IDENT_TBL IDENT_ident_tbl;
extern INOUT_MODE MODES_inout_mode;
extern INT ARRAYA876812706;
extern INT ARRAYF881388824;
extern INT ARRAYI56037128;
extern INT ARRAYS852413528;
extern INT ARRAYd432394813;
extern INT AS_ARG1300937332;
extern INT AS_ARG1872943786;
extern INT AS_ARG551300469;
extern INT AS_ARG685418392;
extern INT AS_CLA1666707132;
extern INT AS_CLA2085573152;
extern INT AS_CLA86009053;
extern INT AS_CLASS_DEF_abs;
extern INT AS_CLASS_DEF_imm;
extern INT AS_CLASS_DEF_ref;
extern INT AS_CLASS_DEF_spr;
extern INT AS_FLT1215265092;
extern INT AS_FLT20428321;
extern INT AS_FLT388140360;
extern INT AS_FLT388140455;
extern INT AS_FLT388140740;
extern INT AS_OUT_indent;
extern INT AS_TYP1851973889;
extern INT AS_TYP1853679690;
extern INT AS_TYPE_SPEC_it;
extern INT AS_TYPE_SPEC_ord;
extern INT AS_TYPE_SPEC_rt;
extern INT CGEN_m1427112264;
extern INT CODE_F1127805475;
extern INT ELT_TB2133420318;
extern INT FILE_M852907408;
extern INT FMAPAM1649190656;
extern INT FMAPAM29230850;
extern INT FMAPAM441967257;
extern INT FMAPAM476780631;
extern INT FMAPAM498940295;
extern INT FMAPAM673663981;
extern INT FMAPID1835582514;
extern INT FMAPID1853260260;
extern INT FMAPSI1253630331;
extern INT FMAPSI1801446319;
extern INT FMAPSI22065355;
extern INT FMAPSI362068854;
extern INT FMAPST1192706365;
extern INT FMAPST1449505507;
extern INT FMAPST2041121537;
extern INT FMAPST47057045;
extern INT FMAPST641873122;
extern INT FMAPTP642805238;
extern INT FMAPTP676284949;
extern INT FMAPdO802731536;
extern INT FMAPdT1555535134;
extern INT FMAPdT2042454662;
extern INT FMAPdT290960897;
extern INT FMAPdT861897287;
extern INT FSETAB246578603;
extern INT FSETAB478854058;
extern INT FSETAM154189325;
extern INT FSETAM1929715922;
extern INT FSETAM2014782744;
extern INT FSETAM295466615;
extern INT FSETAM34126943;
extern INT FSETAM584174453;
extern INT FSETAM687490432;
extern INT FSETAM744778340;
extern INT FSETAS1287498417;
extern INT FSETAS1969137460;
extern INT FSETBO1040487207;
extern INT FSETBO1172349319;
extern INT FSETBO157482060;
extern INT FSETBO717550434;
extern INT FSETBU1565274135;
extern INT FSETBU864102134;
extern INT FSETCL497069731;
extern INT FSETCL786981258;
extern INT FSETCS1883401206;
extern INT FSETCS2145101147;
extern INT FSETEX1632590957;
extern INT FSETEX2023113290;
extern INT FSETIM2015417903;
extern INT FSETIM924207918;
extern INT FSETSF1238115389;
extern INT FSETSF1603235078;
extern INT FSETSI1189203070;
extern INT FSETSI1350248406;
extern INT FSETSI1658388805;
extern INT FSETSI232994243;
extern INT FSETST1746314450;
extern INT FSETST854048553;
extern INT FSETTP1014013453;
extern INT FSETTP752200044;
extern INT FSETTU1908122889;
extern INT FSETTU561674193;
extern INT FSETTU697583666;
extern INT FSETTU939302060;
extern INT FSETdL1013454714;
extern INT FSETdL344753731;
extern INT FSETdT1655792354;
extern INT FSETdT2053737937;
extern INT IDENT_105403148;
extern INT IDENT_tmp_count;
extern INT INLINE1173790597;
extern INT INLINE1181209094;
extern INT INLINE1232309177;
extern INT INLINE1258550073;
extern INT INLINE1301544351;
extern INT INLINE1306635001;
extern INT INLINE1401026874;
extern INT INLINE142050909;
extern INT INLINE1484868694;
extern INT INLINE1516217386;
extern INT INLINE1752510002;
extern INT INLINE1950426034;
extern INT INLINE2014794524;
extern INT INLINE2125477179;
extern INT INLINE29411189;
extern INT INLINE494873411;
extern INT INLINE520918393;
extern INT INLINE545315961;
extern INT INLINE598259372;
extern INT INLINE957944682;
extern INT INTI_B;
extern INT INTI_D;
extern INT INTI_log10D;
extern INT INTI_log2B;
extern INT INT_asize;
extern INT LAYOUT1889414835;
extern INT LAYOUT558382138;
extern INT NAMEMA2064441237;
extern INT PARSER1019557737;
extern INT PARSER1073106908;
extern INT PARSER11100551;
extern INT PARSER1207020061;
extern INT PARSER1209676627;
extern INT PARSER1229493770;
extern INT PARSER1234333718;
extern INT PARSER1281839191;
extern INT PARSER1296410135;
extern INT PARSER1337387114;
extern INT PARSER1338823514;
extern INT PARSER1344673316;
extern INT PARSER1363330612;
extern INT PARSER1374810694;
extern INT PARSER1388509464;
extern INT PARSER1701976116;
extern INT PARSER1758133485;
extern INT PARSER1789316521;
extern INT PARSER1916422053;
extern INT PARSER1963707920;
extern INT PARSER1998725038;
extern INT PARSER2001381604;
extern INT PARSER2011952041;
extern INT PARSER2033525488;
extern INT PARSER265941484;
extern INT PARSER404310286;
extern INT PARSER415742890;
extern INT PARSER460241977;
extern INT PARSER543515898;
extern INT PARSER550831344;
extern INT PARSER658540566;
extern INT PARSER689908022;
extern INT PARSER729653115;
extern INT PARSER836044841;
extern INT PARSER840587154;
extern INT PARSER855619786;
extern INT PARSER886437717;
extern INT PARSER978635316;
extern INT PARSER978809429;
extern INT PARSER985900141;
extern INT PARSER998808835;
extern INT PARSER_ITER_tok;
extern INT PARSER_ROUT_tok;
extern INT PARSER_SAME_tok;
extern INT PARSER_and_tok;
extern INT PARSER_any_tok;
extern INT PARSER_at_tok;
extern INT PARSER_attr_tok;
extern INT PARSER_bang_tok;
extern INT PARSER_bind_tok;
extern INT PARSER_break_tok;
extern INT PARSER_case_tok;
extern INT PARSER_class_tok;
extern INT PARSER_colon_tok;
extern INT PARSER_comma_tok;
extern INT PARSER_const_tok;
extern INT PARSER_do_tok;
extern INT PARSER_dot_tok;
extern INT PARSER_else_tok;
extern INT PARSER_elsif_tok;
extern INT PARSER_end_tok;
extern INT PARSER_eof_tok;
extern INT PARSER_false_tok;
extern INT PARSER_far_tok;
extern INT PARSER_fork_tok;
extern INT PARSER_guard_tok;
extern INT PARSER_here_tok;
extern INT PARSER_ident_tok;
extern INT PARSER_if_tok;
extern INT PARSER_inout_tok;
extern INT PARSER_is_eq_tok;
extern INT PARSER_is_gt_tok;
extern INT PARSER_is_lt_tok;
extern INT PARSER_is_tok;
extern INT PARSER_lchar_tok;
extern INT PARSER_lflt_tok;
extern INT PARSER_lint_tok;
extern INT PARSER_lock_tok;
extern INT PARSER_loop_tok;
extern INT PARSER_lstr_tok;
extern INT PARSER_minus_tok;
extern INT PARSER_mod_tok;
extern INT PARSER_near_tok;
extern INT PARSER_new_tok;
extern INT PARSER_not_tok;
extern INT PARSER_null_tok;
extern INT PARSER_once_tok;
extern INT PARSER_or_tok;
extern INT PARSER_out_tok;
extern INT PARSER_par_tok;
extern INT PARSER_plus_tok;
extern INT PARSER_post_tok;
extern INT PARSER_pow_tok;
extern INT PARSER_pre_tok;
extern INT PARSER_quit_tok;
extern INT PARSER_raise_tok;
extern INT PARSER_self_tok;
extern INT PARSER_semi_tok;
extern INT PARSER_sharp_tok;
extern INT PARSER_stub_tok;
extern INT PARSER_sync_tok;
extern INT PARSER_then_tok;
extern INT PARSER_times_tok;
extern INT PARSER_true_tok;
extern INT PARSER_type_tok;
extern INT PARSER_under_tok;
extern INT PARSER_until_tok;
extern INT PARSER_value_tok;
extern INT PARSER_vbar_tok;
extern INT PARSER_void_tok;
extern INT PARSER_when_tok;
extern INT PARSER_where_tok;
extern INT PARSER_while_tok;
extern INT PARSER_with_tok;
extern INT PARSER_yield_tok;
extern INT PRINT_1541629256;
extern INT PROG_A1730220286;
extern INT SCANNE1010372543;
extern INT SCANNE1014449292;
extern INT SCANNE1021322009;
extern INT SCANNE105624607;
extern INT SCANNE1090219034;
extern INT SCANNE1099722568;
extern INT SCANNE114307641;
extern INT SCANNE116326663;
extern INT SCANNE1180808401;
extern INT SCANNE1233467974;
extern INT SCANNE1252648769;
extern INT SCANNE1277304122;
extern INT SCANNE1284590324;
extern INT SCANNE1290586306;
extern INT SCANNE1388342024;
extern INT SCANNE1391319271;
extern INT SCANNE1412300811;
extern INT SCANNE1414957377;
extern INT SCANNE1421647682;
extern INT SCANNE149137033;
extern INT SCANNE1499824177;
extern INT SCANNE1514574730;
extern INT SCANNE1553373348;
extern INT SCANNE1574122250;
extern INT SCANNE1717311299;
extern INT SCANNE1733136762;
extern INT SCANNE1785932597;
extern INT SCANNE1874840845;
extern INT SCANNE1901892399;
extern INT SCANNE1938931342;
extern INT SCANNE202030842;
extern INT SCANNE2045360664;
extern INT SCANNE2078461540;
extern INT SCANNE2093995558;
extern INT SCANNE2137779101;
extern INT SCANNE2139920629;
extern INT SCANNE269030544;
extern INT SCANNE274598975;
extern INT SCANNE290718062;
extern INT SCANNE304035056;
extern INT SCANNE334166344;
extern INT SCANNE392475110;
extern INT SCANNE401863129;
extern INT SCANNE438656140;
extern INT SCANNE440197020;
extern INT SCANNE463806109;
extern INT SCANNE470944348;
extern INT SCANNE560979044;
extern INT SCANNE589883380;
extern INT SCANNE620595834;
extern INT SCANNE623252400;
extern INT SCANNE637092534;
extern INT SCANNE661829813;
extern INT SCANNE687089717;
extern INT SCANNE694354542;
extern INT SCANNE738612593;
extern INT SCANNE754612661;
extern INT SCANNE766559877;
extern INT SCANNE801235924;
extern INT SCANNE879190010;
extern INT SCANNE916727723;
extern INT SCANNE928026230;
extern INT SCANNE943336743;
extern INT SCANNE951940802;
extern INT SCANNE983081836;
extern INT SCANNER_ITER_tok;
extern INT SCANNER_ROUT_tok;
extern INT SCANNER_SAME_tok;
extern INT SCANNER_and_tok;
extern INT SCANNER_any_tok;
extern INT SCANNER_at_tok;
extern INT SCANNER_attr_tok;
extern INT SCANNER_bang_tok;
extern INT SCANNER_bind_tok;
extern INT SCANNER_case_tok;
extern INT SCANNER_do_tok;
extern INT SCANNER_dot_tok;
extern INT SCANNER_else_tok;
extern INT SCANNER_end_tok;
extern INT SCANNER_eof_tok;
extern INT SCANNER_far_tok;
extern INT SCANNER_fork_tok;
extern INT SCANNER_here_tok;
extern INT SCANNER_if_tok;
extern INT SCANNER_is_tok;
extern INT SCANNER_lflt_tok;
extern INT SCANNER_lint_tok;
extern INT SCANNER_lock_tok;
extern INT SCANNER_loop_tok;
extern INT SCANNER_lstr_tok;
extern INT SCANNER_mod_tok;
extern INT SCANNER_near_tok;
extern INT SCANNER_new_tok;
extern INT SCANNER_not_tok;
extern INT SCANNER_null_tok;
extern INT SCANNER_once_tok;
extern INT SCANNER_or_tok;
extern INT SCANNER_out_tok;
extern INT SCANNER_par_tok;
extern INT SCANNER_plus_tok;
extern INT SCANNER_post_tok;
extern INT SCANNER_pow_tok;
extern INT SCANNER_pre_tok;
extern INT SCANNER_quit_tok;
extern INT SCANNER_self_tok;
extern INT SCANNER_semi_tok;
extern INT SCANNER_stub_tok;
extern INT SCANNER_sync_tok;
extern INT SCANNER_then_tok;
extern INT SCANNER_true_tok;
extern INT SCANNER_type_tok;
extern INT SCANNER_vbar_tok;
extern INT SCANNER_void_tok;
extern INT SCANNER_when_tok;
extern INT SCANNER_with_tok;
extern INT SFILE_1180160528;
extern INT SFILE_1796395955;
extern INT SFILE_ID_B;
extern INT SFILE_ID_column;
extern INT SFILE_ID_line;
extern INT SFILE_ID_pos;
extern INT SIG_TB1720612202;
extern INT STR_CU1107107574;
extern INT STR_CU1114535871;
extern INT STR_CU1474621917;
extern INT STR_CU1719265904;
extern INT STR_CU1921064154;
extern INT STR_CU254621724;
extern INT STR_CU610585082;
extern INT TOKEN_1044792413;
extern INT TOKEN_12855958;
extern INT TOKEN_1310153686;
extern INT TOKEN_1495550102;
extern INT TOKEN_1684560144;
extern INT TOKEN_17213944;
extern INT TOKEN_181187864;
extern INT TOKEN_1901305998;
extern INT TOKEN_1983637546;
extern INT TOKEN_2055015794;
extern INT TOKEN_259588423;
extern INT TOKEN_284824330;
extern INT TOKEN_348462331;
extern INT TOKEN_387428395;
extern INT TOKEN_402103953;
extern INT TOKEN_77733409;
extern INT TOKEN_791010569;
extern INT TOKEN_827305291;
extern INT TOKEN_905236088;
extern INT TOKEN_942285672;
extern INT TOKEN_970494340;
extern INT TOKEN_991243242;
extern INT TOKEN_F_ROUT_tok;
extern INT TOKEN_ITER_tok;
extern INT TOKEN_ROUT_tok;
extern INT TOKEN_SAME_tok;
extern INT TOKEN_and_tok;
extern INT TOKEN_any_tok;
extern INT TOKEN_assert_tok;
extern INT TOKEN_assign_tok;
extern INT TOKEN_at_tok;
extern INT TOKEN_attach_tok;
extern INT TOKEN_attr_tok;
extern INT TOKEN_bang_tok;
extern INT TOKEN_bind_tok;
extern INT TOKEN_break_tok;
extern INT TOKEN_case_tok;
extern INT TOKEN_class_tok;
extern INT TOKEN_cohort_tok;
extern INT TOKEN_colon_tok;
extern INT TOKEN_comma_tok;
extern INT TOKEN_const_tok;
extern INT TOKEN_dcolon_tok;
extern INT TOKEN_do_tok;
extern INT TOKEN_dot_tok;
extern INT TOKEN_else_tok;
extern INT TOKEN_elsif_tok;
extern INT TOKEN_end_tok;
extern INT TOKEN_eof_tok;
extern INT TOKEN_false_tok;
extern INT TOKEN_far_tok;
extern INT TOKEN_fork_tok;
extern INT TOKEN_global_tok;
extern INT TOKEN_guard_tok;
extern INT TOKEN_here_tok;
extern INT TOKEN_ident_tok;
extern INT TOKEN_if_tok;
extern INT TOKEN_inout_tok;
extern INT TOKEN_is_eq_tok;
extern INT TOKEN_is_geq_tok;
extern INT TOKEN_is_gt_tok;
extern INT TOKEN_is_leq_tok;
extern INT TOKEN_is_lt_tok;
extern INT TOKEN_is_neq_tok;
extern INT TOKEN_is_tok;
extern INT TOKEN_lbrace_tok;
extern INT TOKEN_lchar_tok;
extern INT TOKEN_lflt_tok;
extern INT TOKEN_lint_tok;
extern INT TOKEN_lock_tok;
extern INT TOKEN_loop_tok;
extern INT TOKEN_lparen_tok;
extern INT TOKEN_lstr_tok;
extern INT TOKEN_minus_tok;
extern INT TOKEN_mod_tok;
extern INT TOKEN_near_tok;
extern INT TOKEN_new_tok;
extern INT TOKEN_not_tok;
extern INT TOKEN_null_tok;
extern INT TOKEN_once_tok;
extern INT TOKEN_or_tok;
extern INT TOKEN_out_tok;
extern INT TOKEN_par_tok;
extern INT TOKEN_plus_tok;
extern INT TOKEN_post_tok;
extern INT TOKEN_pow_tok;
extern INT TOKEN_pre_tok;
extern INT TOKEN_quit_tok;
extern INT TOKEN_raise_tok;
extern INT TOKEN_rbrace_tok;
extern INT TOKEN_return_tok;
extern INT TOKEN_rparen_tok;
extern INT TOKEN_self_tok;
extern INT TOKEN_semi_tok;
extern INT TOKEN_shared_tok;
extern INT TOKEN_sharp_tok;
extern INT TOKEN_spread_tok;
extern INT TOKEN_stub_tok;
extern INT TOKEN_sync_tok;
extern INT TOKEN_then_tok;
extern INT TOKEN_times_tok;
extern INT TOKEN_true_tok;
extern INT TOKEN_type_tok;
extern INT TOKEN_under_tok;
extern INT TOKEN_unlock_tok;
extern INT TOKEN_until_tok;
extern INT TOKEN_value_tok;
extern INT TOKEN_vbar_tok;
extern INT TOKEN_void_tok;
extern INT TOKEN_when_tok;
extern INT TOKEN_where_tok;
extern INT TOKEN_while_tok;
extern INT TOKEN_with_tok;
extern INT TOKEN_yield_tok;
extern INT TP_ARR899550402;
extern INT TP_CLA1631459432;
extern INT TP_GEN180842239;
extern INT TP_ITE34679094;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_abs_tp;
extern INT TP_KIND_ext_c_tp;
extern INT TP_KIND_iter_tp;
extern INT TP_KIND_part_tp;
extern INT TP_KIND_ref_tp;
extern INT TP_KIND_rout_tp;
extern INT TP_KIND_spr_tp;
extern INT TP_KIND_val_tp;
extern INT TP_ROU558963694;
extern INT TRANS_1924362320;
extern INT TRANS_194341863;
extern INT TRANS_att_code;
extern INT TRANS_frk_code;
extern INT TRANS_par_code;
extern INT WEIGH_1480681287;
extern INT WEIGH_743880746;
extern INT WEIGH_803917570;
extern INT WEIGH_879105801;
extern IN_MODE MODES_in_mode;
extern MANGLE PRINT_OB_mangler;
extern MS_RANDOM_GEN RND_default_gen;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;
extern PROG AS_OUT_prog;
extern PROG CODE_FILE_prog;
extern PROG LAYOUT_TBL_prog;
extern PROG OPT_CO233462019;
extern PROG SE_CONTEXT_prog;
extern STR CODE_FILE_dir;
extern TP_CLASS TP_BUI102735229;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI107670638;
extern TP_CLASS TP_BUI1116976640;
extern TP_CLASS TP_BUI1194559023;
extern TP_CLASS TP_BUI1248326412;
extern TP_CLASS TP_BUI1292722345;
extern TP_CLASS TP_BUI1296146890;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUI1370948386;
extern TP_CLASS TP_BUI139344778;
extern TP_CLASS TP_BUI1606257075;
extern TP_CLASS TP_BUI1609790832;
extern TP_CLASS TP_BUI1643601086;
extern TP_CLASS TP_BUI1686668470;
extern TP_CLASS TP_BUI1696960301;
extern TP_CLASS TP_BUI1716944501;
extern TP_CLASS TP_BUI1768246832;
extern TP_CLASS TP_BUI1769472218;
extern TP_CLASS TP_BUI1798283841;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI1813155122;
extern TP_CLASS TP_BUI1825364383;
extern TP_CLASS TP_BUI191368816;
extern TP_CLASS TP_BUI1959866815;
extern TP_CLASS TP_BUI2025364786;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI2131222993;
extern TP_CLASS TP_BUI231294523;
extern TP_CLASS TP_BUI232519909;
extern TP_CLASS TP_BUI233684457;
extern TP_CLASS TP_BUI274781170;
extern TP_CLASS TP_BUI374366290;
extern TP_CLASS TP_BUI389447236;
extern TP_CLASS TP_BUI403444172;
extern TP_CLASS TP_BUI444762713;
extern TP_CLASS TP_BUI468587458;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUI765623063;
extern TP_CLASS TP_BUI779482534;
extern TP_CLASS TP_BUI845074864;
extern TP_CLASS TP_BUI997290216;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_c_int;
extern TP_CLASS TP_BUILTIN_c_ptr;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_flt;
extern TP_CLASS TP_BUILTIN_fltd;
extern TP_CLASS TP_BUILTIN_fltdx;
extern TP_CLASS TP_BUILTIN_flti;
extern TP_CLASS TP_BUILTIN_fltx;
extern TP_CLASS TP_BUILTIN_fstr;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_inti;
extern TP_CLASS TP_BUILTIN_str;
extern TP_CLASS TP_BUILTIN_sys;
extern TP_CLASS TP_BUILTIN_zone;
extern TP_ROUT TP_BUILTIN_rout;
extern dTP OPT_CO146016663;
extern dTP OPT_CO146094031;
extern dTP OPT_CO1800765742;

/* Function declarations */

BOOL PRINT_1038801195(PRINT_OB, TUPSIGINT, TUPSIGINT);
INOUT_MODE INOUT_2024898585(INOUT_MODE);
INT INT_pow_INTrINT(INT, INT);
INT sather_main(CS, ARRAYSTR);
IN_MODE IN_MOD403789248(IN_MODE);
ONCE_MODE ONCE_M704734922(ONCE_MODE);
OUT_MODE OUT_MO1658580595(OUT_MODE);

/* Bound rout stubs decls */

BOOL bound1(bound1_ob, TUPSIGINT, TUPSIGINT);

/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <math.h>

/* Code */



int main(int argc, char *argv[]) {
 FLTD L11_;
 FLTD L2;
 FLT L31_;
 OB L4;
 OB L5;
 INT L6;
 OB L7;
 INT L8;

 CS main_ob;
 ARRAYSTR main_args;
 int i,j,length;
 STR s;
 int res=0;
 sather_prog_name=argv[0];
 rt_start(0,argc,argv);
 IDENT_959790938 = IDENT_zero;
 IDENT_428696930 = IDENT_zero;
 IDENT_1924458840 = IDENT_zero;
 IDENT_853591262 = IDENT_zero;
 IDENT_1067669005 = IDENT_zero;
 IDENT_12128454 = IDENT_zero;
 IDENT_1644121920 = IDENT_zero;
 IDENT_685437142 = IDENT_zero;
 IDENT_869381517 = IDENT_zero;
 IDENT_291726180 = IDENT_zero;
 IDENT_1834849979 = IDENT_zero;
 IDENT_1295303664 = IDENT_zero;
 IDENT_692534448 = IDENT_zero;
 IDENT_51715464 = IDENT_zero;
 IDENT_58546263 = IDENT_zero;
 IDENT_1418293603 = IDENT_zero;
 IDENT_1061837561 = IDENT_zero;
 IDENT_5303930 = IDENT_zero;
 IDENT_265373817 = IDENT_zero;
 IDENT_1280657180 = IDENT_zero;
 IDENT_386785441 = IDENT_zero;
 IDENT_2085547498 = IDENT_zero;
 IDENT_1129727818 = IDENT_zero;
 IDENT_1616879092 = IDENT_zero;
 IDENT_228725606 = IDENT_zero;
 IDENT_1974148927 = IDENT_zero;
 IDENT_475003333 = IDENT_zero;
 IDENT_421158771 = IDENT_zero;
 IDENT_2108657069 = IDENT_zero;
 IDENT_1837767448 = IDENT_zero;
 IDENT_1485568624 = IDENT_zero;
 IDENT_1666445894 = IDENT_zero;
 IDENT_1140439567 = IDENT_zero;
 IDENT_667688077 = IDENT_zero;
 IDENT_2110206590 = IDENT_zero;
 IDENT_327685657 = IDENT_zero;
 IDENT_458929583 = IDENT_zero;
 IDENT_190278447 = IDENT_zero;
 IDENT_1909070998 = IDENT_zero;
 IDENT_985337352 = IDENT_zero;
 IDENT_2063376192 = IDENT_zero;
 IDENT_1891587148 = IDENT_zero;
 IDENT_857412936 = IDENT_zero;
 IDENT_1137952589 = IDENT_zero;
 IDENT_196328811 = IDENT_zero;
 IDENT_1728630987 = IDENT_zero;
 IDENT_461800478 = IDENT_zero;
 IDENT_604723504 = IDENT_zero;
 IDENT_673807638 = IDENT_zero;
 IDENT_531080196 = IDENT_zero;
 MS_RAN393309507 = 1.6807e4;
 INTI_D = INT_pow_INTrINT(10, INTI_log10D);
 INTI_B = INT_pow_INTrINT(2, INTI_log2B);
 TP_BUI2068715177 = IDENT_zero;
 TP_BUI482029710 = IDENT_zero;
 TP_BUI238610581 = IDENT_zero;
 TP_BUI243806180 = IDENT_zero;
 TP_BUI640672586 = IDENT_zero;
 TP_BUI66959111 = IDENT_zero;
 TP_BUI881541400 = IDENT_zero;
 TP_BUI620054994 = IDENT_zero;
 TP_BUI567029074 = IDENT_zero;
 TP_BUI1390125097 = IDENT_zero;
 TP_BUI436262224 = IDENT_zero;
 TP_BUI354250336 = IDENT_zero;
 TP_BUI726964134 = IDENT_zero;
 TP_BUI764451214 = IDENT_zero;
 TP_BUI388121286 = IDENT_zero;
 TP_BUI505750453 = IDENT_zero;
 TP_BUI1636890711 = IDENT_zero;
 TP_BUI1322220574 = IDENT_zero;
 TP_BUI1597109090 = IDENT_zero;
 TP_BUI1626092738 = IDENT_zero;
 TP_BUI1459211256 = IDENT_zero;
 TP_BUI345188431 = IDENT_zero;
 TP_BUI57672398 = IDENT_zero;
 TP_BUI591808532 = IDENT_zero;
 TP_BUI659858568 = IDENT_zero;
 TP_BUI1586744698 = IDENT_zero;
 TP_BUI222951651 = IDENT_zero;
 TP_BUI1031701849 = IDENT_zero;
 TP_BUI1845282323 = IDENT_zero;
 TP_BUI1928257270 = IDENT_zero;
 TP_BUI2144653755 = IDENT_zero;
 TP_BUI391975957 = IDENT_zero;
 TP_BUI819938711 = IDENT_zero;
 TP_BUI732557391 = IDENT_zero;
 TP_BUI979458576 = IDENT_zero;
 TP_BUI895836687 = IDENT_zero;
 TP_BUI1516264549 = IDENT_zero;
 TP_BUI757402305 = IDENT_zero;
 TP_BUI1825617811 = IDENT_zero;
 TP_BUI1336772878 = IDENT_zero;
 TP_BUI370100142 = IDENT_zero;
 TP_BUI1247087282 = IDENT_zero;
 TP_BUI459983896 = IDENT_zero;
 MODES_in_mode = IN_MOD403789248(((IN_MODE) NULL));
 MODES_out_mode = OUT_MO1658580595(((OUT_MODE) NULL));
 MODES_inout_mode = INOUT_2024898585(((INOUT_MODE) NULL));
 MODES_once_mode = ONCE_M704734922(((ONCE_MODE) NULL));
 L11_=M_LOG2E; 
 L2 = L11_;
 L31_=(FLT)L2; 
 FLT_log2_e = L31_;
 L4=ZALLOC_LEAF(sizeof(struct CS_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 memset(L4,0,sizeof(struct CS_struct));
 ((OB)L4)->header.tag=CS_tag;
 main_ob = ((CS) L4);
 L6=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(argc)*sizeof(STR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 main_args = ((ARRAYSTR) L5);
 main_args->asize = argc;
 for (i=0;i<argc;i++) {
  for (length=0; argv[i][length]!=0; length++);
 L8=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(length)*sizeof(CHAR);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

  s = ((STR) L7);
  s->asize = length;
  for (j=0;j<length;j++) s->arr_part[j] = argv[i][j];
  main_args->arr_part[i] = s;
 }
 PROTECT_BEGIN
  res = sather_main(main_ob,main_args);
  rt_stop();
 PROTECT_WHEN
  if(TAG(EXCEPTION)==STR_tag) {
   fprintf(stderr,"Uncaught STR exception: %s\n",((STR)EXCEPTION)->arr_part);
  } else fprintf(stderr,"Uncaught exception of type %s\n",gen_SYS_str_for_tp(TAG(EXCEPTION))->arr_part);
  abort();
 PROTECT_END
 return res;
}

BOOL bound1(bound1_ob ob, TUPSIGINT unbound_arg0, TUPSIGINT unbound_arg1) {

 return PRINT_1038801195(ob->bound_arg0, unbound_arg0, unbound_arg1);
}


STR gen_SYS_str_for_tp(INT i) {

 extern STR ABSTRA1514981551;
 extern STR ABSTRACT_LAYOUT1;
 extern STR AM_ANY_EXPR1;
 extern STR AM_ARRAY_EXPR1;
 extern STR AM_ARR_CONST1;
 extern STR AM_ASSERT_STMT1;
 extern STR AM_ASSIGN_STMT1;
 extern STR AM_ATTACH_STMT1;
 extern STR AM_ATTR_EXPR1;
 extern STR AM_AT_EXPR1;
 extern STR AM_BND11248771631;
 extern STR AM_BND3672117691;
 extern STR AM_BND2603013291;
 extern STR AM_BOOL_CONST1;
 extern STR AM_BREAK_STMT1;
 extern STR AM_CALL_ARG1;
 extern STR AM_CASE_STMT1;
 extern STR AM_CHAR_CONST1;
 extern STR AM_CLUSTER_EXPR1;
 extern STR AM_CLU12862693351;
 extern STR AM_COMMENT_STMT1;
 extern STR AM_CONST1;
 extern STR AM_CURSOR2;
 extern STR AM_CURSOR_POS1;
 extern STR AM_EXCEPT_EXPR1;
 extern STR AM_EXPR_STMT1;
 extern STR AM_EXT_CALL_EXPR1;
 extern STR AM_FAR_EXPR1;
 extern STR AM_FLTDX_CONST1;
 extern STR AM_FLTD_CONST1;
 extern STR AM_FLTI_CONST1;
 extern STR AM_FLTX_CONST1;
 extern STR AM_FLT_CONST1;
 extern STR AM_FORK_STMT1;
 extern STR AM_FORMAL_ARG1;
 extern STR AM_GLOBAL_EXPR1;
 extern STR AM_HERE_EXPR1;
 extern STR AM_IF_EXPR1;
 extern STR AM_IF_STMT1;
 extern STR AM_INITIAL_STMT1;
 extern STR AM_INTI_CONST1;
 extern STR AM_INT_CONST1;
 extern STR AM_INV11678372301;
 extern STR AM_IS_VOID_EXPR1;
 extern STR AM_ITE18091358501;
 extern STR AM_LOCAL_EXPR1;
 extern STR AM_LOCK_STMT1;
 extern STR AM_LOOP_STMT1;
 extern STR AM_NEAR_EXPR1;
 extern STR AM_NEW_EXPR1;
 extern STR AM_OB_DEF1;
 extern STR AM_PAR_STMT1;
 extern STR AM_POST_STMT1;
 extern STR AM_PREFETCH_STMT1;
 extern STR AM_PRE_STMT1;
 extern STR AM_PROTECT_STMT1;
 extern STR AM_RAISE_STMT1;
 extern STR AM_RETURN_STMT1;
 extern STR AM_ROU19160462901;
 extern STR AM_ROUT_DEF1;
 extern STR AM_SHARED_EXPR1;
 extern STR AM_STMT_EXPR1;
 extern STR AM_STR_CONST1;
 extern STR AM_SYNC_STMT1;
 extern STR AM_TYPECASE_STMT1;
 extern STR AM_UNLOCK_STMT1;
 extern STR AM_VAR7444700971;
 extern STR AM_VAT3199825281;
 extern STR AM_VOID_CONST1;
 extern STR AM_WAITFOR_STMT1;
 extern STR AM_WHERE_EXPR1;
 extern STR AM_WIT19969716031;
 extern STR AM_YIELD_STMT1;
 extern STR AREFFL16982338171;
 extern STR ARG1;
 extern STR ARG_LAYOUT1;
 extern STR ARRAY2BOOL1;
 extern STR ARRAYdAM_CONST1;
 extern STR ARRAYdAM_EXPR1;
 extern STR ARRAYdAM_STMT1;
 extern STR ARRAYdLAYOUT1;
 extern STR ARRAYdTP1;
 extern STR ARRAYA18708083801;
 extern STR ARRAYA8556927141;
 extern STR ARRAYA1325536251;
 extern STR ARRAYAM_CALL_ARG1;
 extern STR ARRAYA362808861;
 extern STR ARRAYAM_CURSOR1;
 extern STR ARRAYA13472925431;
 extern STR ARRAYA13697444921;
 extern STR ARRAYA15756328481;
 extern STR ARRAYAM_ROUT_DEF1;
 extern STR ARRAYA11355487101;
 extern STR ARRAYARG1;
 extern STR ARRAYA4772510191;
 extern STR ARRAYARRAYBOOL1;
 extern STR ARRAYARRAYSTR1;
 extern STR ARRAYAS_FEAT_MOD1;
 extern STR ARRAYATTRSORT1;
 extern STR ARRAYBOOL1;
 extern STR ARRAYB12347653251;
 extern STR ARRAYB19254777281;
 extern STR ARRAYB10393472681;
 extern STR ARRAYCALL_ARG1;
 extern STR ARRAYCHAR1;
 extern STR ARRAYC14386558381;
 extern STR ARRAYCODE_FILE1;
 extern STR ARRAYC17884218991;
 extern STR ARRAYE21264085331;
 extern STR ARRAYE4064872631;
 extern STR ARRAYF10308174421;
 extern STR ARRAYFLISTSTR1;
 extern STR ARRAYFSTR1;
 extern STR ARRAYIDENT1;
 extern STR ARRAYIFC1;
 extern STR ARRAYI13273676921;
 extern STR ARRAYI19919549901;
 extern STR ARRAYINT1;
 extern STR ARRAYO5705941271;
 extern STR ARRAYSFILE_ID1;
 extern STR ARRAYSIDE_EFFECT1;
 extern STR ARRAYSIG1;
 extern STR ARRAYSTR1;
 extern STR ARRAYTP_CLASS1;
 extern STR ARRAYTP_LAYOUT1;
 extern STR ARRAYT7475723801;
 extern STR ARRAYTUPdOBSTR1;
 extern STR ARRAYT14182091731;
 extern STR ARRAYT11531010581;
 extern STR ARRAYT15621412511;
 extern STR ARRAYTUPdTPINT1;
 extern STR ARRAYT12754048341;
 extern STR ARRAYT11462396471;
 extern STR ARRAYT20650450421;
 extern STR ARRAYT17533334251;
 extern STR ARRAYT17447440761;
 extern STR ARRAYT5466472141;
 extern STR ARRAYT19682127951;
 extern STR ARRAYTUPIDENTdTP1;
 extern STR ARRAYT2639536751;
 extern STR ARRAYTUPIDENTINT1;
 extern STR ARRAYT3489312821;
 extern STR ARRAYT5373947651;
 extern STR ARRAYT17591683841;
 extern STR ARRAYT17543487981;
 extern STR ARRAYTUPSIGINT1;
 extern STR ARRAYTUPSIGSIG1;
 extern STR ARRAYTUPSIGSTR1;
 extern STR ARRAYT611121321;
 extern STR ARRAYT3786121471;
 extern STR ARRAYTUPSTRINT1;
 extern STR ARRAYTUPSTRSIG1;
 extern STR ARRAYTUPSTRSTR1;
 extern STR ARRAYT11275057161;
 extern STR ARRAYT16417752701;
 extern STR AS_AND_EXPR1;
 extern STR AS_ANY_EXPR1;
 extern STR AS_ARG_DEC1;
 extern STR AS_ARG_MODE1;
 extern STR AS_ARRAY_EXPR1;
 extern STR AS_ASSERT_STMT1;
 extern STR AS_ASSIGN_STMT1;
 extern STR AS_ATTACH_STMT1;
 extern STR AS_ATTR_DEF1;
 extern STR AS_AT_EXPR1;
 extern STR AS_BOOL_LIT_EXPR1;
 extern STR AS_BOU142693361;
 extern STR AS_BREAK_EXPR1;
 extern STR AS_CALL_EXPR1;
 extern STR AS_CASE_STMT1;
 extern STR AS_CASE_WHEN1;
 extern STR AS_CHAR_LIT_EXPR1;
 extern STR AS_CLASS_DEF1;
 extern STR AS_CLUSTER_EXPR1;
 extern STR AS_CLU5535705991;
 extern STR AS_COHORT_EXPR1;
 extern STR AS_CONST_DEF1;
 extern STR AS_CREATE_EXPR1;
 extern STR AS_DEC_STMT1;
 extern STR AS_EXCEPT_EXPR1;
 extern STR AS_EXPR_STMT1;
 extern STR AS_FAR_EXPR1;
 extern STR AS_FEAT_MOD1;
 extern STR AS_FLT_LIT_EXPR1;
 extern STR AS_FORK_STMT1;
 extern STR AS_GLOBAL_EXPR1;
 extern STR AS_HERE_EXPR1;
 extern STR AS_IDENT_LIST1;
 extern STR AS_IF_STMT1;
 extern STR AS_INC14169170011;
 extern STR AS_INITIAL_EXPR1;
 extern STR AS_INT20312392681;
 extern STR AS_INT_LIT_EXPR1;
 extern STR AS_IS_VOID_EXPR1;
 extern STR AS_LOCK_IF_WHEN1;
 extern STR AS_LOCK_STMT1;
 extern STR AS_LOOP_STMT1;
 extern STR AS_NEAR_EXPR1;
 extern STR AS_NEW_EXPR1;
 extern STR AS_OR_EXPR1;
 extern STR AS_PARAM_DEC1;
 extern STR AS_PAR_STMT1;
 extern STR AS_PROTECT_STMT1;
 extern STR AS_PROTECT_WHEN1;
 extern STR AS_QUIT_STMT1;
 extern STR AS_RAISE_STMT1;
 extern STR AS_RESULT_EXPR1;
 extern STR AS_RETURN_STMT1;
 extern STR AS_ROUT_DEF1;
 extern STR AS_SELF_EXPR1;
 extern STR AS_SHARED_DEF1;
 extern STR AS_STMT1;
 extern STR AS_STMT_CURSOR1;
 extern STR AS_STMT_LIST1;
 extern STR AS_STR_LIT_EXPR1;
 extern STR AS_SYNC_STMT1;
 extern STR AS_TYPECASE_STMT1;
 extern STR AS_TYPECASE_WHEN1;
 extern STR AS_TYPE_SPEC1;
 extern STR AS_UND1529866141;
 extern STR AS_UNLOCK_STMT1;
 extern STR AS_VOID_EXPR1;
 extern STR AS_WHERE_EXPR1;
 extern STR AS_WIT10634373511;
 extern STR AS_YIELD_STMT1;
 extern STR ATTRSORT1;
 extern STR A_STAC12449660981;
 extern STR BFILE1;
 extern STR BOOL1;
 extern STR BOUND_10074073311;
 extern STR BOUND_21403731241;
 extern STR BOUND_16778158021;
 extern STR BOUND_8093907741;
 extern STR BUILD_TYPE_GRAPH1;
 extern STR BUILTIN_LAYOUT1;
 extern STR CALL_ARG1;
 extern STR CALL_SIG1;
 extern STR CALL_TP_ARRAY1;
 extern STR CALL_T5617923671;
 extern STR CALL_TP_CREATE1;
 extern STR CALL_T6663890791;
 extern STR CALL_TP_VOID1;
 extern STR CGEN1;
 extern STR CHANGE_VARS1;
 extern STR CHAR1;
 extern STR CHECK_AM1;
 extern STR CHECK_GENERICS1;
 extern STR CHECK_11745520751;
 extern STR CLASS_LAYOUT1;
 extern STR CODE_FILE1;
 extern STR CODE_FILE_ARRAY1;
 extern STR CONFIG_DEF1;
 extern STR CONFIG_ROUT1;
 extern STR CONFIG_SCANNER1;
 extern STR CONFIG_TBL1;
 extern STR CONVERT1;
 extern STR CS2;
 extern STR CSE_DOUBLE_EXPRS1;
 extern STR CS_OPTIONS1;
 extern STR ELT1;
 extern STR ELT_TBL1;
 extern STR EXPR_HOISTED1;
 extern STR EXTERNAL_LAYOUT1;
 extern STR FILE2;
 extern STR FIND_GENERICS1;
 extern STR FIND_TYPES1;
 extern STR FLISTdAM_CONST1;
 extern STR FLISTdAM_EXPR1;
 extern STR FLISTdAM_STMT1;
 extern STR FLISTdLAYOUT1;
 extern STR FLISTdTP1;
 extern STR FLISTA1627982631;
 extern STR FLISTA18934615111;
 extern STR FLISTA14098462101;
 extern STR FLISTAM_CALL_ARG1;
 extern STR FLISTA8866307051;
 extern STR FLISTAM_CURSOR1;
 extern STR FLISTA20973249341;
 extern STR FLISTA10623349991;
 extern STR FLISTA7252830291;
 extern STR FLISTAM_ROUT_DEF1;
 extern STR FLISTA1123260871;
 extern STR FLISTARG1;
 extern STR FLISTA13276008381;
 extern STR FLISTARRAYBOOL1;
 extern STR FLISTARRAYSTR1;
 extern STR FLISTAS_FEAT_MOD1;
 extern STR FLISTATTRSORT1;
 extern STR FLISTBOOL1;
 extern STR FLISTB4755911181;
 extern STR FLISTB20620800751;
 extern STR FLISTB161246451;
 extern STR FLISTCALL_ARG1;
 extern STR FLISTCHAR1;
 extern STR FLISTC10313092711;
 extern STR FLISTCODE_FILE1;
 extern STR FLISTC17722424521;
 extern STR FLISTE17612121961;
 extern STR FLISTE16271193801;
 extern STR FLISTF75948191;
 extern STR FLISTFLISTSTR1;
 extern STR FLISTFSTR1;
 extern STR FLISTIDENT1;
 extern STR FLISTIFC1;
 extern STR FLISTI20865418991;
 extern STR FLISTI18956657391;
 extern STR FLISTINT1;
 extern STR FLISTO15938167501;
 extern STR FLISTSFILE_ID1;
 extern STR FLISTSIDE_EFFECT1;
 extern STR FLISTSIG1;
 extern STR FLISTSTR1;
 extern STR FLISTTP_CLASS1;
 extern STR FLISTTP_LAYOUT1;
 extern STR FLISTT15137882731;
 extern STR FLISTTUPdOBSTR1;
 extern STR FLISTT5678593541;
 extern STR FLISTT8805055851;
 extern STR FLISTT18586373101;
 extern STR FLISTTUPdTPINT1;
 extern STR FLISTT13113346151;
 extern STR FLISTT16612032541;
 extern STR FLISTT11452363451;
 extern STR FLISTT20498294841;
 extern STR FLISTT4062862511;
 extern STR FLISTT316836071;
 extern STR FLISTT16831652041;
 extern STR FLISTTUPIDENTdTP1;
 extern STR FLISTT10231278821;
 extern STR FLISTTUPIDENTINT1;
 extern STR FLISTT10754949971;
 extern STR FLISTT8448042581;
 extern STR FLISTT17429889371;
 extern STR FLISTT9948054271;
 extern STR FLISTTUPSIGINT1;
 extern STR FLISTTUPSIGSIG1;
 extern STR FLISTTUPSIGSTR1;
 extern STR FLISTT7892376871;
 extern STR FLISTT4717376721;
 extern STR FLISTTUPSTRINT1;
 extern STR FLISTTUPSTRSIG1;
 extern STR FLISTTUPSTRSTR1;
 extern STR FLISTT4183073551;
 extern STR FLISTT1023672251;
 extern STR FLT1;
 extern STR FLTD1;
 extern STR FMAPdOBNAMESPACE1;
 extern STR FMAPdTPdLAYOUT1;
 extern STR FMAPdTPCODE_FILE1;
 extern STR FMAPdT9538166371;
 extern STR FMAPdTPINT1;
 extern STR FMAPAM3396525441;
 extern STR FMAPAM13575969311;
 extern STR FMAPAM11450088111;
 extern STR FMAPAM4180099301;
 extern STR FMAPAM12444835041;
 extern STR FMAPAM17876285731;
 extern STR FMAPIDENTdTP1;
 extern STR FMAPID7390865191;
 extern STR FMAPSI21036215881;
 extern STR FMAPSI5181626691;
 extern STR FMAPSI13499887021;
 extern STR FMAPSI11213731881;
 extern STR FMAPSIGSTR1;
 extern STR FMAPSTRdCONFIG1;
 extern STR FMAPSTRFSETSTR1;
 extern STR FMAPSTRINT1;
 extern STR FMAPSTRSIG1;
 extern STR FMAPSTRSTR1;
 extern STR FMAPTP2917395941;
 extern STR FMAPTP7752976001;
 extern STR FMT_ERROR1;
 extern STR FRAME_LAYOUT1;
 extern STR FSETdLAYOUT1;
 extern STR FSETdTP1;
 extern STR FSETAB15696988081;
 extern STR FSETAM10327783151;
 extern STR FSETAM11584059841;
 extern STR FSETAM_ROUT_DEF1;
 extern STR FSETAM15426789471;
 extern STR FSETAS_FEAT_MOD1;
 extern STR FSETBO15483973181;
 extern STR FSETBO11479317841;
 extern STR FSETBU14464775051;
 extern STR FSETCLASS_LAYOUT1;
 extern STR FSETCS15614182841;
 extern STR FSETEX12609473711;
 extern STR FSETIM1844369611;
 extern STR FSETSFILE_ID1;
 extern STR FSETSIDE_EFFECT1;
 extern STR FSETSIG1;
 extern STR FSETSTR1;
 extern STR FSETTP_CLASS1;
 extern STR FSETTUPIDENTINT1;
 extern STR FSETTUPSIGSIG1;
 extern STR FSTR1;
 extern STR GENERATE_AM1;
 extern STR GET_MAIN_SIG1;
 extern STR GLOBAL_TBL1;
 extern STR IDENT1;
 extern STR IDENT_TBL1;
 extern STR IFC1;
 extern STR IFC_ABS_CREATE1;
 extern STR IMMUTA4217598931;
 extern STR IMPL1;
 extern STR IMPL_CREATE1;
 extern STR IMPL_INCLUDE1;
 extern STR INLINE1;
 extern STR INLINE_ATTR_READ1;
 extern STR INLINE15963118111;
 extern STR INLINE14336689011;
 extern STR INLINE14557683741;
 extern STR INLINE_INT_FOLD1;
 extern STR INLINE_ITER1;
 extern STR INLINE_ITER_TBL1;
 extern STR INLINE_ROUT1;
 extern STR INLINE_ROUT_TBL1;
 extern STR INLINE11649048701;
 extern STR INOUT_MODE1;
 extern STR INT1;
 extern STR INTI;
 extern STR IN_MODE1;
 extern STR LAYOUT_ARR1;
 extern STR LAYOUT_ARRAY1;
 extern STR MANGLE1;
 extern STR MS_RANDOM_GEN1;
 extern STR NAMEMAP1;
 extern STR NAMESPACE1;
 extern STR NULL_STAT1;
 extern STR ONCE_MODE1;
 extern STR OPTIMIZE1;
 extern STR OPT_LOCAL_CALL1;
 extern STR OPT_LOCAL_EXPR1;
 extern STR OUT_MODE1;
 extern STR PARSE1;
 extern STR PARSER1;
 extern STR PROG1;
 extern STR PROG_AS_TBL1;
 extern STR RAT1;
 extern STR ROUTTU4672478931;
 extern STR SCANNER1;
 extern STR SE_CONTEXT1;
 extern STR SFILE_ID1;
 extern STR SIDE_EFFECT1;
 extern STR SIG1;
 extern STR SIG_TBL1;
 extern STR STAT2;
 extern STR STR1;
 extern STR STR_CURSOR1;
 extern STR SYSTEM_LEX1;
 extern STR TP_ARRAY1;
 extern STR TP_CLASS1;
 extern STR TP_CLASS_TBL1;
 extern STR TP_CONTEXT1;
 extern STR TP_GEN_TBL1;
 extern STR TP_GRAPH1;
 extern STR TP_GRAPH_ABS_DES1;
 extern STR TP_ITER1;
 extern STR TP_ITER_TBL1;
 extern STR TP_LAYOUT1;
 extern STR TP_ROUT1;
 extern STR TP_ROUT_TBL1;
 extern STR TP_TBL1;
 extern STR TRANS1;
 extern STR TUPdAM_EXPRdTP1;
 extern STR TUPdAS2686797791;
 extern STR TUPdOBNAMESPACE1;
 extern STR TUPdOBSTR1;
 extern STR TUPdTPdLAYOUT1;
 extern STR TUPdTPCODE_FILE1;
 extern STR TUPdTP17102330511;
 extern STR TUPdTPINT1;
 extern STR TUPAM_3044586491;
 extern STR TUPAM_6678920601;
 extern STR TUPAM_9274374811;
 extern STR TUPAM_15190408771;
 extern STR TUPAM_5046535311;
 extern STR TUPAM_10249948011;
 extern STR TUPAM_6534695741;
 extern STR TUPARRAYARGdTP1;
 extern STR TUPARR17599877761;
 extern STR TUPBOO13929232411;
 extern STR TUPIDENTdTP1;
 extern STR TUPIDE6416541241;
 extern STR TUPIDENTARRAYdTP1;
 extern STR TUPIDENTINT1;
 extern STR TUPINTINT1;
 extern STR TUPSIG17540787361;
 extern STR TUPSIG5786921891;
 extern STR TUPSIGARG_LAYOUT1;
 extern STR TUPSIG20235332471;
 extern STR TUPSIGINT1;
 extern STR TUPSIGSIG1;
 extern STR TUPSIGSTR1;
 extern STR TUPSTRdCONFIG1;
 extern STR TUPSTRFSETSTR1;
 extern STR TUPSTRINT1;
 extern STR TUPSTRSIG1;
 extern STR TUPSTRSTR1;
 extern STR TUPTP_8583212671;
 extern STR TUPTP_16684541721;
 extern STR WEIGH_CODE1;
 extern STR XFORM_CODE1;

 switch (i) {
  case ABSTRA151498155_tag: return ((STR) &ABSTRA1514981551);
  case ABSTRACT_LAYOUT_tag: return ((STR) &ABSTRACT_LAYOUT1);
  case AM_ANY_EXPR_tag: return ((STR) &AM_ANY_EXPR1);
  case AM_ARRAY_EXPR_tag: return ((STR) &AM_ARRAY_EXPR1);
  case AM_ARR_CONST_tag: return ((STR) &AM_ARR_CONST1);
  case AM_ASSERT_STMT_tag: return ((STR) &AM_ASSERT_STMT1);
  case AM_ASSIGN_STMT_tag: return ((STR) &AM_ASSIGN_STMT1);
  case AM_ATTACH_STMT_tag: return ((STR) &AM_ATTACH_STMT1);
  case AM_ATTR_EXPR_tag: return ((STR) &AM_ATTR_EXPR1);
  case AM_AT_EXPR_tag: return ((STR) &AM_AT_EXPR1);
  case AM_BND1124877163_tag: return ((STR) &AM_BND11248771631);
  case AM_BND260301329_tag: return ((STR) &AM_BND2603013291);
  case AM_BND367211769_tag: return ((STR) &AM_BND3672117691);
  case AM_BOOL_CONST_tag: return ((STR) &AM_BOOL_CONST1);
  case AM_BREAK_STMT_tag: return ((STR) &AM_BREAK_STMT1);
  case AM_CALL_ARG_tag: return ((STR) &AM_CALL_ARG1);
  case AM_CASE_STMT_tag: return ((STR) &AM_CASE_STMT1);
  case AM_CHAR_CONST_tag: return ((STR) &AM_CHAR_CONST1);
  case AM_CLU1286269335_tag: return ((STR) &AM_CLU12862693351);
  case AM_CLUSTER_EXPR_tag: return ((STR) &AM_CLUSTER_EXPR1);
  case AM_COMMENT_STMT_tag: return ((STR) &AM_COMMENT_STMT1);
  case AM_CONST_tag: return ((STR) &AM_CONST1);
  case AM_CURSOR_POS_tag: return ((STR) &AM_CURSOR_POS1);
  case AM_CURSOR_tag: return ((STR) &AM_CURSOR2);
  case AM_EXCEPT_EXPR_tag: return ((STR) &AM_EXCEPT_EXPR1);
  case AM_EXPR_STMT_tag: return ((STR) &AM_EXPR_STMT1);
  case AM_EXT_CALL_EXPR_tag: return ((STR) &AM_EXT_CALL_EXPR1);
  case AM_FAR_EXPR_tag: return ((STR) &AM_FAR_EXPR1);
  case AM_FLTDX_CONST_tag: return ((STR) &AM_FLTDX_CONST1);
  case AM_FLTD_CONST_tag: return ((STR) &AM_FLTD_CONST1);
  case AM_FLTI_CONST_tag: return ((STR) &AM_FLTI_CONST1);
  case AM_FLTX_CONST_tag: return ((STR) &AM_FLTX_CONST1);
  case AM_FLT_CONST_tag: return ((STR) &AM_FLT_CONST1);
  case AM_FORK_STMT_tag: return ((STR) &AM_FORK_STMT1);
  case AM_FORMAL_ARG_tag: return ((STR) &AM_FORMAL_ARG1);
  case AM_GLOBAL_EXPR_tag: return ((STR) &AM_GLOBAL_EXPR1);
  case AM_HERE_EXPR_tag: return ((STR) &AM_HERE_EXPR1);
  case AM_IF_EXPR_tag: return ((STR) &AM_IF_EXPR1);
  case AM_IF_STMT_tag: return ((STR) &AM_IF_STMT1);
  case AM_INITIAL_STMT_tag: return ((STR) &AM_INITIAL_STMT1);
  case AM_INTI_CONST_tag: return ((STR) &AM_INTI_CONST1);
  case AM_INT_CONST_tag: return ((STR) &AM_INT_CONST1);
  case AM_INV1167837230_tag: return ((STR) &AM_INV11678372301);
  case AM_IS_VOID_EXPR_tag: return ((STR) &AM_IS_VOID_EXPR1);
  case AM_ITE1809135850_tag: return ((STR) &AM_ITE18091358501);
  case AM_LOCAL_EXPR_tag: return ((STR) &AM_LOCAL_EXPR1);
  case AM_LOCK_STMT_tag: return ((STR) &AM_LOCK_STMT1);
  case AM_LOOP_STMT_tag: return ((STR) &AM_LOOP_STMT1);
  case AM_NEAR_EXPR_tag: return ((STR) &AM_NEAR_EXPR1);
  case AM_NEW_EXPR_tag: return ((STR) &AM_NEW_EXPR1);
  case AM_OB_DEF_tag: return ((STR) &AM_OB_DEF1);
  case AM_PAR_STMT_tag: return ((STR) &AM_PAR_STMT1);
  case AM_POST_STMT_tag: return ((STR) &AM_POST_STMT1);
  case AM_PREFETCH_STMT_tag: return ((STR) &AM_PREFETCH_STMT1);
  case AM_PRE_STMT_tag: return ((STR) &AM_PRE_STMT1);
  case AM_PROTECT_STMT_tag: return ((STR) &AM_PROTECT_STMT1);
  case AM_RAISE_STMT_tag: return ((STR) &AM_RAISE_STMT1);
  case AM_RETURN_STMT_tag: return ((STR) &AM_RETURN_STMT1);
  case AM_ROU1916046290_tag: return ((STR) &AM_ROU19160462901);
  case AM_ROUT_DEF_tag: return ((STR) &AM_ROUT_DEF1);
  case AM_SHARED_EXPR_tag: return ((STR) &AM_SHARED_EXPR1);
  case AM_STMT_EXPR_tag: return ((STR) &AM_STMT_EXPR1);
  case AM_STR_CONST_tag: return ((STR) &AM_STR_CONST1);
  case AM_SYNC_STMT_tag: return ((STR) &AM_SYNC_STMT1);
  case AM_TYPECASE_STMT_tag: return ((STR) &AM_TYPECASE_STMT1);
  case AM_UNLOCK_STMT_tag: return ((STR) &AM_UNLOCK_STMT1);
  case AM_VAR744470097_tag: return ((STR) &AM_VAR7444700971);
  case AM_VAT319982528_tag: return ((STR) &AM_VAT3199825281);
  case AM_VOID_CONST_tag: return ((STR) &AM_VOID_CONST1);
  case AM_WAITFOR_STMT_tag: return ((STR) &AM_WAITFOR_STMT1);
  case AM_WHERE_EXPR_tag: return ((STR) &AM_WHERE_EXPR1);
  case AM_WIT1996971603_tag: return ((STR) &AM_WIT19969716031);
  case AM_YIELD_STMT_tag: return ((STR) &AM_YIELD_STMT1);
  case AREFFL1698233817_tag: return ((STR) &AREFFL16982338171);
  case ARG_LAYOUT_tag: return ((STR) &ARG_LAYOUT1);
  case ARG_tag: return ((STR) &ARG1);
  case ARRAY2BOOL_tag: return ((STR) &ARRAY2BOOL1);
  case ARRAYA1135548710_tag: return ((STR) &ARRAYA11355487101);
  case ARRAYA132553625_tag: return ((STR) &ARRAYA1325536251);
  case ARRAYA1347292543_tag: return ((STR) &ARRAYA13472925431);
  case ARRAYA1369744492_tag: return ((STR) &ARRAYA13697444921);
  case ARRAYA1575632848_tag: return ((STR) &ARRAYA15756328481);
  case ARRAYA1870808380_tag: return ((STR) &ARRAYA18708083801);
  case ARRAYA36280886_tag: return ((STR) &ARRAYA362808861);
  case ARRAYA477251019_tag: return ((STR) &ARRAYA4772510191);
  case ARRAYA855692714_tag: return ((STR) &ARRAYA8556927141);
  case ARRAYAM_CALL_ARG_tag: return ((STR) &ARRAYAM_CALL_ARG1);
  case ARRAYAM_CURSOR_tag: return ((STR) &ARRAYAM_CURSOR1);
  case ARRAYAM_ROUT_DEF_tag: return ((STR) &ARRAYAM_ROUT_DEF1);
  case ARRAYARG_tag: return ((STR) &ARRAYARG1);
  case ARRAYARRAYBOOL_tag: return ((STR) &ARRAYARRAYBOOL1);
  case ARRAYARRAYSTR_tag: return ((STR) &ARRAYARRAYSTR1);
  case ARRAYAS_FEAT_MOD_tag: return ((STR) &ARRAYAS_FEAT_MOD1);
  case ARRAYATTRSORT_tag: return ((STR) &ARRAYATTRSORT1);
  case ARRAYB1039347268_tag: return ((STR) &ARRAYB10393472681);
  case ARRAYB1234765325_tag: return ((STR) &ARRAYB12347653251);
  case ARRAYB1925477728_tag: return ((STR) &ARRAYB19254777281);
  case ARRAYBOOL_tag: return ((STR) &ARRAYBOOL1);
  case ARRAYC1438655838_tag: return ((STR) &ARRAYC14386558381);
  case ARRAYC1788421899_tag: return ((STR) &ARRAYC17884218991);
  case ARRAYCALL_ARG_tag: return ((STR) &ARRAYCALL_ARG1);
  case ARRAYCHAR_tag: return ((STR) &ARRAYCHAR1);
  case ARRAYCODE_FILE_tag: return ((STR) &ARRAYCODE_FILE1);
  case ARRAYE2126408533_tag: return ((STR) &ARRAYE21264085331);
  case ARRAYE406487263_tag: return ((STR) &ARRAYE4064872631);
  case ARRAYF1030817442_tag: return ((STR) &ARRAYF10308174421);
  case ARRAYFLISTSTR_tag: return ((STR) &ARRAYFLISTSTR1);
  case ARRAYFSTR_tag: return ((STR) &ARRAYFSTR1);
  case ARRAYI1327367692_tag: return ((STR) &ARRAYI13273676921);
  case ARRAYI1991954990_tag: return ((STR) &ARRAYI19919549901);
  case ARRAYIDENT_tag: return ((STR) &ARRAYIDENT1);
  case ARRAYIFC_tag: return ((STR) &ARRAYIFC1);
  case ARRAYINT_tag: return ((STR) &ARRAYINT1);
  case ARRAYO570594127_tag: return ((STR) &ARRAYO5705941271);
  case ARRAYSFILE_ID_tag: return ((STR) &ARRAYSFILE_ID1);
  case ARRAYSIDE_EFFECT_tag: return ((STR) &ARRAYSIDE_EFFECT1);
  case ARRAYSIG_tag: return ((STR) &ARRAYSIG1);
  case ARRAYSTR_tag: return ((STR) &ARRAYSTR1);
  case ARRAYT1127505716_tag: return ((STR) &ARRAYT11275057161);
  case ARRAYT1146239647_tag: return ((STR) &ARRAYT11462396471);
  case ARRAYT1153101058_tag: return ((STR) &ARRAYT11531010581);
  case ARRAYT1275404834_tag: return ((STR) &ARRAYT12754048341);
  case ARRAYT1418209173_tag: return ((STR) &ARRAYT14182091731);
  case ARRAYT1562141251_tag: return ((STR) &ARRAYT15621412511);
  case ARRAYT1641775270_tag: return ((STR) &ARRAYT16417752701);
  case ARRAYT1744744076_tag: return ((STR) &ARRAYT17447440761);
  case ARRAYT1753333425_tag: return ((STR) &ARRAYT17533334251);
  case ARRAYT1754348798_tag: return ((STR) &ARRAYT17543487981);
  case ARRAYT1759168384_tag: return ((STR) &ARRAYT17591683841);
  case ARRAYT1968212795_tag: return ((STR) &ARRAYT19682127951);
  case ARRAYT2065045042_tag: return ((STR) &ARRAYT20650450421);
  case ARRAYT263953675_tag: return ((STR) &ARRAYT2639536751);
  case ARRAYT348931282_tag: return ((STR) &ARRAYT3489312821);
  case ARRAYT378612147_tag: return ((STR) &ARRAYT3786121471);
  case ARRAYT537394765_tag: return ((STR) &ARRAYT5373947651);
  case ARRAYT546647214_tag: return ((STR) &ARRAYT5466472141);
  case ARRAYT61112132_tag: return ((STR) &ARRAYT611121321);
  case ARRAYT747572380_tag: return ((STR) &ARRAYT7475723801);
  case ARRAYTP_CLASS_tag: return ((STR) &ARRAYTP_CLASS1);
  case ARRAYTP_LAYOUT_tag: return ((STR) &ARRAYTP_LAYOUT1);
  case ARRAYTUPIDENTINT_tag: return ((STR) &ARRAYTUPIDENTINT1);
  case ARRAYTUPIDENTdTP_tag: return ((STR) &ARRAYTUPIDENTdTP1);
  case ARRAYTUPSIGINT_tag: return ((STR) &ARRAYTUPSIGINT1);
  case ARRAYTUPSIGSIG_tag: return ((STR) &ARRAYTUPSIGSIG1);
  case ARRAYTUPSIGSTR_tag: return ((STR) &ARRAYTUPSIGSTR1);
  case ARRAYTUPSTRINT_tag: return ((STR) &ARRAYTUPSTRINT1);
  case ARRAYTUPSTRSIG_tag: return ((STR) &ARRAYTUPSTRSIG1);
  case ARRAYTUPSTRSTR_tag: return ((STR) &ARRAYTUPSTRSTR1);
  case ARRAYTUPdOBSTR_tag: return ((STR) &ARRAYTUPdOBSTR1);
  case ARRAYTUPdTPINT_tag: return ((STR) &ARRAYTUPdTPINT1);
  case ARRAYdAM_CONST_tag: return ((STR) &ARRAYdAM_CONST1);
  case ARRAYdAM_EXPR_tag: return ((STR) &ARRAYdAM_EXPR1);
  case ARRAYdAM_STMT_tag: return ((STR) &ARRAYdAM_STMT1);
  case ARRAYdLAYOUT_tag: return ((STR) &ARRAYdLAYOUT1);
  case ARRAYdTP_tag: return ((STR) &ARRAYdTP1);
  case AS_AND_EXPR_tag: return ((STR) &AS_AND_EXPR1);
  case AS_ANY_EXPR_tag: return ((STR) &AS_ANY_EXPR1);
  case AS_ARG_DEC_tag: return ((STR) &AS_ARG_DEC1);
  case AS_ARG_MODE_tag: return ((STR) &AS_ARG_MODE1);
  case AS_ARRAY_EXPR_tag: return ((STR) &AS_ARRAY_EXPR1);
  case AS_ASSERT_STMT_tag: return ((STR) &AS_ASSERT_STMT1);
  case AS_ASSIGN_STMT_tag: return ((STR) &AS_ASSIGN_STMT1);
  case AS_ATTACH_STMT_tag: return ((STR) &AS_ATTACH_STMT1);
  case AS_ATTR_DEF_tag: return ((STR) &AS_ATTR_DEF1);
  case AS_AT_EXPR_tag: return ((STR) &AS_AT_EXPR1);
  case AS_BOOL_LIT_EXPR_tag: return ((STR) &AS_BOOL_LIT_EXPR1);
  case AS_BOU14269336_tag: return ((STR) &AS_BOU142693361);
  case AS_BREAK_EXPR_tag: return ((STR) &AS_BREAK_EXPR1);
  case AS_CALL_EXPR_tag: return ((STR) &AS_CALL_EXPR1);
  case AS_CASE_STMT_tag: return ((STR) &AS_CASE_STMT1);
  case AS_CASE_WHEN_tag: return ((STR) &AS_CASE_WHEN1);
  case AS_CHAR_LIT_EXPR_tag: return ((STR) &AS_CHAR_LIT_EXPR1);
  case AS_CLASS_DEF_tag: return ((STR) &AS_CLASS_DEF1);
  case AS_CLU553570599_tag: return ((STR) &AS_CLU5535705991);
  case AS_CLUSTER_EXPR_tag: return ((STR) &AS_CLUSTER_EXPR1);
  case AS_COHORT_EXPR_tag: return ((STR) &AS_COHORT_EXPR1);
  case AS_CONST_DEF_tag: return ((STR) &AS_CONST_DEF1);
  case AS_CREATE_EXPR_tag: return ((STR) &AS_CREATE_EXPR1);
  case AS_DEC_STMT_tag: return ((STR) &AS_DEC_STMT1);
  case AS_EXCEPT_EXPR_tag: return ((STR) &AS_EXCEPT_EXPR1);
  case AS_EXPR_STMT_tag: return ((STR) &AS_EXPR_STMT1);
  case AS_FAR_EXPR_tag: return ((STR) &AS_FAR_EXPR1);
  case AS_FEAT_MOD_tag: return ((STR) &AS_FEAT_MOD1);
  case AS_FLT_LIT_EXPR_tag: return ((STR) &AS_FLT_LIT_EXPR1);
  case AS_FORK_STMT_tag: return ((STR) &AS_FORK_STMT1);
  case AS_GLOBAL_EXPR_tag: return ((STR) &AS_GLOBAL_EXPR1);
  case AS_HERE_EXPR_tag: return ((STR) &AS_HERE_EXPR1);
  case AS_IDENT_LIST_tag: return ((STR) &AS_IDENT_LIST1);
  case AS_IF_STMT_tag: return ((STR) &AS_IF_STMT1);
  case AS_INC1416917001_tag: return ((STR) &AS_INC14169170011);
  case AS_INITIAL_EXPR_tag: return ((STR) &AS_INITIAL_EXPR1);
  case AS_INT2031239268_tag: return ((STR) &AS_INT20312392681);
  case AS_INT_LIT_EXPR_tag: return ((STR) &AS_INT_LIT_EXPR1);
  case AS_IS_VOID_EXPR_tag: return ((STR) &AS_IS_VOID_EXPR1);
  case AS_LOCK_IF_WHEN_tag: return ((STR) &AS_LOCK_IF_WHEN1);
  case AS_LOCK_STMT_tag: return ((STR) &AS_LOCK_STMT1);
  case AS_LOOP_STMT_tag: return ((STR) &AS_LOOP_STMT1);
  case AS_NEAR_EXPR_tag: return ((STR) &AS_NEAR_EXPR1);
  case AS_NEW_EXPR_tag: return ((STR) &AS_NEW_EXPR1);
  case AS_OR_EXPR_tag: return ((STR) &AS_OR_EXPR1);
  case AS_PARAM_DEC_tag: return ((STR) &AS_PARAM_DEC1);
  case AS_PAR_STMT_tag: return ((STR) &AS_PAR_STMT1);
  case AS_PROTECT_STMT_tag: return ((STR) &AS_PROTECT_STMT1);
  case AS_PROTECT_WHEN_tag: return ((STR) &AS_PROTECT_WHEN1);
  case AS_QUIT_STMT_tag: return ((STR) &AS_QUIT_STMT1);
  case AS_RAISE_STMT_tag: return ((STR) &AS_RAISE_STMT1);
  case AS_RESULT_EXPR_tag: return ((STR) &AS_RESULT_EXPR1);
  case AS_RETURN_STMT_tag: return ((STR) &AS_RETURN_STMT1);
  case AS_ROUT_DEF_tag: return ((STR) &AS_ROUT_DEF1);
  case AS_SELF_EXPR_tag: return ((STR) &AS_SELF_EXPR1);
  case AS_SHARED_DEF_tag: return ((STR) &AS_SHARED_DEF1);
  case AS_STMT_CURSOR_tag: return ((STR) &AS_STMT_CURSOR1);
  case AS_STMT_LIST_tag: return ((STR) &AS_STMT_LIST1);
  case AS_STMT_tag: return ((STR) &AS_STMT1);
  case AS_STR_LIT_EXPR_tag: return ((STR) &AS_STR_LIT_EXPR1);
  case AS_SYNC_STMT_tag: return ((STR) &AS_SYNC_STMT1);
  case AS_TYPECASE_STMT_tag: return ((STR) &AS_TYPECASE_STMT1);
  case AS_TYPECASE_WHEN_tag: return ((STR) &AS_TYPECASE_WHEN1);
  case AS_TYPE_SPEC_tag: return ((STR) &AS_TYPE_SPEC1);
  case AS_UND152986614_tag: return ((STR) &AS_UND1529866141);
  case AS_UNLOCK_STMT_tag: return ((STR) &AS_UNLOCK_STMT1);
  case AS_VOID_EXPR_tag: return ((STR) &AS_VOID_EXPR1);
  case AS_WHERE_EXPR_tag: return ((STR) &AS_WHERE_EXPR1);
  case AS_WIT1063437351_tag: return ((STR) &AS_WIT10634373511);
  case AS_YIELD_STMT_tag: return ((STR) &AS_YIELD_STMT1);
  case ATTRSORT_tag: return ((STR) &ATTRSORT1);
  case A_STAC1244966098_tag: return ((STR) &A_STAC12449660981);
  case BFILE_tag: return ((STR) &BFILE1);
  case BOOL_tag: return ((STR) &BOOL1);
  case BOUND_1007407331_tag: return ((STR) &BOUND_10074073311);
  case BOUND_1677815802_tag: return ((STR) &BOUND_16778158021);
  case BOUND_2140373124_tag: return ((STR) &BOUND_21403731241);
  case BOUND_809390774_tag: return ((STR) &BOUND_8093907741);
  case BUILD_TYPE_GRAPH_tag: return ((STR) &BUILD_TYPE_GRAPH1);
  case BUILTIN_LAYOUT_tag: return ((STR) &BUILTIN_LAYOUT1);
  case CALL_ARG_tag: return ((STR) &CALL_ARG1);
  case CALL_SIG_tag: return ((STR) &CALL_SIG1);
  case CALL_T561792367_tag: return ((STR) &CALL_T5617923671);
  case CALL_T666389079_tag: return ((STR) &CALL_T6663890791);
  case CALL_TP_ARRAY_tag: return ((STR) &CALL_TP_ARRAY1);
  case CALL_TP_CREATE_tag: return ((STR) &CALL_TP_CREATE1);
  case CALL_TP_VOID_tag: return ((STR) &CALL_TP_VOID1);
  case CGEN_tag: return ((STR) &CGEN1);
  case CHANGE_VARS_tag: return ((STR) &CHANGE_VARS1);
  case CHAR_tag: return ((STR) &CHAR1);
  case CHECK_1174552075_tag: return ((STR) &CHECK_11745520751);
  case CHECK_AM_tag: return ((STR) &CHECK_AM1);
  case CHECK_GENERICS_tag: return ((STR) &CHECK_GENERICS1);
  case CLASS_LAYOUT_tag: return ((STR) &CLASS_LAYOUT1);
  case CODE_FILE_ARRAY_tag: return ((STR) &CODE_FILE_ARRAY1);
  case CODE_FILE_tag: return ((STR) &CODE_FILE1);
  case CONFIG_DEF_tag: return ((STR) &CONFIG_DEF1);
  case CONFIG_ROUT_tag: return ((STR) &CONFIG_ROUT1);
  case CONFIG_SCANNER_tag: return ((STR) &CONFIG_SCANNER1);
  case CONFIG_TBL_tag: return ((STR) &CONFIG_TBL1);
  case CONVERT_tag: return ((STR) &CONVERT1);
  case CSE_DOUBLE_EXPRS_tag: return ((STR) &CSE_DOUBLE_EXPRS1);
  case CS_OPTIONS_tag: return ((STR) &CS_OPTIONS1);
  case CS_tag: return ((STR) &CS2);
  case ELT_TBL_tag: return ((STR) &ELT_TBL1);
  case ELT_tag: return ((STR) &ELT1);
  case EXPR_HOISTED_tag: return ((STR) &EXPR_HOISTED1);
  case EXTERNAL_LAYOUT_tag: return ((STR) &EXTERNAL_LAYOUT1);
  case FILE1_tag: return ((STR) &FILE2);
  case FIND_GENERICS_tag: return ((STR) &FIND_GENERICS1);
  case FIND_TYPES_tag: return ((STR) &FIND_TYPES1);
  case FLISTA1062334999_tag: return ((STR) &FLISTA10623349991);
  case FLISTA112326087_tag: return ((STR) &FLISTA1123260871);
  case FLISTA1327600838_tag: return ((STR) &FLISTA13276008381);
  case FLISTA1409846210_tag: return ((STR) &FLISTA14098462101);
  case FLISTA162798263_tag: return ((STR) &FLISTA1627982631);
  case FLISTA1893461511_tag: return ((STR) &FLISTA18934615111);
  case FLISTA2097324934_tag: return ((STR) &FLISTA20973249341);
  case FLISTA725283029_tag: return ((STR) &FLISTA7252830291);
  case FLISTA886630705_tag: return ((STR) &FLISTA8866307051);
  case FLISTAM_CALL_ARG_tag: return ((STR) &FLISTAM_CALL_ARG1);
  case FLISTAM_CURSOR_tag: return ((STR) &FLISTAM_CURSOR1);
  case FLISTAM_ROUT_DEF_tag: return ((STR) &FLISTAM_ROUT_DEF1);
  case FLISTARG_tag: return ((STR) &FLISTARG1);
  case FLISTARRAYBOOL_tag: return ((STR) &FLISTARRAYBOOL1);
  case FLISTARRAYSTR_tag: return ((STR) &FLISTARRAYSTR1);
  case FLISTAS_FEAT_MOD_tag: return ((STR) &FLISTAS_FEAT_MOD1);
  case FLISTATTRSORT_tag: return ((STR) &FLISTATTRSORT1);
  case FLISTB16124645_tag: return ((STR) &FLISTB161246451);
  case FLISTB2062080075_tag: return ((STR) &FLISTB20620800751);
  case FLISTB475591118_tag: return ((STR) &FLISTB4755911181);
  case FLISTBOOL_tag: return ((STR) &FLISTBOOL1);
  case FLISTC1031309271_tag: return ((STR) &FLISTC10313092711);
  case FLISTC1772242452_tag: return ((STR) &FLISTC17722424521);
  case FLISTCALL_ARG_tag: return ((STR) &FLISTCALL_ARG1);
  case FLISTCHAR_tag: return ((STR) &FLISTCHAR1);
  case FLISTCODE_FILE_tag: return ((STR) &FLISTCODE_FILE1);
  case FLISTE1627119380_tag: return ((STR) &FLISTE16271193801);
  case FLISTE1761212196_tag: return ((STR) &FLISTE17612121961);
  case FLISTF7594819_tag: return ((STR) &FLISTF75948191);
  case FLISTFLISTSTR_tag: return ((STR) &FLISTFLISTSTR1);
  case FLISTFSTR_tag: return ((STR) &FLISTFSTR1);
  case FLISTI1895665739_tag: return ((STR) &FLISTI18956657391);
  case FLISTI2086541899_tag: return ((STR) &FLISTI20865418991);
  case FLISTIDENT_tag: return ((STR) &FLISTIDENT1);
  case FLISTIFC_tag: return ((STR) &FLISTIFC1);
  case FLISTINT_tag: return ((STR) &FLISTINT1);
  case FLISTO1593816750_tag: return ((STR) &FLISTO15938167501);
  case FLISTSFILE_ID_tag: return ((STR) &FLISTSFILE_ID1);
  case FLISTSIDE_EFFECT_tag: return ((STR) &FLISTSIDE_EFFECT1);
  case FLISTSIG_tag: return ((STR) &FLISTSIG1);
  case FLISTSTR_tag: return ((STR) &FLISTSTR1);
  case FLISTT1023127882_tag: return ((STR) &FLISTT10231278821);
  case FLISTT102367225_tag: return ((STR) &FLISTT1023672251);
  case FLISTT1075494997_tag: return ((STR) &FLISTT10754949971);
  case FLISTT1145236345_tag: return ((STR) &FLISTT11452363451);
  case FLISTT1311334615_tag: return ((STR) &FLISTT13113346151);
  case FLISTT1513788273_tag: return ((STR) &FLISTT15137882731);
  case FLISTT1661203254_tag: return ((STR) &FLISTT16612032541);
  case FLISTT1683165204_tag: return ((STR) &FLISTT16831652041);
  case FLISTT1742988937_tag: return ((STR) &FLISTT17429889371);
  case FLISTT1858637310_tag: return ((STR) &FLISTT18586373101);
  case FLISTT2049829484_tag: return ((STR) &FLISTT20498294841);
  case FLISTT31683607_tag: return ((STR) &FLISTT316836071);
  case FLISTT406286251_tag: return ((STR) &FLISTT4062862511);
  case FLISTT418307355_tag: return ((STR) &FLISTT4183073551);
  case FLISTT471737672_tag: return ((STR) &FLISTT4717376721);
  case FLISTT567859354_tag: return ((STR) &FLISTT5678593541);
  case FLISTT789237687_tag: return ((STR) &FLISTT7892376871);
  case FLISTT844804258_tag: return ((STR) &FLISTT8448042581);
  case FLISTT880505585_tag: return ((STR) &FLISTT8805055851);
  case FLISTT994805427_tag: return ((STR) &FLISTT9948054271);
  case FLISTTP_CLASS_tag: return ((STR) &FLISTTP_CLASS1);
  case FLISTTP_LAYOUT_tag: return ((STR) &FLISTTP_LAYOUT1);
  case FLISTTUPIDENTINT_tag: return ((STR) &FLISTTUPIDENTINT1);
  case FLISTTUPIDENTdTP_tag: return ((STR) &FLISTTUPIDENTdTP1);
  case FLISTTUPSIGINT_tag: return ((STR) &FLISTTUPSIGINT1);
  case FLISTTUPSIGSIG_tag: return ((STR) &FLISTTUPSIGSIG1);
  case FLISTTUPSIGSTR_tag: return ((STR) &FLISTTUPSIGSTR1);
  case FLISTTUPSTRINT_tag: return ((STR) &FLISTTUPSTRINT1);
  case FLISTTUPSTRSIG_tag: return ((STR) &FLISTTUPSTRSIG1);
  case FLISTTUPSTRSTR_tag: return ((STR) &FLISTTUPSTRSTR1);
  case FLISTTUPdOBSTR_tag: return ((STR) &FLISTTUPdOBSTR1);
  case FLISTTUPdTPINT_tag: return ((STR) &FLISTTUPdTPINT1);
  case FLISTdAM_CONST_tag: return ((STR) &FLISTdAM_CONST1);
  case FLISTdAM_EXPR_tag: return ((STR) &FLISTdAM_EXPR1);
  case FLISTdAM_STMT_tag: return ((STR) &FLISTdAM_STMT1);
  case FLISTdLAYOUT_tag: return ((STR) &FLISTdLAYOUT1);
  case FLISTdTP_tag: return ((STR) &FLISTdTP1);
  case FLTD_tag: return ((STR) &FLTD1);
  case FLT_tag: return ((STR) &FLT1);
  case FMAPAM1145008811_tag: return ((STR) &FMAPAM11450088111);
  case FMAPAM1244483504_tag: return ((STR) &FMAPAM12444835041);
  case FMAPAM1357596931_tag: return ((STR) &FMAPAM13575969311);
  case FMAPAM1787628573_tag: return ((STR) &FMAPAM17876285731);
  case FMAPAM339652544_tag: return ((STR) &FMAPAM3396525441);
  case FMAPAM418009930_tag: return ((STR) &FMAPAM4180099301);
  case FMAPID739086519_tag: return ((STR) &FMAPID7390865191);
  case FMAPIDENTdTP_tag: return ((STR) &FMAPIDENTdTP1);
  case FMAPSI1121373188_tag: return ((STR) &FMAPSI11213731881);
  case FMAPSI1349988702_tag: return ((STR) &FMAPSI13499887021);
  case FMAPSI2103621588_tag: return ((STR) &FMAPSI21036215881);
  case FMAPSI518162669_tag: return ((STR) &FMAPSI5181626691);
  case FMAPSIGSTR_tag: return ((STR) &FMAPSIGSTR1);
  case FMAPSTRFSETSTR_tag: return ((STR) &FMAPSTRFSETSTR1);
  case FMAPSTRINT_tag: return ((STR) &FMAPSTRINT1);
  case FMAPSTRSIG_tag: return ((STR) &FMAPSTRSIG1);
  case FMAPSTRSTR_tag: return ((STR) &FMAPSTRSTR1);
  case FMAPSTRdCONFIG_tag: return ((STR) &FMAPSTRdCONFIG1);
  case FMAPTP291739594_tag: return ((STR) &FMAPTP2917395941);
  case FMAPTP775297600_tag: return ((STR) &FMAPTP7752976001);
  case FMAPdOBNAMESPACE_tag: return ((STR) &FMAPdOBNAMESPACE1);
  case FMAPdT953816637_tag: return ((STR) &FMAPdT9538166371);
  case FMAPdTPCODE_FILE_tag: return ((STR) &FMAPdTPCODE_FILE1);
  case FMAPdTPINT_tag: return ((STR) &FMAPdTPINT1);
  case FMAPdTPdLAYOUT_tag: return ((STR) &FMAPdTPdLAYOUT1);
  case FMT_ERROR_tag: return ((STR) &FMT_ERROR1);
  case FRAME_LAYOUT_tag: return ((STR) &FRAME_LAYOUT1);
  case FSETAB1569698808_tag: return ((STR) &FSETAB15696988081);
  case FSETAM1032778315_tag: return ((STR) &FSETAM10327783151);
  case FSETAM1158405984_tag: return ((STR) &FSETAM11584059841);
  case FSETAM1542678947_tag: return ((STR) &FSETAM15426789471);
  case FSETAM_ROUT_DEF_tag: return ((STR) &FSETAM_ROUT_DEF1);
  case FSETAS_FEAT_MOD_tag: return ((STR) &FSETAS_FEAT_MOD1);
  case FSETBO1147931784_tag: return ((STR) &FSETBO11479317841);
  case FSETBO1548397318_tag: return ((STR) &FSETBO15483973181);
  case FSETBU1446477505_tag: return ((STR) &FSETBU14464775051);
  case FSETCLASS_LAYOUT_tag: return ((STR) &FSETCLASS_LAYOUT1);
  case FSETCS1561418284_tag: return ((STR) &FSETCS15614182841);
  case FSETEX1260947371_tag: return ((STR) &FSETEX12609473711);
  case FSETIM184436961_tag: return ((STR) &FSETIM1844369611);
  case FSETSFILE_ID_tag: return ((STR) &FSETSFILE_ID1);
  case FSETSIDE_EFFECT_tag: return ((STR) &FSETSIDE_EFFECT1);
  case FSETSIG_tag: return ((STR) &FSETSIG1);
  case FSETSTR_tag: return ((STR) &FSETSTR1);
  case FSETTP_CLASS_tag: return ((STR) &FSETTP_CLASS1);
  case FSETTUPIDENTINT_tag: return ((STR) &FSETTUPIDENTINT1);
  case FSETTUPSIGSIG_tag: return ((STR) &FSETTUPSIGSIG1);
  case FSETdLAYOUT_tag: return ((STR) &FSETdLAYOUT1);
  case FSETdTP_tag: return ((STR) &FSETdTP1);
  case FSTR_tag: return ((STR) &FSTR1);
  case GENERATE_AM_tag: return ((STR) &GENERATE_AM1);
  case GET_MAIN_SIG_tag: return ((STR) &GET_MAIN_SIG1);
  case GLOBAL_TBL_tag: return ((STR) &GLOBAL_TBL1);
  case IDENT_TBL_tag: return ((STR) &IDENT_TBL1);
  case IDENT_tag: return ((STR) &IDENT1);
  case IFC_ABS_CREATE_tag: return ((STR) &IFC_ABS_CREATE1);
  case IFC_tag: return ((STR) &IFC1);
  case IMMUTA421759893_tag: return ((STR) &IMMUTA4217598931);
  case IMPL_CREATE_tag: return ((STR) &IMPL_CREATE1);
  case IMPL_INCLUDE_tag: return ((STR) &IMPL_INCLUDE1);
  case IMPL_tag: return ((STR) &IMPL1);
  case INLINE1164904870_tag: return ((STR) &INLINE11649048701);
  case INLINE1433668901_tag: return ((STR) &INLINE14336689011);
  case INLINE1455768374_tag: return ((STR) &INLINE14557683741);
  case INLINE1596311811_tag: return ((STR) &INLINE15963118111);
  case INLINE_ATTR_READ_tag: return ((STR) &INLINE_ATTR_READ1);
  case INLINE_INT_FOLD_tag: return ((STR) &INLINE_INT_FOLD1);
  case INLINE_ITER_TBL_tag: return ((STR) &INLINE_ITER_TBL1);
  case INLINE_ITER_tag: return ((STR) &INLINE_ITER1);
  case INLINE_ROUT_TBL_tag: return ((STR) &INLINE_ROUT_TBL1);
  case INLINE_ROUT_tag: return ((STR) &INLINE_ROUT1);
  case INLINE_tag: return ((STR) &INLINE1);
  case INOUT_MODE_tag: return ((STR) &INOUT_MODE1);
  case INTI1_tag: return ((STR) &INTI);
  case INT_tag: return ((STR) &INT1);
  case IN_MODE_tag: return ((STR) &IN_MODE1);
  case LAYOUT_ARRAY_tag: return ((STR) &LAYOUT_ARRAY1);
  case LAYOUT_ARR_tag: return ((STR) &LAYOUT_ARR1);
  case MANGLE_tag: return ((STR) &MANGLE1);
  case MS_RANDOM_GEN_tag: return ((STR) &MS_RANDOM_GEN1);
  case NAMEMAP_tag: return ((STR) &NAMEMAP1);
  case NAMESPACE_tag: return ((STR) &NAMESPACE1);
  case NULL_STAT_tag: return ((STR) &NULL_STAT1);
  case ONCE_MODE_tag: return ((STR) &ONCE_MODE1);
  case OPTIMIZE_tag: return ((STR) &OPTIMIZE1);
  case OPT_LOCAL_CALL_tag: return ((STR) &OPT_LOCAL_CALL1);
  case OPT_LOCAL_EXPR_tag: return ((STR) &OPT_LOCAL_EXPR1);
  case OUT_MODE_tag: return ((STR) &OUT_MODE1);
  case PARSER_tag: return ((STR) &PARSER1);
  case PARSE_tag: return ((STR) &PARSE1);
  case PROG_AS_TBL_tag: return ((STR) &PROG_AS_TBL1);
  case PROG_tag: return ((STR) &PROG1);
  case RAT_tag: return ((STR) &RAT1);
  case ROUTTU467247893_tag: return ((STR) &ROUTTU4672478931);
  case SCANNER_tag: return ((STR) &SCANNER1);
  case SE_CONTEXT_tag: return ((STR) &SE_CONTEXT1);
  case SFILE_ID_tag: return ((STR) &SFILE_ID1);
  case SIDE_EFFECT_tag: return ((STR) &SIDE_EFFECT1);
  case SIG_TBL_tag: return ((STR) &SIG_TBL1);
  case SIG_tag: return ((STR) &SIG1);
  case STAT1_tag: return ((STR) &STAT2);
  case STR_CURSOR_tag: return ((STR) &STR_CURSOR1);
  case STR_tag: return ((STR) &STR1);
  case SYSTEM_LEX_tag: return ((STR) &SYSTEM_LEX1);
  case TP_ARRAY_tag: return ((STR) &TP_ARRAY1);
  case TP_CLASS_TBL_tag: return ((STR) &TP_CLASS_TBL1);
  case TP_CLASS_tag: return ((STR) &TP_CLASS1);
  case TP_CONTEXT_tag: return ((STR) &TP_CONTEXT1);
  case TP_GEN_TBL_tag: return ((STR) &TP_GEN_TBL1);
  case TP_GRAPH_ABS_DES_tag: return ((STR) &TP_GRAPH_ABS_DES1);
  case TP_GRAPH_tag: return ((STR) &TP_GRAPH1);
  case TP_ITER_TBL_tag: return ((STR) &TP_ITER_TBL1);
  case TP_ITER_tag: return ((STR) &TP_ITER1);
  case TP_LAYOUT_tag: return ((STR) &TP_LAYOUT1);
  case TP_ROUT_TBL_tag: return ((STR) &TP_ROUT_TBL1);
  case TP_ROUT_tag: return ((STR) &TP_ROUT1);
  case TP_TBL_tag: return ((STR) &TP_TBL1);
  case TRANS_tag: return ((STR) &TRANS1);
  case TUPAM_1024994801_tag: return ((STR) &TUPAM_10249948011);
  case TUPAM_1519040877_tag: return ((STR) &TUPAM_15190408771);
  case TUPAM_304458649_tag: return ((STR) &TUPAM_3044586491);
  case TUPAM_504653531_tag: return ((STR) &TUPAM_5046535311);
  case TUPAM_653469574_tag: return ((STR) &TUPAM_6534695741);
  case TUPAM_667892060_tag: return ((STR) &TUPAM_6678920601);
  case TUPAM_927437481_tag: return ((STR) &TUPAM_9274374811);
  case TUPARR1759987776_tag: return ((STR) &TUPARR17599877761);
  case TUPARRAYARGdTP_tag: return ((STR) &TUPARRAYARGdTP1);
  case TUPBOO1392923241_tag: return ((STR) &TUPBOO13929232411);
  case TUPIDE641654124_tag: return ((STR) &TUPIDE6416541241);
  case TUPIDENTARRAYdTP_tag: return ((STR) &TUPIDENTARRAYdTP1);
  case TUPIDENTINT_tag: return ((STR) &TUPIDENTINT1);
  case TUPIDENTdTP_tag: return ((STR) &TUPIDENTdTP1);
  case TUPINTINT_tag: return ((STR) &TUPINTINT1);
  case TUPSIG1754078736_tag: return ((STR) &TUPSIG17540787361);
  case TUPSIG2023533247_tag: return ((STR) &TUPSIG20235332471);
  case TUPSIG578692189_tag: return ((STR) &TUPSIG5786921891);
  case TUPSIGARG_LAYOUT_tag: return ((STR) &TUPSIGARG_LAYOUT1);
  case TUPSIGINT_tag: return ((STR) &TUPSIGINT1);
  case TUPSIGSIG_tag: return ((STR) &TUPSIGSIG1);
  case TUPSIGSTR_tag: return ((STR) &TUPSIGSTR1);
  case TUPSTRFSETSTR_tag: return ((STR) &TUPSTRFSETSTR1);
  case TUPSTRINT_tag: return ((STR) &TUPSTRINT1);
  case TUPSTRSIG_tag: return ((STR) &TUPSTRSIG1);
  case TUPSTRSTR_tag: return ((STR) &TUPSTRSTR1);
  case TUPSTRdCONFIG_tag: return ((STR) &TUPSTRdCONFIG1);
  case TUPTP_1668454172_tag: return ((STR) &TUPTP_16684541721);
  case TUPTP_858321267_tag: return ((STR) &TUPTP_8583212671);
  case TUPdAM_EXPRdTP_tag: return ((STR) &TUPdAM_EXPRdTP1);
  case TUPdAS268679779_tag: return ((STR) &TUPdAS2686797791);
  case TUPdOBNAMESPACE_tag: return ((STR) &TUPdOBNAMESPACE1);
  case TUPdOBSTR_tag: return ((STR) &TUPdOBSTR1);
  case TUPdTP1710233051_tag: return ((STR) &TUPdTP17102330511);
  case TUPdTPCODE_FILE_tag: return ((STR) &TUPdTPCODE_FILE1);
  case TUPdTPINT_tag: return ((STR) &TUPdTPINT1);
  case TUPdTPdLAYOUT_tag: return ((STR) &TUPdTPdLAYOUT1);
  case WEIGH_CODE_tag: return ((STR) &WEIGH_CODE1);
  case XFORM_CODE_tag: return ((STR) &XFORM_CODE1);
  default: fprintf(stderr,"Internal error: unknown tag?\n");
           abort();

 }}


BOOL gen_SYS_ob_eq(OB o1,OB o2) {
 INT t1,t2;BOOL r=1;


 if (o1==o2) return TRUE;
 if (o1==NULL || o2==NULL) return FALSE;
 t1 = o1->header.tag; t2 = o2->header.tag;
 if (t1!=t2) return FALSE;
 switch (t1) {
  case BOOL_tag:
    {BOOL v1 = ((BOOL_boxed)o1)->immutable_part;
     BOOL v2 = ((BOOL_boxed)o2)->immutable_part;
     return r&&(BOOL_IS_EQ(v1,v2));}
  case CHAR_tag:
    {CHAR v1 = ((CHAR_boxed)o1)->immutable_part;
     CHAR v2 = ((CHAR_boxed)o2)->immutable_part;
     return r&&(CHAR_IS_EQ(v1,v2));}
  case FLT_tag:
    {FLT v1 = ((FLT_boxed)o1)->immutable_part;
     FLT v2 = ((FLT_boxed)o2)->immutable_part;
     return r&&(FLT_IS_EQ(v1,v2));}
  case FLTD_tag:
    {FLTD v1 = ((FLTD_boxed)o1)->immutable_part;
     FLTD v2 = ((FLTD_boxed)o2)->immutable_part;
     return r&&(FLTD_IS_EQ(v1,v2));}
  case FMT_ERROR_tag:
    {FMT_ERROR v1 = ((FMT_ERROR_boxed)o1)->immutable_part;
     FMT_ERROR v2 = ((FMT_ERROR_boxed)o2)->immutable_part;
     return r&&(INT_IS_EQ(v1.error1,v2.error1))&&SYSOBEQ(v1.str,v2.str);}
  case IDENT_tag:
    {IDENT v1 = ((IDENT_boxed)o1)->immutable_part;
     IDENT v2 = ((IDENT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.str,v2.str);}
  case INT_tag:
    {INT v1 = ((INT_boxed)o1)->immutable_part;
     INT v2 = ((INT_boxed)o2)->immutable_part;
     return r&&(INT_IS_EQ(v1,v2));}
  case RAT_tag:
    {RAT v1 = ((RAT_boxed)o1)->immutable_part;
     RAT v2 = ((RAT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.u,v2.u)&&SYSOBEQ(v1.v,v2.v);}
  case SFILE_ID_tag:
    {SFILE_ID v1 = ((SFILE_ID_boxed)o1)->immutable_part;
     SFILE_ID v2 = ((SFILE_ID_boxed)o2)->immutable_part;
     return r&&(INT_IS_EQ(v1.loc,v2.loc));}
  case TUPdAM_EXPRdTP_tag:
    {TUPdAM_EXPRdTP v1 = ((TUPdAM_EXPRdTP_boxed)o1)->immutable_part;
     TUPdAM_EXPRdTP v2 = ((TUPdAM_EXPRdTP_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdAS268679779_tag:
    {TUPdAS268679779 v1 = ((TUPdAS268679779_boxed)o1)->immutable_part;
     TUPdAS268679779 v2 = ((TUPdAS268679779_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdOBNAMESPACE_tag:
    {TUPdOBNAMESPACE v1 = ((TUPdOBNAMESPACE_boxed)o1)->immutable_part;
     TUPdOBNAMESPACE v2 = ((TUPdOBNAMESPACE_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdOBSTR_tag:
    {TUPdOBSTR v1 = ((TUPdOBSTR_boxed)o1)->immutable_part;
     TUPdOBSTR v2 = ((TUPdOBSTR_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdTPdLAYOUT_tag:
    {TUPdTPdLAYOUT v1 = ((TUPdTPdLAYOUT_boxed)o1)->immutable_part;
     TUPdTPdLAYOUT v2 = ((TUPdTPdLAYOUT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdTPCODE_FILE_tag:
    {TUPdTPCODE_FILE v1 = ((TUPdTPCODE_FILE_boxed)o1)->immutable_part;
     TUPdTPCODE_FILE v2 = ((TUPdTPCODE_FILE_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdTP1710233051_tag:
    {TUPdTP1710233051 v1 = ((TUPdTP1710233051_boxed)o1)->immutable_part;
     TUPdTP1710233051 v2 = ((TUPdTP1710233051_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPdTPINT_tag:
    {TUPdTPINT v1 = ((TUPdTPINT_boxed)o1)->immutable_part;
     TUPdTPINT v2 = ((TUPdTPINT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&(INT_IS_EQ(v1.t2,v2.t2));}
  case TUPAM_304458649_tag:
    {TUPAM_304458649 v1 = ((TUPAM_304458649_boxed)o1)->immutable_part;
     TUPAM_304458649 v2 = ((TUPAM_304458649_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPAM_667892060_tag:
    {TUPAM_667892060 v1 = ((TUPAM_667892060_boxed)o1)->immutable_part;
     TUPAM_667892060 v2 = ((TUPAM_667892060_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPAM_927437481_tag:
    {TUPAM_927437481 v1 = ((TUPAM_927437481_boxed)o1)->immutable_part;
     TUPAM_927437481 v2 = ((TUPAM_927437481_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2)&&SYSOBEQ(v1.t3,v2.t3);}
  case TUPAM_1519040877_tag:
    {TUPAM_1519040877 v1 = ((TUPAM_1519040877_boxed)o1)->immutable_part;
     TUPAM_1519040877 v2 = ((TUPAM_1519040877_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPAM_504653531_tag:
    {TUPAM_504653531 v1 = ((TUPAM_504653531_boxed)o1)->immutable_part;
     TUPAM_504653531 v2 = ((TUPAM_504653531_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPAM_1024994801_tag:
    {TUPAM_1024994801 v1 = ((TUPAM_1024994801_boxed)o1)->immutable_part;
     TUPAM_1024994801 v2 = ((TUPAM_1024994801_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPAM_653469574_tag:
    {TUPAM_653469574 v1 = ((TUPAM_653469574_boxed)o1)->immutable_part;
     TUPAM_653469574 v2 = ((TUPAM_653469574_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPARRAYARGdTP_tag:
    {TUPARRAYARGdTP v1 = ((TUPARRAYARGdTP_boxed)o1)->immutable_part;
     TUPARRAYARGdTP v2 = ((TUPARRAYARGdTP_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPARR1759987776_tag:
    {TUPARR1759987776 v1 = ((TUPARR1759987776_boxed)o1)->immutable_part;
     TUPARR1759987776 v2 = ((TUPARR1759987776_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2)&&SYSOBEQ(v1.t3,v2.t3);}
  case TUPBOO1392923241_tag:
    {TUPBOO1392923241 v1 = ((TUPBOO1392923241_boxed)o1)->immutable_part;
     TUPBOO1392923241 v2 = ((TUPBOO1392923241_boxed)o2)->immutable_part;
     return r&&(BOOL_IS_EQ(v1.t1,v2.t1))&&SYSOBEQ(v1.t2,v2.t2)&&(INT_IS_EQ(v1.t3,v2.t3))&&(BOOL_IS_EQ(v1.t4,v2.t4));}
  case TUPIDENTdTP_tag:
    {TUPIDENTdTP v1 = ((TUPIDENTdTP_boxed)o1)->immutable_part;
     TUPIDENTdTP v2 = ((TUPIDENTdTP_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1.str,v2.t1.str)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPIDE641654124_tag:
    {TUPIDE641654124 v1 = ((TUPIDE641654124_boxed)o1)->immutable_part;
     TUPIDE641654124 v2 = ((TUPIDE641654124_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1.str,v2.t1.str)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPIDENTARRAYdTP_tag:
    {TUPIDENTARRAYdTP v1 = ((TUPIDENTARRAYdTP_boxed)o1)->immutable_part;
     TUPIDENTARRAYdTP v2 = ((TUPIDENTARRAYdTP_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1.str,v2.t1.str)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPIDENTINT_tag:
    {TUPIDENTINT v1 = ((TUPIDENTINT_boxed)o1)->immutable_part;
     TUPIDENTINT v2 = ((TUPIDENTINT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1.str,v2.t1.str)&&(INT_IS_EQ(v1.t2,v2.t2));}
  case TUPINTINT_tag:
    {TUPINTINT v1 = ((TUPINTINT_boxed)o1)->immutable_part;
     TUPINTINT v2 = ((TUPINTINT_boxed)o2)->immutable_part;
     return r&&(INT_IS_EQ(v1.t1,v2.t1))&&(INT_IS_EQ(v1.t2,v2.t2));}
  case TUPSIG1754078736_tag:
    {TUPSIG1754078736 v1 = ((TUPSIG1754078736_boxed)o1)->immutable_part;
     TUPSIG1754078736 v2 = ((TUPSIG1754078736_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSIG578692189_tag:
    {TUPSIG578692189 v1 = ((TUPSIG578692189_boxed)o1)->immutable_part;
     TUPSIG578692189 v2 = ((TUPSIG578692189_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSIGARG_LAYOUT_tag:
    {TUPSIGARG_LAYOUT v1 = ((TUPSIGARG_LAYOUT_boxed)o1)->immutable_part;
     TUPSIGARG_LAYOUT v2 = ((TUPSIGARG_LAYOUT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSIG2023533247_tag:
    {TUPSIG2023533247 v1 = ((TUPSIG2023533247_boxed)o1)->immutable_part;
     TUPSIG2023533247 v2 = ((TUPSIG2023533247_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSIGINT_tag:
    {TUPSIGINT v1 = ((TUPSIGINT_boxed)o1)->immutable_part;
     TUPSIGINT v2 = ((TUPSIGINT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&(INT_IS_EQ(v1.t2,v2.t2));}
  case TUPSIGSIG_tag:
    {TUPSIGSIG v1 = ((TUPSIGSIG_boxed)o1)->immutable_part;
     TUPSIGSIG v2 = ((TUPSIGSIG_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSIGSTR_tag:
    {TUPSIGSTR v1 = ((TUPSIGSTR_boxed)o1)->immutable_part;
     TUPSIGSTR v2 = ((TUPSIGSTR_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSTRdCONFIG_tag:
    {TUPSTRdCONFIG v1 = ((TUPSTRdCONFIG_boxed)o1)->immutable_part;
     TUPSTRdCONFIG v2 = ((TUPSTRdCONFIG_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSTRFSETSTR_tag:
    {TUPSTRFSETSTR v1 = ((TUPSTRFSETSTR_boxed)o1)->immutable_part;
     TUPSTRFSETSTR v2 = ((TUPSTRFSETSTR_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSTRINT_tag:
    {TUPSTRINT v1 = ((TUPSTRINT_boxed)o1)->immutable_part;
     TUPSTRINT v2 = ((TUPSTRINT_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&(INT_IS_EQ(v1.t2,v2.t2));}
  case TUPSTRSIG_tag:
    {TUPSTRSIG v1 = ((TUPSTRSIG_boxed)o1)->immutable_part;
     TUPSTRSIG v2 = ((TUPSTRSIG_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPSTRSTR_tag:
    {TUPSTRSTR v1 = ((TUPSTRSTR_boxed)o1)->immutable_part;
     TUPSTRSTR v2 = ((TUPSTRSTR_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPTP_858321267_tag:
    {TUPTP_858321267 v1 = ((TUPTP_858321267_boxed)o1)->immutable_part;
     TUPTP_858321267 v2 = ((TUPTP_858321267_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  case TUPTP_1668454172_tag:
    {TUPTP_1668454172 v1 = ((TUPTP_1668454172_boxed)o1)->immutable_part;
     TUPTP_1668454172 v2 = ((TUPTP_1668454172_boxed)o2)->immutable_part;
     return r&&SYSOBEQ(v1.t1,v2.t1)&&SYSOBEQ(v1.t2,v2.t2);}
  default: return FALSE;
   }}


