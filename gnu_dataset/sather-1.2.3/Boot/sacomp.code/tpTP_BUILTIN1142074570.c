#include "sather.h"

/* Layouts */

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

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

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYd2147017943_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd2147017943_frame;

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

typedef struct ARRAYTP_CLASS_struct {/* layout for ARRAY{TP_CLASS} */
 OB_HEADER header;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *ARRAYTP_CLASS;

typedef struct CALL_T666389079_struct {/* layout for CALL_TP_UNDERSCORE */
 OB_HEADER header;
 struct dTP_struct *tp;
 } *CALL_T666389079;

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct ELT_EQTP_CLASS_struct {/* layout for ELT_EQ{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQTP_CLASS;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

typedef struct ELT_NI1575801776_struct {/* layout for ELT_NIL{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1575801776;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

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

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

typedef struct MODE_struct {/* layout for MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *MODE;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

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

typedef struct TP_BUILTIN_struct {/* layout for TP_BUILTIN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *TP_BUILTIN;

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

typedef struct TP_CLA83919057_frame_struct {
 INT state;
 TP_CLASS_TBL self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_CLA83919057_frame;

typedef struct TP_CLA1636324039_frame_struct {
 INT state;
 TP_CLASS_TBL self;/* Formal argument: self */
 TP_CLASS arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_CLA1636324039_frame;

typedef struct TP_CLA115798516_frame_struct {
 INT state;
 TP_CLASS_TBL self;/* Formal argument: self */
 TP_CLASS ret_val2;
 TP_CLASS_TBL L61;
 TP_CLASS r;
 INT L31;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_CLA115798516_frame;

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

typedef struct TP_GEN798653528_frame_struct {
 INT state;
 TP_GEN_TBL self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_GEN798653528_frame;

typedef struct TP_GEN1943908786_frame_struct {
 INT state;
 TP_GEN_TBL self;/* Formal argument: self */
 TP_CLASS arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_GEN1943908786_frame;

typedef struct TP_GEN1057619759_frame_struct {
 INT state;
 TP_GEN_TBL self;/* Formal argument: self */
 TP_CLASS ret_val2;
 TP_GEN_TBL L61;
 TP_CLASS r;
 INT L31;
 TP_GEN_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_GEN1057619759_frame;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYd1897420416_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1897420416_frame;

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct ARRAYB741586158_frame_struct {
 INT state;
 ARRAYBOOL self;/* Formal argument: self */
 BOOL arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYB741586158_frame;

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

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

typedef struct ARRAYT1851819274_frame_struct {
 INT state;
 ARRAYTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1851819274_frame;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

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

typedef struct AS_PARAM_DEC_struct {/* layout for AS_PARAM_DEC */
 OB_HEADER header;
 struct AS_PARAM_DEC_struct *next;
 struct AS_TYPE_SPEC_struct *type_constraint;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_PARAM_DEC;

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

typedef struct FMAPTP775297600_struct {/* layout for FMAP{TP_CLASS,FSET{TP_CLASS}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *FMAPTP775297600;

typedef struct FMAPTP1065055386_frame_struct {
 INT state;
 FMAPTP775297600 self;/* Formal argument: self */
 TUPTP_1668454172 ret_val2;
 FMAPTP775297600 L61;
 TUPTP_1668454172 r;
 INT L31;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPTP1065055386_frame;

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

typedef struct FSETTUPIDENTINT_struct {/* layout for FSET{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FSETTUPIDENTINT;

typedef struct FSETTU947212086_frame_struct {
 INT state;
 FSETTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT ret_val2;
 FSETTUPIDENTINT L61;
 TUPIDENTINT r;
 INT i;
 INT sz;
 INT L31;
 FSETTUPIDENTINT is_elt_nil_self;
 TUPIDENTINT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1575801776 is_elt_nil_self1;
 TUPIDENTINT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTU947212086_frame;

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

extern ARRAYTP_CLASS TP_BUI1236844942;
extern ARRAYTP_CLASS TP_BUI1236844961;
extern ARRAYTP_CLASS TP_BUI1517259338;
extern IDENT IDENT_1280657180;
extern IDENT IDENT_265373817;
extern IDENT IDENT_386785441;
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
extern INT AS_TYP1853679690;
extern INT AS_TYPE_SPEC_ord;
extern INT TP_CLA1631459432;
extern INT TP_GEN180842239;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_ext_c_tp;
extern INT TP_KIND_part_tp;
extern INT TP_KIND_val_tp;
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
extern TP_CLASS TP_BUILTIN_fstr;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_inti;
extern TP_CLASS TP_BUILTIN_str;
extern TP_CLASS TP_BUILTIN_sys;
extern TP_CLASS TP_BUILTIN_zone;
extern TP_ROUT TP_BUILTIN_rout;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST AS_TYPE_SPEC (**dTP_as1220129267)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is33989660)(dTP);

extern RETURNED_CONST STR (**dCALL_TP_strrSTR)(dCALL_TP);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ARRAYIDENT TP_CLA2131361672(TP_CLASS);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
AS_TYPE_SPEC TP_CLA981298431(TP_CLASS);
BOOL ARRAYB651176924(ARRAYBOOL, BOOL);
BOOL ARRAYI801160309(ARRAYIDENT, IDENT);
BOOL FSETTP571570007(FSETTP_CLASS, TP_CLASS);
BOOL FSETTU645397512(FSETTUPIDENTINT, TUPIDENTINT);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL IMPL_i932377427(IMPL);
BOOL STR_is111530248(STR, STR);
BOOL STR_is171745744(STR, STR);
BOOL TP_CLA1015994406(TP_CLASS);
BOOL TP_CLA1071798668(TP_CLASS);
BOOL TP_CLA1153639400(TP_CLASS);
BOOL TP_CLA1448883828(TP_CLASS);
BOOL TP_CLA1568240525(TP_CLASS);
BOOL TP_CLA1792166754(TP_CLASS);
BOOL TP_CLA254882760(TP_CLASS, OB);
BOOL TP_CLA272820496(TP_CLASS);
BOOL TP_CLA344221540(TP_CLASS_TBL, TP_CLASS);
BOOL TP_CLA366982046(TP_CLASS);
BOOL TP_CLA430625769(TP_CLASS);
BOOL TP_CLA513015767(TP_CLASS, dCALL_TP);
BOOL TP_CLA650455767(TP_CLASS);
BOOL TP_CLA657911258(TP_CLASS_TBL, TUPIDENTARRAYdTP, TP_CLASS);
BOOL TP_CLA693899249(TP_CLASS, dCALL_TP);
BOOL TP_CLA727826289(TP_CLASS);
BOOL TP_CON1882075561(TP_CONTEXT, AS_TYPE_SPEC);
BOOL TP_CON21826358(TP_CONTEXT, AS_TYPE_SPEC);
BOOL TP_CON282785865(TP_CONTEXT, AS_TYPE_SPEC);
BOOL TP_GEN1082329842(TP_GEN_TBL, TUPIDENTINT, TP_CLASS);
BOOL TP_GRA927228682(TP_GRAPH, dTP, TP_CLASS);
CONFIG_DEF CONFIG1152920950(CONFIG_TBL, STR);
FMAPTP291739594 FMAPTP551118428(FMAPTP291739594, TP_CLASS, FSETdTP);
FMAPTP775297600 FMAPTP594824795(FMAPTP775297600, TP_CLASS, FSETTP_CLASS);
FSETTP_CLASS FMAPTP2096448595(FMAPTP775297600, TP_CLASS);
FSETTP_CLASS FSETTP1186813482(FSETTP_CLASS, TP_CLASS);
FSETTP_CLASS FSETTP633280235(FSETTP_CLASS, FSETTP_CLASS);
FSETTP_CLASS TP_GRA583006971(TP_GRAPH, TP_CLASS);
FSETTUPIDENTINT FSETTU1559268184(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU2045052674(FSETTUPIDENTINT, TUPIDENTINT);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSETdTP FSETdT575601071(FSETdTP, FSETdTP);
FSETdTP TP_GRA2060761681(TP_GRAPH, TP_CLASS);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IDENT IDENT_1150413730(IDENT, STR);
IFC IFC_if331163657(IFC, TP_CLASS);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
IMPL IMPL_C629829570(IMPL_CREATE, TP_CLASS);
IMPL TP_CLA1109727233(TP_CLASS);
INT ARRAYI789953351(ARRAYI789953351_frame);
INT ARRAYd1897420416(ARRAYd1897420416_frame);
INT AS_PAR401840644(AS_PARAM_DEC);
INT AS_TYP2145685956(AS_TYPE_SPEC);
INT INT_hashrINT(INT);
INT INT_upbrINT(INT_upbrINT_frame);
INT TP_CLA1199143806(TP_CLASS_TBL, TUPIDENTARRAYdTP);
INT TP_CLA1206273019(TP_CLASS_TBL, TP_CLASS);
INT TP_CLA1653846135(TP_CLASS);
INT TP_CLA619166775(TP_CLASS);
PROG TP_CLA736641047(TP_CLASS);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR IDENT_1415966123(IDENT, INT);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_CON702863562(TP_CONTEXT);
TP_CLASS ARRAYT1851819274(ARRAYT1851819274_frame);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TP_CLASS TP_CLA115798516(TP_CLA115798516_frame);
TP_CLASS TP_CLA1710809725(TP_CLASS, IDENT, ARRAYdTP, PROG);
TP_CLASS TP_CLA420352644(TP_CLASS_TBL, TUPIDENTARRAYdTP);
TP_CLASS TP_CLA83919057(TP_CLA83919057_frame);
TP_CLASS TP_GEN1057619759(TP_GEN1057619759_frame);
TP_CLASS TP_GEN1932307144(TP_GEN_TBL, TUPIDENTINT);
TP_CLASS TP_GEN798653528(TP_GEN798653528_frame);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
TP_CLASS_TBL TP_CLA1085505433(TP_CLASS_TBL, INT);
TP_CLASS_TBL TP_CLA307403124(TP_CLASS_TBL);
TP_CLASS_TBL TP_CLA426779342(TP_CLASS_TBL, TP_CLASS);
TP_CONTEXT TP_CLA821242177(TP_CLASS);
TP_GEN_TBL TP_GEN1111892401(TP_GEN_TBL, INT);
TP_GEN_TBL TP_GEN1128603126(TP_GEN_TBL);
TP_GEN_TBL TP_GEN1370131280(TP_GEN_TBL, TP_CLASS);
TP_ITER TP_ITE1350567874(TP_ITER, ARRAYARG, ARRAYBOOL, dTP, PROG);
TP_ITER TP_ITE990128539(TP_ITER_TBL, TUPARR1759987776);
TP_ITER_TBL TP_ITE992470229(TP_ITER_TBL, TP_ITER);
TP_ROUT TP_ROU52430373(TP_ROUT, ARRAYARG, dTP, PROG);
TP_ROUT TP_ROU581494640(TP_ROUT_TBL, TUPARRAYARGdTP);
TP_ROUT_TBL TP_ROU1247655203(TP_ROUT_TBL, TP_ROUT);
TUPIDENTINT FSETTU947212086(FSETTU947212086_frame);
TUPTP_1668454172 FMAPTP1065055386(FMAPTP1065055386_frame);
TUPTP_1668454172 FMAPTP1108250174(FMAPTP775297600, TP_CLASS);
TUPTP_858321267 FMAPTP1465407463(FMAPTP1465407463_frame);
TUPTP_858321267 FMAPTP1881680767(FMAPTP291739594, TP_CLASS);
dMODE MODE_c998271775(MODE, AS_ARG_MODE);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
dTP TP_CON1231573657(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON1705076411(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON1795722259(TP_CONTEXT, AS_TYPE_SPEC, BOOL);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON2076339407(TP_CONTEXT, AS_TYPE_SPEC, BOOL);
dTP TP_CON2124638015(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON3366446(TP_CONTEXT, IDENT);
void ARRAYB741586158(ARRAYB741586158_frame);
void ARRAYI1177186037(ARRAYI1177186037_frame);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void AS_TYP1027254244(AS_TYPE_SPEC, AS_TYPE_SPEC);
void FILE_plus_dSTR(FILE1, dSTR);
void FIND_G1798674529(FIND_GENERICS, TP_CLASS);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void TP_BUI113997099(TP_BUILTIN, TP_CLASS, ARRAYTP_CLASS);
void TP_BUI670024966(TP_BUILTIN, TP_TBL, PROG);
void TP_BUI717287368(TP_BUILTIN, IDENT, TP_CLASS, ARRAYdTP);
void TP_CLA1636324039(TP_CLA1636324039_frame);
void TP_GEN1943908786(TP_GEN1943908786_frame);
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

ARRAYIDENT TP_CLA2131361672(TP_CLASS self) {
 ARRAYIDENT ret_val;
 AS_CLASS_DEF as;
 ARRAYIDENT r;
 AS_PARAM_DEC pd;
 INT i = INT_zero;
 ARRAYIDENT L11;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val1;
 ARRAYIDENT create_self;
 INT create_n = INT_zero;
 ARRAYIDENT ret_val2;
 dPARSE L2;
 IDENT L3;
 ARRAYdTP L4;
 INT L51_;
 dPARSE L6;
 ARRAYIDENT L7;
 INT L8;
 OB L9;
 INT L10;
 INT L121_br;
INT i_L121_=0;
 IDENT L13;
 as_tree_self = self;
 if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
  L2 = ATTR(ATTR(as_tree_self,prog),parse);
  L3 = ATTR(as_tree_self,name1);
  L4 = ATTR(as_tree_self,params);
  L51_=(L4)==NULL?0:ASIZE((ARRAYdTP)L4); 
  L6 = L2;
  SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L6)])(L6, L3, L51_));
 }
 ret_val1 = ATTR(as_tree_self,my_as_tree);
 as = ret_val1;
 if ((as==((AS_CLASS_DEF) NULL))) {
  ret_val = ((ARRAYIDENT) NULL);
  return ret_val;
 }
 if ((ATTR(as,params)==((AS_PARAM_DEC) NULL))) {
  ret_val = ((ARRAYIDENT) NULL);
  return ret_val;
 }
 create_self = ((ARRAYIDENT) NULL);
 create_n = AS_PAR401840644(ATTR(as,params));
 L8 = create_n;
 L10=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L8)*sizeof(IDENT);
 L9=ZALLOC_BIG(L10);
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

   ((OB)L9)->header.destroyed=0;
#endif

 L7 = ((ARRAYIDENT) L9);
 L7->asize = L8;
 ret_val2 = L7;
 r = ret_val2;
 pd = ATTR(as,params);
 i = 0;
 {
  BOOL f_L121_ = TRUE;
  L11 = r;
  L121_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
  i_L121_=0;
  while (1) {
   if ((pd==((AS_PARAM_DEC) NULL))) {
    goto after_loop;
   }
   if(i_L121_>=L121_br)  goto after_loop; 
   L13 = ATTR(pd,name1);
   SARR((ARRAYIDENT)L11,i_L121_,L13); i_L121_++;
   ;
   pd = ATTR(pd,next);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC TP_CLA981298431(TP_CLASS self) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC ret;
 AS_TYPE_SPEC asp = ((AS_TYPE_SPEC) NULL);
 ARRAYdTP L11;
 dTP paramtp;
 AS_TYPE_SPEC as0;
 AS_TYPE_SPEC create_self;
 AS_TYPE_SPEC ret_val1;
 INT L21 = INT_zero;
 AS_TYPE_SPEC L3;
 OB L4;
 INT L51_br;
 dTP aL51_;
 dTP L6;
 dTP L7;
 INT L81_;
 create_self = ((AS_TYPE_SPEC) NULL);
 L4=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_TYPE_SPEC_tag;
 L3 = ((AS_TYPE_SPEC) L4);
 ret_val1 = L3;
 ret = ret_val1;
 SATTR(ret,name1,ATTR(self,name1));
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,params);
  L51_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
  while (1) {
   if(L21>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYdTP)L11,L21); 
   paramtp = aL51_;
   L7 = paramtp;
   as0 = (*dTP_as1220129267[TAG(L7)])(L7);
   if ((asp==((AS_TYPE_SPEC) NULL))) {
    asp = as0;
   }
   else {
    AS_TYP1027254244(asp, as0);
   }
   L81_=INTPLUS(L21,1); 
   L21 = L81_;
  }
 }
 after_loop: ;
 SATTR(ret,params,asp);
 ret_val = ret;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1015994406(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
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
 INT L11;
 INT L12;
 BOOL L131_;
 kind_self = self;
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
 L11 = ret_val1;
 L12 = TP_KIND_val_tp;
 L131_=(L11)==(L12); 
 ret_val = L131_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1071798668(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1153639400(TP_CLASS self) {
 return ATTR(self,is_builtin);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1448883828(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 if ((self==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = IMPL_i932377427(TP_CLA1109727233(self));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1568240525(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 STR L1;
 CHAR L21_;
 CHAR L3;
 BOOL L41_;
 if ((self==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = ATTR(self,name1).str;
 L21_=ARR((STR)L1,0); 
 L3 = L21_;
 L41_=L3=='$'; 
 ret_val = L41_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA1792166754(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
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
 INT L11;
 INT L12;
 BOOL L131_;
 kind_self = self;
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
 L11 = ret_val1;
 L12 = TP_KIND_part_tp;
 L131_=(L11)==(L12); 
 ret_val = L131_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA254882760(TP_CLASS self, OB t) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),t);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA272820496(TP_CLASS self) {
 return ATTR(self,is_generic);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA344221540(TP_CLASS_TBL self, TP_CLASS e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 TP_CLASS te;
 TP_CLASS te1;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS_TBL elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS_TBL is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 TP_CLASS_TBL elt_eq_self2;
 TP_CLASS elt_eq_e12;
 TP_CLASS elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self3;
 TP_CLASS elt_eq_e13;
 TP_CLASS elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val10 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_CLASS L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_CLASS L141_;
 INT L151_;
 if ((self==((TP_CLASS_TBL) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = TP_CLA1206273019(self, e);
 L21_=ASIZE((TP_CLASS_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_CLASS)ARR((TP_CLASS_TBL)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_CLASS) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val5 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val4 = ret_val5;
   ret_val3 = ret_val4;
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_CLASS_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_CLASS)ARR((TP_CLASS_TBL)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((TP_CLASS) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val10 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val9 = ret_val10;
    ret_val8 = ret_val9;
    if (ret_val8) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA366982046(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 TP_CLASS kind_self1;
 INT ret_val3 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val4;
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
 kind_self = self;
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
  ret_val2 = ATTR(as_tree_self,my_as_tree);
  L10 = (ret_val2==((AS_CLASS_DEF) NULL));
  L111_=!(L10); 
  if (L111_) {
   SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
  }
 }
 ret_val1 = ATTR(kind_self,kind_cache);
 L12 = ret_val1;
 L13 = TP_KIND_ext_c_tp;
 L141_=(L12)==(L13); 
 if (L141_) {
  L1 = TRUE;
 } else {
  kind_self1 = self;
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
   ret_val4 = ATTR(as_tree_self1,my_as_tree);
   L23 = (ret_val4==((AS_CLASS_DEF) NULL));
   L241_=!(L23); 
   if (L241_) {
    SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
   }
  }
  ret_val3 = ATTR(kind_self1,kind_cache);
  L25 = ret_val3;
  L26 = TP_KIN462481068;
  L271_=(L25)==(L26); 
  L1 = L271_;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA430625769(TP_CLASS self) {
 return ATTR(self,is_atomic);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA513015767(TP_CLASS self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 if ((self==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = self;
 is_eq_t = ((OB) t);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  if (t==NULL) {
   goto other118;
  } else
  switch (TAG(t)) {
   case CALL_T666389079_tag:
    L1 = (ATTR(((CALL_T666389079) t),tp)==((dTP) NULL));
    L21_=!(L1); 
    if (L21_) {
     ret_val = TP_CLA513015767(self, ((dCALL_TP) ATTR(((CALL_T666389079) t),tp)));
     return ret_val;
    }
    else {
     ret_val = TRUE;
     return ret_val;
    } break;
   case TP_CLASS_tag:
    if (TP_CLA1568240525(((TP_CLASS) t))) {
     ret_val = TP_GRA927228682(ATTR(ATTR(self,prog),tp_graph), ((dTP) self), ((TP_CLASS) t));
     return ret_val;
    }
    else {
     ret_val = FALSE;
     return ret_val;
    } break;
   default: ;
   other118: ;
    ret_val = FALSE;
    return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA650455767(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS L11;
 TP_CLASS tp;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self2;
 OB is_eq_t2;
 BOOL ret_val3 = BOOL_zero;
 TP_CLASS is_eq_self3;
 OB is_eq_t3;
 BOOL ret_val4 = BOOL_zero;
 TP_CLASS is_eq_self4;
 OB is_eq_t4;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self5;
 OB is_eq_t5;
 BOOL ret_val6 = BOOL_zero;
 TP_CLASS is_eq_self6;
 OB is_eq_t6;
 BOOL ret_val7 = BOOL_zero;
 TP_CLASS is_eq_self7;
 OB is_eq_t7;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS is_eq_self8;
 OB is_eq_t8;
 BOOL ret_val9 = BOOL_zero;
 IDENT is_eq_self9 = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_eq_self10 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 IDENT is_eq_self11 = IDENT_zero;
 IDENT is_eq_i2 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 BOOL L2;
 BOOL L3;
 L11 = self;
 is_eq_self = L11;
 is_eq_t = ((OB) TP_BUI403444172);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  is_eq_self1 = L11;
  is_eq_t1 = ((OB) TP_BUI374366290);
  ret_val2 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   is_eq_self2 = L11;
   is_eq_t2 = ((OB) TP_BUI1805763264);
   ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    is_eq_self3 = L11;
    is_eq_t3 = ((OB) TP_BUI1194559023);
    ret_val4 = SYSOBEQ(((OB) is_eq_self3),is_eq_t3);
    if (ret_val4) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     is_eq_self4 = L11;
     is_eq_t4 = ((OB) TP_BUI1370948386);
     ret_val5 = SYSOBEQ(((OB) is_eq_self4),is_eq_t4);
     if (ret_val5) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      is_eq_self5 = L11;
      is_eq_t5 = ((OB) TP_BUI633637838);
      ret_val6 = SYSOBEQ(((OB) is_eq_self5),is_eq_t5);
      if (ret_val6) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_eq_self6 = L11;
       is_eq_t6 = ((OB) TP_BUI1032249067);
       ret_val7 = SYSOBEQ(((OB) is_eq_self6),is_eq_t6);
       if (ret_val7) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        is_eq_self7 = L11;
        is_eq_t7 = ((OB) TP_BUI2088674906);
        ret_val8 = SYSOBEQ(((OB) is_eq_self7),is_eq_t7);
        if (ret_val8) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         is_eq_self8 = L11;
         is_eq_t8 = ((OB) TP_BUI1116976640);
         ret_val9 = SYSOBEQ(((OB) is_eq_self8),is_eq_t8);
         if (ret_val9) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          tp = self;
          is_eq_self9 = ATTR(tp,name1);
          is_eq_i = IDENT_265373817;
          ret_val10 = (is_eq_self9.str==is_eq_i.str);
          if (ret_val10) {
           L3 = TRUE;
          } else {
           is_eq_self10 = ATTR(tp,name1);
           is_eq_i1 = IDENT_1280657180;
           ret_val11 = (is_eq_self10.str==is_eq_i1.str);
           L3 = ret_val11;
          }
          if (L3) {
           L2 = TRUE;
          } else {
           is_eq_self11 = ATTR(tp,name1);
           is_eq_i2 = IDENT_386785441;
           ret_val12 = (is_eq_self11.str==is_eq_i2.str);
           L2 = ret_val12;
          }
          if (L2) {
           ret_val = TRUE;
           return ret_val;
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
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA657911258(TP_CLASS_TBL self, TUPIDENTARRAYdTP q, TP_CLASS t) {
 BOOL ret_val = BOOL_zero;
 ARRAYdTP L11;
 ARRAYdTP L21;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 INT L31 = INT_zero;
 BOOL L4;
 BOOL L51_;
 ARRAYdTP L6;
 INT L71_;
 INT L8;
 ARRAYdTP L9;
 INT L101_;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 INT L161_br;
 dTP aL161_;
 INT L171_br;
 dTP aL171_;
 dTP L18;
 dTP L19;
 dTP L20;
 dTP L22;
 BOOL L23;
 BOOL L241_;
 INT L251_;
 if ((t==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = q.t1;
 is_eq_i = ATTR(t,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L4 = ret_val1;
 L51_=!(L4); 
 if (L51_) {
  ret_val = FALSE;
  return ret_val;
 }
 L6 = q.t2;
 L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
 L8 = L71_;
 L9 = ATTR(t,params);
 L101_=(L9)==NULL?0:ASIZE((ARRAYdTP)L9); 
 L12 = L101_;
 L131_=(L8)==(L12); 
 L14 = L131_;
 L151_=!(L14); 
 if (L151_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L161_ = TRUE;
  BOOL f_L171_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = q.t2;
  L21 = ATTRs(t,params,((ARRAYdTP) NULL));
  L161_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
  L171_br=L21==NULL?0:ASIZE((ARRAYdTP)L21); 
  while (1) {
   if(L31>=L161_br)  goto after_loop; 
   aL161_=ARR((ARRAYdTP)L11,L31); 
   L19 = aL161_;
   if(L31>=L171_br)  goto after_loop; 
   aL171_=ARR((ARRAYdTP)L21,L31); 
   L22 = L19;
   L23 = (*dTP_is242312711[TAG(L22)])(L22, ((OB) aL171_));
   L241_=!(L23); 
   if (L241_) {
    ret_val = FALSE;
    return ret_val;
   }
   L251_=INTPLUS(L31,1); 
   L31 = L251_;
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA693899249(TP_CLASS self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 STR L1;
 dCALL_TP L2;
 L1 = TP_CLASS_strrSTR(self);
 L2 = t;
 ret_val = STR_is171745744(L1, (*dCALL_TP_strrSTR[TAG(L2)])(L2));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CLA727826289(TP_CLASS self) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS L11;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self2;
 OB is_eq_t2;
 BOOL ret_val3 = BOOL_zero;
 TP_CLASS is_eq_self3;
 OB is_eq_t3;
 BOOL ret_val4 = BOOL_zero;
 TP_CLASS is_eq_self4;
 OB is_eq_t4;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self5;
 OB is_eq_t5;
 BOOL ret_val6 = BOOL_zero;
 TP_CLASS is_eq_self6;
 OB is_eq_t6;
 BOOL ret_val7 = BOOL_zero;
 TP_CLASS is_eq_self7;
 OB is_eq_t7;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS is_eq_self8;
 OB is_eq_t8;
 BOOL ret_val9 = BOOL_zero;
 TP_CLASS is_eq_self9;
 OB is_eq_t9;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS is_eq_self10;
 OB is_eq_t10;
 BOOL ret_val11 = BOOL_zero;
 TP_CLASS is_eq_self11;
 OB is_eq_t11;
 BOOL ret_val12 = BOOL_zero;
 TP_CLASS is_eq_self12;
 OB is_eq_t12;
 BOOL ret_val13 = BOOL_zero;
 TP_CLASS is_eq_self13;
 OB is_eq_t13;
 BOOL ret_val14 = BOOL_zero;
 TP_CLASS is_eq_self14;
 OB is_eq_t14;
 BOOL ret_val15 = BOOL_zero;
 TP_CLASS is_eq_self15;
 OB is_eq_t15;
 BOOL ret_val16 = BOOL_zero;
 TP_CLASS is_eq_self16;
 OB is_eq_t16;
 BOOL ret_val17 = BOOL_zero;
 TP_CLASS is_eq_self17;
 OB is_eq_t17;
 BOOL ret_val18 = BOOL_zero;
 TP_CLASS is_eq_self18;
 OB is_eq_t18;
 BOOL ret_val19 = BOOL_zero;
 TP_CLASS is_eq_self19;
 OB is_eq_t19;
 BOOL ret_val20 = BOOL_zero;
 TP_CLASS is_eq_self20;
 OB is_eq_t20;
 BOOL ret_val21 = BOOL_zero;
 TP_CLASS is_eq_self21;
 OB is_eq_t21;
 BOOL ret_val22 = BOOL_zero;
 TP_CLASS is_eq_self22;
 OB is_eq_t22;
 BOOL ret_val23 = BOOL_zero;
 TP_CLASS is_eq_self23;
 OB is_eq_t23;
 BOOL ret_val24 = BOOL_zero;
 TP_CLASS is_eq_self24;
 OB is_eq_t24;
 BOOL ret_val25 = BOOL_zero;
 TP_CLASS is_eq_self25;
 OB is_eq_t25;
 BOOL ret_val26 = BOOL_zero;
 TP_CLASS is_eq_self26;
 OB is_eq_t26;
 BOOL ret_val27 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val28 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val29;
 INT L2;
 INT L3;
 BOOL L41_;
 dPARSE L5;
 IDENT L6;
 ARRAYdTP L7;
 INT L81_;
 dPARSE L9;
 BOOL L10;
 BOOL L121_;
 INT L13;
 INT L14;
 BOOL L151_;
 L11 = self;
 is_eq_self = L11;
 is_eq_t = ((OB) TP_BUI231294523);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  is_eq_self1 = L11;
  is_eq_t1 = ((OB) TP_BUI1769472218);
  ret_val2 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   is_eq_self2 = L11;
   is_eq_t2 = ((OB) TP_BUI274781170);
   ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    is_eq_self3 = L11;
    is_eq_t3 = ((OB) TP_BUI139344778);
    ret_val4 = SYSOBEQ(((OB) is_eq_self3),is_eq_t3);
    if (ret_val4) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     is_eq_self4 = L11;
     is_eq_t4 = ((OB) TP_BUILTIN_c_int);
     ret_val5 = SYSOBEQ(((OB) is_eq_self4),is_eq_t4);
     if (ret_val5) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      is_eq_self5 = L11;
      is_eq_t5 = ((OB) TP_BUI232519909);
      ret_val6 = SYSOBEQ(((OB) is_eq_self5),is_eq_t5);
      if (ret_val6) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_eq_self6 = L11;
       is_eq_t6 = ((OB) TP_BUI997290216);
       ret_val7 = SYSOBEQ(((OB) is_eq_self6),is_eq_t6);
       if (ret_val7) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        is_eq_self7 = L11;
        is_eq_t7 = ((OB) TP_BUI779482534);
        ret_val8 = SYSOBEQ(((OB) is_eq_self7),is_eq_t7);
        if (ret_val8) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         is_eq_self8 = L11;
         is_eq_t8 = ((OB) TP_BUI1768246832);
         ret_val9 = SYSOBEQ(((OB) is_eq_self8),is_eq_t8);
         if (ret_val9) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          is_eq_self9 = L11;
          is_eq_t9 = ((OB) TP_BUI107670638);
          ret_val10 = SYSOBEQ(((OB) is_eq_self9),is_eq_t9);
          if (ret_val10) {
           ret_val = TRUE;
           return ret_val;
          }
          else {
           is_eq_self10 = L11;
           is_eq_t10 = ((OB) TP_BUI1959866815);
           ret_val11 = SYSOBEQ(((OB) is_eq_self10),is_eq_t10);
           if (ret_val11) {
            ret_val = TRUE;
            return ret_val;
           }
           else {
            is_eq_self11 = L11;
            is_eq_t11 = ((OB) TP_BUI1825364383);
            ret_val12 = SYSOBEQ(((OB) is_eq_self11),is_eq_t11);
            if (ret_val12) {
             ret_val = TRUE;
             return ret_val;
            }
            else {
             is_eq_self12 = L11;
             is_eq_t12 = ((OB) TP_BUILTIN_c_ptr);
             ret_val13 = SYSOBEQ(((OB) is_eq_self12),is_eq_t12);
             if (ret_val13) {
              ret_val = TRUE;
              return ret_val;
             }
             else {
              is_eq_self13 = L11;
              is_eq_t13 = ((OB) TP_BUI468587458);
              ret_val14 = SYSOBEQ(((OB) is_eq_self13),is_eq_t13);
              if (ret_val14) {
               ret_val = TRUE;
               return ret_val;
              }
              else {
               is_eq_self14 = L11;
               is_eq_t14 = ((OB) TP_BUI1716944501);
               ret_val15 = SYSOBEQ(((OB) is_eq_self14),is_eq_t14);
               if (ret_val15) {
                ret_val = TRUE;
                return ret_val;
               }
               else {
                is_eq_self15 = L11;
                is_eq_t15 = ((OB) TP_BUI1696960301);
                ret_val16 = SYSOBEQ(((OB) is_eq_self15),is_eq_t15);
                if (ret_val16) {
                 ret_val = TRUE;
                 return ret_val;
                }
                else {
                 is_eq_self16 = L11;
                 is_eq_t16 = ((OB) TP_BUI444762713);
                 ret_val17 = SYSOBEQ(((OB) is_eq_self16),is_eq_t16);
                 if (ret_val17) {
                  ret_val = TRUE;
                  return ret_val;
                 }
                 else {
                  is_eq_self17 = L11;
                  is_eq_t17 = ((OB) TP_BUI1292722345);
                  ret_val18 = SYSOBEQ(((OB) is_eq_self17),is_eq_t17);
                  if (ret_val18) {
                   ret_val = TRUE;
                   return ret_val;
                  }
                  else {
                   is_eq_self18 = L11;
                   is_eq_t18 = ((OB) TP_BUI1248326412);
                   ret_val19 = SYSOBEQ(((OB) is_eq_self18),is_eq_t18);
                   if (ret_val19) {
                    ret_val = TRUE;
                    return ret_val;
                   }
                   else {
                    is_eq_self19 = L11;
                    is_eq_t19 = ((OB) TP_BUI1606257075);
                    ret_val20 = SYSOBEQ(((OB) is_eq_self19),is_eq_t19);
                    if (ret_val20) {
                     ret_val = TRUE;
                     return ret_val;
                    }
                    else {
                     is_eq_self20 = L11;
                     is_eq_t20 = ((OB) TP_BUI2131222993);
                     ret_val21 = SYSOBEQ(((OB) is_eq_self20),is_eq_t20);
                     if (ret_val21) {
                      ret_val = TRUE;
                      return ret_val;
                     }
                     else {
                      is_eq_self21 = L11;
                      is_eq_t21 = ((OB) TP_BUI1798283841);
                      ret_val22 = SYSOBEQ(((OB) is_eq_self21),is_eq_t21);
                      if (ret_val22) {
                       ret_val = TRUE;
                       return ret_val;
                      }
                      else {
                       is_eq_self22 = L11;
                       is_eq_t22 = ((OB) TP_BUI102735229);
                       ret_val23 = SYSOBEQ(((OB) is_eq_self22),is_eq_t22);
                       if (ret_val23) {
                        ret_val = TRUE;
                        return ret_val;
                       }
                       else {
                        is_eq_self23 = L11;
                        is_eq_t23 = ((OB) TP_BUI1296146890);
                        ret_val24 = SYSOBEQ(((OB) is_eq_self23),is_eq_t23);
                        if (ret_val24) {
                         ret_val = TRUE;
                         return ret_val;
                        }
                        else {
                         is_eq_self24 = L11;
                         is_eq_t24 = ((OB) TP_BUI2025364786);
                         ret_val25 = SYSOBEQ(((OB) is_eq_self24),is_eq_t24);
                         if (ret_val25) {
                          ret_val = TRUE;
                          return ret_val;
                         }
                         else {
                          is_eq_self25 = L11;
                          is_eq_t25 = ((OB) TP_BUI1643601086);
                          ret_val26 = SYSOBEQ(((OB) is_eq_self25),is_eq_t25);
                          if (ret_val26) {
                           ret_val = TRUE;
                           return ret_val;
                          }
                          else {
                           is_eq_self26 = L11;
                           is_eq_t26 = ((OB) TP_BUI1609790832);
                           ret_val27 = SYSOBEQ(((OB) is_eq_self26),is_eq_t26);
                           if (ret_val27) {
                            ret_val = TRUE;
                            return ret_val;
                           }
                           else {
                            kind_self = self;
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
                             ret_val29 = ATTR(as_tree_self,my_as_tree);
                             L10 = (ret_val29==((AS_CLASS_DEF) NULL));
                             L121_=!(L10); 
                             if (L121_) {
                              SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
                             }
                            }
                            ret_val28 = ATTR(kind_self,kind_cache);
                            L13 = ret_val28;
                            L14 = TP_KIND_ext_c_tp;
                            L151_=(L13)==(L14); 
                            ret_val = L151_;
                            return ret_val;
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
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CON1882075561(TP_CONTEXT self, AS_TYPE_SPEC t) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 L2 = ATTR(t,kind);
 L3 = AS_TYPE_SPEC_ord;
 L41_=(L2)==(L3); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L1 = TRUE;
 } else {
  L7 = (ATTR(t,params)==((AS_TYPE_SPEC) NULL));
  L81_=!(L7); 
  L1 = L81_;
 }
 if (L1) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = ARRAYI801160309(ATTR(self,pnames), ATTR(t,name1));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CON21826358(TP_CONTEXT self, AS_TYPE_SPEC t) {
 BOOL ret_val = BOOL_zero;
 INT L1;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 L1 = ATTR(t,kind);
 L2 = AS_TYP1853679690;
 L31_=(L1)==(L2); 
 if (L31_) {
  ret_val = TRUE;
  return ret_val;
 }
 if (TP_CON282785865(self, t)) {
  ret_val = TRUE;
  return ret_val;
 }
 L4 = (ATTR(t,ret)==((AS_TYPE_SPEC) NULL));
 L51_=!(L4); 
 if (L51_) {
  ret_val = TP_CON21826358(self, ATTR(t,ret));
  return ret_val;
 }
 else {
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_CON282785865(TP_CONTEXT self, AS_TYPE_SPEC t) {
 BOOL ret_val = BOOL_zero;
 AS_TYPE_SPEC p;
 BOOL L1;
 BOOL L21_;
 p = ATTR(t,params);
 while (1) {
  L1 = (p==((AS_TYPE_SPEC) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  if (TP_CON21826358(self, p)) {
   ret_val = TRUE;
   return ret_val;
  }
  p = ATTR(p,next);
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_GEN1082329842(TP_GEN_TBL self, TUPIDENTINT q, TP_CLASS t) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 INT L3;
 ARRAYdTP L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 if ((t==((TP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = q.t1;
 is_eq_i = ATTR(t,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 L3 = q.t2;
 L4 = ATTR(t,params);
 L51_=(L4)==NULL?0:ASIZE((ARRAYdTP)L4); 
 L6 = L51_;
 L71_=(L3)==(L6); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_GRA927228682(TP_GRAPH self, dTP t, TP_CLASS at) {
 BOOL ret_val = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 dTP L2;
 FSETTP_CLASS L3;
 FSETdTP L4;
 L2 = t;
 if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) at))) {
  L1 = TRUE;
 } else {
  is_eq_self = at;
  is_eq_t = ((OB) TP_BUI1325635093);
  ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  L1 = ret_val1;
 }
 if (L1) {
  ret_val = TRUE;
  return ret_val;
 }
 if (t==NULL) {
  goto other117;
 } else
 switch (TAG(t)) {
  case TP_CLASS_tag:
   L3 = TP_GRA583006971(self, ((TP_CLASS) t));
   if (FSETTP571570007(L3, at)) {
    ret_val = TRUE;
    return ret_val;
   } break;
  default: ;
  other117: ;
 }
 L4 = TP_GRA2060761681(self, at);
 if (FSETdT1764379880(L4, t)) {
  ret_val = TRUE;
  return ret_val;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS TP_GRA583006971(TP_GRAPH self, TP_CLASS t) {
 FSETTP_CLASS ret_val;
 TUPTP_1668454172 p = TUPTP_1668454172_zero;
 FSETTP_CLASS parents;
 TUPIDENTINT cq = TUPIDENTINT_zero;
 AS_CLASS_DEF as;
 FSTR s;
 FSETTUPIDENTINT L11;
 STR L21;
 FSETTP_CLASS r = ((FSETTP_CLASS) NULL);
 FSETTP_CLASS L31;
 TP_CLASS t2;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 FSTR create_self1;
 FSTR ret_val3;
 TP_GRAPH tup_str_self;
 TUPIDENTINT tup_str_t = TUPIDENTINT_zero;
 STR ret_val4;
 FSTR str_self;
 STR ret_val5;
 BOOL L4;
 BOOL L51_;
 ARRAYdTP L6;
 INT L71_;
 TUPIDENTINT L8;
 TUPIDENTINT L9;
 dPARSE L10;
 IDENT L12;
 ARRAYdTP L13;
 INT L141_;
 dPARSE L15;
 FSTR L16;
 INT L17;
 OB L18;
 INT L19;
 extern STR Subtyp1009082454;
 extern STR name29;
 STR L20;
 TUPIDENTINT L22;
 TP_CLASS L23;
 p = FMAPTP1108250174(ATTR(self,anc_tbl), t);
 L4 = (p.t1==((TP_CLASS) NULL));
 L51_=!(L4); 
 if (L51_) {
  ret_val = p.t2;
  return ret_val;
 }
 parents = FMAPTP2096448595(ATTR(self,par_tbl), t);
 if ((parents==((FSETTP_CLASS) NULL))) {
  ret_val = ((FSETTP_CLASS) NULL);
  return ret_val;
 }
 create_self = TUPIDENTINT_zero;
 create_at1 = ATTR(t,name1);
 L6 = ATTR(t,params);
 L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
 create_at2 = L71_;
 L8 = create_self;
 L8.t1 = create_at1;
 L9 = L8;
 L9.t2 = create_at2;
 ret_val1 = L9;
 cq = ret_val1;
 if (FSETTU645397512(ATTR(self,cur), cq)) {
  as_tree_self = t;
  if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L10 = ATTR(ATTR(as_tree_self,prog),parse);
   L12 = ATTR(as_tree_self,name1);
   L13 = ATTR(as_tree_self,params);
   L141_=(L13)==NULL?0:ASIZE((ARRAYdTP)L13); 
   L15 = L10;
   SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L15)])(L15, L12, L141_));
  }
  ret_val2 = ATTR(as_tree_self,my_as_tree);
  as = ret_val2;
  PROG_e176405615(ATTR(self,prog), ((dPROG_ERR) as));
  create_self1 = ((FSTR) NULL);
  L17 = 16;
  L19=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L17)*sizeof(CHAR);
  L18=ZALLOC_LEAF_BIG(L19);
  if (L18==NULL) FATAL("Unable to allocate more memory");
  memset(L18,0,L19);
  ((OB)L18)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L18)->header.destroyed=0;
#endif

  L16 = ((FSTR) L18);
  L16->asize = L17;
  ret_val3 = L16;
  s = FSTR_p1752847026(ret_val3, ((STR) &Subtyp1009082454));
  {
   struct FSETTU947212086_frame_struct other1_0;
FSETTU947212086_frame noname1 = &other1_0;
   struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
   L11 = ATTR(self,cur);
   L21 = ((STR) &name29);
   noname1->self = L11;
   noname1->state = 0;
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    tup_str_self = self;
    L22 = FSETTU947212086(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    tup_str_t = L22;
    ret_val4 = IDENT_1415966123(tup_str_t.t1, tup_str_t.t2);
    noname2->arg1 = ((dSTR) ret_val4);
    L20 = STR_se405450305(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    s = FSTR_p1752847026(s, L20);
   }
  }
  after_loop: ;
  str_self = s;
  ret_val5 = STR_fr1097270334(((STR) NULL), str_self);
  PROG_err_STR(ATTR(self,prog), ret_val5);
  SATTR(self,anc_tbl,FMAPTP594824795(ATTR(self,anc_tbl), t, ((FSETTP_CLASS) NULL)));
  SATTR(self,cur,FSETTU1559268184(ATTR(self,cur), cq));
  ret_val = ((FSETTP_CLASS) NULL);
  return ret_val;
 }
 SATTR(self,cur,FSETTU2045052674(ATTR(self,cur), cq));
 {
  struct FSETTP340014621_frame_struct other2_0;
FSETTP340014621_frame noname3 = &other2_0;
  L31 = parents;
  noname3->self = L31;
  noname3->state = 0;
  while (1) {
   L23 = FSETTP340014621(noname3);
   if (noname3->state == -1) {
    goto after_loop1;
   }
   t2 = L23;
   r = FSETTP1186813482(r, t2);
   r = FSETTP633280235(r, TP_GRA583006971(self, t2));
  }
 }
 after_loop1: ;
 SATTR(self,anc_tbl,FMAPTP594824795(ATTR(self,anc_tbl), t, r));
 SATTR(self,cur,FSETTU1559268184(ATTR(self,cur), cq));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP TP_GRA2060761681(TP_GRAPH self, TP_CLASS t) {
 FSETdTP ret_val;
 FSETdTP r = ((FSETdTP) NULL);
 TUPTP_858321267 p = TUPTP_858321267_zero;
 FSETdTP cld;
 TUPIDENTINT cq = TUPIDENTINT_zero;
 AS_CLASS_DEF as;
 FSTR s;
 FSETTUPIDENTINT L11;
 STR L21;
 FSETdTP L31;
 dTP t2;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 FSTR create_self1;
 FSTR ret_val3;
 TP_GRAPH tup_str_self;
 TUPIDENTINT tup_str_t = TUPIDENTINT_zero;
 STR ret_val4;
 FSTR str_self;
 STR ret_val5;
 BOOL L4;
 BOOL L51_;
 ARRAYdTP L6;
 INT L71_;
 TUPIDENTINT L8;
 TUPIDENTINT L9;
 dPARSE L10;
 IDENT L12;
 ARRAYdTP L13;
 INT L141_;
 dPARSE L15;
 FSTR L16;
 INT L17;
 OB L18;
 INT L19;
 extern STR Supert1129829014;
 extern STR name29;
 STR L20;
 TUPIDENTINT L22;
 dTP L23;
 p = FMAPTP1881680767(ATTR(self,des_tbl), t);
 L4 = (p.t1==((TP_CLASS) NULL));
 L51_=!(L4); 
 if (L51_) {
  ret_val = p.t2;
  return ret_val;
 }
 cld = FMAPTP861983338(ATTR(self,child_tbl), t);
 if ((cld==((FSETdTP) NULL))) {
  ret_val = ((FSETdTP) NULL);
  return ret_val;
 }
 create_self = TUPIDENTINT_zero;
 create_at1 = ATTR(t,name1);
 L6 = ATTR(t,params);
 L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
 create_at2 = L71_;
 L8 = create_self;
 L8.t1 = create_at1;
 L9 = L8;
 L9.t2 = create_at2;
 ret_val1 = L9;
 cq = ret_val1;
 if (FSETTU645397512(ATTR(self,cur), cq)) {
  as_tree_self = t;
  if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L10 = ATTR(ATTR(as_tree_self,prog),parse);
   L12 = ATTR(as_tree_self,name1);
   L13 = ATTR(as_tree_self,params);
   L141_=(L13)==NULL?0:ASIZE((ARRAYdTP)L13); 
   L15 = L10;
   SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L15)])(L15, L12, L141_));
  }
  ret_val2 = ATTR(as_tree_self,my_as_tree);
  as = ret_val2;
  PROG_e176405615(ATTR(self,prog), ((dPROG_ERR) as));
  create_self1 = ((FSTR) NULL);
  L17 = 16;
  L19=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L17)*sizeof(CHAR);
  L18=ZALLOC_LEAF_BIG(L19);
  if (L18==NULL) FATAL("Unable to allocate more memory");
  memset(L18,0,L19);
  ((OB)L18)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L18)->header.destroyed=0;
#endif

  L16 = ((FSTR) L18);
  L16->asize = L17;
  ret_val3 = L16;
  s = FSTR_p1752847026(ret_val3, ((STR) &Supert1129829014));
  {
   struct FSETTU947212086_frame_struct other1_0;
FSETTU947212086_frame noname1 = &other1_0;
   struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
   L11 = ATTR(self,cur);
   L21 = ((STR) &name29);
   noname1->self = L11;
   noname1->state = 0;
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    tup_str_self = self;
    L22 = FSETTU947212086(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    tup_str_t = L22;
    ret_val4 = IDENT_1415966123(tup_str_t.t1, tup_str_t.t2);
    noname2->arg1 = ((dSTR) ret_val4);
    L20 = STR_se405450305(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    s = FSTR_p1752847026(s, L20);
   }
  }
  after_loop: ;
  str_self = s;
  ret_val5 = STR_fr1097270334(((STR) NULL), str_self);
  PROG_err_STR(ATTR(self,prog), ret_val5);
  SATTR(self,des_tbl,FMAPTP551118428(ATTR(self,des_tbl), t, ((FSETdTP) NULL)));
  SATTR(self,cur,FSETTU1559268184(ATTR(self,cur), cq));
  ret_val = ((FSETdTP) NULL);
  return ret_val;
 }
 SATTR(self,cur,FSETTU2045052674(ATTR(self,cur), cq));
 {
  struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname3 = &other2_0;
  L31 = cld;
  noname3->self = L31;
  noname3->state = 0;
  while (1) {
   L23 = FSETdTP_eltbrdTP(noname3);
   if (noname3->state == -1) {
    goto after_loop1;
   }
   t2 = L23;
   r = FSETdT1016814448(r, t2);
   if (t2==NULL) {
    goto other112;
   } else
   switch (TAG(t2)) {
    case TP_CLASS_tag:
     r = FSETdT575601071(r, TP_GRA2060761681(self, ((TP_CLASS) t2))); break;
    default: ;
    other112: ;
   }
  }
 }
 after_loop1: ;
 SATTR(self,des_tbl,FMAPTP551118428(ATTR(self,des_tbl), t, r));
 SATTR(self,cur,FSETTU1559268184(ATTR(self,cur), cq));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC TP_CLASS_ifcrIFC(TP_CLASS self) {
 IFC ret_val;
 if ((ATTR(self,my_ifc)==((IFC) NULL))) {
  SATTR(self,my_ifc,IFC_if331163657(((IFC) NULL), self));
 }
 ret_val = ATTR(self,my_ifc);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL TP_CLA1109727233(TP_CLASS self) {
 IMPL ret_val;
 if ((ATTR(self,my_impl)==((IMPL) NULL))) {
  SATTR(self,my_impl,IMPL_C629829570(((IMPL_CREATE) NULL), self));
 }
 ret_val = ATTR(self,my_impl);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_CLA1199143806(TP_CLASS_TBL self, TUPIDENTARRAYdTP q) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYdTP L11;
 INT i = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 INT L21 = INT_zero;
 STR L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_br;
 INT rL81_;
 INT L9;
 ARRAYdTP L10;
 dTP L121_;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 hash_self = q.t1;
 L3 = hash_self.str;
 L41_=SYSID(L3); 
 x = L41_;
 L51_=((unsigned)x*(unsigned)1664525); 
 L6 = L51_;
 L71_=((unsigned)L6+(unsigned)1013904223); 
 ret_val1 = L71_;
 r = ret_val1;
 {
  BOOL f_L81_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = q.t2;
  L81_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
  while (1) {
   if(L21>=L81_br)  goto after_loop; 
   rL81_=L21; 
   i = rL81_;
   L10 = q.t2;
   L121_=(dTP)ARR((ARRAYdTP)L10,i); 
   L13 = L121_;
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_CLA1206273019(TP_CLASS_TBL self, TP_CLASS e) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYdTP L11;
 INT i = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 INT L21 = INT_zero;
 STR L3;
 INT L41_;
 INT L51_;
 INT L6;
 INT L71_;
 INT L81_br;
 INT rL81_;
 INT L9;
 ARRAYdTP L10;
 dTP L121_;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 hash_self = ATTR(e,name1);
 L3 = hash_self.str;
 L41_=SYSID(L3); 
 x = L41_;
 L51_=((unsigned)x*(unsigned)1664525); 
 L6 = L51_;
 L71_=((unsigned)L6+(unsigned)1013904223); 
 ret_val1 = L71_;
 r = ret_val1;
 {
  BOOL f_L81_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(e,params);
  L81_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
  while (1) {
   if(L21>=L81_br)  goto after_loop; 
   rL81_=L21; 
   i = rL81_;
   L10 = ATTR(e,params);
   L121_=(dTP)ARR((ARRAYdTP)L10,i); 
   L13 = L121_;
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_CLA1653846135(TP_CLASS self) {
 INT ret_val = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val1;
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
 L1 = ATTR(self,kind_cache);
 L2 = TP_KIN138039446;
 L31_=(L1)==(L2); 
 if (L31_) {
  as_tree_self = self;
  if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
   L4 = ATTR(ATTR(as_tree_self,prog),parse);
   L5 = ATTR(as_tree_self,name1);
   L6 = ATTR(as_tree_self,params);
   L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
   L8 = L4;
   SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L8)])(L8, L5, L71_));
  }
  ret_val1 = ATTR(as_tree_self,my_as_tree);
  L9 = (ret_val1==((AS_CLASS_DEF) NULL));
  L101_=!(L9); 
  if (L101_) {
   SATTR(self,kind_cache,ATTR(ATTR(self,my_as_tree),kind));
  }
 }
 ret_val = ATTR(self,kind_cache);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_CLA619166775(TP_CLASS self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

PROG TP_CLA736641047(TP_CLASS self) {
 return ATTR(self,prog);
}


#undef IS_ITER
#define IS_ITER 0

STR TP_CLASS_strrSTR(TP_CLASS self) {
 STR ret_val;
 FSTR s;
 ARRAYdTP L11;
 STR L21;
 FSTR create_self;
 FSTR ret_val1;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 INT L31 = INT_zero;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val3;
 FSTR str_self;
 STR ret_val4;
 BOOL L4;
 BOOL L51_;
 extern STR void1;
 FSTR L6;
 INT L7;
 OB L8;
 INT L9;
 extern STR name3;
 INT L101_br;
 dTP aL101_;
 STR L12;
 dTP L13;
 dTP L14;
 INT L151_;
 L4 = (ATTR(self,sas_cache)==((STR) NULL));
 L51_=!(L4); 
 if (L51_) {
  ret_val = ATTR(self,sas_cache);
  return ret_val;
 }
 if ((self==((TP_CLASS) NULL))) {
  ret_val = ((STR) &void1);
  return ret_val;
 }
 if ((ATTR(self,params)==((ARRAYdTP) NULL))) {
  SATTR(self,sas_cache,ATTR(self,name1).str);
 }
 else {
  create_self = ((FSTR) NULL);
  L7 = 16;
  L9=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L7)*sizeof(CHAR);
  L8=ZALLOC_LEAF_BIG(L9);
  if (L8==NULL) FATAL("Unable to allocate more memory");
  memset(L8,0,L9);
  ((OB)L8)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L8)->header.destroyed=0;
#endif

  L6 = ((FSTR) L8);
  L6->asize = L7;
  ret_val1 = L6;
  plus_self = FSTR_p1752847026(ret_val1, ATTR(self,name1).str);
  plus_c = '{';
  ret_val2 = FSTR_p399773021(plus_self, plus_c);
  s = ret_val2;
  {
   BOOL f_L101_ = TRUE;
   struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
   /* loop index variable */
   L31 = 0;
   L11 = ATTR(self,params);
   L21 = ((STR) &name3);
   L101_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
   noname1->self = L21;
   noname1->state = 0;
   while (1) {
    if(L31>=L101_br)  goto after_loop; 
    aL101_=ARR((ARRAYdTP)L11,L31); 
    L14 = aL101_;
    noname1->arg1 = ((dSTR) (*dTP_strrSTR[TAG(L14)])(L14));
    L12 = STR_se405450305(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    s = FSTR_p1752847026(s, L12);
    L151_=INTPLUS(L31,1); 
    L31 = L151_;
   }
  }
  after_loop: ;
  plus_self1 = s;
  plus_c1 = '}';
  ret_val3 = FSTR_p399773021(plus_self1, plus_c1);
  s = ret_val3;
  str_self = s;
  ret_val4 = STR_fr1097270334(((STR) NULL), str_self);
  SATTR(self,sas_cache,ret_val4);
 }
 ret_val = ATTR(self,sas_cache);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TP_CON702863562(TP_CONTEXT self) {
 STR ret_val;
 STR res;
 ARRAYIDENT L11;
 ARRAYdTP L21;
 STR L31;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 INT L41 = INT_zero;
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
 extern STR name24;
 extern STR name16;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 ARRAYIDENT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 extern STR name3;
 INT L131_br;
 IDENT aL131_;
 INT L141_br;
 dTP aL141_;
 STR L15;
 IDENT L16;
 IDENT L17;
 extern STR name35;
 dTP L18;
 dTP L19;
 INT L201_;
 extern STR name26;
 plus_self = ((STR) &name24);
 plus_sarg = TP_CLASS_strrSTR(ATTR(self,same1));
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &name16);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 res = ret_val2;
 L6 = (ATTR(self,pnames)==((ARRAYIDENT) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,pnames);
  L91_=(L8)==NULL?0:ASIZE((ARRAYIDENT)L8); 
  L10 = L91_;
  L121_=(0)<(L10); 
  L5 = L121_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  {
   BOOL f_L131_ = TRUE;
   BOOL f_L141_ = TRUE;
   struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
   /* loop index variable */
   L41 = 0;
   L11 = ATTR(self,pnames);
   L21 = ATTRs(self,ptypes,((ARRAYdTP) NULL));
   L31 = ((STR) &name3);
   L131_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
   L141_br=L21==NULL?0:ASIZE((ARRAYdTP)L21); 
   noname1->self = L31;
   noname1->state = 0;
   while (1) {
    plus_self4 = res;
    if(L41>=L131_br)  goto after_loop; 
    aL131_=ARR((ARRAYIDENT)L11,L41); 
    L17=aL131_;
    plus_self2 = L17.str;
    plus_sarg2 = ((STR) &name35);
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val3;
    if(L41>=L141_br)  goto after_loop; 
    aL141_=ARR((ARRAYdTP)L21,L41); 
    L19 = aL141_;
    plus_sarg3 = (*dTP_strrSTR[TAG(L19)])(L19);
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    noname1->arg1 = ((dSTR) ret_val4);
    L15 = STR_se405450305(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    plus_sarg4 = L15;
    ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
    res = ret_val5;
    L201_=INTPLUS(L41,1); 
    L41 = L201_;
   }
  }
  after_loop: ;
 }
 plus_self5 = res;
 plus_sarg5 = ((STR) &name26);
 ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
 ret_val = ret_val6;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS TP_CLA1710809725(TP_CLASS self, IDENT name111, ARRAYdTP params, PROG prog) {
 TP_CLASS ret_val;
 TP_CLASS r;
 CONFIG_DEF d;
 CONFIG_DEF L11;
 ARRAYSTR e;
 ARRAYSTR L21;
 CONFIG_DEF L31;
 ARRAYSTR e1;
 ARRAYSTR L41;
 ARRAYdTP L51;
 CONFIG_DEF eltb_self;
 ARRAYSTR ret_val1;
 ARRAYARRAYSTR L61;
 BOOL eltb_if_first = BOOL_zero;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 CONFIG_DEF eltb_self1;
 ARRAYSTR ret_val2;
 ARRAYARRAYSTR L91;
 BOOL eltb_if_first1 = BOOL_zero;
 INT L101 = INT_zero;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 TP_CLASS L14;
 OB L15;
 extern STR BUILTIN_CLASSES;
 INT L161_br;
 ARRAYSTR aL161_;
 ARRAYSTR L17;
 INT L181_br;
 STR aL181_;
 STR L19;
 STR L20;
 INT L221_;
 INT L231_;
 extern STR ATOMIC_CLASSES;
 INT L241_br;
 ARRAYSTR aL241_;
 ARRAYSTR L25;
 INT L261_br;
 STR aL261_;
 STR L27;
 STR L28;
 INT L291_;
 INT L301_;
 BOOL L32;
 BOOL L33;
 BOOL L341_;
 INT L351_br;
 dTP aL351_;
 dTP L36;
 dTP L37;
 INT L381_;
 L15=ZALLOC(sizeof(struct TP_CLASS_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=TP_CLASS_tag;
 L14 = ((TP_CLASS) L15);
 r = L14;
 SATTR(r,name1,name111);
 SATTR(r,params,params);
 SATTR(r,prog,prog);
 SATTR(r,kind_cache,TP_KIN138039446);
 d = CONFIG1152920950(ATTR(prog,config), ((STR) &BUILTIN_CLASSES));
 {
  BOOL f_L161_ = TRUE;
  /* Initialization of inlined iter CONFIG_DEF::elt!:ARRAY{STR} */
  L11 = d;
  eltb_self = L11;
  /* loop index variable */
  L71 = 0;
  L61 = ATTRs(eltb_self,expr,((ARRAYARRAYSTR) NULL));
  L161_br=L61==NULL?0:ASIZE((ARRAYARRAYSTR)L61); 
  eltb_if_first = TRUE;
  while (1) {
   if(L71>=L161_br)  goto after_loop; 
   aL161_=ARR((ARRAYARRAYSTR)L61,L71); 
   e = aL161_;
   {
    BOOL f_L181_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L21 = e;
    L181_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
    while (1) {
     if(L81>=L181_br)  goto after_loop1; 
     aL181_=ARR((ARRAYSTR)L21,L81); 
     L20 = aL181_;
     if (STR_is111530248(L20, TP_CLASS_strrSTR(r))) {
      SATTR(r,is_builtin,TRUE);
      goto after_loop1;
     }
     L221_=INTPLUS(L81,1); 
     L81 = L221_;
    }
   }
   after_loop1: ;
   if (ATTR(r,is_builtin)) {
    goto after_loop;
   }
   L231_=INTPLUS(L71,1); 
   L71 = L231_;
  }
 }
 after_loop: ;
 d = CONFIG1152920950(ATTR(prog,config), ((STR) &ATOMIC_CLASSES));
 {
  BOOL f_L241_ = TRUE;
  /* Initialization of inlined iter CONFIG_DEF::elt!:ARRAY{STR} */
  L31 = d;
  eltb_self1 = L31;
  /* loop index variable */
  L101 = 0;
  L91 = ATTRs(eltb_self1,expr,((ARRAYARRAYSTR) NULL));
  L241_br=L91==NULL?0:ASIZE((ARRAYARRAYSTR)L91); 
  eltb_if_first1 = TRUE;
  while (1) {
   if(L101>=L241_br)  goto after_loop2; 
   aL241_=ARR((ARRAYARRAYSTR)L91,L101); 
   e1 = aL241_;
   {
    BOOL f_L261_ = TRUE;
    /* loop index variable */
    L121 = 0;
    L41 = e1;
    L261_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
    while (1) {
     if(L121>=L261_br)  goto after_loop3; 
     aL261_=ARR((ARRAYSTR)L41,L121); 
     L28 = aL261_;
     if (STR_is111530248(L28, TP_CLASS_strrSTR(r))) {
      SATTR(r,is_atomic,TRUE);
      goto after_loop3;
     }
     L291_=INTPLUS(L121,1); 
     L121 = L291_;
    }
   }
   after_loop3: ;
   if (ATTR(r,is_atomic)) {
    goto after_loop2;
   }
   L301_=INTPLUS(L101,1); 
   L101 = L301_;
  }
 }
 after_loop2: ;
 if (ATTR(prog,check_generics)) {
  L33 = (params==((ARRAYdTP) NULL));
  L341_=!(L33); 
  L32 = L341_;
 } else {
  L32 = FALSE;
 }
 if (L32) {
  {
   BOOL f_L351_ = TRUE;
   /* loop index variable */
   L131 = 0;
   L51 = params;
   L351_br=L51==NULL?0:ASIZE((ARRAYdTP)L51); 
   while (1) {
    if(L131>=L351_br)  goto after_loop4; 
    aL351_=ARR((ARRAYdTP)L51,L131); 
    L37 = aL351_;
    if ((*dTP_is33989660[TAG(L37)])(L37)) {
     SATTR(r,is_generic,TRUE);
     goto after_loop4;
    }
    L381_=INTPLUS(L131,1); 
    L131 = L381_;
   }
  }
  after_loop4: ;
 }
 if (ATTR(prog,find_generics_st)) {
  FIND_G1798674529(ATTR(prog,find_generics), r);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS TP_CLA420352644(TP_CLASS_TBL self, TUPIDENTARRAYdTP q) {
 TP_CLASS ret_val;
 INT h = INT_zero;
 TP_CLASS e;
 TP_CLASS e1;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS_TBL is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val4 = BOOL_zero;
 TP_CLASS_TBL elt_nil_self;
 TP_CLASS ret_val5;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val6;
 TP_CLASS e2 = ((TP_CLASS) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_CLASS L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_CLASS L141_;
 INT L151_;
 if ((self==((TP_CLASS_TBL) NULL))) {
  ret_val = ((TP_CLASS) NULL);
  return ret_val;
 }
 L1 = TP_CLA1199143806(self, q);
 L21_=ASIZE((TP_CLASS_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_CLASS)ARR((TP_CLASS_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_CLASS) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   if (TP_CLA657911258(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_CLASS_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_CLASS)ARR((TP_CLASS_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val4 = (is_elt_nil_e3==((TP_CLASS) NULL));
   ret_val3 = ret_val4;
   if (ret_val3) {
    goto after_loop1;
   }
   else {
    if (TP_CLA657911258(self, q, e1)) {
     ret_val = e1;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val6 = ((TP_CLASS) NULL);
 ret_val5 = ret_val6;
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS TP_GEN1932307144(TP_GEN_TBL self, TUPIDENTINT q) {
 TP_CLASS ret_val;
 INT h = INT_zero;
 TP_CLASS e;
 TP_CLASS e1;
 TP_GEN_TBL query_hash_self;
 TUPIDENTINT query_hash_q = TUPIDENTINT_zero;
 INT ret_val1 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val2 = INT_zero;
 INT x = INT_zero;
 TP_GEN_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 TP_GEN_TBL is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val6 = BOOL_zero;
 TP_GEN_TBL elt_nil_self;
 TP_CLASS ret_val7;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val8;
 TP_CLASS e2 = ((TP_CLASS) NULL);
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 TP_CLASS L191_;
 INT L201_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 TP_CLASS L271_;
 INT L281_;
 if ((self==((TP_GEN_TBL) NULL))) {
  ret_val = ((TP_CLASS) NULL);
  return ret_val;
 }
 query_hash_self = self;
 query_hash_q = q;
 hash_self = query_hash_q.t1;
 L1 = hash_self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val2 = L51_;
 L6 = ret_val2;
 L71_=((unsigned)L6*(unsigned)37); 
 L8 = L71_;
 L9 = query_hash_q.t2;
 L101_=((unsigned)L8+(unsigned)L9); 
 L11 = L101_;
 L121_=((unsigned)L11*(unsigned)19); 
 ret_val1 = INT_hashrINT(L121_);
 L13 = ret_val1;
 L141_=ASIZE((TP_GEN_TBL)self); 
 L15 = L141_;
 L161_=INTMINUS(L15,2); 
 L17 = L161_;
 L181_=L13&L17; 
 h = L181_;
 while (1) {
  L191_=(TP_CLASS)ARR((TP_GEN_TBL)self,h); 
  e = L191_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val4 = (is_elt_nil_e1==((TP_CLASS) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  else {
   if (TP_GEN1082329842(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L201_=INTPLUS(h,1); 
  h = L201_;
 }
 after_loop: ;
 L221_=ASIZE((TP_GEN_TBL)self); 
 L23 = L221_;
 L241_=INTMINUS(L23,1); 
 L25 = L241_;
 L261_=(h)==(L25); 
 if (L261_) {
  h = 0;
  while (1) {
   L271_=(TP_CLASS)ARR((TP_GEN_TBL)self,h); 
   e1 = L271_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val6 = (is_elt_nil_e3==((TP_CLASS) NULL));
   ret_val5 = ret_val6;
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    if (TP_GEN1082329842(self, q, e1)) {
     ret_val = e1;
     return ret_val;
    }
   }
   L281_=INTPLUS(h,1); 
   h = L281_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val8 = ((TP_CLASS) NULL);
 ret_val7 = ret_val8;
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS_TBL TP_CLA1085505433(TP_CLASS_TBL self, INT n) {
 TP_CLASS_TBL ret_val;
 TP_CLASS_TBL r;
 TP_CLASS_TBL L11;
 TP_CLASS_TBL elt_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 TP_CLASS_TBL elt_nil_self2;
 TP_CLASS ret_val3;
 ELT_NILTP_CLASS elt_nil_self3;
 TP_CLASS ret_val4;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 INT L21 = INT_zero;
 TP_CLASS_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TP_CLASS L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct TP_CLASS_TBL_struct)-sizeof(TP_CLASS))+(L4)*sizeof(TP_CLASS);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=TP_CLASS_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((TP_CLASS_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val2 = ((TP_CLASS) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((TP_CLASS) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((TP_CLASS_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
    ret_val4 = ((TP_CLASS) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((TP_CLASS_TBL)L11,L21,(TP_CLASS)L10); 
    ;
    L121_=INTPLUS(L21,1); 
    L21 = L121_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS_TBL TP_CLA307403124(TP_CLASS_TBL self) {
 TP_CLASS_TBL ret_val;
 TP_CLASS_TBL r;
 TP_CLASS_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TP_CLASS L9;
 L21_=ASIZE((TP_CLASS_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = TP_CLA1085505433(self, L81_);
 {
  struct TP_CLA115798516_frame_struct other1_0;
TP_CLA115798516_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = TP_CLA115798516(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = TP_CLA426779342(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS_TBL TP_CLA426779342(TP_CLASS_TBL self, TP_CLASS e) {
 TP_CLASS_TBL ret_val;
 TP_CLASS_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TP_CLASS te;
 TP_CLASS te1;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 TP_CLASS_TBL is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS_TBL elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS_TBL is_elt_nil_self4;
 TP_CLASS is_elt_nil_e4;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self5;
 TP_CLASS is_elt_nil_e5;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS_TBL elt_eq_self2;
 TP_CLASS elt_eq_e12;
 TP_CLASS elt_eq_e22;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self3;
 TP_CLASS elt_eq_e13;
 TP_CLASS elt_eq_e23;
 BOOL ret_val12 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val13 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L101_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 TP_CLASS L161_;
 INT L181_;
 INT L191_;
 INT L20;
 BOOL L211_;
 TP_CLASS L221_;
 INT L241_;
 INT L26;
 INT L271_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((TP_CLASS) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((TP_CLASS_TBL) NULL))) {
  r = TP_CLA1085505433(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((TP_CLASS_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = TP_CLA1631459432;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = TP_CLA307403124(r);
  }
 }
 L101_=ASIZE((TP_CLASS_TBL)r); 
 asz = L101_;
 L12 = TP_CLA1206273019(r, e);
 L131_=INTMINUS(asz,2); 
 L14 = L131_;
 L151_=L12&L14; 
 orig_h = L151_;
 h = orig_h;
 while (1) {
  L161_=(TP_CLASS)ARR((TP_CLASS_TBL)r,h); 
  te = L161_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val5 = (is_elt_nil_e3==((TP_CLASS) NULL));
  ret_val4 = ret_val5;
  if (ret_val4) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val8 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val7 = ret_val8;
   ret_val6 = ret_val7;
   if (ret_val6) {
    SARR((TP_CLASS_TBL)r,h,(TP_CLASS)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L181_=INTPLUS(h,1); 
  h = L181_;
 }
 after_loop: ;
 L191_=INTMINUS(asz,1); 
 L20 = L191_;
 L211_=(h)==(L20); 
 if (L211_) {
  h = 0;
  while (1) {
   L221_=(TP_CLASS)ARR((TP_CLASS_TBL)r,h); 
   te1 = L221_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val10 = (is_elt_nil_e5==((TP_CLASS) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val12 = ret_val13;
    ret_val11 = ret_val12;
    if (ret_val11) {
     SARR((TP_CLASS_TBL)r,h,(TP_CLASS)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L241_=INTPLUS(h,1); 
   h = L241_;
  }
  after_loop1: ;
 }
 SARR((TP_CLASS_TBL)r,h,(TP_CLASS)e); 
 ;
 L26 = ATTR(r,hsize);
 L271_=INTPLUS(L26,1); 
 SATTR(r,hsize,L271_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CONTEXT TP_CLA821242177(TP_CLASS self) {
 TP_CONTEXT ret_val;
 ARRAYIDENT pn;
 INT ps = INT_zero;
 TP_CONTEXT r;
 TP_CONTEXT create_self;
 TP_CLASS create_same;
 ARRAYIDENT create_pnames;
 ARRAYdTP create_ptypes;
 PROG create_prog;
 TP_CONTEXT ret_val1;
 TP_CONTEXT r1;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L51_;
 ARRAYdTP L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 TP_CONTEXT L12;
 OB L13;
 L1 = (ATTR(self,my_context)==((TP_CONTEXT) NULL));
 L21_=!(L1); 
 if (L21_) {
  ret_val = ATTR(self,my_context);
  return ret_val;
 }
 pn = TP_CLA2131361672(self);
 L3 = (pn==((ARRAYIDENT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L51_=ASIZE((ARRAYIDENT)pn); 
  ps = L51_;
 }
 L6 = ATTR(self,params);
 L71_=(L6)==NULL?0:ASIZE((ARRAYdTP)L6); 
 L8 = L71_;
 L91_=(ps)==(L8); 
 L10 = L91_;
 L111_=!(L10); 
 if (L111_) {
  ret_val = ((TP_CONTEXT) NULL);
  return ret_val;
 }
 create_self = ((TP_CONTEXT) NULL);
 create_same = self;
 create_pnames = pn;
 create_ptypes = ATTR(self,params);
 create_prog = ATTR(self,prog);
 L13=ZALLOC(sizeof(struct TP_CONTEXT_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=TP_CONTEXT_tag;
 L12 = ((TP_CONTEXT) L13);
 r1 = L12;
 SATTR(r1,same1,create_same);
 SATTR(r1,pnames,create_pnames);
 SATTR(r1,ptypes,create_ptypes);
 SATTR(r1,prog,create_prog);
 ret_val1 = r1;
 r = ret_val1;
 if (TP_CLA1568240525(self)) {
  SATTR(r,is_abs,TRUE);
 }
 SATTR(self,my_context,r);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_GEN_TBL TP_GEN1111892401(TP_GEN_TBL self, INT n) {
 TP_GEN_TBL ret_val;
 TP_GEN_TBL r;
 TP_GEN_TBL L11;
 TP_GEN_TBL elt_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 TP_GEN_TBL elt_nil_self2;
 TP_CLASS ret_val3;
 ELT_NILTP_CLASS elt_nil_self3;
 TP_CLASS ret_val4;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 INT L21 = INT_zero;
 TP_GEN_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TP_CLASS L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct TP_GEN_TBL_struct)-sizeof(TP_CLASS))+(L4)*sizeof(TP_CLASS);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=TP_GEN_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((TP_GEN_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val2 = ((TP_CLASS) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((TP_CLASS) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((TP_GEN_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
    ret_val4 = ((TP_CLASS) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((TP_GEN_TBL)L11,L21,(TP_CLASS)L10); 
    ;
    L121_=INTPLUS(L21,1); 
    L21 = L121_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_GEN_TBL TP_GEN1128603126(TP_GEN_TBL self) {
 TP_GEN_TBL ret_val;
 TP_GEN_TBL r;
 TP_GEN_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TP_CLASS L9;
 L21_=ASIZE((TP_GEN_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = TP_GEN1111892401(self, L81_);
 {
  struct TP_GEN1057619759_frame_struct other1_0;
TP_GEN1057619759_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = TP_GEN1057619759(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = TP_GEN1370131280(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_GEN_TBL TP_GEN1370131280(TP_GEN_TBL self, TP_CLASS e) {
 TP_GEN_TBL ret_val;
 TP_GEN_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TP_CLASS te;
 TP_CLASS te1;
 TP_GEN_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_GEN_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 TP_GEN_TBL elt_hash_self;
 TP_CLASS elt_hash_e;
 INT ret_val4 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val5 = INT_zero;
 INT x = INT_zero;
 TP_GEN_TBL is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 TP_GEN_TBL elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val10 = BOOL_zero;
 TP_GEN_TBL is_elt_nil_self4;
 TP_CLASS is_elt_nil_e4;
 BOOL ret_val11 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self5;
 TP_CLASS is_elt_nil_e5;
 BOOL ret_val12 = BOOL_zero;
 TP_GEN_TBL elt_eq_self2;
 TP_CLASS elt_eq_e12;
 TP_CLASS elt_eq_e22;
 BOOL ret_val13 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self3;
 TP_CLASS elt_eq_e13;
 TP_CLASS elt_eq_e23;
 BOOL ret_val14 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val15 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L101_;
 STR L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 ARRAYdTP L20;
 INT L211_;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 TP_CLASS L301_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 TP_CLASS L361_;
 INT L381_;
 INT L40;
 INT L421_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((TP_CLASS) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((TP_GEN_TBL) NULL))) {
  r = TP_GEN1111892401(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((TP_GEN_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = TP_GEN180842239;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = TP_GEN1128603126(r);
  }
 }
 L101_=ASIZE((TP_GEN_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_e = e;
 hash_self = ATTR(elt_hash_e,name1);
 L12 = hash_self.str;
 L131_=SYSID(L12); 
 x = L131_;
 L141_=((unsigned)x*(unsigned)1664525); 
 L15 = L141_;
 L161_=((unsigned)L15+(unsigned)1013904223); 
 ret_val5 = L161_;
 L17 = ret_val5;
 L181_=((unsigned)L17*(unsigned)37); 
 L19 = L181_;
 L20 = ATTR(elt_hash_e,params);
 L211_=(L20)==NULL?0:ASIZE((ARRAYdTP)L20); 
 L22 = L211_;
 L231_=((unsigned)L19+(unsigned)L22); 
 L24 = L231_;
 L251_=((unsigned)L24*(unsigned)19); 
 ret_val4 = INT_hashrINT(L251_);
 L26 = ret_val4;
 L271_=INTMINUS(asz,2); 
 L28 = L271_;
 L291_=L26&L28; 
 orig_h = L291_;
 h = orig_h;
 while (1) {
  L301_=(TP_CLASS)ARR((TP_GEN_TBL)r,h); 
  te = L301_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val7 = (is_elt_nil_e3==((TP_CLASS) NULL));
  ret_val6 = ret_val7;
  if (ret_val6) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val10 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val9 = ret_val10;
   ret_val8 = ret_val9;
   if (ret_val8) {
    SARR((TP_GEN_TBL)r,h,(TP_CLASS)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L321_=INTPLUS(h,1); 
  h = L321_;
 }
 after_loop: ;
 L331_=INTMINUS(asz,1); 
 L34 = L331_;
 L351_=(h)==(L34); 
 if (L351_) {
  h = 0;
  while (1) {
   L361_=(TP_CLASS)ARR((TP_GEN_TBL)r,h); 
   te1 = L361_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val12 = (is_elt_nil_e5==((TP_CLASS) NULL));
   ret_val11 = ret_val12;
   if (ret_val11) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val15 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val14 = ret_val15;
    ret_val13 = ret_val14;
    if (ret_val13) {
     SARR((TP_GEN_TBL)r,h,(TP_CLASS)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L381_=INTPLUS(h,1); 
   h = L381_;
  }
  after_loop1: ;
 }
 SARR((TP_GEN_TBL)r,h,(TP_CLASS)e); 
 ;
 L40 = ATTR(r,hsize);
 L421_=INTPLUS(L40,1); 
 SATTR(r,hsize,L421_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON1231573657(TP_CONTEXT self, AS_TYPE_SPEC t) {
 dTP ret_val;
 ARRAYARG args;
 AS_TYPE_SPEC tpa;
 INT L11 = INT_zero;
 INT i = INT_zero;
 ARRAYBOOL hot;
 ARRAYBOOL L21;
 TP_TBL tp_iter_for_self;
 ARRAYARG tp_iter_for_args;
 ARRAYBOOL tp_iter_for_hot;
 dTP tp_iter_for_ret;
 TP_ITER ret_val1;
 TP_ITER r;
 TUPARR1759987776 create_self = TUPARR1759987776_zero;
 ARRAYARG create_at1;
 ARRAYBOOL create_at2;
 dTP create_at3;
 TUPARR1759987776 ret_val2 = TUPARR1759987776_zero;
 ARRAYARG create_self1;
 INT create_n = INT_zero;
 ARRAYARG ret_val3;
 ARG create_self2;
 dTP create_t;
 dMODE create_m;
 ARG ret_val4;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val5;
 ARRAYBOOL create_self3;
 INT create_n1 = INT_zero;
 ARRAYBOOL ret_val6;
 TP_TBL tp_iter_for_self1;
 ARRAYARG tp_iter_for_args1;
 ARRAYBOOL tp_iter_for_hot1;
 dTP tp_iter_for_ret1;
 TP_ITER ret_val7;
 TP_ITER r1;
 TUPARR1759987776 create_self4 = TUPARR1759987776_zero;
 ARRAYARG create_at11;
 ARRAYBOOL create_at21;
 dTP create_at31;
 TUPARR1759987776 ret_val8 = TUPARR1759987776_zero;
 TUPARR1759987776 L3;
 TUPARR1759987776 L4;
 TUPARR1759987776 L5;
 ARRAYARG L6;
 INT L7;
 OB L8;
 INT L9;
 INT L101_;
 INT L12;
 ARG L13;
 OB L14;
 ARG L15;
 INT L171_;
 ARRAYBOOL L18;
 INT L19;
 OB L20;
 INT L22;
 INT L231_br;
INT i_L231_=0;
 BOOL L24;
 BOOL L25;
 BOOL L261_;
 TUPARR1759987776 L27;
 TUPARR1759987776 L28;
 TUPARR1759987776 L29;
 if ((ATTR(t,params)==((AS_TYPE_SPEC) NULL))) {
  tp_iter_for_self = ATTR(ATTR(self,prog),tp_tbl);
  tp_iter_for_args = ((ARRAYARG) NULL);
  tp_iter_for_hot = ((ARRAYBOOL) NULL);
  tp_iter_for_ret = TP_CON1800432689(self, ATTR(t,ret));
  create_self = TUPARR1759987776_zero;
  create_at1 = tp_iter_for_args;
  create_at2 = tp_iter_for_hot;
  create_at3 = tp_iter_for_ret;
  L3 = create_self;
  L3.t1 = create_at1;
  L4 = L3;
  L4.t2 = create_at2;
  L5 = L4;
  L5.t3 = create_at3;
  ret_val2 = L5;
  r = TP_ITE990128539(ATTR(tp_iter_for_self,iter_tbl), ret_val2);
  if ((r==((TP_ITER) NULL))) {
   r = TP_ITE1350567874(((TP_ITER) NULL), tp_iter_for_args, tp_iter_for_hot, tp_iter_for_ret, ATTR(tp_iter_for_self,prog));
   SATTR(tp_iter_for_self,iter_tbl,TP_ITE992470229(ATTR(tp_iter_for_self,iter_tbl), r));
  }
  ret_val1 = r;
  ret_val = ((dTP) ret_val1);
  return ret_val;
 }
 create_self1 = ((ARRAYARG) NULL);
 create_n = AS_TYP2145685956(ATTR(t,params));
 L7 = create_n;
 L9=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L7)*sizeof(ARG);
 L8=ZALLOC_BIG(L9);
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L8)->header.destroyed=0;
#endif

 L6 = ((ARRAYARG) L8);
 L6->asize = L7;
 ret_val3 = L6;
 args = ret_val3;
 tpa = ATTR(t,params);
 {
  BOOL f_L101_ = TRUE;
  L11 = 0;
  L101_=L11-1; 
  while (1) {
   if ((tpa==((AS_TYPE_SPEC) NULL))) {
    goto after_loop;
   }
   L101_++; 
   i = L101_;
   create_self2 = ((ARG) NULL);
   create_t = TP_CON1800432689(self, tpa);
   create_m = MODE_c998271775(((MODE) NULL), ATTR(tpa,mode1));
   L14=ZALLOC(sizeof(struct ARG_struct));
   if (L14==NULL) FATAL("Unable to allocate more memory");
   ((OB)L14)->header.tag=ARG_tag;
   L13 = ((ARG) L14);
   init_self = L13;
   init_t = create_t;
   init_m = create_m;
   init_same = FALSE;
   SATTR(init_self,tp,init_t);
   SATTR(init_self,mode1,init_m);
   SATTR(init_self,issame,init_same);
   ret_val5 = init_self;
   ret_val4 = ret_val5;
   L15 = ret_val4;
   SARR((ARRAYARG)args,i,(ARG)L15); 
   ;
   tpa = ATTR(tpa,next);
  }
 }
 after_loop: ;
 create_self3 = ((ARRAYBOOL) NULL);
 L171_=(args)==NULL?0:ASIZE((ARRAYARG)args); 
 create_n1 = L171_;
 L19 = create_n1;
 L22=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L19)*sizeof(BOOL);
 L20=ZALLOC_LEAF_BIG(L22);
 if (L20==NULL) FATAL("Unable to allocate more memory");
 memset(L20,0,L22);
 ((OB)L20)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

   ((OB)L20)->header.destroyed=0;
#endif

 L18 = ((ARRAYBOOL) L20);
 L18->asize = L19;
 ret_val6 = L18;
 hot = ret_val6;
 tpa = ATTR(t,params);
 {
  BOOL f_L231_ = TRUE;
  L21 = hot;
  L231_br=L21==NULL?0:ASIZE((ARRAYBOOL)L21); 
  i_L231_=0;
  while (1) {
   if ((tpa==((AS_TYPE_SPEC) NULL))) {
    goto after_loop1;
   }
   if(i_L231_>=L231_br)  goto after_loop1; 
   L24 = ATTR(tpa,is_hot);
   SARR((ARRAYBOOL)L21,i_L231_,L24); i_L231_++;
   ;
   tpa = ATTR(tpa,next);
  }
 }
 after_loop1: ;
 L25 = ARRAYB651176924(hot, TRUE);
 L261_=!(L25); 
 if (L261_) {
  hot = ((ARRAYBOOL) NULL);
 }
 tp_iter_for_self1 = ATTR(ATTR(self,prog),tp_tbl);
 tp_iter_for_args1 = args;
 tp_iter_for_hot1 = hot;
 tp_iter_for_ret1 = TP_CON1800432689(self, ATTR(t,ret));
 create_self4 = TUPARR1759987776_zero;
 create_at11 = tp_iter_for_args1;
 create_at21 = tp_iter_for_hot1;
 create_at31 = tp_iter_for_ret1;
 L27 = create_self4;
 L27.t1 = create_at11;
 L28 = L27;
 L28.t2 = create_at21;
 L29 = L28;
 L29.t3 = create_at31;
 ret_val8 = L29;
 r1 = TP_ITE990128539(ATTR(tp_iter_for_self1,iter_tbl), ret_val8);
 if ((r1==((TP_ITER) NULL))) {
  r1 = TP_ITE1350567874(((TP_ITER) NULL), tp_iter_for_args1, tp_iter_for_hot1, tp_iter_for_ret1, ATTR(tp_iter_for_self1,prog));
  SATTR(tp_iter_for_self1,iter_tbl,TP_ITE992470229(ATTR(tp_iter_for_self1,iter_tbl), r1));
 }
 ret_val7 = r1;
 ret_val = ((dTP) ret_val7);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON1705076411(TP_CONTEXT self, AS_TYPE_SPEC t) {
 dTP ret_val;
 ARRAYARG args;
 AS_TYPE_SPEC tpe;
 INT L11 = INT_zero;
 INT i = INT_zero;
 TP_TBL tp_f_rout_for_se;
 ARRAYARG tp_f_rout_for_ar;
 dTP tp_f_rout_for_re;
 TP_ROUT ret_val1;
 TP_ROUT r;
 TUPARRAYARGdTP create_self = TUPARRAYARGdTP_zero;
 ARRAYARG create_at1;
 dTP create_at2;
 TUPARRAYARGdTP ret_val2 = TUPARRAYARGdTP_zero;
 ARRAYARG create_self1;
 INT create_n = INT_zero;
 ARRAYARG ret_val3;
 ARG create_self2;
 dTP create_t;
 dMODE create_m;
 ARG ret_val4;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val5;
 TP_TBL tp_f_rout_for_se1;
 ARRAYARG tp_f_rout_for_ar1;
 dTP tp_f_rout_for_re1;
 TP_ROUT ret_val6;
 TP_ROUT r1;
 TUPARRAYARGdTP create_self3 = TUPARRAYARGdTP_zero;
 ARRAYARG create_at11;
 dTP create_at21;
 TUPARRAYARGdTP ret_val7 = TUPARRAYARGdTP_zero;
 TUPARRAYARGdTP L2;
 TUPARRAYARGdTP L3;
 ARRAYARG L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_;
 INT L9;
 ARG L10;
 OB L12;
 ARG L13;
 TUPARRAYARGdTP L15;
 TUPARRAYARGdTP L16;
 if ((ATTR(t,params)==((AS_TYPE_SPEC) NULL))) {
  tp_f_rout_for_se = ATTR(ATTR(self,prog),tp_tbl);
  tp_f_rout_for_ar = ((ARRAYARG) NULL);
  tp_f_rout_for_re = TP_CON1800432689(self, ATTR(t,ret));
  create_self = TUPARRAYARGdTP_zero;
  create_at1 = tp_f_rout_for_ar;
  create_at2 = tp_f_rout_for_re;
  L2 = create_self;
  L2.t1 = create_at1;
  L3 = L2;
  L3.t2 = create_at2;
  ret_val2 = L3;
  r = TP_ROU581494640(ATTR(tp_f_rout_for_se,f_rout_tbl), ret_val2);
  if ((r==((TP_ROUT) NULL))) {
   r = TP_ROU52430373(((TP_ROUT) NULL), tp_f_rout_for_ar, tp_f_rout_for_re, ATTR(tp_f_rout_for_se,prog));
   SATTR(r,is_fortran,TRUE);
   SATTR(tp_f_rout_for_se,f_rout_tbl,TP_ROU1247655203(ATTR(tp_f_rout_for_se,f_rout_tbl), r));
  }
  ret_val1 = r;
  ret_val = ((dTP) ret_val1);
  return ret_val;
 }
 create_self1 = ((ARRAYARG) NULL);
 create_n = AS_TYP2145685956(ATTR(t,params));
 L5 = create_n;
 L7=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L5)*sizeof(ARG);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYARG) L6);
 L4->asize = L5;
 ret_val3 = L4;
 args = ret_val3;
 tpe = ATTR(t,params);
 {
  BOOL f_L81_ = TRUE;
  L11 = 0;
  L81_=L11-1; 
  while (1) {
   if ((tpe==((AS_TYPE_SPEC) NULL))) {
    goto after_loop;
   }
   L81_++; 
   i = L81_;
   create_self2 = ((ARG) NULL);
   create_t = TP_CON1800432689(self, tpe);
   create_m = MODE_c998271775(((MODE) NULL), ATTR(tpe,mode1));
   L12=ZALLOC(sizeof(struct ARG_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=ARG_tag;
   L10 = ((ARG) L12);
   init_self = L10;
   init_t = create_t;
   init_m = create_m;
   init_same = FALSE;
   SATTR(init_self,tp,init_t);
   SATTR(init_self,mode1,init_m);
   SATTR(init_self,issame,init_same);
   ret_val5 = init_self;
   ret_val4 = ret_val5;
   L13 = ret_val4;
   SARR((ARRAYARG)args,i,(ARG)L13); 
   ;
   tpe = ATTR(tpe,next);
  }
 }
 after_loop: ;
 tp_f_rout_for_se1 = ATTR(ATTR(self,prog),tp_tbl);
 tp_f_rout_for_ar1 = args;
 tp_f_rout_for_re1 = TP_CON1800432689(self, ATTR(t,ret));
 create_self3 = TUPARRAYARGdTP_zero;
 create_at11 = tp_f_rout_for_ar1;
 create_at21 = tp_f_rout_for_re1;
 L15 = create_self3;
 L15.t1 = create_at11;
 L16 = L15;
 L16.t2 = create_at21;
 ret_val7 = L16;
 r1 = TP_ROU581494640(ATTR(tp_f_rout_for_se1,f_rout_tbl), ret_val7);
 if ((r1==((TP_ROUT) NULL))) {
  r1 = TP_ROU52430373(((TP_ROUT) NULL), tp_f_rout_for_ar1, tp_f_rout_for_re1, ATTR(tp_f_rout_for_se1,prog));
  SATTR(r1,is_fortran,TRUE);
  SATTR(tp_f_rout_for_se1,f_rout_tbl,TP_ROU1247655203(ATTR(tp_f_rout_for_se1,f_rout_tbl), r1));
 }
 ret_val6 = r1;
 ret_val = ((dTP) ret_val6);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON1795722259(TP_CONTEXT self, AS_TYPE_SPEC t, BOOL memorize) {
 dTP ret_val;
 dTP pv;
 ARRAYdTP ptps;
 AS_TYPE_SPEC tpe;
 dTP ptp;
 ARRAYdTP L11;
 ARRAYdTP create_self;
 INT create_n = INT_zero;
 ARRAYdTP ret_val1;
 STR L2;
 CHAR L31_;
 CHAR L4;
 BOOL L51_;
 extern STR SAMEis1782910777;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 ARRAYdTP L10;
 INT L12;
 OB L13;
 INT L14;
 INT L151_br;
INT i_L151_=0;
 BOOL L16;
 BOOL L171_;
 L2 = ATTR(t,name1).str;
 L31_=ARR((STR)L2,0); 
 L4 = L31_;
 L51_=L4=='$'; 
 if (L51_) {
  if (TP_CON282785865(self, t)) {
   PROG_e176405615(ATTR(self,prog), ((dPROG_ERR) t));
   PROG_err_STR(ATTR(self,prog), ((STR) &SAMEis1782910777));
  }
 }
 if ((ATTR(t,params)==((AS_TYPE_SPEC) NULL))) {
  pv = TP_CON3366446(self, ATTR(t,name1));
  L6 = (pv==((dTP) NULL));
  L71_=!(L6); 
  if (L71_) {
   ret_val = pv;
   return ret_val;
  }
  L8 = (ATTR(t,name1).str==(STR)0);
  L91_=!(L8); 
  if (L91_) {
   ret_val = ((dTP) TP_TBL944008484(ATTR(ATTR(self,prog),tp_tbl), ATTR(t,name1), ((ARRAYdTP) NULL), memorize));
   return ret_val;
  }
 }
 create_self = ((ARRAYdTP) NULL);
 create_n = AS_TYP2145685956(ATTR(t,params));
 L12 = create_n;
 L14=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L12)*sizeof(dTP);
 L13=ZALLOC_BIG(L14);
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L13)->header.destroyed=0;
#endif

 L10 = ((ARRAYdTP) L13);
 L10->asize = L12;
 ret_val1 = L10;
 ptps = ret_val1;
 tpe = ATTR(t,params);
 {
  BOOL f_L151_ = TRUE;
  L11 = ptps;
  L151_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
  i_L151_=0;
  while (1) {
   if ((tpe==((AS_TYPE_SPEC) NULL))) {
    goto after_loop;
   }
   ptp = TP_CON1800432689(self, tpe);
   if(i_L151_>=L151_br)  goto after_loop; 
   SARR((ARRAYdTP)L11,i_L151_,(dTP)ptp); i_L151_++;
   ;
   tpe = ATTR(tpe,next);
  }
 }
 after_loop: ;
 L16 = (ATTR(t,name1).str==(STR)0);
 L171_=!(L16); 
 if (L171_) {
  ret_val = ((dTP) TP_TBL944008484(ATTR(ATTR(self,prog),tp_tbl), ATTR(t,name1), ptps, memorize));
  return ret_val;
 }
 else {
  ret_val = ((dTP) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON1800432689(TP_CONTEXT self, AS_TYPE_SPEC t) {
 dTP ret_val;
 ret_val = TP_CON2076339407(self, t, TRUE);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON2076339407(TP_CONTEXT self, AS_TYPE_SPEC t, BOOL memorize) {
 dTP ret_val;
 INT L11 = INT_zero;
 if ((t==((AS_TYPE_SPEC) NULL))) {
  ret_val = ((dTP) NULL);
  return ret_val;
 }
 L11 = ATTR(t,kind);
 switch (L11) {
  case 0: 
   ret_val = TP_CON1795722259(self, t, memorize);
   return ret_val;
   break;
  case 1: 
   ret_val = TP_CON2124638015(self, t);
   return ret_val;
   break;
  case 2: 
   ret_val = TP_CON1231573657(self, t);
   return ret_val;
   break;
  case 4: 
   ret_val = TP_CON1705076411(self, t);
   return ret_val;
   break;
  case 3: 
   ret_val = ((dTP) ATTR(self,same1));
   return ret_val;
   break;
  default: ;
   FATAL("No applicable target in case statement\nin TP_CONTEXT::tp_of(AS_TYPE_SPEC,BOOL):$TP\n./Representation/tp.sa:922:23");
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON2124638015(TP_CONTEXT self, AS_TYPE_SPEC t) {
 dTP ret_val;
 ARRAYARG args;
 AS_TYPE_SPEC tpe;
 INT L11 = INT_zero;
 INT i = INT_zero;
 TP_TBL tp_rout_for_self;
 ARRAYARG tp_rout_for_args;
 dTP tp_rout_for_ret;
 TP_ROUT ret_val1;
 TP_ROUT r;
 TUPARRAYARGdTP create_self = TUPARRAYARGdTP_zero;
 ARRAYARG create_at1;
 dTP create_at2;
 TUPARRAYARGdTP ret_val2 = TUPARRAYARGdTP_zero;
 ARRAYARG create_self1;
 INT create_n = INT_zero;
 ARRAYARG ret_val3;
 ARG create_self2;
 dTP create_t;
 dMODE create_m;
 ARG ret_val4;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val5;
 TP_TBL tp_rout_for_self1;
 ARRAYARG tp_rout_for_args1;
 dTP tp_rout_for_ret1;
 TP_ROUT ret_val6;
 TP_ROUT r1;
 TUPARRAYARGdTP create_self3 = TUPARRAYARGdTP_zero;
 ARRAYARG create_at11;
 dTP create_at21;
 TUPARRAYARGdTP ret_val7 = TUPARRAYARGdTP_zero;
 TUPARRAYARGdTP L2;
 TUPARRAYARGdTP L3;
 ARRAYARG L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_;
 INT L9;
 ARG L10;
 OB L12;
 ARG L13;
 TUPARRAYARGdTP L15;
 TUPARRAYARGdTP L16;
 if ((ATTR(t,params)==((AS_TYPE_SPEC) NULL))) {
  tp_rout_for_self = ATTR(ATTR(self,prog),tp_tbl);
  tp_rout_for_args = ((ARRAYARG) NULL);
  tp_rout_for_ret = TP_CON1800432689(self, ATTR(t,ret));
  create_self = TUPARRAYARGdTP_zero;
  create_at1 = tp_rout_for_args;
  create_at2 = tp_rout_for_ret;
  L2 = create_self;
  L2.t1 = create_at1;
  L3 = L2;
  L3.t2 = create_at2;
  ret_val2 = L3;
  r = TP_ROU581494640(ATTR(tp_rout_for_self,rout_tbl), ret_val2);
  if ((r==((TP_ROUT) NULL))) {
   r = TP_ROU52430373(((TP_ROUT) NULL), tp_rout_for_args, tp_rout_for_ret, ATTR(tp_rout_for_self,prog));
   SATTR(tp_rout_for_self,rout_tbl,TP_ROU1247655203(ATTR(tp_rout_for_self,rout_tbl), r));
  }
  ret_val1 = r;
  ret_val = ((dTP) ret_val1);
  return ret_val;
 }
 create_self1 = ((ARRAYARG) NULL);
 create_n = AS_TYP2145685956(ATTR(t,params));
 L5 = create_n;
 L7=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L5)*sizeof(ARG);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYARG) L6);
 L4->asize = L5;
 ret_val3 = L4;
 args = ret_val3;
 tpe = ATTR(t,params);
 {
  BOOL f_L81_ = TRUE;
  L11 = 0;
  L81_=L11-1; 
  while (1) {
   if ((tpe==((AS_TYPE_SPEC) NULL))) {
    goto after_loop;
   }
   L81_++; 
   i = L81_;
   create_self2 = ((ARG) NULL);
   create_t = TP_CON1800432689(self, tpe);
   create_m = MODE_c998271775(((MODE) NULL), ATTR(tpe,mode1));
   L12=ZALLOC(sizeof(struct ARG_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=ARG_tag;
   L10 = ((ARG) L12);
   init_self = L10;
   init_t = create_t;
   init_m = create_m;
   init_same = FALSE;
   SATTR(init_self,tp,init_t);
   SATTR(init_self,mode1,init_m);
   SATTR(init_self,issame,init_same);
   ret_val5 = init_self;
   ret_val4 = ret_val5;
   L13 = ret_val4;
   SARR((ARRAYARG)args,i,(ARG)L13); 
   ;
   tpe = ATTR(tpe,next);
  }
 }
 after_loop: ;
 tp_rout_for_self1 = ATTR(ATTR(self,prog),tp_tbl);
 tp_rout_for_args1 = args;
 tp_rout_for_ret1 = TP_CON1800432689(self, ATTR(t,ret));
 create_self3 = TUPARRAYARGdTP_zero;
 create_at11 = tp_rout_for_args1;
 create_at21 = tp_rout_for_ret1;
 L15 = create_self3;
 L15.t1 = create_at11;
 L16 = L15;
 L16.t2 = create_at21;
 ret_val7 = L16;
 r1 = TP_ROU581494640(ATTR(tp_rout_for_self1,rout_tbl), ret_val7);
 if ((r1==((TP_ROUT) NULL))) {
  r1 = TP_ROU52430373(((TP_ROUT) NULL), tp_rout_for_args1, tp_rout_for_ret1, ATTR(tp_rout_for_self1,prog));
  SATTR(tp_rout_for_self1,rout_tbl,TP_ROU1247655203(ATTR(tp_rout_for_self1,rout_tbl), r1));
 }
 ret_val6 = r1;
 ret_val = ((dTP) ret_val6);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP TP_CON3366446(TP_CONTEXT self, IDENT s) {
 dTP ret_val;
 ARRAYIDENT L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 INT L31_br;
 INT rL31_;
 INT L4;
 ARRAYIDENT L5;
 IDENT L61_;
 ARRAYdTP L7;
 dTP L81_;
 INT L91_;
 if ((ATTR(self,pnames)==((ARRAYIDENT) NULL))) {
  ret_val = ((dTP) NULL);
  return ret_val;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,pnames);
  L31_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
  while (1) {
   if(L21>=L31_br)  goto after_loop; 
   rL31_=L21; 
   i = rL31_;
   is_eq_self = s;
   L5 = ATTR(self,pnames);
   L61_=ARR((ARRAYIDENT)L5,i); 
   is_eq_i = L61_;
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   if (ret_val1) {
    L7 = ATTR(self,ptypes);
    L81_=(dTP)ARR((ARRAYdTP)L7,i); 
    ret_val = L81_;
    return ret_val;
   }
   L91_=INTPLUS(L21,1); 
   L21 = L91_;
  }
 }
 after_loop: ;
 ret_val = ((dTP) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void TP_BUI113997099(TP_BUILTIN self, TP_CLASS r, ARRAYTP_CLASS arr) {
 INT i = INT_zero;
 ARRAYTP_CLASS L11;
 TP_CLASS fel;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val = BOOL_zero;
 ERR1 create_self;
 ERR1 ret_val1;
 ERR1 plus_self;
 dSTR plus_s;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r1;
 ERR1 create_self1;
 ERR1 ret_val3;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val4;
 FILE1 stderr_self1;
 FILE1 ret_val5;
 FILE1 r2;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val6;
 FILE1 stderr_self2;
 FILE1 ret_val7;
 FILE1 r3;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val8;
 FILE1 stderr_self3;
 FILE1 ret_val9;
 FILE1 r4;
 ERR1 plus_self4;
 dSTR plus_s4;
 ERR1 ret_val10;
 FILE1 stderr_self4;
 FILE1 ret_val11;
 FILE1 r5;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val12;
 FILE1 stderr_self5;
 FILE1 ret_val13;
 FILE1 r6;
 ERR1 plus_self6;
 dSTR plus_s6;
 FILE1 stderr_self6;
 FILE1 ret_val14;
 FILE1 r7;
 INT L21_br;
INT i_L21_=0;
 TP_CLASS aL21_;
 INT L31_;
 INT L4;
 BOOL L51_;
 TP_CLASS L6;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 extern STR INTERN1689609128;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 extern STR arraysize;
 FILE1 L19;
 OB L20;
 INT L231_;
 dSTR L24;
 OB L25;
 FILE1 L26;
 OB L27;
 extern STR i11;
 FILE1 L29;
 OB L30;
 dSTR L33;
 OB L34;
 FILE1 L35;
 OB L36;
 extern STR name9;
 FILE1 L38;
 OB L39;
 i = 0;
 {
  BOOL f_L21_ = TRUE;
  L11 = arr;
  L21_br=L11==NULL?0:ASIZE((ARRAYTP_CLASS)L11); 
  i_L21_=0;
  while (1) {
   L31_=(arr)==NULL?0:ASIZE((ARRAYTP_CLASS)arr); 
   L4 = L31_;
   L51_=(i)==(L4); 
   if (L51_) {
    goto after_loop;
   }
   if(i_L21_>=L21_br)  goto after_loop; 
   aL21_=ARR((ARRAYTP_CLASS)L11,i_L21_); i_L21_++;
   fel = aL21_;
   is_eq_self = r;
   is_eq_t = ((OB) fel);
   ret_val = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   if (ret_val) {
    goto after_loop;
   }
   if ((fel==((TP_CLASS) NULL))) {
    SARR((ARRAYTP_CLASS)arr,i,(TP_CLASS)r); 
    ;
    goto after_loop;
   }
   L81_=INTPLUS(i,1); 
   i = L81_;
  }
 }
 after_loop: ;
 L91_=(arr)==NULL?0:ASIZE((ARRAYTP_CLASS)arr); 
 L10 = L91_;
 L121_=(i)==(L10); 
 if (L121_) {
  create_self = ((ERR1) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((dSTR) ((STR) &INTERN1689609128));
  stderr_self = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r1 = L13;
  
  SATTR(r1,fp,stderr);
  ret_val2 = r1;
  FILE_plus_dSTR(ret_val2, plus_s);
  create_self1 = ((ERR1) NULL);
  ret_val3 = create_self1;
  plus_self1 = ret_val3;
  plus_s1 = ((dSTR) ATTR(r,name1).str);
  stderr_self1 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r2 = L16;
  
  SATTR(r2,fp,stderr);
  ret_val5 = r2;
  FILE_plus_dSTR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((dSTR) ((STR) &arraysize));
  stderr_self2 = ((FILE1) NULL);
  L20=ZALLOC(sizeof(struct FILE1_struct));
  if (L20==NULL) FATAL("Unable to allocate more memory");
  ((OB)L20)->header.tag=FILE1_tag;
  L19 = ((FILE1) L20);
  r3 = L19;
  
  SATTR(r3,fp,stderr);
  ret_val7 = r3;
  FILE_plus_dSTR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  L231_=(arr)==NULL?0:ASIZE((ARRAYTP_CLASS)arr); 
  L25=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L25==NULL) FATAL("Unable to allocate more memory");
  memset(L25,0,sizeof(struct INT_boxed_struct));
  ((OB)L25)->header.tag=INT_tag;
  L24 = (dSTR)((INT_boxed) L25);
  ((INT_boxed) L24)->immutable_part = L231_;
  plus_s3 = L24;
  stderr_self3 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r4 = L26;
  
  SATTR(r4,fp,stderr);
  ret_val9 = r4;
  FILE_plus_dSTR(ret_val9, plus_s3);
  ret_val8 = plus_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((dSTR) ((STR) &i11));
  stderr_self4 = ((FILE1) NULL);
  L30=ZALLOC(sizeof(struct FILE1_struct));
  if (L30==NULL) FATAL("Unable to allocate more memory");
  ((OB)L30)->header.tag=FILE1_tag;
  L29 = ((FILE1) L30);
  r5 = L29;
  
  SATTR(r5,fp,stderr);
  ret_val11 = r5;
  FILE_plus_dSTR(ret_val11, plus_s4);
  ret_val10 = plus_self4;
  plus_self5 = ret_val10;
  L34=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L34==NULL) FATAL("Unable to allocate more memory");
  memset(L34,0,sizeof(struct INT_boxed_struct));
  ((OB)L34)->header.tag=INT_tag;
  L33 = (dSTR)((INT_boxed) L34);
  ((INT_boxed) L33)->immutable_part = i;
  plus_s5 = L33;
  stderr_self5 = ((FILE1) NULL);
  L36=ZALLOC(sizeof(struct FILE1_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  ((OB)L36)->header.tag=FILE1_tag;
  L35 = ((FILE1) L36);
  r6 = L35;
  
  SATTR(r6,fp,stderr);
  ret_val13 = r6;
  FILE_plus_dSTR(ret_val13, plus_s5);
  ret_val12 = plus_self5;
  plus_self6 = ret_val12;
  plus_s6 = ((dSTR) ((STR) &name9));
  stderr_self6 = ((FILE1) NULL);
  L39=ZALLOC(sizeof(struct FILE1_struct));
  if (L39==NULL) FATAL("Unable to allocate more memory");
  ((OB)L39)->header.tag=FILE1_tag;
  L38 = ((FILE1) L39);
  r7 = L38;
  
  SATTR(r7,fp,stderr);
  ret_val14 = r7;
  FILE_plus_dSTR(ret_val14, plus_s6);
 }
}


#undef IS_ITER
#define IS_ITER 0

void TP_BUI670024966(TP_BUILTIN self, TP_TBL t, PROG prog) {
 ARRAYdTP arr;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val;
 TP_TBL tp_class_for_sel1;
 IDENT tp_class_for_nam1 = IDENT_zero;
 ARRAYdTP tp_class_for_par1;
 TP_CLASS ret_val1;
 TP_TBL tp_class_for_sel2;
 IDENT tp_class_for_nam2 = IDENT_zero;
 ARRAYdTP tp_class_for_par2;
 TP_CLASS ret_val2;
 TP_TBL tp_class_for_sel3;
 IDENT tp_class_for_nam3 = IDENT_zero;
 ARRAYdTP tp_class_for_par3;
 TP_CLASS ret_val3;
 TP_TBL tp_class_for_sel4;
 IDENT tp_class_for_nam4 = IDENT_zero;
 ARRAYdTP tp_class_for_par4;
 TP_CLASS ret_val4;
 TP_TBL tp_class_for_sel5;
 IDENT tp_class_for_nam5 = IDENT_zero;
 ARRAYdTP tp_class_for_par5;
 TP_CLASS ret_val5;
 TP_TBL tp_class_for_sel6;
 IDENT tp_class_for_nam6 = IDENT_zero;
 ARRAYdTP tp_class_for_par6;
 TP_CLASS ret_val6;
 TP_TBL tp_class_for_sel7;
 IDENT tp_class_for_nam7 = IDENT_zero;
 ARRAYdTP tp_class_for_par7;
 TP_CLASS ret_val7;
 TP_TBL tp_class_for_sel8;
 IDENT tp_class_for_nam8 = IDENT_zero;
 ARRAYdTP tp_class_for_par8;
 TP_CLASS ret_val8;
 TP_TBL tp_class_for_sel9;
 IDENT tp_class_for_nam9 = IDENT_zero;
 ARRAYdTP tp_class_for_par9;
 TP_CLASS ret_val9;
 TP_TBL tp_class_for_sel10;
 IDENT tp_class_for_nam10 = IDENT_zero;
 ARRAYdTP tp_class_for_par10;
 TP_CLASS ret_val10;
 TP_TBL tp_class_for_sel11;
 IDENT tp_class_for_nam11 = IDENT_zero;
 ARRAYdTP tp_class_for_par11;
 TP_CLASS ret_val11;
 TP_TBL tp_rout_for_self;
 ARRAYARG tp_rout_for_args;
 dTP tp_rout_for_ret;
 TP_ROUT ret_val12;
 TP_ROUT r;
 TUPARRAYARGdTP create_self = TUPARRAYARGdTP_zero;
 ARRAYARG create_at1;
 dTP create_at2;
 TUPARRAYARGdTP ret_val13 = TUPARRAYARGdTP_zero;
 ARRAYdTP create_self1;
 INT create_n = INT_zero;
 ARRAYdTP ret_val14;
 TP_TBL tp_class_for_sel12;
 IDENT tp_class_for_nam12 = IDENT_zero;
 ARRAYdTP tp_class_for_par12;
 TP_CLASS ret_val15;
 ARRAYTP_CLASS create_self2;
 INT create_n1 = INT_zero;
 ARRAYTP_CLASS ret_val16;
 ARRAYTP_CLASS create_self3;
 INT create_n2 = INT_zero;
 ARRAYTP_CLASS ret_val17;
 ARRAYTP_CLASS create_self4;
 INT create_n3 = INT_zero;
 ARRAYTP_CLASS ret_val18;
 TP_TBL tp_class_for_sel13;
 IDENT tp_class_for_nam13 = IDENT_zero;
 ARRAYdTP tp_class_for_par13;
 TP_CLASS ret_val19;
 TP_TBL tp_class_for_sel14;
 IDENT tp_class_for_nam14 = IDENT_zero;
 ARRAYdTP tp_class_for_par14;
 TP_CLASS ret_val20;
 extern STR dOB;
 extern STR BOOL1;
 extern STR CHAR1;
 extern STR INT1;
 extern STR INTI;
 extern STR FLT1;
 extern STR FLTD1;
 extern STR STR1;
 extern STR SYS1;
 extern STR EXT_OB1;
 extern STR dATTACH;
 extern STR PREFETCH1;
 TUPARRAYARGdTP L1;
 TUPARRAYARGdTP L2;
 ARRAYdTP L3;
 INT L4;
 OB L5;
 INT L6;
 TP_CLASS L7;
 extern STR ARRAY;
 ARRAYTP_CLASS L9;
 INT L10;
 OB L11;
 INT L12;
 ARRAYTP_CLASS L13;
 INT L14;
 OB L15;
 INT L16;
 ARRAYTP_CLASS L17;
 INT L18;
 OB L19;
 INT L20;
 extern STR FSTR1;
 extern STR RUNTIME1;
 extern STR C_UNIX1;
 extern STR F_INTEGER1;
 extern STR F_REAL1;
 extern STR F_LOGICAL1;
 extern STR F_CHARACTER1;
 extern STR F_DOUBLE1;
 extern STR F_COMPLEX1;
 extern STR F_DOUBLE_COMPLEX1;
 extern STR F_HANDLER1;
 extern STR F_ROUT1;
 extern STR F_STRING1;
 extern STR F_ARRAY;
 extern STR F_ARRAY2;
 extern STR F_ARRAY3;
 extern STR C_CHAR1;
 extern STR C_UNSIGNED_CHAR1;
 extern STR C_SIGNED_CHAR1;
 extern STR C_SHORT1;
 extern STR C_INT1;
 extern STR C_LONG1;
 extern STR C_UNSIGNED_SHORT1;
 extern STR C_UNSIGNED_INT1;
 extern STR C_UNSIGNED_LONG1;
 extern STR C_FLOAT1;
 extern STR C_DOUBLE1;
 extern STR C_LONG_DOUBLE1;
 extern STR C_PTR1;
 extern STR C_CHAR_PTR1;
 extern STR C_UNSI1201809828;
 extern STR C_SIGN1747080528;
 extern STR C_SHORT_PTR1;
 extern STR C_INT_PTR1;
 extern STR C_LONG_PTR1;
 extern STR C_UNSI1303080191;
 extern STR C_UNSI924585353;
 extern STR C_UNSI422070874;
 extern STR C_FLOAT_PTR1;
 extern STR C_DOUBLE_PTR1;
 extern STR C_LONG1810188975;
 extern STR C_SIZE_T1;
 extern STR C_PTRDIFF_T1;
 extern STR dLOCK;
 extern STR ZONE1;
 tp_class_for_sel = t;
 tp_class_for_nam = IDENT_1150413730(IDENT_zero, ((STR) &dOB));
 tp_class_for_par = ((ARRAYdTP) NULL);
 ret_val = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
 TP_BUI1325635093 = ret_val;
 tp_class_for_sel1 = t;
 tp_class_for_nam1 = IDENT_1150413730(IDENT_zero, ((STR) &BOOL1));
 tp_class_for_par1 = ((ARRAYdTP) NULL);
 ret_val1 = TP_TBL944008484(tp_class_for_sel1, tp_class_for_nam1, tp_class_for_par1, TRUE);
 TP_BUILTIN_bool = ret_val1;
 tp_class_for_sel2 = t;
 tp_class_for_nam2 = IDENT_1150413730(IDENT_zero, ((STR) &CHAR1));
 tp_class_for_par2 = ((ARRAYdTP) NULL);
 ret_val2 = TP_TBL944008484(tp_class_for_sel2, tp_class_for_nam2, tp_class_for_par2, TRUE);
 TP_BUILTIN_char = ret_val2;
 tp_class_for_sel3 = t;
 tp_class_for_nam3 = IDENT_1150413730(IDENT_zero, ((STR) &INT1));
 tp_class_for_par3 = ((ARRAYdTP) NULL);
 ret_val3 = TP_TBL944008484(tp_class_for_sel3, tp_class_for_nam3, tp_class_for_par3, TRUE);
 TP_BUILTIN_int = ret_val3;
 tp_class_for_sel4 = t;
 tp_class_for_nam4 = IDENT_1150413730(IDENT_zero, ((STR) &INTI));
 tp_class_for_par4 = ((ARRAYdTP) NULL);
 ret_val4 = TP_TBL944008484(tp_class_for_sel4, tp_class_for_nam4, tp_class_for_par4, TRUE);
 TP_BUILTIN_inti = ret_val4;
 tp_class_for_sel5 = t;
 tp_class_for_nam5 = IDENT_1150413730(IDENT_zero, ((STR) &FLT1));
 tp_class_for_par5 = ((ARRAYdTP) NULL);
 ret_val5 = TP_TBL944008484(tp_class_for_sel5, tp_class_for_nam5, tp_class_for_par5, TRUE);
 TP_BUILTIN_flt = ret_val5;
 tp_class_for_sel6 = t;
 tp_class_for_nam6 = IDENT_1150413730(IDENT_zero, ((STR) &FLTD1));
 tp_class_for_par6 = ((ARRAYdTP) NULL);
 ret_val6 = TP_TBL944008484(tp_class_for_sel6, tp_class_for_nam6, tp_class_for_par6, TRUE);
 TP_BUILTIN_fltd = ret_val6;
 tp_class_for_sel7 = t;
 tp_class_for_nam7 = IDENT_1150413730(IDENT_zero, ((STR) &STR1));
 tp_class_for_par7 = ((ARRAYdTP) NULL);
 ret_val7 = TP_TBL944008484(tp_class_for_sel7, tp_class_for_nam7, tp_class_for_par7, TRUE);
 TP_BUILTIN_str = ret_val7;
 tp_class_for_sel8 = t;
 tp_class_for_nam8 = IDENT_1150413730(IDENT_zero, ((STR) &SYS1));
 tp_class_for_par8 = ((ARRAYdTP) NULL);
 ret_val8 = TP_TBL944008484(tp_class_for_sel8, tp_class_for_nam8, tp_class_for_par8, TRUE);
 TP_BUILTIN_sys = ret_val8;
 tp_class_for_sel9 = t;
 tp_class_for_nam9 = IDENT_1150413730(IDENT_zero, ((STR) &EXT_OB1));
 tp_class_for_par9 = ((ARRAYdTP) NULL);
 ret_val9 = TP_TBL944008484(tp_class_for_sel9, tp_class_for_nam9, tp_class_for_par9, TRUE);
 TP_BUI389447236 = ret_val9;
 tp_class_for_sel10 = t;
 tp_class_for_nam10 = IDENT_1150413730(IDENT_zero, ((STR) &dATTACH));
 tp_class_for_par10 = ((ARRAYdTP) NULL);
 ret_val10 = TP_TBL944008484(tp_class_for_sel10, tp_class_for_nam10, tp_class_for_par10, TRUE);
 TP_BUI191368816 = ret_val10;
 tp_class_for_sel11 = t;
 tp_class_for_nam11 = IDENT_1150413730(IDENT_zero, ((STR) &PREFETCH1));
 tp_class_for_par11 = ((ARRAYdTP) NULL);
 ret_val11 = TP_TBL944008484(tp_class_for_sel11, tp_class_for_nam11, tp_class_for_par11, TRUE);
 TP_BUI765623063 = ret_val11;
 tp_rout_for_self = t;
 tp_rout_for_args = ((ARRAYARG) NULL);
 tp_rout_for_ret = ((dTP) NULL);
 create_self = TUPARRAYARGdTP_zero;
 create_at1 = tp_rout_for_args;
 create_at2 = tp_rout_for_ret;
 L1 = create_self;
 L1.t1 = create_at1;
 L2 = L1;
 L2.t2 = create_at2;
 ret_val13 = L2;
 r = TP_ROU581494640(ATTR(tp_rout_for_self,rout_tbl), ret_val13);
 if ((r==((TP_ROUT) NULL))) {
  r = TP_ROU52430373(((TP_ROUT) NULL), tp_rout_for_args, tp_rout_for_ret, ATTR(tp_rout_for_self,prog));
  SATTR(tp_rout_for_self,rout_tbl,TP_ROU1247655203(ATTR(tp_rout_for_self,rout_tbl), r));
 }
 ret_val12 = r;
 TP_BUILTIN_rout = ret_val12;
 create_self1 = ((ARRAYdTP) NULL);
 create_n = 1;
 L4 = create_n;
 L6=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L4)*sizeof(dTP);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((ARRAYdTP) L5);
 L3->asize = L4;
 ret_val14 = L3;
 arr = ret_val14;
 L7 = TP_BUILTIN_str;
 SARR((ARRAYdTP)arr,0,(dTP)L7); 
 ;
 tp_class_for_sel12 = t;
 tp_class_for_nam12 = IDENT_1150413730(IDENT_zero, ((STR) &ARRAY));
 tp_class_for_par12 = arr;
 ret_val15 = TP_TBL944008484(tp_class_for_sel12, tp_class_for_nam12, tp_class_for_par12, TRUE);
 TP_BUI845074864 = ret_val15;
 create_self2 = ((ARRAYTP_CLASS) NULL);
 create_n1 = 7;
 L10 = create_n1;
 L12=(sizeof(struct ARRAYTP_CLASS_struct)-sizeof(TP_CLASS))+(L10)*sizeof(TP_CLASS);
 L11=ZALLOC_BIG(L12);
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=ARRAYTP_CLASS_tag;
#ifdef DESTROY_CHK

   ((OB)L11)->header.destroyed=0;
#endif

 L9 = ((ARRAYTP_CLASS) L11);
 L9->asize = L10;
 ret_val16 = L9;
 TP_BUI1517259338 = ret_val16;
 create_self3 = ((ARRAYTP_CLASS) NULL);
 create_n2 = 7;
 L14 = create_n2;
 L16=(sizeof(struct ARRAYTP_CLASS_struct)-sizeof(TP_CLASS))+(L14)*sizeof(TP_CLASS);
 L15=ZALLOC_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=ARRAYTP_CLASS_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((ARRAYTP_CLASS) L15);
 L13->asize = L14;
 ret_val17 = L13;
 TP_BUI1236844942 = ret_val17;
 create_self4 = ((ARRAYTP_CLASS) NULL);
 create_n3 = 7;
 L18 = create_n3;
 L20=(sizeof(struct ARRAYTP_CLASS_struct)-sizeof(TP_CLASS))+(L18)*sizeof(TP_CLASS);
 L19=ZALLOC_BIG(L20);
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=ARRAYTP_CLASS_tag;
#ifdef DESTROY_CHK

   ((OB)L19)->header.destroyed=0;
#endif

 L17 = ((ARRAYTP_CLASS) L19);
 L17->asize = L18;
 ret_val18 = L17;
 TP_BUI1236844961 = ret_val18;
 TP_BUI2068715177 = IDENT_1150413730(IDENT_zero, ((STR) &FSTR1));
 TP_BUI482029710 = IDENT_1150413730(IDENT_zero, ((STR) &RUNTIME1));
 TP_BUI238610581 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNIX1));
 TP_BUI243806180 = IDENT_1150413730(IDENT_zero, ((STR) &F_INTEGER1));
 TP_BUI640672586 = IDENT_1150413730(IDENT_zero, ((STR) &F_REAL1));
 TP_BUI66959111 = IDENT_1150413730(IDENT_zero, ((STR) &F_LOGICAL1));
 TP_BUI881541400 = IDENT_1150413730(IDENT_zero, ((STR) &F_CHARACTER1));
 TP_BUI620054994 = IDENT_1150413730(IDENT_zero, ((STR) &F_DOUBLE1));
 TP_BUI567029074 = IDENT_1150413730(IDENT_zero, ((STR) &F_COMPLEX1));
 TP_BUI1390125097 = IDENT_1150413730(IDENT_zero, ((STR) &F_DOUBLE_COMPLEX1));
 TP_BUI436262224 = IDENT_1150413730(IDENT_zero, ((STR) &F_HANDLER1));
 TP_BUI354250336 = IDENT_1150413730(IDENT_zero, ((STR) &F_ROUT1));
 TP_BUI726964134 = IDENT_1150413730(IDENT_zero, ((STR) &F_STRING1));
 TP_BUI764451214 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY));
 TP_BUI388121286 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY2));
 TP_BUI505750453 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY3));
 TP_BUI1636890711 = IDENT_1150413730(IDENT_zero, ((STR) &C_CHAR1));
 TP_BUI1322220574 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSIGNED_CHAR1));
 TP_BUI1597109090 = IDENT_1150413730(IDENT_zero, ((STR) &C_SIGNED_CHAR1));
 TP_BUI1626092738 = IDENT_1150413730(IDENT_zero, ((STR) &C_SHORT1));
 TP_BUI1459211256 = IDENT_1150413730(IDENT_zero, ((STR) &C_INT1));
 TP_BUI345188431 = IDENT_1150413730(IDENT_zero, ((STR) &C_LONG1));
 TP_BUI57672398 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSIGNED_SHORT1));
 TP_BUI591808532 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSIGNED_INT1));
 TP_BUI659858568 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSIGNED_LONG1));
 TP_BUI1586744698 = IDENT_1150413730(IDENT_zero, ((STR) &C_FLOAT1));
 TP_BUI222951651 = IDENT_1150413730(IDENT_zero, ((STR) &C_DOUBLE1));
 TP_BUI1031701849 = IDENT_1150413730(IDENT_zero, ((STR) &C_LONG_DOUBLE1));
 TP_BUI1845282323 = IDENT_1150413730(IDENT_zero, ((STR) &C_PTR1));
 TP_BUI1928257270 = IDENT_1150413730(IDENT_zero, ((STR) &C_CHAR_PTR1));
 TP_BUI2144653755 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSI1201809828));
 TP_BUI391975957 = IDENT_1150413730(IDENT_zero, ((STR) &C_SIGN1747080528));
 TP_BUI819938711 = IDENT_1150413730(IDENT_zero, ((STR) &C_SHORT_PTR1));
 TP_BUI732557391 = IDENT_1150413730(IDENT_zero, ((STR) &C_INT_PTR1));
 TP_BUI979458576 = IDENT_1150413730(IDENT_zero, ((STR) &C_LONG_PTR1));
 TP_BUI895836687 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSI1303080191));
 TP_BUI1516264549 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSI924585353));
 TP_BUI757402305 = IDENT_1150413730(IDENT_zero, ((STR) &C_UNSI422070874));
 TP_BUI1825617811 = IDENT_1150413730(IDENT_zero, ((STR) &C_FLOAT_PTR1));
 TP_BUI1336772878 = IDENT_1150413730(IDENT_zero, ((STR) &C_DOUBLE_PTR1));
 TP_BUI370100142 = IDENT_1150413730(IDENT_zero, ((STR) &C_LONG1810188975));
 TP_BUI1247087282 = IDENT_1150413730(IDENT_zero, ((STR) &C_SIZE_T1));
 TP_BUI459983896 = IDENT_1150413730(IDENT_zero, ((STR) &C_PTRDIFF_T1));
 tp_class_for_sel13 = t;
 tp_class_for_nam13 = IDENT_1150413730(IDENT_zero, ((STR) &dLOCK));
 tp_class_for_par13 = ((ARRAYdTP) NULL);
 ret_val19 = TP_TBL944008484(tp_class_for_sel13, tp_class_for_nam13, tp_class_for_par13, TRUE);
 TP_BUI1813155122 = ret_val19;
 if (ATTR(prog,zones)) {
  tp_class_for_sel14 = t;
  tp_class_for_nam14 = IDENT_1150413730(IDENT_zero, ((STR) &ZONE1));
  tp_class_for_par14 = ((ARRAYdTP) NULL);
  ret_val20 = TP_TBL944008484(tp_class_for_sel14, tp_class_for_nam14, tp_class_for_par14, TRUE);
  TP_BUILTIN_zone = ret_val20;
 }
}


#undef IS_ITER
#define IS_ITER 0

void TP_BUI717287368(TP_BUILTIN self, IDENT class_name, TP_CLASS r, ARRAYdTP params) {
 IDENT L11 = IDENT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_eq_self2 = IDENT_zero;
 IDENT is_eq_i2 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 IDENT is_eq_self3 = IDENT_zero;
 IDENT is_eq_i3 = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_eq_self4 = IDENT_zero;
 IDENT is_eq_i4 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_eq_self5 = IDENT_zero;
 IDENT is_eq_i5 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_eq_self6 = IDENT_zero;
 IDENT is_eq_i6 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 IDENT is_eq_self7 = IDENT_zero;
 IDENT is_eq_i7 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 IDENT is_eq_self8 = IDENT_zero;
 IDENT is_eq_i8 = IDENT_zero;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_eq_self9 = IDENT_zero;
 IDENT is_eq_i9 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 IDENT is_eq_self10 = IDENT_zero;
 IDENT is_eq_i10 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_eq_self11 = IDENT_zero;
 IDENT is_eq_i11 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 IDENT is_eq_self12 = IDENT_zero;
 IDENT is_eq_i12 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 IDENT is_eq_self13 = IDENT_zero;
 IDENT is_eq_i13 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 IDENT is_eq_self14 = IDENT_zero;
 IDENT is_eq_i14 = IDENT_zero;
 BOOL ret_val14 = BOOL_zero;
 IDENT is_eq_self15 = IDENT_zero;
 IDENT is_eq_i15 = IDENT_zero;
 BOOL ret_val15 = BOOL_zero;
 IDENT is_eq_self16 = IDENT_zero;
 IDENT is_eq_i16 = IDENT_zero;
 BOOL ret_val16 = BOOL_zero;
 IDENT is_eq_self17 = IDENT_zero;
 IDENT is_eq_i17 = IDENT_zero;
 BOOL ret_val17 = BOOL_zero;
 IDENT is_eq_self18 = IDENT_zero;
 IDENT is_eq_i18 = IDENT_zero;
 BOOL ret_val18 = BOOL_zero;
 IDENT is_eq_self19 = IDENT_zero;
 IDENT is_eq_i19 = IDENT_zero;
 BOOL ret_val19 = BOOL_zero;
 IDENT is_eq_self20 = IDENT_zero;
 IDENT is_eq_i20 = IDENT_zero;
 BOOL ret_val20 = BOOL_zero;
 IDENT is_eq_self21 = IDENT_zero;
 IDENT is_eq_i21 = IDENT_zero;
 BOOL ret_val21 = BOOL_zero;
 IDENT is_eq_self22 = IDENT_zero;
 IDENT is_eq_i22 = IDENT_zero;
 BOOL ret_val22 = BOOL_zero;
 IDENT is_eq_self23 = IDENT_zero;
 IDENT is_eq_i23 = IDENT_zero;
 BOOL ret_val23 = BOOL_zero;
 IDENT is_eq_self24 = IDENT_zero;
 IDENT is_eq_i24 = IDENT_zero;
 BOOL ret_val24 = BOOL_zero;
 IDENT is_eq_self25 = IDENT_zero;
 IDENT is_eq_i25 = IDENT_zero;
 BOOL ret_val25 = BOOL_zero;
 IDENT is_eq_self26 = IDENT_zero;
 IDENT is_eq_i26 = IDENT_zero;
 BOOL ret_val26 = BOOL_zero;
 IDENT is_eq_self27 = IDENT_zero;
 IDENT is_eq_i27 = IDENT_zero;
 BOOL ret_val27 = BOOL_zero;
 IDENT is_eq_self28 = IDENT_zero;
 IDENT is_eq_i28 = IDENT_zero;
 BOOL ret_val28 = BOOL_zero;
 IDENT is_eq_self29 = IDENT_zero;
 IDENT is_eq_i29 = IDENT_zero;
 BOOL ret_val29 = BOOL_zero;
 IDENT is_eq_self30 = IDENT_zero;
 IDENT is_eq_i30 = IDENT_zero;
 BOOL ret_val30 = BOOL_zero;
 IDENT is_eq_self31 = IDENT_zero;
 IDENT is_eq_i31 = IDENT_zero;
 BOOL ret_val31 = BOOL_zero;
 IDENT is_eq_self32 = IDENT_zero;
 IDENT is_eq_i32 = IDENT_zero;
 BOOL ret_val32 = BOOL_zero;
 IDENT is_eq_self33 = IDENT_zero;
 IDENT is_eq_i33 = IDENT_zero;
 BOOL ret_val33 = BOOL_zero;
 IDENT is_eq_self34 = IDENT_zero;
 IDENT is_eq_i34 = IDENT_zero;
 BOOL ret_val34 = BOOL_zero;
 IDENT is_eq_self35 = IDENT_zero;
 IDENT is_eq_i35 = IDENT_zero;
 BOOL ret_val35 = BOOL_zero;
 IDENT is_eq_self36 = IDENT_zero;
 IDENT is_eq_i36 = IDENT_zero;
 BOOL ret_val36 = BOOL_zero;
 IDENT is_eq_self37 = IDENT_zero;
 IDENT is_eq_i37 = IDENT_zero;
 BOOL ret_val37 = BOOL_zero;
 IDENT is_eq_self38 = IDENT_zero;
 IDENT is_eq_i38 = IDENT_zero;
 BOOL ret_val38 = BOOL_zero;
 IDENT is_eq_self39 = IDENT_zero;
 IDENT is_eq_i39 = IDENT_zero;
 BOOL ret_val39 = BOOL_zero;
 IDENT is_eq_self40 = IDENT_zero;
 IDENT is_eq_i40 = IDENT_zero;
 BOOL ret_val40 = BOOL_zero;
 IDENT is_eq_self41 = IDENT_zero;
 IDENT is_eq_i41 = IDENT_zero;
 BOOL ret_val41 = BOOL_zero;
 IDENT is_eq_self42 = IDENT_zero;
 IDENT is_eq_i42 = IDENT_zero;
 BOOL ret_val42 = BOOL_zero;
 L11 = class_name;
 is_eq_self = L11;
 is_eq_i = TP_BUI2068715177;
 ret_val = (is_eq_self.str==is_eq_i.str);
 if (ret_val) {
  TP_BUILTIN_fstr = r;
 }
 else {
  is_eq_self1 = L11;
  is_eq_i1 = TP_BUI482029710;
  ret_val1 = (is_eq_self1.str==is_eq_i1.str);
  if (ret_val1) {
   TP_BUI1686668470 = r;
  }
  else {
   is_eq_self2 = L11;
   is_eq_i2 = TP_BUI238610581;
   ret_val2 = (is_eq_self2.str==is_eq_i2.str);
   if (ret_val2) {
    TP_BUI233684457 = r;
   }
   else {
    is_eq_self3 = L11;
    is_eq_i3 = TP_BUI243806180;
    ret_val3 = (is_eq_self3.str==is_eq_i3.str);
    if (ret_val3) {
     TP_BUI403444172 = r;
    }
    else {
     is_eq_self4 = L11;
     is_eq_i4 = TP_BUI640672586;
     ret_val4 = (is_eq_self4.str==is_eq_i4.str);
     if (ret_val4) {
      TP_BUI374366290 = r;
     }
     else {
      is_eq_self5 = L11;
      is_eq_i5 = TP_BUI66959111;
      ret_val5 = (is_eq_self5.str==is_eq_i5.str);
      if (ret_val5) {
       TP_BUI1194559023 = r;
      }
      else {
       is_eq_self6 = L11;
       is_eq_i6 = TP_BUI881541400;
       ret_val6 = (is_eq_self6.str==is_eq_i6.str);
       if (ret_val6) {
        TP_BUI1805763264 = r;
       }
       else {
        is_eq_self7 = L11;
        is_eq_i7 = TP_BUI620054994;
        ret_val7 = (is_eq_self7.str==is_eq_i7.str);
        if (ret_val7) {
         TP_BUI1370948386 = r;
        }
        else {
         is_eq_self8 = L11;
         is_eq_i8 = TP_BUI567029074;
         ret_val8 = (is_eq_self8.str==is_eq_i8.str);
         if (ret_val8) {
          TP_BUI633637838 = r;
         }
         else {
          is_eq_self9 = L11;
          is_eq_i9 = TP_BUI1390125097;
          ret_val9 = (is_eq_self9.str==is_eq_i9.str);
          if (ret_val9) {
           TP_BUI1032249067 = r;
          }
          else {
           is_eq_self10 = L11;
           is_eq_i10 = TP_BUI436262224;
           ret_val10 = (is_eq_self10.str==is_eq_i10.str);
           if (ret_val10) {
            TP_BUI1116976640 = r;
           }
           else {
            is_eq_self11 = L11;
            is_eq_i11 = TP_BUI354250336;
            ret_val11 = (is_eq_self11.str==is_eq_i11.str);
            if (ret_val11) {
             TP_BUI1116976640 = r;
            }
            else {
             is_eq_self12 = L11;
             is_eq_i12 = TP_BUI726964134;
             ret_val12 = (is_eq_self12.str==is_eq_i12.str);
             if (ret_val12) {
              TP_BUI2088674906 = r;
             }
             else {
              is_eq_self13 = L11;
              is_eq_i13 = TP_BUI764451214;
              ret_val13 = (is_eq_self13.str==is_eq_i13.str);
              if (ret_val13) {
               TP_BUI113997099(self, r, TP_BUI1517259338);
              }
              else {
               is_eq_self14 = L11;
               is_eq_i14 = TP_BUI388121286;
               ret_val14 = (is_eq_self14.str==is_eq_i14.str);
               if (ret_val14) {
                TP_BUI113997099(self, r, TP_BUI1236844942);
               }
               else {
                is_eq_self15 = L11;
                is_eq_i15 = TP_BUI505750453;
                ret_val15 = (is_eq_self15.str==is_eq_i15.str);
                if (ret_val15) {
                 TP_BUI113997099(self, r, TP_BUI1236844961);
                }
                else {
                 is_eq_self16 = L11;
                 is_eq_i16 = TP_BUI1636890711;
                 ret_val16 = (is_eq_self16.str==is_eq_i16.str);
                 if (ret_val16) {
                  TP_BUI231294523 = r;
                 }
                 else {
                  is_eq_self17 = L11;
                  is_eq_i17 = TP_BUI1322220574;
                  ret_val17 = (is_eq_self17.str==is_eq_i17.str);
                  if (ret_val17) {
                   TP_BUI1769472218 = r;
                  }
                  else {
                   is_eq_self18 = L11;
                   is_eq_i18 = TP_BUI1597109090;
                   ret_val18 = (is_eq_self18.str==is_eq_i18.str);
                   if (ret_val18) {
                    TP_BUI274781170 = r;
                   }
                   else {
                    is_eq_self19 = L11;
                    is_eq_i19 = TP_BUI1626092738;
                    ret_val19 = (is_eq_self19.str==is_eq_i19.str);
                    if (ret_val19) {
                     TP_BUI139344778 = r;
                    }
                    else {
                     is_eq_self20 = L11;
                     is_eq_i20 = TP_BUI1459211256;
                     ret_val20 = (is_eq_self20.str==is_eq_i20.str);
                     if (ret_val20) {
                      TP_BUILTIN_c_int = r;
                     }
                     else {
                      is_eq_self21 = L11;
                      is_eq_i21 = TP_BUI345188431;
                      ret_val21 = (is_eq_self21.str==is_eq_i21.str);
                      if (ret_val21) {
                       TP_BUI232519909 = r;
                      }
                      else {
                       is_eq_self22 = L11;
                       is_eq_i22 = TP_BUI57672398;
                       ret_val22 = (is_eq_self22.str==is_eq_i22.str);
                       if (ret_val22) {
                        TP_BUI779482534 = r;
                       }
                       else {
                        is_eq_self23 = L11;
                        is_eq_i23 = TP_BUI591808532;
                        ret_val23 = (is_eq_self23.str==is_eq_i23.str);
                        if (ret_val23) {
                         TP_BUI997290216 = r;
                        }
                        else {
                         is_eq_self24 = L11;
                         is_eq_i24 = TP_BUI659858568;
                         ret_val24 = (is_eq_self24.str==is_eq_i24.str);
                         if (ret_val24) {
                          TP_BUI1768246832 = r;
                         }
                         else {
                          is_eq_self25 = L11;
                          is_eq_i25 = TP_BUI1586744698;
                          ret_val25 = (is_eq_self25.str==is_eq_i25.str);
                          if (ret_val25) {
                           TP_BUI107670638 = r;
                          }
                          else {
                           is_eq_self26 = L11;
                           is_eq_i26 = TP_BUI222951651;
                           ret_val26 = (is_eq_self26.str==is_eq_i26.str);
                           if (ret_val26) {
                            TP_BUI1959866815 = r;
                           }
                           else {
                            is_eq_self27 = L11;
                            is_eq_i27 = TP_BUI1031701849;
                            ret_val27 = (is_eq_self27.str==is_eq_i27.str);
                            if (ret_val27) {
                             TP_BUI1825364383 = r;
                            }
                            else {
                             is_eq_self28 = L11;
                             is_eq_i28 = TP_BUI1845282323;
                             ret_val28 = (is_eq_self28.str==is_eq_i28.str);
                             if (ret_val28) {
                              TP_BUILTIN_c_ptr = r;
                             }
                             else {
                              is_eq_self29 = L11;
                              is_eq_i29 = TP_BUI1928257270;
                              ret_val29 = (is_eq_self29.str==is_eq_i29.str);
                              if (ret_val29) {
                               TP_BUI468587458 = r;
                              }
                              else {
                               is_eq_self30 = L11;
                               is_eq_i30 = TP_BUI2144653755;
                               ret_val30 = (is_eq_self30.str==is_eq_i30.str);
                               if (ret_val30) {
                                TP_BUI1716944501 = r;
                               }
                               else {
                                is_eq_self31 = L11;
                                is_eq_i31 = TP_BUI391975957;
                                ret_val31 = (is_eq_self31.str==is_eq_i31.str);
                                if (ret_val31) {
                                 TP_BUI1696960301 = r;
                                }
                                else {
                                 is_eq_self32 = L11;
                                 is_eq_i32 = TP_BUI819938711;
                                 ret_val32 = (is_eq_self32.str==is_eq_i32.str);
                                 if (ret_val32) {
                                  TP_BUI444762713 = r;
                                 }
                                 else {
                                  is_eq_self33 = L11;
                                  is_eq_i33 = TP_BUI732557391;
                                  ret_val33 = (is_eq_self33.str==is_eq_i33.str);
                                  if (ret_val33) {
                                   TP_BUI1292722345 = r;
                                  }
                                  else {
                                   is_eq_self34 = L11;
                                   is_eq_i34 = TP_BUI979458576;
                                   ret_val34 = (is_eq_self34.str==is_eq_i34.str);
                                   if (ret_val34) {
                                    TP_BUI1248326412 = r;
                                   }
                                   else {
                                    is_eq_self35 = L11;
                                    is_eq_i35 = TP_BUI895836687;
                                    ret_val35 = (is_eq_self35.str==is_eq_i35.str);
                                    if (ret_val35) {
                                     TP_BUI1606257075 = r;
                                    }
                                    else {
                                     is_eq_self36 = L11;
                                     is_eq_i36 = TP_BUI1516264549;
                                     ret_val36 = (is_eq_self36.str==is_eq_i36.str);
                                     if (ret_val36) {
                                      TP_BUI2131222993 = r;
                                     }
                                     else {
                                      is_eq_self37 = L11;
                                      is_eq_i37 = TP_BUI757402305;
                                      ret_val37 = (is_eq_self37.str==is_eq_i37.str);
                                      if (ret_val37) {
                                       TP_BUI1798283841 = r;
                                      }
                                      else {
                                       is_eq_self38 = L11;
                                       is_eq_i38 = TP_BUI1825617811;
                                       ret_val38 = (is_eq_self38.str==is_eq_i38.str);
                                       if (ret_val38) {
                                        TP_BUI102735229 = r;
                                       }
                                       else {
                                        is_eq_self39 = L11;
                                        is_eq_i39 = TP_BUI1336772878;
                                        ret_val39 = (is_eq_self39.str==is_eq_i39.str);
                                        if (ret_val39) {
                                         TP_BUI1296146890 = r;
                                        }
                                        else {
                                         is_eq_self40 = L11;
                                         is_eq_i40 = TP_BUI370100142;
                                         ret_val40 = (is_eq_self40.str==is_eq_i40.str);
                                         if (ret_val40) {
                                          TP_BUI2025364786 = r;
                                         }
                                         else {
                                          is_eq_self41 = L11;
                                          is_eq_i41 = TP_BUI1247087282;
                                          ret_val41 = (is_eq_self41.str==is_eq_i41.str);
                                          if (ret_val41) {
                                           TP_BUI1643601086 = r;
                                          }
                                          else {
                                           is_eq_self42 = L11;
                                           is_eq_i42 = TP_BUI459983896;
                                           ret_val42 = (is_eq_self42.str==is_eq_i42.str);
                                           if (ret_val42) {
                                            TP_BUI1609790832 = r;
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

void TP_GRA1175712875(TP_GRAPH_ABS_DES self) {
 FMAPTP291739594 L11;
 TUPTP_858321267 p = TUPTP_858321267_zero;
 FSETdTP L21;
 dTP ct;
 TP_GRAPH_ABS_DES add_self;
 TP_CLASS add_at;
 dTP add_ct;
 FSETdTP s;
 TUPTP_858321267 L3;
 dTP L4;
 dTP L5;
 BOOL L6;
 BOOL L71_;
 {
  struct FMAPTP1465407463_frame_struct other1_0;
FMAPTP1465407463_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,prog),tp_graph),des_tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = FMAPTP1465407463(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   p = L3;
   {
    struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname2 = &other2_0;
    L21 = p.t2;
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L4 = FSETdTP_eltbrdTP(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     ct = L4;
     L5 = ct;
     L6 = (*dTP_is1811059018[TAG(L5)])(L5);
     L71_=!(L6); 
     if (L71_) {
      add_self = self;
      add_at = p.t1;
      add_ct = ct;
      s = FMAPTP861983338(ATTR(add_self,tbl), add_at);
      s = FSETdT1016814448(s, add_ct);
      SATTR(add_self,tbl,FMAPTP551118428(ATTR(add_self,tbl), add_at, s));
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

void TP_GRA1175730507(TP_GRAPH_ABS_DES self) {
 FMAPTP775297600 L11;
 TUPTP_1668454172 p = TUPTP_1668454172_zero;
 FSETTP_CLASS L21;
 TP_GRAPH_ABS_DES add_self;
 TP_CLASS add_at;
 dTP add_ct;
 FSETdTP s;
 TUPTP_1668454172 L3;
 BOOL L4;
 BOOL L51_;
 TP_CLASS L6;
 {
  struct FMAPTP1065055386_frame_struct other1_0;
FMAPTP1065055386_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,prog),tp_graph),anc_tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = FMAPTP1065055386(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   p = L3;
   L4 = TP_CLA1568240525(p.t1);
   L51_=!(L4); 
   if (L51_) {
    {
     struct FSETTP340014621_frame_struct other2_0;
FSETTP340014621_frame noname2 = &other2_0;
     L21 = p.t2;
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      add_self = self;
      L6 = FSETTP340014621(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      add_at = L6;
      add_ct = ((dTP) p.t1);
      s = FMAPTP861983338(ATTR(add_self,tbl), add_at);
      s = FSETdT1016814448(s, add_ct);
      SATTR(add_self,tbl,FMAPTP551118428(ATTR(add_self,tbl), add_at, s));
     }
    }
    after_loop1: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void TP_GRA1187239150(TP_GRAPH_ABS_DES self) {
 TP_CLASS dob;
 FSETdTP tt;
 FSETdTP L11;
 dTP tp;
 TP_GRAPH_ABS_DES add_self;
 TP_CLASS add_at;
 dTP add_ct;
 FSETdTP s;
 dTP L2;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 dob = TP_BUI1325635093;
 tt = ATTR(ATTR(self,prog),tp_done);
 if ((tt==((FSETdTP) NULL))) {
  return;
 }
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = tt;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L2;
   L3 = tp;
   L4 = (*dTP_is1811059018[TAG(L3)])(L3);
   L51_=!(L4); 
   if (L51_) {
    add_self = self;
    add_at = dob;
    add_ct = tp;
    s = FMAPTP861983338(ATTR(add_self,tbl), add_at);
    s = FSETdT1016814448(s, add_ct);
    SATTR(add_self,tbl,FMAPTP551118428(ATTR(add_self,tbl), add_at, s));
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void TP_GRA628060055(TP_GRAPH self) {
 FSETdTP children1 = ((FSETdTP) NULL);
 FSETTP_CLASS parents = ((FSETTP_CLASS) NULL);
 dTP t;
 FSETdTP L11;
 AS_CLASS_DEF as;
 TP_CONTEXT con;
 AS_TYPE_SPEC ts;
 dTP tp;
 AS_TYPE_SPEC ts1;
 dTP tp1;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val;
 FSETTP_CLASS create_self;
 FSETTP_CLASS ret_val1;
 FSETdTP create_self1;
 FSETdTP ret_val2;
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
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val8;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val9;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val10;
 FSETTP_CLASS create_self2;
 FSETTP_CLASS ret_val11;
 FSETdTP create_self3;
 FSETdTP ret_val12;
 TP_CLASS is_external_self;
 BOOL ret_val13 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val14 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val15;
 TP_CLASS kind_self1;
 INT ret_val16 = INT_zero;
 TP_CLASS as_tree_self2;
 AS_CLASS_DEF ret_val17;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val20;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val21;
 dTP L2;
 dPARSE L3;
 IDENT L4;
 ARRAYdTP L5;
 INT L61_;
 dPARSE L7;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR Intype;
 extern STR thetype;
 extern STR appear487074765;
 extern STR Intype;
 extern STR thetype;
 dTP L15;
 extern STR appear643779015;
 FSETTP_CLASS L16;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 INT L20;
 INT L21;
 BOOL L221_;
 dPARSE L23;
 IDENT L24;
 ARRAYdTP L25;
 INT L261_;
 dPARSE L27;
 BOOL L28;
 BOOL L291_;
 INT L30;
 INT L31;
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
 extern STR Thetype;
 extern STR listst1370126727;
 extern STR initssubtypelist;
 FSETdTP L46;
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
   t = L2;
   if (t==NULL) {
    goto other110;
   } else
   switch (TAG(t)) {
    case TP_CLASS_tag:
     as_tree_self = ((TP_CLASS) t);
     if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L3 = ATTR(ATTR(as_tree_self,prog),parse);
      L4 = ATTR(as_tree_self,name1);
      L5 = ATTR(as_tree_self,params);
      L61_=(L5)==NULL?0:ASIZE((ARRAYdTP)L5); 
      L7 = L3;
      SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L7)])(L7, L4, L61_));
     }
     ret_val = ATTR(as_tree_self,my_as_tree);
     as = ret_val;
     L8 = (as==((AS_CLASS_DEF) NULL));
     L91_=!(L8); 
     if (L91_) {
      con = TP_CLA821242177(((TP_CLASS) t));
      L10 = (ATTR(as,under)==((AS_TYPE_SPEC) NULL));
      L121_=!(L10); 
      if (L121_) {
       create_self = ((FSETTP_CLASS) NULL);
       ret_val1 = ((FSETTP_CLASS) NULL);
       parents = ret_val1;
       create_self1 = ((FSETdTP) NULL);
       ret_val2 = ((FSETdTP) NULL);
       children1 = ret_val2;
       ts = ATTR(as,under);
       while (1) {
        if ((ts==((AS_TYPE_SPEC) NULL))) {
         goto after_loop1;
        }
        tp = TP_CON1800432689(con, ts);
        if (tp==NULL) {
         goto other111;
        } else
        switch (TAG(tp)) {
         case TP_CLASS_tag:
          L13 = TP_CLA1568240525(((TP_CLASS) tp));
          L141_=!(L13); 
          if (L141_) {
           plus_self = ((STR) &Intype);
           plus_sarg = TP_CLASS_strrSTR(((TP_CLASS) t));
           ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
           plus_self1 = ret_val3;
           plus_sarg1 = ((STR) &thetype);
           ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
           plus_self2 = ret_val4;
           plus_sarg2 = TP_CLASS_strrSTR(((TP_CLASS) tp));
           ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
           plus_self3 = ret_val5;
           plus_sarg3 = ((STR) &appear487074765);
           ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
           PROG_err_STR(ATTR(self,prog), ret_val6);
          }
          else {
           parents = FSETTP1186813482(parents, ((TP_CLASS) tp));
           children1 = FMAPTP861983338(ATTR(self,child_tbl), ((TP_CLASS) tp));
           children1 = FSETdT1016814448(children1, t);
           SATTR(self,child_tbl,FMAPTP551118428(ATTR(self,child_tbl), ((TP_CLASS) tp), children1));
          } break;
         default: ;
         other111: ;
          plus_self4 = ((STR) &Intype);
          plus_sarg4 = TP_CLASS_strrSTR(((TP_CLASS) t));
          ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
          plus_self5 = ret_val7;
          plus_sarg5 = ((STR) &thetype);
          ret_val8 = STR_ap2004550586(plus_self5, plus_sarg5);
          plus_self6 = ret_val8;
          L15 = tp;
          plus_sarg6 = (*dTP_strrSTR[TAG(L15)])(L15);
          ret_val9 = STR_ap2004550586(plus_self6, plus_sarg6);
          plus_self7 = ret_val9;
          plus_sarg7 = ((STR) &appear643779015);
          ret_val10 = STR_ap2004550586(plus_self7, plus_sarg7);
          PROG_err_STR(ATTR(self,prog), ret_val10);
        }
        ts = ATTR(ts,next);
       }
       after_loop1: ;
       L16 = FMAPTP2096448595(ATTR(self,par_tbl), ((TP_CLASS) t));
       parents = FSETTP633280235(L16, parents);
       SATTR(self,par_tbl,FMAPTP594824795(ATTR(self,par_tbl), ((TP_CLASS) t), parents));
      }
      L17 = (ATTR(as,over)==((AS_TYPE_SPEC) NULL));
      L181_=!(L17); 
      if (L181_) {
       create_self2 = ((FSETTP_CLASS) NULL);
       ret_val11 = ((FSETTP_CLASS) NULL);
       parents = ret_val11;
       create_self3 = ((FSETdTP) NULL);
       ret_val12 = ((FSETdTP) NULL);
       children1 = ret_val12;
       ts1 = ATTR(as,over);
       while (1) {
        if ((ts1==((AS_TYPE_SPEC) NULL))) {
         goto after_loop2;
        }
        tp1 = TP_CON1800432689(con, ts1);
        switch (TAG(tp1)) {
         case TP_CLASS_tag:
          is_external_self = ((TP_CLASS) tp1);
          kind_self = is_external_self;
          L20 = ATTR(kind_self,kind_cache);
          L21 = TP_KIN138039446;
          L221_=(L20)==(L21); 
          if (L221_) {
           as_tree_self1 = kind_self;
           if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
            L23 = ATTR(ATTR(as_tree_self1,prog),parse);
            L24 = ATTR(as_tree_self1,name1);
            L25 = ATTR(as_tree_self1,params);
            L261_=(L25)==NULL?0:ASIZE((ARRAYdTP)L25); 
            L27 = L23;
            SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L27)])(L27, L24, L261_));
           }
           ret_val15 = ATTR(as_tree_self1,my_as_tree);
           L28 = (ret_val15==((AS_CLASS_DEF) NULL));
           L291_=!(L28); 
           if (L291_) {
            SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
           }
          }
          ret_val14 = ATTR(kind_self,kind_cache);
          L30 = ret_val14;
          L31 = TP_KIND_ext_c_tp;
          L321_=(L30)==(L31); 
          if (L321_) {
           L19 = TRUE;
          } else {
           kind_self1 = is_external_self;
           L33 = ATTR(kind_self1,kind_cache);
           L34 = TP_KIN138039446;
           L351_=(L33)==(L34); 
           if (L351_) {
            as_tree_self2 = kind_self1;
            if ((ATTR(as_tree_self2,my_as_tree)==((AS_CLASS_DEF) NULL))) {
             L36 = ATTR(ATTR(as_tree_self2,prog),parse);
             L37 = ATTR(as_tree_self2,name1);
             L38 = ATTR(as_tree_self2,params);
             L391_=(L38)==NULL?0:ASIZE((ARRAYdTP)L38); 
             L40 = L36;
             SATTR(as_tree_self2,my_as_tree,(*dPARSE1581792481[TAG(L40)])(L40, L37, L391_));
            }
            ret_val17 = ATTR(as_tree_self2,my_as_tree);
            L41 = (ret_val17==((AS_CLASS_DEF) NULL));
            L421_=!(L41); 
            if (L421_) {
             SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
            }
           }
           ret_val16 = ATTR(kind_self1,kind_cache);
           L43 = ret_val16;
           L44 = TP_KIN462481068;
           L451_=(L43)==(L44); 
           L19 = L451_;
          }
          ret_val13 = L19;
          if (ret_val13) {
           plus_self8 = ((STR) &Thetype);
           plus_sarg8 = TP_CLASS_strrSTR(((TP_CLASS) t));
           ret_val18 = STR_ap2004550586(plus_self8, plus_sarg8);
           plus_self9 = ret_val18;
           plus_sarg9 = ((STR) &listst1370126727);
           ret_val19 = STR_ap2004550586(plus_self9, plus_sarg9);
           plus_self10 = ret_val19;
           plus_sarg10 = TP_CLASS_strrSTR(((TP_CLASS) tp1));
           ret_val20 = STR_ap2004550586(plus_self10, plus_sarg10);
           plus_self11 = ret_val20;
           plus_sarg11 = ((STR) &initssubtypelist);
           ret_val21 = STR_ap2004550586(plus_self11, plus_sarg11);
           PROG_err_STR(ATTR(self,prog), ret_val21);
          }
          else {
           children1 = FSETdT1016814448(children1, tp1);
           parents = FMAPTP2096448595(ATTR(self,par_tbl), ((TP_CLASS) tp1));
           parents = FSETTP1186813482(parents, ((TP_CLASS) t));
           SATTR(self,par_tbl,FMAPTP594824795(ATTR(self,par_tbl), ((TP_CLASS) tp1), parents));
          } break;
         default: ;
          FATAL("No applicable type in typecase\nin TP_GRAPH::make_family_members\n./Representation/tp.sa:1654:36");
        }
        ts1 = ATTR(ts1,next);
       }
       after_loop2: ;
       L46 = FMAPTP861983338(ATTR(self,child_tbl), ((TP_CLASS) t));
       children1 = FSETdT575601071(L46, children1);
       SATTR(self,child_tbl,FMAPTP551118428(ATTR(self,child_tbl), ((TP_CLASS) t), children1));
      }
     } break;
    default: ;
    other110: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS TP_CLA115798516(TP_CLA115798516_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 BOOL L1;
 BOOL L21_;
 TP_CLASS L5;
 TP_CLASS aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((TP_CLASS_TBL) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   /* loop index variable */
   frame->L31 = 0;
   frame->f_L41_ = TRUE;
   while (1) {
    if (frame->f_L41_) {
     frame->f_L41_ = FALSE;
     frame->L61 = frame->self;
     frame->L41_br=ASIZE((TP_CLASS_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((TP_CLASS_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((TP_CLASS) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->r;
     state1:;
    }
    L91_=INTPLUS(frame->L31,1); 
    frame->L31 = L91_;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS TP_CLA83919057(TP_CLA83919057_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 TP_CLASS aI_u_I;
 TP_CLASS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_CLASS_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((TP_CLASS_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TP_CLASS TP_GEN1057619759(TP_GEN1057619759_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 BOOL L1;
 BOOL L21_;
 TP_CLASS L5;
 TP_CLASS aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((TP_GEN_TBL) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   /* loop index variable */
   frame->L31 = 0;
   frame->f_L41_ = TRUE;
   while (1) {
    if (frame->f_L41_) {
     frame->f_L41_ = FALSE;
     frame->L61 = frame->self;
     frame->L41_br=ASIZE((TP_GEN_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((TP_GEN_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((TP_CLASS) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->r;
     state1:;
    }
    L91_=INTPLUS(frame->L31,1); 
    frame->L31 = L91_;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS TP_GEN798653528(TP_GEN798653528_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 TP_CLASS aI_u_I;
 TP_CLASS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_GEN_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((TP_GEN_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void TP_CLA1636324039(TP_CLA1636324039_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_CLASS_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((TP_CLASS_TBL)frame->self,frame->i_I_u_I,(TP_CLASS)frame->arg1); frame->i_I_u_I++;
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

void TP_GEN1943908786(TP_GEN1943908786_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_GEN_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((TP_GEN_TBL)frame->self,frame->i_I_u_I,(TP_CLASS)frame->arg1); frame->i_I_u_I++;
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
