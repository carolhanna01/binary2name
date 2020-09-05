#include "sather.h"

/* Layouts */

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

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

typedef struct ARRAYd1329429954_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1329429954_frame;

typedef struct ARRAYdAM_STMT_struct {/* layout for ARRAY{$AM_STMT} */
 OB_HEADER header;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *ARRAYdAM_STMT;

typedef struct ARRAYd615421366_frame_struct {
 INT state;
 ARRAYdAM_STMT self;/* Formal argument: self */
 dAM_STMT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd615421366_frame;

typedef struct ARRAYdLAYOUT_struct {/* layout for ARRAY{$LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *ARRAYdLAYOUT;

typedef struct ARRAYd1647530701_frame_struct {
 INT state;
 ARRAYdLAYOUT self;/* Formal argument: self */
 dLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1647530701_frame;

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

typedef struct CODE_FILE_ARRAY_struct {/* layout for CODE_FILE_ARRAY */
 OB_HEADER header;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *CODE_FILE_ARRAY;

typedef struct ELT_LTdTP_struct {/* layout for ELT_LT{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_LTdTP;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

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

typedef struct LAYOUT_ARRAY_struct {/* layout for LAYOUT_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *LAYOUT_ARRAY;

typedef struct LAYOUT1657363748_frame_struct {
 INT state;
 LAYOUT_ARRAY self;/* Formal argument: self */
 dLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT1657363748_frame;

typedef struct LAYOUT2103796202_frame_struct {
 INT state;
 LAYOUT_ARRAY self;/* Formal argument: self */
 dLAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT2103796202_frame;

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

typedef struct TP_ARRAY_struct {/* layout for TP_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *TP_ARRAY;

typedef struct TP_ARR212643368_frame_struct {
 INT state;
 TP_ARRAY self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ARR212643368_frame;

typedef struct TP_ARR205286314_frame_struct {
 INT state;
 TP_ARRAY self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ARR205286314_frame;

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

typedef struct TUPIDENTdTP_struct {/* layout for TUP{IDENT,$TP} */
 struct dTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTdTP;
static TUPIDENTdTP TUPIDENTdTP_zero;

typedef struct TUPIDENTdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTdTP immutable_part;
 } *TUPIDENTdTP_boxed;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

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

typedef struct ARRAYd1897420416_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1897420416_frame;

typedef struct ARRAYT747572380_struct {/* layout for ARRAY{TUP{$OB,NAMESPACE}} */
 OB_HEADER header;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *ARRAYT747572380;

typedef struct ARRAYT681600609_frame_struct {
 INT state;
 ARRAYT747572380 self;/* Formal argument: self */
 TUPdOBNAMESPACE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT681600609_frame;

typedef struct ARRAYTUPdOBSTR_struct {/* layout for ARRAY{TUP{$OB,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *ARRAYTUPdOBSTR;

typedef struct ARRAYT595865513_frame_struct {
 INT state;
 ARRAYTUPdOBSTR self;/* Formal argument: self */
 TUPdOBSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT595865513_frame;

typedef struct ARRAYT1418209173_struct {/* layout for ARRAY{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *ARRAYT1418209173;

typedef struct ARRAYT2024950707_frame_struct {
 INT state;
 ARRAYT1418209173 self;/* Formal argument: self */
 TUPdTPdLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT2024950707_frame;

typedef struct ARRAYT1153101058_struct {/* layout for ARRAY{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *ARRAYT1153101058;

typedef struct ARRAYT1283478039_frame_struct {
 INT state;
 ARRAYT1153101058 self;/* Formal argument: self */
 TUPdTPCODE_FILE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1283478039_frame;

typedef struct ARRAYT1562141251_struct {/* layout for ARRAY{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *ARRAYT1562141251;

typedef struct ARRAYT1505580189_frame_struct {
 INT state;
 ARRAYT1562141251 self;/* Formal argument: self */
 TUPdTP1710233051 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1505580189_frame;

typedef struct ARRAYTUPdTPINT_struct {/* layout for ARRAY{TUP{$TP,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *ARRAYTUPdTPINT;

typedef struct ARRAYT452391291_frame_struct {
 INT state;
 ARRAYTUPdTPINT self;/* Formal argument: self */
 TUPdTPINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT452391291_frame;

typedef struct ARRAYTUPIDENTINT_struct {/* layout for ARRAY{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *ARRAYTUPIDENTINT;

typedef struct ARRAYT1017446695_frame_struct {
 INT state;
 ARRAYTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1017446695_frame;

typedef struct ARRAYT348931282_struct {/* layout for ARRAY{TUP{SIG,ABSTRACT_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *ARRAYT348931282;

typedef struct ARRAYT1079066005_frame_struct {
 INT state;
 ARRAYT348931282 self;/* Formal argument: self */
 TUPSIG1754078736 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1079066005_frame;

typedef struct ARRAYT537394765_struct {/* layout for ARRAY{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *ARRAYT537394765;

typedef struct ARRAYT1269605207_frame_struct {
 INT state;
 ARRAYT537394765 self;/* Formal argument: self */
 TUPSIG578692189 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1269605207_frame;

typedef struct ARRAYT1759168384_struct {/* layout for ARRAY{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *ARRAYT1759168384;

typedef struct ARRAYT457901414_frame_struct {
 INT state;
 ARRAYT1759168384 self;/* Formal argument: self */
 TUPSIGARG_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT457901414_frame;

typedef struct ARRAYT1754348798_struct {/* layout for ARRAY{TUP{SIG,FRAME_LAYOUT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *ARRAYT1754348798;

typedef struct ARRAYT532396130_frame_struct {
 INT state;
 ARRAYT1754348798 self;/* Formal argument: self */
 TUPSIG2023533247 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT532396130_frame;

typedef struct ARRAYTUPSIGINT_struct {/* layout for ARRAY{TUP{SIG,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGINT_struct arr_part[1];
 } *ARRAYTUPSIGINT;

typedef struct ARRAYT1593820953_frame_struct {
 INT state;
 ARRAYTUPSIGINT self;/* Formal argument: self */
 TUPSIGINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1593820953_frame;

typedef struct ARRAYT1024744539_frame_struct {
 INT state;
 ARRAYTUPSIGINT self;/* Formal argument: self */
 TUPSIGINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1024744539_frame;

typedef struct ARRAYTUPSIGSIG_struct {/* layout for ARRAY{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *ARRAYTUPSIGSIG;

typedef struct ARRAYT1146052097_frame_struct {
 INT state;
 ARRAYTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1146052097_frame;

typedef struct ARRAYTUPSIGSTR_struct {/* layout for ARRAY{TUP{SIG,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *ARRAYTUPSIGSTR;

typedef struct ARRAYT1691893201_frame_struct {
 INT state;
 ARRAYTUPSIGSTR self;/* Formal argument: self */
 TUPSIGSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1691893201_frame;

typedef struct ARRAYT61112132_struct {/* layout for ARRAY{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *ARRAYT61112132;

typedef struct ARRAYT485937919_frame_struct {
 INT state;
 ARRAYT61112132 self;/* Formal argument: self */
 TUPSTRdCONFIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT485937919_frame;

typedef struct ARRAYT378612147_struct {/* layout for ARRAY{TUP{STR,FSET{STR}}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *ARRAYT378612147;

typedef struct ARRAYT1426073269_frame_struct {
 INT state;
 ARRAYT378612147 self;/* Formal argument: self */
 TUPSTRFSETSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1426073269_frame;

typedef struct ARRAYTUPSTRINT_struct {/* layout for ARRAY{TUP{STR,INT}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *ARRAYTUPSTRINT;

typedef struct ARRAYT306471177_frame_struct {
 INT state;
 ARRAYTUPSTRINT self;/* Formal argument: self */
 TUPSTRINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT306471177_frame;

typedef struct ARRAYTUPSTRSIG_struct {/* layout for ARRAY{TUP{STR,SIG}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *ARRAYTUPSTRSIG;

typedef struct ARRAYT141297679_frame_struct {
 INT state;
 ARRAYTUPSTRSIG self;/* Formal argument: self */
 TUPSTRSIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT141297679_frame;

typedef struct ARRAYTUPSTRSTR_struct {/* layout for ARRAY{TUP{STR,STR}} */
 OB_HEADER header;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *ARRAYTUPSTRSTR;

typedef struct ARRAYT404543425_frame_struct {
 INT state;
 ARRAYTUPSTRSTR self;/* Formal argument: self */
 TUPSTRSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT404543425_frame;

typedef struct ARRAYT1127505716_struct {/* layout for ARRAY{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *ARRAYT1127505716;

typedef struct ARRAYT518925702_frame_struct {
 INT state;
 ARRAYT1127505716 self;/* Formal argument: self */
 TUPTP_858321267 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT518925702_frame;

typedef struct ARRAYT1641775270_struct {/* layout for ARRAY{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *ARRAYT1641775270;

typedef struct ARRAYT1509169201_frame_struct {
 INT state;
 ARRAYT1641775270 self;/* Formal argument: self */
 TUPTP_1668454172 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYT1509169201_frame;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

typedef struct AS_STMT_LIST_struct {/* layout for AS_STMT_LIST */
 OB_HEADER header;
 struct dAS_STMT_struct *stmts;
 struct SFILE_ID_struct source1;
 } *AS_STMT_LIST;

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

typedef struct CODE_F809259241_frame_struct {
 INT state;
 CODE_FILE_ARRAY self;/* Formal argument: self */
 CODE_FILE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *CODE_F809259241_frame;

typedef struct CODE_F1378335655_frame_struct {
 INT state;
 CODE_FILE_ARRAY self;/* Formal argument: self */
 CODE_FILE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *CODE_F1378335655_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct LAYOUT_ARR_struct {/* layout for LAYOUT_ARR */
 OB_HEADER header;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *LAYOUT_ARR;

typedef struct LAYOUT1415846778_frame_struct {
 INT state;
 LAYOUT_ARR self;/* Formal argument: self */
 TUPIDENTdTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT1415846778_frame;

typedef struct LAYOUT2137691532_frame_struct {
 INT state;
 LAYOUT_ARR self;/* Formal argument: self */
 TUPIDENTdTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT2137691532_frame;

typedef struct ROUTTU467247893_struct {
 OB_HEADER header;
 BOOL (*funcptr)(void *, TUPSIGINT, TUPSIGINT);
} *ROUTTU467247893;

#include "tags.h"

/* Globals */

extern INT ARRAYd432394813;
extern INT AS_ARG1872943786;
extern INT AS_ARG685418392;
extern INT CODE_F1127805475;
extern INT LAYOUT1889414835;
extern INT LAYOUT558382138;
extern INT TP_ARR899550402;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_fltd;

/* Function declarations */


extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is650936518)(dTP, dTP);

extern RETURNED_CONST STR (**dLAYOUT_strrSTR)(dLAYOUT);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
BOOL AS_ARG1391068469(AS_ARG_MODE);
BOOL LAYOUT1938805770(LAYOUT_ARR, TUPIDENTdTP, TUPIDENTdTP);
BOOL STR_is171745744(STR, STR);
CODE_FILE CODE_F809259241(CODE_F809259241_frame);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT ARRAYd1897420416(ARRAYd1897420416_frame);
INT INT_do676118316(INT_do676118316_frame);
INT INT_up418511718(INT_up418511718_frame);
INT LAYOUT1351559216(LAYOUT_ARR, dTP);
INT RND_in1575450710(RND, INT, INT);
SFILE_ID AS_STM633666973(AS_STMT_LIST);
STR ARRAYT103792371(ARRAYT1759168384);
STR ARRAYT1278969222(ARRAYT1562141251);
STR ARRAYT1355045210(ARRAYTUPIDENTINT);
STR ARRAYT1384853867(ARRAYT348931282);
STR ARRAYT1450640385(ARRAYT1153101058);
STR ARRAYT1561971079(ARRAYT1127505716);
STR ARRAYT1574849761(ARRAYT61112132);
STR ARRAYT160571351(ARRAYT1641775270);
STR ARRAYT1658404076(ARRAYTUPSTRINT);
STR ARRAYT1718290516(ARRAYTUPdOBSTR);
STR ARRAYT1846205279(ARRAYT747572380);
STR ARRAYT189456178(ARRAYTUPSIGSTR);
STR ARRAYT1942593259(ARRAYT1754348798);
STR ARRAYT2122676784(ARRAYT378612147);
STR ARRAYT31468026(ARRAYTUPSIGSIG);
STR ARRAYT585031862(ARRAYT537394765);
STR ARRAYT614884074(ARRAYTUPSTRSTR);
STR ARRAYT835808278(ARRAYTUPSTRSIG);
STR ARRAYT835974574(ARRAYT1418209173);
STR ARRAYT854063824(ARRAYTUPSIGINT);
STR ARRAYT856660275(ARRAYTUPdTPINT);
STR ARRAYd1008822840(ARRAYdAM_STMT, dAM_STMT, INT);
STR ARRAYd1085727763(ARRAYdAM_STMT);
STR ARRAYd1580647420(ARRAYdAM_EXPR, dAM_EXPR, INT);
STR ARRAYd2023686048(ARRAYdAM_EXPR);
STR ARRAYd2065245377(ARRAYdAM_CONST);
STR ARRAYd497012239(ARRAYdAM_CONST, dAM_CONST, INT);
STR ARRAYd946747660(ARRAYdLAYOUT);
STR ARRAYdTP_strrSTR(ARRAYdTP);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TUPIDE2120384756(TUPIDENTINT);
STR TUPSIG1001949053(TUPSIG1754078736);
STR TUPSIG1147554980(TUPSIGSIG);
STR TUPSIG1626178992(TUPSIG578692189);
STR TUPSIG1970150778(TUPSIGINT);
STR TUPSIG610691414(TUPSIGARG_LAYOUT);
STR TUPSIG659332932(TUPSIG2023533247);
STR TUPSIG926630776(TUPSIGSTR);
STR TUPSTR1075084367(TUPSTRdCONFIG);
STR TUPSTR1520476266(TUPSTRINT);
STR TUPSTR1622911390(TUPSTRFSETSTR);
STR TUPSTR1730971028(TUPSTRSTR);
STR TUPSTR1951895232(TUPSTRSIG);
STR TUPTP_131070026(TUPTP_858321267);
STR TUPTP_371689873(TUPTP_1668454172);
STR TUPdOB1460589826(TUPdOBSTR);
STR TUPdOB1872884181(TUPdOBNAMESPACE);
STR TUPdTP1335739968(TUPdTPdLAYOUT);
STR TUPdTP1477319287(TUPdTPCODE_FILE);
STR TUPdTP1972747229(TUPdTPINT);
STR TUPdTP2068651704(TUPdTP1710233051);
TUPIDENTINT ARRAYT1017446695(ARRAYT1017446695_frame);
TUPIDENTdTP LAYOUT1415846778(LAYOUT1415846778_frame);
TUPSIG1754078736 ARRAYT1079066005(ARRAYT1079066005_frame);
TUPSIG2023533247 ARRAYT532396130(ARRAYT532396130_frame);
TUPSIG578692189 ARRAYT1269605207(ARRAYT1269605207_frame);
TUPSIGARG_LAYOUT ARRAYT457901414(ARRAYT457901414_frame);
TUPSIGINT ARRAYT1593820953(ARRAYT1593820953_frame);
TUPSIGSIG ARRAYT1146052097(ARRAYT1146052097_frame);
TUPSIGSTR ARRAYT1691893201(ARRAYT1691893201_frame);
TUPSTRFSETSTR ARRAYT1426073269(ARRAYT1426073269_frame);
TUPSTRINT ARRAYT306471177(ARRAYT306471177_frame);
TUPSTRSIG ARRAYT141297679(ARRAYT141297679_frame);
TUPSTRSTR ARRAYT404543425(ARRAYT404543425_frame);
TUPSTRdCONFIG ARRAYT485937919(ARRAYT485937919_frame);
TUPTP_1668454172 ARRAYT1509169201(ARRAYT1509169201_frame);
TUPTP_858321267 ARRAYT518925702(ARRAYT518925702_frame);
TUPdOBNAMESPACE ARRAYT681600609(ARRAYT681600609_frame);
TUPdOBSTR ARRAYT595865513(ARRAYT595865513_frame);
TUPdTP1710233051 ARRAYT1505580189(ARRAYT1505580189_frame);
TUPdTPCODE_FILE ARRAYT1283478039(ARRAYT1283478039_frame);
TUPdTPINT ARRAYT452391291(ARRAYT452391291_frame);
TUPdTPdLAYOUT ARRAYT2024950707(ARRAYT2024950707_frame);
dAM_CONST ARRAYd198808957(ARRAYd198808957_frame);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
dAM_STMT ARRAYd615421366(ARRAYd615421366_frame);
dLAYOUT ARRAYd1647530701(ARRAYd1647530701_frame);
dLAYOUT LAYOUT1657363748(LAYOUT1657363748_frame);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP ARRAYd244347909(ARRAYd244347909_frame);
dTP TP_ARR212643368(TP_ARR212643368_frame);
void ARRAYT1024744539(ARRAYT1024744539_frame);
void ARRAYT215015885(ARRAYTUPSIGINT, ROUTTU467247893);
void ARRAYd1239612935(ARRAYdTP, INT, INT);
void ARRAYd1329429954(ARRAYd1329429954_frame);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void ARRAYd911395334(ARRAYdTP, INT, INT);
void AS_ARG1328804463(AS_ARG_MODE, AS_ARG_MODE);
void CODE_F1378335655(CODE_F1378335655_frame);
void CODE_F2003422967(CODE_FILE_ARRAY, INT, INT);
void CODE_F989368327(CODE_FILE_ARRAY, INT, INT);
void LAYOUT1075979363(LAYOUT_ARR, INT, INT);
void LAYOUT1368381582(LAYOUT_ARR, INT, INT);
void LAYOUT2061379740(LAYOUT_ARRAY, INT, INT);
void LAYOUT2103796202(LAYOUT2103796202_frame);
void LAYOUT2137691532(LAYOUT2137691532_frame);
void LAYOUT805222467(LAYOUT_ARRAY, INT, INT);
void TP_ARR1424630359(TP_ARRAY, INT, INT);
void TP_ARR205286314(TP_ARR205286314_frame);
void TP_ARR273870804(TP_ARRAY, INT, INT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL AS_ARG1391068469(AS_ARG_MODE self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L6;
 BOOL L71_;
 L2 = ATTR(self,mod);
 L3 = AS_ARG685418392;
 L41_=(L2)==(L3); 
 if (L41_) {
  L1 = TRUE;
 } else {
  L5 = ATTR(self,mod);
  L6 = AS_ARG1872943786;
  L71_=(L5)==(L6); 
  L1 = L71_;
 }
 if (L1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL LAYOUT1938805770(LAYOUT_ARR self, TUPIDENTdTP e1, TUPIDENTdTP e2) {
 BOOL ret_val = BOOL_zero;
 dTP tp1;
 dTP tp2;
 INT a11 = INT_zero;
 INT a21 = INT_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L41_;
 dTP L5;
 BOOL L6;
 BOOL L71_;
 dTP L8;
 STR L9;
 dTP L10;
 tp1 = e1.t2;
 tp2 = e2.t2;
 a11 = LAYOUT1351559216(self, tp1);
 a21 = LAYOUT1351559216(self, tp2);
 L11_=(a11)==(a21); 
 L2 = L11_;
 L31_=!(L2); 
 if (L31_) {
  L41_=(a21)<(a11); 
  ret_val = L41_;
  return ret_val;
 }
 else {
  L5 = tp1;
  L6 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) tp2));
  L71_=!(L6); 
  if (L71_) {
   L8 = tp1;
   L9 = (*dTP_strrSTR[TAG(L8)])(L8);
   L10 = tp2;
   ret_val = STR_is171745744(L9, (*dTP_strrSTR[TAG(L10)])(L10));
   return ret_val;
  }
  else {
   ret_val = STR_is171745744(e1.t1.str, e2.t1.str);
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT LAYOUT1351559216(LAYOUT_ARR self, dTP t) {
 INT ret_val = INT_zero;
 dTP L11;
 dTP L2;
 dTP L3;
 dTP L4;
 L11 = t;
 L2 = L11;
 if ((*dTP_is242312711[TAG(L2)])(L2, ((OB) TP_BUILTIN_fltd))) {
  ret_val = 64;
  return ret_val;
 }
 else {
  L3 = L11;
  if ((*dTP_is242312711[TAG(L3)])(L3, ((OB) TP_BUILTIN_char))) {
   ret_val = 8;
   return ret_val;
  }
  else {
   L4 = L11;
   if ((*dTP_is242312711[TAG(L4)])(L4, ((OB) TP_BUILTIN_bool))) {
    ret_val = 1;
    return ret_val;
   }
   else {
    ret_val = 32;
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AS_STM633666973(AS_STMT_LIST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYT103792371(ARRAYT1759168384 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGARG_LAYOUT e = TUPSIGARG_LAYOUT_zero;
 ARRAYT1759168384 elt_str_self;
 TUPSIGARG_LAYOUT elt_str_e = TUPSIGARG_LAYOUT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1759168384 elt_str_self1;
 TUPSIGARG_LAYOUT elt_str_e1 = TUPSIGARG_LAYOUT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGARG_LAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1759168384)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1759168384)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG610691414(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG610691414(elt_str_e1);
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

STR ARRAYT1278969222(ARRAYT1562141251 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTP1710233051 e = TUPdTP1710233051_zero;
 ARRAYT1562141251 elt_str_self;
 TUPdTP1710233051 elt_str_e = TUPdTP1710233051_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1562141251 elt_str_self1;
 TUPdTP1710233051 elt_str_e1 = TUPdTP1710233051_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTP1710233051 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1562141251)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1562141251)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdTP2068651704(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdTP2068651704(elt_str_e1);
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

STR ARRAYT1355045210(ARRAYTUPIDENTINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPIDENTINT e = TUPIDENTINT_zero;
 ARRAYTUPIDENTINT elt_str_self;
 TUPIDENTINT elt_str_e = TUPIDENTINT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPIDENTINT elt_str_self1;
 TUPIDENTINT elt_str_e1 = TUPIDENTINT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPIDENTINT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPIDENTINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPIDENTINT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPIDE2120384756(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPIDE2120384756(elt_str_e1);
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

STR ARRAYT1384853867(ARRAYT348931282 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG1754078736 e = TUPSIG1754078736_zero;
 ARRAYT348931282 elt_str_self;
 TUPSIG1754078736 elt_str_e = TUPSIG1754078736_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT348931282 elt_str_self1;
 TUPSIG1754078736 elt_str_e1 = TUPSIG1754078736_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG1754078736 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT348931282)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT348931282)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG1001949053(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG1001949053(elt_str_e1);
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

STR ARRAYT1450640385(ARRAYT1153101058 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPCODE_FILE e = TUPdTPCODE_FILE_zero;
 ARRAYT1153101058 elt_str_self;
 TUPdTPCODE_FILE elt_str_e = TUPdTPCODE_FILE_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1153101058 elt_str_self1;
 TUPdTPCODE_FILE elt_str_e1 = TUPdTPCODE_FILE_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPCODE_FILE L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1153101058)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1153101058)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdTP1477319287(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdTP1477319287(elt_str_e1);
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

STR ARRAYT1561971079(ARRAYT1127505716 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPTP_858321267 e = TUPTP_858321267_zero;
 ARRAYT1127505716 elt_str_self;
 TUPTP_858321267 elt_str_e = TUPTP_858321267_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1127505716 elt_str_self1;
 TUPTP_858321267 elt_str_e1 = TUPTP_858321267_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPTP_858321267 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1127505716)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1127505716)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPTP_131070026(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPTP_131070026(elt_str_e1);
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

STR ARRAYT1574849761(ARRAYT61112132 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRdCONFIG e = TUPSTRdCONFIG_zero;
 ARRAYT61112132 elt_str_self;
 TUPSTRdCONFIG elt_str_e = TUPSTRdCONFIG_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT61112132 elt_str_self1;
 TUPSTRdCONFIG elt_str_e1 = TUPSTRdCONFIG_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRdCONFIG L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT61112132)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT61112132)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSTR1075084367(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSTR1075084367(elt_str_e1);
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

STR ARRAYT160571351(ARRAYT1641775270 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPTP_1668454172 e = TUPTP_1668454172_zero;
 ARRAYT1641775270 elt_str_self;
 TUPTP_1668454172 elt_str_e = TUPTP_1668454172_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1641775270 elt_str_self1;
 TUPTP_1668454172 elt_str_e1 = TUPTP_1668454172_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPTP_1668454172 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1641775270)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1641775270)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPTP_371689873(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPTP_371689873(elt_str_e1);
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

STR ARRAYT1658404076(ARRAYTUPSTRINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRINT e = TUPSTRINT_zero;
 ARRAYTUPSTRINT elt_str_self;
 TUPSTRINT elt_str_e = TUPSTRINT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSTRINT elt_str_self1;
 TUPSTRINT elt_str_e1 = TUPSTRINT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRINT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSTRINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSTRINT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSTR1520476266(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSTR1520476266(elt_str_e1);
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

STR ARRAYT1718290516(ARRAYTUPdOBSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdOBSTR e = TUPdOBSTR_zero;
 ARRAYTUPdOBSTR elt_str_self;
 TUPdOBSTR elt_str_e = TUPdOBSTR_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPdOBSTR elt_str_self1;
 TUPdOBSTR elt_str_e1 = TUPdOBSTR_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdOBSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPdOBSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPdOBSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdOB1460589826(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdOB1460589826(elt_str_e1);
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

STR ARRAYT1846205279(ARRAYT747572380 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdOBNAMESPACE e = TUPdOBNAMESPACE_zero;
 ARRAYT747572380 elt_str_self;
 TUPdOBNAMESPACE elt_str_e = TUPdOBNAMESPACE_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT747572380 elt_str_self1;
 TUPdOBNAMESPACE elt_str_e1 = TUPdOBNAMESPACE_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdOBNAMESPACE L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT747572380)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT747572380)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdOB1872884181(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdOB1872884181(elt_str_e1);
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

STR ARRAYT189456178(ARRAYTUPSIGSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGSTR e = TUPSIGSTR_zero;
 ARRAYTUPSIGSTR elt_str_self;
 TUPSIGSTR elt_str_e = TUPSIGSTR_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSIGSTR elt_str_self1;
 TUPSIGSTR elt_str_e1 = TUPSIGSTR_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSIGSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSIGSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG926630776(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG926630776(elt_str_e1);
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

STR ARRAYT1942593259(ARRAYT1754348798 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG2023533247 e = TUPSIG2023533247_zero;
 ARRAYT1754348798 elt_str_self;
 TUPSIG2023533247 elt_str_e = TUPSIG2023533247_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1754348798 elt_str_self1;
 TUPSIG2023533247 elt_str_e1 = TUPSIG2023533247_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG2023533247 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1754348798)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1754348798)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG659332932(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG659332932(elt_str_e1);
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

STR ARRAYT2122676784(ARRAYT378612147 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRFSETSTR e = TUPSTRFSETSTR_zero;
 ARRAYT378612147 elt_str_self;
 TUPSTRFSETSTR elt_str_e = TUPSTRFSETSTR_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT378612147 elt_str_self1;
 TUPSTRFSETSTR elt_str_e1 = TUPSTRFSETSTR_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRFSETSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT378612147)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT378612147)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSTR1622911390(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSTR1622911390(elt_str_e1);
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

STR ARRAYT31468026(ARRAYTUPSIGSIG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGSIG e = TUPSIGSIG_zero;
 ARRAYTUPSIGSIG elt_str_self;
 TUPSIGSIG elt_str_e = TUPSIGSIG_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSIGSIG elt_str_self1;
 TUPSIGSIG elt_str_e1 = TUPSIGSIG_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGSIG L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSIGSIG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSIGSIG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG1147554980(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG1147554980(elt_str_e1);
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

STR ARRAYT585031862(ARRAYT537394765 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG578692189 e = TUPSIG578692189_zero;
 ARRAYT537394765 elt_str_self;
 TUPSIG578692189 elt_str_e = TUPSIG578692189_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT537394765 elt_str_self1;
 TUPSIG578692189 elt_str_e1 = TUPSIG578692189_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG578692189 L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT537394765)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT537394765)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG1626178992(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG1626178992(elt_str_e1);
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

STR ARRAYT614884074(ARRAYTUPSTRSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRSTR e = TUPSTRSTR_zero;
 ARRAYTUPSTRSTR elt_str_self;
 TUPSTRSTR elt_str_e = TUPSTRSTR_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSTRSTR elt_str_self1;
 TUPSTRSTR elt_str_e1 = TUPSTRSTR_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRSTR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSTRSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSTRSTR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSTR1730971028(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSTR1730971028(elt_str_e1);
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

STR ARRAYT835808278(ARRAYTUPSTRSIG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRSIG e = TUPSTRSIG_zero;
 ARRAYTUPSTRSIG elt_str_self;
 TUPSTRSIG elt_str_e = TUPSTRSIG_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSTRSIG elt_str_self1;
 TUPSTRSIG elt_str_e1 = TUPSTRSIG_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRSIG L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSTRSIG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSTRSIG)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSTR1951895232(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSTR1951895232(elt_str_e1);
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

STR ARRAYT835974574(ARRAYT1418209173 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPdLAYOUT e = TUPdTPdLAYOUT_zero;
 ARRAYT1418209173 elt_str_self;
 TUPdTPdLAYOUT elt_str_e = TUPdTPdLAYOUT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYT1418209173 elt_str_self1;
 TUPdTPdLAYOUT elt_str_e1 = TUPdTPdLAYOUT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPdLAYOUT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYT1418209173)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYT1418209173)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdTP1335739968(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdTP1335739968(elt_str_e1);
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

STR ARRAYT854063824(ARRAYTUPSIGINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGINT e = TUPSIGINT_zero;
 ARRAYTUPSIGINT elt_str_self;
 TUPSIGINT elt_str_e = TUPSIGINT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPSIGINT elt_str_self1;
 TUPSIGINT elt_str_e1 = TUPSIGINT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGINT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPSIGINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPSIGINT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPSIG1970150778(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPSIG1970150778(elt_str_e1);
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

STR ARRAYT856660275(ARRAYTUPdTPINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPINT e = TUPdTPINT_zero;
 ARRAYTUPdTPINT elt_str_self;
 TUPdTPINT elt_str_e = TUPdTPINT_zero;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYTUPdTPINT elt_str_self1;
 TUPdTPINT elt_str_e1 = TUPdTPINT_zero;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPINT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYTUPdTPINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((ARRAYTUPdTPINT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   ret_val1 = TUPdTP1972747229(elt_str_e);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   ret_val2 = TUPdTP1972747229(elt_str_e1);
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

STR ARRAYd1008822840(ARRAYdAM_STMT self, dAM_STMT e, INT i) {
 STR ret_val;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 extern STR Unprintable;
 BOOL L1;
 BOOL L21_;
 if (e==NULL) {
  goto other5;
 } else
 switch (TAG(e)) {
  default: ;
  other5: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
   clear_self = buf1;
   L1 = (clear_self==((FSTR) NULL));
   L21_=!(L1); 
   if (L21_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val2;
   str_self1 = buf1;
   ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val1 = ret_val3;
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd1085727763(ARRAYdAM_STMT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dAM_STMT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 dAM_STMT L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYdAM_STMT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(dAM_STMT)ARR((ARRAYdAM_STMT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   res = FSTR_p1752847026(res, ARRAYd1008822840(self, e, i));
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   res = FSTR_p1752847026(L6, ARRAYd1008822840(self, e, i));
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd1580647420(ARRAYdAM_EXPR self, dAM_EXPR e, INT i) {
 STR ret_val;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 extern STR Unprintable;
 BOOL L1;
 BOOL L21_;
 if (e==NULL) {
  goto other4;
 } else
 switch (TAG(e)) {
  default: ;
  other4: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
   clear_self = buf1;
   L1 = (clear_self==((FSTR) NULL));
   L21_=!(L1); 
   if (L21_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val2;
   str_self1 = buf1;
   ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val1 = ret_val3;
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd2023686048(ARRAYdAM_EXPR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dAM_EXPR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 dAM_EXPR L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYdAM_EXPR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(dAM_EXPR)ARR((ARRAYdAM_EXPR)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   res = FSTR_p1752847026(res, ARRAYd1580647420(self, e, i));
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   res = FSTR_p1752847026(L6, ARRAYd1580647420(self, e, i));
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd2065245377(ARRAYdAM_CONST self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dAM_CONST e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 dAM_CONST L41_;
 BOOL L51_;
 extern STR name3;
 FSTR L6;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYdAM_CONST)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(dAM_CONST)ARR((ARRAYdAM_CONST)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   res = FSTR_p1752847026(res, ARRAYd497012239(self, e, i));
  }
  else {
   L6 = FSTR_p1752847026(res, ((STR) &name3));
   res = FSTR_p1752847026(L6, ARRAYd497012239(self, e, i));
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd497012239(ARRAYdAM_CONST self, dAM_CONST e, INT i) {
 STR ret_val;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 extern STR Unprintable;
 BOOL L1;
 BOOL L21_;
 if (e==NULL) {
  goto other3;
 } else
 switch (TAG(e)) {
  default: ;
  other3: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
   clear_self = buf1;
   L1 = (clear_self==((FSTR) NULL));
   L21_=!(L1); 
   if (L21_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val2;
   str_self1 = buf1;
   ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val1 = ret_val3;
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ARRAYd946747660(ARRAYdLAYOUT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dLAYOUT e;
 ARRAYdLAYOUT elt_str_self;
 dLAYOUT elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYdLAYOUT elt_str_self1;
 dLAYOUT elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 dLAYOUT L41_;
 BOOL L51_;
 dLAYOUT L6;
 extern STR name3;
 FSTR L7;
 dLAYOUT L8;
 INT L91_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYdLAYOUT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(dLAYOUT)ARR((ARRAYdLAYOUT)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   L6 = elt_str_e;
   ret_val1 = (*dLAYOUT_strrSTR[TAG(L6)])(L6);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L7 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   L8 = elt_str_e1;
   ret_val2 = (*dLAYOUT_strrSTR[TAG(L8)])(L8);
   res = FSTR_p1752847026(L7, ret_val2);
  }
  L91_=INTPLUS(i,1); 
  i = L91_;
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

STR ARRAYdTP_strrSTR(ARRAYdTP self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dTP e;
 ARRAYdTP elt_str_self;
 dTP elt_str_e;
 INT elt_str_i = INT_zero;
 STR ret_val1;
 ARRAYdTP elt_str_self1;
 dTP elt_str_e1;
 INT elt_str_i1 = INT_zero;
 STR ret_val2;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 dTP L41_;
 BOOL L51_;
 dTP L6;
 extern STR name3;
 FSTR L7;
 dTP L8;
 INT L91_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=(self)==NULL?0:ASIZE((ARRAYdTP)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=(dTP)ARR((ARRAYdTP)self,i); 
  e = L41_;
  L51_=(i)==(0); 
  if (L51_) {
   elt_str_self = self;
   elt_str_e = e;
   elt_str_i = i;
   L6 = elt_str_e;
   ret_val1 = (*dTP_strrSTR[TAG(L6)])(L6);
   res = FSTR_p1752847026(res, ret_val1);
  }
  else {
   L7 = FSTR_p1752847026(res, ((STR) &name3));
   elt_str_self1 = self;
   elt_str_e1 = e;
   elt_str_i1 = i;
   L8 = elt_str_e1;
   ret_val2 = (*dTP_strrSTR[TAG(L8)])(L8);
   res = FSTR_p1752847026(L7, ret_val2);
  }
  L91_=INTPLUS(i,1); 
  i = L91_;
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

void ARRAYT215015885(ARRAYTUPSIGINT self, ROUTTU467247893 lt) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 TUPSIGINT e = TUPSIGINT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 INT L61_;
 INT L7;
 INT L81_;
 INT L51_,L51_m;
 INT L9;
 TUPSIGINT L101_;
 INT L131_;
 INT L121_,L121_m;
 INT L14;
 BOOL L151_;
 ROUTTU467247893 L17;
 TUPSIGINT L181_;
 TUPSIGINT L19;
 INT L201_;
 INT L22;
 INT L241_;
 INT L25;
 TUPSIGINT L261_;
 TUPSIGINT L27;
 if ((self==((ARRAYTUPSIGINT) NULL))) {
  return;
 }
 {
  BOOL f_L51_ = TRUE;
  L11 = 1;
  L61_=ASIZE((ARRAYTUPSIGINT)self); 
  L7 = L61_;
  L81_=INTMINUS(L7,1); 
  L21 = L81_;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L101_=ARR((ARRAYTUPSIGINT)self,i); 
   e = L101_;
   {
    BOOL f_L121_ = TRUE;
    L131_=INTMINUS(i,1); 
    L31 = L131_;
    L41 = -1;
    L121_=L31+1;L121_m=L41; 
    while (1) {
     if(L121_--<=L121_m)  goto after_loop1; 
     j = L121_;
     L151_=(j)<(0); 
     if (L151_) {
      SARR((ARRAYTUPSIGINT)self,0,e); 
      ;
      goto after_loop1;
     }
     else {
      L17 = lt;
      L181_=ARR((ARRAYTUPSIGINT)self,j); 
      L19 = L181_;
      if ((*(L17->funcptr))(L17,L19,e)) {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       SARR((ARRAYTUPSIGINT)self,L22,e); 
       ;
       goto after_loop1;
      }
      else {
       L241_=INTPLUS(j,1); 
       L25 = L241_;
       L261_=ARR((ARRAYTUPSIGINT)self,j); 
       L27 = L261_;
       SARR((ARRAYTUPSIGINT)self,L25,L27); 
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

void ARRAYd1239612935(ARRAYdTP self, INT l, INT u) {
 INT r = INT_zero;
 dTP t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dTP s;
 ARRAYdTP elt_lt_self;
 dTP elt_lt_e1;
 dTP elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTdTP elt_lt_self1;
 dTP elt_lt_e11;
 dTP elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 dTP L71_;
 dTP L81_;
 dTP L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 dTP L161_;
 dTP L17;
 INT L181_;
 dTP L191_;
 dTP L201_;
 dTP L22;
 dTP L251_;
 dTP L261_;
 dTP L27;
 INT L301_;
 INT L31;
 BOOL L321_;
 INT L331_;
 INT L341_;
 INT L35;
 BOOL L361_;
 INT L371_;
 INT L38;
 L3 = ARRAYd432394813;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(dTP)ARR((ARRAYdTP)self,r); 
  t = L71_;
  L81_=(dTP)ARR((ARRAYdTP)self,l); 
  L9 = L81_;
  SARR((ARRAYdTP)self,r,(dTP)L9); 
  ;
  SARR((ARRAYdTP)self,l,(dTP)t); 
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
    L161_=(dTP)ARR((ARRAYdTP)self,i); 
    elt_lt_e1 = L161_;
    elt_lt_e2 = t;
    elt_lt_self1 = ((ELT_LTdTP) NULL);
    elt_lt_e11 = elt_lt_e1;
    elt_lt_e21 = elt_lt_e2;
    L17 = elt_lt_e11;
    ret_val1 = (*dTP_is650936518[TAG(L17)])(L17, elt_lt_e21);
    ret_val = ret_val1;
    if (ret_val) {
     L181_=INTPLUS(m,1); 
     m = L181_;
     L191_=(dTP)ARR((ARRAYdTP)self,m); 
     s = L191_;
     L201_=(dTP)ARR((ARRAYdTP)self,i); 
     L22 = L201_;
     SARR((ARRAYdTP)self,m,(dTP)L22); 
     ;
     SARR((ARRAYdTP)self,i,(dTP)s); 
     ;
    }
   }
  }
  after_loop: ;
  L251_=(dTP)ARR((ARRAYdTP)self,l); 
  t = L251_;
  L261_=(dTP)ARR((ARRAYdTP)self,m); 
  L27 = L261_;
  SARR((ARRAYdTP)self,l,(dTP)L27); 
  ;
  SARR((ARRAYdTP)self,m,(dTP)t); 
  ;
  L301_=INTMINUS(m,1); 
  L31 = L301_;
  L321_=(l)<(L31); 
  if (L321_) {
   L331_=INTMINUS(m,1); 
   ARRAYd1239612935(self, l, L331_);
  }
  L341_=INTPLUS(m,1); 
  L35 = L341_;
  L361_=(L35)<(u); 
  if (L361_) {
   L371_=INTPLUS(m,1); 
   L38 = L371_;
   ARRAYd1239612935(self, L38, u);
  }
 }
 else {
  ARRAYd911395334(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void ARRAYd911395334(ARRAYdTP self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dTP e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 ARRAYdTP elt_lt_self;
 dTP elt_lt_e1;
 dTP elt_lt_e2;
 BOOL ret_val = BOOL_zero;
 ELT_LTdTP elt_lt_self1;
 dTP elt_lt_e11;
 dTP elt_lt_e21;
 BOOL ret_val1 = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 dTP L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 dTP L161_;
 dTP L17;
 INT L181_;
 INT L19;
 INT L221_;
 INT L23;
 dTP L241_;
 dTP L25;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(dTP)ARR((ARRAYdTP)self,i); 
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
      SARR((ARRAYdTP)self,l,(dTP)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(dTP)ARR((ARRAYdTP)self,j); 
      elt_lt_e1 = L161_;
      elt_lt_e2 = e;
      elt_lt_self1 = ((ELT_LTdTP) NULL);
      elt_lt_e11 = elt_lt_e1;
      elt_lt_e21 = elt_lt_e2;
      L17 = elt_lt_e11;
      ret_val1 = (*dTP_is650936518[TAG(L17)])(L17, elt_lt_e21);
      ret_val = ret_val1;
      if (ret_val) {
       L181_=INTPLUS(j,1); 
       L19 = L181_;
       SARR((ARRAYdTP)self,L19,(dTP)e); 
       ;
       goto after_loop1;
      }
      else {
       L221_=INTPLUS(j,1); 
       L23 = L221_;
       L241_=(dTP)ARR((ARRAYdTP)self,j); 
       L25 = L241_;
       SARR((ARRAYdTP)self,L23,(dTP)L25); 
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

void AS_ARG1328804463(AS_ARG_MODE self, AS_ARG_MODE l) {
 AS_ARG_MODE last1;
 if ((ATTR(self,next)==((AS_ARG_MODE) NULL))) {
  SATTR(self,next,l);
  return;
 }
 last1 = ATTR(self,next);
 while (1) {
  if ((ATTR(last1,next)==((AS_ARG_MODE) NULL))) {
   goto after_loop;
  }
  last1 = ATTR(last1,next);
 }
 after_loop: ;
 SATTR(last1,next,l);
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F2003422967(CODE_FILE_ARRAY self, INT l, INT u) {
 INT r = INT_zero;
 CODE_FILE t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CODE_FILE s;
 CODE_FILE_ARRAY elt_lt_self;
 CODE_FILE elt_lt_x;
 CODE_FILE elt_lt_y;
 BOOL ret_val = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 CODE_FILE L71_;
 CODE_FILE L81_;
 CODE_FILE L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 CODE_FILE L161_;
 INT L171_;
 CODE_FILE L181_;
 CODE_FILE L191_;
 CODE_FILE L20;
 CODE_FILE L241_;
 CODE_FILE L251_;
 CODE_FILE L26;
 INT L291_;
 INT L30;
 BOOL L311_;
 INT L321_;
 INT L331_;
 INT L34;
 BOOL L351_;
 INT L361_;
 INT L37;
 L3 = CODE_F1127805475;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,r); 
  t = L71_;
  L81_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,l); 
  L9 = L81_;
  SARR((CODE_FILE_ARRAY)self,r,(CODE_FILE)L9); 
  ;
  SARR((CODE_FILE_ARRAY)self,l,(CODE_FILE)t); 
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
    L161_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,i); 
    elt_lt_x = L161_;
    elt_lt_y = t;
    ret_val = STR_is171745744(ATTR(elt_lt_x,name1), ATTR(elt_lt_y,name1));
    if (ret_val) {
     L171_=INTPLUS(m,1); 
     m = L171_;
     L181_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,m); 
     s = L181_;
     L191_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,i); 
     L20 = L191_;
     SARR((CODE_FILE_ARRAY)self,m,(CODE_FILE)L20); 
     ;
     SARR((CODE_FILE_ARRAY)self,i,(CODE_FILE)s); 
     ;
    }
   }
  }
  after_loop: ;
  L241_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,l); 
  t = L241_;
  L251_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,m); 
  L26 = L251_;
  SARR((CODE_FILE_ARRAY)self,l,(CODE_FILE)L26); 
  ;
  SARR((CODE_FILE_ARRAY)self,m,(CODE_FILE)t); 
  ;
  L291_=INTMINUS(m,1); 
  L30 = L291_;
  L311_=(l)<(L30); 
  if (L311_) {
   L321_=INTMINUS(m,1); 
   CODE_F2003422967(self, l, L321_);
  }
  L331_=INTPLUS(m,1); 
  L34 = L331_;
  L351_=(L34)<(u); 
  if (L351_) {
   L361_=INTPLUS(m,1); 
   L37 = L361_;
   CODE_F2003422967(self, L37, u);
  }
 }
 else {
  CODE_F989368327(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F989368327(CODE_FILE_ARRAY self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CODE_FILE e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 CODE_FILE_ARRAY elt_lt_self;
 CODE_FILE elt_lt_x;
 CODE_FILE elt_lt_y;
 BOOL ret_val = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 CODE_FILE L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 CODE_FILE L161_;
 INT L171_;
 INT L18;
 INT L201_;
 INT L22;
 CODE_FILE L231_;
 CODE_FILE L24;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,i); 
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
      SARR((CODE_FILE_ARRAY)self,l,(CODE_FILE)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,j); 
      elt_lt_x = L161_;
      elt_lt_y = e;
      ret_val = STR_is171745744(ATTR(elt_lt_x,name1), ATTR(elt_lt_y,name1));
      if (ret_val) {
       L171_=INTPLUS(j,1); 
       L18 = L171_;
       SARR((CODE_FILE_ARRAY)self,L18,(CODE_FILE)e); 
       ;
       goto after_loop1;
      }
      else {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       L231_=(CODE_FILE)ARR((CODE_FILE_ARRAY)self,j); 
       L24 = L231_;
       SARR((CODE_FILE_ARRAY)self,L22,(CODE_FILE)L24); 
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

void LAYOUT1075979363(LAYOUT_ARR self, INT l, INT u) {
 INT r = INT_zero;
 TUPIDENTdTP t = TUPIDENTdTP_zero;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 TUPIDENTdTP s = TUPIDENTdTP_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 TUPIDENTdTP L71_;
 TUPIDENTdTP L81_;
 TUPIDENTdTP L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 TUPIDENTdTP L161_;
 TUPIDENTdTP L17;
 INT L181_;
 TUPIDENTdTP L191_;
 TUPIDENTdTP L201_;
 TUPIDENTdTP L22;
 TUPIDENTdTP L251_;
 TUPIDENTdTP L261_;
 TUPIDENTdTP L27;
 INT L301_;
 INT L31;
 BOOL L321_;
 INT L331_;
 INT L341_;
 INT L35;
 BOOL L361_;
 INT L371_;
 INT L38;
 L3 = LAYOUT1889414835;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=ARR((LAYOUT_ARR)self,r); 
  t = L71_;
  L81_=ARR((LAYOUT_ARR)self,l); 
  L9 = L81_;
  SARR((LAYOUT_ARR)self,r,L9); 
  ;
  SARR((LAYOUT_ARR)self,l,t); 
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
    L161_=ARR((LAYOUT_ARR)self,i); 
    L17 = L161_;
    if (LAYOUT1938805770(self, L17, t)) {
     L181_=INTPLUS(m,1); 
     m = L181_;
     L191_=ARR((LAYOUT_ARR)self,m); 
     s = L191_;
     L201_=ARR((LAYOUT_ARR)self,i); 
     L22 = L201_;
     SARR((LAYOUT_ARR)self,m,L22); 
     ;
     SARR((LAYOUT_ARR)self,i,s); 
     ;
    }
   }
  }
  after_loop: ;
  L251_=ARR((LAYOUT_ARR)self,l); 
  t = L251_;
  L261_=ARR((LAYOUT_ARR)self,m); 
  L27 = L261_;
  SARR((LAYOUT_ARR)self,l,L27); 
  ;
  SARR((LAYOUT_ARR)self,m,t); 
  ;
  L301_=INTMINUS(m,1); 
  L31 = L301_;
  L321_=(l)<(L31); 
  if (L321_) {
   L331_=INTMINUS(m,1); 
   LAYOUT1075979363(self, l, L331_);
  }
  L341_=INTPLUS(m,1); 
  L35 = L341_;
  L361_=(L35)<(u); 
  if (L361_) {
   L371_=INTPLUS(m,1); 
   L38 = L371_;
   LAYOUT1075979363(self, L38, u);
  }
 }
 else {
  LAYOUT1368381582(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void LAYOUT1368381582(LAYOUT_ARR self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 TUPIDENTdTP e = TUPIDENTdTP_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 TUPIDENTdTP L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 TUPIDENTdTP L161_;
 TUPIDENTdTP L17;
 INT L181_;
 INT L19;
 INT L221_;
 INT L23;
 TUPIDENTdTP L241_;
 TUPIDENTdTP L25;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=ARR((LAYOUT_ARR)self,i); 
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
      SARR((LAYOUT_ARR)self,l,e); 
      ;
      goto after_loop1;
     }
     else {
      L161_=ARR((LAYOUT_ARR)self,j); 
      L17 = L161_;
      if (LAYOUT1938805770(self, L17, e)) {
       L181_=INTPLUS(j,1); 
       L19 = L181_;
       SARR((LAYOUT_ARR)self,L19,e); 
       ;
       goto after_loop1;
      }
      else {
       L221_=INTPLUS(j,1); 
       L23 = L221_;
       L241_=ARR((LAYOUT_ARR)self,j); 
       L25 = L241_;
       SARR((LAYOUT_ARR)self,L23,L25); 
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

void LAYOUT2061379740(LAYOUT_ARRAY self, INT l, INT u) {
 INT r = INT_zero;
 dLAYOUT t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dLAYOUT s;
 LAYOUT_ARRAY elt_lt_self;
 dLAYOUT elt_lt_x;
 dLAYOUT elt_lt_y;
 BOOL ret_val = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 dLAYOUT L71_;
 dLAYOUT L81_;
 dLAYOUT L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 dLAYOUT L161_;
 dLAYOUT L17;
 STR L18;
 dLAYOUT L19;
 INT L201_;
 dLAYOUT L221_;
 dLAYOUT L231_;
 dLAYOUT L24;
 dLAYOUT L271_;
 dLAYOUT L281_;
 dLAYOUT L29;
 INT L321_;
 INT L33;
 BOOL L341_;
 INT L351_;
 INT L361_;
 INT L37;
 BOOL L381_;
 INT L391_;
 INT L40;
 L3 = LAYOUT558382138;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,r); 
  t = L71_;
  L81_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,l); 
  L9 = L81_;
  SARR((LAYOUT_ARRAY)self,r,(dLAYOUT)L9); 
  ;
  SARR((LAYOUT_ARRAY)self,l,(dLAYOUT)t); 
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
    L161_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,i); 
    elt_lt_x = L161_;
    elt_lt_y = t;
    L17 = elt_lt_x;
    L18 = (*dLAYOUT_strrSTR[TAG(L17)])(L17);
    L19 = elt_lt_y;
    ret_val = STR_is171745744(L18, (*dLAYOUT_strrSTR[TAG(L19)])(L19));
    if (ret_val) {
     L201_=INTPLUS(m,1); 
     m = L201_;
     L221_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,m); 
     s = L221_;
     L231_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,i); 
     L24 = L231_;
     SARR((LAYOUT_ARRAY)self,m,(dLAYOUT)L24); 
     ;
     SARR((LAYOUT_ARRAY)self,i,(dLAYOUT)s); 
     ;
    }
   }
  }
  after_loop: ;
  L271_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,l); 
  t = L271_;
  L281_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,m); 
  L29 = L281_;
  SARR((LAYOUT_ARRAY)self,l,(dLAYOUT)L29); 
  ;
  SARR((LAYOUT_ARRAY)self,m,(dLAYOUT)t); 
  ;
  L321_=INTMINUS(m,1); 
  L33 = L321_;
  L341_=(l)<(L33); 
  if (L341_) {
   L351_=INTMINUS(m,1); 
   LAYOUT2061379740(self, l, L351_);
  }
  L361_=INTPLUS(m,1); 
  L37 = L361_;
  L381_=(L37)<(u); 
  if (L381_) {
   L391_=INTPLUS(m,1); 
   L40 = L391_;
   LAYOUT2061379740(self, L40, u);
  }
 }
 else {
  LAYOUT805222467(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 0

void LAYOUT805222467(LAYOUT_ARRAY self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dLAYOUT e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 LAYOUT_ARRAY elt_lt_self;
 dLAYOUT elt_lt_x;
 dLAYOUT elt_lt_y;
 BOOL ret_val = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 dLAYOUT L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 dLAYOUT L161_;
 dLAYOUT L17;
 STR L18;
 dLAYOUT L19;
 INT L201_;
 INT L22;
 INT L241_;
 INT L25;
 dLAYOUT L261_;
 dLAYOUT L27;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,i); 
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
      SARR((LAYOUT_ARRAY)self,l,(dLAYOUT)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,j); 
      elt_lt_x = L161_;
      elt_lt_y = e;
      L17 = elt_lt_x;
      L18 = (*dLAYOUT_strrSTR[TAG(L17)])(L17);
      L19 = elt_lt_y;
      ret_val = STR_is171745744(L18, (*dLAYOUT_strrSTR[TAG(L19)])(L19));
      if (ret_val) {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       SARR((LAYOUT_ARRAY)self,L22,(dLAYOUT)e); 
       ;
       goto after_loop1;
      }
      else {
       L241_=INTPLUS(j,1); 
       L25 = L241_;
       L261_=(dLAYOUT)ARR((LAYOUT_ARRAY)self,j); 
       L27 = L261_;
       SARR((LAYOUT_ARRAY)self,L25,(dLAYOUT)L27); 
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

void TP_ARR1424630359(TP_ARRAY self, INT l, INT u) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dTP e;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 TP_ARRAY elt_lt_self;
 dTP elt_lt_a;
 dTP elt_lt_b;
 BOOL ret_val = BOOL_zero;
 INT L61_;
 INT L51_,L51_m;
 INT L7;
 dTP L81_;
 INT L101_;
 INT L121_;
 INT L91_,L91_m;
 INT L13;
 BOOL L141_;
 dTP L161_;
 dTP L17;
 STR L18;
 dTP L19;
 INT L201_;
 INT L22;
 INT L241_;
 INT L25;
 dTP L261_;
 dTP L27;
 {
  BOOL f_L51_ = TRUE;
  L61_=INTPLUS(l,1); 
  L11 = L61_;
  L21 = u;
  L51_=L11-1;L51_m=L21; 
  while (1) {
   if(L51_++>=L51_m)  goto after_loop; 
   i = L51_;
   L81_=(dTP)ARR((TP_ARRAY)self,i); 
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
      SARR((TP_ARRAY)self,l,(dTP)e); 
      ;
      goto after_loop1;
     }
     else {
      elt_lt_self = self;
      L161_=(dTP)ARR((TP_ARRAY)self,j); 
      elt_lt_a = L161_;
      elt_lt_b = e;
      L17 = elt_lt_a;
      L18 = (*dTP_strrSTR[TAG(L17)])(L17);
      L19 = elt_lt_b;
      ret_val = STR_is171745744(L18, (*dTP_strrSTR[TAG(L19)])(L19));
      if (ret_val) {
       L201_=INTPLUS(j,1); 
       L22 = L201_;
       SARR((TP_ARRAY)self,L22,(dTP)e); 
       ;
       goto after_loop1;
      }
      else {
       L241_=INTPLUS(j,1); 
       L25 = L241_;
       L261_=(dTP)ARR((TP_ARRAY)self,j); 
       L27 = L261_;
       SARR((TP_ARRAY)self,L25,(dTP)L27); 
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

void TP_ARR273870804(TP_ARRAY self, INT l, INT u) {
 INT r = INT_zero;
 dTP t;
 INT m = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dTP s;
 TP_ARRAY elt_lt_self;
 dTP elt_lt_a;
 dTP elt_lt_b;
 BOOL ret_val = BOOL_zero;
 INT L3;
 INT L41_;
 INT L5;
 BOOL L61_;
 dTP L71_;
 dTP L81_;
 dTP L9;
 INT L141_;
 INT L131_,L131_m;
 INT L15;
 dTP L161_;
 dTP L17;
 STR L18;
 dTP L19;
 INT L201_;
 dTP L221_;
 dTP L231_;
 dTP L24;
 dTP L271_;
 dTP L281_;
 dTP L29;
 INT L321_;
 INT L33;
 BOOL L341_;
 INT L351_;
 INT L361_;
 INT L37;
 BOOL L381_;
 INT L391_;
 INT L40;
 L3 = TP_ARR899550402;
 L41_=INTMINUS(u,l); 
 L5 = L41_;
 L61_=(L3)<(L5); 
 if (L61_) {
  r = RND_in1575450710(((RND) NULL), l, u);
  L71_=(dTP)ARR((TP_ARRAY)self,r); 
  t = L71_;
  L81_=(dTP)ARR((TP_ARRAY)self,l); 
  L9 = L81_;
  SARR((TP_ARRAY)self,r,(dTP)L9); 
  ;
  SARR((TP_ARRAY)self,l,(dTP)t); 
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
    L161_=(dTP)ARR((TP_ARRAY)self,i); 
    elt_lt_a = L161_;
    elt_lt_b = t;
    L17 = elt_lt_a;
    L18 = (*dTP_strrSTR[TAG(L17)])(L17);
    L19 = elt_lt_b;
    ret_val = STR_is171745744(L18, (*dTP_strrSTR[TAG(L19)])(L19));
    if (ret_val) {
     L201_=INTPLUS(m,1); 
     m = L201_;
     L221_=(dTP)ARR((TP_ARRAY)self,m); 
     s = L221_;
     L231_=(dTP)ARR((TP_ARRAY)self,i); 
     L24 = L231_;
     SARR((TP_ARRAY)self,m,(dTP)L24); 
     ;
     SARR((TP_ARRAY)self,i,(dTP)s); 
     ;
    }
   }
  }
  after_loop: ;
  L271_=(dTP)ARR((TP_ARRAY)self,l); 
  t = L271_;
  L281_=(dTP)ARR((TP_ARRAY)self,m); 
  L29 = L281_;
  SARR((TP_ARRAY)self,l,(dTP)L29); 
  ;
  SARR((TP_ARRAY)self,m,(dTP)t); 
  ;
  L321_=INTMINUS(m,1); 
  L33 = L321_;
  L341_=(l)<(L33); 
  if (L341_) {
   L351_=INTMINUS(m,1); 
   TP_ARR273870804(self, l, L351_);
  }
  L361_=INTPLUS(m,1); 
  L37 = L361_;
  L381_=(L37)<(u); 
  if (L381_) {
   L391_=INTPLUS(m,1); 
   L40 = L391_;
   TP_ARR273870804(self, L40, u);
  }
 }
 else {
  TP_ARR1424630359(self, l, u);
 }
}


#undef IS_ITER
#define IS_ITER 1

CODE_FILE CODE_F809259241(CODE_F809259241_frame frame) {
 CODE_FILE dummy = ((CODE_FILE) NULL);
 CODE_FILE aI_u_I;
 CODE_FILE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((CODE_FILE_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((CODE_FILE_ARRAY)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

INT ARRAYd1897420416(ARRAYd1897420416_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdTP)frame->self); 
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

TUPIDENTINT ARRAYT1017446695(ARRAYT1017446695_frame frame) {
 TUPIDENTINT dummy = TUPIDENTINT_zero;
 TUPIDENTINT aI_u_I;
 TUPIDENTINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPIDENTINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPIDENTINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDENTdTP LAYOUT1415846778(LAYOUT1415846778_frame frame) {
 TUPIDENTdTP dummy = TUPIDENTdTP_zero;
 TUPIDENTdTP aI_u_I;
 TUPIDENTdTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((LAYOUT_ARR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((LAYOUT_ARR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG1754078736 ARRAYT1079066005(ARRAYT1079066005_frame frame) {
 TUPSIG1754078736 dummy = TUPSIG1754078736_zero;
 TUPSIG1754078736 aI_u_I;
 TUPSIG1754078736 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT348931282)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT348931282)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG2023533247 ARRAYT532396130(ARRAYT532396130_frame frame) {
 TUPSIG2023533247 dummy = TUPSIG2023533247_zero;
 TUPSIG2023533247 aI_u_I;
 TUPSIG2023533247 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1754348798)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1754348798)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG578692189 ARRAYT1269605207(ARRAYT1269605207_frame frame) {
 TUPSIG578692189 dummy = TUPSIG578692189_zero;
 TUPSIG578692189 aI_u_I;
 TUPSIG578692189 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT537394765)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT537394765)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGARG_LAYOUT ARRAYT457901414(ARRAYT457901414_frame frame) {
 TUPSIGARG_LAYOUT dummy = TUPSIGARG_LAYOUT_zero;
 TUPSIGARG_LAYOUT aI_u_I;
 TUPSIGARG_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1759168384)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1759168384)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGINT ARRAYT1593820953(ARRAYT1593820953_frame frame) {
 TUPSIGINT dummy = TUPSIGINT_zero;
 TUPSIGINT aI_u_I;
 TUPSIGINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSIGINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSIGINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGSIG ARRAYT1146052097(ARRAYT1146052097_frame frame) {
 TUPSIGSIG dummy = TUPSIGSIG_zero;
 TUPSIGSIG aI_u_I;
 TUPSIGSIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSIGSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSIGSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGSTR ARRAYT1691893201(ARRAYT1691893201_frame frame) {
 TUPSIGSTR dummy = TUPSIGSTR_zero;
 TUPSIGSTR aI_u_I;
 TUPSIGSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSIGSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSIGSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRFSETSTR ARRAYT1426073269(ARRAYT1426073269_frame frame) {
 TUPSTRFSETSTR dummy = TUPSTRFSETSTR_zero;
 TUPSTRFSETSTR aI_u_I;
 TUPSTRFSETSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT378612147)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT378612147)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRINT ARRAYT306471177(ARRAYT306471177_frame frame) {
 TUPSTRINT dummy = TUPSTRINT_zero;
 TUPSTRINT aI_u_I;
 TUPSTRINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSTRINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSTRINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRSIG ARRAYT141297679(ARRAYT141297679_frame frame) {
 TUPSTRSIG dummy = TUPSTRSIG_zero;
 TUPSTRSIG aI_u_I;
 TUPSTRSIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSTRSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSTRSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRSTR ARRAYT404543425(ARRAYT404543425_frame frame) {
 TUPSTRSTR dummy = TUPSTRSTR_zero;
 TUPSTRSTR aI_u_I;
 TUPSTRSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSTRSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPSTRSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRdCONFIG ARRAYT485937919(ARRAYT485937919_frame frame) {
 TUPSTRdCONFIG dummy = TUPSTRdCONFIG_zero;
 TUPSTRdCONFIG aI_u_I;
 TUPSTRdCONFIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT61112132)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT61112132)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPTP_1668454172 ARRAYT1509169201(ARRAYT1509169201_frame frame) {
 TUPTP_1668454172 dummy = TUPTP_1668454172_zero;
 TUPTP_1668454172 aI_u_I;
 TUPTP_1668454172 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1641775270)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1641775270)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPTP_858321267 ARRAYT518925702(ARRAYT518925702_frame frame) {
 TUPTP_858321267 dummy = TUPTP_858321267_zero;
 TUPTP_858321267 aI_u_I;
 TUPTP_858321267 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1127505716)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1127505716)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdOBNAMESPACE ARRAYT681600609(ARRAYT681600609_frame frame) {
 TUPdOBNAMESPACE dummy = TUPdOBNAMESPACE_zero;
 TUPdOBNAMESPACE aI_u_I;
 TUPdOBNAMESPACE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT747572380)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT747572380)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdOBSTR ARRAYT595865513(ARRAYT595865513_frame frame) {
 TUPdOBSTR dummy = TUPdOBSTR_zero;
 TUPdOBSTR aI_u_I;
 TUPdOBSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPdOBSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPdOBSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTP1710233051 ARRAYT1505580189(ARRAYT1505580189_frame frame) {
 TUPdTP1710233051 dummy = TUPdTP1710233051_zero;
 TUPdTP1710233051 aI_u_I;
 TUPdTP1710233051 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1562141251)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1562141251)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPCODE_FILE ARRAYT1283478039(ARRAYT1283478039_frame frame) {
 TUPdTPCODE_FILE dummy = TUPdTPCODE_FILE_zero;
 TUPdTPCODE_FILE aI_u_I;
 TUPdTPCODE_FILE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1153101058)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1153101058)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPINT ARRAYT452391291(ARRAYT452391291_frame frame) {
 TUPdTPINT dummy = TUPdTPINT_zero;
 TUPdTPINT aI_u_I;
 TUPdTPINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPdTPINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYTUPdTPINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPdLAYOUT ARRAYT2024950707(ARRAYT2024950707_frame frame) {
 TUPdTPdLAYOUT dummy = TUPdTPdLAYOUT_zero;
 TUPdTPdLAYOUT aI_u_I;
 TUPdTPdLAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYT1418209173)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYT1418209173)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dAM_CONST ARRAYd198808957(ARRAYd198808957_frame frame) {
 dAM_CONST dummy = ((dAM_CONST) NULL);
 dAM_CONST aI_u_I;
 dAM_CONST L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdAM_CONST)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYdAM_CONST)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame frame) {
 dAM_EXPR dummy = ((dAM_EXPR) NULL);
 dAM_EXPR aI_u_I;
 dAM_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdAM_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYdAM_EXPR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dAM_STMT ARRAYd615421366(ARRAYd615421366_frame frame) {
 dAM_STMT dummy = ((dAM_STMT) NULL);
 dAM_STMT aI_u_I;
 dAM_STMT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdAM_STMT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYdAM_STMT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dLAYOUT ARRAYd1647530701(ARRAYd1647530701_frame frame) {
 dLAYOUT dummy = ((dLAYOUT) NULL);
 dLAYOUT aI_u_I;
 dLAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYdLAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dLAYOUT LAYOUT1657363748(LAYOUT1657363748_frame frame) {
 dLAYOUT dummy = ((dLAYOUT) NULL);
 dLAYOUT aI_u_I;
 dLAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((LAYOUT_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((LAYOUT_ARRAY)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dTP ARRAYd2147017943(ARRAYd2147017943_frame frame) {
 dTP dummy = ((dTP) NULL);
 dTP aI_u_I;
 dTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAYdTP)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dTP ARRAYd244347909(ARRAYd244347909_frame frame) {
 dTP dummy = ((dTP) NULL);
 dTP aI_u_I;
 dTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ic=frame->arg1-1;frame->I_u_Im=frame->arg1+frame->arg2; 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(++frame->I_u_Ic>=frame->I_u_Im)  goto after_loop; aI_u_I=ARR((ARRAYdTP)frame->self,frame->I_u_Ic); frame->i_I_u_I++;
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

dTP TP_ARR212643368(TP_ARR212643368_frame frame) {
 dTP dummy = ((dTP) NULL);
 dTP aI_u_I;
 dTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((TP_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((TP_ARRAY)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void ARRAYT1024744539(ARRAYT1024744539_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYTUPSIGINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYTUPSIGINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void ARRAYd1329429954(ARRAYd1329429954_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdAM_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYdAM_EXPR)frame->self,frame->i_I_u_I,(dAM_EXPR)frame->arg1); frame->i_I_u_I++;
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

void ARRAYd1730019671(ARRAYd1730019671_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((ARRAYdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ARRAYdTP)frame->self,frame->i_I_u_I,(dTP)frame->arg1); frame->i_I_u_I++;
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

void CODE_F1378335655(CODE_F1378335655_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((CODE_FILE_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((CODE_FILE_ARRAY)frame->self,frame->i_I_u_I,(CODE_FILE)frame->arg1); frame->i_I_u_I++;
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

void LAYOUT2103796202(LAYOUT2103796202_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((LAYOUT_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((LAYOUT_ARRAY)frame->self,frame->i_I_u_I,(dLAYOUT)frame->arg1); frame->i_I_u_I++;
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

void LAYOUT2137691532(LAYOUT2137691532_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((LAYOUT_ARR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((LAYOUT_ARR)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void TP_ARR205286314(TP_ARR205286314_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=frame->self==NULL?0:ASIZE((TP_ARRAY)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((TP_ARRAY)frame->self,frame->i_I_u_I,(dTP)frame->arg1); frame->i_I_u_I++;
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
