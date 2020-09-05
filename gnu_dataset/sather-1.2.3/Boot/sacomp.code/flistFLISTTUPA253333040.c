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

typedef struct dELTdAM_CONST_struct {
 OB_HEADER header;
 } *dELTdAM_CONST;

typedef struct dELTdAM_EXPR_struct {
 OB_HEADER header;
 } *dELTdAM_EXPR;

typedef struct dELTdAM_STMT_struct {
 OB_HEADER header;
 } *dELTdAM_STMT;

typedef struct dELTdLAYOUT_struct {
 OB_HEADER header;
 } *dELTdLAYOUT;

typedef struct dELTdTP_struct {
 OB_HEADER header;
 } *dELTdTP;

typedef struct dELTTU1079126879_struct {
 OB_HEADER header;
 } *dELTTU1079126879;

typedef struct dELTTUPdOBSTR_struct {
 OB_HEADER header;
 } *dELTTUPdOBSTR;

typedef struct dELTTU1827781436_struct {
 OB_HEADER header;
 } *dELTTU1827781436;

typedef struct dELTTU1315166979_struct {
 OB_HEADER header;
 } *dELTTU1315166979;

typedef struct dELTTU32753540_struct {
 OB_HEADER header;
 } *dELTTU32753540;

typedef struct dELTTUPdTPINT_struct {
 OB_HEADER header;
 } *dELTTUPdTPINT;

typedef struct dELTTU158438634_struct {
 OB_HEADER header;
 } *dELTTU158438634;

typedef struct dELTTU1982974052_struct {
 OB_HEADER header;
 } *dELTTU1982974052;

typedef struct dELTTU1754236182_struct {
 OB_HEADER header;
 } *dELTTU1754236182;

typedef struct dELTTU1450772879_struct {
 OB_HEADER header;
 } *dELTTU1450772879;

typedef struct dELTTUPIDENTdTP_struct {
 OB_HEADER header;
 } *dELTTUPIDENTdTP;

typedef struct dELTTU770070093_struct {
 OB_HEADER header;
 } *dELTTU770070093;

typedef struct dELTTUPIDENTINT_struct {
 OB_HEADER header;
 } *dELTTUPIDENTINT;

typedef struct dELTTU405023937_struct {
 OB_HEADER header;
 } *dELTTU405023937;

typedef struct dELTTU1449135940_struct {
 OB_HEADER header;
 } *dELTTU1449135940;

typedef struct dELTTU1411620812_struct {
 OB_HEADER header;
 } *dELTTU1411620812;

typedef struct dELTTU1629996210_struct {
 OB_HEADER header;
 } *dELTTU1629996210;

typedef struct dELTTUPSIGINT_struct {
 OB_HEADER header;
 } *dELTTUPSIGINT;

typedef struct dELTTUPSIGSIG_struct {
 OB_HEADER header;
 } *dELTTUPSIGSIG;

typedef struct dELTTUPSIGSTR_struct {
 OB_HEADER header;
 } *dELTTUPSIGSTR;

typedef struct dELTTU470684395_struct {
 OB_HEADER header;
 } *dELTTU470684395;

typedef struct dELTTU788184410_struct {
 OB_HEADER header;
 } *dELTTU788184410;

typedef struct dELTTUPSTRINT_struct {
 OB_HEADER header;
 } *dELTTUPSTRINT;

typedef struct dELTTUPSTRSIG_struct {
 OB_HEADER header;
 } *dELTTUPSTRSIG;

typedef struct dELTTUPSTRSTR_struct {
 OB_HEADER header;
 } *dELTTUPSTRSTR;

typedef struct dELTTU216883428_struct {
 OB_HEADER header;
 } *dELTTU216883428;

typedef struct dELTTU186609637_struct {
 OB_HEADER header;
 } *dELTTU186609637;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

typedef struct ELT_ALGdAM_CONST_struct {/* layout for ELT_ALG{$AM_CONST} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdAM_CONST;

typedef struct ELT_ALGdAM_EXPR_struct {/* layout for ELT_ALG{$AM_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdAM_EXPR;

typedef struct ELT_ALGdAM_STMT_struct {/* layout for ELT_ALG{$AM_STMT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdAM_STMT;

typedef struct ELT_ALGdLAYOUT_struct {/* layout for ELT_ALG{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdLAYOUT;

typedef struct ELT_ALGdTP_struct {/* layout for ELT_ALG{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdTP;

typedef struct ELT_AL389001304_struct {/* layout for ELT_ALG{TUP{$OB,NAMESPACE}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL389001304;

typedef struct ELT_ALGTUPdOBSTR_struct {/* layout for ELT_ALG{TUP{$OB,STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPdOBSTR;

typedef struct ELT_AL1361870497_struct {/* layout for ELT_ALG{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1361870497;

typedef struct ELT_AL2005292554_struct {/* layout for ELT_ALG{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2005292554;

typedef struct ELT_AL1089356553_struct {/* layout for ELT_ALG{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1089356553;

typedef struct ELT_ALGTUPdTPINT_struct {/* layout for ELT_ALG{TUP{$TP,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPdTPINT;

typedef struct ELT_AL898164379_struct {/* layout for ELT_ALG{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL898164379;

typedef struct ELT_AL583594762_struct {/* layout for ELT_ALG{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL583594762;

typedef struct ELT_AL1371565672_struct {/* layout for ELT_ALG{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1371565672;

typedef struct ELT_AL616374741_struct {/* layout for ELT_ALG{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL616374741;

typedef struct ELT_AL655625458_struct {/* layout for ELT_ALG{TUP{IDENT,$TP}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL655625458;

typedef struct ELT_AL383472681_struct {/* layout for ELT_ALG{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL383472681;

typedef struct ELT_AL655438175_struct {/* layout for ELT_ALG{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL655438175;

typedef struct ELT_AL110515410_struct {/* layout for ELT_ALG{TUP{SIG,ABSTRACT_FRAME_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL110515410;

typedef struct ELT_AL1476366487_struct {/* layout for ELT_ALG{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1476366487;

typedef struct ELT_AL1639104906_struct {/* layout for ELT_ALG{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1639104906;

typedef struct ELT_AL2147376660_struct {/* layout for ELT_ALG{TUP{SIG,FRAME_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2147376660;

typedef struct ELT_ALGTUPSIGINT_struct {/* layout for ELT_ALG{TUP{SIG,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSIGINT;

typedef struct ELT_ALGTUPSIGSIG_struct {/* layout for ELT_ALG{TUP{SIG,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSIGSIG;

typedef struct ELT_ALGTUPSIGSTR_struct {/* layout for ELT_ALG{TUP{SIG,STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSIGSTR;

typedef struct ELT_AL4773456_struct {/* layout for ELT_ALG{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL4773456;

typedef struct ELT_AL322273471_struct {/* layout for ELT_ALG{TUP{STR,FSET{STR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL322273471;

typedef struct ELT_ALGTUPSTRINT_struct {/* layout for ELT_ALG{TUP{STR,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSTRINT;

typedef struct ELT_ALGTUPSTRSIG_struct {/* layout for ELT_ALG{TUP{STR,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSTRSIG;

typedef struct ELT_ALGTUPSTRSTR_struct {/* layout for ELT_ALG{TUP{STR,STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPSTRSTR;

typedef struct ELT_AL734263878_struct {/* layout for ELT_ALG{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL734263878;

typedef struct ELT_AL629085806_struct {/* layout for ELT_ALG{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL629085806;

typedef struct ELT_EQdAM_EXPR_struct {/* layout for ELT_EQ{$AM_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAM_EXPR;

typedef struct ELT_EQ947319992_struct {/* layout for ELT_EQ{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ947319992;

typedef struct ELT_EQ1704040611_struct {/* layout for ELT_EQ{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1704040611;

typedef struct ELT_EQ1475848140_struct {/* layout for ELT_EQ{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1475848140;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NI1564018771_struct {/* layout for ELT_NIL{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1564018771;

typedef struct ELT_NI19483997_struct {/* layout for ELT_NIL{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI19483997;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

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

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct FLISTdAM_CONST_struct {/* layout for FLIST{$AM_CONST} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *FLISTdAM_CONST;

typedef struct FLISTd68199614_frame_struct {
 INT state;
 FLISTdAM_CONST self;/* Formal argument: self */
 dAM_CONST arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTd68199614_frame;

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

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTd597134281_frame_struct {
 INT state;
 FLISTdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTd597134281_frame;

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

typedef struct FLISTd311576818_frame_struct {
 INT state;
 FLISTdAM_EXPR self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTd311576818_frame;

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTd1458554251_frame_struct {
 INT state;
 FLISTdAM_STMT self;/* Formal argument: self */
 dAM_STMT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTd1458554251_frame;

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

typedef struct FLISTd739102917_frame_struct {
 INT state;
 FLISTdAM_STMT self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTd739102917_frame;

typedef struct FLISTdLAYOUT_struct {/* layout for FLIST{$LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FLISTdLAYOUT;

typedef struct FLISTd1190179840_frame_struct {
 INT state;
 FLISTdLAYOUT self;/* Formal argument: self */
 dLAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTd1190179840_frame;

typedef struct FLISTd1954760550_frame_struct {
 INT state;
 FLISTdLAYOUT self;/* Formal argument: self */
 dLAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTdLAYOUT aget_self;
 INT aget_ind;
 dLAYOUT ret_val;
 } *FLISTd1954760550_frame;

typedef struct FLISTdTP_struct {/* layout for FLIST{$TP} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FLISTdTP;

typedef struct FLISTd344567952_frame_struct {
 INT state;
 FLISTdTP self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTd344567952_frame;

typedef struct FLISTd1740602786_frame_struct {
 INT state;
 FLISTdTP self;/* Formal argument: self */
 dTP ret_val1;
 INT i;
 INT sz;
 FLISTdTP aget_self;
 INT aget_ind;
 dTP ret_val;
 } *FLISTd1740602786_frame;

typedef struct FLISTd1990200313_frame_struct {
 INT state;
 FLISTdTP self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FLISTd1990200313_frame;

typedef struct FLISTT1513788273_struct {/* layout for FLIST{TUP{$OB,NAMESPACE}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FLISTT1513788273;

typedef struct FLISTT396553018_frame_struct {
 INT state;
 FLISTT1513788273 self;/* Formal argument: self */
 TUPdOBNAMESPACE ret_val1;
 INT i;
 INT sz;
 FLISTT1513788273 aget_self;
 INT aget_ind;
 TUPdOBNAMESPACE ret_val;
 } *FLISTT396553018_frame;

typedef struct FLISTTUPdOBSTR_struct {/* layout for FLIST{TUP{$OB,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *FLISTTUPdOBSTR;

typedef struct FLISTT220152704_frame_struct {
 INT state;
 FLISTTUPdOBSTR self;/* Formal argument: self */
 TUPdOBSTR ret_val1;
 INT i;
 INT sz;
 FLISTTUPdOBSTR aget_self;
 INT aget_ind;
 TUPdOBSTR ret_val;
 } *FLISTT220152704_frame;

typedef struct FLISTT567859354_struct {/* layout for FLIST{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FLISTT567859354;

typedef struct FLISTT123078916_frame_struct {
 INT state;
 FLISTT567859354 self;/* Formal argument: self */
 TUPdTPdLAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTT567859354 aget_self;
 INT aget_ind;
 TUPdTPdLAYOUT ret_val;
 } *FLISTT123078916_frame;

typedef struct FLISTT880505585_struct {/* layout for FLIST{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *FLISTT880505585;

typedef struct FLISTT1568525630_frame_struct {
 INT state;
 FLISTT880505585 self;/* Formal argument: self */
 TUPdTPCODE_FILE ret_val1;
 INT i;
 INT sz;
 FLISTT880505585 aget_self;
 INT aget_ind;
 TUPdTPCODE_FILE ret_val;
 } *FLISTT1568525630_frame;

typedef struct FLISTT1858637310_struct {/* layout for FLIST{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FLISTT1858637310;

typedef struct FLISTT1760042796_frame_struct {
 INT state;
 FLISTT1858637310 self;/* Formal argument: self */
 TUPdTP1710233051 ret_val1;
 INT i;
 INT sz;
 FLISTT1858637310 aget_self;
 INT aget_ind;
 TUPdTP1710233051 ret_val;
 } *FLISTT1760042796_frame;

typedef struct FLISTTUPdTPINT_struct {/* layout for FLIST{TUP{$TP,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FLISTTUPdTPINT;

typedef struct FLISTT1268409508_frame_struct {
 INT state;
 FLISTTUPdTPINT self;/* Formal argument: self */
 TUPdTPINT ret_val1;
 INT i;
 INT sz;
 FLISTTUPdTPINT aget_self;
 INT aget_ind;
 TUPdTPINT ret_val;
 } *FLISTT1268409508_frame;

typedef struct FLISTT2049829484_struct {/* layout for FLIST{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FLISTT2049829484;

typedef struct FLISTT344492076_frame_struct {
 INT state;
 FLISTT2049829484 self;/* Formal argument: self */
 TUPAM_1519040877 ret_val1;
 INT i;
 INT sz;
 FLISTT2049829484 aget_self;
 INT aget_ind;
 TUPAM_1519040877 ret_val;
 } *FLISTT344492076_frame;

typedef struct FLISTT406286251_struct {/* layout for FLIST{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *FLISTT406286251;

typedef struct FLISTT1883920969_frame_struct {
 INT state;
 FLISTT406286251 self;/* Formal argument: self */
 TUPAM_504653531 ret_val1;
 INT i;
 INT sz;
 FLISTT406286251 aget_self;
 INT aget_ind;
 TUPAM_504653531 ret_val;
 } *FLISTT1883920969_frame;

typedef struct FLISTT31683607_struct {/* layout for FLIST{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *FLISTT31683607;

typedef struct FLISTT234903853_frame_struct {
 INT state;
 FLISTT31683607 self;/* Formal argument: self */
 TUPAM_1024994801 ret_val1;
 INT i;
 INT sz;
 FLISTT31683607 aget_self;
 INT aget_ind;
 TUPAM_1024994801 ret_val;
 } *FLISTT234903853_frame;

typedef struct FLISTT1683165204_struct {/* layout for FLIST{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *FLISTT1683165204;

typedef struct FLISTT288887925_frame_struct {
 INT state;
 FLISTT1683165204 self;/* Formal argument: self */
 TUPAM_653469574 ret_val1;
 INT i;
 INT sz;
 FLISTT1683165204 aget_self;
 INT aget_ind;
 TUPAM_653469574 ret_val;
 } *FLISTT288887925_frame;

typedef struct FLISTTUPIDENTdTP_struct {/* layout for FLIST{TUP{IDENT,$TP}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FLISTTUPIDENTdTP;

typedef struct FLISTT1769198926_frame_struct {
 INT state;
 FLISTTUPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val1;
 INT i;
 INT sz;
 FLISTTUPIDENTdTP aget_self;
 INT aget_ind;
 TUPIDENTdTP ret_val;
 } *FLISTT1769198926_frame;

typedef struct FLISTT1023127882_struct {/* layout for FLIST{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FLISTT1023127882;

typedef struct FLISTT1472123735_frame_struct {
 INT state;
 FLISTT1023127882 self;/* Formal argument: self */
 TUPIDE641654124 ret_val1;
 INT i;
 INT sz;
 FLISTT1023127882 aget_self;
 INT aget_ind;
 TUPIDE641654124 ret_val;
 } *FLISTT1472123735_frame;

typedef struct FLISTTUPIDENTINT_struct {/* layout for FLIST{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FLISTTUPIDENTINT;

typedef struct FLISTT1251913564_frame_struct {
 INT state;
 FLISTTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTT1251913564_frame;

typedef struct FLISTT97637998_frame_struct {
 INT state;
 FLISTTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT ret_val1;
 INT i;
 INT sz;
 FLISTTUPIDENTINT aget_self;
 INT aget_ind;
 TUPIDENTINT ret_val;
 } *FLISTT97637998_frame;

typedef struct FLISTT1075494997_struct {/* layout for FLIST{TUP{SIG,ABSTRACT_FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FLISTT1075494997;

typedef struct FLISTT287332146_frame_struct {
 INT state;
 FLISTT1075494997 self;/* Formal argument: self */
 TUPSIG1754078736 ret_val1;
 INT i;
 INT sz;
 FLISTT1075494997 aget_self;
 INT aget_ind;
 TUPSIG1754078736 ret_val;
 } *FLISTT287332146_frame;

typedef struct FLISTT844804258_struct {/* layout for FLIST{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FLISTT844804258;

typedef struct FLISTT754641600_frame_struct {
 INT state;
 FLISTT844804258 self;/* Formal argument: self */
 TUPSIG578692189 ret_val1;
 INT i;
 INT sz;
 FLISTT844804258 aget_self;
 INT aget_ind;
 TUPSIG578692189 ret_val;
 } *FLISTT754641600_frame;

typedef struct FLISTT1742988937_struct {/* layout for FLIST{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FLISTT1742988937;

typedef struct FLISTT634936167_frame_struct {
 INT state;
 FLISTT1742988937 self;/* Formal argument: self */
 TUPSIGARG_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTT1742988937 aget_self;
 INT aget_ind;
 TUPSIGARG_LAYOUT ret_val;
 } *FLISTT634936167_frame;

typedef struct FLISTT994805427_struct {/* layout for FLIST{TUP{SIG,FRAME_LAYOUT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FLISTT994805427;

typedef struct FLISTT685872269_frame_struct {
 INT state;
 FLISTT994805427 self;/* Formal argument: self */
 TUPSIG2023533247 ret_val1;
 INT i;
 INT sz;
 FLISTT994805427 aget_self;
 INT aget_ind;
 TUPSIG2023533247 ret_val;
 } *FLISTT685872269_frame;

typedef struct FLISTTUPSIGINT_struct {/* layout for FLIST{TUP{SIG,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGINT_struct arr_part[1];
 } *FLISTTUPSIGINT;

typedef struct FLISTT777802736_frame_struct {
 INT state;
 FLISTTUPSIGINT self;/* Formal argument: self */
 TUPSIGINT ret_val1;
 INT i;
 INT sz;
 FLISTTUPSIGINT aget_self;
 INT aget_ind;
 TUPSIGINT ret_val;
 } *FLISTT777802736_frame;

typedef struct FLISTTUPSIGSIG_struct {/* layout for FLIST{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FLISTTUPSIGSIG;

typedef struct FLISTT330033880_frame_struct {
 INT state;
 FLISTTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG ret_val1;
 INT i;
 INT sz;
 FLISTTUPSIGSIG aget_self;
 INT aget_ind;
 TUPSIGSIG ret_val;
 } *FLISTT330033880_frame;

typedef struct FLISTTUPSIGSTR_struct {/* layout for FLIST{TUP{SIG,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *FLISTTUPSIGSTR;

typedef struct FLISTT875874984_frame_struct {
 INT state;
 FLISTTUPSIGSTR self;/* Formal argument: self */
 TUPSIGSTR ret_val1;
 INT i;
 INT sz;
 FLISTTUPSIGSTR aget_self;
 INT aget_ind;
 TUPSIGSTR ret_val;
 } *FLISTT875874984_frame;

typedef struct FLISTT789237687_struct {/* layout for FLIST{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *FLISTT789237687;

typedef struct FLISTT1662091704_frame_struct {
 INT state;
 FLISTT789237687 self;/* Formal argument: self */
 TUPSTRdCONFIG ret_val1;
 INT i;
 INT sz;
 FLISTT789237687 aget_self;
 INT aget_ind;
 TUPSTRdCONFIG ret_val;
 } *FLISTT1662091704_frame;

typedef struct FLISTT471737672_struct {/* layout for FLIST{TUP{STR,FSET{STR}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FLISTT471737672;

typedef struct FLISTT720864404_frame_struct {
 INT state;
 FLISTT471737672 self;/* Formal argument: self */
 TUPSTRFSETSTR ret_val1;
 INT i;
 INT sz;
 FLISTT471737672 aget_self;
 INT aget_ind;
 TUPSTRFSETSTR ret_val;
 } *FLISTT720864404_frame;

typedef struct FLISTTUPSTRINT_struct {/* layout for FLIST{TUP{STR,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FLISTTUPSTRINT;

typedef struct FLISTT509547040_frame_struct {
 INT state;
 FLISTTUPSTRINT self;/* Formal argument: self */
 TUPSTRINT ret_val1;
 INT i;
 INT sz;
 FLISTTUPSTRINT aget_self;
 INT aget_ind;
 TUPSTRINT ret_val;
 } *FLISTT509547040_frame;

typedef struct FLISTTUPSTRSIG_struct {/* layout for FLIST{TUP{STR,SIG}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FLISTTUPSTRSIG;

typedef struct FLISTT957315896_frame_struct {
 INT state;
 FLISTTUPSTRSIG self;/* Formal argument: self */
 TUPSTRSIG ret_val1;
 INT i;
 INT sz;
 FLISTTUPSTRSIG aget_self;
 INT aget_ind;
 TUPSTRSIG ret_val;
 } *FLISTT957315896_frame;

typedef struct FLISTTUPSTRSTR_struct {/* layout for FLIST{TUP{STR,STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FLISTTUPSTRSTR;

typedef struct FLISTT411474792_frame_struct {
 INT state;
 FLISTTUPSTRSTR self;/* Formal argument: self */
 TUPSTRSTR ret_val1;
 INT i;
 INT sz;
 FLISTTUPSTRSTR aget_self;
 INT aget_ind;
 TUPSTRSTR ret_val;
 } *FLISTT411474792_frame;

typedef struct FLISTT418307355_struct {/* layout for FLIST{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FLISTT418307355;

typedef struct FLISTT699342697_frame_struct {
 INT state;
 FLISTT418307355 self;/* Formal argument: self */
 TUPTP_858321267 ret_val1;
 INT i;
 INT sz;
 FLISTT418307355 aget_self;
 INT aget_ind;
 TUPTP_858321267 ret_val;
 } *FLISTT699342697_frame;

typedef struct FLISTT102367225_struct {/* layout for FLIST{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *FLISTT102367225;

typedef struct FLISTT1725186742_frame_struct {
 INT state;
 FLISTT102367225 self;/* Formal argument: self */
 TUPTP_1668454172 ret_val1;
 INT i;
 INT sz;
 FLISTT102367225 aget_self;
 INT aget_ind;
 TUPTP_1668454172 ret_val;
 } *FLISTT1725186742_frame;

typedef struct FMAPAM339652544_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FMAPAM339652544;

typedef struct FMAPAM1112691238_frame_struct {
 INT state;
 FMAPAM339652544 self;/* Formal argument: self */
 TUPAM_304458649 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1112691238_frame;

typedef struct FMAPAM2127544368_frame_struct {
 INT state;
 FMAPAM339652544 self;/* Formal argument: self */
 TUPAM_304458649 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM2127544368_frame;

typedef struct FMAPAM709445685_frame_struct {
 INT state;
 FMAPAM339652544 self;/* Formal argument: self */
 TUPAM_304458649 ret_val2;
 FMAPAM339652544 L61;
 TUPAM_304458649 r;
 INT L31;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM709445685_frame;

typedef struct FMAPAM1324815836_frame_struct {
 INT state;
 FMAPAM339652544 self;/* Formal argument: self */
 BOUND_1007407331 ret_val2;
 FMAPAM339652544 L61;
 TUPAM_304458649 e;
 INT L31;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1324815836_frame;

typedef struct FMAPAM1357596931_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FMAPAM1357596931;

typedef struct FMAPAM1638362026_frame_struct {
 INT state;
 FMAPAM1357596931 self;/* Formal argument: self */
 TUPAM_667892060 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1638362026_frame;

typedef struct FMAPAM1361186976_frame_struct {
 INT state;
 FMAPAM1357596931 self;/* Formal argument: self */
 TUPAM_667892060 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1361186976_frame;

typedef struct FMAPAM1474331777_frame_struct {
 INT state;
 FMAPAM1357596931 self;/* Formal argument: self */
 TUPAM_667892060 ret_val2;
 FMAPAM1357596931 L61;
 TUPAM_667892060 r;
 INT L31;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1474331777_frame;

typedef struct FMAPAM1626870716_frame_struct {
 INT state;
 FMAPAM1357596931 self;/* Formal argument: self */
 BOUND_1677815802 ret_val2;
 FMAPAM1357596931 L61;
 TUPAM_667892060 e;
 INT L31;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1626870716_frame;

typedef struct FMAPAM1145008811_struct {/* layout for FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1519040877_struct arr_part[1];
 } *FMAPAM1145008811;

typedef struct FMAPAM725708071_frame_struct {
 INT state;
 FMAPAM1145008811 self;/* Formal argument: self */
 TUPAM_1519040877 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM725708071_frame;

typedef struct FMAPAM1521097993_frame_struct {
 INT state;
 FMAPAM1145008811 self;/* Formal argument: self */
 TUPAM_1519040877 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1521097993_frame;

typedef struct FMAPAM1804153404_frame_struct {
 INT state;
 FMAPAM1145008811 self;/* Formal argument: self */
 TUPAM_1519040877 ret_val2;
 FMAPAM1145008811 L61;
 TUPAM_1519040877 r;
 INT L31;
 FMAPAM1145008811 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1804153404_frame;

typedef struct FMAPAM418009930_struct {/* layout for FMAP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *FMAPAM418009930;

typedef struct FMAPAM1263439864_frame_struct {
 INT state;
 FMAPAM418009930 self;/* Formal argument: self */
 TUPAM_504653531 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1263439864_frame;

typedef struct FMAPAM804020802_frame_struct {
 INT state;
 FMAPAM418009930 self;/* Formal argument: self */
 TUPAM_504653531 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM804020802_frame;

typedef struct FMAPAM1518099927_frame_struct {
 INT state;
 FMAPAM418009930 self;/* Formal argument: self */
 TUPAM_504653531 ret_val2;
 FMAPAM418009930 L61;
 TUPAM_504653531 r;
 INT L31;
 FMAPAM418009930 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1518099927_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

#include "tags.h"

/* Globals */

extern INT FMAPAM29230850;
extern INT FMAPAM441967257;
extern INT FMAPAM476780631;
extern INT FMAPAM498940295;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);
AM_LOCAL_EXPR FMAPAM1614132657(FMAPAM1145008811, AM_LOCAL_EXPR);
BOOL FLISTT1340999558(FLISTTUPIDENTINT, TUPIDENTINT);
BOOL FLTD_i1285721587(FLTD, OB);
BOOL FLTD_is_nilrBOOL(FLTD);
BOOL FLT_is1351417193(FLT, OB);
BOOL FLT_is_nilrBOOL(FLT);
BOOL TUPIDE1439069590(TUPIDENTINT, TUPIDENTINT);
BOUND_1007407331 FMAPAM1324815836(FMAPAM1324815836_frame);
BOUND_1007407331 FMAPAM1635877202(FMAPAM339652544, AM_BND1124877163);
BOUND_1677815802 FMAPAM1626870716(FMAPAM1626870716_frame);
BOUND_1677815802 FMAPAM661174734(FMAPAM1357596931, AM_BND1124877163);
FLISTTUPIDENTINT FLISTT1610849855(FLISTTUPIDENTINT, TUPIDENTINT);
FLISTdAM_CONST FLISTd246699990(FLISTdAM_CONST, dAM_CONST);
FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR, FLISTdAM_EXPR);
FLISTdAM_EXPR FLISTd1492920040(FLISTdAM_EXPR, INT);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT, dAM_STMT);
FLISTdAM_STMT FLISTd846796316(FLISTdAM_STMT, INT);
FLISTdLAYOUT FLISTd1429133189(FLISTdLAYOUT, dLAYOUT);
FLISTdTP FLISTd1953924847(FLISTdTP, dTP);
FLT FLT_nilrFLT(FLT);
FLTD FLTD_nilrFLTD(FLTD);
FMAPAM1145008811 FMAPAM1477123193(FMAPAM1145008811, INT);
FMAPAM1145008811 FMAPAM240162218(FMAPAM1145008811);
FMAPAM1145008811 FMAPAM714369621(FMAPAM1145008811, INT);
FMAPAM1145008811 FMAPAM876857188(FMAPAM1145008811, AM_LOCAL_EXPR, AM_LOCAL_EXPR);
FMAPAM1357596931 FMAPAM1106357779(FMAPAM1357596931, AM_BND1124877163, BOUND_1677815802);
FMAPAM1357596931 FMAPAM48293577(FMAPAM1357596931);
FMAPAM1357596931 FMAPAM765222498(FMAPAM1357596931, INT);
FMAPAM339652544 FMAPAM136290044(FMAPAM339652544, AM_BND1124877163, BOUND_1007407331);
FMAPAM339652544 FMAPAM2064639115(FMAPAM339652544);
FMAPAM339652544 FMAPAM597772238(FMAPAM339652544, INT);
FMAPAM418009930 FMAPAM1345096032(FMAPAM418009930, AM_LOCAL_EXPR, OPT_LOCAL_EXPR);
FMAPAM418009930 FMAPAM541564112(FMAPAM418009930, INT);
FMAPAM418009930 FMAPAM994893385(FMAPAM418009930);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1752847026(FSTR, STR);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FLISTT563337031(FLISTTUPIDENTINT);
INT FLISTd102354833(FLISTdLAYOUT);
INT FLISTd1086621942(FLISTdAM_EXPR, dAM_EXPR);
INT FLISTd1406591968(FLISTdAM_EXPR);
INT FLISTd1855368276(FLISTdAM_CONST);
INT FLISTd1990200313(FLISTd1990200313_frame);
INT FLISTd311576818(FLISTd311576818_frame);
INT FLISTd355912233(FLISTdAM_STMT);
INT FLISTd739102917(FLISTd739102917_frame);
INT FLISTd895185163(FLISTdTP);
INT FLTD_hashrINT(FLTD);
INT FLT_hashrINT(FLT);
INT FSTR_sizerINT(FSTR);
INT INT_hashrINT(INT);
INT INT_hi740923800(INT);
INT INT_up418511718(INT_up418511718_frame);
OPT_LOCAL_EXPR FMAPAM731742676(FMAPAM418009930, AM_LOCAL_EXPR);
STR ELT_AL1053727475(ELT_AL1639104906, dELTTU1411620812);
STR ELT_AL1083506231(ELT_AL583594762, dELTTU1982974052);
STR ELT_AL1121979891(ELT_ALGdAM_STMT, dELTdAM_STMT);
STR ELT_AL1178944168(ELT_AL1089356553, dELTTU32753540);
STR ELT_AL1237867596(ELT_ALGTUPdOBSTR, dELTTUPdOBSTR);
STR ELT_AL1304624690(ELT_AL4773456, dELTTU470684395);
STR ELT_AL1448140707(ELT_AL383472681, dELTTU770070093);
STR ELT_AL1458120312(ELT_ALGTUPSIGSTR, dELTTUPSIGSTR);
STR ELT_AL1675438629(ELT_AL2147376660, dELTTU1629996210);
STR ELT_AL1818663869(ELT_AL734263878, dELTTU216883428);
STR ELT_AL184221807(ELT_ALGdAM_EXPR, dELTdAM_EXPR);
STR ELT_AL1860341546(ELT_AL629085806, dELTTU186609637);
STR ELT_AL1873149708(ELT_ALGdTP, dELTdTP);
STR ELT_AL1921017260(ELT_AL322273471, dELTTU788184410);
STR ELT_AL1930239142(ELT_AL2005292554, dELTTU1315166979);
STR ELT_AL1950462334(ELT_AL110515410, dELTTU405023937);
STR ELT_AL1978058288(ELT_ALGTUPSIGSIG, dELTTUPSIGSIG);
STR ELT_AL2030803742(ELT_AL389001304, dELTTU1079126879);
STR ELT_AL2046630250(ELT_ALGTUPdTPINT, dELTTUPdTPINT);
STR ELT_AL2074944584(ELT_AL1476366487, dELTTU1449135940);
STR ELT_AL2104989740(ELT_AL898164379, dELTTU158438634);
STR ELT_AL218385468(ELT_ALGdLAYOUT, dELTdLAYOUT);
STR ELT_AL26398608(ELT_ALGTUPSTRSTR, dELTTUPSTRSTR);
STR ELT_AL429905904(ELT_AL655438175, dELTTUPIDENTINT);
STR ELT_AL47866227(ELT_AL616374741, dELTTU1450772879);
STR ELT_AL510829178(ELT_AL655625458, dELTTUPIDENTdTP);
STR ELT_AL546336584(ELT_ALGTUPSTRSIG, dELTTUPSTRSIG);
STR ELT_AL649416844(ELT_ALGTUPSIGINT, dELTTUPSIGINT);
STR ELT_AL772038856(ELT_AL1361870497, dELTTU1827781436);
STR ELT_AL782304860(ELT_ALGTUPSTRINT, dELTTUPSTRINT);
STR ELT_AL918382858(ELT_ALGdAM_CONST, dELTdAM_CONST);
STR ELT_AL966666787(ELT_AL1371565672, dELTTU1754236182);
STR FLISTT1090491199(FLISTT844804258);
STR FLISTT1312754552(FLISTT1075494997);
STR FLISTT1350994583(FLISTTUPSTRINT);
STR FLISTT1410881023(FLISTTUPdOBSTR);
STR FLISTT1451978730(FLISTT31683607);
STR FLISTT1532344775(FLISTT1683165204);
STR FLISTT158032289(FLISTT1858637310);
STR FLISTT1848908808(FLISTT406286251);
STR FLISTT1906991113(FLISTT1023127882);
STR FLISTT1916592887(FLISTT567859354);
STR FLISTT208196008(FLISTT418307355);
STR FLISTT275941467(FLISTTUPSIGSIG);
STR FLISTT306797234(FLISTT1513788273);
STR FLISTT307474581(FLISTTUPSTRSTR);
STR FLISTT32449926(FLISTT789237687);
STR FLISTT496865671(FLISTTUPSIGSTR);
STR FLISTT499676452(FLISTT102367225);
STR FLISTT528398785(FLISTTUPSTRSIG);
STR FLISTT544048196(FLISTTUPIDENTdTP);
STR FLISTT546654331(FLISTTUPSIGINT);
STR FLISTT549250782(FLISTTUPdTPINT);
STR FLISTT580276949(FLISTT471737672);
STR FLISTT582206950(FLISTT994805427);
STR FLISTT660722487(FLISTTUPIDENTINT);
STR FLISTT689628623(FLISTT2049829484);
STR FLISTT88767660(FLISTT880505585);
STR FLISTT919810588(FLISTT1742988937);
STR FLISTd1101907210(FLISTdAM_STMT);
STR FLISTd1314612993(FLISTdLAYOUT);
STR FLISTd1757835884(FLISTdAM_CONST);
STR FLISTd2007506601(FLISTdAM_EXPR);
STR FLISTdTP_strrSTR(FLISTdTP);
STR FLTD_strrSTR(FLTD);
STR FLT_strrSTR(FLT);
STR FMAPAM1084073011(FMAPAM339652544);
STR FMAPAM1201551978(FMAPAM1357596931);
STR FMAPAM1620534738(FMAPAM1145008811);
STR FMAPAM905477833(FMAPAM418009930);
STR STR_fr1097270334(STR, FSTR);
TUPAM_1024994801 FLISTT234903853(FLISTT234903853_frame);
TUPAM_1519040877 FLISTT344492076(FLISTT344492076_frame);
TUPAM_1519040877 FMAPAM1804153404(FMAPAM1804153404_frame);
TUPAM_1519040877 FMAPAM725708071(FMAPAM725708071_frame);
TUPAM_304458649 FMAPAM1112691238(FMAPAM1112691238_frame);
TUPAM_304458649 FMAPAM709445685(FMAPAM709445685_frame);
TUPAM_504653531 FLISTT1883920969(FLISTT1883920969_frame);
TUPAM_504653531 FMAPAM1263439864(FMAPAM1263439864_frame);
TUPAM_504653531 FMAPAM1518099927(FMAPAM1518099927_frame);
TUPAM_653469574 FLISTT288887925(FLISTT288887925_frame);
TUPAM_667892060 FMAPAM1474331777(FMAPAM1474331777_frame);
TUPAM_667892060 FMAPAM1638362026(FMAPAM1638362026_frame);
TUPIDE641654124 FLISTT1472123735(FLISTT1472123735_frame);
TUPIDENTINT FLISTT108744548(FLISTTUPIDENTINT);
TUPIDENTINT FLISTT97637998(FLISTT97637998_frame);
TUPIDENTdTP FLISTT1769198926(FLISTT1769198926_frame);
TUPSIG1754078736 FLISTT287332146(FLISTT287332146_frame);
TUPSIG2023533247 FLISTT685872269(FLISTT685872269_frame);
TUPSIG578692189 FLISTT754641600(FLISTT754641600_frame);
TUPSIGARG_LAYOUT FLISTT634936167(FLISTT634936167_frame);
TUPSIGINT FLISTT777802736(FLISTT777802736_frame);
TUPSIGSIG FLISTT330033880(FLISTT330033880_frame);
TUPSIGSTR FLISTT875874984(FLISTT875874984_frame);
TUPSTRFSETSTR FLISTT720864404(FLISTT720864404_frame);
TUPSTRINT FLISTT509547040(FLISTT509547040_frame);
TUPSTRSIG FLISTT957315896(FLISTT957315896_frame);
TUPSTRSTR FLISTT411474792(FLISTT411474792_frame);
TUPSTRdCONFIG FLISTT1662091704(FLISTT1662091704_frame);
TUPTP_1668454172 FLISTT1725186742(FLISTT1725186742_frame);
TUPTP_858321267 FLISTT699342697(FLISTT699342697_frame);
TUPdOBNAMESPACE FLISTT396553018(FLISTT396553018_frame);
TUPdOBSTR FLISTT220152704(FLISTT220152704_frame);
TUPdTP1710233051 FLISTT1760042796(FLISTT1760042796_frame);
TUPdTPCODE_FILE FLISTT1568525630(FLISTT1568525630_frame);
TUPdTPINT FLISTT1268409508(FLISTT1268409508_frame);
TUPdTPdLAYOUT FLISTT123078916(FLISTT123078916_frame);
dAM_CONST FLISTd617209260(FLISTd617209260_frame);
dAM_EXPR FLISTd2122660729(FLISTd2122660729_frame);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dLAYOUT FLISTd1954760550(FLISTd1954760550_frame);
dTP FLISTd1740602786(FLISTd1740602786_frame);
void FLISTT1251913564(FLISTT1251913564_frame);
void FLISTd1190179840(FLISTd1190179840_frame);
void FLISTd1252754628(FLISTdAM_EXPR, INT);
void FLISTd1458554251(FLISTd1458554251_frame);
void FLISTd344567952(FLISTd344567952_frame);
void FLISTd597134281(FLISTd597134281_frame);
void FLISTd68199614(FLISTd68199614_frame);
void FMAPAM1361186976(FMAPAM1361186976_frame);
void FMAPAM1521097993(FMAPAM1521097993_frame);
void FMAPAM2127544368(FMAPAM2127544368_frame);
void FMAPAM804020802(FMAPAM804020802_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <math.h>
_FM_F_I(signaling_nan)

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR FMAPAM1614132657(FMAPAM1145008811 self, AM_LOCAL_EXPR k) {
 AM_LOCAL_EXPR ret_val;
 INT h = INT_zero;
 AM_LOCAL_EXPR tk;
 AM_LOCAL_EXPR tk1;
 FMAPAM1145008811 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM1145008811 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM1145008811 key_eq_self;
 AM_LOCAL_EXPR key_eq_e1;
 AM_LOCAL_EXPR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FMAPAM1145008811 is_key_nil_self1;
 AM_LOCAL_EXPR is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPAM1145008811 key_eq_self1;
 AM_LOCAL_EXPR key_eq_e11;
 AM_LOCAL_EXPR key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_1519040877 L71_;
 TUPAM_1519040877 L8;
 TUPAM_1519040877 L91_;
 TUPAM_1519040877 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_1519040877 L171_;
 TUPAM_1519040877 L18;
 TUPAM_1519040877 L191_;
 TUPAM_1519040877 L20;
 INT L221_;
 if ((self==((FMAPAM1145008811) NULL))) {
  ret_val = ((AM_LOCAL_EXPR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM1145008811)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM1145008811)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ1704040611) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_a = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM1145008811)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM1145008811)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM1145008811)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI19483997) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ1704040611) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_a1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPAM1145008811)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((AM_LOCAL_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLISTT1340999558(FLISTTUPIDENTINT self, TUPIDENTINT e) {
 BOOL ret_val = BOOL_zero;
 FLISTTUPIDENTINT L11;
 FLISTTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 TUPIDENTINT L2;
 {
  struct FLISTT97637998_frame_struct other1_0;
FLISTT97637998_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   elt_eq_self = self;
   elt_eq_e1 = e;
   L2 = FLISTT97637998(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   elt_eq_e2 = L2;
   elt_eq_self1 = ((ELT_EQ1475848140) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val2 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
   ret_val1 = ret_val2;
   if (ret_val1) {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLTD_i1285721587(FLTD self, OB arg) {
 BOOL ret_val = BOOL_zero;
 FLTD L1;
 BOOL L21_;
 if (arg==NULL) {
  goto other165;
 } else
 switch (TAG(arg)) {
  case FLTD_tag:
   L1 = ((FLTD_boxed) arg)->immutable_part;
   L21_=(self)==(L1); 
   ret_val = L21_;
   return ret_val; break;
  default: ;
  other165: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLTD_is_nilrBOOL(FLTD self) {
 BOOL ret_val = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 L11_=(self)==(self); 
 L2 = L11_;
 L31_=!(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLT_is1351417193(FLT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 FLT L1;
 BOOL L21_;
 if (arg==NULL) {
  goto other164;
 } else
 switch (TAG(arg)) {
  case FLT_tag:
   L1 = ((FLT_boxed) arg)->immutable_part;
   L21_=(self)==(L1); 
   ret_val = L21_;
   return ret_val; break;
  default: ;
  other164: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLT_is_nilrBOOL(FLT self) {
 BOOL ret_val = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 L11_=(self)==(self); 
 L2 = L11_;
 L31_=!(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOUND_1007407331 FMAPAM1635877202(FMAPAM339652544 self, AM_BND1124877163 k) {
 BOUND_1007407331 ret_val;
 INT h = INT_zero;
 AM_BND1124877163 tk;
 AM_BND1124877163 tk1;
 FMAPAM339652544 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM339652544 key_eq_self;
 AM_BND1124877163 key_eq_e1;
 AM_BND1124877163 key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FMAPAM339652544 is_key_nil_self1;
 AM_BND1124877163 is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPAM339652544 key_eq_self1;
 AM_BND1124877163 key_eq_e11;
 AM_BND1124877163 key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 AM_BND1124877163 is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_304458649 L71_;
 TUPAM_304458649 L8;
 TUPAM_304458649 L91_;
 TUPAM_304458649 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_304458649 L171_;
 TUPAM_304458649 L18;
 TUPAM_304458649 L191_;
 TUPAM_304458649 L20;
 INT L221_;
 if ((self==((FMAPAM339652544) NULL))) {
  ret_val = ((BOUND_1007407331) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM339652544)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM339652544)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ947319992) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_a = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM339652544)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM339652544)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM339652544)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((AM_BND1124877163) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ947319992) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_a1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPAM339652544)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((BOUND_1007407331) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOUND_1677815802 FMAPAM661174734(FMAPAM1357596931 self, AM_BND1124877163 k) {
 BOUND_1677815802 ret_val;
 INT h = INT_zero;
 AM_BND1124877163 tk;
 AM_BND1124877163 tk1;
 FMAPAM1357596931 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM1357596931 key_eq_self;
 AM_BND1124877163 key_eq_e1;
 AM_BND1124877163 key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FMAPAM1357596931 is_key_nil_self1;
 AM_BND1124877163 is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPAM1357596931 key_eq_self1;
 AM_BND1124877163 key_eq_e11;
 AM_BND1124877163 key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 AM_BND1124877163 is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_667892060 L71_;
 TUPAM_667892060 L8;
 TUPAM_667892060 L91_;
 TUPAM_667892060 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_667892060 L171_;
 TUPAM_667892060 L18;
 TUPAM_667892060 L191_;
 TUPAM_667892060 L20;
 INT L221_;
 if ((self==((FMAPAM1357596931) NULL))) {
  ret_val = ((BOUND_1677815802) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM1357596931)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM1357596931)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ947319992) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_a = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM1357596931)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM1357596931)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM1357596931)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((AM_BND1124877163) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ947319992) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_a1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPAM1357596931)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((BOUND_1677815802) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTTUPIDENTINT FLISTT1610849855(FLISTTUPIDENTINT self, TUPIDENTINT e) {
 FLISTTUPIDENTINT ret_val;
 FLISTTUPIDENTINT r = ((FLISTTUPIDENTINT) NULL);
 FLISTTUPIDENTINT L11;
 FLISTTUPIDENTINT L21;
 INT L31 = INT_zero;
 FLISTTUPIDENTINT aset_self;
 INT aset_ind = INT_zero;
 TUPIDENTINT aset_val = TUPIDENTINT_zero;
 FLISTTUPIDENTINT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTTUPIDENTINT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 TUPIDENTINT L22;
 TUPIDENTINT L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTTUPIDENTINT) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTTUPIDENTINT_struct)-sizeof(TUPIDENTINT))+(L5)*sizeof(TUPIDENTINT);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTTUPIDENTINT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTTUPIDENTINT) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTTUPIDENTINT)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTTUPIDENTINT)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTTUPIDENTINT_struct)-sizeof(TUPIDENTINT))+(L14)*sizeof(TUPIDENTINT);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTTUPIDENTINT_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTTUPIDENTINT) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTT97637998_frame_struct other1_0;
FLISTT97637998_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTTUPIDENTINT)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTT97637998(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTTUPIDENTINT)L21,L31,L23); 
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
 SARR((FLISTTUPIDENTINT)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdAM_CONST FLISTd246699990(FLISTdAM_CONST self, dAM_CONST e) {
 FLISTdAM_CONST ret_val;
 FLISTdAM_CONST r = ((FLISTdAM_CONST) NULL);
 FLISTdAM_CONST L11;
 FLISTdAM_CONST L21;
 INT L31 = INT_zero;
 FLISTdAM_CONST aset_self;
 INT aset_ind = INT_zero;
 dAM_CONST aset_val;
 FLISTdAM_CONST L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTdAM_CONST L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 dAM_CONST L22;
 dAM_CONST L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTdAM_CONST) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTdAM_CONST_struct)-sizeof(dAM_CONST))+(L5)*sizeof(dAM_CONST);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTdAM_CONST_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTdAM_CONST) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTdAM_CONST)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTdAM_CONST)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTdAM_CONST_struct)-sizeof(dAM_CONST))+(L14)*sizeof(dAM_CONST);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdAM_CONST_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTdAM_CONST) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTd617209260_frame_struct other1_0;
FLISTd617209260_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTdAM_CONST)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTd617209260(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTdAM_CONST)L21,L31,(dAM_CONST)L23); 
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
 SARR((FLISTdAM_CONST)aset_self,aset_ind,(dAM_CONST)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdAM_EXPR FLISTd1364738838(FLISTdAM_EXPR self, FLISTdAM_EXPR l) {
 FLISTdAM_EXPR ret_val;
 FLISTdAM_EXPR res;
 INT oldsize = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT resi = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val1;
 FLISTdAM_EXPR aset_self;
 INT aset_ind = INT_zero;
 dAM_EXPR aset_val;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 BOOL L61_;
 dAM_EXPR L71_;
 INT L91_;
 INT L101_;
 res = self;
 L1 = (l==((FLISTdAM_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  oldsize = FLISTd1406591968(self);
  L3 = FLISTd1406591968(self);
  L4 = FLISTd1406591968(l);
  L51_=INTPLUS(L3,L4); 
  res = FLISTd1492920040(res, L51_);
  i = 0;
  sz = FLISTd1406591968(l);
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
   L71_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
   ret_val1 = L71_;
   aset_val = ret_val1;
   SARR((FLISTdAM_EXPR)aset_self,aset_ind,(dAM_EXPR)aset_val); 
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

FLISTdAM_EXPR FLISTd1492920040(FLISTdAM_EXPR self, INT new_size) {
 FLISTdAM_EXPR ret_val;
 FLISTdAM_EXPR r = ((FLISTdAM_EXPR) NULL);
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val1;
 FLISTdAM_EXPR aset_self;
 INT aset_ind = INT_zero;
 dAM_EXPR aset_val;
 FLISTdAM_EXPR L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 FLISTdAM_EXPR L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 dAM_EXPR L211_;
 INT L231_;
 if ((self==((FLISTdAM_EXPR) NULL))) {
  L31_=(5>new_size)?5:new_size; 
  L2 = L31_;
  L5=(sizeof(struct FLISTdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L2)*sizeof(dAM_EXPR);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTdAM_EXPR_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L1 = ((FLISTdAM_EXPR) L4);
  L1->asize = L2;
  r = L1;
 }
 else {
  L61_=ASIZE((FLISTdAM_EXPR)self); 
  L7 = L61_;
  L81_=(L7)<(new_size); 
  L9 = L81_;
  L101_=!(L9); 
  if (L101_) {
   r = self;
  }
  else {
   L131_=ASIZE((FLISTdAM_EXPR)self); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L16 = L151_;
   L171_=(L16>new_size)?L16:new_size; 
   L12 = L171_;
   L19=(sizeof(struct FLISTdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L12)*sizeof(dAM_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdAM_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L11 = ((FLISTdAM_EXPR) L18);
   L11->asize = L12;
   r = L11;
   SATTR(r,loc,ATTR(self,loc));
   i = 0;
   sz = FLISTd1406591968(self);
   while (1) {
    L201_=(i)==(sz); 
    if (L201_) {
     goto after_loop;
    }
    aset_self = r;
    aset_ind = i;
    aget_self = self;
    aget_ind = i;
    L211_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
    ret_val1 = L211_;
    aset_val = ret_val1;
    SARR((FLISTdAM_EXPR)aset_self,aset_ind,(dAM_EXPR)aset_val); 
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

FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR self, dAM_EXPR e) {
 FLISTdAM_EXPR ret_val;
 FLISTdAM_EXPR r = ((FLISTdAM_EXPR) NULL);
 FLISTdAM_EXPR L11;
 FLISTdAM_EXPR L21;
 INT L31 = INT_zero;
 FLISTdAM_EXPR aset_self;
 INT aset_ind = INT_zero;
 dAM_EXPR aset_val;
 FLISTdAM_EXPR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTdAM_EXPR L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 dAM_EXPR L22;
 dAM_EXPR L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTdAM_EXPR) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L5)*sizeof(dAM_EXPR);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTdAM_EXPR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTdAM_EXPR) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTdAM_EXPR)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTdAM_EXPR)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTdAM_EXPR_struct)-sizeof(dAM_EXPR))+(L14)*sizeof(dAM_EXPR);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdAM_EXPR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTdAM_EXPR) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTd2122660729_frame_struct other1_0;
FLISTd2122660729_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTdAM_EXPR)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTd2122660729(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTdAM_EXPR)L21,L31,(dAM_EXPR)L23); 
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
 SARR((FLISTdAM_EXPR)aset_self,aset_ind,(dAM_EXPR)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdAM_STMT FLISTd506668277(FLISTdAM_STMT self, dAM_STMT e) {
 FLISTdAM_STMT ret_val;
 FLISTdAM_STMT r = ((FLISTdAM_STMT) NULL);
 FLISTdAM_STMT L11;
 FLISTdAM_STMT L21;
 INT L31 = INT_zero;
 FLISTdAM_STMT aset_self;
 INT aset_ind = INT_zero;
 dAM_STMT aset_val;
 FLISTdAM_STMT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTdAM_STMT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 dAM_STMT L22;
 dAM_STMT L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTdAM_STMT) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTdAM_STMT_struct)-sizeof(dAM_STMT))+(L5)*sizeof(dAM_STMT);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTdAM_STMT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTdAM_STMT) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTdAM_STMT)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTdAM_STMT)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTdAM_STMT_struct)-sizeof(dAM_STMT))+(L14)*sizeof(dAM_STMT);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdAM_STMT_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTdAM_STMT) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTd1374595573_frame_struct other1_0;
FLISTd1374595573_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTdAM_STMT)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTd1374595573(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTdAM_STMT)L21,L31,(dAM_STMT)L23); 
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
 SARR((FLISTdAM_STMT)aset_self,aset_ind,(dAM_STMT)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdAM_STMT FLISTd846796316(FLISTdAM_STMT self, INT n) {
 FLISTdAM_STMT ret_val;
 BOOL L11_;
 FLISTdAM_STMT L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTdAM_STMT) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTdAM_STMT_struct)-sizeof(dAM_STMT))+(L3)*sizeof(dAM_STMT);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTdAM_STMT_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTdAM_STMT) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTdLAYOUT FLISTd1429133189(FLISTdLAYOUT self, dLAYOUT e) {
 FLISTdLAYOUT ret_val;
 FLISTdLAYOUT r = ((FLISTdLAYOUT) NULL);
 FLISTdLAYOUT L11;
 FLISTdLAYOUT L21;
 INT L31 = INT_zero;
 FLISTdLAYOUT aset_self;
 INT aset_ind = INT_zero;
 dLAYOUT aset_val;
 FLISTdLAYOUT L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTdLAYOUT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 dLAYOUT L22;
 dLAYOUT L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTdLAYOUT) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTdLAYOUT_struct)-sizeof(dLAYOUT))+(L5)*sizeof(dLAYOUT);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTdLAYOUT_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTdLAYOUT) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTdLAYOUT)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTdLAYOUT)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTdLAYOUT_struct)-sizeof(dLAYOUT))+(L14)*sizeof(dLAYOUT);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdLAYOUT_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTdLAYOUT) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTd1954760550_frame_struct other1_0;
FLISTd1954760550_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTdLAYOUT)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTd1954760550(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTdLAYOUT)L21,L31,(dLAYOUT)L23); 
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
 SARR((FLISTdLAYOUT)aset_self,aset_ind,(dLAYOUT)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTdTP FLISTd1953924847(FLISTdTP self, dTP e) {
 FLISTdTP ret_val;
 FLISTdTP r = ((FLISTdTP) NULL);
 FLISTdTP L11;
 FLISTdTP L21;
 INT L31 = INT_zero;
 FLISTdTP aset_self;
 INT aset_ind = INT_zero;
 dTP aset_val;
 FLISTdTP L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTdTP L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 dTP L22;
 dTP L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTdTP) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTdTP_struct)-sizeof(dTP))+(L5)*sizeof(dTP);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTdTP_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTdTP) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTdTP)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTdTP)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTdTP_struct)-sizeof(dTP))+(L14)*sizeof(dTP);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTdTP_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTdTP) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTd1740602786_frame_struct other1_0;
FLISTd1740602786_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTdTP)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTd1740602786(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTdTP)L21,L31,(dTP)L23); 
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
 SARR((FLISTdTP)aset_self,aset_ind,(dTP)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLT FLT_nilrFLT(FLT self) {
 FLT ret_val = FLT_zero;
 FLT L11_;
 L11_=r_signaling_nan(0); 
 ret_val = L11_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLTD FLTD_nilrFLTD(FLTD self) {
 FLTD ret_val = FLTD_zero;
 FLTD L11_;
 L11_=signaling_nan(0); 
 ret_val = L11_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1145008811 FMAPAM1477123193(FMAPAM1145008811 self, INT n) {
 FMAPAM1145008811 ret_val;
 FMAPAM1145008811 r;
 FMAPAM1145008811 L11;
 FMAPAM1145008811 key_nil_self;
 AM_LOCAL_EXPR ret_val1;
 ELT_NI19483997 elt_nil_self;
 AM_LOCAL_EXPR ret_val2;
 AM_LOCAL_EXPR e = ((AM_LOCAL_EXPR) NULL);
 FMAPAM1145008811 key_nil_self1;
 AM_LOCAL_EXPR ret_val3;
 ELT_NI19483997 elt_nil_self1;
 AM_LOCAL_EXPR ret_val4;
 AM_LOCAL_EXPR e1 = ((AM_LOCAL_EXPR) NULL);
 TUPAM_1519040877 create_self = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR create_at1;
 AM_LOCAL_EXPR create_at2;
 TUPAM_1519040877 ret_val5 = TUPAM_1519040877_zero;
 INT L21 = INT_zero;
 FMAPAM1145008811 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_1519040877 L10;
 TUPAM_1519040877 L12;
 TUPAM_1519040877 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM1145008811_struct)-sizeof(TUPAM_1519040877))+(L4)*sizeof(TUPAM_1519040877);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM1145008811_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM1145008811) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI19483997) NULL);
 ret_val2 = ((AM_LOCAL_EXPR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_LOCAL_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM1145008811)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_1519040877_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI19483997) NULL);
    ret_val4 = ((AM_LOCAL_EXPR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((AM_LOCAL_EXPR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM1145008811)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1145008811 FMAPAM240162218(FMAPAM1145008811 self) {
 FMAPAM1145008811 ret_val;
 INT ns = INT_zero;
 FMAPAM1145008811 r;
 FMAPAM1145008811 L11;
 FMAPAM1145008811 insert_pair_self;
 TUPAM_1519040877 insert_pair_p = TUPAM_1519040877_zero;
 FMAPAM1145008811 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_1519040877 L9;
 L21_=ASIZE((FMAPAM1145008811)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM1477123193(self, ns);
 {
  struct FMAPAM1804153404_frame_struct other1_0;
FMAPAM1804153404_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM1804153404(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM876857188(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
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

FMAPAM1145008811 FMAPAM714369621(FMAPAM1145008811 self, INT n) {
 FMAPAM1145008811 ret_val;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 L1 = FMAPAM498940295;
 L21_=INTTIMES(3,L1); 
 L3 = L21_;
 L41_=INTTIMES(L3,n); 
 L5 = L41_;
 L61_=INTDIV(L5,4); 
 L7 = INT_hi740923800(L61_);
 L81_=INTPLUS(L7,1); 
 L9 = L81_;
 L101_=((unsigned)1)<<((unsigned)L9); 
 L11 = L101_;
 L121_=INTPLUS(L11,1); 
 ret_val = FMAPAM1477123193(self, L121_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1145008811 FMAPAM876857188(FMAPAM1145008811 self, AM_LOCAL_EXPR k, AM_LOCAL_EXPR t) {
 FMAPAM1145008811 ret_val;
 FMAPAM1145008811 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_LOCAL_EXPR tk;
 AM_LOCAL_EXPR tk1;
 FMAPAM1145008811 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM1145008811 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM1145008811 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM1145008811 key_eq_self;
 AM_LOCAL_EXPR key_eq_e1;
 AM_LOCAL_EXPR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val7 = BOOL_zero;
 TUPAM_1519040877 create_self = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR create_at1;
 AM_LOCAL_EXPR create_at2;
 TUPAM_1519040877 ret_val8 = TUPAM_1519040877_zero;
 FMAPAM1145008811 is_key_nil_self1;
 AM_LOCAL_EXPR is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPAM1145008811 key_eq_self1;
 AM_LOCAL_EXPR key_eq_e11;
 AM_LOCAL_EXPR key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val13 = BOOL_zero;
 TUPAM_1519040877 create_self1 = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR create_at11;
 AM_LOCAL_EXPR create_at21;
 TUPAM_1519040877 ret_val14 = TUPAM_1519040877_zero;
 TUPAM_1519040877 create_self2 = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR create_at12;
 AM_LOCAL_EXPR create_at22;
 TUPAM_1519040877 ret_val15 = TUPAM_1519040877_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPAM_1519040877 L201_;
 TUPAM_1519040877 L21;
 TUPAM_1519040877 L22;
 TUPAM_1519040877 L23;
 TUPAM_1519040877 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_1519040877 L281_;
 TUPAM_1519040877 L29;
 TUPAM_1519040877 L30;
 TUPAM_1519040877 L31;
 TUPAM_1519040877 L32;
 INT L341_;
 TUPAM_1519040877 L35;
 TUPAM_1519040877 L36;
 TUPAM_1519040877 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM1145008811) NULL))) {
  r = FMAPAM1477123193(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM1145008811)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM498940295;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM240162218(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM1145008811)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM1145008811)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM1145008811)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ1704040611) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_a = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_1519040877_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPAM1145008811)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((FMAPAM1145008811)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI19483997) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ1704040611) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_a1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPAM_1519040877_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPAM1145008811)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_1519040877_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPAM1145008811)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1357596931 FMAPAM1106357779(FMAPAM1357596931 self, AM_BND1124877163 k, BOUND_1677815802 t) {
 FMAPAM1357596931 ret_val;
 FMAPAM1357596931 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_BND1124877163 tk;
 AM_BND1124877163 tk1;
 FMAPAM1357596931 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM1357596931 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM1357596931 key_eq_self;
 AM_BND1124877163 key_eq_e1;
 AM_BND1124877163 key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val7 = BOOL_zero;
 TUPAM_667892060 create_self = TUPAM_667892060_zero;
 AM_BND1124877163 create_at1;
 BOUND_1677815802 create_at2;
 TUPAM_667892060 ret_val8 = TUPAM_667892060_zero;
 FMAPAM1357596931 is_key_nil_self1;
 AM_BND1124877163 is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPAM1357596931 key_eq_self1;
 AM_BND1124877163 key_eq_e11;
 AM_BND1124877163 key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 AM_BND1124877163 is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val13 = BOOL_zero;
 TUPAM_667892060 create_self1 = TUPAM_667892060_zero;
 AM_BND1124877163 create_at11;
 BOUND_1677815802 create_at21;
 TUPAM_667892060 ret_val14 = TUPAM_667892060_zero;
 TUPAM_667892060 create_self2 = TUPAM_667892060_zero;
 AM_BND1124877163 create_at12;
 BOUND_1677815802 create_at22;
 TUPAM_667892060 ret_val15 = TUPAM_667892060_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPAM_667892060 L201_;
 TUPAM_667892060 L21;
 TUPAM_667892060 L22;
 TUPAM_667892060 L23;
 TUPAM_667892060 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_667892060 L281_;
 TUPAM_667892060 L29;
 TUPAM_667892060 L30;
 TUPAM_667892060 L31;
 TUPAM_667892060 L32;
 INT L341_;
 TUPAM_667892060 L35;
 TUPAM_667892060 L36;
 TUPAM_667892060 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM1357596931) NULL))) {
  r = FMAPAM765222498(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM1357596931)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM29230850;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM48293577(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM1357596931)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM1357596931)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM1357596931)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ947319992) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_a = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_667892060_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPAM1357596931)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((FMAPAM1357596931)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((AM_BND1124877163) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ947319992) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_a1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPAM_667892060_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPAM1357596931)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_667892060_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPAM1357596931)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1357596931 FMAPAM48293577(FMAPAM1357596931 self) {
 FMAPAM1357596931 ret_val;
 INT ns = INT_zero;
 FMAPAM1357596931 r;
 FMAPAM1357596931 L11;
 FMAPAM1357596931 insert_pair_self;
 TUPAM_667892060 insert_pair_p = TUPAM_667892060_zero;
 FMAPAM1357596931 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_667892060 L9;
 L21_=ASIZE((FMAPAM1357596931)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM765222498(self, ns);
 {
  struct FMAPAM1474331777_frame_struct other1_0;
FMAPAM1474331777_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM1474331777(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM1106357779(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
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

FMAPAM1357596931 FMAPAM765222498(FMAPAM1357596931 self, INT n) {
 FMAPAM1357596931 ret_val;
 FMAPAM1357596931 r;
 FMAPAM1357596931 L11;
 FMAPAM1357596931 key_nil_self;
 AM_BND1124877163 ret_val1;
 ELT_NI1564018771 elt_nil_self;
 AM_BND1124877163 ret_val2;
 AM_BND1124877163 e = ((AM_BND1124877163) NULL);
 FMAPAM1357596931 key_nil_self1;
 AM_BND1124877163 ret_val3;
 ELT_NI1564018771 elt_nil_self1;
 AM_BND1124877163 ret_val4;
 AM_BND1124877163 e1 = ((AM_BND1124877163) NULL);
 TUPAM_667892060 create_self = TUPAM_667892060_zero;
 AM_BND1124877163 create_at1;
 BOUND_1677815802 create_at2;
 TUPAM_667892060 ret_val5 = TUPAM_667892060_zero;
 INT L21 = INT_zero;
 FMAPAM1357596931 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_667892060 L10;
 TUPAM_667892060 L12;
 TUPAM_667892060 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM1357596931_struct)-sizeof(TUPAM_667892060))+(L4)*sizeof(TUPAM_667892060);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM1357596931_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM1357596931) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI1564018771) NULL);
 ret_val2 = ((AM_BND1124877163) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_BND1124877163) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM1357596931)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_667892060_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI1564018771) NULL);
    ret_val4 = ((AM_BND1124877163) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((BOUND_1677815802) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM1357596931)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM339652544 FMAPAM136290044(FMAPAM339652544 self, AM_BND1124877163 k, BOUND_1007407331 t) {
 FMAPAM339652544 ret_val;
 FMAPAM339652544 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_BND1124877163 tk;
 AM_BND1124877163 tk1;
 FMAPAM339652544 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM339652544 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM339652544 key_eq_self;
 AM_BND1124877163 key_eq_e1;
 AM_BND1124877163 key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val7 = BOOL_zero;
 TUPAM_304458649 create_self = TUPAM_304458649_zero;
 AM_BND1124877163 create_at1;
 BOUND_1007407331 create_at2;
 TUPAM_304458649 ret_val8 = TUPAM_304458649_zero;
 FMAPAM339652544 is_key_nil_self1;
 AM_BND1124877163 is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPAM339652544 key_eq_self1;
 AM_BND1124877163 key_eq_e11;
 AM_BND1124877163 key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 AM_BND1124877163 is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val13 = BOOL_zero;
 TUPAM_304458649 create_self1 = TUPAM_304458649_zero;
 AM_BND1124877163 create_at11;
 BOUND_1007407331 create_at21;
 TUPAM_304458649 ret_val14 = TUPAM_304458649_zero;
 TUPAM_304458649 create_self2 = TUPAM_304458649_zero;
 AM_BND1124877163 create_at12;
 BOUND_1007407331 create_at22;
 TUPAM_304458649 ret_val15 = TUPAM_304458649_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPAM_304458649 L201_;
 TUPAM_304458649 L21;
 TUPAM_304458649 L22;
 TUPAM_304458649 L23;
 TUPAM_304458649 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_304458649 L281_;
 TUPAM_304458649 L29;
 TUPAM_304458649 L30;
 TUPAM_304458649 L31;
 TUPAM_304458649 L32;
 INT L341_;
 TUPAM_304458649 L35;
 TUPAM_304458649 L36;
 TUPAM_304458649 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM339652544) NULL))) {
  r = FMAPAM597772238(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM339652544)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM441967257;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM2064639115(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM339652544)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM339652544)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM339652544)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ947319992) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_a = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_304458649_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPAM339652544)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((FMAPAM339652544)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((AM_BND1124877163) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ947319992) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_a1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPAM_304458649_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPAM339652544)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_304458649_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPAM339652544)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM339652544 FMAPAM2064639115(FMAPAM339652544 self) {
 FMAPAM339652544 ret_val;
 INT ns = INT_zero;
 FMAPAM339652544 r;
 FMAPAM339652544 L11;
 FMAPAM339652544 insert_pair_self;
 TUPAM_304458649 insert_pair_p = TUPAM_304458649_zero;
 FMAPAM339652544 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_304458649 L9;
 L21_=ASIZE((FMAPAM339652544)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM597772238(self, ns);
 {
  struct FMAPAM709445685_frame_struct other1_0;
FMAPAM709445685_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM709445685(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM136290044(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
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

FMAPAM339652544 FMAPAM597772238(FMAPAM339652544 self, INT n) {
 FMAPAM339652544 ret_val;
 FMAPAM339652544 r;
 FMAPAM339652544 L11;
 FMAPAM339652544 key_nil_self;
 AM_BND1124877163 ret_val1;
 ELT_NI1564018771 elt_nil_self;
 AM_BND1124877163 ret_val2;
 AM_BND1124877163 e = ((AM_BND1124877163) NULL);
 FMAPAM339652544 key_nil_self1;
 AM_BND1124877163 ret_val3;
 ELT_NI1564018771 elt_nil_self1;
 AM_BND1124877163 ret_val4;
 AM_BND1124877163 e1 = ((AM_BND1124877163) NULL);
 TUPAM_304458649 create_self = TUPAM_304458649_zero;
 AM_BND1124877163 create_at1;
 BOUND_1007407331 create_at2;
 TUPAM_304458649 ret_val5 = TUPAM_304458649_zero;
 INT L21 = INT_zero;
 FMAPAM339652544 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_304458649 L10;
 TUPAM_304458649 L12;
 TUPAM_304458649 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM339652544_struct)-sizeof(TUPAM_304458649))+(L4)*sizeof(TUPAM_304458649);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM339652544_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM339652544) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI1564018771) NULL);
 ret_val2 = ((AM_BND1124877163) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_BND1124877163) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM339652544)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_304458649_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI1564018771) NULL);
    ret_val4 = ((AM_BND1124877163) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((BOUND_1007407331) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM339652544)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM418009930 FMAPAM1345096032(FMAPAM418009930 self, AM_LOCAL_EXPR k, OPT_LOCAL_EXPR t) {
 FMAPAM418009930 ret_val;
 FMAPAM418009930 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_LOCAL_EXPR tk;
 AM_LOCAL_EXPR tk1;
 FMAPAM418009930 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM418009930 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM418009930 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM418009930 key_eq_self;
 AM_LOCAL_EXPR key_eq_e1;
 AM_LOCAL_EXPR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val7 = BOOL_zero;
 TUPAM_504653531 create_self = TUPAM_504653531_zero;
 AM_LOCAL_EXPR create_at1;
 OPT_LOCAL_EXPR create_at2;
 TUPAM_504653531 ret_val8 = TUPAM_504653531_zero;
 FMAPAM418009930 is_key_nil_self1;
 AM_LOCAL_EXPR is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPAM418009930 key_eq_self1;
 AM_LOCAL_EXPR key_eq_e11;
 AM_LOCAL_EXPR key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val13 = BOOL_zero;
 TUPAM_504653531 create_self1 = TUPAM_504653531_zero;
 AM_LOCAL_EXPR create_at11;
 OPT_LOCAL_EXPR create_at21;
 TUPAM_504653531 ret_val14 = TUPAM_504653531_zero;
 TUPAM_504653531 create_self2 = TUPAM_504653531_zero;
 AM_LOCAL_EXPR create_at12;
 OPT_LOCAL_EXPR create_at22;
 TUPAM_504653531 ret_val15 = TUPAM_504653531_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPAM_504653531 L201_;
 TUPAM_504653531 L21;
 TUPAM_504653531 L22;
 TUPAM_504653531 L23;
 TUPAM_504653531 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_504653531 L281_;
 TUPAM_504653531 L29;
 TUPAM_504653531 L30;
 TUPAM_504653531 L31;
 TUPAM_504653531 L32;
 INT L341_;
 TUPAM_504653531 L35;
 TUPAM_504653531 L36;
 TUPAM_504653531 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM418009930) NULL))) {
  r = FMAPAM541564112(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM418009930)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM476780631;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM994893385(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM418009930)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM418009930)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM418009930)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ1704040611) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_a = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_504653531_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPAM418009930)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((FMAPAM418009930)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI19483997) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ1704040611) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_a1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPAM_504653531_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPAM418009930)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_504653531_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPAM418009930)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM418009930 FMAPAM541564112(FMAPAM418009930 self, INT n) {
 FMAPAM418009930 ret_val;
 FMAPAM418009930 r;
 FMAPAM418009930 L11;
 FMAPAM418009930 key_nil_self;
 AM_LOCAL_EXPR ret_val1;
 ELT_NI19483997 elt_nil_self;
 AM_LOCAL_EXPR ret_val2;
 AM_LOCAL_EXPR e = ((AM_LOCAL_EXPR) NULL);
 FMAPAM418009930 key_nil_self1;
 AM_LOCAL_EXPR ret_val3;
 ELT_NI19483997 elt_nil_self1;
 AM_LOCAL_EXPR ret_val4;
 AM_LOCAL_EXPR e1 = ((AM_LOCAL_EXPR) NULL);
 TUPAM_504653531 create_self = TUPAM_504653531_zero;
 AM_LOCAL_EXPR create_at1;
 OPT_LOCAL_EXPR create_at2;
 TUPAM_504653531 ret_val5 = TUPAM_504653531_zero;
 INT L21 = INT_zero;
 FMAPAM418009930 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_504653531 L10;
 TUPAM_504653531 L12;
 TUPAM_504653531 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM418009930_struct)-sizeof(TUPAM_504653531))+(L4)*sizeof(TUPAM_504653531);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM418009930_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM418009930) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI19483997) NULL);
 ret_val2 = ((AM_LOCAL_EXPR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_LOCAL_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM418009930)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_504653531_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI19483997) NULL);
    ret_val4 = ((AM_LOCAL_EXPR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((OPT_LOCAL_EXPR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM418009930)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM418009930 FMAPAM994893385(FMAPAM418009930 self) {
 FMAPAM418009930 ret_val;
 INT ns = INT_zero;
 FMAPAM418009930 r;
 FMAPAM418009930 L11;
 FMAPAM418009930 insert_pair_self;
 TUPAM_504653531 insert_pair_p = TUPAM_504653531_zero;
 FMAPAM418009930 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_504653531 L9;
 L21_=ASIZE((FMAPAM418009930)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM541564112(self, ns);
 {
  struct FMAPAM1518099927_frame_struct other1_0;
FMAPAM1518099927_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM1518099927(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM1345096032(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
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

INT FLISTT563337031(FLISTTUPIDENTINT self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTTUPIDENTINT) NULL))) {
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

INT FLISTd102354833(FLISTdLAYOUT self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTdLAYOUT) NULL))) {
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

INT FLISTd1086621942(FLISTdAM_EXPR self, dAM_EXPR e) {
 INT ret_val = INT_zero;
 FLISTdAM_EXPR L11;
 INT r = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val1;
 FLISTdAM_EXPR elt_eq_self;
 dAM_EXPR elt_eq_e1;
 dAM_EXPR elt_eq_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQdAM_EXPR elt_eq_self1;
 dAM_EXPR elt_eq_e11;
 dAM_EXPR elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 BOOL L2;
 BOOL L31_;
 INT L4;
 dAM_EXPR L51_;
 dAM_EXPR L6;
 L2 = (self==((FLISTdAM_EXPR) NULL));
 L31_=!(L2); 
 if (L31_) {
  {
   struct FLISTd311576818_frame_struct other1_0;
FLISTd311576818_frame noname1 = &other1_0;
   L11 = self;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L4 = FLISTd311576818(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    r = L4;
    elt_eq_self = self;
    elt_eq_e1 = e;
    aget_self = self;
    aget_ind = r;
    L51_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
    ret_val1 = L51_;
    elt_eq_e2 = ret_val1;
    elt_eq_self1 = ((ELT_EQdAM_EXPR) NULL);
    elt_eq_e11 = elt_eq_e1;
    elt_eq_e21 = elt_eq_e2;
    L6 = elt_eq_e11;
    ret_val3 = (*dAM_EX2004971809[TAG(L6)])(L6, ((OB) elt_eq_e21));
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

INT FLISTd1406591968(FLISTdAM_EXPR self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTdAM_EXPR) NULL))) {
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

INT FLISTd1855368276(FLISTdAM_CONST self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTdAM_CONST) NULL))) {
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

INT FLISTd355912233(FLISTdAM_STMT self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTdAM_STMT) NULL))) {
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

INT FLISTd895185163(FLISTdTP self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTdTP) NULL))) {
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

INT FLTD_hashrINT(FLTD self) {
 INT ret_val = INT_zero;
 FLTD L11_;
 FLTD L2;
 INT L31_;
 L11_=FLTDTRUNCATE(self); 
 L2 = L11_;
 L31_=(INT)L2; 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLT_hashrINT(FLT self) {
 INT ret_val = INT_zero;
 FLT L11_;
 FLT L2;
 INT L31_;
 L11_=FLTTRUNCATE(self); 
 L2 = L11_;
 L31_=FLTINT(L2); 
 ret_val = INT_hashrINT(L31_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

OPT_LOCAL_EXPR FMAPAM731742676(FMAPAM418009930 self, AM_LOCAL_EXPR k) {
 OPT_LOCAL_EXPR ret_val;
 INT h = INT_zero;
 AM_LOCAL_EXPR tk;
 AM_LOCAL_EXPR tk1;
 FMAPAM418009930 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM418009930 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM418009930 key_eq_self;
 AM_LOCAL_EXPR key_eq_e1;
 AM_LOCAL_EXPR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FMAPAM418009930 is_key_nil_self1;
 AM_LOCAL_EXPR is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPAM418009930 key_eq_self1;
 AM_LOCAL_EXPR key_eq_e11;
 AM_LOCAL_EXPR key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_504653531 L71_;
 TUPAM_504653531 L8;
 TUPAM_504653531 L91_;
 TUPAM_504653531 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_504653531 L171_;
 TUPAM_504653531 L18;
 TUPAM_504653531 L191_;
 TUPAM_504653531 L20;
 INT L221_;
 if ((self==((FMAPAM418009930) NULL))) {
  ret_val = ((OPT_LOCAL_EXPR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM418009930)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM418009930)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ1704040611) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_a = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM418009930)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM418009930)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM418009930)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI19483997) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ1704040611) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_a1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPAM418009930)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((OPT_LOCAL_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1090491199(FLISTT844804258 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT844804258) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2074944584(((ELT_AL1476366487) NULL), ((dELTTU1449135940) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1312754552(FLISTT1075494997 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1075494997) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1950462334(((ELT_AL110515410) NULL), ((dELTTU405023937) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1350994583(FLISTTUPSTRINT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSTRINT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL782304860(((ELT_ALGTUPSTRINT) NULL), ((dELTTUPSTRINT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1410881023(FLISTTUPdOBSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPdOBSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1237867596(((ELT_ALGTUPdOBSTR) NULL), ((dELTTUPdOBSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1451978730(FLISTT31683607 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT31683607) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL966666787(((ELT_AL1371565672) NULL), ((dELTTU1754236182) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1532344775(FLISTT1683165204 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1683165204) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL47866227(((ELT_AL616374741) NULL), ((dELTTU1450772879) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT158032289(FLISTT1858637310 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1858637310) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1178944168(((ELT_AL1089356553) NULL), ((dELTTU32753540) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1848908808(FLISTT406286251 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT406286251) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1083506231(((ELT_AL583594762) NULL), ((dELTTU1982974052) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1906991113(FLISTT1023127882 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1023127882) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1448140707(((ELT_AL383472681) NULL), ((dELTTU770070093) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT1916592887(FLISTT567859354 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT567859354) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL772038856(((ELT_AL1361870497) NULL), ((dELTTU1827781436) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT208196008(FLISTT418307355 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT418307355) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1818663869(((ELT_AL734263878) NULL), ((dELTTU216883428) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT275941467(FLISTTUPSIGSIG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSIGSIG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1978058288(((ELT_ALGTUPSIGSIG) NULL), ((dELTTUPSIGSIG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT306797234(FLISTT1513788273 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1513788273) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2030803742(((ELT_AL389001304) NULL), ((dELTTU1079126879) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT307474581(FLISTTUPSTRSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSTRSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL26398608(((ELT_ALGTUPSTRSTR) NULL), ((dELTTUPSTRSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT32449926(FLISTT789237687 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT789237687) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1304624690(((ELT_AL4773456) NULL), ((dELTTU470684395) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT496865671(FLISTTUPSIGSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSIGSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1458120312(((ELT_ALGTUPSIGSTR) NULL), ((dELTTUPSIGSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT499676452(FLISTT102367225 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT102367225) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1860341546(((ELT_AL629085806) NULL), ((dELTTU186609637) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT528398785(FLISTTUPSTRSIG self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSTRSIG) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL546336584(((ELT_ALGTUPSTRSIG) NULL), ((dELTTUPSTRSIG) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT544048196(FLISTTUPIDENTdTP self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPIDENTdTP) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL510829178(((ELT_AL655625458) NULL), ((dELTTUPIDENTdTP) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT546654331(FLISTTUPSIGINT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPSIGINT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL649416844(((ELT_ALGTUPSIGINT) NULL), ((dELTTUPSIGINT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT549250782(FLISTTUPdTPINT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPdTPINT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2046630250(((ELT_ALGTUPdTPINT) NULL), ((dELTTUPdTPINT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT580276949(FLISTT471737672 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT471737672) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1921017260(((ELT_AL322273471) NULL), ((dELTTU788184410) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT582206950(FLISTT994805427 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT994805427) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1675438629(((ELT_AL2147376660) NULL), ((dELTTU1629996210) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT660722487(FLISTTUPIDENTINT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTTUPIDENTINT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL429905904(((ELT_AL655438175) NULL), ((dELTTUPIDENTINT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT689628623(FLISTT2049829484 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT2049829484) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL2104989740(((ELT_AL898164379) NULL), ((dELTTU158438634) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT88767660(FLISTT880505585 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT880505585) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1930239142(((ELT_AL2005292554) NULL), ((dELTTU1315166979) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTT919810588(FLISTT1742988937 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTT1742988937) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1053727475(((ELT_AL1639104906) NULL), ((dELTTU1411620812) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTd1101907210(FLISTdAM_STMT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTdAM_STMT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1121979891(((ELT_ALGdAM_STMT) NULL), ((dELTdAM_STMT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTd1314612993(FLISTdLAYOUT self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTdLAYOUT) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL218385468(((ELT_ALGdLAYOUT) NULL), ((dELTdLAYOUT) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTd1757835884(FLISTdAM_CONST self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTdAM_CONST) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL918382858(((ELT_ALGdAM_CONST) NULL), ((dELTdAM_CONST) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTd2007506601(FLISTdAM_EXPR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTdAM_EXPR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL184221807(((ELT_ALGdAM_EXPR) NULL), ((dELTdAM_EXPR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTdTP_strrSTR(FLISTdTP self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTdTP) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1873149708(((ELT_ALGdTP) NULL), ((dELTdTP) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLTD_strrSTR(FLTD self) {
 STR ret_val;
 FSTR fdbuf;
 FSTR fstr;
 FLTD str_in_self = FLTD_zero;
 FSTR str_in_arg;
 FSTR ret_val1;
 FSTR store_in = ((FSTR) NULL);
 INT sz = INT_zero;
 FSTR str_self;
 STR ret_val2;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L51_;
 fdbuf = FSTR_c1998740379(((FSTR) NULL), 30);
 str_in_self = self;
 str_in_arg = fdbuf;
 L1 = FSTR_sizerINT(str_in_arg);
 L21_=(L1)<(30); 
 L3 = L21_;
 L41_=!(L3); 
 if (L41_) {
  store_in = str_in_arg;
 }
 else {
  store_in = FSTR_c1998740379(((FSTR) NULL), 30);
 }
 L51_=FLTDSTORE(str_in_self,store_in); 
 sz = L51_;
 SATTR(store_in,loc,sz);
 ret_val1 = store_in;
 fstr = ret_val1;
 str_self = fstr;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FLT_strrSTR(FLT self) {
 STR ret_val;
 FSTR fbuf;
 FSTR fstr;
 FLT str_in_self = FLT_zero;
 FSTR str_in_arg;
 FSTR ret_val1;
 FSTR store_in = ((FSTR) NULL);
 INT sz = INT_zero;
 FSTR str_self;
 STR ret_val2;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 INT L51_;
 fbuf = FSTR_c1998740379(((FSTR) NULL), 30);
 str_in_self = self;
 str_in_arg = fbuf;
 L1 = FSTR_sizerINT(str_in_arg);
 L21_=(L1)<(30); 
 L3 = L21_;
 L41_=!(L3); 
 if (L41_) {
  store_in = str_in_arg;
 }
 else {
  store_in = FSTR_c1998740379(((FSTR) NULL), 30);
 }
 L51_=FLTSTORE(str_in_self,store_in); 
 sz = L51_;
 SATTR(store_in,loc,sz);
 ret_val1 = store_in;
 fstr = ret_val1;
 str_self = fstr;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPAM1084073011(FMAPAM339652544 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_304458649 p = TUPAM_304458649_zero;
 AM_BND1124877163 k;
 BOUND_1007407331 e;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_304458649 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 FSTR L7;
 FSTR L8;
 extern STR name10;
 extern STR name10;
 INT L91_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPAM339652544)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM339652544)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name10));
   L8 = FSTR_p1752847026(L7, ATTR(e,str));
   res = FSTR_p1752847026(L8, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
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

STR FMAPAM1201551978(FMAPAM1357596931 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_667892060 p = TUPAM_667892060_zero;
 AM_BND1124877163 k;
 BOUND_1677815802 e;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_667892060 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 FSTR L7;
 FSTR L8;
 extern STR name10;
 extern STR name10;
 INT L91_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPAM1357596931)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM1357596931)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI1564018771) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_BND1124877163) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name10));
   L8 = FSTR_p1752847026(L7, ATTR(e,str));
   res = FSTR_p1752847026(L8, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
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

STR FMAPAM1620534738(FMAPAM1145008811 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_1519040877 p = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR k;
 AM_LOCAL_EXPR e;
 FMAPAM1145008811 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_1519040877 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPAM1145008811)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM1145008811)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   res = FSTR_p1752847026(res, ((STR) &name10));
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

STR FMAPAM905477833(FMAPAM418009930 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_504653531 p = TUPAM_504653531_zero;
 AM_LOCAL_EXPR k;
 OPT_LOCAL_EXPR e;
 FMAPAM418009930 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_504653531 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPAM418009930)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM418009930)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI19483997) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   res = FSTR_p1752847026(res, ((STR) &name10));
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

TUPIDENTINT FLISTT108744548(FLISTTUPIDENTINT self) {
 TUPIDENTINT ret_val = TUPIDENTINT_zero;
 TUPIDENTINT r = TUPIDENTINT_zero;
 FLISTTUPIDENTINT aget_self;
 INT aget_ind = INT_zero;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 FLISTTUPIDENTINT aset_self;
 INT aset_ind = INT_zero;
 TUPIDENTINT aset_val = TUPIDENTINT_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 TUPIDENTINT L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTT563337031(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = TUPIDENTINT_zero;
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=ARR((FLISTTUPIDENTINT)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = TUPIDENTINT_zero;
 SARR((FLISTTUPIDENTINT)aset_self,aset_ind,aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void FLISTd1252754628(FLISTdAM_EXPR self, INT ind1) {
 INT i = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val;
 FLISTdAM_EXPR aset_self;
 INT aset_ind = INT_zero;
 dAM_EXPR aset_val;
 INT L11_;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 INT L61_;
 dAM_EXPR L71_;
 INT L91_;
 INT L10;
 INT L121_;
 L11_=INTPLUS(ind1,1); 
 i = L11_;
 while (1) {
  L2 = FLISTd1406591968(self);
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
  L71_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val = L71_;
  aset_val = ret_val;
  SARR((FLISTdAM_EXPR)aset_self,aset_ind,(dAM_EXPR)aset_val); 
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

BOUND_1007407331 FMAPAM1324815836(FMAPAM1324815836_frame frame) {
 BOUND_1007407331 dummy = ((BOUND_1007407331) NULL);
 BOOL L1;
 BOOL L21_;
 TUPAM_304458649 L5;
 TUPAM_304458649 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM339652544) NULL));
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
     frame->L41_br=ASIZE((FMAPAM339652544)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM339652544)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NI1564018771) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_BND1124877163) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->e.t2;
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

BOUND_1677815802 FMAPAM1626870716(FMAPAM1626870716_frame frame) {
 BOUND_1677815802 dummy = ((BOUND_1677815802) NULL);
 BOOL L1;
 BOOL L21_;
 TUPAM_667892060 L5;
 TUPAM_667892060 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM1357596931) NULL));
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
     frame->L41_br=ASIZE((FMAPAM1357596931)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM1357596931)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NI1564018771) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_BND1124877163) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->e.t2;
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

INT FLISTd1990200313(FLISTd1990200313_frame frame) {
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
 L1 = (frame->self==((FLISTdTP) NULL));
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

INT FLISTd311576818(FLISTd311576818_frame frame) {
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
 L1 = (frame->self==((FLISTdAM_EXPR) NULL));
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

INT FLISTd739102917(FLISTd739102917_frame frame) {
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
 L1 = (frame->self==((FLISTdAM_STMT) NULL));
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

TUPAM_1024994801 FLISTT234903853(FLISTT234903853_frame frame) {
 TUPAM_1024994801 dummy = TUPAM_1024994801_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_1024994801 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT31683607) NULL));
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
   L41_=ARR((FLISTT31683607)frame->aget_self,frame->aget_ind); 
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

TUPAM_1519040877 FLISTT344492076(FLISTT344492076_frame frame) {
 TUPAM_1519040877 dummy = TUPAM_1519040877_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_1519040877 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT2049829484) NULL));
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
   L41_=ARR((FLISTT2049829484)frame->aget_self,frame->aget_ind); 
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

TUPAM_1519040877 FMAPAM1804153404(FMAPAM1804153404_frame frame) {
 TUPAM_1519040877 dummy = TUPAM_1519040877_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_1519040877 L5;
 TUPAM_1519040877 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM1145008811) NULL));
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
     frame->L41_br=ASIZE((FMAPAM1145008811)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM1145008811)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI19483997) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
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

TUPAM_1519040877 FMAPAM725708071(FMAPAM725708071_frame frame) {
 TUPAM_1519040877 dummy = TUPAM_1519040877_zero;
 TUPAM_1519040877 aI_u_I;
 TUPAM_1519040877 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1145008811)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM1145008811)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_304458649 FMAPAM1112691238(FMAPAM1112691238_frame frame) {
 TUPAM_304458649 dummy = TUPAM_304458649_zero;
 TUPAM_304458649 aI_u_I;
 TUPAM_304458649 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM339652544)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM339652544)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_304458649 FMAPAM709445685(FMAPAM709445685_frame frame) {
 TUPAM_304458649 dummy = TUPAM_304458649_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_304458649 L5;
 TUPAM_304458649 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM339652544) NULL));
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
     frame->L41_br=ASIZE((FMAPAM339652544)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM339652544)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI1564018771) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_BND1124877163) NULL));
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

TUPAM_504653531 FLISTT1883920969(FLISTT1883920969_frame frame) {
 TUPAM_504653531 dummy = TUPAM_504653531_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_504653531 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT406286251) NULL));
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
   L41_=ARR((FLISTT406286251)frame->aget_self,frame->aget_ind); 
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

TUPAM_504653531 FMAPAM1263439864(FMAPAM1263439864_frame frame) {
 TUPAM_504653531 dummy = TUPAM_504653531_zero;
 TUPAM_504653531 aI_u_I;
 TUPAM_504653531 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM418009930)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM418009930)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_504653531 FMAPAM1518099927(FMAPAM1518099927_frame frame) {
 TUPAM_504653531 dummy = TUPAM_504653531_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_504653531 L5;
 TUPAM_504653531 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM418009930) NULL));
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
     frame->L41_br=ASIZE((FMAPAM418009930)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM418009930)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI19483997) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_LOCAL_EXPR) NULL));
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

TUPAM_653469574 FLISTT288887925(FLISTT288887925_frame frame) {
 TUPAM_653469574 dummy = TUPAM_653469574_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPAM_653469574 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1683165204) NULL));
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
   L41_=ARR((FLISTT1683165204)frame->aget_self,frame->aget_ind); 
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

TUPAM_667892060 FMAPAM1474331777(FMAPAM1474331777_frame frame) {
 TUPAM_667892060 dummy = TUPAM_667892060_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_667892060 L5;
 TUPAM_667892060 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM1357596931) NULL));
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
     frame->L41_br=ASIZE((FMAPAM1357596931)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM1357596931)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI1564018771) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_BND1124877163) NULL));
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

TUPAM_667892060 FMAPAM1638362026(FMAPAM1638362026_frame frame) {
 TUPAM_667892060 dummy = TUPAM_667892060_zero;
 TUPAM_667892060 aI_u_I;
 TUPAM_667892060 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1357596931)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM1357596931)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDE641654124 FLISTT1472123735(FLISTT1472123735_frame frame) {
 TUPIDE641654124 dummy = TUPIDE641654124_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPIDE641654124 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1023127882) NULL));
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
   L41_=ARR((FLISTT1023127882)frame->aget_self,frame->aget_ind); 
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

TUPIDENTINT FLISTT97637998(FLISTT97637998_frame frame) {
 TUPIDENTINT dummy = TUPIDENTINT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPIDENTINT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPIDENTINT) NULL));
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
   L41_=ARR((FLISTTUPIDENTINT)frame->aget_self,frame->aget_ind); 
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

TUPIDENTdTP FLISTT1769198926(FLISTT1769198926_frame frame) {
 TUPIDENTdTP dummy = TUPIDENTdTP_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPIDENTdTP L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPIDENTdTP) NULL));
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
   L41_=ARR((FLISTTUPIDENTdTP)frame->aget_self,frame->aget_ind); 
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

TUPSIG1754078736 FLISTT287332146(FLISTT287332146_frame frame) {
 TUPSIG1754078736 dummy = TUPSIG1754078736_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIG1754078736 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1075494997) NULL));
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
   L41_=ARR((FLISTT1075494997)frame->aget_self,frame->aget_ind); 
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

TUPSIG2023533247 FLISTT685872269(FLISTT685872269_frame frame) {
 TUPSIG2023533247 dummy = TUPSIG2023533247_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIG2023533247 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT994805427) NULL));
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
   L41_=ARR((FLISTT994805427)frame->aget_self,frame->aget_ind); 
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

TUPSIG578692189 FLISTT754641600(FLISTT754641600_frame frame) {
 TUPSIG578692189 dummy = TUPSIG578692189_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIG578692189 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT844804258) NULL));
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
   L41_=ARR((FLISTT844804258)frame->aget_self,frame->aget_ind); 
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

TUPSIGARG_LAYOUT FLISTT634936167(FLISTT634936167_frame frame) {
 TUPSIGARG_LAYOUT dummy = TUPSIGARG_LAYOUT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIGARG_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1742988937) NULL));
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
   L41_=ARR((FLISTT1742988937)frame->aget_self,frame->aget_ind); 
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

TUPSIGINT FLISTT777802736(FLISTT777802736_frame frame) {
 TUPSIGINT dummy = TUPSIGINT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIGINT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSIGINT) NULL));
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
   L41_=ARR((FLISTTUPSIGINT)frame->aget_self,frame->aget_ind); 
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

TUPSIGSIG FLISTT330033880(FLISTT330033880_frame frame) {
 TUPSIGSIG dummy = TUPSIGSIG_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIGSIG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSIGSIG) NULL));
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
   L41_=ARR((FLISTTUPSIGSIG)frame->aget_self,frame->aget_ind); 
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

TUPSIGSTR FLISTT875874984(FLISTT875874984_frame frame) {
 TUPSIGSTR dummy = TUPSIGSTR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSIGSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSIGSTR) NULL));
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
   L41_=ARR((FLISTTUPSIGSTR)frame->aget_self,frame->aget_ind); 
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

TUPSTRFSETSTR FLISTT720864404(FLISTT720864404_frame frame) {
 TUPSTRFSETSTR dummy = TUPSTRFSETSTR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSTRFSETSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT471737672) NULL));
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
   L41_=ARR((FLISTT471737672)frame->aget_self,frame->aget_ind); 
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

TUPSTRINT FLISTT509547040(FLISTT509547040_frame frame) {
 TUPSTRINT dummy = TUPSTRINT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSTRINT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSTRINT) NULL));
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
   L41_=ARR((FLISTTUPSTRINT)frame->aget_self,frame->aget_ind); 
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

TUPSTRSIG FLISTT957315896(FLISTT957315896_frame frame) {
 TUPSTRSIG dummy = TUPSTRSIG_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSTRSIG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSTRSIG) NULL));
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
   L41_=ARR((FLISTTUPSTRSIG)frame->aget_self,frame->aget_ind); 
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

TUPSTRSTR FLISTT411474792(FLISTT411474792_frame frame) {
 TUPSTRSTR dummy = TUPSTRSTR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSTRSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPSTRSTR) NULL));
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
   L41_=ARR((FLISTTUPSTRSTR)frame->aget_self,frame->aget_ind); 
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

TUPSTRdCONFIG FLISTT1662091704(FLISTT1662091704_frame frame) {
 TUPSTRdCONFIG dummy = TUPSTRdCONFIG_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPSTRdCONFIG L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT789237687) NULL));
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
   L41_=ARR((FLISTT789237687)frame->aget_self,frame->aget_ind); 
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

TUPTP_1668454172 FLISTT1725186742(FLISTT1725186742_frame frame) {
 TUPTP_1668454172 dummy = TUPTP_1668454172_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPTP_1668454172 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT102367225) NULL));
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
   L41_=ARR((FLISTT102367225)frame->aget_self,frame->aget_ind); 
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

TUPTP_858321267 FLISTT699342697(FLISTT699342697_frame frame) {
 TUPTP_858321267 dummy = TUPTP_858321267_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPTP_858321267 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT418307355) NULL));
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
   L41_=ARR((FLISTT418307355)frame->aget_self,frame->aget_ind); 
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

TUPdOBNAMESPACE FLISTT396553018(FLISTT396553018_frame frame) {
 TUPdOBNAMESPACE dummy = TUPdOBNAMESPACE_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdOBNAMESPACE L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1513788273) NULL));
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
   L41_=ARR((FLISTT1513788273)frame->aget_self,frame->aget_ind); 
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

TUPdOBSTR FLISTT220152704(FLISTT220152704_frame frame) {
 TUPdOBSTR dummy = TUPdOBSTR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdOBSTR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPdOBSTR) NULL));
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
   L41_=ARR((FLISTTUPdOBSTR)frame->aget_self,frame->aget_ind); 
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

TUPdTP1710233051 FLISTT1760042796(FLISTT1760042796_frame frame) {
 TUPdTP1710233051 dummy = TUPdTP1710233051_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdTP1710233051 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT1858637310) NULL));
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
   L41_=ARR((FLISTT1858637310)frame->aget_self,frame->aget_ind); 
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

TUPdTPCODE_FILE FLISTT1568525630(FLISTT1568525630_frame frame) {
 TUPdTPCODE_FILE dummy = TUPdTPCODE_FILE_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdTPCODE_FILE L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT880505585) NULL));
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
   L41_=ARR((FLISTT880505585)frame->aget_self,frame->aget_ind); 
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

TUPdTPINT FLISTT1268409508(FLISTT1268409508_frame frame) {
 TUPdTPINT dummy = TUPdTPINT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdTPINT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTTUPdTPINT) NULL));
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
   L41_=ARR((FLISTTUPdTPINT)frame->aget_self,frame->aget_ind); 
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

TUPdTPdLAYOUT FLISTT123078916(FLISTT123078916_frame frame) {
 TUPdTPdLAYOUT dummy = TUPdTPdLAYOUT_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 TUPdTPdLAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTT567859354) NULL));
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
   L41_=ARR((FLISTT567859354)frame->aget_self,frame->aget_ind); 
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

dAM_CONST FLISTd617209260(FLISTd617209260_frame frame) {
 dAM_CONST dummy = ((dAM_CONST) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 dAM_CONST L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTdAM_CONST) NULL));
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
   L41_=(dAM_CONST)ARR((FLISTdAM_CONST)frame->aget_self,frame->aget_ind); 
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

dAM_EXPR FLISTd2122660729(FLISTd2122660729_frame frame) {
 dAM_EXPR dummy = ((dAM_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 dAM_EXPR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTdAM_EXPR) NULL));
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
   L41_=(dAM_EXPR)ARR((FLISTdAM_EXPR)frame->aget_self,frame->aget_ind); 
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

dAM_STMT FLISTd1374595573(FLISTd1374595573_frame frame) {
 dAM_STMT dummy = ((dAM_STMT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 dAM_STMT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTdAM_STMT) NULL));
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
   L41_=(dAM_STMT)ARR((FLISTdAM_STMT)frame->aget_self,frame->aget_ind); 
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

dLAYOUT FLISTd1954760550(FLISTd1954760550_frame frame) {
 dLAYOUT dummy = ((dLAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 dLAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTdLAYOUT) NULL));
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
   L41_=(dLAYOUT)ARR((FLISTdLAYOUT)frame->aget_self,frame->aget_ind); 
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

dTP FLISTd1740602786(FLISTd1740602786_frame frame) {
 dTP dummy = ((dTP) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 dTP L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTdTP) NULL));
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
   L41_=(dTP)ARR((FLISTdTP)frame->aget_self,frame->aget_ind); 
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

void FLISTT1251913564(FLISTT1251913564_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTTUPIDENTINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTTUPIDENTINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FLISTd1190179840(FLISTd1190179840_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTdLAYOUT)frame->self,frame->i_I_u_I,(dLAYOUT)frame->arg1); frame->i_I_u_I++;
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

void FLISTd1458554251(FLISTd1458554251_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTdAM_STMT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTdAM_STMT)frame->self,frame->i_I_u_I,(dAM_STMT)frame->arg1); frame->i_I_u_I++;
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

void FLISTd344567952(FLISTd344567952_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTdTP)frame->self,frame->i_I_u_I,(dTP)frame->arg1); frame->i_I_u_I++;
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

void FLISTd597134281(FLISTd597134281_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTdAM_EXPR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTdAM_EXPR)frame->self,frame->i_I_u_I,(dAM_EXPR)frame->arg1); frame->i_I_u_I++;
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

void FLISTd68199614(FLISTd68199614_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTdAM_CONST)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTdAM_CONST)frame->self,frame->i_I_u_I,(dAM_CONST)frame->arg1); frame->i_I_u_I++;
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

void FMAPAM1361186976(FMAPAM1361186976_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1357596931)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM1357596931)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPAM1521097993(FMAPAM1521097993_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1145008811)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM1145008811)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPAM2127544368(FMAPAM2127544368_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM339652544)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM339652544)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPAM804020802(FMAPAM804020802_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM418009930)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM418009930)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
