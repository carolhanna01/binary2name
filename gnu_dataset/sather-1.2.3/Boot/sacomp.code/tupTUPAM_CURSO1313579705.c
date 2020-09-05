#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dCONFIG_struct {
 OB_HEADER header;
 } *dCONFIG;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ABSTRA151498155_struct {/* layout for ABSTRACT_FRAME_LAYOUT */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *sig1;
 STR str;
 } *ABSTRA151498155;

typedef struct ARG_LAYOUT_struct {/* layout for ARG_LAYOUT */
 OB_HEADER header;
 struct SIG_struct *sig1;
 STR str;
 } *ARG_LAYOUT;

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

typedef struct ELT_EQdAM_EXPR_struct {/* layout for ELT_EQ{$AM_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAM_EXPR;

typedef struct ELT_EQdAM_STMT_struct {/* layout for ELT_EQ{$AM_STMT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAM_STMT;

typedef struct ELT_EQdAS_EXPR_struct {/* layout for ELT_EQ{$AS_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAS_EXPR;

typedef struct ELT_EQdCONFIG_struct {/* layout for ELT_EQ{$CONFIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdCONFIG;

typedef struct ELT_EQdLAYOUT_struct {/* layout for ELT_EQ{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdLAYOUT;

typedef struct ELT_EQdOB_struct {/* layout for ELT_EQ{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdOB;

typedef struct ELT_EQdTP_struct {/* layout for ELT_EQ{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdTP;

typedef struct ELT_EQ1002048960_struct {/* layout for ELT_EQ{ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1002048960;

typedef struct ELT_EQAM_CURSOR_struct {/* layout for ELT_EQ{AM_CURSOR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQAM_CURSOR;

typedef struct ELT_EQ1704040611_struct {/* layout for ELT_EQ{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1704040611;

typedef struct ELT_EQ702804563_struct {/* layout for ELT_EQ{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ702804563;

typedef struct ELT_EQ1304198768_struct {/* layout for ELT_EQ{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1304198768;

typedef struct ELT_EQARG_LAYOUT_struct {/* layout for ELT_EQ{ARG_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQARG_LAYOUT;

typedef struct ELT_EQARRAYdTP_struct {/* layout for ELT_EQ{ARRAY{$TP}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQARRAYdTP;

typedef struct ELT_EQARRAYARG_struct {/* layout for ELT_EQ{ARRAY{ARG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQARRAYARG;

typedef struct ELT_EQARRAYBOOL_struct {/* layout for ELT_EQ{ARRAY{BOOL}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQARRAYBOOL;

typedef struct ELT_EQ390028401_struct {/* layout for ELT_EQ{AS_ARG_MODE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ390028401;

typedef struct ELT_EQBOOL_struct {/* layout for ELT_EQ{BOOL} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQBOOL;

typedef struct ELT_EQCODE_FILE_struct {/* layout for ELT_EQ{CODE_FILE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQCODE_FILE;

typedef struct ELT_EQ997295515_struct {/* layout for ELT_EQ{FLIST{AM_BND_ITER_CALL_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ997295515;

typedef struct ELT_EQ1504893818_struct {/* layout for ELT_EQ{FLIST{AM_ITER_CALL_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1504893818;

typedef struct ELT_EQ557769602_struct {/* layout for ELT_EQ{FMAP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ557769602;

typedef struct ELT_EQ602167759_struct {/* layout for ELT_EQ{FRAME_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ602167759;

typedef struct ELT_EQFSETdTP_struct {/* layout for ELT_EQ{FSET{$TP}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQFSETdTP;

typedef struct ELT_EQFSETSTR_struct {/* layout for ELT_EQ{FSET{STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQFSETSTR;

typedef struct ELT_EQ1354147196_struct {/* layout for ELT_EQ{FSET{TP_CLASS}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1354147196;

typedef struct ELT_EQIDENT_struct {/* layout for ELT_EQ{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQIDENT;

typedef struct ELT_EQINTI_struct {/* layout for ELT_EQ{INTI} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQINTI;

typedef struct ELT_EQINT_struct {/* layout for ELT_EQ{INT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQINT;

typedef struct ELT_EQNAMESPACE_struct {/* layout for ELT_EQ{NAMESPACE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQNAMESPACE;

typedef struct ELT_EQ1284625691_struct {/* layout for ELT_EQ{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1284625691;

typedef struct ELT_EQSIG_struct {/* layout for ELT_EQ{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQSIG;

typedef struct ELT_EQSTR_struct {/* layout for ELT_EQ{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQSTR;

typedef struct ELT_EQTP_CLASS_struct {/* layout for ELT_EQ{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQTP_CLASS;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct FRAME_LAYOUT_struct {/* layout for FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_ROUT_DEF_struct *f;
 struct PROG_struct *prog;
 STR str;
 } *FRAME_LAYOUT;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct INTI1_struct {/* layout for INTI */
 OB_HEADER header;
 INT len1;
 INT asize;
 INT arr_part[1];
 } *INTI1;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

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

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

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

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

#include "tags.h"

/* Globals */

extern FSTR INTI_buf;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST STR (**dCONFIG_strrSTR)(dCONFIG);

extern RETURNED_CONST STR (**dLAYOUT_strrSTR)(dLAYOUT);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
BOOL AM_CUR1885691330(AM_CURSOR, AM_CURSOR);
BOOL AM_SHA623484008(AM_SHARED_EXPR, OB);
BOOL ELT_EQ1393525399(ELT_EQdLAYOUT, dLAYOUT, dLAYOUT);
BOOL ELT_EQ1977650129(ELT_EQdCONFIG, dCONFIG, dCONFIG);
BOOL ELT_EQ2067825030(ELT_EQdAM_STMT, dAM_STMT, dAM_STMT);
BOOL ELT_EQ686719113(ELT_EQdAS_EXPR, dAS_EXPR, dAS_EXPR);
BOOL ELT_EQ936498558(ELT_EQdOB, OB, OB);
BOOL STR_is111530248(STR, STR);
BOOL TUPAM_1144039098(TUPAM_1024994801, OB);
BOOL TUPAM_1196406350(TUPAM_504653531, OB);
BOOL TUPAM_1508779545(TUPAM_1024994801, TUPAM_1024994801);
BOOL TUPAM_1557192709(TUPAM_1519040877, OB);
BOOL TUPAM_1677695562(TUPAM_1519040877, TUPAM_1519040877);
BOOL TUPAM_1711472971(TUPAM_653469574, OB);
BOOL TUPAM_1783991982(TUPAM_927437481, OB);
BOOL TUPAM_1827433739(TUPAM_504653531, TUPAM_504653531);
BOOL TUPAM_1888140103(TUPAM_653469574, TUPAM_653469574);
BOOL TUPAM_499588693(TUPAM_927437481, TUPAM_927437481);
BOOL TUPARR1107471104(TUPARRAYARGdTP, OB);
BOOL TUPARR1838121920(TUPARR1759987776, OB);
BOOL TUPARR2002873565(TUPARRAYARGdTP, TUPARRAYARGdTP);
BOOL TUPARR247873404(TUPARR1759987776, TUPARR1759987776);
BOOL TUPBOO1364843103(TUPBOO1392923241, TUPBOO1392923241);
BOOL TUPBOO323223880(TUPBOO1392923241, OB);
BOOL TUPIDE1046275513(TUPIDE641654124, TUPIDE641654124);
BOOL TUPIDE1215794628(TUPIDENTdTP, TUPIDENTdTP);
BOOL TUPIDE1222548283(TUPIDE641654124, OB);
BOOL TUPIDE1439069590(TUPIDENTINT, TUPIDENTINT);
BOOL TUPIDE1955445151(TUPIDENTARRAYdTP, OB);
BOOL TUPIDE324380005(TUPIDENTARRAYdTP, TUPIDENTARRAYdTP);
BOOL TUPIDE429159832(TUPIDENTdTP, OB);
BOOL TUPIDE53463463(TUPIDENTINT, OB);
BOOL TUPINT326447217(TUPINTINT, OB);
BOOL TUPINT664658198(TUPINTINT, TUPINTINT);
BOOL TUPSIG1163511586(TUPSIGSTR, TUPSIGSTR);
BOOL TUPSIG116886773(TUPSIGSTR, OB);
BOOL TUPSIG1376799041(TUPSIG2023533247, TUPSIG2023533247);
BOOL TUPSIG1705966004(TUPSIG1754078736, OB);
BOOL TUPSIG174310016(TUPSIG2023533247, OB);
BOOL TUPSIG214440361(TUPSIGARG_LAYOUT, TUPSIGARG_LAYOUT);
BOOL TUPSIG215029739(TUPSIGINT, OB);
BOOL TUPSIG285591084(TUPSIG1754078736, TUPSIG1754078736);
BOOL TUPSIG662732057(TUPSIGSIG, OB);
BOOL TUPSIG712906526(TUPSIG578692189, TUPSIG578692189);
BOOL TUPSIG738670088(TUPSIGARG_LAYOUT, OB);
BOOL TUPSIG848243115(TUPSIG578692189, OB);
BOOL TUPSIG862384662(TUPSIGINT, TUPSIGINT);
BOOL TUPSIG975230438(TUPSIGSIG, TUPSIGSIG);
BOOL TUPSTR1114355858(TUPSTRINT, TUPSTRINT);
BOOL TUPSTR1342996338(TUPSTRSIG, TUPSTRSIG);
BOOL TUPSTR1375565929(TUPSTRSTR, OB);
BOOL TUPSTR1473708895(TUPSTRINT, OB);
BOOL TUPSTR1512680764(TUPSTRdCONFIG, TUPSTRdCONFIG);
BOOL TUPSTR1795207828(TUPSTRdCONFIG, OB);
BOOL TUPSTR1918897134(TUPSTRFSETSTR, TUPSTRFSETSTR);
BOOL TUPSTR1921411213(TUPSTRSIG, OB);
BOOL TUPSTR813228934(TUPSTRSTR, TUPSTRSTR);
BOOL TUPSTR850883487(TUPSTRFSETSTR, OB);
BOOL TUPTP_1851838836(TUPTP_1668454172, TUPTP_1668454172);
BOOL TUPTP_1994246727(TUPTP_858321267, TUPTP_858321267);
BOOL TUPTP_2145724346(TUPTP_858321267, OB);
BOOL TUPTP_744064396(TUPTP_1668454172, OB);
BOOL TUPdAM1479867(TUPdAM_EXPRdTP, OB);
BOOL TUPdAM385273681(TUPdAM_EXPRdTP, TUPdAM_EXPRdTP);
BOOL TUPdAS1082207056(TUPdAS268679779, OB);
BOOL TUPdAS298340389(TUPdAS268679779, TUPdAS268679779);
BOOL TUPdOB134201346(TUPdOBNAMESPACE, OB);
BOOL TUPdOB1353477744(TUPdOBNAMESPACE, TUPdOBNAMESPACE);
BOOL TUPdOB1661309186(TUPdOBSTR, TUPdOBSTR);
BOOL TUPdOB398929495(TUPdOBSTR, OB);
BOOL TUPdTP1282296289(TUPdTP1710233051, OB);
BOOL TUPdTP1433052028(TUPdTPINT, OB);
BOOL TUPdTP1504236332(TUPdTPCODE_FILE, TUPdTPCODE_FILE);
BOOL TUPdTP1901219094(TUPdTP1710233051, TUPdTP1710233051);
BOOL TUPdTP1932220599(TUPdTPdLAYOUT, OB);
BOOL TUPdTP1961471532(TUPdTPCODE_FILE, OB);
BOOL TUPdTP2063305148(TUPdTPINT, TUPdTPINT);
BOOL TUPdTP562610662(TUPdTPdLAYOUT, TUPdTPdLAYOUT);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INTI_s96356965(INTI1, FSTR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT ELT_HA1612720024(ELT_HASH, OB);
INT INTI_c675035669(INTI1, INTI1);
INT TUPAM_1208907315(TUPAM_927437481);
INT TUPAM_1395325321(TUPAM_504653531);
INT TUPAM_247029134(TUPAM_1519040877);
INT TUPAM_519070420(TUPAM_653469574);
INT TUPAM_755257033(TUPAM_1024994801);
INT TUPARR1458356743(TUPARRAYARGdTP);
INT TUPARR826863757(TUPARR1759987776);
INT TUPBOO1866184529(TUPBOO1392923241);
INT TUPIDE1936372555(TUPIDENTdTP);
INT TUPIDE274165750(TUPIDENTARRAYdTP);
INT TUPIDE280438974(TUPIDENTINT);
INT TUPIDE88035988(TUPIDE641654124);
INT TUPINT283365700(TUPINTINT);
INT TUPSIG1080413784(TUPSIG578692189);
INT TUPSIG1100158695(TUPSIGARG_LAYOUT);
INT TUPSIG1160082702(TUPSIGINT);
INT TUPSIG1486928152(TUPSIGSTR);
INT TUPSIG1584335572(TUPSIGSIG);
INT TUPSIG2024347575(TUPSIG2023533247);
INT TUPSIG524761103(TUPSIG1754078736);
INT TUPSTR1611378322(TUPSTRFSETSTR);
INT TUPSTR737321694(TUPSTRINT);
INT TUPSTR813227328(TUPSTRSIG);
INT TUPSTR864810129(TUPSTRdCONFIG);
INT TUPSTR910634748(TUPSTRSTR);
INT TUPTP_108406739(TUPTP_858321267);
INT TUPTP_385216441(TUPTP_1668454172);
INT TUPdAM1873776308(TUPdAM_EXPRdTP);
INT TUPdAS472200087(TUPdAS268679779);
INT TUPdOB400520666(TUPdOBSTR);
INT TUPdOB687982527(TUPdOBNAMESPACE);
INT TUPdTP1209415271(TUPdTPINT);
INT TUPdTP1263366620(TUPdTP1710233051);
INT TUPdTP1991179276(TUPdTPdLAYOUT);
INT TUPdTP386219079(TUPdTPCODE_FILE);
STR ARRAYARG_strrSTR(ARRAYARG);
STR ARRAYB1997509424(ARRAYBOOL);
STR ARRAYdTP_strrSTR(ARRAYdTP);
STR BOOL_strrSTR(BOOL);
STR FLISTA1884067430(FLISTA1062334999);
STR FLISTA389202533(FLISTA1409846210);
STR FMAPID1895267434(FMAPID739086519);
STR STR_fr1097270334(STR, FSTR);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TUPAM_1694719258(TUPAM_1519040877);
STR TUPAM_489161612(TUPAM_927437481);
STR TUPAM_504028066(TUPAM_504653531);
STR TUPAM_525468809(TUPAM_653469574);
STR TUPAM_85614060(TUPAM_1024994801);
STR TUPARR1404914248(TUPARRAYARGdTP);
STR TUPARR735320083(TUPARR1759987776);
STR TUPBOO1584822636(TUPBOO1392923241);
STR TUPIDE1477036401(TUPIDE641654124);
STR TUPIDE1923523563(TUPIDENTARRAYdTP);
STR TUPIDE2057908249(TUPIDENTdTP);
STR TUPIDE2120384756(TUPIDENTINT);
STR TUPINT567702316(TUPINTINT);
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
STR TUPdAM1835151881(TUPdAM_EXPRdTP);
STR TUPdAS100511776(TUPdAS268679779);
STR TUPdOB1460589826(TUPdOBSTR);
STR TUPdOB1872884181(TUPdOBNAMESPACE);
STR TUPdTP1335739968(TUPdTPdLAYOUT);
STR TUPdTP1477319287(TUPdTPCODE_FILE);
STR TUPdTP1972747229(TUPdTPINT);
STR TUPdTP2068651704(TUPdTP1710233051);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1144039098(TUPAM_1024994801 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_1024994801 L1;
 if (arg==NULL) {
  goto other192;
 } else
 switch (TAG(arg)) {
  case TUPAM_1024994801_tag:
   L1 = ((TUPAM_1024994801_boxed) arg)->immutable_part;
   ret_val = TUPAM_1508779545(self, L1);
   return ret_val; break;
  default: ;
  other192: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1196406350(TUPAM_504653531 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_504653531 L1;
 if (arg==NULL) {
  goto other191;
 } else
 switch (TAG(arg)) {
  case TUPAM_504653531_tag:
   L1 = ((TUPAM_504653531_boxed) arg)->immutable_part;
   ret_val = TUPAM_1827433739(self, L1);
   return ret_val; break;
  default: ;
  other191: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1508779545(TUPAM_1024994801 self, TUPAM_1024994801 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_1024994801 elt_eq_self = TUPAM_1024994801_zero;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPAM_1024994801 elt_eq2_self = TUPAM_1024994801_zero;
 FLISTA1409846210 elt_eq2_e1;
 FLISTA1409846210 elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQ997295515 elt_eq_self2;
 FLISTA1409846210 elt_eq_e12;
 FLISTA1409846210 elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ702804563) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = (elt_eq_e11==elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ997295515) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1557192709(TUPAM_1519040877 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_1519040877 L1;
 if (arg==NULL) {
  goto other190;
 } else
 switch (TAG(arg)) {
  case TUPAM_1519040877_tag:
   L1 = ((TUPAM_1519040877_boxed) arg)->immutable_part;
   ret_val = TUPAM_1677695562(self, L1);
   return ret_val; break;
  default: ;
  other190: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1677695562(TUPAM_1519040877 self, TUPAM_1519040877 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_1519040877 elt_eq_self = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 TUPAM_1519040877 elt_eq2_self = TUPAM_1519040877_zero;
 AM_LOCAL_EXPR elt_eq2_e1;
 AM_LOCAL_EXPR elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self2;
 AM_LOCAL_EXPR elt_eq_e12;
 AM_LOCAL_EXPR elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val6 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ1704040611) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_a = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1704040611) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  is_eq_self1 = elt_eq_e12;
  is_eq_a1 = ((OB) elt_eq_e22);
  ret_val6 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
  ret_val5 = ret_val6;
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1711472971(TUPAM_653469574 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_653469574 L1;
 if (arg==NULL) {
  goto other193;
 } else
 switch (TAG(arg)) {
  case TUPAM_653469574_tag:
   L1 = ((TUPAM_653469574_boxed) arg)->immutable_part;
   ret_val = TUPAM_1888140103(self, L1);
   return ret_val; break;
  default: ;
  other193: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1783991982(TUPAM_927437481 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPAM_927437481 L1;
 if (arg==NULL) {
  goto other189;
 } else
 switch (TAG(arg)) {
  case TUPAM_927437481_tag:
   L1 = ((TUPAM_927437481_boxed) arg)->immutable_part;
   ret_val = TUPAM_499588693(self, L1);
   return ret_val; break;
  default: ;
  other189: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1827433739(TUPAM_504653531 self, TUPAM_504653531 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_504653531 elt_eq_self = TUPAM_504653531_zero;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 TUPAM_504653531 elt_eq2_self = TUPAM_504653531_zero;
 OPT_LOCAL_EXPR elt_eq2_e1;
 OPT_LOCAL_EXPR elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1284625691 elt_eq_self2;
 OPT_LOCAL_EXPR elt_eq_e12;
 OPT_LOCAL_EXPR elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ1704040611) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_a = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1284625691) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_1888140103(TUPAM_653469574 self, TUPAM_653469574 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_653469574 elt_eq_self = TUPAM_653469574_zero;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPAM_653469574 elt_eq2_self = TUPAM_653469574_zero;
 FLISTA1062334999 elt_eq2_e1;
 FLISTA1062334999 elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQ1504893818 elt_eq_self2;
 FLISTA1062334999 elt_eq_e12;
 FLISTA1062334999 elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQ702804563) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = (elt_eq_e11==elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1504893818) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPAM_499588693(TUPAM_927437481 self, TUPAM_927437481 e) {
 BOOL ret_val = BOOL_zero;
 TUPAM_927437481 elt_eq_self = TUPAM_927437481_zero;
 AM_CURSOR elt_eq_e1;
 AM_CURSOR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQAM_CURSOR elt_eq_self1;
 AM_CURSOR elt_eq_e11;
 AM_CURSOR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPAM_927437481 elt_eq2_self = TUPAM_927437481_zero;
 dAM_EXPR elt_eq2_e1;
 dAM_EXPR elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdAM_EXPR elt_eq_self2;
 dAM_EXPR elt_eq_e12;
 dAM_EXPR elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 TUPAM_927437481 elt_eq3_self = TUPAM_927437481_zero;
 dAM_STMT elt_eq3_e1;
 dAM_STMT elt_eq3_e2;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 dAM_EXPR L3;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQAM_CURSOR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = AM_CUR1885691330(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQdAM_EXPR) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L3 = elt_eq_e12;
  ret_val4 = (*dAM_EX2004971809[TAG(L3)])(L3, ((OB) elt_eq_e22));
  ret_val3 = ret_val4;
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   elt_eq3_self = self;
   elt_eq3_e1 = self.t3;
   elt_eq3_e2 = e.t3;
   ret_val5 = ELT_EQ2067825030(((ELT_EQdAM_STMT) NULL), elt_eq3_e1, elt_eq3_e2);
   L6 = ret_val5;
   L71_=!(L6); 
   if (L71_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPARR1107471104(TUPARRAYARGdTP self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPARRAYARGdTP L1;
 if (arg==NULL) {
  goto other194;
 } else
 switch (TAG(arg)) {
  case TUPARRAYARGdTP_tag:
   L1 = ((TUPARRAYARGdTP_boxed) arg)->immutable_part;
   ret_val = TUPARR2002873565(self, L1);
   return ret_val; break;
  default: ;
  other194: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPARR1838121920(TUPARR1759987776 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPARR1759987776 L1;
 if (arg==NULL) {
  goto other195;
 } else
 switch (TAG(arg)) {
  case TUPARR1759987776_tag:
   L1 = ((TUPARR1759987776_boxed) arg)->immutable_part;
   ret_val = TUPARR247873404(self, L1);
   return ret_val; break;
  default: ;
  other195: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPARR2002873565(TUPARRAYARGdTP self, TUPARRAYARGdTP e) {
 BOOL ret_val = BOOL_zero;
 TUPARRAYARGdTP elt_eq_self = TUPARRAYARGdTP_zero;
 ARRAYARG elt_eq_e1;
 ARRAYARG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQARRAYARG elt_eq_self1;
 ARRAYARG elt_eq_e11;
 ARRAYARG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPARRAYARGdTP elt_eq2_self = TUPARRAYARGdTP_zero;
 dTP elt_eq2_e1;
 dTP elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self2;
 dTP elt_eq_e12;
 dTP elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQARRAYARG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = (elt_eq_e11==elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQdTP) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L3 = elt_eq_e12;
  ret_val4 = (*dTP_is242312711[TAG(L3)])(L3, ((OB) elt_eq_e22));
  ret_val3 = ret_val4;
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPARR247873404(TUPARR1759987776 self, TUPARR1759987776 e) {
 BOOL ret_val = BOOL_zero;
 TUPARR1759987776 elt_eq_self = TUPARR1759987776_zero;
 ARRAYARG elt_eq_e1;
 ARRAYARG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQARRAYARG elt_eq_self1;
 ARRAYARG elt_eq_e11;
 ARRAYARG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPARR1759987776 elt_eq2_self = TUPARR1759987776_zero;
 ARRAYBOOL elt_eq2_e1;
 ARRAYBOOL elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQARRAYBOOL elt_eq_self2;
 ARRAYBOOL elt_eq_e12;
 ARRAYBOOL elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 TUPARR1759987776 elt_eq3_self = TUPARR1759987776_zero;
 dTP elt_eq3_e1;
 dTP elt_eq3_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQdTP elt_eq_self3;
 dTP elt_eq_e13;
 dTP elt_eq_e23;
 BOOL ret_val6 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 dTP L5;
 BOOL L6;
 BOOL L71_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQARRAYARG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = (elt_eq_e11==elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQARRAYBOOL) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   elt_eq3_self = self;
   elt_eq3_e1 = self.t3;
   elt_eq3_e2 = e.t3;
   elt_eq_self3 = ((ELT_EQdTP) NULL);
   elt_eq_e13 = elt_eq3_e1;
   elt_eq_e23 = elt_eq3_e2;
   L5 = elt_eq_e13;
   ret_val6 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) elt_eq_e23));
   ret_val5 = ret_val6;
   L6 = ret_val5;
   L71_=!(L6); 
   if (L71_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPBOO1364843103(TUPBOO1392923241 self, TUPBOO1392923241 e) {
 BOOL ret_val = BOOL_zero;
 TUPBOO1392923241 elt_eq_self = TUPBOO1392923241_zero;
 BOOL elt_eq_e1 = BOOL_zero;
 BOOL elt_eq_e2 = BOOL_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQBOOL elt_eq_self1;
 BOOL elt_eq_e11 = BOOL_zero;
 BOOL elt_eq_e21 = BOOL_zero;
 BOOL ret_val2 = BOOL_zero;
 TUPBOO1392923241 elt_eq2_self = TUPBOO1392923241_zero;
 INTI1 elt_eq2_e1;
 INTI1 elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQINTI elt_eq_self2;
 INTI1 elt_eq_e12;
 INTI1 elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val5 = BOOL_zero;
 TUPBOO1392923241 elt_eq3_self = TUPBOO1392923241_zero;
 INT elt_eq3_e1 = INT_zero;
 INT elt_eq3_e2 = INT_zero;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQINT elt_eq_self3;
 INT elt_eq_e13 = INT_zero;
 INT elt_eq_e23 = INT_zero;
 BOOL ret_val7 = BOOL_zero;
 TUPBOO1392923241 elt_eq4_self = TUPBOO1392923241_zero;
 BOOL elt_eq4_e1 = BOOL_zero;
 BOOL elt_eq4_e2 = BOOL_zero;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQBOOL elt_eq_self4;
 BOOL elt_eq_e14 = BOOL_zero;
 BOOL elt_eq_e24 = BOOL_zero;
 BOOL ret_val9 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQBOOL) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L11_=elt_eq_e11==elt_eq_e21; 
 ret_val2 = L11_;
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINTI) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  is_eq_self = elt_eq_e12;
  is_eq_y = elt_eq_e22;
  if ((is_eq_self==is_eq_y)) {
   L4 = TRUE;
  } else {
   L5 = INTI_c675035669(is_eq_self, is_eq_y);
   L61_=(L5)==(0); 
   L4 = L61_;
  }
  ret_val5 = L4;
  ret_val4 = ret_val5;
  ret_val3 = ret_val4;
  L7 = ret_val3;
  L81_=!(L7); 
  if (L81_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   elt_eq3_self = self;
   elt_eq3_e1 = self.t3;
   elt_eq3_e2 = e.t3;
   elt_eq_self3 = ((ELT_EQINT) NULL);
   elt_eq_e13 = elt_eq3_e1;
   elt_eq_e23 = elt_eq3_e2;
   L91_=(elt_eq_e13)==(elt_eq_e23); 
   ret_val7 = L91_;
   ret_val6 = ret_val7;
   L10 = ret_val6;
   L121_=!(L10); 
   if (L121_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    elt_eq4_self = self;
    elt_eq4_e1 = self.t4;
    elt_eq4_e2 = e.t4;
    elt_eq_self4 = ((ELT_EQBOOL) NULL);
    elt_eq_e14 = elt_eq4_e1;
    elt_eq_e24 = elt_eq4_e2;
    L131_=elt_eq_e14==elt_eq_e24; 
    ret_val9 = L131_;
    ret_val8 = ret_val9;
    L14 = ret_val8;
    L151_=!(L14); 
    if (L151_) {
     ret_val = FALSE;
     return ret_val;
    }
    else {
     ret_val = TRUE;
     return ret_val;
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPBOO323223880(TUPBOO1392923241 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPBOO1392923241 L1;
 if (arg==NULL) {
  goto other196;
 } else
 switch (TAG(arg)) {
  case TUPBOO1392923241_tag:
   L1 = ((TUPBOO1392923241_boxed) arg)->immutable_part;
   ret_val = TUPBOO1364843103(self, L1);
   return ret_val; break;
  default: ;
  other196: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE1046275513(TUPIDE641654124 self, TUPIDE641654124 e) {
 BOOL ret_val = BOOL_zero;
 TUPIDE641654124 elt_eq_self = TUPIDE641654124_zero;
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
 TUPIDE641654124 elt_eq2_self = TUPIDE641654124_zero;
 AM_SHARED_EXPR elt_eq2_e1;
 AM_SHARED_EXPR elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1304198768 elt_eq_self2;
 AM_SHARED_EXPR elt_eq_e12;
 AM_SHARED_EXPR elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQIDENT) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_i = elt_eq_e21;
 ret_val3 = (is_eq_self.str==is_eq_i.str);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1304198768) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = AM_SHA623484008(elt_eq_e12, ((OB) elt_eq_e22));
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE1215794628(TUPIDENTdTP self, TUPIDENTdTP e) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTdTP elt_eq_self = TUPIDENTdTP_zero;
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
 TUPIDENTdTP elt_eq2_self = TUPIDENTdTP_zero;
 dTP elt_eq2_e1;
 dTP elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQdTP elt_eq_self2;
 dTP elt_eq_e12;
 dTP elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQIDENT) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_i = elt_eq_e21;
 ret_val3 = (is_eq_self.str==is_eq_i.str);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQdTP) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L3 = elt_eq_e12;
  ret_val5 = (*dTP_is242312711[TAG(L3)])(L3, ((OB) elt_eq_e22));
  ret_val4 = ret_val5;
  L4 = ret_val4;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE1222548283(TUPIDE641654124 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPIDE641654124 L1;
 if (arg==NULL) {
  goto other198;
 } else
 switch (TAG(arg)) {
  case TUPIDE641654124_tag:
   L1 = ((TUPIDE641654124_boxed) arg)->immutable_part;
   ret_val = TUPIDE1046275513(self, L1);
   return ret_val; break;
  default: ;
  other198: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE1439069590(TUPIDENTINT self, TUPIDENTINT e) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTINT elt_eq_self = TUPIDENTINT_zero;
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
 TUPIDENTINT elt_eq2_self = TUPIDENTINT_zero;
 INT elt_eq2_e1 = INT_zero;
 INT elt_eq2_e2 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQINT elt_eq_self2;
 INT elt_eq_e12 = INT_zero;
 INT elt_eq_e22 = INT_zero;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQIDENT) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_i = elt_eq_e21;
 ret_val3 = (is_eq_self.str==is_eq_i.str);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L31_=(elt_eq_e12)==(elt_eq_e22); 
  ret_val5 = L31_;
  ret_val4 = ret_val5;
  L4 = ret_val4;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE1955445151(TUPIDENTARRAYdTP self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTARRAYdTP L1;
 if (arg==NULL) {
  goto other199;
 } else
 switch (TAG(arg)) {
  case TUPIDENTARRAYdTP_tag:
   L1 = ((TUPIDENTARRAYdTP_boxed) arg)->immutable_part;
   ret_val = TUPIDE324380005(self, L1);
   return ret_val; break;
  default: ;
  other199: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE324380005(TUPIDENTARRAYdTP self, TUPIDENTARRAYdTP e) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTARRAYdTP elt_eq_self = TUPIDENTARRAYdTP_zero;
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
 TUPIDENTARRAYdTP elt_eq2_self = TUPIDENTARRAYdTP_zero;
 ARRAYdTP elt_eq2_e1;
 ARRAYdTP elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQARRAYdTP elt_eq_self2;
 ARRAYdTP elt_eq_e12;
 ARRAYdTP elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQIDENT) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_i = elt_eq_e21;
 ret_val3 = (is_eq_self.str==is_eq_i.str);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQARRAYdTP) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE429159832(TUPIDENTdTP self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTdTP L1;
 if (arg==NULL) {
  goto other197;
 } else
 switch (TAG(arg)) {
  case TUPIDENTdTP_tag:
   L1 = ((TUPIDENTdTP_boxed) arg)->immutable_part;
   ret_val = TUPIDE1215794628(self, L1);
   return ret_val; break;
  default: ;
  other197: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPIDE53463463(TUPIDENTINT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPIDENTINT L1;
 if (arg==NULL) {
  goto other200;
 } else
 switch (TAG(arg)) {
  case TUPIDENTINT_tag:
   L1 = ((TUPIDENTINT_boxed) arg)->immutable_part;
   ret_val = TUPIDE1439069590(self, L1);
   return ret_val; break;
  default: ;
  other200: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPINT326447217(TUPINTINT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPINTINT L1;
 if (arg==NULL) {
  goto other201;
 } else
 switch (TAG(arg)) {
  case TUPINTINT_tag:
   L1 = ((TUPINTINT_boxed) arg)->immutable_part;
   ret_val = TUPINT664658198(self, L1);
   return ret_val; break;
  default: ;
  other201: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPINT664658198(TUPINTINT self, TUPINTINT e) {
 BOOL ret_val = BOOL_zero;
 TUPINTINT elt_eq_self = TUPINTINT_zero;
 INT elt_eq_e1 = INT_zero;
 INT elt_eq_e2 = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQINT elt_eq_self1;
 INT elt_eq_e11 = INT_zero;
 INT elt_eq_e21 = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 TUPINTINT elt_eq2_self = TUPINTINT_zero;
 INT elt_eq2_e1 = INT_zero;
 INT elt_eq2_e2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQINT elt_eq_self2;
 INT elt_eq_e12 = INT_zero;
 INT elt_eq_e22 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQINT) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L11_=(elt_eq_e11)==(elt_eq_e21); 
 ret_val2 = L11_;
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L41_=(elt_eq_e12)==(elt_eq_e22); 
  ret_val4 = L41_;
  ret_val3 = ret_val4;
  L5 = ret_val3;
  L61_=!(L5); 
  if (L61_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG1163511586(TUPSIGSTR self, TUPSIGSTR e) {
 BOOL ret_val = BOOL_zero;
 TUPSIGSTR elt_eq_self = TUPSIGSTR_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIGSTR elt_eq2_self = TUPSIGSTR_zero;
 STR elt_eq2_e1;
 STR elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self2;
 STR elt_eq_e12;
 STR elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQSTR) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = STR_is111530248(elt_eq_e12, elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG116886773(TUPSIGSTR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIGSTR L1;
 if (arg==NULL) {
  goto other208;
 } else
 switch (TAG(arg)) {
  case TUPSIGSTR_tag:
   L1 = ((TUPSIGSTR_boxed) arg)->immutable_part;
   ret_val = TUPSIG1163511586(self, L1);
   return ret_val; break;
  default: ;
  other208: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG1376799041(TUPSIG2023533247 self, TUPSIG2023533247 e) {
 BOOL ret_val = BOOL_zero;
 TUPSIG2023533247 elt_eq_self = TUPSIG2023533247_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIG2023533247 elt_eq2_self = TUPSIG2023533247_zero;
 FRAME_LAYOUT elt_eq2_e1;
 FRAME_LAYOUT elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ602167759 elt_eq_self2;
 FRAME_LAYOUT elt_eq_e12;
 FRAME_LAYOUT elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ602167759) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG1705966004(TUPSIG1754078736 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIG1754078736 L1;
 if (arg==NULL) {
  goto other202;
 } else
 switch (TAG(arg)) {
  case TUPSIG1754078736_tag:
   L1 = ((TUPSIG1754078736_boxed) arg)->immutable_part;
   ret_val = TUPSIG285591084(self, L1);
   return ret_val; break;
  default: ;
  other202: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG174310016(TUPSIG2023533247 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIG2023533247 L1;
 if (arg==NULL) {
  goto other205;
 } else
 switch (TAG(arg)) {
  case TUPSIG2023533247_tag:
   L1 = ((TUPSIG2023533247_boxed) arg)->immutable_part;
   ret_val = TUPSIG1376799041(self, L1);
   return ret_val; break;
  default: ;
  other205: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG214440361(TUPSIGARG_LAYOUT self, TUPSIGARG_LAYOUT e) {
 BOOL ret_val = BOOL_zero;
 TUPSIGARG_LAYOUT elt_eq_self = TUPSIGARG_LAYOUT_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIGARG_LAYOUT elt_eq2_self = TUPSIGARG_LAYOUT_zero;
 ARG_LAYOUT elt_eq2_e1;
 ARG_LAYOUT elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQARG_LAYOUT elt_eq_self2;
 ARG_LAYOUT elt_eq_e12;
 ARG_LAYOUT elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQARG_LAYOUT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG215029739(TUPSIGINT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIGINT L1;
 if (arg==NULL) {
  goto other206;
 } else
 switch (TAG(arg)) {
  case TUPSIGINT_tag:
   L1 = ((TUPSIGINT_boxed) arg)->immutable_part;
   ret_val = TUPSIG862384662(self, L1);
   return ret_val; break;
  default: ;
  other206: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG285591084(TUPSIG1754078736 self, TUPSIG1754078736 e) {
 BOOL ret_val = BOOL_zero;
 TUPSIG1754078736 elt_eq_self = TUPSIG1754078736_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIG1754078736 elt_eq2_self = TUPSIG1754078736_zero;
 ABSTRA151498155 elt_eq2_e1;
 ABSTRA151498155 elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1002048960 elt_eq_self2;
 ABSTRA151498155 elt_eq_e12;
 ABSTRA151498155 elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1002048960) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG662732057(TUPSIGSIG self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIGSIG L1;
 if (arg==NULL) {
  goto other207;
 } else
 switch (TAG(arg)) {
  case TUPSIGSIG_tag:
   L1 = ((TUPSIGSIG_boxed) arg)->immutable_part;
   ret_val = TUPSIG975230438(self, L1);
   return ret_val; break;
  default: ;
  other207: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG712906526(TUPSIG578692189 self, TUPSIG578692189 e) {
 BOOL ret_val = BOOL_zero;
 TUPSIG578692189 elt_eq_self = TUPSIG578692189_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIG578692189 elt_eq2_self = TUPSIG578692189_zero;
 AM_ROUT_DEF elt_eq2_e1;
 AM_ROUT_DEF elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self2;
 AM_ROUT_DEF elt_eq_e12;
 AM_ROUT_DEF elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ702804563) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG738670088(TUPSIGARG_LAYOUT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIGARG_LAYOUT L1;
 if (arg==NULL) {
  goto other204;
 } else
 switch (TAG(arg)) {
  case TUPSIGARG_LAYOUT_tag:
   L1 = ((TUPSIGARG_LAYOUT_boxed) arg)->immutable_part;
   ret_val = TUPSIG214440361(self, L1);
   return ret_val; break;
  default: ;
  other204: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG848243115(TUPSIG578692189 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSIG578692189 L1;
 if (arg==NULL) {
  goto other203;
 } else
 switch (TAG(arg)) {
  case TUPSIG578692189_tag:
   L1 = ((TUPSIG578692189_boxed) arg)->immutable_part;
   ret_val = TUPSIG712906526(self, L1);
   return ret_val; break;
  default: ;
  other203: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG862384662(TUPSIGINT self, TUPSIGINT e) {
 BOOL ret_val = BOOL_zero;
 TUPSIGINT elt_eq_self = TUPSIGINT_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIGINT elt_eq2_self = TUPSIGINT_zero;
 INT elt_eq2_e1 = INT_zero;
 INT elt_eq2_e2 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQINT elt_eq_self2;
 INT elt_eq_e12 = INT_zero;
 INT elt_eq_e22 = INT_zero;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L31_=(elt_eq_e12)==(elt_eq_e22); 
  ret_val5 = L31_;
  ret_val4 = ret_val5;
  L4 = ret_val4;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSIG975230438(TUPSIGSIG self, TUPSIGSIG e) {
 BOOL ret_val = BOOL_zero;
 TUPSIGSIG elt_eq_self = TUPSIGSIG_zero;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val3 = BOOL_zero;
 TUPSIGSIG elt_eq2_self = TUPSIGSIG_zero;
 SIG elt_eq2_e1;
 SIG elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self2;
 SIG elt_eq_e12;
 SIG elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val6 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSIG) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_s = elt_eq_e21;
 ret_val3 = (is_eq_self==is_eq_s);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQSIG) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  is_eq_self1 = elt_eq_e12;
  is_eq_s1 = elt_eq_e22;
  ret_val6 = (is_eq_self1==is_eq_s1);
  ret_val5 = ret_val6;
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1114355858(TUPSTRINT self, TUPSTRINT e) {
 BOOL ret_val = BOOL_zero;
 TUPSTRINT elt_eq_self = TUPSTRINT_zero;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPSTRINT elt_eq2_self = TUPSTRINT_zero;
 INT elt_eq2_e1 = INT_zero;
 INT elt_eq2_e2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQINT elt_eq_self2;
 INT elt_eq_e12 = INT_zero;
 INT elt_eq_e22 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSTR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L31_=(elt_eq_e12)==(elt_eq_e22); 
  ret_val4 = L31_;
  ret_val3 = ret_val4;
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1342996338(TUPSTRSIG self, TUPSTRSIG e) {
 BOOL ret_val = BOOL_zero;
 TUPSTRSIG elt_eq_self = TUPSTRSIG_zero;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPSTRSIG elt_eq2_self = TUPSTRSIG_zero;
 SIG elt_eq2_e1;
 SIG elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQSIG elt_eq_self2;
 SIG elt_eq_e12;
 SIG elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSTR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQSIG) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  is_eq_self = elt_eq_e12;
  is_eq_s = elt_eq_e22;
  ret_val5 = (is_eq_self==is_eq_s);
  ret_val4 = ret_val5;
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1375565929(TUPSTRSTR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSTRSTR L1;
 if (arg==NULL) {
  goto other214;
 } else
 switch (TAG(arg)) {
  case TUPSTRSTR_tag:
   L1 = ((TUPSTRSTR_boxed) arg)->immutable_part;
   ret_val = TUPSTR813228934(self, L1);
   return ret_val; break;
  default: ;
  other214: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1473708895(TUPSTRINT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSTRINT L1;
 if (arg==NULL) {
  goto other212;
 } else
 switch (TAG(arg)) {
  case TUPSTRINT_tag:
   L1 = ((TUPSTRINT_boxed) arg)->immutable_part;
   ret_val = TUPSTR1114355858(self, L1);
   return ret_val; break;
  default: ;
  other212: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1512680764(TUPSTRdCONFIG self, TUPSTRdCONFIG e) {
 BOOL ret_val = BOOL_zero;
 TUPSTRdCONFIG elt_eq_self = TUPSTRdCONFIG_zero;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPSTRdCONFIG elt_eq2_self = TUPSTRdCONFIG_zero;
 dCONFIG elt_eq2_e1;
 dCONFIG elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSTR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  ret_val3 = ELT_EQ1977650129(((ELT_EQdCONFIG) NULL), elt_eq2_e1, elt_eq2_e2);
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1795207828(TUPSTRdCONFIG self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSTRdCONFIG L1;
 if (arg==NULL) {
  goto other210;
 } else
 switch (TAG(arg)) {
  case TUPSTRdCONFIG_tag:
   L1 = ((TUPSTRdCONFIG_boxed) arg)->immutable_part;
   ret_val = TUPSTR1512680764(self, L1);
   return ret_val; break;
  default: ;
  other210: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1918897134(TUPSTRFSETSTR self, TUPSTRFSETSTR e) {
 BOOL ret_val = BOOL_zero;
 TUPSTRFSETSTR elt_eq_self = TUPSTRFSETSTR_zero;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPSTRFSETSTR elt_eq2_self = TUPSTRFSETSTR_zero;
 FSETSTR elt_eq2_e1;
 FSETSTR elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQFSETSTR elt_eq_self2;
 FSETSTR elt_eq_e12;
 FSETSTR elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSTR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQFSETSTR) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR1921411213(TUPSTRSIG self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSTRSIG L1;
 if (arg==NULL) {
  goto other213;
 } else
 switch (TAG(arg)) {
  case TUPSTRSIG_tag:
   L1 = ((TUPSTRSIG_boxed) arg)->immutable_part;
   ret_val = TUPSTR1342996338(self, L1);
   return ret_val; break;
  default: ;
  other213: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR813228934(TUPSTRSTR self, TUPSTRSTR e) {
 BOOL ret_val = BOOL_zero;
 TUPSTRSTR elt_eq_self = TUPSTRSTR_zero;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPSTRSTR elt_eq2_self = TUPSTRSTR_zero;
 STR elt_eq2_e1;
 STR elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQSTR elt_eq_self2;
 STR elt_eq_e12;
 STR elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQSTR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQSTR) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = STR_is111530248(elt_eq_e12, elt_eq_e22);
  ret_val3 = ret_val4;
  L3 = ret_val3;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPSTR850883487(TUPSTRFSETSTR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPSTRFSETSTR L1;
 if (arg==NULL) {
  goto other211;
 } else
 switch (TAG(arg)) {
  case TUPSTRFSETSTR_tag:
   L1 = ((TUPSTRFSETSTR_boxed) arg)->immutable_part;
   ret_val = TUPSTR1918897134(self, L1);
   return ret_val; break;
  default: ;
  other211: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPTP_1851838836(TUPTP_1668454172 self, TUPTP_1668454172 e) {
 BOOL ret_val = BOOL_zero;
 TUPTP_1668454172 elt_eq_self = TUPTP_1668454172_zero;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val3 = BOOL_zero;
 TUPTP_1668454172 elt_eq2_self = TUPTP_1668454172_zero;
 FSETTP_CLASS elt_eq2_e1;
 FSETTP_CLASS elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1354147196 elt_eq_self2;
 FSETTP_CLASS elt_eq_e12;
 FSETTP_CLASS elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_t = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ1354147196) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPTP_1994246727(TUPTP_858321267 self, TUPTP_858321267 e) {
 BOOL ret_val = BOOL_zero;
 TUPTP_858321267 elt_eq_self = TUPTP_858321267_zero;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val3 = BOOL_zero;
 TUPTP_858321267 elt_eq2_self = TUPTP_858321267_zero;
 FSETdTP elt_eq2_e1;
 FSETdTP elt_eq2_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQFSETdTP elt_eq_self2;
 FSETdTP elt_eq_e12;
 FSETdTP elt_eq_e22;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 is_eq_self = elt_eq_e11;
 is_eq_t = ((OB) elt_eq_e21);
 ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 ret_val2 = ret_val3;
 ret_val1 = ret_val2;
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQFSETdTP) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val5 = (elt_eq_e12==elt_eq_e22);
  ret_val4 = ret_val5;
  L3 = ret_val4;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPTP_2145724346(TUPTP_858321267 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPTP_858321267 L1;
 if (arg==NULL) {
  goto other215;
 } else
 switch (TAG(arg)) {
  case TUPTP_858321267_tag:
   L1 = ((TUPTP_858321267_boxed) arg)->immutable_part;
   ret_val = TUPTP_1994246727(self, L1);
   return ret_val; break;
  default: ;
  other215: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPTP_744064396(TUPTP_1668454172 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPTP_1668454172 L1;
 if (arg==NULL) {
  goto other216;
 } else
 switch (TAG(arg)) {
  case TUPTP_1668454172_tag:
   L1 = ((TUPTP_1668454172_boxed) arg)->immutable_part;
   ret_val = TUPTP_1851838836(self, L1);
   return ret_val; break;
  default: ;
  other216: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdAM1479867(TUPdAM_EXPRdTP self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdAM_EXPRdTP L1;
 if (arg==NULL) {
  goto other175;
 } else
 switch (TAG(arg)) {
  case TUPdAM_EXPRdTP_tag:
   L1 = ((TUPdAM_EXPRdTP_boxed) arg)->immutable_part;
   ret_val = TUPdAM385273681(self, L1);
   return ret_val; break;
  default: ;
  other175: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdAM385273681(TUPdAM_EXPRdTP self, TUPdAM_EXPRdTP e) {
 BOOL ret_val = BOOL_zero;
 TUPdAM_EXPRdTP elt_eq_self = TUPdAM_EXPRdTP_zero;
 dAM_EXPR elt_eq_e1;
 dAM_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdAM_EXPR elt_eq_self1;
 dAM_EXPR elt_eq_e11;
 dAM_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPdAM_EXPRdTP elt_eq2_self = TUPdAM_EXPRdTP_zero;
 dTP elt_eq2_e1;
 dTP elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self2;
 dTP elt_eq_e12;
 dTP elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 dAM_EXPR L1;
 BOOL L2;
 BOOL L31_;
 dTP L4;
 BOOL L5;
 BOOL L61_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQdAM_EXPR) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L1 = elt_eq_e11;
 ret_val2 = (*dAM_EX2004971809[TAG(L1)])(L1, ((OB) elt_eq_e21));
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQdTP) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L4 = elt_eq_e12;
  ret_val4 = (*dTP_is242312711[TAG(L4)])(L4, ((OB) elt_eq_e22));
  ret_val3 = ret_val4;
  L5 = ret_val3;
  L61_=!(L5); 
  if (L61_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdAS1082207056(TUPdAS268679779 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdAS268679779 L1;
 if (arg==NULL) {
  goto other177;
 } else
 switch (TAG(arg)) {
  case TUPdAS268679779_tag:
   L1 = ((TUPdAS268679779_boxed) arg)->immutable_part;
   ret_val = TUPdAS298340389(self, L1);
   return ret_val; break;
  default: ;
  other177: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdAS298340389(TUPdAS268679779 self, TUPdAS268679779 e) {
 BOOL ret_val = BOOL_zero;
 TUPdAS268679779 elt_eq_self = TUPdAS268679779_zero;
 dAS_EXPR elt_eq_e1;
 dAS_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 TUPdAS268679779 elt_eq2_self = TUPdAS268679779_zero;
 AS_ARG_MODE elt_eq2_e1;
 AS_ARG_MODE elt_eq2_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQ390028401 elt_eq_self1;
 AS_ARG_MODE elt_eq_e11;
 AS_ARG_MODE elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 ret_val1 = ELT_EQ686719113(((ELT_EQdAS_EXPR) NULL), elt_eq_e1, elt_eq_e2);
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self1 = ((ELT_EQ390028401) NULL);
  elt_eq_e11 = elt_eq2_e1;
  elt_eq_e21 = elt_eq2_e2;
  ret_val3 = (elt_eq_e11==elt_eq_e21);
  ret_val2 = ret_val3;
  L3 = ret_val2;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdOB134201346(TUPdOBNAMESPACE self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdOBNAMESPACE L1;
 if (arg==NULL) {
  goto other179;
 } else
 switch (TAG(arg)) {
  case TUPdOBNAMESPACE_tag:
   L1 = ((TUPdOBNAMESPACE_boxed) arg)->immutable_part;
   ret_val = TUPdOB1353477744(self, L1);
   return ret_val; break;
  default: ;
  other179: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdOB1353477744(TUPdOBNAMESPACE self, TUPdOBNAMESPACE e) {
 BOOL ret_val = BOOL_zero;
 TUPdOBNAMESPACE elt_eq_self = TUPdOBNAMESPACE_zero;
 OB elt_eq_e1;
 OB elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 TUPdOBNAMESPACE elt_eq2_self = TUPdOBNAMESPACE_zero;
 NAMESPACE elt_eq2_e1;
 NAMESPACE elt_eq2_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQNAMESPACE elt_eq_self1;
 NAMESPACE elt_eq_e11;
 NAMESPACE elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 ret_val1 = ELT_EQ936498558(((ELT_EQdOB) NULL), elt_eq_e1, elt_eq_e2);
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self1 = ((ELT_EQNAMESPACE) NULL);
  elt_eq_e11 = elt_eq2_e1;
  elt_eq_e21 = elt_eq2_e2;
  ret_val3 = (elt_eq_e11==elt_eq_e21);
  ret_val2 = ret_val3;
  L3 = ret_val2;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdOB1661309186(TUPdOBSTR self, TUPdOBSTR e) {
 BOOL ret_val = BOOL_zero;
 TUPdOBSTR elt_eq_self = TUPdOBSTR_zero;
 OB elt_eq_e1;
 OB elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 TUPdOBSTR elt_eq2_self = TUPdOBSTR_zero;
 STR elt_eq2_e1;
 STR elt_eq2_e2;
 BOOL ret_val2 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 ret_val1 = ELT_EQ936498558(((ELT_EQdOB) NULL), elt_eq_e1, elt_eq_e2);
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self1 = ((ELT_EQSTR) NULL);
  elt_eq_e11 = elt_eq2_e1;
  elt_eq_e21 = elt_eq2_e2;
  ret_val3 = STR_is111530248(elt_eq_e11, elt_eq_e21);
  ret_val2 = ret_val3;
  L3 = ret_val2;
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdOB398929495(TUPdOBSTR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdOBSTR L1;
 if (arg==NULL) {
  goto other180;
 } else
 switch (TAG(arg)) {
  case TUPdOBSTR_tag:
   L1 = ((TUPdOBSTR_boxed) arg)->immutable_part;
   ret_val = TUPdOB1661309186(self, L1);
   return ret_val; break;
  default: ;
  other180: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1282296289(TUPdTP1710233051 self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdTP1710233051 L1;
 if (arg==NULL) {
  goto other184;
 } else
 switch (TAG(arg)) {
  case TUPdTP1710233051_tag:
   L1 = ((TUPdTP1710233051_boxed) arg)->immutable_part;
   ret_val = TUPdTP1901219094(self, L1);
   return ret_val; break;
  default: ;
  other184: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1433052028(TUPdTPINT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdTPINT L1;
 if (arg==NULL) {
  goto other185;
 } else
 switch (TAG(arg)) {
  case TUPdTPINT_tag:
   L1 = ((TUPdTPINT_boxed) arg)->immutable_part;
   ret_val = TUPdTP2063305148(self, L1);
   return ret_val; break;
  default: ;
  other185: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1504236332(TUPdTPCODE_FILE self, TUPdTPCODE_FILE e) {
 BOOL ret_val = BOOL_zero;
 TUPdTPCODE_FILE elt_eq_self = TUPdTPCODE_FILE_zero;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPdTPCODE_FILE elt_eq2_self = TUPdTPCODE_FILE_zero;
 CODE_FILE elt_eq2_e1;
 CODE_FILE elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQCODE_FILE elt_eq_self2;
 CODE_FILE elt_eq_e12;
 CODE_FILE elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 dTP L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQdTP) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L1 = elt_eq_e11;
 ret_val2 = (*dTP_is242312711[TAG(L1)])(L1, ((OB) elt_eq_e21));
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQCODE_FILE) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1901219094(TUPdTP1710233051 self, TUPdTP1710233051 e) {
 BOOL ret_val = BOOL_zero;
 TUPdTP1710233051 elt_eq_self = TUPdTP1710233051_zero;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPdTP1710233051 elt_eq2_self = TUPdTP1710233051_zero;
 FMAPID739086519 elt_eq2_e1;
 FMAPID739086519 elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQ557769602 elt_eq_self2;
 FMAPID739086519 elt_eq_e12;
 FMAPID739086519 elt_eq_e22;
 BOOL ret_val4 = BOOL_zero;
 dTP L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQdTP) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L1 = elt_eq_e11;
 ret_val2 = (*dTP_is242312711[TAG(L1)])(L1, ((OB) elt_eq_e21));
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQ557769602) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  ret_val4 = (elt_eq_e12==elt_eq_e22);
  ret_val3 = ret_val4;
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1932220599(TUPdTPdLAYOUT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdTPdLAYOUT L1;
 if (arg==NULL) {
  goto other182;
 } else
 switch (TAG(arg)) {
  case TUPdTPdLAYOUT_tag:
   L1 = ((TUPdTPdLAYOUT_boxed) arg)->immutable_part;
   ret_val = TUPdTP562610662(self, L1);
   return ret_val; break;
  default: ;
  other182: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP1961471532(TUPdTPCODE_FILE self, OB arg) {
 BOOL ret_val = BOOL_zero;
 TUPdTPCODE_FILE L1;
 if (arg==NULL) {
  goto other183;
 } else
 switch (TAG(arg)) {
  case TUPdTPCODE_FILE_tag:
   L1 = ((TUPdTPCODE_FILE_boxed) arg)->immutable_part;
   ret_val = TUPdTP1504236332(self, L1);
   return ret_val; break;
  default: ;
  other183: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP2063305148(TUPdTPINT self, TUPdTPINT e) {
 BOOL ret_val = BOOL_zero;
 TUPdTPINT elt_eq_self = TUPdTPINT_zero;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPdTPINT elt_eq2_self = TUPdTPINT_zero;
 INT elt_eq2_e1 = INT_zero;
 INT elt_eq2_e2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQINT elt_eq_self2;
 INT elt_eq_e12 = INT_zero;
 INT elt_eq_e22 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 dTP L1;
 BOOL L2;
 BOOL L31_;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQdTP) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L1 = elt_eq_e11;
 ret_val2 = (*dTP_is242312711[TAG(L1)])(L1, ((OB) elt_eq_e21));
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  elt_eq_self2 = ((ELT_EQINT) NULL);
  elt_eq_e12 = elt_eq2_e1;
  elt_eq_e22 = elt_eq2_e2;
  L41_=(elt_eq_e12)==(elt_eq_e22); 
  ret_val4 = L41_;
  ret_val3 = ret_val4;
  L5 = ret_val3;
  L61_=!(L5); 
  if (L61_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TUPdTP562610662(TUPdTPdLAYOUT self, TUPdTPdLAYOUT e) {
 BOOL ret_val = BOOL_zero;
 TUPdTPdLAYOUT elt_eq_self = TUPdTPdLAYOUT_zero;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 TUPdTPdLAYOUT elt_eq2_self = TUPdTPdLAYOUT_zero;
 dLAYOUT elt_eq2_e1;
 dLAYOUT elt_eq2_e2;
 BOOL ret_val3 = BOOL_zero;
 dTP L1;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 elt_eq_self = self;
 elt_eq_e1 = self.t1;
 elt_eq_e2 = e.t1;
 elt_eq_self1 = ((ELT_EQdTP) NULL);
 elt_eq_e11 = elt_eq_e1;
 elt_eq_e21 = elt_eq_e2;
 L1 = elt_eq_e11;
 ret_val2 = (*dTP_is242312711[TAG(L1)])(L1, ((OB) elt_eq_e21));
 ret_val1 = ret_val2;
 L2 = ret_val1;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  elt_eq2_self = self;
  elt_eq2_e1 = self.t2;
  elt_eq2_e2 = e.t2;
  ret_val3 = ELT_EQ1393525399(((ELT_EQdLAYOUT) NULL), elt_eq2_e1, elt_eq2_e2);
  L4 = ret_val3;
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
  else {
   ret_val = TRUE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_1208907315(TUPAM_927437481 self) {
 INT ret_val = INT_zero;
 INT h1 = INT_zero;
 INT h2 = INT_zero;
 INT h3 = INT_zero;
 TUPAM_927437481 elt_hash_self = TUPAM_927437481_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_927437481 elt_hash2_self = TUPAM_927437481_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 TUPAM_927437481 elt_hash3_self = TUPAM_927437481_zero;
 OB elt_hash3_e;
 INT ret_val3 = INT_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 h1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 h2 = ret_val2;
 elt_hash3_self = self;
 elt_hash3_e = ((OB) self.t3);
 ret_val3 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash3_e);
 h3 = ret_val3;
 L11_=((unsigned)h2)<<((unsigned)2); 
 L2 = L11_;
 L31_=h1^L2; 
 L4 = L31_;
 L51_=((unsigned)h3)<<((unsigned)4); 
 L6 = L51_;
 L71_=L4^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_1395325321(TUPAM_504653531 self) {
 INT ret_val = INT_zero;
 TUPAM_504653531 elt_hash_self = TUPAM_504653531_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_504653531 elt_hash2_self = TUPAM_504653531_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_247029134(TUPAM_1519040877 self) {
 INT ret_val = INT_zero;
 TUPAM_1519040877 elt_hash_self = TUPAM_1519040877_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_1519040877 elt_hash2_self = TUPAM_1519040877_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_519070420(TUPAM_653469574 self) {
 INT ret_val = INT_zero;
 TUPAM_653469574 elt_hash_self = TUPAM_653469574_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_653469574 elt_hash2_self = TUPAM_653469574_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPAM_755257033(TUPAM_1024994801 self) {
 INT ret_val = INT_zero;
 TUPAM_1024994801 elt_hash_self = TUPAM_1024994801_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPAM_1024994801 elt_hash2_self = TUPAM_1024994801_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPARR1458356743(TUPARRAYARGdTP self) {
 INT ret_val = INT_zero;
 TUPARRAYARGdTP elt_hash_self = TUPARRAYARGdTP_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPARRAYARGdTP elt_hash2_self = TUPARRAYARGdTP_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPARR826863757(TUPARR1759987776 self) {
 INT ret_val = INT_zero;
 INT h1 = INT_zero;
 INT h2 = INT_zero;
 INT h3 = INT_zero;
 TUPARR1759987776 elt_hash_self = TUPARR1759987776_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPARR1759987776 elt_hash2_self = TUPARR1759987776_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 TUPARR1759987776 elt_hash3_self = TUPARR1759987776_zero;
 OB elt_hash3_e;
 INT ret_val3 = INT_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 h1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 h2 = ret_val2;
 elt_hash3_self = self;
 elt_hash3_e = ((OB) self.t3);
 ret_val3 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash3_e);
 h3 = ret_val3;
 L11_=((unsigned)h2)<<((unsigned)2); 
 L2 = L11_;
 L31_=h1^L2; 
 L4 = L31_;
 L51_=((unsigned)h3)<<((unsigned)4); 
 L6 = L51_;
 L71_=L4^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPBOO1866184529(TUPBOO1392923241 self) {
 INT ret_val = INT_zero;
 INT h1 = INT_zero;
 INT h2 = INT_zero;
 INT h3 = INT_zero;
 INT h4 = INT_zero;
 TUPBOO1392923241 elt_hash_self = TUPBOO1392923241_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPBOO1392923241 elt_hash2_self = TUPBOO1392923241_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 TUPBOO1392923241 elt_hash3_self = TUPBOO1392923241_zero;
 OB elt_hash3_e;
 INT ret_val3 = INT_zero;
 TUPBOO1392923241 elt_hash4_self = TUPBOO1392923241_zero;
 OB elt_hash4_e;
 INT ret_val4 = INT_zero;
 OB L1;
 OB L2;
 OB L3;
 OB L4;
 OB L5;
 OB L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 elt_hash_self = self;
 L2=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L2)->header.tag=BOOL_tag;
 L1 = (OB)((BOOL_boxed) L2);
 ((BOOL_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 h1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 h2 = ret_val2;
 elt_hash3_self = self;
 L4=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 memset(L4,0,sizeof(struct INT_boxed_struct));
 ((OB)L4)->header.tag=INT_tag;
 L3 = (OB)((INT_boxed) L4);
 ((INT_boxed) L3)->immutable_part = self.t3;
 elt_hash3_e = L3;
 ret_val3 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash3_e);
 h3 = ret_val3;
 elt_hash4_self = self;
 L6=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L6)->header.tag=BOOL_tag;
 L5 = (OB)((BOOL_boxed) L6);
 ((BOOL_boxed) L5)->immutable_part = self.t4;
 elt_hash4_e = L5;
 ret_val4 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash4_e);
 h4 = ret_val4;
 L71_=((unsigned)h2)<<((unsigned)2); 
 L8 = L71_;
 L91_=h1^L8; 
 L10 = L91_;
 L111_=((unsigned)h3)<<((unsigned)4); 
 L12 = L111_;
 L131_=L10^L12; 
 L14 = L131_;
 L151_=((unsigned)h4)<<((unsigned)6); 
 L16 = L151_;
 L171_=L14^L16; 
 ret_val = L171_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPIDE1936372555(TUPIDENTdTP self) {
 INT ret_val = INT_zero;
 TUPIDENTdTP elt_hash_self = TUPIDENTdTP_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPIDENTdTP elt_hash2_self = TUPIDENTdTP_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L3^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPIDE274165750(TUPIDENTARRAYdTP self) {
 INT ret_val = INT_zero;
 TUPIDENTARRAYdTP elt_hash_self = TUPIDENTARRAYdTP_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPIDENTARRAYdTP elt_hash2_self = TUPIDENTARRAYdTP_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L3^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPIDE280438974(TUPIDENTINT self) {
 INT ret_val = INT_zero;
 TUPIDENTINT elt_hash_self = TUPIDENTINT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPIDENTINT elt_hash2_self = TUPIDENTINT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 OB L1;
 OB L2;
 INT L3;
 OB L4;
 OB L5;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 elt_hash2_self = self;
 L5=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,sizeof(struct INT_boxed_struct));
 ((OB)L5)->header.tag=INT_tag;
 L4 = (OB)((INT_boxed) L5);
 ((INT_boxed) L4)->immutable_part = self.t2;
 elt_hash2_e = L4;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L6 = ret_val2;
 L71_=((unsigned)L6)<<((unsigned)2); 
 L8 = L71_;
 L91_=L3^L8; 
 ret_val = L91_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPIDE88035988(TUPIDE641654124 self) {
 INT ret_val = INT_zero;
 TUPIDE641654124 elt_hash_self = TUPIDE641654124_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPIDE641654124 elt_hash2_self = TUPIDE641654124_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L3^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPINT283365700(TUPINTINT self) {
 INT ret_val = INT_zero;
 TUPINTINT elt_hash_self = TUPINTINT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPINTINT elt_hash2_self = TUPINTINT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 OB L1;
 OB L2;
 INT L3;
 OB L4;
 OB L5;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 elt_hash_self = self;
 L2=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct INT_boxed_struct));
 ((OB)L2)->header.tag=INT_tag;
 L1 = (OB)((INT_boxed) L2);
 ((INT_boxed) L1)->immutable_part = self.t1;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 elt_hash2_self = self;
 L5=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,sizeof(struct INT_boxed_struct));
 ((OB)L5)->header.tag=INT_tag;
 L4 = (OB)((INT_boxed) L5);
 ((INT_boxed) L4)->immutable_part = self.t2;
 elt_hash2_e = L4;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L6 = ret_val2;
 L71_=((unsigned)L6)<<((unsigned)2); 
 L8 = L71_;
 L91_=L3^L8; 
 ret_val = L91_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG1080413784(TUPSIG578692189 self) {
 INT ret_val = INT_zero;
 TUPSIG578692189 elt_hash_self = TUPSIG578692189_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIG578692189 elt_hash2_self = TUPSIG578692189_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG1100158695(TUPSIGARG_LAYOUT self) {
 INT ret_val = INT_zero;
 TUPSIGARG_LAYOUT elt_hash_self = TUPSIGARG_LAYOUT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIGARG_LAYOUT elt_hash2_self = TUPSIGARG_LAYOUT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG1160082702(TUPSIGINT self) {
 INT ret_val = INT_zero;
 TUPSIGINT elt_hash_self = TUPSIGINT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIGINT elt_hash2_self = TUPSIGINT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 L3=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,sizeof(struct INT_boxed_struct));
 ((OB)L3)->header.tag=INT_tag;
 L2 = (OB)((INT_boxed) L3);
 ((INT_boxed) L2)->immutable_part = self.t2;
 elt_hash2_e = L2;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L1^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG1486928152(TUPSIGSTR self) {
 INT ret_val = INT_zero;
 TUPSIGSTR elt_hash_self = TUPSIGSTR_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIGSTR elt_hash2_self = TUPSIGSTR_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG1584335572(TUPSIGSIG self) {
 INT ret_val = INT_zero;
 TUPSIGSIG elt_hash_self = TUPSIGSIG_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIGSIG elt_hash2_self = TUPSIGSIG_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG2024347575(TUPSIG2023533247 self) {
 INT ret_val = INT_zero;
 TUPSIG2023533247 elt_hash_self = TUPSIG2023533247_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIG2023533247 elt_hash2_self = TUPSIG2023533247_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSIG524761103(TUPSIG1754078736 self) {
 INT ret_val = INT_zero;
 TUPSIG1754078736 elt_hash_self = TUPSIG1754078736_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSIG1754078736 elt_hash2_self = TUPSIG1754078736_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSTR1611378322(TUPSTRFSETSTR self) {
 INT ret_val = INT_zero;
 TUPSTRFSETSTR elt_hash_self = TUPSTRFSETSTR_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSTRFSETSTR elt_hash2_self = TUPSTRFSETSTR_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSTR737321694(TUPSTRINT self) {
 INT ret_val = INT_zero;
 TUPSTRINT elt_hash_self = TUPSTRINT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSTRINT elt_hash2_self = TUPSTRINT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 L3=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,sizeof(struct INT_boxed_struct));
 ((OB)L3)->header.tag=INT_tag;
 L2 = (OB)((INT_boxed) L3);
 ((INT_boxed) L2)->immutable_part = self.t2;
 elt_hash2_e = L2;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L1^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSTR813227328(TUPSTRSIG self) {
 INT ret_val = INT_zero;
 TUPSTRSIG elt_hash_self = TUPSTRSIG_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSTRSIG elt_hash2_self = TUPSTRSIG_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSTR864810129(TUPSTRdCONFIG self) {
 INT ret_val = INT_zero;
 TUPSTRdCONFIG elt_hash_self = TUPSTRdCONFIG_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSTRdCONFIG elt_hash2_self = TUPSTRdCONFIG_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPSTR910634748(TUPSTRSTR self) {
 INT ret_val = INT_zero;
 TUPSTRSTR elt_hash_self = TUPSTRSTR_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPSTRSTR elt_hash2_self = TUPSTRSTR_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPTP_108406739(TUPTP_858321267 self) {
 INT ret_val = INT_zero;
 TUPTP_858321267 elt_hash_self = TUPTP_858321267_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPTP_858321267 elt_hash2_self = TUPTP_858321267_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPTP_385216441(TUPTP_1668454172 self) {
 INT ret_val = INT_zero;
 TUPTP_1668454172 elt_hash_self = TUPTP_1668454172_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPTP_1668454172 elt_hash2_self = TUPTP_1668454172_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdAM1873776308(TUPdAM_EXPRdTP self) {
 INT ret_val = INT_zero;
 TUPdAM_EXPRdTP elt_hash_self = TUPdAM_EXPRdTP_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdAM_EXPRdTP elt_hash2_self = TUPdAM_EXPRdTP_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdAS472200087(TUPdAS268679779 self) {
 INT ret_val = INT_zero;
 TUPdAS268679779 elt_hash_self = TUPdAS268679779_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdAS268679779 elt_hash2_self = TUPdAS268679779_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdOB400520666(TUPdOBSTR self) {
 INT ret_val = INT_zero;
 TUPdOBSTR elt_hash_self = TUPdOBSTR_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdOBSTR elt_hash2_self = TUPdOBSTR_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = self.t1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdOB687982527(TUPdOBNAMESPACE self) {
 INT ret_val = INT_zero;
 TUPdOBNAMESPACE elt_hash_self = TUPdOBNAMESPACE_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdOBNAMESPACE elt_hash2_self = TUPdOBNAMESPACE_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = self.t1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdTP1209415271(TUPdTPINT self) {
 INT ret_val = INT_zero;
 TUPdTPINT elt_hash_self = TUPdTPINT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdTPINT elt_hash2_self = TUPdTPINT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 L3=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,sizeof(struct INT_boxed_struct));
 ((OB)L3)->header.tag=INT_tag;
 L2 = (OB)((INT_boxed) L3);
 ((INT_boxed) L2)->immutable_part = self.t2;
 elt_hash2_e = L2;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L4 = ret_val2;
 L51_=((unsigned)L4)<<((unsigned)2); 
 L6 = L51_;
 L71_=L1^L6; 
 ret_val = L71_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdTP1263366620(TUPdTP1710233051 self) {
 INT ret_val = INT_zero;
 TUPdTP1710233051 elt_hash_self = TUPdTP1710233051_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdTP1710233051 elt_hash2_self = TUPdTP1710233051_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdTP1991179276(TUPdTPdLAYOUT self) {
 INT ret_val = INT_zero;
 TUPdTPdLAYOUT elt_hash_self = TUPdTPdLAYOUT_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdTPdLAYOUT elt_hash2_self = TUPdTPdLAYOUT_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TUPdTP386219079(TUPdTPCODE_FILE self) {
 INT ret_val = INT_zero;
 TUPdTPCODE_FILE elt_hash_self = TUPdTPCODE_FILE_zero;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 TUPdTPCODE_FILE elt_hash2_self = TUPdTPCODE_FILE_zero;
 OB elt_hash2_e;
 INT ret_val2 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 elt_hash_self = self;
 elt_hash_e = ((OB) self.t1);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 elt_hash2_self = self;
 elt_hash2_e = ((OB) self.t2);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash2_e);
 L2 = ret_val2;
 L31_=((unsigned)L2)<<((unsigned)2); 
 L4 = L31_;
 L51_=L1^L4; 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_1694719258(TUPAM_1519040877 self) {
 STR ret_val;
 FSTR res;
 AM_LOCAL_EXPR lt1;
 AM_LOCAL_EXPR lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_489161612(TUPAM_927437481 self) {
 STR ret_val;
 FSTR res;
 AM_CURSOR lt1;
 dAM_EXPR lt2;
 dAM_STMT lt3;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 if (lt2==NULL) {
  goto other10;
 } else
 switch (TAG(lt2)) {
  default: ;
  other10: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt3 = self.t3;
 if (lt3==NULL) {
  goto other11;
 } else
 switch (TAG(lt3)) {
  default: ;
  other11: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_504028066(TUPAM_504653531 self) {
 STR ret_val;
 FSTR res;
 AM_LOCAL_EXPR lt1;
 OPT_LOCAL_EXPR lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_525468809(TUPAM_653469574 self) {
 STR ret_val;
 FSTR res;
 AM_ROUT_DEF lt1;
 FLISTA1062334999 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, FLISTA1884067430(lt2));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPAM_85614060(TUPAM_1024994801 self) {
 STR ret_val;
 FSTR res;
 AM_ROUT_DEF lt1;
 FLISTA1409846210 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, FLISTA389202533(lt2));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPARR1404914248(TUPARRAYARGdTP self) {
 STR ret_val;
 FSTR res;
 ARRAYARG lt1;
 dTP lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 dTP L1;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ARRAYARG_strrSTR(lt1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 L1 = lt2;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPARR735320083(TUPARR1759987776 self) {
 STR ret_val;
 FSTR res;
 ARRAYARG lt1;
 ARRAYBOOL lt2;
 dTP lt3;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR name3;
 dTP L1;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ARRAYARG_strrSTR(lt1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ARRAYB1997509424(lt2));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt3 = self.t3;
 L1 = lt3;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPBOO1584822636(TUPBOO1392923241 self) {
 STR ret_val;
 FSTR res;
 BOOL lt1 = BOOL_zero;
 INTI1 lt2;
 INT lt3 = INT_zero;
 BOOL lt4 = BOOL_zero;
 INTI1 str_self;
 STR ret_val1;
 FSTR clear_self;
 FSTR str_self1;
 STR ret_val2;
 INT str_self2 = INT_zero;
 STR ret_val3;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val4;
 FSTR str_self3;
 STR ret_val5;
 FSTR str_self4;
 STR ret_val6;
 extern STR name2;
 extern STR name3;
 BOOL L1;
 BOOL L21_;
 extern STR name3;
 BOOL L3;
 BOOL L41_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, BOOL_strrSTR(lt1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
 clear_self = INTI_buf;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 INTI_buf = INTI_s96356965(str_self, INTI_buf);
 str_self1 = INTI_buf;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val1 = ret_val2;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt3 = self.t3;
 str_self2 = lt3;
 clear_self1 = buf1;
 L3 = (clear_self1==((FSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self = str_self2;
 str_in_s = buf1;
 ret_val4 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val4;
 str_self3 = buf1;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val3 = ret_val5;
 res = FSTR_p1752847026(res, ret_val3);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt4 = self.t4;
 res = FSTR_p1752847026(res, BOOL_strrSTR(lt4));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val6 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val6;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPIDE1477036401(TUPIDE641654124 self) {
 STR ret_val;
 FSTR res;
 IDENT lt1 = IDENT_zero;
 AM_SHARED_EXPR lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPIDE1923523563(TUPIDENTARRAYdTP self) {
 STR ret_val;
 FSTR res;
 IDENT lt1 = IDENT_zero;
 ARRAYdTP lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ARRAYdTP_strrSTR(lt2));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPIDE2057908249(TUPIDENTdTP self) {
 STR ret_val;
 FSTR res;
 IDENT lt1 = IDENT_zero;
 dTP lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 dTP L1;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 L1 = lt2;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPIDE2120384756(TUPIDENTINT self) {
 STR ret_val;
 FSTR res;
 IDENT lt1 = IDENT_zero;
 INT lt2 = INT_zero;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 FSTR str_self2;
 STR ret_val4;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 BOOL L1;
 BOOL L21_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
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
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPINT567702316(TUPINTINT self) {
 STR ret_val;
 FSTR res;
 INT lt1 = INT_zero;
 INT lt2 = INT_zero;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 INT str_self2 = INT_zero;
 STR ret_val4;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val5;
 FSTR str_self3;
 STR ret_val6;
 FSTR str_self4;
 STR ret_val7;
 extern STR name2;
 BOOL L1;
 BOOL L21_;
 extern STR name3;
 BOOL L3;
 BOOL L41_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
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
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self2 = lt2;
 clear_self1 = buf2;
 L3 = (clear_self1==((FSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self2;
 str_in_s1 = buf2;
 ret_val5 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val5;
 str_self3 = buf2;
 ret_val6 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val4 = ret_val6;
 res = FSTR_p1752847026(res, ret_val4);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self4 = res;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG1001949053(TUPSIG1754078736 self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 ABSTRA151498155 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG1147554980(TUPSIGSIG self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 SIG lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG1626178992(TUPSIG578692189 self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 AM_ROUT_DEF lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG1970150778(TUPSIGINT self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 INT lt2 = INT_zero;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 FSTR str_self2;
 STR ret_val4;
 extern STR name2;
 extern STR name3;
 BOOL L1;
 BOOL L21_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
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
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG610691414(TUPSIGARG_LAYOUT self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 ARG_LAYOUT lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG659332932(TUPSIG2023533247 self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 FRAME_LAYOUT lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSIG926630776(TUPSIGSTR self) {
 STR ret_val;
 FSTR res;
 SIG lt1;
 STR lt2;
 STR str_self;
 STR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, ATTR(lt1,str));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSTR1075084367(TUPSTRdCONFIG self) {
 STR ret_val;
 FSTR res;
 STR lt1;
 dCONFIG lt2;
 STR str_self;
 STR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 extern STR name2;
 extern STR name3;
 dCONFIG L1;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 L1 = lt2;
 res = FSTR_p1752847026(res, (*dCONFIG_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSTR1520476266(TUPSTRINT self) {
 STR ret_val;
 FSTR res;
 STR lt1;
 INT lt2 = INT_zero;
 STR str_self;
 STR ret_val1;
 INT str_self1 = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self2;
 STR ret_val4;
 FSTR str_self3;
 STR ret_val5;
 extern STR name2;
 extern STR name3;
 BOOL L1;
 BOOL L21_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self1 = lt2;
 clear_self = buf1;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self1;
 str_in_s = buf1;
 ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val3;
 str_self2 = buf1;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val2 = ret_val4;
 res = FSTR_p1752847026(res, ret_val2);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self3 = res;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSTR1622911390(TUPSTRFSETSTR self) {
 STR ret_val;
 FSTR res;
 STR lt1;
 FSETSTR lt2;
 STR str_self;
 STR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 extern STR name2;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSTR1730971028(TUPSTRSTR self) {
 STR ret_val;
 FSTR res;
 STR lt1;
 STR lt2;
 STR str_self;
 STR ret_val1;
 STR str_self1;
 STR ret_val2;
 FSTR str_self2;
 STR ret_val3;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self1 = lt2;
 ret_val2 = str_self1;
 res = FSTR_p1752847026(res, ret_val2);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPSTR1951895232(TUPSTRSIG self) {
 STR ret_val;
 FSTR res;
 STR lt1;
 SIG lt2;
 STR str_self;
 STR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 extern STR name2;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 str_self = lt1;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ATTR(lt2,str));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPTP_131070026(TUPTP_858321267 self) {
 STR ret_val;
 FSTR res;
 TP_CLASS lt1;
 FSETdTP lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, TP_CLASS_strrSTR(lt1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPTP_371689873(TUPTP_1668454172 self) {
 STR ret_val;
 FSTR res;
 TP_CLASS lt1;
 FSETTP_CLASS lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 res = FSTR_p1752847026(res, TP_CLASS_strrSTR(lt1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdAM1835151881(TUPdAM_EXPRdTP self) {
 STR ret_val;
 FSTR res;
 dAM_EXPR lt1;
 dTP lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 dTP L1;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 if (lt1==NULL) {
  goto other92;
 } else
 switch (TAG(lt1)) {
  default: ;
  other92: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 L1 = lt2;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdAS100511776(TUPdAS268679779 self) {
 STR ret_val;
 FSTR res;
 dAS_EXPR lt1;
 AS_ARG_MODE lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 if (lt1==NULL) {
  goto other93;
 } else
 switch (TAG(lt1)) {
  default: ;
  other93: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdOB1460589826(TUPdOBSTR self) {
 STR ret_val;
 FSTR res;
 OB lt1;
 STR lt2;
 STR str_self;
 STR ret_val1;
 FSTR str_self1;
 STR ret_val2;
 extern STR name2;
 dSTR L1;
 extern STR S_;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 if (lt1==NULL) {
  goto other9;
 } else
 switch (TAG(lt1)) {
  case ABSTRA151498155_tag:
  case ABSTRACT_LAYOUT_tag:
  case ARG_LAYOUT_tag:
  case ARRAYdAM_CONST_tag:
  case ARRAYdAM_EXPR_tag:
  case ARRAYdAM_STMT_tag:
  case ARRAYdLAYOUT_tag:
  case ARRAYdTP_tag:
  case ARRAYA1870808380_tag:
  case ARRAYA855692714_tag:
  case ARRAYA132553625_tag:
  case ARRAYAM_CALL_ARG_tag:
  case ARRAYA36280886_tag:
  case ARRAYAM_CURSOR_tag:
  case ARRAYA1347292543_tag:
  case ARRAYA1369744492_tag:
  case ARRAYA1575632848_tag:
  case ARRAYAM_ROUT_DEF_tag:
  case ARRAYA1135548710_tag:
  case ARRAYARG_tag:
  case ARRAYA477251019_tag:
  case ARRAYARRAYBOOL_tag:
  case ARRAYARRAYSTR_tag:
  case ARRAYAS_FEAT_MOD_tag:
  case ARRAYATTRSORT_tag:
  case ARRAYBOOL_tag:
  case ARRAYB1234765325_tag:
  case ARRAYB1925477728_tag:
  case ARRAYB1039347268_tag:
  case ARRAYCALL_ARG_tag:
  case ARRAYCHAR_tag:
  case ARRAYC1438655838_tag:
  case ARRAYCODE_FILE_tag:
  case ARRAYC1788421899_tag:
  case ARRAYE2126408533_tag:
  case ARRAYE406487263_tag:
  case ARRAYF1030817442_tag:
  case ARRAYFLISTSTR_tag:
  case ARRAYFSTR_tag:
  case ARRAYIDENT_tag:
  case ARRAYIFC_tag:
  case ARRAYI1327367692_tag:
  case ARRAYI1991954990_tag:
  case ARRAYINT_tag:
  case ARRAYO570594127_tag:
  case ARRAYSFILE_ID_tag:
  case ARRAYSIDE_EFFECT_tag:
  case ARRAYSIG_tag:
  case ARRAYSTR_tag:
  case ARRAYTP_CLASS_tag:
  case ARRAYTP_LAYOUT_tag:
  case ARRAYT747572380_tag:
  case ARRAYTUPdOBSTR_tag:
  case ARRAYT1418209173_tag:
  case ARRAYT1153101058_tag:
  case ARRAYT1562141251_tag:
  case ARRAYTUPdTPINT_tag:
  case ARRAYT1275404834_tag:
  case ARRAYT1146239647_tag:
  case ARRAYT2065045042_tag:
  case ARRAYT1753333425_tag:
  case ARRAYT1744744076_tag:
  case ARRAYT546647214_tag:
  case ARRAYT1968212795_tag:
  case ARRAYTUPIDENTdTP_tag:
  case ARRAYT263953675_tag:
  case ARRAYTUPIDENTINT_tag:
  case ARRAYT348931282_tag:
  case ARRAYT537394765_tag:
  case ARRAYT1759168384_tag:
  case ARRAYT1754348798_tag:
  case ARRAYTUPSIGINT_tag:
  case ARRAYTUPSIGSIG_tag:
  case ARRAYTUPSIGSTR_tag:
  case ARRAYT61112132_tag:
  case ARRAYT378612147_tag:
  case ARRAYTUPSTRINT_tag:
  case ARRAYTUPSTRSIG_tag:
  case ARRAYTUPSTRSTR_tag:
  case ARRAYT1127505716_tag:
  case ARRAYT1641775270_tag:
  case A_STAC1244966098_tag:
  case BOOL_tag:
  case BOUND_1007407331_tag:
  case BOUND_2140373124_tag:
  case BOUND_1677815802_tag:
  case BOUND_809390774_tag:
  case BUILTIN_LAYOUT_tag:
  case CHAR_tag:
  case CLASS_LAYOUT_tag:
  case CONFIG_DEF_tag:
  case CONFIG_ROUT_tag:
  case CONFIG_TBL_tag:
  case ELT_TBL_tag:
  case EXTERNAL_LAYOUT_tag:
  case FLISTdAM_CONST_tag:
  case FLISTdAM_EXPR_tag:
  case FLISTdAM_STMT_tag:
  case FLISTdLAYOUT_tag:
  case FLISTdTP_tag:
  case FLISTA162798263_tag:
  case FLISTA1893461511_tag:
  case FLISTA1409846210_tag:
  case FLISTAM_CALL_ARG_tag:
  case FLISTA886630705_tag:
  case FLISTAM_CURSOR_tag:
  case FLISTA2097324934_tag:
  case FLISTA1062334999_tag:
  case FLISTA725283029_tag:
  case FLISTAM_ROUT_DEF_tag:
  case FLISTA112326087_tag:
  case FLISTARG_tag:
  case FLISTA1327600838_tag:
  case FLISTARRAYBOOL_tag:
  case FLISTARRAYSTR_tag:
  case FLISTAS_FEAT_MOD_tag:
  case FLISTATTRSORT_tag:
  case FLISTBOOL_tag:
  case FLISTB475591118_tag:
  case FLISTB2062080075_tag:
  case FLISTB16124645_tag:
  case FLISTCALL_ARG_tag:
  case FLISTCHAR_tag:
  case FLISTC1031309271_tag:
  case FLISTCODE_FILE_tag:
  case FLISTC1772242452_tag:
  case FLISTE1761212196_tag:
  case FLISTE1627119380_tag:
  case FLISTF7594819_tag:
  case FLISTFLISTSTR_tag:
  case FLISTFSTR_tag:
  case FLISTIDENT_tag:
  case FLISTIFC_tag:
  case FLISTI2086541899_tag:
  case FLISTI1895665739_tag:
  case FLISTINT_tag:
  case FLISTO1593816750_tag:
  case FLISTSFILE_ID_tag:
  case FLISTSIDE_EFFECT_tag:
  case FLISTSIG_tag:
  case FLISTSTR_tag:
  case FLISTTP_CLASS_tag:
  case FLISTTP_LAYOUT_tag:
  case FLISTT1513788273_tag:
  case FLISTTUPdOBSTR_tag:
  case FLISTT567859354_tag:
  case FLISTT880505585_tag:
  case FLISTT1858637310_tag:
  case FLISTTUPdTPINT_tag:
  case FLISTT1311334615_tag:
  case FLISTT1661203254_tag:
  case FLISTT1145236345_tag:
  case FLISTT2049829484_tag:
  case FLISTT406286251_tag:
  case FLISTT31683607_tag:
  case FLISTT1683165204_tag:
  case FLISTTUPIDENTdTP_tag:
  case FLISTT1023127882_tag:
  case FLISTTUPIDENTINT_tag:
  case FLISTT1075494997_tag:
  case FLISTT844804258_tag:
  case FLISTT1742988937_tag:
  case FLISTT994805427_tag:
  case FLISTTUPSIGINT_tag:
  case FLISTTUPSIGSIG_tag:
  case FLISTTUPSIGSTR_tag:
  case FLISTT789237687_tag:
  case FLISTT471737672_tag:
  case FLISTTUPSTRINT_tag:
  case FLISTTUPSTRSIG_tag:
  case FLISTTUPSTRSTR_tag:
  case FLISTT418307355_tag:
  case FLISTT102367225_tag:
  case FLT_tag:
  case FLTD_tag:
  case FMAPdOBNAMESPACE_tag:
  case FMAPdTPdLAYOUT_tag:
  case FMAPdTPCODE_FILE_tag:
  case FMAPdT953816637_tag:
  case FMAPdTPINT_tag:
  case FMAPAM339652544_tag:
  case FMAPAM1357596931_tag:
  case FMAPAM1145008811_tag:
  case FMAPAM418009930_tag:
  case FMAPAM1244483504_tag:
  case FMAPAM1787628573_tag:
  case FMAPIDENTdTP_tag:
  case FMAPID739086519_tag:
  case FMAPSI2103621588_tag:
  case FMAPSI518162669_tag:
  case FMAPSI1349988702_tag:
  case FMAPSI1121373188_tag:
  case FMAPSIGSTR_tag:
  case FMAPSTRdCONFIG_tag:
  case FMAPSTRFSETSTR_tag:
  case FMAPSTRINT_tag:
  case FMAPSTRSIG_tag:
  case FMAPSTRSTR_tag:
  case FMAPTP291739594_tag:
  case FMAPTP775297600_tag:
  case FMT_ERROR_tag:
  case FRAME_LAYOUT_tag:
  case FSTR_tag:
  case IFC_tag:
  case IMMUTA421759893_tag:
  case INOUT_MODE_tag:
  case INT_tag:
  case INTI1_tag:
  case IN_MODE_tag:
  case NULL_STAT_tag:
  case ONCE_MODE_tag:
  case OUT_MODE_tag:
  case RAT_tag:
  case SFILE_ID_tag:
  case SIG_tag:
  case SIG_TBL_tag:
  case STAT1_tag:
  case STR_tag:
  case TP_CLASS_tag:
  case TP_ITER_tag:
  case TP_LAYOUT_tag:
  case TP_ROUT_tag:
  case TUPdAM_EXPRdTP_tag:
  case TUPdAS268679779_tag:
  case TUPdOBNAMESPACE_tag:
  case TUPdOBSTR_tag:
  case TUPdTPdLAYOUT_tag:
  case TUPdTPCODE_FILE_tag:
  case TUPdTP1710233051_tag:
  case TUPdTPINT_tag:
  case TUPAM_304458649_tag:
  case TUPAM_667892060_tag:
  case TUPAM_927437481_tag:
  case TUPAM_1519040877_tag:
  case TUPAM_504653531_tag:
  case TUPAM_1024994801_tag:
  case TUPAM_653469574_tag:
  case TUPARRAYARGdTP_tag:
  case TUPARR1759987776_tag:
  case TUPBOO1392923241_tag:
  case TUPIDENTdTP_tag:
  case TUPIDE641654124_tag:
  case TUPIDENTARRAYdTP_tag:
  case TUPIDENTINT_tag:
  case TUPINTINT_tag:
  case TUPSIG1754078736_tag:
  case TUPSIG578692189_tag:
  case TUPSIGARG_LAYOUT_tag:
  case TUPSIG2023533247_tag:
  case TUPSIGINT_tag:
  case TUPSIGSIG_tag:
  case TUPSIGSTR_tag:
  case TUPSTRdCONFIG_tag:
  case TUPSTRFSETSTR_tag:
  case TUPSTRINT_tag:
  case TUPSTRSIG_tag:
  case TUPSTRSTR_tag:
  case TUPTP_858321267_tag:
  case TUPTP_1668454172_tag:
   L1 = ((dSTR) lt1);
   res = FSTR_p1752847026(res, (*dSTR_strrSTR[TAG(L1)])(L1)); break;
  default: ;
  other9: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
 ret_val1 = str_self;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdOB1872884181(TUPdOBNAMESPACE self) {
 STR ret_val;
 FSTR res;
 OB lt1;
 NAMESPACE lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dSTR L1;
 extern STR S_;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 if (lt1==NULL) {
  goto other8;
 } else
 switch (TAG(lt1)) {
  case ABSTRA151498155_tag:
  case ABSTRACT_LAYOUT_tag:
  case ARG_LAYOUT_tag:
  case ARRAYdAM_CONST_tag:
  case ARRAYdAM_EXPR_tag:
  case ARRAYdAM_STMT_tag:
  case ARRAYdLAYOUT_tag:
  case ARRAYdTP_tag:
  case ARRAYA1870808380_tag:
  case ARRAYA855692714_tag:
  case ARRAYA132553625_tag:
  case ARRAYAM_CALL_ARG_tag:
  case ARRAYA36280886_tag:
  case ARRAYAM_CURSOR_tag:
  case ARRAYA1347292543_tag:
  case ARRAYA1369744492_tag:
  case ARRAYA1575632848_tag:
  case ARRAYAM_ROUT_DEF_tag:
  case ARRAYA1135548710_tag:
  case ARRAYARG_tag:
  case ARRAYA477251019_tag:
  case ARRAYARRAYBOOL_tag:
  case ARRAYARRAYSTR_tag:
  case ARRAYAS_FEAT_MOD_tag:
  case ARRAYATTRSORT_tag:
  case ARRAYBOOL_tag:
  case ARRAYB1234765325_tag:
  case ARRAYB1925477728_tag:
  case ARRAYB1039347268_tag:
  case ARRAYCALL_ARG_tag:
  case ARRAYCHAR_tag:
  case ARRAYC1438655838_tag:
  case ARRAYCODE_FILE_tag:
  case ARRAYC1788421899_tag:
  case ARRAYE2126408533_tag:
  case ARRAYE406487263_tag:
  case ARRAYF1030817442_tag:
  case ARRAYFLISTSTR_tag:
  case ARRAYFSTR_tag:
  case ARRAYIDENT_tag:
  case ARRAYIFC_tag:
  case ARRAYI1327367692_tag:
  case ARRAYI1991954990_tag:
  case ARRAYINT_tag:
  case ARRAYO570594127_tag:
  case ARRAYSFILE_ID_tag:
  case ARRAYSIDE_EFFECT_tag:
  case ARRAYSIG_tag:
  case ARRAYSTR_tag:
  case ARRAYTP_CLASS_tag:
  case ARRAYTP_LAYOUT_tag:
  case ARRAYT747572380_tag:
  case ARRAYTUPdOBSTR_tag:
  case ARRAYT1418209173_tag:
  case ARRAYT1153101058_tag:
  case ARRAYT1562141251_tag:
  case ARRAYTUPdTPINT_tag:
  case ARRAYT1275404834_tag:
  case ARRAYT1146239647_tag:
  case ARRAYT2065045042_tag:
  case ARRAYT1753333425_tag:
  case ARRAYT1744744076_tag:
  case ARRAYT546647214_tag:
  case ARRAYT1968212795_tag:
  case ARRAYTUPIDENTdTP_tag:
  case ARRAYT263953675_tag:
  case ARRAYTUPIDENTINT_tag:
  case ARRAYT348931282_tag:
  case ARRAYT537394765_tag:
  case ARRAYT1759168384_tag:
  case ARRAYT1754348798_tag:
  case ARRAYTUPSIGINT_tag:
  case ARRAYTUPSIGSIG_tag:
  case ARRAYTUPSIGSTR_tag:
  case ARRAYT61112132_tag:
  case ARRAYT378612147_tag:
  case ARRAYTUPSTRINT_tag:
  case ARRAYTUPSTRSIG_tag:
  case ARRAYTUPSTRSTR_tag:
  case ARRAYT1127505716_tag:
  case ARRAYT1641775270_tag:
  case A_STAC1244966098_tag:
  case BOOL_tag:
  case BOUND_1007407331_tag:
  case BOUND_2140373124_tag:
  case BOUND_1677815802_tag:
  case BOUND_809390774_tag:
  case BUILTIN_LAYOUT_tag:
  case CHAR_tag:
  case CLASS_LAYOUT_tag:
  case CONFIG_DEF_tag:
  case CONFIG_ROUT_tag:
  case CONFIG_TBL_tag:
  case ELT_TBL_tag:
  case EXTERNAL_LAYOUT_tag:
  case FLISTdAM_CONST_tag:
  case FLISTdAM_EXPR_tag:
  case FLISTdAM_STMT_tag:
  case FLISTdLAYOUT_tag:
  case FLISTdTP_tag:
  case FLISTA162798263_tag:
  case FLISTA1893461511_tag:
  case FLISTA1409846210_tag:
  case FLISTAM_CALL_ARG_tag:
  case FLISTA886630705_tag:
  case FLISTAM_CURSOR_tag:
  case FLISTA2097324934_tag:
  case FLISTA1062334999_tag:
  case FLISTA725283029_tag:
  case FLISTAM_ROUT_DEF_tag:
  case FLISTA112326087_tag:
  case FLISTARG_tag:
  case FLISTA1327600838_tag:
  case FLISTARRAYBOOL_tag:
  case FLISTARRAYSTR_tag:
  case FLISTAS_FEAT_MOD_tag:
  case FLISTATTRSORT_tag:
  case FLISTBOOL_tag:
  case FLISTB475591118_tag:
  case FLISTB2062080075_tag:
  case FLISTB16124645_tag:
  case FLISTCALL_ARG_tag:
  case FLISTCHAR_tag:
  case FLISTC1031309271_tag:
  case FLISTCODE_FILE_tag:
  case FLISTC1772242452_tag:
  case FLISTE1761212196_tag:
  case FLISTE1627119380_tag:
  case FLISTF7594819_tag:
  case FLISTFLISTSTR_tag:
  case FLISTFSTR_tag:
  case FLISTIDENT_tag:
  case FLISTIFC_tag:
  case FLISTI2086541899_tag:
  case FLISTI1895665739_tag:
  case FLISTINT_tag:
  case FLISTO1593816750_tag:
  case FLISTSFILE_ID_tag:
  case FLISTSIDE_EFFECT_tag:
  case FLISTSIG_tag:
  case FLISTSTR_tag:
  case FLISTTP_CLASS_tag:
  case FLISTTP_LAYOUT_tag:
  case FLISTT1513788273_tag:
  case FLISTTUPdOBSTR_tag:
  case FLISTT567859354_tag:
  case FLISTT880505585_tag:
  case FLISTT1858637310_tag:
  case FLISTTUPdTPINT_tag:
  case FLISTT1311334615_tag:
  case FLISTT1661203254_tag:
  case FLISTT1145236345_tag:
  case FLISTT2049829484_tag:
  case FLISTT406286251_tag:
  case FLISTT31683607_tag:
  case FLISTT1683165204_tag:
  case FLISTTUPIDENTdTP_tag:
  case FLISTT1023127882_tag:
  case FLISTTUPIDENTINT_tag:
  case FLISTT1075494997_tag:
  case FLISTT844804258_tag:
  case FLISTT1742988937_tag:
  case FLISTT994805427_tag:
  case FLISTTUPSIGINT_tag:
  case FLISTTUPSIGSIG_tag:
  case FLISTTUPSIGSTR_tag:
  case FLISTT789237687_tag:
  case FLISTT471737672_tag:
  case FLISTTUPSTRINT_tag:
  case FLISTTUPSTRSIG_tag:
  case FLISTTUPSTRSTR_tag:
  case FLISTT418307355_tag:
  case FLISTT102367225_tag:
  case FLT_tag:
  case FLTD_tag:
  case FMAPdOBNAMESPACE_tag:
  case FMAPdTPdLAYOUT_tag:
  case FMAPdTPCODE_FILE_tag:
  case FMAPdT953816637_tag:
  case FMAPdTPINT_tag:
  case FMAPAM339652544_tag:
  case FMAPAM1357596931_tag:
  case FMAPAM1145008811_tag:
  case FMAPAM418009930_tag:
  case FMAPAM1244483504_tag:
  case FMAPAM1787628573_tag:
  case FMAPIDENTdTP_tag:
  case FMAPID739086519_tag:
  case FMAPSI2103621588_tag:
  case FMAPSI518162669_tag:
  case FMAPSI1349988702_tag:
  case FMAPSI1121373188_tag:
  case FMAPSIGSTR_tag:
  case FMAPSTRdCONFIG_tag:
  case FMAPSTRFSETSTR_tag:
  case FMAPSTRINT_tag:
  case FMAPSTRSIG_tag:
  case FMAPSTRSTR_tag:
  case FMAPTP291739594_tag:
  case FMAPTP775297600_tag:
  case FMT_ERROR_tag:
  case FRAME_LAYOUT_tag:
  case FSTR_tag:
  case IFC_tag:
  case IMMUTA421759893_tag:
  case INOUT_MODE_tag:
  case INT_tag:
  case INTI1_tag:
  case IN_MODE_tag:
  case NULL_STAT_tag:
  case ONCE_MODE_tag:
  case OUT_MODE_tag:
  case RAT_tag:
  case SFILE_ID_tag:
  case SIG_tag:
  case SIG_TBL_tag:
  case STAT1_tag:
  case STR_tag:
  case TP_CLASS_tag:
  case TP_ITER_tag:
  case TP_LAYOUT_tag:
  case TP_ROUT_tag:
  case TUPdAM_EXPRdTP_tag:
  case TUPdAS268679779_tag:
  case TUPdOBNAMESPACE_tag:
  case TUPdOBSTR_tag:
  case TUPdTPdLAYOUT_tag:
  case TUPdTPCODE_FILE_tag:
  case TUPdTP1710233051_tag:
  case TUPdTPINT_tag:
  case TUPAM_304458649_tag:
  case TUPAM_667892060_tag:
  case TUPAM_927437481_tag:
  case TUPAM_1519040877_tag:
  case TUPAM_504653531_tag:
  case TUPAM_1024994801_tag:
  case TUPAM_653469574_tag:
  case TUPARRAYARGdTP_tag:
  case TUPARR1759987776_tag:
  case TUPBOO1392923241_tag:
  case TUPIDENTdTP_tag:
  case TUPIDE641654124_tag:
  case TUPIDENTARRAYdTP_tag:
  case TUPIDENTINT_tag:
  case TUPINTINT_tag:
  case TUPSIG1754078736_tag:
  case TUPSIG578692189_tag:
  case TUPSIGARG_LAYOUT_tag:
  case TUPSIG2023533247_tag:
  case TUPSIGINT_tag:
  case TUPSIGSIG_tag:
  case TUPSIGSTR_tag:
  case TUPSTRdCONFIG_tag:
  case TUPSTRFSETSTR_tag:
  case TUPSTRINT_tag:
  case TUPSTRSIG_tag:
  case TUPSTRSTR_tag:
  case TUPTP_858321267_tag:
  case TUPTP_1668454172_tag:
   L1 = ((dSTR) lt1);
   res = FSTR_p1752847026(res, (*dSTR_strrSTR[TAG(L1)])(L1)); break;
  default: ;
  other8: ;
   res = FSTR_p1752847026(res, ((STR) &S_));
 }
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdTP1335739968(TUPdTPdLAYOUT self) {
 STR ret_val;
 FSTR res;
 dTP lt1;
 dLAYOUT lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dTP L1;
 extern STR name3;
 dLAYOUT L2;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 L1 = lt1;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 L2 = lt2;
 res = FSTR_p1752847026(res, (*dLAYOUT_strrSTR[TAG(L2)])(L2));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdTP1477319287(TUPdTPCODE_FILE self) {
 STR ret_val;
 FSTR res;
 dTP lt1;
 CODE_FILE lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dTP L1;
 extern STR name3;
 extern STR S_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 L1 = lt1;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, ((STR) &S_));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdTP1972747229(TUPdTPINT self) {
 STR ret_val;
 FSTR res;
 dTP lt1;
 INT lt2 = INT_zero;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 FSTR str_self2;
 STR ret_val4;
 extern STR name2;
 dTP L1;
 extern STR name3;
 BOOL L2;
 BOOL L31_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 L1 = lt1;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 str_self = lt2;
 clear_self = buf1;
 L2 = (clear_self==((FSTR) NULL));
 L31_=!(L2); 
 if (L31_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val2;
 str_self1 = buf1;
 ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val1 = ret_val3;
 res = FSTR_p1752847026(res, ret_val1);
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TUPdTP2068651704(TUPdTP1710233051 self) {
 STR ret_val;
 FSTR res;
 dTP lt1;
 FMAPID739086519 lt2;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dTP L1;
 extern STR name3;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 lt1 = self.t1;
 L1 = lt1;
 res = FSTR_p1752847026(res, (*dTP_strrSTR[TAG(L1)])(L1));
 res = FSTR_p1752847026(res, ((STR) &name3));
 lt2 = self.t2;
 res = FSTR_p1752847026(res, FMAPID1895267434(lt2));
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}
