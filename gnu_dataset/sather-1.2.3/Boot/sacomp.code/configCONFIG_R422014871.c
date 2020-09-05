#include "sather.h"

/* Layouts */

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

typedef struct dCONFIG_struct {
 OB_HEADER header;
 } *dCONFIG;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ARRAYdAM_CONST_struct {/* layout for ARRAY{$AM_CONST} */
 OB_HEADER header;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *ARRAYdAM_CONST;

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

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

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

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct INT_timesbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesbrINT_frame;

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

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct TUPSTRdCONFIG_struct {/* layout for TUP{STR,$CONFIG} */
 struct dCONFIG_struct *t2;
 STR t1;
 } TUPSTRdCONFIG;
static TUPSTRdCONFIG TUPSTRdCONFIG_zero;

typedef struct TUPSTRdCONFIG_boxed_struct {
 OB_HEADER header;
 TUPSTRdCONFIG immutable_part;
 } *TUPSTRdCONFIG_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

typedef struct AM_ARR_CONST_struct {/* layout for AM_ARR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct ARRAYdAM_CONST_struct *elts;
 struct SFILE_ID_struct source1;
 struct TP_CLASS_struct *elt_tp;
 } *AM_ARR_CONST;

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

typedef struct AM_CHAR_CONST_struct {/* layout for AM_CHAR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 CHAR bval;
 } *AM_CHAR_CONST;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

typedef struct AM_VOID_CONST_struct {/* layout for AM_VOID_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_VOID_CONST;

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

typedef struct CONFIG_SCANNER_struct {/* layout for CONFIG_SCANNER */
 OB_HEADER header;
 struct FSTR_struct *fstr;
 INT line1;
 INT next;
 STR file1;
 } *CONFIG_SCANNER;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTFLISTSTR_struct {/* layout for FLIST{FLIST{STR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct FLISTSTR_struct *arr_part[1];
 } *FLISTFLISTSTR;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FMAPSTRdCONFIG_struct {/* layout for FMAP{STR,$CONFIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *FMAPSTRdCONFIG;

typedef struct FMAPST1785271125_frame_struct {
 INT state;
 FMAPSTRdCONFIG self;/* Formal argument: self */
 dCONFIG ret_val2;
 FMAPSTRdCONFIG L61;
 TUPSTRdCONFIG e;
 INT L31;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1785271125_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct CONFIG1181711269_frame_struct {
 INT state;
 CONFIG_SCANNER self;/* Formal argument: self */
 dCONFIG ret_val;
 CONFIG_SCANNER L21;
 STR id;
 FLISTFLISTSTR exprs;
 CONFIG_SCANNER L41;
 struct CONFIG875430285_frame_struct *nested1; /* nested iter frame */
 struct CONFIG1589956977_frame_struct *nested2; /* nested iter frame */
 } *CONFIG1181711269_frame;

typedef struct CONFIG1589956977_frame_struct {
 INT state;
 CONFIG_SCANNER self;/* Formal argument: self */
 FLISTSTR ret_val1;
 STR r;
 FSTR aget_self;
 INT aget_ind;
 CHAR ret_val;
 } *CONFIG1589956977_frame;

typedef struct CONFIG875430285_frame_struct {
 INT state;
 CONFIG_SCANNER self;/* Formal argument: self */
 STR ret_val13;
 INT start1;
 STR uid1;
 CONFIG_SCANNER eof_self;
 BOOL ret_val;
 FSTR aget_self;
 INT aget_ind;
 CHAR ret_val1;
 FSTR aget_self1;
 INT aget_ind1;
 CHAR ret_val2;
 FSTR aget_self2;
 INT aget_ind2;
 CHAR ret_val3;
 OUT create_self;
 OUT ret_val4;
 OUT plus_self3;
 STR plus_s;
 OUT ret_val6;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OUT plus_self2;
 STR plus_s1;
 OUT ret_val8;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r1;
 OUT plus_self1;
 dSTR plus_s2;
 OUT ret_val10;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r2;
 OUT plus_self;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val11;
 FILE1 r3;
 UNIX exit_self;
 INT exit_code;
 FSTR str_self;
 STR ret_val12;
 } *CONFIG875430285_frame;

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

#include "tags.h"

/* Globals */


/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_CO637713226)(dAM_CONST, OB);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST INT (**dAM_CO1597271567)(dAM_CONST);

extern RETURNED_CONST STR (**dCONFIG_namerSTR)(dCONFIG);

extern RETURNED_CONST STR (**dCONFIG_strrSTR)(dCONFIG);
AM_ARR_CONST AM_ARR990477897(AM_ARR_CONST);
AM_BOOL_CONST AM_BOO158864518(AM_BOOL_CONST);
AM_CHAR_CONST AM_CHA916745766(AM_CHAR_CONST);
AM_FLTDX_CONST AM_FLT822990115(AM_FLTDX_CONST);
AM_FLTD_CONST AM_FLT1839670182(AM_FLTD_CONST);
AM_FLTX_CONST AM_FLT476428534(AM_FLTX_CONST);
AM_FLT_CONST AM_FLT1362803603(AM_FLT_CONST);
AM_INTI_CONST AM_INT304374138(AM_INTI_CONST);
AM_INT_CONST AM_INT1629629861(AM_INT_CONST);
AM_STR_CONST AM_STR1745454041(AM_STR_CONST);
AM_VOID_CONST AM_VOI1734446450(AM_VOID_CONST);
ARRAYINT CONFIG727192651(CONFIG_ROUT, FLISTSTR);
ARRAYSTR CONFIG1467324007(CONFIG_ROUT, FLISTSTR);
BOOL AM_ARR495057476(AM_ARR_CONST);
BOOL AM_ARR62730649(AM_ARR_CONST, OB);
BOOL AM_BOO1499785931(AM_BOOL_CONST, OB);
BOOL AM_BOO274293822(AM_BOOL_CONST);
BOOL AM_CHA1550967974(AM_CHAR_CONST);
BOOL AM_CHA181917731(AM_CHAR_CONST, OB);
BOOL AM_FLT1108482895(AM_FLTX_CONST, OB);
BOOL AM_FLT1144488899(AM_FLTD_CONST, OB);
BOOL AM_FLT1356295273(AM_FLT_CONST);
BOOL AM_FLT1602735735(AM_FLTDX_CONST, OB);
BOOL AM_FLT1739286436(AM_FLT_CONST, OB);
BOOL AM_FLT1865244436(AM_FLTDX_CONST);
BOOL AM_FLT652620710(AM_FLTX_CONST);
BOOL AM_FLT93541882(AM_FLTD_CONST);
BOOL AM_INT1418540518(AM_INT_CONST);
BOOL AM_INT1567340277(AM_INT_CONST, OB);
BOOL AM_INT341842242(AM_INTI_CONST);
BOOL AM_INT586605643(AM_INTI_CONST, OB);
BOOL AM_STR1095405116(AM_STR_CONST);
BOOL AM_STR2039863889(AM_STR_CONST, OB);
BOOL AM_VOI1214668736(AM_VOID_CONST);
BOOL AM_VOI2144357369(AM_VOID_CONST, OB);
BOOL CONFIG1490450026(CONFIG_ROUT, FLISTFLISTSTR);
BOOL CONFIG1864245349(CONFIG_SCANNER);
BOOL CONFIG298217444(CONFIG_SCANNER);
BOOL CONFIG299011603(CONFIG_SCANNER);
BOOL CONFIG536838194(CONFIG_ROUT, FLISTFLISTSTR);
BOOL CONFIG915316977(CONFIG_SCANNER);
BOOL CONFIG981438152(CONFIG_SCANNER);
BOOL STR_is111530248(STR, STR);
CONFIG_DEF CONFIG1152920950(CONFIG_TBL, STR);
CONFIG_DEF CONFIG2114270854(CONFIG_DEF, STR, FLISTFLISTSTR);
CONFIG_ROUT CONFIG525418741(CONFIG_TBL, STR);
CONFIG_ROUT CONFIG725916253(CONFIG_ROUT, STR, FLISTFLISTSTR);
CONFIG_SCANNER CONFIG647536087(CONFIG_SCANNER, STR);
FLISTA725283029 AM_ARR1270029884(AM_ARR_CONST);
FLISTA725283029 AM_BOO1622702250(AM_BOOL_CONST);
FLISTA725283029 AM_CHA722016210(AM_CHAR_CONST);
FLISTA725283029 AM_FLT1309796914(AM_FLTD_CONST);
FLISTA725283029 AM_FLT1728105754(AM_FLTX_CONST);
FLISTA725283029 AM_FLT1832811964(AM_FLTDX_CONST);
FLISTA725283029 AM_FLT945201335(AM_FLT_CONST);
FLISTA725283029 AM_INT1223291058(AM_INT_CONST);
FLISTA725283029 AM_INT2023229226(AM_INTI_CONST);
FLISTA725283029 AM_STR201189492(AM_STR_CONST);
FLISTA725283029 AM_VOI402497764(AM_VOID_CONST);
FLISTFLISTSTR FLISTF630314943(FLISTFLISTSTR, FLISTSTR);
FLISTSTR CONFIG1589956977(CONFIG1589956977_frame);
FLISTSTR CONFIG666119611(CONFIG_SCANNER);
FLISTSTR FLISTF1766102529(FLISTF1766102529_frame);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FMAPSTRdCONFIG FMAPST2004940399(FMAPSTRdCONFIG, STR, dCONFIG);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_s1920457899(FSTR, INT, INT);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AM_ARR1424607794(AM_ARR_CONST);
INT AM_BOO561475672(AM_BOOL_CONST);
INT AM_CHA1966863808(AM_CHAR_CONST);
INT AM_FLT1054767436(AM_FLTX_CONST);
INT AM_FLT465909163(AM_FLT_CONST);
INT AM_FLT556661152(AM_FLTD_CONST);
INT AM_FLT669520374(AM_FLTDX_CONST);
INT AM_INT2057636940(AM_INT_CONST);
INT AM_INT331860696(AM_INTI_CONST);
INT AM_STR1959493974(AM_STR_CONST);
INT AM_VOI822416442(AM_VOID_CONST);
INT ARRAYI180735351(ARRAYI180735351_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT FLISTS326330076(FLISTSTR);
INT FSTR_sizerINT(FSTR);
INT INTI_c675035669(INTI1, INTI1);
INT INTI_hashrINT(INTI1);
INT INT_hashrINT(INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT STR_hashrINT(STR);
INT STR_se2132153226(STR, STR);
INT STR_sizerINT(STR);
SFILE_ID AM_ARR490952057(AM_ARR_CONST);
SFILE_ID AM_BOO1043294506(AM_BOOL_CONST);
SFILE_ID AM_CHA1773602482(AM_CHAR_CONST);
SFILE_ID AM_FLT1298836824(AM_FLT_CONST);
SFILE_ID AM_FLT1412381806(AM_FLTD_CONST);
SFILE_ID AM_FLT2070042319(AM_FLTDX_CONST);
SFILE_ID AM_FLT2096495882(AM_FLTX_CONST);
SFILE_ID AM_INT1397528579(AM_INT_CONST);
SFILE_ID AM_INT872738218(AM_INTI_CONST);
SFILE_ID AM_STR1580126161(AM_STR_CONST);
SFILE_ID AM_VOI405249940(AM_VOID_CONST);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CONFIG1175493142(CONFIG_ROUT);
STR CONFIG1221082662(CONFIG_DEF);
STR CONFIG133831937(CONFIG_ROUT, ARRAYINT);
STR CONFIG1397479570(CONFIG_TBL, STR, INT);
STR CONFIG1497770745(CONFIG_SCANNER);
STR CONFIG155608225(CONFIG_TBL);
STR CONFIG1745974311(CONFIG_SCANNER);
STR CONFIG336048123(CONFIG_ROUT);
STR CONFIG458671273(CONFIG_SCANNER);
STR CONFIG492273949(CONFIG_ROUT, ARRAYSTR);
STR CONFIG875430285(CONFIG875430285_frame);
STR STR_ap2004550586(STR, STR);
STR STR_as1755591216(STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_pl1270664985(STR, dSTR);
STR STR_prettyrSTR(STR);
dCONFIG CONFIG1181711269(CONFIG1181711269_frame);
dCONFIG FMAPST1785271125(FMAPST1785271125_frame);
dCONFIG FMAPST558704553(FMAPSTRdCONFIG, STR);
dTP AM_ARR925292072(AM_ARR_CONST);
dTP AM_BOO1614359222(AM_BOOL_CONST);
dTP AM_CHA165359522(AM_CHAR_CONST);
dTP AM_FLT1597643896(AM_FLTDX_CONST);
dTP AM_FLT2085750594(AM_FLTD_CONST);
dTP AM_FLT79327789(AM_FLT_CONST);
dTP AM_FLT884139698(AM_FLTX_CONST);
dTP AM_INT1372528330(AM_INTI_CONST);
dTP AM_INT1391542618(AM_INT_CONST);
dTP AM_STR690233376(AM_STR_CONST);
dTP AM_VOI984518984(AM_VOID_CONST);
void CONFIG1316232027(CONFIG_TBL, STR);
void CONFIG1491579599(CONFIG_SCANNER);
void CONFIG1796802473(CONFIG_TBL, STR);
void CONFIG307797953(CONFIG_TBL, STR);
void CONFIG39489600(CONFIG_TBL, STR);
void CONFIG523070343(CONFIG_TBL, STR, INT);
void CONFIG541915266(CONFIG_SCANNER);
void CONFIG_TBL_check(CONFIG_TBL);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ARR_CONST AM_ARR990477897(AM_ARR_CONST self) {
 AM_ARR_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BOOL_CONST AM_BOO158864518(AM_BOOL_CONST self) {
 AM_BOOL_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CHAR_CONST AM_CHA916745766(AM_CHAR_CONST self) {
 AM_CHAR_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FLTDX_CONST AM_FLT822990115(AM_FLTDX_CONST self) {
 AM_FLTDX_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FLTD_CONST AM_FLT1839670182(AM_FLTD_CONST self) {
 AM_FLTD_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FLTX_CONST AM_FLT476428534(AM_FLTX_CONST self) {
 AM_FLTX_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FLT_CONST AM_FLT1362803603(AM_FLT_CONST self) {
 AM_FLT_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_INTI_CONST AM_INT304374138(AM_INTI_CONST self) {
 AM_INTI_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_INT_CONST AM_INT1629629861(AM_INT_CONST self) {
 AM_INT_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_STR_CONST AM_STR1745454041(AM_STR_CONST self) {
 AM_STR_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_VOID_CONST AM_VOI1734446450(AM_VOID_CONST self) {
 AM_VOID_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYINT CONFIG727192651(CONFIG_ROUT self, FLISTSTR e) {
 ARRAYINT ret_val;
 ARRAYINT r;
 ARRAYINT L11;
 INT i = INT_zero;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val1;
 INT L21 = INT_zero;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val2;
 INT L3;
 INT L41_;
 ARRAYINT L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 INT rL91_;
 INT L10;
 INT L121_;
 STR L131_;
 STR L14;
 INT L151_;
 INT L16;
 INT L181_;
 create_self = ((ARRAYINT) NULL);
 L3 = FLISTS326330076(e);
 L41_=INTMINUS(L3,1); 
 create_n = L41_;
 L6 = create_n;
 L8=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L6)*sizeof(INT);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((ARRAYINT) L7);
 L5->asize = L6;
 ret_val1 = L5;
 r = ret_val1;
 {
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = r;
  L91_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   if(L21>=L91_br)  goto after_loop; 
   rL91_=L21; 
   i = rL91_;
   aget_self = e;
   L121_=INTPLUS(i,1); 
   aget_ind = L121_;
   L131_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val2 = L131_;
   L14 = ret_val2;
   L151_=atoi(((STR)L14)->arr_part); 
   L16 = L151_;
   SARR((ARRAYINT)r,i,L16); 
   ;
   L181_=INTPLUS(L21,1); 
   L21 = L181_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CONFIG1467324007(CONFIG_ROUT self, FLISTSTR e) {
 ARRAYSTR ret_val;
 ARRAYSTR r;
 ARRAYSTR L11;
 INT i = INT_zero;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 INT L21 = INT_zero;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val2;
 INT L3;
 INT L41_;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 INT rL91_;
 INT L10;
 INT L121_;
 STR L131_;
 STR L14;
 INT L161_;
 create_self = ((ARRAYSTR) NULL);
 L3 = FLISTS326330076(e);
 L41_=INTMINUS(L3,1); 
 create_n = L41_;
 L6 = create_n;
 L8=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L6)*sizeof(STR);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((ARRAYSTR) L7);
 L5->asize = L6;
 ret_val1 = L5;
 r = ret_val1;
 {
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = r;
  L91_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L21>=L91_br)  goto after_loop; 
   rL91_=L21; 
   i = rL91_;
   aget_self = e;
   L121_=INTPLUS(i,1); 
   aget_ind = L121_;
   L131_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val2 = L131_;
   L14 = ret_val2;
   SARR((ARRAYSTR)r,i,(STR)L14); 
   ;
   L161_=INTPLUS(L21,1); 
   L21 = L161_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ARR495057476(AM_ARR_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_ARR_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ARR62730649(AM_ARR_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 INT i = INT_zero;
 INT L21 = INT_zero;
 ARRAYdAM_CONST L3;
 INT L41_;
 INT L5;
 ARRAYdAM_CONST L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 ARRAYdAM_CONST L14;
 INT L151_;
 INT aL131_;
 INT L16;
 ARRAYdAM_CONST L17;
 dAM_CONST L181_;
 dAM_CONST L19;
 ARRAYdAM_CONST L20;
 dAM_CONST L221_;
 dAM_CONST L23;
 BOOL L24;
 BOOL L251_;
 INT L261_;
 if (a==NULL) {
  goto other146;
 } else
 switch (TAG(a)) {
  case AM_ARR_CONST_tag:
   L3 = ATTR(self,elts);
   L41_=ASIZE((ARRAYdAM_CONST)L3); 
   L5 = L41_;
   L6 = ATTR(((AM_ARR_CONST) a),elts);
   L71_=ASIZE((ARRAYdAM_CONST)L6); 
   L8 = L71_;
   L91_=(L5)==(L8); 
   L10 = L91_;
   L121_=!(L10); 
   if (L121_) {
    ret_val = FALSE;
    return ret_val;
   }
   {
    BOOL f_L131_ = TRUE;
    /* loop index variable */
    L21 = 0;
    L14 = ATTR(self,elts);
    L151_=ASIZE((ARRAYdAM_CONST)L14); 
    L11 = L151_;
    while (1) {
     if(L21>=L11)  goto after_loop; 
     aL131_=L21; 
     i = aL131_;
     L17 = ATTR(self,elts);
     L181_=(dAM_CONST)ARR((ARRAYdAM_CONST)L17,i); 
     L19 = L181_;
     L20 = ATTR(((AM_ARR_CONST) a),elts);
     L221_=(dAM_CONST)ARR((ARRAYdAM_CONST)L20,i); 
     L23 = L19;
     L24 = (*dAM_CO637713226[TAG(L23)])(L23, ((OB) L221_));
     L251_=!(L24); 
     if (L251_) {
      ret_val = FALSE;
      return ret_val;
     }
     L261_=INTPLUS(L21,1); 
     L21 = L261_;
    }
   }
   after_loop: ;
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other146: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BOO1499785931(AM_BOOL_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 if (a==NULL) {
  goto other136;
 } else
 switch (TAG(a)) {
  case AM_BOOL_CONST_tag:
   L1 = ATTR(self,val1);
   L2 = ATTR(((AM_BOOL_CONST) a),val1);
   L31_=L1==L2; 
   ret_val = L31_;
   return ret_val; break;
  default: ;
  other136: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_BOO274293822(AM_BOOL_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_BOOL_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CHA1550967974(AM_CHAR_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_CHAR_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CHA181917731(AM_CHAR_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val1 = BOOL_zero;
 CHAR L1;
 CHAR L2;
 BOOL L31_;
 BOOL L4;
 INT L5;
 BOOL L61_;
 if (a==NULL) {
  goto other137;
 } else
 switch (TAG(a)) {
  case AM_CHAR_CONST_tag:
   if ((ATTR(self,val1)==((INTI1) NULL))) {
    L1 = ATTR(self,bval);
    L2 = ATTR(((AM_CHAR_CONST) a),bval);
    L31_=L1==L2; 
    ret_val = L31_;
    return ret_val;
   }
   else {
    is_eq_self = ATTR(self,val1);
    is_eq_y = ATTR(((AM_CHAR_CONST) a),val1);
    if ((is_eq_self==is_eq_y)) {
     L4 = TRUE;
    } else {
     L5 = INTI_c675035669(is_eq_self, is_eq_y);
     L61_=(L5)==(0); 
     L4 = L61_;
    }
    ret_val1 = L4;
    ret_val = ret_val1;
    return ret_val;
   } break;
  default: ;
  other137: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1108482895(AM_FLTX_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 RAT is_eq_self = RAT_zero;
 RAT is_eq_y = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val2 = BOOL_zero;
 INTI1 is_eq_self2;
 INTI1 is_eq_y2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 INT L6;
 BOOL L71_;
 if (a==NULL) {
  goto other140;
 } else
 switch (TAG(a)) {
  case AM_FLTX_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_FLTX_CONST) a),val1);
   is_eq_self1 = is_eq_self.u;
   is_eq_y1 = is_eq_y.u;
   if ((is_eq_self1==is_eq_y1)) {
    L2 = TRUE;
   } else {
    L3 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L41_=(L3)==(0); 
    L2 = L41_;
   }
   ret_val2 = L2;
   if (ret_val2) {
    is_eq_self2 = is_eq_self.v;
    is_eq_y2 = is_eq_y.v;
    if ((is_eq_self2==is_eq_y2)) {
     L5 = TRUE;
    } else {
     L6 = INTI_c675035669(is_eq_self2, is_eq_y2);
     L71_=(L6)==(0); 
     L5 = L71_;
    }
    ret_val3 = L5;
    L1 = ret_val3;
   } else {
    L1 = FALSE;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other140: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1144488899(AM_FLTD_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 RAT is_eq_self = RAT_zero;
 RAT is_eq_y = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val2 = BOOL_zero;
 INTI1 is_eq_self2;
 INTI1 is_eq_y2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 INT L6;
 BOOL L71_;
 if (a==NULL) {
  goto other139;
 } else
 switch (TAG(a)) {
  case AM_FLTD_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_FLTD_CONST) a),val1);
   is_eq_self1 = is_eq_self.u;
   is_eq_y1 = is_eq_y.u;
   if ((is_eq_self1==is_eq_y1)) {
    L2 = TRUE;
   } else {
    L3 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L41_=(L3)==(0); 
    L2 = L41_;
   }
   ret_val2 = L2;
   if (ret_val2) {
    is_eq_self2 = is_eq_self.v;
    is_eq_y2 = is_eq_y.v;
    if ((is_eq_self2==is_eq_y2)) {
     L5 = TRUE;
    } else {
     L6 = INTI_c675035669(is_eq_self2, is_eq_y2);
     L71_=(L6)==(0); 
     L5 = L71_;
    }
    ret_val3 = L5;
    L1 = ret_val3;
   } else {
    L1 = FALSE;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other139: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1356295273(AM_FLT_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_FLT_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1602735735(AM_FLTDX_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 RAT is_eq_self = RAT_zero;
 RAT is_eq_y = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val2 = BOOL_zero;
 INTI1 is_eq_self2;
 INTI1 is_eq_y2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 INT L6;
 BOOL L71_;
 if (a==NULL) {
  goto other138;
 } else
 switch (TAG(a)) {
  case AM_FLTDX_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_FLTDX_CONST) a),val1);
   is_eq_self1 = is_eq_self.u;
   is_eq_y1 = is_eq_y.u;
   if ((is_eq_self1==is_eq_y1)) {
    L2 = TRUE;
   } else {
    L3 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L41_=(L3)==(0); 
    L2 = L41_;
   }
   ret_val2 = L2;
   if (ret_val2) {
    is_eq_self2 = is_eq_self.v;
    is_eq_y2 = is_eq_y.v;
    if ((is_eq_self2==is_eq_y2)) {
     L5 = TRUE;
    } else {
     L6 = INTI_c675035669(is_eq_self2, is_eq_y2);
     L71_=(L6)==(0); 
     L5 = L71_;
    }
    ret_val3 = L5;
    L1 = ret_val3;
   } else {
    L1 = FALSE;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other138: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1739286436(AM_FLT_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 RAT is_eq_self = RAT_zero;
 RAT is_eq_y = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val2 = BOOL_zero;
 INTI1 is_eq_self2;
 INTI1 is_eq_y2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 INT L6;
 BOOL L71_;
 if (a==NULL) {
  goto other141;
 } else
 switch (TAG(a)) {
  case AM_FLT_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_FLT_CONST) a),val1);
   is_eq_self1 = is_eq_self.u;
   is_eq_y1 = is_eq_y.u;
   if ((is_eq_self1==is_eq_y1)) {
    L2 = TRUE;
   } else {
    L3 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L41_=(L3)==(0); 
    L2 = L41_;
   }
   ret_val2 = L2;
   if (ret_val2) {
    is_eq_self2 = is_eq_self.v;
    is_eq_y2 = is_eq_y.v;
    if ((is_eq_self2==is_eq_y2)) {
     L5 = TRUE;
    } else {
     L6 = INTI_c675035669(is_eq_self2, is_eq_y2);
     L71_=(L6)==(0); 
     L5 = L71_;
    }
    ret_val3 = L5;
    L1 = ret_val3;
   } else {
    L1 = FALSE;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other141: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1865244436(AM_FLTDX_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_FLTDX_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT652620710(AM_FLTX_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_FLTX_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT93541882(AM_FLTD_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_FLTD_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_INT1418540518(AM_INT_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_INT_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_INT1567340277(AM_INT_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 INT L2;
 BOOL L31_;
 if (a==NULL) {
  goto other143;
 } else
 switch (TAG(a)) {
  case AM_INT_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_INT_CONST) a),val1);
   if ((is_eq_self==is_eq_y)) {
    L1 = TRUE;
   } else {
    L2 = INTI_c675035669(is_eq_self, is_eq_y);
    L31_=(L2)==(0); 
    L1 = L31_;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other143: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_INT341842242(AM_INTI_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_INTI_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_INT586605643(AM_INTI_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 INT L2;
 BOOL L31_;
 if (a==NULL) {
  goto other142;
 } else
 switch (TAG(a)) {
  case AM_INTI_CONST_tag:
   is_eq_self = ATTR(self,val1);
   is_eq_y = ATTR(((AM_INTI_CONST) a),val1);
   if ((is_eq_self==is_eq_y)) {
    L1 = TRUE;
   } else {
    L2 = INTI_c675035669(is_eq_self, is_eq_y);
    L31_=(L2)==(0); 
    L1 = L31_;
   }
   ret_val1 = L1;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other142: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_STR1095405116(AM_STR_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_STR_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_STR2039863889(AM_STR_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 if (a==NULL) {
  goto other144;
 } else
 switch (TAG(a)) {
  case AM_STR_CONST_tag:
   ret_val = STR_is111530248(ATTR(self,bval), ATTR(((AM_STR_CONST) a),bval));
   return ret_val; break;
  default: ;
  other144: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VOI1214668736(AM_VOID_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_VOID_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VOI2144357369(AM_VOID_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 if (a==NULL) {
  goto other145;
 } else
 switch (TAG(a)) {
  case AM_VOID_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other145: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CONFIG1490450026(CONFIG_ROUT self, FLISTFLISTSTR exprs) {
 BOOL ret_val = BOOL_zero;
 FLISTFLISTSTR L11;
 STR a;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 FLISTSTR L2;
 STR L31_;
 BOOL L4;
 extern STR iter1;
 extern STR break2;
 {
  struct FLISTF1766102529_frame_struct other1_0;
FLISTF1766102529_frame noname1 = &other1_0;
  L11 = exprs;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTF1766102529(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   aget_self = L2;
   aget_ind = 0;
   L31_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val1 = L31_;
   a = ret_val1;
   if (STR_is111530248(a, ((STR) &iter1))) {
    L4 = TRUE;
   } else {
    L4 = STR_is111530248(a, ((STR) &break2));
   }
   if (L4) {
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

BOOL CONFIG1864245349(CONFIG_SCANNER self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val3 = BOOL_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 CHAR L101_;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 CHAR L181_;
 INT L191_;
 INT L20;
 INT L211_;
 INT L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 eof_self = self;
 L3 = ATTR(eof_self,next);
 L4 = FSTR_sizerINT(ATTR(eof_self,fstr));
 L51_=(L3)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 ret_val1 = L71_;
 L8 = ret_val1;
 L91_=!(L8); 
 if (L91_) {
  aget_self = ATTR(self,fstr);
  aget_ind = ATTR(self,next);
  L101_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L101_;
  is_lt_self = ret_val2;
  is_lt_c = 'A';
  L111_=(INT)((unsigned char)is_lt_self); 
  L12 = L111_;
  L131_=(INT)((unsigned char)is_lt_c); 
  L14 = L131_;
  L151_=(L12)<(L14); 
  ret_val3 = L151_;
  L16 = ret_val3;
  L171_=!(L16); 
  L2 = L171_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  is_lt_self1 = 'Z';
  aget_self1 = ATTR(self,fstr);
  aget_ind1 = ATTR(self,next);
  L181_=ARR((FSTR)aget_self1,aget_ind1); 
  ret_val4 = L181_;
  is_lt_c1 = ret_val4;
  L191_=(INT)((unsigned char)is_lt_self1); 
  L20 = L191_;
  L211_=(INT)((unsigned char)is_lt_c1); 
  L22 = L211_;
  L231_=(L20)<(L22); 
  ret_val5 = L231_;
  L24 = ret_val5;
  L251_=!(L24); 
  L1 = L251_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CONFIG298217444(CONFIG_SCANNER self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val5 = CHAR_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L11;
 CHAR L121_;
 CHAR L13;
 BOOL L141_;
 CHAR L151_;
 CHAR L16;
 BOOL L171_;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 CHAR L211_;
 CHAR L22;
 BOOL L231_;
 eof_self = self;
 L2 = ATTR(eof_self,next);
 L3 = FSTR_sizerINT(ATTR(eof_self,fstr));
 L41_=(L2)<(L3); 
 L5 = L41_;
 L61_=!(L5); 
 ret_val1 = L61_;
 L7 = ret_val1;
 L81_=!(L7); 
 if (L81_) {
  aget_self = ATTR(self,fstr);
  aget_ind = ATTR(self,next);
  L121_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L121_;
  L13 = ret_val2;
  L141_=L13==' '; 
  if (L141_) {
   L11 = TRUE;
  } else {
   aget_self1 = ATTR(self,fstr);
   aget_ind1 = ATTR(self,next);
   L151_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val3 = L151_;
   L16 = ret_val3;
   L171_=L16=='\t'; 
   L11 = L171_;
  }
  if (L11) {
   L10 = TRUE;
  } else {
   aget_self2 = ATTR(self,fstr);
   aget_ind2 = ATTR(self,next);
   L181_=ARR((FSTR)aget_self2,aget_ind2); 
   ret_val4 = L181_;
   L19 = ret_val4;
   L201_=L19=='\n'; 
   L10 = L201_;
  }
  if (L10) {
   L9 = TRUE;
  } else {
   aget_self3 = ATTR(self,fstr);
   aget_ind3 = ATTR(self,next);
   L211_=ARR((FSTR)aget_self3,aget_ind3); 
   ret_val5 = L211_;
   L22 = ret_val5;
   L231_=L22=='\r'; 
   L9 = L231_;
  }
  L1 = L9;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CONFIG299011603(CONFIG_SCANNER self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 CHAR L111_;
 CHAR L12;
 BOOL L131_;
 INT L14;
 INT L151_;
 INT L16;
 FSTR L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 INT L21;
 INT L221_;
 CHAR L231_;
 CHAR L24;
 BOOL L251_;
 eof_self = self;
 L4 = ATTR(eof_self,next);
 L5 = FSTR_sizerINT(ATTR(eof_self,fstr));
 L61_=(L4)<(L5); 
 L7 = L61_;
 L81_=!(L7); 
 ret_val1 = L81_;
 L9 = ret_val1;
 L101_=!(L9); 
 if (L101_) {
  aget_self = ATTR(self,fstr);
  aget_ind = ATTR(self,next);
  L111_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L111_;
  L12 = ret_val2;
  L131_=L12=='-'; 
  L3 = L131_;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  L14 = ATTR(self,next);
  L151_=INTPLUS(L14,1); 
  L16 = L151_;
  L17 = ATTR(self,fstr);
  L181_=ASIZE((FSTR)L17); 
  L19 = L181_;
  L201_=(L16)<(L19); 
  L2 = L201_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  aget_self1 = ATTR(self,fstr);
  L21 = ATTR(self,next);
  L221_=INTPLUS(L21,1); 
  aget_ind1 = L221_;
  L231_=ARR((FSTR)aget_self1,aget_ind1); 
  ret_val3 = L231_;
  L24 = ret_val3;
  L251_=L24=='-'; 
  L1 = L251_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CONFIG536838194(CONFIG_ROUT self, FLISTFLISTSTR exprs) {
 BOOL ret_val = BOOL_zero;
 FLISTFLISTSTR L11;
 STR a;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 FLISTSTR L2;
 STR L31_;
 BOOL L4;
 BOOL L5;
 extern STR exec1;
 extern STR reads1;
 extern STR writes1;
 {
  struct FLISTF1766102529_frame_struct other1_0;
FLISTF1766102529_frame noname1 = &other1_0;
  L11 = exprs;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTF1766102529(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   aget_self = L2;
   aget_ind = 0;
   L31_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val1 = L31_;
   a = ret_val1;
   if (STR_is111530248(a, ((STR) &exec1))) {
    L5 = TRUE;
   } else {
    L5 = STR_is111530248(a, ((STR) &reads1));
   }
   if (L5) {
    L4 = TRUE;
   } else {
    L4 = STR_is111530248(a, ((STR) &writes1));
   }
   if (L4) {
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

BOOL CONFIG915316977(CONFIG_SCANNER self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val3 = BOOL_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 CHAR L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 CHAR L191_;
 INT L201_;
 INT L21;
 INT L221_;
 INT L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 eof_self = self;
 L3 = ATTR(eof_self,next);
 L4 = FSTR_sizerINT(ATTR(eof_self,fstr));
 L51_=(L3)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 ret_val1 = L71_;
 L8 = ret_val1;
 L91_=!(L8); 
 if (L91_) {
  L2 = CONFIG1864245349(self);
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L1 = TRUE;
 } else {
  aget_self = ATTR(self,fstr);
  aget_ind = ATTR(self,next);
  L111_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L111_;
  is_lt_self = ret_val2;
  is_lt_c = 'a';
  L121_=(INT)((unsigned char)is_lt_self); 
  L13 = L121_;
  L141_=(INT)((unsigned char)is_lt_c); 
  L15 = L141_;
  L161_=(L13)<(L15); 
  ret_val3 = L161_;
  L17 = ret_val3;
  L181_=!(L17); 
  if (L181_) {
   is_lt_self1 = 'z';
   aget_self1 = ATTR(self,fstr);
   aget_ind1 = ATTR(self,next);
   L191_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val4 = L191_;
   is_lt_c1 = ret_val4;
   L201_=(INT)((unsigned char)is_lt_self1); 
   L21 = L201_;
   L221_=(INT)((unsigned char)is_lt_c1); 
   L23 = L221_;
   L241_=(L21)<(L23); 
   ret_val5 = L241_;
   L25 = ret_val5;
   L261_=!(L25); 
   L10 = L261_;
  } else {
   L10 = FALSE;
  }
  L1 = L10;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CONFIG981438152(CONFIG_SCANNER self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val3 = BOOL_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val5 = BOOL_zero;
 BOOL L1;
 BOOL L2;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 CHAR L101_;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 CHAR L181_;
 INT L191_;
 INT L20;
 INT L211_;
 INT L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 eof_self = self;
 L3 = ATTR(eof_self,next);
 L4 = FSTR_sizerINT(ATTR(eof_self,fstr));
 L51_=(L3)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 ret_val1 = L71_;
 L8 = ret_val1;
 L91_=!(L8); 
 if (L91_) {
  aget_self = ATTR(self,fstr);
  aget_ind = ATTR(self,next);
  L101_=ARR((FSTR)aget_self,aget_ind); 
  ret_val2 = L101_;
  is_lt_self = ret_val2;
  is_lt_c = '0';
  L111_=(INT)((unsigned char)is_lt_self); 
  L12 = L111_;
  L131_=(INT)((unsigned char)is_lt_c); 
  L14 = L131_;
  L151_=(L12)<(L14); 
  ret_val3 = L151_;
  L16 = ret_val3;
  L171_=!(L16); 
  L2 = L171_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  is_lt_self1 = '9';
  aget_self1 = ATTR(self,fstr);
  aget_ind1 = ATTR(self,next);
  L181_=ARR((FSTR)aget_self1,aget_ind1); 
  ret_val4 = L181_;
  is_lt_c1 = ret_val4;
  L191_=(INT)((unsigned char)is_lt_self1); 
  L20 = L191_;
  L211_=(INT)((unsigned char)is_lt_c1); 
  L22 = L211_;
  L231_=(L20)<(L22); 
  ret_val5 = L231_;
  L24 = ret_val5;
  L251_=!(L24); 
  L1 = L251_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CONFIG_DEF CONFIG1152920950(CONFIG_TBL self, STR s) {
 CONFIG_DEF ret_val;
 dCONFIG r;
 r = FMAPST558704553(ATTR(self,tbl), s);
 if (r==NULL) {
  goto other99;
 } else
 switch (TAG(r)) {
  case CONFIG_DEF_tag:
   ret_val = ((CONFIG_DEF) r);
   return ret_val; break;
  default: ;
  other99: ;
   ret_val = ((CONFIG_DEF) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CONFIG_ROUT CONFIG525418741(CONFIG_TBL self, STR s) {
 CONFIG_ROUT ret_val;
 dCONFIG r;
 r = FMAPST558704553(ATTR(self,tbl), s);
 if (r==NULL) {
  goto other116;
 } else
 switch (TAG(r)) {
  case CONFIG_ROUT_tag:
   ret_val = ((CONFIG_ROUT) r);
   return ret_val; break;
  default: ;
  other116: ;
   ret_val = ((CONFIG_ROUT) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CONFIG_ROUT CONFIG725916253(CONFIG_ROUT self, STR id, FLISTFLISTSTR exprs) {
 CONFIG_ROUT ret_val;
 CONFIG_ROUT r;
 FLISTFLISTSTR L11;
 FLISTSTR e;
 STR l;
 ARRAYSTR L21;
 ARRAYSTR L31;
 ARRAYSTR L41;
 ARRAYSTR L51;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 OUT create_self;
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
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r3;
 UNIX exit_self;
 INT exit_code = INT_zero;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 CONFIG_ROUT L10;
 OB L12;
 FLISTSTR L13;
 STR L141_;
 BOOL L15;
 extern STR volatile2;
 INT L16;
 BOOL L171_;
 BOOL L18;
 extern STR fragile1;
 INT L19;
 BOOL L201_;
 BOOL L22;
 extern STR no_pre1;
 INT L23;
 BOOL L241_;
 BOOL L25;
 extern STR no_post1;
 INT L26;
 BOOL L271_;
 BOOL L28;
 extern STR import1;
 INT L29;
 BOOL L301_;
 BOOL L32;
 extern STR export2;
 INT L33;
 BOOL L341_;
 BOOL L35;
 extern STR block2;
 INT L36;
 BOOL L371_;
 BOOL L38;
 extern STR arith1;
 INT L39;
 BOOL L401_;
 BOOL L42;
 extern STR raises_any1;
 INT L43;
 BOOL L441_;
 BOOL L45;
 extern STR reads_any1;
 INT L46;
 BOOL L471_;
 BOOL L48;
 extern STR writes_any1;
 INT L49;
 BOOL L501_;
 extern STR raises1;
 extern STR reads1;
 extern STR writes1;
 extern STR attr_access;
 extern STR near_depends_on;
 BOOL L52;
 extern STR is_near1;
 INT L53;
 BOOL L541_;
 extern STR declare1;
 extern STR var2;
 extern STR exec1;
 extern STR init1;
 extern STR temp2;
 extern STR break2;
 extern STR iter1;
 extern STR f_declare;
 extern STR f_var;
 extern STR f_exec;
 extern STR f_init;
 extern STR f_temp;
 extern STR f_break;
 extern STR f_iter;
 extern STR CONFIG978467271;
 FILE1 L55;
 OB L56;
 FILE1 L58;
 OB L59;
 extern STR isnotl1509303335;
 FILE1 L62;
 OB L63;
 BOOL L65;
 BOOL L661_;
 BOOL L67;
 BOOL L681_;
 INT L691_br;
 STR aL691_;
 STR L70;
 STR L72;
 extern STR d2;
 INT L73;
 BOOL L741_;
 BOOL L75;
 BOOL L761_;
 INT L771_;
 BOOL L78;
 BOOL L79;
 BOOL L801_;
 BOOL L82;
 BOOL L831_;
 INT L841_br;
 STR aL841_;
 STR L85;
 STR L86;
 extern STR d2;
 INT L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 INT L921_;
 BOOL L93;
 BOOL L941_;
 INT L951_br;
 STR aL951_;
 STR L96;
 STR L97;
 extern STR d2;
 INT L98;
 BOOL L991_;
 BOOL L100;
 BOOL L1011_;
 INT L1021_;
 BOOL L103;
 BOOL L104;
 BOOL L1051_;
 BOOL L106;
 BOOL L1071_;
 INT L1081_br;
 STR aL1081_;
 STR L109;
 STR L110;
 extern STR d2;
 INT L111;
 BOOL L1121_;
 BOOL L113;
 BOOL L1141_;
 INT L1151_;
 L12=ZALLOC(sizeof(struct CONFIG_ROUT_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=CONFIG_ROUT_tag;
 L10 = ((CONFIG_ROUT) L12);
 r = L10;
 SATTR(r,name1,id);
 {
  struct FLISTF1766102529_frame_struct other1_0;
FLISTF1766102529_frame noname1 = &other1_0;
  L11 = exprs;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L13 = FLISTF1766102529(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L13;
   aget_self = e;
   aget_ind = 0;
   L141_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val1 = L141_;
   l = ret_val1;
   if (STR_is111530248(l, ((STR) &volatile2))) {
    L16 = FLISTS326330076(e);
    L171_=(L16)==(1); 
    L15 = L171_;
   } else {
    L15 = FALSE;
   }
   if (L15) {
    SATTR(r,volatile1,TRUE);
   }
   else {
    if (STR_is111530248(l, ((STR) &fragile1))) {
     L19 = FLISTS326330076(e);
     L201_=(L19)==(1); 
     L18 = L201_;
    } else {
     L18 = FALSE;
    }
    if (L18) {
     SATTR(r,fragile,TRUE);
    }
    else {
     if (STR_is111530248(l, ((STR) &no_pre1))) {
      L23 = FLISTS326330076(e);
      L241_=(L23)==(1); 
      L22 = L241_;
     } else {
      L22 = FALSE;
     }
     if (L22) {
      SATTR(r,no_pre,TRUE);
     }
     else {
      if (STR_is111530248(l, ((STR) &no_post1))) {
       L26 = FLISTS326330076(e);
       L271_=(L26)==(1); 
       L25 = L271_;
      } else {
       L25 = FALSE;
      }
      if (L25) {
       SATTR(r,no_post,TRUE);
      }
      else {
       if (STR_is111530248(l, ((STR) &import1))) {
        L29 = FLISTS326330076(e);
        L301_=(L29)==(1); 
        L28 = L301_;
       } else {
        L28 = FALSE;
       }
       if (L28) {
        SATTR(r,does_import,TRUE);
       }
       else {
        if (STR_is111530248(l, ((STR) &export2))) {
         L33 = FLISTS326330076(e);
         L341_=(L33)==(1); 
         L32 = L341_;
        } else {
         L32 = FALSE;
        }
        if (L32) {
         SATTR(r,does_export,TRUE);
        }
        else {
         if (STR_is111530248(l, ((STR) &block2))) {
          L36 = FLISTS326330076(e);
          L371_=(L36)==(1); 
          L35 = L371_;
         } else {
          L35 = FALSE;
         }
         if (L35) {
          SATTR(r,block1,TRUE);
         }
         else {
          if (STR_is111530248(l, ((STR) &arith1))) {
           L39 = FLISTS326330076(e);
           L401_=(L39)==(1); 
           L38 = L401_;
          } else {
           L38 = FALSE;
          }
          if (L38) {
           SATTR(r,arith,TRUE);
          }
          else {
           if (STR_is111530248(l, ((STR) &raises_any1))) {
            L43 = FLISTS326330076(e);
            L441_=(L43)==(1); 
            L42 = L441_;
           } else {
            L42 = FALSE;
           }
           if (L42) {
            SATTR(r,raises_any,TRUE);
           }
           else {
            if (STR_is111530248(l, ((STR) &reads_any1))) {
             L46 = FLISTS326330076(e);
             L471_=(L46)==(1); 
             L45 = L471_;
            } else {
             L45 = FALSE;
            }
            if (L45) {
             SATTR(r,reads_any,TRUE);
            }
            else {
             if (STR_is111530248(l, ((STR) &writes_any1))) {
              L49 = FLISTS326330076(e);
              L501_=(L49)==(1); 
              L48 = L501_;
             } else {
              L48 = FALSE;
             }
             if (L48) {
              SATTR(r,writes_any,TRUE);
             }
             else {
              if (STR_is111530248(l, ((STR) &raises1))) {
               SATTR(r,raises,CONFIG1467324007(self, e));
              }
              else {
               if (STR_is111530248(l, ((STR) &reads1))) {
                SATTR(r,reads,CONFIG1467324007(self, e));
               }
               else {
                if (STR_is111530248(l, ((STR) &writes1))) {
                 SATTR(r,writes,CONFIG1467324007(self, e));
                }
                else {
                 if (STR_is111530248(l, ((STR) &attr_access))) {
                  SATTR(r,attr_access,CONFIG727192651(self, e));
                 }
                 else {
                  if (STR_is111530248(l, ((STR) &near_depends_on))) {
                   SATTR(r,near_depends_on,CONFIG727192651(self, e));
                  }
                  else {
                   if (STR_is111530248(l, ((STR) &is_near1))) {
                    L53 = FLISTS326330076(e);
                    L541_=(L53)==(1); 
                    L52 = L541_;
                   } else {
                    L52 = FALSE;
                   }
                   if (L52) {
                    SATTR(r,is_near,TRUE);
                   }
                   else {
                    if (STR_is111530248(l, ((STR) &declare1))) {
                     SATTR(r,declare,CONFIG1467324007(self, e));
                    }
                    else {
                     if (STR_is111530248(l, ((STR) &var2))) {
                      SATTR(r,var,CONFIG1467324007(self, e));
                     }
                     else {
                      if (STR_is111530248(l, ((STR) &exec1))) {
                       SATTR(r,exec,CONFIG1467324007(self, e));
                      }
                      else {
                       if (STR_is111530248(l, ((STR) &init1))) {
                        SATTR(r,init,CONFIG1467324007(self, e));
                       }
                       else {
                        if (STR_is111530248(l, ((STR) &temp2))) {
                         SATTR(r,temp1,CONFIG1467324007(self, e));
                        }
                        else {
                         if (STR_is111530248(l, ((STR) &break2))) {
                          SATTR(r,break1,CONFIG1467324007(self, e));
                         }
                         else {
                          if (STR_is111530248(l, ((STR) &iter1))) {
                           SATTR(r,iter,CONFIG1467324007(self, e));
                          }
                          else {
                           if (STR_is111530248(l, ((STR) &f_declare))) {
                            SATTR(r,f_declare,CONFIG1467324007(self, e));
                           }
                           else {
                            if (STR_is111530248(l, ((STR) &f_var))) {
                             SATTR(r,f_var,CONFIG1467324007(self, e));
                            }
                            else {
                             if (STR_is111530248(l, ((STR) &f_exec))) {
                              SATTR(r,f_exec,CONFIG1467324007(self, e));
                             }
                             else {
                              if (STR_is111530248(l, ((STR) &f_init))) {
                               SATTR(r,f_init,CONFIG1467324007(self, e));
                              }
                              else {
                               if (STR_is111530248(l, ((STR) &f_temp))) {
                                SATTR(r,f_temp,CONFIG1467324007(self, e));
                               }
                               else {
                                if (STR_is111530248(l, ((STR) &f_break))) {
                                 SATTR(r,f_break,CONFIG1467324007(self, e));
                                }
                                else {
                                 if (STR_is111530248(l, ((STR) &f_iter))) {
                                  SATTR(r,f_iter,CONFIG1467324007(self, e));
                                 }
                                 else {
                                  create_self = ((OUT) NULL);
                                  ret_val2 = create_self;
                                  plus_self = ret_val2;
                                  plus_s = ((STR) &CONFIG978467271);
                                  stdout_self = ((FILE1) NULL);
                                  L56=ZALLOC(sizeof(struct FILE1_struct));
                                  if (L56==NULL) FATAL("Unable to allocate more memory");
                                  ((OB)L56)->header.tag=FILE1_tag;
                                  L55 = ((FILE1) L56);
                                  r1 = L55;
                                  
                                  SATTR(r1,fp,stdout);
                                  ret_val4 = r1;
                                  FILE_plus_STR(ret_val4, plus_s);
                                  ret_val3 = plus_self;
                                  plus_self1 = ret_val3;
                                  plus_s1 = l;
                                  stdout_self1 = ((FILE1) NULL);
                                  L59=ZALLOC(sizeof(struct FILE1_struct));
                                  if (L59==NULL) FATAL("Unable to allocate more memory");
                                  ((OB)L59)->header.tag=FILE1_tag;
                                  L58 = ((FILE1) L59);
                                  r2 = L58;
                                  
                                  SATTR(r2,fp,stdout);
                                  ret_val6 = r2;
                                  FILE_plus_STR(ret_val6, plus_s1);
                                  ret_val5 = plus_self1;
                                  plus_self2 = ret_val5;
                                  plus_s2 = ((STR) &isnotl1509303335);
                                  stdout_self2 = ((FILE1) NULL);
                                  L63=ZALLOC(sizeof(struct FILE1_struct));
                                  if (L63==NULL) FATAL("Unable to allocate more memory");
                                  ((OB)L63)->header.tag=FILE1_tag;
                                  L62 = ((FILE1) L63);
                                  r3 = L62;
                                  
                                  SATTR(r3,fp,stdout);
                                  ret_val7 = r3;
                                  FILE_plus_STR(ret_val7, plus_s2);
                                  exit_self = ((UNIX) NULL);
                                  exit_code = -1;
                                  exit(exit_code);
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
 after_loop: ;
 L65 = (ATTR(r,break1)==((ARRAYSTR) NULL));
 L661_=!(L65); 
 if (L661_) {
  SATTR(r,use_index,TRUE);
 }
 else {
  L67 = (ATTR(r,init)==((ARRAYSTR) NULL));
  L681_=!(L67); 
  if (L681_) {
   {
    BOOL f_L691_ = TRUE;
    /* loop index variable */
    L61 = 0;
    L21 = ATTR(r,init);
    L691_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
    while (1) {
     if(L61>=L691_br)  goto after_loop1; 
     aL691_=ARR((ARRAYSTR)L21,L61); 
     L72 = aL691_;
     L73 = STR_se2132153226(L72, ((STR) &d2));
     L741_=(L73)<(0); 
     L75 = L741_;
     L761_=!(L75); 
     if (L761_) {
      SATTR(r,use_index,TRUE);
      goto after_loop1;
     }
     L771_=INTPLUS(L61,1); 
     L61 = L771_;
    }
   }
   after_loop1: ;
  }
  L79 = ATTR(r,use_index);
  L801_=!(L79); 
  if (L801_) {
   L82 = (ATTR(r,iter)==((ARRAYSTR) NULL));
   L831_=!(L82); 
   L78 = L831_;
  } else {
   L78 = FALSE;
  }
  if (L78) {
   {
    BOOL f_L841_ = TRUE;
    /* loop index variable */
    L71 = 0;
    L31 = ATTR(r,iter);
    L841_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
    while (1) {
     if(L71>=L841_br)  goto after_loop2; 
     aL841_=ARR((ARRAYSTR)L31,L71); 
     L86 = aL841_;
     L87 = STR_se2132153226(L86, ((STR) &d2));
     L881_=(L87)<(0); 
     L89 = L881_;
     L901_=!(L89); 
     if (L901_) {
      SATTR(r,use_index,TRUE);
      goto after_loop2;
     }
     L921_=INTPLUS(L71,1); 
     L71 = L921_;
    }
   }
   after_loop2: ;
  }
  L93 = (ATTR(r,f_init)==((ARRAYSTR) NULL));
  L941_=!(L93); 
  if (L941_) {
   {
    BOOL f_L951_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L41 = ATTR(r,f_init);
    L951_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
    while (1) {
     if(L81>=L951_br)  goto after_loop3; 
     aL951_=ARR((ARRAYSTR)L41,L81); 
     L97 = aL951_;
     L98 = STR_se2132153226(L97, ((STR) &d2));
     L991_=(L98)<(0); 
     L100 = L991_;
     L1011_=!(L100); 
     if (L1011_) {
      SATTR(r,use_index,TRUE);
      goto after_loop3;
     }
     L1021_=INTPLUS(L81,1); 
     L81 = L1021_;
    }
   }
   after_loop3: ;
  }
  L104 = ATTR(r,use_index);
  L1051_=!(L104); 
  if (L1051_) {
   L106 = (ATTR(r,f_iter)==((ARRAYSTR) NULL));
   L1071_=!(L106); 
   L103 = L1071_;
  } else {
   L103 = FALSE;
  }
  if (L103) {
   {
    BOOL f_L1081_ = TRUE;
    /* loop index variable */
    L91 = 0;
    L51 = ATTR(r,f_iter);
    L1081_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
    while (1) {
     if(L91>=L1081_br)  goto after_loop4; 
     aL1081_=ARR((ARRAYSTR)L51,L91); 
     L110 = aL1081_;
     L111 = STR_se2132153226(L110, ((STR) &d2));
     L1121_=(L111)<(0); 
     L113 = L1121_;
     L1141_=!(L113); 
     if (L1141_) {
      SATTR(r,use_index,TRUE);
      goto after_loop4;
     }
     L1151_=INTPLUS(L91,1); 
     L91 = L1151_;
    }
   }
   after_loop4: ;
  }
 }
 if ((ATTR(r,f_iter)==((ARRAYSTR) NULL))) {
  SATTR(r,f_iter,ATTR(r,iter));
 }
 if ((ATTR(r,f_break)==((ARRAYSTR) NULL))) {
  SATTR(r,f_break,ATTR(r,break1));
 }
 if ((ATTR(r,f_var)==((ARRAYSTR) NULL))) {
  SATTR(r,f_var,ATTR(r,var));
 }
 if ((ATTR(r,f_exec)==((ARRAYSTR) NULL))) {
  SATTR(r,f_exec,ATTR(r,exec));
 }
 if ((ATTR(r,f_declare)==((ARRAYSTR) NULL))) {
  SATTR(r,f_declare,ATTR(r,declare));
 }
 if ((ATTR(r,f_temp)==((ARRAYSTR) NULL))) {
  SATTR(r,f_temp,ATTR(r,temp1));
 }
 if ((ATTR(r,f_init)==((ARRAYSTR) NULL))) {
  SATTR(r,f_init,ATTR(r,init));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CONFIG_SCANNER CONFIG647536087(CONFIG_SCANNER self, STR filename) {
 CONFIG_SCANNER ret_val;
 BFILE f;
 CONFIG_SCANNER r;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val1;
 BFILE r1;
 BFILE error_self;
 BOOL ret_val2 = BOOL_zero;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 OUT ret_val4;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r2;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val6;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r3;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 BFILE close_self;
 BFILE L1;
 OB L2;
 extern STR rb;
 STR L3;
 STR L4;
 BOOL L5;
 extern STR cannot1032360751;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR forreading;
 FILE1 L12;
 OB L13;
 CONFIG_SCANNER L15;
 OB L16;
 open_for_read_se = ((BFILE) NULL);
 open_for_read_nm = filename;
 L2=ZALLOC(sizeof(struct BFILE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=BFILE_tag;
 L1 = ((BFILE) L2);
 r1 = L1;
 L3 = open_for_read_nm;
 L4 = ((STR) &rb);
 SATTR(r1,fp,fopen(((L3==NULL)?NULL:L3->arr_part), ((L4==NULL)?NULL:L4->arr_part)));
 ret_val1 = r1;
 f = ret_val1;
 error_self = f;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L5 = TRUE;
 } else {
  L5 = ferror(ATTR(error_self,fp));
 }
 ret_val2 = L5;
 if (ret_val2) {
  create_self = ((OUT) NULL);
  ret_val3 = create_self;
  plus_self = ret_val3;
  plus_s = ((STR) &cannot1032360751);
  stdout_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r2 = L6;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s);
  ret_val4 = plus_self;
  plus_self1 = ret_val4;
  plus_s1 = filename;
  stdout_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r3 = L9;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s1);
  ret_val6 = plus_self1;
  plus_self2 = ret_val6;
  plus_s2 = ((STR) &forreading);
  stdout_self2 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r4 = L12;
  
  SATTR(r4,fp,stdout);
  ret_val8 = r4;
  FILE_plus_STR(ret_val8, plus_s2);
  exit_self = ((UNIX) NULL);
  exit_code = -1;
  exit(exit_code);
 }
 L16=ZALLOC(sizeof(struct CONFIG_SCANNER_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=CONFIG_SCANNER_tag;
 L15 = ((CONFIG_SCANNER) L16);
 r = L15;
 SATTR(r,fstr,BFILE_fstrrFSTR(f));
 SATTR(r,file1,filename);
 close_self = f;
 fclose(ATTR(close_self,fp));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ARR1270029884(AM_ARR_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_BOO1622702250(AM_BOOL_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_CHA722016210(AM_CHAR_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FLT1309796914(AM_FLTD_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FLT1728105754(AM_FLTX_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FLT1832811964(AM_FLTDX_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FLT945201335(AM_FLT_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_INT1223291058(AM_INT_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_INT2023229226(AM_INTI_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_STR201189492(AM_STR_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_VOI402497764(AM_VOID_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR CONFIG666119611(CONFIG_SCANNER self) {
 FLISTSTR ret_val;
 FLISTSTR r = ((FLISTSTR) NULL);
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 CONFIG_SCANNER is_string_self;
 BOOL ret_val4 = BOOL_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val5 = CHAR_zero;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val6 = CHAR_zero;
 FSTR aget_self5;
 INT aget_ind5 = INT_zero;
 CHAR ret_val7 = CHAR_zero;
 FSTR aget_self6;
 INT aget_ind6 = INT_zero;
 CHAR ret_val8 = CHAR_zero;
 FSTR aget_self7;
 INT aget_ind7 = INT_zero;
 CHAR ret_val9 = CHAR_zero;
 CONFIG_SCANNER is_string_self1;
 BOOL ret_val10 = BOOL_zero;
 FSTR aget_self8;
 INT aget_ind8 = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aget_self9;
 INT aget_ind9 = INT_zero;
 CHAR ret_val12 = CHAR_zero;
 FSTR aget_self10;
 INT aget_ind10 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR aget_self11;
 INT aget_ind11 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 FSTR aget_self12;
 INT aget_ind12 = INT_zero;
 CHAR ret_val15 = CHAR_zero;
 OUT create_self;
 OUT ret_val16;
 OUT plus_self;
 STR plus_s;
 OUT ret_val17;
 FILE1 stdout_self;
 FILE1 ret_val18;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val19;
 FILE1 stdout_self1;
 FILE1 ret_val20;
 FILE1 r2;
 OUT plus_self2;
 dSTR plus_s2;
 OUT ret_val21;
 FILE1 stdout_self2;
 FILE1 ret_val22;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val23;
 FILE1 stdout_self3;
 FILE1 ret_val24;
 FILE1 r4;
 FSTR aget_self13;
 INT aget_ind13 = INT_zero;
 CHAR ret_val25 = CHAR_zero;
 OUT plus_self4;
 dSTR plus_s4;
 OUT ret_val26;
 FILE1 stdout_self4;
 FILE1 ret_val27;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val28;
 FILE1 r6;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 BOOL L4;
 BOOL L5;
 CHAR L61_;
 CHAR L7;
 BOOL L81_;
 CHAR L91_;
 CHAR L10;
 BOOL L121_;
 CHAR L131_;
 CHAR L14;
 BOOL L151_;
 BOOL L16;
 CHAR L171_;
 CHAR L18;
 BOOL L191_;
 BOOL L20;
 BOOL L211_;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 BOOL L27;
 BOOL L28;
 CHAR L291_;
 CHAR L30;
 BOOL L321_;
 CHAR L331_;
 CHAR L34;
 BOOL L351_;
 CHAR L361_;
 CHAR L37;
 BOOL L381_;
 BOOL L39;
 CHAR L401_;
 CHAR L41;
 BOOL L421_;
 CHAR L431_;
 CHAR L44;
 BOOL L451_;
 CHAR L461_;
 CHAR L47;
 BOOL L481_;
 INT L49;
 INT L501_;
 CHAR L511_;
 CHAR L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 FILE1 L56;
 OB L57;
 extern STR name21;
 FILE1 L59;
 OB L60;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 extern STR expect1505081358;
 FILE1 L68;
 OB L69;
 CHAR L711_;
 dSTR L72;
 OB L73;
 FILE1 L74;
 OB L75;
 extern STR name9;
 FILE1 L77;
 OB L78;
 CONFIG1491579599(self);
 aget_self = ATTR(self,fstr);
 aget_ind = ATTR(self,next);
 L11_=ARR((FSTR)aget_self,aget_ind); 
 ret_val1 = L11_;
 L2 = ret_val1;
 L31_=L2==';'; 
 if (L31_) {
  ret_val = ((FLISTSTR) NULL);
  return ret_val;
 }
 else {
  aget_self1 = ATTR(self,fstr);
  aget_ind1 = ATTR(self,next);
  L61_=ARR((FSTR)aget_self1,aget_ind1); 
  ret_val2 = L61_;
  L7 = ret_val2;
  L81_=L7=='+'; 
  if (L81_) {
   L5 = TRUE;
  } else {
   aget_self2 = ATTR(self,fstr);
   aget_ind2 = ATTR(self,next);
   L91_=ARR((FSTR)aget_self2,aget_ind2); 
   ret_val3 = L91_;
   L10 = ret_val3;
   L121_=L10=='-'; 
   L5 = L121_;
  }
  if (L5) {
   L4 = TRUE;
  } else {
   L4 = CONFIG981438152(self);
  }
  if (L4) {
   r = FLISTS1425610062(r, CONFIG1745974311(self));
  }
  else {
   is_string_self = self;
   aget_self3 = ATTR(is_string_self,fstr);
   aget_ind3 = ATTR(is_string_self,next);
   L131_=ARR((FSTR)aget_self3,aget_ind3); 
   ret_val5 = L131_;
   L14 = ret_val5;
   L151_=L14=='\"'; 
   ret_val4 = L151_;
   if (ret_val4) {
    r = FLISTS1425610062(r, CONFIG1497770745(self));
   }
   else {
    r = FLISTS1425610062(r, CONFIG458671273(self));
    CONFIG1491579599(self);
    aget_self4 = ATTR(self,fstr);
    aget_ind4 = ATTR(self,next);
    L171_=ARR((FSTR)aget_self4,aget_ind4); 
    ret_val6 = L171_;
    L18 = ret_val6;
    L191_=L18==','; 
    L20 = L191_;
    L211_=!(L20); 
    if (L211_) {
     aget_self5 = ATTR(self,fstr);
     aget_ind5 = ATTR(self,next);
     L221_=ARR((FSTR)aget_self5,aget_ind5); 
     ret_val7 = L221_;
     L23 = ret_val7;
     L241_=L23==';'; 
     L25 = L241_;
     L261_=!(L25); 
     L16 = L261_;
    } else {
     L16 = FALSE;
    }
    if (L16) {
     while (1) {
      aget_self6 = ATTR(self,fstr);
      aget_ind6 = ATTR(self,next);
      L291_=ARR((FSTR)aget_self6,aget_ind6); 
      ret_val8 = L291_;
      L30 = ret_val8;
      L321_=L30=='+'; 
      if (L321_) {
       L28 = TRUE;
      } else {
       aget_self7 = ATTR(self,fstr);
       aget_ind7 = ATTR(self,next);
       L331_=ARR((FSTR)aget_self7,aget_ind7); 
       ret_val9 = L331_;
       L34 = ret_val9;
       L351_=L34=='-'; 
       L28 = L351_;
      }
      if (L28) {
       L27 = TRUE;
      } else {
       L27 = CONFIG981438152(self);
      }
      if (L27) {
       r = FLISTS1425610062(r, CONFIG1745974311(self));
      }
      else {
       is_string_self1 = self;
       aget_self8 = ATTR(is_string_self1,fstr);
       aget_ind8 = ATTR(is_string_self1,next);
       L361_=ARR((FSTR)aget_self8,aget_ind8); 
       ret_val11 = L361_;
       L37 = ret_val11;
       L381_=L37=='\"'; 
       ret_val10 = L381_;
       if (ret_val10) {
        r = FLISTS1425610062(r, CONFIG1497770745(self));
       }
       else {
        r = FLISTS1425610062(r, CONFIG458671273(self));
       }
      }
      CONFIG1491579599(self);
      aget_self9 = ATTR(self,fstr);
      aget_ind9 = ATTR(self,next);
      L401_=ARR((FSTR)aget_self9,aget_ind9); 
      ret_val12 = L401_;
      L41 = ret_val12;
      L421_=L41==','; 
      if (L421_) {
       L39 = TRUE;
      } else {
       aget_self10 = ATTR(self,fstr);
       aget_ind10 = ATTR(self,next);
       L431_=ARR((FSTR)aget_self10,aget_ind10); 
       ret_val13 = L431_;
       L44 = ret_val13;
       L451_=L44==';'; 
       L39 = L451_;
      }
      if (L39) {
       goto after_loop;
      }
     }
     after_loop: ;
    }
   }
  }
 }
 CONFIG1491579599(self);
 aget_self11 = ATTR(self,fstr);
 aget_ind11 = ATTR(self,next);
 L461_=ARR((FSTR)aget_self11,aget_ind11); 
 ret_val14 = L461_;
 L47 = ret_val14;
 L481_=L47==','; 
 if (L481_) {
  L49 = ATTR(self,next);
  L501_=INTPLUS(L49,1); 
  SATTR(self,next,L501_);
 }
 else {
  aget_self12 = ATTR(self,fstr);
  aget_ind12 = ATTR(self,next);
  L511_=ARR((FSTR)aget_self12,aget_ind12); 
  ret_val15 = L511_;
  L52 = ret_val15;
  L531_=L52==';'; 
  L54 = L531_;
  L551_=!(L54); 
  if (L551_) {
   create_self = ((OUT) NULL);
   ret_val16 = create_self;
   plus_self = ret_val16;
   plus_s = ATTR(self,file1);
   stdout_self = ((FILE1) NULL);
   L57=ZALLOC(sizeof(struct FILE1_struct));
   if (L57==NULL) FATAL("Unable to allocate more memory");
   ((OB)L57)->header.tag=FILE1_tag;
   L56 = ((FILE1) L57);
   r1 = L56;
   
   SATTR(r1,fp,stdout);
   ret_val18 = r1;
   FILE_plus_STR(ret_val18, plus_s);
   ret_val17 = plus_self;
   plus_self1 = ret_val17;
   plus_s1 = ((STR) &name21);
   stdout_self1 = ((FILE1) NULL);
   L60=ZALLOC(sizeof(struct FILE1_struct));
   if (L60==NULL) FATAL("Unable to allocate more memory");
   ((OB)L60)->header.tag=FILE1_tag;
   L59 = ((FILE1) L60);
   r2 = L59;
   
   SATTR(r2,fp,stdout);
   ret_val20 = r2;
   FILE_plus_STR(ret_val20, plus_s1);
   ret_val19 = plus_self1;
   plus_self2 = ret_val19;
   L64=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L64==NULL) FATAL("Unable to allocate more memory");
   memset(L64,0,sizeof(struct INT_boxed_struct));
   ((OB)L64)->header.tag=INT_tag;
   L63 = (dSTR)((INT_boxed) L64);
   ((INT_boxed) L63)->immutable_part = ATTR(self,line1);
   plus_s2 = L63;
   stdout_self2 = ((FILE1) NULL);
   L66=ZALLOC(sizeof(struct FILE1_struct));
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=FILE1_tag;
   L65 = ((FILE1) L66);
   r3 = L65;
   
   SATTR(r3,fp,stdout);
   ret_val22 = r3;
   FILE_plus_dSTR(ret_val22, plus_s2);
   ret_val21 = plus_self2;
   plus_self3 = ret_val21;
   plus_s3 = ((STR) &expect1505081358);
   stdout_self3 = ((FILE1) NULL);
   L69=ZALLOC(sizeof(struct FILE1_struct));
   if (L69==NULL) FATAL("Unable to allocate more memory");
   ((OB)L69)->header.tag=FILE1_tag;
   L68 = ((FILE1) L69);
   r4 = L68;
   
   SATTR(r4,fp,stdout);
   ret_val24 = r4;
   FILE_plus_STR(ret_val24, plus_s3);
   ret_val23 = plus_self3;
   plus_self4 = ret_val23;
   aget_self13 = ATTR(self,fstr);
   aget_ind13 = ATTR(self,next);
   L711_=ARR((FSTR)aget_self13,aget_ind13); 
   ret_val25 = L711_;
   L73=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L73==NULL) FATAL("Unable to allocate more memory");
   memset(L73,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L73)->header.tag=CHAR_tag;
   L72 = (dSTR)((CHAR_boxed) L73);
   ((CHAR_boxed) L72)->immutable_part = ret_val25;
   plus_s4 = L72;
   stdout_self4 = ((FILE1) NULL);
   L75=ZALLOC(sizeof(struct FILE1_struct));
   if (L75==NULL) FATAL("Unable to allocate more memory");
   ((OB)L75)->header.tag=FILE1_tag;
   L74 = ((FILE1) L75);
   r5 = L74;
   
   SATTR(r5,fp,stdout);
   ret_val27 = r5;
   FILE_plus_dSTR(ret_val27, plus_s4);
   ret_val26 = plus_self4;
   plus_self5 = ret_val26;
   plus_s5 = ((STR) &name9);
   stdout_self5 = ((FILE1) NULL);
   L78=ZALLOC(sizeof(struct FILE1_struct));
   if (L78==NULL) FATAL("Unable to allocate more memory");
   ((OB)L78)->header.tag=FILE1_tag;
   L77 = ((FILE1) L78);
   r6 = L77;
   
   SATTR(r6,fp,stdout);
   ret_val28 = r6;
   FILE_plus_STR(ret_val28, plus_s5);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ARR1424607794(AM_ARR_CONST self) {
 INT ret_val = INT_zero;
 INT htemp = INT_zero;
 INT L11 = INT_zero;
 INT i = INT_zero;
 INT L21 = INT_zero;
 ARRAYdAM_CONST L4;
 INT L51_;
 INT aL31_;
 INT L6;
 ARRAYdAM_CONST L7;
 dAM_CONST L81_;
 dAM_CONST L9;
 INT L10;
 INT L121_;
 INT L131_;
 htemp = 21857;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L4 = ATTR(self,elts);
  L51_=ASIZE((ARRAYdAM_CONST)L4); 
  L11 = L51_;
  while (1) {
   if(L21>=L11)  goto after_loop; 
   aL31_=L21; 
   i = aL31_;
   htemp = INT_hashrINT(htemp);
   L7 = ATTR(self,elts);
   L81_=(dAM_CONST)ARR((ARRAYdAM_CONST)L7,i); 
   L9 = L81_;
   L10 = (*dAM_CO1597271567[TAG(L9)])(L9);
   L121_=htemp^L10; 
   htemp = L121_;
   L131_=INTPLUS(L21,1); 
   L21 = L131_;
  }
 }
 after_loop: ;
 ret_val = htemp;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_BOO561475672(AM_BOOL_CONST self) {
 INT ret_val = INT_zero;
 if (ATTR(self,val1)) {
  ret_val = 1294812094;
  return ret_val;
 }
 else {
  ret_val = 1284721945;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AM_CHA1966863808(AM_CHAR_CONST self) {
 INT ret_val = INT_zero;
 CHAR hash_self = CHAR_zero;
 INT ret_val1 = INT_zero;
 INT L11_;
 if ((ATTR(self,val1)==((INTI1) NULL))) {
  hash_self = ATTR(self,bval);
  L11_=(INT)((unsigned char)hash_self); 
  ret_val1 = INT_hashrINT(L11_);
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  ret_val = INTI_hashrINT(ATTR(self,val1));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FLT1054767436(AM_FLTX_CONST self) {
 INT ret_val = INT_zero;
 RAT hash_self = RAT_zero;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 hash_self = ATTR(self,val1);
 L1 = INTI_hashrINT(hash_self.u);
 L2 = INTI_hashrINT(hash_self.v);
 L31_=L1^L2; 
 ret_val1 = L31_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FLT465909163(AM_FLT_CONST self) {
 INT ret_val = INT_zero;
 RAT hash_self = RAT_zero;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 hash_self = ATTR(self,val1);
 L1 = INTI_hashrINT(hash_self.u);
 L2 = INTI_hashrINT(hash_self.v);
 L31_=L1^L2; 
 ret_val1 = L31_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FLT556661152(AM_FLTD_CONST self) {
 INT ret_val = INT_zero;
 RAT hash_self = RAT_zero;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 hash_self = ATTR(self,val1);
 L1 = INTI_hashrINT(hash_self.u);
 L2 = INTI_hashrINT(hash_self.v);
 L31_=L1^L2; 
 ret_val1 = L31_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FLT669520374(AM_FLTDX_CONST self) {
 INT ret_val = INT_zero;
 RAT hash_self = RAT_zero;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 INT L31_;
 hash_self = ATTR(self,val1);
 L1 = INTI_hashrINT(hash_self.u);
 L2 = INTI_hashrINT(hash_self.v);
 L31_=L1^L2; 
 ret_val1 = L31_;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_INT2057636940(AM_INT_CONST self) {
 INT ret_val = INT_zero;
 ret_val = INTI_hashrINT(ATTR(self,val1));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_INT331860696(AM_INTI_CONST self) {
 INT ret_val = INT_zero;
 ret_val = INTI_hashrINT(ATTR(self,val1));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_STR1959493974(AM_STR_CONST self) {
 INT ret_val = INT_zero;
 ret_val = STR_hashrINT(ATTR(self,bval));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_VOI822416442(AM_VOID_CONST self) {
 INT ret_val = INT_zero;
 ret_val = 12648273;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ARR490952057(AM_ARR_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_BOO1043294506(AM_BOOL_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CHA1773602482(AM_CHAR_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FLT1298836824(AM_FLT_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FLT1412381806(AM_FLTD_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FLT2070042319(AM_FLTDX_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FLT2096495882(AM_FLTX_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_INT1397528579(AM_INT_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_INT872738218(AM_INTI_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_STR1580126161(AM_STR_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_VOI405249940(AM_VOID_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG1175493142(CONFIG_ROUT self) {
 return ATTR(self,name1);
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG133831937(CONFIG_ROUT self, ARRAYINT e) {
 STR ret_val;
 STR r;
 ARRAYINT L11;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 INT L21 = INT_zero;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 extern STR name1;
 INT L31_br;
 INT aL31_;
 extern STR name10;
 INT L4;
 BOOL L5;
 BOOL L61_;
 INT L71_;
 r = ((STR) &name1);
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = e;
  L31_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
  while (1) {
   plus_self = r;
   plus_sarg = ((STR) &name10);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYINT)L11,L21); 
   str_self = aL31_;
   clear_self = buf1;
   L5 = (clear_self==((FSTR) NULL));
   L61_=!(L5); 
   if (L61_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   plus_sarg1 = ret_val2;
   ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
   r = ret_val5;
   L71_=INTPLUS(L21,1); 
   L21 = L71_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG1397479570(CONFIG_TBL self, STR s, INT i) {
 STR ret_val;
 CONFIG_DEF r;
 CONFIG_DEF size_self;
 INT ret_val1 = INT_zero;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val2;
 BOOL L1;
 ARRAYARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR name1;
 ARRAYARRAYSTR L8;
 ARRAYSTR L91_;
 ARRAYSTR L10;
 STR L111_;
 r = CONFIG1152920950(self, s);
 if ((r==((CONFIG_DEF) NULL))) {
  L1 = TRUE;
 } else {
  size_self = r;
  L2 = ATTR(size_self,expr);
  L31_=(L2)==NULL?0:ASIZE((ARRAYARRAYSTR)L2); 
  ret_val1 = L31_;
  L4 = ret_val1;
  L51_=(i)<(L4); 
  L6 = L51_;
  L71_=!(L6); 
  L1 = L71_;
 }
 if (L1) {
  ret_val = ((STR) &name1);
  return ret_val;
 }
 aget_self = r;
 aget_i = i;
 L8 = ATTR(aget_self,expr);
 L91_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L8,aget_i); 
 ret_val2 = L91_;
 L10 = ret_val2;
 L111_=(STR)ARR((ARRAYSTR)L10,0); 
 ret_val = L111_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG1497770745(CONFIG_SCANNER self) {
 STR ret_val;
 INT start1 = INT_zero;
 STR r;
 CONFIG_SCANNER eof_self;
 BOOL ret_val1 = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 OUT create_self;
 OUT ret_val5;
 OUT plus_self;
 STR plus_s;
 OUT ret_val6;
 FILE1 stdout_self;
 FILE1 ret_val7;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val8;
 FILE1 stdout_self1;
 FILE1 ret_val9;
 FILE1 r2;
 OUT plus_self2;
 dSTR plus_s2;
 OUT ret_val10;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val12;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CONFIG_SCANNER eof_self1;
 BOOL ret_val13 = BOOL_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 OUT create_self1;
 OUT ret_val15;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val16;
 FILE1 stdout_self4;
 FILE1 ret_val17;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val18;
 FILE1 stdout_self5;
 FILE1 ret_val19;
 FILE1 r6;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val20;
 FILE1 stdout_self6;
 FILE1 ret_val21;
 FILE1 r7;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val22;
 FILE1 r8;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 FSTR str_self;
 STR ret_val23;
 INT L1;
 INT L21_;
 BOOL L3;
 INT L4;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 BOOL L11;
 CHAR L121_;
 CHAR L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 INT L17;
 INT L181_;
 CHAR L191_;
 CHAR L20;
 BOOL L221_;
 CHAR L231_;
 CHAR L24;
 BOOL L251_;
 FILE1 L26;
 OB L27;
 extern STR name21;
 FILE1 L29;
 OB L30;
 dSTR L32;
 OB L33;
 FILE1 L34;
 OB L35;
 extern STR runawaystring;
 FILE1 L37;
 OB L38;
 INT L40;
 INT L411_;
 BOOL L42;
 INT L43;
 INT L44;
 BOOL L451_;
 BOOL L46;
 BOOL L471_;
 CHAR L481_;
 CHAR L49;
 BOOL L501_;
 BOOL L51;
 BOOL L521_;
 FILE1 L53;
 OB L54;
 extern STR name21;
 FILE1 L56;
 OB L57;
 dSTR L59;
 OB L60;
 FILE1 L62;
 OB L63;
 extern STR runawaystring;
 FILE1 L65;
 OB L66;
 INT L68;
 INT L691_;
 FSTR L70;
 INT L71;
 INT L721_;
 extern STR name22;
 extern STR name1;
 start1 = ATTR(self,next);
 L1 = ATTR(self,next);
 L21_=INTPLUS(L1,1); 
 SATTR(self,next,L21_);
 while (1) {
  eof_self = self;
  L4 = ATTR(eof_self,next);
  L5 = FSTR_sizerINT(ATTR(eof_self,fstr));
  L61_=(L4)<(L5); 
  L7 = L61_;
  L81_=!(L7); 
  ret_val1 = L81_;
  L9 = ret_val1;
  L101_=!(L9); 
  if (L101_) {
   aget_self = ATTR(self,fstr);
   aget_ind = ATTR(self,next);
   L121_=ARR((FSTR)aget_self,aget_ind); 
   ret_val2 = L121_;
   L13 = ret_val2;
   L141_=L13=='\"'; 
   L15 = L141_;
   L161_=!(L15); 
   if (L161_) {
    L11 = TRUE;
   } else {
    aget_self1 = ATTR(self,fstr);
    L17 = ATTR(self,next);
    L181_=INTMINUS(L17,1); 
    aget_ind1 = L181_;
    L191_=ARR((FSTR)aget_self1,aget_ind1); 
    ret_val3 = L191_;
    L20 = ret_val3;
    L221_=L20=='\\'; 
    L11 = L221_;
   }
   L3 = L11;
  } else {
   L3 = FALSE;
  }
  if (L3) {
  }
  else {
   goto after_loop;
  }
  aget_self2 = ATTR(self,fstr);
  aget_ind2 = ATTR(self,next);
  L231_=ARR((FSTR)aget_self2,aget_ind2); 
  ret_val4 = L231_;
  L24 = ret_val4;
  L251_=L24=='\n'; 
  if (L251_) {
   create_self = ((OUT) NULL);
   ret_val5 = create_self;
   plus_self = ret_val5;
   plus_s = ATTR(self,file1);
   stdout_self = ((FILE1) NULL);
   L27=ZALLOC(sizeof(struct FILE1_struct));
   if (L27==NULL) FATAL("Unable to allocate more memory");
   ((OB)L27)->header.tag=FILE1_tag;
   L26 = ((FILE1) L27);
   r1 = L26;
   
   SATTR(r1,fp,stdout);
   ret_val7 = r1;
   FILE_plus_STR(ret_val7, plus_s);
   ret_val6 = plus_self;
   plus_self1 = ret_val6;
   plus_s1 = ((STR) &name21);
   stdout_self1 = ((FILE1) NULL);
   L30=ZALLOC(sizeof(struct FILE1_struct));
   if (L30==NULL) FATAL("Unable to allocate more memory");
   ((OB)L30)->header.tag=FILE1_tag;
   L29 = ((FILE1) L30);
   r2 = L29;
   
   SATTR(r2,fp,stdout);
   ret_val9 = r2;
   FILE_plus_STR(ret_val9, plus_s1);
   ret_val8 = plus_self1;
   plus_self2 = ret_val8;
   L33=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   memset(L33,0,sizeof(struct INT_boxed_struct));
   ((OB)L33)->header.tag=INT_tag;
   L32 = (dSTR)((INT_boxed) L33);
   ((INT_boxed) L32)->immutable_part = ATTR(self,line1);
   plus_s2 = L32;
   stdout_self2 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   r3 = L34;
   
   SATTR(r3,fp,stdout);
   ret_val11 = r3;
   FILE_plus_dSTR(ret_val11, plus_s2);
   ret_val10 = plus_self2;
   plus_self3 = ret_val10;
   plus_s3 = ((STR) &runawaystring);
   stdout_self3 = ((FILE1) NULL);
   L38=ZALLOC(sizeof(struct FILE1_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=FILE1_tag;
   L37 = ((FILE1) L38);
   r4 = L37;
   
   SATTR(r4,fp,stdout);
   ret_val12 = r4;
   FILE_plus_STR(ret_val12, plus_s3);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
  }
  L40 = ATTR(self,next);
  L411_=INTPLUS(L40,1); 
  SATTR(self,next,L411_);
 }
 after_loop: ;
 eof_self1 = self;
 L43 = ATTR(eof_self1,next);
 L44 = FSTR_sizerINT(ATTR(eof_self1,fstr));
 L451_=(L43)<(L44); 
 L46 = L451_;
 L471_=!(L46); 
 ret_val13 = L471_;
 if (ret_val13) {
  L42 = TRUE;
 } else {
  aget_self3 = ATTR(self,fstr);
  aget_ind3 = ATTR(self,next);
  L481_=ARR((FSTR)aget_self3,aget_ind3); 
  ret_val14 = L481_;
  L49 = ret_val14;
  L501_=L49=='\"'; 
  L51 = L501_;
  L521_=!(L51); 
  L42 = L521_;
 }
 if (L42) {
  create_self1 = ((OUT) NULL);
  ret_val15 = create_self1;
  plus_self4 = ret_val15;
  plus_s4 = ATTR(self,file1);
  stdout_self4 = ((FILE1) NULL);
  L54=ZALLOC(sizeof(struct FILE1_struct));
  if (L54==NULL) FATAL("Unable to allocate more memory");
  ((OB)L54)->header.tag=FILE1_tag;
  L53 = ((FILE1) L54);
  r5 = L53;
  
  SATTR(r5,fp,stdout);
  ret_val17 = r5;
  FILE_plus_STR(ret_val17, plus_s4);
  ret_val16 = plus_self4;
  plus_self5 = ret_val16;
  plus_s5 = ((STR) &name21);
  stdout_self5 = ((FILE1) NULL);
  L57=ZALLOC(sizeof(struct FILE1_struct));
  if (L57==NULL) FATAL("Unable to allocate more memory");
  ((OB)L57)->header.tag=FILE1_tag;
  L56 = ((FILE1) L57);
  r6 = L56;
  
  SATTR(r6,fp,stdout);
  ret_val19 = r6;
  FILE_plus_STR(ret_val19, plus_s5);
  ret_val18 = plus_self5;
  plus_self6 = ret_val18;
  L60=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L60==NULL) FATAL("Unable to allocate more memory");
  memset(L60,0,sizeof(struct INT_boxed_struct));
  ((OB)L60)->header.tag=INT_tag;
  L59 = (dSTR)((INT_boxed) L60);
  ((INT_boxed) L59)->immutable_part = ATTR(self,line1);
  plus_s6 = L59;
  stdout_self6 = ((FILE1) NULL);
  L63=ZALLOC(sizeof(struct FILE1_struct));
  if (L63==NULL) FATAL("Unable to allocate more memory");
  ((OB)L63)->header.tag=FILE1_tag;
  L62 = ((FILE1) L63);
  r7 = L62;
  
  SATTR(r7,fp,stdout);
  ret_val21 = r7;
  FILE_plus_dSTR(ret_val21, plus_s6);
  ret_val20 = plus_self6;
  plus_self7 = ret_val20;
  plus_s7 = ((STR) &runawaystring);
  stdout_self7 = ((FILE1) NULL);
  L66=ZALLOC(sizeof(struct FILE1_struct));
  if (L66==NULL) FATAL("Unable to allocate more memory");
  ((OB)L66)->header.tag=FILE1_tag;
  L65 = ((FILE1) L66);
  r8 = L65;
  
  SATTR(r8,fp,stdout);
  ret_val22 = r8;
  FILE_plus_STR(ret_val22, plus_s7);
  exit_self1 = ((UNIX) NULL);
  exit_code1 = -1;
  exit(exit_code1);
 }
 L68 = ATTR(self,next);
 L691_=INTPLUS(L68,1); 
 SATTR(self,next,L691_);
 L70 = ATTR(self,fstr);
 L71 = ATTR(self,next);
 L721_=INTMINUS(L71,start1); 
 str_self = FSTR_s1920457899(L70, start1, L721_);
 ret_val23 = STR_fr1097270334(((STR) NULL), str_self);
 r = ret_val23;
 if (STR_is111530248(r, ((STR) &name22))) {
  ret_val = ((STR) &name1);
  return ret_val;
 }
 else {
  ret_val = STR_as1755591216(r);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG155608225(CONFIG_TBL self) {
 STR ret_val;
 STR r;
 FMAPSTRdCONFIG L11;
 extern STR name1;
 dCONFIG L2;
 r = ((STR) &name1);
 {
  struct FMAPST1785271125_frame_struct other1_0;
FMAPST1785271125_frame noname1 = &other1_0;
  L11 = ATTR(self,tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FMAPST1785271125(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = STR_pl1270664985(r, ((dSTR) L2));
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG1745974311(CONFIG_SCANNER self) {
 STR ret_val;
 INT start1 = INT_zero;
 STR r;
 FSTR str_self;
 STR ret_val1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 FSTR L5;
 INT L6;
 INT L71_;
 start1 = ATTR(self,next);
 L1 = ATTR(self,next);
 L21_=INTPLUS(L1,1); 
 SATTR(self,next,L21_);
 while (1) {
  if (CONFIG981438152(self)) {
  }
  else {
   goto after_loop;
  }
  L3 = ATTR(self,next);
  L41_=INTPLUS(L3,1); 
  SATTR(self,next,L41_);
 }
 after_loop: ;
 L5 = ATTR(self,fstr);
 L6 = ATTR(self,next);
 L71_=INTMINUS(L6,start1); 
 str_self = FSTR_s1920457899(L5, start1, L71_);
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG336048123(CONFIG_ROUT self) {
 STR ret_val;
 STR r;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
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
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val9;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val10;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val11;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val12;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val13;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val14;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val15;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val16;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val17;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val18;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val19;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val20;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val21;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val22;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val23;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val24;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val25;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val26;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val27;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val28;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val29;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val30;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val31;
 STR plus_self31;
 STR plus_sarg31;
 STR ret_val32;
 STR plus_self32;
 STR plus_sarg32;
 STR ret_val33;
 STR plus_self33;
 STR plus_sarg33;
 STR ret_val34;
 STR plus_self34;
 STR plus_sarg34;
 STR ret_val35;
 STR plus_self35;
 STR plus_sarg35;
 STR ret_val36;
 STR plus_self36;
 STR plus_sarg36;
 STR ret_val37;
 STR plus_self37;
 STR plus_sarg37;
 STR ret_val38;
 STR plus_self38;
 STR plus_sarg38;
 STR ret_val39;
 STR plus_self39;
 STR plus_sarg39;
 STR ret_val40;
 STR plus_self40;
 STR plus_sarg40;
 STR ret_val41;
 STR plus_self41;
 STR plus_sarg41;
 STR ret_val42;
 STR plus_self42;
 STR plus_sarg42;
 STR ret_val43;
 STR plus_self43;
 STR plus_sarg43;
 STR ret_val44;
 STR plus_self44;
 STR plus_sarg44;
 STR ret_val45;
 STR plus_self45;
 STR plus_sarg45;
 STR ret_val46;
 STR plus_self46;
 STR plus_sarg46;
 STR ret_val47;
 STR plus_self47;
 STR plus_sarg47;
 STR ret_val48;
 STR plus_self48;
 STR plus_sarg48;
 STR ret_val49;
 extern STR name11;
 extern STR volatile1;
 extern STR fragile;
 extern STR no_pre;
 extern STR no_post;
 extern STR block1;
 extern STR arith;
 extern STR import;
 extern STR export1;
 extern STR raises_any;
 extern STR reads_any;
 extern STR writes_any;
 extern STR is_near;
 BOOL L1;
 BOOL L21_;
 extern STR attraccess;
 extern STR name12;
 BOOL L3;
 BOOL L41_;
 extern STR attrne1319683518;
 extern STR name12;
 BOOL L5;
 BOOL L61_;
 extern STR raises;
 extern STR name12;
 BOOL L7;
 BOOL L81_;
 extern STR reads;
 extern STR name12;
 BOOL L9;
 BOOL L101_;
 extern STR writes;
 extern STR name12;
 BOOL L11;
 BOOL L121_;
 extern STR var1;
 extern STR name12;
 BOOL L13;
 BOOL L141_;
 extern STR declare;
 extern STR name12;
 BOOL L15;
 BOOL L161_;
 extern STR exec;
 extern STR name13;
 BOOL L17;
 BOOL L181_;
 extern STR init;
 extern STR name12;
 BOOL L19;
 BOOL L201_;
 extern STR temp1;
 extern STR name12;
 BOOL L22;
 BOOL L231_;
 extern STR break1;
 extern STR name12;
 BOOL L24;
 BOOL L251_;
 extern STR iter;
 extern STR name12;
 plus_self = ATTR(self,name1);
 plus_sarg = ((STR) &name11);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 r = ret_val1;
 if (ATTR(self,volatile1)) {
  plus_self1 = r;
  plus_sarg1 = ((STR) &volatile1);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  r = ret_val2;
 }
 if (ATTR(self,fragile)) {
  plus_self2 = r;
  plus_sarg2 = ((STR) &fragile);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  r = ret_val3;
 }
 if (ATTR(self,no_pre)) {
  plus_self3 = r;
  plus_sarg3 = ((STR) &no_pre);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  r = ret_val4;
 }
 if (ATTR(self,no_post)) {
  plus_self4 = r;
  plus_sarg4 = ((STR) &no_post);
  ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
  r = ret_val5;
 }
 if (ATTR(self,block1)) {
  plus_self5 = r;
  plus_sarg5 = ((STR) &block1);
  ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
  r = ret_val6;
 }
 if (ATTR(self,arith)) {
  plus_self6 = r;
  plus_sarg6 = ((STR) &arith);
  ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
  r = ret_val7;
 }
 if (ATTR(self,does_import)) {
  plus_self7 = r;
  plus_sarg7 = ((STR) &import);
  ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
  r = ret_val8;
 }
 if (ATTR(self,does_export)) {
  plus_self8 = r;
  plus_sarg8 = ((STR) &export1);
  ret_val9 = STR_ap2004550586(plus_self8, plus_sarg8);
  r = ret_val9;
 }
 if (ATTR(self,raises_any)) {
  plus_self9 = r;
  plus_sarg9 = ((STR) &raises_any);
  ret_val10 = STR_ap2004550586(plus_self9, plus_sarg9);
  r = ret_val10;
 }
 if (ATTR(self,reads_any)) {
  plus_self10 = r;
  plus_sarg10 = ((STR) &reads_any);
  ret_val11 = STR_ap2004550586(plus_self10, plus_sarg10);
  r = ret_val11;
 }
 if (ATTR(self,writes_any)) {
  plus_self11 = r;
  plus_sarg11 = ((STR) &writes_any);
  ret_val12 = STR_ap2004550586(plus_self11, plus_sarg11);
  r = ret_val12;
 }
 if (ATTR(self,is_near)) {
  plus_self12 = r;
  plus_sarg12 = ((STR) &is_near);
  ret_val13 = STR_ap2004550586(plus_self12, plus_sarg12);
  r = ret_val13;
 }
 L1 = (ATTR(self,attr_access)==((ARRAYINT) NULL));
 L21_=!(L1); 
 if (L21_) {
  plus_self13 = r;
  plus_sarg13 = ((STR) &attraccess);
  ret_val14 = STR_ap2004550586(plus_self13, plus_sarg13);
  plus_self14 = ret_val14;
  plus_sarg14 = CONFIG133831937(self, ATTR(self,attr_access));
  ret_val15 = STR_ap2004550586(plus_self14, plus_sarg14);
  plus_self15 = ret_val15;
  plus_sarg15 = ((STR) &name12);
  ret_val16 = STR_ap2004550586(plus_self15, plus_sarg15);
  r = ret_val16;
 }
 L3 = (ATTR(self,near_depends_on)==((ARRAYINT) NULL));
 L41_=!(L3); 
 if (L41_) {
  plus_self16 = r;
  plus_sarg16 = ((STR) &attrne1319683518);
  ret_val17 = STR_ap2004550586(plus_self16, plus_sarg16);
  plus_self17 = ret_val17;
  plus_sarg17 = CONFIG133831937(self, ATTR(self,near_depends_on));
  ret_val18 = STR_ap2004550586(plus_self17, plus_sarg17);
  plus_self18 = ret_val18;
  plus_sarg18 = ((STR) &name12);
  ret_val19 = STR_ap2004550586(plus_self18, plus_sarg18);
  r = ret_val19;
 }
 L5 = (ATTR(self,raises)==((ARRAYSTR) NULL));
 L61_=!(L5); 
 if (L61_) {
  plus_self19 = r;
  plus_sarg19 = ((STR) &raises);
  ret_val20 = STR_ap2004550586(plus_self19, plus_sarg19);
  plus_self20 = ret_val20;
  plus_sarg20 = CONFIG492273949(self, ATTR(self,raises));
  ret_val21 = STR_ap2004550586(plus_self20, plus_sarg20);
  plus_self21 = ret_val21;
  plus_sarg21 = ((STR) &name12);
  ret_val22 = STR_ap2004550586(plus_self21, plus_sarg21);
  r = ret_val22;
 }
 L7 = (ATTR(self,reads)==((ARRAYSTR) NULL));
 L81_=!(L7); 
 if (L81_) {
  plus_self22 = r;
  plus_sarg22 = ((STR) &reads);
  ret_val23 = STR_ap2004550586(plus_self22, plus_sarg22);
  plus_self23 = ret_val23;
  plus_sarg23 = CONFIG492273949(self, ATTR(self,reads));
  ret_val24 = STR_ap2004550586(plus_self23, plus_sarg23);
  plus_self24 = ret_val24;
  plus_sarg24 = ((STR) &name12);
  ret_val25 = STR_ap2004550586(plus_self24, plus_sarg24);
  r = ret_val25;
 }
 L9 = (ATTR(self,writes)==((ARRAYSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  plus_self25 = r;
  plus_sarg25 = ((STR) &writes);
  ret_val26 = STR_ap2004550586(plus_self25, plus_sarg25);
  plus_self26 = ret_val26;
  plus_sarg26 = CONFIG492273949(self, ATTR(self,writes));
  ret_val27 = STR_ap2004550586(plus_self26, plus_sarg26);
  plus_self27 = ret_val27;
  plus_sarg27 = ((STR) &name12);
  ret_val28 = STR_ap2004550586(plus_self27, plus_sarg27);
  r = ret_val28;
 }
 L11 = (ATTR(self,var)==((ARRAYSTR) NULL));
 L121_=!(L11); 
 if (L121_) {
  plus_self28 = r;
  plus_sarg28 = ((STR) &var1);
  ret_val29 = STR_ap2004550586(plus_self28, plus_sarg28);
  plus_self29 = ret_val29;
  plus_sarg29 = CONFIG492273949(self, ATTR(self,var));
  ret_val30 = STR_ap2004550586(plus_self29, plus_sarg29);
  plus_self30 = ret_val30;
  plus_sarg30 = ((STR) &name12);
  ret_val31 = STR_ap2004550586(plus_self30, plus_sarg30);
  r = ret_val31;
 }
 L13 = (ATTR(self,declare)==((ARRAYSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  plus_self31 = r;
  plus_sarg31 = ((STR) &declare);
  ret_val32 = STR_ap2004550586(plus_self31, plus_sarg31);
  plus_self32 = ret_val32;
  plus_sarg32 = CONFIG492273949(self, ATTR(self,declare));
  ret_val33 = STR_ap2004550586(plus_self32, plus_sarg32);
  plus_self33 = ret_val33;
  plus_sarg33 = ((STR) &name12);
  ret_val34 = STR_ap2004550586(plus_self33, plus_sarg33);
  r = ret_val34;
 }
 L15 = (ATTR(self,exec)==((ARRAYSTR) NULL));
 L161_=!(L15); 
 if (L161_) {
  plus_self34 = r;
  plus_sarg34 = ((STR) &exec);
  ret_val35 = STR_ap2004550586(plus_self34, plus_sarg34);
  plus_self35 = ret_val35;
  plus_sarg35 = CONFIG492273949(self, ATTR(self,exec));
  ret_val36 = STR_ap2004550586(plus_self35, plus_sarg35);
  plus_self36 = ret_val36;
  plus_sarg36 = ((STR) &name13);
  ret_val37 = STR_ap2004550586(plus_self36, plus_sarg36);
  r = ret_val37;
 }
 L17 = (ATTR(self,init)==((ARRAYSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  plus_self37 = r;
  plus_sarg37 = ((STR) &init);
  ret_val38 = STR_ap2004550586(plus_self37, plus_sarg37);
  plus_self38 = ret_val38;
  plus_sarg38 = CONFIG492273949(self, ATTR(self,init));
  ret_val39 = STR_ap2004550586(plus_self38, plus_sarg38);
  plus_self39 = ret_val39;
  plus_sarg39 = ((STR) &name12);
  ret_val40 = STR_ap2004550586(plus_self39, plus_sarg39);
  r = ret_val40;
 }
 L19 = (ATTR(self,temp1)==((ARRAYSTR) NULL));
 L201_=!(L19); 
 if (L201_) {
  plus_self40 = r;
  plus_sarg40 = ((STR) &temp1);
  ret_val41 = STR_ap2004550586(plus_self40, plus_sarg40);
  plus_self41 = ret_val41;
  plus_sarg41 = CONFIG492273949(self, ATTR(self,temp1));
  ret_val42 = STR_ap2004550586(plus_self41, plus_sarg41);
  plus_self42 = ret_val42;
  plus_sarg42 = ((STR) &name12);
  ret_val43 = STR_ap2004550586(plus_self42, plus_sarg42);
  r = ret_val43;
 }
 L22 = (ATTR(self,break1)==((ARRAYSTR) NULL));
 L231_=!(L22); 
 if (L231_) {
  plus_self43 = r;
  plus_sarg43 = ((STR) &break1);
  ret_val44 = STR_ap2004550586(plus_self43, plus_sarg43);
  plus_self44 = ret_val44;
  plus_sarg44 = CONFIG492273949(self, ATTR(self,break1));
  ret_val45 = STR_ap2004550586(plus_self44, plus_sarg44);
  plus_self45 = ret_val45;
  plus_sarg45 = ((STR) &name12);
  ret_val46 = STR_ap2004550586(plus_self45, plus_sarg45);
  r = ret_val46;
 }
 L24 = (ATTR(self,iter)==((ARRAYSTR) NULL));
 L251_=!(L24); 
 if (L251_) {
  plus_self46 = r;
  plus_sarg46 = ((STR) &iter);
  ret_val47 = STR_ap2004550586(plus_self46, plus_sarg46);
  plus_self47 = ret_val47;
  plus_sarg47 = CONFIG492273949(self, ATTR(self,iter));
  ret_val48 = STR_ap2004550586(plus_self47, plus_sarg47);
  plus_self48 = ret_val48;
  plus_sarg48 = ((STR) &name12);
  ret_val49 = STR_ap2004550586(plus_self48, plus_sarg48);
  r = ret_val49;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG458671273(CONFIG_SCANNER self) {
 STR ret_val;
 INT start1 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR str_self;
 STR ret_val2;
 BOOL L1;
 BOOL L2;
 CHAR L31_;
 CHAR L4;
 BOOL L51_;
 INT L6;
 INT L71_;
 FSTR L8;
 INT L9;
 INT L101_;
 start1 = ATTR(self,next);
 if (CONFIG915316977(self)) {
  while (1) {
   if (CONFIG915316977(self)) {
    L2 = TRUE;
   } else {
    L2 = CONFIG981438152(self);
   }
   if (L2) {
    L1 = TRUE;
   } else {
    aget_self = ATTR(self,fstr);
    aget_ind = ATTR(self,next);
    L31_=ARR((FSTR)aget_self,aget_ind); 
    ret_val1 = L31_;
    L4 = ret_val1;
    L51_=L4=='_'; 
    L1 = L51_;
   }
   if (L1) {
   }
   else {
    goto after_loop;
   }
   L6 = ATTR(self,next);
   L71_=INTPLUS(L6,1); 
   SATTR(self,next,L71_);
  }
  after_loop: ;
 }
 L8 = ATTR(self,fstr);
 L9 = ATTR(self,next);
 L101_=INTMINUS(L9,start1); 
 str_self = FSTR_s1920457899(L8, start1, L101_);
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG492273949(CONFIG_ROUT self, ARRAYSTR e) {
 STR ret_val;
 STR r;
 ARRAYSTR L11;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 INT L21 = INT_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 extern STR name1;
 INT L31_br;
 STR aL31_;
 extern STR name10;
 STR L4;
 INT L51_;
 r = ((STR) &name1);
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = e;
  L31_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   plus_self = r;
   plus_sarg = ((STR) &name10);
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   if(L21>=L31_br)  goto after_loop; 
   aL31_=ARR((ARRAYSTR)L11,L21); 
   plus_sarg1 = STR_prettyrSTR(aL31_);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   r = ret_val2;
   L51_=INTPLUS(L21,1); 
   L21 = L51_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ARR925292072(AM_ARR_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_BOO1614359222(AM_BOOL_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_CHA165359522(AM_CHAR_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FLT1597643896(AM_FLTDX_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FLT2085750594(AM_FLTD_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FLT79327789(AM_FLT_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FLT884139698(AM_FLTX_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_INT1372528330(AM_INTI_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_INT1391542618(AM_INT_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_STR690233376(AM_STR_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_VOI984518984(AM_VOID_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG1316232027(CONFIG_TBL self, STR s) {
 dCONFIG r;
 CONFIG_TBL get_self;
 STR get_s;
 dCONFIG ret_val;
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
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CONFIG_DEF size_self;
 INT ret_val5 = INT_zero;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val6;
 CONFIG_DEF aget_self1;
 INT aget_i1 = INT_zero;
 ARRAYSTR ret_val7;
 CONFIG_DEF aget_self2;
 INT aget_i2 = INT_zero;
 ARRAYSTR ret_val8;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val10;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val12;
 FILE1 r4;
 OUT create_self2;
 OUT ret_val13;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val14;
 FILE1 stdout_self4;
 FILE1 ret_val15;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val16;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 OUT create_self3;
 OUT ret_val17;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val18;
 FILE1 stdout_self6;
 FILE1 ret_val19;
 FILE1 r7;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val20;
 FILE1 r8;
 OUT create_self4;
 OUT ret_val21;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val22;
 FILE1 stdout_self8;
 FILE1 ret_val23;
 FILE1 r9;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val24;
 FILE1 r10;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 FILE1 L1;
 OB L2;
 extern STR ismiss1726337107;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L8;
 ARRAYARRAYSTR L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 ARRAYARRAYSTR L15;
 ARRAYSTR L161_;
 ARRAYSTR L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 BOOL L21;
 BOOL L221_;
 BOOL L23;
 ARRAYARRAYSTR L24;
 ARRAYSTR L251_;
 ARRAYSTR L26;
 STR L271_;
 STR L28;
 extern STR true1;
 BOOL L29;
 BOOL L301_;
 ARRAYARRAYSTR L32;
 ARRAYSTR L331_;
 ARRAYSTR L34;
 STR L351_;
 STR L36;
 extern STR false;
 BOOL L37;
 BOOL L381_;
 FILE1 L39;
 OB L40;
 extern STR should1930522270;
 FILE1 L42;
 OB L43;
 extern STR itisdefinedas;
 FILE1 L45;
 OB L46;
 FILE1 L48;
 OB L49;
 FILE1 L51;
 OB L52;
 extern STR should1930522270;
 FILE1 L54;
 OB L55;
 extern STR itisdefinedas;
 FILE1 L57;
 OB L58;
 dCONFIG L60;
 FILE1 L62;
 OB L63;
 get_self = self;
 get_s = s;
 ret_val = FMAPST558704553(ATTR(get_self,tbl), get_s);
 r = ret_val;
 if ((r==((dCONFIG) NULL))) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = s;
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
  plus_s1 = ((STR) &ismiss1726337107);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r2 = L4;
  
  SATTR(r2,fp,stdout);
  ret_val4 = r2;
  FILE_plus_STR(ret_val4, plus_s1);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 if (r==NULL) {
  goto other97;
 } else
 switch (TAG(r)) {
  case CONFIG_DEF_tag:
   size_self = ((CONFIG_DEF) r);
   L9 = ATTR(size_self,expr);
   L101_=(L9)==NULL?0:ASIZE((ARRAYARRAYSTR)L9); 
   ret_val5 = L101_;
   L11 = ret_val5;
   L121_=(L11)==(1); 
   L13 = L121_;
   L141_=!(L13); 
   if (L141_) {
    L8 = TRUE;
   } else {
    aget_self = ((CONFIG_DEF) r);
    aget_i = 0;
    L15 = ATTR(aget_self,expr);
    L161_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L15,aget_i); 
    ret_val6 = L161_;
    L17 = ret_val6;
    L181_=(L17)==NULL?0:ASIZE((ARRAYSTR)L17); 
    L19 = L181_;
    L201_=(L19)==(1); 
    L21 = L201_;
    L221_=!(L21); 
    L8 = L221_;
   }
   if (L8) {
    L7 = TRUE;
   } else {
    aget_self1 = ((CONFIG_DEF) r);
    aget_i1 = 0;
    L24 = ATTR(aget_self1,expr);
    L251_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L24,aget_i1); 
    ret_val7 = L251_;
    L26 = ret_val7;
    L271_=(STR)ARR((ARRAYSTR)L26,0); 
    L28 = L271_;
    L29 = STR_is111530248(L28, ((STR) &true1));
    L301_=!(L29); 
    if (L301_) {
     aget_self2 = ((CONFIG_DEF) r);
     aget_i2 = 0;
     L32 = ATTR(aget_self2,expr);
     L331_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L32,aget_i2); 
     ret_val8 = L331_;
     L34 = ret_val8;
     L351_=(STR)ARR((ARRAYSTR)L34,0); 
     L36 = L351_;
     L37 = STR_is111530248(L36, ((STR) &false));
     L381_=!(L37); 
     L23 = L381_;
    } else {
     L23 = FALSE;
    }
    L7 = L23;
   }
   if (L7) {
    create_self1 = ((OUT) NULL);
    ret_val9 = create_self1;
    plus_self2 = ret_val9;
    plus_s2 = s;
    stdout_self2 = ((FILE1) NULL);
    L40=ZALLOC(sizeof(struct FILE1_struct));
    if (L40==NULL) FATAL("Unable to allocate more memory");
    ((OB)L40)->header.tag=FILE1_tag;
    L39 = ((FILE1) L40);
    r3 = L39;
    
    SATTR(r3,fp,stdout);
    ret_val11 = r3;
    FILE_plus_STR(ret_val11, plus_s2);
    ret_val10 = plus_self2;
    plus_self3 = ret_val10;
    plus_s3 = ((STR) &should1930522270);
    stdout_self3 = ((FILE1) NULL);
    L43=ZALLOC(sizeof(struct FILE1_struct));
    if (L43==NULL) FATAL("Unable to allocate more memory");
    ((OB)L43)->header.tag=FILE1_tag;
    L42 = ((FILE1) L43);
    r4 = L42;
    
    SATTR(r4,fp,stdout);
    ret_val12 = r4;
    FILE_plus_STR(ret_val12, plus_s3);
    create_self2 = ((OUT) NULL);
    ret_val13 = create_self2;
    plus_self4 = ret_val13;
    plus_s4 = ((STR) &itisdefinedas);
    stdout_self4 = ((FILE1) NULL);
    L46=ZALLOC(sizeof(struct FILE1_struct));
    if (L46==NULL) FATAL("Unable to allocate more memory");
    ((OB)L46)->header.tag=FILE1_tag;
    L45 = ((FILE1) L46);
    r5 = L45;
    
    SATTR(r5,fp,stdout);
    ret_val15 = r5;
    FILE_plus_STR(ret_val15, plus_s4);
    ret_val14 = plus_self4;
    plus_self5 = ret_val14;
    plus_s5 = CONFIG1221082662(((CONFIG_DEF) r));
    stdout_self5 = ((FILE1) NULL);
    L49=ZALLOC(sizeof(struct FILE1_struct));
    if (L49==NULL) FATAL("Unable to allocate more memory");
    ((OB)L49)->header.tag=FILE1_tag;
    L48 = ((FILE1) L49);
    r6 = L48;
    
    SATTR(r6,fp,stdout);
    ret_val16 = r6;
    FILE_plus_STR(ret_val16, plus_s5);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = -1;
    exit(exit_code1);
   } break;
  default: ;
  other97: ;
   create_self3 = ((OUT) NULL);
   ret_val17 = create_self3;
   plus_self6 = ret_val17;
   plus_s6 = s;
   stdout_self6 = ((FILE1) NULL);
   L52=ZALLOC(sizeof(struct FILE1_struct));
   if (L52==NULL) FATAL("Unable to allocate more memory");
   ((OB)L52)->header.tag=FILE1_tag;
   L51 = ((FILE1) L52);
   r7 = L51;
   
   SATTR(r7,fp,stdout);
   ret_val19 = r7;
   FILE_plus_STR(ret_val19, plus_s6);
   ret_val18 = plus_self6;
   plus_self7 = ret_val18;
   plus_s7 = ((STR) &should1930522270);
   stdout_self7 = ((FILE1) NULL);
   L55=ZALLOC(sizeof(struct FILE1_struct));
   if (L55==NULL) FATAL("Unable to allocate more memory");
   ((OB)L55)->header.tag=FILE1_tag;
   L54 = ((FILE1) L55);
   r8 = L54;
   
   SATTR(r8,fp,stdout);
   ret_val20 = r8;
   FILE_plus_STR(ret_val20, plus_s7);
   create_self4 = ((OUT) NULL);
   ret_val21 = create_self4;
   plus_self8 = ret_val21;
   plus_s8 = ((STR) &itisdefinedas);
   stdout_self8 = ((FILE1) NULL);
   L58=ZALLOC(sizeof(struct FILE1_struct));
   if (L58==NULL) FATAL("Unable to allocate more memory");
   ((OB)L58)->header.tag=FILE1_tag;
   L57 = ((FILE1) L58);
   r9 = L57;
   
   SATTR(r9,fp,stdout);
   ret_val23 = r9;
   FILE_plus_STR(ret_val23, plus_s8);
   ret_val22 = plus_self8;
   plus_self9 = ret_val22;
   L60 = r;
   plus_s9 = (*dCONFIG_strrSTR[TAG(L60)])(L60);
   stdout_self9 = ((FILE1) NULL);
   L63=ZALLOC(sizeof(struct FILE1_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   ((OB)L63)->header.tag=FILE1_tag;
   L62 = ((FILE1) L63);
   r10 = L62;
   
   SATTR(r10,fp,stdout);
   ret_val24 = r10;
   FILE_plus_STR(ret_val24, plus_s9);
   exit_self2 = ((UNIX) NULL);
   exit_code2 = -1;
   exit(exit_code2);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG1491579599(CONFIG_SCANNER self) {
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val = CHAR_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 while (1) {
  while (1) {
   if (CONFIG298217444(self)) {
   }
   else {
    goto after_loop1;
   }
   aget_self = ATTR(self,fstr);
   aget_ind = ATTR(self,next);
   L11_=ARR((FSTR)aget_self,aget_ind); 
   ret_val = L11_;
   L2 = ret_val;
   L31_=L2=='\n'; 
   if (L31_) {
    L4 = ATTR(self,line1);
    L51_=INTPLUS(L4,1); 
    SATTR(self,line1,L51_);
   }
   L6 = ATTR(self,next);
   L71_=INTPLUS(L6,1); 
   SATTR(self,next,L71_);
  }
  after_loop1: ;
  if (CONFIG299011603(self)) {
   CONFIG541915266(self);
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG1796802473(CONFIG_TBL self, STR filename) {
 CONFIG_SCANNER s;
 CONFIG_SCANNER L11;
 dCONFIG e;
 dCONFIG L2;
 FMAPSTRdCONFIG L3;
 dCONFIG L4;
 STR L5;
 s = CONFIG647536087(((CONFIG_SCANNER) NULL), filename);
 {
  struct CONFIG1181711269_frame_struct other1_0;
CONFIG1181711269_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = CONFIG1181711269(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L2;
   L3 = ATTR(self,tbl);
   L4 = e;
   L5 = (*dCONFIG_namerSTR[TAG(L4)])(L4);
   SATTR(self,tbl,FMAPST2004940399(L3, L5, e));
  }
 }
 after_loop: ;
 CONFIG_TBL_check(self);
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG307797953(CONFIG_TBL self, STR s) {
 dCONFIG r;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CONFIG_TBL get_self;
 STR get_s;
 dCONFIG ret_val;
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
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CONFIG_DEF size_self;
 INT ret_val5 = INT_zero;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self2;
 OUT ret_val10;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val11;
 FILE1 stdout_self4;
 FILE1 ret_val12;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 CONFIG_DEF size_self1;
 INT ret_val14 = INT_zero;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val15;
 OUT create_self3;
 OUT ret_val16;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val17;
 FILE1 stdout_self6;
 FILE1 ret_val18;
 FILE1 r7;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val19;
 FILE1 r8;
 OUT create_self4;
 OUT ret_val20;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val21;
 FILE1 stdout_self8;
 FILE1 ret_val22;
 FILE1 r9;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val23;
 FILE1 r10;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 OUT create_self5;
 OUT ret_val24;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val25;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r11;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val27;
 FILE1 r12;
 OUT create_self6;
 OUT ret_val28;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val29;
 FILE1 stdout_self12;
 FILE1 ret_val30;
 FILE1 r13;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val31;
 FILE1 r14;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 FILE1 L3;
 OB L4;
 extern STR ismiss1738028941;
 FILE1 L6;
 OB L7;
 ARRAYARRAYSTR L9;
 INT L101_;
 INT L12;
 BOOL L131_;
 FILE1 L14;
 OB L15;
 extern STR should1447269570;
 FILE1 L17;
 OB L18;
 extern STR itisdefinedas;
 FILE1 L20;
 OB L22;
 FILE1 L24;
 OB L25;
 ARRAYARRAYSTR L28;
 INT L291_;
 INT L30;
 INT L311_;
 INT L271_,L271_m;
 INT L32;
 ARRAYARRAYSTR L33;
 ARRAYSTR L341_;
 ARRAYSTR L35;
 INT L361_;
 INT L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 FILE1 L41;
 OB L42;
 extern STR should1447269570;
 FILE1 L44;
 OB L45;
 extern STR itisdefinedas;
 FILE1 L47;
 OB L48;
 FILE1 L50;
 OB L52;
 FILE1 L54;
 OB L55;
 extern STR should14472695701;
 FILE1 L57;
 OB L58;
 extern STR itisdefinedas;
 FILE1 L60;
 OB L61;
 dCONFIG L63;
 FILE1 L64;
 OB L65;
 get_self = self;
 get_s = s;
 ret_val = FMAPST558704553(ATTR(get_self,tbl), get_s);
 r = ret_val;
 if ((r==((dCONFIG) NULL))) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = s;
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
  plus_s1 = ((STR) &ismiss1738028941);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r2 = L6;
  
  SATTR(r2,fp,stdout);
  ret_val4 = r2;
  FILE_plus_STR(ret_val4, plus_s1);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 if (r==NULL) {
  goto other96;
 } else
 switch (TAG(r)) {
  case CONFIG_DEF_tag:
   size_self = ((CONFIG_DEF) r);
   L9 = ATTR(size_self,expr);
   L101_=(L9)==NULL?0:ASIZE((ARRAYARRAYSTR)L9); 
   ret_val5 = L101_;
   L12 = ret_val5;
   L131_=(L12)==(0); 
   if (L131_) {
    create_self1 = ((OUT) NULL);
    ret_val6 = create_self1;
    plus_self2 = ret_val6;
    plus_s2 = s;
    stdout_self2 = ((FILE1) NULL);
    L15=ZALLOC(sizeof(struct FILE1_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=FILE1_tag;
    L14 = ((FILE1) L15);
    r3 = L14;
    
    SATTR(r3,fp,stdout);
    ret_val8 = r3;
    FILE_plus_STR(ret_val8, plus_s2);
    ret_val7 = plus_self2;
    plus_self3 = ret_val7;
    plus_s3 = ((STR) &should1447269570);
    stdout_self3 = ((FILE1) NULL);
    L18=ZALLOC(sizeof(struct FILE1_struct));
    if (L18==NULL) FATAL("Unable to allocate more memory");
    ((OB)L18)->header.tag=FILE1_tag;
    L17 = ((FILE1) L18);
    r4 = L17;
    
    SATTR(r4,fp,stdout);
    ret_val9 = r4;
    FILE_plus_STR(ret_val9, plus_s3);
    create_self2 = ((OUT) NULL);
    ret_val10 = create_self2;
    plus_self4 = ret_val10;
    plus_s4 = ((STR) &itisdefinedas);
    stdout_self4 = ((FILE1) NULL);
    L22=ZALLOC(sizeof(struct FILE1_struct));
    if (L22==NULL) FATAL("Unable to allocate more memory");
    ((OB)L22)->header.tag=FILE1_tag;
    L20 = ((FILE1) L22);
    r5 = L20;
    
    SATTR(r5,fp,stdout);
    ret_val12 = r5;
    FILE_plus_STR(ret_val12, plus_s4);
    ret_val11 = plus_self4;
    plus_self5 = ret_val11;
    plus_s5 = CONFIG1221082662(((CONFIG_DEF) r));
    stdout_self5 = ((FILE1) NULL);
    L25=ZALLOC(sizeof(struct FILE1_struct));
    if (L25==NULL) FATAL("Unable to allocate more memory");
    ((OB)L25)->header.tag=FILE1_tag;
    L24 = ((FILE1) L25);
    r6 = L24;
    
    SATTR(r6,fp,stdout);
    ret_val13 = r6;
    FILE_plus_STR(ret_val13, plus_s5);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = -1;
    exit(exit_code1);
   }
   {
    BOOL f_L271_ = TRUE;
    L11 = 0;
    size_self1 = ((CONFIG_DEF) r);
    L28 = ATTR(size_self1,expr);
    L291_=(L28)==NULL?0:ASIZE((ARRAYARRAYSTR)L28); 
    ret_val14 = L291_;
    L30 = ret_val14;
    L311_=INTMINUS(L30,1); 
    L21 = L311_;
    L271_=L11-1;L271_m=L21; 
    while (1) {
     if(L271_++>=L271_m)  goto after_loop; 
     i = L271_;
     aget_self = ((CONFIG_DEF) r);
     aget_i = i;
     L33 = ATTR(aget_self,expr);
     L341_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L33,aget_i); 
     ret_val15 = L341_;
     L35 = ret_val15;
     L361_=(L35)==NULL?0:ASIZE((ARRAYSTR)L35); 
     L37 = L361_;
     L381_=(L37)==(1); 
     L39 = L381_;
     L401_=!(L39); 
     if (L401_) {
      create_self3 = ((OUT) NULL);
      ret_val16 = create_self3;
      plus_self6 = ret_val16;
      plus_s6 = s;
      stdout_self6 = ((FILE1) NULL);
      L42=ZALLOC(sizeof(struct FILE1_struct));
      if (L42==NULL) FATAL("Unable to allocate more memory");
      ((OB)L42)->header.tag=FILE1_tag;
      L41 = ((FILE1) L42);
      r7 = L41;
      
      SATTR(r7,fp,stdout);
      ret_val18 = r7;
      FILE_plus_STR(ret_val18, plus_s6);
      ret_val17 = plus_self6;
      plus_self7 = ret_val17;
      plus_s7 = ((STR) &should1447269570);
      stdout_self7 = ((FILE1) NULL);
      L45=ZALLOC(sizeof(struct FILE1_struct));
      if (L45==NULL) FATAL("Unable to allocate more memory");
      ((OB)L45)->header.tag=FILE1_tag;
      L44 = ((FILE1) L45);
      r8 = L44;
      
      SATTR(r8,fp,stdout);
      ret_val19 = r8;
      FILE_plus_STR(ret_val19, plus_s7);
      create_self4 = ((OUT) NULL);
      ret_val20 = create_self4;
      plus_self8 = ret_val20;
      plus_s8 = ((STR) &itisdefinedas);
      stdout_self8 = ((FILE1) NULL);
      L48=ZALLOC(sizeof(struct FILE1_struct));
      if (L48==NULL) FATAL("Unable to allocate more memory");
      ((OB)L48)->header.tag=FILE1_tag;
      L47 = ((FILE1) L48);
      r9 = L47;
      
      SATTR(r9,fp,stdout);
      ret_val22 = r9;
      FILE_plus_STR(ret_val22, plus_s8);
      ret_val21 = plus_self8;
      plus_self9 = ret_val21;
      plus_s9 = CONFIG1221082662(((CONFIG_DEF) r));
      stdout_self9 = ((FILE1) NULL);
      L52=ZALLOC(sizeof(struct FILE1_struct));
      if (L52==NULL) FATAL("Unable to allocate more memory");
      ((OB)L52)->header.tag=FILE1_tag;
      L50 = ((FILE1) L52);
      r10 = L50;
      
      SATTR(r10,fp,stdout);
      ret_val23 = r10;
      FILE_plus_STR(ret_val23, plus_s9);
      exit_self2 = ((UNIX) NULL);
      exit_code2 = -1;
      exit(exit_code2);
     }
    }
   }
   after_loop: ; break;
  default: ;
  other96: ;
   create_self5 = ((OUT) NULL);
   ret_val24 = create_self5;
   plus_self10 = ret_val24;
   plus_s10 = s;
   stdout_self10 = ((FILE1) NULL);
   L55=ZALLOC(sizeof(struct FILE1_struct));
   if (L55==NULL) FATAL("Unable to allocate more memory");
   ((OB)L55)->header.tag=FILE1_tag;
   L54 = ((FILE1) L55);
   r11 = L54;
   
   SATTR(r11,fp,stdout);
   ret_val26 = r11;
   FILE_plus_STR(ret_val26, plus_s10);
   ret_val25 = plus_self10;
   plus_self11 = ret_val25;
   plus_s11 = ((STR) &should14472695701);
   stdout_self11 = ((FILE1) NULL);
   L58=ZALLOC(sizeof(struct FILE1_struct));
   if (L58==NULL) FATAL("Unable to allocate more memory");
   ((OB)L58)->header.tag=FILE1_tag;
   L57 = ((FILE1) L58);
   r12 = L57;
   
   SATTR(r12,fp,stdout);
   ret_val27 = r12;
   FILE_plus_STR(ret_val27, plus_s11);
   create_self6 = ((OUT) NULL);
   ret_val28 = create_self6;
   plus_self12 = ret_val28;
   plus_s12 = ((STR) &itisdefinedas);
   stdout_self12 = ((FILE1) NULL);
   L61=ZALLOC(sizeof(struct FILE1_struct));
   if (L61==NULL) FATAL("Unable to allocate more memory");
   ((OB)L61)->header.tag=FILE1_tag;
   L60 = ((FILE1) L61);
   r13 = L60;
   
   SATTR(r13,fp,stdout);
   ret_val30 = r13;
   FILE_plus_STR(ret_val30, plus_s12);
   ret_val29 = plus_self12;
   plus_self13 = ret_val29;
   L63 = r;
   plus_s13 = (*dCONFIG_strrSTR[TAG(L63)])(L63);
   stdout_self13 = ((FILE1) NULL);
   L65=ZALLOC(sizeof(struct FILE1_struct));
   if (L65==NULL) FATAL("Unable to allocate more memory");
   ((OB)L65)->header.tag=FILE1_tag;
   L64 = ((FILE1) L65);
   r14 = L64;
   
   SATTR(r14,fp,stdout);
   ret_val31 = r14;
   FILE_plus_STR(ret_val31, plus_s13);
   exit_self3 = ((UNIX) NULL);
   exit_code3 = -1;
   exit(exit_code3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG39489600(CONFIG_TBL self, STR s) {
 dCONFIG r;
 STR a;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CONFIG_TBL get_self;
 STR get_s;
 dCONFIG ret_val;
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
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r2;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CONFIG_DEF size_self;
 INT ret_val5 = INT_zero;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val6;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val8;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r4;
 OUT create_self2;
 OUT ret_val11;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val12;
 FILE1 stdout_self4;
 FILE1 ret_val13;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val14;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 CONFIG_DEF aget_self1;
 INT aget_i1 = INT_zero;
 ARRAYSTR ret_val15;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val16 = BOOL_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val17 = BOOL_zero;
 OUT create_self3;
 OUT ret_val18;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val19;
 FILE1 stdout_self6;
 FILE1 ret_val20;
 FILE1 r7;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val21;
 FILE1 r8;
 OUT create_self4;
 OUT ret_val22;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val23;
 FILE1 stdout_self8;
 FILE1 ret_val24;
 FILE1 r9;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val25;
 FILE1 r10;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 OUT create_self5;
 OUT ret_val26;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val27;
 FILE1 stdout_self10;
 FILE1 ret_val28;
 FILE1 r11;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val29;
 FILE1 r12;
 OUT create_self6;
 OUT ret_val30;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val31;
 FILE1 stdout_self12;
 FILE1 ret_val32;
 FILE1 r13;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val33;
 FILE1 r14;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 FILE1 L3;
 OB L4;
 extern STR ismiss1312503364;
 FILE1 L6;
 OB L7;
 BOOL L9;
 ARRAYARRAYSTR L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 ARRAYARRAYSTR L17;
 ARRAYSTR L181_;
 ARRAYSTR L19;
 INT L201_;
 INT L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 FILE1 L26;
 OB L27;
 extern STR should2111577461;
 FILE1 L29;
 OB L30;
 extern STR itisdefinedas;
 FILE1 L32;
 OB L33;
 FILE1 L35;
 OB L36;
 ARRAYARRAYSTR L38;
 ARRAYSTR L391_;
 ARRAYSTR L40;
 STR L411_;
 INT L43;
 INT L441_;
 INT L421_,L421_m;
 INT L45;
 BOOL L46;
 BOOL L47;
 BOOL L481_;
 BOOL L49;
 CHAR L501_;
 CHAR L52;
 BOOL L531_;
 CHAR L541_;
 CHAR L55;
 BOOL L561_;
 BOOL L57;
 CHAR L581_;
 INT L591_;
 INT L60;
 INT L611_;
 INT L62;
 BOOL L631_;
 BOOL L64;
 BOOL L651_;
 CHAR L661_;
 INT L671_;
 INT L68;
 INT L691_;
 INT L70;
 BOOL L711_;
 BOOL L72;
 BOOL L731_;
 BOOL L74;
 BOOL L751_;
 FILE1 L76;
 OB L77;
 extern STR should2111577461;
 FILE1 L79;
 OB L80;
 extern STR itisdefinedas;
 FILE1 L83;
 OB L84;
 FILE1 L86;
 OB L87;
 FILE1 L89;
 OB L90;
 extern STR should2111577461;
 FILE1 L92;
 OB L93;
 extern STR itisdefinedas;
 FILE1 L95;
 OB L96;
 dCONFIG L98;
 FILE1 L99;
 OB L100;
 get_self = self;
 get_s = s;
 ret_val = FMAPST558704553(ATTR(get_self,tbl), get_s);
 r = ret_val;
 if ((r==((dCONFIG) NULL))) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = s;
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
  plus_s1 = ((STR) &ismiss1312503364);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r2 = L6;
  
  SATTR(r2,fp,stdout);
  ret_val4 = r2;
  FILE_plus_STR(ret_val4, plus_s1);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 if (r==NULL) {
  goto other98;
 } else
 switch (TAG(r)) {
  case CONFIG_DEF_tag:
   size_self = ((CONFIG_DEF) r);
   L10 = ATTR(size_self,expr);
   L121_=(L10)==NULL?0:ASIZE((ARRAYARRAYSTR)L10); 
   ret_val5 = L121_;
   L13 = ret_val5;
   L141_=(L13)==(1); 
   L15 = L141_;
   L161_=!(L15); 
   if (L161_) {
    L9 = TRUE;
   } else {
    aget_self = ((CONFIG_DEF) r);
    aget_i = 0;
    L17 = ATTR(aget_self,expr);
    L181_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L17,aget_i); 
    ret_val6 = L181_;
    L19 = ret_val6;
    L201_=(L19)==NULL?0:ASIZE((ARRAYSTR)L19); 
    L22 = L201_;
    L231_=(L22)==(1); 
    L24 = L231_;
    L251_=!(L24); 
    L9 = L251_;
   }
   if (L9) {
    create_self1 = ((OUT) NULL);
    ret_val7 = create_self1;
    plus_self2 = ret_val7;
    plus_s2 = s;
    stdout_self2 = ((FILE1) NULL);
    L27=ZALLOC(sizeof(struct FILE1_struct));
    if (L27==NULL) FATAL("Unable to allocate more memory");
    ((OB)L27)->header.tag=FILE1_tag;
    L26 = ((FILE1) L27);
    r3 = L26;
    
    SATTR(r3,fp,stdout);
    ret_val9 = r3;
    FILE_plus_STR(ret_val9, plus_s2);
    ret_val8 = plus_self2;
    plus_self3 = ret_val8;
    plus_s3 = ((STR) &should2111577461);
    stdout_self3 = ((FILE1) NULL);
    L30=ZALLOC(sizeof(struct FILE1_struct));
    if (L30==NULL) FATAL("Unable to allocate more memory");
    ((OB)L30)->header.tag=FILE1_tag;
    L29 = ((FILE1) L30);
    r4 = L29;
    
    SATTR(r4,fp,stdout);
    ret_val10 = r4;
    FILE_plus_STR(ret_val10, plus_s3);
    create_self2 = ((OUT) NULL);
    ret_val11 = create_self2;
    plus_self4 = ret_val11;
    plus_s4 = ((STR) &itisdefinedas);
    stdout_self4 = ((FILE1) NULL);
    L33=ZALLOC(sizeof(struct FILE1_struct));
    if (L33==NULL) FATAL("Unable to allocate more memory");
    ((OB)L33)->header.tag=FILE1_tag;
    L32 = ((FILE1) L33);
    r5 = L32;
    
    SATTR(r5,fp,stdout);
    ret_val13 = r5;
    FILE_plus_STR(ret_val13, plus_s4);
    ret_val12 = plus_self4;
    plus_self5 = ret_val12;
    plus_s5 = CONFIG1221082662(((CONFIG_DEF) r));
    stdout_self5 = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r6 = L35;
    
    SATTR(r6,fp,stdout);
    ret_val14 = r6;
    FILE_plus_STR(ret_val14, plus_s5);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = 1;
    exit(exit_code1);
   }
   aget_self1 = ((CONFIG_DEF) r);
   aget_i1 = 0;
   L38 = ATTR(aget_self1,expr);
   L391_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L38,aget_i1); 
   ret_val15 = L391_;
   L40 = ret_val15;
   L411_=(STR)ARR((ARRAYSTR)L40,0); 
   a = L411_;
   {
    BOOL f_L421_ = TRUE;
    L11 = 0;
    L43 = STR_sizerINT(a);
    L441_=INTMINUS(L43,1); 
    L21 = L441_;
    L421_=L11-1;L421_m=L21; 
    while (1) {
     if(L421_++>=L421_m)  goto after_loop; 
     i = L421_;
     L481_=(i)==(0); 
     if (L481_) {
      L501_=ARR((STR)a,i); 
      L52 = L501_;
      L531_=L52=='+'; 
      if (L531_) {
       L49 = TRUE;
      } else {
       L541_=ARR((STR)a,i); 
       L55 = L541_;
       L561_=L55=='-'; 
       L49 = L561_;
      }
      L47 = L49;
     } else {
      L47 = FALSE;
     }
     if (L47) {
      L46 = TRUE;
     } else {
      L581_=ARR((STR)a,i); 
      is_lt_self = L581_;
      is_lt_c = '0';
      L591_=(INT)((unsigned char)is_lt_self); 
      L60 = L591_;
      L611_=(INT)((unsigned char)is_lt_c); 
      L62 = L611_;
      L631_=(L60)<(L62); 
      ret_val16 = L631_;
      L64 = ret_val16;
      L651_=!(L64); 
      if (L651_) {
       is_lt_self1 = '9';
       L661_=ARR((STR)a,i); 
       is_lt_c1 = L661_;
       L671_=(INT)((unsigned char)is_lt_self1); 
       L68 = L671_;
       L691_=(INT)((unsigned char)is_lt_c1); 
       L70 = L691_;
       L711_=(L68)<(L70); 
       ret_val17 = L711_;
       L72 = ret_val17;
       L731_=!(L72); 
       L57 = L731_;
      } else {
       L57 = FALSE;
      }
      L46 = L57;
     }
     L74 = L46;
     L751_=!(L74); 
     if (L751_) {
      create_self3 = ((OUT) NULL);
      ret_val18 = create_self3;
      plus_self6 = ret_val18;
      plus_s6 = s;
      stdout_self6 = ((FILE1) NULL);
      L77=ZALLOC(sizeof(struct FILE1_struct));
      if (L77==NULL) FATAL("Unable to allocate more memory");
      ((OB)L77)->header.tag=FILE1_tag;
      L76 = ((FILE1) L77);
      r7 = L76;
      
      SATTR(r7,fp,stdout);
      ret_val20 = r7;
      FILE_plus_STR(ret_val20, plus_s6);
      ret_val19 = plus_self6;
      plus_self7 = ret_val19;
      plus_s7 = ((STR) &should2111577461);
      stdout_self7 = ((FILE1) NULL);
      L80=ZALLOC(sizeof(struct FILE1_struct));
      if (L80==NULL) FATAL("Unable to allocate more memory");
      ((OB)L80)->header.tag=FILE1_tag;
      L79 = ((FILE1) L80);
      r8 = L79;
      
      SATTR(r8,fp,stdout);
      ret_val21 = r8;
      FILE_plus_STR(ret_val21, plus_s7);
      create_self4 = ((OUT) NULL);
      ret_val22 = create_self4;
      plus_self8 = ret_val22;
      plus_s8 = ((STR) &itisdefinedas);
      stdout_self8 = ((FILE1) NULL);
      L84=ZALLOC(sizeof(struct FILE1_struct));
      if (L84==NULL) FATAL("Unable to allocate more memory");
      ((OB)L84)->header.tag=FILE1_tag;
      L83 = ((FILE1) L84);
      r9 = L83;
      
      SATTR(r9,fp,stdout);
      ret_val24 = r9;
      FILE_plus_STR(ret_val24, plus_s8);
      ret_val23 = plus_self8;
      plus_self9 = ret_val23;
      plus_s9 = CONFIG1221082662(((CONFIG_DEF) r));
      stdout_self9 = ((FILE1) NULL);
      L87=ZALLOC(sizeof(struct FILE1_struct));
      if (L87==NULL) FATAL("Unable to allocate more memory");
      ((OB)L87)->header.tag=FILE1_tag;
      L86 = ((FILE1) L87);
      r10 = L86;
      
      SATTR(r10,fp,stdout);
      ret_val25 = r10;
      FILE_plus_STR(ret_val25, plus_s9);
      exit_self2 = ((UNIX) NULL);
      exit_code2 = 1;
      exit(exit_code2);
     }
    }
   }
   after_loop: ; break;
  default: ;
  other98: ;
   create_self5 = ((OUT) NULL);
   ret_val26 = create_self5;
   plus_self10 = ret_val26;
   plus_s10 = s;
   stdout_self10 = ((FILE1) NULL);
   L90=ZALLOC(sizeof(struct FILE1_struct));
   if (L90==NULL) FATAL("Unable to allocate more memory");
   ((OB)L90)->header.tag=FILE1_tag;
   L89 = ((FILE1) L90);
   r11 = L89;
   
   SATTR(r11,fp,stdout);
   ret_val28 = r11;
   FILE_plus_STR(ret_val28, plus_s10);
   ret_val27 = plus_self10;
   plus_self11 = ret_val27;
   plus_s11 = ((STR) &should2111577461);
   stdout_self11 = ((FILE1) NULL);
   L93=ZALLOC(sizeof(struct FILE1_struct));
   if (L93==NULL) FATAL("Unable to allocate more memory");
   ((OB)L93)->header.tag=FILE1_tag;
   L92 = ((FILE1) L93);
   r12 = L92;
   
   SATTR(r12,fp,stdout);
   ret_val29 = r12;
   FILE_plus_STR(ret_val29, plus_s11);
   create_self6 = ((OUT) NULL);
   ret_val30 = create_self6;
   plus_self12 = ret_val30;
   plus_s12 = ((STR) &itisdefinedas);
   stdout_self12 = ((FILE1) NULL);
   L96=ZALLOC(sizeof(struct FILE1_struct));
   if (L96==NULL) FATAL("Unable to allocate more memory");
   ((OB)L96)->header.tag=FILE1_tag;
   L95 = ((FILE1) L96);
   r13 = L95;
   
   SATTR(r13,fp,stdout);
   ret_val32 = r13;
   FILE_plus_STR(ret_val32, plus_s12);
   ret_val31 = plus_self12;
   plus_self13 = ret_val31;
   L98 = r;
   plus_s13 = (*dCONFIG_strrSTR[TAG(L98)])(L98);
   stdout_self13 = ((FILE1) NULL);
   L100=ZALLOC(sizeof(struct FILE1_struct));
   if (L100==NULL) FATAL("Unable to allocate more memory");
   ((OB)L100)->header.tag=FILE1_tag;
   L99 = ((FILE1) L100);
   r14 = L99;
   
   SATTR(r14,fp,stdout);
   ret_val33 = r14;
   FILE_plus_STR(ret_val33, plus_s13);
   exit_self3 = ((UNIX) NULL);
   exit_code3 = -1;
   exit(exit_code3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG523070343(CONFIG_TBL self, STR s, INT n) {
 dCONFIG r;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 CONFIG_TBL get_self;
 STR get_s;
 dCONFIG ret_val;
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
 dSTR plus_s2;
 OUT ret_val6;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r3;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CONFIG_DEF size_self;
 INT ret_val9 = INT_zero;
 OUT create_self1;
 OUT ret_val10;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val11;
 FILE1 stdout_self4;
 FILE1 ret_val12;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val13;
 FILE1 stdout_self5;
 FILE1 ret_val14;
 FILE1 r6;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val15;
 FILE1 stdout_self6;
 FILE1 ret_val16;
 FILE1 r7;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self2;
 OUT ret_val18;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val19;
 FILE1 stdout_self8;
 FILE1 ret_val20;
 FILE1 r9;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val21;
 FILE1 r10;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val22;
 OUT create_self3;
 OUT ret_val23;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val24;
 FILE1 stdout_self10;
 FILE1 ret_val25;
 FILE1 r11;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val26;
 FILE1 stdout_self11;
 FILE1 ret_val27;
 FILE1 r12;
 OUT plus_self12;
 dSTR plus_s12;
 OUT ret_val28;
 FILE1 stdout_self12;
 FILE1 ret_val29;
 FILE1 r13;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val30;
 FILE1 r14;
 OUT create_self4;
 OUT ret_val31;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val32;
 FILE1 stdout_self14;
 FILE1 ret_val33;
 FILE1 r15;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val34;
 FILE1 r16;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 OUT create_self5;
 OUT ret_val35;
 OUT plus_self16;
 STR plus_s16;
 OUT ret_val36;
 FILE1 stdout_self16;
 FILE1 ret_val37;
 FILE1 r17;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val38;
 FILE1 stdout_self17;
 FILE1 ret_val39;
 FILE1 r18;
 OUT plus_self18;
 dSTR plus_s18;
 OUT ret_val40;
 FILE1 stdout_self18;
 FILE1 ret_val41;
 FILE1 r19;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val42;
 FILE1 r20;
 OUT create_self6;
 OUT ret_val43;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val44;
 FILE1 stdout_self20;
 FILE1 ret_val45;
 FILE1 r21;
 OUT plus_self21;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val46;
 FILE1 r22;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 FILE1 L3;
 OB L4;
 extern STR ismiss951934200;
 FILE1 L6;
 OB L7;
 dSTR L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR strings1;
 FILE1 L15;
 OB L16;
 ARRAYARRAYSTR L18;
 INT L191_;
 INT L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 FILE1 L25;
 OB L26;
 extern STR should1603890023;
 FILE1 L28;
 OB L29;
 dSTR L31;
 OB L32;
 FILE1 L33;
 OB L34;
 extern STR string2145036127;
 FILE1 L36;
 OB L37;
 extern STR itisdefinedas;
 FILE1 L39;
 OB L40;
 FILE1 L42;
 OB L43;
 INT L461_;
 INT L451_,L451_m;
 INT L47;
 ARRAYARRAYSTR L48;
 ARRAYSTR L491_;
 ARRAYSTR L50;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 FILE1 L57;
 OB L58;
 extern STR should1603890023;
 FILE1 L60;
 OB L61;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 extern STR string2145036127;
 FILE1 L68;
 OB L69;
 extern STR itisdefinedas;
 FILE1 L71;
 OB L72;
 FILE1 L74;
 OB L75;
 FILE1 L77;
 OB L78;
 extern STR should1603890023;
 FILE1 L80;
 OB L82;
 dSTR L84;
 OB L85;
 FILE1 L86;
 OB L87;
 extern STR string21450361271;
 FILE1 L89;
 OB L90;
 extern STR itisdefinedas;
 FILE1 L92;
 OB L93;
 dCONFIG L95;
 FILE1 L96;
 OB L97;
 get_self = self;
 get_s = s;
 ret_val = FMAPST558704553(ATTR(get_self,tbl), get_s);
 r = ret_val;
 if ((r==((dCONFIG) NULL))) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = s;
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
  plus_s1 = ((STR) &ismiss951934200);
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
  L10=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  memset(L10,0,sizeof(struct INT_boxed_struct));
  ((OB)L10)->header.tag=INT_tag;
  L9 = (dSTR)((INT_boxed) L10);
  ((INT_boxed) L9)->immutable_part = n;
  plus_s2 = L9;
  stdout_self2 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r3 = L12;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_dSTR(ret_val7, plus_s2);
  ret_val6 = plus_self2;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &strings1);
  stdout_self3 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r4 = L15;
  
  SATTR(r4,fp,stdout);
  ret_val8 = r4;
  FILE_plus_STR(ret_val8, plus_s3);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 if (r==NULL) {
  goto other95;
 } else
 switch (TAG(r)) {
  case CONFIG_DEF_tag:
   size_self = ((CONFIG_DEF) r);
   L18 = ATTR(size_self,expr);
   L191_=(L18)==NULL?0:ASIZE((ARRAYARRAYSTR)L18); 
   ret_val9 = L191_;
   L20 = ret_val9;
   L221_=(L20)==(n); 
   L23 = L221_;
   L241_=!(L23); 
   if (L241_) {
    create_self1 = ((OUT) NULL);
    ret_val10 = create_self1;
    plus_self4 = ret_val10;
    plus_s4 = s;
    stdout_self4 = ((FILE1) NULL);
    L26=ZALLOC(sizeof(struct FILE1_struct));
    if (L26==NULL) FATAL("Unable to allocate more memory");
    ((OB)L26)->header.tag=FILE1_tag;
    L25 = ((FILE1) L26);
    r5 = L25;
    
    SATTR(r5,fp,stdout);
    ret_val12 = r5;
    FILE_plus_STR(ret_val12, plus_s4);
    ret_val11 = plus_self4;
    plus_self5 = ret_val11;
    plus_s5 = ((STR) &should1603890023);
    stdout_self5 = ((FILE1) NULL);
    L29=ZALLOC(sizeof(struct FILE1_struct));
    if (L29==NULL) FATAL("Unable to allocate more memory");
    ((OB)L29)->header.tag=FILE1_tag;
    L28 = ((FILE1) L29);
    r6 = L28;
    
    SATTR(r6,fp,stdout);
    ret_val14 = r6;
    FILE_plus_STR(ret_val14, plus_s5);
    ret_val13 = plus_self5;
    plus_self6 = ret_val13;
    L32=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    memset(L32,0,sizeof(struct INT_boxed_struct));
    ((OB)L32)->header.tag=INT_tag;
    L31 = (dSTR)((INT_boxed) L32);
    ((INT_boxed) L31)->immutable_part = n;
    plus_s6 = L31;
    stdout_self6 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r7 = L33;
    
    SATTR(r7,fp,stdout);
    ret_val16 = r7;
    FILE_plus_dSTR(ret_val16, plus_s6);
    ret_val15 = plus_self6;
    plus_self7 = ret_val15;
    plus_s7 = ((STR) &string2145036127);
    stdout_self7 = ((FILE1) NULL);
    L37=ZALLOC(sizeof(struct FILE1_struct));
    if (L37==NULL) FATAL("Unable to allocate more memory");
    ((OB)L37)->header.tag=FILE1_tag;
    L36 = ((FILE1) L37);
    r8 = L36;
    
    SATTR(r8,fp,stdout);
    ret_val17 = r8;
    FILE_plus_STR(ret_val17, plus_s7);
    create_self2 = ((OUT) NULL);
    ret_val18 = create_self2;
    plus_self8 = ret_val18;
    plus_s8 = ((STR) &itisdefinedas);
    stdout_self8 = ((FILE1) NULL);
    L40=ZALLOC(sizeof(struct FILE1_struct));
    if (L40==NULL) FATAL("Unable to allocate more memory");
    ((OB)L40)->header.tag=FILE1_tag;
    L39 = ((FILE1) L40);
    r9 = L39;
    
    SATTR(r9,fp,stdout);
    ret_val20 = r9;
    FILE_plus_STR(ret_val20, plus_s8);
    ret_val19 = plus_self8;
    plus_self9 = ret_val19;
    plus_s9 = CONFIG1221082662(((CONFIG_DEF) r));
    stdout_self9 = ((FILE1) NULL);
    L43=ZALLOC(sizeof(struct FILE1_struct));
    if (L43==NULL) FATAL("Unable to allocate more memory");
    ((OB)L43)->header.tag=FILE1_tag;
    L42 = ((FILE1) L43);
    r10 = L42;
    
    SATTR(r10,fp,stdout);
    ret_val21 = r10;
    FILE_plus_STR(ret_val21, plus_s9);
    exit_self1 = ((UNIX) NULL);
    exit_code1 = -1;
    exit(exit_code1);
   }
   {
    BOOL f_L451_ = TRUE;
    L11 = 0;
    L461_=INTMINUS(n,1); 
    L21 = L461_;
    L451_=L11-1;L451_m=L21; 
    while (1) {
     if(L451_++>=L451_m)  goto after_loop; 
     i = L451_;
     aget_self = ((CONFIG_DEF) r);
     aget_i = i;
     L48 = ATTR(aget_self,expr);
     L491_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L48,aget_i); 
     ret_val22 = L491_;
     L50 = ret_val22;
     L521_=(L50)==NULL?0:ASIZE((ARRAYSTR)L50); 
     L53 = L521_;
     L541_=(L53)==(1); 
     L55 = L541_;
     L561_=!(L55); 
     if (L561_) {
      create_self3 = ((OUT) NULL);
      ret_val23 = create_self3;
      plus_self10 = ret_val23;
      plus_s10 = s;
      stdout_self10 = ((FILE1) NULL);
      L58=ZALLOC(sizeof(struct FILE1_struct));
      if (L58==NULL) FATAL("Unable to allocate more memory");
      ((OB)L58)->header.tag=FILE1_tag;
      L57 = ((FILE1) L58);
      r11 = L57;
      
      SATTR(r11,fp,stdout);
      ret_val25 = r11;
      FILE_plus_STR(ret_val25, plus_s10);
      ret_val24 = plus_self10;
      plus_self11 = ret_val24;
      plus_s11 = ((STR) &should1603890023);
      stdout_self11 = ((FILE1) NULL);
      L61=ZALLOC(sizeof(struct FILE1_struct));
      if (L61==NULL) FATAL("Unable to allocate more memory");
      ((OB)L61)->header.tag=FILE1_tag;
      L60 = ((FILE1) L61);
      r12 = L60;
      
      SATTR(r12,fp,stdout);
      ret_val27 = r12;
      FILE_plus_STR(ret_val27, plus_s11);
      ret_val26 = plus_self11;
      plus_self12 = ret_val26;
      L64=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L64==NULL) FATAL("Unable to allocate more memory");
      memset(L64,0,sizeof(struct INT_boxed_struct));
      ((OB)L64)->header.tag=INT_tag;
      L63 = (dSTR)((INT_boxed) L64);
      ((INT_boxed) L63)->immutable_part = n;
      plus_s12 = L63;
      stdout_self12 = ((FILE1) NULL);
      L66=ZALLOC(sizeof(struct FILE1_struct));
      if (L66==NULL) FATAL("Unable to allocate more memory");
      ((OB)L66)->header.tag=FILE1_tag;
      L65 = ((FILE1) L66);
      r13 = L65;
      
      SATTR(r13,fp,stdout);
      ret_val29 = r13;
      FILE_plus_dSTR(ret_val29, plus_s12);
      ret_val28 = plus_self12;
      plus_self13 = ret_val28;
      plus_s13 = ((STR) &string2145036127);
      stdout_self13 = ((FILE1) NULL);
      L69=ZALLOC(sizeof(struct FILE1_struct));
      if (L69==NULL) FATAL("Unable to allocate more memory");
      ((OB)L69)->header.tag=FILE1_tag;
      L68 = ((FILE1) L69);
      r14 = L68;
      
      SATTR(r14,fp,stdout);
      ret_val30 = r14;
      FILE_plus_STR(ret_val30, plus_s13);
      create_self4 = ((OUT) NULL);
      ret_val31 = create_self4;
      plus_self14 = ret_val31;
      plus_s14 = ((STR) &itisdefinedas);
      stdout_self14 = ((FILE1) NULL);
      L72=ZALLOC(sizeof(struct FILE1_struct));
      if (L72==NULL) FATAL("Unable to allocate more memory");
      ((OB)L72)->header.tag=FILE1_tag;
      L71 = ((FILE1) L72);
      r15 = L71;
      
      SATTR(r15,fp,stdout);
      ret_val33 = r15;
      FILE_plus_STR(ret_val33, plus_s14);
      ret_val32 = plus_self14;
      plus_self15 = ret_val32;
      plus_s15 = CONFIG1221082662(((CONFIG_DEF) r));
      stdout_self15 = ((FILE1) NULL);
      L75=ZALLOC(sizeof(struct FILE1_struct));
      if (L75==NULL) FATAL("Unable to allocate more memory");
      ((OB)L75)->header.tag=FILE1_tag;
      L74 = ((FILE1) L75);
      r16 = L74;
      
      SATTR(r16,fp,stdout);
      ret_val34 = r16;
      FILE_plus_STR(ret_val34, plus_s15);
      exit_self2 = ((UNIX) NULL);
      exit_code2 = -1;
      exit(exit_code2);
     }
    }
   }
   after_loop: ; break;
  default: ;
  other95: ;
   create_self5 = ((OUT) NULL);
   ret_val35 = create_self5;
   plus_self16 = ret_val35;
   plus_s16 = s;
   stdout_self16 = ((FILE1) NULL);
   L78=ZALLOC(sizeof(struct FILE1_struct));
   if (L78==NULL) FATAL("Unable to allocate more memory");
   ((OB)L78)->header.tag=FILE1_tag;
   L77 = ((FILE1) L78);
   r17 = L77;
   
   SATTR(r17,fp,stdout);
   ret_val37 = r17;
   FILE_plus_STR(ret_val37, plus_s16);
   ret_val36 = plus_self16;
   plus_self17 = ret_val36;
   plus_s17 = ((STR) &should1603890023);
   stdout_self17 = ((FILE1) NULL);
   L82=ZALLOC(sizeof(struct FILE1_struct));
   if (L82==NULL) FATAL("Unable to allocate more memory");
   ((OB)L82)->header.tag=FILE1_tag;
   L80 = ((FILE1) L82);
   r18 = L80;
   
   SATTR(r18,fp,stdout);
   ret_val39 = r18;
   FILE_plus_STR(ret_val39, plus_s17);
   ret_val38 = plus_self17;
   plus_self18 = ret_val38;
   L85=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L85==NULL) FATAL("Unable to allocate more memory");
   memset(L85,0,sizeof(struct INT_boxed_struct));
   ((OB)L85)->header.tag=INT_tag;
   L84 = (dSTR)((INT_boxed) L85);
   ((INT_boxed) L84)->immutable_part = n;
   plus_s18 = L84;
   stdout_self18 = ((FILE1) NULL);
   L87=ZALLOC(sizeof(struct FILE1_struct));
   if (L87==NULL) FATAL("Unable to allocate more memory");
   ((OB)L87)->header.tag=FILE1_tag;
   L86 = ((FILE1) L87);
   r19 = L86;
   
   SATTR(r19,fp,stdout);
   ret_val41 = r19;
   FILE_plus_dSTR(ret_val41, plus_s18);
   ret_val40 = plus_self18;
   plus_self19 = ret_val40;
   plus_s19 = ((STR) &string21450361271);
   stdout_self19 = ((FILE1) NULL);
   L90=ZALLOC(sizeof(struct FILE1_struct));
   if (L90==NULL) FATAL("Unable to allocate more memory");
   ((OB)L90)->header.tag=FILE1_tag;
   L89 = ((FILE1) L90);
   r20 = L89;
   
   SATTR(r20,fp,stdout);
   ret_val42 = r20;
   FILE_plus_STR(ret_val42, plus_s19);
   create_self6 = ((OUT) NULL);
   ret_val43 = create_self6;
   plus_self20 = ret_val43;
   plus_s20 = ((STR) &itisdefinedas);
   stdout_self20 = ((FILE1) NULL);
   L93=ZALLOC(sizeof(struct FILE1_struct));
   if (L93==NULL) FATAL("Unable to allocate more memory");
   ((OB)L93)->header.tag=FILE1_tag;
   L92 = ((FILE1) L93);
   r21 = L92;
   
   SATTR(r21,fp,stdout);
   ret_val45 = r21;
   FILE_plus_STR(ret_val45, plus_s20);
   ret_val44 = plus_self20;
   plus_self21 = ret_val44;
   L95 = r;
   plus_s21 = (*dCONFIG_strrSTR[TAG(L95)])(L95);
   stdout_self21 = ((FILE1) NULL);
   L97=ZALLOC(sizeof(struct FILE1_struct));
   if (L97==NULL) FATAL("Unable to allocate more memory");
   ((OB)L97)->header.tag=FILE1_tag;
   L96 = ((FILE1) L97);
   r22 = L96;
   
   SATTR(r22,fp,stdout);
   ret_val46 = r22;
   FILE_plus_STR(ret_val46, plus_s21);
   exit_self3 = ((UNIX) NULL);
   exit_code3 = -1;
   exit(exit_code3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG541915266(CONFIG_SCANNER self) {
 CONFIG_SCANNER eof_self;
 BOOL ret_val = BOOL_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 CHAR L91_;
 CHAR L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 INT L18;
 INT L191_;
 if (CONFIG299011603(self)) {
  while (1) {
   eof_self = self;
   L2 = ATTR(eof_self,next);
   L3 = FSTR_sizerINT(ATTR(eof_self,fstr));
   L41_=(L2)<(L3); 
   L5 = L41_;
   L61_=!(L5); 
   ret_val = L61_;
   L7 = ret_val;
   L81_=!(L7); 
   if (L81_) {
    aget_self = ATTR(self,fstr);
    aget_ind = ATTR(self,next);
    L91_=ARR((FSTR)aget_self,aget_ind); 
    ret_val1 = L91_;
    L10 = ret_val1;
    L111_=L10=='\n'; 
    L12 = L111_;
    L131_=!(L12); 
    L1 = L131_;
   } else {
    L1 = FALSE;
   }
   if (L1) {
   }
   else {
    goto after_loop;
   }
   L14 = ATTR(self,next);
   L151_=INTPLUS(L14,1); 
   SATTR(self,next,L151_);
  }
  after_loop: ;
  L16 = ATTR(self,next);
  L171_=INTPLUS(L16,1); 
  SATTR(self,next,L171_);
  L18 = ATTR(self,line1);
  L191_=INTPLUS(L18,1); 
  SATTR(self,line1,L191_);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONFIG_TBL_check(CONFIG_TBL self) {
 extern STR C_COMPILER;
 extern STR MAKE_COMMAND;
 extern STR OBJECT_EXT;
 extern STR LIB_EXT;
 extern STR C_EXT;
 extern STR SHELL_SEP;
 extern STR EXEC_OPTION;
 extern STR CC_DEBUG_FLAG;
 extern STR CC_OPTIMIZE_FLAG;
 extern STR MAKE_V183621898;
 extern STR CC_PROLIX_FLAG;
 extern STR LINK_OPTIONS;
 extern STR GC_LINK;
 extern STR BUILTIN_CLASSES;
 extern STR REFERENCE_FREE;
 extern STR PLATFORMS;
 extern STR DEFAULT_PLATFORM;
 extern STR NULL_SEGFAULTS1;
 extern STR SEPARA438130706;
 extern STR THREADS;
 extern STR DISTRIBUTED;
 extern STR ZONES1;
 extern STR TRACE11;
 extern STR LIBRARY;
 extern STR ATOMIC_CLASSES;
 extern STR POLLING;
 CONFIG523070343(self, ((STR) &C_COMPILER), 1);
 CONFIG523070343(self, ((STR) &MAKE_COMMAND), 1);
 CONFIG523070343(self, ((STR) &OBJECT_EXT), 1);
 CONFIG523070343(self, ((STR) &LIB_EXT), 1);
 CONFIG523070343(self, ((STR) &C_EXT), 1);
 CONFIG523070343(self, ((STR) &SHELL_SEP), 1);
 CONFIG523070343(self, ((STR) &EXEC_OPTION), 1);
 CONFIG523070343(self, ((STR) &CC_DEBUG_FLAG), 2);
 CONFIG523070343(self, ((STR) &CC_OPTIMIZE_FLAG), 2);
 CONFIG523070343(self, ((STR) &MAKE_V183621898), 2);
 CONFIG523070343(self, ((STR) &CC_PROLIX_FLAG), 2);
 CONFIG523070343(self, ((STR) &LINK_OPTIONS), 1);
 CONFIG523070343(self, ((STR) &GC_LINK), 1);
 CONFIG307797953(self, ((STR) &BUILTIN_CLASSES));
 CONFIG307797953(self, ((STR) &REFERENCE_FREE));
 CONFIG307797953(self, ((STR) &PLATFORMS));
 CONFIG307797953(self, ((STR) &DEFAULT_PLATFORM));
 CONFIG1316232027(self, ((STR) &NULL_SEGFAULTS1));
 CONFIG1316232027(self, ((STR) &SEPARA438130706));
 CONFIG1316232027(self, ((STR) &THREADS));
 CONFIG1316232027(self, ((STR) &DISTRIBUTED));
 CONFIG1316232027(self, ((STR) &ZONES1));
 CONFIG1316232027(self, ((STR) &TRACE11));
 CONFIG523070343(self, ((STR) &LIBRARY), 2);
 CONFIG307797953(self, ((STR) &ATOMIC_CLASSES));
 CONFIG39489600(self, ((STR) &POLLING));
}


#undef IS_ITER
#define IS_ITER 1

FLISTSTR CONFIG1589956977(CONFIG1589956977_frame frame) {
 FLISTSTR dummy = ((FLISTSTR) NULL);
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 frame->r = ((STR) NULL);
 while (1) {
  CONFIG1491579599(frame->self);
  frame->aget_self = ATTR(frame->self,fstr);
  frame->aget_ind = ATTR(frame->self,next);
  L11_=ARR((FSTR)frame->aget_self,frame->aget_ind); 
  frame->ret_val = L11_;
  L2 = frame->ret_val;
  L31_=L2==';'; 
  if (L31_) {
   L4 = ATTR(frame->self,next);
   L51_=INTPLUS(L4,1); 
   SATTR(frame->self,next,L51_);
   frame->state = -1;
   return;
  }
  frame->state = 1;
  return CONFIG666119611(frame->self);
  state1:;
 }
 after_loop: ;
 L6 = ATTR(frame->self,next);
 L71_=INTPLUS(L6,1); 
 SATTR(frame->self,next,L71_);
 CONFIG1491579599(frame->self);
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR CONFIG875430285(CONFIG875430285_frame frame) {
 STR dummy = ((STR) NULL);
 INT L1;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 BOOL L10;
 BOOL L11;
 CHAR L121_;
 CHAR L13;
 BOOL L141_;
 INT L15;
 INT L161_;
 BOOL L17;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 BOOL L21;
 BOOL L221_;
 INT L23;
 BOOL L241_;
 FILE1 L25;
 OB L26;
 extern STR name21;
 FILE1 L28;
 OB L29;
 dSTR L32;
 OB L33;
 FILE1 L34;
 OB L35;
 extern STR errore529711115;
 FILE1 L37;
 OB L38;
 FSTR L40;
 INT L41;
 INT L421_;
 INT L43;
 INT L441_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 while (1) {
  CONFIG1491579599(frame->self);
  frame->eof_self = frame->self;
  L1 = ATTR(frame->eof_self,next);
  L2 = FSTR_sizerINT(ATTR(frame->eof_self,fstr));
  L31_=(L1)<(L2); 
  L4 = L31_;
  L51_=!(L4); 
  frame->ret_val = L51_;
  if (frame->ret_val) {
   frame->state = -1;
   return;
  }
  frame->start1 = ATTR(frame->self,next);
  if (CONFIG1864245349(frame->self)) {
   L6 = TRUE;
  } else {
   frame->aget_self = ATTR(frame->self,fstr);
   frame->aget_ind = ATTR(frame->self,next);
   L71_=ARR((FSTR)frame->aget_self,frame->aget_ind); 
   frame->ret_val1 = L71_;
   L8 = frame->ret_val1;
   L91_=L8=='_'; 
   L6 = L91_;
  }
  if (L6) {
   while (1) {
    if (CONFIG1864245349(frame->self)) {
     L11 = TRUE;
    } else {
     L11 = CONFIG981438152(frame->self);
    }
    if (L11) {
     L10 = TRUE;
    } else {
     frame->aget_self1 = ATTR(frame->self,fstr);
     frame->aget_ind1 = ATTR(frame->self,next);
     L121_=ARR((FSTR)frame->aget_self1,frame->aget_ind1); 
     frame->ret_val2 = L121_;
     L13 = frame->ret_val2;
     L141_=L13=='_'; 
     L10 = L141_;
    }
    if (L10) {
    }
    else {
     goto after_loop1;
    }
    L15 = ATTR(frame->self,next);
    L161_=INTPLUS(L15,1); 
    SATTR(frame->self,next,L161_);
   }
   after_loop1: ;
  }
  frame->aget_self2 = ATTR(frame->self,fstr);
  frame->aget_ind2 = ATTR(frame->self,next);
  L181_=ARR((FSTR)frame->aget_self2,frame->aget_ind2); 
  frame->ret_val3 = L181_;
  L19 = frame->ret_val3;
  L201_=L19==':'; 
  L21 = L201_;
  L221_=!(L21); 
  if (L221_) {
   L17 = TRUE;
  } else {
   L23 = ATTR(frame->self,next);
   L241_=(L23)==(frame->start1); 
   L17 = L241_;
  }
  if (L17) {
   frame->create_self = ((OUT) NULL);
   frame->ret_val4 = frame->create_self;
   frame->plus_self3 = frame->ret_val4;
   frame->plus_s = ATTR(frame->self,file1);
   frame->stdout_self = ((FILE1) NULL);
   L26=ZALLOC(sizeof(struct FILE1_struct));
   if (L26==NULL) FATAL("Unable to allocate more memory");
   ((OB)L26)->header.tag=FILE1_tag;
   L25 = ((FILE1) L26);
   frame->r = L25;
   
   SATTR(frame->r,fp,stdout);
   frame->ret_val5 = frame->r;
   FILE_plus_STR(frame->ret_val5, frame->plus_s);
   frame->ret_val6 = frame->plus_self3;
   frame->plus_self2 = frame->ret_val6;
   frame->plus_s1 = ((STR) &name21);
   frame->stdout_self1 = ((FILE1) NULL);
   L29=ZALLOC(sizeof(struct FILE1_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=FILE1_tag;
   L28 = ((FILE1) L29);
   frame->r1 = L28;
   
   SATTR(frame->r1,fp,stdout);
   frame->ret_val7 = frame->r1;
   FILE_plus_STR(frame->ret_val7, frame->plus_s1);
   frame->ret_val8 = frame->plus_self2;
   frame->plus_self1 = frame->ret_val8;
   L33=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   memset(L33,0,sizeof(struct INT_boxed_struct));
   ((OB)L33)->header.tag=INT_tag;
   L32 = (dSTR)((INT_boxed) L33);
   ((INT_boxed) L32)->immutable_part = ATTR(frame->self,line1);
   frame->plus_s2 = L32;
   frame->stdout_self2 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   frame->r2 = L34;
   
   SATTR(frame->r2,fp,stdout);
   frame->ret_val9 = frame->r2;
   FILE_plus_dSTR(frame->ret_val9, frame->plus_s2);
   frame->ret_val10 = frame->plus_self1;
   frame->plus_self = frame->ret_val10;
   frame->plus_s3 = ((STR) &errore529711115);
   frame->stdout_self3 = ((FILE1) NULL);
   L38=ZALLOC(sizeof(struct FILE1_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=FILE1_tag;
   L37 = ((FILE1) L38);
   frame->r3 = L37;
   
   SATTR(frame->r3,fp,stdout);
   frame->ret_val11 = frame->r3;
   FILE_plus_STR(frame->ret_val11, frame->plus_s3);
   frame->exit_self = ((UNIX) NULL);
   frame->exit_code = -1;
   exit(frame->exit_code);
  }
  L40 = ATTR(frame->self,fstr);
  L41 = ATTR(frame->self,next);
  L421_=INTMINUS(L41,frame->start1); 
  frame->str_self = FSTR_s1920457899(L40, frame->start1, L421_);
  frame->ret_val12 = STR_fr1097270334(((STR) NULL), frame->str_self);
  frame->uid1 = frame->ret_val12;
  L43 = ATTR(frame->self,next);
  L441_=INTPLUS(L43,1); 
  SATTR(frame->self,next,L441_);
  frame->state = 1;
  return frame->uid1;
  state1:;
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

dCONFIG CONFIG1181711269(CONFIG1181711269_frame frame) {
 dCONFIG dummy = ((dCONFIG) NULL);
 STR L1;
 FLISTSTR L3;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  case 3: goto state3;
  }
 state0:;
 frame->exprs = ((FLISTFLISTSTR) NULL);
 SATTR(frame->self,next,0);
 SATTR(frame->self,line1,1);
 {
  frame->nested1 = OBALLOC(CONFIG875430285_frame);
  frame->nested1->state = 0;
  while (1) {
   if (frame->nested1->state == 0) {
    frame->L21 = frame->self;
    frame->nested1->self = frame->L21;
   }
   L1 = CONFIG875430285(frame->nested1);
   if (frame->nested1->state == -1) {
    goto after_loop;
   }
   frame->id = L1;
   frame->exprs = ((FLISTFLISTSTR) NULL);
   {
    struct CONFIG1589956977_frame_struct other2_0;

    frame->nested2 = &other2_0;
    frame->nested2->state = 0;
    while (1) {
     if (frame->nested2->state == 0) {
      frame->L41 = frame->self;
      frame->nested2->self = frame->L41;
     }
     L3 = CONFIG1589956977(frame->nested2);
     if (frame->nested2->state == -1) {
      goto after_loop1;
     }
     frame->exprs = FLISTF630314943(frame->exprs, L3);
    }
   }
   after_loop1: ;
   if (CONFIG1490450026(((CONFIG_ROUT) NULL), frame->exprs)) {
    frame->state = 1;
    return ((dCONFIG) CONFIG725916253(((CONFIG_ROUT) NULL), frame->id, frame->exprs));
    state1:;
   }
   else {
    if (CONFIG536838194(((CONFIG_ROUT) NULL), frame->exprs)) {
     frame->state = 2;
     return ((dCONFIG) CONFIG725916253(((CONFIG_ROUT) NULL), frame->id, frame->exprs));
     state2:;
    }
    else {
     frame->state = 3;
     return ((dCONFIG) CONFIG2114270854(((CONFIG_DEF) NULL), frame->id, frame->exprs));
     state3:;
    }
   }
  }
 }
 after_loop: ;
 ZFREE(frame->nested1); frame->nested1 = NULL;
 frame->state = -1;
 return dummy;
}
