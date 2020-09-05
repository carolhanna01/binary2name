#include "sather.h"

/* Layouts */

typedef struct dELTAM373291377_struct {
 OB_HEADER header;
 } *dELTAM373291377;

typedef struct dELTAM_CURSOR_struct {
 OB_HEADER header;
 } *dELTAM_CURSOR;

typedef struct dELTAM1985205111_struct {
 OB_HEADER header;
 } *dELTAM1985205111;

typedef struct dELTARRAYBOOL_struct {
 OB_HEADER header;
 } *dELTARRAYBOOL;

typedef struct dELTFL1838879018_struct {
 OB_HEADER header;
 } *dELTFL1838879018;

typedef struct dELTTUPdOBSTR_struct {
 OB_HEADER header;
 } *dELTTUPdOBSTR;

typedef struct dELTTU1827781436_struct {
 OB_HEADER header;
 } *dELTTU1827781436;

typedef struct dELTTUPdTPINT_struct {
 OB_HEADER header;
 } *dELTTUPdTPINT;

typedef struct dELTTU847844253_struct {
 OB_HEADER header;
 } *dELTTU847844253;

typedef struct dELTTU1754236182_struct {
 OB_HEADER header;
 } *dELTTU1754236182;

typedef struct dELTTUPIDENTdTP_struct {
 OB_HEADER header;
 } *dELTTUPIDENTdTP;

typedef struct dELTTUPIDENTINT_struct {
 OB_HEADER header;
 } *dELTTUPIDENTINT;

typedef struct dELTTUPSIGINT_struct {
 OB_HEADER header;
 } *dELTTUPSIGINT;

typedef struct dELTTUPSIGSIG_struct {
 OB_HEADER header;
 } *dELTTUPSIGSIG;

typedef struct dELTTUPSIGSTR_struct {
 OB_HEADER header;
 } *dELTTUPSIGSTR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ELT_AL92619562_struct {/* layout for ELT_ALG{AM_CURSOR_POS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL92619562;

typedef struct ELT_ALGAM_CURSOR_struct {/* layout for ELT_ALG{AM_CURSOR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGAM_CURSOR;

typedef struct ELT_AL1519294172_struct {/* layout for ELT_ALG{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1519294172;

typedef struct ELT_ALGARRAYBOOL_struct {/* layout for ELT_ALG{ARRAY{BOOL}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGARRAYBOOL;

typedef struct ELT_AL2101252324_struct {/* layout for ELT_ALG{FLIST{$AM_CONST}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2101252324;

typedef struct ELT_ALGTUPdOBSTR_struct {/* layout for ELT_ALG{TUP{$OB,STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPdOBSTR;

typedef struct ELT_AL1361870497_struct {/* layout for ELT_ALG{TUP{$TP,$LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1361870497;

typedef struct ELT_ALGTUPdTPINT_struct {/* layout for ELT_ALG{TUP{$TP,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGTUPdTPINT;

typedef struct ELT_AL1230514763_struct {/* layout for ELT_ALG{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1230514763;

typedef struct ELT_AL1371565672_struct {/* layout for ELT_ALG{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1371565672;

typedef struct ELT_AL655625458_struct {/* layout for ELT_ALG{TUP{IDENT,$TP}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL655625458;

typedef struct ELT_AL655438175_struct {/* layout for ELT_ALG{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL655438175;

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

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

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

typedef struct TUPdTPINT_struct {/* layout for TUP{$TP,INT} */
 struct dTP_struct *t1;
 INT t2;
 } TUPdTPINT;
static TUPdTPINT TUPdTPINT_zero;

typedef struct TUPdTPINT_boxed_struct {
 OB_HEADER header;
 TUPdTPINT immutable_part;
 } *TUPdTPINT_boxed;

typedef struct TUPAM_667892060_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1677815802_struct *t2;
 } TUPAM_667892060;
static TUPAM_667892060 TUPAM_667892060_zero;

typedef struct TUPAM_667892060_boxed_struct {
 OB_HEADER header;
 TUPAM_667892060 immutable_part;
 } *TUPAM_667892060_boxed;

typedef struct TUPAM_1024994801_struct {/* layout for TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 struct AM_ROUT_DEF_struct *t1;
 struct FLISTA1409846210_struct *t2;
 } TUPAM_1024994801;
static TUPAM_1024994801 TUPAM_1024994801_zero;

typedef struct TUPAM_1024994801_boxed_struct {
 OB_HEADER header;
 TUPAM_1024994801 immutable_part;
 } *TUPAM_1024994801_boxed;

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

typedef struct dELTTU975440397_frame_struct {
 INT state;
 dELTTUPdOBSTR self;
 } *dELTTU975440397_frame;

/* Dispatch table entry for $ELT{TUP{$OB,STR}}::elt!:TUP{$OB,STR} */
typedef struct dELTTU975440397_entry {
const TUPdOBSTR (*iter)(dELTTU975440397_frame frame);
const void* (*alloc_frame)();
} dELTTU975440397_entry;

typedef struct dELTTU1923274911_frame_struct {
 INT state;
 dELTTU1827781436 self;
 } *dELTTU1923274911_frame;

/* Dispatch table entry for $ELT{TUP{$TP,$LAYOUT}}::elt!:TUP{$TP,$LAYOUT} */
typedef struct dELTTU1923274911_entry {
const TUPdTPdLAYOUT (*iter)(dELTTU1923274911_frame frame);
const void* (*alloc_frame)();
} dELTTU1923274911_entry;

typedef struct dELTTU72816407_frame_struct {
 INT state;
 dELTTUPdTPINT self;
 } *dELTTU72816407_frame;

/* Dispatch table entry for $ELT{TUP{$TP,INT}}::elt!:TUP{$TP,INT} */
typedef struct dELTTU72816407_entry {
const TUPdTPINT (*iter)(dELTTU72816407_frame frame);
const void* (*alloc_frame)();
} dELTTU72816407_entry;

typedef struct dELTTU202706072_frame_struct {
 INT state;
 dELTTU847844253 self;
 } *dELTTU202706072_frame;

/* Dispatch table entry for $ELT{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}}::elt!:TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
typedef struct dELTTU202706072_entry {
const TUPAM_667892060 (*iter)(dELTTU202706072_frame frame);
const void* (*alloc_frame)();
} dELTTU202706072_entry;

typedef struct dELTTU225750038_frame_struct {
 INT state;
 dELTTU1754236182 self;
 } *dELTTU225750038_frame;

/* Dispatch table entry for $ELT{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}}::elt!:TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
typedef struct dELTTU225750038_entry {
const TUPAM_1024994801 (*iter)(dELTTU225750038_frame frame);
const void* (*alloc_frame)();
} dELTTU225750038_entry;

typedef struct dELTTU590857063_frame_struct {
 INT state;
 dELTTUPIDENTdTP self;
 } *dELTTU590857063_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,$TP}}::elt!:TUP{IDENT,$TP} */
typedef struct dELTTU590857063_entry {
const TUPIDENTdTP (*iter)(dELTTU590857063_frame frame);
const void* (*alloc_frame)();
} dELTTU590857063_entry;

typedef struct dELTTU1837273309_frame_struct {
 INT state;
 dELTTUPIDENTINT self;
 } *dELTTU1837273309_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,INT}}::elt!:TUP{IDENT,INT} */
typedef struct dELTTU1837273309_entry {
const TUPIDENTINT (*iter)(dELTTU1837273309_frame frame);
const void* (*alloc_frame)();
} dELTTU1837273309_entry;

typedef struct dELTTU1973395837_frame_struct {
 INT state;
 dELTTUPSIGINT self;
 } *dELTTU1973395837_frame;

/* Dispatch table entry for $ELT{TUP{SIG,INT}}::elt!:TUP{SIG,INT} */
typedef struct dELTTU1973395837_entry {
const TUPSIGINT (*iter)(dELTTU1973395837_frame frame);
const void* (*alloc_frame)();
} dELTTU1973395837_entry;

typedef struct dELTTU1525626981_frame_struct {
 INT state;
 dELTTUPSIGSIG self;
 } *dELTTU1525626981_frame;

/* Dispatch table entry for $ELT{TUP{SIG,SIG}}::elt!:TUP{SIG,SIG} */
typedef struct dELTTU1525626981_entry {
const TUPSIGSIG (*iter)(dELTTU1525626981_frame frame);
const void* (*alloc_frame)();
} dELTTU1525626981_entry;

typedef struct dELTTU2071468085_frame_struct {
 INT state;
 dELTTUPSIGSTR self;
 } *dELTTU2071468085_frame;

/* Dispatch table entry for $ELT{TUP{SIG,STR}}::elt!:TUP{SIG,STR} */
typedef struct dELTTU2071468085_entry {
const TUPSIGSTR (*iter)(dELTTU2071468085_frame frame);
const void* (*alloc_frame)();
} dELTTU2071468085_entry;

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

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct FLISTdAM_CONST_struct {/* layout for FLIST{$AM_CONST} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_CONST_struct *arr_part[1];
 } *FLISTdAM_CONST;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct dELTAM2044693667_frame_struct {
 INT state;
 dELTAM373291377 self;
 } *dELTAM2044693667_frame;

/* Dispatch table entry for $ELT{AM_CURSOR_POS}::elt!:AM_CURSOR_POS */
typedef struct dELTAM2044693667_entry {
const AM_CURSOR_POS (*iter)(dELTAM2044693667_frame frame);
const void* (*alloc_frame)();
} dELTAM2044693667_entry;

typedef struct dELTAM1280476813_frame_struct {
 INT state;
 dELTAM_CURSOR self;
 } *dELTAM1280476813_frame;

/* Dispatch table entry for $ELT{AM_CURSOR}::elt!:AM_CURSOR */
typedef struct dELTAM1280476813_entry {
const AM_CURSOR (*iter)(dELTAM1280476813_frame frame);
const void* (*alloc_frame)();
} dELTAM1280476813_entry;

typedef struct dELTAM128970917_frame_struct {
 INT state;
 dELTAM1985205111 self;
 } *dELTAM128970917_frame;

/* Dispatch table entry for $ELT{AM_LOCAL_EXPR}::elt!:AM_LOCAL_EXPR */
typedef struct dELTAM128970917_entry {
const AM_LOCAL_EXPR (*iter)(dELTAM128970917_frame frame);
const void* (*alloc_frame)();
} dELTAM128970917_entry;

typedef struct dELTAR657435157_frame_struct {
 INT state;
 dELTARRAYBOOL self;
 } *dELTAR657435157_frame;

/* Dispatch table entry for $ELT{ARRAY{BOOL}}::elt!:ARRAY{BOOL} */
typedef struct dELTAR657435157_entry {
const ARRAYBOOL (*iter)(dELTAR657435157_frame frame);
const void* (*alloc_frame)();
} dELTAR657435157_entry;

typedef struct dELTFL688066606_frame_struct {
 INT state;
 dELTFL1838879018 self;
 } *dELTFL688066606_frame;

/* Dispatch table entry for $ELT{FLIST{$AM_CONST}}::elt!:FLIST{$AM_CONST} */
typedef struct dELTFL688066606_entry {
const FLISTdAM_CONST (*iter)(dELTFL688066606_frame frame);
const void* (*alloc_frame)();
} dELTFL688066606_entry;

#include "tags.h"

/* Globals */


/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dELTAM1280476813_entry *dELTAM1280476813;

extern RETURNED_CONST dELTAM128970917_entry *dELTAM128970917;

extern RETURNED_CONST dELTAM2044693667_entry *dELTAM2044693667;

extern RETURNED_CONST dELTAR657435157_entry *dELTAR657435157;

extern RETURNED_CONST dELTFL688066606_entry *dELTFL688066606;

extern RETURNED_CONST dELTTU1525626981_entry *dELTTU1525626981;

extern RETURNED_CONST dELTTU1837273309_entry *dELTTU1837273309;

extern RETURNED_CONST dELTTU1923274911_entry *dELTTU1923274911;

extern RETURNED_CONST dELTTU1973395837_entry *dELTTU1973395837;

extern RETURNED_CONST dELTTU202706072_entry *dELTTU202706072;

extern RETURNED_CONST dELTTU2071468085_entry *dELTTU2071468085;

extern RETURNED_CONST dELTTU225750038_entry *dELTTU225750038;

extern RETURNED_CONST dELTTU590857063_entry *dELTTU590857063;

extern RETURNED_CONST dELTTU72816407_entry *dELTTU72816407;

extern RETURNED_CONST dELTTU975440397_entry *dELTTU975440397;
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
STR ELT_AL101060479(ELT_AL2101252324, dELTFL1838879018);
STR ELT_AL1013957530(ELT_ALGTUPSIGSTR, OB, INT);
STR ELT_AL1046589086(ELT_ALGARRAYBOOL, OB, INT);
STR ELT_AL1069121223(ELT_AL1371565672, OB, INT);
STR ELT_AL1119786764(ELT_ALGAM_CURSOR, dELTAM_CURSOR);
STR ELT_AL1209708374(ELT_AL1361870497, OB, INT);
STR ELT_AL1237867596(ELT_ALGTUPdOBSTR, dELTTUPdOBSTR);
STR ELT_AL1238979875(ELT_AL2101252324, OB, INT);
STR ELT_AL127733498(ELT_AL1519294172, dELTAM1985205111);
STR ELT_AL1289352948(ELT_ALGTUPdOBSTR, OB, INT);
STR ELT_AL130599470(ELT_ALGTUPSIGSIG, OB, INT);
STR ELT_AL1436404424(ELT_ALGTUPSIGINT, OB, INT);
STR ELT_AL1458120312(ELT_ALGTUPSIGSTR, dELTTUPSIGSTR);
STR ELT_AL1651157393(ELT_AL92619562, OB, INT);
STR ELT_AL1813778326(ELT_AL1230514763, OB, INT);
STR ELT_AL1852369832(ELT_ALGARRAYBOOL, dELTARRAYBOOL);
STR ELT_AL1978058288(ELT_ALGTUPSIGSIG, dELTTUPSIGSIG);
STR ELT_AL1989366167(ELT_AL655625458, OB, INT);
STR ELT_AL1993054651(ELT_ALGTUPdTPINT, OB, INT);
STR ELT_AL2041744410(ELT_AL655438175, OB, INT);
STR ELT_AL2046630250(ELT_ALGTUPdTPINT, dELTTUPdTPINT);
STR ELT_AL2080356652(ELT_ALGAM_CURSOR, OB, INT);
STR ELT_AL378639519(ELT_AL1230514763, dELTTU847844253);
STR ELT_AL429905904(ELT_AL655438175, dELTTUPIDENTINT);
STR ELT_AL432174165(ELT_AL1519294172, OB, INT);
STR ELT_AL510829178(ELT_AL655625458, dELTTUPIDENTdTP);
STR ELT_AL649416844(ELT_ALGTUPSIGINT, dELTTUPSIGINT);
STR ELT_AL741225494(ELT_AL92619562, dELTAM373291377);
STR ELT_AL772038856(ELT_AL1361870497, dELTTU1827781436);
STR ELT_AL966666787(ELT_AL1371565672, dELTTU1754236182);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

STR ELT_AL101060479(ELT_AL2101252324 self, dELTFL1838879018 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTFL1838879018 L11;
 FLISTdAM_CONST e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTFL1838879018 L2;
 FLISTdAM_CONST L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTFL688066606_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTFL688066606_frame) (*dELTFL688066606[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTFL688066606[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1238979875(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1238979875(self, ((OB) e), i));
   }
   L61_=INTPLUS(i,1); 
   i = L61_;
  }
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

STR ELT_AL1013957530(ELT_ALGTUPSIGSTR self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other82;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other82: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1046589086(ELT_ALGARRAYBOOL self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other31;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other31: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1069121223(ELT_AL1371565672 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other71;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other71: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1119786764(ELT_ALGAM_CURSOR self, dELTAM_CURSOR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM_CURSOR L11;
 AM_CURSOR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM_CURSOR L2;
 AM_CURSOR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM1280476813_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM1280476813_frame) (*dELTAM1280476813[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM1280476813[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL2080356652(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL2080356652(self, ((OB) e), i));
   }
   L61_=INTPLUS(i,1); 
   i = L61_;
  }
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

STR ELT_AL1209708374(ELT_AL1361870497 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other62;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other62: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1237867596(ELT_ALGTUPdOBSTR self, dELTTUPdOBSTR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPdOBSTR L11;
 TUPdOBSTR e = TUPdOBSTR_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPdOBSTR L2;
 TUPdOBSTR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU975440397_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU975440397_frame) (*dELTTU975440397[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU975440397[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPdOBSTR_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPdOBSTR_tag;
    L6 = (OB)((TUPdOBSTR_boxed) L7);
    ((TUPdOBSTR_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1289352948(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPdOBSTR_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPdOBSTR_tag;
    L8 = (OB)((TUPdOBSTR_boxed) L9);
    ((TUPdOBSTR_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1289352948(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL1238979875(ELT_AL2101252324 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other46;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other46: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL127733498(ELT_AL1519294172 self, dELTAM1985205111 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM1985205111 L11;
 AM_LOCAL_EXPR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM1985205111 L2;
 AM_LOCAL_EXPR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM128970917_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM128970917_frame) (*dELTAM128970917[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM128970917[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL432174165(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL432174165(self, ((OB) e), i));
   }
   L61_=INTPLUS(i,1); 
   i = L61_;
  }
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

STR ELT_AL1289352948(ELT_ALGTUPdOBSTR self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other61;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other61: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL130599470(ELT_ALGTUPSIGSIG self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other81;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other81: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1436404424(ELT_ALGTUPSIGINT self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other80;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other80: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1458120312(ELT_ALGTUPSIGSTR self, dELTTUPSIGSTR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSIGSTR L11;
 TUPSIGSTR e = TUPSIGSTR_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSIGSTR L2;
 TUPSIGSTR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU2071468085_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU2071468085_frame) (*dELTTU2071468085[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU2071468085[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSIGSTR_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSIGSTR_tag;
    L6 = (OB)((TUPSIGSTR_boxed) L7);
    ((TUPSIGSTR_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1013957530(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSIGSTR_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSIGSTR_tag;
    L8 = (OB)((TUPSIGSTR_boxed) L9);
    ((TUPSIGSTR_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1013957530(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL1651157393(ELT_AL92619562 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other14;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other14: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1813778326(ELT_AL1230514763 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other67;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other67: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1852369832(ELT_ALGARRAYBOOL self, dELTARRAYBOOL arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTARRAYBOOL L11;
 ARRAYBOOL e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTARRAYBOOL L2;
 ARRAYBOOL L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAR657435157_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAR657435157_frame) (*dELTAR657435157[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAR657435157[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1046589086(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1046589086(self, ((OB) e), i));
   }
   L61_=INTPLUS(i,1); 
   i = L61_;
  }
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

STR ELT_AL1978058288(ELT_ALGTUPSIGSIG self, dELTTUPSIGSIG arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSIGSIG L11;
 TUPSIGSIG e = TUPSIGSIG_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSIGSIG L2;
 TUPSIGSIG L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU1525626981_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1525626981_frame) (*dELTTU1525626981[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1525626981[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSIGSIG_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSIGSIG_tag;
    L6 = (OB)((TUPSIGSIG_boxed) L7);
    ((TUPSIGSIG_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL130599470(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSIGSIG_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSIGSIG_tag;
    L8 = (OB)((TUPSIGSIG_boxed) L9);
    ((TUPSIGSIG_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL130599470(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL1989366167(ELT_AL655625458 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other73;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other73: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1993054651(ELT_ALGTUPdTPINT self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other65;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other65: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL2041744410(ELT_AL655438175 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other75;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other75: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL2046630250(ELT_ALGTUPdTPINT self, dELTTUPdTPINT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPdTPINT L11;
 TUPdTPINT e = TUPdTPINT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPdTPINT L2;
 TUPdTPINT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU72816407_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU72816407_frame) (*dELTTU72816407[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU72816407[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPdTPINT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPdTPINT_tag;
    L6 = (OB)((TUPdTPINT_boxed) L7);
    ((TUPdTPINT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1993054651(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPdTPINT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPdTPINT_tag;
    L8 = (OB)((TUPdTPINT_boxed) L9);
    ((TUPdTPINT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1993054651(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL2080356652(ELT_ALGAM_CURSOR self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other24;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other24: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL378639519(ELT_AL1230514763 self, dELTTU847844253 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU847844253 L11;
 TUPAM_667892060 e = TUPAM_667892060_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU847844253 L2;
 TUPAM_667892060 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU202706072_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU202706072_frame) (*dELTTU202706072[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU202706072[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPAM_667892060_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPAM_667892060_tag;
    L6 = (OB)((TUPAM_667892060_boxed) L7);
    ((TUPAM_667892060_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1813778326(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPAM_667892060_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPAM_667892060_tag;
    L8 = (OB)((TUPAM_667892060_boxed) L9);
    ((TUPAM_667892060_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1813778326(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL429905904(ELT_AL655438175 self, dELTTUPIDENTINT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPIDENTINT L11;
 TUPIDENTINT e = TUPIDENTINT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPIDENTINT L2;
 TUPIDENTINT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU1837273309_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1837273309_frame) (*dELTTU1837273309[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1837273309[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPIDENTINT_tag;
    L6 = (OB)((TUPIDENTINT_boxed) L7);
    ((TUPIDENTINT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL2041744410(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPIDENTINT_tag;
    L8 = (OB)((TUPIDENTINT_boxed) L9);
    ((TUPIDENTINT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL2041744410(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL432174165(ELT_AL1519294172 self, OB e, INT i) {
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
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other26;
 } else
 switch (TAG(e)) {
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
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other26: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
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
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL510829178(ELT_AL655625458 self, dELTTUPIDENTdTP arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPIDENTdTP L11;
 TUPIDENTdTP e = TUPIDENTdTP_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPIDENTdTP L2;
 TUPIDENTdTP L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU590857063_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU590857063_frame) (*dELTTU590857063[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU590857063[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPIDENTdTP_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPIDENTdTP_tag;
    L6 = (OB)((TUPIDENTdTP_boxed) L7);
    ((TUPIDENTdTP_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1989366167(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPIDENTdTP_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPIDENTdTP_tag;
    L8 = (OB)((TUPIDENTdTP_boxed) L9);
    ((TUPIDENTdTP_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1989366167(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL649416844(ELT_ALGTUPSIGINT self, dELTTUPSIGINT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSIGINT L11;
 TUPSIGINT e = TUPSIGINT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSIGINT L2;
 TUPSIGINT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU1973395837_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1973395837_frame) (*dELTTU1973395837[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1973395837[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSIGINT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSIGINT_tag;
    L6 = (OB)((TUPSIGINT_boxed) L7);
    ((TUPSIGINT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1436404424(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSIGINT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSIGINT_tag;
    L8 = (OB)((TUPSIGINT_boxed) L9);
    ((TUPSIGINT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1436404424(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL741225494(ELT_AL92619562 self, dELTAM373291377 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM373291377 L11;
 AM_CURSOR_POS e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM373291377 L2;
 AM_CURSOR_POS L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM2044693667_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM2044693667_frame) (*dELTAM2044693667[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM2044693667[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1651157393(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1651157393(self, ((OB) e), i));
   }
   L61_=INTPLUS(i,1); 
   i = L61_;
  }
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

STR ELT_AL772038856(ELT_AL1361870497 self, dELTTU1827781436 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1827781436 L11;
 TUPdTPdLAYOUT e = TUPdTPdLAYOUT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1827781436 L2;
 TUPdTPdLAYOUT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU1923274911_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1923274911_frame) (*dELTTU1923274911[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1923274911[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPdTPdLAYOUT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPdTPdLAYOUT_tag;
    L6 = (OB)((TUPdTPdLAYOUT_boxed) L7);
    ((TUPdTPdLAYOUT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1209708374(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPdTPdLAYOUT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPdTPdLAYOUT_tag;
    L8 = (OB)((TUPdTPdLAYOUT_boxed) L9);
    ((TUPdTPdLAYOUT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1209708374(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
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

STR ELT_AL966666787(ELT_AL1371565672 self, dELTTU1754236182 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1754236182 L11;
 TUPAM_1024994801 e = TUPAM_1024994801_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1754236182 L2;
 TUPAM_1024994801 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU225750038_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU225750038_frame) (*dELTTU225750038[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU225750038[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPAM_1024994801_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPAM_1024994801_tag;
    L6 = (OB)((TUPAM_1024994801_boxed) L7);
    ((TUPAM_1024994801_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1069121223(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPAM_1024994801_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPAM_1024994801_tag;
    L8 = (OB)((TUPAM_1024994801_boxed) L9);
    ((TUPAM_1024994801_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1069121223(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}
