#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dELTdAM_EXPR_struct {
 OB_HEADER header;
 } *dELTdAM_EXPR;

typedef struct dELTdA379108592_frame_struct {
 INT state;
 dELTdAM_EXPR self;
 } *dELTdA379108592_frame;

/* Dispatch table entry for $ELT{$AM_EXPR}::elt!:$AM_EXPR */
typedef struct dELTdA379108592_entry {
const dAM_EXPR (*iter)(dELTdA379108592_frame frame);
const void* (*alloc_frame)();
} dELTdA379108592_entry;

typedef struct dELTdAM_STMT_struct {
 OB_HEADER header;
 } *dELTdAM_STMT;

typedef struct dELTdA418602402_frame_struct {
 INT state;
 dELTdAM_STMT self;
 } *dELTdA418602402_frame;

/* Dispatch table entry for $ELT{$AM_STMT}::elt!:$AM_STMT */
typedef struct dELTdA418602402_entry {
const dAM_STMT (*iter)(dELTdA418602402_frame frame);
const void* (*alloc_frame)();
} dELTdA418602402_entry;

typedef struct dELTdLAYOUT_struct {
 OB_HEADER header;
 } *dELTdLAYOUT;

typedef struct dELTBOOL_struct {
 OB_HEADER header;
 } *dELTBOOL;

typedef struct dELTBU1847408844_struct {
 OB_HEADER header;
 } *dELTBU1847408844;

typedef struct dELTCHAR_struct {
 OB_HEADER header;
 } *dELTCHAR;

typedef struct dELTCLASS_LAYOUT_struct {
 OB_HEADER header;
 } *dELTCLASS_LAYOUT;

typedef struct dELTSIDE_EFFECT_struct {
 OB_HEADER header;
 } *dELTSIDE_EFFECT;

typedef struct dELTTU1315166979_struct {
 OB_HEADER header;
 } *dELTTU1315166979;

typedef struct dELTTU32753540_struct {
 OB_HEADER header;
 } *dELTTU32753540;

typedef struct dELTTU770070093_struct {
 OB_HEADER header;
 } *dELTTU770070093;

typedef struct dELTTU470684395_struct {
 OB_HEADER header;
 } *dELTTU470684395;

typedef struct dELTTUPSTRINT_struct {
 OB_HEADER header;
 } *dELTTUPSTRINT;

typedef struct dELTTUPSTRSIG_struct {
 OB_HEADER header;
 } *dELTTUPSTRSIG;

typedef struct dELTTUPSTRSTR_struct {
 OB_HEADER header;
 } *dELTTUPSTRSTR;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct BUILTIN_LAYOUT_struct {/* layout for BUILTIN_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *BUILTIN_LAYOUT;

typedef struct CLASS_LAYOUT_struct {/* layout for CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *CLASS_LAYOUT;

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

typedef struct ELT_ALGBOOL_struct {/* layout for ELT_ALG{BOOL} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGBOOL;

typedef struct ELT_AL2109782150_struct {/* layout for ELT_ALG{BUILTIN_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2109782150;

typedef struct ELT_ALGCHAR_struct {/* layout for ELT_ALG{CHAR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGCHAR;

typedef struct ELT_AL1044938984_struct {/* layout for ELT_ALG{CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1044938984;

typedef struct ELT_AL569350364_struct {/* layout for ELT_ALG{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL569350364;

typedef struct ELT_AL2005292554_struct {/* layout for ELT_ALG{TUP{$TP,CODE_FILE}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL2005292554;

typedef struct ELT_AL1089356553_struct {/* layout for ELT_ALG{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1089356553;

typedef struct ELT_AL383472681_struct {/* layout for ELT_ALG{TUP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL383472681;

typedef struct ELT_AL4773456_struct {/* layout for ELT_ALG{TUP{STR,$CONFIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL4773456;

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

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct SIDE_EFFECT_struct {/* layout for SIDE_EFFECT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct IDENT_struct name1;
 BOOL doeswrite;
 } *SIDE_EFFECT;

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

typedef struct TUPIDE641654124_struct {/* layout for TUP{IDENT,AM_SHARED_EXPR} */
 struct AM_SHARED_EXPR_struct *t2;
 struct IDENT_struct t1;
 } TUPIDE641654124;
static TUPIDE641654124 TUPIDE641654124_zero;

typedef struct TUPIDE641654124_boxed_struct {
 OB_HEADER header;
 TUPIDE641654124 immutable_part;
 } *TUPIDE641654124_boxed;

typedef struct TUPSTRdCONFIG_struct {/* layout for TUP{STR,$CONFIG} */
 struct dCONFIG_struct *t2;
 STR t1;
 } TUPSTRdCONFIG;
static TUPSTRdCONFIG TUPSTRdCONFIG_zero;

typedef struct TUPSTRdCONFIG_boxed_struct {
 OB_HEADER header;
 TUPSTRdCONFIG immutable_part;
 } *TUPSTRdCONFIG_boxed;

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

typedef struct dELTdL669601151_frame_struct {
 INT state;
 dELTdLAYOUT self;
 } *dELTdL669601151_frame;

/* Dispatch table entry for $ELT{$LAYOUT}::elt!:$LAYOUT */
typedef struct dELTdL669601151_entry {
const dLAYOUT (*iter)(dELTdL669601151_frame frame);
const void* (*alloc_frame)();
} dELTdL669601151_entry;

typedef struct dELTBO582795216_frame_struct {
 INT state;
 dELTBOOL self;
 } *dELTBO582795216_frame;

/* Dispatch table entry for $ELT{BOOL}::elt!:BOOL */
typedef struct dELTBO582795216_entry {
const BOOL (*iter)(dELTBO582795216_frame frame);
const void* (*alloc_frame)();
} dELTBO582795216_entry;

typedef struct dELTBU334182326_frame_struct {
 INT state;
 dELTBU1847408844 self;
 } *dELTBU334182326_frame;

/* Dispatch table entry for $ELT{BUILTIN_LAYOUT}::elt!:BUILTIN_LAYOUT */
typedef struct dELTBU334182326_entry {
const BUILTIN_LAYOUT (*iter)(dELTBU334182326_frame frame);
const void* (*alloc_frame)();
} dELTBU334182326_entry;

typedef struct dELTCH686923168_frame_struct {
 INT state;
 dELTCHAR self;
 } *dELTCH686923168_frame;

/* Dispatch table entry for $ELT{CHAR}::elt!:CHAR */
typedef struct dELTCH686923168_entry {
const CHAR (*iter)(dELTCH686923168_frame frame);
const void* (*alloc_frame)();
} dELTCH686923168_entry;

typedef struct dELTCL1056825190_frame_struct {
 INT state;
 dELTCLASS_LAYOUT self;
 } *dELTCL1056825190_frame;

/* Dispatch table entry for $ELT{CLASS_LAYOUT}::elt!:CLASS_LAYOUT */
typedef struct dELTCL1056825190_entry {
const CLASS_LAYOUT (*iter)(dELTCL1056825190_frame frame);
const void* (*alloc_frame)();
} dELTCL1056825190_entry;

typedef struct dELTSI583496193_frame_struct {
 INT state;
 dELTSIDE_EFFECT self;
 } *dELTSI583496193_frame;

/* Dispatch table entry for $ELT{SIDE_EFFECT}::elt!:SIDE_EFFECT */
typedef struct dELTSI583496193_entry {
const SIDE_EFFECT (*iter)(dELTSI583496193_frame frame);
const void* (*alloc_frame)();
} dELTSI583496193_entry;

typedef struct dELTTU1800917955_frame_struct {
 INT state;
 dELTTU1315166979 self;
 } *dELTTU1800917955_frame;

/* Dispatch table entry for $ELT{TUP{$TP,CODE_FILE}}::elt!:TUP{$TP,CODE_FILE} */
typedef struct dELTTU1800917955_entry {
const TUPdTPCODE_FILE (*iter)(dELTTU1800917955_frame frame);
const void* (*alloc_frame)();
} dELTTU1800917955_entry;

typedef struct dELTTU1084171889_frame_struct {
 INT state;
 dELTTU32753540 self;
 } *dELTTU1084171889_frame;

/* Dispatch table entry for $ELT{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}}::elt!:TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
typedef struct dELTTU1084171889_entry {
const TUPdTP1710233051 (*iter)(dELTTU1084171889_frame frame);
const void* (*alloc_frame)();
} dELTTU1084171889_entry;

typedef struct dELTTU577707672_frame_struct {
 INT state;
 dELTTU770070093 self;
 } *dELTTU577707672_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,AM_SHARED_EXPR}}::elt!:TUP{IDENT,AM_SHARED_EXPR} */
typedef struct dELTTU577707672_entry {
const TUPIDE641654124 (*iter)(dELTTU577707672_frame frame);
const void* (*alloc_frame)();
} dELTTU577707672_entry;

typedef struct dELTTU384262123_frame_struct {
 INT state;
 dELTTU470684395 self;
 } *dELTTU384262123_frame;

/* Dispatch table entry for $ELT{TUP{STR,$CONFIG}}::elt!:TUP{STR,$CONFIG} */
typedef struct dELTTU384262123_entry {
const TUPSTRdCONFIG (*iter)(dELTTU384262123_frame frame);
const void* (*alloc_frame)();
} dELTTU384262123_entry;

typedef struct dELTTU686046061_frame_struct {
 INT state;
 dELTTUPSTRINT self;
 } *dELTTU686046061_frame;

/* Dispatch table entry for $ELT{TUP{STR,INT}}::elt!:TUP{STR,INT} */
typedef struct dELTTU686046061_entry {
const TUPSTRINT (*iter)(dELTTU686046061_frame frame);
const void* (*alloc_frame)();
} dELTTU686046061_entry;

typedef struct dELTTU238277205_frame_struct {
 INT state;
 dELTTUPSTRSIG self;
 } *dELTTU238277205_frame;

/* Dispatch table entry for $ELT{TUP{STR,SIG}}::elt!:TUP{STR,SIG} */
typedef struct dELTTU238277205_entry {
const TUPSTRSIG (*iter)(dELTTU238277205_frame frame);
const void* (*alloc_frame)();
} dELTTU238277205_entry;

typedef struct dELTTU784118309_frame_struct {
 INT state;
 dELTTUPSTRSTR self;
 } *dELTTU784118309_frame;

/* Dispatch table entry for $ELT{TUP{STR,STR}}::elt!:TUP{STR,STR} */
typedef struct dELTTU784118309_entry {
const TUPSTRSTR (*iter)(dELTTU784118309_frame frame);
const void* (*alloc_frame)();
} dELTTU784118309_entry;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

#include "tags.h"

/* Globals */


/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dELTBO582795216_entry *dELTBO582795216;

extern RETURNED_CONST dELTBU334182326_entry *dELTBU334182326;

extern RETURNED_CONST dELTCH686923168_entry *dELTCH686923168;

extern RETURNED_CONST dELTCL1056825190_entry *dELTCL1056825190;

extern RETURNED_CONST dELTSI583496193_entry *dELTSI583496193;

extern RETURNED_CONST dELTTU1084171889_entry *dELTTU1084171889;

extern RETURNED_CONST dELTTU1800917955_entry *dELTTU1800917955;

extern RETURNED_CONST dELTTU238277205_entry *dELTTU238277205;

extern RETURNED_CONST dELTTU384262123_entry *dELTTU384262123;

extern RETURNED_CONST dELTTU577707672_entry *dELTTU577707672;

extern RETURNED_CONST dELTTU686046061_entry *dELTTU686046061;

extern RETURNED_CONST dELTTU784118309_entry *dELTTU784118309;

extern RETURNED_CONST dELTdA379108592_entry *dELTdA379108592;

extern RETURNED_CONST dELTdA418602402_entry *dELTdA418602402;

extern RETURNED_CONST dELTdL669601151_entry *dELTdL669601151;
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
STR ELT_AL1010662471(ELT_AL1044938984, dELTCLASS_LAYOUT);
STR ELT_AL1065029381(ELT_ALGCHAR, dELTCHAR);
STR ELT_AL1115321646(ELT_ALGTUPSTRSIG, OB, INT);
STR ELT_AL1121979891(ELT_ALGdAM_STMT, dELTdAM_STMT);
STR ELT_AL1178944168(ELT_AL1089356553, dELTTU32753540);
STR ELT_AL1283969746(ELT_ALGCHAR, OB, INT);
STR ELT_AL1304624690(ELT_AL4773456, dELTTU470684395);
STR ELT_AL1448140707(ELT_AL383472681, dELTTU770070093);
STR ELT_AL1478627858(ELT_AL1089356553, OB, INT);
STR ELT_AL1612641756(ELT_ALGTUPSTRINT, OB, INT);
STR ELT_AL1688888374(ELT_AL569350364, dELTSIDE_EFFECT);
STR ELT_AL1699479844(ELT_ALGdLAYOUT, OB, INT);
STR ELT_AL1795341273(ELT_AL1044938984, OB, INT);
STR ELT_AL1810262765(ELT_ALGdAM_STMT, OB, INT);
STR ELT_AL184221807(ELT_ALGdAM_EXPR, dELTdAM_EXPR);
STR ELT_AL1930239142(ELT_AL2005292554, dELTTU1315166979);
STR ELT_AL1973565354(ELT_ALGdAM_EXPR, OB, INT);
STR ELT_AL2018196471(ELT_AL4773456, OB, INT);
STR ELT_AL2042268315(ELT_AL2109782150, OB, INT);
STR ELT_AL2071496711(ELT_AL2109782150, dELTBU1847408844);
STR ELT_AL218385468(ELT_ALGdLAYOUT, dELTdLAYOUT);
STR ELT_AL229025142(ELT_ALGBOOL, OB, INT);
STR ELT_AL231963586(ELT_ALGTUPSTRSTR, OB, INT);
STR ELT_AL26398608(ELT_ALGTUPSTRSTR, dELTTUPSTRSTR);
STR ELT_AL360284389(ELT_ALGBOOL, dELTBOOL);
STR ELT_AL546336584(ELT_ALGTUPSTRSIG, dELTTUPSTRSIG);
STR ELT_AL767453894(ELT_AL383472681, OB, INT);
STR ELT_AL782304860(ELT_ALGTUPSTRINT, dELTTUPSTRINT);
STR ELT_AL865522207(ELT_AL2005292554, OB, INT);
STR ELT_AL989068859(ELT_AL569350364, OB, INT);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1010662471(ELT_AL1044938984 self, dELTCLASS_LAYOUT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTCLASS_LAYOUT L11;
 CLASS_LAYOUT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTCLASS_LAYOUT L2;
 CLASS_LAYOUT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTCL1056825190_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTCL1056825190_frame) (*dELTCL1056825190[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTCL1056825190[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1795341273(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1795341273(self, ((OB) e), i));
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

STR ELT_AL1065029381(ELT_ALGCHAR self, dELTCHAR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTCHAR L11;
 CHAR e = CHAR_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTCHAR L2;
 CHAR L3;
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
  
  dELTCH686923168_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTCH686923168_frame) (*dELTCH686923168[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTCH686923168[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    memset(L7,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L7)->header.tag=CHAR_tag;
    L6 = (OB)((CHAR_boxed) L7);
    ((CHAR_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1283969746(self, L6, i));
   }
   else {
    L9=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    memset(L9,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L9)->header.tag=CHAR_tag;
    L8 = (OB)((CHAR_boxed) L9);
    ((CHAR_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1283969746(self, L8, i));
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

STR ELT_AL1115321646(ELT_ALGTUPSTRSIG self, OB e, INT i) {
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
  goto other86;
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
  other86: ;
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

STR ELT_AL1121979891(ELT_ALGdAM_STMT self, dELTdAM_STMT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTdAM_STMT L11;
 dAM_STMT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTdAM_STMT L2;
 dAM_STMT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTdA418602402_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTdA418602402_frame) (*dELTdA418602402[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTdA418602402[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1810262765(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1810262765(self, ((OB) e), i));
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

STR ELT_AL1178944168(ELT_AL1089356553 self, dELTTU32753540 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU32753540 L11;
 TUPdTP1710233051 e = TUPdTP1710233051_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU32753540 L2;
 TUPdTP1710233051 L3;
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
  
  dELTTU1084171889_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1084171889_frame) (*dELTTU1084171889[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1084171889[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPdTP1710233051_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPdTP1710233051_tag;
    L6 = (OB)((TUPdTP1710233051_boxed) L7);
    ((TUPdTP1710233051_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1478627858(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPdTP1710233051_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPdTP1710233051_tag;
    L8 = (OB)((TUPdTP1710233051_boxed) L9);
    ((TUPdTP1710233051_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1478627858(self, L8, i));
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

STR ELT_AL1283969746(ELT_ALGCHAR self, OB e, INT i) {
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
  goto other40;
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
  other40: ;
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

STR ELT_AL1304624690(ELT_AL4773456 self, dELTTU470684395 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU470684395 L11;
 TUPSTRdCONFIG e = TUPSTRdCONFIG_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU470684395 L2;
 TUPSTRdCONFIG L3;
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
  
  dELTTU384262123_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU384262123_frame) (*dELTTU384262123[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU384262123[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSTRdCONFIG_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSTRdCONFIG_tag;
    L6 = (OB)((TUPSTRdCONFIG_boxed) L7);
    ((TUPSTRdCONFIG_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL2018196471(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSTRdCONFIG_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSTRdCONFIG_tag;
    L8 = (OB)((TUPSTRdCONFIG_boxed) L9);
    ((TUPSTRdCONFIG_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL2018196471(self, L8, i));
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

STR ELT_AL1448140707(ELT_AL383472681 self, dELTTU770070093 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU770070093 L11;
 TUPIDE641654124 e = TUPIDE641654124_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU770070093 L2;
 TUPIDE641654124 L3;
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
  
  dELTTU577707672_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU577707672_frame) (*dELTTU577707672[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU577707672[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPIDE641654124_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPIDE641654124_tag;
    L6 = (OB)((TUPIDE641654124_boxed) L7);
    ((TUPIDE641654124_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL767453894(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPIDE641654124_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPIDE641654124_tag;
    L8 = (OB)((TUPIDE641654124_boxed) L9);
    ((TUPIDE641654124_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL767453894(self, L8, i));
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

STR ELT_AL1478627858(ELT_AL1089356553 self, OB e, INT i) {
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
  goto other64;
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
  other64: ;
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

STR ELT_AL1612641756(ELT_ALGTUPSTRINT self, OB e, INT i) {
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
  goto other85;
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
  other85: ;
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

STR ELT_AL1688888374(ELT_AL569350364 self, dELTSIDE_EFFECT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTSIDE_EFFECT L11;
 SIDE_EFFECT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTSIDE_EFFECT L2;
 SIDE_EFFECT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTSI583496193_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTSI583496193_frame) (*dELTSI583496193[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTSI583496193[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL989068859(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL989068859(self, ((OB) e), i));
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

STR ELT_AL1699479844(ELT_ALGdLAYOUT self, OB e, INT i) {
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
  goto other18;
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
  other18: ;
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

STR ELT_AL1795341273(ELT_AL1044938984 self, OB e, INT i) {
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
  goto other41;
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
  other41: ;
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

STR ELT_AL1810262765(ELT_ALGdAM_STMT self, OB e, INT i) {
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
  goto other16;
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
  other16: ;
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

STR ELT_AL184221807(ELT_ALGdAM_EXPR self, dELTdAM_EXPR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTdAM_EXPR L11;
 dAM_EXPR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTdAM_EXPR L2;
 dAM_EXPR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTdA379108592_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTdA379108592_frame) (*dELTdA379108592[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTdA379108592[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1973565354(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1973565354(self, ((OB) e), i));
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

STR ELT_AL1930239142(ELT_AL2005292554 self, dELTTU1315166979 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1315166979 L11;
 TUPdTPCODE_FILE e = TUPdTPCODE_FILE_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1315166979 L2;
 TUPdTPCODE_FILE L3;
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
  
  dELTTU1800917955_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1800917955_frame) (*dELTTU1800917955[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1800917955[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPdTPCODE_FILE_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPdTPCODE_FILE_tag;
    L6 = (OB)((TUPdTPCODE_FILE_boxed) L7);
    ((TUPdTPCODE_FILE_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL865522207(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPdTPCODE_FILE_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPdTPCODE_FILE_tag;
    L8 = (OB)((TUPdTPCODE_FILE_boxed) L9);
    ((TUPdTPCODE_FILE_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL865522207(self, L8, i));
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

STR ELT_AL1973565354(ELT_ALGdAM_EXPR self, OB e, INT i) {
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
  goto other15;
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
  other15: ;
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

STR ELT_AL2018196471(ELT_AL4773456 self, OB e, INT i) {
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
  goto other83;
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
  other83: ;
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

STR ELT_AL2042268315(ELT_AL2109782150 self, OB e, INT i) {
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
  goto other38;
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
  other38: ;
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

STR ELT_AL2071496711(ELT_AL2109782150 self, dELTBU1847408844 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTBU1847408844 L11;
 BUILTIN_LAYOUT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTBU1847408844 L2;
 BUILTIN_LAYOUT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTBU334182326_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTBU334182326_frame) (*dELTBU334182326[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTBU334182326[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL2042268315(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL2042268315(self, ((OB) e), i));
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

STR ELT_AL218385468(ELT_ALGdLAYOUT self, dELTdLAYOUT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTdLAYOUT L11;
 dLAYOUT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTdLAYOUT L2;
 dLAYOUT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTdL669601151_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTdL669601151_frame) (*dELTdL669601151[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTdL669601151[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1699479844(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1699479844(self, ((OB) e), i));
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

STR ELT_AL229025142(ELT_ALGBOOL self, OB e, INT i) {
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
  goto other35;
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
  other35: ;
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

STR ELT_AL231963586(ELT_ALGTUPSTRSTR self, OB e, INT i) {
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
  goto other87;
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
  other87: ;
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

STR ELT_AL26398608(ELT_ALGTUPSTRSTR self, dELTTUPSTRSTR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSTRSTR L11;
 TUPSTRSTR e = TUPSTRSTR_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSTRSTR L2;
 TUPSTRSTR L3;
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
  
  dELTTU784118309_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU784118309_frame) (*dELTTU784118309[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU784118309[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSTRSTR_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSTRSTR_tag;
    L6 = (OB)((TUPSTRSTR_boxed) L7);
    ((TUPSTRSTR_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL231963586(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSTRSTR_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSTRSTR_tag;
    L8 = (OB)((TUPSTRSTR_boxed) L9);
    ((TUPSTRSTR_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL231963586(self, L8, i));
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

STR ELT_AL360284389(ELT_ALGBOOL self, dELTBOOL arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTBOOL L11;
 BOOL e = BOOL_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTBOOL L2;
 BOOL L3;
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
  
  dELTBO582795216_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTBO582795216_frame) (*dELTBO582795216[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTBO582795216[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    memset(L7,0,sizeof(struct BOOL_boxed_struct));
    ((OB)L7)->header.tag=BOOL_tag;
    L6 = (OB)((BOOL_boxed) L7);
    ((BOOL_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL229025142(self, L6, i));
   }
   else {
    L9=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    memset(L9,0,sizeof(struct BOOL_boxed_struct));
    ((OB)L9)->header.tag=BOOL_tag;
    L8 = (OB)((BOOL_boxed) L9);
    ((BOOL_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL229025142(self, L8, i));
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

STR ELT_AL546336584(ELT_ALGTUPSTRSIG self, dELTTUPSTRSIG arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSTRSIG L11;
 TUPSTRSIG e = TUPSTRSIG_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSTRSIG L2;
 TUPSTRSIG L3;
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
  
  dELTTU238277205_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU238277205_frame) (*dELTTU238277205[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU238277205[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSTRSIG_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSTRSIG_tag;
    L6 = (OB)((TUPSTRSIG_boxed) L7);
    ((TUPSTRSIG_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1115321646(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSTRSIG_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSTRSIG_tag;
    L8 = (OB)((TUPSTRSIG_boxed) L9);
    ((TUPSTRSIG_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1115321646(self, L8, i));
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

STR ELT_AL767453894(ELT_AL383472681 self, OB e, INT i) {
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
  goto other74;
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
  other74: ;
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

STR ELT_AL782304860(ELT_ALGTUPSTRINT self, dELTTUPSTRINT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTUPSTRINT L11;
 TUPSTRINT e = TUPSTRINT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTUPSTRINT L2;
 TUPSTRINT L3;
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
  
  dELTTU686046061_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU686046061_frame) (*dELTTU686046061[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU686046061[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSTRINT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSTRINT_tag;
    L6 = (OB)((TUPSTRINT_boxed) L7);
    ((TUPSTRINT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1612641756(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSTRINT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSTRINT_tag;
    L8 = (OB)((TUPSTRINT_boxed) L9);
    ((TUPSTRINT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1612641756(self, L8, i));
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

STR ELT_AL865522207(ELT_AL2005292554 self, OB e, INT i) {
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
  goto other63;
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
  other63: ;
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

STR ELT_AL989068859(ELT_AL569350364 self, OB e, INT i) {
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
  goto other56;
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
  other56: ;
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
