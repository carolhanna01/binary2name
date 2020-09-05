#include "sather.h"

/* Layouts */

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dAS_STMT_struct {
 OB_HEADER header;
 } *dAS_STMT;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dBUILD508976674_struct {
 OB_HEADER header;
 } *dBUILD508976674;

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

typedef struct dCHECK_AM_struct {
 OB_HEADER header;
 } *dCHECK_AM;

typedef struct dCHECK1877276428_struct {
 OB_HEADER header;
 } *dCHECK1877276428;

typedef struct dCONFIG_struct {
 OB_HEADER header;
 } *dCONFIG;

typedef struct dELTdAM_CONST_struct {
 OB_HEADER header;
 } *dELTdAM_CONST;

typedef struct dELTdA578383841_frame_struct {
 INT state;
 dELTdAM_CONST self;
 } *dELTdA578383841_frame;

/* Dispatch table entry for $ELT{$AM_CONST}::elt!:$AM_CONST */
typedef struct dELTdA578383841_entry {
const dAM_CONST (*iter)(dELTdA578383841_frame frame);
const void* (*alloc_frame)();
} dELTdA578383841_entry;

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

typedef struct dELTdTP_struct {
 OB_HEADER header;
 } *dELTdTP;

typedef struct dELTAB597459657_struct {
 OB_HEADER header;
 } *dELTAB597459657;

typedef struct dELTAM1766315002_struct {
 OB_HEADER header;
 } *dELTAM1766315002;

typedef struct dELTAM1208385872_struct {
 OB_HEADER header;
 } *dELTAM1208385872;

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

typedef struct dELTTU1558822270_struct {
 OB_HEADER header;
 } *dELTTU1558822270;

typedef struct dELTTU847844253_struct {
 OB_HEADER header;
 } *dELTTU847844253;

typedef struct dELTTU789674962_struct {
 OB_HEADER header;
 } *dELTTU789674962;

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

typedef struct dFIND_TYPES_struct {
 OB_HEADER header;
 } *dFIND_TYPES;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dGET_MAIN_SIG_struct {
 OB_HEADER header;
 } *dGET_MAIN_SIG;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dHASH_struct {
 OB_HEADER header;
 } *dHASH;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dINLINE_ITER_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ITER_SIG;

typedef struct dINLINE_ROUT_SIG_struct {
 OB_HEADER header;
 } *dINLINE_ROUT_SIG;

typedef struct dIS_EQ_struct {
 OB_HEADER header;
 } *dIS_EQ;

typedef struct dIS_NIL_struct {
 OB_HEADER header;
 } *dIS_NIL;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dNIL_struct {
 OB_HEADER header;
 } *dNIL;

typedef struct dOPTIMIZE_struct {
 OB_HEADER header;
 } *dOPTIMIZE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

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

typedef struct ARRAYA1870808380_struct {/* layout for ARRAY{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *ARRAYA1870808380;

typedef struct ARRAYA967821567_frame_struct {
 INT state;
 ARRAYA1870808380 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA967821567_frame;

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

typedef struct ARRAYA44788561_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA44788561_frame;

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

typedef struct ARRAYA1315754333_frame_struct {
 INT state;
 ARRAYA1347292543 self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1315754333_frame;

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

typedef struct A_STAC1244966098_struct {/* layout for A_STACK{AM_CURSOR_POS} */
 OB_HEADER header;
 struct FLISTA886630705_struct *s;
 } *A_STAC1244966098;

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

typedef struct CHECK_AM_struct {/* layout for CHECK_AM */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_AM;

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

typedef struct CSE_DOUBLE_EXPRS_struct {/* layout for CSE_DOUBLE_EXPRS */
 OB_HEADER header;
 struct FLISTdAM_EXPR_struct *expr;
 struct FLISTAM_CURSOR_struct *pos;
 struct FLISTA725283029_struct *locals1;
 } *CSE_DOUBLE_EXPRS;

typedef struct ELT_NI1559244239_struct {/* layout for ELT_NIL{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1559244239;

typedef struct ELT_NI1564018771_struct {/* layout for ELT_NIL{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1564018771;

typedef struct ELT_NI19483997_struct {/* layout for ELT_NIL{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI19483997;

typedef struct ELT_NI602850927_struct {/* layout for ELT_NIL{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI602850927;

typedef struct ELT_NI1377997731_struct {/* layout for ELT_NIL{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1377997731;

typedef struct ELT_NI465844240_struct {/* layout for ELT_NIL{AS_FEAT_MOD} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI465844240;

typedef struct ELT_NI338362486_struct {/* layout for ELT_NIL{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI338362486;

typedef struct ELT_NI1119971763_struct {/* layout for ELT_NIL{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1119971763;

typedef struct ELT_NI1281796289_struct {/* layout for ELT_NIL{BUILTIN_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1281796289;

typedef struct ELT_NI737899749_struct {/* layout for ELT_NIL{CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI737899749;

typedef struct ELT_NI881793976_struct {/* layout for ELT_NIL{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI881793976;

typedef struct ELT_NI94923122_struct {/* layout for ELT_NIL{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI94923122;

typedef struct ELT_NI1394471793_struct {/* layout for ELT_NIL{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1394471793;

typedef struct ELT_NILSFILE_ID_struct {/* layout for ELT_NIL{SFILE_ID} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSFILE_ID;

typedef struct ELT_NI351013237_struct {/* layout for ELT_NIL{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI351013237;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

typedef struct ELT_NI1575801776_struct {/* layout for ELT_NIL{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1575801776;

typedef struct ELT_NILTUPSIGSIG_struct {/* layout for ELT_NIL{TUP{SIG,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTUPSIGSIG;

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

typedef struct IMPL_struct {/* layout for IMPL */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ELT_TBL_struct *elts;
 struct IFC_struct *ifc;
 struct TP_CLASS_struct *arr;
 BOOL is_ref_cache;
 BOOL use_ref_cache;
 } *IMPL;

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

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

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

typedef struct dELTdL669601151_frame_struct {
 INT state;
 dELTdLAYOUT self;
 } *dELTdL669601151_frame;

/* Dispatch table entry for $ELT{$LAYOUT}::elt!:$LAYOUT */
typedef struct dELTdL669601151_entry {
const dLAYOUT (*iter)(dELTdL669601151_frame frame);
const void* (*alloc_frame)();
} dELTdL669601151_entry;

typedef struct dELTdTP_eltbrdTP_frame_struct {
 INT state;
 dELTdTP self;
 } *dELTdTP_eltbrdTP_frame;

/* Dispatch table entry for $ELT{$TP}::elt!:$TP */
typedef struct dELTdTP_eltbrdTP_entry {
const dTP (*iter)(dELTdTP_eltbrdTP_frame frame);
const void* (*alloc_frame)();
} dELTdTP_eltbrdTP_entry;

typedef struct dELTAB1485261483_frame_struct {
 INT state;
 dELTAB597459657 self;
 } *dELTAB1485261483_frame;

/* Dispatch table entry for $ELT{ABSTRACT_LAYOUT}::elt!:ABSTRACT_LAYOUT */
typedef struct dELTAB1485261483_entry {
const ABSTRACT_LAYOUT (*iter)(dELTAB1485261483_frame frame);
const void* (*alloc_frame)();
} dELTAB1485261483_entry;

typedef struct dELTAM1395458731_frame_struct {
 INT state;
 dELTAM_CALL_ARG self;
 } *dELTAM1395458731_frame;

/* Dispatch table entry for $ELT{AM_CALL_ARG}::elt!:AM_CALL_ARG */
typedef struct dELTAM1395458731_entry {
const AM_CALL_ARG (*iter)(dELTAM1395458731_frame frame);
const void* (*alloc_frame)();
} dELTAM1395458731_entry;

typedef struct dELTAM1417430129_frame_struct {
 INT state;
 dELTAM937720280 self;
 } *dELTAM1417430129_frame;

/* Dispatch table entry for $ELT{AM_FORMAL_ARG}::elt!:AM_FORMAL_ARG */
typedef struct dELTAM1417430129_entry {
const AM_FORMAL_ARG (*iter)(dELTAM1417430129_frame frame);
const void* (*alloc_frame)();
} dELTAM1417430129_entry;

typedef struct dELTAR391526113_frame_struct {
 INT state;
 dELTAR67678756 self;
 } *dELTAR391526113_frame;

/* Dispatch table entry for $ELT{ARRAY{$AM_EXPR}}::elt!:ARRAY{$AM_EXPR} */
typedef struct dELTAR391526113_entry {
const ARRAYdAM_EXPR (*iter)(dELTAR391526113_frame frame);
const void* (*alloc_frame)();
} dELTAR391526113_entry;

typedef struct dELTAR1531173752_frame_struct {
 INT state;
 dELTARRAYSTR self;
 } *dELTAR1531173752_frame;

/* Dispatch table entry for $ELT{ARRAY{STR}}::elt!:ARRAY{STR} */
typedef struct dELTAR1531173752_entry {
const ARRAYSTR (*iter)(dELTAR1531173752_frame frame);
const void* (*alloc_frame)();
} dELTAR1531173752_entry;

typedef struct dELTAT1600626566_frame_struct {
 INT state;
 dELTATTRSORT self;
 } *dELTAT1600626566_frame;

/* Dispatch table entry for $ELT{ATTRSORT}::elt!:ATTRSORT */
typedef struct dELTAT1600626566_entry {
const ATTRSORT (*iter)(dELTAT1600626566_frame frame);
const void* (*alloc_frame)();
} dELTAT1600626566_entry;

typedef struct dELTBO582795216_frame_struct {
 INT state;
 dELTBOOL self;
 } *dELTBO582795216_frame;

/* Dispatch table entry for $ELT{BOOL}::elt!:BOOL */
typedef struct dELTBO582795216_entry {
const BOOL (*iter)(dELTBO582795216_frame frame);
const void* (*alloc_frame)();
} dELTBO582795216_entry;

typedef struct dELTBO1349583384_frame_struct {
 INT state;
 dELTBO2026178203 self;
 } *dELTBO1349583384_frame;

/* Dispatch table entry for $ELT{BOUND_ITER_TYPE_LAYOUT}::elt!:BOUND_ITER_TYPE_LAYOUT */
typedef struct dELTBO1349583384_entry {
const BOUND_2140373124 (*iter)(dELTBO1349583384_frame frame);
const void* (*alloc_frame)();
} dELTBO1349583384_entry;

typedef struct dELTBO879923169_frame_struct {
 INT state;
 dELTBO61052977 self;
 } *dELTBO879923169_frame;

/* Dispatch table entry for $ELT{BOUND_TYPE_LAYOUT}::elt!:BOUND_TYPE_LAYOUT */
typedef struct dELTBO879923169_entry {
const BOUND_809390774 (*iter)(dELTBO879923169_frame frame);
const void* (*alloc_frame)();
} dELTBO879923169_entry;

typedef struct dELTBU334182326_frame_struct {
 INT state;
 dELTBU1847408844 self;
 } *dELTBU334182326_frame;

/* Dispatch table entry for $ELT{BUILTIN_LAYOUT}::elt!:BUILTIN_LAYOUT */
typedef struct dELTBU334182326_entry {
const BUILTIN_LAYOUT (*iter)(dELTBU334182326_frame frame);
const void* (*alloc_frame)();
} dELTBU334182326_entry;

typedef struct dELTCA1528219566_frame_struct {
 INT state;
 dELTCALL_ARG self;
 } *dELTCA1528219566_frame;

/* Dispatch table entry for $ELT{CALL_ARG}::elt!:CALL_ARG */
typedef struct dELTCA1528219566_entry {
const CALL_ARG (*iter)(dELTCA1528219566_frame frame);
const void* (*alloc_frame)();
} dELTCA1528219566_entry;

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

typedef struct dELTCS742343900_frame_struct {
 INT state;
 dELTCS1440874327 self;
 } *dELTCS742343900_frame;

/* Dispatch table entry for $ELT{CSE_DOUBLE_EXPRS}::elt!:CSE_DOUBLE_EXPRS */
typedef struct dELTCS742343900_entry {
const CSE_DOUBLE_EXPRS (*iter)(dELTCS742343900_frame frame);
const void* (*alloc_frame)();
} dELTCS742343900_entry;

typedef struct dELTEX1931613508_frame_struct {
 INT state;
 dELTEXPR_HOISTED self;
 } *dELTEX1931613508_frame;

/* Dispatch table entry for $ELT{EXPR_HOISTED}::elt!:EXPR_HOISTED */
typedef struct dELTEX1931613508_entry {
const EXPR_HOISTED (*iter)(dELTEX1931613508_frame frame);
const void* (*alloc_frame)();
} dELTEX1931613508_entry;

typedef struct dELTEX2090504649_frame_struct {
 INT state;
 dELTEX2061780774 self;
 } *dELTEX2090504649_frame;

/* Dispatch table entry for $ELT{EXTERNAL_LAYOUT}::elt!:EXTERNAL_LAYOUT */
typedef struct dELTEX2090504649_entry {
const EXTERNAL_LAYOUT (*iter)(dELTEX2090504649_frame frame);
const void* (*alloc_frame)();
} dELTEX2090504649_entry;

typedef struct dELTID1378398307_frame_struct {
 INT state;
 dELTIDENT self;
 } *dELTID1378398307_frame;

/* Dispatch table entry for $ELT{IDENT}::elt!:IDENT */
typedef struct dELTID1378398307_entry {
const IDENT (*iter)(dELTID1378398307_frame frame);
const void* (*alloc_frame)();
} dELTID1378398307_entry;

typedef struct dELTIFC_eltbrIFC_frame_struct {
 INT state;
 dELTIFC self;
 } *dELTIFC_eltbrIFC_frame;

/* Dispatch table entry for $ELT{IFC}::elt!:IFC */
typedef struct dELTIFC_eltbrIFC_entry {
const IFC (*iter)(dELTIFC_eltbrIFC_frame frame);
const void* (*alloc_frame)();
} dELTIFC_eltbrIFC_entry;

typedef struct dELTIM1138806518_frame_struct {
 INT state;
 dELTIM293343924 self;
 } *dELTIM1138806518_frame;

/* Dispatch table entry for $ELT{IMMUTABLE_CLASS_LAYOUT}::elt!:IMMUTABLE_CLASS_LAYOUT */
typedef struct dELTIM1138806518_entry {
const IMMUTA421759893 (*iter)(dELTIM1138806518_frame frame);
const void* (*alloc_frame)();
} dELTIM1138806518_entry;

typedef struct dELTIM1958116958_frame_struct {
 INT state;
 dELTIMPL_INCLUDE self;
 } *dELTIM1958116958_frame;

/* Dispatch table entry for $ELT{IMPL_INCLUDE}::elt!:IMPL_INCLUDE */
typedef struct dELTIM1958116958_entry {
const IMPL_INCLUDE (*iter)(dELTIM1958116958_frame frame);
const void* (*alloc_frame)();
} dELTIM1958116958_entry;

typedef struct dELTINT_eltbrINT_frame_struct {
 INT state;
 dELTINT self;
 } *dELTINT_eltbrINT_frame;

/* Dispatch table entry for $ELT{INT}::elt!:INT */
typedef struct dELTINT_eltbrINT_entry {
const INT (*iter)(dELTINT_eltbrINT_frame frame);
const void* (*alloc_frame)();
} dELTINT_eltbrINT_entry;

typedef struct dELTOP2009500256_frame_struct {
 INT state;
 dELTOP237467449 self;
 } *dELTOP2009500256_frame;

/* Dispatch table entry for $ELT{OPT_LOCAL_EXPR}::elt!:OPT_LOCAL_EXPR */
typedef struct dELTOP2009500256_entry {
const OPT_LOCAL_EXPR (*iter)(dELTOP2009500256_frame frame);
const void* (*alloc_frame)();
} dELTOP2009500256_entry;

typedef struct dELTSF338070590_frame_struct {
 INT state;
 dELTSFILE_ID self;
 } *dELTSF338070590_frame;

/* Dispatch table entry for $ELT{SFILE_ID}::elt!:SFILE_ID */
typedef struct dELTSF338070590_entry {
const SFILE_ID (*iter)(dELTSF338070590_frame frame);
const void* (*alloc_frame)();
} dELTSF338070590_entry;

typedef struct dELTSI583496193_frame_struct {
 INT state;
 dELTSIDE_EFFECT self;
 } *dELTSI583496193_frame;

/* Dispatch table entry for $ELT{SIDE_EFFECT}::elt!:SIDE_EFFECT */
typedef struct dELTSI583496193_entry {
const SIDE_EFFECT (*iter)(dELTSI583496193_frame frame);
const void* (*alloc_frame)();
} dELTSI583496193_entry;

typedef struct dELTSIG_eltbrSIG_frame_struct {
 INT state;
 dELTSIG self;
 } *dELTSIG_eltbrSIG_frame;

/* Dispatch table entry for $ELT{SIG}::elt!:SIG */
typedef struct dELTSIG_eltbrSIG_entry {
const SIG (*iter)(dELTSIG_eltbrSIG_frame frame);
const void* (*alloc_frame)();
} dELTSIG_eltbrSIG_entry;

typedef struct dELTSTR_eltbrSTR_frame_struct {
 INT state;
 dELTSTR self;
 } *dELTSTR_eltbrSTR_frame;

/* Dispatch table entry for $ELT{STR}::elt!:STR */
typedef struct dELTSTR_eltbrSTR_entry {
const STR (*iter)(dELTSTR_eltbrSTR_frame frame);
const void* (*alloc_frame)();
} dELTSTR_eltbrSTR_entry;

typedef struct dELTTP817795506_frame_struct {
 INT state;
 dELTTP_CLASS self;
 } *dELTTP817795506_frame;

/* Dispatch table entry for $ELT{TP_CLASS}::elt!:TP_CLASS */
typedef struct dELTTP817795506_entry {
const TP_CLASS (*iter)(dELTTP817795506_frame frame);
const void* (*alloc_frame)();
} dELTTP817795506_entry;

typedef struct dELTTP992626963_frame_struct {
 INT state;
 dELTTP_LAYOUT self;
 } *dELTTP992626963_frame;

/* Dispatch table entry for $ELT{TP_LAYOUT}::elt!:TP_LAYOUT */
typedef struct dELTTP992626963_entry {
const TP_LAYOUT (*iter)(dELTTP992626963_frame frame);
const void* (*alloc_frame)();
} dELTTP992626963_entry;

typedef struct dELTTU164160693_frame_struct {
 INT state;
 dELTTU1079126879 self;
 } *dELTTU164160693_frame;

/* Dispatch table entry for $ELT{TUP{$OB,NAMESPACE}}::elt!:TUP{$OB,NAMESPACE} */
typedef struct dELTTU164160693_entry {
const TUPdOBNAMESPACE (*iter)(dELTTU164160693_frame frame);
const void* (*alloc_frame)();
} dELTTU164160693_entry;

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

typedef struct dELTTU72816407_frame_struct {
 INT state;
 dELTTUPdTPINT self;
 } *dELTTU72816407_frame;

/* Dispatch table entry for $ELT{TUP{$TP,INT}}::elt!:TUP{$TP,INT} */
typedef struct dELTTU72816407_entry {
const TUPdTPINT (*iter)(dELTTU72816407_frame frame);
const void* (*alloc_frame)();
} dELTTU72816407_entry;

typedef struct dELTTU1697593574_frame_struct {
 INT state;
 dELTTU1558822270 self;
 } *dELTTU1697593574_frame;

/* Dispatch table entry for $ELT{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}}::elt!:TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
typedef struct dELTTU1697593574_entry {
const TUPAM_304458649 (*iter)(dELTTU1697593574_frame frame);
const void* (*alloc_frame)();
} dELTTU1697593574_entry;

typedef struct dELTTU202706072_frame_struct {
 INT state;
 dELTTU847844253 self;
 } *dELTTU202706072_frame;

/* Dispatch table entry for $ELT{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}}::elt!:TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
typedef struct dELTTU202706072_entry {
const TUPAM_667892060 (*iter)(dELTTU202706072_frame frame);
const void* (*alloc_frame)();
} dELTTU202706072_entry;

typedef struct dELTTU538545734_frame_struct {
 INT state;
 dELTTU789674962 self;
 } *dELTTU538545734_frame;

/* Dispatch table entry for $ELT{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}}::elt!:TUP{AM_CURSOR,$AM_EXPR,$AM_STMT} */
typedef struct dELTTU538545734_entry {
const TUPAM_927437481 (*iter)(dELTTU538545734_frame frame);
const void* (*alloc_frame)();
} dELTTU538545734_entry;

typedef struct dELTTU1106260535_frame_struct {
 INT state;
 dELTTU158438634 self;
 } *dELTTU1106260535_frame;

/* Dispatch table entry for $ELT{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}}::elt!:TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR} */
typedef struct dELTTU1106260535_entry {
const TUPAM_1519040877 (*iter)(dELTTU1106260535_frame frame);
const void* (*alloc_frame)();
} dELTTU1106260535_entry;

typedef struct dELTTU2146728406_frame_struct {
 INT state;
 dELTTU1982974052 self;
 } *dELTTU2146728406_frame;

/* Dispatch table entry for $ELT{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}}::elt!:TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
typedef struct dELTTU2146728406_entry {
const TUPAM_504653531 (*iter)(dELTTU2146728406_frame frame);
const void* (*alloc_frame)();
} dELTTU2146728406_entry;

typedef struct dELTTU225750038_frame_struct {
 INT state;
 dELTTU1754236182 self;
 } *dELTTU225750038_frame;

/* Dispatch table entry for $ELT{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}}::elt!:TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
typedef struct dELTTU225750038_entry {
const TUPAM_1024994801 (*iter)(dELTTU225750038_frame frame);
const void* (*alloc_frame)();
} dELTTU225750038_entry;

typedef struct dELTTU1108719580_frame_struct {
 INT state;
 dELTTU1450772879 self;
 } *dELTTU1108719580_frame;

/* Dispatch table entry for $ELT{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}}::elt!:TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
typedef struct dELTTU1108719580_entry {
const TUPAM_653469574 (*iter)(dELTTU1108719580_frame frame);
const void* (*alloc_frame)();
} dELTTU1108719580_entry;

typedef struct dELTTU590857063_frame_struct {
 INT state;
 dELTTUPIDENTdTP self;
 } *dELTTU590857063_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,$TP}}::elt!:TUP{IDENT,$TP} */
typedef struct dELTTU590857063_entry {
const TUPIDENTdTP (*iter)(dELTTU590857063_frame frame);
const void* (*alloc_frame)();
} dELTTU590857063_entry;

typedef struct dELTTU577707672_frame_struct {
 INT state;
 dELTTU770070093 self;
 } *dELTTU577707672_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,AM_SHARED_EXPR}}::elt!:TUP{IDENT,AM_SHARED_EXPR} */
typedef struct dELTTU577707672_entry {
const TUPIDE641654124 (*iter)(dELTTU577707672_frame frame);
const void* (*alloc_frame)();
} dELTTU577707672_entry;

typedef struct dELTTU1837273309_frame_struct {
 INT state;
 dELTTUPIDENTINT self;
 } *dELTTU1837273309_frame;

/* Dispatch table entry for $ELT{TUP{IDENT,INT}}::elt!:TUP{IDENT,INT} */
typedef struct dELTTU1837273309_entry {
const TUPIDENTINT (*iter)(dELTTU1837273309_frame frame);
const void* (*alloc_frame)();
} dELTTU1837273309_entry;

typedef struct dELTTU2080726233_frame_struct {
 INT state;
 dELTTU405023937 self;
 } *dELTTU2080726233_frame;

/* Dispatch table entry for $ELT{TUP{SIG,ABSTRACT_FRAME_LAYOUT}}::elt!:TUP{SIG,ABSTRACT_FRAME_LAYOUT} */
typedef struct dELTTU2080726233_entry {
const TUPSIG1754078736 (*iter)(dELTTU2080726233_frame frame);
const void* (*alloc_frame)();
} dELTTU2080726233_entry;

typedef struct dELTTU1031278189_frame_struct {
 INT state;
 dELTTU1449135940 self;
 } *dELTTU1031278189_frame;

/* Dispatch table entry for $ELT{TUP{SIG,AM_ROUT_DEF}}::elt!:TUP{SIG,AM_ROUT_DEF} */
typedef struct dELTTU1031278189_entry {
const TUPSIG578692189 (*iter)(dELTTU1031278189_frame frame);
const void* (*alloc_frame)();
} dELTTU1031278189_entry;

typedef struct dELTTU1654314914_frame_struct {
 INT state;
 dELTTU1411620812 self;
 } *dELTTU1654314914_frame;

/* Dispatch table entry for $ELT{TUP{SIG,ARG_LAYOUT}}::elt!:TUP{SIG,ARG_LAYOUT} */
typedef struct dELTTU1654314914_entry {
const TUPSIGARG_LAYOUT (*iter)(dELTTU1654314914_frame frame);
const void* (*alloc_frame)();
} dELTTU1654314914_entry;

typedef struct dELTTU1157822078_frame_struct {
 INT state;
 dELTTU1629996210 self;
 } *dELTTU1157822078_frame;

/* Dispatch table entry for $ELT{TUP{SIG,FRAME_LAYOUT}}::elt!:TUP{SIG,FRAME_LAYOUT} */
typedef struct dELTTU1157822078_entry {
const TUPSIG2023533247 (*iter)(dELTTU1157822078_frame frame);
const void* (*alloc_frame)();
} dELTTU1157822078_entry;

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

typedef struct dELTTU384262123_frame_struct {
 INT state;
 dELTTU470684395 self;
 } *dELTTU384262123_frame;

/* Dispatch table entry for $ELT{TUP{STR,$CONFIG}}::elt!:TUP{STR,$CONFIG} */
typedef struct dELTTU384262123_entry {
const TUPSTRdCONFIG (*iter)(dELTTU384262123_frame frame);
const void* (*alloc_frame)();
} dELTTU384262123_entry;

typedef struct dELTTU1527749065_frame_struct {
 INT state;
 dELTTU788184410 self;
 } *dELTTU1527749065_frame;

/* Dispatch table entry for $ELT{TUP{STR,FSET{STR}}}::elt!:TUP{STR,FSET{STR}} */
typedef struct dELTTU1527749065_entry {
const TUPSTRFSETSTR (*iter)(dELTTU1527749065_frame frame);
const void* (*alloc_frame)();
} dELTTU1527749065_entry;

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

typedef struct dELTTU1171292506_frame_struct {
 INT state;
 dELTTU216883428 self;
 } *dELTTU1171292506_frame;

/* Dispatch table entry for $ELT{TUP{TP_CLASS,FSET{$TP}}}::elt!:TUP{TP_CLASS,FSET{$TP}} */
typedef struct dELTTU1171292506_entry {
const TUPTP_858321267 (*iter)(dELTTU1171292506_frame frame);
const void* (*alloc_frame)();
} dELTTU1171292506_entry;

typedef struct dELTTU963535579_frame_struct {
 INT state;
 dELTTU186609637 self;
 } *dELTTU963535579_frame;

/* Dispatch table entry for $ELT{TUP{TP_CLASS,FSET{TP_CLASS}}}::elt!:TUP{TP_CLASS,FSET{TP_CLASS}} */
typedef struct dELTTU963535579_entry {
const TUPTP_1668454172 (*iter)(dELTTU963535579_frame frame);
const void* (*alloc_frame)();
} dELTTU963535579_entry;

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

typedef struct ARRAY21272457581_frame_struct {
 INT state;
 ARRAY2BOOL self;/* Formal argument: self */
 BOOL ret_val;
 ARRAY2BOOL L41;
 INT L11;
 BOOL f_L21_; /* used by builtin iter */
 INT L21_br; /* used by builtin iter */
 INT i_L21_; /* used by builtin iter */
 } *ARRAY21272457581_frame;

typedef struct ARRAYA945146506_frame_struct {
 INT state;
 ARRAYA855692714 self;/* Formal argument: self */
 AM_BND1124877163 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA945146506_frame;

typedef struct ARRAYA1632293813_frame_struct {
 INT state;
 ARRAYA132553625 self;/* Formal argument: self */
 AM_BND367211769 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1632293813_frame;

typedef struct ARRAYA2146369463_frame_struct {
 INT state;
 ARRAYA36280886 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA2146369463_frame;

typedef struct ARRAYA900901929_frame_struct {
 INT state;
 ARRAYAM_CURSOR self;/* Formal argument: self */
 AM_CURSOR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA900901929_frame;

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

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

typedef struct ARRAYA1660316962_frame_struct {
 INT state;
 ARRAYATTRSORT self;/* Formal argument: self */
 ATTRSORT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1660316962_frame;

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

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

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

typedef struct FLISTdLAYOUT_struct {/* layout for FLIST{$LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FLISTdLAYOUT;

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

typedef struct FLISTA162798263_struct {/* layout for FLIST{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FLISTA162798263;

typedef struct FLISTA1252869158_frame_struct {
 INT state;
 FLISTA162798263 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTA162798263 aget_self;
 INT aget_ind;
 ABSTRACT_LAYOUT ret_val;
 } *FLISTA1252869158_frame;

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTA273121893_frame_struct {
 INT state;
 FLISTA1893461511 self;/* Formal argument: self */
 AM_BND1124877163 ret_val1;
 INT i;
 INT sz;
 FLISTA1893461511 aget_self;
 INT aget_ind;
 AM_BND1124877163 ret_val;
 } *FLISTA273121893_frame;

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTA1446310868_frame_struct {
 INT state;
 FLISTA1409846210 self;/* Formal argument: self */
 AM_BND367211769 ret_val1;
 INT i;
 INT sz;
 FLISTA1409846210 aget_self;
 INT aget_ind;
 AM_BND367211769 ret_val;
 } *FLISTA1446310868_frame;

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

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

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

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

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

typedef struct FSETdL485771726_frame_struct {
 INT state;
 FSETdLAYOUT self;/* Formal argument: self */
 dLAYOUT ret_val1;
 FSETdLAYOUT L61;
 dLAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETdLAYOUT is_elt_nil_self;
 dLAYOUT is_elt_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETdL485771726_frame;

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

typedef struct FSETAB1569698808_struct {/* layout for FSET{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FSETAB1569698808;

typedef struct FSETAB1942662686_frame_struct {
 INT state;
 FSETAB1569698808 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val2;
 FSETAB1569698808 L61;
 ABSTRACT_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETAB1569698808 is_elt_nil_self;
 ABSTRACT_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1559244239 is_elt_nil_self1;
 ABSTRACT_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAB1942662686_frame;

typedef struct FSETAM1032778315_struct {/* layout for FSET{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FSETAM1032778315;

typedef struct FSETAM595751729_frame_struct {
 INT state;
 FSETAM1032778315 self;/* Formal argument: self */
 AM_BND1124877163 ret_val2;
 FSETAM1032778315 L61;
 AM_BND1124877163 r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1032778315 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM595751729_frame;

typedef struct FSETAM1158405984_struct {/* layout for FSET{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FSETAM1158405984;

typedef struct FSETAM441739032_frame_struct {
 INT state;
 FSETAM1158405984 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val2;
 FSETAM1158405984 L61;
 AM_LOCAL_EXPR r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1158405984 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM441739032_frame;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

typedef struct FSETAM1323524382_frame_struct {
 INT state;
 FSETAM_ROUT_DEF self;/* Formal argument: self */
 AM_ROUT_DEF ret_val2;
 FSETAM_ROUT_DEF L61;
 AM_ROUT_DEF r;
 INT i;
 INT sz;
 INT L31;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM1323524382_frame;

typedef struct FSETAM1542678947_struct {/* layout for FSET{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FSETAM1542678947;

typedef struct FSETAM12483681_frame_struct {
 INT state;
 FSETAM1542678947 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val2;
 FSETAM1542678947 L61;
 AM_SHARED_EXPR r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1542678947 is_elt_nil_self;
 AM_SHARED_EXPR is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1377997731 is_elt_nil_self1;
 AM_SHARED_EXPR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM12483681_frame;

typedef struct FSETAS_FEAT_MOD_struct {/* layout for FSET{AS_FEAT_MOD} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AS_FEAT_MOD_struct *arr_part[1];
 } *FSETAS_FEAT_MOD;

typedef struct FSETAS1592299594_frame_struct {
 INT state;
 FSETAS_FEAT_MOD self;/* Formal argument: self */
 AS_FEAT_MOD ret_val2;
 FSETAS_FEAT_MOD L61;
 AS_FEAT_MOD r;
 INT i;
 INT sz;
 INT L31;
 FSETAS_FEAT_MOD is_elt_nil_self;
 AS_FEAT_MOD is_elt_nil_e;
 BOOL ret_val;
 ELT_NI465844240 is_elt_nil_self1;
 AS_FEAT_MOD is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAS1592299594_frame;

typedef struct FSETBO1548397318_struct {/* layout for FSET{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FSETBO1548397318;

typedef struct FSETBO677183779_frame_struct {
 INT state;
 FSETBO1548397318 self;/* Formal argument: self */
 BOUND_2140373124 ret_val2;
 FSETBO1548397318 L61;
 BOUND_2140373124 r;
 INT i;
 INT sz;
 INT L31;
 FSETBO1548397318 is_elt_nil_self;
 BOUND_2140373124 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI338362486 is_elt_nil_self1;
 BOUND_2140373124 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBO677183779_frame;

typedef struct FSETBO1147931784_struct {/* layout for FSET{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FSETBO1147931784;

typedef struct FSETBO10998148_frame_struct {
 INT state;
 FSETBO1147931784 self;/* Formal argument: self */
 BOUND_809390774 ret_val2;
 FSETBO1147931784 L61;
 BOUND_809390774 r;
 INT i;
 INT sz;
 INT L31;
 FSETBO1147931784 is_elt_nil_self;
 BOUND_809390774 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1119971763 is_elt_nil_self1;
 BOUND_809390774 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBO10998148_frame;

typedef struct FSETBU1446477505_struct {/* layout for FSET{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FSETBU1446477505;

typedef struct FSETBU202043707_frame_struct {
 INT state;
 FSETBU1446477505 self;/* Formal argument: self */
 BUILTIN_LAYOUT ret_val2;
 FSETBU1446477505 L61;
 BUILTIN_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETBU1446477505 is_elt_nil_self;
 BUILTIN_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1281796289 is_elt_nil_self1;
 BUILTIN_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETBU202043707_frame;

typedef struct FSETCLASS_LAYOUT_struct {/* layout for FSET{CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FSETCLASS_LAYOUT;

typedef struct FSETCL1233705521_frame_struct {
 INT state;
 FSETCLASS_LAYOUT self;/* Formal argument: self */
 CLASS_LAYOUT ret_val2;
 FSETCLASS_LAYOUT L61;
 CLASS_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETCLASS_LAYOUT is_elt_nil_self;
 CLASS_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI737899749 is_elt_nil_self1;
 CLASS_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETCL1233705521_frame;

typedef struct FSETCS1561418284_struct {/* layout for FSET{CSE_DOUBLE_EXPRS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CSE_DOUBLE_EXPRS_struct *arr_part[1];
 } *FSETCS1561418284;

typedef struct FSETCS1170733135_frame_struct {
 INT state;
 FSETCS1561418284 self;/* Formal argument: self */
 CSE_DOUBLE_EXPRS ret_val2;
 FSETCS1561418284 L61;
 CSE_DOUBLE_EXPRS r;
 INT i;
 INT sz;
 INT L31;
 FSETCS1561418284 is_elt_nil_self;
 CSE_DOUBLE_EXPRS is_elt_nil_e;
 BOOL ret_val;
 ELT_NI881793976 is_elt_nil_self1;
 CSE_DOUBLE_EXPRS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETCS1170733135_frame;

typedef struct FSETEX1260947371_struct {/* layout for FSET{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FSETEX1260947371;

typedef struct FSETEX1633103446_frame_struct {
 INT state;
 FSETEX1260947371 self;/* Formal argument: self */
 EXTERNAL_LAYOUT ret_val2;
 FSETEX1260947371 L61;
 EXTERNAL_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETEX1260947371 is_elt_nil_self;
 EXTERNAL_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI94923122 is_elt_nil_self1;
 EXTERNAL_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETEX1633103446_frame;

typedef struct FSETIM184436961_struct {/* layout for FSET{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FSETIM184436961;

typedef struct FSETIM887960645_frame_struct {
 INT state;
 FSETIM184436961 self;/* Formal argument: self */
 IMMUTA421759893 ret_val2;
 FSETIM184436961 L61;
 IMMUTA421759893 r;
 INT i;
 INT sz;
 INT L31;
 FSETIM184436961 is_elt_nil_self;
 IMMUTA421759893 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1394471793 is_elt_nil_self1;
 IMMUTA421759893 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETIM887960645_frame;

typedef struct FSETSFILE_ID_struct {/* layout for FSET{SFILE_ID} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SFILE_ID_struct arr_part[1];
 } *FSETSFILE_ID;

typedef struct FSETSF139710295_frame_struct {
 INT state;
 FSETSFILE_ID self;/* Formal argument: self */
 SFILE_ID ret_val2;
 FSETSFILE_ID L61;
 SFILE_ID r;
 INT i;
 INT sz;
 INT L31;
 FSETSFILE_ID is_elt_nil_self;
 SFILE_ID is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSFILE_ID is_elt_nil_self1;
 SFILE_ID is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSF139710295_frame;

typedef struct FSETSIDE_EFFECT_struct {/* layout for FSET{SIDE_EFFECT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIDE_EFFECT_struct *arr_part[1];
 } *FSETSIDE_EFFECT;

typedef struct FSETSI2093978094_frame_struct {
 INT state;
 FSETSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val2;
 FSETSIDE_EFFECT L61;
 SIDE_EFFECT r;
 INT i;
 INT sz;
 INT L31;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSI2093978094_frame;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

typedef struct FSETSIG_eltbrSIG_frame_struct {
 INT state;
 FSETSIG self;/* Formal argument: self */
 SIG ret_val2;
 FSETSIG L61;
 SIG r;
 INT i;
 INT sz;
 INT L31;
 FSETSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSIG_eltbrSIG_frame;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

typedef struct FSETSTR_eltbrSTR_frame_struct {
 INT state;
 FSETSTR self;/* Formal argument: self */
 STR ret_val2;
 FSETSTR L61;
 STR r;
 INT i;
 INT sz;
 INT L31;
 FSETSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETSTR_eltbrSTR_frame;

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

typedef struct FSETTUPSIGSIG_struct {/* layout for FSET{TUP{SIG,SIG}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPSIGSIG_struct arr_part[1];
 } *FSETTUPSIGSIG;

typedef struct FSETTU845419336_frame_struct {
 INT state;
 FSETTUPSIGSIG self;/* Formal argument: self */
 TUPSIGSIG ret_val2;
 FSETTUPSIGSIG L61;
 TUPSIGSIG r;
 INT i;
 INT sz;
 INT L31;
 FSETTUPSIGSIG is_elt_nil_self;
 TUPSIGSIG is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTUPSIGSIG is_elt_nil_self1;
 TUPSIGSIG is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTU845419336_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

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

typedef struct dELTAM745071702_frame_struct {
 INT state;
 dELTAM1766315002 self;
 } *dELTAM745071702_frame;

/* Dispatch table entry for $ELT{AM_BND_CREATE_EXPR}::elt!:AM_BND_CREATE_EXPR */
typedef struct dELTAM745071702_entry {
const AM_BND1124877163 (*iter)(dELTAM745071702_frame frame);
const void* (*alloc_frame)();
} dELTAM745071702_entry;

typedef struct dELTAM157362729_frame_struct {
 INT state;
 dELTAM1208385872 self;
 } *dELTAM157362729_frame;

/* Dispatch table entry for $ELT{AM_BND_ITER_CALL_EXPR}::elt!:AM_BND_ITER_CALL_EXPR */
typedef struct dELTAM157362729_entry {
const AM_BND367211769 (*iter)(dELTAM157362729_frame frame);
const void* (*alloc_frame)();
} dELTAM157362729_entry;

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

typedef struct dELTAM1806540817_frame_struct {
 INT state;
 dELTAM938692099 self;
 } *dELTAM1806540817_frame;

/* Dispatch table entry for $ELT{AM_ITER_CALL_EXPR}::elt!:AM_ITER_CALL_EXPR */
typedef struct dELTAM1806540817_entry {
const AM_ITE1809135850 (*iter)(dELTAM1806540817_frame frame);
const void* (*alloc_frame)();
} dELTAM1806540817_entry;

typedef struct dELTAM128970917_frame_struct {
 INT state;
 dELTAM1985205111 self;
 } *dELTAM128970917_frame;

/* Dispatch table entry for $ELT{AM_LOCAL_EXPR}::elt!:AM_LOCAL_EXPR */
typedef struct dELTAM128970917_entry {
const AM_LOCAL_EXPR (*iter)(dELTAM128970917_frame frame);
const void* (*alloc_frame)();
} dELTAM128970917_entry;

typedef struct dELTAM186957519_frame_struct {
 INT state;
 dELTAM_ROUT_DEF self;
 } *dELTAM186957519_frame;

/* Dispatch table entry for $ELT{AM_ROUT_DEF}::elt!:AM_ROUT_DEF */
typedef struct dELTAM186957519_entry {
const AM_ROUT_DEF (*iter)(dELTAM186957519_frame frame);
const void* (*alloc_frame)();
} dELTAM186957519_entry;

typedef struct dELTAM119654938_frame_struct {
 INT state;
 dELTAM1943610286 self;
 } *dELTAM119654938_frame;

/* Dispatch table entry for $ELT{AM_SHARED_EXPR}::elt!:AM_SHARED_EXPR */
typedef struct dELTAM119654938_entry {
const AM_SHARED_EXPR (*iter)(dELTAM119654938_frame frame);
const void* (*alloc_frame)();
} dELTAM119654938_entry;

typedef struct dELTARG_eltbrARG_frame_struct {
 INT state;
 dELTARG self;
 } *dELTARG_eltbrARG_frame;

/* Dispatch table entry for $ELT{ARG}::elt!:ARG */
typedef struct dELTARG_eltbrARG_entry {
const ARG (*iter)(dELTARG_eltbrARG_frame frame);
const void* (*alloc_frame)();
} dELTARG_eltbrARG_entry;

typedef struct dELTAR657435157_frame_struct {
 INT state;
 dELTARRAYBOOL self;
 } *dELTAR657435157_frame;

/* Dispatch table entry for $ELT{ARRAY{BOOL}}::elt!:ARRAY{BOOL} */
typedef struct dELTAR657435157_entry {
const ARRAYBOOL (*iter)(dELTAR657435157_frame frame);
const void* (*alloc_frame)();
} dELTAR657435157_entry;

typedef struct dELTAS81817693_frame_struct {
 INT state;
 dELTAS_FEAT_MOD self;
 } *dELTAS81817693_frame;

/* Dispatch table entry for $ELT{AS_FEAT_MOD}::elt!:AS_FEAT_MOD */
typedef struct dELTAS81817693_entry {
const AS_FEAT_MOD (*iter)(dELTAS81817693_frame frame);
const void* (*alloc_frame)();
} dELTAS81817693_entry;

typedef struct dELTCO1151854791_frame_struct {
 INT state;
 dELTCODE_FILE self;
 } *dELTCO1151854791_frame;

/* Dispatch table entry for $ELT{CODE_FILE}::elt!:CODE_FILE */
typedef struct dELTCO1151854791_entry {
const CODE_FILE (*iter)(dELTCO1151854791_frame frame);
const void* (*alloc_frame)();
} dELTCO1151854791_entry;

typedef struct dELTFL688066606_frame_struct {
 INT state;
 dELTFL1838879018 self;
 } *dELTFL688066606_frame;

/* Dispatch table entry for $ELT{FLIST{$AM_CONST}}::elt!:FLIST{$AM_CONST} */
typedef struct dELTFL688066606_entry {
const FLISTdAM_CONST (*iter)(dELTFL688066606_frame frame);
const void* (*alloc_frame)();
} dELTFL688066606_entry;

typedef struct dELTFL27095446_frame_struct {
 INT state;
 dELTFLISTSTR self;
 } *dELTFL27095446_frame;

/* Dispatch table entry for $ELT{FLIST{STR}}::elt!:FLIST{STR} */
typedef struct dELTFL27095446_entry {
const FLISTSTR (*iter)(dELTFL27095446_frame frame);
const void* (*alloc_frame)();
} dELTFL27095446_entry;

typedef struct dELTFS553577478_frame_struct {
 INT state;
 dELTFSTR self;
 } *dELTFS553577478_frame;

/* Dispatch table entry for $ELT{FSTR}::elt!:FSTR */
typedef struct dELTFS553577478_entry {
const FSTR (*iter)(dELTFS553577478_frame frame);
const void* (*alloc_frame)();
} dELTFS553577478_entry;

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

typedef struct A_STAC408111917_frame_struct {
 INT state;
 A_STAC1244966098 self;/* Formal argument: self */
 AM_CURSOR_POS ret_val1;
 INT L31;
 INT L61;
 FLISTA886630705 aget_self;
 INT aget_ind;
 AM_CURSOR_POS ret_val;
 BOOL f_L11_; /* used by builtin iter */
 INT L11_,L11_m; /* used by builtin iter */
 } *A_STAC408111917_frame;

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


extern RETURNED_CONST AM_CALL_ARG (**dAM_CA967858872)(dAM_CALL_EXPR, INT);

extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST AS_STMT_LIST (**dAS_ST445865822)(dAS_STMT);

extern RETURNED_CONST AS_TYPE_SPEC (**dTP_as1220129267)(dTP);

extern RETURNED_CONST BOOL (**dAM_CO637713226)(dAM_CONST, OB);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST BOOL (**dAS_CL1087157762)(dAS_CLASS_ELT);

extern RETURNED_CONST BOOL (**dAS_ST1616688325)(dAS_STMT);

extern RETURNED_CONST BOOL (**dCALL_358880146)(dCALL_TP, dTP);

extern RETURNED_CONST BOOL (**dINLIN1181425367)(dINLINE_ROUT_SIG);

extern RETURNED_CONST BOOL (**dINLIN203982169)(dINLINE, SIG);

extern RETURNED_CONST BOOL (**dINLIN2120180991)(dINLINE_ITER_SIG);

extern RETURNED_CONST BOOL (**dIS_EQ1272012319)(dIS_EQ, OB);

extern RETURNED_CONST BOOL (**dIS_NI446211542)(dIS_NIL);

extern RETURNED_CONST BOOL (**dMODE_2144735300)(dMODE);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is244083069)(dTP);

extern RETURNED_CONST BOOL (**dTP_is33989660)(dTP);

extern RETURNED_CONST BOOL (**dTP_is411624931)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is650936518)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST BOOL (**dTP_is_crBOOL)(dTP);

extern RETURNED_CONST FLISTA725283029 (**dAM_EX1656723676)(dAM_EXPR);

extern RETURNED_CONST FLISTdLAYOUT (**dLAYOU1845593458)(dLAYOUT);

extern RETURNED_CONST FSETSIG (**dGENER354174443)(dGENERATE_AM);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dAM_CA1128544926)(dAM_CALL_EXPR);

extern RETURNED_CONST INT (**dAM_CO1597271567)(dAM_CONST);

extern RETURNED_CONST INT (**dAM_EX1001627434)(dAM_EXPR);

extern RETURNED_CONST INT (**dAS_EX1952576959)(dAS_EXPR);

extern RETURNED_CONST INT (**dHASH_hashrINT)(dHASH);

extern RETURNED_CONST INT (**dTP_hashrINT)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST PROG (**dTP_progrPROG)(dTP);

extern RETURNED_CONST SFILE_ID (**dAM_EX2051891691)(dAM_EXPR);

extern RETURNED_CONST SFILE_ID (**dPROG_848621491)(dPROG_ERR);

extern RETURNED_CONST SIG (**dGET_M422427726)(dGET_MAIN_SIG);

extern RETURNED_CONST SIG (**dINLIN1366832129)(dINLINE_ROUT_SIG);

extern RETURNED_CONST SIG (**dINLIN1426868953)(dINLINE_ITER_SIG);

extern RETURNED_CONST STR (**dCALL_TP_strrSTR)(dCALL_TP);

extern RETURNED_CONST STR (**dCONFIG_namerSTR)(dCONFIG);

extern RETURNED_CONST STR (**dCONFIG_strrSTR)(dCONFIG);

extern RETURNED_CONST STR (**dGET_O723107324)(dGET_OPTIONS);

extern RETURNED_CONST STR (**dLAYOU959283167)(dLAYOUT, CGEN);

extern RETURNED_CONST STR (**dLAYOUT_strrSTR)(dLAYOUT);

extern RETURNED_CONST STR (**dMODE_strrSTR)(dMODE);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_EXPR (**dINLIN104160276)(dINLINE_ITER_SIG, AM_ITE1809135850);

extern RETURNED_CONST dAM_EXPR (**dINLIN1378053623)(dINLINE, AM_ROUT_DEF, AM_ROU1916046290);

extern RETURNED_CONST dAM_EXPR (**dINLIN17214190)(dINLINE, AM_ITE1809135850);

extern RETURNED_CONST dAM_EXPR (**dINLIN516975284)(dINLINE_ROUT_SIG, AM_ROU1916046290);

extern RETURNED_CONST dAM_EXPR (**dINLIN961094327)(dINLINE, AM_ROUT_DEF, AM_LOOP_STMT, AM_ITE1809135850);

extern RETURNED_CONST dAM_EXPR (**dINLIN970826022)(dINLINE, AM_ROU1916046290);

extern RETURNED_CONST dAM_STMT (**dAM_ST1032597839)(dAM_STMT);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST dAS_CLASS_ELT (**dAS_CL760354380)(dAS_CLASS_ELT);

extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST dELTAB1485261483_entry *dELTAB1485261483;

extern RETURNED_CONST dELTAM119654938_entry *dELTAM119654938;

extern RETURNED_CONST dELTAM1280476813_entry *dELTAM1280476813;

extern RETURNED_CONST dELTAM128970917_entry *dELTAM128970917;

extern RETURNED_CONST dELTAM1395458731_entry *dELTAM1395458731;

extern RETURNED_CONST dELTAM1417430129_entry *dELTAM1417430129;

extern RETURNED_CONST dELTAM157362729_entry *dELTAM157362729;

extern RETURNED_CONST dELTAM1806540817_entry *dELTAM1806540817;

extern RETURNED_CONST dELTAM186957519_entry *dELTAM186957519;

extern RETURNED_CONST dELTAM2044693667_entry *dELTAM2044693667;

extern RETURNED_CONST dELTAM745071702_entry *dELTAM745071702;

extern RETURNED_CONST dELTAR1531173752_entry *dELTAR1531173752;

extern RETURNED_CONST dELTAR391526113_entry *dELTAR391526113;

extern RETURNED_CONST dELTAR657435157_entry *dELTAR657435157;

extern RETURNED_CONST dELTARG_eltbrARG_entry *dELTARG_eltbrARG;

extern RETURNED_CONST dELTAS81817693_entry *dELTAS81817693;

extern RETURNED_CONST dELTAT1600626566_entry *dELTAT1600626566;

extern RETURNED_CONST dELTBO1349583384_entry *dELTBO1349583384;

extern RETURNED_CONST dELTBO582795216_entry *dELTBO582795216;

extern RETURNED_CONST dELTBO879923169_entry *dELTBO879923169;

extern RETURNED_CONST dELTBU334182326_entry *dELTBU334182326;

extern RETURNED_CONST dELTCA1528219566_entry *dELTCA1528219566;

extern RETURNED_CONST dELTCH686923168_entry *dELTCH686923168;

extern RETURNED_CONST dELTCL1056825190_entry *dELTCL1056825190;

extern RETURNED_CONST dELTCO1151854791_entry *dELTCO1151854791;

extern RETURNED_CONST dELTCS742343900_entry *dELTCS742343900;

extern RETURNED_CONST dELTEX1931613508_entry *dELTEX1931613508;

extern RETURNED_CONST dELTEX2090504649_entry *dELTEX2090504649;

extern RETURNED_CONST dELTFL27095446_entry *dELTFL27095446;

extern RETURNED_CONST dELTFL688066606_entry *dELTFL688066606;

extern RETURNED_CONST dELTFS553577478_entry *dELTFS553577478;

extern RETURNED_CONST dELTID1378398307_entry *dELTID1378398307;

extern RETURNED_CONST dELTIFC_eltbrIFC_entry *dELTIFC_eltbrIFC;

extern RETURNED_CONST dELTIM1138806518_entry *dELTIM1138806518;

extern RETURNED_CONST dELTIM1958116958_entry *dELTIM1958116958;

extern RETURNED_CONST dELTINT_eltbrINT_entry *dELTINT_eltbrINT;

extern RETURNED_CONST dELTOP2009500256_entry *dELTOP2009500256;

extern RETURNED_CONST dELTSF338070590_entry *dELTSF338070590;

extern RETURNED_CONST dELTSI583496193_entry *dELTSI583496193;

extern RETURNED_CONST dELTSIG_eltbrSIG_entry *dELTSIG_eltbrSIG;

extern RETURNED_CONST dELTSTR_eltbrSTR_entry *dELTSTR_eltbrSTR;

extern RETURNED_CONST dELTTP817795506_entry *dELTTP817795506;

extern RETURNED_CONST dELTTP992626963_entry *dELTTP992626963;

extern RETURNED_CONST dELTTU1031278189_entry *dELTTU1031278189;

extern RETURNED_CONST dELTTU1084171889_entry *dELTTU1084171889;

extern RETURNED_CONST dELTTU1106260535_entry *dELTTU1106260535;

extern RETURNED_CONST dELTTU1108719580_entry *dELTTU1108719580;

extern RETURNED_CONST dELTTU1157822078_entry *dELTTU1157822078;

extern RETURNED_CONST dELTTU1171292506_entry *dELTTU1171292506;

extern RETURNED_CONST dELTTU1525626981_entry *dELTTU1525626981;

extern RETURNED_CONST dELTTU1527749065_entry *dELTTU1527749065;

extern RETURNED_CONST dELTTU164160693_entry *dELTTU164160693;

extern RETURNED_CONST dELTTU1654314914_entry *dELTTU1654314914;

extern RETURNED_CONST dELTTU1697593574_entry *dELTTU1697593574;

extern RETURNED_CONST dELTTU1800917955_entry *dELTTU1800917955;

extern RETURNED_CONST dELTTU1837273309_entry *dELTTU1837273309;

extern RETURNED_CONST dELTTU1923274911_entry *dELTTU1923274911;

extern RETURNED_CONST dELTTU1973395837_entry *dELTTU1973395837;

extern RETURNED_CONST dELTTU202706072_entry *dELTTU202706072;

extern RETURNED_CONST dELTTU2071468085_entry *dELTTU2071468085;

extern RETURNED_CONST dELTTU2080726233_entry *dELTTU2080726233;

extern RETURNED_CONST dELTTU2146728406_entry *dELTTU2146728406;

extern RETURNED_CONST dELTTU225750038_entry *dELTTU225750038;

extern RETURNED_CONST dELTTU238277205_entry *dELTTU238277205;

extern RETURNED_CONST dELTTU384262123_entry *dELTTU384262123;

extern RETURNED_CONST dELTTU538545734_entry *dELTTU538545734;

extern RETURNED_CONST dELTTU577707672_entry *dELTTU577707672;

extern RETURNED_CONST dELTTU590857063_entry *dELTTU590857063;

extern RETURNED_CONST dELTTU686046061_entry *dELTTU686046061;

extern RETURNED_CONST dELTTU72816407_entry *dELTTU72816407;

extern RETURNED_CONST dELTTU784118309_entry *dELTTU784118309;

extern RETURNED_CONST dELTTU963535579_entry *dELTTU963535579;

extern RETURNED_CONST dELTTU975440397_entry *dELTTU975440397;

extern RETURNED_CONST dELTdA379108592_entry *dELTdA379108592;

extern RETURNED_CONST dELTdA418602402_entry *dELTdA418602402;

extern RETURNED_CONST dELTdA578383841_entry *dELTdA578383841;

extern RETURNED_CONST dELTdL669601151_entry *dELTdL669601151;

extern RETURNED_CONST dELTdTP_eltbrdTP_entry *dELTdTP_eltbrdTP;

extern RETURNED_CONST dNIL (**dNIL_nilrdNIL)(dNIL);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAM_ST2020072576)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_CL1134203643)(dAS_CLASS_ELT, BOOL);

extern RETURNED_CONST void (**dAS_CL1804429169)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_EX1809609236)(dAS_EXPR, dAS_EXPR);

extern RETURNED_CONST void (**dAS_EX1889860882)(dAS_EXPR, dAS_EXPR);

extern RETURNED_CONST void (**dAS_EX753042462)(dAS_EXPR, SFILE_ID);

extern RETURNED_CONST void (**dAS_ST1190543153)(dAS_STMT, SFILE_ID);

extern RETURNED_CONST void (**dAS_ST1454156204)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dAS_ST1569642444)(dAS_STMT, BOOL);

extern RETURNED_CONST void (**dAS_ST164863607)(dAS_STMT, AS_STMT_LIST);

extern RETURNED_CONST void (**dAS_ST1687647066)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dBACK_1049564105)(dBACK_END);

extern RETURNED_CONST void (**dBACK_1132541334)(dBACK_END, AM_ROUT_DEF);

extern RETURNED_CONST void (**dBACK_END_init)(dBACK_END);

extern RETURNED_CONST void (**dBUILD408006298)(dBUILD508976674);

extern RETURNED_CONST void (**dCHECK1977119478)(dCHECK_AM);

extern RETURNED_CONST void (**dCHECK450012483)(dCHECK1877276428);

extern RETURNED_CONST void (**dFIND_896803978)(dFIND_TYPES);

extern RETURNED_CONST void (**dGENER1765432365)(dGENERATE_AM);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);

extern RETURNED_CONST void (**dGET_M1653483544)(dGET_MAIN_SIG, STR);

extern RETURNED_CONST void (**dGET_O2013757500)(dGET_OPTIONS);

extern RETURNED_CONST void (**dGET_O2036054845)(dGET_OPTIONS, ARRAYSTR);

extern RETURNED_CONST void (**dINLIN580030003)(dINLINE, AM_ROUT_DEF);

extern RETURNED_CONST void (**dINLINE_init)(dINLINE);

extern RETURNED_CONST void (**dOPTIM1147845184)(dOPTIMIZE, AM_ROUT_DEF);

extern RETURNED_CONST void (**dOPTIM311966595)(dOPTIMIZE);

extern RETURNED_CONST void (**dOPTIM678468045)(dOPTIMIZE, AM_ROUT_DEF);

extern RETURNED_CONST void (**dOPTIMIZE_init)(dOPTIMIZE);

extern RETURNED_CONST void (**dPARSE_parse_STR)(dPARSE, STR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);

extern RETURNED_CONST void (**dSTAT_print)(dSTAT);
ABSTRACT_LAYOUT ARRAYA967821567(ARRAYA967821567_frame);
ABSTRACT_LAYOUT FLISTA1252869158(FLISTA1252869158_frame);
ABSTRACT_LAYOUT FSETAB1942662686(FSETAB1942662686_frame);
AM_ANY_EXPR AM_ANY934244480(AM_ANY_EXPR);
AM_ARRAY_EXPR AM_ARR1852652918(AM_ARRAY_EXPR);
AM_ARR_CONST AM_ARR990477897(AM_ARR_CONST);
AM_ASSERT_STMT AM_ASS1481858691(AM_ASSERT_STMT);
AM_ASSIGN_STMT AM_ASS782632663(AM_ASSIGN_STMT);
AM_ATTACH_STMT AM_ATT191404837(AM_ATTACH_STMT);
AM_ATTR_EXPR AM_ATT1705760219(AM_ATTR_EXPR);
AM_AT_EXPR AM_AT_1135400775(AM_AT_EXPR);
AM_BND1124877163 AM_BND831738871(AM_BND1124877163);
AM_BND1124877163 ARRAYA945146506(ARRAYA945146506_frame);
AM_BND1124877163 FLISTA273121893(FLISTA273121893_frame);
AM_BND1124877163 FSETAM595751729(FSETAM595751729_frame);
AM_BND260301329 AM_BND2052779026(AM_BND260301329);
AM_BND367211769 AM_BND1416432078(AM_BND367211769);
AM_BND367211769 ARRAYA1632293813(ARRAYA1632293813_frame);
AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame);
AM_BOOL_CONST AM_BOO158864518(AM_BOOL_CONST);
AM_BREAK_STMT AM_BRE1937656818(AM_BREAK_STMT);
AM_CALL_ARG ARRAYA44788561(ARRAYA44788561_frame);
AM_CALL_ARG FLISTA964597258(FLISTA964597258_frame);
AM_CASE_STMT AM_CAS1898951205(AM_CASE_STMT);
AM_CHAR_CONST AM_CHA916745766(AM_CHAR_CONST);
AM_CLU1286269335 AM_CLU779855221(AM_CLU1286269335);
AM_CLUSTER_EXPR AM_CLU1577744436(AM_CLUSTER_EXPR);
AM_COMMENT_STMT AM_COM977130404(AM_COMMENT_STMT);
AM_CONST AM_CON582703191(AM_CONST);
AM_CURSOR ARRAYA900901929(ARRAYA900901929_frame);
AM_CURSOR FLISTA84883712(FLISTA84883712_frame);
AM_CURSOR_POS ARRAYA2146369463(ARRAYA2146369463_frame);
AM_CURSOR_POS A_STAC408111917(A_STAC408111917_frame);
AM_CURSOR_POS FLISTA1660160(FLISTA1660160_frame);
AM_EXCEPT_EXPR AM_EXC449573271(AM_EXCEPT_EXPR);
AM_EXPR_STMT AM_EXP359209435(AM_EXPR_STMT);
AM_EXT_CALL_EXPR AM_EXT1687283405(AM_EXT_CALL_EXPR);
AM_FAR_EXPR AM_FAR755578228(AM_FAR_EXPR);
AM_FLTDX_CONST AM_FLT822990115(AM_FLTDX_CONST);
AM_FLTD_CONST AM_FLT1839670182(AM_FLTD_CONST);
AM_FLTI_CONST AM_FLT425117946(AM_FLTI_CONST);
AM_FLTX_CONST AM_FLT476428534(AM_FLTX_CONST);
AM_FLT_CONST AM_FLT1362803603(AM_FLT_CONST);
AM_FORK_STMT AM_FOR214044719(AM_FORK_STMT);
AM_FORMAL_ARG ARRAYA1315754333(ARRAYA1315754333_frame);
AM_FORMAL_ARG FLISTA831183340(FLISTA831183340_frame);
AM_GLOBAL_EXPR AM_GLO928972965(AM_GLOBAL_EXPR);
AM_HERE_EXPR AM_HER1301311031(AM_HERE_EXPR);
AM_IF_EXPR AM_IF_1266575443(AM_IF_EXPR);
AM_IF_STMT AM_IF_1702280121(AM_IF_STMT);
AM_INITIAL_STMT AM_INI1487747776(AM_INITIAL_STMT);
AM_INTI_CONST AM_INT304374138(AM_INTI_CONST);
AM_INT_CONST AM_INT1629629861(AM_INT_CONST);
AM_INV1167837230 AM_INV1353049642(AM_INV1167837230);
AM_IS_VOID_EXPR AM_IS_193101840(AM_IS_VOID_EXPR);
AM_ITE1809135850 AM_ITE380500422(AM_ITE1809135850);
AM_ITE1809135850 ARRAYA494342579(ARRAYA494342579_frame);
AM_ITE1809135850 FLISTA20621028(FLISTA20621028_frame);
AM_LOCAL_EXPR AM_LOC1798202130(AM_LOCAL_EXPR);
AM_LOCAL_EXPR ARRAYA27295121(ARRAYA27295121_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_LOCAL_EXPR FSETAM441739032(FSETAM441739032_frame);
AM_LOCK_STMT AM_LOC1474178511(AM_LOCK_STMT);
AM_LOOP_STMT AM_LOO1572875803(AM_LOOP_STMT);
AM_NEAR_EXPR AM_NEA1086037759(AM_NEAR_EXPR);
AM_NEW_EXPR AM_NEW1250698656(AM_NEW_EXPR);
AM_PAR_STMT AM_PAR784084248(AM_PAR_STMT);
AM_POST_STMT AM_POS1078136755(AM_POST_STMT);
AM_PREFETCH_STMT AM_PRE896696117(AM_PREFETCH_STMT);
AM_PRE_STMT AM_PRE202271760(AM_PRE_STMT);
AM_PROTECT_STMT AM_PRO1363632140(AM_PROTECT_STMT);
AM_RAISE_STMT AM_RAI1825238682(AM_RAISE_STMT);
AM_RETURN_STMT AM_RET1651971761(AM_RETURN_STMT);
AM_ROU1916046290 AM_ROU960965786(AM_ROU1916046290);
AM_ROUT_DEF ARRAYA1253289773(ARRAYA1253289773_frame);
AM_ROUT_DEF FLISTA2121868826(FLISTA2121868826_frame);
AM_ROUT_DEF FSETAM1323524382(FSETAM1323524382_frame);
AM_SHARED_EXPR AM_SHA1035965785(AM_SHARED_EXPR);
AM_SHARED_EXPR ARRAYA1830087990(ARRAYA1830087990_frame);
AM_SHARED_EXPR FLISTA120298683(FLISTA120298683_frame);
AM_SHARED_EXPR FSETAM12483681(FSETAM12483681_frame);
AM_STMT_EXPR AM_STM416210327(AM_STMT_EXPR);
AM_STR_CONST AM_STR1745454041(AM_STR_CONST);
AM_SYNC_STMT AM_SYN1962040721(AM_SYNC_STMT);
AM_TYPECASE_STMT AM_TYP1756924987(AM_TYPECASE_STMT);
AM_UNLOCK_STMT AM_UNL460993177(AM_UNLOCK_STMT);
AM_VAR744470097 AM_VAR1906690564(AM_VAR744470097);
AM_VAT319982528 AM_VAT2137026865(AM_VAT319982528);
AM_VOID_CONST AM_VOI1734446450(AM_VOID_CONST);
AM_WAITFOR_STMT AM_WAI1946639376(AM_WAITFOR_STMT);
AM_WHERE_EXPR AM_WHE1264983710(AM_WHERE_EXPR);
AM_WIT1996971603 AM_WIT1785216238(AM_WIT1996971603);
AM_YIELD_STMT AM_YIE197772790(AM_YIELD_STMT);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARG FLISTA420234122(FLISTA420234122_frame);
ARRAYBOOL ARRAYA277860273(ARRAYA277860273_frame);
ARRAYBOOL FLISTA538157944(FLISTA538157944_frame);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
ARRAYSTR FLISTA970595569(FLISTA970595569_frame);
ARRAYdAM_EXPR ARRAYA289850317(ARRAYA289850317_frame);
ARRAYdAM_EXPR FLISTA1857087356(FLISTA1857087356_frame);
AS_CLASS_DEF PARSE_962387968(PARSE, IDENT, INT);
AS_FEAT_MOD ARRAYA1522064985(ARRAYA1522064985_frame);
AS_FEAT_MOD FLISTA1853093614(FLISTA1853093614_frame);
AS_FEAT_MOD FSETAS1592299594(FSETAS1592299594_frame);
AS_STMT_LIST AS_ASS1748452704(AS_ASSERT_STMT);
AS_STMT_LIST AS_ASS369354997(AS_ASSIGN_STMT);
AS_STMT_LIST AS_ATT1199964935(AS_ATTACH_STMT);
AS_STMT_LIST AS_CAS422199583(AS_CASE_STMT);
AS_STMT_LIST AS_DEC1951684792(AS_DEC_STMT);
AS_STMT_LIST AS_EXP80096096(AS_EXPR_STMT);
AS_STMT_LIST AS_FOR1300129919(AS_FORK_STMT);
AS_STMT_LIST AS_IF_1377965621(AS_IF_STMT);
AS_STMT_LIST AS_INT1290825245(AS_INT2031239268);
AS_STMT_LIST AS_LOC1425522258(AS_LOCK_STMT);
AS_STMT_LIST AS_LOO1823271233(AS_LOOP_STMT);
AS_STMT_LIST AS_PAR380674305(AS_PAR_STMT);
AS_STMT_LIST AS_PRO1473178483(AS_PROTECT_STMT);
AS_STMT_LIST AS_QUI1856043734(AS_QUIT_STMT);
AS_STMT_LIST AS_RAI1834286773(AS_RAISE_STMT);
AS_STMT_LIST AS_RET1323242550(AS_RETURN_STMT);
AS_STMT_LIST AS_STM87227529(AS_STMT);
AS_STMT_LIST AS_SYN1536258462(AS_SYNC_STMT);
AS_STMT_LIST AS_TYP2083833657(AS_TYPECASE_STMT);
AS_STMT_LIST AS_UNL900516002(AS_UNLOCK_STMT);
AS_STMT_LIST AS_WIT1668425008(AS_WIT1063437351);
AS_STMT_LIST AS_YIE913253602(AS_YIELD_STMT);
AS_TYPE_SPEC TP_CLA981298431(TP_CLASS);
AS_TYPE_SPEC TP_ITE610562510(TP_ITER);
AS_TYPE_SPEC TP_ROU1982035318(TP_ROUT);
ATTRSORT ARRAYA1660316962(ARRAYA1660316962_frame);
ATTRSORT FLISTA901142755(FLISTA901142755_frame);
BOOL AM_ANY1687381320(AM_ANY_EXPR, OB);
BOOL AM_ANY309199231(AM_ANY_EXPR);
BOOL AM_ARR1751046750(AM_ARRAY_EXPR, OB);
BOOL AM_ARR437965311(AM_ARRAY_EXPR);
BOOL AM_ARR495057476(AM_ARR_CONST);
BOOL AM_ARR62730649(AM_ARR_CONST, OB);
BOOL AM_ATT1582125658(AM_ATTR_EXPR);
BOOL AM_ATT1818896075(AM_ATTR_EXPR, OB);
BOOL AM_AT_1010480980(AM_AT_EXPR, OB);
BOOL AM_AT_1425681295(AM_AT_EXPR);
BOOL AM_BND1565517842(AM_BND367211769);
BOOL AM_BND1588574812(AM_BND1124877163, OB);
BOOL AM_BND1666770893(AM_BND260301329, OB);
BOOL AM_BND1960601142(AM_BND260301329);
BOOL AM_BND48137285(AM_BND367211769, OB);
BOOL AM_BND9208257(AM_BND1124877163);
BOOL AM_BOO1499785931(AM_BOOL_CONST, OB);
BOOL AM_BOO274293822(AM_BOOL_CONST);
BOOL AM_CHA1550967974(AM_CHAR_CONST);
BOOL AM_CHA181917731(AM_CHAR_CONST, OB);
BOOL AM_CLU1159843664(AM_CLU1286269335, OB);
BOOL AM_CLU170440611(AM_CLU1286269335);
BOOL AM_CLU528836536(AM_CLUSTER_EXPR, OB);
BOOL AM_CLU862886673(AM_CLUSTER_EXPR);
BOOL AM_CON174643192(AM_CONST);
BOOL AM_CON357292637(AM_CONST, OB);
BOOL AM_CUR1683770295(AM_CURSOR, OB);
BOOL AM_CUR217486862(AM_CURSOR_POS, OB);
BOOL AM_EXC219044661(AM_EXCEPT_EXPR);
BOOL AM_EXC30103808(AM_EXCEPT_EXPR, OB);
BOOL AM_EXT1010597579(AM_EXT_CALL_EXPR);
BOOL AM_EXT1489112584(AM_EXT_CALL_EXPR, OB);
BOOL AM_FAR1472604699(AM_FAR_EXPR, OB);
BOOL AM_FAR833547686(AM_FAR_EXPR);
BOOL AM_FLT1011996250(AM_FLTI_CONST, OB);
BOOL AM_FLT1108482895(AM_FLTX_CONST, OB);
BOOL AM_FLT1144488899(AM_FLTD_CONST, OB);
BOOL AM_FLT1356295273(AM_FLT_CONST);
BOOL AM_FLT1602735735(AM_FLTDX_CONST, OB);
BOOL AM_FLT1739286436(AM_FLT_CONST, OB);
BOOL AM_FLT1865244436(AM_FLTDX_CONST);
BOOL AM_FLT652620710(AM_FLTX_CONST);
BOOL AM_FLT840430235(AM_FLTI_CONST);
BOOL AM_FLT93541882(AM_FLTD_CONST);
BOOL AM_GLO1300832959(AM_GLOBAL_EXPR);
BOOL AM_GLO286344550(AM_GLOBAL_EXPR, OB);
BOOL AM_HER1213808373(AM_HERE_EXPR);
BOOL AM_HER1833766706(AM_HERE_EXPR, OB);
BOOL AM_IF_2781110(AM_IF_EXPR, OB);
BOOL AM_IF_792326817(AM_IF_EXPR);
BOOL AM_INT1418540518(AM_INT_CONST);
BOOL AM_INT1567340277(AM_INT_CONST, OB);
BOOL AM_INT341842242(AM_INTI_CONST);
BOOL AM_INT586605643(AM_INTI_CONST, OB);
BOOL AM_IS_1001428606(AM_IS_VOID_EXPR);
BOOL AM_IS_997133203(AM_IS_VOID_EXPR, OB);
BOOL AM_ITE1429510983(AM_ITE1809135850);
BOOL AM_ITE1901971608(AM_ITE1809135850, OB);
BOOL AM_LOC2055616863(AM_LOCAL_EXPR);
BOOL AM_LOC783953200(AM_LOCAL_EXPR, OB);
BOOL AM_NEA1492883953(AM_NEAR_EXPR);
BOOL AM_NEA499164018(AM_NEAR_EXPR, OB);
BOOL AM_NEW1485150288(AM_NEW_EXPR, OB);
BOOL AM_NEW952487097(AM_NEW_EXPR);
BOOL AM_ROU2096608001(AM_ROU1916046290);
BOOL AM_ROU774362080(AM_ROU1916046290, OB);
BOOL AM_SHA1922018909(AM_SHARED_EXPR);
BOOL AM_SHA623484008(AM_SHARED_EXPR, OB);
BOOL AM_STM214668158(AM_STMT_EXPR, OB);
BOOL AM_STM552683579(AM_STMT_EXPR);
BOOL AM_STR1095405116(AM_STR_CONST);
BOOL AM_STR2039863889(AM_STR_CONST, OB);
BOOL AM_VAR1818998713(AM_VAR744470097, OB);
BOOL AM_VAR394665606(AM_VAR744470097);
BOOL AM_VAT2140838301(AM_VAT319982528, OB);
BOOL AM_VAT239070856(AM_VAT319982528);
BOOL AM_VOI1214668736(AM_VOID_CONST);
BOOL AM_VOI2144357369(AM_VOID_CONST, OB);
BOOL AM_WHE332796028(AM_WHERE_EXPR, OB);
BOOL AM_WHE420295477(AM_WHERE_EXPR);
BOOL ARRAY21272457581(ARRAY21272457581_frame);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL AS_ASS1377838817(AS_ASSERT_STMT);
BOOL AS_ASS1755466016(AS_ASSIGN_STMT);
BOOL AS_ATT1182933590(AS_ATTACH_STMT);
BOOL AS_ATT2034251932(AS_ATTR_DEF);
BOOL AS_CAS964680792(AS_CASE_STMT);
BOOL AS_CON1915749157(AS_CONST_DEF);
BOOL AS_DEC2043607416(AS_DEC_STMT);
BOOL AS_EXP1274701909(AS_EXPR_STMT);
BOOL AS_FOR1720691150(AS_FORK_STMT);
BOOL AS_IF_117934348(AS_IF_STMT);
BOOL AS_INC827145130(AS_INC1416917001);
BOOL AS_INT130358855(AS_INT2031239268);
BOOL AS_LOC802751445(AS_LOCK_STMT);
BOOL AS_LOO612242568(AS_LOOP_STMT);
BOOL AS_PAR818790093(AS_PAR_STMT);
BOOL AS_PRO122180587(AS_PROTECT_STMT);
BOOL AS_QUI1489742669(AS_QUIT_STMT);
BOOL AS_RAI201810971(AS_RAISE_STMT);
BOOL AS_RET863344353(AS_RETURN_STMT);
BOOL AS_ROU1543294757(AS_ROUT_DEF);
BOOL AS_SHA952781759(AS_SHARED_DEF);
BOOL AS_STM1645255095(AS_STMT);
BOOL AS_SYN1201341531(AS_SYNC_STMT);
BOOL AS_TYP1265860748(AS_TYPECASE_STMT);
BOOL AS_UNL1126435291(AS_UNLOCK_STMT);
BOOL AS_WIT1566004798(AS_WIT1063437351);
BOOL AS_YIE1515612652(AS_YIELD_STMT);
BOOL ATTRSO1207195602(ATTRSORT, OB);
BOOL BOOL_i1290536107(BOOL, OB);
BOOL CALL_T148273370(CALL_T561792367, dTP);
BOOL CALL_T16429326(CALL_TP_ARRAY, dTP);
BOOL CALL_T1680302827(CALL_TP_VOID, dTP);
BOOL CALL_T1822200530(CALL_T666389079, dTP);
BOOL CALL_T640058966(CALL_TP_CREATE, dTP);
BOOL CHAR_i1237803373(CHAR, OB);
BOOL CSE_DO4156401(CSE_DOUBLE_EXPRS, OB);
BOOL FLISTB1880887881(FLISTB1880887881_frame);
BOOL FLTD_i1285721587(FLTD, OB);
BOOL FLTD_is_nilrBOOL(FLTD);
BOOL FLT_is1351417193(FLT, OB);
BOOL FLT_is_nilrBOOL(FLT);
BOOL FSTR_i135169312(FSTR, OB);
BOOL IDENT_1780939044(IDENT, OB);
BOOL INLINE1554137872(INLINE_ROUT);
BOOL INLINE1676073295(INLINE_ATTR_READ);
BOOL INLINE1738748633(INLINE1164904870);
BOOL INLINE1799378917(INLINE_INT_FOLD);
BOOL INLINE182457744(INLINE1596311811);
BOOL INLINE182665064(INLINE_ITER);
BOOL INLINE600822962(INLINE1433668901);
BOOL INLINE76438420(INLINE, SIG);
BOOL INLINE862622371(INLINE1455768374);
BOOL INOUT_1534195007(INOUT_MODE);
BOOL INOUT_394279242(INOUT_MODE, dMODE);
BOOL INTI_i1060921131(INTI1, OB);
BOOL INT_is240310584(INT, OB);
BOOL INT_is_nilrBOOL(INT);
BOOL IN_MOD1604254699(IN_MODE, dMODE);
BOOL IN_MOD851634165(IN_MODE);
BOOL ONCE_M1658631798(ONCE_MODE, dMODE);
BOOL ONCE_M1884799084(ONCE_MODE);
BOOL OUT_MO1200475098(OUT_MODE, dMODE);
BOOL OUT_MO898624407(OUT_MODE);
BOOL RAT_is1975945358(RAT, OB);
BOOL SFILE_947438414(SFILE_ID, OB);
BOOL SIDE_E1243435218(SIDE_EFFECT, OB);
BOOL SIG_is688012902(SIG, OB);
BOOL STR_is142167618(STR, OB);
BOOL TP_CLA1015994406(TP_CLASS);
BOOL TP_CLA1071798668(TP_CLASS);
BOOL TP_CLA1153639400(TP_CLASS);
BOOL TP_CLA1448883828(TP_CLASS);
BOOL TP_CLA1568240525(TP_CLASS);
BOOL TP_CLA1792166754(TP_CLASS);
BOOL TP_CLA254882760(TP_CLASS, OB);
BOOL TP_CLA272820496(TP_CLASS);
BOOL TP_CLA366982046(TP_CLASS);
BOOL TP_CLA430625769(TP_CLASS);
BOOL TP_CLA513015767(TP_CLASS, dCALL_TP);
BOOL TP_CLA650455767(TP_CLASS);
BOOL TP_CLA693899249(TP_CLASS, dCALL_TP);
BOOL TP_CLA727826289(TP_CLASS);
BOOL TP_ITE1212470094(TP_ITER);
BOOL TP_ITE1387653699(TP_ITER);
BOOL TP_ITE1549466955(TP_ITER);
BOOL TP_ITE1550467217(TP_ITER, dCALL_TP);
BOOL TP_ITE1606424066(TP_ITER);
BOOL TP_ITE1864681437(TP_ITER);
BOOL TP_ITE1879811989(TP_ITER);
BOOL TP_ITE1906602522(TP_ITER, dCALL_TP);
BOOL TP_ITE1921021286(TP_ITER, OB);
BOOL TP_ITE2052650588(TP_ITER);
BOOL TP_ITE2096764295(TP_ITER);
BOOL TP_ITE532005347(TP_ITER);
BOOL TP_ITE547568872(TP_ITER);
BOOL TP_ITE910939601(TP_ITER);
BOOL TP_ROU1058813051(TP_ROUT);
BOOL TP_ROU1099474123(TP_ROUT);
BOOL TP_ROU1105559654(TP_ROUT);
BOOL TP_ROU1673918401(TP_ROUT);
BOOL TP_ROU177994147(TP_ROUT);
BOOL TP_ROU1849661410(TP_ROUT, OB);
BOOL TP_ROU1919694918(TP_ROUT);
BOOL TP_ROU460533207(TP_ROUT);
BOOL TP_ROU681177780(TP_ROUT);
BOOL TP_ROU68166391(TP_ROUT, dCALL_TP);
BOOL TP_ROU699367341(TP_ROUT);
BOOL TP_ROU730462331(TP_ROUT);
BOOL TP_ROU835748448(TP_ROUT);
BOOL TP_ROU967846898(TP_ROUT, dCALL_TP);
BOOL TUPAM_1144039098(TUPAM_1024994801, OB);
BOOL TUPAM_1196406350(TUPAM_504653531, OB);
BOOL TUPAM_1557192709(TUPAM_1519040877, OB);
BOOL TUPAM_1711472971(TUPAM_653469574, OB);
BOOL TUPAM_1783991982(TUPAM_927437481, OB);
BOOL TUPAM_2013149714(TUPAM_304458649, OB);
BOOL TUPAM_2045709861(TUPAM_667892060, OB);
BOOL TUPARR1107471104(TUPARRAYARGdTP, OB);
BOOL TUPARR1838121920(TUPARR1759987776, OB);
BOOL TUPBOO323223880(TUPBOO1392923241, OB);
BOOL TUPIDE1222548283(TUPIDE641654124, OB);
BOOL TUPIDE1955445151(TUPIDENTARRAYdTP, OB);
BOOL TUPIDE429159832(TUPIDENTdTP, OB);
BOOL TUPIDE53463463(TUPIDENTINT, OB);
BOOL TUPINT326447217(TUPINTINT, OB);
BOOL TUPSIG116886773(TUPSIGSTR, OB);
BOOL TUPSIG1705966004(TUPSIG1754078736, OB);
BOOL TUPSIG174310016(TUPSIG2023533247, OB);
BOOL TUPSIG215029739(TUPSIGINT, OB);
BOOL TUPSIG662732057(TUPSIGSIG, OB);
BOOL TUPSIG738670088(TUPSIGARG_LAYOUT, OB);
BOOL TUPSIG848243115(TUPSIG578692189, OB);
BOOL TUPSTR1375565929(TUPSTRSTR, OB);
BOOL TUPSTR1473708895(TUPSTRINT, OB);
BOOL TUPSTR1795207828(TUPSTRdCONFIG, OB);
BOOL TUPSTR1921411213(TUPSTRSIG, OB);
BOOL TUPSTR850883487(TUPSTRFSETSTR, OB);
BOOL TUPTP_2145724346(TUPTP_858321267, OB);
BOOL TUPTP_744064396(TUPTP_1668454172, OB);
BOOL TUPdAM1479867(TUPdAM_EXPRdTP, OB);
BOOL TUPdAS1082207056(TUPdAS268679779, OB);
BOOL TUPdOB134201346(TUPdOBNAMESPACE, OB);
BOOL TUPdOB398929495(TUPdOBSTR, OB);
BOOL TUPdTP1282296289(TUPdTP1710233051, OB);
BOOL TUPdTP1433052028(TUPdTPINT, OB);
BOOL TUPdTP1932220599(TUPdTPdLAYOUT, OB);
BOOL TUPdTP1961471532(TUPdTPCODE_FILE, OB);
BOUND_2140373124 ARRAYB1475407144(ARRAYB1475407144_frame);
BOUND_2140373124 FLISTB455167321(FLISTB455167321_frame);
BOUND_2140373124 FSETBO677183779(FSETBO677183779_frame);
BOUND_809390774 ARRAYB1420960227(ARRAYB1420960227_frame);
BOUND_809390774 FLISTB905996620(FLISTB905996620_frame);
BOUND_809390774 FSETBO10998148(FSETBO10998148_frame);
BUILTIN_LAYOUT ARRAYB1615560602(ARRAYB1615560602_frame);
BUILTIN_LAYOUT FLISTB334826071(FLISTB334826071_frame);
BUILTIN_LAYOUT FSETBU202043707(FSETBU202043707_frame);
CALL_ARG ARRAYC494195798(ARRAYC494195798_frame);
CALL_ARG FLISTC264978409(FLISTC264978409_frame);
CHAR ARRAYC1494984744(ARRAYC1494984744_frame);
CHAR FLISTC1776759929(FLISTC1776759929_frame);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
CLASS_LAYOUT ARRAYC818595214(ARRAYC818595214_frame);
CLASS_LAYOUT FLISTC519862611(FLISTC519862611_frame);
CLASS_LAYOUT FSETCL1233705521(FSETCL1233705521_frame);
CODE_FILE ARRAYC1531429675(ARRAYC1531429675_frame);
CODE_FILE FLISTC1947519404(FLISTC1947519404_frame);
CSE_DOUBLE_EXPRS ARRAYC1440407068(ARRAYC1440407068_frame);
CSE_DOUBLE_EXPRS FLISTC1263372315(FLISTC1263372315_frame);
CSE_DOUBLE_EXPRS FSETCS1170733135(FSETCS1170733135_frame);
EXPR_HOISTED ARRAYE1693383532(ARRAYE1693383532_frame);
EXPR_HOISTED FLISTE354925707(FLISTE354925707_frame);
EXTERNAL_LAYOUT ARRAYE1687022731(ARRAYE1687022731_frame);
EXTERNAL_LAYOUT FLISTE1972070322(FLISTE1972070322_frame);
EXTERNAL_LAYOUT FSETEX1633103446(FSETEX1633103446_frame);
FLISTA725283029 AM_ANY1678367917(AM_ANY_EXPR);
FLISTA725283029 AM_ARR1270029884(AM_ARR_CONST);
FLISTA725283029 AM_ARR1917250979(AM_ARRAY_EXPR);
FLISTA725283029 AM_ATT2085324530(AM_ATTR_EXPR);
FLISTA725283029 AM_AT_726422231(AM_AT_EXPR);
FLISTA725283029 AM_BND1015009410(AM_BND260301329);
FLISTA725283029 AM_BND1086659687(AM_BND1124877163);
FLISTA725283029 AM_BND776534650(AM_BND367211769);
FLISTA725283029 AM_BOO1622702250(AM_BOOL_CONST);
FLISTA725283029 AM_CHA722016210(AM_CHAR_CONST);
FLISTA725283029 AM_CLU1812304013(AM_CLUSTER_EXPR);
FLISTA725283029 AM_CLU39726141(AM_CLU1286269335);
FLISTA725283029 AM_CON19269408(AM_CONST);
FLISTA725283029 AM_EXC1481209187(AM_EXCEPT_EXPR);
FLISTA725283029 AM_EXT908131195(AM_EXT_CALL_EXPR);
FLISTA725283029 AM_FAR276305130(AM_FAR_EXPR);
FLISTA725283029 AM_FLT1309796914(AM_FLTD_CONST);
FLISTA725283029 AM_FLT1728105754(AM_FLTX_CONST);
FLISTA725283029 AM_FLT1832811964(AM_FLTDX_CONST);
FLISTA725283029 AM_FLT523420577(AM_FLTI_CONST);
FLISTA725283029 AM_FLT945201335(AM_FLT_CONST);
FLISTA725283029 AM_GLO404411817(AM_GLOBAL_EXPR);
FLISTA725283029 AM_HER1483923803(AM_HERE_EXPR);
FLISTA725283029 AM_IF_1267236903(AM_IF_EXPR);
FLISTA725283029 AM_INT1223291058(AM_INT_CONST);
FLISTA725283029 AM_INT2023229226(AM_INTI_CONST);
FLISTA725283029 AM_IS_1111682258(AM_IS_VOID_EXPR);
FLISTA725283029 AM_ITE757218595(AM_ITE1809135850);
FLISTA725283029 AM_LOC1121251403(AM_LOCAL_EXPR);
FLISTA725283029 AM_NEA1654213825(AM_NEAR_EXPR);
FLISTA725283029 AM_NEW1679302875(AM_NEW_EXPR);
FLISTA725283029 AM_ROU518743835(AM_ROU1916046290);
FLISTA725283029 AM_SHA1496045189(AM_SHARED_EXPR);
FLISTA725283029 AM_STM1543836917(AM_STMT_EXPR);
FLISTA725283029 AM_STR201189492(AM_STR_CONST);
FLISTA725283029 AM_VAR567579326(AM_VAR744470097);
FLISTA725283029 AM_VAT467614272(AM_VAT319982528);
FLISTA725283029 AM_VOI402497764(AM_VOID_CONST);
FLISTA725283029 AM_WHE1419632471(AM_WHERE_EXPR);
FLISTSTR ARRAYF1006928322(ARRAYF1006928322_frame);
FLISTSTR FLISTF1766102529(FLISTF1766102529_frame);
FLISTdAM_CONST ARRAYF1657157762(ARRAYF1657157762_frame);
FLISTdAM_CONST FLISTF687422861(FLISTF687422861_frame);
FLISTdLAYOUT ABSTRA1868518641(ABSTRACT_LAYOUT);
FLISTdLAYOUT ABSTRA1904387652(ABSTRA151498155);
FLISTdLAYOUT ARG_LA836569915(ARG_LAYOUT);
FLISTdLAYOUT BOUND_1150883246(BOUND_1007407331);
FLISTdLAYOUT BOUND_1157561903(BOUND_2140373124);
FLISTdLAYOUT BOUND_1585387107(BOUND_1677815802);
FLISTdLAYOUT BOUND_220914967(BOUND_809390774);
FLISTdLAYOUT BUILTI1794609038(BUILTIN_LAYOUT);
FLISTdLAYOUT CLASS_1345471732(CLASS_LAYOUT);
FLISTdLAYOUT EXTERN1033411318(EXTERNAL_LAYOUT);
FLISTdLAYOUT FRAME_31536271(FRAME_LAYOUT);
FLISTdLAYOUT IMMUTA414538126(IMMUTA421759893);
FLISTdLAYOUT TP_LAY265125546(TP_LAYOUT);
FLT FLT_nilrFLT(FLT);
FLTD FLTD_nilrFLTD(FLTD);
FSETSIG GENERA1281179458(GENERATE_AM);
FSTR ARRAYF254484098(ARRAYF254484098_frame);
FSTR FLISTF1277706721(FLISTF1277706721_frame);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IDENT FLISTI1709766432(FLISTI1709766432_frame);
IFC ARRAYI2003604369(ARRAYI2003604369_frame);
IFC FLISTI1596257802(FLISTI1596257802_frame);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
IFC TP_ITER_ifcrIFC(TP_ITER);
IFC TP_ROUT_ifcrIFC(TP_ROUT);
IMMUTA421759893 ARRAYI1264630278(ARRAYI1264630278_frame);
IMMUTA421759893 FLISTI244390455(FLISTI244390455_frame);
IMMUTA421759893 FSETIM887960645(FSETIM887960645_frame);
IMPL TP_CLA1109727233(TP_CLASS);
IMPL TP_ITE830569150(TP_ITER);
IMPL TP_ROU723658710(TP_ROUT);
IMPL_INCLUDE ARRAYI1719886982(ARRAYI1719886982_frame);
IMPL_INCLUDE FLISTI381429157(FLISTI381429157_frame);
INT AM_ANY1965823461(AM_ANY_EXPR);
INT AM_ARR1424607794(AM_ARR_CONST);
INT AM_ARR191206921(AM_ARRAY_EXPR);
INT AM_ATT178000012(AM_ATTR_EXPR);
INT AM_AT_373259037(AM_AT_EXPR);
INT AM_BND1144165392(AM_BND260301329);
INT AM_BND2097777224(AM_BND367211769);
INT AM_BND472678797(AM_BND1124877163);
INT AM_BOO561475672(AM_BOOL_CONST);
INT AM_CHA1966863808(AM_CHAR_CONST);
INT AM_CLU772455521(AM_CLU1286269335);
INT AM_CLU978087499(AM_CLUSTER_EXPR);
INT AM_CON2051977914(AM_CONST);
INT AM_EXC800301049(AM_EXCEPT_EXPR);
INT AM_EXT644325801(AM_EXT_CALL_EXPR);
INT AM_FAR2123150608(AM_FAR_EXPR);
INT AM_FLT1054767436(AM_FLTX_CONST);
INT AM_FLT153804005(AM_FLTI_CONST);
INT AM_FLT465909163(AM_FLT_CONST);
INT AM_FLT556661152(AM_FLTD_CONST);
INT AM_FLT669520374(AM_FLTDX_CONST);
INT AM_GLO57248015(AM_GLOBAL_EXPR);
INT AM_HER774694957(AM_HERE_EXPR);
INT AM_IF_1573308145(AM_IF_EXPR);
INT AM_INT2057636940(AM_INT_CONST);
INT AM_INT331860696(AM_INTI_CONST);
INT AM_IS_128338200(AM_IS_VOID_EXPR);
INT AM_ITE1021507011(AM_ITE1809135850);
INT AM_LOC505223029(AM_LOCAL_EXPR);
INT AM_NEA1314295025(AM_NEAR_EXPR);
INT AM_NEW1014811437(AM_NEW_EXPR);
INT AM_ROU67895179(AM_ROU1916046290);
INT AM_SHA1840537425(AM_SHARED_EXPR);
INT AM_STM804699229(AM_STMT_EXPR);
INT AM_STR1959493974(AM_STR_CONST);
INT AM_VAR2089448132(AM_VAR744470097);
INT AM_VAT495261450(AM_VAT319982528);
INT AM_VOI822416442(AM_VOID_CONST);
INT AM_WHE1598364639(AM_WHERE_EXPR);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT AS_AND1958569903(AS_AND_EXPR);
INT AS_ANY1507835814(AS_ANY_EXPR);
INT AS_ARR1547513756(AS_ARRAY_EXPR);
INT AS_AT_355764446(AS_AT_EXPR);
INT AS_BOO711205340(AS_BOOL_LIT_EXPR);
INT AS_BOU1634505419(AS_BOU14269336);
INT AS_BRE836708546(AS_BREAK_EXPR);
INT AS_CAL755875642(AS_CALL_EXPR);
INT AS_CHA1908653268(AS_CHAR_LIT_EXPR);
INT AS_CLU1234120202(AS_CLUSTER_EXPR);
INT AS_CLU889538930(AS_CLU553570599);
INT AS_COH1949131218(AS_COHORT_EXPR);
INT AS_CRE1921695223(AS_CREATE_EXPR);
INT AS_EXC629960794(AS_EXCEPT_EXPR);
INT AS_FAR1350508667(AS_FAR_EXPR);
INT AS_FLT374280066(AS_FLT_LIT_EXPR);
INT AS_GLO113092240(AS_GLOBAL_EXPR);
INT AS_HER1196006492(AS_HERE_EXPR);
INT AS_INI414727320(AS_INITIAL_EXPR);
INT AS_INT228236077(AS_INT_LIT_EXPR);
INT AS_IS_1954421395(AS_IS_VOID_EXPR);
INT AS_NEA1735606560(AS_NEAR_EXPR);
INT AS_NEW1836119458(AS_NEW_EXPR);
INT AS_OR_1113601798(AS_OR_EXPR);
INT AS_RES32147922(AS_RESULT_EXPR);
INT AS_SEL1486205964(AS_SELF_EXPR);
INT AS_STR1375497755(AS_STR_LIT_EXPR);
INT AS_UND1736379304(AS_UND152986614);
INT AS_VOI1355443380(AS_VOID_EXPR);
INT AS_WHE140356038(AS_WHERE_EXPR);
INT CHAR_hashrINT(CHAR);
INT CSE_DO1342102696(CSE_DOUBLE_EXPRS);
INT FLISTI476396026(FLISTI476396026_frame);
INT FLTD_hashrINT(FLTD);
INT FLT_hashrINT(FLT);
INT FSTR_hashrINT(FSTR);
INT IDENT_hashrINT(IDENT);
INT INTI_hashrINT(INTI1);
INT INT_hashrINT(INT);
INT INT_nilrINT(INT);
INT SFILE_29532291(SFILE_ID);
INT SIDE_E484651583(SIDE_EFFECT);
INT SIG_hashrINT(SIG);
INT STR_hashrINT(STR);
INT TP_CLA1653846135(TP_CLASS);
INT TP_CLA619166775(TP_CLASS);
INT TP_ITER_hashrINT(TP_ITER);
INT TP_ITER_kindrINT(TP_ITER);
INT TP_ROUT_hashrINT(TP_ROUT);
INT TP_ROUT_kindrINT(TP_ROUT);
INT TUPAM_1208907315(TUPAM_927437481);
INT TUPAM_1395325321(TUPAM_504653531);
INT TUPAM_1877096499(TUPAM_304458649);
INT TUPAM_247029134(TUPAM_1519040877);
INT TUPAM_460226510(TUPAM_667892060);
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
OPT_LOCAL_EXPR ARRAYO335724112(ARRAYO335724112_frame);
OPT_LOCAL_EXPR FLISTO2008856511(FLISTO2008856511_frame);
PROG TP_CLA736641047(TP_CLASS);
PROG TP_ITE1618029866(TP_ITER);
PROG TP_ROU1724940306(TP_ROUT);
SFILE_ID AM_ANY312639639(AM_ANY_EXPR);
SFILE_ID AM_ARR490952057(AM_ARR_CONST);
SFILE_ID AM_ARR592984289(AM_ARRAY_EXPR);
SFILE_ID AM_ASS1066541977(AM_ASSIGN_STMT);
SFILE_ID AM_ASS142463220(AM_ASSERT_STMT);
SFILE_ID AM_ATT1483547331(AM_ATTR_EXPR);
SFILE_ID AM_ATT2128304053(AM_ATTACH_STMT);
SFILE_ID AM_AT_336435086(AM_AT_EXPR);
SFILE_ID AM_BND1008256842(AM_BND260301329);
SFILE_ID AM_BND1697524322(AM_BND367211769);
SFILE_ID AM_BND1800984858(AM_BND1124877163);
SFILE_ID AM_BOO1043294506(AM_BOOL_CONST);
SFILE_ID AM_BRE475880308(AM_BREAK_STMT);
SFILE_ID AM_CAL1587798028(AM_CALL_ARG);
SFILE_ID AM_CAS881290947(AM_CASE_STMT);
SFILE_ID AM_CHA1773602482(AM_CHAR_CONST);
SFILE_ID AM_CLU1154173259(AM_CLUSTER_EXPR);
SFILE_ID AM_CLU2049939620(AM_CLU1286269335);
SFILE_ID AM_COM1046808385(AM_COMMENT_STMT);
SFILE_ID AM_CON810758495(AM_CONST);
SFILE_ID AM_EXC2040160518(AM_EXCEPT_EXPR);
SFILE_ID AM_EXP2073687788(AM_EXPR_STMT);
SFILE_ID AM_EXT840500856(AM_EXT_CALL_EXPR);
SFILE_ID AM_FAR526851136(AM_FAR_EXPR);
SFILE_ID AM_FLT1298836824(AM_FLT_CONST);
SFILE_ID AM_FLT1412381806(AM_FLTD_CONST);
SFILE_ID AM_FLT2070042319(AM_FLTDX_CONST);
SFILE_ID AM_FLT2096495882(AM_FLTX_CONST);
SFILE_ID AM_FLT564073323(AM_FLTI_CONST);
SFILE_ID AM_FOR1474728133(AM_FORK_STMT);
SFILE_ID AM_FOR97554330(AM_FORMAL_ARG);
SFILE_ID AM_GLO537255272(AM_GLOBAL_EXPR);
SFILE_ID AM_HER1201005342(AM_HERE_EXPR);
SFILE_ID AM_IF_1192492569(AM_IF_STMT);
SFILE_ID AM_IF_1629993260(AM_IF_EXPR);
SFILE_ID AM_INI1792787320(AM_INITIAL_STMT);
SFILE_ID AM_INT1397528579(AM_INT_CONST);
SFILE_ID AM_INT872738218(AM_INTI_CONST);
SFILE_ID AM_INV963567817(AM_INV1167837230);
SFILE_ID AM_IS_82827228(AM_IS_VOID_EXPR);
SFILE_ID AM_ITE834410629(AM_ITE1809135850);
SFILE_ID AM_LOC327374505(AM_LOCAL_EXPR);
SFILE_ID AM_LOC748262786(AM_LOCK_STMT);
SFILE_ID AM_LOO738236003(AM_LOOP_STMT);
SFILE_ID AM_NEA1717951158(AM_NEAR_EXPR);
SFILE_ID AM_NEW765217327(AM_NEW_EXPR);
SFILE_ID AM_OB_1633498909(AM_OB_DEF);
SFILE_ID AM_PAR1239764599(AM_PAR_STMT);
SFILE_ID AM_POS2123304705(AM_POST_STMT);
SFILE_ID AM_PRE1797857661(AM_PRE_STMT);
SFILE_ID AM_PRE2094346420(AM_PREFETCH_STMT);
SFILE_ID AM_PRO1670847403(AM_PROTECT_STMT);
SFILE_ID AM_RAI1393475571(AM_RAISE_STMT);
SFILE_ID AM_RET702658942(AM_RETURN_STMT);
SFILE_ID AM_ROU145143149(AM_ROU1916046290);
SFILE_ID AM_ROU231894201(AM_ROUT_DEF);
SFILE_ID AM_SHA644128690(AM_SHARED_EXPR);
SFILE_ID AM_STM1466562094(AM_STMT_EXPR);
SFILE_ID AM_STR1580126161(AM_STR_CONST);
SFILE_ID AM_SYN416733298(AM_SYNC_STMT);
SFILE_ID AM_TYP720211797(AM_TYPECASE_STMT);
SFILE_ID AM_UNL1937638738(AM_UNLOCK_STMT);
SFILE_ID AM_VAR1884071700(AM_VAR744470097);
SFILE_ID AM_VAT338387515(AM_VAT319982528);
SFILE_ID AM_VOI405249940(AM_VOID_CONST);
SFILE_ID AM_WAI442200356(AM_WAITFOR_STMT);
SFILE_ID AM_WHE345322829(AM_WHERE_EXPR);
SFILE_ID AM_WIT1120935944(AM_WIT1996971603);
SFILE_ID AM_YIE1303084854(AM_YIELD_STMT);
SFILE_ID ARRAYS1372094358(ARRAYS1372094358_frame);
SFILE_ID AS_AND749298802(AS_AND_EXPR);
SFILE_ID AS_ANY2075595295(AS_ANY_EXPR);
SFILE_ID AS_ARG265670232(AS_ARG_DEC);
SFILE_ID AS_ARR542631033(AS_ARRAY_EXPR);
SFILE_ID AS_ASS40608582(AS_ASSERT_STMT);
SFILE_ID AS_ASS964687339(AS_ASSIGN_STMT);
SFILE_ID AS_ATT1647669388(AS_ATTR_DEF);
SFILE_ID AS_ATT2064808605(AS_ATTACH_STMT);
SFILE_ID AS_AT_1597668772(AS_AT_EXPR);
SFILE_ID AS_BOO291537060(AS_BOOL_LIT_EXPR);
SFILE_ID AS_BOU1793757293(AS_BOU14269336);
SFILE_ID AS_BRE1173994939(AS_BREAK_EXPR);
SFILE_ID AS_CAL82770762(AS_CALL_EXPR);
SFILE_ID AS_CAS1215719733(AS_CASE_WHEN);
SFILE_ID AS_CAS986885563(AS_CASE_STMT);
SFILE_ID AS_CHA1542077308(AS_CHAR_LIT_EXPR);
SFILE_ID AS_CLA374248818(AS_CLASS_DEF);
SFILE_ID AS_CLU1205555915(AS_CLUSTER_EXPR);
SFILE_ID AS_CLU243953950(AS_CLU553570599);
SFILE_ID AS_COH931029852(AS_COHORT_EXPR);
SFILE_ID AS_CON1302703181(AS_CONST_DEF);
SFILE_ID AS_CRE1835137079(AS_CREATE_EXPR);
SFILE_ID AS_DEC1054605514(AS_DEC_STMT);
SFILE_ID AS_EXC2142015156(AS_EXCEPT_EXPR);
SFILE_ID AS_EXP353102998(AS_EXPR_STMT);
SFILE_ID AS_FAR1861383798(AS_FAR_EXPR);
SFILE_ID AS_FEA630208390(AS_FEAT_MOD);
SFILE_ID AS_FLT235098865(AS_FLT_LIT_EXPR);
SFILE_ID AS_FOR393448377(AS_FORK_STMT);
SFILE_ID AS_GLO435400634(AS_GLOBAL_EXPR);
SFILE_ID AS_HER667171168(AS_HERE_EXPR);
SFILE_ID AS_IDE48340397(AS_IDENT_LIST);
SFILE_ID AS_IF_1168370869(AS_IF_STMT);
SFILE_ID AS_INC214099154(AS_INC1416917001);
SFILE_ID AS_INI295049889(AS_INITIAL_EXPR);
SFILE_ID AS_INT1381353727(AS_INT2031239268);
SFILE_ID AS_INT6023736(AS_INT_LIT_EXPR);
SFILE_ID AS_IS_2018065350(AS_IS_VOID_EXPR);
SFILE_ID AS_LOC1119913724(AS_LOCK_STMT);
SFILE_ID AS_LOC1864862121(AS_LOCK_IF_WHEN);
SFILE_ID AS_LOO1129940507(AS_LOOP_STMT);
SFILE_ID AS_NEA708839628(AS_NEAR_EXPR);
SFILE_ID AS_NEW1623017607(AS_NEW_EXPR);
SFILE_ID AS_OR_1363025812(AS_OR_EXPR);
SFILE_ID AS_PAR158230952(AS_PARAM_DEC);
SFILE_ID AS_PAR666967763(AS_PAR_STMT);
SFILE_ID AS_PRO264390719(AS_PROTECT_STMT);
SFILE_ID AS_PRO493224889(AS_PROTECT_WHEN);
SFILE_ID AS_QUI1176752812(AS_QUIT_STMT);
SFILE_ID AS_RAI1765876403(AS_RAISE_STMT);
SFILE_ID AS_RES951018660(AS_RESULT_EXPR);
SFILE_ID AS_RET600804304(AS_RETURN_STMT);
SFILE_ID AS_ROU2138626563(AS_ROUT_DEF);
SFILE_ID AS_SEL957265064(AS_SELF_EXPR);
SFILE_ID AS_SHA1565827735(AS_SHARED_DEF);
SFILE_ID AS_STM1237803845(AS_STMT);
SFILE_ID AS_STM633666973(AS_STMT_LIST);
SFILE_ID AS_STR1687611758(AS_STR_LIT_EXPR);
SFILE_ID AS_SYN1451443212(AS_SYNC_STMT);
SFILE_ID AS_TYP1164969549(AS_TYPECASE_STMT);
SFILE_ID AS_TYP1615798168(AS_TYPE_SPEC);
SFILE_ID AS_TYP936135379(AS_TYPECASE_WHEN);
SFILE_ID AS_UND1334198639(AS_UND152986614);
SFILE_ID AS_UNL1835784100(AS_UNLOCK_STMT);
SFILE_ID AS_VOI1131745048(AS_VOID_EXPR);
SFILE_ID AS_WHE1480938151(AS_WHERE_EXPR);
SFILE_ID AS_WIT337771262(AS_WIT1063437351);
SFILE_ID AS_YIE1856267120(AS_YIELD_STMT);
SFILE_ID FLISTS2131268565(FLISTS2131268565_frame);
SFILE_ID FSETSF139710295(FSETSF139710295_frame);
SIDE_EFFECT ARRAYS2023743485(ARRAYS2023743485_frame);
SIDE_EFFECT FLISTS1351415114(FLISTS1351415114_frame);
SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame);
SIG ARRAYS1481566101(ARRAYS1481566101_frame);
SIG FLISTS1074219534(FLISTS1074219534_frame);
SIG FSETSIG_eltbrSIG(FSETSIG_eltbrSIG_frame);
SIG GET_MA2025156715(GET_MAIN_SIG);
SIG INLINE1060094617(INLINE_ATTR_READ);
SIG INLINE1199394416(INLINE_ITER);
SIG INLINE1204485066(INLINE1433668901);
SIG INLINE1283359029(INLINE1455768374);
SIG INLINE1334459112(INLINE_ROUT);
SIG INLINE1360700008(INLINE1596311811);
SIG INLINE1848276099(INLINE_INT_FOLD);
SIG INLINE1854659937(INLINE1164904870);
STR ABSTRA12614027(ABSTRA151498155, CGEN);
STR ABSTRA1464296195(ABSTRACT_LAYOUT);
STR ABSTRA1984535400(ABSTRA151498155);
STR ABSTRA353314648(ABSTRACT_LAYOUT, CGEN);
STR ARG_LA635972259(ARG_LAYOUT);
STR ARG_LA807737867(ARG_LAYOUT, CGEN);
STR ARRAYA1329352756(ARRAYARRAYSTR);
STR ARRAYA1368915033(ARRAYAM_ROUT_DEF);
STR ARRAYA1490975097(ARRAYA1870808380);
STR ARRAYA1523920654(ARRAYA477251019);
STR ARRAYA1607624311(ARRAYATTRSORT);
STR ARRAYA1621913879(ARRAYA855692714);
STR ARRAYA175904446(ARRAYA1347292543);
STR ARRAYA1826145484(ARRAYAM_CALL_ARG);
STR ARRAYA1836477836(ARRAYAM_CURSOR);
STR ARRAYA208544369(ARRAYA1369744492);
STR ARRAYA449689863(ARRAYA1135548710);
STR ARRAYA56089049(ARRAYA36280886);
STR ARRAYA565856506(ARRAYAS_FEAT_MOD);
STR ARRAYA704584979(ARRAYA1575632848);
STR ARRAYA881422470(ARRAYARRAYBOOL);
STR ARRAYA92706474(ARRAYA132553625);
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
STR ARRAYT103792371(ARRAYT1759168384);
STR ARRAYT1054394674(ARRAYT1968212795);
STR ARRAYT1238370919(ARRAYTUPIDENTdTP);
STR ARRAYT1278969222(ARRAYT1562141251);
STR ARRAYT1355045210(ARRAYTUPIDENTINT);
STR ARRAYT1384853867(ARRAYT348931282);
STR ARRAYT1450640385(ARRAYT1153101058);
STR ARRAYT1561971079(ARRAYT1127505716);
STR ARRAYT1574849761(ARRAYT61112132);
STR ARRAYT1589477172(ARRAYTP_LAYOUT);
STR ARRAYT160571351(ARRAYT1641775270);
STR ARRAYT1626625951(ARRAYT546647214);
STR ARRAYT1658404076(ARRAYTUPSTRINT);
STR ARRAYT1671874055(ARRAYT1744744076);
STR ARRAYT1718290516(ARRAYTUPdOBSTR);
STR ARRAYT1733874469(ARRAYTP_CLASS);
STR ARRAYT1803904833(ARRAYT2065045042);
STR ARRAYT1810565556(ARRAYT1753333425);
STR ARRAYT1846205279(ARRAYT747572380);
STR ARRAYT189456178(ARRAYTUPSIGSTR);
STR ARRAYT1926215796(ARRAYT1146239647);
STR ARRAYT1942593259(ARRAYT1754348798);
STR ARRAYT2122676784(ARRAYT378612147);
STR ARRAYT31468026(ARRAYTUPSIGSIG);
STR ARRAYT568533288(ARRAYT263953675);
STR ARRAYT585031862(ARRAYT537394765);
STR ARRAYT614884074(ARRAYTUPSTRSTR);
STR ARRAYT835808278(ARRAYTUPSTRSIG);
STR ARRAYT835974574(ARRAYT1418209173);
STR ARRAYT854063824(ARRAYTUPSIGINT);
STR ARRAYT856660275(ARRAYTUPdTPINT);
STR ARRAYd1085727763(ARRAYdAM_STMT);
STR ARRAYd2023686048(ARRAYdAM_EXPR);
STR ARRAYd2065245377(ARRAYdAM_CONST);
STR ARRAYd946747660(ARRAYdLAYOUT);
STR ARRAYdTP_strrSTR(ARRAYdTP);
STR A_STAC1193125327(A_STAC1244966098);
STR BOOL_strrSTR(BOOL);
STR BOUND_1245789017(BOUND_2140373124);
STR BOUND_1401874693(BOUND_1007407331, CGEN);
STR BOUND_1537967790(BOUND_809390774, CGEN);
STR BOUND_1707602440(BOUND_1677815802, CGEN);
STR BOUND_1856529942(BOUND_1007407331);
STR BOUND_365396347(BOUND_2140373124, CGEN);
STR BOUND_619031357(BOUND_809390774);
STR BOUND_787102169(BOUND_1677815802);
STR BUILTI1940169806(BUILTIN_LAYOUT);
STR BUILTI35892888(BUILTIN_LAYOUT, CGEN);
STR CALL_T152540990(CALL_TP_ARRAY);
STR CALL_T1577548690(CALL_TP_CREATE);
STR CALL_T1666519150(CALL_T561792367);
STR CALL_T184446387(CALL_TP_VOID);
STR CALL_T47412966(CALL_T666389079);
STR CHAR_strrSTR(CHAR);
STR CLASS_1824950980(CLASS_LAYOUT);
STR CLASS_954167052(CLASS_LAYOUT, CGEN);
STR CONFIG1175493142(CONFIG_ROUT);
STR CONFIG1221082662(CONFIG_DEF);
STR CONFIG155608225(CONFIG_TBL);
STR CONFIG336048123(CONFIG_ROUT);
STR CONFIG811280218(CONFIG_DEF);
STR CS_OPT565500779(CS_OPTIONS);
STR ELT_TBL_strrSTR(ELT_TBL);
STR EXTERN1283013626(EXTERNAL_LAYOUT);
STR EXTERN1472641871(EXTERNAL_LAYOUT, CGEN);
STR FLISTA1208864070(FLISTA112326087);
STR FLISTA1228646807(FLISTA1327600838);
STR FLISTA1264584154(FLISTA162798263);
STR FLISTA128466217(FLISTAS_FEAT_MOD);
STR FLISTA1345532203(FLISTARRAYSTR);
STR FLISTA1366495389(FLISTA2097324934);
STR FLISTA1598488884(FLISTA886630705);
STR FLISTA1623803758(FLISTATTRSORT);
STR FLISTA1774499089(FLISTAM_CALL_ARG);
STR FLISTA1884067430(FLISTA1062334999);
STR FLISTA2047982482(FLISTA725283029);
STR FLISTA2143887329(FLISTAM_CURSOR);
STR FLISTA389202533(FLISTA1409846210);
STR FLISTA574012977(FLISTARRAYBOOL);
STR FLISTA674592310(FLISTAM_ROUT_DEF);
STR FLISTA902886330(FLISTA1893461511);
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
STR FLISTSTR_strrSTR(FLISTSTR);
STR FLISTT1090491199(FLISTT844804258);
STR FLISTT1312754552(FLISTT1075494997);
STR FLISTT1350994583(FLISTTUPSTRINT);
STR FLISTT1410881023(FLISTTUPdOBSTR);
STR FLISTT1451978730(FLISTT31683607);
STR FLISTT1518857242(FLISTT1145236345);
STR FLISTT1532344775(FLISTT1683165204);
STR FLISTT158032289(FLISTT1858637310);
STR FLISTT1750053916(FLISTTP_CLASS);
STR FLISTT1848908808(FLISTT406286251);
STR FLISTT1896886665(FLISTTP_LAYOUT);
STR FLISTT1906991113(FLISTT1023127882);
STR FLISTT1916592887(FLISTT567859354);
STR FLISTT208196008(FLISTT418307355);
STR FLISTT2098083178(FLISTT1311334615);
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
STR FLISTT709853181(FLISTT1661203254);
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
STR FMAPAM118642384(FMAPAM1787628573);
STR FMAPAM1201551978(FMAPAM1357596931);
STR FMAPAM1620534738(FMAPAM1145008811);
STR FMAPAM459426429(FMAPAM1244483504);
STR FMAPAM905477833(FMAPAM418009930);
STR FMAPID1860739187(FMAPIDENTdTP);
STR FMAPID1895267434(FMAPID739086519);
STR FMAPSI1424048021(FMAPSI1349988702);
STR FMAPSI1996405359(FMAPSI2103621588);
STR FMAPSI2071674985(FMAPSI1121373188);
STR FMAPSI866101256(FMAPSIGSTR);
STR FMAPSI99914640(FMAPSI518162669);
STR FMAPST1581005786(FMAPSTRINT);
STR FMAPST1670441508(FMAPSTRSTR);
STR FMAPST1891365712(FMAPSTRSIG);
STR FMAPST35426998(FMAPSTRFSETSTR);
STR FMAPST512400025(FMAPSTRdCONFIG);
STR FMAPTP1432889353(FMAPTP291739594);
STR FMAPTP722426429(FMAPTP775297600);
STR FMAPdO570867591(FMAPdOBNAMESPACE);
STR FMAPdT1371742936(FMAPdTPdLAYOUT);
STR FMAPdT1912217709(FMAPdTPINT);
STR FMAPdT2142836224(FMAPdT953816637);
STR FMAPdT966432485(FMAPdTPCODE_FILE);
STR FMT_ER330399541(FMT_ERROR);
STR FRAME_1195781105(FRAME_LAYOUT);
STR FRAME_691068581(FRAME_LAYOUT, CGEN);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR FSTR_strrSTR(FSTR);
STR IFC_strrSTR(IFC);
STR IMMUTA2093463840(IMMUTA421759893, CGEN);
STR IMMUTA966323602(IMMUTA421759893);
STR INOUT_1907813337(INOUT_MODE);
STR INTI_strrSTR(INTI1);
STR INT_strrSTR(INT);
STR IN_MODE_strrSTR(IN_MODE);
STR NULL_S1933820575(NULL_STAT);
STR ONCE_M1994645546(ONCE_MODE);
STR OUT_MODE_strrSTR(OUT_MODE);
STR RAT_strrSTR(RAT);
STR SFILE_ID_strrSTR(SFILE_ID);
STR SIG_TBL_strrSTR(SIG_TBL);
STR SIG_strrSTR(SIG);
STR STAT_strrSTR(STAT1);
STR STR_strrSTR(STR);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_LAY473390218(TP_LAYOUT);
STR TP_LAY690809295(TP_LAYOUT, CGEN);
STR TP_ROUT_strrSTR(TP_ROUT);
STR TUPAM_1128362628(TUPAM_304458649);
STR TUPAM_1694719258(TUPAM_1519040877);
STR TUPAM_489161612(TUPAM_927437481);
STR TUPAM_504028066(TUPAM_504653531);
STR TUPAM_525468809(TUPAM_653469574);
STR TUPAM_827739609(TUPAM_667892060);
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
TP_CLASS ARRAYT1851819274(ARRAYT1851819274_frame);
TP_CLASS FLISTT1683973815(FLISTT1683973815_frame);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TP_LAYOUT ARRAYT1372201847(ARRAYT1372201847_frame);
TP_LAYOUT FLISTT2106747232(FLISTT2106747232_frame);
TUPAM_1024994801 ARRAYT1758179858(ARRAYT1758179858_frame);
TUPAM_1024994801 FLISTT234903853(FLISTT234903853_frame);
TUPAM_1519040877 ARRAYT684852235(ARRAYT684852235_frame);
TUPAM_1519040877 FLISTT344492076(FLISTT344492076_frame);
TUPAM_304458649 ARRAYT310960754(ARRAYT310960754_frame);
TUPAM_304458649 FLISTT726119853(FLISTT726119853_frame);
TUPAM_504653531 ARRAYT342187846(ARRAYT342187846_frame);
TUPAM_504653531 FLISTT1883920969(FLISTT1883920969_frame);
TUPAM_653469574 ARRAYT1101967660(ARRAYT1101967660_frame);
TUPAM_653469574 FLISTT288887925(FLISTT288887925_frame);
TUPAM_667892060 ARRAYT2108331328(ARRAYT2108331328_frame);
TUPAM_667892060 FLISTT193552257(FLISTT193552257_frame);
TUPAM_927437481 ARRAYT1360634798(ARRAYT1360634798_frame);
TUPAM_927437481 FLISTT709128269(FLISTT709128269_frame);
TUPIDE641654124 ARRAYT451883912(ARRAYT451883912_frame);
TUPIDE641654124 FLISTT1472123735(FLISTT1472123735_frame);
TUPIDENTINT ARRAYT1017446695(ARRAYT1017446695_frame);
TUPIDENTINT FLISTT97637998(FLISTT97637998_frame);
TUPIDENTINT FSETTU947212086(FSETTU947212086_frame);
TUPIDENTdTP ARRAYT849390229(ARRAYT849390229_frame);
TUPIDENTdTP FLISTT1769198926(FLISTT1769198926_frame);
TUPSIG1754078736 ARRAYT1079066005(ARRAYT1079066005_frame);
TUPSIG1754078736 FLISTT287332146(FLISTT287332146_frame);
TUPSIG2023533247 ARRAYT532396130(ARRAYT532396130_frame);
TUPSIG2023533247 FLISTT685872269(FLISTT685872269_frame);
TUPSIG578692189 ARRAYT1269605207(ARRAYT1269605207_frame);
TUPSIG578692189 FLISTT754641600(FLISTT754641600_frame);
TUPSIGARG_LAYOUT ARRAYT457901414(ARRAYT457901414_frame);
TUPSIGARG_LAYOUT FLISTT634936167(FLISTT634936167_frame);
TUPSIGINT ARRAYT1593820953(ARRAYT1593820953_frame);
TUPSIGINT FLISTT777802736(FLISTT777802736_frame);
TUPSIGSIG ARRAYT1146052097(ARRAYT1146052097_frame);
TUPSIGSIG FLISTT330033880(FLISTT330033880_frame);
TUPSIGSIG FSETTU845419336(FSETTU845419336_frame);
TUPSIGSTR ARRAYT1691893201(ARRAYT1691893201_frame);
TUPSIGSTR FLISTT875874984(FLISTT875874984_frame);
TUPSTRFSETSTR ARRAYT1426073269(ARRAYT1426073269_frame);
TUPSTRFSETSTR FLISTT720864404(FLISTT720864404_frame);
TUPSTRINT ARRAYT306471177(ARRAYT306471177_frame);
TUPSTRINT FLISTT509547040(FLISTT509547040_frame);
TUPSTRSIG ARRAYT141297679(ARRAYT141297679_frame);
TUPSTRSIG FLISTT957315896(FLISTT957315896_frame);
TUPSTRSTR ARRAYT404543425(ARRAYT404543425_frame);
TUPSTRSTR FLISTT411474792(FLISTT411474792_frame);
TUPSTRdCONFIG ARRAYT485937919(ARRAYT485937919_frame);
TUPSTRdCONFIG FLISTT1662091704(FLISTT1662091704_frame);
TUPTP_1668454172 ARRAYT1509169201(ARRAYT1509169201_frame);
TUPTP_1668454172 FLISTT1725186742(FLISTT1725186742_frame);
TUPTP_858321267 ARRAYT518925702(ARRAYT518925702_frame);
TUPTP_858321267 FLISTT699342697(FLISTT699342697_frame);
TUPdOBNAMESPACE ARRAYT681600609(ARRAYT681600609_frame);
TUPdOBNAMESPACE FLISTT396553018(FLISTT396553018_frame);
TUPdOBSTR ARRAYT595865513(ARRAYT595865513_frame);
TUPdOBSTR FLISTT220152704(FLISTT220152704_frame);
TUPdTP1710233051 ARRAYT1505580189(ARRAYT1505580189_frame);
TUPdTP1710233051 FLISTT1760042796(FLISTT1760042796_frame);
TUPdTPCODE_FILE ARRAYT1283478039(ARRAYT1283478039_frame);
TUPdTPCODE_FILE FLISTT1568525630(FLISTT1568525630_frame);
TUPdTPINT ARRAYT452391291(ARRAYT452391291_frame);
TUPdTPINT FLISTT1268409508(FLISTT1268409508_frame);
TUPdTPdLAYOUT ARRAYT2024950707(ARRAYT2024950707_frame);
TUPdTPdLAYOUT FLISTT123078916(FLISTT123078916_frame);
dAM_CONST ARRAYd198808957(ARRAYd198808957_frame);
dAM_CONST FLISTd617209260(FLISTd617209260_frame);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
dAM_EXPR FLISTd2122660729(FLISTd2122660729_frame);
dAM_EXPR INLINE1060127472(INLINE, AM_ROUT_DEF, AM_LOOP_STMT, AM_ITE1809135850);
dAM_EXPR INLINE1324370258(INLINE_INT_FOLD, AM_ROU1916046290);
dAM_EXPR INLINE1426016646(INLINE, AM_ITE1809135850);
dAM_EXPR INLINE1475043475(INLINE1433668901, AM_ROU1916046290);
dAM_EXPR INLINE1736265130(INLINE1164904870, AM_ROU1916046290);
dAM_EXPR INLINE1797911459(INLINE, AM_ROUT_DEF, AM_ROU1916046290);
dAM_EXPR INLINE1949170047(INLINE_ROUT, AM_ROU1916046290);
dAM_EXPR INLINE2023716438(INLINE1455768374, AM_ROU1916046290);
dAM_EXPR INLINE46712374(INLINE_ATTR_READ, AM_ROU1916046290);
dAM_EXPR INLINE472404814(INLINE, AM_ROU1916046290);
dAM_EXPR INLINE655229257(INLINE1596311811, AM_ROU1916046290);
dAM_EXPR INLINE986164897(INLINE_ITER, AM_ITE1809135850);
dAM_STMT AM_ASS1201484234(AM_ASSIGN_STMT);
dAM_STMT AM_ASS1841384941(AM_ASSERT_STMT);
dAM_STMT AM_ATT1156941996(AM_ATTACH_STMT);
dAM_STMT AM_BRE1241758751(AM_BREAK_STMT);
dAM_STMT AM_CAS1510303900(AM_CASE_STMT);
dAM_STMT AM_COM1124366108(AM_COMMENT_STMT);
dAM_STMT AM_EXP1561605445(AM_EXPR_STMT);
dAM_STMT AM_FOR1357281358(AM_FORK_STMT);
dAM_STMT AM_IF_1153855302(AM_IF_STMT);
dAM_STMT AM_INI81424971(AM_INITIAL_STMT);
dAM_STMT AM_INV1267245156(AM_INV1167837230);
dAM_STMT AM_LOC724705923(AM_LOCK_STMT);
dAM_STMT AM_LOO439140156(AM_LOOP_STMT);
dAM_STMT AM_PAR309007796(AM_PAR_STMT);
dAM_STMT AM_POS1775621496(AM_POST_STMT);
dAM_STMT AM_PRE1502061133(AM_PREFETCH_STMT);
dAM_STMT AM_PRE1762688610(AM_PRE_STMT);
dAM_STMT AM_PRO557659408(AM_PROTECT_STMT);
dAM_STMT AM_RAI1337438778(AM_RAISE_STMT);
dAM_STMT AM_RET1976133913(AM_RETURN_STMT);
dAM_STMT AM_SYN2020605939(AM_SYNC_STMT);
dAM_STMT AM_TYP1504178732(AM_TYPECASE_STMT);
dAM_STMT AM_UNL1996507749(AM_UNLOCK_STMT);
dAM_STMT AM_WAI1727009663(AM_WAITFOR_STMT);
dAM_STMT AM_WIT315547139(AM_WIT1996971603);
dAM_STMT AM_YIE1267845009(AM_YIELD_STMT);
dAM_STMT ARRAYd615421366(ARRAYd615421366_frame);
dAM_STMT FLISTd1374595573(FLISTd1374595573_frame);
dAS_CLASS_ELT AS_ATT97460488(AS_ATTR_DEF);
dAS_CLASS_ELT AS_CON306298556(AS_CONST_DEF);
dAS_CLASS_ELT AS_INC1775598384(AS_INC1416917001);
dAS_CLASS_ELT AS_ROU123442773(AS_ROUT_DEF);
dAS_CLASS_ELT AS_SHA297921449(AS_SHARED_DEF);
dAS_EXPR AS_AND244040648(AS_AND_EXPR);
dAS_EXPR AS_ANY1960655325(AS_ANY_EXPR);
dAS_EXPR AS_ARR1675309363(AS_ARRAY_EXPR);
dAS_EXPR AS_AT_246103478(AS_AT_EXPR);
dAS_EXPR AS_BOO1496152570(AS_BOOL_LIT_EXPR);
dAS_EXPR AS_BOU110275841(AS_BOU14269336);
dAS_EXPR AS_BRE631040475(AS_BREAK_EXPR);
dAS_EXPR AS_CAL1709269196(AS_CALL_EXPR);
dAS_EXPR AS_CHA1807865070(AS_CHAR_LIT_EXPR);
dAS_EXPR AS_CLU1420620999(AS_CLUSTER_EXPR);
dAS_EXPR AS_CLU792721656(AS_CLU553570599);
dAS_EXPR AS_COH1869044806(AS_COHORT_EXPR);
dAS_EXPR AS_CRE1115272673(AS_CREATE_EXPR);
dAS_EXPR AS_EXC1501278546(AS_EXCEPT_EXPR);
dAS_EXPR AS_FAR2096957028(AS_FAR_EXPR);
dAS_EXPR AS_FLT1204717277(AS_FLT_LIT_EXPR);
dAS_EXPR AS_GLO1872829932(AS_GLOBAL_EXPR);
dAS_EXPR AS_HER846597914(AS_HERE_EXPR);
dAS_EXPR AS_INI1732476001(AS_INITIAL_EXPR);
dAS_EXPR AS_INT896052382(AS_INT_LIT_EXPR);
dAS_EXPR AS_IS_1211675728(AS_IS_VOID_EXPR);
dAS_EXPR AS_NEA160870810(AS_NEAR_EXPR);
dAS_EXPR AS_NEW162891781(AS_NEW_EXPR);
dAS_EXPR AS_OR_261789378(AS_OR_EXPR);
dAS_EXPR AS_RES945429434(AS_RESULT_EXPR);
dAS_EXPR AS_SEL1160430562(AS_SELF_EXPR);
dAS_EXPR AS_STR70439144(AS_STR_LIT_EXPR);
dAS_EXPR AS_UND1982396421(AS_UND152986614);
dAS_EXPR AS_VOI1856591394(AS_VOID_EXPR);
dAS_EXPR AS_WHE2011036209(AS_WHERE_EXPR);
dAS_STMT AS_ASS255483717(AS_ASSERT_STMT);
dAS_STMT AS_ASS384416990(AS_ASSIGN_STMT);
dAS_STMT AS_ATT428959228(AS_ATTACH_STMT);
dAS_STMT AS_CAS568284252(AS_CASE_STMT);
dAS_STMT AS_DEC1501678601(AS_DEC_STMT);
dAS_STMT AS_EXP516982707(AS_EXPR_STMT);
dAS_STMT AS_FOR721306794(AS_FORK_STMT);
dAS_STMT AS_IF_192647666(AS_IF_STMT);
dAS_STMT AS_INT1461683196(AS_INT2031239268);
dAS_STMT AS_LOC1353882229(AS_LOCK_STMT);
dAS_STMT AS_LOO1639447996(AS_LOOP_STMT);
dAS_STMT AS_PAR1281243032(AS_PAR_STMT);
dAS_STMT AS_PRO1410999444(AS_PROTECT_STMT);
dAS_STMT AS_QUI758322419(AS_QUIT_STMT);
dAS_STMT AS_RAI287018298(AS_RAISE_STMT);
dAS_STMT AS_RET732932159(AS_RETURN_STMT);
dAS_STMT AS_STM1163003696(AS_STMT);
dAS_STMT AS_SYN57982213(AS_SYNC_STMT);
dAS_STMT AS_TYP1323758084(AS_TYPECASE_STMT);
dAS_STMT AS_UNL712558323(AS_UNLOCK_STMT);
dAS_STMT AS_WIT1968509975(AS_WIT1063437351);
dAS_STMT AS_YIE1402665211(AS_YIELD_STMT);
dLAYOUT ARRAYd1647530701(ARRAYd1647530701_frame);
dLAYOUT FLISTd1954760550(FLISTd1954760550_frame);
dLAYOUT FSETdL485771726(FSETdL485771726_frame);
dTP AM_ANY772124871(AM_ANY_EXPR);
dTP AM_ARR1099277377(AM_ARRAY_EXPR);
dTP AM_ARR925292072(AM_ARR_CONST);
dTP AM_ATT839964902(AM_ATTR_EXPR);
dTP AM_AT_1635564765(AM_AT_EXPR);
dTP AM_BND192433970(AM_BND260301329);
dTP AM_BND540727053(AM_BND1124877163);
dTP AM_BND95745606(AM_BND367211769);
dTP AM_BOO1614359222(AM_BOOL_CONST);
dTP AM_CHA165359522(AM_CHAR_CONST);
dTP AM_CLU2134150433(AM_CLU1286269335);
dTP AM_CLU889770873(AM_CLUSTER_EXPR);
dTP AM_CONST_tprdTP(AM_CONST);
dTP AM_EXC706383345(AM_EXCEPT_EXPR);
dTP AM_EXT195226505(AM_EXT_CALL_EXPR);
dTP AM_FAR24566214(AM_FAR_EXPR);
dTP AM_FLT1597643896(AM_FLTDX_CONST);
dTP AM_FLT2085750594(AM_FLTD_CONST);
dTP AM_FLT79327789(AM_FLT_CONST);
dTP AM_FLT804205627(AM_FLTI_CONST);
dTP AM_FLT884139698(AM_FLTX_CONST);
dTP AM_GLO541877835(AM_GLOBAL_EXPR);
dTP AM_HER217252257(AM_HERE_EXPR);
dTP AM_IF_179347781(AM_IF_EXPR);
dTP AM_INT1372528330(AM_INTI_CONST);
dTP AM_INT1391542618(AM_INT_CONST);
dTP AM_IS_826521970(AM_IS_VOID_EXPR);
dTP AM_ITE597285895(AM_ITE1809135850);
dTP AM_LOC1004636145(AM_LOCAL_EXPR);
dTP AM_NEA176857219(AM_NEAR_EXPR);
dTP AM_NEW710003407(AM_NEW_EXPR);
dTP AM_ROU885465471(AM_ROU1916046290);
dTP AM_SHA1973416871(AM_SHARED_EXPR);
dTP AM_STM300451057(AM_STMT_EXPR);
dTP AM_STR690233376(AM_STR_CONST);
dTP AM_VAR2117196902(AM_VAR744470097);
dTP AM_VAT1812280108(AM_VAT319982528);
dTP AM_VOI984518984(AM_VOID_CONST);
dTP AM_WHE843899429(AM_WHERE_EXPR);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP FLISTd1740602786(FLISTd1740602786_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void AM_ASS1281039639(AM_ASSIGN_STMT, dAM_STMT);
void AM_ASS306541468(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS356960882(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS946442175(AM_ASSIGN_STMT, dAM_STMT);
void AM_ATT1913806391(AM_ATTACH_STMT, dAM_STMT);
void AM_ATT990984413(AM_ATTACH_STMT, dAM_STMT);
void AM_BRE261382646(AM_BREAK_STMT, dAM_STMT);
void AM_BRE905282136(AM_BREAK_STMT, dAM_STMT);
void AM_CAS1095788609(AM_CASE_STMT, dAM_STMT);
void AM_CAS637622509(AM_CASE_STMT, dAM_STMT);
void AM_COM1022674779(AM_COMMENT_STMT, dAM_STMT);
void AM_COM832310723(AM_COMMENT_STMT, dAM_STMT);
void AM_EXP1859190126(AM_EXPR_STMT, dAM_STMT);
void AM_EXP586320964(AM_EXPR_STMT, dAM_STMT);
void AM_FOR1689225795(AM_FORK_STMT, dAM_STMT);
void AM_FOR790645051(AM_FORK_STMT, dAM_STMT);
void AM_IF_1406990231(AM_IF_STMT, dAM_STMT);
void AM_IF_993185585(AM_IF_STMT, dAM_STMT);
void AM_INI2007284982(AM_INITIAL_STMT, dAM_STMT);
void AM_INI2065615916(AM_INITIAL_STMT, dAM_STMT);
void AM_INV749070155(AM_INV1167837230, dAM_STMT);
void AM_INV880681253(AM_INV1167837230, dAM_STMT);
void AM_LOC1423220486(AM_LOCK_STMT, dAM_STMT);
void AM_LOC962760448(AM_LOCK_STMT, dAM_STMT);
void AM_LOO1708786253(AM_LOOP_STMT, dAM_STMT);
void AM_LOO952733665(AM_LOOP_STMT, dAM_STMT);
void AM_PAR1454262261(AM_PAR_STMT, dAM_STMT);
void AM_PAR1838033091(AM_PAR_STMT, dAM_STMT);
void AM_POS1908807043(AM_POST_STMT, dAM_STMT);
void AM_POS372304913(AM_POST_STMT, dAM_STMT);
void AM_PRE1879848758(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRE2012355323(AM_PRE_STMT, dAM_STMT);
void AM_PRE385237799(AM_PRE_STMT, dAM_STMT);
void AM_PRE645865276(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRO1589381479(AM_PROTECT_STMT, dAM_STMT);
void AM_PRO1885345065(AM_PROTECT_STMT, dAM_STMT);
void AM_RAI1178977909(AM_RAISE_STMT, dAM_STMT);
void AM_RAI810487631(AM_RAISE_STMT, dAM_STMT);
void AM_RET170906974(AM_RETURN_STMT, dAM_STMT);
void AM_RET917156604(AM_RETURN_STMT, dAM_STMT);
void AM_SYN127320470(AM_SYNC_STMT, dAM_STMT);
void AM_SYN631230960(AM_SYNC_STMT, dAM_STMT);
void AM_TYP505714135(AM_TYPECASE_STMT, dAM_STMT);
void AM_TYP642862155(AM_TYPECASE_STMT, dAM_STMT);
void AM_UNL150533138(AM_UNLOCK_STMT, dAM_STMT);
void AM_UNL2142830896(AM_UNLOCK_STMT, dAM_STMT);
void AM_WAI420031224(AM_WAITFOR_STMT, dAM_STMT);
void AM_WAI656698018(AM_WAITFOR_STMT, dAM_STMT);
void AM_WIT1831493748(AM_WIT1996971603, dAM_STMT);
void AM_WIT906438282(AM_WIT1996971603, dAM_STMT);
void AM_YIE1088587192(AM_YIELD_STMT, dAM_STMT);
void AM_YIE879195878(AM_YIELD_STMT, dAM_STMT);
void AS_AND1244275737(AS_AND_EXPR, dAS_EXPR);
void AS_AND1398627607(AS_AND_EXPR, SFILE_ID);
void AS_AND1903000239(AS_AND_EXPR, dAS_EXPR);
void AS_ANY1724395066(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY187271084(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY72331114(AS_ANY_EXPR, SFILE_ID);
void AS_ARR1604409854(AS_ARRAY_EXPR, SFILE_ID);
void AS_ARR471731524(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ARR47654098(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ASS1183239070(AS_ASSIGN_STMT, SFILE_ID);
void AS_ASS1330792936(AS_ASSERT_STMT, BOOL);
void AS_ASS1461460287(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1708420135(AS_ASSIGN_STMT, BOOL);
void AS_ASS1762623897(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1892442692(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS2029454919(AS_ASSERT_STMT, AS_STMT_LIST);
void AS_ASS2107317827(AS_ASSERT_STMT, SFILE_ID);
void AS_ASS537381530(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS650357212(AS_ASSIGN_STMT, AS_STMT_LIST);
void AS_ATT1135887709(AS_ATTACH_STMT, BOOL);
void AS_ATT1141535277(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT1480967150(AS_ATTACH_STMT, AS_STMT_LIST);
void AS_ATT1718081659(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1733385743(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1987206051(AS_ATTR_DEF, BOOL);
void AS_ATT2024939759(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT83117804(AS_ATTACH_STMT, SFILE_ID);
void AS_AT_1901822931(AS_AT_EXPR, dAS_EXPR);
void AS_AT_2092645707(AS_AT_EXPR, dAS_EXPR);
void AS_AT_550257637(AS_AT_EXPR, SFILE_ID);
void AS_BOO1855503827(AS_BOOL_LIT_EXPR, SFILE_ID);
void AS_BOO203439875(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOO650888317(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOU2006233068(AS_BOU14269336, dAS_EXPR);
void AS_BOU2036765046(AS_BOU14269336, dAS_EXPR);
void AS_BOU354169116(AS_BOU14269336, SFILE_ID);
void AS_BRE1516885934(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE679018004(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE973045948(AS_BREAK_EXPR, SFILE_ID);
void AS_CAL2065155647(AS_CALL_EXPR, SFILE_ID);
void AS_CAL437771691(AS_CALL_EXPR, dAS_EXPR);
void AS_CAL577747697(AS_CALL_EXPR, dAS_EXPR);
void AS_CAS1011726673(AS_CASE_STMT, BOOL);
void AS_CAS1160155324(AS_CASE_STMT, SFILE_ID);
void AS_CAS1578756635(AS_CASE_STMT, dAS_STMT);
void AS_CAS490112615(AS_CASE_STMT, dAS_STMT);
void AS_CAS703201798(AS_CASE_STMT, AS_STMT_LIST);
void AS_CHA1047100373(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA340061339(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA604963579(AS_CHAR_LIT_EXPR, SFILE_ID);
void AS_CLU1354319231(AS_CLU553570599, dAS_EXPR);
void AS_CLU1700532850(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU1903972459(AS_CLU553570599, SFILE_ID);
void AS_CLU726419888(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU738930885(AS_CLU553570599, dAS_EXPR);
void AS_CLU942370494(AS_CLUSTER_EXPR, SFILE_ID);
void AS_COH1216011035(AS_COHORT_EXPR, SFILE_ID);
void AS_COH278881603(AS_COHORT_EXPR, dAS_EXPR);
void AS_COH436052917(AS_COHORT_EXPR, dAS_EXPR);
void AS_CON1753066199(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CON1962795038(AS_CONST_DEF, BOOL);
void AS_CON737776233(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CRE1032653736(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE1964853282(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE312789330(AS_CREATE_EXPR, SFILE_ID);
void AS_DEC1092435373(AS_DEC_STMT, SFILE_ID);
void AS_DEC1996561535(AS_DEC_STMT, BOOL);
void AS_DEC2062280289(AS_DEC_STMT, AS_STMT_LIST);
void AS_DEC557832566(AS_DEC_STMT, dAS_STMT);
void AS_DEC646247808(AS_DEC_STMT, dAS_STMT);
void AS_EXC1647038221(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXC5025731(AS_EXCEPT_EXPR, SFILE_ID);
void AS_EXC645762341(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXP1321747790(AS_EXPR_STMT, BOOL);
void AS_EXP1630058180(AS_EXPR_STMT, dAS_STMT);
void AS_EXP1794823411(AS_EXPR_STMT, SFILE_ID);
void AS_EXP200906119(AS_EXPR_STMT, AS_STMT_LIST);
void AS_EXP849875946(AS_EXPR_STMT, dAS_STMT);
void AS_FAR1938606563(AS_FAR_EXPR, dAS_EXPR);
void AS_FAR286542611(AS_FAR_EXPR, SFILE_ID);
void AS_FAR50083859(AS_FAR_EXPR, dAS_EXPR);
void AS_FLT1911942022(AS_FLT_LIT_EXPR, SFILE_ID);
void AS_FLT259878070(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FLT943209132(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FOR1019127704(AS_FORK_STMT, AS_STMT_LIST);
void AS_FOR103324571(AS_FORK_STMT, dAS_STMT);
void AS_FOR1425734093(AS_FORK_STMT, dAS_STMT);
void AS_FOR1673645269(AS_FORK_STMT, BOOL);
void AS_FOR1753592510(AS_FORK_STMT, SFILE_ID);
void AS_GLO1712525775(AS_GLOBAL_EXPR, SFILE_ID);
void AS_GLO275096477(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_GLO930377569(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_HER1301328495(AS_HERE_EXPR, dAS_EXPR);
void AS_HER1479869719(AS_HERE_EXPR, SFILE_ID);
void AS_HER172194233(AS_HERE_EXPR, dAS_EXPR);
void AS_IF_164980229(AS_IF_STMT, BOOL);
void AS_IF_1658967836(AS_IF_STMT, AS_STMT_LIST);
void AS_IF_1955278743(AS_IF_STMT, dAS_STMT);
void AS_IF_671597921(AS_IF_STMT, dAS_STMT);
void AS_IF_978670018(AS_IF_STMT, SFILE_ID);
void AS_INC1475294123(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC1807545759(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC874191011(AS_INC1416917001, BOOL);
void AS_INI1852876520(AS_INITIAL_EXPR, SFILE_ID);
void AS_INI414564886(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INI790026824(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INT1009823030(AS_INT2031239268, AS_STMT_LIST);
void AS_INT1251874027(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT2141902673(AS_INT_LIT_EXPR, SFILE_ID);
void AS_INT501000671(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT685357691(AS_INT2031239268, dAS_STMT);
void AS_INT765687160(AS_INT2031239268, SFILE_ID);
void AS_INT83312974(AS_INT2031239268, BOOL);
void AS_INT884580779(AS_INT2031239268, dAS_STMT);
void AS_IS_128975537(AS_IS_VOID_EXPR, SFILE_ID);
void AS_IS_1523088415(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_IS_936250681(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_LOC1027127163(AS_LOCK_STMT, SFILE_ID);
void AS_LOC1144520043(AS_LOCK_STMT, AS_STMT_LIST);
void AS_LOC623140776(AS_LOCK_STMT, dAS_STMT);
void AS_LOC755705564(AS_LOCK_STMT, BOOL);
void AS_LOC793158658(AS_LOCK_STMT, dAS_STMT);
void AS_LOO1017100380(AS_LOOP_STMT, SFILE_ID);
void AS_LOO1542269018(AS_LOOP_STMT, AS_STMT_LIST);
void AS_LOO507592891(AS_LOOP_STMT, dAS_STMT);
void AS_LOO565196687(AS_LOOP_STMT, BOOL);
void AS_LOO633167559(AS_LOOP_STMT, dAS_STMT);
void AS_NEA1203816563(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEA1439086781(AS_NEAR_EXPR, SFILE_ID);
void AS_NEA1986170077(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEW1985034628(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW2117994542(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW524908802(AS_NEW_EXPR, SFILE_ID);
void AS_OR_1885251509(AS_OR_EXPR, dAS_EXPR);
void AS_OR_784015075(AS_OR_EXPR, SFILE_ID);
void AS_OR_868048877(AS_OR_EXPR, dAS_EXPR);
void AS_PAR1480073124(AS_PAR_STMT, SFILE_ID);
void AS_PAR170194815(AS_PAR_STMT, dAS_STMT);
void AS_PAR771744212(AS_PAR_STMT, BOOL);
void AS_PAR865797855(AS_PAR_STMT, dAS_STMT);
void AS_PAR99672090(AS_PAR_STMT, AS_STMT_LIST);
void AS_PRO169226468(AS_PROTECT_STMT, BOOL);
void AS_PRO1754180698(AS_PROTECT_STMT, AS_STMT_LIST);
void AS_PRO1882650168(AS_PROTECT_STMT, SFILE_ID);
void AS_PRO232382229(AS_PROTECT_STMT, dAS_STMT);
void AS_PRO736041443(AS_PROTECT_STMT, dAS_STMT);
void AS_QUI1389603990(AS_QUIT_STMT, dAS_STMT);
void AS_QUI1442696788(AS_QUIT_STMT, BOOL);
void AS_QUI1673525760(AS_QUIT_STMT, dAS_STMT);
void AS_QUI2137045949(AS_QUIT_STMT, AS_STMT_LIST);
void AS_QUI971173597(AS_QUIT_STMT, SFILE_ID);
void AS_RAI1860022589(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2032317945(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2115288988(AS_RAISE_STMT, AS_STMT_LIST);
void AS_RAI248856852(AS_RAISE_STMT, BOOL);
void AS_RAI382050006(AS_RAISE_STMT, SFILE_ID);
void AS_RES1196907749(AS_RESULT_EXPR, SFILE_ID);
void AS_RES1201611453(AS_RESULT_EXPR, dAS_EXPR);
void AS_RES1445995595(AS_RESULT_EXPR, dAS_EXPR);
void AS_RET1042240335(AS_RETURN_STMT, AS_STMT_LIST);
void AS_RET1097577252(AS_RETURN_STMT, dAS_STMT);
void AS_RET1414994250(AS_RETURN_STMT, dAS_STMT);
void AS_RET1547122105(AS_RETURN_STMT, SFILE_ID);
void AS_RET910390234(AS_RETURN_STMT, BOOL);
void AS_ROU1496248876(AS_ROUT_DEF, BOOL);
void AS_ROU166638356(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_ROU920632016(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_SEL1190661345(AS_SELF_EXPR, SFILE_ID);
void AS_SEL1452241999(AS_SELF_EXPR, dAS_EXPR);
void AS_SEL987495847(AS_SELF_EXPR, dAS_EXPR);
void AS_SHA1271069808(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA746153340(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA905735878(AS_SHARED_DEF, BOOL);
void AS_STM1692300976(AS_STMT, BOOL);
void AS_STM1734576793(AS_STMT, dAS_STMT);
void AS_STM193774686(AS_STMT, AS_STMT_LIST);
void AS_STM910122564(AS_STMT, SFILE_ID);
void AS_STM984922713(AS_STMT, dAS_STMT);
void AS_STR1192634823(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR2077487265(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR459429129(AS_STR_LIT_EXPR, SFILE_ID);
void AS_SYN1248387412(AS_SYNC_STMT, BOOL);
void AS_SYN1817260677(AS_SYNC_STMT, AS_STMT_LIST);
void AS_SYN2089058674(AS_SYNC_STMT, dAS_STMT);
void AS_SYN695597675(AS_SYNC_STMT, SFILE_ID);
void AS_SYN954670264(AS_SYNC_STMT, dAS_STMT);
void AS_TYP1218814867(AS_TYPECASE_STMT, BOOL);
void AS_TYP1661742497(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP1802831442(AS_TYPECASE_STMT, AS_STMT_LIST);
void AS_TYP823282803(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP982956860(AS_TYPECASE_STMT, SFILE_ID);
void AS_UND165529988(AS_UND152986614, dAS_EXPR);
void AS_UND1829175574(AS_UND152986614, dAS_EXPR);
void AS_UND813727770(AS_UND152986614, SFILE_ID);
void AS_UNL1079389410(AS_UNLOCK_STMT, BOOL);
void AS_UNL1435368086(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL1962410248(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL312142309(AS_UNLOCK_STMT, SFILE_ID);
void AS_UNL619513787(AS_UNLOCK_STMT, AS_STMT_LIST);
void AS_VOI1015295839(AS_VOID_EXPR, SFILE_ID);
void AS_VOI291335015(AS_VOID_EXPR, dAS_EXPR);
void AS_VOI636768113(AS_VOID_EXPR, dAS_EXPR);
void AS_WHE136004678(AS_WHERE_EXPR, dAS_EXPR);
void AS_WHE666102736(AS_WHERE_EXPR, SFILE_ID);
void AS_WHE985961216(AS_WHERE_EXPR, dAS_EXPR);
void AS_WIT1613050679(AS_WIT1063437351, BOOL);
void AS_WIT178530912(AS_WIT1063437351, dAS_STMT);
void AS_WIT1810155147(AS_WIT1063437351, SFILE_ID);
void AS_WIT1949427223(AS_WIT1063437351, AS_STMT_LIST);
void AS_WIT834544210(AS_WIT1063437351, dAS_STMT);
void AS_YIE1194255817(AS_YIELD_STMT, AS_STMT_LIST);
void AS_YIE1468566771(AS_YIELD_STMT, BOOL);
void AS_YIE1941927228(AS_YIELD_STMT, dAS_STMT);
void AS_YIE291659289(AS_YIELD_STMT, SFILE_ID);
void AS_YIE745261198(AS_YIELD_STMT, dAS_STMT);
void BUILD_1202586967(BUILD_TYPE_GRAPH);
void CGEN_e103713585(CGEN, AM_ROUT_DEF);
void CGEN_finalize(CGEN);
void CGEN_init(CGEN);
void CHECK_1724422536(CHECK_1174552075);
void CHECK_801221867(CHECK_AM);
void CS_OPT63514076(CS_OPTIONS, ARRAYSTR);
void CS_OPT994959939(CS_OPTIONS);
void FIND_T194079625(FIND_TYPES);
void GENERA1485011776(GENERATE_AM);
void GENERA88776523(GENERATE_AM, SIG);
void GET_MA42890279(GET_MAIN_SIG, STR);
void INLINE1765310158(INLINE, AM_ROUT_DEF);
void INLINE_init(INLINE);
void NULL_S1721445066(NULL_STAT, STR);
void NULL_STAT_print(NULL_STAT);
void OPTIMI1081780129(OPTIMIZE, AM_ROUT_DEF);
void OPTIMI333751220(OPTIMIZE);
void OPTIMI462748081(OPTIMIZE, AM_ROUT_DEF);
void OPTIMIZE_init(OPTIMIZE);
void PARSE_parse_STR(PARSE, STR);
void STAT_incr_STR(STAT1, STR);
void STAT_print(STAT1);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */

ABSTRACT_LAYOUT ARRAYA967821567_unbox(dELTAB1485261483_frame);
void* ARRAYA967821567_frame_alloc();
ABSTRACT_LAYOUT FLISTA1252869158_unbox(dELTAB1485261483_frame);
void* FLISTA1252869158_frame_alloc();
ABSTRACT_LAYOUT FSETAB1942662686_unbox(dELTAB1485261483_frame);
void* FSETAB1942662686_frame_alloc();
AM_BND1124877163 ARRAYA945146506_unbox(dELTAM745071702_frame);
void* ARRAYA945146506_frame_alloc();
AM_BND1124877163 FLISTA273121893_unbox(dELTAM745071702_frame);
void* FLISTA273121893_frame_alloc();
AM_BND1124877163 FSETAM595751729_unbox(dELTAM745071702_frame);
void* FSETAM595751729_frame_alloc();
AM_BND367211769 ARRAYA1632293813_unbox(dELTAM157362729_frame);
void* ARRAYA1632293813_frame_alloc();
AM_BND367211769 FLISTA1446310868_unbox(dELTAM157362729_frame);
void* FLISTA1446310868_frame_alloc();
AM_CALL_ARG AM_BND1292359506_unbox(AM_BND260301329, INT);
AM_CALL_ARG AM_BND360941210_unbox(AM_BND367211769, INT);
AM_CALL_ARG AM_EXT48347235_unbox(AM_EXT_CALL_EXPR, INT);
AM_CALL_ARG AM_ITE329074641_unbox(AM_ITE1809135850, INT);
AM_CALL_ARG AM_ROU602343655_unbox(AM_ROU1916046290, INT);
AM_CALL_ARG ARRAYA44788561_unbox(dELTAM1395458731_frame);
void* ARRAYA44788561_frame_alloc();
AM_CALL_ARG FLISTA964597258_unbox(dELTAM1395458731_frame);
void* FLISTA964597258_frame_alloc();
AM_CURSOR ARRAYA900901929_unbox(dELTAM1280476813_frame);
void* ARRAYA900901929_frame_alloc();
AM_CURSOR FLISTA84883712_unbox(dELTAM1280476813_frame);
void* FLISTA84883712_frame_alloc();
AM_CURSOR_POS ARRAYA2146369463_unbox(dELTAM2044693667_frame);
void* ARRAYA2146369463_frame_alloc();
AM_CURSOR_POS A_STAC408111917_unbox(dELTAM2044693667_frame);
void* A_STAC408111917_frame_alloc();
AM_CURSOR_POS FLISTA1660160_unbox(dELTAM2044693667_frame);
void* FLISTA1660160_frame_alloc();
AM_FORMAL_ARG ARRAYA1315754333_unbox(dELTAM1417430129_frame);
void* ARRAYA1315754333_frame_alloc();
AM_FORMAL_ARG FLISTA831183340_unbox(dELTAM1417430129_frame);
void* FLISTA831183340_frame_alloc();
AM_ITE1809135850 ARRAYA494342579_unbox(dELTAM1806540817_frame);
void* ARRAYA494342579_frame_alloc();
AM_ITE1809135850 FLISTA20621028_unbox(dELTAM1806540817_frame);
void* FLISTA20621028_frame_alloc();
AM_LOCAL_EXPR ARRAYA27295121_unbox(dELTAM128970917_frame);
void* ARRAYA27295121_frame_alloc();
AM_LOCAL_EXPR FLISTA2119642552_unbox(dELTAM128970917_frame);
void* FLISTA2119642552_frame_alloc();
AM_LOCAL_EXPR FSETAM441739032_unbox(dELTAM128970917_frame);
void* FSETAM441739032_frame_alloc();
AM_ROUT_DEF ARRAYA1253289773_unbox(dELTAM186957519_frame);
void* ARRAYA1253289773_frame_alloc();
AM_ROUT_DEF FLISTA2121868826_unbox(dELTAM186957519_frame);
void* FLISTA2121868826_frame_alloc();
AM_ROUT_DEF FSETAM1323524382_unbox(dELTAM186957519_frame);
void* FSETAM1323524382_frame_alloc();
AM_SHARED_EXPR ARRAYA1830087990_unbox(dELTAM119654938_frame);
void* ARRAYA1830087990_frame_alloc();
AM_SHARED_EXPR FLISTA120298683_unbox(dELTAM119654938_frame);
void* FLISTA120298683_frame_alloc();
AM_SHARED_EXPR FSETAM12483681_unbox(dELTAM119654938_frame);
void* FSETAM12483681_frame_alloc();
ARG ARRAYA827580689_unbox(dELTARG_eltbrARG_frame);
void* ARRAYA827580689_frame_alloc();
ARG FLISTA420234122_unbox(dELTARG_eltbrARG_frame);
void* FLISTA420234122_frame_alloc();
ARRAYBOOL ARRAYA277860273_unbox(dELTAR657435157_frame);
void* ARRAYA277860273_frame_alloc();
ARRAYBOOL FLISTA538157944_unbox(dELTAR657435157_frame);
void* FLISTA538157944_frame_alloc();
ARRAYSTR ARRAYA1729769776_unbox(dELTAR1531173752_frame);
void* ARRAYA1729769776_frame_alloc();
ARRAYSTR FLISTA970595569_unbox(dELTAR1531173752_frame);
void* FLISTA970595569_frame_alloc();
ARRAYdAM_EXPR ARRAYA289850317_unbox(dELTAR391526113_frame);
void* ARRAYA289850317_frame_alloc();
ARRAYdAM_EXPR FLISTA1857087356_unbox(dELTAR391526113_frame);
void* FLISTA1857087356_frame_alloc();
AS_CLASS_DEF PARSE_962387968_unbox(PARSE, IDENT, INT);
AS_FEAT_MOD ARRAYA1522064985_unbox(dELTAS81817693_frame);
void* ARRAYA1522064985_frame_alloc();
AS_FEAT_MOD FLISTA1853093614_unbox(dELTAS81817693_frame);
void* FLISTA1853093614_frame_alloc();
AS_FEAT_MOD FSETAS1592299594_unbox(dELTAS81817693_frame);
void* FSETAS1592299594_frame_alloc();
AS_STMT_LIST AS_ASS1748452704_unbox(AS_ASSERT_STMT);
AS_STMT_LIST AS_ASS369354997_unbox(AS_ASSIGN_STMT);
AS_STMT_LIST AS_ATT1199964935_unbox(AS_ATTACH_STMT);
AS_STMT_LIST AS_CAS422199583_unbox(AS_CASE_STMT);
AS_STMT_LIST AS_DEC1951684792_unbox(AS_DEC_STMT);
AS_STMT_LIST AS_EXP80096096_unbox(AS_EXPR_STMT);
AS_STMT_LIST AS_FOR1300129919_unbox(AS_FORK_STMT);
AS_STMT_LIST AS_IF_1377965621_unbox(AS_IF_STMT);
AS_STMT_LIST AS_INT1290825245_unbox(AS_INT2031239268);
AS_STMT_LIST AS_LOC1425522258_unbox(AS_LOCK_STMT);
AS_STMT_LIST AS_LOO1823271233_unbox(AS_LOOP_STMT);
AS_STMT_LIST AS_PAR380674305_unbox(AS_PAR_STMT);
AS_STMT_LIST AS_PRO1473178483_unbox(AS_PROTECT_STMT);
AS_STMT_LIST AS_QUI1856043734_unbox(AS_QUIT_STMT);
AS_STMT_LIST AS_RAI1834286773_unbox(AS_RAISE_STMT);
AS_STMT_LIST AS_RET1323242550_unbox(AS_RETURN_STMT);
AS_STMT_LIST AS_STM87227529_unbox(AS_STMT);
AS_STMT_LIST AS_SYN1536258462_unbox(AS_SYNC_STMT);
AS_STMT_LIST AS_TYP2083833657_unbox(AS_TYPECASE_STMT);
AS_STMT_LIST AS_UNL900516002_unbox(AS_UNLOCK_STMT);
AS_STMT_LIST AS_WIT1668425008_unbox(AS_WIT1063437351);
AS_STMT_LIST AS_YIE913253602_unbox(AS_YIELD_STMT);
AS_TYPE_SPEC TP_CLA981298431_unbox(TP_CLASS);
AS_TYPE_SPEC TP_ITE610562510_unbox(TP_ITER);
AS_TYPE_SPEC TP_ROU1982035318_unbox(TP_ROUT);
ATTRSORT ARRAYA1660316962_unbox(dELTAT1600626566_frame);
void* ARRAYA1660316962_frame_alloc();
ATTRSORT FLISTA901142755_unbox(dELTAT1600626566_frame);
void* FLISTA901142755_frame_alloc();
BOOL AM_ANY1687381320_unbox(AM_ANY_EXPR, OB);
BOOL AM_ANY309199231_unbox(AM_ANY_EXPR);
BOOL AM_ARR1751046750_unbox(AM_ARRAY_EXPR, OB);
BOOL AM_ARR437965311_unbox(AM_ARRAY_EXPR);
BOOL AM_ARR495057476_unbox(AM_ARR_CONST);
BOOL AM_ARR62730649_unbox(AM_ARR_CONST, OB);
BOOL AM_ATT1582125658_unbox(AM_ATTR_EXPR);
BOOL AM_ATT1818896075_unbox(AM_ATTR_EXPR, OB);
BOOL AM_AT_1010480980_unbox(AM_AT_EXPR, OB);
BOOL AM_AT_1425681295_unbox(AM_AT_EXPR);
BOOL AM_BND1565517842_unbox(AM_BND367211769);
BOOL AM_BND1588574812_unbox(AM_BND1124877163, OB);
BOOL AM_BND1666770893_unbox(AM_BND260301329, OB);
BOOL AM_BND1960601142_unbox(AM_BND260301329);
BOOL AM_BND48137285_unbox(AM_BND367211769, OB);
BOOL AM_BND9208257_unbox(AM_BND1124877163);
BOOL AM_BOO1499785931_unbox(AM_BOOL_CONST, OB);
BOOL AM_BOO274293822_unbox(AM_BOOL_CONST);
BOOL AM_CHA1550967974_unbox(AM_CHAR_CONST);
BOOL AM_CHA181917731_unbox(AM_CHAR_CONST, OB);
BOOL AM_CLU1159843664_unbox(AM_CLU1286269335, OB);
BOOL AM_CLU170440611_unbox(AM_CLU1286269335);
BOOL AM_CLU528836536_unbox(AM_CLUSTER_EXPR, OB);
BOOL AM_CLU862886673_unbox(AM_CLUSTER_EXPR);
BOOL AM_CON174643192_unbox(AM_CONST);
BOOL AM_CON357292637_unbox(AM_CONST, OB);
BOOL AM_CUR1683770295_unbox(AM_CURSOR, OB);
BOOL AM_CUR217486862_unbox(AM_CURSOR_POS, OB);
BOOL AM_EXC219044661_unbox(AM_EXCEPT_EXPR);
BOOL AM_EXC30103808_unbox(AM_EXCEPT_EXPR, OB);
BOOL AM_EXT1010597579_unbox(AM_EXT_CALL_EXPR);
BOOL AM_EXT1489112584_unbox(AM_EXT_CALL_EXPR, OB);
BOOL AM_FAR1472604699_unbox(AM_FAR_EXPR, OB);
BOOL AM_FAR833547686_unbox(AM_FAR_EXPR);
BOOL AM_FLT1011996250_unbox(AM_FLTI_CONST, OB);
BOOL AM_FLT1108482895_unbox(AM_FLTX_CONST, OB);
BOOL AM_FLT1144488899_unbox(AM_FLTD_CONST, OB);
BOOL AM_FLT1356295273_unbox(AM_FLT_CONST);
BOOL AM_FLT1602735735_unbox(AM_FLTDX_CONST, OB);
BOOL AM_FLT1739286436_unbox(AM_FLT_CONST, OB);
BOOL AM_FLT1865244436_unbox(AM_FLTDX_CONST);
BOOL AM_FLT652620710_unbox(AM_FLTX_CONST);
BOOL AM_FLT840430235_unbox(AM_FLTI_CONST);
BOOL AM_FLT93541882_unbox(AM_FLTD_CONST);
BOOL AM_GLO1300832959_unbox(AM_GLOBAL_EXPR);
BOOL AM_GLO286344550_unbox(AM_GLOBAL_EXPR, OB);
BOOL AM_HER1213808373_unbox(AM_HERE_EXPR);
BOOL AM_HER1833766706_unbox(AM_HERE_EXPR, OB);
BOOL AM_IF_2781110_unbox(AM_IF_EXPR, OB);
BOOL AM_IF_792326817_unbox(AM_IF_EXPR);
BOOL AM_INT1418540518_unbox(AM_INT_CONST);
BOOL AM_INT1567340277_unbox(AM_INT_CONST, OB);
BOOL AM_INT341842242_unbox(AM_INTI_CONST);
BOOL AM_INT586605643_unbox(AM_INTI_CONST, OB);
BOOL AM_IS_1001428606_unbox(AM_IS_VOID_EXPR);
BOOL AM_IS_997133203_unbox(AM_IS_VOID_EXPR, OB);
BOOL AM_ITE1429510983_unbox(AM_ITE1809135850);
BOOL AM_ITE1901971608_unbox(AM_ITE1809135850, OB);
BOOL AM_LOC2055616863_unbox(AM_LOCAL_EXPR);
BOOL AM_LOC783953200_unbox(AM_LOCAL_EXPR, OB);
BOOL AM_NEA1492883953_unbox(AM_NEAR_EXPR);
BOOL AM_NEA499164018_unbox(AM_NEAR_EXPR, OB);
BOOL AM_NEW1485150288_unbox(AM_NEW_EXPR, OB);
BOOL AM_NEW952487097_unbox(AM_NEW_EXPR);
BOOL AM_ROU2096608001_unbox(AM_ROU1916046290);
BOOL AM_ROU774362080_unbox(AM_ROU1916046290, OB);
BOOL AM_SHA1922018909_unbox(AM_SHARED_EXPR);
BOOL AM_SHA623484008_unbox(AM_SHARED_EXPR, OB);
BOOL AM_STM214668158_unbox(AM_STMT_EXPR, OB);
BOOL AM_STM552683579_unbox(AM_STMT_EXPR);
BOOL AM_STR1095405116_unbox(AM_STR_CONST);
BOOL AM_STR2039863889_unbox(AM_STR_CONST, OB);
BOOL AM_VAR1818998713_unbox(AM_VAR744470097, OB);
BOOL AM_VAR394665606_unbox(AM_VAR744470097);
BOOL AM_VAT2140838301_unbox(AM_VAT319982528, OB);
BOOL AM_VAT239070856_unbox(AM_VAT319982528);
BOOL AM_VOI1214668736_unbox(AM_VOID_CONST);
BOOL AM_VOI2144357369_unbox(AM_VOID_CONST, OB);
BOOL AM_WHE332796028_unbox(AM_WHERE_EXPR, OB);
BOOL AM_WHE420295477_unbox(AM_WHERE_EXPR);
BOOL ARRAY21272457581_unbox(dELTBO582795216_frame);
void* ARRAY21272457581_frame_alloc();
BOOL ARRAYB1390856792_unbox(dELTBO582795216_frame);
void* ARRAYB1390856792_frame_alloc();
BOOL AS_ASS1377838817_unbox(AS_ASSERT_STMT);
BOOL AS_ASS1755466016_unbox(AS_ASSIGN_STMT);
BOOL AS_ATT1182933590_unbox(AS_ATTACH_STMT);
BOOL AS_ATT2034251932_unbox(AS_ATTR_DEF);
BOOL AS_CAS964680792_unbox(AS_CASE_STMT);
BOOL AS_CON1915749157_unbox(AS_CONST_DEF);
BOOL AS_DEC2043607416_unbox(AS_DEC_STMT);
BOOL AS_EXP1274701909_unbox(AS_EXPR_STMT);
BOOL AS_FOR1720691150_unbox(AS_FORK_STMT);
BOOL AS_IF_117934348_unbox(AS_IF_STMT);
BOOL AS_INC827145130_unbox(AS_INC1416917001);
BOOL AS_INT130358855_unbox(AS_INT2031239268);
BOOL AS_LOC802751445_unbox(AS_LOCK_STMT);
BOOL AS_LOO612242568_unbox(AS_LOOP_STMT);
BOOL AS_PAR818790093_unbox(AS_PAR_STMT);
BOOL AS_PRO122180587_unbox(AS_PROTECT_STMT);
BOOL AS_QUI1489742669_unbox(AS_QUIT_STMT);
BOOL AS_RAI201810971_unbox(AS_RAISE_STMT);
BOOL AS_RET863344353_unbox(AS_RETURN_STMT);
BOOL AS_ROU1543294757_unbox(AS_ROUT_DEF);
BOOL AS_SHA952781759_unbox(AS_SHARED_DEF);
BOOL AS_STM1645255095_unbox(AS_STMT);
BOOL AS_SYN1201341531_unbox(AS_SYNC_STMT);
BOOL AS_TYP1265860748_unbox(AS_TYPECASE_STMT);
BOOL AS_UNL1126435291_unbox(AS_UNLOCK_STMT);
BOOL AS_WIT1566004798_unbox(AS_WIT1063437351);
BOOL AS_YIE1515612652_unbox(AS_YIELD_STMT);
BOOL ATTRSO1207195602_unbox(ATTRSORT, OB);
BOOL BOOL_i1290536107_unbox(BOOL_boxed, OB);
BOOL CALL_T148273370_unbox(CALL_T561792367, dTP);
BOOL CALL_T16429326_unbox(CALL_TP_ARRAY, dTP);
BOOL CALL_T1680302827_unbox(CALL_TP_VOID, dTP);
BOOL CALL_T1822200530_unbox(CALL_T666389079, dTP);
BOOL CALL_T640058966_unbox(CALL_TP_CREATE, dTP);
BOOL CHAR_i1237803373_unbox(CHAR_boxed, OB);
BOOL CSE_DO4156401_unbox(CSE_DOUBLE_EXPRS, OB);
BOOL FLISTB1880887881_unbox(dELTBO582795216_frame);
void* FLISTB1880887881_frame_alloc();
BOOL FLTD_i1285721587_unbox(FLTD_boxed, OB);
BOOL FLTD_is_nilrBOOL_unbox(FLTD_boxed);
BOOL FLT_is1351417193_unbox(FLT_boxed, OB);
BOOL FLT_is_nilrBOOL_unbox(FLT_boxed);
BOOL FSTR_i135169312_unbox(FSTR, OB);
BOOL IDENT_1780939044_unbox(IDENT_boxed, OB);
BOOL INLINE1554137872_unbox(INLINE_ROUT);
BOOL INLINE1676073295_unbox(INLINE_ATTR_READ);
BOOL INLINE1738748633_unbox(INLINE1164904870);
BOOL INLINE1799378917_unbox(INLINE_INT_FOLD);
BOOL INLINE182457744_unbox(INLINE1596311811);
BOOL INLINE182665064_unbox(INLINE_ITER);
BOOL INLINE600822962_unbox(INLINE1433668901);
BOOL INLINE76438420_unbox(INLINE, SIG);
BOOL INLINE862622371_unbox(INLINE1455768374);
BOOL INOUT_1534195007_unbox(INOUT_MODE);
BOOL INOUT_394279242_unbox(INOUT_MODE, dMODE);
BOOL INTI_i1060921131_unbox(INTI1, OB);
BOOL INT_is240310584_unbox(INT_boxed, OB);
BOOL INT_is_nilrBOOL_unbox(INT_boxed);
BOOL IN_MOD1604254699_unbox(IN_MODE, dMODE);
BOOL IN_MOD851634165_unbox(IN_MODE);
BOOL ONCE_M1658631798_unbox(ONCE_MODE, dMODE);
BOOL ONCE_M1884799084_unbox(ONCE_MODE);
BOOL OUT_MO1200475098_unbox(OUT_MODE, dMODE);
BOOL OUT_MO898624407_unbox(OUT_MODE);
BOOL RAT_is1975945358_unbox(RAT_boxed, OB);
BOOL SFILE_947438414_unbox(SFILE_ID_boxed, OB);
BOOL SIDE_E1243435218_unbox(SIDE_EFFECT, OB);
BOOL SIG_is688012902_unbox(SIG, OB);
BOOL STR_is142167618_unbox(STR, OB);
BOOL TP_CLA1015994406_unbox(TP_CLASS);
BOOL TP_CLA1071798668_unbox(TP_CLASS);
BOOL TP_CLA1153639400_unbox(TP_CLASS);
BOOL TP_CLA1448883828_unbox(TP_CLASS);
BOOL TP_CLA1568240525_unbox(TP_CLASS);
BOOL TP_CLA1792166754_unbox(TP_CLASS);
BOOL TP_CLA254882760_unbox(TP_CLASS, OB);
BOOL TP_CLA272820496_unbox(TP_CLASS);
BOOL TP_CLA366982046_unbox(TP_CLASS);
BOOL TP_CLA430625769_unbox(TP_CLASS);
BOOL TP_CLA513015767_unbox(TP_CLASS, dTP);
BOOL TP_CLA650455767_unbox(TP_CLASS);
BOOL TP_CLA693899249_unbox(TP_CLASS, dTP);
BOOL TP_CLA727826289_unbox(TP_CLASS);
BOOL TP_ITE1212470094_unbox(TP_ITER);
BOOL TP_ITE1387653699_unbox(TP_ITER);
BOOL TP_ITE1549466955_unbox(TP_ITER);
BOOL TP_ITE1550467217_unbox(TP_ITER, dTP);
BOOL TP_ITE1606424066_unbox(TP_ITER);
BOOL TP_ITE1864681437_unbox(TP_ITER);
BOOL TP_ITE1879811989_unbox(TP_ITER);
BOOL TP_ITE1906602522_unbox(TP_ITER, dTP);
BOOL TP_ITE1921021286_unbox(TP_ITER, OB);
BOOL TP_ITE2052650588_unbox(TP_ITER);
BOOL TP_ITE2096764295_unbox(TP_ITER);
BOOL TP_ITE532005347_unbox(TP_ITER);
BOOL TP_ITE547568872_unbox(TP_ITER);
BOOL TP_ITE910939601_unbox(TP_ITER);
BOOL TP_ROU1058813051_unbox(TP_ROUT);
BOOL TP_ROU1099474123_unbox(TP_ROUT);
BOOL TP_ROU1105559654_unbox(TP_ROUT);
BOOL TP_ROU1673918401_unbox(TP_ROUT);
BOOL TP_ROU177994147_unbox(TP_ROUT);
BOOL TP_ROU1849661410_unbox(TP_ROUT, OB);
BOOL TP_ROU1919694918_unbox(TP_ROUT);
BOOL TP_ROU460533207_unbox(TP_ROUT);
BOOL TP_ROU681177780_unbox(TP_ROUT);
BOOL TP_ROU68166391_unbox(TP_ROUT, dTP);
BOOL TP_ROU699367341_unbox(TP_ROUT);
BOOL TP_ROU730462331_unbox(TP_ROUT);
BOOL TP_ROU835748448_unbox(TP_ROUT);
BOOL TP_ROU967846898_unbox(TP_ROUT, dTP);
BOOL TUPAM_1144039098_unbox(TUPAM_1024994801_boxed, OB);
BOOL TUPAM_1196406350_unbox(TUPAM_504653531_boxed, OB);
BOOL TUPAM_1557192709_unbox(TUPAM_1519040877_boxed, OB);
BOOL TUPAM_1711472971_unbox(TUPAM_653469574_boxed, OB);
BOOL TUPAM_1783991982_unbox(TUPAM_927437481_boxed, OB);
BOOL TUPAM_2013149714_unbox(TUPAM_304458649_boxed, OB);
BOOL TUPAM_2045709861_unbox(TUPAM_667892060_boxed, OB);
BOOL TUPARR1107471104_unbox(TUPARRAYARGdTP_boxed, OB);
BOOL TUPARR1838121920_unbox(TUPARR1759987776_boxed, OB);
BOOL TUPBOO323223880_unbox(TUPBOO1392923241_boxed, OB);
BOOL TUPIDE1222548283_unbox(TUPIDE641654124_boxed, OB);
BOOL TUPIDE1955445151_unbox(TUPIDENTARRAYdTP_boxed, OB);
BOOL TUPIDE429159832_unbox(TUPIDENTdTP_boxed, OB);
BOOL TUPIDE53463463_unbox(TUPIDENTINT_boxed, OB);
BOOL TUPINT326447217_unbox(TUPINTINT_boxed, OB);
BOOL TUPSIG116886773_unbox(TUPSIGSTR_boxed, OB);
BOOL TUPSIG1705966004_unbox(TUPSIG1754078736_boxed, OB);
BOOL TUPSIG174310016_unbox(TUPSIG2023533247_boxed, OB);
BOOL TUPSIG215029739_unbox(TUPSIGINT_boxed, OB);
BOOL TUPSIG662732057_unbox(TUPSIGSIG_boxed, OB);
BOOL TUPSIG738670088_unbox(TUPSIGARG_LAYOUT_boxed, OB);
BOOL TUPSIG848243115_unbox(TUPSIG578692189_boxed, OB);
BOOL TUPSTR1375565929_unbox(TUPSTRSTR_boxed, OB);
BOOL TUPSTR1473708895_unbox(TUPSTRINT_boxed, OB);
BOOL TUPSTR1795207828_unbox(TUPSTRdCONFIG_boxed, OB);
BOOL TUPSTR1921411213_unbox(TUPSTRSIG_boxed, OB);
BOOL TUPSTR850883487_unbox(TUPSTRFSETSTR_boxed, OB);
BOOL TUPTP_2145724346_unbox(TUPTP_858321267_boxed, OB);
BOOL TUPTP_744064396_unbox(TUPTP_1668454172_boxed, OB);
BOOL TUPdAM1479867_unbox(TUPdAM_EXPRdTP_boxed, OB);
BOOL TUPdAS1082207056_unbox(TUPdAS268679779_boxed, OB);
BOOL TUPdOB134201346_unbox(TUPdOBNAMESPACE_boxed, OB);
BOOL TUPdOB398929495_unbox(TUPdOBSTR_boxed, OB);
BOOL TUPdTP1282296289_unbox(TUPdTP1710233051_boxed, OB);
BOOL TUPdTP1433052028_unbox(TUPdTPINT_boxed, OB);
BOOL TUPdTP1932220599_unbox(TUPdTPdLAYOUT_boxed, OB);
BOOL TUPdTP1961471532_unbox(TUPdTPCODE_FILE_boxed, OB);
BOUND_2140373124 ARRAYB1475407144_unbox(dELTBO1349583384_frame);
void* ARRAYB1475407144_frame_alloc();
BOUND_2140373124 FLISTB455167321_unbox(dELTBO1349583384_frame);
void* FLISTB455167321_frame_alloc();
BOUND_2140373124 FSETBO677183779_unbox(dELTBO1349583384_frame);
void* FSETBO677183779_frame_alloc();
BOUND_809390774 ARRAYB1420960227_unbox(dELTBO879923169_frame);
void* ARRAYB1420960227_frame_alloc();
BOUND_809390774 FLISTB905996620_unbox(dELTBO879923169_frame);
void* FLISTB905996620_frame_alloc();
BOUND_809390774 FSETBO10998148_unbox(dELTBO879923169_frame);
void* FSETBO10998148_frame_alloc();
BUILTIN_LAYOUT ARRAYB1615560602_unbox(dELTBU334182326_frame);
void* ARRAYB1615560602_frame_alloc();
BUILTIN_LAYOUT FLISTB334826071_unbox(dELTBU334182326_frame);
void* FLISTB334826071_frame_alloc();
BUILTIN_LAYOUT FSETBU202043707_unbox(dELTBU334182326_frame);
void* FSETBU202043707_frame_alloc();
CALL_ARG ARRAYC494195798_unbox(dELTCA1528219566_frame);
void* ARRAYC494195798_frame_alloc();
CALL_ARG FLISTC264978409_unbox(dELTCA1528219566_frame);
void* FLISTC264978409_frame_alloc();
CHAR ARRAYC1494984744_unbox(dELTCH686923168_frame);
void* ARRAYC1494984744_frame_alloc();
CHAR FLISTC1776759929_unbox(dELTCH686923168_frame);
void* FLISTC1776759929_frame_alloc();
CHAR STR_eltbrCHAR_unbox(dELTCH686923168_frame);
void* STR_eltbrCHAR_frame_alloc();
CLASS_LAYOUT ARRAYC818595214_unbox(dELTCL1056825190_frame);
void* ARRAYC818595214_frame_alloc();
CLASS_LAYOUT FLISTC519862611_unbox(dELTCL1056825190_frame);
void* FLISTC519862611_frame_alloc();
CLASS_LAYOUT FSETCL1233705521_unbox(dELTCL1056825190_frame);
void* FSETCL1233705521_frame_alloc();
CODE_FILE ARRAYC1531429675_unbox(dELTCO1151854791_frame);
void* ARRAYC1531429675_frame_alloc();
CODE_FILE FLISTC1947519404_unbox(dELTCO1151854791_frame);
void* FLISTC1947519404_frame_alloc();
CSE_DOUBLE_EXPRS ARRAYC1440407068_unbox(dELTCS742343900_frame);
void* ARRAYC1440407068_frame_alloc();
CSE_DOUBLE_EXPRS FLISTC1263372315_unbox(dELTCS742343900_frame);
void* FLISTC1263372315_frame_alloc();
CSE_DOUBLE_EXPRS FSETCS1170733135_unbox(dELTCS742343900_frame);
void* FSETCS1170733135_frame_alloc();
EXPR_HOISTED ARRAYE1693383532_unbox(dELTEX1931613508_frame);
void* ARRAYE1693383532_frame_alloc();
EXPR_HOISTED FLISTE354925707_unbox(dELTEX1931613508_frame);
void* FLISTE354925707_frame_alloc();
EXTERNAL_LAYOUT ARRAYE1687022731_unbox(dELTEX2090504649_frame);
void* ARRAYE1687022731_frame_alloc();
EXTERNAL_LAYOUT FLISTE1972070322_unbox(dELTEX2090504649_frame);
void* FLISTE1972070322_frame_alloc();
EXTERNAL_LAYOUT FSETEX1633103446_unbox(dELTEX2090504649_frame);
void* FSETEX1633103446_frame_alloc();
FLISTA725283029 AM_ANY1678367917_unbox(AM_ANY_EXPR);
FLISTA725283029 AM_ARR1270029884_unbox(AM_ARR_CONST);
FLISTA725283029 AM_ARR1917250979_unbox(AM_ARRAY_EXPR);
FLISTA725283029 AM_ATT2085324530_unbox(AM_ATTR_EXPR);
FLISTA725283029 AM_AT_726422231_unbox(AM_AT_EXPR);
FLISTA725283029 AM_BND1015009410_unbox(AM_BND260301329);
FLISTA725283029 AM_BND1086659687_unbox(AM_BND1124877163);
FLISTA725283029 AM_BND776534650_unbox(AM_BND367211769);
FLISTA725283029 AM_BOO1622702250_unbox(AM_BOOL_CONST);
FLISTA725283029 AM_CHA722016210_unbox(AM_CHAR_CONST);
FLISTA725283029 AM_CLU1812304013_unbox(AM_CLUSTER_EXPR);
FLISTA725283029 AM_CLU39726141_unbox(AM_CLU1286269335);
FLISTA725283029 AM_CON19269408_unbox(AM_CONST);
FLISTA725283029 AM_EXC1481209187_unbox(AM_EXCEPT_EXPR);
FLISTA725283029 AM_EXT908131195_unbox(AM_EXT_CALL_EXPR);
FLISTA725283029 AM_FAR276305130_unbox(AM_FAR_EXPR);
FLISTA725283029 AM_FLT1309796914_unbox(AM_FLTD_CONST);
FLISTA725283029 AM_FLT1728105754_unbox(AM_FLTX_CONST);
FLISTA725283029 AM_FLT1832811964_unbox(AM_FLTDX_CONST);
FLISTA725283029 AM_FLT523420577_unbox(AM_FLTI_CONST);
FLISTA725283029 AM_FLT945201335_unbox(AM_FLT_CONST);
FLISTA725283029 AM_GLO404411817_unbox(AM_GLOBAL_EXPR);
FLISTA725283029 AM_HER1483923803_unbox(AM_HERE_EXPR);
FLISTA725283029 AM_IF_1267236903_unbox(AM_IF_EXPR);
FLISTA725283029 AM_INT1223291058_unbox(AM_INT_CONST);
FLISTA725283029 AM_INT2023229226_unbox(AM_INTI_CONST);
FLISTA725283029 AM_IS_1111682258_unbox(AM_IS_VOID_EXPR);
FLISTA725283029 AM_ITE757218595_unbox(AM_ITE1809135850);
FLISTA725283029 AM_LOC1121251403_unbox(AM_LOCAL_EXPR);
FLISTA725283029 AM_NEA1654213825_unbox(AM_NEAR_EXPR);
FLISTA725283029 AM_NEW1679302875_unbox(AM_NEW_EXPR);
FLISTA725283029 AM_ROU518743835_unbox(AM_ROU1916046290);
FLISTA725283029 AM_SHA1496045189_unbox(AM_SHARED_EXPR);
FLISTA725283029 AM_STM1543836917_unbox(AM_STMT_EXPR);
FLISTA725283029 AM_STR201189492_unbox(AM_STR_CONST);
FLISTA725283029 AM_VAR567579326_unbox(AM_VAR744470097);
FLISTA725283029 AM_VAT467614272_unbox(AM_VAT319982528);
FLISTA725283029 AM_VOI402497764_unbox(AM_VOID_CONST);
FLISTA725283029 AM_WHE1419632471_unbox(AM_WHERE_EXPR);
FLISTSTR ARRAYF1006928322_unbox(dELTFL27095446_frame);
void* ARRAYF1006928322_frame_alloc();
FLISTSTR FLISTF1766102529_unbox(dELTFL27095446_frame);
void* FLISTF1766102529_frame_alloc();
FLISTdAM_CONST ARRAYF1657157762_unbox(dELTFL688066606_frame);
void* ARRAYF1657157762_frame_alloc();
FLISTdAM_CONST FLISTF687422861_unbox(dELTFL688066606_frame);
void* FLISTF687422861_frame_alloc();
FLISTdLAYOUT ABSTRA1868518641_unbox(ABSTRACT_LAYOUT);
FLISTdLAYOUT ABSTRA1904387652_unbox(ABSTRA151498155);
FLISTdLAYOUT ARG_LA836569915_unbox(ARG_LAYOUT);
FLISTdLAYOUT BOUND_1150883246_unbox(BOUND_1007407331);
FLISTdLAYOUT BOUND_1157561903_unbox(BOUND_2140373124);
FLISTdLAYOUT BOUND_1585387107_unbox(BOUND_1677815802);
FLISTdLAYOUT BOUND_220914967_unbox(BOUND_809390774);
FLISTdLAYOUT BUILTI1794609038_unbox(BUILTIN_LAYOUT);
FLISTdLAYOUT CLASS_1345471732_unbox(CLASS_LAYOUT);
FLISTdLAYOUT EXTERN1033411318_unbox(EXTERNAL_LAYOUT);
FLISTdLAYOUT FRAME_31536271_unbox(FRAME_LAYOUT);
FLISTdLAYOUT IMMUTA414538126_unbox(IMMUTA421759893);
FLISTdLAYOUT TP_LAY265125546_unbox(TP_LAYOUT);
FSETSIG GENERA1281179458_unbox(GENERATE_AM);
FSTR ARRAYF254484098_unbox(dELTFS553577478_frame);
void* ARRAYF254484098_frame_alloc();
FSTR FLISTF1277706721_unbox(dELTFS553577478_frame);
void* FLISTF1277706721_frame_alloc();
IDENT ARRAYI1725945879_unbox(dELTID1378398307_frame);
void* ARRAYI1725945879_frame_alloc();
IDENT FLISTI1709766432_unbox(dELTID1378398307_frame);
void* FLISTI1709766432_frame_alloc();
IFC ARRAYI2003604369_unbox(dELTIFC_eltbrIFC_frame);
void* ARRAYI2003604369_frame_alloc();
IFC FLISTI1596257802_unbox(dELTIFC_eltbrIFC_frame);
void* FLISTI1596257802_frame_alloc();
IFC TP_CLASS_ifcrIFC_unbox(TP_CLASS);
IFC TP_ITER_ifcrIFC_unbox(TP_ITER);
IFC TP_ROUT_ifcrIFC_unbox(TP_ROUT);
IMMUTA421759893 ARRAYI1264630278_unbox(dELTIM1138806518_frame);
void* ARRAYI1264630278_frame_alloc();
IMMUTA421759893 FLISTI244390455_unbox(dELTIM1138806518_frame);
void* FLISTI244390455_frame_alloc();
IMMUTA421759893 FSETIM887960645_unbox(dELTIM1138806518_frame);
void* FSETIM887960645_frame_alloc();
IMPL TP_CLA1109727233_unbox(TP_CLASS);
IMPL TP_ITE830569150_unbox(TP_ITER);
IMPL TP_ROU723658710_unbox(TP_ROUT);
IMPL_INCLUDE ARRAYI1719886982_unbox(dELTIM1958116958_frame);
void* ARRAYI1719886982_frame_alloc();
IMPL_INCLUDE FLISTI381429157_unbox(dELTIM1958116958_frame);
void* FLISTI381429157_frame_alloc();
INT AM_ANY1965823461_unbox(AM_ANY_EXPR);
INT AM_ARR1424607794_unbox(AM_ARR_CONST);
INT AM_ARR191206921_unbox(AM_ARRAY_EXPR);
INT AM_ATT178000012_unbox(AM_ATTR_EXPR);
INT AM_AT_373259037_unbox(AM_AT_EXPR);
INT AM_BND1144165392_unbox(AM_BND260301329);
INT AM_BND1520373100_unbox(AM_BND367211769);
INT AM_BND1835838572_unbox(AM_BND260301329);
INT AM_BND2097777224_unbox(AM_BND367211769);
INT AM_BND472678797_unbox(AM_BND1124877163);
INT AM_BOO561475672_unbox(AM_BOOL_CONST);
INT AM_CHA1966863808_unbox(AM_CHAR_CONST);
INT AM_CLU772455521_unbox(AM_CLU1286269335);
INT AM_CLU978087499_unbox(AM_CLUSTER_EXPR);
INT AM_CON2051977914_unbox(AM_CONST);
INT AM_EXC800301049_unbox(AM_EXCEPT_EXPR);
INT AM_EXT2080723118_unbox(AM_EXT_CALL_EXPR);
INT AM_EXT644325801_unbox(AM_EXT_CALL_EXPR);
INT AM_FAR2123150608_unbox(AM_FAR_EXPR);
INT AM_FLT1054767436_unbox(AM_FLTX_CONST);
INT AM_FLT153804005_unbox(AM_FLTI_CONST);
INT AM_FLT465909163_unbox(AM_FLT_CONST);
INT AM_FLT556661152_unbox(AM_FLTD_CONST);
INT AM_FLT669520374_unbox(AM_FLTDX_CONST);
INT AM_GLO57248015_unbox(AM_GLOBAL_EXPR);
INT AM_HER774694957_unbox(AM_HERE_EXPR);
INT AM_IF_1573308145_unbox(AM_IF_EXPR);
INT AM_INT2057636940_unbox(AM_INT_CONST);
INT AM_INT331860696_unbox(AM_INTI_CONST);
INT AM_IS_128338200_unbox(AM_IS_VOID_EXPR);
INT AM_ITE1021507011_unbox(AM_ITE1809135850);
INT AM_ITE494670743_unbox(AM_ITE1809135850);
INT AM_LOC505223029_unbox(AM_LOCAL_EXPR);
INT AM_NEA1314295025_unbox(AM_NEAR_EXPR);
INT AM_NEW1014811437_unbox(AM_NEW_EXPR);
INT AM_ROU1433426367_unbox(AM_ROU1916046290);
INT AM_ROU67895179_unbox(AM_ROU1916046290);
INT AM_SHA1840537425_unbox(AM_SHARED_EXPR);
INT AM_STM804699229_unbox(AM_STMT_EXPR);
INT AM_STR1959493974_unbox(AM_STR_CONST);
INT AM_VAR2089448132_unbox(AM_VAR744470097);
INT AM_VAT495261450_unbox(AM_VAT319982528);
INT AM_VOI822416442_unbox(AM_VOID_CONST);
INT AM_WHE1598364639_unbox(AM_WHERE_EXPR);
INT ARRAYI69049459_unbox(dELTINT_eltbrINT_frame);
void* ARRAYI69049459_frame_alloc();
INT AS_AND1958569903_unbox(AS_AND_EXPR);
INT AS_ANY1507835814_unbox(AS_ANY_EXPR);
INT AS_ARR1547513756_unbox(AS_ARRAY_EXPR);
INT AS_AT_355764446_unbox(AS_AT_EXPR);
INT AS_BOO711205340_unbox(AS_BOOL_LIT_EXPR);
INT AS_BOU1634505419_unbox(AS_BOU14269336);
INT AS_BRE836708546_unbox(AS_BREAK_EXPR);
INT AS_CAL755875642_unbox(AS_CALL_EXPR);
INT AS_CHA1908653268_unbox(AS_CHAR_LIT_EXPR);
INT AS_CLU1234120202_unbox(AS_CLUSTER_EXPR);
INT AS_CLU889538930_unbox(AS_CLU553570599);
INT AS_COH1949131218_unbox(AS_COHORT_EXPR);
INT AS_CRE1921695223_unbox(AS_CREATE_EXPR);
INT AS_EXC629960794_unbox(AS_EXCEPT_EXPR);
INT AS_FAR1350508667_unbox(AS_FAR_EXPR);
INT AS_FLT374280066_unbox(AS_FLT_LIT_EXPR);
INT AS_GLO113092240_unbox(AS_GLOBAL_EXPR);
INT AS_HER1196006492_unbox(AS_HERE_EXPR);
INT AS_INI414727320_unbox(AS_INITIAL_EXPR);
INT AS_INT228236077_unbox(AS_INT_LIT_EXPR);
INT AS_IS_1954421395_unbox(AS_IS_VOID_EXPR);
INT AS_NEA1735606560_unbox(AS_NEAR_EXPR);
INT AS_NEW1836119458_unbox(AS_NEW_EXPR);
INT AS_OR_1113601798_unbox(AS_OR_EXPR);
INT AS_RES32147922_unbox(AS_RESULT_EXPR);
INT AS_SEL1486205964_unbox(AS_SELF_EXPR);
INT AS_STR1375497755_unbox(AS_STR_LIT_EXPR);
INT AS_UND1736379304_unbox(AS_UND152986614);
INT AS_VOI1355443380_unbox(AS_VOID_EXPR);
INT AS_WHE140356038_unbox(AS_WHERE_EXPR);
INT CHAR_hashrINT_unbox(CHAR_boxed);
INT CSE_DO1342102696_unbox(CSE_DOUBLE_EXPRS);
INT FLISTI476396026_unbox(dELTINT_eltbrINT_frame);
void* FLISTI476396026_frame_alloc();
INT FLTD_hashrINT_unbox(FLTD_boxed);
INT FLT_hashrINT_unbox(FLT_boxed);
INT FSTR_hashrINT_unbox(FSTR);
INT IDENT_hashrINT_unbox(IDENT_boxed);
INT INTI_hashrINT_unbox(INTI1);
INT INT_hashrINT_unbox(INT_boxed);
INT SFILE_29532291_unbox(SFILE_ID_boxed);
INT SIDE_E484651583_unbox(SIDE_EFFECT);
INT SIG_hashrINT_unbox(SIG);
INT STR_hashrINT_unbox(STR);
INT TP_CLA1653846135_unbox(TP_CLASS);
INT TP_CLA619166775_unbox(TP_CLASS);
INT TP_ITER_hashrINT_unbox(TP_ITER);
INT TP_ITER_kindrINT_unbox(TP_ITER);
INT TP_ROUT_hashrINT_unbox(TP_ROUT);
INT TP_ROUT_kindrINT_unbox(TP_ROUT);
INT TUPAM_1208907315_unbox(TUPAM_927437481_boxed);
INT TUPAM_1395325321_unbox(TUPAM_504653531_boxed);
INT TUPAM_1877096499_unbox(TUPAM_304458649_boxed);
INT TUPAM_247029134_unbox(TUPAM_1519040877_boxed);
INT TUPAM_460226510_unbox(TUPAM_667892060_boxed);
INT TUPAM_519070420_unbox(TUPAM_653469574_boxed);
INT TUPAM_755257033_unbox(TUPAM_1024994801_boxed);
INT TUPARR1458356743_unbox(TUPARRAYARGdTP_boxed);
INT TUPARR826863757_unbox(TUPARR1759987776_boxed);
INT TUPBOO1866184529_unbox(TUPBOO1392923241_boxed);
INT TUPIDE1936372555_unbox(TUPIDENTdTP_boxed);
INT TUPIDE274165750_unbox(TUPIDENTARRAYdTP_boxed);
INT TUPIDE280438974_unbox(TUPIDENTINT_boxed);
INT TUPIDE88035988_unbox(TUPIDE641654124_boxed);
INT TUPINT283365700_unbox(TUPINTINT_boxed);
INT TUPSIG1080413784_unbox(TUPSIG578692189_boxed);
INT TUPSIG1100158695_unbox(TUPSIGARG_LAYOUT_boxed);
INT TUPSIG1160082702_unbox(TUPSIGINT_boxed);
INT TUPSIG1486928152_unbox(TUPSIGSTR_boxed);
INT TUPSIG1584335572_unbox(TUPSIGSIG_boxed);
INT TUPSIG2024347575_unbox(TUPSIG2023533247_boxed);
INT TUPSIG524761103_unbox(TUPSIG1754078736_boxed);
INT TUPSTR1611378322_unbox(TUPSTRFSETSTR_boxed);
INT TUPSTR737321694_unbox(TUPSTRINT_boxed);
INT TUPSTR813227328_unbox(TUPSTRSIG_boxed);
INT TUPSTR864810129_unbox(TUPSTRdCONFIG_boxed);
INT TUPSTR910634748_unbox(TUPSTRSTR_boxed);
INT TUPTP_108406739_unbox(TUPTP_858321267_boxed);
INT TUPTP_385216441_unbox(TUPTP_1668454172_boxed);
INT TUPdAM1873776308_unbox(TUPdAM_EXPRdTP_boxed);
INT TUPdAS472200087_unbox(TUPdAS268679779_boxed);
INT TUPdOB400520666_unbox(TUPdOBSTR_boxed);
INT TUPdOB687982527_unbox(TUPdOBNAMESPACE_boxed);
INT TUPdTP1209415271_unbox(TUPdTPINT_boxed);
INT TUPdTP1263366620_unbox(TUPdTP1710233051_boxed);
INT TUPdTP1991179276_unbox(TUPdTPdLAYOUT_boxed);
INT TUPdTP386219079_unbox(TUPdTPCODE_FILE_boxed);
OPT_LOCAL_EXPR ARRAYO335724112_unbox(dELTOP2009500256_frame);
void* ARRAYO335724112_frame_alloc();
OPT_LOCAL_EXPR FLISTO2008856511_unbox(dELTOP2009500256_frame);
void* FLISTO2008856511_frame_alloc();
PROG TP_CLA736641047_unbox(TP_CLASS);
PROG TP_ITE1618029866_unbox(TP_ITER);
PROG TP_ROU1724940306_unbox(TP_ROUT);
SFILE_ID AM_ANY312639639_unbox(AM_ANY_EXPR);
SFILE_ID AM_ARR490952057_unbox(AM_ARR_CONST);
SFILE_ID AM_ARR592984289_unbox(AM_ARRAY_EXPR);
SFILE_ID AM_ASS1066541977_unbox(AM_ASSIGN_STMT);
SFILE_ID AM_ASS142463220_unbox(AM_ASSERT_STMT);
SFILE_ID AM_ATT1483547331_unbox(AM_ATTR_EXPR);
SFILE_ID AM_ATT2128304053_unbox(AM_ATTACH_STMT);
SFILE_ID AM_AT_336435086_unbox(AM_AT_EXPR);
SFILE_ID AM_BND1008256842_unbox(AM_BND260301329);
SFILE_ID AM_BND1697524322_unbox(AM_BND367211769);
SFILE_ID AM_BND1800984858_unbox(AM_BND1124877163);
SFILE_ID AM_BOO1043294506_unbox(AM_BOOL_CONST);
SFILE_ID AM_BRE475880308_unbox(AM_BREAK_STMT);
SFILE_ID AM_CAL1587798028_unbox(AM_CALL_ARG);
SFILE_ID AM_CAS881290947_unbox(AM_CASE_STMT);
SFILE_ID AM_CHA1773602482_unbox(AM_CHAR_CONST);
SFILE_ID AM_CLU1154173259_unbox(AM_CLUSTER_EXPR);
SFILE_ID AM_CLU2049939620_unbox(AM_CLU1286269335);
SFILE_ID AM_COM1046808385_unbox(AM_COMMENT_STMT);
SFILE_ID AM_CON810758495_unbox(AM_CONST);
SFILE_ID AM_EXC2040160518_unbox(AM_EXCEPT_EXPR);
SFILE_ID AM_EXP2073687788_unbox(AM_EXPR_STMT);
SFILE_ID AM_EXT840500856_unbox(AM_EXT_CALL_EXPR);
SFILE_ID AM_FAR526851136_unbox(AM_FAR_EXPR);
SFILE_ID AM_FLT1298836824_unbox(AM_FLT_CONST);
SFILE_ID AM_FLT1412381806_unbox(AM_FLTD_CONST);
SFILE_ID AM_FLT2070042319_unbox(AM_FLTDX_CONST);
SFILE_ID AM_FLT2096495882_unbox(AM_FLTX_CONST);
SFILE_ID AM_FLT564073323_unbox(AM_FLTI_CONST);
SFILE_ID AM_FOR1474728133_unbox(AM_FORK_STMT);
SFILE_ID AM_FOR97554330_unbox(AM_FORMAL_ARG);
SFILE_ID AM_GLO537255272_unbox(AM_GLOBAL_EXPR);
SFILE_ID AM_HER1201005342_unbox(AM_HERE_EXPR);
SFILE_ID AM_IF_1192492569_unbox(AM_IF_STMT);
SFILE_ID AM_IF_1629993260_unbox(AM_IF_EXPR);
SFILE_ID AM_INI1792787320_unbox(AM_INITIAL_STMT);
SFILE_ID AM_INT1397528579_unbox(AM_INT_CONST);
SFILE_ID AM_INT872738218_unbox(AM_INTI_CONST);
SFILE_ID AM_INV963567817_unbox(AM_INV1167837230);
SFILE_ID AM_IS_82827228_unbox(AM_IS_VOID_EXPR);
SFILE_ID AM_ITE834410629_unbox(AM_ITE1809135850);
SFILE_ID AM_LOC327374505_unbox(AM_LOCAL_EXPR);
SFILE_ID AM_LOC748262786_unbox(AM_LOCK_STMT);
SFILE_ID AM_LOO738236003_unbox(AM_LOOP_STMT);
SFILE_ID AM_NEA1717951158_unbox(AM_NEAR_EXPR);
SFILE_ID AM_NEW765217327_unbox(AM_NEW_EXPR);
SFILE_ID AM_OB_1633498909_unbox(AM_OB_DEF);
SFILE_ID AM_PAR1239764599_unbox(AM_PAR_STMT);
SFILE_ID AM_POS2123304705_unbox(AM_POST_STMT);
SFILE_ID AM_PRE1797857661_unbox(AM_PRE_STMT);
SFILE_ID AM_PRE2094346420_unbox(AM_PREFETCH_STMT);
SFILE_ID AM_PRO1670847403_unbox(AM_PROTECT_STMT);
SFILE_ID AM_RAI1393475571_unbox(AM_RAISE_STMT);
SFILE_ID AM_RET702658942_unbox(AM_RETURN_STMT);
SFILE_ID AM_ROU145143149_unbox(AM_ROU1916046290);
SFILE_ID AM_ROU231894201_unbox(AM_ROUT_DEF);
SFILE_ID AM_SHA644128690_unbox(AM_SHARED_EXPR);
SFILE_ID AM_STM1466562094_unbox(AM_STMT_EXPR);
SFILE_ID AM_STR1580126161_unbox(AM_STR_CONST);
SFILE_ID AM_SYN416733298_unbox(AM_SYNC_STMT);
SFILE_ID AM_TYP720211797_unbox(AM_TYPECASE_STMT);
SFILE_ID AM_UNL1937638738_unbox(AM_UNLOCK_STMT);
SFILE_ID AM_VAR1884071700_unbox(AM_VAR744470097);
SFILE_ID AM_VAT338387515_unbox(AM_VAT319982528);
SFILE_ID AM_VOI405249940_unbox(AM_VOID_CONST);
SFILE_ID AM_WAI442200356_unbox(AM_WAITFOR_STMT);
SFILE_ID AM_WHE345322829_unbox(AM_WHERE_EXPR);
SFILE_ID AM_WIT1120935944_unbox(AM_WIT1996971603);
SFILE_ID AM_YIE1303084854_unbox(AM_YIELD_STMT);
SFILE_ID ARRAYS1372094358_unbox(dELTSF338070590_frame);
void* ARRAYS1372094358_frame_alloc();
SFILE_ID AS_AND749298802_unbox(AS_AND_EXPR);
SFILE_ID AS_ANY2075595295_unbox(AS_ANY_EXPR);
SFILE_ID AS_ARG265670232_unbox(AS_ARG_DEC);
SFILE_ID AS_ARR542631033_unbox(AS_ARRAY_EXPR);
SFILE_ID AS_ASS40608582_unbox(AS_ASSERT_STMT);
SFILE_ID AS_ASS964687339_unbox(AS_ASSIGN_STMT);
SFILE_ID AS_ATT1647669388_unbox(AS_ATTR_DEF);
SFILE_ID AS_ATT2064808605_unbox(AS_ATTACH_STMT);
SFILE_ID AS_AT_1597668772_unbox(AS_AT_EXPR);
SFILE_ID AS_BOO291537060_unbox(AS_BOOL_LIT_EXPR);
SFILE_ID AS_BOU1793757293_unbox(AS_BOU14269336);
SFILE_ID AS_BRE1173994939_unbox(AS_BREAK_EXPR);
SFILE_ID AS_CAL82770762_unbox(AS_CALL_EXPR);
SFILE_ID AS_CAS1215719733_unbox(AS_CASE_WHEN);
SFILE_ID AS_CAS986885563_unbox(AS_CASE_STMT);
SFILE_ID AS_CHA1542077308_unbox(AS_CHAR_LIT_EXPR);
SFILE_ID AS_CLA374248818_unbox(AS_CLASS_DEF);
SFILE_ID AS_CLU1205555915_unbox(AS_CLUSTER_EXPR);
SFILE_ID AS_CLU243953950_unbox(AS_CLU553570599);
SFILE_ID AS_COH931029852_unbox(AS_COHORT_EXPR);
SFILE_ID AS_CON1302703181_unbox(AS_CONST_DEF);
SFILE_ID AS_CRE1835137079_unbox(AS_CREATE_EXPR);
SFILE_ID AS_DEC1054605514_unbox(AS_DEC_STMT);
SFILE_ID AS_EXC2142015156_unbox(AS_EXCEPT_EXPR);
SFILE_ID AS_EXP353102998_unbox(AS_EXPR_STMT);
SFILE_ID AS_FAR1861383798_unbox(AS_FAR_EXPR);
SFILE_ID AS_FEA630208390_unbox(AS_FEAT_MOD);
SFILE_ID AS_FLT235098865_unbox(AS_FLT_LIT_EXPR);
SFILE_ID AS_FOR393448377_unbox(AS_FORK_STMT);
SFILE_ID AS_GLO435400634_unbox(AS_GLOBAL_EXPR);
SFILE_ID AS_HER667171168_unbox(AS_HERE_EXPR);
SFILE_ID AS_IDE48340397_unbox(AS_IDENT_LIST);
SFILE_ID AS_IF_1168370869_unbox(AS_IF_STMT);
SFILE_ID AS_INC214099154_unbox(AS_INC1416917001);
SFILE_ID AS_INI295049889_unbox(AS_INITIAL_EXPR);
SFILE_ID AS_INT1381353727_unbox(AS_INT2031239268);
SFILE_ID AS_INT6023736_unbox(AS_INT_LIT_EXPR);
SFILE_ID AS_IS_2018065350_unbox(AS_IS_VOID_EXPR);
SFILE_ID AS_LOC1119913724_unbox(AS_LOCK_STMT);
SFILE_ID AS_LOC1864862121_unbox(AS_LOCK_IF_WHEN);
SFILE_ID AS_LOO1129940507_unbox(AS_LOOP_STMT);
SFILE_ID AS_NEA708839628_unbox(AS_NEAR_EXPR);
SFILE_ID AS_NEW1623017607_unbox(AS_NEW_EXPR);
SFILE_ID AS_OR_1363025812_unbox(AS_OR_EXPR);
SFILE_ID AS_PAR158230952_unbox(AS_PARAM_DEC);
SFILE_ID AS_PAR666967763_unbox(AS_PAR_STMT);
SFILE_ID AS_PRO264390719_unbox(AS_PROTECT_STMT);
SFILE_ID AS_PRO493224889_unbox(AS_PROTECT_WHEN);
SFILE_ID AS_QUI1176752812_unbox(AS_QUIT_STMT);
SFILE_ID AS_RAI1765876403_unbox(AS_RAISE_STMT);
SFILE_ID AS_RES951018660_unbox(AS_RESULT_EXPR);
SFILE_ID AS_RET600804304_unbox(AS_RETURN_STMT);
SFILE_ID AS_ROU2138626563_unbox(AS_ROUT_DEF);
SFILE_ID AS_SEL957265064_unbox(AS_SELF_EXPR);
SFILE_ID AS_SHA1565827735_unbox(AS_SHARED_DEF);
SFILE_ID AS_STM1237803845_unbox(AS_STMT);
SFILE_ID AS_STM633666973_unbox(AS_STMT_LIST);
SFILE_ID AS_STR1687611758_unbox(AS_STR_LIT_EXPR);
SFILE_ID AS_SYN1451443212_unbox(AS_SYNC_STMT);
SFILE_ID AS_TYP1164969549_unbox(AS_TYPECASE_STMT);
SFILE_ID AS_TYP1615798168_unbox(AS_TYPE_SPEC);
SFILE_ID AS_TYP936135379_unbox(AS_TYPECASE_WHEN);
SFILE_ID AS_UND1334198639_unbox(AS_UND152986614);
SFILE_ID AS_UNL1835784100_unbox(AS_UNLOCK_STMT);
SFILE_ID AS_VOI1131745048_unbox(AS_VOID_EXPR);
SFILE_ID AS_WHE1480938151_unbox(AS_WHERE_EXPR);
SFILE_ID AS_WIT337771262_unbox(AS_WIT1063437351);
SFILE_ID AS_YIE1856267120_unbox(AS_YIELD_STMT);
SFILE_ID FLISTS2131268565_unbox(dELTSF338070590_frame);
void* FLISTS2131268565_frame_alloc();
SFILE_ID FSETSF139710295_unbox(dELTSF338070590_frame);
void* FSETSF139710295_frame_alloc();
SIDE_EFFECT ARRAYS2023743485_unbox(dELTSI583496193_frame);
void* ARRAYS2023743485_frame_alloc();
SIDE_EFFECT FLISTS1351415114_unbox(dELTSI583496193_frame);
void* FLISTS1351415114_frame_alloc();
SIDE_EFFECT FSETSI2093978094_unbox(dELTSI583496193_frame);
void* FSETSI2093978094_frame_alloc();
SIG ARRAYS1481566101_unbox(dELTSIG_eltbrSIG_frame);
void* ARRAYS1481566101_frame_alloc();
SIG FLISTS1074219534_unbox(dELTSIG_eltbrSIG_frame);
void* FLISTS1074219534_frame_alloc();
SIG FSETSIG_eltbrSIG_unbox(dELTSIG_eltbrSIG_frame);
void* FSETSIG_eltbrSIG_frame_alloc();
SIG GET_MA2025156715_unbox(GET_MAIN_SIG);
SIG INLINE1060094617_unbox(INLINE_ATTR_READ);
SIG INLINE1199394416_unbox(INLINE_ITER);
SIG INLINE1204485066_unbox(INLINE1433668901);
SIG INLINE1283359029_unbox(INLINE1455768374);
SIG INLINE1334459112_unbox(INLINE_ROUT);
SIG INLINE1360700008_unbox(INLINE1596311811);
SIG INLINE1848276099_unbox(INLINE_INT_FOLD);
SIG INLINE1854659937_unbox(INLINE1164904870);
STR ABSTRA12614027_unbox(ABSTRA151498155, CGEN);
STR ABSTRA1464296195_unbox(ABSTRACT_LAYOUT);
STR ABSTRA1984535400_unbox(ABSTRA151498155);
STR ABSTRA353314648_unbox(ABSTRACT_LAYOUT, CGEN);
STR ARG_LA635972259_unbox(ARG_LAYOUT);
STR ARG_LA807737867_unbox(ARG_LAYOUT, CGEN);
STR ARRAYA1329352756_unbox(ARRAYARRAYSTR);
STR ARRAYA1368915033_unbox(ARRAYAM_ROUT_DEF);
STR ARRAYA1490975097_unbox(ARRAYA1870808380);
STR ARRAYA1523920654_unbox(ARRAYA477251019);
STR ARRAYA1607624311_unbox(ARRAYATTRSORT);
STR ARRAYA1621913879_unbox(ARRAYA855692714);
STR ARRAYA175904446_unbox(ARRAYA1347292543);
STR ARRAYA1826145484_unbox(ARRAYAM_CALL_ARG);
STR ARRAYA1836477836_unbox(ARRAYAM_CURSOR);
STR ARRAYA208544369_unbox(ARRAYA1369744492);
STR ARRAYA449689863_unbox(ARRAYA1135548710);
STR ARRAYA56089049_unbox(ARRAYA36280886);
STR ARRAYA565856506_unbox(ARRAYAS_FEAT_MOD);
STR ARRAYA704584979_unbox(ARRAYA1575632848);
STR ARRAYA881422470_unbox(ARRAYARRAYBOOL);
STR ARRAYA92706474_unbox(ARRAYA132553625);
STR ARRAYARG_strrSTR_unbox(ARRAYARG);
STR ARRAYB1034562007_unbox(ARRAYB1039347268);
STR ARRAYB1997509424_unbox(ARRAYBOOL);
STR ARRAYB422115773_unbox(ARRAYB1925477728);
STR ARRAYB799780672_unbox(ARRAYB1234765325);
STR ARRAYC1188196456_unbox(ARRAYCHAR);
STR ARRAYC1509950809_unbox(ARRAYCODE_FILE);
STR ARRAYC15432117_unbox(ARRAYCALL_ARG);
STR ARRAYC1557608670_unbox(ARRAYC1788421899);
STR ARRAYC861356497_unbox(ARRAYC1438655838);
STR ARRAYE1256334724_unbox(ARRAYE406487263);
STR ARRAYE1673119298_unbox(ARRAYE2126408533);
STR ARRAYF1350792049_unbox(ARRAYFLISTSTR);
STR ARRAYF378827521_unbox(ARRAYFSTR);
STR ARRAYF852228085_unbox(ARRAYF1030817442);
STR ARRAYI1079246087_unbox(ARRAYI1327367692);
STR ARRAYI1551332689_unbox(ARRAYIDENT);
STR ARRAYI1980470469_unbox(ARRAYI1991954990);
STR ARRAYIFC_strrSTR_unbox(ARRAYIFC);
STR ARRAYINT_strrSTR_unbox(ARRAYINT);
STR ARRAYO2133079268_unbox(ARRAYO570594127);
STR ARRAYS1170645309_unbox(ARRAYSFILE_ID);
STR ARRAYS1578977701_unbox(dELTSTR_eltbrSTR_frame);
void* ARRAYS1578977701_frame_alloc();
STR ARRAYS2043945973_unbox(ARRAYSIDE_EFFECT);
STR ARRAYSIG_strrSTR_unbox(ARRAYSIG);
STR ARRAYSTR_strrSTR_unbox(ARRAYSTR);
STR ARRAYT1037471825_unbox(ARRAYT1275404834);
STR ARRAYT103792371_unbox(ARRAYT1759168384);
STR ARRAYT1054394674_unbox(ARRAYT1968212795);
STR ARRAYT1238370919_unbox(ARRAYTUPIDENTdTP);
STR ARRAYT1278969222_unbox(ARRAYT1562141251);
STR ARRAYT1355045210_unbox(ARRAYTUPIDENTINT);
STR ARRAYT1384853867_unbox(ARRAYT348931282);
STR ARRAYT1450640385_unbox(ARRAYT1153101058);
STR ARRAYT1561971079_unbox(ARRAYT1127505716);
STR ARRAYT1574849761_unbox(ARRAYT61112132);
STR ARRAYT1589477172_unbox(ARRAYTP_LAYOUT);
STR ARRAYT160571351_unbox(ARRAYT1641775270);
STR ARRAYT1626625951_unbox(ARRAYT546647214);
STR ARRAYT1658404076_unbox(ARRAYTUPSTRINT);
STR ARRAYT1671874055_unbox(ARRAYT1744744076);
STR ARRAYT1718290516_unbox(ARRAYTUPdOBSTR);
STR ARRAYT1733874469_unbox(ARRAYTP_CLASS);
STR ARRAYT1803904833_unbox(ARRAYT2065045042);
STR ARRAYT1810565556_unbox(ARRAYT1753333425);
STR ARRAYT1846205279_unbox(ARRAYT747572380);
STR ARRAYT189456178_unbox(ARRAYTUPSIGSTR);
STR ARRAYT1926215796_unbox(ARRAYT1146239647);
STR ARRAYT1942593259_unbox(ARRAYT1754348798);
STR ARRAYT2122676784_unbox(ARRAYT378612147);
STR ARRAYT31468026_unbox(ARRAYTUPSIGSIG);
STR ARRAYT568533288_unbox(ARRAYT263953675);
STR ARRAYT585031862_unbox(ARRAYT537394765);
STR ARRAYT614884074_unbox(ARRAYTUPSTRSTR);
STR ARRAYT835808278_unbox(ARRAYTUPSTRSIG);
STR ARRAYT835974574_unbox(ARRAYT1418209173);
STR ARRAYT854063824_unbox(ARRAYTUPSIGINT);
STR ARRAYT856660275_unbox(ARRAYTUPdTPINT);
STR ARRAYd1085727763_unbox(ARRAYdAM_STMT);
STR ARRAYd2023686048_unbox(ARRAYdAM_EXPR);
STR ARRAYd2065245377_unbox(ARRAYdAM_CONST);
STR ARRAYd946747660_unbox(ARRAYdLAYOUT);
STR ARRAYdTP_strrSTR_unbox(ARRAYdTP);
STR A_STAC1193125327_unbox(A_STAC1244966098);
STR BOOL_strrSTR_unbox(BOOL_boxed);
STR BOUND_1245789017_unbox(BOUND_2140373124);
STR BOUND_1401874693_unbox(BOUND_1007407331, CGEN);
STR BOUND_1537967790_unbox(BOUND_809390774, CGEN);
STR BOUND_1707602440_unbox(BOUND_1677815802, CGEN);
STR BOUND_1856529942_unbox(BOUND_1007407331);
STR BOUND_365396347_unbox(BOUND_2140373124, CGEN);
STR BOUND_619031357_unbox(BOUND_809390774);
STR BOUND_787102169_unbox(BOUND_1677815802);
STR BUILTI1940169806_unbox(BUILTIN_LAYOUT);
STR BUILTI35892888_unbox(BUILTIN_LAYOUT, CGEN);
STR CALL_T152540990_unbox(CALL_TP_ARRAY);
STR CALL_T1577548690_unbox(CALL_TP_CREATE);
STR CALL_T1666519150_unbox(CALL_T561792367);
STR CALL_T184446387_unbox(CALL_TP_VOID);
STR CALL_T47412966_unbox(CALL_T666389079);
STR CHAR_strrSTR_unbox(CHAR_boxed);
STR CLASS_1824950980_unbox(CLASS_LAYOUT);
STR CLASS_954167052_unbox(CLASS_LAYOUT, CGEN);
STR CONFIG1175493142_unbox(CONFIG_ROUT);
STR CONFIG1221082662_unbox(CONFIG_DEF);
STR CONFIG155608225_unbox(CONFIG_TBL);
STR CONFIG336048123_unbox(CONFIG_ROUT);
STR CONFIG811280218_unbox(CONFIG_DEF);
STR CS_OPT565500779_unbox(CS_OPTIONS);
STR ELT_TBL_strrSTR_unbox(ELT_TBL);
STR EXTERN1283013626_unbox(EXTERNAL_LAYOUT);
STR EXTERN1472641871_unbox(EXTERNAL_LAYOUT, CGEN);
STR FLISTA1208864070_unbox(FLISTA112326087);
STR FLISTA1228646807_unbox(FLISTA1327600838);
STR FLISTA1264584154_unbox(FLISTA162798263);
STR FLISTA128466217_unbox(FLISTAS_FEAT_MOD);
STR FLISTA1345532203_unbox(FLISTARRAYSTR);
STR FLISTA1366495389_unbox(FLISTA2097324934);
STR FLISTA1598488884_unbox(FLISTA886630705);
STR FLISTA1623803758_unbox(FLISTATTRSORT);
STR FLISTA1774499089_unbox(FLISTAM_CALL_ARG);
STR FLISTA1884067430_unbox(FLISTA1062334999);
STR FLISTA2047982482_unbox(FLISTA725283029);
STR FLISTA2143887329_unbox(FLISTAM_CURSOR);
STR FLISTA389202533_unbox(FLISTA1409846210);
STR FLISTA574012977_unbox(FLISTARRAYBOOL);
STR FLISTA674592310_unbox(FLISTAM_ROUT_DEF);
STR FLISTA902886330_unbox(FLISTA1893461511);
STR FLISTARG_strrSTR_unbox(FLISTARG);
STR FLISTB1590162857_unbox(FLISTBOOL);
STR FLISTB2097638834_unbox(FLISTB2062080075);
STR FLISTB2138238497_unbox(FLISTB475591118);
STR FLISTB275387800_unbox(FLISTB16124645);
STR FLISTC1168586346_unbox(FLISTC1031309271);
STR FLISTC1817360302_unbox(FLISTCODE_FILE);
STR FLISTC1921340409_unbox(FLISTC1772242452);
STR FLISTC31611564_unbox(FLISTCALL_ARG);
STR FLISTC780849889_unbox(FLISTCHAR);
STR FLISTE1365889449_unbox(FLISTE1761212196);
STR FLISTE283073321_unbox(FLISTE1627119380);
STR FLISTF1366971496_unbox(FLISTFLISTSTR);
STR FLISTF786174088_unbox(FLISTFSTR);
STR FLISTF93053878_unbox(FLISTF7594819);
STR FLISTI1877263384_unbox(FLISTI2086541899);
STR FLISTI2007266978_unbox(FLISTI1895665739);
STR FLISTI700982870_unbox(FLISTIDENT);
STR FLISTIFC_strrSTR_unbox(FLISTIFC);
STR FLISTINT_strrSTR_unbox(FLISTINT);
STR FLISTO1373905061_unbox(FLISTO1593816750);
STR FLISTS1154465862_unbox(FLISTSFILE_ID);
STR FLISTS1171631134_unbox(dELTSTR_eltbrSTR_frame);
void* FLISTS1171631134_frame_alloc();
STR FLISTS1349623250_unbox(FLISTSIDE_EFFECT);
STR FLISTSIG_strrSTR_unbox(FLISTSIG);
STR FLISTSTR_strrSTR_unbox(FLISTSTR);
STR FLISTT1090491199_unbox(FLISTT844804258);
STR FLISTT1312754552_unbox(FLISTT1075494997);
STR FLISTT1350994583_unbox(FLISTTUPSTRINT);
STR FLISTT1410881023_unbox(FLISTTUPdOBSTR);
STR FLISTT1451978730_unbox(FLISTT31683607);
STR FLISTT1518857242_unbox(FLISTT1145236345);
STR FLISTT1532344775_unbox(FLISTT1683165204);
STR FLISTT158032289_unbox(FLISTT1858637310);
STR FLISTT1750053916_unbox(FLISTTP_CLASS);
STR FLISTT1848908808_unbox(FLISTT406286251);
STR FLISTT1896886665_unbox(FLISTTP_LAYOUT);
STR FLISTT1906991113_unbox(FLISTT1023127882);
STR FLISTT1916592887_unbox(FLISTT567859354);
STR FLISTT208196008_unbox(FLISTT418307355);
STR FLISTT2098083178_unbox(FLISTT1311334615);
STR FLISTT275941467_unbox(FLISTTUPSIGSIG);
STR FLISTT306797234_unbox(FLISTT1513788273);
STR FLISTT307474581_unbox(FLISTTUPSTRSTR);
STR FLISTT32449926_unbox(FLISTT789237687);
STR FLISTT496865671_unbox(FLISTTUPSIGSTR);
STR FLISTT499676452_unbox(FLISTT102367225);
STR FLISTT528398785_unbox(FLISTTUPSTRSIG);
STR FLISTT544048196_unbox(FLISTTUPIDENTdTP);
STR FLISTT546654331_unbox(FLISTTUPSIGINT);
STR FLISTT549250782_unbox(FLISTTUPdTPINT);
STR FLISTT580276949_unbox(FLISTT471737672);
STR FLISTT582206950_unbox(FLISTT994805427);
STR FLISTT660722487_unbox(FLISTTUPIDENTINT);
STR FLISTT689628623_unbox(FLISTT2049829484);
STR FLISTT709853181_unbox(FLISTT1661203254);
STR FLISTT88767660_unbox(FLISTT880505585);
STR FLISTT919810588_unbox(FLISTT1742988937);
STR FLISTd1101907210_unbox(FLISTdAM_STMT);
STR FLISTd1314612993_unbox(FLISTdLAYOUT);
STR FLISTd1757835884_unbox(FLISTdAM_CONST);
STR FLISTd2007506601_unbox(FLISTdAM_EXPR);
STR FLISTdTP_strrSTR_unbox(FLISTdTP);
STR FLTD_strrSTR_unbox(FLTD_boxed);
STR FLT_strrSTR_unbox(FLT_boxed);
STR FMAPAM1084073011_unbox(FMAPAM339652544);
STR FMAPAM118642384_unbox(FMAPAM1787628573);
STR FMAPAM1201551978_unbox(FMAPAM1357596931);
STR FMAPAM1620534738_unbox(FMAPAM1145008811);
STR FMAPAM459426429_unbox(FMAPAM1244483504);
STR FMAPAM905477833_unbox(FMAPAM418009930);
STR FMAPID1860739187_unbox(FMAPIDENTdTP);
STR FMAPID1895267434_unbox(FMAPID739086519);
STR FMAPSI1424048021_unbox(FMAPSI1349988702);
STR FMAPSI1996405359_unbox(FMAPSI2103621588);
STR FMAPSI2071674985_unbox(FMAPSI1121373188);
STR FMAPSI866101256_unbox(FMAPSIGSTR);
STR FMAPSI99914640_unbox(FMAPSI518162669);
STR FMAPST1581005786_unbox(FMAPSTRINT);
STR FMAPST1670441508_unbox(FMAPSTRSTR);
STR FMAPST1891365712_unbox(FMAPSTRSIG);
STR FMAPST35426998_unbox(FMAPSTRFSETSTR);
STR FMAPST512400025_unbox(FMAPSTRdCONFIG);
STR FMAPTP1432889353_unbox(FMAPTP291739594);
STR FMAPTP722426429_unbox(FMAPTP775297600);
STR FMAPdO570867591_unbox(FMAPdOBNAMESPACE);
STR FMAPdT1371742936_unbox(FMAPdTPdLAYOUT);
STR FMAPdT1912217709_unbox(FMAPdTPINT);
STR FMAPdT2142836224_unbox(FMAPdT953816637);
STR FMAPdT966432485_unbox(FMAPdTPCODE_FILE);
STR FMT_ER330399541_unbox(FMT_ERROR_boxed);
STR FRAME_1195781105_unbox(FRAME_LAYOUT);
STR FRAME_691068581_unbox(FRAME_LAYOUT, CGEN);
STR FSETSTR_eltbrSTR_unbox(dELTSTR_eltbrSTR_frame);
void* FSETSTR_eltbrSTR_frame_alloc();
STR FSTR_strrSTR_unbox(FSTR);
STR IFC_strrSTR_unbox(IFC);
STR IMMUTA2093463840_unbox(IMMUTA421759893, CGEN);
STR IMMUTA966323602_unbox(IMMUTA421759893);
STR INOUT_1907813337_unbox(INOUT_MODE);
STR INTI_strrSTR_unbox(INTI1);
STR INT_strrSTR_unbox(INT_boxed);
STR IN_MODE_strrSTR_unbox(IN_MODE);
STR NULL_S1933820575_unbox(NULL_STAT);
STR ONCE_M1994645546_unbox(ONCE_MODE);
STR OUT_MODE_strrSTR_unbox(OUT_MODE);
STR RAT_strrSTR_unbox(RAT_boxed);
STR SFILE_ID_strrSTR_unbox(SFILE_ID_boxed);
STR SIG_TBL_strrSTR_unbox(SIG_TBL);
STR SIG_strrSTR_unbox(SIG);
STR STAT_strrSTR_unbox(STAT1);
STR STR_strrSTR_unbox(STR);
STR TP_CLASS_strrSTR_unbox(TP_CLASS);
STR TP_ITER_strrSTR_unbox(TP_ITER);
STR TP_LAY473390218_unbox(TP_LAYOUT);
STR TP_LAY690809295_unbox(TP_LAYOUT, CGEN);
STR TP_ROUT_strrSTR_unbox(TP_ROUT);
STR TUPAM_1128362628_unbox(TUPAM_304458649_boxed);
STR TUPAM_1694719258_unbox(TUPAM_1519040877_boxed);
STR TUPAM_489161612_unbox(TUPAM_927437481_boxed);
STR TUPAM_504028066_unbox(TUPAM_504653531_boxed);
STR TUPAM_525468809_unbox(TUPAM_653469574_boxed);
STR TUPAM_827739609_unbox(TUPAM_667892060_boxed);
STR TUPAM_85614060_unbox(TUPAM_1024994801_boxed);
STR TUPARR1404914248_unbox(TUPARRAYARGdTP_boxed);
STR TUPARR735320083_unbox(TUPARR1759987776_boxed);
STR TUPBOO1584822636_unbox(TUPBOO1392923241_boxed);
STR TUPIDE1477036401_unbox(TUPIDE641654124_boxed);
STR TUPIDE1923523563_unbox(TUPIDENTARRAYdTP_boxed);
STR TUPIDE2057908249_unbox(TUPIDENTdTP_boxed);
STR TUPIDE2120384756_unbox(TUPIDENTINT_boxed);
STR TUPINT567702316_unbox(TUPINTINT_boxed);
STR TUPSIG1001949053_unbox(TUPSIG1754078736_boxed);
STR TUPSIG1147554980_unbox(TUPSIGSIG_boxed);
STR TUPSIG1626178992_unbox(TUPSIG578692189_boxed);
STR TUPSIG1970150778_unbox(TUPSIGINT_boxed);
STR TUPSIG610691414_unbox(TUPSIGARG_LAYOUT_boxed);
STR TUPSIG659332932_unbox(TUPSIG2023533247_boxed);
STR TUPSIG926630776_unbox(TUPSIGSTR_boxed);
STR TUPSTR1075084367_unbox(TUPSTRdCONFIG_boxed);
STR TUPSTR1520476266_unbox(TUPSTRINT_boxed);
STR TUPSTR1622911390_unbox(TUPSTRFSETSTR_boxed);
STR TUPSTR1730971028_unbox(TUPSTRSTR_boxed);
STR TUPSTR1951895232_unbox(TUPSTRSIG_boxed);
STR TUPTP_131070026_unbox(TUPTP_858321267_boxed);
STR TUPTP_371689873_unbox(TUPTP_1668454172_boxed);
STR TUPdAM1835151881_unbox(TUPdAM_EXPRdTP_boxed);
STR TUPdAS100511776_unbox(TUPdAS268679779_boxed);
STR TUPdOB1460589826_unbox(TUPdOBSTR_boxed);
STR TUPdOB1872884181_unbox(TUPdOBNAMESPACE_boxed);
STR TUPdTP1335739968_unbox(TUPdTPdLAYOUT_boxed);
STR TUPdTP1477319287_unbox(TUPdTPCODE_FILE_boxed);
STR TUPdTP1972747229_unbox(TUPdTPINT_boxed);
STR TUPdTP2068651704_unbox(TUPdTP1710233051_boxed);
TP_CLASS ARRAYT1851819274_unbox(dELTTP817795506_frame);
void* ARRAYT1851819274_frame_alloc();
TP_CLASS FLISTT1683973815_unbox(dELTTP817795506_frame);
void* FLISTT1683973815_frame_alloc();
TP_CLASS FSETTP340014621_unbox(dELTTP817795506_frame);
void* FSETTP340014621_frame_alloc();
TP_LAYOUT ARRAYT1372201847_unbox(dELTTP992626963_frame);
void* ARRAYT1372201847_frame_alloc();
TP_LAYOUT FLISTT2106747232_unbox(dELTTP992626963_frame);
void* FLISTT2106747232_frame_alloc();
TUPAM_1024994801 ARRAYT1758179858_unbox(dELTTU225750038_frame);
void* ARRAYT1758179858_frame_alloc();
TUPAM_1024994801 FLISTT234903853_unbox(dELTTU225750038_frame);
void* FLISTT234903853_frame_alloc();
TUPAM_1519040877 ARRAYT684852235_unbox(dELTTU1106260535_frame);
void* ARRAYT684852235_frame_alloc();
TUPAM_1519040877 FLISTT344492076_unbox(dELTTU1106260535_frame);
void* FLISTT344492076_frame_alloc();
TUPAM_304458649 ARRAYT310960754_unbox(dELTTU1697593574_frame);
void* ARRAYT310960754_frame_alloc();
TUPAM_304458649 FLISTT726119853_unbox(dELTTU1697593574_frame);
void* FLISTT726119853_frame_alloc();
TUPAM_504653531 ARRAYT342187846_unbox(dELTTU2146728406_frame);
void* ARRAYT342187846_frame_alloc();
TUPAM_504653531 FLISTT1883920969_unbox(dELTTU2146728406_frame);
void* FLISTT1883920969_frame_alloc();
TUPAM_653469574 ARRAYT1101967660_unbox(dELTTU1108719580_frame);
void* ARRAYT1101967660_frame_alloc();
TUPAM_653469574 FLISTT288887925_unbox(dELTTU1108719580_frame);
void* FLISTT288887925_frame_alloc();
TUPAM_667892060 ARRAYT2108331328_unbox(dELTTU202706072_frame);
void* ARRAYT2108331328_frame_alloc();
TUPAM_667892060 FLISTT193552257_unbox(dELTTU202706072_frame);
void* FLISTT193552257_frame_alloc();
TUPAM_927437481 ARRAYT1360634798_unbox(dELTTU538545734_frame);
void* ARRAYT1360634798_frame_alloc();
TUPAM_927437481 FLISTT709128269_unbox(dELTTU538545734_frame);
void* FLISTT709128269_frame_alloc();
TUPIDE641654124 ARRAYT451883912_unbox(dELTTU577707672_frame);
void* ARRAYT451883912_frame_alloc();
TUPIDE641654124 FLISTT1472123735_unbox(dELTTU577707672_frame);
void* FLISTT1472123735_frame_alloc();
TUPIDENTINT ARRAYT1017446695_unbox(dELTTU1837273309_frame);
void* ARRAYT1017446695_frame_alloc();
TUPIDENTINT FLISTT97637998_unbox(dELTTU1837273309_frame);
void* FLISTT97637998_frame_alloc();
TUPIDENTINT FSETTU947212086_unbox(dELTTU1837273309_frame);
void* FSETTU947212086_frame_alloc();
TUPIDENTdTP ARRAYT849390229_unbox(dELTTU590857063_frame);
void* ARRAYT849390229_frame_alloc();
TUPIDENTdTP FLISTT1769198926_unbox(dELTTU590857063_frame);
void* FLISTT1769198926_frame_alloc();
TUPSIG1754078736 ARRAYT1079066005_unbox(dELTTU2080726233_frame);
void* ARRAYT1079066005_frame_alloc();
TUPSIG1754078736 FLISTT287332146_unbox(dELTTU2080726233_frame);
void* FLISTT287332146_frame_alloc();
TUPSIG2023533247 ARRAYT532396130_unbox(dELTTU1157822078_frame);
void* ARRAYT532396130_frame_alloc();
TUPSIG2023533247 FLISTT685872269_unbox(dELTTU1157822078_frame);
void* FLISTT685872269_frame_alloc();
TUPSIG578692189 ARRAYT1269605207_unbox(dELTTU1031278189_frame);
void* ARRAYT1269605207_frame_alloc();
TUPSIG578692189 FLISTT754641600_unbox(dELTTU1031278189_frame);
void* FLISTT754641600_frame_alloc();
TUPSIGARG_LAYOUT ARRAYT457901414_unbox(dELTTU1654314914_frame);
void* ARRAYT457901414_frame_alloc();
TUPSIGARG_LAYOUT FLISTT634936167_unbox(dELTTU1654314914_frame);
void* FLISTT634936167_frame_alloc();
TUPSIGINT ARRAYT1593820953_unbox(dELTTU1973395837_frame);
void* ARRAYT1593820953_frame_alloc();
TUPSIGINT FLISTT777802736_unbox(dELTTU1973395837_frame);
void* FLISTT777802736_frame_alloc();
TUPSIGSIG ARRAYT1146052097_unbox(dELTTU1525626981_frame);
void* ARRAYT1146052097_frame_alloc();
TUPSIGSIG FLISTT330033880_unbox(dELTTU1525626981_frame);
void* FLISTT330033880_frame_alloc();
TUPSIGSIG FSETTU845419336_unbox(dELTTU1525626981_frame);
void* FSETTU845419336_frame_alloc();
TUPSIGSTR ARRAYT1691893201_unbox(dELTTU2071468085_frame);
void* ARRAYT1691893201_frame_alloc();
TUPSIGSTR FLISTT875874984_unbox(dELTTU2071468085_frame);
void* FLISTT875874984_frame_alloc();
TUPSTRFSETSTR ARRAYT1426073269_unbox(dELTTU1527749065_frame);
void* ARRAYT1426073269_frame_alloc();
TUPSTRFSETSTR FLISTT720864404_unbox(dELTTU1527749065_frame);
void* FLISTT720864404_frame_alloc();
TUPSTRINT ARRAYT306471177_unbox(dELTTU686046061_frame);
void* ARRAYT306471177_frame_alloc();
TUPSTRINT FLISTT509547040_unbox(dELTTU686046061_frame);
void* FLISTT509547040_frame_alloc();
TUPSTRSIG ARRAYT141297679_unbox(dELTTU238277205_frame);
void* ARRAYT141297679_frame_alloc();
TUPSTRSIG FLISTT957315896_unbox(dELTTU238277205_frame);
void* FLISTT957315896_frame_alloc();
TUPSTRSTR ARRAYT404543425_unbox(dELTTU784118309_frame);
void* ARRAYT404543425_frame_alloc();
TUPSTRSTR FLISTT411474792_unbox(dELTTU784118309_frame);
void* FLISTT411474792_frame_alloc();
TUPSTRdCONFIG ARRAYT485937919_unbox(dELTTU384262123_frame);
void* ARRAYT485937919_frame_alloc();
TUPSTRdCONFIG FLISTT1662091704_unbox(dELTTU384262123_frame);
void* FLISTT1662091704_frame_alloc();
TUPTP_1668454172 ARRAYT1509169201_unbox(dELTTU963535579_frame);
void* ARRAYT1509169201_frame_alloc();
TUPTP_1668454172 FLISTT1725186742_unbox(dELTTU963535579_frame);
void* FLISTT1725186742_frame_alloc();
TUPTP_858321267 ARRAYT518925702_unbox(dELTTU1171292506_frame);
void* ARRAYT518925702_frame_alloc();
TUPTP_858321267 FLISTT699342697_unbox(dELTTU1171292506_frame);
void* FLISTT699342697_frame_alloc();
TUPdOBNAMESPACE ARRAYT681600609_unbox(dELTTU164160693_frame);
void* ARRAYT681600609_frame_alloc();
TUPdOBNAMESPACE FLISTT396553018_unbox(dELTTU164160693_frame);
void* FLISTT396553018_frame_alloc();
TUPdOBSTR ARRAYT595865513_unbox(dELTTU975440397_frame);
void* ARRAYT595865513_frame_alloc();
TUPdOBSTR FLISTT220152704_unbox(dELTTU975440397_frame);
void* FLISTT220152704_frame_alloc();
TUPdTP1710233051 ARRAYT1505580189_unbox(dELTTU1084171889_frame);
void* ARRAYT1505580189_frame_alloc();
TUPdTP1710233051 FLISTT1760042796_unbox(dELTTU1084171889_frame);
void* FLISTT1760042796_frame_alloc();
TUPdTPCODE_FILE ARRAYT1283478039_unbox(dELTTU1800917955_frame);
void* ARRAYT1283478039_frame_alloc();
TUPdTPCODE_FILE FLISTT1568525630_unbox(dELTTU1800917955_frame);
void* FLISTT1568525630_frame_alloc();
TUPdTPINT ARRAYT452391291_unbox(dELTTU72816407_frame);
void* ARRAYT452391291_frame_alloc();
TUPdTPINT FLISTT1268409508_unbox(dELTTU72816407_frame);
void* FLISTT1268409508_frame_alloc();
TUPdTPdLAYOUT ARRAYT2024950707_unbox(dELTTU1923274911_frame);
void* ARRAYT2024950707_frame_alloc();
TUPdTPdLAYOUT FLISTT123078916_unbox(dELTTU1923274911_frame);
void* FLISTT123078916_frame_alloc();
dAM_CONST ARRAYd198808957_unbox(dELTdA578383841_frame);
void* ARRAYd198808957_frame_alloc();
dAM_CONST FLISTd617209260_unbox(dELTdA578383841_frame);
void* FLISTd617209260_frame_alloc();
dAM_EXPR AM_ANY934244480_unbox(AM_ANY_EXPR);
dAM_EXPR AM_ARR1852652918_unbox(AM_ARRAY_EXPR);
dAM_EXPR AM_ARR990477897_unbox(AM_ARR_CONST);
dAM_EXPR AM_ATT1705760219_unbox(AM_ATTR_EXPR);
dAM_EXPR AM_AT_1135400775_unbox(AM_AT_EXPR);
dAM_EXPR AM_BND1416432078_unbox(AM_BND367211769);
dAM_EXPR AM_BND2052779026_unbox(AM_BND260301329);
dAM_EXPR AM_BND831738871_unbox(AM_BND1124877163);
dAM_EXPR AM_BOO158864518_unbox(AM_BOOL_CONST);
dAM_EXPR AM_CHA916745766_unbox(AM_CHAR_CONST);
dAM_EXPR AM_CLU1577744436_unbox(AM_CLUSTER_EXPR);
dAM_EXPR AM_CLU779855221_unbox(AM_CLU1286269335);
dAM_EXPR AM_CON582703191_unbox(AM_CONST);
dAM_EXPR AM_EXC449573271_unbox(AM_EXCEPT_EXPR);
dAM_EXPR AM_EXT1687283405_unbox(AM_EXT_CALL_EXPR);
dAM_EXPR AM_FAR755578228_unbox(AM_FAR_EXPR);
dAM_EXPR AM_FLT1362803603_unbox(AM_FLT_CONST);
dAM_EXPR AM_FLT1839670182_unbox(AM_FLTD_CONST);
dAM_EXPR AM_FLT425117946_unbox(AM_FLTI_CONST);
dAM_EXPR AM_FLT476428534_unbox(AM_FLTX_CONST);
dAM_EXPR AM_FLT822990115_unbox(AM_FLTDX_CONST);
dAM_EXPR AM_GLO928972965_unbox(AM_GLOBAL_EXPR);
dAM_EXPR AM_HER1301311031_unbox(AM_HERE_EXPR);
dAM_EXPR AM_IF_1266575443_unbox(AM_IF_EXPR);
dAM_EXPR AM_INT1629629861_unbox(AM_INT_CONST);
dAM_EXPR AM_INT304374138_unbox(AM_INTI_CONST);
dAM_EXPR AM_IS_193101840_unbox(AM_IS_VOID_EXPR);
dAM_EXPR AM_ITE380500422_unbox(AM_ITE1809135850);
dAM_EXPR AM_LOC1798202130_unbox(AM_LOCAL_EXPR);
dAM_EXPR AM_NEA1086037759_unbox(AM_NEAR_EXPR);
dAM_EXPR AM_NEW1250698656_unbox(AM_NEW_EXPR);
dAM_EXPR AM_ROU960965786_unbox(AM_ROU1916046290);
dAM_EXPR AM_SHA1035965785_unbox(AM_SHARED_EXPR);
dAM_EXPR AM_STM416210327_unbox(AM_STMT_EXPR);
dAM_EXPR AM_STR1745454041_unbox(AM_STR_CONST);
dAM_EXPR AM_VAR1906690564_unbox(AM_VAR744470097);
dAM_EXPR AM_VAT2137026865_unbox(AM_VAT319982528);
dAM_EXPR AM_VOI1734446450_unbox(AM_VOID_CONST);
dAM_EXPR AM_WHE1264983710_unbox(AM_WHERE_EXPR);
dAM_EXPR ARRAYd1413132360_unbox(dELTdA379108592_frame);
void* ARRAYd1413132360_frame_alloc();
dAM_EXPR FLISTd2122660729_unbox(dELTdA379108592_frame);
void* FLISTd2122660729_frame_alloc();
dAM_EXPR INLINE1060127472_unbox(INLINE, AM_ROUT_DEF, AM_LOOP_STMT, AM_ITE1809135850);
dAM_EXPR INLINE1324370258_unbox(INLINE_INT_FOLD, AM_ROU1916046290);
dAM_EXPR INLINE1426016646_unbox(INLINE, AM_ITE1809135850);
dAM_EXPR INLINE1475043475_unbox(INLINE1433668901, AM_ROU1916046290);
dAM_EXPR INLINE1736265130_unbox(INLINE1164904870, AM_ROU1916046290);
dAM_EXPR INLINE1797911459_unbox(INLINE, AM_ROUT_DEF, AM_ROU1916046290);
dAM_EXPR INLINE1949170047_unbox(INLINE_ROUT, AM_ROU1916046290);
dAM_EXPR INLINE2023716438_unbox(INLINE1455768374, AM_ROU1916046290);
dAM_EXPR INLINE46712374_unbox(INLINE_ATTR_READ, AM_ROU1916046290);
dAM_EXPR INLINE472404814_unbox(INLINE, AM_ROU1916046290);
dAM_EXPR INLINE655229257_unbox(INLINE1596311811, AM_ROU1916046290);
dAM_EXPR INLINE986164897_unbox(INLINE_ITER, AM_ITE1809135850);
dAM_STMT AM_ASS1201484234_unbox(AM_ASSIGN_STMT);
dAM_STMT AM_ASS1481858691_unbox(AM_ASSERT_STMT);
dAM_STMT AM_ASS1841384941_unbox(AM_ASSERT_STMT);
dAM_STMT AM_ASS782632663_unbox(AM_ASSIGN_STMT);
dAM_STMT AM_ATT1156941996_unbox(AM_ATTACH_STMT);
dAM_STMT AM_ATT191404837_unbox(AM_ATTACH_STMT);
dAM_STMT AM_BRE1241758751_unbox(AM_BREAK_STMT);
dAM_STMT AM_BRE1937656818_unbox(AM_BREAK_STMT);
dAM_STMT AM_CAS1510303900_unbox(AM_CASE_STMT);
dAM_STMT AM_CAS1898951205_unbox(AM_CASE_STMT);
dAM_STMT AM_COM1124366108_unbox(AM_COMMENT_STMT);
dAM_STMT AM_COM977130404_unbox(AM_COMMENT_STMT);
dAM_STMT AM_EXP1561605445_unbox(AM_EXPR_STMT);
dAM_STMT AM_EXP359209435_unbox(AM_EXPR_STMT);
dAM_STMT AM_FOR1357281358_unbox(AM_FORK_STMT);
dAM_STMT AM_FOR214044719_unbox(AM_FORK_STMT);
dAM_STMT AM_IF_1153855302_unbox(AM_IF_STMT);
dAM_STMT AM_IF_1702280121_unbox(AM_IF_STMT);
dAM_STMT AM_INI1487747776_unbox(AM_INITIAL_STMT);
dAM_STMT AM_INI81424971_unbox(AM_INITIAL_STMT);
dAM_STMT AM_INV1267245156_unbox(AM_INV1167837230);
dAM_STMT AM_INV1353049642_unbox(AM_INV1167837230);
dAM_STMT AM_LOC1474178511_unbox(AM_LOCK_STMT);
dAM_STMT AM_LOC724705923_unbox(AM_LOCK_STMT);
dAM_STMT AM_LOO1572875803_unbox(AM_LOOP_STMT);
dAM_STMT AM_LOO439140156_unbox(AM_LOOP_STMT);
dAM_STMT AM_PAR309007796_unbox(AM_PAR_STMT);
dAM_STMT AM_PAR784084248_unbox(AM_PAR_STMT);
dAM_STMT AM_POS1078136755_unbox(AM_POST_STMT);
dAM_STMT AM_POS1775621496_unbox(AM_POST_STMT);
dAM_STMT AM_PRE1502061133_unbox(AM_PREFETCH_STMT);
dAM_STMT AM_PRE1762688610_unbox(AM_PRE_STMT);
dAM_STMT AM_PRE202271760_unbox(AM_PRE_STMT);
dAM_STMT AM_PRE896696117_unbox(AM_PREFETCH_STMT);
dAM_STMT AM_PRO1363632140_unbox(AM_PROTECT_STMT);
dAM_STMT AM_PRO557659408_unbox(AM_PROTECT_STMT);
dAM_STMT AM_RAI1337438778_unbox(AM_RAISE_STMT);
dAM_STMT AM_RAI1825238682_unbox(AM_RAISE_STMT);
dAM_STMT AM_RET1651971761_unbox(AM_RETURN_STMT);
dAM_STMT AM_RET1976133913_unbox(AM_RETURN_STMT);
dAM_STMT AM_SYN1962040721_unbox(AM_SYNC_STMT);
dAM_STMT AM_SYN2020605939_unbox(AM_SYNC_STMT);
dAM_STMT AM_TYP1504178732_unbox(AM_TYPECASE_STMT);
dAM_STMT AM_TYP1756924987_unbox(AM_TYPECASE_STMT);
dAM_STMT AM_UNL1996507749_unbox(AM_UNLOCK_STMT);
dAM_STMT AM_UNL460993177_unbox(AM_UNLOCK_STMT);
dAM_STMT AM_WAI1727009663_unbox(AM_WAITFOR_STMT);
dAM_STMT AM_WAI1946639376_unbox(AM_WAITFOR_STMT);
dAM_STMT AM_WIT1785216238_unbox(AM_WIT1996971603);
dAM_STMT AM_WIT315547139_unbox(AM_WIT1996971603);
dAM_STMT AM_YIE1267845009_unbox(AM_YIELD_STMT);
dAM_STMT AM_YIE197772790_unbox(AM_YIELD_STMT);
dAM_STMT ARRAYd615421366_unbox(dELTdA418602402_frame);
void* ARRAYd615421366_frame_alloc();
dAM_STMT FLISTd1374595573_unbox(dELTdA418602402_frame);
void* FLISTd1374595573_frame_alloc();
dAS_CLASS_ELT AS_ATT97460488_unbox(AS_ATTR_DEF);
dAS_CLASS_ELT AS_CON306298556_unbox(AS_CONST_DEF);
dAS_CLASS_ELT AS_INC1775598384_unbox(AS_INC1416917001);
dAS_CLASS_ELT AS_ROU123442773_unbox(AS_ROUT_DEF);
dAS_CLASS_ELT AS_SHA297921449_unbox(AS_SHARED_DEF);
dAS_EXPR AS_AND244040648_unbox(AS_AND_EXPR);
dAS_EXPR AS_ANY1960655325_unbox(AS_ANY_EXPR);
dAS_EXPR AS_ARR1675309363_unbox(AS_ARRAY_EXPR);
dAS_EXPR AS_AT_246103478_unbox(AS_AT_EXPR);
dAS_EXPR AS_BOO1496152570_unbox(AS_BOOL_LIT_EXPR);
dAS_EXPR AS_BOU110275841_unbox(AS_BOU14269336);
dAS_EXPR AS_BRE631040475_unbox(AS_BREAK_EXPR);
dAS_EXPR AS_CAL1709269196_unbox(AS_CALL_EXPR);
dAS_EXPR AS_CHA1807865070_unbox(AS_CHAR_LIT_EXPR);
dAS_EXPR AS_CLU1420620999_unbox(AS_CLUSTER_EXPR);
dAS_EXPR AS_CLU792721656_unbox(AS_CLU553570599);
dAS_EXPR AS_COH1869044806_unbox(AS_COHORT_EXPR);
dAS_EXPR AS_CRE1115272673_unbox(AS_CREATE_EXPR);
dAS_EXPR AS_EXC1501278546_unbox(AS_EXCEPT_EXPR);
dAS_EXPR AS_FAR2096957028_unbox(AS_FAR_EXPR);
dAS_EXPR AS_FLT1204717277_unbox(AS_FLT_LIT_EXPR);
dAS_EXPR AS_GLO1872829932_unbox(AS_GLOBAL_EXPR);
dAS_EXPR AS_HER846597914_unbox(AS_HERE_EXPR);
dAS_EXPR AS_INI1732476001_unbox(AS_INITIAL_EXPR);
dAS_EXPR AS_INT896052382_unbox(AS_INT_LIT_EXPR);
dAS_EXPR AS_IS_1211675728_unbox(AS_IS_VOID_EXPR);
dAS_EXPR AS_NEA160870810_unbox(AS_NEAR_EXPR);
dAS_EXPR AS_NEW162891781_unbox(AS_NEW_EXPR);
dAS_EXPR AS_OR_261789378_unbox(AS_OR_EXPR);
dAS_EXPR AS_RES945429434_unbox(AS_RESULT_EXPR);
dAS_EXPR AS_SEL1160430562_unbox(AS_SELF_EXPR);
dAS_EXPR AS_STR70439144_unbox(AS_STR_LIT_EXPR);
dAS_EXPR AS_UND1982396421_unbox(AS_UND152986614);
dAS_EXPR AS_VOI1856591394_unbox(AS_VOID_EXPR);
dAS_EXPR AS_WHE2011036209_unbox(AS_WHERE_EXPR);
dAS_STMT AS_ASS255483717_unbox(AS_ASSERT_STMT);
dAS_STMT AS_ASS384416990_unbox(AS_ASSIGN_STMT);
dAS_STMT AS_ATT428959228_unbox(AS_ATTACH_STMT);
dAS_STMT AS_CAS568284252_unbox(AS_CASE_STMT);
dAS_STMT AS_DEC1501678601_unbox(AS_DEC_STMT);
dAS_STMT AS_EXP516982707_unbox(AS_EXPR_STMT);
dAS_STMT AS_FOR721306794_unbox(AS_FORK_STMT);
dAS_STMT AS_IF_192647666_unbox(AS_IF_STMT);
dAS_STMT AS_INT1461683196_unbox(AS_INT2031239268);
dAS_STMT AS_LOC1353882229_unbox(AS_LOCK_STMT);
dAS_STMT AS_LOO1639447996_unbox(AS_LOOP_STMT);
dAS_STMT AS_PAR1281243032_unbox(AS_PAR_STMT);
dAS_STMT AS_PRO1410999444_unbox(AS_PROTECT_STMT);
dAS_STMT AS_QUI758322419_unbox(AS_QUIT_STMT);
dAS_STMT AS_RAI287018298_unbox(AS_RAISE_STMT);
dAS_STMT AS_RET732932159_unbox(AS_RETURN_STMT);
dAS_STMT AS_STM1163003696_unbox(AS_STMT);
dAS_STMT AS_SYN57982213_unbox(AS_SYNC_STMT);
dAS_STMT AS_TYP1323758084_unbox(AS_TYPECASE_STMT);
dAS_STMT AS_UNL712558323_unbox(AS_UNLOCK_STMT);
dAS_STMT AS_WIT1968509975_unbox(AS_WIT1063437351);
dAS_STMT AS_YIE1402665211_unbox(AS_YIELD_STMT);
dLAYOUT ARRAYd1647530701_unbox(dELTdL669601151_frame);
void* ARRAYd1647530701_frame_alloc();
dLAYOUT FLISTd1954760550_unbox(dELTdL669601151_frame);
void* FLISTd1954760550_frame_alloc();
dLAYOUT FSETdL485771726_unbox(dELTdL669601151_frame);
void* FSETdL485771726_frame_alloc();
dNIL FLTD_nilrFLTD_unbox(FLTD_boxed);
dNIL FLT_nilrFLT_unbox(FLT_boxed);
dNIL INT_nilrINT_unbox(INT_boxed);
dTP AM_ANY772124871_unbox(AM_ANY_EXPR);
dTP AM_ARR1099277377_unbox(AM_ARRAY_EXPR);
dTP AM_ARR925292072_unbox(AM_ARR_CONST);
dTP AM_ATT839964902_unbox(AM_ATTR_EXPR);
dTP AM_AT_1635564765_unbox(AM_AT_EXPR);
dTP AM_BND192433970_unbox(AM_BND260301329);
dTP AM_BND540727053_unbox(AM_BND1124877163);
dTP AM_BND95745606_unbox(AM_BND367211769);
dTP AM_BOO1614359222_unbox(AM_BOOL_CONST);
dTP AM_CHA165359522_unbox(AM_CHAR_CONST);
dTP AM_CLU2134150433_unbox(AM_CLU1286269335);
dTP AM_CLU889770873_unbox(AM_CLUSTER_EXPR);
dTP AM_CONST_tprdTP_unbox(AM_CONST);
dTP AM_EXC706383345_unbox(AM_EXCEPT_EXPR);
dTP AM_EXT195226505_unbox(AM_EXT_CALL_EXPR);
dTP AM_FAR24566214_unbox(AM_FAR_EXPR);
dTP AM_FLT1597643896_unbox(AM_FLTDX_CONST);
dTP AM_FLT2085750594_unbox(AM_FLTD_CONST);
dTP AM_FLT79327789_unbox(AM_FLT_CONST);
dTP AM_FLT804205627_unbox(AM_FLTI_CONST);
dTP AM_FLT884139698_unbox(AM_FLTX_CONST);
dTP AM_GLO541877835_unbox(AM_GLOBAL_EXPR);
dTP AM_HER217252257_unbox(AM_HERE_EXPR);
dTP AM_IF_179347781_unbox(AM_IF_EXPR);
dTP AM_INT1372528330_unbox(AM_INTI_CONST);
dTP AM_INT1391542618_unbox(AM_INT_CONST);
dTP AM_IS_826521970_unbox(AM_IS_VOID_EXPR);
dTP AM_ITE597285895_unbox(AM_ITE1809135850);
dTP AM_LOC1004636145_unbox(AM_LOCAL_EXPR);
dTP AM_NEA176857219_unbox(AM_NEAR_EXPR);
dTP AM_NEW710003407_unbox(AM_NEW_EXPR);
dTP AM_ROU885465471_unbox(AM_ROU1916046290);
dTP AM_SHA1973416871_unbox(AM_SHARED_EXPR);
dTP AM_STM300451057_unbox(AM_STMT_EXPR);
dTP AM_STR690233376_unbox(AM_STR_CONST);
dTP AM_VAR2117196902_unbox(AM_VAR744470097);
dTP AM_VAT1812280108_unbox(AM_VAT319982528);
dTP AM_VOI984518984_unbox(AM_VOID_CONST);
dTP AM_WHE843899429_unbox(AM_WHERE_EXPR);
dTP ARRAYd2147017943_unbox(dELTdTP_eltbrdTP_frame);
void* ARRAYd2147017943_frame_alloc();
dTP FLISTd1740602786_unbox(dELTdTP_eltbrdTP_frame);
void* FLISTd1740602786_frame_alloc();
dTP FSETdTP_eltbrdTP_unbox(dELTdTP_eltbrdTP_frame);
void* FSETdTP_eltbrdTP_frame_alloc();
void AM_ASS1281039639_unbox(AM_ASSIGN_STMT, dAM_STMT);
void AM_ASS306541468_unbox(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS356960882_unbox(AM_ASSERT_STMT, dAM_STMT);
void AM_ASS946442175_unbox(AM_ASSIGN_STMT, dAM_STMT);
void AM_ATT1913806391_unbox(AM_ATTACH_STMT, dAM_STMT);
void AM_ATT990984413_unbox(AM_ATTACH_STMT, dAM_STMT);
void AM_BRE261382646_unbox(AM_BREAK_STMT, dAM_STMT);
void AM_BRE905282136_unbox(AM_BREAK_STMT, dAM_STMT);
void AM_CAS1095788609_unbox(AM_CASE_STMT, dAM_STMT);
void AM_CAS637622509_unbox(AM_CASE_STMT, dAM_STMT);
void AM_COM1022674779_unbox(AM_COMMENT_STMT, dAM_STMT);
void AM_COM832310723_unbox(AM_COMMENT_STMT, dAM_STMT);
void AM_EXP1859190126_unbox(AM_EXPR_STMT, dAM_STMT);
void AM_EXP586320964_unbox(AM_EXPR_STMT, dAM_STMT);
void AM_FOR1689225795_unbox(AM_FORK_STMT, dAM_STMT);
void AM_FOR790645051_unbox(AM_FORK_STMT, dAM_STMT);
void AM_IF_1406990231_unbox(AM_IF_STMT, dAM_STMT);
void AM_IF_993185585_unbox(AM_IF_STMT, dAM_STMT);
void AM_INI2007284982_unbox(AM_INITIAL_STMT, dAM_STMT);
void AM_INI2065615916_unbox(AM_INITIAL_STMT, dAM_STMT);
void AM_INV749070155_unbox(AM_INV1167837230, dAM_STMT);
void AM_INV880681253_unbox(AM_INV1167837230, dAM_STMT);
void AM_LOC1423220486_unbox(AM_LOCK_STMT, dAM_STMT);
void AM_LOC962760448_unbox(AM_LOCK_STMT, dAM_STMT);
void AM_LOO1708786253_unbox(AM_LOOP_STMT, dAM_STMT);
void AM_LOO952733665_unbox(AM_LOOP_STMT, dAM_STMT);
void AM_PAR1454262261_unbox(AM_PAR_STMT, dAM_STMT);
void AM_PAR1838033091_unbox(AM_PAR_STMT, dAM_STMT);
void AM_POS1908807043_unbox(AM_POST_STMT, dAM_STMT);
void AM_POS372304913_unbox(AM_POST_STMT, dAM_STMT);
void AM_PRE1879848758_unbox(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRE2012355323_unbox(AM_PRE_STMT, dAM_STMT);
void AM_PRE385237799_unbox(AM_PRE_STMT, dAM_STMT);
void AM_PRE645865276_unbox(AM_PREFETCH_STMT, dAM_STMT);
void AM_PRO1589381479_unbox(AM_PROTECT_STMT, dAM_STMT);
void AM_PRO1885345065_unbox(AM_PROTECT_STMT, dAM_STMT);
void AM_RAI1178977909_unbox(AM_RAISE_STMT, dAM_STMT);
void AM_RAI810487631_unbox(AM_RAISE_STMT, dAM_STMT);
void AM_RET170906974_unbox(AM_RETURN_STMT, dAM_STMT);
void AM_RET917156604_unbox(AM_RETURN_STMT, dAM_STMT);
void AM_SYN127320470_unbox(AM_SYNC_STMT, dAM_STMT);
void AM_SYN631230960_unbox(AM_SYNC_STMT, dAM_STMT);
void AM_TYP505714135_unbox(AM_TYPECASE_STMT, dAM_STMT);
void AM_TYP642862155_unbox(AM_TYPECASE_STMT, dAM_STMT);
void AM_UNL150533138_unbox(AM_UNLOCK_STMT, dAM_STMT);
void AM_UNL2142830896_unbox(AM_UNLOCK_STMT, dAM_STMT);
void AM_WAI420031224_unbox(AM_WAITFOR_STMT, dAM_STMT);
void AM_WAI656698018_unbox(AM_WAITFOR_STMT, dAM_STMT);
void AM_WIT1831493748_unbox(AM_WIT1996971603, dAM_STMT);
void AM_WIT906438282_unbox(AM_WIT1996971603, dAM_STMT);
void AM_YIE1088587192_unbox(AM_YIELD_STMT, dAM_STMT);
void AM_YIE879195878_unbox(AM_YIELD_STMT, dAM_STMT);
void AS_AND1244275737_unbox(AS_AND_EXPR, dAS_EXPR);
void AS_AND1398627607_unbox(AS_AND_EXPR, SFILE_ID);
void AS_AND1903000239_unbox(AS_AND_EXPR, dAS_EXPR);
void AS_ANY1724395066_unbox(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY187271084_unbox(AS_ANY_EXPR, dAS_EXPR);
void AS_ANY72331114_unbox(AS_ANY_EXPR, SFILE_ID);
void AS_ARR1604409854_unbox(AS_ARRAY_EXPR, SFILE_ID);
void AS_ARR471731524_unbox(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ARR47654098_unbox(AS_ARRAY_EXPR, dAS_EXPR);
void AS_ASS1183239070_unbox(AS_ASSIGN_STMT, SFILE_ID);
void AS_ASS1330792936_unbox(AS_ASSERT_STMT, BOOL);
void AS_ASS1461460287_unbox(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1708420135_unbox(AS_ASSIGN_STMT, BOOL);
void AS_ASS1762623897_unbox(AS_ASSIGN_STMT, dAS_STMT);
void AS_ASS1892442692_unbox(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS2029454919_unbox(AS_ASSERT_STMT, AS_STMT_LIST);
void AS_ASS2107317827_unbox(AS_ASSERT_STMT, SFILE_ID);
void AS_ASS537381530_unbox(AS_ASSERT_STMT, dAS_STMT);
void AS_ASS650357212_unbox(AS_ASSIGN_STMT, AS_STMT_LIST);
void AS_ATT1135887709_unbox(AS_ATTACH_STMT, BOOL);
void AS_ATT1141535277_unbox(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT1480967150_unbox(AS_ATTACH_STMT, AS_STMT_LIST);
void AS_ATT1718081659_unbox(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1733385743_unbox(AS_ATTACH_STMT, dAS_STMT);
void AS_ATT1987206051_unbox(AS_ATTR_DEF, BOOL);
void AS_ATT2024939759_unbox(AS_ATTR_DEF, dAS_CLASS_ELT);
void AS_ATT83117804_unbox(AS_ATTACH_STMT, SFILE_ID);
void AS_AT_1901822931_unbox(AS_AT_EXPR, dAS_EXPR);
void AS_AT_2092645707_unbox(AS_AT_EXPR, dAS_EXPR);
void AS_AT_550257637_unbox(AS_AT_EXPR, SFILE_ID);
void AS_BOO1855503827_unbox(AS_BOOL_LIT_EXPR, SFILE_ID);
void AS_BOO203439875_unbox(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOO650888317_unbox(AS_BOOL_LIT_EXPR, dAS_EXPR);
void AS_BOU2006233068_unbox(AS_BOU14269336, dAS_EXPR);
void AS_BOU2036765046_unbox(AS_BOU14269336, dAS_EXPR);
void AS_BOU354169116_unbox(AS_BOU14269336, SFILE_ID);
void AS_BRE1516885934_unbox(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE679018004_unbox(AS_BREAK_EXPR, dAS_EXPR);
void AS_BRE973045948_unbox(AS_BREAK_EXPR, SFILE_ID);
void AS_CAL2065155647_unbox(AS_CALL_EXPR, SFILE_ID);
void AS_CAL437771691_unbox(AS_CALL_EXPR, dAS_EXPR);
void AS_CAL577747697_unbox(AS_CALL_EXPR, dAS_EXPR);
void AS_CAS1011726673_unbox(AS_CASE_STMT, BOOL);
void AS_CAS1160155324_unbox(AS_CASE_STMT, SFILE_ID);
void AS_CAS1578756635_unbox(AS_CASE_STMT, dAS_STMT);
void AS_CAS490112615_unbox(AS_CASE_STMT, dAS_STMT);
void AS_CAS703201798_unbox(AS_CASE_STMT, AS_STMT_LIST);
void AS_CHA1047100373_unbox(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA340061339_unbox(AS_CHAR_LIT_EXPR, dAS_EXPR);
void AS_CHA604963579_unbox(AS_CHAR_LIT_EXPR, SFILE_ID);
void AS_CLU1354319231_unbox(AS_CLU553570599, dAS_EXPR);
void AS_CLU1700532850_unbox(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU1903972459_unbox(AS_CLU553570599, SFILE_ID);
void AS_CLU726419888_unbox(AS_CLUSTER_EXPR, dAS_EXPR);
void AS_CLU738930885_unbox(AS_CLU553570599, dAS_EXPR);
void AS_CLU942370494_unbox(AS_CLUSTER_EXPR, SFILE_ID);
void AS_COH1216011035_unbox(AS_COHORT_EXPR, SFILE_ID);
void AS_COH278881603_unbox(AS_COHORT_EXPR, dAS_EXPR);
void AS_COH436052917_unbox(AS_COHORT_EXPR, dAS_EXPR);
void AS_CON1753066199_unbox(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CON1962795038_unbox(AS_CONST_DEF, BOOL);
void AS_CON737776233_unbox(AS_CONST_DEF, dAS_CLASS_ELT);
void AS_CRE1032653736_unbox(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE1964853282_unbox(AS_CREATE_EXPR, dAS_EXPR);
void AS_CRE312789330_unbox(AS_CREATE_EXPR, SFILE_ID);
void AS_DEC1092435373_unbox(AS_DEC_STMT, SFILE_ID);
void AS_DEC1996561535_unbox(AS_DEC_STMT, BOOL);
void AS_DEC2062280289_unbox(AS_DEC_STMT, AS_STMT_LIST);
void AS_DEC557832566_unbox(AS_DEC_STMT, dAS_STMT);
void AS_DEC646247808_unbox(AS_DEC_STMT, dAS_STMT);
void AS_EXC1647038221_unbox(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXC5025731_unbox(AS_EXCEPT_EXPR, SFILE_ID);
void AS_EXC645762341_unbox(AS_EXCEPT_EXPR, dAS_EXPR);
void AS_EXP1321747790_unbox(AS_EXPR_STMT, BOOL);
void AS_EXP1630058180_unbox(AS_EXPR_STMT, dAS_STMT);
void AS_EXP1794823411_unbox(AS_EXPR_STMT, SFILE_ID);
void AS_EXP200906119_unbox(AS_EXPR_STMT, AS_STMT_LIST);
void AS_EXP849875946_unbox(AS_EXPR_STMT, dAS_STMT);
void AS_FAR1938606563_unbox(AS_FAR_EXPR, dAS_EXPR);
void AS_FAR286542611_unbox(AS_FAR_EXPR, SFILE_ID);
void AS_FAR50083859_unbox(AS_FAR_EXPR, dAS_EXPR);
void AS_FLT1911942022_unbox(AS_FLT_LIT_EXPR, SFILE_ID);
void AS_FLT259878070_unbox(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FLT943209132_unbox(AS_FLT_LIT_EXPR, dAS_EXPR);
void AS_FOR1019127704_unbox(AS_FORK_STMT, AS_STMT_LIST);
void AS_FOR103324571_unbox(AS_FORK_STMT, dAS_STMT);
void AS_FOR1425734093_unbox(AS_FORK_STMT, dAS_STMT);
void AS_FOR1673645269_unbox(AS_FORK_STMT, BOOL);
void AS_FOR1753592510_unbox(AS_FORK_STMT, SFILE_ID);
void AS_GLO1712525775_unbox(AS_GLOBAL_EXPR, SFILE_ID);
void AS_GLO275096477_unbox(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_GLO930377569_unbox(AS_GLOBAL_EXPR, dAS_EXPR);
void AS_HER1301328495_unbox(AS_HERE_EXPR, dAS_EXPR);
void AS_HER1479869719_unbox(AS_HERE_EXPR, SFILE_ID);
void AS_HER172194233_unbox(AS_HERE_EXPR, dAS_EXPR);
void AS_IF_164980229_unbox(AS_IF_STMT, BOOL);
void AS_IF_1658967836_unbox(AS_IF_STMT, AS_STMT_LIST);
void AS_IF_1955278743_unbox(AS_IF_STMT, dAS_STMT);
void AS_IF_671597921_unbox(AS_IF_STMT, dAS_STMT);
void AS_IF_978670018_unbox(AS_IF_STMT, SFILE_ID);
void AS_INC1475294123_unbox(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC1807545759_unbox(AS_INC1416917001, dAS_CLASS_ELT);
void AS_INC874191011_unbox(AS_INC1416917001, BOOL);
void AS_INI1852876520_unbox(AS_INITIAL_EXPR, SFILE_ID);
void AS_INI414564886_unbox(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INI790026824_unbox(AS_INITIAL_EXPR, dAS_EXPR);
void AS_INT1009823030_unbox(AS_INT2031239268, AS_STMT_LIST);
void AS_INT1251874027_unbox(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT2141902673_unbox(AS_INT_LIT_EXPR, SFILE_ID);
void AS_INT501000671_unbox(AS_INT_LIT_EXPR, dAS_EXPR);
void AS_INT685357691_unbox(AS_INT2031239268, dAS_STMT);
void AS_INT765687160_unbox(AS_INT2031239268, SFILE_ID);
void AS_INT83312974_unbox(AS_INT2031239268, BOOL);
void AS_INT884580779_unbox(AS_INT2031239268, dAS_STMT);
void AS_IS_128975537_unbox(AS_IS_VOID_EXPR, SFILE_ID);
void AS_IS_1523088415_unbox(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_IS_936250681_unbox(AS_IS_VOID_EXPR, dAS_EXPR);
void AS_LOC1027127163_unbox(AS_LOCK_STMT, SFILE_ID);
void AS_LOC1144520043_unbox(AS_LOCK_STMT, AS_STMT_LIST);
void AS_LOC623140776_unbox(AS_LOCK_STMT, dAS_STMT);
void AS_LOC755705564_unbox(AS_LOCK_STMT, BOOL);
void AS_LOC793158658_unbox(AS_LOCK_STMT, dAS_STMT);
void AS_LOO1017100380_unbox(AS_LOOP_STMT, SFILE_ID);
void AS_LOO1542269018_unbox(AS_LOOP_STMT, AS_STMT_LIST);
void AS_LOO507592891_unbox(AS_LOOP_STMT, dAS_STMT);
void AS_LOO565196687_unbox(AS_LOOP_STMT, BOOL);
void AS_LOO633167559_unbox(AS_LOOP_STMT, dAS_STMT);
void AS_NEA1203816563_unbox(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEA1439086781_unbox(AS_NEAR_EXPR, SFILE_ID);
void AS_NEA1986170077_unbox(AS_NEAR_EXPR, dAS_EXPR);
void AS_NEW1985034628_unbox(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW2117994542_unbox(AS_NEW_EXPR, dAS_EXPR);
void AS_NEW524908802_unbox(AS_NEW_EXPR, SFILE_ID);
void AS_OR_1885251509_unbox(AS_OR_EXPR, dAS_EXPR);
void AS_OR_784015075_unbox(AS_OR_EXPR, SFILE_ID);
void AS_OR_868048877_unbox(AS_OR_EXPR, dAS_EXPR);
void AS_PAR1480073124_unbox(AS_PAR_STMT, SFILE_ID);
void AS_PAR170194815_unbox(AS_PAR_STMT, dAS_STMT);
void AS_PAR771744212_unbox(AS_PAR_STMT, BOOL);
void AS_PAR865797855_unbox(AS_PAR_STMT, dAS_STMT);
void AS_PAR99672090_unbox(AS_PAR_STMT, AS_STMT_LIST);
void AS_PRO169226468_unbox(AS_PROTECT_STMT, BOOL);
void AS_PRO1754180698_unbox(AS_PROTECT_STMT, AS_STMT_LIST);
void AS_PRO1882650168_unbox(AS_PROTECT_STMT, SFILE_ID);
void AS_PRO232382229_unbox(AS_PROTECT_STMT, dAS_STMT);
void AS_PRO736041443_unbox(AS_PROTECT_STMT, dAS_STMT);
void AS_QUI1389603990_unbox(AS_QUIT_STMT, dAS_STMT);
void AS_QUI1442696788_unbox(AS_QUIT_STMT, BOOL);
void AS_QUI1673525760_unbox(AS_QUIT_STMT, dAS_STMT);
void AS_QUI2137045949_unbox(AS_QUIT_STMT, AS_STMT_LIST);
void AS_QUI971173597_unbox(AS_QUIT_STMT, SFILE_ID);
void AS_RAI1860022589_unbox(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2032317945_unbox(AS_RAISE_STMT, dAS_STMT);
void AS_RAI2115288988_unbox(AS_RAISE_STMT, AS_STMT_LIST);
void AS_RAI248856852_unbox(AS_RAISE_STMT, BOOL);
void AS_RAI382050006_unbox(AS_RAISE_STMT, SFILE_ID);
void AS_RES1196907749_unbox(AS_RESULT_EXPR, SFILE_ID);
void AS_RES1201611453_unbox(AS_RESULT_EXPR, dAS_EXPR);
void AS_RES1445995595_unbox(AS_RESULT_EXPR, dAS_EXPR);
void AS_RET1042240335_unbox(AS_RETURN_STMT, AS_STMT_LIST);
void AS_RET1097577252_unbox(AS_RETURN_STMT, dAS_STMT);
void AS_RET1414994250_unbox(AS_RETURN_STMT, dAS_STMT);
void AS_RET1547122105_unbox(AS_RETURN_STMT, SFILE_ID);
void AS_RET910390234_unbox(AS_RETURN_STMT, BOOL);
void AS_ROU1496248876_unbox(AS_ROUT_DEF, BOOL);
void AS_ROU166638356_unbox(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_ROU920632016_unbox(AS_ROUT_DEF, dAS_CLASS_ELT);
void AS_SEL1190661345_unbox(AS_SELF_EXPR, SFILE_ID);
void AS_SEL1452241999_unbox(AS_SELF_EXPR, dAS_EXPR);
void AS_SEL987495847_unbox(AS_SELF_EXPR, dAS_EXPR);
void AS_SHA1271069808_unbox(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA746153340_unbox(AS_SHARED_DEF, dAS_CLASS_ELT);
void AS_SHA905735878_unbox(AS_SHARED_DEF, BOOL);
void AS_STM1692300976_unbox(AS_STMT, BOOL);
void AS_STM1734576793_unbox(AS_STMT, dAS_STMT);
void AS_STM193774686_unbox(AS_STMT, AS_STMT_LIST);
void AS_STM910122564_unbox(AS_STMT, SFILE_ID);
void AS_STM984922713_unbox(AS_STMT, dAS_STMT);
void AS_STR1192634823_unbox(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR2077487265_unbox(AS_STR_LIT_EXPR, dAS_EXPR);
void AS_STR459429129_unbox(AS_STR_LIT_EXPR, SFILE_ID);
void AS_SYN1248387412_unbox(AS_SYNC_STMT, BOOL);
void AS_SYN1817260677_unbox(AS_SYNC_STMT, AS_STMT_LIST);
void AS_SYN2089058674_unbox(AS_SYNC_STMT, dAS_STMT);
void AS_SYN695597675_unbox(AS_SYNC_STMT, SFILE_ID);
void AS_SYN954670264_unbox(AS_SYNC_STMT, dAS_STMT);
void AS_TYP1218814867_unbox(AS_TYPECASE_STMT, BOOL);
void AS_TYP1661742497_unbox(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP1802831442_unbox(AS_TYPECASE_STMT, AS_STMT_LIST);
void AS_TYP823282803_unbox(AS_TYPECASE_STMT, dAS_STMT);
void AS_TYP982956860_unbox(AS_TYPECASE_STMT, SFILE_ID);
void AS_UND165529988_unbox(AS_UND152986614, dAS_EXPR);
void AS_UND1829175574_unbox(AS_UND152986614, dAS_EXPR);
void AS_UND813727770_unbox(AS_UND152986614, SFILE_ID);
void AS_UNL1079389410_unbox(AS_UNLOCK_STMT, BOOL);
void AS_UNL1435368086_unbox(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL1962410248_unbox(AS_UNLOCK_STMT, dAS_STMT);
void AS_UNL312142309_unbox(AS_UNLOCK_STMT, SFILE_ID);
void AS_UNL619513787_unbox(AS_UNLOCK_STMT, AS_STMT_LIST);
void AS_VOI1015295839_unbox(AS_VOID_EXPR, SFILE_ID);
void AS_VOI291335015_unbox(AS_VOID_EXPR, dAS_EXPR);
void AS_VOI636768113_unbox(AS_VOID_EXPR, dAS_EXPR);
void AS_WHE136004678_unbox(AS_WHERE_EXPR, dAS_EXPR);
void AS_WHE666102736_unbox(AS_WHERE_EXPR, SFILE_ID);
void AS_WHE985961216_unbox(AS_WHERE_EXPR, dAS_EXPR);
void AS_WIT1613050679_unbox(AS_WIT1063437351, BOOL);
void AS_WIT178530912_unbox(AS_WIT1063437351, dAS_STMT);
void AS_WIT1810155147_unbox(AS_WIT1063437351, SFILE_ID);
void AS_WIT1949427223_unbox(AS_WIT1063437351, AS_STMT_LIST);
void AS_WIT834544210_unbox(AS_WIT1063437351, dAS_STMT);
void AS_YIE1194255817_unbox(AS_YIELD_STMT, AS_STMT_LIST);
void AS_YIE1468566771_unbox(AS_YIELD_STMT, BOOL);
void AS_YIE1941927228_unbox(AS_YIELD_STMT, dAS_STMT);
void AS_YIE291659289_unbox(AS_YIELD_STMT, SFILE_ID);
void AS_YIE745261198_unbox(AS_YIELD_STMT, dAS_STMT);
void BUILD_1202586967_unbox(BUILD_TYPE_GRAPH);
void CGEN_e103713585_unbox(CGEN, AM_ROUT_DEF);
void CGEN_finalize_unbox(CGEN);
void CGEN_init_unbox(CGEN);
void CHECK_1724422536_unbox(CHECK_1174552075);
void CHECK_801221867_unbox(CHECK_AM);
void CS_OPT63514076_unbox(CS_OPTIONS, ARRAYSTR);
void CS_OPT994959939_unbox(CS_OPTIONS);
void FIND_T194079625_unbox(FIND_TYPES);
void GENERA1485011776_unbox(GENERATE_AM);
void GENERA88776523_unbox(GENERATE_AM, SIG);
void GET_MA42890279_unbox(GET_MAIN_SIG, STR);
void INLINE1765310158_unbox(INLINE, AM_ROUT_DEF);
void INLINE_init_unbox(INLINE);
void NULL_S1721445066_unbox(NULL_STAT, STR);
void NULL_STAT_print_unbox(NULL_STAT);
void OPTIMI1081780129_unbox(OPTIMIZE, AM_ROUT_DEF);
void OPTIMI333751220_unbox(OPTIMIZE);
void OPTIMI462748081_unbox(OPTIMIZE, AM_ROUT_DEF);
void OPTIMIZE_init_unbox(OPTIMIZE);
void PARSE_parse_STR_unbox(PARSE, STR);
void STAT_incr_STR_unbox(STAT1, STR);
void STAT_print_unbox(STAT1);

/* External calls */


/* Code */



dNIL INT_nilrINT_unbox(INT_boxed self) {

 INT L1;
 dNIL L2;
 OB L3; 
L1 = ((INT_boxed) self)->immutable_part;
L3=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
if (L3==NULL) FATAL("Unable to allocate more memory");
memset(L3,0,sizeof(struct INT_boxed_struct));
((OB)L3)->header.tag=INT_tag;
L2 = (dNIL)((INT_boxed) L3);
((INT_boxed) L2)->immutable_part = INT_nilrINT(L1);
 return L2;
}


dNIL FLTD_nilrFLTD_unbox(FLTD_boxed self) {

 FLTD L4;
 dNIL L5;
 OB L6; 
L4 = ((FLTD_boxed) self)->immutable_part;
L6=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
if (L6==NULL) FATAL("Unable to allocate more memory");
memset(L6,0,sizeof(struct FLTD_boxed_struct));
((OB)L6)->header.tag=FLTD_tag;
L5 = (dNIL)((FLTD_boxed) L6);
((FLTD_boxed) L5)->immutable_part = FLTD_nilrFLTD(L4);
 return L5;
}


dNIL FLT_nilrFLT_unbox(FLT_boxed self) {

 FLT L7;
 dNIL L8;
 OB L9; 
L7 = ((FLT_boxed) self)->immutable_part;
L9=ZALLOC_LEAF(sizeof(struct FLT_boxed_struct));
if (L9==NULL) FATAL("Unable to allocate more memory");
memset(L9,0,sizeof(struct FLT_boxed_struct));
((OB)L9)->header.tag=FLT_tag;
L8 = (dNIL)((FLT_boxed) L9);
((FLT_boxed) L8)->immutable_part = FLT_nilrFLT(L7);
 return L8;
}


STR ABSTRA1984535400_unbox(ABSTRA151498155 self) {
 
 return ABSTRA1984535400(((ABSTRA151498155) self));
}


STR ABSTRA1464296195_unbox(ABSTRACT_LAYOUT self) {
 
 return ABSTRA1464296195(((ABSTRACT_LAYOUT) self));
}


STR ARG_LA635972259_unbox(ARG_LAYOUT self) {
 
 return ARG_LA635972259(((ARG_LAYOUT) self));
}


STR BOUND_1856529942_unbox(BOUND_1007407331 self) {
 
 return BOUND_1856529942(((BOUND_1007407331) self));
}


STR BOUND_1245789017_unbox(BOUND_2140373124 self) {
 
 return BOUND_1245789017(((BOUND_2140373124) self));
}


STR BOUND_787102169_unbox(BOUND_1677815802 self) {
 
 return BOUND_787102169(((BOUND_1677815802) self));
}


STR BOUND_619031357_unbox(BOUND_809390774 self) {
 
 return BOUND_619031357(((BOUND_809390774) self));
}


STR BUILTI1940169806_unbox(BUILTIN_LAYOUT self) {
 
 return BUILTI1940169806(((BUILTIN_LAYOUT) self));
}


STR CLASS_1824950980_unbox(CLASS_LAYOUT self) {
 
 return CLASS_1824950980(((CLASS_LAYOUT) self));
}


STR EXTERN1283013626_unbox(EXTERNAL_LAYOUT self) {
 
 return EXTERN1283013626(((EXTERNAL_LAYOUT) self));
}


STR FRAME_1195781105_unbox(FRAME_LAYOUT self) {
 
 return FRAME_1195781105(((FRAME_LAYOUT) self));
}


STR IMMUTA966323602_unbox(IMMUTA421759893 self) {
 
 return IMMUTA966323602(((IMMUTA421759893) self));
}


STR TP_LAY473390218_unbox(TP_LAYOUT self) {
 
 return TP_LAY473390218(((TP_LAYOUT) self));
}


STR INOUT_1907813337_unbox(INOUT_MODE self) {
 
 return INOUT_1907813337(((INOUT_MODE) self));
}


STR IN_MODE_strrSTR_unbox(IN_MODE self) {
 
 return IN_MODE_strrSTR(((IN_MODE) self));
}


STR ONCE_M1994645546_unbox(ONCE_MODE self) {
 
 return ONCE_M1994645546(((ONCE_MODE) self));
}


STR OUT_MODE_strrSTR_unbox(OUT_MODE self) {
 
 return OUT_MODE_strrSTR(((OUT_MODE) self));
}


STR TP_CLASS_strrSTR_unbox(TP_CLASS self) {
 
 return TP_CLASS_strrSTR(((TP_CLASS) self));
}


STR TP_ITER_strrSTR_unbox(TP_ITER self) {
 
 return TP_ITER_strrSTR(((TP_ITER) self));
}


STR TP_ROUT_strrSTR_unbox(TP_ROUT self) {
 
 return TP_ROUT_strrSTR(((TP_ROUT) self));
}


STR CONFIG1221082662_unbox(CONFIG_DEF self) {
 
 return CONFIG1221082662(((CONFIG_DEF) self));
}


STR CONFIG336048123_unbox(CONFIG_ROUT self) {
 
 return CONFIG336048123(((CONFIG_ROUT) self));
}


BOOL INT_is_nilrBOOL_unbox(INT_boxed self) {

 INT L10; 
L10 = ((INT_boxed) self)->immutable_part;
 return INT_is_nilrBOOL(L10);
}


BOOL FLTD_is_nilrBOOL_unbox(FLTD_boxed self) {

 FLTD L11; 
L11 = ((FLTD_boxed) self)->immutable_part;
 return FLTD_is_nilrBOOL(L11);
}


BOOL FLT_is_nilrBOOL_unbox(FLT_boxed self) {

 FLT L12; 
L12 = ((FLT_boxed) self)->immutable_part;
 return FLT_is_nilrBOOL(L12);
}


STR TUPTP_371689873_unbox(TUPTP_1668454172_boxed self) {

 TUPTP_1668454172 L13; 
L13 = ((TUPTP_1668454172_boxed) self)->immutable_part;
 return TUPTP_371689873(L13);
}


STR TUPTP_131070026_unbox(TUPTP_858321267_boxed self) {

 TUPTP_858321267 L14; 
L14 = ((TUPTP_858321267_boxed) self)->immutable_part;
 return TUPTP_131070026(L14);
}


STR TUPSTR1730971028_unbox(TUPSTRSTR_boxed self) {

 TUPSTRSTR L15; 
L15 = ((TUPSTRSTR_boxed) self)->immutable_part;
 return TUPSTR1730971028(L15);
}


STR TUPSTR1951895232_unbox(TUPSTRSIG_boxed self) {

 TUPSTRSIG L16; 
L16 = ((TUPSTRSIG_boxed) self)->immutable_part;
 return TUPSTR1951895232(L16);
}


STR TUPSTR1520476266_unbox(TUPSTRINT_boxed self) {

 TUPSTRINT L17; 
L17 = ((TUPSTRINT_boxed) self)->immutable_part;
 return TUPSTR1520476266(L17);
}


STR TUPSTR1622911390_unbox(TUPSTRFSETSTR_boxed self) {

 TUPSTRFSETSTR L18; 
L18 = ((TUPSTRFSETSTR_boxed) self)->immutable_part;
 return TUPSTR1622911390(L18);
}


STR TUPSTR1075084367_unbox(TUPSTRdCONFIG_boxed self) {

 TUPSTRdCONFIG L19; 
L19 = ((TUPSTRdCONFIG_boxed) self)->immutable_part;
 return TUPSTR1075084367(L19);
}


STR TUPSIG926630776_unbox(TUPSIGSTR_boxed self) {

 TUPSIGSTR L20; 
L20 = ((TUPSIGSTR_boxed) self)->immutable_part;
 return TUPSIG926630776(L20);
}


STR TUPSIG1147554980_unbox(TUPSIGSIG_boxed self) {

 TUPSIGSIG L21; 
L21 = ((TUPSIGSIG_boxed) self)->immutable_part;
 return TUPSIG1147554980(L21);
}


STR TUPSIG1970150778_unbox(TUPSIGINT_boxed self) {

 TUPSIGINT L22; 
L22 = ((TUPSIGINT_boxed) self)->immutable_part;
 return TUPSIG1970150778(L22);
}


STR TUPSIG659332932_unbox(TUPSIG2023533247_boxed self) {

 TUPSIG2023533247 L23; 
L23 = ((TUPSIG2023533247_boxed) self)->immutable_part;
 return TUPSIG659332932(L23);
}


STR TUPSIG610691414_unbox(TUPSIGARG_LAYOUT_boxed self) {

 TUPSIGARG_LAYOUT L24; 
L24 = ((TUPSIGARG_LAYOUT_boxed) self)->immutable_part;
 return TUPSIG610691414(L24);
}


STR TUPSIG1626178992_unbox(TUPSIG578692189_boxed self) {

 TUPSIG578692189 L25; 
L25 = ((TUPSIG578692189_boxed) self)->immutable_part;
 return TUPSIG1626178992(L25);
}


STR TUPSIG1001949053_unbox(TUPSIG1754078736_boxed self) {

 TUPSIG1754078736 L26; 
L26 = ((TUPSIG1754078736_boxed) self)->immutable_part;
 return TUPSIG1001949053(L26);
}


STR TUPINT567702316_unbox(TUPINTINT_boxed self) {

 TUPINTINT L27; 
L27 = ((TUPINTINT_boxed) self)->immutable_part;
 return TUPINT567702316(L27);
}


STR TUPIDE2120384756_unbox(TUPIDENTINT_boxed self) {

 TUPIDENTINT L28; 
L28 = ((TUPIDENTINT_boxed) self)->immutable_part;
 return TUPIDE2120384756(L28);
}


STR TUPIDE1923523563_unbox(TUPIDENTARRAYdTP_boxed self) {

 TUPIDENTARRAYdTP L29; 
L29 = ((TUPIDENTARRAYdTP_boxed) self)->immutable_part;
 return TUPIDE1923523563(L29);
}


STR TUPIDE1477036401_unbox(TUPIDE641654124_boxed self) {

 TUPIDE641654124 L30; 
L30 = ((TUPIDE641654124_boxed) self)->immutable_part;
 return TUPIDE1477036401(L30);
}


STR TUPIDE2057908249_unbox(TUPIDENTdTP_boxed self) {

 TUPIDENTdTP L31; 
L31 = ((TUPIDENTdTP_boxed) self)->immutable_part;
 return TUPIDE2057908249(L31);
}


STR TUPBOO1584822636_unbox(TUPBOO1392923241_boxed self) {

 TUPBOO1392923241 L32; 
L32 = ((TUPBOO1392923241_boxed) self)->immutable_part;
 return TUPBOO1584822636(L32);
}


STR TUPARR735320083_unbox(TUPARR1759987776_boxed self) {

 TUPARR1759987776 L33; 
L33 = ((TUPARR1759987776_boxed) self)->immutable_part;
 return TUPARR735320083(L33);
}


STR TUPARR1404914248_unbox(TUPARRAYARGdTP_boxed self) {

 TUPARRAYARGdTP L34; 
L34 = ((TUPARRAYARGdTP_boxed) self)->immutable_part;
 return TUPARR1404914248(L34);
}


STR TUPAM_525468809_unbox(TUPAM_653469574_boxed self) {

 TUPAM_653469574 L35; 
L35 = ((TUPAM_653469574_boxed) self)->immutable_part;
 return TUPAM_525468809(L35);
}


STR TUPAM_85614060_unbox(TUPAM_1024994801_boxed self) {

 TUPAM_1024994801 L36; 
L36 = ((TUPAM_1024994801_boxed) self)->immutable_part;
 return TUPAM_85614060(L36);
}


STR TUPAM_504028066_unbox(TUPAM_504653531_boxed self) {

 TUPAM_504653531 L37; 
L37 = ((TUPAM_504653531_boxed) self)->immutable_part;
 return TUPAM_504028066(L37);
}


STR TUPAM_1694719258_unbox(TUPAM_1519040877_boxed self) {

 TUPAM_1519040877 L38; 
L38 = ((TUPAM_1519040877_boxed) self)->immutable_part;
 return TUPAM_1694719258(L38);
}


STR TUPAM_489161612_unbox(TUPAM_927437481_boxed self) {

 TUPAM_927437481 L39; 
L39 = ((TUPAM_927437481_boxed) self)->immutable_part;
 return TUPAM_489161612(L39);
}


STR TUPAM_827739609_unbox(TUPAM_667892060_boxed self) {

 TUPAM_667892060 L40; 
L40 = ((TUPAM_667892060_boxed) self)->immutable_part;
 return TUPAM_827739609(L40);
}


STR TUPAM_1128362628_unbox(TUPAM_304458649_boxed self) {

 TUPAM_304458649 L41; 
L41 = ((TUPAM_304458649_boxed) self)->immutable_part;
 return TUPAM_1128362628(L41);
}


STR TUPdTP1972747229_unbox(TUPdTPINT_boxed self) {

 TUPdTPINT L42; 
L42 = ((TUPdTPINT_boxed) self)->immutable_part;
 return TUPdTP1972747229(L42);
}


STR TUPdTP2068651704_unbox(TUPdTP1710233051_boxed self) {

 TUPdTP1710233051 L43; 
L43 = ((TUPdTP1710233051_boxed) self)->immutable_part;
 return TUPdTP2068651704(L43);
}


STR TUPdTP1477319287_unbox(TUPdTPCODE_FILE_boxed self) {

 TUPdTPCODE_FILE L44; 
L44 = ((TUPdTPCODE_FILE_boxed) self)->immutable_part;
 return TUPdTP1477319287(L44);
}


STR TUPdTP1335739968_unbox(TUPdTPdLAYOUT_boxed self) {

 TUPdTPdLAYOUT L45; 
L45 = ((TUPdTPdLAYOUT_boxed) self)->immutable_part;
 return TUPdTP1335739968(L45);
}


STR TUPdOB1460589826_unbox(TUPdOBSTR_boxed self) {

 TUPdOBSTR L46; 
L46 = ((TUPdOBSTR_boxed) self)->immutable_part;
 return TUPdOB1460589826(L46);
}


STR TUPdOB1872884181_unbox(TUPdOBNAMESPACE_boxed self) {

 TUPdOBNAMESPACE L47; 
L47 = ((TUPdOBNAMESPACE_boxed) self)->immutable_part;
 return TUPdOB1872884181(L47);
}


STR TUPdAS100511776_unbox(TUPdAS268679779_boxed self) {

 TUPdAS268679779 L48; 
L48 = ((TUPdAS268679779_boxed) self)->immutable_part;
 return TUPdAS100511776(L48);
}


STR TUPdAM1835151881_unbox(TUPdAM_EXPRdTP_boxed self) {

 TUPdAM_EXPRdTP L49; 
L49 = ((TUPdAM_EXPRdTP_boxed) self)->immutable_part;
 return TUPdAM1835151881(L49);
}


STR SFILE_ID_strrSTR_unbox(SFILE_ID_boxed self) {

 SFILE_ID L50; 
L50 = ((SFILE_ID_boxed) self)->immutable_part;
 return SFILE_ID_strrSTR(L50);
}


STR RAT_strrSTR_unbox(RAT_boxed self) {

 RAT L51; 
L51 = ((RAT_boxed) self)->immutable_part;
 return RAT_strrSTR(L51);
}


STR INT_strrSTR_unbox(INT_boxed self) {

 INT L52; 
L52 = ((INT_boxed) self)->immutable_part;
 return INT_strrSTR(L52);
}


STR FMT_ER330399541_unbox(FMT_ERROR_boxed self) {

 FMT_ERROR L53; 
L53 = ((FMT_ERROR_boxed) self)->immutable_part;
 return FMT_ER330399541(L53);
}


STR FLTD_strrSTR_unbox(FLTD_boxed self) {

 FLTD L54; 
L54 = ((FLTD_boxed) self)->immutable_part;
 return FLTD_strrSTR(L54);
}


STR FLT_strrSTR_unbox(FLT_boxed self) {

 FLT L55; 
L55 = ((FLT_boxed) self)->immutable_part;
 return FLT_strrSTR(L55);
}


STR CHAR_strrSTR_unbox(CHAR_boxed self) {

 CHAR L56; 
L56 = ((CHAR_boxed) self)->immutable_part;
 return CHAR_strrSTR(L56);
}


STR BOOL_strrSTR_unbox(BOOL_boxed self) {

 BOOL L57; 
L57 = ((BOOL_boxed) self)->immutable_part;
 return BOOL_strrSTR(L57);
}


STR ARRAYd2065245377_unbox(ARRAYdAM_CONST self) {
 
 return ARRAYd2065245377(((ARRAYdAM_CONST) self));
}


STR ARRAYd2023686048_unbox(ARRAYdAM_EXPR self) {
 
 return ARRAYd2023686048(((ARRAYdAM_EXPR) self));
}


STR ARRAYd1085727763_unbox(ARRAYdAM_STMT self) {
 
 return ARRAYd1085727763(((ARRAYdAM_STMT) self));
}


STR ARRAYd946747660_unbox(ARRAYdLAYOUT self) {
 
 return ARRAYd946747660(((ARRAYdLAYOUT) self));
}


STR ARRAYdTP_strrSTR_unbox(ARRAYdTP self) {
 
 return ARRAYdTP_strrSTR(((ARRAYdTP) self));
}


STR ARRAYA1490975097_unbox(ARRAYA1870808380 self) {
 
 return ARRAYA1490975097(((ARRAYA1870808380) self));
}


STR ARRAYA1621913879_unbox(ARRAYA855692714 self) {
 
 return ARRAYA1621913879(((ARRAYA855692714) self));
}


STR ARRAYA92706474_unbox(ARRAYA132553625 self) {
 
 return ARRAYA92706474(((ARRAYA132553625) self));
}


STR ARRAYA1826145484_unbox(ARRAYAM_CALL_ARG self) {
 
 return ARRAYA1826145484(((ARRAYAM_CALL_ARG) self));
}


STR ARRAYA56089049_unbox(ARRAYA36280886 self) {
 
 return ARRAYA56089049(((ARRAYA36280886) self));
}


STR ARRAYA1836477836_unbox(ARRAYAM_CURSOR self) {
 
 return ARRAYA1836477836(((ARRAYAM_CURSOR) self));
}


STR ARRAYA175904446_unbox(ARRAYA1347292543 self) {
 
 return ARRAYA175904446(((ARRAYA1347292543) self));
}


STR ARRAYA208544369_unbox(ARRAYA1369744492 self) {
 
 return ARRAYA208544369(((ARRAYA1369744492) self));
}


STR ARRAYA704584979_unbox(ARRAYA1575632848 self) {
 
 return ARRAYA704584979(((ARRAYA1575632848) self));
}


STR ARRAYA1368915033_unbox(ARRAYAM_ROUT_DEF self) {
 
 return ARRAYA1368915033(((ARRAYAM_ROUT_DEF) self));
}


STR ARRAYA449689863_unbox(ARRAYA1135548710 self) {
 
 return ARRAYA449689863(((ARRAYA1135548710) self));
}


STR ARRAYARG_strrSTR_unbox(ARRAYARG self) {
 
 return ARRAYARG_strrSTR(((ARRAYARG) self));
}


STR ARRAYA1523920654_unbox(ARRAYA477251019 self) {
 
 return ARRAYA1523920654(((ARRAYA477251019) self));
}


STR ARRAYA881422470_unbox(ARRAYARRAYBOOL self) {
 
 return ARRAYA881422470(((ARRAYARRAYBOOL) self));
}


STR ARRAYA1329352756_unbox(ARRAYARRAYSTR self) {
 
 return ARRAYA1329352756(((ARRAYARRAYSTR) self));
}


STR ARRAYA565856506_unbox(ARRAYAS_FEAT_MOD self) {
 
 return ARRAYA565856506(((ARRAYAS_FEAT_MOD) self));
}


STR ARRAYA1607624311_unbox(ARRAYATTRSORT self) {
 
 return ARRAYA1607624311(((ARRAYATTRSORT) self));
}


STR ARRAYB1997509424_unbox(ARRAYBOOL self) {
 
 return ARRAYB1997509424(((ARRAYBOOL) self));
}


STR ARRAYB799780672_unbox(ARRAYB1234765325 self) {
 
 return ARRAYB799780672(((ARRAYB1234765325) self));
}


STR ARRAYB422115773_unbox(ARRAYB1925477728 self) {
 
 return ARRAYB422115773(((ARRAYB1925477728) self));
}


STR ARRAYB1034562007_unbox(ARRAYB1039347268 self) {
 
 return ARRAYB1034562007(((ARRAYB1039347268) self));
}


STR ARRAYC15432117_unbox(ARRAYCALL_ARG self) {
 
 return ARRAYC15432117(((ARRAYCALL_ARG) self));
}


STR ARRAYC1188196456_unbox(ARRAYCHAR self) {
 
 return ARRAYC1188196456(((ARRAYCHAR) self));
}


STR ARRAYC861356497_unbox(ARRAYC1438655838 self) {
 
 return ARRAYC861356497(((ARRAYC1438655838) self));
}


STR ARRAYC1509950809_unbox(ARRAYCODE_FILE self) {
 
 return ARRAYC1509950809(((ARRAYCODE_FILE) self));
}


STR ARRAYC1557608670_unbox(ARRAYC1788421899 self) {
 
 return ARRAYC1557608670(((ARRAYC1788421899) self));
}


STR ARRAYE1673119298_unbox(ARRAYE2126408533 self) {
 
 return ARRAYE1673119298(((ARRAYE2126408533) self));
}


STR ARRAYE1256334724_unbox(ARRAYE406487263 self) {
 
 return ARRAYE1256334724(((ARRAYE406487263) self));
}


STR ARRAYF852228085_unbox(ARRAYF1030817442 self) {
 
 return ARRAYF852228085(((ARRAYF1030817442) self));
}


STR ARRAYF1350792049_unbox(ARRAYFLISTSTR self) {
 
 return ARRAYF1350792049(((ARRAYFLISTSTR) self));
}


STR ARRAYF378827521_unbox(ARRAYFSTR self) {
 
 return ARRAYF378827521(((ARRAYFSTR) self));
}


STR ARRAYI1551332689_unbox(ARRAYIDENT self) {
 
 return ARRAYI1551332689(((ARRAYIDENT) self));
}


STR ARRAYIFC_strrSTR_unbox(ARRAYIFC self) {
 
 return ARRAYIFC_strrSTR(((ARRAYIFC) self));
}


STR ARRAYI1079246087_unbox(ARRAYI1327367692 self) {
 
 return ARRAYI1079246087(((ARRAYI1327367692) self));
}


STR ARRAYI1980470469_unbox(ARRAYI1991954990 self) {
 
 return ARRAYI1980470469(((ARRAYI1991954990) self));
}


STR ARRAYINT_strrSTR_unbox(ARRAYINT self) {
 
 return ARRAYINT_strrSTR(((ARRAYINT) self));
}


STR ARRAYO2133079268_unbox(ARRAYO570594127 self) {
 
 return ARRAYO2133079268(((ARRAYO570594127) self));
}


STR ARRAYS1170645309_unbox(ARRAYSFILE_ID self) {
 
 return ARRAYS1170645309(((ARRAYSFILE_ID) self));
}


STR ARRAYS2043945973_unbox(ARRAYSIDE_EFFECT self) {
 
 return ARRAYS2043945973(((ARRAYSIDE_EFFECT) self));
}


STR ARRAYSIG_strrSTR_unbox(ARRAYSIG self) {
 
 return ARRAYSIG_strrSTR(((ARRAYSIG) self));
}


STR ARRAYSTR_strrSTR_unbox(ARRAYSTR self) {
 
 return ARRAYSTR_strrSTR(((ARRAYSTR) self));
}


STR ARRAYT1733874469_unbox(ARRAYTP_CLASS self) {
 
 return ARRAYT1733874469(((ARRAYTP_CLASS) self));
}


STR ARRAYT1589477172_unbox(ARRAYTP_LAYOUT self) {
 
 return ARRAYT1589477172(((ARRAYTP_LAYOUT) self));
}


STR ARRAYT1846205279_unbox(ARRAYT747572380 self) {
 
 return ARRAYT1846205279(((ARRAYT747572380) self));
}


STR ARRAYT1718290516_unbox(ARRAYTUPdOBSTR self) {
 
 return ARRAYT1718290516(((ARRAYTUPdOBSTR) self));
}


STR ARRAYT835974574_unbox(ARRAYT1418209173 self) {
 
 return ARRAYT835974574(((ARRAYT1418209173) self));
}


STR ARRAYT1450640385_unbox(ARRAYT1153101058 self) {
 
 return ARRAYT1450640385(((ARRAYT1153101058) self));
}


STR ARRAYT1278969222_unbox(ARRAYT1562141251 self) {
 
 return ARRAYT1278969222(((ARRAYT1562141251) self));
}


STR ARRAYT856660275_unbox(ARRAYTUPdTPINT self) {
 
 return ARRAYT856660275(((ARRAYTUPdTPINT) self));
}


STR ARRAYT1037471825_unbox(ARRAYT1275404834 self) {
 
 return ARRAYT1037471825(((ARRAYT1275404834) self));
}


STR ARRAYT1926215796_unbox(ARRAYT1146239647 self) {
 
 return ARRAYT1926215796(((ARRAYT1146239647) self));
}


STR ARRAYT1803904833_unbox(ARRAYT2065045042 self) {
 
 return ARRAYT1803904833(((ARRAYT2065045042) self));
}


STR ARRAYT1810565556_unbox(ARRAYT1753333425 self) {
 
 return ARRAYT1810565556(((ARRAYT1753333425) self));
}


STR ARRAYT1671874055_unbox(ARRAYT1744744076 self) {
 
 return ARRAYT1671874055(((ARRAYT1744744076) self));
}


STR ARRAYT1626625951_unbox(ARRAYT546647214 self) {
 
 return ARRAYT1626625951(((ARRAYT546647214) self));
}


STR ARRAYT1054394674_unbox(ARRAYT1968212795 self) {
 
 return ARRAYT1054394674(((ARRAYT1968212795) self));
}


STR ARRAYT1238370919_unbox(ARRAYTUPIDENTdTP self) {
 
 return ARRAYT1238370919(((ARRAYTUPIDENTdTP) self));
}


STR ARRAYT568533288_unbox(ARRAYT263953675 self) {
 
 return ARRAYT568533288(((ARRAYT263953675) self));
}


STR ARRAYT1355045210_unbox(ARRAYTUPIDENTINT self) {
 
 return ARRAYT1355045210(((ARRAYTUPIDENTINT) self));
}


STR ARRAYT1384853867_unbox(ARRAYT348931282 self) {
 
 return ARRAYT1384853867(((ARRAYT348931282) self));
}


STR ARRAYT585031862_unbox(ARRAYT537394765 self) {
 
 return ARRAYT585031862(((ARRAYT537394765) self));
}


STR ARRAYT103792371_unbox(ARRAYT1759168384 self) {
 
 return ARRAYT103792371(((ARRAYT1759168384) self));
}


STR ARRAYT1942593259_unbox(ARRAYT1754348798 self) {
 
 return ARRAYT1942593259(((ARRAYT1754348798) self));
}


STR ARRAYT854063824_unbox(ARRAYTUPSIGINT self) {
 
 return ARRAYT854063824(((ARRAYTUPSIGINT) self));
}


STR ARRAYT31468026_unbox(ARRAYTUPSIGSIG self) {
 
 return ARRAYT31468026(((ARRAYTUPSIGSIG) self));
}


STR ARRAYT189456178_unbox(ARRAYTUPSIGSTR self) {
 
 return ARRAYT189456178(((ARRAYTUPSIGSTR) self));
}


STR ARRAYT1574849761_unbox(ARRAYT61112132 self) {
 
 return ARRAYT1574849761(((ARRAYT61112132) self));
}


STR ARRAYT2122676784_unbox(ARRAYT378612147 self) {
 
 return ARRAYT2122676784(((ARRAYT378612147) self));
}


STR ARRAYT1658404076_unbox(ARRAYTUPSTRINT self) {
 
 return ARRAYT1658404076(((ARRAYTUPSTRINT) self));
}


STR ARRAYT835808278_unbox(ARRAYTUPSTRSIG self) {
 
 return ARRAYT835808278(((ARRAYTUPSTRSIG) self));
}


STR ARRAYT614884074_unbox(ARRAYTUPSTRSTR self) {
 
 return ARRAYT614884074(((ARRAYTUPSTRSTR) self));
}


STR ARRAYT1561971079_unbox(ARRAYT1127505716 self) {
 
 return ARRAYT1561971079(((ARRAYT1127505716) self));
}


STR ARRAYT160571351_unbox(ARRAYT1641775270 self) {
 
 return ARRAYT160571351(((ARRAYT1641775270) self));
}


STR A_STAC1193125327_unbox(A_STAC1244966098 self) {
 
 return A_STAC1193125327(((A_STAC1244966098) self));
}


STR CONFIG155608225_unbox(CONFIG_TBL self) {
 
 return CONFIG155608225(((CONFIG_TBL) self));
}


STR ELT_TBL_strrSTR_unbox(ELT_TBL self) {
 
 return ELT_TBL_strrSTR(((ELT_TBL) self));
}


STR FLISTd1757835884_unbox(FLISTdAM_CONST self) {
 
 return FLISTd1757835884(((FLISTdAM_CONST) self));
}


STR FLISTd2007506601_unbox(FLISTdAM_EXPR self) {
 
 return FLISTd2007506601(((FLISTdAM_EXPR) self));
}


STR FLISTd1101907210_unbox(FLISTdAM_STMT self) {
 
 return FLISTd1101907210(((FLISTdAM_STMT) self));
}


STR FLISTd1314612993_unbox(FLISTdLAYOUT self) {
 
 return FLISTd1314612993(((FLISTdLAYOUT) self));
}


STR FLISTdTP_strrSTR_unbox(FLISTdTP self) {
 
 return FLISTdTP_strrSTR(((FLISTdTP) self));
}


STR FLISTA1264584154_unbox(FLISTA162798263 self) {
 
 return FLISTA1264584154(((FLISTA162798263) self));
}


STR FLISTA902886330_unbox(FLISTA1893461511 self) {
 
 return FLISTA902886330(((FLISTA1893461511) self));
}


STR FLISTA389202533_unbox(FLISTA1409846210 self) {
 
 return FLISTA389202533(((FLISTA1409846210) self));
}


STR FLISTA1774499089_unbox(FLISTAM_CALL_ARG self) {
 
 return FLISTA1774499089(((FLISTAM_CALL_ARG) self));
}


STR FLISTA1598488884_unbox(FLISTA886630705 self) {
 
 return FLISTA1598488884(((FLISTA886630705) self));
}


STR FLISTA2143887329_unbox(FLISTAM_CURSOR self) {
 
 return FLISTA2143887329(((FLISTAM_CURSOR) self));
}


STR FLISTA1366495389_unbox(FLISTA2097324934 self) {
 
 return FLISTA1366495389(((FLISTA2097324934) self));
}


STR FLISTA1884067430_unbox(FLISTA1062334999 self) {
 
 return FLISTA1884067430(((FLISTA1062334999) self));
}


STR FLISTA2047982482_unbox(FLISTA725283029 self) {
 
 return FLISTA2047982482(((FLISTA725283029) self));
}


STR FLISTA674592310_unbox(FLISTAM_ROUT_DEF self) {
 
 return FLISTA674592310(((FLISTAM_ROUT_DEF) self));
}


STR FLISTA1208864070_unbox(FLISTA112326087 self) {
 
 return FLISTA1208864070(((FLISTA112326087) self));
}


STR FLISTARG_strrSTR_unbox(FLISTARG self) {
 
 return FLISTARG_strrSTR(((FLISTARG) self));
}


STR FLISTA1228646807_unbox(FLISTA1327600838 self) {
 
 return FLISTA1228646807(((FLISTA1327600838) self));
}


STR FLISTA574012977_unbox(FLISTARRAYBOOL self) {
 
 return FLISTA574012977(((FLISTARRAYBOOL) self));
}


STR FLISTA1345532203_unbox(FLISTARRAYSTR self) {
 
 return FLISTA1345532203(((FLISTARRAYSTR) self));
}


STR FLISTA128466217_unbox(FLISTAS_FEAT_MOD self) {
 
 return FLISTA128466217(((FLISTAS_FEAT_MOD) self));
}


STR FLISTA1623803758_unbox(FLISTATTRSORT self) {
 
 return FLISTA1623803758(((FLISTATTRSORT) self));
}


STR FLISTB1590162857_unbox(FLISTBOOL self) {
 
 return FLISTB1590162857(((FLISTBOOL) self));
}


STR FLISTB2138238497_unbox(FLISTB475591118 self) {
 
 return FLISTB2138238497(((FLISTB475591118) self));
}


STR FLISTB2097638834_unbox(FLISTB2062080075 self) {
 
 return FLISTB2097638834(((FLISTB2062080075) self));
}


STR FLISTB275387800_unbox(FLISTB16124645 self) {
 
 return FLISTB275387800(((FLISTB16124645) self));
}


STR FLISTC31611564_unbox(FLISTCALL_ARG self) {
 
 return FLISTC31611564(((FLISTCALL_ARG) self));
}


STR FLISTC780849889_unbox(FLISTCHAR self) {
 
 return FLISTC780849889(((FLISTCHAR) self));
}


STR FLISTC1168586346_unbox(FLISTC1031309271 self) {
 
 return FLISTC1168586346(((FLISTC1031309271) self));
}


STR FLISTC1817360302_unbox(FLISTCODE_FILE self) {
 
 return FLISTC1817360302(((FLISTCODE_FILE) self));
}


STR FLISTC1921340409_unbox(FLISTC1772242452 self) {
 
 return FLISTC1921340409(((FLISTC1772242452) self));
}


STR FLISTE1365889449_unbox(FLISTE1761212196 self) {
 
 return FLISTE1365889449(((FLISTE1761212196) self));
}


STR FLISTE283073321_unbox(FLISTE1627119380 self) {
 
 return FLISTE283073321(((FLISTE1627119380) self));
}


STR FLISTF93053878_unbox(FLISTF7594819 self) {
 
 return FLISTF93053878(((FLISTF7594819) self));
}


STR FLISTF1366971496_unbox(FLISTFLISTSTR self) {
 
 return FLISTF1366971496(((FLISTFLISTSTR) self));
}


STR FLISTF786174088_unbox(FLISTFSTR self) {
 
 return FLISTF786174088(((FLISTFSTR) self));
}


STR FLISTI700982870_unbox(FLISTIDENT self) {
 
 return FLISTI700982870(((FLISTIDENT) self));
}


STR FLISTIFC_strrSTR_unbox(FLISTIFC self) {
 
 return FLISTIFC_strrSTR(((FLISTIFC) self));
}


STR FLISTI1877263384_unbox(FLISTI2086541899 self) {
 
 return FLISTI1877263384(((FLISTI2086541899) self));
}


STR FLISTI2007266978_unbox(FLISTI1895665739 self) {
 
 return FLISTI2007266978(((FLISTI1895665739) self));
}


STR FLISTINT_strrSTR_unbox(FLISTINT self) {
 
 return FLISTINT_strrSTR(((FLISTINT) self));
}


STR FLISTO1373905061_unbox(FLISTO1593816750 self) {
 
 return FLISTO1373905061(((FLISTO1593816750) self));
}


STR FLISTS1154465862_unbox(FLISTSFILE_ID self) {
 
 return FLISTS1154465862(((FLISTSFILE_ID) self));
}


STR FLISTS1349623250_unbox(FLISTSIDE_EFFECT self) {
 
 return FLISTS1349623250(((FLISTSIDE_EFFECT) self));
}


STR FLISTSIG_strrSTR_unbox(FLISTSIG self) {
 
 return FLISTSIG_strrSTR(((FLISTSIG) self));
}


STR FLISTSTR_strrSTR_unbox(FLISTSTR self) {
 
 return FLISTSTR_strrSTR(((FLISTSTR) self));
}


STR FLISTT1750053916_unbox(FLISTTP_CLASS self) {
 
 return FLISTT1750053916(((FLISTTP_CLASS) self));
}


STR FLISTT1896886665_unbox(FLISTTP_LAYOUT self) {
 
 return FLISTT1896886665(((FLISTTP_LAYOUT) self));
}


STR FLISTT306797234_unbox(FLISTT1513788273 self) {
 
 return FLISTT306797234(((FLISTT1513788273) self));
}


STR FLISTT1410881023_unbox(FLISTTUPdOBSTR self) {
 
 return FLISTT1410881023(((FLISTTUPdOBSTR) self));
}


STR FLISTT1916592887_unbox(FLISTT567859354 self) {
 
 return FLISTT1916592887(((FLISTT567859354) self));
}


STR FLISTT88767660_unbox(FLISTT880505585 self) {
 
 return FLISTT88767660(((FLISTT880505585) self));
}


STR FLISTT158032289_unbox(FLISTT1858637310 self) {
 
 return FLISTT158032289(((FLISTT1858637310) self));
}


STR FLISTT549250782_unbox(FLISTTUPdTPINT self) {
 
 return FLISTT549250782(((FLISTTUPdTPINT) self));
}


STR FLISTT2098083178_unbox(FLISTT1311334615 self) {
 
 return FLISTT2098083178(((FLISTT1311334615) self));
}


STR FLISTT709853181_unbox(FLISTT1661203254 self) {
 
 return FLISTT709853181(((FLISTT1661203254) self));
}


STR FLISTT1518857242_unbox(FLISTT1145236345 self) {
 
 return FLISTT1518857242(((FLISTT1145236345) self));
}


STR FLISTT689628623_unbox(FLISTT2049829484 self) {
 
 return FLISTT689628623(((FLISTT2049829484) self));
}


STR FLISTT1848908808_unbox(FLISTT406286251 self) {
 
 return FLISTT1848908808(((FLISTT406286251) self));
}


STR FLISTT1451978730_unbox(FLISTT31683607 self) {
 
 return FLISTT1451978730(((FLISTT31683607) self));
}


STR FLISTT1532344775_unbox(FLISTT1683165204 self) {
 
 return FLISTT1532344775(((FLISTT1683165204) self));
}


STR FLISTT544048196_unbox(FLISTTUPIDENTdTP self) {
 
 return FLISTT544048196(((FLISTTUPIDENTdTP) self));
}


STR FLISTT1906991113_unbox(FLISTT1023127882 self) {
 
 return FLISTT1906991113(((FLISTT1023127882) self));
}


STR FLISTT660722487_unbox(FLISTTUPIDENTINT self) {
 
 return FLISTT660722487(((FLISTTUPIDENTINT) self));
}


STR FLISTT1312754552_unbox(FLISTT1075494997 self) {
 
 return FLISTT1312754552(((FLISTT1075494997) self));
}


STR FLISTT1090491199_unbox(FLISTT844804258 self) {
 
 return FLISTT1090491199(((FLISTT844804258) self));
}


STR FLISTT919810588_unbox(FLISTT1742988937 self) {
 
 return FLISTT919810588(((FLISTT1742988937) self));
}


STR FLISTT582206950_unbox(FLISTT994805427 self) {
 
 return FLISTT582206950(((FLISTT994805427) self));
}


STR FLISTT546654331_unbox(FLISTTUPSIGINT self) {
 
 return FLISTT546654331(((FLISTTUPSIGINT) self));
}


STR FLISTT275941467_unbox(FLISTTUPSIGSIG self) {
 
 return FLISTT275941467(((FLISTTUPSIGSIG) self));
}


STR FLISTT496865671_unbox(FLISTTUPSIGSTR self) {
 
 return FLISTT496865671(((FLISTTUPSIGSTR) self));
}


STR FLISTT32449926_unbox(FLISTT789237687 self) {
 
 return FLISTT32449926(((FLISTT789237687) self));
}


STR FLISTT580276949_unbox(FLISTT471737672 self) {
 
 return FLISTT580276949(((FLISTT471737672) self));
}


STR FLISTT1350994583_unbox(FLISTTUPSTRINT self) {
 
 return FLISTT1350994583(((FLISTTUPSTRINT) self));
}


STR FLISTT528398785_unbox(FLISTTUPSTRSIG self) {
 
 return FLISTT528398785(((FLISTTUPSTRSIG) self));
}


STR FLISTT307474581_unbox(FLISTTUPSTRSTR self) {
 
 return FLISTT307474581(((FLISTTUPSTRSTR) self));
}


STR FLISTT208196008_unbox(FLISTT418307355 self) {
 
 return FLISTT208196008(((FLISTT418307355) self));
}


STR FLISTT499676452_unbox(FLISTT102367225 self) {
 
 return FLISTT499676452(((FLISTT102367225) self));
}


STR FMAPdO570867591_unbox(FMAPdOBNAMESPACE self) {
 
 return FMAPdO570867591(((FMAPdOBNAMESPACE) self));
}


STR FMAPdT1371742936_unbox(FMAPdTPdLAYOUT self) {
 
 return FMAPdT1371742936(((FMAPdTPdLAYOUT) self));
}


STR FMAPdT966432485_unbox(FMAPdTPCODE_FILE self) {
 
 return FMAPdT966432485(((FMAPdTPCODE_FILE) self));
}


STR FMAPdT2142836224_unbox(FMAPdT953816637 self) {
 
 return FMAPdT2142836224(((FMAPdT953816637) self));
}


STR FMAPdT1912217709_unbox(FMAPdTPINT self) {
 
 return FMAPdT1912217709(((FMAPdTPINT) self));
}


STR FMAPAM1084073011_unbox(FMAPAM339652544 self) {
 
 return FMAPAM1084073011(((FMAPAM339652544) self));
}


STR FMAPAM1201551978_unbox(FMAPAM1357596931 self) {
 
 return FMAPAM1201551978(((FMAPAM1357596931) self));
}


STR FMAPAM1620534738_unbox(FMAPAM1145008811 self) {
 
 return FMAPAM1620534738(((FMAPAM1145008811) self));
}


STR FMAPAM905477833_unbox(FMAPAM418009930 self) {
 
 return FMAPAM905477833(((FMAPAM418009930) self));
}


STR FMAPAM459426429_unbox(FMAPAM1244483504 self) {
 
 return FMAPAM459426429(((FMAPAM1244483504) self));
}


STR FMAPAM118642384_unbox(FMAPAM1787628573 self) {
 
 return FMAPAM118642384(((FMAPAM1787628573) self));
}


STR FMAPID1860739187_unbox(FMAPIDENTdTP self) {
 
 return FMAPID1860739187(((FMAPIDENTdTP) self));
}


STR FMAPID1895267434_unbox(FMAPID739086519 self) {
 
 return FMAPID1895267434(((FMAPID739086519) self));
}


STR FMAPSI1996405359_unbox(FMAPSI2103621588 self) {
 
 return FMAPSI1996405359(((FMAPSI2103621588) self));
}


STR FMAPSI99914640_unbox(FMAPSI518162669 self) {
 
 return FMAPSI99914640(((FMAPSI518162669) self));
}


STR FMAPSI1424048021_unbox(FMAPSI1349988702 self) {
 
 return FMAPSI1424048021(((FMAPSI1349988702) self));
}


STR FMAPSI2071674985_unbox(FMAPSI1121373188 self) {
 
 return FMAPSI2071674985(((FMAPSI1121373188) self));
}


STR FMAPSI866101256_unbox(FMAPSIGSTR self) {
 
 return FMAPSI866101256(((FMAPSIGSTR) self));
}


STR FMAPST512400025_unbox(FMAPSTRdCONFIG self) {
 
 return FMAPST512400025(((FMAPSTRdCONFIG) self));
}


STR FMAPST35426998_unbox(FMAPSTRFSETSTR self) {
 
 return FMAPST35426998(((FMAPSTRFSETSTR) self));
}


STR FMAPST1581005786_unbox(FMAPSTRINT self) {
 
 return FMAPST1581005786(((FMAPSTRINT) self));
}


STR FMAPST1891365712_unbox(FMAPSTRSIG self) {
 
 return FMAPST1891365712(((FMAPSTRSIG) self));
}


STR FMAPST1670441508_unbox(FMAPSTRSTR self) {
 
 return FMAPST1670441508(((FMAPSTRSTR) self));
}


STR FMAPTP1432889353_unbox(FMAPTP291739594 self) {
 
 return FMAPTP1432889353(((FMAPTP291739594) self));
}


STR FMAPTP722426429_unbox(FMAPTP775297600 self) {
 
 return FMAPTP722426429(((FMAPTP775297600) self));
}


STR FSTR_strrSTR_unbox(FSTR self) {
 
 return FSTR_strrSTR(((FSTR) self));
}


STR IFC_strrSTR_unbox(IFC self) {
 
 return IFC_strrSTR(((IFC) self));
}


STR INTI_strrSTR_unbox(INTI1 self) {
 
 return INTI_strrSTR(((INTI1) self));
}


STR NULL_S1933820575_unbox(NULL_STAT self) {
 
 return NULL_S1933820575(((NULL_STAT) self));
}


STR SIG_strrSTR_unbox(SIG self) {
 
 return SIG_strrSTR(((SIG) self));
}


STR SIG_TBL_strrSTR_unbox(SIG_TBL self) {
 
 return SIG_TBL_strrSTR(((SIG_TBL) self));
}


STR STAT_strrSTR_unbox(STAT1 self) {
 
 return STAT_strrSTR(((STAT1) self));
}


STR STR_strrSTR_unbox(STR self) {
 
 return STR_strrSTR(((STR) self));
}


void CS_OPT994959939_unbox(CS_OPTIONS self) {
 
 CS_OPT994959939(((CS_OPTIONS) self));
}


INT AM_ARR1424607794_unbox(AM_ARR_CONST self) {
 
 return AM_ARR1424607794(((AM_ARR_CONST) self));
}


INT AM_BOO561475672_unbox(AM_BOOL_CONST self) {
 
 return AM_BOO561475672(((AM_BOOL_CONST) self));
}


INT AM_CHA1966863808_unbox(AM_CHAR_CONST self) {
 
 return AM_CHA1966863808(((AM_CHAR_CONST) self));
}


INT AM_CON2051977914_unbox(AM_CONST self) {
 
 return AM_CON2051977914(((AM_CONST) self));
}


INT AM_FLT669520374_unbox(AM_FLTDX_CONST self) {
 
 return AM_FLT669520374(((AM_FLTDX_CONST) self));
}


INT AM_FLT556661152_unbox(AM_FLTD_CONST self) {
 
 return AM_FLT556661152(((AM_FLTD_CONST) self));
}


INT AM_FLT153804005_unbox(AM_FLTI_CONST self) {
 
 return AM_FLT153804005(((AM_FLTI_CONST) self));
}


INT AM_FLT1054767436_unbox(AM_FLTX_CONST self) {
 
 return AM_FLT1054767436(((AM_FLTX_CONST) self));
}


INT AM_FLT465909163_unbox(AM_FLT_CONST self) {
 
 return AM_FLT465909163(((AM_FLT_CONST) self));
}


INT AM_INT331860696_unbox(AM_INTI_CONST self) {
 
 return AM_INT331860696(((AM_INTI_CONST) self));
}


INT AM_INT2057636940_unbox(AM_INT_CONST self) {
 
 return AM_INT2057636940(((AM_INT_CONST) self));
}


INT AM_STR1959493974_unbox(AM_STR_CONST self) {
 
 return AM_STR1959493974(((AM_STR_CONST) self));
}


INT AM_VOI822416442_unbox(AM_VOID_CONST self) {
 
 return AM_VOI822416442(((AM_VOID_CONST) self));
}


INT TP_CLA619166775_unbox(TP_CLASS self) {
 
 return TP_CLA619166775(((TP_CLASS) self));
}


INT TP_ITER_hashrINT_unbox(TP_ITER self) {
 
 return TP_ITER_hashrINT(((TP_ITER) self));
}


INT TP_ROUT_hashrINT_unbox(TP_ROUT self) {
 
 return TP_ROUT_hashrINT(((TP_ROUT) self));
}


INT AM_ANY1965823461_unbox(AM_ANY_EXPR self) {
 
 return AM_ANY1965823461(((AM_ANY_EXPR) self));
}


INT AM_ARR191206921_unbox(AM_ARRAY_EXPR self) {
 
 return AM_ARR191206921(((AM_ARRAY_EXPR) self));
}


INT AM_ATT178000012_unbox(AM_ATTR_EXPR self) {
 
 return AM_ATT178000012(((AM_ATTR_EXPR) self));
}


INT AM_AT_373259037_unbox(AM_AT_EXPR self) {
 
 return AM_AT_373259037(((AM_AT_EXPR) self));
}


INT AM_BND472678797_unbox(AM_BND1124877163 self) {
 
 return AM_BND472678797(((AM_BND1124877163) self));
}


INT AM_BND2097777224_unbox(AM_BND367211769 self) {
 
 return AM_BND2097777224(((AM_BND367211769) self));
}


INT AM_BND1144165392_unbox(AM_BND260301329 self) {
 
 return AM_BND1144165392(((AM_BND260301329) self));
}


INT AM_CLU978087499_unbox(AM_CLUSTER_EXPR self) {
 
 return AM_CLU978087499(((AM_CLUSTER_EXPR) self));
}


INT AM_CLU772455521_unbox(AM_CLU1286269335 self) {
 
 return AM_CLU772455521(((AM_CLU1286269335) self));
}


INT AM_EXC800301049_unbox(AM_EXCEPT_EXPR self) {
 
 return AM_EXC800301049(((AM_EXCEPT_EXPR) self));
}


INT AM_EXT644325801_unbox(AM_EXT_CALL_EXPR self) {
 
 return AM_EXT644325801(((AM_EXT_CALL_EXPR) self));
}


INT AM_FAR2123150608_unbox(AM_FAR_EXPR self) {
 
 return AM_FAR2123150608(((AM_FAR_EXPR) self));
}


INT AM_GLO57248015_unbox(AM_GLOBAL_EXPR self) {
 
 return AM_GLO57248015(((AM_GLOBAL_EXPR) self));
}


INT AM_HER774694957_unbox(AM_HERE_EXPR self) {
 
 return AM_HER774694957(((AM_HERE_EXPR) self));
}


INT AM_IF_1573308145_unbox(AM_IF_EXPR self) {
 
 return AM_IF_1573308145(((AM_IF_EXPR) self));
}


INT AM_IS_128338200_unbox(AM_IS_VOID_EXPR self) {
 
 return AM_IS_128338200(((AM_IS_VOID_EXPR) self));
}


INT AM_ITE1021507011_unbox(AM_ITE1809135850 self) {
 
 return AM_ITE1021507011(((AM_ITE1809135850) self));
}


INT AM_LOC505223029_unbox(AM_LOCAL_EXPR self) {
 
 return AM_LOC505223029(((AM_LOCAL_EXPR) self));
}


INT AM_NEA1314295025_unbox(AM_NEAR_EXPR self) {
 
 return AM_NEA1314295025(((AM_NEAR_EXPR) self));
}


INT AM_NEW1014811437_unbox(AM_NEW_EXPR self) {
 
 return AM_NEW1014811437(((AM_NEW_EXPR) self));
}


INT AM_ROU67895179_unbox(AM_ROU1916046290 self) {
 
 return AM_ROU67895179(((AM_ROU1916046290) self));
}


INT AM_SHA1840537425_unbox(AM_SHARED_EXPR self) {
 
 return AM_SHA1840537425(((AM_SHARED_EXPR) self));
}


INT AM_STM804699229_unbox(AM_STMT_EXPR self) {
 
 return AM_STM804699229(((AM_STMT_EXPR) self));
}


INT AM_VAR2089448132_unbox(AM_VAR744470097 self) {
 
 return AM_VAR2089448132(((AM_VAR744470097) self));
}


INT AM_VAT495261450_unbox(AM_VAT319982528 self) {
 
 return AM_VAT495261450(((AM_VAT319982528) self));
}


INT AM_WHE1598364639_unbox(AM_WHERE_EXPR self) {
 
 return AM_WHE1598364639(((AM_WHERE_EXPR) self));
}


INT TUPTP_385216441_unbox(TUPTP_1668454172_boxed self) {

 TUPTP_1668454172 L58; 
L58 = ((TUPTP_1668454172_boxed) self)->immutable_part;
 return TUPTP_385216441(L58);
}


INT TUPTP_108406739_unbox(TUPTP_858321267_boxed self) {

 TUPTP_858321267 L59; 
L59 = ((TUPTP_858321267_boxed) self)->immutable_part;
 return TUPTP_108406739(L59);
}


INT TUPSTR910634748_unbox(TUPSTRSTR_boxed self) {

 TUPSTRSTR L60; 
L60 = ((TUPSTRSTR_boxed) self)->immutable_part;
 return TUPSTR910634748(L60);
}


INT TUPSTR813227328_unbox(TUPSTRSIG_boxed self) {

 TUPSTRSIG L61; 
L61 = ((TUPSTRSIG_boxed) self)->immutable_part;
 return TUPSTR813227328(L61);
}


INT TUPSTR737321694_unbox(TUPSTRINT_boxed self) {

 TUPSTRINT L62; 
L62 = ((TUPSTRINT_boxed) self)->immutable_part;
 return TUPSTR737321694(L62);
}


INT TUPSTR1611378322_unbox(TUPSTRFSETSTR_boxed self) {

 TUPSTRFSETSTR L63; 
L63 = ((TUPSTRFSETSTR_boxed) self)->immutable_part;
 return TUPSTR1611378322(L63);
}


INT TUPSTR864810129_unbox(TUPSTRdCONFIG_boxed self) {

 TUPSTRdCONFIG L64; 
L64 = ((TUPSTRdCONFIG_boxed) self)->immutable_part;
 return TUPSTR864810129(L64);
}


INT TUPSIG1486928152_unbox(TUPSIGSTR_boxed self) {

 TUPSIGSTR L65; 
L65 = ((TUPSIGSTR_boxed) self)->immutable_part;
 return TUPSIG1486928152(L65);
}


INT TUPSIG1584335572_unbox(TUPSIGSIG_boxed self) {

 TUPSIGSIG L66; 
L66 = ((TUPSIGSIG_boxed) self)->immutable_part;
 return TUPSIG1584335572(L66);
}


INT TUPSIG1160082702_unbox(TUPSIGINT_boxed self) {

 TUPSIGINT L67; 
L67 = ((TUPSIGINT_boxed) self)->immutable_part;
 return TUPSIG1160082702(L67);
}


INT TUPSIG2024347575_unbox(TUPSIG2023533247_boxed self) {

 TUPSIG2023533247 L68; 
L68 = ((TUPSIG2023533247_boxed) self)->immutable_part;
 return TUPSIG2024347575(L68);
}


INT TUPSIG1100158695_unbox(TUPSIGARG_LAYOUT_boxed self) {

 TUPSIGARG_LAYOUT L69; 
L69 = ((TUPSIGARG_LAYOUT_boxed) self)->immutable_part;
 return TUPSIG1100158695(L69);
}


INT TUPSIG1080413784_unbox(TUPSIG578692189_boxed self) {

 TUPSIG578692189 L70; 
L70 = ((TUPSIG578692189_boxed) self)->immutable_part;
 return TUPSIG1080413784(L70);
}


INT TUPSIG524761103_unbox(TUPSIG1754078736_boxed self) {

 TUPSIG1754078736 L71; 
L71 = ((TUPSIG1754078736_boxed) self)->immutable_part;
 return TUPSIG524761103(L71);
}


INT TUPINT283365700_unbox(TUPINTINT_boxed self) {

 TUPINTINT L72; 
L72 = ((TUPINTINT_boxed) self)->immutable_part;
 return TUPINT283365700(L72);
}


INT TUPIDE280438974_unbox(TUPIDENTINT_boxed self) {

 TUPIDENTINT L73; 
L73 = ((TUPIDENTINT_boxed) self)->immutable_part;
 return TUPIDE280438974(L73);
}


INT TUPIDE274165750_unbox(TUPIDENTARRAYdTP_boxed self) {

 TUPIDENTARRAYdTP L74; 
L74 = ((TUPIDENTARRAYdTP_boxed) self)->immutable_part;
 return TUPIDE274165750(L74);
}


INT TUPIDE88035988_unbox(TUPIDE641654124_boxed self) {

 TUPIDE641654124 L75; 
L75 = ((TUPIDE641654124_boxed) self)->immutable_part;
 return TUPIDE88035988(L75);
}


INT TUPIDE1936372555_unbox(TUPIDENTdTP_boxed self) {

 TUPIDENTdTP L76; 
L76 = ((TUPIDENTdTP_boxed) self)->immutable_part;
 return TUPIDE1936372555(L76);
}


INT TUPBOO1866184529_unbox(TUPBOO1392923241_boxed self) {

 TUPBOO1392923241 L77; 
L77 = ((TUPBOO1392923241_boxed) self)->immutable_part;
 return TUPBOO1866184529(L77);
}


INT TUPARR826863757_unbox(TUPARR1759987776_boxed self) {

 TUPARR1759987776 L78; 
L78 = ((TUPARR1759987776_boxed) self)->immutable_part;
 return TUPARR826863757(L78);
}


INT TUPARR1458356743_unbox(TUPARRAYARGdTP_boxed self) {

 TUPARRAYARGdTP L79; 
L79 = ((TUPARRAYARGdTP_boxed) self)->immutable_part;
 return TUPARR1458356743(L79);
}


INT TUPAM_519070420_unbox(TUPAM_653469574_boxed self) {

 TUPAM_653469574 L80; 
L80 = ((TUPAM_653469574_boxed) self)->immutable_part;
 return TUPAM_519070420(L80);
}


INT TUPAM_755257033_unbox(TUPAM_1024994801_boxed self) {

 TUPAM_1024994801 L81; 
L81 = ((TUPAM_1024994801_boxed) self)->immutable_part;
 return TUPAM_755257033(L81);
}


INT TUPAM_1395325321_unbox(TUPAM_504653531_boxed self) {

 TUPAM_504653531 L82; 
L82 = ((TUPAM_504653531_boxed) self)->immutable_part;
 return TUPAM_1395325321(L82);
}


INT TUPAM_247029134_unbox(TUPAM_1519040877_boxed self) {

 TUPAM_1519040877 L83; 
L83 = ((TUPAM_1519040877_boxed) self)->immutable_part;
 return TUPAM_247029134(L83);
}


INT TUPAM_1208907315_unbox(TUPAM_927437481_boxed self) {

 TUPAM_927437481 L84; 
L84 = ((TUPAM_927437481_boxed) self)->immutable_part;
 return TUPAM_1208907315(L84);
}


INT TUPAM_460226510_unbox(TUPAM_667892060_boxed self) {

 TUPAM_667892060 L85; 
L85 = ((TUPAM_667892060_boxed) self)->immutable_part;
 return TUPAM_460226510(L85);
}


INT TUPAM_1877096499_unbox(TUPAM_304458649_boxed self) {

 TUPAM_304458649 L86; 
L86 = ((TUPAM_304458649_boxed) self)->immutable_part;
 return TUPAM_1877096499(L86);
}


INT TUPdTP1209415271_unbox(TUPdTPINT_boxed self) {

 TUPdTPINT L87; 
L87 = ((TUPdTPINT_boxed) self)->immutable_part;
 return TUPdTP1209415271(L87);
}


INT TUPdTP1263366620_unbox(TUPdTP1710233051_boxed self) {

 TUPdTP1710233051 L88; 
L88 = ((TUPdTP1710233051_boxed) self)->immutable_part;
 return TUPdTP1263366620(L88);
}


INT TUPdTP386219079_unbox(TUPdTPCODE_FILE_boxed self) {

 TUPdTPCODE_FILE L89; 
L89 = ((TUPdTPCODE_FILE_boxed) self)->immutable_part;
 return TUPdTP386219079(L89);
}


INT TUPdTP1991179276_unbox(TUPdTPdLAYOUT_boxed self) {

 TUPdTPdLAYOUT L90; 
L90 = ((TUPdTPdLAYOUT_boxed) self)->immutable_part;
 return TUPdTP1991179276(L90);
}


INT TUPdOB400520666_unbox(TUPdOBSTR_boxed self) {

 TUPdOBSTR L91; 
L91 = ((TUPdOBSTR_boxed) self)->immutable_part;
 return TUPdOB400520666(L91);
}


INT TUPdOB687982527_unbox(TUPdOBNAMESPACE_boxed self) {

 TUPdOBNAMESPACE L92; 
L92 = ((TUPdOBNAMESPACE_boxed) self)->immutable_part;
 return TUPdOB687982527(L92);
}


INT TUPdAS472200087_unbox(TUPdAS268679779_boxed self) {

 TUPdAS268679779 L93; 
L93 = ((TUPdAS268679779_boxed) self)->immutable_part;
 return TUPdAS472200087(L93);
}


INT TUPdAM1873776308_unbox(TUPdAM_EXPRdTP_boxed self) {

 TUPdAM_EXPRdTP L94; 
L94 = ((TUPdAM_EXPRdTP_boxed) self)->immutable_part;
 return TUPdAM1873776308(L94);
}


INT SFILE_29532291_unbox(SFILE_ID_boxed self) {

 SFILE_ID L95; 
L95 = ((SFILE_ID_boxed) self)->immutable_part;
 return SFILE_29532291(L95);
}


INT INT_hashrINT_unbox(INT_boxed self) {

 INT L96; 
L96 = ((INT_boxed) self)->immutable_part;
 return INT_hashrINT(L96);
}


INT IDENT_hashrINT_unbox(IDENT_boxed self) {

 IDENT L97; 
L97 = ((IDENT_boxed) self)->immutable_part;
 return IDENT_hashrINT(L97);
}


INT FLTD_hashrINT_unbox(FLTD_boxed self) {

 FLTD L98; 
L98 = ((FLTD_boxed) self)->immutable_part;
 return FLTD_hashrINT(L98);
}


INT FLT_hashrINT_unbox(FLT_boxed self) {

 FLT L99; 
L99 = ((FLT_boxed) self)->immutable_part;
 return FLT_hashrINT(L99);
}


INT CHAR_hashrINT_unbox(CHAR_boxed self) {

 CHAR L100; 
L100 = ((CHAR_boxed) self)->immutable_part;
 return CHAR_hashrINT(L100);
}


INT CSE_DO1342102696_unbox(CSE_DOUBLE_EXPRS self) {
 
 return CSE_DO1342102696(((CSE_DOUBLE_EXPRS) self));
}


INT FSTR_hashrINT_unbox(FSTR self) {
 
 return FSTR_hashrINT(((FSTR) self));
}


INT INTI_hashrINT_unbox(INTI1 self) {
 
 return INTI_hashrINT(((INTI1) self));
}


INT SIDE_E484651583_unbox(SIDE_EFFECT self) {
 
 return SIDE_E484651583(((SIDE_EFFECT) self));
}


INT SIG_hashrINT_unbox(SIG self) {
 
 return SIG_hashrINT(((SIG) self));
}


INT STR_hashrINT_unbox(STR self) {
 
 return STR_hashrINT(((STR) self));
}


SFILE_ID AM_ANY312639639_unbox(AM_ANY_EXPR self) {
 
 return AM_ANY312639639(((AM_ANY_EXPR) self));
}


SFILE_ID AM_ARR592984289_unbox(AM_ARRAY_EXPR self) {
 
 return AM_ARR592984289(((AM_ARRAY_EXPR) self));
}


SFILE_ID AM_ARR490952057_unbox(AM_ARR_CONST self) {
 
 return AM_ARR490952057(((AM_ARR_CONST) self));
}


SFILE_ID AM_ATT1483547331_unbox(AM_ATTR_EXPR self) {
 
 return AM_ATT1483547331(((AM_ATTR_EXPR) self));
}


SFILE_ID AM_AT_336435086_unbox(AM_AT_EXPR self) {
 
 return AM_AT_336435086(((AM_AT_EXPR) self));
}


SFILE_ID AM_BND1800984858_unbox(AM_BND1124877163 self) {
 
 return AM_BND1800984858(((AM_BND1124877163) self));
}


SFILE_ID AM_BND1697524322_unbox(AM_BND367211769 self) {
 
 return AM_BND1697524322(((AM_BND367211769) self));
}


SFILE_ID AM_BND1008256842_unbox(AM_BND260301329 self) {
 
 return AM_BND1008256842(((AM_BND260301329) self));
}


SFILE_ID AM_BOO1043294506_unbox(AM_BOOL_CONST self) {
 
 return AM_BOO1043294506(((AM_BOOL_CONST) self));
}


SFILE_ID AM_CHA1773602482_unbox(AM_CHAR_CONST self) {
 
 return AM_CHA1773602482(((AM_CHAR_CONST) self));
}


SFILE_ID AM_CLU1154173259_unbox(AM_CLUSTER_EXPR self) {
 
 return AM_CLU1154173259(((AM_CLUSTER_EXPR) self));
}


SFILE_ID AM_CLU2049939620_unbox(AM_CLU1286269335 self) {
 
 return AM_CLU2049939620(((AM_CLU1286269335) self));
}


SFILE_ID AM_CON810758495_unbox(AM_CONST self) {
 
 return AM_CON810758495(((AM_CONST) self));
}


SFILE_ID AM_EXC2040160518_unbox(AM_EXCEPT_EXPR self) {
 
 return AM_EXC2040160518(((AM_EXCEPT_EXPR) self));
}


SFILE_ID AM_EXT840500856_unbox(AM_EXT_CALL_EXPR self) {
 
 return AM_EXT840500856(((AM_EXT_CALL_EXPR) self));
}


SFILE_ID AM_FAR526851136_unbox(AM_FAR_EXPR self) {
 
 return AM_FAR526851136(((AM_FAR_EXPR) self));
}


SFILE_ID AM_FLT2070042319_unbox(AM_FLTDX_CONST self) {
 
 return AM_FLT2070042319(((AM_FLTDX_CONST) self));
}


SFILE_ID AM_FLT1412381806_unbox(AM_FLTD_CONST self) {
 
 return AM_FLT1412381806(((AM_FLTD_CONST) self));
}


SFILE_ID AM_FLT564073323_unbox(AM_FLTI_CONST self) {
 
 return AM_FLT564073323(((AM_FLTI_CONST) self));
}


SFILE_ID AM_FLT2096495882_unbox(AM_FLTX_CONST self) {
 
 return AM_FLT2096495882(((AM_FLTX_CONST) self));
}


SFILE_ID AM_FLT1298836824_unbox(AM_FLT_CONST self) {
 
 return AM_FLT1298836824(((AM_FLT_CONST) self));
}


SFILE_ID AM_GLO537255272_unbox(AM_GLOBAL_EXPR self) {
 
 return AM_GLO537255272(((AM_GLOBAL_EXPR) self));
}


SFILE_ID AM_HER1201005342_unbox(AM_HERE_EXPR self) {
 
 return AM_HER1201005342(((AM_HERE_EXPR) self));
}


SFILE_ID AM_IF_1629993260_unbox(AM_IF_EXPR self) {
 
 return AM_IF_1629993260(((AM_IF_EXPR) self));
}


SFILE_ID AM_INT872738218_unbox(AM_INTI_CONST self) {
 
 return AM_INT872738218(((AM_INTI_CONST) self));
}


SFILE_ID AM_INT1397528579_unbox(AM_INT_CONST self) {
 
 return AM_INT1397528579(((AM_INT_CONST) self));
}


SFILE_ID AM_IS_82827228_unbox(AM_IS_VOID_EXPR self) {
 
 return AM_IS_82827228(((AM_IS_VOID_EXPR) self));
}


SFILE_ID AM_ITE834410629_unbox(AM_ITE1809135850 self) {
 
 return AM_ITE834410629(((AM_ITE1809135850) self));
}


SFILE_ID AM_LOC327374505_unbox(AM_LOCAL_EXPR self) {
 
 return AM_LOC327374505(((AM_LOCAL_EXPR) self));
}


SFILE_ID AM_NEA1717951158_unbox(AM_NEAR_EXPR self) {
 
 return AM_NEA1717951158(((AM_NEAR_EXPR) self));
}


SFILE_ID AM_NEW765217327_unbox(AM_NEW_EXPR self) {
 
 return AM_NEW765217327(((AM_NEW_EXPR) self));
}


SFILE_ID AM_ROU145143149_unbox(AM_ROU1916046290 self) {
 
 return AM_ROU145143149(((AM_ROU1916046290) self));
}


SFILE_ID AM_SHA644128690_unbox(AM_SHARED_EXPR self) {
 
 return AM_SHA644128690(((AM_SHARED_EXPR) self));
}


SFILE_ID AM_STM1466562094_unbox(AM_STMT_EXPR self) {
 
 return AM_STM1466562094(((AM_STMT_EXPR) self));
}


SFILE_ID AM_STR1580126161_unbox(AM_STR_CONST self) {
 
 return AM_STR1580126161(((AM_STR_CONST) self));
}


SFILE_ID AM_VAR1884071700_unbox(AM_VAR744470097 self) {
 
 return AM_VAR1884071700(((AM_VAR744470097) self));
}


SFILE_ID AM_VAT338387515_unbox(AM_VAT319982528 self) {
 
 return AM_VAT338387515(((AM_VAT319982528) self));
}


SFILE_ID AM_VOI405249940_unbox(AM_VOID_CONST self) {
 
 return AM_VOI405249940(((AM_VOID_CONST) self));
}


SFILE_ID AM_WHE345322829_unbox(AM_WHERE_EXPR self) {
 
 return AM_WHE345322829(((AM_WHERE_EXPR) self));
}


SFILE_ID AM_ASS142463220_unbox(AM_ASSERT_STMT self) {
 
 return AM_ASS142463220(((AM_ASSERT_STMT) self));
}


SFILE_ID AM_ASS1066541977_unbox(AM_ASSIGN_STMT self) {
 
 return AM_ASS1066541977(((AM_ASSIGN_STMT) self));
}


SFILE_ID AM_ATT2128304053_unbox(AM_ATTACH_STMT self) {
 
 return AM_ATT2128304053(((AM_ATTACH_STMT) self));
}


SFILE_ID AM_BRE475880308_unbox(AM_BREAK_STMT self) {
 
 return AM_BRE475880308(((AM_BREAK_STMT) self));
}


SFILE_ID AM_CAL1587798028_unbox(AM_CALL_ARG self) {
 
 return AM_CAL1587798028(((AM_CALL_ARG) self));
}


SFILE_ID AM_CAS881290947_unbox(AM_CASE_STMT self) {
 
 return AM_CAS881290947(((AM_CASE_STMT) self));
}


SFILE_ID AM_COM1046808385_unbox(AM_COMMENT_STMT self) {
 
 return AM_COM1046808385(((AM_COMMENT_STMT) self));
}


SFILE_ID AM_EXP2073687788_unbox(AM_EXPR_STMT self) {
 
 return AM_EXP2073687788(((AM_EXPR_STMT) self));
}


SFILE_ID AM_FOR1474728133_unbox(AM_FORK_STMT self) {
 
 return AM_FOR1474728133(((AM_FORK_STMT) self));
}


SFILE_ID AM_FOR97554330_unbox(AM_FORMAL_ARG self) {
 
 return AM_FOR97554330(((AM_FORMAL_ARG) self));
}


SFILE_ID AM_IF_1192492569_unbox(AM_IF_STMT self) {
 
 return AM_IF_1192492569(((AM_IF_STMT) self));
}


SFILE_ID AM_INI1792787320_unbox(AM_INITIAL_STMT self) {
 
 return AM_INI1792787320(((AM_INITIAL_STMT) self));
}


SFILE_ID AM_INV963567817_unbox(AM_INV1167837230 self) {
 
 return AM_INV963567817(((AM_INV1167837230) self));
}


SFILE_ID AM_LOC748262786_unbox(AM_LOCK_STMT self) {
 
 return AM_LOC748262786(((AM_LOCK_STMT) self));
}


SFILE_ID AM_LOO738236003_unbox(AM_LOOP_STMT self) {
 
 return AM_LOO738236003(((AM_LOOP_STMT) self));
}


SFILE_ID AM_OB_1633498909_unbox(AM_OB_DEF self) {
 
 return AM_OB_1633498909(((AM_OB_DEF) self));
}


SFILE_ID AM_PAR1239764599_unbox(AM_PAR_STMT self) {
 
 return AM_PAR1239764599(((AM_PAR_STMT) self));
}


SFILE_ID AM_POS2123304705_unbox(AM_POST_STMT self) {
 
 return AM_POS2123304705(((AM_POST_STMT) self));
}


SFILE_ID AM_PRE2094346420_unbox(AM_PREFETCH_STMT self) {
 
 return AM_PRE2094346420(((AM_PREFETCH_STMT) self));
}


SFILE_ID AM_PRE1797857661_unbox(AM_PRE_STMT self) {
 
 return AM_PRE1797857661(((AM_PRE_STMT) self));
}


SFILE_ID AM_PRO1670847403_unbox(AM_PROTECT_STMT self) {
 
 return AM_PRO1670847403(((AM_PROTECT_STMT) self));
}


SFILE_ID AM_RAI1393475571_unbox(AM_RAISE_STMT self) {
 
 return AM_RAI1393475571(((AM_RAISE_STMT) self));
}


SFILE_ID AM_RET702658942_unbox(AM_RETURN_STMT self) {
 
 return AM_RET702658942(((AM_RETURN_STMT) self));
}


SFILE_ID AM_ROU231894201_unbox(AM_ROUT_DEF self) {
 
 return AM_ROU231894201(((AM_ROUT_DEF) self));
}


SFILE_ID AM_SYN416733298_unbox(AM_SYNC_STMT self) {
 
 return AM_SYN416733298(((AM_SYNC_STMT) self));
}


SFILE_ID AM_TYP720211797_unbox(AM_TYPECASE_STMT self) {
 
 return AM_TYP720211797(((AM_TYPECASE_STMT) self));
}


SFILE_ID AM_UNL1937638738_unbox(AM_UNLOCK_STMT self) {
 
 return AM_UNL1937638738(((AM_UNLOCK_STMT) self));
}


SFILE_ID AM_WAI442200356_unbox(AM_WAITFOR_STMT self) {
 
 return AM_WAI442200356(((AM_WAITFOR_STMT) self));
}


SFILE_ID AM_WIT1120935944_unbox(AM_WIT1996971603 self) {
 
 return AM_WIT1120935944(((AM_WIT1996971603) self));
}


SFILE_ID AM_YIE1303084854_unbox(AM_YIELD_STMT self) {
 
 return AM_YIE1303084854(((AM_YIELD_STMT) self));
}


SFILE_ID AS_AND749298802_unbox(AS_AND_EXPR self) {
 
 return AS_AND749298802(((AS_AND_EXPR) self));
}


SFILE_ID AS_ANY2075595295_unbox(AS_ANY_EXPR self) {
 
 return AS_ANY2075595295(((AS_ANY_EXPR) self));
}


SFILE_ID AS_ARG265670232_unbox(AS_ARG_DEC self) {
 
 return AS_ARG265670232(((AS_ARG_DEC) self));
}


SFILE_ID AS_ARR542631033_unbox(AS_ARRAY_EXPR self) {
 
 return AS_ARR542631033(((AS_ARRAY_EXPR) self));
}


SFILE_ID AS_ASS40608582_unbox(AS_ASSERT_STMT self) {
 
 return AS_ASS40608582(((AS_ASSERT_STMT) self));
}


SFILE_ID AS_ASS964687339_unbox(AS_ASSIGN_STMT self) {
 
 return AS_ASS964687339(((AS_ASSIGN_STMT) self));
}


SFILE_ID AS_ATT2064808605_unbox(AS_ATTACH_STMT self) {
 
 return AS_ATT2064808605(((AS_ATTACH_STMT) self));
}


SFILE_ID AS_ATT1647669388_unbox(AS_ATTR_DEF self) {
 
 return AS_ATT1647669388(((AS_ATTR_DEF) self));
}


SFILE_ID AS_AT_1597668772_unbox(AS_AT_EXPR self) {
 
 return AS_AT_1597668772(((AS_AT_EXPR) self));
}


SFILE_ID AS_BOO291537060_unbox(AS_BOOL_LIT_EXPR self) {
 
 return AS_BOO291537060(((AS_BOOL_LIT_EXPR) self));
}


SFILE_ID AS_BOU1793757293_unbox(AS_BOU14269336 self) {
 
 return AS_BOU1793757293(((AS_BOU14269336) self));
}


SFILE_ID AS_BRE1173994939_unbox(AS_BREAK_EXPR self) {
 
 return AS_BRE1173994939(((AS_BREAK_EXPR) self));
}


SFILE_ID AS_CAL82770762_unbox(AS_CALL_EXPR self) {
 
 return AS_CAL82770762(((AS_CALL_EXPR) self));
}


SFILE_ID AS_CAS986885563_unbox(AS_CASE_STMT self) {
 
 return AS_CAS986885563(((AS_CASE_STMT) self));
}


SFILE_ID AS_CAS1215719733_unbox(AS_CASE_WHEN self) {
 
 return AS_CAS1215719733(((AS_CASE_WHEN) self));
}


SFILE_ID AS_CHA1542077308_unbox(AS_CHAR_LIT_EXPR self) {
 
 return AS_CHA1542077308(((AS_CHAR_LIT_EXPR) self));
}


SFILE_ID AS_CLA374248818_unbox(AS_CLASS_DEF self) {
 
 return AS_CLA374248818(((AS_CLASS_DEF) self));
}


SFILE_ID AS_CLU1205555915_unbox(AS_CLUSTER_EXPR self) {
 
 return AS_CLU1205555915(((AS_CLUSTER_EXPR) self));
}


SFILE_ID AS_CLU243953950_unbox(AS_CLU553570599 self) {
 
 return AS_CLU243953950(((AS_CLU553570599) self));
}


SFILE_ID AS_COH931029852_unbox(AS_COHORT_EXPR self) {
 
 return AS_COH931029852(((AS_COHORT_EXPR) self));
}


SFILE_ID AS_CON1302703181_unbox(AS_CONST_DEF self) {
 
 return AS_CON1302703181(((AS_CONST_DEF) self));
}


SFILE_ID AS_CRE1835137079_unbox(AS_CREATE_EXPR self) {
 
 return AS_CRE1835137079(((AS_CREATE_EXPR) self));
}


SFILE_ID AS_DEC1054605514_unbox(AS_DEC_STMT self) {
 
 return AS_DEC1054605514(((AS_DEC_STMT) self));
}


SFILE_ID AS_EXC2142015156_unbox(AS_EXCEPT_EXPR self) {
 
 return AS_EXC2142015156(((AS_EXCEPT_EXPR) self));
}


SFILE_ID AS_EXP353102998_unbox(AS_EXPR_STMT self) {
 
 return AS_EXP353102998(((AS_EXPR_STMT) self));
}


SFILE_ID AS_FAR1861383798_unbox(AS_FAR_EXPR self) {
 
 return AS_FAR1861383798(((AS_FAR_EXPR) self));
}


SFILE_ID AS_FEA630208390_unbox(AS_FEAT_MOD self) {
 
 return AS_FEA630208390(((AS_FEAT_MOD) self));
}


SFILE_ID AS_FLT235098865_unbox(AS_FLT_LIT_EXPR self) {
 
 return AS_FLT235098865(((AS_FLT_LIT_EXPR) self));
}


SFILE_ID AS_FOR393448377_unbox(AS_FORK_STMT self) {
 
 return AS_FOR393448377(((AS_FORK_STMT) self));
}


SFILE_ID AS_GLO435400634_unbox(AS_GLOBAL_EXPR self) {
 
 return AS_GLO435400634(((AS_GLOBAL_EXPR) self));
}


SFILE_ID AS_HER667171168_unbox(AS_HERE_EXPR self) {
 
 return AS_HER667171168(((AS_HERE_EXPR) self));
}


SFILE_ID AS_IDE48340397_unbox(AS_IDENT_LIST self) {
 
 return AS_IDE48340397(((AS_IDENT_LIST) self));
}


SFILE_ID AS_IF_1168370869_unbox(AS_IF_STMT self) {
 
 return AS_IF_1168370869(((AS_IF_STMT) self));
}


SFILE_ID AS_INC214099154_unbox(AS_INC1416917001 self) {
 
 return AS_INC214099154(((AS_INC1416917001) self));
}


SFILE_ID AS_INI295049889_unbox(AS_INITIAL_EXPR self) {
 
 return AS_INI295049889(((AS_INITIAL_EXPR) self));
}


SFILE_ID AS_INT1381353727_unbox(AS_INT2031239268 self) {
 
 return AS_INT1381353727(((AS_INT2031239268) self));
}


SFILE_ID AS_INT6023736_unbox(AS_INT_LIT_EXPR self) {
 
 return AS_INT6023736(((AS_INT_LIT_EXPR) self));
}


SFILE_ID AS_IS_2018065350_unbox(AS_IS_VOID_EXPR self) {
 
 return AS_IS_2018065350(((AS_IS_VOID_EXPR) self));
}


SFILE_ID AS_LOC1864862121_unbox(AS_LOCK_IF_WHEN self) {
 
 return AS_LOC1864862121(((AS_LOCK_IF_WHEN) self));
}


SFILE_ID AS_LOC1119913724_unbox(AS_LOCK_STMT self) {
 
 return AS_LOC1119913724(((AS_LOCK_STMT) self));
}


SFILE_ID AS_LOO1129940507_unbox(AS_LOOP_STMT self) {
 
 return AS_LOO1129940507(((AS_LOOP_STMT) self));
}


SFILE_ID AS_NEA708839628_unbox(AS_NEAR_EXPR self) {
 
 return AS_NEA708839628(((AS_NEAR_EXPR) self));
}


SFILE_ID AS_NEW1623017607_unbox(AS_NEW_EXPR self) {
 
 return AS_NEW1623017607(((AS_NEW_EXPR) self));
}


SFILE_ID AS_OR_1363025812_unbox(AS_OR_EXPR self) {
 
 return AS_OR_1363025812(((AS_OR_EXPR) self));
}


SFILE_ID AS_PAR158230952_unbox(AS_PARAM_DEC self) {
 
 return AS_PAR158230952(((AS_PARAM_DEC) self));
}


SFILE_ID AS_PAR666967763_unbox(AS_PAR_STMT self) {
 
 return AS_PAR666967763(((AS_PAR_STMT) self));
}


SFILE_ID AS_PRO264390719_unbox(AS_PROTECT_STMT self) {
 
 return AS_PRO264390719(((AS_PROTECT_STMT) self));
}


SFILE_ID AS_PRO493224889_unbox(AS_PROTECT_WHEN self) {
 
 return AS_PRO493224889(((AS_PROTECT_WHEN) self));
}


SFILE_ID AS_QUI1176752812_unbox(AS_QUIT_STMT self) {
 
 return AS_QUI1176752812(((AS_QUIT_STMT) self));
}


SFILE_ID AS_RAI1765876403_unbox(AS_RAISE_STMT self) {
 
 return AS_RAI1765876403(((AS_RAISE_STMT) self));
}


SFILE_ID AS_RES951018660_unbox(AS_RESULT_EXPR self) {
 
 return AS_RES951018660(((AS_RESULT_EXPR) self));
}


SFILE_ID AS_RET600804304_unbox(AS_RETURN_STMT self) {
 
 return AS_RET600804304(((AS_RETURN_STMT) self));
}


SFILE_ID AS_ROU2138626563_unbox(AS_ROUT_DEF self) {
 
 return AS_ROU2138626563(((AS_ROUT_DEF) self));
}


SFILE_ID AS_SEL957265064_unbox(AS_SELF_EXPR self) {
 
 return AS_SEL957265064(((AS_SELF_EXPR) self));
}


SFILE_ID AS_SHA1565827735_unbox(AS_SHARED_DEF self) {
 
 return AS_SHA1565827735(((AS_SHARED_DEF) self));
}


SFILE_ID AS_STM1237803845_unbox(AS_STMT self) {
 
 return AS_STM1237803845(((AS_STMT) self));
}


SFILE_ID AS_STM633666973_unbox(AS_STMT_LIST self) {
 
 return AS_STM633666973(((AS_STMT_LIST) self));
}


SFILE_ID AS_STR1687611758_unbox(AS_STR_LIT_EXPR self) {
 
 return AS_STR1687611758(((AS_STR_LIT_EXPR) self));
}


SFILE_ID AS_SYN1451443212_unbox(AS_SYNC_STMT self) {
 
 return AS_SYN1451443212(((AS_SYNC_STMT) self));
}


SFILE_ID AS_TYP1164969549_unbox(AS_TYPECASE_STMT self) {
 
 return AS_TYP1164969549(((AS_TYPECASE_STMT) self));
}


SFILE_ID AS_TYP936135379_unbox(AS_TYPECASE_WHEN self) {
 
 return AS_TYP936135379(((AS_TYPECASE_WHEN) self));
}


SFILE_ID AS_TYP1615798168_unbox(AS_TYPE_SPEC self) {
 
 return AS_TYP1615798168(((AS_TYPE_SPEC) self));
}


SFILE_ID AS_UND1334198639_unbox(AS_UND152986614 self) {
 
 return AS_UND1334198639(((AS_UND152986614) self));
}


SFILE_ID AS_UNL1835784100_unbox(AS_UNLOCK_STMT self) {
 
 return AS_UNL1835784100(((AS_UNLOCK_STMT) self));
}


SFILE_ID AS_VOI1131745048_unbox(AS_VOID_EXPR self) {
 
 return AS_VOI1131745048(((AS_VOID_EXPR) self));
}


SFILE_ID AS_WHE1480938151_unbox(AS_WHERE_EXPR self) {
 
 return AS_WHE1480938151(((AS_WHERE_EXPR) self));
}


SFILE_ID AS_WIT337771262_unbox(AS_WIT1063437351 self) {
 
 return AS_WIT337771262(((AS_WIT1063437351) self));
}


SFILE_ID AS_YIE1856267120_unbox(AS_YIELD_STMT self) {
 
 return AS_YIE1856267120(((AS_YIELD_STMT) self));
}


void CS_OPT63514076_unbox(CS_OPTIONS self, ARRAYSTR arg1) {
 
 CS_OPT63514076(((CS_OPTIONS) self), arg1);
}


STR CONFIG811280218_unbox(CONFIG_DEF self) {
 
 return CONFIG811280218(((CONFIG_DEF) self));
}


STR CONFIG1175493142_unbox(CONFIG_ROUT self) {
 
 return CONFIG1175493142(((CONFIG_ROUT) self));
}


BOOL TP_CLA254882760_unbox(TP_CLASS self, OB arg1) {
 
 return TP_CLA254882760(((TP_CLASS) self), arg1);
}


BOOL TP_ITE1921021286_unbox(TP_ITER self, OB arg1) {
 
 return TP_ITE1921021286(((TP_ITER) self), arg1);
}


BOOL TP_ROU1849661410_unbox(TP_ROUT self, OB arg1) {
 
 return TP_ROU1849661410(((TP_ROUT) self), arg1);
}


BOOL TP_CLA272820496_unbox(TP_CLASS self) {
 
 return TP_CLA272820496(((TP_CLASS) self));
}


BOOL TP_ITE1864681437_unbox(TP_ITER self) {
 
 return TP_ITE1864681437(((TP_ITER) self));
}


BOOL TP_ROU1058813051_unbox(TP_ROUT self) {
 
 return TP_ROU1058813051(((TP_ROUT) self));
}


dAS_CLASS_ELT AS_ATT97460488_unbox(AS_ATTR_DEF self) {
 
 return AS_ATT97460488(((AS_ATTR_DEF) self));
}


dAS_CLASS_ELT AS_CON306298556_unbox(AS_CONST_DEF self) {
 
 return AS_CON306298556(((AS_CONST_DEF) self));
}


dAS_CLASS_ELT AS_INC1775598384_unbox(AS_INC1416917001 self) {
 
 return AS_INC1775598384(((AS_INC1416917001) self));
}


dAS_CLASS_ELT AS_ROU123442773_unbox(AS_ROUT_DEF self) {
 
 return AS_ROU123442773(((AS_ROUT_DEF) self));
}


dAS_CLASS_ELT AS_SHA297921449_unbox(AS_SHARED_DEF self) {
 
 return AS_SHA297921449(((AS_SHARED_DEF) self));
}


void AS_ATT1141535277_unbox(AS_ATTR_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_ATT1141535277(((AS_ATTR_DEF) self), arg1);
}


void AS_CON737776233_unbox(AS_CONST_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_CON737776233(((AS_CONST_DEF) self), arg1);
}


void AS_INC1475294123_unbox(AS_INC1416917001 self, dAS_CLASS_ELT arg1) {
 
 AS_INC1475294123(((AS_INC1416917001) self), arg1);
}


void AS_ROU920632016_unbox(AS_ROUT_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_ROU920632016(((AS_ROUT_DEF) self), arg1);
}


void AS_SHA746153340_unbox(AS_SHARED_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_SHA746153340(((AS_SHARED_DEF) self), arg1);
}


void AS_ATT2024939759_unbox(AS_ATTR_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_ATT2024939759(((AS_ATTR_DEF) self), arg1);
}


void AS_CON1753066199_unbox(AS_CONST_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_CON1753066199(((AS_CONST_DEF) self), arg1);
}


void AS_INC1807545759_unbox(AS_INC1416917001 self, dAS_CLASS_ELT arg1) {
 
 AS_INC1807545759(((AS_INC1416917001) self), arg1);
}


void AS_ROU166638356_unbox(AS_ROUT_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_ROU166638356(((AS_ROUT_DEF) self), arg1);
}


void AS_SHA1271069808_unbox(AS_SHARED_DEF self, dAS_CLASS_ELT arg1) {
 
 AS_SHA1271069808(((AS_SHARED_DEF) self), arg1);
}


void AS_AND1398627607_unbox(AS_AND_EXPR self, SFILE_ID arg1) {
 
 AS_AND1398627607(((AS_AND_EXPR) self), arg1);
}


void AS_ANY72331114_unbox(AS_ANY_EXPR self, SFILE_ID arg1) {
 
 AS_ANY72331114(((AS_ANY_EXPR) self), arg1);
}


void AS_ARR1604409854_unbox(AS_ARRAY_EXPR self, SFILE_ID arg1) {
 
 AS_ARR1604409854(((AS_ARRAY_EXPR) self), arg1);
}


void AS_AT_550257637_unbox(AS_AT_EXPR self, SFILE_ID arg1) {
 
 AS_AT_550257637(((AS_AT_EXPR) self), arg1);
}


void AS_BOO1855503827_unbox(AS_BOOL_LIT_EXPR self, SFILE_ID arg1) {
 
 AS_BOO1855503827(((AS_BOOL_LIT_EXPR) self), arg1);
}


void AS_BOU354169116_unbox(AS_BOU14269336 self, SFILE_ID arg1) {
 
 AS_BOU354169116(((AS_BOU14269336) self), arg1);
}


void AS_BRE973045948_unbox(AS_BREAK_EXPR self, SFILE_ID arg1) {
 
 AS_BRE973045948(((AS_BREAK_EXPR) self), arg1);
}


void AS_CAL2065155647_unbox(AS_CALL_EXPR self, SFILE_ID arg1) {
 
 AS_CAL2065155647(((AS_CALL_EXPR) self), arg1);
}


void AS_CHA604963579_unbox(AS_CHAR_LIT_EXPR self, SFILE_ID arg1) {
 
 AS_CHA604963579(((AS_CHAR_LIT_EXPR) self), arg1);
}


void AS_CLU942370494_unbox(AS_CLUSTER_EXPR self, SFILE_ID arg1) {
 
 AS_CLU942370494(((AS_CLUSTER_EXPR) self), arg1);
}


void AS_CLU1903972459_unbox(AS_CLU553570599 self, SFILE_ID arg1) {
 
 AS_CLU1903972459(((AS_CLU553570599) self), arg1);
}


void AS_COH1216011035_unbox(AS_COHORT_EXPR self, SFILE_ID arg1) {
 
 AS_COH1216011035(((AS_COHORT_EXPR) self), arg1);
}


void AS_CRE312789330_unbox(AS_CREATE_EXPR self, SFILE_ID arg1) {
 
 AS_CRE312789330(((AS_CREATE_EXPR) self), arg1);
}


void AS_EXC5025731_unbox(AS_EXCEPT_EXPR self, SFILE_ID arg1) {
 
 AS_EXC5025731(((AS_EXCEPT_EXPR) self), arg1);
}


void AS_FAR286542611_unbox(AS_FAR_EXPR self, SFILE_ID arg1) {
 
 AS_FAR286542611(((AS_FAR_EXPR) self), arg1);
}


void AS_FLT1911942022_unbox(AS_FLT_LIT_EXPR self, SFILE_ID arg1) {
 
 AS_FLT1911942022(((AS_FLT_LIT_EXPR) self), arg1);
}


void AS_GLO1712525775_unbox(AS_GLOBAL_EXPR self, SFILE_ID arg1) {
 
 AS_GLO1712525775(((AS_GLOBAL_EXPR) self), arg1);
}


void AS_HER1479869719_unbox(AS_HERE_EXPR self, SFILE_ID arg1) {
 
 AS_HER1479869719(((AS_HERE_EXPR) self), arg1);
}


void AS_INI1852876520_unbox(AS_INITIAL_EXPR self, SFILE_ID arg1) {
 
 AS_INI1852876520(((AS_INITIAL_EXPR) self), arg1);
}


void AS_INT2141902673_unbox(AS_INT_LIT_EXPR self, SFILE_ID arg1) {
 
 AS_INT2141902673(((AS_INT_LIT_EXPR) self), arg1);
}


void AS_IS_128975537_unbox(AS_IS_VOID_EXPR self, SFILE_ID arg1) {
 
 AS_IS_128975537(((AS_IS_VOID_EXPR) self), arg1);
}


void AS_NEA1439086781_unbox(AS_NEAR_EXPR self, SFILE_ID arg1) {
 
 AS_NEA1439086781(((AS_NEAR_EXPR) self), arg1);
}


void AS_NEW524908802_unbox(AS_NEW_EXPR self, SFILE_ID arg1) {
 
 AS_NEW524908802(((AS_NEW_EXPR) self), arg1);
}


void AS_OR_784015075_unbox(AS_OR_EXPR self, SFILE_ID arg1) {
 
 AS_OR_784015075(((AS_OR_EXPR) self), arg1);
}


void AS_RES1196907749_unbox(AS_RESULT_EXPR self, SFILE_ID arg1) {
 
 AS_RES1196907749(((AS_RESULT_EXPR) self), arg1);
}


void AS_SEL1190661345_unbox(AS_SELF_EXPR self, SFILE_ID arg1) {
 
 AS_SEL1190661345(((AS_SELF_EXPR) self), arg1);
}


void AS_STR459429129_unbox(AS_STR_LIT_EXPR self, SFILE_ID arg1) {
 
 AS_STR459429129(((AS_STR_LIT_EXPR) self), arg1);
}


void AS_UND813727770_unbox(AS_UND152986614 self, SFILE_ID arg1) {
 
 AS_UND813727770(((AS_UND152986614) self), arg1);
}


void AS_VOI1015295839_unbox(AS_VOID_EXPR self, SFILE_ID arg1) {
 
 AS_VOI1015295839(((AS_VOID_EXPR) self), arg1);
}


void AS_WHE666102736_unbox(AS_WHERE_EXPR self, SFILE_ID arg1) {
 
 AS_WHE666102736(((AS_WHERE_EXPR) self), arg1);
}


dAS_EXPR AS_AND244040648_unbox(AS_AND_EXPR self) {
 
 return AS_AND244040648(((AS_AND_EXPR) self));
}


dAS_EXPR AS_ANY1960655325_unbox(AS_ANY_EXPR self) {
 
 return AS_ANY1960655325(((AS_ANY_EXPR) self));
}


dAS_EXPR AS_ARR1675309363_unbox(AS_ARRAY_EXPR self) {
 
 return AS_ARR1675309363(((AS_ARRAY_EXPR) self));
}


dAS_EXPR AS_AT_246103478_unbox(AS_AT_EXPR self) {
 
 return AS_AT_246103478(((AS_AT_EXPR) self));
}


dAS_EXPR AS_BOO1496152570_unbox(AS_BOOL_LIT_EXPR self) {
 
 return AS_BOO1496152570(((AS_BOOL_LIT_EXPR) self));
}


dAS_EXPR AS_BOU110275841_unbox(AS_BOU14269336 self) {
 
 return AS_BOU110275841(((AS_BOU14269336) self));
}


dAS_EXPR AS_BRE631040475_unbox(AS_BREAK_EXPR self) {
 
 return AS_BRE631040475(((AS_BREAK_EXPR) self));
}


dAS_EXPR AS_CAL1709269196_unbox(AS_CALL_EXPR self) {
 
 return AS_CAL1709269196(((AS_CALL_EXPR) self));
}


dAS_EXPR AS_CHA1807865070_unbox(AS_CHAR_LIT_EXPR self) {
 
 return AS_CHA1807865070(((AS_CHAR_LIT_EXPR) self));
}


dAS_EXPR AS_CLU1420620999_unbox(AS_CLUSTER_EXPR self) {
 
 return AS_CLU1420620999(((AS_CLUSTER_EXPR) self));
}


dAS_EXPR AS_CLU792721656_unbox(AS_CLU553570599 self) {
 
 return AS_CLU792721656(((AS_CLU553570599) self));
}


dAS_EXPR AS_COH1869044806_unbox(AS_COHORT_EXPR self) {
 
 return AS_COH1869044806(((AS_COHORT_EXPR) self));
}


dAS_EXPR AS_CRE1115272673_unbox(AS_CREATE_EXPR self) {
 
 return AS_CRE1115272673(((AS_CREATE_EXPR) self));
}


dAS_EXPR AS_EXC1501278546_unbox(AS_EXCEPT_EXPR self) {
 
 return AS_EXC1501278546(((AS_EXCEPT_EXPR) self));
}


dAS_EXPR AS_FAR2096957028_unbox(AS_FAR_EXPR self) {
 
 return AS_FAR2096957028(((AS_FAR_EXPR) self));
}


dAS_EXPR AS_FLT1204717277_unbox(AS_FLT_LIT_EXPR self) {
 
 return AS_FLT1204717277(((AS_FLT_LIT_EXPR) self));
}


dAS_EXPR AS_GLO1872829932_unbox(AS_GLOBAL_EXPR self) {
 
 return AS_GLO1872829932(((AS_GLOBAL_EXPR) self));
}


dAS_EXPR AS_HER846597914_unbox(AS_HERE_EXPR self) {
 
 return AS_HER846597914(((AS_HERE_EXPR) self));
}


dAS_EXPR AS_INI1732476001_unbox(AS_INITIAL_EXPR self) {
 
 return AS_INI1732476001(((AS_INITIAL_EXPR) self));
}


dAS_EXPR AS_INT896052382_unbox(AS_INT_LIT_EXPR self) {
 
 return AS_INT896052382(((AS_INT_LIT_EXPR) self));
}


dAS_EXPR AS_IS_1211675728_unbox(AS_IS_VOID_EXPR self) {
 
 return AS_IS_1211675728(((AS_IS_VOID_EXPR) self));
}


dAS_EXPR AS_NEA160870810_unbox(AS_NEAR_EXPR self) {
 
 return AS_NEA160870810(((AS_NEAR_EXPR) self));
}


dAS_EXPR AS_NEW162891781_unbox(AS_NEW_EXPR self) {
 
 return AS_NEW162891781(((AS_NEW_EXPR) self));
}


dAS_EXPR AS_OR_261789378_unbox(AS_OR_EXPR self) {
 
 return AS_OR_261789378(((AS_OR_EXPR) self));
}


dAS_EXPR AS_RES945429434_unbox(AS_RESULT_EXPR self) {
 
 return AS_RES945429434(((AS_RESULT_EXPR) self));
}


dAS_EXPR AS_SEL1160430562_unbox(AS_SELF_EXPR self) {
 
 return AS_SEL1160430562(((AS_SELF_EXPR) self));
}


dAS_EXPR AS_STR70439144_unbox(AS_STR_LIT_EXPR self) {
 
 return AS_STR70439144(((AS_STR_LIT_EXPR) self));
}


dAS_EXPR AS_UND1982396421_unbox(AS_UND152986614 self) {
 
 return AS_UND1982396421(((AS_UND152986614) self));
}


dAS_EXPR AS_VOI1856591394_unbox(AS_VOID_EXPR self) {
 
 return AS_VOI1856591394(((AS_VOID_EXPR) self));
}


dAS_EXPR AS_WHE2011036209_unbox(AS_WHERE_EXPR self) {
 
 return AS_WHE2011036209(((AS_WHERE_EXPR) self));
}


void AS_AND1903000239_unbox(AS_AND_EXPR self, dAS_EXPR arg1) {
 
 AS_AND1903000239(((AS_AND_EXPR) self), arg1);
}


void AS_ANY187271084_unbox(AS_ANY_EXPR self, dAS_EXPR arg1) {
 
 AS_ANY187271084(((AS_ANY_EXPR) self), arg1);
}


void AS_ARR471731524_unbox(AS_ARRAY_EXPR self, dAS_EXPR arg1) {
 
 AS_ARR471731524(((AS_ARRAY_EXPR) self), arg1);
}


void AS_AT_1901822931_unbox(AS_AT_EXPR self, dAS_EXPR arg1) {
 
 AS_AT_1901822931(((AS_AT_EXPR) self), arg1);
}


void AS_BOO650888317_unbox(AS_BOOL_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_BOO650888317(((AS_BOOL_LIT_EXPR) self), arg1);
}


void AS_BOU2036765046_unbox(AS_BOU14269336 self, dAS_EXPR arg1) {
 
 AS_BOU2036765046(((AS_BOU14269336) self), arg1);
}


void AS_BRE1516885934_unbox(AS_BREAK_EXPR self, dAS_EXPR arg1) {
 
 AS_BRE1516885934(((AS_BREAK_EXPR) self), arg1);
}


void AS_CAL437771691_unbox(AS_CALL_EXPR self, dAS_EXPR arg1) {
 
 AS_CAL437771691(((AS_CALL_EXPR) self), arg1);
}


void AS_CHA340061339_unbox(AS_CHAR_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_CHA340061339(((AS_CHAR_LIT_EXPR) self), arg1);
}


void AS_CLU726419888_unbox(AS_CLUSTER_EXPR self, dAS_EXPR arg1) {
 
 AS_CLU726419888(((AS_CLUSTER_EXPR) self), arg1);
}


void AS_CLU1354319231_unbox(AS_CLU553570599 self, dAS_EXPR arg1) {
 
 AS_CLU1354319231(((AS_CLU553570599) self), arg1);
}


void AS_COH278881603_unbox(AS_COHORT_EXPR self, dAS_EXPR arg1) {
 
 AS_COH278881603(((AS_COHORT_EXPR) self), arg1);
}


void AS_CRE1032653736_unbox(AS_CREATE_EXPR self, dAS_EXPR arg1) {
 
 AS_CRE1032653736(((AS_CREATE_EXPR) self), arg1);
}


void AS_EXC645762341_unbox(AS_EXCEPT_EXPR self, dAS_EXPR arg1) {
 
 AS_EXC645762341(((AS_EXCEPT_EXPR) self), arg1);
}


void AS_FAR50083859_unbox(AS_FAR_EXPR self, dAS_EXPR arg1) {
 
 AS_FAR50083859(((AS_FAR_EXPR) self), arg1);
}


void AS_FLT943209132_unbox(AS_FLT_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_FLT943209132(((AS_FLT_LIT_EXPR) self), arg1);
}


void AS_GLO275096477_unbox(AS_GLOBAL_EXPR self, dAS_EXPR arg1) {
 
 AS_GLO275096477(((AS_GLOBAL_EXPR) self), arg1);
}


void AS_HER1301328495_unbox(AS_HERE_EXPR self, dAS_EXPR arg1) {
 
 AS_HER1301328495(((AS_HERE_EXPR) self), arg1);
}


void AS_INI414564886_unbox(AS_INITIAL_EXPR self, dAS_EXPR arg1) {
 
 AS_INI414564886(((AS_INITIAL_EXPR) self), arg1);
}


void AS_INT1251874027_unbox(AS_INT_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_INT1251874027(((AS_INT_LIT_EXPR) self), arg1);
}


void AS_IS_936250681_unbox(AS_IS_VOID_EXPR self, dAS_EXPR arg1) {
 
 AS_IS_936250681(((AS_IS_VOID_EXPR) self), arg1);
}


void AS_NEA1986170077_unbox(AS_NEAR_EXPR self, dAS_EXPR arg1) {
 
 AS_NEA1986170077(((AS_NEAR_EXPR) self), arg1);
}


void AS_NEW1985034628_unbox(AS_NEW_EXPR self, dAS_EXPR arg1) {
 
 AS_NEW1985034628(((AS_NEW_EXPR) self), arg1);
}


void AS_OR_1885251509_unbox(AS_OR_EXPR self, dAS_EXPR arg1) {
 
 AS_OR_1885251509(((AS_OR_EXPR) self), arg1);
}


void AS_RES1201611453_unbox(AS_RESULT_EXPR self, dAS_EXPR arg1) {
 
 AS_RES1201611453(((AS_RESULT_EXPR) self), arg1);
}


void AS_SEL987495847_unbox(AS_SELF_EXPR self, dAS_EXPR arg1) {
 
 AS_SEL987495847(((AS_SELF_EXPR) self), arg1);
}


void AS_STR2077487265_unbox(AS_STR_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_STR2077487265(((AS_STR_LIT_EXPR) self), arg1);
}


void AS_UND165529988_unbox(AS_UND152986614 self, dAS_EXPR arg1) {
 
 AS_UND165529988(((AS_UND152986614) self), arg1);
}


void AS_VOI291335015_unbox(AS_VOID_EXPR self, dAS_EXPR arg1) {
 
 AS_VOI291335015(((AS_VOID_EXPR) self), arg1);
}


void AS_WHE136004678_unbox(AS_WHERE_EXPR self, dAS_EXPR arg1) {
 
 AS_WHE136004678(((AS_WHERE_EXPR) self), arg1);
}


void AS_AND1244275737_unbox(AS_AND_EXPR self, dAS_EXPR arg1) {
 
 AS_AND1244275737(((AS_AND_EXPR) self), arg1);
}


void AS_ANY1724395066_unbox(AS_ANY_EXPR self, dAS_EXPR arg1) {
 
 AS_ANY1724395066(((AS_ANY_EXPR) self), arg1);
}


void AS_ARR47654098_unbox(AS_ARRAY_EXPR self, dAS_EXPR arg1) {
 
 AS_ARR47654098(((AS_ARRAY_EXPR) self), arg1);
}


void AS_AT_2092645707_unbox(AS_AT_EXPR self, dAS_EXPR arg1) {
 
 AS_AT_2092645707(((AS_AT_EXPR) self), arg1);
}


void AS_BOO203439875_unbox(AS_BOOL_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_BOO203439875(((AS_BOOL_LIT_EXPR) self), arg1);
}


void AS_BOU2006233068_unbox(AS_BOU14269336 self, dAS_EXPR arg1) {
 
 AS_BOU2006233068(((AS_BOU14269336) self), arg1);
}


void AS_BRE679018004_unbox(AS_BREAK_EXPR self, dAS_EXPR arg1) {
 
 AS_BRE679018004(((AS_BREAK_EXPR) self), arg1);
}


void AS_CAL577747697_unbox(AS_CALL_EXPR self, dAS_EXPR arg1) {
 
 AS_CAL577747697(((AS_CALL_EXPR) self), arg1);
}


void AS_CHA1047100373_unbox(AS_CHAR_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_CHA1047100373(((AS_CHAR_LIT_EXPR) self), arg1);
}


void AS_CLU1700532850_unbox(AS_CLUSTER_EXPR self, dAS_EXPR arg1) {
 
 AS_CLU1700532850(((AS_CLUSTER_EXPR) self), arg1);
}


void AS_CLU738930885_unbox(AS_CLU553570599 self, dAS_EXPR arg1) {
 
 AS_CLU738930885(((AS_CLU553570599) self), arg1);
}


void AS_COH436052917_unbox(AS_COHORT_EXPR self, dAS_EXPR arg1) {
 
 AS_COH436052917(((AS_COHORT_EXPR) self), arg1);
}


void AS_CRE1964853282_unbox(AS_CREATE_EXPR self, dAS_EXPR arg1) {
 
 AS_CRE1964853282(((AS_CREATE_EXPR) self), arg1);
}


void AS_EXC1647038221_unbox(AS_EXCEPT_EXPR self, dAS_EXPR arg1) {
 
 AS_EXC1647038221(((AS_EXCEPT_EXPR) self), arg1);
}


void AS_FAR1938606563_unbox(AS_FAR_EXPR self, dAS_EXPR arg1) {
 
 AS_FAR1938606563(((AS_FAR_EXPR) self), arg1);
}


void AS_FLT259878070_unbox(AS_FLT_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_FLT259878070(((AS_FLT_LIT_EXPR) self), arg1);
}


void AS_GLO930377569_unbox(AS_GLOBAL_EXPR self, dAS_EXPR arg1) {
 
 AS_GLO930377569(((AS_GLOBAL_EXPR) self), arg1);
}


void AS_HER172194233_unbox(AS_HERE_EXPR self, dAS_EXPR arg1) {
 
 AS_HER172194233(((AS_HERE_EXPR) self), arg1);
}


void AS_INI790026824_unbox(AS_INITIAL_EXPR self, dAS_EXPR arg1) {
 
 AS_INI790026824(((AS_INITIAL_EXPR) self), arg1);
}


void AS_INT501000671_unbox(AS_INT_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_INT501000671(((AS_INT_LIT_EXPR) self), arg1);
}


void AS_IS_1523088415_unbox(AS_IS_VOID_EXPR self, dAS_EXPR arg1) {
 
 AS_IS_1523088415(((AS_IS_VOID_EXPR) self), arg1);
}


void AS_NEA1203816563_unbox(AS_NEAR_EXPR self, dAS_EXPR arg1) {
 
 AS_NEA1203816563(((AS_NEAR_EXPR) self), arg1);
}


void AS_NEW2117994542_unbox(AS_NEW_EXPR self, dAS_EXPR arg1) {
 
 AS_NEW2117994542(((AS_NEW_EXPR) self), arg1);
}


void AS_OR_868048877_unbox(AS_OR_EXPR self, dAS_EXPR arg1) {
 
 AS_OR_868048877(((AS_OR_EXPR) self), arg1);
}


void AS_RES1445995595_unbox(AS_RESULT_EXPR self, dAS_EXPR arg1) {
 
 AS_RES1445995595(((AS_RESULT_EXPR) self), arg1);
}


void AS_SEL1452241999_unbox(AS_SELF_EXPR self, dAS_EXPR arg1) {
 
 AS_SEL1452241999(((AS_SELF_EXPR) self), arg1);
}


void AS_STR1192634823_unbox(AS_STR_LIT_EXPR self, dAS_EXPR arg1) {
 
 AS_STR1192634823(((AS_STR_LIT_EXPR) self), arg1);
}


void AS_UND1829175574_unbox(AS_UND152986614 self, dAS_EXPR arg1) {
 
 AS_UND1829175574(((AS_UND152986614) self), arg1);
}


void AS_VOI636768113_unbox(AS_VOID_EXPR self, dAS_EXPR arg1) {
 
 AS_VOI636768113(((AS_VOID_EXPR) self), arg1);
}


void AS_WHE985961216_unbox(AS_WHERE_EXPR self, dAS_EXPR arg1) {
 
 AS_WHE985961216(((AS_WHERE_EXPR) self), arg1);
}


void AS_ATT1987206051_unbox(AS_ATTR_DEF self, BOOL arg1) {
 
 AS_ATT1987206051(((AS_ATTR_DEF) self), arg1);
}


void AS_CON1962795038_unbox(AS_CONST_DEF self, BOOL arg1) {
 
 AS_CON1962795038(((AS_CONST_DEF) self), arg1);
}


void AS_INC874191011_unbox(AS_INC1416917001 self, BOOL arg1) {
 
 AS_INC874191011(((AS_INC1416917001) self), arg1);
}


void AS_ROU1496248876_unbox(AS_ROUT_DEF self, BOOL arg1) {
 
 AS_ROU1496248876(((AS_ROUT_DEF) self), arg1);
}


void AS_SHA905735878_unbox(AS_SHARED_DEF self, BOOL arg1) {
 
 AS_SHA905735878(((AS_SHARED_DEF) self), arg1);
}


void AS_ASS2107317827_unbox(AS_ASSERT_STMT self, SFILE_ID arg1) {
 
 AS_ASS2107317827(((AS_ASSERT_STMT) self), arg1);
}


void AS_ASS1183239070_unbox(AS_ASSIGN_STMT self, SFILE_ID arg1) {
 
 AS_ASS1183239070(((AS_ASSIGN_STMT) self), arg1);
}


void AS_ATT83117804_unbox(AS_ATTACH_STMT self, SFILE_ID arg1) {
 
 AS_ATT83117804(((AS_ATTACH_STMT) self), arg1);
}


void AS_CAS1160155324_unbox(AS_CASE_STMT self, SFILE_ID arg1) {
 
 AS_CAS1160155324(((AS_CASE_STMT) self), arg1);
}


void AS_DEC1092435373_unbox(AS_DEC_STMT self, SFILE_ID arg1) {
 
 AS_DEC1092435373(((AS_DEC_STMT) self), arg1);
}


void AS_EXP1794823411_unbox(AS_EXPR_STMT self, SFILE_ID arg1) {
 
 AS_EXP1794823411(((AS_EXPR_STMT) self), arg1);
}


void AS_FOR1753592510_unbox(AS_FORK_STMT self, SFILE_ID arg1) {
 
 AS_FOR1753592510(((AS_FORK_STMT) self), arg1);
}


void AS_IF_978670018_unbox(AS_IF_STMT self, SFILE_ID arg1) {
 
 AS_IF_978670018(((AS_IF_STMT) self), arg1);
}


void AS_INT765687160_unbox(AS_INT2031239268 self, SFILE_ID arg1) {
 
 AS_INT765687160(((AS_INT2031239268) self), arg1);
}


void AS_LOC1027127163_unbox(AS_LOCK_STMT self, SFILE_ID arg1) {
 
 AS_LOC1027127163(((AS_LOCK_STMT) self), arg1);
}


void AS_LOO1017100380_unbox(AS_LOOP_STMT self, SFILE_ID arg1) {
 
 AS_LOO1017100380(((AS_LOOP_STMT) self), arg1);
}


void AS_PAR1480073124_unbox(AS_PAR_STMT self, SFILE_ID arg1) {
 
 AS_PAR1480073124(((AS_PAR_STMT) self), arg1);
}


void AS_PRO1882650168_unbox(AS_PROTECT_STMT self, SFILE_ID arg1) {
 
 AS_PRO1882650168(((AS_PROTECT_STMT) self), arg1);
}


void AS_QUI971173597_unbox(AS_QUIT_STMT self, SFILE_ID arg1) {
 
 AS_QUI971173597(((AS_QUIT_STMT) self), arg1);
}


void AS_RAI382050006_unbox(AS_RAISE_STMT self, SFILE_ID arg1) {
 
 AS_RAI382050006(((AS_RAISE_STMT) self), arg1);
}


void AS_RET1547122105_unbox(AS_RETURN_STMT self, SFILE_ID arg1) {
 
 AS_RET1547122105(((AS_RETURN_STMT) self), arg1);
}


void AS_STM910122564_unbox(AS_STMT self, SFILE_ID arg1) {
 
 AS_STM910122564(((AS_STMT) self), arg1);
}


void AS_SYN695597675_unbox(AS_SYNC_STMT self, SFILE_ID arg1) {
 
 AS_SYN695597675(((AS_SYNC_STMT) self), arg1);
}


void AS_TYP982956860_unbox(AS_TYPECASE_STMT self, SFILE_ID arg1) {
 
 AS_TYP982956860(((AS_TYPECASE_STMT) self), arg1);
}


void AS_UNL312142309_unbox(AS_UNLOCK_STMT self, SFILE_ID arg1) {
 
 AS_UNL312142309(((AS_UNLOCK_STMT) self), arg1);
}


void AS_WIT1810155147_unbox(AS_WIT1063437351 self, SFILE_ID arg1) {
 
 AS_WIT1810155147(((AS_WIT1063437351) self), arg1);
}


void AS_YIE291659289_unbox(AS_YIELD_STMT self, SFILE_ID arg1) {
 
 AS_YIE291659289(((AS_YIELD_STMT) self), arg1);
}


dAS_STMT AS_ASS255483717_unbox(AS_ASSERT_STMT self) {
 
 return AS_ASS255483717(((AS_ASSERT_STMT) self));
}


dAS_STMT AS_ASS384416990_unbox(AS_ASSIGN_STMT self) {
 
 return AS_ASS384416990(((AS_ASSIGN_STMT) self));
}


dAS_STMT AS_ATT428959228_unbox(AS_ATTACH_STMT self) {
 
 return AS_ATT428959228(((AS_ATTACH_STMT) self));
}


dAS_STMT AS_CAS568284252_unbox(AS_CASE_STMT self) {
 
 return AS_CAS568284252(((AS_CASE_STMT) self));
}


dAS_STMT AS_DEC1501678601_unbox(AS_DEC_STMT self) {
 
 return AS_DEC1501678601(((AS_DEC_STMT) self));
}


dAS_STMT AS_EXP516982707_unbox(AS_EXPR_STMT self) {
 
 return AS_EXP516982707(((AS_EXPR_STMT) self));
}


dAS_STMT AS_FOR721306794_unbox(AS_FORK_STMT self) {
 
 return AS_FOR721306794(((AS_FORK_STMT) self));
}


dAS_STMT AS_IF_192647666_unbox(AS_IF_STMT self) {
 
 return AS_IF_192647666(((AS_IF_STMT) self));
}


dAS_STMT AS_INT1461683196_unbox(AS_INT2031239268 self) {
 
 return AS_INT1461683196(((AS_INT2031239268) self));
}


dAS_STMT AS_LOC1353882229_unbox(AS_LOCK_STMT self) {
 
 return AS_LOC1353882229(((AS_LOCK_STMT) self));
}


dAS_STMT AS_LOO1639447996_unbox(AS_LOOP_STMT self) {
 
 return AS_LOO1639447996(((AS_LOOP_STMT) self));
}


dAS_STMT AS_PAR1281243032_unbox(AS_PAR_STMT self) {
 
 return AS_PAR1281243032(((AS_PAR_STMT) self));
}


dAS_STMT AS_PRO1410999444_unbox(AS_PROTECT_STMT self) {
 
 return AS_PRO1410999444(((AS_PROTECT_STMT) self));
}


dAS_STMT AS_QUI758322419_unbox(AS_QUIT_STMT self) {
 
 return AS_QUI758322419(((AS_QUIT_STMT) self));
}


dAS_STMT AS_RAI287018298_unbox(AS_RAISE_STMT self) {
 
 return AS_RAI287018298(((AS_RAISE_STMT) self));
}


dAS_STMT AS_RET732932159_unbox(AS_RETURN_STMT self) {
 
 return AS_RET732932159(((AS_RETURN_STMT) self));
}


dAS_STMT AS_STM1163003696_unbox(AS_STMT self) {
 
 return AS_STM1163003696(((AS_STMT) self));
}


dAS_STMT AS_SYN57982213_unbox(AS_SYNC_STMT self) {
 
 return AS_SYN57982213(((AS_SYNC_STMT) self));
}


dAS_STMT AS_TYP1323758084_unbox(AS_TYPECASE_STMT self) {
 
 return AS_TYP1323758084(((AS_TYPECASE_STMT) self));
}


dAS_STMT AS_UNL712558323_unbox(AS_UNLOCK_STMT self) {
 
 return AS_UNL712558323(((AS_UNLOCK_STMT) self));
}


dAS_STMT AS_WIT1968509975_unbox(AS_WIT1063437351 self) {
 
 return AS_WIT1968509975(((AS_WIT1063437351) self));
}


dAS_STMT AS_YIE1402665211_unbox(AS_YIELD_STMT self) {
 
 return AS_YIE1402665211(((AS_YIELD_STMT) self));
}


void AS_ASS1892442692_unbox(AS_ASSERT_STMT self, dAS_STMT arg1) {
 
 AS_ASS1892442692(((AS_ASSERT_STMT) self), arg1);
}


void AS_ASS1762623897_unbox(AS_ASSIGN_STMT self, dAS_STMT arg1) {
 
 AS_ASS1762623897(((AS_ASSIGN_STMT) self), arg1);
}


void AS_ATT1718081659_unbox(AS_ATTACH_STMT self, dAS_STMT arg1) {
 
 AS_ATT1718081659(((AS_ATTACH_STMT) self), arg1);
}


void AS_CAS1578756635_unbox(AS_CASE_STMT self, dAS_STMT arg1) {
 
 AS_CAS1578756635(((AS_CASE_STMT) self), arg1);
}


void AS_DEC646247808_unbox(AS_DEC_STMT self, dAS_STMT arg1) {
 
 AS_DEC646247808(((AS_DEC_STMT) self), arg1);
}


void AS_EXP1630058180_unbox(AS_EXPR_STMT self, dAS_STMT arg1) {
 
 AS_EXP1630058180(((AS_EXPR_STMT) self), arg1);
}


void AS_FOR1425734093_unbox(AS_FORK_STMT self, dAS_STMT arg1) {
 
 AS_FOR1425734093(((AS_FORK_STMT) self), arg1);
}


void AS_IF_1955278743_unbox(AS_IF_STMT self, dAS_STMT arg1) {
 
 AS_IF_1955278743(((AS_IF_STMT) self), arg1);
}


void AS_INT685357691_unbox(AS_INT2031239268 self, dAS_STMT arg1) {
 
 AS_INT685357691(((AS_INT2031239268) self), arg1);
}


void AS_LOC793158658_unbox(AS_LOCK_STMT self, dAS_STMT arg1) {
 
 AS_LOC793158658(((AS_LOCK_STMT) self), arg1);
}


void AS_LOO507592891_unbox(AS_LOOP_STMT self, dAS_STMT arg1) {
 
 AS_LOO507592891(((AS_LOOP_STMT) self), arg1);
}


void AS_PAR865797855_unbox(AS_PAR_STMT self, dAS_STMT arg1) {
 
 AS_PAR865797855(((AS_PAR_STMT) self), arg1);
}


void AS_PRO736041443_unbox(AS_PROTECT_STMT self, dAS_STMT arg1) {
 
 AS_PRO736041443(((AS_PROTECT_STMT) self), arg1);
}


void AS_QUI1389603990_unbox(AS_QUIT_STMT self, dAS_STMT arg1) {
 
 AS_QUI1389603990(((AS_QUIT_STMT) self), arg1);
}


void AS_RAI1860022589_unbox(AS_RAISE_STMT self, dAS_STMT arg1) {
 
 AS_RAI1860022589(((AS_RAISE_STMT) self), arg1);
}


void AS_RET1414994250_unbox(AS_RETURN_STMT self, dAS_STMT arg1) {
 
 AS_RET1414994250(((AS_RETURN_STMT) self), arg1);
}


void AS_STM984922713_unbox(AS_STMT self, dAS_STMT arg1) {
 
 AS_STM984922713(((AS_STMT) self), arg1);
}


void AS_SYN2089058674_unbox(AS_SYNC_STMT self, dAS_STMT arg1) {
 
 AS_SYN2089058674(((AS_SYNC_STMT) self), arg1);
}


void AS_TYP823282803_unbox(AS_TYPECASE_STMT self, dAS_STMT arg1) {
 
 AS_TYP823282803(((AS_TYPECASE_STMT) self), arg1);
}


void AS_UNL1435368086_unbox(AS_UNLOCK_STMT self, dAS_STMT arg1) {
 
 AS_UNL1435368086(((AS_UNLOCK_STMT) self), arg1);
}


void AS_WIT178530912_unbox(AS_WIT1063437351 self, dAS_STMT arg1) {
 
 AS_WIT178530912(((AS_WIT1063437351) self), arg1);
}


void AS_YIE745261198_unbox(AS_YIELD_STMT self, dAS_STMT arg1) {
 
 AS_YIE745261198(((AS_YIELD_STMT) self), arg1);
}


void AS_ASS537381530_unbox(AS_ASSERT_STMT self, dAS_STMT arg1) {
 
 AS_ASS537381530(((AS_ASSERT_STMT) self), arg1);
}


void AS_ASS1461460287_unbox(AS_ASSIGN_STMT self, dAS_STMT arg1) {
 
 AS_ASS1461460287(((AS_ASSIGN_STMT) self), arg1);
}


void AS_ATT1733385743_unbox(AS_ATTACH_STMT self, dAS_STMT arg1) {
 
 AS_ATT1733385743(((AS_ATTACH_STMT) self), arg1);
}


void AS_CAS490112615_unbox(AS_CASE_STMT self, dAS_STMT arg1) {
 
 AS_CAS490112615(((AS_CASE_STMT) self), arg1);
}


void AS_DEC557832566_unbox(AS_DEC_STMT self, dAS_STMT arg1) {
 
 AS_DEC557832566(((AS_DEC_STMT) self), arg1);
}


void AS_EXP849875946_unbox(AS_EXPR_STMT self, dAS_STMT arg1) {
 
 AS_EXP849875946(((AS_EXPR_STMT) self), arg1);
}


void AS_FOR103324571_unbox(AS_FORK_STMT self, dAS_STMT arg1) {
 
 AS_FOR103324571(((AS_FORK_STMT) self), arg1);
}


void AS_IF_671597921_unbox(AS_IF_STMT self, dAS_STMT arg1) {
 
 AS_IF_671597921(((AS_IF_STMT) self), arg1);
}


void AS_INT884580779_unbox(AS_INT2031239268 self, dAS_STMT arg1) {
 
 AS_INT884580779(((AS_INT2031239268) self), arg1);
}


void AS_LOC623140776_unbox(AS_LOCK_STMT self, dAS_STMT arg1) {
 
 AS_LOC623140776(((AS_LOCK_STMT) self), arg1);
}


void AS_LOO633167559_unbox(AS_LOOP_STMT self, dAS_STMT arg1) {
 
 AS_LOO633167559(((AS_LOOP_STMT) self), arg1);
}


void AS_PAR170194815_unbox(AS_PAR_STMT self, dAS_STMT arg1) {
 
 AS_PAR170194815(((AS_PAR_STMT) self), arg1);
}


void AS_PRO232382229_unbox(AS_PROTECT_STMT self, dAS_STMT arg1) {
 
 AS_PRO232382229(((AS_PROTECT_STMT) self), arg1);
}


void AS_QUI1673525760_unbox(AS_QUIT_STMT self, dAS_STMT arg1) {
 
 AS_QUI1673525760(((AS_QUIT_STMT) self), arg1);
}


void AS_RAI2032317945_unbox(AS_RAISE_STMT self, dAS_STMT arg1) {
 
 AS_RAI2032317945(((AS_RAISE_STMT) self), arg1);
}


void AS_RET1097577252_unbox(AS_RETURN_STMT self, dAS_STMT arg1) {
 
 AS_RET1097577252(((AS_RETURN_STMT) self), arg1);
}


void AS_STM1734576793_unbox(AS_STMT self, dAS_STMT arg1) {
 
 AS_STM1734576793(((AS_STMT) self), arg1);
}


void AS_SYN954670264_unbox(AS_SYNC_STMT self, dAS_STMT arg1) {
 
 AS_SYN954670264(((AS_SYNC_STMT) self), arg1);
}


void AS_TYP1661742497_unbox(AS_TYPECASE_STMT self, dAS_STMT arg1) {
 
 AS_TYP1661742497(((AS_TYPECASE_STMT) self), arg1);
}


void AS_UNL1962410248_unbox(AS_UNLOCK_STMT self, dAS_STMT arg1) {
 
 AS_UNL1962410248(((AS_UNLOCK_STMT) self), arg1);
}


void AS_WIT834544210_unbox(AS_WIT1063437351 self, dAS_STMT arg1) {
 
 AS_WIT834544210(((AS_WIT1063437351) self), arg1);
}


void AS_YIE1941927228_unbox(AS_YIELD_STMT self, dAS_STMT arg1) {
 
 AS_YIE1941927228(((AS_YIELD_STMT) self), arg1);
}


void AS_ASS2029454919_unbox(AS_ASSERT_STMT self, AS_STMT_LIST arg1) {
 
 AS_ASS2029454919(((AS_ASSERT_STMT) self), arg1);
}


void AS_ASS650357212_unbox(AS_ASSIGN_STMT self, AS_STMT_LIST arg1) {
 
 AS_ASS650357212(((AS_ASSIGN_STMT) self), arg1);
}


void AS_ATT1480967150_unbox(AS_ATTACH_STMT self, AS_STMT_LIST arg1) {
 
 AS_ATT1480967150(((AS_ATTACH_STMT) self), arg1);
}


void AS_CAS703201798_unbox(AS_CASE_STMT self, AS_STMT_LIST arg1) {
 
 AS_CAS703201798(((AS_CASE_STMT) self), arg1);
}


void AS_DEC2062280289_unbox(AS_DEC_STMT self, AS_STMT_LIST arg1) {
 
 AS_DEC2062280289(((AS_DEC_STMT) self), arg1);
}


void AS_EXP200906119_unbox(AS_EXPR_STMT self, AS_STMT_LIST arg1) {
 
 AS_EXP200906119(((AS_EXPR_STMT) self), arg1);
}


void AS_FOR1019127704_unbox(AS_FORK_STMT self, AS_STMT_LIST arg1) {
 
 AS_FOR1019127704(((AS_FORK_STMT) self), arg1);
}


void AS_IF_1658967836_unbox(AS_IF_STMT self, AS_STMT_LIST arg1) {
 
 AS_IF_1658967836(((AS_IF_STMT) self), arg1);
}


void AS_INT1009823030_unbox(AS_INT2031239268 self, AS_STMT_LIST arg1) {
 
 AS_INT1009823030(((AS_INT2031239268) self), arg1);
}


void AS_LOC1144520043_unbox(AS_LOCK_STMT self, AS_STMT_LIST arg1) {
 
 AS_LOC1144520043(((AS_LOCK_STMT) self), arg1);
}


void AS_LOO1542269018_unbox(AS_LOOP_STMT self, AS_STMT_LIST arg1) {
 
 AS_LOO1542269018(((AS_LOOP_STMT) self), arg1);
}


void AS_PAR99672090_unbox(AS_PAR_STMT self, AS_STMT_LIST arg1) {
 
 AS_PAR99672090(((AS_PAR_STMT) self), arg1);
}


void AS_PRO1754180698_unbox(AS_PROTECT_STMT self, AS_STMT_LIST arg1) {
 
 AS_PRO1754180698(((AS_PROTECT_STMT) self), arg1);
}


void AS_QUI2137045949_unbox(AS_QUIT_STMT self, AS_STMT_LIST arg1) {
 
 AS_QUI2137045949(((AS_QUIT_STMT) self), arg1);
}


void AS_RAI2115288988_unbox(AS_RAISE_STMT self, AS_STMT_LIST arg1) {
 
 AS_RAI2115288988(((AS_RAISE_STMT) self), arg1);
}


void AS_RET1042240335_unbox(AS_RETURN_STMT self, AS_STMT_LIST arg1) {
 
 AS_RET1042240335(((AS_RETURN_STMT) self), arg1);
}


void AS_STM193774686_unbox(AS_STMT self, AS_STMT_LIST arg1) {
 
 AS_STM193774686(((AS_STMT) self), arg1);
}


void AS_SYN1817260677_unbox(AS_SYNC_STMT self, AS_STMT_LIST arg1) {
 
 AS_SYN1817260677(((AS_SYNC_STMT) self), arg1);
}


void AS_TYP1802831442_unbox(AS_TYPECASE_STMT self, AS_STMT_LIST arg1) {
 
 AS_TYP1802831442(((AS_TYPECASE_STMT) self), arg1);
}


void AS_UNL619513787_unbox(AS_UNLOCK_STMT self, AS_STMT_LIST arg1) {
 
 AS_UNL619513787(((AS_UNLOCK_STMT) self), arg1);
}


void AS_WIT1949427223_unbox(AS_WIT1063437351 self, AS_STMT_LIST arg1) {
 
 AS_WIT1949427223(((AS_WIT1063437351) self), arg1);
}


void AS_YIE1194255817_unbox(AS_YIELD_STMT self, AS_STMT_LIST arg1) {
 
 AS_YIE1194255817(((AS_YIELD_STMT) self), arg1);
}


AS_CLASS_DEF PARSE_962387968_unbox(PARSE self, IDENT arg1, INT arg2) {
 
 return PARSE_962387968(((PARSE) self), arg1, arg2);
}


BOOL INOUT_394279242_unbox(INOUT_MODE self, dMODE arg1) {
 
 return INOUT_394279242(((INOUT_MODE) self), arg1);
}


BOOL IN_MOD1604254699_unbox(IN_MODE self, dMODE arg1) {
 
 return IN_MOD1604254699(((IN_MODE) self), arg1);
}


BOOL ONCE_M1658631798_unbox(ONCE_MODE self, dMODE arg1) {
 
 return ONCE_M1658631798(((ONCE_MODE) self), arg1);
}


BOOL OUT_MO1200475098_unbox(OUT_MODE self, dMODE arg1) {
 
 return OUT_MO1200475098(((OUT_MODE) self), arg1);
}


STR CS_OPT565500779_unbox(CS_OPTIONS self) {
 
 return CS_OPT565500779(((CS_OPTIONS) self));
}


void PARSE_parse_STR_unbox(PARSE self, STR arg1) {
 
 PARSE_parse_STR(((PARSE) self), arg1);
}


void FIND_T194079625_unbox(FIND_TYPES self) {
 
 FIND_T194079625(((FIND_TYPES) self));
}


BOOL TP_CLA1568240525_unbox(TP_CLASS self) {
 
 return TP_CLA1568240525(((TP_CLASS) self));
}


BOOL TP_ITE1387653699_unbox(TP_ITER self) {
 
 return TP_ITE1387653699(((TP_ITER) self));
}


BOOL TP_ROU1099474123_unbox(TP_ROUT self) {
 
 return TP_ROU1099474123(((TP_ROUT) self));
}


void BUILD_1202586967_unbox(BUILD_TYPE_GRAPH self) {
 
 BUILD_1202586967(((BUILD_TYPE_GRAPH) self));
}


PROG TP_CLA736641047_unbox(TP_CLASS self) {
 
 return TP_CLA736641047(((TP_CLASS) self));
}


PROG TP_ITE1618029866_unbox(TP_ITER self) {
 
 return TP_ITE1618029866(((TP_ITER) self));
}


PROG TP_ROU1724940306_unbox(TP_ROUT self) {
 
 return TP_ROU1724940306(((TP_ROUT) self));
}


BOOL TP_CLA1071798668_unbox(TP_CLASS self) {
 
 return TP_CLA1071798668(((TP_CLASS) self));
}


BOOL TP_ITE532005347_unbox(TP_ITER self) {
 
 return TP_ITE532005347(((TP_ITER) self));
}


BOOL TP_ROU730462331_unbox(TP_ROUT self) {
 
 return TP_ROU730462331(((TP_ROUT) self));
}


BOOL TP_CLA513015767_unbox(TP_CLASS self, dTP arg1) {
 
 return TP_CLA513015767(((TP_CLASS) self), ((dCALL_TP) arg1));
}


BOOL TP_ITE1550467217_unbox(TP_ITER self, dTP arg1) {
 
 return TP_ITE1550467217(((TP_ITER) self), ((dCALL_TP) arg1));
}


BOOL TP_ROU68166391_unbox(TP_ROUT self, dTP arg1) {
 
 return TP_ROU68166391(((TP_ROUT) self), ((dCALL_TP) arg1));
}


INT TP_CLA1653846135_unbox(TP_CLASS self) {
 
 return TP_CLA1653846135(((TP_CLASS) self));
}


INT TP_ITER_kindrINT_unbox(TP_ITER self) {
 
 return TP_ITER_kindrINT(((TP_ITER) self));
}


INT TP_ROUT_kindrINT_unbox(TP_ROUT self) {
 
 return TP_ROUT_kindrINT(((TP_ROUT) self));
}


BOOL TP_CLA727826289_unbox(TP_CLASS self) {
 
 return TP_CLA727826289(((TP_CLASS) self));
}


BOOL TP_ITE1212470094_unbox(TP_ITER self) {
 
 return TP_ITE1212470094(((TP_ITER) self));
}


BOOL TP_ROU1105559654_unbox(TP_ROUT self) {
 
 return TP_ROU1105559654(((TP_ROUT) self));
}


BOOL TP_CLA650455767_unbox(TP_CLASS self) {
 
 return TP_CLA650455767(((TP_CLASS) self));
}


BOOL TP_ITE2052650588_unbox(TP_ITER self) {
 
 return TP_ITE2052650588(((TP_ITER) self));
}


BOOL TP_ROU681177780_unbox(TP_ROUT self) {
 
 return TP_ROU681177780(((TP_ROUT) self));
}


BOOL AS_ATT2034251932_unbox(AS_ATTR_DEF self) {
 
 return AS_ATT2034251932(((AS_ATTR_DEF) self));
}


BOOL AS_CON1915749157_unbox(AS_CONST_DEF self) {
 
 return AS_CON1915749157(((AS_CONST_DEF) self));
}


BOOL AS_INC827145130_unbox(AS_INC1416917001 self) {
 
 return AS_INC827145130(((AS_INC1416917001) self));
}


BOOL AS_ROU1543294757_unbox(AS_ROUT_DEF self) {
 
 return AS_ROU1543294757(((AS_ROUT_DEF) self));
}


BOOL AS_SHA952781759_unbox(AS_SHARED_DEF self) {
 
 return AS_SHA952781759(((AS_SHARED_DEF) self));
}


void CHECK_1724422536_unbox(CHECK_1174552075 self) {
 
 CHECK_1724422536(((CHECK_1174552075) self));
}


void GET_MA42890279_unbox(GET_MAIN_SIG self, STR arg1) {
 
 GET_MA42890279(((GET_MAIN_SIG) self), arg1);
}


SIG INLINE1060094617_unbox(INLINE_ATTR_READ self) {
 
 return INLINE1060094617(((INLINE_ATTR_READ) self));
}


SIG INLINE1360700008_unbox(INLINE1596311811 self) {
 
 return INLINE1360700008(((INLINE1596311811) self));
}


SIG INLINE1204485066_unbox(INLINE1433668901 self) {
 
 return INLINE1204485066(((INLINE1433668901) self));
}


SIG INLINE1283359029_unbox(INLINE1455768374 self) {
 
 return INLINE1283359029(((INLINE1455768374) self));
}


SIG INLINE1848276099_unbox(INLINE_INT_FOLD self) {
 
 return INLINE1848276099(((INLINE_INT_FOLD) self));
}


SIG INLINE1334459112_unbox(INLINE_ROUT self) {
 
 return INLINE1334459112(((INLINE_ROUT) self));
}


SIG INLINE1854659937_unbox(INLINE1164904870 self) {
 
 return INLINE1854659937(((INLINE1164904870) self));
}


BOOL AM_ARR62730649_unbox(AM_ARR_CONST self, OB arg1) {
 
 return AM_ARR62730649(((AM_ARR_CONST) self), arg1);
}


BOOL AM_BOO1499785931_unbox(AM_BOOL_CONST self, OB arg1) {
 
 return AM_BOO1499785931(((AM_BOOL_CONST) self), arg1);
}


BOOL AM_CHA181917731_unbox(AM_CHAR_CONST self, OB arg1) {
 
 return AM_CHA181917731(((AM_CHAR_CONST) self), arg1);
}


BOOL AM_CON357292637_unbox(AM_CONST self, OB arg1) {
 
 return AM_CON357292637(((AM_CONST) self), arg1);
}


BOOL AM_FLT1602735735_unbox(AM_FLTDX_CONST self, OB arg1) {
 
 return AM_FLT1602735735(((AM_FLTDX_CONST) self), arg1);
}


BOOL AM_FLT1144488899_unbox(AM_FLTD_CONST self, OB arg1) {
 
 return AM_FLT1144488899(((AM_FLTD_CONST) self), arg1);
}


BOOL AM_FLT1011996250_unbox(AM_FLTI_CONST self, OB arg1) {
 
 return AM_FLT1011996250(((AM_FLTI_CONST) self), arg1);
}


BOOL AM_FLT1108482895_unbox(AM_FLTX_CONST self, OB arg1) {
 
 return AM_FLT1108482895(((AM_FLTX_CONST) self), arg1);
}


BOOL AM_FLT1739286436_unbox(AM_FLT_CONST self, OB arg1) {
 
 return AM_FLT1739286436(((AM_FLT_CONST) self), arg1);
}


BOOL AM_INT586605643_unbox(AM_INTI_CONST self, OB arg1) {
 
 return AM_INT586605643(((AM_INTI_CONST) self), arg1);
}


BOOL AM_INT1567340277_unbox(AM_INT_CONST self, OB arg1) {
 
 return AM_INT1567340277(((AM_INT_CONST) self), arg1);
}


BOOL AM_STR2039863889_unbox(AM_STR_CONST self, OB arg1) {
 
 return AM_STR2039863889(((AM_STR_CONST) self), arg1);
}


BOOL AM_VOI2144357369_unbox(AM_VOID_CONST self, OB arg1) {
 
 return AM_VOI2144357369(((AM_VOID_CONST) self), arg1);
}


BOOL AM_ANY1687381320_unbox(AM_ANY_EXPR self, OB arg1) {
 
 return AM_ANY1687381320(((AM_ANY_EXPR) self), arg1);
}


BOOL AM_ARR1751046750_unbox(AM_ARRAY_EXPR self, OB arg1) {
 
 return AM_ARR1751046750(((AM_ARRAY_EXPR) self), arg1);
}


BOOL AM_ATT1818896075_unbox(AM_ATTR_EXPR self, OB arg1) {
 
 return AM_ATT1818896075(((AM_ATTR_EXPR) self), arg1);
}


BOOL AM_AT_1010480980_unbox(AM_AT_EXPR self, OB arg1) {
 
 return AM_AT_1010480980(((AM_AT_EXPR) self), arg1);
}


BOOL AM_BND1588574812_unbox(AM_BND1124877163 self, OB arg1) {
 
 return AM_BND1588574812(((AM_BND1124877163) self), arg1);
}


BOOL AM_BND48137285_unbox(AM_BND367211769 self, OB arg1) {
 
 return AM_BND48137285(((AM_BND367211769) self), arg1);
}


BOOL AM_BND1666770893_unbox(AM_BND260301329 self, OB arg1) {
 
 return AM_BND1666770893(((AM_BND260301329) self), arg1);
}


BOOL AM_CLU528836536_unbox(AM_CLUSTER_EXPR self, OB arg1) {
 
 return AM_CLU528836536(((AM_CLUSTER_EXPR) self), arg1);
}


BOOL AM_CLU1159843664_unbox(AM_CLU1286269335 self, OB arg1) {
 
 return AM_CLU1159843664(((AM_CLU1286269335) self), arg1);
}


BOOL AM_EXC30103808_unbox(AM_EXCEPT_EXPR self, OB arg1) {
 
 return AM_EXC30103808(((AM_EXCEPT_EXPR) self), arg1);
}


BOOL AM_EXT1489112584_unbox(AM_EXT_CALL_EXPR self, OB arg1) {
 
 return AM_EXT1489112584(((AM_EXT_CALL_EXPR) self), arg1);
}


BOOL AM_FAR1472604699_unbox(AM_FAR_EXPR self, OB arg1) {
 
 return AM_FAR1472604699(((AM_FAR_EXPR) self), arg1);
}


BOOL AM_GLO286344550_unbox(AM_GLOBAL_EXPR self, OB arg1) {
 
 return AM_GLO286344550(((AM_GLOBAL_EXPR) self), arg1);
}


BOOL AM_HER1833766706_unbox(AM_HERE_EXPR self, OB arg1) {
 
 return AM_HER1833766706(((AM_HERE_EXPR) self), arg1);
}


BOOL AM_IF_2781110_unbox(AM_IF_EXPR self, OB arg1) {
 
 return AM_IF_2781110(((AM_IF_EXPR) self), arg1);
}


BOOL AM_IS_997133203_unbox(AM_IS_VOID_EXPR self, OB arg1) {
 
 return AM_IS_997133203(((AM_IS_VOID_EXPR) self), arg1);
}


BOOL AM_ITE1901971608_unbox(AM_ITE1809135850 self, OB arg1) {
 
 return AM_ITE1901971608(((AM_ITE1809135850) self), arg1);
}


BOOL AM_LOC783953200_unbox(AM_LOCAL_EXPR self, OB arg1) {
 
 return AM_LOC783953200(((AM_LOCAL_EXPR) self), arg1);
}


BOOL AM_NEA499164018_unbox(AM_NEAR_EXPR self, OB arg1) {
 
 return AM_NEA499164018(((AM_NEAR_EXPR) self), arg1);
}


BOOL AM_NEW1485150288_unbox(AM_NEW_EXPR self, OB arg1) {
 
 return AM_NEW1485150288(((AM_NEW_EXPR) self), arg1);
}


BOOL AM_ROU774362080_unbox(AM_ROU1916046290 self, OB arg1) {
 
 return AM_ROU774362080(((AM_ROU1916046290) self), arg1);
}


BOOL AM_SHA623484008_unbox(AM_SHARED_EXPR self, OB arg1) {
 
 return AM_SHA623484008(((AM_SHARED_EXPR) self), arg1);
}


BOOL AM_STM214668158_unbox(AM_STMT_EXPR self, OB arg1) {
 
 return AM_STM214668158(((AM_STMT_EXPR) self), arg1);
}


BOOL AM_VAR1818998713_unbox(AM_VAR744470097 self, OB arg1) {
 
 return AM_VAR1818998713(((AM_VAR744470097) self), arg1);
}


BOOL AM_VAT2140838301_unbox(AM_VAT319982528 self, OB arg1) {
 
 return AM_VAT2140838301(((AM_VAT319982528) self), arg1);
}


BOOL AM_WHE332796028_unbox(AM_WHERE_EXPR self, OB arg1) {
 
 return AM_WHE332796028(((AM_WHERE_EXPR) self), arg1);
}


BOOL TUPTP_744064396_unbox(TUPTP_1668454172_boxed self, OB arg1) {

 TUPTP_1668454172 L101; 
L101 = ((TUPTP_1668454172_boxed) self)->immutable_part;
 return TUPTP_744064396(L101, arg1);
}


BOOL TUPTP_2145724346_unbox(TUPTP_858321267_boxed self, OB arg1) {

 TUPTP_858321267 L102; 
L102 = ((TUPTP_858321267_boxed) self)->immutable_part;
 return TUPTP_2145724346(L102, arg1);
}


BOOL TUPSTR1375565929_unbox(TUPSTRSTR_boxed self, OB arg1) {

 TUPSTRSTR L103; 
L103 = ((TUPSTRSTR_boxed) self)->immutable_part;
 return TUPSTR1375565929(L103, arg1);
}


BOOL TUPSTR1921411213_unbox(TUPSTRSIG_boxed self, OB arg1) {

 TUPSTRSIG L104; 
L104 = ((TUPSTRSIG_boxed) self)->immutable_part;
 return TUPSTR1921411213(L104, arg1);
}


BOOL TUPSTR1473708895_unbox(TUPSTRINT_boxed self, OB arg1) {

 TUPSTRINT L105; 
L105 = ((TUPSTRINT_boxed) self)->immutable_part;
 return TUPSTR1473708895(L105, arg1);
}


BOOL TUPSTR850883487_unbox(TUPSTRFSETSTR_boxed self, OB arg1) {

 TUPSTRFSETSTR L106; 
L106 = ((TUPSTRFSETSTR_boxed) self)->immutable_part;
 return TUPSTR850883487(L106, arg1);
}


BOOL TUPSTR1795207828_unbox(TUPSTRdCONFIG_boxed self, OB arg1) {

 TUPSTRdCONFIG L107; 
L107 = ((TUPSTRdCONFIG_boxed) self)->immutable_part;
 return TUPSTR1795207828(L107, arg1);
}


BOOL TUPSIG116886773_unbox(TUPSIGSTR_boxed self, OB arg1) {

 TUPSIGSTR L108; 
L108 = ((TUPSIGSTR_boxed) self)->immutable_part;
 return TUPSIG116886773(L108, arg1);
}


BOOL TUPSIG662732057_unbox(TUPSIGSIG_boxed self, OB arg1) {

 TUPSIGSIG L109; 
L109 = ((TUPSIGSIG_boxed) self)->immutable_part;
 return TUPSIG662732057(L109, arg1);
}


BOOL TUPSIG215029739_unbox(TUPSIGINT_boxed self, OB arg1) {

 TUPSIGINT L110; 
L110 = ((TUPSIGINT_boxed) self)->immutable_part;
 return TUPSIG215029739(L110, arg1);
}


BOOL TUPSIG174310016_unbox(TUPSIG2023533247_boxed self, OB arg1) {

 TUPSIG2023533247 L111; 
L111 = ((TUPSIG2023533247_boxed) self)->immutable_part;
 return TUPSIG174310016(L111, arg1);
}


BOOL TUPSIG738670088_unbox(TUPSIGARG_LAYOUT_boxed self, OB arg1) {

 TUPSIGARG_LAYOUT L112; 
L112 = ((TUPSIGARG_LAYOUT_boxed) self)->immutable_part;
 return TUPSIG738670088(L112, arg1);
}


BOOL TUPSIG848243115_unbox(TUPSIG578692189_boxed self, OB arg1) {

 TUPSIG578692189 L113; 
L113 = ((TUPSIG578692189_boxed) self)->immutable_part;
 return TUPSIG848243115(L113, arg1);
}


BOOL TUPSIG1705966004_unbox(TUPSIG1754078736_boxed self, OB arg1) {

 TUPSIG1754078736 L114; 
L114 = ((TUPSIG1754078736_boxed) self)->immutable_part;
 return TUPSIG1705966004(L114, arg1);
}


BOOL TUPINT326447217_unbox(TUPINTINT_boxed self, OB arg1) {

 TUPINTINT L115; 
L115 = ((TUPINTINT_boxed) self)->immutable_part;
 return TUPINT326447217(L115, arg1);
}


BOOL TUPIDE53463463_unbox(TUPIDENTINT_boxed self, OB arg1) {

 TUPIDENTINT L116; 
L116 = ((TUPIDENTINT_boxed) self)->immutable_part;
 return TUPIDE53463463(L116, arg1);
}


BOOL TUPIDE1955445151_unbox(TUPIDENTARRAYdTP_boxed self, OB arg1) {

 TUPIDENTARRAYdTP L117; 
L117 = ((TUPIDENTARRAYdTP_boxed) self)->immutable_part;
 return TUPIDE1955445151(L117, arg1);
}


BOOL TUPIDE1222548283_unbox(TUPIDE641654124_boxed self, OB arg1) {

 TUPIDE641654124 L118; 
L118 = ((TUPIDE641654124_boxed) self)->immutable_part;
 return TUPIDE1222548283(L118, arg1);
}


BOOL TUPIDE429159832_unbox(TUPIDENTdTP_boxed self, OB arg1) {

 TUPIDENTdTP L119; 
L119 = ((TUPIDENTdTP_boxed) self)->immutable_part;
 return TUPIDE429159832(L119, arg1);
}


BOOL TUPBOO323223880_unbox(TUPBOO1392923241_boxed self, OB arg1) {

 TUPBOO1392923241 L120; 
L120 = ((TUPBOO1392923241_boxed) self)->immutable_part;
 return TUPBOO323223880(L120, arg1);
}


BOOL TUPARR1838121920_unbox(TUPARR1759987776_boxed self, OB arg1) {

 TUPARR1759987776 L121; 
L121 = ((TUPARR1759987776_boxed) self)->immutable_part;
 return TUPARR1838121920(L121, arg1);
}


BOOL TUPARR1107471104_unbox(TUPARRAYARGdTP_boxed self, OB arg1) {

 TUPARRAYARGdTP L122; 
L122 = ((TUPARRAYARGdTP_boxed) self)->immutable_part;
 return TUPARR1107471104(L122, arg1);
}


BOOL TUPAM_1711472971_unbox(TUPAM_653469574_boxed self, OB arg1) {

 TUPAM_653469574 L123; 
L123 = ((TUPAM_653469574_boxed) self)->immutable_part;
 return TUPAM_1711472971(L123, arg1);
}


BOOL TUPAM_1144039098_unbox(TUPAM_1024994801_boxed self, OB arg1) {

 TUPAM_1024994801 L124; 
L124 = ((TUPAM_1024994801_boxed) self)->immutable_part;
 return TUPAM_1144039098(L124, arg1);
}


BOOL TUPAM_1196406350_unbox(TUPAM_504653531_boxed self, OB arg1) {

 TUPAM_504653531 L125; 
L125 = ((TUPAM_504653531_boxed) self)->immutable_part;
 return TUPAM_1196406350(L125, arg1);
}


BOOL TUPAM_1557192709_unbox(TUPAM_1519040877_boxed self, OB arg1) {

 TUPAM_1519040877 L126; 
L126 = ((TUPAM_1519040877_boxed) self)->immutable_part;
 return TUPAM_1557192709(L126, arg1);
}


BOOL TUPAM_1783991982_unbox(TUPAM_927437481_boxed self, OB arg1) {

 TUPAM_927437481 L127; 
L127 = ((TUPAM_927437481_boxed) self)->immutable_part;
 return TUPAM_1783991982(L127, arg1);
}


BOOL TUPAM_2045709861_unbox(TUPAM_667892060_boxed self, OB arg1) {

 TUPAM_667892060 L128; 
L128 = ((TUPAM_667892060_boxed) self)->immutable_part;
 return TUPAM_2045709861(L128, arg1);
}


BOOL TUPAM_2013149714_unbox(TUPAM_304458649_boxed self, OB arg1) {

 TUPAM_304458649 L129; 
L129 = ((TUPAM_304458649_boxed) self)->immutable_part;
 return TUPAM_2013149714(L129, arg1);
}


BOOL TUPdTP1433052028_unbox(TUPdTPINT_boxed self, OB arg1) {

 TUPdTPINT L130; 
L130 = ((TUPdTPINT_boxed) self)->immutable_part;
 return TUPdTP1433052028(L130, arg1);
}


BOOL TUPdTP1282296289_unbox(TUPdTP1710233051_boxed self, OB arg1) {

 TUPdTP1710233051 L131; 
L131 = ((TUPdTP1710233051_boxed) self)->immutable_part;
 return TUPdTP1282296289(L131, arg1);
}


BOOL TUPdTP1961471532_unbox(TUPdTPCODE_FILE_boxed self, OB arg1) {

 TUPdTPCODE_FILE L132; 
L132 = ((TUPdTPCODE_FILE_boxed) self)->immutable_part;
 return TUPdTP1961471532(L132, arg1);
}


BOOL TUPdTP1932220599_unbox(TUPdTPdLAYOUT_boxed self, OB arg1) {

 TUPdTPdLAYOUT L133; 
L133 = ((TUPdTPdLAYOUT_boxed) self)->immutable_part;
 return TUPdTP1932220599(L133, arg1);
}


BOOL TUPdOB398929495_unbox(TUPdOBSTR_boxed self, OB arg1) {

 TUPdOBSTR L134; 
L134 = ((TUPdOBSTR_boxed) self)->immutable_part;
 return TUPdOB398929495(L134, arg1);
}


BOOL TUPdOB134201346_unbox(TUPdOBNAMESPACE_boxed self, OB arg1) {

 TUPdOBNAMESPACE L135; 
L135 = ((TUPdOBNAMESPACE_boxed) self)->immutable_part;
 return TUPdOB134201346(L135, arg1);
}


BOOL TUPdAS1082207056_unbox(TUPdAS268679779_boxed self, OB arg1) {

 TUPdAS268679779 L136; 
L136 = ((TUPdAS268679779_boxed) self)->immutable_part;
 return TUPdAS1082207056(L136, arg1);
}


BOOL TUPdAM1479867_unbox(TUPdAM_EXPRdTP_boxed self, OB arg1) {

 TUPdAM_EXPRdTP L137; 
L137 = ((TUPdAM_EXPRdTP_boxed) self)->immutable_part;
 return TUPdAM1479867(L137, arg1);
}


BOOL SFILE_947438414_unbox(SFILE_ID_boxed self, OB arg1) {

 SFILE_ID L138; 
L138 = ((SFILE_ID_boxed) self)->immutable_part;
 return SFILE_947438414(L138, arg1);
}


BOOL RAT_is1975945358_unbox(RAT_boxed self, OB arg1) {

 RAT L139; 
L139 = ((RAT_boxed) self)->immutable_part;
 return RAT_is1975945358(L139, arg1);
}


BOOL INT_is240310584_unbox(INT_boxed self, OB arg1) {

 INT L140; 
L140 = ((INT_boxed) self)->immutable_part;
 return INT_is240310584(L140, arg1);
}


BOOL IDENT_1780939044_unbox(IDENT_boxed self, OB arg1) {

 IDENT L141; 
L141 = ((IDENT_boxed) self)->immutable_part;
 return IDENT_1780939044(L141, arg1);
}


BOOL FLTD_i1285721587_unbox(FLTD_boxed self, OB arg1) {

 FLTD L142; 
L142 = ((FLTD_boxed) self)->immutable_part;
 return FLTD_i1285721587(L142, arg1);
}


BOOL FLT_is1351417193_unbox(FLT_boxed self, OB arg1) {

 FLT L143; 
L143 = ((FLT_boxed) self)->immutable_part;
 return FLT_is1351417193(L143, arg1);
}


BOOL CHAR_i1237803373_unbox(CHAR_boxed self, OB arg1) {

 CHAR L144; 
L144 = ((CHAR_boxed) self)->immutable_part;
 return CHAR_i1237803373(L144, arg1);
}


BOOL BOOL_i1290536107_unbox(BOOL_boxed self, OB arg1) {

 BOOL L145; 
L145 = ((BOOL_boxed) self)->immutable_part;
 return BOOL_i1290536107(L145, arg1);
}


BOOL AM_CUR1683770295_unbox(AM_CURSOR self, OB arg1) {
 
 return AM_CUR1683770295(((AM_CURSOR) self), arg1);
}


BOOL AM_CUR217486862_unbox(AM_CURSOR_POS self, OB arg1) {
 
 return AM_CUR217486862(((AM_CURSOR_POS) self), arg1);
}


BOOL ATTRSO1207195602_unbox(ATTRSORT self, OB arg1) {
 
 return ATTRSO1207195602(((ATTRSORT) self), arg1);
}


BOOL CSE_DO4156401_unbox(CSE_DOUBLE_EXPRS self, OB arg1) {
 
 return CSE_DO4156401(((CSE_DOUBLE_EXPRS) self), arg1);
}


BOOL FSTR_i135169312_unbox(FSTR self, OB arg1) {
 
 return FSTR_i135169312(((FSTR) self), arg1);
}


BOOL INTI_i1060921131_unbox(INTI1 self, OB arg1) {
 
 return INTI_i1060921131(((INTI1) self), arg1);
}


BOOL SIDE_E1243435218_unbox(SIDE_EFFECT self, OB arg1) {
 
 return SIDE_E1243435218(((SIDE_EFFECT) self), arg1);
}


BOOL SIG_is688012902_unbox(SIG self, OB arg1) {
 
 return SIG_is688012902(((SIG) self), arg1);
}


BOOL STR_is142167618_unbox(STR self, OB arg1) {
 
 return STR_is142167618(((STR) self), arg1);
}


void INLINE_init_unbox(INLINE self) {
 
 INLINE_init(((INLINE) self));
}


SIG GET_MA2025156715_unbox(GET_MAIN_SIG self) {
 
 return GET_MA2025156715(((GET_MAIN_SIG) self));
}


void CGEN_init_unbox(CGEN self) {
 
 CGEN_init(((CGEN) self));
}


void OPTIMIZE_init_unbox(OPTIMIZE self) {
 
 OPTIMIZE_init(((OPTIMIZE) self));
}


IFC TP_CLASS_ifcrIFC_unbox(TP_CLASS self) {
 
 return TP_CLASS_ifcrIFC(((TP_CLASS) self));
}


IFC TP_ITER_ifcrIFC_unbox(TP_ITER self) {
 
 return TP_ITER_ifcrIFC(((TP_ITER) self));
}


IFC TP_ROUT_ifcrIFC_unbox(TP_ROUT self) {
 
 return TP_ROUT_ifcrIFC(((TP_ROUT) self));
}


BOOL TP_CLA1015994406_unbox(TP_CLASS self) {
 
 return TP_CLA1015994406(((TP_CLASS) self));
}


BOOL TP_ITE1879811989_unbox(TP_ITER self) {
 
 return TP_ITE1879811989(((TP_ITER) self));
}


BOOL TP_ROU699367341_unbox(TP_ROUT self) {
 
 return TP_ROU699367341(((TP_ROUT) self));
}


dTP AM_ANY772124871_unbox(AM_ANY_EXPR self) {
 
 return AM_ANY772124871(((AM_ANY_EXPR) self));
}


dTP AM_ARR1099277377_unbox(AM_ARRAY_EXPR self) {
 
 return AM_ARR1099277377(((AM_ARRAY_EXPR) self));
}


dTP AM_ARR925292072_unbox(AM_ARR_CONST self) {
 
 return AM_ARR925292072(((AM_ARR_CONST) self));
}


dTP AM_ATT839964902_unbox(AM_ATTR_EXPR self) {
 
 return AM_ATT839964902(((AM_ATTR_EXPR) self));
}


dTP AM_AT_1635564765_unbox(AM_AT_EXPR self) {
 
 return AM_AT_1635564765(((AM_AT_EXPR) self));
}


dTP AM_BND540727053_unbox(AM_BND1124877163 self) {
 
 return AM_BND540727053(((AM_BND1124877163) self));
}


dTP AM_BND95745606_unbox(AM_BND367211769 self) {
 
 return AM_BND95745606(((AM_BND367211769) self));
}


dTP AM_BND192433970_unbox(AM_BND260301329 self) {
 
 return AM_BND192433970(((AM_BND260301329) self));
}


dTP AM_BOO1614359222_unbox(AM_BOOL_CONST self) {
 
 return AM_BOO1614359222(((AM_BOOL_CONST) self));
}


dTP AM_CHA165359522_unbox(AM_CHAR_CONST self) {
 
 return AM_CHA165359522(((AM_CHAR_CONST) self));
}


dTP AM_CLU889770873_unbox(AM_CLUSTER_EXPR self) {
 
 return AM_CLU889770873(((AM_CLUSTER_EXPR) self));
}


dTP AM_CLU2134150433_unbox(AM_CLU1286269335 self) {
 
 return AM_CLU2134150433(((AM_CLU1286269335) self));
}


dTP AM_CONST_tprdTP_unbox(AM_CONST self) {
 
 return AM_CONST_tprdTP(((AM_CONST) self));
}


dTP AM_EXC706383345_unbox(AM_EXCEPT_EXPR self) {
 
 return AM_EXC706383345(((AM_EXCEPT_EXPR) self));
}


dTP AM_EXT195226505_unbox(AM_EXT_CALL_EXPR self) {
 
 return AM_EXT195226505(((AM_EXT_CALL_EXPR) self));
}


dTP AM_FAR24566214_unbox(AM_FAR_EXPR self) {
 
 return AM_FAR24566214(((AM_FAR_EXPR) self));
}


dTP AM_FLT1597643896_unbox(AM_FLTDX_CONST self) {
 
 return AM_FLT1597643896(((AM_FLTDX_CONST) self));
}


dTP AM_FLT2085750594_unbox(AM_FLTD_CONST self) {
 
 return AM_FLT2085750594(((AM_FLTD_CONST) self));
}


dTP AM_FLT804205627_unbox(AM_FLTI_CONST self) {
 
 return AM_FLT804205627(((AM_FLTI_CONST) self));
}


dTP AM_FLT884139698_unbox(AM_FLTX_CONST self) {
 
 return AM_FLT884139698(((AM_FLTX_CONST) self));
}


dTP AM_FLT79327789_unbox(AM_FLT_CONST self) {
 
 return AM_FLT79327789(((AM_FLT_CONST) self));
}


dTP AM_GLO541877835_unbox(AM_GLOBAL_EXPR self) {
 
 return AM_GLO541877835(((AM_GLOBAL_EXPR) self));
}


dTP AM_HER217252257_unbox(AM_HERE_EXPR self) {
 
 return AM_HER217252257(((AM_HERE_EXPR) self));
}


dTP AM_IF_179347781_unbox(AM_IF_EXPR self) {
 
 return AM_IF_179347781(((AM_IF_EXPR) self));
}


dTP AM_INT1372528330_unbox(AM_INTI_CONST self) {
 
 return AM_INT1372528330(((AM_INTI_CONST) self));
}


dTP AM_INT1391542618_unbox(AM_INT_CONST self) {
 
 return AM_INT1391542618(((AM_INT_CONST) self));
}


dTP AM_IS_826521970_unbox(AM_IS_VOID_EXPR self) {
 
 return AM_IS_826521970(((AM_IS_VOID_EXPR) self));
}


dTP AM_ITE597285895_unbox(AM_ITE1809135850 self) {
 
 return AM_ITE597285895(((AM_ITE1809135850) self));
}


dTP AM_LOC1004636145_unbox(AM_LOCAL_EXPR self) {
 
 return AM_LOC1004636145(((AM_LOCAL_EXPR) self));
}


dTP AM_NEA176857219_unbox(AM_NEAR_EXPR self) {
 
 return AM_NEA176857219(((AM_NEAR_EXPR) self));
}


dTP AM_NEW710003407_unbox(AM_NEW_EXPR self) {
 
 return AM_NEW710003407(((AM_NEW_EXPR) self));
}


dTP AM_ROU885465471_unbox(AM_ROU1916046290 self) {
 
 return AM_ROU885465471(((AM_ROU1916046290) self));
}


dTP AM_SHA1973416871_unbox(AM_SHARED_EXPR self) {
 
 return AM_SHA1973416871(((AM_SHARED_EXPR) self));
}


dTP AM_STM300451057_unbox(AM_STMT_EXPR self) {
 
 return AM_STM300451057(((AM_STMT_EXPR) self));
}


dTP AM_STR690233376_unbox(AM_STR_CONST self) {
 
 return AM_STR690233376(((AM_STR_CONST) self));
}


dTP AM_VAR2117196902_unbox(AM_VAR744470097 self) {
 
 return AM_VAR2117196902(((AM_VAR744470097) self));
}


dTP AM_VAT1812280108_unbox(AM_VAT319982528 self) {
 
 return AM_VAT1812280108(((AM_VAT319982528) self));
}


dTP AM_VOI984518984_unbox(AM_VOID_CONST self) {
 
 return AM_VOI984518984(((AM_VOID_CONST) self));
}


dTP AM_WHE843899429_unbox(AM_WHERE_EXPR self) {
 
 return AM_WHE843899429(((AM_WHERE_EXPR) self));
}


BOOL AM_ANY309199231_unbox(AM_ANY_EXPR self) {
 
 return AM_ANY309199231(((AM_ANY_EXPR) self));
}


BOOL AM_ARR437965311_unbox(AM_ARRAY_EXPR self) {
 
 return AM_ARR437965311(((AM_ARRAY_EXPR) self));
}


BOOL AM_ARR495057476_unbox(AM_ARR_CONST self) {
 
 return AM_ARR495057476(((AM_ARR_CONST) self));
}


BOOL AM_ATT1582125658_unbox(AM_ATTR_EXPR self) {
 
 return AM_ATT1582125658(((AM_ATTR_EXPR) self));
}


BOOL AM_AT_1425681295_unbox(AM_AT_EXPR self) {
 
 return AM_AT_1425681295(((AM_AT_EXPR) self));
}


BOOL AM_BND9208257_unbox(AM_BND1124877163 self) {
 
 return AM_BND9208257(((AM_BND1124877163) self));
}


BOOL AM_BND1565517842_unbox(AM_BND367211769 self) {
 
 return AM_BND1565517842(((AM_BND367211769) self));
}


BOOL AM_BND1960601142_unbox(AM_BND260301329 self) {
 
 return AM_BND1960601142(((AM_BND260301329) self));
}


BOOL AM_BOO274293822_unbox(AM_BOOL_CONST self) {
 
 return AM_BOO274293822(((AM_BOOL_CONST) self));
}


BOOL AM_CHA1550967974_unbox(AM_CHAR_CONST self) {
 
 return AM_CHA1550967974(((AM_CHAR_CONST) self));
}


BOOL AM_CLU862886673_unbox(AM_CLUSTER_EXPR self) {
 
 return AM_CLU862886673(((AM_CLUSTER_EXPR) self));
}


BOOL AM_CLU170440611_unbox(AM_CLU1286269335 self) {
 
 return AM_CLU170440611(((AM_CLU1286269335) self));
}


BOOL AM_CON174643192_unbox(AM_CONST self) {
 
 return AM_CON174643192(((AM_CONST) self));
}


BOOL AM_EXC219044661_unbox(AM_EXCEPT_EXPR self) {
 
 return AM_EXC219044661(((AM_EXCEPT_EXPR) self));
}


BOOL AM_EXT1010597579_unbox(AM_EXT_CALL_EXPR self) {
 
 return AM_EXT1010597579(((AM_EXT_CALL_EXPR) self));
}


BOOL AM_FAR833547686_unbox(AM_FAR_EXPR self) {
 
 return AM_FAR833547686(((AM_FAR_EXPR) self));
}


BOOL AM_FLT1865244436_unbox(AM_FLTDX_CONST self) {
 
 return AM_FLT1865244436(((AM_FLTDX_CONST) self));
}


BOOL AM_FLT93541882_unbox(AM_FLTD_CONST self) {
 
 return AM_FLT93541882(((AM_FLTD_CONST) self));
}


BOOL AM_FLT840430235_unbox(AM_FLTI_CONST self) {
 
 return AM_FLT840430235(((AM_FLTI_CONST) self));
}


BOOL AM_FLT652620710_unbox(AM_FLTX_CONST self) {
 
 return AM_FLT652620710(((AM_FLTX_CONST) self));
}


BOOL AM_FLT1356295273_unbox(AM_FLT_CONST self) {
 
 return AM_FLT1356295273(((AM_FLT_CONST) self));
}


BOOL AM_GLO1300832959_unbox(AM_GLOBAL_EXPR self) {
 
 return AM_GLO1300832959(((AM_GLOBAL_EXPR) self));
}


BOOL AM_HER1213808373_unbox(AM_HERE_EXPR self) {
 
 return AM_HER1213808373(((AM_HERE_EXPR) self));
}


BOOL AM_IF_792326817_unbox(AM_IF_EXPR self) {
 
 return AM_IF_792326817(((AM_IF_EXPR) self));
}


BOOL AM_INT341842242_unbox(AM_INTI_CONST self) {
 
 return AM_INT341842242(((AM_INTI_CONST) self));
}


BOOL AM_INT1418540518_unbox(AM_INT_CONST self) {
 
 return AM_INT1418540518(((AM_INT_CONST) self));
}


BOOL AM_IS_1001428606_unbox(AM_IS_VOID_EXPR self) {
 
 return AM_IS_1001428606(((AM_IS_VOID_EXPR) self));
}


BOOL AM_ITE1429510983_unbox(AM_ITE1809135850 self) {
 
 return AM_ITE1429510983(((AM_ITE1809135850) self));
}


BOOL AM_LOC2055616863_unbox(AM_LOCAL_EXPR self) {
 
 return AM_LOC2055616863(((AM_LOCAL_EXPR) self));
}


BOOL AM_NEA1492883953_unbox(AM_NEAR_EXPR self) {
 
 return AM_NEA1492883953(((AM_NEAR_EXPR) self));
}


BOOL AM_NEW952487097_unbox(AM_NEW_EXPR self) {
 
 return AM_NEW952487097(((AM_NEW_EXPR) self));
}


BOOL AM_ROU2096608001_unbox(AM_ROU1916046290 self) {
 
 return AM_ROU2096608001(((AM_ROU1916046290) self));
}


BOOL AM_SHA1922018909_unbox(AM_SHARED_EXPR self) {
 
 return AM_SHA1922018909(((AM_SHARED_EXPR) self));
}


BOOL AM_STM552683579_unbox(AM_STMT_EXPR self) {
 
 return AM_STM552683579(((AM_STMT_EXPR) self));
}


BOOL AM_STR1095405116_unbox(AM_STR_CONST self) {
 
 return AM_STR1095405116(((AM_STR_CONST) self));
}


BOOL AM_VAR394665606_unbox(AM_VAR744470097 self) {
 
 return AM_VAR394665606(((AM_VAR744470097) self));
}


BOOL AM_VAT239070856_unbox(AM_VAT319982528 self) {
 
 return AM_VAT239070856(((AM_VAT319982528) self));
}


BOOL AM_VOI1214668736_unbox(AM_VOID_CONST self) {
 
 return AM_VOI1214668736(((AM_VOID_CONST) self));
}


BOOL AM_WHE420295477_unbox(AM_WHERE_EXPR self) {
 
 return AM_WHE420295477(((AM_WHERE_EXPR) self));
}


dAM_STMT AM_ASS1841384941_unbox(AM_ASSERT_STMT self) {
 
 return AM_ASS1841384941(((AM_ASSERT_STMT) self));
}


dAM_STMT AM_ASS1201484234_unbox(AM_ASSIGN_STMT self) {
 
 return AM_ASS1201484234(((AM_ASSIGN_STMT) self));
}


dAM_STMT AM_ATT1156941996_unbox(AM_ATTACH_STMT self) {
 
 return AM_ATT1156941996(((AM_ATTACH_STMT) self));
}


dAM_STMT AM_BRE1241758751_unbox(AM_BREAK_STMT self) {
 
 return AM_BRE1241758751(((AM_BREAK_STMT) self));
}


dAM_STMT AM_CAS1510303900_unbox(AM_CASE_STMT self) {
 
 return AM_CAS1510303900(((AM_CASE_STMT) self));
}


dAM_STMT AM_COM1124366108_unbox(AM_COMMENT_STMT self) {
 
 return AM_COM1124366108(((AM_COMMENT_STMT) self));
}


dAM_STMT AM_EXP1561605445_unbox(AM_EXPR_STMT self) {
 
 return AM_EXP1561605445(((AM_EXPR_STMT) self));
}


dAM_STMT AM_FOR1357281358_unbox(AM_FORK_STMT self) {
 
 return AM_FOR1357281358(((AM_FORK_STMT) self));
}


dAM_STMT AM_IF_1153855302_unbox(AM_IF_STMT self) {
 
 return AM_IF_1153855302(((AM_IF_STMT) self));
}


dAM_STMT AM_INI81424971_unbox(AM_INITIAL_STMT self) {
 
 return AM_INI81424971(((AM_INITIAL_STMT) self));
}


dAM_STMT AM_INV1267245156_unbox(AM_INV1167837230 self) {
 
 return AM_INV1267245156(((AM_INV1167837230) self));
}


dAM_STMT AM_LOC724705923_unbox(AM_LOCK_STMT self) {
 
 return AM_LOC724705923(((AM_LOCK_STMT) self));
}


dAM_STMT AM_LOO439140156_unbox(AM_LOOP_STMT self) {
 
 return AM_LOO439140156(((AM_LOOP_STMT) self));
}


dAM_STMT AM_PAR309007796_unbox(AM_PAR_STMT self) {
 
 return AM_PAR309007796(((AM_PAR_STMT) self));
}


dAM_STMT AM_POS1775621496_unbox(AM_POST_STMT self) {
 
 return AM_POS1775621496(((AM_POST_STMT) self));
}


dAM_STMT AM_PRE1502061133_unbox(AM_PREFETCH_STMT self) {
 
 return AM_PRE1502061133(((AM_PREFETCH_STMT) self));
}


dAM_STMT AM_PRE1762688610_unbox(AM_PRE_STMT self) {
 
 return AM_PRE1762688610(((AM_PRE_STMT) self));
}


dAM_STMT AM_PRO557659408_unbox(AM_PROTECT_STMT self) {
 
 return AM_PRO557659408(((AM_PROTECT_STMT) self));
}


dAM_STMT AM_RAI1337438778_unbox(AM_RAISE_STMT self) {
 
 return AM_RAI1337438778(((AM_RAISE_STMT) self));
}


dAM_STMT AM_RET1976133913_unbox(AM_RETURN_STMT self) {
 
 return AM_RET1976133913(((AM_RETURN_STMT) self));
}


dAM_STMT AM_SYN2020605939_unbox(AM_SYNC_STMT self) {
 
 return AM_SYN2020605939(((AM_SYNC_STMT) self));
}


dAM_STMT AM_TYP1504178732_unbox(AM_TYPECASE_STMT self) {
 
 return AM_TYP1504178732(((AM_TYPECASE_STMT) self));
}


dAM_STMT AM_UNL1996507749_unbox(AM_UNLOCK_STMT self) {
 
 return AM_UNL1996507749(((AM_UNLOCK_STMT) self));
}


dAM_STMT AM_WAI1727009663_unbox(AM_WAITFOR_STMT self) {
 
 return AM_WAI1727009663(((AM_WAITFOR_STMT) self));
}


dAM_STMT AM_WIT315547139_unbox(AM_WIT1996971603 self) {
 
 return AM_WIT315547139(((AM_WIT1996971603) self));
}


dAM_STMT AM_YIE1267845009_unbox(AM_YIELD_STMT self) {
 
 return AM_YIE1267845009(((AM_YIELD_STMT) self));
}


void AM_ASS306541468_unbox(AM_ASSERT_STMT self, dAM_STMT arg1) {
 
 AM_ASS306541468(((AM_ASSERT_STMT) self), arg1);
}


void AM_ASS946442175_unbox(AM_ASSIGN_STMT self, dAM_STMT arg1) {
 
 AM_ASS946442175(((AM_ASSIGN_STMT) self), arg1);
}


void AM_ATT990984413_unbox(AM_ATTACH_STMT self, dAM_STMT arg1) {
 
 AM_ATT990984413(((AM_ATTACH_STMT) self), arg1);
}


void AM_BRE905282136_unbox(AM_BREAK_STMT self, dAM_STMT arg1) {
 
 AM_BRE905282136(((AM_BREAK_STMT) self), arg1);
}


void AM_CAS637622509_unbox(AM_CASE_STMT self, dAM_STMT arg1) {
 
 AM_CAS637622509(((AM_CASE_STMT) self), arg1);
}


void AM_COM1022674779_unbox(AM_COMMENT_STMT self, dAM_STMT arg1) {
 
 AM_COM1022674779(((AM_COMMENT_STMT) self), arg1);
}


void AM_EXP586320964_unbox(AM_EXPR_STMT self, dAM_STMT arg1) {
 
 AM_EXP586320964(((AM_EXPR_STMT) self), arg1);
}


void AM_FOR790645051_unbox(AM_FORK_STMT self, dAM_STMT arg1) {
 
 AM_FOR790645051(((AM_FORK_STMT) self), arg1);
}


void AM_IF_993185585_unbox(AM_IF_STMT self, dAM_STMT arg1) {
 
 AM_IF_993185585(((AM_IF_STMT) self), arg1);
}


void AM_INI2065615916_unbox(AM_INITIAL_STMT self, dAM_STMT arg1) {
 
 AM_INI2065615916(((AM_INITIAL_STMT) self), arg1);
}


void AM_INV880681253_unbox(AM_INV1167837230 self, dAM_STMT arg1) {
 
 AM_INV880681253(((AM_INV1167837230) self), arg1);
}


void AM_LOC1423220486_unbox(AM_LOCK_STMT self, dAM_STMT arg1) {
 
 AM_LOC1423220486(((AM_LOCK_STMT) self), arg1);
}


void AM_LOO1708786253_unbox(AM_LOOP_STMT self, dAM_STMT arg1) {
 
 AM_LOO1708786253(((AM_LOOP_STMT) self), arg1);
}


void AM_PAR1838033091_unbox(AM_PAR_STMT self, dAM_STMT arg1) {
 
 AM_PAR1838033091(((AM_PAR_STMT) self), arg1);
}


void AM_POS372304913_unbox(AM_POST_STMT self, dAM_STMT arg1) {
 
 AM_POS372304913(((AM_POST_STMT) self), arg1);
}


void AM_PRE645865276_unbox(AM_PREFETCH_STMT self, dAM_STMT arg1) {
 
 AM_PRE645865276(((AM_PREFETCH_STMT) self), arg1);
}


void AM_PRE385237799_unbox(AM_PRE_STMT self, dAM_STMT arg1) {
 
 AM_PRE385237799(((AM_PRE_STMT) self), arg1);
}


void AM_PRO1589381479_unbox(AM_PROTECT_STMT self, dAM_STMT arg1) {
 
 AM_PRO1589381479(((AM_PROTECT_STMT) self), arg1);
}


void AM_RAI810487631_unbox(AM_RAISE_STMT self, dAM_STMT arg1) {
 
 AM_RAI810487631(((AM_RAISE_STMT) self), arg1);
}


void AM_RET170906974_unbox(AM_RETURN_STMT self, dAM_STMT arg1) {
 
 AM_RET170906974(((AM_RETURN_STMT) self), arg1);
}


void AM_SYN127320470_unbox(AM_SYNC_STMT self, dAM_STMT arg1) {
 
 AM_SYN127320470(((AM_SYNC_STMT) self), arg1);
}


void AM_TYP642862155_unbox(AM_TYPECASE_STMT self, dAM_STMT arg1) {
 
 AM_TYP642862155(((AM_TYPECASE_STMT) self), arg1);
}


void AM_UNL150533138_unbox(AM_UNLOCK_STMT self, dAM_STMT arg1) {
 
 AM_UNL150533138(((AM_UNLOCK_STMT) self), arg1);
}


void AM_WAI420031224_unbox(AM_WAITFOR_STMT self, dAM_STMT arg1) {
 
 AM_WAI420031224(((AM_WAITFOR_STMT) self), arg1);
}


void AM_WIT1831493748_unbox(AM_WIT1996971603 self, dAM_STMT arg1) {
 
 AM_WIT1831493748(((AM_WIT1996971603) self), arg1);
}


void AM_YIE879195878_unbox(AM_YIELD_STMT self, dAM_STMT arg1) {
 
 AM_YIE879195878(((AM_YIELD_STMT) self), arg1);
}


INT AM_BND1520373100_unbox(AM_BND367211769 self) {

 INT L1461_; 
L1461_=ASIZE((AM_BND367211769)((AM_BND367211769) self)); 
 return L1461_;
}


INT AM_BND1835838572_unbox(AM_BND260301329 self) {

 INT L1471_; 
L1471_=ASIZE((AM_BND260301329)((AM_BND260301329) self)); 
 return L1471_;
}


INT AM_EXT2080723118_unbox(AM_EXT_CALL_EXPR self) {

 INT L1481_; 
L1481_=ASIZE((AM_EXT_CALL_EXPR)((AM_EXT_CALL_EXPR) self)); 
 return L1481_;
}


INT AM_ITE494670743_unbox(AM_ITE1809135850 self) {

 INT L1491_; 
L1491_=ASIZE((AM_ITE1809135850)((AM_ITE1809135850) self)); 
 return L1491_;
}


INT AM_ROU1433426367_unbox(AM_ROU1916046290 self) {

 INT L1501_; 
L1501_=ASIZE((AM_ROU1916046290)((AM_ROU1916046290) self)); 
 return L1501_;
}


AM_CALL_ARG AM_BND360941210_unbox(AM_BND367211769 self, INT arg1) {

 AM_CALL_ARG L1511_; 
L1511_=(AM_CALL_ARG)ARR((AM_BND367211769)((AM_BND367211769) self),arg1); 
 return L1511_;
}


AM_CALL_ARG AM_BND1292359506_unbox(AM_BND260301329 self, INT arg1) {

 AM_CALL_ARG L1521_; 
L1521_=(AM_CALL_ARG)ARR((AM_BND260301329)((AM_BND260301329) self),arg1); 
 return L1521_;
}


AM_CALL_ARG AM_EXT48347235_unbox(AM_EXT_CALL_EXPR self, INT arg1) {

 AM_CALL_ARG L1531_; 
L1531_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)((AM_EXT_CALL_EXPR) self),arg1); 
 return L1531_;
}


AM_CALL_ARG AM_ITE329074641_unbox(AM_ITE1809135850 self, INT arg1) {

 AM_CALL_ARG L1541_; 
L1541_=(AM_CALL_ARG)ARR((AM_ITE1809135850)((AM_ITE1809135850) self),arg1); 
 return L1541_;
}


AM_CALL_ARG AM_ROU602343655_unbox(AM_ROU1916046290 self, INT arg1) {

 AM_CALL_ARG L1551_; 
L1551_=(AM_CALL_ARG)ARR((AM_ROU1916046290)((AM_ROU1916046290) self),arg1); 
 return L1551_;
}


void NULL_S1721445066_unbox(NULL_STAT self, STR arg1) {
 
 NULL_S1721445066(((NULL_STAT) self), arg1);
}


void STAT_incr_STR_unbox(STAT1 self, STR arg1) {
 
 STAT_incr_STR(((STAT1) self), arg1);
}


dAM_STMT AM_ASS1481858691_unbox(AM_ASSERT_STMT self) {
 
 return ((dAM_STMT) AM_ASS1481858691(((AM_ASSERT_STMT) self)));
}


dAM_STMT AM_ASS782632663_unbox(AM_ASSIGN_STMT self) {
 
 return ((dAM_STMT) AM_ASS782632663(((AM_ASSIGN_STMT) self)));
}


dAM_STMT AM_ATT191404837_unbox(AM_ATTACH_STMT self) {
 
 return ((dAM_STMT) AM_ATT191404837(((AM_ATTACH_STMT) self)));
}


dAM_STMT AM_BRE1937656818_unbox(AM_BREAK_STMT self) {
 
 return ((dAM_STMT) AM_BRE1937656818(((AM_BREAK_STMT) self)));
}


dAM_STMT AM_CAS1898951205_unbox(AM_CASE_STMT self) {
 
 return ((dAM_STMT) AM_CAS1898951205(((AM_CASE_STMT) self)));
}


dAM_STMT AM_COM977130404_unbox(AM_COMMENT_STMT self) {
 
 return ((dAM_STMT) AM_COM977130404(((AM_COMMENT_STMT) self)));
}


dAM_STMT AM_EXP359209435_unbox(AM_EXPR_STMT self) {
 
 return ((dAM_STMT) AM_EXP359209435(((AM_EXPR_STMT) self)));
}


dAM_STMT AM_FOR214044719_unbox(AM_FORK_STMT self) {
 
 return ((dAM_STMT) AM_FOR214044719(((AM_FORK_STMT) self)));
}


dAM_STMT AM_IF_1702280121_unbox(AM_IF_STMT self) {
 
 return ((dAM_STMT) AM_IF_1702280121(((AM_IF_STMT) self)));
}


dAM_STMT AM_INI1487747776_unbox(AM_INITIAL_STMT self) {
 
 return ((dAM_STMT) AM_INI1487747776(((AM_INITIAL_STMT) self)));
}


dAM_STMT AM_INV1353049642_unbox(AM_INV1167837230 self) {
 
 return ((dAM_STMT) AM_INV1353049642(((AM_INV1167837230) self)));
}


dAM_STMT AM_LOC1474178511_unbox(AM_LOCK_STMT self) {
 
 return ((dAM_STMT) AM_LOC1474178511(((AM_LOCK_STMT) self)));
}


dAM_STMT AM_LOO1572875803_unbox(AM_LOOP_STMT self) {
 
 return ((dAM_STMT) AM_LOO1572875803(((AM_LOOP_STMT) self)));
}


dAM_STMT AM_PAR784084248_unbox(AM_PAR_STMT self) {
 
 return ((dAM_STMT) AM_PAR784084248(((AM_PAR_STMT) self)));
}


dAM_STMT AM_POS1078136755_unbox(AM_POST_STMT self) {
 
 return ((dAM_STMT) AM_POS1078136755(((AM_POST_STMT) self)));
}


dAM_STMT AM_PRE896696117_unbox(AM_PREFETCH_STMT self) {
 
 return ((dAM_STMT) AM_PRE896696117(((AM_PREFETCH_STMT) self)));
}


dAM_STMT AM_PRE202271760_unbox(AM_PRE_STMT self) {
 
 return ((dAM_STMT) AM_PRE202271760(((AM_PRE_STMT) self)));
}


dAM_STMT AM_PRO1363632140_unbox(AM_PROTECT_STMT self) {
 
 return ((dAM_STMT) AM_PRO1363632140(((AM_PROTECT_STMT) self)));
}


dAM_STMT AM_RAI1825238682_unbox(AM_RAISE_STMT self) {
 
 return ((dAM_STMT) AM_RAI1825238682(((AM_RAISE_STMT) self)));
}


dAM_STMT AM_RET1651971761_unbox(AM_RETURN_STMT self) {
 
 return ((dAM_STMT) AM_RET1651971761(((AM_RETURN_STMT) self)));
}


dAM_STMT AM_SYN1962040721_unbox(AM_SYNC_STMT self) {
 
 return ((dAM_STMT) AM_SYN1962040721(((AM_SYNC_STMT) self)));
}


dAM_STMT AM_TYP1756924987_unbox(AM_TYPECASE_STMT self) {
 
 return ((dAM_STMT) AM_TYP1756924987(((AM_TYPECASE_STMT) self)));
}


dAM_STMT AM_UNL460993177_unbox(AM_UNLOCK_STMT self) {
 
 return ((dAM_STMT) AM_UNL460993177(((AM_UNLOCK_STMT) self)));
}


dAM_STMT AM_WAI1946639376_unbox(AM_WAITFOR_STMT self) {
 
 return ((dAM_STMT) AM_WAI1946639376(((AM_WAITFOR_STMT) self)));
}


dAM_STMT AM_WIT1785216238_unbox(AM_WIT1996971603 self) {
 
 return ((dAM_STMT) AM_WIT1785216238(((AM_WIT1996971603) self)));
}


dAM_STMT AM_YIE197772790_unbox(AM_YIELD_STMT self) {
 
 return ((dAM_STMT) AM_YIE197772790(((AM_YIELD_STMT) self)));
}


dAM_EXPR AM_ANY934244480_unbox(AM_ANY_EXPR self) {
 
 return ((dAM_EXPR) AM_ANY934244480(((AM_ANY_EXPR) self)));
}


dAM_EXPR AM_ARR1852652918_unbox(AM_ARRAY_EXPR self) {
 
 return ((dAM_EXPR) AM_ARR1852652918(((AM_ARRAY_EXPR) self)));
}


dAM_EXPR AM_ARR990477897_unbox(AM_ARR_CONST self) {
 
 return ((dAM_EXPR) AM_ARR990477897(((AM_ARR_CONST) self)));
}


dAM_EXPR AM_ATT1705760219_unbox(AM_ATTR_EXPR self) {
 
 return ((dAM_EXPR) AM_ATT1705760219(((AM_ATTR_EXPR) self)));
}


dAM_EXPR AM_AT_1135400775_unbox(AM_AT_EXPR self) {
 
 return ((dAM_EXPR) AM_AT_1135400775(((AM_AT_EXPR) self)));
}


dAM_EXPR AM_BND831738871_unbox(AM_BND1124877163 self) {
 
 return ((dAM_EXPR) AM_BND831738871(((AM_BND1124877163) self)));
}


dAM_EXPR AM_BND1416432078_unbox(AM_BND367211769 self) {
 
 return ((dAM_EXPR) AM_BND1416432078(((AM_BND367211769) self)));
}


dAM_EXPR AM_BND2052779026_unbox(AM_BND260301329 self) {
 
 return ((dAM_EXPR) AM_BND2052779026(((AM_BND260301329) self)));
}


dAM_EXPR AM_BOO158864518_unbox(AM_BOOL_CONST self) {
 
 return ((dAM_EXPR) AM_BOO158864518(((AM_BOOL_CONST) self)));
}


dAM_EXPR AM_CHA916745766_unbox(AM_CHAR_CONST self) {
 
 return ((dAM_EXPR) AM_CHA916745766(((AM_CHAR_CONST) self)));
}


dAM_EXPR AM_CLU1577744436_unbox(AM_CLUSTER_EXPR self) {
 
 return ((dAM_EXPR) AM_CLU1577744436(((AM_CLUSTER_EXPR) self)));
}


dAM_EXPR AM_CLU779855221_unbox(AM_CLU1286269335 self) {
 
 return ((dAM_EXPR) AM_CLU779855221(((AM_CLU1286269335) self)));
}


dAM_EXPR AM_CON582703191_unbox(AM_CONST self) {
 
 return ((dAM_EXPR) AM_CON582703191(((AM_CONST) self)));
}


dAM_EXPR AM_EXC449573271_unbox(AM_EXCEPT_EXPR self) {
 
 return ((dAM_EXPR) AM_EXC449573271(((AM_EXCEPT_EXPR) self)));
}


dAM_EXPR AM_EXT1687283405_unbox(AM_EXT_CALL_EXPR self) {
 
 return ((dAM_EXPR) AM_EXT1687283405(((AM_EXT_CALL_EXPR) self)));
}


dAM_EXPR AM_FAR755578228_unbox(AM_FAR_EXPR self) {
 
 return ((dAM_EXPR) AM_FAR755578228(((AM_FAR_EXPR) self)));
}


dAM_EXPR AM_FLT822990115_unbox(AM_FLTDX_CONST self) {
 
 return ((dAM_EXPR) AM_FLT822990115(((AM_FLTDX_CONST) self)));
}


dAM_EXPR AM_FLT1839670182_unbox(AM_FLTD_CONST self) {
 
 return ((dAM_EXPR) AM_FLT1839670182(((AM_FLTD_CONST) self)));
}


dAM_EXPR AM_FLT425117946_unbox(AM_FLTI_CONST self) {
 
 return ((dAM_EXPR) AM_FLT425117946(((AM_FLTI_CONST) self)));
}


dAM_EXPR AM_FLT476428534_unbox(AM_FLTX_CONST self) {
 
 return ((dAM_EXPR) AM_FLT476428534(((AM_FLTX_CONST) self)));
}


dAM_EXPR AM_FLT1362803603_unbox(AM_FLT_CONST self) {
 
 return ((dAM_EXPR) AM_FLT1362803603(((AM_FLT_CONST) self)));
}


dAM_EXPR AM_GLO928972965_unbox(AM_GLOBAL_EXPR self) {
 
 return ((dAM_EXPR) AM_GLO928972965(((AM_GLOBAL_EXPR) self)));
}


dAM_EXPR AM_HER1301311031_unbox(AM_HERE_EXPR self) {
 
 return ((dAM_EXPR) AM_HER1301311031(((AM_HERE_EXPR) self)));
}


dAM_EXPR AM_IF_1266575443_unbox(AM_IF_EXPR self) {
 
 return ((dAM_EXPR) AM_IF_1266575443(((AM_IF_EXPR) self)));
}


dAM_EXPR AM_INT304374138_unbox(AM_INTI_CONST self) {
 
 return ((dAM_EXPR) AM_INT304374138(((AM_INTI_CONST) self)));
}


dAM_EXPR AM_INT1629629861_unbox(AM_INT_CONST self) {
 
 return ((dAM_EXPR) AM_INT1629629861(((AM_INT_CONST) self)));
}


dAM_EXPR AM_IS_193101840_unbox(AM_IS_VOID_EXPR self) {
 
 return ((dAM_EXPR) AM_IS_193101840(((AM_IS_VOID_EXPR) self)));
}


dAM_EXPR AM_ITE380500422_unbox(AM_ITE1809135850 self) {
 
 return ((dAM_EXPR) AM_ITE380500422(((AM_ITE1809135850) self)));
}


dAM_EXPR AM_LOC1798202130_unbox(AM_LOCAL_EXPR self) {
 
 return ((dAM_EXPR) AM_LOC1798202130(((AM_LOCAL_EXPR) self)));
}


dAM_EXPR AM_NEA1086037759_unbox(AM_NEAR_EXPR self) {
 
 return ((dAM_EXPR) AM_NEA1086037759(((AM_NEAR_EXPR) self)));
}


dAM_EXPR AM_NEW1250698656_unbox(AM_NEW_EXPR self) {
 
 return ((dAM_EXPR) AM_NEW1250698656(((AM_NEW_EXPR) self)));
}


dAM_EXPR AM_ROU960965786_unbox(AM_ROU1916046290 self) {
 
 return ((dAM_EXPR) AM_ROU960965786(((AM_ROU1916046290) self)));
}


dAM_EXPR AM_SHA1035965785_unbox(AM_SHARED_EXPR self) {
 
 return ((dAM_EXPR) AM_SHA1035965785(((AM_SHARED_EXPR) self)));
}


dAM_EXPR AM_STM416210327_unbox(AM_STMT_EXPR self) {
 
 return ((dAM_EXPR) AM_STM416210327(((AM_STMT_EXPR) self)));
}


dAM_EXPR AM_STR1745454041_unbox(AM_STR_CONST self) {
 
 return ((dAM_EXPR) AM_STR1745454041(((AM_STR_CONST) self)));
}


dAM_EXPR AM_VAR1906690564_unbox(AM_VAR744470097 self) {
 
 return ((dAM_EXPR) AM_VAR1906690564(((AM_VAR744470097) self)));
}


dAM_EXPR AM_VAT2137026865_unbox(AM_VAT319982528 self) {
 
 return ((dAM_EXPR) AM_VAT2137026865(((AM_VAT319982528) self)));
}


dAM_EXPR AM_VOI1734446450_unbox(AM_VOID_CONST self) {
 
 return ((dAM_EXPR) AM_VOI1734446450(((AM_VOID_CONST) self)));
}


dAM_EXPR AM_WHE1264983710_unbox(AM_WHERE_EXPR self) {
 
 return ((dAM_EXPR) AM_WHE1264983710(((AM_WHERE_EXPR) self)));
}


void AM_ASS356960882_unbox(AM_ASSERT_STMT self, dAM_STMT arg1) {
 
 AM_ASS356960882(((AM_ASSERT_STMT) self), arg1);
}


void AM_ASS1281039639_unbox(AM_ASSIGN_STMT self, dAM_STMT arg1) {
 
 AM_ASS1281039639(((AM_ASSIGN_STMT) self), arg1);
}


void AM_ATT1913806391_unbox(AM_ATTACH_STMT self, dAM_STMT arg1) {
 
 AM_ATT1913806391(((AM_ATTACH_STMT) self), arg1);
}


void AM_BRE261382646_unbox(AM_BREAK_STMT self, dAM_STMT arg1) {
 
 AM_BRE261382646(((AM_BREAK_STMT) self), arg1);
}


void AM_CAS1095788609_unbox(AM_CASE_STMT self, dAM_STMT arg1) {
 
 AM_CAS1095788609(((AM_CASE_STMT) self), arg1);
}


void AM_COM832310723_unbox(AM_COMMENT_STMT self, dAM_STMT arg1) {
 
 AM_COM832310723(((AM_COMMENT_STMT) self), arg1);
}


void AM_EXP1859190126_unbox(AM_EXPR_STMT self, dAM_STMT arg1) {
 
 AM_EXP1859190126(((AM_EXPR_STMT) self), arg1);
}


void AM_FOR1689225795_unbox(AM_FORK_STMT self, dAM_STMT arg1) {
 
 AM_FOR1689225795(((AM_FORK_STMT) self), arg1);
}


void AM_IF_1406990231_unbox(AM_IF_STMT self, dAM_STMT arg1) {
 
 AM_IF_1406990231(((AM_IF_STMT) self), arg1);
}


void AM_INI2007284982_unbox(AM_INITIAL_STMT self, dAM_STMT arg1) {
 
 AM_INI2007284982(((AM_INITIAL_STMT) self), arg1);
}


void AM_INV749070155_unbox(AM_INV1167837230 self, dAM_STMT arg1) {
 
 AM_INV749070155(((AM_INV1167837230) self), arg1);
}


void AM_LOC962760448_unbox(AM_LOCK_STMT self, dAM_STMT arg1) {
 
 AM_LOC962760448(((AM_LOCK_STMT) self), arg1);
}


void AM_LOO952733665_unbox(AM_LOOP_STMT self, dAM_STMT arg1) {
 
 AM_LOO952733665(((AM_LOOP_STMT) self), arg1);
}


void AM_PAR1454262261_unbox(AM_PAR_STMT self, dAM_STMT arg1) {
 
 AM_PAR1454262261(((AM_PAR_STMT) self), arg1);
}


void AM_POS1908807043_unbox(AM_POST_STMT self, dAM_STMT arg1) {
 
 AM_POS1908807043(((AM_POST_STMT) self), arg1);
}


void AM_PRE1879848758_unbox(AM_PREFETCH_STMT self, dAM_STMT arg1) {
 
 AM_PRE1879848758(((AM_PREFETCH_STMT) self), arg1);
}


void AM_PRE2012355323_unbox(AM_PRE_STMT self, dAM_STMT arg1) {
 
 AM_PRE2012355323(((AM_PRE_STMT) self), arg1);
}


void AM_PRO1885345065_unbox(AM_PROTECT_STMT self, dAM_STMT arg1) {
 
 AM_PRO1885345065(((AM_PROTECT_STMT) self), arg1);
}


void AM_RAI1178977909_unbox(AM_RAISE_STMT self, dAM_STMT arg1) {
 
 AM_RAI1178977909(((AM_RAISE_STMT) self), arg1);
}


void AM_RET917156604_unbox(AM_RETURN_STMT self, dAM_STMT arg1) {
 
 AM_RET917156604(((AM_RETURN_STMT) self), arg1);
}


void AM_SYN631230960_unbox(AM_SYNC_STMT self, dAM_STMT arg1) {
 
 AM_SYN631230960(((AM_SYNC_STMT) self), arg1);
}


void AM_TYP505714135_unbox(AM_TYPECASE_STMT self, dAM_STMT arg1) {
 
 AM_TYP505714135(((AM_TYPECASE_STMT) self), arg1);
}


void AM_UNL2142830896_unbox(AM_UNLOCK_STMT self, dAM_STMT arg1) {
 
 AM_UNL2142830896(((AM_UNLOCK_STMT) self), arg1);
}


void AM_WAI656698018_unbox(AM_WAITFOR_STMT self, dAM_STMT arg1) {
 
 AM_WAI656698018(((AM_WAITFOR_STMT) self), arg1);
}


void AM_WIT906438282_unbox(AM_WIT1996971603 self, dAM_STMT arg1) {
 
 AM_WIT906438282(((AM_WIT1996971603) self), arg1);
}


void AM_YIE1088587192_unbox(AM_YIELD_STMT self, dAM_STMT arg1) {
 
 AM_YIE1088587192(((AM_YIELD_STMT) self), arg1);
}


BOOL INLINE1676073295_unbox(INLINE_ATTR_READ self) {
 
 return INLINE1676073295(((INLINE_ATTR_READ) self));
}


BOOL INLINE182457744_unbox(INLINE1596311811 self) {
 
 return INLINE182457744(((INLINE1596311811) self));
}


BOOL INLINE600822962_unbox(INLINE1433668901 self) {
 
 return INLINE600822962(((INLINE1433668901) self));
}


BOOL INLINE862622371_unbox(INLINE1455768374 self) {
 
 return INLINE862622371(((INLINE1455768374) self));
}


BOOL INLINE1799378917_unbox(INLINE_INT_FOLD self) {
 
 return INLINE1799378917(((INLINE_INT_FOLD) self));
}


BOOL INLINE1554137872_unbox(INLINE_ROUT self) {
 
 return INLINE1554137872(((INLINE_ROUT) self));
}


BOOL INLINE1738748633_unbox(INLINE1164904870 self) {
 
 return INLINE1738748633(((INLINE1164904870) self));
}


dAM_EXPR INLINE46712374_unbox(INLINE_ATTR_READ self, AM_ROU1916046290 arg1) {
 
 return INLINE46712374(((INLINE_ATTR_READ) self), arg1);
}


dAM_EXPR INLINE655229257_unbox(INLINE1596311811 self, AM_ROU1916046290 arg1) {
 
 return INLINE655229257(((INLINE1596311811) self), arg1);
}


dAM_EXPR INLINE1475043475_unbox(INLINE1433668901 self, AM_ROU1916046290 arg1) {
 
 return INLINE1475043475(((INLINE1433668901) self), arg1);
}


dAM_EXPR INLINE2023716438_unbox(INLINE1455768374 self, AM_ROU1916046290 arg1) {
 
 return INLINE2023716438(((INLINE1455768374) self), arg1);
}


dAM_EXPR INLINE1324370258_unbox(INLINE_INT_FOLD self, AM_ROU1916046290 arg1) {
 
 return INLINE1324370258(((INLINE_INT_FOLD) self), arg1);
}


dAM_EXPR INLINE1949170047_unbox(INLINE_ROUT self, AM_ROU1916046290 arg1) {
 
 return INLINE1949170047(((INLINE_ROUT) self), arg1);
}


dAM_EXPR INLINE1736265130_unbox(INLINE1164904870 self, AM_ROU1916046290 arg1) {
 
 return INLINE1736265130(((INLINE1164904870) self), arg1);
}


dAM_EXPR INLINE1797911459_unbox(INLINE self, AM_ROUT_DEF arg1, AM_ROU1916046290 arg2) {
 
 return INLINE1797911459(((INLINE) self), arg1, arg2);
}


void GENERA88776523_unbox(GENERATE_AM self, SIG arg1) {
 
 GENERA88776523(((GENERATE_AM) self), arg1);
}


SIG INLINE1199394416_unbox(INLINE_ITER self) {
 
 return INLINE1199394416(((INLINE_ITER) self));
}


BOOL INLINE182665064_unbox(INLINE_ITER self) {
 
 return INLINE182665064(((INLINE_ITER) self));
}


dAM_EXPR INLINE986164897_unbox(INLINE_ITER self, AM_ITE1809135850 arg1) {
 
 return INLINE986164897(((INLINE_ITER) self), arg1);
}


dAM_EXPR INLINE1060127472_unbox(INLINE self, AM_ROUT_DEF arg1, AM_LOOP_STMT arg2, AM_ITE1809135850 arg3) {
 
 return INLINE1060127472(((INLINE) self), arg1, arg2, arg3);
}


FLISTA725283029 AM_ANY1678367917_unbox(AM_ANY_EXPR self) {
 
 return AM_ANY1678367917(((AM_ANY_EXPR) self));
}


FLISTA725283029 AM_ARR1917250979_unbox(AM_ARRAY_EXPR self) {
 
 return AM_ARR1917250979(((AM_ARRAY_EXPR) self));
}


FLISTA725283029 AM_ARR1270029884_unbox(AM_ARR_CONST self) {
 
 return AM_ARR1270029884(((AM_ARR_CONST) self));
}


FLISTA725283029 AM_ATT2085324530_unbox(AM_ATTR_EXPR self) {
 
 return AM_ATT2085324530(((AM_ATTR_EXPR) self));
}


FLISTA725283029 AM_AT_726422231_unbox(AM_AT_EXPR self) {
 
 return AM_AT_726422231(((AM_AT_EXPR) self));
}


FLISTA725283029 AM_BND1086659687_unbox(AM_BND1124877163 self) {
 
 return AM_BND1086659687(((AM_BND1124877163) self));
}


FLISTA725283029 AM_BND776534650_unbox(AM_BND367211769 self) {
 
 return AM_BND776534650(((AM_BND367211769) self));
}


FLISTA725283029 AM_BND1015009410_unbox(AM_BND260301329 self) {
 
 return AM_BND1015009410(((AM_BND260301329) self));
}


FLISTA725283029 AM_BOO1622702250_unbox(AM_BOOL_CONST self) {
 
 return AM_BOO1622702250(((AM_BOOL_CONST) self));
}


FLISTA725283029 AM_CHA722016210_unbox(AM_CHAR_CONST self) {
 
 return AM_CHA722016210(((AM_CHAR_CONST) self));
}


FLISTA725283029 AM_CLU1812304013_unbox(AM_CLUSTER_EXPR self) {
 
 return AM_CLU1812304013(((AM_CLUSTER_EXPR) self));
}


FLISTA725283029 AM_CLU39726141_unbox(AM_CLU1286269335 self) {
 
 return AM_CLU39726141(((AM_CLU1286269335) self));
}


FLISTA725283029 AM_CON19269408_unbox(AM_CONST self) {
 
 return AM_CON19269408(((AM_CONST) self));
}


FLISTA725283029 AM_EXC1481209187_unbox(AM_EXCEPT_EXPR self) {
 
 return AM_EXC1481209187(((AM_EXCEPT_EXPR) self));
}


FLISTA725283029 AM_EXT908131195_unbox(AM_EXT_CALL_EXPR self) {
 
 return AM_EXT908131195(((AM_EXT_CALL_EXPR) self));
}


FLISTA725283029 AM_FAR276305130_unbox(AM_FAR_EXPR self) {
 
 return AM_FAR276305130(((AM_FAR_EXPR) self));
}


FLISTA725283029 AM_FLT1832811964_unbox(AM_FLTDX_CONST self) {
 
 return AM_FLT1832811964(((AM_FLTDX_CONST) self));
}


FLISTA725283029 AM_FLT1309796914_unbox(AM_FLTD_CONST self) {
 
 return AM_FLT1309796914(((AM_FLTD_CONST) self));
}


FLISTA725283029 AM_FLT523420577_unbox(AM_FLTI_CONST self) {
 
 return AM_FLT523420577(((AM_FLTI_CONST) self));
}


FLISTA725283029 AM_FLT1728105754_unbox(AM_FLTX_CONST self) {
 
 return AM_FLT1728105754(((AM_FLTX_CONST) self));
}


FLISTA725283029 AM_FLT945201335_unbox(AM_FLT_CONST self) {
 
 return AM_FLT945201335(((AM_FLT_CONST) self));
}


FLISTA725283029 AM_GLO404411817_unbox(AM_GLOBAL_EXPR self) {
 
 return AM_GLO404411817(((AM_GLOBAL_EXPR) self));
}


FLISTA725283029 AM_HER1483923803_unbox(AM_HERE_EXPR self) {
 
 return AM_HER1483923803(((AM_HERE_EXPR) self));
}


FLISTA725283029 AM_IF_1267236903_unbox(AM_IF_EXPR self) {
 
 return AM_IF_1267236903(((AM_IF_EXPR) self));
}


FLISTA725283029 AM_INT2023229226_unbox(AM_INTI_CONST self) {
 
 return AM_INT2023229226(((AM_INTI_CONST) self));
}


FLISTA725283029 AM_INT1223291058_unbox(AM_INT_CONST self) {
 
 return AM_INT1223291058(((AM_INT_CONST) self));
}


FLISTA725283029 AM_IS_1111682258_unbox(AM_IS_VOID_EXPR self) {
 
 return AM_IS_1111682258(((AM_IS_VOID_EXPR) self));
}


FLISTA725283029 AM_ITE757218595_unbox(AM_ITE1809135850 self) {
 
 return AM_ITE757218595(((AM_ITE1809135850) self));
}


FLISTA725283029 AM_LOC1121251403_unbox(AM_LOCAL_EXPR self) {
 
 return AM_LOC1121251403(((AM_LOCAL_EXPR) self));
}


FLISTA725283029 AM_NEA1654213825_unbox(AM_NEAR_EXPR self) {
 
 return AM_NEA1654213825(((AM_NEAR_EXPR) self));
}


FLISTA725283029 AM_NEW1679302875_unbox(AM_NEW_EXPR self) {
 
 return AM_NEW1679302875(((AM_NEW_EXPR) self));
}


FLISTA725283029 AM_ROU518743835_unbox(AM_ROU1916046290 self) {
 
 return AM_ROU518743835(((AM_ROU1916046290) self));
}


FLISTA725283029 AM_SHA1496045189_unbox(AM_SHARED_EXPR self) {
 
 return AM_SHA1496045189(((AM_SHARED_EXPR) self));
}


FLISTA725283029 AM_STM1543836917_unbox(AM_STMT_EXPR self) {
 
 return AM_STM1543836917(((AM_STMT_EXPR) self));
}


FLISTA725283029 AM_STR201189492_unbox(AM_STR_CONST self) {
 
 return AM_STR201189492(((AM_STR_CONST) self));
}


FLISTA725283029 AM_VAR567579326_unbox(AM_VAR744470097 self) {
 
 return AM_VAR567579326(((AM_VAR744470097) self));
}


FLISTA725283029 AM_VAT467614272_unbox(AM_VAT319982528 self) {
 
 return AM_VAT467614272(((AM_VAT319982528) self));
}


FLISTA725283029 AM_VOI402497764_unbox(AM_VOID_CONST self) {
 
 return AM_VOI402497764(((AM_VOID_CONST) self));
}


FLISTA725283029 AM_WHE1419632471_unbox(AM_WHERE_EXPR self) {
 
 return AM_WHE1419632471(((AM_WHERE_EXPR) self));
}


BOOL TP_CLA1153639400_unbox(TP_CLASS self) {
 
 return TP_CLA1153639400(((TP_CLASS) self));
}


BOOL TP_ITE1549466955_unbox(TP_ITER self) {
 
 return TP_ITE1549466955(((TP_ITER) self));
}


BOOL TP_ROU177994147_unbox(TP_ROUT self) {
 
 return TP_ROU177994147(((TP_ROUT) self));
}


BOOL TP_CLA366982046_unbox(TP_CLASS self) {
 
 return TP_CLA366982046(((TP_CLASS) self));
}


BOOL TP_ITE547568872_unbox(TP_ITER self) {
 
 return TP_ITE547568872(((TP_ITER) self));
}


BOOL TP_ROU835748448_unbox(TP_ROUT self) {
 
 return TP_ROU835748448(((TP_ROUT) self));
}


IMPL TP_CLA1109727233_unbox(TP_CLASS self) {
 
 return TP_CLA1109727233(((TP_CLASS) self));
}


IMPL TP_ITE830569150_unbox(TP_ITER self) {
 
 return TP_ITE830569150(((TP_ITER) self));
}


IMPL TP_ROU723658710_unbox(TP_ROUT self) {
 
 return TP_ROU723658710(((TP_ROUT) self));
}


FLISTdLAYOUT ABSTRA1904387652_unbox(ABSTRA151498155 self) {
 
 return ABSTRA1904387652(((ABSTRA151498155) self));
}


FLISTdLAYOUT ABSTRA1868518641_unbox(ABSTRACT_LAYOUT self) {
 
 return ABSTRA1868518641(((ABSTRACT_LAYOUT) self));
}


FLISTdLAYOUT ARG_LA836569915_unbox(ARG_LAYOUT self) {
 
 return ARG_LA836569915(((ARG_LAYOUT) self));
}


FLISTdLAYOUT BOUND_1150883246_unbox(BOUND_1007407331 self) {
 
 return BOUND_1150883246(((BOUND_1007407331) self));
}


FLISTdLAYOUT BOUND_1157561903_unbox(BOUND_2140373124 self) {
 
 return BOUND_1157561903(((BOUND_2140373124) self));
}


FLISTdLAYOUT BOUND_1585387107_unbox(BOUND_1677815802 self) {
 
 return BOUND_1585387107(((BOUND_1677815802) self));
}


FLISTdLAYOUT BOUND_220914967_unbox(BOUND_809390774 self) {
 
 return BOUND_220914967(((BOUND_809390774) self));
}


FLISTdLAYOUT BUILTI1794609038_unbox(BUILTIN_LAYOUT self) {
 
 return BUILTI1794609038(((BUILTIN_LAYOUT) self));
}


FLISTdLAYOUT CLASS_1345471732_unbox(CLASS_LAYOUT self) {
 
 return CLASS_1345471732(((CLASS_LAYOUT) self));
}


FLISTdLAYOUT EXTERN1033411318_unbox(EXTERNAL_LAYOUT self) {
 
 return EXTERN1033411318(((EXTERNAL_LAYOUT) self));
}


FLISTdLAYOUT FRAME_31536271_unbox(FRAME_LAYOUT self) {
 
 return FRAME_31536271(((FRAME_LAYOUT) self));
}


FLISTdLAYOUT IMMUTA414538126_unbox(IMMUTA421759893 self) {
 
 return IMMUTA414538126(((IMMUTA421759893) self));
}


FLISTdLAYOUT TP_LAY265125546_unbox(TP_LAYOUT self) {
 
 return TP_LAY265125546(((TP_LAYOUT) self));
}


BOOL INLINE76438420_unbox(INLINE self, SIG arg1) {
 
 return INLINE76438420(((INLINE) self), arg1);
}


FSETSIG GENERA1281179458_unbox(GENERATE_AM self) {
 
 return GENERA1281179458(((GENERATE_AM) self));
}


BOOL TP_CLA1448883828_unbox(TP_CLASS self) {
 
 return TP_CLA1448883828(((TP_CLASS) self));
}


BOOL TP_ITE1606424066_unbox(TP_ITER self) {
 
 return TP_ITE1606424066(((TP_ITER) self));
}


BOOL TP_ROU1919694918_unbox(TP_ROUT self) {
 
 return TP_ROU1919694918(((TP_ROUT) self));
}


BOOL TP_CLA430625769_unbox(TP_CLASS self) {
 
 return TP_CLA430625769(((TP_CLASS) self));
}


BOOL TP_ITE2096764295_unbox(TP_ITER self) {
 
 return TP_ITE2096764295(((TP_ITER) self));
}


BOOL TP_ROU1673918401_unbox(TP_ROUT self) {
 
 return TP_ROU1673918401(((TP_ROUT) self));
}


BOOL INOUT_1534195007_unbox(INOUT_MODE self) {
 
 return INOUT_1534195007(((INOUT_MODE) self));
}


BOOL IN_MOD851634165_unbox(IN_MODE self) {
 
 return IN_MOD851634165(((IN_MODE) self));
}


BOOL ONCE_M1884799084_unbox(ONCE_MODE self) {
 
 return ONCE_M1884799084(((ONCE_MODE) self));
}


BOOL OUT_MO898624407_unbox(OUT_MODE self) {
 
 return OUT_MO898624407(((OUT_MODE) self));
}


STR CALL_T152540990_unbox(CALL_TP_ARRAY self) {
 
 return CALL_T152540990(((CALL_TP_ARRAY) self));
}


STR CALL_T1666519150_unbox(CALL_T561792367 self) {
 
 return CALL_T1666519150(((CALL_T561792367) self));
}


STR CALL_T1577548690_unbox(CALL_TP_CREATE self) {
 
 return CALL_T1577548690(((CALL_TP_CREATE) self));
}


STR CALL_T47412966_unbox(CALL_T666389079 self) {
 
 return CALL_T47412966(((CALL_T666389079) self));
}


STR CALL_T184446387_unbox(CALL_TP_VOID self) {
 
 return CALL_T184446387(((CALL_TP_VOID) self));
}


BOOL TP_CLA693899249_unbox(TP_CLASS self, dTP arg1) {
 
 return TP_CLA693899249(((TP_CLASS) self), ((dCALL_TP) arg1));
}


BOOL TP_ITE1906602522_unbox(TP_ITER self, dTP arg1) {
 
 return TP_ITE1906602522(((TP_ITER) self), ((dCALL_TP) arg1));
}


BOOL TP_ROU967846898_unbox(TP_ROUT self, dTP arg1) {
 
 return TP_ROU967846898(((TP_ROUT) self), ((dCALL_TP) arg1));
}


void CGEN_e103713585_unbox(CGEN self, AM_ROUT_DEF arg1) {
 
 CGEN_e103713585(((CGEN) self), arg1);
}


void OPTIMI462748081_unbox(OPTIMIZE self, AM_ROUT_DEF arg1) {
 
 OPTIMI462748081(((OPTIMIZE) self), arg1);
}


BOOL TP_CLA1792166754_unbox(TP_CLASS self) {
 
 return TP_CLA1792166754(((TP_CLASS) self));
}


BOOL TP_ITE910939601_unbox(TP_ITER self) {
 
 return TP_ITE910939601(((TP_ITER) self));
}


BOOL TP_ROU460533207_unbox(TP_ROUT self) {
 
 return TP_ROU460533207(((TP_ROUT) self));
}


INT AS_AND1958569903_unbox(AS_AND_EXPR self) {
 
 return AS_AND1958569903(((AS_AND_EXPR) self));
}


INT AS_ANY1507835814_unbox(AS_ANY_EXPR self) {
 
 return AS_ANY1507835814(((AS_ANY_EXPR) self));
}


INT AS_ARR1547513756_unbox(AS_ARRAY_EXPR self) {
 
 return AS_ARR1547513756(((AS_ARRAY_EXPR) self));
}


INT AS_AT_355764446_unbox(AS_AT_EXPR self) {
 
 return AS_AT_355764446(((AS_AT_EXPR) self));
}


INT AS_BOO711205340_unbox(AS_BOOL_LIT_EXPR self) {
 
 return AS_BOO711205340(((AS_BOOL_LIT_EXPR) self));
}


INT AS_BOU1634505419_unbox(AS_BOU14269336 self) {
 
 return AS_BOU1634505419(((AS_BOU14269336) self));
}


INT AS_BRE836708546_unbox(AS_BREAK_EXPR self) {
 
 return AS_BRE836708546(((AS_BREAK_EXPR) self));
}


INT AS_CAL755875642_unbox(AS_CALL_EXPR self) {
 
 return AS_CAL755875642(((AS_CALL_EXPR) self));
}


INT AS_CHA1908653268_unbox(AS_CHAR_LIT_EXPR self) {
 
 return AS_CHA1908653268(((AS_CHAR_LIT_EXPR) self));
}


INT AS_CLU1234120202_unbox(AS_CLUSTER_EXPR self) {
 
 return AS_CLU1234120202(((AS_CLUSTER_EXPR) self));
}


INT AS_CLU889538930_unbox(AS_CLU553570599 self) {
 
 return AS_CLU889538930(((AS_CLU553570599) self));
}


INT AS_COH1949131218_unbox(AS_COHORT_EXPR self) {
 
 return AS_COH1949131218(((AS_COHORT_EXPR) self));
}


INT AS_CRE1921695223_unbox(AS_CREATE_EXPR self) {
 
 return AS_CRE1921695223(((AS_CREATE_EXPR) self));
}


INT AS_EXC629960794_unbox(AS_EXCEPT_EXPR self) {
 
 return AS_EXC629960794(((AS_EXCEPT_EXPR) self));
}


INT AS_FAR1350508667_unbox(AS_FAR_EXPR self) {
 
 return AS_FAR1350508667(((AS_FAR_EXPR) self));
}


INT AS_FLT374280066_unbox(AS_FLT_LIT_EXPR self) {
 
 return AS_FLT374280066(((AS_FLT_LIT_EXPR) self));
}


INT AS_GLO113092240_unbox(AS_GLOBAL_EXPR self) {
 
 return AS_GLO113092240(((AS_GLOBAL_EXPR) self));
}


INT AS_HER1196006492_unbox(AS_HERE_EXPR self) {
 
 return AS_HER1196006492(((AS_HERE_EXPR) self));
}


INT AS_INI414727320_unbox(AS_INITIAL_EXPR self) {
 
 return AS_INI414727320(((AS_INITIAL_EXPR) self));
}


INT AS_INT228236077_unbox(AS_INT_LIT_EXPR self) {
 
 return AS_INT228236077(((AS_INT_LIT_EXPR) self));
}


INT AS_IS_1954421395_unbox(AS_IS_VOID_EXPR self) {
 
 return AS_IS_1954421395(((AS_IS_VOID_EXPR) self));
}


INT AS_NEA1735606560_unbox(AS_NEAR_EXPR self) {
 
 return AS_NEA1735606560(((AS_NEAR_EXPR) self));
}


INT AS_NEW1836119458_unbox(AS_NEW_EXPR self) {
 
 return AS_NEW1836119458(((AS_NEW_EXPR) self));
}


INT AS_OR_1113601798_unbox(AS_OR_EXPR self) {
 
 return AS_OR_1113601798(((AS_OR_EXPR) self));
}


INT AS_RES32147922_unbox(AS_RESULT_EXPR self) {
 
 return AS_RES32147922(((AS_RESULT_EXPR) self));
}


INT AS_SEL1486205964_unbox(AS_SELF_EXPR self) {
 
 return AS_SEL1486205964(((AS_SELF_EXPR) self));
}


INT AS_STR1375497755_unbox(AS_STR_LIT_EXPR self) {
 
 return AS_STR1375497755(((AS_STR_LIT_EXPR) self));
}


INT AS_UND1736379304_unbox(AS_UND152986614 self) {
 
 return AS_UND1736379304(((AS_UND152986614) self));
}


INT AS_VOI1355443380_unbox(AS_VOID_EXPR self) {
 
 return AS_VOI1355443380(((AS_VOID_EXPR) self));
}


INT AS_WHE140356038_unbox(AS_WHERE_EXPR self) {
 
 return AS_WHE140356038(((AS_WHERE_EXPR) self));
}


BOOL CALL_T16429326_unbox(CALL_TP_ARRAY self, dTP arg1) {
 
 return CALL_T16429326(((CALL_TP_ARRAY) self), arg1);
}


BOOL CALL_T148273370_unbox(CALL_T561792367 self, dTP arg1) {
 
 return CALL_T148273370(((CALL_T561792367) self), arg1);
}


BOOL CALL_T640058966_unbox(CALL_TP_CREATE self, dTP arg1) {
 
 return CALL_T640058966(((CALL_TP_CREATE) self), arg1);
}


BOOL CALL_T1822200530_unbox(CALL_T666389079 self, dTP arg1) {
 
 return CALL_T1822200530(((CALL_T666389079) self), arg1);
}


BOOL CALL_T1680302827_unbox(CALL_TP_VOID self, dTP arg1) {
 
 return CALL_T1680302827(((CALL_TP_VOID) self), arg1);
}


dAM_EXPR INLINE472404814_unbox(INLINE self, AM_ROU1916046290 arg1) {
 
 return INLINE472404814(((INLINE) self), arg1);
}


dAM_EXPR INLINE1426016646_unbox(INLINE self, AM_ITE1809135850 arg1) {
 
 return INLINE1426016646(((INLINE) self), arg1);
}


void AS_ASS1330792936_unbox(AS_ASSERT_STMT self, BOOL arg1) {
 
 AS_ASS1330792936(((AS_ASSERT_STMT) self), arg1);
}


void AS_ASS1708420135_unbox(AS_ASSIGN_STMT self, BOOL arg1) {
 
 AS_ASS1708420135(((AS_ASSIGN_STMT) self), arg1);
}


void AS_ATT1135887709_unbox(AS_ATTACH_STMT self, BOOL arg1) {
 
 AS_ATT1135887709(((AS_ATTACH_STMT) self), arg1);
}


void AS_CAS1011726673_unbox(AS_CASE_STMT self, BOOL arg1) {
 
 AS_CAS1011726673(((AS_CASE_STMT) self), arg1);
}


void AS_DEC1996561535_unbox(AS_DEC_STMT self, BOOL arg1) {
 
 AS_DEC1996561535(((AS_DEC_STMT) self), arg1);
}


void AS_EXP1321747790_unbox(AS_EXPR_STMT self, BOOL arg1) {
 
 AS_EXP1321747790(((AS_EXPR_STMT) self), arg1);
}


void AS_FOR1673645269_unbox(AS_FORK_STMT self, BOOL arg1) {
 
 AS_FOR1673645269(((AS_FORK_STMT) self), arg1);
}


void AS_IF_164980229_unbox(AS_IF_STMT self, BOOL arg1) {
 
 AS_IF_164980229(((AS_IF_STMT) self), arg1);
}


void AS_INT83312974_unbox(AS_INT2031239268 self, BOOL arg1) {
 
 AS_INT83312974(((AS_INT2031239268) self), arg1);
}


void AS_LOC755705564_unbox(AS_LOCK_STMT self, BOOL arg1) {
 
 AS_LOC755705564(((AS_LOCK_STMT) self), arg1);
}


void AS_LOO565196687_unbox(AS_LOOP_STMT self, BOOL arg1) {
 
 AS_LOO565196687(((AS_LOOP_STMT) self), arg1);
}


void AS_PAR771744212_unbox(AS_PAR_STMT self, BOOL arg1) {
 
 AS_PAR771744212(((AS_PAR_STMT) self), arg1);
}


void AS_PRO169226468_unbox(AS_PROTECT_STMT self, BOOL arg1) {
 
 AS_PRO169226468(((AS_PROTECT_STMT) self), arg1);
}


void AS_QUI1442696788_unbox(AS_QUIT_STMT self, BOOL arg1) {
 
 AS_QUI1442696788(((AS_QUIT_STMT) self), arg1);
}


void AS_RAI248856852_unbox(AS_RAISE_STMT self, BOOL arg1) {
 
 AS_RAI248856852(((AS_RAISE_STMT) self), arg1);
}


void AS_RET910390234_unbox(AS_RETURN_STMT self, BOOL arg1) {
 
 AS_RET910390234(((AS_RETURN_STMT) self), arg1);
}


void AS_STM1692300976_unbox(AS_STMT self, BOOL arg1) {
 
 AS_STM1692300976(((AS_STMT) self), arg1);
}


void AS_SYN1248387412_unbox(AS_SYNC_STMT self, BOOL arg1) {
 
 AS_SYN1248387412(((AS_SYNC_STMT) self), arg1);
}


void AS_TYP1218814867_unbox(AS_TYPECASE_STMT self, BOOL arg1) {
 
 AS_TYP1218814867(((AS_TYPECASE_STMT) self), arg1);
}


void AS_UNL1079389410_unbox(AS_UNLOCK_STMT self, BOOL arg1) {
 
 AS_UNL1079389410(((AS_UNLOCK_STMT) self), arg1);
}


void AS_WIT1613050679_unbox(AS_WIT1063437351 self, BOOL arg1) {
 
 AS_WIT1613050679(((AS_WIT1063437351) self), arg1);
}


void AS_YIE1468566771_unbox(AS_YIELD_STMT self, BOOL arg1) {
 
 AS_YIE1468566771(((AS_YIELD_STMT) self), arg1);
}


AS_TYPE_SPEC TP_CLA981298431_unbox(TP_CLASS self) {
 
 return TP_CLA981298431(((TP_CLASS) self));
}


AS_TYPE_SPEC TP_ITE610562510_unbox(TP_ITER self) {
 
 return TP_ITE610562510(((TP_ITER) self));
}


AS_TYPE_SPEC TP_ROU1982035318_unbox(TP_ROUT self) {
 
 return TP_ROU1982035318(((TP_ROUT) self));
}


AS_STMT_LIST AS_ASS1748452704_unbox(AS_ASSERT_STMT self) {
 
 return AS_ASS1748452704(((AS_ASSERT_STMT) self));
}


AS_STMT_LIST AS_ASS369354997_unbox(AS_ASSIGN_STMT self) {
 
 return AS_ASS369354997(((AS_ASSIGN_STMT) self));
}


AS_STMT_LIST AS_ATT1199964935_unbox(AS_ATTACH_STMT self) {
 
 return AS_ATT1199964935(((AS_ATTACH_STMT) self));
}


AS_STMT_LIST AS_CAS422199583_unbox(AS_CASE_STMT self) {
 
 return AS_CAS422199583(((AS_CASE_STMT) self));
}


AS_STMT_LIST AS_DEC1951684792_unbox(AS_DEC_STMT self) {
 
 return AS_DEC1951684792(((AS_DEC_STMT) self));
}


AS_STMT_LIST AS_EXP80096096_unbox(AS_EXPR_STMT self) {
 
 return AS_EXP80096096(((AS_EXPR_STMT) self));
}


AS_STMT_LIST AS_FOR1300129919_unbox(AS_FORK_STMT self) {
 
 return AS_FOR1300129919(((AS_FORK_STMT) self));
}


AS_STMT_LIST AS_IF_1377965621_unbox(AS_IF_STMT self) {
 
 return AS_IF_1377965621(((AS_IF_STMT) self));
}


AS_STMT_LIST AS_INT1290825245_unbox(AS_INT2031239268 self) {
 
 return AS_INT1290825245(((AS_INT2031239268) self));
}


AS_STMT_LIST AS_LOC1425522258_unbox(AS_LOCK_STMT self) {
 
 return AS_LOC1425522258(((AS_LOCK_STMT) self));
}


AS_STMT_LIST AS_LOO1823271233_unbox(AS_LOOP_STMT self) {
 
 return AS_LOO1823271233(((AS_LOOP_STMT) self));
}


AS_STMT_LIST AS_PAR380674305_unbox(AS_PAR_STMT self) {
 
 return AS_PAR380674305(((AS_PAR_STMT) self));
}


AS_STMT_LIST AS_PRO1473178483_unbox(AS_PROTECT_STMT self) {
 
 return AS_PRO1473178483(((AS_PROTECT_STMT) self));
}


AS_STMT_LIST AS_QUI1856043734_unbox(AS_QUIT_STMT self) {
 
 return AS_QUI1856043734(((AS_QUIT_STMT) self));
}


AS_STMT_LIST AS_RAI1834286773_unbox(AS_RAISE_STMT self) {
 
 return AS_RAI1834286773(((AS_RAISE_STMT) self));
}


AS_STMT_LIST AS_RET1323242550_unbox(AS_RETURN_STMT self) {
 
 return AS_RET1323242550(((AS_RETURN_STMT) self));
}


AS_STMT_LIST AS_STM87227529_unbox(AS_STMT self) {
 
 return AS_STM87227529(((AS_STMT) self));
}


AS_STMT_LIST AS_SYN1536258462_unbox(AS_SYNC_STMT self) {
 
 return AS_SYN1536258462(((AS_SYNC_STMT) self));
}


AS_STMT_LIST AS_TYP2083833657_unbox(AS_TYPECASE_STMT self) {
 
 return AS_TYP2083833657(((AS_TYPECASE_STMT) self));
}


AS_STMT_LIST AS_UNL900516002_unbox(AS_UNLOCK_STMT self) {
 
 return AS_UNL900516002(((AS_UNLOCK_STMT) self));
}


AS_STMT_LIST AS_WIT1668425008_unbox(AS_WIT1063437351 self) {
 
 return AS_WIT1668425008(((AS_WIT1063437351) self));
}


AS_STMT_LIST AS_YIE913253602_unbox(AS_YIELD_STMT self) {
 
 return AS_YIE913253602(((AS_YIELD_STMT) self));
}


BOOL AS_ASS1377838817_unbox(AS_ASSERT_STMT self) {
 
 return AS_ASS1377838817(((AS_ASSERT_STMT) self));
}


BOOL AS_ASS1755466016_unbox(AS_ASSIGN_STMT self) {
 
 return AS_ASS1755466016(((AS_ASSIGN_STMT) self));
}


BOOL AS_ATT1182933590_unbox(AS_ATTACH_STMT self) {
 
 return AS_ATT1182933590(((AS_ATTACH_STMT) self));
}


BOOL AS_CAS964680792_unbox(AS_CASE_STMT self) {
 
 return AS_CAS964680792(((AS_CASE_STMT) self));
}


BOOL AS_DEC2043607416_unbox(AS_DEC_STMT self) {
 
 return AS_DEC2043607416(((AS_DEC_STMT) self));
}


BOOL AS_EXP1274701909_unbox(AS_EXPR_STMT self) {
 
 return AS_EXP1274701909(((AS_EXPR_STMT) self));
}


BOOL AS_FOR1720691150_unbox(AS_FORK_STMT self) {
 
 return AS_FOR1720691150(((AS_FORK_STMT) self));
}


BOOL AS_IF_117934348_unbox(AS_IF_STMT self) {
 
 return AS_IF_117934348(((AS_IF_STMT) self));
}


BOOL AS_INT130358855_unbox(AS_INT2031239268 self) {
 
 return AS_INT130358855(((AS_INT2031239268) self));
}


BOOL AS_LOC802751445_unbox(AS_LOCK_STMT self) {
 
 return AS_LOC802751445(((AS_LOCK_STMT) self));
}


BOOL AS_LOO612242568_unbox(AS_LOOP_STMT self) {
 
 return AS_LOO612242568(((AS_LOOP_STMT) self));
}


BOOL AS_PAR818790093_unbox(AS_PAR_STMT self) {
 
 return AS_PAR818790093(((AS_PAR_STMT) self));
}


BOOL AS_PRO122180587_unbox(AS_PROTECT_STMT self) {
 
 return AS_PRO122180587(((AS_PROTECT_STMT) self));
}


BOOL AS_QUI1489742669_unbox(AS_QUIT_STMT self) {
 
 return AS_QUI1489742669(((AS_QUIT_STMT) self));
}


BOOL AS_RAI201810971_unbox(AS_RAISE_STMT self) {
 
 return AS_RAI201810971(((AS_RAISE_STMT) self));
}


BOOL AS_RET863344353_unbox(AS_RETURN_STMT self) {
 
 return AS_RET863344353(((AS_RETURN_STMT) self));
}


BOOL AS_STM1645255095_unbox(AS_STMT self) {
 
 return AS_STM1645255095(((AS_STMT) self));
}


BOOL AS_SYN1201341531_unbox(AS_SYNC_STMT self) {
 
 return AS_SYN1201341531(((AS_SYNC_STMT) self));
}


BOOL AS_TYP1265860748_unbox(AS_TYPECASE_STMT self) {
 
 return AS_TYP1265860748(((AS_TYPECASE_STMT) self));
}


BOOL AS_UNL1126435291_unbox(AS_UNLOCK_STMT self) {
 
 return AS_UNL1126435291(((AS_UNLOCK_STMT) self));
}


BOOL AS_WIT1566004798_unbox(AS_WIT1063437351 self) {
 
 return AS_WIT1566004798(((AS_WIT1063437351) self));
}


BOOL AS_YIE1515612652_unbox(AS_YIELD_STMT self) {
 
 return AS_YIE1515612652(((AS_YIELD_STMT) self));
}


void INLINE1765310158_unbox(INLINE self, AM_ROUT_DEF arg1) {
 
 INLINE1765310158(((INLINE) self), arg1);
}


void OPTIMI1081780129_unbox(OPTIMIZE self, AM_ROUT_DEF arg1) {
 
 OPTIMI1081780129(((OPTIMIZE) self), arg1);
}


void GENERA1485011776_unbox(GENERATE_AM self) {
 
 GENERA1485011776(((GENERATE_AM) self));
}


STR ABSTRA12614027_unbox(ABSTRA151498155 self, CGEN arg1) {
 
 return ABSTRA12614027(((ABSTRA151498155) self), arg1);
}


STR ABSTRA353314648_unbox(ABSTRACT_LAYOUT self, CGEN arg1) {
 
 return ABSTRA353314648(((ABSTRACT_LAYOUT) self), arg1);
}


STR ARG_LA807737867_unbox(ARG_LAYOUT self, CGEN arg1) {
 
 return ARG_LA807737867(((ARG_LAYOUT) self), arg1);
}


STR BOUND_1401874693_unbox(BOUND_1007407331 self, CGEN arg1) {
 
 return BOUND_1401874693(((BOUND_1007407331) self), arg1);
}


STR BOUND_365396347_unbox(BOUND_2140373124 self, CGEN arg1) {
 
 return BOUND_365396347(((BOUND_2140373124) self), arg1);
}


STR BOUND_1707602440_unbox(BOUND_1677815802 self, CGEN arg1) {
 
 return BOUND_1707602440(((BOUND_1677815802) self), arg1);
}


STR BOUND_1537967790_unbox(BOUND_809390774 self, CGEN arg1) {
 
 return BOUND_1537967790(((BOUND_809390774) self), arg1);
}


STR BUILTI35892888_unbox(BUILTIN_LAYOUT self, CGEN arg1) {
 
 return BUILTI35892888(((BUILTIN_LAYOUT) self), arg1);
}


STR CLASS_954167052_unbox(CLASS_LAYOUT self, CGEN arg1) {
 
 return CLASS_954167052(((CLASS_LAYOUT) self), arg1);
}


STR EXTERN1472641871_unbox(EXTERNAL_LAYOUT self, CGEN arg1) {
 
 return EXTERN1472641871(((EXTERNAL_LAYOUT) self), arg1);
}


STR FRAME_691068581_unbox(FRAME_LAYOUT self, CGEN arg1) {
 
 return FRAME_691068581(((FRAME_LAYOUT) self), arg1);
}


STR IMMUTA2093463840_unbox(IMMUTA421759893 self, CGEN arg1) {
 
 return IMMUTA2093463840(((IMMUTA421759893) self), arg1);
}


STR TP_LAY690809295_unbox(TP_LAYOUT self, CGEN arg1) {
 
 return TP_LAY690809295(((TP_LAYOUT) self), arg1);
}


void CGEN_finalize_unbox(CGEN self) {
 
 CGEN_finalize(((CGEN) self));
}


void OPTIMI333751220_unbox(OPTIMIZE self) {
 
 OPTIMI333751220(((OPTIMIZE) self));
}


void CHECK_801221867_unbox(CHECK_AM self) {
 
 CHECK_801221867(((CHECK_AM) self));
}


void NULL_STAT_print_unbox(NULL_STAT self) {
 
 NULL_STAT_print(((NULL_STAT) self));
}


void STAT_print_unbox(STAT1 self) {
 
 STAT_print(((STAT1) self));
}


dAM_CONST ARRAYd198808957_unbox(dELTdA578383841_frame frame) {
 
 return ARRAYd198808957((ARRAYd198808957_frame) frame);
}
void* ARRAYd198808957_frame_alloc() {
 return OBALLOC(ARRAYd198808957_frame);
}


dAM_CONST FLISTd617209260_unbox(dELTdA578383841_frame frame) {
 
 return FLISTd617209260((FLISTd617209260_frame) frame);
}
void* FLISTd617209260_frame_alloc() {
 return OBALLOC(FLISTd617209260_frame);
}


STR ARRAYS1578977701_unbox(dELTSTR_eltbrSTR_frame frame) {
 
 return ARRAYS1578977701((ARRAYS1578977701_frame) frame);
}
void* ARRAYS1578977701_frame_alloc() {
 return OBALLOC(ARRAYS1578977701_frame);
}


STR FLISTS1171631134_unbox(dELTSTR_eltbrSTR_frame frame) {
 
 return FLISTS1171631134((FLISTS1171631134_frame) frame);
}
void* FLISTS1171631134_frame_alloc() {
 return OBALLOC(FLISTS1171631134_frame);
}


STR FSETSTR_eltbrSTR_unbox(dELTSTR_eltbrSTR_frame frame) {
 
 return FSETSTR_eltbrSTR((FSETSTR_eltbrSTR_frame) frame);
}
void* FSETSTR_eltbrSTR_frame_alloc() {
 return OBALLOC(FSETSTR_eltbrSTR_frame);
}


AM_BND367211769 ARRAYA1632293813_unbox(dELTAM157362729_frame frame) {
 
 return ARRAYA1632293813((ARRAYA1632293813_frame) frame);
}
void* ARRAYA1632293813_frame_alloc() {
 return OBALLOC(ARRAYA1632293813_frame);
}


AM_BND367211769 FLISTA1446310868_unbox(dELTAM157362729_frame frame) {
 
 return FLISTA1446310868((FLISTA1446310868_frame) frame);
}
void* FLISTA1446310868_frame_alloc() {
 return OBALLOC(FLISTA1446310868_frame);
}


AM_ITE1809135850 ARRAYA494342579_unbox(dELTAM1806540817_frame frame) {
 
 return ARRAYA494342579((ARRAYA494342579_frame) frame);
}
void* ARRAYA494342579_frame_alloc() {
 return OBALLOC(ARRAYA494342579_frame);
}


AM_ITE1809135850 FLISTA20621028_unbox(dELTAM1806540817_frame frame) {
 
 return FLISTA20621028((FLISTA20621028_frame) frame);
}
void* FLISTA20621028_frame_alloc() {
 return OBALLOC(FLISTA20621028_frame);
}


AM_CURSOR_POS ARRAYA2146369463_unbox(dELTAM2044693667_frame frame) {
 
 return ARRAYA2146369463((ARRAYA2146369463_frame) frame);
}
void* ARRAYA2146369463_frame_alloc() {
 return OBALLOC(ARRAYA2146369463_frame);
}


AM_CURSOR_POS A_STAC408111917_unbox(dELTAM2044693667_frame frame) {
 
 return A_STAC408111917((A_STAC408111917_frame) frame);
}
void* A_STAC408111917_frame_alloc() {
 return OBALLOC(A_STAC408111917_frame);
}


AM_CURSOR_POS FLISTA1660160_unbox(dELTAM2044693667_frame frame) {
 
 return FLISTA1660160((FLISTA1660160_frame) frame);
}
void* FLISTA1660160_frame_alloc() {
 return OBALLOC(FLISTA1660160_frame);
}


dAM_EXPR ARRAYd1413132360_unbox(dELTdA379108592_frame frame) {
 
 return ARRAYd1413132360((ARRAYd1413132360_frame) frame);
}
void* ARRAYd1413132360_frame_alloc() {
 return OBALLOC(ARRAYd1413132360_frame);
}


dAM_EXPR FLISTd2122660729_unbox(dELTdA379108592_frame frame) {
 
 return FLISTd2122660729((FLISTd2122660729_frame) frame);
}
void* FLISTd2122660729_frame_alloc() {
 return OBALLOC(FLISTd2122660729_frame);
}


dAM_STMT ARRAYd615421366_unbox(dELTdA418602402_frame frame) {
 
 return ARRAYd615421366((ARRAYd615421366_frame) frame);
}
void* ARRAYd615421366_frame_alloc() {
 return OBALLOC(ARRAYd615421366_frame);
}


dAM_STMT FLISTd1374595573_unbox(dELTdA418602402_frame frame) {
 
 return FLISTd1374595573((FLISTd1374595573_frame) frame);
}
void* FLISTd1374595573_frame_alloc() {
 return OBALLOC(FLISTd1374595573_frame);
}


dLAYOUT ARRAYd1647530701_unbox(dELTdL669601151_frame frame) {
 
 return ARRAYd1647530701((ARRAYd1647530701_frame) frame);
}
void* ARRAYd1647530701_frame_alloc() {
 return OBALLOC(ARRAYd1647530701_frame);
}


dLAYOUT FLISTd1954760550_unbox(dELTdL669601151_frame frame) {
 
 return FLISTd1954760550((FLISTd1954760550_frame) frame);
}
void* FLISTd1954760550_frame_alloc() {
 return OBALLOC(FLISTd1954760550_frame);
}


dLAYOUT FSETdL485771726_unbox(dELTdL669601151_frame frame) {
 
 return FSETdL485771726((FSETdL485771726_frame) frame);
}
void* FSETdL485771726_frame_alloc() {
 return OBALLOC(FSETdL485771726_frame);
}


dTP ARRAYd2147017943_unbox(dELTdTP_eltbrdTP_frame frame) {
 
 return ARRAYd2147017943((ARRAYd2147017943_frame) frame);
}
void* ARRAYd2147017943_frame_alloc() {
 return OBALLOC(ARRAYd2147017943_frame);
}


dTP FLISTd1740602786_unbox(dELTdTP_eltbrdTP_frame frame) {
 
 return FLISTd1740602786((FLISTd1740602786_frame) frame);
}
void* FLISTd1740602786_frame_alloc() {
 return OBALLOC(FLISTd1740602786_frame);
}


dTP FSETdTP_eltbrdTP_unbox(dELTdTP_eltbrdTP_frame frame) {
 
 return FSETdTP_eltbrdTP((FSETdTP_eltbrdTP_frame) frame);
}
void* FSETdTP_eltbrdTP_frame_alloc() {
 return OBALLOC(FSETdTP_eltbrdTP_frame);
}


ABSTRACT_LAYOUT ARRAYA967821567_unbox(dELTAB1485261483_frame frame) {
 
 return ARRAYA967821567((ARRAYA967821567_frame) frame);
}
void* ARRAYA967821567_frame_alloc() {
 return OBALLOC(ARRAYA967821567_frame);
}


ABSTRACT_LAYOUT FLISTA1252869158_unbox(dELTAB1485261483_frame frame) {
 
 return FLISTA1252869158((FLISTA1252869158_frame) frame);
}
void* FLISTA1252869158_frame_alloc() {
 return OBALLOC(FLISTA1252869158_frame);
}


ABSTRACT_LAYOUT FSETAB1942662686_unbox(dELTAB1485261483_frame frame) {
 
 return FSETAB1942662686((FSETAB1942662686_frame) frame);
}
void* FSETAB1942662686_frame_alloc() {
 return OBALLOC(FSETAB1942662686_frame);
}


AM_BND1124877163 ARRAYA945146506_unbox(dELTAM745071702_frame frame) {
 
 return ARRAYA945146506((ARRAYA945146506_frame) frame);
}
void* ARRAYA945146506_frame_alloc() {
 return OBALLOC(ARRAYA945146506_frame);
}


AM_BND1124877163 FLISTA273121893_unbox(dELTAM745071702_frame frame) {
 
 return FLISTA273121893((FLISTA273121893_frame) frame);
}
void* FLISTA273121893_frame_alloc() {
 return OBALLOC(FLISTA273121893_frame);
}


AM_BND1124877163 FSETAM595751729_unbox(dELTAM745071702_frame frame) {
 
 return FSETAM595751729((FSETAM595751729_frame) frame);
}
void* FSETAM595751729_frame_alloc() {
 return OBALLOC(FSETAM595751729_frame);
}


AM_CALL_ARG ARRAYA44788561_unbox(dELTAM1395458731_frame frame) {
 
 return ARRAYA44788561((ARRAYA44788561_frame) frame);
}
void* ARRAYA44788561_frame_alloc() {
 return OBALLOC(ARRAYA44788561_frame);
}


AM_CALL_ARG FLISTA964597258_unbox(dELTAM1395458731_frame frame) {
 
 return FLISTA964597258((FLISTA964597258_frame) frame);
}
void* FLISTA964597258_frame_alloc() {
 return OBALLOC(FLISTA964597258_frame);
}


AM_CURSOR ARRAYA900901929_unbox(dELTAM1280476813_frame frame) {
 
 return ARRAYA900901929((ARRAYA900901929_frame) frame);
}
void* ARRAYA900901929_frame_alloc() {
 return OBALLOC(ARRAYA900901929_frame);
}


AM_CURSOR FLISTA84883712_unbox(dELTAM1280476813_frame frame) {
 
 return FLISTA84883712((FLISTA84883712_frame) frame);
}
void* FLISTA84883712_frame_alloc() {
 return OBALLOC(FLISTA84883712_frame);
}


AM_FORMAL_ARG ARRAYA1315754333_unbox(dELTAM1417430129_frame frame) {
 
 return ARRAYA1315754333((ARRAYA1315754333_frame) frame);
}
void* ARRAYA1315754333_frame_alloc() {
 return OBALLOC(ARRAYA1315754333_frame);
}


AM_FORMAL_ARG FLISTA831183340_unbox(dELTAM1417430129_frame frame) {
 
 return FLISTA831183340((FLISTA831183340_frame) frame);
}
void* FLISTA831183340_frame_alloc() {
 return OBALLOC(FLISTA831183340_frame);
}


AM_LOCAL_EXPR ARRAYA27295121_unbox(dELTAM128970917_frame frame) {
 
 return ARRAYA27295121((ARRAYA27295121_frame) frame);
}
void* ARRAYA27295121_frame_alloc() {
 return OBALLOC(ARRAYA27295121_frame);
}


AM_LOCAL_EXPR FLISTA2119642552_unbox(dELTAM128970917_frame frame) {
 
 return FLISTA2119642552((FLISTA2119642552_frame) frame);
}
void* FLISTA2119642552_frame_alloc() {
 return OBALLOC(FLISTA2119642552_frame);
}


AM_LOCAL_EXPR FSETAM441739032_unbox(dELTAM128970917_frame frame) {
 
 return FSETAM441739032((FSETAM441739032_frame) frame);
}
void* FSETAM441739032_frame_alloc() {
 return OBALLOC(FSETAM441739032_frame);
}


AM_ROUT_DEF ARRAYA1253289773_unbox(dELTAM186957519_frame frame) {
 
 return ARRAYA1253289773((ARRAYA1253289773_frame) frame);
}
void* ARRAYA1253289773_frame_alloc() {
 return OBALLOC(ARRAYA1253289773_frame);
}


AM_ROUT_DEF FLISTA2121868826_unbox(dELTAM186957519_frame frame) {
 
 return FLISTA2121868826((FLISTA2121868826_frame) frame);
}
void* FLISTA2121868826_frame_alloc() {
 return OBALLOC(FLISTA2121868826_frame);
}


AM_ROUT_DEF FSETAM1323524382_unbox(dELTAM186957519_frame frame) {
 
 return FSETAM1323524382((FSETAM1323524382_frame) frame);
}
void* FSETAM1323524382_frame_alloc() {
 return OBALLOC(FSETAM1323524382_frame);
}


AM_SHARED_EXPR ARRAYA1830087990_unbox(dELTAM119654938_frame frame) {
 
 return ARRAYA1830087990((ARRAYA1830087990_frame) frame);
}
void* ARRAYA1830087990_frame_alloc() {
 return OBALLOC(ARRAYA1830087990_frame);
}


AM_SHARED_EXPR FLISTA120298683_unbox(dELTAM119654938_frame frame) {
 
 return FLISTA120298683((FLISTA120298683_frame) frame);
}
void* FLISTA120298683_frame_alloc() {
 return OBALLOC(FLISTA120298683_frame);
}


AM_SHARED_EXPR FSETAM12483681_unbox(dELTAM119654938_frame frame) {
 
 return FSETAM12483681((FSETAM12483681_frame) frame);
}
void* FSETAM12483681_frame_alloc() {
 return OBALLOC(FSETAM12483681_frame);
}


ARG ARRAYA827580689_unbox(dELTARG_eltbrARG_frame frame) {
 
 return ARRAYA827580689((ARRAYA827580689_frame) frame);
}
void* ARRAYA827580689_frame_alloc() {
 return OBALLOC(ARRAYA827580689_frame);
}


ARG FLISTA420234122_unbox(dELTARG_eltbrARG_frame frame) {
 
 return FLISTA420234122((FLISTA420234122_frame) frame);
}
void* FLISTA420234122_frame_alloc() {
 return OBALLOC(FLISTA420234122_frame);
}


ARRAYdAM_EXPR ARRAYA289850317_unbox(dELTAR391526113_frame frame) {
 
 return ARRAYA289850317((ARRAYA289850317_frame) frame);
}
void* ARRAYA289850317_frame_alloc() {
 return OBALLOC(ARRAYA289850317_frame);
}


ARRAYdAM_EXPR FLISTA1857087356_unbox(dELTAR391526113_frame frame) {
 
 return FLISTA1857087356((FLISTA1857087356_frame) frame);
}
void* FLISTA1857087356_frame_alloc() {
 return OBALLOC(FLISTA1857087356_frame);
}


ARRAYBOOL ARRAYA277860273_unbox(dELTAR657435157_frame frame) {
 
 return ARRAYA277860273((ARRAYA277860273_frame) frame);
}
void* ARRAYA277860273_frame_alloc() {
 return OBALLOC(ARRAYA277860273_frame);
}


ARRAYBOOL FLISTA538157944_unbox(dELTAR657435157_frame frame) {
 
 return FLISTA538157944((FLISTA538157944_frame) frame);
}
void* FLISTA538157944_frame_alloc() {
 return OBALLOC(FLISTA538157944_frame);
}


ARRAYSTR ARRAYA1729769776_unbox(dELTAR1531173752_frame frame) {
 
 return ARRAYA1729769776((ARRAYA1729769776_frame) frame);
}
void* ARRAYA1729769776_frame_alloc() {
 return OBALLOC(ARRAYA1729769776_frame);
}


ARRAYSTR FLISTA970595569_unbox(dELTAR1531173752_frame frame) {
 
 return FLISTA970595569((FLISTA970595569_frame) frame);
}
void* FLISTA970595569_frame_alloc() {
 return OBALLOC(FLISTA970595569_frame);
}


AS_FEAT_MOD ARRAYA1522064985_unbox(dELTAS81817693_frame frame) {
 
 return ARRAYA1522064985((ARRAYA1522064985_frame) frame);
}
void* ARRAYA1522064985_frame_alloc() {
 return OBALLOC(ARRAYA1522064985_frame);
}


AS_FEAT_MOD FLISTA1853093614_unbox(dELTAS81817693_frame frame) {
 
 return FLISTA1853093614((FLISTA1853093614_frame) frame);
}
void* FLISTA1853093614_frame_alloc() {
 return OBALLOC(FLISTA1853093614_frame);
}


AS_FEAT_MOD FSETAS1592299594_unbox(dELTAS81817693_frame frame) {
 
 return FSETAS1592299594((FSETAS1592299594_frame) frame);
}
void* FSETAS1592299594_frame_alloc() {
 return OBALLOC(FSETAS1592299594_frame);
}


ATTRSORT ARRAYA1660316962_unbox(dELTAT1600626566_frame frame) {
 
 return ARRAYA1660316962((ARRAYA1660316962_frame) frame);
}
void* ARRAYA1660316962_frame_alloc() {
 return OBALLOC(ARRAYA1660316962_frame);
}


ATTRSORT FLISTA901142755_unbox(dELTAT1600626566_frame frame) {
 
 return FLISTA901142755((FLISTA901142755_frame) frame);
}
void* FLISTA901142755_frame_alloc() {
 return OBALLOC(FLISTA901142755_frame);
}


BOOL ARRAY21272457581_unbox(dELTBO582795216_frame frame) {
 
 return ARRAY21272457581((ARRAY21272457581_frame) frame);
}
void* ARRAY21272457581_frame_alloc() {
 return OBALLOC(ARRAY21272457581_frame);
}


BOOL ARRAYB1390856792_unbox(dELTBO582795216_frame frame) {
 
 return ARRAYB1390856792((ARRAYB1390856792_frame) frame);
}
void* ARRAYB1390856792_frame_alloc() {
 return OBALLOC(ARRAYB1390856792_frame);
}


BOOL FLISTB1880887881_unbox(dELTBO582795216_frame frame) {
 
 return FLISTB1880887881((FLISTB1880887881_frame) frame);
}
void* FLISTB1880887881_frame_alloc() {
 return OBALLOC(FLISTB1880887881_frame);
}


BOUND_2140373124 ARRAYB1475407144_unbox(dELTBO1349583384_frame frame) {
 
 return ARRAYB1475407144((ARRAYB1475407144_frame) frame);
}
void* ARRAYB1475407144_frame_alloc() {
 return OBALLOC(ARRAYB1475407144_frame);
}


BOUND_2140373124 FLISTB455167321_unbox(dELTBO1349583384_frame frame) {
 
 return FLISTB455167321((FLISTB455167321_frame) frame);
}
void* FLISTB455167321_frame_alloc() {
 return OBALLOC(FLISTB455167321_frame);
}


BOUND_2140373124 FSETBO677183779_unbox(dELTBO1349583384_frame frame) {
 
 return FSETBO677183779((FSETBO677183779_frame) frame);
}
void* FSETBO677183779_frame_alloc() {
 return OBALLOC(FSETBO677183779_frame);
}


BOUND_809390774 ARRAYB1420960227_unbox(dELTBO879923169_frame frame) {
 
 return ARRAYB1420960227((ARRAYB1420960227_frame) frame);
}
void* ARRAYB1420960227_frame_alloc() {
 return OBALLOC(ARRAYB1420960227_frame);
}


BOUND_809390774 FLISTB905996620_unbox(dELTBO879923169_frame frame) {
 
 return FLISTB905996620((FLISTB905996620_frame) frame);
}
void* FLISTB905996620_frame_alloc() {
 return OBALLOC(FLISTB905996620_frame);
}


BOUND_809390774 FSETBO10998148_unbox(dELTBO879923169_frame frame) {
 
 return FSETBO10998148((FSETBO10998148_frame) frame);
}
void* FSETBO10998148_frame_alloc() {
 return OBALLOC(FSETBO10998148_frame);
}


BUILTIN_LAYOUT ARRAYB1615560602_unbox(dELTBU334182326_frame frame) {
 
 return ARRAYB1615560602((ARRAYB1615560602_frame) frame);
}
void* ARRAYB1615560602_frame_alloc() {
 return OBALLOC(ARRAYB1615560602_frame);
}


BUILTIN_LAYOUT FLISTB334826071_unbox(dELTBU334182326_frame frame) {
 
 return FLISTB334826071((FLISTB334826071_frame) frame);
}
void* FLISTB334826071_frame_alloc() {
 return OBALLOC(FLISTB334826071_frame);
}


BUILTIN_LAYOUT FSETBU202043707_unbox(dELTBU334182326_frame frame) {
 
 return FSETBU202043707((FSETBU202043707_frame) frame);
}
void* FSETBU202043707_frame_alloc() {
 return OBALLOC(FSETBU202043707_frame);
}


CALL_ARG ARRAYC494195798_unbox(dELTCA1528219566_frame frame) {
 
 return ARRAYC494195798((ARRAYC494195798_frame) frame);
}
void* ARRAYC494195798_frame_alloc() {
 return OBALLOC(ARRAYC494195798_frame);
}


CALL_ARG FLISTC264978409_unbox(dELTCA1528219566_frame frame) {
 
 return FLISTC264978409((FLISTC264978409_frame) frame);
}
void* FLISTC264978409_frame_alloc() {
 return OBALLOC(FLISTC264978409_frame);
}


CHAR ARRAYC1494984744_unbox(dELTCH686923168_frame frame) {
 
 return ARRAYC1494984744((ARRAYC1494984744_frame) frame);
}
void* ARRAYC1494984744_frame_alloc() {
 return OBALLOC(ARRAYC1494984744_frame);
}


CHAR FLISTC1776759929_unbox(dELTCH686923168_frame frame) {
 
 return FLISTC1776759929((FLISTC1776759929_frame) frame);
}
void* FLISTC1776759929_frame_alloc() {
 return OBALLOC(FLISTC1776759929_frame);
}


CHAR STR_eltbrCHAR_unbox(dELTCH686923168_frame frame) {
 
 return STR_eltbrCHAR((STR_eltbrCHAR_frame) frame);
}
void* STR_eltbrCHAR_frame_alloc() {
 return OBALLOC(STR_eltbrCHAR_frame);
}


CLASS_LAYOUT ARRAYC818595214_unbox(dELTCL1056825190_frame frame) {
 
 return ARRAYC818595214((ARRAYC818595214_frame) frame);
}
void* ARRAYC818595214_frame_alloc() {
 return OBALLOC(ARRAYC818595214_frame);
}


CLASS_LAYOUT FLISTC519862611_unbox(dELTCL1056825190_frame frame) {
 
 return FLISTC519862611((FLISTC519862611_frame) frame);
}
void* FLISTC519862611_frame_alloc() {
 return OBALLOC(FLISTC519862611_frame);
}


CLASS_LAYOUT FSETCL1233705521_unbox(dELTCL1056825190_frame frame) {
 
 return FSETCL1233705521((FSETCL1233705521_frame) frame);
}
void* FSETCL1233705521_frame_alloc() {
 return OBALLOC(FSETCL1233705521_frame);
}


CODE_FILE ARRAYC1531429675_unbox(dELTCO1151854791_frame frame) {
 
 return ARRAYC1531429675((ARRAYC1531429675_frame) frame);
}
void* ARRAYC1531429675_frame_alloc() {
 return OBALLOC(ARRAYC1531429675_frame);
}


CODE_FILE FLISTC1947519404_unbox(dELTCO1151854791_frame frame) {
 
 return FLISTC1947519404((FLISTC1947519404_frame) frame);
}
void* FLISTC1947519404_frame_alloc() {
 return OBALLOC(FLISTC1947519404_frame);
}


CSE_DOUBLE_EXPRS ARRAYC1440407068_unbox(dELTCS742343900_frame frame) {
 
 return ARRAYC1440407068((ARRAYC1440407068_frame) frame);
}
void* ARRAYC1440407068_frame_alloc() {
 return OBALLOC(ARRAYC1440407068_frame);
}


CSE_DOUBLE_EXPRS FLISTC1263372315_unbox(dELTCS742343900_frame frame) {
 
 return FLISTC1263372315((FLISTC1263372315_frame) frame);
}
void* FLISTC1263372315_frame_alloc() {
 return OBALLOC(FLISTC1263372315_frame);
}


CSE_DOUBLE_EXPRS FSETCS1170733135_unbox(dELTCS742343900_frame frame) {
 
 return FSETCS1170733135((FSETCS1170733135_frame) frame);
}
void* FSETCS1170733135_frame_alloc() {
 return OBALLOC(FSETCS1170733135_frame);
}


EXPR_HOISTED ARRAYE1693383532_unbox(dELTEX1931613508_frame frame) {
 
 return ARRAYE1693383532((ARRAYE1693383532_frame) frame);
}
void* ARRAYE1693383532_frame_alloc() {
 return OBALLOC(ARRAYE1693383532_frame);
}


EXPR_HOISTED FLISTE354925707_unbox(dELTEX1931613508_frame frame) {
 
 return FLISTE354925707((FLISTE354925707_frame) frame);
}
void* FLISTE354925707_frame_alloc() {
 return OBALLOC(FLISTE354925707_frame);
}


EXTERNAL_LAYOUT ARRAYE1687022731_unbox(dELTEX2090504649_frame frame) {
 
 return ARRAYE1687022731((ARRAYE1687022731_frame) frame);
}
void* ARRAYE1687022731_frame_alloc() {
 return OBALLOC(ARRAYE1687022731_frame);
}


EXTERNAL_LAYOUT FLISTE1972070322_unbox(dELTEX2090504649_frame frame) {
 
 return FLISTE1972070322((FLISTE1972070322_frame) frame);
}
void* FLISTE1972070322_frame_alloc() {
 return OBALLOC(FLISTE1972070322_frame);
}


EXTERNAL_LAYOUT FSETEX1633103446_unbox(dELTEX2090504649_frame frame) {
 
 return FSETEX1633103446((FSETEX1633103446_frame) frame);
}
void* FSETEX1633103446_frame_alloc() {
 return OBALLOC(FSETEX1633103446_frame);
}


FLISTdAM_CONST ARRAYF1657157762_unbox(dELTFL688066606_frame frame) {
 
 return ARRAYF1657157762((ARRAYF1657157762_frame) frame);
}
void* ARRAYF1657157762_frame_alloc() {
 return OBALLOC(ARRAYF1657157762_frame);
}


FLISTdAM_CONST FLISTF687422861_unbox(dELTFL688066606_frame frame) {
 
 return FLISTF687422861((FLISTF687422861_frame) frame);
}
void* FLISTF687422861_frame_alloc() {
 return OBALLOC(FLISTF687422861_frame);
}


FLISTSTR ARRAYF1006928322_unbox(dELTFL27095446_frame frame) {
 
 return ARRAYF1006928322((ARRAYF1006928322_frame) frame);
}
void* ARRAYF1006928322_frame_alloc() {
 return OBALLOC(ARRAYF1006928322_frame);
}


FLISTSTR FLISTF1766102529_unbox(dELTFL27095446_frame frame) {
 
 return FLISTF1766102529((FLISTF1766102529_frame) frame);
}
void* FLISTF1766102529_frame_alloc() {
 return OBALLOC(FLISTF1766102529_frame);
}


FSTR ARRAYF254484098_unbox(dELTFS553577478_frame frame) {
 
 return ARRAYF254484098((ARRAYF254484098_frame) frame);
}
void* ARRAYF254484098_frame_alloc() {
 return OBALLOC(ARRAYF254484098_frame);
}


FSTR FLISTF1277706721_unbox(dELTFS553577478_frame frame) {
 
 return FLISTF1277706721((FLISTF1277706721_frame) frame);
}
void* FLISTF1277706721_frame_alloc() {
 return OBALLOC(FLISTF1277706721_frame);
}


IDENT ARRAYI1725945879_unbox(dELTID1378398307_frame frame) {
 
 return ARRAYI1725945879((ARRAYI1725945879_frame) frame);
}
void* ARRAYI1725945879_frame_alloc() {
 return OBALLOC(ARRAYI1725945879_frame);
}


IDENT FLISTI1709766432_unbox(dELTID1378398307_frame frame) {
 
 return FLISTI1709766432((FLISTI1709766432_frame) frame);
}
void* FLISTI1709766432_frame_alloc() {
 return OBALLOC(FLISTI1709766432_frame);
}


IFC ARRAYI2003604369_unbox(dELTIFC_eltbrIFC_frame frame) {
 
 return ARRAYI2003604369((ARRAYI2003604369_frame) frame);
}
void* ARRAYI2003604369_frame_alloc() {
 return OBALLOC(ARRAYI2003604369_frame);
}


IFC FLISTI1596257802_unbox(dELTIFC_eltbrIFC_frame frame) {
 
 return FLISTI1596257802((FLISTI1596257802_frame) frame);
}
void* FLISTI1596257802_frame_alloc() {
 return OBALLOC(FLISTI1596257802_frame);
}


IMMUTA421759893 ARRAYI1264630278_unbox(dELTIM1138806518_frame frame) {
 
 return ARRAYI1264630278((ARRAYI1264630278_frame) frame);
}
void* ARRAYI1264630278_frame_alloc() {
 return OBALLOC(ARRAYI1264630278_frame);
}


IMMUTA421759893 FLISTI244390455_unbox(dELTIM1138806518_frame frame) {
 
 return FLISTI244390455((FLISTI244390455_frame) frame);
}
void* FLISTI244390455_frame_alloc() {
 return OBALLOC(FLISTI244390455_frame);
}


IMMUTA421759893 FSETIM887960645_unbox(dELTIM1138806518_frame frame) {
 
 return FSETIM887960645((FSETIM887960645_frame) frame);
}
void* FSETIM887960645_frame_alloc() {
 return OBALLOC(FSETIM887960645_frame);
}


IMPL_INCLUDE ARRAYI1719886982_unbox(dELTIM1958116958_frame frame) {
 
 return ARRAYI1719886982((ARRAYI1719886982_frame) frame);
}
void* ARRAYI1719886982_frame_alloc() {
 return OBALLOC(ARRAYI1719886982_frame);
}


IMPL_INCLUDE FLISTI381429157_unbox(dELTIM1958116958_frame frame) {
 
 return FLISTI381429157((FLISTI381429157_frame) frame);
}
void* FLISTI381429157_frame_alloc() {
 return OBALLOC(FLISTI381429157_frame);
}


INT ARRAYI69049459_unbox(dELTINT_eltbrINT_frame frame) {
 
 return ARRAYI69049459((ARRAYI69049459_frame) frame);
}
void* ARRAYI69049459_frame_alloc() {
 return OBALLOC(ARRAYI69049459_frame);
}


INT FLISTI476396026_unbox(dELTINT_eltbrINT_frame frame) {
 
 return FLISTI476396026((FLISTI476396026_frame) frame);
}
void* FLISTI476396026_frame_alloc() {
 return OBALLOC(FLISTI476396026_frame);
}


OPT_LOCAL_EXPR ARRAYO335724112_unbox(dELTOP2009500256_frame frame) {
 
 return ARRAYO335724112((ARRAYO335724112_frame) frame);
}
void* ARRAYO335724112_frame_alloc() {
 return OBALLOC(ARRAYO335724112_frame);
}


OPT_LOCAL_EXPR FLISTO2008856511_unbox(dELTOP2009500256_frame frame) {
 
 return FLISTO2008856511((FLISTO2008856511_frame) frame);
}
void* FLISTO2008856511_frame_alloc() {
 return OBALLOC(FLISTO2008856511_frame);
}


SFILE_ID ARRAYS1372094358_unbox(dELTSF338070590_frame frame) {
 
 return ARRAYS1372094358((ARRAYS1372094358_frame) frame);
}
void* ARRAYS1372094358_frame_alloc() {
 return OBALLOC(ARRAYS1372094358_frame);
}


SFILE_ID FLISTS2131268565_unbox(dELTSF338070590_frame frame) {
 
 return FLISTS2131268565((FLISTS2131268565_frame) frame);
}
void* FLISTS2131268565_frame_alloc() {
 return OBALLOC(FLISTS2131268565_frame);
}


SFILE_ID FSETSF139710295_unbox(dELTSF338070590_frame frame) {
 
 return FSETSF139710295((FSETSF139710295_frame) frame);
}
void* FSETSF139710295_frame_alloc() {
 return OBALLOC(FSETSF139710295_frame);
}


SIDE_EFFECT ARRAYS2023743485_unbox(dELTSI583496193_frame frame) {
 
 return ARRAYS2023743485((ARRAYS2023743485_frame) frame);
}
void* ARRAYS2023743485_frame_alloc() {
 return OBALLOC(ARRAYS2023743485_frame);
}


SIDE_EFFECT FLISTS1351415114_unbox(dELTSI583496193_frame frame) {
 
 return FLISTS1351415114((FLISTS1351415114_frame) frame);
}
void* FLISTS1351415114_frame_alloc() {
 return OBALLOC(FLISTS1351415114_frame);
}


SIDE_EFFECT FSETSI2093978094_unbox(dELTSI583496193_frame frame) {
 
 return FSETSI2093978094((FSETSI2093978094_frame) frame);
}
void* FSETSI2093978094_frame_alloc() {
 return OBALLOC(FSETSI2093978094_frame);
}


SIG ARRAYS1481566101_unbox(dELTSIG_eltbrSIG_frame frame) {
 
 return ARRAYS1481566101((ARRAYS1481566101_frame) frame);
}
void* ARRAYS1481566101_frame_alloc() {
 return OBALLOC(ARRAYS1481566101_frame);
}


SIG FLISTS1074219534_unbox(dELTSIG_eltbrSIG_frame frame) {
 
 return FLISTS1074219534((FLISTS1074219534_frame) frame);
}
void* FLISTS1074219534_frame_alloc() {
 return OBALLOC(FLISTS1074219534_frame);
}


SIG FSETSIG_eltbrSIG_unbox(dELTSIG_eltbrSIG_frame frame) {
 
 return FSETSIG_eltbrSIG((FSETSIG_eltbrSIG_frame) frame);
}
void* FSETSIG_eltbrSIG_frame_alloc() {
 return OBALLOC(FSETSIG_eltbrSIG_frame);
}


TP_CLASS ARRAYT1851819274_unbox(dELTTP817795506_frame frame) {
 
 return ARRAYT1851819274((ARRAYT1851819274_frame) frame);
}
void* ARRAYT1851819274_frame_alloc() {
 return OBALLOC(ARRAYT1851819274_frame);
}


TP_CLASS FLISTT1683973815_unbox(dELTTP817795506_frame frame) {
 
 return FLISTT1683973815((FLISTT1683973815_frame) frame);
}
void* FLISTT1683973815_frame_alloc() {
 return OBALLOC(FLISTT1683973815_frame);
}


TP_CLASS FSETTP340014621_unbox(dELTTP817795506_frame frame) {
 
 return FSETTP340014621((FSETTP340014621_frame) frame);
}
void* FSETTP340014621_frame_alloc() {
 return OBALLOC(FSETTP340014621_frame);
}


TP_LAYOUT ARRAYT1372201847_unbox(dELTTP992626963_frame frame) {
 
 return ARRAYT1372201847((ARRAYT1372201847_frame) frame);
}
void* ARRAYT1372201847_frame_alloc() {
 return OBALLOC(ARRAYT1372201847_frame);
}


TP_LAYOUT FLISTT2106747232_unbox(dELTTP992626963_frame frame) {
 
 return FLISTT2106747232((FLISTT2106747232_frame) frame);
}
void* FLISTT2106747232_frame_alloc() {
 return OBALLOC(FLISTT2106747232_frame);
}


TUPdOBNAMESPACE ARRAYT681600609_unbox(dELTTU164160693_frame frame) {
 
 return ARRAYT681600609((ARRAYT681600609_frame) frame);
}
void* ARRAYT681600609_frame_alloc() {
 return OBALLOC(ARRAYT681600609_frame);
}


TUPdOBNAMESPACE FLISTT396553018_unbox(dELTTU164160693_frame frame) {
 
 return FLISTT396553018((FLISTT396553018_frame) frame);
}
void* FLISTT396553018_frame_alloc() {
 return OBALLOC(FLISTT396553018_frame);
}


TUPdOBSTR ARRAYT595865513_unbox(dELTTU975440397_frame frame) {
 
 return ARRAYT595865513((ARRAYT595865513_frame) frame);
}
void* ARRAYT595865513_frame_alloc() {
 return OBALLOC(ARRAYT595865513_frame);
}


TUPdOBSTR FLISTT220152704_unbox(dELTTU975440397_frame frame) {
 
 return FLISTT220152704((FLISTT220152704_frame) frame);
}
void* FLISTT220152704_frame_alloc() {
 return OBALLOC(FLISTT220152704_frame);
}


TUPdTPdLAYOUT ARRAYT2024950707_unbox(dELTTU1923274911_frame frame) {
 
 return ARRAYT2024950707((ARRAYT2024950707_frame) frame);
}
void* ARRAYT2024950707_frame_alloc() {
 return OBALLOC(ARRAYT2024950707_frame);
}


TUPdTPdLAYOUT FLISTT123078916_unbox(dELTTU1923274911_frame frame) {
 
 return FLISTT123078916((FLISTT123078916_frame) frame);
}
void* FLISTT123078916_frame_alloc() {
 return OBALLOC(FLISTT123078916_frame);
}


TUPdTPCODE_FILE ARRAYT1283478039_unbox(dELTTU1800917955_frame frame) {
 
 return ARRAYT1283478039((ARRAYT1283478039_frame) frame);
}
void* ARRAYT1283478039_frame_alloc() {
 return OBALLOC(ARRAYT1283478039_frame);
}


TUPdTPCODE_FILE FLISTT1568525630_unbox(dELTTU1800917955_frame frame) {
 
 return FLISTT1568525630((FLISTT1568525630_frame) frame);
}
void* FLISTT1568525630_frame_alloc() {
 return OBALLOC(FLISTT1568525630_frame);
}


TUPdTP1710233051 ARRAYT1505580189_unbox(dELTTU1084171889_frame frame) {
 
 return ARRAYT1505580189((ARRAYT1505580189_frame) frame);
}
void* ARRAYT1505580189_frame_alloc() {
 return OBALLOC(ARRAYT1505580189_frame);
}


TUPdTP1710233051 FLISTT1760042796_unbox(dELTTU1084171889_frame frame) {
 
 return FLISTT1760042796((FLISTT1760042796_frame) frame);
}
void* FLISTT1760042796_frame_alloc() {
 return OBALLOC(FLISTT1760042796_frame);
}


TUPdTPINT ARRAYT452391291_unbox(dELTTU72816407_frame frame) {
 
 return ARRAYT452391291((ARRAYT452391291_frame) frame);
}
void* ARRAYT452391291_frame_alloc() {
 return OBALLOC(ARRAYT452391291_frame);
}


TUPdTPINT FLISTT1268409508_unbox(dELTTU72816407_frame frame) {
 
 return FLISTT1268409508((FLISTT1268409508_frame) frame);
}
void* FLISTT1268409508_frame_alloc() {
 return OBALLOC(FLISTT1268409508_frame);
}


TUPAM_304458649 ARRAYT310960754_unbox(dELTTU1697593574_frame frame) {
 
 return ARRAYT310960754((ARRAYT310960754_frame) frame);
}
void* ARRAYT310960754_frame_alloc() {
 return OBALLOC(ARRAYT310960754_frame);
}


TUPAM_304458649 FLISTT726119853_unbox(dELTTU1697593574_frame frame) {
 
 return FLISTT726119853((FLISTT726119853_frame) frame);
}
void* FLISTT726119853_frame_alloc() {
 return OBALLOC(FLISTT726119853_frame);
}


TUPAM_667892060 ARRAYT2108331328_unbox(dELTTU202706072_frame frame) {
 
 return ARRAYT2108331328((ARRAYT2108331328_frame) frame);
}
void* ARRAYT2108331328_frame_alloc() {
 return OBALLOC(ARRAYT2108331328_frame);
}


TUPAM_667892060 FLISTT193552257_unbox(dELTTU202706072_frame frame) {
 
 return FLISTT193552257((FLISTT193552257_frame) frame);
}
void* FLISTT193552257_frame_alloc() {
 return OBALLOC(FLISTT193552257_frame);
}


TUPAM_927437481 ARRAYT1360634798_unbox(dELTTU538545734_frame frame) {
 
 return ARRAYT1360634798((ARRAYT1360634798_frame) frame);
}
void* ARRAYT1360634798_frame_alloc() {
 return OBALLOC(ARRAYT1360634798_frame);
}


TUPAM_927437481 FLISTT709128269_unbox(dELTTU538545734_frame frame) {
 
 return FLISTT709128269((FLISTT709128269_frame) frame);
}
void* FLISTT709128269_frame_alloc() {
 return OBALLOC(FLISTT709128269_frame);
}


TUPAM_1519040877 ARRAYT684852235_unbox(dELTTU1106260535_frame frame) {
 
 return ARRAYT684852235((ARRAYT684852235_frame) frame);
}
void* ARRAYT684852235_frame_alloc() {
 return OBALLOC(ARRAYT684852235_frame);
}


TUPAM_1519040877 FLISTT344492076_unbox(dELTTU1106260535_frame frame) {
 
 return FLISTT344492076((FLISTT344492076_frame) frame);
}
void* FLISTT344492076_frame_alloc() {
 return OBALLOC(FLISTT344492076_frame);
}


TUPAM_504653531 ARRAYT342187846_unbox(dELTTU2146728406_frame frame) {
 
 return ARRAYT342187846((ARRAYT342187846_frame) frame);
}
void* ARRAYT342187846_frame_alloc() {
 return OBALLOC(ARRAYT342187846_frame);
}


TUPAM_504653531 FLISTT1883920969_unbox(dELTTU2146728406_frame frame) {
 
 return FLISTT1883920969((FLISTT1883920969_frame) frame);
}
void* FLISTT1883920969_frame_alloc() {
 return OBALLOC(FLISTT1883920969_frame);
}


TUPAM_1024994801 ARRAYT1758179858_unbox(dELTTU225750038_frame frame) {
 
 return ARRAYT1758179858((ARRAYT1758179858_frame) frame);
}
void* ARRAYT1758179858_frame_alloc() {
 return OBALLOC(ARRAYT1758179858_frame);
}


TUPAM_1024994801 FLISTT234903853_unbox(dELTTU225750038_frame frame) {
 
 return FLISTT234903853((FLISTT234903853_frame) frame);
}
void* FLISTT234903853_frame_alloc() {
 return OBALLOC(FLISTT234903853_frame);
}


TUPAM_653469574 ARRAYT1101967660_unbox(dELTTU1108719580_frame frame) {
 
 return ARRAYT1101967660((ARRAYT1101967660_frame) frame);
}
void* ARRAYT1101967660_frame_alloc() {
 return OBALLOC(ARRAYT1101967660_frame);
}


TUPAM_653469574 FLISTT288887925_unbox(dELTTU1108719580_frame frame) {
 
 return FLISTT288887925((FLISTT288887925_frame) frame);
}
void* FLISTT288887925_frame_alloc() {
 return OBALLOC(FLISTT288887925_frame);
}


TUPIDENTdTP ARRAYT849390229_unbox(dELTTU590857063_frame frame) {
 
 return ARRAYT849390229((ARRAYT849390229_frame) frame);
}
void* ARRAYT849390229_frame_alloc() {
 return OBALLOC(ARRAYT849390229_frame);
}


TUPIDENTdTP FLISTT1769198926_unbox(dELTTU590857063_frame frame) {
 
 return FLISTT1769198926((FLISTT1769198926_frame) frame);
}
void* FLISTT1769198926_frame_alloc() {
 return OBALLOC(FLISTT1769198926_frame);
}


TUPIDE641654124 ARRAYT451883912_unbox(dELTTU577707672_frame frame) {
 
 return ARRAYT451883912((ARRAYT451883912_frame) frame);
}
void* ARRAYT451883912_frame_alloc() {
 return OBALLOC(ARRAYT451883912_frame);
}


TUPIDE641654124 FLISTT1472123735_unbox(dELTTU577707672_frame frame) {
 
 return FLISTT1472123735((FLISTT1472123735_frame) frame);
}
void* FLISTT1472123735_frame_alloc() {
 return OBALLOC(FLISTT1472123735_frame);
}


TUPIDENTINT ARRAYT1017446695_unbox(dELTTU1837273309_frame frame) {
 
 return ARRAYT1017446695((ARRAYT1017446695_frame) frame);
}
void* ARRAYT1017446695_frame_alloc() {
 return OBALLOC(ARRAYT1017446695_frame);
}


TUPIDENTINT FLISTT97637998_unbox(dELTTU1837273309_frame frame) {
 
 return FLISTT97637998((FLISTT97637998_frame) frame);
}
void* FLISTT97637998_frame_alloc() {
 return OBALLOC(FLISTT97637998_frame);
}


TUPIDENTINT FSETTU947212086_unbox(dELTTU1837273309_frame frame) {
 
 return FSETTU947212086((FSETTU947212086_frame) frame);
}
void* FSETTU947212086_frame_alloc() {
 return OBALLOC(FSETTU947212086_frame);
}


TUPSIG1754078736 ARRAYT1079066005_unbox(dELTTU2080726233_frame frame) {
 
 return ARRAYT1079066005((ARRAYT1079066005_frame) frame);
}
void* ARRAYT1079066005_frame_alloc() {
 return OBALLOC(ARRAYT1079066005_frame);
}


TUPSIG1754078736 FLISTT287332146_unbox(dELTTU2080726233_frame frame) {
 
 return FLISTT287332146((FLISTT287332146_frame) frame);
}
void* FLISTT287332146_frame_alloc() {
 return OBALLOC(FLISTT287332146_frame);
}


TUPSIG578692189 ARRAYT1269605207_unbox(dELTTU1031278189_frame frame) {
 
 return ARRAYT1269605207((ARRAYT1269605207_frame) frame);
}
void* ARRAYT1269605207_frame_alloc() {
 return OBALLOC(ARRAYT1269605207_frame);
}


TUPSIG578692189 FLISTT754641600_unbox(dELTTU1031278189_frame frame) {
 
 return FLISTT754641600((FLISTT754641600_frame) frame);
}
void* FLISTT754641600_frame_alloc() {
 return OBALLOC(FLISTT754641600_frame);
}


TUPSIGARG_LAYOUT ARRAYT457901414_unbox(dELTTU1654314914_frame frame) {
 
 return ARRAYT457901414((ARRAYT457901414_frame) frame);
}
void* ARRAYT457901414_frame_alloc() {
 return OBALLOC(ARRAYT457901414_frame);
}


TUPSIGARG_LAYOUT FLISTT634936167_unbox(dELTTU1654314914_frame frame) {
 
 return FLISTT634936167((FLISTT634936167_frame) frame);
}
void* FLISTT634936167_frame_alloc() {
 return OBALLOC(FLISTT634936167_frame);
}


TUPSIG2023533247 ARRAYT532396130_unbox(dELTTU1157822078_frame frame) {
 
 return ARRAYT532396130((ARRAYT532396130_frame) frame);
}
void* ARRAYT532396130_frame_alloc() {
 return OBALLOC(ARRAYT532396130_frame);
}


TUPSIG2023533247 FLISTT685872269_unbox(dELTTU1157822078_frame frame) {
 
 return FLISTT685872269((FLISTT685872269_frame) frame);
}
void* FLISTT685872269_frame_alloc() {
 return OBALLOC(FLISTT685872269_frame);
}


TUPSIGINT ARRAYT1593820953_unbox(dELTTU1973395837_frame frame) {
 
 return ARRAYT1593820953((ARRAYT1593820953_frame) frame);
}
void* ARRAYT1593820953_frame_alloc() {
 return OBALLOC(ARRAYT1593820953_frame);
}


TUPSIGINT FLISTT777802736_unbox(dELTTU1973395837_frame frame) {
 
 return FLISTT777802736((FLISTT777802736_frame) frame);
}
void* FLISTT777802736_frame_alloc() {
 return OBALLOC(FLISTT777802736_frame);
}


TUPSIGSIG ARRAYT1146052097_unbox(dELTTU1525626981_frame frame) {
 
 return ARRAYT1146052097((ARRAYT1146052097_frame) frame);
}
void* ARRAYT1146052097_frame_alloc() {
 return OBALLOC(ARRAYT1146052097_frame);
}


TUPSIGSIG FLISTT330033880_unbox(dELTTU1525626981_frame frame) {
 
 return FLISTT330033880((FLISTT330033880_frame) frame);
}
void* FLISTT330033880_frame_alloc() {
 return OBALLOC(FLISTT330033880_frame);
}


TUPSIGSIG FSETTU845419336_unbox(dELTTU1525626981_frame frame) {
 
 return FSETTU845419336((FSETTU845419336_frame) frame);
}
void* FSETTU845419336_frame_alloc() {
 return OBALLOC(FSETTU845419336_frame);
}


TUPSIGSTR ARRAYT1691893201_unbox(dELTTU2071468085_frame frame) {
 
 return ARRAYT1691893201((ARRAYT1691893201_frame) frame);
}
void* ARRAYT1691893201_frame_alloc() {
 return OBALLOC(ARRAYT1691893201_frame);
}


TUPSIGSTR FLISTT875874984_unbox(dELTTU2071468085_frame frame) {
 
 return FLISTT875874984((FLISTT875874984_frame) frame);
}
void* FLISTT875874984_frame_alloc() {
 return OBALLOC(FLISTT875874984_frame);
}


TUPSTRdCONFIG ARRAYT485937919_unbox(dELTTU384262123_frame frame) {
 
 return ARRAYT485937919((ARRAYT485937919_frame) frame);
}
void* ARRAYT485937919_frame_alloc() {
 return OBALLOC(ARRAYT485937919_frame);
}


TUPSTRdCONFIG FLISTT1662091704_unbox(dELTTU384262123_frame frame) {
 
 return FLISTT1662091704((FLISTT1662091704_frame) frame);
}
void* FLISTT1662091704_frame_alloc() {
 return OBALLOC(FLISTT1662091704_frame);
}


TUPSTRFSETSTR ARRAYT1426073269_unbox(dELTTU1527749065_frame frame) {
 
 return ARRAYT1426073269((ARRAYT1426073269_frame) frame);
}
void* ARRAYT1426073269_frame_alloc() {
 return OBALLOC(ARRAYT1426073269_frame);
}


TUPSTRFSETSTR FLISTT720864404_unbox(dELTTU1527749065_frame frame) {
 
 return FLISTT720864404((FLISTT720864404_frame) frame);
}
void* FLISTT720864404_frame_alloc() {
 return OBALLOC(FLISTT720864404_frame);
}


TUPSTRINT ARRAYT306471177_unbox(dELTTU686046061_frame frame) {
 
 return ARRAYT306471177((ARRAYT306471177_frame) frame);
}
void* ARRAYT306471177_frame_alloc() {
 return OBALLOC(ARRAYT306471177_frame);
}


TUPSTRINT FLISTT509547040_unbox(dELTTU686046061_frame frame) {
 
 return FLISTT509547040((FLISTT509547040_frame) frame);
}
void* FLISTT509547040_frame_alloc() {
 return OBALLOC(FLISTT509547040_frame);
}


TUPSTRSIG ARRAYT141297679_unbox(dELTTU238277205_frame frame) {
 
 return ARRAYT141297679((ARRAYT141297679_frame) frame);
}
void* ARRAYT141297679_frame_alloc() {
 return OBALLOC(ARRAYT141297679_frame);
}


TUPSTRSIG FLISTT957315896_unbox(dELTTU238277205_frame frame) {
 
 return FLISTT957315896((FLISTT957315896_frame) frame);
}
void* FLISTT957315896_frame_alloc() {
 return OBALLOC(FLISTT957315896_frame);
}


TUPSTRSTR ARRAYT404543425_unbox(dELTTU784118309_frame frame) {
 
 return ARRAYT404543425((ARRAYT404543425_frame) frame);
}
void* ARRAYT404543425_frame_alloc() {
 return OBALLOC(ARRAYT404543425_frame);
}


TUPSTRSTR FLISTT411474792_unbox(dELTTU784118309_frame frame) {
 
 return FLISTT411474792((FLISTT411474792_frame) frame);
}
void* FLISTT411474792_frame_alloc() {
 return OBALLOC(FLISTT411474792_frame);
}


TUPTP_858321267 ARRAYT518925702_unbox(dELTTU1171292506_frame frame) {
 
 return ARRAYT518925702((ARRAYT518925702_frame) frame);
}
void* ARRAYT518925702_frame_alloc() {
 return OBALLOC(ARRAYT518925702_frame);
}


TUPTP_858321267 FLISTT699342697_unbox(dELTTU1171292506_frame frame) {
 
 return FLISTT699342697((FLISTT699342697_frame) frame);
}
void* FLISTT699342697_frame_alloc() {
 return OBALLOC(FLISTT699342697_frame);
}


TUPTP_1668454172 ARRAYT1509169201_unbox(dELTTU963535579_frame frame) {
 
 return ARRAYT1509169201((ARRAYT1509169201_frame) frame);
}
void* ARRAYT1509169201_frame_alloc() {
 return OBALLOC(ARRAYT1509169201_frame);
}


TUPTP_1668454172 FLISTT1725186742_unbox(dELTTU963535579_frame frame) {
 
 return FLISTT1725186742((FLISTT1725186742_frame) frame);
}
void* FLISTT1725186742_frame_alloc() {
 return OBALLOC(FLISTT1725186742_frame);
}

