#include "sather.h"

/* Layouts */

typedef struct dAM_CONST_struct {
 OB_HEADER header;
 } *dAM_CONST;

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

typedef struct dELTAM1208385872_struct {
 OB_HEADER header;
 } *dELTAM1208385872;

typedef struct dELTAM937720280_struct {
 OB_HEADER header;
 } *dELTAM937720280;

typedef struct dELTAM938692099_struct {
 OB_HEADER header;
 } *dELTAM938692099;

typedef struct dELTARRAYSTR_struct {
 OB_HEADER header;
 } *dELTARRAYSTR;

typedef struct dELTATTRSORT_struct {
 OB_HEADER header;
 } *dELTATTRSORT;

typedef struct dELTBO2026178203_struct {
 OB_HEADER header;
 } *dELTBO2026178203;

typedef struct dELTCODE_FILE_struct {
 OB_HEADER header;
 } *dELTCODE_FILE;

typedef struct dELTEX2061780774_struct {
 OB_HEADER header;
 } *dELTEX2061780774;

typedef struct dELTFLISTSTR_struct {
 OB_HEADER header;
 } *dELTFLISTSTR;

typedef struct dELTOP237467449_struct {
 OB_HEADER header;
 } *dELTOP237467449;

typedef struct dELTSFILE_ID_struct {
 OB_HEADER header;
 } *dELTSFILE_ID;

typedef struct dELTTU1982974052_struct {
 OB_HEADER header;
 } *dELTTU1982974052;

typedef struct dELTTU1449135940_struct {
 OB_HEADER header;
 } *dELTTU1449135940;

typedef struct dELTTU1411620812_struct {
 OB_HEADER header;
 } *dELTTU1411620812;

typedef struct dELTTU216883428_struct {
 OB_HEADER header;
 } *dELTTU216883428;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

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

typedef struct ELT_ALGdAM_CONST_struct {/* layout for ELT_ALG{$AM_CONST} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGdAM_CONST;

typedef struct ELT_AL138844101_struct {/* layout for ELT_ALG{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL138844101;

typedef struct ELT_AL1403631219_struct {/* layout for ELT_ALG{AM_FORMAL_ARG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1403631219;

typedef struct ELT_AL911461552_struct {/* layout for ELT_ALG{AM_ITER_CALL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL911461552;

typedef struct ELT_ALGARRAYSTR_struct {/* layout for ELT_ALG{ARRAY{STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGARRAYSTR;

typedef struct ELT_ALGATTRSORT_struct {/* layout for ELT_ALG{ATTRSORT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGATTRSORT;

typedef struct ELT_AL1115246319_struct {/* layout for ELT_ALG{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1115246319;

typedef struct ELT_ALGCODE_FILE_struct {/* layout for ELT_ALG{CODE_FILE} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGCODE_FILE;

typedef struct ELT_AL1543060947_struct {/* layout for ELT_ALG{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1543060947;

typedef struct ELT_ALGFLISTSTR_struct {/* layout for ELT_ALG{FLIST{STR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGFLISTSTR;

typedef struct ELT_AL499840755_struct {/* layout for ELT_ALG{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL499840755;

typedef struct ELT_ALGSFILE_ID_struct {/* layout for ELT_ALG{SFILE_ID} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGSFILE_ID;

typedef struct ELT_AL583594762_struct {/* layout for ELT_ALG{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL583594762;

typedef struct ELT_AL1476366487_struct {/* layout for ELT_ALG{TUP{SIG,AM_ROUT_DEF}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1476366487;

typedef struct ELT_AL1639104906_struct {/* layout for ELT_ALG{TUP{SIG,ARG_LAYOUT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1639104906;

typedef struct ELT_AL734263878_struct {/* layout for ELT_ALG{TUP{TP_CLASS,FSET{$TP}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL734263878;

typedef struct EXTERNAL_LAYOUT_struct {/* layout for EXTERNAL_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct PROG_struct *prog;
 STR str;
 } *EXTERNAL_LAYOUT;

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

typedef struct TUPAM_504653531_struct {/* layout for TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct OPT_LOCAL_EXPR_struct *t2;
 } TUPAM_504653531;
static TUPAM_504653531 TUPAM_504653531_zero;

typedef struct TUPAM_504653531_boxed_struct {
 OB_HEADER header;
 TUPAM_504653531 immutable_part;
 } *TUPAM_504653531_boxed;

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

typedef struct TUPTP_858321267_struct {/* layout for TUP{TP_CLASS,FSET{$TP}} */
 struct FSETdTP_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_858321267;
static TUPTP_858321267 TUPTP_858321267_zero;

typedef struct TUPTP_858321267_boxed_struct {
 OB_HEADER header;
 TUPTP_858321267 immutable_part;
 } *TUPTP_858321267_boxed;

typedef struct dELTAM1417430129_frame_struct {
 INT state;
 dELTAM937720280 self;
 } *dELTAM1417430129_frame;

/* Dispatch table entry for $ELT{AM_FORMAL_ARG}::elt!:AM_FORMAL_ARG */
typedef struct dELTAM1417430129_entry {
const AM_FORMAL_ARG (*iter)(dELTAM1417430129_frame frame);
const void* (*alloc_frame)();
} dELTAM1417430129_entry;

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

typedef struct dELTBO1349583384_frame_struct {
 INT state;
 dELTBO2026178203 self;
 } *dELTBO1349583384_frame;

/* Dispatch table entry for $ELT{BOUND_ITER_TYPE_LAYOUT}::elt!:BOUND_ITER_TYPE_LAYOUT */
typedef struct dELTBO1349583384_entry {
const BOUND_2140373124 (*iter)(dELTBO1349583384_frame frame);
const void* (*alloc_frame)();
} dELTBO1349583384_entry;

typedef struct dELTEX2090504649_frame_struct {
 INT state;
 dELTEX2061780774 self;
 } *dELTEX2090504649_frame;

/* Dispatch table entry for $ELT{EXTERNAL_LAYOUT}::elt!:EXTERNAL_LAYOUT */
typedef struct dELTEX2090504649_entry {
const EXTERNAL_LAYOUT (*iter)(dELTEX2090504649_frame frame);
const void* (*alloc_frame)();
} dELTEX2090504649_entry;

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

typedef struct dELTTU2146728406_frame_struct {
 INT state;
 dELTTU1982974052 self;
 } *dELTTU2146728406_frame;

/* Dispatch table entry for $ELT{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}}::elt!:TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
typedef struct dELTTU2146728406_entry {
const TUPAM_504653531 (*iter)(dELTTU2146728406_frame frame);
const void* (*alloc_frame)();
} dELTTU2146728406_entry;

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

typedef struct dELTTU1171292506_frame_struct {
 INT state;
 dELTTU216883428 self;
 } *dELTTU1171292506_frame;

/* Dispatch table entry for $ELT{TUP{TP_CLASS,FSET{$TP}}}::elt!:TUP{TP_CLASS,FSET{$TP}} */
typedef struct dELTTU1171292506_entry {
const TUPTP_858321267 (*iter)(dELTTU1171292506_frame frame);
const void* (*alloc_frame)();
} dELTTU1171292506_entry;

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

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct dELTAM157362729_frame_struct {
 INT state;
 dELTAM1208385872 self;
 } *dELTAM157362729_frame;

/* Dispatch table entry for $ELT{AM_BND_ITER_CALL_EXPR}::elt!:AM_BND_ITER_CALL_EXPR */
typedef struct dELTAM157362729_entry {
const AM_BND367211769 (*iter)(dELTAM157362729_frame frame);
const void* (*alloc_frame)();
} dELTAM157362729_entry;

typedef struct dELTAM1806540817_frame_struct {
 INT state;
 dELTAM938692099 self;
 } *dELTAM1806540817_frame;

/* Dispatch table entry for $ELT{AM_ITER_CALL_EXPR}::elt!:AM_ITER_CALL_EXPR */
typedef struct dELTAM1806540817_entry {
const AM_ITE1809135850 (*iter)(dELTAM1806540817_frame frame);
const void* (*alloc_frame)();
} dELTAM1806540817_entry;

typedef struct dELTCO1151854791_frame_struct {
 INT state;
 dELTCODE_FILE self;
 } *dELTCO1151854791_frame;

/* Dispatch table entry for $ELT{CODE_FILE}::elt!:CODE_FILE */
typedef struct dELTCO1151854791_entry {
const CODE_FILE (*iter)(dELTCO1151854791_frame frame);
const void* (*alloc_frame)();
} dELTCO1151854791_entry;

typedef struct dELTFL27095446_frame_struct {
 INT state;
 dELTFLISTSTR self;
 } *dELTFL27095446_frame;

/* Dispatch table entry for $ELT{FLIST{STR}}::elt!:FLIST{STR} */
typedef struct dELTFL27095446_entry {
const FLISTSTR (*iter)(dELTFL27095446_frame frame);
const void* (*alloc_frame)();
} dELTFL27095446_entry;

#include "tags.h"

/* Globals */


/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dELTAM1417430129_entry *dELTAM1417430129;

extern RETURNED_CONST dELTAM157362729_entry *dELTAM157362729;

extern RETURNED_CONST dELTAM1806540817_entry *dELTAM1806540817;

extern RETURNED_CONST dELTAR1531173752_entry *dELTAR1531173752;

extern RETURNED_CONST dELTAT1600626566_entry *dELTAT1600626566;

extern RETURNED_CONST dELTBO1349583384_entry *dELTBO1349583384;

extern RETURNED_CONST dELTCO1151854791_entry *dELTCO1151854791;

extern RETURNED_CONST dELTEX2090504649_entry *dELTEX2090504649;

extern RETURNED_CONST dELTFL27095446_entry *dELTFL27095446;

extern RETURNED_CONST dELTOP2009500256_entry *dELTOP2009500256;

extern RETURNED_CONST dELTSF338070590_entry *dELTSF338070590;

extern RETURNED_CONST dELTTU1031278189_entry *dELTTU1031278189;

extern RETURNED_CONST dELTTU1171292506_entry *dELTTU1171292506;

extern RETURNED_CONST dELTTU1654314914_entry *dELTTU1654314914;

extern RETURNED_CONST dELTTU2146728406_entry *dELTTU2146728406;

extern RETURNED_CONST dELTdA578383841_entry *dELTdA578383841;
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
STR ELT_AL1011073298(ELT_AL1476366487, OB, INT);
STR ELT_AL1024390623(ELT_ALGARRAYSTR, dELTARRAYSTR);
STR ELT_AL1053727475(ELT_AL1639104906, dELTTU1411620812);
STR ELT_AL1083506231(ELT_AL583594762, dELTTU1982974052);
STR ELT_AL1155897002(ELT_AL1403631219, OB, INT);
STR ELT_AL1184516335(ELT_ALGCODE_FILE, OB, INT);
STR ELT_AL1236387857(ELT_ALGATTRSORT, OB, INT);
STR ELT_AL130847997(ELT_AL1115246319, dELTBO2026178203);
STR ELT_AL1351346800(ELT_AL138844101, dELTAM1208385872);
STR ELT_AL1416603131(ELT_AL734263878, OB, INT);
STR ELT_AL1417078085(ELT_AL499840755, dELTOP237467449);
STR ELT_AL1425453107(ELT_ALGSFILE_ID, dELTSFILE_ID);
STR ELT_AL1455618083(ELT_ALGATTRSORT, dELTATTRSORT);
STR ELT_AL1512359581(ELT_ALGSFILE_ID, OB, INT);
STR ELT_AL1518957794(ELT_AL499840755, OB, INT);
STR ELT_AL1536396638(ELT_AL1115246319, OB, INT);
STR ELT_AL1787578174(ELT_ALGARRAYSTR, OB, INT);
STR ELT_AL1818663869(ELT_AL734263878, dELTTU216883428);
STR ELT_AL1906929438(ELT_AL911461552, dELTAM938692099);
STR ELT_AL2043762773(ELT_AL583594762, OB, INT);
STR ELT_AL2074944584(ELT_AL1476366487, dELTTU1449135940);
STR ELT_AL519275506(ELT_ALGCODE_FILE, dELTCODE_FILE);
STR ELT_AL550997356(ELT_AL1543060947, dELTEX2061780774);
STR ELT_AL596810930(ELT_AL138844101, OB, INT);
STR ELT_AL60905703(ELT_AL1639104906, OB, INT);
STR ELT_AL656299508(ELT_AL1543060947, OB, INT);
STR ELT_AL75701225(ELT_ALGdAM_CONST, OB, INT);
STR ELT_AL769058240(ELT_AL1403631219, dELTAM937720280);
STR ELT_AL854472091(ELT_ALGFLISTSTR, dELTFLISTSTR);
STR ELT_AL877461897(ELT_AL911461552, OB, INT);
STR ELT_AL918382858(ELT_ALGdAM_CONST, dELTdAM_CONST);
STR ELT_AL967981077(ELT_ALGFLISTSTR, OB, INT);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1011073298(ELT_AL1476366487 self, OB e, INT i) {
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
  goto other77;
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
  other77: ;
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

STR ELT_AL1024390623(ELT_ALGARRAYSTR self, dELTARRAYSTR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTARRAYSTR L11;
 ARRAYSTR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTARRAYSTR L2;
 ARRAYSTR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAR1531173752_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAR1531173752_frame) (*dELTAR1531173752[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAR1531173752[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1787578174(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1787578174(self, ((OB) e), i));
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

STR ELT_AL1053727475(ELT_AL1639104906 self, dELTTU1411620812 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1411620812 L11;
 TUPSIGARG_LAYOUT e = TUPSIGARG_LAYOUT_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1411620812 L2;
 TUPSIGARG_LAYOUT L3;
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
  
  dELTTU1654314914_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1654314914_frame) (*dELTTU1654314914[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1654314914[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSIGARG_LAYOUT_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSIGARG_LAYOUT_tag;
    L6 = (OB)((TUPSIGARG_LAYOUT_boxed) L7);
    ((TUPSIGARG_LAYOUT_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL60905703(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSIGARG_LAYOUT_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSIGARG_LAYOUT_tag;
    L8 = (OB)((TUPSIGARG_LAYOUT_boxed) L9);
    ((TUPSIGARG_LAYOUT_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL60905703(self, L8, i));
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

STR ELT_AL1083506231(ELT_AL583594762 self, dELTTU1982974052 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1982974052 L11;
 TUPAM_504653531 e = TUPAM_504653531_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1982974052 L2;
 TUPAM_504653531 L3;
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
  
  dELTTU2146728406_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU2146728406_frame) (*dELTTU2146728406[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU2146728406[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPAM_504653531_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPAM_504653531_tag;
    L6 = (OB)((TUPAM_504653531_boxed) L7);
    ((TUPAM_504653531_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL2043762773(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPAM_504653531_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPAM_504653531_tag;
    L8 = (OB)((TUPAM_504653531_boxed) L9);
    ((TUPAM_504653531_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL2043762773(self, L8, i));
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

STR ELT_AL1155897002(ELT_AL1403631219 self, OB e, INT i) {
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
  goto other25;
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
  other25: ;
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

STR ELT_AL1184516335(ELT_ALGCODE_FILE self, OB e, INT i) {
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
  goto other42;
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
  other42: ;
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

STR ELT_AL1236387857(ELT_ALGATTRSORT self, OB e, INT i) {
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
  goto other34;
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
  other34: ;
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

STR ELT_AL130847997(ELT_AL1115246319 self, dELTBO2026178203 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTBO2026178203 L11;
 BOUND_2140373124 e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTBO2026178203 L2;
 BOUND_2140373124 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTBO1349583384_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTBO1349583384_frame) (*dELTBO1349583384[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTBO1349583384[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1536396638(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1536396638(self, ((OB) e), i));
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

STR ELT_AL1351346800(ELT_AL138844101 self, dELTAM1208385872 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM1208385872 L11;
 AM_BND367211769 e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM1208385872 L2;
 AM_BND367211769 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM157362729_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM157362729_frame) (*dELTAM157362729[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM157362729[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL596810930(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL596810930(self, ((OB) e), i));
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

STR ELT_AL1416603131(ELT_AL734263878 self, OB e, INT i) {
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
  goto other88;
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
  other88: ;
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

STR ELT_AL1417078085(ELT_AL499840755 self, dELTOP237467449 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTOP237467449 L11;
 OPT_LOCAL_EXPR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTOP237467449 L2;
 OPT_LOCAL_EXPR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTOP2009500256_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTOP2009500256_frame) (*dELTOP2009500256[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTOP2009500256[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1518957794(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1518957794(self, ((OB) e), i));
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

STR ELT_AL1425453107(ELT_ALGSFILE_ID self, dELTSFILE_ID arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTSFILE_ID L11;
 SFILE_ID e = SFILE_ID_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTSFILE_ID L2;
 SFILE_ID L3;
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
  
  dELTSF338070590_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTSF338070590_frame) (*dELTSF338070590[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTSF338070590[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC_LEAF(sizeof(struct SFILE_ID_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    memset(L7,0,sizeof(struct SFILE_ID_boxed_struct));
    ((OB)L7)->header.tag=SFILE_ID_tag;
    L6 = (OB)((SFILE_ID_boxed) L7);
    ((SFILE_ID_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1512359581(self, L6, i));
   }
   else {
    L9=ZALLOC_LEAF(sizeof(struct SFILE_ID_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    memset(L9,0,sizeof(struct SFILE_ID_boxed_struct));
    ((OB)L9)->header.tag=SFILE_ID_tag;
    L8 = (OB)((SFILE_ID_boxed) L9);
    ((SFILE_ID_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1512359581(self, L8, i));
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

STR ELT_AL1455618083(ELT_ALGATTRSORT self, dELTATTRSORT arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTATTRSORT L11;
 ATTRSORT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTATTRSORT L2;
 ATTRSORT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAT1600626566_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAT1600626566_frame) (*dELTAT1600626566[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAT1600626566[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1236387857(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1236387857(self, ((OB) e), i));
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

STR ELT_AL1512359581(ELT_ALGSFILE_ID self, OB e, INT i) {
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
  goto other55;
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
  other55: ;
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

STR ELT_AL1518957794(ELT_AL499840755 self, OB e, INT i) {
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
  goto other54;
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
  other54: ;
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

STR ELT_AL1536396638(ELT_AL1115246319 self, OB e, INT i) {
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
  goto other36;
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
  other36: ;
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

STR ELT_AL1787578174(ELT_ALGARRAYSTR self, OB e, INT i) {
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
  goto other32;
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
  other32: ;
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

STR ELT_AL1818663869(ELT_AL734263878 self, dELTTU216883428 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU216883428 L11;
 TUPTP_858321267 e = TUPTP_858321267_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU216883428 L2;
 TUPTP_858321267 L3;
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
  
  dELTTU1171292506_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1171292506_frame) (*dELTTU1171292506[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1171292506[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPTP_858321267_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPTP_858321267_tag;
    L6 = (OB)((TUPTP_858321267_boxed) L7);
    ((TUPTP_858321267_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1416603131(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPTP_858321267_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPTP_858321267_tag;
    L8 = (OB)((TUPTP_858321267_boxed) L9);
    ((TUPTP_858321267_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1416603131(self, L8, i));
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

STR ELT_AL1906929438(ELT_AL911461552 self, dELTAM938692099 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM938692099 L11;
 AM_ITE1809135850 e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM938692099 L2;
 AM_ITE1809135850 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM1806540817_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM1806540817_frame) (*dELTAM1806540817[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM1806540817[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL877461897(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL877461897(self, ((OB) e), i));
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

STR ELT_AL2043762773(ELT_AL583594762 self, OB e, INT i) {
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
  goto other70;
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
  other70: ;
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

STR ELT_AL2074944584(ELT_AL1476366487 self, dELTTU1449135940 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU1449135940 L11;
 TUPSIG578692189 e = TUPSIG578692189_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU1449135940 L2;
 TUPSIG578692189 L3;
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
  
  dELTTU1031278189_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU1031278189_frame) (*dELTTU1031278189[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU1031278189[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPSIG578692189_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPSIG578692189_tag;
    L6 = (OB)((TUPSIG578692189_boxed) L7);
    ((TUPSIG578692189_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL1011073298(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPSIG578692189_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPSIG578692189_tag;
    L8 = (OB)((TUPSIG578692189_boxed) L9);
    ((TUPSIG578692189_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL1011073298(self, L8, i));
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

STR ELT_AL519275506(ELT_ALGCODE_FILE self, dELTCODE_FILE arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTCODE_FILE L11;
 CODE_FILE e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTCODE_FILE L2;
 CODE_FILE L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTCO1151854791_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTCO1151854791_frame) (*dELTCO1151854791[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTCO1151854791[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1184516335(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1184516335(self, ((OB) e), i));
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

STR ELT_AL550997356(ELT_AL1543060947 self, dELTEX2061780774 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTEX2061780774 L11;
 EXTERNAL_LAYOUT e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTEX2061780774 L2;
 EXTERNAL_LAYOUT L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTEX2090504649_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTEX2090504649_frame) (*dELTEX2090504649[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTEX2090504649[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL656299508(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL656299508(self, ((OB) e), i));
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

STR ELT_AL596810930(ELT_AL138844101 self, OB e, INT i) {
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
  goto other12;
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
  other12: ;
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

STR ELT_AL60905703(ELT_AL1639104906 self, OB e, INT i) {
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
  goto other78;
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
  other78: ;
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

STR ELT_AL656299508(ELT_AL1543060947 self, OB e, INT i) {
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
  goto other45;
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
  other45: ;
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

STR ELT_AL75701225(ELT_ALGdAM_CONST self, OB e, INT i) {
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
  goto other6;
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
  other6: ;
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

STR ELT_AL769058240(ELT_AL1403631219 self, dELTAM937720280 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTAM937720280 L11;
 AM_FORMAL_ARG e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTAM937720280 L2;
 AM_FORMAL_ARG L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTAM1417430129_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTAM1417430129_frame) (*dELTAM1417430129[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTAM1417430129[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL1155897002(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL1155897002(self, ((OB) e), i));
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

STR ELT_AL854472091(ELT_ALGFLISTSTR self, dELTFLISTSTR arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTFLISTSTR L11;
 FLISTSTR e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTFLISTSTR L2;
 FLISTSTR L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTFL27095446_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTFL27095446_frame) (*dELTFL27095446[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTFL27095446[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL967981077(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL967981077(self, ((OB) e), i));
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

STR ELT_AL877461897(ELT_AL911461552 self, OB e, INT i) {
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
  goto other13;
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
  other13: ;
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

STR ELT_AL918382858(ELT_ALGdAM_CONST self, dELTdAM_CONST arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTdAM_CONST L11;
 dAM_CONST e;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTdAM_CONST L2;
 dAM_CONST L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 INT L61_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTdA578383841_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTdA578383841_frame) (*dELTdA578383841[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTdA578383841[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    res = FSTR_p1752847026(L5, ELT_AL75701225(self, ((OB) e), i));
   }
   else {
    res = FSTR_p1752847026(res, ELT_AL75701225(self, ((OB) e), i));
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

STR ELT_AL967981077(ELT_ALGFLISTSTR self, OB e, INT i) {
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
  goto other47;
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
  other47: ;
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
