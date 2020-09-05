#include "sather.h"

/* Layouts */

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_EQdTP_struct {/* layout for ELT_EQ{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdTP;

typedef struct ELT_EQELT_struct {/* layout for ELT_EQ{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQELT;

typedef struct ELT_EQIDENT_struct {/* layout for ELT_EQ{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQIDENT;

typedef struct ELT_EQSIG_struct {/* layout for ELT_EQ{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQSIG;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NILdTP_struct {/* layout for ELT_NIL{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdTP;

typedef struct ELT_NILELT_struct {/* layout for ELT_NIL{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILELT;

typedef struct ELT_NILIDENT_struct {/* layout for ELT_NIL{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILIDENT;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

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

typedef struct SIG_TB1663296138_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 SIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *SIG_TB1663296138_frame;

typedef struct SIG_TB2081225820_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 SIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *SIG_TB2081225820_frame;

typedef struct SIG_TBL_eltbrSIG_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 SIG ret_val2;
 SIG_TBL L61;
 SIG r;
 INT L31;
 SIG_TBL is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *SIG_TBL_eltbrSIG_frame;

typedef struct SIG_TB788365833_frame_struct {
 INT state;
 SIG_TBL self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: q */
 SIG ret_val6;
 INT h;
 SIG e;
 SIG e1;
 SIG_TBL query_hash_self;
 IDENT query_hash_i;
 INT ret_val;
 IDENT hash_self;
 INT ret_val1;
 INT x;
 SIG_TBL is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val3;
 SIG_TBL is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val4;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val5;
 } *SIG_TB788365833_frame;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct TUPIDE641654124_struct {/* layout for TUP{IDENT,AM_SHARED_EXPR} */
 struct AM_SHARED_EXPR_struct *t2;
 struct IDENT_struct t1;
 } TUPIDE641654124;
static TUPIDE641654124 TUPIDE641654124_zero;

typedef struct TUPIDE641654124_boxed_struct {
 OB_HEADER header;
 TUPIDE641654124 immutable_part;
 } *TUPIDE641654124_boxed;

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

typedef struct ARRAYS1161048019_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1161048019_frame;

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

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct ELT_TB799962086_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 ELT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ELT_TB799962086_frame;

typedef struct ELT_TB382032404_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 ELT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ELT_TB382032404_frame;

typedef struct ELT_TBL_eltbrELT_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 ELT ret_val2;
 ELT_TBL L61;
 ELT r;
 INT L31;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *ELT_TBL_eltbrELT_frame;

typedef struct ELT_TB431570775_frame_struct {
 INT state;
 ELT_TBL self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: q */
 ELT ret_val6;
 INT h;
 ELT e;
 ELT e1;
 ELT_TBL query_hash_self;
 IDENT query_hash_i;
 INT ret_val;
 IDENT hash_self;
 INT ret_val1;
 INT x;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val2;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val3;
 ELT_TBL is_elt_nil_self2;
 ELT is_elt_nil_e2;
 BOOL ret_val4;
 ELT_NILELT is_elt_nil_self3;
 ELT is_elt_nil_e3;
 BOOL ret_val5;
 } *ELT_TB431570775_frame;

typedef struct FMAPdTPdLAYOUT_struct {/* layout for FMAP{$TP,$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FMAPdTPdLAYOUT;

typedef struct FMAPdT1924387821_frame_struct {
 INT state;
 FMAPdTPdLAYOUT self;/* Formal argument: self */
 TUPdTPdLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT1924387821_frame;

typedef struct FMAPdT963530613_frame_struct {
 INT state;
 FMAPdTPdLAYOUT self;/* Formal argument: self */
 TUPdTPdLAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT963530613_frame;

typedef struct FMAPdT2027173596_frame_struct {
 INT state;
 FMAPdTPdLAYOUT self;/* Formal argument: self */
 TUPdTPdLAYOUT ret_val1;
 FMAPdTPdLAYOUT L61;
 TUPdTPdLAYOUT r;
 INT L31;
 FMAPdTPdLAYOUT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT2027173596_frame;

typedef struct FMAPdTPCODE_FILE_struct {/* layout for FMAP{$TP,CODE_FILE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPCODE_FILE_struct arr_part[1];
 } *FMAPdTPCODE_FILE;

typedef struct FMAPdT669571245_frame_struct {
 INT state;
 FMAPdTPCODE_FILE self;/* Formal argument: self */
 TUPdTPCODE_FILE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT669571245_frame;

typedef struct FMAPdT468927009_frame_struct {
 INT state;
 FMAPdTPCODE_FILE self;/* Formal argument: self */
 TUPdTPCODE_FILE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT468927009_frame;

typedef struct FMAPdT1644197042_frame_struct {
 INT state;
 FMAPdTPCODE_FILE self;/* Formal argument: self */
 TUPdTPCODE_FILE ret_val1;
 FMAPdTPCODE_FILE L61;
 TUPdTPCODE_FILE r;
 INT L31;
 FMAPdTPCODE_FILE is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT1644197042_frame;

typedef struct FMAPdT953816637_struct {/* layout for FMAP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FMAPdT953816637;

typedef struct FMAPdT247759331_frame_struct {
 INT state;
 FMAPdT953816637 self;/* Formal argument: self */
 TUPdTP1710233051 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT247759331_frame;

typedef struct FMAPdT1043149253_frame_struct {
 INT state;
 FMAPdT953816637 self;/* Formal argument: self */
 TUPdTP1710233051 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT1043149253_frame;

typedef struct FMAPdT2128397196_frame_struct {
 INT state;
 FMAPdT953816637 self;/* Formal argument: self */
 TUPdTP1710233051 ret_val1;
 FMAPdT953816637 L61;
 TUPdTP1710233051 r;
 INT L31;
 FMAPdT953816637 is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT2128397196_frame;

typedef struct FMAPdTPINT_struct {/* layout for FMAP{$TP,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FMAPdTPINT;

typedef struct FMAPdT376430313_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 TUPdTPINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT376430313_frame;

typedef struct FMAPdT192646101_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 TUPdTPINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdT192646101_frame;

typedef struct FMAPdT705360156_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 dTP ret_val1;
 FMAPdTPINT L61;
 dTP r;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT705360156_frame;

typedef struct FMAPdT1857608820_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 TUPdTPINT ret_val1;
 FMAPdTPINT L61;
 TUPdTPINT r;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT1857608820_frame;

typedef struct FMAPdT1072907830_frame_struct {
 INT state;
 FMAPdTPINT self;/* Formal argument: self */
 INT ret_val1;
 FMAPdTPINT L61;
 TUPdTPINT e;
 INT L31;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdT1072907830_frame;

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

typedef struct FMT_ERROR_struct {/* layout for FMT_ERROR */
 INT error1;
 STR str;
 } FMT_ERROR;
static FMT_ERROR FMT_ERROR_zero;

typedef struct FMT_ERROR_boxed_struct {
 OB_HEADER header;
 FMT_ERROR immutable_part;
 } *FMT_ERROR_boxed;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct FSTR_eltbrCHAR_frame_struct {
 INT state;
 FSTR self;/* Formal argument: self */
 CHAR ret_val1;
 INT i;
 INT sz;
 FSTR aget_self;
 INT aget_ind;
 CHAR ret_val;
 } *FSTR_eltbrCHAR_frame;

typedef struct FSTR_indbrINT_frame_struct {
 INT state;
 FSTR self;/* Formal argument: self */
 INT ret_val;
 INT L51;
 INT L61;
 BOOL f_L31_; /* used by builtin iter */
 INT L31_,L31_m; /* used by builtin iter */
 } *FSTR_indbrINT_frame;

typedef struct IDENT_TBL_struct {/* layout for IDENT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *IDENT_TBL;

typedef struct IDENT_1154678862_frame_struct {
 INT state;
 IDENT_TBL self;/* Formal argument: self */
 IDENT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *IDENT_1154678862_frame;

typedef struct IDENT_605919020_frame_struct {
 INT state;
 IDENT_TBL self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *IDENT_605919020_frame;

typedef struct IDENT_119982571_frame_struct {
 INT state;
 IDENT_TBL self;/* Formal argument: self */
 IDENT ret_val2;
 IDENT_TBL L61;
 IDENT r;
 INT L31;
 IDENT_TBL is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *IDENT_119982571_frame;

#include "tags.h"

/* Globals */

extern INT ELT_TB2133420318;
extern INT FMAPdT1555535134;
extern INT FMAPdT2042454662;
extern INT FMAPdT290960897;
extern INT FMAPdT861897287;
extern INT IDENT_105403148;
extern INT SIG_TB1720612202;

/* Function declarations */


extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST STR (**dLAYOUT_strrSTR)(dLAYOUT);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
BOOL ELT_NI125473534(ELT_NILdTP, dTP);
BOOL ELT_TB1828939417(ELT_TBL);
BOOL ELT_TB1905689409(ELT_TBL, IDENT, ELT);
BOOL FSTR_i104531942(FSTR, STR);
BOOL FSTR_i135169312(FSTR, OB);
BOOL FSTR_i470140437(FSTR, FSTR);
BOOL FSTR_i673953987(FSTR, FSTR);
BOOL SIG_TB1063010785(SIG_TBL, IDENT, SIG);
BOOL SIG_co165522953(SIG, SIG);
BOOL STR_is111530248(STR, STR);
CHAR FSTR_eltbrCHAR(FSTR_eltbrCHAR_frame);
CODE_FILE FMAPdT1940836411(FMAPdTPCODE_FILE, dTP);
ELT ELT_TB1201943681(ELT_TBL, ELT);
ELT ELT_TB1949891380(ELT_TBL, SIG);
ELT ELT_TB431570775(ELT_TB431570775_frame);
ELT ELT_TB799962086(ELT_TB799962086_frame);
ELT ELT_TB954957059(ELT_TBL, ELT);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
ELT_TBL ELT_TB1573869061(ELT_TBL, ELT);
ELT_TBL ELT_TB1607182604(ELT_TBL, INT);
ELT_TBL ELT_TB1703146017(ELT_TBL, ELT);
ELT_TBL ELT_TB1862915904(ELT_TBL);
ELT_TBL ELT_TB809991989(ELT_TBL);
FMAPID739086519 FMAPdT183078507(FMAPdT953816637, dTP);
FMAPdT953816637 FMAPdT1221772946(FMAPdT953816637, dTP, FMAPID739086519);
FMAPdT953816637 FMAPdT1662108669(FMAPdT953816637, INT);
FMAPdT953816637 FMAPdT294819778(FMAPdT953816637);
FMAPdTPCODE_FILE FMAPdT2092797743(FMAPdTPCODE_FILE, dTP, CODE_FILE);
FMAPdTPCODE_FILE FMAPdT282854189(FMAPdTPCODE_FILE, INT);
FMAPdTPCODE_FILE FMAPdT815893584(FMAPdTPCODE_FILE);
FMAPdTPINT FMAPdT1224191190(FMAPdTPINT);
FMAPdTPINT FMAPdT1485079589(FMAPdTPINT, dTP, INT);
FMAPdTPINT FMAPdT1801188375(FMAPdTPINT, INT);
FMAPdTPdLAYOUT FMAPdT1248247834(FMAPdTPdLAYOUT, dTP, dLAYOUT);
FMAPdTPdLAYOUT FMAPdT1552226099(FMAPdTPdLAYOUT, INT);
FMAPdTPdLAYOUT FMAPdT391465938(FMAPdTPdLAYOUT);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR FSTR_s1920457899(FSTR, INT, INT);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1154678862(IDENT_1154678862_frame);
IDENT IDENT_119982571(IDENT_119982571_frame);
IDENT IDENT_512720063(IDENT_TBL, STR);
IDENT_TBL IDENT_526982742(IDENT_TBL, INT);
IDENT_TBL IDENT_741964429(IDENT_TBL);
IDENT_TBL IDENT_81729787(IDENT_TBL, IDENT);
IFC ELT_TB462529002(ELT_TBL);
INT ELT_HA1612720024(ELT_HASH, OB);
INT ELT_TBL_sizerINT(ELT_TBL);
INT FMAPdT1072907830(FMAPdT1072907830_frame);
INT FMAPdT1077886323(FMAPdTPINT);
INT FMAPdT165518335(FMAPdTPINT, dTP);
INT FSTR_hash0rINT(FSTR);
INT FSTR_hashrINT(FSTR);
INT FSTR_indbrINT(FSTR_indbrINT_frame);
INT FSTR_sizerINT(FSTR);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT SIG_TBL_sizerINT(SIG_TBL);
INT STR_hashrINT(STR);
INT STR_lengthrINT(STR);
INT STR_sizerINT(STR);
SIG SIG_TB1067194561(SIG_TBL, SIG);
SIG SIG_TB1663296138(SIG_TB1663296138_frame);
SIG SIG_TB788365833(SIG_TB788365833_frame);
SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame);
SIG_TBL SIG_TB1167290220(SIG_TBL, INT);
SIG_TBL SIG_TB1438251609(SIG_TBL, SIG);
SIG_TBL SIG_TB543803525(SIG_TBL);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR ELT_TBL_strrSTR(ELT_TBL);
STR FMAPID1895267434(FMAPID739086519);
STR FMAPdT1371742936(FMAPdTPdLAYOUT);
STR FMAPdT1912217709(FMAPdTPINT);
STR FMAPdT2142836224(FMAPdT953816637);
STR FMAPdT966432485(FMAPdTPCODE_FILE);
STR FMT_ER330399541(FMT_ERROR);
STR FSTR_strrSTR(FSTR);
STR FSTR_t603468843(FSTR);
STR SIG_TBL_strrSTR(SIG_TBL);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
TUPdTP1710233051 FMAPdT2128397196(FMAPdT2128397196_frame);
TUPdTP1710233051 FMAPdT247759331(FMAPdT247759331_frame);
TUPdTPCODE_FILE FMAPdT1644197042(FMAPdT1644197042_frame);
TUPdTPCODE_FILE FMAPdT669571245(FMAPdT669571245_frame);
TUPdTPINT FMAPdT1857608820(FMAPdT1857608820_frame);
TUPdTPINT FMAPdT376430313(FMAPdT376430313_frame);
TUPdTPdLAYOUT FMAPdT1924387821(FMAPdT1924387821_frame);
TUPdTPdLAYOUT FMAPdT2027173596(FMAPdT2027173596_frame);
dLAYOUT FMAPdT7482853(FMAPdTPdLAYOUT, dTP);
dTP ELT_NI1328289538(ELT_NILdTP);
dTP FMAPdT705360156(FMAPdT705360156_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void ELT_TB382032404(ELT_TB382032404_frame);
void FMAPdT1043149253(FMAPdT1043149253_frame);
void FMAPdT192646101(FMAPdT192646101_frame);
void FMAPdT468927009(FMAPdT468927009_frame);
void FMAPdT963530613(FMAPdT963530613_frame);
void FSTR_to_reverse(FSTR);
void IDENT_605919020(IDENT_605919020_frame);
void SIG_TB2081225820(SIG_TB2081225820_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL ELT_TB1828939417(ELT_TBL self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 INT L21_;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 L21_=ASIZE((ELT_TBL)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((ELT_TBL)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = ELT_TB2133420318;
  L131_=INTTIMES(L12,2); 
  L14 = L131_;
  L151_=INTDIV(L11,L14); 
  L16 = L151_;
  L171_=(L7)<(L16); 
  L1 = L171_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_TB1905689409(ELT_TBL self, IDENT name111, ELT e) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 if ((e==((ELT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = ATTR(ATTR(e,sig1),name1);
 is_eq_i = name111;
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSTR_i104531942(FSTR self, STR s) {
 BOOL ret_val = BOOL_zero;
 INT s1 = INT_zero;
 INT s2 = INT_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L41_;
 if ((self==((FSTR) NULL))) {
  s1 = -1;
 }
 else {
  s1 = ATTR(self,loc);
 }
 if ((s==((STR) NULL))) {
  s2 = -1;
 }
 else {
  s2 = STR_sizerINT(s);
 }
 L11_=(s1)==(s2); 
 L2 = L11_;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 L41_=STRISEQHELPER(self,s,s1); 
 ret_val = L41_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSTR_i135169312(FSTR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 if (arg==NULL) {
  goto other166;
 } else
 switch (TAG(arg)) {
  case FSTR_tag:
   ret_val = FSTR_i470140437(self, ((FSTR) arg));
   return ret_val; break;
  default: ;
  other166: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSTR_i470140437(FSTR self, FSTR s) {
 BOOL ret_val = BOOL_zero;
 INT s1 = INT_zero;
 INT s2 = INT_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 BOOL L41_;
 if ((self==((FSTR) NULL))) {
  s1 = -1;
 }
 else {
  s1 = ATTR(self,loc);
 }
 if ((s==((FSTR) NULL))) {
  s2 = -1;
 }
 else {
  s2 = ATTR(s,loc);
 }
 L11_=(s1)==(s2); 
 L2 = L11_;
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 L41_=STRISEQHELPER(self,s,s1); 
 ret_val = L41_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSTR_i673953987(FSTR self, FSTR s) {
 BOOL ret_val = BOOL_zero;
 FSTR L11;
 CHAR c = CHAR_zero;
 FSTR L21;
 CHAR sc = CHAR_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val1 = BOOL_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val2 = BOOL_zero;
 INT L3;
 BOOL L41_;
 INT L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 CHAR L12;
 CHAR L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 INT L191_;
 INT L20;
 INT L221_;
 INT L23;
 BOOL L241_;
 INT L25;
 INT L26;
 BOOL L271_;
 L3 = FSTR_sizerINT(self);
 L41_=(L3)==(0); 
 if (L41_) {
  L5 = FSTR_sizerINT(s);
  L61_=(L5)==(0); 
  L7 = L61_;
  L81_=!(L7); 
  if (L81_) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   ret_val = FALSE;
   return ret_val;
  }
 }
 L9 = FSTR_sizerINT(s);
 L101_=(L9)==(0); 
 if (L101_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct FSTR_eltbrCHAR_frame_struct other1_0;
FSTR_eltbrCHAR_frame noname1 = &other1_0;
  struct FSTR_eltbrCHAR_frame_struct other1_1;
FSTR_eltbrCHAR_frame noname2 = &other1_1;
  L11 = self;
  L21 = s;
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L12 = FSTR_eltbrCHAR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L12;
   L13 = FSTR_eltbrCHAR(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   sc = L13;
   is_lt_self = sc;
   is_lt_c = c;
   L141_=(INT)((unsigned char)is_lt_self); 
   L15 = L141_;
   L161_=(INT)((unsigned char)is_lt_c); 
   L17 = L161_;
   L181_=(L15)<(L17); 
   ret_val1 = L181_;
   if (ret_val1) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    is_lt_self1 = c;
    is_lt_c1 = sc;
    L191_=(INT)((unsigned char)is_lt_self1); 
    L20 = L191_;
    L221_=(INT)((unsigned char)is_lt_c1); 
    L23 = L221_;
    L241_=(L20)<(L23); 
    ret_val2 = L241_;
    if (ret_val2) {
     ret_val = TRUE;
     return ret_val;
    }
   }
  }
 }
 after_loop: ;
 L25 = FSTR_sizerINT(self);
 L26 = FSTR_sizerINT(s);
 L271_=(L25)<(L26); 
 if (L271_) {
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

BOOL SIG_TB1063010785(SIG_TBL self, IDENT name111, SIG s) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 if ((s==((SIG) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = ATTR(s,name1);
 is_eq_i = name111;
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CODE_FILE FMAPdT1940836411(FMAPdTPCODE_FILE self, dTP k) {
 CODE_FILE ret_val;
 INT h = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPCODE_FILE key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdTPCODE_FILE is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdTPCODE_FILE key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 FMAPdTPCODE_FILE is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FMAPdTPCODE_FILE key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdTPCODE_FILE L71_;
 TUPdTPCODE_FILE L8;
 dTP L9;
 TUPdTPCODE_FILE L101_;
 TUPdTPCODE_FILE L11;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPdTPCODE_FILE L181_;
 TUPdTPCODE_FILE L19;
 dTP L20;
 TUPdTPCODE_FILE L221_;
 TUPdTPCODE_FILE L23;
 INT L241_;
 if ((self==((FMAPdTPCODE_FILE) NULL))) {
  ret_val = ((CODE_FILE) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdTPCODE_FILE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdTPCODE_FILE)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQdTP) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   L9 = elt_eq_e1;
   ret_val4 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) elt_eq_e2));
   ret_val3 = ret_val4;
   if (ret_val3) {
    L101_=ARR((FMAPdTPCODE_FILE)self,h); 
    L11=L101_;
    ret_val = L11.t2;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPdTPCODE_FILE)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPdTPCODE_FILE)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQdTP) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    L20 = elt_eq_e11;
    ret_val7 = (*dTP_is242312711[TAG(L20)])(L20, ((OB) elt_eq_e21));
    ret_val6 = ret_val7;
    if (ret_val6) {
     L221_=ARR((FMAPdTPCODE_FILE)self,h); 
     L23=L221_;
     ret_val = L23.t2;
     return ret_val;
    }
   }
   L241_=INTPLUS(h,1); 
   h = L241_;
  }
  after_loop1: ;
 }
 ret_val = ((CODE_FILE) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT ELT_TB1201943681(ELT_TBL self, ELT e) {
 ELT ret_val;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT r;
 ELT name_self;
 IDENT ret_val1 = IDENT_zero;
 ELT conflicts_with_s;
 ELT conflicts_with_e;
 BOOL ret_val2 = BOOL_zero;
 ELT L3;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = self;
  name_self = e;
  ret_val1 = ATTR(ATTR(name_self,sig1),name1);
  L21 = ret_val1;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = L3;
   conflicts_with_s = r;
   conflicts_with_e = e;
   ret_val2 = SIG_co165522953(ATTR(conflicts_with_s,sig1), ATTR(conflicts_with_e,sig1));
   if (ret_val2) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = ((ELT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT ELT_TB1949891380(ELT_TBL self, SIG s) {
 ELT ret_val;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT r;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val1 = BOOL_zero;
 ELT L3;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = self;
  L21 = ATTR(s,name1);
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = L3;
   is_eq_self = s;
   is_eq_s = ATTR(r,sig1);
   ret_val1 = (is_eq_self==is_eq_s);
   if (ret_val1) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = ((ELT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT ELT_TB954957059(ELT_TBL self, ELT e) {
 ELT ret_val;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT name_self;
 IDENT ret_val1 = IDENT_zero;
 ELT L3;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = self;
  name_self = e;
  ret_val1 = ATTR(ATTR(name_self,sig1),name1);
  L21 = ret_val1;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   ret_val = L3;
   return ret_val;
  }
 }
 after_loop: ;
 ret_val = ((ELT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL ELT_TB1573869061(ELT_TBL self, ELT e) {
 ELT_TBL ret_val;
 INT h = INT_zero;
 ELT te;
 INT i = INT_zero;
 ELT te1;
 INT hsh = INT_zero;
 ELT_TBL elt_hash_self;
 ELT elt_hash_e;
 INT ret_val1 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val2 = INT_zero;
 INT x = INT_zero;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 ELT_TBL elt_eq_self;
 ELT elt_eq_e1;
 ELT elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQELT elt_eq_self1;
 ELT elt_eq_e11;
 ELT elt_eq_e21;
 BOOL ret_val6 = BOOL_zero;
 ELT_TBL elt_nil_self;
 ELT ret_val7;
 ELT_NILELT elt_nil_self1;
 ELT ret_val8;
 ELT e1 = ((ELT) NULL);
 ELT_TBL is_elt_nil_self2;
 ELT is_elt_nil_e2;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILELT is_elt_nil_self3;
 ELT is_elt_nil_e3;
 BOOL ret_val10 = BOOL_zero;
 ELT_TBL elt_hash_self1;
 ELT elt_hash_e1;
 INT ret_val11 = INT_zero;
 IDENT hash_self1 = IDENT_zero;
 INT ret_val12 = INT_zero;
 INT x1 = INT_zero;
 ELT_TBL elt_nil_self2;
 ELT ret_val13;
 ELT_NILELT elt_nil_self3;
 ELT ret_val14;
 ELT e2 = ((ELT) NULL);
 ELT_TBL elt_nil_self4;
 ELT ret_val15;
 ELT_NILELT elt_nil_self5;
 ELT ret_val16;
 ELT e3 = ((ELT) NULL);
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 ELT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 INT L181_;
 ELT L19;
 INT L22;
 INT L231_;
 INT L241_;
 INT L25;
 INT L261_;
 INT L27;
 BOOL L281_;
 INT L291_;
 ELT L301_;
 STR L32;
 INT L331_;
 INT L341_;
 INT L35;
 INT L361_;
 INT L37;
 INT L381_;
 INT L39;
 INT L401_;
 INT L41;
 INT L421_;
 BOOL L431_;
 BOOL L44;
 BOOL L451_;
 BOOL L46;
 BOOL L471_;
 BOOL L481_;
 BOOL L49;
 BOOL L501_;
 ELT L521_;
 ELT L53;
 ELT L55;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 BOOL L611_;
 ELT L621_;
 ELT L63;
 ELT L65;
 if ((self==((ELT_TBL) NULL))) {
  ret_val = ((ELT_TBL) NULL);
  return ret_val;
 }
 elt_hash_self = self;
 elt_hash_e = e;
 hash_self = ATTR(ATTR(elt_hash_e,sig1),name1);
 L1 = hash_self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val2 = L51_;
 ret_val1 = ret_val2;
 L6 = ret_val1;
 L71_=ASIZE((ELT_TBL)self); 
 L8 = L71_;
 L91_=INTMINUS(L8,2); 
 L10 = L91_;
 L111_=L6&L10; 
 h = L111_;
 while (1) {
  L121_=(ELT)ARR((ELT_TBL)self,h); 
  te = L121_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILELT) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val4 = (is_elt_nil_e1==((ELT) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQELT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val6 = (elt_eq_e11==elt_eq_e21);
   ret_val5 = ret_val6;
   if (ret_val5) {
    goto after_loop;
   }
  }
  L131_=ASIZE((ELT_TBL)self); 
  L14 = L131_;
  L151_=INTMINUS(L14,2); 
  L16 = L151_;
  L171_=(h)==(L16); 
  if (L171_) {
   h = 0;
  }
  else {
   L181_=INTPLUS(h,1); 
   h = L181_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILELT) NULL);
 ret_val8 = ((ELT) NULL);
 ret_val7 = ret_val8;
 L19 = ret_val7;
 SARR((ELT_TBL)self,h,(ELT)L19); 
 ;
 L22 = ATTR(self,hsize);
 L231_=INTMINUS(L22,1); 
 SATTR(self,hsize,L231_);
 i = h;
 while (1) {
  L241_=ASIZE((ELT_TBL)self); 
  L25 = L241_;
  L261_=INTMINUS(L25,2); 
  L27 = L261_;
  L281_=(i)==(L27); 
  if (L281_) {
   i = 0;
  }
  else {
   L291_=INTPLUS(i,1); 
   i = L291_;
  }
  L301_=(ELT)ARR((ELT_TBL)self,i); 
  te1 = L301_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NILELT) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val10 = (is_elt_nil_e3==((ELT) NULL));
  ret_val9 = ret_val10;
  if (ret_val9) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = te1;
  hash_self1 = ATTR(ATTR(elt_hash_e1,sig1),name1);
  L32 = hash_self1.str;
  L331_=SYSID(L32); 
  x1 = L331_;
  L341_=((unsigned)x1*(unsigned)1664525); 
  L35 = L341_;
  L361_=((unsigned)L35+(unsigned)1013904223); 
  ret_val12 = L361_;
  ret_val11 = ret_val12;
  L37 = ret_val11;
  L381_=ASIZE((ELT_TBL)self); 
  L39 = L381_;
  L401_=INTMINUS(L39,2); 
  L41 = L401_;
  L421_=L37&L41; 
  hsh = L421_;
  L431_=(i)<(hsh); 
  L44 = L431_;
  L451_=!(L44); 
  if (L451_) {
   L471_=(h)<(i); 
   if (L471_) {
    L481_=(h)<(hsh); 
    L49 = L481_;
    L501_=!(L49); 
    L46 = L501_;
   } else {
    L46 = FALSE;
   }
   if (L46) {
    L521_=(ELT)ARR((ELT_TBL)self,i); 
    L53 = L521_;
    SARR((ELT_TBL)self,h,(ELT)L53); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILELT) NULL);
    ret_val14 = ((ELT) NULL);
    ret_val13 = ret_val14;
    L55 = ret_val13;
    SARR((ELT_TBL)self,i,(ELT)L55); 
    ;
   }
  }
  else {
   L581_=(h)<(hsh); 
   L59 = L581_;
   L601_=!(L59); 
   if (L601_) {
    L57 = TRUE;
   } else {
    L611_=(h)<(i); 
    L57 = L611_;
   }
   if (L57) {
    L621_=(ELT)ARR((ELT_TBL)self,i); 
    L63 = L621_;
    SARR((ELT_TBL)self,h,(ELT)L63); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NILELT) NULL);
    ret_val16 = ((ELT) NULL);
    ret_val15 = ret_val16;
    L65 = ret_val15;
    SARR((ELT_TBL)self,i,(ELT)L65); 
    ;
   }
  }
 }
 after_loop1: ;
 if (ELT_TB1828939417(self)) {
  ret_val = ELT_TB1862915904(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL ELT_TB1607182604(ELT_TBL self, INT n) {
 ELT_TBL ret_val;
 ELT_TBL r;
 ELT_TBL L11;
 ELT_TBL elt_nil_self;
 ELT ret_val1;
 ELT_NILELT elt_nil_self1;
 ELT ret_val2;
 ELT e = ((ELT) NULL);
 ELT_TBL elt_nil_self2;
 ELT ret_val3;
 ELT_NILELT elt_nil_self3;
 ELT ret_val4;
 ELT e1 = ((ELT) NULL);
 INT L21 = INT_zero;
 ELT_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 ELT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct ELT_TBL_struct)-sizeof(ELT))+(L4)*sizeof(ELT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=ELT_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((ELT_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILELT) NULL);
 ret_val2 = ((ELT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((ELT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((ELT_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILELT) NULL);
    ret_val4 = ((ELT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((ELT_TBL)L11,L21,(ELT)L10); 
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

ELT_TBL ELT_TB1703146017(ELT_TBL self, ELT e) {
 ELT_TBL ret_val;
 ELT_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 ELT te;
 ELT te1;
 ELT_TBL is_elt_nil_self;
 ELT is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILELT is_elt_nil_self1;
 ELT is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 ELT_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 ELT_TBL elt_hash_self;
 ELT elt_hash_e;
 INT ret_val4 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val5 = INT_zero;
 INT x = INT_zero;
 ELT_TBL is_elt_nil_self2;
 ELT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILELT is_elt_nil_self3;
 ELT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 ELT_TBL elt_eq_self;
 ELT elt_eq_e1;
 ELT elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQELT elt_eq_self1;
 ELT elt_eq_e11;
 ELT elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_TBL is_elt_nil_self4;
 ELT is_elt_nil_e4;
 BOOL ret_val10 = BOOL_zero;
 ELT_NILELT is_elt_nil_self5;
 ELT is_elt_nil_e5;
 BOOL ret_val11 = BOOL_zero;
 ELT_TBL elt_eq_self2;
 ELT elt_eq_e12;
 ELT elt_eq_e22;
 BOOL ret_val12 = BOOL_zero;
 ELT_EQELT elt_eq_self3;
 ELT elt_eq_e13;
 ELT elt_eq_e23;
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
 STR L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 ELT L211_;
 INT L231_;
 INT L241_;
 INT L25;
 BOOL L261_;
 ELT L271_;
 INT L291_;
 INT L31;
 INT L321_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILELT) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((ELT) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((ELT_TBL) NULL))) {
  r = ELT_TB1607182604(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((ELT_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = ELT_TB2133420318;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = ELT_TB809991989(r);
  }
 }
 L101_=ASIZE((ELT_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_e = e;
 hash_self = ATTR(ATTR(elt_hash_e,sig1),name1);
 L12 = hash_self.str;
 L131_=SYSID(L12); 
 x = L131_;
 L141_=((unsigned)x*(unsigned)1664525); 
 L15 = L141_;
 L161_=((unsigned)L15+(unsigned)1013904223); 
 ret_val5 = L161_;
 ret_val4 = ret_val5;
 L17 = ret_val4;
 L181_=INTMINUS(asz,2); 
 L19 = L181_;
 L201_=L17&L19; 
 orig_h = L201_;
 h = orig_h;
 while (1) {
  L211_=(ELT)ARR((ELT_TBL)r,h); 
  te = L211_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILELT) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val7 = (is_elt_nil_e3==((ELT) NULL));
  ret_val6 = ret_val7;
  if (ret_val6) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQELT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val9 = (elt_eq_e11==elt_eq_e21);
   ret_val8 = ret_val9;
   if (ret_val8) {
    SARR((ELT_TBL)r,h,(ELT)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L231_=INTPLUS(h,1); 
  h = L231_;
 }
 after_loop: ;
 L241_=INTMINUS(asz,1); 
 L25 = L241_;
 L261_=(h)==(L25); 
 if (L261_) {
  h = 0;
  while (1) {
   L271_=(ELT)ARR((ELT_TBL)r,h); 
   te1 = L271_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILELT) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val11 = (is_elt_nil_e5==((ELT) NULL));
   ret_val10 = ret_val11;
   if (ret_val10) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQELT) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val13 = (elt_eq_e13==elt_eq_e23);
    ret_val12 = ret_val13;
    if (ret_val12) {
     SARR((ELT_TBL)r,h,(ELT)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L291_=INTPLUS(h,1); 
   h = L291_;
  }
  after_loop1: ;
 }
 SARR((ELT_TBL)r,h,(ELT)e); 
 ;
 L31 = ATTR(r,hsize);
 L321_=INTPLUS(L31,1); 
 SATTR(r,hsize,L321_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ELT_TBL ELT_TB1862915904(ELT_TBL self) {
 ELT_TBL ret_val;
 ELT_TBL r;
 ELT_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 ELT L9;
 L21_=ASIZE((ELT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = ELT_TB1607182604(self, L81_);
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = ELT_TB1703146017(r, L9);
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

ELT_TBL ELT_TB809991989(ELT_TBL self) {
 ELT_TBL ret_val;
 ELT_TBL r;
 ELT_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 ELT L9;
 L21_=ASIZE((ELT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = ELT_TB1607182604(self, L81_);
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = ELT_TB1703146017(r, L9);
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

FMAPID739086519 FMAPdT183078507(FMAPdT953816637 self, dTP k) {
 FMAPID739086519 ret_val;
 INT h = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdT953816637 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdT953816637 is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdT953816637 key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 FMAPdT953816637 is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FMAPdT953816637 key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdTP1710233051 L71_;
 TUPdTP1710233051 L8;
 dTP L9;
 TUPdTP1710233051 L101_;
 TUPdTP1710233051 L11;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPdTP1710233051 L181_;
 TUPdTP1710233051 L19;
 dTP L20;
 TUPdTP1710233051 L221_;
 TUPdTP1710233051 L23;
 INT L241_;
 if ((self==((FMAPdT953816637) NULL))) {
  ret_val = ((FMAPID739086519) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdT953816637)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdT953816637)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQdTP) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   L9 = elt_eq_e1;
   ret_val4 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) elt_eq_e2));
   ret_val3 = ret_val4;
   if (ret_val3) {
    L101_=ARR((FMAPdT953816637)self,h); 
    L11=L101_;
    ret_val = L11.t2;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPdT953816637)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPdT953816637)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQdTP) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    L20 = elt_eq_e11;
    ret_val7 = (*dTP_is242312711[TAG(L20)])(L20, ((OB) elt_eq_e21));
    ret_val6 = ret_val7;
    if (ret_val6) {
     L221_=ARR((FMAPdT953816637)self,h); 
     L23=L221_;
     ret_val = L23.t2;
     return ret_val;
    }
   }
   L241_=INTPLUS(h,1); 
   h = L241_;
  }
  after_loop1: ;
 }
 ret_val = ((FMAPID739086519) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdT953816637 FMAPdT1221772946(FMAPdT953816637 self, dTP k, FMAPID739086519 t) {
 FMAPdT953816637 ret_val;
 FMAPdT953816637 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdT953816637 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPdT953816637 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPdT953816637 is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPdT953816637 key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TUPdTP1710233051 create_self = TUPdTP1710233051_zero;
 dTP create_at1;
 FMAPID739086519 create_at2;
 TUPdTP1710233051 ret_val6 = TUPdTP1710233051_zero;
 FMAPdT953816637 is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPdT953816637 key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 TUPdTP1710233051 create_self1 = TUPdTP1710233051_zero;
 dTP create_at11;
 FMAPID739086519 create_at21;
 TUPdTP1710233051 ret_val10 = TUPdTP1710233051_zero;
 TUPdTP1710233051 create_self2 = TUPdTP1710233051_zero;
 dTP create_at12;
 FMAPID739086519 create_at22;
 TUPdTP1710233051 ret_val11 = TUPdTP1710233051_zero;
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
 TUPdTP1710233051 L201_;
 TUPdTP1710233051 L21;
 dTP L22;
 TUPdTP1710233051 L23;
 TUPdTP1710233051 L24;
 TUPdTP1710233051 L25;
 INT L271_;
 BOOL L281_;
 TUPdTP1710233051 L291_;
 TUPdTP1710233051 L30;
 dTP L31;
 TUPdTP1710233051 L32;
 TUPdTP1710233051 L33;
 TUPdTP1710233051 L34;
 INT L361_;
 TUPdTP1710233051 L37;
 TUPdTP1710233051 L38;
 TUPdTP1710233051 L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPdT953816637) NULL))) {
  r = FMAPdT1662108669(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPdT953816637)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPdT290960897;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPdT294819778(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPdT953816637)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPdT953816637)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPdT953816637)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQdTP) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  L22 = elt_eq_e1;
  ret_val5 = (*dTP_is242312711[TAG(L22)])(L22, ((OB) elt_eq_e2));
  ret_val4 = ret_val5;
  if (ret_val4) {
   create_self = TUPdTP1710233051_zero;
   create_at1 = k;
   create_at2 = t;
   L23 = create_self;
   L23.t1 = create_at1;
   L24 = L23;
   L24.t2 = create_at2;
   ret_val6 = L24;
   L25 = ret_val6;
   SARR((FMAPdT953816637)r,h,L25); 
   ;
   ret_val = r;
   return ret_val;
  }
  L271_=INTPLUS(h,1); 
  h = L271_;
 }
 after_loop: ;
 L281_=(h)==(asm1); 
 if (L281_) {
  h = 0;
  while (1) {
   L291_=ARR((FMAPdT953816637)r,h); 
   L30=L291_;
   tk1 = L30.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val7 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val7) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   L31 = elt_eq_e11;
   ret_val9 = (*dTP_is242312711[TAG(L31)])(L31, ((OB) elt_eq_e21));
   ret_val8 = ret_val9;
   if (ret_val8) {
    create_self1 = TUPdTP1710233051_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val10 = L33;
    L34 = ret_val10;
    SARR((FMAPdT953816637)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdTP1710233051_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val11 = L38;
 L39 = ret_val11;
 SARR((FMAPdT953816637)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdT953816637 FMAPdT1662108669(FMAPdT953816637 self, INT n) {
 FMAPdT953816637 ret_val;
 FMAPdT953816637 r;
 FMAPdT953816637 L11;
 FMAPdT953816637 key_nil_self;
 dTP ret_val1;
 FMAPdT953816637 key_nil_self1;
 dTP ret_val2;
 TUPdTP1710233051 create_self = TUPdTP1710233051_zero;
 dTP create_at1;
 FMAPID739086519 create_at2;
 TUPdTP1710233051 ret_val3 = TUPdTP1710233051_zero;
 INT L21 = INT_zero;
 FMAPdT953816637 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdTP1710233051 L10;
 TUPdTP1710233051 L12;
 TUPdTP1710233051 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPdT953816637_struct)-sizeof(TUPdTP1710233051))+(L4)*sizeof(TUPdTP1710233051);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPdT953816637_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPdT953816637) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPdT953816637)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdTP1710233051_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    create_at1 = ret_val2;
    create_at2 = ((FMAPID739086519) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((FMAPdT953816637)L11,L21,L13); 
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

FMAPdT953816637 FMAPdT294819778(FMAPdT953816637 self) {
 FMAPdT953816637 ret_val;
 INT ns = INT_zero;
 FMAPdT953816637 r;
 FMAPdT953816637 L11;
 FMAPdT953816637 insert_pair_self;
 TUPdTP1710233051 insert_pair_p = TUPdTP1710233051_zero;
 FMAPdT953816637 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdTP1710233051 L9;
 L21_=ASIZE((FMAPdT953816637)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdT1662108669(self, ns);
 {
  struct FMAPdT2128397196_frame_struct other1_0;
FMAPdT2128397196_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdT2128397196(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdT1221772946(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPdTPCODE_FILE FMAPdT2092797743(FMAPdTPCODE_FILE self, dTP k, CODE_FILE t) {
 FMAPdTPCODE_FILE ret_val;
 FMAPdTPCODE_FILE r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPCODE_FILE should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPdTPCODE_FILE key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPdTPCODE_FILE is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPdTPCODE_FILE key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TUPdTPCODE_FILE create_self = TUPdTPCODE_FILE_zero;
 dTP create_at1;
 CODE_FILE create_at2;
 TUPdTPCODE_FILE ret_val6 = TUPdTPCODE_FILE_zero;
 FMAPdTPCODE_FILE is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPdTPCODE_FILE key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 TUPdTPCODE_FILE create_self1 = TUPdTPCODE_FILE_zero;
 dTP create_at11;
 CODE_FILE create_at21;
 TUPdTPCODE_FILE ret_val10 = TUPdTPCODE_FILE_zero;
 TUPdTPCODE_FILE create_self2 = TUPdTPCODE_FILE_zero;
 dTP create_at12;
 CODE_FILE create_at22;
 TUPdTPCODE_FILE ret_val11 = TUPdTPCODE_FILE_zero;
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
 TUPdTPCODE_FILE L201_;
 TUPdTPCODE_FILE L21;
 dTP L22;
 TUPdTPCODE_FILE L23;
 TUPdTPCODE_FILE L24;
 TUPdTPCODE_FILE L25;
 INT L271_;
 BOOL L281_;
 TUPdTPCODE_FILE L291_;
 TUPdTPCODE_FILE L30;
 dTP L31;
 TUPdTPCODE_FILE L32;
 TUPdTPCODE_FILE L33;
 TUPdTPCODE_FILE L34;
 INT L361_;
 TUPdTPCODE_FILE L37;
 TUPdTPCODE_FILE L38;
 TUPdTPCODE_FILE L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPdTPCODE_FILE) NULL))) {
  r = FMAPdT282854189(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPdTPCODE_FILE)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPdT2042454662;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPdT815893584(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPdTPCODE_FILE)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPdTPCODE_FILE)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPdTPCODE_FILE)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQdTP) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  L22 = elt_eq_e1;
  ret_val5 = (*dTP_is242312711[TAG(L22)])(L22, ((OB) elt_eq_e2));
  ret_val4 = ret_val5;
  if (ret_val4) {
   create_self = TUPdTPCODE_FILE_zero;
   create_at1 = k;
   create_at2 = t;
   L23 = create_self;
   L23.t1 = create_at1;
   L24 = L23;
   L24.t2 = create_at2;
   ret_val6 = L24;
   L25 = ret_val6;
   SARR((FMAPdTPCODE_FILE)r,h,L25); 
   ;
   ret_val = r;
   return ret_val;
  }
  L271_=INTPLUS(h,1); 
  h = L271_;
 }
 after_loop: ;
 L281_=(h)==(asm1); 
 if (L281_) {
  h = 0;
  while (1) {
   L291_=ARR((FMAPdTPCODE_FILE)r,h); 
   L30=L291_;
   tk1 = L30.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val7 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val7) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   L31 = elt_eq_e11;
   ret_val9 = (*dTP_is242312711[TAG(L31)])(L31, ((OB) elt_eq_e21));
   ret_val8 = ret_val9;
   if (ret_val8) {
    create_self1 = TUPdTPCODE_FILE_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val10 = L33;
    L34 = ret_val10;
    SARR((FMAPdTPCODE_FILE)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdTPCODE_FILE_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val11 = L38;
 L39 = ret_val11;
 SARR((FMAPdTPCODE_FILE)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdTPCODE_FILE FMAPdT282854189(FMAPdTPCODE_FILE self, INT n) {
 FMAPdTPCODE_FILE ret_val;
 FMAPdTPCODE_FILE r;
 FMAPdTPCODE_FILE L11;
 FMAPdTPCODE_FILE key_nil_self;
 dTP ret_val1;
 FMAPdTPCODE_FILE key_nil_self1;
 dTP ret_val2;
 TUPdTPCODE_FILE create_self = TUPdTPCODE_FILE_zero;
 dTP create_at1;
 CODE_FILE create_at2;
 TUPdTPCODE_FILE ret_val3 = TUPdTPCODE_FILE_zero;
 INT L21 = INT_zero;
 FMAPdTPCODE_FILE L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdTPCODE_FILE L10;
 TUPdTPCODE_FILE L12;
 TUPdTPCODE_FILE L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPdTPCODE_FILE_struct)-sizeof(TUPdTPCODE_FILE))+(L4)*sizeof(TUPdTPCODE_FILE);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPdTPCODE_FILE_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPdTPCODE_FILE) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPdTPCODE_FILE)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdTPCODE_FILE_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    create_at1 = ret_val2;
    create_at2 = ((CODE_FILE) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((FMAPdTPCODE_FILE)L11,L21,L13); 
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

FMAPdTPCODE_FILE FMAPdT815893584(FMAPdTPCODE_FILE self) {
 FMAPdTPCODE_FILE ret_val;
 INT ns = INT_zero;
 FMAPdTPCODE_FILE r;
 FMAPdTPCODE_FILE L11;
 FMAPdTPCODE_FILE insert_pair_self;
 TUPdTPCODE_FILE insert_pair_p = TUPdTPCODE_FILE_zero;
 FMAPdTPCODE_FILE ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdTPCODE_FILE L9;
 L21_=ASIZE((FMAPdTPCODE_FILE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdT282854189(self, ns);
 {
  struct FMAPdT1644197042_frame_struct other1_0;
FMAPdT1644197042_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdT1644197042(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdT2092797743(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPdTPINT FMAPdT1224191190(FMAPdTPINT self) {
 FMAPdTPINT ret_val;
 INT ns = INT_zero;
 FMAPdTPINT r;
 FMAPdTPINT L11;
 FMAPdTPINT insert_pair_self;
 TUPdTPINT insert_pair_p = TUPdTPINT_zero;
 FMAPdTPINT ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdTPINT L9;
 L21_=ASIZE((FMAPdTPINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdT1801188375(self, ns);
 {
  struct FMAPdT1857608820_frame_struct other1_0;
FMAPdT1857608820_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdT1857608820(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdT1485079589(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPdTPINT FMAPdT1485079589(FMAPdTPINT self, dTP k, INT t) {
 FMAPdTPINT ret_val;
 FMAPdTPINT r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPINT should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPdTPINT key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPdTPINT key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TUPdTPINT create_self = TUPdTPINT_zero;
 dTP create_at1;
 INT create_at2 = INT_zero;
 TUPdTPINT ret_val6 = TUPdTPINT_zero;
 FMAPdTPINT is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPdTPINT key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 TUPdTPINT create_self1 = TUPdTPINT_zero;
 dTP create_at11;
 INT create_at21 = INT_zero;
 TUPdTPINT ret_val10 = TUPdTPINT_zero;
 TUPdTPINT create_self2 = TUPdTPINT_zero;
 dTP create_at12;
 INT create_at22 = INT_zero;
 TUPdTPINT ret_val11 = TUPdTPINT_zero;
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
 TUPdTPINT L201_;
 TUPdTPINT L21;
 dTP L22;
 TUPdTPINT L23;
 TUPdTPINT L24;
 TUPdTPINT L25;
 INT L271_;
 BOOL L281_;
 TUPdTPINT L291_;
 TUPdTPINT L30;
 dTP L31;
 TUPdTPINT L32;
 TUPdTPINT L33;
 TUPdTPINT L34;
 INT L361_;
 TUPdTPINT L37;
 TUPdTPINT L38;
 TUPdTPINT L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPdTPINT) NULL))) {
  r = FMAPdT1801188375(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPdTPINT)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPdT1555535134;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPdT1224191190(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPdTPINT)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPdTPINT)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPdTPINT)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQdTP) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  L22 = elt_eq_e1;
  ret_val5 = (*dTP_is242312711[TAG(L22)])(L22, ((OB) elt_eq_e2));
  ret_val4 = ret_val5;
  if (ret_val4) {
   create_self = TUPdTPINT_zero;
   create_at1 = k;
   create_at2 = t;
   L23 = create_self;
   L23.t1 = create_at1;
   L24 = L23;
   L24.t2 = create_at2;
   ret_val6 = L24;
   L25 = ret_val6;
   SARR((FMAPdTPINT)r,h,L25); 
   ;
   ret_val = r;
   return ret_val;
  }
  L271_=INTPLUS(h,1); 
  h = L271_;
 }
 after_loop: ;
 L281_=(h)==(asm1); 
 if (L281_) {
  h = 0;
  while (1) {
   L291_=ARR((FMAPdTPINT)r,h); 
   L30=L291_;
   tk1 = L30.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val7 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val7) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   L31 = elt_eq_e11;
   ret_val9 = (*dTP_is242312711[TAG(L31)])(L31, ((OB) elt_eq_e21));
   ret_val8 = ret_val9;
   if (ret_val8) {
    create_self1 = TUPdTPINT_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val10 = L33;
    L34 = ret_val10;
    SARR((FMAPdTPINT)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdTPINT_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val11 = L38;
 L39 = ret_val11;
 SARR((FMAPdTPINT)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdTPINT FMAPdT1801188375(FMAPdTPINT self, INT n) {
 FMAPdTPINT ret_val;
 FMAPdTPINT r;
 FMAPdTPINT L11;
 FMAPdTPINT key_nil_self;
 dTP ret_val1;
 FMAPdTPINT key_nil_self1;
 dTP ret_val2;
 TUPdTPINT create_self = TUPdTPINT_zero;
 dTP create_at1;
 INT create_at2 = INT_zero;
 TUPdTPINT ret_val3 = TUPdTPINT_zero;
 INT L21 = INT_zero;
 FMAPdTPINT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdTPINT L10;
 TUPdTPINT L12;
 TUPdTPINT L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPdTPINT_struct)-sizeof(TUPdTPINT))+(L4)*sizeof(TUPdTPINT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPdTPINT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPdTPINT) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPdTPINT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdTPINT_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    create_at1 = ret_val2;
    create_at2 = INT_zero;
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((FMAPdTPINT)L11,L21,L13); 
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

FMAPdTPdLAYOUT FMAPdT1248247834(FMAPdTPdLAYOUT self, dTP k, dLAYOUT t) {
 FMAPdTPdLAYOUT ret_val;
 FMAPdTPdLAYOUT r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPdLAYOUT should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPdTPdLAYOUT key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPdTPdLAYOUT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPdTPdLAYOUT key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TUPdTPdLAYOUT create_self = TUPdTPdLAYOUT_zero;
 dTP create_at1;
 dLAYOUT create_at2;
 TUPdTPdLAYOUT ret_val6 = TUPdTPdLAYOUT_zero;
 FMAPdTPdLAYOUT is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPdTPdLAYOUT key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 TUPdTPdLAYOUT create_self1 = TUPdTPdLAYOUT_zero;
 dTP create_at11;
 dLAYOUT create_at21;
 TUPdTPdLAYOUT ret_val10 = TUPdTPdLAYOUT_zero;
 TUPdTPdLAYOUT create_self2 = TUPdTPdLAYOUT_zero;
 dTP create_at12;
 dLAYOUT create_at22;
 TUPdTPdLAYOUT ret_val11 = TUPdTPdLAYOUT_zero;
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
 TUPdTPdLAYOUT L201_;
 TUPdTPdLAYOUT L21;
 dTP L22;
 TUPdTPdLAYOUT L23;
 TUPdTPdLAYOUT L24;
 TUPdTPdLAYOUT L25;
 INT L271_;
 BOOL L281_;
 TUPdTPdLAYOUT L291_;
 TUPdTPdLAYOUT L30;
 dTP L31;
 TUPdTPdLAYOUT L32;
 TUPdTPdLAYOUT L33;
 TUPdTPdLAYOUT L34;
 INT L361_;
 TUPdTPdLAYOUT L37;
 TUPdTPdLAYOUT L38;
 TUPdTPdLAYOUT L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPdTPdLAYOUT) NULL))) {
  r = FMAPdT1552226099(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPdTPdLAYOUT)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPdT861897287;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPdT391465938(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPdTPdLAYOUT)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPdTPdLAYOUT)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPdTPdLAYOUT)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQdTP) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  L22 = elt_eq_e1;
  ret_val5 = (*dTP_is242312711[TAG(L22)])(L22, ((OB) elt_eq_e2));
  ret_val4 = ret_val5;
  if (ret_val4) {
   create_self = TUPdTPdLAYOUT_zero;
   create_at1 = k;
   create_at2 = t;
   L23 = create_self;
   L23.t1 = create_at1;
   L24 = L23;
   L24.t2 = create_at2;
   ret_val6 = L24;
   L25 = ret_val6;
   SARR((FMAPdTPdLAYOUT)r,h,L25); 
   ;
   ret_val = r;
   return ret_val;
  }
  L271_=INTPLUS(h,1); 
  h = L271_;
 }
 after_loop: ;
 L281_=(h)==(asm1); 
 if (L281_) {
  h = 0;
  while (1) {
   L291_=ARR((FMAPdTPdLAYOUT)r,h); 
   L30=L291_;
   tk1 = L30.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val7 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val7) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQdTP) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   L31 = elt_eq_e11;
   ret_val9 = (*dTP_is242312711[TAG(L31)])(L31, ((OB) elt_eq_e21));
   ret_val8 = ret_val9;
   if (ret_val8) {
    create_self1 = TUPdTPdLAYOUT_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val10 = L33;
    L34 = ret_val10;
    SARR((FMAPdTPdLAYOUT)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdTPdLAYOUT_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val11 = L38;
 L39 = ret_val11;
 SARR((FMAPdTPdLAYOUT)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdTPdLAYOUT FMAPdT1552226099(FMAPdTPdLAYOUT self, INT n) {
 FMAPdTPdLAYOUT ret_val;
 FMAPdTPdLAYOUT r;
 FMAPdTPdLAYOUT L11;
 FMAPdTPdLAYOUT key_nil_self;
 dTP ret_val1;
 FMAPdTPdLAYOUT key_nil_self1;
 dTP ret_val2;
 TUPdTPdLAYOUT create_self = TUPdTPdLAYOUT_zero;
 dTP create_at1;
 dLAYOUT create_at2;
 TUPdTPdLAYOUT ret_val3 = TUPdTPdLAYOUT_zero;
 INT L21 = INT_zero;
 FMAPdTPdLAYOUT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdTPdLAYOUT L10;
 TUPdTPdLAYOUT L12;
 TUPdTPdLAYOUT L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPdTPdLAYOUT_struct)-sizeof(TUPdTPdLAYOUT))+(L4)*sizeof(TUPdTPdLAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPdTPdLAYOUT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPdTPdLAYOUT) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI1328289538(((ELT_NILdTP) NULL));
 L7 = (ret_val1==((dTP) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPdTPdLAYOUT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdTPdLAYOUT_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI1328289538(((ELT_NILdTP) NULL));
    create_at1 = ret_val2;
    create_at2 = ((dLAYOUT) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((FMAPdTPdLAYOUT)L11,L21,L13); 
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

FMAPdTPdLAYOUT FMAPdT391465938(FMAPdTPdLAYOUT self) {
 FMAPdTPdLAYOUT ret_val;
 INT ns = INT_zero;
 FMAPdTPdLAYOUT r;
 FMAPdTPdLAYOUT L11;
 FMAPdTPdLAYOUT insert_pair_self;
 TUPdTPdLAYOUT insert_pair_p = TUPdTPdLAYOUT_zero;
 FMAPdTPdLAYOUT ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdTPdLAYOUT L9;
 L21_=ASIZE((FMAPdTPdLAYOUT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdT1552226099(self, ns);
 {
  struct FMAPdT2027173596_frame_struct other1_0;
FMAPdT2027173596_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdT2027173596(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdT1248247834(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FSTR FSTR_c1307336863(FSTR self, STR s) {
 FSTR ret_val;
 INT l = INT_zero;
 FSTR r = ((FSTR) NULL);
 FSTR create_self;
 FSTR ret_val1;
 FSTR L1;
 INT L2;
 OB L3;
 INT L4;
 BOOL L51_;
 FSTR L6;
 INT L7;
 OB L8;
 INT L9;
 FSTR L10;
 INT L11;
 OB L12;
 INT L13;
 if ((s==((STR) NULL))) {
  create_self = self;
  L2 = 16;
  L4=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
  L3=ZALLOC_LEAF_BIG(L4);
  if (L3==NULL) FATAL("Unable to allocate more memory");
  memset(L3,0,L4);
  ((OB)L3)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L3)->header.destroyed=0;
#endif

  L1 = ((FSTR) L3);
  L1->asize = L2;
  ret_val1 = L1;
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  l = STR_lengthrINT(s);
  L51_=(l)==(0); 
  if (L51_) {
   L7 = 1;
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
   r = L6;
  }
  else {
   L11 = l;
   L13=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L11)*sizeof(CHAR);
   L12=ZALLOC_LEAF_BIG(L13);
   if (L12==NULL) FATAL("Unable to allocate more memory");
   memset(L12,0,L13);
   ((OB)L12)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L12)->header.destroyed=0;
#endif

   L10 = ((FSTR) L12);
   L10->asize = L11;
   r = L10;
  }
  SATTR(r,loc,l);
  FSTRACOPYN(r,s,l); 
  ;
  ret_val = r;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSTR FSTR_c1998740379(FSTR self, INT sz) {
 FSTR ret_val;
 BOOL L11_;
 FSTR L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(sz)==(0); 
 if (L11_) {
  ret_val = ((FSTR) NULL);
  return ret_val;
 }
 else {
  L3 = sz;
  L5=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L3)*sizeof(CHAR);
  L4=ZALLOC_LEAF_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  memset(L4,0,L5);
  ((OB)L4)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FSTR) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSTR FSTR_p1410513982(FSTR self, FSTR s) {
 FSTR ret_val;
 FSTR r = ((FSTR) NULL);
 INT l = INT_zero;
 FSTR length_self;
 INT ret_val1 = INT_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 FSTR L8;
 INT L9;
 INT L101_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 OB L16;
 INT L17;
 BOOL L201_;
 INT L21;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 length_self = s;
 ret_val1 = FSTR_sizerINT(length_self);
 l = ret_val1;
 if ((self==((FSTR) NULL))) {
  L11_=INTTIMES(2,l); 
  r = FSTR_c1998740379(self, L11_);
 }
 else {
  L2 = ATTR(self,loc);
  L31_=INTPLUS(L2,l); 
  L4 = L31_;
  L51_=ASIZE((FSTR)self); 
  L6 = L51_;
  L71_=(L4)<(L6); 
  if (L71_) {
   r = self;
  }
  else {
   L101_=ASIZE((FSTR)self); 
   L12 = L101_;
   L131_=INTPLUS(L12,l); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L9 = L151_;
   L17=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
   L16=ZALLOC_LEAF_BIG(L17);
   if (L16==NULL) FATAL("Unable to allocate more memory");
   memset(L16,0,L17);
   ((OB)L16)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L16)->header.destroyed=0;
#endif

   L8 = ((FSTR) L16);
   L8->asize = L9;
   r = L8;
   SATTR(r,loc,ATTR(self,loc));
   FSTRACOPYF(r,self); 
   ;
   SYSDESTROY(self); 
   ;
  }
 }
 L201_=(l)==(0); 
 if (L201_) {
  ret_val = r;
  return ret_val;
 }
 L21 = ATTR(r,loc);
 L221_=INTPLUS(L21,l); 
 SATTR(r,loc,L221_);
 L23 = ATTR(r,loc);
 L241_=INTMINUS(L23,l); 
 L25 = L241_;
 FSTRACOPYIF(r,L25,s); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR FSTR_p1752847026(FSTR self, STR s) {
 FSTR ret_val;
 FSTR r = ((FSTR) NULL);
 INT l = INT_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 FSTR L8;
 INT L9;
 INT L101_;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 OB L16;
 INT L17;
 BOOL L201_;
 INT L21;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 l = STR_lengthrINT(s);
 if ((self==((FSTR) NULL))) {
  L11_=INTTIMES(2,l); 
  r = FSTR_c1998740379(self, L11_);
 }
 else {
  L2 = ATTR(self,loc);
  L31_=INTPLUS(L2,l); 
  L4 = L31_;
  L51_=ASIZE((FSTR)self); 
  L6 = L51_;
  L71_=(L4)<(L6); 
  if (L71_) {
   r = self;
  }
  else {
   L101_=ASIZE((FSTR)self); 
   L12 = L101_;
   L131_=INTPLUS(L12,l); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L9 = L151_;
   L17=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
   L16=ZALLOC_LEAF_BIG(L17);
   if (L16==NULL) FATAL("Unable to allocate more memory");
   memset(L16,0,L17);
   ((OB)L16)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L16)->header.destroyed=0;
#endif

   L8 = ((FSTR) L16);
   L8->asize = L9;
   r = L8;
   SATTR(r,loc,ATTR(self,loc));
   FSTRACOPYF(r,self); 
   ;
   SYSDESTROY(self); 
   ;
  }
 }
 L201_=(l)==(0); 
 if (L201_) {
  ret_val = r;
  return ret_val;
 }
 L21 = ATTR(r,loc);
 L221_=INTPLUS(L21,l); 
 SATTR(r,loc,L221_);
 L23 = ATTR(r,loc);
 L241_=INTMINUS(L23,l); 
 L25 = L241_;
 FSTRACOPYIS(r,L25,s); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR FSTR_p399773021(FSTR self, CHAR c) {
 FSTR ret_val;
 FSTR r = ((FSTR) NULL);
 INT l = INT_zero;
 FSTR create_self;
 FSTR ret_val1;
 FSTR aset_self;
 INT aset_ind = INT_zero;
 CHAR aset_val = CHAR_zero;
 FSTR L1;
 INT L2;
 OB L3;
 INT L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 FSTR L9;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 OB L14;
 INT L15;
 INT L161_;
 INT L191_;
 if ((self==((FSTR) NULL))) {
  create_self = self;
  L2 = 16;
  L4=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
  L3=ZALLOC_LEAF_BIG(L4);
  if (L3==NULL) FATAL("Unable to allocate more memory");
  memset(L3,0,L4);
  ((OB)L3)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L3)->header.destroyed=0;
#endif

  L1 = ((FSTR) L3);
  L1->asize = L2;
  ret_val1 = L1;
  r = ret_val1;
  l = 0;
 }
 else {
  L5 = ATTR(self,loc);
  L61_=ASIZE((FSTR)self); 
  L7 = L61_;
  L81_=(L5)<(L7); 
  if (L81_) {
   r = self;
   l = ATTR(r,loc);
  }
  else {
   L111_=ASIZE((FSTR)self); 
   L12 = L111_;
   L131_=INTTIMES(2,L12); 
   L10 = L131_;
   L15=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L10)*sizeof(CHAR);
   L14=ZALLOC_LEAF_BIG(L15);
   if (L14==NULL) FATAL("Unable to allocate more memory");
   memset(L14,0,L15);
   ((OB)L14)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L14)->header.destroyed=0;
#endif

   L9 = ((FSTR) L14);
   L9->asize = L10;
   r = L9;
   l = ATTR(self,loc);
   L161_=INTPLUS(l,1); 
   SATTR(r,loc,L161_);
   FSTRACOPYNF(r,self,l); 
   ;
   SYSDESTROY(self); 
   ;
  }
 }
 L191_=INTPLUS(l,1); 
 SATTR(r,loc,L191_);
 aset_self = r;
 aset_ind = l;
 aset_val = c;
 SARR((FSTR)aset_self,aset_ind,aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR FSTR_s1920457899(FSTR self, INT beg, INT num1) {
 FSTR ret_val;
 FSTR r;
 if ((self==((FSTR) NULL))) {
  ret_val = ((FSTR) NULL);
  return ret_val;
 }
 r = FSTR_c1998740379(((FSTR) NULL), num1);
 SATTR(r,loc,num1);
 AREFACOPYBNS(r,0,num1,beg,self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IDENT IDENT_512720063(IDENT_TBL self, STR q) {
 IDENT ret_val = IDENT_zero;
 INT h = INT_zero;
 IDENT e = IDENT_zero;
 IDENT e1 = IDENT_zero;
 IDENT_TBL query_hash_self;
 STR query_hash_s;
 INT ret_val1 = INT_zero;
 IDENT_TBL is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 IDENT_TBL query_test_self;
 STR query_test_s;
 IDENT query_test_i = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 IDENT_TBL is_elt_nil_self2;
 IDENT is_elt_nil_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self3;
 IDENT is_elt_nil_e3 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 IDENT_TBL query_test_self1;
 STR query_test_s1;
 IDENT query_test_i1 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 IDENT_TBL elt_nil_self;
 IDENT ret_val8 = IDENT_zero;
 ELT_NILIDENT elt_nil_self1;
 IDENT ret_val9 = IDENT_zero;
 IDENT e2 = IDENT_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 IDENT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 IDENT L141_;
 INT L151_;
 if ((self==((IDENT_TBL) NULL))) {
  ret_val = IDENT_zero;
  return ret_val;
 }
 query_hash_self = self;
 query_hash_s = q;
 ret_val1 = STR_hashrINT(query_hash_s);
 L1 = ret_val1;
 L21_=ASIZE((IDENT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((IDENT_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1.str==(STR)0);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   query_test_self = self;
   query_test_s = q;
   query_test_i = e;
   ret_val4 = STR_is111530248(query_test_s, query_test_i.str);
   if (ret_val4) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((IDENT_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=ARR((IDENT_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val6 = (is_elt_nil_e3.str==(STR)0);
   ret_val5 = ret_val6;
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    query_test_self1 = self;
    query_test_s1 = q;
    query_test_i1 = e1;
    ret_val7 = STR_is111530248(query_test_s1, query_test_i1.str);
    if (ret_val7) {
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
 elt_nil_self1 = ((ELT_NILIDENT) NULL);
 ret_val9 = IDENT_zero;
 ret_val8 = ret_val9;
 ret_val = ret_val8;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IDENT_TBL IDENT_526982742(IDENT_TBL self, INT n) {
 IDENT_TBL ret_val;
 IDENT_TBL r;
 IDENT_TBL L11;
 IDENT_TBL elt_nil_self;
 IDENT ret_val1 = IDENT_zero;
 ELT_NILIDENT elt_nil_self1;
 IDENT ret_val2 = IDENT_zero;
 IDENT e = IDENT_zero;
 IDENT_TBL elt_nil_self2;
 IDENT ret_val3 = IDENT_zero;
 ELT_NILIDENT elt_nil_self3;
 IDENT ret_val4 = IDENT_zero;
 IDENT e1 = IDENT_zero;
 INT L21 = INT_zero;
 IDENT_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 IDENT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct IDENT_TBL_struct)-sizeof(IDENT))+(L4)*sizeof(IDENT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=IDENT_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((IDENT_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILIDENT) NULL);
 ret_val2 = IDENT_zero;
 ret_val1 = ret_val2;
 L7 = (ret_val1.str==(STR)0);
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((IDENT_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILIDENT) NULL);
    ret_val4 = IDENT_zero;
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((IDENT_TBL)L11,L21,L10); 
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

IDENT_TBL IDENT_741964429(IDENT_TBL self) {
 IDENT_TBL ret_val;
 IDENT_TBL r;
 IDENT_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 IDENT L9;
 L21_=ASIZE((IDENT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = IDENT_526982742(self, L81_);
 {
  struct IDENT_119982571_frame_struct other1_0;
IDENT_119982571_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = IDENT_119982571(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = IDENT_81729787(r, L9);
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

IDENT_TBL IDENT_81729787(IDENT_TBL self, IDENT e) {
 IDENT_TBL ret_val;
 IDENT_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 IDENT te = IDENT_zero;
 IDENT te1 = IDENT_zero;
 IDENT_TBL is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 IDENT_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 IDENT_TBL elt_hash_self;
 IDENT elt_hash_i = IDENT_zero;
 INT ret_val4 = INT_zero;
 IDENT_TBL is_elt_nil_self2;
 IDENT is_elt_nil_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self3;
 IDENT is_elt_nil_e3 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 IDENT_TBL elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 IDENT_TBL is_elt_nil_self4;
 IDENT is_elt_nil_e4 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self5;
 IDENT is_elt_nil_e5 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 IDENT_TBL elt_eq_self2;
 IDENT elt_eq_e12 = IDENT_zero;
 IDENT elt_eq_e22 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 ELT_EQIDENT elt_eq_self3;
 IDENT elt_eq_e13 = IDENT_zero;
 IDENT elt_eq_e23 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val14 = BOOL_zero;
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
 IDENT L161_;
 INT L181_;
 INT L191_;
 INT L20;
 BOOL L211_;
 IDENT L221_;
 INT L241_;
 INT L26;
 INT L271_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1.str==(STR)0);
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((IDENT_TBL) NULL))) {
  r = IDENT_526982742(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((IDENT_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = IDENT_105403148;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = IDENT_741964429(r);
  }
 }
 L101_=ASIZE((IDENT_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_i = e;
 ret_val4 = STR_hashrINT(elt_hash_i.str);
 L12 = ret_val4;
 L131_=INTMINUS(asz,2); 
 L14 = L131_;
 L151_=L12&L14; 
 orig_h = L151_;
 h = orig_h;
 while (1) {
  L161_=ARR((IDENT_TBL)r,h); 
  te = L161_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILIDENT) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val6 = (is_elt_nil_e3.str==(STR)0);
  ret_val5 = ret_val6;
  if (ret_val5) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQIDENT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_i = elt_eq_e21;
   ret_val9 = (is_eq_self.str==is_eq_i.str);
   ret_val8 = ret_val9;
   ret_val7 = ret_val8;
   if (ret_val7) {
    SARR((IDENT_TBL)r,h,e); 
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
   L221_=ARR((IDENT_TBL)r,h); 
   te1 = L221_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val11 = (is_elt_nil_e5.str==(STR)0);
   ret_val10 = ret_val11;
   if (ret_val10) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQIDENT) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_i1 = elt_eq_e23;
    ret_val14 = (is_eq_self1.str==is_eq_i1.str);
    ret_val13 = ret_val14;
    ret_val12 = ret_val13;
    if (ret_val12) {
     SARR((IDENT_TBL)r,h,e); 
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
 SARR((IDENT_TBL)r,h,e); 
 ;
 L26 = ATTR(r,hsize);
 L271_=INTPLUS(L26,1); 
 SATTR(r,hsize,L271_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC ELT_TB462529002(ELT_TBL self) {
 IFC ret_val;
 SIG_TBL st = ((SIG_TBL) NULL);
 ELT e = ((ELT) NULL);
 ELT_TBL L11;
 IFC create_self;
 SIG_TBL create_sigs;
 dTP create_tp;
 IFC ret_val1;
 IFC r;
 BOOL L2;
 INT L3;
 BOOL L41_;
 ELT L5;
 BOOL L6;
 BOOL L71_;
 IFC L8;
 OB L9;
 if ((self==((ELT_TBL) NULL))) {
  L2 = TRUE;
 } else {
  L3 = ATTR(self,hsize);
  L41_=(L3)==(0); 
  L2 = L41_;
 }
 if (L2) {
  ret_val = ((IFC) NULL);
  return ret_val;
 }
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L5 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L5;
   L6 = ATTR(e,is_private);
   L71_=!(L6); 
   if (L71_) {
    st = SIG_TB1438251609(st, ATTR(e,sig1));
   }
  }
 }
 after_loop: ;
 create_self = ((IFC) NULL);
 create_sigs = st;
 create_tp = ATTR(ATTR(e,sig1),tp);
 L9=ZALLOC(sizeof(struct IFC_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=IFC_tag;
 L8 = ((IFC) L9);
 r = L8;
 SATTR(r,sigs,create_sigs);
 SATTR(r,tp,create_tp);
 ret_val1 = r;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT ELT_TBL_sizerINT(ELT_TBL self) {
 INT ret_val = INT_zero;
 if ((self==((ELT_TBL) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FMAPdT1077886323(FMAPdTPINT self) {
 INT ret_val = INT_zero;
 if ((self==((FMAPdTPINT) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FMAPdT165518335(FMAPdTPINT self, dTP k) {
 INT ret_val = INT_zero;
 INT h = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPINT key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdTPINT key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 FMAPdTPINT is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FMAPdTPINT key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdTPINT L71_;
 TUPdTPINT L8;
 dTP L9;
 TUPdTPINT L101_;
 TUPdTPINT L11;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPdTPINT L181_;
 TUPdTPINT L19;
 dTP L20;
 TUPdTPINT L221_;
 TUPdTPINT L23;
 INT L241_;
 if ((self==((FMAPdTPINT) NULL))) {
  ret_val = INT_zero;
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdTPINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdTPINT)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQdTP) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   L9 = elt_eq_e1;
   ret_val4 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) elt_eq_e2));
   ret_val3 = ret_val4;
   if (ret_val3) {
    L101_=ARR((FMAPdTPINT)self,h); 
    L11=L101_;
    ret_val = L11.t2;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPdTPINT)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPdTPINT)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQdTP) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    L20 = elt_eq_e11;
    ret_val7 = (*dTP_is242312711[TAG(L20)])(L20, ((OB) elt_eq_e21));
    ret_val6 = ret_val7;
    if (ret_val6) {
     L221_=ARR((FMAPdTPINT)self,h); 
     L23=L221_;
     ret_val = L23.t2;
     return ret_val;
    }
   }
   L241_=INTPLUS(h,1); 
   h = L241_;
  }
  after_loop1: ;
 }
 ret_val = INT_zero;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSTR_hash0rINT(FSTR self) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 FSTR L11;
 INT i = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 INT L2;
 CHAR L31_;
 CHAR L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L121_;
 r = 0;
 {
  struct FSTR_indbrINT_frame_struct other1_0;
FSTR_indbrINT_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSTR_indbrINT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   i = L2;
   aget_self = self;
   aget_ind = i;
   L31_=ARR((FSTR)aget_self,aget_ind); 
   ret_val1 = L31_;
   L4 = ret_val1;
   L51_=(INT)((unsigned char)L4); 
   L6 = L51_;
   L71_=i&15; 
   L8 = L71_;
   L91_=((unsigned)L6)<<((unsigned)L8); 
   L10 = L91_;
   L121_=r^L10; 
   r = L121_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSTR_hashrINT(FSTR self) {
 INT ret_val = INT_zero;
 INT i = INT_zero;
 INT r = INT_zero;
 FSTR length_self;
 INT ret_val1 = INT_zero;
 FSTR length_self1;
 INT ret_val2 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 CHAR L51_;
 CHAR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L101_;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 CHAR L181_;
 CHAR L19;
 INT L201_;
 INT L22;
 INT L231_;
 INT L241_;
 if ((self==((FSTR) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 length_self = self;
 ret_val1 = FSTR_sizerINT(length_self);
 L1 = ret_val1;
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = 0;
  return ret_val;
 }
 length_self1 = self;
 ret_val2 = FSTR_sizerINT(length_self1);
 L3 = ret_val2;
 L41_=INTMINUS(L3,1); 
 i = L41_;
 aget_self = self;
 aget_ind = i;
 L51_=ARR((FSTR)aget_self,aget_ind); 
 ret_val3 = L51_;
 L6 = ret_val3;
 L71_=(INT)((unsigned char)L6); 
 L8 = L71_;
 L91_=((unsigned)532415+(unsigned)L8); 
 r = L91_;
 L101_=INTMINUS(i,1); 
 i = L101_;
 while (1) {
  L111_=(i)<(0); 
  L12 = L111_;
  L131_=!(L12); 
  if (L131_) {
  }
  else {
   goto after_loop;
  }
  L141_=((unsigned)r*(unsigned)1664525); 
  L15 = L141_;
  L161_=((unsigned)L15+(unsigned)1013904223); 
  L17 = L161_;
  aget_self1 = self;
  aget_ind1 = i;
  L181_=ARR((FSTR)aget_self1,aget_ind1); 
  ret_val4 = L181_;
  L19 = ret_val4;
  L201_=(INT)((unsigned char)L19); 
  L22 = L201_;
  L231_=((unsigned)L17+(unsigned)L22); 
  r = L231_;
  L241_=INTMINUS(i,1); 
  i = L241_;
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSTR_sizerINT(FSTR self) {
 INT ret_val = INT_zero;
 if ((self==((FSTR) NULL))) {
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

INT SIG_TBL_sizerINT(SIG_TBL self) {
 INT ret_val = INT_zero;
 if ((self==((SIG_TBL) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SIG SIG_TB1067194561(SIG_TBL self, SIG s) {
 SIG ret_val;
 SIG_TBL L11;
 IDENT L21 = IDENT_zero;
 SIG r;
 SIG L3;
 {
  struct SIG_TB788365833_frame_struct other1_0;
SIG_TB788365833_frame noname1 = &other1_0;
  L11 = self;
  L21 = ATTR(s,name1);
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = SIG_TB788365833(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = L3;
   if (SIG_co165522953(r, s)) {
    ret_val = r;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = ((SIG) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG_TBL SIG_TB1167290220(SIG_TBL self, INT n) {
 SIG_TBL ret_val;
 SIG_TBL r;
 SIG_TBL L11;
 SIG_TBL elt_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 SIG_TBL elt_nil_self2;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self3;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 INT L21 = INT_zero;
 SIG_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 SIG L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct SIG_TBL_struct)-sizeof(SIG))+(L4)*sizeof(SIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=SIG_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((SIG_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSIG) NULL);
 ret_val2 = ((SIG) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((SIG) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((SIG_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((SIG_TBL)L11,L21,(SIG)L10); 
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

SIG_TBL SIG_TB1438251609(SIG_TBL self, SIG e) {
 SIG_TBL ret_val;
 SIG_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 SIG te;
 SIG te1;
 SIG_TBL is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 SIG_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 SIG_TBL elt_hash_self;
 SIG elt_hash_s;
 INT ret_val4 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val5 = INT_zero;
 INT x = INT_zero;
 SIG_TBL is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 SIG_TBL elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val10 = BOOL_zero;
 SIG_TBL is_elt_nil_self4;
 SIG is_elt_nil_e4;
 BOOL ret_val11 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self5;
 SIG is_elt_nil_e5;
 BOOL ret_val12 = BOOL_zero;
 SIG_TBL elt_eq_self2;
 SIG elt_eq_e12;
 SIG elt_eq_e22;
 BOOL ret_val13 = BOOL_zero;
 ELT_EQSIG elt_eq_self3;
 SIG elt_eq_e13;
 SIG elt_eq_e23;
 BOOL ret_val14 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
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
 INT L201_;
 SIG L211_;
 INT L231_;
 INT L241_;
 INT L25;
 BOOL L261_;
 SIG L271_;
 INT L291_;
 INT L31;
 INT L321_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILSIG) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((SIG) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((SIG_TBL) NULL))) {
  r = SIG_TB1167290220(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((SIG_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = SIG_TB1720612202;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = SIG_TB543803525(r);
  }
 }
 L101_=ASIZE((SIG_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_s = e;
 hash_self = ATTR(elt_hash_s,name1);
 L12 = hash_self.str;
 L131_=SYSID(L12); 
 x = L131_;
 L141_=((unsigned)x*(unsigned)1664525); 
 L15 = L141_;
 L161_=((unsigned)L15+(unsigned)1013904223); 
 ret_val5 = L161_;
 ret_val4 = ret_val5;
 L17 = ret_val4;
 L181_=INTMINUS(asz,2); 
 L19 = L181_;
 L201_=L17&L19; 
 orig_h = L201_;
 h = orig_h;
 while (1) {
  L211_=(SIG)ARR((SIG_TBL)r,h); 
  te = L211_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILSIG) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val7 = (is_elt_nil_e3==((SIG) NULL));
  ret_val6 = ret_val7;
  if (ret_val6) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSIG) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_s = elt_eq_e21;
   ret_val10 = (is_eq_self==is_eq_s);
   ret_val9 = ret_val10;
   ret_val8 = ret_val9;
   if (ret_val8) {
    SARR((SIG_TBL)r,h,(SIG)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L231_=INTPLUS(h,1); 
  h = L231_;
 }
 after_loop: ;
 L241_=INTMINUS(asz,1); 
 L25 = L241_;
 L261_=(h)==(L25); 
 if (L261_) {
  h = 0;
  while (1) {
   L271_=(SIG)ARR((SIG_TBL)r,h); 
   te1 = L271_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILSIG) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val12 = (is_elt_nil_e5==((SIG) NULL));
   ret_val11 = ret_val12;
   if (ret_val11) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQSIG) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_s1 = elt_eq_e23;
    ret_val15 = (is_eq_self1==is_eq_s1);
    ret_val14 = ret_val15;
    ret_val13 = ret_val14;
    if (ret_val13) {
     SARR((SIG_TBL)r,h,(SIG)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L291_=INTPLUS(h,1); 
   h = L291_;
  }
  after_loop1: ;
 }
 SARR((SIG_TBL)r,h,(SIG)e); 
 ;
 L31 = ATTR(r,hsize);
 L321_=INTPLUS(L31,1); 
 SATTR(r,hsize,L321_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG_TBL SIG_TB543803525(SIG_TBL self) {
 SIG_TBL ret_val;
 SIG_TBL r;
 SIG_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SIG L9;
 L21_=ASIZE((SIG_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = SIG_TB1167290220(self, L81_);
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = SIG_TB1438251609(r, L9);
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

STR ELT_TBL_strrSTR(ELT_TBL self) {
 STR ret_val;
 dTP tp = ((dTP) NULL);
 ARRAYSTR a;
 INT i = INT_zero;
 ELT_TBL L11;
 SIG sg;
 STR st;
 ARRAYSTR L21;
 STR s;
 ARRAYSTR L31;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 ARRAYSTR sort_self;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 INT L41 = INT_zero;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val10;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
INT i_L91_=0;
 ELT L10;
 ELT L12;
 BOOL L13;
 BOOL L141_;
 extern STR name6;
 extern STR name7;
 extern STR interfaceofclass;
 dTP L15;
 extern STR is;
 BOOL L16;
 BOOL L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_br;
 STR aL221_;
 extern STR name8;
 STR L23;
 extern STR name9;
 INT L241_;
 extern STR end1;
 create_self = ((ARRAYSTR) NULL);
 create_n = ELT_TBL_sizerINT(self);
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
 a = ret_val1;
 i = 0;
 {
  struct ELT_TBL_eltbrELT_frame_struct other1_0;
ELT_TBL_eltbrELT_frame noname1 = &other1_0;
  BOOL f_L91_ = TRUE;
  L11 = self;
  L21 = a;
  noname1->self = L11;
  noname1->state = 0;
  L91_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  i_L91_=0;
  while (1) {
   L10 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L12=L10;
   sg = ATTR(L12,sig1);
   st = ATTR(sg,str);
   tp = ATTR(sg,tp);
   L13 = (ATTR(sg,srcsig)==((SIG) NULL));
   L141_=!(L13); 
   if (L141_) {
    plus_self = st;
    plus_sarg = ((STR) &name6);
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val2;
    plus_sarg1 = ATTR(ATTR(sg,srcsig),str);
    ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val3;
    plus_sarg2 = ((STR) &name7);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    st = ret_val4;
   }
   if(i_L91_>=L91_br)  goto after_loop; 
   SARR((ARRAYSTR)L21,i_L91_,(STR)st); i_L91_++;
   ;
  }
 }
 after_loop: ;
 plus_self3 = ((STR) &interfaceofclass);
 L15 = tp;
 plus_sarg3 = (*dTP_strrSTR[TAG(L15)])(L15);
 ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
 plus_self4 = ret_val5;
 plus_sarg4 = ((STR) &is);
 ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
 s = ret_val6;
 sort_self = a;
 L16 = (sort_self==((ARRAYSTR) NULL));
 L171_=!(L16); 
 if (L171_) {
  L181_=ASIZE((ARRAYSTR)sort_self); 
  L19 = L181_;
  L201_=INTMINUS(L19,1); 
  ARRAYS1450857938(sort_self, 0, L201_);
 }
 {
  BOOL f_L221_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L31 = a;
  L221_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
  while (1) {
   plus_self5 = s;
   plus_sarg5 = ((STR) &name8);
   ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val7;
   if(L41>=L221_br)  goto after_loop1; 
   aL221_=ARR((ARRAYSTR)L31,L41); 
   plus_sarg6 = aL221_;
   ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val8;
   plus_sarg7 = ((STR) &name9);
   ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
   s = ret_val9;
   L241_=INTPLUS(L41,1); 
   L41 = L241_;
  }
 }
 after_loop1: ;
 plus_self8 = s;
 plus_sarg8 = ((STR) &end1);
 ret_val10 = STR_ap2004550586(plus_self8, plus_sarg8);
 s = ret_val10;
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPdT1371742936(FMAPdTPdLAYOUT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPdLAYOUT p = TUPdTPdLAYOUT_zero;
 dTP k;
 dLAYOUT e;
 FMAPdTPdLAYOUT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 FSTR str_self;
 STR ret_val2;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPdLAYOUT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 dTP L8;
 FSTR L9;
 extern STR name15;
 extern STR name10;
 FSTR L10;
 dLAYOUT L12;
 FSTR L13;
 extern STR name10;
 extern STR name10;
 INT L141_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPdTPdLAYOUT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPdTPdLAYOUT)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  ret_val1 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = k;
   L9 = FSTR_p1752847026(L7, (*dTP_strrSTR[TAG(L8)])(L8));
   res = FSTR_p1752847026(L9, ((STR) &name15));
   L10 = FSTR_p1752847026(res, ((STR) &name10));
   L12 = e;
   L13 = FSTR_p1752847026(L10, (*dLAYOUT_strrSTR[TAG(L12)])(L12));
   res = FSTR_p1752847026(L13, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L141_=INTPLUS(i,1); 
  i = L141_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPdT1912217709(FMAPdTPINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPINT p = TUPdTPINT_zero;
 dTP k;
 INT e = INT_zero;
 FMAPdTPINT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 FSTR str_self2;
 STR ret_val5;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPINT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 dTP L8;
 FSTR L9;
 extern STR name15;
 extern STR name10;
 FSTR L10;
 BOOL L12;
 BOOL L131_;
 FSTR L14;
 extern STR name10;
 extern STR name10;
 INT L151_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPdTPINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPdTPINT)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  ret_val1 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = k;
   L9 = FSTR_p1752847026(L7, (*dTP_strrSTR[TAG(L8)])(L8));
   res = FSTR_p1752847026(L9, ((STR) &name15));
   L10 = FSTR_p1752847026(res, ((STR) &name10));
   str_self = e;
   clear_self = buf1;
   L12 = (clear_self==((FSTR) NULL));
   L131_=!(L12); 
   if (L131_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   L14 = FSTR_p1752847026(L10, ret_val2);
   res = FSTR_p1752847026(L14, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L151_=INTPLUS(i,1); 
  i = L151_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self2 = res;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPdT2142836224(FMAPdT953816637 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTP1710233051 p = TUPdTP1710233051_zero;
 dTP k;
 FMAPID739086519 e;
 FMAPdT953816637 is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 FSTR str_self;
 STR ret_val2;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTP1710233051 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 dTP L8;
 FSTR L9;
 extern STR name15;
 extern STR name10;
 FSTR L10;
 FSTR L12;
 extern STR name10;
 extern STR name10;
 INT L131_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPdT953816637)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPdT953816637)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  ret_val1 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = k;
   L9 = FSTR_p1752847026(L7, (*dTP_strrSTR[TAG(L8)])(L8));
   res = FSTR_p1752847026(L9, ((STR) &name15));
   L10 = FSTR_p1752847026(res, ((STR) &name10));
   L12 = FSTR_p1752847026(L10, FMAPID1895267434(e));
   res = FSTR_p1752847026(L12, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L131_=INTPLUS(i,1); 
  i = L131_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPdT966432485(FMAPdTPCODE_FILE self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdTPCODE_FILE p = TUPdTPCODE_FILE_zero;
 dTP k;
 CODE_FILE e;
 FMAPdTPCODE_FILE is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 FSTR str_self;
 STR ret_val2;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdTPCODE_FILE L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 dTP L8;
 FSTR L9;
 extern STR name15;
 extern STR name10;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPdTPCODE_FILE)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPdTPCODE_FILE)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  ret_val1 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = k;
   L9 = FSTR_p1752847026(L7, (*dTP_strrSTR[TAG(L8)])(L8));
   res = FSTR_p1752847026(L9, ((STR) &name15));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L101_=INTPLUS(i,1); 
  i = L101_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val2;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMT_ER330399541(FMT_ERROR self) {
 return self.str;
}


#undef IS_ITER
#define IS_ITER 0

STR FSTR_strrSTR(FSTR self) {
 STR ret_val;
 ret_val = STR_fr1097270334(((STR) NULL), self);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FSTR_t603468843(FSTR self) {
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
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self2 = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 STR plus_self1;
 INT plus_arg1 = INT_zero;
 STR ret_val7;
 STR s1;
 INT str_self3 = INT_zero;
 STR ret_val8;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val9;
 FSTR str_self4;
 STR ret_val10;
 STR plus_self2;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val11;
 STR s2;
 CHAR str_self5 = CHAR_zero;
 STR ret_val12;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val13;
 STR r1;
 STR plus_self3;
 INT plus_arg3 = INT_zero;
 STR ret_val14;
 STR s3;
 INT str_self6 = INT_zero;
 STR ret_val15;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val16;
 FSTR str_self7;
 STR ret_val17;
 BOOL L1;
 BOOL L21_;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L8;
 BOOL L91_;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 BOOL L15;
 BOOL L161_;
 str_self = FSTR_sizerINT(self);
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
 plus_self = ret_val1;
 plus_arg = ':';
 str_self2 = plus_arg;
 create_self = ((STR) NULL);
 create_c = str_self2;
 L4 = 1;
 L6=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L4)*sizeof(CHAR);
 L5=ZALLOC_LEAF_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,L6);
 ((OB)L5)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((STR) L5);
 L3->asize = L4;
 r = L3;
 SARR((STR)r,0,create_c); 
 ;
 ret_val6 = r;
 ret_val5 = ret_val6;
 s = ret_val5;
 ret_val4 = STR_ap1077157958(plus_self, s, TRUE);
 plus_self1 = ret_val4;
 plus_arg1 = FSTR_hashrINT(self);
 str_self3 = plus_arg1;
 clear_self1 = buf2;
 L8 = (clear_self1==((FSTR) NULL));
 L91_=!(L8); 
 if (L91_) {
  SATTR(clear_self1,loc,0);
 }
 str_in_self1 = str_self3;
 str_in_s1 = buf2;
 ret_val9 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
 buf2 = ret_val9;
 str_self4 = buf2;
 ret_val10 = STR_fr1097270334(((STR) NULL), str_self4);
 ret_val8 = ret_val10;
 s1 = ret_val8;
 ret_val7 = STR_ap1077157958(plus_self1, s1, TRUE);
 plus_self2 = ret_val7;
 plus_arg2 = ':';
 str_self5 = plus_arg2;
 create_self1 = ((STR) NULL);
 create_c1 = str_self5;
 L11 = 1;
 L13=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L11)*sizeof(CHAR);
 L12=ZALLOC_LEAF_BIG(L13);
 if (L12==NULL) FATAL("Unable to allocate more memory");
 memset(L12,0,L13);
 ((OB)L12)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L12)->header.destroyed=0;
#endif

 L10 = ((STR) L12);
 L10->asize = L11;
 r1 = L10;
 SARR((STR)r1,0,create_c1); 
 ;
 ret_val13 = r1;
 ret_val12 = ret_val13;
 s2 = ret_val12;
 ret_val11 = STR_ap1077157958(plus_self2, s2, TRUE);
 plus_self3 = ret_val11;
 plus_arg3 = FSTR_hash0rINT(self);
 str_self6 = plus_arg3;
 clear_self2 = buf3;
 L15 = (clear_self2==((FSTR) NULL));
 L161_=!(L15); 
 if (L161_) {
  SATTR(clear_self2,loc,0);
 }
 str_in_self2 = str_self6;
 str_in_s2 = buf3;
 ret_val16 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
 buf3 = ret_val16;
 str_self7 = buf3;
 ret_val17 = STR_fr1097270334(((STR) NULL), str_self7);
 ret_val15 = ret_val17;
 s3 = ret_val15;
 ret_val14 = STR_ap1077157958(plus_self3, s3, TRUE);
 ret_val = ret_val14;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR SIG_TBL_strrSTR(SIG_TBL self) {
 STR ret_val;
 ARRAYSTR a;
 SIG_TBL L11;
 ARRAYSTR L21;
 STR s;
 INT i = INT_zero;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 INT L31 = INT_zero;
 ARRAYSTR sort_self;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 ARRAYSTR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L81_br;
 SIG L9;
 SIG L10;
 STR L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 INT L161_;
 INT L17;
 INT L181_;
 extern STR name1;
 INT L191_;
 INT L20;
 BOOL L221_;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 extern STR name16;
 STR L261_;
 INT L271_;
 create_self = ((ARRAYSTR) NULL);
 create_n = SIG_TBL_sizerINT(self);
 L5 = create_n;
 L7=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L5)*sizeof(STR);
 L6=ZALLOC_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((ARRAYSTR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 a = ret_val1;
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  BOOL f_L81_ = TRUE;
  L11 = self;
  /* loop index variable */
  L31 = 0;
  L21 = a;
  noname1->self = L11;
  noname1->state = 0;
  L81_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  while (1) {
   if(L31>=L81_br)  goto after_loop; 
   L9 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L10=L9;
   L12 = ATTR(L10,str);
   SARR((ARRAYSTR)L21,L31,(STR)L12); 
   ;
   L131_=INTPLUS(L31,1); 
   L31 = L131_;
  }
 }
 after_loop: ;
 sort_self = a;
 L14 = (sort_self==((ARRAYSTR) NULL));
 L151_=!(L14); 
 if (L151_) {
  L161_=ASIZE((ARRAYSTR)sort_self); 
  L17 = L161_;
  L181_=INTMINUS(L17,1); 
  ARRAYS1450857938(sort_self, 0, L181_);
 }
 s = ((STR) &name1);
 i = 0;
 while (1) {
  L191_=ASIZE((ARRAYSTR)a); 
  L20 = L191_;
  L221_=(i)<(L20); 
  if (L221_) {
  }
  else {
   goto after_loop1;
  }
  L231_=(i)==(0); 
  L24 = L231_;
  L251_=!(L24); 
  if (L251_) {
   plus_self = s;
   plus_sarg = ((STR) &name16);
   ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
   s = ret_val2;
  }
  plus_self1 = s;
  L261_=(STR)ARR((ARRAYSTR)a,i); 
  plus_sarg1 = L261_;
  ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
  s = ret_val3;
  L271_=INTPLUS(i,1); 
  i = L271_;
 }
 after_loop1: ;
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dLAYOUT FMAPdT7482853(FMAPdTPdLAYOUT self, dTP k) {
 dLAYOUT ret_val;
 INT h = INT_zero;
 dTP tk;
 dTP tk1;
 FMAPdTPdLAYOUT key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdTPdLAYOUT is_key_nil_self;
 dTP is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdTPdLAYOUT key_eq_self;
 dTP key_eq_e1;
 dTP key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQdTP elt_eq_self;
 dTP elt_eq_e1;
 dTP elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 FMAPdTPdLAYOUT is_key_nil_self1;
 dTP is_key_nil_e1;
 BOOL ret_val5 = BOOL_zero;
 FMAPdTPdLAYOUT key_eq_self1;
 dTP key_eq_e11;
 dTP key_eq_e21;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQdTP elt_eq_self1;
 dTP elt_eq_e11;
 dTP elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdTPdLAYOUT L71_;
 TUPdTPdLAYOUT L8;
 dTP L9;
 TUPdTPdLAYOUT L101_;
 TUPdTPdLAYOUT L11;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPdTPdLAYOUT L181_;
 TUPdTPdLAYOUT L19;
 dTP L20;
 TUPdTPdLAYOUT L221_;
 TUPdTPdLAYOUT L23;
 INT L241_;
 if ((self==((FMAPdTPdLAYOUT) NULL))) {
  ret_val = ((dLAYOUT) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdTPdLAYOUT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdTPdLAYOUT)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQdTP) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   L9 = elt_eq_e1;
   ret_val4 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) elt_eq_e2));
   ret_val3 = ret_val4;
   if (ret_val3) {
    L101_=ARR((FMAPdTPdLAYOUT)self,h); 
    L11=L101_;
    ret_val = L11.t2;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPdTPdLAYOUT)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPdTPdLAYOUT)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val5 = ELT_NI125473534(((ELT_NILdTP) NULL), is_key_nil_e1);
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQdTP) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    L20 = elt_eq_e11;
    ret_val7 = (*dTP_is242312711[TAG(L20)])(L20, ((OB) elt_eq_e21));
    ret_val6 = ret_val7;
    if (ret_val6) {
     L221_=ARR((FMAPdTPdLAYOUT)self,h); 
     L23=L221_;
     ret_val = L23.t2;
     return ret_val;
    }
   }
   L241_=INTPLUS(h,1); 
   h = L241_;
  }
  after_loop1: ;
 }
 ret_val = ((dLAYOUT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void FSTR_to_reverse(FSTR self) {
 INT L11 = INT_zero;
 INT i = INT_zero;
 INT u = INT_zero;
 CHAR t = CHAR_zero;
 INT L21 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR aset_self;
 INT aset_ind = INT_zero;
 CHAR aset_val = CHAR_zero;
 FSTR aset_self1;
 INT aset_ind1 = INT_zero;
 CHAR aset_val1 = CHAR_zero;
 INT L4;
 INT L5;
 INT L61_;
 INT aL31_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 CHAR L121_;
 CHAR L131_;
 INT L161_;
 if ((self==((FSTR) NULL))) {
  return;
 }
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  while (1) {
   if (f_L31_) {
    f_L31_ = FALSE;
    L5 = ATTR(self,loc);
    L61_=INTDIV(L5,2); 
    L11 = L61_;
   }
   if(L21>=L11)  goto after_loop; 
   aL31_=L21; 
   i = aL31_;
   L7 = ATTR(self,loc);
   L81_=INTMINUS(L7,i); 
   L9 = L81_;
   L101_=INTMINUS(L9,1); 
   u = L101_;
   aget_self = self;
   aget_ind = i;
   L121_=ARR((FSTR)aget_self,aget_ind); 
   ret_val = L121_;
   t = ret_val;
   aset_self = self;
   aset_ind = i;
   aget_self1 = self;
   aget_ind1 = u;
   L131_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val1 = L131_;
   aset_val = ret_val1;
   SARR((FSTR)aset_self,aset_ind,aset_val); 
   ;
   aset_self1 = self;
   aset_ind1 = u;
   aset_val1 = t;
   SARR((FSTR)aset_self1,aset_ind1,aset_val1); 
   ;
   L161_=INTPLUS(L21,1); 
   L21 = L161_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

CHAR FSTR_eltbrCHAR(FSTR_eltbrCHAR_frame frame) {
 CHAR dummy = CHAR_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 CHAR L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FSTR) NULL));
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
   L41_=ARR((FSTR)frame->aget_self,frame->aget_ind); 
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

ELT ELT_TB431570775(ELT_TB431570775_frame frame) {
 ELT dummy = ((ELT) NULL);
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 ELT L121_;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 ELT L191_;
 INT L201_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 if ((frame->self==((ELT_TBL) NULL))) {
  frame->state = -1;
  return;
 }
 frame->query_hash_self = frame->self;
 frame->query_hash_i = frame->arg1;
 frame->hash_self = frame->query_hash_i;
 L1 = frame->hash_self.str;
 L21_=SYSID(L1); 
 frame->x = L21_;
 L31_=((unsigned)frame->x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 frame->ret_val1 = L51_;
 frame->ret_val = frame->ret_val1;
 L6 = frame->ret_val;
 L71_=ASIZE((ELT_TBL)frame->self); 
 L8 = L71_;
 L91_=INTMINUS(L8,2); 
 L10 = L91_;
 L111_=L6&L10; 
 frame->h = L111_;
 while (1) {
  L121_=(ELT)ARR((ELT_TBL)frame->self,frame->h); 
  frame->e = L121_;
  frame->is_elt_nil_self = frame->self;
  frame->is_elt_nil_e = frame->e;
  frame->is_elt_nil_self1 = ((ELT_NILELT) NULL);
  frame->is_elt_nil_e1 = frame->is_elt_nil_e;
  frame->ret_val3 = (frame->is_elt_nil_e1==((ELT) NULL));
  frame->ret_val2 = frame->ret_val3;
  if (frame->ret_val2) {
   goto after_loop;
  }
  else {
   if (ELT_TB1905689409(frame->self, frame->arg1, frame->e)) {
    frame->state = 1;
    return frame->e;
    state1:;
   }
  }
  L131_=INTPLUS(frame->h,1); 
  frame->h = L131_;
 }
 after_loop: ;
 L141_=ASIZE((ELT_TBL)frame->self); 
 L15 = L141_;
 L161_=INTMINUS(L15,1); 
 L17 = L161_;
 L181_=(frame->h)==(L17); 
 if (L181_) {
  frame->h = 0;
  while (1) {
   L191_=(ELT)ARR((ELT_TBL)frame->self,frame->h); 
   frame->e1 = L191_;
   frame->is_elt_nil_self2 = frame->self;
   frame->is_elt_nil_e2 = frame->e1;
   frame->is_elt_nil_self3 = ((ELT_NILELT) NULL);
   frame->is_elt_nil_e3 = frame->is_elt_nil_e2;
   frame->ret_val5 = (frame->is_elt_nil_e3==((ELT) NULL));
   frame->ret_val4 = frame->ret_val5;
   if (frame->ret_val4) {
    goto after_loop1;
   }
   else {
    if (ELT_TB1905689409(frame->self, frame->arg1, frame->e1)) {
     frame->state = 2;
     return frame->e1;
     state2:;
    }
   }
   L201_=INTPLUS(frame->h,1); 
   frame->h = L201_;
  }
  after_loop1: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ELT ELT_TB799962086(ELT_TB799962086_frame frame) {
 ELT dummy = ((ELT) NULL);
 ELT aI_u_I;
 ELT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((ELT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ELT_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame frame) {
 ELT dummy = ((ELT) NULL);
 BOOL L1;
 BOOL L21_;
 ELT L5;
 ELT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((ELT_TBL) NULL));
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
     frame->L41_br=ASIZE((ELT_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((ELT_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILELT) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((ELT) NULL));
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

IDENT IDENT_1154678862(IDENT_1154678862_frame frame) {
 IDENT dummy = IDENT_zero;
 IDENT aI_u_I;
 IDENT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((IDENT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((IDENT_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IDENT IDENT_119982571(IDENT_119982571_frame frame) {
 IDENT dummy = IDENT_zero;
 BOOL L1;
 BOOL L21_;
 IDENT L5;
 IDENT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((IDENT_TBL) NULL));
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
     frame->L41_br=ASIZE((IDENT_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((IDENT_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1.str==(STR)0);
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

INT FMAPdT1072907830(FMAPdT1072907830_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 TUPdTPINT L5;
 TUPdTPINT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdTPINT) NULL));
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
     frame->L41_br=ASIZE((FMAPdTPINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdTPINT)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
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

INT FSTR_indbrINT(FSTR_indbrINT_frame frame) {
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
 L1 = (frame->self==((FSTR) NULL));
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

SIG SIG_TB1663296138(SIG_TB1663296138_frame frame) {
 SIG dummy = ((SIG) NULL);
 SIG aI_u_I;
 SIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((SIG_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((SIG_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SIG SIG_TB788365833(SIG_TB788365833_frame frame) {
 SIG dummy = ((SIG) NULL);
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 SIG L121_;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 SIG L191_;
 INT L201_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 if ((frame->self==((SIG_TBL) NULL))) {
  frame->state = -1;
  return;
 }
 frame->query_hash_self = frame->self;
 frame->query_hash_i = frame->arg1;
 frame->hash_self = frame->query_hash_i;
 L1 = frame->hash_self.str;
 L21_=SYSID(L1); 
 frame->x = L21_;
 L31_=((unsigned)frame->x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 frame->ret_val1 = L51_;
 frame->ret_val = frame->ret_val1;
 L6 = frame->ret_val;
 L71_=ASIZE((SIG_TBL)frame->self); 
 L8 = L71_;
 L91_=INTMINUS(L8,2); 
 L10 = L91_;
 L111_=L6&L10; 
 frame->h = L111_;
 while (1) {
  L121_=(SIG)ARR((SIG_TBL)frame->self,frame->h); 
  frame->e = L121_;
  frame->is_elt_nil_self = frame->self;
  frame->is_elt_nil_e = frame->e;
  frame->is_elt_nil_self1 = ((ELT_NILSIG) NULL);
  frame->is_elt_nil_e1 = frame->is_elt_nil_e;
  frame->ret_val3 = (frame->is_elt_nil_e1==((SIG) NULL));
  frame->ret_val2 = frame->ret_val3;
  if (frame->ret_val2) {
   goto after_loop;
  }
  else {
   if (SIG_TB1063010785(frame->self, frame->arg1, frame->e)) {
    frame->state = 1;
    return frame->e;
    state1:;
   }
  }
  L131_=INTPLUS(frame->h,1); 
  frame->h = L131_;
 }
 after_loop: ;
 L141_=ASIZE((SIG_TBL)frame->self); 
 L15 = L141_;
 L161_=INTMINUS(L15,1); 
 L17 = L161_;
 L181_=(frame->h)==(L17); 
 if (L181_) {
  frame->h = 0;
  while (1) {
   L191_=(SIG)ARR((SIG_TBL)frame->self,frame->h); 
   frame->e1 = L191_;
   frame->is_elt_nil_self2 = frame->self;
   frame->is_elt_nil_e2 = frame->e1;
   frame->is_elt_nil_self3 = ((ELT_NILSIG) NULL);
   frame->is_elt_nil_e3 = frame->is_elt_nil_e2;
   frame->ret_val5 = (frame->is_elt_nil_e3==((SIG) NULL));
   frame->ret_val4 = frame->ret_val5;
   if (frame->ret_val4) {
    goto after_loop1;
   }
   else {
    if (SIG_TB1063010785(frame->self, frame->arg1, frame->e1)) {
     frame->state = 2;
     return frame->e1;
     state2:;
    }
   }
   L201_=INTPLUS(frame->h,1); 
   frame->h = L201_;
  }
  after_loop1: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame frame) {
 SIG dummy = ((SIG) NULL);
 BOOL L1;
 BOOL L21_;
 SIG L5;
 SIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((SIG_TBL) NULL));
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
     frame->L41_br=ASIZE((SIG_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((SIG_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((SIG) NULL));
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

TUPdTP1710233051 FMAPdT2128397196(FMAPdT2128397196_frame frame) {
 TUPdTP1710233051 dummy = TUPdTP1710233051_zero;
 BOOL L1;
 BOOL L21_;
 TUPdTP1710233051 L5;
 TUPdTP1710233051 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdT953816637) NULL));
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
     frame->L41_br=ASIZE((FMAPdT953816637)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdT953816637)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
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

TUPdTP1710233051 FMAPdT247759331(FMAPdT247759331_frame frame) {
 TUPdTP1710233051 dummy = TUPdTP1710233051_zero;
 TUPdTP1710233051 aI_u_I;
 TUPdTP1710233051 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdT953816637)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPdT953816637)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPCODE_FILE FMAPdT1644197042(FMAPdT1644197042_frame frame) {
 TUPdTPCODE_FILE dummy = TUPdTPCODE_FILE_zero;
 BOOL L1;
 BOOL L21_;
 TUPdTPCODE_FILE L5;
 TUPdTPCODE_FILE aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdTPCODE_FILE) NULL));
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
     frame->L41_br=ASIZE((FMAPdTPCODE_FILE)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdTPCODE_FILE)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
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

TUPdTPCODE_FILE FMAPdT669571245(FMAPdT669571245_frame frame) {
 TUPdTPCODE_FILE dummy = TUPdTPCODE_FILE_zero;
 TUPdTPCODE_FILE aI_u_I;
 TUPdTPCODE_FILE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPCODE_FILE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPdTPCODE_FILE)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPINT FMAPdT1857608820(FMAPdT1857608820_frame frame) {
 TUPdTPINT dummy = TUPdTPINT_zero;
 BOOL L1;
 BOOL L21_;
 TUPdTPINT L5;
 TUPdTPINT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdTPINT) NULL));
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
     frame->L41_br=ASIZE((FMAPdTPINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdTPINT)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
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

TUPdTPINT FMAPdT376430313(FMAPdT376430313_frame frame) {
 TUPdTPINT dummy = TUPdTPINT_zero;
 TUPdTPINT aI_u_I;
 TUPdTPINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPdTPINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPdLAYOUT FMAPdT1924387821(FMAPdT1924387821_frame frame) {
 TUPdTPdLAYOUT dummy = TUPdTPdLAYOUT_zero;
 TUPdTPdLAYOUT aI_u_I;
 TUPdTPdLAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPdTPdLAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPdTPdLAYOUT FMAPdT2027173596(FMAPdT2027173596_frame frame) {
 TUPdTPdLAYOUT dummy = TUPdTPdLAYOUT_zero;
 BOOL L1;
 BOOL L21_;
 TUPdTPdLAYOUT L5;
 TUPdTPdLAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdTPdLAYOUT) NULL));
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
     frame->L41_br=ASIZE((FMAPdTPdLAYOUT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdTPdLAYOUT)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
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

dTP FMAPdT705360156(FMAPdT705360156_frame frame) {
 dTP dummy = ((dTP) NULL);
 BOOL L1;
 BOOL L21_;
 TUPdTPINT L5;
 TUPdTPINT aL41_;
 TUPdTPINT L7;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdTPINT) NULL));
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
     frame->L41_br=ASIZE((FMAPdTPINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdTPINT)frame->L61,frame->L31); 
    L7=aL41_;
    frame->r = L7.t1;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r;
    frame->ret_val = ELT_NI125473534(((ELT_NILdTP) NULL), frame->is_key_nil_e);
    L8 = frame->ret_val;
    L91_=!(L8); 
    if (L91_) {
     frame->state = 1;
     return frame->r;
     state1:;
    }
    L101_=INTPLUS(frame->L31,1); 
    frame->L31 = L101_;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void ELT_TB382032404(ELT_TB382032404_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((ELT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((ELT_TBL)frame->self,frame->i_I_u_I,(ELT)frame->arg1); frame->i_I_u_I++;
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

void FMAPdT1043149253(FMAPdT1043149253_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdT953816637)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPdT953816637)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPdT192646101(FMAPdT192646101_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPdTPINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPdT468927009(FMAPdT468927009_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPCODE_FILE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPdTPCODE_FILE)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPdT963530613(FMAPdT963530613_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdTPdLAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPdTPdLAYOUT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void IDENT_605919020(IDENT_605919020_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((IDENT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((IDENT_TBL)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void SIG_TB2081225820(SIG_TB2081225820_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((SIG_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((SIG_TBL)frame->self,frame->i_I_u_I,(SIG)frame->arg1); frame->i_I_u_I++;
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
