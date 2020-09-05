#include "sather.h"

/* Layouts */

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

typedef struct ELT_EQ702804563_struct {/* layout for ELT_EQ{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ702804563;

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

typedef struct ELT_NI602850927_struct {/* layout for ELT_NIL{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI602850927;

typedef struct ELT_NILIDENT_struct {/* layout for ELT_NIL{IDENT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILIDENT;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

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

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct TUPSIGSTR_struct {/* layout for TUP{SIG,STR} */
 struct SIG_struct *t1;
 STR t2;
 } TUPSIGSTR;
static TUPSIGSTR TUPSIGSTR_zero;

typedef struct TUPSIGSTR_boxed_struct {
 OB_HEADER header;
 TUPSIGSTR immutable_part;
 } *TUPSIGSTR_boxed;

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

typedef struct FMAPAM1244483504_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_1024994801_struct arr_part[1];
 } *FMAPAM1244483504;

typedef struct FMAPAM1596285950_frame_struct {
 INT state;
 FMAPAM1244483504 self;/* Formal argument: self */
 TUPAM_1024994801 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1596285950_frame;

typedef struct FMAPAM1319110900_frame_struct {
 INT state;
 FMAPAM1244483504 self;/* Formal argument: self */
 TUPAM_1024994801 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM1319110900_frame;

typedef struct FMAPAM1866584795_frame_struct {
 INT state;
 FMAPAM1244483504 self;/* Formal argument: self */
 TUPAM_1024994801 ret_val2;
 FMAPAM1244483504 L61;
 TUPAM_1024994801 r;
 INT L31;
 FMAPAM1244483504 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1866584795_frame;

typedef struct FMAPAM1787628573_struct {/* layout for FMAP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_653469574_struct arr_part[1];
 } *FMAPAM1787628573;

typedef struct FMAPAM213652070_frame_struct {
 INT state;
 FMAPAM1787628573 self;/* Formal argument: self */
 TUPAM_653469574 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM213652070_frame;

typedef struct FMAPAM466264060_frame_struct {
 INT state;
 FMAPAM1787628573 self;/* Formal argument: self */
 TUPAM_653469574 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPAM466264060_frame;

typedef struct FMAPAM2028198677_frame_struct {
 INT state;
 FMAPAM1787628573 self;/* Formal argument: self */
 TUPAM_653469574 ret_val2;
 FMAPAM1787628573 L61;
 TUPAM_653469574 r;
 INT L31;
 FMAPAM1787628573 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM2028198677_frame;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPID328097389_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPID328097389_frame;

typedef struct FMAPID1049942143_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPID1049942143_frame;

typedef struct FMAPID1294197827_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 IDENT ret_val2;
 FMAPIDENTdTP L61;
 IDENT r;
 INT L31;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID1294197827_frame;

typedef struct FMAPID1741388334_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 TUPIDENTdTP ret_val2;
 FMAPIDENTdTP L61;
 TUPIDENTdTP r;
 INT L31;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID1741388334_frame;

typedef struct FMAPID944928219_frame_struct {
 INT state;
 FMAPIDENTdTP self;/* Formal argument: self */
 dTP ret_val2;
 FMAPIDENTdTP L61;
 TUPIDENTdTP e;
 INT L31;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID944928219_frame;

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

typedef struct FMAPID2058523420_frame_struct {
 INT state;
 FMAPID739086519 self;/* Formal argument: self */
 TUPIDE641654124 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPID2058523420_frame;

typedef struct FMAPID404856354_frame_struct {
 INT state;
 FMAPID739086519 self;/* Formal argument: self */
 TUPIDE641654124 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPID404856354_frame;

typedef struct FMAPID906752465_frame_struct {
 INT state;
 FMAPID739086519 self;/* Formal argument: self */
 TUPIDE641654124 ret_val2;
 FMAPID739086519 L61;
 TUPIDE641654124 r;
 INT L31;
 FMAPID739086519 is_key_nil_self;
 IDENT is_key_nil_e;
 BOOL ret_val;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPID906752465_frame;

typedef struct FMAPSI2103621588_struct {/* layout for FMAP{SIG,ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FMAPSI2103621588;

typedef struct FMAPSI714126805_frame_struct {
 INT state;
 FMAPSI2103621588 self;/* Formal argument: self */
 TUPSIG1754078736 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI714126805_frame;

typedef struct FMAPSI1222948761_frame_struct {
 INT state;
 FMAPSI2103621588 self;/* Formal argument: self */
 TUPSIG1754078736 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI1222948761_frame;

typedef struct FMAPSI1591619598_frame_struct {
 INT state;
 FMAPSI2103621588 self;/* Formal argument: self */
 TUPSIG1754078736 ret_val2;
 FMAPSI2103621588 L61;
 TUPSIG1754078736 r;
 INT L31;
 FMAPSI2103621588 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI1591619598_frame;

typedef struct FMAPSI140286185_frame_struct {
 INT state;
 FMAPSI2103621588 self;/* Formal argument: self */
 ABSTRA151498155 ret_val2;
 FMAPSI2103621588 L61;
 TUPSIG1754078736 e;
 INT L31;
 FMAPSI2103621588 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI140286185_frame;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSI803721117_frame_struct {
 INT state;
 FMAPSI518162669 self;/* Formal argument: self */
 TUPSIG578692189 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI803721117_frame;

typedef struct FMAPSI515269605_frame_struct {
 INT state;
 FMAPSI518162669 self;/* Formal argument: self */
 TUPSIG578692189 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI515269605_frame;

typedef struct FMAPSI1135054800_frame_struct {
 INT state;
 FMAPSI518162669 self;/* Formal argument: self */
 TUPSIG578692189 ret_val2;
 FMAPSI518162669 L61;
 TUPSIG578692189 r;
 INT L31;
 FMAPSI518162669 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI1135054800_frame;

typedef struct FMAPSI1349988702_struct {/* layout for FMAP{SIG,ARG_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FMAPSI1349988702;

typedef struct FMAPSI1981723230_frame_struct {
 INT state;
 FMAPSI1349988702 self;/* Formal argument: self */
 TUPSIGARG_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI1981723230_frame;

typedef struct FMAPSI688594122_frame_struct {
 INT state;
 FMAPSI1349988702 self;/* Formal argument: self */
 ARG_LAYOUT ret_val2;
 FMAPSI1349988702 L61;
 TUPSIGARG_LAYOUT e;
 INT L31;
 FMAPSI1349988702 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI688594122_frame;

typedef struct FMAPSI1121373188_struct {/* layout for FMAP{SIG,FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FMAPSI1121373188;

typedef struct FMAPSI442499788_frame_struct {
 INT state;
 FMAPSI1121373188 self;/* Formal argument: self */
 TUPSIG2023533247 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI442499788_frame;

typedef struct FMAPSI266480270_frame_struct {
 INT state;
 FMAPSI1121373188 self;/* Formal argument: self */
 TUPSIG2023533247 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI266480270_frame;

typedef struct FMAPSI118804083_frame_struct {
 INT state;
 FMAPSI1121373188 self;/* Formal argument: self */
 TUPSIG2023533247 ret_val2;
 FMAPSI1121373188 L61;
 TUPSIG2023533247 r;
 INT L31;
 FMAPSI1121373188 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI118804083_frame;

typedef struct FMAPSI1790064862_frame_struct {
 INT state;
 FMAPSI1121373188 self;/* Formal argument: self */
 FRAME_LAYOUT ret_val2;
 FMAPSI1121373188 L61;
 TUPSIG2023533247 e;
 INT L31;
 FMAPSI1121373188 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI1790064862_frame;

typedef struct FMAPSIGSTR_struct {/* layout for FMAP{SIG,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGSTR_struct arr_part[1];
 } *FMAPSIGSTR;

typedef struct FMAPSI441885619_frame_struct {
 INT state;
 FMAPSIGSTR self;/* Formal argument: self */
 TUPSIGSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI441885619_frame;

typedef struct FMAPSI127190795_frame_struct {
 INT state;
 FMAPSIGSTR self;/* Formal argument: self */
 TUPSIGSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPSI127190795_frame;

typedef struct FMAPSI1080015256_frame_struct {
 INT state;
 FMAPSIGSTR self;/* Formal argument: self */
 TUPSIGSTR ret_val2;
 FMAPSIGSTR L61;
 TUPSIGSTR r;
 INT L31;
 FMAPSIGSTR is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPSI1080015256_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

#include "tags.h"

/* Globals */

extern INT FMAPAM1649190656;
extern INT FMAPAM673663981;
extern INT FMAPID1835582514;
extern INT FMAPID1853260260;
extern INT FMAPSI1253630331;
extern INT FMAPSI1801446319;
extern INT FMAPSI22065355;
extern INT FMAPSI362068854;

/* Function declarations */


extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ABSTRA151498155 FMAPSI140286185(FMAPSI140286185_frame);
ABSTRA151498155 FMAPSI146872941(FMAPSI2103621588, SIG);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AM_SHARED_EXPR FMAPID608869096(FMAPID739086519, IDENT);
ARG_LAYOUT FMAPSI688594122(FMAPSI688594122_frame);
BOOL FMAPSI773227610(FMAPSI518162669);
FLISTA1062334999 FMAPAM925529692(FMAPAM1787628573, AM_ROUT_DEF);
FLISTA1409846210 FMAPAM338804820(FMAPAM1244483504, AM_ROUT_DEF);
FMAPAM1244483504 FMAPAM1334262314(FMAPAM1244483504, INT);
FMAPAM1244483504 FMAPAM1818297900(FMAPAM1244483504, AM_ROUT_DEF, FLISTA1409846210);
FMAPAM1244483504 FMAPAM497928773(FMAPAM1244483504);
FMAPAM1787628573 FMAPAM1292066082(FMAPAM1787628573, INT);
FMAPAM1787628573 FMAPAM293039037(FMAPAM1787628573, AM_ROUT_DEF, FLISTA1062334999);
FMAPAM1787628573 FMAPAM889177427(FMAPAM1787628573);
FMAPID739086519 FMAPID1512695779(FMAPID739086519, IDENT, AM_SHARED_EXPR);
FMAPID739086519 FMAPID1523451284(FMAPID739086519, INT);
FMAPID739086519 FMAPID408447785(FMAPID739086519);
FMAPIDENTdTP FMAPID1112745107(FMAPIDENTdTP, INT);
FMAPIDENTdTP FMAPID1786193115(FMAPIDENTdTP, IDENT, dTP);
FMAPIDENTdTP FMAPID1805748084(FMAPIDENTdTP);
FMAPSI1121373188 FMAPSI1358355632(FMAPSI1121373188, SIG, FRAME_LAYOUT);
FMAPSI1121373188 FMAPSI2029716764(FMAPSI1121373188, INT);
FMAPSI1121373188 FMAPSI484237251(FMAPSI1121373188);
FMAPSI2103621588 FMAPSI1221142349(FMAPSI2103621588, SIG, ABSTRA151498155);
FMAPSI2103621588 FMAPSI1818347340(FMAPSI2103621588);
FMAPSI2103621588 FMAPSI337958229(FMAPSI2103621588, INT);
FMAPSI518162669 FMAPSI1205266134(FMAPSI518162669, TUPSIG578692189);
FMAPSI518162669 FMAPSI1355362246(FMAPSI518162669);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPSI518162669 FMAPSI396914288(FMAPSI518162669, SIG, AM_ROUT_DEF);
FMAPSI518162669 FMAPSI522072323(FMAPSI518162669, INT);
FMAPSI518162669 FMAPSI90181471(FMAPSI518162669);
FMAPSIGSTR FMAPSI1249683373(FMAPSIGSTR, INT);
FMAPSIGSTR FMAPSI1579062782(FMAPSIGSTR);
FMAPSIGSTR FMAPSI1897406528(FMAPSIGSTR, SIG, STR);
FRAME_LAYOUT FMAPSI1790064862(FMAPSI1790064862_frame);
FRAME_LAYOUT FMAPSI547426166(FMAPSI1121373188, SIG);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
IDENT FMAPID1294197827(FMAPID1294197827_frame);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FMAPID99794443(FMAPIDENTdTP);
STR FLISTA1884067430(FLISTA1062334999);
STR FLISTA389202533(FLISTA1409846210);
STR FMAPAM118642384(FMAPAM1787628573);
STR FMAPAM459426429(FMAPAM1244483504);
STR FMAPID1860739187(FMAPIDENTdTP);
STR FMAPID1895267434(FMAPID739086519);
STR FMAPSI1424048021(FMAPSI1349988702);
STR FMAPSI1519487311(FMAPSIGSTR, SIG);
STR FMAPSI1996405359(FMAPSI2103621588);
STR FMAPSI2071674985(FMAPSI1121373188);
STR FMAPSI866101256(FMAPSIGSTR);
STR FMAPSI99914640(FMAPSI518162669);
STR STR_fr1097270334(STR, FSTR);
TUPAM_1024994801 FMAPAM1596285950(FMAPAM1596285950_frame);
TUPAM_1024994801 FMAPAM1866584795(FMAPAM1866584795_frame);
TUPAM_653469574 FMAPAM2028198677(FMAPAM2028198677_frame);
TUPAM_653469574 FMAPAM213652070(FMAPAM213652070_frame);
TUPIDE641654124 FMAPID2058523420(FMAPID2058523420_frame);
TUPIDE641654124 FMAPID906752465(FMAPID906752465_frame);
TUPIDENTdTP FMAPID1741388334(FMAPID1741388334_frame);
TUPIDENTdTP FMAPID328097389(FMAPID328097389_frame);
TUPSIG1754078736 FMAPSI1591619598(FMAPSI1591619598_frame);
TUPSIG1754078736 FMAPSI714126805(FMAPSI714126805_frame);
TUPSIG2023533247 FMAPSI118804083(FMAPSI118804083_frame);
TUPSIG2023533247 FMAPSI442499788(FMAPSI442499788_frame);
TUPSIG578692189 FMAPSI1135054800(FMAPSI1135054800_frame);
TUPSIG578692189 FMAPSI803721117(FMAPSI803721117_frame);
TUPSIGARG_LAYOUT FMAPSI1981723230(FMAPSI1981723230_frame);
TUPSIGSTR FMAPSI1080015256(FMAPSI1080015256_frame);
TUPSIGSTR FMAPSI441885619(FMAPSI441885619_frame);
dTP FMAPID86862037(FMAPIDENTdTP, IDENT);
dTP FMAPID944928219(FMAPID944928219_frame);
void FMAPAM1319110900(FMAPAM1319110900_frame);
void FMAPAM466264060(FMAPAM466264060_frame);
void FMAPID1049942143(FMAPID1049942143_frame);
void FMAPID404856354(FMAPID404856354_frame);
void FMAPSI1222948761(FMAPSI1222948761_frame);
void FMAPSI127190795(FMAPSI127190795_frame);
void FMAPSI266480270(FMAPSI266480270_frame);
void FMAPSI515269605(FMAPSI515269605_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

ABSTRA151498155 FMAPSI146872941(FMAPSI2103621588 self, SIG k) {
 ABSTRA151498155 ret_val;
 INT h = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI2103621588 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSI2103621588 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSI2103621588 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FMAPSI2103621588 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPSI2103621588 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSIG1754078736 L71_;
 TUPSIG1754078736 L8;
 TUPSIG1754078736 L91_;
 TUPSIG1754078736 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSIG1754078736 L171_;
 TUPSIG1754078736 L18;
 TUPSIG1754078736 L191_;
 TUPSIG1754078736 L20;
 INT L221_;
 if ((self==((FMAPSI2103621588) NULL))) {
  ret_val = ((ABSTRA151498155) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSI2103621588)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSI2103621588)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSIG) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_s = elt_eq_e2;
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSI2103621588)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSI2103621588)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSI2103621588)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSIG) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_s1 = elt_eq_e21;
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPSI2103621588)self,h); 
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
 ret_val = ((ABSTRA151498155) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669 self, SIG k) {
 AM_ROUT_DEF ret_val;
 INT h = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI518162669 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSI518162669 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSI518162669 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FMAPSI518162669 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPSI518162669 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSIG578692189 L71_;
 TUPSIG578692189 L8;
 TUPSIG578692189 L91_;
 TUPSIG578692189 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSIG578692189 L171_;
 TUPSIG578692189 L18;
 TUPSIG578692189 L191_;
 TUPSIG578692189 L20;
 INT L221_;
 if ((self==((FMAPSI518162669) NULL))) {
  ret_val = ((AM_ROUT_DEF) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSI518162669)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSI518162669)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSIG) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_s = elt_eq_e2;
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSI518162669)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSI518162669)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSI518162669)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSIG) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_s1 = elt_eq_e21;
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPSI518162669)self,h); 
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
 ret_val = ((AM_ROUT_DEF) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_SHARED_EXPR FMAPID608869096(FMAPID739086519 self, IDENT k) {
 AM_SHARED_EXPR ret_val;
 INT h = INT_zero;
 IDENT tk = IDENT_zero;
 IDENT tk1 = IDENT_zero;
 FMAPID739086519 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPID739086519 is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 FMAPID739086519 key_eq_self;
 IDENT key_eq_e1 = IDENT_zero;
 IDENT key_eq_e2 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQIDENT elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 FMAPID739086519 is_key_nil_self1;
 IDENT is_key_nil_e1 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val8 = BOOL_zero;
 FMAPID739086519 key_eq_self1;
 IDENT key_eq_e11 = IDENT_zero;
 IDENT key_eq_e21 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDE641654124 L91_;
 TUPIDE641654124 L10;
 TUPIDE641654124 L111_;
 TUPIDE641654124 L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 TUPIDE641654124 L191_;
 TUPIDE641654124 L20;
 TUPIDE641654124 L211_;
 TUPIDE641654124 L22;
 INT L231_;
 if ((self==((FMAPID739086519) NULL))) {
  ret_val = ((AM_SHARED_EXPR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = k;
 key_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L3 = ret_val1;
 L41_=ASIZE((FMAPID739086519)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FMAPID739086519)self,h); 
  L10=L91_;
  tk = L10.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e.str==(STR)0);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQIDENT) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_i = elt_eq_e2;
   ret_val6 = (is_eq_self.str==is_eq_i.str);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L111_=ARR((FMAPID739086519)self,h); 
    L12=L111_;
    ret_val = L12.t2;
    return ret_val;
   }
  }
  L131_=INTPLUS(h,1); 
  h = L131_;
 }
 after_loop: ;
 L141_=ASIZE((FMAPID739086519)self); 
 L15 = L141_;
 L161_=INTMINUS(L15,1); 
 L17 = L161_;
 L181_=(h)==(L17); 
 if (L181_) {
  h = 0;
  while (1) {
   L191_=ARR((FMAPID739086519)self,h); 
   L20=L191_;
   tk1 = L20.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1.str==(STR)0);
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQIDENT) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_i1 = elt_eq_e21;
    ret_val11 = (is_eq_self1.str==is_eq_i1.str);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L211_=ARR((FMAPID739086519)self,h); 
     L22=L211_;
     ret_val = L22.t2;
     return ret_val;
    }
   }
   L231_=INTPLUS(h,1); 
   h = L231_;
  }
  after_loop1: ;
 }
 ret_val = ((AM_SHARED_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FMAPSI773227610(FMAPSI518162669 self) {
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
 L21_=ASIZE((FMAPSI518162669)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FMAPSI518162669)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FMAPSI1253630331;
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

FLISTA1062334999 FMAPAM925529692(FMAPAM1787628573 self, AM_ROUT_DEF k) {
 FLISTA1062334999 ret_val;
 INT h = INT_zero;
 AM_ROUT_DEF tk;
 AM_ROUT_DEF tk1;
 FMAPAM1787628573 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM1787628573 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM1787628573 key_eq_self;
 AM_ROUT_DEF key_eq_e1;
 AM_ROUT_DEF key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPAM1787628573 is_key_nil_self1;
 AM_ROUT_DEF is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPAM1787628573 key_eq_self1;
 AM_ROUT_DEF key_eq_e11;
 AM_ROUT_DEF key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_653469574 L71_;
 TUPAM_653469574 L8;
 TUPAM_653469574 L91_;
 TUPAM_653469574 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_653469574 L171_;
 TUPAM_653469574 L18;
 TUPAM_653469574 L191_;
 TUPAM_653469574 L20;
 INT L221_;
 if ((self==((FMAPAM1787628573) NULL))) {
  ret_val = ((FLISTA1062334999) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM1787628573)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM1787628573)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ702804563) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = (elt_eq_e1==elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM1787628573)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM1787628573)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM1787628573)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI602850927) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ702804563) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = (elt_eq_e11==elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPAM1787628573)self,h); 
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
 ret_val = ((FLISTA1062334999) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1409846210 FMAPAM338804820(FMAPAM1244483504 self, AM_ROUT_DEF k) {
 FLISTA1409846210 ret_val;
 INT h = INT_zero;
 AM_ROUT_DEF tk;
 AM_ROUT_DEF tk1;
 FMAPAM1244483504 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPAM1244483504 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPAM1244483504 key_eq_self;
 AM_ROUT_DEF key_eq_e1;
 AM_ROUT_DEF key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPAM1244483504 is_key_nil_self1;
 AM_ROUT_DEF is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPAM1244483504 key_eq_self1;
 AM_ROUT_DEF key_eq_e11;
 AM_ROUT_DEF key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPAM_1024994801 L71_;
 TUPAM_1024994801 L8;
 TUPAM_1024994801 L91_;
 TUPAM_1024994801 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPAM_1024994801 L171_;
 TUPAM_1024994801 L18;
 TUPAM_1024994801 L191_;
 TUPAM_1024994801 L20;
 INT L221_;
 if ((self==((FMAPAM1244483504) NULL))) {
  ret_val = ((FLISTA1409846210) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPAM1244483504)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPAM1244483504)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQ702804563) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = (elt_eq_e1==elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPAM1244483504)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPAM1244483504)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPAM1244483504)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI602850927) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQ702804563) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = (elt_eq_e11==elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPAM1244483504)self,h); 
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
 ret_val = ((FLISTA1409846210) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1244483504 FMAPAM1334262314(FMAPAM1244483504 self, INT n) {
 FMAPAM1244483504 ret_val;
 FMAPAM1244483504 r;
 FMAPAM1244483504 L11;
 FMAPAM1244483504 key_nil_self;
 AM_ROUT_DEF ret_val1;
 ELT_NI602850927 elt_nil_self;
 AM_ROUT_DEF ret_val2;
 AM_ROUT_DEF e = ((AM_ROUT_DEF) NULL);
 FMAPAM1244483504 key_nil_self1;
 AM_ROUT_DEF ret_val3;
 ELT_NI602850927 elt_nil_self1;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF e1 = ((AM_ROUT_DEF) NULL);
 TUPAM_1024994801 create_self = TUPAM_1024994801_zero;
 AM_ROUT_DEF create_at1;
 FLISTA1409846210 create_at2;
 TUPAM_1024994801 ret_val5 = TUPAM_1024994801_zero;
 INT L21 = INT_zero;
 FMAPAM1244483504 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_1024994801 L10;
 TUPAM_1024994801 L12;
 TUPAM_1024994801 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM1244483504_struct)-sizeof(TUPAM_1024994801))+(L4)*sizeof(TUPAM_1024994801);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM1244483504_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM1244483504) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI602850927) NULL);
 ret_val2 = ((AM_ROUT_DEF) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_ROUT_DEF) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM1244483504)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_1024994801_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI602850927) NULL);
    ret_val4 = ((AM_ROUT_DEF) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FLISTA1409846210) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM1244483504)L11,L21,L13); 
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

FMAPAM1244483504 FMAPAM1818297900(FMAPAM1244483504 self, AM_ROUT_DEF k, FLISTA1409846210 t) {
 FMAPAM1244483504 ret_val;
 FMAPAM1244483504 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_ROUT_DEF tk;
 AM_ROUT_DEF tk1;
 FMAPAM1244483504 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM1244483504 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM1244483504 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM1244483504 key_eq_self;
 AM_ROUT_DEF key_eq_e1;
 AM_ROUT_DEF key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPAM_1024994801 create_self = TUPAM_1024994801_zero;
 AM_ROUT_DEF create_at1;
 FLISTA1409846210 create_at2;
 TUPAM_1024994801 ret_val7 = TUPAM_1024994801_zero;
 FMAPAM1244483504 is_key_nil_self1;
 AM_ROUT_DEF is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPAM1244483504 key_eq_self1;
 AM_ROUT_DEF key_eq_e11;
 AM_ROUT_DEF key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPAM_1024994801 create_self1 = TUPAM_1024994801_zero;
 AM_ROUT_DEF create_at11;
 FLISTA1409846210 create_at21;
 TUPAM_1024994801 ret_val12 = TUPAM_1024994801_zero;
 TUPAM_1024994801 create_self2 = TUPAM_1024994801_zero;
 AM_ROUT_DEF create_at12;
 FLISTA1409846210 create_at22;
 TUPAM_1024994801 ret_val13 = TUPAM_1024994801_zero;
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
 TUPAM_1024994801 L201_;
 TUPAM_1024994801 L21;
 TUPAM_1024994801 L22;
 TUPAM_1024994801 L23;
 TUPAM_1024994801 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_1024994801 L281_;
 TUPAM_1024994801 L29;
 TUPAM_1024994801 L30;
 TUPAM_1024994801 L31;
 TUPAM_1024994801 L32;
 INT L341_;
 TUPAM_1024994801 L35;
 TUPAM_1024994801 L36;
 TUPAM_1024994801 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM1244483504) NULL))) {
  r = FMAPAM1334262314(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM1244483504)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM673663981;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM497928773(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM1244483504)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM1244483504)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM1244483504)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ702804563) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = (elt_eq_e1==elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_1024994801_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPAM1244483504)r,h,L24); 
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
   L281_=ARR((FMAPAM1244483504)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI602850927) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ702804563) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = (elt_eq_e11==elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPAM_1024994801_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPAM1244483504)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_1024994801_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPAM1244483504)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1244483504 FMAPAM497928773(FMAPAM1244483504 self) {
 FMAPAM1244483504 ret_val;
 INT ns = INT_zero;
 FMAPAM1244483504 r;
 FMAPAM1244483504 L11;
 FMAPAM1244483504 insert_pair_self;
 TUPAM_1024994801 insert_pair_p = TUPAM_1024994801_zero;
 FMAPAM1244483504 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_1024994801 L9;
 L21_=ASIZE((FMAPAM1244483504)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM1334262314(self, ns);
 {
  struct FMAPAM1866584795_frame_struct other1_0;
FMAPAM1866584795_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM1866584795(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM1818297900(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPAM1787628573 FMAPAM1292066082(FMAPAM1787628573 self, INT n) {
 FMAPAM1787628573 ret_val;
 FMAPAM1787628573 r;
 FMAPAM1787628573 L11;
 FMAPAM1787628573 key_nil_self;
 AM_ROUT_DEF ret_val1;
 ELT_NI602850927 elt_nil_self;
 AM_ROUT_DEF ret_val2;
 AM_ROUT_DEF e = ((AM_ROUT_DEF) NULL);
 FMAPAM1787628573 key_nil_self1;
 AM_ROUT_DEF ret_val3;
 ELT_NI602850927 elt_nil_self1;
 AM_ROUT_DEF ret_val4;
 AM_ROUT_DEF e1 = ((AM_ROUT_DEF) NULL);
 TUPAM_653469574 create_self = TUPAM_653469574_zero;
 AM_ROUT_DEF create_at1;
 FLISTA1062334999 create_at2;
 TUPAM_653469574 ret_val5 = TUPAM_653469574_zero;
 INT L21 = INT_zero;
 FMAPAM1787628573 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPAM_653469574 L10;
 TUPAM_653469574 L12;
 TUPAM_653469574 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPAM1787628573_struct)-sizeof(TUPAM_653469574))+(L4)*sizeof(TUPAM_653469574);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPAM1787628573_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPAM1787628573) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NI602850927) NULL);
 ret_val2 = ((AM_ROUT_DEF) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_ROUT_DEF) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPAM1787628573)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPAM_653469574_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NI602850927) NULL);
    ret_val4 = ((AM_ROUT_DEF) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FLISTA1062334999) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPAM1787628573)L11,L21,L13); 
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

FMAPAM1787628573 FMAPAM293039037(FMAPAM1787628573 self, AM_ROUT_DEF k, FLISTA1062334999 t) {
 FMAPAM1787628573 ret_val;
 FMAPAM1787628573 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 AM_ROUT_DEF tk;
 AM_ROUT_DEF tk1;
 FMAPAM1787628573 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPAM1787628573 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPAM1787628573 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPAM1787628573 key_eq_self;
 AM_ROUT_DEF key_eq_e1;
 AM_ROUT_DEF key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self;
 AM_ROUT_DEF elt_eq_e1;
 AM_ROUT_DEF elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPAM_653469574 create_self = TUPAM_653469574_zero;
 AM_ROUT_DEF create_at1;
 FLISTA1062334999 create_at2;
 TUPAM_653469574 ret_val7 = TUPAM_653469574_zero;
 FMAPAM1787628573 is_key_nil_self1;
 AM_ROUT_DEF is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPAM1787628573 key_eq_self1;
 AM_ROUT_DEF key_eq_e11;
 AM_ROUT_DEF key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQ702804563 elt_eq_self1;
 AM_ROUT_DEF elt_eq_e11;
 AM_ROUT_DEF elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPAM_653469574 create_self1 = TUPAM_653469574_zero;
 AM_ROUT_DEF create_at11;
 FLISTA1062334999 create_at21;
 TUPAM_653469574 ret_val12 = TUPAM_653469574_zero;
 TUPAM_653469574 create_self2 = TUPAM_653469574_zero;
 AM_ROUT_DEF create_at12;
 FLISTA1062334999 create_at22;
 TUPAM_653469574 ret_val13 = TUPAM_653469574_zero;
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
 TUPAM_653469574 L201_;
 TUPAM_653469574 L21;
 TUPAM_653469574 L22;
 TUPAM_653469574 L23;
 TUPAM_653469574 L24;
 INT L261_;
 BOOL L271_;
 TUPAM_653469574 L281_;
 TUPAM_653469574 L29;
 TUPAM_653469574 L30;
 TUPAM_653469574 L31;
 TUPAM_653469574 L32;
 INT L341_;
 TUPAM_653469574 L35;
 TUPAM_653469574 L36;
 TUPAM_653469574 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPAM1787628573) NULL))) {
  r = FMAPAM1292066082(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPAM1787628573)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPAM1649190656;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPAM889177427(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPAM1787628573)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPAM1787628573)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPAM1787628573)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQ702804563) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = (elt_eq_e1==elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPAM_653469574_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPAM1787628573)r,h,L24); 
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
   L281_=ARR((FMAPAM1787628573)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NI602850927) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQ702804563) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = (elt_eq_e11==elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPAM_653469574_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPAM1787628573)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPAM_653469574_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPAM1787628573)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPAM1787628573 FMAPAM889177427(FMAPAM1787628573 self) {
 FMAPAM1787628573 ret_val;
 INT ns = INT_zero;
 FMAPAM1787628573 r;
 FMAPAM1787628573 L11;
 FMAPAM1787628573 insert_pair_self;
 TUPAM_653469574 insert_pair_p = TUPAM_653469574_zero;
 FMAPAM1787628573 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPAM_653469574 L9;
 L21_=ASIZE((FMAPAM1787628573)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPAM1292066082(self, ns);
 {
  struct FMAPAM2028198677_frame_struct other1_0;
FMAPAM2028198677_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPAM2028198677(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPAM293039037(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPID739086519 FMAPID1512695779(FMAPID739086519 self, IDENT k, AM_SHARED_EXPR t) {
 FMAPID739086519 ret_val;
 FMAPID739086519 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 IDENT tk = IDENT_zero;
 IDENT tk1 = IDENT_zero;
 FMAPID739086519 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPID739086519 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPID739086519 is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 FMAPID739086519 key_eq_self;
 IDENT key_eq_e1 = IDENT_zero;
 IDENT key_eq_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQIDENT elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 TUPIDE641654124 create_self = TUPIDE641654124_zero;
 IDENT create_at1 = IDENT_zero;
 AM_SHARED_EXPR create_at2;
 TUPIDE641654124 ret_val8 = TUPIDE641654124_zero;
 FMAPID739086519 is_key_nil_self1;
 IDENT is_key_nil_e1 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 FMAPID739086519 key_eq_self1;
 IDENT key_eq_e11 = IDENT_zero;
 IDENT key_eq_e21 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 TUPIDE641654124 create_self1 = TUPIDE641654124_zero;
 IDENT create_at11 = IDENT_zero;
 AM_SHARED_EXPR create_at21;
 TUPIDE641654124 ret_val14 = TUPIDE641654124_zero;
 TUPIDE641654124 create_self2 = TUPIDE641654124_zero;
 IDENT create_at12 = IDENT_zero;
 AM_SHARED_EXPR create_at22;
 TUPIDE641654124 ret_val15 = TUPIDE641654124_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 OB L10;
 OB L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L191_;
 INT L20;
 INT L211_;
 TUPIDE641654124 L221_;
 TUPIDE641654124 L23;
 TUPIDE641654124 L24;
 TUPIDE641654124 L25;
 TUPIDE641654124 L26;
 INT L281_;
 BOOL L291_;
 TUPIDE641654124 L301_;
 TUPIDE641654124 L31;
 TUPIDE641654124 L32;
 TUPIDE641654124 L33;
 TUPIDE641654124 L34;
 INT L361_;
 TUPIDE641654124 L37;
 TUPIDE641654124 L38;
 TUPIDE641654124 L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPID739086519) NULL))) {
  r = FMAPID1523451284(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPID739086519)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPID1853260260;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPID408447785(self);
  }
 }
 key_hash_self = r;
 L12=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=IDENT_tag;
 L10 = (OB)((IDENT_boxed) L12);
 ((IDENT_boxed) L10)->immutable_part = k;
 key_hash_e = L10;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L13 = ret_val2;
 L141_=ASIZE((FMAPID739086519)r); 
 L15 = L141_;
 L161_=INTMINUS(L15,2); 
 L17 = L161_;
 L181_=L13&L17; 
 orig_h = L181_;
 h = orig_h;
 L191_=ASIZE((FMAPID739086519)r); 
 L20 = L191_;
 L211_=INTMINUS(L20,1); 
 asm1 = L211_;
 while (1) {
  L221_=ARR((FMAPID739086519)r,h); 
  L23=L221_;
  tk = L23.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e.str==(STR)0);
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQIDENT) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_i = elt_eq_e2;
  ret_val7 = (is_eq_self.str==is_eq_i.str);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPIDE641654124_zero;
   create_at1 = k;
   create_at2 = t;
   L24 = create_self;
   L24.t1 = create_at1;
   L25 = L24;
   L25.t2 = create_at2;
   ret_val8 = L25;
   L26 = ret_val8;
   SARR((FMAPID739086519)r,h,L26); 
   ;
   ret_val = r;
   return ret_val;
  }
  L281_=INTPLUS(h,1); 
  h = L281_;
 }
 after_loop: ;
 L291_=(h)==(asm1); 
 if (L291_) {
  h = 0;
  while (1) {
   L301_=ARR((FMAPID739086519)r,h); 
   L31=L301_;
   tk1 = L31.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1.str==(STR)0);
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQIDENT) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_i1 = elt_eq_e21;
   ret_val13 = (is_eq_self1.str==is_eq_i1.str);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPIDE641654124_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val14 = L33;
    L34 = ret_val14;
    SARR((FMAPID739086519)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPIDE641654124_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val15 = L38;
 L39 = ret_val15;
 SARR((FMAPID739086519)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPID739086519 FMAPID1523451284(FMAPID739086519 self, INT n) {
 FMAPID739086519 ret_val;
 FMAPID739086519 r;
 FMAPID739086519 L11;
 FMAPID739086519 key_nil_self;
 IDENT ret_val1 = IDENT_zero;
 ELT_NILIDENT elt_nil_self;
 IDENT ret_val2 = IDENT_zero;
 IDENT e = IDENT_zero;
 FMAPID739086519 key_nil_self1;
 IDENT ret_val3 = IDENT_zero;
 ELT_NILIDENT elt_nil_self1;
 IDENT ret_val4 = IDENT_zero;
 IDENT e1 = IDENT_zero;
 TUPIDE641654124 create_self = TUPIDE641654124_zero;
 IDENT create_at1 = IDENT_zero;
 AM_SHARED_EXPR create_at2;
 TUPIDE641654124 ret_val5 = TUPIDE641654124_zero;
 INT L21 = INT_zero;
 FMAPID739086519 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPIDE641654124 L10;
 TUPIDE641654124 L12;
 TUPIDE641654124 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPID739086519_struct)-sizeof(TUPIDE641654124))+(L4)*sizeof(TUPIDE641654124);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPID739086519_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPID739086519) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILIDENT) NULL);
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
   L91_br=ASIZE((FMAPID739086519)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPIDE641654124_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILIDENT) NULL);
    ret_val4 = IDENT_zero;
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((AM_SHARED_EXPR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPID739086519)L11,L21,L13); 
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

FMAPID739086519 FMAPID408447785(FMAPID739086519 self) {
 FMAPID739086519 ret_val;
 INT ns = INT_zero;
 FMAPID739086519 r;
 FMAPID739086519 L11;
 FMAPID739086519 insert_pair_self;
 TUPIDE641654124 insert_pair_p = TUPIDE641654124_zero;
 FMAPID739086519 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDE641654124 L9;
 L21_=ASIZE((FMAPID739086519)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPID1523451284(self, ns);
 {
  struct FMAPID906752465_frame_struct other1_0;
FMAPID906752465_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPID906752465(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPID1512695779(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPIDENTdTP FMAPID1112745107(FMAPIDENTdTP self, INT n) {
 FMAPIDENTdTP ret_val;
 FMAPIDENTdTP r;
 FMAPIDENTdTP L11;
 FMAPIDENTdTP key_nil_self;
 IDENT ret_val1 = IDENT_zero;
 ELT_NILIDENT elt_nil_self;
 IDENT ret_val2 = IDENT_zero;
 IDENT e = IDENT_zero;
 FMAPIDENTdTP key_nil_self1;
 IDENT ret_val3 = IDENT_zero;
 ELT_NILIDENT elt_nil_self1;
 IDENT ret_val4 = IDENT_zero;
 IDENT e1 = IDENT_zero;
 TUPIDENTdTP create_self = TUPIDENTdTP_zero;
 IDENT create_at1 = IDENT_zero;
 dTP create_at2;
 TUPIDENTdTP ret_val5 = TUPIDENTdTP_zero;
 INT L21 = INT_zero;
 FMAPIDENTdTP L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPIDENTdTP L10;
 TUPIDENTdTP L12;
 TUPIDENTdTP L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPIDENTdTP_struct)-sizeof(TUPIDENTdTP))+(L4)*sizeof(TUPIDENTdTP);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPIDENTdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPIDENTdTP) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILIDENT) NULL);
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
   L91_br=ASIZE((FMAPIDENTdTP)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPIDENTdTP_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILIDENT) NULL);
    ret_val4 = IDENT_zero;
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((dTP) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPIDENTdTP)L11,L21,L13); 
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

FMAPIDENTdTP FMAPID1786193115(FMAPIDENTdTP self, IDENT k, dTP t) {
 FMAPIDENTdTP ret_val;
 FMAPIDENTdTP r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 IDENT tk = IDENT_zero;
 IDENT tk1 = IDENT_zero;
 FMAPIDENTdTP should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPIDENTdTP key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 FMAPIDENTdTP key_eq_self;
 IDENT key_eq_e1 = IDENT_zero;
 IDENT key_eq_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQIDENT elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 TUPIDENTdTP create_self = TUPIDENTdTP_zero;
 IDENT create_at1 = IDENT_zero;
 dTP create_at2;
 TUPIDENTdTP ret_val8 = TUPIDENTdTP_zero;
 FMAPIDENTdTP is_key_nil_self1;
 IDENT is_key_nil_e1 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 FMAPIDENTdTP key_eq_self1;
 IDENT key_eq_e11 = IDENT_zero;
 IDENT key_eq_e21 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 TUPIDENTdTP create_self1 = TUPIDENTdTP_zero;
 IDENT create_at11 = IDENT_zero;
 dTP create_at21;
 TUPIDENTdTP ret_val14 = TUPIDENTdTP_zero;
 TUPIDENTdTP create_self2 = TUPIDENTdTP_zero;
 IDENT create_at12 = IDENT_zero;
 dTP create_at22;
 TUPIDENTdTP ret_val15 = TUPIDENTdTP_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 OB L10;
 OB L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L191_;
 INT L20;
 INT L211_;
 TUPIDENTdTP L221_;
 TUPIDENTdTP L23;
 TUPIDENTdTP L24;
 TUPIDENTdTP L25;
 TUPIDENTdTP L26;
 INT L281_;
 BOOL L291_;
 TUPIDENTdTP L301_;
 TUPIDENTdTP L31;
 TUPIDENTdTP L32;
 TUPIDENTdTP L33;
 TUPIDENTdTP L34;
 INT L361_;
 TUPIDENTdTP L37;
 TUPIDENTdTP L38;
 TUPIDENTdTP L39;
 INT L42;
 INT L431_;
 r = self;
 if ((r==((FMAPIDENTdTP) NULL))) {
  r = FMAPID1112745107(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPIDENTdTP)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPID1835582514;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPID1805748084(self);
  }
 }
 key_hash_self = r;
 L12=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=IDENT_tag;
 L10 = (OB)((IDENT_boxed) L12);
 ((IDENT_boxed) L10)->immutable_part = k;
 key_hash_e = L10;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L13 = ret_val2;
 L141_=ASIZE((FMAPIDENTdTP)r); 
 L15 = L141_;
 L161_=INTMINUS(L15,2); 
 L17 = L161_;
 L181_=L13&L17; 
 orig_h = L181_;
 h = orig_h;
 L191_=ASIZE((FMAPIDENTdTP)r); 
 L20 = L191_;
 L211_=INTMINUS(L20,1); 
 asm1 = L211_;
 while (1) {
  L221_=ARR((FMAPIDENTdTP)r,h); 
  L23=L221_;
  tk = L23.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e.str==(STR)0);
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQIDENT) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_i = elt_eq_e2;
  ret_val7 = (is_eq_self.str==is_eq_i.str);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPIDENTdTP_zero;
   create_at1 = k;
   create_at2 = t;
   L24 = create_self;
   L24.t1 = create_at1;
   L25 = L24;
   L25.t2 = create_at2;
   ret_val8 = L25;
   L26 = ret_val8;
   SARR((FMAPIDENTdTP)r,h,L26); 
   ;
   ret_val = r;
   return ret_val;
  }
  L281_=INTPLUS(h,1); 
  h = L281_;
 }
 after_loop: ;
 L291_=(h)==(asm1); 
 if (L291_) {
  h = 0;
  while (1) {
   L301_=ARR((FMAPIDENTdTP)r,h); 
   L31=L301_;
   tk1 = L31.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1.str==(STR)0);
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQIDENT) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_i1 = elt_eq_e21;
   ret_val13 = (is_eq_self1.str==is_eq_i1.str);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPIDENTdTP_zero;
    create_at11 = k;
    create_at21 = t;
    L32 = create_self1;
    L32.t1 = create_at11;
    L33 = L32;
    L33.t2 = create_at21;
    ret_val14 = L33;
    L34 = ret_val14;
    SARR((FMAPIDENTdTP)r,h,L34); 
    ;
    ret_val = r;
    return ret_val;
   }
   L361_=INTPLUS(h,1); 
   h = L361_;
  }
  after_loop1: ;
 }
 create_self2 = TUPIDENTdTP_zero;
 create_at12 = k;
 create_at22 = t;
 L37 = create_self2;
 L37.t1 = create_at12;
 L38 = L37;
 L38.t2 = create_at22;
 ret_val15 = L38;
 L39 = ret_val15;
 SARR((FMAPIDENTdTP)r,h,L39); 
 ;
 L42 = ATTR(r,hsize);
 L431_=INTPLUS(L42,1); 
 SATTR(r,hsize,L431_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPIDENTdTP FMAPID1805748084(FMAPIDENTdTP self) {
 FMAPIDENTdTP ret_val;
 INT ns = INT_zero;
 FMAPIDENTdTP r;
 FMAPIDENTdTP L11;
 FMAPIDENTdTP insert_pair_self;
 TUPIDENTdTP insert_pair_p = TUPIDENTdTP_zero;
 FMAPIDENTdTP ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTdTP L9;
 L21_=ASIZE((FMAPIDENTdTP)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPID1112745107(self, ns);
 {
  struct FMAPID1741388334_frame_struct other1_0;
FMAPID1741388334_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPID1741388334(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPID1786193115(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSI1121373188 FMAPSI1358355632(FMAPSI1121373188 self, SIG k, FRAME_LAYOUT t) {
 FMAPSI1121373188 ret_val;
 FMAPSI1121373188 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI1121373188 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSI1121373188 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSI1121373188 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSI1121373188 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val7 = BOOL_zero;
 TUPSIG2023533247 create_self = TUPSIG2023533247_zero;
 SIG create_at1;
 FRAME_LAYOUT create_at2;
 TUPSIG2023533247 ret_val8 = TUPSIG2023533247_zero;
 FMAPSI1121373188 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPSI1121373188 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val13 = BOOL_zero;
 TUPSIG2023533247 create_self1 = TUPSIG2023533247_zero;
 SIG create_at11;
 FRAME_LAYOUT create_at21;
 TUPSIG2023533247 ret_val14 = TUPSIG2023533247_zero;
 TUPSIG2023533247 create_self2 = TUPSIG2023533247_zero;
 SIG create_at12;
 FRAME_LAYOUT create_at22;
 TUPSIG2023533247 ret_val15 = TUPSIG2023533247_zero;
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
 TUPSIG2023533247 L201_;
 TUPSIG2023533247 L21;
 TUPSIG2023533247 L22;
 TUPSIG2023533247 L23;
 TUPSIG2023533247 L24;
 INT L261_;
 BOOL L271_;
 TUPSIG2023533247 L281_;
 TUPSIG2023533247 L29;
 TUPSIG2023533247 L30;
 TUPSIG2023533247 L31;
 TUPSIG2023533247 L32;
 INT L341_;
 TUPSIG2023533247 L35;
 TUPSIG2023533247 L36;
 TUPSIG2023533247 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSI1121373188) NULL))) {
  r = FMAPSI2029716764(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSI1121373188)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPSI1801446319;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPSI484237251(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSI1121373188)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSI1121373188)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSI1121373188)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((SIG) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSIG) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_s = elt_eq_e2;
  ret_val7 = (is_eq_self==is_eq_s);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSIG2023533247_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPSI1121373188)r,h,L24); 
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
   L281_=ARR((FMAPSI1121373188)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((SIG) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSIG) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_s1 = elt_eq_e21;
   ret_val13 = (is_eq_self1==is_eq_s1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPSIG2023533247_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPSI1121373188)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSIG2023533247_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPSI1121373188)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSI1121373188 FMAPSI2029716764(FMAPSI1121373188 self, INT n) {
 FMAPSI1121373188 ret_val;
 FMAPSI1121373188 r;
 FMAPSI1121373188 L11;
 FMAPSI1121373188 key_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 FMAPSI1121373188 key_nil_self1;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 TUPSIG2023533247 create_self = TUPSIG2023533247_zero;
 SIG create_at1;
 FRAME_LAYOUT create_at2;
 TUPSIG2023533247 ret_val5 = TUPSIG2023533247_zero;
 INT L21 = INT_zero;
 FMAPSI1121373188 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSIG2023533247 L10;
 TUPSIG2023533247 L12;
 TUPSIG2023533247 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSI1121373188_struct)-sizeof(TUPSIG2023533247))+(L4)*sizeof(TUPSIG2023533247);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSI1121373188_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSI1121373188) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSIG) NULL);
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
   L91_br=ASIZE((FMAPSI1121373188)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSIG2023533247_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FRAME_LAYOUT) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSI1121373188)L11,L21,L13); 
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

FMAPSI1121373188 FMAPSI484237251(FMAPSI1121373188 self) {
 FMAPSI1121373188 ret_val;
 INT ns = INT_zero;
 FMAPSI1121373188 r;
 FMAPSI1121373188 L11;
 FMAPSI1121373188 insert_pair_self;
 TUPSIG2023533247 insert_pair_p = TUPSIG2023533247_zero;
 FMAPSI1121373188 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIG2023533247 L9;
 L21_=ASIZE((FMAPSI1121373188)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPSI2029716764(self, ns);
 {
  struct FMAPSI118804083_frame_struct other1_0;
FMAPSI118804083_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPSI118804083(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPSI1358355632(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSI2103621588 FMAPSI1221142349(FMAPSI2103621588 self, SIG k, ABSTRA151498155 t) {
 FMAPSI2103621588 ret_val;
 FMAPSI2103621588 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI2103621588 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSI2103621588 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSI2103621588 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSI2103621588 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val7 = BOOL_zero;
 TUPSIG1754078736 create_self = TUPSIG1754078736_zero;
 SIG create_at1;
 ABSTRA151498155 create_at2;
 TUPSIG1754078736 ret_val8 = TUPSIG1754078736_zero;
 FMAPSI2103621588 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPSI2103621588 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val13 = BOOL_zero;
 TUPSIG1754078736 create_self1 = TUPSIG1754078736_zero;
 SIG create_at11;
 ABSTRA151498155 create_at21;
 TUPSIG1754078736 ret_val14 = TUPSIG1754078736_zero;
 TUPSIG1754078736 create_self2 = TUPSIG1754078736_zero;
 SIG create_at12;
 ABSTRA151498155 create_at22;
 TUPSIG1754078736 ret_val15 = TUPSIG1754078736_zero;
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
 TUPSIG1754078736 L201_;
 TUPSIG1754078736 L21;
 TUPSIG1754078736 L22;
 TUPSIG1754078736 L23;
 TUPSIG1754078736 L24;
 INT L261_;
 BOOL L271_;
 TUPSIG1754078736 L281_;
 TUPSIG1754078736 L29;
 TUPSIG1754078736 L30;
 TUPSIG1754078736 L31;
 TUPSIG1754078736 L32;
 INT L341_;
 TUPSIG1754078736 L35;
 TUPSIG1754078736 L36;
 TUPSIG1754078736 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSI2103621588) NULL))) {
  r = FMAPSI337958229(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSI2103621588)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPSI362068854;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPSI1818347340(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSI2103621588)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSI2103621588)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSI2103621588)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((SIG) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSIG) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_s = elt_eq_e2;
  ret_val7 = (is_eq_self==is_eq_s);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSIG1754078736_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPSI2103621588)r,h,L24); 
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
   L281_=ARR((FMAPSI2103621588)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((SIG) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSIG) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_s1 = elt_eq_e21;
   ret_val13 = (is_eq_self1==is_eq_s1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPSIG1754078736_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPSI2103621588)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSIG1754078736_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPSI2103621588)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSI2103621588 FMAPSI1818347340(FMAPSI2103621588 self) {
 FMAPSI2103621588 ret_val;
 INT ns = INT_zero;
 FMAPSI2103621588 r;
 FMAPSI2103621588 L11;
 FMAPSI2103621588 insert_pair_self;
 TUPSIG1754078736 insert_pair_p = TUPSIG1754078736_zero;
 FMAPSI2103621588 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIG1754078736 L9;
 L21_=ASIZE((FMAPSI2103621588)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPSI337958229(self, ns);
 {
  struct FMAPSI1591619598_frame_struct other1_0;
FMAPSI1591619598_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPSI1591619598(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPSI1221142349(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSI2103621588 FMAPSI337958229(FMAPSI2103621588 self, INT n) {
 FMAPSI2103621588 ret_val;
 FMAPSI2103621588 r;
 FMAPSI2103621588 L11;
 FMAPSI2103621588 key_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 FMAPSI2103621588 key_nil_self1;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 TUPSIG1754078736 create_self = TUPSIG1754078736_zero;
 SIG create_at1;
 ABSTRA151498155 create_at2;
 TUPSIG1754078736 ret_val5 = TUPSIG1754078736_zero;
 INT L21 = INT_zero;
 FMAPSI2103621588 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSIG1754078736 L10;
 TUPSIG1754078736 L12;
 TUPSIG1754078736 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSI2103621588_struct)-sizeof(TUPSIG1754078736))+(L4)*sizeof(TUPSIG1754078736);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSI2103621588_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSI2103621588) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSIG) NULL);
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
   L91_br=ASIZE((FMAPSI2103621588)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSIG1754078736_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((ABSTRA151498155) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSI2103621588)L11,L21,L13); 
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

FMAPSI518162669 FMAPSI1205266134(FMAPSI518162669 self, TUPSIG578692189 p) {
 FMAPSI518162669 ret_val;
 ret_val = FMAPSI396914288(self, p.t1, p.t2);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSI518162669 FMAPSI1355362246(FMAPSI518162669 self) {
 FMAPSI518162669 ret_val;
 INT ns = INT_zero;
 FMAPSI518162669 r;
 FMAPSI518162669 L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIG578692189 L9;
 L21_=ASIZE((FMAPSI518162669)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPSI522072323(self, ns);
 {
  struct FMAPSI1135054800_frame_struct other1_0;
FMAPSI1135054800_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FMAPSI1135054800(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FMAPSI1205266134(r, L9);
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

FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669 self, SIG k) {
 FMAPSI518162669 ret_val;
 INT h = INT_zero;
 SIG tk;
 INT i = INT_zero;
 SIG tk1;
 INT hsh = INT_zero;
 FMAPSI518162669 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSI518162669 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSI518162669 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FMAPSI518162669 key_nil_self;
 SIG ret_val7;
 ELT_NILSIG elt_nil_self;
 SIG ret_val8;
 SIG e = ((SIG) NULL);
 TUPSIG578692189 create_self = TUPSIG578692189_zero;
 SIG create_at1;
 AM_ROUT_DEF create_at2;
 TUPSIG578692189 ret_val9 = TUPSIG578692189_zero;
 FMAPSI518162669 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val11 = BOOL_zero;
 FMAPSI518162669 key_hash_self1;
 OB key_hash_e1;
 INT ret_val12 = INT_zero;
 FMAPSI518162669 key_nil_self1;
 SIG ret_val13;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val14;
 SIG e1 = ((SIG) NULL);
 TUPSIG578692189 create_self1 = TUPSIG578692189_zero;
 SIG create_at11;
 AM_ROUT_DEF create_at21;
 TUPSIG578692189 ret_val15 = TUPSIG578692189_zero;
 FMAPSI518162669 key_nil_self2;
 SIG ret_val16;
 ELT_NILSIG elt_nil_self2;
 SIG ret_val17;
 SIG e2 = ((SIG) NULL);
 TUPSIG578692189 create_self2 = TUPSIG578692189_zero;
 SIG create_at12;
 AM_ROUT_DEF create_at22;
 TUPSIG578692189 ret_val18 = TUPSIG578692189_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSIG578692189 L71_;
 TUPSIG578692189 L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 INT L141_;
 TUPSIG578692189 L15;
 TUPSIG578692189 L16;
 TUPSIG578692189 L17;
 INT L19;
 INT L201_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 INT L271_;
 TUPSIG578692189 L281_;
 TUPSIG578692189 L29;
 INT L30;
 INT L311_;
 INT L32;
 INT L331_;
 INT L34;
 INT L351_;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 BOOL L421_;
 BOOL L43;
 BOOL L441_;
 TUPSIG578692189 L451_;
 TUPSIG578692189 L46;
 TUPSIG578692189 L48;
 TUPSIG578692189 L49;
 TUPSIG578692189 L50;
 BOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 BOOL L561_;
 TUPSIG578692189 L571_;
 TUPSIG578692189 L58;
 TUPSIG578692189 L60;
 TUPSIG578692189 L62;
 TUPSIG578692189 L63;
 if ((self==((FMAPSI518162669) NULL))) {
  ret_val = ((FMAPSI518162669) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSI518162669)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSI518162669)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSIG) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_s = elt_eq_e2;
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L91_=ASIZE((FMAPSI518162669)self); 
  L10 = L91_;
  L111_=INTMINUS(L10,2); 
  L12 = L111_;
  L131_=(h)==(L12); 
  if (L131_) {
   h = 0;
  }
  else {
   L141_=INTPLUS(h,1); 
   h = L141_;
  }
 }
 after_loop: ;
 create_self = TUPSIG578692189_zero;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSIG) NULL);
 ret_val8 = ((SIG) NULL);
 ret_val7 = ret_val8;
 create_at1 = ret_val7;
 create_at2 = ((AM_ROUT_DEF) NULL);
 L15 = create_self;
 L15.t1 = create_at1;
 L16 = L15;
 L16.t2 = create_at2;
 ret_val9 = L16;
 L17 = ret_val9;
 SARR((FMAPSI518162669)self,h,L17); 
 ;
 L19 = ATTR(self,hsize);
 L201_=INTMINUS(L19,1); 
 SATTR(self,hsize,L201_);
 i = h;
 while (1) {
  L221_=ASIZE((FMAPSI518162669)self); 
  L23 = L221_;
  L241_=INTMINUS(L23,2); 
  L25 = L241_;
  L261_=(i)==(L25); 
  if (L261_) {
   i = 0;
  }
  else {
   L271_=INTPLUS(i,1); 
   i = L271_;
  }
  L281_=ARR((FMAPSI518162669)self,i); 
  L29=L281_;
  tk1 = L29.t1;
  is_key_nil_self1 = self;
  is_key_nil_e1 = tk1;
  is_elt_nil_self1 = ((ELT_NILSIG) NULL);
  is_elt_nil_e1 = is_key_nil_e1;
  ret_val11 = (is_elt_nil_e1==((SIG) NULL));
  ret_val10 = ret_val11;
  if (ret_val10) {
   goto after_loop1;
  }
  key_hash_self1 = self;
  key_hash_e1 = ((OB) tk1);
  ret_val12 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e1);
  L30 = ret_val12;
  L311_=ASIZE((FMAPSI518162669)self); 
  L32 = L311_;
  L331_=INTMINUS(L32,2); 
  L34 = L331_;
  L351_=L30&L34; 
  hsh = L351_;
  L361_=(i)<(hsh); 
  L37 = L361_;
  L381_=!(L37); 
  if (L381_) {
   L401_=(h)<(i); 
   if (L401_) {
    L421_=(h)<(hsh); 
    L43 = L421_;
    L441_=!(L43); 
    L39 = L441_;
   } else {
    L39 = FALSE;
   }
   if (L39) {
    L451_=ARR((FMAPSI518162669)self,i); 
    L46 = L451_;
    SARR((FMAPSI518162669)self,h,L46); 
    ;
    h = i;
    create_self1 = TUPSIG578692189_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val14 = ((SIG) NULL);
    ret_val13 = ret_val14;
    create_at11 = ret_val13;
    create_at21 = ((AM_ROUT_DEF) NULL);
    L48 = create_self1;
    L48.t1 = create_at11;
    L49 = L48;
    L49.t2 = create_at21;
    ret_val15 = L49;
    L50 = ret_val15;
    SARR((FMAPSI518162669)self,i,L50); 
    ;
   }
  }
  else {
   L531_=(h)<(hsh); 
   L54 = L531_;
   L551_=!(L54); 
   if (L551_) {
    L52 = TRUE;
   } else {
    L561_=(h)<(i); 
    L52 = L561_;
   }
   if (L52) {
    L571_=ARR((FMAPSI518162669)self,i); 
    L58 = L571_;
    SARR((FMAPSI518162669)self,h,L58); 
    ;
    h = i;
    create_self2 = TUPSIG578692189_zero;
    key_nil_self2 = self;
    elt_nil_self2 = ((ELT_NILSIG) NULL);
    ret_val17 = ((SIG) NULL);
    ret_val16 = ret_val17;
    create_at12 = ret_val16;
    create_at22 = ((AM_ROUT_DEF) NULL);
    L60 = create_self2;
    L60.t1 = create_at12;
    L62 = L60;
    L62.t2 = create_at22;
    ret_val18 = L62;
    L63 = ret_val18;
    SARR((FMAPSI518162669)self,i,L63); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FMAPSI773227610(self)) {
  ret_val = FMAPSI90181471(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FMAPSI518162669 FMAPSI396914288(FMAPSI518162669 self, SIG k, AM_ROUT_DEF t) {
 FMAPSI518162669 ret_val;
 FMAPSI518162669 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI518162669 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSI518162669 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSI518162669 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSI518162669 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val7 = BOOL_zero;
 TUPSIG578692189 create_self = TUPSIG578692189_zero;
 SIG create_at1;
 AM_ROUT_DEF create_at2;
 TUPSIG578692189 ret_val8 = TUPSIG578692189_zero;
 FMAPSI518162669 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPSI518162669 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val13 = BOOL_zero;
 TUPSIG578692189 create_self1 = TUPSIG578692189_zero;
 SIG create_at11;
 AM_ROUT_DEF create_at21;
 TUPSIG578692189 ret_val14 = TUPSIG578692189_zero;
 TUPSIG578692189 create_self2 = TUPSIG578692189_zero;
 SIG create_at12;
 AM_ROUT_DEF create_at22;
 TUPSIG578692189 ret_val15 = TUPSIG578692189_zero;
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
 TUPSIG578692189 L201_;
 TUPSIG578692189 L21;
 TUPSIG578692189 L22;
 TUPSIG578692189 L23;
 TUPSIG578692189 L24;
 INT L261_;
 BOOL L271_;
 TUPSIG578692189 L281_;
 TUPSIG578692189 L29;
 TUPSIG578692189 L30;
 TUPSIG578692189 L31;
 TUPSIG578692189 L32;
 INT L341_;
 TUPSIG578692189 L35;
 TUPSIG578692189 L36;
 TUPSIG578692189 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSI518162669) NULL))) {
  r = FMAPSI522072323(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSI518162669)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPSI1253630331;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPSI1355362246(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSI518162669)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSI518162669)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSI518162669)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((SIG) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSIG) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_s = elt_eq_e2;
  ret_val7 = (is_eq_self==is_eq_s);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSIG578692189_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPSI518162669)r,h,L24); 
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
   L281_=ARR((FMAPSI518162669)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((SIG) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSIG) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_s1 = elt_eq_e21;
   ret_val13 = (is_eq_self1==is_eq_s1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPSIG578692189_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPSI518162669)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSIG578692189_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPSI518162669)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSI518162669 FMAPSI522072323(FMAPSI518162669 self, INT n) {
 FMAPSI518162669 ret_val;
 FMAPSI518162669 r;
 FMAPSI518162669 L11;
 FMAPSI518162669 key_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 FMAPSI518162669 key_nil_self1;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 TUPSIG578692189 create_self = TUPSIG578692189_zero;
 SIG create_at1;
 AM_ROUT_DEF create_at2;
 TUPSIG578692189 ret_val5 = TUPSIG578692189_zero;
 INT L21 = INT_zero;
 FMAPSI518162669 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSIG578692189 L10;
 TUPSIG578692189 L12;
 TUPSIG578692189 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSI518162669_struct)-sizeof(TUPSIG578692189))+(L4)*sizeof(TUPSIG578692189);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSI518162669_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSI518162669) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSIG) NULL);
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
   L91_br=ASIZE((FMAPSI518162669)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSIG578692189_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((AM_ROUT_DEF) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSI518162669)L11,L21,L13); 
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

FMAPSI518162669 FMAPSI90181471(FMAPSI518162669 self) {
 FMAPSI518162669 ret_val;
 INT ns = INT_zero;
 FMAPSI518162669 r;
 FMAPSI518162669 L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIG578692189 L9;
 L21_=ASIZE((FMAPSI518162669)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPSI522072323(self, ns);
 {
  struct FMAPSI1135054800_frame_struct other1_0;
FMAPSI1135054800_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FMAPSI1135054800(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FMAPSI1205266134(r, L9);
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

FMAPSIGSTR FMAPSI1249683373(FMAPSIGSTR self, INT n) {
 FMAPSIGSTR ret_val;
 FMAPSIGSTR r;
 FMAPSIGSTR L11;
 FMAPSIGSTR key_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 FMAPSIGSTR key_nil_self1;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 TUPSIGSTR create_self = TUPSIGSTR_zero;
 SIG create_at1;
 STR create_at2;
 TUPSIGSTR ret_val5 = TUPSIGSTR_zero;
 INT L21 = INT_zero;
 FMAPSIGSTR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSIGSTR L10;
 TUPSIGSTR L12;
 TUPSIGSTR L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSIGSTR_struct)-sizeof(TUPSIGSTR))+(L4)*sizeof(TUPSIGSTR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSIGSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSIGSTR) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSIG) NULL);
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
   L91_br=ASIZE((FMAPSIGSTR)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSIGSTR_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((STR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSIGSTR)L11,L21,L13); 
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

FMAPSIGSTR FMAPSI1579062782(FMAPSIGSTR self) {
 FMAPSIGSTR ret_val;
 INT ns = INT_zero;
 FMAPSIGSTR r;
 FMAPSIGSTR L11;
 FMAPSIGSTR insert_pair_self;
 TUPSIGSTR insert_pair_p = TUPSIGSTR_zero;
 FMAPSIGSTR ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSIGSTR L9;
 L21_=ASIZE((FMAPSIGSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPSI1249683373(self, ns);
 {
  struct FMAPSI1080015256_frame_struct other1_0;
FMAPSI1080015256_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPSI1080015256(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPSI1897406528(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSIGSTR FMAPSI1897406528(FMAPSIGSTR self, SIG k, STR t) {
 FMAPSIGSTR ret_val;
 FMAPSIGSTR r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSIGSTR should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSIGSTR key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSIGSTR is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSIGSTR key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val7 = BOOL_zero;
 TUPSIGSTR create_self = TUPSIGSTR_zero;
 SIG create_at1;
 STR create_at2;
 TUPSIGSTR ret_val8 = TUPSIGSTR_zero;
 FMAPSIGSTR is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPSIGSTR key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val13 = BOOL_zero;
 TUPSIGSTR create_self1 = TUPSIGSTR_zero;
 SIG create_at11;
 STR create_at21;
 TUPSIGSTR ret_val14 = TUPSIGSTR_zero;
 TUPSIGSTR create_self2 = TUPSIGSTR_zero;
 SIG create_at12;
 STR create_at22;
 TUPSIGSTR ret_val15 = TUPSIGSTR_zero;
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
 TUPSIGSTR L201_;
 TUPSIGSTR L21;
 TUPSIGSTR L22;
 TUPSIGSTR L23;
 TUPSIGSTR L24;
 INT L261_;
 BOOL L271_;
 TUPSIGSTR L281_;
 TUPSIGSTR L29;
 TUPSIGSTR L30;
 TUPSIGSTR L31;
 TUPSIGSTR L32;
 INT L341_;
 TUPSIGSTR L35;
 TUPSIGSTR L36;
 TUPSIGSTR L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSIGSTR) NULL))) {
  r = FMAPSI1249683373(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSIGSTR)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPSI22065355;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPSI1579062782(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSIGSTR)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSIGSTR)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSIGSTR)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((SIG) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSIG) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_s = elt_eq_e2;
  ret_val7 = (is_eq_self==is_eq_s);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSIGSTR_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPSIGSTR)r,h,L24); 
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
   L281_=ARR((FMAPSIGSTR)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((SIG) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSIG) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_s1 = elt_eq_e21;
   ret_val13 = (is_eq_self1==is_eq_s1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPSIGSTR_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPSIGSTR)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSIGSTR_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPSIGSTR)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FRAME_LAYOUT FMAPSI547426166(FMAPSI1121373188 self, SIG k) {
 FRAME_LAYOUT ret_val;
 INT h = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSI1121373188 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSI1121373188 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSI1121373188 key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FMAPSI1121373188 is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPSI1121373188 key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSIG2023533247 L71_;
 TUPSIG2023533247 L8;
 TUPSIG2023533247 L91_;
 TUPSIG2023533247 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSIG2023533247 L171_;
 TUPSIG2023533247 L18;
 TUPSIG2023533247 L191_;
 TUPSIG2023533247 L20;
 INT L221_;
 if ((self==((FMAPSI1121373188) NULL))) {
  ret_val = ((FRAME_LAYOUT) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSI1121373188)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSI1121373188)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSIG) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_s = elt_eq_e2;
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSI1121373188)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSI1121373188)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSI1121373188)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSIG) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_s1 = elt_eq_e21;
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPSI1121373188)self,h); 
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
 ret_val = ((FRAME_LAYOUT) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FMAPID99794443(FMAPIDENTdTP self) {
 INT ret_val = INT_zero;
 if ((self==((FMAPIDENTdTP) NULL))) {
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

STR FMAPAM118642384(FMAPAM1787628573 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_653469574 p = TUPAM_653469574_zero;
 AM_ROUT_DEF k;
 FLISTA1062334999 e;
 FMAPAM1787628573 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_653469574 L41_;
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
  L11_=ASIZE((FMAPAM1787628573)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM1787628573)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name10));
   L8 = FSTR_p1752847026(L7, FLISTA1884067430(e));
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

STR FMAPAM459426429(FMAPAM1244483504 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPAM_1024994801 p = TUPAM_1024994801_zero;
 AM_ROUT_DEF k;
 FLISTA1409846210 e;
 FMAPAM1244483504 is_key_nil_self;
 AM_ROUT_DEF is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPAM_1024994801 L41_;
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
  L11_=ASIZE((FMAPAM1244483504)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPAM1244483504)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NI602850927) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((AM_ROUT_DEF) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name10));
   L8 = FSTR_p1752847026(L7, FLISTA389202533(e));
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

STR FMAPID1860739187(FMAPIDENTdTP self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPIDENTdTP p = TUPIDENTdTP_zero;
 IDENT k = IDENT_zero;
 dTP e;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPIDENTdTP L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 FSTR L7;
 dTP L8;
 FSTR L9;
 extern STR name10;
 extern STR name10;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPIDENTdTP)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPIDENTdTP)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e.str==(STR)0);
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name10));
   L8 = e;
   L9 = FSTR_p1752847026(L7, (*dTP_strrSTR[TAG(L8)])(L8));
   res = FSTR_p1752847026(L9, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L101_=INTPLUS(i,1); 
  i = L101_;
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

STR FMAPID1895267434(FMAPID739086519 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPIDE641654124 p = TUPIDE641654124_zero;
 IDENT k = IDENT_zero;
 AM_SHARED_EXPR e;
 FMAPID739086519 is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPIDE641654124 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name10;
 INT L71_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPID739086519)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPID739086519)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e.str==(STR)0);
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

STR FMAPSI1424048021(FMAPSI1349988702 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGARG_LAYOUT p = TUPSIGARG_LAYOUT_zero;
 SIG k;
 ARG_LAYOUT e;
 FMAPSI1349988702 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGARG_LAYOUT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 FSTR L10;
 extern STR name10;
 extern STR name10;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSI1349988702)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSI1349988702)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((SIG) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, ATTR(k,str));
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   L10 = FSTR_p1752847026(L9, ATTR(e,str));
   res = FSTR_p1752847026(L10, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
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

STR FMAPSI1519487311(FMAPSIGSTR self, SIG k) {
 STR ret_val;
 INT h = INT_zero;
 SIG tk;
 SIG tk1;
 FMAPSIGSTR key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSIGSTR is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSIGSTR key_eq_self;
 SIG key_eq_e1;
 SIG key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FMAPSIGSTR is_key_nil_self1;
 SIG is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPSIGSTR key_eq_self1;
 SIG key_eq_e11;
 SIG key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSIGSTR L71_;
 TUPSIGSTR L8;
 TUPSIGSTR L91_;
 TUPSIGSTR L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSIGSTR L171_;
 TUPSIGSTR L18;
 TUPSIGSTR L191_;
 TUPSIGSTR L20;
 INT L221_;
 if ((self==((FMAPSIGSTR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSIGSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSIGSTR)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSIG) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_s = elt_eq_e2;
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSIGSTR)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSIGSTR)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSIGSTR)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSIG) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSIG) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_s1 = elt_eq_e21;
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPSIGSTR)self,h); 
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
 ret_val = ((STR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPSI1996405359(FMAPSI2103621588 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG1754078736 p = TUPSIG1754078736_zero;
 SIG k;
 ABSTRA151498155 e;
 FMAPSI2103621588 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG1754078736 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 FSTR L10;
 extern STR name10;
 extern STR name10;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSI2103621588)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSI2103621588)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((SIG) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, ATTR(k,str));
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   L10 = FSTR_p1752847026(L9, ATTR(e,str));
   res = FSTR_p1752847026(L10, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
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

STR FMAPSI2071674985(FMAPSI1121373188 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG2023533247 p = TUPSIG2023533247_zero;
 SIG k;
 FRAME_LAYOUT e;
 FMAPSI1121373188 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG2023533247 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 FSTR L10;
 extern STR name10;
 extern STR name10;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSI1121373188)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSI1121373188)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((SIG) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, ATTR(k,str));
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   L10 = FSTR_p1752847026(L9, ATTR(e,str));
   res = FSTR_p1752847026(L10, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
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

STR FMAPSI866101256(FMAPSIGSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIGSTR p = TUPSIGSTR_zero;
 SIG k;
 STR e;
 FMAPSIGSTR is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIGSTR L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 FSTR L10;
 extern STR name10;
 extern STR name10;
 INT L121_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSIGSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSIGSTR)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((SIG) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, ATTR(k,str));
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   str_self = e;
   ret_val3 = str_self;
   L10 = FSTR_p1752847026(L9, ret_val3);
   res = FSTR_p1752847026(L10, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPSI99914640(FMAPSI518162669 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSIG578692189 p = TUPSIG578692189_zero;
 SIG k;
 AM_ROUT_DEF e;
 FMAPSI518162669 is_key_nil_self;
 SIG is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSIG578692189 L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 INT L91_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSI518162669)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSI518162669)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSIG) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((SIG) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, ATTR(k,str));
   res = FSTR_p1752847026(L8, ((STR) &name15));
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

dTP FMAPID86862037(FMAPIDENTdTP self, IDENT k) {
 dTP ret_val;
 INT h = INT_zero;
 IDENT tk = IDENT_zero;
 IDENT tk1 = IDENT_zero;
 FMAPIDENTdTP key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPIDENTdTP is_key_nil_self;
 IDENT is_key_nil_e = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self;
 IDENT is_elt_nil_e = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 FMAPIDENTdTP key_eq_self;
 IDENT key_eq_e1 = IDENT_zero;
 IDENT key_eq_e2 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQIDENT elt_eq_self;
 IDENT elt_eq_e1 = IDENT_zero;
 IDENT elt_eq_e2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 FMAPIDENTdTP is_key_nil_self1;
 IDENT is_key_nil_e1 = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILIDENT is_elt_nil_self1;
 IDENT is_elt_nil_e1 = IDENT_zero;
 BOOL ret_val8 = BOOL_zero;
 FMAPIDENTdTP key_eq_self1;
 IDENT key_eq_e11 = IDENT_zero;
 IDENT key_eq_e21 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQIDENT elt_eq_self1;
 IDENT elt_eq_e11 = IDENT_zero;
 IDENT elt_eq_e21 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTdTP L91_;
 TUPIDENTdTP L10;
 TUPIDENTdTP L111_;
 TUPIDENTdTP L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 TUPIDENTdTP L191_;
 TUPIDENTdTP L20;
 TUPIDENTdTP L211_;
 TUPIDENTdTP L22;
 INT L231_;
 if ((self==((FMAPIDENTdTP) NULL))) {
  ret_val = ((dTP) NULL);
  return ret_val;
 }
 key_hash_self = self;
 L2=ZALLOC(sizeof(struct IDENT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=IDENT_tag;
 L1 = (OB)((IDENT_boxed) L2);
 ((IDENT_boxed) L1)->immutable_part = k;
 key_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L3 = ret_val1;
 L41_=ASIZE((FMAPIDENTdTP)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FMAPIDENTdTP)self,h); 
  L10=L91_;
  tk = L10.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILIDENT) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e.str==(STR)0);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQIDENT) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_i = elt_eq_e2;
   ret_val6 = (is_eq_self.str==is_eq_i.str);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L111_=ARR((FMAPIDENTdTP)self,h); 
    L12=L111_;
    ret_val = L12.t2;
    return ret_val;
   }
  }
  L131_=INTPLUS(h,1); 
  h = L131_;
 }
 after_loop: ;
 L141_=ASIZE((FMAPIDENTdTP)self); 
 L15 = L141_;
 L161_=INTMINUS(L15,1); 
 L17 = L161_;
 L181_=(h)==(L17); 
 if (L181_) {
  h = 0;
  while (1) {
   L191_=ARR((FMAPIDENTdTP)self,h); 
   L20=L191_;
   tk1 = L20.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILIDENT) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1.str==(STR)0);
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQIDENT) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_i1 = elt_eq_e21;
    ret_val11 = (is_eq_self1.str==is_eq_i1.str);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L211_=ARR((FMAPIDENTdTP)self,h); 
     L22=L211_;
     ret_val = L22.t2;
     return ret_val;
    }
   }
   L231_=INTPLUS(h,1); 
   h = L231_;
  }
  after_loop1: ;
 }
 ret_val = ((dTP) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 1

ABSTRA151498155 FMAPSI140286185(FMAPSI140286185_frame frame) {
 ABSTRA151498155 dummy = ((ABSTRA151498155) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSIG1754078736 L5;
 TUPSIG1754078736 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI2103621588) NULL));
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
     frame->L41_br=ASIZE((FMAPSI2103621588)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI2103621588)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

ARG_LAYOUT FMAPSI688594122(FMAPSI688594122_frame frame) {
 ARG_LAYOUT dummy = ((ARG_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSIGARG_LAYOUT L5;
 TUPSIGARG_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI1349988702) NULL));
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
     frame->L41_br=ASIZE((FMAPSI1349988702)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI1349988702)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

FRAME_LAYOUT FMAPSI1790064862(FMAPSI1790064862_frame frame) {
 FRAME_LAYOUT dummy = ((FRAME_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSIG2023533247 L5;
 TUPSIG2023533247 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI1121373188) NULL));
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
     frame->L41_br=ASIZE((FMAPSI1121373188)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI1121373188)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

IDENT FMAPID1294197827(FMAPID1294197827_frame frame) {
 IDENT dummy = IDENT_zero;
 BOOL L1;
 BOOL L21_;
 TUPIDENTdTP L5;
 TUPIDENTdTP aL41_;
 TUPIDENTdTP L7;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPIDENTdTP) NULL));
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
     frame->L41_br=ASIZE((FMAPIDENTdTP)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPIDENTdTP)frame->L61,frame->L31); 
    L7=aL41_;
    frame->r = L7.t1;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r;
    frame->is_elt_nil_self = ((ELT_NILIDENT) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e.str==(STR)0);
    frame->ret_val = frame->ret_val1;
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

TUPAM_1024994801 FMAPAM1596285950(FMAPAM1596285950_frame frame) {
 TUPAM_1024994801 dummy = TUPAM_1024994801_zero;
 TUPAM_1024994801 aI_u_I;
 TUPAM_1024994801 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1244483504)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM1244483504)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPAM_1024994801 FMAPAM1866584795(FMAPAM1866584795_frame frame) {
 TUPAM_1024994801 dummy = TUPAM_1024994801_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_1024994801 L5;
 TUPAM_1024994801 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM1244483504) NULL));
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
     frame->L41_br=ASIZE((FMAPAM1244483504)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM1244483504)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI602850927) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_ROUT_DEF) NULL));
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

TUPAM_653469574 FMAPAM2028198677(FMAPAM2028198677_frame frame) {
 TUPAM_653469574 dummy = TUPAM_653469574_zero;
 BOOL L1;
 BOOL L21_;
 TUPAM_653469574 L5;
 TUPAM_653469574 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPAM1787628573) NULL));
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
     frame->L41_br=ASIZE((FMAPAM1787628573)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPAM1787628573)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NI602850927) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((AM_ROUT_DEF) NULL));
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

TUPAM_653469574 FMAPAM213652070(FMAPAM213652070_frame frame) {
 TUPAM_653469574 dummy = TUPAM_653469574_zero;
 TUPAM_653469574 aI_u_I;
 TUPAM_653469574 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1787628573)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPAM1787628573)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDE641654124 FMAPID2058523420(FMAPID2058523420_frame frame) {
 TUPIDE641654124 dummy = TUPIDE641654124_zero;
 TUPIDE641654124 aI_u_I;
 TUPIDE641654124 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPID739086519)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPID739086519)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDE641654124 FMAPID906752465(FMAPID906752465_frame frame) {
 TUPIDE641654124 dummy = TUPIDE641654124_zero;
 BOOL L1;
 BOOL L21_;
 TUPIDE641654124 L5;
 TUPIDE641654124 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPID739086519) NULL));
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
     frame->L41_br=ASIZE((FMAPID739086519)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPID739086519)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILIDENT) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e.str==(STR)0);
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

TUPIDENTdTP FMAPID1741388334(FMAPID1741388334_frame frame) {
 TUPIDENTdTP dummy = TUPIDENTdTP_zero;
 BOOL L1;
 BOOL L21_;
 TUPIDENTdTP L5;
 TUPIDENTdTP aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPIDENTdTP) NULL));
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
     frame->L41_br=ASIZE((FMAPIDENTdTP)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPIDENTdTP)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILIDENT) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e.str==(STR)0);
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

TUPIDENTdTP FMAPID328097389(FMAPID328097389_frame frame) {
 TUPIDENTdTP dummy = TUPIDENTdTP_zero;
 TUPIDENTdTP aI_u_I;
 TUPIDENTdTP L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPIDENTdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPIDENTdTP)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG1754078736 FMAPSI1591619598(FMAPSI1591619598_frame frame) {
 TUPSIG1754078736 dummy = TUPSIG1754078736_zero;
 BOOL L1;
 BOOL L21_;
 TUPSIG1754078736 L5;
 TUPSIG1754078736 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI2103621588) NULL));
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
     frame->L41_br=ASIZE((FMAPSI2103621588)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI2103621588)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

TUPSIG1754078736 FMAPSI714126805(FMAPSI714126805_frame frame) {
 TUPSIG1754078736 dummy = TUPSIG1754078736_zero;
 TUPSIG1754078736 aI_u_I;
 TUPSIG1754078736 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI2103621588)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSI2103621588)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG2023533247 FMAPSI118804083(FMAPSI118804083_frame frame) {
 TUPSIG2023533247 dummy = TUPSIG2023533247_zero;
 BOOL L1;
 BOOL L21_;
 TUPSIG2023533247 L5;
 TUPSIG2023533247 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI1121373188) NULL));
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
     frame->L41_br=ASIZE((FMAPSI1121373188)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI1121373188)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

TUPSIG2023533247 FMAPSI442499788(FMAPSI442499788_frame frame) {
 TUPSIG2023533247 dummy = TUPSIG2023533247_zero;
 TUPSIG2023533247 aI_u_I;
 TUPSIG2023533247 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI1121373188)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSI1121373188)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIG578692189 FMAPSI1135054800(FMAPSI1135054800_frame frame) {
 TUPSIG578692189 dummy = TUPSIG578692189_zero;
 BOOL L1;
 BOOL L21_;
 TUPSIG578692189 L5;
 TUPSIG578692189 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSI518162669) NULL));
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
     frame->L41_br=ASIZE((FMAPSI518162669)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSI518162669)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

TUPSIG578692189 FMAPSI803721117(FMAPSI803721117_frame frame) {
 TUPSIG578692189 dummy = TUPSIG578692189_zero;
 TUPSIG578692189 aI_u_I;
 TUPSIG578692189 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI518162669)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSI518162669)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGARG_LAYOUT FMAPSI1981723230(FMAPSI1981723230_frame frame) {
 TUPSIGARG_LAYOUT dummy = TUPSIGARG_LAYOUT_zero;
 TUPSIGARG_LAYOUT aI_u_I;
 TUPSIGARG_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI1349988702)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSI1349988702)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSIGSTR FMAPSI1080015256(FMAPSI1080015256_frame frame) {
 TUPSIGSTR dummy = TUPSIGSTR_zero;
 BOOL L1;
 BOOL L21_;
 TUPSIGSTR L5;
 TUPSIGSTR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSIGSTR) NULL));
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
     frame->L41_br=ASIZE((FMAPSIGSTR)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSIGSTR)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSIG) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((SIG) NULL));
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

TUPSIGSTR FMAPSI441885619(FMAPSI441885619_frame frame) {
 TUPSIGSTR dummy = TUPSIGSTR_zero;
 TUPSIGSTR aI_u_I;
 TUPSIGSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSIGSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSIGSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

dTP FMAPID944928219(FMAPID944928219_frame frame) {
 dTP dummy = ((dTP) NULL);
 BOOL L1;
 BOOL L21_;
 TUPIDENTdTP L5;
 TUPIDENTdTP aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPIDENTdTP) NULL));
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
     frame->L41_br=ASIZE((FMAPIDENTdTP)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPIDENTdTP)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILIDENT) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e.str==(STR)0);
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

void FMAPAM1319110900(FMAPAM1319110900_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1244483504)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM1244483504)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPAM466264060(FMAPAM466264060_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPAM1787628573)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPAM1787628573)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPID1049942143(FMAPID1049942143_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPIDENTdTP)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPIDENTdTP)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPID404856354(FMAPID404856354_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPID739086519)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPID739086519)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPSI1222948761(FMAPSI1222948761_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI2103621588)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSI2103621588)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPSI127190795(FMAPSI127190795_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSIGSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSIGSTR)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPSI266480270(FMAPSI266480270_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI1121373188)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSI1121373188)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPSI515269605(FMAPSI515269605_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSI518162669)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSI518162669)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
