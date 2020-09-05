#include "sather.h"

/* Layouts */

typedef struct dELTSTR_struct {
 OB_HEADER header;
 } *dELTSTR;

typedef struct ELT_ALGSTR_struct {/* layout for ELT_ALG{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_ALGSTR;

typedef struct ELT_EQ20798926_struct {/* layout for ELT_EQ{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ20798926;

typedef struct ELT_EQSFILE_ID_struct {/* layout for ELT_EQ{SFILE_ID} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQSFILE_ID;

typedef struct ELT_EQ251059601_struct {/* layout for ELT_EQ{SIDE_EFFECT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ251059601;

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

typedef struct ELT_EQ1475848140_struct {/* layout for ELT_EQ{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1475848140;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

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

typedef struct ELT_NILTP_LAYOUT_struct {/* layout for ELT_NIL{TP_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_LAYOUT;

typedef struct ELT_NI1575801776_struct {/* layout for ELT_NIL{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1575801776;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct IMMUTA421759893_struct {/* layout for IMMUTABLE_CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *IMMUTA421759893;

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

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

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

typedef struct TP_LAYOUT_struct {/* layout for TP_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 STR str;
 } *TP_LAYOUT;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

typedef struct FSETIM184436961_struct {/* layout for FSET{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FSETIM184436961;

typedef struct FSETIM205083650_frame_struct {
 INT state;
 FSETIM184436961 self;/* Formal argument: self */
 IMMUTA421759893 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETIM205083650_frame;

typedef struct FSETIM1858750716_frame_struct {
 INT state;
 FSETIM184436961 self;/* Formal argument: self */
 IMMUTA421759893 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETIM1858750716_frame;

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

typedef struct FSETSF12648100_frame_struct {
 INT state;
 FSETSFILE_ID self;/* Formal argument: self */
 SFILE_ID ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSF12648100_frame;

typedef struct FSETSF1565053082_frame_struct {
 INT state;
 FSETSFILE_ID self;/* Formal argument: self */
 SFILE_ID arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSF1565053082_frame;

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

typedef struct FSETSI1267678819_frame_struct {
 INT state;
 FSETSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSI1267678819_frame;

typedef struct FSETSI1989523573_frame_struct {
 INT state;
 FSETSIDE_EFFECT self;/* Formal argument: self */
 SIDE_EFFECT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSI1989523573_frame;

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

typedef struct FSETSI2112123043_frame_struct {
 INT state;
 FSETSIG self;/* Formal argument: self */
 SIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSI2112123043_frame;

typedef struct FSETSI1764914571_frame_struct {
 INT state;
 FSETSIG self;/* Formal argument: self */
 SIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETSI1764914571_frame;

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

typedef struct FSETST261377883_frame_struct {
 INT state;
 FSETSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETST261377883_frame;

typedef struct FSETST679307565_frame_struct {
 INT state;
 FSETSTR self;/* Formal argument: self */
 STR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETST679307565_frame;

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

typedef struct FSETTP49057756_frame_struct {
 INT state;
 FSETTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTP49057756_frame;

typedef struct FSETTP1601462738_frame_struct {
 INT state;
 FSETTP_CLASS self;/* Formal argument: self */
 TP_CLASS arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTP1601462738_frame;

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

typedef struct FSETTP_LAYOUT_struct {/* layout for FSET{TP_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TP_LAYOUT_struct *arr_part[1];
 } *FSETTP_LAYOUT;

typedef struct FSETTP381129783_frame_struct {
 INT state;
 FSETTP_LAYOUT self;/* Formal argument: self */
 TP_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTP381129783_frame;

typedef struct FSETTP1672834608_frame_struct {
 INT state;
 FSETTP_LAYOUT self;/* Formal argument: self */
 TP_LAYOUT ret_val2;
 FSETTP_LAYOUT L61;
 TP_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETTP_LAYOUT is_elt_nil_self;
 TP_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_LAYOUT is_elt_nil_self1;
 TP_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTP1672834608_frame;

typedef struct FSETTUPIDENTINT_struct {/* layout for FSET{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FSETTUPIDENTINT;

typedef struct FSETTU507892683_frame_struct {
 INT state;
 FSETTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTU507892683_frame;

typedef struct FSETTU213952071_frame_struct {
 INT state;
 FSETTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETTU213952071_frame;

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

#include "tags.h"

/* Globals */

extern BOOL FSETIM1693735320;
extern BOOL FSETIM742607465;
extern BOOL FSETSF1336677965;
extern BOOL FSETSF521946546;
extern BOOL FSETSI1125125409;
extern BOOL FSETSI1311217376;
extern BOOL FSETSI2024459664;
extern BOOL FSETSI411883121;
extern BOOL FSETST1011828476;
extern BOOL FSETST846796035;
extern BOOL FSETTP138222400;
extern BOOL FSETTP1720402111;
extern BOOL FSETTU1862000339;
extern BOOL FSETTU574342446;
extern INT FSETIM2015417903;
extern INT FSETIM924207918;
extern INT FSETSF1238115389;
extern INT FSETSF1603235078;
extern INT FSETSI1189203070;
extern INT FSETSI1350248406;
extern INT FSETSI1658388805;
extern INT FSETSI232994243;
extern INT FSETST1746314450;
extern INT FSETST854048553;
extern INT FSETTP1014013453;
extern INT FSETTP752200044;
extern INT FSETTU561674193;
extern INT FSETTU939302060;

/* Function declarations */

BOOL FSETSF406940155(FSETSFILE_ID, SFILE_ID);
BOOL FSETSF468746162(FSETSFILE_ID, SFILE_ID);
BOOL FSETSF598747399(FSETSFILE_ID, SFILE_ID);
BOOL FSETSI1729190431(FSETSIG, SIG);
BOOL FSETSI488259882(FSETSIG);
BOOL FSETSI675594706(FSETSIG, SIG);
BOOL FSETSI722431140(FSETSIG, SIG);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL FSETST1951807602(FSETSTR);
BOOL FSETST453061519(FSETSTR, STR);
BOOL FSETST770335290(FSETSTR, STR);
BOOL FSETTP1285909750(FSETTP_CLASS, TP_CLASS);
BOOL FSETTP1673810127(FSETTP_CLASS, TP_CLASS);
BOOL FSETTP571570007(FSETTP_CLASS, TP_CLASS);
BOOL FSETTU1015522869(FSETTUPIDENTINT, TUPIDENTINT);
BOOL FSETTU556789752(FSETTUPIDENTINT, TUPIDENTINT);
BOOL FSETTU645397512(FSETTUPIDENTINT, TUPIDENTINT);
BOOL FSETTU719980391(FSETTUPIDENTINT);
BOOL SIDE_E526657440(SIDE_EFFECT, SIDE_EFFECT);
BOOL STR_is111530248(STR, STR);
BOOL TUPIDE1439069590(TUPIDENTINT, TUPIDENTINT);
FSETIM184436961 FSETIM1042743561(FSETIM184436961);
FSETIM184436961 FSETIM1218054210(FSETIM184436961, FSETIM184436961, IMMUTA421759893);
FSETIM184436961 FSETIM1706711326(FSETIM184436961, IMMUTA421759893);
FSETIM184436961 FSETIM195195621(FSETIM184436961, INT);
FSETIM184436961 FSETIM420846274(FSETIM184436961, FSETIM184436961, IMMUTA421759893);
FSETIM184436961 FSETIM94937506(FSETIM184436961);
FSETSFILE_ID FSETSF1416244118(FSETSFILE_ID, FSETSFILE_ID, SFILE_ID);
FSETSFILE_ID FSETSF1583575786(FSETSFILE_ID, FSETSFILE_ID, SFILE_ID);
FSETSFILE_ID FSETSF2018407377(FSETSFILE_ID, INT);
FSETSFILE_ID FSETSF2021002472(FSETSFILE_ID);
FSETSFILE_ID FSETSF547386475(FSETSFILE_ID);
FSETSFILE_ID FSETSF663191272(FSETSFILE_ID, SFILE_ID);
FSETSIDE_EFFECT FSETSI128083460(FSETSIDE_EFFECT, FSETSIDE_EFFECT, SIDE_EFFECT);
FSETSIDE_EFFECT FSETSI1549225340(FSETSIDE_EFFECT, FSETSIDE_EFFECT, SIDE_EFFECT);
FSETSIDE_EFFECT FSETSI1985067196(FSETSIDE_EFFECT);
FSETSIDE_EFFECT FSETSI313951960(FSETSIDE_EFFECT, INT);
FSETSIDE_EFFECT FSETSI770361133(FSETSIDE_EFFECT);
FSETSIDE_EFFECT FSETSI784163459(FSETSIDE_EFFECT, SIDE_EFFECT);
FSETSIG FSETSI1119826300(FSETSIG, FSETSIG, SIG);
FSETSIG FSETSI112281076(FSETSIG, SIG);
FSETSIG FSETSI123770722(FSETSIG, FSETSIG);
FSETSIG FSETSI1299465556(FSETSIG);
FSETSIG FSETSI217755790(FSETSIG, SIG);
FSETSIG FSETSI325078524(FSETSIG, FSETSIG, SIG);
FSETSIG FSETSI346777215(FSETSIG, SIG);
FSETSIG FSETSI461354464(FSETSIG);
FSETSIG FSETSI548941202(FSETSIG, FSETSIG);
FSETSIG FSETSI587106640(FSETSIG, INT);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSIG FSETSI719979460(FSETSIG);
FSETSIG FSETSI833456188(FSETSIG);
FSETSIG FSETSI917491731(FSETSIG);
FSETSTR FSETST1229526412(FSETSTR);
FSETSTR FSETST1242756882(FSETSTR, STR);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETSTR FSETST1608657812(FSETSTR, FSETSTR, STR);
FSETSTR FSETST1872370245(FSETSTR, STR);
FSETSTR FSETST1884046860(FSETSTR);
FSETSTR FSETST2089025776(FSETSTR, STR);
FSETSTR FSETST34137851(FSETSTR);
FSETSTR FSETST555430076(FSETSTR);
FSETSTR FSETST813910036(FSETSTR, FSETSTR, STR);
FSETSTR FSETST866022026(FSETSTR, FSETSTR);
FSETSTR FSETST897681893(FSETSTR, FSETSTR);
FSETSTR FSETST983176864(FSETSTR, INT);
FSETTP_CLASS FSETTP1145293274(FSETTP_CLASS, FSETTP_CLASS, TP_CLASS);
FSETTP_CLASS FSETTP1186813482(FSETTP_CLASS, TP_CLASS);
FSETTP_CLASS FSETTP149854118(FSETTP_CLASS, FSETTP_CLASS, TP_CLASS);
FSETTP_CLASS FSETTP1786600189(FSETTP_CLASS, INT);
FSETTP_CLASS FSETTP1983095296(FSETTP_CLASS);
FSETTP_CLASS FSETTP633280235(FSETTP_CLASS, FSETTP_CLASS);
FSETTP_CLASS FSETTP779193663(FSETTP_CLASS);
FSETTUPIDENTINT FSETTU134672224(FSETTUPIDENTINT, FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU1542636576(FSETTUPIDENTINT, FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU1559268184(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU1996664292(FSETTUPIDENTINT, INT);
FSETTUPIDENTINT FSETTU2045052674(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU302354864(FSETTUPIDENTINT);
FSETTUPIDENTINT FSETTU419377758(FSETTUPIDENTINT);
FSETTUPIDENTINT FSETTU425674983(FSETTUPIDENTINT);
FSETTUPIDENTINT FSETTU445885760(FSETTUPIDENTINT);
FSETTUPIDENTINT FSETTU475443387(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU725727349(FSETTUPIDENTINT, TUPIDENTINT);
IMMUTA421759893 FSETIM205083650(FSETIM205083650_frame);
IMMUTA421759893 FSETIM887960645(FSETIM887960645_frame);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FSETSI2058428832(FSETSIDE_EFFECT);
INT FSETSIG_sizerINT(FSETSIG);
INT FSETSTR_sizerINT(FSETSTR);
SFILE_ID FSETSF12648100(FSETSF12648100_frame);
SFILE_ID FSETSF139710295(FSETSF139710295_frame);
SIDE_EFFECT FSETSI1267678819(FSETSI1267678819_frame);
SIDE_EFFECT FSETSI1316299458(FSETSIDE_EFFECT, SIDE_EFFECT);
SIDE_EFFECT FSETSI2035884295(FSETSIDE_EFFECT, SIDE_EFFECT);
SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame);
SIDE_EFFECT FSETSI937439554(FSETSIDE_EFFECT, SIDE_EFFECT);
SIG FSETSI2020712453(FSETSIG);
SIG FSETSI2112123043(FSETSI2112123043_frame);
SIG FSETSIG_eltbrSIG(FSETSIG_eltbrSIG_frame);
STR ELT_AL1717624110(ELT_ALGSTR, dELTSTR);
STR FSETST261377883(FSETST261377883_frame);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR FSETSTR_strrSTR(FSETSTR);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TP_CLASS FSETTP49057756(FSETTP49057756_frame);
TP_LAYOUT FSETTP1672834608(FSETTP1672834608_frame);
TP_LAYOUT FSETTP381129783(FSETTP381129783_frame);
TUPIDENTINT FSETTU507892683(FSETTU507892683_frame);
TUPIDENTINT FSETTU947212086(FSETTU947212086_frame);
void FSETIM1858750716(FSETIM1858750716_frame);
void FSETSF1565053082(FSETSF1565053082_frame);
void FSETSI1764914571(FSETSI1764914571_frame);
void FSETSI1989523573(FSETSI1989523573_frame);
void FSETST679307565(FSETST679307565_frame);
void FSETTP1601462738(FSETTP1601462738_frame);
void FSETTU213952071(FSETTU213952071_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL FSETSF406940155(FSETSFILE_ID self, SFILE_ID e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETSFILE_ID) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETSF468746162(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETSF598747399(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETSF468746162(FSETSFILE_ID self, SFILE_ID e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 SFILE_ID te = SFILE_ID_zero;
 SFILE_ID te1 = SFILE_ID_zero;
 FSETSFILE_ID elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSFILE_ID is_elt_nil_self;
 SFILE_ID is_elt_nil_e = SFILE_ID_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSFILE_ID is_elt_nil_self1;
 SFILE_ID is_elt_nil_e1 = SFILE_ID_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETSFILE_ID elt_eq_self;
 SFILE_ID elt_eq_e1 = SFILE_ID_zero;
 SFILE_ID elt_eq_e2 = SFILE_ID_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self1;
 SFILE_ID elt_eq_e11 = SFILE_ID_zero;
 SFILE_ID elt_eq_e21 = SFILE_ID_zero;
 BOOL ret_val5 = BOOL_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val6 = BOOL_zero;
 FSETSFILE_ID is_elt_nil_self2;
 SFILE_ID is_elt_nil_e2 = SFILE_ID_zero;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSFILE_ID is_elt_nil_self3;
 SFILE_ID is_elt_nil_e3 = SFILE_ID_zero;
 BOOL ret_val8 = BOOL_zero;
 FSETSFILE_ID elt_eq_self2;
 SFILE_ID elt_eq_e12 = SFILE_ID_zero;
 SFILE_ID elt_eq_e22 = SFILE_ID_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self3;
 SFILE_ID elt_eq_e13 = SFILE_ID_zero;
 SFILE_ID elt_eq_e23 = SFILE_ID_zero;
 BOOL ret_val10 = BOOL_zero;
 SFILE_ID is_eq_self1 = SFILE_ID_zero;
 SFILE_ID is_eq_y1 = SFILE_ID_zero;
 BOOL ret_val11 = BOOL_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SFILE_ID L91_;
 INT L10;
 INT L11;
 BOOL L121_;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 BOOL L181_;
 SFILE_ID L191_;
 INT L20;
 INT L21;
 BOOL L221_;
 INT L231_;
 elt_hash_self = self;
 L2=ZALLOC_LEAF(sizeof(struct SFILE_ID_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct SFILE_ID_boxed_struct));
 ((OB)L2)->header.tag=SFILE_ID_tag;
 L1 = (OB)((SFILE_ID_boxed) L2);
 ((SFILE_ID_boxed) L1)->immutable_part = e;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 L41_=ASIZE((FSETSFILE_ID)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FSETSFILE_ID)self,h); 
  te = L91_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSFILE_ID) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = INT_IS_VOID(is_elt_nil_e1.loc);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSFILE_ID) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_y = elt_eq_e21;
   L10 = is_eq_self.loc;
   L11 = is_eq_y.loc;
   L121_=(L10)==(L11); 
   ret_val6 = L121_;
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L131_=INTPLUS(h,1); 
  h = L131_;
 }
 after_loop: ;
 L141_=ASIZE((FSETSFILE_ID)self); 
 L15 = L141_;
 L161_=INTMINUS(L15,1); 
 L17 = L161_;
 L181_=(h)==(L17); 
 if (L181_) {
  h = 0;
  while (1) {
   L191_=ARR((FSETSFILE_ID)self,h); 
   te1 = L191_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSFILE_ID) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = INT_IS_VOID(is_elt_nil_e3.loc);
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQSFILE_ID) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_y1 = elt_eq_e23;
    L20 = is_eq_self1.loc;
    L21 = is_eq_y1.loc;
    L221_=(L20)==(L21); 
    ret_val11 = L221_;
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L231_=INTPLUS(h,1); 
   h = L231_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETSF598747399(FSETSFILE_ID self, SFILE_ID e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSFILE_ID elt_eq_self;
 SFILE_ID elt_eq_e1 = SFILE_ID_zero;
 SFILE_ID elt_eq_e2 = SFILE_ID_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self1;
 SFILE_ID elt_eq_e11 = SFILE_ID_zero;
 SFILE_ID elt_eq_e21 = SFILE_ID_zero;
 BOOL ret_val2 = BOOL_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val3 = BOOL_zero;
 BOOL L11_;
 SFILE_ID L21_;
 INT L3;
 INT L4;
 BOOL L51_;
 INT L61_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=ARR((FSETSFILE_ID)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSFILE_ID) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_y = elt_eq_e21;
  L3 = is_eq_self.loc;
  L4 = is_eq_y.loc;
  L51_=(L3)==(L4); 
  ret_val3 = L51_;
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L61_=INTPLUS(i,1); 
  i = L61_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETSI1729190431(FSETSIG self, SIG e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 SIG te;
 SIG te1;
 FSETSIG elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FSETSIG is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETSIG elt_eq_self2;
 SIG elt_eq_e12;
 SIG elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self3;
 SIG elt_eq_e13;
 SIG elt_eq_e23;
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
 SIG L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 SIG L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(SIG)ARR((FSETSIG)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSIG) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
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
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETSIG)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(SIG)ARR((FSETSIG)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSIG) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
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
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETSI488259882(FSETSIG self) {
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
 L21_=ASIZE((FSETSIG)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETSIG)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETSI1658388805;
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

BOOL FSETSI675594706(FSETSIG self, SIG e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSIG elt_eq_self;
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
 BOOL L11_;
 SIG L21_;
 INT L31_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(SIG)ARR((FSETSIG)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSIG) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_s = elt_eq_e21;
  ret_val3 = (is_eq_self==is_eq_s);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L31_=INTPLUS(i,1); 
  i = L31_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETSI722431140(FSETSIG self, SIG e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETSIG) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETSI1729190431(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETSI675594706(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETST1025458804(FSETSTR self, STR e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETSTR) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETST453061519(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETST770335290(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETST1951807602(FSETSTR self) {
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
 L21_=ASIZE((FSETSTR)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETSTR)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETST854048553;
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

BOOL FSETST453061519(FSETSTR self, STR e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 STR te;
 STR te1;
 FSETSTR elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETSTR is_elt_nil_self2;
 STR is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self3;
 STR is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETSTR elt_eq_self2;
 STR elt_eq_e12;
 STR elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self3;
 STR elt_eq_e13;
 STR elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 STR L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 STR L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(STR)ARR((FSETSTR)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSTR) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETSTR)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(STR)ARR((FSETSTR)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSTR) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQSTR) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = STR_is111530248(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETST770335290(FSETSTR self, STR e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 STR L21_;
 INT L31_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(STR)ARR((FSETSTR)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSTR) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L31_=INTPLUS(i,1); 
  i = L31_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTP1285909750(FSETTP_CLASS self, TP_CLASS e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 TP_CLASS te;
 TP_CLASS te1;
 FSETTP_CLASS elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val6 = BOOL_zero;
 FSETTP_CLASS is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETTP_CLASS elt_eq_self2;
 TP_CLASS elt_eq_e12;
 TP_CLASS elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self3;
 TP_CLASS elt_eq_e13;
 TP_CLASS elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_CLASS L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_CLASS L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETTP_CLASS)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_CLASS)ARR((FSETTP_CLASS)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((TP_CLASS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETTP_CLASS)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_CLASS)ARR((FSETTP_CLASS)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((TP_CLASS) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTP1673810127(FSETTP_CLASS self, TP_CLASS e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETTP_CLASS elt_eq_self;
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
 BOOL L11_;
 TP_CLASS L21_;
 INT L31_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(TP_CLASS)ARR((FSETTP_CLASS)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_t = ((OB) elt_eq_e21);
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L31_=INTPLUS(i,1); 
  i = L31_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTP571570007(FSETTP_CLASS self, TP_CLASS e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETTP_CLASS) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETTP1285909750(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETTP1673810127(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTU1015522869(FSETTUPIDENTINT self, TUPIDENTINT e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 TUPIDENTINT te = TUPIDENTINT_zero;
 TUPIDENTINT te1 = TUPIDENTINT_zero;
 FSETTUPIDENTINT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTUPIDENTINT is_elt_nil_self;
 TUPIDENTINT is_elt_nil_e = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self1;
 TUPIDENTINT is_elt_nil_e1 = TUPIDENTINT_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val5 = BOOL_zero;
 FSETTUPIDENTINT is_elt_nil_self2;
 TUPIDENTINT is_elt_nil_e2 = TUPIDENTINT_zero;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self3;
 TUPIDENTINT is_elt_nil_e3 = TUPIDENTINT_zero;
 BOOL ret_val7 = BOOL_zero;
 FSETTUPIDENTINT elt_eq_self2;
 TUPIDENTINT elt_eq_e12 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e22 = TUPIDENTINT_zero;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self3;
 TUPIDENTINT elt_eq_e13 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e23 = TUPIDENTINT_zero;
 BOOL ret_val9 = BOOL_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTINT L91_;
 INT L101_;
 INT L111_;
 INT L12;
 INT L131_;
 INT L14;
 BOOL L151_;
 TUPIDENTINT L161_;
 INT L171_;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=TUPIDENTINT_tag;
 L1 = (OB)((TUPIDENTINT_boxed) L2);
 ((TUPIDENTINT_boxed) L1)->immutable_part = e;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 L41_=ASIZE((FSETTUPIDENTINT)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FSETTUPIDENTINT)self,h); 
  te = L91_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1575801776) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e1.t2);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1475848140) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L101_=INTPLUS(h,1); 
  h = L101_;
 }
 after_loop: ;
 L111_=ASIZE((FSETTUPIDENTINT)self); 
 L12 = L111_;
 L131_=INTMINUS(L12,1); 
 L14 = L131_;
 L151_=(h)==(L14); 
 if (L151_) {
  h = 0;
  while (1) {
   L161_=ARR((FSETTUPIDENTINT)self,h); 
   te1 = L161_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1575801776) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e3.t2);
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1475848140) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = TUPIDE1439069590(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L171_=INTPLUS(h,1); 
   h = L171_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTU556789752(FSETTUPIDENTINT self, TUPIDENTINT e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 TUPIDENTINT L21_;
 INT L31_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=ARR((FSETTUPIDENTINT)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1475848140) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   ret_val = TRUE;
   return ret_val;
  }
  L31_=INTPLUS(i,1); 
  i = L31_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTU645397512(FSETTUPIDENTINT self, TUPIDENTINT e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETTUPIDENTINT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETTU1015522869(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETTU556789752(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETTU719980391(FSETTUPIDENTINT self) {
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
 L21_=ASIZE((FSETTUPIDENTINT)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETTUPIDENTINT)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETTU939302060;
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

FSETIM184436961 FSETIM1042743561(FSETIM184436961 self) {
 FSETIM184436961 ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETIM184436961)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETIM2015417903;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETIM94937506(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETIM184436961)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETIM94937506(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETIM184436961 FSETIM1218054210(FSETIM184436961 self, FSETIM184436961 r, IMMUTA421759893 e) {
 FSETIM184436961 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 IMMUTA421759893 te;
 IMMUTA421759893 te1;
 FSETIM184436961 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETIM184436961 is_elt_nil_self;
 IMMUTA421759893 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1394471793 is_elt_nil_self1;
 IMMUTA421759893 is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETIM184436961 elt_eq_self;
 IMMUTA421759893 elt_eq_e1;
 IMMUTA421759893 elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ20798926 elt_eq_self1;
 IMMUTA421759893 elt_eq_e11;
 IMMUTA421759893 elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETIM184436961 is_elt_nil_self2;
 IMMUTA421759893 is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1394471793 is_elt_nil_self3;
 IMMUTA421759893 is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETIM184436961 elt_eq_self2;
 IMMUTA421759893 elt_eq_e12;
 IMMUTA421759893 elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ20798926 elt_eq_self3;
 IMMUTA421759893 elt_eq_e13;
 IMMUTA421759893 elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 IMMUTA421759893 L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 IMMUTA421759893 L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETIM184436961)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(IMMUTA421759893)ARR((FSETIM184436961)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1394471793) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((IMMUTA421759893) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ20798926) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETIM184436961)r,h,(IMMUTA421759893)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(IMMUTA421759893)ARR((FSETIM184436961)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1394471793) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((IMMUTA421759893) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ20798926) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETIM184436961)r,h,(IMMUTA421759893)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETIM184436961)r,h,(IMMUTA421759893)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETIM184436961 FSETIM1706711326(FSETIM184436961 self, IMMUTA421759893 e) {
 FSETIM184436961 ret_val;
 FSETIM184436961 r;
 FSETIM184436961 set_initial_stru;
 r = self;
 if ((r==((FSETIM184436961) NULL))) {
  r = FSETIM195195621(self, FSETIM924207918);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETIM742607465);
 }
 else {
  r = FSETIM1042743561(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETIM1218054210(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETIM420846274(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETIM184436961 FSETIM195195621(FSETIM184436961 self, INT n) {
 FSETIM184436961 ret_val;
 FSETIM184436961 r;
 FSETIM184436961 L11;
 FSETIM184436961 elt_nil_self;
 IMMUTA421759893 ret_val1;
 ELT_NI1394471793 elt_nil_self1;
 IMMUTA421759893 ret_val2;
 IMMUTA421759893 e = ((IMMUTA421759893) NULL);
 FSETIM184436961 elt_nil_self2;
 IMMUTA421759893 ret_val3;
 ELT_NI1394471793 elt_nil_self3;
 IMMUTA421759893 ret_val4;
 IMMUTA421759893 e1 = ((IMMUTA421759893) NULL);
 INT L21 = INT_zero;
 FSETIM184436961 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 IMMUTA421759893 L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETIM184436961_struct)-sizeof(IMMUTA421759893))+(L4)*sizeof(IMMUTA421759893);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETIM184436961_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETIM184436961) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1394471793) NULL);
 ret_val2 = ((IMMUTA421759893) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((IMMUTA421759893) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETIM184436961)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1394471793) NULL);
    ret_val4 = ((IMMUTA421759893) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETIM184436961)L11,L21,(IMMUTA421759893)L10); 
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

FSETIM184436961 FSETIM420846274(FSETIM184436961 self, FSETIM184436961 r, IMMUTA421759893 e) {
 FSETIM184436961 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETIM184436961 elt_eq_self;
 IMMUTA421759893 elt_eq_e1;
 IMMUTA421759893 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ20798926 elt_eq_self1;
 IMMUTA421759893 elt_eq_e11;
 IMMUTA421759893 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 IMMUTA421759893 L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(IMMUTA421759893)ARR((FSETIM184436961)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ20798926) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETIM184436961)r,i,(IMMUTA421759893)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETIM184436961)r,L5,(IMMUTA421759893)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETIM184436961 FSETIM94937506(FSETIM184436961 self) {
 FSETIM184436961 ret_val;
 FSETIM184436961 r;
 FSETIM184436961 L11;
 FSETIM184436961 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 IMMUTA421759893 L9;
 L21_=ASIZE((FSETIM184436961)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETIM195195621(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETIM1693735320) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETIM887960645_frame_struct other1_0;
FSETIM887960645_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETIM887960645(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETIM1706711326(r, L9);
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

FSETSFILE_ID FSETSF1416244118(FSETSFILE_ID self, FSETSFILE_ID r, SFILE_ID e) {
 FSETSFILE_ID ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSFILE_ID elt_eq_self;
 SFILE_ID elt_eq_e1 = SFILE_ID_zero;
 SFILE_ID elt_eq_e2 = SFILE_ID_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self1;
 SFILE_ID elt_eq_e11 = SFILE_ID_zero;
 SFILE_ID elt_eq_e21 = SFILE_ID_zero;
 BOOL ret_val2 = BOOL_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val3 = BOOL_zero;
 BOOL L11_;
 SFILE_ID L21_;
 INT L3;
 INT L4;
 BOOL L51_;
 INT L71_;
 INT L8;
 INT L10;
 INT L121_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=ARR((FSETSFILE_ID)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSFILE_ID) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_y = elt_eq_e21;
  L3 = is_eq_self.loc;
  L4 = is_eq_y.loc;
  L51_=(L3)==(L4); 
  ret_val3 = L51_;
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETSFILE_ID)r,i,e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L71_=INTPLUS(i,1); 
  i = L71_;
 }
 after_loop: ;
 L8 = ATTR(r,hsize);
 SARR((FSETSFILE_ID)r,L8,e); 
 ;
 L10 = ATTR(r,hsize);
 L121_=INTPLUS(L10,1); 
 SATTR(r,hsize,L121_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSFILE_ID FSETSF1583575786(FSETSFILE_ID self, FSETSFILE_ID r, SFILE_ID e) {
 FSETSFILE_ID ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 SFILE_ID te = SFILE_ID_zero;
 SFILE_ID te1 = SFILE_ID_zero;
 FSETSFILE_ID elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSFILE_ID is_elt_nil_self;
 SFILE_ID is_elt_nil_e = SFILE_ID_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSFILE_ID is_elt_nil_self1;
 SFILE_ID is_elt_nil_e1 = SFILE_ID_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETSFILE_ID elt_eq_self;
 SFILE_ID elt_eq_e1 = SFILE_ID_zero;
 SFILE_ID elt_eq_e2 = SFILE_ID_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self1;
 SFILE_ID elt_eq_e11 = SFILE_ID_zero;
 SFILE_ID elt_eq_e21 = SFILE_ID_zero;
 BOOL ret_val5 = BOOL_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val6 = BOOL_zero;
 FSETSFILE_ID is_elt_nil_self2;
 SFILE_ID is_elt_nil_e2 = SFILE_ID_zero;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSFILE_ID is_elt_nil_self3;
 SFILE_ID is_elt_nil_e3 = SFILE_ID_zero;
 BOOL ret_val8 = BOOL_zero;
 FSETSFILE_ID elt_eq_self2;
 SFILE_ID elt_eq_e12 = SFILE_ID_zero;
 SFILE_ID elt_eq_e22 = SFILE_ID_zero;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSFILE_ID elt_eq_self3;
 SFILE_ID elt_eq_e13 = SFILE_ID_zero;
 SFILE_ID elt_eq_e23 = SFILE_ID_zero;
 BOOL ret_val10 = BOOL_zero;
 SFILE_ID is_eq_self1 = SFILE_ID_zero;
 SFILE_ID is_eq_y1 = SFILE_ID_zero;
 BOOL ret_val11 = BOOL_zero;
 INT L11_;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 SFILE_ID L81_;
 INT L9;
 INT L10;
 BOOL L121_;
 INT L141_;
 INT L151_;
 INT L16;
 BOOL L171_;
 SFILE_ID L181_;
 INT L19;
 INT L20;
 BOOL L211_;
 INT L231_;
 INT L25;
 INT L261_;
 L11_=ASIZE((FSETSFILE_ID)r); 
 asz = L11_;
 elt_hash_self = r;
 L3=ZALLOC_LEAF(sizeof(struct SFILE_ID_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,sizeof(struct SFILE_ID_boxed_struct));
 ((OB)L3)->header.tag=SFILE_ID_tag;
 L2 = (OB)((SFILE_ID_boxed) L3);
 ((SFILE_ID_boxed) L2)->immutable_part = e;
 elt_hash_e = L2;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L4 = ret_val1;
 L51_=INTMINUS(asz,2); 
 L6 = L51_;
 L71_=L4&L6; 
 orig_h = L71_;
 h = orig_h;
 while (1) {
  L81_=ARR((FSETSFILE_ID)r,h); 
  te = L81_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSFILE_ID) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = INT_IS_VOID(is_elt_nil_e1.loc);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSFILE_ID) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_y = elt_eq_e21;
   L9 = is_eq_self.loc;
   L10 = is_eq_y.loc;
   L121_=(L9)==(L10); 
   ret_val6 = L121_;
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETSFILE_ID)r,h,e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L141_=INTPLUS(h,1); 
  h = L141_;
 }
 after_loop: ;
 L151_=INTMINUS(asz,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FSETSFILE_ID)r,h); 
   te1 = L181_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSFILE_ID) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = INT_IS_VOID(is_elt_nil_e3.loc);
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQSFILE_ID) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_y1 = elt_eq_e23;
    L19 = is_eq_self1.loc;
    L20 = is_eq_y1.loc;
    L211_=(L19)==(L20); 
    ret_val11 = L211_;
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     SARR((FSETSFILE_ID)r,h,e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L231_=INTPLUS(h,1); 
   h = L231_;
  }
  after_loop1: ;
 }
 SARR((FSETSFILE_ID)r,h,e); 
 ;
 L25 = ATTR(r,hsize);
 L261_=INTPLUS(L25,1); 
 SATTR(r,hsize,L261_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSFILE_ID FSETSF2018407377(FSETSFILE_ID self, INT n) {
 FSETSFILE_ID ret_val;
 FSETSFILE_ID r;
 FSETSFILE_ID L11;
 FSETSFILE_ID elt_nil_self;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 ELT_NILSFILE_ID elt_nil_self1;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 SFILE_ID e = SFILE_ID_zero;
 FSETSFILE_ID elt_nil_self2;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 ELT_NILSFILE_ID elt_nil_self3;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 SFILE_ID e1 = SFILE_ID_zero;
 INT L21 = INT_zero;
 FSETSFILE_ID L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 SFILE_ID L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETSFILE_ID_struct)+1-sizeof(SFILE_ID))+(L4)*sizeof(SFILE_ID);
 L5=ZALLOC_LEAF_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,L6);
 ((OB)L5)->header.tag=FSETSFILE_ID_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETSFILE_ID) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSFILE_ID) NULL);
 ret_val2 = SFILE_ID_zero;
 ret_val1 = ret_val2;
 L7 = INT_IS_VOID(ret_val1.loc);
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETSFILE_ID)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSFILE_ID) NULL);
    ret_val4 = SFILE_ID_zero;
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETSFILE_ID)L11,L21,L10); 
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

FSETSFILE_ID FSETSF2021002472(FSETSFILE_ID self) {
 FSETSFILE_ID ret_val;
 FSETSFILE_ID r;
 FSETSFILE_ID L11;
 FSETSFILE_ID switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SFILE_ID L9;
 L21_=ASIZE((FSETSFILE_ID)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETSF2018407377(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETSF521946546) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSF139710295_frame_struct other1_0;
FSETSF139710295_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSF139710295(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSF663191272(r, L9);
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

FSETSFILE_ID FSETSF547386475(FSETSFILE_ID self) {
 FSETSFILE_ID ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETSFILE_ID)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETSF1238115389;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETSF2021002472(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETSFILE_ID)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETSF2021002472(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSFILE_ID FSETSF663191272(FSETSFILE_ID self, SFILE_ID e) {
 FSETSFILE_ID ret_val;
 FSETSFILE_ID r;
 FSETSFILE_ID set_initial_stru;
 r = self;
 if ((r==((FSETSFILE_ID) NULL))) {
  r = FSETSF2018407377(self, FSETSF1603235078);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETSF1336677965);
 }
 else {
  r = FSETSF547386475(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETSF1583575786(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETSF1416244118(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIDE_EFFECT FSETSI128083460(FSETSIDE_EFFECT self, FSETSIDE_EFFECT r, SIDE_EFFECT e) {
 FSETSIDE_EFFECT ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSIDE_EFFECT elt_eq_self;
 SIDE_EFFECT elt_eq_e1;
 SIDE_EFFECT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self1;
 SIDE_EFFECT elt_eq_e11;
 SIDE_EFFECT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 SIDE_EFFECT L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ251059601) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = SIDE_E526657440(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETSIDE_EFFECT)r,i,(SIDE_EFFECT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETSIDE_EFFECT)r,L5,(SIDE_EFFECT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIDE_EFFECT FSETSI1549225340(FSETSIDE_EFFECT self, FSETSIDE_EFFECT r, SIDE_EFFECT e) {
 FSETSIDE_EFFECT ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 SIDE_EFFECT te;
 SIDE_EFFECT te1;
 FSETSIDE_EFFECT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSIDE_EFFECT elt_eq_self;
 SIDE_EFFECT elt_eq_e1;
 SIDE_EFFECT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self1;
 SIDE_EFFECT elt_eq_e11;
 SIDE_EFFECT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETSIDE_EFFECT is_elt_nil_self2;
 SIDE_EFFECT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI351013237 is_elt_nil_self3;
 SIDE_EFFECT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETSIDE_EFFECT elt_eq_self2;
 SIDE_EFFECT elt_eq_e12;
 SIDE_EFFECT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self3;
 SIDE_EFFECT elt_eq_e13;
 SIDE_EFFECT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 SIDE_EFFECT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 SIDE_EFFECT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETSIDE_EFFECT)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI351013237) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((SIDE_EFFECT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ251059601) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = SIDE_E526657440(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETSIDE_EFFECT)r,h,(SIDE_EFFECT)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI351013237) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((SIDE_EFFECT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ251059601) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = SIDE_E526657440(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETSIDE_EFFECT)r,h,(SIDE_EFFECT)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETSIDE_EFFECT)r,h,(SIDE_EFFECT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIDE_EFFECT FSETSI1985067196(FSETSIDE_EFFECT self) {
 FSETSIDE_EFFECT ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETSIDE_EFFECT)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETSI232994243;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETSI770361133(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETSIDE_EFFECT)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETSI770361133(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIDE_EFFECT FSETSI313951960(FSETSIDE_EFFECT self, INT n) {
 FSETSIDE_EFFECT ret_val;
 FSETSIDE_EFFECT r;
 FSETSIDE_EFFECT L11;
 FSETSIDE_EFFECT elt_nil_self;
 SIDE_EFFECT ret_val1;
 ELT_NI351013237 elt_nil_self1;
 SIDE_EFFECT ret_val2;
 SIDE_EFFECT e = ((SIDE_EFFECT) NULL);
 FSETSIDE_EFFECT elt_nil_self2;
 SIDE_EFFECT ret_val3;
 ELT_NI351013237 elt_nil_self3;
 SIDE_EFFECT ret_val4;
 SIDE_EFFECT e1 = ((SIDE_EFFECT) NULL);
 INT L21 = INT_zero;
 FSETSIDE_EFFECT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 SIDE_EFFECT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETSIDE_EFFECT_struct)-sizeof(SIDE_EFFECT))+(L4)*sizeof(SIDE_EFFECT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETSIDE_EFFECT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETSIDE_EFFECT) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI351013237) NULL);
 ret_val2 = ((SIDE_EFFECT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((SIDE_EFFECT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETSIDE_EFFECT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI351013237) NULL);
    ret_val4 = ((SIDE_EFFECT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETSIDE_EFFECT)L11,L21,(SIDE_EFFECT)L10); 
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

FSETSIDE_EFFECT FSETSI770361133(FSETSIDE_EFFECT self) {
 FSETSIDE_EFFECT ret_val;
 FSETSIDE_EFFECT r;
 FSETSIDE_EFFECT L11;
 FSETSIDE_EFFECT switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SIDE_EFFECT L9;
 L21_=ASIZE((FSETSIDE_EFFECT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETSI313951960(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETSI1125125409) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSI2093978094_frame_struct other1_0;
FSETSI2093978094_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSI2093978094(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSI784163459(r, L9);
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

FSETSIDE_EFFECT FSETSI784163459(FSETSIDE_EFFECT self, SIDE_EFFECT e) {
 FSETSIDE_EFFECT ret_val;
 FSETSIDE_EFFECT r;
 FSETSIDE_EFFECT set_initial_stru;
 r = self;
 if ((r==((FSETSIDE_EFFECT) NULL))) {
  r = FSETSI313951960(self, FSETSI1189203070);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETSI1311217376);
 }
 else {
  r = FSETSI1985067196(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETSI1549225340(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETSI128083460(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI1119826300(FSETSIG self, FSETSIG r, SIG e) {
 FSETSIG ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 SIG te;
 SIG te1;
 FSETSIG elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FSETSIG is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETSIG elt_eq_self2;
 SIG elt_eq_e12;
 SIG elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQSIG elt_eq_self3;
 SIG elt_eq_e13;
 SIG elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 SIG is_eq_self1;
 SIG is_eq_s1;
 BOOL ret_val11 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 SIG L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 SIG L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETSIG)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(SIG)ARR((FSETSIG)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSIG) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
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
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETSIG)r,h,(SIG)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(SIG)ARR((FSETSIG)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSIG) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((SIG) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
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
    ret_val11 = (is_eq_self1==is_eq_s1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     SARR((FSETSIG)r,h,(SIG)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETSIG)r,h,(SIG)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI112281076(FSETSIG self, SIG e) {
 FSETSIG ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETSIG elt_eq_self;
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
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 SIG L41_;
 INT L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L13;
 INT L141_;
 BOOL L151_;
 INT L161_;
 SIG L171_;
 SIG L18;
 INT L20;
 INT L211_;
 delete_elt_ind = -1;
 hash_table_size = ATTR(self,hsize);
 i = 0;
 while (1) {
  L11_=(i)<(hash_table_size); 
  L2 = L11_;
  L31_=!(L2); 
  if (L31_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L41_=(SIG)ARR((FSETSIG)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQSIG) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_s = elt_eq_e21;
  ret_val3 = (is_eq_self==is_eq_s);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L71_=(delete_elt_ind)<(0); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,hsize);
  L121_=(delete_elt_ind)<(L10); 
  L6 = L121_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  empty_loc = delete_elt_ind;
  L13 = ATTR(self,hsize);
  L141_=INTMINUS(L13,2); 
  second_to_last_i = L141_;
  while (1) {
   L151_=(second_to_last_i)<(empty_loc); 
   if (L151_) {
    goto after_loop1;
   }
   L161_=INTPLUS(empty_loc,1); 
   next = L161_;
   L171_=(SIG)ARR((FSETSIG)self,next); 
   L18 = L171_;
   SARR((FSETSIG)self,empty_loc,(SIG)L18); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L20 = ATTR(self,hsize);
  L211_=INTMINUS(L20,1); 
  SATTR(self,hsize,L211_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI123770722(FSETSIG self, FSETSIG s) {
 FSETSIG ret_val;
 FSETSIG r = ((FSETSIG) NULL);
 FSETSIG L11;
 SIG e;
 SIG L2;
 {
  struct FSETSIG_eltbrSIG_frame_struct other1_0;
FSETSIG_eltbrSIG_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSIG_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L2;
   if (FSETSI722431140(s, e)) {
    r = FSETSI671175003(r, e);
   }
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI1299465556(FSETSIG self) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG L11;
 FSETSIG switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SIG L9;
 L21_=ASIZE((FSETSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETSI587106640(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETSI411883121) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSIG_eltbrSIG_frame_struct other1_0;
FSETSIG_eltbrSIG_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSIG_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSI671175003(r, L9);
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

FSETSIG FSETSI217755790(FSETSIG self, SIG e) {
 FSETSIG ret_val;
 INT h = INT_zero;
 SIG te;
 INT i = INT_zero;
 SIG te1;
 INT hsh = INT_zero;
 FSETSIG elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSIG elt_eq_self;
 SIG elt_eq_e1;
 SIG elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSIG elt_eq_self1;
 SIG elt_eq_e11;
 SIG elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 FSETSIG elt_nil_self;
 SIG ret_val7;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val8;
 SIG e1 = ((SIG) NULL);
 FSETSIG is_elt_nil_self2;
 SIG is_elt_nil_e2;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self3;
 SIG is_elt_nil_e3;
 BOOL ret_val10 = BOOL_zero;
 FSETSIG elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val11 = INT_zero;
 FSETSIG elt_nil_self2;
 SIG ret_val12;
 ELT_NILSIG elt_nil_self3;
 SIG ret_val13;
 SIG e2 = ((SIG) NULL);
 FSETSIG elt_nil_self4;
 SIG ret_val14;
 ELT_NILSIG elt_nil_self5;
 SIG ret_val15;
 SIG e3 = ((SIG) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 SIG L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 INT L131_;
 SIG L14;
 INT L16;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 BOOL L231_;
 INT L241_;
 SIG L251_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L311_;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 SIG L401_;
 SIG L42;
 SIG L44;
 BOOL L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 BOOL L501_;
 SIG L511_;
 SIG L52;
 SIG L54;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(SIG)ARR((FSETSIG)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSIG) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((SIG) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
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
   ret_val6 = (is_eq_self==is_eq_s);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L81_=ASIZE((FSETSIG)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,2); 
  L11 = L101_;
  L121_=(h)==(L11); 
  if (L121_) {
   h = 0;
  }
  else {
   L131_=INTPLUS(h,1); 
   h = L131_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSIG) NULL);
 ret_val8 = ((SIG) NULL);
 ret_val7 = ret_val8;
 L14 = ret_val7;
 SARR((FSETSIG)self,h,(SIG)L14); 
 ;
 L16 = ATTR(self,hsize);
 L171_=INTMINUS(L16,1); 
 SATTR(self,hsize,L171_);
 i = h;
 while (1) {
  L181_=ASIZE((FSETSIG)self); 
  L19 = L181_;
  L201_=INTMINUS(L19,2); 
  L22 = L201_;
  L231_=(i)==(L22); 
  if (L231_) {
   i = 0;
  }
  else {
   L241_=INTPLUS(i,1); 
   i = L241_;
  }
  L251_=(SIG)ARR((FSETSIG)self,i); 
  te1 = L251_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NILSIG) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val10 = (is_elt_nil_e3==((SIG) NULL));
  ret_val9 = ret_val10;
  if (ret_val9) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = ((OB) te1);
  ret_val11 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L26 = ret_val11;
  L271_=ASIZE((FSETSIG)self); 
  L28 = L271_;
  L291_=INTMINUS(L28,2); 
  L30 = L291_;
  L311_=L26&L30; 
  hsh = L311_;
  L321_=(i)<(hsh); 
  L33 = L321_;
  L341_=!(L33); 
  if (L341_) {
   L361_=(h)<(i); 
   if (L361_) {
    L371_=(h)<(hsh); 
    L38 = L371_;
    L391_=!(L38); 
    L35 = L391_;
   } else {
    L35 = FALSE;
   }
   if (L35) {
    L401_=(SIG)ARR((FSETSIG)self,i); 
    L42 = L401_;
    SARR((FSETSIG)self,h,(SIG)L42); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSIG) NULL);
    ret_val13 = ((SIG) NULL);
    ret_val12 = ret_val13;
    L44 = ret_val12;
    SARR((FSETSIG)self,i,(SIG)L44); 
    ;
   }
  }
  else {
   L471_=(h)<(hsh); 
   L48 = L471_;
   L491_=!(L48); 
   if (L491_) {
    L46 = TRUE;
   } else {
    L501_=(h)<(i); 
    L46 = L501_;
   }
   if (L46) {
    L511_=(SIG)ARR((FSETSIG)self,i); 
    L52 = L511_;
    SARR((FSETSIG)self,h,(SIG)L52); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NILSIG) NULL);
    ret_val15 = ((SIG) NULL);
    ret_val14 = ret_val15;
    L54 = ret_val14;
    SARR((FSETSIG)self,i,(SIG)L54); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETSI488259882(self)) {
  ret_val = FSETSI1299465556(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI325078524(FSETSIG self, FSETSIG r, SIG e) {
 FSETSIG ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSIG elt_eq_self;
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
 BOOL L11_;
 SIG L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(SIG)ARR((FSETSIG)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSIG) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_s = elt_eq_e21;
  ret_val3 = (is_eq_self==is_eq_s);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETSIG)r,i,(SIG)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETSIG)r,L5,(SIG)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI346777215(FSETSIG self, SIG e) {
 FSETSIG ret_val;
 if ((self==((FSETSIG) NULL))) {
  ret_val = ((FSETSIG) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETSI217755790(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETSI112281076(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI461354464(FSETSIG self) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG L11;
 FSETSIG elt_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 INT L21 = INT_zero;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L81_br;
 SIG L9;
 INT L101_;
 if ((self==((FSETSIG) NULL))) {
  ret_val = ((FSETSIG) NULL);
  return ret_val;
 }
 L31_=ASIZE((FSETSIG)self); 
 L4 = L31_;
 L51_=(17)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 if (L71_) {
  r = self;
  SATTR(r,hsize,0);
  {
   BOOL f_L81_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L81_br=ASIZE((FSETSIG)L11); 
   while (1) {
    if(L21>=L81_br)  goto after_loop; 
    elt_nil_self = self;
    elt_nil_self1 = ((ELT_NILSIG) NULL);
    ret_val2 = ((SIG) NULL);
    ret_val1 = ret_val2;
    L9 = ret_val1;
    SARR((FSETSIG)L11,L21,(SIG)L9); 
    ;
    L101_=INTPLUS(L21,1); 
    L21 = L101_;
   }
  }
  after_loop: ;
  ret_val = r;
  return ret_val;
 }
 else {
  ret_val = ((FSETSIG) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI548941202(FSETSIG self, FSETSIG s) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG L11;
 SIG L2;
 r = self;
 {
  struct FSETSIG_eltbrSIG_frame_struct other1_0;
FSETSIG_eltbrSIG_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSIG_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSI671175003(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI587106640(FSETSIG self, INT n) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG L11;
 FSETSIG elt_nil_self;
 SIG ret_val1;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val2;
 SIG e = ((SIG) NULL);
 FSETSIG elt_nil_self2;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self3;
 SIG ret_val4;
 SIG e1 = ((SIG) NULL);
 INT L21 = INT_zero;
 FSETSIG L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 SIG L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETSIG_struct)-sizeof(SIG))+(L4)*sizeof(SIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETSIG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETSIG) L5);
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
   L91_br=ASIZE((FSETSIG)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSIG) NULL);
    ret_val4 = ((SIG) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETSIG)L11,L21,(SIG)L10); 
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

FSETSIG FSETSI671175003(FSETSIG self, SIG e) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG set_initial_stru;
 r = self;
 if ((r==((FSETSIG) NULL))) {
  r = FSETSI587106640(self, FSETSI1350248406);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETSI2024459664);
 }
 else {
  r = FSETSI833456188(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETSI1119826300(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETSI325078524(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI719979460(FSETSIG self) {
 FSETSIG ret_val;
 FSETSIG r = ((FSETSIG) NULL);
 FSETSIG L11;
 SIG L2;
 {
  struct FSETSIG_eltbrSIG_frame_struct other1_0;
FSETSIG_eltbrSIG_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSIG_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSI671175003(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI833456188(FSETSIG self) {
 FSETSIG ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETSIG)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETSI1658388805;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETSI917491731(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETSIG)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETSI917491731(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSIG FSETSI917491731(FSETSIG self) {
 FSETSIG ret_val;
 FSETSIG r;
 FSETSIG L11;
 FSETSIG switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 SIG L9;
 L21_=ASIZE((FSETSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETSI587106640(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETSI411883121) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSIG_eltbrSIG_frame_struct other1_0;
FSETSIG_eltbrSIG_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSIG_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETSI671175003(r, L9);
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

FSETSTR FSETST1229526412(FSETSTR self) {
 FSETSTR ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETSTR)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETST854048553;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETST34137851(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETSTR)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETST34137851(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST1242756882(FSETSTR self, STR e) {
 FSETSTR ret_val;
 INT h = INT_zero;
 STR te;
 INT i = INT_zero;
 STR te1;
 INT hsh = INT_zero;
 FSETSTR elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETSTR elt_nil_self;
 STR ret_val6;
 ELT_NILSTR elt_nil_self1;
 STR ret_val7;
 STR e1 = ((STR) NULL);
 FSETSTR is_elt_nil_self2;
 STR is_elt_nil_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self3;
 STR is_elt_nil_e3;
 BOOL ret_val9 = BOOL_zero;
 FSETSTR elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val10 = INT_zero;
 FSETSTR elt_nil_self2;
 STR ret_val11;
 ELT_NILSTR elt_nil_self3;
 STR ret_val12;
 STR e2 = ((STR) NULL);
 FSETSTR elt_nil_self4;
 STR ret_val13;
 ELT_NILSTR elt_nil_self5;
 STR ret_val14;
 STR e3 = ((STR) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 STR L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 INT L131_;
 STR L14;
 INT L16;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 BOOL L231_;
 INT L241_;
 STR L251_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L311_;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 STR L401_;
 STR L42;
 STR L44;
 BOOL L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 BOOL L501_;
 STR L511_;
 STR L52;
 STR L54;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(STR)ARR((FSETSTR)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSTR) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L81_=ASIZE((FSETSTR)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,2); 
  L11 = L101_;
  L121_=(h)==(L11); 
  if (L121_) {
   h = 0;
  }
  else {
   L131_=INTPLUS(h,1); 
   h = L131_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSTR) NULL);
 ret_val7 = ((STR) NULL);
 ret_val6 = ret_val7;
 L14 = ret_val6;
 SARR((FSETSTR)self,h,(STR)L14); 
 ;
 L16 = ATTR(self,hsize);
 L171_=INTMINUS(L16,1); 
 SATTR(self,hsize,L171_);
 i = h;
 while (1) {
  L181_=ASIZE((FSETSTR)self); 
  L19 = L181_;
  L201_=INTMINUS(L19,2); 
  L22 = L201_;
  L231_=(i)==(L22); 
  if (L231_) {
   i = 0;
  }
  else {
   L241_=INTPLUS(i,1); 
   i = L241_;
  }
  L251_=(STR)ARR((FSETSTR)self,i); 
  te1 = L251_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NILSTR) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val9 = (is_elt_nil_e3==((STR) NULL));
  ret_val8 = ret_val9;
  if (ret_val8) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = ((OB) te1);
  ret_val10 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L26 = ret_val10;
  L271_=ASIZE((FSETSTR)self); 
  L28 = L271_;
  L291_=INTMINUS(L28,2); 
  L30 = L291_;
  L311_=L26&L30; 
  hsh = L311_;
  L321_=(i)<(hsh); 
  L33 = L321_;
  L341_=!(L33); 
  if (L341_) {
   L361_=(h)<(i); 
   if (L361_) {
    L371_=(h)<(hsh); 
    L38 = L371_;
    L391_=!(L38); 
    L35 = L391_;
   } else {
    L35 = FALSE;
   }
   if (L35) {
    L401_=(STR)ARR((FSETSTR)self,i); 
    L42 = L401_;
    SARR((FSETSTR)self,h,(STR)L42); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSTR) NULL);
    ret_val12 = ((STR) NULL);
    ret_val11 = ret_val12;
    L44 = ret_val11;
    SARR((FSETSTR)self,i,(STR)L44); 
    ;
   }
  }
  else {
   L471_=(h)<(hsh); 
   L48 = L471_;
   L491_=!(L48); 
   if (L491_) {
    L46 = TRUE;
   } else {
    L501_=(h)<(i); 
    L46 = L501_;
   }
   if (L46) {
    L511_=(STR)ARR((FSETSTR)self,i); 
    L52 = L511_;
    SARR((FSETSTR)self,h,(STR)L52); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NILSTR) NULL);
    ret_val14 = ((STR) NULL);
    ret_val13 = ret_val14;
    L54 = ret_val13;
    SARR((FSETSTR)self,i,(STR)L54); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETST1951807602(self)) {
  ret_val = FSETST555430076(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST1404644833(FSETSTR self, STR e) {
 FSETSTR ret_val;
 FSETSTR r;
 FSETSTR set_initial_stru;
 r = self;
 if ((r==((FSETSTR) NULL))) {
  r = FSETST983176864(self, FSETST1746314450);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETST1011828476);
 }
 else {
  r = FSETST1229526412(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETST813910036(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETST1608657812(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST1608657812(FSETSTR self, FSETSTR r, STR e) {
 FSETSTR ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 STR L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(STR)ARR((FSETSTR)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQSTR) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETSTR)r,i,(STR)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETSTR)r,L5,(STR)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST1872370245(FSETSTR self, STR e) {
 FSETSTR ret_val;
 if ((self==((FSETSTR) NULL))) {
  ret_val = ((FSETSTR) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETST1242756882(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETST2089025776(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST1884046860(FSETSTR self) {
 FSETSTR ret_val;
 FSETSTR r = ((FSETSTR) NULL);
 FSETSTR L11;
 STR L2;
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETST1404644833(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST2089025776(FSETSTR self, STR e) {
 FSETSTR ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 STR L41_;
 INT L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L13;
 INT L141_;
 BOOL L151_;
 INT L161_;
 STR L171_;
 STR L18;
 INT L20;
 INT L211_;
 delete_elt_ind = -1;
 hash_table_size = ATTR(self,hsize);
 i = 0;
 while (1) {
  L11_=(i)<(hash_table_size); 
  L2 = L11_;
  L31_=!(L2); 
  if (L31_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L41_=(STR)ARR((FSETSTR)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQSTR) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = STR_is111530248(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L71_=(delete_elt_ind)<(0); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,hsize);
  L121_=(delete_elt_ind)<(L10); 
  L6 = L121_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  empty_loc = delete_elt_ind;
  L13 = ATTR(self,hsize);
  L141_=INTMINUS(L13,2); 
  second_to_last_i = L141_;
  while (1) {
   L151_=(second_to_last_i)<(empty_loc); 
   if (L151_) {
    goto after_loop1;
   }
   L161_=INTPLUS(empty_loc,1); 
   next = L161_;
   L171_=(STR)ARR((FSETSTR)self,next); 
   L18 = L171_;
   SARR((FSETSTR)self,empty_loc,(STR)L18); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L20 = ATTR(self,hsize);
  L211_=INTMINUS(L20,1); 
  SATTR(self,hsize,L211_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST34137851(FSETSTR self) {
 FSETSTR ret_val;
 FSETSTR r;
 FSETSTR L11;
 FSETSTR switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 STR L9;
 L21_=ASIZE((FSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETST983176864(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETST846796035) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETST1404644833(r, L9);
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

FSETSTR FSETST555430076(FSETSTR self) {
 FSETSTR ret_val;
 FSETSTR r;
 FSETSTR L11;
 FSETSTR switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 STR L9;
 L21_=ASIZE((FSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETST983176864(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETST846796035) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETST1404644833(r, L9);
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

FSETSTR FSETST813910036(FSETSTR self, FSETSTR r, STR e) {
 FSETSTR ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 STR te;
 STR te1;
 FSETSTR elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETSTR is_elt_nil_self2;
 STR is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self3;
 STR is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETSTR elt_eq_self2;
 STR elt_eq_e12;
 STR elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self3;
 STR elt_eq_e13;
 STR elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 STR L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 STR L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETSTR)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(STR)ARR((FSETSTR)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILSTR) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETSTR)r,h,(STR)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(STR)ARR((FSETSTR)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILSTR) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQSTR) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = STR_is111530248(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETSTR)r,h,(STR)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETSTR)r,h,(STR)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST866022026(FSETSTR self, FSETSTR s) {
 FSETSTR ret_val;
 FSETSTR r;
 FSETSTR L11;
 STR L2;
 r = self;
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETST1404644833(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST897681893(FSETSTR self, FSETSTR s) {
 FSETSTR ret_val;
 FSETSTR r = ((FSETSTR) NULL);
 FSETSTR L11;
 STR e;
 STR L2;
 BOOL L3;
 BOOL L41_;
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L2;
   L3 = FSETST1025458804(s, e);
   L41_=!(L3); 
   if (L41_) {
    r = FSETST1404644833(r, e);
   }
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR FSETST983176864(FSETSTR self, INT n) {
 FSETSTR ret_val;
 FSETSTR r;
 FSETSTR L11;
 FSETSTR elt_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self1;
 STR ret_val2;
 STR e = ((STR) NULL);
 FSETSTR elt_nil_self2;
 STR ret_val3;
 ELT_NILSTR elt_nil_self3;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 INT L21 = INT_zero;
 FSETSTR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 STR L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETSTR_struct)-sizeof(STR))+(L4)*sizeof(STR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETSTR) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSTR) NULL);
 ret_val2 = ((STR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((STR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETSTR)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETSTR)L11,L21,(STR)L10); 
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

FSETTP_CLASS FSETTP1145293274(FSETTP_CLASS self, FSETTP_CLASS r, TP_CLASS e) {
 FSETTP_CLASS ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETTP_CLASS elt_eq_self;
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
 BOOL L11_;
 TP_CLASS L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(TP_CLASS)ARR((FSETTP_CLASS)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_t = ((OB) elt_eq_e21);
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETTP_CLASS)r,i,(TP_CLASS)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETTP_CLASS)r,L5,(TP_CLASS)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS FSETTP1186813482(FSETTP_CLASS self, TP_CLASS e) {
 FSETTP_CLASS ret_val;
 FSETTP_CLASS r;
 FSETTP_CLASS set_initial_stru;
 r = self;
 if ((r==((FSETTP_CLASS) NULL))) {
  r = FSETTP1786600189(self, FSETTP752200044);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETTP1720402111);
 }
 else {
  r = FSETTP779193663(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETTP149854118(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETTP1145293274(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS FSETTP149854118(FSETTP_CLASS self, FSETTP_CLASS r, TP_CLASS e) {
 FSETTP_CLASS ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TP_CLASS te;
 TP_CLASS te1;
 FSETTP_CLASS elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val6 = BOOL_zero;
 FSETTP_CLASS is_elt_nil_self2;
 TP_CLASS is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self3;
 TP_CLASS is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETTP_CLASS elt_eq_self2;
 TP_CLASS elt_eq_e12;
 TP_CLASS elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self3;
 TP_CLASS elt_eq_e13;
 TP_CLASS elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val11 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 TP_CLASS L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 TP_CLASS L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETTP_CLASS)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(TP_CLASS)ARR((FSETTP_CLASS)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((TP_CLASS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETTP_CLASS)r,h,(TP_CLASS)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(TP_CLASS)ARR((FSETTP_CLASS)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((TP_CLASS) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     SARR((FSETTP_CLASS)r,h,(TP_CLASS)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETTP_CLASS)r,h,(TP_CLASS)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS FSETTP1786600189(FSETTP_CLASS self, INT n) {
 FSETTP_CLASS ret_val;
 FSETTP_CLASS r;
 FSETTP_CLASS L11;
 FSETTP_CLASS elt_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 FSETTP_CLASS elt_nil_self2;
 TP_CLASS ret_val3;
 ELT_NILTP_CLASS elt_nil_self3;
 TP_CLASS ret_val4;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 INT L21 = INT_zero;
 FSETTP_CLASS L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TP_CLASS L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETTP_CLASS_struct)-sizeof(TP_CLASS))+(L4)*sizeof(TP_CLASS);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETTP_CLASS_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETTP_CLASS) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val2 = ((TP_CLASS) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((TP_CLASS) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETTP_CLASS)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTP_CLASS) NULL);
    ret_val4 = ((TP_CLASS) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETTP_CLASS)L11,L21,(TP_CLASS)L10); 
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

FSETTP_CLASS FSETTP1983095296(FSETTP_CLASS self) {
 FSETTP_CLASS ret_val;
 FSETTP_CLASS r;
 FSETTP_CLASS L11;
 FSETTP_CLASS switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TP_CLASS L9;
 L21_=ASIZE((FSETTP_CLASS)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETTP1786600189(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETTP138222400) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETTP340014621_frame_struct other1_0;
FSETTP340014621_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETTP340014621(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTP1186813482(r, L9);
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

FSETTP_CLASS FSETTP633280235(FSETTP_CLASS self, FSETTP_CLASS s) {
 FSETTP_CLASS ret_val;
 FSETTP_CLASS r;
 FSETTP_CLASS L11;
 TP_CLASS L2;
 r = self;
 {
  struct FSETTP340014621_frame_struct other1_0;
FSETTP340014621_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETTP340014621(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTP1186813482(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS FSETTP779193663(FSETTP_CLASS self) {
 FSETTP_CLASS ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETTP_CLASS)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETTP1014013453;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETTP1983095296(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETTP_CLASS)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETTP1983095296(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU134672224(FSETTUPIDENTINT self, FSETTUPIDENTINT r, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TUPIDENTINT te = TUPIDENTINT_zero;
 TUPIDENTINT te1 = TUPIDENTINT_zero;
 FSETTUPIDENTINT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTUPIDENTINT is_elt_nil_self;
 TUPIDENTINT is_elt_nil_e = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self1;
 TUPIDENTINT is_elt_nil_e1 = TUPIDENTINT_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val5 = BOOL_zero;
 FSETTUPIDENTINT is_elt_nil_self2;
 TUPIDENTINT is_elt_nil_e2 = TUPIDENTINT_zero;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self3;
 TUPIDENTINT is_elt_nil_e3 = TUPIDENTINT_zero;
 BOOL ret_val7 = BOOL_zero;
 FSETTUPIDENTINT elt_eq_self2;
 TUPIDENTINT elt_eq_e12 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e22 = TUPIDENTINT_zero;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self3;
 TUPIDENTINT elt_eq_e13 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e23 = TUPIDENTINT_zero;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 OB L2;
 OB L3;
 INT L4;
 INT L51_;
 INT L6;
 INT L71_;
 TUPIDENTINT L81_;
 INT L101_;
 INT L121_;
 INT L13;
 BOOL L141_;
 TUPIDENTINT L151_;
 INT L171_;
 INT L19;
 INT L201_;
 L11_=ASIZE((FSETTUPIDENTINT)r); 
 asz = L11_;
 elt_hash_self = r;
 L3=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=TUPIDENTINT_tag;
 L2 = (OB)((TUPIDENTINT_boxed) L3);
 ((TUPIDENTINT_boxed) L2)->immutable_part = e;
 elt_hash_e = L2;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L4 = ret_val1;
 L51_=INTMINUS(asz,2); 
 L6 = L51_;
 L71_=L4&L6; 
 orig_h = L71_;
 h = orig_h;
 while (1) {
  L81_=ARR((FSETTUPIDENTINT)r,h); 
  te = L81_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1575801776) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e1.t2);
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1475848140) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETTUPIDENTINT)r,h,e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L101_=INTPLUS(h,1); 
  h = L101_;
 }
 after_loop: ;
 L121_=INTMINUS(asz,1); 
 L13 = L121_;
 L141_=(h)==(L13); 
 if (L141_) {
  h = 0;
  while (1) {
   L151_=ARR((FSETTUPIDENTINT)r,h); 
   te1 = L151_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1575801776) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e3.t2);
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1475848140) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = TUPIDE1439069590(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETTUPIDENTINT)r,h,e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L171_=INTPLUS(h,1); 
   h = L171_;
  }
  after_loop1: ;
 }
 SARR((FSETTUPIDENTINT)r,h,e); 
 ;
 L19 = ATTR(r,hsize);
 L201_=INTPLUS(L19,1); 
 SATTR(r,hsize,L201_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU1542636576(FSETTUPIDENTINT self, FSETTUPIDENTINT r, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 TUPIDENTINT L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=ARR((FSETTUPIDENTINT)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1475848140) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETTUPIDENTINT)r,i,e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETTUPIDENTINT)r,L5,e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU1559268184(FSETTUPIDENTINT self, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 if ((self==((FSETTUPIDENTINT) NULL))) {
  ret_val = ((FSETTUPIDENTINT) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETTU475443387(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETTU725727349(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU1996664292(FSETTUPIDENTINT self, INT n) {
 FSETTUPIDENTINT ret_val;
 FSETTUPIDENTINT r;
 FSETTUPIDENTINT L11;
 FSETTUPIDENTINT elt_nil_self;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self1;
 TUPIDENTINT ret_val2 = TUPIDENTINT_zero;
 TUPIDENTINT e = TUPIDENTINT_zero;
 FSETTUPIDENTINT elt_nil_self2;
 TUPIDENTINT ret_val3 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self3;
 TUPIDENTINT ret_val4 = TUPIDENTINT_zero;
 TUPIDENTINT e1 = TUPIDENTINT_zero;
 INT L21 = INT_zero;
 FSETTUPIDENTINT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPIDENTINT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETTUPIDENTINT_struct)-sizeof(TUPIDENTINT))+(L4)*sizeof(TUPIDENTINT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETTUPIDENTINT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETTUPIDENTINT) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1575801776) NULL);
 ret_val2 = TUPIDENTINT_zero;
 ret_val1 = ret_val2;
 L7 = (ret_val1.t1.str==(STR)0)&&INT_IS_VOID(ret_val1.t2);
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETTUPIDENTINT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1575801776) NULL);
    ret_val4 = TUPIDENTINT_zero;
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETTUPIDENTINT)L11,L21,L10); 
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

FSETTUPIDENTINT FSETTU2045052674(FSETTUPIDENTINT self, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 FSETTUPIDENTINT r;
 FSETTUPIDENTINT set_initial_stru;
 r = self;
 if ((r==((FSETTUPIDENTINT) NULL))) {
  r = FSETTU1996664292(self, FSETTU561674193);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETTU1862000339);
 }
 else {
  r = FSETTU302354864(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETTU134672224(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETTU1542636576(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU302354864(FSETTUPIDENTINT self) {
 FSETTUPIDENTINT ret_val;
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
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETTUPIDENTINT)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETTU939302060;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETTU425674983(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETTUPIDENTINT)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETTU425674983(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU419377758(FSETTUPIDENTINT self) {
 FSETTUPIDENTINT ret_val;
 FSETTUPIDENTINT r;
 FSETTUPIDENTINT L11;
 FSETTUPIDENTINT elt_nil_self;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self1;
 TUPIDENTINT ret_val2 = TUPIDENTINT_zero;
 TUPIDENTINT e = TUPIDENTINT_zero;
 INT L21 = INT_zero;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L81_br;
 TUPIDENTINT L9;
 INT L101_;
 if ((self==((FSETTUPIDENTINT) NULL))) {
  ret_val = ((FSETTUPIDENTINT) NULL);
  return ret_val;
 }
 L31_=ASIZE((FSETTUPIDENTINT)self); 
 L4 = L31_;
 L51_=(17)<(L4); 
 L6 = L51_;
 L71_=!(L6); 
 if (L71_) {
  r = self;
  SATTR(r,hsize,0);
  {
   BOOL f_L81_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L81_br=ASIZE((FSETTUPIDENTINT)L11); 
   while (1) {
    if(L21>=L81_br)  goto after_loop; 
    elt_nil_self = self;
    elt_nil_self1 = ((ELT_NI1575801776) NULL);
    ret_val2 = TUPIDENTINT_zero;
    ret_val1 = ret_val2;
    L9 = ret_val1;
    SARR((FSETTUPIDENTINT)L11,L21,L9); 
    ;
    L101_=INTPLUS(L21,1); 
    L21 = L101_;
   }
  }
  after_loop: ;
  ret_val = r;
  return ret_val;
 }
 else {
  ret_val = ((FSETTUPIDENTINT) NULL);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU425674983(FSETTUPIDENTINT self) {
 FSETTUPIDENTINT ret_val;
 FSETTUPIDENTINT r;
 FSETTUPIDENTINT L11;
 FSETTUPIDENTINT switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTINT L9;
 L21_=ASIZE((FSETTUPIDENTINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETTU1996664292(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETTU574342446) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETTU947212086_frame_struct other1_0;
FSETTU947212086_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETTU947212086(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTU2045052674(r, L9);
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

FSETTUPIDENTINT FSETTU445885760(FSETTUPIDENTINT self) {
 FSETTUPIDENTINT ret_val;
 FSETTUPIDENTINT r;
 FSETTUPIDENTINT L11;
 FSETTUPIDENTINT switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTINT L9;
 L21_=ASIZE((FSETTUPIDENTINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETTU1996664292(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETTU574342446) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETTU947212086_frame_struct other1_0;
FSETTU947212086_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETTU947212086(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETTU2045052674(r, L9);
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

FSETTUPIDENTINT FSETTU475443387(FSETTUPIDENTINT self, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 INT h = INT_zero;
 TUPIDENTINT te = TUPIDENTINT_zero;
 INT i = INT_zero;
 TUPIDENTINT te1 = TUPIDENTINT_zero;
 INT hsh = INT_zero;
 FSETTUPIDENTINT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETTUPIDENTINT is_elt_nil_self;
 TUPIDENTINT is_elt_nil_e = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self1;
 TUPIDENTINT is_elt_nil_e1 = TUPIDENTINT_zero;
 BOOL ret_val3 = BOOL_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val5 = BOOL_zero;
 FSETTUPIDENTINT elt_nil_self;
 TUPIDENTINT ret_val6 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self1;
 TUPIDENTINT ret_val7 = TUPIDENTINT_zero;
 TUPIDENTINT e1 = TUPIDENTINT_zero;
 FSETTUPIDENTINT is_elt_nil_self2;
 TUPIDENTINT is_elt_nil_e2 = TUPIDENTINT_zero;
 BOOL ret_val8 = BOOL_zero;
 ELT_NI1575801776 is_elt_nil_self3;
 TUPIDENTINT is_elt_nil_e3 = TUPIDENTINT_zero;
 BOOL ret_val9 = BOOL_zero;
 FSETTUPIDENTINT elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val10 = INT_zero;
 FSETTUPIDENTINT elt_nil_self2;
 TUPIDENTINT ret_val11 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self3;
 TUPIDENTINT ret_val12 = TUPIDENTINT_zero;
 TUPIDENTINT e2 = TUPIDENTINT_zero;
 FSETTUPIDENTINT elt_nil_self4;
 TUPIDENTINT ret_val13 = TUPIDENTINT_zero;
 ELT_NI1575801776 elt_nil_self5;
 TUPIDENTINT ret_val14 = TUPIDENTINT_zero;
 TUPIDENTINT e3 = TUPIDENTINT_zero;
 OB L1;
 OB L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPIDENTINT L91_;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 BOOL L141_;
 INT L151_;
 TUPIDENTINT L16;
 INT L18;
 INT L191_;
 INT L201_;
 INT L21;
 INT L221_;
 INT L23;
 BOOL L241_;
 INT L251_;
 TUPIDENTINT L261_;
 OB L27;
 OB L28;
 INT L29;
 INT L301_;
 INT L31;
 INT L321_;
 INT L33;
 INT L341_;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 BOOL L401_;
 BOOL L42;
 BOOL L431_;
 TUPIDENTINT L441_;
 TUPIDENTINT L45;
 TUPIDENTINT L47;
 BOOL L49;
 BOOL L501_;
 BOOL L51;
 BOOL L521_;
 BOOL L531_;
 TUPIDENTINT L541_;
 TUPIDENTINT L55;
 TUPIDENTINT L57;
 elt_hash_self = self;
 L2=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=TUPIDENTINT_tag;
 L1 = (OB)((TUPIDENTINT_boxed) L2);
 ((TUPIDENTINT_boxed) L1)->immutable_part = e;
 elt_hash_e = L1;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L3 = ret_val1;
 L41_=ASIZE((FSETTUPIDENTINT)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FSETTUPIDENTINT)self,h); 
  te = L91_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1575801776) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e1.t2);
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1475848140) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L101_=ASIZE((FSETTUPIDENTINT)self); 
  L11 = L101_;
  L121_=INTMINUS(L11,2); 
  L13 = L121_;
  L141_=(h)==(L13); 
  if (L141_) {
   h = 0;
  }
  else {
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1575801776) NULL);
 ret_val7 = TUPIDENTINT_zero;
 ret_val6 = ret_val7;
 L16 = ret_val6;
 SARR((FSETTUPIDENTINT)self,h,L16); 
 ;
 L18 = ATTR(self,hsize);
 L191_=INTMINUS(L18,1); 
 SATTR(self,hsize,L191_);
 i = h;
 while (1) {
  L201_=ASIZE((FSETTUPIDENTINT)self); 
  L21 = L201_;
  L221_=INTMINUS(L21,2); 
  L23 = L221_;
  L241_=(i)==(L23); 
  if (L241_) {
   i = 0;
  }
  else {
   L251_=INTPLUS(i,1); 
   i = L251_;
  }
  L261_=ARR((FSETTUPIDENTINT)self,i); 
  te1 = L261_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NI1575801776) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val9 = (is_elt_nil_e3.t1.str==(STR)0)&&INT_IS_VOID(is_elt_nil_e3.t2);
  ret_val8 = ret_val9;
  if (ret_val8) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  L28=ZALLOC(sizeof(struct TUPIDENTINT_boxed_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=TUPIDENTINT_tag;
  L27 = (OB)((TUPIDENTINT_boxed) L28);
  ((TUPIDENTINT_boxed) L27)->immutable_part = te1;
  elt_hash_e1 = L27;
  ret_val10 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L29 = ret_val10;
  L301_=ASIZE((FSETTUPIDENTINT)self); 
  L31 = L301_;
  L321_=INTMINUS(L31,2); 
  L33 = L321_;
  L341_=L29&L33; 
  hsh = L341_;
  L351_=(i)<(hsh); 
  L36 = L351_;
  L371_=!(L36); 
  if (L371_) {
   L391_=(h)<(i); 
   if (L391_) {
    L401_=(h)<(hsh); 
    L42 = L401_;
    L431_=!(L42); 
    L38 = L431_;
   } else {
    L38 = FALSE;
   }
   if (L38) {
    L441_=ARR((FSETTUPIDENTINT)self,i); 
    L45 = L441_;
    SARR((FSETTUPIDENTINT)self,h,L45); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1575801776) NULL);
    ret_val12 = TUPIDENTINT_zero;
    ret_val11 = ret_val12;
    L47 = ret_val11;
    SARR((FSETTUPIDENTINT)self,i,L47); 
    ;
   }
  }
  else {
   L501_=(h)<(hsh); 
   L51 = L501_;
   L521_=!(L51); 
   if (L521_) {
    L49 = TRUE;
   } else {
    L531_=(h)<(i); 
    L49 = L531_;
   }
   if (L49) {
    L541_=ARR((FSETTUPIDENTINT)self,i); 
    L55 = L541_;
    SARR((FSETTUPIDENTINT)self,h,L55); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NI1575801776) NULL);
    ret_val14 = TUPIDENTINT_zero;
    ret_val13 = ret_val14;
    L57 = ret_val13;
    SARR((FSETTUPIDENTINT)self,i,L57); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETTU719980391(self)) {
  ret_val = FSETTU445885760(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETTUPIDENTINT FSETTU725727349(FSETTUPIDENTINT self, TUPIDENTINT e) {
 FSETTUPIDENTINT ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETTUPIDENTINT elt_eq_self;
 TUPIDENTINT elt_eq_e1 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e2 = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1475848140 elt_eq_self1;
 TUPIDENTINT elt_eq_e11 = TUPIDENTINT_zero;
 TUPIDENTINT elt_eq_e21 = TUPIDENTINT_zero;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 TUPIDENTINT L41_;
 INT L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L13;
 INT L141_;
 BOOL L151_;
 INT L161_;
 TUPIDENTINT L171_;
 TUPIDENTINT L18;
 INT L20;
 INT L211_;
 delete_elt_ind = -1;
 hash_table_size = ATTR(self,hsize);
 i = 0;
 while (1) {
  L11_=(i)<(hash_table_size); 
  L2 = L11_;
  L31_=!(L2); 
  if (L31_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L41_=ARR((FSETTUPIDENTINT)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQ1475848140) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = TUPIDE1439069590(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L71_=(delete_elt_ind)<(0); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,hsize);
  L121_=(delete_elt_ind)<(L10); 
  L6 = L121_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  empty_loc = delete_elt_ind;
  L13 = ATTR(self,hsize);
  L141_=INTMINUS(L13,2); 
  second_to_last_i = L141_;
  while (1) {
   L151_=(second_to_last_i)<(empty_loc); 
   if (L151_) {
    goto after_loop1;
   }
   L161_=INTPLUS(empty_loc,1); 
   next = L161_;
   L171_=ARR((FSETTUPIDENTINT)self,next); 
   L18 = L171_;
   SARR((FSETTUPIDENTINT)self,empty_loc,L18); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L20 = ATTR(self,hsize);
  L211_=INTMINUS(L20,1); 
  SATTR(self,hsize,L211_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FSETSI2058428832(FSETSIDE_EFFECT self) {
 INT ret_val = INT_zero;
 if ((self==((FSETSIDE_EFFECT) NULL))) {
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

INT FSETSIG_sizerINT(FSETSIG self) {
 INT ret_val = INT_zero;
 if ((self==((FSETSIG) NULL))) {
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

INT FSETSTR_sizerINT(FSETSTR self) {
 INT ret_val = INT_zero;
 if ((self==((FSETSTR) NULL))) {
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

SIDE_EFFECT FSETSI1316299458(FSETSIDE_EFFECT self, SIDE_EFFECT e) {
 SIDE_EFFECT ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETSIDE_EFFECT elt_eq_self;
 SIDE_EFFECT elt_eq_e1;
 SIDE_EFFECT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self1;
 SIDE_EFFECT elt_eq_e11;
 SIDE_EFFECT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 FSETSIDE_EFFECT elt_nil_self;
 SIDE_EFFECT ret_val3;
 ELT_NI351013237 elt_nil_self1;
 SIDE_EFFECT ret_val4;
 SIDE_EFFECT e1 = ((SIDE_EFFECT) NULL);
 BOOL L11_;
 SIDE_EFFECT L21_;
 SIDE_EFFECT L31_;
 INT L41_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ251059601) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = SIDE_E526657440(elt_eq_e11, elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   L31_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)self,i); 
   ret_val = L31_;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI351013237) NULL);
 ret_val4 = ((SIDE_EFFECT) NULL);
 ret_val3 = ret_val4;
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIDE_EFFECT FSETSI2035884295(FSETSIDE_EFFECT self, SIDE_EFFECT e) {
 SIDE_EFFECT ret_val;
 INT h = INT_zero;
 SIDE_EFFECT te;
 SIDE_EFFECT te1;
 FSETSIDE_EFFECT elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETSIDE_EFFECT is_elt_nil_self;
 SIDE_EFFECT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI351013237 is_elt_nil_self1;
 SIDE_EFFECT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETSIDE_EFFECT elt_eq_self;
 SIDE_EFFECT elt_eq_e1;
 SIDE_EFFECT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self1;
 SIDE_EFFECT elt_eq_e11;
 SIDE_EFFECT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETSIDE_EFFECT is_elt_nil_self2;
 SIDE_EFFECT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI351013237 is_elt_nil_self3;
 SIDE_EFFECT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETSIDE_EFFECT elt_eq_self2;
 SIDE_EFFECT elt_eq_e12;
 SIDE_EFFECT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ251059601 elt_eq_self3;
 SIDE_EFFECT elt_eq_e13;
 SIDE_EFFECT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 FSETSIDE_EFFECT elt_nil_self;
 SIDE_EFFECT ret_val10;
 ELT_NI351013237 elt_nil_self1;
 SIDE_EFFECT ret_val11;
 SIDE_EFFECT e1 = ((SIDE_EFFECT) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 SIDE_EFFECT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 SIDE_EFFECT L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETSIDE_EFFECT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI351013237) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((SIDE_EFFECT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ251059601) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = SIDE_E526657440(elt_eq_e11, elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = te;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETSIDE_EFFECT)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI351013237) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((SIDE_EFFECT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ251059601) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = SIDE_E526657440(elt_eq_e13, elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     ret_val = te1;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI351013237) NULL);
 ret_val11 = ((SIDE_EFFECT) NULL);
 ret_val10 = ret_val11;
 ret_val = ret_val10;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIDE_EFFECT FSETSI937439554(FSETSIDE_EFFECT self, SIDE_EFFECT e) {
 SIDE_EFFECT ret_val;
 FSETSIDE_EFFECT elt_nil_self;
 SIDE_EFFECT ret_val1;
 ELT_NI351013237 elt_nil_self1;
 SIDE_EFFECT ret_val2;
 SIDE_EFFECT e1 = ((SIDE_EFFECT) NULL);
 if ((self==((FSETSIDE_EFFECT) NULL))) {
  elt_nil_self = self;
  elt_nil_self1 = ((ELT_NI351013237) NULL);
  ret_val2 = ((SIDE_EFFECT) NULL);
  ret_val1 = ret_val2;
  ret_val = ret_val1;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETSI2035884295(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETSI1316299458(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SIG FSETSI2020712453(FSETSIG self) {
 SIG ret_val;
 FSETSIG L11;
 SIG r;
 INT L21 = INT_zero;
 FSETSIG is_elt_nil_self;
 SIG is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSIG is_elt_nil_self1;
 SIG is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 FSETSIG elt_nil_self;
 SIG ret_val3;
 ELT_NILSIG elt_nil_self1;
 SIG ret_val4;
 SIG e = ((SIG) NULL);
 BOOL L3;
 BOOL L41_;
 INT L51_br;
 SIG aL51_;
 SIG L6;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 SIG L131_;
 L3 = (self==((FSETSIG) NULL));
 L41_=!(L3); 
 if (L41_) {
  if (ATTR(self,use_map)) {
   {
    BOOL f_L51_ = TRUE;
    /* loop index variable */
    L21 = 0;
    L11 = self;
    L51_br=ASIZE((FSETSIG)L11); 
    while (1) {
     if(L21>=L51_br)  goto after_loop; 
     aL51_=ARR((FSETSIG)L11,L21); 
     r = aL51_;
     is_elt_nil_self = self;
     is_elt_nil_e = r;
     is_elt_nil_self1 = ((ELT_NILSIG) NULL);
     is_elt_nil_e1 = is_elt_nil_e;
     ret_val2 = (is_elt_nil_e1==((SIG) NULL));
     ret_val1 = ret_val2;
     L7 = ret_val1;
     L81_=!(L7); 
     if (L81_) {
      ret_val = r;
      return ret_val;
     }
     L91_=INTPLUS(L21,1); 
     L21 = L91_;
    }
   }
   after_loop: ;
  }
  else {
   L10 = ATTR(self,hsize);
   L121_=(0)<(L10); 
   if (L121_) {
    L131_=(SIG)ARR((FSETSIG)self,0); 
    ret_val = L131_;
    return ret_val;
   }
  }
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILSIG) NULL);
 ret_val4 = ((SIG) NULL);
 ret_val3 = ret_val4;
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FSETSTR_strrSTR(FSETSTR self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FSETSTR) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1717624110(((ELT_ALGSTR) NULL), ((dELTSTR) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 1

IMMUTA421759893 FSETIM205083650(FSETIM205083650_frame frame) {
 IMMUTA421759893 dummy = ((IMMUTA421759893) NULL);
 IMMUTA421759893 aI_u_I;
 IMMUTA421759893 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETIM184436961)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETIM184436961)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

IMMUTA421759893 FSETIM887960645(FSETIM887960645_frame frame) {
 IMMUTA421759893 dummy = ((IMMUTA421759893) NULL);
 BOOL L1;
 BOOL L21_;
 IMMUTA421759893 L5;
 IMMUTA421759893 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 IMMUTA421759893 L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETIM184436961) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETIM184436961)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETIM184436961)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1394471793) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((IMMUTA421759893) NULL));
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(IMMUTA421759893)ARR((FSETIM184436961)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SFILE_ID FSETSF12648100(FSETSF12648100_frame frame) {
 SFILE_ID dummy = SFILE_ID_zero;
 SFILE_ID aI_u_I;
 SFILE_ID L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSFILE_ID)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETSFILE_ID)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SFILE_ID FSETSF139710295(FSETSF139710295_frame frame) {
 SFILE_ID dummy = SFILE_ID_zero;
 BOOL L1;
 BOOL L21_;
 SFILE_ID L5;
 SFILE_ID aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 SFILE_ID L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETSFILE_ID) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETSFILE_ID)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETSFILE_ID)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NILSFILE_ID) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = INT_IS_VOID(frame->is_elt_nil_e1.loc);
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=ARR((FSETSFILE_ID)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SIDE_EFFECT FSETSI1267678819(FSETSI1267678819_frame frame) {
 SIDE_EFFECT dummy = ((SIDE_EFFECT) NULL);
 SIDE_EFFECT aI_u_I;
 SIDE_EFFECT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSIDE_EFFECT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETSIDE_EFFECT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SIDE_EFFECT FSETSI2093978094(FSETSI2093978094_frame frame) {
 SIDE_EFFECT dummy = ((SIDE_EFFECT) NULL);
 BOOL L1;
 BOOL L21_;
 SIDE_EFFECT L5;
 SIDE_EFFECT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 SIDE_EFFECT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETSIDE_EFFECT) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETSIDE_EFFECT)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETSIDE_EFFECT)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI351013237) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((SIDE_EFFECT) NULL));
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(SIDE_EFFECT)ARR((FSETSIDE_EFFECT)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

SIG FSETSI2112123043(FSETSI2112123043_frame frame) {
 SIG dummy = ((SIG) NULL);
 SIG aI_u_I;
 SIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

SIG FSETSIG_eltbrSIG(FSETSIG_eltbrSIG_frame frame) {
 SIG dummy = ((SIG) NULL);
 BOOL L1;
 BOOL L21_;
 SIG L5;
 SIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 SIG L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETSIG) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETSIG)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETSIG)frame->L61,frame->L31); 
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(SIG)ARR((FSETSIG)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

STR FSETST261377883(FSETST261377883_frame frame) {
 STR dummy = ((STR) NULL);
 STR aI_u_I;
 STR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame frame) {
 STR dummy = ((STR) NULL);
 BOOL L1;
 BOOL L21_;
 STR L5;
 STR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 STR L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETSTR)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETSTR)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NILSTR) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((STR) NULL));
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(STR)ARR((FSETSTR)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS FSETTP340014621(FSETTP340014621_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 BOOL L1;
 BOOL L21_;
 TP_CLASS L5;
 TP_CLASS aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 TP_CLASS L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETTP_CLASS) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETTP_CLASS)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETTP_CLASS)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((TP_CLASS) NULL));
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(TP_CLASS)ARR((FSETTP_CLASS)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_CLASS FSETTP49057756(FSETTP49057756_frame frame) {
 TP_CLASS dummy = ((TP_CLASS) NULL);
 TP_CLASS aI_u_I;
 TP_CLASS L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTP_CLASS)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETTP_CLASS)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TP_LAYOUT FSETTP1672834608(FSETTP1672834608_frame frame) {
 TP_LAYOUT dummy = ((TP_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 TP_LAYOUT L5;
 TP_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 TP_LAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETTP_LAYOUT) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETTP_LAYOUT)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETTP_LAYOUT)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NILTP_LAYOUT) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((TP_LAYOUT) NULL));
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(TP_LAYOUT)ARR((FSETTP_LAYOUT)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TP_LAYOUT FSETTP381129783(FSETTP381129783_frame frame) {
 TP_LAYOUT dummy = ((TP_LAYOUT) NULL);
 TP_LAYOUT aI_u_I;
 TP_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTP_LAYOUT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETTP_LAYOUT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDENTINT FSETTU507892683(FSETTU507892683_frame frame) {
 TUPIDENTINT dummy = TUPIDENTINT_zero;
 TUPIDENTINT aI_u_I;
 TUPIDENTINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTUPIDENTINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETTUPIDENTINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPIDENTINT FSETTU947212086(FSETTU947212086_frame frame) {
 TUPIDENTINT dummy = TUPIDENTINT_zero;
 BOOL L1;
 BOOL L21_;
 TUPIDENTINT L5;
 TUPIDENTINT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 TUPIDENTINT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETTUPIDENTINT) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETTUPIDENTINT)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETTUPIDENTINT)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1575801776) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1.t1.str==(STR)0)&&INT_IS_VOID(frame->is_elt_nil_e1.t2);
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
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=ARR((FSETTUPIDENTINT)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void FSETIM1858750716(FSETIM1858750716_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETIM184436961)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETIM184436961)frame->self,frame->i_I_u_I,(IMMUTA421759893)frame->arg1); frame->i_I_u_I++;
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

void FSETSF1565053082(FSETSF1565053082_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSFILE_ID)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETSFILE_ID)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FSETSI1764914571(FSETSI1764914571_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETSIG)frame->self,frame->i_I_u_I,(SIG)frame->arg1); frame->i_I_u_I++;
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

void FSETSI1989523573(FSETSI1989523573_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSIDE_EFFECT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETSIDE_EFFECT)frame->self,frame->i_I_u_I,(SIDE_EFFECT)frame->arg1); frame->i_I_u_I++;
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

void FSETST679307565(FSETST679307565_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETSTR)frame->self,frame->i_I_u_I,(STR)frame->arg1); frame->i_I_u_I++;
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

void FSETTP1601462738(FSETTP1601462738_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTP_CLASS)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETTP_CLASS)frame->self,frame->i_I_u_I,(TP_CLASS)frame->arg1); frame->i_I_u_I++;
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

void FSETTU213952071(FSETTU213952071_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETTUPIDENTINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETTUPIDENTINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
