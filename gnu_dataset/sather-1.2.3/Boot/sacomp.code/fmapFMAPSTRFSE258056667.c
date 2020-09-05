#include "sather.h"

/* Layouts */

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ELT_EQdOB_struct {/* layout for ELT_EQ{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdOB;

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

typedef struct ELT_NILdOB_struct {/* layout for ELT_NIL{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdOB;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

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

typedef struct TUPdOBNAMESPACE_struct {/* layout for TUP{$OB,NAMESPACE} */
 OB t1;
 struct NAMESPACE_struct *t2;
 } TUPdOBNAMESPACE;
static TUPdOBNAMESPACE TUPdOBNAMESPACE_zero;

typedef struct TUPdOBNAMESPACE_boxed_struct {
 OB_HEADER header;
 TUPdOBNAMESPACE immutable_part;
 } *TUPdOBNAMESPACE_boxed;

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

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPdO1840166113_frame_struct {
 INT state;
 FMAPdOBNAMESPACE self;/* Formal argument: self */
 TUPdOBNAMESPACE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdO1840166113_frame;

typedef struct FMAPdO701667859_frame_struct {
 INT state;
 FMAPdOBNAMESPACE self;/* Formal argument: self */
 TUPdOBNAMESPACE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPdO701667859_frame;

typedef struct FMAPdO1025347514_frame_struct {
 INT state;
 FMAPdOBNAMESPACE self;/* Formal argument: self */
 TUPdOBNAMESPACE ret_val1;
 FMAPdOBNAMESPACE L61;
 TUPdOBNAMESPACE r;
 INT L31;
 FMAPdOBNAMESPACE is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPdO1025347514_frame;

typedef struct FMAPSTRFSETSTR_struct {/* layout for FMAP{STR,FSET{STR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FMAPSTRFSETSTR;

typedef struct FMAPST753435567_frame_struct {
 INT state;
 FMAPSTRFSETSTR self;/* Formal argument: self */
 TUPSTRFSETSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST753435567_frame;

typedef struct FMAPST653613295_frame_struct {
 INT state;
 FMAPSTRFSETSTR self;/* Formal argument: self */
 TUPSTRFSETSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST653613295_frame;

typedef struct FMAPST1155259492_frame_struct {
 INT state;
 FMAPSTRFSETSTR self;/* Formal argument: self */
 TUPSTRFSETSTR ret_val2;
 FMAPSTRFSETSTR L61;
 TUPSTRFSETSTR r;
 INT L31;
 FMAPSTRFSETSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1155259492_frame;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FMAPST403469175_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 TUPSTRINT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST403469175_frame;

typedef struct FMAPST165607239_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 TUPSTRINT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST165607239_frame;

typedef struct FMAPST961226608_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 STR ret_val2;
 FMAPSTRINT L61;
 STR r;
 INT L31;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST961226608_frame;

typedef struct FMAPST864901056_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 TUPSTRINT ret_val2;
 FMAPSTRINT L61;
 TUPSTRINT r;
 INT L31;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST864901056_frame;

typedef struct FMAPST1937251125_frame_struct {
 INT state;
 FMAPSTRINT self;/* Formal argument: self */
 INT ret_val2;
 FMAPSTRINT L61;
 TUPSTRINT e;
 INT L31;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1937251125_frame;

typedef struct FMAPSTRSIG_struct {/* layout for FMAP{STR,SIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FMAPSTRSIG;

typedef struct FMAPST486993187_frame_struct {
 INT state;
 FMAPSTRSIG self;/* Formal argument: self */
 TUPSTRSIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST486993187_frame;

typedef struct FMAPST82083227_frame_struct {
 INT state;
 FMAPSTRSIG self;/* Formal argument: self */
 TUPSTRSIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST82083227_frame;

typedef struct FMAPST1841912328_frame_struct {
 INT state;
 FMAPSTRSIG self;/* Formal argument: self */
 TUPSTRSIG ret_val2;
 FMAPSTRSIG L61;
 TUPSTRSIG r;
 INT L31;
 FMAPSTRSIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1841912328_frame;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

typedef struct FMAPST2026852485_frame_struct {
 INT state;
 FMAPSTRSTR self;/* Formal argument: self */
 TUPSTRSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST2026852485_frame;

typedef struct FMAPST1699038397_frame_struct {
 INT state;
 FMAPSTRSTR self;/* Formal argument: self */
 TUPSTRSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST1699038397_frame;

typedef struct FMAPST2087339362_frame_struct {
 INT state;
 FMAPSTRSTR self;/* Formal argument: self */
 STR ret_val2;
 FMAPSTRSTR L61;
 STR r;
 INT L31;
 FMAPSTRSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST2087339362_frame;

typedef struct FMAPST1934702696_frame_struct {
 INT state;
 FMAPSTRSTR self;/* Formal argument: self */
 TUPSTRSTR ret_val2;
 FMAPSTRSTR L61;
 TUPSTRSTR r;
 INT L31;
 FMAPSTRSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1934702696_frame;

typedef struct FMAPTP291739594_struct {/* layout for FMAP{TP_CLASS,FSET{$TP}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_858321267_struct arr_part[1];
 } *FMAPTP291739594;

typedef struct FMAPTP1032211620_frame_struct {
 INT state;
 FMAPTP291739594 self;/* Formal argument: self */
 TUPTP_858321267 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPTP1032211620_frame;

typedef struct FMAPTP323231562_frame_struct {
 INT state;
 FMAPTP291739594 self;/* Formal argument: self */
 TUPTP_858321267 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPTP323231562_frame;

typedef struct FMAPTP1465407463_frame_struct {
 INT state;
 FMAPTP291739594 self;/* Formal argument: self */
 TUPTP_858321267 ret_val2;
 FMAPTP291739594 L61;
 TUPTP_858321267 r;
 INT L31;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPTP1465407463_frame;

typedef struct FMAPTP775297600_struct {/* layout for FMAP{TP_CLASS,FSET{TP_CLASS}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPTP_1668454172_struct arr_part[1];
 } *FMAPTP775297600;

typedef struct FMAPTP652676349_frame_struct {
 INT state;
 FMAPTP775297600 self;/* Formal argument: self */
 TUPTP_1668454172 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPTP652676349_frame;

typedef struct FMAPTP702226833_frame_struct {
 INT state;
 FMAPTP775297600 self;/* Formal argument: self */
 TUPTP_1668454172 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPTP702226833_frame;

typedef struct FMAPTP1065055386_frame_struct {
 INT state;
 FMAPTP775297600 self;/* Formal argument: self */
 TUPTP_1668454172 ret_val2;
 FMAPTP775297600 L61;
 TUPTP_1668454172 r;
 INT L31;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPTP1065055386_frame;

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

extern INT FMAPST1192706365;
extern INT FMAPST1449505507;
extern INT FMAPST2041121537;
extern INT FMAPST47057045;
extern INT FMAPTP642805238;
extern INT FMAPTP676284949;
extern INT FMAPdO802731536;

/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);
BOOL ELT_EQ936498558(ELT_EQdOB, OB, OB);
BOOL ELT_NI455285430(ELT_NILdOB, OB);
BOOL FMAPST800079349(FMAPSTRINT, STR);
BOOL FMAPdO1742903675(FMAPdOBNAMESPACE);
BOOL STR_is111530248(STR, STR);
FMAPSTRFSETSTR FMAPST1555523727(FMAPSTRFSETSTR, INT);
FMAPSTRFSETSTR FMAPST1602224682(FMAPSTRFSETSTR);
FMAPSTRFSETSTR FMAPST935488038(FMAPSTRFSETSTR, STR, FSETSTR);
FMAPSTRINT FMAPST1221291526(FMAPSTRINT);
FMAPSTRINT FMAPST1767801431(FMAPSTRINT, INT);
FMAPSTRINT FMAPST951578658(FMAPSTRINT, STR, INT);
FMAPSTRSIG FMAPST1822785341(FMAPSTRSIG, INT);
FMAPSTRSIG FMAPST451382576(FMAPSTRSIG, STR, SIG);
FMAPSTRSIG FMAPST850176290(FMAPSTRSIG);
FMAPSTRSTR FMAPST274513051(FMAPSTRSTR, INT);
FMAPSTRSTR FMAPST395749006(FMAPSTRSTR);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FMAPTP291739594 FMAPTP2133001945(FMAPTP291739594);
FMAPTP291739594 FMAPTP551118428(FMAPTP291739594, TP_CLASS, FSETdTP);
FMAPTP291739594 FMAPTP786961404(FMAPTP291739594, INT);
FMAPTP775297600 FMAPTP1339820925(FMAPTP775297600, INT);
FMAPTP775297600 FMAPTP148513176(FMAPTP775297600);
FMAPTP775297600 FMAPTP594824795(FMAPTP775297600, TP_CLASS, FSETTP_CLASS);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FMAPdOBNAMESPACE FMAPdO1864084602(FMAPdOBNAMESPACE, OB);
FMAPdOBNAMESPACE FMAPdO1902821281(FMAPdOBNAMESPACE, INT);
FMAPdOBNAMESPACE FMAPdO588587769(FMAPdOBNAMESPACE);
FMAPdOBNAMESPACE FMAPdO899482984(FMAPdOBNAMESPACE);
FSETSTR FMAPST521088757(FMAPSTRFSETSTR, STR);
FSETTP_CLASS FMAPTP2096448595(FMAPTP775297600, TP_CLASS);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FMAPST1755966813(FMAPSTRINT, STR);
INT FMAPST1937251125(FMAPST1937251125_frame);
INT FMAPST868850642(FMAPSTRINT);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
OB ELT_NI509824640(ELT_NILdOB);
SIG FMAPST879183273(FMAPSTRSIG, STR);
STR FMAPST1518383807(FMAPSTRSTR, STR);
STR FMAPST1581005786(FMAPSTRINT);
STR FMAPST1670441508(FMAPSTRSTR);
STR FMAPST1891365712(FMAPSTRSIG);
STR FMAPST2087339362(FMAPST2087339362_frame);
STR FMAPST35426998(FMAPSTRFSETSTR);
STR FMAPST961226608(FMAPST961226608_frame);
STR FMAPTP1432889353(FMAPTP291739594);
STR FMAPTP722426429(FMAPTP775297600);
STR FMAPdO570867591(FMAPdOBNAMESPACE);
STR STR_fr1097270334(STR, FSTR);
STR TP_CLASS_strrSTR(TP_CLASS);
TUPSTRFSETSTR FMAPST1155259492(FMAPST1155259492_frame);
TUPSTRFSETSTR FMAPST753435567(FMAPST753435567_frame);
TUPSTRINT FMAPST403469175(FMAPST403469175_frame);
TUPSTRINT FMAPST864901056(FMAPST864901056_frame);
TUPSTRSIG FMAPST1841912328(FMAPST1841912328_frame);
TUPSTRSIG FMAPST486993187(FMAPST486993187_frame);
TUPSTRSTR FMAPST1934702696(FMAPST1934702696_frame);
TUPSTRSTR FMAPST2026852485(FMAPST2026852485_frame);
TUPTP_1668454172 FMAPTP1065055386(FMAPTP1065055386_frame);
TUPTP_1668454172 FMAPTP1108250174(FMAPTP775297600, TP_CLASS);
TUPTP_1668454172 FMAPTP652676349(FMAPTP652676349_frame);
TUPTP_858321267 FMAPTP1032211620(FMAPTP1032211620_frame);
TUPTP_858321267 FMAPTP1465407463(FMAPTP1465407463_frame);
TUPTP_858321267 FMAPTP1881680767(FMAPTP291739594, TP_CLASS);
TUPdOBNAMESPACE FMAPdO1025347514(FMAPdO1025347514_frame);
TUPdOBNAMESPACE FMAPdO1840166113(FMAPdO1840166113_frame);
void FMAPST165607239(FMAPST165607239_frame);
void FMAPST1699038397(FMAPST1699038397_frame);
void FMAPST653613295(FMAPST653613295_frame);
void FMAPST82083227(FMAPST82083227_frame);
void FMAPTP323231562(FMAPTP323231562_frame);
void FMAPTP702226833(FMAPTP702226833_frame);
void FMAPdO701667859(FMAPdO701667859_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL FMAPST800079349(FMAPSTRINT self, STR k) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRINT key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRINT key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRINT is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRINT key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRINT L71_;
 TUPSTRINT L8;
 INT L91_;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 BOOL L141_;
 TUPSTRINT L151_;
 TUPSTRINT L16;
 INT L171_;
 if ((self==((FMAPSTRINT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRINT)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L91_=INTPLUS(h,1); 
  h = L91_;
 }
 after_loop: ;
 L101_=ASIZE((FMAPSTRINT)self); 
 L11 = L101_;
 L121_=INTMINUS(L11,1); 
 L13 = L121_;
 L141_=(h)==(L13); 
 if (L141_) {
  h = 0;
  while (1) {
   L151_=ARR((FMAPSTRINT)self,h); 
   L16=L151_;
   tk1 = L16.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
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

BOOL FMAPdO1742903675(FMAPdOBNAMESPACE self) {
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
 L21_=ASIZE((FMAPdOBNAMESPACE)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FMAPdOBNAMESPACE)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FMAPdO802731536;
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

FMAPSTRFSETSTR FMAPST1555523727(FMAPSTRFSETSTR self, INT n) {
 FMAPSTRFSETSTR ret_val;
 FMAPSTRFSETSTR r;
 FMAPSTRFSETSTR L11;
 FMAPSTRFSETSTR key_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self;
 STR ret_val2;
 STR e = ((STR) NULL);
 FMAPSTRFSETSTR key_nil_self1;
 STR ret_val3;
 ELT_NILSTR elt_nil_self1;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 TUPSTRFSETSTR create_self = TUPSTRFSETSTR_zero;
 STR create_at1;
 FSETSTR create_at2;
 TUPSTRFSETSTR ret_val5 = TUPSTRFSETSTR_zero;
 INT L21 = INT_zero;
 FMAPSTRFSETSTR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSTRFSETSTR L10;
 TUPSTRFSETSTR L12;
 TUPSTRFSETSTR L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSTRFSETSTR_struct)-sizeof(TUPSTRFSETSTR))+(L4)*sizeof(TUPSTRFSETSTR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSTRFSETSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSTRFSETSTR) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSTR) NULL);
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
   L91_br=ASIZE((FMAPSTRFSETSTR)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSTRFSETSTR_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FSETSTR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSTRFSETSTR)L11,L21,L13); 
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

FMAPSTRFSETSTR FMAPST1602224682(FMAPSTRFSETSTR self) {
 FMAPSTRFSETSTR ret_val;
 INT ns = INT_zero;
 FMAPSTRFSETSTR r;
 FMAPSTRFSETSTR L11;
 FMAPSTRFSETSTR insert_pair_self;
 TUPSTRFSETSTR insert_pair_p = TUPSTRFSETSTR_zero;
 FMAPSTRFSETSTR ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSTRFSETSTR L9;
 L21_=ASIZE((FMAPSTRFSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPST1555523727(self, ns);
 {
  struct FMAPST1155259492_frame_struct other1_0;
FMAPST1155259492_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPST1155259492(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPST935488038(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSTRFSETSTR FMAPST935488038(FMAPSTRFSETSTR self, STR k, FSETSTR t) {
 FMAPSTRFSETSTR ret_val;
 FMAPSTRFSETSTR r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRFSETSTR should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSTRFSETSTR key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSTRFSETSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSTRFSETSTR key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPSTRFSETSTR create_self = TUPSTRFSETSTR_zero;
 STR create_at1;
 FSETSTR create_at2;
 TUPSTRFSETSTR ret_val7 = TUPSTRFSETSTR_zero;
 FMAPSTRFSETSTR is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPSTRFSETSTR key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPSTRFSETSTR create_self1 = TUPSTRFSETSTR_zero;
 STR create_at11;
 FSETSTR create_at21;
 TUPSTRFSETSTR ret_val12 = TUPSTRFSETSTR_zero;
 TUPSTRFSETSTR create_self2 = TUPSTRFSETSTR_zero;
 STR create_at12;
 FSETSTR create_at22;
 TUPSTRFSETSTR ret_val13 = TUPSTRFSETSTR_zero;
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
 TUPSTRFSETSTR L201_;
 TUPSTRFSETSTR L21;
 TUPSTRFSETSTR L22;
 TUPSTRFSETSTR L23;
 TUPSTRFSETSTR L24;
 INT L261_;
 BOOL L271_;
 TUPSTRFSETSTR L281_;
 TUPSTRFSETSTR L29;
 TUPSTRFSETSTR L30;
 TUPSTRFSETSTR L31;
 TUPSTRFSETSTR L32;
 INT L341_;
 TUPSTRFSETSTR L35;
 TUPSTRFSETSTR L36;
 TUPSTRFSETSTR L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSTRFSETSTR) NULL))) {
  r = FMAPST1555523727(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSTRFSETSTR)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPST1192706365;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPST1602224682(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSTRFSETSTR)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSTRFSETSTR)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSTRFSETSTR)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((STR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSTR) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = STR_is111530248(elt_eq_e1, elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSTRFSETSTR_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPSTRFSETSTR)r,h,L24); 
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
   L281_=ARR((FMAPSTRFSETSTR)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((STR) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPSTRFSETSTR_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPSTRFSETSTR)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSTRFSETSTR_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPSTRFSETSTR)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRINT FMAPST1221291526(FMAPSTRINT self) {
 FMAPSTRINT ret_val;
 INT ns = INT_zero;
 FMAPSTRINT r;
 FMAPSTRINT L11;
 FMAPSTRINT insert_pair_self;
 TUPSTRINT insert_pair_p = TUPSTRINT_zero;
 FMAPSTRINT ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSTRINT L9;
 L21_=ASIZE((FMAPSTRINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPST1767801431(self, ns);
 {
  struct FMAPST864901056_frame_struct other1_0;
FMAPST864901056_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPST864901056(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPST951578658(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSTRINT FMAPST1767801431(FMAPSTRINT self, INT n) {
 FMAPSTRINT ret_val;
 FMAPSTRINT r;
 FMAPSTRINT L11;
 FMAPSTRINT key_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self;
 STR ret_val2;
 STR e = ((STR) NULL);
 FMAPSTRINT key_nil_self1;
 STR ret_val3;
 ELT_NILSTR elt_nil_self1;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 TUPSTRINT create_self = TUPSTRINT_zero;
 STR create_at1;
 INT create_at2 = INT_zero;
 TUPSTRINT ret_val5 = TUPSTRINT_zero;
 INT L21 = INT_zero;
 FMAPSTRINT L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSTRINT L10;
 TUPSTRINT L12;
 TUPSTRINT L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSTRINT_struct)-sizeof(TUPSTRINT))+(L4)*sizeof(TUPSTRINT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSTRINT_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSTRINT) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSTR) NULL);
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
   L91_br=ASIZE((FMAPSTRINT)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSTRINT_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = INT_zero;
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSTRINT)L11,L21,L13); 
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

FMAPSTRINT FMAPST951578658(FMAPSTRINT self, STR k, INT t) {
 FMAPSTRINT ret_val;
 FMAPSTRINT r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRINT should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSTRINT key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSTRINT key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPSTRINT create_self = TUPSTRINT_zero;
 STR create_at1;
 INT create_at2 = INT_zero;
 TUPSTRINT ret_val7 = TUPSTRINT_zero;
 FMAPSTRINT is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPSTRINT key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPSTRINT create_self1 = TUPSTRINT_zero;
 STR create_at11;
 INT create_at21 = INT_zero;
 TUPSTRINT ret_val12 = TUPSTRINT_zero;
 TUPSTRINT create_self2 = TUPSTRINT_zero;
 STR create_at12;
 INT create_at22 = INT_zero;
 TUPSTRINT ret_val13 = TUPSTRINT_zero;
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
 TUPSTRINT L201_;
 TUPSTRINT L21;
 TUPSTRINT L22;
 TUPSTRINT L23;
 TUPSTRINT L24;
 INT L261_;
 BOOL L271_;
 TUPSTRINT L281_;
 TUPSTRINT L29;
 TUPSTRINT L30;
 TUPSTRINT L31;
 TUPSTRINT L32;
 INT L341_;
 TUPSTRINT L35;
 TUPSTRINT L36;
 TUPSTRINT L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSTRINT) NULL))) {
  r = FMAPST1767801431(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSTRINT)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPST47057045;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPST1221291526(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSTRINT)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSTRINT)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSTRINT)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((STR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSTR) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = STR_is111530248(elt_eq_e1, elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSTRINT_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPSTRINT)r,h,L24); 
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
   L281_=ARR((FMAPSTRINT)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((STR) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPSTRINT_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPSTRINT)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSTRINT_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPSTRINT)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRSIG FMAPST1822785341(FMAPSTRSIG self, INT n) {
 FMAPSTRSIG ret_val;
 FMAPSTRSIG r;
 FMAPSTRSIG L11;
 FMAPSTRSIG key_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self;
 STR ret_val2;
 STR e = ((STR) NULL);
 FMAPSTRSIG key_nil_self1;
 STR ret_val3;
 ELT_NILSTR elt_nil_self1;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 TUPSTRSIG create_self = TUPSTRSIG_zero;
 STR create_at1;
 SIG create_at2;
 TUPSTRSIG ret_val5 = TUPSTRSIG_zero;
 INT L21 = INT_zero;
 FMAPSTRSIG L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSTRSIG L10;
 TUPSTRSIG L12;
 TUPSTRSIG L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSTRSIG_struct)-sizeof(TUPSTRSIG))+(L4)*sizeof(TUPSTRSIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSTRSIG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSTRSIG) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSTR) NULL);
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
   L91_br=ASIZE((FMAPSTRSIG)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSTRSIG_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((SIG) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSTRSIG)L11,L21,L13); 
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

FMAPSTRSIG FMAPST451382576(FMAPSTRSIG self, STR k, SIG t) {
 FMAPSTRSIG ret_val;
 FMAPSTRSIG r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRSIG should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSTRSIG key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSTRSIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSTRSIG key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPSTRSIG create_self = TUPSTRSIG_zero;
 STR create_at1;
 SIG create_at2;
 TUPSTRSIG ret_val7 = TUPSTRSIG_zero;
 FMAPSTRSIG is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPSTRSIG key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPSTRSIG create_self1 = TUPSTRSIG_zero;
 STR create_at11;
 SIG create_at21;
 TUPSTRSIG ret_val12 = TUPSTRSIG_zero;
 TUPSTRSIG create_self2 = TUPSTRSIG_zero;
 STR create_at12;
 SIG create_at22;
 TUPSTRSIG ret_val13 = TUPSTRSIG_zero;
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
 TUPSTRSIG L201_;
 TUPSTRSIG L21;
 TUPSTRSIG L22;
 TUPSTRSIG L23;
 TUPSTRSIG L24;
 INT L261_;
 BOOL L271_;
 TUPSTRSIG L281_;
 TUPSTRSIG L29;
 TUPSTRSIG L30;
 TUPSTRSIG L31;
 TUPSTRSIG L32;
 INT L341_;
 TUPSTRSIG L35;
 TUPSTRSIG L36;
 TUPSTRSIG L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSTRSIG) NULL))) {
  r = FMAPST1822785341(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSTRSIG)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPST1449505507;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPST850176290(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSTRSIG)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSTRSIG)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSTRSIG)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((STR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSTR) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = STR_is111530248(elt_eq_e1, elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSTRSIG_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPSTRSIG)r,h,L24); 
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
   L281_=ARR((FMAPSTRSIG)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((STR) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPSTRSIG_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPSTRSIG)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSTRSIG_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPSTRSIG)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRSIG FMAPST850176290(FMAPSTRSIG self) {
 FMAPSTRSIG ret_val;
 INT ns = INT_zero;
 FMAPSTRSIG r;
 FMAPSTRSIG L11;
 FMAPSTRSIG insert_pair_self;
 TUPSTRSIG insert_pair_p = TUPSTRSIG_zero;
 FMAPSTRSIG ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSTRSIG L9;
 L21_=ASIZE((FMAPSTRSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPST1822785341(self, ns);
 {
  struct FMAPST1841912328_frame_struct other1_0;
FMAPST1841912328_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPST1841912328(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPST451382576(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSTRSTR FMAPST274513051(FMAPSTRSTR self, INT n) {
 FMAPSTRSTR ret_val;
 FMAPSTRSTR r;
 FMAPSTRSTR L11;
 FMAPSTRSTR key_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self;
 STR ret_val2;
 STR e = ((STR) NULL);
 FMAPSTRSTR key_nil_self1;
 STR ret_val3;
 ELT_NILSTR elt_nil_self1;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 TUPSTRSTR create_self = TUPSTRSTR_zero;
 STR create_at1;
 STR create_at2;
 TUPSTRSTR ret_val5 = TUPSTRSTR_zero;
 INT L21 = INT_zero;
 FMAPSTRSTR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSTRSTR L10;
 TUPSTRSTR L12;
 TUPSTRSTR L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSTRSTR_struct)-sizeof(TUPSTRSTR))+(L4)*sizeof(TUPSTRSTR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSTRSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSTRSTR) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSTR) NULL);
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
   L91_br=ASIZE((FMAPSTRSTR)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSTRSTR_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((STR) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSTRSTR)L11,L21,L13); 
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

FMAPSTRSTR FMAPST395749006(FMAPSTRSTR self) {
 FMAPSTRSTR ret_val;
 INT ns = INT_zero;
 FMAPSTRSTR r;
 FMAPSTRSTR L11;
 FMAPSTRSTR insert_pair_self;
 TUPSTRSTR insert_pair_p = TUPSTRSTR_zero;
 FMAPSTRSTR ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSTRSTR L9;
 L21_=ASIZE((FMAPSTRSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPST274513051(self, ns);
 {
  struct FMAPST1934702696_frame_struct other1_0;
FMAPST1934702696_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPST1934702696(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPST859022540(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPSTRSTR FMAPST859022540(FMAPSTRSTR self, STR k, STR t) {
 FMAPSTRSTR ret_val;
 FMAPSTRSTR r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRSTR should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSTRSTR key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSTRSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSTRSTR key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPSTRSTR create_self = TUPSTRSTR_zero;
 STR create_at1;
 STR create_at2;
 TUPSTRSTR ret_val7 = TUPSTRSTR_zero;
 FMAPSTRSTR is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPSTRSTR key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPSTRSTR create_self1 = TUPSTRSTR_zero;
 STR create_at11;
 STR create_at21;
 TUPSTRSTR ret_val12 = TUPSTRSTR_zero;
 TUPSTRSTR create_self2 = TUPSTRSTR_zero;
 STR create_at12;
 STR create_at22;
 TUPSTRSTR ret_val13 = TUPSTRSTR_zero;
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
 TUPSTRSTR L201_;
 TUPSTRSTR L21;
 TUPSTRSTR L22;
 TUPSTRSTR L23;
 TUPSTRSTR L24;
 INT L261_;
 BOOL L271_;
 TUPSTRSTR L281_;
 TUPSTRSTR L29;
 TUPSTRSTR L30;
 TUPSTRSTR L31;
 TUPSTRSTR L32;
 INT L341_;
 TUPSTRSTR L35;
 TUPSTRSTR L36;
 TUPSTRSTR L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSTRSTR) NULL))) {
  r = FMAPST274513051(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSTRSTR)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPST2041121537;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPST395749006(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSTRSTR)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSTRSTR)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSTRSTR)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((STR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSTR) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = STR_is111530248(elt_eq_e1, elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSTRSTR_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPSTRSTR)r,h,L24); 
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
   L281_=ARR((FMAPSTRSTR)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((STR) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPSTRSTR_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPSTRSTR)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSTRSTR_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPSTRSTR)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPTP291739594 FMAPTP2133001945(FMAPTP291739594 self) {
 FMAPTP291739594 ret_val;
 INT ns = INT_zero;
 FMAPTP291739594 r;
 FMAPTP291739594 L11;
 FMAPTP291739594 insert_pair_self;
 TUPTP_858321267 insert_pair_p = TUPTP_858321267_zero;
 FMAPTP291739594 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPTP_858321267 L9;
 L21_=ASIZE((FMAPTP291739594)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPTP786961404(self, ns);
 {
  struct FMAPTP1465407463_frame_struct other1_0;
FMAPTP1465407463_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPTP1465407463(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPTP551118428(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPTP291739594 FMAPTP551118428(FMAPTP291739594 self, TP_CLASS k, FSETdTP t) {
 FMAPTP291739594 ret_val;
 FMAPTP291739594 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP291739594 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPTP291739594 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPTP291739594 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val7 = BOOL_zero;
 TUPTP_858321267 create_self = TUPTP_858321267_zero;
 TP_CLASS create_at1;
 FSETdTP create_at2;
 TUPTP_858321267 ret_val8 = TUPTP_858321267_zero;
 FMAPTP291739594 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPTP291739594 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val13 = BOOL_zero;
 TUPTP_858321267 create_self1 = TUPTP_858321267_zero;
 TP_CLASS create_at11;
 FSETdTP create_at21;
 TUPTP_858321267 ret_val14 = TUPTP_858321267_zero;
 TUPTP_858321267 create_self2 = TUPTP_858321267_zero;
 TP_CLASS create_at12;
 FSETdTP create_at22;
 TUPTP_858321267 ret_val15 = TUPTP_858321267_zero;
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
 TUPTP_858321267 L201_;
 TUPTP_858321267 L21;
 TUPTP_858321267 L22;
 TUPTP_858321267 L23;
 TUPTP_858321267 L24;
 INT L261_;
 BOOL L271_;
 TUPTP_858321267 L281_;
 TUPTP_858321267 L29;
 TUPTP_858321267 L30;
 TUPTP_858321267 L31;
 TUPTP_858321267 L32;
 INT L341_;
 TUPTP_858321267 L35;
 TUPTP_858321267 L36;
 TUPTP_858321267 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPTP291739594) NULL))) {
  r = FMAPTP786961404(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPTP291739594)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPTP676284949;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPTP2133001945(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPTP291739594)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPTP291739594)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPTP291739594)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQTP_CLASS) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_t = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPTP_858321267_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPTP291739594)r,h,L24); 
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
   L281_=ARR((FMAPTP291739594)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_t1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPTP_858321267_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPTP291739594)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPTP_858321267_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPTP291739594)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPTP291739594 FMAPTP786961404(FMAPTP291739594 self, INT n) {
 FMAPTP291739594 ret_val;
 FMAPTP291739594 r;
 FMAPTP291739594 L11;
 FMAPTP291739594 key_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 FMAPTP291739594 key_nil_self1;
 TP_CLASS ret_val3;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val4;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 TUPTP_858321267 create_self = TUPTP_858321267_zero;
 TP_CLASS create_at1;
 FSETdTP create_at2;
 TUPTP_858321267 ret_val5 = TUPTP_858321267_zero;
 INT L21 = INT_zero;
 FMAPTP291739594 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPTP_858321267 L10;
 TUPTP_858321267 L12;
 TUPTP_858321267 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPTP291739594_struct)-sizeof(TUPTP_858321267))+(L4)*sizeof(TUPTP_858321267);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPTP291739594_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPTP291739594) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILTP_CLASS) NULL);
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
   L91_br=ASIZE((FMAPTP291739594)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPTP_858321267_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
    ret_val4 = ((TP_CLASS) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FSETdTP) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPTP291739594)L11,L21,L13); 
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

FMAPTP775297600 FMAPTP1339820925(FMAPTP775297600 self, INT n) {
 FMAPTP775297600 ret_val;
 FMAPTP775297600 r;
 FMAPTP775297600 L11;
 FMAPTP775297600 key_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 FMAPTP775297600 key_nil_self1;
 TP_CLASS ret_val3;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val4;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 TUPTP_1668454172 create_self = TUPTP_1668454172_zero;
 TP_CLASS create_at1;
 FSETTP_CLASS create_at2;
 TUPTP_1668454172 ret_val5 = TUPTP_1668454172_zero;
 INT L21 = INT_zero;
 FMAPTP775297600 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPTP_1668454172 L10;
 TUPTP_1668454172 L12;
 TUPTP_1668454172 L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPTP775297600_struct)-sizeof(TUPTP_1668454172))+(L4)*sizeof(TUPTP_1668454172);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPTP775297600_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPTP775297600) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILTP_CLASS) NULL);
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
   L91_br=ASIZE((FMAPTP775297600)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPTP_1668454172_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
    ret_val4 = ((TP_CLASS) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((FSETTP_CLASS) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPTP775297600)L11,L21,L13); 
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

FMAPTP775297600 FMAPTP148513176(FMAPTP775297600 self) {
 FMAPTP775297600 ret_val;
 INT ns = INT_zero;
 FMAPTP775297600 r;
 FMAPTP775297600 L11;
 FMAPTP775297600 insert_pair_self;
 TUPTP_1668454172 insert_pair_p = TUPTP_1668454172_zero;
 FMAPTP775297600 ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPTP_1668454172 L9;
 L21_=ASIZE((FMAPTP775297600)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPTP1339820925(self, ns);
 {
  struct FMAPTP1065055386_frame_struct other1_0;
FMAPTP1065055386_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPTP1065055386(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPTP594824795(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPTP775297600 FMAPTP594824795(FMAPTP775297600 self, TP_CLASS k, FSETTP_CLASS t) {
 FMAPTP775297600 ret_val;
 FMAPTP775297600 r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP775297600 should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPTP775297600 key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPTP775297600 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val7 = BOOL_zero;
 TUPTP_1668454172 create_self = TUPTP_1668454172_zero;
 TP_CLASS create_at1;
 FSETTP_CLASS create_at2;
 TUPTP_1668454172 ret_val8 = TUPTP_1668454172_zero;
 FMAPTP775297600 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 FMAPTP775297600 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val13 = BOOL_zero;
 TUPTP_1668454172 create_self1 = TUPTP_1668454172_zero;
 TP_CLASS create_at11;
 FSETTP_CLASS create_at21;
 TUPTP_1668454172 ret_val14 = TUPTP_1668454172_zero;
 TUPTP_1668454172 create_self2 = TUPTP_1668454172_zero;
 TP_CLASS create_at12;
 FSETTP_CLASS create_at22;
 TUPTP_1668454172 ret_val15 = TUPTP_1668454172_zero;
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
 TUPTP_1668454172 L201_;
 TUPTP_1668454172 L21;
 TUPTP_1668454172 L22;
 TUPTP_1668454172 L23;
 TUPTP_1668454172 L24;
 INT L261_;
 BOOL L271_;
 TUPTP_1668454172 L281_;
 TUPTP_1668454172 L29;
 TUPTP_1668454172 L30;
 TUPTP_1668454172 L31;
 TUPTP_1668454172 L32;
 INT L341_;
 TUPTP_1668454172 L35;
 TUPTP_1668454172 L36;
 TUPTP_1668454172 L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPTP775297600) NULL))) {
  r = FMAPTP1339820925(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPTP775297600)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPTP642805238;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPTP148513176(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPTP775297600)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPTP775297600)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPTP775297600)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQTP_CLASS) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  is_eq_self = elt_eq_e1;
  is_eq_t = ((OB) elt_eq_e2);
  ret_val7 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
  ret_val6 = ret_val7;
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPTP_1668454172_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val8 = L23;
   L24 = ret_val8;
   SARR((FMAPTP775297600)r,h,L24); 
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
   L281_=ARR((FMAPTP775297600)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val10 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   is_eq_self1 = elt_eq_e11;
   is_eq_t1 = ((OB) elt_eq_e21);
   ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
   ret_val12 = ret_val13;
   ret_val11 = ret_val12;
   if (ret_val11) {
    create_self1 = TUPTP_1668454172_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val14 = L31;
    L32 = ret_val14;
    SARR((FMAPTP775297600)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPTP_1668454172_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val15 = L36;
 L37 = ret_val15;
 SARR((FMAPTP775297600)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE self, OB k, NAMESPACE t) {
 FMAPdOBNAMESPACE ret_val;
 FMAPdOBNAMESPACE r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 OB tk;
 OB tk1;
 FMAPdOBNAMESPACE should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPdOBNAMESPACE key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPdOBNAMESPACE is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPdOBNAMESPACE key_eq_self;
 OB key_eq_e1;
 OB key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 TUPdOBNAMESPACE create_self = TUPdOBNAMESPACE_zero;
 OB create_at1;
 NAMESPACE create_at2;
 TUPdOBNAMESPACE ret_val5 = TUPdOBNAMESPACE_zero;
 FMAPdOBNAMESPACE is_key_nil_self1;
 OB is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 FMAPdOBNAMESPACE key_eq_self1;
 OB key_eq_e11;
 OB key_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 TUPdOBNAMESPACE create_self1 = TUPdOBNAMESPACE_zero;
 OB create_at11;
 NAMESPACE create_at21;
 TUPdOBNAMESPACE ret_val8 = TUPdOBNAMESPACE_zero;
 TUPdOBNAMESPACE create_self2 = TUPdOBNAMESPACE_zero;
 OB create_at12;
 NAMESPACE create_at22;
 TUPdOBNAMESPACE ret_val9 = TUPdOBNAMESPACE_zero;
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
 TUPdOBNAMESPACE L201_;
 TUPdOBNAMESPACE L21;
 TUPdOBNAMESPACE L22;
 TUPdOBNAMESPACE L23;
 TUPdOBNAMESPACE L24;
 INT L261_;
 BOOL L271_;
 TUPdOBNAMESPACE L281_;
 TUPdOBNAMESPACE L29;
 TUPdOBNAMESPACE L30;
 TUPdOBNAMESPACE L31;
 TUPdOBNAMESPACE L32;
 INT L341_;
 TUPdOBNAMESPACE L35;
 TUPdOBNAMESPACE L36;
 TUPdOBNAMESPACE L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPdOBNAMESPACE) NULL))) {
  r = FMAPdO1902821281(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPdOBNAMESPACE)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPdO802731536;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPdO899482984(self);
  }
 }
 key_hash_self = r;
 key_hash_e = k;
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPdOBNAMESPACE)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPdOBNAMESPACE)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPdOBNAMESPACE)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val3 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  ret_val4 = ELT_EQ936498558(((ELT_EQdOB) NULL), key_eq_e1, key_eq_e2);
  if (ret_val4) {
   create_self = TUPdOBNAMESPACE_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val5 = L23;
   L24 = ret_val5;
   SARR((FMAPdOBNAMESPACE)r,h,L24); 
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
   L281_=ARR((FMAPdOBNAMESPACE)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val6 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e1);
   if (ret_val6) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   ret_val7 = ELT_EQ936498558(((ELT_EQdOB) NULL), key_eq_e11, key_eq_e21);
   if (ret_val7) {
    create_self1 = TUPdOBNAMESPACE_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val8 = L31;
    L32 = ret_val8;
    SARR((FMAPdOBNAMESPACE)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPdOBNAMESPACE_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val9 = L36;
 L37 = ret_val9;
 SARR((FMAPdOBNAMESPACE)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPdOBNAMESPACE FMAPdO1864084602(FMAPdOBNAMESPACE self, OB k) {
 FMAPdOBNAMESPACE ret_val;
 INT h = INT_zero;
 OB tk;
 INT i = INT_zero;
 OB tk1;
 INT hsh = INT_zero;
 FMAPdOBNAMESPACE key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdOBNAMESPACE is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdOBNAMESPACE key_eq_self;
 OB key_eq_e1;
 OB key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 FMAPdOBNAMESPACE key_nil_self;
 OB ret_val4;
 TUPdOBNAMESPACE create_self = TUPdOBNAMESPACE_zero;
 OB create_at1;
 NAMESPACE create_at2;
 TUPdOBNAMESPACE ret_val5 = TUPdOBNAMESPACE_zero;
 FMAPdOBNAMESPACE is_key_nil_self1;
 OB is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 FMAPdOBNAMESPACE key_hash_self1;
 OB key_hash_e1;
 INT ret_val7 = INT_zero;
 FMAPdOBNAMESPACE key_nil_self1;
 OB ret_val8;
 TUPdOBNAMESPACE create_self1 = TUPdOBNAMESPACE_zero;
 OB create_at11;
 NAMESPACE create_at21;
 TUPdOBNAMESPACE ret_val9 = TUPdOBNAMESPACE_zero;
 FMAPdOBNAMESPACE key_nil_self2;
 OB ret_val10;
 TUPdOBNAMESPACE create_self2 = TUPdOBNAMESPACE_zero;
 OB create_at12;
 NAMESPACE create_at22;
 TUPdOBNAMESPACE ret_val11 = TUPdOBNAMESPACE_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdOBNAMESPACE L71_;
 TUPdOBNAMESPACE L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 INT L141_;
 TUPdOBNAMESPACE L15;
 TUPdOBNAMESPACE L16;
 TUPdOBNAMESPACE L17;
 INT L19;
 INT L201_;
 INT L221_;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 INT L271_;
 TUPdOBNAMESPACE L281_;
 TUPdOBNAMESPACE L29;
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
 TUPdOBNAMESPACE L451_;
 TUPdOBNAMESPACE L46;
 TUPdOBNAMESPACE L48;
 TUPdOBNAMESPACE L49;
 TUPdOBNAMESPACE L50;
 BOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 BOOL L561_;
 TUPdOBNAMESPACE L571_;
 TUPdOBNAMESPACE L58;
 TUPdOBNAMESPACE L60;
 TUPdOBNAMESPACE L62;
 TUPdOBNAMESPACE L63;
 if ((self==((FMAPdOBNAMESPACE) NULL))) {
  ret_val = ((FMAPdOBNAMESPACE) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = k;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdOBNAMESPACE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdOBNAMESPACE)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   ret_val3 = ELT_EQ936498558(((ELT_EQdOB) NULL), key_eq_e1, key_eq_e2);
   if (ret_val3) {
    goto after_loop;
   }
  }
  L91_=ASIZE((FMAPdOBNAMESPACE)self); 
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
 create_self = TUPdOBNAMESPACE_zero;
 key_nil_self = self;
 ret_val4 = ELT_NI509824640(((ELT_NILdOB) NULL));
 create_at1 = ret_val4;
 create_at2 = ((NAMESPACE) NULL);
 L15 = create_self;
 L15.t1 = create_at1;
 L16 = L15;
 L16.t2 = create_at2;
 ret_val5 = L16;
 L17 = ret_val5;
 SARR((FMAPdOBNAMESPACE)self,h,L17); 
 ;
 L19 = ATTR(self,hsize);
 L201_=INTMINUS(L19,1); 
 SATTR(self,hsize,L201_);
 i = h;
 while (1) {
  L221_=ASIZE((FMAPdOBNAMESPACE)self); 
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
  L281_=ARR((FMAPdOBNAMESPACE)self,i); 
  L29=L281_;
  tk1 = L29.t1;
  is_key_nil_self1 = self;
  is_key_nil_e1 = tk1;
  ret_val6 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e1);
  if (ret_val6) {
   goto after_loop1;
  }
  key_hash_self1 = self;
  key_hash_e1 = tk1;
  ret_val7 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e1);
  L30 = ret_val7;
  L311_=ASIZE((FMAPdOBNAMESPACE)self); 
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
    L451_=ARR((FMAPdOBNAMESPACE)self,i); 
    L46 = L451_;
    SARR((FMAPdOBNAMESPACE)self,h,L46); 
    ;
    h = i;
    create_self1 = TUPdOBNAMESPACE_zero;
    key_nil_self1 = self;
    ret_val8 = ELT_NI509824640(((ELT_NILdOB) NULL));
    create_at11 = ret_val8;
    create_at21 = ((NAMESPACE) NULL);
    L48 = create_self1;
    L48.t1 = create_at11;
    L49 = L48;
    L49.t2 = create_at21;
    ret_val9 = L49;
    L50 = ret_val9;
    SARR((FMAPdOBNAMESPACE)self,i,L50); 
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
    L571_=ARR((FMAPdOBNAMESPACE)self,i); 
    L58 = L571_;
    SARR((FMAPdOBNAMESPACE)self,h,L58); 
    ;
    h = i;
    create_self2 = TUPdOBNAMESPACE_zero;
    key_nil_self2 = self;
    ret_val10 = ELT_NI509824640(((ELT_NILdOB) NULL));
    create_at12 = ret_val10;
    create_at22 = ((NAMESPACE) NULL);
    L60 = create_self2;
    L60.t1 = create_at12;
    L62 = L60;
    L62.t2 = create_at22;
    ret_val11 = L62;
    L63 = ret_val11;
    SARR((FMAPdOBNAMESPACE)self,i,L63); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FMAPdO1742903675(self)) {
  ret_val = FMAPdO588587769(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FMAPdOBNAMESPACE FMAPdO1902821281(FMAPdOBNAMESPACE self, INT n) {
 FMAPdOBNAMESPACE ret_val;
 FMAPdOBNAMESPACE r;
 FMAPdOBNAMESPACE L11;
 FMAPdOBNAMESPACE key_nil_self;
 OB ret_val1;
 FMAPdOBNAMESPACE key_nil_self1;
 OB ret_val2;
 TUPdOBNAMESPACE create_self = TUPdOBNAMESPACE_zero;
 OB create_at1;
 NAMESPACE create_at2;
 TUPdOBNAMESPACE ret_val3 = TUPdOBNAMESPACE_zero;
 INT L21 = INT_zero;
 FMAPdOBNAMESPACE L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPdOBNAMESPACE L10;
 TUPdOBNAMESPACE L12;
 TUPdOBNAMESPACE L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPdOBNAMESPACE_struct)-sizeof(TUPdOBNAMESPACE))+(L4)*sizeof(TUPdOBNAMESPACE);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPdOBNAMESPACE_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPdOBNAMESPACE) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 ret_val1 = ELT_NI509824640(((ELT_NILdOB) NULL));
 L7 = (ret_val1==((OB) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPdOBNAMESPACE)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPdOBNAMESPACE_zero;
    key_nil_self1 = self;
    ret_val2 = ELT_NI509824640(((ELT_NILdOB) NULL));
    create_at1 = ret_val2;
    create_at2 = ((NAMESPACE) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val3 = L12;
    L13 = ret_val3;
    SARR((FMAPdOBNAMESPACE)L11,L21,L13); 
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

FMAPdOBNAMESPACE FMAPdO588587769(FMAPdOBNAMESPACE self) {
 FMAPdOBNAMESPACE ret_val;
 INT ns = INT_zero;
 FMAPdOBNAMESPACE r;
 FMAPdOBNAMESPACE L11;
 FMAPdOBNAMESPACE insert_pair_self;
 TUPdOBNAMESPACE insert_pair_p = TUPdOBNAMESPACE_zero;
 FMAPdOBNAMESPACE ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdOBNAMESPACE L9;
 L21_=ASIZE((FMAPdOBNAMESPACE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdO1902821281(self, ns);
 {
  struct FMAPdO1025347514_frame_struct other1_0;
FMAPdO1025347514_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdO1025347514(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdO1199466663(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FMAPdOBNAMESPACE FMAPdO899482984(FMAPdOBNAMESPACE self) {
 FMAPdOBNAMESPACE ret_val;
 INT ns = INT_zero;
 FMAPdOBNAMESPACE r;
 FMAPdOBNAMESPACE L11;
 FMAPdOBNAMESPACE insert_pair_self;
 TUPdOBNAMESPACE insert_pair_p = TUPdOBNAMESPACE_zero;
 FMAPdOBNAMESPACE ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPdOBNAMESPACE L9;
 L21_=ASIZE((FMAPdOBNAMESPACE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPdO1902821281(self, ns);
 {
  struct FMAPdO1025347514_frame_struct other1_0;
FMAPdO1025347514_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPdO1025347514(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPdO1199466663(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
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

FSETSTR FMAPST521088757(FMAPSTRFSETSTR self, STR k) {
 FSETSTR ret_val;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRFSETSTR key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRFSETSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRFSETSTR key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRFSETSTR is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRFSETSTR key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRFSETSTR L71_;
 TUPSTRFSETSTR L8;
 TUPSTRFSETSTR L91_;
 TUPSTRFSETSTR L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSTRFSETSTR L171_;
 TUPSTRFSETSTR L18;
 TUPSTRFSETSTR L191_;
 TUPSTRFSETSTR L20;
 INT L221_;
 if ((self==((FMAPSTRFSETSTR) NULL))) {
  ret_val = ((FSETSTR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRFSETSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRFSETSTR)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSTRFSETSTR)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSTRFSETSTR)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSTRFSETSTR)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPSTRFSETSTR)self,h); 
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
 ret_val = ((FSETSTR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETTP_CLASS FMAPTP2096448595(FMAPTP775297600 self, TP_CLASS k) {
 FSETTP_CLASS ret_val;
 INT h = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP775297600 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPTP775297600 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val6 = BOOL_zero;
 FMAPTP775297600 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPTP775297600 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
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
 TUPTP_1668454172 L71_;
 TUPTP_1668454172 L8;
 TUPTP_1668454172 L91_;
 TUPTP_1668454172 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPTP_1668454172 L171_;
 TUPTP_1668454172 L18;
 TUPTP_1668454172 L191_;
 TUPTP_1668454172 L20;
 INT L221_;
 if ((self==((FMAPTP775297600) NULL))) {
  ret_val = ((FSETTP_CLASS) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPTP775297600)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPTP775297600)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_t = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPTP775297600)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPTP775297600)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPTP775297600)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_t1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPTP775297600)self,h); 
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
 ret_val = ((FSETTP_CLASS) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETdTP FMAPTP861983338(FMAPTP291739594 self, TP_CLASS k) {
 FSETdTP ret_val;
 INT h = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP291739594 key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPTP291739594 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val6 = BOOL_zero;
 FMAPTP291739594 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 FMAPTP291739594 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
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
 TUPTP_858321267 L71_;
 TUPTP_858321267 L8;
 TUPTP_858321267 L91_;
 TUPTP_858321267 L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPTP_858321267 L171_;
 TUPTP_858321267 L18;
 TUPTP_858321267 L191_;
 TUPTP_858321267 L20;
 INT L221_;
 if ((self==((FMAPTP291739594) NULL))) {
  ret_val = ((FSETdTP) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPTP291739594)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPTP291739594)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_t = ((OB) elt_eq_e2);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPTP291739594)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPTP291739594)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPTP291739594)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val8 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_t1 = ((OB) elt_eq_e21);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     L191_=ARR((FMAPTP291739594)self,h); 
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
 ret_val = ((FSETdTP) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FMAPST1755966813(FMAPSTRINT self, STR k) {
 INT ret_val = INT_zero;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRINT key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRINT key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRINT is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRINT key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRINT L71_;
 TUPSTRINT L8;
 TUPSTRINT L91_;
 TUPSTRINT L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSTRINT L171_;
 TUPSTRINT L18;
 TUPSTRINT L191_;
 TUPSTRINT L20;
 INT L221_;
 if ((self==((FMAPSTRINT) NULL))) {
  ret_val = INT_zero;
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRINT)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRINT)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSTRINT)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSTRINT)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSTRINT)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPSTRINT)self,h); 
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
 ret_val = INT_zero;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FMAPST868850642(FMAPSTRINT self) {
 INT ret_val = INT_zero;
 if ((self==((FMAPSTRINT) NULL))) {
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

NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE self, OB k) {
 NAMESPACE ret_val;
 INT h = INT_zero;
 OB tk;
 OB tk1;
 FMAPdOBNAMESPACE key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPdOBNAMESPACE is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FMAPdOBNAMESPACE key_eq_self;
 OB key_eq_e1;
 OB key_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 FMAPdOBNAMESPACE is_key_nil_self1;
 OB is_key_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 FMAPdOBNAMESPACE key_eq_self1;
 OB key_eq_e11;
 OB key_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPdOBNAMESPACE L71_;
 TUPdOBNAMESPACE L8;
 TUPdOBNAMESPACE L91_;
 TUPdOBNAMESPACE L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPdOBNAMESPACE L171_;
 TUPdOBNAMESPACE L18;
 TUPdOBNAMESPACE L191_;
 TUPdOBNAMESPACE L20;
 INT L221_;
 if ((self==((FMAPdOBNAMESPACE) NULL))) {
  ret_val = ((NAMESPACE) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = k;
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPdOBNAMESPACE)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPdOBNAMESPACE)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  ret_val2 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   ret_val3 = ELT_EQ936498558(((ELT_EQdOB) NULL), key_eq_e1, key_eq_e2);
   if (ret_val3) {
    L91_=ARR((FMAPdOBNAMESPACE)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPdOBNAMESPACE)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPdOBNAMESPACE)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   ret_val4 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e1);
   if (ret_val4) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    ret_val5 = ELT_EQ936498558(((ELT_EQdOB) NULL), key_eq_e11, key_eq_e21);
    if (ret_val5) {
     L191_=ARR((FMAPdOBNAMESPACE)self,h); 
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
 ret_val = ((NAMESPACE) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG FMAPST879183273(FMAPSTRSIG self, STR k) {
 SIG ret_val;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRSIG key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRSIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRSIG key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRSIG is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRSIG key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRSIG L71_;
 TUPSTRSIG L8;
 TUPSTRSIG L91_;
 TUPSTRSIG L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSTRSIG L171_;
 TUPSTRSIG L18;
 TUPSTRSIG L191_;
 TUPSTRSIG L20;
 INT L221_;
 if ((self==((FMAPSTRSIG) NULL))) {
  ret_val = ((SIG) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRSIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRSIG)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSTRSIG)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSTRSIG)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSTRSIG)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPSTRSIG)self,h); 
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
 ret_val = ((SIG) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPST1518383807(FMAPSTRSTR self, STR k) {
 STR ret_val;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRSTR key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRSTR key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRSTR is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRSTR key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRSTR L71_;
 TUPSTRSTR L8;
 TUPSTRSTR L91_;
 TUPSTRSTR L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSTRSTR L171_;
 TUPSTRSTR L18;
 TUPSTRSTR L191_;
 TUPSTRSTR L20;
 INT L221_;
 if ((self==((FMAPSTRSTR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRSTR)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRSTR)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSTRSTR)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSTRSTR)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSTRSTR)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPSTRSTR)self,h); 
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

STR FMAPST1581005786(FMAPSTRINT self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRINT p = TUPSTRINT_zero;
 STR k;
 INT e = INT_zero;
 FMAPSTRINT is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 INT str_self1 = INT_zero;
 STR ret_val4;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val5;
 FSTR str_self2;
 STR ret_val6;
 FSTR str_self3;
 STR ret_val7;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRINT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 BOOL L10;
 BOOL L121_;
 FSTR L13;
 extern STR name10;
 extern STR name10;
 INT L141_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSTRINT)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSTRINT)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((STR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   str_self = k;
   ret_val3 = str_self;
   L8 = FSTR_p1752847026(L7, ret_val3);
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   str_self1 = e;
   clear_self = buf1;
   L10 = (clear_self==((FSTR) NULL));
   L121_=!(L10); 
   if (L121_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self1;
   str_in_s = buf1;
   ret_val5 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val5;
   str_self2 = buf1;
   ret_val6 = STR_fr1097270334(((STR) NULL), str_self2);
   ret_val4 = ret_val6;
   L13 = FSTR_p1752847026(L9, ret_val4);
   res = FSTR_p1752847026(L13, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L141_=INTPLUS(i,1); 
  i = L141_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self3 = res;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self3);
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPST1670441508(FMAPSTRSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRSTR p = TUPSTRSTR_zero;
 STR k;
 STR e;
 FMAPSTRSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 STR str_self1;
 STR ret_val4;
 FSTR str_self2;
 STR ret_val5;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRSTR L41_;
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
  L11_=ASIZE((FMAPSTRSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSTRSTR)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((STR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   str_self = k;
   ret_val3 = str_self;
   L8 = FSTR_p1752847026(L7, ret_val3);
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   str_self1 = e;
   ret_val4 = str_self1;
   L10 = FSTR_p1752847026(L9, ret_val4);
   res = FSTR_p1752847026(L10, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L121_=INTPLUS(i,1); 
  i = L121_;
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

STR FMAPST1891365712(FMAPSTRSIG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRSIG p = TUPSTRSIG_zero;
 STR k;
 SIG e;
 FMAPSTRSIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRSIG L41_;
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
  L11_=ASIZE((FMAPSTRSIG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSTRSIG)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((STR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   str_self = k;
   ret_val3 = str_self;
   L8 = FSTR_p1752847026(L7, ret_val3);
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
 str_self1 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPST35426998(FMAPSTRFSETSTR self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRFSETSTR p = TUPSTRFSETSTR_zero;
 STR k;
 FSETSTR e;
 FMAPSTRFSETSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRFSETSTR L41_;
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
  L11_=ASIZE((FMAPSTRFSETSTR)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSTRFSETSTR)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((STR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   str_self = k;
   ret_val3 = str_self;
   L8 = FSTR_p1752847026(L7, ret_val3);
   res = FSTR_p1752847026(L8, ((STR) &name15));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L91_=INTPLUS(i,1); 
  i = L91_;
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

STR FMAPTP1432889353(FMAPTP291739594 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPTP_858321267 p = TUPTP_858321267_zero;
 TP_CLASS k;
 FSETdTP e;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPTP_858321267 L41_;
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
  L11_=ASIZE((FMAPTP291739594)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPTP291739594)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, TP_CLASS_strrSTR(k));
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

STR FMAPTP722426429(FMAPTP775297600 self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPTP_1668454172 p = TUPTP_1668454172_zero;
 TP_CLASS k;
 FSETTP_CLASS e;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 FSTR str_self;
 STR ret_val3;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPTP_1668454172 L41_;
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
  L11_=ASIZE((FMAPTP775297600)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPTP775297600)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   L8 = FSTR_p1752847026(L7, TP_CLASS_strrSTR(k));
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

STR FMAPdO570867591(FMAPdOBNAMESPACE self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPdOBNAMESPACE p = TUPdOBNAMESPACE_zero;
 OB k;
 NAMESPACE e;
 FMAPdOBNAMESPACE is_key_nil_self;
 OB is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 FSTR str_self;
 STR ret_val2;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPdOBNAMESPACE L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 dSTR L8;
 FSTR L9;
 extern STR name15;
 extern STR name10;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPdOBNAMESPACE)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPdOBNAMESPACE)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  ret_val1 = ELT_NI455285430(((ELT_NILdOB) NULL), is_key_nil_e);
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   if (k==NULL) {
    goto other91;
   } else
   switch (TAG(k)) {
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
     L7 = FSTR_p1752847026(res, ((STR) &name14));
     L8 = ((dSTR) k);
     L9 = FSTR_p1752847026(L7, (*dSTR_strrSTR[TAG(L8)])(L8));
     res = FSTR_p1752847026(L9, ((STR) &name15)); break;
    default: ;
    other91: ;
   }
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

TUPTP_1668454172 FMAPTP1108250174(FMAPTP775297600 self, TP_CLASS k) {
 TUPTP_1668454172 ret_val = TUPTP_1668454172_zero;
 INT h = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP775297600 key_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 TUPTP_1668454172 create_self = TUPTP_1668454172_zero;
 TP_CLASS create_at1;
 FSETTP_CLASS create_at2;
 TUPTP_1668454172 ret_val3 = TUPTP_1668454172_zero;
 FMAPTP775297600 key_hash_self;
 OB key_hash_e;
 INT ret_val4 = INT_zero;
 FMAPTP775297600 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val5 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val6 = BOOL_zero;
 FMAPTP775297600 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val9 = BOOL_zero;
 FMAPTP775297600 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val11 = BOOL_zero;
 FMAPTP775297600 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val13 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val14 = BOOL_zero;
 FMAPTP775297600 key_nil_self1;
 TP_CLASS ret_val15;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val16;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 TUPTP_1668454172 create_self1 = TUPTP_1668454172_zero;
 TP_CLASS create_at11;
 FSETTP_CLASS create_at21;
 TUPTP_1668454172 ret_val17 = TUPTP_1668454172_zero;
 TUPTP_1668454172 L1;
 TUPTP_1668454172 L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPTP_1668454172 L91_;
 TUPTP_1668454172 L10;
 TUPTP_1668454172 L111_;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPTP_1668454172 L181_;
 TUPTP_1668454172 L19;
 TUPTP_1668454172 L201_;
 INT L211_;
 TUPTP_1668454172 L22;
 TUPTP_1668454172 L23;
 if ((self==((FMAPTP775297600) NULL))) {
  create_self = TUPTP_1668454172_zero;
  key_nil_self = self;
  elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  ret_val2 = ((TP_CLASS) NULL);
  ret_val1 = ret_val2;
  create_at1 = ret_val1;
  create_at2 = ((FSETTP_CLASS) NULL);
  L1 = create_self;
  L1.t1 = create_at1;
  L2 = L1;
  L2.t2 = create_at2;
  ret_val3 = L2;
  ret_val = ret_val3;
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val4 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L3 = ret_val4;
 L41_=ASIZE((FMAPTP775297600)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FMAPTP775297600)self,h); 
  L10=L91_;
  tk = L10.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val6 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val5 = ret_val6;
  if (ret_val5) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_t = ((OB) elt_eq_e2);
   ret_val9 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val8 = ret_val9;
   ret_val7 = ret_val8;
   if (ret_val7) {
    L111_=ARR((FMAPTP775297600)self,h); 
    ret_val = L111_;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPTP775297600)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPTP775297600)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val11 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val10 = ret_val11;
   if (ret_val10) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_t1 = ((OB) elt_eq_e21);
    ret_val14 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val13 = ret_val14;
    ret_val12 = ret_val13;
    if (ret_val12) {
     L201_=ARR((FMAPTP775297600)self,h); 
     ret_val = L201_;
     return ret_val;
    }
   }
   L211_=INTPLUS(h,1); 
   h = L211_;
  }
  after_loop1: ;
 }
 create_self1 = TUPTP_1668454172_zero;
 key_nil_self1 = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val16 = ((TP_CLASS) NULL);
 ret_val15 = ret_val16;
 create_at11 = ret_val15;
 create_at21 = ((FSETTP_CLASS) NULL);
 L22 = create_self1;
 L22.t1 = create_at11;
 L23 = L22;
 L23.t2 = create_at21;
 ret_val17 = L23;
 ret_val = ret_val17;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TUPTP_858321267 FMAPTP1881680767(FMAPTP291739594 self, TP_CLASS k) {
 TUPTP_858321267 ret_val = TUPTP_858321267_zero;
 INT h = INT_zero;
 TP_CLASS tk;
 TP_CLASS tk1;
 FMAPTP291739594 key_nil_self;
 TP_CLASS ret_val1;
 ELT_NILTP_CLASS elt_nil_self;
 TP_CLASS ret_val2;
 TP_CLASS e = ((TP_CLASS) NULL);
 TUPTP_858321267 create_self = TUPTP_858321267_zero;
 TP_CLASS create_at1;
 FSETdTP create_at2;
 TUPTP_858321267 ret_val3 = TUPTP_858321267_zero;
 FMAPTP291739594 key_hash_self;
 OB key_hash_e;
 INT ret_val4 = INT_zero;
 FMAPTP291739594 is_key_nil_self;
 TP_CLASS is_key_nil_e;
 BOOL ret_val5 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val6 = BOOL_zero;
 FMAPTP291739594 key_eq_self;
 TP_CLASS key_eq_e1;
 TP_CLASS key_eq_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self;
 TP_CLASS elt_eq_e1;
 TP_CLASS elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val9 = BOOL_zero;
 FMAPTP291739594 is_key_nil_self1;
 TP_CLASS is_key_nil_e1;
 BOOL ret_val10 = BOOL_zero;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val11 = BOOL_zero;
 FMAPTP291739594 key_eq_self1;
 TP_CLASS key_eq_e11;
 TP_CLASS key_eq_e21;
 BOOL ret_val12 = BOOL_zero;
 ELT_EQTP_CLASS elt_eq_self1;
 TP_CLASS elt_eq_e11;
 TP_CLASS elt_eq_e21;
 BOOL ret_val13 = BOOL_zero;
 TP_CLASS is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val14 = BOOL_zero;
 FMAPTP291739594 key_nil_self1;
 TP_CLASS ret_val15;
 ELT_NILTP_CLASS elt_nil_self1;
 TP_CLASS ret_val16;
 TP_CLASS e1 = ((TP_CLASS) NULL);
 TUPTP_858321267 create_self1 = TUPTP_858321267_zero;
 TP_CLASS create_at11;
 FSETdTP create_at21;
 TUPTP_858321267 ret_val17 = TUPTP_858321267_zero;
 TUPTP_858321267 L1;
 TUPTP_858321267 L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPTP_858321267 L91_;
 TUPTP_858321267 L10;
 TUPTP_858321267 L111_;
 INT L121_;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 TUPTP_858321267 L181_;
 TUPTP_858321267 L19;
 TUPTP_858321267 L201_;
 INT L211_;
 TUPTP_858321267 L22;
 TUPTP_858321267 L23;
 if ((self==((FMAPTP291739594) NULL))) {
  create_self = TUPTP_858321267_zero;
  key_nil_self = self;
  elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  ret_val2 = ((TP_CLASS) NULL);
  ret_val1 = ret_val2;
  create_at1 = ret_val1;
  create_at2 = ((FSETdTP) NULL);
  L1 = create_self;
  L1.t1 = create_at1;
  L2 = L1;
  L2.t2 = create_at2;
  ret_val3 = L2;
  ret_val = ret_val3;
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val4 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L3 = ret_val4;
 L41_=ASIZE((FMAPTP291739594)self); 
 L5 = L41_;
 L61_=INTMINUS(L5,2); 
 L7 = L61_;
 L81_=L3&L7; 
 h = L81_;
 while (1) {
  L91_=ARR((FMAPTP291739594)self,h); 
  L10=L91_;
  tk = L10.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val6 = (is_elt_nil_e==((TP_CLASS) NULL));
  ret_val5 = ret_val6;
  if (ret_val5) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQTP_CLASS) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   is_eq_self = elt_eq_e1;
   is_eq_t = ((OB) elt_eq_e2);
   ret_val9 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val8 = ret_val9;
   ret_val7 = ret_val8;
   if (ret_val7) {
    L111_=ARR((FMAPTP291739594)self,h); 
    ret_val = L111_;
    return ret_val;
   }
  }
  L121_=INTPLUS(h,1); 
  h = L121_;
 }
 after_loop: ;
 L131_=ASIZE((FMAPTP291739594)self); 
 L14 = L131_;
 L151_=INTMINUS(L14,1); 
 L16 = L151_;
 L171_=(h)==(L16); 
 if (L171_) {
  h = 0;
  while (1) {
   L181_=ARR((FMAPTP291739594)self,h); 
   L19=L181_;
   tk1 = L19.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val11 = (is_elt_nil_e1==((TP_CLASS) NULL));
   ret_val10 = ret_val11;
   if (ret_val10) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQTP_CLASS) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    is_eq_self1 = elt_eq_e11;
    is_eq_t1 = ((OB) elt_eq_e21);
    ret_val14 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val13 = ret_val14;
    ret_val12 = ret_val13;
    if (ret_val12) {
     L201_=ARR((FMAPTP291739594)self,h); 
     ret_val = L201_;
     return ret_val;
    }
   }
   L211_=INTPLUS(h,1); 
   h = L211_;
  }
  after_loop1: ;
 }
 create_self1 = TUPTP_858321267_zero;
 key_nil_self1 = self;
 elt_nil_self1 = ((ELT_NILTP_CLASS) NULL);
 ret_val16 = ((TP_CLASS) NULL);
 ret_val15 = ret_val16;
 create_at11 = ret_val15;
 create_at21 = ((FSETdTP) NULL);
 L22 = create_self1;
 L22.t1 = create_at11;
 L23 = L22;
 L23.t2 = create_at21;
 ret_val17 = L23;
 ret_val = ret_val17;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 1

INT FMAPST1937251125(FMAPST1937251125_frame frame) {
 INT dummy = INT_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRINT L5;
 TUPSTRINT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRINT) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRINT)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

STR FMAPST2087339362(FMAPST2087339362_frame frame) {
 STR dummy = ((STR) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSTRSTR L5;
 TUPSTRSTR aL41_;
 TUPSTRSTR L7;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRSTR) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRSTR)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRSTR)frame->L61,frame->L31); 
    L7=aL41_;
    frame->r = L7.t1;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

STR FMAPST961226608(FMAPST961226608_frame frame) {
 STR dummy = ((STR) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSTRINT L5;
 TUPSTRINT aL41_;
 TUPSTRINT L7;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRINT) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRINT)frame->L61,frame->L31); 
    L7=aL41_;
    frame->r = L7.t1;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

TUPSTRFSETSTR FMAPST1155259492(FMAPST1155259492_frame frame) {
 TUPSTRFSETSTR dummy = TUPSTRFSETSTR_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRFSETSTR L5;
 TUPSTRFSETSTR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRFSETSTR) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRFSETSTR)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRFSETSTR)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

TUPSTRFSETSTR FMAPST753435567(FMAPST753435567_frame frame) {
 TUPSTRFSETSTR dummy = TUPSTRFSETSTR_zero;
 TUPSTRFSETSTR aI_u_I;
 TUPSTRFSETSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRFSETSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSTRFSETSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRINT FMAPST403469175(FMAPST403469175_frame frame) {
 TUPSTRINT dummy = TUPSTRINT_zero;
 TUPSTRINT aI_u_I;
 TUPSTRINT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSTRINT)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRINT FMAPST864901056(FMAPST864901056_frame frame) {
 TUPSTRINT dummy = TUPSTRINT_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRINT L5;
 TUPSTRINT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRINT) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRINT)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRINT)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

TUPSTRSIG FMAPST1841912328(FMAPST1841912328_frame frame) {
 TUPSTRSIG dummy = TUPSTRSIG_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRSIG L5;
 TUPSTRSIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRSIG) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRSIG)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRSIG)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

TUPSTRSIG FMAPST486993187(FMAPST486993187_frame frame) {
 TUPSTRSIG dummy = TUPSTRSIG_zero;
 TUPSTRSIG aI_u_I;
 TUPSTRSIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSTRSIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPSTRSTR FMAPST1934702696(FMAPST1934702696_frame frame) {
 TUPSTRSTR dummy = TUPSTRSTR_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRSTR L5;
 TUPSTRSTR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRSTR) NULL));
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
     frame->L41_br=ASIZE((FMAPSTRSTR)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRSTR)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
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

TUPSTRSTR FMAPST2026852485(FMAPST2026852485_frame frame) {
 TUPSTRSTR dummy = TUPSTRSTR_zero;
 TUPSTRSTR aI_u_I;
 TUPSTRSTR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSTRSTR)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPTP_1668454172 FMAPTP1065055386(FMAPTP1065055386_frame frame) {
 TUPTP_1668454172 dummy = TUPTP_1668454172_zero;
 BOOL L1;
 BOOL L21_;
 TUPTP_1668454172 L5;
 TUPTP_1668454172 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPTP775297600) NULL));
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
     frame->L41_br=ASIZE((FMAPTP775297600)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPTP775297600)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((TP_CLASS) NULL));
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

TUPTP_1668454172 FMAPTP652676349(FMAPTP652676349_frame frame) {
 TUPTP_1668454172 dummy = TUPTP_1668454172_zero;
 TUPTP_1668454172 aI_u_I;
 TUPTP_1668454172 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPTP775297600)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPTP775297600)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPTP_858321267 FMAPTP1032211620(FMAPTP1032211620_frame frame) {
 TUPTP_858321267 dummy = TUPTP_858321267_zero;
 TUPTP_858321267 aI_u_I;
 TUPTP_858321267 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPTP291739594)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPTP291739594)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TUPTP_858321267 FMAPTP1465407463(FMAPTP1465407463_frame frame) {
 TUPTP_858321267 dummy = TUPTP_858321267_zero;
 BOOL L1;
 BOOL L21_;
 TUPTP_858321267 L5;
 TUPTP_858321267 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPTP291739594) NULL));
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
     frame->L41_br=ASIZE((FMAPTP291739594)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPTP291739594)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILTP_CLASS) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((TP_CLASS) NULL));
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

TUPdOBNAMESPACE FMAPdO1025347514(FMAPdO1025347514_frame frame) {
 TUPdOBNAMESPACE dummy = TUPdOBNAMESPACE_zero;
 BOOL L1;
 BOOL L21_;
 TUPdOBNAMESPACE L5;
 TUPdOBNAMESPACE aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPdOBNAMESPACE) NULL));
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
     frame->L41_br=ASIZE((FMAPdOBNAMESPACE)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPdOBNAMESPACE)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->ret_val = ELT_NI455285430(((ELT_NILdOB) NULL), frame->is_key_nil_e);
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

TUPdOBNAMESPACE FMAPdO1840166113(FMAPdO1840166113_frame frame) {
 TUPdOBNAMESPACE dummy = TUPdOBNAMESPACE_zero;
 TUPdOBNAMESPACE aI_u_I;
 TUPdOBNAMESPACE L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdOBNAMESPACE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPdOBNAMESPACE)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void FMAPST165607239(FMAPST165607239_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRINT)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSTRINT)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPST1699038397(FMAPST1699038397_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSTRSTR)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPST653613295(FMAPST653613295_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRFSETSTR)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSTRFSETSTR)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPST82083227(FMAPST82083227_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRSIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSTRSIG)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPTP323231562(FMAPTP323231562_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPTP291739594)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPTP291739594)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPTP702226833(FMAPTP702226833_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPTP775297600)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPTP775297600)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FMAPdO701667859(FMAPdO701667859_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPdOBNAMESPACE)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPdOBNAMESPACE)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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
