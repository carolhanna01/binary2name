#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dCALL_TP_struct {
 OB_HEADER header;
 } *dCALL_TP;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

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

typedef struct ARRAYARRAYSTR_struct {/* layout for ARRAY{ARRAY{STR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *ARRAYARRAYSTR;

typedef struct ARRAYCALL_ARG_struct {/* layout for ARRAY{CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct CALL_ARG_struct *arr_part[1];
 } *ARRAYCALL_ARG;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct CALL_ARG_struct {/* layout for CALL_ARG */
 OB_HEADER header;
 struct dCALL_TP_struct *tp;
 struct dMODE_struct *mode1;
 } *CALL_ARG;

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_NILELT_struct {/* layout for ELT_NIL{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILELT;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ELT_NI1575801776_struct {/* layout for ELT_NIL{TUP{IDENT,INT}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1575801776;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct GLOBAL_TBL_struct {/* layout for GLOBAL_TBL */
 OB_HEADER header;
 struct FLISTA112326087_struct *top_sort;
 struct FMAPdT953816637_struct *tbl;
 struct PROG_struct *prog;
 } *GLOBAL_TBL;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct IDENT_BUILTIN_struct {/* layout for IDENT_BUILTIN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IDENT_BUILTIN;

typedef struct IFC_struct {/* layout for IFC */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SIG_TBL_struct *sigs;
 } *IFC;

typedef struct IFC_ABS_CREATE_struct {/* layout for IFC_ABS_CREATE */
 OB_HEADER header;
 struct AS_CLASS_DEF_struct *tr;
 struct FLISTIFC_struct *supers;
 struct SIG_TBL_struct *class_sigs;
 struct TP_CLASS_struct *tp;
 struct TP_CONTEXT_struct *con;
 } *IFC_ABS_CREATE;

typedef struct IMPL_struct {/* layout for IMPL */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ELT_TBL_struct *elts;
 struct IFC_struct *ifc;
 struct TP_CLASS_struct *arr;
 BOOL is_ref_cache;
 BOOL use_ref_cache;
 } *IMPL;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

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

typedef struct SIG_TBL_struct {/* layout for SIG_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *SIG_TBL;

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

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

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

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

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

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

typedef struct TUPSTRSTR_struct {/* layout for TUP{STR,STR} */
 STR t1;
 STR t2;
 } TUPSTRSTR;
static TUPSTRSTR TUPSTRSTR_zero;

typedef struct TUPSTRSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRSTR immutable_part;
 } *TUPSTRSTR_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

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

typedef struct AS_CONST_DEF_struct {/* layout for AS_CONST_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_CONST_DEF;

typedef struct AS_INT_LIT_EXPR_struct {/* layout for AS_INT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 BOOL is_inti;
 } *AS_INT_LIT_EXPR;

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

typedef struct AS_STR_LIT_EXPR_struct {/* layout for AS_STR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 STR s;
 } *AS_STR_LIT_EXPR;

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

typedef struct CALL_SIG_struct {/* layout for CALL_SIG */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct ARRAYCALL_ARG_struct *args;
 struct IDENT_struct name1;
 BOOL has_ret;
 BOOL unknown_ret;
 } *CALL_SIG;

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

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

typedef struct FLISTIFC_struct {/* layout for FLIST{IFC} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct IFC_struct *arr_part[1];
 } *FLISTIFC;

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

typedef struct FMAPdT953816637_struct {/* layout for FMAP{$TP,FMAP{IDENT,AM_SHARED_EXPR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTP1710233051_struct arr_part[1];
 } *FMAPdT953816637;

typedef struct FMAPID739086519_struct {/* layout for FMAP{IDENT,AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDE641654124_struct arr_part[1];
 } *FMAPID739086519;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

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

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct GET_OPTIONS_struct {/* layout for GET_OPTIONS */
 OB_HEADER header;
 struct ARRAYSTR_struct *arg_locations;
 struct ARRAYSTR_struct *args;
 struct FMAPSTRSTR_struct *module_inclusion;
 struct FSETSTR_struct *classes1;
 struct FSETSTR_struct *paths;
 INT next;
 struct PROG_struct *prog;
 BOOL all1;
 BOOL got_libs;
 } *GET_OPTIONS;

typedef struct IDENT_TBL_struct {/* layout for IDENT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *IDENT_TBL;

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

#include "tags.h"

/* Globals */

extern FSETTUPIDENTINT IFC_abs_cur;
extern IDENT IDENT_1061837561;
extern IDENT IDENT_1067669005;
extern IDENT IDENT_1129727818;
extern IDENT IDENT_1137952589;
extern IDENT IDENT_1140439567;
extern IDENT IDENT_12128454;
extern IDENT IDENT_1280657180;
extern IDENT IDENT_1295303664;
extern IDENT IDENT_1418293603;
extern IDENT IDENT_1485568624;
extern IDENT IDENT_1616879092;
extern IDENT IDENT_1644121920;
extern IDENT IDENT_1666445894;
extern IDENT IDENT_1728630987;
extern IDENT IDENT_1834849979;
extern IDENT IDENT_1837767448;
extern IDENT IDENT_1891587148;
extern IDENT IDENT_190278447;
extern IDENT IDENT_1909070998;
extern IDENT IDENT_1924458840;
extern IDENT IDENT_196328811;
extern IDENT IDENT_1974148927;
extern IDENT IDENT_2063376192;
extern IDENT IDENT_2085547498;
extern IDENT IDENT_2108657069;
extern IDENT IDENT_2110206590;
extern IDENT IDENT_228725606;
extern IDENT IDENT_265373817;
extern IDENT IDENT_291726180;
extern IDENT IDENT_327685657;
extern IDENT IDENT_386785441;
extern IDENT IDENT_421158771;
extern IDENT IDENT_428696930;
extern IDENT IDENT_458929583;
extern IDENT IDENT_461800478;
extern IDENT IDENT_475003333;
extern IDENT IDENT_51715464;
extern IDENT IDENT_5303930;
extern IDENT IDENT_531080196;
extern IDENT IDENT_58546263;
extern IDENT IDENT_604723504;
extern IDENT IDENT_667688077;
extern IDENT IDENT_673807638;
extern IDENT IDENT_685437142;
extern IDENT IDENT_692534448;
extern IDENT IDENT_853591262;
extern IDENT IDENT_857412936;
extern IDENT IDENT_869381517;
extern IDENT IDENT_959790938;
extern IDENT IDENT_985337352;
extern IDENT_TBL IDENT_ident_tbl;
extern INOUT_MODE MODES_inout_mode;
extern IN_MODE MODES_in_mode;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_str;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is244083069)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST PROG (**dTP_progrPROG)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAS_CLASS_ELT (**dAS_CL760354380)(dAS_CLASS_ELT);
AM_SHARED_EXPR FMAPID608869096(FMAPID739086519, IDENT);
AM_SHARED_EXPR GLOBAL499677090(GLOBAL_TBL, IDENT, dTP);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
ARRAYSTR ARRAYS417083844(ARRAYSTR, ARRAYSTR);
ARRAYSTR FLISTS533235694(FLISTSTR);
BOOL CALL_S1637743358(CALL_SIG, SIG);
BOOL CHAR_i84735023(CHAR);
BOOL ELT_is1269015106(ELT);
BOOL ELT_is380037919(ELT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL FSETTU645397512(FSETTUPIDENTINT, TUPIDENTINT);
BOOL GET_OP1847571144(GET_OPTIONS, STR);
BOOL IDENT_1780939044(IDENT, OB);
BOOL IFC_is2032003003(IFC, dCALL_TP);
BOOL IMPL_i1397207249(IMPL, dCALL_TP);
BOOL IMPL_i932377427(IMPL);
BOOL SIG_co1757762355(SIG, SIG);
BOOL SIG_is1893596208(SIG, SIG);
BOOL STR_is111530248(STR, STR);
BOOL TP_CLA1568240525(TP_CLASS);
CONFIG_DEF CONFIG1152920950(CONFIG_TBL, STR);
ELT ELT_TB431570775(ELT_TB431570775_frame);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
FLISTIFC FLISTI985525641(FLISTIFC, IFC);
FLISTSIG FLISTS69519306(FLISTSIG, SIG);
FLISTSIG IFC_no96952824(IFC, IFC);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTSTR GET_OP1478203685(GET_OPTIONS, STR);
FMAPID739086519 FMAPdT183078507(FMAPdT953816637, dTP);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FSETSIG FSETSI123770722(FSETSIG, FSETSIG);
FSETSIG FSETSI461354464(FSETSIG);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSIG FSETSI719979460(FSETSIG);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETTUPIDENTINT FSETTU1559268184(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU2045052674(FSETTUPIDENTINT, TUPIDENTINT);
FSETTUPIDENTINT FSETTU419377758(FSETTUPIDENTINT);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR FSTR_s1920457899(FSTR, INT, INT);
IDENT IDENT_1150413730(IDENT, STR);
IDENT IDENT_512720063(IDENT_TBL, STR);
IDENT_TBL IDENT_81729787(IDENT_TBL, IDENT);
IFC FLISTIFC_poprIFC(FLISTIFC);
IFC IFC_AB622464468(IFC_ABS_CREATE, TP_CLASS);
IFC IFC_if331163657(IFC, TP_CLASS);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
IMPL TP_CLA1109727233(TP_CLASS);
INT FLISTI751030673(FLISTIFC);
INT FLISTS228922656(FLISTSIG);
INT FLISTS326330076(FLISTSTR);
INT FSETSIG_sizerINT(FSETSIG);
INT FSTR_sizerINT(FSTR);
INT IDENT_hashrINT(IDENT);
INT IMPL_a1897060099(IMPL);
INT INTI_intrINT(INTI1);
INT INT_do676118316(INT_do676118316_frame);
INT INT_up418511718(INT_up418511718_frame);
INT SIG_TBL_sizerINT(SIG_TBL);
INT STR_lengthrINT(STR);
INT STR_se1725540125(STR, CHAR);
INT STR_sizerINT(STR);
SIG FLISTS1074219534(FLISTS1074219534_frame);
SIG FLISTSIG_toprSIG(FLISTSIG);
SIG FSETSI2020712453(FSETSIG);
SIG FSETSIG_eltbrSIG(FSETSIG_eltbrSIG_frame);
SIG IFC_no472265690(IFC, IFC);
SIG IFC_se383132405(IFC, SIG, FLISTSIG, BOOL);
SIG IFC_se461463192(IFC, CALL_SIG, FLISTSIG, BOOL);
SIG IFC_si178998979(IFC, CALL_SIG);
SIG IFC_si2085565042(IFC, SIG);
SIG IMPL_i1943552357(IMPL);
SIG IMPL_s1294117947(IMPL, CALL_SIG, FLISTSIG, BOOL);
SIG IMPL_s773282892(IMPL, CALL_SIG);
SIG SIG_TB1067194561(SIG_TBL, SIG);
SIG SIG_TB788365833(SIG_TB788365833_frame);
SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame);
SIG SIG_ro1276623596(SIG, AS_ROUT_DEF, IDENT, ARRAYdTP, TP_CONTEXT);
SIG SIG_wi1810596376(SIG, dTP);
SIG_TBL IFC_AB597363174(IFC_ABS_CREATE);
SIG_TBL SIG_TB1438251609(SIG_TBL, SIG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CALL_SIG_strrSTR(CALL_SIG);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR GET_OP1568448036(GET_OPTIONS, STR, dSTR);
STR GET_OP1767328402(GET_OPTIONS, STR, STR, STR);
STR GET_OP1833245241(GET_OPTIONS, STR, dSTR, INT*);
STR GET_OP1899004935(GET_OPTIONS);
STR GET_OP2007358201(GET_OPTIONS, STR);
STR GET_OP751661850(GET_OPTIONS, STR);
STR GET_OP93258057(GET_OPTIONS);
STR IDENT_1415966123(IDENT, INT);
STR IFC_strrSTR(IFC);
STR IMPL_s709672763(IMPL, IDENT);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_as1755591216(STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_se405450305(STR_se405450305_frame);
STR STR_tail_INTrSTR(STR, INT);
STR TP_CLASS_strrSTR(TP_CLASS);
STR UNIX_g108606936(UNIX, STR);
TP_CONTEXT TP_CLA821242177(TP_CLASS);
TUPIDENTINT FSETTU947212086(FSETTU947212086_frame);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void GET_OP1011839251(GET_OPTIONS);
void GET_OP1239350398(GET_OPTIONS);
void GET_OP1297032912(GET_OPTIONS, FLISTSTR, STR);
void GET_OP1722411507(GET_OPTIONS, ARRAYSTR);
void GET_OP383637490(GET_OPTIONS, STR);
void GET_OP584215778(GET_OPTIONS, STR, STR);
void IDENT_1047918280(IDENT_BUILTIN);
void IFC_AB1563215475(IFC_ABS_CREATE);
void IFC_AB1878942039(IFC_ABS_CREATE);
void IFC_cycle_err(IFC);
void IFC_show(IFC);
void INT_timesb(INT_timesb_frame);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_SHARED_EXPR GLOBAL499677090(GLOBAL_TBL self, IDENT name111, dTP tp) {
 AM_SHARED_EXPR ret_val;
 FMAPID739086519 m;
 BOOL L1;
 BOOL L21_;
 m = FMAPdT183078507(ATTR(self,tbl), tp);
 L1 = (m==((FMAPID739086519) NULL));
 L21_=!(L1); 
 if (L21_) {
  ret_val = FMAPID608869096(m, name111);
  return ret_val;
 }
 ret_val = ((AM_SHARED_EXPR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL GET_OP1847571144(GET_OPTIONS self, STR fn) {
 BOOL ret_val = BOOL_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 L11_=ARR((STR)fn,0); 
 L2 = L11_;
 L31_=L2=='-'; 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 L41_=ARR((STR)fn,0); 
 L5 = L41_;
 L61_=L5=='/'; 
 if (L61_) {
  ret_val = TRUE;
  return ret_val;
 }
 L7 = STR_se1725540125(fn, '.');
 L81_=(L7)==(-1); 
 L9 = L81_;
 L101_=!(L9); 
 ret_val = L101_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL IDENT_1780939044(IDENT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 IDENT L1;
 if (arg==NULL) {
  goto other167;
 } else
 switch (TAG(arg)) {
  case IDENT_tag:
   is_eq_self = self;
   L1 = ((IDENT_boxed) arg)->immutable_part;
   is_eq_i = L1;
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other167: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL IFC_is2032003003(IFC self, dCALL_TP ctp) {
 BOOL ret_val = BOOL_zero;
 if (ctp==NULL) {
  goto other132;
 } else
 switch (TAG(ctp)) {
  case CALL_TP_VOID_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_TP_CREATE_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_T561792367_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_TP_ARRAY_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_T666389079_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other132: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL IMPL_i1397207249(IMPL self, dCALL_TP ctp) {
 BOOL ret_val = BOOL_zero;
 if (ctp==NULL) {
  goto other395;
 } else
 switch (TAG(ctp)) {
  case CALL_TP_VOID_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_TP_CREATE_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_T561792367_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_TP_ARRAY_tag:
   ret_val = TRUE;
   return ret_val; break;
  case CALL_T666389079_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other395: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL IMPL_i932377427(IMPL self) {
 BOOL ret_val = BOOL_zero;
 CONFIG_DEF d;
 CONFIG_DEF L11;
 ARRAYSTR e;
 ARRAYSTR L21;
 STR c;
 ELT_TBL L31;
 ELT e1;
 dTP atp;
 dTP tparam;
 IMPL prog_self;
 PROG ret_val1;
 CONFIG_DEF eltb_self;
 ARRAYSTR ret_val2;
 ARRAYARRAYSTR L41;
 BOOL eltb_if_first = BOOL_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 dTP L7;
 dTP L8;
 PROG L9;
 extern STR REFERENCE_FREE;
 INT L101_br;
 ARRAYSTR aL101_;
 ARRAYSTR L12;
 INT L131_br;
 STR aL131_;
 STR L14;
 dTP L15;
 STR L16;
 INT L171_;
 INT L181_;
 ELT L19;
 dTP L20;
 BOOL L22;
 BOOL L231_;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 ARRAYdTP L27;
 INT L281_;
 INT L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 ARRAYdTP L34;
 dTP L351_;
 dTP L36;
 BOOL L37;
 BOOL L381_;
 dTP L39;
 BOOL L40;
 BOOL L421_;
 if (ATTR(self,use_ref_cache)) {
  ret_val = ATTR(self,is_ref_cache);
  return ret_val;
 }
 SATTR(self,use_ref_cache,TRUE);
 L7 = ATTR(self,tp);
 if ((*dTP_is914808564[TAG(L7)])(L7)) {
  prog_self = self;
  L8 = ATTR(prog_self,tp);
  ret_val1 = (*dTP_progrPROG[TAG(L8)])(L8);
  L9=ret_val1;
  d = CONFIG1152920950(ATTR(L9,config), ((STR) &REFERENCE_FREE));
  {
   BOOL f_L101_ = TRUE;
   /* Initialization of inlined iter CONFIG_DEF::elt!:ARRAY{STR} */
   L11 = d;
   eltb_self = L11;
   /* loop index variable */
   L51 = 0;
   L41 = ATTRs(eltb_self,expr,((ARRAYARRAYSTR) NULL));
   L101_br=L41==NULL?0:ASIZE((ARRAYARRAYSTR)L41); 
   eltb_if_first = TRUE;
   while (1) {
    if(L51>=L101_br)  goto after_loop; 
    aL101_=ARR((ARRAYARRAYSTR)L41,L51); 
    e = aL101_;
    {
     BOOL f_L131_ = TRUE;
     /* loop index variable */
     L61 = 0;
     L21 = e;
     L131_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
     while (1) {
      if(L61>=L131_br)  goto after_loop1; 
      aL131_=ARR((ARRAYSTR)L21,L61); 
      c = aL131_;
      L15 = ATTR(self,tp);
      L16 = (*dTP_strrSTR[TAG(L15)])(L15);
      if (STR_is111530248(L16, c)) {
       SATTR(self,is_ref_cache,TRUE);
       ret_val = TRUE;
       return ret_val;
      }
      L171_=INTPLUS(L61,1); 
      L61 = L171_;
     }
    }
    after_loop1: ;
    L181_=INTPLUS(L51,1); 
    L51 = L181_;
   }
  }
  after_loop: ;
  SATTR(self,is_ref_cache,FALSE);
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct ELT_TBL_eltbrELT_frame_struct other3_0;
ELT_TBL_eltbrELT_frame noname1 = &other3_0;
  L31 = ATTR(self,elts);
  noname1->self = L31;
  noname1->state = 0;
  while (1) {
   L19 = ELT_TBL_eltbrELT(noname1);
   if (noname1->state == -1) {
    goto after_loop2;
   }
   e1 = L19;
   if (ELT_is380037919(e1)) {
    atp = ATTR(ATTR(e1,sig1),ret);
    L20 = atp;
    L22 = (*dTP_is1334578382[TAG(L20)])(L20);
    L231_=!(L22); 
    if (L231_) {
     SATTR(self,is_ref_cache,FALSE);
     ret_val = FALSE;
     return ret_val;
    }
    L24 = atp;
    L25 = (*dTP_is244083069[TAG(L24)])(L24);
    L261_=!(L25); 
    if (L261_) {
     SATTR(self,is_ref_cache,FALSE);
     ret_val = FALSE;
     return ret_val;
    }
   }
  }
 }
 after_loop2: ;
 if ((ATTR(self,arr)==((TP_CLASS) NULL))) {
  SATTR(self,is_ref_cache,TRUE);
  ret_val = TRUE;
  return ret_val;
 }
 else {
  if ((ATTR(ATTR(self,arr),params)==((ARRAYdTP) NULL))) {
   SATTR(self,is_ref_cache,FALSE);
   ret_val = FALSE;
   return ret_val;
  }
  else {
   L27 = ATTR(ATTR(self,arr),params);
   L281_=ASIZE((ARRAYdTP)L27); 
   L29 = L281_;
   L301_=(L29)==(1); 
   L32 = L301_;
   L331_=!(L32); 
   if (L331_) {
    SATTR(self,is_ref_cache,FALSE);
    ret_val = FALSE;
    return ret_val;
   }
   else {
    L34 = ATTR(ATTR(self,arr),params);
    L351_=(dTP)ARR((ARRAYdTP)L34,0); 
    tparam = L351_;
    L36 = tparam;
    L37 = (*dTP_is1334578382[TAG(L36)])(L36);
    L381_=!(L37); 
    if (L381_) {
     SATTR(self,is_ref_cache,FALSE);
     ret_val = FALSE;
     return ret_val;
    }
    else {
     L39 = tparam;
     L40 = (*dTP_is244083069[TAG(L39)])(L39);
     L421_=!(L40); 
     if (L421_) {
      SATTR(self,is_ref_cache,FALSE);
      ret_val = FALSE;
      return ret_val;
     }
     else {
      SATTR(self,is_ref_cache,TRUE);
      ret_val = TRUE;
      return ret_val;
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTSIG IFC_no96952824(IFC self, IFC i) {
 FLISTSIG ret_val;
 FLISTSIG res = ((FLISTSIG) NULL);
 SIG_TBL L11;
 SIG s;
 SIG L2;
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  L11 = ATTR(i,sigs);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   s = L2;
   if ((IFC_si2085565042(self, s)==((SIG) NULL))) {
    res = FLISTS69519306(res, s);
   }
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR GET_OP1478203685(GET_OPTIONS self, STR name111) {
 FLISTSTR ret_val;
 STR wd;
 FLISTSTR cl;
 BFILE f;
 FSTR fs;
 STR tok;
 INT pos = INT_zero;
 CHAR c = CHAR_zero;
 INT start1 = INT_zero;
 FLISTSTR create_self;
 FLISTSTR ret_val1;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val2;
 BFILE r;
 BFILE error_self;
 BOOL ret_val3 = BOOL_zero;
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val5;
 FILE1 stderr_self;
 FILE1 ret_val6;
 FILE1 r1;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val7;
 FILE1 stderr_self1;
 FILE1 ret_val8;
 FILE1 r2;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val9;
 FILE1 r3;
 FSTR plus_self3;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val10;
 BFILE close_self;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val12 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val15;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val17;
 STR r4;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val18 = CHAR_zero;
 ERR1 create_self3;
 ERR1 ret_val19;
 ERR1 plus_self5;
 dSTR plus_s3;
 ERR1 ret_val20;
 FILE1 stderr_self3;
 FILE1 ret_val21;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s4;
 ERR1 ret_val22;
 FILE1 stderr_self4;
 FILE1 ret_val23;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val24;
 FILE1 r7;
 FSTR aget_self5;
 INT aget_ind5 = INT_zero;
 CHAR ret_val25 = CHAR_zero;
 FSTR aget_self6;
 INT aget_ind6 = INT_zero;
 CHAR ret_val26 = CHAR_zero;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val27;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val28;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val29;
 STR r8;
 ERR1 create_self5;
 ERR1 ret_val30;
 ERR1 plus_self9;
 dSTR plus_s6;
 ERR1 ret_val31;
 FILE1 stderr_self6;
 FILE1 ret_val32;
 FILE1 r9;
 ERR1 plus_self10;
 dSTR plus_s7;
 ERR1 ret_val33;
 FILE1 stderr_self7;
 FILE1 ret_val34;
 FILE1 r10;
 ERR1 plus_self11;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val35;
 FILE1 r11;
 FSTR aget_self7;
 INT aget_ind7 = INT_zero;
 CHAR ret_val36 = CHAR_zero;
 FSTR str_self2;
 STR ret_val37;
 FSTR aget_self8;
 INT aget_ind8 = INT_zero;
 CHAR ret_val38 = CHAR_zero;
 FSTR aget_self9;
 INT aget_ind9 = INT_zero;
 CHAR ret_val39 = CHAR_zero;
 STR plus_self12;
 STR plus_sarg;
 STR ret_val40;
 FSTR aget_self10;
 INT aget_ind10 = INT_zero;
 CHAR ret_val41 = CHAR_zero;
 FSTR aget_self11;
 INT aget_ind11 = INT_zero;
 CHAR ret_val42 = CHAR_zero;
 STR plus_self13;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val43;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val45;
 STR r12;
 STR plus_self14;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val46;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val47;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val48;
 STR r13;
 STR plus_self15;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val49;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val50;
 STR create_self8;
 CHAR create_c4 = CHAR_zero;
 STR ret_val51;
 STR r14;
 STR plus_self16;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val52;
 STR s5;
 CHAR str_self6 = CHAR_zero;
 STR ret_val53;
 STR create_self9;
 CHAR create_c5 = CHAR_zero;
 STR ret_val54;
 STR r15;
 STR plus_self17;
 STR plus_sarg1;
 STR ret_val55;
 BFILE L1;
 OB L2;
 extern STR rb;
 STR L3;
 STR L4;
 BOOL L5;
 extern STR Couldntopenfile;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 dSTR L12;
 OB L13;
 FILE1 L14;
 OB L15;
 extern STR name1;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 CHAR L211_;
 BOOL L221_;
 INT L231_;
 CHAR L241_;
 CHAR L25;
 BOOL L261_;
 INT L271_;
 BOOL L28;
 BOOL L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 CHAR L341_;
 CHAR L35;
 BOOL L361_;
 CHAR L371_;
 CHAR L38;
 BOOL L391_;
 STR L40;
 INT L41;
 OB L42;
 INT L43;
 BOOL L451_;
 INT L461_;
 CHAR L471_;
 CHAR L48;
 BOOL L491_;
 INT L501_;
 INT L51;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 extern STR Unterm820051874;
 FILE1 L57;
 OB L58;
 FILE1 L60;
 OB L61;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 BOOL L68;
 CHAR L691_;
 CHAR L70;
 BOOL L711_;
 INT L721_;
 CHAR L731_;
 CHAR L74;
 BOOL L751_;
 INT L761_;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 BOOL L831_;
 INT L841_;
 INT L85;
 INT L861_;
 INT L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 extern STR Unterm919914180;
 FILE1 L91;
 OB L92;
 FILE1 L94;
 OB L95;
 dSTR L97;
 OB L98;
 FILE1 L99;
 OB L100;
 CHAR L1021_;
 CHAR L103;
 BOOL L1041_;
 INT L1051_;
 INT L106;
 INT L1071_;
 extern STR name1;
 INT L1081_;
 BOOL L1091_;
 BOOL L110;
 INT L1121_;
 CHAR L1131_;
 CHAR L114;
 BOOL L1151_;
 INT L1161_;
 CHAR L1171_;
 CHAR L118;
 BOOL L1191_;
 INT* L120;
 STR L121;
 BOOL L122;
 BOOL L1231_;
 BOOL L124;
 CHAR L1251_;
 BOOL L126;
 BOOL L1271_;
 INT L128;
 BOOL L1291_;
 CHAR L1301_;
 STR L131;
 INT L132;
 OB L133;
 INT L134;
 INT L1361_;
 extern STR name1;
 STR L137;
 INT L138;
 OB L139;
 INT L140;
 BOOL L142;
 BOOL L1431_;
 STR L144;
 INT L145;
 OB L146;
 INT L147;
 extern STR name1;
 BOOL L149;
 BOOL L1501_;
 BOOL L151;
 CHAR L1521_;
 CHAR L153;
 BOOL L1541_;
 BOOL L155;
 BOOL L1561_;
 STR L157;
 INT L158;
 OB L159;
 INT L160;
 extern STR name1;
 INT L1631_;
 wd = GET_OP751661850(self, name111);
 create_self = ((FLISTSTR) NULL);
 ret_val1 = ((FLISTSTR) NULL);
 cl = ret_val1;
 open_for_read_se = ((BFILE) NULL);
 open_for_read_nm = name111;
 L2=ZALLOC(sizeof(struct BFILE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=BFILE_tag;
 L1 = ((BFILE) L2);
 r = L1;
 L3 = open_for_read_nm;
 L4 = ((STR) &rb);
 SATTR(r,fp,fopen(((L3==NULL)?NULL:L3->arr_part), ((L4==NULL)?NULL:L4->arr_part)));
 ret_val2 = r;
 f = ret_val2;
 error_self = f;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L5 = TRUE;
 } else {
  L5 = ferror(ATTR(error_self,fp));
 }
 ret_val3 = L5;
 if (ret_val3) {
  create_self1 = ((ERR1) NULL);
  ret_val4 = create_self1;
  plus_self = ret_val4;
  plus_s = ((dSTR) ((STR) &Couldntopenfile));
  stderr_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stderr);
  ret_val6 = r1;
  FILE_plus_dSTR(ret_val6, plus_s);
  ret_val5 = plus_self;
  plus_self1 = ret_val5;
  plus_s1 = ((dSTR) name111);
  stderr_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stderr);
  ret_val8 = r2;
  FILE_plus_dSTR(ret_val8, plus_s1);
  ret_val7 = plus_self1;
  plus_self2 = ret_val7;
  L13=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  memset(L13,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L13)->header.tag=CHAR_tag;
  L12 = (dSTR)((CHAR_boxed) L13);
  ((CHAR_boxed) L12)->immutable_part = '\n';
  plus_s2 = L12;
  stderr_self2 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stderr);
  ret_val9 = r3;
  FILE_plus_dSTR(ret_val9, plus_s2);
  GET_OP1011839251(self);
 }
 plus_self3 = BFILE_fstrrFSTR(f);
 plus_c = ' ';
 ret_val10 = FSTR_p399773021(plus_self3, plus_c);
 fs = ret_val10;
 close_self = f;
 fclose(ATTR(close_self,fp));
 tok = ((STR) &name1);
 pos = 0;
 while (1) {
  L17 = FSTR_sizerINT(fs);
  L181_=(pos)<(L17); 
  L19 = L181_;
  L201_=!(L19); 
  if (L201_) {
   goto after_loop;
  }
  aget_self = fs;
  aget_ind = pos;
  L211_=ARR((FSTR)aget_self,aget_ind); 
  ret_val11 = L211_;
  c = ret_val11;
  L221_=c=='-'; 
  if (L221_) {
   aget_self1 = fs;
   L231_=INTPLUS(pos,1); 
   aget_ind1 = L231_;
   L241_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val12 = L241_;
   L25 = ret_val12;
   L261_=L25=='-'; 
   if (L261_) {
    while (1) {
     L271_=INTPLUS(pos,1); 
     pos = L271_;
     L30 = FSTR_sizerINT(fs);
     L311_=(pos)<(L30); 
     L32 = L311_;
     L331_=!(L32); 
     if (L331_) {
      L29 = TRUE;
     } else {
      aget_self2 = fs;
      aget_ind2 = pos;
      L341_=ARR((FSTR)aget_self2,aget_ind2); 
      ret_val13 = L341_;
      L35 = ret_val13;
      L361_=L35=='\n'; 
      L29 = L361_;
     }
     if (L29) {
      L28 = TRUE;
     } else {
      aget_self3 = fs;
      aget_ind3 = pos;
      L371_=ARR((FSTR)aget_self3,aget_ind3); 
      ret_val14 = L371_;
      L38 = ret_val14;
      L391_=L38=='\r'; 
      L28 = L391_;
     }
     if (L28) {
      goto after_loop1;
     }
    }
    after_loop1: ;
   }
   else {
    plus_self4 = tok;
    plus_arg = '-';
    str_self = plus_arg;
    create_self2 = ((STR) NULL);
    create_c = str_self;
    L41 = 1;
    L43=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L41)*sizeof(CHAR);
    L42=ZALLOC_LEAF_BIG(L43);
    if (L42==NULL) FATAL("Unable to allocate more memory");
    memset(L42,0,L43);
    ((OB)L42)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L42)->header.destroyed=0;
#endif

    L40 = ((STR) L42);
    L40->asize = L41;
    r4 = L40;
    SARR((STR)r4,0,create_c); 
    ;
    ret_val17 = r4;
    ret_val16 = ret_val17;
    s = ret_val16;
    ret_val15 = STR_ap1077157958(plus_self4, s, TRUE);
    tok = ret_val15;
   }
  }
  else {
   L451_=c=='('; 
   if (L451_) {
    aget_self4 = fs;
    L461_=INTPLUS(pos,1); 
    aget_ind4 = L461_;
    L471_=ARR((FSTR)aget_self4,aget_ind4); 
    ret_val18 = L471_;
    L48 = ret_val18;
    L491_=L48=='*'; 
    if (L491_) {
     while (1) {
      L501_=INTPLUS(pos,1); 
      pos = L501_;
      L51 = FSTR_sizerINT(fs);
      L521_=INTMINUS(L51,1); 
      L53 = L521_;
      L541_=(pos)<(L53); 
      L55 = L541_;
      L561_=!(L55); 
      if (L561_) {
       create_self3 = ((ERR1) NULL);
       ret_val19 = create_self3;
       plus_self5 = ret_val19;
       plus_s3 = ((dSTR) ((STR) &Unterm820051874));
       stderr_self3 = ((FILE1) NULL);
       L58=ZALLOC(sizeof(struct FILE1_struct));
       if (L58==NULL) FATAL("Unable to allocate more memory");
       ((OB)L58)->header.tag=FILE1_tag;
       L57 = ((FILE1) L58);
       r5 = L57;
       
       SATTR(r5,fp,stderr);
       ret_val21 = r5;
       FILE_plus_dSTR(ret_val21, plus_s3);
       ret_val20 = plus_self5;
       plus_self6 = ret_val20;
       plus_s4 = ((dSTR) name111);
       stderr_self4 = ((FILE1) NULL);
       L61=ZALLOC(sizeof(struct FILE1_struct));
       if (L61==NULL) FATAL("Unable to allocate more memory");
       ((OB)L61)->header.tag=FILE1_tag;
       L60 = ((FILE1) L61);
       r6 = L60;
       
       SATTR(r6,fp,stderr);
       ret_val23 = r6;
       FILE_plus_dSTR(ret_val23, plus_s4);
       ret_val22 = plus_self6;
       plus_self7 = ret_val22;
       L64=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L64==NULL) FATAL("Unable to allocate more memory");
       memset(L64,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L64)->header.tag=CHAR_tag;
       L63 = (dSTR)((CHAR_boxed) L64);
       ((CHAR_boxed) L63)->immutable_part = '\n';
       plus_s5 = L63;
       stderr_self5 = ((FILE1) NULL);
       L66=ZALLOC(sizeof(struct FILE1_struct));
       if (L66==NULL) FATAL("Unable to allocate more memory");
       ((OB)L66)->header.tag=FILE1_tag;
       L65 = ((FILE1) L66);
       r7 = L65;
       
       SATTR(r7,fp,stderr);
       ret_val24 = r7;
       FILE_plus_dSTR(ret_val24, plus_s5);
       goto after_loop2;
      }
      aget_self5 = fs;
      aget_ind5 = pos;
      L691_=ARR((FSTR)aget_self5,aget_ind5); 
      ret_val25 = L691_;
      L70 = ret_val25;
      L711_=L70=='*'; 
      if (L711_) {
       aget_self6 = fs;
       L721_=INTPLUS(pos,1); 
       aget_ind6 = L721_;
       L731_=ARR((FSTR)aget_self6,aget_ind6); 
       ret_val26 = L731_;
       L74 = ret_val26;
       L751_=L74==')'; 
       L68 = L751_;
      } else {
       L68 = FALSE;
      }
      if (L68) {
       goto after_loop2;
      }
     }
     after_loop2: ;
     L761_=INTPLUS(pos,1); 
     pos = L761_;
    }
    else {
     plus_self8 = tok;
     plus_arg1 = '(';
     str_self1 = plus_arg1;
     create_self4 = ((STR) NULL);
     create_c1 = str_self1;
     L78 = 1;
     L80=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L78)*sizeof(CHAR);
     L79=ZALLOC_LEAF_BIG(L80);
     if (L79==NULL) FATAL("Unable to allocate more memory");
     memset(L79,0,L80);
     ((OB)L79)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L79)->header.destroyed=0;
#endif

     L77 = ((STR) L79);
     L77->asize = L78;
     r8 = L77;
     SARR((STR)r8,0,create_c1); 
     ;
     ret_val29 = r8;
     ret_val28 = ret_val29;
     s1 = ret_val28;
     ret_val27 = STR_ap1077157958(plus_self8, s1, TRUE);
     tok = ret_val27;
    }
   }
   else {
    L831_=c=='\"'; 
    if (L831_) {
     start1 = pos;
     while (1) {
      L841_=INTPLUS(pos,1); 
      pos = L841_;
      L85 = FSTR_sizerINT(fs);
      L861_=INTMINUS(L85,1); 
      L87 = L861_;
      L881_=(pos)<(L87); 
      L89 = L881_;
      L901_=!(L89); 
      if (L901_) {
       create_self5 = ((ERR1) NULL);
       ret_val30 = create_self5;
       plus_self9 = ret_val30;
       plus_s6 = ((dSTR) ((STR) &Unterm919914180));
       stderr_self6 = ((FILE1) NULL);
       L92=ZALLOC(sizeof(struct FILE1_struct));
       if (L92==NULL) FATAL("Unable to allocate more memory");
       ((OB)L92)->header.tag=FILE1_tag;
       L91 = ((FILE1) L92);
       r9 = L91;
       
       SATTR(r9,fp,stderr);
       ret_val32 = r9;
       FILE_plus_dSTR(ret_val32, plus_s6);
       ret_val31 = plus_self9;
       plus_self10 = ret_val31;
       plus_s7 = ((dSTR) name111);
       stderr_self7 = ((FILE1) NULL);
       L95=ZALLOC(sizeof(struct FILE1_struct));
       if (L95==NULL) FATAL("Unable to allocate more memory");
       ((OB)L95)->header.tag=FILE1_tag;
       L94 = ((FILE1) L95);
       r10 = L94;
       
       SATTR(r10,fp,stderr);
       ret_val34 = r10;
       FILE_plus_dSTR(ret_val34, plus_s7);
       ret_val33 = plus_self10;
       plus_self11 = ret_val33;
       L98=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L98==NULL) FATAL("Unable to allocate more memory");
       memset(L98,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L98)->header.tag=CHAR_tag;
       L97 = (dSTR)((CHAR_boxed) L98);
       ((CHAR_boxed) L97)->immutable_part = '\n';
       plus_s8 = L97;
       stderr_self8 = ((FILE1) NULL);
       L100=ZALLOC(sizeof(struct FILE1_struct));
       if (L100==NULL) FATAL("Unable to allocate more memory");
       ((OB)L100)->header.tag=FILE1_tag;
       L99 = ((FILE1) L100);
       r11 = L99;
       
       SATTR(r11,fp,stderr);
       ret_val35 = r11;
       FILE_plus_dSTR(ret_val35, plus_s8);
       goto after_loop3;
      }
      aget_self7 = fs;
      aget_ind7 = pos;
      L1021_=ARR((FSTR)aget_self7,aget_ind7); 
      ret_val36 = L1021_;
      L103 = ret_val36;
      L1041_=L103=='\"'; 
      if (L1041_) {
       goto after_loop3;
      }
     }
     after_loop3: ;
     L1051_=INTMINUS(pos,start1); 
     L106 = L1051_;
     L1071_=INTPLUS(L106,1); 
     str_self2 = FSTR_s1920457899(fs, start1, L1071_);
     ret_val37 = STR_fr1097270334(((STR) NULL), str_self2);
     tok = ret_val37;
     tok = STR_as1755591216(tok);
     tok = GET_OP1568448036(self, name111, ((dSTR) tok));
     cl = FLISTS1425610062(cl, tok);
     tok = ((STR) &name1);
     L1081_=INTPLUS(pos,1); 
     pos = L1081_;
    }
    else {
     L1091_=c=='$'; 
     if (L1091_) {
      aget_self8 = fs;
      L1121_=INTPLUS(pos,1); 
      aget_ind8 = L1121_;
      L1131_=ARR((FSTR)aget_self8,aget_ind8); 
      ret_val38 = L1131_;
      L114 = ret_val38;
      L1151_=L114=='('; 
      if (L1151_) {
       L110 = TRUE;
      } else {
       aget_self9 = fs;
       L1161_=INTPLUS(pos,1); 
       aget_ind9 = L1161_;
       L1171_=ARR((FSTR)aget_self9,aget_ind9); 
       ret_val39 = L1171_;
       L118 = ret_val39;
       L1191_=L118=='{'; 
       L110 = L1191_;
      }
      if (L110) {
       plus_self12 = tok;
       L120 = &pos;
       L121 = GET_OP1833245241(self, name111, ((dSTR) fs), L120);
       plus_sarg = L121;
       ret_val40 = STR_ap2004550586(plus_self12, plus_sarg);
       tok = ret_val40;
       L122 = (tok==((STR) NULL));
       L1231_=!(L122); 
       if (L1231_) {
        while (1) {
         aget_self10 = fs;
         aget_ind10 = pos;
         L1251_=ARR((FSTR)aget_self10,aget_ind10); 
         ret_val41 = L1251_;
         L126 = CHAR_i84735023(ret_val41);
         L1271_=!(L126); 
         if (L1271_) {
          L128 = FSTR_sizerINT(fs);
          L1291_=(pos)<(L128); 
          L124 = L1291_;
         } else {
          L124 = FALSE;
         }
         if (L124) {
         }
         else {
          goto after_loop4;
         }
         plus_self13 = tok;
         aget_self11 = fs;
         aget_ind11 = pos;
         L1301_=ARR((FSTR)aget_self11,aget_ind11); 
         ret_val42 = L1301_;
         plus_arg2 = ret_val42;
         str_self3 = plus_arg2;
         create_self6 = ((STR) NULL);
         create_c2 = str_self3;
         L132 = 1;
         L134=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L132)*sizeof(CHAR);
         L133=ZALLOC_LEAF_BIG(L134);
         if (L133==NULL) FATAL("Unable to allocate more memory");
         memset(L133,0,L134);
         ((OB)L133)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L133)->header.destroyed=0;
#endif

         L131 = ((STR) L133);
         L131->asize = L132;
         r12 = L131;
         SARR((STR)r12,0,create_c2); 
         ;
         ret_val45 = r12;
         ret_val44 = ret_val45;
         s2 = ret_val44;
         ret_val43 = STR_ap1077157958(plus_self13, s2, TRUE);
         tok = ret_val43;
         L1361_=INTPLUS(pos,1); 
         pos = L1361_;
        }
        after_loop4: ;
        cl = FLISTS1425610062(cl, tok);
       }
       tok = ((STR) &name1);
      }
      else {
       plus_self14 = tok;
       plus_arg3 = '$';
       str_self4 = plus_arg3;
       create_self7 = ((STR) NULL);
       create_c3 = str_self4;
       L138 = 1;
       L140=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L138)*sizeof(CHAR);
       L139=ZALLOC_LEAF_BIG(L140);
       if (L139==NULL) FATAL("Unable to allocate more memory");
       memset(L139,0,L140);
       ((OB)L139)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L139)->header.destroyed=0;
#endif

       L137 = ((STR) L139);
       L137->asize = L138;
       r13 = L137;
       SARR((STR)r13,0,create_c3); 
       ;
       ret_val48 = r13;
       ret_val47 = ret_val48;
       s3 = ret_val47;
       ret_val46 = STR_ap1077157958(plus_self14, s3, TRUE);
       tok = ret_val46;
      }
     }
     else {
      L142 = CHAR_i84735023(c);
      L1431_=!(L142); 
      if (L1431_) {
       plus_self15 = tok;
       plus_arg4 = c;
       str_self5 = plus_arg4;
       create_self8 = ((STR) NULL);
       create_c4 = str_self5;
       L145 = 1;
       L147=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L145)*sizeof(CHAR);
       L146=ZALLOC_LEAF_BIG(L147);
       if (L146==NULL) FATAL("Unable to allocate more memory");
       memset(L146,0,L147);
       ((OB)L146)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L146)->header.destroyed=0;
#endif

       L144 = ((STR) L146);
       L144->asize = L145;
       r14 = L144;
       SARR((STR)r14,0,create_c4); 
       ;
       ret_val51 = r14;
       ret_val50 = ret_val51;
       s4 = ret_val50;
       ret_val49 = STR_ap1077157958(plus_self15, s4, TRUE);
       tok = ret_val49;
      }
      else {
       L149 = STR_is111530248(tok, ((STR) &name1));
       L1501_=!(L149); 
       if (L1501_) {
        if (GET_OP1847571144(self, tok)) {
         L1521_=ARR((STR)tok,0); 
         L153 = L1521_;
         L1541_=L153=='/'; 
         L155 = L1541_;
         L1561_=!(L155); 
         L151 = L1561_;
        } else {
         L151 = FALSE;
        }
        if (L151) {
         plus_self16 = wd;
         plus_arg5 = '/';
         str_self6 = plus_arg5;
         create_self9 = ((STR) NULL);
         create_c5 = str_self6;
         L158 = 1;
         L160=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L158)*sizeof(CHAR);
         L159=ZALLOC_LEAF_BIG(L160);
         if (L159==NULL) FATAL("Unable to allocate more memory");
         memset(L159,0,L160);
         ((OB)L159)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L159)->header.destroyed=0;
#endif

         L157 = ((STR) L159);
         L157->asize = L158;
         r15 = L157;
         SARR((STR)r15,0,create_c5); 
         ;
         ret_val54 = r15;
         ret_val53 = ret_val54;
         s5 = ret_val53;
         ret_val52 = STR_ap1077157958(plus_self16, s5, TRUE);
         plus_self17 = ret_val52;
         plus_sarg1 = tok;
         ret_val55 = STR_ap2004550586(plus_self17, plus_sarg1);
         tok = ret_val55;
        }
        if (GET_OP1847571144(self, tok)) {
         SATTR(self,module_inclusion,FMAPST859022540(ATTR(self,module_inclusion), tok, name111));
        }
        cl = FLISTS1425610062(cl, tok);
        tok = ((STR) &name1);
       }
      }
     }
    }
   }
  }
  L1631_=INTPLUS(pos,1); 
  pos = L1631_;
 }
 after_loop: ;
 ret_val = cl;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IDENT IDENT_1150413730(IDENT self, STR s) {
 IDENT ret_val = IDENT_zero;
 IDENT i = IDENT_zero;
 if ((s==((STR) NULL))) {
  ret_val = IDENT_zero;
  return ret_val;
 }
 i = IDENT_512720063(IDENT_ident_tbl, s);
 if ((i.str==(STR)0)) {
  i.str = s;
  IDENT_ident_tbl = IDENT_81729787(IDENT_ident_tbl, i);
 }
 ret_val = i;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC IFC_AB622464468(IFC_ABS_CREATE self, TP_CLASS t) {
 IFC ret_val;
 IFC_ABS_CREATE ic;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val7;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val8;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val9;
 IFC create_self1;
 SIG_TBL create_sigs;
 dTP create_tp;
 IFC ret_val10;
 IFC r3;
 extern STR Abstra1892527628;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name23;
 FILE1 L7;
 OB L8;
 IFC_ABS_CREATE L10;
 OB L11;
 dPARSE L12;
 IDENT L13;
 ARRAYdTP L14;
 INT L151_;
 dPARSE L16;
 extern STR Compil583698995;
 extern STR name18;
 IFC L17;
 OB L18;
 if (ATTR(ATTR(t,prog),show_ifc_abs_cre)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &Abstra1892527628);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = TP_CLASS_strrSTR(t);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name23);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
 }
 L11=ZALLOC(sizeof(struct IFC_ABS_CREATE_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=IFC_ABS_CREATE_tag;
 L10 = ((IFC_ABS_CREATE) L11);
 ic = L10;
 SATTR(ic,tp,t);
 SATTR(ic,con,TP_CLA821242177(t));
 if ((ATTR(ic,con)==((TP_CONTEXT) NULL))) {
  ret_val = ((IFC) NULL);
  return ret_val;
 }
 as_tree_self = t;
 if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
  L12 = ATTR(ATTR(as_tree_self,prog),parse);
  L13 = ATTR(as_tree_self,name1);
  L14 = ATTR(as_tree_self,params);
  L151_=(L14)==NULL?0:ASIZE((ARRAYdTP)L14); 
  L16 = L12;
  SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L16)])(L16, L13, L151_));
 }
 ret_val7 = ATTR(as_tree_self,my_as_tree);
 SATTR(ic,tr,ret_val7);
 if ((ATTR(ic,tr)==((AS_CLASS_DEF) NULL))) {
  plus_self3 = ((STR) &Compil583698995);
  plus_sarg = TP_CLASS_strrSTR(t);
  ret_val8 = STR_ap2004550586(plus_self3, plus_sarg);
  plus_self4 = ret_val8;
  plus_sarg1 = ((STR) &name18);
  ret_val9 = STR_ap2004550586(plus_self4, plus_sarg1);
  PROG_err_STR(ATTR(t,prog), ret_val9);
  ret_val = ((IFC) NULL);
  return ret_val;
 }
 IFC_AB1878942039(ic);
 IFC_AB1563215475(ic);
 create_self1 = ((IFC) NULL);
 create_sigs = IFC_AB597363174(ic);
 create_tp = ((dTP) t);
 L18=ZALLOC(sizeof(struct IFC_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=IFC_tag;
 L17 = ((IFC) L18);
 r3 = L17;
 SATTR(r3,sigs,create_sigs);
 SATTR(r3,tp,create_tp);
 ret_val10 = r3;
 ret_val = ret_val10;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC IFC_if331163657(IFC self, TP_CLASS t) {
 IFC ret_val;
 IFC res;
 TUPIDENTINT cq = TUPIDENTINT_zero;
 IMPL im;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 ARRAYdTP L1;
 INT L21_;
 TUPIDENTINT L3;
 TUPIDENTINT L4;
 BOOL L5;
 BOOL L61_;
 res = ((IFC) NULL);
 if (TP_CLA1568240525(t)) {
  create_self = TUPIDENTINT_zero;
  create_at1 = ATTR(t,name1);
  L1 = ATTR(t,params);
  L21_=(L1)==NULL?0:ASIZE((ARRAYdTP)L1); 
  create_at2 = L21_;
  L3 = create_self;
  L3.t1 = create_at1;
  L4 = L3;
  L4.t2 = create_at2;
  ret_val1 = L4;
  cq = ret_val1;
  if (FSETTU645397512(IFC_abs_cur, cq)) {
   IFC_cycle_err(self);
   IFC_abs_cur = FSETTU419377758(IFC_abs_cur);
  }
  else {
   IFC_abs_cur = FSETTU2045052674(IFC_abs_cur, cq);
   res = IFC_AB622464468(((IFC_ABS_CREATE) NULL), t);
   IFC_abs_cur = FSETTU1559268184(IFC_abs_cur, cq);
  }
 }
 else {
  im = TP_CLA1109727233(t);
  L5 = (im==((IMPL) NULL));
  L61_=!(L5); 
  if (L61_) {
   res = ATTR(im,ifc);
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT IDENT_hashrINT(IDENT self) {
 INT ret_val = INT_zero;
 INT x = INT_zero;
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 L1 = self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val = L51_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT IMPL_a1897060099(IMPL self) {
 INT ret_val = INT_zero;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT asze;
 dAS_CLASS_ELT as;
 dAS_EXPR ai;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 dSTR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 IMPL prog_self;
 PROG ret_val7;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val8;
 UNIX exit_self;
 INT exit_code = INT_zero;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self4;
 STR plus_s3;
 OUT ret_val10;
 FILE1 stdout_self3;
 FILE1 ret_val11;
 FILE1 r3;
 OUT plus_self5;
 dSTR plus_s4;
 OUT ret_val12;
 FILE1 stdout_self4;
 FILE1 ret_val13;
 FILE1 r4;
 OUT plus_self6;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val14;
 FILE1 r5;
 IMPL prog_self1;
 PROG ret_val15;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self7;
 STR plus_sarg1;
 STR ret_val16;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 ELT L3;
 BOOL L4;
 dTP L5;
 extern STR TYPE1;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR name9;
 FILE1 L13;
 OB L14;
 dTP L16;
 extern STR asizef934973982;
 extern STR Intern28965746;
 extern STR TYPE1;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 dTP L27;
 extern STR asizef934973982;
 extern STR Intern28965746;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = ATTR(self,elts);
  L21 = IDENT_1891587148;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   asze = L3;
   if (ELT_is1269015106(asze)) {
    L5 = ATTR(ATTR(asze,sig1),ret);
    L4 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUILTIN_int));
   } else {
    L4 = FALSE;
   }
   if (L4) {
    as = ATTR(asze,as);
    if (as==NULL) {
     goto other350;
    } else
    switch (TAG(as)) {
     case AS_CONST_DEF_tag:
      ai = ATTR(((AS_CONST_DEF) as),init);
      if (ai==NULL) {
       goto other351;
      } else
      switch (TAG(ai)) {
       case AS_INT_LIT_EXPR_tag:
        ret_val = INTI_intrINT(ATTR(((AS_INT_LIT_EXPR) ai),val1));
        return ret_val; break;
       default: ;
       other351: ;
        create_self = ((OUT) NULL);
        ret_val1 = create_self;
        plus_self = ret_val1;
        plus_s = ((STR) &TYPE1);
        stdout_self = ((FILE1) NULL);
        L7=ZALLOC(sizeof(struct FILE1_struct));
        if (L7==NULL) FATAL("Unable to allocate more memory");
        ((OB)L7)->header.tag=FILE1_tag;
        L6 = ((FILE1) L7);
        r = L6;
        
        SATTR(r,fp,stdout);
        ret_val3 = r;
        FILE_plus_STR(ret_val3, plus_s);
        ret_val2 = plus_self;
        plus_self1 = ret_val2;
        plus_s1 = ((dSTR) ATTR(self,tp));
        stdout_self1 = ((FILE1) NULL);
        L10=ZALLOC(sizeof(struct FILE1_struct));
        if (L10==NULL) FATAL("Unable to allocate more memory");
        ((OB)L10)->header.tag=FILE1_tag;
        L9 = ((FILE1) L10);
        r1 = L9;
        
        SATTR(r1,fp,stdout);
        ret_val5 = r1;
        FILE_plus_dSTR(ret_val5, plus_s1);
        ret_val4 = plus_self1;
        plus_self2 = ret_val4;
        plus_s2 = ((STR) &name9);
        stdout_self2 = ((FILE1) NULL);
        L14=ZALLOC(sizeof(struct FILE1_struct));
        if (L14==NULL) FATAL("Unable to allocate more memory");
        ((OB)L14)->header.tag=FILE1_tag;
        L13 = ((FILE1) L14);
        r2 = L13;
        
        SATTR(r2,fp,stdout);
        ret_val6 = r2;
        FILE_plus_STR(ret_val6, plus_s2);
        prog_self = self;
        L16 = ATTR(prog_self,tp);
        ret_val7 = (*dTP_progrPROG[TAG(L16)])(L16);
        barf_self = ret_val7;
        barf_msg = ((STR) &asizef934973982);
        barf_at_self = barf_self;
        barf_at_msg = barf_msg;
        barf_at_at = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self, barf_at_at);
        plus_self3 = ((STR) &Intern28965746);
        plus_sarg = barf_at_msg;
        ret_val8 = STR_ap2004550586(plus_self3, plus_sarg);
        PROG_err_STR(barf_at_self, ret_val8);
        exit_self = ((UNIX) NULL);
        exit_code = 1;
        exit(exit_code);
      } break;
     default: ;
     other350: ;
      create_self1 = ((OUT) NULL);
      ret_val9 = create_self1;
      plus_self4 = ret_val9;
      plus_s3 = ((STR) &TYPE1);
      stdout_self3 = ((FILE1) NULL);
      L18=ZALLOC(sizeof(struct FILE1_struct));
      if (L18==NULL) FATAL("Unable to allocate more memory");
      ((OB)L18)->header.tag=FILE1_tag;
      L17 = ((FILE1) L18);
      r3 = L17;
      
      SATTR(r3,fp,stdout);
      ret_val11 = r3;
      FILE_plus_STR(ret_val11, plus_s3);
      ret_val10 = plus_self4;
      plus_self5 = ret_val10;
      plus_s4 = ((dSTR) ATTR(self,tp));
      stdout_self4 = ((FILE1) NULL);
      L22=ZALLOC(sizeof(struct FILE1_struct));
      if (L22==NULL) FATAL("Unable to allocate more memory");
      ((OB)L22)->header.tag=FILE1_tag;
      L20 = ((FILE1) L22);
      r4 = L20;
      
      SATTR(r4,fp,stdout);
      ret_val13 = r4;
      FILE_plus_dSTR(ret_val13, plus_s4);
      ret_val12 = plus_self5;
      plus_self6 = ret_val12;
      plus_s5 = ((STR) &name9);
      stdout_self5 = ((FILE1) NULL);
      L25=ZALLOC(sizeof(struct FILE1_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=FILE1_tag;
      L24 = ((FILE1) L25);
      r5 = L24;
      
      SATTR(r5,fp,stdout);
      ret_val14 = r5;
      FILE_plus_STR(ret_val14, plus_s5);
      prog_self1 = self;
      L27 = ATTR(prog_self1,tp);
      ret_val15 = (*dTP_progrPROG[TAG(L27)])(L27);
      barf_self1 = ret_val15;
      barf_msg1 = ((STR) &asizef934973982);
      barf_at_self1 = barf_self1;
      barf_at_msg1 = barf_msg1;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self7 = ((STR) &Intern28965746);
      plus_sarg1 = barf_at_msg1;
      ret_val16 = STR_ap2004550586(plus_self7, plus_sarg1);
      PROG_err_STR(barf_at_self1, ret_val16);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
    }
   }
   else {
    ret_val = -1;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = -1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IFC_no472265690(IFC self, IFC i) {
 SIG ret_val;
 SIG_TBL L11;
 SIG s;
 SIG L2;
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  L11 = ATTR(i,sigs);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   s = L2;
   if ((IFC_si2085565042(self, s)==((SIG) NULL))) {
    ret_val = s;
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

SIG IFC_se383132405(IFC self, SIG c, FLISTSIG sig_list, BOOL internal_call) {
 SIG ret_val;
 SIG winner_sig = ((SIG) NULL);
 FSETSIG winners = ((FSETSIG) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG cba;
 FSETSIG pos_winner_sigs = ((FSETSIG) NULL);
 FLISTSIG L31;
 SIG s;
 ARG sa1;
 dMODE L41;
 FLISTSIG L51;
 SIG s1;
 STR err_msg;
 FSETSIG L61;
 SIG s2;
 STR L71;
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
 INT L8;
 BOOL L91_;
 ARRAYARG L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L101_,L101_m;
 INT L16;
 SIG L17;
 ARRAYARG L18;
 ARG L191_;
 SIG L20;
 ARRAYARG L22;
 ARG L231_;
 dMODE L24;
 dTP L25;
 dMODE L26;
 dTP L27;
 dMODE L28;
 dMODE L29;
 dTP L30;
 SIG L32;
 BOOL L33;
 ARRAYARG L34;
 ARG L351_;
 ARG L36;
 ARRAYARG L37;
 ARG L381_;
 ARG L39;
 dTP L40;
 INT L42;
 BOOL L431_;
 extern STR name1;
 extern STR name37;
 SIG L44;
 STR L45;
 dTP L46;
 PROG L47;
 extern STR Incorr1937141046;
 INT L48;
 BOOL L491_;
 dTP L50;
 PROG L52;
 extern STR Nomatc540980171;
 dTP L53;
 PROG L54;
 extern STR Nomatc888131830;
 INT L55;
 BOOL L561_;
 INT L57;
 BOOL L581_;
 L8 = FLISTS228922656(sig_list);
 L91_=(1)<(L8); 
 if (L91_) {
  {
   BOOL f_L101_ = TRUE;
   L11 = 0;
   L12 = ATTR(c,args);
   L131_=(L12)==NULL?0:ASIZE((ARRAYARG)L12); 
   L14 = L131_;
   L151_=INTMINUS(L14,1); 
   L21 = L151_;
   L101_=L11-1;L101_m=L21; 
   while (1) {
    if(L101_++>=L101_m)  goto after_loop; 
    i = L101_;
    L17=FLISTSIG_toprSIG(sig_list);
    L18 = ATTR(L17,args);
    L191_=(ARG)ARR((ARRAYARG)L18,i); 
    cba = L191_;
    pos_winner_sigs = FSETSI461354464(pos_winner_sigs);
    {
     struct FLISTS1074219534_frame_struct other2_0;
FLISTS1074219534_frame noname1 = &other2_0;
     L31 = sig_list;
     noname1->self = L31;
     noname1->state = 0;
     while (1) {
      L20 = FLISTS1074219534(noname1);
      if (noname1->state == -1) {
       goto after_loop1;
      }
      s = L20;
      L22 = ATTR(s,args);
      L231_=(ARG)ARR((ARRAYARG)L22,i); 
      sa1 = L231_;
      L41 = ATTR(sa1,mode1);
      L24 = L41;
      if ((*dMODE_814247358[TAG(L24)])(L24, ((dMODE) MODES_in_mode))) {
       L25 = ATTR(sa1,tp);
       if ((*dTP_is1999456142[TAG(L25)])(L25, ATTR(cba,tp))) {
        cba = sa1;
       }
      }
      else {
       L26 = L41;
       if ((*dMODE_814247358[TAG(L26)])(L26, ((dMODE) MODES_out_mode))) {
        L27 = ATTR(cba,tp);
        if ((*dTP_is1999456142[TAG(L27)])(L27, ATTR(sa1,tp))) {
         cba = sa1;
        }
       }
       else {
        L28 = L41;
        if ((*dMODE_814247358[TAG(L28)])(L28, ((dMODE) MODES_inout_mode))) {
        }
        else {
         L29 = L41;
         if ((*dMODE_814247358[TAG(L29)])(L29, ((dMODE) MODES_once_mode))) {
          L30 = ATTR(sa1,tp);
          if ((*dTP_is1999456142[TAG(L30)])(L30, ATTR(cba,tp))) {
           cba = sa1;
          }
         }
         else {
          FATAL("No applicable target in case statement\nin IFC::select_sig(SIG,FLIST{SIG},BOOL):SIG\n./Representation/ifc.sa:479:19");
         }
        }
       }
      }
     }
    }
    after_loop1: ;
    {
     struct FLISTS1074219534_frame_struct other3_0;
FLISTS1074219534_frame noname2 = &other3_0;
     L51 = sig_list;
     noname2->self = L51;
     noname2->state = 0;
     while (1) {
      L32 = FLISTS1074219534(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      s1 = L32;
      L34 = ATTR(c,args);
      L351_=(ARG)ARR((ARRAYARG)L34,i); 
      L36=L351_;
      if (IFC_is2032003003(self, ((dCALL_TP) ATTR(L36,tp)))) {
       L33 = TRUE;
      } else {
       L37 = ATTR(s1,args);
       L381_=(ARG)ARR((ARRAYARG)L37,i); 
       L39=L381_;
       L40 = ATTR(L39,tp);
       L33 = (*dTP_is242312711[TAG(L40)])(L40, ((OB) ATTR(cba,tp)));
      }
      if (L33) {
       pos_winner_sigs = FSETSI671175003(pos_winner_sigs, s1);
      }
     }
    }
    after_loop2: ;
    if ((winners==((FSETSIG) NULL))) {
     winners = FSETSI719979460(pos_winner_sigs);
    }
    else {
     winners = FSETSI123770722(winners, pos_winner_sigs);
    }
   }
  }
  after_loop: ;
  L42 = FSETSIG_sizerINT(winners);
  L431_=(1)<(L42); 
  if (L431_) {
   err_msg = ((STR) &name1);
   {
    struct FSETSIG_eltbrSIG_frame_struct other4_0;
FSETSIG_eltbrSIG_frame noname3 = &other4_0;
    struct STR_se405450305_frame_struct other4_1;
STR_se405450305_frame noname4 = &other4_1;
    L61 = winners;
    L71 = ((STR) &name37);
    noname3->self = L61;
    noname3->state = 0;
    noname4->self = L71;
    noname4->state = 0;
    while (1) {
     L44 = FSETSIG_eltbrSIG(noname3);
     if (noname3->state == -1) {
      goto after_loop3;
     }
     s2 = L44;
     plus_self = err_msg;
     noname4->arg1 = ((dSTR) ATTR(s2,str));
     L45 = STR_se405450305(noname4);
     if (noname4->state == -1) {
      goto after_loop3;
     }
     plus_sarg = L45;
     ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
     err_msg = ret_val1;
    }
   }
   after_loop3: ;
   L46 = ATTR(c,tp);
   L47 = (*dTP_progrPROG[TAG(L46)])(L46);
   plus_self1 = ((STR) &Incorr1937141046);
   plus_sarg1 = err_msg;
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   PROG_err_STR(L47, ret_val2);
   ret_val = ((SIG) NULL);
   return ret_val;
  }
  else {
   L48 = FSETSIG_sizerINT(winners);
   L491_=(L48)==(0); 
   if (L491_) {
    if (internal_call) {
     L50 = ATTR(c,tp);
     L52 = (*dTP_progrPROG[TAG(L50)])(L50);
     plus_self2 = ((STR) &Nomatc540980171);
     plus_sarg2 = ATTR(c,str);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     PROG_err_STR(L52, ret_val3);
    }
    else {
     L53 = ATTR(c,tp);
     L54 = (*dTP_progrPROG[TAG(L53)])(L53);
     plus_self3 = ((STR) &Nomatc888131830);
     plus_sarg3 = ATTR(c,str);
     ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(L54, ret_val4);
    }
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   else {
    L55 = FSETSIG_sizerINT(winners);
    L561_=(L55)==(1); 
    if (L561_) {
     winner_sig = FSETSI2020712453(winners);
    }
   }
  }
 }
 else {
  L57 = FLISTS228922656(sig_list);
  L581_=(L57)==(1); 
  if (L581_) {
   winner_sig = FLISTSIG_toprSIG(sig_list);
  }
 }
 ret_val = winner_sig;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IFC_se461463192(IFC self, CALL_SIG c, FLISTSIG sig_list, BOOL internal_call) {
 SIG ret_val;
 SIG winner_sig = ((SIG) NULL);
 FSETSIG winners = ((FSETSIG) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG cba;
 FSETSIG pos_winner_sigs = ((FSETSIG) NULL);
 FLISTSIG L31;
 SIG s;
 ARG sa1;
 dMODE L41;
 FLISTSIG L51;
 SIG s1;
 STR err_msg;
 FSETSIG L61;
 SIG s2;
 STR L71;
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
 INT L8;
 BOOL L91_;
 ARRAYCALL_ARG L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L101_,L101_m;
 INT L16;
 SIG L17;
 ARRAYARG L18;
 ARG L191_;
 SIG L20;
 ARRAYARG L22;
 ARG L231_;
 dMODE L24;
 dTP L25;
 dMODE L26;
 dTP L27;
 dMODE L28;
 dMODE L29;
 dTP L30;
 SIG L32;
 BOOL L33;
 ARRAYCALL_ARG L34;
 CALL_ARG L351_;
 CALL_ARG L36;
 ARRAYARG L37;
 ARG L381_;
 ARG L39;
 dTP L40;
 INT L42;
 BOOL L431_;
 extern STR name1;
 extern STR name37;
 SIG L44;
 STR L45;
 dTP L46;
 PROG L47;
 extern STR Incorr1937141046;
 INT L48;
 BOOL L491_;
 dTP L50;
 PROG L52;
 extern STR Nomatc540980171;
 dTP L53;
 PROG L54;
 extern STR Nomatc888131830;
 INT L55;
 BOOL L561_;
 INT L57;
 BOOL L581_;
 L8 = FLISTS228922656(sig_list);
 L91_=(1)<(L8); 
 if (L91_) {
  {
   BOOL f_L101_ = TRUE;
   L11 = 0;
   L12 = ATTR(c,args);
   L131_=(L12)==NULL?0:ASIZE((ARRAYCALL_ARG)L12); 
   L14 = L131_;
   L151_=INTMINUS(L14,1); 
   L21 = L151_;
   L101_=L11-1;L101_m=L21; 
   while (1) {
    if(L101_++>=L101_m)  goto after_loop; 
    i = L101_;
    L17=FLISTSIG_toprSIG(sig_list);
    L18 = ATTR(L17,args);
    L191_=(ARG)ARR((ARRAYARG)L18,i); 
    cba = L191_;
    pos_winner_sigs = FSETSI461354464(pos_winner_sigs);
    {
     struct FLISTS1074219534_frame_struct other2_0;
FLISTS1074219534_frame noname1 = &other2_0;
     L31 = sig_list;
     noname1->self = L31;
     noname1->state = 0;
     while (1) {
      L20 = FLISTS1074219534(noname1);
      if (noname1->state == -1) {
       goto after_loop1;
      }
      s = L20;
      L22 = ATTR(s,args);
      L231_=(ARG)ARR((ARRAYARG)L22,i); 
      sa1 = L231_;
      L41 = ATTR(sa1,mode1);
      L24 = L41;
      if ((*dMODE_814247358[TAG(L24)])(L24, ((dMODE) MODES_in_mode))) {
       L25 = ATTR(sa1,tp);
       if ((*dTP_is1999456142[TAG(L25)])(L25, ATTR(cba,tp))) {
        cba = sa1;
       }
      }
      else {
       L26 = L41;
       if ((*dMODE_814247358[TAG(L26)])(L26, ((dMODE) MODES_out_mode))) {
        L27 = ATTR(cba,tp);
        if ((*dTP_is1999456142[TAG(L27)])(L27, ATTR(sa1,tp))) {
         cba = sa1;
        }
       }
       else {
        L28 = L41;
        if ((*dMODE_814247358[TAG(L28)])(L28, ((dMODE) MODES_inout_mode))) {
        }
        else {
         L29 = L41;
         if ((*dMODE_814247358[TAG(L29)])(L29, ((dMODE) MODES_once_mode))) {
          L30 = ATTR(sa1,tp);
          if ((*dTP_is1999456142[TAG(L30)])(L30, ATTR(cba,tp))) {
           cba = sa1;
          }
         }
         else {
          FATAL("No applicable target in case statement\nin IFC::select_sig(CALL_SIG,FLIST{SIG},BOOL):SIG\n./Representation/ifc.sa:378:19");
         }
        }
       }
      }
     }
    }
    after_loop1: ;
    {
     struct FLISTS1074219534_frame_struct other3_0;
FLISTS1074219534_frame noname2 = &other3_0;
     L51 = sig_list;
     noname2->self = L51;
     noname2->state = 0;
     while (1) {
      L32 = FLISTS1074219534(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      s1 = L32;
      L34 = ATTR(c,args);
      L351_=(CALL_ARG)ARR((ARRAYCALL_ARG)L34,i); 
      L36=L351_;
      if (IFC_is2032003003(self, ATTR(L36,tp))) {
       L33 = TRUE;
      } else {
       L37 = ATTR(s1,args);
       L381_=(ARG)ARR((ARRAYARG)L37,i); 
       L39=L381_;
       L40 = ATTR(L39,tp);
       L33 = (*dTP_is242312711[TAG(L40)])(L40, ((OB) ATTR(cba,tp)));
      }
      if (L33) {
       pos_winner_sigs = FSETSI671175003(pos_winner_sigs, s1);
      }
     }
    }
    after_loop2: ;
    if ((winners==((FSETSIG) NULL))) {
     winners = FSETSI719979460(pos_winner_sigs);
    }
    else {
     winners = FSETSI123770722(winners, pos_winner_sigs);
    }
   }
  }
  after_loop: ;
  L42 = FSETSIG_sizerINT(winners);
  L431_=(1)<(L42); 
  if (L431_) {
   err_msg = ((STR) &name1);
   {
    struct FSETSIG_eltbrSIG_frame_struct other4_0;
FSETSIG_eltbrSIG_frame noname3 = &other4_0;
    struct STR_se405450305_frame_struct other4_1;
STR_se405450305_frame noname4 = &other4_1;
    L61 = winners;
    L71 = ((STR) &name37);
    noname3->self = L61;
    noname3->state = 0;
    noname4->self = L71;
    noname4->state = 0;
    while (1) {
     L44 = FSETSIG_eltbrSIG(noname3);
     if (noname3->state == -1) {
      goto after_loop3;
     }
     s2 = L44;
     plus_self = err_msg;
     noname4->arg1 = ((dSTR) ATTR(s2,str));
     L45 = STR_se405450305(noname4);
     if (noname4->state == -1) {
      goto after_loop3;
     }
     plus_sarg = L45;
     ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
     err_msg = ret_val1;
    }
   }
   after_loop3: ;
   L46 = ATTR(c,tp);
   L47 = (*dTP_progrPROG[TAG(L46)])(L46);
   plus_self1 = ((STR) &Incorr1937141046);
   plus_sarg1 = err_msg;
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   PROG_err_STR(L47, ret_val2);
   ret_val = ((SIG) NULL);
   return ret_val;
  }
  else {
   L48 = FSETSIG_sizerINT(winners);
   L491_=(L48)==(0); 
   if (L491_) {
    if (internal_call) {
     L50 = ATTR(c,tp);
     L52 = (*dTP_progrPROG[TAG(L50)])(L50);
     plus_self2 = ((STR) &Nomatc540980171);
     plus_sarg2 = CALL_SIG_strrSTR(c);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     PROG_err_STR(L52, ret_val3);
    }
    else {
     L53 = ATTR(c,tp);
     L54 = (*dTP_progrPROG[TAG(L53)])(L53);
     plus_self3 = ((STR) &Nomatc888131830);
     plus_sarg3 = CALL_SIG_strrSTR(c);
     ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(L54, ret_val4);
    }
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   else {
    L55 = FSETSIG_sizerINT(winners);
    L561_=(L55)==(1); 
    if (L561_) {
     winner_sig = FSETSI2020712453(winners);
    }
   }
  }
 }
 else {
  L57 = FLISTS228922656(sig_list);
  L581_=(L57)==(1); 
  if (L581_) {
   winner_sig = FLISTSIG_toprSIG(sig_list);
  }
 }
 ret_val = winner_sig;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IFC_si178998979(IFC self, CALL_SIG c) {
 SIG ret_val;
 SIG r = ((SIG) NULL);
 FLISTSIG sig_list;
 SIG_TBL L11;
 IDENT L21 = IDENT_zero;
 SIG s;
 BOOL found_one = BOOL_zero;
 STR res;
 SIG_TBL L31;
 IDENT L41 = IDENT_zero;
 SIG s1;
 STR L51;
 STR print_err;
 FLISTSIG create_self;
 FLISTSIG ret_val1;
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
 CALL_SIG prog_self;
 PROG ret_val6;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 SIG L6;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 extern STR name1;
 extern STR or2;
 SIG L14;
 STR L15;
 extern STR name9;
 extern STR Suggest;
 dTP L16;
 extern STR Nomatc888131830;
 create_self = ((FLISTSIG) NULL);
 ret_val1 = ((FLISTSIG) NULL);
 sig_list = ret_val1;
 {
  struct SIG_TB788365833_frame_struct other1_0;
SIG_TB788365833_frame noname1 = &other1_0;
  L11 = ATTR(self,sigs);
  L21 = ATTR(c,name1);
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L6 = SIG_TB788365833(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   s = L6;
   if (CALL_S1637743358(c, s)) {
    if (ATTR(c,unknown_ret)) {
     L7 = TRUE;
    } else {
     L8 = ATTR(c,has_ret);
     L9 = (ATTR(s,ret)==((dTP) NULL));
     L101_=!(L9); 
     L12 = L101_;
     L131_=L8==L12; 
     L7 = L131_;
    }
    if (L7) {
     sig_list = FLISTS69519306(sig_list, s);
    }
   }
  }
 }
 after_loop: ;
 r = IFC_se461463192(self, c, sig_list, FALSE);
 if ((r==((SIG) NULL))) {
  found_one = FALSE;
  res = ((STR) &name1);
  {
   struct SIG_TB788365833_frame_struct other2_0;
SIG_TB788365833_frame noname2 = &other2_0;
   struct STR_se405450305_frame_struct other2_1;
STR_se405450305_frame noname3 = &other2_1;
   L31 = ATTR(self,sigs);
   L41 = ATTR(c,name1);
   L51 = ((STR) &or2);
   noname2->self = L31;
   noname2->arg1 = L41;
   noname2->state = 0;
   noname3->self = L51;
   noname3->state = 0;
   while (1) {
    L14 = SIG_TB788365833(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    s1 = L14;
    found_one = TRUE;
    plus_self = res;
    noname3->arg1 = ((dSTR) ATTR(s1,str));
    L15 = STR_se405450305(noname3);
    if (noname3->state == -1) {
     goto after_loop1;
    }
    plus_sarg = L15;
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    res = ret_val2;
   }
  }
  after_loop1: ;
  print_err = CALL_SIG_strrSTR(c);
  if (found_one) {
   plus_self1 = print_err;
   plus_sarg1 = ((STR) &name9);
   ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val3;
   plus_sarg2 = ((STR) &Suggest);
   ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val4;
   plus_sarg3 = res;
   ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
   print_err = ret_val5;
  }
  prog_self = c;
  L16 = ATTR(prog_self,tp);
  ret_val6 = (*dTP_progrPROG[TAG(L16)])(L16);
  plus_self4 = ((STR) &Nomatc888131830);
  plus_sarg4 = print_err;
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
  PROG_err_STR(ret_val6, ret_val7);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IFC_si2085565042(IFC self, SIG s) {
 SIG ret_val;
 FLISTSIG sig_list = ((FLISTSIG) NULL);
 SIG sig1 = ((SIG) NULL);
 SIG_TBL L11;
 IDENT L21 = IDENT_zero;
 SIG ts;
 FLISTSIG is_empty_self;
 BOOL ret_val1 = BOOL_zero;
 FLISTSIG is_empty_self1;
 BOOL ret_val2 = BOOL_zero;
 SIG L3;
 INT L4;
 BOOL L51_;
 INT L6;
 BOOL L71_;
 {
  struct SIG_TB788365833_frame_struct other1_0;
SIG_TB788365833_frame noname1 = &other1_0;
  L11 = ATTR(self,sigs);
  L21 = ATTR(s,name1);
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = SIG_TB788365833(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   ts = L3;
   if (SIG_co1757762355(ts, s)) {
    if ((sig1==((SIG) NULL))) {
     sig1 = ts;
    }
    else {
     is_empty_self = sig_list;
     L4 = FLISTS228922656(is_empty_self);
     L51_=(L4)==(0); 
     ret_val1 = L51_;
     if (ret_val1) {
      sig_list = FLISTS69519306(sig_list, sig1);
     }
     sig_list = FLISTS69519306(sig_list, ts);
    }
   }
  }
 }
 after_loop: ;
 is_empty_self1 = sig_list;
 L6 = FLISTS228922656(is_empty_self1);
 L71_=(L6)==(0); 
 ret_val2 = L71_;
 if (ret_val2) {
  ret_val = sig1;
  return ret_val;
 }
 else {
  sig1 = IFC_se383132405(self, s, sig_list, FALSE);
 }
 ret_val = sig1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IMPL_i1943552357(IMPL self) {
 SIG ret_val;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT e;
 ELT is_invariant_sel;
 BOOL ret_val1 = BOOL_zero;
 SIG is_invariant_sel1;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS is_eq_self;
 OB is_eq_t;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 ELT L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = ATTR(self,elts);
  L21 = IDENT_2063376192;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   is_invariant_sel = e;
   is_invariant_sel1 = ATTR(is_invariant_sel,sig1);
   if ((ATTR(is_invariant_sel1,args)==((ARRAYARG) NULL))) {
    L7 = (ATTR(is_invariant_sel1,ret)==((dTP) NULL));
    L81_=!(L7); 
    L6 = L81_;
   } else {
    L6 = FALSE;
   }
   if (L6) {
    is_eq_self = TP_BUILTIN_bool;
    is_eq_t = ((OB) ATTR(is_invariant_sel1,ret));
    ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
    L5 = ret_val3;
   } else {
    L5 = FALSE;
   }
   if (L5) {
    is_eq_self1 = ATTR(is_invariant_sel1,name1);
    is_eq_i = IDENT_2063376192;
    ret_val4 = (is_eq_self1.str==is_eq_i.str);
    L4 = ret_val4;
   } else {
    L4 = FALSE;
   }
   ret_val2 = L4;
   ret_val1 = ret_val2;
   if (ret_val1) {
    ret_val = ATTR(e,sig1);
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

SIG IMPL_s1294117947(IMPL self, CALL_SIG c, FLISTSIG sig_list, BOOL internal_call) {
 SIG ret_val;
 SIG winner_sig = ((SIG) NULL);
 FSETSIG winners = ((FSETSIG) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG cba;
 FSETSIG pos_winner_sigs = ((FSETSIG) NULL);
 FLISTSIG L31;
 SIG s;
 ARG sa1;
 dMODE L41;
 FLISTSIG L51;
 SIG s1;
 STR err_msg;
 FSETSIG L61;
 SIG s2;
 STR L71;
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
 INT L8;
 BOOL L91_;
 ARRAYCALL_ARG L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L101_,L101_m;
 INT L16;
 SIG L17;
 ARRAYARG L18;
 ARG L191_;
 SIG L20;
 ARRAYARG L22;
 ARG L231_;
 dMODE L24;
 dTP L25;
 dMODE L26;
 dTP L27;
 dMODE L28;
 dMODE L29;
 dTP L30;
 SIG L32;
 BOOL L33;
 ARRAYCALL_ARG L34;
 CALL_ARG L351_;
 CALL_ARG L36;
 ARRAYARG L37;
 ARG L381_;
 ARG L39;
 dTP L40;
 INT L42;
 BOOL L431_;
 extern STR name1;
 extern STR name37;
 SIG L44;
 STR L45;
 dTP L46;
 PROG L47;
 extern STR Incorr1937141046;
 INT L48;
 BOOL L491_;
 dTP L50;
 PROG L52;
 extern STR Nomatc540980171;
 dTP L53;
 PROG L54;
 extern STR Nomatc888131830;
 INT L55;
 BOOL L561_;
 INT L57;
 BOOL L581_;
 L8 = FLISTS228922656(sig_list);
 L91_=(1)<(L8); 
 if (L91_) {
  {
   BOOL f_L101_ = TRUE;
   L11 = 0;
   L12 = ATTR(c,args);
   L131_=(L12)==NULL?0:ASIZE((ARRAYCALL_ARG)L12); 
   L14 = L131_;
   L151_=INTMINUS(L14,1); 
   L21 = L151_;
   L101_=L11-1;L101_m=L21; 
   while (1) {
    if(L101_++>=L101_m)  goto after_loop; 
    i = L101_;
    L17=FLISTSIG_toprSIG(sig_list);
    L18 = ATTR(L17,args);
    L191_=(ARG)ARR((ARRAYARG)L18,i); 
    cba = L191_;
    pos_winner_sigs = FSETSI461354464(pos_winner_sigs);
    {
     struct FLISTS1074219534_frame_struct other2_0;
FLISTS1074219534_frame noname1 = &other2_0;
     L31 = sig_list;
     noname1->self = L31;
     noname1->state = 0;
     while (1) {
      L20 = FLISTS1074219534(noname1);
      if (noname1->state == -1) {
       goto after_loop1;
      }
      s = L20;
      L22 = ATTR(s,args);
      L231_=(ARG)ARR((ARRAYARG)L22,i); 
      sa1 = L231_;
      L41 = ATTR(sa1,mode1);
      L24 = L41;
      if ((*dMODE_814247358[TAG(L24)])(L24, ((dMODE) MODES_in_mode))) {
       L25 = ATTR(sa1,tp);
       if ((*dTP_is1999456142[TAG(L25)])(L25, ATTR(cba,tp))) {
        cba = sa1;
       }
      }
      else {
       L26 = L41;
       if ((*dMODE_814247358[TAG(L26)])(L26, ((dMODE) MODES_out_mode))) {
        L27 = ATTR(cba,tp);
        if ((*dTP_is1999456142[TAG(L27)])(L27, ATTR(sa1,tp))) {
         cba = sa1;
        }
       }
       else {
        L28 = L41;
        if ((*dMODE_814247358[TAG(L28)])(L28, ((dMODE) MODES_inout_mode))) {
        }
        else {
         L29 = L41;
         if ((*dMODE_814247358[TAG(L29)])(L29, ((dMODE) MODES_once_mode))) {
          L30 = ATTR(sa1,tp);
          if ((*dTP_is1999456142[TAG(L30)])(L30, ATTR(cba,tp))) {
           cba = sa1;
          }
         }
         else {
          FATAL("No applicable target in case statement\nin IMPL::select_sig(CALL_SIG,FLIST{SIG},BOOL):SIG\n./Representation/ifc.sa:378:19");
         }
        }
       }
      }
     }
    }
    after_loop1: ;
    {
     struct FLISTS1074219534_frame_struct other3_0;
FLISTS1074219534_frame noname2 = &other3_0;
     L51 = sig_list;
     noname2->self = L51;
     noname2->state = 0;
     while (1) {
      L32 = FLISTS1074219534(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      s1 = L32;
      L34 = ATTR(c,args);
      L351_=(CALL_ARG)ARR((ARRAYCALL_ARG)L34,i); 
      L36=L351_;
      if (IMPL_i1397207249(self, ATTR(L36,tp))) {
       L33 = TRUE;
      } else {
       L37 = ATTR(s1,args);
       L381_=(ARG)ARR((ARRAYARG)L37,i); 
       L39=L381_;
       L40 = ATTR(L39,tp);
       L33 = (*dTP_is242312711[TAG(L40)])(L40, ((OB) ATTR(cba,tp)));
      }
      if (L33) {
       pos_winner_sigs = FSETSI671175003(pos_winner_sigs, s1);
      }
     }
    }
    after_loop2: ;
    if ((winners==((FSETSIG) NULL))) {
     winners = FSETSI719979460(pos_winner_sigs);
    }
    else {
     winners = FSETSI123770722(winners, pos_winner_sigs);
    }
   }
  }
  after_loop: ;
  L42 = FSETSIG_sizerINT(winners);
  L431_=(1)<(L42); 
  if (L431_) {
   err_msg = ((STR) &name1);
   {
    struct FSETSIG_eltbrSIG_frame_struct other4_0;
FSETSIG_eltbrSIG_frame noname3 = &other4_0;
    struct STR_se405450305_frame_struct other4_1;
STR_se405450305_frame noname4 = &other4_1;
    L61 = winners;
    L71 = ((STR) &name37);
    noname3->self = L61;
    noname3->state = 0;
    noname4->self = L71;
    noname4->state = 0;
    while (1) {
     L44 = FSETSIG_eltbrSIG(noname3);
     if (noname3->state == -1) {
      goto after_loop3;
     }
     s2 = L44;
     plus_self = err_msg;
     noname4->arg1 = ((dSTR) ATTR(s2,str));
     L45 = STR_se405450305(noname4);
     if (noname4->state == -1) {
      goto after_loop3;
     }
     plus_sarg = L45;
     ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
     err_msg = ret_val1;
    }
   }
   after_loop3: ;
   L46 = ATTR(c,tp);
   L47 = (*dTP_progrPROG[TAG(L46)])(L46);
   plus_self1 = ((STR) &Incorr1937141046);
   plus_sarg1 = err_msg;
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   PROG_err_STR(L47, ret_val2);
   ret_val = ((SIG) NULL);
   return ret_val;
  }
  else {
   L48 = FSETSIG_sizerINT(winners);
   L491_=(L48)==(0); 
   if (L491_) {
    if (internal_call) {
     L50 = ATTR(c,tp);
     L52 = (*dTP_progrPROG[TAG(L50)])(L50);
     plus_self2 = ((STR) &Nomatc540980171);
     plus_sarg2 = CALL_SIG_strrSTR(c);
     ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
     PROG_err_STR(L52, ret_val3);
    }
    else {
     L53 = ATTR(c,tp);
     L54 = (*dTP_progrPROG[TAG(L53)])(L53);
     plus_self3 = ((STR) &Nomatc888131830);
     plus_sarg3 = CALL_SIG_strrSTR(c);
     ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(L54, ret_val4);
    }
    ret_val = ((SIG) NULL);
    return ret_val;
   }
   else {
    L55 = FSETSIG_sizerINT(winners);
    L561_=(L55)==(1); 
    if (L561_) {
     winner_sig = FSETSI2020712453(winners);
    }
   }
  }
 }
 else {
  L57 = FLISTS228922656(sig_list);
  L581_=(L57)==(1); 
  if (L581_) {
   winner_sig = FLISTSIG_toprSIG(sig_list);
  }
 }
 ret_val = winner_sig;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG IMPL_s773282892(IMPL self, CALL_SIG c) {
 SIG ret_val;
 SIG r = ((SIG) NULL);
 FLISTSIG sig_list;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 SIG s;
 BOOL found_one = BOOL_zero;
 STR res;
 ELT_TBL L31;
 IDENT L41 = IDENT_zero;
 SIG s1;
 STR L51;
 STR print_err;
 FLISTSIG create_self;
 FLISTSIG ret_val1;
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
 CALL_SIG prog_self;
 PROG ret_val6;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val7;
 ELT L6;
 ELT L7;
 BOOL L8;
 BOOL L9;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR name1;
 extern STR or2;
 ELT L15;
 ELT L16;
 STR L17;
 extern STR name9;
 extern STR Suggest;
 dTP L18;
 extern STR Nomatc1537033921;
 create_self = ((FLISTSIG) NULL);
 ret_val1 = ((FLISTSIG) NULL);
 sig_list = ret_val1;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = ATTR(self,elts);
  L21 = ATTR(c,name1);
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L6 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L7=L6;
   s = ATTR(L7,sig1);
   if (CALL_S1637743358(c, s)) {
    if (ATTR(c,unknown_ret)) {
     L8 = TRUE;
    } else {
     L9 = ATTR(c,has_ret);
     L10 = (ATTR(s,ret)==((dTP) NULL));
     L121_=!(L10); 
     L13 = L121_;
     L141_=L9==L13; 
     L8 = L141_;
    }
    if (L8) {
     sig_list = FLISTS69519306(sig_list, s);
    }
   }
  }
 }
 after_loop: ;
 r = IMPL_s1294117947(self, c, sig_list, TRUE);
 if ((r==((SIG) NULL))) {
  found_one = FALSE;
  res = ((STR) &name1);
  {
   struct ELT_TB431570775_frame_struct other2_0;
ELT_TB431570775_frame noname2 = &other2_0;
   struct STR_se405450305_frame_struct other2_1;
STR_se405450305_frame noname3 = &other2_1;
   L31 = ATTR(self,elts);
   L41 = ATTR(c,name1);
   L51 = ((STR) &or2);
   noname2->self = L31;
   noname2->arg1 = L41;
   noname2->state = 0;
   noname3->self = L51;
   noname3->state = 0;
   while (1) {
    L15 = ELT_TB431570775(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    L16=L15;
    s1 = ATTR(L16,sig1);
    found_one = TRUE;
    plus_self = res;
    noname3->arg1 = ((dSTR) ATTR(s1,str));
    L17 = STR_se405450305(noname3);
    if (noname3->state == -1) {
     goto after_loop1;
    }
    plus_sarg = L17;
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    res = ret_val2;
   }
  }
  after_loop1: ;
  print_err = CALL_SIG_strrSTR(c);
  if (found_one) {
   plus_self1 = print_err;
   plus_sarg1 = ((STR) &name9);
   ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val3;
   plus_sarg2 = ((STR) &Suggest);
   ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val4;
   plus_sarg3 = res;
   ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
   print_err = ret_val5;
  }
  prog_self = c;
  L18 = ATTR(prog_self,tp);
  ret_val6 = (*dTP_progrPROG[TAG(L18)])(L18);
  plus_self4 = ((STR) &Nomatc1537033921);
  plus_sarg4 = print_err;
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg4);
  PROG_err_STR(ret_val6, ret_val7);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SIG_TBL IFC_AB597363174(IFC_ABS_CREATE self) {
 SIG_TBL ret_val;
 SIG_TBL r = ((SIG_TBL) NULL);
 SIG_TBL L11;
 IFC si;
 SIG_TBL L21;
 SIG sig1;
 INT i = INT_zero;
 SIG cs;
 FLISTIFC is_empty_self;
 BOOL ret_val1 = BOOL_zero;
 FLISTIFC aget_self;
 INT aget_ind = INT_zero;
 IFC ret_val2;
 IFC_ABS_CREATE prog_self;
 PROG ret_val3;
 IFC_ABS_CREATE prog_self1;
 PROG ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 SIG L3;
 INT L4;
 BOOL L51_;
 SIG L6;
 BOOL L7;
 BOOL L81_;
 INT L9;
 BOOL L101_;
 IFC L121_;
 IFC L13;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR Thesignatures;
 extern STR and3;
 extern STR mustbe934284467;
 INT L181_;
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  L11 = ATTR(self,class_sigs);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = SIG_TB1438251609(r, L3);
  }
 }
 after_loop: ;
 while (1) {
  is_empty_self = ATTR(self,supers);
  L4 = FLISTI751030673(is_empty_self);
  L51_=(L4)==(0); 
  ret_val1 = L51_;
  if (ret_val1) {
   goto after_loop1;
  }
  else {
   si = FLISTIFC_poprIFC(ATTR(self,supers));
   {
    struct SIG_TBL_eltbrSIG_frame_struct other3_0;
SIG_TBL_eltbrSIG_frame noname2 = &other3_0;
    L21 = ATTR(si,sigs);
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L6 = SIG_TBL_eltbrSIG(noname2);
     if (noname2->state == -1) {
      goto after_loop2;
     }
     sig1 = L6;
     L7 = (SIG_TB1067194561(r, sig1)==((SIG) NULL));
     L81_=!(L7); 
     if (L81_) {
     }
     else {
      i = 0;
      while (1) {
       L9 = FLISTI751030673(ATTR(self,supers));
       L101_=(i)<(L9); 
       if (L101_) {
       }
       else {
        goto after_loop3;
       }
       aget_self = ATTR(self,supers);
       aget_ind = i;
       L121_=(IFC)ARR((FLISTIFC)aget_self,aget_ind); 
       ret_val2 = L121_;
       L13=ret_val2;
       cs = SIG_TB1067194561(ATTR(L13,sigs), sig1);
       L14 = (cs==((SIG) NULL));
       L151_=!(L14); 
       if (L151_) {
        L16 = SIG_is1893596208(cs, sig1);
        L171_=!(L16); 
        if (L171_) {
         prog_self = self;
         ret_val3 = ATTR(ATTR(prog_self,tp),prog);
         PROG_e176405615(ret_val3, ((dPROG_ERR) ATTR(self,tr)));
         prog_self1 = self;
         ret_val4 = ATTR(ATTR(prog_self1,tp),prog);
         plus_self = ((STR) &Thesignatures);
         plus_sarg = ATTR(sig1,str);
         ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
         plus_self1 = ret_val5;
         plus_sarg1 = ((STR) &and3);
         ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
         plus_self2 = ret_val6;
         plus_sarg2 = ATTR(cs,str);
         ret_val7 = STR_ap2004550586(plus_self2, plus_sarg2);
         plus_self3 = ret_val7;
         plus_sarg3 = ((STR) &mustbe934284467);
         ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
         PROG_err_STR(ret_val4, ret_val8);
        }
       }
       L181_=INTPLUS(i,1); 
       i = L181_;
      }
      after_loop3: ;
      r = SIG_TB1438251609(r, SIG_wi1810596376(sig1, ((dTP) ATTR(self,tp))));
     }
    }
   }
   after_loop2: ;
  }
 }
 after_loop1: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP1568448036(GET_OPTIONS self, STR fname, dSTR arg_str) {
 STR ret_val;
 STR s;
 INT i = INT_zero;
 STR res = ((STR) NULL);
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r;
 dSTR L1;
 INT L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT* L7;
 STR L8;
 CHAR L91_;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 INT L151_;
 L1 = arg_str;
 s = (*dSTR_strrSTR[TAG(L1)])(L1);
 i = 0;
 while (1) {
  L2 = STR_sizerINT(s);
  L31_=(i)<(L2); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L41_=ARR((STR)s,i); 
  L5 = L41_;
  L61_=L5=='$'; 
  if (L61_) {
   plus_self = res;
   L7 = &i;
   L8 = GET_OP1833245241(self, fname, ((dSTR) s), L7);
   plus_sarg = L8;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   res = ret_val1;
  }
  else {
   plus_self1 = res;
   L91_=ARR((STR)s,i); 
   plus_arg = L91_;
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
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
   r = L10;
   SARR((STR)r,0,create_c); 
   ;
   ret_val4 = r;
   ret_val3 = ret_val4;
   s1 = ret_val3;
   ret_val2 = STR_ap1077157958(plus_self1, s1, TRUE);
   res = ret_val2;
   L151_=INTPLUS(i,1); 
   i = L151_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP1767328402(GET_OPTIONS self, STR name111, STR s1, STR s2) {
 STR ret_val;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 extern STR name1;
 BOOL L1;
 BOOL L21_;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 extern STR name20;
 L1 = STR_is111530248(s2, ((STR) &name1));
 L21_=!(L1); 
 if (L21_) {
  plus_self = s1;
  plus_arg = '\n';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
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
  ret_val3 = r;
  ret_val2 = ret_val3;
  s = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val1;
  plus_sarg = name111;
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
  plus_self2 = ret_val4;
  plus_sarg1 = ((STR) &name20);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
  plus_self3 = ret_val5;
  plus_sarg2 = s2;
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
  ret_val = ret_val6;
  return ret_val;
 }
 else {
  ret_val = s1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP1833245241(GET_OPTIONS self, STR fname, dSTR arg_str, INT* pos) {
 INT L1 = *pos;/*Local for arg*/
 STR ret_val;
 STR s;
 STR e = ((STR) NULL);
 CHAR op = CHAR_zero;
 BOOL skip1 = BOOL_zero;
 CHAR cp = CHAR_zero;
 STR tenv;
 ERR1 create_self;
 ERR1 ret_val1;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val2;
 FILE1 stderr_self;
 FILE1 ret_val3;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val4;
 FILE1 stderr_self1;
 FILE1 ret_val5;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val6;
 FILE1 stderr_self2;
 FILE1 ret_val7;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val8;
 FILE1 stderr_self3;
 FILE1 ret_val9;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 ERR1 ret_val10;
 FILE1 stderr_self4;
 FILE1 ret_val11;
 FILE1 r4;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val12;
 FILE1 stderr_self5;
 FILE1 ret_val13;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 ERR1 ret_val14;
 FILE1 stderr_self6;
 FILE1 ret_val15;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s7;
 ERR1 ret_val16;
 FILE1 stderr_self7;
 FILE1 ret_val17;
 FILE1 r7;
 ERR1 plus_self8;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val18;
 FILE1 r8;
 STR plus_self9;
 CHAR plus_arg = CHAR_zero;
 STR ret_val19;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val20;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val21;
 STR r9;
 STR plus_self10;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val22;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val23;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r10;
 STR plus_self11;
 STR plus_sarg;
 STR ret_val25;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val26;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val27;
 STR create_self3;
 CHAR create_c2 = CHAR_zero;
 STR ret_val28;
 STR r11;
 dSTR L2;
 INT L31_;
 INT L4;
 CHAR L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L81_;
 BOOL L91_;
 extern STR name1;
 INT L101_;
 BOOL L11;
 INT L12;
 BOOL L131_;
 CHAR L141_;
 CHAR L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L20;
 BOOL L21;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 CHAR L251_;
 CHAR L26;
 BOOL L271_;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 CHAR L321_;
 CHAR L33;
 BOOL L341_;
 extern STR Unterm889448459;
 FILE1 L35;
 OB L36;
 extern STR name8;
 FILE1 L38;
 OB L39;
 FILE1 L41;
 OB L42;
 extern STR in;
 FILE1 L44;
 OB L45;
 FILE1 L47;
 OB L48;
 extern STR assume;
 FILE1 L50;
 OB L52;
 dSTR L54;
 OB L55;
 FILE1 L56;
 OB L57;
 extern STR name19;
 FILE1 L59;
 OB L60;
 extern STR skipthistoken;
 FILE1 L62;
 OB L63;
 CHAR L651_;
 STR L66;
 INT L67;
 OB L68;
 INT L69;
 INT L721_;
 BOOL L731_;
 INT L74;
 BOOL L751_;
 INT L761_;
 extern STR d1;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 STR L83;
 INT L84;
 OB L85;
 INT L86;
 L2 = arg_str;
 s = (*dSTR_strrSTR[TAG(L2)])(L2);
 L31_=INTPLUS(L1,1); 
 L4 = L31_;
 L51_=ARR((STR)s,L4); 
 op = L51_;
 L71_=op=='{'; 
 if (L71_) {
  L6 = TRUE;
 } else {
  L81_=op=='('; 
  L6 = L81_;
 }
 if (L6) {
  skip1 = FALSE;
  cp = '}';
  L91_=op=='('; 
  if (L91_) {
   cp = ')';
  }
  tenv = ((STR) &name1);
  L101_=INTPLUS(L1,2); 
  L1 = L101_;
  while (1) {
   L12 = STR_sizerINT(s);
   L131_=(L1)<(L12); 
   if (L131_) {
    L141_=ARR((STR)s,L1); 
    L15 = L141_;
    L161_=L15==cp; 
    L17 = L161_;
    L181_=!(L17); 
    L11 = L181_;
   } else {
    L11 = FALSE;
   }
   if (L11) {
   }
   else {
    goto after_loop;
   }
   L221_=ARR((STR)s,L1); 
   L23 = L221_;
   L241_=L23==')'; 
   if (L241_) {
    L21 = TRUE;
   } else {
    L251_=ARR((STR)s,L1); 
    L26 = L251_;
    L271_=L26=='}'; 
    L21 = L271_;
   }
   if (L21) {
    L20 = TRUE;
   } else {
    L281_=ARR((STR)s,L1); 
    L29 = L281_;
    L301_=L29=='\n'; 
    L20 = L301_;
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L321_=ARR((STR)s,L1); 
    L33 = L321_;
    L341_=L33=='\r'; 
    L19 = L341_;
   }
   if (L19) {
    create_self = ((ERR1) NULL);
    ret_val1 = create_self;
    plus_self = ret_val1;
    plus_s = ((dSTR) ((STR) &Unterm889448459));
    stderr_self = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r = L35;
    
    SATTR(r,fp,stderr);
    ret_val3 = r;
    FILE_plus_dSTR(ret_val3, plus_s);
    ret_val2 = plus_self;
    plus_self1 = ret_val2;
    plus_s1 = ((dSTR) ((STR) &name8));
    stderr_self1 = ((FILE1) NULL);
    L39=ZALLOC(sizeof(struct FILE1_struct));
    if (L39==NULL) FATAL("Unable to allocate more memory");
    ((OB)L39)->header.tag=FILE1_tag;
    L38 = ((FILE1) L39);
    r1 = L38;
    
    SATTR(r1,fp,stderr);
    ret_val5 = r1;
    FILE_plus_dSTR(ret_val5, plus_s1);
    ret_val4 = plus_self1;
    plus_self2 = ret_val4;
    plus_s2 = ((dSTR) tenv);
    stderr_self2 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L41 = ((FILE1) L42);
    r2 = L41;
    
    SATTR(r2,fp,stderr);
    ret_val7 = r2;
    FILE_plus_dSTR(ret_val7, plus_s2);
    ret_val6 = plus_self2;
    plus_self3 = ret_val6;
    plus_s3 = ((dSTR) ((STR) &in));
    stderr_self3 = ((FILE1) NULL);
    L45=ZALLOC(sizeof(struct FILE1_struct));
    if (L45==NULL) FATAL("Unable to allocate more memory");
    ((OB)L45)->header.tag=FILE1_tag;
    L44 = ((FILE1) L45);
    r3 = L44;
    
    SATTR(r3,fp,stderr);
    ret_val9 = r3;
    FILE_plus_dSTR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    plus_s4 = ((dSTR) fname);
    stderr_self4 = ((FILE1) NULL);
    L48=ZALLOC(sizeof(struct FILE1_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=FILE1_tag;
    L47 = ((FILE1) L48);
    r4 = L47;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    ret_val10 = plus_self4;
    plus_self5 = ret_val10;
    plus_s5 = ((dSTR) ((STR) &assume));
    stderr_self5 = ((FILE1) NULL);
    L52=ZALLOC(sizeof(struct FILE1_struct));
    if (L52==NULL) FATAL("Unable to allocate more memory");
    ((OB)L52)->header.tag=FILE1_tag;
    L50 = ((FILE1) L52);
    r5 = L50;
    
    SATTR(r5,fp,stderr);
    ret_val13 = r5;
    FILE_plus_dSTR(ret_val13, plus_s5);
    ret_val12 = plus_self5;
    plus_self6 = ret_val12;
    L55=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L55==NULL) FATAL("Unable to allocate more memory");
    memset(L55,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L55)->header.tag=CHAR_tag;
    L54 = (dSTR)((CHAR_boxed) L55);
    ((CHAR_boxed) L54)->immutable_part = cp;
    plus_s6 = L54;
    stderr_self6 = ((FILE1) NULL);
    L57=ZALLOC(sizeof(struct FILE1_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=FILE1_tag;
    L56 = ((FILE1) L57);
    r6 = L56;
    
    SATTR(r6,fp,stderr);
    ret_val15 = r6;
    FILE_plus_dSTR(ret_val15, plus_s6);
    ret_val14 = plus_self6;
    plus_self7 = ret_val14;
    plus_s7 = ((dSTR) ((STR) &name19));
    stderr_self7 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r7 = L59;
    
    SATTR(r7,fp,stderr);
    ret_val17 = r7;
    FILE_plus_dSTR(ret_val17, plus_s7);
    ret_val16 = plus_self7;
    plus_self8 = ret_val16;
    plus_s8 = ((dSTR) ((STR) &skipthistoken));
    stderr_self8 = ((FILE1) NULL);
    L63=ZALLOC(sizeof(struct FILE1_struct));
    if (L63==NULL) FATAL("Unable to allocate more memory");
    ((OB)L63)->header.tag=FILE1_tag;
    L62 = ((FILE1) L63);
    r8 = L62;
    
    SATTR(r8,fp,stderr);
    ret_val18 = r8;
    FILE_plus_dSTR(ret_val18, plus_s8);
    skip1 = TRUE;
    goto after_loop;
   }
   plus_self9 = tenv;
   L651_=ARR((STR)s,L1); 
   plus_arg = L651_;
   str_self = plus_arg;
   create_self1 = ((STR) NULL);
   create_c = str_self;
   L67 = 1;
   L69=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L67)*sizeof(CHAR);
   L68=ZALLOC_LEAF_BIG(L69);
   if (L68==NULL) FATAL("Unable to allocate more memory");
   memset(L68,0,L69);
   ((OB)L68)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L68)->header.destroyed=0;
#endif

   L66 = ((STR) L68);
   L66->asize = L67;
   r9 = L66;
   SARR((STR)r9,0,create_c); 
   ;
   ret_val21 = r9;
   ret_val20 = ret_val21;
   s1 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self9, s1, TRUE);
   tenv = ret_val19;
   L721_=INTPLUS(L1,1); 
   L1 = L721_;
  }
  after_loop: ;
  L731_=!(skip1); 
  if (L731_) {
   L74 = STR_sizerINT(s);
   L751_=(L1)<(L74); 
   if (L751_) {
    L761_=INTPLUS(L1,1); 
    L1 = L761_;
   }
   e = UNIX_g108606936(((UNIX) NULL), tenv);
   if ((e==((STR) NULL))) {
    plus_self10 = ((STR) &d1);
    plus_arg1 = op;
    str_self1 = plus_arg1;
    create_self2 = ((STR) NULL);
    create_c1 = str_self1;
    L78 = 1;
    L80=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L78)*sizeof(CHAR);
    L79=ZALLOC_LEAF_BIG(L80);
    if (L79==NULL) FATAL("Unable to allocate more memory");
    memset(L79,0,L80);
    ((OB)L79)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L79)->header.destroyed=0;
#endif

    L77 = ((STR) L79);
    L77->asize = L78;
    r10 = L77;
    SARR((STR)r10,0,create_c1); 
    ;
    ret_val24 = r10;
    ret_val23 = ret_val24;
    s2 = ret_val23;
    ret_val22 = STR_ap1077157958(plus_self10, s2, TRUE);
    plus_self11 = ret_val22;
    plus_sarg = tenv;
    ret_val25 = STR_ap2004550586(plus_self11, plus_sarg);
    plus_self12 = ret_val25;
    plus_arg2 = cp;
    str_self2 = plus_arg2;
    create_self3 = ((STR) NULL);
    create_c2 = str_self2;
    L84 = 1;
    L86=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L84)*sizeof(CHAR);
    L85=ZALLOC_LEAF_BIG(L86);
    if (L85==NULL) FATAL("Unable to allocate more memory");
    memset(L85,0,L86);
    ((OB)L85)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L85)->header.destroyed=0;
#endif

    L83 = ((STR) L85);
    L83->asize = L84;
    r11 = L83;
    SARR((STR)r11,0,create_c2); 
    ;
    ret_val28 = r11;
    ret_val27 = ret_val28;
    s3 = ret_val27;
    ret_val26 = STR_ap1077157958(plus_self12, s3, TRUE);
    e = ret_val26;
   }
  }
 }
 ret_val = e;
 *pos = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP1899004935(GET_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r1;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 extern STR name1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 STR L7;
 extern STR Satherfiles;
 extern STR name1;
 extern STR Mainclass;
 extern STR Homedirectory;
 extern STR Prolix;
 extern STR Verbose;
 r = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   plus_self = r;
   plus_arg = ' ';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L3 = 1;
   L5=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L3)*sizeof(CHAR);
   L4=ZALLOC_LEAF_BIG(L5);
   if (L4==NULL) FATAL("Unable to allocate more memory");
   memset(L4,0,L5);
   ((OB)L4)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L4)->header.destroyed=0;
#endif

   L2 = ((STR) L4);
   L2->asize = L3;
   r1 = L2;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val3 = r1;
   ret_val2 = ret_val3;
   s1 = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
   plus_self1 = ret_val1;
   if (noname1->state == 0) {
    L11 = ATTR(ATTR(self,prog),sather_files);
    noname1->self = L11;
   }
   L7 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L7;
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
   r = ret_val4;
  }
 }
 after_loop: ;
 s = GET_OP1767328402(self, ((STR) &Satherfiles), ((STR) &name1), r);
 s = GET_OP1767328402(self, ((STR) &Mainclass), s, ATTR(ATTR(self,prog),main_class));
 plus_self2 = s;
 plus_sarg1 = ((STR) &Homedirectory);
 ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
 plus_self3 = ret_val5;
 plus_sarg2 = ATTR(ATTR(self,prog),home);
 ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
 s = ret_val6;
 if (ATTR(ATTR(self,prog),prolix)) {
  plus_self4 = s;
  plus_sarg3 = ((STR) &Prolix);
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
  s = ret_val7;
 }
 if (ATTR(ATTR(self,prog),verbose)) {
  plus_self5 = s;
  plus_sarg4 = ((STR) &Verbose);
  ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
  s = ret_val8;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP2007358201(GET_OPTIONS self, STR a) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_lengthrINT(a);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)a,pos); 
   L8 = L71_;
   L91_=L8=='.'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 L10 = STR_lengthrINT(a);
 L121_=INTMINUS(L10,pos); 
 ret_val = STR_tail_INTrSTR(a, L121_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP751661850(GET_OPTIONS self, STR nm) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 STR r;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 extern STR name1;
 extern STR name18;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_sizerINT(nm);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)nm,pos); 
   L8 = L71_;
   L91_=L8=='/'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 r = STR_head_INTrSTR(nm, pos);
 if (STR_is111530248(r, ((STR) &name1))) {
  r = ((STR) &name18);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR GET_OP93258057(GET_OPTIONS self) {
 STR ret_val;
 STR res;
 GET_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 ARRAYSTR L6;
 INT L7;
 STR L81_;
 INT L9;
 INT L101_;
 extern STR Arg;
 FILE1 L11;
 OB L12;
 FILE1 L14;
 OB L15;
 extern STR name9;
 FILE1 L17;
 OB L18;
 extern STR name1;
 more_args_self = self;
 L1 = ATTR(more_args_self,next);
 L2 = ATTR(more_args_self,args);
 L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
 L4 = L31_;
 L51_=(L1)<(L4); 
 ret_val1 = L51_;
 if (ret_val1) {
  L6 = ATTR(self,args);
  L7 = ATTR(self,next);
  L81_=(STR)ARR((ARRAYSTR)L6,L7); 
  res = L81_;
  L9 = ATTR(self,next);
  L101_=INTPLUS(L9,1); 
  SATTR(self,next,L101_);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self = ((OUT) NULL);
   ret_val2 = create_self;
   plus_self = ret_val2;
   plus_s = ((STR) &Arg);
   stdout_self = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L11 = ((FILE1) L12);
   r = L11;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = res;
   stdout_self1 = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r1 = L14;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r2 = L17;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  ret_val = res;
  return ret_val;
 }
 else {
  GET_OP1011839251(self);
  ret_val = ((STR) &name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR IDENT_1415966123(IDENT self, INT i) {
 STR ret_val;
 STR res = ((STR) NULL);
 INT L11 = INT_zero;
 STR L21;
 INT L31 = INT_zero;
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
 BOOL L41_;
 extern STR name3;
 STR L6;
 extern STR S_;
 INT L71_;
 extern STR name2;
 extern STR name4;
 L41_=(i)==(0); 
 if (L41_) {
  ret_val = self.str;
  return ret_val;
 }
 {
  BOOL f_L51_ = TRUE;
  struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
  /* loop index variable */
  L31 = 0;
  L11 = i;
  L21 = ((STR) &name3);
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   if(L31>=L11)  goto after_loop; 
   ;
   plus_self = res;
   noname1->arg1 = ((dSTR) ((STR) &S_));
   L6 = STR_se405450305(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L6;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   res = ret_val1;
   L71_=INTPLUS(L31,1); 
   L31 = L71_;
  }
 }
 after_loop: ;
 plus_self1 = self.str;
 plus_sarg1 = ((STR) &name2);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = res;
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 plus_sarg3 = ((STR) &name4);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR IFC_strrSTR(IFC self) {
 STR ret_val;
 STR s;
 ARRAYSTR a;
 INT i = INT_zero;
 SIG_TBL L11;
 SIG sig1;
 STR st;
 ARRAYSTR L21;
 ARRAYSTR L31;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val3;
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
 extern STR public507392;
 dTP L5;
 extern STR is;
 ARRAYSTR L6;
 INT L7;
 OB L8;
 INT L9;
 INT L101_br;
INT i_L101_=0;
 SIG L12;
 BOOL L13;
 BOOL L141_;
 extern STR name6;
 extern STR name7;
 BOOL L15;
 BOOL L161_;
 INT L171_;
 INT L18;
 INT L191_;
 INT L201_br;
 STR aL201_;
 extern STR name8;
 STR L22;
 extern STR name9;
 INT L231_;
 extern STR end1;
 plus_self = ((STR) &public507392);
 L5 = ATTR(self,tp);
 plus_sarg = (*dTP_strrSTR[TAG(L5)])(L5);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val1;
 plus_sarg1 = ((STR) &is);
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 s = ret_val2;
 create_self = ((ARRAYSTR) NULL);
 create_n = SIG_TBL_sizerINT(ATTR(self,sigs));
 L7 = create_n;
 L9=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L7)*sizeof(STR);
 L8=ZALLOC_BIG(L9);
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L8)->header.destroyed=0;
#endif

 L6 = ((ARRAYSTR) L8);
 L6->asize = L7;
 ret_val3 = L6;
 a = ret_val3;
 i = 0;
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  BOOL f_L101_ = TRUE;
  L11 = ATTR(self,sigs);
  L21 = a;
  noname1->self = L11;
  noname1->state = 0;
  L101_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  i_L101_=0;
  while (1) {
   L12 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   sig1 = L12;
   st = ATTR(sig1,str);
   L13 = (ATTR(sig1,srcsig)==((SIG) NULL));
   L141_=!(L13); 
   if (L141_) {
    plus_self2 = st;
    plus_sarg2 = ((STR) &name6);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val4;
    plus_sarg3 = ATTR(ATTR(sig1,srcsig),str);
    ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val5;
    plus_sarg4 = ((STR) &name7);
    ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
    st = ret_val6;
   }
   if(i_L101_>=L101_br)  goto after_loop; 
   SARR((ARRAYSTR)L21,i_L101_,(STR)st); i_L101_++;
   ;
  }
 }
 after_loop: ;
 sort_self = a;
 L15 = (sort_self==((ARRAYSTR) NULL));
 L161_=!(L15); 
 if (L161_) {
  L171_=ASIZE((ARRAYSTR)sort_self); 
  L18 = L171_;
  L191_=INTMINUS(L18,1); 
  ARRAYS1450857938(sort_self, 0, L191_);
 }
 {
  BOOL f_L201_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L31 = a;
  L201_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
  while (1) {
   plus_self5 = s;
   plus_sarg5 = ((STR) &name8);
   ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val7;
   if(L41>=L201_br)  goto after_loop1; 
   aL201_=ARR((ARRAYSTR)L31,L41); 
   plus_sarg6 = aL201_;
   ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val8;
   plus_sarg7 = ((STR) &name9);
   ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
   s = ret_val9;
   L231_=INTPLUS(L41,1); 
   L41 = L231_;
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

STR IMPL_s709672763(IMPL self, IDENT ident1) {
 STR ret_val;
 ELT_TBL L11;
 IDENT L21 = IDENT_zero;
 ELT cnst;
 dAS_CLASS_ELT as;
 dAS_EXPR ai;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 OUT ret_val2;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT plus_self1;
 dSTR plus_s1;
 OUT ret_val4;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 IMPL prog_self;
 PROG ret_val7;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val8;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val9;
 UNIX exit_self;
 INT exit_code = INT_zero;
 OUT create_self1;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s3;
 OUT ret_val11;
 FILE1 stdout_self3;
 FILE1 ret_val12;
 FILE1 r3;
 OUT plus_self6;
 dSTR plus_s4;
 OUT ret_val13;
 FILE1 stdout_self4;
 FILE1 ret_val14;
 FILE1 r4;
 OUT plus_self7;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val15;
 FILE1 r5;
 IMPL prog_self1;
 PROG ret_val16;
 STR plus_self8;
 STR plus_sarg2;
 STR ret_val17;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self9;
 STR plus_sarg3;
 STR ret_val18;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 ELT L3;
 BOOL L4;
 dTP L5;
 extern STR TYPE1;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR name9;
 FILE1 L13;
 OB L14;
 dTP L16;
 extern STR forane106255635;
 extern STR Intern28965746;
 extern STR TYPE1;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 dTP L27;
 extern STR forane106255635;
 extern STR Intern28965746;
 {
  struct ELT_TB431570775_frame_struct other1_0;
ELT_TB431570775_frame noname1 = &other1_0;
  L11 = ATTR(self,elts);
  L21 = ident1;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L3 = ELT_TB431570775(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   cnst = L3;
   if (ELT_is1269015106(cnst)) {
    L5 = ATTR(ATTR(cnst,sig1),ret);
    L4 = (*dTP_is242312711[TAG(L5)])(L5, ((OB) TP_BUILTIN_str));
   } else {
    L4 = FALSE;
   }
   if (L4) {
    as = ATTR(cnst,as);
    if (as==NULL) {
     goto other352;
    } else
    switch (TAG(as)) {
     case AS_CONST_DEF_tag:
      ai = ATTR(((AS_CONST_DEF) as),init);
      if (ai==NULL) {
       goto other353;
      } else
      switch (TAG(ai)) {
       case AS_STR_LIT_EXPR_tag:
        ret_val = ATTR(((AS_STR_LIT_EXPR) ai),s);
        return ret_val; break;
       default: ;
       other353: ;
        create_self = ((OUT) NULL);
        ret_val1 = create_self;
        plus_self = ret_val1;
        plus_s = ((STR) &TYPE1);
        stdout_self = ((FILE1) NULL);
        L7=ZALLOC(sizeof(struct FILE1_struct));
        if (L7==NULL) FATAL("Unable to allocate more memory");
        ((OB)L7)->header.tag=FILE1_tag;
        L6 = ((FILE1) L7);
        r = L6;
        
        SATTR(r,fp,stdout);
        ret_val3 = r;
        FILE_plus_STR(ret_val3, plus_s);
        ret_val2 = plus_self;
        plus_self1 = ret_val2;
        plus_s1 = ((dSTR) ATTR(self,tp));
        stdout_self1 = ((FILE1) NULL);
        L10=ZALLOC(sizeof(struct FILE1_struct));
        if (L10==NULL) FATAL("Unable to allocate more memory");
        ((OB)L10)->header.tag=FILE1_tag;
        L9 = ((FILE1) L10);
        r1 = L9;
        
        SATTR(r1,fp,stdout);
        ret_val5 = r1;
        FILE_plus_dSTR(ret_val5, plus_s1);
        ret_val4 = plus_self1;
        plus_self2 = ret_val4;
        plus_s2 = ((STR) &name9);
        stdout_self2 = ((FILE1) NULL);
        L14=ZALLOC(sizeof(struct FILE1_struct));
        if (L14==NULL) FATAL("Unable to allocate more memory");
        ((OB)L14)->header.tag=FILE1_tag;
        L13 = ((FILE1) L14);
        r2 = L13;
        
        SATTR(r2,fp,stdout);
        ret_val6 = r2;
        FILE_plus_STR(ret_val6, plus_s2);
        prog_self = self;
        L16 = ATTR(prog_self,tp);
        ret_val7 = (*dTP_progrPROG[TAG(L16)])(L16);
        barf_self = ret_val7;
        plus_self3 = ident1.str;
        plus_sarg = ((STR) &forane106255635);
        ret_val8 = STR_ap2004550586(plus_self3, plus_sarg);
        barf_msg = ret_val8;
        barf_at_self = barf_self;
        barf_at_msg = barf_msg;
        barf_at_at = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self, barf_at_at);
        plus_self4 = ((STR) &Intern28965746);
        plus_sarg1 = barf_at_msg;
        ret_val9 = STR_ap2004550586(plus_self4, plus_sarg1);
        PROG_err_STR(barf_at_self, ret_val9);
        exit_self = ((UNIX) NULL);
        exit_code = 1;
        exit(exit_code);
      } break;
     default: ;
     other352: ;
      create_self1 = ((OUT) NULL);
      ret_val10 = create_self1;
      plus_self5 = ret_val10;
      plus_s3 = ((STR) &TYPE1);
      stdout_self3 = ((FILE1) NULL);
      L18=ZALLOC(sizeof(struct FILE1_struct));
      if (L18==NULL) FATAL("Unable to allocate more memory");
      ((OB)L18)->header.tag=FILE1_tag;
      L17 = ((FILE1) L18);
      r3 = L17;
      
      SATTR(r3,fp,stdout);
      ret_val12 = r3;
      FILE_plus_STR(ret_val12, plus_s3);
      ret_val11 = plus_self5;
      plus_self6 = ret_val11;
      plus_s4 = ((dSTR) ATTR(self,tp));
      stdout_self4 = ((FILE1) NULL);
      L22=ZALLOC(sizeof(struct FILE1_struct));
      if (L22==NULL) FATAL("Unable to allocate more memory");
      ((OB)L22)->header.tag=FILE1_tag;
      L20 = ((FILE1) L22);
      r4 = L20;
      
      SATTR(r4,fp,stdout);
      ret_val14 = r4;
      FILE_plus_dSTR(ret_val14, plus_s4);
      ret_val13 = plus_self6;
      plus_self7 = ret_val13;
      plus_s5 = ((STR) &name9);
      stdout_self5 = ((FILE1) NULL);
      L25=ZALLOC(sizeof(struct FILE1_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=FILE1_tag;
      L24 = ((FILE1) L25);
      r5 = L24;
      
      SATTR(r5,fp,stdout);
      ret_val15 = r5;
      FILE_plus_STR(ret_val15, plus_s5);
      prog_self1 = self;
      L27 = ATTR(prog_self1,tp);
      ret_val16 = (*dTP_progrPROG[TAG(L27)])(L27);
      barf_self1 = ret_val16;
      plus_self8 = ident1.str;
      plus_sarg2 = ((STR) &forane106255635);
      ret_val17 = STR_ap2004550586(plus_self8, plus_sarg2);
      barf_msg1 = ret_val17;
      barf_at_self1 = barf_self1;
      barf_at_msg1 = barf_msg1;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self9 = ((STR) &Intern28965746);
      plus_sarg3 = barf_at_msg1;
      ret_val18 = STR_ap2004550586(plus_self9, plus_sarg3);
      PROG_err_STR(barf_at_self1, ret_val18);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
    }
   }
   else {
    ret_val = ((STR) NULL);
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = ((STR) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP1011839251(GET_OPTIONS self) {
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val1;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val3;
 FILE1 r1;
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val5;
 FILE1 stderr_self2;
 FILE1 ret_val6;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val7;
 FILE1 stderr_self3;
 FILE1 ret_val8;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val9;
 FILE1 r4;
 ERR1 create_self2;
 ERR1 ret_val10;
 ERR1 plus_self5;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val11;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 extern STR Comman1382430749;
 FILE1 L1;
 OB L2;
 ARRAYSTR L4;
 INT L5;
 ARRAYSTR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 STR L131_;
 FILE1 L14;
 OB L15;
 extern STR in;
 FILE1 L17;
 OB L18;
 ARRAYSTR L20;
 INT L21;
 ARRAYSTR L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 INT L271_;
 INT L28;
 STR L291_;
 FILE1 L30;
 OB L32;
 dSTR L34;
 OB L35;
 FILE1 L36;
 OB L37;
 extern STR Seemanpage;
 FILE1 L39;
 OB L40;
 create_self = ((ERR1) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((dSTR) ((STR) &Comman1382430749));
 stderr_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stderr);
 ret_val2 = r;
 FILE_plus_dSTR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 L4 = ATTR(self,args);
 L5 = ATTR(self,next);
 L6 = ATTR(self,args);
 L71_=(L6)==NULL?0:ASIZE((ARRAYSTR)L6); 
 L8 = L71_;
 L91_=INTMINUS(L8,1); 
 L10 = L91_;
 L111_=(L5<L10)?L5:L10; 
 L12 = L111_;
 L131_=(STR)ARR((ARRAYSTR)L4,L12); 
 plus_s1 = ((dSTR) L131_);
 stderr_self1 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r1 = L14;
 
 SATTR(r1,fp,stderr);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
 create_self1 = ((ERR1) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((dSTR) ((STR) &in));
 stderr_self2 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r2 = L17;
 
 SATTR(r2,fp,stderr);
 ret_val6 = r2;
 FILE_plus_dSTR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L20 = ATTR(self,arg_locations);
 L21 = ATTR(self,next);
 L22 = ATTR(self,args);
 L231_=(L22)==NULL?0:ASIZE((ARRAYSTR)L22); 
 L24 = L231_;
 L251_=INTMINUS(L24,1); 
 L26 = L251_;
 L271_=(L21<L26)?L21:L26; 
 L28 = L271_;
 L291_=(STR)ARR((ARRAYSTR)L20,L28); 
 plus_s3 = ((dSTR) L291_);
 stderr_self3 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r3 = L30;
 
 SATTR(r3,fp,stderr);
 ret_val8 = r3;
 FILE_plus_dSTR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 L35=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 memset(L35,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L35)->header.tag=CHAR_tag;
 L34 = (dSTR)((CHAR_boxed) L35);
 ((CHAR_boxed) L34)->immutable_part = '\n';
 plus_s4 = L34;
 stderr_self4 = ((FILE1) NULL);
 L37=ZALLOC(sizeof(struct FILE1_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=FILE1_tag;
 L36 = ((FILE1) L37);
 r4 = L36;
 
 SATTR(r4,fp,stderr);
 ret_val9 = r4;
 FILE_plus_dSTR(ret_val9, plus_s4);
 create_self2 = ((ERR1) NULL);
 ret_val10 = create_self2;
 plus_self5 = ret_val10;
 plus_s5 = ((dSTR) ((STR) &Seemanpage));
 stderr_self5 = ((FILE1) NULL);
 L40=ZALLOC(sizeof(struct FILE1_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=FILE1_tag;
 L39 = ((FILE1) L40);
 r5 = L39;
 
 SATTR(r5,fp,stderr);
 ret_val11 = r5;
 FILE_plus_dSTR(ret_val11, plus_s5);
 exit_self = ((UNIX) NULL);
 exit_code = 1;
 exit(exit_code);
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP1239350398(GET_OPTIONS self) {
 STR arg;
 STR suf;
 STR path11;
 GET_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 GET_OPTIONS rewind_self;
 BFILE resolve_path_sel;
 STR resolve_path_fn;
 STR ret_val1;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L8;
 INT L91_;
 extern STR sa;
 BOOL L10;
 BOOL L111_;
 extern STR module;
 FLISTSTR L12;
 while (1) {
  more_args_self = self;
  L1 = ATTR(more_args_self,next);
  L2 = ATTR(more_args_self,args);
  L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
  L4 = L31_;
  L51_=(L1)<(L4); 
  ret_val = L51_;
  if (ret_val) {
  }
  else {
   goto after_loop;
  }
  arg = GET_OP93258057(self);
  L6 = GET_OP1847571144(self, arg);
  L71_=!(L6); 
  if (L71_) {
   rewind_self = self;
   L8 = ATTR(rewind_self,next);
   L91_=INTMINUS(L8,1); 
   SATTR(rewind_self,next,L91_);
   goto after_loop;
  }
  suf = GET_OP2007358201(self, arg);
  if (STR_is111530248(suf, ((STR) &sa))) {
   resolve_path_sel = ((BFILE) NULL);
   resolve_path_fn = arg;
   ret_val1 = resolve_path_fn;
   path11 = ret_val1;
   L10 = FSETST1025458804(ATTR(self,paths), path11);
   L111_=!(L10); 
   if (L111_) {
    SATTR(ATTR(self,prog),sather_files,FSETST1404644833(ATTR(ATTR(self,prog),sather_files), arg));
    SATTR(self,paths,FSETST1404644833(ATTR(self,paths), path11));
   }
  }
  else {
   if (STR_is111530248(suf, ((STR) &module))) {
    L12 = GET_OP1478203685(self, arg);
    GET_OP1297032912(self, L12, arg);
   }
   else {
    GET_OP584215778(self, arg, suf);
    return;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP1297032912(GET_OPTIONS self, FLISTSTR cl, STR filename) {
 ARRAYSTR tail = ((ARRAYSTR) NULL);
 ARRAYSTR tail_names = ((ARRAYSTR) NULL);
 ARRAYSTR L11;
 GET_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 ARRAYSTR subarr_self;
 INT subarr_beg = INT_zero;
 INT subarr_num = INT_zero;
 ARRAYSTR ret_val1;
 ARRAYSTR r;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val2;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val3;
 ARRAYSTR create_self2;
 INT create_n2 = INT_zero;
 ARRAYSTR ret_val4;
 ARRAYSTR create_self3;
 INT create_n3 = INT_zero;
 ARRAYSTR ret_val5;
 INT L21 = INT_zero;
 INT L3;
 ARRAYSTR L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 ARRAYSTR L8;
 INT L91_;
 INT L10;
 INT L12;
 INT L131_;
 ARRAYSTR L14;
 INT L15;
 OB L16;
 INT L17;
 ARRAYSTR L19;
 INT L201_;
 INT L22;
 INT L23;
 INT L241_;
 ARRAYSTR L25;
 INT L26;
 OB L27;
 INT L28;
 ARRAYSTR L29;
 INT L30;
 OB L31;
 INT L32;
 ARRAYSTR L33;
 INT L34;
 OB L35;
 INT L36;
 ARRAYSTR L37;
 ARRAYSTR L38;
 INT L39;
 OB L40;
 INT L41;
 INT L421_br;
 INT L431_;
 more_args_self = self;
 L3 = ATTR(more_args_self,next);
 L4 = ATTR(more_args_self,args);
 L51_=(L4)==NULL?0:ASIZE((ARRAYSTR)L4); 
 L6 = L51_;
 L71_=(L3)<(L6); 
 ret_val = L71_;
 if (ret_val) {
  subarr_self = ATTR(self,args);
  subarr_beg = ATTR(self,next);
  L8 = ATTR(self,args);
  L91_=(L8)==NULL?0:ASIZE((ARRAYSTR)L8); 
  L10 = L91_;
  L12 = ATTR(self,next);
  L131_=INTMINUS(L10,L12); 
  subarr_num = L131_;
  L15 = subarr_num;
  L17=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L15)*sizeof(STR);
  L16=ZALLOC_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((ARRAYSTR) L16);
  L14->asize = L15;
  r = L14;
  AREFACOPYBNS(r,0,subarr_num,subarr_beg,subarr_self); 
  ;
  ret_val1 = r;
  tail = ret_val1;
  create_self = ((ARRAYSTR) NULL);
  L19 = ATTR(self,args);
  L201_=(L19)==NULL?0:ASIZE((ARRAYSTR)L19); 
  L22 = L201_;
  L23 = ATTR(self,next);
  L241_=INTMINUS(L22,L23); 
  create_n = L241_;
  L26 = create_n;
  L28=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L26)*sizeof(STR);
  L27=ZALLOC_BIG(L28);
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L27)->header.destroyed=0;
#endif

  L25 = ((ARRAYSTR) L27);
  L25->asize = L26;
  ret_val2 = L25;
  tail_names = ret_val2;
 }
 else {
  create_self1 = ((ARRAYSTR) NULL);
  create_n1 = 0;
  L30 = create_n1;
  L32=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L30)*sizeof(STR);
  L31=ZALLOC_BIG(L32);
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L31)->header.destroyed=0;
#endif

  L29 = ((ARRAYSTR) L31);
  L29->asize = L30;
  ret_val3 = L29;
  tail = ret_val3;
  create_self2 = ((ARRAYSTR) NULL);
  create_n2 = 0;
  L34 = create_n2;
  L36=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L34)*sizeof(STR);
  L35=ZALLOC_BIG(L36);
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L35)->header.destroyed=0;
#endif

  L33 = ((ARRAYSTR) L35);
  L33->asize = L34;
  ret_val4 = L33;
  tail_names = ret_val4;
 }
 L37 = FLISTS533235694(cl);
 SATTR(self,args,ARRAYS417083844(L37, tail));
 create_self3 = ((ARRAYSTR) NULL);
 create_n3 = FLISTS326330076(cl);
 L39 = create_n3;
 L41=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L39)*sizeof(STR);
 L40=ZALLOC_BIG(L41);
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L40)->header.destroyed=0;
#endif

 L38 = ((ARRAYSTR) L40);
 L38->asize = L39;
 ret_val5 = L38;
 SATTR(self,arg_locations,ret_val5);
 {
  BOOL f_L421_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,arg_locations);
  L421_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L21>=L421_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L21,(STR)filename); 
   ;
   L431_=INTPLUS(L21,1); 
   L21 = L431_;
  }
 }
 after_loop: ;
 SATTR(self,arg_locations,ARRAYS417083844(ATTR(self,arg_locations), tail_names));
 SATTR(self,next,0);
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP1722411507(GET_OPTIONS self, ARRAYSTR a) {
 ARRAYSTR L11;
 STR s;
 STR L21;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 INT L31 = INT_zero;
 GET_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 GET_OPTIONS barf_self;
 STR barf_msg;
 ERR1 create_self2;
 ERR1 ret_val8;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val9;
 FILE1 stderr_self;
 FILE1 ret_val10;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self1;
 FILE1 ret_val11;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 GET_OPTIONS handle_other_opt;
 STR handle_other_opt1;
 GET_OPTIONS barf_self1;
 STR barf_msg1;
 ERR1 create_self3;
 ERR1 ret_val12;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val13;
 FILE1 stderr_self2;
 FILE1 ret_val14;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 FILE1 stderr_self3;
 FILE1 ret_val15;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 INT L41_;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 extern STR commandline;
 INT L101_;
 INT L12;
 ARRAYSTR L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 extern STR Processingoption;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 extern STR main2;
 extern STR home;
 extern STR homeis1034977208;
 FILE1 L27;
 OB L28;
 dSTR L30;
 OB L32;
 FILE1 L33;
 OB L34;
 extern STR verbose;
 extern STR prolix;
 extern STR prolix706342420;
 extern STR O_debug;
 extern STR O_debug_func;
 FLISTSTR L36;
 extern STR O_verbose;
 extern STR end2;
 extern STR Nohome1873319144;
 FILE1 L37;
 OB L38;
 dSTR L40;
 OB L42;
 FILE1 L43;
 OB L44;
 SATTR(self,args,a);
 create_self = ((ARRAYSTR) NULL);
 L41_=(a)==NULL?0:ASIZE((ARRAYSTR)a); 
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
 ret_val = L5;
 SATTR(self,arg_locations,ret_val);
 {
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,arg_locations);
  L91_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L31,(STR)((STR) &commandline)); 
   ;
   L101_=INTPLUS(L31,1); 
   L31 = L101_;
  }
 }
 after_loop: ;
 SATTR(self,next,1);
 GET_OP1239350398(self);
 while (1) {
  more_args_self = self;
  L12 = ATTR(more_args_self,next);
  L13 = ATTR(more_args_self,args);
  L141_=(L13)==NULL?0:ASIZE((ARRAYSTR)L13); 
  L15 = L141_;
  L161_=(L12)<(L15); 
  ret_val1 = L161_;
  if (ret_val1) {
  }
  else {
   goto after_loop1;
  }
  s = GET_OP93258057(self);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self = ret_val2;
   plus_s = ((STR) &Processingoption);
   stdout_self = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r = L17;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = s;
   stdout_self1 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r1 = L20;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L25=ZALLOC(sizeof(struct FILE1_struct));
   if (L25==NULL) FATAL("Unable to allocate more memory");
   ((OB)L25)->header.tag=FILE1_tag;
   L24 = ((FILE1) L25);
   r2 = L24;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  L21 = s;
  if (STR_is111530248(L21, ((STR) &main2))) {
   SATTR(ATTR(self,prog),main_class,GET_OP93258057(self));
  }
  else {
   if (STR_is111530248(L21, ((STR) &home))) {
    barf_self = self;
    barf_msg = ((STR) &homeis1034977208);
    create_self2 = ((ERR1) NULL);
    ret_val8 = create_self2;
    plus_self3 = ret_val8;
    plus_s3 = ((dSTR) barf_msg);
    stderr_self = ((FILE1) NULL);
    L28=ZALLOC(sizeof(struct FILE1_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=FILE1_tag;
    L27 = ((FILE1) L28);
    r3 = L27;
    
    SATTR(r3,fp,stderr);
    ret_val10 = r3;
    FILE_plus_dSTR(ret_val10, plus_s3);
    ret_val9 = plus_self3;
    plus_self4 = ret_val9;
    L32=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    memset(L32,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L32)->header.tag=CHAR_tag;
    L30 = (dSTR)((CHAR_boxed) L32);
    ((CHAR_boxed) L30)->immutable_part = '\n';
    plus_s4 = L30;
    stderr_self1 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r4 = L33;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
   }
   else {
    if (STR_is111530248(L21, ((STR) &verbose))) {
     SATTR(ATTR(self,prog),verbose,TRUE);
    }
    else {
     if (STR_is111530248(L21, ((STR) &prolix))) {
      SATTR(ATTR(self,prog),verbose,TRUE);
      SATTR(ATTR(self,prog),prolix,TRUE);
     }
     else {
      if (STR_is111530248(L21, ((STR) &prolix706342420))) {
       SATTR(ATTR(self,prog),show_types,TRUE);
       SATTR(ATTR(self,prog),prolix,TRUE);
      }
      else {
       if (STR_is111530248(L21, ((STR) &O_debug))) {
        SATTR(ATTR(self,prog),opt_debug,TRUE);
        SATTR(ATTR(self,prog),opt_verbose,TRUE);
       }
       else {
        if (STR_is111530248(L21, ((STR) &O_debug_func))) {
         L36 = ATTR(ATTR(self,prog),opt_debug_func);
         SATTR(ATTR(self,prog),opt_debug_func,FLISTS1425610062(L36, GET_OP93258057(self)));
         SATTR(ATTR(self,prog),opt_verbose,TRUE);
        }
        else {
         if (STR_is111530248(L21, ((STR) &O_verbose))) {
          SATTR(ATTR(self,prog),opt_verbose,TRUE);
         }
         else {
          if (STR_is111530248(L21, ((STR) &end2))) {
          }
          else {
           handle_other_opt = self;
           handle_other_opt1 = s;
           GET_OP383637490(handle_other_opt, handle_other_opt1);
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
 after_loop1: ;
 if ((ATTR(ATTR(self,prog),home)==((STR) NULL))) {
  barf_self1 = self;
  barf_msg1 = ((STR) &Nohome1873319144);
  create_self3 = ((ERR1) NULL);
  ret_val12 = create_self3;
  plus_self5 = ret_val12;
  plus_s5 = ((dSTR) barf_msg1);
  stderr_self2 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r5 = L37;
  
  SATTR(r5,fp,stderr);
  ret_val14 = r5;
  FILE_plus_dSTR(ret_val14, plus_s5);
  ret_val13 = plus_self5;
  plus_self6 = ret_val13;
  L42=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  memset(L42,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L42)->header.tag=CHAR_tag;
  L40 = (dSTR)((CHAR_boxed) L42);
  ((CHAR_boxed) L40)->immutable_part = '\n';
  plus_s6 = L40;
  stderr_self3 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r6 = L43;
  
  SATTR(r6,fp,stderr);
  ret_val15 = r6;
  FILE_plus_dSTR(ret_val15, plus_s6);
  exit_self1 = ((UNIX) NULL);
  exit_code1 = 1;
  exit(exit_code1);
 }
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP383637490(GET_OPTIONS self, STR s) {
 GET_OPTIONS rewind_self;
 GET_OPTIONS rewind_self1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 if (GET_OP1847571144(self, s)) {
  rewind_self = self;
  L1 = ATTR(rewind_self,next);
  L21_=INTMINUS(L1,1); 
  SATTR(rewind_self,next,L21_);
  GET_OP1239350398(self);
 }
 else {
  rewind_self1 = self;
  L3 = ATTR(rewind_self1,next);
  L41_=INTMINUS(L3,1); 
  SATTR(rewind_self1,next,L41_);
  GET_OP1011839251(self);
  return;
 }
}


#undef IS_ITER
#define IS_ITER 0

void GET_OP584215778(GET_OPTIONS self, STR arg, STR suf) {
 STR L11;
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val1;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val3;
 FILE1 stderr_self1;
 FILE1 ret_val4;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR c1;
 extern STR o;
 extern STR a1;
 extern STR obj;
 extern STR lib1;
 extern STR Didntu1702411602;
 FILE1 L2;
 OB L3;
 FILE1 L5;
 OB L6;
 dSTR L8;
 OB L9;
 FILE1 L10;
 OB L12;
 L11 = suf;
 if (STR_is111530248(L11, ((STR) &c1))) {
  return;
 }
 else {
  if (STR_is111530248(L11, ((STR) &o))) {
   return;
  }
  else {
   if (STR_is111530248(L11, ((STR) &a1))) {
    return;
   }
   else {
    if (STR_is111530248(L11, ((STR) &obj))) {
     return;
    }
    else {
     if (STR_is111530248(L11, ((STR) &lib1))) {
      return;
     }
     else {
      create_self = ((ERR1) NULL);
      ret_val = create_self;
      plus_self = ret_val;
      plus_s = ((dSTR) ((STR) &Didntu1702411602));
      stderr_self = ((FILE1) NULL);
      L3=ZALLOC(sizeof(struct FILE1_struct));
      if (L3==NULL) FATAL("Unable to allocate more memory");
      ((OB)L3)->header.tag=FILE1_tag;
      L2 = ((FILE1) L3);
      r = L2;
      
      SATTR(r,fp,stderr);
      ret_val2 = r;
      FILE_plus_dSTR(ret_val2, plus_s);
      ret_val1 = plus_self;
      plus_self1 = ret_val1;
      plus_s1 = ((dSTR) arg);
      stderr_self1 = ((FILE1) NULL);
      L6=ZALLOC(sizeof(struct FILE1_struct));
      if (L6==NULL) FATAL("Unable to allocate more memory");
      ((OB)L6)->header.tag=FILE1_tag;
      L5 = ((FILE1) L6);
      r1 = L5;
      
      SATTR(r1,fp,stderr);
      ret_val4 = r1;
      FILE_plus_dSTR(ret_val4, plus_s1);
      ret_val3 = plus_self1;
      plus_self2 = ret_val3;
      L9=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L9==NULL) FATAL("Unable to allocate more memory");
      memset(L9,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L9)->header.tag=CHAR_tag;
      L8 = (dSTR)((CHAR_boxed) L9);
      ((CHAR_boxed) L8)->immutable_part = '\n';
      plus_s2 = L8;
      stderr_self2 = ((FILE1) NULL);
      L12=ZALLOC(sizeof(struct FILE1_struct));
      if (L12==NULL) FATAL("Unable to allocate more memory");
      ((OB)L12)->header.tag=FILE1_tag;
      L10 = ((FILE1) L12);
      r2 = L10;
      
      SATTR(r2,fp,stderr);
      ret_val5 = r2;
      FILE_plus_dSTR(ret_val5, plus_s2);
      GET_OP1011839251(self);
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void IDENT_1047918280(IDENT_BUILTIN self) {
 extern STR dOB;
 extern STR ARR1;
 extern STR INT1;
 extern STR INTI;
 extern STR FLT1;
 extern STR FLTD1;
 extern STR FLTDX1;
 extern STR FLTX1;
 extern STR F_INTEGER1;
 extern STR F_REAL1;
 extern STR F_CHARACTER1;
 extern STR F_STRING1;
 extern STR F_LOGICAL1;
 extern STR F_DOUBLE1;
 extern STR F_COMPLEX1;
 extern STR F_DOUBLE_COMPLEX1;
 extern STR F_HANDLER1;
 extern STR F_ROUT1;
 extern STR F_ARRAY;
 extern STR F_ARRAY2;
 extern STR F_ARRAY3;
 extern STR plus1;
 extern STR minus;
 extern STR times1;
 extern STR div1;
 extern STR pow1;
 extern STR mod;
 extern STR is_lt;
 extern STR negate;
 extern STR not1;
 extern STR AVAL;
 extern STR AREF;
 extern STR aget;
 extern STR aset;
 extern STR call1;
 extern STR callb;
 extern STR create1;
 extern STR ARRAY;
 extern STR ret_val111;
 extern STR self1;
 extern STR invariant;
 extern STR asize;
 extern STR C_name;
 extern STR C_header;
 extern STR main1;
 extern STR is_eq;
 extern STR ob_eq;
 extern STR b;
 extern STR SYS1;
 extern STR ZONE1;
 IDENT_959790938 = IDENT_1150413730(IDENT_zero, ((STR) &dOB));
 IDENT_428696930 = IDENT_1150413730(IDENT_zero, ((STR) &ARR1));
 IDENT_1924458840 = IDENT_1150413730(IDENT_zero, ((STR) &INT1));
 IDENT_853591262 = IDENT_1150413730(IDENT_zero, ((STR) &INTI));
 IDENT_1067669005 = IDENT_1150413730(IDENT_zero, ((STR) &FLT1));
 IDENT_12128454 = IDENT_1150413730(IDENT_zero, ((STR) &FLTD1));
 IDENT_1644121920 = IDENT_1150413730(IDENT_zero, ((STR) &FLTDX1));
 IDENT_685437142 = IDENT_1150413730(IDENT_zero, ((STR) &FLTX1));
 IDENT_869381517 = IDENT_1150413730(IDENT_zero, ((STR) &F_INTEGER1));
 IDENT_291726180 = IDENT_1150413730(IDENT_zero, ((STR) &F_REAL1));
 IDENT_1834849979 = IDENT_1150413730(IDENT_zero, ((STR) &F_CHARACTER1));
 IDENT_1295303664 = IDENT_1150413730(IDENT_zero, ((STR) &F_STRING1));
 IDENT_692534448 = IDENT_1150413730(IDENT_zero, ((STR) &F_LOGICAL1));
 IDENT_51715464 = IDENT_1150413730(IDENT_zero, ((STR) &F_DOUBLE1));
 IDENT_58546263 = IDENT_1150413730(IDENT_zero, ((STR) &F_COMPLEX1));
 IDENT_1418293603 = IDENT_1150413730(IDENT_zero, ((STR) &F_DOUBLE_COMPLEX1));
 IDENT_1061837561 = IDENT_1150413730(IDENT_zero, ((STR) &F_HANDLER1));
 IDENT_5303930 = IDENT_1150413730(IDENT_zero, ((STR) &F_ROUT1));
 IDENT_265373817 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY));
 IDENT_1280657180 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY2));
 IDENT_386785441 = IDENT_1150413730(IDENT_zero, ((STR) &F_ARRAY3));
 IDENT_2085547498 = IDENT_1150413730(IDENT_zero, ((STR) &plus1));
 IDENT_1129727818 = IDENT_1150413730(IDENT_zero, ((STR) &minus));
 IDENT_1616879092 = IDENT_1150413730(IDENT_zero, ((STR) &times1));
 IDENT_228725606 = IDENT_1150413730(IDENT_zero, ((STR) &div1));
 IDENT_1974148927 = IDENT_1150413730(IDENT_zero, ((STR) &pow1));
 IDENT_475003333 = IDENT_1150413730(IDENT_zero, ((STR) &mod));
 IDENT_421158771 = IDENT_1150413730(IDENT_zero, ((STR) &is_lt));
 IDENT_2108657069 = IDENT_1150413730(IDENT_zero, ((STR) &negate));
 IDENT_1837767448 = IDENT_1150413730(IDENT_zero, ((STR) &not1));
 IDENT_1485568624 = IDENT_1150413730(IDENT_zero, ((STR) &AVAL));
 IDENT_1666445894 = IDENT_1150413730(IDENT_zero, ((STR) &AREF));
 IDENT_1140439567 = IDENT_1150413730(IDENT_zero, ((STR) &aget));
 IDENT_667688077 = IDENT_1150413730(IDENT_zero, ((STR) &aset));
 IDENT_2110206590 = IDENT_1150413730(IDENT_zero, ((STR) &call1));
 IDENT_327685657 = IDENT_1150413730(IDENT_zero, ((STR) &callb));
 IDENT_458929583 = IDENT_1150413730(IDENT_zero, ((STR) &create1));
 IDENT_190278447 = IDENT_1150413730(IDENT_zero, ((STR) &ARRAY));
 IDENT_1909070998 = IDENT_1150413730(IDENT_zero, ((STR) &ret_val111));
 IDENT_985337352 = IDENT_1150413730(IDENT_zero, ((STR) &self1));
 IDENT_2063376192 = IDENT_1150413730(IDENT_zero, ((STR) &invariant));
 IDENT_1891587148 = IDENT_1150413730(IDENT_zero, ((STR) &asize));
 IDENT_857412936 = IDENT_1150413730(IDENT_zero, ((STR) &C_name));
 IDENT_1137952589 = IDENT_1150413730(IDENT_zero, ((STR) &C_header));
 IDENT_196328811 = IDENT_1150413730(IDENT_zero, ((STR) &main1));
 IDENT_1728630987 = IDENT_1150413730(IDENT_zero, ((STR) &is_eq));
 IDENT_461800478 = IDENT_1150413730(IDENT_zero, ((STR) &ob_eq));
 IDENT_604723504 = IDENT_1150413730(IDENT_zero, ((STR) &b));
 IDENT_673807638 = IDENT_1150413730(IDENT_zero, ((STR) &SYS1));
 IDENT_531080196 = IDENT_1150413730(IDENT_zero, ((STR) &ZONE1));
}


#undef IS_ITER
#define IS_ITER 0

void IFC_AB1563215475(IFC_ABS_CREATE self) {
 AS_TYPE_SPEC ut;
 dTP tp;
 IFC itp;
 IFC_ABS_CREATE prog_self;
 PROG ret_val;
 IFC_ABS_CREATE prog_self1;
 PROG ret_val1;
 IFC_ABS_CREATE prog_self2;
 PROG ret_val2;
 BOOL L1;
 BOOL L21_;
 extern STR Abstra303237185;
 extern STR Abstra303237185;
 ut = ATTR(ATTR(self,tr),under);
 while (1) {
  if ((ut==((AS_TYPE_SPEC) NULL))) {
   goto after_loop;
  }
  tp = TP_CON1800432689(ATTR(self,con), ut);
  prog_self = self;
  ret_val = ATTR(ATTR(prog_self,tp),prog);
  PROG_e176405615(ret_val, ((dPROG_ERR) ut));
  if (tp==NULL) {
   goto other122;
  } else
  switch (TAG(tp)) {
   case TP_CLASS_tag:
    L1 = TP_CLA1568240525(((TP_CLASS) tp));
    L21_=!(L1); 
    if (L21_) {
     prog_self1 = self;
     ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
     PROG_err_STR(ret_val1, ((STR) &Abstra303237185));
    }
    else {
     itp = TP_CLASS_ifcrIFC(((TP_CLASS) tp));
     SATTR(self,supers,FLISTI985525641(ATTR(self,supers), itp));
    } break;
   default: ;
   other122: ;
    prog_self2 = self;
    ret_val2 = ATTR(ATTR(prog_self2,tp),prog);
    PROG_err_STR(ret_val2, ((STR) &Abstra303237185));
  }
  ut = ATTR(ut,next);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void IFC_AB1878942039(IFC_ABS_CREATE self) {
 dAS_CLASS_ELT be;
 SIG ns;
 SIG cs;
 IFC_ABS_CREATE prog_self;
 PROG ret_val;
 IFC_ABS_CREATE prog_self1;
 PROG ret_val1;
 IFC_ABS_CREATE prog_self2;
 PROG ret_val2;
 IFC_ABS_CREATE prog_self3;
 PROG ret_val3;
 IFC_ABS_CREATE prog_self4;
 PROG ret_val4;
 IFC_ABS_CREATE prog_self5;
 PROG ret_val5;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 IFC_ABS_CREATE prog_self6;
 PROG ret_val10;
 extern STR Abstra1019572228;
 extern STR Abstra683659090;
 extern STR Abstra1952962902;
 BOOL L1;
 BOOL L21_;
 extern STR Abstra1195098517;
 BOOL L3;
 BOOL L41_;
 extern STR Thetwo495127004;
 extern STR and3;
 extern STR conflict1;
 extern STR Abstra1509597057;
 dAS_CLASS_ELT L5;
 be = ATTR(ATTR(self,tr),body);
 while (1) {
  if ((be==((dAS_CLASS_ELT) NULL))) {
   goto after_loop;
  }
  prog_self = self;
  ret_val = ATTR(ATTR(prog_self,tp),prog);
  PROG_e176405615(ret_val, ((dPROG_ERR) be));
  switch (TAG(be)) {
   case AS_CONST_DEF_tag:
    prog_self1 = self;
    ret_val1 = ATTR(ATTR(prog_self1,tp),prog);
    PROG_err_STR(ret_val1, ((STR) &Abstra1019572228)); break;
   case AS_SHARED_DEF_tag:
    prog_self2 = self;
    ret_val2 = ATTR(ATTR(prog_self2,tp),prog);
    PROG_err_STR(ret_val2, ((STR) &Abstra683659090)); break;
   case AS_ATTR_DEF_tag:
    prog_self3 = self;
    ret_val3 = ATTR(ATTR(prog_self3,tp),prog);
    PROG_err_STR(ret_val3, ((STR) &Abstra1952962902)); break;
   case AS_ROUT_DEF_tag:
    L1 = ATTR(((AS_ROUT_DEF) be),is_abstract);
    L21_=!(L1); 
    if (L21_) {
     prog_self4 = self;
     ret_val4 = ATTR(ATTR(prog_self4,tp),prog);
     PROG_err_STR(ret_val4, ((STR) &Abstra1195098517));
    }
    ns = SIG_ro1276623596(((SIG) NULL), ((AS_ROUT_DEF) be), ATTR(((AS_ROUT_DEF) be),name1), ATTR(ATTR(self,con),ptypes), ATTR(self,con));
    cs = SIG_TB1067194561(ATTR(self,class_sigs), ns);
    L3 = (cs==((SIG) NULL));
    L41_=!(L3); 
    if (L41_) {
     prog_self5 = self;
     ret_val5 = ATTR(ATTR(prog_self5,tp),prog);
     plus_self = ((STR) &Thetwo495127004);
     plus_sarg = ATTR(ns,str);
     ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val6;
     plus_sarg1 = ((STR) &and3);
     ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val7;
     plus_sarg2 = ATTR(cs,str);
     ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
     plus_self3 = ret_val8;
     plus_sarg3 = ((STR) &conflict1);
     ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
     PROG_err_STR(ret_val5, ret_val9);
    }
    else {
     SATTR(self,class_sigs,SIG_TB1438251609(ATTR(self,class_sigs), ns));
    } break;
   case AS_INC1416917001_tag:
    prog_self6 = self;
    ret_val10 = ATTR(ATTR(prog_self6,tp),prog);
    PROG_err_STR(ret_val10, ((STR) &Abstra1509597057)); break;
   default: ;
    FATAL("No applicable type in typecase\nin IFC_ABS_CREATE::do_explicit_class_sigs\n./Representation/ifc.sa:274:17");
  }
  L5 = be;
  be = (*dAS_CL760354380[TAG(L5)])(L5);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void IFC_cycle_err(IFC self) {
 STR s;
 FSETTUPIDENTINT L11;
 TUPIDENTINT cur = TUPIDENTINT_zero;
 STR L21;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 IFC prog_self;
 PROG ret_val1;
 extern STR Cycled226243192;
 extern STR name29;
 TUPIDENTINT L3;
 STR L4;
 dTP L5;
 s = ((STR) &Cycled226243192);
 {
  struct FSETTU947212086_frame_struct other1_0;
FSETTU947212086_frame noname1 = &other1_0;
  struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
  L11 = IFC_abs_cur;
  L21 = ((STR) &name29);
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L3 = FSETTU947212086(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   cur = L3;
   plus_self = s;
   noname2->arg1 = ((dSTR) IDENT_1415966123(cur.t1, cur.t2));
   L4 = STR_se405450305(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   plus_sarg = L4;
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   s = ret_val;
  }
 }
 after_loop: ;
 prog_self = self;
 L5 = ATTR(prog_self,tp);
 ret_val1 = (*dTP_progrPROG[TAG(L5)])(L5);
 PROG_err_STR(ret_val1, s);
}


#undef IS_ITER
#define IS_ITER 0

void IFC_show(IFC self) {
 SIG_TBL L11;
 SIG s;
 STR L21;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 extern STR Interfacevoid;
 FILE1 L3;
 OB L4;
 extern STR Interfacetpvoid;
 FILE1 L6;
 OB L7;
 extern STR Interfaceof;
 FILE1 L9;
 OB L10;
 dTP L13;
 FILE1 L14;
 OB L15;
 extern STR name36;
 FILE1 L17;
 OB L18;
 extern STR void3;
 FILE1 L20;
 OB L22;
 extern STR name10;
 SIG L24;
 BOOL L25;
 BOOL L261_;
 STR L27;
 FILE1 L28;
 OB L29;
 extern STR name9;
 FILE1 L31;
 OB L32;
 if ((self==((IFC) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Interfacevoid);
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 if ((ATTR(self,tp)==((dTP) NULL))) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &Interfacetpvoid);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  return;
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &Interfaceof);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L13 = ATTR(self,tp);
 plus_s3 = (*dTP_strrSTR[TAG(L13)])(L13);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name36);
 stdout_self4 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r4 = L17;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 if ((ATTR(self,sigs)==((SIG_TBL) NULL))) {
  create_self3 = ((OUT) NULL);
  ret_val10 = create_self3;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &void3);
  stdout_self5 = ((FILE1) NULL);
  L22=ZALLOC(sizeof(struct FILE1_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=FILE1_tag;
  L20 = ((FILE1) L22);
  r5 = L20;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  return;
 }
 {
  struct SIG_TBL_eltbrSIG_frame_struct other1_0;
SIG_TBL_eltbrSIG_frame noname1 = &other1_0;
  struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
  L11 = ATTR(self,sigs);
  L21 = ((STR) &name10);
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L24 = SIG_TBL_eltbrSIG(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   s = L24;
   L25 = (s==((SIG) NULL));
   L261_=!(L25); 
   if (L261_) {
    create_self4 = ((OUT) NULL);
    ret_val12 = create_self4;
    plus_self6 = ret_val12;
    noname2->arg1 = ((dSTR) ATTR(s,str));
    L27 = STR_se405450305(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    plus_s6 = L27;
    stdout_self6 = ((FILE1) NULL);
    L29=ZALLOC(sizeof(struct FILE1_struct));
    if (L29==NULL) FATAL("Unable to allocate more memory");
    ((OB)L29)->header.tag=FILE1_tag;
    L28 = ((FILE1) L29);
    r6 = L28;
    
    SATTR(r6,fp,stdout);
    ret_val13 = r6;
    FILE_plus_STR(ret_val13, plus_s6);
   }
  }
 }
 after_loop: ;
 create_self5 = ((OUT) NULL);
 ret_val14 = create_self5;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &name9);
 stdout_self7 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L31 = ((FILE1) L32);
 r7 = L31;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
}
