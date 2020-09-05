#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dCONFIG_struct {
 OB_HEADER header;
 } *dCONFIG;

typedef struct dHASH_struct {
 OB_HEADER header;
 } *dHASH;

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

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

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

typedef struct AREFFL1698233817_struct {/* layout for AREF{FLIST{AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTA725283029_struct *arr_part[1];
 } *AREFFL1698233817;

typedef struct ARG_LAYOUT_struct {/* layout for ARG_LAYOUT */
 OB_HEADER header;
 struct SIG_struct *sig1;
 STR str;
 } *ARG_LAYOUT;

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

typedef struct ARRAYFSTR_struct {/* layout for ARRAY{FSTR} */
 OB_HEADER header;
 INT asize;
 struct FSTR_struct *arr_part[1];
 } *ARRAYFSTR;

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

typedef struct CODE_FILE_ARRAY_struct {/* layout for CODE_FILE_ARRAY */
 OB_HEADER header;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *CODE_FILE_ARRAY;

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

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

typedef struct ELT_EQdAM_STMT_struct {/* layout for ELT_EQ{$AM_STMT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAM_STMT;

typedef struct ELT_EQdAS_EXPR_struct {/* layout for ELT_EQ{$AS_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdAS_EXPR;

typedef struct ELT_EQdCONFIG_struct {/* layout for ELT_EQ{$CONFIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdCONFIG;

typedef struct ELT_EQ773986801_struct {/* layout for ELT_EQ{$INLINE_ITER_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ773986801;

typedef struct ELT_EQ1957447193_struct {/* layout for ELT_EQ{$INLINE_ROUT_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1957447193;

typedef struct ELT_EQdLAYOUT_struct {/* layout for ELT_EQ{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdLAYOUT;

typedef struct ELT_EQdOB_struct {/* layout for ELT_EQ{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQdOB;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NI1131308118_struct {/* layout for ELT_NIL{$INLINE_ITER_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1131308118;

typedef struct ELT_NI52152274_struct {/* layout for ELT_NIL{$INLINE_ROUT_SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI52152274;

typedef struct ELT_NILdLAYOUT_struct {/* layout for ELT_NIL{$LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdLAYOUT;

typedef struct ELT_NILdOB_struct {/* layout for ELT_NIL{$OB} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdOB;

typedef struct ELT_NILdTP_struct {/* layout for ELT_NIL{$TP} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILdTP;

typedef struct ELT_NI1559244239_struct {/* layout for ELT_NIL{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1559244239;

typedef struct ELT_NI1564018771_struct {/* layout for ELT_NIL{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1564018771;

typedef struct ELT_NI1377997731_struct {/* layout for ELT_NIL{AM_SHARED_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1377997731;

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

typedef struct ELT_NI94923122_struct {/* layout for ELT_NIL{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI94923122;

typedef struct ELT_NI1394471793_struct {/* layout for ELT_NIL{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1394471793;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ELT_NILTUPSIGSIG_struct {/* layout for ELT_NIL{TUP{SIG,SIG}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTUPSIGSIG;

typedef struct EXTERNAL_LAYOUT_struct {/* layout for EXTERNAL_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct PROG_struct *prog;
 STR str;
 } *EXTERNAL_LAYOUT;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

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

typedef struct IMMUTA421759893_struct {/* layout for IMMUTABLE_CLASS_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct AM_OB_DEF_struct *l;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *IMMUTA421759893;

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

typedef struct INT_timesb_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesb_frame;

typedef struct INT_timesbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *INT_timesbrINT_frame;

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct LAYOUT_ARRAY_struct {/* layout for LAYOUT_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *LAYOUT_ARRAY;

typedef struct LAYOUT1657363748_frame_struct {
 INT state;
 LAYOUT_ARRAY self;/* Formal argument: self */
 dLAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT1657363748_frame;

typedef struct LAYOUT2103796202_frame_struct {
 INT state;
 LAYOUT_ARRAY self;/* Formal argument: self */
 dLAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *LAYOUT2103796202_frame;

typedef struct LAYOUT_TBL_struct {/* layout for LAYOUT_TBL */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *LAYOUT_TBL;

typedef struct MANGLE_struct {/* layout for MANGLE */
 OB_HEADER header;
 struct FMAPdOBNAMESPACE_struct *namespaces;
 struct FSETSTR_struct *forbidden;
 struct FSETSTR_struct *used_by_local;
 INT max_identifier_l;
 struct NAMESPACE_struct *global_space;
 struct PROG_struct *prog;
 BOOL append_hash_to_i;
 } *MANGLE;

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

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

typedef struct TUPSIGSIG_struct {/* layout for TUP{SIG,SIG} */
 struct SIG_struct *t1;
 struct SIG_struct *t2;
 } TUPSIGSIG;
static TUPSIGSIG TUPSIGSIG_zero;

typedef struct TUPSIGSIG_boxed_struct {
 OB_HEADER header;
 TUPSIGSIG immutable_part;
 } *TUPSIGSIG_boxed;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct ARRAYA712604839_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA712604839_frame;

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

typedef struct CODE_F423548741_frame_struct {
 INT state;
 CODE_FILE self;/* Formal argument: self */
 SIG arg1;/* Formal argument: sig */
 STR ret_val41;
 STR res;
 STR decl;
 ARRAYARG L191;
 ARG e;
 ARRAYARG L371;
 ARG e1;
 STR L391;
 ARRAYARG L471;
 ARG e2;
 STR args;
 ARRAYARG L521;
 ARG e3;
 INT L611;
 INT L621;
 INT i;
 STR suffix;
 INT L701;
 INT L741;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 SIG is_iter_self;
 BOOL ret_val1;
 IDENT is_iter_self1;
 BOOL ret_val2;
 CODE_FILE mang_self;
 OB mang_ob;
 STR ret_val3;
 CODE_FILE mang_self1;
 OB mang_ob1;
 STR ret_val4;
 CODE_FILE mang_self2;
 OB mang_ob2;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val7;
 CODE_FILE mang_self3;
 OB mang_ob3;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 CODE_FILE mang_self4;
 OB mang_ob4;
 STR ret_val10;
 INT L161;
 CODE_FILE mang_self5;
 OB mang_ob5;
 STR ret_val11;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val12;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val13;
 CODE_FILE mang_self6;
 OB mang_ob6;
 STR ret_val14;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val15;
 CODE_FILE mang_self7;
 OB mang_ob7;
 STR ret_val16;
 SIG is_iter_self2;
 BOOL ret_val17;
 IDENT is_iter_self3;
 BOOL ret_val18;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val19;
 CODE_FILE mang_self8;
 OB mang_ob8;
 STR ret_val20;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val21;
 STR plus_self7;
 STR plus_sarg9;
 STR ret_val22;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val23;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val24;
 INT L341;
 CODE_FILE mang_self9;
 OB mang_ob9;
 STR ret_val25;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val26;
 SIG is_forked_self;
 BOOL ret_val27;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val28;
 INT L441;
 CODE_FILE mang_self10;
 OB mang_ob10;
 STR ret_val29;
 CODE_FILE mang_self11;
 OB mang_ob11;
 STR ret_val30;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val31;
 INT L491;
 CODE_FILE mang_self12;
 OB mang_ob12;
 STR ret_val32;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val33;
 INT L671;
 INT aeltb_self;
 BOOL ret_val42;
 INT L761;
 BOOL aeltb_if_first;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val34;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val35;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val36;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val37;
 STR plus_self18;
 STR plus_sarg20;
 STR ret_val38;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val39;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val40;
 BOOL f_L171_; /* used by builtin iter */
 INT L171_br; /* used by builtin iter */
 INT i_L171_; /* used by builtin iter */
 BOOL f_L351_; /* used by builtin iter */
 INT L351_br; /* used by builtin iter */
 INT i_L351_; /* used by builtin iter */
 struct STR_se405450305_frame_struct *nested3; /* nested iter frame */
 BOOL f_L451_; /* used by builtin iter */
 INT L451_br; /* used by builtin iter */
 INT i_L451_; /* used by builtin iter */
 BOOL f_L501_; /* used by builtin iter */
 INT L501_br; /* used by builtin iter */
 INT i_L501_; /* used by builtin iter */
 BOOL f_L591_; /* used by builtin iter */
 INT L591_,L591_m; /* used by builtin iter */
 BOOL f_L681_; /* used by builtin iter */
 INT i_L681_; /* used by builtin iter */
 BOOL f_L691_; /* used by builtin iter */
 INT i_L691_; /* used by builtin iter */
 } *CODE_F423548741_frame;

typedef struct CODE_F809259241_frame_struct {
 INT state;
 CODE_FILE_ARRAY self;/* Formal argument: self */
 CODE_FILE ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *CODE_F809259241_frame;

typedef struct CODE_F1378335655_frame_struct {
 INT state;
 CODE_FILE_ARRAY self;/* Formal argument: self */
 CODE_FILE arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *CODE_F1378335655_frame;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

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

typedef struct FMAPAM339652544_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FMAPAM339652544;

typedef struct FMAPAM1324815836_frame_struct {
 INT state;
 FMAPAM339652544 self;/* Formal argument: self */
 BOUND_1007407331 ret_val2;
 FMAPAM339652544 L61;
 TUPAM_304458649 e;
 INT L31;
 FMAPAM339652544 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1324815836_frame;

typedef struct FMAPAM1357596931_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FMAPAM1357596931;

typedef struct FMAPAM1626870716_frame_struct {
 INT state;
 FMAPAM1357596931 self;/* Formal argument: self */
 BOUND_1677815802 ret_val2;
 FMAPAM1357596931 L61;
 TUPAM_667892060 e;
 INT L31;
 FMAPAM1357596931 is_key_nil_self;
 AM_BND1124877163 is_key_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1626870716_frame;

typedef struct FMAPSI2103621588_struct {/* layout for FMAP{SIG,ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FMAPSI2103621588;

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

typedef struct FMAPSI1349988702_struct {/* layout for FMAP{SIG,ARG_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FMAPSI1349988702;

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

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

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

typedef struct FSETdLAYOUT_struct {/* layout for FSET{$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FSETdLAYOUT;

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

typedef struct ARRAYF254484098_frame_struct {
 INT state;
 ARRAYFSTR self;/* Formal argument: self */
 FSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF254484098_frame;

typedef struct ARRAYF394786536_frame_struct {
 INT state;
 ARRAYFSTR self;/* Formal argument: self */
 FSTR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYF394786536_frame;

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

extern CGEN CODE_FILE_cgen;
extern FLISTCODE_FILE CODE_FILE_all;
extern FLISTCODE_FILE CODE_FILE_todo;
extern FLISTSTR CODE_F1806827531;
extern FLISTdLAYOUT CODE_F19188927;
extern FMAPAM1357596931 BOUND_2051717332;
extern FMAPAM339652544 BOUND_982289559;
extern FMAPSI1121373188 FRAME_1643038396;
extern FMAPSI1349988702 ARG_LA2092120054;
extern FMAPSI2103621588 ABSTRA854284101;
extern FMAPSI518162669 CODE_F508943357;
extern FMAPSTRSTR CODE_F1403139677;
extern FSETAB1569698808 ABSTRA8148400;
extern FSETBO1147931784 BOUND_2075179152;
extern FSETBO1548397318 BOUND_210358778;
extern FSETBU1446477505 BUILTI898649695;
extern FSETCLASS_LAYOUT CLASS_368803185;
extern FSETEX1260947371 EXTERN1555805875;
extern FSETIM184436961 IMMUTA489824193;
extern INOUT_MODE MODES_inout_mode;
extern INT INT_asize;
extern INT TP_KIN462481068;
extern OUT_MODE MODES_out_mode;
extern PROG CODE_FILE_prog;
extern STR CODE_FILE_dir;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST BOOL (**dIS_EQ1272012319)(dIS_EQ, OB);

extern RETURNED_CONST BOOL (**dIS_NI446211542)(dIS_NIL);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST FLISTdLAYOUT (**dLAYOU1845593458)(dLAYOUT);

extern RETURNED_CONST INT (**dAM_EX1001627434)(dAM_EXPR);

extern RETURNED_CONST INT (**dHASH_hashrINT)(dHASH);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST STR (**dLAYOU959283167)(dLAYOUT, CGEN);

extern RETURNED_CONST STR (**dLAYOUT_strrSTR)(dLAYOUT);

extern RETURNED_CONST dNIL (**dNIL_nilrdNIL)(dNIL);
ABSTRA151498155 ABSTRA1493033801(ABSTRA151498155, SIG, PROG);
ABSTRA151498155 FMAPSI140286185(FMAPSI140286185_frame);
ABSTRACT_LAYOUT FSETAB1942662686(FSETAB1942662686_frame);
AM_BND1124877163 FLISTA273121893(FLISTA273121893_frame);
AM_BND1124877163 FSETAM595751729(FSETAM595751729_frame);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AM_SHARED_EXPR FSETAM12483681(FSETAM12483681_frame);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARG_LAYOUT FMAPSI688594122(FMAPSI688594122_frame);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
BOOL ATTRSO1207195602(ATTRSORT, OB);
BOOL CODE_F1219277062(CODE_FILE, CODE_FILE);
BOOL CSE_DO4156401(CSE_DOUBLE_EXPRS, OB);
BOOL ELT_EQ1393525399(ELT_EQdLAYOUT, dLAYOUT, dLAYOUT);
BOOL ELT_EQ182879965(ELT_EQ773986801, dINLINE_ITER_SIG, dINLINE_ITER_SIG);
BOOL ELT_EQ1977650129(ELT_EQdCONFIG, dCONFIG, dCONFIG);
BOOL ELT_EQ2067825030(ELT_EQdAM_STMT, dAM_STMT, dAM_STMT);
BOOL ELT_EQ398237803(ELT_EQ1957447193, dINLINE_ROUT_SIG, dINLINE_ROUT_SIG);
BOOL ELT_EQ686719113(ELT_EQdAS_EXPR, dAS_EXPR, dAS_EXPR);
BOOL ELT_EQ936498558(ELT_EQdOB, OB, OB);
BOOL ELT_NI125473534(ELT_NILdTP, dTP);
BOOL ELT_NI1893940731(ELT_NI52152274, dINLINE_ROUT_SIG);
BOOL ELT_NI455285430(ELT_NILdOB, OB);
BOOL ELT_NI569783157(ELT_NI1131308118, dINLINE_ITER_SIG);
BOOL ELT_NI978759818(ELT_NILdLAYOUT, dLAYOUT);
BOOL FSETdL1260128492(FSETdLAYOUT, dLAYOUT);
BOOL SIG_is275586960(SIG);
BOOL STR_is111530248(STR, STR);
BOUND_1007407331 FMAPAM1324815836(FMAPAM1324815836_frame);
BOUND_1677815802 FMAPAM1626870716(FMAPAM1626870716_frame);
BOUND_2140373124 FSETBO677183779(FSETBO677183779_frame);
BOUND_809390774 FSETBO10998148(FSETBO10998148_frame);
BUILTIN_LAYOUT FSETBU202043707(FSETBU202043707_frame);
CLASS_LAYOUT FSETCL1233705521(FSETCL1233705521_frame);
CODE_FILE CODE_F1432464034(CODE_FILE, STR);
CODE_FILE CODE_F809259241(CODE_F809259241_frame);
CODE_FILE FLISTC1947519404(FLISTC1947519404_frame);
CONFIG_DEF CONFIG2114270854(CONFIG_DEF, STR, FLISTFLISTSTR);
EXTERNAL_LAYOUT FSETEX1633103446(FSETEX1633103446_frame);
FLISTA1893461511 FLISTA226859063(FLISTA1893461511, FLISTA1893461511);
FLISTCODE_FILE FLISTC1532120756(FLISTCODE_FILE, CODE_FILE);
FLISTFSTR FLISTF1280148353(FLISTFSTR, FSTR);
FLISTFSTR FLISTF524229941(FLISTFSTR, FLISTFSTR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTSTR FLISTS1900495285(FLISTSTR, INT);
FLISTdLAYOUT FLISTd1429133189(FLISTdLAYOUT, dLAYOUT);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FRAME_LAYOUT FMAPSI1790064862(FMAPSI1790064862_frame);
FRAME_LAYOUT FRAME_248172754(FRAME_LAYOUT, AM_ROUT_DEF, PROG);
FSETAM1032778315 FSETAM381650379(FSETAM1032778315, FSETAM1032778315);
FSETAM1032778315 FSETAM74171957(FSETAM1032778315);
FSETAM1542678947 FSETAM1298377859(FSETAM1542678947, FSETAM1542678947);
FSETAM1542678947 FSETAM1609335141(FSETAM1542678947);
FSETSIG FSETSI548941202(FSETSIG, FSETSIG);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSIG FSETSI719979460(FSETSIG);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETSTR FSETST1872370245(FSETSTR, STR);
FSETSTR FSETST1884046860(FSETSTR);
FSETSTR FSETST866022026(FSETSTR, FSETSTR);
FSETTUPSIGSIG FSETTU1446784814(FSETTUPSIGSIG);
FSETTUPSIGSIG FSETTU814351307(FSETTUPSIGSIG, FSETTUPSIGSIG);
FSETdLAYOUT FSETdL1582668907(FSETdLAYOUT, FSETdLAYOUT);
FSETdLAYOUT FSETdL1585396236(FSETdLAYOUT);
FSETdLAYOUT FSETdL835744292(FSETdLAYOUT, dLAYOUT);
FSTR ARRAYF254484098(ARRAYF254484098_frame);
FSTR CODE_F1554664505(CODE_FILE);
FSTR FILE_g1986509982(FILE1);
FSTR FLISTF1277706721(FLISTF1277706721_frame);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
IMMUTA421759893 FSETIM887960645(FSETIM887960645_frame);
INT ARRAYA712604839(ARRAYA712604839_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT CSE_DO1342102696(CSE_DOUBLE_EXPRS);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FLISTA919540447(FLISTA1893461511);
INT FLISTC1064619074(FLISTCODE_FILE);
INT FLISTF1097166371(FLISTFLISTSTR);
INT FLISTF1348049713(FLISTFSTR);
INT FLISTS326330076(FLISTSTR);
INT FLISTd102354833(FLISTdLAYOUT);
INT FSETAM1190305782(FSETAM1032778315);
INT FSETAM2105800198(FSETAM1542678947);
INT FSETSIG_sizerINT(FSETSIG);
INT FSETSTR_sizerINT(FSETSTR);
INT FSETTU1591851819(FSETTUPSIGSIG);
INT FSETdL858951727(FSETdLAYOUT);
INT FSTR_sizerINT(FSTR);
INT INT_hashrINT(INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_lengthrINT(STR);
INT STR_sizerINT(STR);
OB ELT_NI509824640(ELT_NILdOB);
SIG FSETSIG_eltbrSIG(FSETSIG_eltbrSIG_frame);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_f1593827635(CGEN, SIG);
STR CODE_F1366222275(CODE_FILE);
STR CODE_F223990950(CODE_FILE, SIG, SIG);
STR CODE_F244102140(CODE_FILE);
STR CODE_F423548741(CODE_F423548741_frame);
STR CODE_F44077436(CODE_FILE);
STR CODE_F881092569(CODE_FILE, ARG, STR);
STR CONFIG1221082662(CONFIG_DEF);
STR CONFIG1397479570(CONFIG_TBL, STR, INT);
STR CONFIG811280218(CONFIG_DEF);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FMAPST1518383807(FMAPSTRSTR, STR);
STR FMAPST2087339362(FMAPST2087339362_frame);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR FSTR_t603468843(FSTR);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_prettyrSTR(STR);
STR STR_se405450305(STR_se405450305_frame);
TUPSIGSIG FSETTU845419336(FSETTU845419336_frame);
dINLINE_ITER_SIG ELT_NI1331652151(ELT_NI1131308118);
dINLINE_ROUT_SIG ELT_NI706507177(ELT_NI52152274);
dLAYOUT ELT_NI947803854(ELT_NILdLAYOUT);
dLAYOUT FLISTd1954760550(FLISTd1954760550_frame);
dLAYOUT LAYOUT1657363748(LAYOUT1657363748_frame);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP ELT_NI1328289538(ELT_NILdTP);
void ARRAYF1076245134(ARRAYFSTR, INT, INT);
void ARRAYF394786536(ARRAYF394786536_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void ARRAYS1450857938(ARRAYSTR, INT, INT);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F1378335655(CODE_F1378335655_frame);
void CODE_F1479352172(CODE_FILE, PROG, STR);
void CODE_F1933153953(CODE_FILE);
void CODE_F2003422967(CODE_FILE_ARRAY, INT, INT);
void CODE_F2105979088(CODE_FILE);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void CODE_F65146505(CODE_FILE);
void FILE_plus_FSTR(FILE1, FSTR);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void INT_timesb(INT_timesb_frame);
void LAYOUT2061379740(LAYOUT_ARRAY, INT, INT);
void LAYOUT2103796202(LAYOUT2103796202_frame);
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

BOOL ATTRSO1207195602(ATTRSORT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 ATTRSORT is_eq_self;
 ATTRSORT is_eq_b;
 BOOL ret_val1 = BOOL_zero;
 if (arg==NULL) {
  goto other160;
 } else
 switch (TAG(arg)) {
  case ATTRSORT_tag:
   is_eq_self = self;
   is_eq_b = ((ATTRSORT) arg);
   ret_val1 = STR_is111530248(ATTR(is_eq_self,name1), ATTR(is_eq_b,name1));
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other160: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CODE_F1219277062(CODE_FILE self, CODE_FILE s) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L19;
 BOOL L201_;
 BOOL L21;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L25;
 BOOL L261_;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L31;
 BOOL L321_;
 BOOL L33;
 BOOL L34;
 BOOL L351_;
 L9 = ATTR(self,chk_pre);
 L10 = ATTR(s,chk_pre);
 L111_=L9==L10; 
 if (L111_) {
  L12 = ATTR(self,chk_post);
  L13 = ATTR(s,chk_post);
  L141_=L12==L13; 
  L8 = L141_;
 } else {
  L8 = FALSE;
 }
 if (L8) {
  L15 = ATTR(self,chk_invariant);
  L16 = ATTR(s,chk_invariant);
  L171_=L15==L16; 
  L7 = L171_;
 } else {
  L7 = FALSE;
 }
 if (L7) {
  L18 = ATTR(self,chk_assert);
  L19 = ATTR(s,chk_assert);
  L201_=L18==L19; 
  L6 = L201_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  L21 = ATTR(self,chk_arith);
  L22 = ATTR(s,chk_arith);
  L231_=L21==L22; 
  L5 = L231_;
 } else {
  L5 = FALSE;
 }
 if (L5) {
  L24 = ATTR(self,chk_bounds);
  L25 = ATTR(s,chk_bounds);
  L261_=L24==L25; 
  L4 = L261_;
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L27 = ATTR(self,chk_void);
  L28 = ATTR(s,chk_void);
  L291_=L27==L28; 
  L3 = L291_;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  L30 = ATTR(self,chk_when);
  L31 = ATTR(s,chk_when);
  L321_=L30==L31; 
  L2 = L321_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L33 = ATTR(self,chk_return);
  L34 = ATTR(s,chk_return);
  L351_=L33==L34; 
  L1 = L351_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CSE_DO4156401(CSE_DOUBLE_EXPRS self, OB arg) {
 BOOL ret_val = BOOL_zero;
 CSE_DOUBLE_EXPRS is_eq_self;
 CSE_DOUBLE_EXPRS is_eq_e;
 BOOL ret_val1 = BOOL_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val2;
 FLISTdAM_EXPR aget_self1;
 INT aget_ind1 = INT_zero;
 dAM_EXPR ret_val3;
 dAM_EXPR L11_;
 dAM_EXPR L21_;
 dAM_EXPR L3;
 if (arg==NULL) {
  goto other163;
 } else
 switch (TAG(arg)) {
  case CSE_DOUBLE_EXPRS_tag:
   is_eq_self = self;
   is_eq_e = ((CSE_DOUBLE_EXPRS) arg);
   aget_self = ATTR(is_eq_self,expr);
   aget_ind = 0;
   L11_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
   ret_val2 = L11_;
   aget_self1 = ATTR(is_eq_e,expr);
   aget_ind1 = 0;
   L21_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self1,aget_ind1); 
   ret_val3 = L21_;
   L3 = ret_val2;
   ret_val1 = (*dAM_EX2004971809[TAG(L3)])(L3, ((OB) ret_val3));
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other163: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ1393525399(ELT_EQdLAYOUT self, dLAYOUT e1, dLAYOUT e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other181;
 } else
 switch (TAG(e1)) {
  default: ;
  other181: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ182879965(ELT_EQ773986801 self, dINLINE_ITER_SIG e1, dINLINE_ITER_SIG e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other456;
 } else
 switch (TAG(e1)) {
  default: ;
  other456: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ1977650129(ELT_EQdCONFIG self, dCONFIG e1, dCONFIG e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other209;
 } else
 switch (TAG(e1)) {
  default: ;
  other209: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ2067825030(ELT_EQdAM_STMT self, dAM_STMT e1, dAM_STMT e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other188;
 } else
 switch (TAG(e1)) {
  default: ;
  other188: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ398237803(ELT_EQ1957447193 self, dINLINE_ROUT_SIG e1, dINLINE_ROUT_SIG e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other217;
 } else
 switch (TAG(e1)) {
  default: ;
  other217: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ686719113(ELT_EQdAS_EXPR self, dAS_EXPR e1, dAS_EXPR e2) {
 BOOL ret_val = BOOL_zero;
 if (e1==NULL) {
  goto other176;
 } else
 switch (TAG(e1)) {
  default: ;
  other176: ;
   ret_val = SYSOBEQ(((OB) e1),((OB) e2));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_EQ936498558(ELT_EQdOB self, OB e1, OB e2) {
 BOOL ret_val = BOOL_zero;
 dIS_EQ L1;
 if (e1==NULL) {
  goto other178;
 } else
 switch (TAG(e1)) {
  case AM_ANY_EXPR_tag:
  case AM_ARRAY_EXPR_tag:
  case AM_ARR_CONST_tag:
  case AM_ATTR_EXPR_tag:
  case AM_AT_EXPR_tag:
  case AM_BND1124877163_tag:
  case AM_BND367211769_tag:
  case AM_BND260301329_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CLUSTER_EXPR_tag:
  case AM_CLU1286269335_tag:
  case AM_CONST_tag:
  case AM_CURSOR_tag:
  case AM_CURSOR_POS_tag:
  case AM_EXCEPT_EXPR_tag:
  case AM_EXT_CALL_EXPR_tag:
  case AM_FAR_EXPR_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_GLOBAL_EXPR_tag:
  case AM_HERE_EXPR_tag:
  case AM_IF_EXPR_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_IS_VOID_EXPR_tag:
  case AM_ITE1809135850_tag:
  case AM_LOCAL_EXPR_tag:
  case AM_NEAR_EXPR_tag:
  case AM_NEW_EXPR_tag:
  case AM_ROU1916046290_tag:
  case AM_SHARED_EXPR_tag:
  case AM_STMT_EXPR_tag:
  case AM_STR_CONST_tag:
  case AM_VAR744470097_tag:
  case AM_VAT319982528_tag:
  case AM_VOID_CONST_tag:
  case AM_WHERE_EXPR_tag:
  case ATTRSORT_tag:
  case BOOL_tag:
  case CHAR_tag:
  case CSE_DOUBLE_EXPRS_tag:
  case FLT_tag:
  case FLTD_tag:
  case FSTR_tag:
  case IDENT_tag:
  case INT_tag:
  case INTI1_tag:
  case RAT_tag:
  case SFILE_ID_tag:
  case SIDE_EFFECT_tag:
  case SIG_tag:
  case STR_tag:
  case TP_CLASS_tag:
  case TP_ITER_tag:
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
   L1 = ((dIS_EQ) e1);
   ret_val = (*dIS_EQ1272012319[TAG(L1)])(L1, e2);
   return ret_val; break;
  default: ;
  other178: ;
   ret_val = SYSOBEQ(e1,e2);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_NI125473534(ELT_NILdTP self, dTP e) {
 BOOL ret_val = BOOL_zero;
 if (e==NULL) {
  goto other19;
 } else
 switch (TAG(e)) {
  default: ;
  other19: ;
   ret_val = (e==((dTP) NULL));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_NI1893940731(ELT_NI52152274 self, dINLINE_ROUT_SIG e) {
 BOOL ret_val = BOOL_zero;
 if (e==NULL) {
  goto other135;
 } else
 switch (TAG(e)) {
  default: ;
  other135: ;
   ret_val = (e==((dINLINE_ROUT_SIG) NULL));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_NI455285430(ELT_NILdOB self, OB e) {
 BOOL ret_val = BOOL_zero;
 dIS_NIL L1;
 if (e==NULL) {
  goto other90;
 } else
 switch (TAG(e)) {
  case FLT_tag:
  case FLTD_tag:
  case INT_tag:
   L1 = ((dIS_NIL) e);
   ret_val = (*dIS_NI446211542[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other90: ;
   ret_val = (e==((OB) NULL));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_NI569783157(ELT_NI1131308118 self, dINLINE_ITER_SIG e) {
 BOOL ret_val = BOOL_zero;
 if (e==NULL) {
  goto other295;
 } else
 switch (TAG(e)) {
  default: ;
  other295: ;
   ret_val = (e==((dINLINE_ITER_SIG) NULL));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL ELT_NI978759818(ELT_NILdLAYOUT self, dLAYOUT e) {
 BOOL ret_val = BOOL_zero;
 if (e==NULL) {
  goto other17;
 } else
 switch (TAG(e)) {
  default: ;
  other17: ;
   ret_val = (e==((dLAYOUT) NULL));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CODE_FILE CODE_F1432464034(CODE_FILE self, STR s) {
 CODE_FILE ret_val;
 CODE_FILE r;
 FLISTFSTR create_self;
 FLISTFSTR ret_val1;
 FSTR create_self1;
 FSTR ret_val2;
 FSETSIG create_self2;
 FSETSIG ret_val3;
 FSETdLAYOUT create_self3;
 FSETdLAYOUT ret_val4;
 FSETAM1542678947 create_self4;
 FSETAM1542678947 ret_val5;
 FSETAM1032778315 create_self5;
 FSETAM1032778315 ret_val6;
 FLISTA1893461511 create_self6;
 FLISTA1893461511 ret_val7;
 FSETTUPSIGSIG create_self7;
 FSETTUPSIGSIG ret_val8;
 FSETSTR create_self8;
 FSETSTR ret_val9;
 FSETSIG create_self9;
 FSETSIG ret_val10;
 CODE_FILE L1;
 OB L2;
 FSTR L3;
 INT L4;
 OB L5;
 INT L6;
 L2=ZALLOC(sizeof(struct CODE_FILE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=CODE_FILE_tag;
 L1 = ((CODE_FILE) L2);
 r = L1;
 create_self = ((FLISTFSTR) NULL);
 ret_val1 = ((FLISTFSTR) NULL);
 SATTR(r,text1,ret_val1);
 create_self1 = ((FSTR) NULL);
 L4 = 16;
 L6=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L4)*sizeof(CHAR);
 L5=ZALLOC_LEAF_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,L6);
 ((OB)L5)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSTR) L5);
 L3->asize = L4;
 ret_val2 = L3;
 SATTR(r,ntext,ret_val2);
 SATTR(r,length1,0);
 SATTR(r,name1,s);
 create_self2 = ((FSETSIG) NULL);
 ret_val3 = ((FSETSIG) NULL);
 SATTR(r,decs,ret_val3);
 create_self3 = ((FSETdLAYOUT) NULL);
 ret_val4 = ((FSETdLAYOUT) NULL);
 SATTR(r,layouts,ret_val4);
 create_self4 = ((FSETAM1542678947) NULL);
 ret_val5 = ((FSETAM1542678947) NULL);
 SATTR(r,globals,ret_val5);
 create_self5 = ((FSETAM1032778315) NULL);
 ret_val6 = ((FSETAM1032778315) NULL);
 SATTR(r,bnd_rout_creates,ret_val6);
 create_self6 = ((FLISTA1893461511) NULL);
 ret_val7 = ((FLISTA1893461511) NULL);
 SATTR(r,bnd_iter_creates,ret_val7);
 create_self7 = ((FSETTUPSIGSIG) NULL);
 ret_val8 = ((FSETTUPSIGSIG) NULL);
 SATTR(r,unboxes,ret_val8);
 create_self8 = ((FSETSTR) NULL);
 ret_val9 = ((FSETSTR) NULL);
 SATTR(r,externs1,ret_val9);
 create_self9 = ((FSETSIG) NULL);
 ret_val10 = ((FSETSIG) NULL);
 SATTR(r,iters,ret_val10);
 SATTR(r,do_not_merge,FALSE);
 CODE_FILE_all = FLISTC1532120756(CODE_FILE_all, r);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CONFIG_DEF CONFIG2114270854(CONFIG_DEF self, STR n, FLISTFLISTSTR e) {
 CONFIG_DEF ret_val;
 CONFIG_DEF r;
 ARRAYARRAYSTR L11;
 INT i = INT_zero;
 FLISTSTR L21;
 ARRAYSTR L31;
 CONFIG_DEF create_self;
 STR create_n;
 CONFIG_DEF ret_val1;
 CONFIG_DEF r1;
 ARRAYARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYARRAYSTR ret_val2;
 INT L41 = INT_zero;
 FLISTFLISTSTR aget_self;
 INT aget_ind = INT_zero;
 FLISTSTR ret_val3;
 ARRAYSTR create_self2;
 INT create_n2 = INT_zero;
 ARRAYSTR ret_val4;
 FLISTFLISTSTR aget_self1;
 INT aget_ind1 = INT_zero;
 FLISTSTR ret_val5;
 INT L51 = INT_zero;
 CONFIG_DEF L6;
 OB L7;
 ARRAYARRAYSTR L8;
 INT L9;
 OB L10;
 INT L12;
 INT L131_br;
 INT rL131_;
 INT L14;
 ARRAYARRAYSTR L15;
 FLISTSTR L161_;
 ARRAYSTR L17;
 INT L18;
 OB L19;
 INT L20;
 ARRAYSTR L22;
 ARRAYARRAYSTR L25;
 ARRAYSTR L261_;
 INT L241_br;
 STR L27;
 FLISTSTR L281_;
 STR L29;
 INT L301_;
 INT L321_;
 create_self = self;
 create_n = n;
 L7=ZALLOC(sizeof(struct CONFIG_DEF_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=CONFIG_DEF_tag;
 L6 = ((CONFIG_DEF) L7);
 r1 = L6;
 SATTR(r1,name1,create_n);
 ret_val1 = r1;
 r = ret_val1;
 create_self1 = ((ARRAYARRAYSTR) NULL);
 create_n1 = FLISTF1097166371(e);
 L9 = create_n1;
 L12=(sizeof(struct ARRAYARRAYSTR_struct)-sizeof(ARRAYSTR))+(L9)*sizeof(ARRAYSTR);
 L10=ZALLOC_BIG(L12);
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=ARRAYARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L10)->header.destroyed=0;
#endif

 L8 = ((ARRAYARRAYSTR) L10);
 L8->asize = L9;
 ret_val2 = L8;
 SATTR(r,expr,ret_val2);
 {
  BOOL f_L131_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ATTR(r,expr);
  L131_br=L11==NULL?0:ASIZE((ARRAYARRAYSTR)L11); 
  while (1) {
   if(L41>=L131_br)  goto after_loop; 
   rL131_=L41; 
   i = rL131_;
   L15 = ATTR(r,expr);
   create_self2 = ((ARRAYSTR) NULL);
   aget_self = e;
   aget_ind = i;
   L161_=(FLISTSTR)ARR((FLISTFLISTSTR)aget_self,aget_ind); 
   ret_val3 = L161_;
   create_n2 = FLISTS326330076(ret_val3);
   L18 = create_n2;
   L20=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L18)*sizeof(STR);
   L19=ZALLOC_BIG(L20);
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L19)->header.destroyed=0;
#endif

   L17 = ((ARRAYSTR) L19);
   L17->asize = L18;
   ret_val4 = L17;
   L22 = ret_val4;
   SARR((ARRAYARRAYSTR)L15,i,(ARRAYSTR)L22); 
   ;
   {
    struct FLISTS1171631134_frame_struct other2_0;
FLISTS1171631134_frame noname1 = &other2_0;
    BOOL f_L241_ = TRUE;
    /* loop index variable */
    L51 = 0;
    noname1->state = 0;
    while (1) {
     if (f_L241_) {
      f_L241_ = FALSE;
      L25 = ATTR(r,expr);
      L261_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L25,i); 
      L31 = L261_;
      L241_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
     }
     if(L51>=L241_br)  goto after_loop1; 
     if (noname1->state == 0) {
      aget_self1 = e;
      aget_ind1 = i;
      L281_=(FLISTSTR)ARR((FLISTFLISTSTR)aget_self1,aget_ind1); 
      ret_val5 = L281_;
      L21 = ret_val5;
      noname1->self = L21;
     }
     L27 = FLISTS1171631134(noname1);
     if (noname1->state == -1) {
      goto after_loop1;
     }
     L29 = L27;
     SARR((ARRAYSTR)L31,L51,(STR)L29); 
     ;
     L301_=INTPLUS(L51,1); 
     L51 = L301_;
    }
   }
   after_loop1: ;
   L321_=INTPLUS(L41,1); 
   L41 = L321_;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR CODE_F1554664505(CODE_FILE self) {
 FSTR ret_val;
 FSTR f;
 FLISTdLAYOUT L11;
 dLAYOUT l;
 FLISTSTR L21;
 STR t;
 ARRAYSTR globstr;
 FSETAM1542678947 L31;
 AM_SHARED_EXPR age;
 ARRAYSTR L41;
 ARRAYSTR L51;
 FLISTSTR decstr;
 FSETSIG L61;
 SIG d;
 CODE_FILE L71;
 ARRAYSTR decstrs;
 FLISTSTR L81;
 ARRAYSTR L91;
 ARRAYSTR L101;
 ARRAYSTR unstr;
 FSETTUPSIGSIG L121;
 TUPSIGSIG pair = TUPSIGSIG_zero;
 ARRAYSTR L131;
 ARRAYSTR L141;
 ARRAYSTR exstr;
 FSETSTR L151;
 ARRAYSTR L161;
 ARRAYSTR L171;
 FSTR create_self;
 FSTR ret_val1;
 ARRAYSTR create_self1;
 INT create_n = INT_zero;
 ARRAYSTR ret_val2;
 AM_SHARED_EXPR tp_self;
 dTP ret_val3;
 CODE_FILE mang_self;
 OB mang_ob;
 STR ret_val4;
 CODE_FILE mang_self1;
 OB mang_ob1;
 STR ret_val5;
 INT L181 = INT_zero;
 ARRAYSTR sort_self;
 INT L191 = INT_zero;
 ARRAYSTR create_self2;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val6;
 INT L201 = INT_zero;
 ARRAYSTR sort_self1;
 INT L221 = INT_zero;
 ARRAYSTR create_self3;
 INT create_n2 = INT_zero;
 ARRAYSTR ret_val7;
 INT L231 = INT_zero;
 ARRAYSTR sort_self2;
 INT L241 = INT_zero;
 ARRAYSTR create_self4;
 INT create_n3 = INT_zero;
 ARRAYSTR ret_val8;
 INT L251 = INT_zero;
 ARRAYSTR sort_self3;
 INT L261 = INT_zero;
 FSTR L27;
 INT L28;
 OB L29;
 INT L30;
 extern STR definePRE_CHK;
 extern STR definePOST_CHK;
 extern STR define1369334899;
 extern STR defineASSERT_CHK;
 extern STR defineARITH_CHK;
 extern STR defineBOUNDS_CHK;
 extern STR defineVOID_CHK;
 extern STR defineWHEN_CHK;
 extern STR defineRETURN_CHK;
 extern STR includesatherh;
 extern STR Layouts;
 dLAYOUT L32;
 STR L33;
 extern STR includetagsh;
 extern STR Globals;
 ARRAYSTR L34;
 INT L35;
 OB L36;
 INT L37;
 INT L381_br;
 AM_SHARED_EXPR L39;
 extern STR extern2;
 extern STR name10;
 extern STR name13;
 STR L40;
 INT L421_;
 BOOL L43;
 BOOL L441_;
 INT L451_;
 INT L46;
 INT L471_;
 INT L481_br;
 STR aL481_;
 STR L49;
 INT L501_;
 extern STR Functi2098237195;
 SIG L53;
 STR L54;
 ARRAYSTR L55;
 INT L56;
 OB L57;
 INT L58;
 INT L591_br;
 STR L60;
 STR L62;
 INT L631_;
 BOOL L64;
 BOOL L651_;
 BOOL L67;
 BOOL L681_;
 INT L691_;
 INT L70;
 INT L721_;
 INT L731_br;
 STR aL731_;
 STR L74;
 INT L751_;
 extern STR Boundr882254879;
 extern STR Boundi775344439;
 extern STR Unboxdecls;
 ARRAYSTR L77;
 INT L78;
 OB L79;
 INT L80;
 INT L821_br;
 TUPSIGSIG L83;
 STR L84;
 INT L851_;
 BOOL L86;
 BOOL L871_;
 INT L881_;
 INT L89;
 INT L901_;
 INT L921_br;
 STR aL921_;
 STR L93;
 INT L941_;
 extern STR Externalcalls;
 ARRAYSTR L96;
 INT L97;
 OB L98;
 INT L99;
 INT L1001_br;
 STR L102;
 STR L103;
 INT L1041_;
 BOOL L105;
 BOOL L1061_;
 INT L1071_;
 INT L108;
 INT L1091_;
 INT L1101_br;
 STR aL1101_;
 STR L111;
 INT L1121_;
 extern STR Code;
 create_self = ((FSTR) NULL);
 L28 = 16;
 L30=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L28)*sizeof(CHAR);
 L29=ZALLOC_LEAF_BIG(L30);
 if (L29==NULL) FATAL("Unable to allocate more memory");
 memset(L29,0,L30);
 ((OB)L29)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L29)->header.destroyed=0;
#endif

 L27 = ((FSTR) L29);
 L27->asize = L28;
 ret_val1 = L27;
 f = ret_val1;
 if (ATTR(self,chk_pre)) {
  f = FSTR_p1752847026(f, ((STR) &definePRE_CHK));
 }
 if (ATTR(self,chk_post)) {
  f = FSTR_p1752847026(f, ((STR) &definePOST_CHK));
 }
 if (ATTR(self,chk_invariant)) {
  f = FSTR_p1752847026(f, ((STR) &define1369334899));
 }
 if (ATTR(self,chk_assert)) {
  f = FSTR_p1752847026(f, ((STR) &defineASSERT_CHK));
 }
 if (ATTR(self,chk_arith)) {
  f = FSTR_p1752847026(f, ((STR) &defineARITH_CHK));
 }
 if (ATTR(self,chk_bounds)) {
  f = FSTR_p1752847026(f, ((STR) &defineBOUNDS_CHK));
 }
 if (ATTR(self,chk_void)) {
  f = FSTR_p1752847026(f, ((STR) &defineVOID_CHK));
 }
 if (ATTR(self,chk_when)) {
  f = FSTR_p1752847026(f, ((STR) &defineWHEN_CHK));
 }
 if (ATTR(self,chk_return)) {
  f = FSTR_p1752847026(f, ((STR) &defineRETURN_CHK));
 }
 f = FSTR_p1752847026(f, ((STR) &includesatherh));
 f = FSTR_p1752847026(f, ((STR) &Layouts));
 {
  struct FLISTd1954760550_frame_struct other1_0;
FLISTd1954760550_frame noname1 = &other1_0;
  struct FLISTS1171631134_frame_struct other1_1;
FLISTS1171631134_frame noname2 = &other1_1;
  L11 = CODE_F19188927;
  L21 = CODE_F1806827531;
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L32 = FLISTd1954760550(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   l = L32;
   L33 = FLISTS1171631134(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   t = L33;
   if (FSETdL1260128492(ATTR(self,layouts), l)) {
    f = FSTR_p1752847026(f, t);
   }
  }
 }
 after_loop: ;
 f = FSTR_p1752847026(f, ((STR) &includetagsh));
 f = FSTR_p1752847026(f, ((STR) &Globals));
 create_self1 = ((ARRAYSTR) NULL);
 create_n = FSETAM2105800198(ATTR(self,globals));
 L35 = create_n;
 L37=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L35)*sizeof(STR);
 L36=ZALLOC_BIG(L37);
 if (L36==NULL) FATAL("Unable to allocate more memory");
 ((OB)L36)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L36)->header.destroyed=0;
#endif

 L34 = ((ARRAYSTR) L36);
 L34->asize = L35;
 ret_val2 = L34;
 globstr = ret_val2;
 {
  struct FSETAM12483681_frame_struct other2_0;
FSETAM12483681_frame noname3 = &other2_0;
  BOOL f_L381_ = TRUE;
  L31 = ATTR(self,globals);
  /* loop index variable */
  L181 = 0;
  L41 = globstr;
  noname3->self = L31;
  noname3->state = 0;
  L381_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
  while (1) {
   L39 = FSETAM12483681(noname3);
   if (noname3->state == -1) {
    goto after_loop1;
   }
   age = L39;
   if(L181>=L381_br)  goto after_loop1; 
   mang_self = self;
   tp_self = age;
   ret_val3 = ATTR(tp_self,tp_at);
   mang_ob = ((OB) ret_val3);
   ret_val4 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob, ((OB) NULL));
   mang_self1 = self;
   mang_ob1 = ((OB) age);
   ret_val5 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob1, ((OB) NULL));
   L40 = STR_ap44172742(((STR) &extern2), ret_val4, ((STR) &name10), ret_val5, ((STR) &name13));
   SARR((ARRAYSTR)L41,L181,(STR)L40); 
   ;
   L421_=INTPLUS(L181,1); 
   L181 = L421_;
  }
 }
 after_loop1: ;
 sort_self = globstr;
 L43 = (sort_self==((ARRAYSTR) NULL));
 L441_=!(L43); 
 if (L441_) {
  L451_=ASIZE((ARRAYSTR)sort_self); 
  L46 = L451_;
  L471_=INTMINUS(L46,1); 
  ARRAYS1450857938(sort_self, 0, L471_);
 }
 {
  BOOL f_L481_ = TRUE;
  /* loop index variable */
  L191 = 0;
  L51 = globstr;
  L481_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
  while (1) {
   if(L191>=L481_br)  goto after_loop2; 
   aL481_=ARR((ARRAYSTR)L51,L191); 
   f = FSTR_p1752847026(f, aL481_);
   L501_=INTPLUS(L191,1); 
   L191 = L501_;
  }
 }
 after_loop2: ;
 SYSDESTROY(globstr); 
 ;
 f = FSTR_p1752847026(f, ((STR) &Functi2098237195));
 decstr = FLISTS1900495285(((FLISTSTR) NULL), FSETSIG_sizerINT(ATTR(self,decs)));
 {
  struct FSETSIG_eltbrSIG_frame_struct other4_0;
FSETSIG_eltbrSIG_frame noname4 = &other4_0;
  L61 = ATTR(self,decs);
  noname4->self = L61;
  noname4->state = 0;
  while (1) {
   L53 = FSETSIG_eltbrSIG(noname4);
   if (noname4->state == -1) {
    goto after_loop3;
   }
   d = L53;
   {
    struct CODE_F423548741_frame_struct other5_0;
CODE_F423548741_frame noname5 = &other5_0;
    L71 = self;
    noname5->self = L71;
    noname5->state = 0;
    while (1) {
     noname5->arg1 = d;
     L54 = CODE_F423548741(noname5);
     if (noname5->state == -1) {
      goto after_loop4;
     }
     decstr = FLISTS1425610062(decstr, L54);
    }
   }
   after_loop4: ;
  }
 }
 after_loop3: ;
 create_self2 = ((ARRAYSTR) NULL);
 create_n1 = FLISTS326330076(decstr);
 L56 = create_n1;
 L58=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L56)*sizeof(STR);
 L57=ZALLOC_BIG(L58);
 if (L57==NULL) FATAL("Unable to allocate more memory");
 ((OB)L57)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L57)->header.destroyed=0;
#endif

 L55 = ((ARRAYSTR) L57);
 L55->asize = L56;
 ret_val6 = L55;
 decstrs = ret_val6;
 {
  struct FLISTS1171631134_frame_struct other6_0;
FLISTS1171631134_frame noname6 = &other6_0;
  BOOL f_L591_ = TRUE;
  L81 = decstr;
  /* loop index variable */
  L201 = 0;
  L91 = decstrs;
  noname6->self = L81;
  noname6->state = 0;
  L591_br=L91==NULL?0:ASIZE((ARRAYSTR)L91); 
  while (1) {
   if(L201>=L591_br)  goto after_loop5; 
   L60 = FLISTS1171631134(noname6);
   if (noname6->state == -1) {
    goto after_loop5;
   }
   L62 = L60;
   SARR((ARRAYSTR)L91,L201,(STR)L62); 
   ;
   L631_=INTPLUS(L201,1); 
   L201 = L631_;
  }
 }
 after_loop5: ;
 L64 = (decstr==((FLISTSTR) NULL));
 L651_=!(L64); 
 if (L651_) {
  SYSDESTROY(decstr); 
  ;
 }
 sort_self1 = decstrs;
 L67 = (sort_self1==((ARRAYSTR) NULL));
 L681_=!(L67); 
 if (L681_) {
  L691_=ASIZE((ARRAYSTR)sort_self1); 
  L70 = L691_;
  L721_=INTMINUS(L70,1); 
  ARRAYS1450857938(sort_self1, 0, L721_);
 }
 {
  BOOL f_L731_ = TRUE;
  /* loop index variable */
  L221 = 0;
  L101 = decstrs;
  L731_br=L101==NULL?0:ASIZE((ARRAYSTR)L101); 
  while (1) {
   if(L221>=L731_br)  goto after_loop6; 
   aL731_=ARR((ARRAYSTR)L101,L221); 
   f = FSTR_p1752847026(f, aL731_);
   L751_=INTPLUS(L221,1); 
   L221 = L751_;
  }
 }
 after_loop6: ;
 SYSDESTROY(decstrs); 
 ;
 f = FSTR_p1752847026(f, ((STR) &Boundr882254879));
 f = FSTR_p1752847026(f, CODE_F244102140(self));
 f = FSTR_p1752847026(f, ((STR) &Boundi775344439));
 f = FSTR_p1752847026(f, CODE_F44077436(self));
 f = FSTR_p1752847026(f, ((STR) &Unboxdecls));
 create_self3 = ((ARRAYSTR) NULL);
 create_n2 = FSETTU1591851819(ATTR(self,unboxes));
 L78 = create_n2;
 L80=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L78)*sizeof(STR);
 L79=ZALLOC_BIG(L80);
 if (L79==NULL) FATAL("Unable to allocate more memory");
 ((OB)L79)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L79)->header.destroyed=0;
#endif

 L77 = ((ARRAYSTR) L79);
 L77->asize = L78;
 ret_val7 = L77;
 unstr = ret_val7;
 {
  struct FSETTU845419336_frame_struct other8_0;
FSETTU845419336_frame noname7 = &other8_0;
  BOOL f_L821_ = TRUE;
  L121 = ATTR(self,unboxes);
  /* loop index variable */
  L231 = 0;
  L131 = unstr;
  noname7->self = L121;
  noname7->state = 0;
  L821_br=L131==NULL?0:ASIZE((ARRAYSTR)L131); 
  while (1) {
   L83 = FSETTU845419336(noname7);
   if (noname7->state == -1) {
    goto after_loop7;
   }
   pair = L83;
   if(L231>=L821_br)  goto after_loop7; 
   L84 = CODE_F223990950(self, pair.t1, pair.t2);
   SARR((ARRAYSTR)L131,L231,(STR)L84); 
   ;
   L851_=INTPLUS(L231,1); 
   L231 = L851_;
  }
 }
 after_loop7: ;
 sort_self2 = unstr;
 L86 = (sort_self2==((ARRAYSTR) NULL));
 L871_=!(L86); 
 if (L871_) {
  L881_=ASIZE((ARRAYSTR)sort_self2); 
  L89 = L881_;
  L901_=INTMINUS(L89,1); 
  ARRAYS1450857938(sort_self2, 0, L901_);
 }
 {
  BOOL f_L921_ = TRUE;
  /* loop index variable */
  L241 = 0;
  L141 = unstr;
  L921_br=L141==NULL?0:ASIZE((ARRAYSTR)L141); 
  while (1) {
   if(L241>=L921_br)  goto after_loop8; 
   aL921_=ARR((ARRAYSTR)L141,L241); 
   f = FSTR_p1752847026(f, aL921_);
   L941_=INTPLUS(L241,1); 
   L241 = L941_;
  }
 }
 after_loop8: ;
 SYSDESTROY(unstr); 
 ;
 f = FSTR_p1752847026(f, ((STR) &Externalcalls));
 create_self4 = ((ARRAYSTR) NULL);
 create_n3 = FSETSTR_sizerINT(ATTR(self,externs1));
 L97 = create_n3;
 L99=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L97)*sizeof(STR);
 L98=ZALLOC_BIG(L99);
 if (L98==NULL) FATAL("Unable to allocate more memory");
 ((OB)L98)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L98)->header.destroyed=0;
#endif

 L96 = ((ARRAYSTR) L98);
 L96->asize = L97;
 ret_val8 = L96;
 exstr = ret_val8;
 {
  struct FSETSTR_eltbrSTR_frame_struct other10_0;
FSETSTR_eltbrSTR_frame noname8 = &other10_0;
  BOOL f_L1001_ = TRUE;
  L151 = ATTR(self,externs1);
  /* loop index variable */
  L251 = 0;
  L161 = exstr;
  noname8->self = L151;
  noname8->state = 0;
  L1001_br=L161==NULL?0:ASIZE((ARRAYSTR)L161); 
  while (1) {
   if(L251>=L1001_br)  goto after_loop9; 
   L102 = FSETSTR_eltbrSTR(noname8);
   if (noname8->state == -1) {
    goto after_loop9;
   }
   L103 = L102;
   SARR((ARRAYSTR)L161,L251,(STR)L103); 
   ;
   L1041_=INTPLUS(L251,1); 
   L251 = L1041_;
  }
 }
 after_loop9: ;
 sort_self3 = exstr;
 L105 = (sort_self3==((ARRAYSTR) NULL));
 L1061_=!(L105); 
 if (L1061_) {
  L1071_=ASIZE((ARRAYSTR)sort_self3); 
  L108 = L1071_;
  L1091_=INTMINUS(L108,1); 
  ARRAYS1450857938(sort_self3, 0, L1091_);
 }
 {
  BOOL f_L1101_ = TRUE;
  /* loop index variable */
  L261 = 0;
  L171 = exstr;
  L1101_br=L171==NULL?0:ASIZE((ARRAYSTR)L171); 
  while (1) {
   if(L261>=L1101_br)  goto after_loop10; 
   aL1101_=ARR((ARRAYSTR)L171,L261); 
   f = FSTR_p1752847026(f, aL1101_);
   L1121_=INTPLUS(L261,1); 
   L261 = L1121_;
  }
 }
 after_loop10: ;
 SYSDESTROY(exstr); 
 ;
 f = FSTR_p1752847026(f, ((STR) &Code));
 ret_val = f;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT CSE_DO1342102696(CSE_DOUBLE_EXPRS self) {
 INT ret_val = INT_zero;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val1;
 dAM_EXPR L11_;
 dAM_EXPR L2;
 aget_self = ATTR(self,expr);
 aget_ind = 0;
 L11_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
 ret_val1 = L11_;
 L2 = ret_val1;
 ret_val = (*dAM_EX1001627434[TAG(L2)])(L2);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT ELT_HA1612720024(ELT_HASH self, OB e) {
 INT ret_val = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 dHASH L1;
 extern STR Cannot1624781897;
 INT L21_;
 INT L3;
 STR L41_;
 extern STR sincei1786532990;
 INT L51_;
 if (e==NULL) {
  goto other94;
 } else
 switch (TAG(e)) {
  case AM_ANY_EXPR_tag:
  case AM_ARRAY_EXPR_tag:
  case AM_ARR_CONST_tag:
  case AM_ATTR_EXPR_tag:
  case AM_AT_EXPR_tag:
  case AM_BND1124877163_tag:
  case AM_BND367211769_tag:
  case AM_BND260301329_tag:
  case AM_BOOL_CONST_tag:
  case AM_CHAR_CONST_tag:
  case AM_CLUSTER_EXPR_tag:
  case AM_CLU1286269335_tag:
  case AM_CONST_tag:
  case AM_EXCEPT_EXPR_tag:
  case AM_EXT_CALL_EXPR_tag:
  case AM_FAR_EXPR_tag:
  case AM_FLTDX_CONST_tag:
  case AM_FLTD_CONST_tag:
  case AM_FLTI_CONST_tag:
  case AM_FLTX_CONST_tag:
  case AM_FLT_CONST_tag:
  case AM_GLOBAL_EXPR_tag:
  case AM_HERE_EXPR_tag:
  case AM_IF_EXPR_tag:
  case AM_INTI_CONST_tag:
  case AM_INT_CONST_tag:
  case AM_IS_VOID_EXPR_tag:
  case AM_ITE1809135850_tag:
  case AM_LOCAL_EXPR_tag:
  case AM_NEAR_EXPR_tag:
  case AM_NEW_EXPR_tag:
  case AM_ROU1916046290_tag:
  case AM_SHARED_EXPR_tag:
  case AM_STMT_EXPR_tag:
  case AM_STR_CONST_tag:
  case AM_VAR744470097_tag:
  case AM_VAT319982528_tag:
  case AM_VOID_CONST_tag:
  case AM_WHERE_EXPR_tag:
  case CHAR_tag:
  case CSE_DOUBLE_EXPRS_tag:
  case FLT_tag:
  case FLTD_tag:
  case FSTR_tag:
  case IDENT_tag:
  case INT_tag:
  case INTI1_tag:
  case SFILE_ID_tag:
  case SIDE_EFFECT_tag:
  case SIG_tag:
  case STR_tag:
  case TP_CLASS_tag:
  case TP_ITER_tag:
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
   L1 = ((dHASH) e);
   ret_val = (*dHASH_hashrINT[TAG(L1)])(L1);
   return ret_val; break;
  case AM_CURSOR_tag:
  case AM_CURSOR_POS_tag:
  case ATTRSORT_tag:
  case BOOL_tag:
  case RAT_tag:
   plus_self = ((STR) &Cannot1624781897);
   L21_=SYSTP(e); 
   L3 = L21_;
   L41_=SYSSTRFORTP(L3); 
   plus_sarg = L41_;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &sincei1786532990);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   RAISE(((OB) ret_val2)); break;
  default: ;
  other94: ;
   if ((e==((OB) NULL))) {
    ret_val = 0;
    return ret_val;
   }
   else {
    L51_=SYSID(e); 
    ret_val = INT_hashrINT(L51_);
    return ret_val;
   }
 }
}


#undef IS_ITER
#define IS_ITER 0

OB ELT_NI509824640(ELT_NILdOB self) {
 OB ret_val;
 OB e = ((OB) NULL);
 dNIL res;
 dNIL L1;
 if (e==NULL) {
  goto other218;
 } else
 switch (TAG(e)) {
  case FLT_tag:
  case FLTD_tag:
  case INT_tag:
   L1 = ((dNIL) e);
   res = (*dNIL_nilrdNIL[TAG(L1)])(L1);
   ret_val = ((OB) res);
   return ret_val; break;
  default: ;
  other218: ;
   ret_val = ((OB) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_F1366222275(CODE_FILE self) {
 STR ret_val;
 CODE_FILE_ARRAY sorted;
 FLISTCODE_FILE L11;
 CODE_FILE_ARRAY L21;
 CODE_FILE last1;
 CODE_FILE_ARRAY L31;
 CODE_FILE next;
 STR res;
 FLISTCODE_FILE L41;
 CODE_FILE next1;
 CODE_FILE_ARRAY create_self;
 INT create_n = INT_zero;
 CODE_FILE_ARRAY ret_val1;
 INT L51 = INT_zero;
 CODE_FILE_ARRAY sort_self;
 FLISTCODE_FILE create_self1;
 FLISTCODE_FILE ret_val2;
 INT L61 = INT_zero;
 FSTR length_self;
 INT ret_val3 = INT_zero;
 FSTR create_self2;
 FSTR ret_val4;
 OUT create_self3;
 OUT ret_val5;
 OUT plus_self;
 STR plus_s;
 OUT ret_val6;
 FILE1 stdout_self;
 FILE1 ret_val7;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val8;
 FILE1 stdout_self1;
 FILE1 ret_val9;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val10;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r2;
 OUT plus_self3;
 dSTR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val12;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val13;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val14;
 FILE1 stdout_self4;
 FILE1 ret_val15;
 FILE1 r4;
 OUT plus_self5;
 dSTR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val16;
 FILE1 r5;
 OUT create_self5;
 OUT ret_val17;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val18;
 FILE1 r6;
 OUT create_self6;
 OUT ret_val19;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val20;
 FILE1 stdout_self7;
 FILE1 ret_val21;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val22;
 FILE1 stdout_self8;
 FILE1 ret_val23;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val24;
 FILE1 stdout_self9;
 FILE1 ret_val25;
 FILE1 r9;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val26;
 FILE1 stdout_self10;
 FILE1 ret_val27;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val28;
 FILE1 r11;
 FSETSIG union_self;
 FSETSIG union_s;
 FSETSIG ret_val29;
 FSETdLAYOUT union_self1;
 FSETdLAYOUT union_s1;
 FSETdLAYOUT ret_val30;
 FSETAM1542678947 union_self2;
 FSETAM1542678947 union_s2;
 FSETAM1542678947 ret_val31;
 FSETAM1032778315 union_self3;
 FSETAM1032778315 union_s3;
 FSETAM1032778315 ret_val32;
 FSETTUPSIGSIG union_self4;
 FSETTUPSIGSIG union_s4;
 FSETTUPSIGSIG ret_val33;
 FSETSTR union_self5;
 FSETSTR union_s5;
 FSETSTR ret_val34;
 FSETSIG union_self6;
 FSETSIG union_s6;
 FSETSIG ret_val35;
 STR plus_self12;
 CHAR plus_arg = CHAR_zero;
 STR ret_val36;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val37;
 STR create_self7;
 CHAR create_c = CHAR_zero;
 STR ret_val38;
 STR r12;
 STR replace_suffix_s;
 STR replace_suffix_o;
 STR replace_suffix_r;
 STR ret_val39;
 STR plus_self13;
 STR plus_sarg;
 STR ret_val40;
 STR plus_self14;
 STR plus_sarg1;
 STR ret_val41;
 CODE_FILE_ARRAY L7;
 INT L8;
 OB L9;
 INT L10;
 INT L121_br;
 CODE_FILE L13;
 CODE_FILE L14;
 INT L151_;
 BOOL L16;
 BOOL L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_br;
 CODE_FILE aL221_;
 CODE_FILE L23;
 INT L24;
 INT L25;
 INT L261_;
 FSTR L27;
 INT L28;
 OB L29;
 INT L30;
 BOOL L32;
 BOOL L331_;
 extern STR Considering;
 FILE1 L34;
 OB L35;
 FILE1 L37;
 OB L38;
 extern STR size6;
 FILE1 L40;
 OB L42;
 dSTR L44;
 OB L45;
 FILE1 L46;
 OB L47;
 BOOL L49;
 BOOL L501_;
 extern STR Currentlength;
 FILE1 L52;
 OB L53;
 dSTR L55;
 OB L56;
 FILE1 L57;
 OB L58;
 extern STR name9;
 FILE1 L60;
 OB L62;
 BOOL L64;
 BOOL L65;
 BOOL L661_;
 BOOL L67;
 BOOL L68;
 BOOL L691_;
 INT L70;
 INT L71;
 INT L72;
 INT L731_;
 INT L74;
 BOOL L751_;
 extern STR Merging;
 FILE1 L76;
 OB L77;
 FILE1 L79;
 OB L80;
 extern STR with4;
 FILE1 L82;
 OB L83;
 FILE1 L85;
 OB L86;
 extern STR name9;
 FILE1 L88;
 OB L89;
 INT L91;
 INT L92;
 INT L931_;
 FSETSIG L94;
 FSETdLAYOUT L95;
 FSETAM1542678947 L96;
 FSETAM1032778315 L97;
 FSETTUPSIGSIG L98;
 FSETSTR L99;
 FSETSIG L100;
 FLISTFSTR L101;
 BOOL L103;
 BOOL L1041_;
 FSETdLAYOUT L105;
 BOOL L107;
 BOOL L1081_;
 FSETSIG L109;
 INT L1111_;
 extern STR name1;
 CODE_FILE L112;
 STR L113;
 INT L114;
 OB L115;
 INT L116;
 extern STR C_EXT;
 extern STR OBJECT_EXT;
 INT L118;
 INT L119;
 INT L1201_;
 create_self = ((CODE_FILE_ARRAY) NULL);
 create_n = FLISTC1064619074(CODE_FILE_all);
 L8 = create_n;
 L10=(sizeof(struct CODE_FILE_ARRAY_struct)-sizeof(CODE_FILE))+(L8)*sizeof(CODE_FILE);
 L9=ZALLOC_BIG(L10);
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=CODE_FILE_ARRAY_tag;
#ifdef DESTROY_CHK

   ((OB)L9)->header.destroyed=0;
#endif

 L7 = ((CODE_FILE_ARRAY) L9);
 L7->asize = L8;
 ret_val1 = L7;
 sorted = ret_val1;
 {
  struct FLISTC1947519404_frame_struct other1_0;
FLISTC1947519404_frame noname1 = &other1_0;
  BOOL f_L121_ = TRUE;
  L11 = CODE_FILE_all;
  /* loop index variable */
  L51 = 0;
  L21 = sorted;
  noname1->self = L11;
  noname1->state = 0;
  L121_br=L21==NULL?0:ASIZE((CODE_FILE_ARRAY)L21); 
  while (1) {
   if(L51>=L121_br)  goto after_loop; 
   L13 = FLISTC1947519404(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L14 = L13;
   SARR((CODE_FILE_ARRAY)L21,L51,(CODE_FILE)L14); 
   ;
   L151_=INTPLUS(L51,1); 
   L51 = L151_;
  }
 }
 after_loop: ;
 sort_self = sorted;
 L16 = (sort_self==((CODE_FILE_ARRAY) NULL));
 L171_=!(L16); 
 if (L171_) {
  L181_=ASIZE((CODE_FILE_ARRAY)sort_self); 
  L19 = L181_;
  L201_=INTMINUS(L19,1); 
  CODE_F2003422967(sort_self, 0, L201_);
 }
 create_self1 = ((FLISTCODE_FILE) NULL);
 ret_val2 = ((FLISTCODE_FILE) NULL);
 CODE_FILE_todo = ret_val2;
 last1 = ((CODE_FILE) NULL);
 {
  BOOL f_L221_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L31 = sorted;
  L221_br=L31==NULL?0:ASIZE((CODE_FILE_ARRAY)L31); 
  while (1) {
   if(L61>=L221_br)  goto after_loop1; 
   aL221_=ARR((CODE_FILE_ARRAY)L31,L61); 
   next = aL221_;
   SATTR(next,text1,FLISTF1280148353(ATTR(next,text1), ATTR(next,ntext)));
   L24 = ATTR(next,length1);
   length_self = ATTR(next,ntext);
   ret_val3 = FSTR_sizerINT(length_self);
   L25 = ret_val3;
   L261_=INTPLUS(L24,L25); 
   SATTR(next,length1,L261_);
   create_self2 = ((FSTR) NULL);
   L28 = 16;
   L30=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L28)*sizeof(CHAR);
   L29=ZALLOC_LEAF_BIG(L30);
   if (L29==NULL) FATAL("Unable to allocate more memory");
   memset(L29,0,L30);
   ((OB)L29)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L29)->header.destroyed=0;
#endif

   L27 = ((FSTR) L29);
   L27->asize = L28;
   ret_val4 = L27;
   SATTR(next,ntext,ret_val4);
   if (ATTR(CODE_FILE_prog,prolix)) {
    L32 = (next==((CODE_FILE) NULL));
    L331_=!(L32); 
    if (L331_) {
     create_self3 = ((OUT) NULL);
     ret_val5 = create_self3;
     plus_self = ret_val5;
     plus_s = ((STR) &Considering);
     stdout_self = ((FILE1) NULL);
     L35=ZALLOC(sizeof(struct FILE1_struct));
     if (L35==NULL) FATAL("Unable to allocate more memory");
     ((OB)L35)->header.tag=FILE1_tag;
     L34 = ((FILE1) L35);
     r = L34;
     
     SATTR(r,fp,stdout);
     ret_val7 = r;
     FILE_plus_STR(ret_val7, plus_s);
     ret_val6 = plus_self;
     plus_self1 = ret_val6;
     plus_s1 = ATTR(next,name1);
     stdout_self1 = ((FILE1) NULL);
     L38=ZALLOC(sizeof(struct FILE1_struct));
     if (L38==NULL) FATAL("Unable to allocate more memory");
     ((OB)L38)->header.tag=FILE1_tag;
     L37 = ((FILE1) L38);
     r1 = L37;
     
     SATTR(r1,fp,stdout);
     ret_val9 = r1;
     FILE_plus_STR(ret_val9, plus_s1);
     ret_val8 = plus_self1;
     plus_self2 = ret_val8;
     plus_s2 = ((STR) &size6);
     stdout_self2 = ((FILE1) NULL);
     L42=ZALLOC(sizeof(struct FILE1_struct));
     if (L42==NULL) FATAL("Unable to allocate more memory");
     ((OB)L42)->header.tag=FILE1_tag;
     L40 = ((FILE1) L42);
     r2 = L40;
     
     SATTR(r2,fp,stdout);
     ret_val11 = r2;
     FILE_plus_STR(ret_val11, plus_s2);
     ret_val10 = plus_self2;
     plus_self3 = ret_val10;
     L45=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L45==NULL) FATAL("Unable to allocate more memory");
     memset(L45,0,sizeof(struct INT_boxed_struct));
     ((OB)L45)->header.tag=INT_tag;
     L44 = (dSTR)((INT_boxed) L45);
     ((INT_boxed) L44)->immutable_part = ATTR(next,length1);
     plus_s3 = L44;
     stdout_self3 = ((FILE1) NULL);
     L47=ZALLOC(sizeof(struct FILE1_struct));
     if (L47==NULL) FATAL("Unable to allocate more memory");
     ((OB)L47)->header.tag=FILE1_tag;
     L46 = ((FILE1) L47);
     r3 = L46;
     
     SATTR(r3,fp,stdout);
     ret_val12 = r3;
     FILE_plus_dSTR(ret_val12, plus_s3);
     L49 = (last1==((CODE_FILE) NULL));
     L501_=!(L49); 
     if (L501_) {
      create_self4 = ((OUT) NULL);
      ret_val13 = create_self4;
      plus_self4 = ret_val13;
      plus_s4 = ((STR) &Currentlength);
      stdout_self4 = ((FILE1) NULL);
      L53=ZALLOC(sizeof(struct FILE1_struct));
      if (L53==NULL) FATAL("Unable to allocate more memory");
      ((OB)L53)->header.tag=FILE1_tag;
      L52 = ((FILE1) L53);
      r4 = L52;
      
      SATTR(r4,fp,stdout);
      ret_val15 = r4;
      FILE_plus_STR(ret_val15, plus_s4);
      ret_val14 = plus_self4;
      plus_self5 = ret_val14;
      L56=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L56==NULL) FATAL("Unable to allocate more memory");
      memset(L56,0,sizeof(struct INT_boxed_struct));
      ((OB)L56)->header.tag=INT_tag;
      L55 = (dSTR)((INT_boxed) L56);
      ((INT_boxed) L55)->immutable_part = ATTR(last1,length1);
      plus_s5 = L55;
      stdout_self5 = ((FILE1) NULL);
      L58=ZALLOC(sizeof(struct FILE1_struct));
      if (L58==NULL) FATAL("Unable to allocate more memory");
      ((OB)L58)->header.tag=FILE1_tag;
      L57 = ((FILE1) L58);
      r5 = L57;
      
      SATTR(r5,fp,stdout);
      ret_val16 = r5;
      FILE_plus_dSTR(ret_val16, plus_s5);
     }
     create_self5 = ((OUT) NULL);
     ret_val17 = create_self5;
     plus_self6 = ret_val17;
     plus_s6 = ((STR) &name9);
     stdout_self6 = ((FILE1) NULL);
     L62=ZALLOC(sizeof(struct FILE1_struct));
     if (L62==NULL) FATAL("Unable to allocate more memory");
     ((OB)L62)->header.tag=FILE1_tag;
     L60 = ((FILE1) L62);
     r6 = L60;
     
     SATTR(r6,fp,stdout);
     ret_val18 = r6;
     FILE_plus_STR(ret_val18, plus_s6);
    }
   }
   L65 = ATTR(next,is_c_code);
   L661_=!(L65); 
   if (L661_) {
    L64 = TRUE;
   } else {
    L64 = ATTR(next,do_not_merge);
   }
   if (L64) {
    CODE_FILE_todo = FLISTC1532120756(CODE_FILE_todo, next);
   }
   else {
    if ((last1==((CODE_FILE) NULL))) {
     last1 = next;
    }
    else {
     L68 = CODE_F1219277062(last1, next);
     L691_=!(L68); 
     if (L691_) {
      L67 = TRUE;
     } else {
      L70 = ATTR(CODE_FILE_cgen,threshold);
      L71 = ATTR(last1,length1);
      L72 = ATTR(next,length1);
      L731_=INTPLUS(L71,L72); 
      L74 = L731_;
      L751_=(L70)<(L74); 
      L67 = L751_;
     }
     if (L67) {
      CODE_FILE_todo = FLISTC1532120756(CODE_FILE_todo, last1);
      last1 = next;
     }
     else {
      SATTR(last1,text1,FLISTF524229941(ATTR(last1,text1), ATTR(next,text1)));
      if (ATTR(CODE_FILE_prog,prolix)) {
       create_self6 = ((OUT) NULL);
       ret_val19 = create_self6;
       plus_self7 = ret_val19;
       plus_s7 = ((STR) &Merging);
       stdout_self7 = ((FILE1) NULL);
       L77=ZALLOC(sizeof(struct FILE1_struct));
       if (L77==NULL) FATAL("Unable to allocate more memory");
       ((OB)L77)->header.tag=FILE1_tag;
       L76 = ((FILE1) L77);
       r7 = L76;
       
       SATTR(r7,fp,stdout);
       ret_val21 = r7;
       FILE_plus_STR(ret_val21, plus_s7);
       ret_val20 = plus_self7;
       plus_self8 = ret_val20;
       plus_s8 = ATTR(last1,name1);
       stdout_self8 = ((FILE1) NULL);
       L80=ZALLOC(sizeof(struct FILE1_struct));
       if (L80==NULL) FATAL("Unable to allocate more memory");
       ((OB)L80)->header.tag=FILE1_tag;
       L79 = ((FILE1) L80);
       r8 = L79;
       
       SATTR(r8,fp,stdout);
       ret_val23 = r8;
       FILE_plus_STR(ret_val23, plus_s8);
       ret_val22 = plus_self8;
       plus_self9 = ret_val22;
       plus_s9 = ((STR) &with4);
       stdout_self9 = ((FILE1) NULL);
       L83=ZALLOC(sizeof(struct FILE1_struct));
       if (L83==NULL) FATAL("Unable to allocate more memory");
       ((OB)L83)->header.tag=FILE1_tag;
       L82 = ((FILE1) L83);
       r9 = L82;
       
       SATTR(r9,fp,stdout);
       ret_val25 = r9;
       FILE_plus_STR(ret_val25, plus_s9);
       ret_val24 = plus_self9;
       plus_self10 = ret_val24;
       plus_s10 = ATTR(next,name1);
       stdout_self10 = ((FILE1) NULL);
       L86=ZALLOC(sizeof(struct FILE1_struct));
       if (L86==NULL) FATAL("Unable to allocate more memory");
       ((OB)L86)->header.tag=FILE1_tag;
       L85 = ((FILE1) L86);
       r10 = L85;
       
       SATTR(r10,fp,stdout);
       ret_val27 = r10;
       FILE_plus_STR(ret_val27, plus_s10);
       ret_val26 = plus_self10;
       plus_self11 = ret_val26;
       plus_s11 = ((STR) &name9);
       stdout_self11 = ((FILE1) NULL);
       L89=ZALLOC(sizeof(struct FILE1_struct));
       if (L89==NULL) FATAL("Unable to allocate more memory");
       ((OB)L89)->header.tag=FILE1_tag;
       L88 = ((FILE1) L89);
       r11 = L88;
       
       SATTR(r11,fp,stdout);
       ret_val28 = r11;
       FILE_plus_STR(ret_val28, plus_s11);
      }
      L91 = ATTR(last1,length1);
      L92 = ATTR(next,length1);
      L931_=INTPLUS(L91,L92); 
      SATTR(last1,length1,L931_);
      union_self = ATTR(last1,decs);
      union_s = ATTR(next,decs);
      L94 = FSETSI719979460(union_self);
      ret_val29 = FSETSI548941202(L94, union_s);
      SATTR(last1,decs,ret_val29);
      union_self1 = ATTR(last1,layouts);
      union_s1 = ATTR(next,layouts);
      L95 = FSETdL1585396236(union_self1);
      ret_val30 = FSETdL1582668907(L95, union_s1);
      SATTR(last1,layouts,ret_val30);
      union_self2 = ATTR(last1,globals);
      union_s2 = ATTR(next,globals);
      L96 = FSETAM1609335141(union_self2);
      ret_val31 = FSETAM1298377859(L96, union_s2);
      SATTR(last1,globals,ret_val31);
      union_self3 = ATTR(last1,bnd_rout_creates);
      union_s3 = ATTR(next,bnd_rout_creates);
      L97 = FSETAM74171957(union_self3);
      ret_val32 = FSETAM381650379(L97, union_s3);
      SATTR(last1,bnd_rout_creates,ret_val32);
      SATTR(last1,bnd_iter_creates,FLISTA226859063(ATTR(last1,bnd_iter_creates), ATTR(next,bnd_iter_creates)));
      union_self4 = ATTR(last1,unboxes);
      union_s4 = ATTR(next,unboxes);
      L98 = FSETTU1446784814(union_self4);
      ret_val33 = FSETTU814351307(L98, union_s4);
      SATTR(last1,unboxes,ret_val33);
      union_self5 = ATTR(last1,externs1);
      union_s5 = ATTR(next,externs1);
      L99 = FSETST1884046860(union_self5);
      ret_val34 = FSETST866022026(L99, union_s5);
      SATTR(last1,externs1,ret_val34);
      union_self6 = ATTR(last1,iters);
      union_s6 = ATTR(next,iters);
      L100 = FSETSI719979460(union_self6);
      ret_val35 = FSETSI548941202(L100, union_s6);
      SATTR(last1,iters,ret_val35);
      L101 = ATTR(next,text1);
      SYSDESTROY(L101); 
      ;
      L103 = (ATTR(next,layouts)==((FSETdLAYOUT) NULL));
      L1041_=!(L103); 
      if (L1041_) {
       L105 = ATTR(next,layouts);
       SYSDESTROY(L105); 
       ;
      }
      L107 = (ATTR(next,decs)==((FSETSIG) NULL));
      L1081_=!(L107); 
      if (L1081_) {
       L109 = ATTR(next,decs);
       SYSDESTROY(L109); 
       ;
      }
     }
    }
   }
   L1111_=INTPLUS(L61,1); 
   L61 = L1111_;
  }
 }
 after_loop1: ;
 CODE_FILE_todo = FLISTC1532120756(CODE_FILE_todo, last1);
 res = ((STR) &name1);
 {
  struct FLISTC1947519404_frame_struct other3_0;
FLISTC1947519404_frame noname2 = &other3_0;
  L41 = CODE_FILE_todo;
  noname2->self = L41;
  noname2->state = 0;
  while (1) {
   L112 = FLISTC1947519404(noname2);
   if (noname2->state == -1) {
    goto after_loop2;
   }
   next1 = L112;
   if (ATTR(next1,is_c_code)) {
    plus_self12 = res;
    plus_arg = ' ';
    str_self = plus_arg;
    create_self7 = ((STR) NULL);
    create_c = str_self;
    L114 = 1;
    L116=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L114)*sizeof(CHAR);
    L115=ZALLOC_LEAF_BIG(L116);
    if (L115==NULL) FATAL("Unable to allocate more memory");
    memset(L115,0,L116);
    ((OB)L115)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L115)->header.destroyed=0;
#endif

    L113 = ((STR) L115);
    L113->asize = L114;
    r12 = L113;
    SARR((STR)r12,0,create_c); 
    ;
    ret_val38 = r12;
    ret_val37 = ret_val38;
    s = ret_val37;
    ret_val36 = STR_ap1077157958(plus_self12, s, TRUE);
    plus_self14 = ret_val36;
    replace_suffix_s = ATTR(next1,name1);
    replace_suffix_o = CONFIG1397479570(ATTR(CODE_FILE_prog,config), ((STR) &C_EXT), 0);
    replace_suffix_r = CONFIG1397479570(ATTR(CODE_FILE_prog,config), ((STR) &OBJECT_EXT), 0);
    L118 = STR_lengthrINT(replace_suffix_s);
    L119 = STR_sizerINT(replace_suffix_o);
    L1201_=INTMINUS(L118,L119); 
    plus_self13 = STR_head_INTrSTR(replace_suffix_s, L1201_);
    plus_sarg = replace_suffix_r;
    ret_val40 = STR_ap2004550586(plus_self13, plus_sarg);
    ret_val39 = ret_val40;
    plus_sarg1 = ret_val39;
    ret_val41 = STR_ap2004550586(plus_self14, plus_sarg1);
    res = ret_val41;
   }
  }
 }
 after_loop2: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_F223990950(CODE_FILE self, SIG s, SIG abs1) {
 STR ret_val;
 STR res;
 STR name111;
 ARRAYARG L11;
 ARG sa1;
 dTP se;
 ARRAYARG L21;
 dTP abse;
 INT L31 = INT_zero;
 INT idx = INT_zero;
 CODE_FILE mang_self;
 OB mang_ob;
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 CODE_FILE mang_self1;
 OB mang_ob1;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 SIG is_iter_self;
 BOOL ret_val9 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val11;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val12;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val13;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val14;
 STR r1;
 CODE_FILE mang_self2;
 OB mang_ob2;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val16;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val17;
 CODE_FILE mang_self3;
 OB mang_ob3;
 STR ret_val18;
 CODE_FILE mang_self4;
 OB mang_ob4;
 STR ret_val19;
 INT L41 = INT_zero;
 CGEN boxed_arg_type_s;
 ARG boxed_arg_type_s1;
 STR ret_val20;
 STR res1;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val21;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val22;
 CODE_FILE mang_self6;
 OB mang_ob6;
 STR ret_val23;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val24;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val25;
 SIG is_iter_self2;
 BOOL ret_val26 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val27 = BOOL_zero;
 CODE_FILE mang_self7;
 OB mang_ob7;
 STR ret_val28;
 extern STR name1;
 extern STR S_unbox;
 extern STR void6;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 BOOL L10;
 BOOL L12;
 BOOL L131_;
 STR L14;
 INT L15;
 INT L161_;
 INT L17;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 extern STR S_frame4;
 BOOL L27;
 dTP L28;
 dTP L29;
 extern STR name24;
 extern STR S_boxed2;
 extern STR name24;
 INT L301_br;
 ARG aL301_;
 INT L321_br;
 ARG aL321_;
 INT L331_;
 ARG L34;
 ARG L35;
 ARG L36;
 INT L37;
 BOOL L38;
 dTP L39;
 dTP L40;
 extern STR name29;
 extern STR S_boxed2;
 BOOL L42;
 dMODE L43;
 dMODE L44;
 extern STR name58;
 extern STR name29;
 INT L451_;
 extern STR struct1392482001;
 extern STR name127;
 BOOL L46;
 BOOL L47;
 BOOL L481_;
 STR L49;
 INT L50;
 INT L511_;
 INT L52;
 CHAR L531_;
 CHAR L54;
 BOOL L551_;
 extern STR void12;
 extern STR S_frame_alloc1;
 res = ((STR) &name1);
 mang_self = self;
 mang_ob = ((OB) s);
 ret_val1 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob, ((OB) NULL));
 plus_self = ret_val1;
 plus_sarg = ((STR) &S_unbox);
 ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
 name111 = ret_val2;
 if ((ATTR(s,ret)==((dTP) NULL))) {
  plus_self1 = res;
  plus_sarg1 = ((STR) &void6);
  ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
  res = ret_val3;
 }
 else {
  plus_self2 = res;
  mang_self1 = self;
  mang_ob1 = ((OB) ATTR(abs1,ret));
  ret_val4 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob1, ((OB) NULL));
  plus_sarg2 = ret_val4;
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val5;
  plus_arg = ' ';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L6 = 1;
  L8=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L6)*sizeof(CHAR);
  L7=ZALLOC_LEAF_BIG(L8);
  if (L7==NULL) FATAL("Unable to allocate more memory");
  memset(L7,0,L8);
  ((OB)L7)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L7)->header.destroyed=0;
#endif

  L5 = ((STR) L7);
  L5->asize = L6;
  r = L5;
  SARR((STR)r,0,create_c); 
  ;
  ret_val8 = r;
  ret_val7 = ret_val8;
  s1 = ret_val7;
  ret_val6 = STR_ap1077157958(plus_self3, s1, TRUE);
  res = ret_val6;
 }
 is_iter_self = s;
 is_iter_self1 = ATTR(is_iter_self,name1);
 L12 = (is_iter_self1.str==((STR) NULL));
 L131_=!(L12); 
 if (L131_) {
  L14 = is_iter_self1.str;
  L15 = STR_sizerINT(is_iter_self1.str);
  L161_=INTMINUS(L15,1); 
  L17 = L161_;
  L181_=ARR((STR)L14,L17); 
  L19 = L181_;
  L201_=L19=='!'; 
  L10 = L201_;
 } else {
  L10 = FALSE;
 }
 ret_val10 = L10;
 ret_val9 = ret_val10;
 if (ret_val9) {
  plus_self4 = res;
  plus_sarg3 = name111;
  ret_val11 = STR_ap2004550586(plus_self4, plus_sarg3);
  plus_self5 = ret_val11;
  plus_arg1 = '(';
  str_self1 = plus_arg1;
  create_self1 = ((STR) NULL);
  create_c1 = str_self1;
  L23 = 1;
  L25=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L23)*sizeof(CHAR);
  L24=ZALLOC_LEAF_BIG(L25);
  if (L24==NULL) FATAL("Unable to allocate more memory");
  memset(L24,0,L25);
  ((OB)L24)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L24)->header.destroyed=0;
#endif

  L22 = ((STR) L24);
  L22->asize = L23;
  r1 = L22;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val14 = r1;
  ret_val13 = ret_val14;
  s2 = ret_val13;
  ret_val12 = STR_ap1077157958(plus_self5, s2, TRUE);
  plus_self6 = ret_val12;
  mang_self2 = self;
  mang_ob2 = ((OB) abs1);
  ret_val15 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob2, ((OB) NULL));
  plus_sarg4 = ret_val15;
  ret_val16 = STR_ap2004550586(plus_self6, plus_sarg4);
  plus_self7 = ret_val16;
  plus_sarg5 = ((STR) &S_frame4);
  ret_val17 = STR_ap2004550586(plus_self7, plus_sarg5);
  res = ret_val17;
 }
 else {
  L28 = ATTR(abs1,tp);
  if ((*dTP_is1811059018[TAG(L28)])(L28)) {
   L29 = ATTR(s,tp);
   L27 = (*dTP_is1334578382[TAG(L29)])(L29);
  } else {
   L27 = FALSE;
  }
  if (L27) {
   mang_self3 = self;
   mang_ob3 = ((OB) ATTR(s,tp));
   ret_val18 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob3, ((OB) NULL));
   res = STR_ap44172742(res, name111, ((STR) &name24), ret_val18, ((STR) &S_boxed2));
  }
  else {
   mang_self4 = self;
   mang_ob4 = ((OB) ATTR(s,tp));
   ret_val19 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob4, ((OB) NULL));
   res = STR_ap1417971546(res, name111, ((STR) &name24), ret_val19);
  }
  {
   BOOL f_L301_ = TRUE;
   BOOL f_L321_ = TRUE;
   BOOL f_L331_ = TRUE;
   /* loop index variable */
   L41 = 0;
   L11 = ATTR(s,args);
   L21 = ATTRs(abs1,args,((ARRAYARG) NULL));
   L31 = 1;
   L301_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
   L321_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
   L331_=L31-1; 
   while (1) {
    if(L41>=L301_br)  goto after_loop; 
    aL301_=ARR((ARRAYARG)L11,L41); 
    sa1 = aL301_;
    se = ATTR(sa1,tp);
    if(L41>=L321_br)  goto after_loop; 
    aL321_=ARR((ARRAYARG)L21,L41); 
    L36=aL321_;
    abse = ATTR(L36,tp);
    L331_++; 
    idx = L331_;
    L39 = abse;
    if ((*dTP_is1811059018[TAG(L39)])(L39)) {
     L40 = se;
     L38 = (*dTP_is1334578382[TAG(L40)])(L40);
    } else {
     L38 = FALSE;
    }
    if (L38) {
     boxed_arg_type_s = CODE_FILE_cgen;
     boxed_arg_type_s1 = sa1;
     mang_self5 = boxed_arg_type_s;
     mang_ob5 = ((OB) ATTR(boxed_arg_type_s1,tp));
     ret_val21 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
     plus_self8 = ret_val21;
     plus_sarg6 = ((STR) &S_boxed2);
     ret_val22 = STR_ap2004550586(plus_self8, plus_sarg6);
     res1 = ret_val22;
     L43 = ATTR(boxed_arg_type_s1,mode1);
     if ((*dMODE_814247358[TAG(L43)])(L43, ((dMODE) MODES_inout_mode))) {
      L42 = TRUE;
     } else {
      L44 = ATTR(boxed_arg_type_s1,mode1);
      L42 = (*dMODE_814247358[TAG(L44)])(L44, ((dMODE) MODES_out_mode));
     }
     if (L42) {
      res1 = STR_ap2004550586(res1, ((STR) &name58));
     }
     ret_val20 = res1;
     res = STR_ap1693864410(res, ((STR) &name29), ret_val20);
    }
    else {
     mang_self6 = self;
     mang_ob6 = ((OB) abse);
     ret_val23 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob6, ((OB) NULL));
     res = STR_ap1693864410(res, ((STR) &name29), CODE_F881092569(self, sa1, ret_val23));
    }
    L451_=INTPLUS(L41,1); 
    L41 = L451_;
   }
  }
  after_loop: ;
 }
 if (ATTR(CODE_FILE_cgen,func_tables)) {
  plus_self9 = res;
  plus_sarg7 = ((STR) &struct1392482001);
  ret_val24 = STR_ap2004550586(plus_self9, plus_sarg7);
  res = ret_val24;
 }
 plus_self10 = res;
 plus_sarg8 = ((STR) &name127);
 ret_val25 = STR_ap2004550586(plus_self10, plus_sarg8);
 res = ret_val25;
 is_iter_self2 = s;
 is_iter_self3 = ATTR(is_iter_self2,name1);
 L47 = (is_iter_self3.str==((STR) NULL));
 L481_=!(L47); 
 if (L481_) {
  L49 = is_iter_self3.str;
  L50 = STR_sizerINT(is_iter_self3.str);
  L511_=INTMINUS(L50,1); 
  L52 = L511_;
  L531_=ARR((STR)L49,L52); 
  L54 = L531_;
  L551_=L54=='!'; 
  L46 = L551_;
 } else {
  L46 = FALSE;
 }
 ret_val27 = L46;
 ret_val26 = ret_val27;
 if (ret_val26) {
  mang_self7 = self;
  mang_ob7 = ((OB) s);
  ret_val28 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob7, ((OB) NULL));
  res = STR_ap1417971546(res, ((STR) &void12), ret_val28, ((STR) &S_frame_alloc1));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_F244102140(CODE_FILE self) {
 STR ret_val;
 ARRAYSTR bstr;
 STR s = ((STR) NULL);
 FSETAM1032778315 L11;
 AM_BND1124877163 e;
 STR name111;
 ARRAYINT L21;
 INT i = INT_zero;
 INT L31 = INT_zero;
 INT num1 = INT_zero;
 ARRAYSTR L41;
 STR res;
 ARRAYSTR L51;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 CODE_FILE mang_self;
 OB mang_ob;
 STR ret_val2;
 CODE_FILE mang_self1;
 OB mang_ob1;
 STR ret_val3;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self2;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val8;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val9;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val10;
 STR r1;
 STR plus_self3;
 STR plus_sarg1;
 STR ret_val11;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val12;
 INT L61 = INT_zero;
 CODE_FILE mang_self2;
 OB mang_ob2;
 STR ret_val13;
 CODE_FILE mang_self3;
 OB mang_ob3;
 STR ret_val14;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val16;
 ARRAYSTR sort_self;
 INT L71 = INT_zero;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val17;
 ARRAYSTR L8;
 INT L9;
 OB L10;
 INT L12;
 INT L131_br;
INT i_L131_=0;
 AM_BND1124877163 L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 extern STR void6;
 STR L26;
 INT L27;
 OB L28;
 INT L29;
 extern STR S_ob1;
 INT L321_br;
 INT aL321_;
 INT L331_;
 INT L34;
 INT L35;
 BOOL L361_;
 dTP L37;
 BOOL L38;
 BOOL L391_;
 extern STR name29;
 extern STR name29;
 ARRAYARG L40;
 INT L421_;
 INT L43;
 ARG L441_;
 ARG L45;
 ARRAYARG L46;
 INT L471_;
 INT L48;
 ARG L491_;
 ARG L50;
 INT L521_;
 extern STR struct1392482001;
 extern STR name127;
 BOOL L53;
 BOOL L541_;
 INT L551_;
 INT L56;
 INT L571_;
 extern STR name1;
 INT L581_br;
 STR aL581_;
 STR L59;
 INT L601_;
 create_self = ((ARRAYSTR) NULL);
 create_n = FSETAM1190305782(ATTR(self,bnd_rout_creates));
 L9 = create_n;
 L12=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L9)*sizeof(STR);
 L10=ZALLOC_BIG(L12);
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L10)->header.destroyed=0;
#endif

 L8 = ((ARRAYSTR) L10);
 L8->asize = L9;
 ret_val1 = L8;
 bstr = ret_val1;
 {
  struct FSETAM595751729_frame_struct other1_0;
FSETAM595751729_frame noname1 = &other1_0;
  BOOL f_L131_ = TRUE;
  L11 = ATTR(self,bnd_rout_creates);
  L41 = bstr;
  noname1->self = L11;
  noname1->state = 0;
  L131_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
  i_L131_=0;
  while (1) {
   L14 = FSETAM595751729(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L14;
   mang_self = self;
   mang_ob = ((OB) e);
   ret_val2 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob, ((OB) NULL));
   name111 = ret_val2;
   L16 = ATTR(e,is_remote);
   L171_=!(L16); 
   if (L171_) {
    L18 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
    L191_=!(L18); 
    L15 = L191_;
   } else {
    L15 = FALSE;
   }
   if (L15) {
    mang_self1 = self;
    mang_ob1 = ((OB) ATTR(ATTR(e,fun),ret));
    ret_val3 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob1, ((OB) NULL));
    plus_self = ret_val3;
    plus_arg = ' ';
    str_self = plus_arg;
    create_self1 = ((STR) NULL);
    create_c = str_self;
    L22 = 1;
    L24=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L22)*sizeof(CHAR);
    L23=ZALLOC_LEAF_BIG(L24);
    if (L23==NULL) FATAL("Unable to allocate more memory");
    memset(L23,0,L24);
    ((OB)L23)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L23)->header.destroyed=0;
#endif

    L20 = ((STR) L23);
    L20->asize = L22;
    r = L20;
    SARR((STR)r,0,create_c); 
    ;
    ret_val6 = r;
    ret_val5 = ret_val6;
    s1 = ret_val5;
    ret_val4 = STR_ap1077157958(plus_self, s1, TRUE);
    s = ret_val4;
   }
   else {
    s = ((STR) &void6);
   }
   plus_self1 = s;
   plus_sarg = name111;
   ret_val7 = STR_ap2004550586(plus_self1, plus_sarg);
   plus_self2 = ret_val7;
   plus_arg1 = '(';
   str_self1 = plus_arg1;
   create_self2 = ((STR) NULL);
   create_c1 = str_self1;
   L27 = 1;
   L29=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L27)*sizeof(CHAR);
   L28=ZALLOC_LEAF_BIG(L29);
   if (L28==NULL) FATAL("Unable to allocate more memory");
   memset(L28,0,L29);
   ((OB)L28)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L28)->header.destroyed=0;
#endif

   L26 = ((STR) L28);
   L26->asize = L27;
   r1 = L26;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val10 = r1;
   ret_val9 = ret_val10;
   s2 = ret_val9;
   ret_val8 = STR_ap1077157958(plus_self2, s2, TRUE);
   plus_self3 = ret_val8;
   plus_sarg1 = name111;
   ret_val11 = STR_ap2004550586(plus_self3, plus_sarg1);
   plus_self4 = ret_val11;
   plus_sarg2 = ((STR) &S_ob1);
   ret_val12 = STR_ap2004550586(plus_self4, plus_sarg2);
   s = ret_val12;
   {
    BOOL f_L321_ = TRUE;
    BOOL f_L331_ = TRUE;
    /* loop index variable */
    L61 = 0;
    L21 = ATTR(e,unbnd_args);
    L31 = 0;
    L321_br=L21==NULL?0:ASIZE((ARRAYINT)L21); 
    L331_=L31-1; 
    while (1) {
     if(L61>=L321_br)  goto after_loop1; 
     aL321_=ARR((ARRAYINT)L21,L61); 
     i = aL321_;
     L331_++; 
     num1 = L331_;
     L361_=(i)==(0); 
     if (L361_) {
      L37 = ATTR(ATTR(e,fun),tp);
      L38 = (*dTP_is124163553[TAG(L37)])(L37);
      L391_=!(L38); 
      if (L391_) {
       mang_self2 = self;
       mang_ob2 = ((OB) ATTR(ATTR(e,fun),tp));
       ret_val13 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob2, ((OB) NULL));
       s = STR_ap1693864410(s, ((STR) &name29), ret_val13);
      }
     }
     else {
      L40 = ATTR(ATTR(e,fun),args);
      L421_=INTMINUS(i,1); 
      L43 = L421_;
      L441_=(ARG)ARR((ARRAYARG)L40,L43); 
      L45 = L441_;
      mang_self3 = self;
      L46 = ATTR(ATTR(e,fun),args);
      L471_=INTMINUS(i,1); 
      L48 = L471_;
      L491_=(ARG)ARR((ARRAYARG)L46,L48); 
      L50=L491_;
      mang_ob3 = ((OB) ATTR(L50,tp));
      ret_val14 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob3, ((OB) NULL));
      s = STR_ap1693864410(s, ((STR) &name29), CODE_F881092569(self, L45, ret_val14));
     }
     L521_=INTPLUS(L61,1); 
     L61 = L521_;
    }
   }
   after_loop1: ;
   if (ATTR(CODE_FILE_cgen,func_tables)) {
    plus_self5 = s;
    plus_sarg3 = ((STR) &struct1392482001);
    ret_val15 = STR_ap2004550586(plus_self5, plus_sarg3);
    s = ret_val15;
   }
   plus_self6 = s;
   plus_sarg4 = ((STR) &name127);
   ret_val16 = STR_ap2004550586(plus_self6, plus_sarg4);
   s = ret_val16;
   if(i_L131_>=L131_br)  goto after_loop; 
   SARR((ARRAYSTR)L41,i_L131_,(STR)s); i_L131_++;
   ;
  }
 }
 after_loop: ;
 sort_self = bstr;
 L53 = (sort_self==((ARRAYSTR) NULL));
 L541_=!(L53); 
 if (L541_) {
  L551_=ASIZE((ARRAYSTR)sort_self); 
  L56 = L551_;
  L571_=INTMINUS(L56,1); 
  ARRAYS1450857938(sort_self, 0, L571_);
 }
 res = ((STR) &name1);
 {
  BOOL f_L581_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L51 = bstr;
  L581_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
  while (1) {
   plus_self7 = res;
   if(L71>=L581_br)  goto after_loop2; 
   aL581_=ARR((ARRAYSTR)L51,L71); 
   plus_sarg5 = aL581_;
   ret_val17 = STR_ap2004550586(plus_self7, plus_sarg5);
   res = ret_val17;
   L601_=INTPLUS(L71,1); 
   L71 = L601_;
  }
 }
 after_loop2: ;
 SYSDESTROY(bstr); 
 ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_F44077436(CODE_FILE self) {
 STR ret_val;
 ARRAYSTR bstr;
 STR s = ((STR) NULL);
 FLISTA1893461511 L11;
 AM_BND1124877163 e;
 STR name111;
 ARRAYSTR L21;
 STR res;
 ARRAYSTR L31;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 CODE_FILE mang_self;
 OB mang_ob;
 STR ret_val2;
 CODE_FILE mang_self1;
 OB mang_ob1;
 STR ret_val3;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val8;
 ARRAYSTR sort_self;
 INT L41 = INT_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val10;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
INT i_L91_=0;
 AM_BND1124877163 L10;
 BOOL L12;
 BOOL L131_;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 extern STR void6;
 extern STR S_call_function1;
 extern STR name24;
 extern STR S_iter_ob2;
 extern STR struct1392482001;
 extern STR name127;
 BOOL L19;
 BOOL L201_;
 INT L221_;
 INT L23;
 INT L241_;
 extern STR name1;
 INT L251_br;
 STR aL251_;
 STR L26;
 extern STR name9;
 INT L271_;
 create_self = ((ARRAYSTR) NULL);
 create_n = FLISTA919540447(ATTR(self,bnd_iter_creates));
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
 bstr = ret_val1;
 {
  struct FLISTA273121893_frame_struct other1_0;
FLISTA273121893_frame noname1 = &other1_0;
  BOOL f_L91_ = TRUE;
  L11 = ATTR(self,bnd_iter_creates);
  L21 = bstr;
  noname1->self = L11;
  noname1->state = 0;
  L91_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  i_L91_=0;
  while (1) {
   L10 = FLISTA273121893(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L10;
   mang_self = self;
   mang_ob = ((OB) e);
   ret_val2 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob, ((OB) NULL));
   name111 = ret_val2;
   L12 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
   L131_=!(L12); 
   if (L131_) {
    mang_self1 = self;
    mang_ob1 = ((OB) ATTR(ATTR(e,fun),ret));
    ret_val3 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), mang_ob1, ((OB) NULL));
    plus_self = ret_val3;
    plus_arg = ' ';
    str_self = plus_arg;
    create_self1 = ((STR) NULL);
    create_c = str_self;
    L15 = 1;
    L17=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L15)*sizeof(CHAR);
    L16=ZALLOC_LEAF_BIG(L17);
    if (L16==NULL) FATAL("Unable to allocate more memory");
    memset(L16,0,L17);
    ((OB)L16)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L16)->header.destroyed=0;
#endif

    L14 = ((STR) L16);
    L14->asize = L15;
    r = L14;
    SARR((STR)r,0,create_c); 
    ;
    ret_val6 = r;
    ret_val5 = ret_val6;
    s1 = ret_val5;
    ret_val4 = STR_ap1077157958(plus_self, s1, TRUE);
    s = ret_val4;
   }
   else {
    s = ((STR) &void6);
   }
   s = STR_ap1925291910(s, name111, ((STR) &S_call_function1), ((STR) &name24), name111, ((STR) &S_iter_ob2));
   if (ATTR(CODE_FILE_cgen,func_tables)) {
    plus_self1 = s;
    plus_sarg = ((STR) &struct1392482001);
    ret_val7 = STR_ap2004550586(plus_self1, plus_sarg);
    s = ret_val7;
   }
   plus_self2 = s;
   plus_sarg1 = ((STR) &name127);
   ret_val8 = STR_ap2004550586(plus_self2, plus_sarg1);
   s = ret_val8;
   if(i_L91_>=L91_br)  goto after_loop; 
   SARR((ARRAYSTR)L21,i_L91_,(STR)s); i_L91_++;
   ;
  }
 }
 after_loop: ;
 sort_self = bstr;
 L19 = (sort_self==((ARRAYSTR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L221_=ASIZE((ARRAYSTR)sort_self); 
  L23 = L221_;
  L241_=INTMINUS(L23,1); 
  ARRAYS1450857938(sort_self, 0, L241_);
 }
 res = ((STR) &name1);
 {
  BOOL f_L251_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L31 = bstr;
  L251_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
  while (1) {
   plus_self3 = res;
   if(L41>=L251_br)  goto after_loop1; 
   aL251_=ARR((ARRAYSTR)L31,L41); 
   plus_sarg2 = aL251_;
   ret_val9 = STR_ap2004550586(plus_self3, plus_sarg2);
   plus_self4 = ret_val9;
   plus_sarg3 = ((STR) &name9);
   ret_val10 = STR_ap2004550586(plus_self4, plus_sarg3);
   res = ret_val10;
   L271_=INTPLUS(L41,1); 
   L41 = L271_;
  }
 }
 after_loop1: ;
 SYSDESTROY(bstr); 
 ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CODE_F881092569(CODE_FILE self, ARG e, STR s) {
 STR ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 BOOL L1;
 BOOL L2;
 dMODE L3;
 dMODE L4;
 dTP L5;
 INT L6;
 INT L7;
 BOOL L81_;
 extern STR name58;
 L3 = ATTR(e,mode1);
 if ((*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_out_mode))) {
  L2 = TRUE;
 } else {
  L4 = ATTR(e,mode1);
  L2 = (*dMODE_814247358[TAG(L4)])(L4, ((dMODE) MODES_inout_mode));
 }
 if (L2) {
  L1 = TRUE;
 } else {
  L5 = ATTR(e,tp);
  L6 = (*dTP_kindrINT[TAG(L5)])(L5);
  L7 = TP_KIN462481068;
  L81_=(L6)==(L7); 
  L1 = L81_;
 }
 if (L1) {
  plus_self = s;
  plus_sarg = ((STR) &name58);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val = ret_val1;
  return ret_val;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG1221082662(CONFIG_DEF self) {
 STR ret_val;
 STR r;
 BOOL first1 = BOOL_zero;
 ARRAYARRAYSTR L11;
 ARRAYSTR l;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 INT L41 = INT_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val3;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val4;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val5;
 STR r1;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val7;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val8;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val9;
 STR r2;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val11;
 extern STR name11;
 INT L51_br;
 ARRAYSTR aL51_;
 ARRAYSTR L6;
 BOOL L71_;
 extern STR name12;
 STR L8;
 INT L9;
 OB L10;
 INT L12;
 STR L141_;
 INT L151_;
 INT L16;
 BOOL L171_;
 STR L19;
 INT L20;
 OB L22;
 INT L23;
 INT L25;
 INT L261_;
 INT L27;
 INT L281_;
 INT L181_,L181_m;
 INT L29;
 STR L301_;
 INT L321_;
 extern STR name13;
 plus_self = ATTR(self,name1);
 plus_sarg = ((STR) &name11);
 ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
 r = ret_val1;
 first1 = TRUE;
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ATTR(self,expr);
  L51_br=L11==NULL?0:ASIZE((ARRAYARRAYSTR)L11); 
  while (1) {
   if(L41>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARRAYSTR)L11,L41); 
   l = aL51_;
   L71_=!(first1); 
   if (L71_) {
    plus_self1 = r;
    plus_sarg1 = ((STR) &name12);
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    r = ret_val2;
   }
   else {
    first1 = FALSE;
   }
   plus_self2 = r;
   plus_arg = '\t';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L9 = 1;
   L12=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
   L10=ZALLOC_LEAF_BIG(L12);
   if (L10==NULL) FATAL("Unable to allocate more memory");
   memset(L10,0,L12);
   ((OB)L10)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L10)->header.destroyed=0;
#endif

   L8 = ((STR) L10);
   L8->asize = L9;
   r1 = L8;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val5 = r1;
   ret_val4 = ret_val5;
   s = ret_val4;
   ret_val3 = STR_ap1077157958(plus_self2, s, TRUE);
   plus_self3 = ret_val3;
   L141_=(STR)ARR((ARRAYSTR)l,0); 
   plus_sarg2 = STR_prettyrSTR(L141_);
   ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
   r = ret_val6;
   L151_=(l)==NULL?0:ASIZE((ARRAYSTR)l); 
   L16 = L151_;
   L171_=(1)<(L16); 
   if (L171_) {
    {
     BOOL f_L181_ = TRUE;
     while (1) {
      plus_self4 = r;
      plus_arg1 = ' ';
      str_self1 = plus_arg1;
      create_self1 = ((STR) NULL);
      create_c1 = str_self1;
      L20 = 1;
      L23=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
      L22=ZALLOC_LEAF_BIG(L23);
      if (L22==NULL) FATAL("Unable to allocate more memory");
      memset(L22,0,L23);
      ((OB)L22)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L22)->header.destroyed=0;
#endif

      L19 = ((STR) L22);
      L19->asize = L20;
      r2 = L19;
      SARR((STR)r2,0,create_c1); 
      ;
      ret_val9 = r2;
      ret_val8 = ret_val9;
      s1 = ret_val8;
      ret_val7 = STR_ap1077157958(plus_self4, s1, TRUE);
      plus_self5 = ret_val7;
      if (f_L181_) {
       f_L181_ = FALSE;
       L21 = 1;
       L261_=(l)==NULL?0:ASIZE((ARRAYSTR)l); 
       L27 = L261_;
       L281_=INTMINUS(L27,1); 
       L31 = L281_;
       L181_=L21-1;L181_m=L31; 
      }
      if(L181_++>=L181_m)  goto after_loop1; 
      L29 = L181_;
      L301_=(STR)ARR((ARRAYSTR)l,L29); 
      plus_sarg3 = STR_prettyrSTR(L301_);
      ret_val10 = STR_ap2004550586(plus_self5, plus_sarg3);
      r = ret_val10;
     }
    }
    after_loop1: ;
   }
   L321_=INTPLUS(L41,1); 
   L41 = L321_;
  }
 }
 after_loop: ;
 plus_self6 = r;
 plus_sarg4 = ((STR) &name13);
 ret_val11 = STR_ap2004550586(plus_self6, plus_sarg4);
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CONFIG811280218(CONFIG_DEF self) {
 return ATTR(self,name1);
}


#undef IS_ITER
#define IS_ITER 0

dINLINE_ITER_SIG ELT_NI1331652151(ELT_NI1131308118 self) {
 dINLINE_ITER_SIG ret_val;
 dINLINE_ITER_SIG e = ((dINLINE_ITER_SIG) NULL);
 dNIL res;
 if (e==NULL) {
  goto other2;
 } else
 switch (TAG(e)) {
  default: ;
  other2: ;
   ret_val = ((dINLINE_ITER_SIG) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dINLINE_ROUT_SIG ELT_NI706507177(ELT_NI52152274 self) {
 dINLINE_ROUT_SIG ret_val;
 dINLINE_ROUT_SIG e = ((dINLINE_ROUT_SIG) NULL);
 dNIL res;
 if (e==NULL) {
  goto other1;
 } else
 switch (TAG(e)) {
  default: ;
  other1: ;
   ret_val = ((dINLINE_ROUT_SIG) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dLAYOUT ELT_NI947803854(ELT_NILdLAYOUT self) {
 dLAYOUT ret_val;
 dLAYOUT e = ((dLAYOUT) NULL);
 dNIL res;
 if (e==NULL) {
  goto other357;
 } else
 switch (TAG(e)) {
  default: ;
  other357: ;
   ret_val = ((dLAYOUT) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP ELT_NI1328289538(ELT_NILdTP self) {
 dTP ret_val;
 dTP e = ((dTP) NULL);
 dNIL res;
 if (e==NULL) {
  goto other108;
 } else
 switch (TAG(e)) {
  default: ;
  other108: ;
   ret_val = ((dTP) NULL);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F1291349628(CODE_FILE self, SIG s) {
 ARRAYARG L11;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CODE_FILE uses_tp_self1;
 dTP uses_tp_t1;
 INT L21 = INT_zero;
 CODE_FILE uses_tp_self2;
 dTP uses_tp_t2;
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CODE_FILE uses_iter_self;
 SIG uses_iter_s;
 BOOL L3;
 BOOL L41_;
 INT L51_br;
 ARG aL51_;
 ARG L6;
 ARG L7;
 INT L81_;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 BOOL L131_;
 STR L14;
 INT L15;
 INT L161_;
 INT L17;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 dTP L22;
 SATTR(self,decs,FSETSI671175003(ATTR(self,decs), s));
 uses_tp_self = self;
 uses_tp_t = ATTR(s,tp);
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 L3 = (ATTR(s,ret)==((dTP) NULL));
 L41_=!(L3); 
 if (L41_) {
  uses_tp_self1 = self;
  uses_tp_t1 = ATTR(s,ret);
  CODE_F525364269(uses_tp_self1, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t1));
 }
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(s,args);
  L51_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   uses_tp_self2 = self;
   if(L21>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARG)L11,L21); 
   L7=aL51_;
   uses_tp_t2 = ATTR(L7,tp);
   CODE_F525364269(uses_tp_self2, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t2));
   L81_=INTPLUS(L21,1); 
   L21 = L81_;
  }
 }
 after_loop: ;
 is_iter_self = s;
 is_iter_self1 = ATTR(is_iter_self,name1);
 L12 = (is_iter_self1.str==((STR) NULL));
 L131_=!(L12); 
 if (L131_) {
  L14 = is_iter_self1.str;
  L15 = STR_sizerINT(is_iter_self1.str);
  L161_=INTMINUS(L15,1); 
  L17 = L161_;
  L181_=ARR((STR)L14,L17); 
  L19 = L181_;
  L201_=L19=='!'; 
  L10 = L201_;
 } else {
  L10 = FALSE;
 }
 ret_val1 = L10;
 ret_val = ret_val1;
 if (ret_val) {
  L22 = ATTR(s,tp);
  L9 = (*dTP_is1811059018[TAG(L22)])(L22);
 } else {
  L9 = FALSE;
 }
 if (L9) {
  uses_iter_self = self;
  uses_iter_s = s;
  SATTR(uses_iter_self,iters,FSETSI671175003(ATTR(uses_iter_self,iters), uses_iter_s));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F1479352172(CODE_FILE self, PROG p, STR s) {
 dBACK_END cg;
 FILE1 th;
 STR fn;
 CHAR c = CHAR_zero;
 FMAPSI518162669 create_self;
 FMAPSI518162669 ret_val;
 FILE1 create_directory;
 STR create_directory1;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 FILE1 open_for_read_se;
 STR open_for_read_nm;
 FILE1 ret_val5;
 FILE1 r1;
 FILE1 error_self;
 BOOL ret_val6 = BOOL_zero;
 FILE1 get_char_self;
 CHAR ret_val7 = CHAR_zero;
 FILE1 eof_self;
 BOOL ret_val8 = BOOL_zero;
 FILE1 error_self1;
 BOOL ret_val9 = BOOL_zero;
 FILE1 error_self2;
 BOOL ret_val10 = BOOL_zero;
 STR plus_self2;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val11;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val12;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val13;
 STR r2;
 FILE1 get_char_self1;
 CHAR ret_val14 = CHAR_zero;
 FILE1 get_str_self;
 STR ret_val15;
 FSTR str_self2;
 STR ret_val16;
 STR L1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 extern STR THUMBPRINTS;
 FILE1 L7;
 OB L8;
 extern STR r211;
 STR L9;
 STR L10;
 BOOL L11;
 BOOL L12;
 BOOL L131_;
 extern STR name1;
 BOOL L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 STR L19;
 INT L20;
 OB L21;
 INT L22;
 CODE_FILE_prog = p;
 cg = ATTR(CODE_FILE_prog,back_end);
 switch (TAG(cg)) {
  case CGEN_tag:
   CODE_FILE_cgen = ((CGEN) cg); break;
  default: ;
   FATAL("No applicable type in typecase\nin CODE_FILE::set_directory(PROG,STR)\n./Back/code_file.sa:106:14");
 }
 CODE_FILE_dir = s;
 create_self = ((FMAPSI518162669) NULL);
 ret_val = ((FMAPSI518162669) NULL);
 CODE_F508943357 = ret_val;
 create_directory = ((FILE1) NULL);
 create_directory1 = CODE_FILE_dir;
 L1 = create_directory1;
 mkdir(((L1==NULL)?NULL:L1->arr_part), 511);
 open_for_read_se = ((FILE1) NULL);
 plus_self = s;
 plus_arg = '/';
 str_self = plus_arg;
 create_self1 = ((STR) NULL);
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
 r = L2;
 SARR((STR)r,0,create_c); 
 ;
 ret_val3 = r;
 ret_val2 = ret_val3;
 s1 = ret_val2;
 ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
 plus_self1 = ret_val1;
 plus_sarg = ((STR) &THUMBPRINTS);
 ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
 open_for_read_nm = ret_val4;
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 L9 = open_for_read_nm;
 L10 = ((STR) &r211);
 SATTR(r1,fp,fopen(((L9==NULL)?NULL:L9->arr_part), ((L10==NULL)?NULL:L10->arr_part)));
 ret_val5 = r1;
 th = ret_val5;
 error_self = th;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L11 = TRUE;
 } else {
  L11 = ferror(ATTR(error_self,fp));
 }
 ret_val6 = L11;
 L12 = ret_val6;
 L131_=!(L12); 
 if (L131_) {
  while (1) {
   fn = ((STR) &name1);
   get_char_self = th;
   ret_val7 = fgetc(ATTR(get_char_self,fp));
   c = ret_val7;
   eof_self = th;
   ret_val8 = feof(ATTR(eof_self,fp));
   if (ret_val8) {
    L14 = TRUE;
   } else {
    error_self1 = th;
    if ((ATTR(error_self1,fp)==((EXT_OB) NULL))) {
     L15 = TRUE;
    } else {
     L15 = ferror(ATTR(error_self1,fp));
    }
    ret_val9 = L15;
    L14 = ret_val9;
   }
   if (L14) {
    goto after_loop;
   }
   while (1) {
    L171_=c==' '; 
    if (L171_) {
     L16 = TRUE;
    } else {
     error_self2 = th;
     if ((ATTR(error_self2,fp)==((EXT_OB) NULL))) {
      L18 = TRUE;
     } else {
      L18 = ferror(ATTR(error_self2,fp));
     }
     ret_val10 = L18;
     L16 = ret_val10;
    }
    if (L16) {
     goto after_loop1;
    }
    plus_self2 = fn;
    plus_arg1 = c;
    str_self1 = plus_arg1;
    create_self2 = ((STR) NULL);
    create_c1 = str_self1;
    L20 = 1;
    L22=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
    L21=ZALLOC_LEAF_BIG(L22);
    if (L21==NULL) FATAL("Unable to allocate more memory");
    memset(L21,0,L22);
    ((OB)L21)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L21)->header.destroyed=0;
#endif

    L19 = ((STR) L21);
    L19->asize = L20;
    r2 = L19;
    SARR((STR)r2,0,create_c1); 
    ;
    ret_val13 = r2;
    ret_val12 = ret_val13;
    s2 = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self2, s2, TRUE);
    fn = ret_val11;
    get_char_self1 = th;
    ret_val14 = fgetc(ATTR(get_char_self1,fp));
    c = ret_val14;
   }
   after_loop1: ;
   get_str_self = th;
   str_self2 = FILE_g1986509982(get_str_self);
   ret_val16 = STR_fr1097270334(((STR) NULL), str_self2);
   ret_val15 = ret_val16;
   CODE_F1403139677 = FMAPST859022540(CODE_F1403139677, fn, ret_val15);
  }
  after_loop: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F1933153953(CODE_FILE self) {
 FSETdLAYOUT done;
 FLISTdLAYOUT stilltodo;
 FLISTCODE_FILE L11;
 CODE_FILE cc;
 FSETSIG L21;
 SIG sig1;
 AM_ROUT_DEF a;
 FSETBU1446477505 L31;
 FSETEX1260947371 L41;
 FSETAB1569698808 L51;
 FMAPSI2103621588 L61;
 FSETCLASS_LAYOUT L71;
 FSETIM184436961 L81;
 FMAPAM1357596931 L91;
 FSETBO1147931784 L101;
 FMAPAM339652544 L121;
 FSETBO1548397318 L131;
 FMAPSI1121373188 L141;
 FMAPSI1349988702 L151;
 LAYOUT_ARRAY stillarr;
 FLISTdLAYOUT L161;
 LAYOUT_ARRAY L171;
 LAYOUT_ARRAY L181;
 FLISTdLAYOUT next_stilltodo;
 FLISTdLAYOUT L191;
 dLAYOUT l;
 FLISTdLAYOUT deps;
 BOOL okay = BOOL_zero;
 FLISTdLAYOUT L201;
 dLAYOUT d;
 dLAYOUT e;
 FSETdLAYOUT seen;
 dLAYOUT e2 = ((dLAYOUT) NULL);
 FLISTdLAYOUT L221;
 FSETdLAYOUT create_self;
 FSETdLAYOUT ret_val;
 FLISTdLAYOUT create_self1;
 FLISTdLAYOUT ret_val1;
 FLISTSTR create_self2;
 FLISTSTR ret_val2;
 FLISTdLAYOUT create_self3;
 FLISTdLAYOUT ret_val3;
 LAYOUT_ARRAY create_self4;
 INT create_n = INT_zero;
 LAYOUT_ARRAY ret_val4;
 INT L231 = INT_zero;
 LAYOUT_ARRAY sort_self;
 FLISTdLAYOUT create_self5;
 FLISTdLAYOUT ret_val5;
 INT L241 = INT_zero;
 FLISTdLAYOUT is_empty_self;
 BOOL ret_val6 = BOOL_zero;
 FLISTdLAYOUT create_self6;
 FLISTdLAYOUT ret_val7;
 OUT create_self7;
 OUT ret_val8;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val9;
 FILE1 r;
 OUT create_self8;
 OUT ret_val10;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val11;
 FILE1 r1;
 FLISTdLAYOUT aget_self;
 INT aget_ind = INT_zero;
 dLAYOUT ret_val12;
 FSETdLAYOUT create_self9;
 FSETdLAYOUT ret_val13;
 OUT create_self10;
 OUT ret_val14;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val15;
 FILE1 stdout_self2;
 FILE1 ret_val16;
 FILE1 r2;
 OUT plus_self3;
 dSTR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val17;
 FILE1 r3;
 OUT create_self11;
 OUT ret_val18;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val19;
 FILE1 stdout_self4;
 FILE1 ret_val20;
 FILE1 r4;
 OUT plus_self5;
 dSTR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val21;
 FILE1 r5;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self6;
 STR plus_sarg;
 STR ret_val22;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CODE_FILE L25;
 SIG L26;
 dTP L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L321_;
 BUILTIN_LAYOUT L33;
 EXTERNAL_LAYOUT L34;
 ABSTRACT_LAYOUT L35;
 ABSTRA151498155 L36;
 CLASS_LAYOUT L37;
 IMMUTA421759893 L38;
 BOUND_1677815802 L39;
 BOUND_809390774 L40;
 BOUND_1007407331 L42;
 BOUND_2140373124 L43;
 FRAME_LAYOUT L44;
 ARG_LAYOUT L45;
 LAYOUT_ARRAY L46;
 INT L47;
 OB L48;
 INT L49;
 INT L501_br;
 dLAYOUT L52;
 dLAYOUT L53;
 INT L541_;
 BOOL L55;
 BOOL L561_;
 INT L571_;
 INT L58;
 INT L591_;
 INT L601_br;
 dLAYOUT aL601_;
 dLAYOUT L62;
 INT L631_;
 INT L65;
 BOOL L661_;
 dLAYOUT L67;
 dLAYOUT L68;
 dLAYOUT L69;
 BOOL L70;
 BOOL L721_;
 FLISTSTR L73;
 dLAYOUT L74;
 INT L75;
 INT L76;
 BOOL L771_;
 extern STR Therea756358858;
 FILE1 L78;
 OB L79;
 extern STR Heresonecycle;
 FILE1 L82;
 OB L83;
 dLAYOUT L851_;
 dLAYOUT L86;
 FILE1 L87;
 OB L88;
 dSTR L90;
 OB L92;
 FILE1 L93;
 OB L94;
 dLAYOUT L96;
 dLAYOUT L97;
 BOOL L98;
 BOOL L991_;
 dLAYOUT L100;
 FILE1 L102;
 OB L103;
 dSTR L105;
 OB L106;
 FILE1 L107;
 OB L108;
 extern STR givingup;
 extern STR Intern28965746;
 create_self = ((FSETdLAYOUT) NULL);
 ret_val = ((FSETdLAYOUT) NULL);
 done = ret_val;
 create_self1 = ((FLISTdLAYOUT) NULL);
 ret_val1 = ((FLISTdLAYOUT) NULL);
 CODE_F19188927 = ret_val1;
 create_self2 = ((FLISTSTR) NULL);
 ret_val2 = ((FLISTSTR) NULL);
 CODE_F1806827531 = ret_val2;
 create_self3 = ((FLISTdLAYOUT) NULL);
 ret_val3 = ((FLISTdLAYOUT) NULL);
 stilltodo = ret_val3;
 {
  struct FLISTC1947519404_frame_struct other1_0;
FLISTC1947519404_frame noname1 = &other1_0;
  L11 = CODE_FILE_todo;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L25 = FLISTC1947519404(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   cc = L25;
   {
    struct FSETSIG_eltbrSIG_frame_struct other2_0;
FSETSIG_eltbrSIG_frame noname2 = &other2_0;
    L21 = ATTR(cc,iters);
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L26 = FSETSIG_eltbrSIG(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     sig1 = L26;
     L27 = ATTR(sig1,tp);
     L28 = (*dTP_is1811059018[TAG(L27)])(L27);
     L291_=!(L28); 
     if (L291_) {
      a = FMAPSI2016001247(CODE_F508943357, sig1);
      L30 = (a==((AM_ROUT_DEF) NULL));
      L321_=!(L30); 
      if (L321_) {
       CODE_F525364269(cc, ((dLAYOUT) FRAME_248172754(((FRAME_LAYOUT) NULL), a, CODE_FILE_prog)));
      }
     }
     else {
      CODE_F525364269(cc, ((dLAYOUT) ABSTRA1493033801(((ABSTRA151498155) NULL), sig1, CODE_FILE_prog)));
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
 {
  struct FSETBU202043707_frame_struct other3_0;
FSETBU202043707_frame noname3 = &other3_0;
  L31 = BUILTI898649695;
  noname3->self = L31;
  noname3->state = 0;
  while (1) {
   L33 = FSETBU202043707(noname3);
   if (noname3->state == -1) {
    goto after_loop2;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L33));
  }
 }
 after_loop2: ;
 {
  struct FSETEX1633103446_frame_struct other4_0;
FSETEX1633103446_frame noname4 = &other4_0;
  L41 = EXTERN1555805875;
  noname4->self = L41;
  noname4->state = 0;
  while (1) {
   L34 = FSETEX1633103446(noname4);
   if (noname4->state == -1) {
    goto after_loop3;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L34));
  }
 }
 after_loop3: ;
 {
  struct FSETAB1942662686_frame_struct other5_0;
FSETAB1942662686_frame noname5 = &other5_0;
  L51 = ABSTRA8148400;
  noname5->self = L51;
  noname5->state = 0;
  while (1) {
   L35 = FSETAB1942662686(noname5);
   if (noname5->state == -1) {
    goto after_loop4;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L35));
  }
 }
 after_loop4: ;
 {
  struct FMAPSI140286185_frame_struct other6_0;
FMAPSI140286185_frame noname6 = &other6_0;
  L61 = ABSTRA854284101;
  noname6->self = L61;
  noname6->state = 0;
  while (1) {
   L36 = FMAPSI140286185(noname6);
   if (noname6->state == -1) {
    goto after_loop5;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L36));
  }
 }
 after_loop5: ;
 {
  struct FSETCL1233705521_frame_struct other7_0;
FSETCL1233705521_frame noname7 = &other7_0;
  L71 = CLASS_368803185;
  noname7->self = L71;
  noname7->state = 0;
  while (1) {
   L37 = FSETCL1233705521(noname7);
   if (noname7->state == -1) {
    goto after_loop6;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L37));
  }
 }
 after_loop6: ;
 {
  struct FSETIM887960645_frame_struct other8_0;
FSETIM887960645_frame noname8 = &other8_0;
  L81 = IMMUTA489824193;
  noname8->self = L81;
  noname8->state = 0;
  while (1) {
   L38 = FSETIM887960645(noname8);
   if (noname8->state == -1) {
    goto after_loop7;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L38));
  }
 }
 after_loop7: ;
 {
  struct FMAPAM1626870716_frame_struct other9_0;
FMAPAM1626870716_frame noname9 = &other9_0;
  L91 = BOUND_2051717332;
  noname9->self = L91;
  noname9->state = 0;
  while (1) {
   L39 = FMAPAM1626870716(noname9);
   if (noname9->state == -1) {
    goto after_loop8;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L39));
  }
 }
 after_loop8: ;
 {
  struct FSETBO10998148_frame_struct other10_0;
FSETBO10998148_frame noname10 = &other10_0;
  L101 = BOUND_2075179152;
  noname10->self = L101;
  noname10->state = 0;
  while (1) {
   L40 = FSETBO10998148(noname10);
   if (noname10->state == -1) {
    goto after_loop9;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L40));
  }
 }
 after_loop9: ;
 {
  struct FMAPAM1324815836_frame_struct other11_0;
FMAPAM1324815836_frame noname11 = &other11_0;
  L121 = BOUND_982289559;
  noname11->self = L121;
  noname11->state = 0;
  while (1) {
   L42 = FMAPAM1324815836(noname11);
   if (noname11->state == -1) {
    goto after_loop10;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L42));
  }
 }
 after_loop10: ;
 {
  struct FSETBO677183779_frame_struct other12_0;
FSETBO677183779_frame noname12 = &other12_0;
  L131 = BOUND_210358778;
  noname12->self = L131;
  noname12->state = 0;
  while (1) {
   L43 = FSETBO677183779(noname12);
   if (noname12->state == -1) {
    goto after_loop11;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L43));
  }
 }
 after_loop11: ;
 {
  struct FMAPSI1790064862_frame_struct other13_0;
FMAPSI1790064862_frame noname13 = &other13_0;
  L141 = FRAME_1643038396;
  noname13->self = L141;
  noname13->state = 0;
  while (1) {
   L44 = FMAPSI1790064862(noname13);
   if (noname13->state == -1) {
    goto after_loop12;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L44));
  }
 }
 after_loop12: ;
 {
  struct FMAPSI688594122_frame_struct other14_0;
FMAPSI688594122_frame noname14 = &other14_0;
  L151 = ARG_LA2092120054;
  noname14->self = L151;
  noname14->state = 0;
  while (1) {
   L45 = FMAPSI688594122(noname14);
   if (noname14->state == -1) {
    goto after_loop13;
   }
   stilltodo = FLISTd1429133189(stilltodo, ((dLAYOUT) L45));
  }
 }
 after_loop13: ;
 create_self4 = ((LAYOUT_ARRAY) NULL);
 create_n = FLISTd102354833(stilltodo);
 L47 = create_n;
 L49=(sizeof(struct LAYOUT_ARRAY_struct)-sizeof(dLAYOUT))+(L47)*sizeof(dLAYOUT);
 L48=ZALLOC_BIG(L49);
 if (L48==NULL) FATAL("Unable to allocate more memory");
 ((OB)L48)->header.tag=LAYOUT_ARRAY_tag;
#ifdef DESTROY_CHK

   ((OB)L48)->header.destroyed=0;
#endif

 L46 = ((LAYOUT_ARRAY) L48);
 L46->asize = L47;
 ret_val4 = L46;
 stillarr = ret_val4;
 {
  struct FLISTd1954760550_frame_struct other15_0;
FLISTd1954760550_frame noname15 = &other15_0;
  BOOL f_L501_ = TRUE;
  L161 = stilltodo;
  /* loop index variable */
  L231 = 0;
  L171 = stillarr;
  noname15->self = L161;
  noname15->state = 0;
  L501_br=L171==NULL?0:ASIZE((LAYOUT_ARRAY)L171); 
  while (1) {
   if(L231>=L501_br)  goto after_loop14; 
   L52 = FLISTd1954760550(noname15);
   if (noname15->state == -1) {
    goto after_loop14;
   }
   L53 = L52;
   SARR((LAYOUT_ARRAY)L171,L231,(dLAYOUT)L53); 
   ;
   L541_=INTPLUS(L231,1); 
   L231 = L541_;
  }
 }
 after_loop14: ;
 sort_self = stillarr;
 L55 = (sort_self==((LAYOUT_ARRAY) NULL));
 L561_=!(L55); 
 if (L561_) {
  L571_=ASIZE((LAYOUT_ARRAY)sort_self); 
  L58 = L571_;
  L591_=INTMINUS(L58,1); 
  LAYOUT2061379740(sort_self, 0, L591_);
 }
 create_self5 = ((FLISTdLAYOUT) NULL);
 ret_val5 = ((FLISTdLAYOUT) NULL);
 stilltodo = ret_val5;
 {
  BOOL f_L601_ = TRUE;
  /* loop index variable */
  L241 = 0;
  L181 = stillarr;
  L601_br=L181==NULL?0:ASIZE((LAYOUT_ARRAY)L181); 
  while (1) {
   if(L241>=L601_br)  goto after_loop15; 
   aL601_=ARR((LAYOUT_ARRAY)L181,L241); 
   stilltodo = FLISTd1429133189(stilltodo, aL601_);
   L631_=INTPLUS(L241,1); 
   L241 = L631_;
  }
 }
 after_loop15: ;
 SYSDESTROY(stillarr); 
 ;
 while (1) {
  is_empty_self = stilltodo;
  L65 = FLISTd102354833(is_empty_self);
  L661_=(L65)==(0); 
  ret_val6 = L661_;
  if (ret_val6) {
   goto after_loop16;
  }
  create_self6 = ((FLISTdLAYOUT) NULL);
  ret_val7 = ((FLISTdLAYOUT) NULL);
  next_stilltodo = ret_val7;
  {
   struct FLISTd1954760550_frame_struct other18_0;
FLISTd1954760550_frame noname16 = &other18_0;
   L191 = stilltodo;
   noname16->self = L191;
   noname16->state = 0;
   while (1) {
    L67 = FLISTd1954760550(noname16);
    if (noname16->state == -1) {
     goto after_loop17;
    }
    l = L67;
    L68 = l;
    deps = (*dLAYOU1845593458[TAG(L68)])(L68);
    okay = TRUE;
    {
     struct FLISTd1954760550_frame_struct other19_0;
FLISTd1954760550_frame noname17 = &other19_0;
     L201 = deps;
     noname17->self = L201;
     noname17->state = 0;
     while (1) {
      if (okay) {
      }
      else {
       goto after_loop18;
      }
      L69 = FLISTd1954760550(noname17);
      if (noname17->state == -1) {
       goto after_loop18;
      }
      d = L69;
      L70 = FSETdL1260128492(done, d);
      L721_=!(L70); 
      if (L721_) {
       okay = FALSE;
      }
     }
    }
    after_loop18: ;
    if (okay) {
     CODE_F19188927 = FLISTd1429133189(CODE_F19188927, l);
     L73 = CODE_F1806827531;
     L74 = l;
     CODE_F1806827531 = FLISTS1425610062(L73, (*dLAYOU959283167[TAG(L74)])(L74, CODE_FILE_cgen));
     done = FSETdL835744292(done, l);
    }
    else {
     next_stilltodo = FLISTd1429133189(next_stilltodo, l);
    }
   }
  }
  after_loop17: ;
  L75 = FLISTd102354833(next_stilltodo);
  L76 = FLISTd102354833(stilltodo);
  L771_=(L75)==(L76); 
  if (L771_) {
   create_self7 = ((OUT) NULL);
   ret_val8 = create_self7;
   plus_self = ret_val8;
   plus_s = ((STR) &Therea756358858);
   stdout_self = ((FILE1) NULL);
   L79=ZALLOC(sizeof(struct FILE1_struct));
   if (L79==NULL) FATAL("Unable to allocate more memory");
   ((OB)L79)->header.tag=FILE1_tag;
   L78 = ((FILE1) L79);
   r = L78;
   
   SATTR(r,fp,stdout);
   ret_val9 = r;
   FILE_plus_STR(ret_val9, plus_s);
   create_self8 = ((OUT) NULL);
   ret_val10 = create_self8;
   plus_self1 = ret_val10;
   plus_s1 = ((STR) &Heresonecycle);
   stdout_self1 = ((FILE1) NULL);
   L83=ZALLOC(sizeof(struct FILE1_struct));
   if (L83==NULL) FATAL("Unable to allocate more memory");
   ((OB)L83)->header.tag=FILE1_tag;
   L82 = ((FILE1) L83);
   r1 = L82;
   
   SATTR(r1,fp,stdout);
   ret_val11 = r1;
   FILE_plus_STR(ret_val11, plus_s1);
   aget_self = stilltodo;
   aget_ind = 0;
   L851_=(dLAYOUT)ARR((FLISTdLAYOUT)aget_self,aget_ind); 
   ret_val12 = L851_;
   e = ret_val12;
   create_self9 = ((FSETdLAYOUT) NULL);
   ret_val13 = ((FSETdLAYOUT) NULL);
   seen = ret_val13;
   while (1) {
    create_self10 = ((OUT) NULL);
    ret_val14 = create_self10;
    plus_self2 = ret_val14;
    L86 = e;
    plus_s2 = (*dLAYOUT_strrSTR[TAG(L86)])(L86);
    stdout_self2 = ((FILE1) NULL);
    L88=ZALLOC(sizeof(struct FILE1_struct));
    if (L88==NULL) FATAL("Unable to allocate more memory");
    ((OB)L88)->header.tag=FILE1_tag;
    L87 = ((FILE1) L88);
    r2 = L87;
    
    SATTR(r2,fp,stdout);
    ret_val16 = r2;
    FILE_plus_STR(ret_val16, plus_s2);
    ret_val15 = plus_self2;
    plus_self3 = ret_val15;
    L92=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L92==NULL) FATAL("Unable to allocate more memory");
    memset(L92,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L92)->header.tag=CHAR_tag;
    L90 = (dSTR)((CHAR_boxed) L92);
    ((CHAR_boxed) L90)->immutable_part = '\n';
    plus_s3 = L90;
    stdout_self3 = ((FILE1) NULL);
    L94=ZALLOC(sizeof(struct FILE1_struct));
    if (L94==NULL) FATAL("Unable to allocate more memory");
    ((OB)L94)->header.tag=FILE1_tag;
    L93 = ((FILE1) L94);
    r3 = L93;
    
    SATTR(r3,fp,stdout);
    ret_val17 = r3;
    FILE_plus_dSTR(ret_val17, plus_s3);
    seen = FSETdL835744292(seen, e);
    {
     struct FLISTd1954760550_frame_struct other21_0;
FLISTd1954760550_frame noname18 = &other21_0;
     noname18->state = 0;
     while (1) {
      if (noname18->state == 0) {
       L97 = e;
       L221 = (*dLAYOU1845593458[TAG(L97)])(L97);
       noname18->self = L221;
      }
      L96 = FLISTd1954760550(noname18);
      if (noname18->state == -1) {
       goto after_loop20;
      }
      e2 = L96;
      L98 = FSETdL1260128492(done, e2);
      L991_=!(L98); 
      if (L991_) {
       goto after_loop20;
      }
     }
    }
    after_loop20: ;
    e = e2;
    if (FSETdL1260128492(seen, e)) {
     goto after_loop19;
    }
   }
   after_loop19: ;
   create_self11 = ((OUT) NULL);
   ret_val18 = create_self11;
   plus_self4 = ret_val18;
   L100 = e;
   plus_s4 = (*dLAYOUT_strrSTR[TAG(L100)])(L100);
   stdout_self4 = ((FILE1) NULL);
   L103=ZALLOC(sizeof(struct FILE1_struct));
   if (L103==NULL) FATAL("Unable to allocate more memory");
   ((OB)L103)->header.tag=FILE1_tag;
   L102 = ((FILE1) L103);
   r4 = L102;
   
   SATTR(r4,fp,stdout);
   ret_val20 = r4;
   FILE_plus_STR(ret_val20, plus_s4);
   ret_val19 = plus_self4;
   plus_self5 = ret_val19;
   L106=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L106==NULL) FATAL("Unable to allocate more memory");
   memset(L106,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L106)->header.tag=CHAR_tag;
   L105 = (dSTR)((CHAR_boxed) L106);
   ((CHAR_boxed) L105)->immutable_part = '\n';
   plus_s5 = L105;
   stdout_self5 = ((FILE1) NULL);
   L108=ZALLOC(sizeof(struct FILE1_struct));
   if (L108==NULL) FATAL("Unable to allocate more memory");
   ((OB)L108)->header.tag=FILE1_tag;
   L107 = ((FILE1) L108);
   r5 = L107;
   
   SATTR(r5,fp,stdout);
   ret_val21 = r5;
   FILE_plus_dSTR(ret_val21, plus_s5);
   barf_self = CODE_FILE_cgen;
   barf_msg = ((STR) &givingup);
   barf_self1 = ATTR(barf_self,prog);
   barf_msg1 = barf_msg;
   barf_at_self = barf_self1;
   barf_at_msg = barf_msg1;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self6 = ((STR) &Intern28965746);
   plus_sarg = barf_at_msg;
   ret_val22 = STR_ap2004550586(plus_self6, plus_sarg);
   PROG_err_STR(barf_at_self, ret_val22);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
  stilltodo = next_stilltodo;
 }
 after_loop16: ;
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F2105979088(CODE_FILE self) {
 FSETSTR to_delete;
 FMAPSTRSTR L11;
 INT total1 = INT_zero;
 INT regen = INT_zero;
 FILE1 th = ((FILE1) NULL);
 FLISTCODE_FILE L21;
 CODE_FILE cc;
 STR oldthp;
 FSTR wholetext;
 FSTR header1;
 ARRAYFSTR textarr;
 FLISTFSTR L31;
 ARRAYFSTR L41;
 ARRAYFSTR L51;
 FSTR t;
 STR newthp;
 FILE1 f;
 FSETSTR L61;
 STR name111;
 FSETSTR create_self;
 FSETSTR ret_val;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 FILE1 open_for_write_s;
 STR open_for_write_n;
 FILE1 ret_val5;
 FILE1 r1;
 FILE1 error_self;
 BOOL ret_val6 = BOOL_zero;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val7;
 UNIX exit_self;
 INT exit_code = INT_zero;
 FSTR create_self2;
 FSTR ret_val8;
 ARRAYFSTR create_self3;
 INT create_n = INT_zero;
 ARRAYFSTR ret_val9;
 INT L71 = INT_zero;
 ARRAYFSTR sort_self;
 INT L81 = INT_zero;
 STR plus_self3;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val10;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val11;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val12;
 STR r2;
 STR plus_self4;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val13;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val14;
 STR create_self5;
 CHAR create_c2 = CHAR_zero;
 STR ret_val15;
 STR r3;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val16;
 FILE1 open_for_write_s1;
 STR open_for_write_n1;
 FILE1 ret_val17;
 FILE1 r4;
 FILE1 error_self1;
 BOOL ret_val18 = BOOL_zero;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val19;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val20;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 FILE1 plus_self8;
 FSTR plus_s;
 FILE1 ret_val21;
 FILE1 plus_self9;
 CHAR plus_c = CHAR_zero;
 FILE1 close_self;
 FILE1 plus_self10;
 STR plus_s1;
 FILE1 ret_val22;
 FILE1 plus_self11;
 CHAR plus_c1 = CHAR_zero;
 FILE1 ret_val23;
 FILE1 plus_self12;
 STR plus_s2;
 FILE1 ret_val24;
 FILE1 plus_self13;
 CHAR plus_c2 = CHAR_zero;
 FILE1 close_self1;
 STR plus_self14;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val25;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val26;
 STR create_self6;
 CHAR create_c3 = CHAR_zero;
 STR ret_val27;
 STR r5;
 STR plus_self15;
 STR plus_sarg5;
 STR ret_val28;
 FILE1 delete_self;
 STR delete_nm;
 STR replace_suffix_s;
 STR replace_suffix_o;
 STR replace_suffix_r;
 STR ret_val29;
 STR plus_self16;
 STR plus_sarg6;
 STR ret_val30;
 FILE1 delete_self1;
 STR delete_nm1;
 OUT create_self7;
 OUT ret_val31;
 OUT plus_self17;
 STR plus_s3;
 OUT ret_val32;
 FILE1 stdout_self;
 FILE1 ret_val33;
 FILE1 r6;
 OUT plus_self18;
 dSTR plus_s4;
 OUT ret_val34;
 FILE1 stdout_self1;
 FILE1 ret_val35;
 FILE1 r7;
 OUT plus_self19;
 STR plus_s5;
 OUT ret_val36;
 FILE1 stdout_self2;
 FILE1 ret_val37;
 FILE1 r8;
 OUT plus_self20;
 dSTR plus_s6;
 OUT ret_val38;
 FILE1 stdout_self3;
 FILE1 ret_val39;
 FILE1 r9;
 OUT plus_self21;
 STR plus_s7;
 OUT ret_val40;
 FILE1 stdout_self4;
 FILE1 ret_val41;
 FILE1 r10;
 OUT flush_self;
 FILE1 stdout_self5;
 FILE1 ret_val42;
 FILE1 r11;
 FILE1 flush_self1;
 STR L9;
 BOOL L10;
 BOOL L121_;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 extern STR THUMBPRINTS;
 FILE1 L18;
 OB L19;
 extern STR w1;
 STR L20;
 STR L22;
 BOOL L23;
 extern STR Couldn1439942807;
 extern STR Intern28965746;
 CODE_FILE L24;
 FSTR L25;
 INT L26;
 OB L27;
 INT L28;
 INT L30;
 INT L321_;
 ARRAYFSTR L33;
 INT L34;
 OB L35;
 INT L36;
 INT L371_br;
 FSTR L38;
 FSTR L39;
 INT L401_;
 INT L421_;
 INT L43;
 INT L441_;
 INT L45;
 FSTR L46;
 BOOL L48;
 BOOL L491_;
 INT L501_;
 INT L52;
 INT L531_;
 INT L541_br;
 FSTR aL541_;
 FSTR L55;
 INT L571_;
 BOOL L58;
 BOOL L591_;
 FLISTFSTR L60;
 BOOL L63;
 BOOL L641_;
 STR L66;
 INT L67;
 OB L68;
 INT L69;
 BOOL L72;
 BOOL L731_;
 BOOL L74;
 BOOL L751_;
 STR L76;
 INT L77;
 OB L78;
 INT L79;
 FILE1 L82;
 OB L83;
 extern STR w1;
 STR L84;
 STR L85;
 BOOL L86;
 extern STR Couldntwrite;
 extern STR Intern28965746;
 INT L871_;
 BOOL L88;
 BOOL L891_;
 INT L911_;
 BOOL L92;
 BOOL L931_;
 STR L94;
 INT L95;
 OB L96;
 INT L97;
 STR L99;
 STR L100;
 extern STR C_EXT;
 extern STR OBJECT_EXT;
 INT L101;
 INT L102;
 INT L1031_;
 STR L104;
 extern STR Regenerated;
 FILE1 L105;
 OB L106;
 dSTR L108;
 OB L109;
 FILE1 L110;
 OB L111;
 extern STR of;
 FILE1 L113;
 OB L114;
 dSTR L116;
 OB L117;
 FILE1 L118;
 OB L119;
 extern STR files1;
 FILE1 L122;
 OB L123;
 FILE1 L125;
 OB L126;
 CODE_F1933153953(self);
 create_self = ((FSETSTR) NULL);
 ret_val = ((FSETSTR) NULL);
 to_delete = ret_val;
 {
  struct FMAPST2087339362_frame_struct other1_0;
FMAPST2087339362_frame noname1 = &other1_0;
  L11 = CODE_F1403139677;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FMAPST2087339362(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   to_delete = FSETST1404644833(to_delete, L9);
  }
 }
 after_loop: ;
 total1 = 0;
 regen = 0;
 L10 = ATTR(CODE_FILE_prog,benchmark);
 L121_=!(L10); 
 if (L121_) {
  open_for_write_s = ((FILE1) NULL);
  plus_self = CODE_FILE_dir;
  plus_arg = '/';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
  create_c = str_self;
  L14 = 1;
  L16=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L14)*sizeof(CHAR);
  L15=ZALLOC_LEAF_BIG(L16);
  if (L15==NULL) FATAL("Unable to allocate more memory");
  memset(L15,0,L16);
  ((OB)L15)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L15)->header.destroyed=0;
#endif

  L13 = ((STR) L15);
  L13->asize = L14;
  r = L13;
  SARR((STR)r,0,create_c); 
  ;
  ret_val3 = r;
  ret_val2 = ret_val3;
  s = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val1;
  plus_sarg = ((STR) &THUMBPRINTS);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
  open_for_write_n = ret_val4;
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r1 = L18;
  L20 = open_for_write_n;
  L22 = ((STR) &w1);
  SATTR(r1,fp,fopen(((L20==NULL)?NULL:L20->arr_part), ((L22==NULL)?NULL:L22->arr_part)));
  ret_val5 = r1;
  th = ret_val5;
  error_self = th;
  if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
   L23 = TRUE;
  } else {
   L23 = ferror(ATTR(error_self,fp));
  }
  ret_val6 = L23;
  if (ret_val6) {
   barf_self = CODE_FILE_prog;
   barf_msg = ((STR) &Couldn1439942807);
   barf_at_self = barf_self;
   barf_at_msg = barf_msg;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self2 = ((STR) &Intern28965746);
   plus_sarg1 = barf_at_msg;
   ret_val7 = STR_ap2004550586(plus_self2, plus_sarg1);
   PROG_err_STR(barf_at_self, ret_val7);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
 }
 {
  struct FLISTC1947519404_frame_struct other2_0;
FLISTC1947519404_frame noname2 = &other2_0;
  L21 = CODE_FILE_todo;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L24 = FLISTC1947519404(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   cc = L24;
   to_delete = FSETST1872370245(to_delete, ATTR(cc,name1));
   oldthp = FMAPST1518383807(CODE_F1403139677, ATTR(cc,name1));
   create_self2 = ((FSTR) NULL);
   L26 = 16;
   L28=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L26)*sizeof(CHAR);
   L27=ZALLOC_LEAF_BIG(L28);
   if (L27==NULL) FATAL("Unable to allocate more memory");
   memset(L27,0,L28);
   ((OB)L27)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L27)->header.destroyed=0;
#endif

   L25 = ((FSTR) L27);
   L25->asize = L26;
   ret_val8 = L25;
   wholetext = ret_val8;
   if (ATTR(cc,is_c_code)) {
    header1 = CODE_F1554664505(cc);
    wholetext = FSTR_p1410513982(wholetext, header1);
    SYSDESTROY(header1); 
    ;
   }
   create_self3 = ((ARRAYFSTR) NULL);
   L30 = FLISTF1348049713(ATTR(cc,text1));
   L321_=INTPLUS(L30,1); 
   create_n = L321_;
   L34 = create_n;
   L36=(sizeof(struct ARRAYFSTR_struct)-sizeof(FSTR))+(L34)*sizeof(FSTR);
   L35=ZALLOC_BIG(L36);
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=ARRAYFSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L35)->header.destroyed=0;
#endif

   L33 = ((ARRAYFSTR) L35);
   L33->asize = L34;
   ret_val9 = L33;
   textarr = ret_val9;
   {
    struct FLISTF1277706721_frame_struct other3_0;
FLISTF1277706721_frame noname3 = &other3_0;
    BOOL f_L371_ = TRUE;
    L31 = ATTR(cc,text1);
    /* loop index variable */
    L71 = 0;
    L41 = textarr;
    noname3->self = L31;
    noname3->state = 0;
    L371_br=L41==NULL?0:ASIZE((ARRAYFSTR)L41); 
    while (1) {
     if(L71>=L371_br)  goto after_loop2; 
     L38 = FLISTF1277706721(noname3);
     if (noname3->state == -1) {
      goto after_loop2;
     }
     L39 = L38;
     SARR((ARRAYFSTR)L41,L71,(FSTR)L39); 
     ;
     L401_=INTPLUS(L71,1); 
     L71 = L401_;
    }
   }
   after_loop2: ;
   L421_=(textarr)==NULL?0:ASIZE((ARRAYFSTR)textarr); 
   L43 = L421_;
   L441_=INTMINUS(L43,1); 
   L45 = L441_;
   L46 = ATTR(cc,ntext);
   SARR((ARRAYFSTR)textarr,L45,(FSTR)L46); 
   ;
   sort_self = textarr;
   L48 = (sort_self==((ARRAYFSTR) NULL));
   L491_=!(L48); 
   if (L491_) {
    L501_=ASIZE((ARRAYFSTR)sort_self); 
    L52 = L501_;
    L531_=INTMINUS(L52,1); 
    ARRAYF1076245134(sort_self, 0, L531_);
   }
   {
    BOOL f_L541_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L51 = textarr;
    L541_br=L51==NULL?0:ASIZE((ARRAYFSTR)L51); 
    while (1) {
     if(L81>=L541_br)  goto after_loop3; 
     aL541_=ARR((ARRAYFSTR)L51,L81); 
     t = aL541_;
     wholetext = FSTR_p1410513982(wholetext, t);
     SYSDESTROY(t); 
     ;
     L571_=INTPLUS(L81,1); 
     L81 = L571_;
    }
   }
   after_loop3: ;
   L58 = (ATTR(cc,text1)==((FLISTFSTR) NULL));
   L591_=!(L58); 
   if (L591_) {
    L60 = ATTR(cc,text1);
    SYSDESTROY(L60); 
    ;
   }
   L63 = (textarr==((ARRAYFSTR) NULL));
   L641_=!(L63); 
   if (L641_) {
    SYSDESTROY(textarr); 
    ;
   }
   newthp = FSTR_t603468843(wholetext);
   plus_self3 = newthp;
   plus_arg1 = '\n';
   str_self1 = plus_arg1;
   create_self4 = ((STR) NULL);
   create_c1 = str_self1;
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
   r2 = L66;
   SARR((STR)r2,0,create_c1); 
   ;
   ret_val12 = r2;
   ret_val11 = ret_val12;
   s1 = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self3, s1, TRUE);
   L72 = STR_is111530248(oldthp, ret_val10);
   L731_=!(L72); 
   if (L731_) {
    L74 = ATTR(CODE_FILE_prog,benchmark);
    L751_=!(L74); 
    if (L751_) {
     open_for_write_s1 = ((FILE1) NULL);
     plus_self4 = CODE_FILE_dir;
     plus_arg2 = '/';
     str_self2 = plus_arg2;
     create_self5 = ((STR) NULL);
     create_c2 = str_self2;
     L77 = 1;
     L79=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L77)*sizeof(CHAR);
     L78=ZALLOC_LEAF_BIG(L79);
     if (L78==NULL) FATAL("Unable to allocate more memory");
     memset(L78,0,L79);
     ((OB)L78)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L78)->header.destroyed=0;
#endif

     L76 = ((STR) L78);
     L76->asize = L77;
     r3 = L76;
     SARR((STR)r3,0,create_c2); 
     ;
     ret_val15 = r3;
     ret_val14 = ret_val15;
     s2 = ret_val14;
     ret_val13 = STR_ap1077157958(plus_self4, s2, TRUE);
     plus_self5 = ret_val13;
     plus_sarg2 = ATTR(cc,name1);
     ret_val16 = STR_ap2004550586(plus_self5, plus_sarg2);
     open_for_write_n1 = ret_val16;
     L83=ZALLOC(sizeof(struct FILE1_struct));
     if (L83==NULL) FATAL("Unable to allocate more memory");
     ((OB)L83)->header.tag=FILE1_tag;
     L82 = ((FILE1) L83);
     r4 = L82;
     L84 = open_for_write_n1;
     L85 = ((STR) &w1);
     SATTR(r4,fp,fopen(((L84==NULL)?NULL:L84->arr_part), ((L85==NULL)?NULL:L85->arr_part)));
     ret_val17 = r4;
     f = ret_val17;
     error_self1 = f;
     if ((ATTR(error_self1,fp)==((EXT_OB) NULL))) {
      L86 = TRUE;
     } else {
      L86 = ferror(ATTR(error_self1,fp));
     }
     ret_val18 = L86;
     if (ret_val18) {
      barf_self1 = CODE_FILE_prog;
      plus_self6 = ((STR) &Couldntwrite);
      plus_sarg3 = ATTR(cc,name1);
      ret_val19 = STR_ap2004550586(plus_self6, plus_sarg3);
      barf_msg1 = ret_val19;
      barf_at_self1 = barf_self1;
      barf_at_msg1 = barf_msg1;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self7 = ((STR) &Intern28965746);
      plus_sarg4 = barf_at_msg1;
      ret_val20 = STR_ap2004550586(plus_self7, plus_sarg4);
      PROG_err_STR(barf_at_self1, ret_val20);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
     }
     plus_self8 = f;
     plus_s = wholetext;
     FILE_plus_FSTR(plus_self8, plus_s);
     ret_val21 = plus_self8;
     plus_self9 = ret_val21;
     plus_c = '\n';
     fputc(plus_c, ATTR(plus_self9,fp));
     close_self = f;
     fclose(ATTR(close_self,fp));
    }
    L871_=INTPLUS(regen,1); 
    regen = L871_;
   }
   L88 = ATTR(CODE_FILE_prog,benchmark);
   L891_=!(L88); 
   if (L891_) {
    plus_self10 = th;
    plus_s1 = ATTR(cc,name1);
    FILE_plus_STR(plus_self10, plus_s1);
    ret_val22 = plus_self10;
    plus_self11 = ret_val22;
    plus_c1 = ' ';
    fputc(plus_c1, ATTR(plus_self11,fp));
    ret_val23 = plus_self11;
    plus_self12 = ret_val23;
    plus_s2 = newthp;
    FILE_plus_STR(plus_self12, plus_s2);
    ret_val24 = plus_self12;
    plus_self13 = ret_val24;
    plus_c2 = '\n';
    fputc(plus_c2, ATTR(plus_self13,fp));
   }
   SYSDESTROY(wholetext); 
   ;
   L911_=INTPLUS(total1,1); 
   total1 = L911_;
  }
 }
 after_loop1: ;
 L92 = ATTR(CODE_FILE_prog,benchmark);
 L931_=!(L92); 
 if (L931_) {
  close_self1 = th;
  fclose(ATTR(close_self1,fp));
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other5_0;
FSETSTR_eltbrSTR_frame noname4 = &other5_0;
  L61 = to_delete;
  noname4->self = L61;
  noname4->state = 0;
  while (1) {
   plus_self14 = CODE_FILE_dir;
   plus_arg3 = '/';
   str_self3 = plus_arg3;
   create_self6 = ((STR) NULL);
   create_c3 = str_self3;
   L95 = 1;
   L97=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L95)*sizeof(CHAR);
   L96=ZALLOC_LEAF_BIG(L97);
   if (L96==NULL) FATAL("Unable to allocate more memory");
   memset(L96,0,L97);
   ((OB)L96)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L96)->header.destroyed=0;
#endif

   L94 = ((STR) L96);
   L94->asize = L95;
   r5 = L94;
   SARR((STR)r5,0,create_c3); 
   ;
   ret_val27 = r5;
   ret_val26 = ret_val27;
   s3 = ret_val26;
   ret_val25 = STR_ap1077157958(plus_self14, s3, TRUE);
   plus_self15 = ret_val25;
   L99 = FSETSTR_eltbrSTR(noname4);
   if (noname4->state == -1) {
    goto after_loop4;
   }
   plus_sarg5 = L99;
   ret_val28 = STR_ap2004550586(plus_self15, plus_sarg5);
   name111 = ret_val28;
   delete_self = ((FILE1) NULL);
   delete_nm = name111;
   L100 = delete_nm;
   unlink(((L100==NULL)?NULL:L100->arr_part));
   delete_self1 = ((FILE1) NULL);
   replace_suffix_s = name111;
   replace_suffix_o = CONFIG1397479570(ATTR(CODE_FILE_prog,config), ((STR) &C_EXT), 0);
   replace_suffix_r = CONFIG1397479570(ATTR(CODE_FILE_prog,config), ((STR) &OBJECT_EXT), 0);
   L101 = STR_lengthrINT(replace_suffix_s);
   L102 = STR_sizerINT(replace_suffix_o);
   L1031_=INTMINUS(L101,L102); 
   plus_self16 = STR_head_INTrSTR(replace_suffix_s, L1031_);
   plus_sarg6 = replace_suffix_r;
   ret_val30 = STR_ap2004550586(plus_self16, plus_sarg6);
   ret_val29 = ret_val30;
   delete_nm1 = ret_val29;
   L104 = delete_nm1;
   unlink(((L104==NULL)?NULL:L104->arr_part));
  }
 }
 after_loop4: ;
 if (ATTR(CODE_FILE_prog,verbose)) {
  create_self7 = ((OUT) NULL);
  ret_val31 = create_self7;
  plus_self17 = ret_val31;
  plus_s3 = ((STR) &Regenerated);
  stdout_self = ((FILE1) NULL);
  L106=ZALLOC(sizeof(struct FILE1_struct));
  if (L106==NULL) FATAL("Unable to allocate more memory");
  ((OB)L106)->header.tag=FILE1_tag;
  L105 = ((FILE1) L106);
  r6 = L105;
  
  SATTR(r6,fp,stdout);
  ret_val33 = r6;
  FILE_plus_STR(ret_val33, plus_s3);
  ret_val32 = plus_self17;
  plus_self18 = ret_val32;
  L109=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L109==NULL) FATAL("Unable to allocate more memory");
  memset(L109,0,sizeof(struct INT_boxed_struct));
  ((OB)L109)->header.tag=INT_tag;
  L108 = (dSTR)((INT_boxed) L109);
  ((INT_boxed) L108)->immutable_part = regen;
  plus_s4 = L108;
  stdout_self1 = ((FILE1) NULL);
  L111=ZALLOC(sizeof(struct FILE1_struct));
  if (L111==NULL) FATAL("Unable to allocate more memory");
  ((OB)L111)->header.tag=FILE1_tag;
  L110 = ((FILE1) L111);
  r7 = L110;
  
  SATTR(r7,fp,stdout);
  ret_val35 = r7;
  FILE_plus_dSTR(ret_val35, plus_s4);
  ret_val34 = plus_self18;
  plus_self19 = ret_val34;
  plus_s5 = ((STR) &of);
  stdout_self2 = ((FILE1) NULL);
  L114=ZALLOC(sizeof(struct FILE1_struct));
  if (L114==NULL) FATAL("Unable to allocate more memory");
  ((OB)L114)->header.tag=FILE1_tag;
  L113 = ((FILE1) L114);
  r8 = L113;
  
  SATTR(r8,fp,stdout);
  ret_val37 = r8;
  FILE_plus_STR(ret_val37, plus_s5);
  ret_val36 = plus_self19;
  plus_self20 = ret_val36;
  L117=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L117==NULL) FATAL("Unable to allocate more memory");
  memset(L117,0,sizeof(struct INT_boxed_struct));
  ((OB)L117)->header.tag=INT_tag;
  L116 = (dSTR)((INT_boxed) L117);
  ((INT_boxed) L116)->immutable_part = total1;
  plus_s6 = L116;
  stdout_self3 = ((FILE1) NULL);
  L119=ZALLOC(sizeof(struct FILE1_struct));
  if (L119==NULL) FATAL("Unable to allocate more memory");
  ((OB)L119)->header.tag=FILE1_tag;
  L118 = ((FILE1) L119);
  r9 = L118;
  
  SATTR(r9,fp,stdout);
  ret_val39 = r9;
  FILE_plus_dSTR(ret_val39, plus_s6);
  ret_val38 = plus_self20;
  plus_self21 = ret_val38;
  plus_s7 = ((STR) &files1);
  stdout_self4 = ((FILE1) NULL);
  L123=ZALLOC(sizeof(struct FILE1_struct));
  if (L123==NULL) FATAL("Unable to allocate more memory");
  ((OB)L123)->header.tag=FILE1_tag;
  L122 = ((FILE1) L123);
  r10 = L122;
  
  SATTR(r10,fp,stdout);
  ret_val41 = r10;
  FILE_plus_STR(ret_val41, plus_s7);
  ret_val40 = plus_self21;
  flush_self = ret_val40;
  stdout_self5 = ((FILE1) NULL);
  L126=ZALLOC(sizeof(struct FILE1_struct));
  if (L126==NULL) FATAL("Unable to allocate more memory");
  ((OB)L126)->header.tag=FILE1_tag;
  L125 = ((FILE1) L126);
  r11 = L125;
  
  SATTR(r11,fp,stdout);
  ret_val42 = r11;
  flush_self1 = ret_val42;
  fflush(ATTR(flush_self1,fp));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F525364269(CODE_FILE self, dLAYOUT l) {
 INT s = INT_zero;
 FLISTdLAYOUT d;
 FLISTdLAYOUT L11;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 dLAYOUT L6;
 dLAYOUT L7;
 s = FSETdL858951727(ATTR(self,layouts));
 SATTR(self,layouts,FSETdL835744292(ATTR(self,layouts), l));
 L2 = FSETdL858951727(ATTR(self,layouts));
 L31_=(L2)==(s); 
 L4 = L31_;
 L51_=!(L4); 
 if (L51_) {
  L6 = l;
  d = (*dLAYOU1845593458[TAG(L6)])(L6);
  {
   struct FLISTd1954760550_frame_struct other1_0;
FLISTd1954760550_frame noname1 = &other1_0;
   L11 = d;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L7 = FLISTd1954760550(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    CODE_F525364269(self, L7);
   }
  }
  after_loop: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CODE_F65146505(CODE_FILE self) {
 CODE_FILE old1;
 FSTR length_self;
 INT ret_val = INT_zero;
 FSTR create_self;
 FSTR ret_val1;
 FLISTFSTR create_self1;
 FLISTFSTR ret_val2;
 FSTR create_self2;
 FSTR ret_val3;
 STR replace_suffix_s;
 STR replace_suffix_o;
 STR replace_suffix_r;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 FSETSIG create_self3;
 FSETSIG ret_val6;
 FSETdLAYOUT create_self4;
 FSETdLAYOUT ret_val7;
 FSETAM1542678947 create_self5;
 FSETAM1542678947 ret_val8;
 FSETAM1032778315 create_self6;
 FSETAM1032778315 ret_val9;
 FLISTA1893461511 create_self7;
 FLISTA1893461511 ret_val10;
 FSETTUPSIGSIG create_self8;
 FSETTUPSIGSIG ret_val11;
 FSETSTR create_self9;
 FSETSTR ret_val12;
 FSETSIG create_self10;
 FSETSIG ret_val13;
 INT L1;
 INT L2;
 INT L31_;
 FSTR L4;
 INT L5;
 OB L6;
 INT L7;
 BOOL L8;
 INT L9;
 INT L10;
 BOOL L111_;
 FSTR L12;
 INT L13;
 OB L14;
 INT L15;
 extern STR c1;
 extern STR xc11;
 INT L16;
 INT L17;
 INT L181_;
 SATTR(self,text1,FLISTF1280148353(ATTR(self,text1), ATTR(self,ntext)));
 L1 = ATTR(self,length1);
 length_self = ATTR(self,ntext);
 ret_val = FSTR_sizerINT(length_self);
 L2 = ret_val;
 L31_=INTPLUS(L1,L2); 
 SATTR(self,length1,L31_);
 create_self = ((FSTR) NULL);
 L5 = 16;
 L7=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
 L6=ZALLOC_LEAF_BIG(L7);
 if (L6==NULL) FATAL("Unable to allocate more memory");
 memset(L6,0,L7);
 ((OB)L6)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L6)->header.destroyed=0;
#endif

 L4 = ((FSTR) L6);
 L4->asize = L5;
 ret_val1 = L4;
 SATTR(self,ntext,ret_val1);
 L9 = ATTR(CODE_FILE_cgen,threshold);
 L10 = ATTR(self,length1);
 L111_=(L9)<(L10); 
 if (L111_) {
  L8 = ATTR(self,is_c_code);
 } else {
  L8 = FALSE;
 }
 if (L8) {
  old1 = CODE_F1432464034(((CODE_FILE) NULL), ATTR(self,name1));
  SATTR(old1,text1,ATTR(self,text1));
  SATTR(old1,ntext,ATTR(self,ntext));
  SATTR(old1,length1,ATTR(self,length1));
  SATTR(old1,chk_pre,ATTR(self,chk_pre));
  SATTR(old1,chk_post,ATTR(self,chk_post));
  SATTR(old1,chk_invariant,ATTR(self,chk_invariant));
  SATTR(old1,chk_assert,ATTR(self,chk_assert));
  SATTR(old1,chk_arith,ATTR(self,chk_arith));
  SATTR(old1,chk_bounds,ATTR(self,chk_bounds));
  SATTR(old1,chk_void,ATTR(self,chk_void));
  SATTR(old1,chk_when,ATTR(self,chk_when));
  SATTR(old1,chk_return,ATTR(self,chk_return));
  SATTR(old1,is_c_code,TRUE);
  SATTR(old1,do_not_merge,ATTR(self,do_not_merge));
  SATTR(old1,decs,ATTR(self,decs));
  SATTR(old1,layouts,ATTR(self,layouts));
  SATTR(old1,globals,ATTR(self,globals));
  SATTR(old1,bnd_rout_creates,ATTR(self,bnd_rout_creates));
  SATTR(old1,bnd_iter_creates,ATTR(self,bnd_iter_creates));
  SATTR(old1,unboxes,ATTR(self,unboxes));
  SATTR(old1,externs1,ATTR(self,externs1));
  SATTR(old1,iters,ATTR(self,iters));
  create_self1 = ((FLISTFSTR) NULL);
  ret_val2 = ((FLISTFSTR) NULL);
  SATTR(self,text1,ret_val2);
  create_self2 = ((FSTR) NULL);
  L13 = 16;
  L15=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L13)*sizeof(CHAR);
  L14=ZALLOC_LEAF_BIG(L15);
  if (L14==NULL) FATAL("Unable to allocate more memory");
  memset(L14,0,L15);
  ((OB)L14)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L14)->header.destroyed=0;
#endif

  L12 = ((FSTR) L14);
  L12->asize = L13;
  ret_val3 = L12;
  SATTR(self,ntext,ret_val3);
  SATTR(self,length1,0);
  replace_suffix_s = ATTR(self,name1);
  replace_suffix_o = ((STR) &c1);
  replace_suffix_r = ((STR) &xc11);
  L16 = STR_lengthrINT(replace_suffix_s);
  L17 = STR_sizerINT(replace_suffix_o);
  L181_=INTMINUS(L16,L17); 
  plus_self = STR_head_INTrSTR(replace_suffix_s, L181_);
  plus_sarg = replace_suffix_r;
  ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
  ret_val4 = ret_val5;
  SATTR(self,name1,ret_val4);
  create_self3 = ((FSETSIG) NULL);
  ret_val6 = ((FSETSIG) NULL);
  SATTR(self,decs,ret_val6);
  create_self4 = ((FSETdLAYOUT) NULL);
  ret_val7 = ((FSETdLAYOUT) NULL);
  SATTR(self,layouts,ret_val7);
  create_self5 = ((FSETAM1542678947) NULL);
  ret_val8 = ((FSETAM1542678947) NULL);
  SATTR(self,globals,ret_val8);
  create_self6 = ((FSETAM1032778315) NULL);
  ret_val9 = ((FSETAM1032778315) NULL);
  SATTR(self,bnd_rout_creates,ret_val9);
  create_self7 = ((FLISTA1893461511) NULL);
  ret_val10 = ((FLISTA1893461511) NULL);
  SATTR(self,bnd_iter_creates,ret_val10);
  create_self8 = ((FSETTUPSIGSIG) NULL);
  ret_val11 = ((FSETTUPSIGSIG) NULL);
  SATTR(self,unboxes,ret_val11);
  create_self9 = ((FSETSTR) NULL);
  ret_val12 = ((FSETSTR) NULL);
  SATTR(self,externs1,ret_val12);
  create_self10 = ((FSETSIG) NULL);
  ret_val13 = ((FSETSIG) NULL);
  SATTR(self,iters,ret_val13);
 }
}


#undef IS_ITER
#define IS_ITER 1

STR CODE_F423548741(CODE_F423548741_frame frame) {
 STR dummy = ((STR) NULL);
 extern STR name9;
 extern STR name1;
 dTP L1;
 extern STR RETURNED_CONST1;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 STR L5;
 INT L6;
 INT L71_;
 INT L8;
 CHAR L91_;
 CHAR L10;
 BOOL L111_;
 extern STR S_entry4;
 BOOL L12;
 BOOL L131_;
 extern STR void1;
 extern STR name166;
 extern STR name167;
 BOOL L14;
 BOOL L151_;
 ARG L18;
 ARG aL171_;
 extern STR name29;
 INT L201_;
 extern STR struct1392482001;
 extern STR name26;
 extern STR extern3;
 extern STR name13;
 BOOL L21;
 BOOL L221_;
 extern STR name10;
 extern STR void6;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 STR L26;
 INT L27;
 INT L281_;
 INT L29;
 CHAR L301_;
 CHAR L31;
 BOOL L321_;
 extern STR name24;
 extern STR S_frame4;
 extern STR struct139248200;
 dTP L33;
 extern STR name24;
 ARG L36;
 ARG aL351_;
 STR L38;
 extern STR name29;
 INT L401_;
 BOOL L42;
 BOOL L43;
 extern STR OB4;
 ARG L46;
 ARG aL451_;
 extern STR name29;
 INT L481_;
 extern STR name24;
 ARG L51;
 ARG aL501_;
 extern STR name29;
 INT L531_;
 extern STR struct139248200;
 BOOL L54;
 BOOL L55;
 BOOL L561_;
 BOOL L57;
 BOOL L581_;
 INT L60;
 AREFFL1698233817 L63;
 INT L641_;
 INT L65;
 INT L661_;
 extern STR S___;
 ARRAYINT L72;
 INT L731_;
 INT L75;
 INT aL691_;
 INT L77;
 BOOL L781_;
 extern STR L;
 extern STR f11;
 INT L791_;
 extern STR name127;
 extern STR name127;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  case 3: goto state3;
  case 4: goto state4;
  }
 state0:;
 frame->res = ((STR) NULL);
 if (SIG_is275586960(frame->arg1)) {
  frame->plus_self = CGEN_f1593827635(CODE_FILE_cgen, frame->arg1);
  frame->plus_sarg = ((STR) &name9);
  frame->ret_val = STR_ap2004550586(frame->plus_self, frame->plus_sarg);
  frame->res = frame->ret_val;
  frame->state = 1;
  return frame->res;
  state1:;
  frame->state = -1;
  return;
 }
 frame->res = ((STR) &name1);
 L1 = ATTR(frame->arg1,tp);
 if ((*dTP_is1811059018[TAG(L1)])(L1)) {
  frame->decl = ((STR) &RETURNED_CONST1);
  frame->is_iter_self = frame->arg1;
  frame->is_iter_self1 = ATTR(frame->is_iter_self,name1);
  L3 = (frame->is_iter_self1.str==((STR) NULL));
  L41_=!(L3); 
  if (L41_) {
   L5 = frame->is_iter_self1.str;
   L6 = STR_sizerINT(frame->is_iter_self1.str);
   L71_=INTMINUS(L6,1); 
   L8 = L71_;
   L91_=ARR((STR)L5,L8); 
   L10 = L91_;
   L111_=L10=='!'; 
   L2 = L111_;
  } else {
   L2 = FALSE;
  }
  frame->ret_val2 = L2;
  frame->ret_val1 = frame->ret_val2;
  if (frame->ret_val1) {
   frame->mang_self = frame->self;
   frame->mang_ob = ((OB) frame->arg1);
   frame->ret_val3 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob, ((OB) NULL));
   frame->mang_self1 = frame->self;
   frame->mang_ob1 = ((OB) frame->arg1);
   frame->ret_val4 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob1, ((OB) NULL));
   frame->decl = STR_ap1417971546(frame->decl, frame->ret_val3, ((STR) &S_entry4), frame->ret_val4);
  }
  else {
   L12 = (ATTR(frame->arg1,ret)==((dTP) NULL));
   L131_=!(L12); 
   if (L131_) {
    frame->plus_self1 = frame->decl;
    frame->mang_self2 = frame->self;
    frame->mang_ob2 = ((OB) ATTR(frame->arg1,ret));
    frame->ret_val5 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob2, ((OB) NULL));
    frame->plus_sarg1 = frame->ret_val5;
    frame->ret_val6 = STR_ap2004550586(frame->plus_self1, frame->plus_sarg1);
    frame->decl = frame->ret_val6;
   }
   else {
    frame->plus_self2 = frame->decl;
    frame->plus_sarg2 = ((STR) &void1);
    frame->ret_val7 = STR_ap2004550586(frame->plus_self2, frame->plus_sarg2);
    frame->decl = frame->ret_val7;
   }
   frame->plus_self3 = ((STR) &name166);
   frame->mang_self3 = frame->self;
   frame->mang_ob3 = ((OB) frame->arg1);
   frame->ret_val8 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob3, ((OB) NULL));
   frame->plus_sarg3 = frame->ret_val8;
   frame->ret_val9 = STR_ap2004550586(frame->plus_self3, frame->plus_sarg3);
   frame->mang_self4 = frame->self;
   frame->mang_ob4 = ((OB) ATTR(frame->arg1,tp));
   frame->ret_val10 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob4, ((OB) NULL));
   frame->decl = STR_ap1417971546(frame->decl, frame->ret_val9, ((STR) &name167), frame->ret_val10);
   L14 = (ATTR(frame->arg1,args)==((ARRAYARG) NULL));
   L151_=!(L14); 
   if (L151_) {
    {
     /* loop index variable */
     frame->L161 = 0;
     frame->f_L171_ = TRUE;
     while (1) {
      if (frame->f_L171_) {
       frame->f_L171_ = FALSE;
       frame->L191 = ATTR(frame->arg1,args);
       frame->L171_br=frame->L191==NULL?0:ASIZE((ARRAYARG)frame->L191); 
      }
      if(frame->L161>=frame->L171_br)  goto after_loop; 
      aL171_=ARR((ARRAYARG)frame->L191,frame->L161); 
      frame->e = aL171_;
      frame->mang_self5 = frame->self;
      frame->mang_ob5 = ((OB) ATTR(frame->e,tp));
      frame->ret_val11 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob5, ((OB) NULL));
      frame->decl = STR_ap1693864410(frame->decl, ((STR) &name29), frame->ret_val11);
      frame->decl = CODE_F881092569(frame->self, frame->e, frame->decl);
      L201_=INTPLUS(frame->L161,1); 
      frame->L161 = L201_;
     }
    }
    after_loop: ;
   }
   if (ATTR(CODE_FILE_cgen,func_tables)) {
    frame->plus_self4 = frame->decl;
    frame->plus_sarg4 = ((STR) &struct1392482001);
    frame->ret_val12 = STR_ap2004550586(frame->plus_self4, frame->plus_sarg4);
    frame->decl = frame->ret_val12;
   }
   frame->plus_self5 = frame->decl;
   frame->plus_sarg5 = ((STR) &name26);
   frame->ret_val13 = STR_ap2004550586(frame->plus_self5, frame->plus_sarg5);
   frame->decl = frame->ret_val13;
  }
  frame->res = STR_ap1417971546(frame->res, ((STR) &extern3), frame->decl, ((STR) &name13));
  frame->state = 2;
  return frame->res;
  state2:;
  frame->state = -1;
  return;
 }
 L21 = (ATTR(frame->arg1,ret)==((dTP) NULL));
 L221_=!(L21); 
 if (L221_) {
  frame->mang_self6 = frame->self;
  frame->mang_ob6 = ((OB) ATTR(frame->arg1,ret));
  frame->ret_val14 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob6, ((OB) NULL));
  frame->res = STR_ap1693864410(frame->res, frame->ret_val14, ((STR) &name10));
 }
 else {
  frame->plus_self6 = frame->res;
  frame->plus_sarg6 = ((STR) &void6);
  frame->ret_val15 = STR_ap2004550586(frame->plus_self6, frame->plus_sarg6);
  frame->res = frame->ret_val15;
 }
 frame->mang_self7 = frame->self;
 frame->mang_ob7 = ((OB) frame->arg1);
 frame->ret_val16 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob7, ((OB) NULL));
 frame->res = STR_ap2004550586(frame->res, frame->ret_val16);
 frame->is_iter_self2 = frame->arg1;
 frame->is_iter_self3 = ATTR(frame->is_iter_self2,name1);
 L24 = (frame->is_iter_self3.str==((STR) NULL));
 L251_=!(L24); 
 if (L251_) {
  L26 = frame->is_iter_self3.str;
  L27 = STR_sizerINT(frame->is_iter_self3.str);
  L281_=INTMINUS(L27,1); 
  L29 = L281_;
  L301_=ARR((STR)L26,L29); 
  L31 = L301_;
  L321_=L31=='!'; 
  L23 = L321_;
 } else {
  L23 = FALSE;
 }
 frame->ret_val18 = L23;
 frame->ret_val17 = frame->ret_val18;
 if (frame->ret_val17) {
  frame->plus_self9 = frame->res;
  frame->plus_sarg7 = ((STR) &name24);
  frame->ret_val19 = STR_ap2004550586(frame->plus_self9, frame->plus_sarg7);
  frame->plus_self8 = frame->ret_val19;
  frame->mang_self8 = frame->self;
  frame->mang_ob8 = ((OB) frame->arg1);
  frame->ret_val20 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob8, ((OB) NULL));
  frame->plus_sarg8 = frame->ret_val20;
  frame->ret_val21 = STR_ap2004550586(frame->plus_self8, frame->plus_sarg8);
  frame->plus_self7 = frame->ret_val21;
  frame->plus_sarg9 = ((STR) &S_frame4);
  frame->ret_val22 = STR_ap2004550586(frame->plus_self7, frame->plus_sarg9);
  frame->res = frame->ret_val22;
  if (ATTR(CODE_FILE_cgen,func_tables)) {
   frame->plus_self10 = frame->res;
   frame->plus_sarg10 = ((STR) &struct139248200);
   frame->ret_val23 = STR_ap2004550586(frame->plus_self10, frame->plus_sarg10);
   frame->res = frame->ret_val23;
  }
 }
 else {
  L33 = ATTR(frame->arg1,tp);
  if ((*dTP_is124163553[TAG(L33)])(L33)) {
   frame->plus_self11 = frame->res;
   frame->plus_sarg11 = ((STR) &name24);
   frame->ret_val24 = STR_ap2004550586(frame->plus_self11, frame->plus_sarg11);
   frame->res = frame->ret_val24;
   {
    struct STR_se405450305_frame_struct other2_0;

    /* loop index variable */
    frame->L341 = 0;
    frame->nested3 = &other2_0;
    frame->f_L351_ = TRUE;
    frame->nested3->state = 0;
    while (1) {
     if (frame->f_L351_) {
      frame->f_L351_ = FALSE;
      frame->L371 = ATTR(frame->arg1,args);
      frame->L351_br=frame->L371==NULL?0:ASIZE((ARRAYARG)frame->L371); 
     }
     if(frame->L341>=frame->L351_br)  goto after_loop1; 
     aL351_=ARR((ARRAYARG)frame->L371,frame->L341); 
     frame->e1 = aL351_;
     frame->plus_self12 = frame->res;
     if (frame->nested3->state == 0) {
      frame->L391 = ((STR) &name29);
      frame->nested3->self = frame->L391;
     }
     frame->mang_self9 = frame->self;
     frame->mang_ob9 = ((OB) ATTR(frame->e1,tp));
     frame->ret_val25 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob9, ((OB) NULL));
     frame->nested3->arg1 = ((dSTR) frame->ret_val25);
     L38 = STR_se405450305(frame->nested3);
     if (frame->nested3->state == -1) {
      goto after_loop1;
     }
     frame->plus_sarg12 = L38;
     frame->ret_val26 = STR_ap2004550586(frame->plus_self12, frame->plus_sarg12);
     frame->res = frame->ret_val26;
     frame->res = CODE_F881092569(frame->self, frame->e1, frame->res);
     L401_=INTPLUS(frame->L341,1); 
     frame->L341 = L401_;
    }
   }
   after_loop1: ;
  }
  else {
   frame->is_forked_self = frame->arg1;
   if (ATTR(frame->is_forked_self,is_par_routine)) {
    L43 = TRUE;
   } else {
    L43 = ATTR(frame->is_forked_self,is_fork_routine);
   }
   if (L43) {
    L42 = TRUE;
   } else {
    L42 = ATTR(frame->is_forked_self,is_attach_routin);
   }
   frame->ret_val27 = L42;
   if (frame->ret_val27) {
    frame->plus_self13 = frame->res;
    frame->plus_sarg13 = ((STR) &OB4);
    frame->ret_val28 = STR_ap2004550586(frame->plus_self13, frame->plus_sarg13);
    frame->res = frame->ret_val28;
    {
     /* loop index variable */
     frame->L441 = 0;
     frame->f_L451_ = TRUE;
     while (1) {
      if (frame->f_L451_) {
       frame->f_L451_ = FALSE;
       frame->L471 = ATTR(frame->arg1,args);
       frame->L451_br=frame->L471==NULL?0:ASIZE((ARRAYARG)frame->L471); 
      }
      if(frame->L441>=frame->L451_br)  goto after_loop2; 
      aL451_=ARR((ARRAYARG)frame->L471,frame->L441); 
      frame->e2 = aL451_;
      frame->mang_self10 = frame->self;
      frame->mang_ob10 = ((OB) ATTR(frame->e2,tp));
      frame->ret_val29 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob10, ((OB) NULL));
      frame->res = STR_ap1693864410(frame->res, ((STR) &name29), frame->ret_val29);
      frame->res = CODE_F881092569(frame->self, frame->e2, frame->res);
      L481_=INTPLUS(frame->L441,1); 
      frame->L441 = L481_;
     }
    }
    after_loop2: ;
   }
   else {
    frame->plus_self14 = ((STR) &name24);
    frame->mang_self11 = frame->self;
    frame->mang_ob11 = ((OB) ATTR(frame->arg1,tp));
    frame->ret_val30 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob11, ((OB) NULL));
    frame->plus_sarg14 = frame->ret_val30;
    frame->ret_val31 = STR_ap2004550586(frame->plus_self14, frame->plus_sarg14);
    frame->args = frame->ret_val31;
    {
     /* loop index variable */
     frame->L491 = 0;
     frame->f_L501_ = TRUE;
     while (1) {
      if (frame->f_L501_) {
       frame->f_L501_ = FALSE;
       frame->L521 = ATTR(frame->arg1,args);
       frame->L501_br=frame->L521==NULL?0:ASIZE((ARRAYARG)frame->L521); 
      }
      if(frame->L491>=frame->L501_br)  goto after_loop3; 
      aL501_=ARR((ARRAYARG)frame->L521,frame->L491); 
      frame->e3 = aL501_;
      frame->mang_self12 = frame->self;
      frame->mang_ob12 = ((OB) ATTR(frame->e3,tp));
      frame->ret_val32 = MANGLE119219986(ATTR(CODE_FILE_cgen,mangler), frame->mang_ob12, ((OB) NULL));
      frame->args = STR_ap1693864410(frame->args, ((STR) &name29), frame->ret_val32);
      frame->args = CODE_F881092569(frame->self, frame->e3, frame->args);
      L531_=INTPLUS(frame->L491,1); 
      frame->L491 = L531_;
     }
    }
    after_loop3: ;
    if (ATTR(CODE_FILE_cgen,func_tables)) {
     frame->plus_self15 = frame->args;
     frame->plus_sarg15 = ((STR) &struct139248200);
     frame->ret_val33 = STR_ap2004550586(frame->plus_self15, frame->plus_sarg15);
     frame->args = frame->ret_val33;
    }
    L55 = (ATTR(frame->arg1,opt_info)==((OPT_LOCAL_CALL) NULL));
    L561_=!(L55); 
    if (L561_) {
     L57 = (ATTR(ATTR(frame->arg1,opt_info),consider)==((ARRAYINT) NULL));
     L581_=!(L57); 
     L54 = L581_;
    } else {
     L54 = FALSE;
    }
    if (L54) {
     {
      frame->f_L591_ = TRUE;
      while (1) {
       if (frame->f_L591_) {
        frame->f_L591_ = FALSE;
        frame->L611 = 1;
        L63 = ATTR(ATTR(frame->arg1,opt_info),near_local);
        L641_=ASIZE((AREFFL1698233817)L63); 
        L65 = L641_;
        L661_=INTMINUS(L65,1); 
        frame->L621 = L661_;
        frame->L591_=frame->L611-1;frame->L591_m=frame->L621; 
       }
       if(frame->L591_++>=frame->L591_m)  goto after_loop4; 
       frame->i = frame->L591_;
       frame->suffix = ((STR) &S___);
       {
        /* loop index variable */
        frame->L671 = 0;
        frame->f_L681_ = TRUE;
        frame->f_L691_ = TRUE;
        frame->aeltb_if_first = TRUE;
        while (1) {
         if (frame->f_L681_) {
          frame->f_L681_ = FALSE;
          L72 = ATTR(ATTR(frame->arg1,opt_info),consider);
          L731_=(L72)==NULL?0:ASIZE((ARRAYINT)L72); 
          frame->L701 = L731_;
         }
         if(frame->L671>=frame->L701)  goto after_loop5; 
         ;
         if (frame->aeltb_if_first) {
          frame->L741 = frame->i;
          frame->aeltb_self = frame->L741;
          frame->aeltb_if_first = FALSE;
         }
         if (frame->f_L691_) {
          frame->f_L691_ = FALSE;
          frame->L761 = INT_asize;
         }
         if(frame->L671>=frame->L761)  goto after_loop5; 
         aL691_=frame->L671; 
         L77 = aL691_;
         L781_=((CHAR)((frame->aeltb_self&(1<<L77))!=0)); 
         if (L781_) {
          frame->plus_self16 = frame->suffix;
          frame->plus_sarg16 = ((STR) &L);
          frame->ret_val34 = STR_ap2004550586(frame->plus_self16, frame->plus_sarg16);
          frame->suffix = frame->ret_val34;
         }
         else {
          frame->plus_self17 = frame->suffix;
          frame->plus_sarg17 = ((STR) &f11);
          frame->ret_val35 = STR_ap2004550586(frame->plus_self17, frame->plus_sarg17);
          frame->suffix = frame->ret_val35;
         }
         L791_=INTPLUS(frame->L671,1); 
         frame->L671 = L791_;
        }
       }
       after_loop5: ;
       frame->state = 3;
       frame->plus_self20 = frame->res;
       frame->plus_sarg18 = frame->suffix;
       frame->ret_val36 = STR_ap2004550586(frame->plus_self20, frame->plus_sarg18);
       frame->plus_self19 = frame->ret_val36;
       frame->plus_sarg19 = frame->args;
       frame->ret_val37 = STR_ap2004550586(frame->plus_self19, frame->plus_sarg19);
       frame->plus_self18 = frame->ret_val37;
       frame->plus_sarg20 = ((STR) &name127);
       frame->ret_val38 = STR_ap2004550586(frame->plus_self18, frame->plus_sarg20);
       return frame->ret_val38;
       state3:;
      }
     }
     after_loop4: ;
    }
    frame->plus_self21 = frame->res;
    frame->plus_sarg21 = frame->args;
    frame->ret_val39 = STR_ap2004550586(frame->plus_self21, frame->plus_sarg21);
    frame->res = frame->ret_val39;
   }
  }
 }
 frame->plus_self22 = frame->res;
 frame->plus_sarg22 = ((STR) &name127);
 frame->ret_val40 = STR_ap2004550586(frame->plus_self22, frame->plus_sarg22);
 frame->res = frame->ret_val40;
 frame->state = 4;
 return frame->res;
 state4:;
 frame->state = -1;
 return dummy;
}
