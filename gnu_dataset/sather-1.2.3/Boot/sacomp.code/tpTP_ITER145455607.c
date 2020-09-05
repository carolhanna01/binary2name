#include "sather.h"

/* Layouts */

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

typedef struct ARRAYd2147017943_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd2147017943_frame;

typedef struct ARRAYd1730019671_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1730019671_frame;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct ELT_EQTP_ITER_struct {/* layout for ELT_EQ{TP_ITER} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQTP_ITER;

typedef struct ELT_EQTP_ROUT_struct {/* layout for ELT_EQ{TP_ROUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQTP_ROUT;

typedef struct ELT_NILTP_ITER_struct {/* layout for ELT_NIL{TP_ITER} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_ITER;

typedef struct ELT_NILTP_ROUT_struct {/* layout for ELT_NIL{TP_ROUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_ROUT;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

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

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

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

typedef struct TP_BUILTIN_struct {/* layout for TP_BUILTIN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *TP_BUILTIN;

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

typedef struct TP_CLASS_TBL_struct {/* layout for TP_CLASS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_CLASS_TBL;

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

typedef struct TP_GEN_TBL_struct {/* layout for TP_GEN_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_GEN_TBL;

typedef struct TP_GRAPH_struct {/* layout for TP_GRAPH */
 OB_HEADER header;
 struct FMAPTP291739594_struct *child_tbl;
 struct FMAPTP291739594_struct *des_tbl;
 struct FMAPTP775297600_struct *anc_tbl;
 struct FMAPTP775297600_struct *par_tbl;
 struct FSETTUPIDENTINT_struct *cur;
 struct PROG_struct *prog;
 } *TP_GRAPH;

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

typedef struct TP_ITER_TBL_struct {/* layout for TP_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_ITER_struct *arr_part[1];
 } *TP_ITER_TBL;

typedef struct TP_ITE948168378_frame_struct {
 INT state;
 TP_ITER_TBL self;/* Formal argument: self */
 TP_ITER ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ITE948168378_frame;

typedef struct TP_ITE1481975724_frame_struct {
 INT state;
 TP_ITER_TBL self;/* Formal argument: self */
 TP_ITER arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ITE1481975724_frame;

typedef struct TP_ITE1090047011_frame_struct {
 INT state;
 TP_ITER_TBL self;/* Formal argument: self */
 TP_ITER ret_val2;
 TP_ITER_TBL L61;
 TP_ITER r;
 INT L31;
 TP_ITER_TBL is_elt_nil_self;
 TP_ITER is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_ITER is_elt_nil_self1;
 TP_ITER is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_ITE1090047011_frame;

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

typedef struct TP_ROUT_TBL_struct {/* layout for TP_ROUT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_ROUT_struct *arr_part[1];
 } *TP_ROUT_TBL;

typedef struct TP_ROU2127468618_frame_struct {
 INT state;
 TP_ROUT_TBL self;/* Formal argument: self */
 TP_ROUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ROU2127468618_frame;

typedef struct TP_ROU1633691332_frame_struct {
 INT state;
 TP_ROUT_TBL self;/* Formal argument: self */
 TP_ROUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ROU1633691332_frame;

typedef struct TP_ROU1377082179_frame_struct {
 INT state;
 TP_ROUT_TBL self;/* Formal argument: self */
 TP_ROUT ret_val2;
 TP_ROUT_TBL L61;
 TP_ROUT r;
 INT L31;
 TP_ROUT_TBL is_elt_nil_self;
 TP_ROUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_ROUT is_elt_nil_self1;
 TP_ROUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_ROU1377082179_frame;

typedef struct TP_TBL_struct {/* layout for TP_TBL */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CLASS_TBL_struct *class_tbl;
 struct TP_CLASS_TBL_struct *included_classes;
 struct TP_GEN_TBL_struct *gen_tbl;
 struct TP_ITER_TBL_struct *iter_tbl;
 struct TP_ROUT_TBL_struct *f_rout_tbl;
 struct TP_ROUT_TBL_struct *rout_tbl;
 } *TP_TBL;

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

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct ARRAYA1077419174_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1077419174_frame;

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

typedef struct ARRAYI1177186037_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1177186037_frame;

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

typedef struct AS_PARAM_DEC_struct {/* layout for AS_PARAM_DEC */
 OB_HEADER header;
 struct AS_PARAM_DEC_struct *next;
 struct AS_TYPE_SPEC_struct *type_constraint;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_PARAM_DEC;

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

#include "tags.h"

/* Globals */

extern IDENT IDENT_2110206590;
extern IDENT IDENT_327685657;
extern INOUT_MODE MODES_inout_mode;
extern INT AS_TYP1851973889;
extern INT AS_TYPE_SPEC_it;
extern INT AS_TYPE_SPEC_rt;
extern INT TP_ITE34679094;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_abs_tp;
extern INT TP_KIND_ext_c_tp;
extern INT TP_KIND_iter_tp;
extern INT TP_KIND_part_tp;
extern INT TP_KIND_rout_tp;
extern INT TP_KIND_val_tp;
extern INT TP_ROU558963694;
extern IN_MODE MODES_in_mode;
extern ONCE_MODE MODES_once_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI102735229;
extern TP_CLASS TP_BUI1032249067;
extern TP_CLASS TP_BUI107670638;
extern TP_CLASS TP_BUI1116976640;
extern TP_CLASS TP_BUI1194559023;
extern TP_CLASS TP_BUI1248326412;
extern TP_CLASS TP_BUI1292722345;
extern TP_CLASS TP_BUI1296146890;
extern TP_CLASS TP_BUI1370948386;
extern TP_CLASS TP_BUI139344778;
extern TP_CLASS TP_BUI1606257075;
extern TP_CLASS TP_BUI1609790832;
extern TP_CLASS TP_BUI1643601086;
extern TP_CLASS TP_BUI1696960301;
extern TP_CLASS TP_BUI1716944501;
extern TP_CLASS TP_BUI1768246832;
extern TP_CLASS TP_BUI1769472218;
extern TP_CLASS TP_BUI1798283841;
extern TP_CLASS TP_BUI1805763264;
extern TP_CLASS TP_BUI1825364383;
extern TP_CLASS TP_BUI1959866815;
extern TP_CLASS TP_BUI2025364786;
extern TP_CLASS TP_BUI2088674906;
extern TP_CLASS TP_BUI2131222993;
extern TP_CLASS TP_BUI231294523;
extern TP_CLASS TP_BUI232519909;
extern TP_CLASS TP_BUI274781170;
extern TP_CLASS TP_BUI374366290;
extern TP_CLASS TP_BUI403444172;
extern TP_CLASS TP_BUI444762713;
extern TP_CLASS TP_BUI468587458;
extern TP_CLASS TP_BUI633637838;
extern TP_CLASS TP_BUI779482534;
extern TP_CLASS TP_BUI997290216;
extern TP_CLASS TP_BUILTIN_c_int;
extern TP_CLASS TP_BUILTIN_c_ptr;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST AS_TYPE_SPEC (**dTP_as1220129267)(dTP);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is33989660)(dTP);

extern RETURNED_CONST STR (**dCALL_TP_strrSTR)(dCALL_TP);

extern RETURNED_CONST STR (**dMODE_strrSTR)(dMODE);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
AS_TYPE_SPEC TP_ITE610562510(TP_ITER);
AS_TYPE_SPEC TP_ROU1982035318(TP_ROUT);
BOOL ARRAYB1390856792(ARRAYB1390856792_frame);
BOOL CHAR_i1303832348(CHAR);
BOOL CHAR_i424896496(CHAR);
BOOL CHAR_i629101715(CHAR);
BOOL STR_is171745744(STR, STR);
BOOL TP_CLA1568240525(TP_CLASS);
BOOL TP_CLA344221540(TP_CLASS_TBL, TP_CLASS);
BOOL TP_GRA927228682(TP_GRAPH, dTP, TP_CLASS);
BOOL TP_ITE1212470094(TP_ITER);
BOOL TP_ITE1360397017(TP_ITER_TBL, TP_ITER);
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
BOOL TP_ITE226322531(TP_ITER_TBL, TUPARR1759987776, TP_ITER);
BOOL TP_ITE387309106(TP_ITER);
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
BOOL TP_ROU1936415536(TP_ROUT_TBL, TUPARRAYARGdTP, TP_ROUT);
BOOL TP_ROU460533207(TP_ROUT);
BOOL TP_ROU681177780(TP_ROUT);
BOOL TP_ROU68166391(TP_ROUT, dCALL_TP);
BOOL TP_ROU699367341(TP_ROUT);
BOOL TP_ROU730462331(TP_ROUT);
BOOL TP_ROU761122169(TP_ROUT_TBL, TP_ROUT);
BOOL TP_ROU835748448(TP_ROUT);
BOOL TP_ROU967846898(TP_ROUT, dCALL_TP);
BOOL TP_TBL663685613(TP_TBL, dTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IDENT IDENT_1150413730(IDENT, STR);
IFC TP_ITER_ifcrIFC(TP_ITER);
IFC TP_ROUT_ifcrIFC(TP_ROUT);
IMPL TP_ITE830569150(TP_ITER);
IMPL TP_ROU723658710(TP_ROUT);
INT ARRAYA1077419174(ARRAYA1077419174_frame);
INT INT_hashrINT(INT);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
INT TP_ITE1218800034(TP_ITER_TBL, TP_ITER);
INT TP_ITE513527302(TP_ITER_TBL, TUPARR1759987776);
INT TP_ITER_hashrINT(TP_ITER);
INT TP_ITER_kindrINT(TP_ITER);
INT TP_ROU1653973250(TP_ROUT_TBL, TP_ROUT);
INT TP_ROU2036003026(TP_ROUT_TBL, TUPARRAYARGdTP);
INT TP_ROUT_hashrINT(TP_ROUT);
INT TP_ROUT_kindrINT(TP_ROUT);
PROG TP_ITE1618029866(TP_ITER);
PROG TP_ROU1724940306(TP_ROUT);
SIG SIG_ma1442297600(SIG);
SIG_TBL SIG_TB1438251609(SIG_TBL, SIG);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
STR STR_su1627796404(STR, INT);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_ROUT_strrSTR(TP_ROUT);
TP_CLASS TP_CLA1710809725(TP_CLASS, IDENT, ARRAYdTP, PROG);
TP_CLASS TP_CLA420352644(TP_CLASS_TBL, TUPIDENTARRAYdTP);
TP_CLASS TP_GEN1932307144(TP_GEN_TBL, TUPIDENTINT);
TP_CLASS TP_TBL1484824239(TP_TBL, STR, INT*);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
TP_CLASS_TBL TP_CLA426779342(TP_CLASS_TBL, TP_CLASS);
TP_GEN_TBL TP_GEN1370131280(TP_GEN_TBL, TP_CLASS);
TP_ITER TP_ITE1090047011(TP_ITE1090047011_frame);
TP_ITER TP_ITE1350567874(TP_ITER, ARRAYARG, ARRAYBOOL, dTP, PROG);
TP_ITER TP_ITE948168378(TP_ITE948168378_frame);
TP_ITER TP_ITE990128539(TP_ITER_TBL, TUPARR1759987776);
TP_ITER_TBL TP_ITE1565476760(TP_ITER_TBL, INT);
TP_ITER_TBL TP_ITE50234037(TP_ITER_TBL);
TP_ITER_TBL TP_ITE992470229(TP_ITER_TBL, TP_ITER);
TP_ROUT TP_ROU1377082179(TP_ROU1377082179_frame);
TP_ROUT TP_ROU2127468618(TP_ROU2127468618_frame);
TP_ROUT TP_ROU52430373(TP_ROUT, ARRAYARG, dTP, PROG);
TP_ROUT TP_ROU581494640(TP_ROUT_TBL, TUPARRAYARGdTP);
TP_ROUT_TBL TP_ROU1247655203(TP_ROUT_TBL, TP_ROUT);
TP_ROUT_TBL TP_ROU1415769243(TP_ROUT_TBL);
TP_ROUT_TBL TP_ROU472682568(TP_ROUT_TBL, INT);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
void ARRAYI1177186037(ARRAYI1177186037_frame);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void AS_TYP1027254244(AS_TYPE_SPEC, AS_TYPE_SPEC);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void TP_BUI717287368(TP_BUILTIN, IDENT, TP_CLASS, ARRAYdTP);
void TP_ITE1481975724(TP_ITE1481975724_frame);
void TP_ROU1633691332(TP_ROU1633691332_frame);
void TP_TBL1525652988(TP_TBL, IDENT, INT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC TP_ITE610562510(TP_ITER self) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC r;
 AS_TYPE_SPEC asp = ((AS_TYPE_SPEC) NULL);
 ARRAYARG L11;
 ARG arg;
 dTP paramtp;
 INT L21 = INT_zero;
 INT i = INT_zero;
 AS_TYPE_SPEC as0;
 AS_TYPE_SPEC create_self;
 AS_TYPE_SPEC ret_val1;
 INT L31 = INT_zero;
 AS_TYPE_SPEC L4;
 OB L5;
 INT L61_br;
 ARG aL61_;
 INT L71_;
 ARG L8;
 INT L9;
 dTP L10;
 BOOL L12;
 BOOL L131_;
 ARRAYBOOL L14;
 BOOL L151_;
 INT L161_;
 BOOL L17;
 BOOL L181_;
 dTP L19;
 create_self = ((AS_TYPE_SPEC) NULL);
 L5=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=AS_TYPE_SPEC_tag;
 L4 = ((AS_TYPE_SPEC) L5);
 ret_val1 = L4;
 r = ret_val1;
 SATTR(r,kind,AS_TYPE_SPEC_it);
 {
  BOOL f_L61_ = TRUE;
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,args);
  L21 = 0;
  L61_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L71_=L21-1; 
  while (1) {
   if(L31>=L61_br)  goto after_loop; 
   aL61_=ARR((ARRAYARG)L11,L31); 
   arg = aL61_;
   paramtp = ATTR(arg,tp);
   L71_++; 
   i = L71_;
   L10 = paramtp;
   as0 = (*dTP_as1220129267[TAG(L10)])(L10);
   L12 = (ATTR(self,hot)==((ARRAYBOOL) NULL));
   L131_=!(L12); 
   if (L131_) {
    L14 = ATTR(self,hot);
    L151_=ARR((ARRAYBOOL)L14,i); 
    SATTR(as0,is_hot,L151_);
   }
   if ((asp==((AS_TYPE_SPEC) NULL))) {
    asp = as0;
   }
   else {
    AS_TYP1027254244(asp, as0);
   }
   L161_=INTPLUS(L31,1); 
   L31 = L161_;
  }
 }
 after_loop: ;
 SATTR(r,params,asp);
 L17 = (ATTR(self,ret)==((dTP) NULL));
 L181_=!(L17); 
 if (L181_) {
  L19 = ATTR(self,ret);
  SATTR(r,ret,(*dTP_as1220129267[TAG(L19)])(L19));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC TP_ROU1982035318(TP_ROUT self) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC r;
 AS_TYPE_SPEC asp = ((AS_TYPE_SPEC) NULL);
 ARRAYARG L11;
 ARG arg;
 dTP paramtp;
 AS_TYPE_SPEC as0;
 AS_TYPE_SPEC create_self;
 AS_TYPE_SPEC ret_val1;
 INT L21 = INT_zero;
 AS_TYPE_SPEC L3;
 OB L4;
 INT L51_br;
 ARG aL51_;
 ARG L6;
 dTP L7;
 INT L81_;
 BOOL L9;
 BOOL L101_;
 dTP L12;
 create_self = ((AS_TYPE_SPEC) NULL);
 L4=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_TYPE_SPEC_tag;
 L3 = ((AS_TYPE_SPEC) L4);
 ret_val1 = L3;
 r = ret_val1;
 if (ATTR(self,is_fortran)) {
  SATTR(r,kind,AS_TYP1851973889);
 }
 else {
  SATTR(r,kind,AS_TYPE_SPEC_rt);
 }
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,args);
  L51_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARG)L11,L21); 
   arg = aL51_;
   paramtp = ATTR(arg,tp);
   L7 = paramtp;
   as0 = (*dTP_as1220129267[TAG(L7)])(L7);
   if ((asp==((AS_TYPE_SPEC) NULL))) {
    asp = as0;
   }
   else {
    AS_TYP1027254244(asp, as0);
   }
   L81_=INTPLUS(L21,1); 
   L21 = L81_;
  }
 }
 after_loop: ;
 SATTR(r,params,asp);
 L9 = (ATTR(self,ret)==((dTP) NULL));
 L101_=!(L9); 
 if (L101_) {
  L12 = ATTR(self,ret);
  SATTR(r,ret,(*dTP_as1220129267[TAG(L12)])(L12));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1212470094(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 TP_ITER L11;
 TP_ITER is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 TP_ITER is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val2 = BOOL_zero;
 TP_ITER is_eq_self2;
 OB is_eq_t2;
 BOOL ret_val3 = BOOL_zero;
 TP_ITER is_eq_self3;
 OB is_eq_t3;
 BOOL ret_val4 = BOOL_zero;
 TP_ITER is_eq_self4;
 OB is_eq_t4;
 BOOL ret_val5 = BOOL_zero;
 TP_ITER is_eq_self5;
 OB is_eq_t5;
 BOOL ret_val6 = BOOL_zero;
 TP_ITER is_eq_self6;
 OB is_eq_t6;
 BOOL ret_val7 = BOOL_zero;
 TP_ITER is_eq_self7;
 OB is_eq_t7;
 BOOL ret_val8 = BOOL_zero;
 TP_ITER is_eq_self8;
 OB is_eq_t8;
 BOOL ret_val9 = BOOL_zero;
 TP_ITER is_eq_self9;
 OB is_eq_t9;
 BOOL ret_val10 = BOOL_zero;
 TP_ITER is_eq_self10;
 OB is_eq_t10;
 BOOL ret_val11 = BOOL_zero;
 TP_ITER is_eq_self11;
 OB is_eq_t11;
 BOOL ret_val12 = BOOL_zero;
 TP_ITER is_eq_self12;
 OB is_eq_t12;
 BOOL ret_val13 = BOOL_zero;
 TP_ITER is_eq_self13;
 OB is_eq_t13;
 BOOL ret_val14 = BOOL_zero;
 TP_ITER is_eq_self14;
 OB is_eq_t14;
 BOOL ret_val15 = BOOL_zero;
 TP_ITER is_eq_self15;
 OB is_eq_t15;
 BOOL ret_val16 = BOOL_zero;
 TP_ITER is_eq_self16;
 OB is_eq_t16;
 BOOL ret_val17 = BOOL_zero;
 TP_ITER is_eq_self17;
 OB is_eq_t17;
 BOOL ret_val18 = BOOL_zero;
 TP_ITER is_eq_self18;
 OB is_eq_t18;
 BOOL ret_val19 = BOOL_zero;
 TP_ITER is_eq_self19;
 OB is_eq_t19;
 BOOL ret_val20 = BOOL_zero;
 TP_ITER is_eq_self20;
 OB is_eq_t20;
 BOOL ret_val21 = BOOL_zero;
 TP_ITER is_eq_self21;
 OB is_eq_t21;
 BOOL ret_val22 = BOOL_zero;
 TP_ITER is_eq_self22;
 OB is_eq_t22;
 BOOL ret_val23 = BOOL_zero;
 TP_ITER is_eq_self23;
 OB is_eq_t23;
 BOOL ret_val24 = BOOL_zero;
 TP_ITER is_eq_self24;
 OB is_eq_t24;
 BOOL ret_val25 = BOOL_zero;
 TP_ITER is_eq_self25;
 OB is_eq_t25;
 BOOL ret_val26 = BOOL_zero;
 TP_ITER is_eq_self26;
 OB is_eq_t26;
 BOOL ret_val27 = BOOL_zero;
 TP_ITER kind_self;
 INT ret_val28 = INT_zero;
 INT L2;
 INT L3;
 BOOL L41_;
 L11 = self;
 is_eq_self = L11;
 is_eq_t = ((OB) TP_BUI231294523);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  is_eq_self1 = L11;
  is_eq_t1 = ((OB) TP_BUI1769472218);
  ret_val2 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   is_eq_self2 = L11;
   is_eq_t2 = ((OB) TP_BUI274781170);
   ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    is_eq_self3 = L11;
    is_eq_t3 = ((OB) TP_BUI139344778);
    ret_val4 = SYSOBEQ(((OB) is_eq_self3),is_eq_t3);
    if (ret_val4) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     is_eq_self4 = L11;
     is_eq_t4 = ((OB) TP_BUILTIN_c_int);
     ret_val5 = SYSOBEQ(((OB) is_eq_self4),is_eq_t4);
     if (ret_val5) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      is_eq_self5 = L11;
      is_eq_t5 = ((OB) TP_BUI232519909);
      ret_val6 = SYSOBEQ(((OB) is_eq_self5),is_eq_t5);
      if (ret_val6) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_eq_self6 = L11;
       is_eq_t6 = ((OB) TP_BUI997290216);
       ret_val7 = SYSOBEQ(((OB) is_eq_self6),is_eq_t6);
       if (ret_val7) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        is_eq_self7 = L11;
        is_eq_t7 = ((OB) TP_BUI779482534);
        ret_val8 = SYSOBEQ(((OB) is_eq_self7),is_eq_t7);
        if (ret_val8) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         is_eq_self8 = L11;
         is_eq_t8 = ((OB) TP_BUI1768246832);
         ret_val9 = SYSOBEQ(((OB) is_eq_self8),is_eq_t8);
         if (ret_val9) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          is_eq_self9 = L11;
          is_eq_t9 = ((OB) TP_BUI107670638);
          ret_val10 = SYSOBEQ(((OB) is_eq_self9),is_eq_t9);
          if (ret_val10) {
           ret_val = TRUE;
           return ret_val;
          }
          else {
           is_eq_self10 = L11;
           is_eq_t10 = ((OB) TP_BUI1959866815);
           ret_val11 = SYSOBEQ(((OB) is_eq_self10),is_eq_t10);
           if (ret_val11) {
            ret_val = TRUE;
            return ret_val;
           }
           else {
            is_eq_self11 = L11;
            is_eq_t11 = ((OB) TP_BUI1825364383);
            ret_val12 = SYSOBEQ(((OB) is_eq_self11),is_eq_t11);
            if (ret_val12) {
             ret_val = TRUE;
             return ret_val;
            }
            else {
             is_eq_self12 = L11;
             is_eq_t12 = ((OB) TP_BUILTIN_c_ptr);
             ret_val13 = SYSOBEQ(((OB) is_eq_self12),is_eq_t12);
             if (ret_val13) {
              ret_val = TRUE;
              return ret_val;
             }
             else {
              is_eq_self13 = L11;
              is_eq_t13 = ((OB) TP_BUI468587458);
              ret_val14 = SYSOBEQ(((OB) is_eq_self13),is_eq_t13);
              if (ret_val14) {
               ret_val = TRUE;
               return ret_val;
              }
              else {
               is_eq_self14 = L11;
               is_eq_t14 = ((OB) TP_BUI1716944501);
               ret_val15 = SYSOBEQ(((OB) is_eq_self14),is_eq_t14);
               if (ret_val15) {
                ret_val = TRUE;
                return ret_val;
               }
               else {
                is_eq_self15 = L11;
                is_eq_t15 = ((OB) TP_BUI1696960301);
                ret_val16 = SYSOBEQ(((OB) is_eq_self15),is_eq_t15);
                if (ret_val16) {
                 ret_val = TRUE;
                 return ret_val;
                }
                else {
                 is_eq_self16 = L11;
                 is_eq_t16 = ((OB) TP_BUI444762713);
                 ret_val17 = SYSOBEQ(((OB) is_eq_self16),is_eq_t16);
                 if (ret_val17) {
                  ret_val = TRUE;
                  return ret_val;
                 }
                 else {
                  is_eq_self17 = L11;
                  is_eq_t17 = ((OB) TP_BUI1292722345);
                  ret_val18 = SYSOBEQ(((OB) is_eq_self17),is_eq_t17);
                  if (ret_val18) {
                   ret_val = TRUE;
                   return ret_val;
                  }
                  else {
                   is_eq_self18 = L11;
                   is_eq_t18 = ((OB) TP_BUI1248326412);
                   ret_val19 = SYSOBEQ(((OB) is_eq_self18),is_eq_t18);
                   if (ret_val19) {
                    ret_val = TRUE;
                    return ret_val;
                   }
                   else {
                    is_eq_self19 = L11;
                    is_eq_t19 = ((OB) TP_BUI1606257075);
                    ret_val20 = SYSOBEQ(((OB) is_eq_self19),is_eq_t19);
                    if (ret_val20) {
                     ret_val = TRUE;
                     return ret_val;
                    }
                    else {
                     is_eq_self20 = L11;
                     is_eq_t20 = ((OB) TP_BUI2131222993);
                     ret_val21 = SYSOBEQ(((OB) is_eq_self20),is_eq_t20);
                     if (ret_val21) {
                      ret_val = TRUE;
                      return ret_val;
                     }
                     else {
                      is_eq_self21 = L11;
                      is_eq_t21 = ((OB) TP_BUI1798283841);
                      ret_val22 = SYSOBEQ(((OB) is_eq_self21),is_eq_t21);
                      if (ret_val22) {
                       ret_val = TRUE;
                       return ret_val;
                      }
                      else {
                       is_eq_self22 = L11;
                       is_eq_t22 = ((OB) TP_BUI102735229);
                       ret_val23 = SYSOBEQ(((OB) is_eq_self22),is_eq_t22);
                       if (ret_val23) {
                        ret_val = TRUE;
                        return ret_val;
                       }
                       else {
                        is_eq_self23 = L11;
                        is_eq_t23 = ((OB) TP_BUI1296146890);
                        ret_val24 = SYSOBEQ(((OB) is_eq_self23),is_eq_t23);
                        if (ret_val24) {
                         ret_val = TRUE;
                         return ret_val;
                        }
                        else {
                         is_eq_self24 = L11;
                         is_eq_t24 = ((OB) TP_BUI2025364786);
                         ret_val25 = SYSOBEQ(((OB) is_eq_self24),is_eq_t24);
                         if (ret_val25) {
                          ret_val = TRUE;
                          return ret_val;
                         }
                         else {
                          is_eq_self25 = L11;
                          is_eq_t25 = ((OB) TP_BUI1643601086);
                          ret_val26 = SYSOBEQ(((OB) is_eq_self25),is_eq_t25);
                          if (ret_val26) {
                           ret_val = TRUE;
                           return ret_val;
                          }
                          else {
                           is_eq_self26 = L11;
                           is_eq_t26 = ((OB) TP_BUI1609790832);
                           ret_val27 = SYSOBEQ(((OB) is_eq_self26),is_eq_t26);
                           if (ret_val27) {
                            ret_val = TRUE;
                            return ret_val;
                           }
                           else {
                            kind_self = self;
                            ret_val28 = TP_KIND_iter_tp;
                            L2 = ret_val28;
                            L3 = TP_KIND_ext_c_tp;
                            L41_=(L2)==(L3); 
                            ret_val = L41_;
                            return ret_val;
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
       }
      }
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1360397017(TP_ITER_TBL self, TP_ITER e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 TP_ITER te;
 TP_ITER te1;
 TP_ITER_TBL is_elt_nil_self;
 TP_ITER is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self1;
 TP_ITER is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ITER_TBL elt_eq_self;
 TP_ITER elt_eq_e1;
 TP_ITER elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQTP_ITER elt_eq_self1;
 TP_ITER elt_eq_e11;
 TP_ITER elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 TP_ITER is_eq_self;
 OB is_eq_t;
 BOOL ret_val5 = BOOL_zero;
 TP_ITER_TBL is_elt_nil_self2;
 TP_ITER is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self3;
 TP_ITER is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 TP_ITER_TBL elt_eq_self2;
 TP_ITER elt_eq_e12;
 TP_ITER elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQTP_ITER elt_eq_self3;
 TP_ITER elt_eq_e13;
 TP_ITER elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 TP_ITER is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val10 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_ITER L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_ITER L141_;
 INT L151_;
 if ((self==((TP_ITER_TBL) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = TP_ITE1218800034(self, e);
 L21_=ASIZE((TP_ITER_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_ITER)ARR((TP_ITER_TBL)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_ITER) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_ITER) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val5 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val4 = ret_val5;
   ret_val3 = ret_val4;
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_ITER_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_ITER)ARR((TP_ITER_TBL)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTP_ITER) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((TP_ITER) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_ITER) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val10 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val9 = ret_val10;
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

BOOL TP_ITE1387653699(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1549466955(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1550467217(TP_ITER self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 ARG sar;
 ARRAYARG L21;
 ARG tar1;
 dTP sar_tp;
 ARRAYARG L31;
 ARG self_arg;
 ARRAYARG L41;
 ARG t_arg;
 dMODE L51;
 ARRAYARG L61;
 ARRAYARG L71;
 ARRAYBOOL L81;
 ARRAYBOOL L91;
 INT L101 = INT_zero;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 INT L141 = INT_zero;
 OUT create_self;
 OUT ret_val1;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 BOOL L15;
 BOOL L161_;
 ARRAYARG L17;
 INT L181_;
 INT L19;
 ARRAYARG L20;
 INT L221_;
 INT L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 INT L271_br;
 ARG aL271_;
 INT L281_br;
 ARG aL281_;
 ARG L29;
 ARG L30;
 dMODE L32;
 BOOL L33;
 BOOL L341_;
 INT L351_;
 INT L361_br;
 ARG aL361_;
 INT L371_br;
 ARG aL371_;
 ARG L38;
 ARG L39;
 dMODE L40;
 dTP L42;
 BOOL L43;
 BOOL L441_;
 dMODE L45;
 dTP L46;
 BOOL L47;
 BOOL L481_;
 dMODE L49;
 dTP L50;
 BOOL L52;
 BOOL L531_;
 dMODE L54;
 dTP L55;
 BOOL L56;
 BOOL L571_;
 INT L581_;
 INT L591_br;
 ARG aL591_;
 INT L601_br;
 ARG aL601_;
 ARG L62;
 ARG L63;
 ARG L64;
 ARG L65;
 dTP L66;
 BOOL L67;
 BOOL L681_;
 extern STR Dispat1656583420;
 INT L691_;
 ARRAYBOOL L70;
 INT L721_;
 INT L73;
 ARRAYBOOL L74;
 INT L751_;
 INT L76;
 BOOL L771_;
 BOOL L78;
 BOOL L791_;
 INT L801_br;
 BOOL aL801_;
 INT L821_br;
 BOOL aL821_;
 BOOL L83;
 BOOL L84;
 BOOL L85;
 BOOL L86;
 BOOL L871_;
 BOOL L88;
 BOOL L891_;
 extern STR hotmismatch;
 FILE1 L90;
 OB L92;
 INT L941_;
 BOOL L95;
 BOOL L96;
 BOOL L971_;
 BOOL L98;
 BOOL L991_;
 BOOL L100;
 dTP L102;
 BOOL L103;
 BOOL L1041_;
 extern STR Dispat532724520;
 if ((self==((TP_ITER) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (t==NULL) {
  goto other119;
 } else
 switch (TAG(t)) {
  case TP_CLASS_tag:
   L15 = TP_CLA1568240525(((TP_CLASS) t));
   L161_=!(L15); 
   if (L161_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    ret_val = TP_GRA927228682(ATTR(ATTR(self,prog),tp_graph), ((dTP) self), ((TP_CLASS) t));
    return ret_val;
   } break;
  case TP_ITER_tag:
   L17 = ATTR(self,args);
   L181_=(L17)==NULL?0:ASIZE((ARRAYARG)L17); 
   L19 = L181_;
   L20 = ATTR(((TP_ITER) t),args);
   L221_=(L20)==NULL?0:ASIZE((ARRAYARG)L20); 
   L23 = L221_;
   L241_=(L19)==(L23); 
   L25 = L241_;
   L261_=!(L25); 
   if (L261_) {
    ret_val = FALSE;
    return ret_val;
   }
   {
    BOOL f_L271_ = TRUE;
    BOOL f_L281_ = TRUE;
    /* loop index variable */
    L101 = 0;
    L11 = ATTR(self,args);
    L21 = ATTRs(((TP_ITER) t),args,((ARRAYARG) NULL));
    L271_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
    L281_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
    while (1) {
     if(L101>=L271_br)  goto after_loop; 
     aL271_=ARR((ARRAYARG)L11,L101); 
     sar = aL271_;
     if(L101>=L281_br)  goto after_loop; 
     aL281_=ARR((ARRAYARG)L21,L101); 
     tar1 = aL281_;
     sar_tp = ATTR(sar,tp);
     if (sar_tp==NULL) {
      goto other120;
     } else
     switch (TAG(sar_tp)) {
      default: ;
      other120: ;
       L32 = ATTR(sar,mode1);
       L33 = (*dMODE_814247358[TAG(L32)])(L32, ATTR(tar1,mode1));
       L341_=!(L33); 
       if (L341_) {
        ret_val = FALSE;
        return ret_val;
       }
     }
     L351_=INTPLUS(L101,1); 
     L101 = L351_;
    }
   }
   after_loop: ;
   {
    BOOL f_L361_ = TRUE;
    BOOL f_L371_ = TRUE;
    /* loop index variable */
    L121 = 0;
    L31 = ATTR(self,args);
    L41 = ATTRs(((TP_ITER) t),args,((ARRAYARG) NULL));
    L361_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
    L371_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
    while (1) {
     if(L121>=L361_br)  goto after_loop1; 
     aL361_=ARR((ARRAYARG)L31,L121); 
     self_arg = aL361_;
     if(L121>=L371_br)  goto after_loop1; 
     aL371_=ARR((ARRAYARG)L41,L121); 
     t_arg = aL371_;
     L51 = ATTR(self_arg,mode1);
     L40 = L51;
     if ((*dMODE_814247358[TAG(L40)])(L40, ((dMODE) MODES_in_mode))) {
      L42 = ATTR(t_arg,tp);
      L43 = (*dTP_is1999456142[TAG(L42)])(L42, ATTR(self_arg,tp));
      L441_=!(L43); 
      if (L441_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      L45 = L51;
      if ((*dMODE_814247358[TAG(L45)])(L45, ((dMODE) MODES_out_mode))) {
       L46 = ATTR(self_arg,tp);
       L47 = (*dTP_is1999456142[TAG(L46)])(L46, ATTR(t_arg,tp));
       L481_=!(L47); 
       if (L481_) {
        ret_val = FALSE;
        return ret_val;
       }
      }
      else {
       L49 = L51;
       if ((*dMODE_814247358[TAG(L49)])(L49, ((dMODE) MODES_inout_mode))) {
        L50 = ATTR(self_arg,tp);
        L52 = (*dTP_is242312711[TAG(L50)])(L50, ((OB) ATTR(t_arg,tp)));
        L531_=!(L52); 
        if (L531_) {
         ret_val = FALSE;
         return ret_val;
        }
       }
       else {
        L54 = L51;
        if ((*dMODE_814247358[TAG(L54)])(L54, ((dMODE) MODES_once_mode))) {
         L55 = ATTR(t_arg,tp);
         L56 = (*dTP_is1999456142[TAG(L55)])(L55, ATTR(self_arg,tp));
         L571_=!(L56); 
         if (L571_) {
          ret_val = FALSE;
          return ret_val;
         }
        }
        else {
         FATAL("No applicable target in case statement\nin TP_ITER::is_subtype($CALL_TP):BOOL\n./Representation/tp.sa:779:16");
        }
       }
      }
     }
     L581_=INTPLUS(L121,1); 
     L121 = L581_;
    }
   }
   after_loop1: ;
   {
    BOOL f_L591_ = TRUE;
    BOOL f_L601_ = TRUE;
    /* loop index variable */
    L131 = 0;
    L61 = ATTR(self,args);
    L71 = ATTRs(((TP_ITER) t),args,((ARRAYARG) NULL));
    L591_br=L61==NULL?0:ASIZE((ARRAYARG)L61); 
    L601_br=L71==NULL?0:ASIZE((ARRAYARG)L71); 
    while (1) {
     if(L131>=L591_br)  goto after_loop2; 
     aL591_=ARR((ARRAYARG)L61,L131); 
     L63=aL591_;
     if(L131>=L601_br)  goto after_loop2; 
     aL601_=ARR((ARRAYARG)L71,L131); 
     L65=aL601_;
     L66 = ATTR(L63,tp);
     L67 = (*dTP_is242312711[TAG(L66)])(L66, ((OB) ATTR(L65,tp)));
     L681_=!(L67); 
     if (L681_) {
      PROG_err_STR(ATTR(self,prog), ((STR) &Dispat1656583420));
      ret_val = FALSE;
      return ret_val;
     }
     L691_=INTPLUS(L131,1); 
     L131 = L691_;
    }
   }
   after_loop2: ;
   L70 = ATTR(self,hot);
   L721_=(L70)==NULL?0:ASIZE((ARRAYBOOL)L70); 
   L73 = L721_;
   L74 = ATTR(((TP_ITER) t),hot);
   L751_=(L74)==NULL?0:ASIZE((ARRAYBOOL)L74); 
   L76 = L751_;
   L771_=(L73)==(L76); 
   L78 = L771_;
   L791_=!(L78); 
   if (L791_) {
    ret_val = FALSE;
    return ret_val;
   }
   {
    BOOL f_L801_ = TRUE;
    BOOL f_L821_ = TRUE;
    /* loop index variable */
    L141 = 0;
    L81 = ATTR(self,hot);
    L91 = ATTRs(((TP_ITER) t),hot,((ARRAYBOOL) NULL));
    L801_br=L81==NULL?0:ASIZE((ARRAYBOOL)L81); 
    L821_br=L91==NULL?0:ASIZE((ARRAYBOOL)L91); 
    while (1) {
     if(L141>=L801_br)  goto after_loop3; 
     aL801_=ARR((ARRAYBOOL)L81,L141); 
     L84 = aL801_;
     if(L141>=L821_br)  goto after_loop3; 
     aL821_=ARR((ARRAYBOOL)L91,L141); 
     L86 = aL821_;
     L871_=L84==L86; 
     L88 = L871_;
     L891_=!(L88); 
     if (L891_) {
      create_self = ((OUT) NULL);
      ret_val1 = create_self;
      plus_self = ret_val1;
      plus_s = ((STR) &hotmismatch);
      stdout_self = ((FILE1) NULL);
      L92=ZALLOC(sizeof(struct FILE1_struct));
      if (L92==NULL) FATAL("Unable to allocate more memory");
      ((OB)L92)->header.tag=FILE1_tag;
      L90 = ((FILE1) L92);
      r = L90;
      
      SATTR(r,fp,stdout);
      ret_val2 = r;
      FILE_plus_STR(ret_val2, plus_s);
      ret_val = FALSE;
      return ret_val;
     }
     L941_=INTPLUS(L141,1); 
     L141 = L941_;
    }
   }
   after_loop3: ;
   L95 = TP_ITE387309106(self);
   L96 = TP_ITE387309106(((TP_ITER) t));
   L971_=L95==L96; 
   L98 = L971_;
   L991_=!(L98); 
   if (L991_) {
    ret_val = FALSE;
    return ret_val;
   }
   if (TP_ITE387309106(self)) {
    L102 = ATTR(self,ret);
    L103 = (*dTP_is242312711[TAG(L102)])(L102, ((OB) ATTR(((TP_ITER) t),ret)));
    L1041_=!(L103); 
    L100 = L1041_;
   } else {
    L100 = FALSE;
   }
   if (L100) {
    PROG_err_STR(ATTR(self,prog), ((STR) &Dispat532724520));
    ret_val = FALSE;
    return ret_val;
   }
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other119: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1606424066(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1864681437(TP_ITER self) {
 return ATTR(self,is_generic);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1879811989(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 TP_ITER kind_self;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 BOOL L31_;
 kind_self = self;
 ret_val1 = TP_KIND_iter_tp;
 L1 = ret_val1;
 L2 = TP_KIND_val_tp;
 L31_=(L1)==(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1906602522(TP_ITER self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 STR L1;
 dCALL_TP L2;
 L1 = TP_ITER_strrSTR(self);
 L2 = t;
 ret_val = STR_is171745744(L1, (*dCALL_TP_strrSTR[TAG(L2)])(L2));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE1921021286(TP_ITER self, OB t) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),t);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE2052650588(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 TP_ITER L11;
 TP_ITER tp;
 TP_ITER is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 TP_ITER is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val2 = BOOL_zero;
 TP_ITER is_eq_self2;
 OB is_eq_t2;
 BOOL ret_val3 = BOOL_zero;
 TP_ITER is_eq_self3;
 OB is_eq_t3;
 BOOL ret_val4 = BOOL_zero;
 TP_ITER is_eq_self4;
 OB is_eq_t4;
 BOOL ret_val5 = BOOL_zero;
 TP_ITER is_eq_self5;
 OB is_eq_t5;
 BOOL ret_val6 = BOOL_zero;
 TP_ITER is_eq_self6;
 OB is_eq_t6;
 BOOL ret_val7 = BOOL_zero;
 TP_ITER is_eq_self7;
 OB is_eq_t7;
 BOOL ret_val8 = BOOL_zero;
 TP_ITER is_eq_self8;
 OB is_eq_t8;
 BOOL ret_val9 = BOOL_zero;
 L11 = self;
 is_eq_self = L11;
 is_eq_t = ((OB) TP_BUI403444172);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  is_eq_self1 = L11;
  is_eq_t1 = ((OB) TP_BUI374366290);
  ret_val2 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   is_eq_self2 = L11;
   is_eq_t2 = ((OB) TP_BUI1805763264);
   ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    is_eq_self3 = L11;
    is_eq_t3 = ((OB) TP_BUI1194559023);
    ret_val4 = SYSOBEQ(((OB) is_eq_self3),is_eq_t3);
    if (ret_val4) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     is_eq_self4 = L11;
     is_eq_t4 = ((OB) TP_BUI1370948386);
     ret_val5 = SYSOBEQ(((OB) is_eq_self4),is_eq_t4);
     if (ret_val5) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      is_eq_self5 = L11;
      is_eq_t5 = ((OB) TP_BUI633637838);
      ret_val6 = SYSOBEQ(((OB) is_eq_self5),is_eq_t5);
      if (ret_val6) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_eq_self6 = L11;
       is_eq_t6 = ((OB) TP_BUI1032249067);
       ret_val7 = SYSOBEQ(((OB) is_eq_self6),is_eq_t6);
       if (ret_val7) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        is_eq_self7 = L11;
        is_eq_t7 = ((OB) TP_BUI2088674906);
        ret_val8 = SYSOBEQ(((OB) is_eq_self7),is_eq_t7);
        if (ret_val8) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         is_eq_self8 = L11;
         is_eq_t8 = ((OB) TP_BUI1116976640);
         ret_val9 = SYSOBEQ(((OB) is_eq_self8),is_eq_t8);
         if (ret_val9) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          tp = self;
         }
        }
       }
      }
     }
    }
   }
  }
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE2096764295(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE226322531(TP_ITER_TBL self, TUPARR1759987776 q, TP_ITER t) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 ARRAYARG L21;
 ARRAYBOOL L31;
 ARRAYBOOL L41;
 INT L51 = INT_zero;
 ARG is_eq_self;
 ARG is_eq_a;
 BOOL ret_val1 = BOOL_zero;
 INT L61 = INT_zero;
 BOOL L7;
 BOOL L81_;
 dTP L9;
 BOOL L10;
 BOOL L121_;
 ARRAYARG L13;
 INT L141_;
 INT L15;
 ARRAYARG L16;
 INT L171_;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L221_;
 INT L231_br;
 ARG aL231_;
 INT L241_br;
 ARG aL241_;
 ARG L25;
 ARG L26;
 BOOL L27;
 dTP L28;
 dMODE L29;
 BOOL L30;
 BOOL L321_;
 INT L331_;
 ARRAYBOOL L34;
 INT L351_;
 INT L36;
 ARRAYBOOL L37;
 INT L381_;
 INT L39;
 BOOL L401_;
 BOOL L42;
 BOOL L431_;
 INT L441_br;
 BOOL aL441_;
 INT L451_br;
 BOOL aL451_;
 BOOL L46;
 BOOL L47;
 BOOL L48;
 BOOL L49;
 BOOL L501_;
 BOOL L52;
 BOOL L531_;
 INT L541_;
 if ((t==((TP_ITER) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if ((q.t3==((dTP) NULL))) {
  L7 = (ATTR(t,ret)==((dTP) NULL));
  L81_=!(L7); 
  if (L81_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 else {
  L9 = q.t3;
  L10 = (*dTP_is242312711[TAG(L9)])(L9, ((OB) ATTR(t,ret)));
  L121_=!(L10); 
  if (L121_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 L13 = q.t1;
 L141_=(L13)==NULL?0:ASIZE((ARRAYARG)L13); 
 L15 = L141_;
 L16 = ATTR(t,args);
 L171_=(L16)==NULL?0:ASIZE((ARRAYARG)L16); 
 L18 = L171_;
 L191_=(L15)==(L18); 
 L20 = L191_;
 L221_=!(L20); 
 if (L221_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L231_ = TRUE;
  BOOL f_L241_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L11 = q.t1;
  L21 = ATTRs(t,args,((ARRAYARG) NULL));
  L231_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L241_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L51>=L231_br)  goto after_loop; 
   aL231_=ARR((ARRAYARG)L11,L51); 
   is_eq_self = aL231_;
   if(L51>=L241_br)  goto after_loop; 
   aL241_=ARR((ARRAYARG)L21,L51); 
   is_eq_a = aL241_;
   L28 = ATTR(is_eq_self,tp);
   if ((*dTP_is242312711[TAG(L28)])(L28, ((OB) ATTR(is_eq_a,tp)))) {
    L29 = ATTR(is_eq_self,mode1);
    L27 = (*dMODE_814247358[TAG(L29)])(L29, ATTR(is_eq_a,mode1));
   } else {
    L27 = FALSE;
   }
   ret_val1 = L27;
   L30 = ret_val1;
   L321_=!(L30); 
   if (L321_) {
    ret_val = FALSE;
    return ret_val;
   }
   L331_=INTPLUS(L51,1); 
   L51 = L331_;
  }
 }
 after_loop: ;
 L34 = q.t2;
 L351_=(L34)==NULL?0:ASIZE((ARRAYBOOL)L34); 
 L36 = L351_;
 L37 = ATTR(t,hot);
 L381_=(L37)==NULL?0:ASIZE((ARRAYBOOL)L37); 
 L39 = L381_;
 L401_=(L36)==(L39); 
 L42 = L401_;
 L431_=!(L42); 
 if (L431_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L441_ = TRUE;
  BOOL f_L451_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L31 = q.t2;
  L41 = ATTRs(t,hot,((ARRAYBOOL) NULL));
  L441_br=L31==NULL?0:ASIZE((ARRAYBOOL)L31); 
  L451_br=L41==NULL?0:ASIZE((ARRAYBOOL)L41); 
  while (1) {
   if(L61>=L441_br)  goto after_loop1; 
   aL441_=ARR((ARRAYBOOL)L31,L61); 
   L47 = aL441_;
   if(L61>=L451_br)  goto after_loop1; 
   aL451_=ARR((ARRAYBOOL)L41,L61); 
   L49 = aL451_;
   L501_=L47==L49; 
   L52 = L501_;
   L531_=!(L52); 
   if (L531_) {
    ret_val = FALSE;
    return ret_val;
   }
   L541_=INTPLUS(L61,1); 
   L61 = L541_;
  }
 }
 after_loop1: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE387309106(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 if ((self==((TP_ITER) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = (ATTR(self,ret)==((dTP) NULL));
 L21_=!(L1); 
 ret_val = L21_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE532005347(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE547568872(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 TP_ITER kind_self;
 INT ret_val1 = INT_zero;
 TP_ITER kind_self1;
 INT ret_val2 = INT_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L6;
 BOOL L71_;
 kind_self = self;
 ret_val1 = TP_KIND_iter_tp;
 L2 = ret_val1;
 L3 = TP_KIND_ext_c_tp;
 L41_=(L2)==(L3); 
 if (L41_) {
  L1 = TRUE;
 } else {
  kind_self1 = self;
  ret_val2 = TP_KIND_iter_tp;
  L5 = ret_val2;
  L6 = TP_KIN462481068;
  L71_=(L5)==(L6); 
  L1 = L71_;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ITE910939601(TP_ITER self) {
 BOOL ret_val = BOOL_zero;
 TP_ITER kind_self;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 BOOL L31_;
 kind_self = self;
 ret_val1 = TP_KIND_iter_tp;
 L1 = ret_val1;
 L2 = TP_KIND_part_tp;
 L31_=(L1)==(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1058813051(TP_ROUT self) {
 return ATTR(self,is_generic);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1099474123(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1105559654(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 TP_ROUT L11;
 TP_ROUT is_eq_self;
 OB is_eq_t;
 BOOL ret_val1 = BOOL_zero;
 TP_ROUT is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val2 = BOOL_zero;
 TP_ROUT is_eq_self2;
 OB is_eq_t2;
 BOOL ret_val3 = BOOL_zero;
 TP_ROUT is_eq_self3;
 OB is_eq_t3;
 BOOL ret_val4 = BOOL_zero;
 TP_ROUT is_eq_self4;
 OB is_eq_t4;
 BOOL ret_val5 = BOOL_zero;
 TP_ROUT is_eq_self5;
 OB is_eq_t5;
 BOOL ret_val6 = BOOL_zero;
 TP_ROUT is_eq_self6;
 OB is_eq_t6;
 BOOL ret_val7 = BOOL_zero;
 TP_ROUT is_eq_self7;
 OB is_eq_t7;
 BOOL ret_val8 = BOOL_zero;
 TP_ROUT is_eq_self8;
 OB is_eq_t8;
 BOOL ret_val9 = BOOL_zero;
 TP_ROUT is_eq_self9;
 OB is_eq_t9;
 BOOL ret_val10 = BOOL_zero;
 TP_ROUT is_eq_self10;
 OB is_eq_t10;
 BOOL ret_val11 = BOOL_zero;
 TP_ROUT is_eq_self11;
 OB is_eq_t11;
 BOOL ret_val12 = BOOL_zero;
 TP_ROUT is_eq_self12;
 OB is_eq_t12;
 BOOL ret_val13 = BOOL_zero;
 TP_ROUT is_eq_self13;
 OB is_eq_t13;
 BOOL ret_val14 = BOOL_zero;
 TP_ROUT is_eq_self14;
 OB is_eq_t14;
 BOOL ret_val15 = BOOL_zero;
 TP_ROUT is_eq_self15;
 OB is_eq_t15;
 BOOL ret_val16 = BOOL_zero;
 TP_ROUT is_eq_self16;
 OB is_eq_t16;
 BOOL ret_val17 = BOOL_zero;
 TP_ROUT is_eq_self17;
 OB is_eq_t17;
 BOOL ret_val18 = BOOL_zero;
 TP_ROUT is_eq_self18;
 OB is_eq_t18;
 BOOL ret_val19 = BOOL_zero;
 TP_ROUT is_eq_self19;
 OB is_eq_t19;
 BOOL ret_val20 = BOOL_zero;
 TP_ROUT is_eq_self20;
 OB is_eq_t20;
 BOOL ret_val21 = BOOL_zero;
 TP_ROUT is_eq_self21;
 OB is_eq_t21;
 BOOL ret_val22 = BOOL_zero;
 TP_ROUT is_eq_self22;
 OB is_eq_t22;
 BOOL ret_val23 = BOOL_zero;
 TP_ROUT is_eq_self23;
 OB is_eq_t23;
 BOOL ret_val24 = BOOL_zero;
 TP_ROUT is_eq_self24;
 OB is_eq_t24;
 BOOL ret_val25 = BOOL_zero;
 TP_ROUT is_eq_self25;
 OB is_eq_t25;
 BOOL ret_val26 = BOOL_zero;
 TP_ROUT is_eq_self26;
 OB is_eq_t26;
 BOOL ret_val27 = BOOL_zero;
 TP_ROUT kind_self;
 INT ret_val28 = INT_zero;
 INT L2;
 INT L3;
 BOOL L41_;
 L11 = self;
 is_eq_self = L11;
 is_eq_t = ((OB) TP_BUI231294523);
 ret_val1 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
 if (ret_val1) {
  ret_val = TRUE;
  return ret_val;
 }
 else {
  is_eq_self1 = L11;
  is_eq_t1 = ((OB) TP_BUI1769472218);
  ret_val2 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
  if (ret_val2) {
   ret_val = TRUE;
   return ret_val;
  }
  else {
   is_eq_self2 = L11;
   is_eq_t2 = ((OB) TP_BUI274781170);
   ret_val3 = SYSOBEQ(((OB) is_eq_self2),is_eq_t2);
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
   else {
    is_eq_self3 = L11;
    is_eq_t3 = ((OB) TP_BUI139344778);
    ret_val4 = SYSOBEQ(((OB) is_eq_self3),is_eq_t3);
    if (ret_val4) {
     ret_val = TRUE;
     return ret_val;
    }
    else {
     is_eq_self4 = L11;
     is_eq_t4 = ((OB) TP_BUILTIN_c_int);
     ret_val5 = SYSOBEQ(((OB) is_eq_self4),is_eq_t4);
     if (ret_val5) {
      ret_val = TRUE;
      return ret_val;
     }
     else {
      is_eq_self5 = L11;
      is_eq_t5 = ((OB) TP_BUI232519909);
      ret_val6 = SYSOBEQ(((OB) is_eq_self5),is_eq_t5);
      if (ret_val6) {
       ret_val = TRUE;
       return ret_val;
      }
      else {
       is_eq_self6 = L11;
       is_eq_t6 = ((OB) TP_BUI997290216);
       ret_val7 = SYSOBEQ(((OB) is_eq_self6),is_eq_t6);
       if (ret_val7) {
        ret_val = TRUE;
        return ret_val;
       }
       else {
        is_eq_self7 = L11;
        is_eq_t7 = ((OB) TP_BUI779482534);
        ret_val8 = SYSOBEQ(((OB) is_eq_self7),is_eq_t7);
        if (ret_val8) {
         ret_val = TRUE;
         return ret_val;
        }
        else {
         is_eq_self8 = L11;
         is_eq_t8 = ((OB) TP_BUI1768246832);
         ret_val9 = SYSOBEQ(((OB) is_eq_self8),is_eq_t8);
         if (ret_val9) {
          ret_val = TRUE;
          return ret_val;
         }
         else {
          is_eq_self9 = L11;
          is_eq_t9 = ((OB) TP_BUI107670638);
          ret_val10 = SYSOBEQ(((OB) is_eq_self9),is_eq_t9);
          if (ret_val10) {
           ret_val = TRUE;
           return ret_val;
          }
          else {
           is_eq_self10 = L11;
           is_eq_t10 = ((OB) TP_BUI1959866815);
           ret_val11 = SYSOBEQ(((OB) is_eq_self10),is_eq_t10);
           if (ret_val11) {
            ret_val = TRUE;
            return ret_val;
           }
           else {
            is_eq_self11 = L11;
            is_eq_t11 = ((OB) TP_BUI1825364383);
            ret_val12 = SYSOBEQ(((OB) is_eq_self11),is_eq_t11);
            if (ret_val12) {
             ret_val = TRUE;
             return ret_val;
            }
            else {
             is_eq_self12 = L11;
             is_eq_t12 = ((OB) TP_BUILTIN_c_ptr);
             ret_val13 = SYSOBEQ(((OB) is_eq_self12),is_eq_t12);
             if (ret_val13) {
              ret_val = TRUE;
              return ret_val;
             }
             else {
              is_eq_self13 = L11;
              is_eq_t13 = ((OB) TP_BUI468587458);
              ret_val14 = SYSOBEQ(((OB) is_eq_self13),is_eq_t13);
              if (ret_val14) {
               ret_val = TRUE;
               return ret_val;
              }
              else {
               is_eq_self14 = L11;
               is_eq_t14 = ((OB) TP_BUI1716944501);
               ret_val15 = SYSOBEQ(((OB) is_eq_self14),is_eq_t14);
               if (ret_val15) {
                ret_val = TRUE;
                return ret_val;
               }
               else {
                is_eq_self15 = L11;
                is_eq_t15 = ((OB) TP_BUI1696960301);
                ret_val16 = SYSOBEQ(((OB) is_eq_self15),is_eq_t15);
                if (ret_val16) {
                 ret_val = TRUE;
                 return ret_val;
                }
                else {
                 is_eq_self16 = L11;
                 is_eq_t16 = ((OB) TP_BUI444762713);
                 ret_val17 = SYSOBEQ(((OB) is_eq_self16),is_eq_t16);
                 if (ret_val17) {
                  ret_val = TRUE;
                  return ret_val;
                 }
                 else {
                  is_eq_self17 = L11;
                  is_eq_t17 = ((OB) TP_BUI1292722345);
                  ret_val18 = SYSOBEQ(((OB) is_eq_self17),is_eq_t17);
                  if (ret_val18) {
                   ret_val = TRUE;
                   return ret_val;
                  }
                  else {
                   is_eq_self18 = L11;
                   is_eq_t18 = ((OB) TP_BUI1248326412);
                   ret_val19 = SYSOBEQ(((OB) is_eq_self18),is_eq_t18);
                   if (ret_val19) {
                    ret_val = TRUE;
                    return ret_val;
                   }
                   else {
                    is_eq_self19 = L11;
                    is_eq_t19 = ((OB) TP_BUI1606257075);
                    ret_val20 = SYSOBEQ(((OB) is_eq_self19),is_eq_t19);
                    if (ret_val20) {
                     ret_val = TRUE;
                     return ret_val;
                    }
                    else {
                     is_eq_self20 = L11;
                     is_eq_t20 = ((OB) TP_BUI2131222993);
                     ret_val21 = SYSOBEQ(((OB) is_eq_self20),is_eq_t20);
                     if (ret_val21) {
                      ret_val = TRUE;
                      return ret_val;
                     }
                     else {
                      is_eq_self21 = L11;
                      is_eq_t21 = ((OB) TP_BUI1798283841);
                      ret_val22 = SYSOBEQ(((OB) is_eq_self21),is_eq_t21);
                      if (ret_val22) {
                       ret_val = TRUE;
                       return ret_val;
                      }
                      else {
                       is_eq_self22 = L11;
                       is_eq_t22 = ((OB) TP_BUI102735229);
                       ret_val23 = SYSOBEQ(((OB) is_eq_self22),is_eq_t22);
                       if (ret_val23) {
                        ret_val = TRUE;
                        return ret_val;
                       }
                       else {
                        is_eq_self23 = L11;
                        is_eq_t23 = ((OB) TP_BUI1296146890);
                        ret_val24 = SYSOBEQ(((OB) is_eq_self23),is_eq_t23);
                        if (ret_val24) {
                         ret_val = TRUE;
                         return ret_val;
                        }
                        else {
                         is_eq_self24 = L11;
                         is_eq_t24 = ((OB) TP_BUI2025364786);
                         ret_val25 = SYSOBEQ(((OB) is_eq_self24),is_eq_t24);
                         if (ret_val25) {
                          ret_val = TRUE;
                          return ret_val;
                         }
                         else {
                          is_eq_self25 = L11;
                          is_eq_t25 = ((OB) TP_BUI1643601086);
                          ret_val26 = SYSOBEQ(((OB) is_eq_self25),is_eq_t25);
                          if (ret_val26) {
                           ret_val = TRUE;
                           return ret_val;
                          }
                          else {
                           is_eq_self26 = L11;
                           is_eq_t26 = ((OB) TP_BUI1609790832);
                           ret_val27 = SYSOBEQ(((OB) is_eq_self26),is_eq_t26);
                           if (ret_val27) {
                            ret_val = TRUE;
                            return ret_val;
                           }
                           else {
                            kind_self = self;
                            ret_val28 = TP_KIND_rout_tp;
                            L2 = ret_val28;
                            L3 = TP_KIND_ext_c_tp;
                            L41_=(L2)==(L3); 
                            ret_val = L41_;
                            return ret_val;
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
       }
      }
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1673918401(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU177994147(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1849661410(TP_ROUT self, OB t) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),t);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1919694918(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU1936415536(TP_ROUT_TBL self, TUPARRAYARGdTP q, TP_ROUT t) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 ARRAYARG L21;
 INT L31 = INT_zero;
 ARG is_eq_self;
 ARG is_eq_a;
 BOOL ret_val1 = BOOL_zero;
 BOOL L4;
 BOOL L51_;
 dTP L6;
 BOOL L7;
 BOOL L81_;
 ARRAYARG L9;
 INT L101_;
 INT L12;
 ARRAYARG L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 INT L191_br;
 ARG aL191_;
 INT L201_br;
 ARG aL201_;
 ARG L22;
 ARG L23;
 BOOL L24;
 dTP L25;
 dMODE L26;
 BOOL L27;
 BOOL L281_;
 INT L291_;
 if ((t==((TP_ROUT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if ((q.t2==((dTP) NULL))) {
  L4 = (ATTR(t,ret)==((dTP) NULL));
  L51_=!(L4); 
  if (L51_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 else {
  L6 = q.t2;
  L7 = (*dTP_is242312711[TAG(L6)])(L6, ((OB) ATTR(t,ret)));
  L81_=!(L7); 
  if (L81_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 L9 = q.t1;
 L101_=(L9)==NULL?0:ASIZE((ARRAYARG)L9); 
 L12 = L101_;
 L13 = ATTR(t,args);
 L141_=(L13)==NULL?0:ASIZE((ARRAYARG)L13); 
 L15 = L141_;
 L161_=(L12)==(L15); 
 L17 = L161_;
 L181_=!(L17); 
 if (L181_) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  BOOL f_L191_ = TRUE;
  BOOL f_L201_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = q.t1;
  L21 = ATTRs(t,args,((ARRAYARG) NULL));
  L191_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  L201_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
  while (1) {
   if(L31>=L191_br)  goto after_loop; 
   aL191_=ARR((ARRAYARG)L11,L31); 
   is_eq_self = aL191_;
   if(L31>=L201_br)  goto after_loop; 
   aL201_=ARR((ARRAYARG)L21,L31); 
   is_eq_a = aL201_;
   L25 = ATTR(is_eq_self,tp);
   if ((*dTP_is242312711[TAG(L25)])(L25, ((OB) ATTR(is_eq_a,tp)))) {
    L26 = ATTR(is_eq_self,mode1);
    L24 = (*dMODE_814247358[TAG(L26)])(L26, ATTR(is_eq_a,mode1));
   } else {
    L24 = FALSE;
   }
   ret_val1 = L24;
   L27 = ret_val1;
   L281_=!(L27); 
   if (L281_) {
    ret_val = FALSE;
    return ret_val;
   }
   L291_=INTPLUS(L31,1); 
   L31 = L291_;
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU460533207(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 TP_ROUT kind_self;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 BOOL L31_;
 kind_self = self;
 ret_val1 = TP_KIND_rout_tp;
 L1 = ret_val1;
 L2 = TP_KIND_part_tp;
 L31_=(L1)==(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU681177780(TP_ROUT self) {
 return ATTR(self,is_fortran);
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU68166391(TP_ROUT self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 ARRAYARG L11;
 ARRAYARG L21;
 ARRAYARG L31;
 ARG self_arg;
 ARRAYARG L41;
 ARG t_arg;
 dMODE L51;
 ARRAYARG L61;
 ARRAYARG L71;
 TP_ROUT has_ret_self;
 BOOL ret_val1 = BOOL_zero;
 TP_ROUT has_ret_self1;
 BOOL ret_val2 = BOOL_zero;
 INT L81 = INT_zero;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 TP_ROUT has_ret_self2;
 BOOL ret_val3 = BOOL_zero;
 BOOL L12;
 BOOL L131_;
 ARRAYARG L14;
 INT L151_;
 INT L16;
 ARRAYARG L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 INT L341_br;
 ARG aL341_;
 INT L351_br;
 ARG aL351_;
 ARG L36;
 ARG L37;
 ARG L38;
 ARG L39;
 dMODE L40;
 BOOL L42;
 BOOL L431_;
 INT L441_;
 INT L451_br;
 ARG aL451_;
 INT L461_br;
 ARG aL461_;
 ARG L47;
 ARG L48;
 dMODE L49;
 dTP L50;
 BOOL L52;
 BOOL L531_;
 dMODE L54;
 dTP L55;
 BOOL L56;
 BOOL L571_;
 dMODE L58;
 dTP L59;
 BOOL L60;
 BOOL L621_;
 dMODE L63;
 dTP L64;
 BOOL L65;
 BOOL L661_;
 INT L671_;
 INT L681_br;
 ARG aL681_;
 INT L691_br;
 ARG aL691_;
 ARG L70;
 ARG L72;
 ARG L73;
 ARG L74;
 dTP L75;
 BOOL L76;
 BOOL L771_;
 extern STR Dispat1823530907;
 INT L781_;
 BOOL L79;
 BOOL L80;
 BOOL L821_;
 dTP L83;
 BOOL L84;
 BOOL L851_;
 extern STR Dispat878212051;
 if ((self==((TP_ROUT) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (t==NULL) {
  goto other121;
 } else
 switch (TAG(t)) {
  case TP_CLASS_tag:
   L12 = TP_CLA1568240525(((TP_CLASS) t));
   L131_=!(L12); 
   if (L131_) {
    ret_val = FALSE;
    return ret_val;
   }
   else {
    ret_val = TP_GRA927228682(ATTR(ATTR(self,prog),tp_graph), ((dTP) self), ((TP_CLASS) t));
    return ret_val;
   } break;
  case TP_ROUT_tag:
   L14 = ATTR(self,args);
   L151_=(L14)==NULL?0:ASIZE((ARRAYARG)L14); 
   L16 = L151_;
   L17 = ATTR(((TP_ROUT) t),args);
   L181_=(L17)==NULL?0:ASIZE((ARRAYARG)L17); 
   L19 = L181_;
   L201_=(L16)==(L19); 
   L22 = L201_;
   L231_=!(L22); 
   if (L231_) {
    ret_val = FALSE;
    return ret_val;
   }
   has_ret_self = self;
   L24 = (ATTR(has_ret_self,ret)==((dTP) NULL));
   L251_=!(L24); 
   ret_val1 = L251_;
   L26 = ret_val1;
   has_ret_self1 = ((TP_ROUT) t);
   L27 = (ATTR(has_ret_self1,ret)==((dTP) NULL));
   L281_=!(L27); 
   ret_val2 = L281_;
   L29 = ret_val2;
   L301_=L26==L29; 
   L32 = L301_;
   L331_=!(L32); 
   if (L331_) {
    ret_val = FALSE;
    return ret_val;
   }
   {
    BOOL f_L341_ = TRUE;
    BOOL f_L351_ = TRUE;
    /* loop index variable */
    L81 = 0;
    L11 = ATTR(self,args);
    L21 = ATTRs(((TP_ROUT) t),args,((ARRAYARG) NULL));
    L341_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
    L351_br=L21==NULL?0:ASIZE((ARRAYARG)L21); 
    while (1) {
     if(L81>=L341_br)  goto after_loop; 
     aL341_=ARR((ARRAYARG)L11,L81); 
     L37=aL341_;
     if(L81>=L351_br)  goto after_loop; 
     aL351_=ARR((ARRAYARG)L21,L81); 
     L39=aL351_;
     L40 = ATTR(L37,mode1);
     L42 = (*dMODE_814247358[TAG(L40)])(L40, ATTR(L39,mode1));
     L431_=!(L42); 
     if (L431_) {
      ret_val = FALSE;
      return ret_val;
     }
     L441_=INTPLUS(L81,1); 
     L81 = L441_;
    }
   }
   after_loop: ;
   {
    BOOL f_L451_ = TRUE;
    BOOL f_L461_ = TRUE;
    /* loop index variable */
    L91 = 0;
    L31 = ATTR(self,args);
    L41 = ATTRs(((TP_ROUT) t),args,((ARRAYARG) NULL));
    L451_br=L31==NULL?0:ASIZE((ARRAYARG)L31); 
    L461_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
    while (1) {
     if(L91>=L451_br)  goto after_loop1; 
     aL451_=ARR((ARRAYARG)L31,L91); 
     self_arg = aL451_;
     if(L91>=L461_br)  goto after_loop1; 
     aL461_=ARR((ARRAYARG)L41,L91); 
     t_arg = aL461_;
     L51 = ATTR(self_arg,mode1);
     L49 = L51;
     if ((*dMODE_814247358[TAG(L49)])(L49, ((dMODE) MODES_in_mode))) {
      L50 = ATTR(t_arg,tp);
      L52 = (*dTP_is1999456142[TAG(L50)])(L50, ATTR(self_arg,tp));
      L531_=!(L52); 
      if (L531_) {
       ret_val = FALSE;
       return ret_val;
      }
     }
     else {
      L54 = L51;
      if ((*dMODE_814247358[TAG(L54)])(L54, ((dMODE) MODES_out_mode))) {
       L55 = ATTR(self_arg,tp);
       L56 = (*dTP_is1999456142[TAG(L55)])(L55, ATTR(t_arg,tp));
       L571_=!(L56); 
       if (L571_) {
        ret_val = FALSE;
        return ret_val;
       }
      }
      else {
       L58 = L51;
       if ((*dMODE_814247358[TAG(L58)])(L58, ((dMODE) MODES_inout_mode))) {
        L59 = ATTR(self_arg,tp);
        L60 = (*dTP_is242312711[TAG(L59)])(L59, ((OB) ATTR(t_arg,tp)));
        L621_=!(L60); 
        if (L621_) {
         ret_val = FALSE;
         return ret_val;
        }
       }
       else {
        L63 = L51;
        if ((*dMODE_814247358[TAG(L63)])(L63, ((dMODE) MODES_once_mode))) {
         L64 = ATTR(t_arg,tp);
         L65 = (*dTP_is1999456142[TAG(L64)])(L64, ATTR(self_arg,tp));
         L661_=!(L65); 
         if (L661_) {
          ret_val = FALSE;
          return ret_val;
         }
        }
        else {
         FATAL("No applicable target in case statement\nin TP_ROUT::is_subtype($CALL_TP):BOOL\n./Representation/tp.sa:581:20");
        }
       }
      }
     }
     L671_=INTPLUS(L91,1); 
     L91 = L671_;
    }
   }
   after_loop1: ;
   {
    BOOL f_L681_ = TRUE;
    BOOL f_L691_ = TRUE;
    /* loop index variable */
    L101 = 0;
    L61 = ATTR(self,args);
    L71 = ATTRs(((TP_ROUT) t),args,((ARRAYARG) NULL));
    L681_br=L61==NULL?0:ASIZE((ARRAYARG)L61); 
    L691_br=L71==NULL?0:ASIZE((ARRAYARG)L71); 
    while (1) {
     if(L101>=L681_br)  goto after_loop2; 
     aL681_=ARR((ARRAYARG)L61,L101); 
     L72=aL681_;
     if(L101>=L691_br)  goto after_loop2; 
     aL691_=ARR((ARRAYARG)L71,L101); 
     L74=aL691_;
     L75 = ATTR(L72,tp);
     L76 = (*dTP_is242312711[TAG(L75)])(L75, ((OB) ATTR(L74,tp)));
     L771_=!(L76); 
     if (L771_) {
      PROG_err_STR(ATTR(self,prog), ((STR) &Dispat1823530907));
      ret_val = FALSE;
      return ret_val;
     }
     L781_=INTPLUS(L101,1); 
     L101 = L781_;
    }
   }
   after_loop2: ;
   has_ret_self2 = self;
   L80 = (ATTR(has_ret_self2,ret)==((dTP) NULL));
   L821_=!(L80); 
   ret_val3 = L821_;
   if (ret_val3) {
    L83 = ATTR(self,ret);
    L84 = (*dTP_is242312711[TAG(L83)])(L83, ((OB) ATTR(((TP_ROUT) t),ret)));
    L851_=!(L84); 
    L79 = L851_;
   } else {
    L79 = FALSE;
   }
   if (L79) {
    PROG_err_STR(ATTR(self,prog), ((STR) &Dispat878212051));
    ret_val = FALSE;
    return ret_val;
   }
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other121: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU699367341(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 TP_ROUT kind_self;
 INT ret_val1 = INT_zero;
 INT L1;
 INT L2;
 BOOL L31_;
 kind_self = self;
 ret_val1 = TP_KIND_rout_tp;
 L1 = ret_val1;
 L2 = TP_KIND_val_tp;
 L31_=(L1)==(L2); 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU730462331(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU761122169(TP_ROUT_TBL self, TP_ROUT e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 TP_ROUT te;
 TP_ROUT te1;
 TP_ROUT_TBL is_elt_nil_self;
 TP_ROUT is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self1;
 TP_ROUT is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ROUT_TBL elt_eq_self;
 TP_ROUT elt_eq_e1;
 TP_ROUT elt_eq_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_EQTP_ROUT elt_eq_self1;
 TP_ROUT elt_eq_e11;
 TP_ROUT elt_eq_e21;
 BOOL ret_val4 = BOOL_zero;
 TP_ROUT is_eq_self;
 OB is_eq_t;
 BOOL ret_val5 = BOOL_zero;
 TP_ROUT_TBL is_elt_nil_self2;
 TP_ROUT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self3;
 TP_ROUT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 TP_ROUT_TBL elt_eq_self2;
 TP_ROUT elt_eq_e12;
 TP_ROUT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQTP_ROUT elt_eq_self3;
 TP_ROUT elt_eq_e13;
 TP_ROUT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 TP_ROUT is_eq_self1;
 OB is_eq_t1;
 BOOL ret_val10 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_ROUT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_ROUT L141_;
 INT L151_;
 if ((self==((TP_ROUT_TBL) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 L1 = TP_ROU1653973250(self, e);
 L21_=ASIZE((TP_ROUT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_ROUT)ARR((TP_ROUT_TBL)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_ROUT) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_ROUT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val5 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val4 = ret_val5;
   ret_val3 = ret_val4;
   if (ret_val3) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_ROUT_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_ROUT)ARR((TP_ROUT_TBL)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NILTP_ROUT) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((TP_ROUT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_ROUT) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val10 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val9 = ret_val10;
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

BOOL TP_ROU835748448(TP_ROUT self) {
 BOOL ret_val = BOOL_zero;
 TP_ROUT kind_self;
 INT ret_val1 = INT_zero;
 TP_ROUT kind_self1;
 INT ret_val2 = INT_zero;
 BOOL L1;
 INT L2;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L6;
 BOOL L71_;
 kind_self = self;
 ret_val1 = TP_KIND_rout_tp;
 L2 = ret_val1;
 L3 = TP_KIND_ext_c_tp;
 L41_=(L2)==(L3); 
 if (L41_) {
  L1 = TRUE;
 } else {
  kind_self1 = self;
  ret_val2 = TP_KIND_rout_tp;
  L5 = ret_val2;
  L6 = TP_KIN462481068;
  L71_=(L5)==(L6); 
  L1 = L71_;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_ROU967846898(TP_ROUT self, dCALL_TP t) {
 BOOL ret_val = BOOL_zero;
 STR L1;
 dCALL_TP L2;
 L1 = TP_ROUT_strrSTR(self);
 L2 = t;
 ret_val = STR_is171745744(L1, (*dCALL_TP_strrSTR[TAG(L2)])(L2));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL TP_TBL663685613(TP_TBL self, dTP t) {
 BOOL ret_val = BOOL_zero;
 switch (TAG(t)) {
  case TP_CLASS_tag:
   ret_val = TP_CLA344221540(ATTR(self,class_tbl), ((TP_CLASS) t));
   return ret_val; break;
  case TP_ROUT_tag:
   if (ATTR(((TP_ROUT) t),is_fortran)) {
    ret_val = TP_ROU761122169(ATTR(self,f_rout_tbl), ((TP_ROUT) t));
    return ret_val;
   }
   else {
    ret_val = TP_ROU761122169(ATTR(self,rout_tbl), ((TP_ROUT) t));
    return ret_val;
   } break;
  case TP_ITER_tag:
   ret_val = TP_ITE1360397017(ATTR(self,iter_tbl), ((TP_ITER) t));
   return ret_val; break;
  default: ;
   FATAL("No applicable type in typecase\nin TP_TBL::test($TP):BOOL\n./Representation/tp.sa:1301:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

IFC TP_ITER_ifcrIFC(TP_ITER self) {
 IFC ret_val;
 IFC ifc_for_iter_sel;
 TP_ITER ifc_for_iter_t;
 IFC ret_val1;
 IFC r;
 SIG bound_iter_call_;
 TP_ITER bound_iter_call_1;
 SIG ret_val2;
 SIG r1;
 IFC L1;
 OB L2;
 SIG L3;
 OB L4;
 if ((ATTR(self,my_ifc)==((IFC) NULL))) {
  ifc_for_iter_sel = ((IFC) NULL);
  ifc_for_iter_t = self;
  L2=ZALLOC(sizeof(struct IFC_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=IFC_tag;
  L1 = ((IFC) L2);
  r = L1;
  SATTR(r,tp,((dTP) ifc_for_iter_t));
  bound_iter_call_ = ((SIG) NULL);
  bound_iter_call_1 = ifc_for_iter_t;
  L4=ZALLOC(sizeof(struct SIG_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=SIG_tag;
  L3 = ((SIG) L4);
  r1 = L3;
  SATTR(r1,tp,((dTP) bound_iter_call_1));
  SATTR(r1,name1,IDENT_327685657);
  SATTR(r1,args,ATTR(bound_iter_call_1,args));
  SATTR(r1,hot,ATTR(bound_iter_call_1,hot));
  SATTR(r1,ret,ATTR(bound_iter_call_1,ret));
  ret_val2 = SIG_ma1442297600(r1);
  SATTR(r,sigs,SIG_TB1438251609(ATTR(r,sigs), ret_val2));
  ret_val1 = r;
  SATTR(self,my_ifc,ret_val1);
 }
 ret_val = ATTR(self,my_ifc);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IFC TP_ROUT_ifcrIFC(TP_ROUT self) {
 IFC ret_val;
 IFC ifc_for_rout_sel;
 TP_ROUT ifc_for_rout_t;
 IFC ret_val1;
 IFC r;
 SIG bound_routine_ca;
 TP_ROUT bound_routine_ca1;
 SIG ret_val2;
 SIG r1;
 IFC L1;
 OB L2;
 SIG L3;
 OB L4;
 if ((ATTR(self,my_ifc)==((IFC) NULL))) {
  ifc_for_rout_sel = ((IFC) NULL);
  ifc_for_rout_t = self;
  L2=ZALLOC(sizeof(struct IFC_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=IFC_tag;
  L1 = ((IFC) L2);
  r = L1;
  SATTR(r,tp,((dTP) ifc_for_rout_t));
  bound_routine_ca = ((SIG) NULL);
  bound_routine_ca1 = ifc_for_rout_t;
  L4=ZALLOC(sizeof(struct SIG_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=SIG_tag;
  L3 = ((SIG) L4);
  r1 = L3;
  SATTR(r1,tp,((dTP) bound_routine_ca1));
  SATTR(r1,name1,IDENT_2110206590);
  SATTR(r1,args,ATTR(bound_routine_ca1,args));
  SATTR(r1,ret,ATTR(bound_routine_ca1,ret));
  ret_val2 = SIG_ma1442297600(r1);
  SATTR(r,sigs,SIG_TB1438251609(ATTR(r,sigs), ret_val2));
  ret_val1 = r;
  SATTR(self,my_ifc,ret_val1);
 }
 ret_val = ATTR(self,my_ifc);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL TP_ITE830569150(TP_ITER self) {
 IMPL ret_val;
 ret_val = ((IMPL) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IMPL TP_ROU723658710(TP_ROUT self) {
 IMPL ret_val;
 ret_val = ((IMPL) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ITE1218800034(TP_ITER_TBL self, TP_ITER e) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 BOOL L3;
 BOOL L41_;
 dTP L5;
 INT L61_;
 INT L71_br;
 INT rL71_;
 INT L8;
 ARRAYARG L9;
 ARG L101_;
 ARG L12;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 r = 0;
 L3 = (ATTR(e,ret)==((dTP) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(e,ret);
  L61_=SYSID(L5); 
  r = INT_hashrINT(L61_);
 }
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(e,args);
  L71_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L71_br)  goto after_loop; 
   rL71_=L21; 
   i = rL71_;
   L9 = ATTR(e,args);
   L101_=(ARG)ARR((ARRAYARG)L9,i); 
   L12=L101_;
   L13 = ATTR(L12,tp);
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ITE513527302(TP_ITER_TBL self, TUPARR1759987776 q) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 BOOL L3;
 BOOL L41_;
 dTP L5;
 INT L61_;
 INT L71_br;
 INT rL71_;
 INT L8;
 ARRAYARG L9;
 ARG L101_;
 ARG L12;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 r = 0;
 L3 = (q.t3==((dTP) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = q.t3;
  L61_=SYSID(L5); 
  r = INT_hashrINT(L61_);
 }
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = q.t1;
  L71_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L71_br)  goto after_loop; 
   rL71_=L21; 
   i = rL71_;
   L9 = q.t1;
   L101_=(ARG)ARR((ARRAYARG)L9,i); 
   L12=L101_;
   L13 = ATTR(L12,tp);
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ITER_hashrINT(TP_ITER self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ITER_kindrINT(TP_ITER self) {
 INT ret_val = INT_zero;
 ret_val = TP_KIND_iter_tp;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ROU1653973250(TP_ROUT_TBL self, TP_ROUT e) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 BOOL L3;
 BOOL L41_;
 dTP L5;
 INT L61_;
 INT L71_br;
 INT rL71_;
 INT L8;
 ARRAYARG L9;
 ARG L101_;
 ARG L12;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 r = 0;
 L3 = (ATTR(e,ret)==((dTP) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(e,ret);
  L61_=SYSID(L5); 
  r = INT_hashrINT(L61_);
 }
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(e,args);
  L71_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L71_br)  goto after_loop; 
   rL71_=L21; 
   i = rL71_;
   L9 = ATTR(e,args);
   L101_=(ARG)ARR((ARRAYARG)L9,i); 
   L12=L101_;
   L13 = ATTR(L12,tp);
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ROU2036003026(TP_ROUT_TBL self, TUPARRAYARGdTP q) {
 INT ret_val = INT_zero;
 INT r = INT_zero;
 ARRAYARG L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 BOOL L3;
 BOOL L41_;
 dTP L5;
 INT L61_;
 INT L71_br;
 INT rL71_;
 INT L8;
 ARRAYARG L9;
 ARG L101_;
 ARG L12;
 dTP L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L221_;
 r = 0;
 L3 = (q.t2==((dTP) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = q.t2;
  L61_=SYSID(L5); 
  r = INT_hashrINT(L61_);
 }
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = q.t1;
  L71_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L71_br)  goto after_loop; 
   rL71_=L21; 
   i = rL71_;
   L9 = q.t1;
   L101_=(ARG)ARR((ARRAYARG)L9,i); 
   L12=L101_;
   L13 = ATTR(L12,tp);
   L141_=SYSID(L13); 
   L15 = L141_;
   L161_=((unsigned)r+(unsigned)L15); 
   L17 = L161_;
   L181_=((unsigned)L17+(unsigned)i); 
   L19 = L181_;
   L201_=((unsigned)L19*(unsigned)19); 
   r = L201_;
   L221_=INTPLUS(L21,1); 
   L21 = L221_;
  }
 }
 after_loop: ;
 ret_val = INT_hashrINT(r);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ROUT_hashrINT(TP_ROUT self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT TP_ROUT_kindrINT(TP_ROUT self) {
 INT ret_val = INT_zero;
 ret_val = TP_KIND_rout_tp;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

PROG TP_ITE1618029866(TP_ITER self) {
 return ATTR(self,prog);
}


#undef IS_ITER
#define IS_ITER 0

PROG TP_ROU1724940306(TP_ROUT self) {
 return ATTR(self,prog);
}


#undef IS_ITER
#define IS_ITER 0

STR TP_ITER_strrSTR(TP_ITER self) {
 STR ret_val;
 FSTR s;
 ARRAYARG L11;
 ARG arg;
 STR L21;
 ARRAYBOOL L31;
 FSTR create_self;
 FSTR ret_val1;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 INT L41 = INT_zero;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val3;
 FSTR plus_self2;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val4;
 FSTR plus_self3;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self4;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val6;
 FSTR str_self;
 STR ret_val7;
 extern STR void1;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 extern STR ITER;
 FSTR L8;
 INT L9;
 OB L10;
 INT L12;
 extern STR ITER;
 BOOL L13;
 BOOL L141_;
 extern STR name3;
 INT L151_br;
 ARG aL151_;
 ARG L17;
 STR L18;
 dMODE L19;
 dTP L20;
 BOOL L22;
 BOOL L23;
 BOOL L241_;
 BOOL L25;
 INT L161_br;
INT i_L161_=0;
 BOOL aL161_;
 INT L261_;
 BOOL L27;
 BOOL L281_;
 FSTR L29;
 dTP L30;
 if ((self==((TP_ITER) NULL))) {
  ret_val = ((STR) &void1);
  return ret_val;
 }
 L5 = (ATTR(self,sas_cache)==((STR) NULL));
 L61_=!(L5); 
 if (L61_) {
 }
 else {
  if ((ATTR(self,args)==((ARRAYARG) NULL))) {
   L7 = (ATTR(self,ret)==((dTP) NULL));
  } else {
   L7 = FALSE;
  }
  if (L7) {
   SATTR(self,sas_cache,((STR) &ITER));
  }
  else {
   create_self = ((FSTR) NULL);
   L9 = 16;
   L12=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
   L10=ZALLOC_LEAF_BIG(L12);
   if (L10==NULL) FATAL("Unable to allocate more memory");
   memset(L10,0,L12);
   ((OB)L10)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L10)->header.destroyed=0;
#endif

   L8 = ((FSTR) L10);
   L8->asize = L9;
   ret_val1 = L8;
   s = FSTR_p1752847026(ret_val1, ((STR) &ITER));
   L13 = (ATTR(self,args)==((ARRAYARG) NULL));
   L141_=!(L13); 
   if (L141_) {
    plus_self = s;
    plus_c = '{';
    ret_val2 = FSTR_p399773021(plus_self, plus_c);
    s = ret_val2;
    {
     BOOL f_L151_ = TRUE;
     struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
     BOOL f_L161_ = TRUE;
     /* loop index variable */
     L41 = 0;
     L11 = ATTR(self,args);
     L21 = ((STR) &name3);
     L151_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
     noname1->self = L21;
     noname1->state = 0;
     while (1) {
      if(L41>=L151_br)  goto after_loop; 
      aL151_=ARR((ARRAYARG)L11,L41); 
      arg = aL151_;
      L19 = ATTR(arg,mode1);
      plus_self1 = (*dMODE_strrSTR[TAG(L19)])(L19);
      L20 = ATTR(arg,tp);
      plus_sarg = (*dTP_strrSTR[TAG(L20)])(L20);
      ret_val3 = STR_ap2004550586(plus_self1, plus_sarg);
      noname1->arg1 = ((dSTR) ret_val3);
      L18 = STR_se405450305(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      s = FSTR_p1752847026(s, L18);
      L23 = (ATTR(self,hot)==((ARRAYBOOL) NULL));
      L241_=!(L23); 
      if (L241_) {
       if (f_L161_) {
        f_L161_ = FALSE;
        L31 = ATTR(self,hot);
        L161_br=L31==NULL?0:ASIZE((ARRAYBOOL)L31); 
        i_L161_=0;
       }
       if(i_L161_>=L161_br)  goto after_loop; 
       aL161_=ARR((ARRAYBOOL)L31,i_L161_); i_L161_++;
       L22 = aL161_;
      } else {
       L22 = FALSE;
      }
      if (L22) {
       plus_self2 = s;
       plus_c1 = '!';
       ret_val4 = FSTR_p399773021(plus_self2, plus_c1);
       s = ret_val4;
      }
      L261_=INTPLUS(L41,1); 
      L41 = L261_;
     }
    }
    after_loop: ;
    plus_self3 = s;
    plus_c2 = '}';
    ret_val5 = FSTR_p399773021(plus_self3, plus_c2);
    s = ret_val5;
   }
   L27 = (ATTR(self,ret)==((dTP) NULL));
   L281_=!(L27); 
   if (L281_) {
    plus_self4 = s;
    plus_c3 = ':';
    ret_val6 = FSTR_p399773021(plus_self4, plus_c3);
    L29 = ret_val6;
    L30 = ATTR(self,ret);
    s = FSTR_p1752847026(L29, (*dTP_strrSTR[TAG(L30)])(L30));
   }
   str_self = s;
   ret_val7 = STR_fr1097270334(((STR) NULL), str_self);
   SATTR(self,sas_cache,ret_val7);
  }
 }
 ret_val = ATTR(self,sas_cache);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR TP_ROUT_strrSTR(TP_ROUT self) {
 STR ret_val;
 FSTR s = ((FSTR) NULL);
 ARRAYARG L11;
 ARG arg;
 STR L21;
 FSTR create_self;
 FSTR ret_val1;
 FSTR create_self1;
 FSTR ret_val2;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val3;
 INT L31 = INT_zero;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 FSTR plus_self2;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self3;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val6;
 FSTR str_self;
 STR ret_val7;
 extern STR void1;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 extern STR F_ROUT1;
 extern STR ROUT1;
 FSTR L7;
 INT L8;
 OB L9;
 INT L10;
 extern STR F_ROUT1;
 FSTR L12;
 INT L13;
 OB L14;
 INT L15;
 extern STR ROUT1;
 BOOL L16;
 BOOL L171_;
 extern STR name3;
 INT L181_br;
 ARG aL181_;
 ARG L19;
 STR L20;
 dMODE L22;
 dTP L23;
 INT L241_;
 BOOL L25;
 BOOL L261_;
 FSTR L27;
 dTP L28;
 if ((self==((TP_ROUT) NULL))) {
  ret_val = ((STR) &void1);
  return ret_val;
 }
 L4 = (ATTR(self,sas_cache)==((STR) NULL));
 L51_=!(L4); 
 if (L51_) {
 }
 else {
  if ((ATTR(self,args)==((ARRAYARG) NULL))) {
   L6 = (ATTR(self,ret)==((dTP) NULL));
  } else {
   L6 = FALSE;
  }
  if (L6) {
   if (ATTR(self,is_fortran)) {
    SATTR(self,sas_cache,((STR) &F_ROUT1));
   }
   else {
    SATTR(self,sas_cache,((STR) &ROUT1));
   }
  }
  else {
   if (ATTR(self,is_fortran)) {
    create_self = ((FSTR) NULL);
    L8 = 16;
    L10=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L8)*sizeof(CHAR);
    L9=ZALLOC_LEAF_BIG(L10);
    if (L9==NULL) FATAL("Unable to allocate more memory");
    memset(L9,0,L10);
    ((OB)L9)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

      ((OB)L9)->header.destroyed=0;
#endif

    L7 = ((FSTR) L9);
    L7->asize = L8;
    ret_val1 = L7;
    s = FSTR_p1752847026(ret_val1, ((STR) &F_ROUT1));
   }
   else {
    create_self1 = ((FSTR) NULL);
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
    ret_val2 = L12;
    s = FSTR_p1752847026(ret_val2, ((STR) &ROUT1));
   }
   L16 = (ATTR(self,args)==((ARRAYARG) NULL));
   L171_=!(L16); 
   if (L171_) {
    plus_self = s;
    plus_c = '{';
    ret_val3 = FSTR_p399773021(plus_self, plus_c);
    s = ret_val3;
    {
     BOOL f_L181_ = TRUE;
     struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
     /* loop index variable */
     L31 = 0;
     L11 = ATTR(self,args);
     L21 = ((STR) &name3);
     L181_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
     noname1->self = L21;
     noname1->state = 0;
     while (1) {
      if(L31>=L181_br)  goto after_loop; 
      aL181_=ARR((ARRAYARG)L11,L31); 
      arg = aL181_;
      L22 = ATTR(arg,mode1);
      plus_self1 = (*dMODE_strrSTR[TAG(L22)])(L22);
      L23 = ATTR(arg,tp);
      plus_sarg = (*dTP_strrSTR[TAG(L23)])(L23);
      ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
      noname1->arg1 = ((dSTR) ret_val4);
      L20 = STR_se405450305(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      s = FSTR_p1752847026(s, L20);
      L241_=INTPLUS(L31,1); 
      L31 = L241_;
     }
    }
    after_loop: ;
    plus_self2 = s;
    plus_c1 = '}';
    ret_val5 = FSTR_p399773021(plus_self2, plus_c1);
    s = ret_val5;
   }
   L25 = (ATTR(self,ret)==((dTP) NULL));
   L261_=!(L25); 
   if (L261_) {
    plus_self3 = s;
    plus_c2 = ':';
    ret_val6 = FSTR_p399773021(plus_self3, plus_c2);
    L27 = ret_val6;
    L28 = ATTR(self,ret);
    s = FSTR_p1752847026(L27, (*dTP_strrSTR[TAG(L28)])(L28));
   }
   str_self = s;
   ret_val7 = STR_fr1097270334(((STR) NULL), str_self);
   SATTR(self,sas_cache,ret_val7);
  }
 }
 ret_val = ATTR(self,sas_cache);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS TP_TBL1484824239(TP_TBL self, STR s, INT* i) {
 INT L1 = *i;/*Local for arg*/
 TP_CLASS ret_val;
 ARRAYdTP params = ((ARRAYdTP) NULL);
 STR cl = ((STR) NULL);
 INT j = INT_zero;
 ARRAYdTP p;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val4;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val5;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val6;
 STR r1;
 STR plus_self2;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg1;
 STR ret_val8;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val9;
 UNIX exit_self;
 INT exit_code = INT_zero;
 ARRAYdTP create_self2;
 INT create_n = INT_zero;
 ARRAYdTP ret_val10;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val12;
 STR plus_self7;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val13;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val14;
 STR create_self3;
 CHAR create_c2 = CHAR_zero;
 STR ret_val15;
 STR r2;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val16;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val17;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 ARRAYdTP create_self4;
 INT create_n1 = INT_zero;
 ARRAYdTP ret_val18;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val19;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val20;
 STR plus_self12;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val21;
 STR s4;
 CHAR str_self3 = CHAR_zero;
 STR ret_val22;
 STR create_self5;
 CHAR create_c3 = CHAR_zero;
 STR ret_val23;
 STR r3;
 STR plus_self13;
 STR plus_sarg9;
 STR ret_val24;
 PROG barf_self2;
 STR barf_msg2;
 PROG barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 STR plus_self14;
 STR plus_sarg10;
 STR ret_val25;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val26;
 CHAR L21_;
 CHAR L3;
 BOOL L41_;
 CHAR L51_;
 STR L6;
 INT L7;
 OB L8;
 INT L9;
 INT L111_;
 BOOL L12;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L16;
 BOOL L17;
 CHAR L181_;
 CHAR L191_;
 CHAR L20;
 BOOL L221_;
 CHAR L231_;
 CHAR L241_;
 CHAR L251_;
 STR L26;
 INT L27;
 OB L28;
 INT L29;
 INT L311_;
 BOOL L321_;
 extern STR syntax1919171187;
 extern STR name42;
 extern STR Intern28965746;
 INT L33;
 BOOL L341_;
 CHAR L351_;
 CHAR L36;
 BOOL L371_;
 INT L381_;
 ARRAYdTP L39;
 INT L40;
 OB L42;
 INT L43;
 INT* L44;
 TP_CLASS L45;
 TP_CLASS L46;
 INT L481_;
 CHAR L491_;
 CHAR L50;
 BOOL L521_;
 INT L531_;
 CHAR L541_;
 CHAR L55;
 BOOL L561_;
 INT L571_;
 extern STR syntax1919171187;
 extern STR expectedorgot;
 CHAR L581_;
 STR L59;
 INT L60;
 OB L61;
 INT L62;
 extern STR name42;
 extern STR Intern28965746;
 ARRAYdTP L64;
 INT L65;
 OB L66;
 INT L67;
 BOOL L69;
 CHAR L701_;
 CHAR L71;
 BOOL L721_;
 BOOL L73;
 BOOL L741_;
 CHAR L751_;
 CHAR L76;
 BOOL L771_;
 BOOL L78;
 BOOL L791_;
 extern STR syntax1919171187;
 extern STR expectedorgot1;
 CHAR L801_;
 STR L81;
 INT L82;
 OB L83;
 INT L84;
 extern STR name42;
 extern STR Intern28965746;
 L21_=ARR((STR)s,L1); 
 L3 = L21_;
 L41_=L3=='$'; 
 if (L41_) {
  plus_self = cl;
  L51_=ARR((STR)s,L1); 
  plus_arg = L51_;
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L7 = 1;
  L9=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L7)*sizeof(CHAR);
  L8=ZALLOC_LEAF_BIG(L9);
  if (L8==NULL) FATAL("Unable to allocate more memory");
  memset(L8,0,L9);
  ((OB)L8)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L8)->header.destroyed=0;
#endif

  L6 = ((STR) L8);
  L6->asize = L7;
  r = L6;
  SARR((STR)r,0,create_c); 
  ;
  ret_val3 = r;
  ret_val2 = ret_val3;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  cl = ret_val1;
  L111_=INTPLUS(L1,1); 
  L1 = L111_;
 }
 while (1) {
  L13 = STR_sizerINT(s);
  L141_=(L1)<(L13); 
  if (L141_) {
   L181_=ARR((STR)s,L1); 
   if (CHAR_i629101715(L181_)) {
    L17 = TRUE;
   } else {
    L191_=ARR((STR)s,L1); 
    L20 = L191_;
    L221_=L20=='_'; 
    L17 = L221_;
   }
   if (L17) {
    L16 = TRUE;
   } else {
    L231_=ARR((STR)s,L1); 
    L16 = CHAR_i1303832348(L231_);
   }
   if (L16) {
    L15 = TRUE;
   } else {
    L241_=ARR((STR)s,L1); 
    L15 = CHAR_i424896496(L241_);
   }
   L12 = L15;
  } else {
   L12 = FALSE;
  }
  if (L12) {
  }
  else {
   goto after_loop;
  }
  plus_self1 = cl;
  L251_=ARR((STR)s,L1); 
  plus_arg1 = L251_;
  str_self1 = plus_arg1;
  create_self1 = ((STR) NULL);
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
  ret_val6 = r1;
  ret_val5 = ret_val6;
  s2 = ret_val5;
  ret_val4 = STR_ap1077157958(plus_self1, s2, TRUE);
  cl = ret_val4;
  L311_=INTPLUS(L1,1); 
  L1 = L311_;
 }
 after_loop: ;
 L321_=(L1)==(0); 
 if (L321_) {
  barf_self = ATTR(self,prog);
  plus_self2 = ((STR) &syntax1919171187);
  plus_sarg = s;
  ret_val7 = STR_ap2004550586(plus_self2, plus_sarg);
  plus_self3 = ret_val7;
  plus_sarg1 = ((STR) &name42);
  ret_val8 = STR_ap2004550586(plus_self3, plus_sarg1);
  barf_msg = ret_val8;
  barf_at_self = barf_self;
  barf_at_msg = barf_msg;
  barf_at_at = ((dPROG_ERR) NULL);
  PROG_e176405615(barf_at_self, barf_at_at);
  plus_self4 = ((STR) &Intern28965746);
  plus_sarg2 = barf_at_msg;
  ret_val9 = STR_ap2004550586(plus_self4, plus_sarg2);
  PROG_err_STR(barf_at_self, ret_val9);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 L33 = STR_sizerINT(s);
 L341_=(L1)<(L33); 
 if (L341_) {
  L351_=ARR((STR)s,L1); 
  L36 = L351_;
  L371_=L36=='{'; 
  if (L371_) {
   L381_=INTPLUS(L1,1); 
   L1 = L381_;
   j = 0;
   create_self2 = ((ARRAYdTP) NULL);
   create_n = 50;
   L40 = create_n;
   L43=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L40)*sizeof(dTP);
   L42=ZALLOC_BIG(L43);
   if (L42==NULL) FATAL("Unable to allocate more memory");
   ((OB)L42)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

     ((OB)L42)->header.destroyed=0;
#endif

   L39 = ((ARRAYdTP) L42);
   L39->asize = L40;
   ret_val10 = L39;
   p = ret_val10;
   while (1) {
    L44 = &L1;
    L45 = TP_TBL1484824239(self, s, L44);
    L46 = L45;
    SARR((ARRAYdTP)p,j,(dTP)L46); 
    ;
    L481_=INTPLUS(j,1); 
    j = L481_;
    L491_=ARR((STR)s,L1); 
    L50 = L491_;
    L521_=L50=='}'; 
    if (L521_) {
     L531_=INTPLUS(L1,1); 
     L1 = L531_;
     goto after_loop1;
    }
    else {
     L541_=ARR((STR)s,L1); 
     L55 = L541_;
     L561_=L55==','; 
     if (L561_) {
      L571_=INTPLUS(L1,1); 
      L1 = L571_;
     }
     else {
      barf_self1 = ATTR(self,prog);
      plus_self5 = ((STR) &syntax1919171187);
      plus_sarg3 = s;
      ret_val11 = STR_ap2004550586(plus_self5, plus_sarg3);
      plus_self6 = ret_val11;
      plus_sarg4 = ((STR) &expectedorgot);
      ret_val12 = STR_ap2004550586(plus_self6, plus_sarg4);
      plus_self7 = ret_val12;
      L581_=ARR((STR)s,L1); 
      plus_arg2 = L581_;
      str_self2 = plus_arg2;
      create_self3 = ((STR) NULL);
      create_c2 = str_self2;
      L60 = 1;
      L62=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L60)*sizeof(CHAR);
      L61=ZALLOC_LEAF_BIG(L62);
      if (L61==NULL) FATAL("Unable to allocate more memory");
      memset(L61,0,L62);
      ((OB)L61)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L61)->header.destroyed=0;
#endif

      L59 = ((STR) L61);
      L59->asize = L60;
      r2 = L59;
      SARR((STR)r2,0,create_c2); 
      ;
      ret_val15 = r2;
      ret_val14 = ret_val15;
      s3 = ret_val14;
      ret_val13 = STR_ap1077157958(plus_self7, s3, TRUE);
      plus_self8 = ret_val13;
      plus_sarg5 = ((STR) &name42);
      ret_val16 = STR_ap2004550586(plus_self8, plus_sarg5);
      barf_msg1 = ret_val16;
      barf_at_self1 = barf_self1;
      barf_at_msg1 = barf_msg1;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self9 = ((STR) &Intern28965746);
      plus_sarg6 = barf_at_msg1;
      ret_val17 = STR_ap2004550586(plus_self9, plus_sarg6);
      PROG_err_STR(barf_at_self1, ret_val17);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
     }
    }
   }
   after_loop1: ;
   create_self4 = ((ARRAYdTP) NULL);
   create_n1 = j;
   L65 = create_n1;
   L67=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L65)*sizeof(dTP);
   L66=ZALLOC_BIG(L67);
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

     ((OB)L66)->header.destroyed=0;
#endif

   L64 = ((ARRAYdTP) L66);
   L64->asize = L65;
   ret_val18 = L64;
   params = ret_val18;
   if(params!=NULL && p!=NULL) AREFACOPY(params,p); 
   ;
  }
  else {
   L701_=ARR((STR)s,L1); 
   L71 = L701_;
   L721_=L71=='}'; 
   L73 = L721_;
   L741_=!(L73); 
   if (L741_) {
    L751_=ARR((STR)s,L1); 
    L76 = L751_;
    L771_=L76==','; 
    L78 = L771_;
    L791_=!(L78); 
    L69 = L791_;
   } else {
    L69 = FALSE;
   }
   if (L69) {
    barf_self2 = ATTR(self,prog);
    plus_self10 = ((STR) &syntax1919171187);
    plus_sarg7 = s;
    ret_val19 = STR_ap2004550586(plus_self10, plus_sarg7);
    plus_self11 = ret_val19;
    plus_sarg8 = ((STR) &expectedorgot1);
    ret_val20 = STR_ap2004550586(plus_self11, plus_sarg8);
    plus_self12 = ret_val20;
    L801_=ARR((STR)s,L1); 
    plus_arg3 = L801_;
    str_self3 = plus_arg3;
    create_self5 = ((STR) NULL);
    create_c3 = str_self3;
    L82 = 1;
    L84=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L82)*sizeof(CHAR);
    L83=ZALLOC_LEAF_BIG(L84);
    if (L83==NULL) FATAL("Unable to allocate more memory");
    memset(L83,0,L84);
    ((OB)L83)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L83)->header.destroyed=0;
#endif

    L81 = ((STR) L83);
    L81->asize = L82;
    r3 = L81;
    SARR((STR)r3,0,create_c3); 
    ;
    ret_val23 = r3;
    ret_val22 = ret_val23;
    s4 = ret_val22;
    ret_val21 = STR_ap1077157958(plus_self12, s4, TRUE);
    plus_self13 = ret_val21;
    plus_sarg9 = ((STR) &name42);
    ret_val24 = STR_ap2004550586(plus_self13, plus_sarg9);
    barf_msg2 = ret_val24;
    barf_at_self2 = barf_self2;
    barf_at_msg2 = barf_msg2;
    barf_at_at2 = ((dPROG_ERR) NULL);
    PROG_e176405615(barf_at_self2, barf_at_at2);
    plus_self14 = ((STR) &Intern28965746);
    plus_sarg10 = barf_at_msg2;
    ret_val25 = STR_ap2004550586(plus_self14, plus_sarg10);
    PROG_err_STR(barf_at_self2, ret_val25);
    exit_self2 = ((UNIX) NULL);
    exit_code2 = 1;
    exit(exit_code2);
   }
  }
 }
 tp_class_for_sel = self;
 tp_class_for_nam = IDENT_1150413730(IDENT_zero, cl);
 tp_class_for_par = params;
 ret_val26 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
 ret_val = ret_val26;
 *i = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_CLASS TP_TBL944008484(TP_TBL self, IDENT name111, ARRAYdTP params, BOOL memorize) {
 TP_CLASS ret_val;
 TUPIDENTARRAYdTP query1 = TUPIDENTARRAYdTP_zero;
 TP_CLASS r;
 TUPIDENTARRAYdTP create_self = TUPIDENTARRAYdTP_zero;
 IDENT create_at1 = IDENT_zero;
 ARRAYdTP create_at2;
 TUPIDENTARRAYdTP ret_val1 = TUPIDENTARRAYdTP_zero;
 TP_CLASS is_partial_self;
 BOOL ret_val2 = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val3 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val4;
 ERR1 create_self1;
 ERR1 ret_val5;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val6;
 FILE1 stderr_self;
 FILE1 ret_val7;
 FILE1 r1;
 ERR1 plus_self1;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val8;
 FILE1 r2;
 TUPIDENTARRAYdTP L1;
 TUPIDENTARRAYdTP L2;
 BOOL L3;
 BOOL L4;
 INT L5;
 INT L6;
 BOOL L71_;
 dPARSE L8;
 IDENT L9;
 ARRAYdTP L10;
 INT L111_;
 dPARSE L12;
 BOOL L13;
 BOOL L141_;
 INT L15;
 INT L16;
 BOOL L171_;
 extern STR Errorc1865369528;
 FILE1 L18;
 OB L19;
 FILE1 L21;
 OB L22;
 create_self = TUPIDENTARRAYdTP_zero;
 create_at1 = name111;
 create_at2 = params;
 L1 = create_self;
 L1.t1 = create_at1;
 L2 = L1;
 L2.t2 = create_at2;
 ret_val1 = L2;
 query1 = ret_val1;
 r = TP_CLA420352644(ATTR(self,class_tbl), query1);
 if ((r==((TP_CLASS) NULL))) {
  r = TP_CLA420352644(ATTR(self,included_classes), query1);
  if ((r==((TP_CLASS) NULL))) {
   r = TP_CLA1710809725(((TP_CLASS) NULL), name111, params, ATTR(self,prog));
   SATTR(self,included_classes,TP_CLA426779342(ATTR(self,included_classes), r));
  }
  if (ATTR(ATTR(self,prog),find_generics_st)) {
   is_partial_self = r;
   kind_self = is_partial_self;
   L5 = ATTR(kind_self,kind_cache);
   L6 = TP_KIN138039446;
   L71_=(L5)==(L6); 
   if (L71_) {
    as_tree_self = kind_self;
    if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L8 = ATTR(ATTR(as_tree_self,prog),parse);
     L9 = ATTR(as_tree_self,name1);
     L10 = ATTR(as_tree_self,params);
     L111_=(L10)==NULL?0:ASIZE((ARRAYdTP)L10); 
     L12 = L8;
     SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L12)])(L12, L9, L111_));
    }
    ret_val4 = ATTR(as_tree_self,my_as_tree);
    L13 = (ret_val4==((AS_CLASS_DEF) NULL));
    L141_=!(L13); 
    if (L141_) {
     SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
    }
   }
   ret_val3 = ATTR(kind_self,kind_cache);
   L15 = ret_val3;
   L16 = TP_KIND_part_tp;
   L171_=(L15)==(L16); 
   ret_val2 = L171_;
   L4 = ret_val2;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   L3 = memorize;
  } else {
   L3 = FALSE;
  }
  if (L3) {
   create_self1 = ((ERR1) NULL);
   ret_val5 = create_self1;
   plus_self = ret_val5;
   plus_s = ((dSTR) ((STR) &Errorc1865369528));
   stderr_self = ((FILE1) NULL);
   L19=ZALLOC(sizeof(struct FILE1_struct));
   if (L19==NULL) FATAL("Unable to allocate more memory");
   ((OB)L19)->header.tag=FILE1_tag;
   L18 = ((FILE1) L19);
   r1 = L18;
   
   SATTR(r1,fp,stderr);
   ret_val7 = r1;
   FILE_plus_dSTR(ret_val7, plus_s);
   ret_val6 = plus_self;
   plus_self1 = ret_val6;
   plus_s1 = ((dSTR) TP_CLASS_strrSTR(r));
   stderr_self1 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L21 = ((FILE1) L22);
   r2 = L21;
   
   SATTR(r2,fp,stderr);
   ret_val8 = r2;
   FILE_plus_dSTR(ret_val8, plus_s1);
  }
  if (memorize) {
   SATTR(self,class_tbl,TP_CLA426779342(ATTR(self,class_tbl), r));
  }
  TP_BUI717287368(((TP_BUILTIN) NULL), name111, r, params);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ITER TP_ITE1350567874(TP_ITER self, ARRAYARG args, ARRAYBOOL hot, dTP ret, PROG prog) {
 TP_ITER ret_val;
 TP_ITER r;
 ARG arg;
 ARRAYARG L11;
 INT L21 = INT_zero;
 TP_ITER L3;
 OB L4;
 INT L51_br;
 ARG aL51_;
 ARG L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 dTP L10;
 INT L121_;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 dTP L16;
 L4=ZALLOC(sizeof(struct TP_ITER_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=TP_ITER_tag;
 L3 = ((TP_ITER) L4);
 r = L3;
 SATTR(r,args,args);
 SATTR(r,hot,hot);
 SATTR(r,ret,ret);
 SATTR(r,prog,prog);
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = args;
  L51_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARG)L11,L21); 
   arg = aL51_;
   L8 = (arg==((ARG) NULL));
   L91_=!(L8); 
   if (L91_) {
    L10 = ATTR(arg,tp);
    L7 = (*dTP_is33989660[TAG(L10)])(L10);
   } else {
    L7 = FALSE;
   }
   if (L7) {
    SATTR(r,is_generic,TRUE);
    goto after_loop;
   }
   L121_=INTPLUS(L21,1); 
   L21 = L121_;
  }
 }
 after_loop: ;
 L14 = (ret==((dTP) NULL));
 L151_=!(L14); 
 if (L151_) {
  L16 = ret;
  L13 = (*dTP_is33989660[TAG(L16)])(L16);
 } else {
  L13 = FALSE;
 }
 if (L13) {
  SATTR(r,is_generic,TRUE);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ITER TP_ITE990128539(TP_ITER_TBL self, TUPARR1759987776 q) {
 TP_ITER ret_val;
 INT h = INT_zero;
 TP_ITER e;
 TP_ITER e1;
 TP_ITER_TBL is_elt_nil_self;
 TP_ITER is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self1;
 TP_ITER is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ITER_TBL is_elt_nil_self2;
 TP_ITER is_elt_nil_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self3;
 TP_ITER is_elt_nil_e3;
 BOOL ret_val4 = BOOL_zero;
 TP_ITER_TBL elt_nil_self;
 TP_ITER ret_val5;
 ELT_NILTP_ITER elt_nil_self1;
 TP_ITER ret_val6;
 TP_ITER e2 = ((TP_ITER) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_ITER L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_ITER L141_;
 INT L151_;
 if ((self==((TP_ITER_TBL) NULL))) {
  ret_val = ((TP_ITER) NULL);
  return ret_val;
 }
 L1 = TP_ITE513527302(self, q);
 L21_=ASIZE((TP_ITER_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_ITER)ARR((TP_ITER_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_ITER) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   if (TP_ITE226322531(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_ITER_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_ITER)ARR((TP_ITER_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NILTP_ITER) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val4 = (is_elt_nil_e3==((TP_ITER) NULL));
   ret_val3 = ret_val4;
   if (ret_val3) {
    goto after_loop1;
   }
   else {
    if (TP_ITE226322531(self, q, e1)) {
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
 elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
 ret_val6 = ((TP_ITER) NULL);
 ret_val5 = ret_val6;
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ITER_TBL TP_ITE1565476760(TP_ITER_TBL self, INT n) {
 TP_ITER_TBL ret_val;
 TP_ITER_TBL r;
 TP_ITER_TBL L11;
 TP_ITER_TBL elt_nil_self;
 TP_ITER ret_val1;
 ELT_NILTP_ITER elt_nil_self1;
 TP_ITER ret_val2;
 TP_ITER e = ((TP_ITER) NULL);
 TP_ITER_TBL elt_nil_self2;
 TP_ITER ret_val3;
 ELT_NILTP_ITER elt_nil_self3;
 TP_ITER ret_val4;
 TP_ITER e1 = ((TP_ITER) NULL);
 INT L21 = INT_zero;
 TP_ITER_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TP_ITER L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct TP_ITER_TBL_struct)-sizeof(TP_ITER))+(L4)*sizeof(TP_ITER);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=TP_ITER_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((TP_ITER_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
 ret_val2 = ((TP_ITER) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((TP_ITER) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((TP_ITER_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTP_ITER) NULL);
    ret_val4 = ((TP_ITER) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((TP_ITER_TBL)L11,L21,(TP_ITER)L10); 
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

TP_ITER_TBL TP_ITE50234037(TP_ITER_TBL self) {
 TP_ITER_TBL ret_val;
 TP_ITER_TBL r;
 TP_ITER_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TP_ITER L9;
 L21_=ASIZE((TP_ITER_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = TP_ITE1565476760(self, L81_);
 {
  struct TP_ITE1090047011_frame_struct other1_0;
TP_ITE1090047011_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = TP_ITE1090047011(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = TP_ITE992470229(r, L9);
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

TP_ITER_TBL TP_ITE992470229(TP_ITER_TBL self, TP_ITER e) {
 TP_ITER_TBL ret_val;
 TP_ITER_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TP_ITER te;
 TP_ITER te1;
 TP_ITER_TBL is_elt_nil_self;
 TP_ITER is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self1;
 TP_ITER is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ITER_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 TP_ITER_TBL is_elt_nil_self2;
 TP_ITER is_elt_nil_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self3;
 TP_ITER is_elt_nil_e3;
 BOOL ret_val5 = BOOL_zero;
 TP_ITER_TBL elt_eq_self;
 TP_ITER elt_eq_e1;
 TP_ITER elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQTP_ITER elt_eq_self1;
 TP_ITER elt_eq_e11;
 TP_ITER elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 TP_ITER is_eq_self;
 OB is_eq_t;
 BOOL ret_val8 = BOOL_zero;
 TP_ITER_TBL is_elt_nil_self4;
 TP_ITER is_elt_nil_e4;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILTP_ITER is_elt_nil_self5;
 TP_ITER is_elt_nil_e5;
 BOOL ret_val10 = BOOL_zero;
 TP_ITER_TBL elt_eq_self2;
 TP_ITER elt_eq_e12;
 TP_ITER elt_eq_e22;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQTP_ITER elt_eq_self3;
 TP_ITER elt_eq_e13;
 TP_ITER elt_eq_e23;
 BOOL ret_val12 = BOOL_zero;
 TP_ITER is_eq_self1;
 OB is_eq_t1;
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
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 TP_ITER L161_;
 INT L181_;
 INT L191_;
 INT L20;
 BOOL L211_;
 TP_ITER L221_;
 INT L241_;
 INT L26;
 INT L271_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((TP_ITER) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((TP_ITER_TBL) NULL))) {
  r = TP_ITE1565476760(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((TP_ITER_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = TP_ITE34679094;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = TP_ITE50234037(r);
  }
 }
 L101_=ASIZE((TP_ITER_TBL)r); 
 asz = L101_;
 L12 = TP_ITE1218800034(r, e);
 L131_=INTMINUS(asz,2); 
 L14 = L131_;
 L151_=L12&L14; 
 orig_h = L151_;
 h = orig_h;
 while (1) {
  L161_=(TP_ITER)ARR((TP_ITER_TBL)r,h); 
  te = L161_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILTP_ITER) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val5 = (is_elt_nil_e3==((TP_ITER) NULL));
  ret_val4 = ret_val5;
  if (ret_val4) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_ITER) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val8 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val7 = ret_val8;
   ret_val6 = ret_val7;
   if (ret_val6) {
    SARR((TP_ITER_TBL)r,h,(TP_ITER)e); 
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
   L221_=(TP_ITER)ARR((TP_ITER_TBL)r,h); 
   te1 = L221_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILTP_ITER) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val10 = (is_elt_nil_e5==((TP_ITER) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_ITER) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val12 = ret_val13;
    ret_val11 = ret_val12;
    if (ret_val11) {
     SARR((TP_ITER_TBL)r,h,(TP_ITER)e); 
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
 SARR((TP_ITER_TBL)r,h,(TP_ITER)e); 
 ;
 L26 = ATTR(r,hsize);
 L271_=INTPLUS(L26,1); 
 SATTR(r,hsize,L271_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ROUT TP_ROU52430373(TP_ROUT self, ARRAYARG args, dTP ret, PROG prog) {
 TP_ROUT ret_val;
 TP_ROUT r;
 ARG arg;
 ARRAYARG L11;
 INT L21 = INT_zero;
 TP_ROUT L3;
 OB L4;
 INT L51_br;
 ARG aL51_;
 ARG L6;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 dTP L10;
 INT L121_;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 dTP L16;
 L4=ZALLOC(sizeof(struct TP_ROUT_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=TP_ROUT_tag;
 L3 = ((TP_ROUT) L4);
 r = L3;
 SATTR(r,args,args);
 SATTR(r,ret,ret);
 SATTR(r,prog,prog);
 {
  BOOL f_L51_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = args;
  L51_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
  while (1) {
   if(L21>=L51_br)  goto after_loop; 
   aL51_=ARR((ARRAYARG)L11,L21); 
   arg = aL51_;
   L8 = (arg==((ARG) NULL));
   L91_=!(L8); 
   if (L91_) {
    L10 = ATTR(arg,tp);
    L7 = (*dTP_is33989660[TAG(L10)])(L10);
   } else {
    L7 = FALSE;
   }
   if (L7) {
    SATTR(r,is_generic,TRUE);
    goto after_loop;
   }
   L121_=INTPLUS(L21,1); 
   L21 = L121_;
  }
 }
 after_loop: ;
 L14 = (ret==((dTP) NULL));
 L151_=!(L14); 
 if (L151_) {
  L16 = ret;
  L13 = (*dTP_is33989660[TAG(L16)])(L16);
 } else {
  L13 = FALSE;
 }
 if (L13) {
  SATTR(r,is_generic,TRUE);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ROUT TP_ROU581494640(TP_ROUT_TBL self, TUPARRAYARGdTP q) {
 TP_ROUT ret_val;
 INT h = INT_zero;
 TP_ROUT e;
 TP_ROUT e1;
 TP_ROUT_TBL is_elt_nil_self;
 TP_ROUT is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self1;
 TP_ROUT is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ROUT_TBL is_elt_nil_self2;
 TP_ROUT is_elt_nil_e2;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self3;
 TP_ROUT is_elt_nil_e3;
 BOOL ret_val4 = BOOL_zero;
 TP_ROUT_TBL elt_nil_self;
 TP_ROUT ret_val5;
 ELT_NILTP_ROUT elt_nil_self1;
 TP_ROUT ret_val6;
 TP_ROUT e2 = ((TP_ROUT) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TP_ROUT L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 TP_ROUT L141_;
 INT L151_;
 if ((self==((TP_ROUT_TBL) NULL))) {
  ret_val = ((TP_ROUT) NULL);
  return ret_val;
 }
 L1 = TP_ROU2036003026(self, q);
 L21_=ASIZE((TP_ROUT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(TP_ROUT)ARR((TP_ROUT_TBL)self,h); 
  e = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val2 = (is_elt_nil_e1==((TP_ROUT) NULL));
  ret_val1 = ret_val2;
  if (ret_val1) {
   goto after_loop;
  }
  else {
   if (TP_ROU1936415536(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((TP_ROUT_TBL)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(TP_ROUT)ARR((TP_ROUT_TBL)self,h); 
   e1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NILTP_ROUT) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val4 = (is_elt_nil_e3==((TP_ROUT) NULL));
   ret_val3 = ret_val4;
   if (ret_val3) {
    goto after_loop1;
   }
   else {
    if (TP_ROU1936415536(self, q, e1)) {
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
 elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
 ret_val6 = ((TP_ROUT) NULL);
 ret_val5 = ret_val6;
 ret_val = ret_val5;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ROUT_TBL TP_ROU1247655203(TP_ROUT_TBL self, TP_ROUT e) {
 TP_ROUT_TBL ret_val;
 TP_ROUT_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 TP_ROUT te;
 TP_ROUT te1;
 TP_ROUT_TBL is_elt_nil_self;
 TP_ROUT is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self1;
 TP_ROUT is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 TP_ROUT_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 TP_ROUT_TBL is_elt_nil_self2;
 TP_ROUT is_elt_nil_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self3;
 TP_ROUT is_elt_nil_e3;
 BOOL ret_val5 = BOOL_zero;
 TP_ROUT_TBL elt_eq_self;
 TP_ROUT elt_eq_e1;
 TP_ROUT elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_EQTP_ROUT elt_eq_self1;
 TP_ROUT elt_eq_e11;
 TP_ROUT elt_eq_e21;
 BOOL ret_val7 = BOOL_zero;
 TP_ROUT is_eq_self;
 OB is_eq_t;
 BOOL ret_val8 = BOOL_zero;
 TP_ROUT_TBL is_elt_nil_self4;
 TP_ROUT is_elt_nil_e4;
 BOOL ret_val9 = BOOL_zero;
 ELT_NILTP_ROUT is_elt_nil_self5;
 TP_ROUT is_elt_nil_e5;
 BOOL ret_val10 = BOOL_zero;
 TP_ROUT_TBL elt_eq_self2;
 TP_ROUT elt_eq_e12;
 TP_ROUT elt_eq_e22;
 BOOL ret_val11 = BOOL_zero;
 ELT_EQTP_ROUT elt_eq_self3;
 TP_ROUT elt_eq_e13;
 TP_ROUT elt_eq_e23;
 BOOL ret_val12 = BOOL_zero;
 TP_ROUT is_eq_self1;
 OB is_eq_t1;
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
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 TP_ROUT L161_;
 INT L181_;
 INT L191_;
 INT L20;
 BOOL L211_;
 TP_ROUT L221_;
 INT L241_;
 INT L26;
 INT L271_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((TP_ROUT) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((TP_ROUT_TBL) NULL))) {
  r = TP_ROU472682568(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((TP_ROUT_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = TP_ROU558963694;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = TP_ROU1415769243(r);
  }
 }
 L101_=ASIZE((TP_ROUT_TBL)r); 
 asz = L101_;
 L12 = TP_ROU1653973250(r, e);
 L131_=INTMINUS(asz,2); 
 L14 = L131_;
 L151_=L12&L14; 
 orig_h = L151_;
 h = orig_h;
 while (1) {
  L161_=(TP_ROUT)ARR((TP_ROUT_TBL)r,h); 
  te = L161_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NILTP_ROUT) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val5 = (is_elt_nil_e3==((TP_ROUT) NULL));
  ret_val4 = ret_val5;
  if (ret_val4) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQTP_ROUT) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_t = ((OB) elt_eq_e21);
   ret_val8 = SYSOBEQ(((OB) is_eq_self),is_eq_t);
   ret_val7 = ret_val8;
   ret_val6 = ret_val7;
   if (ret_val6) {
    SARR((TP_ROUT_TBL)r,h,(TP_ROUT)e); 
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
   L221_=(TP_ROUT)ARR((TP_ROUT_TBL)r,h); 
   te1 = L221_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NILTP_ROUT) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val10 = (is_elt_nil_e5==((TP_ROUT) NULL));
   ret_val9 = ret_val10;
   if (ret_val9) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQTP_ROUT) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_t1 = ((OB) elt_eq_e23);
    ret_val13 = SYSOBEQ(((OB) is_eq_self1),is_eq_t1);
    ret_val12 = ret_val13;
    ret_val11 = ret_val12;
    if (ret_val11) {
     SARR((TP_ROUT_TBL)r,h,(TP_ROUT)e); 
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
 SARR((TP_ROUT_TBL)r,h,(TP_ROUT)e); 
 ;
 L26 = ATTR(r,hsize);
 L271_=INTPLUS(L26,1); 
 SATTR(r,hsize,L271_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TP_ROUT_TBL TP_ROU1415769243(TP_ROUT_TBL self) {
 TP_ROUT_TBL ret_val;
 TP_ROUT_TBL r;
 TP_ROUT_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TP_ROUT L9;
 L21_=ASIZE((TP_ROUT_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = TP_ROU472682568(self, L81_);
 {
  struct TP_ROU1377082179_frame_struct other1_0;
TP_ROU1377082179_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = TP_ROU1377082179(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = TP_ROU1247655203(r, L9);
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

TP_ROUT_TBL TP_ROU472682568(TP_ROUT_TBL self, INT n) {
 TP_ROUT_TBL ret_val;
 TP_ROUT_TBL r;
 TP_ROUT_TBL L11;
 TP_ROUT_TBL elt_nil_self;
 TP_ROUT ret_val1;
 ELT_NILTP_ROUT elt_nil_self1;
 TP_ROUT ret_val2;
 TP_ROUT e = ((TP_ROUT) NULL);
 TP_ROUT_TBL elt_nil_self2;
 TP_ROUT ret_val3;
 ELT_NILTP_ROUT elt_nil_self3;
 TP_ROUT ret_val4;
 TP_ROUT e1 = ((TP_ROUT) NULL);
 INT L21 = INT_zero;
 TP_ROUT_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TP_ROUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct TP_ROUT_TBL_struct)-sizeof(TP_ROUT))+(L4)*sizeof(TP_ROUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=TP_ROUT_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((TP_ROUT_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
 ret_val2 = ((TP_ROUT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((TP_ROUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((TP_ROUT_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NILTP_ROUT) NULL);
    ret_val4 = ((TP_ROUT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((TP_ROUT_TBL)L11,L21,(TP_ROUT)L10); 
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

void TP_TBL1525652988(TP_TBL self, IDENT name111, INT p) {
 TP_CLASS r;
 AS_CLASS_DEF as;
 STR name_suffix;
 ARRAYdTP param_types;
 ARRAYIDENT param_names;
 AS_PARAM_DEC param1;
 AS_CLASS_DEF as_classdef;
 TP_CLASS dummy_class;
 ARRAYIDENT L11;
 ARRAYdTP L21;
 INT L31 = INT_zero;
 ARRAYIDENT dc_names;
 INT L41 = INT_zero;
 ARRAYdTP dc_types;
 ARRAYIDENT L51;
 ARRAYIDENT L61;
 ARRAYdTP L71;
 ARRAYdTP L81;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val = TUPIDENTINT_zero;
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
 INT plus_arg = INT_zero;
 STR ret_val4;
 STR s;
 INT str_self = INT_zero;
 STR ret_val5;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val6;
 FSTR str_self1;
 STR ret_val7;
 ARRAYdTP create_self1;
 INT create_n = INT_zero;
 ARRAYdTP ret_val8;
 ARRAYIDENT create_self2;
 INT create_n1 = INT_zero;
 ARRAYIDENT ret_val9;
 AS_CLASS_DEF create_self3;
 AS_CLASS_DEF ret_val10;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val11;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val12;
 TP_CLASS create_dummyclas;
 AS_CLASS_DEF create_dummyclas1;
 PROG create_dummyclas2;
 TP_CLASS ret_val13;
 TP_CLASS res;
 ERR1 create_self4;
 ERR1 ret_val14;
 ERR1 plus_self6;
 dSTR plus_s;
 ERR1 ret_val15;
 FILE1 stderr_self;
 FILE1 ret_val16;
 FILE1 r1;
 ERR1 plus_self7;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val17;
 FILE1 r2;
 ARRAYIDENT create_self5;
 INT create_n2 = INT_zero;
 ARRAYIDENT ret_val18;
 ARRAYdTP create_self6;
 INT create_n3 = INT_zero;
 ARRAYdTP ret_val19;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 TP_CONTEXT create_self7;
 TP_CLASS create_same;
 ARRAYIDENT create_pnames;
 ARRAYdTP create_ptypes;
 PROG create_prog;
 TP_CONTEXT ret_val20;
 TP_CONTEXT r3;
 TP_CLASS set_context_self;
 TP_CONTEXT set_context_con;
 TUPIDENTINT L12;
 TUPIDENTINT L13;
 BOOL L14;
 BOOL L151_;
 dPARSE L16;
 CHAR L171_;
 CHAR L18;
 BOOL L191_;
 extern STR p1;
 extern STR in2;
 extern STR p1;
 BOOL L20;
 BOOL L221_;
 ARRAYdTP L23;
 INT L24;
 OB L25;
 INT L26;
 ARRAYIDENT L27;
 INT L28;
 OB L29;
 INT L30;
 INT L321_br;
INT i_L321_=0;
 INT L331_br;
INT i_L331_=0;
 INT L341_;
 INT L351_;
 AS_CLASS_DEF L36;
 OB L37;
 extern STR d1;
 extern STR Creating;
 FILE1 L38;
 OB L39;
 FILE1 L42;
 OB L43;
 IDENT L45;
 INT L46;
 ARRAYIDENT L47;
 INT L48;
 OB L49;
 INT L50;
 INT L52;
 ARRAYdTP L53;
 INT L54;
 OB L55;
 INT L56;
 INT L571_br;
 IDENT aL571_;
 INT L581_br;
 IDENT L59;
 IDENT L60;
 INT L621_;
 INT L631_br;
 dTP aL631_;
 INT L641_br;
 dTP L65;
 dTP L66;
 INT L671_;
 TP_CONTEXT L68;
 OB L69;
 create_self = TUPIDENTINT_zero;
 create_at1 = name111;
 create_at2 = p;
 L12 = create_self;
 L12.t1 = create_at1;
 L13 = L12;
 L13.t2 = create_at2;
 ret_val = L13;
 r = TP_GEN1932307144(ATTR(self,gen_tbl), ret_val);
 L14 = (r==((TP_CLASS) NULL));
 L151_=!(L14); 
 if (L151_) {
  return;
 }
 L16 = ATTR(ATTR(self,prog),parse);
 as = (*dPARSE1581792481[TAG(L16)])(L16, name111, p);
 if ((as==((AS_CLASS_DEF) NULL))) {
  return;
 }
 name_suffix = name111.str;
 L171_=ARR((STR)name_suffix,0); 
 L18 = L171_;
 L191_=L18=='$'; 
 if (L191_) {
  plus_self = ((STR) &p1);
  plus_sarg = STR_su1627796404(name_suffix, 1);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  name_suffix = ret_val1;
 }
 plus_self1 = ((STR) &in2);
 plus_sarg1 = name_suffix;
 ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val2;
 plus_sarg2 = ((STR) &p1);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val3;
 plus_arg = p;
 str_self = plus_arg;
 clear_self = buf1;
 L20 = (clear_self==((FSTR) NULL));
 L221_=!(L20); 
 if (L221_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val6 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val6;
 str_self1 = buf1;
 ret_val7 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val5 = ret_val7;
 s = ret_val5;
 ret_val4 = STR_ap1077157958(plus_self3, s, TRUE);
 name_suffix = ret_val4;
 create_self1 = ((ARRAYdTP) NULL);
 create_n = p;
 L24 = create_n;
 L26=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L24)*sizeof(dTP);
 L25=ZALLOC_BIG(L26);
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L25)->header.destroyed=0;
#endif

 L23 = ((ARRAYdTP) L25);
 L23->asize = L24;
 ret_val8 = L23;
 param_types = ret_val8;
 create_self2 = ((ARRAYIDENT) NULL);
 create_n1 = p;
 L28 = create_n1;
 L30=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L28)*sizeof(IDENT);
 L29=ZALLOC_BIG(L30);
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

   ((OB)L29)->header.destroyed=0;
#endif

 L27 = ((ARRAYIDENT) L29);
 L27->asize = L28;
 ret_val9 = L27;
 param_names = ret_val9;
 param1 = ATTR(as,params);
 {
  BOOL f_L321_ = TRUE;
  BOOL f_L331_ = TRUE;
  BOOL f_L341_ = TRUE;
  BOOL f_L351_ = TRUE;
  L11 = param_names;
  L21 = param_types;
  L31 = 1;
  L41 = 1;
  L321_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
  i_L321_=0;
  L331_br=L21==NULL?0:ASIZE((ARRAYdTP)L21); 
  i_L331_=0;
  L341_=L31-1; 
  L351_=L41-1; 
  while (1) {
   if ((param1==((AS_PARAM_DEC) NULL))) {
    goto after_loop;
   }
   create_self3 = ((AS_CLASS_DEF) NULL);
   L37=ZALLOC(sizeof(struct AS_CLASS_DEF_struct));
   if (L37==NULL) FATAL("Unable to allocate more memory");
   ((OB)L37)->header.tag=AS_CLASS_DEF_tag;
   L36 = ((AS_CLASS_DEF) L37);
   ret_val10 = L36;
   as_classdef = ret_val10;
   plus_self4 = ((STR) &d1);
   plus_sarg3 = ATTR(param1,name1).str;
   ret_val11 = STR_ap2004550586(plus_self4, plus_sarg3);
   plus_self5 = ret_val11;
   plus_sarg4 = name_suffix;
   ret_val12 = STR_ap2004550586(plus_self5, plus_sarg4);
   SATTR(as_classdef,name1,IDENT_1150413730(IDENT_zero, ret_val12));
   SATTR(as_classdef,kind,TP_KIND_abs_tp);
   SATTR(as_classdef,under,ATTR(param1,type_constraint));
   SATTR(as_classdef,source1,ATTR(as,source1));
   create_dummyclas = ((TP_CLASS) NULL);
   create_dummyclas1 = as_classdef;
   create_dummyclas2 = ATTR(self,prog);
   if (ATTR(create_dummyclas2,trace_generics)) {
    create_self4 = ((ERR1) NULL);
    ret_val14 = create_self4;
    plus_self6 = ret_val14;
    plus_s = ((dSTR) ((STR) &Creating));
    stderr_self = ((FILE1) NULL);
    L39=ZALLOC(sizeof(struct FILE1_struct));
    if (L39==NULL) FATAL("Unable to allocate more memory");
    ((OB)L39)->header.tag=FILE1_tag;
    L38 = ((FILE1) L39);
    r1 = L38;
    
    SATTR(r1,fp,stderr);
    ret_val16 = r1;
    FILE_plus_dSTR(ret_val16, plus_s);
    ret_val15 = plus_self6;
    plus_self7 = ret_val15;
    plus_s1 = ((dSTR) ATTR(create_dummyclas1,name1).str);
    stderr_self1 = ((FILE1) NULL);
    L43=ZALLOC(sizeof(struct FILE1_struct));
    if (L43==NULL) FATAL("Unable to allocate more memory");
    ((OB)L43)->header.tag=FILE1_tag;
    L42 = ((FILE1) L43);
    r2 = L42;
    
    SATTR(r2,fp,stderr);
    ret_val17 = r2;
    FILE_plus_dSTR(ret_val17, plus_s1);
   }
   res = TP_CLA1710809725(create_dummyclas, ATTR(create_dummyclas1,name1), ((ARRAYdTP) NULL), create_dummyclas2);
   SATTR(res,my_as_tree,create_dummyclas1);
   SATTR(res,is_generic,TRUE);
   SATTR(res,is_dummy_class,TRUE);
   ret_val13 = res;
   dummy_class = ret_val13;
   if(i_L321_>=L321_br)  goto after_loop; 
   L45 = ATTR(param1,name1);
   SARR((ARRAYIDENT)L11,i_L321_,L45); i_L321_++;
   ;
   if(i_L331_>=L331_br)  goto after_loop; 
   SARR((ARRAYdTP)L21,i_L331_,(dTP)dummy_class); i_L331_++;
   ;
   create_self5 = ((ARRAYIDENT) NULL);
   L341_++; 
   create_n2 = L341_;
   L48 = create_n2;
   L50=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L48)*sizeof(IDENT);
   L49=ZALLOC_BIG(L50);
   if (L49==NULL) FATAL("Unable to allocate more memory");
   ((OB)L49)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

     ((OB)L49)->header.destroyed=0;
#endif

   L47 = ((ARRAYIDENT) L49);
   L47->asize = L48;
   ret_val18 = L47;
   dc_names = ret_val18;
   create_self6 = ((ARRAYdTP) NULL);
   L351_++; 
   create_n3 = L351_;
   L54 = create_n3;
   L56=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L54)*sizeof(dTP);
   L55=ZALLOC_BIG(L56);
   if (L55==NULL) FATAL("Unable to allocate more memory");
   ((OB)L55)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

     ((OB)L55)->header.destroyed=0;
#endif

   L53 = ((ARRAYdTP) L55);
   L53->asize = L54;
   ret_val19 = L53;
   dc_types = ret_val19;
   {
    BOOL f_L571_ = TRUE;
    BOOL f_L581_ = TRUE;
    /* loop index variable */
    L91 = 0;
    L51 = param_names;
    L61 = dc_names;
    L571_br=L51==NULL?0:ASIZE((ARRAYIDENT)L51); 
    L581_br=L61==NULL?0:ASIZE((ARRAYIDENT)L61); 
    while (1) {
     if(L91>=L581_br)  goto after_loop1; 
     if(L91>=L571_br)  goto after_loop1; 
     aL571_=ARR((ARRAYIDENT)L51,L91); 
     L60 = aL571_;
     SARR((ARRAYIDENT)L61,L91,L60); 
     ;
     L621_=INTPLUS(L91,1); 
     L91 = L621_;
    }
   }
   after_loop1: ;
   {
    BOOL f_L631_ = TRUE;
    BOOL f_L641_ = TRUE;
    /* loop index variable */
    L101 = 0;
    L71 = param_types;
    L81 = dc_types;
    L631_br=L71==NULL?0:ASIZE((ARRAYdTP)L71); 
    L641_br=L81==NULL?0:ASIZE((ARRAYdTP)L81); 
    while (1) {
     if(L101>=L641_br)  goto after_loop2; 
     if(L101>=L631_br)  goto after_loop2; 
     aL631_=ARR((ARRAYdTP)L71,L101); 
     L66 = aL631_;
     SARR((ARRAYdTP)L81,L101,(dTP)L66); 
     ;
     L671_=INTPLUS(L101,1); 
     L101 = L671_;
    }
   }
   after_loop2: ;
   set_context_self = dummy_class;
   create_self7 = ((TP_CONTEXT) NULL);
   create_same = dummy_class;
   create_pnames = dc_names;
   create_ptypes = dc_types;
   create_prog = ATTR(self,prog);
   L69=ZALLOC(sizeof(struct TP_CONTEXT_struct));
   if (L69==NULL) FATAL("Unable to allocate more memory");
   ((OB)L69)->header.tag=TP_CONTEXT_tag;
   L68 = ((TP_CONTEXT) L69);
   r3 = L68;
   SATTR(r3,same1,create_same);
   SATTR(r3,pnames,create_pnames);
   SATTR(r3,ptypes,create_ptypes);
   SATTR(r3,prog,create_prog);
   ret_val20 = r3;
   set_context_con = ret_val20;
   SATTR(set_context_self,my_context,set_context_con);
   SATTR(self,gen_tbl,TP_GEN1370131280(ATTR(self,gen_tbl), dummy_class));
   param1 = ATTR(param1,next);
  }
 }
 after_loop: ;
 r = TP_CLA1710809725(((TP_CLASS) NULL), name111, param_types, ATTR(self,prog));
 SATTR(self,gen_tbl,TP_GEN1370131280(ATTR(self,gen_tbl), r));
}


#undef IS_ITER
#define IS_ITER 1

TP_ITER TP_ITE1090047011(TP_ITE1090047011_frame frame) {
 TP_ITER dummy = ((TP_ITER) NULL);
 BOOL L1;
 BOOL L21_;
 TP_ITER L5;
 TP_ITER aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((TP_ITER_TBL) NULL));
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
     frame->L41_br=ASIZE((TP_ITER_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((TP_ITER_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILTP_ITER) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((TP_ITER) NULL));
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

TP_ITER TP_ITE948168378(TP_ITE948168378_frame frame) {
 TP_ITER dummy = ((TP_ITER) NULL);
 TP_ITER aI_u_I;
 TP_ITER L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_ITER_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((TP_ITER_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

TP_ROUT TP_ROU1377082179(TP_ROU1377082179_frame frame) {
 TP_ROUT dummy = ((TP_ROUT) NULL);
 BOOL L1;
 BOOL L21_;
 TP_ROUT L5;
 TP_ROUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((TP_ROUT_TBL) NULL));
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
     frame->L41_br=ASIZE((TP_ROUT_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((TP_ROUT_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NILTP_ROUT) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((TP_ROUT) NULL));
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

TP_ROUT TP_ROU2127468618(TP_ROU2127468618_frame frame) {
 TP_ROUT dummy = ((TP_ROUT) NULL);
 TP_ROUT aI_u_I;
 TP_ROUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_ROUT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((TP_ROUT_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

void TP_ITE1481975724(TP_ITE1481975724_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_ITER_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((TP_ITER_TBL)frame->self,frame->i_I_u_I,(TP_ITER)frame->arg1); frame->i_I_u_I++;
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

void TP_ROU1633691332(TP_ROU1633691332_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((TP_ROUT_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((TP_ROUT_TBL)frame->self,frame->i_I_u_I,(TP_ROUT)frame->arg1); frame->i_I_u_I++;
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
