#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

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

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct CHECK_AM_struct {/* layout for CHECK_AM */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_AM;

typedef struct CHECK_GENERICS_struct {/* layout for CHECK_GENERICS */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_GENERICS;

typedef struct CHECK_1174552075_struct {/* layout for CHECK_IFC_CONFORMANCE */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *CHECK_1174552075;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_NILSIG_struct {/* layout for ELT_NIL{SIG} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSIG;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct FILE_MANGLE_struct {/* layout for FILE_MANGLE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *FILE_MANGLE;

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

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

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

typedef struct TP_CLA115798516_frame_struct {
 INT state;
 TP_CLASS_TBL self;/* Formal argument: self */
 TP_CLASS ret_val2;
 TP_CLASS_TBL L61;
 TP_CLASS r;
 INT L31;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_CLA115798516_frame;

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

typedef struct TRANS_struct {/* layout for TRANS */
 OB_HEADER header;
 struct dAM_STMT_struct *cur_lock;
 struct dAM_STMT_struct *init_stmts;
 struct dTP_struct *ex_tp;
 struct AM_INV1167837230_struct *inv_stmt;
 struct AM_LOCAL_EXPR_struct *cur_cohort;
 struct AM_LOCAL_EXPR_struct *cur_par_ob;
 struct AM_LOCAL_EXPR_struct *cur_param_ob;
 struct AM_LOOP_STMT_struct *cur_loop;
 struct AM_PAR_STMT_struct *cur_simple_par;
 struct AM_POST_STMT_struct *post_stmt;
 struct AM_ROUT_DEF_struct *cur_rout;
 struct FLISTA725283029_struct *active_locals;
 struct IMPL_struct *impl;
 INT cur_yield_ind;
 struct PROG_struct *prog;
 struct SE_CONTEXT_struct *cur_se;
 struct TP_CONTEXT_struct *tp_con;
 BOOL chk_assert;
 BOOL chk_invariant;
 BOOL chk_post;
 BOOL chk_pre;
 BOOL in_constant;
 BOOL in_external;
 BOOL in_initial;
 BOOL in_invariant;
 BOOL in_post;
 BOOL in_pre;
 BOOL in_protect_body;
 BOOL in_protect_but_n;
 BOOL in_protect_then;
 BOOL in_simple_par;
 BOOL is_in_par;
 } *TRANS;

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

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

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

extern FSETSTR FILE_M683825354;
extern INT FILE_M852907408;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST void (**dBACK_1132541334)(dBACK_END, AM_ROUT_DEF);
AM_ROUT_DEF TRANS_503579921(TRANS, ELT);
BOOL CHAR_i1016724011(CHAR);
BOOL CHAR_i1237803373(CHAR, OB);
BOOL CHAR_i1303832348(CHAR);
BOOL CHAR_i1942379470(CHAR);
BOOL CHAR_i424896496(CHAR);
BOOL CHAR_i629101715(CHAR);
BOOL CHAR_i696824757(CHAR);
BOOL CHAR_i705101630(CHAR);
BOOL CHAR_i725352188(CHAR);
BOOL CHAR_i84735023(CHAR);
BOOL ELT_is1907475257(ELT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL TP_CLA1568240525(TP_CLASS);
CHAR CHAR_upperrCHAR(CHAR);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
ELT ELT_TB1949891380(ELT_TBL, SIG);
FLISTSIG IFC_no96952824(IFC, IFC);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETTP_CLASS FMAPTP2096448595(FMAPTP775297600, TP_CLASS);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
FSTR FSTR_p1410513982(FSTR, FSTR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
INT CHAR_a641490070(CHAR);
INT CHAR_d198943415(CHAR);
INT CHAR_h840494345(CHAR);
INT CHAR_hashrINT(CHAR);
INT CHAR_o1022220512(CHAR);
INT FLISTS228922656(FLISTSIG);
INT FSETdTP_sizerINT(FSETdTP);
INT FSTR_sizerINT(FSTR);
INT INT_hashrINT(INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_hashrINT(STR);
INT STR_sizerINT(STR);
SIG IFC_no472265690(IFC, IFC);
SIG SIG_TB788365833(SIG_TB788365833_frame);
SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame);
STR CHAR_prettyrSTR(CHAR);
STR CHAR_strrSTR(CHAR);
STR FILE_M1987416824(FILE_MANGLE, STR, dTP);
STR INT_oc2110318348(INT);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_se405450305(STR_se405450305_frame);
STR STR_tail_INTrSTR(STR, INT);
STR TP_CLASS_strrSTR(TP_CLASS);
STR TP_ITER_strrSTR(TP_ITER);
STR TP_ROUT_strrSTR(TP_ROUT);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TP_CLASS TP_CLA115798516(TP_CLA115798516_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void AM_OUT1439666796(AM_OUT, dAM);
void CHECK_1724422536(CHECK_1174552075);
void CHECK_397667670(CHECK_GENERICS, SIG);
void CHECK_452836548(CHECK_GENERICS);
void CHECK_801221867(CHECK_AM);
void CHECK_835201167(CHECK_AM, SIG);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void IFC_show(IFC);
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

BOOL CHAR_i1016724011(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case 'a': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'b': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'c': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'd': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'e': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'g': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'h': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'i': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'j': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'k': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'l': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'm': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'n': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'o': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'p': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'r': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 's': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 't': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'u': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'v': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'w': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'x': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'z': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'A': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'B': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'C': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'D': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'E': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'F': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'G': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'H': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'I': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'J': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'K': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'L': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'M': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'N': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'O': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'P': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'R': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'S': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'T': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'U': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'V': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'W': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'X': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Z': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i1237803373(CHAR self, OB arg) {
 BOOL ret_val = BOOL_zero;
 CHAR L1;
 BOOL L21_;
 if (arg==NULL) {
  goto other162;
 } else
 switch (TAG(arg)) {
  case CHAR_tag:
   L1 = ((CHAR_boxed) arg)->immutable_part;
   L21_=self==L1; 
   ret_val = L21_;
   return ret_val; break;
  default: ;
  other162: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i1303832348(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '1': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '2': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '3': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '4': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '5': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '6': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '7': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '8': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '9': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i1942379470(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case 'a': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'b': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'c': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'd': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'e': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'g': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'h': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'i': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'j': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'k': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'l': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'm': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'n': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'o': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'p': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'r': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 's': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 't': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'u': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'v': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'w': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'x': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'z': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'A': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'B': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'C': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'D': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'E': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'F': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'G': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'H': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'I': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'J': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'K': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'L': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'M': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'N': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'O': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'P': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'R': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'S': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'T': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'U': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'V': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'W': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'X': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Z': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '0': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '1': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '2': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '3': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '4': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '5': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '6': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '7': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '8': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '9': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i424896496(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case 'a': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'b': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'c': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'd': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'e': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'g': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'h': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'i': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'j': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'k': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'l': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'm': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'n': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'o': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'p': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'r': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 's': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 't': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'u': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'v': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'w': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'x': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'z': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i629101715(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case 'A': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'B': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'C': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'D': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'E': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'F': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'G': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'H': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'I': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'J': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'K': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'L': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'M': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'N': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'O': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'P': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'R': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'S': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'T': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'U': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'V': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'W': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'X': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Z': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i696824757(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '1': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '2': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '3': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '4': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '5': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '6': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '7': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i705101630(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '1': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '2': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '3': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '4': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '5': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '6': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '7': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '8': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '9': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'a': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'b': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'c': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'd': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'e': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'A': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'B': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'C': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'D': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'E': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'F': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i725352188(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case ' ': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'a': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'b': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'c': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'd': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'e': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'g': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'h': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'i': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'j': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'k': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'l': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'm': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'n': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'o': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'p': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'r': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 's': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 't': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'u': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'v': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'w': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'x': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'z': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'A': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'B': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'C': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'D': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'E': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'F': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'G': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'H': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'I': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'J': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'K': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'L': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'M': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'N': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'O': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'P': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Q': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'R': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'S': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'T': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'U': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'V': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'W': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'X': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Y': 
   ret_val = TRUE;
   return ret_val;
   break;
  case 'Z': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '0': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '1': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '2': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '3': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '4': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '5': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '6': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '7': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '8': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '9': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '!': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\"': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '#': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '$': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '%': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '&': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\'': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '(': 
   ret_val = TRUE;
   return ret_val;
   break;
  case ')': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '*': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '+': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '-': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '.': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '/': 
   ret_val = TRUE;
   return ret_val;
   break;
  case ':': 
   ret_val = TRUE;
   return ret_val;
   break;
  case ';': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '<': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '=': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '>': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '?': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '@': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '[': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\\': 
   ret_val = TRUE;
   return ret_val;
   break;
  case ']': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '^': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '_': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '`': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '{': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '|': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '}': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '~': 
   ret_val = TRUE;
   return ret_val;
   break;
  case ',': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL CHAR_i84735023(CHAR self) {
 BOOL ret_val = BOOL_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case ' ': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\f': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\n': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\r': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\t': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\v': 
   ret_val = TRUE;
   return ret_val;
   break;
  case '\32': 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CHAR CHAR_upperrCHAR(CHAR self) {
 CHAR ret_val = CHAR_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case 'a': 
   ret_val = 'A';
   return ret_val;
   break;
  case 'b': 
   ret_val = 'B';
   return ret_val;
   break;
  case 'c': 
   ret_val = 'C';
   return ret_val;
   break;
  case 'd': 
   ret_val = 'D';
   return ret_val;
   break;
  case 'e': 
   ret_val = 'E';
   return ret_val;
   break;
  case 'f': 
   ret_val = 'F';
   return ret_val;
   break;
  case 'g': 
   ret_val = 'G';
   return ret_val;
   break;
  case 'h': 
   ret_val = 'H';
   return ret_val;
   break;
  case 'i': 
   ret_val = 'I';
   return ret_val;
   break;
  case 'j': 
   ret_val = 'J';
   return ret_val;
   break;
  case 'k': 
   ret_val = 'K';
   return ret_val;
   break;
  case 'l': 
   ret_val = 'L';
   return ret_val;
   break;
  case 'm': 
   ret_val = 'M';
   return ret_val;
   break;
  case 'n': 
   ret_val = 'N';
   return ret_val;
   break;
  case 'o': 
   ret_val = 'O';
   return ret_val;
   break;
  case 'p': 
   ret_val = 'P';
   return ret_val;
   break;
  case 'q': 
   ret_val = 'Q';
   return ret_val;
   break;
  case 'r': 
   ret_val = 'R';
   return ret_val;
   break;
  case 's': 
   ret_val = 'S';
   return ret_val;
   break;
  case 't': 
   ret_val = 'T';
   return ret_val;
   break;
  case 'u': 
   ret_val = 'U';
   return ret_val;
   break;
  case 'v': 
   ret_val = 'V';
   return ret_val;
   break;
  case 'w': 
   ret_val = 'W';
   return ret_val;
   break;
  case 'x': 
   ret_val = 'X';
   return ret_val;
   break;
  case 'y': 
   ret_val = 'Y';
   return ret_val;
   break;
  case 'z': 
   ret_val = 'Z';
   return ret_val;
   break;
  default: ;
   ret_val = self;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT CHAR_a641490070(CHAR self) {
 INT ret_val = INT_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '\b': 
   ret_val = 8;
   return ret_val;
   break;
  case '\t': 
   ret_val = 9;
   return ret_val;
   break;
  case '\n': 
   ret_val = 10;
   return ret_val;
   break;
  case '\v': 
   ret_val = 11;
   return ret_val;
   break;
  case '\r': 
   ret_val = 13;
   return ret_val;
   break;
  case ' ': 
   ret_val = 32;
   return ret_val;
   break;
  case '!': 
   ret_val = 33;
   return ret_val;
   break;
  case '\"': 
   ret_val = 34;
   return ret_val;
   break;
  case '#': 
   ret_val = 35;
   return ret_val;
   break;
  case '$': 
   ret_val = 36;
   return ret_val;
   break;
  case '%': 
   ret_val = 37;
   return ret_val;
   break;
  case '&': 
   ret_val = 38;
   return ret_val;
   break;
  case '\'': 
   ret_val = 39;
   return ret_val;
   break;
  case '(': 
   ret_val = 40;
   return ret_val;
   break;
  case ')': 
   ret_val = 41;
   return ret_val;
   break;
  case '*': 
   ret_val = 42;
   return ret_val;
   break;
  case '+': 
   ret_val = 43;
   return ret_val;
   break;
  case ',': 
   ret_val = 44;
   return ret_val;
   break;
  case '-': 
   ret_val = 45;
   return ret_val;
   break;
  case '.': 
   ret_val = 46;
   return ret_val;
   break;
  case '/': 
   ret_val = 47;
   return ret_val;
   break;
  case '0': 
   ret_val = 48;
   return ret_val;
   break;
  case '1': 
   ret_val = 49;
   return ret_val;
   break;
  case '2': 
   ret_val = 50;
   return ret_val;
   break;
  case '3': 
   ret_val = 51;
   return ret_val;
   break;
  case '4': 
   ret_val = 52;
   return ret_val;
   break;
  case '5': 
   ret_val = 53;
   return ret_val;
   break;
  case '6': 
   ret_val = 54;
   return ret_val;
   break;
  case '7': 
   ret_val = 55;
   return ret_val;
   break;
  case '8': 
   ret_val = 56;
   return ret_val;
   break;
  case '9': 
   ret_val = 57;
   return ret_val;
   break;
  case ':': 
   ret_val = 58;
   return ret_val;
   break;
  case ';': 
   ret_val = 59;
   return ret_val;
   break;
  case '<': 
   ret_val = 60;
   return ret_val;
   break;
  case '=': 
   ret_val = 61;
   return ret_val;
   break;
  case '>': 
   ret_val = 62;
   return ret_val;
   break;
  case '?': 
   ret_val = 63;
   return ret_val;
   break;
  case '@': 
   ret_val = 64;
   return ret_val;
   break;
  case 'A': 
   ret_val = 65;
   return ret_val;
   break;
  case 'B': 
   ret_val = 66;
   return ret_val;
   break;
  case 'C': 
   ret_val = 67;
   return ret_val;
   break;
  case 'D': 
   ret_val = 68;
   return ret_val;
   break;
  case 'E': 
   ret_val = 69;
   return ret_val;
   break;
  case 'F': 
   ret_val = 70;
   return ret_val;
   break;
  case 'G': 
   ret_val = 71;
   return ret_val;
   break;
  case 'H': 
   ret_val = 72;
   return ret_val;
   break;
  case 'I': 
   ret_val = 73;
   return ret_val;
   break;
  case 'J': 
   ret_val = 74;
   return ret_val;
   break;
  case 'K': 
   ret_val = 75;
   return ret_val;
   break;
  case 'L': 
   ret_val = 76;
   return ret_val;
   break;
  case 'M': 
   ret_val = 77;
   return ret_val;
   break;
  case 'N': 
   ret_val = 78;
   return ret_val;
   break;
  case 'O': 
   ret_val = 79;
   return ret_val;
   break;
  case 'P': 
   ret_val = 80;
   return ret_val;
   break;
  case 'Q': 
   ret_val = 81;
   return ret_val;
   break;
  case 'R': 
   ret_val = 82;
   return ret_val;
   break;
  case 'S': 
   ret_val = 83;
   return ret_val;
   break;
  case 'T': 
   ret_val = 84;
   return ret_val;
   break;
  case 'U': 
   ret_val = 85;
   return ret_val;
   break;
  case 'V': 
   ret_val = 86;
   return ret_val;
   break;
  case 'W': 
   ret_val = 87;
   return ret_val;
   break;
  case 'X': 
   ret_val = 88;
   return ret_val;
   break;
  case 'Y': 
   ret_val = 89;
   return ret_val;
   break;
  case 'Z': 
   ret_val = 90;
   return ret_val;
   break;
  case '[': 
   ret_val = 91;
   return ret_val;
   break;
  case '\\': 
   ret_val = 92;
   return ret_val;
   break;
  case ']': 
   ret_val = 93;
   return ret_val;
   break;
  case '^': 
   ret_val = 94;
   return ret_val;
   break;
  case '_': 
   ret_val = 95;
   return ret_val;
   break;
  case '`': 
   ret_val = 96;
   return ret_val;
   break;
  case 'a': 
   ret_val = 97;
   return ret_val;
   break;
  case 'b': 
   ret_val = 98;
   return ret_val;
   break;
  case 'c': 
   ret_val = 99;
   return ret_val;
   break;
  case 'd': 
   ret_val = 100;
   return ret_val;
   break;
  case 'e': 
   ret_val = 101;
   return ret_val;
   break;
  case 'f': 
   ret_val = 102;
   return ret_val;
   break;
  case 'g': 
   ret_val = 103;
   return ret_val;
   break;
  case 'h': 
   ret_val = 104;
   return ret_val;
   break;
  case 'i': 
   ret_val = 105;
   return ret_val;
   break;
  case 'j': 
   ret_val = 106;
   return ret_val;
   break;
  case 'k': 
   ret_val = 107;
   return ret_val;
   break;
  case 'l': 
   ret_val = 108;
   return ret_val;
   break;
  case 'm': 
   ret_val = 109;
   return ret_val;
   break;
  case 'n': 
   ret_val = 110;
   return ret_val;
   break;
  case 'o': 
   ret_val = 111;
   return ret_val;
   break;
  case 'p': 
   ret_val = 112;
   return ret_val;
   break;
  case 'q': 
   ret_val = 113;
   return ret_val;
   break;
  case 'r': 
   ret_val = 114;
   return ret_val;
   break;
  case 's': 
   ret_val = 115;
   return ret_val;
   break;
  case 't': 
   ret_val = 116;
   return ret_val;
   break;
  case 'u': 
   ret_val = 117;
   return ret_val;
   break;
  case 'v': 
   ret_val = 118;
   return ret_val;
   break;
  case 'w': 
   ret_val = 119;
   return ret_val;
   break;
  case 'x': 
   ret_val = 120;
   return ret_val;
   break;
  case 'y': 
   ret_val = 121;
   return ret_val;
   break;
  case 'z': 
   ret_val = 122;
   return ret_val;
   break;
  case '{': 
   ret_val = 123;
   return ret_val;
   break;
  case '|': 
   ret_val = 124;
   return ret_val;
   break;
  case '}': 
   ret_val = 125;
   return ret_val;
   break;
  case '~': 
   ret_val = 126;
   return ret_val;
   break;
  default: ;
   ret_val = -1;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT CHAR_d198943415(CHAR self) {
 INT ret_val = INT_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = 0;
   return ret_val;
   break;
  case '1': 
   ret_val = 1;
   return ret_val;
   break;
  case '2': 
   ret_val = 2;
   return ret_val;
   break;
  case '3': 
   ret_val = 3;
   return ret_val;
   break;
  case '4': 
   ret_val = 4;
   return ret_val;
   break;
  case '5': 
   ret_val = 5;
   return ret_val;
   break;
  case '6': 
   ret_val = 6;
   return ret_val;
   break;
  case '7': 
   ret_val = 7;
   return ret_val;
   break;
  case '8': 
   ret_val = 8;
   return ret_val;
   break;
  case '9': 
   ret_val = 9;
   return ret_val;
   break;
  default: ;
   ret_val = -1;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT CHAR_h840494345(CHAR self) {
 INT ret_val = INT_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = 0;
   return ret_val;
   break;
  case '1': 
   ret_val = 1;
   return ret_val;
   break;
  case '2': 
   ret_val = 2;
   return ret_val;
   break;
  case '3': 
   ret_val = 3;
   return ret_val;
   break;
  case '4': 
   ret_val = 4;
   return ret_val;
   break;
  case '5': 
   ret_val = 5;
   return ret_val;
   break;
  case '6': 
   ret_val = 6;
   return ret_val;
   break;
  case '7': 
   ret_val = 7;
   return ret_val;
   break;
  case '8': 
   ret_val = 8;
   return ret_val;
   break;
  case '9': 
   ret_val = 9;
   return ret_val;
   break;
  case 'a': 
   ret_val = 10;
   return ret_val;
   break;
  case 'A': 
   ret_val = 10;
   return ret_val;
   break;
  case 'b': 
   ret_val = 11;
   return ret_val;
   break;
  case 'B': 
   ret_val = 11;
   return ret_val;
   break;
  case 'c': 
   ret_val = 12;
   return ret_val;
   break;
  case 'C': 
   ret_val = 12;
   return ret_val;
   break;
  case 'd': 
   ret_val = 13;
   return ret_val;
   break;
  case 'D': 
   ret_val = 13;
   return ret_val;
   break;
  case 'e': 
   ret_val = 14;
   return ret_val;
   break;
  case 'E': 
   ret_val = 14;
   return ret_val;
   break;
  case 'f': 
   ret_val = 15;
   return ret_val;
   break;
  case 'F': 
   ret_val = 15;
   return ret_val;
   break;
  default: ;
   ret_val = -1;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT CHAR_hashrINT(CHAR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=(INT)((unsigned char)self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT CHAR_o1022220512(CHAR self) {
 INT ret_val = INT_zero;
 CHAR L11 = CHAR_zero;
 L11 = self;
 switch (L11) {
  case '0': 
   ret_val = 0;
   return ret_val;
   break;
  case '1': 
   ret_val = 1;
   return ret_val;
   break;
  case '2': 
   ret_val = 2;
   return ret_val;
   break;
  case '3': 
   ret_val = 3;
   return ret_val;
   break;
  case '4': 
   ret_val = 4;
   return ret_val;
   break;
  case '5': 
   ret_val = 5;
   return ret_val;
   break;
  case '6': 
   ret_val = 6;
   return ret_val;
   break;
  case '7': 
   ret_val = 7;
   return ret_val;
   break;
  default: ;
   ret_val = -1;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CHAR_prettyrSTR(CHAR self) {
 STR ret_val;
 CHAR L11 = CHAR_zero;
 STR s;
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
 STR plus_sarg;
 STR ret_val4;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR name42;
 STR L10;
 INT L12;
 OB L13;
 INT L14;
 extern STR name42;
 extern STR a4;
 extern STR b11;
 extern STR f21;
 extern STR n1;
 extern STR r111;
 extern STR t21;
 extern STR v111;
 extern STR name43;
 extern STR name44;
 INT L161_;
 extern STR name45;
 INT L17;
 INT L181_;
 STR L19;
 extern STR name42;
 if (CHAR_i725352188(self)) {
  L41_=self=='\''; 
  L5 = L41_;
  L61_=!(L5); 
  L3 = L61_;
 } else {
  L3 = FALSE;
 }
 if (L3) {
  L71_=self=='\\'; 
  L8 = L71_;
  L91_=!(L8); 
  L2 = L91_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  plus_self = ((STR) &name42);
  plus_arg = self;
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L12 = 1;
  L14=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L12)*sizeof(CHAR);
  L13=ZALLOC_LEAF_BIG(L14);
  if (L13==NULL) FATAL("Unable to allocate more memory");
  memset(L13,0,L14);
  ((OB)L13)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L13)->header.destroyed=0;
#endif

  L10 = ((STR) L13);
  L10->asize = L12;
  r = L10;
  SARR((STR)r,0,create_c); 
  ;
  ret_val3 = r;
  ret_val2 = ret_val3;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  plus_self1 = ret_val1;
  plus_sarg = ((STR) &name42);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
  ret_val = ret_val4;
  return ret_val;
 }
 L11 = self;
 switch (L11) {
  case '\7': 
   ret_val = ((STR) &a4);
   return ret_val;
   break;
  case '\b': 
   ret_val = ((STR) &b11);
   return ret_val;
   break;
  case '\f': 
   ret_val = ((STR) &f21);
   return ret_val;
   break;
  case '\n': 
   ret_val = ((STR) &n1);
   return ret_val;
   break;
  case '\r': 
   ret_val = ((STR) &r111);
   return ret_val;
   break;
  case '\t': 
   ret_val = ((STR) &t21);
   return ret_val;
   break;
  case '\v': 
   ret_val = ((STR) &v111);
   return ret_val;
   break;
  case '\\': 
   ret_val = ((STR) &name43);
   return ret_val;
   break;
  case '\'': 
   ret_val = ((STR) &name44);
   return ret_val;
   break;
  default: ;
   L161_=(INT)((unsigned char)self); 
   s = INT_oc2110318348(L161_);
   L17 = STR_sizerINT(s);
   L181_=INTMINUS(L17,2); 
   L19 = STR_tail_INTrSTR(s, L181_);
   ret_val = STR_ap1693864410(((STR) &name45), L19, ((STR) &name42));
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CHAR_strrSTR(CHAR self) {
 STR ret_val;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val1;
 STR r;
 STR L1;
 INT L2;
 OB L3;
 INT L4;
 create_self = ((STR) NULL);
 create_c = self;
 L2 = 1;
 L4=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((STR) L3);
 L1->asize = L2;
 r = L1;
 SARR((STR)r,0,create_c); 
 ;
 ret_val1 = r;
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR FILE_M1987416824(FILE_MANGLE self, STR prefix1, dTP tp) {
 STR ret_val;
 STR class_name = ((STR) NULL);
 STR s;
 FSTR tmp1;
 STR L11;
 CHAR c = CHAR_zero;
 CHAR L21 = CHAR_zero;
 CHAR L31 = CHAR_zero;
 STR full_name;
 STR hash_str;
 STR res;
 INT L41 = INT_zero;
 INT i = INT_zero;
 FSTR create_self;
 FSTR ret_val1;
 FSTR plus_self;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 FSTR plus_self1;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val3;
 FSTR plus_self2;
 CHAR plus_c2 = CHAR_zero;
 FSTR ret_val4;
 FSTR plus_self3;
 CHAR plus_c3 = CHAR_zero;
 FSTR ret_val5;
 FSTR plus_self4;
 CHAR plus_c4 = CHAR_zero;
 FSTR ret_val6;
 FSTR plus_self5;
 CHAR plus_c5 = CHAR_zero;
 FSTR ret_val7;
 FSTR plus_self6;
 CHAR plus_c6 = CHAR_zero;
 FSTR ret_val8;
 FSTR plus_self7;
 CHAR plus_c7 = CHAR_zero;
 FSTR ret_val9;
 FSTR plus_self8;
 CHAR plus_c8 = CHAR_zero;
 FSTR ret_val10;
 FSTR plus_self9;
 CHAR plus_c9 = CHAR_zero;
 FSTR ret_val11;
 FSTR plus_self10;
 CHAR plus_c10 = CHAR_zero;
 FSTR ret_val12;
 FSTR plus_self11;
 CHAR plus_c11 = CHAR_zero;
 FSTR ret_val13;
 FSTR plus_self12;
 CHAR plus_c12 = CHAR_zero;
 FSTR ret_val14;
 FSTR plus_self13;
 CHAR plus_c13 = CHAR_zero;
 FSTR ret_val15;
 FSTR plus_self14;
 CHAR plus_c14 = CHAR_zero;
 FSTR ret_val16;
 FSTR plus_self15;
 CHAR plus_c15 = CHAR_zero;
 FSTR ret_val17;
 FSTR plus_self16;
 CHAR plus_c16 = CHAR_zero;
 FSTR ret_val18;
 FSTR plus_self17;
 CHAR plus_c17 = CHAR_zero;
 FSTR ret_val19;
 FSTR plus_self18;
 CHAR plus_c18 = CHAR_zero;
 FSTR ret_val20;
 FSTR plus_self19;
 CHAR plus_c19 = CHAR_zero;
 FSTR ret_val21;
 FSTR plus_self20;
 CHAR plus_c20 = CHAR_zero;
 FSTR ret_val22;
 FSTR plus_self21;
 CHAR plus_c21 = CHAR_zero;
 FSTR ret_val23;
 FSTR plus_self22;
 CHAR plus_c22 = CHAR_zero;
 FSTR ret_val24;
 FSTR plus_self23;
 CHAR plus_c23 = CHAR_zero;
 FSTR ret_val25;
 FSTR plus_self24;
 CHAR plus_c24 = CHAR_zero;
 FSTR ret_val26;
 FSTR plus_self25;
 CHAR plus_c25 = CHAR_zero;
 FSTR ret_val27;
 FSTR plus_self26;
 CHAR plus_c26 = CHAR_zero;
 FSTR ret_val28;
 FSTR plus_self27;
 CHAR plus_c27 = CHAR_zero;
 FSTR ret_val29;
 FSTR plus_self28;
 CHAR plus_c28 = CHAR_zero;
 FSTR ret_val30;
 FSTR plus_self29;
 CHAR plus_c29 = CHAR_zero;
 FSTR ret_val31;
 FSTR plus_self30;
 CHAR plus_c30 = CHAR_zero;
 FSTR ret_val32;
 FSTR plus_self31;
 CHAR plus_c31 = CHAR_zero;
 FSTR ret_val33;
 FSTR plus_self32;
 CHAR plus_c32 = CHAR_zero;
 FSTR ret_val34;
 FSTR plus_self33;
 CHAR plus_c33 = CHAR_zero;
 FSTR ret_val35;
 FSTR plus_self34;
 CHAR plus_c34 = CHAR_zero;
 FSTR ret_val36;
 FSTR plus_self35;
 CHAR plus_c35 = CHAR_zero;
 FSTR ret_val37;
 FSTR plus_self36;
 CHAR plus_c36 = CHAR_zero;
 FSTR ret_val38;
 FSTR plus_self37;
 CHAR plus_c37 = CHAR_zero;
 FSTR ret_val39;
 FSTR plus_self38;
 CHAR plus_c38 = CHAR_zero;
 FSTR ret_val40;
 FSTR plus_self39;
 CHAR plus_c39 = CHAR_zero;
 FSTR ret_val41;
 FSTR plus_self40;
 CHAR plus_c40 = CHAR_zero;
 FSTR ret_val42;
 FSTR plus_self41;
 CHAR plus_c41 = CHAR_zero;
 FSTR ret_val43;
 FSTR plus_self42;
 CHAR plus_c42 = CHAR_zero;
 FSTR ret_val44;
 FSTR plus_self43;
 CHAR plus_c43 = CHAR_zero;
 FSTR ret_val45;
 FSTR plus_self44;
 CHAR plus_c44 = CHAR_zero;
 FSTR ret_val46;
 FSTR plus_self45;
 CHAR plus_c45 = CHAR_zero;
 FSTR ret_val47;
 FSTR plus_self46;
 CHAR plus_c46 = CHAR_zero;
 FSTR ret_val48;
 FSTR plus_self47;
 CHAR plus_c47 = CHAR_zero;
 FSTR ret_val49;
 FSTR plus_self48;
 CHAR plus_c48 = CHAR_zero;
 FSTR ret_val50;
 FSTR plus_self49;
 CHAR plus_c49 = CHAR_zero;
 FSTR ret_val51;
 FSTR plus_self50;
 CHAR plus_c50 = CHAR_zero;
 FSTR ret_val52;
 FSTR plus_self51;
 CHAR plus_c51 = CHAR_zero;
 FSTR ret_val53;
 FSTR plus_self52;
 CHAR plus_c52 = CHAR_zero;
 FSTR ret_val54;
 FSTR plus_self53;
 CHAR plus_c53 = CHAR_zero;
 FSTR ret_val55;
 FSTR plus_self54;
 CHAR plus_c54 = CHAR_zero;
 FSTR ret_val56;
 FSTR plus_self55;
 CHAR plus_c55 = CHAR_zero;
 FSTR ret_val57;
 FSTR plus_self56;
 CHAR plus_c56 = CHAR_zero;
 FSTR ret_val58;
 FSTR plus_self57;
 CHAR plus_c57 = CHAR_zero;
 FSTR ret_val59;
 FSTR plus_self58;
 CHAR plus_c58 = CHAR_zero;
 FSTR ret_val60;
 FSTR plus_self59;
 CHAR plus_c59 = CHAR_zero;
 FSTR ret_val61;
 FSTR plus_self60;
 CHAR plus_c60 = CHAR_zero;
 FSTR ret_val62;
 FSTR plus_self61;
 CHAR plus_c61 = CHAR_zero;
 FSTR ret_val63;
 FSTR plus_self62;
 CHAR plus_c62 = CHAR_zero;
 FSTR ret_val64;
 FSTR plus_self63;
 CHAR plus_c63 = CHAR_zero;
 FSTR ret_val65;
 FSTR plus_self64;
 CHAR plus_c64 = CHAR_zero;
 FSTR ret_val66;
 FSTR length_self;
 INT ret_val67 = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val68 = CHAR_zero;
 FSTR create_self1;
 FSTR ret_val69;
 FSTR create_self2;
 FSTR ret_val70;
 FSTR create_self3;
 FSTR ret_val71;
 FSTR create_self4;
 FSTR ret_val72;
 FSTR create_self5;
 FSTR ret_val73;
 FSTR create_self6;
 FSTR ret_val74;
 FSTR create_self7;
 FSTR ret_val75;
 FSTR create_self8;
 FSTR ret_val76;
 FSTR create_self9;
 FSTR ret_val77;
 FSTR create_self10;
 FSTR ret_val78;
 FSTR create_self11;
 FSTR ret_val79;
 FSTR str_self;
 STR ret_val80;
 STR plus_self65;
 STR plus_sarg;
 STR ret_val81;
 INT str_self1 = INT_zero;
 STR ret_val82;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val83;
 FSTR str_self2;
 STR ret_val84;
 STR plus_self66;
 STR plus_sarg1;
 STR ret_val85;
 INT str_self3 = INT_zero;
 STR ret_val86;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val87;
 FSTR str_self4;
 STR ret_val88;
 STR plus_self67;
 STR plus_sarg2;
 STR ret_val89;
 extern STR INTERN350222499;
 FSTR L5;
 INT L6;
 OB L7;
 INT L8;
 CHAR L9;
 INT L10;
 BOOL L121_;
 extern STR name74;
 CHAR L131_;
 FSTR L14;
 INT L15;
 OB L16;
 INT L17;
 extern STR S1;
 FSTR L18;
 FSTR L19;
 INT L20;
 OB L22;
 INT L23;
 extern STR S1;
 FSTR L24;
 FSTR L25;
 INT L26;
 OB L27;
 INT L28;
 extern STR S1;
 FSTR L29;
 FSTR L30;
 INT L32;
 OB L33;
 INT L34;
 extern STR S1;
 FSTR L35;
 FSTR L36;
 INT L37;
 OB L38;
 INT L39;
 extern STR S1;
 FSTR L40;
 FSTR L42;
 INT L43;
 OB L44;
 INT L45;
 extern STR S1;
 FSTR L46;
 FSTR L47;
 INT L48;
 OB L49;
 INT L50;
 extern STR S1;
 FSTR L51;
 FSTR L52;
 INT L53;
 OB L54;
 INT L55;
 extern STR S1;
 FSTR L56;
 FSTR L57;
 INT L58;
 OB L59;
 INT L60;
 extern STR S1;
 FSTR L61;
 FSTR L62;
 INT L63;
 OB L64;
 INT L65;
 extern STR S1;
 FSTR L66;
 FSTR L67;
 INT L68;
 OB L69;
 INT L70;
 extern STR S1;
 FSTR L71;
 INT L72;
 INT L731_;
 BOOL L74;
 BOOL L751_;
 INT L76;
 INT L771_;
 INT L78;
 INT L79;
 BOOL L801_;
 INT L81;
 INT L821_;
 INT L831_;
 INT L84;
 BOOL L85;
 BOOL L861_;
 BOOL L871_;
 extern STR Intern1789924828;
 if (tp==NULL) {
  goto other358;
 } else
 switch (TAG(tp)) {
  case TP_ROUT_tag:
   class_name = TP_ROUT_strrSTR(((TP_ROUT) tp)); break;
  case TP_ITER_tag:
   class_name = TP_ITER_strrSTR(((TP_ITER) tp)); break;
  case TP_CLASS_tag:
   class_name = TP_CLASS_strrSTR(((TP_CLASS) tp)); break;
  default: ;
  other358: ;
   RAISE(((OB) ((STR) &INTERN350222499)));
 }
 s = class_name;
 create_self = ((FSTR) NULL);
 L6 = 16;
 L8=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L6)*sizeof(CHAR);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((FSTR) L7);
 L5->asize = L6;
 ret_val1 = L5;
 tmp1 = ret_val1;
 {
  struct STR_eltbrCHAR_frame_struct other1_0;
STR_eltbrCHAR_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = STR_eltbrCHAR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L9;
   L21 = c;
   switch (L21) {
    case 'a': 
     plus_self = tmp1;
     plus_c = c;
     ret_val2 = FSTR_p399773021(plus_self, plus_c);
     tmp1 = ret_val2;
     break;
    case 'b': 
     plus_self1 = tmp1;
     plus_c1 = c;
     ret_val3 = FSTR_p399773021(plus_self1, plus_c1);
     tmp1 = ret_val3;
     break;
    case 'c': 
     plus_self2 = tmp1;
     plus_c2 = c;
     ret_val4 = FSTR_p399773021(plus_self2, plus_c2);
     tmp1 = ret_val4;
     break;
    case 'd': 
     plus_self3 = tmp1;
     plus_c3 = c;
     ret_val5 = FSTR_p399773021(plus_self3, plus_c3);
     tmp1 = ret_val5;
     break;
    case 'e': 
     plus_self4 = tmp1;
     plus_c4 = c;
     ret_val6 = FSTR_p399773021(plus_self4, plus_c4);
     tmp1 = ret_val6;
     break;
    case 'f': 
     plus_self5 = tmp1;
     plus_c5 = c;
     ret_val7 = FSTR_p399773021(plus_self5, plus_c5);
     tmp1 = ret_val7;
     break;
    case 'g': 
     plus_self6 = tmp1;
     plus_c6 = c;
     ret_val8 = FSTR_p399773021(plus_self6, plus_c6);
     tmp1 = ret_val8;
     break;
    case 'h': 
     plus_self7 = tmp1;
     plus_c7 = c;
     ret_val9 = FSTR_p399773021(plus_self7, plus_c7);
     tmp1 = ret_val9;
     break;
    case 'i': 
     plus_self8 = tmp1;
     plus_c8 = c;
     ret_val10 = FSTR_p399773021(plus_self8, plus_c8);
     tmp1 = ret_val10;
     break;
    case 'j': 
     plus_self9 = tmp1;
     plus_c9 = c;
     ret_val11 = FSTR_p399773021(plus_self9, plus_c9);
     tmp1 = ret_val11;
     break;
    case 'k': 
     plus_self10 = tmp1;
     plus_c10 = c;
     ret_val12 = FSTR_p399773021(plus_self10, plus_c10);
     tmp1 = ret_val12;
     break;
    case 'l': 
     plus_self11 = tmp1;
     plus_c11 = c;
     ret_val13 = FSTR_p399773021(plus_self11, plus_c11);
     tmp1 = ret_val13;
     break;
    case 'm': 
     plus_self12 = tmp1;
     plus_c12 = c;
     ret_val14 = FSTR_p399773021(plus_self12, plus_c12);
     tmp1 = ret_val14;
     break;
    case 'n': 
     plus_self13 = tmp1;
     plus_c13 = c;
     ret_val15 = FSTR_p399773021(plus_self13, plus_c13);
     tmp1 = ret_val15;
     break;
    case 'o': 
     plus_self14 = tmp1;
     plus_c14 = c;
     ret_val16 = FSTR_p399773021(plus_self14, plus_c14);
     tmp1 = ret_val16;
     break;
    case 'p': 
     plus_self15 = tmp1;
     plus_c15 = c;
     ret_val17 = FSTR_p399773021(plus_self15, plus_c15);
     tmp1 = ret_val17;
     break;
    case 'q': 
     plus_self16 = tmp1;
     plus_c16 = c;
     ret_val18 = FSTR_p399773021(plus_self16, plus_c16);
     tmp1 = ret_val18;
     break;
    case 'r': 
     plus_self17 = tmp1;
     plus_c17 = c;
     ret_val19 = FSTR_p399773021(plus_self17, plus_c17);
     tmp1 = ret_val19;
     break;
    case 's': 
     plus_self18 = tmp1;
     plus_c18 = c;
     ret_val20 = FSTR_p399773021(plus_self18, plus_c18);
     tmp1 = ret_val20;
     break;
    case 't': 
     plus_self19 = tmp1;
     plus_c19 = c;
     ret_val21 = FSTR_p399773021(plus_self19, plus_c19);
     tmp1 = ret_val21;
     break;
    case 'u': 
     plus_self20 = tmp1;
     plus_c20 = c;
     ret_val22 = FSTR_p399773021(plus_self20, plus_c20);
     tmp1 = ret_val22;
     break;
    case 'v': 
     plus_self21 = tmp1;
     plus_c21 = c;
     ret_val23 = FSTR_p399773021(plus_self21, plus_c21);
     tmp1 = ret_val23;
     break;
    case 'w': 
     plus_self22 = tmp1;
     plus_c22 = c;
     ret_val24 = FSTR_p399773021(plus_self22, plus_c22);
     tmp1 = ret_val24;
     break;
    case 'x': 
     plus_self23 = tmp1;
     plus_c23 = c;
     ret_val25 = FSTR_p399773021(plus_self23, plus_c23);
     tmp1 = ret_val25;
     break;
    case 'y': 
     plus_self24 = tmp1;
     plus_c24 = c;
     ret_val26 = FSTR_p399773021(plus_self24, plus_c24);
     tmp1 = ret_val26;
     break;
    case 'z': 
     plus_self25 = tmp1;
     plus_c25 = c;
     ret_val27 = FSTR_p399773021(plus_self25, plus_c25);
     tmp1 = ret_val27;
     break;
    case '0': 
     plus_self26 = tmp1;
     plus_c26 = c;
     ret_val28 = FSTR_p399773021(plus_self26, plus_c26);
     tmp1 = ret_val28;
     break;
    case '1': 
     plus_self27 = tmp1;
     plus_c27 = c;
     ret_val29 = FSTR_p399773021(plus_self27, plus_c27);
     tmp1 = ret_val29;
     break;
    case '2': 
     plus_self28 = tmp1;
     plus_c28 = c;
     ret_val30 = FSTR_p399773021(plus_self28, plus_c28);
     tmp1 = ret_val30;
     break;
    case '3': 
     plus_self29 = tmp1;
     plus_c29 = c;
     ret_val31 = FSTR_p399773021(plus_self29, plus_c29);
     tmp1 = ret_val31;
     break;
    case '4': 
     plus_self30 = tmp1;
     plus_c30 = c;
     ret_val32 = FSTR_p399773021(plus_self30, plus_c30);
     tmp1 = ret_val32;
     break;
    case '5': 
     plus_self31 = tmp1;
     plus_c31 = c;
     ret_val33 = FSTR_p399773021(plus_self31, plus_c31);
     tmp1 = ret_val33;
     break;
    case '6': 
     plus_self32 = tmp1;
     plus_c32 = c;
     ret_val34 = FSTR_p399773021(plus_self32, plus_c32);
     tmp1 = ret_val34;
     break;
    case '7': 
     plus_self33 = tmp1;
     plus_c33 = c;
     ret_val35 = FSTR_p399773021(plus_self33, plus_c33);
     tmp1 = ret_val35;
     break;
    case '8': 
     plus_self34 = tmp1;
     plus_c34 = c;
     ret_val36 = FSTR_p399773021(plus_self34, plus_c34);
     tmp1 = ret_val36;
     break;
    case '9': 
     plus_self35 = tmp1;
     plus_c35 = c;
     ret_val37 = FSTR_p399773021(plus_self35, plus_c35);
     tmp1 = ret_val37;
     break;
    case '_': 
     plus_self36 = tmp1;
     plus_c36 = c;
     ret_val38 = FSTR_p399773021(plus_self36, plus_c36);
     tmp1 = ret_val38;
     break;
    case 'A': 
     plus_self37 = tmp1;
     plus_c37 = c;
     ret_val39 = FSTR_p399773021(plus_self37, plus_c37);
     tmp1 = ret_val39;
     break;
    case 'B': 
     plus_self38 = tmp1;
     plus_c38 = c;
     ret_val40 = FSTR_p399773021(plus_self38, plus_c38);
     tmp1 = ret_val40;
     break;
    case 'C': 
     plus_self39 = tmp1;
     plus_c39 = c;
     ret_val41 = FSTR_p399773021(plus_self39, plus_c39);
     tmp1 = ret_val41;
     break;
    case 'D': 
     plus_self40 = tmp1;
     plus_c40 = c;
     ret_val42 = FSTR_p399773021(plus_self40, plus_c40);
     tmp1 = ret_val42;
     break;
    case 'E': 
     plus_self41 = tmp1;
     plus_c41 = c;
     ret_val43 = FSTR_p399773021(plus_self41, plus_c41);
     tmp1 = ret_val43;
     break;
    case 'F': 
     plus_self42 = tmp1;
     plus_c42 = c;
     ret_val44 = FSTR_p399773021(plus_self42, plus_c42);
     tmp1 = ret_val44;
     break;
    case 'G': 
     plus_self43 = tmp1;
     plus_c43 = c;
     ret_val45 = FSTR_p399773021(plus_self43, plus_c43);
     tmp1 = ret_val45;
     break;
    case 'H': 
     plus_self44 = tmp1;
     plus_c44 = c;
     ret_val46 = FSTR_p399773021(plus_self44, plus_c44);
     tmp1 = ret_val46;
     break;
    case 'I': 
     plus_self45 = tmp1;
     plus_c45 = c;
     ret_val47 = FSTR_p399773021(plus_self45, plus_c45);
     tmp1 = ret_val47;
     break;
    case 'J': 
     plus_self46 = tmp1;
     plus_c46 = c;
     ret_val48 = FSTR_p399773021(plus_self46, plus_c46);
     tmp1 = ret_val48;
     break;
    case 'K': 
     plus_self47 = tmp1;
     plus_c47 = c;
     ret_val49 = FSTR_p399773021(plus_self47, plus_c47);
     tmp1 = ret_val49;
     break;
    case 'L': 
     plus_self48 = tmp1;
     plus_c48 = c;
     ret_val50 = FSTR_p399773021(plus_self48, plus_c48);
     tmp1 = ret_val50;
     break;
    case 'M': 
     plus_self49 = tmp1;
     plus_c49 = c;
     ret_val51 = FSTR_p399773021(plus_self49, plus_c49);
     tmp1 = ret_val51;
     break;
    case 'N': 
     plus_self50 = tmp1;
     plus_c50 = c;
     ret_val52 = FSTR_p399773021(plus_self50, plus_c50);
     tmp1 = ret_val52;
     break;
    case 'O': 
     plus_self51 = tmp1;
     plus_c51 = c;
     ret_val53 = FSTR_p399773021(plus_self51, plus_c51);
     tmp1 = ret_val53;
     break;
    case 'P': 
     plus_self52 = tmp1;
     plus_c52 = c;
     ret_val54 = FSTR_p399773021(plus_self52, plus_c52);
     tmp1 = ret_val54;
     break;
    case 'Q': 
     plus_self53 = tmp1;
     plus_c53 = c;
     ret_val55 = FSTR_p399773021(plus_self53, plus_c53);
     tmp1 = ret_val55;
     break;
    case 'R': 
     plus_self54 = tmp1;
     plus_c54 = c;
     ret_val56 = FSTR_p399773021(plus_self54, plus_c54);
     tmp1 = ret_val56;
     break;
    case 'S': 
     plus_self55 = tmp1;
     plus_c55 = c;
     ret_val57 = FSTR_p399773021(plus_self55, plus_c55);
     tmp1 = ret_val57;
     break;
    case 'T': 
     plus_self56 = tmp1;
     plus_c56 = c;
     ret_val58 = FSTR_p399773021(plus_self56, plus_c56);
     tmp1 = ret_val58;
     break;
    case 'U': 
     plus_self57 = tmp1;
     plus_c57 = c;
     ret_val59 = FSTR_p399773021(plus_self57, plus_c57);
     tmp1 = ret_val59;
     break;
    case 'V': 
     plus_self58 = tmp1;
     plus_c58 = c;
     ret_val60 = FSTR_p399773021(plus_self58, plus_c58);
     tmp1 = ret_val60;
     break;
    case 'W': 
     plus_self59 = tmp1;
     plus_c59 = c;
     ret_val61 = FSTR_p399773021(plus_self59, plus_c59);
     tmp1 = ret_val61;
     break;
    case 'X': 
     plus_self60 = tmp1;
     plus_c60 = c;
     ret_val62 = FSTR_p399773021(plus_self60, plus_c60);
     tmp1 = ret_val62;
     break;
    case 'Y': 
     plus_self61 = tmp1;
     plus_c61 = c;
     ret_val63 = FSTR_p399773021(plus_self61, plus_c61);
     tmp1 = ret_val63;
     break;
    case 'Z': 
     plus_self62 = tmp1;
     plus_c62 = c;
     ret_val64 = FSTR_p399773021(plus_self62, plus_c62);
     tmp1 = ret_val64;
     break;
    case '$': 
     plus_self63 = tmp1;
     plus_c63 = 'd';
     ret_val65 = FSTR_p399773021(plus_self63, plus_c63);
     tmp1 = ret_val65;
     break;
    case '!': 
     plus_self64 = tmp1;
     plus_c64 = 'b';
     ret_val66 = FSTR_p399773021(plus_self64, plus_c64);
     tmp1 = ret_val66;
     break;
    default: ;
   }
  }
 }
 after_loop: ;
 length_self = tmp1;
 ret_val67 = FSTR_sizerINT(length_self);
 L10 = ret_val67;
 L121_=(L10)==(0); 
 if (L121_) {
  tmp1 = FSTR_p1752847026(tmp1, ((STR) &name74));
 }
 aget_self = tmp1;
 aget_ind = 0;
 L131_=ARR((FSTR)aget_self,aget_ind); 
 ret_val68 = L131_;
 L31 = ret_val68;
 switch (L31) {
  case '0': 
   create_self1 = ((FSTR) NULL);
   L15 = 16;
   L17=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L15)*sizeof(CHAR);
   L16=ZALLOC_LEAF_BIG(L17);
   if (L16==NULL) FATAL("Unable to allocate more memory");
   memset(L16,0,L17);
   ((OB)L16)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L16)->header.destroyed=0;
#endif

   L14 = ((FSTR) L16);
   L14->asize = L15;
   ret_val69 = L14;
   L18 = FSTR_p1752847026(ret_val69, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L18, tmp1);
   break;
  case '1': 
   create_self2 = ((FSTR) NULL);
   L20 = 16;
   L23=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
   L22=ZALLOC_LEAF_BIG(L23);
   if (L22==NULL) FATAL("Unable to allocate more memory");
   memset(L22,0,L23);
   ((OB)L22)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L22)->header.destroyed=0;
#endif

   L19 = ((FSTR) L22);
   L19->asize = L20;
   ret_val70 = L19;
   L24 = FSTR_p1752847026(ret_val70, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L24, tmp1);
   break;
  case '2': 
   create_self3 = ((FSTR) NULL);
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
   ret_val71 = L25;
   L29 = FSTR_p1752847026(ret_val71, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L29, tmp1);
   break;
  case '3': 
   create_self4 = ((FSTR) NULL);
   L32 = 16;
   L34=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L32)*sizeof(CHAR);
   L33=ZALLOC_LEAF_BIG(L34);
   if (L33==NULL) FATAL("Unable to allocate more memory");
   memset(L33,0,L34);
   ((OB)L33)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L33)->header.destroyed=0;
#endif

   L30 = ((FSTR) L33);
   L30->asize = L32;
   ret_val72 = L30;
   L35 = FSTR_p1752847026(ret_val72, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L35, tmp1);
   break;
  case '4': 
   create_self5 = ((FSTR) NULL);
   L37 = 16;
   L39=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L37)*sizeof(CHAR);
   L38=ZALLOC_LEAF_BIG(L39);
   if (L38==NULL) FATAL("Unable to allocate more memory");
   memset(L38,0,L39);
   ((OB)L38)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L38)->header.destroyed=0;
#endif

   L36 = ((FSTR) L38);
   L36->asize = L37;
   ret_val73 = L36;
   L40 = FSTR_p1752847026(ret_val73, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L40, tmp1);
   break;
  case '5': 
   create_self6 = ((FSTR) NULL);
   L43 = 16;
   L45=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L43)*sizeof(CHAR);
   L44=ZALLOC_LEAF_BIG(L45);
   if (L44==NULL) FATAL("Unable to allocate more memory");
   memset(L44,0,L45);
   ((OB)L44)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L44)->header.destroyed=0;
#endif

   L42 = ((FSTR) L44);
   L42->asize = L43;
   ret_val74 = L42;
   L46 = FSTR_p1752847026(ret_val74, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L46, tmp1);
   break;
  case '6': 
   create_self7 = ((FSTR) NULL);
   L48 = 16;
   L50=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L48)*sizeof(CHAR);
   L49=ZALLOC_LEAF_BIG(L50);
   if (L49==NULL) FATAL("Unable to allocate more memory");
   memset(L49,0,L50);
   ((OB)L49)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L49)->header.destroyed=0;
#endif

   L47 = ((FSTR) L49);
   L47->asize = L48;
   ret_val75 = L47;
   L51 = FSTR_p1752847026(ret_val75, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L51, tmp1);
   break;
  case '7': 
   create_self8 = ((FSTR) NULL);
   L53 = 16;
   L55=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L53)*sizeof(CHAR);
   L54=ZALLOC_LEAF_BIG(L55);
   if (L54==NULL) FATAL("Unable to allocate more memory");
   memset(L54,0,L55);
   ((OB)L54)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L54)->header.destroyed=0;
#endif

   L52 = ((FSTR) L54);
   L52->asize = L53;
   ret_val76 = L52;
   L56 = FSTR_p1752847026(ret_val76, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L56, tmp1);
   break;
  case '8': 
   create_self9 = ((FSTR) NULL);
   L58 = 16;
   L60=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L58)*sizeof(CHAR);
   L59=ZALLOC_LEAF_BIG(L60);
   if (L59==NULL) FATAL("Unable to allocate more memory");
   memset(L59,0,L60);
   ((OB)L59)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L59)->header.destroyed=0;
#endif

   L57 = ((FSTR) L59);
   L57->asize = L58;
   ret_val77 = L57;
   L61 = FSTR_p1752847026(ret_val77, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L61, tmp1);
   break;
  case '9': 
   create_self10 = ((FSTR) NULL);
   L63 = 16;
   L65=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L63)*sizeof(CHAR);
   L64=ZALLOC_LEAF_BIG(L65);
   if (L64==NULL) FATAL("Unable to allocate more memory");
   memset(L64,0,L65);
   ((OB)L64)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L64)->header.destroyed=0;
#endif

   L62 = ((FSTR) L64);
   L62->asize = L63;
   ret_val78 = L62;
   L66 = FSTR_p1752847026(ret_val78, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L66, tmp1);
   break;
  case '_': 
   create_self11 = ((FSTR) NULL);
   L68 = 16;
   L70=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L68)*sizeof(CHAR);
   L69=ZALLOC_LEAF_BIG(L70);
   if (L69==NULL) FATAL("Unable to allocate more memory");
   memset(L69,0,L70);
   ((OB)L69)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

     ((OB)L69)->header.destroyed=0;
#endif

   L67 = ((FSTR) L69);
   L67->asize = L68;
   ret_val79 = L67;
   L71 = FSTR_p1752847026(ret_val79, ((STR) &S1));
   tmp1 = FSTR_p1410513982(L71, tmp1);
   break;
  default: ;
 }
 str_self = tmp1;
 ret_val80 = STR_fr1097270334(((STR) NULL), str_self);
 class_name = ret_val80;
 plus_self65 = prefix1;
 plus_sarg = class_name;
 ret_val81 = STR_ap2004550586(plus_self65, plus_sarg);
 full_name = ret_val81;
 L72 = STR_hashrINT(full_name);
 L731_=(L72<0)?-L72:L72; 
 str_self1 = L731_;
 clear_self = buf1;
 L74 = (clear_self==((FSTR) NULL));
 L751_=!(L74); 
 if (L751_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self1;
 str_in_s = buf1;
 ret_val83 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val83;
 str_self2 = buf1;
 ret_val84 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val82 = ret_val84;
 hash_str = ret_val82;
 L76 = FILE_M852907408;
 L771_=INTMINUS(L76,10); 
 L78 = L771_;
 L79 = STR_sizerINT(full_name);
 L801_=(L78)<(L79); 
 if (L801_) {
  L81 = FILE_M852907408;
  L821_=INTMINUS(L81,10); 
  full_name = STR_head_INTrSTR(full_name, L821_);
 }
 plus_self66 = full_name;
 plus_sarg1 = hash_str;
 ret_val85 = STR_ap2004550586(plus_self66, plus_sarg1);
 full_name = ret_val85;
 res = full_name;
 {
  BOOL f_L831_ = TRUE;
  L41 = 0;
  L831_=L41-1; 
  while (1) {
   L831_++; 
   i = L831_;
   if (FSETST1025458804(FILE_M683825354, res)) {
   }
   else {
    goto after_loop1;
   }
   plus_self67 = full_name;
   str_self3 = i;
   clear_self1 = buf2;
   L85 = (clear_self1==((FSTR) NULL));
   L861_=!(L85); 
   if (L861_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self3;
   str_in_s1 = buf2;
   ret_val87 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val87;
   str_self4 = buf2;
   ret_val88 = STR_fr1097270334(((STR) NULL), str_self4);
   ret_val86 = ret_val88;
   plus_sarg2 = ret_val86;
   ret_val89 = STR_ap2004550586(plus_self67, plus_sarg2);
   res = ret_val89;
   L871_=(1000)<(i); 
   if (L871_) {
    RAISE(((OB) ((STR) &Intern1789924828)));
   }
  }
 }
 after_loop1: ;
 FILE_M683825354 = FSETST1404644833(FILE_M683825354, res);
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CHECK_1724422536(CHECK_1174552075 self) {
 FSETdTP L11;
 dTP tp;
 IFC ti;
 TP_CLASS parents;
 FSETTP_CLASS L21;
 IFC pi1;
 FLISTSIG ncs;
 STR print_err;
 SIG_TBL sigtbl;
 INT sig_index = INT_zero;
 INT L31 = INT_zero;
 SIG asig;
 BOOL found_one = BOOL_zero;
 STR sug_str;
 SIG actual1;
 SIG_TBL L41;
 IDENT L51 = IDENT_zero;
 STR L61;
 dTP chld;
 FSETdTP L71;
 CHECK_1174552075 err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 CHECK_1174552075 err_self;
 STR err_s;
 TP_GRAPH get_parents_self;
 TP_CLASS get_parents_tp;
 FSETTP_CLASS ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 INT L81 = INT_zero;
 FLISTSIG aget_self;
 INT aget_ind = INT_zero;
 SIG ret_val7;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val8;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val9;
 STR plus_self8;
 INT plus_arg = INT_zero;
 STR ret_val10;
 STR s;
 INT str_self = INT_zero;
 STR ret_val11;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val12;
 FSTR str_self1;
 STR ret_val13;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val14;
 STR plus_self10;
 STR plus_sarg9;
 STR ret_val15;
 STR plus_self11;
 STR plus_sarg10;
 STR ret_val16;
 STR plus_self12;
 STR plus_sarg11;
 STR ret_val17;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val18;
 CHECK_1174552075 err_loc_self1;
 dPROG_ERR err_loc_t1;
 CHECK_1174552075 err_self1;
 STR err_s1;
 TP_GRAPH get_children_sel;
 TP_CLASS get_children_tp;
 FSETdTP ret_val19;
 IFC conforms_to_self;
 IFC conforms_to_i;
 BOOL ret_val20 = BOOL_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val21;
 CHECK_1174552075 err_loc_self2;
 dPROG_ERR err_loc_t2;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val22;
 STR plus_self14;
 STR plus_sarg13;
 STR ret_val23;
 STR plus_self15;
 STR plus_sarg14;
 STR ret_val24;
 STR plus_self16;
 STR plus_sarg15;
 STR ret_val25;
 CHECK_1174552075 err_self2;
 STR err_s2;
 dTP L9;
 extern STR Cantco962219184;
 extern STR name18;
 TP_CLASS L10;
 BOOL L12;
 BOOL L131_;
 extern STR Theint654357930;
 extern STR doesnt1808182440;
 extern STR name18;
 INT aL141_;
 INT L15;
 SIG L161_;
 extern STR name1;
 extern STR and4;
 SIG L17;
 STR L18;
 extern STR name37;
 INT L191_;
 BOOL L20;
 BOOL L221_;
 extern STR Forsignature;
 extern STR Ithas;
 INT L231_;
 dPARSE L24;
 IDENT L25;
 ARRAYdTP L26;
 INT L271_;
 dPARSE L28;
 dTP L29;
 BOOL L30;
 BOOL L321_;
 dPARSE L33;
 IDENT L34;
 ARRAYdTP L35;
 INT L361_;
 dPARSE L37;
 extern STR Theint654357930;
 extern STR isntco77568086;
 extern STR name18;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,prog),tp_done);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   tp = L9;
   if (tp==NULL) {
    goto other133;
   } else
   switch (TAG(tp)) {
    case TP_CLASS_tag:
     ti = TP_CLASS_ifcrIFC(((TP_CLASS) tp));
     if ((ti==((IFC) NULL))) {
      err_loc_self = self;
      err_loc_t = ((dPROG_ERR) NULL);
      PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
      err_self = self;
      plus_self = ((STR) &Cantco962219184);
      plus_sarg = TP_CLASS_strrSTR(((TP_CLASS) tp));
      ret_val = STR_ap2004550586(plus_self, plus_sarg);
      plus_self1 = ret_val;
      plus_sarg1 = ((STR) &name18);
      ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
      err_s = ret_val1;
      PROG_err_STR(ATTR(err_self,prog), err_s);
      return;
     }
     if (ATTR(ATTR(self,prog),show_ifc)) {
      IFC_show(ti);
     }
     {
      struct FSETTP340014621_frame_struct other2_0;
FSETTP340014621_frame noname2 = &other2_0;
      noname2->state = 0;
      while (1) {
       if (noname2->state == 0) {
        get_parents_self = ATTR(ATTR(self,prog),tp_graph);
        get_parents_tp = ((TP_CLASS) tp);
        ret_val2 = FMAPTP2096448595(ATTR(get_parents_self,par_tbl), get_parents_tp);
        L21 = ret_val2;
        noname2->self = L21;
       }
       L10 = FSETTP340014621(noname2);
       if (noname2->state == -1) {
        goto after_loop1;
       }
       parents = L10;
       pi1 = TP_CLASS_ifcrIFC(parents);
       ncs = IFC_no96952824(ti, pi1);
       L12 = (ncs==((FLISTSIG) NULL));
       L131_=!(L12); 
       if (L131_) {
        plus_self2 = ((STR) &Theint654357930);
        plus_sarg2 = TP_CLASS_strrSTR(((TP_CLASS) tp));
        ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
        plus_self3 = ret_val3;
        plus_sarg3 = ((STR) &doesnt1808182440);
        ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
        plus_self4 = ret_val4;
        plus_sarg4 = TP_CLASS_strrSTR(parents);
        ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
        plus_self5 = ret_val5;
        plus_sarg5 = ((STR) &name18);
        ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
        print_err = ret_val6;
        sigtbl = ATTR(ti,sigs);
        {
         BOOL f_L141_ = TRUE;
         /* loop index variable */
         L81 = 0;
         L31 = FLISTS228922656(ncs);
         while (1) {
          if(L81>=L31)  goto after_loop2; 
          aL141_=L81; 
          sig_index = aL141_;
          aget_self = ncs;
          aget_ind = sig_index;
          L161_=(SIG)ARR((FLISTSIG)aget_self,aget_ind); 
          ret_val7 = L161_;
          asig = ret_val7;
          found_one = FALSE;
          sug_str = ((STR) &name1);
          actual1 = ((SIG) NULL);
          {
           struct SIG_TB788365833_frame_struct other4_0;
SIG_TB788365833_frame noname3 = &other4_0;
           struct STR_se405450305_frame_struct other4_1;
STR_se405450305_frame noname4 = &other4_1;
           L41 = sigtbl;
           L51 = ATTR(asig,name1);
           L61 = ((STR) &and4);
           noname3->self = L41;
           noname3->arg1 = L51;
           noname3->state = 0;
           noname4->self = L61;
           noname4->state = 0;
           while (1) {
            L17 = SIG_TB788365833(noname3);
            if (noname3->state == -1) {
             goto after_loop3;
            }
            actual1 = L17;
            found_one = TRUE;
            plus_self6 = sug_str;
            noname4->arg1 = ((dSTR) ATTR(actual1,str));
            L18 = STR_se405450305(noname4);
            if (noname4->state == -1) {
             goto after_loop3;
            }
            plus_sarg6 = L18;
            ret_val8 = STR_ap2004550586(plus_self6, plus_sarg6);
            sug_str = ret_val8;
           }
          }
          after_loop3: ;
          plus_self7 = print_err;
          plus_sarg7 = ((STR) &name37);
          ret_val9 = STR_ap2004550586(plus_self7, plus_sarg7);
          plus_self8 = ret_val9;
          L191_=INTPLUS(sig_index,1); 
          plus_arg = L191_;
          str_self = plus_arg;
          clear_self = buf1;
          L20 = (clear_self==((FSTR) NULL));
          L221_=!(L20); 
          if (L221_) {
           SATTR(clear_self,loc,0);
          }
          str_in_self = str_self;
          str_in_s = buf1;
          ret_val12 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
          buf1 = ret_val12;
          str_self1 = buf1;
          ret_val13 = STR_fr1097270334(((STR) NULL), str_self1);
          ret_val11 = ret_val13;
          s = ret_val11;
          ret_val10 = STR_ap1077157958(plus_self8, s, TRUE);
          plus_self9 = ret_val10;
          plus_sarg8 = ((STR) &Forsignature);
          ret_val14 = STR_ap2004550586(plus_self9, plus_sarg8);
          plus_self10 = ret_val14;
          plus_sarg9 = ATTR(asig,str);
          ret_val15 = STR_ap2004550586(plus_self10, plus_sarg9);
          print_err = ret_val15;
          if (found_one) {
           plus_self11 = print_err;
           plus_sarg10 = ((STR) &Ithas);
           ret_val16 = STR_ap2004550586(plus_self11, plus_sarg10);
           plus_self12 = ret_val16;
           plus_sarg11 = sug_str;
           ret_val17 = STR_ap2004550586(plus_self12, plus_sarg11);
           print_err = ret_val17;
          }
          L231_=INTPLUS(L81,1); 
          L81 = L231_;
         }
        }
        after_loop2: ;
        err_loc_self1 = self;
        as_tree_self = ((TP_CLASS) tp);
        if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
         L24 = ATTR(ATTR(as_tree_self,prog),parse);
         L25 = ATTR(as_tree_self,name1);
         L26 = ATTR(as_tree_self,params);
         L271_=(L26)==NULL?0:ASIZE((ARRAYdTP)L26); 
         L28 = L24;
         SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L28)])(L28, L25, L271_));
        }
        ret_val18 = ATTR(as_tree_self,my_as_tree);
        err_loc_t1 = ((dPROG_ERR) ret_val18);
        PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
        err_self1 = self;
        err_s1 = print_err;
        PROG_err_STR(ATTR(err_self1,prog), err_s1);
       }
      }
     }
     after_loop1: ;
     {
      struct FSETdTP_eltbrdTP_frame_struct other5_0;
FSETdTP_eltbrdTP_frame noname5 = &other5_0;
      noname5->state = 0;
      while (1) {
       if (noname5->state == 0) {
        get_children_sel = ATTR(ATTR(self,prog),tp_graph);
        get_children_tp = ((TP_CLASS) tp);
        ret_val19 = FMAPTP861983338(ATTR(get_children_sel,child_tbl), get_children_tp);
        L71 = ret_val19;
        noname5->self = L71;
       }
       L29 = FSETdTP_eltbrdTP(noname5);
       if (noname5->state == -1) {
        goto after_loop4;
       }
       chld = L29;
       if (chld==NULL) {
        goto other134;
       } else
       switch (TAG(chld)) {
        case TP_CLASS_tag:
         conforms_to_self = TP_CLASS_ifcrIFC(((TP_CLASS) chld));
         conforms_to_i = ti;
         ret_val20 = (IFC_no472265690(conforms_to_self, conforms_to_i)==((SIG) NULL));
         L30 = ret_val20;
         L321_=!(L30); 
         if (L321_) {
          err_loc_self2 = self;
          as_tree_self1 = ((TP_CLASS) tp);
          if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
           L33 = ATTR(ATTR(as_tree_self1,prog),parse);
           L34 = ATTR(as_tree_self1,name1);
           L35 = ATTR(as_tree_self1,params);
           L361_=(L35)==NULL?0:ASIZE((ARRAYdTP)L35); 
           L37 = L33;
           SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L37)])(L37, L34, L361_));
          }
          ret_val21 = ATTR(as_tree_self1,my_as_tree);
          err_loc_t2 = ((dPROG_ERR) ret_val21);
          PROG_e176405615(ATTR(err_loc_self2,prog), err_loc_t2);
          err_self2 = self;
          plus_self13 = ((STR) &Theint654357930);
          plus_sarg12 = TP_CLASS_strrSTR(((TP_CLASS) tp));
          ret_val22 = STR_ap2004550586(plus_self13, plus_sarg12);
          plus_self14 = ret_val22;
          plus_sarg13 = ((STR) &isntco77568086);
          ret_val23 = STR_ap2004550586(plus_self14, plus_sarg13);
          plus_self15 = ret_val23;
          plus_sarg14 = TP_CLASS_strrSTR(((TP_CLASS) chld));
          ret_val24 = STR_ap2004550586(plus_self15, plus_sarg14);
          plus_self16 = ret_val24;
          plus_sarg15 = ((STR) &name18);
          ret_val25 = STR_ap2004550586(plus_self16, plus_sarg15);
          err_s2 = ret_val25;
          PROG_err_STR(ATTR(err_self2,prog), err_s2);
         } break;
        default: ;
        other134: ;
         goto after_loop4;
       }
      }
     }
     after_loop4: ; break;
    default: ;
    other133: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CHECK_397667670(CHECK_GENERICS self, SIG s) {
 ELT elt1;
 AM_ROUT_DEF am;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 OUT ret_val1;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR Checksig;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name9;
 FILE1 L7;
 OB L8;
 dTP L10;
 dTP L11;
 IMPL L12;
 if (ATTR(s,am_created)) {
  return;
 }
 SATTR(s,am_created,TRUE);
 if (ATTR(ATTR(self,prog),show_checked_sig)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Checksig);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ATTR(s,str);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 L10 = ATTR(s,tp);
 if ((*dTP_is1811059018[TAG(L10)])(L10)) {
  return;
 }
 L11 = ATTR(s,tp);
 L12=(*dTP_implrIMPL[TAG(L11)])(L11);
 elt1 = ELT_TB1949891380(ATTR(L12,elts), s);
 am = TRANS_503579921(((TRANS) NULL), elt1);
}


#undef IS_ITER
#define IS_ITER 0

void CHECK_452836548(CHECK_GENERICS self) {
 TP_CLASS_TBL L11;
 TP_CLASS gen_class;
 IFC ti;
 SIG_TBL L21;
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
 TP_CLASS L3;
 extern STR Checking;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L12;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 BOOL L20;
 BOOL L221_;
 SIG L23;
 {
  struct TP_CLA115798516_frame_struct other1_0;
TP_CLA115798516_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = TP_CLA115798516(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   gen_class = L3;
   if (ATTR(ATTR(self,prog),trace_generics)) {
    create_self = ((ERR1) NULL);
    ret_val = create_self;
    plus_self = ret_val;
    plus_s = ((dSTR) ((STR) &Checking));
    stderr_self = ((FILE1) NULL);
    L5=ZALLOC(sizeof(struct FILE1_struct));
    if (L5==NULL) FATAL("Unable to allocate more memory");
    ((OB)L5)->header.tag=FILE1_tag;
    L4 = ((FILE1) L5);
    r = L4;
    
    SATTR(r,fp,stderr);
    ret_val2 = r;
    FILE_plus_dSTR(ret_val2, plus_s);
    ret_val1 = plus_self;
    plus_self1 = ret_val1;
    plus_s1 = ((dSTR) TP_CLASS_strrSTR(gen_class));
    stderr_self1 = ((FILE1) NULL);
    L8=ZALLOC(sizeof(struct FILE1_struct));
    if (L8==NULL) FATAL("Unable to allocate more memory");
    ((OB)L8)->header.tag=FILE1_tag;
    L7 = ((FILE1) L8);
    r1 = L7;
    
    SATTR(r1,fp,stderr);
    ret_val4 = r1;
    FILE_plus_dSTR(ret_val4, plus_s1);
    ret_val3 = plus_self1;
    plus_self2 = ret_val3;
    plus_s2 = ((dSTR) ((STR) &name9));
    stderr_self2 = ((FILE1) NULL);
    L12=ZALLOC(sizeof(struct FILE1_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=FILE1_tag;
    L10 = ((FILE1) L12);
    r2 = L10;
    
    SATTR(r2,fp,stderr);
    ret_val5 = r2;
    FILE_plus_dSTR(ret_val5, plus_s2);
   }
   if (ATTR(gen_class,is_generic)) {
    L15 = TP_CLA1568240525(gen_class);
    L161_=!(L15); 
    L14 = L161_;
   } else {
    L14 = FALSE;
   }
   if (L14) {
    ti = TP_CLASS_ifcrIFC(gen_class);
    L18 = (ti==((IFC) NULL));
    L191_=!(L18); 
    if (L191_) {
     L20 = (ATTR(ti,sigs)==((SIG_TBL) NULL));
     L221_=!(L20); 
     L17 = L221_;
    } else {
     L17 = FALSE;
    }
    if (L17) {
     {
      struct SIG_TBL_eltbrSIG_frame_struct other2_0;
SIG_TBL_eltbrSIG_frame noname2 = &other2_0;
      L21 = ATTR(ti,sigs);
      noname2->self = L21;
      noname2->state = 0;
      while (1) {
       L23 = SIG_TBL_eltbrSIG(noname2);
       if (noname2->state == -1) {
        goto after_loop1;
       }
       CHECK_397667670(self, L23);
      }
     }
     after_loop1: ;
    }
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CHECK_801221867(CHECK_AM self) {
 FSETdTP tp_done;
 dTP tp;
 FSETdTP L11;
 IFC ti;
 SIG_TBL L21;
 INT L3;
 BOOL L41_;
 dTP L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 SIG L14;
 SATTR(ATTR(self,prog),is_check_am,TRUE);
 while (1) {
  L3 = FSETdTP_sizerINT(ATTR(ATTR(self,prog),tp_done));
  L41_=(L3)==(0); 
  if (L41_) {
   goto after_loop;
  }
  tp_done = ATTR(ATTR(self,prog),tp_done);
  SATTR(ATTR(self,prog),tp_done,((FSETdTP) NULL));
  {
   struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname1 = &other2_0;
   L11 = tp_done;
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L5 = FSETdTP_eltbrdTP(noname1);
    if (noname1->state == -1) {
     goto after_loop1;
    }
    tp = L5;
    if (tp==NULL) {
     goto other468;
    } else
    switch (TAG(tp)) {
     case TP_CLASS_tag:
      L6 = TP_CLA1568240525(((TP_CLASS) tp));
      L71_=!(L6); 
      if (L71_) {
       ti = TP_CLASS_ifcrIFC(((TP_CLASS) tp));
       L9 = (ti==((IFC) NULL));
       L101_=!(L9); 
       if (L101_) {
        L12 = (ATTR(ti,sigs)==((SIG_TBL) NULL));
        L131_=!(L12); 
        L8 = L131_;
       } else {
        L8 = FALSE;
       }
       if (L8) {
        {
         struct SIG_TBL_eltbrSIG_frame_struct other3_0;
SIG_TBL_eltbrSIG_frame noname2 = &other3_0;
         L21 = ATTR(ti,sigs);
         noname2->self = L21;
         noname2->state = 0;
         while (1) {
          L14 = SIG_TBL_eltbrSIG(noname2);
          if (noname2->state == -1) {
           goto after_loop2;
          }
          CHECK_835201167(self, L14);
         }
        }
        after_loop2: ;
       }
      } break;
     default: ;
     other468: ;
    }
   }
  }
  after_loop1: ;
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CHECK_835201167(CHECK_AM self, SIG s) {
 ELT elt1;
 AM_ROUT_DEF am;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 OUT ret_val1;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self2;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 extern STR Checksig;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name9;
 FILE1 L7;
 OB L8;
 dTP L10;
 dTP L11;
 IMPL L12;
 BOOL L13;
 extern STR CheckAMfor;
 FILE1 L14;
 OB L15;
 FILE1 L17;
 OB L18;
 extern STR name33;
 FILE1 L20;
 OB L21;
 extern STR name9;
 FILE1 L23;
 OB L24;
 dBACK_END L26;
 if (ATTR(s,am_created)) {
  return;
 }
 SATTR(s,am_created,TRUE);
 if (ATTR(ATTR(self,prog),show_checked_sig)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Checksig);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ATTR(s,str);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r2 = L7;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 L10 = ATTR(s,tp);
 if ((*dTP_is1811059018[TAG(L10)])(L10)) {
  return;
 }
 L11 = ATTR(s,tp);
 L12=(*dTP_implrIMPL[TAG(L11)])(L11);
 elt1 = ELT_TB1949891380(ATTR(L12,elts), s);
 if (ATTR(elt1,is_external)) {
  L13 = ELT_is1907475257(elt1);
 } else {
  L13 = FALSE;
 }
 if (L13) {
  return;
 }
 if ((elt1==((ELT) NULL))) {
  return;
 }
 am = TRANS_503579921(((TRANS) NULL), elt1);
 if ((am==((AM_ROUT_DEF) NULL))) {
  return;
 }
 if (ATTR(ATTR(self,prog),show_am_check)) {
  create_self1 = ((OUT) NULL);
  ret_val6 = create_self1;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &CheckAMfor);
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
  plus_s4 = ATTR(ATTR(am,sig1),str);
  stdout_self4 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r4 = L17;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s4);
  ret_val9 = plus_self4;
  plus_self5 = ret_val9;
  plus_s5 = ((STR) &name33);
  stdout_self5 = ((FILE1) NULL);
  L21=ZALLOC(sizeof(struct FILE1_struct));
  if (L21==NULL) FATAL("Unable to allocate more memory");
  ((OB)L21)->header.tag=FILE1_tag;
  L20 = ((FILE1) L21);
  r5 = L20;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
  create_self2 = ((OUT) NULL);
  ret_val12 = create_self2;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name9);
  stdout_self6 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r6 = L23;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
 }
 if (ATTR(ATTR(self,prog),generate_checked)) {
  L26 = ATTR(ATTR(self,prog),back_end);
  (*dBACK_1132541334[TAG(L26)])(L26, am);
 }
}
