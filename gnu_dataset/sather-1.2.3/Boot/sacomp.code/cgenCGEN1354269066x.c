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

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

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

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

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

typedef struct ELT_NILELT_struct {/* layout for ELT_NIL{ELT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILELT;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

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

typedef struct INT_fo686245905_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_fo686245905_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

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

typedef struct NAMESPACE_struct {/* layout for NAMESPACE */
 OB_HEADER header;
 struct FMAPSTRINT_struct *uniques;
 struct FSETSTR_struct *set;
 INT counter;
 struct NAMEMAP_struct *map;
 } *NAMESPACE;

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

typedef struct TUPIDENTdTP_struct {/* layout for TUP{IDENT,$TP} */
 struct dTP_struct *t2;
 struct IDENT_struct t1;
 } TUPIDENTdTP;
static TUPIDENTdTP TUPIDENTdTP_zero;

typedef struct TUPIDENTdTP_boxed_struct {
 OB_HEADER header;
 TUPIDENTdTP immutable_part;
 } *TUPIDENTdTP_boxed;

typedef struct TUPSTRINT_struct {/* layout for TUP{STR,INT} */
 INT t2;
 STR t1;
 } TUPSTRINT;
static TUPSTRINT TUPSTRINT_zero;

typedef struct TUPSTRINT_boxed_struct {
 OB_HEADER header;
 TUPSTRINT immutable_part;
 } *TUPSTRINT_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

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

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPIDENTdTP_struct {/* layout for FMAP{IDENT,$TP} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPIDENTdTP_struct arr_part[1];
 } *FMAPIDENTdTP;

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct NAMEMAP_struct {/* layout for NAMEMAP */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBSTR_struct arr_part[1];
 } *NAMEMAP;

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

extern INOUT_MODE MODES_inout_mode;
extern OUT_MODE MODES_out_mode;

/* Function declarations */


extern RETURNED_CONST AM_CALL_ARG (**dAM_CA967858872)(dAM_CALL_EXPR, INT);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dAM_CA1128544926)(dAM_CALL_EXPR);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dTP (**dAM_CONST_tprdTP)(dAM_CONST);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_OB_DEF CGEN_a1221397173(CGEN, dTP);
ARRAYSTR CGEN_e1933193154(CGEN, dAM_CALL_EXPR);
ARRAYSTR CGEN_e862722519(CGEN, dAM_CALL_EXPR);
BOOL CGEN_n1296275061(CGEN, dAM_CALL_EXPR);
BOOL CHAR_i1016724011(CHAR);
BOOL CHAR_i1942379470(CHAR);
BOOL ELT_is1269015106(ELT);
BOOL ELT_is1936066670(ELT);
BOOL ELT_is380037919(ELT);
BOOL SIG_is418491101(SIG);
ELT ELT_TBL_eltbrELT(ELT_TBL_eltbrELT_frame);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT ARRAYI1725945879(ARRAYI1725945879_frame);
IDENT IDENT_1150413730(IDENT, STR);
INT INT_do676118316(INT_do676118316_frame);
INT INT_fo686245905(INT_fo686245905_frame);
INT INT_up418511718(INT_up418511718_frame);
INT STR_sizerINT(STR);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_d2002278524(CGEN, dTP);
STR CGEN_e1606764211(CGEN, STR, dAM_CALL_EXPR, SIG, ARRAYSTR, STR, STR, STR);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e236145586(CGEN, AM_LOCAL_EXPR);
STR CGEN_e414796278(CGEN, AM_CALL_ARG, ARG, BOOL, BOOL);
STR CGEN_eolrSTR(CGEN);
STR CGEN_v205291382(CGEN, dTP, STR, STR);
STR MANGLE119219986(MANGLE, OB, OB);
STR NAMESP180237388(NAMESPACE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1182453786(STR, STR, STR, STR, STR, STR, STR);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
dTP FMAPID86862037(FMAPIDENTdTP, IDENT);
void CGEN_d695831906(CGEN);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CGEN_e1933193154(CGEN self, dAM_CALL_EXPR arce) {
 ARRAYSTR ret_val;
 ARRAYARG sig_args = ((ARRAYARG) NULL);
 dTP self_tp1 = ((dTP) NULL);
 ARRAYSTR arg_list;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val1;
 CGEN cast_arg_self;
 dTP cast_arg_dest_tp;
 dTP cast_arg_src_tp;
 STR cast_arg_expr;
 BOOL cast_arg_only_bo = BOOL_zero;
 dMODE cast_arg_mode;
 STR ret_val2;
 CGEN cast_arg_self1;
 dTP cast_arg_dest_tp1;
 dTP cast_arg_src_tp1;
 STR cast_arg_expr1;
 BOOL cast_arg_only_bo1 = BOOL_zero;
 dMODE cast_arg_mode1;
 STR ret_val3;
 AM_CALL_ARG L51_;
 AM_CALL_ARG L6;
 dAM_EXPR L7;
 STR L81_;
 STR L9;
 INT L131_;
 INT L14;
 INT L151_;
 INT L121_,L121_m;
 INT L16;
 BOOL L17;
 BOOL L18;
 ARRAYARG L19;
 INT L201_;
 INT L22;
 ARG L231_;
 ARG L24;
 dTP L25;
 AM_CALL_ARG L261_;
 AM_CALL_ARG L27;
 AM_CALL_ARG L281_;
 AM_CALL_ARG L29;
 dAM_EXPR L30;
 dTP L32;
 BOOL L33;
 BOOL L341_;
 ARRAYARG L35;
 INT L361_;
 INT L37;
 ARG L381_;
 ARG L39;
 AM_CALL_ARG L401_;
 AM_CALL_ARG L42;
 dAM_EXPR L43;
 STR L441_;
 AM_CALL_ARG L451_;
 AM_CALL_ARG L46;
 STR L47;
 INT L501_;
 INT L52;
 INT L531_;
 INT L491_,L491_m;
 INT L54;
 BOOL L55;
 BOOL L56;
 ARRAYARG L57;
 ARG L581_;
 ARG L59;
 dTP L60;
 ARRAYARG L61;
 ARG L621_;
 ARG L63;
 AM_CALL_ARG L641_;
 AM_CALL_ARG L65;
 dAM_EXPR L66;
 dTP L67;
 BOOL L68;
 BOOL L691_;
 ARRAYARG L70;
 ARG L711_;
 ARG L72;
 AM_CALL_ARG L731_;
 AM_CALL_ARG L74;
 dAM_EXPR L75;
 STR L761_;
 AM_CALL_ARG L771_;
 AM_CALL_ARG L78;
 STR L79;
 arg_list = CGEN_e862722519(self, arce);
 switch (TAG(arce)) {
  case AM_ROU1916046290_tag:
   cast_self = self;
   cast_dest_tp = ATTR(ATTR(((AM_ROU1916046290) arce),fun),tp);
   L51_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,0); 
   L6=L51_;
   L7 = ATTR(L6,expr);
   cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
   L81_=(STR)ARR((ARRAYSTR)arg_list,0); 
   cast_expr = L81_;
   cast_only_boxing = SIG_is418491101(ATTR(((AM_ROU1916046290) arce),fun));
   ret_val1 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   L9 = ret_val1;
   SARR((ARRAYSTR)arg_list,0,(STR)L9); 
   ;
   {
    BOOL f_L121_ = TRUE;
    L11 = 1;
    L131_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
    L14 = L131_;
    L151_=INTMINUS(L14,1); 
    L21 = L151_;
    L121_=L11-1;L121_m=L21; 
    while (1) {
     if(L121_++>=L121_m)  goto after_loop; 
     i = L121_;
     L19 = ATTR(ATTR(((AM_ROU1916046290) arce),fun),args);
     L201_=INTMINUS(i,1); 
     L22 = L201_;
     L231_=(ARG)ARR((ARRAYARG)L19,L22); 
     L24=L231_;
     L25 = ATTR(L24,tp);
     if ((*dTP_is1334578382[TAG(L25)])(L25)) {
      L261_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,i); 
      L27=L261_;
      L18 = SYSOBEQ(((OB) ATTR(L27,mode1)),((OB) MODES_out_mode));
     } else {
      L18 = FALSE;
     }
     if (L18) {
      L281_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,i); 
      L29=L281_;
      L30 = ATTR(L29,expr);
      L32 = (*dAM_EXPR_tprdTP[TAG(L30)])(L30);
      L17 = (*dTP_is1811059018[TAG(L32)])(L32);
     } else {
      L17 = FALSE;
     }
     L33 = L17;
     L341_=!(L33); 
     if (L341_) {
      cast_arg_self = self;
      L35 = ATTR(ATTR(((AM_ROU1916046290) arce),fun),args);
      L361_=INTMINUS(i,1); 
      L37 = L361_;
      L381_=(ARG)ARR((ARRAYARG)L35,L37); 
      L39=L381_;
      cast_arg_dest_tp = ATTR(L39,tp);
      L401_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,i); 
      L42=L401_;
      L43 = ATTR(L42,expr);
      cast_arg_src_tp = (*dAM_EXPR_tprdTP[TAG(L43)])(L43);
      L441_=(STR)ARR((ARRAYSTR)arg_list,i); 
      cast_arg_expr = L441_;
      cast_arg_only_bo = SIG_is418491101(ATTR(((AM_ROU1916046290) arce),fun));
      L451_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,i); 
      L46=L451_;
      cast_arg_mode = ATTR(L46,mode1);
      ret_val2 = CGEN_d1915593494(cast_arg_self, cast_arg_dest_tp, cast_arg_src_tp, cast_arg_expr, cast_arg_only_bo, cast_arg_mode);
      L47 = ret_val2;
      SARR((ARRAYSTR)arg_list,i,(STR)L47); 
      ;
     }
    }
   }
   after_loop: ; break;
  case AM_BND260301329_tag:
   {
    BOOL f_L491_ = TRUE;
    L31 = 0;
    L501_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
    L52 = L501_;
    L531_=INTMINUS(L52,1); 
    L41 = L531_;
    L491_=L31-1;L491_m=L41; 
    while (1) {
     if(L491_++>=L491_m)  goto after_loop1; 
     i1 = L491_;
     L57 = ATTR(ATTR(((AM_BND260301329) arce),br_tp),args);
     L581_=(ARG)ARR((ARRAYARG)L57,i1); 
     L59=L581_;
     L60 = ATTR(L59,tp);
     if ((*dTP_is1334578382[TAG(L60)])(L60)) {
      L61 = ATTR(ATTR(((AM_BND260301329) arce),br_tp),args);
      L621_=(ARG)ARR((ARRAYARG)L61,i1); 
      L63=L621_;
      L56 = SYSOBEQ(((OB) ATTR(L63,mode1)),((OB) MODES_out_mode));
     } else {
      L56 = FALSE;
     }
     if (L56) {
      L641_=(AM_CALL_ARG)ARR((AM_BND260301329)arce,i1); 
      L65=L641_;
      L66 = ATTR(L65,expr);
      L67 = (*dAM_EXPR_tprdTP[TAG(L66)])(L66);
      L55 = (*dTP_is1811059018[TAG(L67)])(L67);
     } else {
      L55 = FALSE;
     }
     L68 = L55;
     L691_=!(L68); 
     if (L691_) {
      cast_arg_self1 = self;
      L70 = ATTR(ATTR(((AM_BND260301329) arce),br_tp),args);
      L711_=(ARG)ARR((ARRAYARG)L70,i1); 
      L72=L711_;
      cast_arg_dest_tp1 = ATTR(L72,tp);
      L731_=(AM_CALL_ARG)ARR((AM_BND260301329)arce,i1); 
      L74=L731_;
      L75 = ATTR(L74,expr);
      cast_arg_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L75)])(L75);
      L761_=(STR)ARR((ARRAYSTR)arg_list,i1); 
      cast_arg_expr1 = L761_;
      cast_arg_only_bo1 = FALSE;
      L771_=(AM_CALL_ARG)ARR((AM_BND260301329)arce,i1); 
      L78=L771_;
      cast_arg_mode1 = ATTR(L78,mode1);
      ret_val3 = CGEN_d1915593494(cast_arg_self1, cast_arg_dest_tp1, cast_arg_src_tp1, cast_arg_expr1, cast_arg_only_bo1, cast_arg_mode1);
      L79 = ret_val3;
      SARR((ARRAYSTR)arg_list,i1,(STR)L79); 
      ;
     }
    }
   }
   after_loop1: ; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_and_cast_args($AM_CALL_EXPR):ARRAY{STR}\n./Back/cgen.sa:4623:14");
 }
 ret_val = arg_list;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CGEN_e862722519(CGEN self, dAM_CALL_EXPR arg) {
 ARRAYSTR ret_val;
 STR subexpr = ((STR) NULL);
 ARRAYSTR res;
 ARRAYARG sig_args = ((ARRAYARG) NULL);
 BOOL is_ext = BOOL_zero;
 BOOL is_ext_fortran = BOOL_zero;
 BOOL is_bnd = BOOL_zero;
 BOOL is_iter = BOOL_zero;
 BOOL might_be_macro = BOOL_zero;
 INT last1 = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 dAM_EXPR e;
 dAM_EXPR self_expr;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i = INT_zero;
 dAM_EXPR e1;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT i1 = INT_zero;
 dAM_EXPR e2;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val1;
 PROG psather_self;
 BOOL ret_val2 = BOOL_zero;
 PROG psather_self1;
 BOOL ret_val3 = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 PROG psather_self2;
 BOOL ret_val4 = BOOL_zero;
 dAM_CALL_EXPR L7;
 ARRAYSTR L8;
 INT L9;
 OB L10;
 INT L12;
 INT L14;
 dAM_CALL_EXPR L15;
 INT L16;
 INT L171_;
 INT L131_,L131_m;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 dAM_CALL_EXPR L22;
 BOOL L23;
 BOOL L241_;
 dAM_CALL_EXPR L25;
 AM_CALL_ARG L26;
 INT L271_;
 INT L28;
 dAM_CALL_EXPR L29;
 INT L30;
 INT L321_;
 INT L33;
 INT L341_;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 dAM_CALL_EXPR L38;
 BOOL L39;
 BOOL L401_;
 dAM_CALL_EXPR L42;
 AM_CALL_ARG L43;
 STR L44;
 BOOL L46;
 BOOL L47;
 STR L481_;
 dAM_CONST L49;
 STR L50;
 STR L531_;
 STR L54;
 extern STR name36;
 extern STR name16;
 BOOL L55;
 BOOL L561_;
 STR L571_;
 dAM_EXPR L58;
 STR L59;
 STR L621_;
 STR L63;
 extern STR name36;
 extern STR name16;
 dAM_CALL_EXPR L64;
 INT L65;
 BOOL L661_;
 INT L68;
 dAM_CALL_EXPR L69;
 INT L70;
 INT L711_;
 INT L671_,L671_m;
 dAM_CALL_EXPR L72;
 BOOL L73;
 BOOL L741_;
 dAM_CALL_EXPR L75;
 AM_CALL_ARG L76;
 BOOL L77;
 BOOL L78;
 dAM_CALL_EXPR L79;
 AM_CALL_ARG L80;
 INT L811_;
 INT L82;
 ARG L831_;
 ARG L84;
 STR L85;
 STR L87;
 dAM_CALL_EXPR L89;
 AM_CALL_ARG L90;
 INT L911_;
 INT L92;
 ARG L931_;
 ARG L94;
 STR L95;
 BOOL L97;
 BOOL L98;
 BOOL L99;
 BOOL L1001_;
 dAM_CALL_EXPR L101;
 AM_CALL_ARG L102;
 dAM_CALL_EXPR L103;
 AM_CALL_ARG L104;
 dAM_CALL_EXPR L105;
 AM_CALL_ARG L106;
 INT L1071_;
 INT L108;
 ARG L1091_;
 ARG L110;
 STR L111;
 STR L113;
 extern STR errorI265231944;
 INT L117;
 dAM_CALL_EXPR L118;
 INT L119;
 INT L1201_;
 INT L1161_,L1161_m;
 dAM_CALL_EXPR L121;
 AM_CALL_ARG L122;
 STR L123;
 dAM_CALL_EXPR L125;
 AM_CALL_ARG L126;
 ARG L1271_;
 ARG L128;
 STR L129;
 BOOL L132;
 BOOL L133;
 BOOL L134;
 BOOL L1351_;
 dAM_CALL_EXPR L136;
 AM_CALL_ARG L137;
 dAM_CALL_EXPR L138;
 AM_CALL_ARG L139;
 dAM_CALL_EXPR L140;
 AM_CALL_ARG L141;
 ARG L1421_;
 ARG L143;
 STR L144;
 STR L146;
 create_self = ((ARRAYSTR) NULL);
 L7 = arg;
 create_n = (*dAM_CA1128544926[TAG(L7)])(L7);
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
 res = ret_val1;
 is_ext = FALSE;
 is_ext_fortran = FALSE;
 is_bnd = FALSE;
 is_iter = FALSE;
 might_be_macro = FALSE;
 switch (TAG(arg)) {
  case AM_EXT_CALL_EXPR_tag:
   is_ext = TRUE;
   sig_args = ATTR(ATTR(((AM_EXT_CALL_EXPR) arg),fun),args); break;
  case AM_ROU1916046290_tag:
   might_be_macro = SIG_is418491101(ATTR(((AM_ROU1916046290) arg),fun));
   sig_args = ATTR(ATTR(((AM_ROU1916046290) arg),fun),args); break;
  case AM_ITE1809135850_tag:
   might_be_macro = SIG_is418491101(ATTR(((AM_ITE1809135850) arg),fun));
   sig_args = ATTR(ATTR(((AM_ITE1809135850) arg),fun),args);
   is_iter = TRUE; break;
  case AM_BND260301329_tag:
   sig_args = ATTR(ATTR(((AM_BND260301329) arg),br_tp),args);
   is_bnd = TRUE; break;
  case AM_BND367211769_tag:
   sig_args = ATTR(ATTR(((AM_BND367211769) arg),bi_tp),args);
   is_bnd = TRUE;
   is_iter = TRUE; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_args($AM_CALL_EXPR):ARRAY{STR}\n./Back/cgen.sa:4268:14");
 }
 {
  BOOL f_L131_ = TRUE;
  while (1) {
   if (f_L131_) {
    f_L131_ = FALSE;
    L15 = arg;
    L16 = (*dAM_CA1128544926[TAG(L15)])(L15);
    L171_=INTMINUS(L16,1); 
    L11 = L171_;
    L21 = -1;
    L131_=L11+1;L131_m=L21; 
   }
   if(L131_--<=L131_m)  goto after_loop; 
   last1 = L131_;
   L181_=(last1)<(0); 
   L19 = L181_;
   L201_=!(L19); 
   if (L201_) {
   }
   else {
    goto after_loop;
   }
   L22 = arg;
   L23 = ((*dAM_CA967858872[TAG(L22)])(L22, last1)==((AM_CALL_ARG) NULL));
   L241_=!(L23); 
   if (L241_) {
    L25 = arg;
    L26=(*dAM_CA967858872[TAG(L25)])(L25, last1);
    e = ATTR(L26,expr);
    if (e==NULL) {
     goto other360;
    } else
    switch (TAG(e)) {
     case AM_BND367211769_tag:
     case AM_BND260301329_tag:
     case AM_EXT_CALL_EXPR_tag:
     case AM_ITE1809135850_tag:
     case AM_ROU1916046290_tag:
      goto after_loop; break;
     default: ;
     other360: ;
    }
   }
  }
 }
 after_loop: ;
 L271_=INTPLUS(last1,1); 
 L28 = L271_;
 L29 = arg;
 L30 = (*dAM_CA1128544926[TAG(L29)])(L29);
 L321_=INTMINUS(L30,1); 
 L33 = L321_;
 L341_=(L28<L33)?L28:L33; 
 last1 = L341_;
 L351_=!(is_bnd); 
 if (L351_) {
  L371_=!(is_ext); 
  if (L371_) {
   L38 = arg;
   L39 = ((*dAM_CA967858872[TAG(L38)])(L38, 0)==((AM_CALL_ARG) NULL));
   L401_=!(L39); 
   L36 = L401_;
  } else {
   L36 = FALSE;
  }
  if (L36) {
   L42 = arg;
   L43=(*dAM_CA967858872[TAG(L42)])(L42, 0);
   self_expr = ATTR(L43,expr);
   L44 = CGEN_e1639504572(self, self_expr);
   SARR((ARRAYSTR)res,0,(STR)L44); 
   ;
   if (self_expr==NULL) {
    goto other361;
   } else
   switch (TAG(self_expr)) {
    case AM_LOCAL_EXPR_tag: break;
    case AM_ARR_CONST_tag:
    case AM_BOOL_CONST_tag:
    case AM_CHAR_CONST_tag:
    case AM_CONST_tag:
    case AM_FLTDX_CONST_tag:
    case AM_FLTD_CONST_tag:
    case AM_FLTI_CONST_tag:
    case AM_FLTX_CONST_tag:
    case AM_FLT_CONST_tag:
    case AM_INTI_CONST_tag:
    case AM_INT_CONST_tag:
    case AM_STR_CONST_tag:
    case AM_VOID_CONST_tag:
     psather_self1 = ATTR(self,prog);
     if (ATTR(psather_self1,threads)) {
      L47 = TRUE;
     } else {
      L47 = ATTR(psather_self1,distributed);
     }
     ret_val3 = L47;
     if (ret_val3) {
      L46 = might_be_macro;
     } else {
      L46 = FALSE;
     }
     if (L46) {
      L481_=(STR)ARR((ARRAYSTR)res,0); 
      subexpr = L481_;
      L49 = ((dAM_CONST) self_expr);
      L50 = CGEN_d1425664957(self, (*dAM_CONST_tprdTP[TAG(L49)])(L49));
      SARR((ARRAYSTR)res,0,(STR)L50); 
      ;
      ndefer_self = self;
      L531_=(STR)ARR((ARRAYSTR)res,0); 
      L54 = L531_;
      ndefer_s = STR_ap1417971546(L54, ((STR) &name36), subexpr, ((STR) &name16));
      CGEN_d695831906(ndefer_self);
      SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     } break;
    default: ;
    other361: ;
     if (might_be_macro) {
      L55 = TRUE;
     } else {
      L561_=(0)<(last1); 
      L55 = L561_;
     }
     if (L55) {
      L571_=(STR)ARR((ARRAYSTR)res,0); 
      subexpr = L571_;
      L58 = self_expr;
      L59 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L58)])(L58));
      SARR((ARRAYSTR)res,0,(STR)L59); 
      ;
      ndefer_self1 = self;
      L621_=(STR)ARR((ARRAYSTR)res,0); 
      L63 = L621_;
      ndefer_s1 = STR_ap1417971546(L63, ((STR) &name36), subexpr, ((STR) &name16));
      CGEN_d695831906(ndefer_self1);
      SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
     }
   }
  }
  L64 = arg;
  L65 = (*dAM_CA1128544926[TAG(L64)])(L64);
  L661_=(1)<(L65); 
  if (L661_) {
   {
    BOOL f_L671_ = TRUE;
    while (1) {
     if (f_L671_) {
      f_L671_ = FALSE;
      L31 = 1;
      L69 = arg;
      L70 = (*dAM_CA1128544926[TAG(L69)])(L69);
      L711_=INTMINUS(L70,1); 
      L41 = L711_;
      L671_=L31-1;L671_m=L41; 
     }
     if(L671_++>=L671_m)  goto after_loop1; 
     i = L671_;
     L72 = arg;
     L73 = ((*dAM_CA967858872[TAG(L72)])(L72, i)==((AM_CALL_ARG) NULL));
     L741_=!(L73); 
     if (L741_) {
      L75 = arg;
      L76=(*dAM_CA967858872[TAG(L75)])(L75, i);
      e1 = ATTR(L76,expr);
      if (e1==NULL) {
       goto other362;
      } else
      switch (TAG(e1)) {
       case AM_ARR_CONST_tag:
       case AM_BOOL_CONST_tag:
       case AM_CHAR_CONST_tag:
       case AM_CONST_tag:
       case AM_FLTDX_CONST_tag:
       case AM_FLTD_CONST_tag:
       case AM_FLTI_CONST_tag:
       case AM_FLTX_CONST_tag:
       case AM_FLT_CONST_tag:
       case AM_INTI_CONST_tag:
       case AM_INT_CONST_tag:
       case AM_STR_CONST_tag:
       case AM_VOID_CONST_tag:
        psather_self2 = ATTR(self,prog);
        if (ATTR(psather_self2,threads)) {
         L78 = TRUE;
        } else {
         L78 = ATTR(psather_self2,distributed);
        }
        ret_val4 = L78;
        if (ret_val4) {
         L77 = might_be_macro;
        } else {
         L77 = FALSE;
        }
        if (L77) {
         L79 = arg;
         L80 = (*dAM_CA967858872[TAG(L79)])(L79, i);
         L811_=INTMINUS(i,1); 
         L82 = L811_;
         L831_=(ARG)ARR((ARRAYARG)sig_args,L82); 
         L84 = L831_;
         L85 = CGEN_e414796278(self, L80, L84, TRUE, FALSE);
         SARR((ARRAYSTR)res,i,(STR)L85); 
         ;
        }
        else {
         L87 = CGEN_e1639504572(self, e1);
         SARR((ARRAYSTR)res,i,(STR)L87); 
         ;
        } break;
       case AM_LOCAL_EXPR_tag:
        L89 = arg;
        L90 = (*dAM_CA967858872[TAG(L89)])(L89, i);
        L911_=INTMINUS(i,1); 
        L92 = L911_;
        L931_=(ARG)ARR((ARRAYARG)sig_args,L92); 
        L94 = L931_;
        L95 = CGEN_e414796278(self, L90, L94, FALSE, FALSE);
        SARR((ARRAYSTR)res,i,(STR)L95); 
        ; break;
       default: ;
       other362: ;
        if (might_be_macro) {
         L99 = TRUE;
        } else {
         L1001_=(i)<(last1); 
         L99 = L1001_;
        }
        if (L99) {
         L98 = TRUE;
        } else {
         L101 = arg;
         L102=(*dAM_CA967858872[TAG(L101)])(L101, i);
         L98 = SYSOBEQ(((OB) ATTR(L102,mode1)),((OB) MODES_out_mode));
        }
        if (L98) {
         L97 = TRUE;
        } else {
         L103 = arg;
         L104=(*dAM_CA967858872[TAG(L103)])(L103, i);
         L97 = SYSOBEQ(((OB) ATTR(L104,mode1)),((OB) MODES_inout_mode));
        }
        if (L97) {
         L105 = arg;
         L106 = (*dAM_CA967858872[TAG(L105)])(L105, i);
         L1071_=INTMINUS(i,1); 
         L108 = L1071_;
         L1091_=(ARG)ARR((ARRAYARG)sig_args,L108); 
         L110 = L1091_;
         L111 = CGEN_e414796278(self, L106, L110, TRUE, FALSE);
         SARR((ARRAYSTR)res,i,(STR)L111); 
         ;
        }
        else {
         L113 = CGEN_e1639504572(self, e1);
         SARR((ARRAYSTR)res,i,(STR)L113); 
         ;
        }
      }
     }
     else {
      SARR((ARRAYSTR)res,i,(STR)((STR) &errorI265231944)); 
      ;
     }
    }
   }
   after_loop1: ;
  }
 }
 else {
  {
   BOOL f_L1161_ = TRUE;
   while (1) {
    if (f_L1161_) {
     f_L1161_ = FALSE;
     L51 = 0;
     L118 = arg;
     L119 = (*dAM_CA1128544926[TAG(L118)])(L118);
     L1201_=INTMINUS(L119,1); 
     L61 = L1201_;
     L1161_=L51-1;L1161_m=L61; 
    }
    if(L1161_++>=L1161_m)  goto after_loop2; 
    i1 = L1161_;
    L121 = arg;
    L122=(*dAM_CA967858872[TAG(L121)])(L121, i1);
    e2 = ATTR(L122,expr);
    if (e2==NULL) {
     goto other363;
    } else
    switch (TAG(e2)) {
     case AM_ARR_CONST_tag:
     case AM_BOOL_CONST_tag:
     case AM_CHAR_CONST_tag:
     case AM_CONST_tag:
     case AM_FLTDX_CONST_tag:
     case AM_FLTD_CONST_tag:
     case AM_FLTI_CONST_tag:
     case AM_FLTX_CONST_tag:
     case AM_FLT_CONST_tag:
     case AM_INTI_CONST_tag:
     case AM_INT_CONST_tag:
     case AM_STR_CONST_tag:
     case AM_VOID_CONST_tag:
      L123 = CGEN_e1639504572(self, e2);
      SARR((ARRAYSTR)res,i1,(STR)L123); 
      ; break;
     case AM_LOCAL_EXPR_tag:
      L125 = arg;
      L126 = (*dAM_CA967858872[TAG(L125)])(L125, i1);
      L1271_=(ARG)ARR((ARRAYARG)sig_args,i1); 
      L128 = L1271_;
      L129 = CGEN_e414796278(self, L126, L128, FALSE, FALSE);
      SARR((ARRAYSTR)res,i1,(STR)L129); 
      ; break;
     default: ;
     other363: ;
      if (might_be_macro) {
       L134 = TRUE;
      } else {
       L1351_=(i1)<(last1); 
       L134 = L1351_;
      }
      if (L134) {
       L133 = TRUE;
      } else {
       L136 = arg;
       L137=(*dAM_CA967858872[TAG(L136)])(L136, i1);
       L133 = SYSOBEQ(((OB) ATTR(L137,mode1)),((OB) MODES_out_mode));
      }
      if (L133) {
       L132 = TRUE;
      } else {
       L138 = arg;
       L139=(*dAM_CA967858872[TAG(L138)])(L138, i1);
       L132 = SYSOBEQ(((OB) ATTR(L139,mode1)),((OB) MODES_inout_mode));
      }
      if (L132) {
       L140 = arg;
       L141 = (*dAM_CA967858872[TAG(L140)])(L140, i1);
       L1421_=(ARG)ARR((ARRAYARG)sig_args,i1); 
       L143 = L1421_;
       L144 = CGEN_e414796278(self, L141, L143, TRUE, FALSE);
       SARR((ARRAYSTR)res,i1,(STR)L144); 
       ;
      }
      else {
       L146 = CGEN_e1639504572(self, e2);
       SARR((ARRAYSTR)res,i1,(STR)L146); 
       ;
      }
    }
   }
  }
  after_loop2: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_n1296275061(CGEN self, dAM_CALL_EXPR ce) {
 BOOL ret_val = BOOL_zero;
 AM_CALL_ARG arg = ((AM_CALL_ARG) NULL);
 ARG sig_arg = ((ARG) NULL);
 ARRAYARG sig_args = ((ARRAYARG) NULL);
 BOOL is_ext = BOOL_zero;
 BOOL is_bnd = BOOL_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 dAM_EXPR a;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 BOOL L51_;
 INT L7;
 dAM_CALL_EXPR L8;
 INT L9;
 INT L101_;
 INT L61_,L61_m;
 dAM_CALL_EXPR L12;
 BOOL L13;
 dMODE L14;
 dMODE L15;
 INT L161_;
 INT L17;
 ARG L181_;
 BOOL L19;
 BOOL L20;
 dMODE L22;
 dMODE L23;
 BOOL L24;
 BOOL L25;
 dTP L26;
 dAM_EXPR L27;
 dTP L28;
 dMODE L29;
 INT L32;
 dAM_CALL_EXPR L33;
 INT L34;
 INT L351_;
 INT L301_,L301_m;
 dAM_CALL_EXPR L36;
 ARG L371_;
 BOOL L38;
 BOOL L39;
 dTP L40;
 dAM_EXPR L42;
 dTP L43;
 dMODE L44;
 is_ext = FALSE;
 is_bnd = FALSE;
 switch (TAG(ce)) {
  case AM_EXT_CALL_EXPR_tag:
   sig_args = ATTR(ATTR(((AM_EXT_CALL_EXPR) ce),fun),args);
   is_ext = TRUE; break;
  case AM_ROU1916046290_tag:
   sig_args = ATTR(ATTR(((AM_ROU1916046290) ce),fun),args); break;
  case AM_ITE1809135850_tag:
   sig_args = ATTR(ATTR(((AM_ITE1809135850) ce),fun),args); break;
  case AM_BND260301329_tag:
   sig_args = ATTR(ATTR(((AM_BND260301329) ce),br_tp),args);
   is_bnd = TRUE; break;
  case AM_BND367211769_tag:
   sig_args = ATTR(ATTR(((AM_BND367211769) ce),bi_tp),args);
   is_bnd = TRUE; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::need_caller_copy_out($AM_CALL_EXPR):BOOL\n./Back/cgen.sa:4458:14");
 }
 L51_=!(is_bnd); 
 if (L51_) {
  {
   BOOL f_L61_ = TRUE;
   while (1) {
    if (f_L61_) {
     f_L61_ = FALSE;
     L11 = 1;
     L8 = ce;
     L9 = (*dAM_CA1128544926[TAG(L8)])(L8);
     L101_=INTMINUS(L9,1); 
     L21 = L101_;
     L61_=L11-1;L61_m=L21; 
    }
    if(L61_++>=L61_m)  goto after_loop; 
    i = L61_;
    L12 = ce;
    arg = (*dAM_CA967858872[TAG(L12)])(L12, i);
    a = ATTR(arg,expr);
    if (a==NULL) {
     goto other364;
    } else
    switch (TAG(a)) {
     case AM_SHARED_EXPR_tag:
      L14 = ATTR(arg,mode1);
      if ((*dMODE_814247358[TAG(L14)])(L14, ((dMODE) MODES_out_mode))) {
       L13 = TRUE;
      } else {
       L15 = ATTR(arg,mode1);
       L13 = (*dMODE_814247358[TAG(L15)])(L15, ((dMODE) MODES_inout_mode));
      }
      if (L13) {
       ret_val = TRUE;
       return ret_val;
      } break;
     default: ;
     other364: ;
    }
    L161_=INTMINUS(i,1); 
    L17 = L161_;
    L181_=(ARG)ARR((ARRAYARG)sig_args,L17); 
    sig_arg = L181_;
    if (is_ext) {
     L22 = ATTR(arg,mode1);
     L20 = (*dMODE_814247358[TAG(L22)])(L22, ((dMODE) MODES_out_mode));
    } else {
     L20 = FALSE;
    }
    if (L20) {
     L19 = TRUE;
    } else {
     L23 = ATTR(arg,mode1);
     L19 = (*dMODE_814247358[TAG(L23)])(L23, ((dMODE) MODES_inout_mode));
    }
    if (L19) {
     ret_val = TRUE;
     return ret_val;
    }
    L26 = ATTR(sig_arg,tp);
    if ((*dTP_is1334578382[TAG(L26)])(L26)) {
     L27 = ATTR(arg,expr);
     L28 = (*dAM_EXPR_tprdTP[TAG(L27)])(L27);
     L25 = (*dTP_is1811059018[TAG(L28)])(L28);
    } else {
     L25 = FALSE;
    }
    if (L25) {
     L29 = ATTR(sig_arg,mode1);
     L24 = (*dMODE_814247358[TAG(L29)])(L29, ((dMODE) MODES_out_mode));
    } else {
     L24 = FALSE;
    }
    if (L24) {
     ret_val = TRUE;
     return ret_val;
    }
   }
  }
  after_loop: ;
 }
 else {
  {
   BOOL f_L301_ = TRUE;
   while (1) {
    if (f_L301_) {
     f_L301_ = FALSE;
     L31 = 0;
     L33 = ce;
     L34 = (*dAM_CA1128544926[TAG(L33)])(L33);
     L351_=INTMINUS(L34,1); 
     L41 = L351_;
     L301_=L31-1;L301_m=L41; 
    }
    if(L301_++>=L301_m)  goto after_loop1; 
    i1 = L301_;
    L36 = ce;
    arg = (*dAM_CA967858872[TAG(L36)])(L36, i1);
    L371_=(ARG)ARR((ARRAYARG)sig_args,i1); 
    sig_arg = L371_;
    L40 = ATTR(sig_arg,tp);
    if ((*dTP_is1334578382[TAG(L40)])(L40)) {
     L42 = ATTR(arg,expr);
     L43 = (*dAM_EXPR_tprdTP[TAG(L42)])(L42);
     L39 = (*dTP_is1811059018[TAG(L43)])(L43);
    } else {
     L39 = FALSE;
    }
    if (L39) {
     L44 = ATTR(sig_arg,mode1);
     L38 = (*dMODE_814247358[TAG(L44)])(L44, ((dMODE) MODES_out_mode));
    } else {
     L38 = FALSE;
    }
    if (L38) {
     ret_val = TRUE;
     return ret_val;
    }
   }
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_d2002278524(CGEN self, dTP t) {
 STR ret_val;
 STR res;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val1;
 STR name111;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val2;
 NAMESPACE r;
 NAMESPACE create_self;
 NAMESPACE ret_val3;
 NAMESPACE r1;
 NAMEMAP create_self1;
 NAMEMAP ret_val4;
 FSETSTR create_self2;
 FSETSTR ret_val5;
 FMAPSTRINT create_self3;
 FMAPSTRINT ret_val6;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val7;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val8;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val9;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val10;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val11;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val12;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 NAMESPACE L1;
 OB L2;
 extern STR L;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 dSTR L7;
 OB L8;
 BOOL L9;
 BOOL L101_;
 FSTR L11;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 FSTR L15;
 dSTR L16;
 extern STR name77;
 BOOL L17;
 BOOL L181_;
 FSTR L19;
 dSTR L20;
 BOOL L21;
 BOOL L221_;
 FSTR L23;
 dSTR L24;
 dSTR L25;
 OB L26;
 FSTR L27;
 dSTR L28;
 genlocal_self = ATTR(self,mangler);
 genlocal_ns = ((OB) ATTR(self,current_sig));
 space_self = genlocal_self;
 space_ns = genlocal_ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self = ((NAMESPACE) NULL);
  L2=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=NAMESPACE_tag;
  L1 = ((NAMESPACE) L2);
  r1 = L1;
  create_self1 = ((NAMEMAP) NULL);
  ret_val4 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val4);
  create_self2 = ((FSETSTR) NULL);
  ret_val5 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val5);
  SATTR(r1,counter,0);
  create_self3 = ((FMAPSTRINT) NULL);
  ret_val6 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val6);
  ret_val3 = r1;
  r = ret_val3;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val2 = r;
 name111 = NAMESP180237388(ret_val2, ((STR) &L));
 SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name111));
 ret_val1 = name111;
 res = ret_val1;
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val7 = plus_self;
 plus_self1 = ret_val7;
 L8=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 memset(L8,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L8)->header.tag=CHAR_tag;
 L7 = (dSTR)((CHAR_boxed) L8);
 ((CHAR_boxed) L7)->immutable_part = ' ';
 plus_s1 = L7;
 L9 = (plus_s1==((dSTR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(plus_self1,ntext);
  L12 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
 }
 ret_val8 = plus_self1;
 plus_self2 = ret_val8;
 mang_self = self;
 mang_ob = ((OB) t);
 ret_val9 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val9);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val10 = plus_self2;
 plus_self3 = ret_val10;
 plus_s3 = ((dSTR) ((STR) &name77));
 L17 = (plus_s3==((dSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  L19 = ATTR(plus_self3,ntext);
  L20 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 }
 ret_val11 = plus_self3;
 plus_self4 = ret_val11;
 plus_s4 = ((dSTR) res);
 L21 = (plus_s4==((dSTR) NULL));
 L221_=!(L21); 
 if (L221_) {
  L23 = ATTR(plus_self4,ntext);
  L24 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L23, (*dSTR_strrSTR[TAG(L24)])(L24)));
 }
 ret_val12 = plus_self4;
 plus_self5 = ret_val12;
 L26=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 memset(L26,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L26)->header.tag=CHAR_tag;
 L25 = (dSTR)((CHAR_boxed) L26);
 ((CHAR_boxed) L25)->immutable_part = ';';
 plus_s5 = L25;
 L27 = ATTR(plus_self5,ntext);
 L28 = plus_s5;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1606764211(CGEN self, STR r, dAM_CALL_EXPR call11, SIG fun11, ARRAYSTR args, STR uniq, STR frame11, STR loop_index) {
 STR ret_val;
 STR res = ((STR) NULL);
 INT i = INT_zero;
 BOOL has_near = BOOL_zero;
 BOOL near_failed = BOOL_zero;
 CHAR L11 = CHAR_zero;
 BOOL cast = BOOL_zero;
 dTP tp = ((dTP) NULL);
 CHAR L21 = CHAR_zero;
 INT l = INT_zero;
 INT l1 = INT_zero;
 INT l2 = INT_zero;
 INT l3 = INT_zero;
 INT l4 = INT_zero;
 INT l5 = INT_zero;
 INT l6 = INT_zero;
 INT l7 = INT_zero;
 INT l8 = INT_zero;
 ARRAYdTP tparams;
 STR ru;
 INT L31 = INT_zero;
 STR arg_name;
 IDENT argi = IDENT_zero;
 ELT_TBL L41;
 ELT elt1;
 CHAR L51 = CHAR_zero;
 INT l9 = INT_zero;
 INT l10 = INT_zero;
 INT l11 = INT_zero;
 INT l12 = INT_zero;
 INT l13 = INT_zero;
 INT l14 = INT_zero;
 INT l15 = INT_zero;
 INT l16 = INT_zero;
 INT l17 = INT_zero;
 INT l18 = INT_zero;
 CHAR L61 = CHAR_zero;
 INT l19 = INT_zero;
 INT l20 = INT_zero;
 INT l21 = INT_zero;
 INT l22 = INT_zero;
 INT l23 = INT_zero;
 INT l24 = INT_zero;
 INT l25 = INT_zero;
 INT l26 = INT_zero;
 INT l27 = INT_zero;
 INT l28 = INT_zero;
 STR arg_name1;
 BOOL done = BOOL_zero;
 IDENT argi1 = IDENT_zero;
 ELT_TBL L71;
 ELT elt2;
 CHAR L81 = CHAR_zero;
 INT l29 = INT_zero;
 INT l30 = INT_zero;
 INT l31 = INT_zero;
 INT l32 = INT_zero;
 INT l33 = INT_zero;
 INT l34 = INT_zero;
 INT l35 = INT_zero;
 INT l36 = INT_zero;
 INT l37 = INT_zero;
 STR arg_name2;
 BOOL done11 = BOOL_zero;
 IDENT argi2 = IDENT_zero;
 ELT_TBL L91;
 ELT elt3;
 ARRAYdTP tparams1;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s;
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
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CHAR is_lt_self = CHAR_zero;
 CHAR is_lt_c = CHAR_zero;
 BOOL ret_val8 = BOOL_zero;
 CHAR is_lt_self1 = CHAR_zero;
 CHAR is_lt_c1 = CHAR_zero;
 BOOL ret_val9 = BOOL_zero;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val10;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val11;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val12;
 STR r2;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val13;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val14;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self8;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val15;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val17;
 STR r3;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val18;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val19;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val20;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val21;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val22;
 CGEN barf_self4;
 STR barf_msg4;
 PROG barf_self5;
 STR barf_msg5;
 PROG barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val23;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val24;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val25;
 INT str_self3 = INT_zero;
 STR ret_val26;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val27;
 FSTR str_self4;
 STR ret_val28;
 STR plus_self17;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val29;
 STR s3;
 CHAR str_self5 = CHAR_zero;
 STR ret_val30;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val31;
 STR r4;
 ELT name_self;
 IDENT ret_val32 = IDENT_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val33 = BOOL_zero;
 ELT ret_self;
 dTP ret_val34;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val35;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val36;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val37;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val38;
 STR plus_self22;
 STR plus_sarg18;
 STR ret_val39;
 STR plus_self23;
 STR plus_sarg19;
 STR ret_val40;
 CGEN barf_self6;
 STR barf_msg6;
 PROG barf_self7;
 STR barf_msg7;
 PROG barf_at_self3;
 STR barf_at_msg3;
 dPROG_ERR barf_at_at3;
 STR plus_self24;
 STR plus_sarg20;
 STR ret_val41;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 STR plus_self25;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val42;
 STR s4;
 CHAR str_self6 = CHAR_zero;
 STR ret_val43;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val44;
 STR r5;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val45;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val46;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val47;
 STR plus_self29;
 STR plus_sarg24;
 STR ret_val48;
 STR plus_self30;
 STR plus_sarg25;
 STR ret_val49;
 CGEN barf_self8;
 STR barf_msg8;
 PROG barf_self9;
 STR barf_msg9;
 PROG barf_at_self4;
 STR barf_at_msg4;
 dPROG_ERR barf_at_at4;
 STR plus_self31;
 STR plus_sarg26;
 STR ret_val50;
 UNIX exit_self4;
 INT exit_code4 = INT_zero;
 STR plus_self32;
 STR plus_sarg27;
 STR ret_val51;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val52;
 STR plus_self33;
 STR plus_sarg28;
 STR ret_val53;
 STR plus_self34;
 STR plus_sarg29;
 STR ret_val54;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val55;
 STR plus_self35;
 STR plus_sarg30;
 STR ret_val56;
 STR plus_self36;
 STR plus_sarg31;
 STR ret_val57;
 STR plus_self37;
 STR plus_sarg32;
 STR ret_val58;
 STR plus_self38;
 STR plus_sarg33;
 STR ret_val59;
 STR plus_self39;
 STR plus_sarg34;
 STR ret_val60;
 CGEN barf_self10;
 STR barf_msg10;
 PROG barf_self11;
 STR barf_msg11;
 PROG barf_at_self5;
 STR barf_at_msg5;
 dPROG_ERR barf_at_at5;
 STR plus_self40;
 STR plus_sarg35;
 STR ret_val61;
 UNIX exit_self5;
 INT exit_code5 = INT_zero;
 STR plus_self41;
 STR plus_sarg36;
 STR ret_val62;
 STR plus_self42;
 STR plus_sarg37;
 STR ret_val63;
 STR plus_self43;
 STR plus_sarg38;
 STR ret_val64;
 STR plus_self44;
 STR plus_sarg39;
 STR ret_val65;
 STR plus_self45;
 STR plus_sarg40;
 STR ret_val66;
 STR plus_self46;
 STR plus_sarg41;
 STR ret_val67;
 STR plus_self47;
 STR plus_sarg42;
 STR ret_val68;
 STR plus_self48;
 STR plus_sarg43;
 STR ret_val69;
 CGEN barf_self12;
 STR barf_msg12;
 PROG barf_self13;
 STR barf_msg13;
 PROG barf_at_self6;
 STR barf_at_msg6;
 dPROG_ERR barf_at_at6;
 STR plus_self49;
 STR plus_sarg44;
 STR ret_val70;
 UNIX exit_self6;
 INT exit_code6 = INT_zero;
 STR plus_self50;
 STR plus_sarg45;
 STR ret_val71;
 STR plus_self51;
 STR plus_sarg46;
 STR ret_val72;
 STR plus_self52;
 STR plus_sarg47;
 STR ret_val73;
 STR plus_self53;
 STR plus_sarg48;
 STR ret_val74;
 STR plus_self54;
 STR plus_sarg49;
 STR ret_val75;
 STR plus_self55;
 STR plus_sarg50;
 STR ret_val76;
 STR plus_self56;
 STR plus_sarg51;
 STR ret_val77;
 STR plus_self57;
 STR plus_sarg52;
 STR ret_val78;
 CGEN barf_self14;
 STR barf_msg14;
 PROG barf_self15;
 STR barf_msg15;
 PROG barf_at_self7;
 STR barf_at_msg7;
 dPROG_ERR barf_at_at7;
 STR plus_self58;
 STR plus_sarg53;
 STR ret_val79;
 UNIX exit_self7;
 INT exit_code7 = INT_zero;
 STR plus_self59;
 STR plus_sarg54;
 STR ret_val80;
 STR plus_self60;
 STR plus_sarg55;
 STR ret_val81;
 STR plus_self61;
 STR plus_sarg56;
 STR ret_val82;
 STR plus_self62;
 STR plus_sarg57;
 STR ret_val83;
 STR plus_self63;
 STR plus_sarg58;
 STR ret_val84;
 STR plus_self64;
 STR plus_sarg59;
 STR ret_val85;
 STR plus_self65;
 STR plus_sarg60;
 STR ret_val86;
 STR plus_self66;
 STR plus_sarg61;
 STR ret_val87;
 CGEN barf_self16;
 STR barf_msg16;
 PROG barf_self17;
 STR barf_msg17;
 PROG barf_at_self8;
 STR barf_at_msg8;
 dPROG_ERR barf_at_at8;
 STR plus_self67;
 STR plus_sarg62;
 STR ret_val88;
 UNIX exit_self8;
 INT exit_code8 = INT_zero;
 STR plus_self68;
 STR plus_sarg63;
 STR ret_val89;
 STR plus_self69;
 STR plus_sarg64;
 STR ret_val90;
 STR plus_self70;
 STR plus_sarg65;
 STR ret_val91;
 STR plus_self71;
 STR plus_sarg66;
 STR ret_val92;
 STR plus_self72;
 STR plus_sarg67;
 STR ret_val93;
 STR plus_self73;
 STR plus_sarg68;
 STR ret_val94;
 STR plus_self74;
 STR plus_sarg69;
 STR ret_val95;
 STR plus_self75;
 STR plus_sarg70;
 STR ret_val96;
 CGEN barf_self18;
 STR barf_msg18;
 PROG barf_self19;
 STR barf_msg19;
 PROG barf_at_self9;
 STR barf_at_msg9;
 dPROG_ERR barf_at_at9;
 STR plus_self76;
 STR plus_sarg71;
 STR ret_val97;
 UNIX exit_self9;
 INT exit_code9 = INT_zero;
 STR plus_self77;
 STR plus_sarg72;
 STR ret_val98;
 STR plus_self78;
 STR plus_sarg73;
 STR ret_val99;
 STR plus_self79;
 STR plus_sarg74;
 STR ret_val100;
 STR plus_self80;
 STR plus_sarg75;
 STR ret_val101;
 STR plus_self81;
 STR plus_sarg76;
 STR ret_val102;
 STR plus_self82;
 STR plus_sarg77;
 STR ret_val103;
 STR plus_self83;
 STR plus_sarg78;
 STR ret_val104;
 STR plus_self84;
 STR plus_sarg79;
 STR ret_val105;
 CGEN barf_self20;
 STR barf_msg20;
 PROG barf_self21;
 STR barf_msg21;
 PROG barf_at_self10;
 STR barf_at_msg10;
 dPROG_ERR barf_at_at10;
 STR plus_self85;
 STR plus_sarg80;
 STR ret_val106;
 UNIX exit_self10;
 INT exit_code10 = INT_zero;
 STR plus_self86;
 STR plus_sarg81;
 STR ret_val107;
 STR plus_self87;
 STR plus_sarg82;
 STR ret_val108;
 STR plus_self88;
 STR plus_sarg83;
 STR ret_val109;
 STR plus_self89;
 STR plus_sarg84;
 STR ret_val110;
 STR plus_self90;
 STR plus_sarg85;
 STR ret_val1111;
 STR plus_self91;
 STR plus_sarg86;
 STR ret_val112;
 STR plus_self92;
 STR plus_sarg87;
 STR ret_val113;
 STR plus_self93;
 STR plus_sarg88;
 STR ret_val114;
 CGEN barf_self22;
 STR barf_msg22;
 PROG barf_self23;
 STR barf_msg23;
 PROG barf_at_self11;
 STR barf_at_msg11;
 dPROG_ERR barf_at_at11;
 STR plus_self94;
 STR plus_sarg89;
 STR ret_val115;
 UNIX exit_self11;
 INT exit_code11 = INT_zero;
 STR plus_self95;
 STR plus_sarg90;
 STR ret_val116;
 STR plus_self96;
 STR plus_sarg91;
 STR ret_val117;
 STR plus_self97;
 STR plus_sarg92;
 STR ret_val118;
 STR plus_self98;
 STR plus_sarg93;
 STR ret_val119;
 STR plus_self99;
 STR plus_sarg94;
 STR ret_val120;
 STR plus_self100;
 STR plus_sarg95;
 STR ret_val121;
 STR plus_self101;
 STR plus_sarg96;
 STR ret_val122;
 STR plus_self102;
 STR plus_sarg97;
 STR ret_val123;
 CGEN barf_self24;
 STR barf_msg24;
 PROG barf_self25;
 STR barf_msg25;
 PROG barf_at_self12;
 STR barf_at_msg12;
 dPROG_ERR barf_at_at12;
 STR plus_self103;
 STR plus_sarg98;
 STR ret_val124;
 UNIX exit_self12;
 INT exit_code12 = INT_zero;
 STR plus_self104;
 STR plus_sarg99;
 STR ret_val125;
 STR plus_self105;
 STR plus_sarg100;
 STR ret_val126;
 STR plus_self106;
 STR plus_sarg101;
 STR ret_val127;
 STR plus_self107;
 STR plus_sarg102;
 STR ret_val128;
 STR plus_self108;
 STR plus_sarg103;
 STR ret_val129;
 STR plus_self109;
 STR plus_sarg104;
 STR ret_val130;
 STR plus_self110;
 STR plus_sarg105;
 STR ret_val131;
 STR plus_self111;
 STR plus_sarg106;
 STR ret_val132;
 CGEN barf_self26;
 STR barf_msg26;
 PROG barf_self27;
 STR barf_msg27;
 PROG barf_at_self13;
 STR barf_at_msg13;
 dPROG_ERR barf_at_at13;
 STR plus_self112;
 STR plus_sarg107;
 STR ret_val133;
 UNIX exit_self13;
 INT exit_code13 = INT_zero;
 STR plus_self113;
 STR plus_sarg108;
 STR ret_val134;
 STR plus_self114;
 STR plus_sarg109;
 STR ret_val135;
 STR plus_self115;
 STR plus_sarg110;
 STR ret_val136;
 STR plus_self116;
 STR plus_sarg111;
 STR ret_val137;
 STR plus_self117;
 STR plus_sarg112;
 STR ret_val138;
 STR plus_self118;
 STR plus_sarg113;
 STR ret_val139;
 STR plus_self119;
 STR plus_sarg114;
 STR ret_val140;
 STR plus_self120;
 STR plus_sarg115;
 STR ret_val141;
 CGEN barf_self28;
 STR barf_msg28;
 PROG barf_self29;
 STR barf_msg29;
 PROG barf_at_self14;
 STR barf_at_msg14;
 dPROG_ERR barf_at_at14;
 STR plus_self121;
 STR plus_sarg116;
 STR ret_val142;
 UNIX exit_self14;
 INT exit_code14 = INT_zero;
 STR plus_self122;
 STR plus_sarg117;
 STR ret_val143;
 STR plus_self123;
 STR plus_sarg118;
 STR ret_val144;
 STR plus_self124;
 STR plus_sarg119;
 STR ret_val145;
 STR plus_self125;
 STR plus_sarg120;
 STR ret_val146;
 STR plus_self126;
 STR plus_sarg121;
 STR ret_val147;
 STR plus_self127;
 STR plus_sarg122;
 STR ret_val148;
 STR plus_self128;
 STR plus_sarg123;
 STR ret_val149;
 STR plus_self129;
 STR plus_sarg124;
 STR ret_val150;
 CGEN barf_self30;
 STR barf_msg30;
 PROG barf_self31;
 STR barf_msg31;
 PROG barf_at_self15;
 STR barf_at_msg15;
 dPROG_ERR barf_at_at15;
 STR plus_self130;
 STR plus_sarg125;
 STR ret_val151;
 UNIX exit_self15;
 INT exit_code15 = INT_zero;
 STR plus_self131;
 STR plus_sarg126;
 STR ret_val152;
 STR plus_self132;
 STR plus_sarg127;
 STR ret_val153;
 STR plus_self133;
 STR plus_sarg128;
 STR ret_val154;
 STR plus_self134;
 STR plus_sarg129;
 STR ret_val155;
 CGEN barf_self32;
 STR barf_msg32;
 PROG barf_self33;
 STR barf_msg33;
 PROG barf_at_self16;
 STR barf_at_msg16;
 dPROG_ERR barf_at_at16;
 STR plus_self135;
 STR plus_sarg130;
 STR ret_val156;
 UNIX exit_self16;
 INT exit_code16 = INT_zero;
 STR plus_self136;
 STR plus_sarg131;
 STR ret_val157;
 STR plus_self137;
 STR plus_sarg132;
 STR ret_val158;
 STR plus_self138;
 STR plus_sarg133;
 STR ret_val159;
 STR plus_self139;
 STR plus_sarg134;
 STR ret_val160;
 CGEN barf_self34;
 STR barf_msg34;
 PROG barf_self35;
 STR barf_msg35;
 PROG barf_at_self17;
 STR barf_at_msg17;
 dPROG_ERR barf_at_at17;
 STR plus_self140;
 STR plus_sarg135;
 STR ret_val161;
 UNIX exit_self17;
 INT exit_code17 = INT_zero;
 STR plus_self141;
 STR plus_sarg136;
 STR ret_val162;
 STR plus_self142;
 STR plus_sarg137;
 STR ret_val163;
 STR plus_self143;
 STR plus_sarg138;
 STR ret_val164;
 STR plus_self144;
 STR plus_sarg139;
 STR ret_val165;
 CGEN barf_self36;
 STR barf_msg36;
 PROG barf_self37;
 STR barf_msg37;
 PROG barf_at_self18;
 STR barf_at_msg18;
 dPROG_ERR barf_at_at18;
 STR plus_self145;
 STR plus_sarg140;
 STR ret_val166;
 UNIX exit_self18;
 INT exit_code18 = INT_zero;
 STR plus_self146;
 STR plus_sarg141;
 STR ret_val167;
 STR plus_self147;
 STR plus_sarg142;
 STR ret_val168;
 STR plus_self148;
 STR plus_sarg143;
 STR ret_val169;
 STR plus_self149;
 STR plus_sarg144;
 STR ret_val170;
 CGEN barf_self38;
 STR barf_msg38;
 PROG barf_self39;
 STR barf_msg39;
 PROG barf_at_self19;
 STR barf_at_msg19;
 dPROG_ERR barf_at_at19;
 STR plus_self150;
 STR plus_sarg145;
 STR ret_val171;
 UNIX exit_self19;
 INT exit_code19 = INT_zero;
 STR plus_self151;
 STR plus_sarg146;
 STR ret_val172;
 STR plus_self152;
 STR plus_sarg147;
 STR ret_val173;
 STR plus_self153;
 STR plus_sarg148;
 STR ret_val174;
 STR plus_self154;
 STR plus_sarg149;
 STR ret_val175;
 CGEN barf_self40;
 STR barf_msg40;
 PROG barf_self41;
 STR barf_msg41;
 PROG barf_at_self20;
 STR barf_at_msg20;
 dPROG_ERR barf_at_at20;
 STR plus_self155;
 STR plus_sarg150;
 STR ret_val176;
 UNIX exit_self20;
 INT exit_code20 = INT_zero;
 STR plus_self156;
 STR plus_sarg151;
 STR ret_val177;
 STR plus_self157;
 STR plus_sarg152;
 STR ret_val178;
 STR plus_self158;
 STR plus_sarg153;
 STR ret_val179;
 STR plus_self159;
 STR plus_sarg154;
 STR ret_val180;
 CGEN barf_self42;
 STR barf_msg42;
 PROG barf_self43;
 STR barf_msg43;
 PROG barf_at_self21;
 STR barf_at_msg21;
 dPROG_ERR barf_at_at21;
 STR plus_self160;
 STR plus_sarg155;
 STR ret_val181;
 UNIX exit_self21;
 INT exit_code21 = INT_zero;
 STR plus_self161;
 STR plus_sarg156;
 STR ret_val182;
 STR plus_self162;
 STR plus_sarg157;
 STR ret_val183;
 STR plus_self163;
 STR plus_sarg158;
 STR ret_val184;
 STR plus_self164;
 STR plus_sarg159;
 STR ret_val185;
 CGEN barf_self44;
 STR barf_msg44;
 PROG barf_self45;
 STR barf_msg45;
 PROG barf_at_self22;
 STR barf_at_msg22;
 dPROG_ERR barf_at_at22;
 STR plus_self165;
 STR plus_sarg160;
 STR ret_val186;
 UNIX exit_self22;
 INT exit_code22 = INT_zero;
 STR plus_self166;
 STR plus_sarg161;
 STR ret_val187;
 STR plus_self167;
 STR plus_sarg162;
 STR ret_val188;
 STR plus_self168;
 STR plus_sarg163;
 STR ret_val189;
 STR plus_self169;
 STR plus_sarg164;
 STR ret_val190;
 CGEN barf_self46;
 STR barf_msg46;
 PROG barf_self47;
 STR barf_msg47;
 PROG barf_at_self23;
 STR barf_at_msg23;
 dPROG_ERR barf_at_at23;
 STR plus_self170;
 STR plus_sarg165;
 STR ret_val191;
 UNIX exit_self23;
 INT exit_code23 = INT_zero;
 STR plus_self171;
 STR plus_sarg166;
 STR ret_val192;
 STR plus_self172;
 STR plus_sarg167;
 STR ret_val193;
 STR plus_self173;
 STR plus_sarg168;
 STR ret_val194;
 STR plus_self174;
 STR plus_sarg169;
 STR ret_val195;
 CGEN barf_self48;
 STR barf_msg48;
 PROG barf_self49;
 STR barf_msg49;
 PROG barf_at_self24;
 STR barf_at_msg24;
 dPROG_ERR barf_at_at24;
 STR plus_self175;
 STR plus_sarg170;
 STR ret_val196;
 UNIX exit_self24;
 INT exit_code24 = INT_zero;
 STR plus_self176;
 STR plus_sarg171;
 STR ret_val197;
 STR plus_self177;
 STR plus_sarg172;
 STR ret_val198;
 STR plus_self178;
 STR plus_sarg173;
 STR ret_val199;
 STR plus_self179;
 STR plus_sarg174;
 STR ret_val200;
 STR plus_self180;
 STR plus_sarg175;
 STR ret_val201;
 STR plus_self181;
 STR plus_sarg176;
 STR ret_val202;
 STR plus_self182;
 STR plus_sarg177;
 STR ret_val203;
 STR plus_self183;
 STR plus_sarg178;
 STR ret_val204;
 STR plus_self184;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val205;
 STR s5;
 CHAR str_self7 = CHAR_zero;
 STR ret_val206;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val207;
 STR r6;
 ELT name_self1;
 IDENT ret_val208 = IDENT_zero;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val209 = BOOL_zero;
 ELT tp_self;
 dTP ret_val210;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns;
 STR ret_val211;
 STR plus_self185;
 STR plus_sarg179;
 STR ret_val212;
 STR plus_self186;
 STR plus_sarg180;
 STR ret_val213;
 STR plus_self187;
 STR plus_sarg181;
 STR ret_val214;
 STR plus_self188;
 STR plus_sarg182;
 STR ret_val215;
 STR plus_self189;
 STR plus_sarg183;
 STR ret_val216;
 STR plus_self190;
 STR plus_sarg184;
 STR ret_val217;
 STR plus_self191;
 STR plus_sarg185;
 STR ret_val218;
 CGEN barf_self50;
 STR barf_msg50;
 PROG barf_self51;
 STR barf_msg51;
 PROG barf_at_self25;
 STR barf_at_msg25;
 dPROG_ERR barf_at_at25;
 STR plus_self192;
 STR plus_sarg186;
 STR ret_val219;
 UNIX exit_self25;
 INT exit_code25 = INT_zero;
 STR plus_self193;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val220;
 STR s6;
 CHAR str_self8 = CHAR_zero;
 STR ret_val221;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val222;
 STR r7;
 STR plus_self194;
 STR plus_sarg187;
 STR ret_val223;
 STR plus_self195;
 STR plus_sarg188;
 STR ret_val224;
 STR plus_self196;
 STR plus_sarg189;
 STR ret_val225;
 STR plus_self197;
 STR plus_sarg190;
 STR ret_val226;
 STR plus_self198;
 STR plus_sarg191;
 STR ret_val227;
 CGEN barf_self52;
 STR barf_msg52;
 PROG barf_self53;
 STR barf_msg53;
 PROG barf_at_self26;
 STR barf_at_msg26;
 dPROG_ERR barf_at_at26;
 STR plus_self199;
 STR plus_sarg192;
 STR ret_val228;
 UNIX exit_self26;
 INT exit_code26 = INT_zero;
 STR plus_self200;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val229;
 STR s7;
 CHAR str_self9 = CHAR_zero;
 STR ret_val230;
 STR create_self7;
 CHAR create_c7 = CHAR_zero;
 STR ret_val231;
 STR r8;
 STR plus_self201;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val232;
 STR s8;
 CHAR str_self10 = CHAR_zero;
 STR ret_val233;
 STR create_self8;
 CHAR create_c8 = CHAR_zero;
 STR ret_val234;
 STR r9;
 STR plus_self202;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val235;
 STR s9;
 CHAR str_self11 = CHAR_zero;
 STR ret_val236;
 STR create_self9;
 CHAR create_c9 = CHAR_zero;
 STR ret_val237;
 STR r10;
 STR plus_self203;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val238;
 STR s10;
 CHAR str_self12 = CHAR_zero;
 STR ret_val239;
 STR create_self10;
 CHAR create_c10 = CHAR_zero;
 STR ret_val240;
 STR r11;
 STR plus_self204;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val241;
 STR s11;
 CHAR str_self13 = CHAR_zero;
 STR ret_val242;
 STR create_self11;
 CHAR create_c11 = CHAR_zero;
 STR ret_val243;
 STR r12;
 STR plus_self205;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val244;
 STR s12;
 CHAR str_self14 = CHAR_zero;
 STR ret_val245;
 STR create_self12;
 CHAR create_c12 = CHAR_zero;
 STR ret_val246;
 STR r13;
 STR plus_self206;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val247;
 STR s13;
 CHAR str_self15 = CHAR_zero;
 STR ret_val248;
 STR create_self13;
 CHAR create_c13 = CHAR_zero;
 STR ret_val249;
 STR r14;
 STR plus_self207;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val250;
 STR s14;
 CHAR str_self16 = CHAR_zero;
 STR ret_val251;
 STR create_self14;
 CHAR create_c14 = CHAR_zero;
 STR ret_val252;
 STR r15;
 STR plus_self208;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val253;
 STR s15;
 CHAR str_self17 = CHAR_zero;
 STR ret_val254;
 STR create_self15;
 CHAR create_c15 = CHAR_zero;
 STR ret_val255;
 STR r16;
 STR plus_self209;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val256;
 STR s16;
 CHAR str_self18 = CHAR_zero;
 STR ret_val257;
 STR create_self16;
 CHAR create_c16 = CHAR_zero;
 STR ret_val258;
 STR r17;
 STR plus_self210;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val259;
 STR s17;
 CHAR str_self19 = CHAR_zero;
 STR ret_val260;
 STR create_self17;
 CHAR create_c17 = CHAR_zero;
 STR ret_val261;
 STR r18;
 STR plus_self211;
 CHAR plus_arg18 = CHAR_zero;
 STR ret_val262;
 STR s18;
 CHAR str_self20 = CHAR_zero;
 STR ret_val263;
 STR create_self18;
 CHAR create_c18 = CHAR_zero;
 STR ret_val264;
 STR r19;
 STR plus_self212;
 CHAR plus_arg19 = CHAR_zero;
 STR ret_val265;
 STR s19;
 CHAR str_self21 = CHAR_zero;
 STR ret_val266;
 STR create_self19;
 CHAR create_c19 = CHAR_zero;
 STR ret_val267;
 STR r20;
 STR plus_self213;
 CHAR plus_arg20 = CHAR_zero;
 STR ret_val268;
 STR s20;
 CHAR str_self22 = CHAR_zero;
 STR ret_val269;
 STR create_self20;
 CHAR create_c20 = CHAR_zero;
 STR ret_val270;
 STR r21;
 STR plus_self214;
 CHAR plus_arg21 = CHAR_zero;
 STR ret_val271;
 STR s21;
 CHAR str_self23 = CHAR_zero;
 STR ret_val272;
 STR create_self21;
 CHAR create_c21 = CHAR_zero;
 STR ret_val273;
 STR r22;
 STR plus_self215;
 CHAR plus_arg22 = CHAR_zero;
 STR ret_val274;
 STR s22;
 CHAR str_self24 = CHAR_zero;
 STR ret_val275;
 STR create_self22;
 CHAR create_c22 = CHAR_zero;
 STR ret_val276;
 STR r23;
 STR plus_self216;
 CHAR plus_arg23 = CHAR_zero;
 STR ret_val277;
 STR s23;
 CHAR str_self25 = CHAR_zero;
 STR ret_val278;
 STR create_self23;
 CHAR create_c23 = CHAR_zero;
 STR ret_val279;
 STR r24;
 STR plus_self217;
 CHAR plus_arg24 = CHAR_zero;
 STR ret_val280;
 STR s24;
 CHAR str_self26 = CHAR_zero;
 STR ret_val281;
 STR create_self24;
 CHAR create_c24 = CHAR_zero;
 STR ret_val282;
 STR r25;
 STR plus_self218;
 CHAR plus_arg25 = CHAR_zero;
 STR ret_val283;
 STR s25;
 CHAR str_self27 = CHAR_zero;
 STR ret_val284;
 STR create_self25;
 CHAR create_c25 = CHAR_zero;
 STR ret_val285;
 STR r26;
 STR plus_self219;
 CHAR plus_arg26 = CHAR_zero;
 STR ret_val286;
 STR s26;
 CHAR str_self28 = CHAR_zero;
 STR ret_val287;
 STR create_self26;
 CHAR create_c26 = CHAR_zero;
 STR ret_val288;
 STR r27;
 STR plus_self220;
 CHAR plus_arg27 = CHAR_zero;
 STR ret_val289;
 STR s27;
 CHAR str_self29 = CHAR_zero;
 STR ret_val290;
 STR create_self27;
 CHAR create_c27 = CHAR_zero;
 STR ret_val291;
 STR r28;
 STR plus_self221;
 CHAR plus_arg28 = CHAR_zero;
 STR ret_val292;
 STR s28;
 CHAR str_self30 = CHAR_zero;
 STR ret_val293;
 STR create_self28;
 CHAR create_c28 = CHAR_zero;
 STR ret_val294;
 STR r29;
 STR plus_self222;
 CHAR plus_arg29 = CHAR_zero;
 STR ret_val295;
 STR s29;
 CHAR str_self31 = CHAR_zero;
 STR ret_val296;
 STR create_self29;
 CHAR create_c29 = CHAR_zero;
 STR ret_val297;
 STR r30;
 STR plus_self223;
 CHAR plus_arg30 = CHAR_zero;
 STR ret_val298;
 STR s30;
 CHAR str_self32 = CHAR_zero;
 STR ret_val299;
 STR create_self30;
 CHAR create_c30 = CHAR_zero;
 STR ret_val300;
 STR r31;
 STR plus_self224;
 CHAR plus_arg31 = CHAR_zero;
 STR ret_val301;
 STR s31;
 CHAR str_self33 = CHAR_zero;
 STR ret_val302;
 STR create_self31;
 CHAR create_c31 = CHAR_zero;
 STR ret_val303;
 STR r32;
 STR plus_self225;
 CHAR plus_arg32 = CHAR_zero;
 STR ret_val304;
 STR s32;
 CHAR str_self34 = CHAR_zero;
 STR ret_val305;
 STR create_self32;
 CHAR create_c32 = CHAR_zero;
 STR ret_val306;
 STR r33;
 STR plus_self226;
 CHAR plus_arg33 = CHAR_zero;
 STR ret_val307;
 STR s33;
 CHAR str_self35 = CHAR_zero;
 STR ret_val308;
 STR create_self33;
 CHAR create_c33 = CHAR_zero;
 STR ret_val309;
 STR r34;
 STR plus_self227;
 CHAR plus_arg34 = CHAR_zero;
 STR ret_val310;
 STR s34;
 CHAR str_self36 = CHAR_zero;
 STR ret_val311;
 STR create_self34;
 CHAR create_c34 = CHAR_zero;
 STR ret_val312;
 STR r35;
 STR plus_self228;
 CHAR plus_arg35 = CHAR_zero;
 STR ret_val313;
 STR s35;
 CHAR str_self37 = CHAR_zero;
 STR ret_val314;
 STR create_self35;
 CHAR create_c35 = CHAR_zero;
 STR ret_val315;
 STR r36;
 STR plus_self229;
 CHAR plus_arg36 = CHAR_zero;
 STR ret_val316;
 STR s36;
 CHAR str_self38 = CHAR_zero;
 STR ret_val317;
 STR create_self36;
 CHAR create_c36 = CHAR_zero;
 STR ret_val318;
 STR r37;
 STR plus_self230;
 CHAR plus_arg37 = CHAR_zero;
 STR ret_val319;
 STR s37;
 CHAR str_self39 = CHAR_zero;
 STR ret_val320;
 STR create_self37;
 CHAR create_c37 = CHAR_zero;
 STR ret_val321;
 STR r38;
 ELT name_self2;
 IDENT ret_val322 = IDENT_zero;
 IDENT is_eq_self2 = IDENT_zero;
 IDENT is_eq_i2 = IDENT_zero;
 BOOL ret_val323 = BOOL_zero;
 ELT ret_self1;
 dTP ret_val324;
 ELT ret_self2;
 dTP ret_val325;
 STR plus_self231;
 CHAR plus_arg38 = CHAR_zero;
 STR ret_val326;
 STR s38;
 CHAR str_self40 = CHAR_zero;
 STR ret_val327;
 STR create_self38;
 CHAR create_c38 = CHAR_zero;
 STR ret_val328;
 STR r39;
 STR plus_self232;
 CHAR plus_arg39 = CHAR_zero;
 STR ret_val329;
 STR s39;
 CHAR str_self41 = CHAR_zero;
 STR ret_val330;
 STR create_self39;
 CHAR create_c39 = CHAR_zero;
 STR ret_val331;
 STR r40;
 STR plus_self233;
 CHAR plus_arg40 = CHAR_zero;
 STR ret_val332;
 STR s40;
 CHAR str_self42 = CHAR_zero;
 STR ret_val333;
 STR create_self40;
 CHAR create_c40 = CHAR_zero;
 STR ret_val334;
 STR r41;
 STR plus_self234;
 STR plus_sarg193;
 STR ret_val335;
 STR plus_self235;
 STR plus_sarg194;
 STR ret_val336;
 STR plus_self236;
 STR plus_sarg195;
 STR ret_val337;
 STR plus_self237;
 STR plus_sarg196;
 STR ret_val338;
 STR plus_self238;
 STR plus_sarg197;
 STR ret_val339;
 STR plus_self239;
 STR plus_sarg198;
 STR ret_val340;
 CGEN barf_self54;
 STR barf_msg54;
 PROG barf_self55;
 STR barf_msg55;
 PROG barf_at_self27;
 STR barf_at_msg27;
 dPROG_ERR barf_at_at27;
 STR plus_self240;
 STR plus_sarg199;
 STR ret_val341;
 UNIX exit_self27;
 INT exit_code27 = INT_zero;
 STR plus_self241;
 STR plus_sarg200;
 STR ret_val342;
 STR plus_self242;
 STR plus_sarg201;
 STR ret_val343;
 STR plus_self243;
 STR plus_sarg202;
 STR ret_val344;
 CGEN barf_self56;
 STR barf_msg56;
 PROG barf_self57;
 STR barf_msg57;
 PROG barf_at_self28;
 STR barf_at_msg28;
 dPROG_ERR barf_at_at28;
 STR plus_self244;
 STR plus_sarg203;
 STR ret_val345;
 UNIX exit_self28;
 INT exit_code28 = INT_zero;
 STR plus_self245;
 CHAR plus_arg41 = CHAR_zero;
 STR ret_val346;
 STR s41;
 CHAR str_self43 = CHAR_zero;
 STR ret_val347;
 STR create_self41;
 CHAR create_c41 = CHAR_zero;
 STR ret_val348;
 STR r42;
 STR plus_self246;
 CHAR plus_arg42 = CHAR_zero;
 STR ret_val349;
 STR s42;
 CHAR str_self44 = CHAR_zero;
 STR ret_val350;
 STR create_self42;
 CHAR create_c42 = CHAR_zero;
 STR ret_val351;
 STR r43;
 STR plus_self247;
 CHAR plus_arg43 = CHAR_zero;
 STR ret_val352;
 STR s43;
 CHAR str_self45 = CHAR_zero;
 STR ret_val353;
 STR create_self43;
 CHAR create_c43 = CHAR_zero;
 STR ret_val354;
 STR r44;
 CHAR is_lt_self2 = CHAR_zero;
 CHAR is_lt_c2 = CHAR_zero;
 BOOL ret_val355 = BOOL_zero;
 CHAR is_lt_self3 = CHAR_zero;
 CHAR is_lt_c3 = CHAR_zero;
 BOOL ret_val356 = BOOL_zero;
 STR plus_self248;
 CHAR plus_arg44 = CHAR_zero;
 STR ret_val357;
 STR s44;
 CHAR str_self46 = CHAR_zero;
 STR ret_val358;
 STR create_self44;
 CHAR create_c44 = CHAR_zero;
 STR ret_val359;
 STR r45;
 STR plus_self249;
 STR plus_sarg204;
 STR ret_val360;
 CGEN barf_self58;
 STR barf_msg58;
 PROG barf_self59;
 STR barf_msg59;
 PROG barf_at_self29;
 STR barf_at_msg29;
 dPROG_ERR barf_at_at29;
 STR plus_self250;
 STR plus_sarg205;
 STR ret_val361;
 UNIX exit_self29;
 INT exit_code29 = INT_zero;
 STR plus_self251;
 CHAR plus_arg45 = CHAR_zero;
 STR ret_val362;
 STR s45;
 CHAR str_self47 = CHAR_zero;
 STR ret_val363;
 STR create_self45;
 CHAR create_c45 = CHAR_zero;
 STR ret_val364;
 STR r46;
 STR plus_self252;
 STR plus_sarg206;
 STR ret_val365;
 STR plus_self253;
 STR plus_sarg207;
 STR ret_val366;
 STR plus_self254;
 STR plus_sarg208;
 STR ret_val367;
 STR plus_self255;
 STR plus_sarg209;
 STR ret_val368;
 STR plus_self256;
 STR plus_sarg210;
 STR ret_val369;
 CGEN barf_self60;
 STR barf_msg60;
 PROG barf_self61;
 STR barf_msg61;
 PROG barf_at_self30;
 STR barf_at_msg30;
 dPROG_ERR barf_at_at30;
 STR plus_self257;
 STR plus_sarg211;
 STR ret_val370;
 UNIX exit_self30;
 INT exit_code30 = INT_zero;
 STR plus_self258;
 CHAR plus_arg46 = CHAR_zero;
 STR ret_val371;
 STR s46;
 CHAR str_self48 = CHAR_zero;
 STR ret_val372;
 STR create_self46;
 CHAR create_c46 = CHAR_zero;
 STR ret_val373;
 STR r47;
 STR plus_self259;
 CHAR plus_arg47 = CHAR_zero;
 STR ret_val374;
 STR s47;
 CHAR str_self49 = CHAR_zero;
 STR ret_val375;
 STR create_self47;
 CHAR create_c47 = CHAR_zero;
 STR ret_val376;
 STR r48;
 STR plus_self260;
 CHAR plus_arg48 = CHAR_zero;
 STR ret_val377;
 STR s48;
 CHAR str_self50 = CHAR_zero;
 STR ret_val378;
 STR create_self48;
 CHAR create_c48 = CHAR_zero;
 STR ret_val379;
 STR r49;
 STR plus_self261;
 CHAR plus_arg49 = CHAR_zero;
 STR ret_val380;
 STR s49;
 CHAR str_self51 = CHAR_zero;
 STR ret_val381;
 STR create_self49;
 CHAR create_c49 = CHAR_zero;
 STR ret_val382;
 STR r50;
 STR plus_self262;
 CHAR plus_arg50 = CHAR_zero;
 STR ret_val383;
 STR s50;
 CHAR str_self52 = CHAR_zero;
 STR ret_val384;
 STR create_self50;
 CHAR create_c50 = CHAR_zero;
 STR ret_val385;
 STR r51;
 STR plus_self263;
 STR plus_sarg212;
 STR ret_val386;
 STR plus_self264;
 STR plus_sarg213;
 STR ret_val387;
 STR plus_self265;
 STR plus_sarg214;
 STR ret_val388;
 STR plus_self266;
 STR plus_sarg215;
 STR ret_val389;
 STR plus_self267;
 STR plus_sarg216;
 STR ret_val390;
 CGEN barf_self62;
 STR barf_msg62;
 PROG barf_self63;
 STR barf_msg63;
 PROG barf_at_self31;
 STR barf_at_msg31;
 dPROG_ERR barf_at_at31;
 STR plus_self268;
 STR plus_sarg217;
 STR ret_val391;
 UNIX exit_self31;
 INT exit_code31 = INT_zero;
 STR plus_self269;
 CHAR plus_arg51 = CHAR_zero;
 STR ret_val392;
 STR s51;
 CHAR str_self53 = CHAR_zero;
 STR ret_val393;
 STR create_self51;
 CHAR create_c51 = CHAR_zero;
 STR ret_val394;
 STR r52;
 INT L10;
 BOOL L121_;
 CHAR L131_;
 INT L141_;
 CHAR L151_;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 INT L221_;
 CHAR L231_;
 CHAR L24;
 BOOL L251_;
 INT L261_;
 CHAR L271_;
 CHAR L281_;
 CHAR L29;
 INT L301_;
 INT L32;
 INT L331_;
 INT L34;
 INT L351_;
 ARRAYARG L36;
 ARG L371_;
 ARG L38;
 CHAR L391_;
 CHAR L40;
 INT L421_;
 INT L43;
 INT L441_;
 INT L45;
 INT L461_;
 ARRAYARG L47;
 ARG L481_;
 ARG L49;
 CHAR L501_;
 CHAR L52;
 INT L531_;
 INT L54;
 INT L551_;
 INT L56;
 INT L571_;
 ARRAYARG L58;
 ARG L591_;
 ARG L60;
 CHAR L621_;
 CHAR L63;
 INT L641_;
 INT L65;
 INT L661_;
 INT L67;
 INT L681_;
 ARRAYARG L69;
 ARG L701_;
 ARG L72;
 CHAR L731_;
 CHAR L74;
 INT L751_;
 INT L76;
 INT L771_;
 INT L78;
 INT L791_;
 ARRAYARG L80;
 ARG L821_;
 ARG L83;
 CHAR L841_;
 CHAR L85;
 INT L861_;
 INT L87;
 INT L881_;
 INT L89;
 INT L901_;
 ARRAYARG L92;
 ARG L931_;
 ARG L94;
 CHAR L951_;
 CHAR L96;
 INT L971_;
 INT L98;
 INT L991_;
 INT L100;
 INT L1011_;
 ARRAYARG L102;
 ARG L1031_;
 ARG L104;
 CHAR L1051_;
 CHAR L106;
 INT L1071_;
 INT L108;
 INT L1091_;
 INT L110;
 INT L1111_;
 ARRAYARG L112;
 ARG L1131_;
 ARG L114;
 CHAR L1151_;
 CHAR L116;
 INT L1171_;
 INT L118;
 INT L1191_;
 INT L120;
 INT L1221_;
 ARRAYARG L123;
 ARG L1241_;
 ARG L125;
 extern STR risund1670463801;
 extern STR inCONFIGfor;
 extern STR Intern28965746;
 INT L1261_;
 BOOL L127;
 CHAR L1281_;
 INT L1291_;
 INT L130;
 INT L1321_;
 INT L133;
 BOOL L1341_;
 CHAR L1351_;
 INT L1361_;
 INT L137;
 INT L1381_;
 INT L139;
 BOOL L1401_;
 extern STR Pmustb23117515;
 CHAR L1421_;
 STR L143;
 INT L144;
 OB L145;
 INT L146;
 extern STR name9;
 extern STR Intern28965746;
 CHAR L1481_;
 CHAR L149;
 INT L1501_;
 INT L152;
 INT L1531_;
 INT L154;
 INT L1551_;
 INT L156;
 INT L1571_;
 INT L158;
 BOOL L1591_;
 BOOL L160;
 BOOL L1611_;
 extern STR P;
 CHAR L1621_;
 STR L163;
 INT L164;
 OB L165;
 INT L166;
 extern STR outofboundsmacro;
 extern STR sig2;
 extern STR name50;
 extern STR Intern28965746;
 CHAR L1681_;
 CHAR L169;
 INT L1701_;
 INT L171;
 INT L1721_;
 INT L173;
 INT L1741_;
 INT L175;
 dTP L1761_;
 extern STR name10;
 extern STR name1;
 extern STR LM_LOC1545209594;
 INT L177;
 INT L178;
 INT L1791_;
 extern STR goto1;
 extern STR do3;
 extern STR POP_EX889458020;
 extern STR while0;
 extern STR do3;
 extern STR POP_EXCEPTION11;
 INT L180;
 INT L181;
 INT L1821_;
 BOOL L183;
 BOOL L1841_;
 extern STR goto2;
 extern STR while0;
 extern STR name1;
 INT L1851_;
 BOOL L186;
 CHAR L1871_;
 CHAR L1881_;
 CHAR L189;
 BOOL L1901_;
 CHAR L1911_;
 STR L192;
 INT L193;
 OB L194;
 INT L195;
 INT L1971_;
 INT L1981_;
 ELT L199;
 dTP L200;
 IMPL L202;
 BOOL L203;
 BOOL L204;
 BOOL L205;
 extern STR unknwo513321933;
 extern STR inCONFIGmacrofor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 extern STR unknown1;
 CHAR L2061_;
 STR L207;
 INT L208;
 OB L209;
 INT L210;
 extern STR macroinCONFIGfor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 CHAR L2121_;
 CHAR L213;
 BOOL L2141_;
 BOOL L215;
 BOOL L2161_;
 dTP L217;
 BOOL L218;
 BOOL L2191_;
 extern STR name24;
 extern STR name26;
 INT L2201_;
 INT L2221_;
 extern STR S01;
 CHAR L2231_;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2241_;
 CHAR L225;
 INT L2261_;
 INT L227;
 INT L2281_;
 INT L229;
 INT L2301_;
 dAM_CALL_EXPR L232;
 AM_CALL_ARG L233;
 dAM_EXPR L234;
 extern STR S11;
 extern STR name93;
 STR L2351_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2361_;
 CHAR L237;
 INT L2381_;
 INT L239;
 INT L2401_;
 INT L241;
 INT L2421_;
 dAM_CALL_EXPR L243;
 AM_CALL_ARG L244;
 dAM_EXPR L245;
 extern STR S11;
 extern STR name93;
 STR L2461_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2471_;
 CHAR L248;
 INT L2491_;
 INT L250;
 INT L2521_;
 INT L253;
 INT L2541_;
 dAM_CALL_EXPR L255;
 AM_CALL_ARG L256;
 dAM_EXPR L257;
 extern STR S11;
 extern STR name93;
 STR L2581_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2591_;
 CHAR L260;
 INT L2621_;
 INT L263;
 INT L2641_;
 INT L265;
 INT L2661_;
 dAM_CALL_EXPR L267;
 AM_CALL_ARG L268;
 dAM_EXPR L269;
 extern STR S11;
 extern STR name93;
 STR L2701_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2721_;
 CHAR L273;
 INT L2741_;
 INT L275;
 INT L2761_;
 INT L277;
 INT L2781_;
 dAM_CALL_EXPR L279;
 AM_CALL_ARG L280;
 dAM_EXPR L282;
 extern STR S11;
 extern STR name93;
 STR L2831_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2841_;
 CHAR L285;
 INT L2861_;
 INT L287;
 INT L2881_;
 INT L289;
 INT L2901_;
 dAM_CALL_EXPR L291;
 AM_CALL_ARG L292;
 dAM_EXPR L293;
 extern STR S11;
 extern STR name93;
 STR L2941_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L2951_;
 CHAR L296;
 INT L2971_;
 INT L298;
 INT L2991_;
 INT L300;
 INT L3021_;
 dAM_CALL_EXPR L303;
 AM_CALL_ARG L304;
 dAM_EXPR L305;
 extern STR S11;
 extern STR name93;
 STR L3061_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3071_;
 CHAR L308;
 INT L3091_;
 INT L310;
 INT L3111_;
 INT L312;
 INT L3131_;
 dAM_CALL_EXPR L314;
 AM_CALL_ARG L315;
 dAM_EXPR L316;
 extern STR S11;
 extern STR name93;
 STR L3171_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3181_;
 CHAR L319;
 INT L3201_;
 INT L321;
 INT L3221_;
 INT L323;
 INT L3241_;
 dAM_CALL_EXPR L325;
 AM_CALL_ARG L326;
 dAM_EXPR L327;
 extern STR S11;
 extern STR name93;
 STR L3281_;
 extern STR name94;
 extern STR S01;
 extern STR ncanno888628440;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3291_;
 CHAR L330;
 INT L3321_;
 INT L333;
 INT L3341_;
 INT L335;
 INT L3361_;
 dAM_CALL_EXPR L337;
 AM_CALL_ARG L338;
 dAM_EXPR L339;
 extern STR S11;
 extern STR name93;
 STR L3401_;
 extern STR name94;
 extern STR S01;
 INT L3411_;
 CHAR L3421_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3431_;
 CHAR L344;
 INT L3451_;
 INT L346;
 INT L3471_;
 INT L348;
 INT L3491_;
 STR L3501_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3521_;
 CHAR L353;
 INT L3541_;
 INT L355;
 INT L3561_;
 INT L357;
 INT L3581_;
 STR L3591_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3601_;
 CHAR L361;
 INT L3621_;
 INT L363;
 INT L3641_;
 INT L365;
 INT L3661_;
 STR L3671_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3681_;
 CHAR L369;
 INT L3701_;
 INT L372;
 INT L3731_;
 INT L374;
 INT L3751_;
 STR L3761_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3771_;
 CHAR L378;
 INT L3791_;
 INT L380;
 INT L3811_;
 INT L382;
 INT L3831_;
 STR L3841_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3851_;
 CHAR L386;
 INT L3871_;
 INT L388;
 INT L3891_;
 INT L390;
 INT L3921_;
 STR L3931_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L3941_;
 CHAR L395;
 INT L3961_;
 INT L397;
 INT L3981_;
 INT L399;
 INT L4001_;
 STR L4011_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L4021_;
 CHAR L403;
 INT L4041_;
 INT L405;
 INT L4061_;
 INT L407;
 INT L4081_;
 STR L4091_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L4101_;
 CHAR L411;
 INT L4121_;
 INT L413;
 INT L4141_;
 INT L415;
 INT L4161_;
 STR L4171_;
 extern STR dncann144395436;
 extern STR inCONFIGfor1;
 extern STR Intern28965746;
 CHAR L4181_;
 CHAR L419;
 INT L4201_;
 INT L422;
 INT L4231_;
 INT L424;
 INT L4251_;
 STR L4261_;
 BOOL L427;
 BOOL L4281_;
 INT L4291_;
 INT L430;
 CHAR L4311_;
 extern STR i_;
 extern STR name1;
 INT L4321_;
 BOOL L433;
 CHAR L4341_;
 CHAR L4351_;
 CHAR L436;
 BOOL L4371_;
 CHAR L4381_;
 STR L439;
 INT L440;
 OB L442;
 INT L443;
 INT L4451_;
 INT L4461_;
 ELT L447;
 dTP L448;
 IMPL L449;
 BOOL L450;
 BOOL L451;
 BOOL L452;
 OB L453;
 OB L454;
 BOOL L4551_;
 extern STR unknwo513321933;
 extern STR inCONFIGmacrofor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 extern STR unknownd;
 CHAR L4561_;
 STR L457;
 INT L458;
 OB L459;
 INT L460;
 extern STR macroinCONFIGfor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 INT L4631_;
 CHAR L4641_;
 dTP L465;
 dTP L466;
 STR L467;
 INT L468;
 OB L469;
 INT L470;
 STR L472;
 INT L473;
 OB L474;
 INT L475;
 STR L477;
 INT L478;
 OB L479;
 INT L480;
 CHAR L4831_;
 CHAR L484;
 INT L4851_;
 INT L486;
 INT L4871_;
 INT L488;
 INT L4891_;
 ARRAYARG L490;
 ARG L4911_;
 ARG L492;
 dTP L493;
 ARRAYARG L494;
 ARG L4951_;
 ARG L496;
 dTP L497;
 STR L498;
 INT L499;
 OB L500;
 INT L502;
 STR L504;
 INT L505;
 OB L506;
 INT L507;
 STR L509;
 INT L510;
 OB L511;
 INT L512;
 CHAR L5141_;
 CHAR L515;
 INT L5161_;
 INT L517;
 INT L5181_;
 INT L519;
 INT L5201_;
 ARRAYARG L521;
 ARG L5221_;
 ARG L523;
 dTP L524;
 ARRAYARG L525;
 ARG L5261_;
 ARG L527;
 dTP L528;
 STR L529;
 INT L530;
 OB L532;
 INT L533;
 STR L535;
 INT L536;
 OB L537;
 INT L538;
 STR L540;
 INT L541;
 OB L542;
 INT L543;
 CHAR L5451_;
 CHAR L546;
 INT L5471_;
 INT L548;
 INT L5491_;
 INT L550;
 INT L5521_;
 ARRAYARG L553;
 ARG L5541_;
 ARG L555;
 dTP L556;
 ARRAYARG L557;
 ARG L5581_;
 ARG L559;
 dTP L560;
 STR L561;
 INT L562;
 OB L563;
 INT L564;
 STR L566;
 INT L567;
 OB L568;
 INT L569;
 STR L572;
 INT L573;
 OB L574;
 INT L575;
 CHAR L5771_;
 CHAR L578;
 INT L5791_;
 INT L580;
 INT L5811_;
 INT L582;
 INT L5831_;
 ARRAYARG L584;
 ARG L5851_;
 ARG L586;
 dTP L587;
 ARRAYARG L588;
 ARG L5891_;
 ARG L590;
 dTP L592;
 STR L593;
 INT L594;
 OB L595;
 INT L596;
 STR L598;
 INT L599;
 OB L600;
 INT L601;
 STR L603;
 INT L604;
 OB L605;
 INT L606;
 CHAR L6081_;
 CHAR L609;
 INT L6101_;
 INT L611;
 INT L6121_;
 INT L613;
 INT L6141_;
 ARRAYARG L615;
 ARG L6161_;
 ARG L617;
 dTP L618;
 ARRAYARG L619;
 ARG L6201_;
 ARG L622;
 dTP L623;
 STR L624;
 INT L625;
 OB L626;
 INT L627;
 STR L629;
 INT L630;
 OB L631;
 INT L632;
 STR L634;
 INT L635;
 OB L636;
 INT L637;
 CHAR L6391_;
 CHAR L640;
 INT L6421_;
 INT L643;
 INT L6441_;
 INT L645;
 INT L6461_;
 ARRAYARG L647;
 ARG L6481_;
 ARG L649;
 dTP L650;
 ARRAYARG L651;
 ARG L6521_;
 ARG L653;
 dTP L654;
 STR L655;
 INT L656;
 OB L657;
 INT L658;
 STR L660;
 INT L662;
 OB L663;
 INT L664;
 STR L666;
 INT L667;
 OB L668;
 INT L669;
 CHAR L6711_;
 CHAR L672;
 INT L6731_;
 INT L674;
 INT L6751_;
 INT L676;
 INT L6771_;
 ARRAYARG L678;
 ARG L6791_;
 ARG L680;
 dTP L682;
 ARRAYARG L683;
 ARG L6841_;
 ARG L685;
 dTP L686;
 STR L687;
 INT L688;
 OB L689;
 INT L690;
 STR L692;
 INT L693;
 OB L694;
 INT L695;
 STR L697;
 INT L698;
 OB L699;
 INT L700;
 CHAR L7031_;
 CHAR L704;
 INT L7051_;
 INT L706;
 INT L7071_;
 INT L708;
 INT L7091_;
 ARRAYARG L710;
 ARG L7111_;
 ARG L712;
 dTP L713;
 ARRAYARG L714;
 ARG L7151_;
 ARG L716;
 dTP L717;
 STR L718;
 INT L719;
 OB L720;
 INT L721;
 STR L723;
 INT L724;
 OB L725;
 INT L726;
 STR L728;
 INT L729;
 OB L730;
 INT L732;
 CHAR L7341_;
 CHAR L735;
 INT L7361_;
 INT L737;
 INT L7381_;
 INT L739;
 INT L7401_;
 ARRAYARG L741;
 ARG L7421_;
 ARG L743;
 dTP L744;
 ARRAYARG L745;
 ARG L7461_;
 ARG L747;
 dTP L748;
 STR L749;
 INT L750;
 OB L752;
 INT L753;
 STR L755;
 INT L756;
 OB L757;
 INT L758;
 STR L760;
 INT L761;
 OB L762;
 INT L763;
 extern STR name1;
 INT L7651_;
 BOOL L766;
 CHAR L7671_;
 CHAR L7681_;
 CHAR L769;
 BOOL L7701_;
 CHAR L7721_;
 STR L773;
 INT L774;
 OB L775;
 INT L776;
 INT L7781_;
 INT L7791_;
 ELT L780;
 dTP L781;
 IMPL L782;
 BOOL L783;
 BOOL L784;
 BOOL L785;
 dTP L786;
 dTP L787;
 STR L788;
 INT L789;
 OB L790;
 INT L792;
 STR L794;
 INT L795;
 OB L796;
 INT L797;
 STR L799;
 INT L800;
 OB L801;
 INT L802;
 BOOL L8041_;
 extern STR unknwo513321933;
 extern STR inCONFIGmacrofor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 extern STR risund16704638011;
 extern STR inCONFIGfor;
 extern STR Intern28965746;
 dTP L805;
 dTP L806;
 STR L807;
 INT L808;
 OB L809;
 INT L810;
 STR L812;
 INT L813;
 OB L814;
 INT L815;
 STR L817;
 INT L818;
 OB L819;
 INT L820;
 INT L8231_;
 BOOL L824;
 CHAR L8251_;
 INT L8261_;
 INT L827;
 INT L8281_;
 INT L829;
 BOOL L8301_;
 CHAR L8311_;
 INT L8321_;
 INT L833;
 INT L8341_;
 INT L835;
 BOOL L8361_;
 extern STR Pmustb231175151;
 CHAR L8371_;
 STR L838;
 INT L839;
 OB L840;
 INT L842;
 extern STR name9;
 extern STR Intern28965746;
 CHAR L8441_;
 CHAR L845;
 INT L8461_;
 INT L847;
 INT L8481_;
 INT L849;
 INT L8501_;
 INT L851;
 INT L8521_;
 INT L853;
 BOOL L8541_;
 BOOL L855;
 BOOL L8561_;
 extern STR P1;
 CHAR L8571_;
 STR L858;
 INT L859;
 OB L860;
 INT L862;
 extern STR outofboundsmacro;
 extern STR sig2;
 extern STR name50;
 extern STR Intern28965746;
 CHAR L8641_;
 CHAR L865;
 INT L8661_;
 INT L867;
 INT L8681_;
 INT L869;
 INT L8701_;
 INT L871;
 dTP L8721_;
 dTP L873;
 CHAR L8741_;
 CHAR L875;
 INT L8761_;
 INT L877;
 INT L8781_;
 INT L879;
 INT L8801_;
 INT L882;
 dTP L8831_;
 dTP L884;
 STR L885;
 INT L886;
 OB L887;
 INT L888;
 STR L890;
 INT L891;
 OB L892;
 INT L893;
 STR L895;
 INT L896;
 OB L897;
 INT L898;
 STR L900;
 INT L902;
 OB L903;
 INT L904;
 extern STR unknown2;
 CHAR L9061_;
 STR L907;
 INT L908;
 OB L909;
 INT L910;
 extern STR macroinCONFIGfor;
 extern STR name52;
 extern STR name26;
 extern STR Intern28965746;
 CHAR L9121_;
 STR L913;
 INT L914;
 OB L915;
 INT L916;
 INT L9181_;
 extern STR name1;
 BOOL L919;
 BOOL L9201_;
 extern STR Oofbui2034740200;
 dSTAT L921;
 i = 0;
 while (1) {
  L10 = STR_sizerINT(r);
  L121_=(i)<(L10); 
  if (L121_) {
  }
  else {
   goto after_loop;
  }
  L131_=ARR((STR)r,i); 
  L11 = L131_;
  switch (L11) {
   case '\\': 
    L141_=INTPLUS(i,1); 
    i = L141_;
    plus_self = res;
    L151_=ARR((STR)r,i); 
    plus_arg = L151_;
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L17 = 1;
    L19=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L17)*sizeof(CHAR);
    L18=ZALLOC_LEAF_BIG(L19);
    if (L18==NULL) FATAL("Unable to allocate more memory");
    memset(L18,0,L19);
    ((OB)L18)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L18)->header.destroyed=0;
#endif

    L16 = ((STR) L18);
    L16->asize = L17;
    r1 = L16;
    SARR((STR)r1,0,create_c); 
    ;
    ret_val3 = r1;
    ret_val2 = ret_val3;
    s = ret_val2;
    ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
    res = ret_val1;
    break;
   case '@': 
    L221_=INTPLUS(i,1); 
    i = L221_;
    cast = FALSE;
    L231_=ARR((STR)r,i); 
    L24 = L231_;
    L251_=L24=='('; 
    if (L251_) {
     cast = TRUE;
     L261_=INTPLUS(i,1); 
     i = L261_;
    }
    L271_=ARR((STR)r,i); 
    L21 = L271_;
    switch (L21) {
     case '0': 
      tp = ATTR(fun11,tp);
      break;
     case '1': 
      L281_=ARR((STR)r,i); 
      L29 = L281_;
      L301_=(INT)((unsigned char)L29); 
      L32 = L301_;
      L331_=(INT)((unsigned char)'1'); 
      L34 = L331_;
      L351_=INTMINUS(L32,L34); 
      l = L351_;
      L36 = ATTR(fun11,args);
      L371_=(ARG)ARR((ARRAYARG)L36,l); 
      L38=L371_;
      tp = ATTR(L38,tp);
      break;
     case '2': 
      L391_=ARR((STR)r,i); 
      L40 = L391_;
      L421_=(INT)((unsigned char)L40); 
      L43 = L421_;
      L441_=(INT)((unsigned char)'1'); 
      L45 = L441_;
      L461_=INTMINUS(L43,L45); 
      l1 = L461_;
      L47 = ATTR(fun11,args);
      L481_=(ARG)ARR((ARRAYARG)L47,l1); 
      L49=L481_;
      tp = ATTR(L49,tp);
      break;
     case '3': 
      L501_=ARR((STR)r,i); 
      L52 = L501_;
      L531_=(INT)((unsigned char)L52); 
      L54 = L531_;
      L551_=(INT)((unsigned char)'1'); 
      L56 = L551_;
      L571_=INTMINUS(L54,L56); 
      l2 = L571_;
      L58 = ATTR(fun11,args);
      L591_=(ARG)ARR((ARRAYARG)L58,l2); 
      L60=L591_;
      tp = ATTR(L60,tp);
      break;
     case '4': 
      L621_=ARR((STR)r,i); 
      L63 = L621_;
      L641_=(INT)((unsigned char)L63); 
      L65 = L641_;
      L661_=(INT)((unsigned char)'1'); 
      L67 = L661_;
      L681_=INTMINUS(L65,L67); 
      l3 = L681_;
      L69 = ATTR(fun11,args);
      L701_=(ARG)ARR((ARRAYARG)L69,l3); 
      L72=L701_;
      tp = ATTR(L72,tp);
      break;
     case '5': 
      L731_=ARR((STR)r,i); 
      L74 = L731_;
      L751_=(INT)((unsigned char)L74); 
      L76 = L751_;
      L771_=(INT)((unsigned char)'1'); 
      L78 = L771_;
      L791_=INTMINUS(L76,L78); 
      l4 = L791_;
      L80 = ATTR(fun11,args);
      L821_=(ARG)ARR((ARRAYARG)L80,l4); 
      L83=L821_;
      tp = ATTR(L83,tp);
      break;
     case '6': 
      L841_=ARR((STR)r,i); 
      L85 = L841_;
      L861_=(INT)((unsigned char)L85); 
      L87 = L861_;
      L881_=(INT)((unsigned char)'1'); 
      L89 = L881_;
      L901_=INTMINUS(L87,L89); 
      l5 = L901_;
      L92 = ATTR(fun11,args);
      L931_=(ARG)ARR((ARRAYARG)L92,l5); 
      L94=L931_;
      tp = ATTR(L94,tp);
      break;
     case '7': 
      L951_=ARR((STR)r,i); 
      L96 = L951_;
      L971_=(INT)((unsigned char)L96); 
      L98 = L971_;
      L991_=(INT)((unsigned char)'1'); 
      L100 = L991_;
      L1011_=INTMINUS(L98,L100); 
      l6 = L1011_;
      L102 = ATTR(fun11,args);
      L1031_=(ARG)ARR((ARRAYARG)L102,l6); 
      L104=L1031_;
      tp = ATTR(L104,tp);
      break;
     case '8': 
      L1051_=ARR((STR)r,i); 
      L106 = L1051_;
      L1071_=(INT)((unsigned char)L106); 
      L108 = L1071_;
      L1091_=(INT)((unsigned char)'1'); 
      L110 = L1091_;
      L1111_=INTMINUS(L108,L110); 
      l7 = L1111_;
      L112 = ATTR(fun11,args);
      L1131_=(ARG)ARR((ARRAYARG)L112,l7); 
      L114=L1131_;
      tp = ATTR(L114,tp);
      break;
     case '9': 
      L1151_=ARR((STR)r,i); 
      L116 = L1151_;
      L1171_=(INT)((unsigned char)L116); 
      L118 = L1171_;
      L1191_=(INT)((unsigned char)'1'); 
      L120 = L1191_;
      L1221_=INTMINUS(L118,L120); 
      l8 = L1221_;
      L123 = ATTR(fun11,args);
      L1241_=(ARG)ARR((ARRAYARG)L123,l8); 
      L125=L1241_;
      tp = ATTR(L125,tp);
      break;
     case 'r': 
      if ((ATTR(fun11,ret)==((dTP) NULL))) {
       barf_self = self;
       plus_self1 = ((STR) &risund1670463801);
       plus_sarg = r;
       ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
       plus_self2 = ret_val4;
       plus_sarg1 = ((STR) &inCONFIGfor);
       ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
       plus_self3 = ret_val5;
       plus_sarg2 = ATTR(fun11,str);
       ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
       barf_msg = ret_val6;
       barf_self1 = ATTR(barf_self,prog);
       barf_msg1 = barf_msg;
       barf_at_self = barf_self1;
       barf_at_msg = barf_msg1;
       barf_at_at = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self, barf_at_at);
       plus_self4 = ((STR) &Intern28965746);
       plus_sarg3 = barf_at_msg;
       ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
       PROG_err_STR(barf_at_self, ret_val7);
       exit_self = ((UNIX) NULL);
       exit_code = 1;
       exit(exit_code);
      }
      tp = ATTR(fun11,ret);
      break;
     case 'P': 
      L1261_=INTPLUS(i,1); 
      i = L1261_;
      tparams = ATTR(fun11,src_tparams);
      L1281_=ARR((STR)r,i); 
      is_lt_self = L1281_;
      is_lt_c = '0';
      L1291_=(INT)((unsigned char)is_lt_self); 
      L130 = L1291_;
      L1321_=(INT)((unsigned char)is_lt_c); 
      L133 = L1321_;
      L1341_=(L130)<(L133); 
      ret_val8 = L1341_;
      if (ret_val8) {
       L127 = TRUE;
      } else {
       is_lt_self1 = '9';
       L1351_=ARR((STR)r,i); 
       is_lt_c1 = L1351_;
       L1361_=(INT)((unsigned char)is_lt_self1); 
       L137 = L1361_;
       L1381_=(INT)((unsigned char)is_lt_c1); 
       L139 = L1381_;
       L1401_=(L137)<(L139); 
       ret_val9 = L1401_;
       L127 = ret_val9;
      }
      if (L127) {
       barf_self2 = self;
       plus_self5 = ((STR) &Pmustb23117515);
       L1421_=ARR((STR)r,i); 
       plus_arg1 = L1421_;
       str_self1 = plus_arg1;
       create_self1 = ((STR) NULL);
       create_c1 = str_self1;
       L144 = 1;
       L146=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L144)*sizeof(CHAR);
       L145=ZALLOC_LEAF_BIG(L146);
       if (L145==NULL) FATAL("Unable to allocate more memory");
       memset(L145,0,L146);
       ((OB)L145)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L145)->header.destroyed=0;
#endif

       L143 = ((STR) L145);
       L143->asize = L144;
       r2 = L143;
       SARR((STR)r2,0,create_c1); 
       ;
       ret_val12 = r2;
       ret_val11 = ret_val12;
       s1 = ret_val11;
       ret_val10 = STR_ap1077157958(plus_self5, s1, TRUE);
       plus_self6 = ret_val10;
       plus_sarg4 = ((STR) &name9);
       ret_val13 = STR_ap2004550586(plus_self6, plus_sarg4);
       barf_msg2 = ret_val13;
       barf_self3 = ATTR(barf_self2,prog);
       barf_msg3 = barf_msg2;
       barf_at_self1 = barf_self3;
       barf_at_msg1 = barf_msg3;
       barf_at_at1 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self1, barf_at_at1);
       plus_self7 = ((STR) &Intern28965746);
       plus_sarg5 = barf_at_msg1;
       ret_val14 = STR_ap2004550586(plus_self7, plus_sarg5);
       PROG_err_STR(barf_at_self1, ret_val14);
       exit_self1 = ((UNIX) NULL);
       exit_code1 = 1;
       exit(exit_code1);
      }
      L1481_=ARR((STR)r,i); 
      L149 = L1481_;
      L1501_=(INT)((unsigned char)L149); 
      L152 = L1501_;
      L1531_=(INT)((unsigned char)'0'); 
      L154 = L1531_;
      L1551_=INTMINUS(L152,L154); 
      L156 = L1551_;
      L1571_=(tparams)==NULL?0:ASIZE((ARRAYdTP)tparams); 
      L158 = L1571_;
      L1591_=(L156)<(L158); 
      L160 = L1591_;
      L1611_=!(L160); 
      if (L1611_) {
       barf_self4 = self;
       plus_self8 = ((STR) &P);
       L1621_=ARR((STR)r,i); 
       plus_arg2 = L1621_;
       str_self2 = plus_arg2;
       create_self2 = ((STR) NULL);
       create_c2 = str_self2;
       L164 = 1;
       L166=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L164)*sizeof(CHAR);
       L165=ZALLOC_LEAF_BIG(L166);
       if (L165==NULL) FATAL("Unable to allocate more memory");
       memset(L165,0,L166);
       ((OB)L165)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L165)->header.destroyed=0;
#endif

       L163 = ((STR) L165);
       L163->asize = L164;
       r3 = L163;
       SARR((STR)r3,0,create_c2); 
       ;
       ret_val17 = r3;
       ret_val16 = ret_val17;
       s2 = ret_val16;
       ret_val15 = STR_ap1077157958(plus_self8, s2, TRUE);
       plus_self9 = ret_val15;
       plus_sarg6 = ((STR) &outofboundsmacro);
       ret_val18 = STR_ap2004550586(plus_self9, plus_sarg6);
       plus_self10 = ret_val18;
       plus_sarg7 = r;
       ret_val19 = STR_ap2004550586(plus_self10, plus_sarg7);
       plus_self11 = ret_val19;
       plus_sarg8 = ((STR) &sig2);
       ret_val20 = STR_ap2004550586(plus_self11, plus_sarg8);
       plus_self12 = ret_val20;
       plus_sarg9 = ATTR(fun11,str);
       ret_val21 = STR_ap2004550586(plus_self12, plus_sarg9);
       plus_self13 = ret_val21;
       plus_sarg10 = ((STR) &name50);
       ret_val22 = STR_ap2004550586(plus_self13, plus_sarg10);
       barf_msg4 = ret_val22;
       barf_self5 = ATTR(barf_self4,prog);
       barf_msg5 = barf_msg4;
       barf_at_self2 = barf_self5;
       barf_at_msg2 = barf_msg5;
       barf_at_at2 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self2, barf_at_at2);
       plus_self14 = ((STR) &Intern28965746);
       plus_sarg11 = barf_at_msg2;
       ret_val23 = STR_ap2004550586(plus_self14, plus_sarg11);
       PROG_err_STR(barf_at_self2, ret_val23);
       exit_self2 = ((UNIX) NULL);
       exit_code2 = 1;
       exit(exit_code2);
      }
      L1681_=ARR((STR)r,i); 
      L169 = L1681_;
      L1701_=(INT)((unsigned char)L169); 
      L171 = L1701_;
      L1721_=(INT)((unsigned char)'0'); 
      L173 = L1721_;
      L1741_=INTMINUS(L171,L173); 
      L175 = L1741_;
      L1761_=(dTP)ARR((ARRAYdTP)tparams,L175); 
      tp = L1761_;
      break;
     case '@': 
      plus_self15 = res;
      plus_sarg12 = ((STR) &name10);
      ret_val24 = STR_ap2004550586(plus_self15, plus_sarg12);
      res = ret_val24;
      ru = ((STR) &name1);
      if (ATTR(self,manual_loop_unlo)) {
       ru = ((STR) &LM_LOC1545209594);
      }
      L177 = ATTR(self,ex_nesting);
      L178 = ATTR(self,current_loop_ex_);
      L1791_=INTMINUS(L177,L178); 
      L31 = L1791_;
      switch (L31) {
       case 0: 
        res = STR_ap1417971546(res, ru, ((STR) &goto1), ATTR(self,current_loop));
        break;
       case 1: 
        res = STR_ap1925291910(res, ((STR) &do3), ru, ((STR) &POP_EX889458020), ATTR(self,current_loop), ((STR) &while0));
        break;
       default: ;
        plus_self16 = ((STR) &do3);
        plus_sarg13 = ru;
        ret_val25 = STR_ap2004550586(plus_self16, plus_sarg13);
        L180 = ATTR(self,ex_nesting);
        L181 = ATTR(self,current_loop_ex_);
        L1821_=INTMINUS(L180,L181); 
        str_self3 = L1821_;
        clear_self = buf1;
        L183 = (clear_self==((FSTR) NULL));
        L1841_=!(L183); 
        if (L1841_) {
         SATTR(clear_self,loc,0);
        }
        str_in_self = str_self3;
        str_in_s = buf1;
        ret_val27 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
        buf1 = ret_val27;
        str_self4 = buf1;
        ret_val28 = STR_fr1097270334(((STR) NULL), str_self4);
        ret_val26 = ret_val28;
        res = STR_ap1182453786(res, ret_val25, ((STR) &POP_EXCEPTION11), ret_val26, ((STR) &goto2), ATTR(self,current_loop), ((STR) &while0));
      }
      break;
     case '.': 
      arg_name = ((STR) &name1);
      L1851_=INTPLUS(i,1); 
      i = L1851_;
      while (1) {
       L1871_=ARR((STR)r,i); 
       if (CHAR_i1942379470(L1871_)) {
        L186 = TRUE;
       } else {
        L1881_=ARR((STR)r,i); 
        L189 = L1881_;
        L1901_=L189=='_'; 
        L186 = L1901_;
       }
       if (L186) {
       }
       else {
        goto after_loop1;
       }
       plus_self17 = arg_name;
       L1911_=ARR((STR)r,i); 
       plus_arg3 = L1911_;
       str_self5 = plus_arg3;
       create_self3 = ((STR) NULL);
       create_c3 = str_self5;
       L193 = 1;
       L195=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L193)*sizeof(CHAR);
       L194=ZALLOC_LEAF_BIG(L195);
       if (L194==NULL) FATAL("Unable to allocate more memory");
       memset(L194,0,L195);
       ((OB)L194)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L194)->header.destroyed=0;
#endif

       L192 = ((STR) L194);
       L192->asize = L193;
       r4 = L192;
       SARR((STR)r4,0,create_c3); 
       ;
       ret_val31 = r4;
       ret_val30 = ret_val31;
       s3 = ret_val30;
       ret_val29 = STR_ap1077157958(plus_self17, s3, TRUE);
       arg_name = ret_val29;
       L1971_=INTPLUS(i,1); 
       i = L1971_;
      }
      after_loop1: ;
      L1981_=INTMINUS(i,1); 
      i = L1981_;
      tp = ((dTP) NULL);
      argi = IDENT_1150413730(IDENT_zero, arg_name);
      {
       struct ELT_TBL_eltbrELT_frame_struct other3_0;
ELT_TBL_eltbrELT_frame noname1 = &other3_0;
       noname1->state = 0;
       while (1) {
        if (noname1->state == 0) {
         L200 = ATTR(fun11,tp);
         L202=(*dTP_implrIMPL[TAG(L200)])(L200);
         L41 = ATTR(L202,elts);
         noname1->self = L41;
        }
        L199 = ELT_TBL_eltbrELT(noname1);
        if (noname1->state == -1) {
         goto after_loop2;
        }
        elt1 = L199;
        if (ELT_is380037919(elt1)) {
         L205 = TRUE;
        } else {
         L205 = ELT_is1936066670(elt1);
        }
        if (L205) {
         L204 = TRUE;
        } else {
         L204 = ELT_is1269015106(elt1);
        }
        if (L204) {
         name_self = elt1;
         ret_val32 = ATTR(ATTR(name_self,sig1),name1);
         is_eq_self = ret_val32;
         is_eq_i = argi;
         ret_val33 = (is_eq_self.str==is_eq_i.str);
         L203 = ret_val33;
        } else {
         L203 = FALSE;
        }
        if (L203) {
         ret_self = elt1;
         ret_val34 = ATTR(ATTR(ret_self,sig1),ret);
         tp = ret_val34;
         goto after_loop2;
        }
       }
      }
      after_loop2: ;
      if ((tp==((dTP) NULL))) {
       barf_self6 = self;
       plus_self18 = ((STR) &unknwo513321933);
       plus_sarg14 = arg_name;
       ret_val35 = STR_ap2004550586(plus_self18, plus_sarg14);
       plus_self19 = ret_val35;
       plus_sarg15 = ((STR) &inCONFIGmacrofor);
       ret_val36 = STR_ap2004550586(plus_self19, plus_sarg15);
       plus_self20 = ret_val36;
       plus_sarg16 = ATTR(fun11,str);
       ret_val37 = STR_ap2004550586(plus_self20, plus_sarg16);
       plus_self21 = ret_val37;
       plus_sarg17 = ((STR) &name52);
       ret_val38 = STR_ap2004550586(plus_self21, plus_sarg17);
       plus_self22 = ret_val38;
       plus_sarg18 = r;
       ret_val39 = STR_ap2004550586(plus_self22, plus_sarg18);
       plus_self23 = ret_val39;
       plus_sarg19 = ((STR) &name26);
       ret_val40 = STR_ap2004550586(plus_self23, plus_sarg19);
       barf_msg6 = ret_val40;
       barf_self7 = ATTR(barf_self6,prog);
       barf_msg7 = barf_msg6;
       barf_at_self3 = barf_self7;
       barf_at_msg3 = barf_msg7;
       barf_at_at3 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self3, barf_at_at3);
       plus_self24 = ((STR) &Intern28965746);
       plus_sarg20 = barf_at_msg3;
       ret_val41 = STR_ap2004550586(plus_self24, plus_sarg20);
       PROG_err_STR(barf_at_self3, ret_val41);
       exit_self3 = ((UNIX) NULL);
       exit_code3 = 1;
       exit(exit_code3);
      }
      break;
     default: ;
      barf_self8 = self;
      plus_self25 = ((STR) &unknown1);
      L2061_=ARR((STR)r,i); 
      plus_arg4 = L2061_;
      str_self6 = plus_arg4;
      create_self4 = ((STR) NULL);
      create_c4 = str_self6;
      L208 = 1;
      L210=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L208)*sizeof(CHAR);
      L209=ZALLOC_LEAF_BIG(L210);
      if (L209==NULL) FATAL("Unable to allocate more memory");
      memset(L209,0,L210);
      ((OB)L209)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L209)->header.destroyed=0;
#endif

      L207 = ((STR) L209);
      L207->asize = L208;
      r5 = L207;
      SARR((STR)r5,0,create_c4); 
      ;
      ret_val44 = r5;
      ret_val43 = ret_val44;
      s4 = ret_val43;
      ret_val42 = STR_ap1077157958(plus_self25, s4, TRUE);
      plus_self26 = ret_val42;
      plus_sarg21 = ((STR) &macroinCONFIGfor);
      ret_val45 = STR_ap2004550586(plus_self26, plus_sarg21);
      plus_self27 = ret_val45;
      plus_sarg22 = ATTR(fun11,str);
      ret_val46 = STR_ap2004550586(plus_self27, plus_sarg22);
      plus_self28 = ret_val46;
      plus_sarg23 = ((STR) &name52);
      ret_val47 = STR_ap2004550586(plus_self28, plus_sarg23);
      plus_self29 = ret_val47;
      plus_sarg24 = r;
      ret_val48 = STR_ap2004550586(plus_self29, plus_sarg24);
      plus_self30 = ret_val48;
      plus_sarg25 = ((STR) &name26);
      ret_val49 = STR_ap2004550586(plus_self30, plus_sarg25);
      barf_msg8 = ret_val49;
      barf_self9 = ATTR(barf_self8,prog);
      barf_msg9 = barf_msg8;
      barf_at_self4 = barf_self9;
      barf_at_msg4 = barf_msg9;
      barf_at_at4 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self4, barf_at_at4);
      plus_self31 = ((STR) &Intern28965746);
      plus_sarg26 = barf_at_msg4;
      ret_val50 = STR_ap2004550586(plus_self31, plus_sarg26);
      PROG_err_STR(barf_at_self4, ret_val50);
      exit_self4 = ((UNIX) NULL);
      exit_code4 = 1;
      exit(exit_code4);
    }
    L2121_=ARR((STR)r,i); 
    L213 = L2121_;
    L2141_=L213=='@'; 
    L215 = L2141_;
    L2161_=!(L215); 
    if (L2161_) {
     if (cast) {
      L217 = tp;
      L218 = (*dTP_is1334578382[TAG(L217)])(L217);
      L2191_=!(L218); 
      if (L2191_) {
       plus_self32 = res;
       plus_sarg27 = ((STR) &name24);
       ret_val51 = STR_ap2004550586(plus_self32, plus_sarg27);
       plus_self33 = ret_val51;
       mang_self = self;
       mang_ob = ((OB) tp);
       ret_val52 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
       plus_sarg28 = ret_val52;
       ret_val53 = STR_ap2004550586(plus_self33, plus_sarg28);
       plus_self34 = ret_val53;
       plus_sarg29 = ((STR) &name26);
       ret_val54 = STR_ap2004550586(plus_self34, plus_sarg29);
       res = ret_val54;
      }
      L2201_=INTPLUS(i,1); 
      i = L2201_;
     }
     else {
      plus_self35 = res;
      mang_self1 = self;
      mang_ob1 = ((OB) tp);
      ret_val55 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
      plus_sarg30 = ret_val55;
      ret_val56 = STR_ap2004550586(plus_self35, plus_sarg30);
      res = ret_val56;
     }
    }
    break;
   case '^': 
    L2221_=INTPLUS(i,1); 
    i = L2221_;
    if ((call11==((dAM_CALL_EXPR) NULL))) {
     plus_self36 = res;
     plus_sarg31 = ((STR) &S01);
     ret_val57 = STR_ap2004550586(plus_self36, plus_sarg31);
     res = ret_val57;
    }
    else {
     L2231_=ARR((STR)r,i); 
     L51 = L2231_;
     switch (L51) {
      case '0': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self10 = self;
        plus_self37 = ((STR) &ncanno888628440);
        plus_sarg32 = r;
        ret_val58 = STR_ap2004550586(plus_self37, plus_sarg32);
        plus_self38 = ret_val58;
        plus_sarg33 = ((STR) &inCONFIGfor1);
        ret_val59 = STR_ap2004550586(plus_self38, plus_sarg33);
        plus_self39 = ret_val59;
        plus_sarg34 = ATTR(fun11,str);
        ret_val60 = STR_ap2004550586(plus_self39, plus_sarg34);
        barf_msg10 = ret_val60;
        barf_self11 = ATTR(barf_self10,prog);
        barf_msg11 = barf_msg10;
        barf_at_self5 = barf_self11;
        barf_at_msg5 = barf_msg11;
        barf_at_at5 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self5, barf_at_at5);
        plus_self40 = ((STR) &Intern28965746);
        plus_sarg35 = barf_at_msg5;
        ret_val61 = STR_ap2004550586(plus_self40, plus_sarg35);
        PROG_err_STR(barf_at_self5, ret_val61);
        exit_self5 = ((UNIX) NULL);
        exit_code5 = 1;
        exit(exit_code5);
       }
       L2241_=ARR((STR)r,i); 
       L225 = L2241_;
       L2261_=(INT)((unsigned char)L225); 
       L227 = L2261_;
       L2281_=(INT)((unsigned char)'0'); 
       L229 = L2281_;
       L2301_=INTMINUS(L227,L229); 
       l9 = L2301_;
       has_near = TRUE;
       L232 = call11;
       L233=(*dAM_CA967858872[TAG(L232)])(L232, l9);
       L234 = ATTR(L233,expr);
       if ((*dAM_EX1117610980[TAG(L234)])(L234)) {
        plus_self41 = res;
        plus_sarg36 = ((STR) &S11);
        ret_val62 = STR_ap2004550586(plus_self41, plus_sarg36);
        plus_self42 = ret_val62;
        plus_sarg37 = ((STR) &name93);
        ret_val63 = STR_ap2004550586(plus_self42, plus_sarg37);
        plus_self43 = ret_val63;
        L2351_=(STR)ARR((ARRAYSTR)args,l9); 
        plus_sarg38 = L2351_;
        ret_val64 = STR_ap2004550586(plus_self43, plus_sarg38);
        plus_self44 = ret_val64;
        plus_sarg39 = ((STR) &name94);
        ret_val65 = STR_ap2004550586(plus_self44, plus_sarg39);
        res = ret_val65;
       }
       else {
        plus_self45 = res;
        plus_sarg40 = ((STR) &S01);
        ret_val66 = STR_ap2004550586(plus_self45, plus_sarg40);
        res = ret_val66;
        near_failed = TRUE;
       }
       break;
      case '1': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self12 = self;
        plus_self46 = ((STR) &ncanno888628440);
        plus_sarg41 = r;
        ret_val67 = STR_ap2004550586(plus_self46, plus_sarg41);
        plus_self47 = ret_val67;
        plus_sarg42 = ((STR) &inCONFIGfor1);
        ret_val68 = STR_ap2004550586(plus_self47, plus_sarg42);
        plus_self48 = ret_val68;
        plus_sarg43 = ATTR(fun11,str);
        ret_val69 = STR_ap2004550586(plus_self48, plus_sarg43);
        barf_msg12 = ret_val69;
        barf_self13 = ATTR(barf_self12,prog);
        barf_msg13 = barf_msg12;
        barf_at_self6 = barf_self13;
        barf_at_msg6 = barf_msg13;
        barf_at_at6 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self6, barf_at_at6);
        plus_self49 = ((STR) &Intern28965746);
        plus_sarg44 = barf_at_msg6;
        ret_val70 = STR_ap2004550586(plus_self49, plus_sarg44);
        PROG_err_STR(barf_at_self6, ret_val70);
        exit_self6 = ((UNIX) NULL);
        exit_code6 = 1;
        exit(exit_code6);
       }
       L2361_=ARR((STR)r,i); 
       L237 = L2361_;
       L2381_=(INT)((unsigned char)L237); 
       L239 = L2381_;
       L2401_=(INT)((unsigned char)'0'); 
       L241 = L2401_;
       L2421_=INTMINUS(L239,L241); 
       l10 = L2421_;
       has_near = TRUE;
       L243 = call11;
       L244=(*dAM_CA967858872[TAG(L243)])(L243, l10);
       L245 = ATTR(L244,expr);
       if ((*dAM_EX1117610980[TAG(L245)])(L245)) {
        plus_self50 = res;
        plus_sarg45 = ((STR) &S11);
        ret_val71 = STR_ap2004550586(plus_self50, plus_sarg45);
        plus_self51 = ret_val71;
        plus_sarg46 = ((STR) &name93);
        ret_val72 = STR_ap2004550586(plus_self51, plus_sarg46);
        plus_self52 = ret_val72;
        L2461_=(STR)ARR((ARRAYSTR)args,l10); 
        plus_sarg47 = L2461_;
        ret_val73 = STR_ap2004550586(plus_self52, plus_sarg47);
        plus_self53 = ret_val73;
        plus_sarg48 = ((STR) &name94);
        ret_val74 = STR_ap2004550586(plus_self53, plus_sarg48);
        res = ret_val74;
       }
       else {
        plus_self54 = res;
        plus_sarg49 = ((STR) &S01);
        ret_val75 = STR_ap2004550586(plus_self54, plus_sarg49);
        res = ret_val75;
        near_failed = TRUE;
       }
       break;
      case '2': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self14 = self;
        plus_self55 = ((STR) &ncanno888628440);
        plus_sarg50 = r;
        ret_val76 = STR_ap2004550586(plus_self55, plus_sarg50);
        plus_self56 = ret_val76;
        plus_sarg51 = ((STR) &inCONFIGfor1);
        ret_val77 = STR_ap2004550586(plus_self56, plus_sarg51);
        plus_self57 = ret_val77;
        plus_sarg52 = ATTR(fun11,str);
        ret_val78 = STR_ap2004550586(plus_self57, plus_sarg52);
        barf_msg14 = ret_val78;
        barf_self15 = ATTR(barf_self14,prog);
        barf_msg15 = barf_msg14;
        barf_at_self7 = barf_self15;
        barf_at_msg7 = barf_msg15;
        barf_at_at7 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self7, barf_at_at7);
        plus_self58 = ((STR) &Intern28965746);
        plus_sarg53 = barf_at_msg7;
        ret_val79 = STR_ap2004550586(plus_self58, plus_sarg53);
        PROG_err_STR(barf_at_self7, ret_val79);
        exit_self7 = ((UNIX) NULL);
        exit_code7 = 1;
        exit(exit_code7);
       }
       L2471_=ARR((STR)r,i); 
       L248 = L2471_;
       L2491_=(INT)((unsigned char)L248); 
       L250 = L2491_;
       L2521_=(INT)((unsigned char)'0'); 
       L253 = L2521_;
       L2541_=INTMINUS(L250,L253); 
       l11 = L2541_;
       has_near = TRUE;
       L255 = call11;
       L256=(*dAM_CA967858872[TAG(L255)])(L255, l11);
       L257 = ATTR(L256,expr);
       if ((*dAM_EX1117610980[TAG(L257)])(L257)) {
        plus_self59 = res;
        plus_sarg54 = ((STR) &S11);
        ret_val80 = STR_ap2004550586(plus_self59, plus_sarg54);
        plus_self60 = ret_val80;
        plus_sarg55 = ((STR) &name93);
        ret_val81 = STR_ap2004550586(plus_self60, plus_sarg55);
        plus_self61 = ret_val81;
        L2581_=(STR)ARR((ARRAYSTR)args,l11); 
        plus_sarg56 = L2581_;
        ret_val82 = STR_ap2004550586(plus_self61, plus_sarg56);
        plus_self62 = ret_val82;
        plus_sarg57 = ((STR) &name94);
        ret_val83 = STR_ap2004550586(plus_self62, plus_sarg57);
        res = ret_val83;
       }
       else {
        plus_self63 = res;
        plus_sarg58 = ((STR) &S01);
        ret_val84 = STR_ap2004550586(plus_self63, plus_sarg58);
        res = ret_val84;
        near_failed = TRUE;
       }
       break;
      case '3': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self16 = self;
        plus_self64 = ((STR) &ncanno888628440);
        plus_sarg59 = r;
        ret_val85 = STR_ap2004550586(plus_self64, plus_sarg59);
        plus_self65 = ret_val85;
        plus_sarg60 = ((STR) &inCONFIGfor1);
        ret_val86 = STR_ap2004550586(plus_self65, plus_sarg60);
        plus_self66 = ret_val86;
        plus_sarg61 = ATTR(fun11,str);
        ret_val87 = STR_ap2004550586(plus_self66, plus_sarg61);
        barf_msg16 = ret_val87;
        barf_self17 = ATTR(barf_self16,prog);
        barf_msg17 = barf_msg16;
        barf_at_self8 = barf_self17;
        barf_at_msg8 = barf_msg17;
        barf_at_at8 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self8, barf_at_at8);
        plus_self67 = ((STR) &Intern28965746);
        plus_sarg62 = barf_at_msg8;
        ret_val88 = STR_ap2004550586(plus_self67, plus_sarg62);
        PROG_err_STR(barf_at_self8, ret_val88);
        exit_self8 = ((UNIX) NULL);
        exit_code8 = 1;
        exit(exit_code8);
       }
       L2591_=ARR((STR)r,i); 
       L260 = L2591_;
       L2621_=(INT)((unsigned char)L260); 
       L263 = L2621_;
       L2641_=(INT)((unsigned char)'0'); 
       L265 = L2641_;
       L2661_=INTMINUS(L263,L265); 
       l12 = L2661_;
       has_near = TRUE;
       L267 = call11;
       L268=(*dAM_CA967858872[TAG(L267)])(L267, l12);
       L269 = ATTR(L268,expr);
       if ((*dAM_EX1117610980[TAG(L269)])(L269)) {
        plus_self68 = res;
        plus_sarg63 = ((STR) &S11);
        ret_val89 = STR_ap2004550586(plus_self68, plus_sarg63);
        plus_self69 = ret_val89;
        plus_sarg64 = ((STR) &name93);
        ret_val90 = STR_ap2004550586(plus_self69, plus_sarg64);
        plus_self70 = ret_val90;
        L2701_=(STR)ARR((ARRAYSTR)args,l12); 
        plus_sarg65 = L2701_;
        ret_val91 = STR_ap2004550586(plus_self70, plus_sarg65);
        plus_self71 = ret_val91;
        plus_sarg66 = ((STR) &name94);
        ret_val92 = STR_ap2004550586(plus_self71, plus_sarg66);
        res = ret_val92;
       }
       else {
        plus_self72 = res;
        plus_sarg67 = ((STR) &S01);
        ret_val93 = STR_ap2004550586(plus_self72, plus_sarg67);
        res = ret_val93;
        near_failed = TRUE;
       }
       break;
      case '4': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self18 = self;
        plus_self73 = ((STR) &ncanno888628440);
        plus_sarg68 = r;
        ret_val94 = STR_ap2004550586(plus_self73, plus_sarg68);
        plus_self74 = ret_val94;
        plus_sarg69 = ((STR) &inCONFIGfor1);
        ret_val95 = STR_ap2004550586(plus_self74, plus_sarg69);
        plus_self75 = ret_val95;
        plus_sarg70 = ATTR(fun11,str);
        ret_val96 = STR_ap2004550586(plus_self75, plus_sarg70);
        barf_msg18 = ret_val96;
        barf_self19 = ATTR(barf_self18,prog);
        barf_msg19 = barf_msg18;
        barf_at_self9 = barf_self19;
        barf_at_msg9 = barf_msg19;
        barf_at_at9 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self9, barf_at_at9);
        plus_self76 = ((STR) &Intern28965746);
        plus_sarg71 = barf_at_msg9;
        ret_val97 = STR_ap2004550586(plus_self76, plus_sarg71);
        PROG_err_STR(barf_at_self9, ret_val97);
        exit_self9 = ((UNIX) NULL);
        exit_code9 = 1;
        exit(exit_code9);
       }
       L2721_=ARR((STR)r,i); 
       L273 = L2721_;
       L2741_=(INT)((unsigned char)L273); 
       L275 = L2741_;
       L2761_=(INT)((unsigned char)'0'); 
       L277 = L2761_;
       L2781_=INTMINUS(L275,L277); 
       l13 = L2781_;
       has_near = TRUE;
       L279 = call11;
       L280=(*dAM_CA967858872[TAG(L279)])(L279, l13);
       L282 = ATTR(L280,expr);
       if ((*dAM_EX1117610980[TAG(L282)])(L282)) {
        plus_self77 = res;
        plus_sarg72 = ((STR) &S11);
        ret_val98 = STR_ap2004550586(plus_self77, plus_sarg72);
        plus_self78 = ret_val98;
        plus_sarg73 = ((STR) &name93);
        ret_val99 = STR_ap2004550586(plus_self78, plus_sarg73);
        plus_self79 = ret_val99;
        L2831_=(STR)ARR((ARRAYSTR)args,l13); 
        plus_sarg74 = L2831_;
        ret_val100 = STR_ap2004550586(plus_self79, plus_sarg74);
        plus_self80 = ret_val100;
        plus_sarg75 = ((STR) &name94);
        ret_val101 = STR_ap2004550586(plus_self80, plus_sarg75);
        res = ret_val101;
       }
       else {
        plus_self81 = res;
        plus_sarg76 = ((STR) &S01);
        ret_val102 = STR_ap2004550586(plus_self81, plus_sarg76);
        res = ret_val102;
        near_failed = TRUE;
       }
       break;
      case '5': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self20 = self;
        plus_self82 = ((STR) &ncanno888628440);
        plus_sarg77 = r;
        ret_val103 = STR_ap2004550586(plus_self82, plus_sarg77);
        plus_self83 = ret_val103;
        plus_sarg78 = ((STR) &inCONFIGfor1);
        ret_val104 = STR_ap2004550586(plus_self83, plus_sarg78);
        plus_self84 = ret_val104;
        plus_sarg79 = ATTR(fun11,str);
        ret_val105 = STR_ap2004550586(plus_self84, plus_sarg79);
        barf_msg20 = ret_val105;
        barf_self21 = ATTR(barf_self20,prog);
        barf_msg21 = barf_msg20;
        barf_at_self10 = barf_self21;
        barf_at_msg10 = barf_msg21;
        barf_at_at10 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self10, barf_at_at10);
        plus_self85 = ((STR) &Intern28965746);
        plus_sarg80 = barf_at_msg10;
        ret_val106 = STR_ap2004550586(plus_self85, plus_sarg80);
        PROG_err_STR(barf_at_self10, ret_val106);
        exit_self10 = ((UNIX) NULL);
        exit_code10 = 1;
        exit(exit_code10);
       }
       L2841_=ARR((STR)r,i); 
       L285 = L2841_;
       L2861_=(INT)((unsigned char)L285); 
       L287 = L2861_;
       L2881_=(INT)((unsigned char)'0'); 
       L289 = L2881_;
       L2901_=INTMINUS(L287,L289); 
       l14 = L2901_;
       has_near = TRUE;
       L291 = call11;
       L292=(*dAM_CA967858872[TAG(L291)])(L291, l14);
       L293 = ATTR(L292,expr);
       if ((*dAM_EX1117610980[TAG(L293)])(L293)) {
        plus_self86 = res;
        plus_sarg81 = ((STR) &S11);
        ret_val107 = STR_ap2004550586(plus_self86, plus_sarg81);
        plus_self87 = ret_val107;
        plus_sarg82 = ((STR) &name93);
        ret_val108 = STR_ap2004550586(plus_self87, plus_sarg82);
        plus_self88 = ret_val108;
        L2941_=(STR)ARR((ARRAYSTR)args,l14); 
        plus_sarg83 = L2941_;
        ret_val109 = STR_ap2004550586(plus_self88, plus_sarg83);
        plus_self89 = ret_val109;
        plus_sarg84 = ((STR) &name94);
        ret_val110 = STR_ap2004550586(plus_self89, plus_sarg84);
        res = ret_val110;
       }
       else {
        plus_self90 = res;
        plus_sarg85 = ((STR) &S01);
        ret_val1111 = STR_ap2004550586(plus_self90, plus_sarg85);
        res = ret_val1111;
        near_failed = TRUE;
       }
       break;
      case '6': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self22 = self;
        plus_self91 = ((STR) &ncanno888628440);
        plus_sarg86 = r;
        ret_val112 = STR_ap2004550586(plus_self91, plus_sarg86);
        plus_self92 = ret_val112;
        plus_sarg87 = ((STR) &inCONFIGfor1);
        ret_val113 = STR_ap2004550586(plus_self92, plus_sarg87);
        plus_self93 = ret_val113;
        plus_sarg88 = ATTR(fun11,str);
        ret_val114 = STR_ap2004550586(plus_self93, plus_sarg88);
        barf_msg22 = ret_val114;
        barf_self23 = ATTR(barf_self22,prog);
        barf_msg23 = barf_msg22;
        barf_at_self11 = barf_self23;
        barf_at_msg11 = barf_msg23;
        barf_at_at11 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self11, barf_at_at11);
        plus_self94 = ((STR) &Intern28965746);
        plus_sarg89 = barf_at_msg11;
        ret_val115 = STR_ap2004550586(plus_self94, plus_sarg89);
        PROG_err_STR(barf_at_self11, ret_val115);
        exit_self11 = ((UNIX) NULL);
        exit_code11 = 1;
        exit(exit_code11);
       }
       L2951_=ARR((STR)r,i); 
       L296 = L2951_;
       L2971_=(INT)((unsigned char)L296); 
       L298 = L2971_;
       L2991_=(INT)((unsigned char)'0'); 
       L300 = L2991_;
       L3021_=INTMINUS(L298,L300); 
       l15 = L3021_;
       has_near = TRUE;
       L303 = call11;
       L304=(*dAM_CA967858872[TAG(L303)])(L303, l15);
       L305 = ATTR(L304,expr);
       if ((*dAM_EX1117610980[TAG(L305)])(L305)) {
        plus_self95 = res;
        plus_sarg90 = ((STR) &S11);
        ret_val116 = STR_ap2004550586(plus_self95, plus_sarg90);
        plus_self96 = ret_val116;
        plus_sarg91 = ((STR) &name93);
        ret_val117 = STR_ap2004550586(plus_self96, plus_sarg91);
        plus_self97 = ret_val117;
        L3061_=(STR)ARR((ARRAYSTR)args,l15); 
        plus_sarg92 = L3061_;
        ret_val118 = STR_ap2004550586(plus_self97, plus_sarg92);
        plus_self98 = ret_val118;
        plus_sarg93 = ((STR) &name94);
        ret_val119 = STR_ap2004550586(plus_self98, plus_sarg93);
        res = ret_val119;
       }
       else {
        plus_self99 = res;
        plus_sarg94 = ((STR) &S01);
        ret_val120 = STR_ap2004550586(plus_self99, plus_sarg94);
        res = ret_val120;
        near_failed = TRUE;
       }
       break;
      case '7': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self24 = self;
        plus_self100 = ((STR) &ncanno888628440);
        plus_sarg95 = r;
        ret_val121 = STR_ap2004550586(plus_self100, plus_sarg95);
        plus_self101 = ret_val121;
        plus_sarg96 = ((STR) &inCONFIGfor1);
        ret_val122 = STR_ap2004550586(plus_self101, plus_sarg96);
        plus_self102 = ret_val122;
        plus_sarg97 = ATTR(fun11,str);
        ret_val123 = STR_ap2004550586(plus_self102, plus_sarg97);
        barf_msg24 = ret_val123;
        barf_self25 = ATTR(barf_self24,prog);
        barf_msg25 = barf_msg24;
        barf_at_self12 = barf_self25;
        barf_at_msg12 = barf_msg25;
        barf_at_at12 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self12, barf_at_at12);
        plus_self103 = ((STR) &Intern28965746);
        plus_sarg98 = barf_at_msg12;
        ret_val124 = STR_ap2004550586(plus_self103, plus_sarg98);
        PROG_err_STR(barf_at_self12, ret_val124);
        exit_self12 = ((UNIX) NULL);
        exit_code12 = 1;
        exit(exit_code12);
       }
       L3071_=ARR((STR)r,i); 
       L308 = L3071_;
       L3091_=(INT)((unsigned char)L308); 
       L310 = L3091_;
       L3111_=(INT)((unsigned char)'0'); 
       L312 = L3111_;
       L3131_=INTMINUS(L310,L312); 
       l16 = L3131_;
       has_near = TRUE;
       L314 = call11;
       L315=(*dAM_CA967858872[TAG(L314)])(L314, l16);
       L316 = ATTR(L315,expr);
       if ((*dAM_EX1117610980[TAG(L316)])(L316)) {
        plus_self104 = res;
        plus_sarg99 = ((STR) &S11);
        ret_val125 = STR_ap2004550586(plus_self104, plus_sarg99);
        plus_self105 = ret_val125;
        plus_sarg100 = ((STR) &name93);
        ret_val126 = STR_ap2004550586(plus_self105, plus_sarg100);
        plus_self106 = ret_val126;
        L3171_=(STR)ARR((ARRAYSTR)args,l16); 
        plus_sarg101 = L3171_;
        ret_val127 = STR_ap2004550586(plus_self106, plus_sarg101);
        plus_self107 = ret_val127;
        plus_sarg102 = ((STR) &name94);
        ret_val128 = STR_ap2004550586(plus_self107, plus_sarg102);
        res = ret_val128;
       }
       else {
        plus_self108 = res;
        plus_sarg103 = ((STR) &S01);
        ret_val129 = STR_ap2004550586(plus_self108, plus_sarg103);
        res = ret_val129;
        near_failed = TRUE;
       }
       break;
      case '8': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self26 = self;
        plus_self109 = ((STR) &ncanno888628440);
        plus_sarg104 = r;
        ret_val130 = STR_ap2004550586(plus_self109, plus_sarg104);
        plus_self110 = ret_val130;
        plus_sarg105 = ((STR) &inCONFIGfor1);
        ret_val131 = STR_ap2004550586(plus_self110, plus_sarg105);
        plus_self111 = ret_val131;
        plus_sarg106 = ATTR(fun11,str);
        ret_val132 = STR_ap2004550586(plus_self111, plus_sarg106);
        barf_msg26 = ret_val132;
        barf_self27 = ATTR(barf_self26,prog);
        barf_msg27 = barf_msg26;
        barf_at_self13 = barf_self27;
        barf_at_msg13 = barf_msg27;
        barf_at_at13 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self13, barf_at_at13);
        plus_self112 = ((STR) &Intern28965746);
        plus_sarg107 = barf_at_msg13;
        ret_val133 = STR_ap2004550586(plus_self112, plus_sarg107);
        PROG_err_STR(barf_at_self13, ret_val133);
        exit_self13 = ((UNIX) NULL);
        exit_code13 = 1;
        exit(exit_code13);
       }
       L3181_=ARR((STR)r,i); 
       L319 = L3181_;
       L3201_=(INT)((unsigned char)L319); 
       L321 = L3201_;
       L3221_=(INT)((unsigned char)'0'); 
       L323 = L3221_;
       L3241_=INTMINUS(L321,L323); 
       l17 = L3241_;
       has_near = TRUE;
       L325 = call11;
       L326=(*dAM_CA967858872[TAG(L325)])(L325, l17);
       L327 = ATTR(L326,expr);
       if ((*dAM_EX1117610980[TAG(L327)])(L327)) {
        plus_self113 = res;
        plus_sarg108 = ((STR) &S11);
        ret_val134 = STR_ap2004550586(plus_self113, plus_sarg108);
        plus_self114 = ret_val134;
        plus_sarg109 = ((STR) &name93);
        ret_val135 = STR_ap2004550586(plus_self114, plus_sarg109);
        plus_self115 = ret_val135;
        L3281_=(STR)ARR((ARRAYSTR)args,l17); 
        plus_sarg110 = L3281_;
        ret_val136 = STR_ap2004550586(plus_self115, plus_sarg110);
        plus_self116 = ret_val136;
        plus_sarg111 = ((STR) &name94);
        ret_val137 = STR_ap2004550586(plus_self116, plus_sarg111);
        res = ret_val137;
       }
       else {
        plus_self117 = res;
        plus_sarg112 = ((STR) &S01);
        ret_val138 = STR_ap2004550586(plus_self117, plus_sarg112);
        res = ret_val138;
        near_failed = TRUE;
       }
       break;
      case '9': 
       if ((args==((ARRAYSTR) NULL))) {
        barf_self28 = self;
        plus_self118 = ((STR) &ncanno888628440);
        plus_sarg113 = r;
        ret_val139 = STR_ap2004550586(plus_self118, plus_sarg113);
        plus_self119 = ret_val139;
        plus_sarg114 = ((STR) &inCONFIGfor1);
        ret_val140 = STR_ap2004550586(plus_self119, plus_sarg114);
        plus_self120 = ret_val140;
        plus_sarg115 = ATTR(fun11,str);
        ret_val141 = STR_ap2004550586(plus_self120, plus_sarg115);
        barf_msg28 = ret_val141;
        barf_self29 = ATTR(barf_self28,prog);
        barf_msg29 = barf_msg28;
        barf_at_self14 = barf_self29;
        barf_at_msg14 = barf_msg29;
        barf_at_at14 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self14, barf_at_at14);
        plus_self121 = ((STR) &Intern28965746);
        plus_sarg116 = barf_at_msg14;
        ret_val142 = STR_ap2004550586(plus_self121, plus_sarg116);
        PROG_err_STR(barf_at_self14, ret_val142);
        exit_self14 = ((UNIX) NULL);
        exit_code14 = 1;
        exit(exit_code14);
       }
       L3291_=ARR((STR)r,i); 
       L330 = L3291_;
       L3321_=(INT)((unsigned char)L330); 
       L333 = L3321_;
       L3341_=(INT)((unsigned char)'0'); 
       L335 = L3341_;
       L3361_=INTMINUS(L333,L335); 
       l18 = L3361_;
       has_near = TRUE;
       L337 = call11;
       L338=(*dAM_CA967858872[TAG(L337)])(L337, l18);
       L339 = ATTR(L338,expr);
       if ((*dAM_EX1117610980[TAG(L339)])(L339)) {
        plus_self122 = res;
        plus_sarg117 = ((STR) &S11);
        ret_val143 = STR_ap2004550586(plus_self122, plus_sarg117);
        plus_self123 = ret_val143;
        plus_sarg118 = ((STR) &name93);
        ret_val144 = STR_ap2004550586(plus_self123, plus_sarg118);
        plus_self124 = ret_val144;
        L3401_=(STR)ARR((ARRAYSTR)args,l18); 
        plus_sarg119 = L3401_;
        ret_val145 = STR_ap2004550586(plus_self124, plus_sarg119);
        plus_self125 = ret_val145;
        plus_sarg120 = ((STR) &name94);
        ret_val146 = STR_ap2004550586(plus_self125, plus_sarg120);
        res = ret_val146;
       }
       else {
        plus_self126 = res;
        plus_sarg121 = ((STR) &S01);
        ret_val147 = STR_ap2004550586(plus_self126, plus_sarg121);
        res = ret_val147;
        near_failed = TRUE;
       }
       break;
      default: ;
       FATAL("No applicable target in case statement\nin CGEN::expand_macro(STR,$AM_CALL_EXPR,SIG,ARRAY{STR},STR,STR,STR):STR\n./Back/cgen.sa:4753:23");
     }
    }
    break;
   case '$': 
    L3411_=INTPLUS(i,1); 
    i = L3411_;
    L3421_=ARR((STR)r,i); 
    L61 = L3421_;
    switch (L61) {
     case '0': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self30 = self;
       plus_self127 = ((STR) &dncann144395436);
       plus_sarg122 = r;
       ret_val148 = STR_ap2004550586(plus_self127, plus_sarg122);
       plus_self128 = ret_val148;
       plus_sarg123 = ((STR) &inCONFIGfor1);
       ret_val149 = STR_ap2004550586(plus_self128, plus_sarg123);
       plus_self129 = ret_val149;
       plus_sarg124 = ATTR(fun11,str);
       ret_val150 = STR_ap2004550586(plus_self129, plus_sarg124);
       barf_msg30 = ret_val150;
       barf_self31 = ATTR(barf_self30,prog);
       barf_msg31 = barf_msg30;
       barf_at_self15 = barf_self31;
       barf_at_msg15 = barf_msg31;
       barf_at_at15 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self15, barf_at_at15);
       plus_self130 = ((STR) &Intern28965746);
       plus_sarg125 = barf_at_msg15;
       ret_val151 = STR_ap2004550586(plus_self130, plus_sarg125);
       PROG_err_STR(barf_at_self15, ret_val151);
       exit_self15 = ((UNIX) NULL);
       exit_code15 = 1;
       exit(exit_code15);
      }
      L3431_=ARR((STR)r,i); 
      L344 = L3431_;
      L3451_=(INT)((unsigned char)L344); 
      L346 = L3451_;
      L3471_=(INT)((unsigned char)'0'); 
      L348 = L3471_;
      L3491_=INTMINUS(L346,L348); 
      l19 = L3491_;
      plus_self131 = res;
      L3501_=(STR)ARR((ARRAYSTR)args,l19); 
      plus_sarg126 = L3501_;
      ret_val152 = STR_ap2004550586(plus_self131, plus_sarg126);
      res = ret_val152;
      break;
     case '1': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self32 = self;
       plus_self132 = ((STR) &dncann144395436);
       plus_sarg127 = r;
       ret_val153 = STR_ap2004550586(plus_self132, plus_sarg127);
       plus_self133 = ret_val153;
       plus_sarg128 = ((STR) &inCONFIGfor1);
       ret_val154 = STR_ap2004550586(plus_self133, plus_sarg128);
       plus_self134 = ret_val154;
       plus_sarg129 = ATTR(fun11,str);
       ret_val155 = STR_ap2004550586(plus_self134, plus_sarg129);
       barf_msg32 = ret_val155;
       barf_self33 = ATTR(barf_self32,prog);
       barf_msg33 = barf_msg32;
       barf_at_self16 = barf_self33;
       barf_at_msg16 = barf_msg33;
       barf_at_at16 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self16, barf_at_at16);
       plus_self135 = ((STR) &Intern28965746);
       plus_sarg130 = barf_at_msg16;
       ret_val156 = STR_ap2004550586(plus_self135, plus_sarg130);
       PROG_err_STR(barf_at_self16, ret_val156);
       exit_self16 = ((UNIX) NULL);
       exit_code16 = 1;
       exit(exit_code16);
      }
      L3521_=ARR((STR)r,i); 
      L353 = L3521_;
      L3541_=(INT)((unsigned char)L353); 
      L355 = L3541_;
      L3561_=(INT)((unsigned char)'0'); 
      L357 = L3561_;
      L3581_=INTMINUS(L355,L357); 
      l20 = L3581_;
      plus_self136 = res;
      L3591_=(STR)ARR((ARRAYSTR)args,l20); 
      plus_sarg131 = L3591_;
      ret_val157 = STR_ap2004550586(plus_self136, plus_sarg131);
      res = ret_val157;
      break;
     case '2': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self34 = self;
       plus_self137 = ((STR) &dncann144395436);
       plus_sarg132 = r;
       ret_val158 = STR_ap2004550586(plus_self137, plus_sarg132);
       plus_self138 = ret_val158;
       plus_sarg133 = ((STR) &inCONFIGfor1);
       ret_val159 = STR_ap2004550586(plus_self138, plus_sarg133);
       plus_self139 = ret_val159;
       plus_sarg134 = ATTR(fun11,str);
       ret_val160 = STR_ap2004550586(plus_self139, plus_sarg134);
       barf_msg34 = ret_val160;
       barf_self35 = ATTR(barf_self34,prog);
       barf_msg35 = barf_msg34;
       barf_at_self17 = barf_self35;
       barf_at_msg17 = barf_msg35;
       barf_at_at17 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self17, barf_at_at17);
       plus_self140 = ((STR) &Intern28965746);
       plus_sarg135 = barf_at_msg17;
       ret_val161 = STR_ap2004550586(plus_self140, plus_sarg135);
       PROG_err_STR(barf_at_self17, ret_val161);
       exit_self17 = ((UNIX) NULL);
       exit_code17 = 1;
       exit(exit_code17);
      }
      L3601_=ARR((STR)r,i); 
      L361 = L3601_;
      L3621_=(INT)((unsigned char)L361); 
      L363 = L3621_;
      L3641_=(INT)((unsigned char)'0'); 
      L365 = L3641_;
      L3661_=INTMINUS(L363,L365); 
      l21 = L3661_;
      plus_self141 = res;
      L3671_=(STR)ARR((ARRAYSTR)args,l21); 
      plus_sarg136 = L3671_;
      ret_val162 = STR_ap2004550586(plus_self141, plus_sarg136);
      res = ret_val162;
      break;
     case '3': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self36 = self;
       plus_self142 = ((STR) &dncann144395436);
       plus_sarg137 = r;
       ret_val163 = STR_ap2004550586(plus_self142, plus_sarg137);
       plus_self143 = ret_val163;
       plus_sarg138 = ((STR) &inCONFIGfor1);
       ret_val164 = STR_ap2004550586(plus_self143, plus_sarg138);
       plus_self144 = ret_val164;
       plus_sarg139 = ATTR(fun11,str);
       ret_val165 = STR_ap2004550586(plus_self144, plus_sarg139);
       barf_msg36 = ret_val165;
       barf_self37 = ATTR(barf_self36,prog);
       barf_msg37 = barf_msg36;
       barf_at_self18 = barf_self37;
       barf_at_msg18 = barf_msg37;
       barf_at_at18 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self18, barf_at_at18);
       plus_self145 = ((STR) &Intern28965746);
       plus_sarg140 = barf_at_msg18;
       ret_val166 = STR_ap2004550586(plus_self145, plus_sarg140);
       PROG_err_STR(barf_at_self18, ret_val166);
       exit_self18 = ((UNIX) NULL);
       exit_code18 = 1;
       exit(exit_code18);
      }
      L3681_=ARR((STR)r,i); 
      L369 = L3681_;
      L3701_=(INT)((unsigned char)L369); 
      L372 = L3701_;
      L3731_=(INT)((unsigned char)'0'); 
      L374 = L3731_;
      L3751_=INTMINUS(L372,L374); 
      l22 = L3751_;
      plus_self146 = res;
      L3761_=(STR)ARR((ARRAYSTR)args,l22); 
      plus_sarg141 = L3761_;
      ret_val167 = STR_ap2004550586(plus_self146, plus_sarg141);
      res = ret_val167;
      break;
     case '4': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self38 = self;
       plus_self147 = ((STR) &dncann144395436);
       plus_sarg142 = r;
       ret_val168 = STR_ap2004550586(plus_self147, plus_sarg142);
       plus_self148 = ret_val168;
       plus_sarg143 = ((STR) &inCONFIGfor1);
       ret_val169 = STR_ap2004550586(plus_self148, plus_sarg143);
       plus_self149 = ret_val169;
       plus_sarg144 = ATTR(fun11,str);
       ret_val170 = STR_ap2004550586(plus_self149, plus_sarg144);
       barf_msg38 = ret_val170;
       barf_self39 = ATTR(barf_self38,prog);
       barf_msg39 = barf_msg38;
       barf_at_self19 = barf_self39;
       barf_at_msg19 = barf_msg39;
       barf_at_at19 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self19, barf_at_at19);
       plus_self150 = ((STR) &Intern28965746);
       plus_sarg145 = barf_at_msg19;
       ret_val171 = STR_ap2004550586(plus_self150, plus_sarg145);
       PROG_err_STR(barf_at_self19, ret_val171);
       exit_self19 = ((UNIX) NULL);
       exit_code19 = 1;
       exit(exit_code19);
      }
      L3771_=ARR((STR)r,i); 
      L378 = L3771_;
      L3791_=(INT)((unsigned char)L378); 
      L380 = L3791_;
      L3811_=(INT)((unsigned char)'0'); 
      L382 = L3811_;
      L3831_=INTMINUS(L380,L382); 
      l23 = L3831_;
      plus_self151 = res;
      L3841_=(STR)ARR((ARRAYSTR)args,l23); 
      plus_sarg146 = L3841_;
      ret_val172 = STR_ap2004550586(plus_self151, plus_sarg146);
      res = ret_val172;
      break;
     case '5': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self40 = self;
       plus_self152 = ((STR) &dncann144395436);
       plus_sarg147 = r;
       ret_val173 = STR_ap2004550586(plus_self152, plus_sarg147);
       plus_self153 = ret_val173;
       plus_sarg148 = ((STR) &inCONFIGfor1);
       ret_val174 = STR_ap2004550586(plus_self153, plus_sarg148);
       plus_self154 = ret_val174;
       plus_sarg149 = ATTR(fun11,str);
       ret_val175 = STR_ap2004550586(plus_self154, plus_sarg149);
       barf_msg40 = ret_val175;
       barf_self41 = ATTR(barf_self40,prog);
       barf_msg41 = barf_msg40;
       barf_at_self20 = barf_self41;
       barf_at_msg20 = barf_msg41;
       barf_at_at20 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self20, barf_at_at20);
       plus_self155 = ((STR) &Intern28965746);
       plus_sarg150 = barf_at_msg20;
       ret_val176 = STR_ap2004550586(plus_self155, plus_sarg150);
       PROG_err_STR(barf_at_self20, ret_val176);
       exit_self20 = ((UNIX) NULL);
       exit_code20 = 1;
       exit(exit_code20);
      }
      L3851_=ARR((STR)r,i); 
      L386 = L3851_;
      L3871_=(INT)((unsigned char)L386); 
      L388 = L3871_;
      L3891_=(INT)((unsigned char)'0'); 
      L390 = L3891_;
      L3921_=INTMINUS(L388,L390); 
      l24 = L3921_;
      plus_self156 = res;
      L3931_=(STR)ARR((ARRAYSTR)args,l24); 
      plus_sarg151 = L3931_;
      ret_val177 = STR_ap2004550586(plus_self156, plus_sarg151);
      res = ret_val177;
      break;
     case '6': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self42 = self;
       plus_self157 = ((STR) &dncann144395436);
       plus_sarg152 = r;
       ret_val178 = STR_ap2004550586(plus_self157, plus_sarg152);
       plus_self158 = ret_val178;
       plus_sarg153 = ((STR) &inCONFIGfor1);
       ret_val179 = STR_ap2004550586(plus_self158, plus_sarg153);
       plus_self159 = ret_val179;
       plus_sarg154 = ATTR(fun11,str);
       ret_val180 = STR_ap2004550586(plus_self159, plus_sarg154);
       barf_msg42 = ret_val180;
       barf_self43 = ATTR(barf_self42,prog);
       barf_msg43 = barf_msg42;
       barf_at_self21 = barf_self43;
       barf_at_msg21 = barf_msg43;
       barf_at_at21 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self21, barf_at_at21);
       plus_self160 = ((STR) &Intern28965746);
       plus_sarg155 = barf_at_msg21;
       ret_val181 = STR_ap2004550586(plus_self160, plus_sarg155);
       PROG_err_STR(barf_at_self21, ret_val181);
       exit_self21 = ((UNIX) NULL);
       exit_code21 = 1;
       exit(exit_code21);
      }
      L3941_=ARR((STR)r,i); 
      L395 = L3941_;
      L3961_=(INT)((unsigned char)L395); 
      L397 = L3961_;
      L3981_=(INT)((unsigned char)'0'); 
      L399 = L3981_;
      L4001_=INTMINUS(L397,L399); 
      l25 = L4001_;
      plus_self161 = res;
      L4011_=(STR)ARR((ARRAYSTR)args,l25); 
      plus_sarg156 = L4011_;
      ret_val182 = STR_ap2004550586(plus_self161, plus_sarg156);
      res = ret_val182;
      break;
     case '7': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self44 = self;
       plus_self162 = ((STR) &dncann144395436);
       plus_sarg157 = r;
       ret_val183 = STR_ap2004550586(plus_self162, plus_sarg157);
       plus_self163 = ret_val183;
       plus_sarg158 = ((STR) &inCONFIGfor1);
       ret_val184 = STR_ap2004550586(plus_self163, plus_sarg158);
       plus_self164 = ret_val184;
       plus_sarg159 = ATTR(fun11,str);
       ret_val185 = STR_ap2004550586(plus_self164, plus_sarg159);
       barf_msg44 = ret_val185;
       barf_self45 = ATTR(barf_self44,prog);
       barf_msg45 = barf_msg44;
       barf_at_self22 = barf_self45;
       barf_at_msg22 = barf_msg45;
       barf_at_at22 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self22, barf_at_at22);
       plus_self165 = ((STR) &Intern28965746);
       plus_sarg160 = barf_at_msg22;
       ret_val186 = STR_ap2004550586(plus_self165, plus_sarg160);
       PROG_err_STR(barf_at_self22, ret_val186);
       exit_self22 = ((UNIX) NULL);
       exit_code22 = 1;
       exit(exit_code22);
      }
      L4021_=ARR((STR)r,i); 
      L403 = L4021_;
      L4041_=(INT)((unsigned char)L403); 
      L405 = L4041_;
      L4061_=(INT)((unsigned char)'0'); 
      L407 = L4061_;
      L4081_=INTMINUS(L405,L407); 
      l26 = L4081_;
      plus_self166 = res;
      L4091_=(STR)ARR((ARRAYSTR)args,l26); 
      plus_sarg161 = L4091_;
      ret_val187 = STR_ap2004550586(plus_self166, plus_sarg161);
      res = ret_val187;
      break;
     case '8': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self46 = self;
       plus_self167 = ((STR) &dncann144395436);
       plus_sarg162 = r;
       ret_val188 = STR_ap2004550586(plus_self167, plus_sarg162);
       plus_self168 = ret_val188;
       plus_sarg163 = ((STR) &inCONFIGfor1);
       ret_val189 = STR_ap2004550586(plus_self168, plus_sarg163);
       plus_self169 = ret_val189;
       plus_sarg164 = ATTR(fun11,str);
       ret_val190 = STR_ap2004550586(plus_self169, plus_sarg164);
       barf_msg46 = ret_val190;
       barf_self47 = ATTR(barf_self46,prog);
       barf_msg47 = barf_msg46;
       barf_at_self23 = barf_self47;
       barf_at_msg23 = barf_msg47;
       barf_at_at23 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self23, barf_at_at23);
       plus_self170 = ((STR) &Intern28965746);
       plus_sarg165 = barf_at_msg23;
       ret_val191 = STR_ap2004550586(plus_self170, plus_sarg165);
       PROG_err_STR(barf_at_self23, ret_val191);
       exit_self23 = ((UNIX) NULL);
       exit_code23 = 1;
       exit(exit_code23);
      }
      L4101_=ARR((STR)r,i); 
      L411 = L4101_;
      L4121_=(INT)((unsigned char)L411); 
      L413 = L4121_;
      L4141_=(INT)((unsigned char)'0'); 
      L415 = L4141_;
      L4161_=INTMINUS(L413,L415); 
      l27 = L4161_;
      plus_self171 = res;
      L4171_=(STR)ARR((ARRAYSTR)args,l27); 
      plus_sarg166 = L4171_;
      ret_val192 = STR_ap2004550586(plus_self171, plus_sarg166);
      res = ret_val192;
      break;
     case '9': 
      if ((args==((ARRAYSTR) NULL))) {
       barf_self48 = self;
       plus_self172 = ((STR) &dncann144395436);
       plus_sarg167 = r;
       ret_val193 = STR_ap2004550586(plus_self172, plus_sarg167);
       plus_self173 = ret_val193;
       plus_sarg168 = ((STR) &inCONFIGfor1);
       ret_val194 = STR_ap2004550586(plus_self173, plus_sarg168);
       plus_self174 = ret_val194;
       plus_sarg169 = ATTR(fun11,str);
       ret_val195 = STR_ap2004550586(plus_self174, plus_sarg169);
       barf_msg48 = ret_val195;
       barf_self49 = ATTR(barf_self48,prog);
       barf_msg49 = barf_msg48;
       barf_at_self24 = barf_self49;
       barf_at_msg24 = barf_msg49;
       barf_at_at24 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self24, barf_at_at24);
       plus_self175 = ((STR) &Intern28965746);
       plus_sarg170 = barf_at_msg24;
       ret_val196 = STR_ap2004550586(plus_self175, plus_sarg170);
       PROG_err_STR(barf_at_self24, ret_val196);
       exit_self24 = ((UNIX) NULL);
       exit_code24 = 1;
       exit(exit_code24);
      }
      L4181_=ARR((STR)r,i); 
      L419 = L4181_;
      L4201_=(INT)((unsigned char)L419); 
      L422 = L4201_;
      L4231_=(INT)((unsigned char)'0'); 
      L424 = L4231_;
      L4251_=INTMINUS(L422,L424); 
      l28 = L4251_;
      plus_self176 = res;
      L4261_=(STR)ARR((ARRAYSTR)args,l28); 
      plus_sarg171 = L4261_;
      ret_val197 = STR_ap2004550586(plus_self176, plus_sarg171);
      res = ret_val197;
      break;
     case '$': 
      L4281_=(1)<(i); 
      if (L4281_) {
       L4291_=INTMINUS(i,2); 
       L430 = L4291_;
       L4311_=ARR((STR)r,L430); 
       L427 = CHAR_i1016724011(L4311_);
      } else {
       L427 = FALSE;
      }
      if (L427) {
       plus_self177 = res;
       plus_sarg172 = uniq;
       ret_val198 = STR_ap2004550586(plus_self177, plus_sarg172);
       res = ret_val198;
      }
      else {
       plus_self178 = res;
       plus_sarg173 = frame11;
       ret_val199 = STR_ap2004550586(plus_self178, plus_sarg173);
       plus_self179 = ret_val199;
       plus_sarg174 = uniq;
       ret_val200 = STR_ap2004550586(plus_self179, plus_sarg174);
       res = ret_val200;
      }
      break;
     case '#': 
      if ((loop_index==((STR) NULL))) {
       plus_self180 = res;
       plus_sarg175 = frame11;
       ret_val201 = STR_ap2004550586(plus_self180, plus_sarg175);
       plus_self181 = ret_val201;
       plus_sarg176 = ((STR) &i_);
       ret_val202 = STR_ap2004550586(plus_self181, plus_sarg176);
       plus_self182 = ret_val202;
       plus_sarg177 = uniq;
       ret_val203 = STR_ap2004550586(plus_self182, plus_sarg177);
       res = ret_val203;
      }
      else {
       plus_self183 = res;
       plus_sarg178 = loop_index;
       ret_val204 = STR_ap2004550586(plus_self183, plus_sarg178);
       res = ret_val204;
      }
      break;
     case '.': 
      arg_name1 = ((STR) &name1);
      L4321_=INTPLUS(i,1); 
      i = L4321_;
      while (1) {
       L4341_=ARR((STR)r,i); 
       if (CHAR_i1942379470(L4341_)) {
        L433 = TRUE;
       } else {
        L4351_=ARR((STR)r,i); 
        L436 = L4351_;
        L4371_=L436=='_'; 
        L433 = L4371_;
       }
       if (L433) {
       }
       else {
        goto after_loop3;
       }
       plus_self184 = arg_name1;
       L4381_=ARR((STR)r,i); 
       plus_arg5 = L4381_;
       str_self7 = plus_arg5;
       create_self5 = ((STR) NULL);
       create_c5 = str_self7;
       L440 = 1;
       L443=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L440)*sizeof(CHAR);
       L442=ZALLOC_LEAF_BIG(L443);
       if (L442==NULL) FATAL("Unable to allocate more memory");
       memset(L442,0,L443);
       ((OB)L442)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L442)->header.destroyed=0;
#endif

       L439 = ((STR) L442);
       L439->asize = L440;
       r6 = L439;
       SARR((STR)r6,0,create_c5); 
       ;
       ret_val207 = r6;
       ret_val206 = ret_val207;
       s5 = ret_val206;
       ret_val205 = STR_ap1077157958(plus_self184, s5, TRUE);
       arg_name1 = ret_val205;
       L4451_=INTPLUS(i,1); 
       i = L4451_;
      }
      after_loop3: ;
      L4461_=INTMINUS(i,1); 
      i = L4461_;
      done = FALSE;
      argi1 = IDENT_1150413730(IDENT_zero, arg_name1);
      {
       struct ELT_TBL_eltbrELT_frame_struct other5_0;
ELT_TBL_eltbrELT_frame noname2 = &other5_0;
       noname2->state = 0;
       while (1) {
        if (noname2->state == 0) {
         L448 = ATTR(fun11,tp);
         L449=(*dTP_implrIMPL[TAG(L448)])(L448);
         L71 = ATTR(L449,elts);
         noname2->self = L71;
        }
        L447 = ELT_TBL_eltbrELT(noname2);
        if (noname2->state == -1) {
         goto after_loop4;
        }
        elt2 = L447;
        if (ELT_is380037919(elt2)) {
         L452 = TRUE;
        } else {
         L452 = ELT_is1936066670(elt2);
        }
        if (L452) {
         L451 = TRUE;
        } else {
         L451 = ELT_is1269015106(elt2);
        }
        if (L451) {
         name_self1 = elt2;
         ret_val208 = ATTR(ATTR(name_self1,sig1),name1);
         is_eq_self1 = ret_val208;
         is_eq_i1 = argi1;
         ret_val209 = (is_eq_self1.str==is_eq_i1.str);
         L450 = ret_val209;
        } else {
         L450 = FALSE;
        }
        if (L450) {
         plus_self185 = res;
         mang_self2 = self;
         L454=ZALLOC(sizeof(struct IDENT_boxed_struct));
         if (L454==NULL) FATAL("Unable to allocate more memory");
         ((OB)L454)->header.tag=IDENT_tag;
         L453 = (OB)((IDENT_boxed) L454);
         ((IDENT_boxed) L453)->immutable_part = argi1;
         mang_ob2 = L453;
         tp_self = elt2;
         ret_val210 = ATTR(ATTR(tp_self,sig1),tp);
         mang_ns = ((OB) ret_val210);
         ret_val211 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns);
         plus_sarg179 = ret_val211;
         ret_val212 = STR_ap2004550586(plus_self185, plus_sarg179);
         res = ret_val212;
         done = TRUE;
         goto after_loop4;
        }
       }
      }
      after_loop4: ;
      L4551_=!(done); 
      if (L4551_) {
       barf_self50 = self;
       plus_self186 = ((STR) &unknwo513321933);
       plus_sarg180 = arg_name1;
       ret_val213 = STR_ap2004550586(plus_self186, plus_sarg180);
       plus_self187 = ret_val213;
       plus_sarg181 = ((STR) &inCONFIGmacrofor);
       ret_val214 = STR_ap2004550586(plus_self187, plus_sarg181);
       plus_self188 = ret_val214;
       plus_sarg182 = ATTR(fun11,str);
       ret_val215 = STR_ap2004550586(plus_self188, plus_sarg182);
       plus_self189 = ret_val215;
       plus_sarg183 = ((STR) &name52);
       ret_val216 = STR_ap2004550586(plus_self189, plus_sarg183);
       plus_self190 = ret_val216;
       plus_sarg184 = r;
       ret_val217 = STR_ap2004550586(plus_self190, plus_sarg184);
       plus_self191 = ret_val217;
       plus_sarg185 = ((STR) &name26);
       ret_val218 = STR_ap2004550586(plus_self191, plus_sarg185);
       barf_msg50 = ret_val218;
       barf_self51 = ATTR(barf_self50,prog);
       barf_msg51 = barf_msg50;
       barf_at_self25 = barf_self51;
       barf_at_msg25 = barf_msg51;
       barf_at_at25 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self25, barf_at_at25);
       plus_self192 = ((STR) &Intern28965746);
       plus_sarg186 = barf_at_msg25;
       ret_val219 = STR_ap2004550586(plus_self192, plus_sarg186);
       PROG_err_STR(barf_at_self25, ret_val219);
       exit_self25 = ((UNIX) NULL);
       exit_code25 = 1;
       exit(exit_code25);
      }
      break;
     default: ;
      barf_self52 = self;
      plus_self193 = ((STR) &unknownd);
      L4561_=ARR((STR)r,i); 
      plus_arg6 = L4561_;
      str_self8 = plus_arg6;
      create_self6 = ((STR) NULL);
      create_c6 = str_self8;
      L458 = 1;
      L460=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L458)*sizeof(CHAR);
      L459=ZALLOC_LEAF_BIG(L460);
      if (L459==NULL) FATAL("Unable to allocate more memory");
      memset(L459,0,L460);
      ((OB)L459)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L459)->header.destroyed=0;
#endif

      L457 = ((STR) L459);
      L457->asize = L458;
      r7 = L457;
      SARR((STR)r7,0,create_c6); 
      ;
      ret_val222 = r7;
      ret_val221 = ret_val222;
      s6 = ret_val221;
      ret_val220 = STR_ap1077157958(plus_self193, s6, TRUE);
      plus_self194 = ret_val220;
      plus_sarg187 = ((STR) &macroinCONFIGfor);
      ret_val223 = STR_ap2004550586(plus_self194, plus_sarg187);
      plus_self195 = ret_val223;
      plus_sarg188 = ATTR(fun11,str);
      ret_val224 = STR_ap2004550586(plus_self195, plus_sarg188);
      plus_self196 = ret_val224;
      plus_sarg189 = ((STR) &name52);
      ret_val225 = STR_ap2004550586(plus_self196, plus_sarg189);
      plus_self197 = ret_val225;
      plus_sarg190 = r;
      ret_val226 = STR_ap2004550586(plus_self197, plus_sarg190);
      plus_self198 = ret_val226;
      plus_sarg191 = ((STR) &name26);
      ret_val227 = STR_ap2004550586(plus_self198, plus_sarg191);
      barf_msg52 = ret_val227;
      barf_self53 = ATTR(barf_self52,prog);
      barf_msg53 = barf_msg52;
      barf_at_self26 = barf_self53;
      barf_at_msg26 = barf_msg53;
      barf_at_at26 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self26, barf_at_at26);
      plus_self199 = ((STR) &Intern28965746);
      plus_sarg192 = barf_at_msg26;
      ret_val228 = STR_ap2004550586(plus_self199, plus_sarg192);
      PROG_err_STR(barf_at_self26, ret_val228);
      exit_self26 = ((UNIX) NULL);
      exit_code26 = 1;
      exit(exit_code26);
    }
    break;
   case '%': 
    L4631_=INTPLUS(i,1); 
    i = L4631_;
    L4641_=ARR((STR)r,i); 
    L81 = L4641_;
    switch (L81) {
     case '0': 
      L465 = ATTR(fun11,tp);
      if ((*dTP_is1334578382[TAG(L465)])(L465)) {
       L466 = ATTR(fun11,tp);
       if ((*dTP_is418055720[TAG(L466)])(L466)) {
        plus_self200 = res;
        plus_arg7 = '1';
        str_self9 = plus_arg7;
        create_self7 = ((STR) NULL);
        create_c7 = str_self9;
        L468 = 1;
        L470=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L468)*sizeof(CHAR);
        L469=ZALLOC_LEAF_BIG(L470);
        if (L469==NULL) FATAL("Unable to allocate more memory");
        memset(L469,0,L470);
        ((OB)L469)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L469)->header.destroyed=0;
#endif

        L467 = ((STR) L469);
        L467->asize = L468;
        r8 = L467;
        SARR((STR)r8,0,create_c7); 
        ;
        ret_val231 = r8;
        ret_val230 = ret_val231;
        s7 = ret_val230;
        ret_val229 = STR_ap1077157958(plus_self200, s7, TRUE);
        res = ret_val229;
       }
       else {
        plus_self201 = res;
        plus_arg8 = '2';
        str_self10 = plus_arg8;
        create_self8 = ((STR) NULL);
        create_c8 = str_self10;
        L473 = 1;
        L475=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L473)*sizeof(CHAR);
        L474=ZALLOC_LEAF_BIG(L475);
        if (L474==NULL) FATAL("Unable to allocate more memory");
        memset(L474,0,L475);
        ((OB)L474)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L474)->header.destroyed=0;
#endif

        L472 = ((STR) L474);
        L472->asize = L473;
        r9 = L472;
        SARR((STR)r9,0,create_c8); 
        ;
        ret_val234 = r9;
        ret_val233 = ret_val234;
        s8 = ret_val233;
        ret_val232 = STR_ap1077157958(plus_self201, s8, TRUE);
        res = ret_val232;
       }
      }
      else {
       plus_self202 = res;
       plus_arg9 = '0';
       str_self11 = plus_arg9;
       create_self9 = ((STR) NULL);
       create_c9 = str_self11;
       L478 = 1;
       L480=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L478)*sizeof(CHAR);
       L479=ZALLOC_LEAF_BIG(L480);
       if (L479==NULL) FATAL("Unable to allocate more memory");
       memset(L479,0,L480);
       ((OB)L479)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L479)->header.destroyed=0;
#endif

       L477 = ((STR) L479);
       L477->asize = L478;
       r10 = L477;
       SARR((STR)r10,0,create_c9); 
       ;
       ret_val237 = r10;
       ret_val236 = ret_val237;
       s9 = ret_val236;
       ret_val235 = STR_ap1077157958(plus_self202, s9, TRUE);
       res = ret_val235;
      }
      break;
     case '1': 
      L4831_=ARR((STR)r,i); 
      L484 = L4831_;
      L4851_=(INT)((unsigned char)L484); 
      L486 = L4851_;
      L4871_=(INT)((unsigned char)'1'); 
      L488 = L4871_;
      L4891_=INTMINUS(L486,L488); 
      l29 = L4891_;
      L490 = ATTR(fun11,args);
      L4911_=(ARG)ARR((ARRAYARG)L490,l29); 
      L492=L4911_;
      L493 = ATTR(L492,tp);
      if ((*dTP_is1334578382[TAG(L493)])(L493)) {
       L494 = ATTR(fun11,args);
       L4951_=(ARG)ARR((ARRAYARG)L494,l29); 
       L496=L4951_;
       L497 = ATTR(L496,tp);
       if ((*dTP_is418055720[TAG(L497)])(L497)) {
        plus_self203 = res;
        plus_arg10 = '1';
        str_self12 = plus_arg10;
        create_self10 = ((STR) NULL);
        create_c10 = str_self12;
        L499 = 1;
        L502=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L499)*sizeof(CHAR);
        L500=ZALLOC_LEAF_BIG(L502);
        if (L500==NULL) FATAL("Unable to allocate more memory");
        memset(L500,0,L502);
        ((OB)L500)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L500)->header.destroyed=0;
#endif

        L498 = ((STR) L500);
        L498->asize = L499;
        r11 = L498;
        SARR((STR)r11,0,create_c10); 
        ;
        ret_val240 = r11;
        ret_val239 = ret_val240;
        s10 = ret_val239;
        ret_val238 = STR_ap1077157958(plus_self203, s10, TRUE);
        res = ret_val238;
       }
       else {
        plus_self204 = res;
        plus_arg11 = '2';
        str_self13 = plus_arg11;
        create_self11 = ((STR) NULL);
        create_c11 = str_self13;
        L505 = 1;
        L507=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L505)*sizeof(CHAR);
        L506=ZALLOC_LEAF_BIG(L507);
        if (L506==NULL) FATAL("Unable to allocate more memory");
        memset(L506,0,L507);
        ((OB)L506)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L506)->header.destroyed=0;
#endif

        L504 = ((STR) L506);
        L504->asize = L505;
        r12 = L504;
        SARR((STR)r12,0,create_c11); 
        ;
        ret_val243 = r12;
        ret_val242 = ret_val243;
        s11 = ret_val242;
        ret_val241 = STR_ap1077157958(plus_self204, s11, TRUE);
        res = ret_val241;
       }
      }
      else {
       plus_self205 = res;
       plus_arg12 = '0';
       str_self14 = plus_arg12;
       create_self12 = ((STR) NULL);
       create_c12 = str_self14;
       L510 = 1;
       L512=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L510)*sizeof(CHAR);
       L511=ZALLOC_LEAF_BIG(L512);
       if (L511==NULL) FATAL("Unable to allocate more memory");
       memset(L511,0,L512);
       ((OB)L511)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L511)->header.destroyed=0;
#endif

       L509 = ((STR) L511);
       L509->asize = L510;
       r13 = L509;
       SARR((STR)r13,0,create_c12); 
       ;
       ret_val246 = r13;
       ret_val245 = ret_val246;
       s12 = ret_val245;
       ret_val244 = STR_ap1077157958(plus_self205, s12, TRUE);
       res = ret_val244;
      }
      break;
     case '2': 
      L5141_=ARR((STR)r,i); 
      L515 = L5141_;
      L5161_=(INT)((unsigned char)L515); 
      L517 = L5161_;
      L5181_=(INT)((unsigned char)'1'); 
      L519 = L5181_;
      L5201_=INTMINUS(L517,L519); 
      l30 = L5201_;
      L521 = ATTR(fun11,args);
      L5221_=(ARG)ARR((ARRAYARG)L521,l30); 
      L523=L5221_;
      L524 = ATTR(L523,tp);
      if ((*dTP_is1334578382[TAG(L524)])(L524)) {
       L525 = ATTR(fun11,args);
       L5261_=(ARG)ARR((ARRAYARG)L525,l30); 
       L527=L5261_;
       L528 = ATTR(L527,tp);
       if ((*dTP_is418055720[TAG(L528)])(L528)) {
        plus_self206 = res;
        plus_arg13 = '1';
        str_self15 = plus_arg13;
        create_self13 = ((STR) NULL);
        create_c13 = str_self15;
        L530 = 1;
        L533=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L530)*sizeof(CHAR);
        L532=ZALLOC_LEAF_BIG(L533);
        if (L532==NULL) FATAL("Unable to allocate more memory");
        memset(L532,0,L533);
        ((OB)L532)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L532)->header.destroyed=0;
#endif

        L529 = ((STR) L532);
        L529->asize = L530;
        r14 = L529;
        SARR((STR)r14,0,create_c13); 
        ;
        ret_val249 = r14;
        ret_val248 = ret_val249;
        s13 = ret_val248;
        ret_val247 = STR_ap1077157958(plus_self206, s13, TRUE);
        res = ret_val247;
       }
       else {
        plus_self207 = res;
        plus_arg14 = '2';
        str_self16 = plus_arg14;
        create_self14 = ((STR) NULL);
        create_c14 = str_self16;
        L536 = 1;
        L538=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L536)*sizeof(CHAR);
        L537=ZALLOC_LEAF_BIG(L538);
        if (L537==NULL) FATAL("Unable to allocate more memory");
        memset(L537,0,L538);
        ((OB)L537)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L537)->header.destroyed=0;
#endif

        L535 = ((STR) L537);
        L535->asize = L536;
        r15 = L535;
        SARR((STR)r15,0,create_c14); 
        ;
        ret_val252 = r15;
        ret_val251 = ret_val252;
        s14 = ret_val251;
        ret_val250 = STR_ap1077157958(plus_self207, s14, TRUE);
        res = ret_val250;
       }
      }
      else {
       plus_self208 = res;
       plus_arg15 = '0';
       str_self17 = plus_arg15;
       create_self15 = ((STR) NULL);
       create_c15 = str_self17;
       L541 = 1;
       L543=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L541)*sizeof(CHAR);
       L542=ZALLOC_LEAF_BIG(L543);
       if (L542==NULL) FATAL("Unable to allocate more memory");
       memset(L542,0,L543);
       ((OB)L542)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L542)->header.destroyed=0;
#endif

       L540 = ((STR) L542);
       L540->asize = L541;
       r16 = L540;
       SARR((STR)r16,0,create_c15); 
       ;
       ret_val255 = r16;
       ret_val254 = ret_val255;
       s15 = ret_val254;
       ret_val253 = STR_ap1077157958(plus_self208, s15, TRUE);
       res = ret_val253;
      }
      break;
     case '3': 
      L5451_=ARR((STR)r,i); 
      L546 = L5451_;
      L5471_=(INT)((unsigned char)L546); 
      L548 = L5471_;
      L5491_=(INT)((unsigned char)'1'); 
      L550 = L5491_;
      L5521_=INTMINUS(L548,L550); 
      l31 = L5521_;
      L553 = ATTR(fun11,args);
      L5541_=(ARG)ARR((ARRAYARG)L553,l31); 
      L555=L5541_;
      L556 = ATTR(L555,tp);
      if ((*dTP_is1334578382[TAG(L556)])(L556)) {
       L557 = ATTR(fun11,args);
       L5581_=(ARG)ARR((ARRAYARG)L557,l31); 
       L559=L5581_;
       L560 = ATTR(L559,tp);
       if ((*dTP_is418055720[TAG(L560)])(L560)) {
        plus_self209 = res;
        plus_arg16 = '1';
        str_self18 = plus_arg16;
        create_self16 = ((STR) NULL);
        create_c16 = str_self18;
        L562 = 1;
        L564=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L562)*sizeof(CHAR);
        L563=ZALLOC_LEAF_BIG(L564);
        if (L563==NULL) FATAL("Unable to allocate more memory");
        memset(L563,0,L564);
        ((OB)L563)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L563)->header.destroyed=0;
#endif

        L561 = ((STR) L563);
        L561->asize = L562;
        r17 = L561;
        SARR((STR)r17,0,create_c16); 
        ;
        ret_val258 = r17;
        ret_val257 = ret_val258;
        s16 = ret_val257;
        ret_val256 = STR_ap1077157958(plus_self209, s16, TRUE);
        res = ret_val256;
       }
       else {
        plus_self210 = res;
        plus_arg17 = '2';
        str_self19 = plus_arg17;
        create_self17 = ((STR) NULL);
        create_c17 = str_self19;
        L567 = 1;
        L569=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L567)*sizeof(CHAR);
        L568=ZALLOC_LEAF_BIG(L569);
        if (L568==NULL) FATAL("Unable to allocate more memory");
        memset(L568,0,L569);
        ((OB)L568)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L568)->header.destroyed=0;
#endif

        L566 = ((STR) L568);
        L566->asize = L567;
        r18 = L566;
        SARR((STR)r18,0,create_c17); 
        ;
        ret_val261 = r18;
        ret_val260 = ret_val261;
        s17 = ret_val260;
        ret_val259 = STR_ap1077157958(plus_self210, s17, TRUE);
        res = ret_val259;
       }
      }
      else {
       plus_self211 = res;
       plus_arg18 = '0';
       str_self20 = plus_arg18;
       create_self18 = ((STR) NULL);
       create_c18 = str_self20;
       L573 = 1;
       L575=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L573)*sizeof(CHAR);
       L574=ZALLOC_LEAF_BIG(L575);
       if (L574==NULL) FATAL("Unable to allocate more memory");
       memset(L574,0,L575);
       ((OB)L574)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L574)->header.destroyed=0;
#endif

       L572 = ((STR) L574);
       L572->asize = L573;
       r19 = L572;
       SARR((STR)r19,0,create_c18); 
       ;
       ret_val264 = r19;
       ret_val263 = ret_val264;
       s18 = ret_val263;
       ret_val262 = STR_ap1077157958(plus_self211, s18, TRUE);
       res = ret_val262;
      }
      break;
     case '4': 
      L5771_=ARR((STR)r,i); 
      L578 = L5771_;
      L5791_=(INT)((unsigned char)L578); 
      L580 = L5791_;
      L5811_=(INT)((unsigned char)'1'); 
      L582 = L5811_;
      L5831_=INTMINUS(L580,L582); 
      l32 = L5831_;
      L584 = ATTR(fun11,args);
      L5851_=(ARG)ARR((ARRAYARG)L584,l32); 
      L586=L5851_;
      L587 = ATTR(L586,tp);
      if ((*dTP_is1334578382[TAG(L587)])(L587)) {
       L588 = ATTR(fun11,args);
       L5891_=(ARG)ARR((ARRAYARG)L588,l32); 
       L590=L5891_;
       L592 = ATTR(L590,tp);
       if ((*dTP_is418055720[TAG(L592)])(L592)) {
        plus_self212 = res;
        plus_arg19 = '1';
        str_self21 = plus_arg19;
        create_self19 = ((STR) NULL);
        create_c19 = str_self21;
        L594 = 1;
        L596=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L594)*sizeof(CHAR);
        L595=ZALLOC_LEAF_BIG(L596);
        if (L595==NULL) FATAL("Unable to allocate more memory");
        memset(L595,0,L596);
        ((OB)L595)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L595)->header.destroyed=0;
#endif

        L593 = ((STR) L595);
        L593->asize = L594;
        r20 = L593;
        SARR((STR)r20,0,create_c19); 
        ;
        ret_val267 = r20;
        ret_val266 = ret_val267;
        s19 = ret_val266;
        ret_val265 = STR_ap1077157958(plus_self212, s19, TRUE);
        res = ret_val265;
       }
       else {
        plus_self213 = res;
        plus_arg20 = '2';
        str_self22 = plus_arg20;
        create_self20 = ((STR) NULL);
        create_c20 = str_self22;
        L599 = 1;
        L601=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L599)*sizeof(CHAR);
        L600=ZALLOC_LEAF_BIG(L601);
        if (L600==NULL) FATAL("Unable to allocate more memory");
        memset(L600,0,L601);
        ((OB)L600)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L600)->header.destroyed=0;
#endif

        L598 = ((STR) L600);
        L598->asize = L599;
        r21 = L598;
        SARR((STR)r21,0,create_c20); 
        ;
        ret_val270 = r21;
        ret_val269 = ret_val270;
        s20 = ret_val269;
        ret_val268 = STR_ap1077157958(plus_self213, s20, TRUE);
        res = ret_val268;
       }
      }
      else {
       plus_self214 = res;
       plus_arg21 = '0';
       str_self23 = plus_arg21;
       create_self21 = ((STR) NULL);
       create_c21 = str_self23;
       L604 = 1;
       L606=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L604)*sizeof(CHAR);
       L605=ZALLOC_LEAF_BIG(L606);
       if (L605==NULL) FATAL("Unable to allocate more memory");
       memset(L605,0,L606);
       ((OB)L605)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L605)->header.destroyed=0;
#endif

       L603 = ((STR) L605);
       L603->asize = L604;
       r22 = L603;
       SARR((STR)r22,0,create_c21); 
       ;
       ret_val273 = r22;
       ret_val272 = ret_val273;
       s21 = ret_val272;
       ret_val271 = STR_ap1077157958(plus_self214, s21, TRUE);
       res = ret_val271;
      }
      break;
     case '5': 
      L6081_=ARR((STR)r,i); 
      L609 = L6081_;
      L6101_=(INT)((unsigned char)L609); 
      L611 = L6101_;
      L6121_=(INT)((unsigned char)'1'); 
      L613 = L6121_;
      L6141_=INTMINUS(L611,L613); 
      l33 = L6141_;
      L615 = ATTR(fun11,args);
      L6161_=(ARG)ARR((ARRAYARG)L615,l33); 
      L617=L6161_;
      L618 = ATTR(L617,tp);
      if ((*dTP_is1334578382[TAG(L618)])(L618)) {
       L619 = ATTR(fun11,args);
       L6201_=(ARG)ARR((ARRAYARG)L619,l33); 
       L622=L6201_;
       L623 = ATTR(L622,tp);
       if ((*dTP_is418055720[TAG(L623)])(L623)) {
        plus_self215 = res;
        plus_arg22 = '1';
        str_self24 = plus_arg22;
        create_self22 = ((STR) NULL);
        create_c22 = str_self24;
        L625 = 1;
        L627=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L625)*sizeof(CHAR);
        L626=ZALLOC_LEAF_BIG(L627);
        if (L626==NULL) FATAL("Unable to allocate more memory");
        memset(L626,0,L627);
        ((OB)L626)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L626)->header.destroyed=0;
#endif

        L624 = ((STR) L626);
        L624->asize = L625;
        r23 = L624;
        SARR((STR)r23,0,create_c22); 
        ;
        ret_val276 = r23;
        ret_val275 = ret_val276;
        s22 = ret_val275;
        ret_val274 = STR_ap1077157958(plus_self215, s22, TRUE);
        res = ret_val274;
       }
       else {
        plus_self216 = res;
        plus_arg23 = '2';
        str_self25 = plus_arg23;
        create_self23 = ((STR) NULL);
        create_c23 = str_self25;
        L630 = 1;
        L632=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L630)*sizeof(CHAR);
        L631=ZALLOC_LEAF_BIG(L632);
        if (L631==NULL) FATAL("Unable to allocate more memory");
        memset(L631,0,L632);
        ((OB)L631)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L631)->header.destroyed=0;
#endif

        L629 = ((STR) L631);
        L629->asize = L630;
        r24 = L629;
        SARR((STR)r24,0,create_c23); 
        ;
        ret_val279 = r24;
        ret_val278 = ret_val279;
        s23 = ret_val278;
        ret_val277 = STR_ap1077157958(plus_self216, s23, TRUE);
        res = ret_val277;
       }
      }
      else {
       plus_self217 = res;
       plus_arg24 = '0';
       str_self26 = plus_arg24;
       create_self24 = ((STR) NULL);
       create_c24 = str_self26;
       L635 = 1;
       L637=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L635)*sizeof(CHAR);
       L636=ZALLOC_LEAF_BIG(L637);
       if (L636==NULL) FATAL("Unable to allocate more memory");
       memset(L636,0,L637);
       ((OB)L636)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L636)->header.destroyed=0;
#endif

       L634 = ((STR) L636);
       L634->asize = L635;
       r25 = L634;
       SARR((STR)r25,0,create_c24); 
       ;
       ret_val282 = r25;
       ret_val281 = ret_val282;
       s24 = ret_val281;
       ret_val280 = STR_ap1077157958(plus_self217, s24, TRUE);
       res = ret_val280;
      }
      break;
     case '6': 
      L6391_=ARR((STR)r,i); 
      L640 = L6391_;
      L6421_=(INT)((unsigned char)L640); 
      L643 = L6421_;
      L6441_=(INT)((unsigned char)'1'); 
      L645 = L6441_;
      L6461_=INTMINUS(L643,L645); 
      l34 = L6461_;
      L647 = ATTR(fun11,args);
      L6481_=(ARG)ARR((ARRAYARG)L647,l34); 
      L649=L6481_;
      L650 = ATTR(L649,tp);
      if ((*dTP_is1334578382[TAG(L650)])(L650)) {
       L651 = ATTR(fun11,args);
       L6521_=(ARG)ARR((ARRAYARG)L651,l34); 
       L653=L6521_;
       L654 = ATTR(L653,tp);
       if ((*dTP_is418055720[TAG(L654)])(L654)) {
        plus_self218 = res;
        plus_arg25 = '1';
        str_self27 = plus_arg25;
        create_self25 = ((STR) NULL);
        create_c25 = str_self27;
        L656 = 1;
        L658=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L656)*sizeof(CHAR);
        L657=ZALLOC_LEAF_BIG(L658);
        if (L657==NULL) FATAL("Unable to allocate more memory");
        memset(L657,0,L658);
        ((OB)L657)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L657)->header.destroyed=0;
#endif

        L655 = ((STR) L657);
        L655->asize = L656;
        r26 = L655;
        SARR((STR)r26,0,create_c25); 
        ;
        ret_val285 = r26;
        ret_val284 = ret_val285;
        s25 = ret_val284;
        ret_val283 = STR_ap1077157958(plus_self218, s25, TRUE);
        res = ret_val283;
       }
       else {
        plus_self219 = res;
        plus_arg26 = '2';
        str_self28 = plus_arg26;
        create_self26 = ((STR) NULL);
        create_c26 = str_self28;
        L662 = 1;
        L664=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L662)*sizeof(CHAR);
        L663=ZALLOC_LEAF_BIG(L664);
        if (L663==NULL) FATAL("Unable to allocate more memory");
        memset(L663,0,L664);
        ((OB)L663)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L663)->header.destroyed=0;
#endif

        L660 = ((STR) L663);
        L660->asize = L662;
        r27 = L660;
        SARR((STR)r27,0,create_c26); 
        ;
        ret_val288 = r27;
        ret_val287 = ret_val288;
        s26 = ret_val287;
        ret_val286 = STR_ap1077157958(plus_self219, s26, TRUE);
        res = ret_val286;
       }
      }
      else {
       plus_self220 = res;
       plus_arg27 = '0';
       str_self29 = plus_arg27;
       create_self27 = ((STR) NULL);
       create_c27 = str_self29;
       L667 = 1;
       L669=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L667)*sizeof(CHAR);
       L668=ZALLOC_LEAF_BIG(L669);
       if (L668==NULL) FATAL("Unable to allocate more memory");
       memset(L668,0,L669);
       ((OB)L668)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L668)->header.destroyed=0;
#endif

       L666 = ((STR) L668);
       L666->asize = L667;
       r28 = L666;
       SARR((STR)r28,0,create_c27); 
       ;
       ret_val291 = r28;
       ret_val290 = ret_val291;
       s27 = ret_val290;
       ret_val289 = STR_ap1077157958(plus_self220, s27, TRUE);
       res = ret_val289;
      }
      break;
     case '7': 
      L6711_=ARR((STR)r,i); 
      L672 = L6711_;
      L6731_=(INT)((unsigned char)L672); 
      L674 = L6731_;
      L6751_=(INT)((unsigned char)'1'); 
      L676 = L6751_;
      L6771_=INTMINUS(L674,L676); 
      l35 = L6771_;
      L678 = ATTR(fun11,args);
      L6791_=(ARG)ARR((ARRAYARG)L678,l35); 
      L680=L6791_;
      L682 = ATTR(L680,tp);
      if ((*dTP_is1334578382[TAG(L682)])(L682)) {
       L683 = ATTR(fun11,args);
       L6841_=(ARG)ARR((ARRAYARG)L683,l35); 
       L685=L6841_;
       L686 = ATTR(L685,tp);
       if ((*dTP_is418055720[TAG(L686)])(L686)) {
        plus_self221 = res;
        plus_arg28 = '1';
        str_self30 = plus_arg28;
        create_self28 = ((STR) NULL);
        create_c28 = str_self30;
        L688 = 1;
        L690=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L688)*sizeof(CHAR);
        L689=ZALLOC_LEAF_BIG(L690);
        if (L689==NULL) FATAL("Unable to allocate more memory");
        memset(L689,0,L690);
        ((OB)L689)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L689)->header.destroyed=0;
#endif

        L687 = ((STR) L689);
        L687->asize = L688;
        r29 = L687;
        SARR((STR)r29,0,create_c28); 
        ;
        ret_val294 = r29;
        ret_val293 = ret_val294;
        s28 = ret_val293;
        ret_val292 = STR_ap1077157958(plus_self221, s28, TRUE);
        res = ret_val292;
       }
       else {
        plus_self222 = res;
        plus_arg29 = '2';
        str_self31 = plus_arg29;
        create_self29 = ((STR) NULL);
        create_c29 = str_self31;
        L693 = 1;
        L695=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L693)*sizeof(CHAR);
        L694=ZALLOC_LEAF_BIG(L695);
        if (L694==NULL) FATAL("Unable to allocate more memory");
        memset(L694,0,L695);
        ((OB)L694)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L694)->header.destroyed=0;
#endif

        L692 = ((STR) L694);
        L692->asize = L693;
        r30 = L692;
        SARR((STR)r30,0,create_c29); 
        ;
        ret_val297 = r30;
        ret_val296 = ret_val297;
        s29 = ret_val296;
        ret_val295 = STR_ap1077157958(plus_self222, s29, TRUE);
        res = ret_val295;
       }
      }
      else {
       plus_self223 = res;
       plus_arg30 = '0';
       str_self32 = plus_arg30;
       create_self30 = ((STR) NULL);
       create_c30 = str_self32;
       L698 = 1;
       L700=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L698)*sizeof(CHAR);
       L699=ZALLOC_LEAF_BIG(L700);
       if (L699==NULL) FATAL("Unable to allocate more memory");
       memset(L699,0,L700);
       ((OB)L699)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L699)->header.destroyed=0;
#endif

       L697 = ((STR) L699);
       L697->asize = L698;
       r31 = L697;
       SARR((STR)r31,0,create_c30); 
       ;
       ret_val300 = r31;
       ret_val299 = ret_val300;
       s30 = ret_val299;
       ret_val298 = STR_ap1077157958(plus_self223, s30, TRUE);
       res = ret_val298;
      }
      break;
     case '8': 
      L7031_=ARR((STR)r,i); 
      L704 = L7031_;
      L7051_=(INT)((unsigned char)L704); 
      L706 = L7051_;
      L7071_=(INT)((unsigned char)'1'); 
      L708 = L7071_;
      L7091_=INTMINUS(L706,L708); 
      l36 = L7091_;
      L710 = ATTR(fun11,args);
      L7111_=(ARG)ARR((ARRAYARG)L710,l36); 
      L712=L7111_;
      L713 = ATTR(L712,tp);
      if ((*dTP_is1334578382[TAG(L713)])(L713)) {
       L714 = ATTR(fun11,args);
       L7151_=(ARG)ARR((ARRAYARG)L714,l36); 
       L716=L7151_;
       L717 = ATTR(L716,tp);
       if ((*dTP_is418055720[TAG(L717)])(L717)) {
        plus_self224 = res;
        plus_arg31 = '1';
        str_self33 = plus_arg31;
        create_self31 = ((STR) NULL);
        create_c31 = str_self33;
        L719 = 1;
        L721=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L719)*sizeof(CHAR);
        L720=ZALLOC_LEAF_BIG(L721);
        if (L720==NULL) FATAL("Unable to allocate more memory");
        memset(L720,0,L721);
        ((OB)L720)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L720)->header.destroyed=0;
#endif

        L718 = ((STR) L720);
        L718->asize = L719;
        r32 = L718;
        SARR((STR)r32,0,create_c31); 
        ;
        ret_val303 = r32;
        ret_val302 = ret_val303;
        s31 = ret_val302;
        ret_val301 = STR_ap1077157958(plus_self224, s31, TRUE);
        res = ret_val301;
       }
       else {
        plus_self225 = res;
        plus_arg32 = '2';
        str_self34 = plus_arg32;
        create_self32 = ((STR) NULL);
        create_c32 = str_self34;
        L724 = 1;
        L726=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L724)*sizeof(CHAR);
        L725=ZALLOC_LEAF_BIG(L726);
        if (L725==NULL) FATAL("Unable to allocate more memory");
        memset(L725,0,L726);
        ((OB)L725)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L725)->header.destroyed=0;
#endif

        L723 = ((STR) L725);
        L723->asize = L724;
        r33 = L723;
        SARR((STR)r33,0,create_c32); 
        ;
        ret_val306 = r33;
        ret_val305 = ret_val306;
        s32 = ret_val305;
        ret_val304 = STR_ap1077157958(plus_self225, s32, TRUE);
        res = ret_val304;
       }
      }
      else {
       plus_self226 = res;
       plus_arg33 = '0';
       str_self35 = plus_arg33;
       create_self33 = ((STR) NULL);
       create_c33 = str_self35;
       L729 = 1;
       L732=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L729)*sizeof(CHAR);
       L730=ZALLOC_LEAF_BIG(L732);
       if (L730==NULL) FATAL("Unable to allocate more memory");
       memset(L730,0,L732);
       ((OB)L730)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L730)->header.destroyed=0;
#endif

       L728 = ((STR) L730);
       L728->asize = L729;
       r34 = L728;
       SARR((STR)r34,0,create_c33); 
       ;
       ret_val309 = r34;
       ret_val308 = ret_val309;
       s33 = ret_val308;
       ret_val307 = STR_ap1077157958(plus_self226, s33, TRUE);
       res = ret_val307;
      }
      break;
     case '9': 
      L7341_=ARR((STR)r,i); 
      L735 = L7341_;
      L7361_=(INT)((unsigned char)L735); 
      L737 = L7361_;
      L7381_=(INT)((unsigned char)'1'); 
      L739 = L7381_;
      L7401_=INTMINUS(L737,L739); 
      l37 = L7401_;
      L741 = ATTR(fun11,args);
      L7421_=(ARG)ARR((ARRAYARG)L741,l37); 
      L743=L7421_;
      L744 = ATTR(L743,tp);
      if ((*dTP_is1334578382[TAG(L744)])(L744)) {
       L745 = ATTR(fun11,args);
       L7461_=(ARG)ARR((ARRAYARG)L745,l37); 
       L747=L7461_;
       L748 = ATTR(L747,tp);
       if ((*dTP_is418055720[TAG(L748)])(L748)) {
        plus_self227 = res;
        plus_arg34 = '1';
        str_self36 = plus_arg34;
        create_self34 = ((STR) NULL);
        create_c34 = str_self36;
        L750 = 1;
        L753=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L750)*sizeof(CHAR);
        L752=ZALLOC_LEAF_BIG(L753);
        if (L752==NULL) FATAL("Unable to allocate more memory");
        memset(L752,0,L753);
        ((OB)L752)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L752)->header.destroyed=0;
#endif

        L749 = ((STR) L752);
        L749->asize = L750;
        r35 = L749;
        SARR((STR)r35,0,create_c34); 
        ;
        ret_val312 = r35;
        ret_val311 = ret_val312;
        s34 = ret_val311;
        ret_val310 = STR_ap1077157958(plus_self227, s34, TRUE);
        res = ret_val310;
       }
       else {
        plus_self228 = res;
        plus_arg35 = '2';
        str_self37 = plus_arg35;
        create_self35 = ((STR) NULL);
        create_c35 = str_self37;
        L756 = 1;
        L758=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L756)*sizeof(CHAR);
        L757=ZALLOC_LEAF_BIG(L758);
        if (L757==NULL) FATAL("Unable to allocate more memory");
        memset(L757,0,L758);
        ((OB)L757)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L757)->header.destroyed=0;
#endif

        L755 = ((STR) L757);
        L755->asize = L756;
        r36 = L755;
        SARR((STR)r36,0,create_c35); 
        ;
        ret_val315 = r36;
        ret_val314 = ret_val315;
        s35 = ret_val314;
        ret_val313 = STR_ap1077157958(plus_self228, s35, TRUE);
        res = ret_val313;
       }
      }
      else {
       plus_self229 = res;
       plus_arg36 = '0';
       str_self38 = plus_arg36;
       create_self36 = ((STR) NULL);
       create_c36 = str_self38;
       L761 = 1;
       L763=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L761)*sizeof(CHAR);
       L762=ZALLOC_LEAF_BIG(L763);
       if (L762==NULL) FATAL("Unable to allocate more memory");
       memset(L762,0,L763);
       ((OB)L762)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L762)->header.destroyed=0;
#endif

       L760 = ((STR) L762);
       L760->asize = L761;
       r37 = L760;
       SARR((STR)r37,0,create_c36); 
       ;
       ret_val318 = r37;
       ret_val317 = ret_val318;
       s36 = ret_val317;
       ret_val316 = STR_ap1077157958(plus_self229, s36, TRUE);
       res = ret_val316;
      }
      break;
     case '.': 
      arg_name2 = ((STR) &name1);
      L7651_=INTPLUS(i,1); 
      i = L7651_;
      while (1) {
       L7671_=ARR((STR)r,i); 
       if (CHAR_i1942379470(L7671_)) {
        L766 = TRUE;
       } else {
        L7681_=ARR((STR)r,i); 
        L769 = L7681_;
        L7701_=L769=='_'; 
        L766 = L7701_;
       }
       if (L766) {
       }
       else {
        goto after_loop5;
       }
       plus_self230 = arg_name2;
       L7721_=ARR((STR)r,i); 
       plus_arg37 = L7721_;
       str_self39 = plus_arg37;
       create_self37 = ((STR) NULL);
       create_c37 = str_self39;
       L774 = 1;
       L776=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L774)*sizeof(CHAR);
       L775=ZALLOC_LEAF_BIG(L776);
       if (L775==NULL) FATAL("Unable to allocate more memory");
       memset(L775,0,L776);
       ((OB)L775)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L775)->header.destroyed=0;
#endif

       L773 = ((STR) L775);
       L773->asize = L774;
       r38 = L773;
       SARR((STR)r38,0,create_c37); 
       ;
       ret_val321 = r38;
       ret_val320 = ret_val321;
       s37 = ret_val320;
       ret_val319 = STR_ap1077157958(plus_self230, s37, TRUE);
       arg_name2 = ret_val319;
       L7781_=INTPLUS(i,1); 
       i = L7781_;
      }
      after_loop5: ;
      L7791_=INTMINUS(i,1); 
      i = L7791_;
      done11 = FALSE;
      argi2 = IDENT_1150413730(IDENT_zero, arg_name2);
      {
       struct ELT_TBL_eltbrELT_frame_struct other7_0;
ELT_TBL_eltbrELT_frame noname3 = &other7_0;
       noname3->state = 0;
       while (1) {
        if (noname3->state == 0) {
         L781 = ATTR(fun11,tp);
         L782=(*dTP_implrIMPL[TAG(L781)])(L781);
         L91 = ATTR(L782,elts);
         noname3->self = L91;
        }
        L780 = ELT_TBL_eltbrELT(noname3);
        if (noname3->state == -1) {
         goto after_loop6;
        }
        elt3 = L780;
        if (ELT_is380037919(elt3)) {
         L785 = TRUE;
        } else {
         L785 = ELT_is1936066670(elt3);
        }
        if (L785) {
         L784 = TRUE;
        } else {
         L784 = ELT_is1269015106(elt3);
        }
        if (L784) {
         name_self2 = elt3;
         ret_val322 = ATTR(ATTR(name_self2,sig1),name1);
         is_eq_self2 = ret_val322;
         is_eq_i2 = argi2;
         ret_val323 = (is_eq_self2.str==is_eq_i2.str);
         L783 = ret_val323;
        } else {
         L783 = FALSE;
        }
        if (L783) {
         ret_self1 = elt3;
         ret_val324 = ATTR(ATTR(ret_self1,sig1),ret);
         L786 = ret_val324;
         if ((*dTP_is1334578382[TAG(L786)])(L786)) {
          ret_self2 = elt3;
          ret_val325 = ATTR(ATTR(ret_self2,sig1),ret);
          L787 = ret_val325;
          if ((*dTP_is418055720[TAG(L787)])(L787)) {
           plus_self231 = res;
           plus_arg38 = '1';
           str_self40 = plus_arg38;
           create_self38 = ((STR) NULL);
           create_c38 = str_self40;
           L789 = 1;
           L792=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L789)*sizeof(CHAR);
           L790=ZALLOC_LEAF_BIG(L792);
           if (L790==NULL) FATAL("Unable to allocate more memory");
           memset(L790,0,L792);
           ((OB)L790)->header.tag=STR_tag;
#ifdef DESTROY_CHK

             ((OB)L790)->header.destroyed=0;
#endif

           L788 = ((STR) L790);
           L788->asize = L789;
           r39 = L788;
           SARR((STR)r39,0,create_c38); 
           ;
           ret_val328 = r39;
           ret_val327 = ret_val328;
           s38 = ret_val327;
           ret_val326 = STR_ap1077157958(plus_self231, s38, TRUE);
           res = ret_val326;
          }
          else {
           plus_self232 = res;
           plus_arg39 = '2';
           str_self41 = plus_arg39;
           create_self39 = ((STR) NULL);
           create_c39 = str_self41;
           L795 = 1;
           L797=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L795)*sizeof(CHAR);
           L796=ZALLOC_LEAF_BIG(L797);
           if (L796==NULL) FATAL("Unable to allocate more memory");
           memset(L796,0,L797);
           ((OB)L796)->header.tag=STR_tag;
#ifdef DESTROY_CHK

             ((OB)L796)->header.destroyed=0;
#endif

           L794 = ((STR) L796);
           L794->asize = L795;
           r40 = L794;
           SARR((STR)r40,0,create_c39); 
           ;
           ret_val331 = r40;
           ret_val330 = ret_val331;
           s39 = ret_val330;
           ret_val329 = STR_ap1077157958(plus_self232, s39, TRUE);
           res = ret_val329;
          }
         }
         else {
          plus_self233 = res;
          plus_arg40 = '0';
          str_self42 = plus_arg40;
          create_self40 = ((STR) NULL);
          create_c40 = str_self42;
          L800 = 1;
          L802=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L800)*sizeof(CHAR);
          L801=ZALLOC_LEAF_BIG(L802);
          if (L801==NULL) FATAL("Unable to allocate more memory");
          memset(L801,0,L802);
          ((OB)L801)->header.tag=STR_tag;
#ifdef DESTROY_CHK

            ((OB)L801)->header.destroyed=0;
#endif

          L799 = ((STR) L801);
          L799->asize = L800;
          r41 = L799;
          SARR((STR)r41,0,create_c40); 
          ;
          ret_val334 = r41;
          ret_val333 = ret_val334;
          s40 = ret_val333;
          ret_val332 = STR_ap1077157958(plus_self233, s40, TRUE);
          res = ret_val332;
         }
         done11 = TRUE;
         goto after_loop6;
        }
       }
      }
      after_loop6: ;
      L8041_=!(done11); 
      if (L8041_) {
       barf_self54 = self;
       plus_self234 = ((STR) &unknwo513321933);
       plus_sarg193 = arg_name2;
       ret_val335 = STR_ap2004550586(plus_self234, plus_sarg193);
       plus_self235 = ret_val335;
       plus_sarg194 = ((STR) &inCONFIGmacrofor);
       ret_val336 = STR_ap2004550586(plus_self235, plus_sarg194);
       plus_self236 = ret_val336;
       plus_sarg195 = ATTR(fun11,str);
       ret_val337 = STR_ap2004550586(plus_self236, plus_sarg195);
       plus_self237 = ret_val337;
       plus_sarg196 = ((STR) &name52);
       ret_val338 = STR_ap2004550586(plus_self237, plus_sarg196);
       plus_self238 = ret_val338;
       plus_sarg197 = r;
       ret_val339 = STR_ap2004550586(plus_self238, plus_sarg197);
       plus_self239 = ret_val339;
       plus_sarg198 = ((STR) &name26);
       ret_val340 = STR_ap2004550586(plus_self239, plus_sarg198);
       barf_msg54 = ret_val340;
       barf_self55 = ATTR(barf_self54,prog);
       barf_msg55 = barf_msg54;
       barf_at_self27 = barf_self55;
       barf_at_msg27 = barf_msg55;
       barf_at_at27 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self27, barf_at_at27);
       plus_self240 = ((STR) &Intern28965746);
       plus_sarg199 = barf_at_msg27;
       ret_val341 = STR_ap2004550586(plus_self240, plus_sarg199);
       PROG_err_STR(barf_at_self27, ret_val341);
       exit_self27 = ((UNIX) NULL);
       exit_code27 = 1;
       exit(exit_code27);
      }
      break;
     case 'r': 
      if ((ATTR(fun11,ret)==((dTP) NULL))) {
       barf_self56 = self;
       plus_self241 = ((STR) &risund16704638011);
       plus_sarg200 = r;
       ret_val342 = STR_ap2004550586(plus_self241, plus_sarg200);
       plus_self242 = ret_val342;
       plus_sarg201 = ((STR) &inCONFIGfor);
       ret_val343 = STR_ap2004550586(plus_self242, plus_sarg201);
       plus_self243 = ret_val343;
       plus_sarg202 = ATTR(fun11,str);
       ret_val344 = STR_ap2004550586(plus_self243, plus_sarg202);
       barf_msg56 = ret_val344;
       barf_self57 = ATTR(barf_self56,prog);
       barf_msg57 = barf_msg56;
       barf_at_self28 = barf_self57;
       barf_at_msg28 = barf_msg57;
       barf_at_at28 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self28, barf_at_at28);
       plus_self244 = ((STR) &Intern28965746);
       plus_sarg203 = barf_at_msg28;
       ret_val345 = STR_ap2004550586(plus_self244, plus_sarg203);
       PROG_err_STR(barf_at_self28, ret_val345);
       exit_self28 = ((UNIX) NULL);
       exit_code28 = 1;
       exit(exit_code28);
      }
      L805 = ATTR(fun11,ret);
      if ((*dTP_is1334578382[TAG(L805)])(L805)) {
       L806 = ATTR(fun11,ret);
       if ((*dTP_is418055720[TAG(L806)])(L806)) {
        plus_self245 = res;
        plus_arg41 = '1';
        str_self43 = plus_arg41;
        create_self41 = ((STR) NULL);
        create_c41 = str_self43;
        L808 = 1;
        L810=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L808)*sizeof(CHAR);
        L809=ZALLOC_LEAF_BIG(L810);
        if (L809==NULL) FATAL("Unable to allocate more memory");
        memset(L809,0,L810);
        ((OB)L809)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L809)->header.destroyed=0;
#endif

        L807 = ((STR) L809);
        L807->asize = L808;
        r42 = L807;
        SARR((STR)r42,0,create_c41); 
        ;
        ret_val348 = r42;
        ret_val347 = ret_val348;
        s41 = ret_val347;
        ret_val346 = STR_ap1077157958(plus_self245, s41, TRUE);
        res = ret_val346;
       }
       else {
        plus_self246 = res;
        plus_arg42 = '2';
        str_self44 = plus_arg42;
        create_self42 = ((STR) NULL);
        create_c42 = str_self44;
        L813 = 1;
        L815=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L813)*sizeof(CHAR);
        L814=ZALLOC_LEAF_BIG(L815);
        if (L814==NULL) FATAL("Unable to allocate more memory");
        memset(L814,0,L815);
        ((OB)L814)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L814)->header.destroyed=0;
#endif

        L812 = ((STR) L814);
        L812->asize = L813;
        r43 = L812;
        SARR((STR)r43,0,create_c42); 
        ;
        ret_val351 = r43;
        ret_val350 = ret_val351;
        s42 = ret_val350;
        ret_val349 = STR_ap1077157958(plus_self246, s42, TRUE);
        res = ret_val349;
       }
      }
      else {
       plus_self247 = res;
       plus_arg43 = '0';
       str_self45 = plus_arg43;
       create_self43 = ((STR) NULL);
       create_c43 = str_self45;
       L818 = 1;
       L820=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L818)*sizeof(CHAR);
       L819=ZALLOC_LEAF_BIG(L820);
       if (L819==NULL) FATAL("Unable to allocate more memory");
       memset(L819,0,L820);
       ((OB)L819)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L819)->header.destroyed=0;
#endif

       L817 = ((STR) L819);
       L817->asize = L818;
       r44 = L817;
       SARR((STR)r44,0,create_c43); 
       ;
       ret_val354 = r44;
       ret_val353 = ret_val354;
       s43 = ret_val353;
       ret_val352 = STR_ap1077157958(plus_self247, s43, TRUE);
       res = ret_val352;
      }
      break;
     case 'P': 
      L8231_=INTPLUS(i,1); 
      i = L8231_;
      tparams1 = ATTR(fun11,src_tparams);
      L8251_=ARR((STR)r,i); 
      is_lt_self2 = L8251_;
      is_lt_c2 = '0';
      L8261_=(INT)((unsigned char)is_lt_self2); 
      L827 = L8261_;
      L8281_=(INT)((unsigned char)is_lt_c2); 
      L829 = L8281_;
      L8301_=(L827)<(L829); 
      ret_val355 = L8301_;
      if (ret_val355) {
       L824 = TRUE;
      } else {
       is_lt_self3 = '9';
       L8311_=ARR((STR)r,i); 
       is_lt_c3 = L8311_;
       L8321_=(INT)((unsigned char)is_lt_self3); 
       L833 = L8321_;
       L8341_=(INT)((unsigned char)is_lt_c3); 
       L835 = L8341_;
       L8361_=(L833)<(L835); 
       ret_val356 = L8361_;
       L824 = ret_val356;
      }
      if (L824) {
       barf_self58 = self;
       plus_self248 = ((STR) &Pmustb231175151);
       L8371_=ARR((STR)r,i); 
       plus_arg44 = L8371_;
       str_self46 = plus_arg44;
       create_self44 = ((STR) NULL);
       create_c44 = str_self46;
       L839 = 1;
       L842=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L839)*sizeof(CHAR);
       L840=ZALLOC_LEAF_BIG(L842);
       if (L840==NULL) FATAL("Unable to allocate more memory");
       memset(L840,0,L842);
       ((OB)L840)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L840)->header.destroyed=0;
#endif

       L838 = ((STR) L840);
       L838->asize = L839;
       r45 = L838;
       SARR((STR)r45,0,create_c44); 
       ;
       ret_val359 = r45;
       ret_val358 = ret_val359;
       s44 = ret_val358;
       ret_val357 = STR_ap1077157958(plus_self248, s44, TRUE);
       plus_self249 = ret_val357;
       plus_sarg204 = ((STR) &name9);
       ret_val360 = STR_ap2004550586(plus_self249, plus_sarg204);
       barf_msg58 = ret_val360;
       barf_self59 = ATTR(barf_self58,prog);
       barf_msg59 = barf_msg58;
       barf_at_self29 = barf_self59;
       barf_at_msg29 = barf_msg59;
       barf_at_at29 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self29, barf_at_at29);
       plus_self250 = ((STR) &Intern28965746);
       plus_sarg205 = barf_at_msg29;
       ret_val361 = STR_ap2004550586(plus_self250, plus_sarg205);
       PROG_err_STR(barf_at_self29, ret_val361);
       exit_self29 = ((UNIX) NULL);
       exit_code29 = 1;
       exit(exit_code29);
      }
      L8441_=ARR((STR)r,i); 
      L845 = L8441_;
      L8461_=(INT)((unsigned char)L845); 
      L847 = L8461_;
      L8481_=(INT)((unsigned char)'0'); 
      L849 = L8481_;
      L8501_=INTMINUS(L847,L849); 
      L851 = L8501_;
      L8521_=(tparams1)==NULL?0:ASIZE((ARRAYdTP)tparams1); 
      L853 = L8521_;
      L8541_=(L851)<(L853); 
      L855 = L8541_;
      L8561_=!(L855); 
      if (L8561_) {
       barf_self60 = self;
       plus_self251 = ((STR) &P1);
       L8571_=ARR((STR)r,i); 
       plus_arg45 = L8571_;
       str_self47 = plus_arg45;
       create_self45 = ((STR) NULL);
       create_c45 = str_self47;
       L859 = 1;
       L862=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L859)*sizeof(CHAR);
       L860=ZALLOC_LEAF_BIG(L862);
       if (L860==NULL) FATAL("Unable to allocate more memory");
       memset(L860,0,L862);
       ((OB)L860)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L860)->header.destroyed=0;
#endif

       L858 = ((STR) L860);
       L858->asize = L859;
       r46 = L858;
       SARR((STR)r46,0,create_c45); 
       ;
       ret_val364 = r46;
       ret_val363 = ret_val364;
       s45 = ret_val363;
       ret_val362 = STR_ap1077157958(plus_self251, s45, TRUE);
       plus_self252 = ret_val362;
       plus_sarg206 = ((STR) &outofboundsmacro);
       ret_val365 = STR_ap2004550586(plus_self252, plus_sarg206);
       plus_self253 = ret_val365;
       plus_sarg207 = r;
       ret_val366 = STR_ap2004550586(plus_self253, plus_sarg207);
       plus_self254 = ret_val366;
       plus_sarg208 = ((STR) &sig2);
       ret_val367 = STR_ap2004550586(plus_self254, plus_sarg208);
       plus_self255 = ret_val367;
       plus_sarg209 = ATTR(fun11,str);
       ret_val368 = STR_ap2004550586(plus_self255, plus_sarg209);
       plus_self256 = ret_val368;
       plus_sarg210 = ((STR) &name50);
       ret_val369 = STR_ap2004550586(plus_self256, plus_sarg210);
       barf_msg60 = ret_val369;
       barf_self61 = ATTR(barf_self60,prog);
       barf_msg61 = barf_msg60;
       barf_at_self30 = barf_self61;
       barf_at_msg30 = barf_msg61;
       barf_at_at30 = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self30, barf_at_at30);
       plus_self257 = ((STR) &Intern28965746);
       plus_sarg211 = barf_at_msg30;
       ret_val370 = STR_ap2004550586(plus_self257, plus_sarg211);
       PROG_err_STR(barf_at_self30, ret_val370);
       exit_self30 = ((UNIX) NULL);
       exit_code30 = 1;
       exit(exit_code30);
      }
      L8641_=ARR((STR)r,i); 
      L865 = L8641_;
      L8661_=(INT)((unsigned char)L865); 
      L867 = L8661_;
      L8681_=(INT)((unsigned char)'0'); 
      L869 = L8681_;
      L8701_=INTMINUS(L867,L869); 
      L871 = L8701_;
      L8721_=(dTP)ARR((ARRAYdTP)tparams1,L871); 
      L873 = L8721_;
      if ((*dTP_is1334578382[TAG(L873)])(L873)) {
       L8741_=ARR((STR)r,i); 
       L875 = L8741_;
       L8761_=(INT)((unsigned char)L875); 
       L877 = L8761_;
       L8781_=(INT)((unsigned char)'0'); 
       L879 = L8781_;
       L8801_=INTMINUS(L877,L879); 
       L882 = L8801_;
       L8831_=(dTP)ARR((ARRAYdTP)tparams1,L882); 
       L884 = L8831_;
       if ((*dTP_is418055720[TAG(L884)])(L884)) {
        plus_self258 = res;
        plus_arg46 = '1';
        str_self48 = plus_arg46;
        create_self46 = ((STR) NULL);
        create_c46 = str_self48;
        L886 = 1;
        L888=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L886)*sizeof(CHAR);
        L887=ZALLOC_LEAF_BIG(L888);
        if (L887==NULL) FATAL("Unable to allocate more memory");
        memset(L887,0,L888);
        ((OB)L887)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L887)->header.destroyed=0;
#endif

        L885 = ((STR) L887);
        L885->asize = L886;
        r47 = L885;
        SARR((STR)r47,0,create_c46); 
        ;
        ret_val373 = r47;
        ret_val372 = ret_val373;
        s46 = ret_val372;
        ret_val371 = STR_ap1077157958(plus_self258, s46, TRUE);
        res = ret_val371;
       }
       else {
        plus_self259 = res;
        plus_arg47 = '2';
        str_self49 = plus_arg47;
        create_self47 = ((STR) NULL);
        create_c47 = str_self49;
        L891 = 1;
        L893=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L891)*sizeof(CHAR);
        L892=ZALLOC_LEAF_BIG(L893);
        if (L892==NULL) FATAL("Unable to allocate more memory");
        memset(L892,0,L893);
        ((OB)L892)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L892)->header.destroyed=0;
#endif

        L890 = ((STR) L892);
        L890->asize = L891;
        r48 = L890;
        SARR((STR)r48,0,create_c47); 
        ;
        ret_val376 = r48;
        ret_val375 = ret_val376;
        s47 = ret_val375;
        ret_val374 = STR_ap1077157958(plus_self259, s47, TRUE);
        res = ret_val374;
       }
      }
      else {
       plus_self260 = res;
       plus_arg48 = '0';
       str_self50 = plus_arg48;
       create_self48 = ((STR) NULL);
       create_c48 = str_self50;
       L896 = 1;
       L898=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L896)*sizeof(CHAR);
       L897=ZALLOC_LEAF_BIG(L898);
       if (L897==NULL) FATAL("Unable to allocate more memory");
       memset(L897,0,L898);
       ((OB)L897)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L897)->header.destroyed=0;
#endif

       L895 = ((STR) L897);
       L895->asize = L896;
       r49 = L895;
       SARR((STR)r49,0,create_c48); 
       ;
       ret_val379 = r49;
       ret_val378 = ret_val379;
       s48 = ret_val378;
       ret_val377 = STR_ap1077157958(plus_self260, s48, TRUE);
       res = ret_val377;
      }
      break;
     case '%': 
      plus_self261 = res;
      plus_arg49 = '%';
      str_self51 = plus_arg49;
      create_self49 = ((STR) NULL);
      create_c49 = str_self51;
      L902 = 1;
      L904=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L902)*sizeof(CHAR);
      L903=ZALLOC_LEAF_BIG(L904);
      if (L903==NULL) FATAL("Unable to allocate more memory");
      memset(L903,0,L904);
      ((OB)L903)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L903)->header.destroyed=0;
#endif

      L900 = ((STR) L903);
      L900->asize = L902;
      r50 = L900;
      SARR((STR)r50,0,create_c49); 
      ;
      ret_val382 = r50;
      ret_val381 = ret_val382;
      s49 = ret_val381;
      ret_val380 = STR_ap1077157958(plus_self261, s49, TRUE);
      res = ret_val380;
      break;
     default: ;
      barf_self62 = self;
      plus_self262 = ((STR) &unknown2);
      L9061_=ARR((STR)r,i); 
      plus_arg50 = L9061_;
      str_self52 = plus_arg50;
      create_self50 = ((STR) NULL);
      create_c50 = str_self52;
      L908 = 1;
      L910=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L908)*sizeof(CHAR);
      L909=ZALLOC_LEAF_BIG(L910);
      if (L909==NULL) FATAL("Unable to allocate more memory");
      memset(L909,0,L910);
      ((OB)L909)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L909)->header.destroyed=0;
#endif

      L907 = ((STR) L909);
      L907->asize = L908;
      r51 = L907;
      SARR((STR)r51,0,create_c50); 
      ;
      ret_val385 = r51;
      ret_val384 = ret_val385;
      s50 = ret_val384;
      ret_val383 = STR_ap1077157958(plus_self262, s50, TRUE);
      plus_self263 = ret_val383;
      plus_sarg212 = ((STR) &macroinCONFIGfor);
      ret_val386 = STR_ap2004550586(plus_self263, plus_sarg212);
      plus_self264 = ret_val386;
      plus_sarg213 = ATTR(fun11,str);
      ret_val387 = STR_ap2004550586(plus_self264, plus_sarg213);
      plus_self265 = ret_val387;
      plus_sarg214 = ((STR) &name52);
      ret_val388 = STR_ap2004550586(plus_self265, plus_sarg214);
      plus_self266 = ret_val388;
      plus_sarg215 = r;
      ret_val389 = STR_ap2004550586(plus_self266, plus_sarg215);
      plus_self267 = ret_val389;
      plus_sarg216 = ((STR) &name26);
      ret_val390 = STR_ap2004550586(plus_self267, plus_sarg216);
      barf_msg62 = ret_val390;
      barf_self63 = ATTR(barf_self62,prog);
      barf_msg63 = barf_msg62;
      barf_at_self31 = barf_self63;
      barf_at_msg31 = barf_msg63;
      barf_at_at31 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self31, barf_at_at31);
      plus_self268 = ((STR) &Intern28965746);
      plus_sarg217 = barf_at_msg31;
      ret_val391 = STR_ap2004550586(plus_self268, plus_sarg217);
      PROG_err_STR(barf_at_self31, ret_val391);
      exit_self31 = ((UNIX) NULL);
      exit_code31 = 1;
      exit(exit_code31);
    }
    break;
   default: ;
    plus_self269 = res;
    L9121_=ARR((STR)r,i); 
    plus_arg51 = L9121_;
    str_self53 = plus_arg51;
    create_self51 = ((STR) NULL);
    create_c51 = str_self53;
    L914 = 1;
    L916=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L914)*sizeof(CHAR);
    L915=ZALLOC_LEAF_BIG(L916);
    if (L915==NULL) FATAL("Unable to allocate more memory");
    memset(L915,0,L916);
    ((OB)L915)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L915)->header.destroyed=0;
#endif

    L913 = ((STR) L915);
    L913->asize = L914;
    r52 = L913;
    SARR((STR)r52,0,create_c51); 
    ;
    ret_val394 = r52;
    ret_val393 = ret_val394;
    s51 = ret_val393;
    ret_val392 = STR_ap1077157958(plus_self269, s51, TRUE);
    res = ret_val392;
  }
  L9181_=INTPLUS(i,1); 
  i = L9181_;
 }
 after_loop: ;
 if ((res==((STR) NULL))) {
  res = ((STR) &name1);
 }
 if (has_near) {
  L9201_=!(near_failed); 
  L919 = L9201_;
 } else {
  L919 = FALSE;
 }
 if (L919) {
  L921 = ATTR(ATTR(self,prog),stat1);
  (*dSTAT_incr_STR[TAG(L921)])(L921, ((STR) &Oofbui2034740200));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e236145586(CGEN self, AM_LOCAL_EXPR arg) {
 STR ret_val;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val3;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns1;
 STR ret_val4;
 BOOL L1;
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
 BOOL L12;
 BOOL L131_;
 extern STR frame1;
 is_iter_self = ATTR(self,current_sig);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L3 = (is_iter_self1.str==((STR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = is_iter_self1.str;
  L6 = STR_sizerINT(is_iter_self1.str);
  L71_=INTMINUS(L6,1); 
  L8 = L71_;
  L91_=ARR((STR)L5,L8); 
  L10 = L91_;
  L111_=L10=='!'; 
  L2 = L111_;
 } else {
  L2 = FALSE;
 }
 ret_val2 = L2;
 ret_val1 = ret_val2;
 if (ret_val1) {
  L12 = ATTR(arg,not_in_frame);
  L131_=!(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  mang_self = self;
  mang_ob = ((OB) arg);
  mang_ns = ((OB) ATTR(self,current_sig));
  ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
  ret_val = STR_ap2004550586(((STR) &frame1), ret_val3);
  return ret_val;
 }
 else {
  mang_self1 = self;
  mang_ob1 = ((OB) arg);
  mang_ns1 = ((OB) ATTR(self,current_sig));
  ret_val4 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns1);
  ret_val = ret_val4;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e414796278(CGEN self, AM_CALL_ARG a, ARG sa1, BOOL need_local, BOOL force_reference) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR se = ((STR) NULL);
 STR tmp1;
 STR tmp2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 CGEN ndefer_self3;
 STR ndefer_s3;
 BOOL L1;
 dMODE L2;
 dMODE L3;
 BOOL L4;
 BOOL L5;
 dTP L6;
 dAM_EXPR L7;
 dTP L8;
 extern STR name91;
 extern STR name16;
 dAM_EXPR L9;
 extern STR name91;
 extern STR name16;
 dAM_EXPR L10;
 extern STR name36;
 extern STR name16;
 extern STR name92;
 extern STR name92;
 dAM_EXPR L11;
 extern STR name36;
 extern STR name16;
 se = CGEN_e1639504572(self, ATTR(a,expr));
 L2 = ATTR(a,mode1);
 if ((*dMODE_814247358[TAG(L2)])(L2, ((dMODE) MODES_out_mode))) {
  L1 = TRUE;
 } else {
  L3 = ATTR(a,mode1);
  L1 = (*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_inout_mode));
 }
 if (L1) {
  L6 = ATTR(sa1,tp);
  if ((*dTP_is1334578382[TAG(L6)])(L6)) {
   L5 = SYSOBEQ(((OB) ATTR(a,mode1)),((OB) MODES_out_mode));
  } else {
   L5 = FALSE;
  }
  if (L5) {
   L7 = ATTR(a,expr);
   L8 = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
   L4 = (*dTP_is1811059018[TAG(L8)])(L8);
  } else {
   L4 = FALSE;
  }
  if (L4) {
   tmp1 = CGEN_d1425664957(self, ATTR(sa1,tp));
   res = CGEN_d2002278524(self, ATTR(sa1,tp));
   ndefer_self = self;
   ndefer_s = STR_ap1417971546(res, ((STR) &name91), tmp1, ((STR) &name16));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  }
  else {
   L9 = ATTR(a,expr);
   res = CGEN_d2002278524(self, (*dAM_EXPR_tprdTP[TAG(L9)])(L9));
   ndefer_self1 = self;
   ndefer_s1 = STR_ap1417971546(res, ((STR) &name91), se, ((STR) &name16));
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
 }
 else {
  if (force_reference) {
   if (need_local) {
    L10 = ATTR(a,expr);
    tmp2 = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L10)])(L10));
    ndefer_self2 = self;
    plus_self = ((STR) &name36);
    plus_sarg = se;
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = ((STR) &name16);
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    ndefer_s2 = STR_ap2004550586(tmp2, ret_val2);
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
    plus_self2 = ((STR) &name92);
    plus_sarg2 = tmp2;
    ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
    res = ret_val3;
   }
   else {
    plus_self3 = ((STR) &name92);
    plus_sarg3 = se;
    ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
    res = ret_val4;
   }
  }
  else {
   if (need_local) {
    L11 = ATTR(a,expr);
    res = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L11)])(L11));
    ndefer_self3 = self;
    ndefer_s3 = STR_ap1417971546(res, ((STR) &name36), se, ((STR) &name16));
    CGEN_d695831906(ndefer_self3);
    SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   }
   else {
    res = se;
   }
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_v205291382(CGEN self, dTP tp, STR e1, STR e2) {
 STR ret_val;
 AM_OB_DEF aod;
 BOOL after_first = BOOL_zero;
 STR res;
 FMAPIDENTdTP aod_at;
 ARRAYIDENT aod_idents;
 IDENT p_key = IDENT_zero;
 ARRAYIDENT L11;
 dTP p_tp;
 STR key1;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 INT L41 = INT_zero;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns;
 STR ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val7;
 STR plus_self3;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val8;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val10;
 STR r1;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val11;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val12;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val14;
 STR plus_self8;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val15;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val17;
 STR r2;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val18;
 STR plus_self10;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val19;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val20;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val21;
 STR r3;
 STR plus_self11;
 STR plus_sarg7;
 STR ret_val22;
 STR plus_self12;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val23;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val24;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val25;
 STR r4;
 STR plus_self13;
 STR plus_sarg8;
 STR ret_val26;
 STR plus_self14;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val27;
 STR s5;
 CHAR str_self5 = CHAR_zero;
 STR ret_val28;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val29;
 STR r5;
 STR plus_self15;
 STR plus_sarg9;
 STR ret_val30;
 STR plus_self16;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self17;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val32;
 STR s6;
 CHAR str_self6 = CHAR_zero;
 STR ret_val33;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val34;
 STR r6;
 STR plus_self18;
 STR plus_sarg11;
 STR ret_val35;
 STR plus_self19;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val36;
 STR s7;
 CHAR str_self7 = CHAR_zero;
 STR ret_val37;
 STR create_self7;
 CHAR create_c7 = CHAR_zero;
 STR ret_val38;
 STR r7;
 STR plus_self20;
 STR plus_sarg12;
 STR ret_val39;
 STR plus_self21;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val40;
 STR s8;
 CHAR str_self8 = CHAR_zero;
 STR ret_val41;
 STR create_self8;
 CHAR create_c8 = CHAR_zero;
 STR ret_val42;
 STR r8;
 STR plus_self22;
 STR plus_sarg13;
 STR ret_val43;
 STR plus_self23;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val44;
 STR s9;
 CHAR str_self9 = CHAR_zero;
 STR ret_val45;
 STR create_self9;
 CHAR create_c9 = CHAR_zero;
 STR ret_val46;
 STR r9;
 STR plus_self24;
 STR plus_sarg14;
 STR ret_val47;
 STR plus_self25;
 STR plus_sarg15;
 STR ret_val48;
 STR plus_self26;
 INT plus_arg10 = INT_zero;
 STR ret_val49;
 STR s10;
 INT str_self10 = INT_zero;
 STR ret_val50;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val51;
 FSTR str_self11;
 STR ret_val52;
 STR plus_self27;
 STR plus_sarg16;
 STR ret_val53;
 STR plus_self28;
 STR plus_sarg17;
 STR ret_val54;
 STR plus_self29;
 INT plus_arg11 = INT_zero;
 STR ret_val55;
 STR s11;
 INT str_self12 = INT_zero;
 STR ret_val56;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val57;
 FSTR str_self13;
 STR ret_val58;
 STR plus_self30;
 STR plus_sarg18;
 STR ret_val59;
 STR plus_self31;
 STR plus_sarg19;
 STR ret_val60;
 STR plus_self32;
 STR plus_sarg20;
 STR ret_val61;
 STR plus_self33;
 STR plus_sarg21;
 STR ret_val62;
 STR plus_self34;
 STR plus_sarg22;
 STR ret_val63;
 STR plus_self35;
 INT plus_arg12 = INT_zero;
 STR ret_val64;
 STR s12;
 INT str_self14 = INT_zero;
 STR ret_val65;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val66;
 FSTR str_self15;
 STR ret_val67;
 STR plus_self36;
 STR plus_sarg23;
 STR ret_val68;
 STR plus_self37;
 STR plus_sarg24;
 STR ret_val69;
 STR plus_self38;
 STR plus_sarg25;
 STR ret_val70;
 STR plus_self39;
 INT plus_arg13 = INT_zero;
 STR ret_val71;
 STR s13;
 INT str_self16 = INT_zero;
 STR ret_val72;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val73;
 FSTR str_self17;
 STR ret_val74;
 STR plus_self40;
 STR plus_sarg26;
 STR ret_val75;
 dTP L5;
 extern STR name24;
 extern STR S_IS_EQ;
 extern STR name3;
 extern STR name87;
 extern STR S11;
 extern STR name1;
 INT L61_br;
 IDENT aL61_;
 IDENT L7;
 OB L8;
 OB L9;
 extern STR name88;
 dTP L10;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
 extern STR F_SYSOBEQ1;
 STR L23;
 INT L24;
 OB L25;
 INT L26;
 STR L28;
 INT L29;
 OB L30;
 INT L32;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 STR L39;
 INT L40;
 OB L42;
 INT L43;
 extern STR SYSOBEQ1;
 STR L45;
 INT L46;
 OB L47;
 INT L48;
 STR L50;
 INT L51;
 OB L52;
 INT L53;
 STR L55;
 INT L56;
 OB L57;
 INT L58;
 STR L60;
 INT L62;
 OB L63;
 INT L64;
 INT L661_;
 BOOL L67;
 BOOL L681_;
 INT L691_,L691_m;
 INT L70;
 extern STR name88;
 dTP L71;
 extern STR arr_part1;
 BOOL L72;
 BOOL L731_;
 extern STR name7;
 extern STR arr_part1;
 BOOL L74;
 BOOL L751_;
 extern STR name7;
 extern STR name24;
 extern STR arr_part1;
 BOOL L76;
 BOOL L771_;
 extern STR name89;
 extern STR arr_part1;
 BOOL L78;
 BOOL L791_;
 extern STR name90;
 L5 = tp;
 if ((*dTP_is914808564[TAG(L5)])(L5)) {
  mang_self = self;
  mang_ob = ((OB) tp);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  ret_val = STR_ap1182453786(((STR) &name24), ret_val1, ((STR) &S_IS_EQ), e1, ((STR) &name3), e2, ((STR) &name87));
  return ret_val;
 }
 aod = CGEN_a1221397173(self, tp);
 after_first = FALSE;
 if ((ATTR(aod,at)==((FMAPIDENTdTP) NULL))) {
  ret_val = ((STR) &S11);
  return ret_val;
 }
 res = ((STR) &name1);
 aod_at = ATTR(aod,at);
 aod_idents = ATTR(aod,sorted_at);
 {
  BOOL f_L61_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = aod_idents;
  L61_br=L11==NULL?0:ASIZE((ARRAYIDENT)L11); 
  while (1) {
   if(L41>=L61_br)  goto after_loop; 
   aL61_=ARR((ARRAYIDENT)L11,L41); 
   p_key = aL61_;
   p_tp = FMAPID86862037(aod_at, p_key);
   mang_self1 = self;
   L9=ZALLOC(sizeof(struct IDENT_boxed_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=IDENT_tag;
   L8 = (OB)((IDENT_boxed) L9);
   ((IDENT_boxed) L8)->immutable_part = p_key;
   mang_ob1 = L8;
   mang_ns = ((OB) tp);
   ret_val2 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns);
   key1 = ret_val2;
   if (after_first) {
    plus_self = res;
    plus_sarg = ((STR) &name88);
    ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
    res = ret_val3;
   }
   L10 = p_tp;
   if ((*dTP_is1334578382[TAG(L10)])(L10)) {
    plus_self5 = res;
    plus_self1 = e1;
    plus_arg = '.';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L13 = 1;
    L15=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L13)*sizeof(CHAR);
    L14=ZALLOC_LEAF_BIG(L15);
    if (L14==NULL) FATAL("Unable to allocate more memory");
    memset(L14,0,L15);
    ((OB)L14)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L14)->header.destroyed=0;
#endif

    L12 = ((STR) L14);
    L12->asize = L13;
    r = L12;
    SARR((STR)r,0,create_c); 
    ;
    ret_val6 = r;
    ret_val5 = ret_val6;
    s = ret_val5;
    ret_val4 = STR_ap1077157958(plus_self1, s, TRUE);
    plus_self2 = ret_val4;
    plus_sarg1 = key1;
    ret_val7 = STR_ap2004550586(plus_self2, plus_sarg1);
    plus_self3 = e2;
    plus_arg1 = '.';
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
    L18 = 1;
    L20=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L18)*sizeof(CHAR);
    L19=ZALLOC_LEAF_BIG(L20);
    if (L19==NULL) FATAL("Unable to allocate more memory");
    memset(L19,0,L20);
    ((OB)L19)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L19)->header.destroyed=0;
#endif

    L17 = ((STR) L19);
    L17->asize = L18;
    r1 = L17;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val10 = r1;
    ret_val9 = ret_val10;
    s1 = ret_val9;
    ret_val8 = STR_ap1077157958(plus_self3, s1, TRUE);
    plus_self4 = ret_val8;
    plus_sarg2 = key1;
    ret_val11 = STR_ap2004550586(plus_self4, plus_sarg2);
    plus_sarg3 = CGEN_v205291382(self, p_tp, ret_val7, ret_val11);
    ret_val12 = STR_ap2004550586(plus_self5, plus_sarg3);
    res = ret_val12;
   }
   else {
    if (ATTR(ATTR(self,prog),distributed)) {
     plus_self6 = res;
     plus_sarg4 = ((STR) &F_SYSOBEQ1);
     ret_val13 = STR_ap2004550586(plus_self6, plus_sarg4);
     plus_self7 = ret_val13;
     plus_sarg5 = e1;
     ret_val14 = STR_ap2004550586(plus_self7, plus_sarg5);
     plus_self8 = ret_val14;
     plus_arg2 = '.';
     str_self2 = plus_arg2;
     create_self2 = ((STR) NULL);
     create_c2 = str_self2;
     L24 = 1;
     L26=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L24)*sizeof(CHAR);
     L25=ZALLOC_LEAF_BIG(L26);
     if (L25==NULL) FATAL("Unable to allocate more memory");
     memset(L25,0,L26);
     ((OB)L25)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L25)->header.destroyed=0;
#endif

     L23 = ((STR) L25);
     L23->asize = L24;
     r2 = L23;
     SARR((STR)r2,0,create_c2); 
     ;
     ret_val17 = r2;
     ret_val16 = ret_val17;
     s2 = ret_val16;
     ret_val15 = STR_ap1077157958(plus_self8, s2, TRUE);
     plus_self9 = ret_val15;
     plus_sarg6 = key1;
     ret_val18 = STR_ap2004550586(plus_self9, plus_sarg6);
     plus_self10 = ret_val18;
     plus_arg3 = ',';
     str_self3 = plus_arg3;
     create_self3 = ((STR) NULL);
     create_c3 = str_self3;
     L29 = 1;
     L32=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L29)*sizeof(CHAR);
     L30=ZALLOC_LEAF_BIG(L32);
     if (L30==NULL) FATAL("Unable to allocate more memory");
     memset(L30,0,L32);
     ((OB)L30)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L30)->header.destroyed=0;
#endif

     L28 = ((STR) L30);
     L28->asize = L29;
     r3 = L28;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val21 = r3;
     ret_val20 = ret_val21;
     s3 = ret_val20;
     ret_val19 = STR_ap1077157958(plus_self10, s3, TRUE);
     plus_self11 = ret_val19;
     plus_sarg7 = e2;
     ret_val22 = STR_ap2004550586(plus_self11, plus_sarg7);
     plus_self12 = ret_val22;
     plus_arg4 = '.';
     str_self4 = plus_arg4;
     create_self4 = ((STR) NULL);
     create_c4 = str_self4;
     L35 = 1;
     L37=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L35)*sizeof(CHAR);
     L36=ZALLOC_LEAF_BIG(L37);
     if (L36==NULL) FATAL("Unable to allocate more memory");
     memset(L36,0,L37);
     ((OB)L36)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L36)->header.destroyed=0;
#endif

     L34 = ((STR) L36);
     L34->asize = L35;
     r4 = L34;
     SARR((STR)r4,0,create_c4); 
     ;
     ret_val25 = r4;
     ret_val24 = ret_val25;
     s4 = ret_val24;
     ret_val23 = STR_ap1077157958(plus_self12, s4, TRUE);
     plus_self13 = ret_val23;
     plus_sarg8 = key1;
     ret_val26 = STR_ap2004550586(plus_self13, plus_sarg8);
     plus_self14 = ret_val26;
     plus_arg5 = ')';
     str_self5 = plus_arg5;
     create_self5 = ((STR) NULL);
     create_c5 = str_self5;
     L40 = 1;
     L43=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L40)*sizeof(CHAR);
     L42=ZALLOC_LEAF_BIG(L43);
     if (L42==NULL) FATAL("Unable to allocate more memory");
     memset(L42,0,L43);
     ((OB)L42)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L42)->header.destroyed=0;
#endif

     L39 = ((STR) L42);
     L39->asize = L40;
     r5 = L39;
     SARR((STR)r5,0,create_c5); 
     ;
     ret_val29 = r5;
     ret_val28 = ret_val29;
     s5 = ret_val28;
     ret_val27 = STR_ap1077157958(plus_self14, s5, TRUE);
     res = ret_val27;
    }
    else {
     plus_self15 = res;
     plus_sarg9 = ((STR) &SYSOBEQ1);
     ret_val30 = STR_ap2004550586(plus_self15, plus_sarg9);
     plus_self16 = ret_val30;
     plus_sarg10 = e1;
     ret_val31 = STR_ap2004550586(plus_self16, plus_sarg10);
     plus_self17 = ret_val31;
     plus_arg6 = '.';
     str_self6 = plus_arg6;
     create_self6 = ((STR) NULL);
     create_c6 = str_self6;
     L46 = 1;
     L48=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L46)*sizeof(CHAR);
     L47=ZALLOC_LEAF_BIG(L48);
     if (L47==NULL) FATAL("Unable to allocate more memory");
     memset(L47,0,L48);
     ((OB)L47)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L47)->header.destroyed=0;
#endif

     L45 = ((STR) L47);
     L45->asize = L46;
     r6 = L45;
     SARR((STR)r6,0,create_c6); 
     ;
     ret_val34 = r6;
     ret_val33 = ret_val34;
     s6 = ret_val33;
     ret_val32 = STR_ap1077157958(plus_self17, s6, TRUE);
     plus_self18 = ret_val32;
     plus_sarg11 = key1;
     ret_val35 = STR_ap2004550586(plus_self18, plus_sarg11);
     plus_self19 = ret_val35;
     plus_arg7 = ',';
     str_self7 = plus_arg7;
     create_self7 = ((STR) NULL);
     create_c7 = str_self7;
     L51 = 1;
     L53=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L51)*sizeof(CHAR);
     L52=ZALLOC_LEAF_BIG(L53);
     if (L52==NULL) FATAL("Unable to allocate more memory");
     memset(L52,0,L53);
     ((OB)L52)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L52)->header.destroyed=0;
#endif

     L50 = ((STR) L52);
     L50->asize = L51;
     r7 = L50;
     SARR((STR)r7,0,create_c7); 
     ;
     ret_val38 = r7;
     ret_val37 = ret_val38;
     s7 = ret_val37;
     ret_val36 = STR_ap1077157958(plus_self19, s7, TRUE);
     plus_self20 = ret_val36;
     plus_sarg12 = e2;
     ret_val39 = STR_ap2004550586(plus_self20, plus_sarg12);
     plus_self21 = ret_val39;
     plus_arg8 = '.';
     str_self8 = plus_arg8;
     create_self8 = ((STR) NULL);
     create_c8 = str_self8;
     L56 = 1;
     L58=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L56)*sizeof(CHAR);
     L57=ZALLOC_LEAF_BIG(L58);
     if (L57==NULL) FATAL("Unable to allocate more memory");
     memset(L57,0,L58);
     ((OB)L57)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L57)->header.destroyed=0;
#endif

     L55 = ((STR) L57);
     L55->asize = L56;
     r8 = L55;
     SARR((STR)r8,0,create_c8); 
     ;
     ret_val42 = r8;
     ret_val41 = ret_val42;
     s8 = ret_val41;
     ret_val40 = STR_ap1077157958(plus_self21, s8, TRUE);
     plus_self22 = ret_val40;
     plus_sarg13 = key1;
     ret_val43 = STR_ap2004550586(plus_self22, plus_sarg13);
     plus_self23 = ret_val43;
     plus_arg9 = ')';
     str_self9 = plus_arg9;
     create_self9 = ((STR) NULL);
     create_c9 = str_self9;
     L62 = 1;
     L64=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L62)*sizeof(CHAR);
     L63=ZALLOC_LEAF_BIG(L64);
     if (L63==NULL) FATAL("Unable to allocate more memory");
     memset(L63,0,L64);
     ((OB)L63)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L63)->header.destroyed=0;
#endif

     L60 = ((STR) L63);
     L60->asize = L62;
     r9 = L60;
     SARR((STR)r9,0,create_c9); 
     ;
     ret_val46 = r9;
     ret_val45 = ret_val46;
     s9 = ret_val45;
     ret_val44 = STR_ap1077157958(plus_self23, s9, TRUE);
     res = ret_val44;
    }
   }
   after_first = TRUE;
   L661_=INTPLUS(L41,1); 
   L41 = L661_;
  }
 }
 after_loop: ;
 L67 = (ATTR(aod,arr)==((dTP) NULL));
 L681_=!(L67); 
 if (L681_) {
  {
   BOOL f_L691_ = TRUE;
   L21 = 0;
   L31 = ATTR(aod,asize);
   L691_=L21-1;L691_m=L31; 
   while (1) {
    if((L691_m--)<=0)  goto after_loop1;L691_++; 
    i = L691_;
    if (after_first) {
     plus_self24 = res;
     plus_sarg14 = ((STR) &name88);
     ret_val47 = STR_ap2004550586(plus_self24, plus_sarg14);
     res = ret_val47;
    }
    L71 = ATTR(aod,arr);
    if ((*dTP_is1334578382[TAG(L71)])(L71)) {
     plus_self31 = res;
     plus_self25 = e1;
     plus_sarg15 = ((STR) &arr_part1);
     ret_val48 = STR_ap2004550586(plus_self25, plus_sarg15);
     plus_self26 = ret_val48;
     plus_arg10 = i;
     str_self10 = plus_arg10;
     clear_self = buf1;
     L72 = (clear_self==((FSTR) NULL));
     L731_=!(L72); 
     if (L731_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self10;
     str_in_s = buf1;
     ret_val51 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val51;
     str_self11 = buf1;
     ret_val52 = STR_fr1097270334(((STR) NULL), str_self11);
     ret_val50 = ret_val52;
     s10 = ret_val50;
     ret_val49 = STR_ap1077157958(plus_self26, s10, TRUE);
     plus_self27 = ret_val49;
     plus_sarg16 = ((STR) &name7);
     ret_val53 = STR_ap2004550586(plus_self27, plus_sarg16);
     plus_self28 = e2;
     plus_sarg17 = ((STR) &arr_part1);
     ret_val54 = STR_ap2004550586(plus_self28, plus_sarg17);
     plus_self29 = ret_val54;
     plus_arg11 = i;
     str_self12 = plus_arg11;
     clear_self1 = buf2;
     L74 = (clear_self1==((FSTR) NULL));
     L751_=!(L74); 
     if (L751_) {
      SATTR(clear_self1,loc,0);
     }
     str_in_self1 = str_self12;
     str_in_s1 = buf2;
     ret_val57 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
     buf2 = ret_val57;
     str_self13 = buf2;
     ret_val58 = STR_fr1097270334(((STR) NULL), str_self13);
     ret_val56 = ret_val58;
     s11 = ret_val56;
     ret_val55 = STR_ap1077157958(plus_self29, s11, TRUE);
     plus_self30 = ret_val55;
     plus_sarg18 = ((STR) &name7);
     ret_val59 = STR_ap2004550586(plus_self30, plus_sarg18);
     plus_sarg19 = CGEN_v205291382(self, ATTR(aod,arr), ret_val53, ret_val59);
     ret_val60 = STR_ap2004550586(plus_self31, plus_sarg19);
     res = ret_val60;
    }
    else {
     plus_self32 = res;
     plus_sarg20 = ((STR) &name24);
     ret_val61 = STR_ap2004550586(plus_self32, plus_sarg20);
     plus_self33 = ret_val61;
     plus_sarg21 = e1;
     ret_val62 = STR_ap2004550586(plus_self33, plus_sarg21);
     plus_self34 = ret_val62;
     plus_sarg22 = ((STR) &arr_part1);
     ret_val63 = STR_ap2004550586(plus_self34, plus_sarg22);
     plus_self35 = ret_val63;
     plus_arg12 = i;
     str_self14 = plus_arg12;
     clear_self2 = buf3;
     L76 = (clear_self2==((FSTR) NULL));
     L771_=!(L76); 
     if (L771_) {
      SATTR(clear_self2,loc,0);
     }
     str_in_self2 = str_self14;
     str_in_s2 = buf3;
     ret_val66 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
     buf3 = ret_val66;
     str_self15 = buf3;
     ret_val67 = STR_fr1097270334(((STR) NULL), str_self15);
     ret_val65 = ret_val67;
     s12 = ret_val65;
     ret_val64 = STR_ap1077157958(plus_self35, s12, TRUE);
     plus_self36 = ret_val64;
     plus_sarg23 = ((STR) &name89);
     ret_val68 = STR_ap2004550586(plus_self36, plus_sarg23);
     plus_self37 = ret_val68;
     plus_sarg24 = e2;
     ret_val69 = STR_ap2004550586(plus_self37, plus_sarg24);
     plus_self38 = ret_val69;
     plus_sarg25 = ((STR) &arr_part1);
     ret_val70 = STR_ap2004550586(plus_self38, plus_sarg25);
     plus_self39 = ret_val70;
     plus_arg13 = i;
     str_self16 = plus_arg13;
     clear_self3 = buf4;
     L78 = (clear_self3==((FSTR) NULL));
     L791_=!(L78); 
     if (L791_) {
      SATTR(clear_self3,loc,0);
     }
     str_in_self3 = str_self16;
     str_in_s3 = buf4;
     ret_val73 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
     buf4 = ret_val73;
     str_self17 = buf4;
     ret_val74 = STR_fr1097270334(((STR) NULL), str_self17);
     ret_val72 = ret_val74;
     s13 = ret_val72;
     ret_val71 = STR_ap1077157958(plus_self39, s13, TRUE);
     plus_self40 = ret_val71;
     plus_sarg26 = ((STR) &name90);
     ret_val75 = STR_ap2004550586(plus_self40, plus_sarg26);
     res = ret_val75;
    }
    after_first = TRUE;
   }
  }
  after_loop1: ;
 }
 ret_val = res;
 return ret_val;
}
