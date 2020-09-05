#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dGET_MAIN_SIG_struct {
 OB_HEADER header;
 } *dGET_MAIN_SIG;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dOPTIMIZE_struct {
 OB_HEADER header;
 } *dOPTIMIZE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

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

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct GENERATE_AM_struct {/* layout for GENERATE_AM */
 OB_HEADER header;
 struct FLISTSIG_struct *sig_list;
 struct FSETSIG_struct *sig_inprocout;
 struct FSETSIG_struct *sig_recursive;
 struct GLOBAL_TBL_struct *global_tbl;
 struct PROG_struct *prog;
 BOOL only_check;
 BOOL optimize;
 } *GENERATE_AM;

typedef struct GET_MAIN_SIG_struct {/* layout for GET_MAIN_SIG */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct SIG_struct *main_sig;
 } *GET_MAIN_SIG;

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

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct TP_ARRAY_struct {/* layout for TP_ARRAY */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *TP_ARRAY;

typedef struct TP_ARR212643368_frame_struct {
 INT state;
 TP_ARRAY self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ARR212643368_frame;

typedef struct TP_ARR205286314_frame_struct {
 INT state;
 TP_ARRAY self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *TP_ARR205286314_frame;

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

typedef struct TP_GRAPH_ABS_DES_struct {/* layout for TP_GRAPH_ABS_DES */
 OB_HEADER header;
 struct FMAPTP291739594_struct *tbl;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *dollar_lock;
 struct TP_CLASS_struct *gate;
 struct TP_CLASS_struct *mutex;
 } *TP_GRAPH_ABS_DES;

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

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

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

typedef struct ELT_TBL_struct {/* layout for ELT_TBL */
 OB_HEADER header;
 INT aref_asize;
 INT aref_get;
 INT aref_set;
 INT hsize;
 INT asize;
 struct ELT_struct *arr_part[1];
 } *ELT_TBL;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

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

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

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

extern IDENT IDENT_196328811;
extern INT TP_KIN138039446;
extern INT TP_KIN462481068;
extern INT TP_KIND_ext_c_tp;
extern TP_CLASS TP_BUI845074864;
extern TP_CLASS TP_BUILTIN_int;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dTP_is1553335918)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is33989660)(dTP);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST IFC (**dTP_ifcrIFC)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST SIG (**dGET_M422427726)(dGET_MAIN_SIG);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST void (**dINLIN580030003)(dINLINE, AM_ROUT_DEF);

extern RETURNED_CONST void (**dOPTIM1147845184)(dOPTIMIZE, AM_ROUT_DEF);

extern RETURNED_CONST void (**dOPTIM678468045)(dOPTIMIZE, AM_ROUT_DEF);
AM_ROUT_DEF TRANS_503579921(TRANS, ELT);
BOOL ELT_is1907475257(ELT);
BOOL FSETSI722431140(FSETSIG, SIG);
ELT ELT_TB1949891380(ELT_TBL, SIG);
FSETSIG FSETSI346777215(FSETSIG, SIG);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSETSIG GENERA1281179458(GENERATE_AM);
FSETdTP FMAPTP861983338(FMAPTP291739594, TP_CLASS);
IDENT IDENT_1150413730(IDENT, STR);
IFC TP_CLASS_ifcrIFC(TP_CLASS);
IMPL TP_CLA1109727233(TP_CLASS);
INT FLISTS228922656(FLISTSIG);
INT FLISTd1406591968(FLISTdAM_EXPR);
INT FSETdTP_sizerINT(FSETdTP);
SIG FLISTSIG_poprSIG(FLISTSIG);
SIG GET_MA2025156715(GET_MAIN_SIG);
SIG IFC_si2085565042(IFC, SIG);
SIG SIG_TB788365833(SIG_TB788365833_frame);
SIG SIG_TBL_eltbrSIG(SIG_TBL_eltbrSIG_frame);
STR STR_ap2004550586(STR, STR);
TP_CLASS TP_CLA115798516(TP_CLA115798516_frame);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
dTP TP_ARR212643368(TP_ARR212643368_frame);
void AM_OUT1439666796(AM_OUT, dAM);
void FILE_plus_STR(FILE1, STR);
void GENERA1049384869(GENERATE_AM);
void GENERA1485011776(GENERATE_AM);
void GENERA1521863584(GENERATE_AM, SIG);
void GENERA88776523(GENERATE_AM, SIG);
void GET_MA42890279(GET_MAIN_SIG, STR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void TP_ARR205286314(TP_ARR205286314_frame);
void TP_ARR273870804(TP_ARRAY, INT, INT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

FSETSIG GENERA1281179458(GENERATE_AM self) {
 return ATTR(self,sig_recursive);
}


#undef IS_ITER
#define IS_ITER 0

SIG GET_MA2025156715(GET_MAIN_SIG self) {
 return ATTR(self,main_sig);
}


#undef IS_ITER
#define IS_ITER 0

void GENERA1049384869(GENERATE_AM self) {
 TP_CLASS_TBL L11;
 TP_CLASS tp;
 ELT_TBL et;
 SIG_TBL sig_tbl;
 SIG_TBL L21;
 SIG sig1;
 ELT elt1;
 TP_CLASS is_external_self;
 BOOL ret_val = BOOL_zero;
 TP_CLASS kind_self;
 INT ret_val1 = INT_zero;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val2;
 TP_CLASS kind_self1;
 INT ret_val3 = INT_zero;
 TP_CLASS as_tree_self1;
 AS_CLASS_DEF ret_val4;
 TP_CLASS L3;
 BOOL L4;
 INT L5;
 INT L6;
 BOOL L71_;
 dPARSE L8;
 IDENT L9;
 ARRAYdTP L10;
 INT L121_;
 dPARSE L13;
 BOOL L14;
 BOOL L151_;
 INT L16;
 INT L17;
 BOOL L181_;
 INT L19;
 INT L20;
 BOOL L221_;
 dPARSE L23;
 IDENT L24;
 ARRAYdTP L25;
 INT L261_;
 dPARSE L27;
 BOOL L28;
 BOOL L291_;
 INT L30;
 INT L31;
 BOOL L321_;
 IMPL L33;
 IFC L34;
 SIG L35;
 BOOL L36;
 BOOL L371_;
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
   tp = L3;
   is_external_self = tp;
   kind_self = is_external_self;
   L5 = ATTR(kind_self,kind_cache);
   L6 = TP_KIN138039446;
   L71_=(L5)==(L6); 
   if (L71_) {
    as_tree_self = kind_self;
    if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
     L8 = ATTR(ATTR(as_tree_self,prog),parse);
     L9 = ATTR(as_tree_self,name1);
     L10 = ATTR(as_tree_self,params);
     L121_=(L10)==NULL?0:ASIZE((ARRAYdTP)L10); 
     L13 = L8;
     SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L13)])(L13, L9, L121_));
    }
    ret_val2 = ATTR(as_tree_self,my_as_tree);
    L14 = (ret_val2==((AS_CLASS_DEF) NULL));
    L151_=!(L14); 
    if (L151_) {
     SATTR(kind_self,kind_cache,ATTR(ATTR(kind_self,my_as_tree),kind));
    }
   }
   ret_val1 = ATTR(kind_self,kind_cache);
   L16 = ret_val1;
   L17 = TP_KIND_ext_c_tp;
   L181_=(L16)==(L17); 
   if (L181_) {
    L4 = TRUE;
   } else {
    kind_self1 = is_external_self;
    L19 = ATTR(kind_self1,kind_cache);
    L20 = TP_KIN138039446;
    L221_=(L19)==(L20); 
    if (L221_) {
     as_tree_self1 = kind_self1;
     if ((ATTR(as_tree_self1,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L23 = ATTR(ATTR(as_tree_self1,prog),parse);
      L24 = ATTR(as_tree_self1,name1);
      L25 = ATTR(as_tree_self1,params);
      L261_=(L25)==NULL?0:ASIZE((ARRAYdTP)L25); 
      L27 = L23;
      SATTR(as_tree_self1,my_as_tree,(*dPARSE1581792481[TAG(L27)])(L27, L24, L261_));
     }
     ret_val4 = ATTR(as_tree_self1,my_as_tree);
     L28 = (ret_val4==((AS_CLASS_DEF) NULL));
     L291_=!(L28); 
     if (L291_) {
      SATTR(kind_self1,kind_cache,ATTR(ATTR(kind_self1,my_as_tree),kind));
     }
    }
    ret_val3 = ATTR(kind_self1,kind_cache);
    L30 = ret_val3;
    L31 = TP_KIN462481068;
    L321_=(L30)==(L31); 
    L4 = L321_;
   }
   ret_val = L4;
   if (ret_val) {
    L33=TP_CLA1109727233(tp);
    et = ATTR(L33,elts);
    L34=TP_CLASS_ifcrIFC(tp);
    sig_tbl = ATTR(L34,sigs);
    {
     struct SIG_TBL_eltbrSIG_frame_struct other2_0;
SIG_TBL_eltbrSIG_frame noname2 = &other2_0;
     L21 = sig_tbl;
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L35 = SIG_TBL_eltbrSIG(noname2);
      if (noname2->state == -1) {
       goto after_loop1;
      }
      sig1 = L35;
      elt1 = ELT_TB1949891380(et, sig1);
      L36 = ELT_is1907475257(elt1);
      L371_=!(L36); 
      if (L371_) {
       GENERA88776523(self, sig1);
      }
     }
    }
    after_loop1: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void GENERA1485011776(GENERATE_AM self) {
 dBACK_END cgen;
 SIG mn;
 SIG s;
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
 FLISTSIG is_empty_self;
 BOOL ret_val6 = BOOL_zero;
 OUT create_self1;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val8;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val10;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
 dGET_MAIN_SIG L1;
 extern STR Outputsig;
 FILE1 L2;
 OB L3;
 FILE1 L5;
 OB L6;
 extern STR name9;
 FILE1 L8;
 OB L9;
 INT L11;
 BOOL L121_;
 extern STR Outputsig;
 FILE1 L13;
 OB L14;
 FILE1 L16;
 OB L17;
 extern STR name9;
 FILE1 L19;
 OB L20;
 cgen = ATTR(ATTR(self,prog),back_end);
 if (cgen==NULL) {
  goto other462;
 } else
 switch (TAG(cgen)) {
  case CGEN_tag:
   SATTR(self,optimize,ATTR(((CGEN) cgen),optimize));
   SATTR(self,only_check,ATTR(((CGEN) cgen),only_check)); break;
  default: ;
  other462: ;
 }
 L1 = ATTR(ATTR(self,prog),get_main_sig);
 mn = (*dGET_M422427726[TAG(L1)])(L1);
 if ((mn==((SIG) NULL))) {
  return;
 }
 if (ATTR(ATTR(self,prog),show_generated_s)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Outputsig);
  stdout_self = ((FILE1) NULL);
  L3=ZALLOC(sizeof(struct FILE1_struct));
  if (L3==NULL) FATAL("Unable to allocate more memory");
  ((OB)L3)->header.tag=FILE1_tag;
  L2 = ((FILE1) L3);
  r = L2;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ATTR(mn,str);
  stdout_self1 = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r1 = L5;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r2 = L8;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 GENERA88776523(self, mn);
 GENERA1049384869(self);
 while (1) {
  is_empty_self = ATTR(self,sig_list);
  L11 = FLISTS228922656(is_empty_self);
  L121_=(L11)==(0); 
  ret_val6 = L121_;
  if (ret_val6) {
   goto after_loop;
  }
  s = FLISTSIG_poprSIG(ATTR(self,sig_list));
  if (ATTR(ATTR(self,prog),show_generated_s)) {
   create_self1 = ((OUT) NULL);
   ret_val7 = create_self1;
   plus_self3 = ret_val7;
   plus_s3 = ((STR) &Outputsig);
   stdout_self3 = ((FILE1) NULL);
   L14=ZALLOC(sizeof(struct FILE1_struct));
   if (L14==NULL) FATAL("Unable to allocate more memory");
   ((OB)L14)->header.tag=FILE1_tag;
   L13 = ((FILE1) L14);
   r3 = L13;
   
   SATTR(r3,fp,stdout);
   ret_val9 = r3;
   FILE_plus_STR(ret_val9, plus_s3);
   ret_val8 = plus_self3;
   plus_self4 = ret_val8;
   plus_s4 = ATTR(s,str);
   stdout_self4 = ((FILE1) NULL);
   L17=ZALLOC(sizeof(struct FILE1_struct));
   if (L17==NULL) FATAL("Unable to allocate more memory");
   ((OB)L17)->header.tag=FILE1_tag;
   L16 = ((FILE1) L17);
   r4 = L16;
   
   SATTR(r4,fp,stdout);
   ret_val11 = r4;
   FILE_plus_STR(ret_val11, plus_s4);
   ret_val10 = plus_self4;
   plus_self5 = ret_val10;
   plus_s5 = ((STR) &name9);
   stdout_self5 = ((FILE1) NULL);
   L20=ZALLOC(sizeof(struct FILE1_struct));
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=FILE1_tag;
   L19 = ((FILE1) L20);
   r5 = L19;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s5);
  }
  GENERA88776523(self, s);
 }
 after_loop: ;
 SATTR(ATTR(self,prog),all_reached,TRUE);
}


#undef IS_ITER
#define IS_ITER 0

void GENERA1521863584(GENERATE_AM self, SIG s) {
 AM_ROUT_DEF am;
 TP_CLASS stp = ((TP_CLASS) NULL);
 dTP ostp;
 FSETdTP desof;
 TP_ARRAY tparr;
 FSETdTP L11;
 TP_ARRAY L21;
 TP_ARRAY L31;
 dTP tp;
 IFC ifc;
 SIG cs;
 AM_ROUT_DEF create_self;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ROUT_DEF ret_val;
 AM_ROUT_DEF r;
 TP_GRAPH_ABS_DES des_of_self;
 TP_CLASS des_of_tp;
 FSETdTP ret_val1;
 TP_ARRAY create_self1;
 INT create_n = INT_zero;
 TP_ARRAY ret_val2;
 INT L41 = INT_zero;
 TP_ARRAY sort_self;
 INT L51 = INT_zero;
 OUT create_self2;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 OUT ret_val4;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val6;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r2;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r3;
 OUT create_self3;
 OUT ret_val9;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r4;
 ARRAYARG L6;
 INT L71_;
 INT L8;
 INT L91_;
 AM_ROUT_DEF L10;
 INT L12;
 OB L13;
 INT L14;
 TP_ARRAY L15;
 INT L16;
 OB L17;
 INT L18;
 INT L191_br;
 dTP L20;
 dTP L22;
 INT L231_;
 BOOL L24;
 BOOL L251_;
 INT L261_;
 INT L27;
 INT L281_;
 INT L291_br;
 dTP aL291_;
 dTP L30;
 dTP L32;
 INT L331_;
 extern STR AMfor;
 FILE1 L35;
 OB L36;
 FILE1 L38;
 OB L39;
 extern STR name33;
 FILE1 L42;
 OB L43;
 extern STR name9;
 FILE1 L45;
 OB L46;
 BOOL L48;
 BOOL L49;
 BOOL L501_;
 BOOL L52;
 BOOL L531_;
 dOPTIMIZE L54;
 if (ATTR(s,am_created)) {
  return;
 }
 SATTR(s,am_created,TRUE);
 create_self = ((AM_ROUT_DEF) NULL);
 L6 = ATTR(s,args);
 L71_=(L6)==NULL?0:ASIZE((ARRAYARG)L6); 
 L8 = L71_;
 L91_=INTPLUS(1,L8); 
 create_nargs = L91_;
 create_source = SFILE_ID_zero;
 L12 = create_nargs;
 L14=(sizeof(struct AM_ROUT_DEF_struct)-sizeof(AM_FORMAL_ARG))+(L12)*sizeof(AM_FORMAL_ARG);
 L13=ZALLOC_BIG(L14);
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=AM_ROUT_DEF_tag;
#ifdef DESTROY_CHK

   ((OB)L13)->header.destroyed=0;
#endif

 L10 = ((AM_ROUT_DEF) L13);
 L10->asize = L12;
 r = L10;
 SATTR(r,source1,create_source);
 ret_val = r;
 am = ret_val;
 SATTR(am,sig1,s);
 SATTR(am,is_abstract,TRUE);
 ostp = ATTR(s,tp);
 switch (TAG(ostp)) {
  case TP_CLASS_tag:
   stp = ((TP_CLASS) ostp); break;
  default: ;
   FATAL("No applicable type in typecase\nin GENERATE_AM::output_abs_sig(SIG)\n./Middle/generate_am.sa:142:14");
 }
 des_of_self = ATTR(ATTR(self,prog),tp_graph_abs_des);
 des_of_tp = stp;
 ret_val1 = FMAPTP861983338(ATTR(des_of_self,tbl), des_of_tp);
 desof = ret_val1;
 create_self1 = ((TP_ARRAY) NULL);
 create_n = FSETdTP_sizerINT(desof);
 L16 = create_n;
 L18=(sizeof(struct TP_ARRAY_struct)-sizeof(dTP))+(L16)*sizeof(dTP);
 L17=ZALLOC_BIG(L18);
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=TP_ARRAY_tag;
#ifdef DESTROY_CHK

   ((OB)L17)->header.destroyed=0;
#endif

 L15 = ((TP_ARRAY) L17);
 L15->asize = L16;
 ret_val2 = L15;
 tparr = ret_val2;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  BOOL f_L191_ = TRUE;
  L11 = desof;
  /* loop index variable */
  L41 = 0;
  L21 = tparr;
  noname1->self = L11;
  noname1->state = 0;
  L191_br=L21==NULL?0:ASIZE((TP_ARRAY)L21); 
  while (1) {
   if(L41>=L191_br)  goto after_loop; 
   L20 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L22 = L20;
   SARR((TP_ARRAY)L21,L41,(dTP)L22); 
   ;
   L231_=INTPLUS(L41,1); 
   L41 = L231_;
  }
 }
 after_loop: ;
 sort_self = tparr;
 L24 = (sort_self==((TP_ARRAY) NULL));
 L251_=!(L24); 
 if (L251_) {
  L261_=ASIZE((TP_ARRAY)sort_self); 
  L27 = L261_;
  L281_=INTMINUS(L27,1); 
  TP_ARR273870804(sort_self, 0, L281_);
 }
 {
  BOOL f_L291_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L31 = tparr;
  L291_br=L31==NULL?0:ASIZE((TP_ARRAY)L31); 
  while (1) {
   if(L51>=L291_br)  goto after_loop1; 
   aL291_=ARR((TP_ARRAY)L31,L51); 
   tp = aL291_;
   L32 = tp;
   ifc = (*dTP_ifcrIFC[TAG(L32)])(L32);
   cs = IFC_si2085565042(ifc, s);
   GENERA88776523(self, cs);
   L331_=INTPLUS(L51,1); 
   L51 = L331_;
  }
 }
 after_loop1: ;
 SYSDESTROY(tparr); 
 ;
 if (ATTR(ATTR(self,prog),show_am)) {
  create_self2 = ((OUT) NULL);
  ret_val3 = create_self2;
  plus_self = ret_val3;
  plus_s = ((STR) &AMfor);
  stdout_self = ((FILE1) NULL);
  L36=ZALLOC(sizeof(struct FILE1_struct));
  if (L36==NULL) FATAL("Unable to allocate more memory");
  ((OB)L36)->header.tag=FILE1_tag;
  L35 = ((FILE1) L36);
  r1 = L35;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s);
  ret_val4 = plus_self;
  plus_self1 = ret_val4;
  plus_s1 = ATTR(ATTR(am,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L39=ZALLOC(sizeof(struct FILE1_struct));
  if (L39==NULL) FATAL("Unable to allocate more memory");
  ((OB)L39)->header.tag=FILE1_tag;
  L38 = ((FILE1) L39);
  r2 = L38;
  
  SATTR(r2,fp,stdout);
  ret_val7 = r2;
  FILE_plus_STR(ret_val7, plus_s1);
  ret_val6 = plus_self1;
  plus_self2 = ret_val6;
  plus_s2 = ((STR) &name33);
  stdout_self2 = ((FILE1) NULL);
  L43=ZALLOC(sizeof(struct FILE1_struct));
  if (L43==NULL) FATAL("Unable to allocate more memory");
  ((OB)L43)->header.tag=FILE1_tag;
  L42 = ((FILE1) L43);
  r3 = L42;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s2);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
  create_self3 = ((OUT) NULL);
  ret_val9 = create_self3;
  plus_self3 = ret_val9;
  plus_s3 = ((STR) &name9);
  stdout_self3 = ((FILE1) NULL);
  L46=ZALLOC(sizeof(struct FILE1_struct));
  if (L46==NULL) FATAL("Unable to allocate more memory");
  ((OB)L46)->header.tag=FILE1_tag;
  L45 = ((FILE1) L46);
  r4 = L45;
  
  SATTR(r4,fp,stdout);
  ret_val10 = r4;
  FILE_plus_STR(ret_val10, plus_s3);
 }
 L49 = ATTR(ATTR(self,prog),err_seen);
 L501_=!(L49); 
 if (L501_) {
  L52 = ATTR(self,only_check);
  L531_=!(L52); 
  L48 = L531_;
 } else {
  L48 = FALSE;
 }
 if (L48) {
  L54 = ATTR(ATTR(self,prog),optimizer);
  (*dOPTIM1147845184[TAG(L54)])(L54, am);
 }
}


#undef IS_ITER
#define IS_ITER 0

void GENERA88776523(GENERATE_AM self, SIG s) {
 ELT elt1;
 AM_ROUT_DEF am;
 INT i = INT_zero;
 dAM_EXPR c;
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
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val8;
 dTP L1;
 dTP L2;
 dTP L3;
 dTP L4;
 dTP L5;
 IMPL L6;
 BOOL L7;
 dINLINE L8;
 extern STR AMfor;
 FILE1 L9;
 OB L10;
 FILE1 L12;
 OB L13;
 extern STR name33;
 FILE1 L15;
 OB L16;
 extern STR name9;
 FILE1 L18;
 OB L19;
 INT L21;
 BOOL L221_;
 dAM_EXPR L231_;
 INT L241_;
 BOOL L25;
 BOOL L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 dOPTIMIZE L30;
 dOPTIMIZE L31;
 L1 = ATTR(s,tp);
 if ((*dTP_is33989660[TAG(L1)])(L1)) {
  return;
 }
 if (ATTR(s,am_created)) {
  if (FSETSI722431140(ATTR(self,sig_inprocout), s)) {
   SATTR(self,sig_recursive,FSETSI671175003(ATTR(self,sig_recursive), s));
  }
  return;
 }
 if (ATTR(s,am_created)) {
  return;
 }
 L2 = ATTR(s,tp);
 if ((*dTP_is1811059018[TAG(L2)])(L2)) {
  GENERA1521863584(self, s);
  return;
 }
 L3 = ATTR(s,tp);
 if ((*dTP_is1553335918[TAG(L3)])(L3)) {
  return;
 }
 SATTR(s,am_created,TRUE);
 L4 = ATTR(s,tp);
 if ((*dTP_is735947028[TAG(L4)])(L4)) {
  return;
 }
 L5 = ATTR(s,tp);
 L6=(*dTP_implrIMPL[TAG(L5)])(L5);
 elt1 = ELT_TB1949891380(ATTR(L6,elts), s);
 if ((elt1==((ELT) NULL))) {
  return;
 }
 if (ATTR(elt1,is_external)) {
  L7 = ELT_is1907475257(elt1);
 } else {
  L7 = FALSE;
 }
 if (L7) {
  return;
 }
 SATTR(self,sig_inprocout,FSETSI671175003(ATTR(self,sig_inprocout), s));
 am = TRANS_503579921(((TRANS) NULL), elt1);
 SATTR(self,sig_inprocout,FSETSI346777215(ATTR(self,sig_inprocout), s));
 if ((am==((AM_ROUT_DEF) NULL))) {
  return;
 }
 L8 = ATTR(ATTR(self,prog),inliner);
 (*dINLIN580030003[TAG(L8)])(L8, am);
 if (ATTR(ATTR(self,prog),show_am)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AMfor);
  stdout_self = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r = L9;
  
  SATTR(r,fp,stdout);
  ret_val2 = r;
  FILE_plus_STR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  plus_s1 = ATTR(ATTR(am,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r1 = L12;
  
  SATTR(r1,fp,stdout);
  ret_val4 = r1;
  FILE_plus_STR(ret_val4, plus_s1);
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((STR) &name33);
  stdout_self2 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r2 = L15;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
  create_self1 = ((OUT) NULL);
  ret_val6 = create_self1;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name9);
  stdout_self3 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r3 = L18;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 while (1) {
  L21 = FLISTd1406591968(ATTR(am,calls1));
  L221_=(i)<(L21); 
  if (L221_) {
  }
  else {
   goto after_loop;
  }
  aget_self = ATTR(am,calls1);
  aget_ind = i;
  L231_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val8 = L231_;
  c = ret_val8;
  if (c==NULL) {
   goto other461;
  } else
  switch (TAG(c)) {
   case AM_ROU1916046290_tag:
    GENERA88776523(self, ATTR(((AM_ROU1916046290) c),fun)); break;
   case AM_ITE1809135850_tag:
    GENERA88776523(self, ATTR(((AM_ITE1809135850) c),fun)); break;
   case AM_BND1124877163_tag:
    GENERA88776523(self, ATTR(((AM_BND1124877163) c),fun)); break;
   default: ;
   other461: ;
  }
  L241_=INTPLUS(i,1); 
  i = L241_;
 }
 after_loop: ;
 L26 = ATTR(ATTR(self,prog),err_seen);
 L271_=!(L26); 
 if (L271_) {
  L28 = ATTR(self,only_check);
  L291_=!(L28); 
  L25 = L291_;
 } else {
  L25 = FALSE;
 }
 if (L25) {
  if (ATTR(self,optimize)) {
   L30 = ATTR(ATTR(self,prog),optimizer);
   (*dOPTIM1147845184[TAG(L30)])(L30, am);
  }
  else {
   L31 = ATTR(ATTR(self,prog),optimizer);
   (*dOPTIM678468045[TAG(L31)])(L31, am);
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void GET_MA42890279(GET_MAIN_SIG self, STR nm) {
 AS_CLASS_DEF as;
 TP_CLASS mt;
 IMPL im;
 IFC ifc;
 SIG msig = ((SIG) NULL);
 SIG omsig = ((SIG) NULL);
 SIG_TBL L11;
 IDENT L21 = IDENT_zero;
 GET_MAIN_SIG err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 GET_MAIN_SIG err_self;
 STR err_s;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val2;
 GET_MAIN_SIG err_self1;
 STR err_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 GET_MAIN_SIG err_self2;
 STR err_s2;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 GET_MAIN_SIG err_self3;
 STR err_s3;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val8;
 GET_MAIN_SIG err_self4;
 STR err_s4;
 OUT create_self;
 OUT ret_val9;
 OUT plus_self8;
 STR plus_s;
 OUT ret_val10;
 FILE1 stdout_self;
 FILE1 ret_val11;
 FILE1 r;
 OUT plus_self9;
 STR plus_s1;
 OUT ret_val12;
 FILE1 stdout_self1;
 FILE1 ret_val13;
 FILE1 r1;
 OUT plus_self10;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val14;
 FILE1 r2;
 dPARSE L3;
 IDENT L4;
 dPARSE L5;
 extern STR Thereisnotype;
 extern STR formain;
 SIG L6;
 BOOL L7;
 BOOL L81_;
 extern STR Therem1239554929;
 extern STR Norout722149070;
 dTP L9;
 extern STR name18;
 BOOL L10;
 BOOL L121_;
 BOOL L13;
 ARRAYARG L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 ARRAYARG L20;
 ARG L221_;
 ARG L23;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 extern STR Thesig1291482036;
 extern STR doesnt1241815584;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 dTP L30;
 BOOL L31;
 BOOL L321_;
 extern STR Thesig1291482036;
 extern STR doesnt245124874;
 extern STR Mainsig;
 FILE1 L33;
 OB L34;
 FILE1 L36;
 OB L37;
 extern STR name9;
 FILE1 L39;
 OB L40;
 L3 = ATTR(ATTR(self,prog),parse);
 L4 = IDENT_1150413730(IDENT_zero, nm);
 L5 = L3;
 as = (*dPARSE1581792481[TAG(L5)])(L5, L4, 0);
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) as);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 if ((as==((AS_CLASS_DEF) NULL))) {
  err_self = self;
  plus_self = ((STR) &Thereisnotype);
  plus_sarg = nm;
  ret_val = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val;
  plus_sarg1 = ((STR) &formain);
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  err_s = ret_val1;
  PROG_err_STR(ATTR(err_self,prog), err_s);
  return;
 }
 tp_class_for_sel = ATTR(ATTR(self,prog),tp_tbl);
 tp_class_for_nam = IDENT_1150413730(IDENT_zero, nm);
 tp_class_for_par = ((ARRAYdTP) NULL);
 ret_val2 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
 mt = ret_val2;
 im = TP_CLA1109727233(mt);
 ifc = TP_CLASS_ifcrIFC(mt);
 if ((ifc==((IFC) NULL))) {
  return;
 }
 {
  struct SIG_TB788365833_frame_struct other1_0;
SIG_TB788365833_frame noname1 = &other1_0;
  L11 = ATTR(ifc,sigs);
  L21 = IDENT_196328811;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L6 = SIG_TB788365833(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   msig = L6;
   L7 = (omsig==((SIG) NULL));
   L81_=!(L7); 
   if (L81_) {
    err_self1 = self;
    err_s1 = ((STR) &Therem1239554929);
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
    return;
   }
   omsig = msig;
  }
 }
 after_loop: ;
 if ((msig==((SIG) NULL))) {
  err_self2 = self;
  plus_self2 = ((STR) &Norout722149070);
  L9 = ATTR(ifc,tp);
  plus_sarg2 = (*dTP_strrSTR[TAG(L9)])(L9);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &name18);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  err_s2 = ret_val4;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
  return;
 }
 L10 = (ATTR(msig,args)==((ARRAYARG) NULL));
 L121_=!(L10); 
 if (L121_) {
  L14 = ATTR(msig,args);
  L151_=(L14)==NULL?0:ASIZE((ARRAYARG)L14); 
  L16 = L151_;
  L171_=(L16)==(1); 
  L18 = L171_;
  L191_=!(L18); 
  if (L191_) {
   L13 = TRUE;
  } else {
   L20 = ATTR(msig,args);
   L221_=(ARG)ARR((ARRAYARG)L20,0); 
   L23=L221_;
   L24 = ATTR(L23,tp);
   L25 = (*dTP_is242312711[TAG(L24)])(L24, ((OB) TP_BUI845074864));
   L261_=!(L25); 
   L13 = L261_;
  }
  if (L13) {
   err_self3 = self;
   plus_self4 = ((STR) &Thesig1291482036);
   plus_sarg4 = ATTR(msig,str);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = ((STR) &doesnt1241815584);
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   err_s3 = ret_val6;
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
   return;
  }
 }
 else {
  L28 = (ATTR(msig,ret)==((dTP) NULL));
  L291_=!(L28); 
  if (L291_) {
   L30 = ATTR(msig,ret);
   L31 = (*dTP_is242312711[TAG(L30)])(L30, ((OB) TP_BUILTIN_int));
   L321_=!(L31); 
   L27 = L321_;
  } else {
   L27 = FALSE;
  }
  if (L27) {
   err_self4 = self;
   plus_self6 = ((STR) &Thesig1291482036);
   plus_sarg6 = ATTR(msig,str);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val7;
   plus_sarg7 = ((STR) &doesnt245124874);
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg7);
   err_s4 = ret_val8;
   PROG_err_STR(ATTR(err_self4,prog), err_s4);
   return;
  }
 }
 if (ATTR(ATTR(self,prog),show_main)) {
  create_self = ((OUT) NULL);
  ret_val9 = create_self;
  plus_self8 = ret_val9;
  plus_s = ((STR) &Mainsig);
  stdout_self = ((FILE1) NULL);
  L34=ZALLOC(sizeof(struct FILE1_struct));
  if (L34==NULL) FATAL("Unable to allocate more memory");
  ((OB)L34)->header.tag=FILE1_tag;
  L33 = ((FILE1) L34);
  r = L33;
  
  SATTR(r,fp,stdout);
  ret_val11 = r;
  FILE_plus_STR(ret_val11, plus_s);
  ret_val10 = plus_self8;
  plus_self9 = ret_val10;
  plus_s1 = ATTR(msig,str);
  stdout_self1 = ((FILE1) NULL);
  L37=ZALLOC(sizeof(struct FILE1_struct));
  if (L37==NULL) FATAL("Unable to allocate more memory");
  ((OB)L37)->header.tag=FILE1_tag;
  L36 = ((FILE1) L37);
  r1 = L36;
  
  SATTR(r1,fp,stdout);
  ret_val13 = r1;
  FILE_plus_STR(ret_val13, plus_s1);
  ret_val12 = plus_self9;
  plus_self10 = ret_val12;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L40=ZALLOC(sizeof(struct FILE1_struct));
  if (L40==NULL) FATAL("Unable to allocate more memory");
  ((OB)L40)->header.tag=FILE1_tag;
  L39 = ((FILE1) L40);
  r2 = L39;
  
  SATTR(r2,fp,stdout);
  ret_val14 = r2;
  FILE_plus_STR(ret_val14, plus_s2);
 }
 SATTR(self,main_sig,msig);
}
