#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

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

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BIND_FORTRAN_struct {/* layout for BIND_FORTRAN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *BIND_FORTRAN;

typedef struct BOUND_1007407331_struct {/* layout for BOUND_ITER_FRAME_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 struct PROG_struct *prog;
 STR str;
 } *BOUND_1007407331;

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

typedef struct ELT_struct {/* layout for ELT */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *as;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 struct TP_CONTEXT_struct *con;
 BOOL is_external;
 BOOL is_private;
 } *ELT;

typedef struct ELT_NI602850927_struct {/* layout for ELT_NIL{AM_ROUT_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI602850927;

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

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

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

typedef struct TUPdTPINT_struct {/* layout for TUP{$TP,INT} */
 struct dTP_struct *t1;
 INT t2;
 } TUPdTPINT;
static TUPdTPINT TUPdTPINT_zero;

typedef struct TUPdTPINT_boxed_struct {
 OB_HEADER header;
 TUPdTPINT immutable_part;
 } *TUPdTPINT_boxed;

typedef struct TUPAM_667892060_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1677815802_struct *t2;
 } TUPAM_667892060;
static TUPAM_667892060 TUPAM_667892060_zero;

typedef struct TUPAM_667892060_boxed_struct {
 OB_HEADER header;
 TUPAM_667892060 immutable_part;
 } *TUPAM_667892060_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

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

typedef struct AM_BND1502113605_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1502113605_frame;

typedef struct AM_BND1640868266_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1640868266_frame;

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

typedef struct ARRAYA827580689_frame_struct {
 INT state;
 ARRAYARG self;/* Formal argument: self */
 ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA827580689_frame;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

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

typedef struct ARRAYS1828691793_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1828691793_frame;

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

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FMAPdTPINT_struct {/* layout for FMAP{$TP,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPINT_struct arr_part[1];
 } *FMAPdTPINT;

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

typedef struct FMAPAM1357596931_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FMAPAM1357596931;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

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

typedef struct FSETAM1032778315_struct {/* layout for FSET{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FSETAM1032778315;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

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

extern FMAPAM1357596931 BOUND_2051717332;
extern INOUT_MODE MODES_inout_mode;
extern INT TP_KIN462481068;
extern IN_MODE MODES_in_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI845074864;
extern TP_CLASS TP_BUILTIN_str;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST IMPL (**dTP_implrIMPL)(dTP);

extern RETURNED_CONST INT (**dTP_kindrINT)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);
ABSTRA151498155 ABSTRA1493033801(ABSTRA151498155, SIG, PROG);
AM_BND1124877163 FLISTA1230733197(FLISTA1893461511);
AM_CALL_ARG AM_BND1502113605(AM_BND1502113605_frame);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AM_ROUT_DEF FSETAM1484639533(FSETAM_ROUT_DEF);
ARG ARRAYA827580689(ARRAYA827580689_frame);
BOOL CGEN_a952489530(CGEN);
BOOL ELT_is1907475257(ELT);
BOOL FSETAM195283444(FSETAM_ROUT_DEF, AM_ROUT_DEF);
BOOL STR_is111530248(STR, STR);
BOUND_1007407331 BOUND_11324444(BOUND_1007407331, AM_BND1124877163, PROG);
BOUND_1677815802 FMAPAM661174734(FMAPAM1357596931, AM_BND1124877163);
ELT ELT_TB1949891380(ELT_TBL, SIG);
FLISTA1893461511 FLISTA80425519(FLISTA1893461511, AM_BND1124877163);
FMAPAM1357596931 FMAPAM1106357779(FMAPAM1357596931, AM_BND1124877163, BOUND_1677815802);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPdTPINT FMAPdT1485079589(FMAPdTPINT, dTP, INT);
FRAME_LAYOUT FRAME_248172754(FRAME_LAYOUT, AM_ROUT_DEF, PROG);
FSETAM1032778315 FSETAM1990484536(FSETAM1032778315, AM_BND1124877163);
FSETAM_ROUT_DEF FSETAM1353877611(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSETdTP FSETdT2034766666(FSETdTP, dTP);
FSETdTP FSETdT217460096(FSETdTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT FLISTA919540447(FLISTA1893461511);
INT FMAPdT1072907830(FMAPdT1072907830_frame);
INT FSETdTP_sizerINT(FSETdTP);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
STR CGEN_a1155118157(CGEN, dTP);
STR CGEN_a59983474(CGEN, dTP, STR);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_e1835933683(CGEN, SIG, dAM_EXPR, ARRAYSTR);
STR CGEN_e1909287073(CGEN, SIG, ARRAYSTR);
STR CGEN_eolrSTR(CGEN);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
TP_CLASS CGEN_a1247402136(CGEN, dTP);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP FMAPdT705360156(FMAPdT705360156_frame);
dTP FSETdTP_eltbrdTP(FSETdTP_eltbrdTP_frame);
void ARRAYd1239612935(ARRAYdTP, INT, INT);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void CGEN_d498181638(CGEN);
void CGEN_d695831906(CGEN);
void CGEN_e324254282(CGEN, AM_ROUT_DEF);
void CGEN_e640781662(CGEN);
void CGEN_g1629839129(CGEN);
void CGEN_g333195909(CGEN);
void CGEN_g637688959(CGEN);
void CGEN_m875729145(CGEN);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_a952489530(CGEN self) {
 BOOL ret_val = BOOL_zero;
 FSETdTP new_ntg;
 FMAPdTPINT L11;
 ARRAYdTP a;
 FSETdTP L21;
 ARRAYdTP L31;
 INT pos = INT_zero;
 INT neg1 = INT_zero;
 FMAPdTPINT L41;
 INT n = INT_zero;
 ARRAYdTP L51;
 dTP tp;
 ARRAYdTP create_self;
 INT create_n = INT_zero;
 ARRAYdTP ret_val1;
 INT L61 = INT_zero;
 ARRAYdTP sort_self;
 INT L71 = INT_zero;
 dTP L8;
 INT L9;
 BOOL L101_;
 ARRAYdTP L12;
 INT L13;
 OB L14;
 INT L15;
 INT L161_br;
 dTP L17;
 dTP L18;
 INT L191_;
 BOOL L20;
 BOOL L221_;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 BOOL L271_;
 BOOL L281_;
 INT L291_;
 INT L301_;
 INT L321_br;
 dTP aL321_;
 dTP L33;
 dTP L34;
 INT L351_;
 INT L361_;
 INT L371_;
 new_ntg = FSETdT217460096(ATTR(self,needs_tag));
 {
  struct FMAPdT705360156_frame_struct other1_0;
FMAPdT705360156_frame noname1 = &other1_0;
  L11 = ATTR(self,tags);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L8 = FMAPdT705360156(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   new_ntg = FSETdT2034766666(new_ntg, L8);
  }
 }
 after_loop: ;
 L9 = FSETdTP_sizerINT(new_ntg);
 L101_=(L9)==(0); 
 if (L101_) {
  ret_val = FALSE;
  return ret_val;
 }
 create_self = ((ARRAYdTP) NULL);
 create_n = FSETdTP_sizerINT(new_ntg);
 L13 = create_n;
 L15=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L13)*sizeof(dTP);
 L14=ZALLOC_BIG(L15);
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L14)->header.destroyed=0;
#endif

 L12 = ((ARRAYdTP) L14);
 L12->asize = L13;
 ret_val1 = L12;
 a = ret_val1;
 {
  struct FSETdTP_eltbrdTP_frame_struct other2_0;
FSETdTP_eltbrdTP_frame noname2 = &other2_0;
  BOOL f_L161_ = TRUE;
  L21 = new_ntg;
  /* loop index variable */
  L61 = 0;
  L31 = a;
  noname2->self = L21;
  noname2->state = 0;
  L161_br=L31==NULL?0:ASIZE((ARRAYdTP)L31); 
  while (1) {
   if(L61>=L161_br)  goto after_loop1; 
   L17 = FSETdTP_eltbrdTP(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   L18 = L17;
   SARR((ARRAYdTP)L31,L61,(dTP)L18); 
   ;
   L191_=INTPLUS(L61,1); 
   L61 = L191_;
  }
 }
 after_loop1: ;
 sort_self = a;
 L20 = (sort_self==((ARRAYdTP) NULL));
 L221_=!(L20); 
 if (L221_) {
  L231_=ASIZE((ARRAYdTP)sort_self); 
  L24 = L231_;
  L251_=INTMINUS(L24,1); 
  ARRAYd1239612935(sort_self, 0, L251_);
 }
 pos = 0;
 neg1 = 0;
 {
  struct FMAPdT1072907830_frame_struct other3_0;
FMAPdT1072907830_frame noname3 = &other3_0;
  L41 = ATTR(self,tags);
  noname3->self = L41;
  noname3->state = 0;
  while (1) {
   L26 = FMAPdT1072907830(noname3);
   if (noname3->state == -1) {
    goto after_loop2;
   }
   n = L26;
   L271_=(pos)<(n); 
   if (L271_) {
    pos = n;
   }
   L281_=(n)<(neg1); 
   if (L281_) {
    neg1 = n;
   }
  }
 }
 after_loop2: ;
 L291_=INTPLUS(pos,1); 
 pos = L291_;
 L301_=INTMINUS(neg1,1); 
 neg1 = L301_;
 {
  BOOL f_L321_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L51 = a;
  L321_br=L51==NULL?0:ASIZE((ARRAYdTP)L51); 
  while (1) {
   if(L71>=L321_br)  goto after_loop3; 
   aL321_=ARR((ARRAYdTP)L51,L71); 
   tp = aL321_;
   L34 = tp;
   if ((*dTP_is1334578382[TAG(L34)])(L34)) {
    SATTR(self,tags,FMAPdT1485079589(ATTR(self,tags), tp, neg1));
    L351_=INTMINUS(neg1,1); 
    neg1 = L351_;
   }
   else {
    SATTR(self,tags,FMAPdT1485079589(ATTR(self,tags), tp, pos));
    L361_=INTPLUS(pos,1); 
    pos = L361_;
   }
   L371_=INTPLUS(L71,1); 
   L71 = L371_;
  }
 }
 after_loop3: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1909287073(CGEN self, SIG sig1, ARRAYSTR arg_list) {
 STR ret_val;
 ELT elt1;
 STR extern11;
 STR res;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG sa1;
 dTP tp;
 TP_CLASS arr;
 STR local11;
 IMPL elt_with_sig_sel;
 SIG elt_with_sig_s;
 ELT ret_val1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CGEN bind_fortran_nam;
 STR bind_fortran_nam1;
 STR ret_val3;
 STR res1 = ((STR) NULL);
 BIND_FORTRAN bind_name_self;
 STR bind_name_name;
 STR ret_val4;
 STR res2 = ((STR) NULL);
 BIND_FORTRAN append_underscor;
 STR append_underscor1;
 STR ret_val5;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val7;
 STR res3;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val8;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val9;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val10;
 STR res4;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val11;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val12;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val13;
 CGEN arg_type_str_sel1;
 ARG arg_type_str_e1;
 STR ret_val14;
 STR res5;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val15;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val16;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val17;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val18;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val19;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 dTP L3;
 BOOL L4;
 BOOL L51_;
 extern STR extern2;
 BOOL L6;
 BOOL L71_;
 extern STR name10;
 extern STR void6;
 dTP L8;
 INT L9;
 INT L10;
 BOOL L121_;
 extern STR S_;
 extern STR name24;
 extern STR name24;
 INT L141_;
 INT L15;
 INT L161_;
 INT L131_,L131_m;
 INT L17;
 ARRAYARG L18;
 INT L191_;
 INT L20;
 ARG L221_;
 BOOL L23;
 BOOL L241_;
 extern STR Localf164518964;
 extern STR name78;
 BOOL L25;
 BOOL L261_;
 FSTR L27;
 dSTR L28;
 BOOL L29;
 BOOL L301_;
 FSTR L31;
 dSTR L32;
 extern STR name79;
 FSTR L33;
 dSTR L34;
 extern STR name80;
 ARRAYARG L35;
 INT L361_;
 INT L37;
 ARG L381_;
 BOOL L39;
 dMODE L40;
 dMODE L41;
 extern STR name58;
 STR L42;
 extern STR name26;
 STR L431_;
 STR L44;
 extern STR name16;
 ARRAYdTP L45;
 dTP L461_;
 extern STR name128;
 extern STR name75;
 extern STR NULLNULL;
 extern STR arr_part2;
 ARRAYARG L47;
 INT L481_;
 INT L49;
 ARG L501_;
 BOOL L52;
 dMODE L53;
 dMODE L54;
 extern STR name58;
 STR L551_;
 INT L561_;
 INT L57;
 INT L581_;
 INT L59;
 BOOL L601_;
 extern STR name29;
 extern STR name29;
 extern STR name76;
 dTP L61;
 BOOL L62;
 BOOL L631_;
 extern STR name26;
 L3 = ATTR(sig1,tp);
 elt_with_sig_sel = (*dTP_implrIMPL[TAG(L3)])(L3);
 elt_with_sig_s = sig1;
 ret_val1 = ELT_TB1949891380(ATTR(elt_with_sig_sel,elts), elt_with_sig_s);
 elt1 = ret_val1;
 L4 = ELT_is1907475257(elt1);
 L51_=!(L4); 
 if (L51_) {
  ret_val = CGEN_e1835933683(self, sig1, ((dAM_EXPR) NULL), arg_list);
  return ret_val;
 }
 extern11 = ((STR) &extern2);
 L6 = (ATTR(sig1,ret)==((dTP) NULL));
 L71_=!(L6); 
 if (L71_) {
  mang_self = self;
  mang_ob = ((OB) ATTR(sig1,ret));
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  extern11 = STR_ap1693864410(extern11, ret_val2, ((STR) &name10));
 }
 else {
  extern11 = STR_ap2004550586(extern11, ((STR) &void6));
 }
 res = ATTR(sig1,name1).str;
 L8 = ATTR(sig1,tp);
 L9 = (*dTP_kindrINT[TAG(L8)])(L8);
 L10 = TP_KIN462481068;
 L121_=(L9)==(L10); 
 if (L121_) {
  bind_fortran_nam = self;
  bind_fortran_nam1 = res;
  res1 = STR_ap1693864410(ATTR(bind_fortran_nam,fortran_prepend_), bind_fortran_nam1, ATTR(bind_fortran_nam,fortran_append_u));
  if (ATTR(bind_fortran_nam,fortran_bind_fun)) {
   bind_name_self = ((BIND_FORTRAN) NULL);
   bind_name_name = res1;
   append_underscor = bind_name_self;
   append_underscor1 = bind_name_name;
   plus_self = append_underscor1;
   plus_sarg = ((STR) &S_);
   ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val5 = ret_val6;
   res2 = ret_val5;
   ret_val4 = res2;
   res1 = ret_val4;
  }
  ret_val3 = res1;
  res = ret_val3;
 }
 res = STR_ap2004550586(res, ((STR) &name24));
 extern11 = STR_ap1693864410(extern11, ATTR(sig1,name1).str, ((STR) &name24));
 {
  BOOL f_L131_ = TRUE;
  L11 = 1;
  L141_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
  L15 = L141_;
  L161_=INTMINUS(L15,1); 
  L21 = L161_;
  L131_=L11-1;L131_m=L21; 
  while (1) {
   if(L131_++>=L131_m)  goto after_loop; 
   i = L131_;
   L18 = ATTR(sig1,args);
   L191_=INTMINUS(i,1); 
   L20 = L191_;
   L221_=(ARG)ARR((ARRAYARG)L18,L20); 
   sa1 = L221_;
   tp = ATTR(sa1,tp);
   arr = CGEN_a1247402136(self, tp);
   L23 = (arr==((TP_CLASS) NULL));
   L241_=!(L23); 
   if (L241_) {
    dec_local_commen = self;
    dec_local_commen1 = tp;
    dec_local_commen2 = ((STR) &Localf164518964);
    res3 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
    comment_self = dec_local_commen;
    comment_f = ATTR(dec_local_commen,code_c);
    comment_com = dec_local_commen2;
    if (ATTR(comment_self,pretty)) {
     plus_self1 = comment_f;
     plus_s = ((dSTR) ((STR) &name78));
     L25 = (plus_s==((dSTR) NULL));
     L261_=!(L25); 
     if (L261_) {
      L27 = ATTR(plus_self1,ntext);
      L28 = plus_s;
      SATTR(plus_self1,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
     }
     ret_val8 = plus_self1;
     plus_self2 = ret_val8;
     plus_s1 = ((dSTR) comment_com);
     L29 = (plus_s1==((dSTR) NULL));
     L301_=!(L29); 
     if (L301_) {
      L31 = ATTR(plus_self2,ntext);
      L32 = plus_s1;
      SATTR(plus_self2,ntext,FSTR_p1752847026(L31, (*dSTR_strrSTR[TAG(L32)])(L32)));
     }
     ret_val9 = plus_self2;
     plus_self3 = ret_val9;
     plus_s2 = ((dSTR) ((STR) &name79));
     L33 = ATTR(plus_self3,ntext);
     L34 = plus_s2;
     SATTR(plus_self3,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
    }
    ret_val7 = res3;
    local11 = ret_val7;
    ndefer_self = self;
    arg_type_str_sel = self;
    L35 = ATTR(sig1,args);
    L361_=INTMINUS(i,1); 
    L37 = L361_;
    L381_=(ARG)ARR((ARRAYARG)L35,L37); 
    arg_type_str_e = L381_;
    mang_self1 = arg_type_str_sel;
    mang_ob1 = ((OB) ATTR(arg_type_str_e,tp));
    ret_val11 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    res4 = ret_val11;
    L40 = ATTR(arg_type_str_e,mode1);
    if ((*dMODE_814247358[TAG(L40)])(L40, ((dMODE) MODES_inout_mode))) {
     L39 = TRUE;
    } else {
     L41 = ATTR(arg_type_str_e,mode1);
     L39 = (*dMODE_814247358[TAG(L41)])(L41, ((dMODE) MODES_out_mode));
    }
    if (L39) {
     plus_self4 = res4;
     plus_sarg1 = ((STR) &name58);
     ret_val12 = STR_ap2004550586(plus_self4, plus_sarg1);
     res4 = ret_val12;
    }
    ret_val10 = res4;
    L42 = ret_val10;
    L431_=(STR)ARR((ARRAYSTR)arg_list,i); 
    L44 = L431_;
    ndefer_s = STR_ap1925291910(local11, ((STR) &name80), L42, ((STR) &name26), L44, ((STR) &name16));
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
    mang_self2 = self;
    L45 = ATTR(arr,params);
    L461_=(dTP)ARR((ARRAYdTP)L45,0); 
    mang_ob2 = ((OB) L461_);
    ret_val13 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    extern11 = STR_ap1693864410(extern11, ret_val13, ((STR) &name128));
    res = STR_ap1925291910(res, ((STR) &name75), local11, ((STR) &NULLNULL), local11, ((STR) &arr_part2));
   }
   else {
    plus_self6 = extern11;
    arg_type_str_sel1 = self;
    L47 = ATTR(sig1,args);
    L481_=INTMINUS(i,1); 
    L49 = L481_;
    L501_=(ARG)ARR((ARRAYARG)L47,L49); 
    arg_type_str_e1 = L501_;
    mang_self3 = arg_type_str_sel1;
    mang_ob3 = ((OB) ATTR(arg_type_str_e1,tp));
    ret_val15 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
    res5 = ret_val15;
    L53 = ATTR(arg_type_str_e1,mode1);
    if ((*dMODE_814247358[TAG(L53)])(L53, ((dMODE) MODES_inout_mode))) {
     L52 = TRUE;
    } else {
     L54 = ATTR(arg_type_str_e1,mode1);
     L52 = (*dMODE_814247358[TAG(L54)])(L54, ((dMODE) MODES_out_mode));
    }
    if (L52) {
     plus_self5 = res5;
     plus_sarg2 = ((STR) &name58);
     ret_val16 = STR_ap2004550586(plus_self5, plus_sarg2);
     res5 = ret_val16;
    }
    ret_val14 = res5;
    plus_sarg3 = ret_val14;
    ret_val17 = STR_ap2004550586(plus_self6, plus_sarg3);
    extern11 = ret_val17;
    plus_self7 = res;
    L551_=(STR)ARR((ARRAYSTR)arg_list,i); 
    plus_sarg4 = L551_;
    ret_val18 = STR_ap2004550586(plus_self7, plus_sarg4);
    res = ret_val18;
   }
   L561_=ASIZE((ARRAYSTR)arg_list); 
   L57 = L561_;
   L581_=INTMINUS(L57,1); 
   L59 = L581_;
   L601_=(i)<(L59); 
   if (L601_) {
    extern11 = STR_ap2004550586(extern11, ((STR) &name29));
    res = STR_ap2004550586(res, ((STR) &name29));
   }
  }
 }
 after_loop: ;
 plus_self8 = ((STR) &name76);
 plus_sarg5 = CGEN_eolrSTR(self);
 ret_val19 = STR_ap2004550586(plus_self8, plus_sarg5);
 extern11 = STR_ap2004550586(extern11, ret_val19);
 L61 = ATTR(sig1,tp);
 L62 = (*dTP_is914808564[TAG(L61)])(L61);
 L631_=!(L62); 
 if (L631_) {
  uses_extern_self = ATTR(self,code_c);
  uses_extern_dec = extern11;
  SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
 }
 ret_val = STR_ap2004550586(res, ((STR) &name26));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_d498181638(CGEN self) {
 STR main_tp;
 STR f;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val;
 CODE_FILE plus_self;
 dSTR plus_s;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val1;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val2;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 FSTR create_self;
 FSTR ret_val3;
 CGEN in_self;
 STR plus_self6;
 STR plus_sarg;
 STR ret_val4;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
 STR plus_self7;
 STR plus_sarg1;
 STR ret_val6;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 PROG psather_self;
 BOOL ret_val7 = BOOL_zero;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN in_self1;
 STR plus_self8;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self9;
 STR plus_sarg3;
 STR ret_val9;
 CGEN ndefer_self8;
 STR ndefer_s8;
 STR plus_self10;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self11;
 STR plus_sarg5;
 STR ret_val11;
 CGEN ndefer_self9;
 STR ndefer_s9;
 STR plus_self12;
 STR plus_sarg6;
 STR ret_val12;
 STR plus_self13;
 STR plus_sarg7;
 STR ret_val13;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN ndefer_self12;
 STR ndefer_s12;
 CGEN ndefer_self13;
 STR ndefer_s13;
 STR plus_self14;
 STR plus_sarg8;
 STR ret_val14;
 STR plus_self15;
 STR plus_sarg9;
 STR ret_val15;
 CGEN ndefer_self14;
 STR ndefer_s14;
 CGEN ndefer_self15;
 STR ndefer_s15;
 CGEN ndefer_self16;
 STR ndefer_s16;
 CGEN ndefer_self17;
 STR ndefer_s17;
 CGEN ndefer_self18;
 STR ndefer_s18;
 CGEN ndefer_self19;
 STR ndefer_s19;
 CGEN ndefer_self20;
 STR ndefer_s20;
 CGEN in_self2;
 STR plus_self16;
 STR plus_sarg10;
 STR ret_val16;
 STR plus_self17;
 STR plus_sarg11;
 STR ret_val17;
 CGEN ndefer_self21;
 STR ndefer_s21;
 STR plus_self18;
 STR plus_sarg12;
 STR ret_val18;
 STR plus_self19;
 STR plus_sarg13;
 STR ret_val19;
 CGEN ndefer_self22;
 STR ndefer_s22;
 STR plus_self20;
 STR plus_sarg14;
 STR ret_val20;
 STR plus_self21;
 STR plus_sarg15;
 STR ret_val21;
 CGEN ndefer_self23;
 STR ndefer_s23;
 STR plus_self22;
 STR plus_sarg16;
 STR ret_val22;
 STR plus_self23;
 STR plus_sarg17;
 STR ret_val23;
 CGEN ndefer_self24;
 STR ndefer_s24;
 CGEN ndefer_self25;
 STR ndefer_s25;
 CGEN move_out_self;
 CGEN ndefer_self26;
 STR ndefer_s26;
 CGEN in_self3;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val24;
 STR res;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val25;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self24;
 STR plus_sarg18;
 STR ret_val26;
 STR plus_self25;
 STR plus_sarg19;
 STR ret_val27;
 CGEN ndefer_self27;
 STR ndefer_s27;
 CGEN ndefer_self28;
 STR ndefer_s28;
 CGEN ndefer_self29;
 STR ndefer_s29;
 CGEN ndefer_self30;
 STR ndefer_s30;
 CGEN ndefer_self31;
 STR ndefer_s31;
 CGEN ndefer_self32;
 STR ndefer_s32;
 CGEN ndefer_self33;
 STR ndefer_s33;
 CGEN tag_for_self1;
 dTP tag_for_tp1;
 STR ret_val28;
 STR res1;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val29;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 STR plus_self26;
 STR plus_sarg20;
 STR ret_val30;
 STR plus_self27;
 STR plus_sarg21;
 STR ret_val31;
 CGEN ndefer_self34;
 STR ndefer_s34;
 CGEN ndefer_self35;
 STR ndefer_s35;
 CGEN ndefer_self36;
 STR ndefer_s36;
 PROG psather_self1;
 BOOL ret_val32 = BOOL_zero;
 CGEN ndefer_self37;
 STR ndefer_s37;
 CGEN ndefer_self38;
 STR ndefer_s38;
 CGEN move_out_self1;
 CGEN ndefer_self39;
 STR ndefer_s39;
 PROG psather_self2;
 BOOL ret_val33 = BOOL_zero;
 CGEN move_out_self2;
 CGEN ndefer_self40;
 STR ndefer_s40;
 CGEN ndefer_self41;
 STR ndefer_s41;
 CGEN move_out_self3;
 CODE_FILE plus_self28;
 dSTR plus_s6;
 CODE_FILE ret_val34;
 CODE_FILE plus_self29;
 dSTR plus_s7;
 dSTR L1;
 OB L2;
 FSTR L3;
 dSTR L4;
 extern STR Defini1520955740;
 extern STR name78;
 BOOL L5;
 BOOL L61_;
 FSTR L7;
 dSTR L8;
 BOOL L9;
 BOOL L101_;
 FSTR L11;
 dSTR L12;
 extern STR name79;
 FSTR L13;
 dSTR L14;
 extern STR intmai1559884463;
 FSTR L15;
 dSTR L16;
 extern STR struct1585435604;
 FSTR L17;
 dSTR L18;
 FSTR L19;
 INT L20;
 OB L21;
 INT L22;
 extern STR name9;
 INT L23;
 INT L241_;
 extern STR main_ob1;
 BOOL L25;
 BOOL L261_;
 extern STR main_args1;
 extern STR intijlength;
 extern STR STRs;
 extern STR intres0;
 extern STR sather540173678;
 extern STR rt_sta483296400;
 BOOL L27;
 extern STR PSATHE958595919;
 INT L28;
 INT L291_;
 dTP L30;
 extern STR main_ob2;
 extern STR S_zero1;
 extern STR main_ob2;
 extern STR name16;
 BOOL L31;
 BOOL L321_;
 extern STR main_args2;
 extern STR argc1;
 extern STR name16;
 extern STR main_a1626095403;
 extern STR fori0iargci;
 extern STR forlen1652118724;
 extern STR s111;
 extern STR length1;
 extern STR name16;
 extern STR sasizelength;
 extern STR forj0j563736514;
 extern STR main_a652001093;
 extern STR name4;
 extern STR signal1726310935;
 extern STR PROTECT_BEGIN1;
 INT L33;
 INT L341_;
 extern STR name1;
 extern STR NULL5;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 BOOL L381_;
 extern STR ressat1960690682;
 extern STR name76;
 extern STR ressat1527183294;
 extern STR name76;
 BOOL L39;
 BOOL L401_;
 extern STR sather1651745304;
 extern STR name76;
 extern STR sather1842900085;
 extern STR name76;
 extern STR rt_stop1;
 INT L41;
 INT L421_;
 extern STR PROTECT_WHEN1;
 INT L43;
 INT L441_;
 extern STR ifF_TAGEXCEPTION;
 extern STR S_tag;
 extern STR name84;
 extern STR STRs1;
 extern STR sSTRrt1487513575;
 extern STR sasize845310572;
 extern STR F_R_AR1075403797;
 extern STR fprint334871173;
 extern STR elsefp1785202769;
 extern STR ifTAGEXCEPTION;
 extern STR S_tag;
 extern STR name84;
 extern STR fprint1701882405;
 extern STR elsefp1645913891;
 BOOL L45;
 extern STR PSATHER_ABORT1;
 extern STR abort1;
 INT L46;
 INT L471_;
 extern STR PROTECT_END1;
 BOOL L48;
 INT L49;
 INT L501_;
 extern STR PSATHER_STOP1;
 extern STR returnres;
 INT L51;
 INT L521_;
 BOOL L53;
 BOOL L541_;
 FSTR L55;
 dSTR L56;
 extern STR name160;
 FSTR L57;
 dSTR L58;
 mang_self = self;
 mang_ob = ((OB) ATTR(ATTR(self,main_sig),tp));
 ret_val = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 main_tp = ret_val;
 SATTR(self,code_c,ATTR(self,system_c));
 plus_self = ATTR(self,system_c);
 L2=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L2)->header.tag=CHAR_tag;
 L1 = (dSTR)((CHAR_boxed) L2);
 ((CHAR_boxed) L1)->immutable_part = '\n';
 plus_s = L1;
 L3 = ATTR(plus_self,ntext);
 L4 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L3, (*dSTR_strrSTR[TAG(L4)])(L4)));
 comment_self = self;
 comment_f = ATTR(self,system_c);
 comment_com = ((STR) &Defini1520955740);
 if (ATTR(comment_self,pretty)) {
  plus_self1 = comment_f;
  plus_s1 = ((dSTR) ((STR) &name78));
  L5 = (plus_s1==((dSTR) NULL));
  L61_=!(L5); 
  if (L61_) {
   L7 = ATTR(plus_self1,ntext);
   L8 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L7, (*dSTR_strrSTR[TAG(L8)])(L8)));
  }
  ret_val1 = plus_self1;
  plus_self2 = ret_val1;
  plus_s2 = ((dSTR) comment_com);
  L9 = (plus_s2==((dSTR) NULL));
  L101_=!(L9); 
  if (L101_) {
   L11 = ATTR(plus_self2,ntext);
   L12 = plus_s2;
   SATTR(plus_self2,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
  }
  ret_val2 = plus_self2;
  plus_self3 = ret_val2;
  plus_s3 = ((dSTR) ((STR) &name79));
  L13 = ATTR(plus_self3,ntext);
  L14 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L13, (*dSTR_strrSTR[TAG(L14)])(L14)));
 }
 plus_self4 = ATTR(self,system_c);
 plus_s4 = ((dSTR) ((STR) &intmai1559884463));
 L15 = ATTR(plus_self4,ntext);
 L16 = plus_s4;
 SATTR(plus_self4,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 if (ATTR(self,func_tables)) {
  plus_self5 = ATTR(self,system_c);
  plus_s5 = ((dSTR) ((STR) &struct1585435604));
  L17 = ATTR(plus_self5,ntext);
  L18 = plus_s5;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L17, (*dSTR_strrSTR[TAG(L18)])(L18)));
 }
 create_self = ((FSTR) NULL);
 L20 = 16;
 L22=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L20)*sizeof(CHAR);
 L21=ZALLOC_LEAF_BIG(L22);
 if (L21==NULL) FATAL("Unable to allocate more memory");
 memset(L21,0,L22);
 ((OB)L21)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L21)->header.destroyed=0;
#endif

 L19 = ((FSTR) L21);
 L19->asize = L20;
 ret_val3 = L19;
 SATTR(self,routine_code,FSTR_p1752847026(ret_val3, ((STR) &name9)));
 in_self = self;
 L23 = ATTR(in_self,indent);
 L241_=INTPLUS(L23,1); 
 SATTR(in_self,indent,L241_);
 ndefer_self = self;
 plus_self6 = main_tp;
 plus_sarg = ((STR) &main_ob1);
 ret_val4 = STR_ap2004550586(plus_self6, plus_sarg);
 ndefer_s = ret_val4;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 L25 = (ATTR(ATTR(self,main_sig),args)==((ARRAYARG) NULL));
 L261_=!(L25); 
 if (L261_) {
  ndefer_self1 = self;
  mang_self1 = self;
  mang_ob1 = ((OB) TP_BUI845074864);
  ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  plus_self7 = ret_val5;
  plus_sarg1 = ((STR) &main_args1);
  ret_val6 = STR_ap2004550586(plus_self7, plus_sarg1);
  ndefer_s1 = ret_val6;
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  ndefer_self2 = self;
  ndefer_s2 = ((STR) &intijlength);
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &STRs);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 ndefer_self4 = self;
 ndefer_s4 = ((STR) &intres0);
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 ndefer_self5 = self;
 ndefer_s5 = ((STR) &sather540173678);
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 ndefer_self6 = self;
 ndefer_s6 = ((STR) &rt_sta483296400);
 CGEN_d695831906(ndefer_self6);
 SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L27 = TRUE;
 } else {
  L27 = ATTR(psather_self,distributed);
 }
 ret_val7 = L27;
 if (ret_val7) {
  ndefer_self7 = self;
  ndefer_s7 = ((STR) &PSATHE958595919);
  CGEN_d695831906(ndefer_self7);
  SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
  in_self1 = self;
  L28 = ATTR(in_self1,indent);
  L291_=INTPLUS(L28,1); 
  SATTR(in_self1,indent,L291_);
 }
 CGEN_e640781662(self);
 SATTR(self,code_c,ATTR(self,system_c));
 L30 = ATTR(ATTR(self,main_sig),tp);
 if ((*dTP_is1334578382[TAG(L30)])(L30)) {
  ndefer_self8 = self;
  plus_self8 = ((STR) &main_ob2);
  plus_sarg2 = main_tp;
  ret_val8 = STR_ap2004550586(plus_self8, plus_sarg2);
  plus_self9 = ret_val8;
  plus_sarg3 = ((STR) &S_zero1);
  ret_val9 = STR_ap2004550586(plus_self9, plus_sarg3);
  ndefer_s8 = ret_val9;
  CGEN_d695831906(ndefer_self8);
  SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
 }
 else {
  ndefer_self9 = self;
  plus_self10 = ((STR) &main_ob2);
  plus_sarg4 = CGEN_a1155118157(self, ATTR(ATTR(self,main_sig),tp));
  ret_val10 = STR_ap2004550586(plus_self10, plus_sarg4);
  plus_self11 = ret_val10;
  plus_sarg5 = ((STR) &name16);
  ret_val11 = STR_ap2004550586(plus_self11, plus_sarg5);
  ndefer_s9 = ret_val11;
  CGEN_d695831906(ndefer_self9);
  SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
 }
 L31 = (ATTR(ATTR(self,main_sig),args)==((ARRAYARG) NULL));
 L321_=!(L31); 
 if (L321_) {
  ndefer_self10 = self;
  plus_self12 = ((STR) &main_args2);
  plus_sarg6 = CGEN_a59983474(self, ((dTP) TP_BUI845074864), ((STR) &argc1));
  ret_val12 = STR_ap2004550586(plus_self12, plus_sarg6);
  plus_self13 = ret_val12;
  plus_sarg7 = ((STR) &name16);
  ret_val13 = STR_ap2004550586(plus_self13, plus_sarg7);
  ndefer_s10 = ret_val13;
  CGEN_d695831906(ndefer_self10);
  SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
  ndefer_self11 = self;
  ndefer_s11 = ((STR) &main_a1626095403);
  CGEN_d695831906(ndefer_self11);
  SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
  ndefer_self12 = self;
  ndefer_s12 = ((STR) &fori0iargci);
  CGEN_d695831906(ndefer_self12);
  SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
  ndefer_self13 = self;
  ndefer_s13 = ((STR) &forlen1652118724);
  CGEN_d695831906(ndefer_self13);
  SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
  ndefer_self14 = self;
  plus_self14 = ((STR) &s111);
  plus_sarg8 = CGEN_a59983474(self, ((dTP) TP_BUILTIN_str), ((STR) &length1));
  ret_val14 = STR_ap2004550586(plus_self14, plus_sarg8);
  plus_self15 = ret_val14;
  plus_sarg9 = ((STR) &name16);
  ret_val15 = STR_ap2004550586(plus_self15, plus_sarg9);
  ndefer_s14 = ret_val15;
  CGEN_d695831906(ndefer_self14);
  SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
  ndefer_self15 = self;
  ndefer_s15 = ((STR) &sasizelength);
  CGEN_d695831906(ndefer_self15);
  SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
  ndefer_self16 = self;
  ndefer_s16 = ((STR) &forj0j563736514);
  CGEN_d695831906(ndefer_self16);
  SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
  ndefer_self17 = self;
  ndefer_s17 = ((STR) &main_a652001093);
  CGEN_d695831906(ndefer_self17);
  SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
  ndefer_self18 = self;
  ndefer_s18 = ((STR) &name4);
  CGEN_d695831906(ndefer_self18);
  SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
 }
 if (ATTR(self,null_segfaults)) {
  ndefer_self19 = self;
  ndefer_s19 = ((STR) &signal1726310935);
  CGEN_d695831906(ndefer_self19);
  SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
 }
 ndefer_self20 = self;
 ndefer_s20 = ((STR) &PROTECT_BEGIN1);
 CGEN_d695831906(ndefer_self20);
 SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
 in_self2 = self;
 L33 = ATTR(in_self2,indent);
 L341_=INTPLUS(L33,1); 
 SATTR(in_self2,indent,L341_);
 f = ((STR) &name1);
 if (ATTR(self,func_tables)) {
  f = ((STR) &NULL5);
 }
 L35 = (ATTR(ATTR(self,main_sig),ret)==((dTP) NULL));
 L361_=!(L35); 
 if (L361_) {
  L37 = (ATTR(ATTR(self,main_sig),args)==((ARRAYARG) NULL));
  L381_=!(L37); 
  if (L381_) {
   ndefer_self21 = self;
   plus_self16 = ((STR) &ressat1960690682);
   plus_sarg10 = f;
   ret_val16 = STR_ap2004550586(plus_self16, plus_sarg10);
   plus_self17 = ret_val16;
   plus_sarg11 = ((STR) &name76);
   ret_val17 = STR_ap2004550586(plus_self17, plus_sarg11);
   ndefer_s21 = ret_val17;
   CGEN_d695831906(ndefer_self21);
   SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
  }
  else {
   ndefer_self22 = self;
   plus_self18 = ((STR) &ressat1527183294);
   plus_sarg12 = f;
   ret_val18 = STR_ap2004550586(plus_self18, plus_sarg12);
   plus_self19 = ret_val18;
   plus_sarg13 = ((STR) &name76);
   ret_val19 = STR_ap2004550586(plus_self19, plus_sarg13);
   ndefer_s22 = ret_val19;
   CGEN_d695831906(ndefer_self22);
   SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
  }
 }
 else {
  L39 = (ATTR(ATTR(self,main_sig),args)==((ARRAYARG) NULL));
  L401_=!(L39); 
  if (L401_) {
   ndefer_self23 = self;
   plus_self20 = ((STR) &sather1651745304);
   plus_sarg14 = f;
   ret_val20 = STR_ap2004550586(plus_self20, plus_sarg14);
   plus_self21 = ret_val20;
   plus_sarg15 = ((STR) &name76);
   ret_val21 = STR_ap2004550586(plus_self21, plus_sarg15);
   ndefer_s23 = ret_val21;
   CGEN_d695831906(ndefer_self23);
   SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
  }
  else {
   ndefer_self24 = self;
   plus_self22 = ((STR) &sather1842900085);
   plus_sarg16 = f;
   ret_val22 = STR_ap2004550586(plus_self22, plus_sarg16);
   plus_self23 = ret_val22;
   plus_sarg17 = ((STR) &name76);
   ret_val23 = STR_ap2004550586(plus_self23, plus_sarg17);
   ndefer_s24 = ret_val23;
   CGEN_d695831906(ndefer_self24);
   SATTR(ndefer_self24,routine_code,FSTR_p1752847026(ATTR(ndefer_self24,routine_code), ndefer_s24));
  }
 }
 ndefer_self25 = self;
 ndefer_s25 = ((STR) &rt_stop1);
 CGEN_d695831906(ndefer_self25);
 SATTR(ndefer_self25,routine_code,FSTR_p1752847026(ATTR(ndefer_self25,routine_code), ndefer_s25));
 move_out_self = self;
 L41 = ATTR(move_out_self,indent);
 L421_=INTMINUS(L41,1); 
 SATTR(move_out_self,indent,L421_);
 ndefer_self26 = self;
 ndefer_s26 = ((STR) &PROTECT_WHEN1);
 CGEN_d695831906(ndefer_self26);
 SATTR(ndefer_self26,routine_code,FSTR_p1752847026(ATTR(ndefer_self26,routine_code), ndefer_s26));
 in_self3 = self;
 L43 = ATTR(in_self3,indent);
 L441_=INTPLUS(L43,1); 
 SATTR(in_self3,indent,L441_);
 uses_tp_self = ATTR(self,code_c);
 uses_tp_t = ((dTP) TP_BUILTIN_str);
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 if (ATTR(ATTR(self,prog),distributed)) {
  ndefer_self27 = self;
  plus_self24 = ((STR) &ifF_TAGEXCEPTION);
  tag_for_self = self;
  tag_for_tp = ((dTP) TP_BUILTIN_str);
  SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
  mang_self2 = tag_for_self;
  mang_ob2 = ((OB) tag_for_tp);
  ret_val25 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  res = STR_ap2004550586(ret_val25, ((STR) &S_tag));
  forbid_self = tag_for_self;
  forbid_s = res;
  forbid_self1 = ATTR(forbid_self,mangler);
  forbid_s1 = forbid_s;
  SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
  ret_val24 = res;
  plus_sarg18 = ret_val24;
  ret_val26 = STR_ap2004550586(plus_self24, plus_sarg18);
  plus_self25 = ret_val26;
  plus_sarg19 = ((STR) &name84);
  ret_val27 = STR_ap2004550586(plus_self25, plus_sarg19);
  ndefer_s27 = ret_val27;
  CGEN_d695831906(ndefer_self27);
  SATTR(ndefer_self27,routine_code,FSTR_p1752847026(ATTR(ndefer_self27,routine_code), ndefer_s27));
  ndefer_self28 = self;
  ndefer_s28 = ((STR) &STRs1);
  CGEN_d695831906(ndefer_self28);
  SATTR(ndefer_self28,routine_code,FSTR_p1752847026(ATTR(ndefer_self28,routine_code), ndefer_s28));
  ndefer_self29 = self;
  ndefer_s29 = ((STR) &sSTRrt1487513575);
  CGEN_d695831906(ndefer_self29);
  SATTR(ndefer_self29,routine_code,FSTR_p1752847026(ATTR(ndefer_self29,routine_code), ndefer_s29));
  ndefer_self30 = self;
  ndefer_s30 = ((STR) &sasize845310572);
  CGEN_d695831906(ndefer_self30);
  SATTR(ndefer_self30,routine_code,FSTR_p1752847026(ATTR(ndefer_self30,routine_code), ndefer_s30));
  ndefer_self31 = self;
  ndefer_s31 = ((STR) &F_R_AR1075403797);
  CGEN_d695831906(ndefer_self31);
  SATTR(ndefer_self31,routine_code,FSTR_p1752847026(ATTR(ndefer_self31,routine_code), ndefer_s31));
  ndefer_self32 = self;
  ndefer_s32 = ((STR) &fprint334871173);
  CGEN_d695831906(ndefer_self32);
  SATTR(ndefer_self32,routine_code,FSTR_p1752847026(ATTR(ndefer_self32,routine_code), ndefer_s32));
  ndefer_self33 = self;
  ndefer_s33 = ((STR) &elsefp1785202769);
  CGEN_d695831906(ndefer_self33);
  SATTR(ndefer_self33,routine_code,FSTR_p1752847026(ATTR(ndefer_self33,routine_code), ndefer_s33));
 }
 else {
  ndefer_self34 = self;
  plus_self26 = ((STR) &ifTAGEXCEPTION);
  tag_for_self1 = self;
  tag_for_tp1 = ((dTP) TP_BUILTIN_str);
  SATTR(tag_for_self1,needs_tag,FSETdT1016814448(ATTR(tag_for_self1,needs_tag), tag_for_tp1));
  mang_self3 = tag_for_self1;
  mang_ob3 = ((OB) tag_for_tp1);
  ret_val29 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
  res1 = STR_ap2004550586(ret_val29, ((STR) &S_tag));
  forbid_self2 = tag_for_self1;
  forbid_s2 = res1;
  forbid_self3 = ATTR(forbid_self2,mangler);
  forbid_s3 = forbid_s2;
  SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
  ret_val28 = res1;
  plus_sarg20 = ret_val28;
  ret_val30 = STR_ap2004550586(plus_self26, plus_sarg20);
  plus_self27 = ret_val30;
  plus_sarg21 = ((STR) &name84);
  ret_val31 = STR_ap2004550586(plus_self27, plus_sarg21);
  ndefer_s34 = ret_val31;
  CGEN_d695831906(ndefer_self34);
  SATTR(ndefer_self34,routine_code,FSTR_p1752847026(ATTR(ndefer_self34,routine_code), ndefer_s34));
  ndefer_self35 = self;
  ndefer_s35 = ((STR) &fprint1701882405);
  CGEN_d695831906(ndefer_self35);
  SATTR(ndefer_self35,routine_code,FSTR_p1752847026(ATTR(ndefer_self35,routine_code), ndefer_s35));
  ndefer_self36 = self;
  ndefer_s36 = ((STR) &elsefp1645913891);
  CGEN_d695831906(ndefer_self36);
  SATTR(ndefer_self36,routine_code,FSTR_p1752847026(ATTR(ndefer_self36,routine_code), ndefer_s36));
 }
 psather_self1 = ATTR(self,prog);
 if (ATTR(psather_self1,threads)) {
  L45 = TRUE;
 } else {
  L45 = ATTR(psather_self1,distributed);
 }
 ret_val32 = L45;
 if (ret_val32) {
  ndefer_self37 = self;
  ndefer_s37 = ((STR) &PSATHER_ABORT1);
  CGEN_d695831906(ndefer_self37);
  SATTR(ndefer_self37,routine_code,FSTR_p1752847026(ATTR(ndefer_self37,routine_code), ndefer_s37));
 }
 else {
  ndefer_self38 = self;
  ndefer_s38 = ((STR) &abort1);
  CGEN_d695831906(ndefer_self38);
  SATTR(ndefer_self38,routine_code,FSTR_p1752847026(ATTR(ndefer_self38,routine_code), ndefer_s38));
 }
 move_out_self1 = self;
 L46 = ATTR(move_out_self1,indent);
 L471_=INTMINUS(L46,1); 
 SATTR(move_out_self1,indent,L471_);
 ndefer_self39 = self;
 ndefer_s39 = ((STR) &PROTECT_END1);
 CGEN_d695831906(ndefer_self39);
 SATTR(ndefer_self39,routine_code,FSTR_p1752847026(ATTR(ndefer_self39,routine_code), ndefer_s39));
 psather_self2 = ATTR(self,prog);
 if (ATTR(psather_self2,threads)) {
  L48 = TRUE;
 } else {
  L48 = ATTR(psather_self2,distributed);
 }
 ret_val33 = L48;
 if (ret_val33) {
  move_out_self2 = self;
  L49 = ATTR(move_out_self2,indent);
  L501_=INTMINUS(L49,1); 
  SATTR(move_out_self2,indent,L501_);
  ndefer_self40 = self;
  ndefer_s40 = ((STR) &PSATHER_STOP1);
  CGEN_d695831906(ndefer_self40);
  SATTR(ndefer_self40,routine_code,FSTR_p1752847026(ATTR(ndefer_self40,routine_code), ndefer_s40));
 }
 ndefer_self41 = self;
 ndefer_s41 = ((STR) &returnres);
 CGEN_d695831906(ndefer_self41);
 SATTR(ndefer_self41,routine_code,FSTR_p1752847026(ATTR(ndefer_self41,routine_code), ndefer_s41));
 move_out_self3 = self;
 L51 = ATTR(move_out_self3,indent);
 L521_=INTMINUS(L51,1); 
 SATTR(move_out_self3,indent,L521_);
 plus_self28 = ATTR(self,system_c);
 plus_s6 = ((dSTR) ATTR(self,routine_code));
 L53 = (plus_s6==((dSTR) NULL));
 L541_=!(L53); 
 if (L541_) {
  L55 = ATTR(plus_self28,ntext);
  L56 = plus_s6;
  SATTR(plus_self28,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
 }
 ret_val34 = plus_self28;
 plus_self29 = ret_val34;
 plus_s7 = ((dSTR) ((STR) &name160));
 L57 = ATTR(plus_self29,ntext);
 L58 = plus_s7;
 SATTR(plus_self29,ntext,FSTR_p1752847026(L57, (*dSTR_strrSTR[TAG(L58)])(L58)));
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g1629839129(CGEN self) {
 AM_BND1124877163 e;
 STR name111;
 ARRAYINT L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 INT num1 = INT_zero;
 STR dec = ((STR) NULL);
 ARG a;
 AM_BND1124877163 L31;
 AM_CALL_ARG arg;
 AM_BND1124877163 L41;
 INT i1 = INT_zero;
 ARRAYSTR arg_list;
 INT bnd = INT_zero;
 INT unbnd = INT_zero;
 BOOL is_bnd = BOOL_zero;
 ARRAYSTR L51;
 INT i2 = INT_zero;
 BOOL isout = BOOL_zero;
 ARRAYARG L61;
 STR func_res;
 STR ec = ((STR) NULL);
 FLISTA1893461511 is_empty_self;
 BOOL ret_val = BOOL_zero;
 CODE_FILE uses_bnd_rout_cr;
 AM_BND1124877163 uses_bnd_rout_cr1;
 BOUND_1677815802 create_self;
 AM_BND1124877163 create_e;
 BOUND_1677815802 ret_val1;
 BOUND_1677815802 res;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val6;
 CODE_FILE plus_self3;
 dSTR plus_s;
 CODE_FILE ret_val7;
 CODE_FILE plus_self4;
 dSTR plus_s1;
 CODE_FILE plus_self5;
 dSTR plus_s2;
 CODE_FILE plus_self6;
 dSTR plus_s3;
 CODE_FILE ret_val8;
 CODE_FILE plus_self7;
 dSTR plus_s4;
 CODE_FILE ret_val9;
 CODE_FILE plus_self8;
 dSTR plus_s5;
 CODE_FILE ret_val10;
 CODE_FILE plus_self9;
 dSTR plus_s6;
 INT L71 = INT_zero;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val11;
 CODE_FILE plus_self10;
 dSTR plus_s7;
 CODE_FILE ret_val12;
 CODE_FILE plus_self11;
 dSTR plus_s8;
 CODE_FILE ret_val13;
 CODE_FILE plus_self12;
 dSTR plus_s9;
 CODE_FILE ret_val14;
 CODE_FILE plus_self13;
 dSTR plus_s10;
 CODE_FILE plus_self14;
 dSTR plus_s11;
 CODE_FILE ret_val15;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val16;
 STR res1;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val17;
 STR plus_self15;
 STR plus_sarg3;
 STR ret_val18;
 CODE_FILE plus_self16;
 dSTR plus_s12;
 CODE_FILE ret_val19;
 CODE_FILE plus_self17;
 dSTR plus_s13;
 CODE_FILE ret_val20;
 CODE_FILE plus_self18;
 dSTR plus_s14;
 CODE_FILE plus_self19;
 dSTR plus_s15;
 CODE_FILE plus_self20;
 dSTR plus_s16;
 CODE_FILE ret_val21;
 CODE_FILE plus_self21;
 dSTR plus_s17;
 CGEN in_self;
 FSTR create_self1;
 FSTR ret_val22;
 CODE_FILE plus_self22;
 dSTR plus_s18;
 CODE_FILE ret_val23;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val24;
 CODE_FILE plus_self23;
 dSTR plus_s19;
 CODE_FILE ret_val25;
 CODE_FILE plus_self24;
 dSTR plus_s20;
 CODE_FILE plus_self25;
 dSTR plus_s21;
 CODE_FILE ret_val26;
 CODE_FILE plus_self26;
 dSTR plus_s22;
 CODE_FILE ret_val27;
 CODE_FILE plus_self27;
 dSTR plus_s23;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT L81 = INT_zero;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val28;
 STR res2;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val29;
 CGEN forbid_self4;
 STR forbid_s4;
 MANGLE forbid_self5;
 STR forbid_s5;
 STR plus_self28;
 STR plus_sarg4;
 STR ret_val30;
 STR plus_self29;
 STR plus_sarg5;
 STR ret_val31;
 STR plus_self30;
 INT plus_arg = INT_zero;
 STR ret_val32;
 STR s;
 INT str_self = INT_zero;
 STR ret_val33;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val34;
 FSTR str_self1;
 STR ret_val35;
 STR plus_self31;
 STR plus_sarg6;
 STR ret_val36;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self32;
 INT plus_arg1 = INT_zero;
 STR ret_val37;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val38;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val39;
 FSTR str_self3;
 STR ret_val40;
 STR plus_self33;
 STR plus_sarg7;
 STR ret_val41;
 STR plus_self34;
 INT plus_arg2 = INT_zero;
 STR ret_val42;
 STR s2;
 INT str_self4 = INT_zero;
 STR ret_val43;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val44;
 FSTR str_self5;
 STR ret_val45;
 STR plus_self35;
 STR plus_sarg8;
 STR ret_val46;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 ARRAYSTR create_self2;
 INT create_n = INT_zero;
 ARRAYSTR ret_val47;
 INT L91 = INT_zero;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self36;
 STR plus_sarg9;
 STR ret_val48;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self37;
 STR plus_sarg10;
 STR ret_val49;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self38;
 INT plus_arg3 = INT_zero;
 STR ret_val50;
 STR s3;
 INT str_self6 = INT_zero;
 STR ret_val51;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val52;
 FSTR str_self7;
 STR ret_val53;
 STR plus_self39;
 INT plus_arg4 = INT_zero;
 STR ret_val54;
 STR s4;
 INT str_self8 = INT_zero;
 STR ret_val55;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val56;
 FSTR str_self9;
 STR ret_val57;
 STR plus_self40;
 INT plus_arg5 = INT_zero;
 STR ret_val58;
 STR s5;
 INT str_self10 = INT_zero;
 STR ret_val59;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val60;
 FSTR str_self11;
 STR ret_val61;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 CODE_FILE plus_self41;
 dSTR plus_s24;
 CODE_FILE plus_self42;
 dSTR plus_s25;
 CODE_FILE plus_self43;
 dSTR plus_s26;
 CODE_FILE plus_self44;
 dSTR plus_s27;
 CODE_FILE plus_self45;
 dSTR plus_s28;
 CODE_FILE plus_self46;
 dSTR plus_s29;
 CODE_FILE ret_val62;
 CODE_FILE plus_self47;
 dSTR plus_s30;
 CODE_FILE plus_self48;
 dSTR plus_s31;
 CODE_FILE plus_self49;
 dSTR plus_s32;
 CODE_FILE plus_self50;
 dSTR plus_s33;
 CGEN move_out_self;
 BOOL L10;
 INT L12;
 BOOL L131_;
 BOUND_1677815802 L14;
 OB L15;
 extern STR boundobject;
 extern STR S_ob1;
 extern STR S_ob_struct1;
 BOOL L16;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 FSTR L24;
 dSTR L25;
 dSTR L26;
 OB L27;
 FSTR L28;
 dSTR L29;
 extern STR void6;
 FSTR L30;
 dSTR L32;
 BOOL L33;
 BOOL L341_;
 FSTR L35;
 dSTR L36;
 dSTR L37;
 OB L38;
 BOOL L39;
 BOOL L401_;
 FSTR L42;
 dSTR L43;
 BOOL L44;
 BOOL L451_;
 FSTR L46;
 dSTR L47;
 extern STR S_obob;
 FSTR L48;
 dSTR L49;
 INT L501_br;
 INT aL501_;
 INT L521_;
 INT L53;
 INT L54;
 BOOL L551_;
 dTP L56;
 BOOL L57;
 BOOL L581_;
 extern STR name29;
 BOOL L59;
 BOOL L601_;
 FSTR L62;
 dSTR L63;
 BOOL L64;
 BOOL L651_;
 FSTR L66;
 dSTR L67;
 extern STR unbound_arg;
 BOOL L68;
 BOOL L691_;
 FSTR L70;
 dSTR L72;
 dSTR L73;
 OB L74;
 FSTR L75;
 dSTR L76;
 ARRAYARG L77;
 INT L781_;
 INT L79;
 ARG L801_;
 extern STR name29;
 BOOL L82;
 BOOL L831_;
 FSTR L84;
 dSTR L85;
 BOOL L86;
 dMODE L87;
 dMODE L88;
 extern STR name58;
 BOOL L89;
 BOOL L901_;
 FSTR L92;
 dSTR L93;
 extern STR unbound_arg;
 BOOL L94;
 BOOL L951_;
 FSTR L96;
 dSTR L97;
 dSTR L98;
 OB L99;
 FSTR L100;
 dSTR L101;
 INT L1021_;
 extern STR struct1621582358;
 FSTR L103;
 dSTR L104;
 extern STR name84;
 BOOL L105;
 BOOL L1061_;
 FSTR L107;
 dSTR L108;
 FSTR L109;
 dSTR L110;
 INT L111;
 INT L1121_;
 FSTR L113;
 INT L114;
 OB L115;
 INT L116;
 BOOL L117;
 BOOL L1181_;
 BOOL L119;
 BOOL L1201_;
 extern STR name10;
 BOOL L121;
 BOOL L1221_;
 FSTR L123;
 dSTR L124;
 BOOL L125;
 BOOL L1261_;
 FSTR L127;
 dSTR L128;
 extern STR retval1;
 FSTR L129;
 dSTR L130;
 extern STR name10;
 BOOL L132;
 BOOL L1331_;
 FSTR L134;
 dSTR L135;
 BOOL L136;
 BOOL L1371_;
 FSTR L138;
 dSTR L139;
 extern STR S_obobnNULL;
 FSTR L140;
 dSTR L141;
 extern STR ifFARob;
 extern STR obnZAL437024763;
 extern STR get_me375728054;
 INT L1421_br;
 AM_CALL_ARG aL1421_;
 INT L1431_br;
 INT rL1431_;
 AM_CALL_ARG L144;
 INT L145;
 dAM_EXPR L146;
 dTP L147;
 dAM_EXPR L148;
 dTP L149;
 BOOL L150;
 BOOL L1511_;
 extern STR RECVOB2;
 dAM_EXPR L152;
 extern STR S_tag;
 extern STR obnbound_arg;
 BOOL L153;
 BOOL L1541_;
 extern STR WHEREob;
 extern STR obnbound_arg1;
 BOOL L155;
 BOOL L1561_;
 extern STR RECVFO1140193010;
 BOOL L157;
 BOOL L1581_;
 extern STR WHEREob;
 INT L1591_;
 extern STR obobn;
 extern STR name139;
 ARRAYARG L160;
 INT L1611_;
 INT L162;
 INT L1631_;
 ARRAYSTR L164;
 INT L165;
 OB L166;
 INT L167;
 INT L1681_br;
 INT rL1681_;
 INT L170;
 ARRAYINT L171;
 INT L1721_;
 INT L173;
 BOOL L1741_;
 ARRAYINT L175;
 INT L1761_;
 INT L177;
 BOOL L1781_;
 ARRAYINT L179;
 INT L1801_;
 INT L182;
 BOOL L1831_;
 extern STR Ranoff1276757503;
 extern STR Intern28965746;
 ARRAYINT L184;
 INT L1851_;
 INT L186;
 BOOL L1871_;
 extern STR Ranoff158073728;
 extern STR Intern28965746;
 BOOL L188;
 BOOL L1891_;
 ARG L190;
 INT L1691_br;
INT i_L1691_=0;
 ARG aL1691_;
 ARG L191;
 dMODE L192;
 BOOL L193;
 BOOL L1941_;
 BOOL L195;
 BOOL L1961_;
 dTP L197;
 BOOL L198;
 BOOL L1991_;
 extern STR obbound_arg;
 BOOL L200;
 BOOL L2021_;
 STR L203;
 extern STR obbound_arg1;
 BOOL L205;
 BOOL L2061_;
 STR L207;
 extern STR unbound_arg1;
 BOOL L209;
 BOOL L2101_;
 STR L211;
 INT L2131_;
 INT L2141_;
 INT L2151_;
 BOOL L216;
 BOOL L2171_;
 dTP L218;
 FSTR L219;
 dSTR L220;
 BOOL L221;
 BOOL L2221_;
 extern STR obret_arg;
 FSTR L223;
 dSTR L224;
 extern STR retval2;
 FSTR L225;
 dSTR L226;
 extern STR return6;
 FSTR L227;
 dSTR L228;
 extern STR name38;
 FSTR L229;
 dSTR L230;
 BOOL L232;
 BOOL L2331_;
 FSTR L234;
 dSTR L235;
 extern STR name13;
 FSTR L236;
 dSTR L237;
 BOOL L238;
 BOOL L239;
 BOOL L2401_;
 extern STR ifobnb2144963834;
 FSTR L241;
 dSTR L242;
 BOOL L243;
 BOOL L2441_;
 extern STR returnretval;
 FSTR L245;
 dSTR L246;
 extern STR name161;
 FSTR L247;
 dSTR L248;
 INT L249;
 INT L2501_;
 SATTR(self,code_c,ATTR(self,system_c));
 while (1) {
  if ((ATTR(self,bnd_rout_creates)==((FLISTA1893461511) NULL))) {
   L10 = TRUE;
  } else {
   is_empty_self = ATTR(self,bnd_rout_creates);
   L12 = FLISTA919540447(is_empty_self);
   L131_=(L12)==(0); 
   ret_val = L131_;
   L10 = ret_val;
  }
  if (L10) {
   goto after_loop;
  }
  e = FLISTA1230733197(ATTR(self,bnd_rout_creates));
  SATTR(self,bnd_rout_creates1,FLISTA80425519(ATTR(self,bnd_rout_creates1), e));
  uses_bnd_rout_cr = ATTR(self,code_c);
  uses_bnd_rout_cr1 = e;
  SATTR(uses_bnd_rout_cr,bnd_rout_creates,FSETAM1990484536(ATTR(uses_bnd_rout_cr,bnd_rout_creates), uses_bnd_rout_cr1));
  create_self = ((BOUND_1677815802) NULL);
  create_e = e;
  res = FMAPAM661174734(BOUND_2051717332, create_e);
  if ((res==((BOUND_1677815802) NULL))) {
   L15=ZALLOC(sizeof(struct BOUND_1677815802_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=BOUND_1677815802_tag;
   L14 = ((BOUND_1677815802) L15);
   res = L14;
   SATTR(res,e,create_e);
   plus_self = ATTR(ATTR(ATTR(res,e),fun),str);
   plus_sarg = ((STR) &boundobject);
   ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
   SATTR(res,str,ret_val2);
   BOUND_2051717332 = FMAPAM1106357779(BOUND_2051717332, create_e, res);
  }
  ret_val1 = res;
  CODE_F525364269(ATTR(self,code_c), ((dLAYOUT) ret_val1));
  mang_self = self;
  mang_ob = ((OB) e);
  ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  name111 = ret_val3;
  forbid_self = self;
  plus_self1 = name111;
  plus_sarg1 = ((STR) &S_ob1);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  forbid_s = ret_val4;
  forbid_self1 = ATTR(forbid_self,mangler);
  forbid_s1 = forbid_s;
  SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
  forbid_self2 = self;
  plus_self2 = name111;
  plus_sarg2 = ((STR) &S_ob_struct1);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  forbid_s2 = ret_val5;
  forbid_self3 = ATTR(forbid_self2,mangler);
  forbid_s3 = forbid_s2;
  SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
  L17 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
  L181_=!(L17); 
  if (L181_) {
   L19 = ATTR(e,is_remote);
   L201_=!(L19); 
   L16 = L201_;
  } else {
   L16 = FALSE;
  }
  if (L16) {
   plus_self3 = ATTR(self,code_c);
   mang_self1 = self;
   mang_ob1 = ((OB) ATTR(ATTR(e,fun),ret));
   ret_val6 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
   plus_s = ((dSTR) ret_val6);
   L22 = (plus_s==((dSTR) NULL));
   L231_=!(L22); 
   if (L231_) {
    L24 = ATTR(plus_self3,ntext);
    L25 = plus_s;
    SATTR(plus_self3,ntext,FSTR_p1752847026(L24, (*dSTR_strrSTR[TAG(L25)])(L25)));
   }
   ret_val7 = plus_self3;
   plus_self4 = ret_val7;
   L27=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L27==NULL) FATAL("Unable to allocate more memory");
   memset(L27,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L27)->header.tag=CHAR_tag;
   L26 = (dSTR)((CHAR_boxed) L27);
   ((CHAR_boxed) L26)->immutable_part = ' ';
   plus_s1 = L26;
   L28 = ATTR(plus_self4,ntext);
   L29 = plus_s1;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L28, (*dSTR_strrSTR[TAG(L29)])(L29)));
  }
  else {
   plus_self5 = ATTR(self,code_c);
   plus_s2 = ((dSTR) ((STR) &void6));
   L30 = ATTR(plus_self5,ntext);
   L32 = plus_s2;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L30, (*dSTR_strrSTR[TAG(L32)])(L32)));
  }
  plus_self6 = ATTR(self,code_c);
  plus_s3 = ((dSTR) name111);
  L33 = (plus_s3==((dSTR) NULL));
  L341_=!(L33); 
  if (L341_) {
   L35 = ATTR(plus_self6,ntext);
   L36 = plus_s3;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L35, (*dSTR_strrSTR[TAG(L36)])(L36)));
  }
  ret_val8 = plus_self6;
  plus_self7 = ret_val8;
  L38=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  memset(L38,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L38)->header.tag=CHAR_tag;
  L37 = (dSTR)((CHAR_boxed) L38);
  ((CHAR_boxed) L37)->immutable_part = '(';
  plus_s4 = L37;
  L39 = (plus_s4==((dSTR) NULL));
  L401_=!(L39); 
  if (L401_) {
   L42 = ATTR(plus_self7,ntext);
   L43 = plus_s4;
   SATTR(plus_self7,ntext,FSTR_p1752847026(L42, (*dSTR_strrSTR[TAG(L43)])(L43)));
  }
  ret_val9 = plus_self7;
  plus_self8 = ret_val9;
  plus_s5 = ((dSTR) name111);
  L44 = (plus_s5==((dSTR) NULL));
  L451_=!(L44); 
  if (L451_) {
   L46 = ATTR(plus_self8,ntext);
   L47 = plus_s5;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L46, (*dSTR_strrSTR[TAG(L47)])(L47)));
  }
  ret_val10 = plus_self8;
  plus_self9 = ret_val10;
  plus_s6 = ((dSTR) ((STR) &S_obob));
  L48 = ATTR(plus_self9,ntext);
  L49 = plus_s6;
  SATTR(plus_self9,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
  {
   BOOL f_L501_ = TRUE;
   BOOL f_L521_ = TRUE;
   /* loop index variable */
   L71 = 0;
   L11 = ATTR(e,unbnd_args);
   L21 = 0;
   L501_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
   L521_=L21-1; 
   while (1) {
    if(L71>=L501_br)  goto after_loop1; 
    aL501_=ARR((ARRAYINT)L11,L71); 
    i = aL501_;
    L521_++; 
    num1 = L521_;
    L551_=(i)==(0); 
    if (L551_) {
     L56 = ATTR(ATTR(e,fun),tp);
     L57 = (*dTP_is124163553[TAG(L56)])(L56);
     L581_=!(L57); 
     if (L581_) {
      mang_self2 = self;
      mang_ob2 = ((OB) ATTR(ATTR(e,fun),tp));
      ret_val11 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
      dec = ret_val11;
      plus_self10 = ATTR(self,code_c);
      plus_s7 = ((dSTR) ((STR) &name29));
      L59 = (plus_s7==((dSTR) NULL));
      L601_=!(L59); 
      if (L601_) {
       L62 = ATTR(plus_self10,ntext);
       L63 = plus_s7;
       SATTR(plus_self10,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
      }
      ret_val12 = plus_self10;
      plus_self11 = ret_val12;
      plus_s8 = ((dSTR) dec);
      L64 = (plus_s8==((dSTR) NULL));
      L651_=!(L64); 
      if (L651_) {
       L66 = ATTR(plus_self11,ntext);
       L67 = plus_s8;
       SATTR(plus_self11,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
      }
      ret_val13 = plus_self11;
      plus_self12 = ret_val13;
      plus_s9 = ((dSTR) ((STR) &unbound_arg));
      L68 = (plus_s9==((dSTR) NULL));
      L691_=!(L68); 
      if (L691_) {
       L70 = ATTR(plus_self12,ntext);
       L72 = plus_s9;
       SATTR(plus_self12,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
      }
      ret_val14 = plus_self12;
      plus_self13 = ret_val14;
      L74=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L74==NULL) FATAL("Unable to allocate more memory");
      memset(L74,0,sizeof(struct INT_boxed_struct));
      ((OB)L74)->header.tag=INT_tag;
      L73 = (dSTR)((INT_boxed) L74);
      ((INT_boxed) L73)->immutable_part = num1;
      plus_s10 = L73;
      L75 = ATTR(plus_self13,ntext);
      L76 = plus_s10;
      SATTR(plus_self13,ntext,FSTR_p1752847026(L75, (*dSTR_strrSTR[TAG(L76)])(L76)));
     }
    }
    else {
     L77 = ATTR(ATTR(e,fun),args);
     L781_=INTMINUS(i,1); 
     L79 = L781_;
     L801_=(ARG)ARR((ARRAYARG)L77,L79); 
     a = L801_;
     plus_self14 = ATTR(self,code_c);
     plus_s11 = ((dSTR) ((STR) &name29));
     L82 = (plus_s11==((dSTR) NULL));
     L831_=!(L82); 
     if (L831_) {
      L84 = ATTR(plus_self14,ntext);
      L85 = plus_s11;
      SATTR(plus_self14,ntext,FSTR_p1752847026(L84, (*dSTR_strrSTR[TAG(L85)])(L85)));
     }
     ret_val15 = plus_self14;
     plus_self16 = ret_val15;
     arg_type_str_sel = self;
     arg_type_str_e = a;
     mang_self3 = arg_type_str_sel;
     mang_ob3 = ((OB) ATTR(arg_type_str_e,tp));
     ret_val17 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     res1 = ret_val17;
     L87 = ATTR(arg_type_str_e,mode1);
     if ((*dMODE_814247358[TAG(L87)])(L87, ((dMODE) MODES_inout_mode))) {
      L86 = TRUE;
     } else {
      L88 = ATTR(arg_type_str_e,mode1);
      L86 = (*dMODE_814247358[TAG(L88)])(L88, ((dMODE) MODES_out_mode));
     }
     if (L86) {
      plus_self15 = res1;
      plus_sarg3 = ((STR) &name58);
      ret_val18 = STR_ap2004550586(plus_self15, plus_sarg3);
      res1 = ret_val18;
     }
     ret_val16 = res1;
     plus_s12 = ((dSTR) ret_val16);
     L89 = (plus_s12==((dSTR) NULL));
     L901_=!(L89); 
     if (L901_) {
      L92 = ATTR(plus_self16,ntext);
      L93 = plus_s12;
      SATTR(plus_self16,ntext,FSTR_p1752847026(L92, (*dSTR_strrSTR[TAG(L93)])(L93)));
     }
     ret_val19 = plus_self16;
     plus_self17 = ret_val19;
     plus_s13 = ((dSTR) ((STR) &unbound_arg));
     L94 = (plus_s13==((dSTR) NULL));
     L951_=!(L94); 
     if (L951_) {
      L96 = ATTR(plus_self17,ntext);
      L97 = plus_s13;
      SATTR(plus_self17,ntext,FSTR_p1752847026(L96, (*dSTR_strrSTR[TAG(L97)])(L97)));
     }
     ret_val20 = plus_self17;
     plus_self18 = ret_val20;
     L99=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
     if (L99==NULL) FATAL("Unable to allocate more memory");
     memset(L99,0,sizeof(struct INT_boxed_struct));
     ((OB)L99)->header.tag=INT_tag;
     L98 = (dSTR)((INT_boxed) L99);
     ((INT_boxed) L98)->immutable_part = num1;
     plus_s14 = L98;
     L100 = ATTR(plus_self18,ntext);
     L101 = plus_s14;
     SATTR(plus_self18,ntext,FSTR_p1752847026(L100, (*dSTR_strrSTR[TAG(L101)])(L101)));
    }
    L1021_=INTPLUS(L71,1); 
    L71 = L1021_;
   }
  }
  after_loop1: ;
  if (ATTR(self,func_tables)) {
   plus_self19 = ATTR(self,code_c);
   plus_s15 = ((dSTR) ((STR) &struct1621582358));
   L103 = ATTR(plus_self19,ntext);
   L104 = plus_s15;
   SATTR(plus_self19,ntext,FSTR_p1752847026(L103, (*dSTR_strrSTR[TAG(L104)])(L104)));
  }
  plus_self20 = ATTR(self,code_c);
  plus_s16 = ((dSTR) ((STR) &name84));
  L105 = (plus_s16==((dSTR) NULL));
  L1061_=!(L105); 
  if (L1061_) {
   L107 = ATTR(plus_self20,ntext);
   L108 = plus_s16;
   SATTR(plus_self20,ntext,FSTR_p1752847026(L107, (*dSTR_strrSTR[TAG(L108)])(L108)));
  }
  ret_val21 = plus_self20;
  plus_self21 = ret_val21;
  plus_s17 = ((dSTR) CGEN_eolrSTR(self));
  L109 = ATTR(plus_self21,ntext);
  L110 = plus_s17;
  SATTR(plus_self21,ntext,FSTR_p1752847026(L109, (*dSTR_strrSTR[TAG(L110)])(L110)));
  in_self = self;
  L111 = ATTR(in_self,indent);
  L1121_=INTPLUS(L111,1); 
  SATTR(in_self,indent,L1121_);
  CODE_F1291349628(ATTR(self,code_c), ATTR(e,fun));
  create_self1 = ((FSTR) NULL);
  L114 = 16;
  L116=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L114)*sizeof(CHAR);
  L115=ZALLOC_LEAF_BIG(L116);
  if (L115==NULL) FATAL("Unable to allocate more memory");
  memset(L115,0,L116);
  ((OB)L115)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L115)->header.destroyed=0;
#endif

  L113 = ((FSTR) L115);
  L113->asize = L114;
  ret_val22 = L113;
  SATTR(self,routine_code,ret_val22);
  if (ATTR(ATTR(self,prog),distributed)) {
   L117 = ATTR(e,is_remote);
   L1181_=!(L117); 
   if (L1181_) {
    L119 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
    L1201_=!(L119); 
    if (L1201_) {
     plus_self22 = ATTR(self,code_c);
     plus_s18 = ((dSTR) ((STR) &name10));
     L121 = (plus_s18==((dSTR) NULL));
     L1221_=!(L121); 
     if (L1221_) {
      L123 = ATTR(plus_self22,ntext);
      L124 = plus_s18;
      SATTR(plus_self22,ntext,FSTR_p1752847026(L123, (*dSTR_strrSTR[TAG(L124)])(L124)));
     }
     ret_val23 = plus_self22;
     plus_self23 = ret_val23;
     mang_self4 = self;
     mang_ob4 = ((OB) ATTR(ATTR(e,fun),ret));
     ret_val24 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
     plus_s19 = ((dSTR) ret_val24);
     L125 = (plus_s19==((dSTR) NULL));
     L1261_=!(L125); 
     if (L1261_) {
      L127 = ATTR(plus_self23,ntext);
      L128 = plus_s19;
      SATTR(plus_self23,ntext,FSTR_p1752847026(L127, (*dSTR_strrSTR[TAG(L128)])(L128)));
     }
     ret_val25 = plus_self23;
     plus_self24 = ret_val25;
     plus_s20 = ((dSTR) ((STR) &retval1));
     L129 = ATTR(plus_self24,ntext);
     L130 = plus_s20;
     SATTR(plus_self24,ntext,FSTR_p1752847026(L129, (*dSTR_strrSTR[TAG(L130)])(L130)));
    }
    plus_self25 = ATTR(self,code_c);
    plus_s21 = ((dSTR) ((STR) &name10));
    L132 = (plus_s21==((dSTR) NULL));
    L1331_=!(L132); 
    if (L1331_) {
     L134 = ATTR(plus_self25,ntext);
     L135 = plus_s21;
     SATTR(plus_self25,ntext,FSTR_p1752847026(L134, (*dSTR_strrSTR[TAG(L135)])(L135)));
    }
    ret_val26 = plus_self25;
    plus_self26 = ret_val26;
    plus_s22 = ((dSTR) name111);
    L136 = (plus_s22==((dSTR) NULL));
    L1371_=!(L136); 
    if (L1371_) {
     L138 = ATTR(plus_self26,ntext);
     L139 = plus_s22;
     SATTR(plus_self26,ntext,FSTR_p1752847026(L138, (*dSTR_strrSTR[TAG(L139)])(L139)));
    }
    ret_val27 = plus_self26;
    plus_self27 = ret_val27;
    plus_s23 = ((dSTR) ((STR) &S_obobnNULL));
    L140 = ATTR(plus_self27,ntext);
    L141 = plus_s23;
    SATTR(plus_self27,ntext,FSTR_p1752847026(L140, (*dSTR_strrSTR[TAG(L141)])(L141)));
    ndefer_self = self;
    ndefer_s = ((STR) &ifFARob);
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
    ndefer_self1 = self;
    ndefer_s1 = ((STR) &obnZAL437024763);
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
    ndefer_self2 = self;
    ndefer_s2 = ((STR) &get_me375728054);
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
    {
     BOOL f_L1421_ = TRUE;
     BOOL f_L1431_ = TRUE;
     /* loop index variable */
     L81 = 0;
     L31 = e;
     L41 = e;
     L1421_br=L31==NULL?0:ASIZE((AM_BND1124877163)L31); 
     L1431_br=L41==NULL?0:ASIZE((AM_BND1124877163)L41); 
     while (1) {
      if(L81>=L1421_br)  goto after_loop2; 
      aL1421_=ARR((AM_BND1124877163)L31,L81); 
      arg = aL1421_;
      if(L81>=L1431_br)  goto after_loop2; 
      rL1431_=L81; 
      i1 = rL1431_;
      L146 = ATTR(arg,expr);
      L147 = (*dAM_EXPR_tprdTP[TAG(L146)])(L146);
      if ((*dTP_is1334578382[TAG(L147)])(L147)) {
       L148 = ATTR(arg,expr);
       L149 = (*dAM_EXPR_tprdTP[TAG(L148)])(L148);
       L150 = (*dTP_is418055720[TAG(L149)])(L149);
       L1511_=!(L150); 
       if (L1511_) {
        ndefer_self3 = self;
        plus_self28 = ((STR) &RECVOB2);
        tag_for_self = self;
        L152 = ATTR(arg,expr);
        tag_for_tp = (*dAM_EXPR_tprdTP[TAG(L152)])(L152);
        SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
        mang_self5 = tag_for_self;
        mang_ob5 = ((OB) tag_for_tp);
        ret_val29 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
        res2 = STR_ap2004550586(ret_val29, ((STR) &S_tag));
        forbid_self4 = tag_for_self;
        forbid_s4 = res2;
        forbid_self5 = ATTR(forbid_self4,mangler);
        forbid_s5 = forbid_s4;
        SATTR(forbid_self5,forbidden,FSETST1404644833(ATTR(forbid_self5,forbidden), forbid_s5));
        ret_val28 = res2;
        plus_sarg4 = ret_val28;
        ret_val30 = STR_ap2004550586(plus_self28, plus_sarg4);
        plus_self29 = ret_val30;
        plus_sarg5 = ((STR) &obnbound_arg);
        ret_val31 = STR_ap2004550586(plus_self29, plus_sarg5);
        plus_self30 = ret_val31;
        plus_arg = i1;
        str_self = plus_arg;
        clear_self = buf1;
        L153 = (clear_self==((FSTR) NULL));
        L1541_=!(L153); 
        if (L1541_) {
         SATTR(clear_self,loc,0);
        }
        str_in_self = str_self;
        str_in_s = buf1;
        ret_val34 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
        buf1 = ret_val34;
        str_self1 = buf1;
        ret_val35 = STR_fr1097270334(((STR) NULL), str_self1);
        ret_val33 = ret_val35;
        s = ret_val33;
        ret_val32 = STR_ap1077157958(plus_self30, s, TRUE);
        plus_self31 = ret_val32;
        plus_sarg6 = ((STR) &WHEREob);
        ret_val36 = STR_ap2004550586(plus_self31, plus_sarg6);
        ndefer_s3 = ret_val36;
        CGEN_d695831906(ndefer_self3);
        SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
       }
      }
      else {
       ndefer_self4 = self;
       plus_self32 = ((STR) &obnbound_arg1);
       plus_arg1 = i1;
       str_self2 = plus_arg1;
       clear_self1 = buf2;
       L155 = (clear_self1==((FSTR) NULL));
       L1561_=!(L155); 
       if (L1561_) {
        SATTR(clear_self1,loc,0);
       }
       str_in_self1 = str_self2;
       str_in_s1 = buf2;
       ret_val39 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
       buf2 = ret_val39;
       str_self3 = buf2;
       ret_val40 = STR_fr1097270334(((STR) NULL), str_self3);
       ret_val38 = ret_val40;
       s1 = ret_val38;
       ret_val37 = STR_ap1077157958(plus_self32, s1, TRUE);
       plus_self33 = ret_val37;
       plus_sarg7 = ((STR) &RECVFO1140193010);
       ret_val41 = STR_ap2004550586(plus_self33, plus_sarg7);
       plus_self34 = ret_val41;
       plus_arg2 = i1;
       str_self4 = plus_arg2;
       clear_self2 = buf3;
       L157 = (clear_self2==((FSTR) NULL));
       L1581_=!(L157); 
       if (L1581_) {
        SATTR(clear_self2,loc,0);
       }
       str_in_self2 = str_self4;
       str_in_s2 = buf3;
       ret_val44 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
       buf3 = ret_val44;
       str_self5 = buf3;
       ret_val45 = STR_fr1097270334(((STR) NULL), str_self5);
       ret_val43 = ret_val45;
       s2 = ret_val43;
       ret_val42 = STR_ap1077157958(plus_self34, s2, TRUE);
       plus_self35 = ret_val42;
       plus_sarg8 = ((STR) &WHEREob);
       ret_val46 = STR_ap2004550586(plus_self35, plus_sarg8);
       ndefer_s4 = ret_val46;
       CGEN_d695831906(ndefer_self4);
       SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
      }
      L1591_=INTPLUS(L81,1); 
      L81 = L1591_;
     }
    }
    after_loop2: ;
    ndefer_self5 = self;
    ndefer_s5 = ((STR) &obobn);
    CGEN_d695831906(ndefer_self5);
    SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
    ndefer_self6 = self;
    ndefer_s6 = ((STR) &name139);
    CGEN_d695831906(ndefer_self6);
    SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
   }
  }
  create_self2 = ((ARRAYSTR) NULL);
  L160 = ATTR(ATTR(e,fun),args);
  L1611_=(L160)==NULL?0:ASIZE((ARRAYARG)L160); 
  L162 = L1611_;
  L1631_=INTPLUS(L162,1); 
  create_n = L1631_;
  L165 = create_n;
  L167=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L165)*sizeof(STR);
  L166=ZALLOC_BIG(L167);
  if (L166==NULL) FATAL("Unable to allocate more memory");
  ((OB)L166)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L166)->header.destroyed=0;
#endif

  L164 = ((ARRAYSTR) L166);
  L164->asize = L165;
  ret_val47 = L164;
  arg_list = ret_val47;
  bnd = 0;
  unbnd = 0;
  {
   BOOL f_L1681_ = TRUE;
   BOOL f_L1691_ = TRUE;
   /* loop index variable */
   L91 = 0;
   L51 = arg_list;
   L1681_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
   while (1) {
    if(L91>=L1681_br)  goto after_loop3; 
    rL1681_=L91; 
    i2 = rL1681_;
    L171 = ATTR(e,bnd_args);
    L1721_=(L171)==NULL?0:ASIZE((ARRAYINT)L171); 
    L173 = L1721_;
    L1741_=(bnd)<(L173); 
    if (L1741_) {
     L175 = ATTR(e,bnd_args);
     L1761_=ARR((ARRAYINT)L175,bnd); 
     L177 = L1761_;
     L1781_=(L177)==(i2); 
     if (L1781_) {
      is_bnd = TRUE;
     }
     else {
      L179 = ATTR(e,unbnd_args);
      L1801_=ARR((ARRAYINT)L179,unbnd); 
      L182 = L1801_;
      L1831_=(L182)==(i2); 
      if (L1831_) {
       is_bnd = FALSE;
      }
      else {
       barf_self = self;
       barf_msg = ((STR) &Ranoff1276757503);
       barf_self1 = ATTR(barf_self,prog);
       barf_msg1 = barf_msg;
       barf_at_self = barf_self1;
       barf_at_msg = barf_msg1;
       barf_at_at = ((dPROG_ERR) NULL);
       PROG_e176405615(barf_at_self, barf_at_at);
       plus_self36 = ((STR) &Intern28965746);
       plus_sarg9 = barf_at_msg;
       ret_val48 = STR_ap2004550586(plus_self36, plus_sarg9);
       PROG_err_STR(barf_at_self, ret_val48);
       exit_self = ((UNIX) NULL);
       exit_code = 1;
       exit(exit_code);
      }
     }
    }
    else {
     L184 = ATTR(e,unbnd_args);
     L1851_=ARR((ARRAYINT)L184,unbnd); 
     L186 = L1851_;
     L1871_=(L186)==(i2); 
     if (L1871_) {
      is_bnd = FALSE;
     }
     else {
      barf_self2 = self;
      barf_msg2 = ((STR) &Ranoff158073728);
      barf_self3 = ATTR(barf_self2,prog);
      barf_msg3 = barf_msg2;
      barf_at_self1 = barf_self3;
      barf_at_msg1 = barf_msg3;
      barf_at_at1 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self1, barf_at_at1);
      plus_self37 = ((STR) &Intern28965746);
      plus_sarg10 = barf_at_msg1;
      ret_val49 = STR_ap2004550586(plus_self37, plus_sarg10);
      PROG_err_STR(barf_at_self1, ret_val49);
      exit_self1 = ((UNIX) NULL);
      exit_code1 = 1;
      exit(exit_code1);
     }
    }
    isout = FALSE;
    L1891_=(0)<(i2); 
    if (L1891_) {
     if (f_L1691_) {
      f_L1691_ = FALSE;
      L61 = ATTR(ATTR(e,fun),args);
      L1691_br=L61==NULL?0:ASIZE((ARRAYARG)L61); 
      i_L1691_=0;
     }
     if(i_L1691_>=L1691_br)  goto after_loop3; 
     aL1691_=ARR((ARRAYARG)L61,i_L1691_); i_L1691_++;
     L191=aL1691_;
     L192 = ATTR(L191,mode1);
     L193 = (*dMODE_814247358[TAG(L192)])(L192, ((dMODE) MODES_in_mode));
     L1941_=!(L193); 
     L188 = L1941_;
    } else {
     L188 = FALSE;
    }
    if (L188) {
     isout = TRUE;
    }
    L1961_=(i2)==(0); 
    if (L1961_) {
     L197 = ATTR(ATTR(e,fun),tp);
     L195 = (*dTP_is124163553[TAG(L197)])(L197);
    } else {
     L195 = FALSE;
    }
    L198 = L195;
    L1991_=!(L198); 
    if (L1991_) {
     if (is_bnd) {
      if (isout) {
       plus_self38 = ((STR) &obbound_arg);
       plus_arg3 = bnd;
       str_self6 = plus_arg3;
       clear_self3 = buf4;
       L200 = (clear_self3==((FSTR) NULL));
       L2021_=!(L200); 
       if (L2021_) {
        SATTR(clear_self3,loc,0);
       }
       str_in_self3 = str_self6;
       str_in_s3 = buf4;
       ret_val52 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
       buf4 = ret_val52;
       str_self7 = buf4;
       ret_val53 = STR_fr1097270334(((STR) NULL), str_self7);
       ret_val51 = ret_val53;
       s3 = ret_val51;
       ret_val50 = STR_ap1077157958(plus_self38, s3, TRUE);
       L203 = ret_val50;
       SARR((ARRAYSTR)arg_list,i2,(STR)L203); 
       ;
      }
      else {
       plus_self39 = ((STR) &obbound_arg1);
       plus_arg4 = bnd;
       str_self8 = plus_arg4;
       clear_self4 = buf5;
       L205 = (clear_self4==((FSTR) NULL));
       L2061_=!(L205); 
       if (L2061_) {
        SATTR(clear_self4,loc,0);
       }
       str_in_self4 = str_self8;
       str_in_s4 = buf5;
       ret_val56 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
       buf5 = ret_val56;
       str_self9 = buf5;
       ret_val57 = STR_fr1097270334(((STR) NULL), str_self9);
       ret_val55 = ret_val57;
       s4 = ret_val55;
       ret_val54 = STR_ap1077157958(plus_self39, s4, TRUE);
       L207 = ret_val54;
       SARR((ARRAYSTR)arg_list,i2,(STR)L207); 
       ;
      }
     }
     else {
      plus_self40 = ((STR) &unbound_arg1);
      plus_arg5 = unbnd;
      str_self10 = plus_arg5;
      clear_self5 = buf6;
      L209 = (clear_self5==((FSTR) NULL));
      L2101_=!(L209); 
      if (L2101_) {
       SATTR(clear_self5,loc,0);
      }
      str_in_self5 = str_self10;
      str_in_s5 = buf6;
      ret_val60 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
      buf6 = ret_val60;
      str_self11 = buf6;
      ret_val61 = STR_fr1097270334(((STR) NULL), str_self11);
      ret_val59 = ret_val61;
      s5 = ret_val59;
      ret_val58 = STR_ap1077157958(plus_self40, s5, TRUE);
      L211 = ret_val58;
      SARR((ARRAYSTR)arg_list,i2,(STR)L211); 
      ;
     }
    }
    if (is_bnd) {
     L2131_=INTPLUS(bnd,1); 
     bnd = L2131_;
    }
    else {
     L2141_=INTPLUS(unbnd,1); 
     unbnd = L2141_;
    }
    L2151_=INTPLUS(L91,1); 
    L91 = L2151_;
   }
  }
  after_loop3: ;
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(e,fun);
  ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
  L216 = (ard==((AM_ROUT_DEF) NULL));
  L2171_=!(L216); 
  if (L2171_) {
   SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
   SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
  }
  func_res = ((STR) NULL);
  SATTR(self,in_bnd_rout_call,TRUE);
  L218 = ATTR(ATTR(e,fun),tp);
  if ((*dTP_is124163553[TAG(L218)])(L218)) {
   ec = CGEN_e1909287073(self, ATTR(e,fun), arg_list);
  }
  else {
   ec = CGEN_e1835933683(self, ATTR(e,fun), ((dAM_EXPR) e), arg_list);
  }
  SATTR(self,in_bnd_rout_call,FALSE);
  plus_self41 = ATTR(self,code_c);
  plus_s24 = ((dSTR) ATTR(self,routine_code));
  L219 = ATTR(plus_self41,ntext);
  L220 = plus_s24;
  SATTR(plus_self41,ntext,FSTR_p1752847026(L219, (*dSTR_strrSTR[TAG(L220)])(L220)));
  L221 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
  L2221_=!(L221); 
  if (L2221_) {
   if (ATTR(e,is_remote)) {
    plus_self42 = ATTR(self,code_c);
    plus_s25 = ((dSTR) ((STR) &obret_arg));
    L223 = ATTR(plus_self42,ntext);
    L224 = plus_s25;
    SATTR(plus_self42,ntext,FSTR_p1752847026(L223, (*dSTR_strrSTR[TAG(L224)])(L224)));
   }
   else {
    if (ATTR(ATTR(self,prog),distributed)) {
     plus_self43 = ATTR(self,code_c);
     plus_s26 = ((dSTR) ((STR) &retval2));
     L225 = ATTR(plus_self43,ntext);
     L226 = plus_s26;
     SATTR(plus_self43,ntext,FSTR_p1752847026(L225, (*dSTR_strrSTR[TAG(L226)])(L226)));
    }
    else {
     plus_self44 = ATTR(self,code_c);
     plus_s27 = ((dSTR) ((STR) &return6));
     L227 = ATTR(plus_self44,ntext);
     L228 = plus_s27;
     SATTR(plus_self44,ntext,FSTR_p1752847026(L227, (*dSTR_strrSTR[TAG(L228)])(L228)));
    }
   }
  }
  else {
   plus_self45 = ATTR(self,code_c);
   plus_s28 = ((dSTR) ((STR) &name38));
   L229 = ATTR(plus_self45,ntext);
   L230 = plus_s28;
   SATTR(plus_self45,ntext,FSTR_p1752847026(L229, (*dSTR_strrSTR[TAG(L230)])(L230)));
  }
  plus_self46 = ATTR(self,code_c);
  plus_s29 = ((dSTR) ec);
  L232 = (plus_s29==((dSTR) NULL));
  L2331_=!(L232); 
  if (L2331_) {
   L234 = ATTR(plus_self46,ntext);
   L235 = plus_s29;
   SATTR(plus_self46,ntext,FSTR_p1752847026(L234, (*dSTR_strrSTR[TAG(L235)])(L235)));
  }
  ret_val62 = plus_self46;
  plus_self47 = ret_val62;
  plus_s30 = ((dSTR) ((STR) &name13));
  L236 = ATTR(plus_self47,ntext);
  L237 = plus_s30;
  SATTR(plus_self47,ntext,FSTR_p1752847026(L236, (*dSTR_strrSTR[TAG(L237)])(L237)));
  if (ATTR(ATTR(self,prog),distributed)) {
   L239 = ATTR(e,is_remote);
   L2401_=!(L239); 
   L238 = L2401_;
  } else {
   L238 = FALSE;
  }
  if (L238) {
   plus_self48 = ATTR(self,code_c);
   plus_s31 = ((dSTR) ((STR) &ifobnb2144963834));
   L241 = ATTR(plus_self48,ntext);
   L242 = plus_s31;
   SATTR(plus_self48,ntext,FSTR_p1752847026(L241, (*dSTR_strrSTR[TAG(L242)])(L242)));
   L243 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
   L2441_=!(L243); 
   if (L2441_) {
    plus_self49 = ATTR(self,code_c);
    plus_s32 = ((dSTR) ((STR) &returnretval));
    L245 = ATTR(plus_self49,ntext);
    L246 = plus_s32;
    SATTR(plus_self49,ntext,FSTR_p1752847026(L245, (*dSTR_strrSTR[TAG(L246)])(L246)));
   }
  }
  plus_self50 = ATTR(self,code_c);
  plus_s33 = ((dSTR) ((STR) &name161));
  L247 = ATTR(plus_self50,ntext);
  L248 = plus_s33;
  SATTR(plus_self50,ntext,FSTR_p1752847026(L247, (*dSTR_strrSTR[TAG(L248)])(L248)));
  move_out_self = self;
  L249 = ATTR(move_out_self,indent);
  L2501_=INTMINUS(L249,1); 
  SATTR(move_out_self,indent,L2501_);
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g333195909(CGEN self) {
 AM_ROUT_DEF e;
 FSETAM_ROUT_DEF is_elt_nil_self;
 AM_ROUT_DEF is_elt_nil_e;
 BOOL ret_val = BOOL_zero;
 ELT_NI602850927 is_elt_nil_self1;
 AM_ROUT_DEF is_elt_nil_e1;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 while (1) {
  e = FSETAM1484639533(ATTR(self,leftovers));
  is_elt_nil_self = ATTR(self,leftovers);
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NI602850927) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val1 = (is_elt_nil_e1==((AM_ROUT_DEF) NULL));
  ret_val = ret_val1;
  L1 = ret_val;
  L21_=!(L1); 
  if (L21_) {
   L3 = FSETAM195283444(ATTR(self,emitted_leftover), e);
   L41_=!(L3); 
   if (L41_) {
    CGEN_e324254282(self, e);
    SATTR(self,emitted_leftover,FSETAM2044524049(ATTR(self,emitted_leftover), e));
   }
   SATTR(self,leftovers,FSETAM1353877611(ATTR(self,leftovers), e));
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g637688959(CGEN self) {
 STR l_ret_val = ((STR) NULL);
 AM_BND1124877163 e;
 BOOL binding_iter_clo = BOOL_zero;
 IDENT ident1 = IDENT_zero;
 AM_ROUT_DEF iframe;
 STR name111;
 STR self_str = ((STR) NULL);
 STR state011;
 STR state11;
 ARRAYBOOL arg_mode = ((ARRAYBOOL) NULL);
 ARRAYARG args_list = ((ARRAYARG) NULL);
 dTP type_of_bnd_crea;
 INT i_arg = INT_zero;
 ARRAYINT L11;
 INT a = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 INT arg_cnt = INT_zero;
 ARRAYINT L31;
 INT a11 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 STR dtbl_ptr;
 STR l;
 ARRAYINT L51;
 INT a21 = INT_zero;
 INT L61 = INT_zero;
 INT i2 = INT_zero;
 BOOL hot = BOOL_zero;
 ARRAYINT L71;
 INT a31 = INT_zero;
 INT L81 = INT_zero;
 INT i3 = INT_zero;
 BOOL hot1 = BOOL_zero;
 ARRAYINT L91;
 INT a41 = INT_zero;
 INT L101 = INT_zero;
 INT i4 = INT_zero;
 BOOL hot2 = BOOL_zero;
 ARRAYINT L121;
 INT a5 = INT_zero;
 INT L131 = INT_zero;
 INT i5 = INT_zero;
 BOOL hot3 = BOOL_zero;
 ARRAYSTR arg_list;
 STR iter_call = ((STR) NULL);
 FLISTA1893461511 is_empty_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CODE_FILE uses_bnd_iter_cr;
 AM_BND1124877163 uses_bnd_iter_cr1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val7;
 CODE_FILE plus_self4;
 dSTR plus_s;
 CODE_FILE ret_val8;
 CODE_FILE plus_self5;
 dSTR plus_s1;
 CODE_FILE plus_self6;
 dSTR plus_s2;
 CODE_FILE plus_self7;
 dSTR plus_s3;
 CODE_FILE ret_val9;
 CODE_FILE plus_self8;
 dSTR plus_s4;
 CODE_FILE ret_val10;
 CODE_FILE plus_self9;
 dSTR plus_s5;
 CODE_FILE ret_val11;
 CODE_FILE plus_self10;
 dSTR plus_s6;
 CODE_FILE ret_val12;
 CODE_FILE plus_self11;
 dSTR plus_s7;
 CODE_FILE plus_self12;
 dSTR plus_s8;
 CODE_FILE plus_self13;
 dSTR plus_s9;
 CODE_FILE plus_self14;
 dSTR plus_s10;
 CODE_FILE ret_val13;
 CODE_FILE plus_self15;
 dSTR plus_s11;
 CODE_FILE plus_self16;
 dSTR plus_s12;
 CODE_FILE ret_val14;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val15;
 CODE_FILE plus_self17;
 dSTR plus_s13;
 CODE_FILE ret_val16;
 CODE_FILE plus_self18;
 dSTR plus_s14;
 CODE_FILE ret_val17;
 CODE_FILE plus_self19;
 dSTR plus_s15;
 FSTR create_self;
 FSTR ret_val18;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN defer_self;
 STR defer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L141 = INT_zero;
 STR plus_self20;
 STR plus_sarg4;
 STR ret_val19;
 STR plus_self21;
 STR plus_sarg5;
 STR ret_val20;
 STR plus_self22;
 STR plus_sarg6;
 STR ret_val21;
 STR plus_self23;
 INT plus_arg = INT_zero;
 STR ret_val22;
 STR s;
 INT str_self = INT_zero;
 STR ret_val23;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val24;
 FSTR str_self1;
 STR ret_val25;
 STR plus_self24;
 STR plus_sarg7;
 STR ret_val26;
 STR plus_self25;
 INT plus_arg1 = INT_zero;
 STR ret_val27;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val28;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val29;
 FSTR str_self3;
 STR ret_val30;
 STR plus_self26;
 STR plus_sarg8;
 STR ret_val31;
 STR plus_self27;
 STR plus_sarg9;
 STR ret_val32;
 STR plus_self28;
 STR plus_sarg10;
 STR ret_val33;
 STR plus_self29;
 STR plus_sarg11;
 STR ret_val34;
 STR plus_self30;
 INT plus_arg2 = INT_zero;
 STR ret_val35;
 STR s2;
 INT str_self4 = INT_zero;
 STR ret_val36;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val37;
 FSTR str_self5;
 STR ret_val38;
 STR plus_self31;
 STR plus_sarg12;
 STR ret_val39;
 STR plus_self32;
 INT plus_arg3 = INT_zero;
 STR ret_val40;
 STR s3;
 INT str_self6 = INT_zero;
 STR ret_val41;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val42;
 FSTR str_self7;
 STR ret_val43;
 STR plus_self33;
 STR plus_sarg13;
 STR ret_val44;
 INT L151 = INT_zero;
 STR plus_self34;
 STR plus_sarg14;
 STR ret_val45;
 STR plus_self35;
 STR plus_sarg15;
 STR ret_val46;
 STR plus_self36;
 STR plus_sarg16;
 STR ret_val47;
 STR plus_self37;
 INT plus_arg4 = INT_zero;
 STR ret_val48;
 STR s4;
 INT str_self8 = INT_zero;
 STR ret_val49;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val50;
 FSTR str_self9;
 STR ret_val51;
 STR plus_self38;
 STR plus_sarg17;
 STR ret_val52;
 STR plus_self39;
 STR plus_sarg18;
 STR ret_val53;
 STR plus_self40;
 INT plus_arg5 = INT_zero;
 STR ret_val54;
 STR s5;
 INT str_self10 = INT_zero;
 STR ret_val55;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val56;
 FSTR str_self11;
 STR ret_val57;
 STR plus_self41;
 STR plus_sarg19;
 STR ret_val58;
 STR plus_self42;
 STR plus_sarg20;
 STR ret_val59;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self43;
 STR plus_sarg21;
 STR ret_val60;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self44;
 STR plus_sarg22;
 STR ret_val61;
 STR plus_self45;
 STR plus_sarg23;
 STR ret_val62;
 STR plus_self46;
 STR plus_sarg24;
 STR ret_val63;
 STR plus_self47;
 INT plus_arg6 = INT_zero;
 STR ret_val64;
 STR s6;
 INT str_self12 = INT_zero;
 STR ret_val65;
 FSTR buf7 = ((FSTR) NULL);
 FSTR clear_self6;
 INT str_in_self6 = INT_zero;
 FSTR str_in_s6;
 FSTR ret_val66;
 FSTR str_self13;
 STR ret_val67;
 STR plus_self48;
 STR plus_sarg25;
 STR ret_val68;
 STR plus_self49;
 STR plus_sarg26;
 STR ret_val69;
 CGEN barf_self4;
 STR barf_msg4;
 PROG barf_self5;
 STR barf_msg5;
 PROG barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 STR plus_self50;
 STR plus_sarg27;
 STR ret_val70;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 STR plus_self51;
 STR plus_sarg28;
 STR ret_val71;
 STR plus_self52;
 INT plus_arg7 = INT_zero;
 STR ret_val72;
 STR s7;
 INT str_self14 = INT_zero;
 STR ret_val73;
 FSTR buf8 = ((FSTR) NULL);
 FSTR clear_self7;
 INT str_in_self7 = INT_zero;
 FSTR str_in_s7;
 FSTR ret_val74;
 FSTR str_self15;
 STR ret_val75;
 STR plus_self53;
 STR plus_sarg29;
 STR ret_val76;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val77;
 STR plus_self54;
 STR plus_sarg30;
 STR ret_val78;
 STR plus_self55;
 STR plus_sarg31;
 STR ret_val79;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self;
 STR plus_self56;
 STR plus_sarg32;
 STR ret_val80;
 STR plus_self57;
 STR plus_sarg33;
 STR ret_val81;
 STR plus_self58;
 STR plus_sarg34;
 STR ret_val82;
 CGEN runtime_error_se;
 STR runtime_error_s;
 STR plus_self59;
 STR plus_sarg35;
 STR ret_val83;
 STR plus_self60;
 STR plus_sarg36;
 STR ret_val84;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN move_out_self;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val85;
 STR plus_self61;
 STR plus_sarg37;
 STR ret_val86;
 STR plus_self62;
 STR plus_sarg38;
 STR ret_val87;
 STR plus_self63;
 STR plus_sarg39;
 STR ret_val88;
 STR plus_self64;
 STR plus_sarg40;
 STR ret_val89;
 STR plus_self65;
 STR plus_sarg41;
 STR ret_val90;
 STR plus_self66;
 STR plus_sarg42;
 STR ret_val91;
 STR plus_self67;
 STR plus_sarg43;
 STR ret_val92;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val93;
 STR plus_self68;
 STR plus_sarg44;
 STR ret_val94;
 CGEN defer_self1;
 STR defer_s1;
 CGEN comment_self;
 STR comment_com;
 STR plus_self69;
 STR plus_sarg45;
 STR ret_val95;
 STR plus_self70;
 STR plus_sarg46;
 STR ret_val96;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN ndefer_self8;
 STR ndefer_s8;
 STR plus_self71;
 STR plus_sarg47;
 STR ret_val97;
 STR plus_self72;
 STR plus_sarg48;
 STR ret_val98;
 STR plus_self73;
 STR plus_sarg49;
 STR ret_val99;
 CGEN ndefer_self9;
 STR ndefer_s9;
 STR plus_self74;
 STR plus_sarg50;
 STR ret_val100;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val101;
 STR plus_self75;
 STR plus_sarg51;
 STR ret_val102;
 STR plus_self76;
 STR plus_sarg52;
 STR ret_val103;
 STR plus_self77;
 STR plus_sarg53;
 STR ret_val104;
 STR plus_self78;
 STR plus_sarg54;
 STR ret_val105;
 STR plus_self79;
 STR plus_sarg55;
 STR ret_val106;
 STR plus_self80;
 STR plus_sarg56;
 STR ret_val107;
 STR plus_self81;
 STR plus_sarg57;
 STR ret_val108;
 STR plus_self82;
 STR plus_sarg58;
 STR ret_val109;
 CGEN ndefer_self10;
 STR ndefer_s10;
 STR plus_self83;
 STR plus_sarg59;
 STR ret_val110;
 CGEN ndefer_self11;
 STR ndefer_s11;
 STR plus_self84;
 STR plus_sarg60;
 STR ret_val1111;
 STR plus_self85;
 STR plus_sarg61;
 STR ret_val112;
 STR plus_self86;
 STR plus_sarg62;
 STR ret_val113;
 STR plus_self87;
 STR plus_sarg63;
 STR ret_val114;
 CGEN ndefer_self12;
 STR ndefer_s12;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self88;
 STR plus_sarg64;
 STR ret_val115;
 STR plus_self89;
 STR plus_sarg65;
 STR ret_val116;
 CGEN ndefer_self14;
 STR ndefer_s14;
 INT L161 = INT_zero;
 CGEN ndefer_self15;
 STR ndefer_s15;
 STR plus_self90;
 INT plus_arg8 = INT_zero;
 STR ret_val117;
 STR s8;
 INT str_self16 = INT_zero;
 STR ret_val118;
 FSTR buf9 = ((FSTR) NULL);
 FSTR clear_self8;
 INT str_in_self8 = INT_zero;
 FSTR str_in_s8;
 FSTR ret_val119;
 FSTR str_self17;
 STR ret_val120;
 STR plus_self91;
 STR plus_sarg66;
 STR ret_val121;
 STR plus_self92;
 INT plus_arg9 = INT_zero;
 STR ret_val122;
 STR s9;
 INT str_self18 = INT_zero;
 STR ret_val123;
 FSTR buf10 = ((FSTR) NULL);
 FSTR clear_self9;
 INT str_in_self9 = INT_zero;
 FSTR str_in_s9;
 FSTR ret_val124;
 FSTR str_self19;
 STR ret_val125;
 STR plus_self93;
 STR plus_sarg67;
 STR ret_val126;
 CGEN ndefer_self16;
 STR ndefer_s16;
 STR plus_self94;
 INT plus_arg10 = INT_zero;
 STR ret_val127;
 STR s10;
 INT str_self20 = INT_zero;
 STR ret_val128;
 FSTR buf11 = ((FSTR) NULL);
 FSTR clear_self10;
 INT str_in_self10 = INT_zero;
 FSTR str_in_s10;
 FSTR ret_val129;
 FSTR str_self21;
 STR ret_val130;
 STR plus_self95;
 STR plus_sarg68;
 STR ret_val131;
 STR plus_self96;
 INT plus_arg11 = INT_zero;
 STR ret_val132;
 STR s11;
 INT str_self22 = INT_zero;
 STR ret_val133;
 FSTR buf12 = ((FSTR) NULL);
 FSTR clear_self11;
 INT str_in_self11 = INT_zero;
 FSTR str_in_s11;
 FSTR ret_val134;
 FSTR str_self23;
 STR ret_val135;
 STR plus_self97;
 STR plus_sarg69;
 STR ret_val136;
 CGEN ndefer_self17;
 STR ndefer_s17;
 INT L171 = INT_zero;
 CGEN ndefer_self18;
 STR ndefer_s18;
 STR plus_self98;
 INT plus_arg12 = INT_zero;
 STR ret_val137;
 STR s12;
 INT str_self24 = INT_zero;
 STR ret_val138;
 FSTR buf13 = ((FSTR) NULL);
 FSTR clear_self12;
 INT str_in_self12 = INT_zero;
 FSTR str_in_s12;
 FSTR ret_val139;
 FSTR str_self25;
 STR ret_val140;
 STR plus_self99;
 STR plus_sarg70;
 STR ret_val141;
 STR plus_self100;
 INT plus_arg13 = INT_zero;
 STR ret_val142;
 STR s13;
 INT str_self26 = INT_zero;
 STR ret_val143;
 FSTR buf14 = ((FSTR) NULL);
 FSTR clear_self13;
 INT str_in_self13 = INT_zero;
 FSTR str_in_s13;
 FSTR ret_val144;
 FSTR str_self27;
 STR ret_val145;
 STR plus_self101;
 STR plus_sarg71;
 STR ret_val146;
 CGEN ndefer_self19;
 STR ndefer_s19;
 STR plus_self102;
 INT plus_arg14 = INT_zero;
 STR ret_val147;
 STR s14;
 INT str_self28 = INT_zero;
 STR ret_val148;
 FSTR buf15 = ((FSTR) NULL);
 FSTR clear_self14;
 INT str_in_self14 = INT_zero;
 FSTR str_in_s14;
 FSTR ret_val149;
 FSTR str_self29;
 STR ret_val150;
 STR plus_self103;
 STR plus_sarg72;
 STR ret_val151;
 CGEN ndefer_self20;
 STR ndefer_s20;
 STR plus_self104;
 INT plus_arg15 = INT_zero;
 STR ret_val152;
 STR s15;
 INT str_self30 = INT_zero;
 STR ret_val153;
 FSTR buf16 = ((FSTR) NULL);
 FSTR clear_self15;
 INT str_in_self15 = INT_zero;
 FSTR str_in_s15;
 FSTR ret_val154;
 FSTR str_self31;
 STR ret_val155;
 STR plus_self105;
 STR plus_sarg73;
 STR ret_val156;
 CGEN defer_self2;
 STR defer_s2;
 CGEN ndefer_self21;
 STR ndefer_s21;
 CGEN comment_self2;
 STR comment_com2;
 STR plus_self106;
 STR plus_sarg74;
 STR ret_val157;
 STR plus_self107;
 STR plus_sarg75;
 STR ret_val158;
 CGEN ndefer_self22;
 STR ndefer_s22;
 CGEN ndefer_self23;
 STR ndefer_s23;
 CGEN comment_self3;
 STR comment_com3;
 STR plus_self108;
 STR plus_sarg76;
 STR ret_val159;
 STR plus_self109;
 STR plus_sarg77;
 STR ret_val160;
 CGEN ndefer_self24;
 STR ndefer_s24;
 INT L181 = INT_zero;
 STR plus_self110;
 INT plus_arg16 = INT_zero;
 STR ret_val161;
 STR s16;
 INT str_self32 = INT_zero;
 STR ret_val162;
 FSTR buf17 = ((FSTR) NULL);
 FSTR clear_self16;
 INT str_in_self16 = INT_zero;
 FSTR str_in_s16;
 FSTR ret_val163;
 FSTR str_self33;
 STR ret_val164;
 STR plus_self111;
 STR plus_sarg78;
 STR ret_val165;
 CGEN ndefer_self25;
 STR ndefer_s25;
 STR plus_self112;
 INT plus_arg17 = INT_zero;
 STR ret_val166;
 STR s17;
 INT str_self34 = INT_zero;
 STR ret_val167;
 FSTR buf18 = ((FSTR) NULL);
 FSTR clear_self17;
 INT str_in_self17 = INT_zero;
 FSTR str_in_s17;
 FSTR ret_val168;
 FSTR str_self35;
 STR ret_val169;
 STR plus_self113;
 STR plus_sarg79;
 STR ret_val170;
 CGEN defer_self3;
 STR defer_s3;
 INT L191 = INT_zero;
 STR plus_self114;
 INT plus_arg18 = INT_zero;
 STR ret_val171;
 STR s18;
 INT str_self36 = INT_zero;
 STR ret_val172;
 FSTR buf19 = ((FSTR) NULL);
 FSTR clear_self18;
 INT str_in_self18 = INT_zero;
 FSTR str_in_s18;
 FSTR ret_val173;
 FSTR str_self37;
 STR ret_val174;
 STR plus_self115;
 STR plus_sarg80;
 STR ret_val175;
 CGEN ndefer_self26;
 STR ndefer_s26;
 STR plus_self116;
 INT plus_arg19 = INT_zero;
 STR ret_val176;
 STR s19;
 INT str_self38 = INT_zero;
 STR ret_val177;
 FSTR buf20 = ((FSTR) NULL);
 FSTR clear_self19;
 INT str_in_self19 = INT_zero;
 FSTR str_in_s19;
 FSTR ret_val178;
 FSTR str_self39;
 STR ret_val179;
 STR plus_self117;
 STR plus_sarg81;
 STR ret_val180;
 CGEN defer_self4;
 STR defer_s4;
 CGEN comment_self4;
 STR comment_com4;
 STR plus_self118;
 STR plus_sarg82;
 STR ret_val181;
 STR plus_self119;
 STR plus_sarg83;
 STR ret_val182;
 CGEN ndefer_self27;
 STR ndefer_s27;
 ARRAYSTR create_self1;
 INT create_n = INT_zero;
 ARRAYSTR ret_val183;
 STR plus_self120;
 STR plus_sarg84;
 STR ret_val184;
 STR plus_self121;
 STR plus_sarg85;
 STR ret_val185;
 STR plus_self122;
 STR plus_sarg86;
 STR ret_val186;
 CGEN ndefer_self28;
 STR ndefer_s28;
 STR plus_self123;
 STR plus_sarg87;
 STR ret_val187;
 STR plus_self124;
 STR plus_sarg88;
 STR ret_val188;
 CGEN ndefer_self29;
 STR ndefer_s29;
 CGEN ndefer_self30;
 STR ndefer_s30;
 STR plus_self125;
 STR plus_sarg89;
 STR ret_val189;
 CGEN ndefer_self31;
 STR ndefer_s31;
 CGEN ndefer_self32;
 STR ndefer_s32;
 STR plus_self126;
 STR plus_sarg90;
 STR ret_val190;
 STR plus_self127;
 STR plus_sarg91;
 STR ret_val191;
 CGEN ndefer_self33;
 STR ndefer_s33;
 CGEN ndefer_self34;
 STR ndefer_s34;
 CGEN ndefer_self35;
 STR ndefer_s35;
 CODE_FILE plus_self128;
 dSTR plus_s16;
 CODE_FILE ret_val192;
 CODE_FILE plus_self129;
 dSTR plus_s17;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 BOOL L20;
 INT L22;
 BOOL L231_;
 extern STR callb;
 dTP L24;
 CODE_FILE L25;
 extern STR Itersi136016895;
 extern STR Intern28965746;
 CODE_FILE L26;
 CODE_FILE L27;
 extern STR S_iter_ob2;
 extern STR S_iter_ob_struct1;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L321_;
 FSTR L33;
 dSTR L34;
 dSTR L35;
 OB L36;
 FSTR L37;
 dSTR L38;
 extern STR void6;
 FSTR L39;
 dSTR L40;
 BOOL L42;
 BOOL L431_;
 FSTR L44;
 dSTR L45;
 extern STR S_call_function1;
 BOOL L46;
 BOOL L471_;
 FSTR L48;
 dSTR L49;
 dSTR L50;
 OB L52;
 BOOL L53;
 BOOL L541_;
 FSTR L55;
 dSTR L56;
 BOOL L57;
 BOOL L581_;
 FSTR L59;
 dSTR L60;
 extern STR S_iter_obf;
 FSTR L62;
 dSTR L63;
 extern STR struct16215823581;
 FSTR L64;
 dSTR L65;
 extern STR name26;
 FSTR L66;
 dSTR L67;
 extern STR name2;
 BOOL L68;
 BOOL L691_;
 FSTR L70;
 dSTR L72;
 FSTR L73;
 dSTR L74;
 BOOL L751_;
 BOOL L76;
 BOOL L771_;
 extern STR name60;
 BOOL L78;
 BOOL L791_;
 FSTR L80;
 dSTR L82;
 BOOL L83;
 BOOL L841_;
 FSTR L85;
 dSTR L86;
 dSTR L87;
 OB L88;
 BOOL L89;
 BOOL L901_;
 FSTR L92;
 dSTR L93;
 extern STR ret_val2111;
 FSTR L94;
 dSTR L95;
 FSTR L96;
 INT L97;
 OB L98;
 INT L99;
 extern STR name1;
 extern STR name1;
 BOOL L100;
 BOOL L102;
 BOOL L1031_;
 ARRAYINT L104;
 INT L1051_;
 INT L106;
 BOOL L1071_;
 extern STR bound_arg0;
 extern STR oncearg0;
 extern STR switch81042052;
 extern STR case1gotostate1;
 extern STR state02;
 INT L1081_br;
 INT aL1081_;
 INT L1091_;
 INT L110;
 INT L111;
 BOOL L1121_;
 BOOL L113;
 BOOL L1141_;
 BOOL L115;
 BOOL L116;
 BOOL L1171_;
 ARRAYBOOL L118;
 INT L1191_;
 INT L120;
 BOOL L1221_;
 extern STR f5;
 extern STR hotarg;
 INT L1231_;
 BOOL L124;
 BOOL L1251_;
 extern STR fbound_arg;
 BOOL L126;
 BOOL L1271_;
 extern STR name13;
 extern STR f5;
 extern STR oncearg;
 INT L1281_;
 BOOL L129;
 BOOL L1301_;
 extern STR fbound_arg;
 BOOL L132;
 BOOL L1331_;
 extern STR name13;
 INT L1341_;
 INT L1351_;
 extern STR oncearg0;
 INT L1361_br;
 INT aL1361_;
 INT L1371_;
 INT L138;
 INT L139;
 BOOL L1401_;
 BOOL L142;
 BOOL L1431_;
 BOOL L144;
 BOOL L145;
 BOOL L1461_;
 ARRAYBOOL L147;
 INT L1481_;
 INT L149;
 BOOL L1501_;
 extern STR f5;
 extern STR hotarg;
 INT L1521_;
 BOOL L153;
 BOOL L1541_;
 extern STR name36;
 BOOL L155;
 BOOL L156;
 BOOL L1571_;
 BOOL L1581_;
 extern STR fhotarg;
 BOOL L159;
 BOOL L1601_;
 extern STR name13;
 extern STR Ingene409399871;
 extern STR argume2028503219;
 extern STR Intern28965746;
 extern STR f5;
 extern STR oncearg;
 INT L1621_;
 BOOL L163;
 BOOL L1641_;
 extern STR name36;
 BOOL L165;
 BOOL L166;
 BOOL L1671_;
 BOOL L1681_;
 extern STR Ingene409399871;
 extern STR argume2028503200;
 extern STR Intern28965746;
 extern STR foncearg;
 BOOL L169;
 BOOL L1701_;
 extern STR name13;
 INT L1721_;
 INT L1731_;
 INT L1741_;
 dTP L175;
 BOOL L176;
 BOOL L177;
 BOOL L1781_;
 extern STR iff1;
 extern STR NULL6;
 INT L179;
 INT L1801_;
 extern STR Dispatchedcallto;
 extern STR onvoidselfin;
 extern STR FATAL1;
 extern STR name95;
 INT L182;
 INT L1831_;
 extern STR name4;
 extern STR fiter_frame;
 extern STR S_frame3;
 extern STR name85;
 extern STR TAGf;
 extern STR alloc_frame;
 extern STR fiter_2029027530;
 extern STR S_frame1;
 extern STR Initia393638030;
 extern STR name54;
 extern STR name79;
 BOOL L1841_;
 extern STR fiter_1590862403;
 extern STR ZALLOCf;
 extern STR size4;
 extern STR name80;
 extern STR memcpy1;
 extern STR f6;
 extern STR f6;
 extern STR size4;
 extern STR state01;
 extern STR f7;
 extern STR name36;
 extern STR name16;
 extern STR Fillin1044524838;
 extern STR name54;
 extern STR name79;
 INT L1851_br;
 INT aL1851_;
 INT L1861_;
 INT L187;
 INT L188;
 BOOL L1891_;
 extern STR fiter_612822643;
 extern STR fiter_framearg;
 BOOL L190;
 BOOL L1921_;
 extern STR foncearg1;
 BOOL L193;
 BOOL L1941_;
 extern STR name16;
 ARRAYBOOL L195;
 INT L1961_;
 INT L197;
 BOOL L1981_;
 BOOL L1991_;
 extern STR fiter_framearg;
 BOOL L200;
 BOOL L2011_;
 extern STR foncearg1;
 BOOL L202;
 BOOL L2031_;
 extern STR name16;
 INT L2041_;
 INT L2051_br;
 INT aL2051_;
 INT L2061_;
 INT L207;
 INT L208;
 BOOL L2091_;
 BOOL L2101_;
 extern STR fiter_907730610;
 extern STR fiter_framearg;
 BOOL L211;
 BOOL L2121_;
 extern STR fbound_arg;
 BOOL L213;
 BOOL L2141_;
 extern STR name16;
 ARRAYBOOL L215;
 INT L2161_;
 INT L217;
 BOOL L2181_;
 BOOL L2191_;
 extern STR fiter_framearg;
 BOOL L220;
 BOOL L2211_;
 extern STR f8;
 extern STR bound_arg1;
 BOOL L222;
 BOOL L2231_;
 extern STR name16;
 INT L2241_;
 extern STR state111;
 extern STR Sethot404575151;
 extern STR name54;
 extern STR name79;
 extern STR Fillin1000309002;
 extern STR name54;
 extern STR name79;
 BOOL L225;
 BOOL L2261_;
 INT L2271_br;
 INT aL2271_;
 INT L2281_;
 INT L229;
 INT L230;
 BOOL L2321_;
 BOOL L233;
 BOOL L2341_;
 ARRAYBOOL L235;
 INT L2361_;
 INT L237;
 BOOL L2381_;
 extern STR fiter_framearg;
 BOOL L239;
 BOOL L2401_;
 extern STR f8;
 extern STR hotarg1;
 BOOL L241;
 BOOL L2421_;
 extern STR name16;
 INT L2431_;
 INT L2441_br;
 INT aL2441_;
 INT L2451_;
 INT L246;
 INT L247;
 BOOL L2481_;
 BOOL L249;
 BOOL L2501_;
 ARRAYBOOL L251;
 INT L2521_;
 INT L253;
 BOOL L2541_;
 extern STR fiter_framearg;
 BOOL L255;
 BOOL L2561_;
 extern STR f8;
 extern STR bound_arg1;
 BOOL L257;
 BOOL L2581_;
 extern STR name16;
 INT L2591_;
 extern STR calliter;
 extern STR name54;
 extern STR name79;
 ARRAYSTR L260;
 INT L261;
 OB L262;
 INT L263;
 extern STR f7;
 STR L264;
 extern STR fiter_frame1;
 extern STR name16;
 BOOL L267;
 BOOL L2681_;
 extern STR ret_val3111;
 extern STR fstatef;
 extern STR state1111;
 extern STR fstate886545549;
 extern STR returnret_val;
 extern STR name162;
 extern STR fstatef;
 extern STR state1111;
 extern STR fstate886545549;
 extern STR return7;
 BOOL L269;
 BOOL L2701_;
 FSTR L271;
 dSTR L272;
 extern STR name163;
 FSTR L273;
 dSTR L274;
 BOOL L275;
 BOOL L2761_;
 while (1) {
  if ((ATTR(self,bnd_iter_creates)==((FLISTA1893461511) NULL))) {
   L20 = TRUE;
  } else {
   is_empty_self = ATTR(self,bnd_iter_creates);
   L22 = FLISTA919540447(is_empty_self);
   L231_=(L22)==(0); 
   ret_val = L231_;
   L20 = ret_val;
  }
  if (L20) {
   goto after_loop;
  }
  e = FLISTA1230733197(ATTR(self,bnd_iter_creates));
  binding_iter_clo = FALSE;
  ident1 = IDENT_1150413730(IDENT_zero, ((STR) &callb));
  is_eq_self = ATTR(ATTR(e,fun),name1);
  is_eq_i = ident1;
  ret_val1 = (is_eq_self.str==is_eq_i.str);
  if (ret_val1) {
   binding_iter_clo = TRUE;
  }
  uses_bnd_iter_cr = ATTR(self,code_c);
  uses_bnd_iter_cr1 = e;
  SATTR(uses_bnd_iter_cr,bnd_iter_creates,FLISTA80425519(ATTR(uses_bnd_iter_cr,bnd_iter_creates), uses_bnd_iter_cr1));
  L24 = ATTR(ATTR(e,fun),tp);
  if ((*dTP_is1811059018[TAG(L24)])(L24)) {
   L25 = ATTR(self,code_c);
   CODE_F525364269(L25, ((dLAYOUT) ABSTRA1493033801(((ABSTRA151498155) NULL), ATTR(e,fun), ATTR(self,prog))));
  }
  else {
   if (binding_iter_clo) {
   }
   else {
    iframe = FMAPSI2016001247(ATTR(self,itersig_map), ATTR(e,fun));
    if ((iframe==((AM_ROUT_DEF) NULL))) {
     barf_self = self;
     plus_self = ((STR) &Itersi136016895);
     plus_sarg = ATTR(ATTR(e,fun),str);
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     barf_msg = ret_val2;
     barf_self1 = ATTR(barf_self,prog);
     barf_msg1 = barf_msg;
     barf_at_self = barf_self1;
     barf_at_msg = barf_msg1;
     barf_at_at = ((dPROG_ERR) NULL);
     PROG_e176405615(barf_at_self, barf_at_at);
     plus_self1 = ((STR) &Intern28965746);
     plus_sarg1 = barf_at_msg;
     ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
     PROG_err_STR(barf_at_self, ret_val3);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    }
    L26 = ATTR(self,code_c);
    CODE_F525364269(L26, ((dLAYOUT) FRAME_248172754(((FRAME_LAYOUT) NULL), iframe, ATTR(self,prog))));
   }
  }
  L27 = ATTR(self,code_c);
  CODE_F525364269(L27, ((dLAYOUT) BOUND_11324444(((BOUND_1007407331) NULL), e, ATTR(self,prog))));
  mang_self = self;
  mang_ob = ((OB) e);
  ret_val4 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  name111 = ret_val4;
  forbid_self = self;
  plus_self2 = name111;
  plus_sarg2 = ((STR) &S_iter_ob2);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
  forbid_s = ret_val5;
  forbid_self1 = ATTR(forbid_self,mangler);
  forbid_s1 = forbid_s;
  SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
  forbid_self2 = self;
  plus_self3 = name111;
  plus_sarg3 = ((STR) &S_iter_ob_struct1);
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
  forbid_s2 = ret_val6;
  forbid_self3 = ATTR(forbid_self2,mangler);
  forbid_s3 = forbid_s2;
  SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
  L28 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
  L291_=!(L28); 
  if (L291_) {
   plus_self4 = ATTR(self,code_c);
   mang_self1 = self;
   mang_ob1 = ((OB) ATTR(ATTR(e,fun),ret));
   ret_val7 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
   plus_s = ((dSTR) ret_val7);
   L30 = (plus_s==((dSTR) NULL));
   L321_=!(L30); 
   if (L321_) {
    L33 = ATTR(plus_self4,ntext);
    L34 = plus_s;
    SATTR(plus_self4,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
   }
   ret_val8 = plus_self4;
   plus_self5 = ret_val8;
   L36=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   memset(L36,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L36)->header.tag=CHAR_tag;
   L35 = (dSTR)((CHAR_boxed) L36);
   ((CHAR_boxed) L35)->immutable_part = ' ';
   plus_s1 = L35;
   L37 = ATTR(plus_self5,ntext);
   L38 = plus_s1;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L37, (*dSTR_strrSTR[TAG(L38)])(L38)));
  }
  else {
   plus_self6 = ATTR(self,code_c);
   plus_s2 = ((dSTR) ((STR) &void6));
   L39 = ATTR(plus_self6,ntext);
   L40 = plus_s2;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L39, (*dSTR_strrSTR[TAG(L40)])(L40)));
  }
  plus_self7 = ATTR(self,code_c);
  plus_s3 = ((dSTR) name111);
  L42 = (plus_s3==((dSTR) NULL));
  L431_=!(L42); 
  if (L431_) {
   L44 = ATTR(plus_self7,ntext);
   L45 = plus_s3;
   SATTR(plus_self7,ntext,FSTR_p1752847026(L44, (*dSTR_strrSTR[TAG(L45)])(L45)));
  }
  ret_val9 = plus_self7;
  plus_self8 = ret_val9;
  plus_s4 = ((dSTR) ((STR) &S_call_function1));
  L46 = (plus_s4==((dSTR) NULL));
  L471_=!(L46); 
  if (L471_) {
   L48 = ATTR(plus_self8,ntext);
   L49 = plus_s4;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
  }
  ret_val10 = plus_self8;
  plus_self9 = ret_val10;
  L52=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L52==NULL) FATAL("Unable to allocate more memory");
  memset(L52,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L52)->header.tag=CHAR_tag;
  L50 = (dSTR)((CHAR_boxed) L52);
  ((CHAR_boxed) L50)->immutable_part = '(';
  plus_s5 = L50;
  L53 = (plus_s5==((dSTR) NULL));
  L541_=!(L53); 
  if (L541_) {
   L55 = ATTR(plus_self9,ntext);
   L56 = plus_s5;
   SATTR(plus_self9,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
  }
  ret_val11 = plus_self9;
  plus_self10 = ret_val11;
  plus_s6 = ((dSTR) name111);
  L57 = (plus_s6==((dSTR) NULL));
  L581_=!(L57); 
  if (L581_) {
   L59 = ATTR(plus_self10,ntext);
   L60 = plus_s6;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
  }
  ret_val12 = plus_self10;
  plus_self11 = ret_val12;
  plus_s7 = ((dSTR) ((STR) &S_iter_obf));
  L62 = ATTR(plus_self11,ntext);
  L63 = plus_s7;
  SATTR(plus_self11,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
  if (ATTR(self,func_tables)) {
   plus_self12 = ATTR(self,code_c);
   plus_s8 = ((dSTR) ((STR) &struct16215823581));
   L64 = ATTR(plus_self12,ntext);
   L65 = plus_s8;
   SATTR(plus_self12,ntext,FSTR_p1752847026(L64, (*dSTR_strrSTR[TAG(L65)])(L65)));
  }
  plus_self13 = ATTR(self,code_c);
  plus_s9 = ((dSTR) ((STR) &name26));
  L66 = ATTR(plus_self13,ntext);
  L67 = plus_s9;
  SATTR(plus_self13,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
  plus_self14 = ATTR(self,code_c);
  plus_s10 = ((dSTR) ((STR) &name2));
  L68 = (plus_s10==((dSTR) NULL));
  L691_=!(L68); 
  if (L691_) {
   L70 = ATTR(plus_self14,ntext);
   L72 = plus_s10;
   SATTR(plus_self14,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
  }
  ret_val13 = plus_self14;
  plus_self15 = ret_val13;
  plus_s11 = ((dSTR) CGEN_eolrSTR(self));
  L73 = ATTR(plus_self15,ntext);
  L74 = plus_s11;
  SATTR(plus_self15,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
  L751_=!(binding_iter_clo); 
  if (L751_) {
   CODE_F1291349628(ATTR(self,code_c), ATTR(e,fun));
  }
  L76 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
  L771_=!(L76); 
  if (L771_) {
   plus_self16 = ATTR(self,code_c);
   plus_s12 = ((dSTR) ((STR) &name60));
   L78 = (plus_s12==((dSTR) NULL));
   L791_=!(L78); 
   if (L791_) {
    L80 = ATTR(plus_self16,ntext);
    L82 = plus_s12;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L82)])(L82)));
   }
   ret_val14 = plus_self16;
   plus_self17 = ret_val14;
   mang_self2 = self;
   mang_ob2 = ((OB) ATTR(ATTR(e,fun),ret));
   ret_val15 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
   plus_s13 = ((dSTR) ret_val15);
   L83 = (plus_s13==((dSTR) NULL));
   L841_=!(L83); 
   if (L841_) {
    L85 = ATTR(plus_self17,ntext);
    L86 = plus_s13;
    SATTR(plus_self17,ntext,FSTR_p1752847026(L85, (*dSTR_strrSTR[TAG(L86)])(L86)));
   }
   ret_val16 = plus_self17;
   plus_self18 = ret_val16;
   L88=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L88==NULL) FATAL("Unable to allocate more memory");
   memset(L88,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L88)->header.tag=CHAR_tag;
   L87 = (dSTR)((CHAR_boxed) L88);
   ((CHAR_boxed) L87)->immutable_part = ' ';
   plus_s14 = L87;
   L89 = (plus_s14==((dSTR) NULL));
   L901_=!(L89); 
   if (L901_) {
    L92 = ATTR(plus_self18,ntext);
    L93 = plus_s14;
    SATTR(plus_self18,ntext,FSTR_p1752847026(L92, (*dSTR_strrSTR[TAG(L93)])(L93)));
   }
   ret_val17 = plus_self18;
   plus_self19 = ret_val17;
   plus_s15 = ((dSTR) ((STR) &ret_val2111));
   L94 = ATTR(plus_self19,ntext);
   L95 = plus_s15;
   SATTR(plus_self19,ntext,FSTR_p1752847026(L94, (*dSTR_strrSTR[TAG(L95)])(L95)));
  }
  create_self = ((FSTR) NULL);
  L97 = 16;
  L99=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L97)*sizeof(CHAR);
  L98=ZALLOC_LEAF_BIG(L99);
  if (L98==NULL) FATAL("Unable to allocate more memory");
  memset(L98,0,L99);
  ((OB)L98)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L98)->header.destroyed=0;
#endif

  L96 = ((FSTR) L98);
  L96->asize = L97;
  ret_val18 = L96;
  SATTR(self,routine_code,ret_val18);
  state011 = ((STR) &name1);
  state11 = ((STR) &name1);
  L102 = (ATTR(e,bnd_args)==((ARRAYINT) NULL));
  L1031_=!(L102); 
  if (L1031_) {
   L104 = ATTR(e,bnd_args);
   L1051_=ARR((ARRAYINT)L104,0); 
   L106 = L1051_;
   L1071_=(L106)==(0); 
   L100 = L1071_;
  } else {
   L100 = FALSE;
  }
  if (L100) {
   self_str = ((STR) &bound_arg0);
  }
  else {
   self_str = ((STR) &oncearg0);
  }
  ndefer_self = self;
  ndefer_s = ((STR) &switch81042052);
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  defer_self = self;
  defer_s = ((STR) &case1gotostate1);
  SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
  ndefer_self1 = self;
  ndefer_s1 = ((STR) &state02);
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  if (binding_iter_clo) {
   type_of_bnd_crea = ATTR(e,tp_at);
   switch (TAG(type_of_bnd_crea)) {
    case TP_ITER_tag:
     arg_mode = ATTR(((TP_ITER) type_of_bnd_crea),hot);
     args_list = ATTR(((TP_ITER) type_of_bnd_crea),args); break;
    default: ;
     FATAL("No applicable type in typecase\nin CGEN::generate_bnd_iter_stubs\n./Back/cgen.sa:2224:20");
   }
   i_arg = 0;
   {
    BOOL f_L1081_ = TRUE;
    BOOL f_L1091_ = TRUE;
    /* loop index variable */
    L141 = 0;
    L11 = ATTR(e,bnd_args);
    L21 = 0;
    L1081_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
    L1091_=L21-1; 
    while (1) {
     if(L141>=L1081_br)  goto after_loop1; 
     aL1081_=ARR((ARRAYINT)L11,L141); 
     a = aL1081_;
     L1091_++; 
     i = L1091_;
     L1121_=(a)==(0); 
     L113 = L1121_;
     L1141_=!(L113); 
     if (L1141_) {
      L116 = (ATTR(ATTR(e,fun),hot)==((ARRAYBOOL) NULL));
      L1171_=!(L116); 
      if (L1171_) {
       L118 = ATTR(ATTR(e,fun),hot);
       L1191_=INTMINUS(a,1); 
       L120 = L1191_;
       L1221_=ARR((ARRAYBOOL)L118,L120); 
       L115 = L1221_;
      } else {
       L115 = FALSE;
      }
      if (L115) {
       plus_self20 = state11;
       plus_sarg4 = ((STR) &f5);
       ret_val19 = STR_ap2004550586(plus_self20, plus_sarg4);
       plus_self21 = ret_val19;
       plus_sarg5 = self_str;
       ret_val20 = STR_ap2004550586(plus_self21, plus_sarg5);
       plus_self22 = ret_val20;
       plus_sarg6 = ((STR) &hotarg);
       ret_val21 = STR_ap2004550586(plus_self22, plus_sarg6);
       plus_self23 = ret_val21;
       L1231_=INTMINUS(a,1); 
       plus_arg = L1231_;
       str_self = plus_arg;
       clear_self = buf1;
       L124 = (clear_self==((FSTR) NULL));
       L1251_=!(L124); 
       if (L1251_) {
        SATTR(clear_self,loc,0);
       }
       str_in_self = str_self;
       str_in_s = buf1;
       ret_val24 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
       buf1 = ret_val24;
       str_self1 = buf1;
       ret_val25 = STR_fr1097270334(((STR) NULL), str_self1);
       ret_val23 = ret_val25;
       s = ret_val23;
       ret_val22 = STR_ap1077157958(plus_self23, s, TRUE);
       plus_self24 = ret_val22;
       plus_sarg7 = ((STR) &fbound_arg);
       ret_val26 = STR_ap2004550586(plus_self24, plus_sarg7);
       plus_self25 = ret_val26;
       plus_arg1 = i;
       str_self2 = plus_arg1;
       clear_self1 = buf2;
       L126 = (clear_self1==((FSTR) NULL));
       L1271_=!(L126); 
       if (L1271_) {
        SATTR(clear_self1,loc,0);
       }
       str_in_self1 = str_self2;
       str_in_s1 = buf2;
       ret_val29 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
       buf2 = ret_val29;
       str_self3 = buf2;
       ret_val30 = STR_fr1097270334(((STR) NULL), str_self3);
       ret_val28 = ret_val30;
       s1 = ret_val28;
       ret_val27 = STR_ap1077157958(plus_self25, s1, TRUE);
       plus_self26 = ret_val27;
       plus_sarg8 = ((STR) &name13);
       ret_val31 = STR_ap2004550586(plus_self26, plus_sarg8);
       state11 = ret_val31;
      }
      else {
       plus_self27 = state011;
       plus_sarg9 = ((STR) &f5);
       ret_val32 = STR_ap2004550586(plus_self27, plus_sarg9);
       plus_self28 = ret_val32;
       plus_sarg10 = self_str;
       ret_val33 = STR_ap2004550586(plus_self28, plus_sarg10);
       plus_self29 = ret_val33;
       plus_sarg11 = ((STR) &oncearg);
       ret_val34 = STR_ap2004550586(plus_self29, plus_sarg11);
       plus_self30 = ret_val34;
       L1281_=INTMINUS(a,1); 
       plus_arg2 = L1281_;
       str_self4 = plus_arg2;
       clear_self2 = buf3;
       L129 = (clear_self2==((FSTR) NULL));
       L1301_=!(L129); 
       if (L1301_) {
        SATTR(clear_self2,loc,0);
       }
       str_in_self2 = str_self4;
       str_in_s2 = buf3;
       ret_val37 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
       buf3 = ret_val37;
       str_self5 = buf3;
       ret_val38 = STR_fr1097270334(((STR) NULL), str_self5);
       ret_val36 = ret_val38;
       s2 = ret_val36;
       ret_val35 = STR_ap1077157958(plus_self30, s2, TRUE);
       plus_self31 = ret_val35;
       plus_sarg12 = ((STR) &fbound_arg);
       ret_val39 = STR_ap2004550586(plus_self31, plus_sarg12);
       plus_self32 = ret_val39;
       plus_arg3 = i;
       str_self6 = plus_arg3;
       clear_self3 = buf4;
       L132 = (clear_self3==((FSTR) NULL));
       L1331_=!(L132); 
       if (L1331_) {
        SATTR(clear_self3,loc,0);
       }
       str_in_self3 = str_self6;
       str_in_s3 = buf4;
       ret_val42 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
       buf4 = ret_val42;
       str_self7 = buf4;
       ret_val43 = STR_fr1097270334(((STR) NULL), str_self7);
       ret_val41 = ret_val43;
       s3 = ret_val41;
       ret_val40 = STR_ap1077157958(plus_self32, s3, TRUE);
       plus_self33 = ret_val40;
       plus_sarg13 = ((STR) &name13);
       ret_val44 = STR_ap2004550586(plus_self33, plus_sarg13);
       state011 = ret_val44;
      }
      L1341_=INTPLUS(i_arg,1); 
      i_arg = L1341_;
     }
     L1351_=INTPLUS(L141,1); 
     L141 = L1351_;
    }
   }
   after_loop1: ;
   arg_cnt = 0;
   if (STR_is111530248(self_str, ((STR) &oncearg0))) {
    arg_cnt = 1;
   }
   {
    BOOL f_L1361_ = TRUE;
    BOOL f_L1371_ = TRUE;
    /* loop index variable */
    L151 = 0;
    L31 = ATTR(e,unbnd_args);
    L41 = 0;
    L1361_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
    L1371_=L41-1; 
    while (1) {
     if(L151>=L1361_br)  goto after_loop2; 
     aL1361_=ARR((ARRAYINT)L31,L151); 
     a11 = aL1361_;
     L1371_++; 
     i1 = L1371_;
     L1401_=(a11)==(0); 
     L142 = L1401_;
     L1431_=!(L142); 
     if (L1431_) {
      L145 = (ATTR(ATTR(e,fun),hot)==((ARRAYBOOL) NULL));
      L1461_=!(L145); 
      if (L1461_) {
       L147 = ATTR(ATTR(e,fun),hot);
       L1481_=INTMINUS(a11,1); 
       L149 = L1481_;
       L1501_=ARR((ARRAYBOOL)L147,L149); 
       L144 = L1501_;
      } else {
       L144 = FALSE;
      }
      if (L144) {
       plus_self34 = state11;
       plus_sarg14 = ((STR) &f5);
       ret_val45 = STR_ap2004550586(plus_self34, plus_sarg14);
       plus_self35 = ret_val45;
       plus_sarg15 = self_str;
       ret_val46 = STR_ap2004550586(plus_self35, plus_sarg15);
       plus_self36 = ret_val46;
       plus_sarg16 = ((STR) &hotarg);
       ret_val47 = STR_ap2004550586(plus_self36, plus_sarg16);
       plus_self37 = ret_val47;
       L1521_=INTMINUS(a11,1); 
       plus_arg4 = L1521_;
       str_self8 = plus_arg4;
       clear_self4 = buf5;
       L153 = (clear_self4==((FSTR) NULL));
       L1541_=!(L153); 
       if (L1541_) {
        SATTR(clear_self4,loc,0);
       }
       str_in_self4 = str_self8;
       str_in_s4 = buf5;
       ret_val50 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
       buf5 = ret_val50;
       str_self9 = buf5;
       ret_val51 = STR_fr1097270334(((STR) NULL), str_self9);
       ret_val49 = ret_val51;
       s4 = ret_val49;
       ret_val48 = STR_ap1077157958(plus_self37, s4, TRUE);
       plus_self38 = ret_val48;
       plus_sarg17 = ((STR) &name36);
       ret_val52 = STR_ap2004550586(plus_self38, plus_sarg17);
       state11 = ret_val52;
       L156 = (arg_mode==((ARRAYBOOL) NULL));
       L1571_=!(L156); 
       if (L1571_) {
        L1581_=ARR((ARRAYBOOL)arg_mode,i1); 
        L155 = L1581_;
       } else {
        L155 = FALSE;
       }
       if (L155) {
        plus_self39 = state11;
        plus_sarg18 = ((STR) &fhotarg);
        ret_val53 = STR_ap2004550586(plus_self39, plus_sarg18);
        plus_self40 = ret_val53;
        plus_arg5 = arg_cnt;
        str_self10 = plus_arg5;
        clear_self5 = buf6;
        L159 = (clear_self5==((FSTR) NULL));
        L1601_=!(L159); 
        if (L1601_) {
         SATTR(clear_self5,loc,0);
        }
        str_in_self5 = str_self10;
        str_in_s5 = buf6;
        ret_val56 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
        buf6 = ret_val56;
        str_self11 = buf6;
        ret_val57 = STR_fr1097270334(((STR) NULL), str_self11);
        ret_val55 = ret_val57;
        s5 = ret_val55;
        ret_val54 = STR_ap1077157958(plus_self40, s5, TRUE);
        plus_self41 = ret_val54;
        plus_sarg19 = ((STR) &name13);
        ret_val58 = STR_ap2004550586(plus_self41, plus_sarg19);
        state11 = ret_val58;
       }
       else {
        barf_self2 = self;
        plus_self42 = ((STR) &Ingene409399871);
        plus_sarg20 = ((STR) &argume2028503219);
        ret_val59 = STR_ap2004550586(plus_self42, plus_sarg20);
        barf_msg2 = ret_val59;
        barf_self3 = ATTR(barf_self2,prog);
        barf_msg3 = barf_msg2;
        barf_at_self1 = barf_self3;
        barf_at_msg1 = barf_msg3;
        barf_at_at1 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self1, barf_at_at1);
        plus_self43 = ((STR) &Intern28965746);
        plus_sarg21 = barf_at_msg1;
        ret_val60 = STR_ap2004550586(plus_self43, plus_sarg21);
        PROG_err_STR(barf_at_self1, ret_val60);
        exit_self1 = ((UNIX) NULL);
        exit_code1 = 1;
        exit(exit_code1);
       }
      }
      else {
       plus_self44 = state011;
       plus_sarg22 = ((STR) &f5);
       ret_val61 = STR_ap2004550586(plus_self44, plus_sarg22);
       plus_self45 = ret_val61;
       plus_sarg23 = self_str;
       ret_val62 = STR_ap2004550586(plus_self45, plus_sarg23);
       plus_self46 = ret_val62;
       plus_sarg24 = ((STR) &oncearg);
       ret_val63 = STR_ap2004550586(plus_self46, plus_sarg24);
       plus_self47 = ret_val63;
       L1621_=INTMINUS(a11,1); 
       plus_arg6 = L1621_;
       str_self12 = plus_arg6;
       clear_self6 = buf7;
       L163 = (clear_self6==((FSTR) NULL));
       L1641_=!(L163); 
       if (L1641_) {
        SATTR(clear_self6,loc,0);
       }
       str_in_self6 = str_self12;
       str_in_s6 = buf7;
       ret_val66 = INT_st367594495(str_in_self6, str_in_s6, 0, 10, ' ');
       buf7 = ret_val66;
       str_self13 = buf7;
       ret_val67 = STR_fr1097270334(((STR) NULL), str_self13);
       ret_val65 = ret_val67;
       s6 = ret_val65;
       ret_val64 = STR_ap1077157958(plus_self47, s6, TRUE);
       plus_self48 = ret_val64;
       plus_sarg25 = ((STR) &name36);
       ret_val68 = STR_ap2004550586(plus_self48, plus_sarg25);
       state011 = ret_val68;
       L166 = (arg_mode==((ARRAYBOOL) NULL));
       L1671_=!(L166); 
       if (L1671_) {
        L1681_=ARR((ARRAYBOOL)arg_mode,i1); 
        L165 = L1681_;
       } else {
        L165 = FALSE;
       }
       if (L165) {
        barf_self4 = self;
        plus_self49 = ((STR) &Ingene409399871);
        plus_sarg26 = ((STR) &argume2028503200);
        ret_val69 = STR_ap2004550586(plus_self49, plus_sarg26);
        barf_msg4 = ret_val69;
        barf_self5 = ATTR(barf_self4,prog);
        barf_msg5 = barf_msg4;
        barf_at_self2 = barf_self5;
        barf_at_msg2 = barf_msg5;
        barf_at_at2 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self2, barf_at_at2);
        plus_self50 = ((STR) &Intern28965746);
        plus_sarg27 = barf_at_msg2;
        ret_val70 = STR_ap2004550586(plus_self50, plus_sarg27);
        PROG_err_STR(barf_at_self2, ret_val70);
        exit_self2 = ((UNIX) NULL);
        exit_code2 = 1;
        exit(exit_code2);
       }
       else {
        plus_self51 = state011;
        plus_sarg28 = ((STR) &foncearg);
        ret_val71 = STR_ap2004550586(plus_self51, plus_sarg28);
        plus_self52 = ret_val71;
        plus_arg7 = arg_cnt;
        str_self14 = plus_arg7;
        clear_self7 = buf8;
        L169 = (clear_self7==((FSTR) NULL));
        L1701_=!(L169); 
        if (L1701_) {
         SATTR(clear_self7,loc,0);
        }
        str_in_self7 = str_self14;
        str_in_s7 = buf8;
        ret_val74 = INT_st367594495(str_in_self7, str_in_s7, 0, 10, ' ');
        buf8 = ret_val74;
        str_self15 = buf8;
        ret_val75 = STR_fr1097270334(((STR) NULL), str_self15);
        ret_val73 = ret_val75;
        s7 = ret_val73;
        ret_val72 = STR_ap1077157958(plus_self52, s7, TRUE);
        plus_self53 = ret_val72;
        plus_sarg29 = ((STR) &name13);
        ret_val76 = STR_ap2004550586(plus_self53, plus_sarg29);
        state011 = ret_val76;
       }
      }
      L1721_=INTPLUS(i_arg,1); 
      i_arg = L1721_;
      L1731_=INTPLUS(arg_cnt,1); 
      arg_cnt = L1731_;
     }
     L1741_=INTPLUS(L151,1); 
     L151 = L1741_;
    }
   }
   after_loop2: ;
  }
  else {
   L175 = ATTR(ATTR(e,fun),tp);
   if ((*dTP_is1811059018[TAG(L175)])(L175)) {
    mang_self3 = self;
    mang_ob3 = ((OB) ATTR(e,fun));
    ret_val77 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
    dtbl_ptr = ret_val77;
    if (ATTR(self,chk_void)) {
     L177 = ATTR(self,null_segfaults);
     L1781_=!(L177); 
     L176 = L1781_;
    } else {
     L176 = FALSE;
    }
    if (L176) {
     ndefer_self2 = self;
     plus_self54 = ((STR) &iff1);
     plus_sarg30 = self_str;
     ret_val78 = STR_ap2004550586(plus_self54, plus_sarg30);
     plus_self55 = ret_val78;
     plus_sarg31 = ((STR) &NULL6);
     ret_val79 = STR_ap2004550586(plus_self55, plus_sarg31);
     ndefer_s2 = ret_val79;
     CGEN_d695831906(ndefer_self2);
     SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     in_self = self;
     L179 = ATTR(in_self,indent);
     L1801_=INTPLUS(L179,1); 
     SATTR(in_self,indent,L1801_);
     runtime_error_se = self;
     plus_self56 = ((STR) &Dispatchedcallto);
     plus_sarg32 = ATTR(ATTR(e,fun),str);
     ret_val80 = STR_ap2004550586(plus_self56, plus_sarg32);
     plus_self57 = ret_val80;
     plus_sarg33 = ((STR) &onvoidselfin);
     ret_val81 = STR_ap2004550586(plus_self57, plus_sarg33);
     plus_self58 = ret_val81;
     plus_sarg34 = ATTR(self,current_function);
     ret_val82 = STR_ap2004550586(plus_self58, plus_sarg34);
     runtime_error_s = ret_val82;
     ndefer_self3 = runtime_error_se;
     plus_self59 = ((STR) &FATAL1);
     plus_sarg35 = runtime_error_s;
     ret_val83 = STR_ap2004550586(plus_self59, plus_sarg35);
     plus_self60 = ret_val83;
     plus_sarg36 = ((STR) &name95);
     ret_val84 = STR_ap2004550586(plus_self60, plus_sarg36);
     ndefer_s3 = ret_val84;
     CGEN_d695831906(ndefer_self3);
     SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
     move_out_self = self;
     L182 = ATTR(move_out_self,indent);
     L1831_=INTMINUS(L182,1); 
     SATTR(move_out_self,indent,L1831_);
     ndefer_self4 = self;
     ndefer_s4 = ((STR) &name4);
     CGEN_d695831906(ndefer_self4);
     SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
    }
    ndefer_self5 = self;
    plus_self61 = ((STR) &fiter_frame);
    mang_self4 = self;
    mang_ob4 = ((OB) ATTR(e,fun));
    ret_val85 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
    plus_sarg37 = ret_val85;
    ret_val86 = STR_ap2004550586(plus_self61, plus_sarg37);
    plus_self62 = ret_val86;
    plus_sarg38 = ((STR) &S_frame3);
    ret_val87 = STR_ap2004550586(plus_self62, plus_sarg38);
    plus_self63 = ret_val87;
    plus_sarg39 = ((STR) &name85);
    ret_val88 = STR_ap2004550586(plus_self63, plus_sarg39);
    plus_self64 = ret_val88;
    plus_sarg40 = dtbl_ptr;
    ret_val89 = STR_ap2004550586(plus_self64, plus_sarg40);
    plus_self65 = ret_val89;
    plus_sarg41 = ((STR) &TAGf);
    ret_val90 = STR_ap2004550586(plus_self65, plus_sarg41);
    plus_self66 = ret_val90;
    plus_sarg42 = self_str;
    ret_val91 = STR_ap2004550586(plus_self66, plus_sarg42);
    plus_self67 = ret_val91;
    plus_sarg43 = ((STR) &alloc_frame);
    ret_val92 = STR_ap2004550586(plus_self67, plus_sarg43);
    ndefer_s5 = ret_val92;
    CGEN_d695831906(ndefer_self5);
    SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
   }
   else {
    ndefer_self6 = self;
    ndefer_s6 = ((STR) &fiter_2029027530);
    CGEN_d695831906(ndefer_self6);
    SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
    defer_self1 = self;
    mang_self5 = self;
    mang_ob5 = ((OB) ATTR(e,fun));
    ret_val93 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
    plus_self68 = ret_val93;
    plus_sarg44 = ((STR) &S_frame1);
    ret_val94 = STR_ap2004550586(plus_self68, plus_sarg44);
    defer_s1 = ret_val94;
    SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
   }
  }
  comment_self = self;
  comment_com = ((STR) &Initia393638030);
  if (ATTR(comment_self,pretty)) {
   ndefer_self7 = comment_self;
   plus_self69 = ((STR) &name54);
   plus_sarg45 = comment_com;
   ret_val95 = STR_ap2004550586(plus_self69, plus_sarg45);
   plus_self70 = ret_val95;
   plus_sarg46 = ((STR) &name79);
   ret_val96 = STR_ap2004550586(plus_self70, plus_sarg46);
   ndefer_s7 = ret_val96;
   CGEN_d695831906(ndefer_self7);
   SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
  }
  L1841_=!(binding_iter_clo); 
  if (L1841_) {
   ndefer_self8 = self;
   ndefer_s8 = ((STR) &fiter_1590862403);
   CGEN_d695831906(ndefer_self8);
   SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
  }
  if (binding_iter_clo) {
   l = CGEN_d1425664957(self, ATTR(ATTR(e,fun),tp));
   ndefer_self9 = self;
   plus_self71 = l;
   plus_sarg47 = ((STR) &ZALLOCf);
   ret_val97 = STR_ap2004550586(plus_self71, plus_sarg47);
   plus_self72 = ret_val97;
   plus_sarg48 = self_str;
   ret_val98 = STR_ap2004550586(plus_self72, plus_sarg48);
   plus_self73 = ret_val98;
   plus_sarg49 = ((STR) &size4);
   ret_val99 = STR_ap2004550586(plus_self73, plus_sarg49);
   ndefer_s9 = ret_val99;
   CGEN_d695831906(ndefer_self9);
   SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
   ndefer_self10 = self;
   plus_self74 = l;
   plus_sarg50 = ((STR) &name80);
   ret_val100 = STR_ap2004550586(plus_self74, plus_sarg50);
   plus_self75 = ret_val100;
   mang_self6 = self;
   mang_ob6 = ((OB) ATTR(ATTR(e,fun),tp));
   ret_val101 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
   plus_sarg51 = ret_val101;
   ret_val102 = STR_ap2004550586(plus_self75, plus_sarg51);
   plus_self76 = ret_val102;
   plus_sarg52 = ((STR) &memcpy1);
   ret_val103 = STR_ap2004550586(plus_self76, plus_sarg52);
   plus_self77 = ret_val103;
   plus_sarg53 = l;
   ret_val104 = STR_ap2004550586(plus_self77, plus_sarg53);
   plus_self78 = ret_val104;
   plus_sarg54 = ((STR) &f6);
   ret_val105 = STR_ap2004550586(plus_self78, plus_sarg54);
   plus_self79 = ret_val105;
   plus_sarg55 = self_str;
   ret_val106 = STR_ap2004550586(plus_self79, plus_sarg55);
   plus_self80 = ret_val106;
   plus_sarg56 = ((STR) &f6);
   ret_val107 = STR_ap2004550586(plus_self80, plus_sarg56);
   plus_self81 = ret_val107;
   plus_sarg57 = self_str;
   ret_val108 = STR_ap2004550586(plus_self81, plus_sarg57);
   plus_self82 = ret_val108;
   plus_sarg58 = ((STR) &size4);
   ret_val109 = STR_ap2004550586(plus_self82, plus_sarg58);
   ndefer_s10 = ret_val109;
   CGEN_d695831906(ndefer_self10);
   SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
   ndefer_self11 = self;
   plus_self83 = l;
   plus_sarg59 = ((STR) &state01);
   ret_val110 = STR_ap2004550586(plus_self83, plus_sarg59);
   ndefer_s11 = ret_val110;
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
   ndefer_self12 = self;
   plus_self84 = ((STR) &f7);
   plus_sarg60 = self_str;
   ret_val1111 = STR_ap2004550586(plus_self84, plus_sarg60);
   plus_self85 = ret_val1111;
   plus_sarg61 = ((STR) &name36);
   ret_val112 = STR_ap2004550586(plus_self85, plus_sarg61);
   plus_self86 = ret_val112;
   plus_sarg62 = l;
   ret_val113 = STR_ap2004550586(plus_self86, plus_sarg62);
   plus_self87 = ret_val113;
   plus_sarg63 = ((STR) &name16);
   ret_val114 = STR_ap2004550586(plus_self87, plus_sarg63);
   ndefer_s12 = ret_val114;
   CGEN_d695831906(ndefer_self12);
   SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
   ndefer_self13 = self;
   ndefer_s13 = state011;
   CGEN_d695831906(ndefer_self13);
   SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
  }
  else {
   comment_self1 = self;
   comment_com1 = ((STR) &Fillin1044524838);
   if (ATTR(comment_self1,pretty)) {
    ndefer_self14 = comment_self1;
    plus_self88 = ((STR) &name54);
    plus_sarg64 = comment_com1;
    ret_val115 = STR_ap2004550586(plus_self88, plus_sarg64);
    plus_self89 = ret_val115;
    plus_sarg65 = ((STR) &name79);
    ret_val116 = STR_ap2004550586(plus_self89, plus_sarg65);
    ndefer_s14 = ret_val116;
    CGEN_d695831906(ndefer_self14);
    SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
   }
   {
    BOOL f_L1851_ = TRUE;
    BOOL f_L1861_ = TRUE;
    /* loop index variable */
    L161 = 0;
    L51 = ATTR(e,unbnd_args);
    L61 = 0;
    L1851_br=L51==NULL?0:ASIZE((ARRAYINT)L51); 
    L1861_=L61-1; 
    while (1) {
     if(L161>=L1851_br)  goto after_loop3; 
     aL1851_=ARR((ARRAYINT)L51,L161); 
     a21 = aL1851_;
     L1861_++; 
     i2 = L1861_;
     L1891_=(a21)==(0); 
     if (L1891_) {
      ndefer_self15 = self;
      ndefer_s15 = ((STR) &fiter_612822643);
      CGEN_d695831906(ndefer_self15);
      SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
     }
     else {
      if ((ATTR(ATTR(e,fun),hot)==((ARRAYBOOL) NULL))) {
       ndefer_self16 = self;
       plus_self90 = ((STR) &fiter_framearg);
       plus_arg8 = a21;
       str_self16 = plus_arg8;
       clear_self8 = buf9;
       L190 = (clear_self8==((FSTR) NULL));
       L1921_=!(L190); 
       if (L1921_) {
        SATTR(clear_self8,loc,0);
       }
       str_in_self8 = str_self16;
       str_in_s8 = buf9;
       ret_val119 = INT_st367594495(str_in_self8, str_in_s8, 0, 10, ' ');
       buf9 = ret_val119;
       str_self17 = buf9;
       ret_val120 = STR_fr1097270334(((STR) NULL), str_self17);
       ret_val118 = ret_val120;
       s8 = ret_val118;
       ret_val117 = STR_ap1077157958(plus_self90, s8, TRUE);
       plus_self91 = ret_val117;
       plus_sarg66 = ((STR) &foncearg1);
       ret_val121 = STR_ap2004550586(plus_self91, plus_sarg66);
       plus_self92 = ret_val121;
       plus_arg9 = i2;
       str_self18 = plus_arg9;
       clear_self9 = buf10;
       L193 = (clear_self9==((FSTR) NULL));
       L1941_=!(L193); 
       if (L1941_) {
        SATTR(clear_self9,loc,0);
       }
       str_in_self9 = str_self18;
       str_in_s9 = buf10;
       ret_val124 = INT_st367594495(str_in_self9, str_in_s9, 0, 10, ' ');
       buf10 = ret_val124;
       str_self19 = buf10;
       ret_val125 = STR_fr1097270334(((STR) NULL), str_self19);
       ret_val123 = ret_val125;
       s9 = ret_val123;
       ret_val122 = STR_ap1077157958(plus_self92, s9, TRUE);
       plus_self93 = ret_val122;
       plus_sarg67 = ((STR) &name16);
       ret_val126 = STR_ap2004550586(plus_self93, plus_sarg67);
       ndefer_s16 = ret_val126;
       CGEN_d695831906(ndefer_self16);
       SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
      }
      else {
       L195 = ATTR(ATTR(e,fun),hot);
       L1961_=INTMINUS(a21,1); 
       L197 = L1961_;
       L1981_=ARR((ARRAYBOOL)L195,L197); 
       hot = L1981_;
       L1991_=!(hot); 
       if (L1991_) {
        ndefer_self17 = self;
        plus_self94 = ((STR) &fiter_framearg);
        plus_arg10 = a21;
        str_self20 = plus_arg10;
        clear_self10 = buf11;
        L200 = (clear_self10==((FSTR) NULL));
        L2011_=!(L200); 
        if (L2011_) {
         SATTR(clear_self10,loc,0);
        }
        str_in_self10 = str_self20;
        str_in_s10 = buf11;
        ret_val129 = INT_st367594495(str_in_self10, str_in_s10, 0, 10, ' ');
        buf11 = ret_val129;
        str_self21 = buf11;
        ret_val130 = STR_fr1097270334(((STR) NULL), str_self21);
        ret_val128 = ret_val130;
        s10 = ret_val128;
        ret_val127 = STR_ap1077157958(plus_self94, s10, TRUE);
        plus_self95 = ret_val127;
        plus_sarg68 = ((STR) &foncearg1);
        ret_val131 = STR_ap2004550586(plus_self95, plus_sarg68);
        plus_self96 = ret_val131;
        plus_arg11 = i2;
        str_self22 = plus_arg11;
        clear_self11 = buf12;
        L202 = (clear_self11==((FSTR) NULL));
        L2031_=!(L202); 
        if (L2031_) {
         SATTR(clear_self11,loc,0);
        }
        str_in_self11 = str_self22;
        str_in_s11 = buf12;
        ret_val134 = INT_st367594495(str_in_self11, str_in_s11, 0, 10, ' ');
        buf12 = ret_val134;
        str_self23 = buf12;
        ret_val135 = STR_fr1097270334(((STR) NULL), str_self23);
        ret_val133 = ret_val135;
        s11 = ret_val133;
        ret_val132 = STR_ap1077157958(plus_self96, s11, TRUE);
        plus_self97 = ret_val132;
        plus_sarg69 = ((STR) &name16);
        ret_val136 = STR_ap2004550586(plus_self97, plus_sarg69);
        ndefer_s17 = ret_val136;
        CGEN_d695831906(ndefer_self17);
        SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
       }
      }
     }
     L2041_=INTPLUS(L161,1); 
     L161 = L2041_;
    }
   }
   after_loop3: ;
   {
    BOOL f_L2051_ = TRUE;
    BOOL f_L2061_ = TRUE;
    /* loop index variable */
    L171 = 0;
    L71 = ATTR(e,bnd_args);
    L81 = 0;
    L2051_br=L71==NULL?0:ASIZE((ARRAYINT)L71); 
    L2061_=L81-1; 
    while (1) {
     if(L171>=L2051_br)  goto after_loop4; 
     aL2051_=ARR((ARRAYINT)L71,L171); 
     a31 = aL2051_;
     L2061_++; 
     i3 = L2061_;
     L2091_=(a31)==(0); 
     if (L2091_) {
      L2101_=!(binding_iter_clo); 
      if (L2101_) {
       ndefer_self18 = self;
       ndefer_s18 = ((STR) &fiter_907730610);
       CGEN_d695831906(ndefer_self18);
       SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
      }
     }
     else {
      if ((ATTR(ATTR(e,fun),hot)==((ARRAYBOOL) NULL))) {
       ndefer_self19 = self;
       plus_self98 = ((STR) &fiter_framearg);
       plus_arg12 = a31;
       str_self24 = plus_arg12;
       clear_self12 = buf13;
       L211 = (clear_self12==((FSTR) NULL));
       L2121_=!(L211); 
       if (L2121_) {
        SATTR(clear_self12,loc,0);
       }
       str_in_self12 = str_self24;
       str_in_s12 = buf13;
       ret_val139 = INT_st367594495(str_in_self12, str_in_s12, 0, 10, ' ');
       buf13 = ret_val139;
       str_self25 = buf13;
       ret_val140 = STR_fr1097270334(((STR) NULL), str_self25);
       ret_val138 = ret_val140;
       s12 = ret_val138;
       ret_val137 = STR_ap1077157958(plus_self98, s12, TRUE);
       plus_self99 = ret_val137;
       plus_sarg70 = ((STR) &fbound_arg);
       ret_val141 = STR_ap2004550586(plus_self99, plus_sarg70);
       plus_self100 = ret_val141;
       plus_arg13 = i3;
       str_self26 = plus_arg13;
       clear_self13 = buf14;
       L213 = (clear_self13==((FSTR) NULL));
       L2141_=!(L213); 
       if (L2141_) {
        SATTR(clear_self13,loc,0);
       }
       str_in_self13 = str_self26;
       str_in_s13 = buf14;
       ret_val144 = INT_st367594495(str_in_self13, str_in_s13, 0, 10, ' ');
       buf14 = ret_val144;
       str_self27 = buf14;
       ret_val145 = STR_fr1097270334(((STR) NULL), str_self27);
       ret_val143 = ret_val145;
       s13 = ret_val143;
       ret_val142 = STR_ap1077157958(plus_self100, s13, TRUE);
       plus_self101 = ret_val142;
       plus_sarg71 = ((STR) &name16);
       ret_val146 = STR_ap2004550586(plus_self101, plus_sarg71);
       ndefer_s19 = ret_val146;
       CGEN_d695831906(ndefer_self19);
       SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
      }
      else {
       L215 = ATTR(ATTR(e,fun),hot);
       L2161_=INTMINUS(a31,1); 
       L217 = L2161_;
       L2181_=ARR((ARRAYBOOL)L215,L217); 
       hot1 = L2181_;
       L2191_=!(hot1); 
       if (L2191_) {
        ndefer_self20 = self;
        plus_self102 = ((STR) &fiter_framearg);
        plus_arg14 = a31;
        str_self28 = plus_arg14;
        clear_self14 = buf15;
        L220 = (clear_self14==((FSTR) NULL));
        L2211_=!(L220); 
        if (L2211_) {
         SATTR(clear_self14,loc,0);
        }
        str_in_self14 = str_self28;
        str_in_s14 = buf15;
        ret_val149 = INT_st367594495(str_in_self14, str_in_s14, 0, 10, ' ');
        buf15 = ret_val149;
        str_self29 = buf15;
        ret_val150 = STR_fr1097270334(((STR) NULL), str_self29);
        ret_val148 = ret_val150;
        s14 = ret_val148;
        ret_val147 = STR_ap1077157958(plus_self102, s14, TRUE);
        plus_self103 = ret_val147;
        plus_sarg72 = ((STR) &f8);
        ret_val151 = STR_ap2004550586(plus_self103, plus_sarg72);
        ndefer_s20 = ret_val151;
        CGEN_d695831906(ndefer_self20);
        SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
        defer_self2 = self;
        plus_self104 = ((STR) &bound_arg1);
        plus_arg15 = i3;
        str_self30 = plus_arg15;
        clear_self15 = buf16;
        L222 = (clear_self15==((FSTR) NULL));
        L2231_=!(L222); 
        if (L2231_) {
         SATTR(clear_self15,loc,0);
        }
        str_in_self15 = str_self30;
        str_in_s15 = buf16;
        ret_val154 = INT_st367594495(str_in_self15, str_in_s15, 0, 10, ' ');
        buf16 = ret_val154;
        str_self31 = buf16;
        ret_val155 = STR_fr1097270334(((STR) NULL), str_self31);
        ret_val153 = ret_val155;
        s15 = ret_val153;
        ret_val152 = STR_ap1077157958(plus_self104, s15, TRUE);
        plus_self105 = ret_val152;
        plus_sarg73 = ((STR) &name16);
        ret_val156 = STR_ap2004550586(plus_self105, plus_sarg73);
        defer_s2 = ret_val156;
        SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
       }
      }
     }
     L2241_=INTPLUS(L171,1); 
     L171 = L2241_;
    }
   }
   after_loop4: ;
  }
  ndefer_self21 = self;
  ndefer_s21 = ((STR) &state111);
  CGEN_d695831906(ndefer_self21);
  SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
  comment_self2 = self;
  comment_com2 = ((STR) &Sethot404575151);
  if (ATTR(comment_self2,pretty)) {
   ndefer_self22 = comment_self2;
   plus_self106 = ((STR) &name54);
   plus_sarg74 = comment_com2;
   ret_val157 = STR_ap2004550586(plus_self106, plus_sarg74);
   plus_self107 = ret_val157;
   plus_sarg75 = ((STR) &name79);
   ret_val158 = STR_ap2004550586(plus_self107, plus_sarg75);
   ndefer_s22 = ret_val158;
   CGEN_d695831906(ndefer_self22);
   SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
  }
  if (binding_iter_clo) {
   ndefer_self23 = self;
   ndefer_s23 = state11;
   CGEN_d695831906(ndefer_self23);
   SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
  }
  else {
   comment_self3 = self;
   comment_com3 = ((STR) &Fillin1000309002);
   if (ATTR(comment_self3,pretty)) {
    ndefer_self24 = comment_self3;
    plus_self108 = ((STR) &name54);
    plus_sarg76 = comment_com3;
    ret_val159 = STR_ap2004550586(plus_self108, plus_sarg76);
    plus_self109 = ret_val159;
    plus_sarg77 = ((STR) &name79);
    ret_val160 = STR_ap2004550586(plus_self109, plus_sarg77);
    ndefer_s24 = ret_val160;
    CGEN_d695831906(ndefer_self24);
    SATTR(ndefer_self24,routine_code,FSTR_p1752847026(ATTR(ndefer_self24,routine_code), ndefer_s24));
   }
   L225 = (ATTR(ATTR(e,fun),hot)==((ARRAYBOOL) NULL));
   L2261_=!(L225); 
   if (L2261_) {
    {
     BOOL f_L2271_ = TRUE;
     BOOL f_L2281_ = TRUE;
     /* loop index variable */
     L181 = 0;
     L91 = ATTR(e,unbnd_args);
     L101 = 0;
     L2271_br=L91==NULL?0:ASIZE((ARRAYINT)L91); 
     L2281_=L101-1; 
     while (1) {
      if(L181>=L2271_br)  goto after_loop5; 
      aL2271_=ARR((ARRAYINT)L91,L181); 
      a41 = aL2271_;
      L2281_++; 
      i4 = L2281_;
      L2321_=(a41)==(0); 
      L233 = L2321_;
      L2341_=!(L233); 
      if (L2341_) {
       L235 = ATTR(ATTR(e,fun),hot);
       L2361_=INTMINUS(a41,1); 
       L237 = L2361_;
       L2381_=ARR((ARRAYBOOL)L235,L237); 
       hot2 = L2381_;
       if (hot2) {
        ndefer_self25 = self;
        plus_self110 = ((STR) &fiter_framearg);
        plus_arg16 = a41;
        str_self32 = plus_arg16;
        clear_self16 = buf17;
        L239 = (clear_self16==((FSTR) NULL));
        L2401_=!(L239); 
        if (L2401_) {
         SATTR(clear_self16,loc,0);
        }
        str_in_self16 = str_self32;
        str_in_s16 = buf17;
        ret_val163 = INT_st367594495(str_in_self16, str_in_s16, 0, 10, ' ');
        buf17 = ret_val163;
        str_self33 = buf17;
        ret_val164 = STR_fr1097270334(((STR) NULL), str_self33);
        ret_val162 = ret_val164;
        s16 = ret_val162;
        ret_val161 = STR_ap1077157958(plus_self110, s16, TRUE);
        plus_self111 = ret_val161;
        plus_sarg78 = ((STR) &f8);
        ret_val165 = STR_ap2004550586(plus_self111, plus_sarg78);
        ndefer_s25 = ret_val165;
        CGEN_d695831906(ndefer_self25);
        SATTR(ndefer_self25,routine_code,FSTR_p1752847026(ATTR(ndefer_self25,routine_code), ndefer_s25));
        defer_self3 = self;
        plus_self112 = ((STR) &hotarg1);
        plus_arg17 = i4;
        str_self34 = plus_arg17;
        clear_self17 = buf18;
        L241 = (clear_self17==((FSTR) NULL));
        L2421_=!(L241); 
        if (L2421_) {
         SATTR(clear_self17,loc,0);
        }
        str_in_self17 = str_self34;
        str_in_s17 = buf18;
        ret_val168 = INT_st367594495(str_in_self17, str_in_s17, 0, 10, ' ');
        buf18 = ret_val168;
        str_self35 = buf18;
        ret_val169 = STR_fr1097270334(((STR) NULL), str_self35);
        ret_val167 = ret_val169;
        s17 = ret_val167;
        ret_val166 = STR_ap1077157958(plus_self112, s17, TRUE);
        plus_self113 = ret_val166;
        plus_sarg79 = ((STR) &name16);
        ret_val170 = STR_ap2004550586(plus_self113, plus_sarg79);
        defer_s3 = ret_val170;
        SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
       }
      }
      L2431_=INTPLUS(L181,1); 
      L181 = L2431_;
     }
    }
    after_loop5: ;
    {
     BOOL f_L2441_ = TRUE;
     BOOL f_L2451_ = TRUE;
     /* loop index variable */
     L191 = 0;
     L121 = ATTR(e,bnd_args);
     L131 = 0;
     L2441_br=L121==NULL?0:ASIZE((ARRAYINT)L121); 
     L2451_=L131-1; 
     while (1) {
      if(L191>=L2441_br)  goto after_loop6; 
      aL2441_=ARR((ARRAYINT)L121,L191); 
      a5 = aL2441_;
      L2451_++; 
      i5 = L2451_;
      L2481_=(a5)==(0); 
      L249 = L2481_;
      L2501_=!(L249); 
      if (L2501_) {
       L251 = ATTR(ATTR(e,fun),hot);
       L2521_=INTMINUS(a5,1); 
       L253 = L2521_;
       L2541_=ARR((ARRAYBOOL)L251,L253); 
       hot3 = L2541_;
       if (hot3) {
        ndefer_self26 = self;
        plus_self114 = ((STR) &fiter_framearg);
        plus_arg18 = a5;
        str_self36 = plus_arg18;
        clear_self18 = buf19;
        L255 = (clear_self18==((FSTR) NULL));
        L2561_=!(L255); 
        if (L2561_) {
         SATTR(clear_self18,loc,0);
        }
        str_in_self18 = str_self36;
        str_in_s18 = buf19;
        ret_val173 = INT_st367594495(str_in_self18, str_in_s18, 0, 10, ' ');
        buf19 = ret_val173;
        str_self37 = buf19;
        ret_val174 = STR_fr1097270334(((STR) NULL), str_self37);
        ret_val172 = ret_val174;
        s18 = ret_val172;
        ret_val171 = STR_ap1077157958(plus_self114, s18, TRUE);
        plus_self115 = ret_val171;
        plus_sarg80 = ((STR) &f8);
        ret_val175 = STR_ap2004550586(plus_self115, plus_sarg80);
        ndefer_s26 = ret_val175;
        CGEN_d695831906(ndefer_self26);
        SATTR(ndefer_self26,routine_code,FSTR_p1752847026(ATTR(ndefer_self26,routine_code), ndefer_s26));
        defer_self4 = self;
        plus_self116 = ((STR) &bound_arg1);
        plus_arg19 = i5;
        str_self38 = plus_arg19;
        clear_self19 = buf20;
        L257 = (clear_self19==((FSTR) NULL));
        L2581_=!(L257); 
        if (L2581_) {
         SATTR(clear_self19,loc,0);
        }
        str_in_self19 = str_self38;
        str_in_s19 = buf20;
        ret_val178 = INT_st367594495(str_in_self19, str_in_s19, 0, 10, ' ');
        buf20 = ret_val178;
        str_self39 = buf20;
        ret_val179 = STR_fr1097270334(((STR) NULL), str_self39);
        ret_val177 = ret_val179;
        s19 = ret_val177;
        ret_val176 = STR_ap1077157958(plus_self116, s19, TRUE);
        plus_self117 = ret_val176;
        plus_sarg81 = ((STR) &name16);
        ret_val180 = STR_ap2004550586(plus_self117, plus_sarg81);
        defer_s4 = ret_val180;
        SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
       }
      }
      L2591_=INTPLUS(L191,1); 
      L191 = L2591_;
     }
    }
    after_loop6: ;
   }
  }
  comment_self4 = self;
  comment_com4 = ((STR) &calliter);
  if (ATTR(comment_self4,pretty)) {
   ndefer_self27 = comment_self4;
   plus_self118 = ((STR) &name54);
   plus_sarg82 = comment_com4;
   ret_val181 = STR_ap2004550586(plus_self118, plus_sarg82);
   plus_self119 = ret_val181;
   plus_sarg83 = ((STR) &name79);
   ret_val182 = STR_ap2004550586(plus_self119, plus_sarg83);
   ndefer_s27 = ret_val182;
   CGEN_d695831906(ndefer_self27);
   SATTR(ndefer_self27,routine_code,FSTR_p1752847026(ATTR(ndefer_self27,routine_code), ndefer_s27));
  }
  create_self1 = ((ARRAYSTR) NULL);
  create_n = 1;
  L261 = create_n;
  L263=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L261)*sizeof(STR);
  L262=ZALLOC_BIG(L263);
  if (L262==NULL) FATAL("Unable to allocate more memory");
  ((OB)L262)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L262)->header.destroyed=0;
#endif

  L260 = ((ARRAYSTR) L262);
  L260->asize = L261;
  ret_val183 = L260;
  arg_list = ret_val183;
  if (binding_iter_clo) {
   plus_self120 = ((STR) &f7);
   plus_sarg84 = self_str;
   ret_val184 = STR_ap2004550586(plus_self120, plus_sarg84);
   L264 = ret_val184;
   SARR((ARRAYSTR)arg_list,0,(STR)L264); 
   ;
  }
  else {
   SARR((ARRAYSTR)arg_list,0,(STR)((STR) &fiter_frame1)); 
   ;
  }
  iter_call = CGEN_e1835933683(self, ATTR(e,fun), ((dAM_EXPR) e), arg_list);
  plus_self121 = iter_call;
  plus_sarg85 = ((STR) &name16);
  ret_val185 = STR_ap2004550586(plus_self121, plus_sarg85);
  iter_call = ret_val185;
  L267 = (ATTR(ATTR(e,fun),ret)==((dTP) NULL));
  L2681_=!(L267); 
  if (L2681_) {
   ndefer_self28 = self;
   plus_self122 = ((STR) &ret_val3111);
   plus_sarg86 = iter_call;
   ret_val186 = STR_ap2004550586(plus_self122, plus_sarg86);
   ndefer_s28 = ret_val186;
   CGEN_d695831906(ndefer_self28);
   SATTR(ndefer_self28,routine_code,FSTR_p1752847026(ATTR(ndefer_self28,routine_code), ndefer_s28));
   if (binding_iter_clo) {
    ndefer_self29 = self;
    plus_self123 = ((STR) &fstatef);
    plus_sarg87 = self_str;
    ret_val187 = STR_ap2004550586(plus_self123, plus_sarg87);
    plus_self124 = ret_val187;
    plus_sarg88 = ((STR) &state1111);
    ret_val188 = STR_ap2004550586(plus_self124, plus_sarg88);
    ndefer_s29 = ret_val188;
    CGEN_d695831906(ndefer_self29);
    SATTR(ndefer_self29,routine_code,FSTR_p1752847026(ATTR(ndefer_self29,routine_code), ndefer_s29));
   }
   else {
    ndefer_self30 = self;
    ndefer_s30 = ((STR) &fstate886545549);
    CGEN_d695831906(ndefer_self30);
    SATTR(ndefer_self30,routine_code,FSTR_p1752847026(ATTR(ndefer_self30,routine_code), ndefer_s30));
   }
   ndefer_self31 = self;
   plus_self125 = ((STR) &returnret_val);
   plus_sarg89 = ((STR) &name162);
   ret_val189 = STR_ap2004550586(plus_self125, plus_sarg89);
   ndefer_s31 = ret_val189;
   CGEN_d695831906(ndefer_self31);
   SATTR(ndefer_self31,routine_code,FSTR_p1752847026(ATTR(ndefer_self31,routine_code), ndefer_s31));
  }
  else {
   ndefer_self32 = self;
   ndefer_s32 = iter_call;
   CGEN_d695831906(ndefer_self32);
   SATTR(ndefer_self32,routine_code,FSTR_p1752847026(ATTR(ndefer_self32,routine_code), ndefer_s32));
   if (binding_iter_clo) {
    ndefer_self33 = self;
    plus_self126 = ((STR) &fstatef);
    plus_sarg90 = self_str;
    ret_val190 = STR_ap2004550586(plus_self126, plus_sarg90);
    plus_self127 = ret_val190;
    plus_sarg91 = ((STR) &state1111);
    ret_val191 = STR_ap2004550586(plus_self127, plus_sarg91);
    ndefer_s33 = ret_val191;
    CGEN_d695831906(ndefer_self33);
    SATTR(ndefer_self33,routine_code,FSTR_p1752847026(ATTR(ndefer_self33,routine_code), ndefer_s33));
   }
   else {
    ndefer_self34 = self;
    ndefer_s34 = ((STR) &fstate886545549);
    CGEN_d695831906(ndefer_self34);
    SATTR(ndefer_self34,routine_code,FSTR_p1752847026(ATTR(ndefer_self34,routine_code), ndefer_s34));
   }
   ndefer_self35 = self;
   ndefer_s35 = ((STR) &return7);
   CGEN_d695831906(ndefer_self35);
   SATTR(ndefer_self35,routine_code,FSTR_p1752847026(ATTR(ndefer_self35,routine_code), ndefer_s35));
  }
  plus_self128 = ATTR(self,code_c);
  plus_s16 = ((dSTR) ATTR(self,routine_code));
  L269 = (plus_s16==((dSTR) NULL));
  L2701_=!(L269); 
  if (L2701_) {
   L271 = ATTR(plus_self128,ntext);
   L272 = plus_s16;
   SATTR(plus_self128,ntext,FSTR_p1752847026(L271, (*dSTR_strrSTR[TAG(L272)])(L272)));
  }
  ret_val192 = plus_self128;
  plus_self129 = ret_val192;
  plus_s17 = ((dSTR) ((STR) &name163));
  L273 = ATTR(plus_self129,ntext);
  L274 = plus_s17;
  SATTR(plus_self129,ntext,FSTR_p1752847026(L273, (*dSTR_strrSTR[TAG(L274)])(L274)));
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(e,fun);
  ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
  L275 = (ard==((AM_ROUT_DEF) NULL));
  L2761_=!(L275); 
  if (L2761_) {
   SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
   SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_m875729145(CGEN self) {
 ARRAYdTP a;
 FSETdTP L11;
 ARRAYdTP L21;
 INT pos = INT_zero;
 INT neg1 = INT_zero;
 ARRAYdTP L31;
 dTP tp;
 ARRAYdTP create_self;
 INT create_n = INT_zero;
 ARRAYdTP ret_val;
 INT L41 = INT_zero;
 ARRAYdTP sort_self;
 INT L51 = INT_zero;
 ARRAYdTP L6;
 INT L7;
 OB L8;
 INT L9;
 INT L101_br;
 dTP L12;
 dTP L13;
 INT L141_;
 BOOL L15;
 BOOL L161_;
 INT L171_;
 INT L18;
 INT L191_;
 INT L201_br;
 dTP aL201_;
 dTP L22;
 dTP L23;
 INT L241_;
 INT L251_;
 INT L261_;
 create_self = ((ARRAYdTP) NULL);
 create_n = FSETdTP_sizerINT(ATTR(self,needs_tag));
 L7 = create_n;
 L9=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L7)*sizeof(dTP);
 L8=ZALLOC_BIG(L9);
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

   ((OB)L8)->header.destroyed=0;
#endif

 L6 = ((ARRAYdTP) L8);
 L6->asize = L7;
 ret_val = L6;
 a = ret_val;
 {
  struct FSETdTP_eltbrdTP_frame_struct other1_0;
FSETdTP_eltbrdTP_frame noname1 = &other1_0;
  BOOL f_L101_ = TRUE;
  L11 = ATTR(self,needs_tag);
  /* loop index variable */
  L41 = 0;
  L21 = a;
  noname1->self = L11;
  noname1->state = 0;
  L101_br=L21==NULL?0:ASIZE((ARRAYdTP)L21); 
  while (1) {
   if(L41>=L101_br)  goto after_loop; 
   L12 = FSETdTP_eltbrdTP(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   L13 = L12;
   SARR((ARRAYdTP)L21,L41,(dTP)L13); 
   ;
   L141_=INTPLUS(L41,1); 
   L41 = L141_;
  }
 }
 after_loop: ;
 sort_self = a;
 L15 = (sort_self==((ARRAYdTP) NULL));
 L161_=!(L15); 
 if (L161_) {
  L171_=ASIZE((ARRAYdTP)sort_self); 
  L18 = L171_;
  L191_=INTMINUS(L18,1); 
  ARRAYd1239612935(sort_self, 0, L191_);
 }
 pos = 1;
 neg1 = -1;
 {
  BOOL f_L201_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L31 = a;
  L201_br=L31==NULL?0:ASIZE((ARRAYdTP)L31); 
  while (1) {
   if(L51>=L201_br)  goto after_loop1; 
   aL201_=ARR((ARRAYdTP)L31,L51); 
   tp = aL201_;
   L23 = tp;
   if ((*dTP_is1334578382[TAG(L23)])(L23)) {
    SATTR(self,tags,FMAPdT1485079589(ATTR(self,tags), tp, neg1));
    L241_=INTMINUS(neg1,1); 
    neg1 = L241_;
   }
   else {
    SATTR(self,tags,FMAPdT1485079589(ATTR(self,tags), tp, pos));
    L251_=INTPLUS(pos,1); 
    pos = L251_;
   }
   L261_=INTPLUS(L51,1); 
   L51 = L261_;
  }
 }
 after_loop1: ;
}
