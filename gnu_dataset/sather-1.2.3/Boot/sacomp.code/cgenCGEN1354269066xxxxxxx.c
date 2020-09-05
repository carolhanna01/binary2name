#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

typedef struct AREFFL1698233817_struct {/* layout for AREF{FLIST{AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTA725283029_struct *arr_part[1];
 } *AREFFL1698233817;

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

typedef struct INOUT_MODE_struct {/* layout for INOUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *INOUT_MODE;

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

typedef struct PRINT_OB_struct {/* layout for PRINT_OB */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *PRINT_OB;

typedef struct SE_CONTEXT_struct {/* layout for SE_CONTEXT */
 OB_HEADER header;
 struct FSETSIDE_EFFECT_struct *set;
 INT weight;
 struct SIG_struct *rsig;
 BOOL forks;
 BOOL has_arith_error;
 BOOL has_export;
 BOOL has_fatal_error;
 BOOL has_import;
 BOOL has_new;
 BOOL has_raise;
 BOOL has_yield_in_loc;
 BOOL is_full;
 BOOL may_block;
 BOOL unsafe;
 } *SE_CONTEXT;

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

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

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

typedef struct AM_ROU1235468764_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1235468764_frame;

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

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

typedef struct AREFFL1603407436_frame_struct {
 INT state;
 AREFFL1698233817 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AREFFL1603407436_frame;

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

typedef struct CS_OPTIONS_struct {/* layout for CS_OPTIONS */
 OB_HEADER header;
 struct ARRAYSTR_struct *arg_locations;
 struct ARRAYSTR_struct *args;
 struct CGEN_struct *cgen;
 struct CS_struct *cs;
 struct FMAPSTRSTR_struct *module_inclusion;
 struct FSETSTR_struct *arith_in;
 struct FSETSTR_struct *arith_out;
 struct FSETSTR_struct *assert_in;
 struct FSETSTR_struct *assert_out;
 struct FSETSTR_struct *bounds_in;
 struct FSETSTR_struct *bounds_out;
 struct FSETSTR_struct *classes1;
 struct FSETSTR_struct *invariant_in;
 struct FSETSTR_struct *invariant_out;
 struct FSETSTR_struct *paths;
 struct FSETSTR_struct *post_in;
 struct FSETSTR_struct *post_out;
 struct FSETSTR_struct *pre_in;
 struct FSETSTR_struct *pre_out;
 struct FSETSTR_struct *return_in;
 struct FSETSTR_struct *return_out;
 struct FSETSTR_struct *void_in;
 struct FSETSTR_struct *void_out;
 struct FSETSTR_struct *when_in;
 struct FSETSTR_struct *when_out;
 struct INLINE_struct *inliner;
 INT cache_size;
 INT cache_slot_size;
 INT local_call_acces;
 INT local_call_dynam;
 INT next;
 INT prefetch_weight;
 INT remote_call_acce;
 struct PARSE_struct *parser;
 struct PROG_struct *prog;
 BOOL all1;
 BOOL arith_all;
 BOOL assert_all;
 BOOL bounds_all;
 BOOL cache1;
 BOOL cse;
 BOOL cse_debug;
 BOOL debug_C_seen;
 BOOL debug_source_see;
 BOOL destroy_chk;
 BOOL got_libs;
 BOOL invariant_all;
 BOOL local_call;
 BOOL local_call_dynam1;
 BOOL local_opt;
 BOOL loop_poll;
 BOOL loop_prefetch;
 BOOL parloops;
 BOOL post_all;
 BOOL post_write;
 BOOL pre_all;
 BOOL prefetch;
 BOOL psather_chk;
 BOOL psather_stats;
 BOOL psather_trace;
 BOOL remote_call;
 BOOL remote_call_crea;
 BOOL return_all;
 BOOL side_debug;
 BOOL side_effects;
 BOOL specul_prefetch;
 BOOL stats;
 BOOL void_all;
 BOOL when_all;
 } *CS_OPTIONS;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTA2119642552_frame_struct {
 INT state;
 FLISTA725283029 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA725283029 aget_self;
 INT aget_ind;
 AM_LOCAL_EXPR ret_val;
 } *FLISTA2119642552_frame;

typedef struct FLISTAM_ROUT_DEF_struct {/* layout for FLIST{AM_ROUT_DEF} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FLISTAM_ROUT_DEF;

typedef struct FLISTA112326087_struct {/* layout for FLIST{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FLISTA112326087;

typedef struct FLISTA120298683_frame_struct {
 INT state;
 FLISTA112326087 self;/* Formal argument: self */
 AM_SHARED_EXPR ret_val1;
 INT i;
 INT sz;
 FLISTA112326087 aget_self;
 INT aget_ind;
 AM_SHARED_EXPR ret_val;
 } *FLISTA120298683_frame;

typedef struct FLISTSIG_struct {/* layout for FLIST{SIG} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FLISTSIG;

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

typedef struct FMAPdOBNAMESPACE_struct {/* layout for FMAP{$OB,NAMESPACE} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdOBNAMESPACE_struct arr_part[1];
 } *FMAPdOBNAMESPACE;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FSETAM1542678947_struct {/* layout for FSET{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FSETAM1542678947;

typedef struct FSETSIG_struct {/* layout for FSET{SIG} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct SIG_struct *arr_part[1];
 } *FSETSIG;

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

extern FLISTSIG PRINT_OB_funcs;
extern FLISTSTR SFILE_ID_files;
extern FMAPSI518162669 CODE_F508943357;
extern INOUT_MODE MODES_inout_mode;
extern INT INT_asize;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1325635093;

/* Function declarations */


extern RETURNED_CONST BOOL (**dINLIN203982169)(dINLINE, SIG);

extern RETURNED_CONST BOOL (**dMODE_2144735300)(dMODE);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is914808564)(dTP);

extern RETURNED_CONST FSETSIG (**dGENER354174443)(dGENERATE_AM);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);
AM_FORMAL_ARG AM_ROU1235468764(AM_ROU1235468764_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_SHARED_EXPR FLISTA120298683(FLISTA120298683_frame);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYINT SIG_at89227380(SIG);
BOOL CGEN_i1416238209(CGEN, dAM_EXPR);
BOOL FSETSI722431140(FSETSIG, SIG);
BOOL SIG_is275586960(SIG);
BOOL SIG_is418491101(SIG);
BOOL SIG_is600379186(SIG);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA189055531(FLISTA725283029, FLISTA725283029);
FLISTAM_ROUT_DEF FLISTA1773009698(FLISTAM_ROUT_DEF, AM_ROUT_DEF);
FLISTSIG FLISTS69519306(FLISTSIG, SIG);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FMAPSI518162669 FMAPSI396914288(FMAPSI518162669, SIG, AM_ROUT_DEF);
FMAPdOBNAMESPACE FMAPdO1864084602(FMAPdOBNAMESPACE, OB);
FSETAM1542678947 FSETAM381445024(FSETAM1542678947, AM_SHARED_EXPR);
FSETSIG FSETSI671175003(FSETSIG, SIG);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AREFFL1603407436(AREFFL1603407436_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT FLISTS228922656(FLISTSIG);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT SFILE_1102877860(SFILE_ID);
INT SFILE_2070755978(SFILE_ID);
INT STR_sizerINT(STR);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_d1279589030(CGEN, AM_FORMAL_ARG, SIG, STR);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_d815167520(CGEN, dTP);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR MANGLE119219986(MANGLE, OB, OB);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
void CGEN_a1893106143(CGEN, SFILE_ID);
void CGEN_b2123071570(CGEN, AM_SHARED_EXPR);
void CGEN_c1869222825(CGEN, AM_ROUT_DEF);
void CGEN_d695831906(CGEN);
void CGEN_e103713585(CGEN, AM_ROUT_DEF);
void CGEN_e233957487(CGEN, AM_ROUT_DEF);
void CGEN_e324254282(CGEN, AM_ROUT_DEF);
void CGEN_e351697741(CGEN, AM_ROUT_DEF);
void CGEN_e494460490(CGEN, dAM_STMT);
void CGEN_e520888267(CGEN, AM_ROUT_DEF);
void CGEN_e640781662(CGEN);
void CGEN_e756677294(CGEN, AM_ROUT_DEF);
void CGEN_g628546529(CGEN, AM_ROUT_DEF, ARRAYSTR*, ARRAYSTR*);
void CGEN_newline(CGEN);
void CGEN_p214884983(CGEN, AM_ROUT_DEF);
void CGEN_s1419723103(CGEN, AM_ROUT_DEF);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void CODE_F65146505(CODE_FILE);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void MANGLE1827205184(MANGLE, OB, STR, OB);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1416238209(CGEN self, dAM_EXPR e) {
 BOOL ret_val = BOOL_zero;
 if ((e==((dAM_EXPR) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (e==NULL) {
  goto other466;
 } else
 switch (TAG(e)) {
  case AM_VOID_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_BOOL_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_CHAR_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_STR_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  case AM_INT_CONST_tag:
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other466: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e103713585(CGEN self, AM_ROUT_DEF func) {
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
 dSTR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 SIG is_eq_self;
 SIG is_eq_s;
 BOOL ret_val6 = BOOL_zero;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val7 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 SIG is_iter_self3;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_iter_self4 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 SIG is_builtin_routi;
 BOOL ret_val12 = BOOL_zero;
 SIG is_routine_self;
 BOOL ret_val13 = BOOL_zero;
 SIG is_iter_self5;
 BOOL ret_val14 = BOOL_zero;
 IDENT is_iter_self6 = IDENT_zero;
 BOOL ret_val15 = BOOL_zero;
 extern STR GeneratingCfor;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 dSTR L7;
 OB L8;
 FILE1 L9;
 OB L10;
 extern STR sather_main1;
 BOOL L12;
 BOOL L13;
 BOOL L141_;
 STR L15;
 INT L16;
 INT L171_;
 INT L18;
 CHAR L191_;
 CHAR L20;
 BOOL L211_;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 dINLINE L25;
 dGENERATE_AM L26;
 FSETSIG L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 BOOL L32;
 BOOL L331_;
 STR L34;
 INT L35;
 INT L361_;
 INT L37;
 CHAR L381_;
 CHAR L39;
 BOOL L401_;
 BOOL L41;
 BOOL L42;
 BOOL L43;
 BOOL L441_;
 STR L45;
 INT L46;
 INT L471_;
 INT L48;
 CHAR L491_;
 CHAR L50;
 BOOL L511_;
 BOOL L52;
 BOOL L531_;
 BOOL L54;
 BOOL L551_;
 CGEN_a1893106143(self, ATTR(func,source1));
 if (ATTR(self,only_check)) {
  return;
 }
 if (ATTR(ATTR(self,prog),show_am)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &GeneratingCfor);
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
  plus_s1 = ATTR(ATTR(func,sig1),str);
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
  L8=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  memset(L8,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L8)->header.tag=CHAR_tag;
  L7 = (dSTR)((CHAR_boxed) L8);
  ((CHAR_boxed) L7)->immutable_part = '\n';
  plus_s2 = L7;
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_dSTR(ret_val5, plus_s2);
 }
 is_eq_self = ATTR(func,sig1);
 is_eq_s = ATTR(self,main_sig);
 ret_val6 = (is_eq_self==is_eq_s);
 if (ret_val6) {
  MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(func,sig1)), ((STR) &sather_main1), ((OB) NULL));
  CODE_F1291349628(ATTR(self,system_c), ATTR(func,sig1));
 }
 if (ATTR(func,is_abstract)) {
  is_iter_self = func;
  is_iter_self1 = ATTR(is_iter_self,sig1);
  is_iter_self2 = ATTR(is_iter_self1,name1);
  L13 = (is_iter_self2.str==((STR) NULL));
  L141_=!(L13); 
  if (L141_) {
   L15 = is_iter_self2.str;
   L16 = STR_sizerINT(is_iter_self2.str);
   L171_=INTMINUS(L16,1); 
   L18 = L171_;
   L191_=ARR((STR)L15,L18); 
   L20 = L191_;
   L211_=L20=='!'; 
   L12 = L211_;
  } else {
   L12 = FALSE;
  }
  ret_val9 = L12;
  ret_val8 = ret_val9;
  ret_val7 = ret_val8;
  L22 = ret_val7;
  L231_=!(L22); 
  if (L231_) {
   SATTR(self,abstract_routs,FLISTA1773009698(ATTR(self,abstract_routs), func));
  }
  else {
   SATTR(self,abstract_iters,FLISTA1773009698(ATTR(self,abstract_iters), func));
  }
 }
 else {
  L25 = ATTR(ATTR(self,prog),inliner);
  if ((*dINLIN203982169[TAG(L25)])(L25, ATTR(func,sig1))) {
   L26 = ATTR(ATTR(self,prog),generate_am);
   L27 = (*dGENER354174443[TAG(L26)])(L26);
   L28 = FSETSI722431140(L27, ATTR(func,sig1));
   L291_=!(L28); 
   L24 = L291_;
  } else {
   L24 = FALSE;
  }
  if (L24) {
   SATTR(self,not_emitted,FMAPSI396914288(ATTR(self,not_emitted), ATTR(func,sig1), func));
   is_iter_self3 = ATTR(func,sig1);
   is_iter_self4 = ATTR(is_iter_self3,name1);
   L32 = (is_iter_self4.str==((STR) NULL));
   L331_=!(L32); 
   if (L331_) {
    L34 = is_iter_self4.str;
    L35 = STR_sizerINT(is_iter_self4.str);
    L361_=INTMINUS(L35,1); 
    L37 = L361_;
    L381_=ARR((STR)L34,L37); 
    L39 = L381_;
    L401_=L39=='!'; 
    L30 = L401_;
   } else {
    L30 = FALSE;
   }
   ret_val11 = L30;
   ret_val10 = ret_val11;
   if (ret_val10) {
    SATTR(self,itersig_map,FMAPSI396914288(ATTR(self,itersig_map), ATTR(func,sig1), func));
   }
  }
  else {
   is_builtin_routi = ATTR(func,sig1);
   if (SIG_is418491101(is_builtin_routi)) {
    is_routine_self = is_builtin_routi;
    is_iter_self5 = is_routine_self;
    is_iter_self6 = ATTR(is_iter_self5,name1);
    L43 = (is_iter_self6.str==((STR) NULL));
    L441_=!(L43); 
    if (L441_) {
     L45 = is_iter_self6.str;
     L46 = STR_sizerINT(is_iter_self6.str);
     L471_=INTMINUS(L46,1); 
     L48 = L471_;
     L491_=ARR((STR)L45,L48); 
     L50 = L491_;
     L511_=L50=='!'; 
     L42 = L511_;
    } else {
     L42 = FALSE;
    }
    ret_val15 = L42;
    ret_val14 = ret_val15;
    L52 = ret_val14;
    L531_=!(L52); 
    ret_val13 = L531_;
    L41 = ret_val13;
   } else {
    L41 = FALSE;
   }
   ret_val12 = L41;
   L54 = ret_val12;
   L551_=!(L54); 
   if (L551_) {
    CGEN_e324254282(self, func);
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e324254282(CGEN self, AM_ROUT_DEF func) {
 AREFFL1698233817 L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 INT aa = INT_zero;
 INT L31 = INT_zero;
 FLISTA725283029 L41;
 FLISTA725283029 L51;
 ARRAYINT L61;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT aeltb_self = INT_zero;
 BOOL ret_val = BOOL_zero;
 INT L91 = INT_zero;
 BOOL aeltb_if_first = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 INT plus_arg = INT_zero;
 STR ret_val3;
 STR s;
 INT str_self = INT_zero;
 STR ret_val4;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val5;
 FSTR str_self1;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val9;
 INT L101 = INT_zero;
 STR plus_self6;
 INT plus_arg1 = INT_zero;
 STR ret_val10;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val11;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val12;
 FSTR str_self3;
 STR ret_val13;
 extern STR name1;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 INT L181_br;
 INT rL181_;
 INT L19;
 BOOL L201_;
 BOOL L22;
 BOOL L231_;
 extern STR S___;
 extern STR name1;
 ARRAYINT L26;
 INT L271_;
 INT aL241_;
 INT aL251_;
 INT L28;
 INT L29;
 INT L30;
 BOOL L321_;
 extern STR L;
 extern STR arg5;
 ARRAYINT L33;
 INT L341_;
 BOOL L35;
 BOOL L361_;
 extern STR islocal;
 extern STR f11;
 INT L371_;
 AM_LOCAL_EXPR L38;
 AREFFL1698233817 L39;
 FLISTA725283029 L401_;
 AM_LOCAL_EXPR L42;
 AREFFL1698233817 L43;
 FLISTA725283029 L441_;
 INT L451_;
 extern STR attr_access1;
 INT L461_br;
 INT aL461_;
 extern STR name10;
 INT L47;
 BOOL L48;
 BOOL L491_;
 INT L501_;
 extern STR name1;
 SATTR(self,func_name_commen,((STR) &name1));
 if (ATTR(ATTR(self,options1),local_call)) {
  L14 = (ATTR(ATTR(func,sig1),opt_info)==((OPT_LOCAL_CALL) NULL));
  L151_=!(L14); 
  L13 = L151_;
 } else {
  L13 = FALSE;
 }
 if (L13) {
  L16 = (ATTR(ATTR(ATTR(func,sig1),opt_info),consider)==((ARRAYINT) NULL));
  L171_=!(L16); 
  L12 = L171_;
 } else {
  L12 = FALSE;
 }
 if (L12) {
  SATTR(self,no_destroy,TRUE);
  {
   BOOL f_L181_ = TRUE;
   /* loop index variable */
   L71 = 0;
   L11 = ATTR(ATTR(ATTR(func,sig1),opt_info),near_local);
   L181_br=ASIZE((AREFFL1698233817)L11); 
   while (1) {
    if(L71>=L181_br)  goto after_loop; 
    rL181_=L71; 
    i = rL181_;
    L201_=(i)==(0); 
    L22 = L201_;
    L231_=!(L22); 
    if (L231_) {
     SATTR(self,func_name_suffix,((STR) &S___));
     SATTR(self,func_name_commen,((STR) &name1));
     {
      BOOL f_L241_ = TRUE;
      BOOL f_L251_ = TRUE;
      /* loop index variable */
      L81 = 0;
      L26 = ATTR(ATTR(ATTR(func,sig1),opt_info),consider);
      L271_=(L26)==NULL?0:ASIZE((ARRAYINT)L26); 
      L21 = L271_;
      /* Initialization of inlined iter INT::aelt!:BOOL */
      L31 = i;
      aeltb_self = L31;
      L91 = INT_asize;
      aeltb_if_first = TRUE;
      while (1) {
       if(L81>=L21)  goto after_loop1; 
       aL241_=L81; 
       aa = aL241_;
       if(L81>=L91)  goto after_loop1; 
       aL251_=L81; 
       L30 = aL251_;
       L321_=((CHAR)((aeltb_self&(1<<L30))!=0)); 
       if (L321_) {
        plus_self = ATTR(self,func_name_suffix);
        plus_sarg = ((STR) &L);
        ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
        SATTR(self,func_name_suffix,ret_val1);
        plus_self1 = ATTR(self,func_name_commen);
        plus_sarg1 = ((STR) &arg5);
        ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
        plus_self2 = ret_val2;
        L33 = ATTR(ATTR(ATTR(func,sig1),opt_info),consider);
        L341_=ARR((ARRAYINT)L33,aa); 
        plus_arg = L341_;
        str_self = plus_arg;
        clear_self = buf1;
        L35 = (clear_self==((FSTR) NULL));
        L361_=!(L35); 
        if (L361_) {
         SATTR(clear_self,loc,0);
        }
        str_in_self = str_self;
        str_in_s = buf1;
        ret_val5 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
        buf1 = ret_val5;
        str_self1 = buf1;
        ret_val6 = STR_fr1097270334(((STR) NULL), str_self1);
        ret_val4 = ret_val6;
        s = ret_val4;
        ret_val3 = STR_ap1077157958(plus_self2, s, TRUE);
        plus_self3 = ret_val3;
        plus_sarg2 = ((STR) &islocal);
        ret_val7 = STR_ap2004550586(plus_self3, plus_sarg2);
        SATTR(self,func_name_commen,ret_val7);
       }
       else {
        plus_self4 = ATTR(self,func_name_suffix);
        plus_sarg3 = ((STR) &f11);
        ret_val8 = STR_ap2004550586(plus_self4, plus_sarg3);
        SATTR(self,func_name_suffix,ret_val8);
       }
       L371_=INTPLUS(L81,1); 
       L81 = L371_;
      }
     }
     after_loop1: ;
     {
      struct FLISTA2119642552_frame_struct other3_0;
FLISTA2119642552_frame noname1 = &other3_0;
      noname1->state = 0;
      while (1) {
       if (noname1->state == 0) {
        L39 = ATTR(ATTR(ATTR(func,sig1),opt_info),near_local);
        L401_=(FLISTA725283029)ARR((AREFFL1698233817)L39,i); 
        L41 = L401_;
        noname1->self = L41;
       }
       L38 = FLISTA2119642552(noname1);
       if (noname1->state == -1) {
        goto after_loop2;
       }
       SATTR(L38,is_always_near,TRUE);
      }
     }
     after_loop2: ;
     CGEN_p214884983(self, func);
     {
      struct FLISTA2119642552_frame_struct other4_0;
FLISTA2119642552_frame noname2 = &other4_0;
      noname2->state = 0;
      while (1) {
       if (noname2->state == 0) {
        L43 = ATTR(ATTR(ATTR(func,sig1),opt_info),near_local);
        L441_=(FLISTA725283029)ARR((AREFFL1698233817)L43,i); 
        L51 = L441_;
        noname2->self = L51;
       }
       L42 = FLISTA2119642552(noname2);
       if (noname2->state == -1) {
        goto after_loop3;
       }
       SATTR(L42,is_always_near,FALSE);
      }
     }
     after_loop3: ;
    }
    L451_=INTPLUS(L71,1); 
    L71 = L451_;
   }
  }
  after_loop: ;
  SATTR(self,func_name_commen,((STR) &attr_access1));
  {
   BOOL f_L461_ = TRUE;
   /* loop index variable */
   L101 = 0;
   L61 = SIG_at89227380(ATTR(func,sig1));
   L461_br=L61==NULL?0:ASIZE((ARRAYINT)L61); 
   while (1) {
    plus_self5 = ATTR(self,func_name_commen);
    plus_sarg4 = ((STR) &name10);
    ret_val9 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val9;
    if(L101>=L461_br)  goto after_loop4; 
    aL461_=ARR((ARRAYINT)L61,L101); 
    plus_arg1 = aL461_;
    str_self2 = plus_arg1;
    clear_self1 = buf2;
    L48 = (clear_self1==((FSTR) NULL));
    L491_=!(L48); 
    if (L491_) {
     SATTR(clear_self1,loc,0);
    }
    str_in_self1 = str_self2;
    str_in_s1 = buf2;
    ret_val12 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
    buf2 = ret_val12;
    str_self3 = buf2;
    ret_val13 = STR_fr1097270334(((STR) NULL), str_self3);
    ret_val11 = ret_val13;
    s1 = ret_val11;
    ret_val10 = STR_ap1077157958(plus_self6, s1, TRUE);
    SATTR(self,func_name_commen,ret_val10);
    L501_=INTPLUS(L101,1); 
    L101 = L501_;
   }
  }
  after_loop4: ;
 }
 SATTR(self,no_destroy,FALSE);
 SATTR(self,func_name_suffix,((STR) &name1));
 CGEN_p214884983(self, func);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e351697741(CGEN self, AM_ROUT_DEF f) {
 STR res_arg = ((STR) NULL);
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR final_arg_list = ((ARRAYSTR) NULL);
 ARRAYSTR type_list = ((ARRAYSTR) NULL);
 INT num_args = INT_zero;
 BOOL has_return = BOOL_zero;
 SIG sig1;
 STR rout_index = ((STR) NULL);
 STR name111;
 BOOL out_args = BOOL_zero;
 ARRAYSTR L11;
 STR arg_str;
 ARRAYSTR L21;
 STR type_str;
 STR L31;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i = INT_zero;
 AM_FORMAL_ARG e;
 STR init_str = ((STR) NULL);
 STR local11;
 FLISTSTR ll;
 INT cn = INT_zero;
 AM_ROUT_DEF L61;
 STR k;
 INT L71 = INT_zero;
 FLISTA725283029 L81;
 AM_LOCAL_EXPR l;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT i1 = INT_zero;
 FLISTSTR L121;
 AM_LOCAL_EXPR lv;
 FLISTA725283029 L131;
 STR def;
 CODE_FILE plus_self;
 dSTR plus_s;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self3;
 dSTR plus_s1;
 CODE_FILE ret_val2;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE ret_val3;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 PRINT_OB register_self;
 SIG register_s;
 STR ret_val4;
 STR plus_self7;
 INT plus_arg = INT_zero;
 STR ret_val5;
 STR s;
 INT str_self = INT_zero;
 STR ret_val6;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val7;
 FSTR str_self1;
 STR ret_val8;
 CODE_FILE plus_self8;
 dSTR plus_s5;
 CODE_FILE ret_val9;
 CODE_FILE plus_self9;
 dSTR plus_s6;
 CODE_FILE ret_val10;
 CODE_FILE plus_self10;
 dSTR plus_s7;
 CODE_FILE ret_val11;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val12;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val13;
 CODE_FILE plus_self11;
 dSTR plus_s8;
 CODE_FILE ret_val14;
 CODE_FILE plus_self12;
 dSTR plus_s9;
 CODE_FILE plus_self13;
 dSTR plus_s10;
 CODE_FILE ret_val15;
 CODE_FILE plus_self14;
 dSTR plus_s11;
 CODE_FILE ret_val16;
 CODE_FILE plus_self15;
 dSTR plus_s12;
 CODE_FILE ret_val17;
 SFILE_ID file_in_self1 = SFILE_ID_zero;
 STR ret_val18;
 FLISTSTR aget_self1;
 INT aget_ind1 = INT_zero;
 STR ret_val19;
 CODE_FILE plus_self16;
 dSTR plus_s13;
 CODE_FILE ret_val20;
 CODE_FILE plus_self17;
 dSTR plus_s14;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val21;
 CODE_FILE plus_self18;
 dSTR plus_s15;
 CODE_FILE ret_val22;
 CODE_FILE plus_self19;
 dSTR plus_s16;
 CODE_FILE plus_self20;
 dSTR plus_s17;
 CGEN bind_c_name_self;
 STR bind_c_name_name;
 STR ret_val23;
 STR res = ((STR) NULL);
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val24;
 CODE_FILE plus_self21;
 dSTR plus_s18;
 CODE_FILE ret_val25;
 CODE_FILE plus_self22;
 dSTR plus_s19;
 CODE_FILE ret_val26;
 CODE_FILE plus_self23;
 dSTR plus_s20;
 INT L141 = INT_zero;
 STR plus_self24;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val27;
 STR s1;
 CHAR str_self2 = CHAR_zero;
 STR ret_val28;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val29;
 STR r;
 STR plus_self25;
 STR plus_sarg2;
 STR ret_val30;
 CODE_FILE plus_self26;
 dSTR plus_s21;
 CODE_FILE plus_self27;
 dSTR plus_s22;
 CODE_FILE plus_self28;
 dSTR plus_s23;
 CGEN in_self;
 STR plus_self29;
 STR plus_sarg3;
 STR ret_val31;
 AM_FORMAL_ARG tp_self;
 dTP ret_val32;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val33;
 FLISTSTR create_self1;
 FLISTSTR ret_val34;
 INT L151 = INT_zero;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns;
 STR ret_val35;
 STR plus_self30;
 STR plus_sarg4;
 STR ret_val36;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val37 = BOOL_zero;
 CGEN mang_self3;
 OB mang_ob3;
 OB mang_ns1;
 STR ret_val38;
 STR plus_self31;
 STR plus_sarg5;
 STR ret_val39;
 STR plus_self32;
 INT plus_arg2 = INT_zero;
 STR ret_val40;
 STR s2;
 INT str_self3 = INT_zero;
 STR ret_val41;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val42;
 FSTR str_self4;
 STR ret_val43;
 STR plus_self33;
 STR plus_sarg6;
 STR ret_val44;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT str_self5 = INT_zero;
 STR ret_val45;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val46;
 FSTR str_self6;
 STR ret_val47;
 CGEN defer_self;
 STR defer_s;
 CGEN defer_self1;
 STR defer_s1;
 SIG is_forked_self;
 BOOL ret_val48 = BOOL_zero;
 CGEN defer_self2;
 STR defer_s2;
 CGEN defer_self3;
 STR defer_s3;
 CGEN defer_self4;
 STR defer_s4;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val49;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CODE_FILE plus_self34;
 dSTR plus_s24;
 AM_LOCAL_EXPR tp_self3;
 dTP ret_val50;
 AM_LOCAL_EXPR tp_self4;
 dTP ret_val51;
 AM_LOCAL_EXPR tp_self5;
 dTP ret_val52;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val53;
 CODE_FILE plus_self35;
 dSTR plus_s25;
 CODE_FILE ret_val54;
 CODE_FILE plus_self36;
 dSTR plus_s26;
 CODE_FILE ret_val55;
 CGEN mang_self5;
 OB mang_ob5;
 OB mang_ns2;
 STR ret_val56;
 CODE_FILE plus_self37;
 dSTR plus_s27;
 CODE_FILE ret_val57;
 CODE_FILE plus_self38;
 dSTR plus_s28;
 CODE_FILE ret_val58;
 CODE_FILE plus_self39;
 dSTR plus_s29;
 CODE_FILE ret_val59;
 CODE_FILE plus_self40;
 dSTR plus_s30;
 AM_LOCAL_EXPR tp_self6;
 dTP ret_val60;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val61;
 CODE_FILE plus_self41;
 dSTR plus_s31;
 CODE_FILE ret_val62;
 CODE_FILE plus_self42;
 dSTR plus_s32;
 CODE_FILE ret_val63;
 CGEN mang_self7;
 OB mang_ob7;
 OB mang_ns3;
 STR ret_val64;
 CODE_FILE plus_self43;
 dSTR plus_s33;
 CODE_FILE ret_val65;
 CODE_FILE plus_self44;
 dSTR plus_s34;
 SIG is_forked_self1;
 BOOL ret_val66 = BOOL_zero;
 AM_LOCAL_EXPR tp_self7;
 dTP ret_val67;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val68;
 CODE_FILE plus_self45;
 dSTR plus_s35;
 CODE_FILE ret_val69;
 CODE_FILE plus_self46;
 dSTR plus_s36;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val70;
 STR plus_self47;
 STR plus_sarg7;
 STR ret_val71;
 STR plus_self48;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val72;
 STR s3;
 CHAR str_self7 = CHAR_zero;
 STR ret_val73;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val74;
 STR r1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 extern STR undefI419930828;
 FSTR L16;
 dSTR L17;
 extern STR Definitionof;
 extern STR name78;
 BOOL L18;
 BOOL L191_;
 FSTR L20;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 extern STR name79;
 FSTR L27;
 dSTR L28;
 dSTR L29;
 OB L30;
 FSTR L32;
 dSTR L33;
 extern STR F_tag_;
 INT L34;
 INT L351_;
 BOOL L36;
 BOOL L371_;
 extern STR line2;
 BOOL L38;
 BOOL L391_;
 FSTR L40;
 dSTR L42;
 dSTR L43;
 OB L44;
 BOOL L45;
 BOOL L461_;
 FSTR L47;
 dSTR L48;
 extern STR name71;
 BOOL L49;
 BOOL L501_;
 FSTR L52;
 dSTR L53;
 STR L541_;
 BOOL L55;
 BOOL L561_;
 FSTR L57;
 dSTR L58;
 extern STR name73;
 FSTR L59;
 dSTR L60;
 extern STR line1;
 BOOL L62;
 BOOL L631_;
 FSTR L64;
 dSTR L65;
 dSTR L66;
 OB L67;
 BOOL L68;
 BOOL L691_;
 FSTR L70;
 dSTR L72;
 extern STR name71;
 BOOL L73;
 BOOL L741_;
 FSTR L75;
 dSTR L76;
 STR L771_;
 BOOL L78;
 BOOL L791_;
 FSTR L80;
 dSTR L82;
 extern STR name83;
 FSTR L83;
 dSTR L84;
 ARRAYSTR* L85;
 ARRAYSTR* L86;
 BOOL L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 FSTR L92;
 dSTR L93;
 dSTR L94;
 OB L95;
 FSTR L96;
 dSTR L97;
 extern STR void6;
 FSTR L98;
 dSTR L99;
 BOOL L100;
 BOOL L1021_;
 FSTR L103;
 dSTR L104;
 BOOL L105;
 BOOL L1061_;
 FSTR L107;
 dSTR L108;
 dSTR L109;
 OB L110;
 FSTR L111;
 dSTR L112;
 extern STR name29;
 INT L1131_br;
 STR aL1131_;
 INT L1141_br;
 STR aL1141_;
 STR L115;
 STR L116;
 STR L117;
 STR L118;
 INT L119;
 OB L120;
 INT L122;
 FSTR L124;
 dSTR L125;
 INT L1261_;
 extern STR struct167654362;
 FSTR L127;
 dSTR L128;
 extern STR name84;
 FSTR L129;
 dSTR L130;
 INT L132;
 INT L1331_;
 INT L135;
 INT L1361_;
 INT L137;
 INT L1381_;
 INT L1341_,L1341_m;
 AM_FORMAL_ARG L1391_;
 dMODE L140;
 dMODE L142;
 extern STR name58;
 INT L1431_;
 INT L144;
 STR L1451_;
 INT L1461_br;
 AM_FORMAL_ARG aL1461_;
 INT L1471_;
 extern STR void7;
 AM_FORMAL_ARG L148;
 AM_FORMAL_ARG L149;
 BOOL L150;
 INT L152;
 INT L153;
 BOOL L1541_;
 extern STR NULL2;
 INT L1551_;
 INT L1561_;
 AM_LOCAL_EXPR L157;
 BOOL L158;
 BOOL L159;
 BOOL L1601_;
 BOOL L161;
 BOOL L1621_;
 extern STR void7;
 INT L1631_;
 extern STR void_local_frame;
 BOOL L164;
 BOOL L1651_;
 extern STR name86;
 extern STR struct1270974273;
 INT L1671_;
 INT L1661_,L1661_m;
 INT L168;
 extern STR S_local_frame;
 BOOL L169;
 BOOL L1701_;
 STR L171;
 extern STR name15;
 STR L172;
 STR L173;
 extern STR name16;
 extern STR FFfunc;
 extern STR name16;
 BOOL L174;
 BOOL L175;
 BOOL L176;
 extern STR FFprevNULL;
 extern STR FFprev530765537;
 extern STR FFargs931318564;
 BOOL L177;
 BOOL L1781_;
 AM_LOCAL_EXPR L179;
 extern STR volatile3;
 FSTR L180;
 dSTR L181;
 BOOL L182;
 dTP L183;
 BOOL L184;
 BOOL L1851_;
 FSTR L186;
 dSTR L187;
 dSTR L188;
 OB L189;
 BOOL L190;
 BOOL L1921_;
 FSTR L193;
 dSTR L194;
 BOOL L195;
 BOOL L1961_;
 FSTR L197;
 dSTR L198;
 extern STR name36;
 BOOL L199;
 BOOL L2001_;
 FSTR L201;
 dSTR L202;
 BOOL L203;
 BOOL L2041_;
 FSTR L205;
 dSTR L206;
 dSTR L207;
 OB L208;
 FSTR L209;
 dSTR L210;
 BOOL L211;
 BOOL L2121_;
 FSTR L213;
 dSTR L214;
 dSTR L215;
 OB L216;
 BOOL L217;
 BOOL L2181_;
 FSTR L219;
 dSTR L220;
 BOOL L221;
 BOOL L2221_;
 FSTR L223;
 dSTR L224;
 dSTR L225;
 OB L226;
 FSTR L227;
 dSTR L228;
 BOOL L229;
 BOOL L230;
 AM_FORMAL_ARG L2311_;
 AM_FORMAL_ARG L232;
 BOOL L233;
 BOOL L2341_;
 FSTR L235;
 dSTR L236;
 extern STR self4;
 FSTR L237;
 dSTR L238;
 extern STR self5;
 extern STR self_to_be;
 STR L239;
 INT L240;
 OB L242;
 INT L243;
 SATTR(self,saw_outer_return,FALSE);
 sig1 = ATTR(f,sig1);
 CGEN_newline(self);
 CGEN_newline(self);
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) ((STR) &undefI419930828));
 L16 = ATTR(plus_self,ntext);
 L17 = plus_s;
 SATTR(plus_self,ntext,FSTR_p1752847026(L16, (*dSTR_strrSTR[TAG(L17)])(L17)));
 comment_self = self;
 comment_f = ATTR(self,code_c);
 plus_self1 = ((STR) &Definitionof);
 plus_sarg = ATTR(sig1,str);
 ret_val = STR_ap2004550586(plus_self1, plus_sarg);
 plus_self2 = ret_val;
 plus_sarg1 = ATTR(self,func_name_commen);
 ret_val1 = STR_ap2004550586(plus_self2, plus_sarg1);
 comment_com = ret_val1;
 if (ATTR(comment_self,pretty)) {
  plus_self3 = comment_f;
  plus_s1 = ((dSTR) ((STR) &name78));
  L18 = (plus_s1==((dSTR) NULL));
  L191_=!(L18); 
  if (L191_) {
   L20 = ATTR(plus_self3,ntext);
   L22 = plus_s1;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L20, (*dSTR_strrSTR[TAG(L22)])(L22)));
  }
  ret_val2 = plus_self3;
  plus_self4 = ret_val2;
  plus_s2 = ((dSTR) comment_com);
  L23 = (plus_s2==((dSTR) NULL));
  L241_=!(L23); 
  if (L241_) {
   L25 = ATTR(plus_self4,ntext);
   L26 = plus_s2;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
  }
  ret_val3 = plus_self4;
  plus_self5 = ret_val3;
  plus_s3 = ((dSTR) ((STR) &name79));
  L27 = ATTR(plus_self5,ntext);
  L28 = plus_s3;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
 }
 plus_self6 = ATTR(self,code_c);
 L30=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L30==NULL) FATAL("Unable to allocate more memory");
 memset(L30,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L30)->header.tag=CHAR_tag;
 L29 = (dSTR)((CHAR_boxed) L30);
 ((CHAR_boxed) L29)->immutable_part = '\n';
 plus_s4 = L29;
 L32 = ATTR(plus_self6,ntext);
 L33 = plus_s4;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L32, (*dSTR_strrSTR[TAG(L33)])(L33)));
 if (ATTR(self,func_tables)) {
  register_self = ((PRINT_OB) NULL);
  register_s = ATTR(f,sig1);
  PRINT_OB_funcs = FLISTS69519306(PRINT_OB_funcs, register_s);
  plus_self7 = ((STR) &F_tag_);
  L34 = FLISTS228922656(PRINT_OB_funcs);
  L351_=INTMINUS(L34,1); 
  plus_arg = L351_;
  str_self = plus_arg;
  clear_self = buf1;
  L36 = (clear_self==((FSTR) NULL));
  L371_=!(L36); 
  if (L371_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val7 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val7;
  str_self1 = buf1;
  ret_val8 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val6 = ret_val8;
  s = ret_val6;
  ret_val5 = STR_ap1077157958(plus_self7, s, TRUE);
  ret_val4 = ret_val5;
  rout_index = ret_val4;
 }
 if (ATTR(self,debug1)) {
  plus_self8 = ATTR(self,code_c);
  plus_s5 = ((dSTR) ((STR) &line2));
  L38 = (plus_s5==((dSTR) NULL));
  L391_=!(L38); 
  if (L391_) {
   L40 = ATTR(plus_self8,ntext);
   L42 = plus_s5;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L40, (*dSTR_strrSTR[TAG(L42)])(L42)));
  }
  ret_val9 = plus_self8;
  plus_self9 = ret_val9;
  L44=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  memset(L44,0,sizeof(struct INT_boxed_struct));
  ((OB)L44)->header.tag=INT_tag;
  L43 = (dSTR)((INT_boxed) L44);
  ((INT_boxed) L43)->immutable_part = SFILE_2070755978(ATTR(f,source1));
  plus_s6 = L43;
  L45 = (plus_s6==((dSTR) NULL));
  L461_=!(L45); 
  if (L461_) {
   L47 = ATTR(plus_self9,ntext);
   L48 = plus_s6;
   SATTR(plus_self9,ntext,FSTR_p1752847026(L47, (*dSTR_strrSTR[TAG(L48)])(L48)));
  }
  ret_val10 = plus_self9;
  plus_self10 = ret_val10;
  plus_s7 = ((dSTR) ((STR) &name71));
  L49 = (plus_s7==((dSTR) NULL));
  L501_=!(L49); 
  if (L501_) {
   L52 = ATTR(plus_self10,ntext);
   L53 = plus_s7;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L52, (*dSTR_strrSTR[TAG(L53)])(L53)));
  }
  ret_val11 = plus_self10;
  plus_self11 = ret_val11;
  file_in_self = ATTR(f,source1);
  aget_self = SFILE_ID_files;
  aget_ind = SFILE_1102877860(file_in_self);
  L541_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
  ret_val13 = L541_;
  ret_val12 = ret_val13;
  plus_s8 = ((dSTR) ret_val12);
  L55 = (plus_s8==((dSTR) NULL));
  L561_=!(L55); 
  if (L561_) {
   L57 = ATTR(plus_self11,ntext);
   L58 = plus_s8;
   SATTR(plus_self11,ntext,FSTR_p1752847026(L57, (*dSTR_strrSTR[TAG(L58)])(L58)));
  }
  ret_val14 = plus_self11;
  plus_self12 = ret_val14;
  plus_s9 = ((dSTR) ((STR) &name73));
  L59 = ATTR(plus_self12,ntext);
  L60 = plus_s9;
  SATTR(plus_self12,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
  SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
 }
 else {
  if (ATTR(self,pretty)) {
   plus_self13 = ATTR(self,code_c);
   plus_s10 = ((dSTR) ((STR) &line1));
   L62 = (plus_s10==((dSTR) NULL));
   L631_=!(L62); 
   if (L631_) {
    L64 = ATTR(plus_self13,ntext);
    L65 = plus_s10;
    SATTR(plus_self13,ntext,FSTR_p1752847026(L64, (*dSTR_strrSTR[TAG(L65)])(L65)));
   }
   ret_val15 = plus_self13;
   plus_self14 = ret_val15;
   L67=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L67==NULL) FATAL("Unable to allocate more memory");
   memset(L67,0,sizeof(struct INT_boxed_struct));
   ((OB)L67)->header.tag=INT_tag;
   L66 = (dSTR)((INT_boxed) L67);
   ((INT_boxed) L66)->immutable_part = SFILE_2070755978(ATTR(f,source1));
   plus_s11 = L66;
   L68 = (plus_s11==((dSTR) NULL));
   L691_=!(L68); 
   if (L691_) {
    L70 = ATTR(plus_self14,ntext);
    L72 = plus_s11;
    SATTR(plus_self14,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
   }
   ret_val16 = plus_self14;
   plus_self15 = ret_val16;
   plus_s12 = ((dSTR) ((STR) &name71));
   L73 = (plus_s12==((dSTR) NULL));
   L741_=!(L73); 
   if (L741_) {
    L75 = ATTR(plus_self15,ntext);
    L76 = plus_s12;
    SATTR(plus_self15,ntext,FSTR_p1752847026(L75, (*dSTR_strrSTR[TAG(L76)])(L76)));
   }
   ret_val17 = plus_self15;
   plus_self16 = ret_val17;
   file_in_self1 = ATTR(f,source1);
   aget_self1 = SFILE_ID_files;
   aget_ind1 = SFILE_1102877860(file_in_self1);
   L771_=(STR)ARR((FLISTSTR)aget_self1,aget_ind1); 
   ret_val19 = L771_;
   ret_val18 = ret_val19;
   plus_s13 = ((dSTR) ret_val18);
   L78 = (plus_s13==((dSTR) NULL));
   L791_=!(L78); 
   if (L791_) {
    L80 = ATTR(plus_self16,ntext);
    L82 = plus_s13;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L82)])(L82)));
   }
   ret_val20 = plus_self16;
   plus_self17 = ret_val20;
   plus_s14 = ((dSTR) ((STR) &name83));
   L83 = ATTR(plus_self17,ntext);
   L84 = plus_s14;
   SATTR(plus_self17,ntext,FSTR_p1752847026(L83, (*dSTR_strrSTR[TAG(L84)])(L84)));
   SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
  }
 }
 L85 = &arg_list;
 L86 = &type_list;
 CGEN_g628546529(self, f, L85, L86);
 L87 = (ATTR(ATTR(f,sig1),ret)==((dTP) NULL));
 L881_=!(L87); 
 has_return = L881_;
 final_arg_list = arg_list;
 SATTR(self,current_c_arg_li,final_arg_list);
 if (has_return) {
  plus_self18 = ATTR(self,code_c);
  mang_self = self;
  mang_ob = ((OB) ATTR(sig1,ret));
  ret_val21 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_s15 = ((dSTR) ret_val21);
  L89 = (plus_s15==((dSTR) NULL));
  L901_=!(L89); 
  if (L901_) {
   L92 = ATTR(plus_self18,ntext);
   L93 = plus_s15;
   SATTR(plus_self18,ntext,FSTR_p1752847026(L92, (*dSTR_strrSTR[TAG(L93)])(L93)));
  }
  ret_val22 = plus_self18;
  plus_self19 = ret_val22;
  L95=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L95==NULL) FATAL("Unable to allocate more memory");
  memset(L95,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L95)->header.tag=CHAR_tag;
  L94 = (dSTR)((CHAR_boxed) L95);
  ((CHAR_boxed) L94)->immutable_part = ' ';
  plus_s16 = L94;
  L96 = ATTR(plus_self19,ntext);
  L97 = plus_s16;
  SATTR(plus_self19,ntext,FSTR_p1752847026(L96, (*dSTR_strrSTR[TAG(L97)])(L97)));
 }
 else {
  plus_self20 = ATTR(self,code_c);
  plus_s17 = ((dSTR) ((STR) &void6));
  L98 = ATTR(plus_self20,ntext);
  L99 = plus_s17;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L98, (*dSTR_strrSTR[TAG(L99)])(L99)));
 }
 bind_c_name_self = self;
 bind_c_name_name = ATTR(sig1,name1).str;
 res = bind_c_name_name;
 ret_val23 = res;
 name111 = ret_val23;
 MANGLE1827205184(ATTR(self,mangler), ((OB) sig1), name111, ((OB) NULL));
 plus_self21 = ATTR(self,code_c);
 mang_self1 = self;
 mang_ob1 = ((OB) sig1);
 ret_val24 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_s18 = ((dSTR) ret_val24);
 L100 = (plus_s18==((dSTR) NULL));
 L1021_=!(L100); 
 if (L1021_) {
  L103 = ATTR(plus_self21,ntext);
  L104 = plus_s18;
  SATTR(plus_self21,ntext,FSTR_p1752847026(L103, (*dSTR_strrSTR[TAG(L104)])(L104)));
 }
 ret_val25 = plus_self21;
 plus_self22 = ret_val25;
 plus_s19 = ((dSTR) ATTR(self,func_name_suffix));
 L105 = (plus_s19==((dSTR) NULL));
 L1061_=!(L105); 
 if (L1061_) {
  L107 = ATTR(plus_self22,ntext);
  L108 = plus_s19;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L107, (*dSTR_strrSTR[TAG(L108)])(L108)));
 }
 ret_val26 = plus_self22;
 plus_self23 = ret_val26;
 L110=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L110==NULL) FATAL("Unable to allocate more memory");
 memset(L110,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L110)->header.tag=CHAR_tag;
 L109 = (dSTR)((CHAR_boxed) L110);
 ((CHAR_boxed) L109)->immutable_part = '(';
 plus_s20 = L109;
 L111 = ATTR(plus_self23,ntext);
 L112 = plus_s20;
 SATTR(plus_self23,ntext,FSTR_p1752847026(L111, (*dSTR_strrSTR[TAG(L112)])(L112)));
 CODE_F1291349628(ATTR(self,code_c), sig1);
 out_args = FALSE;
 {
  BOOL f_L1131_ = TRUE;
  BOOL f_L1141_ = TRUE;
  struct STR_se405450305_frame_struct other1_0;
STR_se405450305_frame noname1 = &other1_0;
  /* loop index variable */
  L141 = 0;
  L11 = final_arg_list;
  L21 = type_list;
  L31 = ((STR) &name29);
  L1131_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  L1141_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
  noname1->self = L31;
  noname1->state = 0;
  while (1) {
   if(L141>=L1131_br)  goto after_loop; 
   aL1131_=ARR((ARRAYSTR)L11,L141); 
   arg_str = aL1131_;
   if(L141>=L1141_br)  goto after_loop; 
   aL1141_=ARR((ARRAYSTR)L21,L141); 
   type_str = aL1141_;
   plus_self26 = ATTR(self,code_c);
   plus_self24 = type_str;
   plus_arg1 = ' ';
   str_self2 = plus_arg1;
   create_self = ((STR) NULL);
   create_c = str_self2;
   L119 = 1;
   L122=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L119)*sizeof(CHAR);
   L120=ZALLOC_LEAF_BIG(L122);
   if (L120==NULL) FATAL("Unable to allocate more memory");
   memset(L120,0,L122);
   ((OB)L120)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L120)->header.destroyed=0;
#endif

   L118 = ((STR) L120);
   L118->asize = L119;
   r = L118;
   SARR((STR)r,0,create_c); 
   ;
   ret_val29 = r;
   ret_val28 = ret_val29;
   s1 = ret_val28;
   ret_val27 = STR_ap1077157958(plus_self24, s1, TRUE);
   plus_self25 = ret_val27;
   plus_sarg2 = arg_str;
   ret_val30 = STR_ap2004550586(plus_self25, plus_sarg2);
   noname1->arg1 = ((dSTR) ret_val30);
   L117 = STR_se405450305(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_s21 = ((dSTR) L117);
   L124 = ATTR(plus_self26,ntext);
   L125 = plus_s21;
   SATTR(plus_self26,ntext,FSTR_p1752847026(L124, (*dSTR_strrSTR[TAG(L125)])(L125)));
   L1261_=INTPLUS(L141,1); 
   L141 = L1261_;
  }
 }
 after_loop: ;
 if (ATTR(self,func_tables)) {
  plus_self27 = ATTR(self,code_c);
  plus_s22 = ((dSTR) ((STR) &struct167654362));
  L127 = ATTR(plus_self27,ntext);
  L128 = plus_s22;
  SATTR(plus_self27,ntext,FSTR_p1752847026(L127, (*dSTR_strrSTR[TAG(L128)])(L128)));
 }
 plus_self28 = ATTR(self,code_c);
 plus_s23 = ((dSTR) ((STR) &name84));
 L129 = ATTR(plus_self28,ntext);
 L130 = plus_s23;
 SATTR(plus_self28,ntext,FSTR_p1752847026(L129, (*dSTR_strrSTR[TAG(L130)])(L130)));
 in_self = self;
 L132 = ATTR(in_self,indent);
 L1331_=INTPLUS(L132,1); 
 SATTR(in_self,indent,L1331_);
 {
  BOOL f_L1341_ = TRUE;
  while (1) {
   if (f_L1341_) {
    f_L1341_ = FALSE;
    L41 = 1;
    L1361_=ASIZE((AM_ROUT_DEF)f); 
    L137 = L1361_;
    L1381_=INTMINUS(L137,1); 
    L51 = L1381_;
    L1341_=L41-1;L1341_m=L51; 
   }
   if(L1341_++>=L1341_m)  goto after_loop1; 
   i = L1341_;
   L1391_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i); 
   e = L1391_;
   L140 = ATTR(e,mode1);
   if ((*dMODE_2144735300[TAG(L140)])(L140)) {
    L142 = ATTR(e,mode1);
    if ((*dMODE_814247358[TAG(L142)])(L142, ((dMODE) MODES_inout_mode))) {
     plus_self29 = ((STR) &name58);
     L1431_=INTMINUS(i,1); 
     L144 = L1431_;
     L1451_=(STR)ARR((ARRAYSTR)final_arg_list,L144); 
     plus_sarg3 = L1451_;
     ret_val31 = STR_ap2004550586(plus_self29, plus_sarg3);
     init_str = ret_val31;
    }
    else {
     tp_self = e;
     tp_self1 = ATTR(tp_self,expr);
     ret_val33 = ATTR(tp_self1,tp_at);
     ret_val32 = ret_val33;
     init_str = CGEN_d815167520(self, ret_val32);
    }
    local11 = CGEN_d1279589030(self, e, ATTR(f,sig1), init_str);
   }
  }
 }
 after_loop1: ;
 if (ATTR(self,func_tables)) {
  create_self1 = ((FLISTSTR) NULL);
  ret_val34 = ((FLISTSTR) NULL);
  ll = ret_val34;
  cn = 0;
  {
   BOOL f_L1461_ = TRUE;
   BOOL f_L1471_ = TRUE;
   /* loop index variable */
   L151 = 0;
   L61 = f;
   L71 = 1;
   L1461_br=L61==NULL?0:ASIZE((AM_ROUT_DEF)L61); 
   L1471_=L71-1; 
   while (1) {
    plus_self30 = ((STR) &void7);
    mang_self2 = self;
    if(L151>=L1461_br)  goto after_loop2; 
    aL1461_=ARR((AM_ROUT_DEF)L61,L151); 
    L149=aL1461_;
    mang_ob2 = ((OB) ATTR(L149,expr));
    mang_ns = ((OB) sig1);
    ret_val35 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns);
    plus_sarg4 = ret_val35;
    ret_val36 = STR_ap2004550586(plus_self30, plus_sarg4);
    k = ret_val36;
    if (ATTR(f,is_external)) {
     L1471_++; 
     L153 = L1471_;
     L1541_=(L153)==(1); 
     L150 = L1541_;
    } else {
     L150 = FALSE;
    }
    if (L150) {
     k = ((STR) &NULL2);
    }
    ll = FLISTS1425610062(ll, k);
    L1551_=INTPLUS(cn,1); 
    cn = L1551_;
    L1561_=INTPLUS(L151,1); 
    L151 = L1561_;
   }
  }
  after_loop2: ;
  {
   struct FLISTA2119642552_frame_struct other4_0;
FLISTA2119642552_frame noname2 = &other4_0;
   L81 = ATTR(f,locals1);
   noname2->self = L81;
   noname2->state = 0;
   while (1) {
    L157 = FLISTA2119642552(noname2);
    if (noname2->state == -1) {
     goto after_loop3;
    }
    l = L157;
    L159 = (ATTR(l,name1).str==(STR)0);
    L1601_=!(L159); 
    if (L1601_) {
     is_eq_self = l;
     is_eq_a = ((OB) ATTR(f,rres));
     ret_val37 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
     L161 = ret_val37;
     L1621_=!(L161); 
     L158 = L1621_;
    } else {
     L158 = FALSE;
    }
    if (L158) {
     SATTR(l,needs_init,TRUE);
     plus_self31 = ((STR) &void7);
     mang_self3 = self;
     mang_ob3 = ((OB) l);
     mang_ns1 = ((OB) sig1);
     ret_val38 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, mang_ns1);
     plus_sarg5 = ret_val38;
     ret_val39 = STR_ap2004550586(plus_self31, plus_sarg5);
     ll = FLISTS1425610062(ll, ret_val39);
     L1631_=INTPLUS(cn,1); 
     cn = L1631_;
    }
   }
  }
  after_loop3: ;
  ndefer_self = self;
  plus_self32 = ((STR) &void_local_frame);
  plus_arg2 = cn;
  str_self3 = plus_arg2;
  clear_self1 = buf2;
  L164 = (clear_self1==((FSTR) NULL));
  L1651_=!(L164); 
  if (L1651_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self3;
  str_in_s1 = buf2;
  ret_val42 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val42;
  str_self4 = buf2;
  ret_val43 = STR_fr1097270334(((STR) NULL), str_self4);
  ret_val41 = ret_val43;
  s2 = ret_val41;
  ret_val40 = STR_ap1077157958(plus_self32, s2, TRUE);
  plus_self33 = ret_val40;
  plus_sarg6 = ((STR) &name86);
  ret_val44 = STR_ap2004550586(plus_self33, plus_sarg6);
  ndefer_s = ret_val44;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  ndefer_self1 = self;
  ndefer_s1 = ((STR) &struct1270974273);
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  {
   BOOL f_L1661_ = TRUE;
   struct FLISTS1171631134_frame_struct other5_0;
FLISTS1171631134_frame noname3 = &other5_0;
   L91 = 0;
   L1671_=INTMINUS(cn,1); 
   L101 = L1671_;
   L121 = ll;
   L1661_=L91-1;L1661_m=L101; 
   noname3->self = L121;
   noname3->state = 0;
   while (1) {
    if(L1661_++>=L1661_m)  goto after_loop4; 
    i1 = L1661_;
    defer_self = self;
    str_self5 = i1;
    clear_self2 = buf3;
    L169 = (clear_self2==((FSTR) NULL));
    L1701_=!(L169); 
    if (L1701_) {
     SATTR(clear_self2,loc,0);
    }
    str_in_self2 = str_self5;
    str_in_s2 = buf3;
    ret_val46 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
    buf3 = ret_val46;
    str_self6 = buf3;
    ret_val47 = STR_fr1097270334(((STR) NULL), str_self6);
    ret_val45 = ret_val47;
    L171 = ret_val45;
    L172 = FLISTS1171631134(noname3);
    if (noname3->state == -1) {
     goto after_loop4;
    }
    L173 = L172;
    defer_s = STR_ap44172742(((STR) &S_local_frame), L171, ((STR) &name15), L173, ((STR) &name16));
    SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
   }
  }
  after_loop4: ;
  defer_self1 = self;
  defer_s1 = STR_ap1693864410(((STR) &FFfunc), rout_index, ((STR) &name16));
  SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
  if (ATTR(f,is_external)) {
   L174 = TRUE;
  } else {
   is_forked_self = sig1;
   if (ATTR(is_forked_self,is_par_routine)) {
    L176 = TRUE;
   } else {
    L176 = ATTR(is_forked_self,is_fork_routine);
   }
   if (L176) {
    L175 = TRUE;
   } else {
    L175 = ATTR(is_forked_self,is_attach_routin);
   }
   ret_val48 = L175;
   L174 = ret_val48;
  }
  if (L174) {
   defer_self2 = self;
   defer_s2 = ((STR) &FFprevNULL);
   SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
  }
  else {
   defer_self3 = self;
   defer_s3 = ((STR) &FFprev530765537);
   SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
  }
  defer_self4 = self;
  defer_s4 = ((STR) &FFargs931318564);
  SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
 }
 L177 = (ATTR(f,locals1)==((FLISTA725283029) NULL));
 L1781_=!(L177); 
 if (L1781_) {
  {
   struct FLISTA2119642552_frame_struct other6_0;
FLISTA2119642552_frame noname4 = &other6_0;
   L131 = ATTR(f,locals1);
   noname4->self = L131;
   noname4->state = 0;
   while (1) {
    L179 = FLISTA2119642552(noname4);
    if (noname4->state == -1) {
     goto after_loop5;
    }
    lv = L179;
    uses_tp_self = ATTR(self,code_c);
    tp_self2 = lv;
    ret_val49 = ATTR(tp_self2,tp_at);
    uses_tp_t = ret_val49;
    CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
    CGEN_newline(self);
    if (ATTR(lv,is_volatile)) {
     plus_self34 = ATTR(self,code_c);
     plus_s24 = ((dSTR) ((STR) &volatile3));
     L180 = ATTR(plus_self34,ntext);
     L181 = plus_s24;
     SATTR(plus_self34,ntext,FSTR_p1752847026(L180, (*dSTR_strrSTR[TAG(L181)])(L181)));
    }
    if (ATTR(lv,needs_init)) {
     L182 = TRUE;
    } else {
     tp_self3 = lv;
     ret_val50 = ATTR(tp_self3,tp_at);
     L183 = ret_val50;
     L182 = (*dTP_is1334578382[TAG(L183)])(L183);
    }
    if (L182) {
     tp_self4 = lv;
     ret_val51 = ATTR(tp_self4,tp_at);
     def = CGEN_d815167520(self, ret_val51);
     plus_self35 = ATTR(self,code_c);
     mang_self4 = self;
     tp_self5 = lv;
     ret_val52 = ATTR(tp_self5,tp_at);
     mang_ob4 = ((OB) ret_val52);
     ret_val53 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
     plus_s25 = ((dSTR) ret_val53);
     L184 = (plus_s25==((dSTR) NULL));
     L1851_=!(L184); 
     if (L1851_) {
      L186 = ATTR(plus_self35,ntext);
      L187 = plus_s25;
      SATTR(plus_self35,ntext,FSTR_p1752847026(L186, (*dSTR_strrSTR[TAG(L187)])(L187)));
     }
     ret_val54 = plus_self35;
     plus_self36 = ret_val54;
     L189=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L189==NULL) FATAL("Unable to allocate more memory");
     memset(L189,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L189)->header.tag=CHAR_tag;
     L188 = (dSTR)((CHAR_boxed) L189);
     ((CHAR_boxed) L188)->immutable_part = ' ';
     plus_s26 = L188;
     L190 = (plus_s26==((dSTR) NULL));
     L1921_=!(L190); 
     if (L1921_) {
      L193 = ATTR(plus_self36,ntext);
      L194 = plus_s26;
      SATTR(plus_self36,ntext,FSTR_p1752847026(L193, (*dSTR_strrSTR[TAG(L194)])(L194)));
     }
     ret_val55 = plus_self36;
     plus_self37 = ret_val55;
     mang_self5 = self;
     mang_ob5 = ((OB) lv);
     mang_ns2 = ((OB) sig1);
     ret_val56 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, mang_ns2);
     plus_s27 = ((dSTR) ret_val56);
     L195 = (plus_s27==((dSTR) NULL));
     L1961_=!(L195); 
     if (L1961_) {
      L197 = ATTR(plus_self37,ntext);
      L198 = plus_s27;
      SATTR(plus_self37,ntext,FSTR_p1752847026(L197, (*dSTR_strrSTR[TAG(L198)])(L198)));
     }
     ret_val57 = plus_self37;
     plus_self38 = ret_val57;
     plus_s28 = ((dSTR) ((STR) &name36));
     L199 = (plus_s28==((dSTR) NULL));
     L2001_=!(L199); 
     if (L2001_) {
      L201 = ATTR(plus_self38,ntext);
      L202 = plus_s28;
      SATTR(plus_self38,ntext,FSTR_p1752847026(L201, (*dSTR_strrSTR[TAG(L202)])(L202)));
     }
     ret_val58 = plus_self38;
     plus_self39 = ret_val58;
     plus_s29 = ((dSTR) def);
     L203 = (plus_s29==((dSTR) NULL));
     L2041_=!(L203); 
     if (L2041_) {
      L205 = ATTR(plus_self39,ntext);
      L206 = plus_s29;
      SATTR(plus_self39,ntext,FSTR_p1752847026(L205, (*dSTR_strrSTR[TAG(L206)])(L206)));
     }
     ret_val59 = plus_self39;
     plus_self40 = ret_val59;
     L208=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L208==NULL) FATAL("Unable to allocate more memory");
     memset(L208,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L208)->header.tag=CHAR_tag;
     L207 = (dSTR)((CHAR_boxed) L208);
     ((CHAR_boxed) L207)->immutable_part = ';';
     plus_s30 = L207;
     L209 = ATTR(plus_self40,ntext);
     L210 = plus_s30;
     SATTR(plus_self40,ntext,FSTR_p1752847026(L209, (*dSTR_strrSTR[TAG(L210)])(L210)));
    }
    else {
     plus_self41 = ATTR(self,code_c);
     mang_self6 = self;
     tp_self6 = lv;
     ret_val60 = ATTR(tp_self6,tp_at);
     mang_ob6 = ((OB) ret_val60);
     ret_val61 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
     plus_s31 = ((dSTR) ret_val61);
     L211 = (plus_s31==((dSTR) NULL));
     L2121_=!(L211); 
     if (L2121_) {
      L213 = ATTR(plus_self41,ntext);
      L214 = plus_s31;
      SATTR(plus_self41,ntext,FSTR_p1752847026(L213, (*dSTR_strrSTR[TAG(L214)])(L214)));
     }
     ret_val62 = plus_self41;
     plus_self42 = ret_val62;
     L216=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L216==NULL) FATAL("Unable to allocate more memory");
     memset(L216,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L216)->header.tag=CHAR_tag;
     L215 = (dSTR)((CHAR_boxed) L216);
     ((CHAR_boxed) L215)->immutable_part = ' ';
     plus_s32 = L215;
     L217 = (plus_s32==((dSTR) NULL));
     L2181_=!(L217); 
     if (L2181_) {
      L219 = ATTR(plus_self42,ntext);
      L220 = plus_s32;
      SATTR(plus_self42,ntext,FSTR_p1752847026(L219, (*dSTR_strrSTR[TAG(L220)])(L220)));
     }
     ret_val63 = plus_self42;
     plus_self43 = ret_val63;
     mang_self7 = self;
     mang_ob7 = ((OB) lv);
     mang_ns3 = ((OB) sig1);
     ret_val64 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, mang_ns3);
     plus_s33 = ((dSTR) ret_val64);
     L221 = (plus_s33==((dSTR) NULL));
     L2221_=!(L221); 
     if (L2221_) {
      L223 = ATTR(plus_self43,ntext);
      L224 = plus_s33;
      SATTR(plus_self43,ntext,FSTR_p1752847026(L223, (*dSTR_strrSTR[TAG(L224)])(L224)));
     }
     ret_val65 = plus_self43;
     plus_self44 = ret_val65;
     L226=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L226==NULL) FATAL("Unable to allocate more memory");
     memset(L226,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L226)->header.tag=CHAR_tag;
     L225 = (dSTR)((CHAR_boxed) L226);
     ((CHAR_boxed) L225)->immutable_part = ';';
     plus_s34 = L225;
     L227 = ATTR(plus_self44,ntext);
     L228 = plus_s34;
     SATTR(plus_self44,ntext,FSTR_p1752847026(L227, (*dSTR_strrSTR[TAG(L228)])(L228)));
    }
   }
  }
  after_loop5: ;
 }
 is_forked_self1 = sig1;
 if (ATTR(is_forked_self1,is_par_routine)) {
  L230 = TRUE;
 } else {
  L230 = ATTR(is_forked_self1,is_fork_routine);
 }
 if (L230) {
  L229 = TRUE;
 } else {
  L229 = ATTR(is_forked_self1,is_attach_routin);
 }
 ret_val66 = L229;
 if (ret_val66) {
  CGEN_newline(self);
  plus_self45 = ATTR(self,code_c);
  mang_self8 = self;
  L2311_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,0); 
  L232=L2311_;
  tp_self7 = ATTR(L232,expr);
  ret_val67 = ATTR(tp_self7,tp_at);
  mang_ob8 = ((OB) ret_val67);
  ret_val68 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
  plus_s35 = ((dSTR) ret_val68);
  L233 = (plus_s35==((dSTR) NULL));
  L2341_=!(L233); 
  if (L2341_) {
   L235 = ATTR(plus_self45,ntext);
   L236 = plus_s35;
   SATTR(plus_self45,ntext,FSTR_p1752847026(L235, (*dSTR_strrSTR[TAG(L236)])(L236)));
  }
  ret_val69 = plus_self45;
  plus_self46 = ret_val69;
  plus_s36 = ((dSTR) ((STR) &self4));
  L237 = ATTR(plus_self46,ntext);
  L238 = plus_s36;
  SATTR(plus_self46,ntext,FSTR_p1752847026(L237, (*dSTR_strrSTR[TAG(L238)])(L238)));
  ndefer_self2 = self;
  plus_self47 = ((STR) &self5);
  cast_self = self;
  cast_dest_tp = ATTR(sig1,tp);
  cast_src_tp = ((dTP) TP_BUI1325635093);
  cast_expr = ((STR) &self_to_be);
  cast_only_boxing = FALSE;
  ret_val70 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  plus_sarg7 = ret_val70;
  ret_val71 = STR_ap2004550586(plus_self47, plus_sarg7);
  plus_self48 = ret_val71;
  plus_arg3 = ';';
  str_self7 = plus_arg3;
  create_self2 = ((STR) NULL);
  create_c1 = str_self7;
  L240 = 1;
  L243=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L240)*sizeof(CHAR);
  L242=ZALLOC_LEAF_BIG(L243);
  if (L242==NULL) FATAL("Unable to allocate more memory");
  memset(L242,0,L243);
  ((OB)L242)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L242)->header.destroyed=0;
#endif

  L239 = ((STR) L242);
  L239->asize = L240;
  r1 = L239;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val74 = r1;
  ret_val73 = ret_val74;
  s3 = ret_val73;
  ret_val72 = STR_ap1077157958(plus_self48, s3, TRUE);
  ndefer_s2 = ret_val72;
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 SATTR(self,current_arg_list,final_arg_list);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e640781662(CGEN self) {
 FLISTA112326087 L11;
 AM_SHARED_EXPR age;
 STR e;
 AM_SHARED_EXPR tp_self;
 dTP ret_val;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 AM_SHARED_EXPR tp_self1;
 dTP ret_val1;
 CODE_FILE uses_tp_self1;
 dTP uses_tp_t1;
 CODE_FILE uses_global_self;
 AM_SHARED_EXPR uses_global_age;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 AM_SHARED_EXPR tp_self2;
 dTP ret_val2;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val3;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val4;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val5;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val6;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val7;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CODE_FILE ret_val8;
 CODE_FILE plus_self6;
 dSTR plus_s6;
 CODE_FILE ret_val9;
 CODE_FILE plus_self7;
 dSTR plus_s7;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val10;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self8;
 dSTR plus_s8;
 CODE_FILE ret_val11;
 CODE_FILE plus_self9;
 dSTR plus_s9;
 CODE_FILE ret_val12;
 CODE_FILE plus_self10;
 dSTR plus_s10;
 AM_SHARED_EXPR tp_self3;
 dTP ret_val13;
 AM_SHARED_EXPR tp_self4;
 dTP ret_val14;
 CODE_FILE plus_self11;
 dSTR plus_s11;
 CODE_FILE ret_val15;
 AM_SHARED_EXPR tp_self5;
 dTP ret_val16;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val17;
 CODE_FILE plus_self12;
 dSTR plus_s12;
 CODE_FILE ret_val18;
 CODE_FILE plus_self13;
 dSTR plus_s13;
 CODE_FILE ret_val19;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val20;
 CODE_FILE plus_self14;
 dSTR plus_s14;
 CODE_FILE ret_val21;
 CODE_FILE plus_self15;
 dSTR plus_s15;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val22;
 STR plus_self16;
 STR plus_sarg;
 STR ret_val23;
 STR plus_self17;
 STR plus_sarg1;
 STR ret_val24;
 STR plus_self18;
 STR plus_sarg2;
 STR ret_val25;
 CGEN comment_self1;
 CODE_FILE comment_f1;
 STR comment_com1;
 CODE_FILE plus_self19;
 dSTR plus_s16;
 CODE_FILE ret_val26;
 CODE_FILE plus_self20;
 dSTR plus_s17;
 CODE_FILE ret_val27;
 CODE_FILE plus_self21;
 dSTR plus_s18;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val28;
 STR plus_self22;
 STR plus_sarg3;
 STR ret_val29;
 STR plus_self23;
 STR plus_sarg4;
 STR ret_val30;
 STR plus_self24;
 STR plus_sarg5;
 STR ret_val31;
 CGEN comment_self2;
 STR comment_com2;
 STR plus_self25;
 STR plus_sarg6;
 STR ret_val32;
 STR plus_self26;
 STR plus_sarg7;
 STR ret_val33;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val34;
 STR plus_self27;
 STR plus_sarg8;
 STR ret_val35;
 STR plus_self28;
 STR plus_sarg9;
 STR ret_val36;
 STR plus_self29;
 CHAR plus_arg = CHAR_zero;
 STR ret_val37;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val38;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val39;
 STR r;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val40;
 STR plus_self30;
 STR plus_sarg10;
 STR ret_val41;
 AM_SHARED_EXPR tp_self6;
 dTP ret_val42;
 STR plus_self31;
 STR plus_sarg11;
 STR ret_val43;
 STR plus_self32;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val44;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val45;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val46;
 STR r1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CODE_FILE plus_self33;
 dSTR plus_s19;
 CODE_FILE ret_val47;
 AM_SHARED_EXPR tp_self7;
 dTP ret_val48;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val49;
 CODE_FILE plus_self34;
 dSTR plus_s20;
 CODE_FILE ret_val50;
 CODE_FILE plus_self35;
 dSTR plus_s21;
 CODE_FILE ret_val51;
 CGEN mang_self10;
 OB mang_ob10;
 STR ret_val52;
 CODE_FILE plus_self36;
 dSTR plus_s22;
 CODE_FILE ret_val53;
 CODE_FILE plus_self37;
 dSTR plus_s23;
 CODE_FILE ret_val54;
 AM_SHARED_EXPR tp_self8;
 dTP ret_val55;
 CODE_FILE plus_self38;
 dSTR plus_s24;
 CODE_FILE ret_val56;
 CODE_FILE plus_self39;
 dSTR plus_s25;
 CGEN mang_self11;
 OB mang_ob11;
 STR ret_val57;
 STR plus_self40;
 STR plus_sarg12;
 STR ret_val58;
 STR plus_self41;
 STR plus_sarg13;
 STR ret_val59;
 STR plus_self42;
 STR plus_sarg14;
 STR ret_val60;
 CGEN comment_self3;
 CODE_FILE comment_f2;
 STR comment_com3;
 CODE_FILE plus_self43;
 dSTR plus_s26;
 CODE_FILE ret_val61;
 CODE_FILE plus_self44;
 dSTR plus_s27;
 CODE_FILE ret_val62;
 CODE_FILE plus_self45;
 dSTR plus_s28;
 AM_SHARED_EXPR L2;
 dSTR L3;
 OB L4;
 FSTR L5;
 dSTR L6;
 extern STR const3;
 FSTR L7;
 dSTR L8;
 BOOL L9;
 BOOL L101_;
 FSTR L12;
 dSTR L13;
 dSTR L14;
 OB L15;
 BOOL L16;
 BOOL L171_;
 FSTR L18;
 dSTR L19;
 BOOL L20;
 BOOL L211_;
 FSTR L22;
 dSTR L23;
 extern STR name36;
 BOOL L24;
 BOOL L251_;
 FSTR L26;
 dSTR L27;
 BOOL L28;
 BOOL L291_;
 FSTR L30;
 dSTR L31;
 dSTR L32;
 OB L33;
 FSTR L34;
 dSTR L35;
 extern STR Const;
 extern STR name34;
 extern STR name78;
 BOOL L36;
 BOOL L371_;
 FSTR L38;
 dSTR L39;
 BOOL L40;
 BOOL L411_;
 FSTR L42;
 dSTR L43;
 extern STR name79;
 FSTR L44;
 dSTR L45;
 BOOL L46;
 BOOL L47;
 BOOL L481_;
 BOOL L49;
 dTP L50;
 dTP L51;
 BOOL L52;
 BOOL L531_;
 dSTR L54;
 OB L55;
 BOOL L56;
 BOOL L571_;
 FSTR L58;
 dSTR L59;
 BOOL L60;
 BOOL L611_;
 FSTR L62;
 dSTR L63;
 dSTR L64;
 OB L65;
 BOOL L66;
 BOOL L671_;
 FSTR L68;
 dSTR L69;
 BOOL L70;
 BOOL L711_;
 FSTR L72;
 dSTR L73;
 dSTR L74;
 OB L75;
 FSTR L76;
 dSTR L77;
 extern STR Shared;
 extern STR name34;
 extern STR name78;
 BOOL L78;
 BOOL L791_;
 FSTR L80;
 dSTR L81;
 BOOL L82;
 BOOL L831_;
 FSTR L84;
 dSTR L85;
 extern STR name79;
 FSTR L86;
 dSTR L87;
 extern STR Initializeshared;
 extern STR name34;
 extern STR name54;
 extern STR name79;
 BOOL L88;
 BOOL L891_;
 extern STR name36;
 STR L90;
 INT L91;
 OB L92;
 INT L93;
 extern STR name36;
 STR L95;
 INT L96;
 OB L97;
 INT L98;
 dSTR L100;
 OB L102;
 BOOL L103;
 BOOL L1041_;
 FSTR L105;
 dSTR L106;
 BOOL L107;
 BOOL L1081_;
 FSTR L109;
 dSTR L110;
 dSTR L111;
 OB L112;
 BOOL L113;
 BOOL L1141_;
 FSTR L115;
 dSTR L116;
 BOOL L117;
 BOOL L1181_;
 FSTR L119;
 dSTR L120;
 extern STR name36;
 BOOL L121;
 BOOL L1221_;
 FSTR L123;
 dSTR L124;
 BOOL L125;
 BOOL L1261_;
 FSTR L127;
 dSTR L128;
 dSTR L129;
 OB L130;
 FSTR L131;
 dSTR L132;
 extern STR Shared;
 extern STR name34;
 extern STR name78;
 BOOL L133;
 BOOL L1341_;
 FSTR L135;
 dSTR L136;
 BOOL L137;
 BOOL L1381_;
 FSTR L139;
 dSTR L140;
 extern STR name79;
 FSTR L141;
 dSTR L142;
 {
  struct FLISTA120298683_frame_struct other1_0;
FLISTA120298683_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,gen),global_tbl),top_sort);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTA120298683(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   age = L2;
   uses_tp_self = ATTR(self,globals_c);
   tp_self = age;
   ret_val = ATTR(tp_self,tp_at);
   uses_tp_t = ret_val;
   CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
   uses_tp_self1 = ATTR(self,system_c);
   tp_self1 = age;
   ret_val1 = ATTR(tp_self1,tp_at);
   uses_tp_t1 = ret_val1;
   CODE_F525364269(uses_tp_self1, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t1));
   uses_global_self = ATTR(self,system_c);
   uses_global_age = age;
   SATTR(uses_global_self,globals,FSETAM381445024(ATTR(uses_global_self,globals), uses_global_age));
   if (CGEN_i1416238209(self, ATTR(age,init))) {
    SATTR(self,code_c,ATTR(self,globals_c));
    plus_self = ATTR(self,globals_c);
    L4=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    memset(L4,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L4)->header.tag=CHAR_tag;
    L3 = (dSTR)((CHAR_boxed) L4);
    ((CHAR_boxed) L3)->immutable_part = '\n';
    plus_s = L3;
    L5 = ATTR(plus_self,ntext);
    L6 = plus_s;
    SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
    e = CGEN_e1639504572(self, ATTR(age,init));
    if (ATTR(age,is_const)) {
     plus_self1 = ATTR(self,globals_c);
     plus_s1 = ((dSTR) ((STR) &const3));
     L7 = ATTR(plus_self1,ntext);
     L8 = plus_s1;
     SATTR(plus_self1,ntext,FSTR_p1752847026(L7, (*dSTR_strrSTR[TAG(L8)])(L8)));
    }
    plus_self2 = ATTR(self,globals_c);
    mang_self = self;
    tp_self2 = age;
    ret_val2 = ATTR(tp_self2,tp_at);
    mang_ob = ((OB) ret_val2);
    ret_val3 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
    plus_s2 = ((dSTR) ret_val3);
    L9 = (plus_s2==((dSTR) NULL));
    L101_=!(L9); 
    if (L101_) {
     L12 = ATTR(plus_self2,ntext);
     L13 = plus_s2;
     SATTR(plus_self2,ntext,FSTR_p1752847026(L12, (*dSTR_strrSTR[TAG(L13)])(L13)));
    }
    ret_val4 = plus_self2;
    plus_self3 = ret_val4;
    L15=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    memset(L15,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L15)->header.tag=CHAR_tag;
    L14 = (dSTR)((CHAR_boxed) L15);
    ((CHAR_boxed) L14)->immutable_part = ' ';
    plus_s3 = L14;
    L16 = (plus_s3==((dSTR) NULL));
    L171_=!(L16); 
    if (L171_) {
     L18 = ATTR(plus_self3,ntext);
     L19 = plus_s3;
     SATTR(plus_self3,ntext,FSTR_p1752847026(L18, (*dSTR_strrSTR[TAG(L19)])(L19)));
    }
    ret_val5 = plus_self3;
    plus_self4 = ret_val5;
    mang_self1 = self;
    mang_ob1 = ((OB) age);
    ret_val6 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    plus_s4 = ((dSTR) ret_val6);
    L20 = (plus_s4==((dSTR) NULL));
    L211_=!(L20); 
    if (L211_) {
     L22 = ATTR(plus_self4,ntext);
     L23 = plus_s4;
     SATTR(plus_self4,ntext,FSTR_p1752847026(L22, (*dSTR_strrSTR[TAG(L23)])(L23)));
    }
    ret_val7 = plus_self4;
    plus_self5 = ret_val7;
    plus_s5 = ((dSTR) ((STR) &name36));
    L24 = (plus_s5==((dSTR) NULL));
    L251_=!(L24); 
    if (L251_) {
     L26 = ATTR(plus_self5,ntext);
     L27 = plus_s5;
     SATTR(plus_self5,ntext,FSTR_p1752847026(L26, (*dSTR_strrSTR[TAG(L27)])(L27)));
    }
    ret_val8 = plus_self5;
    plus_self6 = ret_val8;
    plus_s6 = ((dSTR) e);
    L28 = (plus_s6==((dSTR) NULL));
    L291_=!(L28); 
    if (L291_) {
     L30 = ATTR(plus_self6,ntext);
     L31 = plus_s6;
     SATTR(plus_self6,ntext,FSTR_p1752847026(L30, (*dSTR_strrSTR[TAG(L31)])(L31)));
    }
    ret_val9 = plus_self6;
    plus_self7 = ret_val9;
    L33=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L33==NULL) FATAL("Unable to allocate more memory");
    memset(L33,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L33)->header.tag=CHAR_tag;
    L32 = (dSTR)((CHAR_boxed) L33);
    ((CHAR_boxed) L32)->immutable_part = ';';
    plus_s7 = L32;
    L34 = ATTR(plus_self7,ntext);
    L35 = plus_s7;
    SATTR(plus_self7,ntext,FSTR_p1752847026(L34, (*dSTR_strrSTR[TAG(L35)])(L35)));
    comment_self = self;
    comment_f = ATTR(self,globals_c);
    mang_self2 = self;
    mang_ob2 = ((OB) ATTR(age,class_tp));
    ret_val10 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    comment_com = STR_ap1417971546(((STR) &Const), ret_val10, ((STR) &name34), ATTR(age,name1).str);
    if (ATTR(comment_self,pretty)) {
     plus_self8 = comment_f;
     plus_s8 = ((dSTR) ((STR) &name78));
     L36 = (plus_s8==((dSTR) NULL));
     L371_=!(L36); 
     if (L371_) {
      L38 = ATTR(plus_self8,ntext);
      L39 = plus_s8;
      SATTR(plus_self8,ntext,FSTR_p1752847026(L38, (*dSTR_strrSTR[TAG(L39)])(L39)));
     }
     ret_val11 = plus_self8;
     plus_self9 = ret_val11;
     plus_s9 = ((dSTR) comment_com);
     L40 = (plus_s9==((dSTR) NULL));
     L411_=!(L40); 
     if (L411_) {
      L42 = ATTR(plus_self9,ntext);
      L43 = plus_s9;
      SATTR(plus_self9,ntext,FSTR_p1752847026(L42, (*dSTR_strrSTR[TAG(L43)])(L43)));
     }
     ret_val12 = plus_self9;
     plus_self10 = ret_val12;
     plus_s10 = ((dSTR) ((STR) &name79));
     L44 = ATTR(plus_self10,ntext);
     L45 = plus_s10;
     SATTR(plus_self10,ntext,FSTR_p1752847026(L44, (*dSTR_strrSTR[TAG(L45)])(L45)));
    }
   }
   else {
    L47 = (ATTR(age,init)==((dAM_EXPR) NULL));
    L481_=!(L47); 
    if (L481_) {
     L46 = TRUE;
    } else {
     tp_self3 = age;
     ret_val13 = ATTR(tp_self3,tp_at);
     L50 = ret_val13;
     if ((*dTP_is1334578382[TAG(L50)])(L50)) {
      tp_self4 = age;
      ret_val14 = ATTR(tp_self4,tp_at);
      L51 = ret_val14;
      L52 = (*dTP_is914808564[TAG(L51)])(L51);
      L531_=!(L52); 
      L49 = L531_;
     } else {
      L49 = FALSE;
     }
     L46 = L49;
    }
    if (L46) {
     SATTR(self,code_c,ATTR(self,system_c));
     plus_self11 = ATTR(self,globals_c);
     L55=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L55==NULL) FATAL("Unable to allocate more memory");
     memset(L55,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L55)->header.tag=CHAR_tag;
     L54 = (dSTR)((CHAR_boxed) L55);
     ((CHAR_boxed) L54)->immutable_part = '\n';
     plus_s11 = L54;
     L56 = (plus_s11==((dSTR) NULL));
     L571_=!(L56); 
     if (L571_) {
      L58 = ATTR(plus_self11,ntext);
      L59 = plus_s11;
      SATTR(plus_self11,ntext,FSTR_p1752847026(L58, (*dSTR_strrSTR[TAG(L59)])(L59)));
     }
     ret_val15 = plus_self11;
     plus_self12 = ret_val15;
     mang_self3 = self;
     tp_self5 = age;
     ret_val16 = ATTR(tp_self5,tp_at);
     mang_ob3 = ((OB) ret_val16);
     ret_val17 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
     plus_s12 = ((dSTR) ret_val17);
     L60 = (plus_s12==((dSTR) NULL));
     L611_=!(L60); 
     if (L611_) {
      L62 = ATTR(plus_self12,ntext);
      L63 = plus_s12;
      SATTR(plus_self12,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
     }
     ret_val18 = plus_self12;
     plus_self13 = ret_val18;
     L65=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L65==NULL) FATAL("Unable to allocate more memory");
     memset(L65,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L65)->header.tag=CHAR_tag;
     L64 = (dSTR)((CHAR_boxed) L65);
     ((CHAR_boxed) L64)->immutable_part = ' ';
     plus_s13 = L64;
     L66 = (plus_s13==((dSTR) NULL));
     L671_=!(L66); 
     if (L671_) {
      L68 = ATTR(plus_self13,ntext);
      L69 = plus_s13;
      SATTR(plus_self13,ntext,FSTR_p1752847026(L68, (*dSTR_strrSTR[TAG(L69)])(L69)));
     }
     ret_val19 = plus_self13;
     plus_self14 = ret_val19;
     mang_self4 = self;
     mang_ob4 = ((OB) age);
     ret_val20 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
     plus_s14 = ((dSTR) ret_val20);
     L70 = (plus_s14==((dSTR) NULL));
     L711_=!(L70); 
     if (L711_) {
      L72 = ATTR(plus_self14,ntext);
      L73 = plus_s14;
      SATTR(plus_self14,ntext,FSTR_p1752847026(L72, (*dSTR_strrSTR[TAG(L73)])(L73)));
     }
     ret_val21 = plus_self14;
     plus_self15 = ret_val21;
     L75=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L75==NULL) FATAL("Unable to allocate more memory");
     memset(L75,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L75)->header.tag=CHAR_tag;
     L74 = (dSTR)((CHAR_boxed) L75);
     ((CHAR_boxed) L74)->immutable_part = ';';
     plus_s15 = L74;
     L76 = ATTR(plus_self15,ntext);
     L77 = plus_s15;
     SATTR(plus_self15,ntext,FSTR_p1752847026(L76, (*dSTR_strrSTR[TAG(L77)])(L77)));
     comment_self1 = self;
     comment_f1 = ATTR(self,globals_c);
     plus_self16 = ((STR) &Shared);
     mang_self5 = self;
     mang_ob5 = ((OB) ATTR(age,class_tp));
     ret_val22 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
     plus_sarg = ret_val22;
     ret_val23 = STR_ap2004550586(plus_self16, plus_sarg);
     plus_self17 = ret_val23;
     plus_sarg1 = ((STR) &name34);
     ret_val24 = STR_ap2004550586(plus_self17, plus_sarg1);
     plus_self18 = ret_val24;
     plus_sarg2 = ATTR(age,name1).str;
     ret_val25 = STR_ap2004550586(plus_self18, plus_sarg2);
     comment_com1 = ret_val25;
     if (ATTR(comment_self1,pretty)) {
      plus_self19 = comment_f1;
      plus_s16 = ((dSTR) ((STR) &name78));
      L78 = (plus_s16==((dSTR) NULL));
      L791_=!(L78); 
      if (L791_) {
       L80 = ATTR(plus_self19,ntext);
       L81 = plus_s16;
       SATTR(plus_self19,ntext,FSTR_p1752847026(L80, (*dSTR_strrSTR[TAG(L81)])(L81)));
      }
      ret_val26 = plus_self19;
      plus_self20 = ret_val26;
      plus_s17 = ((dSTR) comment_com1);
      L82 = (plus_s17==((dSTR) NULL));
      L831_=!(L82); 
      if (L831_) {
       L84 = ATTR(plus_self20,ntext);
       L85 = plus_s17;
       SATTR(plus_self20,ntext,FSTR_p1752847026(L84, (*dSTR_strrSTR[TAG(L85)])(L85)));
      }
      ret_val27 = plus_self20;
      plus_self21 = ret_val27;
      plus_s18 = ((dSTR) ((STR) &name79));
      L86 = ATTR(plus_self21,ntext);
      L87 = plus_s18;
      SATTR(plus_self21,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
     }
     comment_self2 = self;
     plus_self22 = ((STR) &Initializeshared);
     mang_self6 = self;
     mang_ob6 = ((OB) ATTR(age,class_tp));
     ret_val28 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
     plus_sarg3 = ret_val28;
     ret_val29 = STR_ap2004550586(plus_self22, plus_sarg3);
     plus_self23 = ret_val29;
     plus_sarg4 = ((STR) &name34);
     ret_val30 = STR_ap2004550586(plus_self23, plus_sarg4);
     plus_self24 = ret_val30;
     plus_sarg5 = ATTR(age,name1).str;
     ret_val31 = STR_ap2004550586(plus_self24, plus_sarg5);
     comment_com2 = ret_val31;
     if (ATTR(comment_self2,pretty)) {
      ndefer_self = comment_self2;
      plus_self25 = ((STR) &name54);
      plus_sarg6 = comment_com2;
      ret_val32 = STR_ap2004550586(plus_self25, plus_sarg6);
      plus_self26 = ret_val32;
      plus_sarg7 = ((STR) &name79);
      ret_val33 = STR_ap2004550586(plus_self26, plus_sarg7);
      ndefer_s = ret_val33;
      CGEN_d695831906(ndefer_self);
      SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
     }
     L88 = (ATTR(age,init)==((dAM_EXPR) NULL));
     L891_=!(L88); 
     if (L891_) {
      SATTR(self,in_constant,TRUE);
      ndefer_self1 = self;
      mang_self7 = self;
      mang_ob7 = ((OB) age);
      ret_val34 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
      plus_self27 = ret_val34;
      plus_sarg8 = ((STR) &name36);
      ret_val35 = STR_ap2004550586(plus_self27, plus_sarg8);
      plus_self28 = ret_val35;
      plus_sarg9 = CGEN_e1639504572(self, ATTR(age,init));
      ret_val36 = STR_ap2004550586(plus_self28, plus_sarg9);
      plus_self29 = ret_val36;
      plus_arg = ';';
      str_self = plus_arg;
      create_self = ((STR) NULL);
      create_c = str_self;
      L91 = 1;
      L93=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L91)*sizeof(CHAR);
      L92=ZALLOC_LEAF_BIG(L93);
      if (L92==NULL) FATAL("Unable to allocate more memory");
      memset(L92,0,L93);
      ((OB)L92)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L92)->header.destroyed=0;
#endif

      L90 = ((STR) L92);
      L90->asize = L91;
      r = L90;
      SARR((STR)r,0,create_c); 
      ;
      ret_val39 = r;
      ret_val38 = ret_val39;
      s = ret_val38;
      ret_val37 = STR_ap1077157958(plus_self29, s, TRUE);
      ndefer_s1 = ret_val37;
      CGEN_d695831906(ndefer_self1);
      SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      SATTR(self,in_constant,FALSE);
     }
     else {
      ndefer_self2 = self;
      mang_self8 = self;
      mang_ob8 = ((OB) age);
      ret_val40 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
      plus_self30 = ret_val40;
      plus_sarg10 = ((STR) &name36);
      ret_val41 = STR_ap2004550586(plus_self30, plus_sarg10);
      plus_self31 = ret_val41;
      tp_self6 = age;
      ret_val42 = ATTR(tp_self6,tp_at);
      plus_sarg11 = CGEN_d815167520(self, ret_val42);
      ret_val43 = STR_ap2004550586(plus_self31, plus_sarg11);
      plus_self32 = ret_val43;
      plus_arg1 = ';';
      str_self1 = plus_arg1;
      create_self1 = ((STR) NULL);
      create_c1 = str_self1;
      L96 = 1;
      L98=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L96)*sizeof(CHAR);
      L97=ZALLOC_LEAF_BIG(L98);
      if (L97==NULL) FATAL("Unable to allocate more memory");
      memset(L97,0,L98);
      ((OB)L97)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L97)->header.destroyed=0;
#endif

      L95 = ((STR) L97);
      L95->asize = L96;
      r1 = L95;
      SARR((STR)r1,0,create_c1); 
      ;
      ret_val46 = r1;
      ret_val45 = ret_val46;
      s1 = ret_val45;
      ret_val44 = STR_ap1077157958(plus_self32, s1, TRUE);
      ndefer_s2 = ret_val44;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
    }
    else {
     SATTR(self,code_c,ATTR(self,globals_c));
     plus_self33 = ATTR(self,globals_c);
     L102=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L102==NULL) FATAL("Unable to allocate more memory");
     memset(L102,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L102)->header.tag=CHAR_tag;
     L100 = (dSTR)((CHAR_boxed) L102);
     ((CHAR_boxed) L100)->immutable_part = '\n';
     plus_s19 = L100;
     L103 = (plus_s19==((dSTR) NULL));
     L1041_=!(L103); 
     if (L1041_) {
      L105 = ATTR(plus_self33,ntext);
      L106 = plus_s19;
      SATTR(plus_self33,ntext,FSTR_p1752847026(L105, (*dSTR_strrSTR[TAG(L106)])(L106)));
     }
     ret_val47 = plus_self33;
     plus_self34 = ret_val47;
     mang_self9 = self;
     tp_self7 = age;
     ret_val48 = ATTR(tp_self7,tp_at);
     mang_ob9 = ((OB) ret_val48);
     ret_val49 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
     plus_s20 = ((dSTR) ret_val49);
     L107 = (plus_s20==((dSTR) NULL));
     L1081_=!(L107); 
     if (L1081_) {
      L109 = ATTR(plus_self34,ntext);
      L110 = plus_s20;
      SATTR(plus_self34,ntext,FSTR_p1752847026(L109, (*dSTR_strrSTR[TAG(L110)])(L110)));
     }
     ret_val50 = plus_self34;
     plus_self35 = ret_val50;
     L112=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L112==NULL) FATAL("Unable to allocate more memory");
     memset(L112,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L112)->header.tag=CHAR_tag;
     L111 = (dSTR)((CHAR_boxed) L112);
     ((CHAR_boxed) L111)->immutable_part = ' ';
     plus_s21 = L111;
     L113 = (plus_s21==((dSTR) NULL));
     L1141_=!(L113); 
     if (L1141_) {
      L115 = ATTR(plus_self35,ntext);
      L116 = plus_s21;
      SATTR(plus_self35,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
     }
     ret_val51 = plus_self35;
     plus_self36 = ret_val51;
     mang_self10 = self;
     mang_ob10 = ((OB) age);
     ret_val52 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, ((OB) NULL));
     plus_s22 = ((dSTR) ret_val52);
     L117 = (plus_s22==((dSTR) NULL));
     L1181_=!(L117); 
     if (L1181_) {
      L119 = ATTR(plus_self36,ntext);
      L120 = plus_s22;
      SATTR(plus_self36,ntext,FSTR_p1752847026(L119, (*dSTR_strrSTR[TAG(L120)])(L120)));
     }
     ret_val53 = plus_self36;
     plus_self37 = ret_val53;
     plus_s23 = ((dSTR) ((STR) &name36));
     L121 = (plus_s23==((dSTR) NULL));
     L1221_=!(L121); 
     if (L1221_) {
      L123 = ATTR(plus_self37,ntext);
      L124 = plus_s23;
      SATTR(plus_self37,ntext,FSTR_p1752847026(L123, (*dSTR_strrSTR[TAG(L124)])(L124)));
     }
     ret_val54 = plus_self37;
     plus_self38 = ret_val54;
     tp_self8 = age;
     ret_val55 = ATTR(tp_self8,tp_at);
     plus_s24 = ((dSTR) CGEN_d815167520(self, ret_val55));
     L125 = (plus_s24==((dSTR) NULL));
     L1261_=!(L125); 
     if (L1261_) {
      L127 = ATTR(plus_self38,ntext);
      L128 = plus_s24;
      SATTR(plus_self38,ntext,FSTR_p1752847026(L127, (*dSTR_strrSTR[TAG(L128)])(L128)));
     }
     ret_val56 = plus_self38;
     plus_self39 = ret_val56;
     L130=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L130==NULL) FATAL("Unable to allocate more memory");
     memset(L130,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L130)->header.tag=CHAR_tag;
     L129 = (dSTR)((CHAR_boxed) L130);
     ((CHAR_boxed) L129)->immutable_part = ';';
     plus_s25 = L129;
     L131 = ATTR(plus_self39,ntext);
     L132 = plus_s25;
     SATTR(plus_self39,ntext,FSTR_p1752847026(L131, (*dSTR_strrSTR[TAG(L132)])(L132)));
     comment_self3 = self;
     comment_f2 = ATTR(self,globals_c);
     plus_self40 = ((STR) &Shared);
     mang_self11 = self;
     mang_ob11 = ((OB) ATTR(age,class_tp));
     ret_val57 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, ((OB) NULL));
     plus_sarg12 = ret_val57;
     ret_val58 = STR_ap2004550586(plus_self40, plus_sarg12);
     plus_self41 = ret_val58;
     plus_sarg13 = ((STR) &name34);
     ret_val59 = STR_ap2004550586(plus_self41, plus_sarg13);
     plus_self42 = ret_val59;
     plus_sarg14 = ATTR(age,name1).str;
     ret_val60 = STR_ap2004550586(plus_self42, plus_sarg14);
     comment_com3 = ret_val60;
     if (ATTR(comment_self3,pretty)) {
      plus_self43 = comment_f2;
      plus_s26 = ((dSTR) ((STR) &name78));
      L133 = (plus_s26==((dSTR) NULL));
      L1341_=!(L133); 
      if (L1341_) {
       L135 = ATTR(plus_self43,ntext);
       L136 = plus_s26;
       SATTR(plus_self43,ntext,FSTR_p1752847026(L135, (*dSTR_strrSTR[TAG(L136)])(L136)));
      }
      ret_val61 = plus_self43;
      plus_self44 = ret_val61;
      plus_s27 = ((dSTR) comment_com3);
      L137 = (plus_s27==((dSTR) NULL));
      L1381_=!(L137); 
      if (L1381_) {
       L139 = ATTR(plus_self44,ntext);
       L140 = plus_s27;
       SATTR(plus_self44,ntext,FSTR_p1752847026(L139, (*dSTR_strrSTR[TAG(L140)])(L140)));
      }
      ret_val62 = plus_self44;
      plus_self45 = ret_val62;
      plus_s28 = ((dSTR) ((STR) &name79));
      L141 = ATTR(plus_self45,ntext);
      L142 = plus_s28;
      SATTR(plus_self45,ntext,FSTR_p1752847026(L141, (*dSTR_strrSTR[TAG(L142)])(L142)));
     }
    }
    CGEN_b2123071570(self, age);
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e756677294(CGEN self, AM_ROUT_DEF f) {
 ARRAYSTR arg_list;
 SIG sig1;
 STR rout_index = ((STR) NULL);
 STR name111;
 FLISTA725283029 iter_out_arg_loc = ((FLISTA725283029) NULL);
 BOOL out_args = BOOL_zero;
 AM_ROUT_DEF L11;
 INT L21 = INT_zero;
 AM_FORMAL_ARG arg;
 AM_LOCAL_EXPR lv;
 STR d;
 INT L31 = INT_zero;
 INT i = INT_zero;
 ARRAYARG L41;
 ARG e;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 INT i1 = INT_zero;
 AM_FORMAL_ARG e1;
 AM_LOCAL_EXPR expr11;
 AM_LOCAL_EXPR lv1;
 STR L81;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT i2 = INT_zero;
 AM_FORMAL_ARG e2;
 AM_LOCAL_EXPR expr21;
 AM_LOCAL_EXPR lv2;
 INT L121 = INT_zero;
 INT L131 = INT_zero;
 INT i3 = INT_zero;
 AM_FORMAL_ARG e3;
 AM_LOCAL_EXPR expr31;
 AM_LOCAL_EXPR lv3;
 STR L141;
 INT L151 = INT_zero;
 INT L161 = INT_zero;
 INT i4 = INT_zero;
 AM_FORMAL_ARG e4;
 STR init_str = ((STR) NULL);
 STR local11;
 FLISTSTR ll;
 INT cn = INT_zero;
 AM_ROUT_DEF L171;
 AM_FORMAL_ARG e5;
 AM_LOCAL_EXPR loc;
 FLISTA725283029 L181;
 FLISTA725283029 L191;
 AM_LOCAL_EXPR l;
 AM_ROUT_DEF L201;
 STR k;
 INT L221 = INT_zero;
 FLISTA725283029 L231;
 AM_LOCAL_EXPR l1;
 INT L241 = INT_zero;
 INT L251 = INT_zero;
 INT i5 = INT_zero;
 FLISTSTR L261;
 AM_LOCAL_EXPR l2 = ((AM_LOCAL_EXPR) NULL);
 AM_ROUT_DEF L271;
 AM_FORMAL_ARG e6;
 FLISTA725283029 L281;
 dMODE L291;
 AM_LOCAL_EXPR lv4;
 FLISTA725283029 L301;
 STR def;
 INT L321 = INT_zero;
 INT L331 = INT_zero;
 INT i6 = INT_zero;
 FLISTA725283029 L341;
 AM_LOCAL_EXPR i7;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 STR plus_self2;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg1;
 STR ret_val4;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self4;
 dSTR plus_s2;
 CODE_FILE ret_val5;
 CODE_FILE plus_self5;
 dSTR plus_s3;
 CODE_FILE ret_val6;
 CODE_FILE plus_self6;
 dSTR plus_s4;
 CODE_FILE plus_self7;
 dSTR plus_s5;
 PRINT_OB register_self;
 SIG register_s;
 STR ret_val7;
 STR plus_self8;
 INT plus_arg = INT_zero;
 STR ret_val8;
 STR s;
 INT str_self = INT_zero;
 STR ret_val9;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val10;
 FSTR str_self1;
 STR ret_val11;
 CODE_FILE plus_self9;
 dSTR plus_s6;
 CODE_FILE ret_val12;
 CODE_FILE plus_self10;
 dSTR plus_s7;
 CODE_FILE ret_val13;
 CODE_FILE plus_self11;
 dSTR plus_s8;
 CODE_FILE ret_val14;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val15;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val16;
 CODE_FILE plus_self12;
 dSTR plus_s9;
 CODE_FILE ret_val17;
 CODE_FILE plus_self13;
 dSTR plus_s10;
 CODE_FILE plus_self14;
 dSTR plus_s11;
 CODE_FILE ret_val18;
 CODE_FILE plus_self15;
 dSTR plus_s12;
 CODE_FILE ret_val19;
 CODE_FILE plus_self16;
 dSTR plus_s13;
 CODE_FILE ret_val20;
 SFILE_ID file_in_self1 = SFILE_ID_zero;
 STR ret_val21;
 FLISTSTR aget_self1;
 INT aget_ind1 = INT_zero;
 STR ret_val22;
 CODE_FILE plus_self17;
 dSTR plus_s14;
 CODE_FILE ret_val23;
 CODE_FILE plus_self18;
 dSTR plus_s15;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val24;
 CODE_FILE plus_self19;
 dSTR plus_s16;
 CODE_FILE ret_val25;
 CODE_FILE plus_self20;
 dSTR plus_s17;
 CODE_FILE plus_self21;
 dSTR plus_s18;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val26;
 CODE_FILE plus_self22;
 dSTR plus_s19;
 CODE_FILE ret_val27;
 CODE_FILE plus_self23;
 dSTR plus_s20;
 CODE_FILE ret_val28;
 CODE_FILE plus_self24;
 dSTR plus_s21;
 AM_ROUT_DEF is_iter_self2;
 BOOL ret_val29 = BOOL_zero;
 SIG is_iter_self3;
 BOOL ret_val30 = BOOL_zero;
 IDENT is_iter_self4 = IDENT_zero;
 BOOL ret_val31 = BOOL_zero;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val32;
 CODE_FILE plus_self25;
 dSTR plus_s22;
 CODE_FILE ret_val33;
 CODE_FILE plus_self26;
 dSTR plus_s23;
 CODE_FILE plus_self27;
 dSTR plus_s24;
 INT L351 = INT_zero;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val34;
 AM_LOCAL_EXPR create_self1;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AS_TYPE_SPEC create_as_tp;
 AM_LOCAL_EXPR ret_val35;
 AM_LOCAL_EXPR r;
 CGEN mang_self3;
 OB mang_ob3;
 OB mang_ns;
 STR ret_val36;
 STR plus_self28;
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
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val41;
 CODE_FILE plus_self29;
 dSTR plus_s25;
 CODE_FILE ret_val42;
 CODE_FILE plus_self30;
 dSTR plus_s26;
 INT L361 = INT_zero;
 STR plus_self31;
 INT plus_arg2 = INT_zero;
 STR ret_val43;
 STR s2;
 INT str_self4 = INT_zero;
 STR ret_val44;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val45;
 FSTR str_self5;
 STR ret_val46;
 CODE_FILE plus_self32;
 dSTR plus_s27;
 CODE_FILE ret_val47;
 CGEN arg_type_str_sel;
 ARG arg_type_str_e;
 STR ret_val48;
 STR res;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val49;
 STR plus_self33;
 STR plus_sarg2;
 STR ret_val50;
 CODE_FILE plus_self34;
 dSTR plus_s28;
 CODE_FILE ret_val51;
 CODE_FILE plus_self35;
 dSTR plus_s29;
 CODE_FILE plus_self36;
 dSTR plus_s30;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val52;
 AM_LOCAL_EXPR create_self2;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AS_TYPE_SPEC create_as_tp1;
 AM_LOCAL_EXPR ret_val53;
 AM_LOCAL_EXPR r1;
 CGEN mang_self6;
 OB mang_ob6;
 OB mang_ns1;
 STR ret_val54;
 CGEN mang_self7;
 OB mang_ob7;
 OB mang_ns2;
 STR ret_val55;
 CGEN arg_type_str_sel1;
 AM_FORMAL_ARG arg_type_str_e1;
 STR ret_val56;
 STR res1;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val57;
 CGEN mang_self8;
 OB mang_ob8;
 STR ret_val58;
 STR plus_self37;
 STR plus_sarg3;
 STR ret_val59;
 STR plus_self38;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val60;
 STR s3;
 CHAR str_self6 = CHAR_zero;
 STR ret_val61;
 STR create_self3;
 CHAR create_c = CHAR_zero;
 STR ret_val62;
 STR r2;
 STR plus_self39;
 STR plus_sarg4;
 STR ret_val63;
 CODE_FILE plus_self40;
 dSTR plus_s31;
 SIG is_forked_self;
 BOOL ret_val64 = BOOL_zero;
 CODE_FILE plus_self41;
 dSTR plus_s32;
 CODE_FILE ret_val65;
 CODE_FILE plus_self42;
 dSTR plus_s33;
 AM_LOCAL_EXPR tp_self3;
 dTP ret_val66;
 AM_LOCAL_EXPR create_self4;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name2 = IDENT_zero;
 dTP create_tp2;
 AS_TYPE_SPEC create_as_tp2;
 AM_LOCAL_EXPR ret_val67;
 AM_LOCAL_EXPR r3;
 CGEN mang_self9;
 OB mang_ob9;
 OB mang_ns3;
 STR ret_val68;
 CGEN mang_self10;
 OB mang_ob10;
 OB mang_ns4;
 STR ret_val69;
 CODE_FILE plus_self43;
 dSTR plus_s34;
 CODE_FILE ret_val70;
 CGEN arg_type_str_sel2;
 AM_FORMAL_ARG arg_type_str_e2;
 STR ret_val71;
 STR res2;
 AM_LOCAL_EXPR tp_self4;
 dTP ret_val72;
 CGEN mang_self11;
 OB mang_ob11;
 STR ret_val73;
 STR plus_self44;
 STR plus_sarg5;
 STR ret_val74;
 CODE_FILE plus_self45;
 dSTR plus_s35;
 CODE_FILE ret_val75;
 CODE_FILE plus_self46;
 dSTR plus_s36;
 CODE_FILE ret_val76;
 CODE_FILE plus_self47;
 dSTR plus_s37;
 AM_LOCAL_EXPR tp_self5;
 dTP ret_val77;
 AM_LOCAL_EXPR create_self5;
 SFILE_ID create_src3 = SFILE_ID_zero;
 IDENT create_name3 = IDENT_zero;
 dTP create_tp3;
 AS_TYPE_SPEC create_as_tp3;
 AM_LOCAL_EXPR ret_val78;
 AM_LOCAL_EXPR r4;
 CGEN mang_self12;
 OB mang_ob12;
 OB mang_ns5;
 STR ret_val79;
 CGEN mang_self13;
 OB mang_ob13;
 OB mang_ns6;
 STR ret_val80;
 CGEN arg_type_str_sel3;
 AM_FORMAL_ARG arg_type_str_e3;
 STR ret_val81;
 STR res3;
 AM_LOCAL_EXPR tp_self6;
 dTP ret_val82;
 CGEN mang_self14;
 OB mang_ob14;
 STR ret_val83;
 STR plus_self48;
 STR plus_sarg6;
 STR ret_val84;
 STR plus_self49;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val85;
 STR s4;
 CHAR str_self7 = CHAR_zero;
 STR ret_val86;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val87;
 STR r5;
 STR plus_self50;
 STR plus_sarg7;
 STR ret_val88;
 CODE_FILE plus_self51;
 dSTR plus_s38;
 CODE_FILE plus_self52;
 dSTR plus_s39;
 CODE_FILE plus_self53;
 dSTR plus_s40;
 CGEN in_self;
 AM_ROUT_DEF is_iter_self5;
 BOOL ret_val89 = BOOL_zero;
 SIG is_iter_self6;
 BOOL ret_val90 = BOOL_zero;
 IDENT is_iter_self7 = IDENT_zero;
 BOOL ret_val91 = BOOL_zero;
 STR plus_self54;
 STR plus_sarg8;
 STR ret_val92;
 AM_FORMAL_ARG tp_self7;
 dTP ret_val93;
 AM_LOCAL_EXPR tp_self8;
 dTP ret_val94;
 FLISTSTR create_self7;
 FLISTSTR ret_val95;
 AM_ROUT_DEF is_iter_self8;
 BOOL ret_val96 = BOOL_zero;
 SIG is_iter_self9;
 BOOL ret_val97 = BOOL_zero;
 IDENT is_iter_self10 = IDENT_zero;
 BOOL ret_val98 = BOOL_zero;
 INT L371 = INT_zero;
 CGEN mang_self15;
 OB mang_ob15;
 OB mang_ns7;
 STR ret_val99;
 STR plus_self55;
 STR plus_sarg9;
 STR ret_val100;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val101 = BOOL_zero;
 CGEN mang_self16;
 OB mang_ob16;
 OB mang_ns8;
 STR ret_val102;
 STR plus_self56;
 STR plus_sarg10;
 STR ret_val103;
 INT L381 = INT_zero;
 CGEN mang_self17;
 OB mang_ob17;
 OB mang_ns9;
 STR ret_val104;
 STR plus_self57;
 STR plus_sarg11;
 STR ret_val105;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val106 = BOOL_zero;
 CGEN mang_self18;
 OB mang_ob18;
 OB mang_ns10;
 STR ret_val107;
 STR plus_self58;
 STR plus_sarg12;
 STR ret_val108;
 STR plus_self59;
 INT plus_arg5 = INT_zero;
 STR ret_val109;
 STR s5;
 INT str_self8 = INT_zero;
 STR ret_val110;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val1111;
 FSTR str_self9;
 STR ret_val112;
 STR plus_self60;
 STR plus_sarg13;
 STR ret_val113;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT str_self10 = INT_zero;
 STR ret_val114;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val115;
 FSTR str_self11;
 STR ret_val116;
 CGEN defer_self;
 STR defer_s;
 CGEN defer_self1;
 STR defer_s1;
 SIG is_forked_self1;
 BOOL ret_val117 = BOOL_zero;
 CGEN defer_self2;
 STR defer_s2;
 CGEN defer_self3;
 STR defer_s3;
 CGEN defer_self4;
 STR defer_s4;
 AM_ROUT_DEF is_iter_self11;
 BOOL ret_val118 = BOOL_zero;
 SIG is_iter_self12;
 BOOL ret_val119 = BOOL_zero;
 IDENT is_iter_self13 = IDENT_zero;
 BOOL ret_val120 = BOOL_zero;
 INT L391 = INT_zero;
 CGEN mang_self19;
 OB mang_ob19;
 OB mang_ns11;
 STR ret_val121;
 STR plus_self61;
 STR plus_sarg14;
 STR ret_val122;
 STR plus_self62;
 STR plus_sarg15;
 STR ret_val123;
 STR plus_self63;
 STR plus_sarg16;
 STR ret_val124;
 CGEN mang_self20;
 OB mang_ob20;
 OB mang_ns12;
 STR ret_val125;
 STR plus_self64;
 STR plus_sarg17;
 STR ret_val126;
 STR plus_self65;
 STR plus_sarg18;
 STR ret_val127;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN mang_self21;
 OB mang_ob21;
 OB mang_ns13;
 STR ret_val128;
 STR plus_self66;
 STR plus_sarg19;
 STR ret_val129;
 STR plus_self67;
 STR plus_sarg20;
 STR ret_val130;
 AM_LOCAL_EXPR tp_self9;
 dTP ret_val131;
 STR plus_self68;
 STR plus_sarg21;
 STR ret_val132;
 STR plus_self69;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val133;
 STR s6;
 CHAR str_self12 = CHAR_zero;
 STR ret_val134;
 STR create_self8;
 CHAR create_c2 = CHAR_zero;
 STR ret_val135;
 STR r6;
 CGEN ndefer_self3;
 STR ndefer_s3;
 AM_ROUT_DEF is_iter_self14;
 BOOL ret_val136 = BOOL_zero;
 SIG is_iter_self15;
 BOOL ret_val137 = BOOL_zero;
 IDENT is_iter_self16 = IDENT_zero;
 BOOL ret_val138 = BOOL_zero;
 AM_LOCAL_EXPR tp_self10;
 dTP ret_val139;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CODE_FILE plus_self70;
 dSTR plus_s41;
 AM_LOCAL_EXPR tp_self11;
 dTP ret_val140;
 AM_LOCAL_EXPR tp_self12;
 dTP ret_val141;
 AM_LOCAL_EXPR tp_self13;
 dTP ret_val142;
 CGEN mang_self22;
 OB mang_ob22;
 STR ret_val143;
 CODE_FILE plus_self71;
 dSTR plus_s42;
 CODE_FILE ret_val144;
 CODE_FILE plus_self72;
 dSTR plus_s43;
 CODE_FILE ret_val145;
 CGEN mang_self23;
 OB mang_ob23;
 OB mang_ns14;
 STR ret_val146;
 CODE_FILE plus_self73;
 dSTR plus_s44;
 CODE_FILE ret_val147;
 CODE_FILE plus_self74;
 dSTR plus_s45;
 CODE_FILE ret_val148;
 CODE_FILE plus_self75;
 dSTR plus_s46;
 CODE_FILE ret_val149;
 CODE_FILE plus_self76;
 dSTR plus_s47;
 AM_LOCAL_EXPR tp_self14;
 dTP ret_val150;
 CGEN mang_self24;
 OB mang_ob24;
 STR ret_val151;
 CODE_FILE plus_self77;
 dSTR plus_s48;
 CODE_FILE ret_val152;
 CODE_FILE plus_self78;
 dSTR plus_s49;
 CODE_FILE ret_val153;
 CGEN mang_self25;
 OB mang_ob25;
 OB mang_ns15;
 STR ret_val154;
 CODE_FILE plus_self79;
 dSTR plus_s50;
 CODE_FILE ret_val155;
 CODE_FILE plus_self80;
 dSTR plus_s51;
 SIG is_forked_self2;
 BOOL ret_val156 = BOOL_zero;
 AM_LOCAL_EXPR tp_self15;
 dTP ret_val157;
 CGEN mang_self26;
 OB mang_ob26;
 STR ret_val158;
 CODE_FILE plus_self81;
 dSTR plus_s52;
 CODE_FILE ret_val159;
 CODE_FILE plus_self82;
 dSTR plus_s53;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val160;
 STR plus_self83;
 STR plus_sarg22;
 STR ret_val161;
 STR plus_self84;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val162;
 STR s7;
 CHAR str_self13 = CHAR_zero;
 STR ret_val163;
 STR create_self9;
 CHAR create_c3 = CHAR_zero;
 STR ret_val164;
 STR r7;
 CGEN ndefer_self4;
 STR ndefer_s4;
 AM_ROUT_DEF is_iter_self17;
 BOOL ret_val165 = BOOL_zero;
 SIG is_iter_self18;
 BOOL ret_val166 = BOOL_zero;
 IDENT is_iter_self19 = IDENT_zero;
 BOOL ret_val167 = BOOL_zero;
 CGEN mang_self27;
 OB mang_ob27;
 STR ret_val168;
 CODE_FILE plus_self85;
 dSTR plus_s54;
 CODE_FILE ret_val169;
 CODE_FILE plus_self86;
 dSTR plus_s55;
 CODE_FILE ret_val170;
 CODE_FILE plus_self87;
 dSTR plus_s56;
 CODE_FILE ret_val171;
 CODE_FILE plus_self88;
 dSTR plus_s57;
 CODE_FILE ret_val172;
 CODE_FILE plus_self89;
 dSTR plus_s58;
 PROG psather_self;
 BOOL ret_val173 = BOOL_zero;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN in_self1;
 STR plus_self90;
 INT plus_arg8 = INT_zero;
 STR ret_val174;
 STR s8;
 INT str_self14 = INT_zero;
 STR ret_val175;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val176;
 FSTR str_self15;
 STR ret_val177;
 STR plus_self91;
 STR plus_sarg23;
 STR ret_val178;
 STR plus_self92;
 INT plus_arg9 = INT_zero;
 STR ret_val179;
 STR s9;
 INT str_self16 = INT_zero;
 STR ret_val180;
 FSTR buf7 = ((FSTR) NULL);
 FSTR clear_self6;
 INT str_in_self6 = INT_zero;
 FSTR str_in_s6;
 FSTR ret_val181;
 FSTR str_self17;
 STR ret_val182;
 STR plus_self93;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val183;
 STR s10;
 CHAR str_self18 = CHAR_zero;
 STR ret_val184;
 STR create_self10;
 CHAR create_c4 = CHAR_zero;
 STR ret_val185;
 STR r8;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN move_out_self;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN mang_self28;
 OB mang_ob28;
 OB mang_ns16;
 STR ret_val186;
 STR plus_self94;
 STR plus_sarg24;
 STR ret_val187;
 STR plus_self95;
 STR plus_sarg25;
 STR ret_val188;
 AM_LOCAL_EXPR tp_self16;
 dTP ret_val189;
 STR plus_self96;
 STR plus_sarg26;
 STR ret_val190;
 STR plus_self97;
 STR plus_sarg27;
 STR ret_val191;
 CGEN ndefer_self10;
 STR ndefer_s10;
 INT L401_;
 ARRAYSTR L42;
 INT L43;
 OB L44;
 INT L45;
 AM_FORMAL_ARG L461_;
 AM_FORMAL_ARG L47;
 extern STR self1;
 BOOL L48;
 BOOL L49;
 BOOL L501_;
 STR L52;
 INT L53;
 INT L541_;
 INT L55;
 CHAR L561_;
 CHAR L57;
 BOOL L581_;
 extern STR undefI419930847;
 FSTR L59;
 dSTR L60;
 extern STR undefI419930828;
 FSTR L62;
 dSTR L63;
 extern STR Definitionof;
 extern STR name78;
 BOOL L64;
 BOOL L651_;
 FSTR L66;
 dSTR L67;
 BOOL L68;
 BOOL L691_;
 FSTR L70;
 dSTR L72;
 extern STR name79;
 FSTR L73;
 dSTR L74;
 dSTR L75;
 OB L76;
 FSTR L77;
 dSTR L78;
 extern STR F_tag_;
 INT L79;
 INT L801_;
 BOOL L82;
 BOOL L831_;
 extern STR line2;
 BOOL L84;
 BOOL L851_;
 FSTR L86;
 dSTR L87;
 dSTR L88;
 OB L89;
 BOOL L90;
 BOOL L921_;
 FSTR L93;
 dSTR L94;
 extern STR name71;
 BOOL L95;
 BOOL L961_;
 FSTR L97;
 dSTR L98;
 STR L991_;
 BOOL L100;
 BOOL L1021_;
 FSTR L103;
 dSTR L104;
 extern STR name73;
 FSTR L105;
 dSTR L106;
 extern STR line1;
 BOOL L107;
 BOOL L1081_;
 FSTR L109;
 dSTR L110;
 dSTR L111;
 OB L112;
 BOOL L113;
 BOOL L1141_;
 FSTR L115;
 dSTR L116;
 extern STR name71;
 BOOL L117;
 BOOL L1181_;
 FSTR L119;
 dSTR L120;
 STR L1221_;
 BOOL L123;
 BOOL L1241_;
 FSTR L125;
 dSTR L126;
 extern STR name83;
 FSTR L127;
 dSTR L128;
 BOOL L129;
 BOOL L1301_;
 BOOL L132;
 BOOL L1331_;
 FSTR L134;
 dSTR L135;
 dSTR L136;
 OB L137;
 FSTR L138;
 dSTR L139;
 extern STR void6;
 FSTR L140;
 dSTR L142;
 dTP L143;
 STR L144;
 extern STR S_;
 BOOL L145;
 BOOL L1461_;
 FSTR L147;
 dSTR L148;
 BOOL L149;
 BOOL L1501_;
 FSTR L152;
 dSTR L153;
 dSTR L154;
 OB L155;
 FSTR L156;
 dSTR L157;
 BOOL L158;
 BOOL L159;
 BOOL L1601_;
 STR L162;
 INT L163;
 INT L1641_;
 INT L165;
 CHAR L1661_;
 CHAR L167;
 BOOL L1681_;
 BOOL L169;
 BOOL L1701_;
 FSTR L172;
 dSTR L173;
 extern STR S_frameframe;
 FSTR L174;
 dSTR L175;
 extern STR struct167654362;
 FSTR L176;
 dSTR L177;
 INT L1781_;
 INT L179;
 BOOL L1801_;
 INT L1821_c,L1821_s;
 AM_FORMAL_ARG aL1821_;
 INT L1831_;
 AM_FORMAL_ARG L184;
 BOOL L185;
 dMODE L186;
 dMODE L187;
 AM_LOCAL_EXPR L188;
 OB L189;
 extern STR arg21;
 INT L190;
 BOOL L192;
 BOOL L1931_;
 INT L1941_;
 BOOL L195;
 BOOL L1961_;
 FSTR L197;
 dSTR L198;
 extern STR self8;
 FSTR L199;
 dSTR L200;
 BOOL L202;
 BOOL L2031_;
 INT L2041_br;
 ARG aL2041_;
 INT L2051_;
 ARG L206;
 extern STR arg4;
 INT L207;
 BOOL L208;
 BOOL L2091_;
 STR L210;
 extern STR name29;
 BOOL L212;
 BOOL L2131_;
 FSTR L214;
 dSTR L215;
 BOOL L216;
 dMODE L217;
 dMODE L218;
 extern STR name58;
 BOOL L219;
 BOOL L2201_;
 FSTR L222;
 dSTR L223;
 STR L2241_;
 FSTR L225;
 dSTR L226;
 INT L2271_;
 extern STR struct167654362;
 FSTR L228;
 dSTR L229;
 extern STR name29;
 INT L232;
 INT L2331_;
 INT L234;
 INT L2351_;
 INT L2301_,L2301_m;
 AM_FORMAL_ARG L2361_;
 BOOL L237;
 dMODE L238;
 dMODE L239;
 AM_LOCAL_EXPR L240;
 OB L242;
 STR L243;
 STR L245;
 STR L247;
 BOOL L248;
 dMODE L249;
 dMODE L250;
 extern STR name58;
 STR L252;
 INT L253;
 OB L254;
 INT L255;
 STR L2571_;
 FSTR L258;
 dSTR L259;
 BOOL L260;
 BOOL L262;
 INT L264;
 INT L2651_;
 INT L266;
 INT L2671_;
 INT L2631_,L2631_m;
 AM_FORMAL_ARG L2681_;
 BOOL L2691_;
 extern STR self_to_be;
 extern STR OB3;
 BOOL L272;
 BOOL L2731_;
 FSTR L274;
 dSTR L275;
 STR L2761_;
 FSTR L277;
 dSTR L278;
 BOOL L279;
 dMODE L280;
 dMODE L282;
 AM_LOCAL_EXPR L283;
 OB L284;
 STR L285;
 STR L287;
 extern STR name29;
 BOOL L289;
 BOOL L2901_;
 FSTR L292;
 dSTR L293;
 BOOL L294;
 dMODE L295;
 dMODE L296;
 extern STR name58;
 BOOL L297;
 BOOL L2981_;
 FSTR L299;
 dSTR L300;
 dSTR L302;
 OB L303;
 BOOL L304;
 BOOL L3051_;
 FSTR L306;
 dSTR L307;
 STR L3081_;
 FSTR L309;
 dSTR L310;
 extern STR name29;
 INT L312;
 INT L3131_;
 INT L314;
 INT L3151_;
 INT L3111_,L3111_m;
 AM_FORMAL_ARG L3161_;
 BOOL L317;
 dMODE L318;
 dMODE L319;
 AM_LOCAL_EXPR L320;
 OB L322;
 STR L323;
 STR L325;
 STR L327;
 BOOL L328;
 dMODE L329;
 dMODE L330;
 extern STR name58;
 STR L332;
 INT L333;
 OB L334;
 INT L335;
 STR L3371_;
 FSTR L338;
 dSTR L339;
 extern STR struct167654362;
 FSTR L340;
 dSTR L342;
 extern STR name84;
 FSTR L343;
 dSTR L344;
 INT L345;
 INT L3461_;
 BOOL L347;
 BOOL L348;
 BOOL L349;
 BOOL L3501_;
 STR L352;
 INT L353;
 INT L3541_;
 INT L355;
 CHAR L3561_;
 CHAR L357;
 BOOL L3581_;
 BOOL L359;
 BOOL L3601_;
 INT L363;
 INT L3641_;
 INT L365;
 INT L3661_;
 INT L3621_,L3621_m;
 AM_FORMAL_ARG L3671_;
 BOOL L368;
 dMODE L369;
 dMODE L370;
 dMODE L372;
 extern STR name58;
 STR L3731_;
 BOOL L374;
 BOOL L375;
 BOOL L3761_;
 STR L377;
 INT L378;
 INT L3791_;
 INT L380;
 CHAR L3821_;
 CHAR L383;
 BOOL L3841_;
 INT L3851_br;
 AM_FORMAL_ARG aL3851_;
 INT L3861_;
 AM_FORMAL_ARG L387;
 BOOL L388;
 dMODE L389;
 dMODE L390;
 AM_LOCAL_EXPR L392;
 extern STR voidframe;
 INT L3931_;
 AM_LOCAL_EXPR L394;
 BOOL L395;
 BOOL L396;
 BOOL L3971_;
 BOOL L398;
 BOOL L3991_;
 extern STR voidframe;
 INT L4001_;
 INT L4021_br;
 AM_FORMAL_ARG aL4021_;
 INT L4031_;
 extern STR void7;
 AM_FORMAL_ARG L404;
 AM_FORMAL_ARG L405;
 BOOL L406;
 INT L407;
 INT L408;
 BOOL L4091_;
 extern STR NULL2;
 INT L4101_;
 INT L4111_;
 AM_LOCAL_EXPR L412;
 BOOL L413;
 BOOL L414;
 BOOL L4151_;
 BOOL L416;
 BOOL L4171_;
 extern STR void7;
 INT L4181_;
 extern STR void_local_frame;
 BOOL L419;
 BOOL L4201_;
 extern STR name86;
 extern STR struct1270974273;
 INT L4221_;
 INT L4211_,L4211_m;
 INT L423;
 extern STR S_local_frame;
 BOOL L424;
 BOOL L4251_;
 STR L426;
 extern STR name15;
 STR L427;
 STR L428;
 extern STR name16;
 extern STR FFfunc;
 extern STR name16;
 BOOL L429;
 BOOL L430;
 BOOL L431;
 extern STR FFprevNULL;
 extern STR FFprev530765537;
 extern STR FFargs931318564;
 BOOL L432;
 BOOL L433;
 BOOL L434;
 BOOL L4351_;
 STR L436;
 INT L437;
 INT L4381_;
 INT L439;
 CHAR L4401_;
 CHAR L441;
 BOOL L4421_;
 INT L4431_br;
 AM_FORMAL_ARG aL4431_;
 AM_FORMAL_ARG L444;
 BOOL L445;
 dMODE L446;
 dMODE L447;
 AM_LOCAL_EXPR L448;
 dMODE L449;
 extern STR frame1;
 extern STR name149;
 extern STR frame1;
 extern STR name76;
 dMODE L450;
 extern STR frame1;
 extern STR name36;
 STR L451;
 INT L452;
 OB L453;
 INT L454;
 INT L4561_;
 BOOL L457;
 BOOL L458;
 BOOL L459;
 BOOL L4601_;
 STR L462;
 INT L463;
 INT L4641_;
 INT L465;
 CHAR L4661_;
 CHAR L467;
 BOOL L4681_;
 BOOL L469;
 BOOL L4701_;
 BOOL L471;
 BOOL L4721_;
 AM_LOCAL_EXPR L473;
 extern STR volatile3;
 FSTR L474;
 dSTR L475;
 BOOL L476;
 dTP L477;
 BOOL L478;
 BOOL L4791_;
 FSTR L480;
 dSTR L481;
 dSTR L482;
 OB L483;
 BOOL L484;
 BOOL L4851_;
 FSTR L486;
 dSTR L487;
 BOOL L488;
 BOOL L4891_;
 FSTR L490;
 dSTR L491;
 extern STR name36;
 BOOL L492;
 BOOL L4931_;
 FSTR L494;
 dSTR L495;
 BOOL L496;
 BOOL L4971_;
 FSTR L498;
 dSTR L499;
 dSTR L500;
 OB L502;
 FSTR L503;
 dSTR L504;
 BOOL L505;
 BOOL L5061_;
 FSTR L507;
 dSTR L508;
 dSTR L509;
 OB L510;
 BOOL L511;
 BOOL L5121_;
 FSTR L513;
 dSTR L514;
 BOOL L515;
 BOOL L5161_;
 FSTR L517;
 dSTR L518;
 dSTR L519;
 OB L520;
 FSTR L521;
 dSTR L522;
 BOOL L523;
 BOOL L524;
 AM_FORMAL_ARG L5251_;
 AM_FORMAL_ARG L526;
 BOOL L527;
 BOOL L5281_;
 FSTR L529;
 dSTR L530;
 extern STR self4;
 FSTR L531;
 dSTR L532;
 extern STR self5;
 extern STR self_to_be;
 STR L533;
 INT L534;
 OB L535;
 INT L536;
 BOOL L538;
 BOOL L539;
 BOOL L5401_;
 STR L542;
 INT L543;
 INT L5441_;
 INT L545;
 CHAR L5461_;
 CHAR L547;
 BOOL L5481_;
 BOOL L549;
 BOOL L5501_;
 BOOL L551;
 BOOL L5521_;
 FSTR L553;
 dSTR L554;
 dSTR L555;
 OB L556;
 BOOL L557;
 BOOL L5581_;
 FSTR L559;
 dSTR L560;
 extern STR dummy11;
 BOOL L562;
 BOOL L5631_;
 FSTR L564;
 dSTR L565;
 BOOL L566;
 BOOL L5671_;
 FSTR L568;
 dSTR L569;
 dSTR L570;
 OB L571;
 FSTR L572;
 dSTR L573;
 BOOL L574;
 BOOL L575;
 BOOL L576;
 BOOL L577;
 BOOL L578;
 BOOL L5791_;
 BOOL L580;
 BOOL L5821_;
 SE_CONTEXT L583;
 extern STR iffram1733377389;
 extern STR switchframestate;
 INT L584;
 INT L5851_;
 INT L5861_,L5861_m;
 INT L587;
 extern STR case3;
 BOOL L588;
 BOOL L5891_;
 extern STR gotostate;
 BOOL L590;
 BOOL L5911_;
 STR L592;
 INT L593;
 OB L594;
 INT L595;
 extern STR name4;
 INT L597;
 INT L5981_;
 extern STR state02;
 BOOL L599;
 BOOL L6001_;
 AM_LOCAL_EXPR L601;
 extern STR frame1;
 extern STR name36;
 extern STR name16;
 create_self = ((ARRAYSTR) NULL);
 L401_=ASIZE((AM_ROUT_DEF)f); 
 create_n = L401_;
 L43 = create_n;
 L45=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L43)*sizeof(STR);
 L44=ZALLOC_BIG(L45);
 if (L44==NULL) FATAL("Unable to allocate more memory");
 ((OB)L44)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L44)->header.destroyed=0;
#endif

 L42 = ((ARRAYSTR) L44);
 L42->asize = L43;
 ret_val = L42;
 arg_list = ret_val;
 SATTR(self,saw_outer_return,FALSE);
 sig1 = ATTR(f,sig1);
 L461_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,0); 
 L47=L461_;
 MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(L47,expr)), ((STR) &self1), ((OB) sig1));
 CGEN_newline(self);
 CGEN_newline(self);
 is_iter_self = ATTR(f,sig1);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L49 = (is_iter_self1.str==((STR) NULL));
 L501_=!(L49); 
 if (L501_) {
  L52 = is_iter_self1.str;
  L53 = STR_sizerINT(is_iter_self1.str);
  L541_=INTMINUS(L53,1); 
  L55 = L541_;
  L561_=ARR((STR)L52,L55); 
  L57 = L561_;
  L581_=L57=='!'; 
  L48 = L581_;
 } else {
  L48 = FALSE;
 }
 ret_val2 = L48;
 ret_val1 = ret_val2;
 if (ret_val1) {
  plus_self = ATTR(self,code_c);
  plus_s = ((dSTR) ((STR) &undefI419930847));
  L59 = ATTR(plus_self,ntext);
  L60 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
 }
 else {
  plus_self1 = ATTR(self,code_c);
  plus_s1 = ((dSTR) ((STR) &undefI419930828));
  L62 = ATTR(plus_self1,ntext);
  L63 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L62, (*dSTR_strrSTR[TAG(L63)])(L63)));
 }
 comment_self = self;
 comment_f = ATTR(self,code_c);
 plus_self2 = ((STR) &Definitionof);
 plus_sarg = ATTR(sig1,str);
 ret_val3 = STR_ap2004550586(plus_self2, plus_sarg);
 plus_self3 = ret_val3;
 plus_sarg1 = ATTR(self,func_name_commen);
 ret_val4 = STR_ap2004550586(plus_self3, plus_sarg1);
 comment_com = ret_val4;
 if (ATTR(comment_self,pretty)) {
  plus_self4 = comment_f;
  plus_s2 = ((dSTR) ((STR) &name78));
  L64 = (plus_s2==((dSTR) NULL));
  L651_=!(L64); 
  if (L651_) {
   L66 = ATTR(plus_self4,ntext);
   L67 = plus_s2;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
  }
  ret_val5 = plus_self4;
  plus_self5 = ret_val5;
  plus_s3 = ((dSTR) comment_com);
  L68 = (plus_s3==((dSTR) NULL));
  L691_=!(L68); 
  if (L691_) {
   L70 = ATTR(plus_self5,ntext);
   L72 = plus_s3;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L70, (*dSTR_strrSTR[TAG(L72)])(L72)));
  }
  ret_val6 = plus_self5;
  plus_self6 = ret_val6;
  plus_s4 = ((dSTR) ((STR) &name79));
  L73 = ATTR(plus_self6,ntext);
  L74 = plus_s4;
  SATTR(plus_self6,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
 }
 plus_self7 = ATTR(self,code_c);
 L76=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L76==NULL) FATAL("Unable to allocate more memory");
 memset(L76,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L76)->header.tag=CHAR_tag;
 L75 = (dSTR)((CHAR_boxed) L76);
 ((CHAR_boxed) L75)->immutable_part = '\n';
 plus_s5 = L75;
 L77 = ATTR(plus_self7,ntext);
 L78 = plus_s5;
 SATTR(plus_self7,ntext,FSTR_p1752847026(L77, (*dSTR_strrSTR[TAG(L78)])(L78)));
 if (ATTR(self,func_tables)) {
  register_self = ((PRINT_OB) NULL);
  register_s = ATTR(f,sig1);
  PRINT_OB_funcs = FLISTS69519306(PRINT_OB_funcs, register_s);
  plus_self8 = ((STR) &F_tag_);
  L79 = FLISTS228922656(PRINT_OB_funcs);
  L801_=INTMINUS(L79,1); 
  plus_arg = L801_;
  str_self = plus_arg;
  clear_self = buf1;
  L82 = (clear_self==((FSTR) NULL));
  L831_=!(L82); 
  if (L831_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val10 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val10;
  str_self1 = buf1;
  ret_val11 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val9 = ret_val11;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self8, s, TRUE);
  ret_val7 = ret_val8;
  rout_index = ret_val7;
 }
 if (ATTR(self,debug1)) {
  plus_self9 = ATTR(self,code_c);
  plus_s6 = ((dSTR) ((STR) &line2));
  L84 = (plus_s6==((dSTR) NULL));
  L851_=!(L84); 
  if (L851_) {
   L86 = ATTR(plus_self9,ntext);
   L87 = plus_s6;
   SATTR(plus_self9,ntext,FSTR_p1752847026(L86, (*dSTR_strrSTR[TAG(L87)])(L87)));
  }
  ret_val12 = plus_self9;
  plus_self10 = ret_val12;
  L89=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L89==NULL) FATAL("Unable to allocate more memory");
  memset(L89,0,sizeof(struct INT_boxed_struct));
  ((OB)L89)->header.tag=INT_tag;
  L88 = (dSTR)((INT_boxed) L89);
  ((INT_boxed) L88)->immutable_part = SFILE_2070755978(ATTR(f,source1));
  plus_s7 = L88;
  L90 = (plus_s7==((dSTR) NULL));
  L921_=!(L90); 
  if (L921_) {
   L93 = ATTR(plus_self10,ntext);
   L94 = plus_s7;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L93, (*dSTR_strrSTR[TAG(L94)])(L94)));
  }
  ret_val13 = plus_self10;
  plus_self11 = ret_val13;
  plus_s8 = ((dSTR) ((STR) &name71));
  L95 = (plus_s8==((dSTR) NULL));
  L961_=!(L95); 
  if (L961_) {
   L97 = ATTR(plus_self11,ntext);
   L98 = plus_s8;
   SATTR(plus_self11,ntext,FSTR_p1752847026(L97, (*dSTR_strrSTR[TAG(L98)])(L98)));
  }
  ret_val14 = plus_self11;
  plus_self12 = ret_val14;
  file_in_self = ATTR(f,source1);
  aget_self = SFILE_ID_files;
  aget_ind = SFILE_1102877860(file_in_self);
  L991_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
  ret_val16 = L991_;
  ret_val15 = ret_val16;
  plus_s9 = ((dSTR) ret_val15);
  L100 = (plus_s9==((dSTR) NULL));
  L1021_=!(L100); 
  if (L1021_) {
   L103 = ATTR(plus_self12,ntext);
   L104 = plus_s9;
   SATTR(plus_self12,ntext,FSTR_p1752847026(L103, (*dSTR_strrSTR[TAG(L104)])(L104)));
  }
  ret_val17 = plus_self12;
  plus_self13 = ret_val17;
  plus_s10 = ((dSTR) ((STR) &name73));
  L105 = ATTR(plus_self13,ntext);
  L106 = plus_s10;
  SATTR(plus_self13,ntext,FSTR_p1752847026(L105, (*dSTR_strrSTR[TAG(L106)])(L106)));
  SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
 }
 else {
  if (ATTR(self,pretty)) {
   plus_self14 = ATTR(self,code_c);
   plus_s11 = ((dSTR) ((STR) &line1));
   L107 = (plus_s11==((dSTR) NULL));
   L1081_=!(L107); 
   if (L1081_) {
    L109 = ATTR(plus_self14,ntext);
    L110 = plus_s11;
    SATTR(plus_self14,ntext,FSTR_p1752847026(L109, (*dSTR_strrSTR[TAG(L110)])(L110)));
   }
   ret_val18 = plus_self14;
   plus_self15 = ret_val18;
   L112=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L112==NULL) FATAL("Unable to allocate more memory");
   memset(L112,0,sizeof(struct INT_boxed_struct));
   ((OB)L112)->header.tag=INT_tag;
   L111 = (dSTR)((INT_boxed) L112);
   ((INT_boxed) L111)->immutable_part = SFILE_2070755978(ATTR(f,source1));
   plus_s12 = L111;
   L113 = (plus_s12==((dSTR) NULL));
   L1141_=!(L113); 
   if (L1141_) {
    L115 = ATTR(plus_self15,ntext);
    L116 = plus_s12;
    SATTR(plus_self15,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
   }
   ret_val19 = plus_self15;
   plus_self16 = ret_val19;
   plus_s13 = ((dSTR) ((STR) &name71));
   L117 = (plus_s13==((dSTR) NULL));
   L1181_=!(L117); 
   if (L1181_) {
    L119 = ATTR(plus_self16,ntext);
    L120 = plus_s13;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L119, (*dSTR_strrSTR[TAG(L120)])(L120)));
   }
   ret_val20 = plus_self16;
   plus_self17 = ret_val20;
   file_in_self1 = ATTR(f,source1);
   aget_self1 = SFILE_ID_files;
   aget_ind1 = SFILE_1102877860(file_in_self1);
   L1221_=(STR)ARR((FLISTSTR)aget_self1,aget_ind1); 
   ret_val22 = L1221_;
   ret_val21 = ret_val22;
   plus_s14 = ((dSTR) ret_val21);
   L123 = (plus_s14==((dSTR) NULL));
   L1241_=!(L123); 
   if (L1241_) {
    L125 = ATTR(plus_self17,ntext);
    L126 = plus_s14;
    SATTR(plus_self17,ntext,FSTR_p1752847026(L125, (*dSTR_strrSTR[TAG(L126)])(L126)));
   }
   ret_val23 = plus_self17;
   plus_self18 = ret_val23;
   plus_s15 = ((dSTR) ((STR) &name83));
   L127 = ATTR(plus_self18,ntext);
   L128 = plus_s15;
   SATTR(plus_self18,ntext,FSTR_p1752847026(L127, (*dSTR_strrSTR[TAG(L128)])(L128)));
   SATTR(self,last_lineno,SFILE_2070755978(ATTR(f,source1)));
  }
 }
 L129 = (ATTR(sig1,ret)==((dTP) NULL));
 L1301_=!(L129); 
 if (L1301_) {
  plus_self19 = ATTR(self,code_c);
  mang_self = self;
  mang_ob = ((OB) ATTR(sig1,ret));
  ret_val24 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_s16 = ((dSTR) ret_val24);
  L132 = (plus_s16==((dSTR) NULL));
  L1331_=!(L132); 
  if (L1331_) {
   L134 = ATTR(plus_self19,ntext);
   L135 = plus_s16;
   SATTR(plus_self19,ntext,FSTR_p1752847026(L134, (*dSTR_strrSTR[TAG(L135)])(L135)));
  }
  ret_val25 = plus_self19;
  plus_self20 = ret_val25;
  L137=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L137==NULL) FATAL("Unable to allocate more memory");
  memset(L137,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L137)->header.tag=CHAR_tag;
  L136 = (dSTR)((CHAR_boxed) L137);
  ((CHAR_boxed) L136)->immutable_part = ' ';
  plus_s17 = L136;
  L138 = ATTR(plus_self20,ntext);
  L139 = plus_s17;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L138, (*dSTR_strrSTR[TAG(L139)])(L139)));
 }
 else {
  plus_self21 = ATTR(self,code_c);
  plus_s18 = ((dSTR) ((STR) &void6));
  L140 = ATTR(plus_self21,ntext);
  L142 = plus_s18;
  SATTR(plus_self21,ntext,FSTR_p1752847026(L140, (*dSTR_strrSTR[TAG(L142)])(L142)));
 }
 if (ATTR(f,is_external)) {
  L143 = ATTR(sig1,tp);
  L144 = (*dTP_strrSTR[TAG(L143)])(L143);
  name111 = STR_ap1693864410(L144, ((STR) &S_), ATTR(sig1,name1).str);
  MANGLE1827205184(ATTR(self,mangler), ((OB) sig1), name111, ((OB) NULL));
 }
 plus_self22 = ATTR(self,code_c);
 mang_self1 = self;
 mang_ob1 = ((OB) sig1);
 ret_val26 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_s19 = ((dSTR) ret_val26);
 L145 = (plus_s19==((dSTR) NULL));
 L1461_=!(L145); 
 if (L1461_) {
  L147 = ATTR(plus_self22,ntext);
  L148 = plus_s19;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L147, (*dSTR_strrSTR[TAG(L148)])(L148)));
 }
 ret_val27 = plus_self22;
 plus_self23 = ret_val27;
 plus_s20 = ((dSTR) ATTR(self,func_name_suffix));
 L149 = (plus_s20==((dSTR) NULL));
 L1501_=!(L149); 
 if (L1501_) {
  L152 = ATTR(plus_self23,ntext);
  L153 = plus_s20;
  SATTR(plus_self23,ntext,FSTR_p1752847026(L152, (*dSTR_strrSTR[TAG(L153)])(L153)));
 }
 ret_val28 = plus_self23;
 plus_self24 = ret_val28;
 L155=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L155==NULL) FATAL("Unable to allocate more memory");
 memset(L155,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L155)->header.tag=CHAR_tag;
 L154 = (dSTR)((CHAR_boxed) L155);
 ((CHAR_boxed) L154)->immutable_part = '(';
 plus_s21 = L154;
 L156 = ATTR(plus_self24,ntext);
 L157 = plus_s21;
 SATTR(plus_self24,ntext,FSTR_p1752847026(L156, (*dSTR_strrSTR[TAG(L157)])(L157)));
 CODE_F1291349628(ATTR(self,code_c), sig1);
 out_args = FALSE;
 is_iter_self2 = f;
 is_iter_self3 = ATTR(is_iter_self2,sig1);
 is_iter_self4 = ATTR(is_iter_self3,name1);
 L159 = (is_iter_self4.str==((STR) NULL));
 L1601_=!(L159); 
 if (L1601_) {
  L162 = is_iter_self4.str;
  L163 = STR_sizerINT(is_iter_self4.str);
  L1641_=INTMINUS(L163,1); 
  L165 = L1641_;
  L1661_=ARR((STR)L162,L165); 
  L167 = L1661_;
  L1681_=L167=='!'; 
  L158 = L1681_;
 } else {
  L158 = FALSE;
 }
 ret_val31 = L158;
 ret_val30 = ret_val31;
 ret_val29 = ret_val30;
 if (ret_val29) {
  plus_self25 = ATTR(self,code_c);
  mang_self2 = self;
  mang_ob2 = ((OB) ATTR(f,sig1));
  ret_val32 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  plus_s22 = ((dSTR) ret_val32);
  L169 = (plus_s22==((dSTR) NULL));
  L1701_=!(L169); 
  if (L1701_) {
   L172 = ATTR(plus_self25,ntext);
   L173 = plus_s22;
   SATTR(plus_self25,ntext,FSTR_p1752847026(L172, (*dSTR_strrSTR[TAG(L173)])(L173)));
  }
  ret_val33 = plus_self25;
  plus_self26 = ret_val33;
  plus_s23 = ((dSTR) ((STR) &S_frameframe));
  L174 = ATTR(plus_self26,ntext);
  L175 = plus_s23;
  SATTR(plus_self26,ntext,FSTR_p1752847026(L174, (*dSTR_strrSTR[TAG(L175)])(L175)));
  if (ATTR(self,func_tables)) {
   plus_self27 = ATTR(self,code_c);
   plus_s24 = ((dSTR) ((STR) &struct167654362));
   L176 = ATTR(plus_self27,ntext);
   L177 = plus_s24;
   SATTR(plus_self27,ntext,FSTR_p1752847026(L176, (*dSTR_strrSTR[TAG(L177)])(L177)));
  }
  L1781_=(f)==NULL?0:ASIZE((AM_ROUT_DEF)f); 
  L179 = L1781_;
  L1801_=(1)<(L179); 
  if (L1801_) {
   {
    BOOL f_L1821_ = TRUE;
    BOOL f_L1831_ = TRUE;
    /* loop index variable */
    L351 = 0;
    L11 = f;
    L21 = 1;
    L31 = 1;
    L1821_c=L21-1;L1821_s=L11==0?0:ASIZE((AM_ROUT_DEF)L11); 
    L1831_=L31-1; 
    while (1) {
     if(++L1821_c>=L1821_s)  goto after_loop; aL1821_=ARR((AM_ROUT_DEF)L11,L1821_c); 
     arg = aL1821_;
     lv = ATTR(arg,expr);
     L186 = ATTR(arg,mode1);
     if ((*dMODE_814247358[TAG(L186)])(L186, ((dMODE) MODES_out_mode))) {
      L185 = TRUE;
     } else {
      L187 = ATTR(arg,mode1);
      L185 = (*dMODE_814247358[TAG(L187)])(L187, ((dMODE) MODES_inout_mode));
     }
     if (L185) {
      iter_out_arg_loc = FLISTA1817671564(iter_out_arg_loc, lv);
      create_self1 = ((AM_LOCAL_EXPR) NULL);
      create_src = ATTR(lv,source1);
      create_name = ATTR(lv,name1);
      tp_self = lv;
      ret_val34 = ATTR(tp_self,tp_at);
      create_tp = ret_val34;
      create_as_tp = ATTR(lv,as_type);
      L189=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
      if (L189==NULL) FATAL("Unable to allocate more memory");
      ((OB)L189)->header.tag=AM_LOCAL_EXPR_tag;
      L188 = ((AM_LOCAL_EXPR) L189);
      r = L188;
      SATTR(r,source1,create_src);
      SATTR(r,name1,create_name);
      SATTR(r,tp_at,create_tp);
      SATTR(r,as_type,create_as_tp);
      ret_val35 = r;
      SATTR(arg,expr,ret_val35);
      out_args = TRUE;
      mang_self3 = self;
      mang_ob3 = ((OB) lv);
      mang_ns = ((OB) sig1);
      ret_val36 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, mang_ns);
      d = ret_val36;
      SATTR(lv,name1,IDENT_zero);
     }
     plus_self28 = ((STR) &arg21);
     L1831_++; 
     plus_arg1 = L1831_;
     str_self2 = plus_arg1;
     clear_self1 = buf2;
     L192 = (clear_self1==((FSTR) NULL));
     L1931_=!(L192); 
     if (L1931_) {
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
     ret_val37 = STR_ap1077157958(plus_self28, s1, TRUE);
     MANGLE1827205184(ATTR(self,mangler), ((OB) ATTR(arg,expr)), ret_val37, ((OB) sig1));
     L1941_=INTPLUS(L351,1); 
     L351 = L1941_;
    }
   }
   after_loop: ;
  }
 }
 else {
  if (ATTR(f,is_abstract)) {
   plus_self29 = ATTR(self,code_c);
   mang_self4 = self;
   mang_ob4 = ((OB) ATTR(ATTR(f,sig1),tp));
   ret_val41 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
   plus_s25 = ((dSTR) ret_val41);
   L195 = (plus_s25==((dSTR) NULL));
   L1961_=!(L195); 
   if (L1961_) {
    L197 = ATTR(plus_self29,ntext);
    L198 = plus_s25;
    SATTR(plus_self29,ntext,FSTR_p1752847026(L197, (*dSTR_strrSTR[TAG(L198)])(L198)));
   }
   ret_val42 = plus_self29;
   plus_self30 = ret_val42;
   plus_s26 = ((dSTR) ((STR) &self8));
   L199 = ATTR(plus_self30,ntext);
   L200 = plus_s26;
   SATTR(plus_self30,ntext,FSTR_p1752847026(L199, (*dSTR_strrSTR[TAG(L200)])(L200)));
   L202 = (ATTR(ATTR(f,sig1),args)==((ARRAYARG) NULL));
   L2031_=!(L202); 
   if (L2031_) {
    i = 0;
    {
     BOOL f_L2041_ = TRUE;
     BOOL f_L2051_ = TRUE;
     /* loop index variable */
     L361 = 0;
     L41 = ATTR(ATTR(f,sig1),args);
     L51 = 1;
     L2041_br=L41==NULL?0:ASIZE((ARRAYARG)L41); 
     L2051_=L51-1; 
     while (1) {
      if(L361>=L2041_br)  goto after_loop1; 
      aL2041_=ARR((ARRAYARG)L41,L361); 
      e = aL2041_;
      plus_self31 = ((STR) &arg4);
      L2051_++; 
      plus_arg2 = L2051_;
      str_self4 = plus_arg2;
      clear_self2 = buf3;
      L208 = (clear_self2==((FSTR) NULL));
      L2091_=!(L208); 
      if (L2091_) {
       SATTR(clear_self2,loc,0);
      }
      str_in_self2 = str_self4;
      str_in_s2 = buf3;
      ret_val45 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
      buf3 = ret_val45;
      str_self5 = buf3;
      ret_val46 = STR_fr1097270334(((STR) NULL), str_self5);
      ret_val44 = ret_val46;
      s2 = ret_val44;
      ret_val43 = STR_ap1077157958(plus_self31, s2, TRUE);
      L210 = ret_val43;
      SARR((ARRAYSTR)arg_list,i,(STR)L210); 
      ;
      plus_self32 = ATTR(self,code_c);
      plus_s27 = ((dSTR) ((STR) &name29));
      L212 = (plus_s27==((dSTR) NULL));
      L2131_=!(L212); 
      if (L2131_) {
       L214 = ATTR(plus_self32,ntext);
       L215 = plus_s27;
       SATTR(plus_self32,ntext,FSTR_p1752847026(L214, (*dSTR_strrSTR[TAG(L215)])(L215)));
      }
      ret_val47 = plus_self32;
      plus_self34 = ret_val47;
      arg_type_str_sel = self;
      arg_type_str_e = e;
      mang_self5 = arg_type_str_sel;
      mang_ob5 = ((OB) ATTR(arg_type_str_e,tp));
      ret_val49 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
      res = ret_val49;
      L217 = ATTR(arg_type_str_e,mode1);
      if ((*dMODE_814247358[TAG(L217)])(L217, ((dMODE) MODES_inout_mode))) {
       L216 = TRUE;
      } else {
       L218 = ATTR(arg_type_str_e,mode1);
       L216 = (*dMODE_814247358[TAG(L218)])(L218, ((dMODE) MODES_out_mode));
      }
      if (L216) {
       plus_self33 = res;
       plus_sarg2 = ((STR) &name58);
       ret_val50 = STR_ap2004550586(plus_self33, plus_sarg2);
       res = ret_val50;
      }
      ret_val48 = res;
      plus_s28 = ((dSTR) ret_val48);
      L219 = (plus_s28==((dSTR) NULL));
      L2201_=!(L219); 
      if (L2201_) {
       L222 = ATTR(plus_self34,ntext);
       L223 = plus_s28;
       SATTR(plus_self34,ntext,FSTR_p1752847026(L222, (*dSTR_strrSTR[TAG(L223)])(L223)));
      }
      ret_val51 = plus_self34;
      plus_self35 = ret_val51;
      L2241_=(STR)ARR((ARRAYSTR)arg_list,i); 
      plus_s29 = ((dSTR) L2241_);
      L225 = ATTR(plus_self35,ntext);
      L226 = plus_s29;
      SATTR(plus_self35,ntext,FSTR_p1752847026(L225, (*dSTR_strrSTR[TAG(L226)])(L226)));
      L2271_=INTPLUS(L361,1); 
      L361 = L2271_;
     }
    }
    after_loop1: ;
   }
   if (ATTR(self,func_tables)) {
    plus_self36 = ATTR(self,code_c);
    plus_s30 = ((dSTR) ((STR) &struct167654362));
    L228 = ATTR(plus_self36,ntext);
    L229 = plus_s30;
    SATTR(plus_self36,ntext,FSTR_p1752847026(L228, (*dSTR_strrSTR[TAG(L229)])(L229)));
   }
  }
  else {
   if (ATTR(f,is_external)) {
    {
     BOOL f_L2301_ = TRUE;
     struct STR_se405450305_frame_struct other3_0;
STR_se405450305_frame noname1 = &other3_0;
     L81 = ((STR) &name29);
     noname1->self = L81;
     noname1->state = 0;
     while (1) {
      if (f_L2301_) {
       f_L2301_ = FALSE;
       L61 = 1;
       L2331_=ASIZE((AM_ROUT_DEF)f); 
       L234 = L2331_;
       L2351_=INTMINUS(L234,1); 
       L71 = L2351_;
       L2301_=L61-1;L2301_m=L71; 
      }
      if(L2301_++>=L2301_m)  goto after_loop2; 
      i1 = L2301_;
      L2361_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i1); 
      e1 = L2361_;
      L238 = ATTR(e1,mode1);
      if ((*dMODE_814247358[TAG(L238)])(L238, ((dMODE) MODES_inout_mode))) {
       L237 = TRUE;
      } else {
       L239 = ATTR(e1,mode1);
       L237 = (*dMODE_814247358[TAG(L239)])(L239, ((dMODE) MODES_out_mode));
      }
      if (L237) {
       expr11 = ATTR(e1,expr);
       create_self2 = ((AM_LOCAL_EXPR) NULL);
       create_src1 = ATTR(expr11,source1);
       create_name1 = ATTR(expr11,name1);
       tp_self1 = expr11;
       ret_val52 = ATTR(tp_self1,tp_at);
       create_tp1 = ret_val52;
       create_as_tp1 = ATTR(expr11,as_type);
       L242=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
       if (L242==NULL) FATAL("Unable to allocate more memory");
       ((OB)L242)->header.tag=AM_LOCAL_EXPR_tag;
       L240 = ((AM_LOCAL_EXPR) L242);
       r1 = L240;
       SATTR(r1,source1,create_src1);
       SATTR(r1,name1,create_name1);
       SATTR(r1,tp_at,create_tp1);
       SATTR(r1,as_type,create_as_tp1);
       ret_val53 = r1;
       lv1 = ret_val53;
       mang_self6 = self;
       mang_ob6 = ((OB) lv1);
       mang_ns1 = ((OB) sig1);
       ret_val54 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, mang_ns1);
       L243 = ret_val54;
       SARR((ARRAYSTR)arg_list,i1,(STR)L243); 
       ;
       out_args = TRUE;
      }
      else {
       mang_self7 = self;
       mang_ob7 = ((OB) ATTR(e1,expr));
       mang_ns2 = ((OB) sig1);
       ret_val55 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, mang_ns2);
       L245 = ret_val55;
       SARR((ARRAYSTR)arg_list,i1,(STR)L245); 
       ;
      }
      plus_self40 = ATTR(self,code_c);
      arg_type_str_sel1 = self;
      arg_type_str_e1 = e1;
      mang_self8 = arg_type_str_sel1;
      tp_self2 = ATTR(arg_type_str_e1,expr);
      ret_val57 = ATTR(tp_self2,tp_at);
      mang_ob8 = ((OB) ret_val57);
      ret_val58 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, ((OB) NULL));
      res1 = ret_val58;
      L249 = ATTR(arg_type_str_e1,mode1);
      if ((*dMODE_814247358[TAG(L249)])(L249, ((dMODE) MODES_inout_mode))) {
       L248 = TRUE;
      } else {
       L250 = ATTR(arg_type_str_e1,mode1);
       L248 = (*dMODE_814247358[TAG(L250)])(L250, ((dMODE) MODES_out_mode));
      }
      if (L248) {
       plus_self37 = res1;
       plus_sarg3 = ((STR) &name58);
       ret_val59 = STR_ap2004550586(plus_self37, plus_sarg3);
       res1 = ret_val59;
      }
      ret_val56 = res1;
      plus_self38 = ret_val56;
      plus_arg3 = ' ';
      str_self6 = plus_arg3;
      create_self3 = ((STR) NULL);
      create_c = str_self6;
      L253 = 1;
      L255=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L253)*sizeof(CHAR);
      L254=ZALLOC_LEAF_BIG(L255);
      if (L254==NULL) FATAL("Unable to allocate more memory");
      memset(L254,0,L255);
      ((OB)L254)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L254)->header.destroyed=0;
#endif

      L252 = ((STR) L254);
      L252->asize = L253;
      r2 = L252;
      SARR((STR)r2,0,create_c); 
      ;
      ret_val62 = r2;
      ret_val61 = ret_val62;
      s3 = ret_val61;
      ret_val60 = STR_ap1077157958(plus_self38, s3, TRUE);
      plus_self39 = ret_val60;
      L2571_=(STR)ARR((ARRAYSTR)arg_list,i1); 
      plus_sarg4 = L2571_;
      ret_val63 = STR_ap2004550586(plus_self39, plus_sarg4);
      noname1->arg1 = ((dSTR) ret_val63);
      L247 = STR_se405450305(noname1);
      if (noname1->state == -1) {
       goto after_loop2;
      }
      plus_s31 = ((dSTR) L247);
      L258 = ATTR(plus_self40,ntext);
      L259 = plus_s31;
      SATTR(plus_self40,ntext,FSTR_p1752847026(L258, (*dSTR_strrSTR[TAG(L259)])(L259)));
     }
    }
    after_loop2: ;
   }
   else {
    is_forked_self = sig1;
    if (ATTR(is_forked_self,is_par_routine)) {
     L262 = TRUE;
    } else {
     L262 = ATTR(is_forked_self,is_fork_routine);
    }
    if (L262) {
     L260 = TRUE;
    } else {
     L260 = ATTR(is_forked_self,is_attach_routin);
    }
    ret_val64 = L260;
    if (ret_val64) {
     {
      BOOL f_L2631_ = TRUE;
      while (1) {
       if (f_L2631_) {
        f_L2631_ = FALSE;
        L91 = 0;
        L2651_=ASIZE((AM_ROUT_DEF)f); 
        L266 = L2651_;
        L2671_=INTMINUS(L266,1); 
        L101 = L2671_;
        L2631_=L91-1;L2631_m=L101; 
       }
       if(L2631_++>=L2631_m)  goto after_loop3; 
       i2 = L2631_;
       L2681_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i2); 
       e2 = L2681_;
       L2691_=(i2)==(0); 
       if (L2691_) {
        SARR((ARRAYSTR)arg_list,i2,(STR)((STR) &self_to_be)); 
        ;
        plus_self41 = ATTR(self,code_c);
        plus_s32 = ((dSTR) ((STR) &OB3));
        L272 = (plus_s32==((dSTR) NULL));
        L2731_=!(L272); 
        if (L2731_) {
         L274 = ATTR(plus_self41,ntext);
         L275 = plus_s32;
         SATTR(plus_self41,ntext,FSTR_p1752847026(L274, (*dSTR_strrSTR[TAG(L275)])(L275)));
        }
        ret_val65 = plus_self41;
        plus_self42 = ret_val65;
        L2761_=(STR)ARR((ARRAYSTR)arg_list,i2); 
        plus_s33 = ((dSTR) L2761_);
        L277 = ATTR(plus_self42,ntext);
        L278 = plus_s33;
        SATTR(plus_self42,ntext,FSTR_p1752847026(L277, (*dSTR_strrSTR[TAG(L278)])(L278)));
       }
       else {
        L280 = ATTR(e2,mode1);
        if ((*dMODE_814247358[TAG(L280)])(L280, ((dMODE) MODES_out_mode))) {
         L279 = TRUE;
        } else {
         L282 = ATTR(e2,mode1);
         L279 = (*dMODE_814247358[TAG(L282)])(L282, ((dMODE) MODES_inout_mode));
        }
        if (L279) {
         expr21 = ATTR(e2,expr);
         create_self4 = ((AM_LOCAL_EXPR) NULL);
         create_src2 = ATTR(expr21,source1);
         create_name2 = ATTR(expr21,name1);
         tp_self3 = expr21;
         ret_val66 = ATTR(tp_self3,tp_at);
         create_tp2 = ret_val66;
         create_as_tp2 = ATTR(expr21,as_type);
         L284=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
         if (L284==NULL) FATAL("Unable to allocate more memory");
         ((OB)L284)->header.tag=AM_LOCAL_EXPR_tag;
         L283 = ((AM_LOCAL_EXPR) L284);
         r3 = L283;
         SATTR(r3,source1,create_src2);
         SATTR(r3,name1,create_name2);
         SATTR(r3,tp_at,create_tp2);
         SATTR(r3,as_type,create_as_tp2);
         ret_val67 = r3;
         lv2 = ret_val67;
         mang_self9 = self;
         mang_ob9 = ((OB) lv2);
         mang_ns3 = ((OB) sig1);
         ret_val68 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, mang_ns3);
         L285 = ret_val68;
         SARR((ARRAYSTR)arg_list,i2,(STR)L285); 
         ;
         out_args = TRUE;
        }
        else {
         mang_self10 = self;
         mang_ob10 = ((OB) ATTR(e2,expr));
         mang_ns4 = ((OB) sig1);
         ret_val69 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, mang_ns4);
         L287 = ret_val69;
         SARR((ARRAYSTR)arg_list,i2,(STR)L287); 
         ;
        }
        plus_self43 = ATTR(self,code_c);
        plus_s34 = ((dSTR) ((STR) &name29));
        L289 = (plus_s34==((dSTR) NULL));
        L2901_=!(L289); 
        if (L2901_) {
         L292 = ATTR(plus_self43,ntext);
         L293 = plus_s34;
         SATTR(plus_self43,ntext,FSTR_p1752847026(L292, (*dSTR_strrSTR[TAG(L293)])(L293)));
        }
        ret_val70 = plus_self43;
        plus_self45 = ret_val70;
        arg_type_str_sel2 = self;
        arg_type_str_e2 = e2;
        mang_self11 = arg_type_str_sel2;
        tp_self4 = ATTR(arg_type_str_e2,expr);
        ret_val72 = ATTR(tp_self4,tp_at);
        mang_ob11 = ((OB) ret_val72);
        ret_val73 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, ((OB) NULL));
        res2 = ret_val73;
        L295 = ATTR(arg_type_str_e2,mode1);
        if ((*dMODE_814247358[TAG(L295)])(L295, ((dMODE) MODES_inout_mode))) {
         L294 = TRUE;
        } else {
         L296 = ATTR(arg_type_str_e2,mode1);
         L294 = (*dMODE_814247358[TAG(L296)])(L296, ((dMODE) MODES_out_mode));
        }
        if (L294) {
         plus_self44 = res2;
         plus_sarg5 = ((STR) &name58);
         ret_val74 = STR_ap2004550586(plus_self44, plus_sarg5);
         res2 = ret_val74;
        }
        ret_val71 = res2;
        plus_s35 = ((dSTR) ret_val71);
        L297 = (plus_s35==((dSTR) NULL));
        L2981_=!(L297); 
        if (L2981_) {
         L299 = ATTR(plus_self45,ntext);
         L300 = plus_s35;
         SATTR(plus_self45,ntext,FSTR_p1752847026(L299, (*dSTR_strrSTR[TAG(L300)])(L300)));
        }
        ret_val75 = plus_self45;
        plus_self46 = ret_val75;
        L303=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
        if (L303==NULL) FATAL("Unable to allocate more memory");
        memset(L303,0,sizeof(struct CHAR_boxed_struct));
        ((OB)L303)->header.tag=CHAR_tag;
        L302 = (dSTR)((CHAR_boxed) L303);
        ((CHAR_boxed) L302)->immutable_part = ' ';
        plus_s36 = L302;
        L304 = (plus_s36==((dSTR) NULL));
        L3051_=!(L304); 
        if (L3051_) {
         L306 = ATTR(plus_self46,ntext);
         L307 = plus_s36;
         SATTR(plus_self46,ntext,FSTR_p1752847026(L306, (*dSTR_strrSTR[TAG(L307)])(L307)));
        }
        ret_val76 = plus_self46;
        plus_self47 = ret_val76;
        L3081_=(STR)ARR((ARRAYSTR)arg_list,i2); 
        plus_s37 = ((dSTR) L3081_);
        L309 = ATTR(plus_self47,ntext);
        L310 = plus_s37;
        SATTR(plus_self47,ntext,FSTR_p1752847026(L309, (*dSTR_strrSTR[TAG(L310)])(L310)));
       }
      }
     }
     after_loop3: ;
    }
    else {
     {
      BOOL f_L3111_ = TRUE;
      struct STR_se405450305_frame_struct other5_0;
STR_se405450305_frame noname2 = &other5_0;
      L141 = ((STR) &name29);
      noname2->self = L141;
      noname2->state = 0;
      while (1) {
       if (f_L3111_) {
        f_L3111_ = FALSE;
        L121 = 0;
        L3131_=ASIZE((AM_ROUT_DEF)f); 
        L314 = L3131_;
        L3151_=INTMINUS(L314,1); 
        L131 = L3151_;
        L3111_=L121-1;L3111_m=L131; 
       }
       if(L3111_++>=L3111_m)  goto after_loop4; 
       i3 = L3111_;
       L3161_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i3); 
       e3 = L3161_;
       L318 = ATTR(e3,mode1);
       if ((*dMODE_814247358[TAG(L318)])(L318, ((dMODE) MODES_inout_mode))) {
        L317 = TRUE;
       } else {
        L319 = ATTR(e3,mode1);
        L317 = (*dMODE_814247358[TAG(L319)])(L319, ((dMODE) MODES_out_mode));
       }
       if (L317) {
        expr31 = ATTR(e3,expr);
        create_self5 = ((AM_LOCAL_EXPR) NULL);
        create_src3 = ATTR(expr31,source1);
        create_name3 = ATTR(expr31,name1);
        tp_self5 = expr31;
        ret_val77 = ATTR(tp_self5,tp_at);
        create_tp3 = ret_val77;
        create_as_tp3 = ATTR(expr31,as_type);
        L322=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
        if (L322==NULL) FATAL("Unable to allocate more memory");
        ((OB)L322)->header.tag=AM_LOCAL_EXPR_tag;
        L320 = ((AM_LOCAL_EXPR) L322);
        r4 = L320;
        SATTR(r4,source1,create_src3);
        SATTR(r4,name1,create_name3);
        SATTR(r4,tp_at,create_tp3);
        SATTR(r4,as_type,create_as_tp3);
        ret_val78 = r4;
        lv3 = ret_val78;
        mang_self12 = self;
        mang_ob12 = ((OB) lv3);
        mang_ns5 = ((OB) sig1);
        ret_val79 = MANGLE119219986(ATTR(mang_self12,mangler), mang_ob12, mang_ns5);
        L323 = ret_val79;
        SARR((ARRAYSTR)arg_list,i3,(STR)L323); 
        ;
        out_args = TRUE;
       }
       else {
        mang_self13 = self;
        mang_ob13 = ((OB) ATTR(e3,expr));
        mang_ns6 = ((OB) sig1);
        ret_val80 = MANGLE119219986(ATTR(mang_self13,mangler), mang_ob13, mang_ns6);
        L325 = ret_val80;
        SARR((ARRAYSTR)arg_list,i3,(STR)L325); 
        ;
       }
       plus_self51 = ATTR(self,code_c);
       arg_type_str_sel3 = self;
       arg_type_str_e3 = e3;
       mang_self14 = arg_type_str_sel3;
       tp_self6 = ATTR(arg_type_str_e3,expr);
       ret_val82 = ATTR(tp_self6,tp_at);
       mang_ob14 = ((OB) ret_val82);
       ret_val83 = MANGLE119219986(ATTR(mang_self14,mangler), mang_ob14, ((OB) NULL));
       res3 = ret_val83;
       L329 = ATTR(arg_type_str_e3,mode1);
       if ((*dMODE_814247358[TAG(L329)])(L329, ((dMODE) MODES_inout_mode))) {
        L328 = TRUE;
       } else {
        L330 = ATTR(arg_type_str_e3,mode1);
        L328 = (*dMODE_814247358[TAG(L330)])(L330, ((dMODE) MODES_out_mode));
       }
       if (L328) {
        plus_self48 = res3;
        plus_sarg6 = ((STR) &name58);
        ret_val84 = STR_ap2004550586(plus_self48, plus_sarg6);
        res3 = ret_val84;
       }
       ret_val81 = res3;
       plus_self49 = ret_val81;
       plus_arg4 = ' ';
       str_self7 = plus_arg4;
       create_self6 = ((STR) NULL);
       create_c1 = str_self7;
       L333 = 1;
       L335=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L333)*sizeof(CHAR);
       L334=ZALLOC_LEAF_BIG(L335);
       if (L334==NULL) FATAL("Unable to allocate more memory");
       memset(L334,0,L335);
       ((OB)L334)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L334)->header.destroyed=0;
#endif

       L332 = ((STR) L334);
       L332->asize = L333;
       r5 = L332;
       SARR((STR)r5,0,create_c1); 
       ;
       ret_val87 = r5;
       ret_val86 = ret_val87;
       s4 = ret_val86;
       ret_val85 = STR_ap1077157958(plus_self49, s4, TRUE);
       plus_self50 = ret_val85;
       L3371_=(STR)ARR((ARRAYSTR)arg_list,i3); 
       plus_sarg7 = L3371_;
       ret_val88 = STR_ap2004550586(plus_self50, plus_sarg7);
       noname2->arg1 = ((dSTR) ret_val88);
       L327 = STR_se405450305(noname2);
       if (noname2->state == -1) {
        goto after_loop4;
       }
       plus_s38 = ((dSTR) L327);
       L338 = ATTR(plus_self51,ntext);
       L339 = plus_s38;
       SATTR(plus_self51,ntext,FSTR_p1752847026(L338, (*dSTR_strrSTR[TAG(L339)])(L339)));
      }
     }
     after_loop4: ;
     if (ATTR(self,func_tables)) {
      plus_self52 = ATTR(self,code_c);
      plus_s39 = ((dSTR) ((STR) &struct167654362));
      L340 = ATTR(plus_self52,ntext);
      L342 = plus_s39;
      SATTR(plus_self52,ntext,FSTR_p1752847026(L340, (*dSTR_strrSTR[TAG(L342)])(L342)));
     }
    }
   }
  }
 }
 plus_self53 = ATTR(self,code_c);
 plus_s40 = ((dSTR) ((STR) &name84));
 L343 = ATTR(plus_self53,ntext);
 L344 = plus_s40;
 SATTR(plus_self53,ntext,FSTR_p1752847026(L343, (*dSTR_strrSTR[TAG(L344)])(L344)));
 in_self = self;
 L345 = ATTR(in_self,indent);
 L3461_=INTPLUS(L345,1); 
 SATTR(in_self,indent,L3461_);
 if (out_args) {
  is_iter_self5 = f;
  is_iter_self6 = ATTR(is_iter_self5,sig1);
  is_iter_self7 = ATTR(is_iter_self6,name1);
  L349 = (is_iter_self7.str==((STR) NULL));
  L3501_=!(L349); 
  if (L3501_) {
   L352 = is_iter_self7.str;
   L353 = STR_sizerINT(is_iter_self7.str);
   L3541_=INTMINUS(L353,1); 
   L355 = L3541_;
   L3561_=ARR((STR)L352,L355); 
   L357 = L3561_;
   L3581_=L357=='!'; 
   L348 = L3581_;
  } else {
   L348 = FALSE;
  }
  ret_val91 = L348;
  ret_val90 = ret_val91;
  ret_val89 = ret_val90;
  L359 = ret_val89;
  L3601_=!(L359); 
  L347 = L3601_;
 } else {
  L347 = FALSE;
 }
 if (L347) {
  {
   BOOL f_L3621_ = TRUE;
   while (1) {
    if (f_L3621_) {
     f_L3621_ = FALSE;
     L151 = 1;
     L3641_=ASIZE((AM_ROUT_DEF)f); 
     L365 = L3641_;
     L3661_=INTMINUS(L365,1); 
     L161 = L3661_;
     L3621_=L151-1;L3621_m=L161; 
    }
    if(L3621_++>=L3621_m)  goto after_loop5; 
    i4 = L3621_;
    L3671_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,i4); 
    e4 = L3671_;
    L369 = ATTR(e4,mode1);
    if ((*dMODE_814247358[TAG(L369)])(L369, ((dMODE) MODES_inout_mode))) {
     L368 = TRUE;
    } else {
     L370 = ATTR(e4,mode1);
     L368 = (*dMODE_814247358[TAG(L370)])(L370, ((dMODE) MODES_out_mode));
    }
    if (L368) {
     L372 = ATTR(e4,mode1);
     if ((*dMODE_814247358[TAG(L372)])(L372, ((dMODE) MODES_inout_mode))) {
      plus_self54 = ((STR) &name58);
      L3731_=(STR)ARR((ARRAYSTR)arg_list,i4); 
      plus_sarg8 = L3731_;
      ret_val92 = STR_ap2004550586(plus_self54, plus_sarg8);
      init_str = ret_val92;
     }
     else {
      tp_self7 = e4;
      tp_self8 = ATTR(tp_self7,expr);
      ret_val94 = ATTR(tp_self8,tp_at);
      ret_val93 = ret_val94;
      init_str = CGEN_d815167520(self, ret_val93);
     }
     local11 = CGEN_d1279589030(self, e4, ATTR(f,sig1), init_str);
    }
   }
  }
  after_loop5: ;
 }
 if (ATTR(self,func_tables)) {
  create_self7 = ((FLISTSTR) NULL);
  ret_val95 = ((FLISTSTR) NULL);
  ll = ret_val95;
  cn = 0;
  is_iter_self8 = f;
  is_iter_self9 = ATTR(is_iter_self8,sig1);
  is_iter_self10 = ATTR(is_iter_self9,name1);
  L375 = (is_iter_self10.str==((STR) NULL));
  L3761_=!(L375); 
  if (L3761_) {
   L377 = is_iter_self10.str;
   L378 = STR_sizerINT(is_iter_self10.str);
   L3791_=INTMINUS(L378,1); 
   L380 = L3791_;
   L3821_=ARR((STR)L377,L380); 
   L383 = L3821_;
   L3841_=L383=='!'; 
   L374 = L3841_;
  } else {
   L374 = FALSE;
  }
  ret_val98 = L374;
  ret_val97 = ret_val98;
  ret_val96 = ret_val97;
  if (ret_val96) {
   {
    BOOL f_L3851_ = TRUE;
    struct FLISTA2119642552_frame_struct other7_0;
FLISTA2119642552_frame noname3 = &other7_0;
    /* loop index variable */
    L371 = 0;
    L171 = f;
    L181 = iter_out_arg_loc;
    L3851_br=L171==NULL?0:ASIZE((AM_ROUT_DEF)L171); 
    noname3->self = L181;
    noname3->state = 0;
    while (1) {
     L3861_=INTPLUS(cn,1); 
     cn = L3861_;
     if(L371>=L3851_br)  goto after_loop6; 
     aL3851_=ARR((AM_ROUT_DEF)L171,L371); 
     e5 = aL3851_;
     loc = ATTR(e5,expr);
     L389 = ATTR(e5,mode1);
     if ((*dMODE_814247358[TAG(L389)])(L389, ((dMODE) MODES_out_mode))) {
      L388 = TRUE;
     } else {
      L390 = ATTR(e5,mode1);
      L388 = (*dMODE_814247358[TAG(L390)])(L390, ((dMODE) MODES_inout_mode));
     }
     if (L388) {
      L392 = FLISTA2119642552(noname3);
      if (noname3->state == -1) {
       goto after_loop6;
      }
      loc = L392;
     }
     plus_self55 = ((STR) &voidframe);
     mang_self15 = self;
     mang_ob15 = ((OB) loc);
     mang_ns7 = ((OB) sig1);
     ret_val99 = MANGLE119219986(ATTR(mang_self15,mangler), mang_ob15, mang_ns7);
     plus_sarg9 = ret_val99;
     ret_val100 = STR_ap2004550586(plus_self55, plus_sarg9);
     ll = FLISTS1425610062(ll, ret_val100);
     L3931_=INTPLUS(L371,1); 
     L371 = L3931_;
    }
   }
   after_loop6: ;
   {
    struct FLISTA2119642552_frame_struct other8_0;
FLISTA2119642552_frame noname4 = &other8_0;
    L191 = ATTR(f,locals1);
    noname4->self = L191;
    noname4->state = 0;
    while (1) {
     L394 = FLISTA2119642552(noname4);
     if (noname4->state == -1) {
      goto after_loop7;
     }
     l = L394;
     L396 = (ATTR(l,name1).str==(STR)0);
     L3971_=!(L396); 
     if (L3971_) {
      is_eq_self = l;
      is_eq_a = ((OB) ATTR(f,rres));
      ret_val101 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
      L398 = ret_val101;
      L3991_=!(L398); 
      L395 = L3991_;
     } else {
      L395 = FALSE;
     }
     if (L395) {
      SATTR(l,needs_init,TRUE);
      plus_self56 = ((STR) &voidframe);
      mang_self16 = self;
      mang_ob16 = ((OB) l);
      mang_ns8 = ((OB) sig1);
      ret_val102 = MANGLE119219986(ATTR(mang_self16,mangler), mang_ob16, mang_ns8);
      plus_sarg10 = ret_val102;
      ret_val103 = STR_ap2004550586(plus_self56, plus_sarg10);
      ll = FLISTS1425610062(ll, ret_val103);
      L4001_=INTPLUS(cn,1); 
      cn = L4001_;
     }
    }
   }
   after_loop7: ;
  }
  else {
   {
    BOOL f_L4021_ = TRUE;
    BOOL f_L4031_ = TRUE;
    /* loop index variable */
    L381 = 0;
    L201 = f;
    L221 = 1;
    L4021_br=L201==NULL?0:ASIZE((AM_ROUT_DEF)L201); 
    L4031_=L221-1; 
    while (1) {
     plus_self57 = ((STR) &void7);
     mang_self17 = self;
     if(L381>=L4021_br)  goto after_loop8; 
     aL4021_=ARR((AM_ROUT_DEF)L201,L381); 
     L405=aL4021_;
     mang_ob17 = ((OB) ATTR(L405,expr));
     mang_ns9 = ((OB) sig1);
     ret_val104 = MANGLE119219986(ATTR(mang_self17,mangler), mang_ob17, mang_ns9);
     plus_sarg11 = ret_val104;
     ret_val105 = STR_ap2004550586(plus_self57, plus_sarg11);
     k = ret_val105;
     if (ATTR(f,is_external)) {
      L4031_++; 
      L408 = L4031_;
      L4091_=(L408)==(1); 
      L406 = L4091_;
     } else {
      L406 = FALSE;
     }
     if (L406) {
      k = ((STR) &NULL2);
     }
     ll = FLISTS1425610062(ll, k);
     L4101_=INTPLUS(cn,1); 
     cn = L4101_;
     L4111_=INTPLUS(L381,1); 
     L381 = L4111_;
    }
   }
   after_loop8: ;
   {
    struct FLISTA2119642552_frame_struct other10_0;
FLISTA2119642552_frame noname5 = &other10_0;
    L231 = ATTR(f,locals1);
    noname5->self = L231;
    noname5->state = 0;
    while (1) {
     L412 = FLISTA2119642552(noname5);
     if (noname5->state == -1) {
      goto after_loop9;
     }
     l1 = L412;
     L414 = (ATTR(l1,name1).str==(STR)0);
     L4151_=!(L414); 
     if (L4151_) {
      is_eq_self1 = l1;
      is_eq_a1 = ((OB) ATTR(f,rres));
      ret_val106 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
      L416 = ret_val106;
      L4171_=!(L416); 
      L413 = L4171_;
     } else {
      L413 = FALSE;
     }
     if (L413) {
      SATTR(l1,needs_init,TRUE);
      plus_self58 = ((STR) &void7);
      mang_self18 = self;
      mang_ob18 = ((OB) l1);
      mang_ns10 = ((OB) sig1);
      ret_val107 = MANGLE119219986(ATTR(mang_self18,mangler), mang_ob18, mang_ns10);
      plus_sarg12 = ret_val107;
      ret_val108 = STR_ap2004550586(plus_self58, plus_sarg12);
      ll = FLISTS1425610062(ll, ret_val108);
      L4181_=INTPLUS(cn,1); 
      cn = L4181_;
     }
    }
   }
   after_loop9: ;
  }
  ndefer_self = self;
  plus_self59 = ((STR) &void_local_frame);
  plus_arg5 = cn;
  str_self8 = plus_arg5;
  clear_self3 = buf4;
  L419 = (clear_self3==((FSTR) NULL));
  L4201_=!(L419); 
  if (L4201_) {
   SATTR(clear_self3,loc,0);
  }
  str_in_self3 = str_self8;
  str_in_s3 = buf4;
  ret_val1111 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
  buf4 = ret_val1111;
  str_self9 = buf4;
  ret_val112 = STR_fr1097270334(((STR) NULL), str_self9);
  ret_val110 = ret_val112;
  s5 = ret_val110;
  ret_val109 = STR_ap1077157958(plus_self59, s5, TRUE);
  plus_self60 = ret_val109;
  plus_sarg13 = ((STR) &name86);
  ret_val113 = STR_ap2004550586(plus_self60, plus_sarg13);
  ndefer_s = ret_val113;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  ndefer_self1 = self;
  ndefer_s1 = ((STR) &struct1270974273);
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  {
   BOOL f_L4211_ = TRUE;
   struct FLISTS1171631134_frame_struct other11_0;
FLISTS1171631134_frame noname6 = &other11_0;
   L241 = 0;
   L4221_=INTMINUS(cn,1); 
   L251 = L4221_;
   L261 = ll;
   L4211_=L241-1;L4211_m=L251; 
   noname6->self = L261;
   noname6->state = 0;
   while (1) {
    if(L4211_++>=L4211_m)  goto after_loop10; 
    i5 = L4211_;
    defer_self = self;
    str_self10 = i5;
    clear_self4 = buf5;
    L424 = (clear_self4==((FSTR) NULL));
    L4251_=!(L424); 
    if (L4251_) {
     SATTR(clear_self4,loc,0);
    }
    str_in_self4 = str_self10;
    str_in_s4 = buf5;
    ret_val115 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
    buf5 = ret_val115;
    str_self11 = buf5;
    ret_val116 = STR_fr1097270334(((STR) NULL), str_self11);
    ret_val114 = ret_val116;
    L426 = ret_val114;
    L427 = FLISTS1171631134(noname6);
    if (noname6->state == -1) {
     goto after_loop10;
    }
    L428 = L427;
    defer_s = STR_ap44172742(((STR) &S_local_frame), L426, ((STR) &name15), L428, ((STR) &name16));
    SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
   }
  }
  after_loop10: ;
  defer_self1 = self;
  defer_s1 = STR_ap1693864410(((STR) &FFfunc), rout_index, ((STR) &name16));
  SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
  if (ATTR(f,is_external)) {
   L429 = TRUE;
  } else {
   is_forked_self1 = sig1;
   if (ATTR(is_forked_self1,is_par_routine)) {
    L431 = TRUE;
   } else {
    L431 = ATTR(is_forked_self1,is_fork_routine);
   }
   if (L431) {
    L430 = TRUE;
   } else {
    L430 = ATTR(is_forked_self1,is_attach_routin);
   }
   ret_val117 = L430;
   L429 = ret_val117;
  }
  if (L429) {
   defer_self2 = self;
   defer_s2 = ((STR) &FFprevNULL);
   SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
  }
  else {
   defer_self3 = self;
   defer_s3 = ((STR) &FFprev530765537);
   SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
  }
  defer_self4 = self;
  defer_s4 = ((STR) &FFargs931318564);
  SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
 }
 if (out_args) {
  is_iter_self11 = f;
  is_iter_self12 = ATTR(is_iter_self11,sig1);
  is_iter_self13 = ATTR(is_iter_self12,name1);
  L434 = (is_iter_self13.str==((STR) NULL));
  L4351_=!(L434); 
  if (L4351_) {
   L436 = is_iter_self13.str;
   L437 = STR_sizerINT(is_iter_self13.str);
   L4381_=INTMINUS(L437,1); 
   L439 = L4381_;
   L4401_=ARR((STR)L436,L439); 
   L441 = L4401_;
   L4421_=L441=='!'; 
   L433 = L4421_;
  } else {
   L433 = FALSE;
  }
  ret_val120 = L433;
  ret_val119 = ret_val120;
  ret_val118 = ret_val119;
  L432 = ret_val118;
 } else {
  L432 = FALSE;
 }
 if (L432) {
  SATTR(f,locals1,FLISTA189055531(ATTR(f,locals1), iter_out_arg_loc));
  {
   BOOL f_L4431_ = TRUE;
   struct FLISTA2119642552_frame_struct other12_0;
FLISTA2119642552_frame noname7 = &other12_0;
   /* loop index variable */
   L391 = 0;
   L271 = f;
   L281 = iter_out_arg_loc;
   L4431_br=L271==NULL?0:ASIZE((AM_ROUT_DEF)L271); 
   noname7->self = L281;
   noname7->state = 0;
   while (1) {
    if(L391>=L4431_br)  goto after_loop11; 
    aL4431_=ARR((AM_ROUT_DEF)L271,L391); 
    e6 = aL4431_;
    L446 = ATTR(e6,mode1);
    if ((*dMODE_814247358[TAG(L446)])(L446, ((dMODE) MODES_out_mode))) {
     L445 = TRUE;
    } else {
     L447 = ATTR(e6,mode1);
     L445 = (*dMODE_814247358[TAG(L447)])(L447, ((dMODE) MODES_inout_mode));
    }
    if (L445) {
     L448 = FLISTA2119642552(noname7);
     if (noname7->state == -1) {
      goto after_loop11;
     }
     l2 = L448;
     L291 = ATTR(e6,mode1);
     L449 = L291;
     if ((*dMODE_814247358[TAG(L449)])(L449, ((dMODE) MODES_inout_mode))) {
      ndefer_self2 = self;
      plus_self61 = ((STR) &frame1);
      mang_self19 = self;
      mang_ob19 = ((OB) l2);
      mang_ns11 = ((OB) sig1);
      ret_val121 = MANGLE119219986(ATTR(mang_self19,mangler), mang_ob19, mang_ns11);
      plus_sarg14 = ret_val121;
      ret_val122 = STR_ap2004550586(plus_self61, plus_sarg14);
      plus_self62 = ret_val122;
      plus_sarg15 = ((STR) &name149);
      ret_val123 = STR_ap2004550586(plus_self62, plus_sarg15);
      plus_self63 = ret_val123;
      plus_sarg16 = ((STR) &frame1);
      ret_val124 = STR_ap2004550586(plus_self63, plus_sarg16);
      plus_self64 = ret_val124;
      mang_self20 = self;
      mang_ob20 = ((OB) ATTR(e6,expr));
      mang_ns12 = ((OB) sig1);
      ret_val125 = MANGLE119219986(ATTR(mang_self20,mangler), mang_ob20, mang_ns12);
      plus_sarg17 = ret_val125;
      ret_val126 = STR_ap2004550586(plus_self64, plus_sarg17);
      plus_self65 = ret_val126;
      plus_sarg18 = ((STR) &name76);
      ret_val127 = STR_ap2004550586(plus_self65, plus_sarg18);
      ndefer_s2 = ret_val127;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
     else {
      L450 = L291;
      if ((*dMODE_814247358[TAG(L450)])(L450, ((dMODE) MODES_out_mode))) {
       ndefer_self3 = self;
       plus_self66 = ((STR) &frame1);
       mang_self21 = self;
       mang_ob21 = ((OB) l2);
       mang_ns13 = ((OB) sig1);
       ret_val128 = MANGLE119219986(ATTR(mang_self21,mangler), mang_ob21, mang_ns13);
       plus_sarg19 = ret_val128;
       ret_val129 = STR_ap2004550586(plus_self66, plus_sarg19);
       plus_self67 = ret_val129;
       plus_sarg20 = ((STR) &name36);
       ret_val130 = STR_ap2004550586(plus_self67, plus_sarg20);
       plus_self68 = ret_val130;
       tp_self9 = l2;
       ret_val131 = ATTR(tp_self9,tp_at);
       plus_sarg21 = CGEN_d815167520(self, ret_val131);
       ret_val132 = STR_ap2004550586(plus_self68, plus_sarg21);
       plus_self69 = ret_val132;
       plus_arg6 = ';';
       str_self12 = plus_arg6;
       create_self8 = ((STR) NULL);
       create_c2 = str_self12;
       L452 = 1;
       L454=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L452)*sizeof(CHAR);
       L453=ZALLOC_LEAF_BIG(L454);
       if (L453==NULL) FATAL("Unable to allocate more memory");
       memset(L453,0,L454);
       ((OB)L453)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L453)->header.destroyed=0;
#endif

       L451 = ((STR) L453);
       L451->asize = L452;
       r6 = L451;
       SARR((STR)r6,0,create_c2); 
       ;
       ret_val135 = r6;
       ret_val134 = ret_val135;
       s6 = ret_val134;
       ret_val133 = STR_ap1077157958(plus_self69, s6, TRUE);
       ndefer_s3 = ret_val133;
       CGEN_d695831906(ndefer_self3);
       SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
      }
      else {
       FATAL("No applicable target in case statement\nin CGEN::emit_prologue(AM_ROUT_DEF)\n./Back/cgen.sa:1847:19");
      }
     }
    }
    L4561_=INTPLUS(L391,1); 
    L391 = L4561_;
   }
  }
  after_loop11: ;
 }
 is_iter_self14 = f;
 is_iter_self15 = ATTR(is_iter_self14,sig1);
 is_iter_self16 = ATTR(is_iter_self15,name1);
 L459 = (is_iter_self16.str==((STR) NULL));
 L4601_=!(L459); 
 if (L4601_) {
  L462 = is_iter_self16.str;
  L463 = STR_sizerINT(is_iter_self16.str);
  L4641_=INTMINUS(L463,1); 
  L465 = L4641_;
  L4661_=ARR((STR)L462,L465); 
  L467 = L4661_;
  L4681_=L467=='!'; 
  L458 = L4681_;
 } else {
  L458 = FALSE;
 }
 ret_val138 = L458;
 ret_val137 = ret_val138;
 ret_val136 = ret_val137;
 L469 = ret_val136;
 L4701_=!(L469); 
 if (L4701_) {
  L471 = (ATTR(f,locals1)==((FLISTA725283029) NULL));
  L4721_=!(L471); 
  L457 = L4721_;
 } else {
  L457 = FALSE;
 }
 if (L457) {
  {
   struct FLISTA2119642552_frame_struct other13_0;
FLISTA2119642552_frame noname8 = &other13_0;
   L301 = ATTR(f,locals1);
   noname8->self = L301;
   noname8->state = 0;
   while (1) {
    L473 = FLISTA2119642552(noname8);
    if (noname8->state == -1) {
     goto after_loop12;
    }
    lv4 = L473;
    uses_tp_self = ATTR(self,code_c);
    tp_self10 = lv4;
    ret_val139 = ATTR(tp_self10,tp_at);
    uses_tp_t = ret_val139;
    CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
    CGEN_newline(self);
    if (ATTR(lv4,is_volatile)) {
     plus_self70 = ATTR(self,code_c);
     plus_s41 = ((dSTR) ((STR) &volatile3));
     L474 = ATTR(plus_self70,ntext);
     L475 = plus_s41;
     SATTR(plus_self70,ntext,FSTR_p1752847026(L474, (*dSTR_strrSTR[TAG(L475)])(L475)));
    }
    if (ATTR(lv4,needs_init)) {
     L476 = TRUE;
    } else {
     tp_self11 = lv4;
     ret_val140 = ATTR(tp_self11,tp_at);
     L477 = ret_val140;
     L476 = (*dTP_is1334578382[TAG(L477)])(L477);
    }
    if (L476) {
     tp_self12 = lv4;
     ret_val141 = ATTR(tp_self12,tp_at);
     def = CGEN_d815167520(self, ret_val141);
     plus_self71 = ATTR(self,code_c);
     mang_self22 = self;
     tp_self13 = lv4;
     ret_val142 = ATTR(tp_self13,tp_at);
     mang_ob22 = ((OB) ret_val142);
     ret_val143 = MANGLE119219986(ATTR(mang_self22,mangler), mang_ob22, ((OB) NULL));
     plus_s42 = ((dSTR) ret_val143);
     L478 = (plus_s42==((dSTR) NULL));
     L4791_=!(L478); 
     if (L4791_) {
      L480 = ATTR(plus_self71,ntext);
      L481 = plus_s42;
      SATTR(plus_self71,ntext,FSTR_p1752847026(L480, (*dSTR_strrSTR[TAG(L481)])(L481)));
     }
     ret_val144 = plus_self71;
     plus_self72 = ret_val144;
     L483=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L483==NULL) FATAL("Unable to allocate more memory");
     memset(L483,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L483)->header.tag=CHAR_tag;
     L482 = (dSTR)((CHAR_boxed) L483);
     ((CHAR_boxed) L482)->immutable_part = ' ';
     plus_s43 = L482;
     L484 = (plus_s43==((dSTR) NULL));
     L4851_=!(L484); 
     if (L4851_) {
      L486 = ATTR(plus_self72,ntext);
      L487 = plus_s43;
      SATTR(plus_self72,ntext,FSTR_p1752847026(L486, (*dSTR_strrSTR[TAG(L487)])(L487)));
     }
     ret_val145 = plus_self72;
     plus_self73 = ret_val145;
     mang_self23 = self;
     mang_ob23 = ((OB) lv4);
     mang_ns14 = ((OB) sig1);
     ret_val146 = MANGLE119219986(ATTR(mang_self23,mangler), mang_ob23, mang_ns14);
     plus_s44 = ((dSTR) ret_val146);
     L488 = (plus_s44==((dSTR) NULL));
     L4891_=!(L488); 
     if (L4891_) {
      L490 = ATTR(plus_self73,ntext);
      L491 = plus_s44;
      SATTR(plus_self73,ntext,FSTR_p1752847026(L490, (*dSTR_strrSTR[TAG(L491)])(L491)));
     }
     ret_val147 = plus_self73;
     plus_self74 = ret_val147;
     plus_s45 = ((dSTR) ((STR) &name36));
     L492 = (plus_s45==((dSTR) NULL));
     L4931_=!(L492); 
     if (L4931_) {
      L494 = ATTR(plus_self74,ntext);
      L495 = plus_s45;
      SATTR(plus_self74,ntext,FSTR_p1752847026(L494, (*dSTR_strrSTR[TAG(L495)])(L495)));
     }
     ret_val148 = plus_self74;
     plus_self75 = ret_val148;
     plus_s46 = ((dSTR) def);
     L496 = (plus_s46==((dSTR) NULL));
     L4971_=!(L496); 
     if (L4971_) {
      L498 = ATTR(plus_self75,ntext);
      L499 = plus_s46;
      SATTR(plus_self75,ntext,FSTR_p1752847026(L498, (*dSTR_strrSTR[TAG(L499)])(L499)));
     }
     ret_val149 = plus_self75;
     plus_self76 = ret_val149;
     L502=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L502==NULL) FATAL("Unable to allocate more memory");
     memset(L502,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L502)->header.tag=CHAR_tag;
     L500 = (dSTR)((CHAR_boxed) L502);
     ((CHAR_boxed) L500)->immutable_part = ';';
     plus_s47 = L500;
     L503 = ATTR(plus_self76,ntext);
     L504 = plus_s47;
     SATTR(plus_self76,ntext,FSTR_p1752847026(L503, (*dSTR_strrSTR[TAG(L504)])(L504)));
    }
    else {
     plus_self77 = ATTR(self,code_c);
     mang_self24 = self;
     tp_self14 = lv4;
     ret_val150 = ATTR(tp_self14,tp_at);
     mang_ob24 = ((OB) ret_val150);
     ret_val151 = MANGLE119219986(ATTR(mang_self24,mangler), mang_ob24, ((OB) NULL));
     plus_s48 = ((dSTR) ret_val151);
     L505 = (plus_s48==((dSTR) NULL));
     L5061_=!(L505); 
     if (L5061_) {
      L507 = ATTR(plus_self77,ntext);
      L508 = plus_s48;
      SATTR(plus_self77,ntext,FSTR_p1752847026(L507, (*dSTR_strrSTR[TAG(L508)])(L508)));
     }
     ret_val152 = plus_self77;
     plus_self78 = ret_val152;
     L510=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L510==NULL) FATAL("Unable to allocate more memory");
     memset(L510,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L510)->header.tag=CHAR_tag;
     L509 = (dSTR)((CHAR_boxed) L510);
     ((CHAR_boxed) L509)->immutable_part = ' ';
     plus_s49 = L509;
     L511 = (plus_s49==((dSTR) NULL));
     L5121_=!(L511); 
     if (L5121_) {
      L513 = ATTR(plus_self78,ntext);
      L514 = plus_s49;
      SATTR(plus_self78,ntext,FSTR_p1752847026(L513, (*dSTR_strrSTR[TAG(L514)])(L514)));
     }
     ret_val153 = plus_self78;
     plus_self79 = ret_val153;
     mang_self25 = self;
     mang_ob25 = ((OB) lv4);
     mang_ns15 = ((OB) sig1);
     ret_val154 = MANGLE119219986(ATTR(mang_self25,mangler), mang_ob25, mang_ns15);
     plus_s50 = ((dSTR) ret_val154);
     L515 = (plus_s50==((dSTR) NULL));
     L5161_=!(L515); 
     if (L5161_) {
      L517 = ATTR(plus_self79,ntext);
      L518 = plus_s50;
      SATTR(plus_self79,ntext,FSTR_p1752847026(L517, (*dSTR_strrSTR[TAG(L518)])(L518)));
     }
     ret_val155 = plus_self79;
     plus_self80 = ret_val155;
     L520=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
     if (L520==NULL) FATAL("Unable to allocate more memory");
     memset(L520,0,sizeof(struct CHAR_boxed_struct));
     ((OB)L520)->header.tag=CHAR_tag;
     L519 = (dSTR)((CHAR_boxed) L520);
     ((CHAR_boxed) L519)->immutable_part = ';';
     plus_s51 = L519;
     L521 = ATTR(plus_self80,ntext);
     L522 = plus_s51;
     SATTR(plus_self80,ntext,FSTR_p1752847026(L521, (*dSTR_strrSTR[TAG(L522)])(L522)));
    }
   }
  }
  after_loop12: ;
 }
 is_forked_self2 = sig1;
 if (ATTR(is_forked_self2,is_par_routine)) {
  L524 = TRUE;
 } else {
  L524 = ATTR(is_forked_self2,is_fork_routine);
 }
 if (L524) {
  L523 = TRUE;
 } else {
  L523 = ATTR(is_forked_self2,is_attach_routin);
 }
 ret_val156 = L523;
 if (ret_val156) {
  CGEN_newline(self);
  plus_self81 = ATTR(self,code_c);
  mang_self26 = self;
  L5251_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)f,0); 
  L526=L5251_;
  tp_self15 = ATTR(L526,expr);
  ret_val157 = ATTR(tp_self15,tp_at);
  mang_ob26 = ((OB) ret_val157);
  ret_val158 = MANGLE119219986(ATTR(mang_self26,mangler), mang_ob26, ((OB) NULL));
  plus_s52 = ((dSTR) ret_val158);
  L527 = (plus_s52==((dSTR) NULL));
  L5281_=!(L527); 
  if (L5281_) {
   L529 = ATTR(plus_self81,ntext);
   L530 = plus_s52;
   SATTR(plus_self81,ntext,FSTR_p1752847026(L529, (*dSTR_strrSTR[TAG(L530)])(L530)));
  }
  ret_val159 = plus_self81;
  plus_self82 = ret_val159;
  plus_s53 = ((dSTR) ((STR) &self4));
  L531 = ATTR(plus_self82,ntext);
  L532 = plus_s53;
  SATTR(plus_self82,ntext,FSTR_p1752847026(L531, (*dSTR_strrSTR[TAG(L532)])(L532)));
  ndefer_self4 = self;
  plus_self83 = ((STR) &self5);
  cast_self = self;
  cast_dest_tp = ATTR(sig1,tp);
  cast_src_tp = ((dTP) TP_BUI1325635093);
  cast_expr = ((STR) &self_to_be);
  cast_only_boxing = FALSE;
  ret_val160 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  plus_sarg22 = ret_val160;
  ret_val161 = STR_ap2004550586(plus_self83, plus_sarg22);
  plus_self84 = ret_val161;
  plus_arg7 = ';';
  str_self13 = plus_arg7;
  create_self9 = ((STR) NULL);
  create_c3 = str_self13;
  L534 = 1;
  L536=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L534)*sizeof(CHAR);
  L535=ZALLOC_LEAF_BIG(L536);
  if (L535==NULL) FATAL("Unable to allocate more memory");
  memset(L535,0,L536);
  ((OB)L535)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L535)->header.destroyed=0;
#endif

  L533 = ((STR) L535);
  L533->asize = L534;
  r7 = L533;
  SARR((STR)r7,0,create_c3); 
  ;
  ret_val164 = r7;
  ret_val163 = ret_val164;
  s7 = ret_val163;
  ret_val162 = STR_ap1077157958(plus_self84, s7, TRUE);
  ndefer_s4 = ret_val162;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 is_iter_self17 = f;
 is_iter_self18 = ATTR(is_iter_self17,sig1);
 is_iter_self19 = ATTR(is_iter_self18,name1);
 L539 = (is_iter_self19.str==((STR) NULL));
 L5401_=!(L539); 
 if (L5401_) {
  L542 = is_iter_self19.str;
  L543 = STR_sizerINT(is_iter_self19.str);
  L5441_=INTMINUS(L543,1); 
  L545 = L5441_;
  L5461_=ARR((STR)L542,L545); 
  L547 = L5461_;
  L5481_=L547=='!'; 
  L538 = L5481_;
 } else {
  L538 = FALSE;
 }
 ret_val167 = L538;
 ret_val166 = ret_val167;
 ret_val165 = ret_val166;
 if (ret_val165) {
  L549 = (ATTR(sig1,ret)==((dTP) NULL));
  L5501_=!(L549); 
  if (L5501_) {
   CGEN_newline(self);
   plus_self85 = ATTR(self,code_c);
   mang_self27 = self;
   mang_ob27 = ((OB) ATTR(sig1,ret));
   ret_val168 = MANGLE119219986(ATTR(mang_self27,mangler), mang_ob27, ((OB) NULL));
   plus_s54 = ((dSTR) ret_val168);
   L551 = (plus_s54==((dSTR) NULL));
   L5521_=!(L551); 
   if (L5521_) {
    L553 = ATTR(plus_self85,ntext);
    L554 = plus_s54;
    SATTR(plus_self85,ntext,FSTR_p1752847026(L553, (*dSTR_strrSTR[TAG(L554)])(L554)));
   }
   ret_val169 = plus_self85;
   plus_self86 = ret_val169;
   L556=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L556==NULL) FATAL("Unable to allocate more memory");
   memset(L556,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L556)->header.tag=CHAR_tag;
   L555 = (dSTR)((CHAR_boxed) L556);
   ((CHAR_boxed) L555)->immutable_part = ' ';
   plus_s55 = L555;
   L557 = (plus_s55==((dSTR) NULL));
   L5581_=!(L557); 
   if (L5581_) {
    L559 = ATTR(plus_self86,ntext);
    L560 = plus_s55;
    SATTR(plus_self86,ntext,FSTR_p1752847026(L559, (*dSTR_strrSTR[TAG(L560)])(L560)));
   }
   ret_val170 = plus_self86;
   plus_self87 = ret_val170;
   plus_s56 = ((dSTR) ((STR) &dummy11));
   L562 = (plus_s56==((dSTR) NULL));
   L5631_=!(L562); 
   if (L5631_) {
    L564 = ATTR(plus_self87,ntext);
    L565 = plus_s56;
    SATTR(plus_self87,ntext,FSTR_p1752847026(L564, (*dSTR_strrSTR[TAG(L565)])(L565)));
   }
   ret_val171 = plus_self87;
   plus_self88 = ret_val171;
   plus_s57 = ((dSTR) CGEN_d815167520(self, ATTR(sig1,ret)));
   L566 = (plus_s57==((dSTR) NULL));
   L5671_=!(L566); 
   if (L5671_) {
    L568 = ATTR(plus_self88,ntext);
    L569 = plus_s57;
    SATTR(plus_self88,ntext,FSTR_p1752847026(L568, (*dSTR_strrSTR[TAG(L569)])(L569)));
   }
   ret_val172 = plus_self88;
   plus_self89 = ret_val172;
   L571=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L571==NULL) FATAL("Unable to allocate more memory");
   memset(L571,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L571)->header.tag=CHAR_tag;
   L570 = (dSTR)((CHAR_boxed) L571);
   ((CHAR_boxed) L570)->immutable_part = ';';
   plus_s58 = L570;
   L572 = ATTR(plus_self89,ntext);
   L573 = plus_s58;
   SATTR(plus_self89,ntext,FSTR_p1752847026(L572, (*dSTR_strrSTR[TAG(L573)])(L573)));
  }
  psather_self = ATTR(self,prog);
  if (ATTR(psather_self,threads)) {
   L575 = TRUE;
  } else {
   L575 = ATTR(psather_self,distributed);
  }
  ret_val173 = L575;
  if (ret_val173) {
   L578 = ATTR(ATTR(self,prog),yields_in_locks);
   L5791_=!(L578); 
   if (L5791_) {
    L577 = TRUE;
   } else {
    L580 = ATTR(ATTR(self,options1),side_effects);
    L5821_=!(L580); 
    L577 = L5821_;
   }
   if (L577) {
    L576 = TRUE;
   } else {
    L583=SIG_ge642497605(sig1, ATTR(self,prog));
    L576 = ATTR(L583,has_yield_in_loc);
   }
   L574 = L576;
  } else {
   L574 = FALSE;
  }
  if (L574) {
   ndefer_self5 = self;
   ndefer_s5 = ((STR) &iffram1733377389);
   CGEN_d695831906(ndefer_self5);
   SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
  }
  ndefer_self6 = self;
  ndefer_s6 = ((STR) &switchframestate);
  CGEN_d695831906(ndefer_self6);
  SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
  in_self1 = self;
  L584 = ATTR(in_self1,indent);
  L5851_=INTPLUS(L584,1); 
  SATTR(in_self1,indent,L5851_);
  {
   BOOL f_L5861_ = TRUE;
   L321 = 0;
   L331 = ATTR(f,num_yields);
   L5861_=L321-1;L5861_m=L331; 
   while (1) {
    if(L5861_++>=L5861_m)  goto after_loop13; 
    i6 = L5861_;
    ndefer_self7 = self;
    plus_self90 = ((STR) &case3);
    plus_arg8 = i6;
    str_self14 = plus_arg8;
    clear_self5 = buf6;
    L588 = (clear_self5==((FSTR) NULL));
    L5891_=!(L588); 
    if (L5891_) {
     SATTR(clear_self5,loc,0);
    }
    str_in_self5 = str_self14;
    str_in_s5 = buf6;
    ret_val176 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
    buf6 = ret_val176;
    str_self15 = buf6;
    ret_val177 = STR_fr1097270334(((STR) NULL), str_self15);
    ret_val175 = ret_val177;
    s8 = ret_val175;
    ret_val174 = STR_ap1077157958(plus_self90, s8, TRUE);
    plus_self91 = ret_val174;
    plus_sarg23 = ((STR) &gotostate);
    ret_val178 = STR_ap2004550586(plus_self91, plus_sarg23);
    plus_self92 = ret_val178;
    plus_arg9 = i6;
    str_self16 = plus_arg9;
    clear_self6 = buf7;
    L590 = (clear_self6==((FSTR) NULL));
    L5911_=!(L590); 
    if (L5911_) {
     SATTR(clear_self6,loc,0);
    }
    str_in_self6 = str_self16;
    str_in_s6 = buf7;
    ret_val181 = INT_st367594495(str_in_self6, str_in_s6, 0, 10, ' ');
    buf7 = ret_val181;
    str_self17 = buf7;
    ret_val182 = STR_fr1097270334(((STR) NULL), str_self17);
    ret_val180 = ret_val182;
    s9 = ret_val180;
    ret_val179 = STR_ap1077157958(plus_self92, s9, TRUE);
    plus_self93 = ret_val179;
    plus_arg10 = ';';
    str_self18 = plus_arg10;
    create_self10 = ((STR) NULL);
    create_c4 = str_self18;
    L593 = 1;
    L595=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L593)*sizeof(CHAR);
    L594=ZALLOC_LEAF_BIG(L595);
    if (L594==NULL) FATAL("Unable to allocate more memory");
    memset(L594,0,L595);
    ((OB)L594)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L594)->header.destroyed=0;
#endif

    L592 = ((STR) L594);
    L592->asize = L593;
    r8 = L592;
    SARR((STR)r8,0,create_c4); 
    ;
    ret_val185 = r8;
    ret_val184 = ret_val185;
    s10 = ret_val184;
    ret_val183 = STR_ap1077157958(plus_self93, s10, TRUE);
    ndefer_s7 = ret_val183;
    CGEN_d695831906(ndefer_self7);
    SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
   }
  }
  after_loop13: ;
  ndefer_self8 = self;
  ndefer_s8 = ((STR) &name4);
  CGEN_d695831906(ndefer_self8);
  SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
  move_out_self = self;
  L597 = ATTR(move_out_self,indent);
  L5981_=INTMINUS(L597,1); 
  SATTR(move_out_self,indent,L5981_);
  ndefer_self9 = self;
  ndefer_s9 = ((STR) &state02);
  CGEN_d695831906(ndefer_self9);
  SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
  SATTR(self,state_counter,1);
  L599 = (ATTR(f,locals1)==((FLISTA725283029) NULL));
  L6001_=!(L599); 
  if (L6001_) {
   {
    struct FLISTA2119642552_frame_struct other15_0;
FLISTA2119642552_frame noname9 = &other15_0;
    L341 = ATTR(f,locals1);
    noname9->self = L341;
    noname9->state = 0;
    while (1) {
     L601 = FLISTA2119642552(noname9);
     if (noname9->state == -1) {
      goto after_loop14;
     }
     i7 = L601;
     if (ATTR(i7,needs_init)) {
      ndefer_self10 = self;
      plus_self94 = ((STR) &frame1);
      mang_self28 = self;
      mang_ob28 = ((OB) i7);
      mang_ns16 = ((OB) sig1);
      ret_val186 = MANGLE119219986(ATTR(mang_self28,mangler), mang_ob28, mang_ns16);
      plus_sarg24 = ret_val186;
      ret_val187 = STR_ap2004550586(plus_self94, plus_sarg24);
      plus_self95 = ret_val187;
      plus_sarg25 = ((STR) &name36);
      ret_val188 = STR_ap2004550586(plus_self95, plus_sarg25);
      plus_self96 = ret_val188;
      tp_self16 = i7;
      ret_val189 = ATTR(tp_self16,tp_at);
      plus_sarg26 = CGEN_d815167520(self, ret_val189);
      ret_val190 = STR_ap2004550586(plus_self96, plus_sarg26);
      plus_self97 = ret_val190;
      plus_sarg27 = ((STR) &name16);
      ret_val191 = STR_ap2004550586(plus_self97, plus_sarg27);
      ndefer_s10 = ret_val191;
      CGEN_d695831906(ndefer_self10);
      SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
     }
    }
   }
   after_loop14: ;
  }
 }
 SATTR(self,current_arg_list,arg_list);
 SATTR(self,current_iter_out,iter_out_arg_loc);
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_p214884983(CGEN self, AM_ROUT_DEF func) {
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 INT ind1 = INT_zero;
 SIG is_iter_self;
 BOOL ret_val = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 AM_ROUT_DEF is_iter_self2;
 BOOL ret_val2 = BOOL_zero;
 SIG is_iter_self3;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_iter_self4 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 CODE_FILE here_is_iter_sel;
 AM_ROUT_DEF here_is_iter_a;
 CODE_FILE uses_iter_self;
 SIG uses_iter_s;
 MANGLE dispose_namespac;
 OB dispose_namespac1;
 BOOL L1;
 BOOL L2;
 BOOL L31_;
 STR L4;
 INT L5;
 INT L61_;
 INT L7;
 CHAR L81_;
 CHAR L9;
 BOOL L101_;
 INT L11;
 INT L121_;
 BOOL L13;
 BOOL L14;
 extern STR AM_POLL_NETWORK1;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 STR L18;
 INT L19;
 INT L201_;
 INT L21;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 SATTR(self,ex_nesting,0);
 ind1 = ATTR(self,indent);
 SATTR(self,indent,0);
 SATTR(self,manual_unlock,FALSE);
 SATTR(self,manual_loop_unlo,FALSE);
 if (SIG_is418491101(ATTR(func,sig1))) {
  is_iter_self = ATTR(func,sig1);
  is_iter_self1 = ATTR(is_iter_self,name1);
  L2 = (is_iter_self1.str==((STR) NULL));
  L31_=!(L2); 
  if (L31_) {
   L4 = is_iter_self1.str;
   L5 = STR_sizerINT(is_iter_self1.str);
   L61_=INTMINUS(L5,1); 
   L7 = L61_;
   L81_=ARR((STR)L4,L7); 
   L9 = L81_;
   L101_=L9=='!'; 
   L1 = L101_;
  } else {
   L1 = FALSE;
  }
  ret_val1 = L1;
  ret_val = ret_val1;
  if (ret_val) {
   CGEN_c1869222825(self, func);
  }
  else {
   return;
  }
 }
 L11 = ATTR(self,routine_count);
 L121_=INTPLUS(L11,1); 
 SATTR(self,routine_count,L121_);
 CGEN_s1419723103(self, func);
 if (ATTR(func,is_external)) {
  L13 = SIG_is275586960(ATTR(func,sig1));
 } else {
  L13 = FALSE;
 }
 if (L13) {
  CGEN_e233957487(self, func);
  CGEN_e494460490(self, ATTR(func,code1));
  CGEN_e520888267(self, func);
 }
 else {
  if (ATTR(func,is_external)) {
   L14 = SIG_is600379186(ATTR(func,sig1));
  } else {
   L14 = FALSE;
  }
  if (L14) {
   CGEN_e351697741(self, func);
   CGEN_e494460490(self, ATTR(func,code1));
   CGEN_e520888267(self, func);
  }
  else {
   CGEN_e756677294(self, func);
   if (ATTR(self,polling)) {
    ndefer_self = self;
    ndefer_s = ((STR) &AM_POLL_NETWORK1);
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   CGEN_e494460490(self, ATTR(func,code1));
   CGEN_e520888267(self, func);
  }
 }
 is_iter_self2 = func;
 is_iter_self3 = ATTR(is_iter_self2,sig1);
 is_iter_self4 = ATTR(is_iter_self3,name1);
 L16 = (is_iter_self4.str==((STR) NULL));
 L171_=!(L16); 
 if (L171_) {
  L18 = is_iter_self4.str;
  L19 = STR_sizerINT(is_iter_self4.str);
  L201_=INTMINUS(L19,1); 
  L21 = L201_;
  L221_=ARR((STR)L18,L21); 
  L23 = L221_;
  L241_=L23=='!'; 
  L15 = L241_;
 } else {
  L15 = FALSE;
 }
 ret_val4 = L15;
 ret_val3 = ret_val4;
 ret_val2 = ret_val3;
 if (ret_val2) {
  here_is_iter_sel = ATTR(self,code_c);
  here_is_iter_a = func;
  CODE_F508943357 = FMAPSI396914288(CODE_F508943357, ATTR(here_is_iter_a,sig1), here_is_iter_a);
  uses_iter_self = ATTR(self,code_c);
  uses_iter_s = ATTR(func,sig1);
  SATTR(uses_iter_self,iters,FSETSI671175003(ATTR(uses_iter_self,iters), uses_iter_s));
 }
 else {
  dispose_namespac = ATTR(self,mangler);
  dispose_namespac1 = ((OB) ATTR(func,sig1));
  SATTR(dispose_namespac,namespaces,FMAPdO1864084602(ATTR(dispose_namespac,namespaces), dispose_namespac1));
 }
 CODE_F65146505(ATTR(self,code_c));
 SATTR(self,indent,ind1);
}
