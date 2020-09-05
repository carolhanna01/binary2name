#include "sather.h"

/* Layouts */

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dLAYOUT_struct {
 OB_HEADER header;
 } *dLAYOUT;

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

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

typedef struct ARRAYAM_CALL_ARG_struct {/* layout for ARRAY{AM_CALL_ARG} */
 OB_HEADER header;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *ARRAYAM_CALL_ARG;

typedef struct ARRAYA44788561_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA44788561_frame;

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

typedef struct CONFIG_ROUT_struct {/* layout for CONFIG_ROUT */
 OB_HEADER header;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *near_depends_on;
 struct ARRAYSTR_struct *break1;
 struct ARRAYSTR_struct *declare;
 struct ARRAYSTR_struct *exec;
 struct ARRAYSTR_struct *f_break;
 struct ARRAYSTR_struct *f_declare;
 struct ARRAYSTR_struct *f_exec;
 struct ARRAYSTR_struct *f_init;
 struct ARRAYSTR_struct *f_iter;
 struct ARRAYSTR_struct *f_temp;
 struct ARRAYSTR_struct *f_var;
 struct ARRAYSTR_struct *init;
 struct ARRAYSTR_struct *iter;
 struct ARRAYSTR_struct *raises;
 struct ARRAYSTR_struct *reads;
 struct ARRAYSTR_struct *temp1;
 struct ARRAYSTR_struct *var;
 struct ARRAYSTR_struct *writes;
 STR name1;
 BOOL arith;
 BOOL block1;
 BOOL does_export;
 BOOL does_import;
 BOOL fragile;
 BOOL is_near;
 BOOL no_post;
 BOOL no_pre;
 BOOL raises_any;
 BOOL reads_any;
 BOOL use_index;
 BOOL volatile1;
 BOOL writes_any;
 } *CONFIG_ROUT;

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

typedef struct OUT_MODE_struct {/* layout for OUT_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT_MODE;

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

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

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

typedef struct AM_BND260301329_struct {/* layout for AM_BND_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *br;
 struct SFILE_ID_struct source1;
 struct TP_ROUT_struct *br_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND260301329;

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

typedef struct AM_IF_EXPR_struct {/* layout for AM_IF_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *if_false;
 struct dAM_EXPR_struct *if_true;
 struct dAM_EXPR_struct *test1;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IF_EXPR;

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

typedef struct AM_ITE235385604_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ic,I_u_Is; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE235385604_frame;

typedef struct AM_ITE1159913222_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE1159913222_frame;

typedef struct AM_ITE756865079_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE756865079_frame;

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

typedef struct AM_LOOP_STMT_struct {/* layout for AM_LOOP_STMT */
 OB_HEADER header;
 struct dAM_struct *first_while_move;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *init;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *loop_index_var;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTE1761212196_struct *hoisted;
 struct FLISTSTR_struct *ar_max_expr;
 INT breaks;
 struct SFILE_ID_struct source1;
 STR loop_index;
 BOOL has_init_stmt;
 BOOL has_yield;
 BOOL no_begin_loop;
 } *AM_LOOP_STMT;

typedef struct AM_NEW_EXPR_struct {/* layout for AM_NEW_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *asz;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_NEW_EXPR;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA537529312_frame_struct {
 INT state;
 ARRAYAM_CALL_ARG self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA537529312_frame;

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

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

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

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

typedef struct FSETAM1542678947_struct {/* layout for FSET{AM_SHARED_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_SHARED_EXPR_struct *arr_part[1];
 } *FSETAM1542678947;

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

extern IDENT IDENT_461800478;
extern INOUT_MODE MODES_inout_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUI1325635093;
extern TP_CLASS TP_BUI1813155122;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_str;
extern TP_CLASS TP_BUILTIN_sys;

/* Function declarations */


extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1999456142)(dTP, dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST BOOL (**dTP_is244083069)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is_crBOOL)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CALL_ARG AM_ITE235385604(AM_ITE235385604_frame);
AM_CALL_ARG ARRAYA44788561(ARRAYA44788561_frame);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_OB_DEF CGEN_a1221397173(CGEN, dTP);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AM_SHARED_EXPR GLOBAL499677090(GLOBAL_TBL, IDENT, dTP);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYSTR CGEN_e1792349707(CGEN, AM_ITE1809135850);
ARRAYSTR CGEN_e1933193154(CGEN, dAM_CALL_EXPR);
ARRAYSTR CGEN_e2096447517(CGEN, AM_ITE1809135850);
ARRAYSTR CGEN_e862722519(CGEN, dAM_CALL_EXPR);
BOOL CGEN_n1296275061(CGEN, dAM_CALL_EXPR);
BOOL CHAR_i1942379470(CHAR);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FSETAM1542678947 FSETAM381445024(FSETAM1542678947, AM_SHARED_EXPR);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT ARRAYA537529312(ARRAYA537529312_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_lengthrINT(STR);
INT STR_sizerINT(STR);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_a1155118157(CGEN, dTP);
STR CGEN_a59983474(CGEN, dTP, STR);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_e124364216(CGEN, AM_BOOL_CONST);
STR CGEN_e134814882(CGEN, AM_ITE1809135850);
STR CGEN_e1606764211(CGEN, STR, dAM_CALL_EXPR, SIG, ARRAYSTR, STR, STR, STR);
STR CGEN_e1633118543(CGEN, AM_STR_CONST);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e1793966983(CGEN, AM_ITE1809135850);
STR CGEN_e1835933683(CGEN, SIG, dAM_EXPR, ARRAYSTR);
STR CGEN_e2038430067(CGEN, AM_IF_EXPR);
STR CGEN_e231282482(CGEN, AM_NEW_EXPR);
STR CGEN_e414796278(CGEN, AM_CALL_ARG, ARG, BOOL, BOOL);
STR CGEN_e453436338(CGEN, AM_ROU1916046290);
STR CGEN_e604348447(CGEN, AM_SHARED_EXPR);
STR CGEN_eolrSTR(CGEN);
STR CGEN_i87475608(CGEN);
STR CGEN_s51495157(CGEN, dTP);
STR CGEN_v205291382(CGEN, dTP, STR, STR);
STR FMAPST1518383807(FMAPSTRSTR, STR);
STR MANGLE119219986(MANGLE, OB, OB);
STR MANGLE1995580116(MANGLE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
dLAYOUT LAYOUT79932797(LAYOUT_TBL, dTP);
void AM_ITE756865079(AM_ITE756865079_frame);
void CGEN_b2123071570(CGEN, AM_SHARED_EXPR);
void CGEN_c170352684(CGEN, AM_CALL_ARG, ARG, STR);
void CGEN_c657721823(CGEN, AM_CALL_ARG, ARG, STR);
void CGEN_c922285894(CGEN, dAM_CALL_EXPR, ARRAYSTR);
void CGEN_d695831906(CGEN);
void CGEN_e494460490(CGEN, dAM_STMT);
void CGEN_e844901491(CGEN, AM_ITE1809135850);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void CODE_F65146505(CODE_FILE);
void MANGLE1827205184(MANGLE, OB, STR, OB);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CGEN_e1792349707(CGEN self, AM_ITE1809135850 arce) {
 ARRAYSTR ret_val;
 ARRAYAM_CALL_ARG am;
 ARRAYAM_CALL_ARG L11;
 INT i = INT_zero;
 ARRAYSTR arg_list;
 ARRAYAM_CALL_ARG L21;
 AM_ITE1809135850 L31;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i1 = INT_zero;
 ARRAYAM_CALL_ARG create_self;
 INT create_n = INT_zero;
 ARRAYAM_CALL_ARG ret_val1;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val2;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val3;
 INT L81_;
 ARRAYAM_CALL_ARG L9;
 INT L10;
 OB L12;
 INT L13;
 INT L141_br;
 INT rL141_;
 INT L15;
 AM_CALL_ARG L161_;
 AM_CALL_ARG L17;
 BOOL L19;
 BOOL L201_;
 BOOL L22;
 ARRAYBOOL L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 INT L301_;
 INT L331_br;
 AM_CALL_ARG aL331_;
 INT L341_br;
 AM_CALL_ARG L35;
 AM_CALL_ARG L36;
 INT L371_;
 AM_CALL_ARG L381_;
 AM_CALL_ARG L39;
 dAM_EXPR L40;
 STR L421_;
 STR L43;
 INT L461_;
 INT L47;
 INT L481_;
 INT L451_,L451_m;
 INT L49;
 ARRAYBOOL L50;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L56;
 ARRAYARG L57;
 INT L581_;
 INT L59;
 ARG L601_;
 ARG L62;
 dTP L63;
 AM_CALL_ARG L641_;
 AM_CALL_ARG L65;
 AM_CALL_ARG L661_;
 AM_CALL_ARG L67;
 dAM_EXPR L68;
 dTP L69;
 BOOL L70;
 BOOL L721_;
 ARRAYARG L73;
 INT L741_;
 INT L75;
 ARG L761_;
 ARG L77;
 AM_CALL_ARG L781_;
 AM_CALL_ARG L79;
 dAM_EXPR L80;
 STR L821_;
 STR L83;
 ARRAYSTR L85;
 STR L861_;
 STR L87;
 if ((ATTR(ATTR(arce,fun),hot)==((ARRAYBOOL) NULL))) {
  ret_val = ATTR(arce,arg_list);
  return ret_val;
 }
 create_self = ((ARRAYAM_CALL_ARG) NULL);
 L81_=ASIZE((AM_ITE1809135850)arce); 
 create_n = L81_;
 L10 = create_n;
 L13=(sizeof(struct ARRAYAM_CALL_ARG_struct)-sizeof(AM_CALL_ARG))+(L10)*sizeof(AM_CALL_ARG);
 L12=ZALLOC_BIG(L13);
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=ARRAYAM_CALL_ARG_tag;
#ifdef DESTROY_CHK

   ((OB)L12)->header.destroyed=0;
#endif

 L9 = ((ARRAYAM_CALL_ARG) L12);
 L9->asize = L10;
 ret_val1 = L9;
 am = ret_val1;
 {
  BOOL f_L141_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L11 = am;
  L141_br=L11==NULL?0:ASIZE((ARRAYAM_CALL_ARG)L11); 
  while (1) {
   if(L61>=L141_br)  goto after_loop; 
   rL141_=L61; 
   i = rL141_;
   L161_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i); 
   L17 = L161_;
   SARR((ARRAYAM_CALL_ARG)am,i,(AM_CALL_ARG)L17); 
   ;
   L201_=(0)<(i); 
   if (L201_) {
    if ((ATTR(ATTR(arce,fun),hot)==((ARRAYBOOL) NULL))) {
     L22 = TRUE;
    } else {
     L23 = ATTR(ATTR(arce,fun),hot);
     L241_=INTMINUS(i,1); 
     L25 = L241_;
     L261_=ARR((ARRAYBOOL)L23,L25); 
     L27 = L261_;
     L281_=!(L27); 
     L22 = L281_;
    }
    L19 = L22;
   } else {
    L19 = FALSE;
   }
   if (L19) {
    SARR((AM_ITE1809135850)arce,i,(AM_CALL_ARG)((AM_CALL_ARG) NULL)); 
    ;
   }
   L301_=INTPLUS(L61,1); 
   L61 = L301_;
  }
 }
 after_loop: ;
 SARR((AM_ITE1809135850)arce,0,(AM_CALL_ARG)((AM_CALL_ARG) NULL)); 
 ;
 arg_list = CGEN_e862722519(self, ((dAM_CALL_EXPR) arce));
 {
  BOOL f_L331_ = TRUE;
  BOOL f_L341_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L21 = am;
  L31 = arce;
  L331_br=L21==NULL?0:ASIZE((ARRAYAM_CALL_ARG)L21); 
  L341_br=L31==NULL?0:ASIZE((AM_ITE1809135850)L31); 
  while (1) {
   if(L71>=L341_br)  goto after_loop1; 
   if(L71>=L331_br)  goto after_loop1; 
   aL331_=ARR((ARRAYAM_CALL_ARG)L21,L71); 
   L36 = aL331_;
   SARR((AM_ITE1809135850)L31,L71,(AM_CALL_ARG)L36); 
   ;
   L371_=INTPLUS(L71,1); 
   L71 = L371_;
  }
 }
 after_loop1: ;
 cast_self = self;
 cast_dest_tp = ATTR(ATTR(arce,fun),tp);
 L381_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,0); 
 L39=L381_;
 L40 = ATTR(L39,expr);
 cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L40)])(L40);
 L421_=(STR)ARR((ARRAYSTR)arg_list,0); 
 cast_expr = L421_;
 cast_only_boxing = SIG_is418491101(ATTR(arce,fun));
 ret_val2 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
 L43 = ret_val2;
 SARR((ARRAYSTR)arg_list,0,(STR)L43); 
 ;
 {
  BOOL f_L451_ = TRUE;
  L41 = 1;
  L461_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
  L47 = L461_;
  L481_=INTMINUS(L47,1); 
  L51 = L481_;
  L451_=L41-1;L451_m=L51; 
  while (1) {
   if(L451_++>=L451_m)  goto after_loop2; 
   i1 = L451_;
   L50 = ATTR(ATTR(arce,fun),hot);
   L521_=INTMINUS(i1,1); 
   L53 = L521_;
   L541_=ARR((ARRAYBOOL)L50,L53); 
   if (L541_) {
    L57 = ATTR(ATTR(arce,fun),args);
    L581_=INTMINUS(i1,1); 
    L59 = L581_;
    L601_=(ARG)ARR((ARRAYARG)L57,L59); 
    L62=L601_;
    L63 = ATTR(L62,tp);
    if ((*dTP_is1334578382[TAG(L63)])(L63)) {
     L641_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L65=L641_;
     L56 = SYSOBEQ(((OB) ATTR(L65,mode1)),((OB) MODES_out_mode));
    } else {
     L56 = FALSE;
    }
    if (L56) {
     L661_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L67=L661_;
     L68 = ATTR(L67,expr);
     L69 = (*dAM_EXPR_tprdTP[TAG(L68)])(L68);
     L55 = (*dTP_is1811059018[TAG(L69)])(L69);
    } else {
     L55 = FALSE;
    }
    L70 = L55;
    L721_=!(L70); 
    if (L721_) {
     cast_self1 = self;
     L73 = ATTR(ATTR(arce,fun),args);
     L741_=INTMINUS(i1,1); 
     L75 = L741_;
     L761_=(ARG)ARR((ARRAYARG)L73,L75); 
     L77=L761_;
     cast_dest_tp1 = ATTR(L77,tp);
     L781_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L79=L781_;
     L80 = ATTR(L79,expr);
     cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L80)])(L80);
     L821_=(STR)ARR((ARRAYSTR)arg_list,i1); 
     cast_expr1 = L821_;
     cast_only_boxing1 = SIG_is418491101(ATTR(arce,fun));
     ret_val3 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
     L83 = ret_val3;
     SARR((ARRAYSTR)arg_list,i1,(STR)L83); 
     ;
    }
    L85 = ATTR(arce,arg_list);
    L861_=(STR)ARR((ARRAYSTR)arg_list,i1); 
    L87 = L861_;
    SARR((ARRAYSTR)L85,i1,(STR)L87); 
    ;
   }
  }
 }
 after_loop2: ;
 ret_val = ATTR(arce,arg_list);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

ARRAYSTR CGEN_e2096447517(CGEN self, AM_ITE1809135850 arce) {
 ARRAYSTR ret_val;
 ARRAYAM_CALL_ARG am;
 ARRAYAM_CALL_ARG L11;
 INT i = INT_zero;
 ARRAYSTR arg_list;
 ARRAYAM_CALL_ARG L21;
 AM_ITE1809135850 L31;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT i1 = INT_zero;
 ARRAYAM_CALL_ARG create_self;
 INT create_n = INT_zero;
 ARRAYAM_CALL_ARG ret_val1;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val2;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val3;
 INT L81_;
 ARRAYAM_CALL_ARG L9;
 INT L10;
 OB L12;
 INT L13;
 INT L141_br;
 INT rL141_;
 INT L15;
 AM_CALL_ARG L161_;
 AM_CALL_ARG L17;
 BOOL L19;
 BOOL L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 ARRAYBOOL L25;
 INT L261_;
 INT L27;
 BOOL L281_;
 INT L301_;
 INT L321_br;
 AM_CALL_ARG aL321_;
 INT L331_br;
 AM_CALL_ARG L34;
 AM_CALL_ARG L35;
 INT L361_;
 AM_CALL_ARG L371_;
 AM_CALL_ARG L38;
 dAM_EXPR L39;
 STR L401_;
 STR L42;
 INT L451_;
 INT L46;
 INT L471_;
 INT L441_,L441_m;
 INT L48;
 BOOL L49;
 ARRAYBOOL L50;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 BOOL L57;
 BOOL L58;
 ARRAYARG L59;
 INT L601_;
 INT L62;
 ARG L631_;
 ARG L64;
 dTP L65;
 AM_CALL_ARG L661_;
 AM_CALL_ARG L67;
 AM_CALL_ARG L681_;
 AM_CALL_ARG L69;
 dAM_EXPR L70;
 dTP L72;
 BOOL L73;
 BOOL L741_;
 ARRAYARG L75;
 INT L761_;
 INT L77;
 ARG L781_;
 ARG L79;
 AM_CALL_ARG L801_;
 AM_CALL_ARG L82;
 dAM_EXPR L83;
 STR L841_;
 STR L85;
 create_self = ((ARRAYAM_CALL_ARG) NULL);
 L81_=ASIZE((AM_ITE1809135850)arce); 
 create_n = L81_;
 L10 = create_n;
 L13=(sizeof(struct ARRAYAM_CALL_ARG_struct)-sizeof(AM_CALL_ARG))+(L10)*sizeof(AM_CALL_ARG);
 L12=ZALLOC_BIG(L13);
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=ARRAYAM_CALL_ARG_tag;
#ifdef DESTROY_CHK

   ((OB)L12)->header.destroyed=0;
#endif

 L9 = ((ARRAYAM_CALL_ARG) L12);
 L9->asize = L10;
 ret_val1 = L9;
 am = ret_val1;
 {
  BOOL f_L141_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L11 = am;
  L141_br=L11==NULL?0:ASIZE((ARRAYAM_CALL_ARG)L11); 
  while (1) {
   if(L61>=L141_br)  goto after_loop; 
   rL141_=L61; 
   i = rL141_;
   L161_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i); 
   L17 = L161_;
   SARR((ARRAYAM_CALL_ARG)am,i,(AM_CALL_ARG)L17); 
   ;
   L221_=(0)<(i); 
   if (L221_) {
    L23 = (ATTR(ATTR(arce,fun),hot)==((ARRAYBOOL) NULL));
    L241_=!(L23); 
    L20 = L241_;
   } else {
    L20 = FALSE;
   }
   if (L20) {
    L25 = ATTR(ATTR(arce,fun),hot);
    L261_=INTMINUS(i,1); 
    L27 = L261_;
    L281_=ARR((ARRAYBOOL)L25,L27); 
    L19 = L281_;
   } else {
    L19 = FALSE;
   }
   if (L19) {
    SARR((AM_ITE1809135850)arce,i,(AM_CALL_ARG)((AM_CALL_ARG) NULL)); 
    ;
   }
   L301_=INTPLUS(L61,1); 
   L61 = L301_;
  }
 }
 after_loop: ;
 arg_list = CGEN_e862722519(self, ((dAM_CALL_EXPR) arce));
 {
  BOOL f_L321_ = TRUE;
  BOOL f_L331_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L21 = am;
  L31 = arce;
  L321_br=L21==NULL?0:ASIZE((ARRAYAM_CALL_ARG)L21); 
  L331_br=L31==NULL?0:ASIZE((AM_ITE1809135850)L31); 
  while (1) {
   if(L71>=L331_br)  goto after_loop1; 
   if(L71>=L321_br)  goto after_loop1; 
   aL321_=ARR((ARRAYAM_CALL_ARG)L21,L71); 
   L35 = aL321_;
   SARR((AM_ITE1809135850)L31,L71,(AM_CALL_ARG)L35); 
   ;
   L361_=INTPLUS(L71,1); 
   L71 = L361_;
  }
 }
 after_loop1: ;
 cast_self = self;
 cast_dest_tp = ATTR(ATTR(arce,fun),tp);
 L371_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,0); 
 L38=L371_;
 L39 = ATTR(L38,expr);
 cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L39)])(L39);
 L401_=(STR)ARR((ARRAYSTR)arg_list,0); 
 cast_expr = L401_;
 cast_only_boxing = FALSE;
 ret_val2 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
 L42 = ret_val2;
 SARR((ARRAYSTR)arg_list,0,(STR)L42); 
 ;
 {
  BOOL f_L441_ = TRUE;
  L41 = 1;
  L451_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
  L46 = L451_;
  L471_=INTMINUS(L46,1); 
  L51 = L471_;
  L441_=L41-1;L441_m=L51; 
  while (1) {
   if(L441_++>=L441_m)  goto after_loop2; 
   i1 = L441_;
   if ((ATTR(ATTR(arce,fun),hot)==((ARRAYBOOL) NULL))) {
    L49 = TRUE;
   } else {
    L50 = ATTR(ATTR(arce,fun),hot);
    L521_=INTMINUS(i1,1); 
    L53 = L521_;
    L541_=ARR((ARRAYBOOL)L50,L53); 
    L55 = L541_;
    L561_=!(L55); 
    L49 = L561_;
   }
   if (L49) {
    L59 = ATTR(ATTR(arce,fun),args);
    L601_=INTMINUS(i1,1); 
    L62 = L601_;
    L631_=(ARG)ARR((ARRAYARG)L59,L62); 
    L64=L631_;
    L65 = ATTR(L64,tp);
    if ((*dTP_is1334578382[TAG(L65)])(L65)) {
     L661_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L67=L661_;
     L58 = SYSOBEQ(((OB) ATTR(L67,mode1)),((OB) MODES_out_mode));
    } else {
     L58 = FALSE;
    }
    if (L58) {
     L681_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L69=L681_;
     L70 = ATTR(L69,expr);
     L72 = (*dAM_EXPR_tprdTP[TAG(L70)])(L70);
     L57 = (*dTP_is1811059018[TAG(L72)])(L72);
    } else {
     L57 = FALSE;
    }
    L73 = L57;
    L741_=!(L73); 
    if (L741_) {
     cast_self1 = self;
     L75 = ATTR(ATTR(arce,fun),args);
     L761_=INTMINUS(i1,1); 
     L77 = L761_;
     L781_=(ARG)ARR((ARRAYARG)L75,L77); 
     L79=L781_;
     cast_dest_tp1 = ATTR(L79,tp);
     L801_=(AM_CALL_ARG)ARR((AM_ITE1809135850)arce,i1); 
     L82=L801_;
     L83 = ATTR(L82,expr);
     cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L83)])(L83);
     L841_=(STR)ARR((ARRAYSTR)arg_list,i1); 
     cast_expr1 = L841_;
     cast_only_boxing1 = SIG_is418491101(ATTR(arce,fun));
     ret_val3 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
     L85 = ret_val3;
     SARR((ARRAYSTR)arg_list,i1,(STR)L85); 
     ;
    }
   }
  }
 }
 after_loop2: ;
 SATTR(arce,arg_list,arg_list);
 ret_val = arg_list;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_a59983474(CGEN self, dTP t, STR n) {
 STR ret_val;
 dTP t2;
 BOOL leaf1 = BOOL_zero;
 STR call_string = ((STR) NULL);
 STR xtra = ((STR) NULL);
 STR r;
 STR sz2;
 STR sz = ((STR) NULL);
 STR s2;
 CODE_FILE uses_tp_self;
 dTP uses_tp_t;
 CODE_FILE uses_tp_self1;
 dTP uses_tp_t1;
 CGEN sizeof_boxed_sel;
 dTP sizeof_boxed_tp;
 STR ret_val1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val9;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val10;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val11;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val12;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val13;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val14;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val15;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val16;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val17;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val18;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val19;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val20;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val21;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val22;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self19;
 STR plus_sarg19;
 STR ret_val23;
 STR plus_self20;
 STR plus_sarg20;
 STR ret_val24;
 STR plus_self21;
 STR plus_sarg21;
 STR ret_val25;
 STR plus_self22;
 STR plus_sarg22;
 STR ret_val26;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self23;
 STR plus_sarg23;
 STR ret_val27;
 STR plus_self24;
 STR plus_sarg24;
 STR ret_val28;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val29;
 STR res;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val30;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self25;
 STR plus_sarg25;
 STR ret_val31;
 STR plus_self26;
 STR plus_sarg26;
 STR ret_val32;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self27;
 STR plus_sarg27;
 STR ret_val33;
 STR plus_self28;
 STR plus_sarg28;
 STR ret_val34;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN defer_self;
 STR defer_s;
 STR plus_self29;
 STR plus_sarg29;
 STR ret_val35;
 STR plus_self30;
 STR plus_sarg30;
 STR ret_val36;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN defer_self1;
 STR defer_s1;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val37;
 STR plus_self31;
 STR plus_sarg31;
 STR ret_val38;
 STR plus_self32;
 STR plus_sarg32;
 STR ret_val39;
 STR plus_self33;
 STR plus_sarg33;
 STR ret_val40;
 STR plus_self34;
 STR plus_sarg34;
 STR ret_val41;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val42;
 STR plus_self35;
 STR plus_sarg35;
 STR ret_val43;
 STR plus_self36;
 STR plus_sarg36;
 STR ret_val44;
 STR plus_self37;
 STR plus_sarg37;
 STR ret_val45;
 STR plus_self38;
 STR plus_sarg38;
 STR ret_val46;
 AM_OB_DEF L1;
 dTP L2;
 dTP L3;
 extern STR sizeofstruct;
 extern STR S_boxed_struct;
 extern STR ZALLOC_LEAF_BIG;
 extern STR S12;
 extern STR ZALLOC_BIG1;
 extern STR name1;
 extern STR sizeof1;
 extern STR name26;
 extern STR name101;
 extern STR name123;
 extern STR name124;
 extern STR name125;
 extern STR name16;
 extern STR name76;
 BOOL L4;
 BOOL L51_;
 extern STR if3;
 extern STR NULLFA1853100247;
 extern STR memset1;
 extern STR S0;
 extern STR name76;
 extern STR OB1;
 extern STR headertag;
 extern STR S_tag;
 extern STR name16;
 extern STR OB1;
 extern STR headeridob_count;
 extern STR ifdefDESTROY_CHK;
 extern STR OB2;
 extern STR headerdestroyed0;
 extern STR endif2;
 dTP L6;
 extern STR name75;
 extern STR S_boxed;
 extern STR name26;
 extern STR name75;
 extern STR name23;
 extern STR name26;
 L1=CGEN_a1221397173(self, t);
 t2 = ATTR(L1,arr);
 uses_tp_self = ATTR(self,code_c);
 uses_tp_t = t;
 CODE_F525364269(uses_tp_self, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t));
 uses_tp_self1 = ATTR(self,code_c);
 uses_tp_t1 = t2;
 CODE_F525364269(uses_tp_self1, LAYOUT79932797(((LAYOUT_TBL) NULL), uses_tp_t1));
 L2 = t;
 leaf1 = (*dTP_is244083069[TAG(L2)])(L2);
 r = CGEN_d1425664957(self, ((dTP) TP_BUI1325635093));
 sz2 = CGEN_d1425664957(self, ((dTP) TP_BUILTIN_int));
 L3 = t;
 if ((*dTP_is1334578382[TAG(L3)])(L3)) {
  sizeof_boxed_sel = self;
  sizeof_boxed_tp = t;
  plus_self = ((STR) &sizeofstruct);
  mang_self = sizeof_boxed_sel;
  mang_ob = ((OB) sizeof_boxed_tp);
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  plus_sarg = ret_val2;
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = ((STR) &S_boxed_struct);
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  ret_val1 = ret_val4;
  sz = ret_val1;
 }
 else {
  sz = CGEN_s51495157(self, t);
 }
 if (leaf1) {
  call_string = ((STR) &ZALLOC_LEAF_BIG);
  xtra = ((STR) &S12);
 }
 else {
  call_string = ((STR) &ZALLOC_BIG1);
  xtra = ((STR) &name1);
 }
 plus_self2 = ((STR) &sizeof1);
 mang_self1 = self;
 mang_ob1 = ((OB) t2);
 ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_sarg2 = ret_val5;
 ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
 plus_self3 = ret_val6;
 plus_sarg3 = ((STR) &name26);
 ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
 s2 = ret_val7;
 ndefer_self = self;
 plus_self4 = sz2;
 plus_sarg4 = ((STR) &name101);
 ret_val8 = STR_ap2004550586(plus_self4, plus_sarg4);
 plus_self5 = ret_val8;
 plus_sarg5 = sz;
 ret_val9 = STR_ap2004550586(plus_self5, plus_sarg5);
 plus_self6 = ret_val9;
 plus_sarg6 = xtra;
 ret_val10 = STR_ap2004550586(plus_self6, plus_sarg6);
 plus_self7 = ret_val10;
 plus_sarg7 = ((STR) &name123);
 ret_val11 = STR_ap2004550586(plus_self7, plus_sarg7);
 plus_self8 = ret_val11;
 plus_sarg8 = s2;
 ret_val12 = STR_ap2004550586(plus_self8, plus_sarg8);
 plus_self9 = ret_val12;
 plus_sarg9 = ((STR) &name124);
 ret_val13 = STR_ap2004550586(plus_self9, plus_sarg9);
 plus_self10 = ret_val13;
 plus_sarg10 = n;
 ret_val14 = STR_ap2004550586(plus_self10, plus_sarg10);
 plus_self11 = ret_val14;
 plus_sarg11 = ((STR) &name125);
 ret_val15 = STR_ap2004550586(plus_self11, plus_sarg11);
 plus_self12 = ret_val15;
 plus_sarg12 = s2;
 ret_val16 = STR_ap2004550586(plus_self12, plus_sarg12);
 plus_self13 = ret_val16;
 plus_sarg13 = ((STR) &name16);
 ret_val17 = STR_ap2004550586(plus_self13, plus_sarg13);
 ndefer_s = ret_val17;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 ndefer_self1 = self;
 plus_self14 = r;
 plus_sarg14 = call_string;
 ret_val18 = STR_ap2004550586(plus_self14, plus_sarg14);
 plus_self15 = ret_val18;
 plus_sarg15 = sz2;
 ret_val19 = STR_ap2004550586(plus_self15, plus_sarg15);
 plus_self16 = ret_val19;
 plus_sarg16 = ((STR) &name76);
 ret_val20 = STR_ap2004550586(plus_self16, plus_sarg16);
 ndefer_s1 = ret_val20;
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 L4 = ATTR(self,siva);
 L51_=!(L4); 
 if (L51_) {
  ndefer_self2 = self;
  plus_self17 = ((STR) &if3);
  plus_sarg17 = r;
  ret_val21 = STR_ap2004550586(plus_self17, plus_sarg17);
  plus_self18 = ret_val21;
  plus_sarg18 = ((STR) &NULLFA1853100247);
  ret_val22 = STR_ap2004550586(plus_self18, plus_sarg18);
  ndefer_s2 = ret_val22;
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  if (leaf1) {
   ndefer_self3 = self;
   plus_self19 = ((STR) &memset1);
   plus_sarg19 = r;
   ret_val23 = STR_ap2004550586(plus_self19, plus_sarg19);
   plus_self20 = ret_val23;
   plus_sarg20 = ((STR) &S0);
   ret_val24 = STR_ap2004550586(plus_self20, plus_sarg20);
   plus_self21 = ret_val24;
   plus_sarg21 = sz2;
   ret_val25 = STR_ap2004550586(plus_self21, plus_sarg21);
   plus_self22 = ret_val25;
   plus_sarg22 = ((STR) &name76);
   ret_val26 = STR_ap2004550586(plus_self22, plus_sarg22);
   ndefer_s3 = ret_val26;
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  }
 }
 ndefer_self4 = self;
 plus_self23 = ((STR) &OB1);
 plus_sarg23 = r;
 ret_val27 = STR_ap2004550586(plus_self23, plus_sarg23);
 plus_self24 = ret_val27;
 plus_sarg24 = ((STR) &headertag);
 ret_val28 = STR_ap2004550586(plus_self24, plus_sarg24);
 plus_self25 = ret_val28;
 tag_for_self = self;
 tag_for_tp = t;
 SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
 mang_self2 = tag_for_self;
 mang_ob2 = ((OB) tag_for_tp);
 ret_val30 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 res = STR_ap2004550586(ret_val30, ((STR) &S_tag));
 forbid_self = tag_for_self;
 forbid_s = res;
 forbid_self1 = ATTR(forbid_self,mangler);
 forbid_s1 = forbid_s;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val29 = res;
 plus_sarg25 = ret_val29;
 ret_val31 = STR_ap2004550586(plus_self25, plus_sarg25);
 plus_self26 = ret_val31;
 plus_sarg26 = ((STR) &name16);
 ret_val32 = STR_ap2004550586(plus_self26, plus_sarg26);
 ndefer_s4 = ret_val32;
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 if (ATTR(self,deterministic1)) {
  ndefer_self5 = self;
  plus_self27 = ((STR) &OB1);
  plus_sarg27 = r;
  ret_val33 = STR_ap2004550586(plus_self27, plus_sarg27);
  plus_self28 = ret_val33;
  plus_sarg28 = ((STR) &headeridob_count);
  ret_val34 = STR_ap2004550586(plus_self28, plus_sarg28);
  ndefer_s5 = ret_val34;
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 }
 defer_self = self;
 defer_s = ((STR) &ifdefDESTROY_CHK);
 SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
 ndefer_self6 = self;
 plus_self29 = ((STR) &OB2);
 plus_sarg29 = r;
 ret_val35 = STR_ap2004550586(plus_self29, plus_sarg29);
 plus_self30 = ret_val35;
 plus_sarg30 = ((STR) &headerdestroyed0);
 ret_val36 = STR_ap2004550586(plus_self30, plus_sarg30);
 ndefer_s6 = ret_val36;
 CGEN_d695831906(ndefer_self6);
 SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 defer_self1 = self;
 defer_s1 = ((STR) &endif2);
 SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
 L6 = t;
 if ((*dTP_is1334578382[TAG(L6)])(L6)) {
  plus_self31 = ((STR) &name75);
  mang_self3 = self;
  mang_ob3 = ((OB) t);
  ret_val37 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
  plus_sarg31 = ret_val37;
  ret_val38 = STR_ap2004550586(plus_self31, plus_sarg31);
  plus_self32 = ret_val38;
  plus_sarg32 = ((STR) &S_boxed);
  ret_val39 = STR_ap2004550586(plus_self32, plus_sarg32);
  plus_self33 = ret_val39;
  plus_sarg33 = r;
  ret_val40 = STR_ap2004550586(plus_self33, plus_sarg33);
  plus_self34 = ret_val40;
  plus_sarg34 = ((STR) &name26);
  ret_val41 = STR_ap2004550586(plus_self34, plus_sarg34);
  ret_val = ret_val41;
  return ret_val;
 }
 else {
  plus_self35 = ((STR) &name75);
  mang_self4 = self;
  mang_ob4 = ((OB) t);
  ret_val42 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
  plus_sarg35 = ret_val42;
  ret_val43 = STR_ap2004550586(plus_self35, plus_sarg35);
  plus_self36 = ret_val43;
  plus_sarg36 = ((STR) &name23);
  ret_val44 = STR_ap2004550586(plus_self36, plus_sarg36);
  plus_self37 = ret_val44;
  plus_sarg37 = r;
  ret_val45 = STR_ap2004550586(plus_self37, plus_sarg37);
  plus_self38 = ret_val45;
  plus_sarg38 = ((STR) &name26);
  ret_val46 = STR_ap2004550586(plus_self38, plus_sarg38);
  ret_val = ret_val46;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e124364216(CGEN self, AM_BOOL_CONST arg) {
 STR ret_val;
 extern STR TRUE1;
 extern STR FALSE1;
 if (ATTR(arg,val1)) {
  ret_val = ((STR) &TRUE1);
  return ret_val;
 }
 else {
  ret_val = ((STR) &FALSE1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e134814882(CGEN self, AM_ITE1809135850 aice) {
 STR ret_val;
 STR call_str = ((STR) NULL);
 STR s1 = ((STR) NULL);
 STR res = ((STR) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 STR arg_str;
 STR pro;
 STR tag1;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 AM_ITE1809135850 tp_self;
 dTP ret_val1;
 AM_ITE1809135850 tp_self1;
 dTP ret_val2;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val4;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val5;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val6;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 SIG is_builtin_iter_;
 BOOL ret_val7 = BOOL_zero;
 SIG is_iter_self;
 BOOL ret_val8 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val9 = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val12;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN move_out_self;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val13;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN in_self1;
 CGEN move_out_self1;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns1;
 STR ret_val14;
 INT str_self = INT_zero;
 STR ret_val15;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val16;
 FSTR str_self1;
 STR ret_val17;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN mang_self2;
 OB mang_ob2;
 OB mang_ns2;
 STR ret_val18;
 INT str_self2 = INT_zero;
 STR ret_val19;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val20;
 FSTR str_self3;
 STR ret_val21;
 CGEN cast_arg_self;
 dTP cast_arg_dest_tp;
 dTP cast_arg_src_tp;
 STR cast_arg_expr;
 BOOL cast_arg_only_bo = BOOL_zero;
 dMODE cast_arg_mode;
 STR ret_val22;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val23;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val24;
 CGEN ndefer_self7;
 STR ndefer_s7;
 PROG psather_self;
 BOOL ret_val25 = BOOL_zero;
 SIG is_iter_self2;
 BOOL ret_val26 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val27 = BOOL_zero;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN in_self2;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val28;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val29;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val30;
 CGEN runtime_error_se;
 STR runtime_error_s;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val32;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN move_out_self2;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN ndefer_self12;
 STR ndefer_s12;
 AM_ITE1809135850 tp_self2;
 dTP ret_val33;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val34;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val35;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val36;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val37;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val38;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val39;
 STR plus_self19;
 STR plus_sarg16;
 STR ret_val40;
 STR plus_self20;
 STR plus_sarg17;
 STR ret_val41;
 STR plus_self21;
 STR plus_sarg18;
 STR ret_val42;
 CGEN mang_self4;
 OB mang_ob4;
 OB mang_ns3;
 STR ret_val43;
 STR plus_self22;
 STR plus_sarg19;
 STR ret_val44;
 STR plus_self23;
 STR plus_sarg20;
 STR ret_val45;
 STR plus_self24;
 STR plus_sarg21;
 STR ret_val46;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val47;
 STR plus_self25;
 STR plus_sarg22;
 STR ret_val48;
 STR plus_self26;
 STR plus_sarg23;
 STR ret_val49;
 STR plus_self27;
 STR plus_sarg24;
 STR ret_val50;
 STR plus_self28;
 STR plus_sarg25;
 STR ret_val51;
 STR plus_self29;
 STR plus_sarg26;
 STR ret_val52;
 STR plus_self30;
 STR plus_sarg27;
 STR ret_val53;
 STR plus_self31;
 STR plus_sarg28;
 STR ret_val54;
 CGEN mang_self6;
 OB mang_ob6;
 OB mang_ns4;
 STR ret_val55;
 STR plus_self32;
 STR plus_sarg29;
 STR ret_val56;
 STR plus_self33;
 STR plus_sarg30;
 STR ret_val57;
 STR plus_self34;
 STR plus_sarg31;
 STR ret_val58;
 CGEN ndefer_self14;
 STR ndefer_s14;
 AM_ITE1809135850 tp_self3;
 dTP ret_val59;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val60;
 CGEN mang_self8;
 OB mang_ob8;
 OB mang_ns5;
 STR ret_val61;
 STR plus_self35;
 STR plus_sarg32;
 STR ret_val62;
 CGEN ndefer_self15;
 STR ndefer_s15;
 CGEN mang_self9;
 OB mang_ob9;
 STR ret_val63;
 CGEN mang_self10;
 OB mang_ob10;
 OB mang_ns6;
 STR ret_val64;
 STR plus_self36;
 STR plus_sarg33;
 STR ret_val65;
 CGEN ndefer_self16;
 STR ndefer_s16;
 PROG psather_self1;
 BOOL ret_val66 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val67 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val68 = BOOL_zero;
 CGEN ndefer_self17;
 STR ndefer_s17;
 CGEN mang_self11;
 OB mang_ob11;
 OB mang_ns7;
 STR ret_val69;
 CGEN ndefer_self18;
 STR ndefer_s18;
 CGEN in_self3;
 CGEN ndefer_self19;
 STR ndefer_s19;
 CGEN pop_exceptions_s;
 INT pop_exceptions_n = INT_zero;
 INT L31 = INT_zero;
 CGEN ndefer_self20;
 STR ndefer_s20;
 INT str_self4 = INT_zero;
 STR ret_val70;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val71;
 FSTR str_self5;
 STR ret_val72;
 CGEN ndefer_self21;
 STR ndefer_s21;
 CGEN ndefer_self22;
 STR ndefer_s22;
 CGEN move_out_self3;
 CGEN ndefer_self23;
 STR ndefer_s23;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR Holdsr2142202862;
 extern STR name78;
 BOOL L8;
 BOOL L91_;
 FSTR L10;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 FSTR L15;
 dSTR L16;
 extern STR name79;
 FSTR L17;
 dSTR L18;
 BOOL L19;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 STR L24;
 INT L25;
 INT L261_;
 INT L27;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 extern STR if3;
 STR L34;
 extern STR f_;
 extern STR name84;
 INT L35;
 INT L361_;
 extern STR f_;
 extern STR FALSE2;
 INT L37;
 INT L381_;
 extern STR name4;
 BOOL L39;
 BOOL L401_;
 extern STR if3;
 extern STR state0;
 INT L41;
 INT L421_;
 INT L43;
 INT L441_;
 extern STR name4;
 BOOL L45;
 BOOL L461_;
 INT L48;
 INT L491_;
 INT L50;
 INT L521_;
 INT L471_,L471_m;
 ARRAYBOOL L53;
 INT L541_;
 INT L55;
 BOOL L561_;
 AM_CALL_ARG L571_;
 AM_CALL_ARG L58;
 ARRAYARG L59;
 INT L601_;
 INT L61;
 ARG L621_;
 ARG L63;
 BOOL L64;
 BOOL L65;
 ARRAYARG L66;
 INT L671_;
 INT L68;
 ARG L691_;
 ARG L70;
 dTP L72;
 AM_CALL_ARG L731_;
 AM_CALL_ARG L74;
 AM_CALL_ARG L751_;
 AM_CALL_ARG L76;
 dAM_EXPR L77;
 dTP L78;
 extern STR arg3;
 BOOL L79;
 BOOL L801_;
 extern STR name36;
 extern STR name16;
 extern STR arg3;
 BOOL L81;
 BOOL L821_;
 extern STR name36;
 ARRAYARG L83;
 INT L841_;
 INT L85;
 ARG L861_;
 ARG L87;
 AM_CALL_ARG L881_;
 AM_CALL_ARG L89;
 dAM_EXPR L90;
 AM_CALL_ARG L921_;
 AM_CALL_ARG L93;
 extern STR name16;
 extern STR hotargument;
 extern STR name54;
 extern STR name79;
 extern STR name1;
 extern STR FF2;
 BOOL L94;
 BOOL L95;
 BOOL L96;
 BOOL L97;
 BOOL L981_;
 BOOL L99;
 BOOL L100;
 BOOL L1011_;
 STR L102;
 INT L103;
 INT L1041_;
 INT L105;
 CHAR L1061_;
 CHAR L107;
 BOOL L1081_;
 extern STR RESTOR712275783;
 dTP L109;
 BOOL L110;
 BOOL L111;
 BOOL L1121_;
 extern STR if3;
 AM_CALL_ARG L1131_;
 AM_CALL_ARG L114;
 STR L115;
 extern STR NULL3;
 INT L116;
 INT L1171_;
 extern STR Dispatchedcallto;
 extern STR onvoidselfin;
 extern STR FATAL1;
 extern STR name95;
 INT L118;
 INT L1191_;
 extern STR name4;
 extern STR COUNT_DISPATCH1;
 extern STR TAG2;
 extern STR F_TAG2;
 BOOL L120;
 BOOL L1211_;
 extern STR name113;
 extern STR name14;
 extern STR name24;
 AM_CALL_ARG L1221_;
 AM_CALL_ARG L123;
 extern STR iter3;
 extern STR name24;
 extern STR name76;
 extern STR name85;
 extern STR name14;
 extern STR name24;
 AM_CALL_ARG L1241_;
 AM_CALL_ARG L125;
 extern STR iter3;
 extern STR name24;
 extern STR name76;
 extern STR Noretu151888568;
 BOOL L126;
 BOOL L1271_;
 extern STR name36;
 extern STR name24;
 extern STR name76;
 extern STR name24;
 extern STR name76;
 extern STR Noretu151888568;
 BOOL L128;
 BOOL L129;
 BOOL L130;
 BOOL L131;
 BOOL L132;
 BOOL L1331_;
 BOOL L134;
 BOOL L135;
 BOOL L136;
 BOOL L1371_;
 BOOL L138;
 BOOL L1391_;
 SE_CONTEXT L140;
 BOOL L142;
 BOOL L143;
 BOOL L144;
 BOOL L1451_;
 STR L146;
 INT L147;
 INT L1481_;
 INT L149;
 CHAR L1501_;
 CHAR L151;
 BOOL L1521_;
 BOOL L153;
 BOOL L1541_;
 BOOL L155;
 BOOL L156;
 BOOL L157;
 BOOL L1581_;
 BOOL L159;
 BOOL L1601_;
 SE_CONTEXT L161;
 extern STR SET_CU1561499530;
 extern STR if3;
 extern STR state1;
 INT L162;
 INT L1631_;
 extern STR LM_LOC1545209594;
 INT L164;
 INT L165;
 INT L1661_;
 extern STR POP_EXCEPTION12;
 extern STR POP_EXCEPTION11;
 BOOL L167;
 BOOL L1681_;
 extern STR name76;
 extern STR goto1;
 extern STR name16;
 INT L169;
 INT L1701_;
 extern STR name4;
 if (ATTR(self,in_constant)) {
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(aice,fun);
  ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
  L4 = (ard==((AM_ROUT_DEF) NULL));
  L51_=!(L4); 
  if (L51_) {
   SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
   SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
  }
 }
 tp_self = aice;
 ret_val1 = ATTR(ATTR(tp_self,fun),ret);
 L6 = (ret_val1==((dTP) NULL));
 L71_=!(L6); 
 if (L71_) {
  dec_local_commen = self;
  tp_self1 = aice;
  ret_val2 = ATTR(ATTR(tp_self1,fun),ret);
  dec_local_commen1 = ret_val2;
  plus_self = ((STR) &Holdsr2142202862);
  plus_sarg = ATTR(ATTR(aice,fun),str);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  dec_local_commen2 = ret_val3;
  res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
  comment_self = dec_local_commen;
  comment_f = ATTR(dec_local_commen,code_c);
  comment_com = dec_local_commen2;
  if (ATTR(comment_self,pretty)) {
   plus_self1 = comment_f;
   plus_s = ((dSTR) ((STR) &name78));
   L8 = (plus_s==((dSTR) NULL));
   L91_=!(L8); 
   if (L91_) {
    L10 = ATTR(plus_self1,ntext);
    L12 = plus_s;
    SATTR(plus_self1,ntext,FSTR_p1752847026(L10, (*dSTR_strrSTR[TAG(L12)])(L12)));
   }
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s1 = ((dSTR) comment_com);
   L13 = (plus_s1==((dSTR) NULL));
   L141_=!(L13); 
   if (L141_) {
    L15 = ATTR(plus_self2,ntext);
    L16 = plus_s1;
    SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
   }
   ret_val6 = plus_self2;
   plus_self3 = ret_val6;
   plus_s2 = ((dSTR) ((STR) &name79));
   L17 = ATTR(plus_self3,ntext);
   L18 = plus_s2;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L17, (*dSTR_strrSTR[TAG(L18)])(L18)));
  }
  ret_val4 = res1;
  s1 = ret_val4;
 }
 is_builtin_iter_ = ATTR(aice,fun);
 if (SIG_is418491101(is_builtin_iter_)) {
  is_iter_self = is_builtin_iter_;
  is_iter_self1 = ATTR(is_iter_self,name1);
  L22 = (is_iter_self1.str==((STR) NULL));
  L231_=!(L22); 
  if (L231_) {
   L24 = is_iter_self1.str;
   L25 = STR_sizerINT(is_iter_self1.str);
   L261_=INTMINUS(L25,1); 
   L27 = L261_;
   L281_=ARR((STR)L24,L27); 
   L29 = L281_;
   L301_=L29=='!'; 
   L20 = L301_;
  } else {
   L20 = FALSE;
  }
  ret_val9 = L20;
  ret_val8 = ret_val9;
  L19 = ret_val8;
 } else {
  L19 = FALSE;
 }
 ret_val7 = L19;
 if (ret_val7) {
  L32 = ATTR(aice,init_before_loop);
  L331_=!(L32); 
  if (L331_) {
   ndefer_self = self;
   L34 = CGEN_i87475608(self);
   ndefer_s = STR_ap44172742(((STR) &if3), L34, ((STR) &f_), ATTR(aice,uniq), ((STR) &name84));
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   in_self = self;
   L35 = ATTR(in_self,indent);
   L361_=INTPLUS(L35,1); 
   SATTR(in_self,indent,L361_);
   ndefer_self1 = self;
   plus_self4 = CGEN_i87475608(self);
   plus_sarg1 = ((STR) &f_);
   ret_val10 = STR_ap2004550586(plus_self4, plus_sarg1);
   plus_self5 = ret_val10;
   plus_sarg2 = ATTR(aice,uniq);
   ret_val11 = STR_ap2004550586(plus_self5, plus_sarg2);
   plus_self6 = ret_val11;
   plus_sarg3 = ((STR) &FALSE2);
   ret_val12 = STR_ap2004550586(plus_self6, plus_sarg3);
   ndefer_s1 = ret_val12;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   CGEN_e844901491(self, aice);
   move_out_self = self;
   L37 = ATTR(move_out_self,indent);
   L381_=INTMINUS(L37,1); 
   SATTR(move_out_self,indent,L381_);
   ndefer_self2 = self;
   ndefer_s2 = ((STR) &name4);
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  }
  ret_val = CGEN_e1793966983(self, aice);
  return ret_val;
 }
 L39 = ATTR(aice,init_before_loop);
 L401_=!(L39); 
 if (L401_) {
  ndefer_self3 = self;
  mang_self = self;
  mang_ob = ((OB) aice);
  mang_ns = ((OB) ATTR(self,current_sig));
  ret_val13 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
  ndefer_s3 = STR_ap1693864410(((STR) &if3), ret_val13, ((STR) &state0));
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  in_self1 = self;
  L41 = ATTR(in_self1,indent);
  L421_=INTPLUS(L41,1); 
  SATTR(in_self1,indent,L421_);
  CGEN_e844901491(self, aice);
  move_out_self1 = self;
  L43 = ATTR(move_out_self1,indent);
  L441_=INTMINUS(L43,1); 
  SATTR(move_out_self1,indent,L441_);
  ndefer_self4 = self;
  ndefer_s4 = ((STR) &name4);
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 L45 = (ATTR(ATTR(aice,fun),hot)==((ARRAYBOOL) NULL));
 L461_=!(L45); 
 if (L461_) {
  {
   BOOL f_L471_ = TRUE;
   while (1) {
    if (f_L471_) {
     f_L471_ = FALSE;
     L11 = 1;
     L491_=ASIZE((AM_ITE1809135850)aice); 
     L50 = L491_;
     L521_=INTMINUS(L50,1); 
     L21 = L521_;
     L471_=L11-1;L471_m=L21; 
    }
    if(L471_++>=L471_m)  goto after_loop; 
    i = L471_;
    L53 = ATTR(ATTR(aice,fun),hot);
    L541_=INTMINUS(i,1); 
    L55 = L541_;
    L561_=ARR((ARRAYBOOL)L53,L55); 
    if (L561_) {
     L571_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i); 
     L58 = L571_;
     L59 = ATTR(ATTR(aice,fun),args);
     L601_=INTMINUS(i,1); 
     L61 = L601_;
     L621_=(ARG)ARR((ARRAYARG)L59,L61); 
     L63 = L621_;
     arg_str = CGEN_e414796278(self, L58, L63, FALSE, FALSE);
     L66 = ATTR(ATTR(aice,fun),args);
     L671_=INTMINUS(i,1); 
     L68 = L671_;
     L691_=(ARG)ARR((ARRAYARG)L66,L68); 
     L70=L691_;
     L72 = ATTR(L70,tp);
     if ((*dTP_is1334578382[TAG(L72)])(L72)) {
      L731_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i); 
      L74=L731_;
      L65 = SYSOBEQ(((OB) ATTR(L74,mode1)),((OB) MODES_out_mode));
     } else {
      L65 = FALSE;
     }
     if (L65) {
      L751_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i); 
      L76=L751_;
      L77 = ATTR(L76,expr);
      L78 = (*dAM_EXPR_tprdTP[TAG(L77)])(L77);
      L64 = (*dTP_is1811059018[TAG(L78)])(L78);
     } else {
      L64 = FALSE;
     }
     if (L64) {
      ndefer_self5 = self;
      mang_self1 = self;
      mang_ob1 = ((OB) aice);
      mang_ns1 = ((OB) ATTR(self,current_sig));
      ret_val14 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns1);
      str_self = i;
      clear_self = buf1;
      L79 = (clear_self==((FSTR) NULL));
      L801_=!(L79); 
      if (L801_) {
       SATTR(clear_self,loc,0);
      }
      str_in_self = str_self;
      str_in_s = buf1;
      ret_val16 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
      buf1 = ret_val16;
      str_self1 = buf1;
      ret_val17 = STR_fr1097270334(((STR) NULL), str_self1);
      ret_val15 = ret_val17;
      ndefer_s5 = STR_ap1925291910(ret_val14, ((STR) &arg3), ret_val15, ((STR) &name36), arg_str, ((STR) &name16));
      CGEN_d695831906(ndefer_self5);
      SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
     }
     else {
      ndefer_self6 = self;
      mang_self2 = self;
      mang_ob2 = ((OB) aice);
      mang_ns2 = ((OB) ATTR(self,current_sig));
      ret_val18 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, mang_ns2);
      str_self2 = i;
      clear_self1 = buf2;
      L81 = (clear_self1==((FSTR) NULL));
      L821_=!(L81); 
      if (L821_) {
       SATTR(clear_self1,loc,0);
      }
      str_in_self1 = str_self2;
      str_in_s1 = buf2;
      ret_val20 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
      buf2 = ret_val20;
      str_self3 = buf2;
      ret_val21 = STR_fr1097270334(((STR) NULL), str_self3);
      ret_val19 = ret_val21;
      cast_arg_self = self;
      L83 = ATTR(ATTR(aice,fun),args);
      L841_=INTMINUS(i,1); 
      L85 = L841_;
      L861_=(ARG)ARR((ARRAYARG)L83,L85); 
      L87=L861_;
      cast_arg_dest_tp = ATTR(L87,tp);
      L881_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i); 
      L89=L881_;
      L90 = ATTR(L89,expr);
      cast_arg_src_tp = (*dAM_EXPR_tprdTP[TAG(L90)])(L90);
      cast_arg_expr = arg_str;
      cast_arg_only_bo = FALSE;
      L921_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i); 
      L93=L921_;
      cast_arg_mode = ATTR(L93,mode1);
      ret_val22 = CGEN_d1915593494(cast_arg_self, cast_arg_dest_tp, cast_arg_src_tp, cast_arg_expr, cast_arg_only_bo, cast_arg_mode);
      ndefer_s6 = STR_ap1925291910(ret_val18, ((STR) &arg3), ret_val19, ((STR) &name36), ret_val22, ((STR) &name16));
      CGEN_d695831906(ndefer_self6);
      SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
     }
     comment_self1 = self;
     comment_com1 = ((STR) &hotargument);
     if (ATTR(comment_self1,pretty)) {
      ndefer_self7 = comment_self1;
      plus_self7 = ((STR) &name54);
      plus_sarg4 = comment_com1;
      ret_val23 = STR_ap2004550586(plus_self7, plus_sarg4);
      plus_self8 = ret_val23;
      plus_sarg5 = ((STR) &name79);
      ret_val24 = STR_ap2004550586(plus_self8, plus_sarg5);
      ndefer_s7 = ret_val24;
      CGEN_d695831906(ndefer_self7);
      SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
     }
    }
   }
  }
  after_loop: ;
 }
 pro = ((STR) &name1);
 if (ATTR(self,func_tables)) {
  pro = ((STR) &FF2);
 }
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L96 = TRUE;
 } else {
  L96 = ATTR(psather_self,distributed);
 }
 ret_val25 = L96;
 if (ret_val25) {
  L97 = ATTR(ATTR(aice,lp),no_begin_loop);
  L981_=!(L97); 
  L95 = L981_;
 } else {
  L95 = FALSE;
 }
 if (L95) {
  is_iter_self2 = ATTR(self,current_sig);
  is_iter_self3 = ATTR(is_iter_self2,name1);
  L100 = (is_iter_self3.str==((STR) NULL));
  L1011_=!(L100); 
  if (L1011_) {
   L102 = is_iter_self3.str;
   L103 = STR_sizerINT(is_iter_self3.str);
   L1041_=INTMINUS(L103,1); 
   L105 = L1041_;
   L1061_=ARR((STR)L102,L105); 
   L107 = L1061_;
   L1081_=L107=='!'; 
   L99 = L1081_;
  } else {
   L99 = FALSE;
  }
  ret_val27 = L99;
  ret_val26 = ret_val27;
  L94 = ret_val26;
 } else {
  L94 = FALSE;
 }
 if (L94) {
  ndefer_self8 = self;
  ndefer_s8 = ((STR) &RESTOR712275783);
  CGEN_d695831906(ndefer_self8);
  SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
 }
 L109 = ATTR(ATTR(aice,fun),tp);
 if ((*dTP_is1811059018[TAG(L109)])(L109)) {
  if (ATTR(self,chk_void)) {
   L111 = ATTR(self,null_segfaults);
   L1121_=!(L111); 
   L110 = L1121_;
  } else {
   L110 = FALSE;
  }
  if (L110) {
   ndefer_self9 = self;
   L1131_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,0); 
   L114=L1131_;
   L115 = CGEN_e1639504572(self, ATTR(L114,expr));
   ndefer_s9 = STR_ap1693864410(((STR) &if3), L115, ((STR) &NULL3));
   CGEN_d695831906(ndefer_self9);
   SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
   in_self2 = self;
   L116 = ATTR(in_self2,indent);
   L1171_=INTPLUS(L116,1); 
   SATTR(in_self2,indent,L1171_);
   runtime_error_se = self;
   plus_self9 = ((STR) &Dispatchedcallto);
   plus_sarg6 = ATTR(ATTR(aice,fun),str);
   ret_val28 = STR_ap2004550586(plus_self9, plus_sarg6);
   plus_self10 = ret_val28;
   plus_sarg7 = ((STR) &onvoidselfin);
   ret_val29 = STR_ap2004550586(plus_self10, plus_sarg7);
   plus_self11 = ret_val29;
   plus_sarg8 = ATTR(self,current_function);
   ret_val30 = STR_ap2004550586(plus_self11, plus_sarg8);
   runtime_error_s = ret_val30;
   ndefer_self10 = runtime_error_se;
   plus_self12 = ((STR) &FATAL1);
   plus_sarg9 = runtime_error_s;
   ret_val31 = STR_ap2004550586(plus_self12, plus_sarg9);
   plus_self13 = ret_val31;
   plus_sarg10 = ((STR) &name95);
   ret_val32 = STR_ap2004550586(plus_self13, plus_sarg10);
   ndefer_s10 = ret_val32;
   CGEN_d695831906(ndefer_self10);
   SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
   move_out_self2 = self;
   L118 = ATTR(move_out_self2,indent);
   L1191_=INTMINUS(L118,1); 
   SATTR(move_out_self2,indent,L1191_);
   ndefer_self11 = self;
   ndefer_s11 = ((STR) &name4);
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
  }
  if (ATTR(ATTR(self,options1),stats)) {
   ndefer_self12 = self;
   ndefer_s12 = ((STR) &COUNT_DISPATCH1);
   CGEN_d695831906(ndefer_self12);
   SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
  }
  tag1 = ((STR) &TAG2);
  if (ATTR(ATTR(self,prog),distributed)) {
   tag1 = ((STR) &F_TAG2);
  }
  tp_self2 = aice;
  ret_val33 = ATTR(ATTR(tp_self2,fun),ret);
  L120 = (ret_val33==((dTP) NULL));
  L1211_=!(L120); 
  if (L1211_) {
   ndefer_self13 = self;
   plus_self14 = s1;
   plus_sarg11 = ((STR) &name113);
   ret_val34 = STR_ap2004550586(plus_self14, plus_sarg11);
   plus_self15 = ret_val34;
   mang_self3 = self;
   mang_ob3 = ((OB) ATTR(aice,fun));
   ret_val35 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
   plus_sarg12 = ret_val35;
   ret_val36 = STR_ap2004550586(plus_self15, plus_sarg12);
   plus_self16 = ret_val36;
   plus_sarg13 = ((STR) &name14);
   ret_val37 = STR_ap2004550586(plus_self16, plus_sarg13);
   plus_self17 = ret_val37;
   plus_sarg14 = tag1;
   ret_val38 = STR_ap2004550586(plus_self17, plus_sarg14);
   plus_self18 = ret_val38;
   plus_sarg15 = ((STR) &name24);
   ret_val39 = STR_ap2004550586(plus_self18, plus_sarg15);
   plus_self19 = ret_val39;
   L1221_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,0); 
   L123=L1221_;
   plus_sarg16 = CGEN_e1639504572(self, ATTR(L123,expr));
   ret_val40 = STR_ap2004550586(plus_self19, plus_sarg16);
   plus_self20 = ret_val40;
   plus_sarg17 = ((STR) &iter3);
   ret_val41 = STR_ap2004550586(plus_self20, plus_sarg17);
   plus_self21 = ret_val41;
   plus_sarg18 = ((STR) &name24);
   ret_val42 = STR_ap2004550586(plus_self21, plus_sarg18);
   plus_self22 = ret_val42;
   mang_self4 = self;
   mang_ob4 = ((OB) aice);
   mang_ns3 = ((OB) ATTR(self,current_sig));
   ret_val43 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, mang_ns3);
   plus_sarg19 = ret_val43;
   ret_val44 = STR_ap2004550586(plus_self22, plus_sarg19);
   plus_self23 = ret_val44;
   plus_sarg20 = pro;
   ret_val45 = STR_ap2004550586(plus_self23, plus_sarg20);
   plus_self24 = ret_val45;
   plus_sarg21 = ((STR) &name76);
   ret_val46 = STR_ap2004550586(plus_self24, plus_sarg21);
   ndefer_s13 = ret_val46;
   CGEN_d695831906(ndefer_self13);
   SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
   res = s1;
  }
  else {
   ndefer_self14 = self;
   plus_self25 = ((STR) &name85);
   mang_self5 = self;
   mang_ob5 = ((OB) ATTR(aice,fun));
   ret_val47 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
   plus_sarg22 = ret_val47;
   ret_val48 = STR_ap2004550586(plus_self25, plus_sarg22);
   plus_self26 = ret_val48;
   plus_sarg23 = ((STR) &name14);
   ret_val49 = STR_ap2004550586(plus_self26, plus_sarg23);
   plus_self27 = ret_val49;
   plus_sarg24 = tag1;
   ret_val50 = STR_ap2004550586(plus_self27, plus_sarg24);
   plus_self28 = ret_val50;
   plus_sarg25 = ((STR) &name24);
   ret_val51 = STR_ap2004550586(plus_self28, plus_sarg25);
   plus_self29 = ret_val51;
   L1241_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,0); 
   L125=L1241_;
   plus_sarg26 = CGEN_e1639504572(self, ATTR(L125,expr));
   ret_val52 = STR_ap2004550586(plus_self29, plus_sarg26);
   plus_self30 = ret_val52;
   plus_sarg27 = ((STR) &iter3);
   ret_val53 = STR_ap2004550586(plus_self30, plus_sarg27);
   plus_self31 = ret_val53;
   plus_sarg28 = ((STR) &name24);
   ret_val54 = STR_ap2004550586(plus_self31, plus_sarg28);
   plus_self32 = ret_val54;
   mang_self6 = self;
   mang_ob6 = ((OB) aice);
   mang_ns4 = ((OB) ATTR(self,current_sig));
   ret_val55 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, mang_ns4);
   plus_sarg29 = ret_val55;
   ret_val56 = STR_ap2004550586(plus_self32, plus_sarg29);
   plus_self33 = ret_val56;
   plus_sarg30 = pro;
   ret_val57 = STR_ap2004550586(plus_self33, plus_sarg30);
   plus_self34 = ret_val57;
   plus_sarg31 = ((STR) &name76);
   ret_val58 = STR_ap2004550586(plus_self34, plus_sarg31);
   ndefer_s14 = ret_val58;
   CGEN_d695831906(ndefer_self14);
   SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
   res = ((STR) &Noretu151888568);
  }
 }
 else {
  tp_self3 = aice;
  ret_val59 = ATTR(ATTR(tp_self3,fun),ret);
  L126 = (ret_val59==((dTP) NULL));
  L1271_=!(L126); 
  if (L1271_) {
   ndefer_self15 = self;
   mang_self7 = self;
   mang_ob7 = ((OB) ATTR(aice,fun));
   ret_val60 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
   mang_self8 = self;
   mang_ob8 = ((OB) aice);
   mang_ns5 = ((OB) ATTR(self,current_sig));
   ret_val61 = MANGLE119219986(ATTR(mang_self8,mangler), mang_ob8, mang_ns5);
   plus_self35 = ret_val61;
   plus_sarg32 = pro;
   ret_val62 = STR_ap2004550586(plus_self35, plus_sarg32);
   ndefer_s15 = STR_ap1925291910(s1, ((STR) &name36), ret_val60, ((STR) &name24), ret_val62, ((STR) &name76));
   CGEN_d695831906(ndefer_self15);
   SATTR(ndefer_self15,routine_code,FSTR_p1752847026(ATTR(ndefer_self15,routine_code), ndefer_s15));
   res = s1;
  }
  else {
   ndefer_self16 = self;
   mang_self9 = self;
   mang_ob9 = ((OB) ATTR(aice,fun));
   ret_val63 = MANGLE119219986(ATTR(mang_self9,mangler), mang_ob9, ((OB) NULL));
   mang_self10 = self;
   mang_ob10 = ((OB) aice);
   mang_ns6 = ((OB) ATTR(self,current_sig));
   ret_val64 = MANGLE119219986(ATTR(mang_self10,mangler), mang_ob10, mang_ns6);
   plus_self36 = ret_val64;
   plus_sarg33 = pro;
   ret_val65 = STR_ap2004550586(plus_self36, plus_sarg33);
   ndefer_s16 = STR_ap1417971546(ret_val63, ((STR) &name24), ret_val65, ((STR) &name76));
   CGEN_d695831906(ndefer_self16);
   SATTR(ndefer_self16,routine_code,FSTR_p1752847026(ATTR(ndefer_self16,routine_code), ndefer_s16));
   res = ((STR) &Noretu151888568);
  }
 }
 psather_self1 = ATTR(self,prog);
 if (ATTR(psather_self1,threads)) {
  L131 = TRUE;
 } else {
  L131 = ATTR(psather_self1,distributed);
 }
 ret_val66 = L131;
 if (ret_val66) {
  L132 = ATTR(ATTR(aice,lp),no_begin_loop);
  L1331_=!(L132); 
  L130 = L1331_;
 } else {
  L130 = FALSE;
 }
 if (L130) {
  L136 = ATTR(ATTR(self,prog),yields_in_locks);
  L1371_=!(L136); 
  if (L1371_) {
   L135 = TRUE;
  } else {
   L138 = ATTR(ATTR(self,options1),side_effects);
   L1391_=!(L138); 
   L135 = L1391_;
  }
  if (L135) {
   L134 = TRUE;
  } else {
   L140=SIG_ge642497605(ATTR(aice,fun), ATTR(self,prog));
   L134 = ATTR(L140,has_yield_in_loc);
  }
  L129 = L134;
 } else {
  L129 = FALSE;
 }
 if (L129) {
  is_iter_self4 = ATTR(self,current_sig);
  is_iter_self5 = ATTR(is_iter_self4,name1);
  L144 = (is_iter_self5.str==((STR) NULL));
  L1451_=!(L144); 
  if (L1451_) {
   L146 = is_iter_self5.str;
   L147 = STR_sizerINT(is_iter_self5.str);
   L1481_=INTMINUS(L147,1); 
   L149 = L1481_;
   L1501_=ARR((STR)L146,L149); 
   L151 = L1501_;
   L1521_=L151=='!'; 
   L143 = L1521_;
  } else {
   L143 = FALSE;
  }
  ret_val68 = L143;
  ret_val67 = ret_val68;
  L153 = ret_val67;
  L1541_=!(L153); 
  if (L1541_) {
   L142 = TRUE;
  } else {
   L157 = ATTR(ATTR(self,prog),yields_in_locks);
   L1581_=!(L157); 
   if (L1581_) {
    L156 = TRUE;
   } else {
    L159 = ATTR(ATTR(self,options1),side_effects);
    L1601_=!(L159); 
    L156 = L1601_;
   }
   if (L156) {
    L155 = TRUE;
   } else {
    L161=SIG_ge642497605(ATTR(self,current_sig), ATTR(self,prog));
    L155 = ATTR(L161,has_yield_in_loc);
   }
   L142 = L155;
  }
  L128 = L142;
 } else {
  L128 = FALSE;
 }
 if (L128) {
  ndefer_self17 = self;
  ndefer_s17 = ((STR) &SET_CU1561499530);
  CGEN_d695831906(ndefer_self17);
  SATTR(ndefer_self17,routine_code,FSTR_p1752847026(ATTR(ndefer_self17,routine_code), ndefer_s17));
 }
 ndefer_self18 = self;
 mang_self11 = self;
 mang_ob11 = ((OB) aice);
 mang_ns7 = ((OB) ATTR(self,current_sig));
 ret_val69 = MANGLE119219986(ATTR(mang_self11,mangler), mang_ob11, mang_ns7);
 ndefer_s18 = STR_ap1693864410(((STR) &if3), ret_val69, ((STR) &state1));
 CGEN_d695831906(ndefer_self18);
 SATTR(ndefer_self18,routine_code,FSTR_p1752847026(ATTR(ndefer_self18,routine_code), ndefer_s18));
 in_self3 = self;
 L162 = ATTR(in_self3,indent);
 L1631_=INTPLUS(L162,1); 
 SATTR(in_self3,indent,L1631_);
 if (ATTR(self,manual_loop_unlo)) {
  ndefer_self19 = self;
  ndefer_s19 = ((STR) &LM_LOC1545209594);
  CGEN_d695831906(ndefer_self19);
  SATTR(ndefer_self19,routine_code,FSTR_p1752847026(ATTR(ndefer_self19,routine_code), ndefer_s19));
 }
 pop_exceptions_s = self;
 L164 = ATTR(self,ex_nesting);
 L165 = ATTR(self,current_loop_ex_);
 L1661_=INTMINUS(L164,L165); 
 pop_exceptions_n = L1661_;
 L31 = pop_exceptions_n;
 switch (L31) {
  case 0: 
   break;
  case 1: 
   ndefer_self20 = pop_exceptions_s;
   ndefer_s20 = ((STR) &POP_EXCEPTION12);
   CGEN_d695831906(ndefer_self20);
   SATTR(ndefer_self20,routine_code,FSTR_p1752847026(ATTR(ndefer_self20,routine_code), ndefer_s20));
   break;
  default: ;
   ndefer_self21 = pop_exceptions_s;
   str_self4 = pop_exceptions_n;
   clear_self2 = buf3;
   L167 = (clear_self2==((FSTR) NULL));
   L1681_=!(L167); 
   if (L1681_) {
    SATTR(clear_self2,loc,0);
   }
   str_in_self2 = str_self4;
   str_in_s2 = buf3;
   ret_val71 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
   buf3 = ret_val71;
   str_self5 = buf3;
   ret_val72 = STR_fr1097270334(((STR) NULL), str_self5);
   ret_val70 = ret_val72;
   ndefer_s21 = STR_ap1693864410(((STR) &POP_EXCEPTION11), ret_val70, ((STR) &name76));
   CGEN_d695831906(ndefer_self21);
   SATTR(ndefer_self21,routine_code,FSTR_p1752847026(ATTR(ndefer_self21,routine_code), ndefer_s21));
 }
 ndefer_self22 = self;
 ndefer_s22 = STR_ap1693864410(((STR) &goto1), ATTR(self,current_loop), ((STR) &name16));
 CGEN_d695831906(ndefer_self22);
 SATTR(ndefer_self22,routine_code,FSTR_p1752847026(ATTR(ndefer_self22,routine_code), ndefer_s22));
 move_out_self3 = self;
 L169 = ATTR(move_out_self3,indent);
 L1701_=INTMINUS(L169,1); 
 SATTR(move_out_self3,indent,L1701_);
 ndefer_self23 = self;
 ndefer_s23 = ((STR) &name4);
 CGEN_d695831906(ndefer_self23);
 SATTR(ndefer_self23,routine_code,FSTR_p1752847026(ATTR(ndefer_self23,routine_code), ndefer_s23));
 if (CGEN_n1296275061(self, ((dAM_CALL_EXPR) aice))) {
  CGEN_c922285894(self, ((dAM_CALL_EXPR) aice), ((ARRAYSTR) NULL));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1633118543(CGEN self, AM_STR_CONST asc) {
 STR ret_val;
 STR s;
 STR name111;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val2;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val3;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 CODE_FILE ret_val4;
 CODE_FILE plus_self6;
 dSTR plus_s6;
 CODE_FILE ret_val5;
 CODE_FILE plus_self7;
 dSTR plus_s7;
 CODE_FILE plus_self8;
 dSTR plus_s8;
 CODE_FILE ret_val6;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val7;
 STR res;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val8;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 CODE_FILE plus_self9;
 dSTR plus_s9;
 CODE_FILE plus_self10;
 dSTR plus_s10;
 CODE_FILE plus_self11;
 dSTR plus_s11;
 CODE_FILE ret_val9;
 CODE_FILE plus_self12;
 dSTR plus_s12;
 CODE_FILE plus_self13;
 dSTR plus_s13;
 CODE_FILE ret_val10;
 CODE_FILE plus_self14;
 dSTR plus_s14;
 CODE_FILE ret_val11;
 CODE_FILE plus_self15;
 dSTR plus_s15;
 CODE_FILE ret_val12;
 CODE_FILE plus_self16;
 dSTR plus_s16;
 CODE_FILE ret_val13;
 CODE_FILE plus_self17;
 dSTR plus_s17;
 CODE_FILE plus_self18;
 dSTR plus_s18;
 CODE_FILE ret_val14;
 CODE_FILE plus_self19;
 dSTR plus_s19;
 CODE_FILE ret_val15;
 CODE_FILE plus_self20;
 dSTR plus_s20;
 CODE_FILE ret_val16;
 CODE_FILE plus_self21;
 dSTR plus_s21;
 extern STR struct1;
 FSTR L1;
 dSTR L2;
 extern STR OB_HEADERheader;
 FSTR L3;
 dSTR L4;
 extern STR INTasize;
 FSTR L5;
 dSTR L6;
 extern STR CHARarr_part;
 BOOL L7;
 BOOL L81_;
 FSTR L9;
 dSTR L10;
 INT L11;
 INT L121_;
 dSTR L13;
 OB L14;
 BOOL L15;
 BOOL L161_;
 FSTR L17;
 dSTR L18;
 extern STR name117;
 BOOL L19;
 BOOL L201_;
 FSTR L21;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 extern STR name118;
 FSTR L27;
 dSTR L28;
 extern STR name2;
 BOOL L29;
 BOOL L301_;
 FSTR L31;
 dSTR L32;
 extern STR S_tag;
 FSTR L33;
 dSTR L34;
 extern STR S03;
 FSTR L35;
 dSTR L36;
 extern STR name119;
 BOOL L37;
 BOOL L381_;
 FSTR L39;
 dSTR L40;
 dSTR L41;
 OB L42;
 FSTR L43;
 dSTR L44;
 INT L45;
 INT L461_;
 extern STR name120;
 BOOL L47;
 BOOL L481_;
 FSTR L49;
 dSTR L50;
 dSTR L51;
 OB L52;
 BOOL L53;
 BOOL L541_;
 FSTR L55;
 dSTR L56;
 extern STR name121;
 BOOL L57;
 BOOL L581_;
 FSTR L59;
 dSTR L60;
 BOOL L61;
 BOOL L621_;
 FSTR L63;
 dSTR L64;
 extern STR name122;
 FSTR L65;
 dSTR L66;
 BOOL L67;
 BOOL L681_;
 FSTR L69;
 dSTR L70;
 extern STR externSTR;
 BOOL L71;
 BOOL L721_;
 FSTR L73;
 dSTR L74;
 BOOL L75;
 BOOL L761_;
 FSTR L77;
 dSTR L78;
 dSTR L79;
 OB L80;
 FSTR L82;
 dSTR L83;
 extern STR STR2;
 extern STR name26;
 s = ATTR(asc,bval);
 name111 = FMAPST1518383807(ATTR(self,string_constants), s);
 if ((name111==((STR) NULL))) {
  mang_self = self;
  mang_ob = ((OB) asc);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  name111 = ret_val1;
  plus_self = ATTR(self,strings_c);
  plus_s = ((dSTR) ((STR) &struct1));
  L1 = ATTR(plus_self,ntext);
  L2 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L1, (*dSTR_strrSTR[TAG(L2)])(L2)));
  plus_self1 = ATTR(self,strings_c);
  plus_s1 = ((dSTR) ((STR) &OB_HEADERheader));
  L3 = ATTR(plus_self1,ntext);
  L4 = plus_s1;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L3, (*dSTR_strrSTR[TAG(L4)])(L4)));
  plus_self2 = ATTR(self,strings_c);
  plus_s2 = ((dSTR) ((STR) &INTasize));
  L5 = ATTR(plus_self2,ntext);
  L6 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
  plus_self3 = ATTR(self,strings_c);
  plus_s3 = ((dSTR) ((STR) &CHARarr_part));
  L7 = (plus_s3==((dSTR) NULL));
  L81_=!(L7); 
  if (L81_) {
   L9 = ATTR(plus_self3,ntext);
   L10 = plus_s3;
   SATTR(plus_self3,ntext,FSTR_p1752847026(L9, (*dSTR_strrSTR[TAG(L10)])(L10)));
  }
  ret_val2 = plus_self3;
  plus_self4 = ret_val2;
  L11 = STR_lengthrINT(s);
  L121_=INTPLUS(L11,1); 
  L14=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  memset(L14,0,sizeof(struct INT_boxed_struct));
  ((OB)L14)->header.tag=INT_tag;
  L13 = (dSTR)((INT_boxed) L14);
  ((INT_boxed) L13)->immutable_part = L121_;
  plus_s4 = L13;
  L15 = (plus_s4==((dSTR) NULL));
  L161_=!(L15); 
  if (L161_) {
   L17 = ATTR(plus_self4,ntext);
   L18 = plus_s4;
   SATTR(plus_self4,ntext,FSTR_p1752847026(L17, (*dSTR_strrSTR[TAG(L18)])(L18)));
  }
  ret_val3 = plus_self4;
  plus_self5 = ret_val3;
  plus_s5 = ((dSTR) ((STR) &name117));
  L19 = (plus_s5==((dSTR) NULL));
  L201_=!(L19); 
  if (L201_) {
   L21 = ATTR(plus_self5,ntext);
   L22 = plus_s5;
   SATTR(plus_self5,ntext,FSTR_p1752847026(L21, (*dSTR_strrSTR[TAG(L22)])(L22)));
  }
  ret_val4 = plus_self5;
  plus_self6 = ret_val4;
  plus_s6 = ((dSTR) name111);
  L23 = (plus_s6==((dSTR) NULL));
  L241_=!(L23); 
  if (L241_) {
   L25 = ATTR(plus_self6,ntext);
   L26 = plus_s6;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
  }
  ret_val5 = plus_self6;
  plus_self7 = ret_val5;
  plus_s7 = ((dSTR) ((STR) &name118));
  L27 = ATTR(plus_self7,ntext);
  L28 = plus_s7;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
  plus_self8 = ATTR(self,strings_c);
  plus_s8 = ((dSTR) ((STR) &name2));
  L29 = (plus_s8==((dSTR) NULL));
  L301_=!(L29); 
  if (L301_) {
   L31 = ATTR(plus_self8,ntext);
   L32 = plus_s8;
   SATTR(plus_self8,ntext,FSTR_p1752847026(L31, (*dSTR_strrSTR[TAG(L32)])(L32)));
  }
  ret_val6 = plus_self8;
  plus_self9 = ret_val6;
  tag_for_self = self;
  tag_for_tp = ((dTP) TP_BUILTIN_str);
  SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
  mang_self1 = tag_for_self;
  mang_ob1 = ((OB) tag_for_tp);
  ret_val8 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  res = STR_ap2004550586(ret_val8, ((STR) &S_tag));
  forbid_self = tag_for_self;
  forbid_s = res;
  forbid_self1 = ATTR(forbid_self,mangler);
  forbid_s1 = forbid_s;
  SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
  ret_val7 = res;
  plus_s9 = ((dSTR) ret_val7);
  L33 = ATTR(plus_self9,ntext);
  L34 = plus_s9;
  SATTR(plus_self9,ntext,FSTR_p1752847026(L33, (*dSTR_strrSTR[TAG(L34)])(L34)));
  if (ATTR(ATTR(self,options1),destroy_chk)) {
   plus_self10 = ATTR(self,strings_c);
   plus_s10 = ((dSTR) ((STR) &S03));
   L35 = ATTR(plus_self10,ntext);
   L36 = plus_s10;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L35, (*dSTR_strrSTR[TAG(L36)])(L36)));
  }
  if (ATTR(self,deterministic1)) {
   plus_self11 = ATTR(self,strings_c);
   plus_s11 = ((dSTR) ((STR) &name119));
   L37 = (plus_s11==((dSTR) NULL));
   L381_=!(L37); 
   if (L381_) {
    L39 = ATTR(plus_self11,ntext);
    L40 = plus_s11;
    SATTR(plus_self11,ntext,FSTR_p1752847026(L39, (*dSTR_strrSTR[TAG(L40)])(L40)));
   }
   ret_val9 = plus_self11;
   plus_self12 = ret_val9;
   L42=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
   if (L42==NULL) FATAL("Unable to allocate more memory");
   memset(L42,0,sizeof(struct INT_boxed_struct));
   ((OB)L42)->header.tag=INT_tag;
   L41 = (dSTR)((INT_boxed) L42);
   ((INT_boxed) L41)->immutable_part = ATTR(self,str_count);
   plus_s12 = L41;
   L43 = ATTR(plus_self12,ntext);
   L44 = plus_s12;
   SATTR(plus_self12,ntext,FSTR_p1752847026(L43, (*dSTR_strrSTR[TAG(L44)])(L44)));
   L45 = ATTR(self,str_count);
   L461_=INTPLUS(L45,1); 
   SATTR(self,str_count,L461_);
  }
  plus_self13 = ATTR(self,strings_c);
  plus_s13 = ((dSTR) ((STR) &name120));
  L47 = (plus_s13==((dSTR) NULL));
  L481_=!(L47); 
  if (L481_) {
   L49 = ATTR(plus_self13,ntext);
   L50 = plus_s13;
   SATTR(plus_self13,ntext,FSTR_p1752847026(L49, (*dSTR_strrSTR[TAG(L50)])(L50)));
  }
  ret_val10 = plus_self13;
  plus_self14 = ret_val10;
  L52=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
  if (L52==NULL) FATAL("Unable to allocate more memory");
  memset(L52,0,sizeof(struct INT_boxed_struct));
  ((OB)L52)->header.tag=INT_tag;
  L51 = (dSTR)((INT_boxed) L52);
  ((INT_boxed) L51)->immutable_part = STR_lengthrINT(s);
  plus_s14 = L51;
  L53 = (plus_s14==((dSTR) NULL));
  L541_=!(L53); 
  if (L541_) {
   L55 = ATTR(plus_self14,ntext);
   L56 = plus_s14;
   SATTR(plus_self14,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
  }
  ret_val11 = plus_self14;
  plus_self15 = ret_val11;
  plus_s15 = ((dSTR) ((STR) &name121));
  L57 = (plus_s15==((dSTR) NULL));
  L581_=!(L57); 
  if (L581_) {
   L59 = ATTR(plus_self15,ntext);
   L60 = plus_s15;
   SATTR(plus_self15,ntext,FSTR_p1752847026(L59, (*dSTR_strrSTR[TAG(L60)])(L60)));
  }
  ret_val12 = plus_self15;
  plus_self16 = ret_val12;
  plus_s16 = ((dSTR) MANGLE1995580116(ATTR(self,mangler), s));
  L61 = (plus_s16==((dSTR) NULL));
  L621_=!(L61); 
  if (L621_) {
   L63 = ATTR(plus_self16,ntext);
   L64 = plus_s16;
   SATTR(plus_self16,ntext,FSTR_p1752847026(L63, (*dSTR_strrSTR[TAG(L64)])(L64)));
  }
  ret_val13 = plus_self16;
  plus_self17 = ret_val13;
  plus_s17 = ((dSTR) ((STR) &name122));
  L65 = ATTR(plus_self17,ntext);
  L66 = plus_s17;
  SATTR(plus_self17,ntext,FSTR_p1752847026(L65, (*dSTR_strrSTR[TAG(L66)])(L66)));
  SATTR(self,string_constants,FMAPST859022540(ATTR(self,string_constants), s, name111));
  CODE_F65146505(ATTR(self,strings_c));
 }
 plus_self18 = ATTR(self,code_c);
 plus_s18 = ((dSTR) CGEN_eolrSTR(self));
 L67 = (plus_s18==((dSTR) NULL));
 L681_=!(L67); 
 if (L681_) {
  L69 = ATTR(plus_self18,ntext);
  L70 = plus_s18;
  SATTR(plus_self18,ntext,FSTR_p1752847026(L69, (*dSTR_strrSTR[TAG(L70)])(L70)));
 }
 ret_val14 = plus_self18;
 plus_self19 = ret_val14;
 plus_s19 = ((dSTR) ((STR) &externSTR));
 L71 = (plus_s19==((dSTR) NULL));
 L721_=!(L71); 
 if (L721_) {
  L73 = ATTR(plus_self19,ntext);
  L74 = plus_s19;
  SATTR(plus_self19,ntext,FSTR_p1752847026(L73, (*dSTR_strrSTR[TAG(L74)])(L74)));
 }
 ret_val15 = plus_self19;
 plus_self20 = ret_val15;
 plus_s20 = ((dSTR) name111);
 L75 = (plus_s20==((dSTR) NULL));
 L761_=!(L75); 
 if (L761_) {
  L77 = ATTR(plus_self20,ntext);
  L78 = plus_s20;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L77, (*dSTR_strrSTR[TAG(L78)])(L78)));
 }
 ret_val16 = plus_self20;
 plus_self21 = ret_val16;
 L80=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L80==NULL) FATAL("Unable to allocate more memory");
 memset(L80,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L80)->header.tag=CHAR_tag;
 L79 = (dSTR)((CHAR_boxed) L80);
 ((CHAR_boxed) L79)->immutable_part = ';';
 plus_s21 = L79;
 L82 = ATTR(plus_self21,ntext);
 L83 = plus_s21;
 SATTR(plus_self21,ntext,FSTR_p1752847026(L82, (*dSTR_strrSTR[TAG(L83)])(L83)));
 ret_val = STR_ap1693864410(((STR) &STR2), name111, ((STR) &name26));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1793966983(CGEN self, AM_ITE1809135850 aice) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR ret = ((STR) NULL);
 STR biname;
 STR li = ((STR) NULL);
 ARRAYSTR L11;
 ARRAYSTR L21;
 ARRAYSTR in11;
 ARRAYSTR L31;
 INT i = INT_zero;
 INT L41 = INT_zero;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 INT L51 = INT_zero;
 CODE_FILE uses_extern_self1;
 STR uses_extern_dec1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val4;
 CGEN ndefer_self1;
 STR ndefer_s1;
 INT L61 = INT_zero;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self6;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val9;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val10;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val11;
 STR r1;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self8;
 INT plus_arg2 = INT_zero;
 STR ret_val13;
 STR s2;
 INT str_self2 = INT_zero;
 STR ret_val14;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val15;
 FSTR str_self3;
 STR ret_val16;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val17;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val18;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val19;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val20;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val21;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val22;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN expand_macro_sel;
 STR expand_macro_r;
 dAM_CALL_EXPR expand_macro_cal;
 SIG expand_macro_fun;
 ARRAYSTR expand_macro_arg;
 STR expand_macro_uni;
 STR expand_macro_fra;
 STR ret_val23;
 CGEN ndefer_self3;
 STR ndefer_s3;
 INT L7;
 INT L81_;
 BOOL L9;
 extern STR Bofbui227171716;
 dSTAT L10;
 BOOL L12;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 INT L171_br;
 STR aL171_;
 STR L18;
 INT L191_;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 INT L241_br;
 STR aL241_;
 STR L25;
 INT L261_;
 BOOL L27;
 BOOL L281_;
 extern STR ifd;
 ARRAYSTR L29;
 STR L301_;
 extern STR name112;
 STR L32;
 SIG L33;
 ARRAYSTR L34;
 STR L35;
 STR L36;
 BOOL L37;
 BOOL L381_;
 extern STR ifd;
 ARRAYSTR L39;
 STR L401_;
 extern STR name112;
 STR L42;
 SIG L43;
 ARRAYSTR L44;
 STR L45;
 STR L46;
 BOOL L47;
 BOOL L481_;
 INT L491_br;
 INT rL491_;
 INT L50;
 BOOL L52;
 INT L531_;
 INT L54;
 INT L551_;
 INT L56;
 BOOL L571_;
 BOOL L58;
 BOOL L591_;
 STR L601_;
 STR L621_;
 STR L63;
 CHAR L641_;
 CHAR L65;
 BOOL L661_;
 STR L67;
 INT L68;
 OB L69;
 INT L70;
 STR L721_;
 STR L73;
 INT L74;
 OB L75;
 INT L76;
 extern STR line3;
 BOOL L78;
 BOOL L791_;
 extern STR name71;
 extern STR name73;
 STR L801_;
 extern STR name10;
 INT L821_;
 BOOL L83;
 BOOL L84;
 BOOL L851_;
 extern STR d31;
 SIG L86;
 ARRAYSTR L87;
 STR L88;
 STR L89;
 SIG L90;
 ARRAYSTR L91;
 STR L92;
 STR L93;
 BOOL L94;
 BOOL L95;
 BOOL L961_;
 extern STR d31;
 extern STR name1;
 L7 = ATTR(self,inlined_iter_cou);
 L81_=INTPLUS(L7,1); 
 SATTR(self,inlined_iter_cou,L81_);
 biname = ((STR) NULL);
 if (ATTR(ATTR(ATTR(aice,fun),builtin_info),use_index)) {
  L9 = ATTR(aice,use_loop_index);
 } else {
  L9 = FALSE;
 }
 if (L9) {
  li = ATTR(ATTR(aice,lp),loop_index);
 }
 L10 = ATTR(ATTR(self,prog),stat1);
 (*dSTAT_incr_STR[TAG(L10)])(L10, ((STR) &Bofbui227171716));
 L13 = ATTR(ATTR(self,prog),distributed);
 L141_=!(L13); 
 if (L141_) {
  L15 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),declare)==((ARRAYSTR) NULL));
  L161_=!(L15); 
  L12 = L161_;
 } else {
  L12 = FALSE;
 }
 if (L12) {
  {
   BOOL f_L171_ = TRUE;
   /* loop index variable */
   L41 = 0;
   L11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),declare);
   L171_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
   while (1) {
    uses_extern_self = ATTR(self,code_c);
    if(L41>=L171_br)  goto after_loop; 
    aL171_=ARR((ARRAYSTR)L11,L41); 
    uses_extern_dec = aL171_;
    SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
    L191_=INTPLUS(L41,1); 
    L41 = L191_;
   }
  }
  after_loop: ;
 }
 if (ATTR(ATTR(self,prog),distributed)) {
  L22 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),f_declare)==((ARRAYSTR) NULL));
  L231_=!(L22); 
  L20 = L231_;
 } else {
  L20 = FALSE;
 }
 if (L20) {
  {
   BOOL f_L241_ = TRUE;
   /* loop index variable */
   L51 = 0;
   L21 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_declare);
   L241_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
   while (1) {
    uses_extern_self1 = ATTR(self,code_c);
    if(L51>=L241_br)  goto after_loop1; 
    aL241_=ARR((ARRAYSTR)L21,L51); 
    uses_extern_dec1 = aL241_;
    SATTR(uses_extern_self1,externs1,FSETST1404644833(ATTR(uses_extern_self1,externs1), uses_extern_dec1));
    L261_=INTPLUS(L51,1); 
    L51 = L261_;
   }
  }
  after_loop1: ;
 }
 if (ATTR(ATTR(self,prog),distributed)) {
  L27 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),f_break)==((ARRAYSTR) NULL));
  L281_=!(L27); 
  if (L281_) {
   ndefer_self = self;
   plus_self = ((STR) &ifd);
   L29 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_break);
   L301_=(STR)ARR((ARRAYSTR)L29,0); 
   plus_sarg = L301_;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val1;
   plus_sarg1 = ((STR) &name112);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   L32 = ret_val2;
   L33 = ATTR(aice,fun);
   L34 = ATTR(aice,arg_list);
   L35 = ATTR(aice,uniq);
   L36 = CGEN_i87475608(self);
   ndefer_s = CGEN_e1606764211(self, L32, ((dAM_CALL_EXPR) aice), L33, L34, L35, L36, li);
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  }
 }
 else {
  L37 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),break1)==((ARRAYSTR) NULL));
  L381_=!(L37); 
  if (L381_) {
   ndefer_self1 = self;
   plus_self2 = ((STR) &ifd);
   L39 = ATTR(ATTR(ATTR(aice,fun),builtin_info),break1);
   L401_=(STR)ARR((ARRAYSTR)L39,0); 
   plus_sarg2 = L401_;
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_sarg3 = ((STR) &name112);
   ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
   L42 = ret_val4;
   L43 = ATTR(aice,fun);
   L44 = ATTR(aice,arg_list);
   L45 = ATTR(aice,uniq);
   L46 = CGEN_i87475608(self);
   ndefer_s1 = CGEN_e1606764211(self, L42, ((dAM_CALL_EXPR) aice), L43, L44, L45, L46, li);
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
 }
 in11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),iter);
 if (ATTR(ATTR(self,prog),distributed)) {
  in11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_iter);
 }
 L47 = (in11==((ARRAYSTR) NULL));
 L481_=!(L47); 
 if (L481_) {
  {
   BOOL f_L491_ = TRUE;
   /* loop index variable */
   L61 = 0;
   L31 = in11;
   L491_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
   while (1) {
    if(L61>=L491_br)  goto after_loop2; 
    rL491_=L61; 
    i = rL491_;
    L531_=(in11)==NULL?0:ASIZE((ARRAYSTR)in11); 
    L54 = L531_;
    L551_=INTMINUS(L54,1); 
    L56 = L551_;
    L571_=(i)==(L56); 
    if (L571_) {
     L58 = (ATTR(ATTR(aice,fun),ret)==((dTP) NULL));
     L591_=!(L58); 
     L52 = L591_;
    } else {
     L52 = FALSE;
    }
    if (L52) {
     L601_=(STR)ARR((ARRAYSTR)in11,i); 
     ret = L601_;
    }
    else {
     L621_=(STR)ARR((ARRAYSTR)in11,i); 
     L63 = L621_;
     L641_=ARR((STR)L63,0); 
     L65 = L641_;
     L661_=L65=='#'; 
     if (L661_) {
      plus_self4 = res;
      plus_arg = '\n';
      str_self = plus_arg;
      create_self = ((STR) NULL);
      create_c = str_self;
      L68 = 1;
      L70=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L68)*sizeof(CHAR);
      L69=ZALLOC_LEAF_BIG(L70);
      if (L69==NULL) FATAL("Unable to allocate more memory");
      memset(L69,0,L70);
      ((OB)L69)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L69)->header.destroyed=0;
#endif

      L67 = ((STR) L69);
      L67->asize = L68;
      r = L67;
      SARR((STR)r,0,create_c); 
      ;
      ret_val7 = r;
      ret_val6 = ret_val7;
      s = ret_val6;
      ret_val5 = STR_ap1077157958(plus_self4, s, TRUE);
      plus_self5 = ret_val5;
      L721_=(STR)ARR((ARRAYSTR)in11,i); 
      plus_sarg4 = L721_;
      ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
      plus_self6 = ret_val8;
      plus_arg1 = '\n';
      str_self1 = plus_arg1;
      create_self1 = ((STR) NULL);
      create_c1 = str_self1;
      L74 = 1;
      L76=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L74)*sizeof(CHAR);
      L75=ZALLOC_LEAF_BIG(L76);
      if (L75==NULL) FATAL("Unable to allocate more memory");
      memset(L75,0,L76);
      ((OB)L75)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L75)->header.destroyed=0;
#endif

      L73 = ((STR) L75);
      L73->asize = L74;
      r1 = L73;
      SARR((STR)r1,0,create_c1); 
      ;
      ret_val11 = r1;
      ret_val10 = ret_val11;
      s1 = ret_val10;
      ret_val9 = STR_ap1077157958(plus_self6, s1, TRUE);
      res = ret_val9;
      if (ATTR(self,debug1)) {
       plus_self7 = res;
       plus_sarg5 = ((STR) &line3);
       ret_val12 = STR_ap2004550586(plus_self7, plus_sarg5);
       plus_self8 = ret_val12;
       plus_arg2 = ATTR(self,last_lineno);
       str_self2 = plus_arg2;
       clear_self = buf1;
       L78 = (clear_self==((FSTR) NULL));
       L791_=!(L78); 
       if (L791_) {
        SATTR(clear_self,loc,0);
       }
       str_in_self = str_self2;
       str_in_s = buf1;
       ret_val15 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
       buf1 = ret_val15;
       str_self3 = buf1;
       ret_val16 = STR_fr1097270334(((STR) NULL), str_self3);
       ret_val14 = ret_val16;
       s2 = ret_val14;
       ret_val13 = STR_ap1077157958(plus_self8, s2, TRUE);
       plus_self9 = ret_val13;
       plus_sarg6 = ((STR) &name71);
       ret_val17 = STR_ap2004550586(plus_self9, plus_sarg6);
       plus_self10 = ret_val17;
       plus_sarg7 = ATTR(self,last_file);
       ret_val18 = STR_ap2004550586(plus_self10, plus_sarg7);
       plus_self11 = ret_val18;
       plus_sarg8 = ((STR) &name73);
       ret_val19 = STR_ap2004550586(plus_self11, plus_sarg8);
       res = ret_val19;
      }
     }
     else {
      plus_self12 = res;
      L801_=(STR)ARR((ARRAYSTR)in11,i); 
      plus_sarg9 = L801_;
      ret_val20 = STR_ap2004550586(plus_self12, plus_sarg9);
      plus_self13 = ret_val20;
      plus_sarg10 = ((STR) &name10);
      ret_val21 = STR_ap2004550586(plus_self13, plus_sarg10);
      res = ret_val21;
     }
    }
    L821_=INTPLUS(L61,1); 
    L61 = L821_;
   }
  }
  after_loop2: ;
  if (ATTR(ATTR(ATTR(aice,fun),builtin_info),use_index)) {
   L84 = ATTR(aice,use_loop_index);
   L851_=!(L84); 
   L83 = L851_;
  } else {
   L83 = FALSE;
  }
  if (L83) {
   plus_self14 = res;
   plus_sarg11 = ((STR) &d31);
   ret_val22 = STR_ap2004550586(plus_self14, plus_sarg11);
   res = ret_val22;
  }
  SATTR(aice,arg_list,CGEN_e1792349707(self, aice));
  ndefer_self2 = self;
  L86 = ATTR(aice,fun);
  L87 = ATTR(aice,arg_list);
  L88 = ATTR(aice,uniq);
  L89 = CGEN_i87475608(self);
  ndefer_s2 = CGEN_e1606764211(self, res, ((dAM_CALL_EXPR) aice), L86, L87, L88, L89, li);
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  L90 = ATTR(aice,fun);
  L91 = ATTR(aice,arg_list);
  L92 = ATTR(aice,uniq);
  L93 = CGEN_i87475608(self);
  ret_val = CGEN_e1606764211(self, ret, ((dAM_CALL_EXPR) aice), L90, L91, L92, L93, li);
  return ret_val;
 }
 else {
  if (ATTR(ATTR(ATTR(aice,fun),builtin_info),use_index)) {
   L95 = ATTR(aice,use_loop_index);
   L961_=!(L95); 
   L94 = L961_;
  } else {
   L94 = FALSE;
  }
  if (L94) {
   ndefer_self3 = self;
   expand_macro_sel = self;
   expand_macro_r = ((STR) &d31);
   expand_macro_cal = ((dAM_CALL_EXPR) aice);
   expand_macro_fun = ATTR(aice,fun);
   expand_macro_arg = ((ARRAYSTR) NULL);
   expand_macro_uni = ATTR(aice,uniq);
   expand_macro_fra = CGEN_i87475608(self);
   ret_val23 = CGEN_e1606764211(expand_macro_sel, expand_macro_r, expand_macro_cal, expand_macro_fun, expand_macro_arg, expand_macro_uni, expand_macro_fra, ((STR) NULL));
   ndefer_s3 = ret_val23;
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  }
  ret_val = ((STR) &name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e2038430067(CGEN self, AM_IF_EXPR arg) {
 STR ret_val;
 STR res;
 AM_IF_EXPR tp_self;
 dTP ret_val1;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val2;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val3;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val4;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN in_self;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN move_out_self;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN in_self1;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN move_out_self1;
 CGEN ndefer_self4;
 STR ndefer_s4;
 extern STR localfortest;
 extern STR name78;
 BOOL L1;
 BOOL L21_;
 FSTR L3;
 dSTR L4;
 BOOL L5;
 BOOL L61_;
 FSTR L7;
 dSTR L8;
 extern STR name79;
 FSTR L9;
 dSTR L10;
 extern STR if3;
 STR L11;
 extern STR name84;
 INT L12;
 INT L131_;
 extern STR name36;
 STR L14;
 extern STR name16;
 INT L15;
 INT L161_;
 extern STR else5;
 INT L17;
 INT L181_;
 extern STR name36;
 STR L19;
 extern STR name16;
 INT L20;
 INT L221_;
 extern STR name4;
 dec_local_commen = self;
 tp_self = arg;
 ret_val1 = ATTR(tp_self,tp_at);
 dec_local_commen1 = ret_val1;
 dec_local_commen2 = ((STR) &localfortest);
 res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
 comment_self = dec_local_commen;
 comment_f = ATTR(dec_local_commen,code_c);
 comment_com = dec_local_commen2;
 if (ATTR(comment_self,pretty)) {
  plus_self = comment_f;
  plus_s = ((dSTR) ((STR) &name78));
  L1 = (plus_s==((dSTR) NULL));
  L21_=!(L1); 
  if (L21_) {
   L3 = ATTR(plus_self,ntext);
   L4 = plus_s;
   SATTR(plus_self,ntext,FSTR_p1752847026(L3, (*dSTR_strrSTR[TAG(L4)])(L4)));
  }
  ret_val3 = plus_self;
  plus_self1 = ret_val3;
  plus_s1 = ((dSTR) comment_com);
  L5 = (plus_s1==((dSTR) NULL));
  L61_=!(L5); 
  if (L61_) {
   L7 = ATTR(plus_self1,ntext);
   L8 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L7, (*dSTR_strrSTR[TAG(L8)])(L8)));
  }
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((dSTR) ((STR) &name79));
  L9 = ATTR(plus_self2,ntext);
  L10 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L9, (*dSTR_strrSTR[TAG(L10)])(L10)));
 }
 ret_val2 = res1;
 res = ret_val2;
 ndefer_self = self;
 L11 = CGEN_e1639504572(self, ATTR(arg,test1));
 ndefer_s = STR_ap1693864410(((STR) &if3), L11, ((STR) &name84));
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 in_self = self;
 L12 = ATTR(in_self,indent);
 L131_=INTPLUS(L12,1); 
 SATTR(in_self,indent,L131_);
 ndefer_self1 = self;
 L14 = CGEN_e1639504572(self, ATTR(arg,if_true));
 ndefer_s1 = STR_ap1417971546(res, ((STR) &name36), L14, ((STR) &name16));
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 move_out_self = self;
 L15 = ATTR(move_out_self,indent);
 L161_=INTMINUS(L15,1); 
 SATTR(move_out_self,indent,L161_);
 ndefer_self2 = self;
 ndefer_s2 = ((STR) &else5);
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 in_self1 = self;
 L17 = ATTR(in_self1,indent);
 L181_=INTPLUS(L17,1); 
 SATTR(in_self1,indent,L181_);
 ndefer_self3 = self;
 L19 = CGEN_e1639504572(self, ATTR(arg,if_false));
 ndefer_s3 = STR_ap1417971546(res, ((STR) &name36), L19, ((STR) &name16));
 CGEN_d695831906(ndefer_self3);
 SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 move_out_self1 = self;
 L20 = ATTR(move_out_self1,indent);
 L221_=INTMINUS(L20,1); 
 SATTR(move_out_self1,indent,L221_);
 ndefer_self4 = self;
 ndefer_s4 = ((STR) &name4);
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e231282482(CGEN self, AM_NEW_EXPR arg) {
 STR ret_val;
 STR res;
 STR sizevar;
 STR s2;
 CGEN dec_local_commen;
 dTP dec_local_commen1;
 STR dec_local_commen2;
 STR ret_val1;
 STR res1;
 CGEN comment_self;
 CODE_FILE comment_f;
 STR comment_com;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val2;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val3;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg1;
 STR ret_val5;
 CGEN ndefer_self4;
 STR ndefer_s4;
 extern STR localfor;
 dTP L1;
 STR L2;
 extern STR create2;
 extern STR name78;
 BOOL L3;
 BOOL L41_;
 FSTR L5;
 dSTR L6;
 BOOL L7;
 BOOL L81_;
 FSTR L9;
 dSTR L10;
 extern STR name79;
 FSTR L11;
 dSTR L12;
 BOOL L13;
 BOOL L141_;
 dAM_EXPR L15;
 extern STR name36;
 extern STR name16;
 extern STR name36;
 STR L16;
 extern STR name16;
 extern STR asize1;
 extern STR name16;
 extern STR name36;
 STR L17;
 extern STR name16;
 dTP L18;
 extern STR INIT_LOCK_HEADER1;
 extern STR name76;
 dec_local_commen = self;
 dec_local_commen1 = ATTR(arg,tp_at);
 L1 = ATTR(arg,tp_at);
 L2 = (*dTP_strrSTR[TAG(L1)])(L1);
 dec_local_commen2 = STR_ap1693864410(((STR) &localfor), L2, ((STR) &create2));
 res1 = CGEN_d1425664957(dec_local_commen, dec_local_commen1);
 comment_self = dec_local_commen;
 comment_f = ATTR(dec_local_commen,code_c);
 comment_com = dec_local_commen2;
 if (ATTR(comment_self,pretty)) {
  plus_self = comment_f;
  plus_s = ((dSTR) ((STR) &name78));
  L3 = (plus_s==((dSTR) NULL));
  L41_=!(L3); 
  if (L41_) {
   L5 = ATTR(plus_self,ntext);
   L6 = plus_s;
   SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
  }
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = ((dSTR) comment_com);
  L7 = (plus_s1==((dSTR) NULL));
  L81_=!(L7); 
  if (L81_) {
   L9 = ATTR(plus_self1,ntext);
   L10 = plus_s1;
   SATTR(plus_self1,ntext,FSTR_p1752847026(L9, (*dSTR_strrSTR[TAG(L10)])(L10)));
  }
  ret_val3 = plus_self1;
  plus_self2 = ret_val3;
  plus_s2 = ((dSTR) ((STR) &name79));
  L11 = ATTR(plus_self2,ntext);
  L12 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L11, (*dSTR_strrSTR[TAG(L12)])(L12)));
 }
 ret_val1 = res1;
 res = ret_val1;
 L13 = (ATTR(arg,asz)==((dAM_EXPR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(arg,asz);
  sizevar = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L15)])(L15));
  s2 = CGEN_e1639504572(self, ATTR(arg,asz));
  ndefer_self = self;
  ndefer_s = STR_ap1417971546(sizevar, ((STR) &name36), s2, ((STR) &name16));
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  ndefer_self1 = self;
  L16 = CGEN_a59983474(self, ATTR(arg,tp_at), sizevar);
  ndefer_s1 = STR_ap1417971546(res, ((STR) &name36), L16, ((STR) &name16));
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  ndefer_self2 = self;
  ndefer_s2 = STR_ap1417971546(res, ((STR) &asize1), sizevar, ((STR) &name16));
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 else {
  ndefer_self3 = self;
  L17 = CGEN_a1155118157(self, ATTR(arg,tp_at));
  ndefer_s3 = STR_ap1417971546(res, ((STR) &name36), L17, ((STR) &name16));
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 L18 = ATTR(arg,tp_at);
 if ((*dTP_is1999456142[TAG(L18)])(L18, ((dTP) TP_BUI1813155122))) {
  ndefer_self4 = self;
  plus_self3 = ((STR) &INIT_LOCK_HEADER1);
  plus_sarg = res;
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg);
  plus_self4 = ret_val4;
  plus_sarg1 = ((STR) &name76);
  ret_val5 = STR_ap2004550586(plus_self4, plus_sarg1);
  ndefer_s4 = ret_val5;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e453436338(CGEN self, AM_ROU1916046290 arce) {
 STR ret_val;
 dTP t1;
 dTP t2;
 STR arg11;
 STR arg2;
 STR carg1;
 STR carg2;
 dAM_EXPR argexpr;
 STR arg = ((STR) NULL);
 STR res;
 INT i = INT_zero;
 CHAR c = CHAR_zero;
 CHAR L11 = CHAR_zero;
 STR var;
 CHAR vc = CHAR_zero;
 BOOL done = BOOL_zero;
 AM_ROUT_DEF L21;
 AM_LOCAL_EXPR lcl;
 FLISTA725283029 L31;
 AM_LOCAL_EXPR lcl1;
 ARRAYSTR arg_list;
 STR func_res;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val2;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val3;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 INT L41 = INT_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val13;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val14;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val15;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val16;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val17;
 STR r1;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val20;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val21;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val22;
 STR r2;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self13;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val23;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val24;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val25;
 STR r3;
 CGEN ndefer_self1;
 STR ndefer_s1;
 dTP L5;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 BOOL L10;
 BOOL L121_;
 dTP L13;
 AM_CALL_ARG L141_;
 AM_CALL_ARG L15;
 dAM_EXPR L16;
 AM_CALL_ARG L171_;
 AM_CALL_ARG L18;
 dAM_EXPR L19;
 AM_CALL_ARG L201_;
 AM_CALL_ARG L22;
 AM_CALL_ARG L231_;
 AM_CALL_ARG L24;
 BOOL L25;
 dTP L26;
 dTP L27;
 INT L28;
 INT L291_;
 dTP L30;
 extern STR FALSE1;
 BOOL L32;
 BOOL L33;
 BOOL L34;
 dTP L35;
 dTP L36;
 dTP L37;
 dTP L38;
 extern STR F_SYSOBEQ1;
 extern STR name3;
 extern STR name26;
 extern STR SYSOBEQ1;
 extern STR name3;
 extern STR name26;
 INT L39;
 INT L401_;
 extern STR name24;
 extern STR name110;
 extern STR name26;
 extern STR inlined_C;
 AM_CALL_ARG L421_;
 AM_CALL_ARG L43;
 extern STR inline29312080;
 extern STR Intern28965746;
 extern STR name1;
 INT L44;
 BOOL L451_;
 CHAR L461_;
 BOOL L47;
 INT L481_;
 INT L49;
 INT L50;
 BOOL L511_;
 INT L521_;
 INT L53;
 CHAR L541_;
 CHAR L55;
 BOOL L561_;
 INT L571_;
 extern STR name1111;
 extern STR name1;
 INT L581_;
 INT L59;
 BOOL L601_;
 CHAR L611_;
 BOOL L62;
 BOOL L631_;
 STR L64;
 INT L65;
 OB L66;
 INT L67;
 INT L691_br;
 AM_FORMAL_ARG aL691_;
 AM_FORMAL_ARG L70;
 AM_FORMAL_ARG L72;
 INT L731_;
 BOOL L741_;
 AM_LOCAL_EXPR L75;
 BOOL L761_;
 extern STR Unknownvariable;
 extern STR toinlinein;
 extern STR Intern28965746;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 INT L821_;
 BOOL L83;
 BOOL L841_;
 extern STR name36;
 STR L85;
 INT L86;
 OB L87;
 INT L88;
 STR L90;
 INT L92;
 OB L93;
 INT L94;
 L5 = ATTR(ATTR(arce,fun),tp);
 if ((*dTP_is1811059018[TAG(L5)])(L5)) {
  L6 = ATTR(self,abstract_calls);
  L71_=INTPLUS(L6,1); 
  SATTR(self,abstract_calls,L71_);
 }
 else {
  L8 = ATTR(self,concrete_calls);
  L91_=INTPLUS(L8,1); 
  SATTR(self,concrete_calls,L91_);
 }
 if (ATTR(self,in_constant)) {
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(arce,fun);
  ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
  L10 = (ard==((AM_ROUT_DEF) NULL));
  L121_=!(L10); 
  if (L121_) {
   SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
   SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
  }
 }
 L13 = ATTR(ATTR(arce,fun),tp);
 if ((*dTP_is242312711[TAG(L13)])(L13, ((OB) TP_BUILTIN_sys))) {
  is_eq_self = ATTR(ATTR(arce,fun),name1);
  is_eq_i = IDENT_461800478;
  ret_val1 = (is_eq_self.str==is_eq_i.str);
  if (ret_val1) {
   L141_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,1); 
   L15=L141_;
   L16 = ATTR(L15,expr);
   t1 = (*dAM_EXPR_tprdTP[TAG(L16)])(L16);
   L171_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,2); 
   L18=L171_;
   L19 = ATTR(L18,expr);
   t2 = (*dAM_EXPR_tprdTP[TAG(L19)])(L19);
   L201_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,1); 
   L22=L201_;
   arg11 = CGEN_e1639504572(self, ATTR(L22,expr));
   L231_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,2); 
   L24=L231_;
   arg2 = CGEN_e1639504572(self, ATTR(L24,expr));
   L26 = t1;
   if ((*dTP_is1334578382[TAG(L26)])(L26)) {
    L27 = t2;
    L25 = (*dTP_is1334578382[TAG(L27)])(L27);
   } else {
    L25 = FALSE;
   }
   if (L25) {
    L28 = ATTR(self,inlined_value_ob);
    L291_=INTPLUS(L28,1); 
    SATTR(self,inlined_value_ob,L291_);
    L30 = t1;
    if ((*dTP_is242312711[TAG(L30)])(L30, ((OB) t2))) {
     ret_val = CGEN_v205291382(self, t1, arg11, arg2);
     return ret_val;
    }
    else {
     ret_val = ((STR) &FALSE1);
     return ret_val;
    }
   }
   else {
    L35 = t1;
    if ((*dTP_is1334578382[TAG(L35)])(L35)) {
     L34 = TRUE;
    } else {
     L36 = t2;
     L34 = (*dTP_is1334578382[TAG(L36)])(L36);
    }
    if (L34) {
     L33 = TRUE;
    } else {
     L37 = t1;
     L33 = (*dTP_is1811059018[TAG(L37)])(L37);
    }
    if (L33) {
     L32 = TRUE;
    } else {
     L38 = t2;
     L32 = (*dTP_is1811059018[TAG(L38)])(L38);
    }
    if (L32) {
     cast_self = self;
     cast_dest_tp = ((dTP) TP_BUI1325635093);
     cast_src_tp = t1;
     cast_expr = arg11;
     cast_only_boxing = FALSE;
     ret_val2 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
     carg1 = ret_val2;
     cast_self1 = self;
     cast_dest_tp1 = ((dTP) TP_BUI1325635093);
     cast_src_tp1 = t2;
     cast_expr1 = arg2;
     cast_only_boxing1 = FALSE;
     ret_val3 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
     carg2 = ret_val3;
     if (ATTR(ATTR(self,prog),distributed)) {
      ret_val = STR_ap44172742(((STR) &F_SYSOBEQ1), carg1, ((STR) &name3), carg2, ((STR) &name26));
      return ret_val;
     }
     else {
      ret_val = STR_ap44172742(((STR) &SYSOBEQ1), carg1, ((STR) &name3), carg2, ((STR) &name26));
      return ret_val;
     }
    }
    else {
     L39 = ATTR(self,inlined_pointer_);
     L401_=INTPLUS(L39,1); 
     SATTR(self,inlined_pointer_,L401_);
     ret_val = STR_ap44172742(((STR) &name24), arg11, ((STR) &name110), arg2, ((STR) &name26));
     return ret_val;
    }
   }
  }
  else {
   if (STR_is111530248(ATTR(ATTR(arce,fun),name1).str, ((STR) &inlined_C))) {
    L421_=(AM_CALL_ARG)ARR((AM_ROU1916046290)arce,1); 
    L43=L421_;
    argexpr = ATTR(L43,expr);
    if (argexpr==NULL) {
     goto other370;
    } else
    switch (TAG(argexpr)) {
     case AM_STR_CONST_tag:
      arg = ATTR(((AM_STR_CONST) argexpr),bval); break;
     default: ;
     other370: ;
      barf_self = self;
      barf_msg = ((STR) &inline29312080);
      barf_self1 = ATTR(barf_self,prog);
      barf_msg1 = barf_msg;
      barf_at_self = barf_self1;
      barf_at_msg = barf_msg1;
      barf_at_at = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self, barf_at_at);
      plus_self = ((STR) &Intern28965746);
      plus_sarg = barf_at_msg;
      ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
      PROG_err_STR(barf_at_self, ret_val4);
      exit_self = ((UNIX) NULL);
      exit_code = 1;
      exit(exit_code);
    }
    res = ((STR) &name1);
    i = 0;
    while (1) {
     L44 = STR_sizerINT(arg);
     L451_=(i)<(L44); 
     if (L451_) {
     }
     else {
      goto after_loop;
     }
     L461_=ARR((STR)arg,i); 
     c = L461_;
     L11 = c;
     switch (L11) {
      case '#': 
       L481_=INTPLUS(i,2); 
       L49 = L481_;
       L50 = STR_sizerINT(arg);
       L511_=(L49)<(L50); 
       if (L511_) {
        L521_=INTPLUS(i,1); 
        L53 = L521_;
        L541_=ARR((STR)arg,L53); 
        L55 = L541_;
        L561_=L55=='#'; 
        L47 = L561_;
       } else {
        L47 = FALSE;
       }
       if (L47) {
        L571_=INTPLUS(i,2); 
        i = L571_;
        plus_self1 = res;
        plus_sarg1 = ((STR) &name1111);
        ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
        res = ret_val5;
       }
       else {
        var = ((STR) &name1);
        while (1) {
         L581_=INTPLUS(i,1); 
         i = L581_;
         L59 = STR_sizerINT(arg);
         L601_=(i)<(L59); 
         if (L601_) {
         }
         else {
          goto after_loop1;
         }
         L611_=ARR((STR)arg,i); 
         vc = L611_;
         if (CHAR_i1942379470(vc)) {
          L62 = TRUE;
         } else {
          L631_=vc=='_'; 
          L62 = L631_;
         }
         if (L62) {
         }
         else {
          goto after_loop1;
         }
         plus_self2 = var;
         plus_arg = vc;
         str_self = plus_arg;
         create_self = ((STR) NULL);
         create_c = str_self;
         L65 = 1;
         L67=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L65)*sizeof(CHAR);
         L66=ZALLOC_LEAF_BIG(L67);
         if (L66==NULL) FATAL("Unable to allocate more memory");
         memset(L66,0,L67);
         ((OB)L66)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L66)->header.destroyed=0;
#endif

         L64 = ((STR) L66);
         L64->asize = L65;
         r = L64;
         SARR((STR)r,0,create_c); 
         ;
         ret_val8 = r;
         ret_val7 = ret_val8;
         s = ret_val7;
         ret_val6 = STR_ap1077157958(plus_self2, s, TRUE);
         var = ret_val6;
        }
        after_loop1: ;
        done = FALSE;
        {
         BOOL f_L691_ = TRUE;
         /* loop index variable */
         L41 = 0;
         L21 = ATTR(self,current_am_rout_);
         L691_br=L21==NULL?0:ASIZE((AM_ROUT_DEF)L21); 
         while (1) {
          if(L41>=L691_br)  goto after_loop2; 
          aL691_=ARR((AM_ROUT_DEF)L21,L41); 
          L72=aL691_;
          lcl = ATTR(L72,expr);
          if (STR_is111530248(ATTR(lcl,name1).str, var)) {
           plus_self3 = res;
           plus_sarg2 = CGEN_e1639504572(self, ((dAM_EXPR) lcl));
           ret_val9 = STR_ap2004550586(plus_self3, plus_sarg2);
           res = ret_val9;
           done = TRUE;
           goto after_loop2;
          }
          L731_=INTPLUS(L41,1); 
          L41 = L731_;
         }
        }
        after_loop2: ;
        L741_=!(done); 
        if (L741_) {
         {
          struct FLISTA2119642552_frame_struct other4_0;
FLISTA2119642552_frame noname1 = &other4_0;
          L31 = ATTR(ATTR(self,current_am_rout_),locals1);
          noname1->self = L31;
          noname1->state = 0;
          while (1) {
           L75 = FLISTA2119642552(noname1);
           if (noname1->state == -1) {
            goto after_loop3;
           }
           lcl1 = L75;
           if (STR_is111530248(ATTR(lcl1,name1).str, var)) {
            plus_self4 = res;
            plus_sarg3 = CGEN_e1639504572(self, ((dAM_EXPR) lcl1));
            ret_val10 = STR_ap2004550586(plus_self4, plus_sarg3);
            res = ret_val10;
            done = TRUE;
            goto after_loop3;
           }
          }
         }
         after_loop3: ;
         L761_=!(done); 
         if (L761_) {
          barf_self2 = self;
          plus_self5 = ((STR) &Unknownvariable);
          plus_sarg4 = var;
          ret_val11 = STR_ap2004550586(plus_self5, plus_sarg4);
          plus_self6 = ret_val11;
          plus_sarg5 = ((STR) &toinlinein);
          ret_val12 = STR_ap2004550586(plus_self6, plus_sarg5);
          plus_self7 = ret_val12;
          plus_sarg6 = arg;
          ret_val13 = STR_ap2004550586(plus_self7, plus_sarg6);
          barf_msg2 = ret_val13;
          barf_self3 = ATTR(barf_self2,prog);
          barf_msg3 = barf_msg2;
          barf_at_self1 = barf_self3;
          barf_at_msg1 = barf_msg3;
          barf_at_at1 = ((dPROG_ERR) NULL);
          PROG_e176405615(barf_at_self1, barf_at_at1);
          plus_self8 = ((STR) &Intern28965746);
          plus_sarg7 = barf_at_msg1;
          ret_val14 = STR_ap2004550586(plus_self8, plus_sarg7);
          PROG_err_STR(barf_at_self1, ret_val14);
          exit_self1 = ((UNIX) NULL);
          exit_code1 = 1;
          exit(exit_code1);
         }
        }
       }
       break;
      default: ;
       plus_self9 = res;
       plus_arg1 = c;
       str_self1 = plus_arg1;
       create_self1 = ((STR) NULL);
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
       r1 = L77;
       SARR((STR)r1,0,create_c1); 
       ;
       ret_val17 = r1;
       ret_val16 = ret_val17;
       s1 = ret_val16;
       ret_val15 = STR_ap1077157958(plus_self9, s1, TRUE);
       res = ret_val15;
       L821_=INTPLUS(i,1); 
       i = L821_;
     }
    }
    after_loop: ;
    ret_val = res;
    return ret_val;
   }
  }
 }
 arg_list = CGEN_e1933193154(self, ((dAM_CALL_EXPR) arce));
 if (CGEN_n1296275061(self, ((dAM_CALL_EXPR) arce))) {
  L83 = (ATTR(ATTR(arce,fun),ret)==((dTP) NULL));
  L841_=!(L83); 
  if (L841_) {
   func_res = CGEN_d1425664957(self, ATTR(ATTR(arce,fun),ret));
   ndefer_self = self;
   plus_self10 = func_res;
   plus_sarg8 = ((STR) &name36);
   ret_val18 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val18;
   plus_sarg9 = CGEN_e1835933683(self, ATTR(arce,fun), ((dAM_EXPR) arce), arg_list);
   ret_val19 = STR_ap2004550586(plus_self11, plus_sarg9);
   plus_self12 = ret_val19;
   plus_arg2 = ';';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
   L86 = 1;
   L88=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L86)*sizeof(CHAR);
   L87=ZALLOC_LEAF_BIG(L88);
   if (L87==NULL) FATAL("Unable to allocate more memory");
   memset(L87,0,L88);
   ((OB)L87)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L87)->header.destroyed=0;
#endif

   L85 = ((STR) L87);
   L85->asize = L86;
   r2 = L85;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val22 = r2;
   ret_val21 = ret_val22;
   s2 = ret_val21;
   ret_val20 = STR_ap1077157958(plus_self12, s2, TRUE);
   ndefer_s = ret_val20;
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   CGEN_c922285894(self, ((dAM_CALL_EXPR) arce), arg_list);
   ret_val = func_res;
   return ret_val;
  }
  else {
   ndefer_self1 = self;
   plus_self13 = CGEN_e1835933683(self, ATTR(arce,fun), ((dAM_EXPR) arce), arg_list);
   plus_arg3 = ';';
   str_self3 = plus_arg3;
   create_self3 = ((STR) NULL);
   create_c3 = str_self3;
   L92 = 1;
   L94=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L92)*sizeof(CHAR);
   L93=ZALLOC_LEAF_BIG(L94);
   if (L93==NULL) FATAL("Unable to allocate more memory");
   memset(L93,0,L94);
   ((OB)L93)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L93)->header.destroyed=0;
#endif

   L90 = ((STR) L93);
   L90->asize = L92;
   r3 = L90;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val25 = r3;
   ret_val24 = ret_val25;
   s3 = ret_val24;
   ret_val23 = STR_ap1077157958(plus_self13, s3, TRUE);
   ndefer_s1 = ret_val23;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   CGEN_c922285894(self, ((dAM_CALL_EXPR) arce), arg_list);
   ret_val = ((STR) NULL);
   return ret_val;
  }
 }
 else {
  ret_val = CGEN_e1835933683(self, ATTR(arce,fun), ((dAM_EXPR) arce), arg_list);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e604348447(CGEN self, AM_SHARED_EXPR arg) {
 STR ret_val;
 AM_SHARED_EXPR orig;
 CODE_FILE uses_global_self;
 AM_SHARED_EXPR uses_global_age;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 dTP L1;
 orig = GLOBAL499677090(ATTR(ATTR(self,gen),global_tbl), ATTR(arg,name1), ATTR(arg,class_tp));
 uses_global_self = ATTR(self,code_c);
 uses_global_age = orig;
 SATTR(uses_global_self,globals,FSETAM381445024(ATTR(uses_global_self,globals), uses_global_age));
 L1 = ATTR(arg,class_tp);
 if ((*dTP_is_crBOOL[TAG(L1)])(L1)) {
  MANGLE1827205184(ATTR(self,mangler), ((OB) orig), ATTR(arg,name1).str, ((OB) NULL));
  ret_val = ATTR(arg,name1).str;
  return ret_val;
 }
 else {
  mang_self = self;
  mang_ob = ((OB) orig);
  ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  ret_val = ret_val1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_i87475608(CGEN self) {
 STR ret_val;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
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
 extern STR frame1;
 extern STR name1;
 is_iter_self = ATTR(self,current_sig);
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
 ret_val2 = L1;
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = ((STR) &frame1);
  return ret_val;
 }
 else {
  ret_val = ((STR) &name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_b2123071570(CGEN self, AM_SHARED_EXPR global11) {
 STR dummy1;
 AM_SHARED_EXPR tp_self;
 dTP ret_val;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val1;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val2;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 AM_SHARED_EXPR tp_self1;
 dTP ret_val3;
 AM_SHARED_EXPR tp_self2;
 dTP ret_val4;
 AM_SHARED_EXPR tp_self3;
 dTP ret_val5;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val6;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val10;
 CGEN ndefer_self;
 STR ndefer_s;
 AM_SHARED_EXPR tp_self4;
 dTP ret_val11;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val12;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val13;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val14;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val15;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val16;
 CGEN ndefer_self1;
 STR ndefer_s1;
 AM_SHARED_EXPR tp_self5;
 dTP ret_val17;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val18;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val19;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val20;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val21;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val22;
 CGEN ndefer_self2;
 STR ndefer_s2;
 extern STR S_tag;
 dTP L1;
 dTP L2;
 extern STR BROADC1653099486;
 extern STR name3;
 extern STR name76;
 extern STR BROADC1443310781;
 extern STR name3;
 extern STR name76;
 extern STR BROADC1443310857;
 extern STR name3;
 extern STR name76;
 if (ATTR(ATTR(self,prog),distributed)) {
  tag_for_self = self;
  tp_self = global11;
  ret_val = ATTR(tp_self,tp_at);
  tag_for_tp = ret_val;
  SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
  mang_self = tag_for_self;
  mang_ob = ((OB) tag_for_tp);
  ret_val2 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
  res = STR_ap2004550586(ret_val2, ((STR) &S_tag));
  forbid_self = tag_for_self;
  forbid_s = res;
  forbid_self1 = ATTR(forbid_self,mangler);
  forbid_s1 = forbid_s;
  SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
  ret_val1 = res;
  dummy1 = ret_val1;
  tp_self1 = global11;
  ret_val3 = ATTR(tp_self1,tp_at);
  L1 = ret_val3;
  if ((*dTP_is1334578382[TAG(L1)])(L1)) {
   tp_self2 = global11;
   ret_val4 = ATTR(tp_self2,tp_at);
   L2 = ret_val4;
   if ((*dTP_is418055720[TAG(L2)])(L2)) {
    ndefer_self = self;
    plus_self = ((STR) &BROADC1653099486);
    mang_self1 = self;
    tp_self3 = global11;
    ret_val5 = ATTR(tp_self3,tp_at);
    mang_ob1 = ((OB) ret_val5);
    ret_val6 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    plus_sarg = ret_val6;
    ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val7;
    plus_sarg1 = ((STR) &name3);
    ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val8;
    plus_sarg2 = CGEN_e604348447(self, global11);
    ret_val9 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val9;
    plus_sarg3 = ((STR) &name76);
    ret_val10 = STR_ap2004550586(plus_self3, plus_sarg3);
    ndefer_s = ret_val10;
    CGEN_d695831906(ndefer_self);
    SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
   }
   else {
    ndefer_self1 = self;
    plus_self4 = ((STR) &BROADC1443310781);
    mang_self2 = self;
    tp_self4 = global11;
    ret_val11 = ATTR(tp_self4,tp_at);
    mang_ob2 = ((OB) ret_val11);
    ret_val12 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    plus_sarg4 = ret_val12;
    ret_val13 = STR_ap2004550586(plus_self4, plus_sarg4);
    plus_self5 = ret_val13;
    plus_sarg5 = ((STR) &name3);
    ret_val14 = STR_ap2004550586(plus_self5, plus_sarg5);
    plus_self6 = ret_val14;
    plus_sarg6 = CGEN_e604348447(self, global11);
    ret_val15 = STR_ap2004550586(plus_self6, plus_sarg6);
    plus_self7 = ret_val15;
    plus_sarg7 = ((STR) &name76);
    ret_val16 = STR_ap2004550586(plus_self7, plus_sarg7);
    ndefer_s1 = ret_val16;
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
  }
  else {
   ndefer_self2 = self;
   plus_self8 = ((STR) &BROADC1443310857);
   mang_self3 = self;
   tp_self5 = global11;
   ret_val17 = ATTR(tp_self5,tp_at);
   mang_ob3 = ((OB) ret_val17);
   ret_val18 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
   plus_sarg8 = ret_val18;
   ret_val19 = STR_ap2004550586(plus_self8, plus_sarg8);
   plus_self9 = ret_val19;
   plus_sarg9 = ((STR) &name3);
   ret_val20 = STR_ap2004550586(plus_self9, plus_sarg9);
   plus_self10 = ret_val20;
   plus_sarg10 = CGEN_e604348447(self, global11);
   ret_val21 = STR_ap2004550586(plus_self10, plus_sarg10);
   plus_self11 = ret_val21;
   plus_sarg11 = ((STR) &name76);
   ret_val22 = STR_ap2004550586(plus_self11, plus_sarg11);
   ndefer_s2 = ret_val22;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c170352684(CGEN self, AM_CALL_ARG arg, ARG sig_arg, STR arg_str) {
 STR local11;
 dAM_EXPR g;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 STR plus_self6;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val9;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val10;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val11;
 STR r1;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self10;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val15;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val17;
 STR r2;
 CGEN ndefer_self2;
 STR ndefer_s2;
 BOOL L1;
 dMODE L2;
 dMODE L3;
 dAM_EXPR L4;
 dTP L5;
 dAM_EXPR L6;
 extern STR name58;
 extern STR name36;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 dAM_EXPR L12;
 dTP L13;
 extern STR name107;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 extern STR name108;
 extern STR name109;
 STR L19;
 INT L20;
 OB L21;
 INT L22;
 L2 = ATTR(arg,mode1);
 if ((*dMODE_814247358[TAG(L2)])(L2, ((dMODE) MODES_out_mode))) {
  L1 = TRUE;
 } else {
  L3 = ATTR(arg,mode1);
  L1 = (*dMODE_814247358[TAG(L3)])(L3, ((dMODE) MODES_inout_mode));
 }
 if (L1) {
  L4 = ATTR(arg,expr);
  L5 = (*dAM_EXPR_tprdTP[TAG(L4)])(L4);
  if ((*dTP_is1811059018[TAG(L5)])(L5)) {
   cast_self = self;
   L6 = ATTR(arg,expr);
   cast_dest_tp = (*dAM_EXPR_tprdTP[TAG(L6)])(L6);
   cast_src_tp = ATTR(sig_arg,tp);
   plus_self = ((STR) &name58);
   plus_sarg = arg_str;
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   cast_expr = ret_val;
   cast_only_boxing = FALSE;
   ret_val1 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
   local11 = ret_val1;
   ndefer_self = self;
   plus_self1 = CGEN_e1639504572(self, ATTR(arg,expr));
   plus_sarg1 = ((STR) &name36);
   ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val2;
   plus_sarg2 = local11;
   ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val3;
   plus_arg = ';';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L8 = 1;
   L10=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L8)*sizeof(CHAR);
   L9=ZALLOC_LEAF_BIG(L10);
   if (L9==NULL) FATAL("Unable to allocate more memory");
   memset(L9,0,L10);
   ((OB)L9)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L9)->header.destroyed=0;
#endif

   L7 = ((STR) L9);
   L7->asize = L8;
   r = L7;
   SARR((STR)r,0,create_c); 
   ;
   ret_val6 = r;
   ret_val5 = ret_val6;
   s = ret_val5;
   ret_val4 = STR_ap1077157958(plus_self3, s, TRUE);
   ndefer_s = ret_val4;
   CGEN_d695831906(ndefer_self);
   SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  }
  else {
   L12 = ATTR(arg,expr);
   L13 = (*dAM_EXPR_tprdTP[TAG(L12)])(L12);
   if ((*dTP_is1334578382[TAG(L13)])(L13)) {
    ndefer_self1 = self;
    plus_self4 = CGEN_e1639504572(self, ATTR(arg,expr));
    plus_sarg3 = ((STR) &name107);
    ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
    plus_self5 = ret_val7;
    plus_sarg4 = arg_str;
    ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val8;
    plus_arg1 = ';';
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
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
    r1 = L14;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val11 = r1;
    ret_val10 = ret_val11;
    s1 = ret_val10;
    ret_val9 = STR_ap1077157958(plus_self6, s1, TRUE);
    ndefer_s1 = ret_val9;
    CGEN_d695831906(ndefer_self1);
    SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
   }
   else {
    ndefer_self2 = self;
    plus_self7 = ((STR) &name108);
    plus_sarg5 = CGEN_e1639504572(self, ATTR(arg,expr));
    ret_val12 = STR_ap2004550586(plus_self7, plus_sarg5);
    plus_self8 = ret_val12;
    plus_sarg6 = ((STR) &name109);
    ret_val13 = STR_ap2004550586(plus_self8, plus_sarg6);
    plus_self9 = ret_val13;
    plus_sarg7 = arg_str;
    ret_val14 = STR_ap2004550586(plus_self9, plus_sarg7);
    plus_self10 = ret_val14;
    plus_arg2 = ';';
    str_self2 = plus_arg2;
    create_self2 = ((STR) NULL);
    create_c2 = str_self2;
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
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val17 = r2;
    ret_val16 = ret_val17;
    s2 = ret_val16;
    ret_val15 = STR_ap1077157958(plus_self10, s2, TRUE);
    ndefer_s2 = ret_val15;
    CGEN_d695831906(ndefer_self2);
    SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
   }
  }
  g = ATTR(arg,expr);
  if (g==NULL) {
   goto other368;
  } else
  switch (TAG(g)) {
   case AM_SHARED_EXPR_tag:
    CGEN_b2123071570(self, ((AM_SHARED_EXPR) g)); break;
   default: ;
   other368: ;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c657721823(CGEN self, AM_CALL_ARG arg, ARG sig_arg, STR arg_str) {
 STR local11;
 dAM_EXPR g;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 CGEN ndefer_self;
 STR ndefer_s;
 BOOL L1;
 BOOL L2;
 dTP L3;
 dAM_EXPR L4;
 dTP L5;
 dAM_EXPR L6;
 extern STR name58;
 extern STR name36;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 L3 = ATTR(sig_arg,tp);
 if ((*dTP_is1334578382[TAG(L3)])(L3)) {
  L2 = SYSOBEQ(((OB) ATTR(arg,mode1)),((OB) MODES_out_mode));
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L4 = ATTR(arg,expr);
  L5 = (*dAM_EXPR_tprdTP[TAG(L4)])(L4);
  L1 = (*dTP_is1811059018[TAG(L5)])(L5);
 } else {
  L1 = FALSE;
 }
 if (L1) {
  cast_self = self;
  L6 = ATTR(arg,expr);
  cast_dest_tp = (*dAM_EXPR_tprdTP[TAG(L6)])(L6);
  cast_src_tp = ATTR(sig_arg,tp);
  plus_self = ((STR) &name58);
  plus_sarg = arg_str;
  ret_val = STR_ap2004550586(plus_self, plus_sarg);
  cast_expr = ret_val;
  cast_only_boxing = FALSE;
  ret_val1 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
  local11 = ret_val1;
  ndefer_self = self;
  plus_self1 = CGEN_e1639504572(self, ATTR(arg,expr));
  plus_sarg1 = ((STR) &name36);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val2;
  plus_sarg2 = local11;
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_arg = ';';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L8 = 1;
  L10=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L8)*sizeof(CHAR);
  L9=ZALLOC_LEAF_BIG(L10);
  if (L9==NULL) FATAL("Unable to allocate more memory");
  memset(L9,0,L10);
  ((OB)L9)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L9)->header.destroyed=0;
#endif

  L7 = ((STR) L9);
  L7->asize = L8;
  r = L7;
  SARR((STR)r,0,create_c); 
  ;
  ret_val6 = r;
  ret_val5 = ret_val6;
  s = ret_val5;
  ret_val4 = STR_ap1077157958(plus_self3, s, TRUE);
  ndefer_s = ret_val4;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 g = ATTR(arg,expr);
 if (g==NULL) {
  goto other369;
 } else
 switch (TAG(g)) {
  case AM_SHARED_EXPR_tag:
   CGEN_b2123071570(self, ((AM_SHARED_EXPR) g)); break;
  default: ;
  other369: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_c922285894(CGEN self, dAM_CALL_EXPR ce, ARRAYSTR arg_list) {
 STR res = ((STR) NULL);
 AM_CALL_ARG arg = ((AM_CALL_ARG) NULL);
 ARG sig_arg = ((ARG) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 AM_CALL_ARG ca;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT i1 = INT_zero;
 AM_ITE1809135850 L51;
 INT L61 = INT_zero;
 AM_CALL_ARG e;
 ARRAYARG L71;
 ARG fe;
 INT L81 = INT_zero;
 INT i2 = INT_zero;
 INT L91 = INT_zero;
 INT L101 = INT_zero;
 INT i3 = INT_zero;
 INT L121 = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 INT str_self = INT_zero;
 STR ret_val3;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val4;
 FSTR str_self1;
 STR ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 INT L141_;
 INT L15;
 INT L161_;
 INT L131_,L131_m;
 INT L17;
 AM_CALL_ARG L181_;
 ARRAYARG L19;
 INT L201_;
 INT L22;
 ARG L231_;
 ARG L24;
 STR L251_;
 INT L27;
 INT L281_;
 INT L29;
 INT L301_;
 INT L261_,L261_m;
 AM_CALL_ARG L321_;
 AM_CALL_ARG L33;
 ARRAYARG L34;
 INT L351_;
 INT L36;
 ARG L371_;
 ARG L38;
 STR L391_;
 INT L401_c,L401_s;
 AM_CALL_ARG aL401_;
 INT L431_;
 AM_CALL_ARG L44;
 ARG L45;
 INT L421_br;
 ARG aL421_;
 INT L46;
 extern STR name35;
 extern STR arg21;
 BOOL L47;
 BOOL L481_;
 INT L491_;
 INT L52;
 INT L531_;
 INT L54;
 INT L551_;
 INT L501_,L501_m;
 AM_CALL_ARG L561_;
 AM_CALL_ARG L57;
 ARRAYARG L58;
 ARG L591_;
 ARG L60;
 STR L621_;
 switch (TAG(ce)) {
  case AM_EXT_CALL_EXPR_tag:
   {
    BOOL f_L131_ = TRUE;
    L11 = 1;
    L141_=ASIZE((AM_EXT_CALL_EXPR)ce); 
    L15 = L141_;
    L161_=INTMINUS(L15,1); 
    L21 = L161_;
    L131_=L11-1;L131_m=L21; 
    while (1) {
     if(L131_++>=L131_m)  goto after_loop; 
     i = L131_;
     L181_=(AM_CALL_ARG)ARR((AM_EXT_CALL_EXPR)ce,i); 
     ca = L181_;
     L19 = ATTR(ATTR(((AM_EXT_CALL_EXPR) ce),fun),args);
     L201_=INTMINUS(i,1); 
     L22 = L201_;
     L231_=(ARG)ARR((ARRAYARG)L19,L22); 
     L24 = L231_;
     L251_=(STR)ARR((ARRAYSTR)arg_list,i); 
     CGEN_c170352684(self, ca, L24, L251_);
    }
   }
   after_loop: ; break;
  case AM_ROU1916046290_tag:
   {
    BOOL f_L261_ = TRUE;
    while (1) {
     if (f_L261_) {
      f_L261_ = FALSE;
      L31 = 1;
      L281_=ASIZE((AM_ROU1916046290)ce); 
      L29 = L281_;
      L301_=INTMINUS(L29,1); 
      L41 = L301_;
      L261_=L31-1;L261_m=L41; 
     }
     if(L261_++>=L261_m)  goto after_loop1; 
     i1 = L261_;
     L321_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ce,i1); 
     L33 = L321_;
     L34 = ATTR(ATTR(((AM_ROU1916046290) ce),fun),args);
     L351_=INTMINUS(i1,1); 
     L36 = L351_;
     L371_=(ARG)ARR((ARRAYARG)L34,L36); 
     L38 = L371_;
     L391_=(STR)ARR((ARRAYSTR)arg_list,i1); 
     CGEN_c657721823(self, L33, L38, L391_);
    }
   }
   after_loop1: ; break;
  case AM_ITE1809135850_tag:
   {
    BOOL f_L401_ = TRUE;
    BOOL f_L421_ = TRUE;
    BOOL f_L431_ = TRUE;
    /* loop index variable */
    L121 = 0;
    L51 = ((AM_ITE1809135850) ce);
    L61 = 1;
    L81 = 1;
    L401_c=L61-1;L401_s=L51==0?0:ASIZE((AM_ITE1809135850)L51); 
    L431_=L81-1; 
    while (1) {
     if(++L401_c>=L401_s)  goto after_loop2; aL401_=ARR((AM_ITE1809135850)L51,L401_c); 
     e = aL401_;
     if (f_L421_) {
      f_L421_ = FALSE;
      L71 = ATTR(ATTR(((AM_ITE1809135850) ce),fun),args);
      L421_br=L71==NULL?0:ASIZE((ARRAYARG)L71); 
     }
     if(L121>=L421_br)  goto after_loop2; 
     aL421_=ARR((ARRAYARG)L71,L121); 
     fe = aL421_;
     L431_++; 
     i2 = L431_;
     mang_self = self;
     mang_ob = ((OB) ce);
     mang_ns = ((OB) ATTR(self,current_sig));
     ret_val = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
     plus_self = ret_val;
     plus_sarg = ((STR) &name35);
     ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val1;
     plus_sarg1 = ((STR) &arg21);
     ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val2;
     str_self = i2;
     clear_self = buf1;
     L47 = (clear_self==((FSTR) NULL));
     L481_=!(L47); 
     if (L481_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val4 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val4;
     str_self1 = buf1;
     ret_val5 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val3 = ret_val5;
     plus_sarg2 = ret_val3;
     ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
     CGEN_c657721823(self, e, fe, ret_val6);
     L491_=INTPLUS(L121,1); 
     L121 = L491_;
    }
   }
   after_loop2: ; break;
  case AM_BND260301329_tag:
   {
    BOOL f_L501_ = TRUE;
    while (1) {
     if (f_L501_) {
      f_L501_ = FALSE;
      L91 = 0;
      L531_=ASIZE((AM_BND260301329)ce); 
      L54 = L531_;
      L551_=INTMINUS(L54,1); 
      L101 = L551_;
      L501_=L91-1;L501_m=L101; 
     }
     if(L501_++>=L501_m)  goto after_loop3; 
     i3 = L501_;
     L561_=(AM_CALL_ARG)ARR((AM_BND260301329)ce,i3); 
     L57 = L561_;
     L58 = ATTR(ATTR(((AM_BND260301329) ce),br_tp),args);
     L591_=(ARG)ARR((ARRAYARG)L58,i3); 
     L60 = L591_;
     L621_=(STR)ARR((ARRAYSTR)arg_list,i3); 
     CGEN_c657721823(self, L57, L60, L621_);
    }
   }
   after_loop3: ; break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::caller_copy_out($AM_CALL_EXPR,ARRAY{STR})\n./Back/cgen.sa:4426:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_e844901491(CGEN self, AM_ITE1809135850 aice) {
 dAM_STMT aice_init;
 STR res;
 ARRAYSTR L11;
 STR res1;
 ARRAYSTR L21;
 STR res2;
 ARRAYSTR L31;
 STR res3;
 ARRAYSTR L41;
 ARRAYSTR in11;
 STR res4;
 ARRAYSTR L51;
 INT i = INT_zero;
 AM_ITE1809135850 L61;
 INT i1 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 CGEN comment_self;
 STR comment_com;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 SIG is_iter_self;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val6;
 INT L71 = INT_zero;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val7;
 CGEN expand_macro_sel;
 STR expand_macro_r;
 dAM_CALL_EXPR expand_macro_cal;
 SIG expand_macro_fun;
 ARRAYSTR expand_macro_arg;
 STR expand_macro_uni;
 STR ret_val8;
 CODE_FILE plus_self6;
 dSTR plus_s;
 CODE_FILE ret_val9;
 SIG is_iter_self2;
 BOOL ret_val10 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val12;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val13;
 INT L81 = INT_zero;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val14;
 CGEN expand_macro_sel1;
 STR expand_macro_r1;
 dAM_CALL_EXPR expand_macro_cal1;
 SIG expand_macro_fun1;
 ARRAYSTR expand_macro_arg1;
 STR expand_macro_uni1;
 STR ret_val15;
 CODE_FILE plus_self10;
 dSTR plus_s1;
 CODE_FILE ret_val16;
 SIG is_iter_self4;
 BOOL ret_val17 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val18 = BOOL_zero;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val19;
 CGEN expand_macro_sel2;
 STR expand_macro_r2;
 dAM_CALL_EXPR expand_macro_cal2;
 SIG expand_macro_fun2;
 ARRAYSTR expand_macro_arg2;
 STR expand_macro_uni2;
 STR ret_val20;
 CODE_FILE plus_self12;
 dSTR plus_s2;
 CODE_FILE ret_val21;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val22;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val23;
 INT L91 = INT_zero;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val24;
 CGEN expand_macro_sel3;
 STR expand_macro_r3;
 dAM_CALL_EXPR expand_macro_cal3;
 SIG expand_macro_fun3;
 ARRAYSTR expand_macro_arg3;
 STR expand_macro_uni3;
 STR ret_val25;
 CODE_FILE plus_self16;
 dSTR plus_s3;
 CODE_FILE ret_val26;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val27;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val28;
 INT L101 = INT_zero;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val29;
 CGEN expand_macro_sel4;
 STR expand_macro_r4;
 dAM_CALL_EXPR expand_macro_cal4;
 SIG expand_macro_fun4;
 ARRAYSTR expand_macro_arg4;
 STR expand_macro_uni4;
 STR ret_val30;
 CODE_FILE plus_self20;
 dSTR plus_s4;
 CODE_FILE ret_val31;
 INT L121 = INT_zero;
 STR plus_self21;
 CHAR plus_arg = CHAR_zero;
 STR ret_val32;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val33;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val34;
 STR r;
 STR plus_self22;
 STR plus_sarg16;
 STR ret_val35;
 STR plus_self23;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val36;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val37;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val38;
 STR r1;
 STR plus_self24;
 STR plus_sarg17;
 STR ret_val39;
 STR plus_self25;
 INT plus_arg2 = INT_zero;
 STR ret_val40;
 STR s2;
 INT str_self2 = INT_zero;
 STR ret_val41;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val42;
 FSTR str_self3;
 STR ret_val43;
 STR plus_self26;
 STR plus_sarg18;
 STR ret_val44;
 STR plus_self27;
 STR plus_sarg19;
 STR ret_val45;
 STR plus_self28;
 STR plus_sarg20;
 STR ret_val46;
 STR plus_self29;
 STR plus_sarg21;
 STR ret_val47;
 STR plus_self30;
 STR plus_sarg22;
 STR ret_val48;
 CGEN expand_macro_sel5;
 STR expand_macro_r5;
 dAM_CALL_EXPR expand_macro_cal5;
 SIG expand_macro_fun5;
 ARRAYSTR expand_macro_arg5;
 STR expand_macro_uni5;
 STR expand_macro_fra;
 STR ret_val49;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN expand_macro_sel6;
 STR expand_macro_r6;
 dAM_CALL_EXPR expand_macro_cal6;
 SIG expand_macro_fun6;
 ARRAYSTR expand_macro_arg6;
 STR expand_macro_uni6;
 STR expand_macro_fra1;
 STR ret_val50;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT L131 = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val51;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN mang_self1;
 OB mang_ob1;
 OB mang_ns1;
 STR ret_val52;
 INT str_self4 = INT_zero;
 STR ret_val53;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val54;
 FSTR str_self5;
 STR ret_val55;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val56;
 CGEN ndefer_self4;
 STR ndefer_s4;
 extern STR Initia354244688;
 extern STR name54;
 extern STR name79;
 BOOL L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 BOOL L20;
 BOOL L22;
 BOOL L231_;
 STR L24;
 INT L25;
 INT L261_;
 INT L27;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 extern STR name1;
 extern STR name10;
 STR L35;
 INT L341_br;
 STR aL341_;
 INT L361_;
 extern STR name1;
 BOOL L37;
 BOOL L381_;
 FSTR L39;
 dSTR L40;
 BOOL L42;
 BOOL L43;
 BOOL L44;
 BOOL L451_;
 BOOL L46;
 BOOL L47;
 BOOL L481_;
 STR L49;
 INT L50;
 INT L521_;
 INT L53;
 CHAR L541_;
 CHAR L55;
 BOOL L561_;
 BOOL L57;
 BOOL L581_;
 extern STR name1;
 extern STR name10;
 STR L60;
 INT L591_br;
 STR aL591_;
 INT L621_;
 extern STR name1;
 BOOL L63;
 BOOL L641_;
 FSTR L65;
 dSTR L66;
 BOOL L67;
 BOOL L68;
 BOOL L69;
 BOOL L70;
 BOOL L721_;
 STR L73;
 INT L74;
 INT L751_;
 INT L76;
 CHAR L771_;
 CHAR L78;
 BOOL L791_;
 BOOL L80;
 BOOL L821_;
 BOOL L83;
 BOOL L841_;
 extern STR INTd0;
 extern STR name1;
 BOOL L85;
 BOOL L861_;
 FSTR L87;
 dSTR L88;
 BOOL L89;
 BOOL L90;
 BOOL L921_;
 extern STR name1;
 extern STR name10;
 STR L94;
 INT L931_br;
 STR aL931_;
 INT L951_;
 extern STR name1;
 BOOL L96;
 BOOL L971_;
 FSTR L98;
 dSTR L99;
 BOOL L100;
 BOOL L102;
 BOOL L1031_;
 BOOL L104;
 BOOL L1051_;
 extern STR name1;
 extern STR name10;
 STR L107;
 INT L1061_br;
 STR aL1061_;
 INT L1081_;
 extern STR name1;
 BOOL L109;
 BOOL L1101_;
 FSTR L111;
 dSTR L112;
 BOOL L113;
 BOOL L1141_;
 extern STR name1;
 INT L1151_br;
 INT rL1151_;
 INT L116;
 STR L1171_;
 STR L118;
 CHAR L1191_;
 CHAR L120;
 BOOL L1221_;
 STR L123;
 INT L124;
 OB L125;
 INT L126;
 STR L1281_;
 STR L129;
 INT L130;
 OB L132;
 INT L133;
 extern STR line3;
 BOOL L135;
 BOOL L1361_;
 extern STR name71;
 extern STR name73;
 STR L1371_;
 extern STR name10;
 INT L1381_;
 BOOL L139;
 BOOL L140;
 BOOL L1411_;
 extern STR d0;
 INT L1421_br;
 INT rL1421_;
 INT L143;
 BOOL L1441_;
 STR L145;
 extern STR self6;
 AM_CALL_ARG L1461_;
 AM_CALL_ARG L147;
 STR L148;
 extern STR name16;
 BOOL L149;
 ARRAYBOOL L150;
 INT L1511_;
 INT L152;
 BOOL L1531_;
 BOOL L154;
 BOOL L1551_;
 extern STR arg3;
 BOOL L156;
 BOOL L1571_;
 extern STR name36;
 ARRAYARG L158;
 INT L1591_;
 INT L160;
 ARG L1611_;
 ARG L162;
 AM_CALL_ARG L1631_;
 AM_CALL_ARG L164;
 dAM_EXPR L165;
 AM_CALL_ARG L1661_;
 AM_CALL_ARG L167;
 extern STR name16;
 INT L1681_;
 comment_self = self;
 plus_self = ((STR) &Initia354244688);
 plus_sarg = ATTR(ATTR(aice,fun),str);
 ret_val = STR_ap2004550586(plus_self, plus_sarg);
 comment_com = ret_val;
 if (ATTR(comment_self,pretty)) {
  ndefer_self = comment_self;
  plus_self1 = ((STR) &name54);
  plus_sarg1 = comment_com;
  ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val1;
  plus_sarg2 = ((STR) &name79);
  ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
  ndefer_s = ret_val2;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 CGEN_e494460490(self, ATTR(aice,init));
 aice_init = ATTR(aice,init);
 SATTR(aice,init,((dAM_STMT) NULL));
 if (SIG_is418491101(ATTR(aice,fun))) {
  SATTR(aice,arg_list,CGEN_e2096447517(self, aice));
  L16 = ATTR(ATTR(self,prog),distributed);
  L171_=!(L16); 
  if (L171_) {
   L18 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),var)==((ARRAYSTR) NULL));
   L191_=!(L18); 
   L15 = L191_;
  } else {
   L15 = FALSE;
  }
  if (L15) {
   is_iter_self = ATTR(self,current_sig);
   is_iter_self1 = ATTR(is_iter_self,name1);
   L22 = (is_iter_self1.str==((STR) NULL));
   L231_=!(L22); 
   if (L231_) {
    L24 = is_iter_self1.str;
    L25 = STR_sizerINT(is_iter_self1.str);
    L261_=INTMINUS(L25,1); 
    L27 = L261_;
    L281_=ARR((STR)L24,L27); 
    L29 = L281_;
    L301_=L29=='!'; 
    L20 = L301_;
   } else {
    L20 = FALSE;
   }
   ret_val4 = L20;
   ret_val3 = ret_val4;
   L32 = ret_val3;
   L331_=!(L32); 
   L14 = L331_;
  } else {
   L14 = FALSE;
  }
  if (L14) {
   res = ((STR) &name1);
   {
    BOOL f_L341_ = TRUE;
    /* loop index variable */
    L71 = 0;
    while (1) {
     plus_self3 = res;
     plus_sarg3 = CGEN_eolrSTR(self);
     ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self4 = ret_val5;
     plus_sarg4 = ((STR) &name10);
     ret_val6 = STR_ap2004550586(plus_self4, plus_sarg4);
     plus_self5 = ret_val6;
     if (f_L341_) {
      f_L341_ = FALSE;
      L11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),var);
      L341_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
     }
     if(L71>=L341_br)  goto after_loop; 
     aL341_=ARR((ARRAYSTR)L11,L71); 
     plus_sarg5 = aL341_;
     ret_val7 = STR_ap2004550586(plus_self5, plus_sarg5);
     res = ret_val7;
     L361_=INTPLUS(L71,1); 
     L71 = L361_;
    }
   }
   after_loop: ;
   plus_self6 = ATTR(self,code_c);
   expand_macro_sel = self;
   expand_macro_r = res;
   expand_macro_cal = ((dAM_CALL_EXPR) aice);
   expand_macro_fun = ATTR(aice,fun);
   expand_macro_arg = ATTR(aice,arg_list);
   expand_macro_uni = ATTR(aice,uniq);
   ret_val8 = CGEN_e1606764211(expand_macro_sel, expand_macro_r, expand_macro_cal, expand_macro_fun, expand_macro_arg, expand_macro_uni, ((STR) &name1), ((STR) NULL));
   plus_s = ((dSTR) ret_val8);
   L37 = (plus_s==((dSTR) NULL));
   L381_=!(L37); 
   if (L381_) {
    L39 = ATTR(plus_self6,ntext);
    L40 = plus_s;
    SATTR(plus_self6,ntext,FSTR_p1752847026(L39, (*dSTR_strrSTR[TAG(L40)])(L40)));
   }
   ret_val9 = plus_self6;
   SATTR(self,code_c,ret_val9);
  }
  if (ATTR(ATTR(self,prog),distributed)) {
   L44 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),f_var)==((ARRAYSTR) NULL));
   L451_=!(L44); 
   L43 = L451_;
  } else {
   L43 = FALSE;
  }
  if (L43) {
   is_iter_self2 = ATTR(self,current_sig);
   is_iter_self3 = ATTR(is_iter_self2,name1);
   L47 = (is_iter_self3.str==((STR) NULL));
   L481_=!(L47); 
   if (L481_) {
    L49 = is_iter_self3.str;
    L50 = STR_sizerINT(is_iter_self3.str);
    L521_=INTMINUS(L50,1); 
    L53 = L521_;
    L541_=ARR((STR)L49,L53); 
    L55 = L541_;
    L561_=L55=='!'; 
    L46 = L561_;
   } else {
    L46 = FALSE;
   }
   ret_val11 = L46;
   ret_val10 = ret_val11;
   L57 = ret_val10;
   L581_=!(L57); 
   L42 = L581_;
  } else {
   L42 = FALSE;
  }
  if (L42) {
   res1 = ((STR) &name1);
   {
    BOOL f_L591_ = TRUE;
    /* loop index variable */
    L81 = 0;
    while (1) {
     plus_self7 = res1;
     plus_sarg6 = CGEN_eolrSTR(self);
     ret_val12 = STR_ap2004550586(plus_self7, plus_sarg6);
     plus_self8 = ret_val12;
     plus_sarg7 = ((STR) &name10);
     ret_val13 = STR_ap2004550586(plus_self8, plus_sarg7);
     plus_self9 = ret_val13;
     if (f_L591_) {
      f_L591_ = FALSE;
      L21 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_var);
      L591_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
     }
     if(L81>=L591_br)  goto after_loop1; 
     aL591_=ARR((ARRAYSTR)L21,L81); 
     plus_sarg8 = aL591_;
     ret_val14 = STR_ap2004550586(plus_self9, plus_sarg8);
     res1 = ret_val14;
     L621_=INTPLUS(L81,1); 
     L81 = L621_;
    }
   }
   after_loop1: ;
   plus_self10 = ATTR(self,code_c);
   expand_macro_sel1 = self;
   expand_macro_r1 = res1;
   expand_macro_cal1 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun1 = ATTR(aice,fun);
   expand_macro_arg1 = ATTR(aice,arg_list);
   expand_macro_uni1 = ATTR(aice,uniq);
   ret_val15 = CGEN_e1606764211(expand_macro_sel1, expand_macro_r1, expand_macro_cal1, expand_macro_fun1, expand_macro_arg1, expand_macro_uni1, ((STR) &name1), ((STR) NULL));
   plus_s1 = ((dSTR) ret_val15);
   L63 = (plus_s1==((dSTR) NULL));
   L641_=!(L63); 
   if (L641_) {
    L65 = ATTR(plus_self10,ntext);
    L66 = plus_s1;
    SATTR(plus_self10,ntext,FSTR_p1752847026(L65, (*dSTR_strrSTR[TAG(L66)])(L66)));
   }
   ret_val16 = plus_self10;
   SATTR(self,code_c,ret_val16);
  }
  if (ATTR(ATTR(ATTR(aice,fun),builtin_info),use_index)) {
   is_iter_self4 = ATTR(self,current_sig);
   is_iter_self5 = ATTR(is_iter_self4,name1);
   L70 = (is_iter_self5.str==((STR) NULL));
   L721_=!(L70); 
   if (L721_) {
    L73 = is_iter_self5.str;
    L74 = STR_sizerINT(is_iter_self5.str);
    L751_=INTMINUS(L74,1); 
    L76 = L751_;
    L771_=ARR((STR)L73,L76); 
    L78 = L771_;
    L791_=L78=='!'; 
    L69 = L791_;
   } else {
    L69 = FALSE;
   }
   ret_val18 = L69;
   ret_val17 = ret_val18;
   L80 = ret_val17;
   L821_=!(L80); 
   L68 = L821_;
  } else {
   L68 = FALSE;
  }
  if (L68) {
   L83 = ATTR(aice,use_loop_index);
   L841_=!(L83); 
   L67 = L841_;
  } else {
   L67 = FALSE;
  }
  if (L67) {
   plus_self12 = ATTR(self,code_c);
   expand_macro_sel2 = self;
   plus_self11 = CGEN_eolrSTR(self);
   plus_sarg9 = ((STR) &INTd0);
   ret_val19 = STR_ap2004550586(plus_self11, plus_sarg9);
   expand_macro_r2 = ret_val19;
   expand_macro_cal2 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun2 = ATTR(aice,fun);
   expand_macro_arg2 = ((ARRAYSTR) NULL);
   expand_macro_uni2 = ATTR(aice,uniq);
   ret_val20 = CGEN_e1606764211(expand_macro_sel2, expand_macro_r2, expand_macro_cal2, expand_macro_fun2, expand_macro_arg2, expand_macro_uni2, ((STR) &name1), ((STR) NULL));
   plus_s2 = ((dSTR) ret_val20);
   L85 = (plus_s2==((dSTR) NULL));
   L861_=!(L85); 
   if (L861_) {
    L87 = ATTR(plus_self12,ntext);
    L88 = plus_s2;
    SATTR(plus_self12,ntext,FSTR_p1752847026(L87, (*dSTR_strrSTR[TAG(L88)])(L88)));
   }
   ret_val21 = plus_self12;
   SATTR(self,code_c,ret_val21);
  }
  if (ATTR(ATTR(self,prog),distributed)) {
   L90 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),f_temp)==((ARRAYSTR) NULL));
   L921_=!(L90); 
   L89 = L921_;
  } else {
   L89 = FALSE;
  }
  if (L89) {
   res2 = ((STR) &name1);
   {
    BOOL f_L931_ = TRUE;
    /* loop index variable */
    L91 = 0;
    while (1) {
     plus_self13 = res2;
     plus_sarg10 = CGEN_eolrSTR(self);
     ret_val22 = STR_ap2004550586(plus_self13, plus_sarg10);
     plus_self14 = ret_val22;
     plus_sarg11 = ((STR) &name10);
     ret_val23 = STR_ap2004550586(plus_self14, plus_sarg11);
     plus_self15 = ret_val23;
     if (f_L931_) {
      f_L931_ = FALSE;
      L31 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_temp);
      L931_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
     }
     if(L91>=L931_br)  goto after_loop2; 
     aL931_=ARR((ARRAYSTR)L31,L91); 
     plus_sarg12 = aL931_;
     ret_val24 = STR_ap2004550586(plus_self15, plus_sarg12);
     res2 = ret_val24;
     L951_=INTPLUS(L91,1); 
     L91 = L951_;
    }
   }
   after_loop2: ;
   plus_self16 = ATTR(self,code_c);
   expand_macro_sel3 = self;
   expand_macro_r3 = res2;
   expand_macro_cal3 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun3 = ATTR(aice,fun);
   expand_macro_arg3 = ATTR(aice,arg_list);
   expand_macro_uni3 = ATTR(aice,uniq);
   ret_val25 = CGEN_e1606764211(expand_macro_sel3, expand_macro_r3, expand_macro_cal3, expand_macro_fun3, expand_macro_arg3, expand_macro_uni3, ((STR) &name1), ((STR) NULL));
   plus_s3 = ((dSTR) ret_val25);
   L96 = (plus_s3==((dSTR) NULL));
   L971_=!(L96); 
   if (L971_) {
    L98 = ATTR(plus_self16,ntext);
    L99 = plus_s3;
    SATTR(plus_self16,ntext,FSTR_p1752847026(L98, (*dSTR_strrSTR[TAG(L99)])(L99)));
   }
   ret_val26 = plus_self16;
   SATTR(self,code_c,ret_val26);
  }
  L102 = ATTR(ATTR(self,prog),distributed);
  L1031_=!(L102); 
  if (L1031_) {
   L104 = (ATTR(ATTR(ATTR(aice,fun),builtin_info),temp1)==((ARRAYSTR) NULL));
   L1051_=!(L104); 
   L100 = L1051_;
  } else {
   L100 = FALSE;
  }
  if (L100) {
   res3 = ((STR) &name1);
   {
    BOOL f_L1061_ = TRUE;
    /* loop index variable */
    L101 = 0;
    while (1) {
     plus_self17 = res3;
     plus_sarg13 = CGEN_eolrSTR(self);
     ret_val27 = STR_ap2004550586(plus_self17, plus_sarg13);
     plus_self18 = ret_val27;
     plus_sarg14 = ((STR) &name10);
     ret_val28 = STR_ap2004550586(plus_self18, plus_sarg14);
     plus_self19 = ret_val28;
     if (f_L1061_) {
      f_L1061_ = FALSE;
      L41 = ATTR(ATTR(ATTR(aice,fun),builtin_info),temp1);
      L1061_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
     }
     if(L101>=L1061_br)  goto after_loop3; 
     aL1061_=ARR((ARRAYSTR)L41,L101); 
     plus_sarg15 = aL1061_;
     ret_val29 = STR_ap2004550586(plus_self19, plus_sarg15);
     res3 = ret_val29;
     L1081_=INTPLUS(L101,1); 
     L101 = L1081_;
    }
   }
   after_loop3: ;
   plus_self20 = ATTR(self,code_c);
   expand_macro_sel4 = self;
   expand_macro_r4 = res3;
   expand_macro_cal4 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun4 = ATTR(aice,fun);
   expand_macro_arg4 = ATTR(aice,arg_list);
   expand_macro_uni4 = ATTR(aice,uniq);
   ret_val30 = CGEN_e1606764211(expand_macro_sel4, expand_macro_r4, expand_macro_cal4, expand_macro_fun4, expand_macro_arg4, expand_macro_uni4, ((STR) &name1), ((STR) NULL));
   plus_s4 = ((dSTR) ret_val30);
   L109 = (plus_s4==((dSTR) NULL));
   L1101_=!(L109); 
   if (L1101_) {
    L111 = ATTR(plus_self20,ntext);
    L112 = plus_s4;
    SATTR(plus_self20,ntext,FSTR_p1752847026(L111, (*dSTR_strrSTR[TAG(L112)])(L112)));
   }
   ret_val31 = plus_self20;
   SATTR(self,code_c,ret_val31);
  }
  in11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),init);
  if (ATTR(ATTR(self,prog),distributed)) {
   in11 = ATTR(ATTR(ATTR(aice,fun),builtin_info),f_init);
  }
  L113 = (in11==((ARRAYSTR) NULL));
  L1141_=!(L113); 
  if (L1141_) {
   res4 = ((STR) &name1);
   {
    BOOL f_L1151_ = TRUE;
    /* loop index variable */
    L121 = 0;
    L51 = in11;
    L1151_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
    while (1) {
     if(L121>=L1151_br)  goto after_loop4; 
     rL1151_=L121; 
     i = rL1151_;
     L1171_=(STR)ARR((ARRAYSTR)in11,i); 
     L118 = L1171_;
     L1191_=ARR((STR)L118,0); 
     L120 = L1191_;
     L1221_=L120=='#'; 
     if (L1221_) {
      plus_self21 = res4;
      plus_arg = '\n';
      str_self = plus_arg;
      create_self = ((STR) NULL);
      create_c = str_self;
      L124 = 1;
      L126=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L124)*sizeof(CHAR);
      L125=ZALLOC_LEAF_BIG(L126);
      if (L125==NULL) FATAL("Unable to allocate more memory");
      memset(L125,0,L126);
      ((OB)L125)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L125)->header.destroyed=0;
#endif

      L123 = ((STR) L125);
      L123->asize = L124;
      r = L123;
      SARR((STR)r,0,create_c); 
      ;
      ret_val34 = r;
      ret_val33 = ret_val34;
      s = ret_val33;
      ret_val32 = STR_ap1077157958(plus_self21, s, TRUE);
      plus_self22 = ret_val32;
      L1281_=(STR)ARR((ARRAYSTR)in11,i); 
      plus_sarg16 = L1281_;
      ret_val35 = STR_ap2004550586(plus_self22, plus_sarg16);
      plus_self23 = ret_val35;
      plus_arg1 = '\n';
      str_self1 = plus_arg1;
      create_self1 = ((STR) NULL);
      create_c1 = str_self1;
      L130 = 1;
      L133=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L130)*sizeof(CHAR);
      L132=ZALLOC_LEAF_BIG(L133);
      if (L132==NULL) FATAL("Unable to allocate more memory");
      memset(L132,0,L133);
      ((OB)L132)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L132)->header.destroyed=0;
#endif

      L129 = ((STR) L132);
      L129->asize = L130;
      r1 = L129;
      SARR((STR)r1,0,create_c1); 
      ;
      ret_val38 = r1;
      ret_val37 = ret_val38;
      s1 = ret_val37;
      ret_val36 = STR_ap1077157958(plus_self23, s1, TRUE);
      res4 = ret_val36;
      if (ATTR(self,debug1)) {
       plus_self24 = res4;
       plus_sarg17 = ((STR) &line3);
       ret_val39 = STR_ap2004550586(plus_self24, plus_sarg17);
       plus_self25 = ret_val39;
       plus_arg2 = ATTR(self,last_lineno);
       str_self2 = plus_arg2;
       clear_self = buf1;
       L135 = (clear_self==((FSTR) NULL));
       L1361_=!(L135); 
       if (L1361_) {
        SATTR(clear_self,loc,0);
       }
       str_in_self = str_self2;
       str_in_s = buf1;
       ret_val42 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
       buf1 = ret_val42;
       str_self3 = buf1;
       ret_val43 = STR_fr1097270334(((STR) NULL), str_self3);
       ret_val41 = ret_val43;
       s2 = ret_val41;
       ret_val40 = STR_ap1077157958(plus_self25, s2, TRUE);
       plus_self26 = ret_val40;
       plus_sarg18 = ((STR) &name71);
       ret_val44 = STR_ap2004550586(plus_self26, plus_sarg18);
       plus_self27 = ret_val44;
       plus_sarg19 = ATTR(self,last_file);
       ret_val45 = STR_ap2004550586(plus_self27, plus_sarg19);
       plus_self28 = ret_val45;
       plus_sarg20 = ((STR) &name73);
       ret_val46 = STR_ap2004550586(plus_self28, plus_sarg20);
       res4 = ret_val46;
      }
     }
     else {
      plus_self29 = res4;
      L1371_=(STR)ARR((ARRAYSTR)in11,i); 
      plus_sarg21 = L1371_;
      ret_val47 = STR_ap2004550586(plus_self29, plus_sarg21);
      plus_self30 = ret_val47;
      plus_sarg22 = ((STR) &name10);
      ret_val48 = STR_ap2004550586(plus_self30, plus_sarg22);
      res4 = ret_val48;
     }
     L1381_=INTPLUS(L121,1); 
     L121 = L1381_;
    }
   }
   after_loop4: ;
   ndefer_self1 = self;
   expand_macro_sel5 = self;
   expand_macro_r5 = res4;
   expand_macro_cal5 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun5 = ATTR(aice,fun);
   expand_macro_arg5 = ATTR(aice,arg_list);
   expand_macro_uni5 = ATTR(aice,uniq);
   expand_macro_fra = CGEN_i87475608(self);
   ret_val49 = CGEN_e1606764211(expand_macro_sel5, expand_macro_r5, expand_macro_cal5, expand_macro_fun5, expand_macro_arg5, expand_macro_uni5, expand_macro_fra, ((STR) NULL));
   ndefer_s1 = ret_val49;
   CGEN_d695831906(ndefer_self1);
   SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  }
  if (ATTR(ATTR(ATTR(aice,fun),builtin_info),use_index)) {
   L140 = ATTR(aice,use_loop_index);
   L1411_=!(L140); 
   L139 = L1411_;
  } else {
   L139 = FALSE;
  }
  if (L139) {
   ndefer_self2 = self;
   expand_macro_sel6 = self;
   expand_macro_r6 = ((STR) &d0);
   expand_macro_cal6 = ((dAM_CALL_EXPR) aice);
   expand_macro_fun6 = ATTR(aice,fun);
   expand_macro_arg6 = ((ARRAYSTR) NULL);
   expand_macro_uni6 = ATTR(aice,uniq);
   expand_macro_fra1 = CGEN_i87475608(self);
   ret_val50 = CGEN_e1606764211(expand_macro_sel6, expand_macro_r6, expand_macro_cal6, expand_macro_fun6, expand_macro_arg6, expand_macro_uni6, expand_macro_fra1, ((STR) NULL));
   ndefer_s2 = ret_val50;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  }
 }
 else {
  {
   BOOL f_L1421_ = TRUE;
   /* loop index variable */
   L131 = 0;
   L61 = aice;
   L1421_br=L61==NULL?0:ASIZE((AM_ITE1809135850)L61); 
   while (1) {
    if(L131>=L1421_br)  goto after_loop5; 
    rL1421_=L131; 
    i1 = rL1421_;
    L1441_=(i1)==(0); 
    if (L1441_) {
     ndefer_self3 = self;
     mang_self = self;
     mang_ob = ((OB) aice);
     mang_ns = ((OB) ATTR(self,current_sig));
     ret_val51 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
     L145 = ret_val51;
     L1461_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i1); 
     L147=L1461_;
     L148 = CGEN_e1639504572(self, ATTR(L147,expr));
     ndefer_s3 = STR_ap1417971546(L145, ((STR) &self6), L148, ((STR) &name16));
     CGEN_d695831906(ndefer_self3);
     SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
    }
    else {
     if ((ATTR(ATTR(aice,fun),hot)==((ARRAYBOOL) NULL))) {
      L149 = TRUE;
     } else {
      L150 = ATTR(ATTR(aice,fun),hot);
      L1511_=INTMINUS(i1,1); 
      L152 = L1511_;
      L1531_=ARR((ARRAYBOOL)L150,L152); 
      L154 = L1531_;
      L1551_=!(L154); 
      L149 = L1551_;
     }
     if (L149) {
      ndefer_self4 = self;
      mang_self1 = self;
      mang_ob1 = ((OB) aice);
      mang_ns1 = ((OB) ATTR(self,current_sig));
      ret_val52 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, mang_ns1);
      str_self4 = i1;
      clear_self1 = buf2;
      L156 = (clear_self1==((FSTR) NULL));
      L1571_=!(L156); 
      if (L1571_) {
       SATTR(clear_self1,loc,0);
      }
      str_in_self1 = str_self4;
      str_in_s1 = buf2;
      ret_val54 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
      buf2 = ret_val54;
      str_self5 = buf2;
      ret_val55 = STR_fr1097270334(((STR) NULL), str_self5);
      ret_val53 = ret_val55;
      cast_self = self;
      L158 = ATTR(ATTR(aice,fun),args);
      L1591_=INTMINUS(i1,1); 
      L160 = L1591_;
      L1611_=(ARG)ARR((ARRAYARG)L158,L160); 
      L162=L1611_;
      cast_dest_tp = ATTR(L162,tp);
      L1631_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i1); 
      L164=L1631_;
      L165 = ATTR(L164,expr);
      cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L165)])(L165);
      L1661_=(AM_CALL_ARG)ARR((AM_ITE1809135850)aice,i1); 
      L167=L1661_;
      cast_expr = CGEN_e1639504572(self, ATTR(L167,expr));
      cast_only_boxing = FALSE;
      ret_val56 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
      ndefer_s4 = STR_ap1925291910(ret_val52, ((STR) &arg3), ret_val53, ((STR) &name36), ret_val56, ((STR) &name16));
      CGEN_d695831906(ndefer_self4);
      SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
     }
    }
    L1681_=INTPLUS(L131,1); 
    L131 = L1681_;
   }
  }
  after_loop5: ;
 }
 SATTR(aice,init,aice_init);
}
