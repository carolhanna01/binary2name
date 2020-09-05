#include "sather.h"

/* Layouts */

typedef struct dAM_CALL_EXPR_struct {
 OB_HEADER header;
 } *dAM_CALL_EXPR;

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

typedef struct ARRAYA1575632848_struct {/* layout for ARRAY{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *ARRAYA1575632848;

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

typedef struct BOUND_1677815802_struct {/* layout for BOUND_OBJECT_LAYOUT */
 OB_HEADER header;
 struct AM_BND1124877163_struct *e;
 STR str;
 } *BOUND_1677815802;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

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

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

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

typedef struct AM_AT_EXPR_struct {/* layout for AM_AT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *e;
 struct SFILE_ID_struct source1;
 } *AM_AT_EXPR;

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

typedef struct AM_BND1640868266_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND1640868266_frame;

typedef struct AM_BND2071008937_frame_struct {
 INT state;
 AM_BND1124877163 self;/* Formal argument: self */
 AM_CALL_ARG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_BND2071008937_frame;

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

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

typedef struct AM_ROU176159907_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 AM_CALL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU176159907_frame;

typedef struct AM_ROU2113525054_frame_struct {
 INT state;
 AM_ROU1916046290 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU2113525054_frame;

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

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYA702716235_frame_struct {
 INT state;
 ARRAYA1575632848 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA702716235_frame;

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

typedef struct ARRAYI486979141_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI486979141_frame;

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

typedef struct FMAPSTRINT_struct {/* layout for FMAP{STR,INT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRINT_struct arr_part[1];
 } *FMAPSTRINT;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

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

extern FMAPAM1357596931 BOUND_2051717332;
extern INOUT_MODE MODES_inout_mode;
extern INT INT_asize;
extern IN_MODE MODES_in_mode;
extern OUT_MODE MODES_out_mode;
extern TP_CLASS TP_BUILTIN_zone;

/* Function declarations */


extern RETURNED_CONST AM_CALL_ARG (**dAM_CA967858872)(dAM_CALL_EXPR, INT);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_2144735300)(dMODE);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is124163553)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is1811059018)(dTP);

extern RETURNED_CONST BOOL (**dTP_is418055720)(dTP);

extern RETURNED_CONST BOOL (**dTP_is735947028)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_BND1124877163 FLISTA273121893(FLISTA273121893_frame);
AM_CALL_ARG AM_ROU176159907(AM_ROU176159907_frame);
AM_LOCAL_EXPR CGEN_d1823718364(CGEN, dTP);
AM_LOCAL_EXPR CGEN_d609641833(CGEN, dTP);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
ARG ARRAYA827580689(ARRAYA827580689_frame);
ARRAYSTR CGEN_e1933193154(CGEN, dAM_CALL_EXPR);
BOOL AM_BND89426869(AM_BND1124877163, AM_BND1124877163);
BOOL CGEN_i1006303229(CGEN, AM_AT_EXPR);
BOOL CGEN_i1610952633(CGEN, dTP);
BOOL SIG_is418491101(SIG);
BOUND_1677815802 FMAPAM661174734(FMAPAM1357596931, AM_BND1124877163);
FLISTA1893461511 FLISTA80425519(FLISTA1893461511, AM_BND1124877163);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FMAPAM1357596931 FMAPAM1106357779(FMAPAM1357596931, AM_BND1124877163, BOUND_1677815802);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FMAPdOBNAMESPACE FMAPdO1199466663(FMAPdOBNAMESPACE, OB, NAMESPACE);
FSETAM1032778315 FSETAM1990484536(FSETAM1032778315, AM_BND1124877163);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT AM_BND1640868266(AM_BND1640868266_frame);
INT AM_ROU2113525054(AM_ROU2113525054_frame);
INT ARRAYA1077419174(ARRAYA1077419174_frame);
INT ARRAYA702716235(ARRAYA702716235_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT ARRAYS1828691793(ARRAYS1828691793_frame);
INT INT_pow_INTrINT(INT, INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
NAMESPACE FMAPdO790518483(FMAPdOBNAMESPACE, OB);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CGEN_d1425664957(CGEN, dTP);
STR CGEN_d1915593494(CGEN, dTP, dTP, STR, BOOL, dMODE);
STR CGEN_e1606764211(CGEN, STR, dAM_CALL_EXPR, SIG, ARRAYSTR, STR, STR, STR);
STR CGEN_e1639504572(CGEN, dAM_EXPR);
STR CGEN_e1835933683(CGEN, SIG, dAM_EXPR, ARRAYSTR);
STR CGEN_e236145586(CGEN, AM_LOCAL_EXPR);
STR CGEN_e283387820(CGEN, AM_AT_EXPR);
STR CGEN_e409242292(CGEN, AM_BND1124877163, BOOL, FLISTSTR*);
STR CGEN_e712006663(CGEN, AM_AT_EXPR, AM_ROU1916046290, BOOL);
STR CGEN_e969629179(CGEN, AM_AT_EXPR);
STR CGEN_eolrSTR(CGEN);
STR CGEN_p1740273317(CGEN, SIG, dAM_CALL_EXPR, ARRAYSTR);
STR CGEN_p1779648146(CGEN, SIG, ARRAYBOOL, dAM_EXPR, ARRAYSTR);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR MANGLE119219986(MANGLE, OB, OB);
STR NAMESP180237388(NAMESPACE, STR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap1417971546(STR, STR, STR, STR);
STR STR_ap1693864410(STR, STR, STR);
STR STR_ap1925291910(STR, STR, STR, STR, STR, STR);
STR STR_ap2004550586(STR, STR);
STR STR_ap44172742(STR, STR, STR, STR, STR);
STR STR_fr1097270334(STR, FSTR);
TP_ROUT TP_ROU52430373(TP_ROUT, ARRAYARG, dTP, PROG);
void AM_BND2071008937(AM_BND2071008937_frame);
void ARRAYI486979141(ARRAYI486979141_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void CGEN_d695831906(CGEN);
void CGEN_g136295071(CGEN, AM_BND1124877163, FLISTSTR, STR, STR);
void CODE_F1291349628(CODE_FILE, SIG);
void CODE_F525364269(CODE_FILE, dLAYOUT);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_message_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR CGEN_d1823718364(CGEN self, dTP t) {
 AM_LOCAL_EXPR ret_val;
 AM_LOCAL_EXPR am;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val1;
 AM_LOCAL_EXPR r;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val2;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val3;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val4;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val5;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val6;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val7;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 AM_LOCAL_EXPR L1;
 OB L2;
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
 dSTR L17;
 OB L18;
 BOOL L19;
 BOOL L201_;
 FSTR L21;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 dSTR L27;
 OB L28;
 FSTR L29;
 dSTR L30;
 create_self = ((AM_LOCAL_EXPR) NULL);
 create_source = SFILE_ID_zero;
 L2=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_LOCAL_EXPR_tag;
 L1 = ((AM_LOCAL_EXPR) L2);
 r = L1;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 am = ret_val1;
 SATTR(am,tp_at,t);
 SATTR(am,not_in_frame,TRUE);
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val2 = plus_self;
 plus_self1 = ret_val2;
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
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 mang_self = self;
 mang_ob = ((OB) t);
 ret_val4 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val4);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L18=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 memset(L18,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L18)->header.tag=CHAR_tag;
 L17 = (dSTR)((CHAR_boxed) L18);
 ((CHAR_boxed) L17)->immutable_part = ' ';
 plus_s3 = L17;
 L19 = (plus_s3==((dSTR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L21 = ATTR(plus_self3,ntext);
  L22 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L21, (*dSTR_strrSTR[TAG(L22)])(L22)));
 }
 ret_val6 = plus_self3;
 plus_self4 = ret_val6;
 plus_s4 = ((dSTR) CGEN_e236145586(self, am));
 L23 = (plus_s4==((dSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(plus_self4,ntext);
  L26 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 }
 ret_val7 = plus_self4;
 plus_self5 = ret_val7;
 L28=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 memset(L28,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L28)->header.tag=CHAR_tag;
 L27 = (dSTR)((CHAR_boxed) L28);
 ((CHAR_boxed) L27)->immutable_part = ';';
 plus_s5 = L27;
 L29 = ATTR(plus_self5,ntext);
 L30 = plus_s5;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L29, (*dSTR_strrSTR[TAG(L30)])(L30)));
 ret_val = am;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR CGEN_d609641833(CGEN self, dTP t) {
 AM_LOCAL_EXPR ret_val;
 AM_LOCAL_EXPR am;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val1;
 AM_LOCAL_EXPR r;
 CODE_FILE plus_self;
 dSTR plus_s;
 CODE_FILE ret_val2;
 CODE_FILE plus_self1;
 dSTR plus_s1;
 CODE_FILE ret_val3;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val4;
 CODE_FILE plus_self2;
 dSTR plus_s2;
 CODE_FILE ret_val5;
 CODE_FILE plus_self3;
 dSTR plus_s3;
 CODE_FILE ret_val6;
 CODE_FILE plus_self4;
 dSTR plus_s4;
 CODE_FILE ret_val7;
 CODE_FILE plus_self5;
 dSTR plus_s5;
 AM_LOCAL_EXPR L1;
 OB L2;
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
 extern STR name97;
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
 create_self = ((AM_LOCAL_EXPR) NULL);
 create_source = SFILE_ID_zero;
 L2=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_LOCAL_EXPR_tag;
 L1 = ((AM_LOCAL_EXPR) L2);
 r = L1;
 SATTR(r,source1,create_source);
 ret_val1 = r;
 am = ret_val1;
 SATTR(am,tp_at,t);
 SATTR(am,not_in_frame,TRUE);
 plus_self = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L3 = (plus_s==((dSTR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(plus_self,ntext);
  L6 = plus_s;
  SATTR(plus_self,ntext,FSTR_p1752847026(L5, (*dSTR_strrSTR[TAG(L6)])(L6)));
 }
 ret_val2 = plus_self;
 plus_self1 = ret_val2;
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
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 mang_self = self;
 mang_ob = ((OB) t);
 ret_val4 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val4);
 L13 = (plus_s2==((dSTR) NULL));
 L141_=!(L13); 
 if (L141_) {
  L15 = ATTR(plus_self2,ntext);
  L16 = plus_s2;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L15, (*dSTR_strrSTR[TAG(L16)])(L16)));
 }
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 plus_s3 = ((dSTR) ((STR) &name97));
 L17 = (plus_s3==((dSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  L19 = ATTR(plus_self3,ntext);
  L20 = plus_s3;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L19, (*dSTR_strrSTR[TAG(L20)])(L20)));
 }
 ret_val6 = plus_self3;
 plus_self4 = ret_val6;
 plus_s4 = ((dSTR) CGEN_e236145586(self, am));
 L21 = (plus_s4==((dSTR) NULL));
 L221_=!(L21); 
 if (L221_) {
  L23 = ATTR(plus_self4,ntext);
  L24 = plus_s4;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L23, (*dSTR_strrSTR[TAG(L24)])(L24)));
 }
 ret_val7 = plus_self4;
 plus_self5 = ret_val7;
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
 ret_val = am;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1006303229(CGEN self, AM_AT_EXPR ate) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR e;
 ARRAYARG L11;
 ARG a;
 SE_CONTEXT sec1;
 INT L21 = INT_zero;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 BOOL L10;
 BOOL L121_;
 INT L131_br;
 ARG aL131_;
 ARG L14;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 dMODE L18;
 INT L191_;
 e = ATTR(ate,e);
 if (e==NULL) {
  goto other366;
 } else
 switch (TAG(e)) {
  case AM_ROU1916046290_tag:
   L31_=(e)==NULL?0:ASIZE((AM_ROU1916046290)e); 
   L4 = L31_;
   L51_=(2)<(L4); 
   if (L51_) {
    ret_val = FALSE;
    return ret_val;
   }
   L6 = CGEN_i1610952633(self, ATTR(ATTR(((AM_ROU1916046290) e),fun),tp));
   L71_=!(L6); 
   if (L71_) {
    ret_val = FALSE;
    return ret_val;
   }
   L8 = (ATTR(ATTR(((AM_ROU1916046290) e),fun),ret)==((dTP) NULL));
   L91_=!(L8); 
   if (L91_) {
    L10 = CGEN_i1610952633(self, ATTR(ATTR(((AM_ROU1916046290) e),fun),ret));
    L121_=!(L10); 
    if (L121_) {
     ret_val = FALSE;
     return ret_val;
    }
   }
   {
    BOOL f_L131_ = TRUE;
    /* loop index variable */
    L21 = 0;
    L11 = ATTR(ATTR(((AM_ROU1916046290) e),fun),args);
    L131_br=L11==NULL?0:ASIZE((ARRAYARG)L11); 
    while (1) {
     if(L21>=L131_br)  goto after_loop; 
     aL131_=ARR((ARRAYARG)L11,L21); 
     a = aL131_;
     L16 = CGEN_i1610952633(self, ATTR(a,tp));
     L171_=!(L16); 
     if (L171_) {
      L15 = TRUE;
     } else {
      L18 = ATTR(a,mode1);
      L15 = (*dMODE_2144735300[TAG(L18)])(L18);
     }
     if (L15) {
      ret_val = FALSE;
      return ret_val;
     }
     L191_=INTPLUS(L21,1); 
     L21 = L191_;
    }
   }
   after_loop: ;
   sec1 = SIG_ge642497605(ATTR(((AM_ROU1916046290) e),fun), ATTR(self,prog));
   if ((sec1==((SE_CONTEXT) NULL))) {
    ret_val = TRUE;
    return ret_val;
   }
   if (ATTR(sec1,has_raise)) {
    ret_val = FALSE;
    return ret_val;
   }
   ret_val = TRUE;
   return ret_val; break;
  default: ;
  other366: ;
   ret_val = FALSE;
   return ret_val;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CGEN_i1610952633(CGEN self, dTP tp) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 dTP L2;
 BOOL L3;
 BOOL L41_;
 L1 = tp;
 if ((*dTP_is1334578382[TAG(L1)])(L1)) {
  L2 = tp;
  L3 = (*dTP_is418055720[TAG(L2)])(L2);
  L41_=!(L3); 
  if (L41_) {
   ret_val = FALSE;
   return ret_val;
  }
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e1835933683(CGEN self, SIG fun11, dAM_EXPR amexpr, ARRAYSTR arg_list) {
 STR ret_val;
 dAM_CALL_EXPR call11 = ((dAM_CALL_EXPR) NULL);
 dAM_EXPR ltp;
 ARRAYINT to_cons;
 INT to_cons_s = INT_zero;
 INT remote_call = INT_zero;
 INT remote_call_acce = INT_zero;
 ARRAYINT L11;
 INT i = INT_zero;
 STR l = ((STR) NULL);
 ARRAYBOOL nearness;
 INT L21 = INT_zero;
 INT i1 = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT j = INT_zero;
 INT accs = INT_zero;
 INT L61 = INT_zero;
 INT k = INT_zero;
 INT L71 = INT_zero;
 INT L81 = INT_zero;
 INT k1 = INT_zero;
 INT L91 = INT_zero;
 ARRAYA1575632848 args;
 ARRAYA1575632848 argsr;
 ARRAYARG L101;
 INT i2 = INT_zero;
 AM_AT_EXPR amat;
 AM_WHERE_EXPR amw;
 AM_ROU1916046290 amf;
 ARRAYA1575632848 L121;
 INT i3 = INT_zero;
 AM_CALL_ARG amc;
 STR att;
 ARRAYARG L131;
 INT i4 = INT_zero;
 SIG is_iter_self;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 SIG is_forked_self;
 BOOL ret_val3 = BOOL_zero;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val4;
 INT L141 = INT_zero;
 AM_CALL_ARG is_near_self;
 BOOL ret_val5 = BOOL_zero;
 AM_CALL_ARG is_near_self1;
 BOOL ret_val6 = BOOL_zero;
 AM_CALL_ARG is_near_self2;
 BOOL ret_val7 = BOOL_zero;
 ARRAYBOOL create_self1;
 INT create_n1 = INT_zero;
 ARRAYBOOL ret_val8;
 CGEN ndefer_self;
 STR ndefer_s;
 INT L151 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 STR plus_self2;
 INT plus_arg = INT_zero;
 STR ret_val11;
 STR s;
 INT str_self = INT_zero;
 STR ret_val12;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val13;
 FSTR str_self1;
 STR ret_val14;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val15;
 CGEN defer_self;
 STR defer_s;
 CGEN defer_self1;
 STR defer_s1;
 INT L161 = INT_zero;
 INT aeltb_self = INT_zero;
 BOOL ret_val16 = BOOL_zero;
 INT L171 = INT_zero;
 BOOL aeltb_if_first = BOOL_zero;
 STR plus_self4;
 INT plus_arg1 = INT_zero;
 STR ret_val17;
 STR s1;
 INT str_self2 = INT_zero;
 STR ret_val18;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val19;
 FSTR str_self3;
 STR ret_val20;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val21;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val22;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT L181 = INT_zero;
 INT aeltb_self1 = INT_zero;
 BOOL ret_val23 = BOOL_zero;
 INT L191 = INT_zero;
 BOOL aeltb_if_first1 = BOOL_zero;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val24;
 CGEN defer_self2;
 STR defer_s2;
 CGEN move_out_self;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN in_self1;
 ARRAYA1575632848 create_self2;
 INT create_n2 = INT_zero;
 ARRAYA1575632848 ret_val25;
 ARRAYA1575632848 create_self3;
 INT create_n3 = INT_zero;
 ARRAYA1575632848 ret_val26;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val27;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val28;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val29;
 CGEN ndefer_self4;
 STR ndefer_s4;
 INT L201 = INT_zero;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val30;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val32;
 CGEN ndefer_self5;
 STR ndefer_s5;
 STR plus_self14;
 STR plus_sarg12;
 STR ret_val33;
 STR plus_self15;
 STR plus_sarg13;
 STR ret_val34;
 STR plus_self16;
 STR plus_sarg14;
 STR ret_val35;
 CGEN ndefer_self6;
 STR ndefer_s6;
 STR plus_self17;
 STR plus_sarg15;
 STR ret_val36;
 STR plus_self18;
 STR plus_sarg16;
 STR ret_val37;
 STR plus_self19;
 STR plus_sarg17;
 STR ret_val38;
 CGEN ndefer_self7;
 STR ndefer_s7;
 STR plus_self20;
 STR plus_sarg18;
 STR ret_val39;
 STR plus_self21;
 STR plus_sarg19;
 STR ret_val40;
 CGEN ndefer_self8;
 STR ndefer_s8;
 CGEN in_self2;
 AM_AT_EXPR create_self4;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_AT_EXPR ret_val41;
 AM_AT_EXPR r;
 AM_WHERE_EXPR create_self5;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WHERE_EXPR ret_val42;
 AM_WHERE_EXPR r1;
 AM_ROU1916046290 create_self6;
 INT create_nargs = INT_zero;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val43;
 AM_ROU1916046290 r2;
 INT L221 = INT_zero;
 AM_CALL_ARG create_self7;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val44;
 AM_CALL_ARG res;
 STR plus_self22;
 STR plus_sarg20;
 STR ret_val45;
 CGEN ndefer_self9;
 STR ndefer_s9;
 STR plus_self23;
 STR plus_sarg21;
 STR ret_val46;
 CGEN defer_self3;
 STR defer_s3;
 INT L231 = INT_zero;
 STR plus_self24;
 STR plus_sarg22;
 STR ret_val47;
 STR plus_self25;
 STR plus_sarg23;
 STR ret_val48;
 STR plus_self26;
 STR plus_sarg24;
 STR ret_val49;
 STR plus_self27;
 STR plus_sarg25;
 STR ret_val50;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN move_out_self1;
 CGEN ndefer_self11;
 STR ndefer_s11;
 STR plus_self28;
 STR plus_sarg26;
 STR ret_val51;
 CGEN defer_self4;
 STR defer_s4;
 STR plus_self29;
 STR plus_sarg27;
 STR ret_val52;
 CGEN defer_self5;
 STR defer_s5;
 STR plus_self30;
 STR plus_sarg28;
 STR ret_val53;
 CGEN defer_self6;
 STR defer_s6;
 STR plus_self31;
 STR plus_sarg29;
 STR ret_val54;
 CGEN defer_self7;
 STR defer_s7;
 CGEN move_out_self2;
 CGEN ndefer_self12;
 STR ndefer_s12;
 BOOL L24;
 BOOL L251_;
 BOOL L26;
 BOOL L27;
 BOOL L28;
 BOOL L29;
 BOOL L30;
 BOOL L32;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 dTP L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 STR L42;
 INT L43;
 INT L441_;
 INT L45;
 CHAR L461_;
 CHAR L47;
 BOOL L481_;
 BOOL L49;
 BOOL L50;
 ARRAYINT L52;
 INT L531_;
 ARRAYINT L54;
 INT L55;
 OB L56;
 INT L57;
 INT L581_br;
 INT aL581_;
 INT L59;
 BOOL L60;
 BOOL L62;
 dAM_CALL_EXPR L63;
 dAM_EXPR L64;
 BOOL L65;
 BOOL L661_;
 ARRAYINT L67;
 INT L681_;
 INT L69;
 INT L70;
 BOOL L721_;
 BOOL L73;
 BOOL L741_;
 INT L761_;
 ARRAYINT L77;
 INT L781_;
 INT L79;
 BOOL L801_;
 ARRAYINT L82;
 INT L831_;
 BOOL L84;
 dAM_CALL_EXPR L85;
 dAM_EXPR L86;
 BOOL L87;
 BOOL L881_;
 BOOL L89;
 BOOL L90;
 ARRAYINT L92;
 INT L931_;
 INT L94;
 BOOL L951_;
 BOOL L96;
 BOOL L971_;
 dAM_CALL_EXPR L98;
 dAM_EXPR L99;
 INT L1001_;
 BOOL L102;
 BOOL L103;
 BOOL L104;
 BOOL L105;
 BOOL L1061_;
 INT L107;
 BOOL L1081_;
 BOOL L109;
 BOOL L1101_;
 BOOL L111;
 SE_CONTEXT L112;
 BOOL L113;
 BOOL L1141_;
 BOOL L1151_;
 BOOL L116;
 BOOL L1171_;
 INT L1181_;
 ARRAYBOOL L119;
 INT L120;
 OB L122;
 INT L123;
 extern STR switch1;
 INT aL1241_;
 INT L1251_;
 INT L126;
 extern STR NEAR1;
 INT L1271_;
 INT L128;
 STR L1291_;
 extern STR name103;
 INT L130;
 BOOL L132;
 BOOL L1331_;
 extern STR S02;
 INT L1341_;
 extern STR name84;
 INT L136;
 INT L1371_;
 INT L1351_,L1351_m;
 INT L138;
 BOOL L1391_;
 BOOL L140;
 BOOL L1421_;
 INT aL1431_;
 INT aL1441_;
 INT L145;
 INT L146;
 INT L147;
 BOOL L1481_;
 INT L1491_;
 INT L150;
 BOOL L1521_;
 ARRAYINT L153;
 INT L1541_;
 INT L155;
 INT L1561_;
 INT L1571_;
 BOOL L158;
 BOOL L1591_;
 BOOL L160;
 BOOL L1621_;
 BOOL L1631_;
 extern STR case3;
 BOOL L164;
 BOOL L1651_;
 extern STR name20;
 INT L166;
 INT L1671_;
 BOOL L168;
 BOOL L1691_;
 extern STR name33;
 INT aL1701_;
 INT aL1721_;
 INT L173;
 INT L174;
 INT L175;
 BOOL L1761_;
 INT L1771_;
 INT L178;
 INT L1801_;
 extern STR break3;
 INT L183;
 INT L1841_;
 extern STR default1;
 INT L185;
 INT L1861_;
 BOOL L1871_;
 BOOL L188;
 BOOL L1891_;
 ARRAYARG L190;
 INT L1921_;
 INT L193;
 INT L1941_;
 ARRAYA1575632848 L195;
 INT L196;
 OB L197;
 INT L198;
 ARRAYARG L199;
 INT L2001_;
 INT L202;
 INT L2031_;
 ARRAYA1575632848 L204;
 INT L205;
 OB L206;
 INT L207;
 AM_LOCAL_EXPR L208;
 AM_LOCAL_EXPR L2101_;
 AM_LOCAL_EXPR L211;
 AM_LOCAL_EXPR L2131_;
 extern STR name33;
 STR L2141_;
 extern STR name16;
 AM_LOCAL_EXPR L2151_;
 STR L216;
 INT L2181_br;
 INT rL2181_;
 INT L219;
 ARRAYARG L220;
 ARG L2221_;
 ARG L223;
 dMODE L224;
 BOOL L225;
 BOOL L2261_;
 INT L2271_;
 INT L228;
 ARRAYARG L229;
 ARG L2301_;
 ARG L232;
 AM_LOCAL_EXPR L233;
 INT L2351_;
 INT L236;
 ARRAYARG L237;
 ARG L2381_;
 ARG L239;
 AM_LOCAL_EXPR L240;
 INT L2421_;
 INT L243;
 AM_LOCAL_EXPR L2441_;
 extern STR name33;
 INT L2451_;
 INT L246;
 STR L2471_;
 extern STR name16;
 INT L2481_;
 INT L249;
 INT L2501_;
 INT L252;
 AM_LOCAL_EXPR L2531_;
 STR L254;
 ARRAYARG L256;
 ARG L2571_;
 ARG L258;
 dMODE L259;
 INT L2601_;
 INT L261;
 AM_LOCAL_EXPR L2621_;
 extern STR name104;
 INT L2631_;
 INT L264;
 STR L2651_;
 extern STR name16;
 INT L2661_;
 INT L267;
 ARRAYARG L268;
 ARG L2691_;
 ARG L270;
 AM_LOCAL_EXPR L271;
 INT L2731_;
 INT L274;
 INT L2751_;
 INT L276;
 AM_LOCAL_EXPR L2771_;
 AM_LOCAL_EXPR L278;
 INT L2801_;
 INT L281;
 AM_LOCAL_EXPR L2821_;
 extern STR name33;
 INT L2831_;
 INT L284;
 STR L2851_;
 extern STR name16;
 INT L2861_;
 INT L287;
 INT L2881_;
 INT L289;
 AM_LOCAL_EXPR L2901_;
 STR L291;
 INT L2931_;
 extern STR ifFAR;
 STR L2941_;
 extern STR name105;
 INT L295;
 INT L2961_;
 AM_AT_EXPR L297;
 OB L298;
 AM_WHERE_EXPR L299;
 OB L300;
 AM_LOCAL_EXPR L3011_;
 INT L3021_;
 AM_ROU1916046290 L303;
 INT L304;
 OB L305;
 INT L306;
 INT L3071_br;
 INT rL3071_;
 INT L308;
 AM_LOCAL_EXPR L3091_;
 AM_CALL_ARG L310;
 OB L311;
 BOOL L3121_;
 ARRAYARG L313;
 INT L3141_;
 INT L315;
 ARG L3161_;
 ARG L317;
 INT L3191_;
 BOOL L320;
 BOOL L3211_;
 extern STR name33;
 extern STR name16;
 INT L3221_br;
 INT rL3221_;
 INT L323;
 ARRAYARG L324;
 ARG L3251_;
 ARG L326;
 dMODE L327;
 BOOL L328;
 BOOL L3291_;
 extern STR name58;
 INT L3301_;
 INT L331;
 STR L3321_;
 extern STR name33;
 INT L3331_;
 INT L334;
 AM_LOCAL_EXPR L3351_;
 extern STR name16;
 INT L3361_;
 INT L337;
 INT L3381_;
 extern STR else5;
 BOOL L339;
 BOOL L3401_;
 extern STR name33;
 extern STR name106;
 BOOL L342;
 BOOL L3431_;
 extern STR name33;
 extern STR name16;
 INT L344;
 INT L3451_;
 extern STR name4;
 ltp = amexpr;
 L24 = (amexpr==((dAM_EXPR) NULL));
 L251_=!(L24); 
 if (L251_) {
  switch (TAG(ltp)) {
   case AM_BND367211769_tag:
   case AM_BND260301329_tag:
   case AM_EXT_CALL_EXPR_tag:
   case AM_ITE1809135850_tag:
   case AM_ROU1916046290_tag:
    call11 = ((dAM_CALL_EXPR) ltp); break;
   case AM_BND1124877163_tag:
    call11 = ((dAM_CALL_EXPR) NULL); break;
   default: ;
    FATAL("No applicable type in typecase\nin CGEN::emit_call(SIG,$AM_EXPR,ARRAY{STR}):STR\n./Back/cgen.sa:4961:17");
  }
 }
 else {
  call11 = ((dAM_CALL_EXPR) NULL);
 }
 L33 = ATTR(ATTR(self,prog),distributed);
 L341_=!(L33); 
 if (L341_) {
  L32 = TRUE;
 } else {
  L35 = ATTR(ATTR(self,options1),local_call_dynam1);
  L361_=!(L35); 
  L32 = L361_;
 }
 if (L32) {
  L30 = TRUE;
 } else {
  L37 = ATTR(fun11,tp);
  L30 = (*dTP_is1811059018[TAG(L37)])(L37);
 }
 if (L30) {
  L29 = TRUE;
 } else {
  is_iter_self = fun11;
  is_iter_self1 = ATTR(is_iter_self,name1);
  L39 = (is_iter_self1.str==((STR) NULL));
  L401_=!(L39); 
  if (L401_) {
   L42 = is_iter_self1.str;
   L43 = STR_sizerINT(is_iter_self1.str);
   L441_=INTMINUS(L43,1); 
   L45 = L441_;
   L461_=ARR((STR)L42,L45); 
   L47 = L461_;
   L481_=L47=='!'; 
   L38 = L481_;
  } else {
   L38 = FALSE;
  }
  ret_val2 = L38;
  ret_val1 = ret_val2;
  L29 = ret_val1;
 }
 if (L29) {
  L28 = TRUE;
 } else {
  is_forked_self = fun11;
  if (ATTR(is_forked_self,is_par_routine)) {
   L50 = TRUE;
  } else {
   L50 = ATTR(is_forked_self,is_fork_routine);
  }
  if (L50) {
   L49 = TRUE;
  } else {
   L49 = ATTR(is_forked_self,is_attach_routin);
  }
  ret_val3 = L49;
  L28 = ret_val3;
 }
 if (L28) {
  L27 = TRUE;
 } else {
  L27 = (ATTR(fun11,opt_info)==((OPT_LOCAL_CALL) NULL));
 }
 if (L27) {
  L26 = TRUE;
 } else {
  L26 = (ATTR(ATTR(fun11,opt_info),consider)==((ARRAYINT) NULL));
 }
 if (L26) {
  ret_val = CGEN_p1779648146(self, fun11, ((ARRAYBOOL) NULL), amexpr, arg_list);
  return ret_val;
 }
 else {
  create_self = ((ARRAYINT) NULL);
  L52 = ATTR(ATTR(fun11,opt_info),consider);
  L531_=ASIZE((ARRAYINT)L52); 
  create_n = L531_;
  L55 = create_n;
  L57=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L55)*sizeof(INT);
  L56=ZALLOC_LEAF_BIG(L57);
  if (L56==NULL) FATAL("Unable to allocate more memory");
  memset(L56,0,L57);
  ((OB)L56)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

    ((OB)L56)->header.destroyed=0;
#endif

  L54 = ((ARRAYINT) L56);
  L54->asize = L55;
  ret_val4 = L54;
  to_cons = ret_val4;
  to_cons_s = 0;
  remote_call = -1;
  remote_call_acce = 0;
  {
   BOOL f_L581_ = TRUE;
   /* loop index variable */
   L141 = 0;
   L11 = ATTR(ATTR(fun11,opt_info),consider);
   L581_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
   while (1) {
    if(L141>=L581_br)  goto after_loop; 
    aL581_=ARR((ARRAYINT)L11,L141); 
    i = aL581_;
    if ((call11==((dAM_CALL_EXPR) NULL))) {
     L62 = TRUE;
    } else {
     L63 = call11;
     is_near_self = (*dAM_CA967858872[TAG(L63)])(L63, i);
     L64 = ATTR(is_near_self,expr);
     ret_val5 = (*dAM_EX1117610980[TAG(L64)])(L64);
     L65 = ret_val5;
     L661_=!(L65); 
     L62 = L661_;
    }
    if (L62) {
     L67 = ATTR(ATTR(fun11,opt_info),attr_access);
     L681_=ARR((ARRAYINT)L67,i); 
     L69 = L681_;
     L70 = ATTR(ATTR(self,options1),local_call_dynam);
     L721_=(L69)<(L70); 
     L73 = L721_;
     L741_=!(L73); 
     L60 = L741_;
    } else {
     L60 = FALSE;
    }
    if (L60) {
     SARR((ARRAYINT)to_cons,to_cons_s,i); 
     ;
     L761_=INTPLUS(to_cons_s,1); 
     to_cons_s = L761_;
    }
    L77 = ATTR(ATTR(fun11,opt_info),attr_access);
    L781_=ARR((ARRAYINT)L77,i); 
    L79 = L781_;
    L801_=(remote_call_acce)<(L79); 
    if (L801_) {
     L82 = ATTR(ATTR(fun11,opt_info),attr_access);
     L831_=ARR((ARRAYINT)L82,i); 
     remote_call_acce = L831_;
     if ((call11==((dAM_CALL_EXPR) NULL))) {
      L84 = TRUE;
     } else {
      L85 = call11;
      is_near_self1 = (*dAM_CA967858872[TAG(L85)])(L85, i);
      L86 = ATTR(is_near_self1,expr);
      ret_val6 = (*dAM_EX1117610980[TAG(L86)])(L86);
      L87 = ret_val6;
      L881_=!(L87); 
      L84 = L881_;
     }
     if (L84) {
      remote_call = i;
     }
     else {
      remote_call = -1;
     }
    }
    else {
     L92 = ATTR(ATTR(fun11,opt_info),attr_access);
     L931_=ARR((ARRAYINT)L92,i); 
     L94 = L931_;
     L951_=(remote_call_acce)==(L94); 
     if (L951_) {
      L96 = (call11==((dAM_CALL_EXPR) NULL));
      L971_=!(L96); 
      L90 = L971_;
     } else {
      L90 = FALSE;
     }
     if (L90) {
      L98 = call11;
      is_near_self2 = (*dAM_CA967858872[TAG(L98)])(L98, i);
      L99 = ATTR(is_near_self2,expr);
      ret_val7 = (*dAM_EX1117610980[TAG(L99)])(L99);
      L89 = ret_val7;
     } else {
      L89 = FALSE;
     }
     if (L89) {
      remote_call = -1;
     }
    }
    L1001_=INTPLUS(L141,1); 
    L141 = L1001_;
   }
  }
  after_loop: ;
  L105 = ATTR(ATTR(self,options1),remote_call);
  L1061_=!(L105); 
  if (L1061_) {
   L104 = TRUE;
  } else {
   L107 = ATTR(ATTR(self,options1),remote_call_acce);
   L1081_=(remote_call_acce)<(L107); 
   L104 = L1081_;
  }
  if (L104) {
   L103 = TRUE;
  } else {
   L109 = ATTR(ATTR(self,options1),side_effects);
   L1101_=!(L109); 
   L103 = L1101_;
  }
  if (L103) {
   L102 = TRUE;
  } else {
   L112=SIG_ge642497605(fun11, ATTR(self,prog));
   if (ATTR(L112,has_new)) {
    L113 = ATTR(ATTR(self,options1),remote_call_crea);
    L1141_=!(L113); 
    L111 = L1141_;
   } else {
    L111 = FALSE;
   }
   L102 = L111;
  }
  if (L102) {
   remote_call = -1;
  }
  L1151_=(to_cons_s)==(0); 
  if (L1151_) {
   ret_val = CGEN_p1779648146(self, fun11, ((ARRAYBOOL) NULL), amexpr, arg_list);
   return ret_val;
  }
  L116 = (ATTR(fun11,ret)==((dTP) NULL));
  L1171_=!(L116); 
  if (L1171_) {
   l = CGEN_d1425664957(self, ATTR(fun11,ret));
  }
  create_self1 = ((ARRAYBOOL) NULL);
  L1181_=ASIZE((ARRAYSTR)arg_list); 
  create_n1 = L1181_;
  L120 = create_n1;
  L123=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L120)*sizeof(BOOL);
  L122=ZALLOC_LEAF_BIG(L123);
  if (L122==NULL) FATAL("Unable to allocate more memory");
  memset(L122,0,L123);
  ((OB)L122)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

    ((OB)L122)->header.destroyed=0;
#endif

  L119 = ((ARRAYBOOL) L122);
  L119->asize = L120;
  ret_val8 = L119;
  nearness = ret_val8;
  ndefer_self = self;
  ndefer_s = ((STR) &switch1);
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  {
   BOOL f_L1241_ = TRUE;
   BOOL f_L1251_ = TRUE;
   /* loop index variable */
   L151 = 0;
   L21 = to_cons_s;
   L31 = 0;
   L1251_=L31-1; 
   while (1) {
    if(L151>=L21)  goto after_loop1; 
    aL1241_=L151; 
    i1 = aL1241_;
    defer_self = self;
    plus_self = ((STR) &NEAR1);
    L1271_=ARR((ARRAYINT)to_cons,i1); 
    L128 = L1271_;
    L1291_=(STR)ARR((ARRAYSTR)arg_list,L128); 
    plus_sarg = L1291_;
    ret_val9 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val9;
    plus_sarg1 = ((STR) &name103);
    ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val10;
    L1251_++; 
    plus_arg = INT_pow_INTrINT(2, L1251_);
    str_self = plus_arg;
    clear_self = buf1;
    L132 = (clear_self==((FSTR) NULL));
    L1331_=!(L132); 
    if (L1331_) {
     SATTR(clear_self,loc,0);
    }
    str_in_self = str_self;
    str_in_s = buf1;
    ret_val13 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
    buf1 = ret_val13;
    str_self1 = buf1;
    ret_val14 = STR_fr1097270334(((STR) NULL), str_self1);
    ret_val12 = ret_val14;
    s = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self2, s, TRUE);
    plus_self3 = ret_val11;
    plus_sarg2 = ((STR) &S02);
    ret_val15 = STR_ap2004550586(plus_self3, plus_sarg2);
    defer_s = ret_val15;
    SATTR(defer_self,routine_code,FSTR_p1752847026(ATTR(defer_self,routine_code), defer_s));
    L1341_=INTPLUS(L151,1); 
    L151 = L1341_;
   }
  }
  after_loop1: ;
  defer_self1 = self;
  defer_s1 = ((STR) &name84);
  SATTR(defer_self1,routine_code,FSTR_p1752847026(ATTR(defer_self1,routine_code), defer_s1));
  {
   BOOL f_L1351_ = TRUE;
   L41 = 1;
   L136 = INT_pow_INTrINT(2, to_cons_s);
   L1371_=INTMINUS(L136,1); 
   L51 = L1371_;
   L1351_=L41-1;L1351_m=L51; 
   while (1) {
    if(L1351_++>=L1351_m)  goto after_loop2; 
    j = L1351_;
    accs = 0;
    L1391_=(remote_call)<(0); 
    L140 = L1391_;
    L1421_=!(L140); 
    if (L1421_) {
     {
      BOOL f_L1431_ = TRUE;
      BOOL f_L1441_ = TRUE;
      /* loop index variable */
      L161 = 0;
      L61 = to_cons_s;
      /* Initialization of inlined iter INT::aelt!:BOOL */
      L71 = j;
      aeltb_self = L71;
      L171 = INT_asize;
      aeltb_if_first = TRUE;
      while (1) {
       if(L161>=L61)  goto after_loop3; 
       aL1431_=L161; 
       k = aL1431_;
       if(L161>=L171)  goto after_loop3; 
       aL1441_=L161; 
       L147 = aL1441_;
       L1481_=((CHAR)((aeltb_self&(1<<L147))!=0)); 
       if (L1481_) {
        L1491_=ARR((ARRAYINT)to_cons,k); 
        L150 = L1491_;
        L1521_=(remote_call)==(L150); 
        if (L1521_) {
         accs = remote_call_acce;
         goto after_loop3;
        }
        L153 = ATTR(ATTR(fun11,opt_info),attr_access);
        L1541_=ARR((ARRAYINT)L153,k); 
        L155 = L1541_;
        L1561_=INTPLUS(accs,L155); 
        accs = L1561_;
       }
       L1571_=INTPLUS(L161,1); 
       L161 = L1571_;
      }
     }
     after_loop3: ;
    }
    L1591_=(accs)<(remote_call_acce); 
    L160 = L1591_;
    L1621_=!(L160); 
    if (L1621_) {
     L158 = TRUE;
    } else {
     L1631_=(remote_call)<(0); 
     L158 = L1631_;
    }
    if (L158) {
     ndefer_self1 = self;
     plus_self4 = ((STR) &case3);
     plus_arg1 = j;
     str_self2 = plus_arg1;
     clear_self1 = buf2;
     L164 = (clear_self1==((FSTR) NULL));
     L1651_=!(L164); 
     if (L1651_) {
      SATTR(clear_self1,loc,0);
     }
     str_in_self1 = str_self2;
     str_in_s1 = buf2;
     ret_val19 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
     buf2 = ret_val19;
     str_self3 = buf2;
     ret_val20 = STR_fr1097270334(((STR) NULL), str_self3);
     ret_val18 = ret_val20;
     s1 = ret_val18;
     ret_val17 = STR_ap1077157958(plus_self4, s1, TRUE);
     plus_self5 = ret_val17;
     plus_sarg3 = ((STR) &name20);
     ret_val21 = STR_ap2004550586(plus_self5, plus_sarg3);
     ndefer_s1 = ret_val21;
     CGEN_d695831906(ndefer_self1);
     SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
     in_self = self;
     L166 = ATTR(in_self,indent);
     L1671_=INTPLUS(L166,1); 
     SATTR(in_self,indent,L1671_);
     L168 = (ATTR(fun11,ret)==((dTP) NULL));
     L1691_=!(L168); 
     if (L1691_) {
      ndefer_self2 = self;
      plus_self6 = l;
      plus_sarg4 = ((STR) &name33);
      ret_val22 = STR_ap2004550586(plus_self6, plus_sarg4);
      ndefer_s2 = ret_val22;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
     {
      BOOL f_L1701_ = TRUE;
      BOOL f_L1721_ = TRUE;
      /* loop index variable */
      L181 = 0;
      L81 = to_cons_s;
      /* Initialization of inlined iter INT::aelt!:BOOL */
      L91 = j;
      aeltb_self1 = L91;
      L191 = INT_asize;
      aeltb_if_first1 = TRUE;
      while (1) {
       if(L181>=L81)  goto after_loop4; 
       aL1701_=L181; 
       k1 = aL1701_;
       if(L181>=L191)  goto after_loop4; 
       aL1721_=L181; 
       L175 = aL1721_;
       L1761_=((CHAR)((aeltb_self1&(1<<L175))!=0)); 
       if (L1761_) {
        L1771_=ARR((ARRAYINT)to_cons,k1); 
        L178 = L1771_;
        SARR((ARRAYBOOL)nearness,L178,TRUE); 
        ;
       }
       L1801_=INTPLUS(L181,1); 
       L181 = L1801_;
      }
     }
     after_loop4: ;
     defer_self2 = self;
     plus_self7 = CGEN_p1779648146(self, fun11, nearness, amexpr, arg_list);
     plus_sarg5 = ((STR) &break3);
     ret_val24 = STR_ap2004550586(plus_self7, plus_sarg5);
     defer_s2 = ret_val24;
     SATTR(defer_self2,routine_code,FSTR_p1752847026(ATTR(defer_self2,routine_code), defer_s2));
     if(nearness!=NULL) AREFACLEAR(nearness); 
     ;
     move_out_self = self;
     L183 = ATTR(move_out_self,indent);
     L1841_=INTMINUS(L183,1); 
     SATTR(move_out_self,indent,L1841_);
    }
   }
  }
  after_loop2: ;
  ndefer_self3 = self;
  ndefer_s3 = ((STR) &default1);
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
  in_self1 = self;
  L185 = ATTR(in_self1,indent);
  L1861_=INTPLUS(L185,1); 
  SATTR(in_self1,indent,L1861_);
  L1871_=(remote_call)<(0); 
  L188 = L1871_;
  L1891_=!(L188); 
  if (L1891_) {
   create_self2 = ((ARRAYA1575632848) NULL);
   L190 = ATTR(fun11,args);
   L1921_=(L190)==NULL?0:ASIZE((ARRAYARG)L190); 
   L193 = L1921_;
   L1941_=INTPLUS(L193,1); 
   create_n2 = L1941_;
   L196 = create_n2;
   L198=(sizeof(struct ARRAYA1575632848_struct)-sizeof(AM_LOCAL_EXPR))+(L196)*sizeof(AM_LOCAL_EXPR);
   L197=ZALLOC_BIG(L198);
   if (L197==NULL) FATAL("Unable to allocate more memory");
   ((OB)L197)->header.tag=ARRAYA1575632848_tag;
#ifdef DESTROY_CHK

     ((OB)L197)->header.destroyed=0;
#endif

   L195 = ((ARRAYA1575632848) L197);
   L195->asize = L196;
   ret_val25 = L195;
   args = ret_val25;
   create_self3 = ((ARRAYA1575632848) NULL);
   L199 = ATTR(fun11,args);
   L2001_=(L199)==NULL?0:ASIZE((ARRAYARG)L199); 
   L202 = L2001_;
   L2031_=INTPLUS(L202,1); 
   create_n3 = L2031_;
   L205 = create_n3;
   L207=(sizeof(struct ARRAYA1575632848_struct)-sizeof(AM_LOCAL_EXPR))+(L205)*sizeof(AM_LOCAL_EXPR);
   L206=ZALLOC_BIG(L207);
   if (L206==NULL) FATAL("Unable to allocate more memory");
   ((OB)L206)->header.tag=ARRAYA1575632848_tag;
#ifdef DESTROY_CHK

     ((OB)L206)->header.destroyed=0;
#endif

   L204 = ((ARRAYA1575632848) L206);
   L204->asize = L205;
   ret_val26 = L204;
   argsr = ret_val26;
   L208 = CGEN_d1823718364(self, ATTR(fun11,tp));
   SARR((ARRAYA1575632848)args,0,(AM_LOCAL_EXPR)L208); 
   ;
   L2101_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,0); 
   L211 = L2101_;
   SARR((ARRAYA1575632848)argsr,0,(AM_LOCAL_EXPR)L211); 
   ;
   ndefer_self4 = self;
   L2131_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,0); 
   plus_self8 = CGEN_e236145586(self, L2131_);
   plus_sarg6 = ((STR) &name33);
   ret_val27 = STR_ap2004550586(plus_self8, plus_sarg6);
   plus_self9 = ret_val27;
   L2141_=(STR)ARR((ARRAYSTR)arg_list,0); 
   plus_sarg7 = L2141_;
   ret_val28 = STR_ap2004550586(plus_self9, plus_sarg7);
   plus_self10 = ret_val28;
   plus_sarg8 = ((STR) &name16);
   ret_val29 = STR_ap2004550586(plus_self10, plus_sarg8);
   ndefer_s4 = ret_val29;
   CGEN_d695831906(ndefer_self4);
   SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
   L2151_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,0); 
   L216 = CGEN_e236145586(self, L2151_);
   SARR((ARRAYSTR)arg_list,0,(STR)L216); 
   ;
   {
    BOOL f_L2181_ = TRUE;
    /* loop index variable */
    L201 = 0;
    L101 = ATTR(fun11,args);
    L2181_br=L101==NULL?0:ASIZE((ARRAYARG)L101); 
    while (1) {
     if(L201>=L2181_br)  goto after_loop5; 
     rL2181_=L201; 
     i2 = rL2181_;
     L220 = ATTR(fun11,args);
     L2221_=(ARG)ARR((ARRAYARG)L220,i2); 
     L223=L2221_;
     L224 = ATTR(L223,mode1);
     L225 = (*dMODE_814247358[TAG(L224)])(L224, ((dMODE) MODES_in_mode));
     L2261_=!(L225); 
     if (L2261_) {
      L2271_=INTPLUS(i2,1); 
      L228 = L2271_;
      L229 = ATTR(fun11,args);
      L2301_=(ARG)ARR((ARRAYARG)L229,i2); 
      L232=L2301_;
      L233 = CGEN_d609641833(self, ATTR(L232,tp));
      SARR((ARRAYA1575632848)args,L228,(AM_LOCAL_EXPR)L233); 
      ;
      L2351_=INTPLUS(i2,1); 
      L236 = L2351_;
      L237 = ATTR(fun11,args);
      L2381_=(ARG)ARR((ARRAYARG)L237,i2); 
      L239=L2381_;
      L240 = CGEN_d1823718364(self, ATTR(L239,tp));
      SARR((ARRAYA1575632848)argsr,L236,(AM_LOCAL_EXPR)L240); 
      ;
      ndefer_self5 = self;
      L2421_=INTPLUS(i2,1); 
      L243 = L2421_;
      L2441_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,L243); 
      plus_self11 = CGEN_e236145586(self, L2441_);
      plus_sarg9 = ((STR) &name33);
      ret_val30 = STR_ap2004550586(plus_self11, plus_sarg9);
      plus_self12 = ret_val30;
      L2451_=INTPLUS(i2,1); 
      L246 = L2451_;
      L2471_=(STR)ARR((ARRAYSTR)arg_list,L246); 
      plus_sarg10 = L2471_;
      ret_val31 = STR_ap2004550586(plus_self12, plus_sarg10);
      plus_self13 = ret_val31;
      plus_sarg11 = ((STR) &name16);
      ret_val32 = STR_ap2004550586(plus_self13, plus_sarg11);
      ndefer_s5 = ret_val32;
      CGEN_d695831906(ndefer_self5);
      SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
      L2481_=INTPLUS(i2,1); 
      L249 = L2481_;
      L2501_=INTPLUS(i2,1); 
      L252 = L2501_;
      L2531_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,L252); 
      L254 = CGEN_e236145586(self, L2531_);
      SARR((ARRAYSTR)arg_list,L249,(STR)L254); 
      ;
      L256 = ATTR(fun11,args);
      L2571_=(ARG)ARR((ARRAYARG)L256,i2); 
      L258=L2571_;
      L259 = ATTR(L258,mode1);
      if ((*dMODE_814247358[TAG(L259)])(L259, ((dMODE) MODES_inout_mode))) {
       ndefer_self6 = self;
       L2601_=INTPLUS(i2,1); 
       L261 = L2601_;
       L2621_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)argsr,L261); 
       plus_self14 = CGEN_e236145586(self, L2621_);
       plus_sarg12 = ((STR) &name104);
       ret_val33 = STR_ap2004550586(plus_self14, plus_sarg12);
       plus_self15 = ret_val33;
       L2631_=INTPLUS(i2,1); 
       L264 = L2631_;
       L2651_=(STR)ARR((ARRAYSTR)arg_list,L264); 
       plus_sarg13 = L2651_;
       ret_val34 = STR_ap2004550586(plus_self15, plus_sarg13);
       plus_self16 = ret_val34;
       plus_sarg14 = ((STR) &name16);
       ret_val35 = STR_ap2004550586(plus_self16, plus_sarg14);
       ndefer_s6 = ret_val35;
       CGEN_d695831906(ndefer_self6);
       SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
      }
     }
     else {
      L2661_=INTPLUS(i2,1); 
      L267 = L2661_;
      L268 = ATTR(fun11,args);
      L2691_=(ARG)ARR((ARRAYARG)L268,i2); 
      L270=L2691_;
      L271 = CGEN_d1823718364(self, ATTR(L270,tp));
      SARR((ARRAYA1575632848)args,L267,(AM_LOCAL_EXPR)L271); 
      ;
      L2731_=INTPLUS(i2,1); 
      L274 = L2731_;
      L2751_=INTPLUS(i2,1); 
      L276 = L2751_;
      L2771_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,L276); 
      L278 = L2771_;
      SARR((ARRAYA1575632848)argsr,L274,(AM_LOCAL_EXPR)L278); 
      ;
      ndefer_self7 = self;
      L2801_=INTPLUS(i2,1); 
      L281 = L2801_;
      L2821_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,L281); 
      plus_self17 = CGEN_e236145586(self, L2821_);
      plus_sarg15 = ((STR) &name33);
      ret_val36 = STR_ap2004550586(plus_self17, plus_sarg15);
      plus_self18 = ret_val36;
      L2831_=INTPLUS(i2,1); 
      L284 = L2831_;
      L2851_=(STR)ARR((ARRAYSTR)arg_list,L284); 
      plus_sarg16 = L2851_;
      ret_val37 = STR_ap2004550586(plus_self18, plus_sarg16);
      plus_self19 = ret_val37;
      plus_sarg17 = ((STR) &name16);
      ret_val38 = STR_ap2004550586(plus_self19, plus_sarg17);
      ndefer_s7 = ret_val38;
      CGEN_d695831906(ndefer_self7);
      SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
      L2861_=INTPLUS(i2,1); 
      L287 = L2861_;
      L2881_=INTPLUS(i2,1); 
      L289 = L2881_;
      L2901_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,L289); 
      L291 = CGEN_e236145586(self, L2901_);
      SARR((ARRAYSTR)arg_list,L287,(STR)L291); 
      ;
     }
     L2931_=INTPLUS(L201,1); 
     L201 = L2931_;
    }
   }
   after_loop5: ;
   ndefer_self8 = self;
   plus_self20 = ((STR) &ifFAR);
   L2941_=(STR)ARR((ARRAYSTR)arg_list,remote_call); 
   plus_sarg18 = L2941_;
   ret_val39 = STR_ap2004550586(plus_self20, plus_sarg18);
   plus_self21 = ret_val39;
   plus_sarg19 = ((STR) &name105);
   ret_val40 = STR_ap2004550586(plus_self21, plus_sarg19);
   ndefer_s8 = ret_val40;
   CGEN_d695831906(ndefer_self8);
   SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
   in_self2 = self;
   L295 = ATTR(in_self2,indent);
   L2961_=INTPLUS(L295,1); 
   SATTR(in_self2,indent,L2961_);
   create_self4 = ((AM_AT_EXPR) NULL);
   create_src = SFILE_ID_zero;
   L298=ZALLOC(sizeof(struct AM_AT_EXPR_struct));
   if (L298==NULL) FATAL("Unable to allocate more memory");
   ((OB)L298)->header.tag=AM_AT_EXPR_tag;
   L297 = ((AM_AT_EXPR) L298);
   r = L297;
   SATTR(r,source1,create_src);
   ret_val41 = r;
   amat = ret_val41;
   create_self5 = ((AM_WHERE_EXPR) NULL);
   create_source = SFILE_ID_zero;
   L300=ZALLOC(sizeof(struct AM_WHERE_EXPR_struct));
   if (L300==NULL) FATAL("Unable to allocate more memory");
   ((OB)L300)->header.tag=AM_WHERE_EXPR_tag;
   L299 = ((AM_WHERE_EXPR) L300);
   r1 = L299;
   SATTR(r1,source1,create_source);
   ret_val42 = r1;
   amw = ret_val42;
   L3011_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)args,remote_call); 
   SATTR(amw,arg,((dAM_EXPR) L3011_));
   SATTR(amat,at,((dAM_EXPR) amw));
   create_self6 = ((AM_ROU1916046290) NULL);
   L3021_=(args)==NULL?0:ASIZE((ARRAYA1575632848)args); 
   create_nargs = L3021_;
   create_source1 = SFILE_ID_zero;
   L304 = create_nargs;
   L306=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L304)*sizeof(AM_CALL_ARG);
   L305=ZALLOC_BIG(L306);
   if (L305==NULL) FATAL("Unable to allocate more memory");
   ((OB)L305)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

     ((OB)L305)->header.destroyed=0;
#endif

   L303 = ((AM_ROU1916046290) L305);
   L303->asize = L304;
   r2 = L303;
   SATTR(r2,source1,create_source1);
   ret_val43 = r2;
   amf = ret_val43;
   SATTR(amf,fun,fun11);
   {
    BOOL f_L3071_ = TRUE;
    /* loop index variable */
    L221 = 0;
    L121 = args;
    L3071_br=L121==NULL?0:ASIZE((ARRAYA1575632848)L121); 
    while (1) {
     if(L221>=L3071_br)  goto after_loop6; 
     rL3071_=L221; 
     i3 = rL3071_;
     create_self7 = ((AM_CALL_ARG) NULL);
     L3091_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)argsr,i3); 
     create_e = ((dAM_EXPR) L3091_);
     L311=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
     if (L311==NULL) FATAL("Unable to allocate more memory");
     ((OB)L311)->header.tag=AM_CALL_ARG_tag;
     L310 = ((AM_CALL_ARG) L311);
     res = L310;
     SATTR(res,expr,create_e);
     SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
     ret_val44 = res;
     amc = ret_val44;
     L3121_=(i3)==(0); 
     if (L3121_) {
      SATTR(amc,mode1,((dMODE) MODES_in_mode));
     }
     else {
      L313 = ATTR(fun11,args);
      L3141_=INTMINUS(i3,1); 
      L315 = L3141_;
      L3161_=(ARG)ARR((ARRAYARG)L313,L315); 
      L317=L3161_;
      SATTR(amc,mode1,ATTR(L317,mode1));
     }
     SARR((AM_ROU1916046290)amf,i3,(AM_CALL_ARG)amc); 
     ;
     L3191_=INTPLUS(L221,1); 
     L221 = L3191_;
    }
   }
   after_loop6: ;
   SATTR(amat,e,((dAM_EXPR) amf));
   att = CGEN_e969629179(self, amat);
   L320 = (ATTR(fun11,ret)==((dTP) NULL));
   L3211_=!(L320); 
   if (L3211_) {
    ndefer_self9 = self;
    plus_self22 = l;
    plus_sarg20 = ((STR) &name33);
    ret_val45 = STR_ap2004550586(plus_self22, plus_sarg20);
    ndefer_s9 = ret_val45;
    CGEN_d695831906(ndefer_self9);
    SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
   }
   defer_self3 = self;
   plus_self23 = att;
   plus_sarg21 = ((STR) &name16);
   ret_val46 = STR_ap2004550586(plus_self23, plus_sarg21);
   defer_s3 = ret_val46;
   SATTR(defer_self3,routine_code,FSTR_p1752847026(ATTR(defer_self3,routine_code), defer_s3));
   {
    BOOL f_L3221_ = TRUE;
    /* loop index variable */
    L231 = 0;
    L131 = ATTR(fun11,args);
    L3221_br=L131==NULL?0:ASIZE((ARRAYARG)L131); 
    while (1) {
     if(L231>=L3221_br)  goto after_loop7; 
     rL3221_=L231; 
     i4 = rL3221_;
     L324 = ATTR(fun11,args);
     L3251_=(ARG)ARR((ARRAYARG)L324,i4); 
     L326=L3251_;
     L327 = ATTR(L326,mode1);
     L328 = (*dMODE_814247358[TAG(L327)])(L327, ((dMODE) MODES_in_mode));
     L3291_=!(L328); 
     if (L3291_) {
      ndefer_self10 = self;
      plus_self24 = ((STR) &name58);
      L3301_=INTPLUS(i4,1); 
      L331 = L3301_;
      L3321_=(STR)ARR((ARRAYSTR)arg_list,L331); 
      plus_sarg22 = L3321_;
      ret_val47 = STR_ap2004550586(plus_self24, plus_sarg22);
      plus_self25 = ret_val47;
      plus_sarg23 = ((STR) &name33);
      ret_val48 = STR_ap2004550586(plus_self25, plus_sarg23);
      plus_self26 = ret_val48;
      L3331_=INTPLUS(i4,1); 
      L334 = L3331_;
      L3351_=(AM_LOCAL_EXPR)ARR((ARRAYA1575632848)argsr,L334); 
      plus_sarg24 = CGEN_e236145586(self, L3351_);
      ret_val49 = STR_ap2004550586(plus_self26, plus_sarg24);
      plus_self27 = ret_val49;
      plus_sarg25 = ((STR) &name16);
      ret_val50 = STR_ap2004550586(plus_self27, plus_sarg25);
      ndefer_s10 = ret_val50;
      CGEN_d695831906(ndefer_self10);
      SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
     }
     L3361_=INTPLUS(L231,1); 
     L231 = L3361_;
    }
   }
   after_loop7: ;
   move_out_self1 = self;
   L337 = ATTR(move_out_self1,indent);
   L3381_=INTMINUS(L337,1); 
   SATTR(move_out_self1,indent,L3381_);
   ndefer_self11 = self;
   ndefer_s11 = ((STR) &else5);
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
   L339 = (ATTR(fun11,ret)==((dTP) NULL));
   L3401_=!(L339); 
   if (L3401_) {
    defer_self4 = self;
    plus_self28 = l;
    plus_sarg26 = ((STR) &name33);
    ret_val51 = STR_ap2004550586(plus_self28, plus_sarg26);
    defer_s4 = ret_val51;
    SATTR(defer_self4,routine_code,FSTR_p1752847026(ATTR(defer_self4,routine_code), defer_s4));
   }
   defer_self5 = self;
   plus_self29 = CGEN_p1779648146(self, fun11, ((ARRAYBOOL) NULL), amexpr, arg_list);
   plus_sarg27 = ((STR) &name106);
   ret_val52 = STR_ap2004550586(plus_self29, plus_sarg27);
   defer_s5 = ret_val52;
   SATTR(defer_self5,routine_code,FSTR_p1752847026(ATTR(defer_self5,routine_code), defer_s5));
  }
  else {
   L342 = (ATTR(fun11,ret)==((dTP) NULL));
   L3431_=!(L342); 
   if (L3431_) {
    defer_self6 = self;
    plus_self30 = l;
    plus_sarg28 = ((STR) &name33);
    ret_val53 = STR_ap2004550586(plus_self30, plus_sarg28);
    defer_s6 = ret_val53;
    SATTR(defer_self6,routine_code,FSTR_p1752847026(ATTR(defer_self6,routine_code), defer_s6));
   }
   defer_self7 = self;
   plus_self31 = CGEN_p1779648146(self, fun11, ((ARRAYBOOL) NULL), amexpr, arg_list);
   plus_sarg29 = ((STR) &name16);
   ret_val54 = STR_ap2004550586(plus_self31, plus_sarg29);
   defer_s7 = ret_val54;
   SATTR(defer_self7,routine_code,FSTR_p1752847026(ATTR(defer_self7,routine_code), defer_s7));
  }
  move_out_self2 = self;
  L344 = ATTR(move_out_self2,indent);
  L3451_=INTMINUS(L344,1); 
  SATTR(move_out_self2,indent,L3451_);
  ndefer_self12 = self;
  ndefer_s12 = ((STR) &name4);
  CGEN_d695831906(ndefer_self12);
  SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
  ret_val = l;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e283387820(CGEN self, AM_AT_EXPR ate) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR dest1 = ((STR) NULL);
 STR req_str = ((STR) NULL);
 ARRAYSTR arg_list = ((ARRAYSTR) NULL);
 AM_ROU1916046290 ca = ((AM_ROU1916046290) NULL);
 BOOL may_block = BOOL_zero;
 dAM_EXPR e;
 STR call_str;
 INT num_args = INT_zero;
 STR arg_str = ((STR) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 ARG sa1;
 STR decl;
 CGEN comment_self;
 STR comment_com;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 CGEN in_self;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val3;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN move_out_self;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN in_self1;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 INT str_self = INT_zero;
 STR ret_val4;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val5;
 FSTR str_self1;
 STR ret_val6;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val7;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val8;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val9;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val11;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN ndefer_self8;
 STR ndefer_s8;
 AM_ROU1916046290 tp_self;
 dTP ret_val12;
 AM_ROU1916046290 tp_self1;
 dTP ret_val13;
 AM_ROU1916046290 tp_self2;
 dTP ret_val14;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val15;
 STR res1;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val16;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val17;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val20;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val21;
 CGEN ndefer_self9;
 STR ndefer_s9;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val22;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val23;
 CGEN ndefer_self11;
 STR ndefer_s11;
 CGEN ndefer_self12;
 STR ndefer_s12;
 CGEN move_out_self1;
 CGEN ndefer_self13;
 STR ndefer_s13;
 CGEN comment_self1;
 STR comment_com1;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val24;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val25;
 CGEN ndefer_self14;
 STR ndefer_s14;
 CODE_FILE plus_self14;
 dSTR plus_s;
 SE_CONTEXT L3;
 BOOL L4;
 BOOL L51_;
 extern STR Simple351050430;
 extern STR name54;
 extern STR name79;
 extern STR ifBR_HERE;
 extern STR name99;
 INT L6;
 INT L71_;
 BOOL L8;
 BOOL L91_;
 extern STR name33;
 extern STR name16;
 extern STR name16;
 INT L10;
 INT L121_;
 extern STR else4;
 INT L13;
 INT L141_;
 extern STR AT_ATT216595904;
 extern STR AT_ATT601389983;
 INT L151_;
 INT L16;
 INT L171_;
 extern STR BR_FORK_;
 extern STR BR_REQUEST_;
 BOOL L18;
 BOOL L191_;
 extern STR name24;
 extern STR name3;
 extern STR name3;
 extern STR BR_wor1065271233;
 dTP L20;
 extern STR BR_word_t2;
 STR L221_;
 extern STR BR_word_tSENDFOB;
 STR L231_;
 STR L24;
 extern STR name3;
 extern STR name26;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L251_,L251_m;
 ARRAYARG L30;
 INT L311_;
 INT L32;
 ARG L331_;
 dTP L34;
 dTP L35;
 BOOL L36;
 BOOL L371_;
 extern STR nonato642173603;
 extern STR Intern28965746;
 extern STR BR_word_t3;
 STR L381_;
 extern STR BR_word_t3;
 extern STR SENDFOB1;
 STR L391_;
 STR L40;
 extern STR name3;
 extern STR name26;
 extern STR name3;
 extern STR name76;
 extern STR AT_ATT1091925463;
 BOOL L41;
 BOOL L421_;
 dTP L43;
 dTP L44;
 BOOL L45;
 BOOL L461_;
 extern STR RECVOB1;
 extern STR S_tag;
 extern STR name100;
 extern STR at_attachresult;
 extern STR name76;
 extern STR name33;
 extern STR at_attach_result;
 extern STR name101;
 extern STR RECVFO1329722496;
 extern STR name76;
 extern STR AT_ATT2128087939;
 INT L47;
 INT L481_;
 extern STR name4;
 extern STR Simple512463066;
 extern STR name54;
 extern STR name79;
 extern STR extern1;
 extern STR name13;
 FSTR L49;
 dSTR L50;
 dest1 = CGEN_e1639504572(self, ATTR(ate,at));
 e = ATTR(ate,e);
 switch (TAG(e)) {
  case AM_ROU1916046290_tag:
   ca = ((AM_ROU1916046290) e);
   if (ATTR(ATTR(self,prog),fast_at2)) {
    L3=SIG_ge642497605(ATTR(((AM_ROU1916046290) e),fun), ATTR(self,prog));
    if (ATTR(L3,may_block)) {
     may_block = TRUE;
    }
    else {
     may_block = FALSE;
    }
   }
   else {
    may_block = TRUE;
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin CGEN::emit_simple_am_at_expr(AM_AT_EXPR):STR\n./Back/cgen.sa:6786:14");
 }
 L4 = (ATTR(ATTR(ca,fun),ret)==((dTP) NULL));
 L51_=!(L4); 
 if (L51_) {
  res = CGEN_d1425664957(self, ATTR(ATTR(ca,fun),ret));
 }
 comment_self = self;
 comment_com = ((STR) &Simple351050430);
 if (ATTR(comment_self,pretty)) {
  ndefer_self = comment_self;
  plus_self = ((STR) &name54);
  plus_sarg = comment_com;
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val1;
  plus_sarg1 = ((STR) &name79);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s = ret_val2;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 }
 ndefer_self1 = self;
 ndefer_s1 = STR_ap1693864410(((STR) &ifBR_HERE), dest1, ((STR) &name99));
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 in_self = self;
 L6 = ATTR(in_self,indent);
 L71_=INTPLUS(L6,1); 
 SATTR(in_self,indent,L71_);
 call_str = CGEN_e1639504572(self, ((dAM_EXPR) ca));
 L8 = (ATTR(ATTR(ca,fun),ret)==((dTP) NULL));
 L91_=!(L8); 
 if (L91_) {
  ndefer_self2 = self;
  ndefer_s2 = STR_ap1417971546(res, ((STR) &name33), call_str, ((STR) &name16));
  CGEN_d695831906(ndefer_self2);
  SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 }
 else {
  ndefer_self3 = self;
  plus_self2 = call_str;
  plus_sarg2 = ((STR) &name16);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  ndefer_s3 = ret_val3;
  CGEN_d695831906(ndefer_self3);
  SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
 }
 move_out_self = self;
 L10 = ATTR(move_out_self,indent);
 L121_=INTMINUS(L10,1); 
 SATTR(move_out_self,indent,L121_);
 ndefer_self4 = self;
 ndefer_s4 = ((STR) &else4);
 CGEN_d695831906(ndefer_self4);
 SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
 in_self1 = self;
 L13 = ATTR(in_self1,indent);
 L141_=INTPLUS(L13,1); 
 SATTR(in_self1,indent,L141_);
 ndefer_self5 = self;
 ndefer_s5 = ((STR) &AT_ATT216595904);
 CGEN_d695831906(ndefer_self5);
 SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
 ndefer_self6 = self;
 ndefer_s6 = ((STR) &AT_ATT601389983);
 CGEN_d695831906(ndefer_self6);
 SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
 arg_list = CGEN_e1933193154(self, ((dAM_CALL_EXPR) ca));
 L151_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
 L16 = L151_;
 L171_=INTPLUS(L16,2); 
 num_args = L171_;
 if (may_block) {
  req_str = ((STR) &BR_FORK_);
 }
 else {
  req_str = ((STR) &BR_REQUEST_);
 }
 str_self = num_args;
 clear_self = buf1;
 L18 = (clear_self==((FSTR) NULL));
 L191_=!(L18); 
 if (L191_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val5 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val5;
 str_self1 = buf1;
 ret_val6 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val4 = ret_val6;
 req_str = STR_ap44172742(req_str, ret_val4, ((STR) &name24), dest1, ((STR) &name3));
 mang_self = self;
 mang_ob = ((OB) ate);
 ret_val7 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 req_str = STR_ap1693864410(req_str, ret_val7, ((STR) &name3));
 plus_self3 = req_str;
 plus_sarg3 = ((STR) &BR_wor1065271233);
 ret_val8 = STR_ap2004550586(plus_self3, plus_sarg3);
 req_str = ret_val8;
 L20 = ATTR(ATTR(ca,fun),tp);
 if ((*dTP_is1334578382[TAG(L20)])(L20)) {
  L221_=(STR)ARR((ARRAYSTR)arg_list,0); 
  req_str = STR_ap1693864410(req_str, ((STR) &BR_word_t2), L221_);
 }
 else {
  L231_=(STR)ARR((ARRAYSTR)arg_list,0); 
  L24 = L231_;
  req_str = STR_ap1925291910(req_str, ((STR) &BR_word_tSENDFOB), L24, ((STR) &name3), dest1, ((STR) &name26));
 }
 {
  BOOL f_L251_ = TRUE;
  while (1) {
   if (f_L251_) {
    f_L251_ = FALSE;
    L11 = 1;
    L271_=(ca)==NULL?0:ASIZE((AM_ROU1916046290)ca); 
    L28 = L271_;
    L291_=INTMINUS(L28,1); 
    L21 = L291_;
    L251_=L11-1;L251_m=L21; 
   }
   if(L251_++>=L251_m)  goto after_loop; 
   i = L251_;
   L30 = ATTR(ATTR(ca,fun),args);
   L311_=INTMINUS(i,1); 
   L32 = L311_;
   L331_=(ARG)ARR((ARRAYARG)L30,L32); 
   sa1 = L331_;
   L34 = ATTR(sa1,tp);
   if ((*dTP_is1334578382[TAG(L34)])(L34)) {
    L35 = ATTR(sa1,tp);
    L36 = (*dTP_is418055720[TAG(L35)])(L35);
    L371_=!(L36); 
    if (L371_) {
     barf_self = self;
     barf_msg = ((STR) &nonato642173603);
     barf_self1 = ATTR(barf_self,prog);
     barf_msg1 = barf_msg;
     barf_at_self = barf_self1;
     barf_at_msg = barf_msg1;
     barf_at_at = ((dPROG_ERR) NULL);
     PROG_e176405615(barf_at_self, barf_at_at);
     plus_self4 = ((STR) &Intern28965746);
     plus_sarg4 = barf_at_msg;
     ret_val9 = STR_ap2004550586(plus_self4, plus_sarg4);
     PROG_err_STR(barf_at_self, ret_val9);
     exit_self = ((UNIX) NULL);
     exit_code = 1;
     exit(exit_code);
    }
    plus_self5 = ((STR) &BR_word_t3);
    L381_=(STR)ARR((ARRAYSTR)arg_list,i); 
    plus_sarg5 = L381_;
    ret_val10 = STR_ap2004550586(plus_self5, plus_sarg5);
    arg_str = ret_val10;
   }
   else {
    L391_=(STR)ARR((ARRAYSTR)arg_list,i); 
    L40 = L391_;
    arg_str = STR_ap1925291910(((STR) &BR_word_t3), ((STR) &SENDFOB1), L40, ((STR) &name3), dest1, ((STR) &name26));
   }
   req_str = STR_ap1693864410(req_str, ((STR) &name3), arg_str);
  }
 }
 after_loop: ;
 plus_self6 = req_str;
 plus_sarg6 = ((STR) &name76);
 ret_val11 = STR_ap2004550586(plus_self6, plus_sarg6);
 req_str = ret_val11;
 ndefer_self7 = self;
 ndefer_s7 = req_str;
 CGEN_d695831906(ndefer_self7);
 SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
 ndefer_self8 = self;
 ndefer_s8 = ((STR) &AT_ATT1091925463);
 CGEN_d695831906(ndefer_self8);
 SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
 L41 = (ATTR(ATTR(ca,fun),ret)==((dTP) NULL));
 L421_=!(L41); 
 if (L421_) {
  tp_self = ca;
  ret_val12 = ATTR(ATTR(tp_self,fun),ret);
  L43 = ret_val12;
  if ((*dTP_is1334578382[TAG(L43)])(L43)) {
   tp_self1 = ca;
   ret_val13 = ATTR(ATTR(tp_self1,fun),ret);
   L44 = ret_val13;
   L45 = (*dTP_is418055720[TAG(L44)])(L44);
   L461_=!(L45); 
   if (L461_) {
    ndefer_self9 = self;
    plus_self7 = ((STR) &RECVOB1);
    tag_for_self = self;
    tp_self2 = ca;
    ret_val14 = ATTR(ATTR(tp_self2,fun),ret);
    tag_for_tp = ret_val14;
    SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
    mang_self1 = tag_for_self;
    mang_ob1 = ((OB) tag_for_tp);
    ret_val16 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
    res1 = STR_ap2004550586(ret_val16, ((STR) &S_tag));
    forbid_self = tag_for_self;
    forbid_s = res1;
    forbid_self1 = ATTR(forbid_self,mangler);
    forbid_s1 = forbid_s;
    SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
    ret_val15 = res1;
    plus_sarg7 = ret_val15;
    ret_val17 = STR_ap2004550586(plus_self7, plus_sarg7);
    plus_self8 = ret_val17;
    plus_sarg8 = ((STR) &name100);
    ret_val18 = STR_ap2004550586(plus_self8, plus_sarg8);
    plus_self9 = ret_val18;
    plus_sarg9 = ((STR) &at_attachresult);
    ret_val19 = STR_ap2004550586(plus_self9, plus_sarg9);
    plus_self10 = ret_val19;
    plus_sarg10 = dest1;
    ret_val20 = STR_ap2004550586(plus_self10, plus_sarg10);
    plus_self11 = ret_val20;
    plus_sarg11 = ((STR) &name76);
    ret_val21 = STR_ap2004550586(plus_self11, plus_sarg11);
    ndefer_s9 = ret_val21;
    CGEN_d695831906(ndefer_self9);
    SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
    ndefer_self10 = self;
    mang_self2 = self;
    mang_ob2 = ((OB) ATTR(ATTR(ca,fun),ret));
    ret_val22 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
    ndefer_s10 = STR_ap1417971546(res, ((STR) &name33), ret_val22, ((STR) &at_attach_result));
    CGEN_d695831906(ndefer_self10);
    SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
   }
  }
  else {
   ndefer_self11 = self;
   mang_self3 = self;
   mang_ob3 = ((OB) ATTR(ATTR(ca,fun),ret));
   ret_val23 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
   ndefer_s11 = STR_ap1925291910(res, ((STR) &name101), ret_val23, ((STR) &RECVFO1329722496), dest1, ((STR) &name76));
   CGEN_d695831906(ndefer_self11);
   SATTR(ndefer_self11,routine_code,FSTR_p1752847026(ATTR(ndefer_self11,routine_code), ndefer_s11));
  }
 }
 ndefer_self12 = self;
 ndefer_s12 = ((STR) &AT_ATT2128087939);
 CGEN_d695831906(ndefer_self12);
 SATTR(ndefer_self12,routine_code,FSTR_p1752847026(ATTR(ndefer_self12,routine_code), ndefer_s12));
 move_out_self1 = self;
 L47 = ATTR(move_out_self1,indent);
 L481_=INTMINUS(L47,1); 
 SATTR(move_out_self1,indent,L481_);
 ndefer_self13 = self;
 ndefer_s13 = ((STR) &name4);
 CGEN_d695831906(ndefer_self13);
 SATTR(ndefer_self13,routine_code,FSTR_p1752847026(ATTR(ndefer_self13,routine_code), ndefer_s13));
 comment_self1 = self;
 comment_com1 = ((STR) &Simple512463066);
 if (ATTR(comment_self1,pretty)) {
  ndefer_self14 = comment_self1;
  plus_self12 = ((STR) &name54);
  plus_sarg12 = comment_com1;
  ret_val24 = STR_ap2004550586(plus_self12, plus_sarg12);
  plus_self13 = ret_val24;
  plus_sarg13 = ((STR) &name79);
  ret_val25 = STR_ap2004550586(plus_self13, plus_sarg13);
  ndefer_s14 = ret_val25;
  CGEN_d695831906(ndefer_self14);
  SATTR(ndefer_self14,routine_code,FSTR_p1752847026(ATTR(ndefer_self14,routine_code), ndefer_s14));
 }
 decl = CGEN_e712006663(self, ate, ca, may_block);
 plus_self14 = ATTR(self,code_c);
 plus_s = ((dSTR) STR_ap1693864410(((STR) &extern1), decl, ((STR) &name13)));
 L49 = ATTR(plus_self14,ntext);
 L50 = plus_s;
 SATTR(plus_self14,ntext,FSTR_p1752847026(L49, (*dSTR_strrSTR[TAG(L50)])(L50)));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e409242292(CGEN self, AM_BND1124877163 arg, BOOL casting, FLISTSTR* argstr) {
 FLISTSTR L1 = ((FLISTSTR) NULL);/*Local for arg*/
 STR ret_val;
 AM_BND1124877163 carg;
 FLISTA1893461511 L21;
 AM_BND1124877163 c;
 FLISTA1893461511 L31;
 AM_BND1124877163 c11;
 STR res;
 AM_BND1124877163 L41;
 INT i = INT_zero;
 STR entry1 = ((STR) NULL);
 INT idx = INT_zero;
 SIG a;
 ARG a11;
 STR ex1;
 CODE_FILE uses_bnd_rout_cr;
 AM_BND1124877163 uses_bnd_rout_cr1;
 BOUND_1677815802 create_self;
 AM_BND1124877163 create_e;
 BOUND_1677815802 ret_val1;
 BOUND_1677815802 res1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 MANGLE genlocal_self;
 OB genlocal_ns;
 STR ret_val3;
 STR name111;
 MANGLE space_self;
 OB space_ns;
 NAMESPACE ret_val4;
 NAMESPACE r;
 NAMESPACE create_self1;
 NAMESPACE ret_val5;
 NAMESPACE r1;
 NAMEMAP create_self2;
 NAMEMAP ret_val6;
 FSETSTR create_self3;
 FSETSTR ret_val7;
 FMAPSTRINT create_self4;
 FMAPSTRINT ret_val8;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE ret_val9;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE ret_val10;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val11;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CODE_FILE ret_val12;
 CODE_FILE plus_self4;
 dSTR plus_s3;
 CODE_FILE ret_val13;
 CODE_FILE plus_self5;
 dSTR plus_s4;
 CODE_FILE ret_val14;
 CODE_FILE plus_self6;
 dSTR plus_s5;
 STR plus_self7;
 STR plus_sarg1;
 STR ret_val15;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val16;
 STR plus_self8;
 STR plus_sarg2;
 STR ret_val17;
 STR plus_self9;
 STR plus_sarg3;
 STR ret_val18;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val19;
 STR plus_self10;
 STR plus_sarg4;
 STR ret_val20;
 STR plus_self11;
 STR plus_sarg5;
 STR ret_val21;
 CGEN ndefer_self;
 STR ndefer_s;
 STR plus_self12;
 STR plus_sarg6;
 STR ret_val22;
 AM_BND1124877163 tp_self;
 dTP ret_val23;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val24;
 STR res2;
 CGEN mang_self3;
 OB mang_ob3;
 STR ret_val25;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self13;
 STR plus_sarg7;
 STR ret_val26;
 STR plus_self14;
 CHAR plus_arg = CHAR_zero;
 STR ret_val27;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val28;
 STR create_self5;
 CHAR create_c = CHAR_zero;
 STR ret_val29;
 STR r2;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self15;
 STR plus_sarg8;
 STR ret_val30;
 CGEN mang_self4;
 OB mang_ob4;
 STR ret_val31;
 STR plus_self16;
 STR plus_sarg9;
 STR ret_val32;
 STR plus_self17;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val33;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val34;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val35;
 STR r3;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT L51 = INT_zero;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val36;
 STR plus_self18;
 STR plus_sarg10;
 STR ret_val37;
 STR plus_self19;
 INT plus_arg2 = INT_zero;
 STR ret_val38;
 STR s2;
 INT str_self2 = INT_zero;
 STR ret_val39;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val40;
 FSTR str_self3;
 STR ret_val41;
 STR plus_self20;
 STR plus_sarg11;
 STR ret_val42;
 STR plus_self21;
 STR plus_sarg12;
 STR ret_val43;
 STR plus_self22;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val44;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val45;
 STR create_self7;
 CHAR create_c2 = CHAR_zero;
 STR ret_val46;
 STR r4;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN tag_for_self1;
 dTP tag_for_tp1;
 STR ret_val47;
 STR res3;
 CGEN mang_self5;
 OB mang_ob5;
 STR ret_val48;
 CGEN forbid_self2;
 STR forbid_s2;
 MANGLE forbid_self3;
 STR forbid_s3;
 STR plus_self23;
 STR plus_sarg13;
 STR ret_val49;
 STR plus_self24;
 STR plus_sarg14;
 STR ret_val50;
 STR plus_self25;
 STR plus_sarg15;
 STR ret_val51;
 STR plus_self26;
 STR plus_sarg16;
 STR ret_val52;
 STR plus_self27;
 INT plus_arg4 = INT_zero;
 STR ret_val53;
 STR s4;
 INT str_self5 = INT_zero;
 STR ret_val54;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val55;
 FSTR str_self6;
 STR ret_val56;
 STR plus_self28;
 STR plus_sarg17;
 STR ret_val57;
 STR plus_self29;
 STR plus_sarg18;
 STR ret_val58;
 STR plus_self30;
 STR plus_sarg19;
 STR ret_val59;
 CGEN ndefer_self4;
 STR ndefer_s4;
 STR plus_self31;
 STR plus_sarg20;
 STR ret_val60;
 STR plus_self32;
 INT plus_arg5 = INT_zero;
 STR ret_val61;
 STR s5;
 INT str_self7 = INT_zero;
 STR ret_val62;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val63;
 FSTR str_self8;
 STR ret_val64;
 STR plus_self33;
 STR plus_sarg21;
 STR ret_val65;
 STR plus_self34;
 STR plus_sarg22;
 STR ret_val66;
 STR plus_self35;
 STR plus_sarg23;
 STR ret_val67;
 STR plus_self36;
 STR plus_sarg24;
 STR ret_val68;
 STR plus_self37;
 STR plus_sarg25;
 STR ret_val69;
 CGEN ndefer_self5;
 STR ndefer_s5;
 STR plus_self38;
 STR plus_sarg26;
 STR ret_val70;
 STR plus_self39;
 INT plus_arg6 = INT_zero;
 STR ret_val71;
 STR s6;
 INT str_self9 = INT_zero;
 STR ret_val72;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val73;
 FSTR str_self10;
 STR ret_val74;
 STR plus_self40;
 STR plus_sarg27;
 STR ret_val75;
 STR plus_self41;
 STR plus_sarg28;
 STR ret_val76;
 STR plus_self42;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val77;
 STR s7;
 CHAR str_self11 = CHAR_zero;
 STR ret_val78;
 STR create_self8;
 CHAR create_c3 = CHAR_zero;
 STR ret_val79;
 STR r5;
 CGEN ndefer_self6;
 STR ndefer_s6;
 CGEN cast_self1;
 dTP cast_dest_tp1;
 dTP cast_src_tp1;
 STR cast_expr1;
 BOOL cast_only_boxing1 = BOOL_zero;
 STR ret_val80;
 STR plus_self43;
 STR plus_sarg29;
 STR ret_val81;
 STR plus_self44;
 INT plus_arg8 = INT_zero;
 STR ret_val82;
 STR s8;
 INT str_self12 = INT_zero;
 STR ret_val83;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val84;
 FSTR str_self13;
 STR ret_val85;
 STR plus_self45;
 STR plus_sarg30;
 STR ret_val86;
 STR plus_self46;
 STR plus_sarg31;
 STR ret_val87;
 STR plus_self47;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val88;
 STR s9;
 CHAR str_self14 = CHAR_zero;
 STR ret_val89;
 STR create_self9;
 CHAR create_c4 = CHAR_zero;
 STR ret_val90;
 STR r6;
 CGEN ndefer_self7;
 STR ndefer_s7;
 CGEN tag_for_self2;
 dTP tag_for_tp2;
 STR ret_val91;
 STR res4;
 CGEN mang_self6;
 OB mang_ob6;
 STR ret_val92;
 CGEN forbid_self4;
 STR forbid_s4;
 MANGLE forbid_self5;
 STR forbid_s5;
 STR plus_self48;
 STR plus_sarg32;
 STR ret_val93;
 STR plus_self49;
 STR plus_sarg33;
 STR ret_val94;
 STR plus_self50;
 STR plus_sarg34;
 STR ret_val95;
 STR plus_self51;
 STR plus_sarg35;
 STR ret_val96;
 STR plus_self52;
 INT plus_arg10 = INT_zero;
 STR ret_val97;
 STR s10;
 INT str_self15 = INT_zero;
 STR ret_val98;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val99;
 FSTR str_self16;
 STR ret_val100;
 STR plus_self53;
 STR plus_sarg36;
 STR ret_val101;
 STR plus_self54;
 STR plus_sarg37;
 STR ret_val102;
 STR plus_self55;
 STR plus_sarg38;
 STR ret_val103;
 CGEN ndefer_self8;
 STR ndefer_s8;
 STR plus_self56;
 STR plus_sarg39;
 STR ret_val104;
 STR plus_self57;
 INT plus_arg11 = INT_zero;
 STR ret_val105;
 STR s11;
 INT str_self17 = INT_zero;
 STR ret_val106;
 FSTR buf7 = ((FSTR) NULL);
 FSTR clear_self6;
 INT str_in_self6 = INT_zero;
 FSTR str_in_s6;
 FSTR ret_val107;
 FSTR str_self18;
 STR ret_val108;
 STR plus_self58;
 STR plus_sarg40;
 STR ret_val109;
 STR plus_self59;
 STR plus_sarg41;
 STR ret_val110;
 STR plus_self60;
 STR plus_sarg42;
 STR ret_val1111;
 STR plus_self61;
 STR plus_sarg43;
 STR ret_val112;
 STR plus_self62;
 STR plus_sarg44;
 STR ret_val113;
 CGEN ndefer_self9;
 STR ndefer_s9;
 STR plus_self63;
 STR plus_sarg45;
 STR ret_val114;
 STR plus_self64;
 INT plus_arg12 = INT_zero;
 STR ret_val115;
 STR s12;
 INT str_self19 = INT_zero;
 STR ret_val116;
 FSTR buf8 = ((FSTR) NULL);
 FSTR clear_self7;
 INT str_in_self7 = INT_zero;
 FSTR str_in_s7;
 FSTR ret_val117;
 FSTR str_self20;
 STR ret_val118;
 STR plus_self65;
 STR plus_sarg46;
 STR ret_val119;
 STR plus_self66;
 STR plus_sarg47;
 STR ret_val120;
 STR plus_self67;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val121;
 STR s13;
 CHAR str_self21 = CHAR_zero;
 STR ret_val122;
 STR create_self10;
 CHAR create_c5 = CHAR_zero;
 STR ret_val123;
 STR r7;
 CGEN ndefer_self10;
 STR ndefer_s10;
 CGEN make_sure_emitte;
 SIG make_sure_emitte1;
 AM_ROUT_DEF ard;
 AM_BND1124877163 tp_self1;
 dTP ret_val124;
 CGEN mang_self7;
 OB mang_ob7;
 STR ret_val125;
 STR plus_self68;
 STR plus_sarg48;
 STR ret_val126;
 STR plus_self69;
 STR plus_sarg49;
 STR ret_val127;
 STR plus_self70;
 STR plus_sarg50;
 STR ret_val128;
 AM_BND1124877163 L6;
 AM_BND1124877163 L7;
 BOUND_1677815802 L8;
 OB L9;
 extern STR boundobject;
 NAMESPACE L10;
 OB L11;
 extern STR L;
 BOOL L12;
 BOOL L131_;
 FSTR L14;
 dSTR L15;
 dSTR L16;
 OB L17;
 BOOL L18;
 BOOL L191_;
 FSTR L20;
 dSTR L22;
 BOOL L23;
 BOOL L241_;
 FSTR L25;
 dSTR L26;
 extern STR S_ob;
 BOOL L27;
 BOOL L281_;
 FSTR L29;
 dSTR L30;
 BOOL L32;
 BOOL L331_;
 FSTR L34;
 dSTR L35;
 dSTR L36;
 OB L37;
 FSTR L38;
 dSTR L39;
 extern STR name80;
 extern STR S_obZA389105886;
 extern STR S_ob_struct;
 extern STR headertag1;
 extern STR S_tag;
 STR L40;
 INT L42;
 OB L43;
 INT L44;
 extern STR funcptr1;
 STR L46;
 INT L47;
 OB L48;
 INT L49;
 INT L521_br;
 INT rL521_;
 INT L53;
 ARRAYINT L54;
 INT L551_;
 BOOL L561_;
 dTP L57;
 BOOL L58;
 BOOL L591_;
 AM_CALL_ARG L601_;
 AM_CALL_ARG L61;
 dAM_EXPR L62;
 AM_CALL_ARG L631_;
 AM_CALL_ARG L64;
 dTP L65;
 extern STR bound_arg;
 BOOL L66;
 BOOL L671_;
 extern STR name36;
 STR L68;
 INT L69;
 OB L70;
 INT L71;
 dTP L73;
 BOOL L74;
 BOOL L751_;
 extern STR SENDOB1;
 extern STR S_tag;
 extern STR name100;
 extern STR bound_arg;
 BOOL L76;
 BOOL L771_;
 extern STR name3;
 extern STR name76;
 extern STR bound_arg;
 BOOL L78;
 BOOL L791_;
 extern STR SENDFOB2;
 extern STR name3;
 extern STR name76;
 extern STR bound_arg;
 BOOL L80;
 BOOL L811_;
 extern STR name36;
 STR L82;
 INT L83;
 OB L84;
 INT L85;
 ARRAYARG L87;
 INT L881_;
 INT L89;
 ARG L901_;
 dMODE L91;
 BOOL L92;
 BOOL L931_;
 AM_CALL_ARG L941_;
 AM_CALL_ARG L95;
 dMODE L96;
 AM_CALL_ARG L971_;
 AM_CALL_ARG L98;
 dAM_EXPR L99;
 dTP L100;
 extern STR bound_arg;
 BOOL L101;
 BOOL L1021_;
 extern STR name36;
 STR L103;
 INT L104;
 OB L105;
 INT L106;
 dTP L108;
 BOOL L109;
 BOOL L1101_;
 extern STR SENDOB1;
 extern STR S_tag;
 extern STR name100;
 extern STR bound_arg;
 BOOL L111;
 BOOL L1121_;
 extern STR name3;
 extern STR name76;
 extern STR bound_arg;
 BOOL L113;
 BOOL L1141_;
 extern STR SENDFOB2;
 extern STR name3;
 extern STR name76;
 extern STR bound_arg;
 BOOL L115;
 BOOL L1161_;
 extern STR name36;
 STR L117;
 INT L118;
 OB L119;
 INT L120;
 INT L1221_;
 BOOL L123;
 BOOL L1241_;
 extern STR name24;
 extern STR name23;
 L1 = ((FLISTSTR) NULL);
 carg = ((AM_BND1124877163) NULL);
 {
  struct FLISTA273121893_frame_struct other1_0;
FLISTA273121893_frame noname1 = &other1_0;
  L21 = ATTR(self,bnd_rout_creates);
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   L6 = FLISTA273121893(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L6;
   if (AM_BND89426869(c, arg)) {
    carg = c;
    goto after_loop;
   }
  }
 }
 after_loop: ;
 if ((carg==((AM_BND1124877163) NULL))) {
  {
   struct FLISTA273121893_frame_struct other2_0;
FLISTA273121893_frame noname2 = &other2_0;
   L31 = ATTR(self,bnd_rout_creates1);
   noname2->self = L31;
   noname2->state = 0;
   while (1) {
    L7 = FLISTA273121893(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    c11 = L7;
    if (AM_BND89426869(c11, arg)) {
     carg = c11;
     goto after_loop1;
    }
   }
  }
  after_loop1: ;
 }
 if ((carg==((AM_BND1124877163) NULL))) {
  SATTR(self,bnd_rout_creates,FLISTA80425519(ATTR(self,bnd_rout_creates), arg));
  carg = arg;
 }
 uses_bnd_rout_cr = ATTR(self,code_c);
 uses_bnd_rout_cr1 = carg;
 SATTR(uses_bnd_rout_cr,bnd_rout_creates,FSETAM1990484536(ATTR(uses_bnd_rout_cr,bnd_rout_creates), uses_bnd_rout_cr1));
 create_self = ((BOUND_1677815802) NULL);
 create_e = carg;
 res1 = FMAPAM661174734(BOUND_2051717332, create_e);
 if ((res1==((BOUND_1677815802) NULL))) {
  L9=ZALLOC(sizeof(struct BOUND_1677815802_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=BOUND_1677815802_tag;
  L8 = ((BOUND_1677815802) L9);
  res1 = L8;
  SATTR(res1,e,create_e);
  plus_self = ATTR(ATTR(ATTR(res1,e),fun),str);
  plus_sarg = ((STR) &boundobject);
  ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
  SATTR(res1,str,ret_val2);
  BOUND_2051717332 = FMAPAM1106357779(BOUND_2051717332, create_e, res1);
 }
 ret_val1 = res1;
 CODE_F525364269(ATTR(self,code_c), ((dLAYOUT) ret_val1));
 genlocal_self = ATTR(self,mangler);
 genlocal_ns = ((OB) ATTR(self,current_sig));
 space_self = genlocal_self;
 space_ns = genlocal_ns;
 if ((space_ns==((OB) NULL))) {
  space_ns = ((OB) space_self);
 }
 r = FMAPdO790518483(ATTR(space_self,namespaces), space_ns);
 if ((r==((NAMESPACE) NULL))) {
  create_self1 = ((NAMESPACE) NULL);
  L11=ZALLOC(sizeof(struct NAMESPACE_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=NAMESPACE_tag;
  L10 = ((NAMESPACE) L11);
  r1 = L10;
  create_self2 = ((NAMEMAP) NULL);
  ret_val6 = ((NAMEMAP) NULL);
  SATTR(r1,map,ret_val6);
  create_self3 = ((FSETSTR) NULL);
  ret_val7 = ((FSETSTR) NULL);
  SATTR(r1,set,ret_val7);
  SATTR(r1,counter,0);
  create_self4 = ((FMAPSTRINT) NULL);
  ret_val8 = ((FMAPSTRINT) NULL);
  SATTR(r1,uniques,ret_val8);
  ret_val5 = r1;
  r = ret_val5;
  SATTR(space_self,namespaces,FMAPdO1199466663(ATTR(space_self,namespaces), space_ns, r));
 }
 ret_val4 = r;
 name111 = NAMESP180237388(ret_val4, ((STR) &L));
 SATTR(genlocal_self,used_by_local,FSETST1404644833(ATTR(genlocal_self,used_by_local), name111));
 ret_val3 = name111;
 res = ret_val3;
 plus_self1 = ATTR(self,code_c);
 plus_s = ((dSTR) CGEN_eolrSTR(self));
 L12 = (plus_s==((dSTR) NULL));
 L131_=!(L12); 
 if (L131_) {
  L14 = ATTR(plus_self1,ntext);
  L15 = plus_s;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L14, (*dSTR_strrSTR[TAG(L15)])(L15)));
 }
 ret_val9 = plus_self1;
 plus_self2 = ret_val9;
 L17=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 memset(L17,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L17)->header.tag=CHAR_tag;
 L16 = (dSTR)((CHAR_boxed) L17);
 ((CHAR_boxed) L16)->immutable_part = ' ';
 plus_s1 = L16;
 L18 = (plus_s1==((dSTR) NULL));
 L191_=!(L18); 
 if (L191_) {
  L20 = ATTR(plus_self2,ntext);
  L22 = plus_s1;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L20, (*dSTR_strrSTR[TAG(L22)])(L22)));
 }
 ret_val10 = plus_self2;
 plus_self3 = ret_val10;
 mang_self = self;
 mang_ob = ((OB) carg);
 ret_val11 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 plus_s2 = ((dSTR) ret_val11);
 L23 = (plus_s2==((dSTR) NULL));
 L241_=!(L23); 
 if (L241_) {
  L25 = ATTR(plus_self3,ntext);
  L26 = plus_s2;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 }
 ret_val12 = plus_self3;
 plus_self4 = ret_val12;
 plus_s3 = ((dSTR) ((STR) &S_ob));
 L27 = (plus_s3==((dSTR) NULL));
 L281_=!(L27); 
 if (L281_) {
  L29 = ATTR(plus_self4,ntext);
  L30 = plus_s3;
  SATTR(plus_self4,ntext,FSTR_p1752847026(L29, (*dSTR_strrSTR[TAG(L30)])(L30)));
 }
 ret_val13 = plus_self4;
 plus_self5 = ret_val13;
 plus_s4 = ((dSTR) res);
 L32 = (plus_s4==((dSTR) NULL));
 L331_=!(L32); 
 if (L331_) {
  L34 = ATTR(plus_self5,ntext);
  L35 = plus_s4;
  SATTR(plus_self5,ntext,FSTR_p1752847026(L34, (*dSTR_strrSTR[TAG(L35)])(L35)));
 }
 ret_val14 = plus_self5;
 plus_self6 = ret_val14;
 L37=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 memset(L37,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L37)->header.tag=CHAR_tag;
 L36 = (dSTR)((CHAR_boxed) L37);
 ((CHAR_boxed) L36)->immutable_part = ';';
 plus_s5 = L36;
 L38 = ATTR(plus_self6,ntext);
 L39 = plus_s5;
 SATTR(plus_self6,ntext,FSTR_p1752847026(L38, (*dSTR_strrSTR[TAG(L39)])(L39)));
 ndefer_self = self;
 plus_self7 = res;
 plus_sarg1 = ((STR) &name80);
 ret_val15 = STR_ap2004550586(plus_self7, plus_sarg1);
 plus_self8 = ret_val15;
 mang_self1 = self;
 mang_ob1 = ((OB) carg);
 ret_val16 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
 plus_sarg2 = ret_val16;
 ret_val17 = STR_ap2004550586(plus_self8, plus_sarg2);
 plus_self9 = ret_val17;
 plus_sarg3 = ((STR) &S_obZA389105886);
 ret_val18 = STR_ap2004550586(plus_self9, plus_sarg3);
 plus_self10 = ret_val18;
 mang_self2 = self;
 mang_ob2 = ((OB) carg);
 ret_val19 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
 plus_sarg4 = ret_val19;
 ret_val20 = STR_ap2004550586(plus_self10, plus_sarg4);
 plus_self11 = ret_val20;
 plus_sarg5 = ((STR) &S_ob_struct);
 ret_val21 = STR_ap2004550586(plus_self11, plus_sarg5);
 ndefer_s = ret_val21;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 ndefer_self1 = self;
 plus_self12 = res;
 plus_sarg6 = ((STR) &headertag1);
 ret_val22 = STR_ap2004550586(plus_self12, plus_sarg6);
 plus_self13 = ret_val22;
 tag_for_self = self;
 tp_self = arg;
 ret_val23 = ATTR(tp_self,tp_at);
 tag_for_tp = ret_val23;
 SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
 mang_self3 = tag_for_self;
 mang_ob3 = ((OB) tag_for_tp);
 ret_val25 = MANGLE119219986(ATTR(mang_self3,mangler), mang_ob3, ((OB) NULL));
 res2 = STR_ap2004550586(ret_val25, ((STR) &S_tag));
 forbid_self = tag_for_self;
 forbid_s = res2;
 forbid_self1 = ATTR(forbid_self,mangler);
 forbid_s1 = forbid_s;
 SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
 ret_val24 = res2;
 plus_sarg7 = ret_val24;
 ret_val26 = STR_ap2004550586(plus_self13, plus_sarg7);
 plus_self14 = ret_val26;
 plus_arg = ';';
 str_self = plus_arg;
 create_self5 = ((STR) NULL);
 create_c = str_self;
 L42 = 1;
 L44=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L42)*sizeof(CHAR);
 L43=ZALLOC_LEAF_BIG(L44);
 if (L43==NULL) FATAL("Unable to allocate more memory");
 memset(L43,0,L44);
 ((OB)L43)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L43)->header.destroyed=0;
#endif

 L40 = ((STR) L43);
 L40->asize = L42;
 r2 = L40;
 SARR((STR)r2,0,create_c); 
 ;
 ret_val29 = r2;
 ret_val28 = ret_val29;
 s = ret_val28;
 ret_val27 = STR_ap1077157958(plus_self14, s, TRUE);
 ndefer_s1 = ret_val27;
 CGEN_d695831906(ndefer_self1);
 SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
 ndefer_self2 = self;
 plus_self15 = res;
 plus_sarg8 = ((STR) &funcptr1);
 ret_val30 = STR_ap2004550586(plus_self15, plus_sarg8);
 plus_self16 = ret_val30;
 mang_self4 = self;
 mang_ob4 = ((OB) carg);
 ret_val31 = MANGLE119219986(ATTR(mang_self4,mangler), mang_ob4, ((OB) NULL));
 plus_sarg9 = ret_val31;
 ret_val32 = STR_ap2004550586(plus_self16, plus_sarg9);
 plus_self17 = ret_val32;
 plus_arg1 = ';';
 str_self1 = plus_arg1;
 create_self6 = ((STR) NULL);
 create_c1 = str_self1;
 L47 = 1;
 L49=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L47)*sizeof(CHAR);
 L48=ZALLOC_LEAF_BIG(L49);
 if (L48==NULL) FATAL("Unable to allocate more memory");
 memset(L48,0,L49);
 ((OB)L48)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L48)->header.destroyed=0;
#endif

 L46 = ((STR) L48);
 L46->asize = L47;
 r3 = L46;
 SARR((STR)r3,0,create_c1); 
 ;
 ret_val35 = r3;
 ret_val34 = ret_val35;
 s1 = ret_val34;
 ret_val33 = STR_ap1077157958(plus_self17, s1, TRUE);
 ndefer_s2 = ret_val33;
 CGEN_d695831906(ndefer_self2);
 SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
 {
  BOOL f_L521_ = TRUE;
  /* loop index variable */
  L51 = 0;
  L41 = arg;
  L521_br=L41==NULL?0:ASIZE((AM_BND1124877163)L41); 
  while (1) {
   if(L51>=L521_br)  goto after_loop2; 
   rL521_=L51; 
   i = rL521_;
   L54 = ATTR(arg,bnd_args);
   L551_=ARR((ARRAYINT)L54,i); 
   idx = L551_;
   L561_=(idx)==(0); 
   if (L561_) {
    L57 = ATTR(ATTR(arg,fun),tp);
    L58 = (*dTP_is124163553[TAG(L57)])(L57);
    L591_=!(L58); 
    if (L591_) {
     a = ATTR(arg,fun);
     cast_self = self;
     cast_dest_tp = ATTR(ATTR(arg,fun),tp);
     L601_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L61=L601_;
     L62 = ATTR(L61,expr);
     cast_src_tp = (*dAM_EXPR_tprdTP[TAG(L62)])(L62);
     L631_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L64=L631_;
     cast_expr = CGEN_e1639504572(self, ATTR(L64,expr));
     cast_only_boxing = FALSE;
     ret_val36 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
     entry1 = ret_val36;
     if (ATTR(arg,is_remote)) {
      L65 = ATTR(a,tp);
      if ((*dTP_is1334578382[TAG(L65)])(L65)) {
       ndefer_self3 = self;
       plus_self18 = res;
       plus_sarg10 = ((STR) &bound_arg);
       ret_val37 = STR_ap2004550586(plus_self18, plus_sarg10);
       plus_self19 = ret_val37;
       plus_arg2 = i;
       str_self2 = plus_arg2;
       clear_self = buf1;
       L66 = (clear_self==((FSTR) NULL));
       L671_=!(L66); 
       if (L671_) {
        SATTR(clear_self,loc,0);
       }
       str_in_self = str_self2;
       str_in_s = buf1;
       ret_val40 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
       buf1 = ret_val40;
       str_self3 = buf1;
       ret_val41 = STR_fr1097270334(((STR) NULL), str_self3);
       ret_val39 = ret_val41;
       s2 = ret_val39;
       ret_val38 = STR_ap1077157958(plus_self19, s2, TRUE);
       plus_self20 = ret_val38;
       plus_sarg11 = ((STR) &name36);
       ret_val42 = STR_ap2004550586(plus_self20, plus_sarg11);
       plus_self21 = ret_val42;
       plus_sarg12 = entry1;
       ret_val43 = STR_ap2004550586(plus_self21, plus_sarg12);
       plus_self22 = ret_val43;
       plus_arg3 = ';';
       str_self4 = plus_arg3;
       create_self7 = ((STR) NULL);
       create_c2 = str_self4;
       L69 = 1;
       L71=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L69)*sizeof(CHAR);
       L70=ZALLOC_LEAF_BIG(L71);
       if (L70==NULL) FATAL("Unable to allocate more memory");
       memset(L70,0,L71);
       ((OB)L70)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L70)->header.destroyed=0;
#endif

       L68 = ((STR) L70);
       L68->asize = L69;
       r4 = L68;
       SARR((STR)r4,0,create_c2); 
       ;
       ret_val46 = r4;
       ret_val45 = ret_val46;
       s3 = ret_val45;
       ret_val44 = STR_ap1077157958(plus_self22, s3, TRUE);
       ndefer_s3 = ret_val44;
       CGEN_d695831906(ndefer_self3);
       SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
       L73 = ATTR(a,tp);
       L74 = (*dTP_is418055720[TAG(L73)])(L73);
       L751_=!(L74); 
       if (L751_) {
        ndefer_self4 = self;
        plus_self23 = ((STR) &SENDOB1);
        tag_for_self1 = self;
        tag_for_tp1 = ATTR(a,tp);
        SATTR(tag_for_self1,needs_tag,FSETdT1016814448(ATTR(tag_for_self1,needs_tag), tag_for_tp1));
        mang_self5 = tag_for_self1;
        mang_ob5 = ((OB) tag_for_tp1);
        ret_val48 = MANGLE119219986(ATTR(mang_self5,mangler), mang_ob5, ((OB) NULL));
        res3 = STR_ap2004550586(ret_val48, ((STR) &S_tag));
        forbid_self2 = tag_for_self1;
        forbid_s2 = res3;
        forbid_self3 = ATTR(forbid_self2,mangler);
        forbid_s3 = forbid_s2;
        SATTR(forbid_self3,forbidden,FSETST1404644833(ATTR(forbid_self3,forbidden), forbid_s3));
        ret_val47 = res3;
        plus_sarg13 = ret_val47;
        ret_val49 = STR_ap2004550586(plus_self23, plus_sarg13);
        plus_self24 = ret_val49;
        plus_sarg14 = ((STR) &name100);
        ret_val50 = STR_ap2004550586(plus_self24, plus_sarg14);
        plus_self25 = ret_val50;
        plus_sarg15 = res;
        ret_val51 = STR_ap2004550586(plus_self25, plus_sarg15);
        plus_self26 = ret_val51;
        plus_sarg16 = ((STR) &bound_arg);
        ret_val52 = STR_ap2004550586(plus_self26, plus_sarg16);
        plus_self27 = ret_val52;
        plus_arg4 = i;
        str_self5 = plus_arg4;
        clear_self1 = buf2;
        L76 = (clear_self1==((FSTR) NULL));
        L771_=!(L76); 
        if (L771_) {
         SATTR(clear_self1,loc,0);
        }
        str_in_self1 = str_self5;
        str_in_s1 = buf2;
        ret_val55 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
        buf2 = ret_val55;
        str_self6 = buf2;
        ret_val56 = STR_fr1097270334(((STR) NULL), str_self6);
        ret_val54 = ret_val56;
        s4 = ret_val54;
        ret_val53 = STR_ap1077157958(plus_self27, s4, TRUE);
        plus_self28 = ret_val53;
        plus_sarg17 = ((STR) &name3);
        ret_val57 = STR_ap2004550586(plus_self28, plus_sarg17);
        plus_self29 = ret_val57;
        plus_sarg18 = ATTR(arg,clst);
        ret_val58 = STR_ap2004550586(plus_self29, plus_sarg18);
        plus_self30 = ret_val58;
        plus_sarg19 = ((STR) &name76);
        ret_val59 = STR_ap2004550586(plus_self30, plus_sarg19);
        ndefer_s4 = ret_val59;
        CGEN_d695831906(ndefer_self4);
        SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
       }
      }
      else {
       ndefer_self5 = self;
       plus_self31 = res;
       plus_sarg20 = ((STR) &bound_arg);
       ret_val60 = STR_ap2004550586(plus_self31, plus_sarg20);
       plus_self32 = ret_val60;
       plus_arg5 = i;
       str_self7 = plus_arg5;
       clear_self2 = buf3;
       L78 = (clear_self2==((FSTR) NULL));
       L791_=!(L78); 
       if (L791_) {
        SATTR(clear_self2,loc,0);
       }
       str_in_self2 = str_self7;
       str_in_s2 = buf3;
       ret_val63 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
       buf3 = ret_val63;
       str_self8 = buf3;
       ret_val64 = STR_fr1097270334(((STR) NULL), str_self8);
       ret_val62 = ret_val64;
       s5 = ret_val62;
       ret_val61 = STR_ap1077157958(plus_self32, s5, TRUE);
       plus_self33 = ret_val61;
       plus_sarg21 = ((STR) &SENDFOB2);
       ret_val65 = STR_ap2004550586(plus_self33, plus_sarg21);
       plus_self34 = ret_val65;
       plus_sarg22 = entry1;
       ret_val66 = STR_ap2004550586(plus_self34, plus_sarg22);
       plus_self35 = ret_val66;
       plus_sarg23 = ((STR) &name3);
       ret_val67 = STR_ap2004550586(plus_self35, plus_sarg23);
       plus_self36 = ret_val67;
       plus_sarg24 = ATTR(arg,clst);
       ret_val68 = STR_ap2004550586(plus_self36, plus_sarg24);
       plus_self37 = ret_val68;
       plus_sarg25 = ((STR) &name76);
       ret_val69 = STR_ap2004550586(plus_self37, plus_sarg25);
       ndefer_s5 = ret_val69;
       CGEN_d695831906(ndefer_self5);
       SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
      }
     }
     else {
      ndefer_self6 = self;
      plus_self38 = res;
      plus_sarg26 = ((STR) &bound_arg);
      ret_val70 = STR_ap2004550586(plus_self38, plus_sarg26);
      plus_self39 = ret_val70;
      plus_arg6 = i;
      str_self9 = plus_arg6;
      clear_self3 = buf4;
      L80 = (clear_self3==((FSTR) NULL));
      L811_=!(L80); 
      if (L811_) {
       SATTR(clear_self3,loc,0);
      }
      str_in_self3 = str_self9;
      str_in_s3 = buf4;
      ret_val73 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
      buf4 = ret_val73;
      str_self10 = buf4;
      ret_val74 = STR_fr1097270334(((STR) NULL), str_self10);
      ret_val72 = ret_val74;
      s6 = ret_val72;
      ret_val71 = STR_ap1077157958(plus_self39, s6, TRUE);
      plus_self40 = ret_val71;
      plus_sarg27 = ((STR) &name36);
      ret_val75 = STR_ap2004550586(plus_self40, plus_sarg27);
      plus_self41 = ret_val75;
      plus_sarg28 = entry1;
      ret_val76 = STR_ap2004550586(plus_self41, plus_sarg28);
      plus_self42 = ret_val76;
      plus_arg7 = ';';
      str_self11 = plus_arg7;
      create_self8 = ((STR) NULL);
      create_c3 = str_self11;
      L83 = 1;
      L85=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L83)*sizeof(CHAR);
      L84=ZALLOC_LEAF_BIG(L85);
      if (L84==NULL) FATAL("Unable to allocate more memory");
      memset(L84,0,L85);
      ((OB)L84)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L84)->header.destroyed=0;
#endif

      L82 = ((STR) L84);
      L82->asize = L83;
      r5 = L82;
      SARR((STR)r5,0,create_c3); 
      ;
      ret_val79 = r5;
      ret_val78 = ret_val79;
      s7 = ret_val78;
      ret_val77 = STR_ap1077157958(plus_self42, s7, TRUE);
      ndefer_s6 = ret_val77;
      CGEN_d695831906(ndefer_self6);
      SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
     }
    }
   }
   else {
    L87 = ATTR(ATTR(arg,fun),args);
    L881_=INTMINUS(idx,1); 
    L89 = L881_;
    L901_=(ARG)ARR((ARRAYARG)L87,L89); 
    a11 = L901_;
    L91 = ATTR(a11,mode1);
    L92 = (*dMODE_814247358[TAG(L91)])(L91, ((dMODE) MODES_out_mode));
    L931_=!(L92); 
    if (L931_) {
     L941_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L95=L941_;
     ex1 = CGEN_e1639504572(self, ATTR(L95,expr));
     L96 = ATTR(a11,mode1);
     if ((*dMODE_814247358[TAG(L96)])(L96, ((dMODE) MODES_inout_mode))) {
      L1 = FLISTS1425610062(L1, ex1);
     }
     cast_self1 = self;
     cast_dest_tp1 = ATTR(a11,tp);
     L971_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L98=L971_;
     L99 = ATTR(L98,expr);
     cast_src_tp1 = (*dAM_EXPR_tprdTP[TAG(L99)])(L99);
     cast_expr1 = ex1;
     cast_only_boxing1 = FALSE;
     ret_val80 = CGEN_d1915593494(cast_self1, cast_dest_tp1, cast_src_tp1, cast_expr1, cast_only_boxing1, ((dMODE) NULL));
     entry1 = ret_val80;
     if (ATTR(arg,is_remote)) {
      L100 = ATTR(a11,tp);
      if ((*dTP_is1334578382[TAG(L100)])(L100)) {
       ndefer_self7 = self;
       plus_self43 = res;
       plus_sarg29 = ((STR) &bound_arg);
       ret_val81 = STR_ap2004550586(plus_self43, plus_sarg29);
       plus_self44 = ret_val81;
       plus_arg8 = i;
       str_self12 = plus_arg8;
       clear_self4 = buf5;
       L101 = (clear_self4==((FSTR) NULL));
       L1021_=!(L101); 
       if (L1021_) {
        SATTR(clear_self4,loc,0);
       }
       str_in_self4 = str_self12;
       str_in_s4 = buf5;
       ret_val84 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
       buf5 = ret_val84;
       str_self13 = buf5;
       ret_val85 = STR_fr1097270334(((STR) NULL), str_self13);
       ret_val83 = ret_val85;
       s8 = ret_val83;
       ret_val82 = STR_ap1077157958(plus_self44, s8, TRUE);
       plus_self45 = ret_val82;
       plus_sarg30 = ((STR) &name36);
       ret_val86 = STR_ap2004550586(plus_self45, plus_sarg30);
       plus_self46 = ret_val86;
       plus_sarg31 = entry1;
       ret_val87 = STR_ap2004550586(plus_self46, plus_sarg31);
       plus_self47 = ret_val87;
       plus_arg9 = ';';
       str_self14 = plus_arg9;
       create_self9 = ((STR) NULL);
       create_c4 = str_self14;
       L104 = 1;
       L106=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L104)*sizeof(CHAR);
       L105=ZALLOC_LEAF_BIG(L106);
       if (L105==NULL) FATAL("Unable to allocate more memory");
       memset(L105,0,L106);
       ((OB)L105)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L105)->header.destroyed=0;
#endif

       L103 = ((STR) L105);
       L103->asize = L104;
       r6 = L103;
       SARR((STR)r6,0,create_c4); 
       ;
       ret_val90 = r6;
       ret_val89 = ret_val90;
       s9 = ret_val89;
       ret_val88 = STR_ap1077157958(plus_self47, s9, TRUE);
       ndefer_s7 = ret_val88;
       CGEN_d695831906(ndefer_self7);
       SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
       L108 = ATTR(a11,tp);
       L109 = (*dTP_is418055720[TAG(L108)])(L108);
       L1101_=!(L109); 
       if (L1101_) {
        ndefer_self8 = self;
        plus_self48 = ((STR) &SENDOB1);
        tag_for_self2 = self;
        tag_for_tp2 = ATTR(a11,tp);
        SATTR(tag_for_self2,needs_tag,FSETdT1016814448(ATTR(tag_for_self2,needs_tag), tag_for_tp2));
        mang_self6 = tag_for_self2;
        mang_ob6 = ((OB) tag_for_tp2);
        ret_val92 = MANGLE119219986(ATTR(mang_self6,mangler), mang_ob6, ((OB) NULL));
        res4 = STR_ap2004550586(ret_val92, ((STR) &S_tag));
        forbid_self4 = tag_for_self2;
        forbid_s4 = res4;
        forbid_self5 = ATTR(forbid_self4,mangler);
        forbid_s5 = forbid_s4;
        SATTR(forbid_self5,forbidden,FSETST1404644833(ATTR(forbid_self5,forbidden), forbid_s5));
        ret_val91 = res4;
        plus_sarg32 = ret_val91;
        ret_val93 = STR_ap2004550586(plus_self48, plus_sarg32);
        plus_self49 = ret_val93;
        plus_sarg33 = ((STR) &name100);
        ret_val94 = STR_ap2004550586(plus_self49, plus_sarg33);
        plus_self50 = ret_val94;
        plus_sarg34 = res;
        ret_val95 = STR_ap2004550586(plus_self50, plus_sarg34);
        plus_self51 = ret_val95;
        plus_sarg35 = ((STR) &bound_arg);
        ret_val96 = STR_ap2004550586(plus_self51, plus_sarg35);
        plus_self52 = ret_val96;
        plus_arg10 = i;
        str_self15 = plus_arg10;
        clear_self5 = buf6;
        L111 = (clear_self5==((FSTR) NULL));
        L1121_=!(L111); 
        if (L1121_) {
         SATTR(clear_self5,loc,0);
        }
        str_in_self5 = str_self15;
        str_in_s5 = buf6;
        ret_val99 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
        buf6 = ret_val99;
        str_self16 = buf6;
        ret_val100 = STR_fr1097270334(((STR) NULL), str_self16);
        ret_val98 = ret_val100;
        s10 = ret_val98;
        ret_val97 = STR_ap1077157958(plus_self52, s10, TRUE);
        plus_self53 = ret_val97;
        plus_sarg36 = ((STR) &name3);
        ret_val101 = STR_ap2004550586(plus_self53, plus_sarg36);
        plus_self54 = ret_val101;
        plus_sarg37 = ATTR(arg,clst);
        ret_val102 = STR_ap2004550586(plus_self54, plus_sarg37);
        plus_self55 = ret_val102;
        plus_sarg38 = ((STR) &name76);
        ret_val103 = STR_ap2004550586(plus_self55, plus_sarg38);
        ndefer_s8 = ret_val103;
        CGEN_d695831906(ndefer_self8);
        SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
       }
      }
      else {
       ndefer_self9 = self;
       plus_self56 = res;
       plus_sarg39 = ((STR) &bound_arg);
       ret_val104 = STR_ap2004550586(plus_self56, plus_sarg39);
       plus_self57 = ret_val104;
       plus_arg11 = i;
       str_self17 = plus_arg11;
       clear_self6 = buf7;
       L113 = (clear_self6==((FSTR) NULL));
       L1141_=!(L113); 
       if (L1141_) {
        SATTR(clear_self6,loc,0);
       }
       str_in_self6 = str_self17;
       str_in_s6 = buf7;
       ret_val107 = INT_st367594495(str_in_self6, str_in_s6, 0, 10, ' ');
       buf7 = ret_val107;
       str_self18 = buf7;
       ret_val108 = STR_fr1097270334(((STR) NULL), str_self18);
       ret_val106 = ret_val108;
       s11 = ret_val106;
       ret_val105 = STR_ap1077157958(plus_self57, s11, TRUE);
       plus_self58 = ret_val105;
       plus_sarg40 = ((STR) &SENDFOB2);
       ret_val109 = STR_ap2004550586(plus_self58, plus_sarg40);
       plus_self59 = ret_val109;
       plus_sarg41 = entry1;
       ret_val110 = STR_ap2004550586(plus_self59, plus_sarg41);
       plus_self60 = ret_val110;
       plus_sarg42 = ((STR) &name3);
       ret_val1111 = STR_ap2004550586(plus_self60, plus_sarg42);
       plus_self61 = ret_val1111;
       plus_sarg43 = ATTR(arg,clst);
       ret_val112 = STR_ap2004550586(plus_self61, plus_sarg43);
       plus_self62 = ret_val112;
       plus_sarg44 = ((STR) &name76);
       ret_val113 = STR_ap2004550586(plus_self62, plus_sarg44);
       ndefer_s9 = ret_val113;
       CGEN_d695831906(ndefer_self9);
       SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
      }
     }
     else {
      ndefer_self10 = self;
      plus_self63 = res;
      plus_sarg45 = ((STR) &bound_arg);
      ret_val114 = STR_ap2004550586(plus_self63, plus_sarg45);
      plus_self64 = ret_val114;
      plus_arg12 = i;
      str_self19 = plus_arg12;
      clear_self7 = buf8;
      L115 = (clear_self7==((FSTR) NULL));
      L1161_=!(L115); 
      if (L1161_) {
       SATTR(clear_self7,loc,0);
      }
      str_in_self7 = str_self19;
      str_in_s7 = buf8;
      ret_val117 = INT_st367594495(str_in_self7, str_in_s7, 0, 10, ' ');
      buf8 = ret_val117;
      str_self20 = buf8;
      ret_val118 = STR_fr1097270334(((STR) NULL), str_self20);
      ret_val116 = ret_val118;
      s12 = ret_val116;
      ret_val115 = STR_ap1077157958(plus_self64, s12, TRUE);
      plus_self65 = ret_val115;
      plus_sarg46 = ((STR) &name36);
      ret_val119 = STR_ap2004550586(plus_self65, plus_sarg46);
      plus_self66 = ret_val119;
      plus_sarg47 = entry1;
      ret_val120 = STR_ap2004550586(plus_self66, plus_sarg47);
      plus_self67 = ret_val120;
      plus_arg13 = ';';
      str_self21 = plus_arg13;
      create_self10 = ((STR) NULL);
      create_c5 = str_self21;
      L118 = 1;
      L120=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L118)*sizeof(CHAR);
      L119=ZALLOC_LEAF_BIG(L120);
      if (L119==NULL) FATAL("Unable to allocate more memory");
      memset(L119,0,L120);
      ((OB)L119)->header.tag=STR_tag;
#ifdef DESTROY_CHK

        ((OB)L119)->header.destroyed=0;
#endif

      L117 = ((STR) L119);
      L117->asize = L118;
      r7 = L117;
      SARR((STR)r7,0,create_c5); 
      ;
      ret_val123 = r7;
      ret_val122 = ret_val123;
      s13 = ret_val122;
      ret_val121 = STR_ap1077157958(plus_self67, s13, TRUE);
      ndefer_s10 = ret_val121;
      CGEN_d695831906(ndefer_self10);
      SATTR(ndefer_self10,routine_code,FSTR_p1752847026(ATTR(ndefer_self10,routine_code), ndefer_s10));
     }
    }
   }
   L1221_=INTPLUS(L51,1); 
   L51 = L1221_;
  }
 }
 after_loop2: ;
 make_sure_emitte = self;
 make_sure_emitte1 = ATTR(arg,fun);
 ard = FMAPSI2016001247(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1);
 L123 = (ard==((AM_ROUT_DEF) NULL));
 L1241_=!(L123); 
 if (L1241_) {
  SATTR(make_sure_emitte,leftovers,FSETAM2044524049(ATTR(make_sure_emitte,leftovers), ard));
  SATTR(make_sure_emitte,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte,not_emitted), make_sure_emitte1));
 }
 if (casting) {
  plus_self68 = ((STR) &name24);
  mang_self7 = self;
  tp_self1 = arg;
  ret_val124 = ATTR(tp_self1,tp_at);
  mang_ob7 = ((OB) ret_val124);
  ret_val125 = MANGLE119219986(ATTR(mang_self7,mangler), mang_ob7, ((OB) NULL));
  plus_sarg48 = ret_val125;
  ret_val126 = STR_ap2004550586(plus_self68, plus_sarg48);
  plus_self69 = ret_val126;
  plus_sarg49 = ((STR) &name23);
  ret_val127 = STR_ap2004550586(plus_self69, plus_sarg49);
  plus_self70 = ret_val127;
  plus_sarg50 = res;
  ret_val128 = STR_ap2004550586(plus_self70, plus_sarg50);
  ret_val = ret_val128;
  *argstr = L1;
  return ret_val;
 }
 else {
  ret_val = res;
  *argstr = L1;
  return ret_val;
 }
 *argstr = L1;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e712006663(CGEN self, AM_AT_EXPR ate, AM_ROU1916046290 ca, BOOL fork_handler) {
 STR ret_val;
 STR decl = ((STR) NULL);
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT ind1 = INT_zero;
 AM_ROU1916046290 call11;
 AM_ROU1916046290 L31;
 INT i = INT_zero;
 AM_LOCAL_EXPR arg_expr;
 ARRAYSTR arg_list;
 AM_ROU1916046290 L41;
 INT i1 = INT_zero;
 ARRAYSTR L51;
 FSTR save_rout_code;
 CODE_FILE save_file;
 STR call_str;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 CODE_FILE plus_self1;
 dSTR plus_s;
 CODE_FILE plus_self2;
 dSTR plus_s1;
 CODE_FILE plus_self3;
 dSTR plus_s2;
 CODE_FILE ret_val6;
 CODE_FILE plus_self4;
 dSTR plus_s3;
 CODE_FILE plus_self5;
 dSTR plus_s4;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val7;
 CODE_FILE plus_self6;
 dSTR plus_s5;
 CODE_FILE ret_val8;
 CODE_FILE plus_self7;
 dSTR plus_s6;
 CODE_FILE plus_self8;
 dSTR plus_s7;
 CODE_FILE plus_self9;
 dSTR plus_s8;
 CODE_FILE plus_self10;
 dSTR plus_s9;
 CODE_FILE plus_self11;
 dSTR plus_s10;
 CODE_FILE plus_self12;
 dSTR plus_s11;
 CODE_FILE plus_self13;
 dSTR plus_s12;
 CODE_FILE plus_self14;
 dSTR plus_s13;
 CODE_FILE plus_self15;
 dSTR plus_s14;
 CODE_FILE plus_self16;
 dSTR plus_s15;
 AM_ROU1916046290 create_self;
 INT create_n = INT_zero;
 AM_ROU1916046290 ret_val9;
 INT L61 = INT_zero;
 AM_LOCAL_EXPR create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_LOCAL_EXPR ret_val10;
 AM_LOCAL_EXPR r;
 INT str_self2 = INT_zero;
 STR ret_val11;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val12;
 FSTR str_self3;
 STR ret_val13;
 STR plus_self17;
 STR plus_sarg1;
 STR ret_val14;
 AM_CALL_ARG create_self2;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val15;
 AM_CALL_ARG res;
 ARRAYSTR create_self3;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val16;
 INT L71 = INT_zero;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val17;
 INT str_self4 = INT_zero;
 STR ret_val18;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val19;
 FSTR str_self5;
 STR ret_val20;
 FSTR create_self4;
 FSTR ret_val21;
 CODE_FILE plus_self18;
 dSTR plus_s16;
 CODE_FILE plus_self19;
 dSTR plus_s17;
 CODE_FILE plus_self20;
 dSTR plus_s18;
 CODE_FILE plus_self21;
 dSTR plus_s19;
 CODE_FILE ret_val22;
 CODE_FILE plus_self22;
 dSTR plus_s20;
 CODE_FILE ret_val23;
 CODE_FILE plus_self23;
 dSTR plus_s21;
 CODE_FILE plus_self24;
 dSTR plus_s22;
 CODE_FILE plus_self25;
 dSTR plus_s23;
 CODE_FILE plus_self26;
 dSTR plus_s24;
 CODE_FILE plus_self27;
 dSTR plus_s25;
 CODE_FILE plus_self28;
 dSTR plus_s26;
 CODE_FILE plus_self29;
 dSTR plus_s27;
 CODE_FILE plus_self30;
 dSTR plus_s28;
 extern STR void6;
 extern STR BR_clu1835965243;
 INT L9;
 INT L101_;
 INT L12;
 INT L131_;
 INT L81_,L81_m;
 extern STR BR_word_t1;
 extern STR arg_;
 BOOL L14;
 BOOL L151_;
 extern STR name26;
 extern STR Simple1040713615;
 FSTR L16;
 dSTR L17;
 extern STR Simple287998794;
 FSTR L18;
 dSTR L19;
 BOOL L20;
 BOOL L221_;
 FSTR L23;
 dSTR L24;
 extern STR name98;
 FSTR L25;
 dSTR L26;
 extern STR name10;
 FSTR L27;
 dSTR L28;
 BOOL L29;
 BOOL L301_;
 BOOL L32;
 BOOL L331_;
 FSTR L34;
 dSTR L35;
 extern STR S__res;
 FSTR L36;
 dSTR L37;
 extern STR BR_word_t__res0;
 FSTR L38;
 dSTR L39;
 extern STR struct88687480;
 FSTR L40;
 dSTR L42;
 extern STR struct1342450411;
 FSTR L43;
 dSTR L44;
 BOOL L451_;
 extern STR LOCAL_2068279904;
 FSTR L46;
 dSTR L47;
 extern STR struct340882784;
 FSTR L48;
 dSTR L49;
 extern STR BR_SET165350689;
 FSTR L50;
 dSTR L52;
 extern STR INIT_T802300136;
 FSTR L53;
 dSTR L54;
 extern STR PSATHE898285019;
 FSTR L55;
 dSTR L56;
 extern STR THREAD163596686;
 FSTR L57;
 dSTR L58;
 INT L591_;
 AM_ROU1916046290 L60;
 INT L62;
 OB L63;
 INT L64;
 INT L651_br;
 INT rL651_;
 INT L66;
 AM_LOCAL_EXPR L67;
 OB L68;
 AM_CALL_ARG L691_;
 AM_CALL_ARG L70;
 dAM_EXPR L72;
 extern STR arg_;
 BOOL L73;
 BOOL L741_;
 AM_CALL_ARG L75;
 OB L76;
 AM_CALL_ARG L77;
 INT L791_;
 INT L801_;
 ARRAYSTR L82;
 INT L83;
 OB L84;
 INT L85;
 INT L861_br;
 INT rL861_;
 INT L871_br;
 INT L88;
 extern STR name24;
 AM_CALL_ARG L891_;
 AM_CALL_ARG L90;
 dAM_EXPR L91;
 extern STR name26;
 extern STR arg_;
 BOOL L92;
 BOOL L931_;
 STR L94;
 INT L951_;
 FSTR L96;
 INT L97;
 OB L98;
 INT L99;
 extern STR name9;
 extern STR do4;
 FSTR L100;
 dSTR L102;
 FSTR L103;
 dSTR L104;
 BOOL L105;
 BOOL L1061_;
 extern STR S__res1;
 FSTR L107;
 dSTR L108;
 extern STR name60;
 BOOL L109;
 BOOL L1101_;
 FSTR L111;
 dSTR L112;
 BOOL L113;
 BOOL L1141_;
 FSTR L115;
 dSTR L116;
 extern STR name13;
 FSTR L117;
 dSTR L118;
 extern STR while01;
 FSTR L119;
 dSTR L120;
 extern STR THREAD1879362277;
 FSTR L121;
 dSTR L122;
 BOOL L1231_;
 extern STR BR_SET1034520431;
 FSTR L124;
 dSTR L125;
 extern STR AT_ATT1332028378;
 FSTR L126;
 dSTR L127;
 extern STR AT_ATT1665390693;
 FSTR L128;
 dSTR L129;
 extern STR srcat_1636109666;
 FSTR L130;
 dSTR L132;
 extern STR name32;
 FSTR L133;
 dSTR L134;
 mang_self = self;
 mang_ob = ((OB) ate);
 ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
 decl = STR_ap1693864410(((STR) &void6), ret_val1, ((STR) &BR_clu1835965243));
 {
  BOOL f_L81_ = TRUE;
  while (1) {
   if (f_L81_) {
    f_L81_ = FALSE;
    L11 = 0;
    L101_=(ca)==NULL?0:ASIZE((AM_ROU1916046290)ca); 
    L12 = L101_;
    L131_=INTMINUS(L12,1); 
    L21 = L131_;
    L81_=L11-1;L81_m=L21; 
   }
   if(L81_++>=L81_m)  goto after_loop; 
   ind1 = L81_;
   str_self = ind1;
   clear_self = buf1;
   L14 = (clear_self==((FSTR) NULL));
   L151_=!(L14); 
   if (L151_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val3;
   str_self1 = buf1;
   ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val2 = ret_val4;
   decl = STR_ap1417971546(decl, ((STR) &BR_word_t1), ((STR) &arg_), ret_val2);
  }
 }
 after_loop: ;
 plus_self = decl;
 plus_sarg = ((STR) &name26);
 ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
 decl = ret_val5;
 if (fork_handler) {
  plus_self1 = ATTR(self,unbox_c);
  plus_s = ((dSTR) ((STR) &Simple1040713615));
  L16 = ATTR(plus_self1,ntext);
  L17 = plus_s;
  SATTR(plus_self1,ntext,FSTR_p1752847026(L16, (*dSTR_strrSTR[TAG(L17)])(L17)));
 }
 else {
  plus_self2 = ATTR(self,unbox_c);
  plus_s1 = ((dSTR) ((STR) &Simple287998794));
  L18 = ATTR(plus_self2,ntext);
  L19 = plus_s1;
  SATTR(plus_self2,ntext,FSTR_p1752847026(L18, (*dSTR_strrSTR[TAG(L19)])(L19)));
 }
 plus_self3 = ATTR(self,unbox_c);
 plus_s2 = ((dSTR) decl);
 L20 = (plus_s2==((dSTR) NULL));
 L221_=!(L20); 
 if (L221_) {
  L23 = ATTR(plus_self3,ntext);
  L24 = plus_s2;
  SATTR(plus_self3,ntext,FSTR_p1752847026(L23, (*dSTR_strrSTR[TAG(L24)])(L24)));
 }
 ret_val6 = plus_self3;
 plus_self4 = ret_val6;
 plus_s3 = ((dSTR) ((STR) &name98));
 L25 = ATTR(plus_self4,ntext);
 L26 = plus_s3;
 SATTR(plus_self4,ntext,FSTR_p1752847026(L25, (*dSTR_strrSTR[TAG(L26)])(L26)));
 plus_self5 = ATTR(self,unbox_c);
 plus_s4 = ((dSTR) ((STR) &name10));
 L27 = ATTR(plus_self5,ntext);
 L28 = plus_s4;
 SATTR(plus_self5,ntext,FSTR_p1752847026(L27, (*dSTR_strrSTR[TAG(L28)])(L28)));
 L29 = (ATTR(ATTR(ca,fun),ret)==((dTP) NULL));
 L301_=!(L29); 
 if (L301_) {
  plus_self6 = ATTR(self,unbox_c);
  mang_self1 = self;
  mang_ob1 = ((OB) ATTR(ATTR(ca,fun),ret));
  ret_val7 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
  plus_s5 = ((dSTR) ret_val7);
  L32 = (plus_s5==((dSTR) NULL));
  L331_=!(L32); 
  if (L331_) {
   L34 = ATTR(plus_self6,ntext);
   L35 = plus_s5;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L34, (*dSTR_strrSTR[TAG(L35)])(L35)));
  }
  ret_val8 = plus_self6;
  plus_self7 = ret_val8;
  plus_s6 = ((dSTR) ((STR) &S__res));
  L36 = ATTR(plus_self7,ntext);
  L37 = plus_s6;
  SATTR(plus_self7,ntext,FSTR_p1752847026(L36, (*dSTR_strrSTR[TAG(L37)])(L37)));
 }
 else {
  plus_self8 = ATTR(self,unbox_c);
  plus_s7 = ((dSTR) ((STR) &BR_word_t__res0));
  L38 = ATTR(plus_self8,ntext);
  L39 = plus_s7;
  SATTR(plus_self8,ntext,FSTR_p1752847026(L38, (*dSTR_strrSTR[TAG(L39)])(L39)));
 }
 if (ATTR(self,func_tables)) {
  plus_self9 = ATTR(self,unbox_c);
  plus_s8 = ((dSTR) ((STR) &struct88687480));
  L40 = ATTR(plus_self9,ntext);
  L42 = plus_s8;
  SATTR(plus_self9,ntext,FSTR_p1752847026(L40, (*dSTR_strrSTR[TAG(L42)])(L42)));
  plus_self10 = ATTR(self,unbox_c);
  plus_s9 = ((dSTR) ((STR) &struct1342450411));
  L43 = ATTR(plus_self10,ntext);
  L44 = plus_s9;
  SATTR(plus_self10,ntext,FSTR_p1752847026(L43, (*dSTR_strrSTR[TAG(L44)])(L44)));
 }
 L451_=!(fork_handler); 
 if (L451_) {
  plus_self11 = ATTR(self,unbox_c);
  plus_s10 = ((dSTR) ((STR) &LOCAL_2068279904));
  L46 = ATTR(plus_self11,ntext);
  L47 = plus_s10;
  SATTR(plus_self11,ntext,FSTR_p1752847026(L46, (*dSTR_strrSTR[TAG(L47)])(L47)));
  plus_self12 = ATTR(self,unbox_c);
  plus_s11 = ((dSTR) ((STR) &struct340882784));
  L48 = ATTR(plus_self12,ntext);
  L49 = plus_s11;
  SATTR(plus_self12,ntext,FSTR_p1752847026(L48, (*dSTR_strrSTR[TAG(L49)])(L49)));
  plus_self13 = ATTR(self,unbox_c);
  plus_s12 = ((dSTR) ((STR) &BR_SET165350689));
  L50 = ATTR(plus_self13,ntext);
  L52 = plus_s12;
  SATTR(plus_self13,ntext,FSTR_p1752847026(L50, (*dSTR_strrSTR[TAG(L52)])(L52)));
 }
 plus_self14 = ATTR(self,unbox_c);
 plus_s13 = ((dSTR) ((STR) &INIT_T802300136));
 L53 = ATTR(plus_self14,ntext);
 L54 = plus_s13;
 SATTR(plus_self14,ntext,FSTR_p1752847026(L53, (*dSTR_strrSTR[TAG(L54)])(L54)));
 plus_self15 = ATTR(self,unbox_c);
 plus_s14 = ((dSTR) ((STR) &PSATHE898285019));
 L55 = ATTR(plus_self15,ntext);
 L56 = plus_s14;
 SATTR(plus_self15,ntext,FSTR_p1752847026(L55, (*dSTR_strrSTR[TAG(L56)])(L56)));
 plus_self16 = ATTR(self,unbox_c);
 plus_s15 = ((dSTR) ((STR) &THREAD163596686));
 L57 = ATTR(plus_self16,ntext);
 L58 = plus_s15;
 SATTR(plus_self16,ntext,FSTR_p1752847026(L57, (*dSTR_strrSTR[TAG(L58)])(L58)));
 create_self = ((AM_ROU1916046290) NULL);
 L591_=(ca)==NULL?0:ASIZE((AM_ROU1916046290)ca); 
 create_n = L591_;
 L62 = create_n;
 L64=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L62)*sizeof(AM_CALL_ARG);
 L63=ZALLOC_BIG(L64);
 if (L63==NULL) FATAL("Unable to allocate more memory");
 ((OB)L63)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

   ((OB)L63)->header.destroyed=0;
#endif

 L60 = ((AM_ROU1916046290) L63);
 L60->asize = L62;
 ret_val9 = L60;
 call11 = ret_val9;
 SATTR(call11,fun,ATTR(ca,fun));
 {
  BOOL f_L651_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L31 = ca;
  L651_br=L31==NULL?0:ASIZE((AM_ROU1916046290)L31); 
  while (1) {
   if(L61>=L651_br)  goto after_loop1; 
   rL651_=L61; 
   i = rL651_;
   create_self1 = ((AM_LOCAL_EXPR) NULL);
   create_source = SFILE_ID_zero;
   L68=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
   if (L68==NULL) FATAL("Unable to allocate more memory");
   ((OB)L68)->header.tag=AM_LOCAL_EXPR_tag;
   L67 = ((AM_LOCAL_EXPR) L68);
   r = L67;
   SATTR(r,source1,create_source);
   ret_val10 = r;
   arg_expr = ret_val10;
   L691_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ca,i); 
   L70=L691_;
   L72 = ATTR(L70,expr);
   SATTR(arg_expr,tp_at,(*dAM_EXPR_tprdTP[TAG(L72)])(L72));
   plus_self17 = ((STR) &arg_);
   str_self2 = i;
   clear_self1 = buf2;
   L73 = (clear_self1==((FSTR) NULL));
   L741_=!(L73); 
   if (L741_) {
    SATTR(clear_self1,loc,0);
   }
   str_in_self1 = str_self2;
   str_in_s1 = buf2;
   ret_val12 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
   buf2 = ret_val12;
   str_self3 = buf2;
   ret_val13 = STR_fr1097270334(((STR) NULL), str_self3);
   ret_val11 = ret_val13;
   plus_sarg1 = ret_val11;
   ret_val14 = STR_ap2004550586(plus_self17, plus_sarg1);
   SATTR(arg_expr,name1,IDENT_1150413730(IDENT_zero, ret_val14));
   create_self2 = ((AM_CALL_ARG) NULL);
   create_e = ((dAM_EXPR) arg_expr);
   L76=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
   if (L76==NULL) FATAL("Unable to allocate more memory");
   ((OB)L76)->header.tag=AM_CALL_ARG_tag;
   L75 = ((AM_CALL_ARG) L76);
   res = L75;
   SATTR(res,expr,create_e);
   SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
   ret_val15 = res;
   L77 = ret_val15;
   SARR((AM_ROU1916046290)call11,i,(AM_CALL_ARG)L77); 
   ;
   L791_=INTPLUS(L61,1); 
   L61 = L791_;
  }
 }
 after_loop1: ;
 create_self3 = ((ARRAYSTR) NULL);
 L801_=(ca)==NULL?0:ASIZE((AM_ROU1916046290)ca); 
 create_n1 = L801_;
 L83 = create_n1;
 L85=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L83)*sizeof(STR);
 L84=ZALLOC_BIG(L85);
 if (L84==NULL) FATAL("Unable to allocate more memory");
 ((OB)L84)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L84)->header.destroyed=0;
#endif

 L82 = ((ARRAYSTR) L84);
 L82->asize = L83;
 ret_val16 = L82;
 arg_list = ret_val16;
 {
  BOOL f_L861_ = TRUE;
  BOOL f_L871_ = TRUE;
  /* loop index variable */
  L71 = 0;
  L41 = ca;
  L51 = arg_list;
  L861_br=L41==NULL?0:ASIZE((AM_ROU1916046290)L41); 
  L871_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
  while (1) {
   if(L71>=L861_br)  goto after_loop2; 
   rL861_=L71; 
   i1 = rL861_;
   if(L71>=L871_br)  goto after_loop2; 
   mang_self2 = self;
   L891_=(AM_CALL_ARG)ARR((AM_ROU1916046290)ca,i1); 
   L90=L891_;
   L91 = ATTR(L90,expr);
   mang_ob2 = ((OB) (*dAM_EXPR_tprdTP[TAG(L91)])(L91));
   ret_val17 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
   str_self4 = i1;
   clear_self2 = buf3;
   L92 = (clear_self2==((FSTR) NULL));
   L931_=!(L92); 
   if (L931_) {
    SATTR(clear_self2,loc,0);
   }
   str_in_self2 = str_self4;
   str_in_s2 = buf3;
   ret_val19 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
   buf3 = ret_val19;
   str_self5 = buf3;
   ret_val20 = STR_fr1097270334(((STR) NULL), str_self5);
   ret_val18 = ret_val20;
   L94 = STR_ap44172742(((STR) &name24), ret_val17, ((STR) &name26), ((STR) &arg_), ret_val18);
   SARR((ARRAYSTR)L51,L71,(STR)L94); 
   ;
   L951_=INTPLUS(L71,1); 
   L71 = L951_;
  }
 }
 after_loop2: ;
 save_rout_code = ATTR(self,routine_code);
 save_file = ATTR(self,code_c);
 create_self4 = ((FSTR) NULL);
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
 ret_val21 = L96;
 SATTR(self,routine_code,FSTR_p1752847026(ret_val21, ((STR) &name9)));
 SATTR(self,code_c,ATTR(self,unbox_c));
 plus_self18 = ATTR(self,unbox_c);
 plus_s16 = ((dSTR) ((STR) &do4));
 L100 = ATTR(plus_self18,ntext);
 L102 = plus_s16;
 SATTR(plus_self18,ntext,FSTR_p1752847026(L100, (*dSTR_strrSTR[TAG(L102)])(L102)));
 call_str = CGEN_e1835933683(self, ATTR(call11,fun), ((dAM_EXPR) call11), arg_list);
 plus_self19 = ATTR(self,unbox_c);
 plus_s17 = ((dSTR) ATTR(self,routine_code));
 L103 = ATTR(plus_self19,ntext);
 L104 = plus_s17;
 SATTR(plus_self19,ntext,FSTR_p1752847026(L103, (*dSTR_strrSTR[TAG(L104)])(L104)));
 SATTR(self,routine_code,save_rout_code);
 SATTR(self,code_c,save_file);
 L105 = (ATTR(ATTR(ca,fun),ret)==((dTP) NULL));
 L1061_=!(L105); 
 if (L1061_) {
  plus_self20 = ATTR(self,unbox_c);
  plus_s18 = ((dSTR) ((STR) &S__res1));
  L107 = ATTR(plus_self20,ntext);
  L108 = plus_s18;
  SATTR(plus_self20,ntext,FSTR_p1752847026(L107, (*dSTR_strrSTR[TAG(L108)])(L108)));
 }
 plus_self21 = ATTR(self,unbox_c);
 plus_s19 = ((dSTR) ((STR) &name60));
 L109 = (plus_s19==((dSTR) NULL));
 L1101_=!(L109); 
 if (L1101_) {
  L111 = ATTR(plus_self21,ntext);
  L112 = plus_s19;
  SATTR(plus_self21,ntext,FSTR_p1752847026(L111, (*dSTR_strrSTR[TAG(L112)])(L112)));
 }
 ret_val22 = plus_self21;
 plus_self22 = ret_val22;
 plus_s20 = ((dSTR) call_str);
 L113 = (plus_s20==((dSTR) NULL));
 L1141_=!(L113); 
 if (L1141_) {
  L115 = ATTR(plus_self22,ntext);
  L116 = plus_s20;
  SATTR(plus_self22,ntext,FSTR_p1752847026(L115, (*dSTR_strrSTR[TAG(L116)])(L116)));
 }
 ret_val23 = plus_self22;
 plus_self23 = ret_val23;
 plus_s21 = ((dSTR) ((STR) &name13));
 L117 = ATTR(plus_self23,ntext);
 L118 = plus_s21;
 SATTR(plus_self23,ntext,FSTR_p1752847026(L117, (*dSTR_strrSTR[TAG(L118)])(L118)));
 plus_self24 = ATTR(self,unbox_c);
 plus_s22 = ((dSTR) ((STR) &while01));
 L119 = ATTR(plus_self24,ntext);
 L120 = plus_s22;
 SATTR(plus_self24,ntext,FSTR_p1752847026(L119, (*dSTR_strrSTR[TAG(L120)])(L120)));
 plus_self25 = ATTR(self,unbox_c);
 plus_s23 = ((dSTR) ((STR) &THREAD1879362277));
 L121 = ATTR(plus_self25,ntext);
 L122 = plus_s23;
 SATTR(plus_self25,ntext,FSTR_p1752847026(L121, (*dSTR_strrSTR[TAG(L122)])(L122)));
 L1231_=!(fork_handler); 
 if (L1231_) {
  plus_self26 = ATTR(self,unbox_c);
  plus_s24 = ((dSTR) ((STR) &BR_SET1034520431));
  L124 = ATTR(plus_self26,ntext);
  L125 = plus_s24;
  SATTR(plus_self26,ntext,FSTR_p1752847026(L124, (*dSTR_strrSTR[TAG(L125)])(L125)));
 }
 if (fork_handler) {
  plus_self27 = ATTR(self,unbox_c);
  plus_s25 = ((dSTR) ((STR) &AT_ATT1332028378));
  L126 = ATTR(plus_self27,ntext);
  L127 = plus_s25;
  SATTR(plus_self27,ntext,FSTR_p1752847026(L126, (*dSTR_strrSTR[TAG(L127)])(L127)));
 }
 else {
  plus_self28 = ATTR(self,unbox_c);
  plus_s26 = ((dSTR) ((STR) &AT_ATT1665390693));
  L128 = ATTR(plus_self28,ntext);
  L129 = plus_s26;
  SATTR(plus_self28,ntext,FSTR_p1752847026(L128, (*dSTR_strrSTR[TAG(L129)])(L129)));
 }
 plus_self29 = ATTR(self,unbox_c);
 plus_s27 = ((dSTR) ((STR) &srcat_1636109666));
 L130 = ATTR(plus_self29,ntext);
 L132 = plus_s27;
 SATTR(plus_self29,ntext,FSTR_p1752847026(L130, (*dSTR_strrSTR[TAG(L132)])(L132)));
 plus_self30 = ATTR(self,unbox_c);
 plus_s28 = ((dSTR) ((STR) &name32));
 L133 = ATTR(plus_self30,ntext);
 L134 = plus_s28;
 SATTR(plus_self30,ntext,FSTR_p1752847026(L133, (*dSTR_strrSTR[TAG(L134)])(L134)));
 CODE_F1291349628(ATTR(self,unbox_c), ATTR(ca,fun));
 ret_val = decl;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_e969629179(CGEN self, AM_AT_EXPR arg) {
 STR ret_val;
 STR z;
 dTP tp;
 STR r = ((STR) NULL);
 STR loc;
 dAM_EXPR e;
 STR loc1;
 STR res = ((STR) NULL);
 AM_BND1124877163 bnd;
 AM_ROU1916046290 L11;
 AM_BND1124877163 L21;
 INT L31 = INT_zero;
 ARRAYINT L41;
 TP_ROUT tbnd;
 FLISTSTR al = ((FLISTSTR) NULL);
 STR rt;
 CGEN err_loc_self;
 dPROG_ERR err_loc_t;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 UNIX exit_self;
 INT exit_code = INT_zero;
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
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val6;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val7;
 CGEN ndefer_self2;
 STR ndefer_s2;
 STR plus_self7;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r1;
 CGEN ndefer_self3;
 STR ndefer_s3;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val11;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val12;
 CGEN ndefer_self4;
 STR ndefer_s4;
 CGEN err_loc_self1;
 dPROG_ERR err_loc_t1;
 STR plus_self10;
 STR plus_sarg9;
 STR ret_val13;
 STR plus_self11;
 STR plus_sarg10;
 STR ret_val14;
 CGEN ndefer_self5;
 STR ndefer_s5;
 CGEN message_self;
 STR message_s;
 CGEN warning_self;
 STR warning_s;
 AM_BND1124877163 create_self1;
 INT create_nargs = INT_zero;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_BND1124877163 ret_val15;
 AM_BND1124877163 r2;
 INT L51 = INT_zero;
 ARRAYINT create_self2;
 INT create_n = INT_zero;
 ARRAYINT ret_val16;
 INT L61 = INT_zero;
 STR plus_self12;
 STR plus_sarg11;
 STR ret_val17;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val18;
 STR plus_self14;
 STR plus_sarg13;
 STR ret_val19;
 STR plus_self15;
 STR plus_sarg14;
 STR ret_val20;
 STR plus_self16;
 STR plus_sarg15;
 STR ret_val21;
 CGEN ndefer_self6;
 STR ndefer_s6;
 STR plus_self17;
 STR plus_sarg16;
 STR ret_val22;
 STR plus_self18;
 STR plus_sarg17;
 STR ret_val23;
 STR plus_self19;
 STR plus_sarg18;
 STR ret_val24;
 STR plus_self20;
 STR plus_sarg19;
 STR ret_val25;
 CGEN ndefer_self7;
 STR ndefer_s7;
 AM_ROU1916046290 tp_self;
 dTP ret_val26;
 AM_ROU1916046290 tp_self1;
 dTP ret_val27;
 AM_ROU1916046290 tp_self2;
 dTP ret_val28;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val29;
 STR res1;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val30;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self21;
 STR plus_sarg20;
 STR ret_val31;
 STR plus_self22;
 STR plus_sarg21;
 STR ret_val32;
 STR plus_self23;
 STR plus_sarg22;
 STR ret_val33;
 STR plus_self24;
 STR plus_sarg23;
 STR ret_val34;
 STR plus_self25;
 STR plus_sarg24;
 STR ret_val35;
 STR plus_self26;
 STR plus_sarg25;
 STR ret_val36;
 CGEN ndefer_self8;
 STR ndefer_s8;
 STR plus_self27;
 STR plus_sarg26;
 STR ret_val37;
 STR plus_self28;
 STR plus_sarg27;
 STR ret_val38;
 STR plus_self29;
 STR plus_sarg28;
 STR ret_val39;
 STR plus_self30;
 STR plus_sarg29;
 STR ret_val40;
 STR plus_self31;
 STR plus_sarg30;
 STR ret_val41;
 STR plus_self32;
 STR plus_sarg31;
 STR ret_val42;
 STR plus_self33;
 STR plus_sarg32;
 STR ret_val43;
 STR plus_self34;
 STR plus_sarg33;
 STR ret_val44;
 STR plus_self35;
 STR plus_sarg34;
 STR ret_val45;
 STR plus_self36;
 STR plus_sarg35;
 STR ret_val46;
 STR plus_self37;
 STR plus_sarg36;
 STR ret_val47;
 CGEN warning_self1;
 STR warning_s1;
 CGEN ndefer_self9;
 STR ndefer_s9;
 extern STR Zonesa266747214;
 extern STR Intern28965746;
 extern STR SI_HERE1;
 extern STR SI_SET_HERE1;
 extern STR name76;
 dAM_EXPR L7;
 BOOL L8;
 BOOL L91_;
 extern STR name33;
 extern STR name16;
 STR L10;
 INT L12;
 OB L13;
 INT L14;
 extern STR SI_SET_HERE1;
 extern STR name76;
 BOOL L16;
 BOOL L171_;
 extern STR INTdummy_INT_;
 extern STR name102;
 BOOL L18;
 extern STR Simple318344970;
 extern STR Comple1197394349;
 INT L191_;
 AM_BND1124877163 L20;
 INT L22;
 OB L23;
 INT L24;
 INT L251_br;
 AM_CALL_ARG aL251_;
 INT L261_br;
 AM_CALL_ARG L27;
 AM_CALL_ARG L28;
 INT L291_;
 INT L301_;
 ARRAYINT L32;
 INT L33;
 OB L34;
 INT L35;
 INT L361_;
 INT L371_br;
 INT L38;
 INT L39;
 INT L401_;
 FLISTSTR* L42;
 extern STR localSENDFOB;
 extern STR name3;
 extern STR name76;
 dAM_EXPR L43;
 BOOL L44;
 BOOL L451_;
 dAM_EXPR L46;
 extern STR REMOTE_EXEC;
 extern STR name3;
 extern STR name76;
 dTP L47;
 dTP L48;
 BOOL L49;
 BOOL L501_;
 extern STR RECVOB1;
 extern STR S_tag;
 extern STR name100;
 extern STR ret_arg;
 extern STR name76;
 extern STR ret_arg1;
 extern STR RECVFOB2;
 extern STR ret_arg;
 extern STR name26;
 extern STR REMOTE_EXEC;
 extern STR name3;
 extern STR name76;
 extern STR cluste72287095;
 INT L521_;
 INT L53;
 STR L541_;
 extern STR ignoring;
 extern STR thefol1791862455;
 if (ATTR(ATTR(self,prog),zones)) {
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) arg);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  if (ATTR(ATTR(self,prog),distributed)) {
   barf_self = self;
   barf_msg = ((STR) &Zonesa266747214);
   barf_self1 = ATTR(barf_self,prog);
   barf_msg1 = barf_msg;
   barf_at_self = barf_self1;
   barf_at_msg = barf_msg1;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self = ((STR) &Intern28965746);
   plus_sarg = barf_at_msg;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   PROG_err_STR(barf_at_self, ret_val1);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
  }
  z = CGEN_d1425664957(self, ((dTP) TP_BUILTIN_zone));
  ndefer_self = self;
  plus_self1 = z;
  plus_sarg1 = ((STR) &SI_HERE1);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
  ndefer_s = ret_val2;
  CGEN_d695831906(ndefer_self);
  SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
  ndefer_self1 = self;
  plus_self2 = ((STR) &SI_SET_HERE1);
  plus_sarg2 = CGEN_e1639504572(self, ATTR(arg,at));
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val3;
  plus_sarg3 = ((STR) &name76);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg3);
  ndefer_s1 = ret_val4;
  CGEN_d695831906(ndefer_self1);
  SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
  L7 = ATTR(arg,e);
  tp = (*dAM_EXPR_tprdTP[TAG(L7)])(L7);
  L8 = (tp==((dTP) NULL));
  L91_=!(L8); 
  if (L91_) {
   r = CGEN_d1425664957(self, tp);
   ndefer_self2 = self;
   plus_self4 = r;
   plus_sarg4 = ((STR) &name33);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg4);
   plus_self5 = ret_val5;
   plus_sarg5 = CGEN_e1639504572(self, ATTR(arg,e));
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg5);
   plus_self6 = ret_val6;
   plus_sarg6 = ((STR) &name16);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg6);
   ndefer_s2 = ret_val7;
   CGEN_d695831906(ndefer_self2);
   SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
  }
  else {
   ndefer_self3 = self;
   plus_self7 = CGEN_e1639504572(self, ATTR(arg,e));
   plus_arg = ';';
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
   r1 = L10;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val10 = r1;
   ret_val9 = ret_val10;
   s = ret_val9;
   ret_val8 = STR_ap1077157958(plus_self7, s, TRUE);
   ndefer_s3 = ret_val8;
   CGEN_d695831906(ndefer_self3);
   SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
   r = ((STR) NULL);
  }
  ndefer_self4 = self;
  plus_self8 = ((STR) &SI_SET_HERE1);
  plus_sarg7 = z;
  ret_val11 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val11;
  plus_sarg8 = ((STR) &name76);
  ret_val12 = STR_ap2004550586(plus_self9, plus_sarg8);
  ndefer_s4 = ret_val12;
  CGEN_d695831906(ndefer_self4);
  SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
  ret_val = r;
  return ret_val;
 }
 L16 = ATTR(ATTR(self,prog),distributed);
 L171_=!(L16); 
 if (L171_) {
  err_loc_self1 = self;
  err_loc_t1 = ((dPROG_ERR) arg);
  PROG_e176405615(ATTR(err_loc_self1,prog), err_loc_t1);
  loc = CGEN_e1639504572(self, ATTR(arg,at));
  ndefer_self5 = self;
  plus_self10 = ((STR) &INTdummy_INT_);
  plus_sarg9 = loc;
  ret_val13 = STR_ap2004550586(plus_self10, plus_sarg9);
  plus_self11 = ret_val13;
  plus_sarg10 = ((STR) &name102);
  ret_val14 = STR_ap2004550586(plus_self11, plus_sarg10);
  ndefer_s5 = ret_val14;
  CGEN_d695831906(ndefer_self5);
  SATTR(ndefer_self5,routine_code,FSTR_p1752847026(ATTR(ndefer_self5,routine_code), ndefer_s5));
  ret_val = CGEN_e1639504572(self, ATTR(arg,e));
  return ret_val;
 }
 if (ATTR(ATTR(self,prog),fast_at1)) {
  L18 = TRUE;
 } else {
  L18 = ATTR(ATTR(self,prog),fast_at2);
 }
 if (L18) {
  if (CGEN_i1006303229(self, arg)) {
   if (ATTR(ATTR(self,prog),warnings)) {
    message_self = self;
    message_s = ((STR) &Simple318344970);
    PROG_message_STR(ATTR(message_self,prog), message_s);
   }
   ret_val = CGEN_e283387820(self, arg);
   return ret_val;
  }
  else {
   if (ATTR(ATTR(self,prog),warnings)) {
    warning_self = self;
    warning_s = ((STR) &Comple1197394349);
    PROG_warning_STR(ATTR(warning_self,prog), warning_s);
   }
  }
 }
 e = ATTR(arg,e);
 if (e==NULL) {
  goto other367;
 } else
 switch (TAG(e)) {
  case AM_ROU1916046290_tag:
   loc1 = CGEN_e1639504572(self, ATTR(arg,at));
   create_self1 = ((AM_BND1124877163) NULL);
   L191_=(e)==NULL?0:ASIZE((AM_ROU1916046290)e); 
   create_nargs = L191_;
   create_source = ATTR(((AM_ROU1916046290) e),source1);
   L22 = create_nargs;
   L24=(sizeof(struct AM_BND1124877163_struct)-sizeof(AM_CALL_ARG))+(L22)*sizeof(AM_CALL_ARG);
   L23=ZALLOC_BIG(L24);
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=AM_BND1124877163_tag;
#ifdef DESTROY_CHK

     ((OB)L23)->header.destroyed=0;
#endif

   L20 = ((AM_BND1124877163) L23);
   L20->asize = L22;
   r2 = L20;
   SATTR(r2,source1,create_source);
   ret_val15 = r2;
   bnd = ret_val15;
   {
    BOOL f_L251_ = TRUE;
    BOOL f_L261_ = TRUE;
    /* loop index variable */
    L51 = 0;
    L11 = ((AM_ROU1916046290) e);
    L21 = bnd;
    L251_br=L11==NULL?0:ASIZE((AM_ROU1916046290)L11); 
    L261_br=L21==NULL?0:ASIZE((AM_BND1124877163)L21); 
    while (1) {
     if(L51>=L261_br)  goto after_loop; 
     if(L51>=L251_br)  goto after_loop; 
     aL251_=ARR((AM_ROU1916046290)L11,L51); 
     L28 = aL251_;
     SARR((AM_BND1124877163)L21,L51,(AM_CALL_ARG)L28); 
     ;
     L291_=INTPLUS(L51,1); 
     L51 = L291_;
    }
   }
   after_loop: ;
   SATTR(bnd,is_remote,TRUE);
   SATTR(bnd,clst,loc1);
   SATTR(bnd,fun,ATTR(((AM_ROU1916046290) e),fun));
   create_self2 = ((ARRAYINT) NULL);
   L301_=(e)==NULL?0:ASIZE((AM_ROU1916046290)e); 
   create_n = L301_;
   L33 = create_n;
   L35=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L33)*sizeof(INT);
   L34=ZALLOC_LEAF_BIG(L35);
   if (L34==NULL) FATAL("Unable to allocate more memory");
   memset(L34,0,L35);
   ((OB)L34)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

     ((OB)L34)->header.destroyed=0;
#endif

   L32 = ((ARRAYINT) L34);
   L32->asize = L33;
   ret_val16 = L32;
   SATTR(bnd,bnd_args,ret_val16);
   {
    BOOL f_L361_ = TRUE;
    BOOL f_L371_ = TRUE;
    L31 = 0;
    /* loop index variable */
    L61 = 0;
    L41 = ATTRs(bnd,bnd_args,((ARRAYINT) NULL));
    L361_=L31-1; 
    L371_br=L41==NULL?0:ASIZE((ARRAYINT)L41); 
    while (1) {
     if(L61>=L371_br)  goto after_loop1; 
     L361_++; 
     L39 = L361_;
     SARR((ARRAYINT)L41,L61,L39); 
     ;
     L401_=INTPLUS(L61,1); 
     L61 = L401_;
    }
   }
   after_loop1: ;
   tbnd = TP_ROU52430373(((TP_ROUT) NULL), ((ARRAYARG) NULL), ATTR(ATTR(((AM_ROU1916046290) e),fun),ret), ATTR(self,prog));
   SATTR(bnd,tp_at,((dTP) tbnd));
   L42 = &al;
   rt = CGEN_e409242292(self, bnd, FALSE, L42);
   ndefer_self6 = self;
   plus_self12 = rt;
   plus_sarg11 = ((STR) &localSENDFOB);
   ret_val17 = STR_ap2004550586(plus_self12, plus_sarg11);
   plus_self13 = ret_val17;
   plus_sarg12 = rt;
   ret_val18 = STR_ap2004550586(plus_self13, plus_sarg12);
   plus_self14 = ret_val18;
   plus_sarg13 = ((STR) &name3);
   ret_val19 = STR_ap2004550586(plus_self14, plus_sarg13);
   plus_self15 = ret_val19;
   plus_sarg14 = loc1;
   ret_val20 = STR_ap2004550586(plus_self15, plus_sarg14);
   plus_self16 = ret_val20;
   plus_sarg15 = ((STR) &name76);
   ret_val21 = STR_ap2004550586(plus_self16, plus_sarg15);
   ndefer_s6 = ret_val21;
   CGEN_d695831906(ndefer_self6);
   SATTR(ndefer_self6,routine_code,FSTR_p1752847026(ATTR(ndefer_self6,routine_code), ndefer_s6));
   L43 = ATTR(arg,e);
   L44 = ((*dAM_EXPR_tprdTP[TAG(L43)])(L43)==((dTP) NULL));
   L451_=!(L44); 
   if (L451_) {
    L46 = ATTR(arg,e);
    res = CGEN_d1425664957(self, (*dAM_EXPR_tprdTP[TAG(L46)])(L46));
    ndefer_self7 = self;
    plus_self17 = ((STR) &REMOTE_EXEC);
    plus_sarg16 = loc1;
    ret_val22 = STR_ap2004550586(plus_self17, plus_sarg16);
    plus_self18 = ret_val22;
    plus_sarg17 = ((STR) &name3);
    ret_val23 = STR_ap2004550586(plus_self18, plus_sarg17);
    plus_self19 = ret_val23;
    plus_sarg18 = rt;
    ret_val24 = STR_ap2004550586(plus_self19, plus_sarg18);
    plus_self20 = ret_val24;
    plus_sarg19 = ((STR) &name76);
    ret_val25 = STR_ap2004550586(plus_self20, plus_sarg19);
    ndefer_s7 = ret_val25;
    CGEN_d695831906(ndefer_self7);
    SATTR(ndefer_self7,routine_code,FSTR_p1752847026(ATTR(ndefer_self7,routine_code), ndefer_s7));
    tp_self = ((AM_ROU1916046290) e);
    ret_val26 = ATTR(ATTR(tp_self,fun),ret);
    L47 = ret_val26;
    if ((*dTP_is1334578382[TAG(L47)])(L47)) {
     tp_self1 = ((AM_ROU1916046290) e);
     ret_val27 = ATTR(ATTR(tp_self1,fun),ret);
     L48 = ret_val27;
     L49 = (*dTP_is418055720[TAG(L48)])(L48);
     L501_=!(L49); 
     if (L501_) {
      ndefer_self8 = self;
      plus_self21 = ((STR) &RECVOB1);
      tag_for_self = self;
      tp_self2 = ((AM_ROU1916046290) e);
      ret_val28 = ATTR(ATTR(tp_self2,fun),ret);
      tag_for_tp = ret_val28;
      SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
      mang_self = tag_for_self;
      mang_ob = ((OB) tag_for_tp);
      ret_val30 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
      res1 = STR_ap2004550586(ret_val30, ((STR) &S_tag));
      forbid_self = tag_for_self;
      forbid_s = res1;
      forbid_self1 = ATTR(forbid_self,mangler);
      forbid_s1 = forbid_s;
      SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
      ret_val29 = res1;
      plus_sarg20 = ret_val29;
      ret_val31 = STR_ap2004550586(plus_self21, plus_sarg20);
      plus_self22 = ret_val31;
      plus_sarg21 = ((STR) &name100);
      ret_val32 = STR_ap2004550586(plus_self22, plus_sarg21);
      plus_self23 = ret_val32;
      plus_sarg22 = rt;
      ret_val33 = STR_ap2004550586(plus_self23, plus_sarg22);
      plus_self24 = ret_val33;
      plus_sarg23 = ((STR) &ret_arg);
      ret_val34 = STR_ap2004550586(plus_self24, plus_sarg23);
      plus_self25 = ret_val34;
      plus_sarg24 = loc1;
      ret_val35 = STR_ap2004550586(plus_self25, plus_sarg24);
      plus_self26 = ret_val35;
      plus_sarg25 = ((STR) &name76);
      ret_val36 = STR_ap2004550586(plus_self26, plus_sarg25);
      ndefer_s8 = ret_val36;
      CGEN_d695831906(ndefer_self8);
      SATTR(ndefer_self8,routine_code,FSTR_p1752847026(ATTR(ndefer_self8,routine_code), ndefer_s8));
     }
     plus_self27 = rt;
     plus_sarg26 = ((STR) &ret_arg1);
     ret_val37 = STR_ap2004550586(plus_self27, plus_sarg26);
     res = ret_val37;
    }
    else {
     plus_self28 = ((STR) &RECVFOB2);
     plus_sarg27 = rt;
     ret_val38 = STR_ap2004550586(plus_self28, plus_sarg27);
     plus_self29 = ret_val38;
     plus_sarg28 = ((STR) &ret_arg);
     ret_val39 = STR_ap2004550586(plus_self29, plus_sarg28);
     plus_self30 = ret_val39;
     plus_sarg29 = loc1;
     ret_val40 = STR_ap2004550586(plus_self30, plus_sarg29);
     plus_self31 = ret_val40;
     plus_sarg30 = ((STR) &name26);
     ret_val41 = STR_ap2004550586(plus_self31, plus_sarg30);
     res = ret_val41;
    }
   }
   else {
    plus_self32 = ((STR) &REMOTE_EXEC);
    plus_sarg31 = loc1;
    ret_val42 = STR_ap2004550586(plus_self32, plus_sarg31);
    plus_self33 = ret_val42;
    plus_sarg32 = ((STR) &name3);
    ret_val43 = STR_ap2004550586(plus_self33, plus_sarg32);
    plus_self34 = ret_val43;
    plus_sarg33 = rt;
    ret_val44 = STR_ap2004550586(plus_self34, plus_sarg33);
    plus_self35 = ret_val44;
    plus_sarg34 = ((STR) &name76);
    ret_val45 = STR_ap2004550586(plus_self35, plus_sarg34);
    res = ret_val45;
   }
   CGEN_g136295071(self, bnd, al, rt, loc1);
   ret_val = res;
   return ret_val; break;
  default: ;
  other367: ;
   warning_self1 = self;
   plus_self36 = ((STR) &cluste72287095);
   L521_=SYSTP(e); 
   L53 = L521_;
   L541_=SYSSTRFORTP(L53); 
   plus_sarg35 = L541_;
   ret_val46 = STR_ap2004550586(plus_self36, plus_sarg35);
   plus_self37 = ret_val46;
   plus_sarg36 = ((STR) &ignoring);
   ret_val47 = STR_ap2004550586(plus_self37, plus_sarg36);
   warning_s1 = ret_val47;
   PROG_warning_STR(ATTR(warning_self1,prog), warning_s1);
   ndefer_self9 = self;
   ndefer_s9 = ((STR) &thefol1791862455);
   CGEN_d695831906(ndefer_self9);
   SATTR(ndefer_self9,routine_code,FSTR_p1752847026(ATTR(ndefer_self9,routine_code), ndefer_s9));
   ret_val = CGEN_e1639504572(self, e);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_p1740273317(CGEN self, SIG fun11, dAM_CALL_EXPR call11, ARRAYSTR arg_list) {
 STR ret_val;
 STR res = ((STR) NULL);
 STR ret = ((STR) NULL);
 STR biname;
 ARRAYSTR L11;
 ARRAYSTR L21;
 STR uniq;
 ARRAYSTR L31;
 ARRAYSTR L41;
 ARRAYSTR exec11;
 ARRAYSTR L51;
 INT i = INT_zero;
 INT L61 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 CODE_FILE uses_extern_self;
 STR uses_extern_dec;
 INT L71 = INT_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 CODE_FILE uses_extern_self1;
 STR uses_extern_dec1;
 CGEN builtin_cntr_sel;
 INT ret_val3 = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 OB mang_ns;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 INT L81 = INT_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val10;
 CGEN expand_macro_sel;
 STR expand_macro_r;
 dAM_CALL_EXPR expand_macro_cal;
 SIG expand_macro_fun;
 ARRAYSTR expand_macro_arg;
 STR expand_macro_uni;
 STR ret_val11;
 CODE_FILE plus_self6;
 dSTR plus_s;
 CODE_FILE ret_val12;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val13;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val14;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val15;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val16;
 STR r1;
 INT L91 = INT_zero;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val17;
 CGEN expand_macro_sel1;
 STR expand_macro_r1;
 dAM_CALL_EXPR expand_macro_cal1;
 SIG expand_macro_fun1;
 ARRAYSTR expand_macro_arg1;
 STR expand_macro_uni1;
 STR ret_val18;
 CODE_FILE plus_self10;
 dSTR plus_s1;
 CODE_FILE ret_val19;
 INT L101 = INT_zero;
 STR plus_self11;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val20;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val21;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val22;
 STR r2;
 STR plus_self12;
 STR plus_sarg7;
 STR ret_val23;
 STR plus_self13;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val24;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val25;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val26;
 STR r3;
 STR plus_self14;
 STR plus_sarg8;
 STR ret_val27;
 STR plus_self15;
 INT plus_arg4 = INT_zero;
 STR ret_val28;
 STR s4;
 INT str_self4 = INT_zero;
 STR ret_val29;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val30;
 FSTR str_self5;
 STR ret_val31;
 STR plus_self16;
 STR plus_sarg9;
 STR ret_val32;
 STR plus_self17;
 STR plus_sarg10;
 STR ret_val33;
 STR plus_self18;
 STR plus_sarg11;
 STR ret_val34;
 STR plus_self19;
 STR plus_sarg12;
 STR ret_val35;
 STR plus_self20;
 STR plus_sarg13;
 STR ret_val36;
 CGEN expand_macro_sel2;
 STR expand_macro_r2;
 dAM_CALL_EXPR expand_macro_cal2;
 SIG expand_macro_fun2;
 ARRAYSTR expand_macro_arg2;
 STR expand_macro_uni2;
 STR ret_val37;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN expand_macro_sel3;
 STR expand_macro_r3;
 dAM_CALL_EXPR expand_macro_cal3;
 SIG expand_macro_fun3;
 ARRAYSTR expand_macro_arg3;
 STR expand_macro_uni3;
 STR ret_val38;
 extern STR Bofbui1943088302;
 dSTAT L12;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 INT L181_br;
 STR aL181_;
 STR L19;
 extern STR name9;
 INT L201_;
 BOOL L22;
 BOOL L23;
 BOOL L241_;
 INT L251_br;
 STR aL251_;
 STR L26;
 extern STR name9;
 INT L271_;
 INT L28;
 INT L291_;
 OB L30;
 OB L32;
 extern STR S_;
 BOOL L33;
 BOOL L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 STR L39;
 INT L40;
 OB L42;
 INT L43;
 STR L45;
 INT L381_br;
 STR aL381_;
 INT L461_;
 extern STR name1;
 BOOL L47;
 BOOL L481_;
 FSTR L49;
 dSTR L50;
 extern STR name1;
 BOOL L52;
 BOOL L53;
 BOOL L541_;
 STR L56;
 INT L57;
 OB L58;
 INT L59;
 STR L62;
 INT L551_br;
 STR aL551_;
 INT L631_;
 extern STR name1;
 BOOL L64;
 BOOL L651_;
 FSTR L66;
 dSTR L67;
 extern STR name1;
 INT L681_br;
 INT rL681_;
 INT L69;
 BOOL L70;
 INT L721_;
 INT L73;
 INT L741_;
 INT L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 STR L791_;
 STR L801_;
 STR L82;
 CHAR L831_;
 CHAR L84;
 BOOL L851_;
 STR L86;
 INT L87;
 OB L88;
 INT L89;
 STR L921_;
 STR L93;
 INT L94;
 OB L95;
 INT L96;
 extern STR line3;
 BOOL L98;
 BOOL L991_;
 extern STR name71;
 extern STR name73;
 STR L1001_;
 extern STR name10;
 INT L1021_;
 extern STR name1;
 extern STR name1;
 biname = ((STR) NULL);
 L12 = ATTR(ATTR(self,prog),stat1);
 (*dSTAT_incr_STR[TAG(L12)])(L12, ((STR) &Bofbui1943088302));
 L14 = ATTR(ATTR(self,prog),distributed);
 L151_=!(L14); 
 if (L151_) {
  L16 = (ATTR(ATTR(fun11,builtin_info),declare)==((ARRAYSTR) NULL));
  L171_=!(L16); 
  L13 = L171_;
 } else {
  L13 = FALSE;
 }
 if (L13) {
  {
   BOOL f_L181_ = TRUE;
   /* loop index variable */
   L61 = 0;
   L11 = ATTR(ATTR(fun11,builtin_info),declare);
   L181_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
   while (1) {
    uses_extern_self = ATTR(self,code_c);
    if(L61>=L181_br)  goto after_loop; 
    aL181_=ARR((ARRAYSTR)L11,L61); 
    plus_self = aL181_;
    plus_sarg = ((STR) &name9);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    uses_extern_dec = ret_val1;
    SATTR(uses_extern_self,externs1,FSETST1404644833(ATTR(uses_extern_self,externs1), uses_extern_dec));
    L201_=INTPLUS(L61,1); 
    L61 = L201_;
   }
  }
  after_loop: ;
 }
 if (ATTR(ATTR(self,prog),distributed)) {
  L23 = (ATTR(ATTR(fun11,builtin_info),f_declare)==((ARRAYSTR) NULL));
  L241_=!(L23); 
  L22 = L241_;
 } else {
  L22 = FALSE;
 }
 if (L22) {
  {
   BOOL f_L251_ = TRUE;
   /* loop index variable */
   L71 = 0;
   L21 = ATTR(ATTR(fun11,builtin_info),f_declare);
   L251_br=L21==NULL?0:ASIZE((ARRAYSTR)L21); 
   while (1) {
    uses_extern_self1 = ATTR(self,code_c);
    if(L71>=L251_br)  goto after_loop1; 
    aL251_=ARR((ARRAYSTR)L21,L71); 
    plus_self1 = aL251_;
    plus_sarg1 = ((STR) &name9);
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    uses_extern_dec1 = ret_val2;
    SATTR(uses_extern_self1,externs1,FSETST1404644833(ATTR(uses_extern_self1,externs1), uses_extern_dec1));
    L271_=INTPLUS(L71,1); 
    L71 = L271_;
   }
  }
  after_loop1: ;
 }
 mang_self = self;
 builtin_cntr_sel = self;
 L28 = ATTR(builtin_cntr_sel,builtin_cntr_n);
 L291_=INTPLUS(L28,1); 
 SATTR(builtin_cntr_sel,builtin_cntr_n,L291_);
 ret_val3 = ATTR(builtin_cntr_sel,builtin_cntr_n);
 L32=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 memset(L32,0,sizeof(struct INT_boxed_struct));
 ((OB)L32)->header.tag=INT_tag;
 L30 = (OB)((INT_boxed) L32);
 ((INT_boxed) L30)->immutable_part = ret_val3;
 mang_ob = L30;
 mang_ns = ((OB) ATTR(self,current_sig));
 ret_val4 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, mang_ns);
 plus_self2 = ret_val4;
 plus_sarg2 = ((STR) &S_);
 ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
 uniq = ret_val5;
 L34 = ATTR(ATTR(self,prog),distributed);
 L351_=!(L34); 
 if (L351_) {
  L36 = (ATTR(ATTR(fun11,builtin_info),var)==((ARRAYSTR) NULL));
  L371_=!(L36); 
  L33 = L371_;
 } else {
  L33 = FALSE;
 }
 if (L33) {
  {
   BOOL f_L381_ = TRUE;
   /* loop index variable */
   L81 = 0;
   while (1) {
    plus_self3 = res;
    plus_sarg3 = CGEN_eolrSTR(self);
    ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val6;
    plus_arg = ' ';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
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
    r = L39;
    SARR((STR)r,0,create_c); 
    ;
    ret_val9 = r;
    ret_val8 = ret_val9;
    s = ret_val8;
    ret_val7 = STR_ap1077157958(plus_self4, s, TRUE);
    plus_self5 = ret_val7;
    if (f_L381_) {
     f_L381_ = FALSE;
     L31 = ATTR(ATTR(fun11,builtin_info),var);
     L381_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
    }
    if(L81>=L381_br)  goto after_loop2; 
    aL381_=ARR((ARRAYSTR)L31,L81); 
    plus_sarg4 = aL381_;
    ret_val10 = STR_ap2004550586(plus_self5, plus_sarg4);
    res = ret_val10;
    L461_=INTPLUS(L81,1); 
    L81 = L461_;
   }
  }
  after_loop2: ;
  plus_self6 = ATTR(self,code_c);
  expand_macro_sel = self;
  expand_macro_r = res;
  expand_macro_cal = call11;
  expand_macro_fun = fun11;
  expand_macro_arg = arg_list;
  expand_macro_uni = uniq;
  ret_val11 = CGEN_e1606764211(expand_macro_sel, expand_macro_r, expand_macro_cal, expand_macro_fun, expand_macro_arg, expand_macro_uni, ((STR) &name1), ((STR) NULL));
  plus_s = ((dSTR) ret_val11);
  L47 = (plus_s==((dSTR) NULL));
  L481_=!(L47); 
  if (L481_) {
   L49 = ATTR(plus_self6,ntext);
   L50 = plus_s;
   SATTR(plus_self6,ntext,FSTR_p1752847026(L49, (*dSTR_strrSTR[TAG(L50)])(L50)));
  }
  ret_val12 = plus_self6;
  SATTR(self,code_c,ret_val12);
  res = ((STR) &name1);
 }
 if (ATTR(ATTR(self,prog),distributed)) {
  L53 = (ATTR(ATTR(fun11,builtin_info),f_var)==((ARRAYSTR) NULL));
  L541_=!(L53); 
  L52 = L541_;
 } else {
  L52 = FALSE;
 }
 if (L52) {
  {
   BOOL f_L551_ = TRUE;
   /* loop index variable */
   L91 = 0;
   while (1) {
    plus_self7 = res;
    plus_sarg5 = CGEN_eolrSTR(self);
    ret_val13 = STR_ap2004550586(plus_self7, plus_sarg5);
    plus_self8 = ret_val13;
    plus_arg1 = ' ';
    str_self1 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self1;
    L57 = 1;
    L59=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L57)*sizeof(CHAR);
    L58=ZALLOC_LEAF_BIG(L59);
    if (L58==NULL) FATAL("Unable to allocate more memory");
    memset(L58,0,L59);
    ((OB)L58)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L58)->header.destroyed=0;
#endif

    L56 = ((STR) L58);
    L56->asize = L57;
    r1 = L56;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val16 = r1;
    ret_val15 = ret_val16;
    s1 = ret_val15;
    ret_val14 = STR_ap1077157958(plus_self8, s1, TRUE);
    plus_self9 = ret_val14;
    if (f_L551_) {
     f_L551_ = FALSE;
     L41 = ATTR(ATTR(fun11,builtin_info),f_var);
     L551_br=L41==NULL?0:ASIZE((ARRAYSTR)L41); 
    }
    if(L91>=L551_br)  goto after_loop3; 
    aL551_=ARR((ARRAYSTR)L41,L91); 
    plus_sarg6 = aL551_;
    ret_val17 = STR_ap2004550586(plus_self9, plus_sarg6);
    res = ret_val17;
    L631_=INTPLUS(L91,1); 
    L91 = L631_;
   }
  }
  after_loop3: ;
  plus_self10 = ATTR(self,code_c);
  expand_macro_sel1 = self;
  expand_macro_r1 = res;
  expand_macro_cal1 = call11;
  expand_macro_fun1 = fun11;
  expand_macro_arg1 = arg_list;
  expand_macro_uni1 = uniq;
  ret_val18 = CGEN_e1606764211(expand_macro_sel1, expand_macro_r1, expand_macro_cal1, expand_macro_fun1, expand_macro_arg1, expand_macro_uni1, ((STR) &name1), ((STR) NULL));
  plus_s1 = ((dSTR) ret_val18);
  L64 = (plus_s1==((dSTR) NULL));
  L651_=!(L64); 
  if (L651_) {
   L66 = ATTR(plus_self10,ntext);
   L67 = plus_s1;
   SATTR(plus_self10,ntext,FSTR_p1752847026(L66, (*dSTR_strrSTR[TAG(L67)])(L67)));
  }
  ret_val19 = plus_self10;
  SATTR(self,code_c,ret_val19);
  res = ((STR) &name1);
 }
 exec11 = ATTR(ATTR(fun11,builtin_info),exec);
 if (ATTR(ATTR(self,prog),distributed)) {
  exec11 = ATTR(ATTR(fun11,builtin_info),f_exec);
 }
 {
  BOOL f_L681_ = TRUE;
  /* loop index variable */
  L101 = 0;
  L51 = exec11;
  L681_br=L51==NULL?0:ASIZE((ARRAYSTR)L51); 
  while (1) {
   if(L101>=L681_br)  goto after_loop4; 
   rL681_=L101; 
   i = rL681_;
   L721_=(exec11)==NULL?0:ASIZE((ARRAYSTR)exec11); 
   L73 = L721_;
   L741_=INTMINUS(L73,1); 
   L75 = L741_;
   L761_=(i)==(L75); 
   if (L761_) {
    L77 = (ATTR(fun11,ret)==((dTP) NULL));
    L781_=!(L77); 
    L70 = L781_;
   } else {
    L70 = FALSE;
   }
   if (L70) {
    L791_=(STR)ARR((ARRAYSTR)exec11,i); 
    ret = L791_;
   }
   else {
    L801_=(STR)ARR((ARRAYSTR)exec11,i); 
    L82 = L801_;
    L831_=ARR((STR)L82,0); 
    L84 = L831_;
    L851_=L84=='#'; 
    if (L851_) {
     plus_self11 = res;
     plus_arg2 = '\n';
     str_self2 = plus_arg2;
     create_self2 = ((STR) NULL);
     create_c2 = str_self2;
     L87 = 1;
     L89=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L87)*sizeof(CHAR);
     L88=ZALLOC_LEAF_BIG(L89);
     if (L88==NULL) FATAL("Unable to allocate more memory");
     memset(L88,0,L89);
     ((OB)L88)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L88)->header.destroyed=0;
#endif

     L86 = ((STR) L88);
     L86->asize = L87;
     r2 = L86;
     SARR((STR)r2,0,create_c2); 
     ;
     ret_val22 = r2;
     ret_val21 = ret_val22;
     s2 = ret_val21;
     ret_val20 = STR_ap1077157958(plus_self11, s2, TRUE);
     plus_self12 = ret_val20;
     L921_=(STR)ARR((ARRAYSTR)exec11,i); 
     plus_sarg7 = L921_;
     ret_val23 = STR_ap2004550586(plus_self12, plus_sarg7);
     plus_self13 = ret_val23;
     plus_arg3 = '\n';
     str_self3 = plus_arg3;
     create_self3 = ((STR) NULL);
     create_c3 = str_self3;
     L94 = 1;
     L96=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L94)*sizeof(CHAR);
     L95=ZALLOC_LEAF_BIG(L96);
     if (L95==NULL) FATAL("Unable to allocate more memory");
     memset(L95,0,L96);
     ((OB)L95)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L95)->header.destroyed=0;
#endif

     L93 = ((STR) L95);
     L93->asize = L94;
     r3 = L93;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val26 = r3;
     ret_val25 = ret_val26;
     s3 = ret_val25;
     ret_val24 = STR_ap1077157958(plus_self13, s3, TRUE);
     res = ret_val24;
     if (ATTR(self,debug1)) {
      plus_self14 = res;
      plus_sarg8 = ((STR) &line3);
      ret_val27 = STR_ap2004550586(plus_self14, plus_sarg8);
      plus_self15 = ret_val27;
      plus_arg4 = ATTR(self,last_lineno);
      str_self4 = plus_arg4;
      clear_self = buf1;
      L98 = (clear_self==((FSTR) NULL));
      L991_=!(L98); 
      if (L991_) {
       SATTR(clear_self,loc,0);
      }
      str_in_self = str_self4;
      str_in_s = buf1;
      ret_val30 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
      buf1 = ret_val30;
      str_self5 = buf1;
      ret_val31 = STR_fr1097270334(((STR) NULL), str_self5);
      ret_val29 = ret_val31;
      s4 = ret_val29;
      ret_val28 = STR_ap1077157958(plus_self15, s4, TRUE);
      plus_self16 = ret_val28;
      plus_sarg9 = ((STR) &name71);
      ret_val32 = STR_ap2004550586(plus_self16, plus_sarg9);
      plus_self17 = ret_val32;
      plus_sarg10 = ATTR(self,last_file);
      ret_val33 = STR_ap2004550586(plus_self17, plus_sarg10);
      plus_self18 = ret_val33;
      plus_sarg11 = ((STR) &name73);
      ret_val34 = STR_ap2004550586(plus_self18, plus_sarg11);
      res = ret_val34;
     }
    }
    else {
     plus_self19 = res;
     L1001_=(STR)ARR((ARRAYSTR)exec11,i); 
     plus_sarg12 = L1001_;
     ret_val35 = STR_ap2004550586(plus_self19, plus_sarg12);
     plus_self20 = ret_val35;
     plus_sarg13 = ((STR) &name10);
     ret_val36 = STR_ap2004550586(plus_self20, plus_sarg13);
     res = ret_val36;
    }
   }
   L1021_=INTPLUS(L101,1); 
   L101 = L1021_;
  }
 }
 after_loop4: ;
 ndefer_self = self;
 expand_macro_sel2 = self;
 expand_macro_r2 = res;
 expand_macro_cal2 = call11;
 expand_macro_fun2 = fun11;
 expand_macro_arg2 = arg_list;
 expand_macro_uni2 = uniq;
 ret_val37 = CGEN_e1606764211(expand_macro_sel2, expand_macro_r2, expand_macro_cal2, expand_macro_fun2, expand_macro_arg2, expand_macro_uni2, ((STR) &name1), ((STR) NULL));
 ndefer_s = ret_val37;
 CGEN_d695831906(ndefer_self);
 SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
 expand_macro_sel3 = self;
 expand_macro_r3 = ret;
 expand_macro_cal3 = call11;
 expand_macro_fun3 = fun11;
 expand_macro_arg3 = arg_list;
 expand_macro_uni3 = uniq;
 ret_val38 = CGEN_e1606764211(expand_macro_sel3, expand_macro_r3, expand_macro_cal3, expand_macro_fun3, expand_macro_arg3, expand_macro_uni3, ((STR) &name1), ((STR) NULL));
 ret_val = ret_val38;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CGEN_p1779648146(CGEN self, SIG fun11, ARRAYBOOL nearness, dAM_EXPR amexpr, ARRAYSTR arg_list) {
 STR ret_val;
 dAM_EXPR l;
 dAM_CALL_EXPR call11 = ((dAM_CALL_EXPR) NULL);
 BOOL binding_iter_clo = BOOL_zero;
 BOOL binding_rout_clo = BOOL_zero;
 STR name111;
 STR self_str = ((STR) NULL);
 IDENT ident_iter = IDENT_zero;
 IDENT ident_rout = IDENT_zero;
 STR res = ((STR) NULL);
 STR dtbl_ptr;
 STR self_ob;
 STR suffix;
 BOOL has_local = BOOL_zero;
 ARRAYINT L11;
 INT i = INT_zero;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT i1 = INT_zero;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val1;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 IDENT is_eq_self1 = IDENT_zero;
 IDENT is_eq_i1 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 CGEN mang_self1;
 OB mang_ob1;
 STR ret_val5;
 CGEN mang_self2;
 OB mang_ob2;
 STR ret_val6;
 SIG is_builtin_routi;
 BOOL ret_val7 = BOOL_zero;
 SIG is_routine_self;
 BOOL ret_val8 = BOOL_zero;
 SIG is_iter_self;
 BOOL ret_val9 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 SIG is_builtin_iter_;
 BOOL ret_val11 = BOOL_zero;
 SIG is_iter_self2;
 BOOL ret_val12 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 CGEN barf_self;
 STR barf_msg;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val14;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val15;
 SIG is_iter_self4;
 BOOL ret_val16 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val17 = BOOL_zero;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val18;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val19;
 SIG is_routine_self1;
 BOOL ret_val20 = BOOL_zero;
 SIG is_iter_self6;
 BOOL ret_val21 = BOOL_zero;
 IDENT is_iter_self7 = IDENT_zero;
 BOOL ret_val22 = BOOL_zero;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val23;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val24;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val25;
 CGEN runtime_error_se;
 STR runtime_error_s;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val26;
 STR plus_self9;
 STR plus_sarg9;
 STR ret_val27;
 CGEN ndefer_self2;
 STR ndefer_s2;
 CGEN ndefer_self3;
 STR ndefer_s3;
 CGEN ndefer_self4;
 STR ndefer_s4;
 SIG is_iter_self8;
 BOOL ret_val28 = BOOL_zero;
 IDENT is_iter_self9 = IDENT_zero;
 BOOL ret_val29 = BOOL_zero;
 CGEN barf_self2;
 STR barf_msg2;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self10;
 STR plus_sarg10;
 STR ret_val30;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self11;
 STR plus_sarg11;
 STR ret_val31;
 CGEN barf_self4;
 STR barf_msg4;
 PROG barf_self5;
 STR barf_msg5;
 PROG barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 STR plus_self12;
 STR plus_sarg12;
 STR ret_val32;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 INT L41 = INT_zero;
 AM_CALL_ARG is_near_self;
 BOOL ret_val33 = BOOL_zero;
 STR plus_self13;
 STR plus_sarg13;
 STR ret_val34;
 STR plus_self14;
 STR plus_sarg14;
 STR ret_val35;
 STR plus_self15;
 STR plus_sarg15;
 STR ret_val36;
 STR plus_self16;
 STR plus_sarg16;
 STR ret_val37;
 STR plus_self17;
 STR plus_sarg17;
 STR ret_val38;
 STR plus_self18;
 STR plus_sarg18;
 STR ret_val39;
 extern STR name1;
 BOOL L5;
 BOOL L61_;
 extern STR callb;
 extern STR call1;
 BOOL L7;
 BOOL L8;
 BOOL L91_;
 ARRAYINT L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 extern STR bound_arg0;
 extern STR oncearg0;
 extern STR f3;
 extern STR call2;
 BOOL L15;
 BOOL L16;
 BOOL L171_;
 ARRAYINT L18;
 INT L191_;
 INT L20;
 BOOL L221_;
 extern STR unboun1814884135;
 extern STR obboun2027030443;
 BOOL L23;
 BOOL L24;
 BOOL L25;
 BOOL L261_;
 STR L27;
 INT L28;
 INT L291_;
 INT L30;
 CHAR L321_;
 CHAR L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 STR L42;
 INT L43;
 INT L441_;
 INT L45;
 CHAR L461_;
 CHAR L47;
 BOOL L481_;
 dTP L49;
 extern STR Dispat1038375013;
 extern STR Intern28965746;
 extern STR name24;
 dTP L50;
 BOOL L51;
 BOOL L52;
 BOOL L531_;
 STR L54;
 INT L55;
 INT L561_;
 INT L57;
 CHAR L581_;
 CHAR L59;
 BOOL L601_;
 extern STR name85;
 extern STR TAGfit851270275;
 BOOL L62;
 BOOL L63;
 BOOL L641_;
 STR L65;
 INT L66;
 INT L671_;
 INT L68;
 CHAR L691_;
 CHAR L70;
 BOOL L711_;
 BOOL L72;
 BOOL L731_;
 extern STR name36;
 STR L741_;
 STR L75;
 extern STR name16;
 BOOL L77;
 BOOL L78;
 BOOL L791_;
 extern STR if3;
 STR L801_;
 STR L81;
 extern STR NULL3;
 extern STR Dispatchedcallto;
 extern STR onvoidselfin;
 extern STR FATAL1;
 extern STR name95;
 extern STR name4;
 extern STR COUNT_DISPATCH1;
 BOOL L82;
 BOOL L83;
 BOOL L84;
 BOOL L851_;
 STR L86;
 INT L87;
 INT L881_;
 INT L89;
 CHAR L901_;
 CHAR L92;
 BOOL L931_;
 dTP L94;
 extern STR Distri728606362;
 extern STR Intern28965746;
 extern STR name85;
 extern STR F_TAG1;
 STR L951_;
 STR L96;
 extern STR name96;
 extern STR name85;
 extern STR TAG1;
 STR L971_;
 STR L98;
 extern STR name96;
 extern STR Inemit721671903;
 extern STR Intern28965746;
 extern STR name1;
 BOOL L99;
 BOOL L100;
 BOOL L101;
 BOOL L1021_;
 BOOL L103;
 BOOL L1041_;
 extern STR S___;
 INT L1051_br;
 INT aL1051_;
 INT L106;
 BOOL L107;
 BOOL L108;
 BOOL L109;
 BOOL L1101_;
 BOOL L1111_;
 BOOL L112;
 BOOL L113;
 BOOL L1141_;
 dAM_CALL_EXPR L115;
 dAM_EXPR L116;
 extern STR L;
 extern STR f11;
 INT L1171_;
 BOOL L1181_;
 extern STR name1;
 extern STR name24;
 BOOL L119;
 BOOL L1201_;
 BOOL L1221_;
 dTP L123;
 BOOL L124;
 BOOL L1251_;
 STR L1261_;
 INT L1281_;
 INT L129;
 INT L1301_;
 INT L1271_,L1271_m;
 INT L131;
 dTP L132;
 BOOL L1331_;
 STR L1341_;
 extern STR name29;
 STR L1351_;
 extern STR name29;
 STR L1361_;
 extern STR pFF;
 extern STR FF11;
 extern STR name26;
 l = amexpr;
 binding_iter_clo = FALSE;
 binding_rout_clo = FALSE;
 name111 = ((STR) &name1);
 L5 = (l==((dAM_EXPR) NULL));
 L61_=!(L5); 
 if (L61_) {
  switch (TAG(l)) {
   case AM_BND367211769_tag:
   case AM_BND260301329_tag:
   case AM_EXT_CALL_EXPR_tag:
   case AM_ITE1809135850_tag:
   case AM_ROU1916046290_tag:
    call11 = ((dAM_CALL_EXPR) l);
    mang_self = self;
    mang_ob = ((OB) fun11);
    ret_val1 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
    name111 = ret_val1; break;
   case AM_BND1124877163_tag:
    call11 = ((dAM_CALL_EXPR) NULL);
    ident_iter = IDENT_1150413730(IDENT_zero, ((STR) &callb));
    ident_rout = IDENT_1150413730(IDENT_zero, ((STR) &call1));
    is_eq_self = ATTR(fun11,name1);
    is_eq_i = ident_iter;
    ret_val2 = (is_eq_self.str==is_eq_i.str);
    if (ret_val2) {
     binding_iter_clo = TRUE;
     L8 = (ATTR(((AM_BND1124877163) l),bnd_args)==((ARRAYINT) NULL));
     L91_=!(L8); 
     if (L91_) {
      L10 = ATTR(((AM_BND1124877163) l),bnd_args);
      L121_=ARR((ARRAYINT)L10,0); 
      L13 = L121_;
      L141_=(L13)==(0); 
      L7 = L141_;
     } else {
      L7 = FALSE;
     }
     if (L7) {
      self_str = ((STR) &bound_arg0);
     }
     else {
      self_str = ((STR) &oncearg0);
     }
     plus_self = self_str;
     plus_sarg = ((STR) &call2);
     ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
     name111 = STR_ap2004550586(((STR) &f3), ret_val3);
    }
    else {
     is_eq_self1 = ATTR(fun11,name1);
     is_eq_i1 = ident_rout;
     ret_val4 = (is_eq_self1.str==is_eq_i1.str);
     if (ret_val4) {
      binding_rout_clo = TRUE;
      L16 = (ATTR(((AM_BND1124877163) l),unbnd_args)==((ARRAYINT) NULL));
      L171_=!(L16); 
      if (L171_) {
       L18 = ATTR(((AM_BND1124877163) l),unbnd_args);
       L191_=ARR((ARRAYINT)L18,0); 
       L20 = L191_;
       L221_=(L20)==(0); 
       L15 = L221_;
      } else {
       L15 = FALSE;
      }
      if (L15) {
       name111 = ((STR) &unboun1814884135);
      }
      else {
       name111 = ((STR) &obboun2027030443);
      }
     }
     else {
      mang_self1 = self;
      mang_ob1 = ((OB) fun11);
      ret_val5 = MANGLE119219986(ATTR(mang_self1,mangler), mang_ob1, ((OB) NULL));
      name111 = ret_val5;
     }
    } break;
   default: ;
    FATAL("No applicable type in typecase\nin CGEN::pemit_call(SIG,ARRAY{BOOL},$AM_EXPR,ARRAY{STR}):STR\n./Back/cgen.sa:5131:17");
  }
 }
 else {
  call11 = ((dAM_CALL_EXPR) NULL);
  mang_self2 = self;
  mang_ob2 = ((OB) fun11);
  ret_val6 = MANGLE119219986(ATTR(mang_self2,mangler), mang_ob2, ((OB) NULL));
  name111 = ret_val6;
 }
 is_builtin_routi = fun11;
 if (SIG_is418491101(is_builtin_routi)) {
  is_routine_self = is_builtin_routi;
  is_iter_self = is_routine_self;
  is_iter_self1 = ATTR(is_iter_self,name1);
  L25 = (is_iter_self1.str==((STR) NULL));
  L261_=!(L25); 
  if (L261_) {
   L27 = is_iter_self1.str;
   L28 = STR_sizerINT(is_iter_self1.str);
   L291_=INTMINUS(L28,1); 
   L30 = L291_;
   L321_=ARR((STR)L27,L30); 
   L33 = L321_;
   L341_=L33=='!'; 
   L24 = L341_;
  } else {
   L24 = FALSE;
  }
  ret_val10 = L24;
  ret_val9 = ret_val10;
  L35 = ret_val9;
  L361_=!(L35); 
  ret_val8 = L361_;
  L23 = ret_val8;
 } else {
  L23 = FALSE;
 }
 ret_val7 = L23;
 if (ret_val7) {
  ret_val = CGEN_p1740273317(self, fun11, call11, arg_list);
  return ret_val;
 }
 else {
  is_builtin_iter_ = fun11;
  if (SIG_is418491101(is_builtin_iter_)) {
   is_iter_self2 = is_builtin_iter_;
   is_iter_self3 = ATTR(is_iter_self2,name1);
   L39 = (is_iter_self3.str==((STR) NULL));
   L401_=!(L39); 
   if (L401_) {
    L42 = is_iter_self3.str;
    L43 = STR_sizerINT(is_iter_self3.str);
    L441_=INTMINUS(L43,1); 
    L45 = L441_;
    L461_=ARR((STR)L42,L45); 
    L47 = L461_;
    L481_=L47=='!'; 
    L38 = L481_;
   } else {
    L38 = FALSE;
   }
   ret_val13 = L38;
   ret_val12 = ret_val13;
   L37 = ret_val12;
  } else {
   L37 = FALSE;
  }
  ret_val11 = L37;
  if (ret_val11) {
   L49 = ATTR(fun11,tp);
   if ((*dTP_is1811059018[TAG(L49)])(L49)) {
    barf_self = self;
    barf_msg = ((STR) &Dispat1038375013);
    barf_self1 = ATTR(barf_self,prog);
    barf_msg1 = barf_msg;
    barf_at_self = barf_self1;
    barf_at_msg = barf_msg1;
    barf_at_at = ((dPROG_ERR) NULL);
    PROG_e176405615(barf_at_self, barf_at_at);
    plus_self1 = ((STR) &Intern28965746);
    plus_sarg1 = barf_at_msg;
    ret_val14 = STR_ap2004550586(plus_self1, plus_sarg1);
    PROG_err_STR(barf_at_self, ret_val14);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
   }
   else {
    plus_self2 = name111;
    plus_sarg2 = ((STR) &name24);
    ret_val15 = STR_ap2004550586(plus_self2, plus_sarg2);
    res = ret_val15;
   }
  }
  else {
   L50 = ATTR(fun11,tp);
   if ((*dTP_is1811059018[TAG(L50)])(L50)) {
    is_iter_self4 = fun11;
    is_iter_self5 = ATTR(is_iter_self4,name1);
    L52 = (is_iter_self5.str==((STR) NULL));
    L531_=!(L52); 
    if (L531_) {
     L54 = is_iter_self5.str;
     L55 = STR_sizerINT(is_iter_self5.str);
     L561_=INTMINUS(L55,1); 
     L57 = L561_;
     L581_=ARR((STR)L54,L57); 
     L59 = L581_;
     L601_=L59=='!'; 
     L51 = L601_;
    } else {
     L51 = FALSE;
    }
    ret_val17 = L51;
    ret_val16 = ret_val17;
    if (ret_val16) {
     dtbl_ptr = name111;
     plus_self3 = ((STR) &name85);
     plus_sarg3 = dtbl_ptr;
     ret_val18 = STR_ap2004550586(plus_self3, plus_sarg3);
     plus_self4 = ret_val18;
     plus_sarg4 = ((STR) &TAGfit851270275);
     ret_val19 = STR_ap2004550586(plus_self4, plus_sarg4);
     res = ret_val19;
    }
    else {
     is_routine_self1 = fun11;
     is_iter_self6 = is_routine_self1;
     is_iter_self7 = ATTR(is_iter_self6,name1);
     L63 = (is_iter_self7.str==((STR) NULL));
     L641_=!(L63); 
     if (L641_) {
      L65 = is_iter_self7.str;
      L66 = STR_sizerINT(is_iter_self7.str);
      L671_=INTMINUS(L66,1); 
      L68 = L671_;
      L691_=ARR((STR)L65,L68); 
      L70 = L691_;
      L711_=L70=='!'; 
      L62 = L711_;
     } else {
      L62 = FALSE;
     }
     ret_val22 = L62;
     ret_val21 = ret_val22;
     L72 = ret_val21;
     L731_=!(L72); 
     ret_val20 = L731_;
     if (ret_val20) {
      self_ob = CGEN_d1425664957(self, ATTR(fun11,tp));
      ndefer_self = self;
      L741_=(STR)ARR((ARRAYSTR)arg_list,0); 
      L75 = L741_;
      ndefer_s = STR_ap1417971546(self_ob, ((STR) &name36), L75, ((STR) &name16));
      CGEN_d695831906(ndefer_self);
      SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
      SARR((ARRAYSTR)arg_list,0,(STR)self_ob); 
      ;
      if (ATTR(self,chk_void)) {
       L78 = ATTR(self,null_segfaults);
       L791_=!(L78); 
       L77 = L791_;
      } else {
       L77 = FALSE;
      }
      if (L77) {
       ndefer_self1 = self;
       L801_=(STR)ARR((ARRAYSTR)arg_list,0); 
       L81 = L801_;
       ndefer_s1 = STR_ap1693864410(((STR) &if3), L81, ((STR) &NULL3));
       CGEN_d695831906(ndefer_self1);
       SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
       runtime_error_se = self;
       plus_self5 = ((STR) &Dispatchedcallto);
       plus_sarg5 = ATTR(fun11,str);
       ret_val23 = STR_ap2004550586(plus_self5, plus_sarg5);
       plus_self6 = ret_val23;
       plus_sarg6 = ((STR) &onvoidselfin);
       ret_val24 = STR_ap2004550586(plus_self6, plus_sarg6);
       plus_self7 = ret_val24;
       plus_sarg7 = ATTR(self,current_function);
       ret_val25 = STR_ap2004550586(plus_self7, plus_sarg7);
       runtime_error_s = ret_val25;
       ndefer_self2 = runtime_error_se;
       plus_self8 = ((STR) &FATAL1);
       plus_sarg8 = runtime_error_s;
       ret_val26 = STR_ap2004550586(plus_self8, plus_sarg8);
       plus_self9 = ret_val26;
       plus_sarg9 = ((STR) &name95);
       ret_val27 = STR_ap2004550586(plus_self9, plus_sarg9);
       ndefer_s2 = ret_val27;
       CGEN_d695831906(ndefer_self2);
       SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
       ndefer_self3 = self;
       ndefer_s3 = ((STR) &name4);
       CGEN_d695831906(ndefer_self3);
       SATTR(ndefer_self3,routine_code,FSTR_p1752847026(ATTR(ndefer_self3,routine_code), ndefer_s3));
      }
      if (ATTR(ATTR(self,options1),stats)) {
       ndefer_self4 = self;
       ndefer_s4 = ((STR) &COUNT_DISPATCH1);
       CGEN_d695831906(ndefer_self4);
       SATTR(ndefer_self4,routine_code,FSTR_p1752847026(ATTR(ndefer_self4,routine_code), ndefer_s4));
      }
      if (ATTR(ATTR(self,prog),distributed)) {
       is_iter_self8 = fun11;
       is_iter_self9 = ATTR(is_iter_self8,name1);
       L84 = (is_iter_self9.str==((STR) NULL));
       L851_=!(L84); 
       if (L851_) {
        L86 = is_iter_self9.str;
        L87 = STR_sizerINT(is_iter_self9.str);
        L881_=INTMINUS(L87,1); 
        L89 = L881_;
        L901_=ARR((STR)L86,L89); 
        L92 = L901_;
        L931_=L92=='!'; 
        L83 = L931_;
       } else {
        L83 = FALSE;
       }
       ret_val29 = L83;
       ret_val28 = ret_val29;
       if (ret_val28) {
        L94 = ATTR(fun11,tp);
        L82 = (*dTP_is735947028[TAG(L94)])(L94);
       } else {
        L82 = FALSE;
       }
       if (L82) {
        barf_self2 = self;
        barf_msg2 = ((STR) &Distri728606362);
        barf_self3 = ATTR(barf_self2,prog);
        barf_msg3 = barf_msg2;
        barf_at_self1 = barf_self3;
        barf_at_msg1 = barf_msg3;
        barf_at_at1 = ((dPROG_ERR) NULL);
        PROG_e176405615(barf_at_self1, barf_at_at1);
        plus_self10 = ((STR) &Intern28965746);
        plus_sarg10 = barf_at_msg1;
        ret_val30 = STR_ap2004550586(plus_self10, plus_sarg10);
        PROG_err_STR(barf_at_self1, ret_val30);
        exit_self1 = ((UNIX) NULL);
        exit_code1 = 1;
        exit(exit_code1);
       }
       else {
        L951_=(STR)ARR((ARRAYSTR)arg_list,0); 
        L96 = L951_;
        res = STR_ap44172742(((STR) &name85), name111, ((STR) &F_TAG1), L96, ((STR) &name96));
       }
      }
      else {
       L971_=(STR)ARR((ARRAYSTR)arg_list,0); 
       L98 = L971_;
       res = STR_ap44172742(((STR) &name85), name111, ((STR) &TAG1), L98, ((STR) &name96));
      }
     }
     else {
      barf_self4 = self;
      plus_self11 = ((STR) &Inemit721671903);
      plus_sarg11 = ATTR(fun11,str);
      ret_val31 = STR_ap2004550586(plus_self11, plus_sarg11);
      barf_msg4 = ret_val31;
      barf_self5 = ATTR(barf_self4,prog);
      barf_msg5 = barf_msg4;
      barf_at_self2 = barf_self5;
      barf_at_msg2 = barf_msg5;
      barf_at_at2 = ((dPROG_ERR) NULL);
      PROG_e176405615(barf_at_self2, barf_at_at2);
      plus_self12 = ((STR) &Intern28965746);
      plus_sarg12 = barf_at_msg2;
      ret_val32 = STR_ap2004550586(plus_self12, plus_sarg12);
      PROG_err_STR(barf_at_self2, ret_val32);
      exit_self2 = ((UNIX) NULL);
      exit_code2 = 1;
      exit(exit_code2);
     }
    }
   }
   else {
    suffix = ((STR) &name1);
    if (ATTR(ATTR(self,options1),local_call)) {
     L101 = (ATTR(fun11,opt_info)==((OPT_LOCAL_CALL) NULL));
     L1021_=!(L101); 
     L100 = L1021_;
    } else {
     L100 = FALSE;
    }
    if (L100) {
     L103 = (ATTR(ATTR(fun11,opt_info),consider)==((ARRAYINT) NULL));
     L1041_=!(L103); 
     L99 = L1041_;
    } else {
     L99 = FALSE;
    }
    if (L99) {
     suffix = ((STR) &S___);
     has_local = FALSE;
     {
      BOOL f_L1051_ = TRUE;
      /* loop index variable */
      L41 = 0;
      L11 = ATTR(ATTR(fun11,opt_info),consider);
      L1051_br=L11==NULL?0:ASIZE((ARRAYINT)L11); 
      while (1) {
       if(L41>=L1051_br)  goto after_loop; 
       aL1051_=ARR((ARRAYINT)L11,L41); 
       i = aL1051_;
       L109 = (nearness==((ARRAYBOOL) NULL));
       L1101_=!(L109); 
       if (L1101_) {
        L1111_=ARR((ARRAYBOOL)nearness,i); 
        L108 = L1111_;
       } else {
        L108 = FALSE;
       }
       if (L108) {
        L107 = TRUE;
       } else {
        L113 = (call11==((dAM_CALL_EXPR) NULL));
        L1141_=!(L113); 
        if (L1141_) {
         L115 = call11;
         is_near_self = (*dAM_CA967858872[TAG(L115)])(L115, i);
         L116 = ATTR(is_near_self,expr);
         ret_val33 = (*dAM_EX1117610980[TAG(L116)])(L116);
         L112 = ret_val33;
        } else {
         L112 = FALSE;
        }
        L107 = L112;
       }
       if (L107) {
        has_local = TRUE;
        plus_self13 = suffix;
        plus_sarg13 = ((STR) &L);
        ret_val34 = STR_ap2004550586(plus_self13, plus_sarg13);
        suffix = ret_val34;
       }
       else {
        plus_self14 = suffix;
        plus_sarg14 = ((STR) &f11);
        ret_val35 = STR_ap2004550586(plus_self14, plus_sarg14);
        suffix = ret_val35;
       }
       L1171_=INTPLUS(L41,1); 
       L41 = L1171_;
      }
     }
     after_loop: ;
     L1181_=!(has_local); 
     if (L1181_) {
      suffix = ((STR) &name1);
     }
    }
    plus_self15 = name111;
    plus_sarg15 = suffix;
    ret_val36 = STR_ap2004550586(plus_self15, plus_sarg15);
    plus_self16 = ret_val36;
    plus_sarg16 = ((STR) &name24);
    ret_val37 = STR_ap2004550586(plus_self16, plus_sarg16);
    res = ret_val37;
   }
  }
 }
 L1201_=!(binding_iter_clo); 
 if (L1201_) {
  L1221_=!(binding_rout_clo); 
  L119 = L1221_;
 } else {
  L119 = FALSE;
 }
 if (L119) {
  CODE_F1291349628(ATTR(self,code_c), fun11);
 }
 L123 = ATTR(fun11,tp);
 L124 = (*dTP_is124163553[TAG(L123)])(L123);
 L1251_=!(L124); 
 if (L1251_) {
  plus_self17 = res;
  L1261_=(STR)ARR((ARRAYSTR)arg_list,0); 
  plus_sarg17 = L1261_;
  ret_val38 = STR_ap2004550586(plus_self17, plus_sarg17);
  res = ret_val38;
 }
 {
  BOOL f_L1271_ = TRUE;
  L21 = 1;
  L1281_=(arg_list)==NULL?0:ASIZE((ARRAYSTR)arg_list); 
  L129 = L1281_;
  L1301_=INTMINUS(L129,1); 
  L31 = L1301_;
  L1271_=L21-1;L1271_m=L31; 
  while (1) {
   if(L1271_++>=L1271_m)  goto after_loop1; 
   i1 = L1271_;
   L132 = ATTR(fun11,tp);
   if ((*dTP_is124163553[TAG(L132)])(L132)) {
    L1331_=(i1)==(1); 
    if (L1331_) {
     plus_self18 = res;
     L1341_=(STR)ARR((ARRAYSTR)arg_list,i1); 
     plus_sarg18 = L1341_;
     ret_val39 = STR_ap2004550586(plus_self18, plus_sarg18);
     res = ret_val39;
    }
    else {
     L1351_=(STR)ARR((ARRAYSTR)arg_list,i1); 
     res = STR_ap1693864410(res, ((STR) &name29), L1351_);
    }
   }
   else {
    L1361_=(STR)ARR((ARRAYSTR)arg_list,i1); 
    res = STR_ap1693864410(res, ((STR) &name29), L1361_);
   }
  }
 }
 after_loop1: ;
 if (ATTR(self,func_tables)) {
  if (ATTR(self,in_bnd_rout_call)) {
   res = STR_ap2004550586(res, ((STR) &pFF));
  }
  else {
   res = STR_ap2004550586(res, ((STR) &FF11));
  }
 }
 ret_val = STR_ap2004550586(res, ((STR) &name26));
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CGEN_g136295071(CGEN self, AM_BND1124877163 arg, FLISTSTR arglist, STR local11, STR where_str) {
 AM_BND1124877163 L11;
 INT i = INT_zero;
 STR entry1 = ((STR) NULL);
 INT idx = INT_zero;
 ARG a;
 STR ex1;
 FLISTSTR L21;
 STR b1;
 INT L31 = INT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 INT plus_arg = INT_zero;
 STR ret_val1;
 STR s;
 INT str_self = INT_zero;
 STR ret_val2;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 CGEN cast_self;
 dTP cast_dest_tp;
 dTP cast_src_tp;
 STR cast_expr;
 BOOL cast_only_boxing = BOOL_zero;
 STR ret_val5;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val7;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val8;
 CGEN ndefer_self;
 STR ndefer_s;
 CGEN tag_for_self;
 dTP tag_for_tp;
 STR ret_val9;
 STR res;
 CGEN mang_self;
 OB mang_ob;
 STR ret_val10;
 CGEN forbid_self;
 STR forbid_s;
 MANGLE forbid_self1;
 STR forbid_s1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val12;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val15;
 STR plus_self10;
 STR plus_sarg9;
 STR ret_val16;
 CGEN ndefer_self1;
 STR ndefer_s1;
 STR plus_self11;
 STR plus_sarg10;
 STR ret_val17;
 STR plus_self12;
 STR plus_sarg11;
 STR ret_val18;
 STR plus_self13;
 STR plus_sarg12;
 STR ret_val19;
 STR plus_self14;
 STR plus_sarg13;
 STR ret_val20;
 STR plus_self15;
 STR plus_sarg14;
 STR ret_val21;
 CGEN ndefer_self2;
 STR ndefer_s2;
 INT L41_br;
 INT rL41_;
 INT L5;
 ARRAYINT L6;
 INT L71_;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 ARRAYARG L12;
 INT L131_;
 INT L14;
 ARG L151_;
 extern STR name1;
 dMODE L16;
 BOOL L17;
 BOOL L181_;
 dMODE L19;
 AM_CALL_ARG L201_;
 AM_CALL_ARG L22;
 STR L23;
 extern STR bound_arg;
 BOOL L24;
 BOOL L251_;
 AM_CALL_ARG L261_;
 AM_CALL_ARG L27;
 dAM_EXPR L28;
 AM_CALL_ARG L291_;
 AM_CALL_ARG L30;
 dAM_EXPR L32;
 dTP L33;
 extern STR name36;
 extern STR name16;
 AM_CALL_ARG L341_;
 AM_CALL_ARG L35;
 dAM_EXPR L36;
 dTP L37;
 BOOL L38;
 BOOL L391_;
 extern STR RECVOB1;
 AM_CALL_ARG L401_;
 AM_CALL_ARG L42;
 dAM_EXPR L43;
 extern STR S_tag;
 extern STR name100;
 extern STR name3;
 extern STR name76;
 extern STR RECVFOB1;
 extern STR name3;
 extern STR name76;
 INT L441_;
 {
  BOOL f_L41_ = TRUE;
  struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
  /* loop index variable */
  L31 = 0;
  L11 = arg;
  L21 = arglist;
  L41_br=L11==NULL?0:ASIZE((AM_BND1124877163)L11); 
  noname1->self = L21;
  noname1->state = 0;
  while (1) {
   if(L31>=L41_br)  goto after_loop; 
   rL41_=L31; 
   i = rL41_;
   L6 = ATTR(arg,bnd_args);
   L71_=ARR((ARRAYINT)L6,i); 
   idx = L71_;
   L81_=(idx)==(0); 
   L9 = L81_;
   L101_=!(L9); 
   if (L101_) {
    L12 = ATTR(ATTR(arg,fun),args);
    L131_=INTMINUS(idx,1); 
    L14 = L131_;
    L151_=(ARG)ARR((ARRAYARG)L12,L14); 
    a = L151_;
    ex1 = ((STR) &name1);
    L16 = ATTR(a,mode1);
    L17 = (*dMODE_814247358[TAG(L16)])(L16, ((dMODE) MODES_in_mode));
    L181_=!(L17); 
    if (L181_) {
     L19 = ATTR(a,mode1);
     if ((*dMODE_814247358[TAG(L19)])(L19, ((dMODE) MODES_out_mode))) {
      L201_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
      L22=L201_;
      ex1 = CGEN_e1639504572(self, ATTR(L22,expr));
     }
     else {
      L23 = FLISTS1171631134(noname1);
      if (noname1->state == -1) {
       goto after_loop;
      }
      ex1 = L23;
     }
     plus_self = local11;
     plus_sarg = ((STR) &bound_arg);
     ret_val = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val;
     plus_arg = i;
     str_self = plus_arg;
     clear_self = buf1;
     L24 = (clear_self==((FSTR) NULL));
     L251_=!(L24); 
     if (L251_) {
      SATTR(clear_self,loc,0);
     }
     str_in_self = str_self;
     str_in_s = buf1;
     ret_val3 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
     buf1 = ret_val3;
     str_self1 = buf1;
     ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
     ret_val2 = ret_val4;
     s = ret_val2;
     ret_val1 = STR_ap1077157958(plus_self1, s, TRUE);
     b1 = ret_val1;
     cast_self = self;
     L261_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L27=L261_;
     L28 = ATTR(L27,expr);
     cast_dest_tp = (*dAM_EXPR_tprdTP[TAG(L28)])(L28);
     cast_src_tp = ATTR(a,tp);
     cast_expr = b1;
     cast_only_boxing = FALSE;
     ret_val5 = CGEN_d1915593494(cast_self, cast_dest_tp, cast_src_tp, cast_expr, cast_only_boxing, ((dMODE) NULL));
     entry1 = ret_val5;
     L291_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
     L30=L291_;
     L32 = ATTR(L30,expr);
     L33 = (*dAM_EXPR_tprdTP[TAG(L32)])(L32);
     if ((*dTP_is1334578382[TAG(L33)])(L33)) {
      ndefer_self = self;
      plus_self2 = ex1;
      plus_sarg1 = ((STR) &name36);
      ret_val6 = STR_ap2004550586(plus_self2, plus_sarg1);
      plus_self3 = ret_val6;
      plus_sarg2 = entry1;
      ret_val7 = STR_ap2004550586(plus_self3, plus_sarg2);
      plus_self4 = ret_val7;
      plus_sarg3 = ((STR) &name16);
      ret_val8 = STR_ap2004550586(plus_self4, plus_sarg3);
      ndefer_s = ret_val8;
      CGEN_d695831906(ndefer_self);
      SATTR(ndefer_self,routine_code,FSTR_p1752847026(ATTR(ndefer_self,routine_code), ndefer_s));
      L341_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
      L35=L341_;
      L36 = ATTR(L35,expr);
      L37 = (*dAM_EXPR_tprdTP[TAG(L36)])(L36);
      L38 = (*dTP_is418055720[TAG(L37)])(L37);
      L391_=!(L38); 
      if (L391_) {
       ndefer_self1 = self;
       plus_self5 = ((STR) &RECVOB1);
       tag_for_self = self;
       L401_=(AM_CALL_ARG)ARR((AM_BND1124877163)arg,i); 
       L42=L401_;
       L43 = ATTR(L42,expr);
       tag_for_tp = (*dAM_EXPR_tprdTP[TAG(L43)])(L43);
       SATTR(tag_for_self,needs_tag,FSETdT1016814448(ATTR(tag_for_self,needs_tag), tag_for_tp));
       mang_self = tag_for_self;
       mang_ob = ((OB) tag_for_tp);
       ret_val10 = MANGLE119219986(ATTR(mang_self,mangler), mang_ob, ((OB) NULL));
       res = STR_ap2004550586(ret_val10, ((STR) &S_tag));
       forbid_self = tag_for_self;
       forbid_s = res;
       forbid_self1 = ATTR(forbid_self,mangler);
       forbid_s1 = forbid_s;
       SATTR(forbid_self1,forbidden,FSETST1404644833(ATTR(forbid_self1,forbidden), forbid_s1));
       ret_val9 = res;
       plus_sarg4 = ret_val9;
       ret_val11 = STR_ap2004550586(plus_self5, plus_sarg4);
       plus_self6 = ret_val11;
       plus_sarg5 = ((STR) &name100);
       ret_val12 = STR_ap2004550586(plus_self6, plus_sarg5);
       plus_self7 = ret_val12;
       plus_sarg6 = ex1;
       ret_val13 = STR_ap2004550586(plus_self7, plus_sarg6);
       plus_self8 = ret_val13;
       plus_sarg7 = ((STR) &name3);
       ret_val14 = STR_ap2004550586(plus_self8, plus_sarg7);
       plus_self9 = ret_val14;
       plus_sarg8 = where_str;
       ret_val15 = STR_ap2004550586(plus_self9, plus_sarg8);
       plus_self10 = ret_val15;
       plus_sarg9 = ((STR) &name76);
       ret_val16 = STR_ap2004550586(plus_self10, plus_sarg9);
       ndefer_s1 = ret_val16;
       CGEN_d695831906(ndefer_self1);
       SATTR(ndefer_self1,routine_code,FSTR_p1752847026(ATTR(ndefer_self1,routine_code), ndefer_s1));
      }
     }
     else {
      ndefer_self2 = self;
      plus_self11 = ex1;
      plus_sarg10 = ((STR) &RECVFOB1);
      ret_val17 = STR_ap2004550586(plus_self11, plus_sarg10);
      plus_self12 = ret_val17;
      plus_sarg11 = entry1;
      ret_val18 = STR_ap2004550586(plus_self12, plus_sarg11);
      plus_self13 = ret_val18;
      plus_sarg12 = ((STR) &name3);
      ret_val19 = STR_ap2004550586(plus_self13, plus_sarg12);
      plus_self14 = ret_val19;
      plus_sarg13 = where_str;
      ret_val20 = STR_ap2004550586(plus_self14, plus_sarg13);
      plus_self15 = ret_val20;
      plus_sarg14 = ((STR) &name76);
      ret_val21 = STR_ap2004550586(plus_self15, plus_sarg14);
      ndefer_s2 = ret_val21;
      CGEN_d695831906(ndefer_self2);
      SATTR(ndefer_self2,routine_code,FSTR_p1752847026(ATTR(ndefer_self2,routine_code), ndefer_s2));
     }
    }
   }
   L441_=INTPLUS(L31,1); 
   L31 = L441_;
  }
 }
 after_loop: ;
}
