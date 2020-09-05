#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

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

typedef struct OPTIMIZE_struct {/* layout for OPTIMIZE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPTIMIZE;

typedef struct OPT_CONST_struct {/* layout for OPT_CONST */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_CONST;

typedef struct OPT_CSE_struct {/* layout for OPT_CSE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_CSE;

typedef struct OPT_ITER_struct {/* layout for OPT_ITER */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_ITER;

typedef struct OPT_LOCAL_struct {/* layout for OPT_LOCAL */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_LOCAL;

typedef struct OPT_PREFETCH_struct {/* layout for OPT_PREFETCH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_PREFETCH;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

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

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct AM_ASSERT_STMT_struct {/* layout for AM_ASSERT_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_ASSERT_STMT;

typedef struct AM_ATTACH_STMT_struct {/* layout for AM_ATTACH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *gate;
 struct AM_LOCAL_EXPR_struct *helper;
 struct SFILE_ID_struct source1;
 struct SIG_struct *rout;
 } *AM_ATTACH_STMT;

typedef struct AM_ATTR_EXPR_struct {/* layout for AM_ATTR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 BOOL secure1;
 } *AM_ATTR_EXPR;

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

typedef struct AM_CURSOR_struct {/* layout for AM_CURSOR */
 OB_HEADER header;
 struct dAM_struct *cur;
 struct dAM_struct *top1;
 struct A_STAC1244966098_struct *stack1;
 INT indent;
 INT loops;
 struct PROG_struct *prog;
 BOOL all_in_eval_orde;
 BOOL all_in_pre_order;
 BOOL assign_in_order;
 BOOL calls_in_eval_or;
 BOOL ignore_assert;
 BOOL ignore_next;
 BOOL ignore_post;
 BOOL ignore_pre;
 BOOL mark;
 BOOL started;
 BOOL with_side_effect;
 } *AM_CURSOR;

typedef struct AM_CUR1040670508_frame_struct {
 INT state;
 AM_CURSOR self;/* Formal argument: self */
 dAM ret_val;
 dAM n;
 } *AM_CUR1040670508_frame;

typedef struct AM_HERE_EXPR_struct {/* layout for AM_HERE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_HERE_EXPR;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

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

typedef struct AM_LOCK_STMT_struct {/* layout for AM_LOCK_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_EXPR_struct *guards;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTA1327600838_struct *locks1;
 struct SFILE_ID_struct source1;
 BOOL manual_unlock;
 } *AM_LOCK_STMT;

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

typedef struct AM_POST_STMT_struct {/* layout for AM_POST_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_POST_STMT;

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_PRE_STMT_struct {/* layout for AM_PRE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_PRE_STMT;

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

typedef struct AM_STMT_EXPR_struct {/* layout for AM_STMT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dAM_EXPR_struct *replaced;
 struct dAM_STMT_struct *stmts;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTA725283029_struct *locals1;
 struct SFILE_ID_struct source1;
 } *AM_STMT_EXPR;

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

typedef struct AS_INT_LIT_EXPR_struct {/* layout for AS_INT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 BOOL is_inti;
 } *AS_INT_LIT_EXPR;

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

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FSETAM1158405984_struct {/* layout for FSET{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FSETAM1158405984;

typedef struct FSETAM_ROUT_DEF_struct {/* layout for FSET{AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_ROUT_DEF_struct *arr_part[1];
 } *FSETAM_ROUT_DEF;

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

extern AM_INT_CONST OPT_CO150672939;
extern AM_INT_CONST OPT_CO150710673;
extern AM_INT_CONST OPT_CO232246741;
extern AM_INT_CONST OPT_CO238309264;
extern AM_INT_CONST OPT_CO238346998;
extern CS_OPTIONS OPT_CO1299251581;
extern PROG OPT_CO233462019;
extern TP_CLASS TP_BUI765623063;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_char;
extern TP_CLASS TP_BUILTIN_int;
extern dTP OPT_CO146016663;
extern dTP OPT_CO146094031;
extern dTP OPT_CO1800765742;

/* Function declarations */


extern RETURNED_CONST dAM_EXPR (**dINLIN1378053623)(dINLINE, AM_ROUT_DEF, AM_ROU1916046290);

extern RETURNED_CONST void (**dBACK_1049564105)(dBACK_END);

extern RETURNED_CONST void (**dBACK_1132541334)(dBACK_END, AM_ROUT_DEF);

extern RETURNED_CONST void (**dBACK_END_init)(dBACK_END);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_LOCK_STMT AM_CUR201931527(AM_CURSOR);
AM_LOOP_STMT AM_CUR153953493(AM_CURSOR);
AM_PREFETCH_STMT AM_CUR141764973(AM_CURSOR);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AM_WAITFOR_STMT AM_CUR1250801636(AM_CURSOR);
BOOL AM_CUR1106436467(AM_CURSOR);
BOOL AM_CUR1481630356(AM_CURSOR);
BOOL AM_CUR2013152412(AM_CURSOR);
BOOL AM_CUR2066246826(AM_CURSOR);
BOOL AM_CUR403450479(AM_CURSOR, dAM);
BOOL AM_CUR921612586(AM_CURSOR, dAM_STMT, INT, BOOL);
BOOL FSETAM215631726(FSETAM1158405984, AM_LOCAL_EXPR);
BOOL OPTIMI1660661857(OPTIMIZE, AM_ROUT_DEF, AM_ROU1916046290, AM_CURSOR);
BOOL OPT_CO1865909003(OPT_CONST, AM_ROUT_DEF, INT);
BOOL STR_is111530248(STR, STR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FSETAM1158405984 FSETAM483153547(FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM83361925(FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
INT INT_up418511718(INT_up418511718_frame);
INT STR_se2132153226(STR, STR);
INT STR_sizerINT(STR);
INTI1 INTI_c1588968505(INTI1, INT);
OPTIMIZE OPTIMI831024750(OPTIMIZE, PROG);
SE_CONTEXT SIG_ge642497605(SIG, PROG);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap2004550586(STR, STR);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM_STMT AM_CUR643033494(AM_CURSOR);
void AM_CUR1119852682(AM_CURSOR);
void AM_CUR1754599450(AM_CURSOR);
void AM_CUR915996644(AM_CURSOR, dAM_EXPR);
void AM_OUT1439666796(AM_OUT, dAM);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void INT_timesb(INT_timesb_frame);
void OPTIMI1081780129(OPTIMIZE, AM_ROUT_DEF);
void OPTIMI333751220(OPTIMIZE);
void OPTIMI462748081(OPTIMIZE, AM_ROUT_DEF);
void OPTIMI671687564(OPTIMIZE, AM_ROUT_DEF);
void OPTIMI683370008(OPTIMIZE, dAM_EXPR);
void OPTIMIZE_init(OPTIMIZE);
void OPT_CS641813603(OPT_CSE, AM_ROUT_DEF);
void OPT_IT505798087(OPT_ITER, AM_ROUT_DEF, AM_ITE1809135850, AM_CURSOR, ARRAYBOOL);
void OPT_LO1015703310(OPT_LOCAL, AM_ROUT_DEF);
void OPT_PR1134320793(OPT_PREFETCH, AM_ROUT_DEF);
void OPT_PR1794418601(OPT_PREFETCH, AM_ROUT_DEF);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL OPTIMI1660661857(OPTIMIZE self, AM_ROUT_DEF func, AM_ROU1916046290 am, AM_CURSOR c) {
 BOOL ret_val = BOOL_zero;
 BOOL ok = BOOL_zero;
 dAM_EXPR n;
 OPTIMIZE prog_self;
 PROG ret_val1;
 OPTIMIZE make_sure_emitte;
 SIG make_sure_emitte1;
 dBACK_END cgen;
 OPTIMIZE prog_self1;
 PROG ret_val2;
 CGEN make_sure_emitte2;
 SIG make_sure_emitte3;
 AM_ROUT_DEF ard;
 OPTIMIZE prog_self2;
 PROG ret_val3;
 OUT create_self;
 OUT ret_val4;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OPTIMIZE make_sure_emitte4;
 SIG make_sure_emitte5;
 dBACK_END cgen1;
 OPTIMIZE prog_self3;
 PROG ret_val6;
 CGEN make_sure_emitte6;
 SIG make_sure_emitte7;
 AM_ROUT_DEF ard1;
 OPTIMIZE prog_self4;
 PROG ret_val7;
 OUT create_self1;
 OUT ret_val8;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val9;
 FILE1 stdout_self1;
 FILE1 ret_val10;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val11;
 FILE1 stdout_self2;
 FILE1 ret_val12;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val13;
 FILE1 r3;
 OPTIMIZE prog_self5;
 PROG ret_val14;
 OPTIMIZE prog_self6;
 PROG ret_val15;
 OPTIMIZE prog_self7;
 PROG ret_val16;
 OUT create_self2;
 OUT ret_val17;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val18;
 FILE1 r4;
 ERR1 create_self3;
 ERR1 ret_val19;
 ERR1 plus_self5;
 dSTR plus_s5;
 FILE1 stderr_self;
 FILE1 ret_val20;
 FILE1 r5;
 OPTIMIZE prog_self8;
 PROG ret_val21;
 OUT create_self4;
 OUT ret_val22;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self5;
 FILE1 ret_val23;
 FILE1 r6;
 PROG L1;
 BOOL L2;
 BOOL L31_;
 PROG L4;
 BOOL L5;
 BOOL L61_;
 PROG L7;
 extern STR INSIDEFORKSTMT;
 FILE1 L8;
 OB L9;
 PROG L11;
 BOOL L12;
 BOOL L131_;
 PROG L14;
 extern STR Trying723620804;
 FILE1 L15;
 OB L16;
 FILE1 L18;
 OB L19;
 extern STR name21;
 FILE1 L21;
 OB L22;
 PROG L24;
 dINLINE L25;
 BOOL L26;
 BOOL L271_;
 PROG L28;
 extern STR Ooffun1954749845;
 dSTAT L29;
 PROG L30;
 extern STR done1;
 FILE1 L32;
 OB L33;
 extern STR huchin559792866;
 FILE1 L35;
 OB L36;
 PROG L38;
 extern STR noway;
 FILE1 L39;
 OB L40;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 L1=ret_val1;
 L2 = ATTR(L1,inline_routs);
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 if (AM_CUR2013152412(c)) {
  make_sure_emitte = self;
  make_sure_emitte1 = ATTR(am,fun);
  prog_self1 = make_sure_emitte;
  ret_val2 = OPT_CO233462019;
  L4=ret_val2;
  cgen = ATTR(L4,back_end);
  switch (TAG(cgen)) {
   case CGEN_tag:
    make_sure_emitte2 = ((CGEN) cgen);
    make_sure_emitte3 = make_sure_emitte1;
    ard = FMAPSI2016001247(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3);
    L5 = (ard==((AM_ROUT_DEF) NULL));
    L61_=!(L5); 
    if (L61_) {
     SATTR(make_sure_emitte2,leftovers,FSETAM2044524049(ATTR(make_sure_emitte2,leftovers), ard));
     SATTR(make_sure_emitte2,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3));
    } break;
   default: ;
    FATAL("No applicable type in typecase\nin OPTIMIZE::inline_func(AM_ROUT_DEF,AM_ROUT_CALL_EXPR,AM_CURSOR):BOOL\n./Back/optimize.sa:159:14");
  }
  ret_val = FALSE;
  return ret_val;
 }
 prog_self2 = self;
 ret_val3 = OPT_CO233462019;
 L7=ret_val3;
 if (ATTR(L7,fast_fork)) {
  if (AM_CUR1106436467(c)) {
   create_self = ((OUT) NULL);
   ret_val4 = create_self;
   plus_self = ret_val4;
   plus_s = ((STR) &INSIDEFORKSTMT);
   stdout_self = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r = L8;
   
   SATTR(r,fp,stdout);
   ret_val5 = r;
   FILE_plus_STR(ret_val5, plus_s);
   make_sure_emitte4 = self;
   make_sure_emitte5 = ATTR(am,fun);
   prog_self3 = make_sure_emitte4;
   ret_val6 = OPT_CO233462019;
   L11=ret_val6;
   cgen1 = ATTR(L11,back_end);
   switch (TAG(cgen1)) {
    case CGEN_tag:
     make_sure_emitte6 = ((CGEN) cgen1);
     make_sure_emitte7 = make_sure_emitte5;
     ard1 = FMAPSI2016001247(ATTR(make_sure_emitte6,not_emitted), make_sure_emitte7);
     L12 = (ard1==((AM_ROUT_DEF) NULL));
     L131_=!(L12); 
     if (L131_) {
      SATTR(make_sure_emitte6,leftovers,FSETAM2044524049(ATTR(make_sure_emitte6,leftovers), ard1));
      SATTR(make_sure_emitte6,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte6,not_emitted), make_sure_emitte7));
     } break;
    default: ;
     FATAL("No applicable type in typecase\nin OPTIMIZE::inline_func(AM_ROUT_DEF,AM_ROUT_CALL_EXPR,AM_CURSOR):BOOL\n./Back/optimize.sa:159:14");
   }
   ret_val = FALSE;
   return ret_val;
  }
 }
 ok = FALSE;
 prog_self4 = self;
 ret_val7 = OPT_CO233462019;
 L14=ret_val7;
 if (ATTR(L14,opt_debug)) {
  create_self1 = ((OUT) NULL);
  ret_val8 = create_self1;
  plus_self1 = ret_val8;
  plus_s1 = ((STR) &Trying723620804);
  stdout_self1 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r1 = L15;
  
  SATTR(r1,fp,stdout);
  ret_val10 = r1;
  FILE_plus_STR(ret_val10, plus_s1);
  ret_val9 = plus_self1;
  plus_self2 = ret_val9;
  plus_s2 = ATTR(ATTR(am,fun),str);
  stdout_self2 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r2 = L18;
  
  SATTR(r2,fp,stdout);
  ret_val12 = r2;
  FILE_plus_STR(ret_val12, plus_s2);
  ret_val11 = plus_self2;
  plus_self3 = ret_val11;
  plus_s3 = ((STR) &name21);
  stdout_self3 = ((FILE1) NULL);
  L22=ZALLOC(sizeof(struct FILE1_struct));
  if (L22==NULL) FATAL("Unable to allocate more memory");
  ((OB)L22)->header.tag=FILE1_tag;
  L21 = ((FILE1) L22);
  r3 = L21;
  
  SATTR(r3,fp,stdout);
  ret_val13 = r3;
  FILE_plus_STR(ret_val13, plus_s3);
 }
 prog_self5 = self;
 ret_val14 = OPT_CO233462019;
 L24=ret_val14;
 L25 = ATTR(L24,inliner);
 n = (*dINLIN1378053623[TAG(L25)])(L25, func, am);
 L26 = SYSOBEQ(((OB) n),((OB) am));
 L271_=!(L26); 
 if (L271_) {
  prog_self6 = self;
  ret_val15 = OPT_CO233462019;
  L28=ret_val15;
  L29 = ATTR(L28,stat1);
  (*dSTAT_incr_STR[TAG(L29)])(L29, ((STR) &Ooffun1954749845));
  prog_self7 = self;
  ret_val16 = OPT_CO233462019;
  L30=ret_val16;
  if (ATTR(L30,opt_debug)) {
   create_self2 = ((OUT) NULL);
   ret_val17 = create_self2;
   plus_self4 = ret_val17;
   plus_s4 = ((STR) &done1);
   stdout_self4 = ((FILE1) NULL);
   L33=ZALLOC(sizeof(struct FILE1_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   ((OB)L33)->header.tag=FILE1_tag;
   L32 = ((FILE1) L33);
   r4 = L32;
   
   SATTR(r4,fp,stdout);
   ret_val18 = r4;
   FILE_plus_STR(ret_val18, plus_s4);
  }
  if (n==NULL) {
   goto other292;
  } else
  switch (TAG(n)) {
   case AM_STMT_EXPR_tag:
    SATTR(((AM_STMT_EXPR) n),replaced,((dAM_EXPR) am)); break;
   default: ;
   other292: ;
    create_self3 = ((ERR1) NULL);
    ret_val19 = create_self3;
    plus_self5 = ret_val19;
    plus_s5 = ((dSTR) ((STR) &huchin559792866));
    stderr_self = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r5 = L35;
    
    SATTR(r5,fp,stderr);
    ret_val20 = r5;
    FILE_plus_dSTR(ret_val20, plus_s5);
  }
  AM_CUR915996644(c, n);
  ok = TRUE;
 }
 else {
  prog_self8 = self;
  ret_val21 = OPT_CO233462019;
  L38=ret_val21;
  if (ATTR(L38,opt_debug)) {
   create_self4 = ((OUT) NULL);
   ret_val22 = create_self4;
   plus_self6 = ret_val22;
   plus_s6 = ((STR) &noway);
   stdout_self5 = ((FILE1) NULL);
   L40=ZALLOC(sizeof(struct FILE1_struct));
   if (L40==NULL) FATAL("Unable to allocate more memory");
   ((OB)L40)->header.tag=FILE1_tag;
   L39 = ((FILE1) L40);
   r6 = L39;
   
   SATTR(r6,fp,stdout);
   ret_val23 = r6;
   FILE_plus_STR(ret_val23, plus_s6);
  }
 }
 ret_val = ok;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

OPTIMIZE OPTIMI831024750(OPTIMIZE self, PROG p) {
 OPTIMIZE ret_val;
 OPTIMIZE r;
 AS_INT_LIT_EXPR asint;
 AS_INT_LIT_EXPR create_self;
 AS_INT_LIT_EXPR ret_val1;
 AM_INT_CONST create_self1;
 AS_INT_LIT_EXPR create_t;
 AM_INT_CONST ret_val2;
 AM_INT_CONST r1;
 OPTIMIZE tp_int_self;
 dTP ret_val3;
 AS_INT_LIT_EXPR create_self2;
 AS_INT_LIT_EXPR ret_val4;
 AM_INT_CONST create_self3;
 AS_INT_LIT_EXPR create_t1;
 AM_INT_CONST ret_val5;
 AM_INT_CONST r2;
 OPTIMIZE tp_int_self1;
 dTP ret_val6;
 AS_INT_LIT_EXPR create_self4;
 AS_INT_LIT_EXPR ret_val7;
 AM_INT_CONST create_self5;
 AS_INT_LIT_EXPR create_t2;
 AM_INT_CONST ret_val8;
 AM_INT_CONST r3;
 OPTIMIZE tp_int_self2;
 dTP ret_val9;
 AS_INT_LIT_EXPR create_self6;
 AS_INT_LIT_EXPR ret_val10;
 AM_INT_CONST create_self7;
 AS_INT_LIT_EXPR create_t3;
 AM_INT_CONST ret_val11;
 AM_INT_CONST r4;
 OPTIMIZE tp_int_self3;
 dTP ret_val12;
 AS_INT_LIT_EXPR create_self8;
 AS_INT_LIT_EXPR ret_val13;
 AM_INT_CONST create_self9;
 AS_INT_LIT_EXPR create_t4;
 AM_INT_CONST ret_val14;
 AM_INT_CONST r5;
 OPTIMIZE tp_int_self4;
 dTP ret_val15;
 OPTIMIZE L1;
 OB L2;
 AS_INT_LIT_EXPR L3;
 OB L4;
 AM_INT_CONST L5;
 OB L6;
 AS_INT_LIT_EXPR L7;
 OB L8;
 AM_INT_CONST L9;
 OB L10;
 AS_INT_LIT_EXPR L11;
 OB L12;
 AM_INT_CONST L13;
 OB L14;
 AS_INT_LIT_EXPR L15;
 OB L16;
 AM_INT_CONST L17;
 OB L18;
 AS_INT_LIT_EXPR L19;
 OB L20;
 AM_INT_CONST L21;
 OB L22;
 L2=ZALLOC_LEAF(sizeof(struct OPTIMIZE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct OPTIMIZE_struct));
 ((OB)L2)->header.tag=OPTIMIZE_tag;
 L1 = ((OPTIMIZE) L2);
 r = L1;
 OPT_CO233462019 = p;
 OPT_CO1800765742 = ((dTP) TP_BUILTIN_int);
 OPT_CO146094031 = ((dTP) TP_BUILTIN_bool);
 OPT_CO146016663 = ((dTP) TP_BUILTIN_char);
 create_self = ((AS_INT_LIT_EXPR) NULL);
 L4=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_INT_LIT_EXPR_tag;
 L3 = ((AS_INT_LIT_EXPR) L4);
 ret_val1 = L3;
 asint = ret_val1;
 SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), 0));
 create_self1 = ((AM_INT_CONST) NULL);
 create_t = asint;
 L6=ZALLOC(sizeof(struct AM_INT_CONST_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=AM_INT_CONST_tag;
 L5 = ((AM_INT_CONST) L6);
 r1 = L5;
 SATTR(r1,source1,ATTR(create_t,source1));
 SATTR(r1,val1,ATTR(create_t,val1));
 ret_val2 = r1;
 OPT_CO232246741 = ret_val2;
 tp_int_self = r;
 ret_val3 = OPT_CO1800765742;
 SATTR(OPT_CO232246741,tp_at,ret_val3);
 create_self2 = ((AS_INT_LIT_EXPR) NULL);
 L8=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AS_INT_LIT_EXPR_tag;
 L7 = ((AS_INT_LIT_EXPR) L8);
 ret_val4 = L7;
 asint = ret_val4;
 SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), 1));
 create_self3 = ((AM_INT_CONST) NULL);
 create_t1 = asint;
 L10=ZALLOC(sizeof(struct AM_INT_CONST_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=AM_INT_CONST_tag;
 L9 = ((AM_INT_CONST) L10);
 r2 = L9;
 SATTR(r2,source1,ATTR(create_t1,source1));
 SATTR(r2,val1,ATTR(create_t1,val1));
 ret_val5 = r2;
 OPT_CO238346998 = ret_val5;
 tp_int_self1 = r;
 ret_val6 = OPT_CO1800765742;
 SATTR(OPT_CO238346998,tp_at,ret_val6);
 create_self4 = ((AS_INT_LIT_EXPR) NULL);
 L12=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=AS_INT_LIT_EXPR_tag;
 L11 = ((AS_INT_LIT_EXPR) L12);
 ret_val7 = L11;
 asint = ret_val7;
 SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), 2));
 create_self5 = ((AM_INT_CONST) NULL);
 create_t2 = asint;
 L14=ZALLOC(sizeof(struct AM_INT_CONST_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AM_INT_CONST_tag;
 L13 = ((AM_INT_CONST) L14);
 r3 = L13;
 SATTR(r3,source1,ATTR(create_t2,source1));
 SATTR(r3,val1,ATTR(create_t2,val1));
 ret_val8 = r3;
 OPT_CO238309264 = ret_val8;
 tp_int_self2 = r;
 ret_val9 = OPT_CO1800765742;
 SATTR(OPT_CO238309264,tp_at,ret_val9);
 create_self6 = ((AS_INT_LIT_EXPR) NULL);
 L16=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=AS_INT_LIT_EXPR_tag;
 L15 = ((AS_INT_LIT_EXPR) L16);
 ret_val10 = L15;
 asint = ret_val10;
 SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), -1));
 create_self7 = ((AM_INT_CONST) NULL);
 create_t3 = asint;
 L18=ZALLOC(sizeof(struct AM_INT_CONST_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=AM_INT_CONST_tag;
 L17 = ((AM_INT_CONST) L18);
 r4 = L17;
 SATTR(r4,source1,ATTR(create_t3,source1));
 SATTR(r4,val1,ATTR(create_t3,val1));
 ret_val11 = r4;
 OPT_CO150710673 = ret_val11;
 tp_int_self3 = r;
 ret_val12 = OPT_CO1800765742;
 SATTR(OPT_CO150710673,tp_at,ret_val12);
 create_self8 = ((AS_INT_LIT_EXPR) NULL);
 L20=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=AS_INT_LIT_EXPR_tag;
 L19 = ((AS_INT_LIT_EXPR) L20);
 ret_val13 = L19;
 asint = ret_val13;
 SATTR(asint,val1,INTI_c1588968505(((INTI1) NULL), -2));
 create_self9 = ((AM_INT_CONST) NULL);
 create_t4 = asint;
 L22=ZALLOC(sizeof(struct AM_INT_CONST_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=AM_INT_CONST_tag;
 L21 = ((AM_INT_CONST) L22);
 r5 = L21;
 SATTR(r5,source1,ATTR(create_t4,source1));
 SATTR(r5,val1,ATTR(create_t4,val1));
 ret_val14 = r5;
 OPT_CO150672939 = ret_val14;
 tp_int_self4 = r;
 ret_val15 = OPT_CO1800765742;
 SATTR(OPT_CO150672939,tp_at,ret_val15);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMI1081780129(OPTIMIZE self, AM_ROUT_DEF func) {
 FLISTSTR L11;
 AM_CURSOR c;
 AM_CURSOR L21;
 dAM am;
 INT L31 = INT_zero;
 OPTIMIZE prog_self;
 PROG ret_val;
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
 OPTIMIZE prog_self1;
 PROG ret_val7;
 OPTIMIZE prog_self2;
 PROG ret_val8;
 OPTIMIZE prog_self3;
 PROG ret_val9;
 OPTIMIZE prog_self4;
 PROG ret_val10;
 OPTIMIZE prog_self5;
 PROG ret_val11;
 OUT create_self1;
 OUT ret_val12;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val13;
 FILE1 stdout_self3;
 FILE1 ret_val14;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val15;
 FILE1 r4;
 OUT create_self2;
 OUT ret_val16;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val17;
 FILE1 stdout_self5;
 FILE1 ret_val18;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val19;
 FILE1 r6;
 OUT create_self3;
 OUT ret_val20;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val21;
 FILE1 r7;
 OPTIMIZE prog_self6;
 PROG ret_val22;
 INT L41 = INT_zero;
 OUT create_self4;
 OUT ret_val23;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val24;
 FILE1 r8;
 OUT create_self5;
 OUT ret_val25;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val26;
 FILE1 stdout_self9;
 FILE1 ret_val27;
 FILE1 r9;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val28;
 FILE1 stdout_self10;
 FILE1 ret_val29;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val30;
 FILE1 r11;
 OUT create_self6;
 OUT ret_val31;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val32;
 FILE1 r12;
 OUT create_self7;
 OUT ret_val33;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val34;
 FILE1 r13;
 OUT create_self8;
 OUT ret_val35;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val36;
 FILE1 stdout_self14;
 FILE1 ret_val37;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val38;
 FILE1 stdout_self15;
 FILE1 ret_val39;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val40;
 FILE1 r16;
 OUT create_self9;
 OUT ret_val41;
 OUT flush_self;
 FILE1 stdout_self17;
 FILE1 ret_val42;
 FILE1 r17;
 FILE1 flush_self1;
 OPTIMIZE prog_self7;
 PROG ret_val43;
 PROG L5;
 extern STR workingon;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 extern STR name9;
 FILE1 L13;
 OB L14;
 PROG L16;
 BOOL L17;
 BOOL L181_;
 PROG L19;
 STR L20;
 STR L22;
 INT L23;
 BOOL L241_;
 BOOL L25;
 BOOL L261_;
 PROG L27;
 extern STR Optimi440073291;
 FILE1 L28;
 OB L29;
 FILE1 L32;
 OB L33;
 BOOL L35;
 BOOL L361_;
 extern STR name60;
 FILE1 L37;
 OB L38;
 FILE1 L40;
 OB L42;
 extern STR name9;
 FILE1 L44;
 OB L45;
 dAM L47;
 INT L49;
 INT L501_;
 extern STR name123;
 FILE1 L51;
 OB L52;
 INT L541_;
 extern STR name77;
 FILE1 L55;
 OB L56;
 INT L581_;
 INT L59;
 STR L601_;
 FILE1 L61;
 OB L62;
 extern STR name9;
 FILE1 L64;
 OB L65;
 extern STR AM_OUT1;
 FILE1 L67;
 OB L68;
 extern STR name32;
 FILE1 L70;
 OB L71;
 extern STR Optimi440073291;
 FILE1 L73;
 OB L74;
 FILE1 L76;
 OB L77;
 extern STR DONE;
 FILE1 L79;
 OB L80;
 FILE1 L83;
 OB L84;
 PROG L86;
 dBACK_END L87;
 prog_self = self;
 ret_val = OPT_CO233462019;
 L5=ret_val;
 if (ATTR(L5,opt_verbose)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &workingon);
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
  plus_s1 = ATTR(ATTR(func,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r1 = L9;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
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
 }
 prog_self1 = self;
 ret_val7 = OPT_CO233462019;
 L16=ret_val7;
 L17 = (ATTR(L16,opt_debug_func)==((FLISTSTR) NULL));
 L181_=!(L17); 
 if (L181_) {
  prog_self2 = self;
  ret_val8 = OPT_CO233462019;
  SATTR(ret_val8,opt_debug,FALSE);
  {
   struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
   prog_self3 = self;
   ret_val9 = OPT_CO233462019;
   L19=ret_val9;
   L11 = ATTR(L19,opt_debug_func);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L20 = ATTR(ATTR(func,sig1),str);
    L22 = FLISTS1171631134(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    L23 = STR_se2132153226(L20, L22);
    L241_=(L23)<(0); 
    L25 = L241_;
    L261_=!(L25); 
    if (L261_) {
     prog_self4 = self;
     ret_val10 = OPT_CO233462019;
     SATTR(ret_val10,opt_debug,TRUE);
     goto after_loop;
    }
   }
  }
  after_loop: ;
 }
 prog_self5 = self;
 ret_val11 = OPT_CO233462019;
 L27=ret_val11;
 if (ATTR(L27,opt_debug)) {
  create_self1 = ((OUT) NULL);
  ret_val12 = create_self1;
  plus_self3 = ret_val12;
  plus_s3 = ((STR) &Optimi440073291);
  stdout_self3 = ((FILE1) NULL);
  L29=ZALLOC(sizeof(struct FILE1_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=FILE1_tag;
  L28 = ((FILE1) L29);
  r3 = L28;
  
  SATTR(r3,fp,stdout);
  ret_val14 = r3;
  FILE_plus_STR(ret_val14, plus_s3);
  ret_val13 = plus_self3;
  plus_self4 = ret_val13;
  plus_s4 = ATTR(ATTR(func,sig1),str);
  stdout_self4 = ((FILE1) NULL);
  L33=ZALLOC(sizeof(struct FILE1_struct));
  if (L33==NULL) FATAL("Unable to allocate more memory");
  ((OB)L33)->header.tag=FILE1_tag;
  L32 = ((FILE1) L33);
  r4 = L32;
  
  SATTR(r4,fp,stdout);
  ret_val15 = r4;
  FILE_plus_STR(ret_val15, plus_s4);
  L35 = (ATTR(func,srcsig)==((SIG) NULL));
  L361_=!(L35); 
  if (L361_) {
   create_self2 = ((OUT) NULL);
   ret_val16 = create_self2;
   plus_self5 = ret_val16;
   plus_s5 = ((STR) &name60);
   stdout_self5 = ((FILE1) NULL);
   L38=ZALLOC(sizeof(struct FILE1_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=FILE1_tag;
   L37 = ((FILE1) L38);
   r5 = L37;
   
   SATTR(r5,fp,stdout);
   ret_val18 = r5;
   FILE_plus_STR(ret_val18, plus_s5);
   ret_val17 = plus_self5;
   plus_self6 = ret_val17;
   plus_s6 = ATTR(ATTR(func,srcsig),str);
   stdout_self6 = ((FILE1) NULL);
   L42=ZALLOC(sizeof(struct FILE1_struct));
   if (L42==NULL) FATAL("Unable to allocate more memory");
   ((OB)L42)->header.tag=FILE1_tag;
   L40 = ((FILE1) L42);
   r6 = L40;
   
   SATTR(r6,fp,stdout);
   ret_val19 = r6;
   FILE_plus_STR(ret_val19, plus_s6);
  }
  create_self3 = ((OUT) NULL);
  ret_val20 = create_self3;
  plus_self7 = ret_val20;
  plus_s7 = ((STR) &name9);
  stdout_self7 = ((FILE1) NULL);
  L45=ZALLOC(sizeof(struct FILE1_struct));
  if (L45==NULL) FATAL("Unable to allocate more memory");
  ((OB)L45)->header.tag=FILE1_tag;
  L44 = ((FILE1) L45);
  r7 = L44;
  
  SATTR(r7,fp,stdout);
  ret_val21 = r7;
  FILE_plus_STR(ret_val21, plus_s7);
  prog_self6 = self;
  ret_val22 = OPT_CO233462019;
  c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val22, ((dAM) ATTR(func,code1)));
  {
   struct AM_CUR1040670508_frame_struct other2_0;
AM_CUR1040670508_frame noname2 = &other2_0;
   L21 = c;
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    L47 = AM_CUR1040670508(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    am = L47;
    {
     BOOL f_L481_ = TRUE;
     /* loop index variable */
     L41 = 0;
     L49 = ATTR(c,indent);
     L501_=INTPLUS(L49,1); 
     L31 = L501_;
     while (1) {
      if(L41>=L31)  goto after_loop2; 
      ;
      create_self4 = ((OUT) NULL);
      ret_val23 = create_self4;
      plus_self8 = ret_val23;
      plus_s8 = ((STR) &name123);
      stdout_self8 = ((FILE1) NULL);
      L52=ZALLOC(sizeof(struct FILE1_struct));
      if (L52==NULL) FATAL("Unable to allocate more memory");
      ((OB)L52)->header.tag=FILE1_tag;
      L51 = ((FILE1) L52);
      r8 = L51;
      
      SATTR(r8,fp,stdout);
      ret_val24 = r8;
      FILE_plus_STR(ret_val24, plus_s8);
      L541_=INTPLUS(L41,1); 
      L41 = L541_;
     }
    }
    after_loop2: ;
    create_self5 = ((OUT) NULL);
    ret_val25 = create_self5;
    plus_self9 = ret_val25;
    plus_s9 = ((STR) &name77);
    stdout_self9 = ((FILE1) NULL);
    L56=ZALLOC(sizeof(struct FILE1_struct));
    if (L56==NULL) FATAL("Unable to allocate more memory");
    ((OB)L56)->header.tag=FILE1_tag;
    L55 = ((FILE1) L56);
    r9 = L55;
    
    SATTR(r9,fp,stdout);
    ret_val27 = r9;
    FILE_plus_STR(ret_val27, plus_s9);
    ret_val26 = plus_self9;
    plus_self10 = ret_val26;
    L581_=SYSTP(am); 
    L59 = L581_;
    L601_=SYSSTRFORTP(L59); 
    plus_s10 = L601_;
    stdout_self10 = ((FILE1) NULL);
    L62=ZALLOC(sizeof(struct FILE1_struct));
    if (L62==NULL) FATAL("Unable to allocate more memory");
    ((OB)L62)->header.tag=FILE1_tag;
    L61 = ((FILE1) L62);
    r10 = L61;
    
    SATTR(r10,fp,stdout);
    ret_val29 = r10;
    FILE_plus_STR(ret_val29, plus_s10);
    ret_val28 = plus_self10;
    plus_self11 = ret_val28;
    plus_s11 = ((STR) &name9);
    stdout_self11 = ((FILE1) NULL);
    L65=ZALLOC(sizeof(struct FILE1_struct));
    if (L65==NULL) FATAL("Unable to allocate more memory");
    ((OB)L65)->header.tag=FILE1_tag;
    L64 = ((FILE1) L65);
    r11 = L64;
    
    SATTR(r11,fp,stdout);
    ret_val30 = r11;
    FILE_plus_STR(ret_val30, plus_s11);
   }
  }
  after_loop1: ;
  create_self6 = ((OUT) NULL);
  ret_val31 = create_self6;
  plus_self12 = ret_val31;
  plus_s12 = ((STR) &AM_OUT1);
  stdout_self12 = ((FILE1) NULL);
  L68=ZALLOC(sizeof(struct FILE1_struct));
  if (L68==NULL) FATAL("Unable to allocate more memory");
  ((OB)L68)->header.tag=FILE1_tag;
  L67 = ((FILE1) L68);
  r12 = L67;
  
  SATTR(r12,fp,stdout);
  ret_val32 = r12;
  FILE_plus_STR(ret_val32, plus_s12);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(func,code1)));
  create_self7 = ((OUT) NULL);
  ret_val33 = create_self7;
  plus_self13 = ret_val33;
  plus_s13 = ((STR) &name32);
  stdout_self13 = ((FILE1) NULL);
  L71=ZALLOC(sizeof(struct FILE1_struct));
  if (L71==NULL) FATAL("Unable to allocate more memory");
  ((OB)L71)->header.tag=FILE1_tag;
  L70 = ((FILE1) L71);
  r13 = L70;
  
  SATTR(r13,fp,stdout);
  ret_val34 = r13;
  FILE_plus_STR(ret_val34, plus_s13);
  create_self8 = ((OUT) NULL);
  ret_val35 = create_self8;
  plus_self14 = ret_val35;
  plus_s14 = ((STR) &Optimi440073291);
  stdout_self14 = ((FILE1) NULL);
  L74=ZALLOC(sizeof(struct FILE1_struct));
  if (L74==NULL) FATAL("Unable to allocate more memory");
  ((OB)L74)->header.tag=FILE1_tag;
  L73 = ((FILE1) L74);
  r14 = L73;
  
  SATTR(r14,fp,stdout);
  ret_val37 = r14;
  FILE_plus_STR(ret_val37, plus_s14);
  ret_val36 = plus_self14;
  plus_self15 = ret_val36;
  plus_s15 = ATTR(ATTR(func,sig1),str);
  stdout_self15 = ((FILE1) NULL);
  L77=ZALLOC(sizeof(struct FILE1_struct));
  if (L77==NULL) FATAL("Unable to allocate more memory");
  ((OB)L77)->header.tag=FILE1_tag;
  L76 = ((FILE1) L77);
  r15 = L76;
  
  SATTR(r15,fp,stdout);
  ret_val39 = r15;
  FILE_plus_STR(ret_val39, plus_s15);
  ret_val38 = plus_self15;
  plus_self16 = ret_val38;
  plus_s16 = ((STR) &DONE);
  stdout_self16 = ((FILE1) NULL);
  L80=ZALLOC(sizeof(struct FILE1_struct));
  if (L80==NULL) FATAL("Unable to allocate more memory");
  ((OB)L80)->header.tag=FILE1_tag;
  L79 = ((FILE1) L80);
  r16 = L79;
  
  SATTR(r16,fp,stdout);
  ret_val40 = r16;
  FILE_plus_STR(ret_val40, plus_s16);
 }
 create_self9 = ((OUT) NULL);
 ret_val41 = create_self9;
 flush_self = ret_val41;
 stdout_self17 = ((FILE1) NULL);
 L84=ZALLOC(sizeof(struct FILE1_struct));
 if (L84==NULL) FATAL("Unable to allocate more memory");
 ((OB)L84)->header.tag=FILE1_tag;
 L83 = ((FILE1) L84);
 r17 = L83;
 
 SATTR(r17,fp,stdout);
 ret_val42 = r17;
 flush_self1 = ret_val42;
 fflush(ATTR(flush_self1,fp));
 prog_self7 = self;
 ret_val43 = OPT_CO233462019;
 L86=ret_val43;
 L87 = ATTR(L86,back_end);
 (*dBACK_1132541334[TAG(L87)])(L87, func);
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMI333751220(OPTIMIZE self) {
 OPTIMIZE prog_self;
 PROG ret_val;
 PROG L1;
 dBACK_END L2;
 prog_self = self;
 ret_val = OPT_CO233462019;
 L1=ret_val;
 L2 = ATTR(L1,back_end);
 (*dBACK_1049564105[TAG(L2)])(L2);
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMI462748081(OPTIMIZE self, AM_ROUT_DEF func) {
 FLISTSTR L11;
 dAM_STMT am;
 AM_CURSOR c;
 FLISTSTR L21;
 dAM_STMT am11;
 OPTIMIZE prog_self;
 PROG ret_val;
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
 OPTIMIZE prog_self1;
 PROG ret_val7;
 OPTIMIZE prog_self2;
 PROG ret_val8;
 OPTIMIZE prog_self3;
 PROG ret_val9;
 OPTIMIZE prog_self4;
 PROG ret_val10;
 OPTIMIZE prog_self5;
 PROG ret_val11;
 OUT create_self1;
 OUT ret_val12;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val13;
 FILE1 stdout_self3;
 FILE1 ret_val14;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val15;
 FILE1 r4;
 OUT create_self2;
 OUT ret_val16;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val17;
 FILE1 stdout_self5;
 FILE1 ret_val18;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val19;
 FILE1 r6;
 OUT create_self3;
 OUT ret_val20;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val21;
 FILE1 r7;
 OUT create_self4;
 OUT ret_val22;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val23;
 FILE1 r8;
 OUT create_self5;
 OUT ret_val24;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val25;
 FILE1 r9;
 OPTIMIZE prog_self6;
 PROG ret_val26;
 OPTIMIZE cs_options_self;
 CS_OPTIONS ret_val27;
 OPTIMIZE cs_options_self1;
 CS_OPTIONS ret_val28;
 OPTIMIZE cs_options_self2;
 CS_OPTIONS ret_val29;
 OPTIMIZE cs_options_self3;
 CS_OPTIONS ret_val30;
 OPTIMIZE cs_options_self4;
 CS_OPTIONS ret_val31;
 OPTIMIZE cs_options_self5;
 CS_OPTIONS ret_val32;
 OPTIMIZE prog_self7;
 PROG ret_val33;
 OPTIMIZE prog_self8;
 PROG ret_val34;
 OPTIMIZE prog_self9;
 PROG ret_val35;
 OPTIMIZE prog_self10;
 PROG ret_val36;
 OPTIMIZE prog_self11;
 PROG ret_val37;
 OPTIMIZE prog_self12;
 PROG ret_val38;
 OUT create_self6;
 OUT ret_val39;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val40;
 FILE1 r10;
 OPTIMIZE cs_options_self6;
 CS_OPTIONS ret_val41;
 OPTIMIZE cs_options_self7;
 CS_OPTIONS ret_val42;
 OPTIMIZE prog_self13;
 PROG ret_val43;
 OPTIMIZE prog_self14;
 PROG ret_val44;
 OUT create_self7;
 OUT ret_val45;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val46;
 FILE1 r11;
 OPTIMIZE prog_self15;
 PROG ret_val47;
 OPTIMIZE prog_self16;
 PROG ret_val48;
 OPTIMIZE prog_self17;
 PROG ret_val49;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val50;
 OUT create_self8;
 OUT ret_val51;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val52;
 FILE1 r12;
 OUT create_self9;
 OUT ret_val53;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val54;
 FILE1 r13;
 OUT create_self10;
 OUT ret_val55;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val56;
 FILE1 stdout_self14;
 FILE1 ret_val57;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val58;
 FILE1 stdout_self15;
 FILE1 ret_val59;
 FILE1 r15;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val60;
 FILE1 r16;
 OPTIMIZE prog_self18;
 PROG ret_val61;
 PROG L3;
 extern STR workingon;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L12;
 PROG L14;
 BOOL L15;
 BOOL L161_;
 PROG L17;
 STR L18;
 STR L19;
 INT L20;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 PROG L25;
 extern STR Optimi440073291;
 FILE1 L26;
 OB L27;
 FILE1 L29;
 OB L30;
 BOOL L32;
 BOOL L331_;
 extern STR name60;
 FILE1 L34;
 OB L35;
 FILE1 L37;
 OB L38;
 extern STR name9;
 FILE1 L40;
 OB L41;
 extern STR nonoptimizedcode;
 FILE1 L43;
 OB L44;
 extern STR name32;
 FILE1 L46;
 OB L47;
 BOOL L49;
 CS_OPTIONS L50;
 CS_OPTIONS L51;
 BOOL L52;
 CS_OPTIONS L53;
 CS_OPTIONS L54;
 BOOL L55;
 BOOL L56;
 CS_OPTIONS L57;
 CS_OPTIONS L58;
 PROG L59;
 PROG L60;
 BOOL L62;
 BOOL L631_;
 PROG L64;
 STR L65;
 STR L66;
 INT L67;
 BOOL L681_;
 BOOL L69;
 BOOL L701_;
 PROG L71;
 extern STR starti1899675989;
 FILE1 L72;
 OB L73;
 BOOL L75;
 BOOL L76;
 CS_OPTIONS L77;
 CS_OPTIONS L78;
 PROG L79;
 PROG L80;
 extern STR locala767931605;
 FILE1 L81;
 OB L82;
 BOOL L84;
 PROG L85;
 BOOL L86;
 PROG L87;
 BOOL L88;
 BOOL L891_;
 PROG L90;
 STR L921_;
 extern STR AM;
 extern STR optimizedcode;
 FILE1 L93;
 OB L94;
 extern STR name32;
 FILE1 L96;
 OB L97;
 extern STR Optimi440073291;
 FILE1 L99;
 OB L100;
 FILE1 L102;
 OB L103;
 extern STR DONE;
 FILE1 L105;
 OB L106;
 PROG L108;
 dBACK_END L109;
 prog_self = self;
 ret_val = OPT_CO233462019;
 L3=ret_val;
 if (ATTR(L3,opt_verbose)) {
  create_self = ((OUT) NULL);
  ret_val1 = create_self;
  plus_self = ret_val1;
  plus_s = ((STR) &workingon);
  stdout_self = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r = L4;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  ret_val2 = plus_self;
  plus_self1 = ret_val2;
  plus_s1 = ATTR(ATTR(func,sig1),str);
  stdout_self1 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r1 = L7;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
  ret_val4 = plus_self1;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L10 = ((FILE1) L12);
  r2 = L10;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
 }
 prog_self1 = self;
 ret_val7 = OPT_CO233462019;
 L14=ret_val7;
 L15 = (ATTR(L14,opt_debug_func)==((FLISTSTR) NULL));
 L161_=!(L15); 
 if (L161_) {
  prog_self2 = self;
  ret_val8 = OPT_CO233462019;
  SATTR(ret_val8,opt_debug,FALSE);
  {
   struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
   prog_self3 = self;
   ret_val9 = OPT_CO233462019;
   L17=ret_val9;
   L11 = ATTR(L17,opt_debug_func);
   noname1->self = L11;
   noname1->state = 0;
   while (1) {
    L18 = ATTR(ATTR(func,sig1),str);
    L19 = FLISTS1171631134(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    L20 = STR_se2132153226(L18, L19);
    L221_=(L20)<(0); 
    L23 = L221_;
    L241_=!(L23); 
    if (L241_) {
     prog_self4 = self;
     ret_val10 = OPT_CO233462019;
     SATTR(ret_val10,opt_debug,TRUE);
     goto after_loop;
    }
   }
  }
  after_loop: ;
 }
 prog_self5 = self;
 ret_val11 = OPT_CO233462019;
 L25=ret_val11;
 if (ATTR(L25,opt_debug)) {
  create_self1 = ((OUT) NULL);
  ret_val12 = create_self1;
  plus_self3 = ret_val12;
  plus_s3 = ((STR) &Optimi440073291);
  stdout_self3 = ((FILE1) NULL);
  L27=ZALLOC(sizeof(struct FILE1_struct));
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=FILE1_tag;
  L26 = ((FILE1) L27);
  r3 = L26;
  
  SATTR(r3,fp,stdout);
  ret_val14 = r3;
  FILE_plus_STR(ret_val14, plus_s3);
  ret_val13 = plus_self3;
  plus_self4 = ret_val13;
  plus_s4 = ATTR(ATTR(func,sig1),str);
  stdout_self4 = ((FILE1) NULL);
  L30=ZALLOC(sizeof(struct FILE1_struct));
  if (L30==NULL) FATAL("Unable to allocate more memory");
  ((OB)L30)->header.tag=FILE1_tag;
  L29 = ((FILE1) L30);
  r4 = L29;
  
  SATTR(r4,fp,stdout);
  ret_val15 = r4;
  FILE_plus_STR(ret_val15, plus_s4);
  L32 = (ATTR(func,srcsig)==((SIG) NULL));
  L331_=!(L32); 
  if (L331_) {
   create_self2 = ((OUT) NULL);
   ret_val16 = create_self2;
   plus_self5 = ret_val16;
   plus_s5 = ((STR) &name60);
   stdout_self5 = ((FILE1) NULL);
   L35=ZALLOC(sizeof(struct FILE1_struct));
   if (L35==NULL) FATAL("Unable to allocate more memory");
   ((OB)L35)->header.tag=FILE1_tag;
   L34 = ((FILE1) L35);
   r5 = L34;
   
   SATTR(r5,fp,stdout);
   ret_val18 = r5;
   FILE_plus_STR(ret_val18, plus_s5);
   ret_val17 = plus_self5;
   plus_self6 = ret_val17;
   plus_s6 = ATTR(ATTR(func,srcsig),str);
   stdout_self6 = ((FILE1) NULL);
   L38=ZALLOC(sizeof(struct FILE1_struct));
   if (L38==NULL) FATAL("Unable to allocate more memory");
   ((OB)L38)->header.tag=FILE1_tag;
   L37 = ((FILE1) L38);
   r6 = L37;
   
   SATTR(r6,fp,stdout);
   ret_val19 = r6;
   FILE_plus_STR(ret_val19, plus_s6);
  }
  create_self3 = ((OUT) NULL);
  ret_val20 = create_self3;
  plus_self7 = ret_val20;
  plus_s7 = ((STR) &name9);
  stdout_self7 = ((FILE1) NULL);
  L41=ZALLOC(sizeof(struct FILE1_struct));
  if (L41==NULL) FATAL("Unable to allocate more memory");
  ((OB)L41)->header.tag=FILE1_tag;
  L40 = ((FILE1) L41);
  r7 = L40;
  
  SATTR(r7,fp,stdout);
  ret_val21 = r7;
  FILE_plus_STR(ret_val21, plus_s7);
  create_self4 = ((OUT) NULL);
  ret_val22 = create_self4;
  plus_self8 = ret_val22;
  plus_s8 = ((STR) &nonoptimizedcode);
  stdout_self8 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r8 = L43;
  
  SATTR(r8,fp,stdout);
  ret_val23 = r8;
  FILE_plus_STR(ret_val23, plus_s8);
  am = ATTR(func,code1);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am));
  create_self5 = ((OUT) NULL);
  ret_val24 = create_self5;
  plus_self9 = ret_val24;
  plus_s9 = ((STR) &name32);
  stdout_self9 = ((FILE1) NULL);
  L47=ZALLOC(sizeof(struct FILE1_struct));
  if (L47==NULL) FATAL("Unable to allocate more memory");
  ((OB)L47)->header.tag=FILE1_tag;
  L46 = ((FILE1) L47);
  r9 = L46;
  
  SATTR(r9,fp,stdout);
  ret_val25 = r9;
  FILE_plus_STR(ret_val25, plus_s9);
  prog_self6 = self;
  ret_val26 = OPT_CO233462019;
  c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val26, ((dAM) ATTR(func,code1)));
  SATTR(c,assign_in_order,TRUE);
  AM_CUR1754599450(c);
 }
 cs_options_self = self;
 ret_val27 = OPT_CO1299251581;
 L50=ret_val27;
 if (ATTR(L50,cse)) {
  cs_options_self1 = self;
  ret_val28 = OPT_CO1299251581;
  L51=ret_val28;
  L49 = ATTR(L51,side_effects);
 } else {
  L49 = FALSE;
 }
 if (L49) {
  OPT_CS641813603(((OPT_CSE) NULL), func);
 }
 OPTIMI671687564(self, func);
 cs_options_self2 = self;
 ret_val29 = OPT_CO1299251581;
 L53=ret_val29;
 if (ATTR(L53,cse)) {
  cs_options_self3 = self;
  ret_val30 = OPT_CO1299251581;
  L54=ret_val30;
  L52 = ATTR(L54,side_effects);
 } else {
  L52 = FALSE;
 }
 if (L52) {
  OPT_CS641813603(((OPT_CSE) NULL), func);
 }
 cs_options_self4 = self;
 ret_val31 = OPT_CO1299251581;
 L57=ret_val31;
 if (ATTR(L57,prefetch)) {
  cs_options_self5 = self;
  ret_val32 = OPT_CO1299251581;
  L58=ret_val32;
  L56 = ATTR(L58,side_effects);
 } else {
  L56 = FALSE;
 }
 if (L56) {
  prog_self7 = self;
  ret_val33 = OPT_CO233462019;
  L59=ret_val33;
  L55 = ATTR(L59,distributed);
 } else {
  L55 = FALSE;
 }
 if (L55) {
  OPT_PR1794418601(((OPT_PREFETCH) NULL), func);
 }
 prog_self8 = self;
 ret_val34 = OPT_CO233462019;
 L60=ret_val34;
 L62 = (ATTR(L60,opt_debug_func)==((FLISTSTR) NULL));
 L631_=!(L62); 
 if (L631_) {
  prog_self9 = self;
  ret_val35 = OPT_CO233462019;
  SATTR(ret_val35,opt_debug,FALSE);
  {
   struct FLISTS1171631134_frame_struct other2_0;
FLISTS1171631134_frame noname2 = &other2_0;
   prog_self10 = self;
   ret_val36 = OPT_CO233462019;
   L64=ret_val36;
   L21 = ATTR(L64,opt_debug_func);
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    L65 = ATTR(ATTR(func,sig1),str);
    L66 = FLISTS1171631134(noname2);
    if (noname2->state == -1) {
     goto after_loop1;
    }
    L67 = STR_se2132153226(L65, L66);
    L681_=(L67)<(0); 
    L69 = L681_;
    L701_=!(L69); 
    if (L701_) {
     prog_self11 = self;
     ret_val37 = OPT_CO233462019;
     SATTR(ret_val37,opt_debug,TRUE);
     goto after_loop1;
    }
   }
  }
  after_loop1: ;
 }
 prog_self12 = self;
 ret_val38 = OPT_CO233462019;
 L71=ret_val38;
 if (ATTR(L71,opt_debug)) {
  create_self6 = ((OUT) NULL);
  ret_val39 = create_self6;
  plus_self10 = ret_val39;
  plus_s10 = ((STR) &starti1899675989);
  stdout_self10 = ((FILE1) NULL);
  L73=ZALLOC(sizeof(struct FILE1_struct));
  if (L73==NULL) FATAL("Unable to allocate more memory");
  ((OB)L73)->header.tag=FILE1_tag;
  L72 = ((FILE1) L73);
  r10 = L72;
  
  SATTR(r10,fp,stdout);
  ret_val40 = r10;
  FILE_plus_STR(ret_val40, plus_s10);
 }
 cs_options_self6 = self;
 ret_val41 = OPT_CO1299251581;
 L77=ret_val41;
 if (ATTR(L77,local_opt)) {
  cs_options_self7 = self;
  ret_val42 = OPT_CO1299251581;
  L78=ret_val42;
  L76 = ATTR(L78,side_effects);
 } else {
  L76 = FALSE;
 }
 if (L76) {
  prog_self13 = self;
  ret_val43 = OPT_CO233462019;
  L79=ret_val43;
  L75 = ATTR(L79,distributed);
 } else {
  L75 = FALSE;
 }
 if (L75) {
  OPT_LO1015703310(((OPT_LOCAL) NULL), func);
 }
 prog_self14 = self;
 ret_val44 = OPT_CO233462019;
 L80=ret_val44;
 if (ATTR(L80,opt_debug)) {
  create_self7 = ((OUT) NULL);
  ret_val45 = create_self7;
  plus_self11 = ret_val45;
  plus_s11 = ((STR) &locala767931605);
  stdout_self11 = ((FILE1) NULL);
  L82=ZALLOC(sizeof(struct FILE1_struct));
  if (L82==NULL) FATAL("Unable to allocate more memory");
  ((OB)L82)->header.tag=FILE1_tag;
  L81 = ((FILE1) L82);
  r11 = L81;
  
  SATTR(r11,fp,stdout);
  ret_val46 = r11;
  FILE_plus_STR(ret_val46, plus_s11);
 }
 prog_self15 = self;
 ret_val47 = OPT_CO233462019;
 L85=ret_val47;
 if (ATTR(L85,opt_debug)) {
  L84 = TRUE;
 } else {
  prog_self16 = self;
  ret_val48 = OPT_CO233462019;
  L87=ret_val48;
  L88 = (ATTR(L87,opt_debug_func)==((FLISTSTR) NULL));
  L891_=!(L88); 
  if (L891_) {
   prog_self17 = self;
   ret_val49 = OPT_CO233462019;
   L90=ret_val49;
   aget_self = ATTR(L90,opt_debug_func);
   aget_ind = 0;
   L921_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val50 = L921_;
   L86 = STR_is111530248(ret_val50, ((STR) &AM));
  } else {
   L86 = FALSE;
  }
  L84 = L86;
 }
 if (L84) {
  create_self8 = ((OUT) NULL);
  ret_val51 = create_self8;
  plus_self12 = ret_val51;
  plus_s12 = ((STR) &optimizedcode);
  stdout_self12 = ((FILE1) NULL);
  L94=ZALLOC(sizeof(struct FILE1_struct));
  if (L94==NULL) FATAL("Unable to allocate more memory");
  ((OB)L94)->header.tag=FILE1_tag;
  L93 = ((FILE1) L94);
  r12 = L93;
  
  SATTR(r12,fp,stdout);
  ret_val52 = r12;
  FILE_plus_STR(ret_val52, plus_s12);
  am11 = ATTR(func,code1);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) am11));
  create_self9 = ((OUT) NULL);
  ret_val53 = create_self9;
  plus_self13 = ret_val53;
  plus_s13 = ((STR) &name32);
  stdout_self13 = ((FILE1) NULL);
  L97=ZALLOC(sizeof(struct FILE1_struct));
  if (L97==NULL) FATAL("Unable to allocate more memory");
  ((OB)L97)->header.tag=FILE1_tag;
  L96 = ((FILE1) L97);
  r13 = L96;
  
  SATTR(r13,fp,stdout);
  ret_val54 = r13;
  FILE_plus_STR(ret_val54, plus_s13);
  create_self10 = ((OUT) NULL);
  ret_val55 = create_self10;
  plus_self14 = ret_val55;
  plus_s14 = ((STR) &Optimi440073291);
  stdout_self14 = ((FILE1) NULL);
  L100=ZALLOC(sizeof(struct FILE1_struct));
  if (L100==NULL) FATAL("Unable to allocate more memory");
  ((OB)L100)->header.tag=FILE1_tag;
  L99 = ((FILE1) L100);
  r14 = L99;
  
  SATTR(r14,fp,stdout);
  ret_val57 = r14;
  FILE_plus_STR(ret_val57, plus_s14);
  ret_val56 = plus_self14;
  plus_self15 = ret_val56;
  plus_s15 = ATTR(ATTR(func,sig1),str);
  stdout_self15 = ((FILE1) NULL);
  L103=ZALLOC(sizeof(struct FILE1_struct));
  if (L103==NULL) FATAL("Unable to allocate more memory");
  ((OB)L103)->header.tag=FILE1_tag;
  L102 = ((FILE1) L103);
  r15 = L102;
  
  SATTR(r15,fp,stdout);
  ret_val59 = r15;
  FILE_plus_STR(ret_val59, plus_s15);
  ret_val58 = plus_self15;
  plus_self16 = ret_val58;
  plus_s16 = ((STR) &DONE);
  stdout_self16 = ((FILE1) NULL);
  L106=ZALLOC(sizeof(struct FILE1_struct));
  if (L106==NULL) FATAL("Unable to allocate more memory");
  ((OB)L106)->header.tag=FILE1_tag;
  L105 = ((FILE1) L106);
  r16 = L105;
  
  SATTR(r16,fp,stdout);
  ret_val60 = r16;
  FILE_plus_STR(ret_val60, plus_s16);
 }
 prog_self18 = self;
 ret_val61 = OPT_CO233462019;
 L108=ret_val61;
 L109 = ATTR(L108,back_end);
 (*dBACK_1132541334[TAG(L109)])(L109, func);
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMI671687564(OPTIMIZE self, AM_ROUT_DEF func) {
 BOOL opt_fork = BOOL_zero;
 BOOL has_locks = BOOL_zero;
 ARRAYBOOL after_loop_break;
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM am;
 SE_CONTEXT con;
 AM_LOCK_STMT l;
 AM_LOCK_STMT l1;
 AM_LOCK_STMT l2;
 AM_LOCK_STMT l3;
 SE_CONTEXT con1;
 AM_LOCK_STMT l4;
 SE_CONTEXT con2;
 dAM_STMT l5;
 AM_LOCK_STMT l6;
 AM_LOCK_STMT l7;
 dAM tam;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 OPTIMIZE cs_options_self;
 CS_OPTIONS ret_val;
 OPTIMIZE cs_options_self1;
 CS_OPTIONS ret_val1;
 OPTIMIZE prog_self;
 PROG ret_val2;
 ARRAYBOOL create_self;
 INT create_n = INT_zero;
 ARRAYBOOL ret_val3;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val4 = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 OPTIMIZE prog_self1;
 PROG ret_val7;
 OPTIMIZE prog_self2;
 PROG ret_val8;
 OPT_CONST const_hoisting_s;
 AM_ROUT_DEF const_hoisting_f;
 INT const_hoisting_p = INT_zero;
 BOOL d = BOOL_zero;
 OPTIMIZE prog_self3;
 PROG ret_val9;
 OPTIMIZE make_sure_emitte;
 SIG make_sure_emitte1;
 dBACK_END cgen;
 OPTIMIZE prog_self4;
 PROG ret_val10;
 CGEN make_sure_emitte2;
 SIG make_sure_emitte3;
 AM_ROUT_DEF ard;
 OPTIMIZE cs_options_self2;
 CS_OPTIONS ret_val11;
 OPTIMIZE cs_options_self3;
 CS_OPTIONS ret_val12;
 AM_HERE_EXPR create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_HERE_EXPR ret_val13;
 AM_HERE_EXPR r;
 OPTIMIZE prog_self5;
 PROG ret_val14;
 OPTIMIZE cs_options_self4;
 CS_OPTIONS ret_val15;
 OPTIMIZE prog_self6;
 PROG ret_val16;
 PROG psather_self;
 BOOL ret_val17 = BOOL_zero;
 OPTIMIZE cs_options_self5;
 CS_OPTIONS ret_val18;
 OPTIMIZE cs_options_self6;
 CS_OPTIONS ret_val19;
 OPTIMIZE prog_self7;
 PROG ret_val20;
 PROG psather_self1;
 BOOL ret_val21 = BOOL_zero;
 OPTIMIZE prog_self8;
 PROG ret_val22;
 OPTIMIZE prog_self9;
 PROG ret_val23;
 OPTIMIZE prog_self10;
 PROG ret_val24;
 OPTIMIZE prog_self11;
 PROG ret_val25;
 PROG psather_self2;
 BOOL ret_val26 = BOOL_zero;
 OPTIMIZE cs_options_self7;
 CS_OPTIONS ret_val27;
 OPTIMIZE cs_options_self8;
 CS_OPTIONS ret_val28;
 OPTIMIZE prog_self12;
 PROG ret_val29;
 OPTIMIZE prog_self13;
 PROG ret_val30;
 PROG psather_self3;
 BOOL ret_val31 = BOOL_zero;
 OPTIMIZE prog_self14;
 PROG ret_val32;
 OPTIMIZE cs_options_self9;
 CS_OPTIONS ret_val33;
 AM_ROUT_DEF is_iter_self3;
 BOOL ret_val34 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val35 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val36 = BOOL_zero;
 OPTIMIZE prog_self15;
 PROG ret_val37;
 OPTIMIZE prog_self16;
 PROG ret_val38;
 OPTIMIZE prog_self17;
 PROG ret_val39;
 OUT create_self2;
 OUT ret_val40;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val41;
 FILE1 r1;
 OPTIMIZE prog_self18;
 PROG ret_val42;
 OUT create_self3;
 OUT ret_val43;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val44;
 FILE1 r2;
 BOOL L4;
 BOOL L5;
 BOOL L6;
 CS_OPTIONS L7;
 CS_OPTIONS L8;
 SE_CONTEXT L9;
 BOOL L10;
 BOOL L121_;
 ARRAYBOOL L13;
 INT L14;
 OB L15;
 INT L16;
 BOOL L17;
 BOOL L18;
 BOOL L19;
 BOOL L201_;
 STR L22;
 INT L23;
 INT L241_;
 INT L25;
 CHAR L261_;
 CHAR L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 BOOL L32;
 PROG L33;
 PROG L34;
 dAM L35;
 PROG L36;
 BOOL L37;
 BOOL L381_;
 BOOL L39;
 BOOL L40;
 BOOL L41;
 BOOL L42;
 CS_OPTIONS L43;
 CS_OPTIONS L44;
 BOOL L45;
 dAM_EXPR L46;
 INT L471_;
 INT L48;
 AM_HERE_EXPR L49;
 OB L50;
 AM_HERE_EXPR L51;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L56;
 BOOL L571_;
 BOOL L58;
 BOOL L591_;
 CS_OPTIONS L60;
 INT L61;
 INT L621_;
 INT L63;
 BOOL L65;
 BOOL L661_;
 INT L67;
 BOOL L69;
 BOOL L701_;
 INT L71;
 AM_LOOP_STMT L73;
 INT L74;
 INT L751_;
 INT L76;
 INT L78;
 BOOL L80;
 BOOL L81;
 BOOL L82;
 CS_OPTIONS L83;
 BOOL L84;
 BOOL L851_;
 BOOL L86;
 BOOL L87;
 CS_OPTIONS L88;
 BOOL L89;
 BOOL L90;
 BOOL L911_;
 BOOL L92;
 BOOL L931_;
 INT L94;
 BOOL L95;
 BOOL L96;
 INT L97;
 BOOL L981_;
 BOOL L99;
 BOOL L1001_;
 BOOL L101;
 BOOL L102;
 BOOL L1031_;
 SE_CONTEXT L104;
 BOOL L105;
 BOOL L107;
 BOOL L108;
 BOOL L109;
 CS_OPTIONS L110;
 BOOL L111;
 BOOL L1121_;
 CS_OPTIONS L113;
 BOOL L114;
 BOOL L115;
 BOOL L116;
 BOOL L1171_;
 INT L118;
 PROG L120;
 BOOL L122;
 BOOL L1231_;
 CS_OPTIONS L124;
 BOOL L125;
 BOOL L1261_;
 BOOL L127;
 BOOL L128;
 BOOL L129;
 BOOL L1301_;
 STR L131;
 INT L132;
 INT L1331_;
 INT L134;
 CHAR L1351_;
 CHAR L136;
 BOOL L1371_;
 BOOL L138;
 BOOL L1391_;
 BOOL L140;
 PROG L141;
 PROG L142;
 INT L1431_,L1431_m;
 PROG L144;
 extern STR HOISTING;
 FILE1 L145;
 OB L146;
 INT L148;
 PROG L149;
 extern STR AFTERHOISTING;
 FILE1 L150;
 OB L151;
 if (ATTR(ATTR(func,sig1),is_par_routine)) {
  cs_options_self = self;
  ret_val = OPT_CO1299251581;
  L7=ret_val;
  L6 = ATTR(L7,parloops);
 } else {
  L6 = FALSE;
 }
 if (L6) {
  cs_options_self1 = self;
  ret_val1 = OPT_CO1299251581;
  L8=ret_val1;
  L5 = ATTR(L8,side_effects);
 } else {
  L5 = FALSE;
 }
 if (L5) {
  prog_self = self;
  ret_val2 = OPT_CO233462019;
  L9=SIG_ge642497605(ATTR(func,sig1), ret_val2);
  L10 = ATTR(L9,may_block);
  L121_=!(L10); 
  L4 = L121_;
 } else {
  L4 = FALSE;
 }
 opt_fork = L4;
 has_locks = FALSE;
 create_self = ((ARRAYBOOL) NULL);
 create_n = 30;
 L14 = create_n;
 L16=(sizeof(struct ARRAYBOOL_struct)+1-sizeof(BOOL))+(L14)*sizeof(BOOL);
 L15=ZALLOC_LEAF_BIG(L16);
 if (L15==NULL) FATAL("Unable to allocate more memory");
 memset(L15,0,L16);
 ((OB)L15)->header.tag=ARRAYBOOL_tag;
#ifdef DESTROY_CHK

   ((OB)L15)->header.destroyed=0;
#endif

 L13 = ((ARRAYBOOL) L15);
 L13->asize = L14;
 ret_val3 = L13;
 after_loop_break = ret_val3;
 is_iter_self = func;
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L19 = (is_iter_self2.str==((STR) NULL));
 L201_=!(L19); 
 if (L201_) {
  L22 = is_iter_self2.str;
  L23 = STR_sizerINT(is_iter_self2.str);
  L241_=INTMINUS(L23,1); 
  L25 = L241_;
  L261_=ARR((STR)L22,L25); 
  L27 = L261_;
  L281_=L27=='!'; 
  L18 = L281_;
 } else {
  L18 = FALSE;
 }
 ret_val6 = L18;
 ret_val5 = ret_val6;
 ret_val4 = ret_val5;
 L29 = ret_val4;
 L301_=!(L29); 
 if (L301_) {
  prog_self1 = self;
  ret_val7 = OPT_CO233462019;
  L33=ret_val7;
  if (ATTR(L33,hoist_const)) {
   L32 = TRUE;
  } else {
   prog_self2 = self;
   ret_val8 = OPT_CO233462019;
   L34=ret_val8;
   L32 = ATTR(L34,move_while);
  }
  L17 = L32;
 } else {
  L17 = FALSE;
 }
 if (L17) {
  const_hoisting_s = ((OPT_CONST) NULL);
  const_hoisting_f = func;
  const_hoisting_p = 1;
  d = OPT_CO1865909003(const_hoisting_s, const_hoisting_f, const_hoisting_p);
 }
 prog_self3 = self;
 ret_val9 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val9, ((dAM) ATTR(func,code1)));
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L35 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L35;
   if (am==NULL) {
    goto other320;
   } else
   switch (TAG(am)) {
    case AM_PRE_STMT_tag:
     OPTIMI683370008(self, ATTR(((AM_PRE_STMT) am),test1)); break;
    case AM_POST_STMT_tag:
     OPTIMI683370008(self, ATTR(((AM_POST_STMT) am),test1)); break;
    case AM_ASSERT_STMT_tag:
     OPTIMI683370008(self, ATTR(((AM_ASSERT_STMT) am),test1)); break;
    case AM_ATTACH_STMT_tag:
     make_sure_emitte = self;
     make_sure_emitte1 = ATTR(((AM_ATTACH_STMT) am),rout);
     prog_self4 = make_sure_emitte;
     ret_val10 = OPT_CO233462019;
     L36=ret_val10;
     cgen = ATTR(L36,back_end);
     switch (TAG(cgen)) {
      case CGEN_tag:
       make_sure_emitte2 = ((CGEN) cgen);
       make_sure_emitte3 = make_sure_emitte1;
       ard = FMAPSI2016001247(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3);
       L37 = (ard==((AM_ROUT_DEF) NULL));
       L381_=!(L37); 
       if (L381_) {
        SATTR(make_sure_emitte2,leftovers,FSETAM2044524049(ATTR(make_sure_emitte2,leftovers), ard));
        SATTR(make_sure_emitte2,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3));
       } break;
      default: ;
       FATAL("No applicable type in typecase\nin OPTIMIZE::do_loop_opt(AM_ROUT_DEF)\n./Back/optimize.sa:159:14");
     }
     if (opt_fork) {
      cs_options_self2 = self;
      ret_val11 = OPT_CO1299251581;
      L43=ret_val11;
      L42 = ATTR(L43,side_effects);
     } else {
      L42 = FALSE;
     }
     if (L42) {
      cs_options_self3 = self;
      ret_val12 = OPT_CO1299251581;
      L44=ret_val12;
      L41 = ATTR(L44,parloops);
     } else {
      L41 = FALSE;
     }
     if (L41) {
      L40 = ATTR(ATTR(((AM_ATTACH_STMT) am),rout),is_fork_routine);
     } else {
      L40 = FALSE;
     }
     if (L40) {
      if ((ATTR(((AM_ATTACH_STMT) am),at)==((dAM_EXPR) NULL))) {
       L45 = TRUE;
      } else {
       L46 = ATTR(((AM_ATTACH_STMT) am),at);
       L471_=SYSID(L46); 
       L48 = L471_;
       create_self1 = ((AM_HERE_EXPR) NULL);
       create_source = SFILE_ID_zero;
       L50=ZALLOC(sizeof(struct AM_HERE_EXPR_struct));
       if (L50==NULL) FATAL("Unable to allocate more memory");
       ((OB)L50)->header.tag=AM_HERE_EXPR_tag;
       L49 = ((AM_HERE_EXPR) L50);
       r = L49;
       SATTR(r,source1,create_source);
       ret_val13 = r;
       L51 = ret_val13;
       L521_=SYSID(L51); 
       L53 = L521_;
       L541_=(L48)==(L53); 
       L45 = L541_;
      }
      L39 = L45;
     } else {
      L39 = FALSE;
     }
     if (L39) {
      prog_self5 = self;
      ret_val14 = OPT_CO233462019;
      con = SIG_ge642497605(ATTR(((AM_ATTACH_STMT) am),rout), ret_val14);
      L56 = ATTR(con,may_block);
      L571_=!(L56); 
      if (L571_) {
       L58 = ATTR(con,forks);
       L591_=!(L58); 
       L55 = L591_;
      } else {
       L55 = FALSE;
      }
      SATTR(ATTR(((AM_ATTACH_STMT) am),rout),opt_fork,L55);
      opt_fork = ATTR(ATTR(((AM_ATTACH_STMT) am),rout),opt_fork);
     }
     else {
      opt_fork = FALSE;
     } break;
    case AM_LOOP_STMT_tag:
     cs_options_self4 = self;
     ret_val15 = OPT_CO1299251581;
     L60=ret_val15;
     SATTR(((AM_LOOP_STMT) am),no_begin_loop,ATTR(L60,side_effects));
     L61 = ATTR(c,loops);
     L621_=INTPLUS(L61,1); 
     L63 = L621_;
     SARR((ARRAYBOOL)after_loop_break,L63,FALSE); 
     ; break;
    case AM_YIELD_STMT_tag:
     if (has_locks) {
      l = AM_CUR201931527(c);
      L65 = (l==((AM_LOCK_STMT) NULL));
      L661_=!(L65); 
      if (L661_) {
       SATTR(l,manual_unlock,FALSE);
      }
     }
     L67 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L67,TRUE); 
     ; break;
    case AM_RAISE_STMT_tag:
     if (has_locks) {
      l1 = AM_CUR201931527(c);
      L69 = (l1==((AM_LOCK_STMT) NULL));
      L701_=!(L69); 
      if (L701_) {
       SATTR(l1,manual_unlock,FALSE);
      }
     }
     L71 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L71,TRUE); 
     ; break;
    case AM_BREAK_STMT_tag:
     L73=AM_CUR153953493(c);
     L74 = ATTR(L73,breaks);
     L751_=INTPLUS(L74,1); 
     SATTR(AM_CUR153953493(c),breaks,L751_);
     L76 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L76,TRUE); 
     ; break;
    case AM_RETURN_STMT_tag:
     L78 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L78,TRUE); 
     ; break;
    case AM_BND260301329_tag:
     prog_self6 = self;
     ret_val16 = OPT_CO233462019;
     psather_self = ret_val16;
     if (ATTR(psather_self,threads)) {
      L82 = TRUE;
     } else {
      L82 = ATTR(psather_self,distributed);
     }
     ret_val17 = L82;
     if (ret_val17) {
      L81 = has_locks;
     } else {
      L81 = FALSE;
     }
     if (L81) {
      cs_options_self5 = self;
      ret_val18 = OPT_CO1299251581;
      L83=ret_val18;
      L80 = ATTR(L83,side_effects);
     } else {
      L80 = FALSE;
     }
     if (L80) {
      l2 = AM_CUR201931527(c);
      L84 = (l2==((AM_LOCK_STMT) NULL));
      L851_=!(L84); 
      if (L851_) {
       SATTR(l2,manual_unlock,FALSE);
      }
     } break;
    case AM_ROU1916046290_tag:
     cs_options_self6 = self;
     ret_val19 = OPT_CO1299251581;
     L88=ret_val19;
     if (ATTR(L88,side_effects)) {
      prog_self7 = self;
      ret_val20 = OPT_CO233462019;
      psather_self1 = ret_val20;
      if (ATTR(psather_self1,threads)) {
       L89 = TRUE;
      } else {
       L89 = ATTR(psather_self1,distributed);
      }
      ret_val21 = L89;
      L87 = ret_val21;
     } else {
      L87 = FALSE;
     }
     if (L87) {
      L86 = has_locks;
     } else {
      L86 = FALSE;
     }
     if (L86) {
      l3 = AM_CUR201931527(c);
      L90 = (l3==((AM_LOCK_STMT) NULL));
      L911_=!(L90); 
      if (L911_) {
       prog_self8 = self;
       ret_val22 = OPT_CO233462019;
       con1 = SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val22);
       if (ATTR(con1,has_raise)) {
        SATTR(l3,manual_unlock,FALSE);
       }
      }
     }
     L92 = OPTIMI1660661857(self, func, ((AM_ROU1916046290) am), c);
     L931_=!(L92); 
     if (L931_) {
      L94 = ATTR(c,loops);
      L97 = ATTR(c,loops);
      L981_=ARR((ARRAYBOOL)after_loop_break,L97); 
      if (L981_) {
       L96 = TRUE;
      } else {
       L99 = ATTR(c,with_side_effect);
       L1001_=!(L99); 
       L96 = L1001_;
      }
      if (L96) {
       L95 = TRUE;
      } else {
       prog_self9 = self;
       ret_val23 = OPT_CO233462019;
       L102 = (SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val23)==((SE_CONTEXT) NULL));
       L1031_=!(L102); 
       if (L1031_) {
        prog_self10 = self;
        ret_val24 = OPT_CO233462019;
        L104=SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val24);
        L101 = ATTR(L104,has_raise);
       } else {
        L101 = FALSE;
       }
       L95 = L101;
      }
      L105 = L95;
      SARR((ARRAYBOOL)after_loop_break,L94,L105); 
      ;
     } break;
    case AM_BND367211769_tag:
     prog_self11 = self;
     ret_val25 = OPT_CO233462019;
     psather_self2 = ret_val25;
     if (ATTR(psather_self2,threads)) {
      L109 = TRUE;
     } else {
      L109 = ATTR(psather_self2,distributed);
     }
     ret_val26 = L109;
     if (ret_val26) {
      L108 = has_locks;
     } else {
      L108 = FALSE;
     }
     if (L108) {
      cs_options_self7 = self;
      ret_val27 = OPT_CO1299251581;
      L110=ret_val27;
      L107 = ATTR(L110,side_effects);
     } else {
      L107 = FALSE;
     }
     if (L107) {
      l4 = AM_CUR201931527(c);
      L111 = (l4==((AM_LOCK_STMT) NULL));
      L1121_=!(L111); 
      if (L1121_) {
       SATTR(l4,manual_unlock,FALSE);
      }
     }
     SATTR(ATTR(((AM_BND367211769) am),lp),no_begin_loop,FALSE); break;
    case AM_ITE1809135850_tag:
     cs_options_self8 = self;
     ret_val28 = OPT_CO1299251581;
     L113=ret_val28;
     if (ATTR(L113,side_effects)) {
      prog_self12 = self;
      ret_val29 = OPT_CO233462019;
      con2 = SIG_ge642497605(ATTR(((AM_ITE1809135850) am),fun), ret_val29);
      prog_self13 = self;
      ret_val30 = OPT_CO233462019;
      psather_self3 = ret_val30;
      if (ATTR(psather_self3,threads)) {
       L115 = TRUE;
      } else {
       L115 = ATTR(psather_self3,distributed);
      }
      ret_val31 = L115;
      if (ret_val31) {
       L114 = has_locks;
      } else {
       L114 = FALSE;
      }
      if (L114) {
       l5 = AM_CUR643033494(c);
       L116 = (l5==((dAM_STMT) NULL));
       L1171_=!(L116); 
       if (L1171_) {
        if (l5==NULL) {
         goto other321;
        } else
        switch (TAG(l5)) {
         case AM_LOCK_STMT_tag:
          if (ATTR(con2,has_raise)) {
           SATTR(((AM_LOCK_STMT) l5),manual_unlock,FALSE);
          } break;
         default: ;
         other321: ;
        }
       }
      }
      if (ATTR(con2,has_yield_in_loc)) {
       SATTR(ATTR(((AM_ITE1809135850) am),lp),no_begin_loop,FALSE);
      }
     }
     OPT_IT505798087(((OPT_ITER) NULL), func, ((AM_ITE1809135850) am), c, after_loop_break);
     L118 = ATTR(c,loops);
     SARR((ARRAYBOOL)after_loop_break,L118,TRUE); 
     ; break;
    case AM_LOCK_STMT_tag:
     prog_self14 = self;
     ret_val32 = OPT_CO233462019;
     L120=ret_val32;
     if (ATTR(L120,locks_on_stack)) {
      if (has_locks) {
       l6 = AM_CUR201931527(c);
       L122 = (l6==((AM_LOCK_STMT) NULL));
       L1231_=!(L122); 
       if (L1231_) {
        SATTR(l6,manual_unlock,FALSE);
       }
      }
      cs_options_self9 = self;
      ret_val33 = OPT_CO1299251581;
      L124=ret_val33;
      SATTR(((AM_LOCK_STMT) am),manual_unlock,ATTR(L124,side_effects));
      has_locks = TRUE;
     } break;
    case AM_UNLOCK_STMT_tag:
     l7 = AM_CUR201931527(c);
     SATTR(l7,manual_unlock,FALSE); break;
    default: ;
    other320: ;
   }
  }
 }
 after_loop: ;
 SATTR(ATTR(func,sig1),opt_fork,opt_fork);
 L125 = (ATTR(c,top1)==((dAM) NULL));
 L1261_=!(L125); 
 if (L1261_) {
  tam = ATTR(c,top1);
  switch (TAG(tam)) {
   case AM_ASSERT_STMT_tag:
   case AM_ASSIGN_STMT_tag:
   case AM_ATTACH_STMT_tag:
   case AM_BREAK_STMT_tag:
   case AM_CASE_STMT_tag:
   case AM_COMMENT_STMT_tag:
   case AM_EXPR_STMT_tag:
   case AM_FORK_STMT_tag:
   case AM_IF_STMT_tag:
   case AM_INITIAL_STMT_tag:
   case AM_INV1167837230_tag:
   case AM_LOCK_STMT_tag:
   case AM_LOOP_STMT_tag:
   case AM_PAR_STMT_tag:
   case AM_POST_STMT_tag:
   case AM_PREFETCH_STMT_tag:
   case AM_PRE_STMT_tag:
   case AM_PROTECT_STMT_tag:
   case AM_RAISE_STMT_tag:
   case AM_RETURN_STMT_tag:
   case AM_SYNC_STMT_tag:
   case AM_TYPECASE_STMT_tag:
   case AM_UNLOCK_STMT_tag:
   case AM_WAITFOR_STMT_tag:
   case AM_WIT1996971603_tag:
   case AM_YIELD_STMT_tag:
    SATTR(func,code1,((dAM_STMT) tam)); break;
   default: ;
    FATAL("No applicable type in typecase\nin OPTIMIZE::do_loop_opt(AM_ROUT_DEF)\n./Back/optimize.sa:472:17");
  }
 }
 is_iter_self3 = func;
 is_iter_self4 = ATTR(is_iter_self3,sig1);
 is_iter_self5 = ATTR(is_iter_self4,name1);
 L129 = (is_iter_self5.str==((STR) NULL));
 L1301_=!(L129); 
 if (L1301_) {
  L131 = is_iter_self5.str;
  L132 = STR_sizerINT(is_iter_self5.str);
  L1331_=INTMINUS(L132,1); 
  L134 = L1331_;
  L1351_=ARR((STR)L131,L134); 
  L136 = L1351_;
  L1371_=L136=='!'; 
  L128 = L1371_;
 } else {
  L128 = FALSE;
 }
 ret_val36 = L128;
 ret_val35 = ret_val36;
 ret_val34 = ret_val35;
 L138 = ret_val34;
 L1391_=!(L138); 
 if (L1391_) {
  prog_self15 = self;
  ret_val37 = OPT_CO233462019;
  L141=ret_val37;
  if (ATTR(L141,hoist_const)) {
   L140 = TRUE;
  } else {
   prog_self16 = self;
   ret_val38 = OPT_CO233462019;
   L142=ret_val38;
   L140 = ATTR(L142,move_while);
  }
  L127 = L140;
 } else {
  L127 = FALSE;
 }
 if (L127) {
  {
   BOOL f_L1431_ = TRUE;
   L21 = 2;
   L31 = 10;
   L1431_=L21-1;L1431_m=L31; 
   while (1) {
    prog_self17 = self;
    ret_val39 = OPT_CO233462019;
    L144=ret_val39;
    if (ATTR(L144,opt_debug)) {
     create_self2 = ((OUT) NULL);
     ret_val40 = create_self2;
     plus_self = ret_val40;
     plus_s = ((STR) &HOISTING);
     stdout_self = ((FILE1) NULL);
     L146=ZALLOC(sizeof(struct FILE1_struct));
     if (L146==NULL) FATAL("Unable to allocate more memory");
     ((OB)L146)->header.tag=FILE1_tag;
     L145 = ((FILE1) L146);
     r1 = L145;
     
     SATTR(r1,fp,stdout);
     ret_val41 = r1;
     FILE_plus_STR(ret_val41, plus_s);
    }
    if(L1431_++>=L1431_m)  goto after_loop1; 
    if (OPT_CO1865909003(((OPT_CONST) NULL), func, L1431_)) {
    }
    else {
     goto after_loop1;
    }
    prog_self18 = self;
    ret_val42 = OPT_CO233462019;
    L149=ret_val42;
    if (ATTR(L149,opt_debug)) {
     create_self3 = ((OUT) NULL);
     ret_val43 = create_self3;
     plus_self1 = ret_val43;
     plus_s1 = ((STR) &AFTERHOISTING);
     stdout_self1 = ((FILE1) NULL);
     L151=ZALLOC(sizeof(struct FILE1_struct));
     if (L151==NULL) FATAL("Unable to allocate more memory");
     ((OB)L151)->header.tag=FILE1_tag;
     L150 = ((FILE1) L151);
     r2 = L150;
     
     SATTR(r2,fp,stdout);
     ret_val44 = r2;
     FILE_plus_STR(ret_val44, plus_s1);
     AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(func,code1)));
    }
   }
  }
  after_loop1: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMI683370008(OPTIMIZE self, dAM_EXPR am) {
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM a;
 OPTIMIZE prog_self;
 PROG ret_val;
 OPTIMIZE make_sure_emitte;
 SIG make_sure_emitte1;
 dBACK_END cgen;
 OPTIMIZE prog_self1;
 PROG ret_val1;
 CGEN make_sure_emitte2;
 SIG make_sure_emitte3;
 AM_ROUT_DEF ard;
 OPTIMIZE make_sure_emitte4;
 SIG make_sure_emitte5;
 dBACK_END cgen1;
 OPTIMIZE prog_self2;
 PROG ret_val2;
 CGEN make_sure_emitte6;
 SIG make_sure_emitte7;
 AM_ROUT_DEF ard1;
 dAM L2;
 PROG L3;
 BOOL L4;
 BOOL L51_;
 PROG L6;
 BOOL L7;
 BOOL L81_;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, ((dAM) am));
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   a = L2;
   if (a==NULL) {
    goto other286;
   } else
   switch (TAG(a)) {
    case AM_ITE1809135850_tag:
     make_sure_emitte = self;
     make_sure_emitte1 = ATTR(((AM_ITE1809135850) a),fun);
     prog_self1 = make_sure_emitte;
     ret_val1 = OPT_CO233462019;
     L3=ret_val1;
     cgen = ATTR(L3,back_end);
     switch (TAG(cgen)) {
      case CGEN_tag:
       make_sure_emitte2 = ((CGEN) cgen);
       make_sure_emitte3 = make_sure_emitte1;
       ard = FMAPSI2016001247(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3);
       L4 = (ard==((AM_ROUT_DEF) NULL));
       L51_=!(L4); 
       if (L51_) {
        SATTR(make_sure_emitte2,leftovers,FSETAM2044524049(ATTR(make_sure_emitte2,leftovers), ard));
        SATTR(make_sure_emitte2,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte2,not_emitted), make_sure_emitte3));
       } break;
      default: ;
       FATAL("No applicable type in typecase\nin OPTIMIZE::check_leftover_functions($AM_EXPR)\n./Back/optimize.sa:159:14");
     } break;
    case AM_ROU1916046290_tag:
     make_sure_emitte4 = self;
     make_sure_emitte5 = ATTR(((AM_ROU1916046290) a),fun);
     prog_self2 = make_sure_emitte4;
     ret_val2 = OPT_CO233462019;
     L6=ret_val2;
     cgen1 = ATTR(L6,back_end);
     switch (TAG(cgen1)) {
      case CGEN_tag:
       make_sure_emitte6 = ((CGEN) cgen1);
       make_sure_emitte7 = make_sure_emitte5;
       ard1 = FMAPSI2016001247(ATTR(make_sure_emitte6,not_emitted), make_sure_emitte7);
       L7 = (ard1==((AM_ROUT_DEF) NULL));
       L81_=!(L7); 
       if (L81_) {
        SATTR(make_sure_emitte6,leftovers,FSETAM2044524049(ATTR(make_sure_emitte6,leftovers), ard1));
        SATTR(make_sure_emitte6,not_emitted,FMAPSI1625125906(ATTR(make_sure_emitte6,not_emitted), make_sure_emitte7));
       } break;
      default: ;
       FATAL("No applicable type in typecase\nin OPTIMIZE::check_leftover_functions($AM_EXPR)\n./Back/optimize.sa:159:14");
     } break;
    default: ;
    other286: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPTIMIZE_init(OPTIMIZE self) {
 dGET_OPTIONS temp11;
 OPTIMIZE prog_self;
 PROG ret_val;
 OPTIMIZE prog_self1;
 PROG ret_val1;
 PROG L1;
 dBACK_END L2;
 PROG L3;
 prog_self = self;
 ret_val = OPT_CO233462019;
 L1=ret_val;
 L2 = ATTR(L1,back_end);
 (*dBACK_END_init[TAG(L2)])(L2);
 prog_self1 = self;
 ret_val1 = OPT_CO233462019;
 L3=ret_val1;
 temp11 = ATTR(L3,get_options);
 switch (TAG(temp11)) {
  case CS_OPTIONS_tag:
   OPT_CO1299251581 = ((CS_OPTIONS) temp11); break;
  default: ;
   FATAL("No applicable type in typecase\nin OPTIMIZE::init\n./Back/optimize.sa:329:14");
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPT_PR1134320793(OPT_PREFETCH self, AM_ROUT_DEF func) {
 AM_CURSOR c;
 FSETAM1158405984 m = ((FSETAM1158405984) NULL);
 BOOL error1 = BOOL_zero;
 AM_CURSOR L11;
 dAM t;
 OPT_PREFETCH prog_self;
 PROG ret_val;
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
 dAM L2;
 BOOL L3;
 BOOL L41_;
 extern STR waitfo829569008;
 FILE1 L5;
 OB L6;
 FILE1 L8;
 OB L9;
 extern STR name9;
 FILE1 L12;
 OB L13;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, ((dAM) ATTR(func,code1)));
 SATTR(c,assign_in_order,TRUE);
 error1 = FALSE;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   t = L2;
   if (t==NULL) {
    goto other335;
   } else
   switch (TAG(t)) {
    case AM_PREFETCH_STMT_tag:
     m = FSETAM83361925(m, ATTR(((AM_PREFETCH_STMT) t),prefetch)); break;
    case AM_WAITFOR_STMT_tag:
     L3 = FSETAM215631726(m, ATTR(((AM_WAITFOR_STMT) t),prefetch));
     L41_=!(L3); 
     if (L41_) {
      create_self = ((OUT) NULL);
      ret_val1 = create_self;
      plus_self = ret_val1;
      plus_s = ((STR) &waitfo829569008);
      stdout_self = ((FILE1) NULL);
      L6=ZALLOC(sizeof(struct FILE1_struct));
      if (L6==NULL) FATAL("Unable to allocate more memory");
      ((OB)L6)->header.tag=FILE1_tag;
      L5 = ((FILE1) L6);
      r = L5;
      
      SATTR(r,fp,stdout);
      ret_val3 = r;
      FILE_plus_STR(ret_val3, plus_s);
      ret_val2 = plus_self;
      plus_self1 = ret_val2;
      plus_s1 = ATTR(ATTR(func,sig1),str);
      stdout_self1 = ((FILE1) NULL);
      L9=ZALLOC(sizeof(struct FILE1_struct));
      if (L9==NULL) FATAL("Unable to allocate more memory");
      ((OB)L9)->header.tag=FILE1_tag;
      L8 = ((FILE1) L9);
      r1 = L8;
      
      SATTR(r1,fp,stdout);
      ret_val5 = r1;
      FILE_plus_STR(ret_val5, plus_s1);
      ret_val4 = plus_self1;
      plus_self2 = ret_val4;
      plus_s2 = ((STR) &name9);
      stdout_self2 = ((FILE1) NULL);
      L13=ZALLOC(sizeof(struct FILE1_struct));
      if (L13==NULL) FATAL("Unable to allocate more memory");
      ((OB)L13)->header.tag=FILE1_tag;
      L12 = ((FILE1) L13);
      r2 = L12;
      
      SATTR(r2,fp,stdout);
      ret_val6 = r2;
      FILE_plus_STR(ret_val6, plus_s2);
      AM_OUT1439666796(((AM_OUT) NULL), t);
      error1 = TRUE;
     }
     m = FSETAM483153547(m, ATTR(((AM_WAITFOR_STMT) t),prefetch)); break;
    default: ;
    other335: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_PR1794418601(OPT_PREFETCH self, AM_ROUT_DEF func) {
 BOOL prefetch_done = BOOL_zero;
 AM_CURSOR c;
 BOOL specul = BOOL_zero;
 BOOL loopp = BOOL_zero;
 AM_CURSOR L11;
 dAM am;
 AM_PREFETCH_STMT p;
 dAM_EXPR t;
 AM_WAITFOR_STMT w;
 AM_STMT_EXPR e;
 dAM tam;
 dAM tmp1;
 OPT_PREFETCH cs_options_self;
 CS_OPTIONS ret_val;
 OPT_PREFETCH prog_self;
 PROG ret_val1;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 OPT_PREFETCH prog_self1;
 PROG ret_val6;
 OPT_PREFETCH cs_options_self1;
 CS_OPTIONS ret_val7;
 OPT_PREFETCH cs_options_self2;
 CS_OPTIONS ret_val8;
 SIG is_iter_self;
 BOOL ret_val9 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val10 = BOOL_zero;
 OPT_PREFETCH prog_self2;
 PROG ret_val11;
 OPT_PREFETCH prog_self3;
 PROG ret_val12;
 OPT_PREFETCH prog_self4;
 PROG ret_val13;
 OUT create_self2;
 OUT ret_val14;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val15;
 FILE1 stdout_self2;
 FILE1 ret_val16;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val17;
 FILE1 stdout_self3;
 FILE1 ret_val18;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val19;
 FILE1 r4;
 OPT_PREFETCH prog_self5;
 PROG ret_val20;
 SIG is_iter_self2;
 BOOL ret_val21 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val22 = BOOL_zero;
 AM_PREFETCH_STMT create_self3;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_PREFETCH_STMT ret_val23;
 AM_PREFETCH_STMT r5;
 AM_ATTR_EXPR tp_self;
 dTP ret_val24;
 AM_LOCAL_EXPR create_self4;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val25;
 AM_LOCAL_EXPR r6;
 AM_LOCAL_EXPR create_self5;
 SFILE_ID create_src1 = SFILE_ID_zero;
 IDENT create_name1 = IDENT_zero;
 dTP create_tp1;
 AM_LOCAL_EXPR ret_val26;
 AM_LOCAL_EXPR r7;
 STR plus_self5;
 STR plus_sarg;
 STR ret_val27;
 OPT_PREFETCH cs_options_self3;
 CS_OPTIONS ret_val28;
 AM_CURSOR insert_after_mar;
 dAM_STMT insert_after_mar1;
 INT insert_after_mar2 = INT_zero;
 BOOL ret_val29 = BOOL_zero;
 OPT_PREFETCH prog_self6;
 PROG ret_val30;
 OPT_PREFETCH prog_self7;
 PROG ret_val31;
 OUT create_self6;
 OUT ret_val32;
 OUT plus_self6;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val33;
 FILE1 r8;
 OUT create_self7;
 OUT ret_val34;
 OUT plus_self7;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val35;
 FILE1 r9;
 AM_LOCAL_EXPR create_self8;
 SFILE_ID create_src2 = SFILE_ID_zero;
 IDENT create_name2 = IDENT_zero;
 dTP create_tp2;
 AM_LOCAL_EXPR ret_val36;
 AM_LOCAL_EXPR r10;
 OPT_PREFETCH prog_self8;
 PROG ret_val37;
 OUT create_self9;
 OUT ret_val38;
 OUT plus_self8;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val39;
 FILE1 r11;
 OUT create_self10;
 OUT ret_val40;
 OUT plus_self9;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val41;
 FILE1 r12;
 AM_WAITFOR_STMT create_self11;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_WAITFOR_STMT ret_val42;
 AM_WAITFOR_STMT r13;
 AM_STMT_EXPR create_self12;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_STMT_EXPR ret_val43;
 AM_STMT_EXPR r14;
 OPT_PREFETCH prog_self9;
 PROG ret_val44;
 OUT create_self13;
 OUT ret_val45;
 OUT plus_self10;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val46;
 FILE1 r15;
 OUT create_self14;
 OUT ret_val47;
 OUT plus_self11;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val48;
 FILE1 r16;
 OPT_PREFETCH prog_self10;
 PROG ret_val49;
 OUT create_self15;
 OUT ret_val50;
 OUT plus_self12;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val51;
 FILE1 r17;
 OUT create_self16;
 OUT ret_val52;
 OUT plus_self13;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val53;
 FILE1 r18;
 BOOL L2;
 CS_OPTIONS L3;
 BOOL L4;
 BOOL L51_;
 PROG L6;
 extern STR STARTI1374838211;
 FILE1 L7;
 OB L8;
 extern STR name62;
 FILE1 L10;
 OB L12;
 CS_OPTIONS L14;
 CS_OPTIONS L15;
 BOOL L16;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 STR L20;
 INT L21;
 INT L221_;
 INT L23;
 CHAR L241_;
 CHAR L25;
 BOOL L261_;
 SE_CONTEXT L27;
 dAM L28;
 SE_CONTEXT L29;
 PROG L30;
 extern STR foundi391347248;
 FILE1 L31;
 OB L32;
 FILE1 L34;
 OB L35;
 extern STR name9;
 FILE1 L37;
 OB L38;
 BOOL L40;
 BOOL L41;
 SE_CONTEXT L42;
 BOOL L431_;
 BOOL L44;
 BOOL L45;
 BOOL L461_;
 STR L47;
 INT L48;
 INT L491_;
 INT L50;
 CHAR L521_;
 CHAR L53;
 BOOL L541_;
 BOOL L551_;
 BOOL L561_;
 BOOL L571_;
 BOOL L581_;
 BOOL L591_;
 BOOL L601_;
 BOOL L611_;
 BOOL L621_;
 BOOL L631_;
 BOOL L64;
 BOOL L65;
 BOOL L66;
 BOOL L67;
 BOOL L68;
 BOOL L691_;
 BOOL L70;
 BOOL L711_;
 BOOL L72;
 BOOL L731_;
 AM_PREFETCH_STMT L74;
 OB L75;
 AM_LOCAL_EXPR L76;
 OB L77;
 AM_LOCAL_EXPR L78;
 OB L79;
 extern STR prefet1360873088;
 CS_OPTIONS L80;
 PROG L81;
 extern STR Oofpre1246140551;
 dSTAT L82;
 PROG L83;
 extern STR Insert191735456;
 FILE1 L84;
 OB L85;
 extern STR name9;
 FILE1 L87;
 OB L88;
 BOOL L90;
 BOOL L92;
 AM_LOCAL_EXPR L93;
 OB L94;
 PROG L95;
 extern STR specul_prefetch;
 FILE1 L96;
 OB L97;
 extern STR name9;
 FILE1 L99;
 OB L100;
 AM_WAITFOR_STMT L102;
 OB L103;
 AM_STMT_EXPR L104;
 OB L105;
 PROG L106;
 extern STR functi806347010;
 FILE1 L107;
 OB L108;
 extern STR name63;
 FILE1 L110;
 OB L111;
 PROG L113;
 extern STR PREFETCHINGDONE;
 FILE1 L114;
 OB L115;
 extern STR name64;
 FILE1 L117;
 OB L118;
 prefetch_done = FALSE;
 if ((ATTR(func,code1)==((dAM_STMT) NULL))) {
  L2 = TRUE;
 } else {
  cs_options_self = self;
  ret_val = OPT_CO1299251581;
  L3=ret_val;
  L4 = ATTR(L3,side_effects);
  L51_=!(L4); 
  L2 = L51_;
 }
 if (L2) {
  return;
 }
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 L6=ret_val1;
 if (ATTR(L6,opt_debug)) {
  create_self = ((OUT) NULL);
  ret_val2 = create_self;
  plus_self = ret_val2;
  plus_s = ((STR) &STARTI1374838211);
  stdout_self = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r = L7;
  
  SATTR(r,fp,stdout);
  ret_val3 = r;
  FILE_plus_STR(ret_val3, plus_s);
  AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(func,code1)));
  create_self1 = ((OUT) NULL);
  ret_val4 = create_self1;
  plus_self1 = ret_val4;
  plus_s1 = ((STR) &name62);
  stdout_self1 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L10 = ((FILE1) L12);
  r1 = L10;
  
  SATTR(r1,fp,stdout);
  ret_val5 = r1;
  FILE_plus_STR(ret_val5, plus_s1);
 }
 prog_self1 = self;
 ret_val6 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val6, ((dAM) ATTR(func,code1)));
 SATTR(c,ignore_pre,TRUE);
 SATTR(c,ignore_post,TRUE);
 SATTR(c,ignore_assert,TRUE);
 SATTR(c,assign_in_order,TRUE);
 cs_options_self1 = self;
 ret_val7 = OPT_CO1299251581;
 L14=ret_val7;
 specul = ATTR(L14,specul_prefetch);
 cs_options_self2 = self;
 ret_val8 = OPT_CO1299251581;
 L15=ret_val8;
 loopp = ATTR(L15,loop_prefetch);
 is_iter_self = ATTR(func,sig1);
 is_iter_self1 = ATTR(is_iter_self,name1);
 L18 = (is_iter_self1.str==((STR) NULL));
 L191_=!(L18); 
 if (L191_) {
  L20 = is_iter_self1.str;
  L21 = STR_sizerINT(is_iter_self1.str);
  L221_=INTMINUS(L21,1); 
  L23 = L221_;
  L241_=ARR((STR)L20,L23); 
  L25 = L241_;
  L261_=L25=='!'; 
  L17 = L261_;
 } else {
  L17 = FALSE;
 }
 ret_val10 = L17;
 ret_val9 = ret_val10;
 if (ret_val9) {
  L16 = TRUE;
 } else {
  prog_self2 = self;
  ret_val11 = OPT_CO233462019;
  L27=SIG_ge642497605(ATTR(func,sig1), ret_val11);
  L16 = ATTR(L27,has_raise);
 }
 if (L16) {
  loopp = FALSE;
  specul = FALSE;
 }
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L28 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L28;
   if (am==NULL) {
    goto other336;
   } else
   switch (TAG(am)) {
    case AM_ROU1916046290_tag:
     prog_self3 = self;
     ret_val12 = OPT_CO233462019;
     L29=SIG_ge642497605(ATTR(((AM_ROU1916046290) am),fun), ret_val12);
     if (ATTR(L29,has_import)) {
      prog_self4 = self;
      ret_val13 = OPT_CO233462019;
      L30=ret_val13;
      if (ATTR(L30,opt_debug)) {
       create_self2 = ((OUT) NULL);
       ret_val14 = create_self2;
       plus_self2 = ret_val14;
       plus_s2 = ((STR) &foundi391347248);
       stdout_self2 = ((FILE1) NULL);
       L32=ZALLOC(sizeof(struct FILE1_struct));
       if (L32==NULL) FATAL("Unable to allocate more memory");
       ((OB)L32)->header.tag=FILE1_tag;
       L31 = ((FILE1) L32);
       r2 = L31;
       
       SATTR(r2,fp,stdout);
       ret_val16 = r2;
       FILE_plus_STR(ret_val16, plus_s2);
       ret_val15 = plus_self2;
       plus_self3 = ret_val15;
       plus_s3 = ATTR(ATTR(((AM_ROU1916046290) am),fun),str);
       stdout_self3 = ((FILE1) NULL);
       L35=ZALLOC(sizeof(struct FILE1_struct));
       if (L35==NULL) FATAL("Unable to allocate more memory");
       ((OB)L35)->header.tag=FILE1_tag;
       L34 = ((FILE1) L35);
       r3 = L34;
       
       SATTR(r3,fp,stdout);
       ret_val18 = r3;
       FILE_plus_STR(ret_val18, plus_s3);
       ret_val17 = plus_self3;
       plus_self4 = ret_val17;
       plus_s4 = ((STR) &name9);
       stdout_self4 = ((FILE1) NULL);
       L38=ZALLOC(sizeof(struct FILE1_struct));
       if (L38==NULL) FATAL("Unable to allocate more memory");
       ((OB)L38)->header.tag=FILE1_tag;
       L37 = ((FILE1) L38);
       r4 = L37;
       
       SATTR(r4,fp,stdout);
       ret_val19 = r4;
       FILE_plus_STR(ret_val19, plus_s4);
      }
      AM_CUR1119852682(c);
     } break;
    case AM_ITE1809135850_tag:
     prog_self5 = self;
     ret_val20 = OPT_CO233462019;
     L42=SIG_ge642497605(ATTR(((AM_ITE1809135850) am),fun), ret_val20);
     if (ATTR(L42,has_import)) {
      L41 = TRUE;
     } else {
      L431_=!(loopp); 
      L41 = L431_;
     }
     if (L41) {
      L40 = TRUE;
     } else {
      is_iter_self2 = ATTR(func,sig1);
      is_iter_self3 = ATTR(is_iter_self2,name1);
      L45 = (is_iter_self3.str==((STR) NULL));
      L461_=!(L45); 
      if (L461_) {
       L47 = is_iter_self3.str;
       L48 = STR_sizerINT(is_iter_self3.str);
       L491_=INTMINUS(L48,1); 
       L50 = L491_;
       L521_=ARR((STR)L47,L50); 
       L53 = L521_;
       L541_=L53=='!'; 
       L44 = L541_;
      } else {
       L44 = FALSE;
      }
      ret_val22 = L44;
      ret_val21 = ret_val22;
      L40 = ret_val21;
     }
     if (L40) {
      AM_CUR1119852682(c);
     } break;
    case AM_BREAK_STMT_tag:
     L551_=!(loopp); 
     if (L551_) {
      AM_CUR1119852682(c);
     } break;
    case AM_RAISE_STMT_tag:
     L561_=!(specul); 
     if (L561_) {
      AM_CUR1119852682(c);
     } break;
    case AM_IF_STMT_tag:
     L571_=!(specul); 
     if (L571_) {
      AM_CUR1119852682(c);
     } break;
    case AM_IF_EXPR_tag:
     L581_=!(specul); 
     if (L581_) {
      AM_CUR1119852682(c);
     } break;
    case AM_TYPECASE_STMT_tag:
     L591_=!(specul); 
     if (L591_) {
      AM_CUR1119852682(c);
     } break;
    case AM_CASE_STMT_tag:
     L601_=!(specul); 
     if (L601_) {
      AM_CUR1119852682(c);
     } break;
    case AM_LOCK_STMT_tag:
     L611_=!(specul); 
     if (L611_) {
      AM_CUR1119852682(c);
     } break;
    case AM_WIT1996971603_tag:
     L621_=!(specul); 
     if (L621_) {
      AM_CUR1119852682(c);
     } break;
    case AM_RETURN_STMT_tag:
     L631_=!(specul); 
     if (L631_) {
      AM_CUR1119852682(c);
     } break;
    case AM_YIELD_STMT_tag:
     AM_CUR1119852682(c); break;
    case AM_BND260301329_tag:
     AM_CUR1119852682(c); break;
    case AM_BND367211769_tag:
     AM_CUR1119852682(c); break;
    default: ;
    other336: ;
   }
   if (am==NULL) {
    goto other337;
   } else
   switch (TAG(am)) {
    case AM_ATTR_EXPR_tag:
     L68 = AM_CUR1481630356(c);
     L691_=!(L68); 
     if (L691_) {
      L67 = (AM_CUR141764973(c)==((AM_PREFETCH_STMT) NULL));
     } else {
      L67 = FALSE;
     }
     if (L67) {
      L66 = (AM_CUR1250801636(c)==((AM_WAITFOR_STMT) NULL));
     } else {
      L66 = FALSE;
     }
     if (L66) {
      L70 = AM_CUR2066246826(c);
      L711_=!(L70); 
      L65 = L711_;
     } else {
      L65 = FALSE;
     }
     if (L65) {
      L72 = AM_CUR403450479(c, am);
      L731_=!(L72); 
      L64 = L731_;
     } else {
      L64 = FALSE;
     }
     if (L64) {
      create_self3 = ((AM_PREFETCH_STMT) NULL);
      create_source = ATTR(((AM_ATTR_EXPR) am),source1);
      L75=ZALLOC(sizeof(struct AM_PREFETCH_STMT_struct));
      if (L75==NULL) FATAL("Unable to allocate more memory");
      ((OB)L75)->header.tag=AM_PREFETCH_STMT_tag;
      L74 = ((AM_PREFETCH_STMT) L75);
      r5 = L74;
      SATTR(r5,source1,create_source);
      ret_val23 = r5;
      p = ret_val23;
      create_self4 = ((AM_LOCAL_EXPR) NULL);
      create_src = ATTR(((AM_ATTR_EXPR) am),source1);
      create_name = IDENT_zero;
      tp_self = ((AM_ATTR_EXPR) am);
      ret_val24 = ATTR(tp_self,tp_at);
      create_tp = ret_val24;
      L77=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
      if (L77==NULL) FATAL("Unable to allocate more memory");
      ((OB)L77)->header.tag=AM_LOCAL_EXPR_tag;
      L76 = ((AM_LOCAL_EXPR) L77);
      r6 = L76;
      SATTR(r6,source1,create_src);
      SATTR(r6,name1,create_name);
      SATTR(r6,tp_at,create_tp);
      ret_val25 = r6;
      SATTR(p,dest,((dAM_EXPR) ret_val25));
      create_self5 = ((AM_LOCAL_EXPR) NULL);
      create_src1 = ATTR(((AM_ATTR_EXPR) am),source1);
      create_name1 = IDENT_zero;
      create_tp1 = ((dTP) TP_BUI765623063);
      L79=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
      if (L79==NULL) FATAL("Unable to allocate more memory");
      ((OB)L79)->header.tag=AM_LOCAL_EXPR_tag;
      L78 = ((AM_LOCAL_EXPR) L79);
      r7 = L78;
      SATTR(r7,source1,create_src1);
      SATTR(r7,name1,create_name1);
      SATTR(r7,tp_at,create_tp1);
      ret_val26 = r7;
      SATTR(p,prefetch,ret_val26);
      SATTR(ATTR(p,prefetch),is_volatile,TRUE);
      plus_self5 = ((STR) &prefet1360873088);
      plus_sarg = SFILE_ID_strrSTR(ATTR(((AM_ATTR_EXPR) am),source1));
      ret_val27 = STR_ap2004550586(plus_self5, plus_sarg);
      SATTR(p,comment1,ret_val27);
      SATTR(p,src1,((dAM_EXPR) am));
      insert_after_mar = c;
      insert_after_mar1 = ((dAM_STMT) p);
      cs_options_self3 = self;
      ret_val28 = OPT_CO1299251581;
      L80=ret_val28;
      insert_after_mar2 = ATTR(L80,prefetch_weight);
      ret_val29 = AM_CUR921612586(insert_after_mar, insert_after_mar1, insert_after_mar2, TRUE);
      if (ret_val29) {
       prog_self6 = self;
       ret_val30 = OPT_CO233462019;
       L81=ret_val30;
       L82 = ATTR(L81,stat1);
       (*dSTAT_incr_STR[TAG(L82)])(L82, ((STR) &Oofpre1246140551));
       prefetch_done = TRUE;
       prog_self7 = self;
       ret_val31 = OPT_CO233462019;
       L83=ret_val31;
       if (ATTR(L83,opt_debug)) {
        create_self6 = ((OUT) NULL);
        ret_val32 = create_self6;
        plus_self6 = ret_val32;
        plus_s5 = ((STR) &Insert191735456);
        stdout_self5 = ((FILE1) NULL);
        L85=ZALLOC(sizeof(struct FILE1_struct));
        if (L85==NULL) FATAL("Unable to allocate more memory");
        ((OB)L85)->header.tag=FILE1_tag;
        L84 = ((FILE1) L85);
        r8 = L84;
        
        SATTR(r8,fp,stdout);
        ret_val33 = r8;
        FILE_plus_STR(ret_val33, plus_s5);
        AM_OUT1439666796(((AM_OUT) NULL), am);
        create_self7 = ((OUT) NULL);
        ret_val34 = create_self7;
        plus_self7 = ret_val34;
        plus_s6 = ((STR) &name9);
        stdout_self6 = ((FILE1) NULL);
        L88=ZALLOC(sizeof(struct FILE1_struct));
        if (L88==NULL) FATAL("Unable to allocate more memory");
        ((OB)L88)->header.tag=FILE1_tag;
        L87 = ((FILE1) L88);
        r9 = L87;
        
        SATTR(r9,fp,stdout);
        ret_val35 = r9;
        FILE_plus_STR(ret_val35, plus_s6);
       }
       SATTR(func,locals1,FLISTA1817671564(ATTR(func,locals1), ATTR(p,prefetch)));
       t = ATTR(p,dest);
       switch (TAG(t)) {
        case AM_LOCAL_EXPR_tag:
         SATTR(func,locals1,FLISTA1817671564(ATTR(func,locals1), ((AM_LOCAL_EXPR) t))); break;
        default: ;
         FATAL("No applicable type in typecase\nin OPT_PREFETCH::prefetch(AM_ROUT_DEF)\n./Back/o_prefetch.sa:77:26");
       }
       if ((ATTR(func,specul_prefetch)==((AM_LOCAL_EXPR) NULL))) {
        if (specul) {
         L92 = TRUE;
        } else {
         L92 = loopp;
        }
        L90 = L92;
       } else {
        L90 = FALSE;
       }
       if (L90) {
        create_self8 = ((AM_LOCAL_EXPR) NULL);
        create_src2 = ATTR(((AM_ATTR_EXPR) am),source1);
        create_name2 = IDENT_zero;
        create_tp2 = ((dTP) TP_BUI765623063);
        L94=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
        if (L94==NULL) FATAL("Unable to allocate more memory");
        ((OB)L94)->header.tag=AM_LOCAL_EXPR_tag;
        L93 = ((AM_LOCAL_EXPR) L94);
        r10 = L93;
        SATTR(r10,source1,create_src2);
        SATTR(r10,name1,create_name2);
        SATTR(r10,tp_at,create_tp2);
        ret_val36 = r10;
        SATTR(func,specul_prefetch,ret_val36);
        prog_self8 = self;
        ret_val37 = OPT_CO233462019;
        L95=ret_val37;
        if (ATTR(L95,opt_debug)) {
         create_self9 = ((OUT) NULL);
         ret_val38 = create_self9;
         plus_self8 = ret_val38;
         plus_s7 = ((STR) &specul_prefetch);
         stdout_self7 = ((FILE1) NULL);
         L97=ZALLOC(sizeof(struct FILE1_struct));
         if (L97==NULL) FATAL("Unable to allocate more memory");
         ((OB)L97)->header.tag=FILE1_tag;
         L96 = ((FILE1) L97);
         r11 = L96;
         
         SATTR(r11,fp,stdout);
         ret_val39 = r11;
         FILE_plus_STR(ret_val39, plus_s7);
         AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(func,specul_prefetch)));
         create_self10 = ((OUT) NULL);
         ret_val40 = create_self10;
         plus_self9 = ret_val40;
         plus_s8 = ((STR) &name9);
         stdout_self8 = ((FILE1) NULL);
         L100=ZALLOC(sizeof(struct FILE1_struct));
         if (L100==NULL) FATAL("Unable to allocate more memory");
         ((OB)L100)->header.tag=FILE1_tag;
         L99 = ((FILE1) L100);
         r12 = L99;
         
         SATTR(r12,fp,stdout);
         ret_val41 = r12;
         FILE_plus_STR(ret_val41, plus_s8);
        }
        SATTR(ATTR(func,specul_prefetch),is_volatile,TRUE);
        SATTR(func,locals1,FLISTA1817671564(ATTR(func,locals1), ATTR(func,specul_prefetch)));
       }
       create_self11 = ((AM_WAITFOR_STMT) NULL);
       create_source1 = ATTR(((AM_ATTR_EXPR) am),source1);
       L103=ZALLOC(sizeof(struct AM_WAITFOR_STMT_struct));
       if (L103==NULL) FATAL("Unable to allocate more memory");
       ((OB)L103)->header.tag=AM_WAITFOR_STMT_tag;
       L102 = ((AM_WAITFOR_STMT) L103);
       r13 = L102;
       SATTR(r13,source1,create_source1);
       ret_val42 = r13;
       w = ret_val42;
       SATTR(w,prefetch,ATTR(p,prefetch));
       SATTR(w,src1,ATTR(p,src1));
       SATTR(w,dest,ATTR(p,dest));
       create_self12 = ((AM_STMT_EXPR) NULL);
       create_source2 = ATTR(((AM_ATTR_EXPR) am),source1);
       L105=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
       if (L105==NULL) FATAL("Unable to allocate more memory");
       ((OB)L105)->header.tag=AM_STMT_EXPR_tag;
       L104 = ((AM_STMT_EXPR) L105);
       r14 = L104;
       SATTR(r14,source1,create_source2);
       ret_val43 = r14;
       e = ret_val43;
       SATTR(e,expr,ATTR(w,dest));
       SATTR(e,stmts,((dAM_STMT) w));
       AM_CUR915996644(c, ((dAM_EXPR) e));
       tam = ATTR(c,top1);
       switch (TAG(tam)) {
        case AM_ASSERT_STMT_tag:
        case AM_ASSIGN_STMT_tag:
        case AM_ATTACH_STMT_tag:
        case AM_BREAK_STMT_tag:
        case AM_CASE_STMT_tag:
        case AM_COMMENT_STMT_tag:
        case AM_EXPR_STMT_tag:
        case AM_FORK_STMT_tag:
        case AM_IF_STMT_tag:
        case AM_INITIAL_STMT_tag:
        case AM_INV1167837230_tag:
        case AM_LOCK_STMT_tag:
        case AM_LOOP_STMT_tag:
        case AM_PAR_STMT_tag:
        case AM_POST_STMT_tag:
        case AM_PREFETCH_STMT_tag:
        case AM_PRE_STMT_tag:
        case AM_PROTECT_STMT_tag:
        case AM_RAISE_STMT_tag:
        case AM_RETURN_STMT_tag:
        case AM_SYNC_STMT_tag:
        case AM_TYPECASE_STMT_tag:
        case AM_UNLOCK_STMT_tag:
        case AM_WAITFOR_STMT_tag:
        case AM_WIT1996971603_tag:
        case AM_YIELD_STMT_tag:
         SATTR(func,code1,((dAM_STMT) tam)); break;
        default: ;
         FATAL("No applicable type in typecase\nin OPT_PREFETCH::prefetch(AM_ROUT_DEF)\n./Back/o_prefetch.sa:96:26");
       }
       prog_self9 = self;
       ret_val44 = OPT_CO233462019;
       L106=ret_val44;
       if (ATTR(L106,opt_debug)) {
        create_self13 = ((OUT) NULL);
        ret_val45 = create_self13;
        plus_self10 = ret_val45;
        plus_s9 = ((STR) &functi806347010);
        stdout_self9 = ((FILE1) NULL);
        L108=ZALLOC(sizeof(struct FILE1_struct));
        if (L108==NULL) FATAL("Unable to allocate more memory");
        ((OB)L108)->header.tag=FILE1_tag;
        L107 = ((FILE1) L108);
        r15 = L107;
        
        SATTR(r15,fp,stdout);
        ret_val46 = r15;
        FILE_plus_STR(ret_val46, plus_s9);
        AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(func,code1)));
        create_self14 = ((OUT) NULL);
        ret_val47 = create_self14;
        plus_self11 = ret_val47;
        plus_s10 = ((STR) &name63);
        stdout_self10 = ((FILE1) NULL);
        L111=ZALLOC(sizeof(struct FILE1_struct));
        if (L111==NULL) FATAL("Unable to allocate more memory");
        ((OB)L111)->header.tag=FILE1_tag;
        L110 = ((FILE1) L111);
        r16 = L110;
        
        SATTR(r16,fp,stdout);
        ret_val48 = r16;
        FILE_plus_STR(ret_val48, plus_s10);
       }
      }
     } break;
    default: ;
    other337: ;
   }
  }
 }
 after_loop: ;
 tmp1 = ATTR(c,top1);
 switch (TAG(tmp1)) {
  case AM_ASSERT_STMT_tag:
  case AM_ASSIGN_STMT_tag:
  case AM_ATTACH_STMT_tag:
  case AM_BREAK_STMT_tag:
  case AM_CASE_STMT_tag:
  case AM_COMMENT_STMT_tag:
  case AM_EXPR_STMT_tag:
  case AM_FORK_STMT_tag:
  case AM_IF_STMT_tag:
  case AM_INITIAL_STMT_tag:
  case AM_INV1167837230_tag:
  case AM_LOCK_STMT_tag:
  case AM_LOOP_STMT_tag:
  case AM_PAR_STMT_tag:
  case AM_POST_STMT_tag:
  case AM_PREFETCH_STMT_tag:
  case AM_PRE_STMT_tag:
  case AM_PROTECT_STMT_tag:
  case AM_RAISE_STMT_tag:
  case AM_RETURN_STMT_tag:
  case AM_SYNC_STMT_tag:
  case AM_TYPECASE_STMT_tag:
  case AM_UNLOCK_STMT_tag:
  case AM_WAITFOR_STMT_tag:
  case AM_WIT1996971603_tag:
  case AM_YIELD_STMT_tag:
   SATTR(func,code1,((dAM_STMT) tmp1)); break;
  default: ;
   FATAL("No applicable type in typecase\nin OPT_PREFETCH::prefetch(AM_ROUT_DEF)\n./Back/o_prefetch.sa:107:14");
 }
 if (prefetch_done) {
  OPT_PR1134320793(self, func);
 }
 prog_self10 = self;
 ret_val49 = OPT_CO233462019;
 L113=ret_val49;
 if (ATTR(L113,opt_debug)) {
  create_self15 = ((OUT) NULL);
  ret_val50 = create_self15;
  plus_self12 = ret_val50;
  plus_s11 = ((STR) &PREFETCHINGDONE);
  stdout_self11 = ((FILE1) NULL);
  L115=ZALLOC(sizeof(struct FILE1_struct));
  if (L115==NULL) FATAL("Unable to allocate more memory");
  ((OB)L115)->header.tag=FILE1_tag;
  L114 = ((FILE1) L115);
  r17 = L114;
  
  SATTR(r17,fp,stdout);
  ret_val51 = r17;
  FILE_plus_STR(ret_val51, plus_s11);
  create_self16 = ((OUT) NULL);
  ret_val52 = create_self16;
  plus_self13 = ret_val52;
  plus_s12 = ((STR) &name64);
  stdout_self12 = ((FILE1) NULL);
  L118=ZALLOC(sizeof(struct FILE1_struct));
  if (L118==NULL) FATAL("Unable to allocate more memory");
  ((OB)L118)->header.tag=FILE1_tag;
  L117 = ((FILE1) L118);
  r18 = L117;
  
  SATTR(r18,fp,stdout);
  ret_val53 = r18;
  FILE_plus_STR(ret_val53, plus_s12);
 }
}
