#include "sather.h"

/* Layouts */

typedef struct dAS_NODE_struct {
 OB_HEADER header;
 } *dAS_NODE;

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dBUILD508976674_struct {
 OB_HEADER header;
 } *dBUILD508976674;

typedef struct dCHECK_AM_struct {
 OB_HEADER header;
 } *dCHECK_AM;

typedef struct dCHECK1877276428_struct {
 OB_HEADER header;
 } *dCHECK1877276428;

typedef struct dFIND_TYPES_struct {
 OB_HEADER header;
 } *dFIND_TYPES;

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dGET_MAIN_SIG_struct {
 OB_HEADER header;
 } *dGET_MAIN_SIG;

typedef struct dGET_OPTIONS_struct {
 OB_HEADER header;
 } *dGET_OPTIONS;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dOPTIMIZE_struct {
 OB_HEADER header;
 } *dOPTIMIZE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dSTAT_struct {
 OB_HEADER header;
 } *dSTAT;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ARRAYARRAYSTR_struct {/* layout for ARRAY{ARRAY{STR}} */
 OB_HEADER header;
 INT asize;
 struct ARRAYSTR_struct *arr_part[1];
 } *ARRAYARRAYSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct AS_OUT_struct {/* layout for AS_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_OUT;

typedef struct BUILD_TYPE_GRAPH_struct {/* layout for BUILD_TYPE_GRAPH */
 OB_HEADER header;
 struct PROG_struct *prog;
 } *BUILD_TYPE_GRAPH;

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

typedef struct CONFIG_DEF_struct {/* layout for CONFIG_DEF */
 OB_HEADER header;
 struct ARRAYARRAYSTR_struct *expr;
 STR name1;
 } *CONFIG_DEF;

typedef struct CONFIG_TBL_struct {/* layout for CONFIG_TBL */
 OB_HEADER header;
 struct FMAPSTRdCONFIG_struct *tbl;
 } *CONFIG_TBL;

typedef struct CS_struct {/* layout for CS */
 OB_HEADER header;
 BOOL only_parse;
 BOOL only_reachable;
 } *CS;

typedef struct ELT_EQ487127284_struct {/* layout for ELT_EQ{AS_CLASS_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ487127284;

typedef struct ELT_NI1411991819_struct {/* layout for ELT_NIL{AS_CLASS_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1411991819;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct FIND_GENERICS_struct {/* layout for FIND_GENERICS */
 OB_HEADER header;
 struct FSETTP_CLASS_struct *tp_todo;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *generic_class;
 struct TP_CONTEXT_struct *con;
 } *FIND_GENERICS;

typedef struct FIND_TYPES_struct {/* layout for FIND_TYPES */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CONTEXT_struct *con;
 } *FIND_TYPES;

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

typedef struct IDENT_BUILTIN_struct {/* layout for IDENT_BUILTIN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IDENT_BUILTIN;

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

typedef struct MS_RANDOM_GEN_struct {/* layout for MS_RANDOM_GEN */
 OB_HEADER header;
 INT seed;
 } *MS_RANDOM_GEN;

typedef struct NULL_STAT_struct {/* layout for NULL_STAT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *NULL_STAT;

typedef struct OPTIMIZE_struct {/* layout for OPTIMIZE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPTIMIZE;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct PARSE_struct {/* layout for PARSE */
 OB_HEADER header;
 struct FMAPSTRSTR_struct *has;
 struct FSETSTR_struct *convert_files;
 struct FSETSTR_struct *known_files;
 struct FSETSTR_struct *missing_classes;
 struct FSETSTR_struct *parsed;
 struct PROG_struct *prog;
 BOOL convert_all;
 BOOL version_1_0;
 } *PARSE;

typedef struct PROG_AS_TBL_struct {/* layout for PROG_AS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct AS_CLASS_DEF_struct *arr_part[1];
 } *PROG_AS_TBL;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

typedef struct RND_struct {/* layout for RND */
 OB_HEADER header;
 FLTD norm_cache;
 struct MS_RANDOM_GEN_struct *own_gen;
 BOOL norm_cache_valid;
 } *RND;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct STAT1_struct {/* layout for STAT */
 OB_HEADER header;
 struct FMAPSTRINT_struct *map;
 } *STAT1;

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

typedef struct TIME_struct {/* layout for TIME */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *TIME;

typedef struct TP_BUILTIN_struct {/* layout for TP_BUILTIN */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *TP_BUILTIN;

typedef struct TP_GRAPH_struct {/* layout for TP_GRAPH */
 OB_HEADER header;
 struct FMAPTP291739594_struct *child_tbl;
 struct FMAPTP291739594_struct *des_tbl;
 struct FMAPTP775297600_struct *anc_tbl;
 struct FMAPTP775297600_struct *par_tbl;
 struct FSETTUPIDENTINT_struct *cur;
 struct PROG_struct *prog;
 } *TP_GRAPH;

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

typedef struct TUPBOO1392923241_struct {/* layout for TUP{BOOL,INTI,INT,BOOL} */
 INT t3;
 struct INTI1_struct *t2;
 BOOL t1;
 BOOL t4;
 } TUPBOO1392923241;
static TUPBOO1392923241 TUPBOO1392923241_zero;

typedef struct TUPBOO1392923241_boxed_struct {
 OB_HEADER header;
 TUPBOO1392923241 immutable_part;
 } *TUPBOO1392923241_boxed;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

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

typedef struct ARRAYA1729769776_frame_struct {
 INT state;
 ARRAYARRAYSTR self;/* Formal argument: self */
 ARRAYSTR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYA1729769776_frame;

typedef struct ARRAYS1578977701_frame_struct {
 INT state;
 ARRAYSTR self;/* Formal argument: self */
 STR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYS1578977701_frame;

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

typedef struct FLISTTUPIDENTINT_struct {/* layout for FLIST{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FLISTTUPIDENTINT;

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

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct INLINE_struct {/* layout for INLINE */
 OB_HEADER header;
 struct INLINE_ITER_TBL_struct *itbl;
 struct INLINE_ROUT_TBL_struct *rtbl;
 INT iter_thres;
 INT rout_thres;
 struct PROG_struct *prog;
 BOOL inline_iters;
 BOOL inline_routs;
 } *INLINE;

typedef struct INLINE_ITER_TBL_struct {/* layout for INLINE_ITER_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ITER_SIG_struct *arr_part[1];
 } *INLINE_ITER_TBL;

typedef struct INLINE_ROUT_TBL_struct {/* layout for INLINE_ROUT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct dINLINE_ROUT_SIG_struct *arr_part[1];
 } *INLINE_ROUT_TBL;

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

typedef struct PROG_A1089081349_frame_struct {
 INT state;
 PROG_AS_TBL self;/* Formal argument: self */
 AS_CLASS_DEF ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *PROG_A1089081349_frame;

typedef struct PROG_A1919229787_frame_struct {
 INT state;
 PROG_AS_TBL self;/* Formal argument: self */
 AS_CLASS_DEF arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *PROG_A1919229787_frame;

typedef struct PROG_A1283019219_frame_struct {
 INT state;
 PROG_AS_TBL self;/* Formal argument: self */
 AS_CLASS_DEF ret_val2;
 PROG_AS_TBL L61;
 AS_CLASS_DEF r;
 INT L31;
 PROG_AS_TBL is_elt_nil_self;
 AS_CLASS_DEF is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1411991819 is_elt_nil_self1;
 AS_CLASS_DEF is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *PROG_A1283019219_frame;

#include "tags.h"

/* Globals */

extern CGEN LAYOUT_TBL_cgen;
extern FLISTSTR SFILE_ID_files;
extern FLISTTUPIDENTINT FIND_G1866376849;
extern FLISTTUPIDENTINT FIND_T1407637984;
extern FLT FLT_log2_e;
extern FLTD MS_RAN1117060533;
extern FLTD MS_RAN393309279;
extern FLTD MS_RAN393309507;
extern FLTD TIME_t854034019;
extern FSTR INTI_buf;
extern INT INLINE142050909;
extern INT INLINE2125477179;
extern INT INTI_log2B;
extern INT PROG_A1730220286;
extern MS_RANDOM_GEN RND_default_gen;
extern PROG AS_OUT_prog;
extern PROG LAYOUT_TBL_prog;

/* Function declarations */


extern RETURNED_CONST STR (**dGET_O723107324)(dGET_OPTIONS);

extern RETURNED_CONST void (**dBUILD408006298)(dBUILD508976674);

extern RETURNED_CONST void (**dCHECK1977119478)(dCHECK_AM);

extern RETURNED_CONST void (**dCHECK450012483)(dCHECK1877276428);

extern RETURNED_CONST void (**dFIND_896803978)(dFIND_TYPES);

extern RETURNED_CONST void (**dGENER1765432365)(dGENERATE_AM);

extern RETURNED_CONST void (**dGET_M1653483544)(dGET_MAIN_SIG, STR);

extern RETURNED_CONST void (**dGET_O2013757500)(dGET_OPTIONS);

extern RETURNED_CONST void (**dGET_O2036054845)(dGET_OPTIONS, ARRAYSTR);

extern RETURNED_CONST void (**dINLINE_init)(dINLINE);

extern RETURNED_CONST void (**dOPTIM311966595)(dOPTIMIZE);

extern RETURNED_CONST void (**dOPTIMIZE_init)(dOPTIMIZE);

extern RETURNED_CONST void (**dPARSE_parse_STR)(dPARSE, STR);

extern RETURNED_CONST void (**dSTAT_print)(dSTAT);
ARRAYSTR ARRAYA1729769776(ARRAYA1729769776_frame);
AS_CLASS_DEF PROG_A1089081349(PROG_A1089081349_frame);
AS_CLASS_DEF PROG_A1283019219(PROG_A1283019219_frame);
AS_CLASS_DEF PROG_A382977477(PROG_AS_TBL, TUPIDENTINT);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL PROG_A1289978353(PROG_AS_TBL, TUPIDENTINT, AS_CLASS_DEF);
BOOL PROG_A878808304(PROG_AS_TBL, TUPIDENTINT);
BOOL RAT_is1975945358(RAT, OB);
BOOL STR_is111530248(STR, STR);
CONFIG_DEF CONFIG1152920950(CONFIG_TBL, STR);
CS_OPTIONS CS_OPT1986127469(CS_OPTIONS, PROG, CS);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLTD MS_RAN2052767907(MS_RANDOM_GEN);
FSTR INTI_s96356965(INTI1, FSTR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INLINE_ITER_TBL INLINE785230726(INLINE_ITER_TBL, INT);
INLINE_ROUT_TBL INLINE1394857882(INLINE_ROUT_TBL, INT);
INT AS_PAR401840644(AS_PARAM_DEC);
INT INTI_c675035669(INTI1, INTI1);
INT INT_hi740923800(INT);
INT INT_up418511718(INT_up418511718_frame);
INT PROG_A864188141(PROG_AS_TBL);
INT RND_in1575450710(RND, INT, INT);
INT SFILE_1102877860(SFILE_ID);
INT STR_lengthrINT(STR);
INT sather_main(CS, ARRAYSTR);
INTI1 INTI_absrINTI(INTI1);
INTI1 INTI_c1588968505(INTI1, INT);
INTI1 INTI_d359767808(INTI1, INTI1);
INTI1 INTI_g1917565373(INTI1, INTI1);
INTI1 INTI_m436971372(INTI1, INTI1);
INTI1 INTI_m582155739(INTI1, INTI1);
INTI1 INTI_negaterINTI(INTI1);
INTI1 INTI_p1518979099(INTI1, INT);
INTI1 INTI_p1769163857(INTI1, INTI1);
INTI1 INTI_t1692605406(INTI1, INTI1);
MS_RANDOM_GEN RND_ge1507420718(RND);
OPTIMIZE OPTIMI831024750(OPTIMIZE, PROG);
PROG_AS_TBL PROG_A1306680723(PROG_AS_TBL);
PROG_AS_TBL PROG_A1754951656(PROG_AS_TBL, INT);
PROG_AS_TBL PROG_A2047269051(PROG_AS_TBL, AS_CLASS_DEF);
RAT RAT_cr9703311(RAT, INTI1, INTI1);
STR ARRAYS1578977701(ARRAYS1578977701_frame);
STR CONFIG1397479570(CONFIG_TBL, STR, INT);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR RAT_str_INTrSTR(RAT, INT);
STR RAT_strrSTR(RAT);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
STR STR_su203383946(STR, INT, INT);
STR UNIX_g108606936(UNIX, STR);
TUPBOO1392923241 RAT_fl2087165576(RAT, INT, INT);
void AS_OUT1171738231(AS_OUT, dAS_NODE);
void CHECK_452836548(CHECK_GENERICS);
void CONFIG1796802473(CONFIG_TBL, STR);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void FIND_G1788878638(FIND_GENERICS);
void IDENT_1047918280(IDENT_BUILTIN);
void MS_RAN795181255(MS_RANDOM_GEN, INT);
void PROG_A148914482(PROG_AS_TBL, PROG);
void PROG_A1919229787(PROG_A1919229787_frame);
void PROG_AS_TBL_dump(PROG_AS_TBL);
void TP_BUI670024966(TP_BUILTIN, TP_TBL, PROG);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <math.h>
#include <stdio.h>
_FM_F_F(log)

/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_CLASS_DEF PROG_A382977477(PROG_AS_TBL self, TUPIDENTINT q) {
 AS_CLASS_DEF ret_val;
 INT h = INT_zero;
 AS_CLASS_DEF e;
 AS_CLASS_DEF e1;
 PROG_AS_TBL query_hash_self;
 TUPIDENTINT query_hash_q = TUPIDENTINT_zero;
 INT ret_val1 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val2 = INT_zero;
 INT x = INT_zero;
 PROG_AS_TBL is_elt_nil_self;
 AS_CLASS_DEF is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self1;
 AS_CLASS_DEF is_elt_nil_e1;
 BOOL ret_val4 = BOOL_zero;
 PROG_AS_TBL is_elt_nil_self2;
 AS_CLASS_DEF is_elt_nil_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self3;
 AS_CLASS_DEF is_elt_nil_e3;
 BOOL ret_val6 = BOOL_zero;
 PROG_AS_TBL elt_nil_self;
 AS_CLASS_DEF ret_val7;
 ELT_NI1411991819 elt_nil_self1;
 AS_CLASS_DEF ret_val8;
 AS_CLASS_DEF e2 = ((AS_CLASS_DEF) NULL);
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 AS_CLASS_DEF L171_;
 INT L181_;
 INT L191_;
 INT L20;
 INT L221_;
 INT L23;
 BOOL L241_;
 AS_CLASS_DEF L251_;
 INT L261_;
 if ((self==((PROG_AS_TBL) NULL))) {
  ret_val = ((AS_CLASS_DEF) NULL);
  return ret_val;
 }
 query_hash_self = self;
 query_hash_q = q;
 hash_self = query_hash_q.t1;
 L1 = hash_self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val2 = L51_;
 L6 = ret_val2;
 L7 = query_hash_q.t2;
 L81_=INTTIMES(1111,L7); 
 L9 = L81_;
 L101_=INTPLUS(L6,L9); 
 ret_val1 = L101_;
 L11 = ret_val1;
 L121_=ASIZE((PROG_AS_TBL)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L11&L15; 
 h = L161_;
 while (1) {
  L171_=(AS_CLASS_DEF)ARR((PROG_AS_TBL)self,h); 
  e = L171_;
  is_elt_nil_self = self;
  is_elt_nil_e = e;
  is_elt_nil_self1 = ((ELT_NI1411991819) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val4 = (is_elt_nil_e1==((AS_CLASS_DEF) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  else {
   if (PROG_A1289978353(self, q, e)) {
    ret_val = e;
    return ret_val;
   }
  }
  L181_=INTPLUS(h,1); 
  h = L181_;
 }
 after_loop: ;
 L191_=ASIZE((PROG_AS_TBL)self); 
 L20 = L191_;
 L221_=INTMINUS(L20,1); 
 L23 = L221_;
 L241_=(h)==(L23); 
 if (L241_) {
  h = 0;
  while (1) {
   L251_=(AS_CLASS_DEF)ARR((PROG_AS_TBL)self,h); 
   e1 = L251_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = e1;
   is_elt_nil_self3 = ((ELT_NI1411991819) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val6 = (is_elt_nil_e3==((AS_CLASS_DEF) NULL));
   ret_val5 = ret_val6;
   if (ret_val5) {
    goto after_loop1;
   }
   else {
    if (PROG_A1289978353(self, q, e1)) {
     ret_val = e1;
     return ret_val;
    }
   }
   L261_=INTPLUS(h,1); 
   h = L261_;
  }
  after_loop1: ;
 }
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1411991819) NULL);
 ret_val8 = ((AS_CLASS_DEF) NULL);
 ret_val7 = ret_val8;
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PROG_A1289978353(PROG_AS_TBL self, TUPIDENTINT q, AS_CLASS_DEF t) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 if ((t==((AS_CLASS_DEF) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 is_eq_self = q.t1;
 is_eq_i = ATTR(t,name1);
 ret_val1 = (is_eq_self.str==is_eq_i.str);
 L1 = ret_val1;
 L21_=!(L1); 
 if (L21_) {
  ret_val = FALSE;
  return ret_val;
 }
 L3 = q.t2;
 L4 = AS_PAR401840644(ATTR(t,params));
 L51_=(L3)==(L4); 
 L6 = L51_;
 L71_=!(L6); 
 if (L71_) {
  ret_val = FALSE;
  return ret_val;
 }
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PROG_A878808304(PROG_AS_TBL self, TUPIDENTINT q) {
 BOOL ret_val = BOOL_zero;
 PROG_AS_TBL is_elt_nil_self;
 AS_CLASS_DEF is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self1;
 AS_CLASS_DEF is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 is_elt_nil_self = self;
 is_elt_nil_e = PROG_A382977477(self, q);
 is_elt_nil_self1 = ((ELT_NI1411991819) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((AS_CLASS_DEF) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  ret_val = TRUE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL RAT_is1975945358(RAT self, OB arg) {
 BOOL ret_val = BOOL_zero;
 RAT is_eq_self = RAT_zero;
 RAT is_eq_y = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val2 = BOOL_zero;
 INTI1 is_eq_self2;
 INTI1 is_eq_y2;
 BOOL ret_val3 = BOOL_zero;
 RAT L1;
 BOOL L2;
 BOOL L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 INT L7;
 BOOL L81_;
 if (arg==NULL) {
  goto other170;
 } else
 switch (TAG(arg)) {
  case RAT_tag:
   is_eq_self = self;
   L1 = ((RAT_boxed) arg)->immutable_part;
   is_eq_y = L1;
   is_eq_self1 = is_eq_self.u;
   is_eq_y1 = is_eq_y.u;
   if ((is_eq_self1==is_eq_y1)) {
    L3 = TRUE;
   } else {
    L4 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L51_=(L4)==(0); 
    L3 = L51_;
   }
   ret_val2 = L3;
   if (ret_val2) {
    is_eq_self2 = is_eq_self.v;
    is_eq_y2 = is_eq_y.v;
    if ((is_eq_self2==is_eq_y2)) {
     L6 = TRUE;
    } else {
     L7 = INTI_c675035669(is_eq_self2, is_eq_y2);
     L81_=(L7)==(0); 
     L6 = L81_;
    }
    ret_val3 = L6;
    L2 = ret_val3;
   } else {
    L2 = FALSE;
   }
   ret_val1 = L2;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other170: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLTD MS_RAN2052767907(MS_RANDOM_GEN self) {
 FLTD ret_val = FLTD_zero;
 FLTD tmp1 = FLTD_zero;
 FLTD L1;
 INT L2;
 FLTD L31_;
 FLTD L4;
 FLTD L51_;
 FLTD L6;
 FLTD L7;
 FLTD L81_;
 FLTD L9;
 FLTD L101_;
 FLTD L11;
 FLTD L121_;
 FLTD L13;
 FLTD L141_;
 FLTD L15;
 FLTD L161_;
 FLTD L17;
 INT L181_;
 INT L19;
 INT L201_;
 INT L21;
 FLTD L221_;
 FLTD L23;
 FLTD L24;
 FLTD L251_;
 L1 = MS_RAN393309507;
 L2 = ATTR(self,seed);
 L31_=(FLTD)L2; 
 L4 = L31_;
 L51_=L1*L4; 
 tmp1 = L51_;
 L6 = MS_RAN393309279;
 L7 = MS_RAN393309279;
 L81_=FLTDIV(tmp1,L7); 
 L9 = L81_;
 L101_=floor(L9); 
 L11 = L101_;
 L121_=L6*L11; 
 L13 = L121_;
 L141_=tmp1-L13; 
 L15 = L141_;
 L161_=floor(L15); 
 L17 = L161_;
 L181_=(INT)L17; 
 SATTR(self,seed,L181_);
 L19 = ATTR(self,seed);
 L201_=INTMINUS(L19,1); 
 L21 = L201_;
 L221_=(FLTD)L21; 
 L23 = L221_;
 L24 = MS_RAN1117060533;
 L251_=FLTDIV(L23,L24); 
 ret_val = L251_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT PROG_A864188141(PROG_AS_TBL self) {
 INT ret_val = INT_zero;
 if ((self==((PROG_AS_TBL) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT RND_in1575450710(RND self, INT l, INT u) {
 INT ret_val = INT_zero;
 RND uniform_self;
 FLTD ret_val1 = FLTD_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 FLTD L51_;
 FLTD L6;
 FLTD L7;
 FLTD L81_;
 FLTD L9;
 FLTD L101_;
 FLTD L12;
 INT L131_;
 INT L14;
 INT L151_;
 L11_=INTMINUS(u,l); 
 L2 = L11_;
 L31_=INTPLUS(L2,1); 
 L4 = L31_;
 L51_=(FLTD)L4; 
 L6 = L51_;
 uniform_self = self;
 ret_val1 = MS_RAN2052767907(RND_ge1507420718(uniform_self));
 L7 = ret_val1;
 L81_=L6*L7; 
 L9 = L81_;
 L101_=floor(L9); 
 L12 = L101_;
 L131_=(INT)L12; 
 L14 = L131_;
 L151_=INTPLUS(l,L14); 
 ret_val = L151_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT sather_main(CS self, ARRAYSTR a) {
 INT ret_val = INT_zero;
 PROG p;
 CGEN cgen;
 STR common1;
 STR plt;
 FLISTSTR L11;
 STR v;
 CONFIG_DEF L21;
 ARRAYSTR strs;
 ARRAYSTR L31;
 STR ps;
 STR sl;
 FSETSTR L41;
 STR fn;
 TIME start_self;
 PROG create_self;
 PROG ret_val1;
 PROG r;
 PARSE create_self1;
 PROG create_p;
 PARSE ret_val2;
 PARSE r1;
 FIND_TYPES create_self2;
 PROG create_p1;
 FIND_TYPES ret_val3;
 FIND_TYPES r2;
 FLISTTUPIDENTINT create_self3;
 FLISTTUPIDENTINT ret_val4;
 FIND_GENERICS create_self4;
 PROG create_p2;
 FIND_GENERICS ret_val5;
 FIND_GENERICS r3;
 FLISTTUPIDENTINT create_self5;
 FLISTTUPIDENTINT ret_val6;
 BUILD_TYPE_GRAPH create_self6;
 PROG create_p3;
 BUILD_TYPE_GRAPH ret_val7;
 BUILD_TYPE_GRAPH r4;
 CHECK_1174552075 create_self7;
 PROG create_p4;
 CHECK_1174552075 ret_val8;
 CHECK_1174552075 r5;
 GET_MAIN_SIG create_self8;
 PROG create_p5;
 GET_MAIN_SIG ret_val9;
 GET_MAIN_SIG r6;
 GENERATE_AM create_self9;
 PROG create_p6;
 GENERATE_AM ret_val10;
 GENERATE_AM r7;
 GLOBAL_TBL create_self10;
 PROG create_p7;
 GLOBAL_TBL ret_val11;
 CHECK_AM create_self11;
 PROG create_p8;
 CHECK_AM ret_val12;
 CHECK_AM r8;
 CHECK_GENERICS create_self12;
 PROG create_p9;
 CHECK_GENERICS ret_val13;
 CHECK_GENERICS r9;
 INLINE create_self13;
 PROG create_p10;
 INLINE ret_val14;
 INLINE r10;
 CGEN create_self14;
 PROG create_p11;
 CGEN ret_val15;
 CGEN res;
 STR plus_self;
 STR plus_sarg;
 STR ret_val16;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val17;
 CONFIG_TBL create_self15;
 STR create_filename;
 CONFIG_TBL ret_val18;
 CONFIG_TBL r11;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val19;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val20;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val21;
 ERR1 create_self16;
 ERR1 ret_val22;
 ERR1 plus_self5;
 dSTR plus_s;
 FILE1 stderr_self;
 FILE1 ret_val23;
 FILE1 r12;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val24;
 TP_TBL create_self17;
 PROG create_prog;
 TP_TBL ret_val25;
 TP_TBL r13;
 TP_GRAPH create_self18;
 PROG create_prog1;
 TP_GRAPH ret_val26;
 TP_GRAPH r14;
 TP_GRAPH_ABS_DES create_self19;
 PROG create_prog2;
 TP_GRAPH_ABS_DES ret_val27;
 TP_GRAPH_ABS_DES r15;
 PROG_AS_TBL create_self20;
 PROG_AS_TBL ret_val28;
 OUT create_self21;
 OUT ret_val29;
 OUT plus_self7;
 STR plus_s1;
 OUT ret_val30;
 FILE1 stdout_self;
 FILE1 ret_val31;
 FILE1 r16;
 OUT flush_self;
 FILE1 stdout_self1;
 FILE1 ret_val32;
 FILE1 r17;
 FILE1 flush_self1;
 CONFIG_TBL get_bool_self;
 STR get_bool_s;
 BOOL ret_val33 = BOOL_zero;
 CONFIG_DEF r18;
 CONFIG_DEF aget_self;
 INT aget_i = INT_zero;
 ARRAYSTR ret_val34;
 CONFIG_TBL get_bool_self1;
 STR get_bool_s1;
 BOOL ret_val35 = BOOL_zero;
 CONFIG_DEF r19;
 CONFIG_DEF aget_self1;
 INT aget_i1 = INT_zero;
 ARRAYSTR ret_val36;
 CONFIG_TBL get_bool_self2;
 STR get_bool_s2;
 BOOL ret_val37 = BOOL_zero;
 CONFIG_DEF r20;
 CONFIG_DEF aget_self2;
 INT aget_i2 = INT_zero;
 ARRAYSTR ret_val38;
 CONFIG_TBL get_bool_self3;
 STR get_bool_s3;
 BOOL ret_val39 = BOOL_zero;
 CONFIG_DEF r21;
 CONFIG_DEF aget_self3;
 INT aget_i3 = INT_zero;
 ARRAYSTR ret_val40;
 CONFIG_TBL get_bool_self4;
 STR get_bool_s4;
 BOOL ret_val41 = BOOL_zero;
 CONFIG_DEF r22;
 CONFIG_DEF aget_self4;
 INT aget_i4 = INT_zero;
 ARRAYSTR ret_val42;
 ERR1 create_self22;
 ERR1 ret_val43;
 ERR1 plus_self8;
 dSTR plus_s2;
 FILE1 stderr_self1;
 FILE1 ret_val44;
 FILE1 r23;
 OUT create_self23;
 OUT ret_val45;
 OUT plus_self9;
 STR plus_s3;
 OUT ret_val46;
 FILE1 stdout_self2;
 FILE1 ret_val47;
 FILE1 r24;
 OUT plus_self10;
 STR plus_s4;
 OUT ret_val48;
 FILE1 stdout_self3;
 FILE1 ret_val49;
 FILE1 r25;
 OUT plus_self11;
 STR plus_s5;
 FILE1 stdout_self4;
 FILE1 ret_val50;
 FILE1 r26;
 OUT create_self24;
 OUT ret_val51;
 OUT plus_self12;
 STR plus_s6;
 OUT ret_val52;
 FILE1 stdout_self5;
 FILE1 ret_val53;
 FILE1 r27;
 OUT plus_self13;
 STR plus_s7;
 OUT ret_val54;
 FILE1 stdout_self6;
 FILE1 ret_val55;
 FILE1 r28;
 OUT plus_self14;
 dSTR plus_s8;
 FILE1 stdout_self7;
 FILE1 ret_val56;
 FILE1 r29;
 OUT create_self25;
 OUT ret_val57;
 OUT plus_self15;
 STR plus_s9;
 FILE1 stdout_self8;
 FILE1 ret_val58;
 FILE1 r30;
 CONFIG_DEF eltb_self;
 ARRAYSTR ret_val59;
 ARRAYARRAYSTR L51;
 BOOL eltb_if_first = BOOL_zero;
 INT L61 = INT_zero;
 INT L71 = INT_zero;
 OUT create_self26;
 OUT ret_val60;
 OUT plus_self16;
 STR plus_s10;
 OUT ret_val61;
 FILE1 stdout_self9;
 FILE1 ret_val62;
 FILE1 r31;
 OUT plus_self17;
 STR plus_s11;
 OUT ret_val63;
 FILE1 stdout_self10;
 FILE1 ret_val64;
 FILE1 r32;
 OUT plus_self18;
 dSTR plus_s12;
 FILE1 stdout_self11;
 FILE1 ret_val65;
 FILE1 r33;
 OUT create_self27;
 OUT ret_val66;
 OUT plus_self19;
 STR plus_s13;
 OUT ret_val67;
 FILE1 stdout_self12;
 FILE1 ret_val68;
 FILE1 r34;
 OUT plus_self20;
 STR plus_s14;
 OUT ret_val69;
 FILE1 stdout_self13;
 FILE1 ret_val70;
 FILE1 r35;
 OUT plus_self21;
 dSTR plus_s15;
 FILE1 stdout_self14;
 FILE1 ret_val71;
 FILE1 r36;
 STR plus_self22;
 STR plus_sarg6;
 STR ret_val72;
 STR plus_self23;
 STR plus_sarg7;
 STR ret_val73;
 OUT create_self28;
 OUT ret_val74;
 OUT plus_self24;
 STR plus_s16;
 OUT ret_val75;
 FILE1 stdout_self15;
 FILE1 ret_val76;
 FILE1 r37;
 OUT plus_self25;
 STR plus_s17;
 FILE1 stdout_self16;
 FILE1 ret_val77;
 FILE1 r38;
 STAT1 create_self29;
 STAT1 ret_val78;
 STAT1 res1;
 FMAPSTRINT create_self30;
 FMAPSTRINT ret_val79;
 NULL_STAT create_self31;
 NULL_STAT ret_val80;
 OUT create_self32;
 OUT ret_val81;
 OUT plus_self26;
 dSTR plus_s18;
 OUT ret_val82;
 FILE1 stdout_self17;
 FILE1 ret_val83;
 FILE1 r39;
 TIME elapsed_self;
 FLTD ret_val84 = FLTD_zero;
 OUT plus_self27;
 dSTR plus_s19;
 OUT ret_val85;
 FILE1 stdout_self18;
 FILE1 ret_val86;
 FILE1 r40;
 OUT plus_self28;
 STR plus_s20;
 FILE1 stdout_self19;
 FILE1 ret_val87;
 FILE1 r41;
 TIME start_self1;
 OUT create_self33;
 OUT ret_val88;
 OUT plus_self29;
 STR plus_s21;
 OUT ret_val89;
 FILE1 stdout_self20;
 FILE1 ret_val90;
 FILE1 r42;
 OUT flush_self2;
 FILE1 stdout_self21;
 FILE1 ret_val91;
 FILE1 r43;
 FILE1 flush_self3;
 OUT create_self34;
 OUT ret_val92;
 OUT plus_self30;
 dSTR plus_s22;
 OUT ret_val93;
 FILE1 stdout_self22;
 FILE1 ret_val94;
 FILE1 r44;
 TIME elapsed_self1;
 FLTD ret_val95 = FLTD_zero;
 OUT plus_self31;
 dSTR plus_s23;
 OUT ret_val96;
 FILE1 stdout_self23;
 FILE1 ret_val97;
 FILE1 r45;
 OUT plus_self32;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val98;
 FILE1 r46;
 OUT create_self35;
 OUT ret_val99;
 OUT plus_self33;
 dSTR plus_s25;
 FILE1 stdout_self25;
 FILE1 ret_val100;
 FILE1 r47;
 OUT create_self36;
 OUT ret_val101;
 OUT plus_self34;
 dSTR plus_s26;
 FILE1 stdout_self26;
 FILE1 ret_val102;
 FILE1 r48;
 TIME start_self2;
 OUT create_self37;
 OUT ret_val103;
 OUT plus_self35;
 STR plus_s27;
 OUT ret_val104;
 FILE1 stdout_self27;
 FILE1 ret_val105;
 FILE1 r49;
 OUT flush_self4;
 FILE1 stdout_self28;
 FILE1 ret_val106;
 FILE1 r50;
 FILE1 flush_self5;
 OUT create_self38;
 OUT ret_val107;
 OUT plus_self36;
 dSTR plus_s28;
 OUT ret_val108;
 FILE1 stdout_self29;
 FILE1 ret_val109;
 FILE1 r51;
 TIME elapsed_self2;
 FLTD ret_val110 = FLTD_zero;
 OUT plus_self37;
 dSTR plus_s29;
 OUT ret_val1111;
 FILE1 stdout_self30;
 FILE1 ret_val112;
 FILE1 r52;
 OUT plus_self38;
 STR plus_s30;
 FILE1 stdout_self31;
 FILE1 ret_val113;
 FILE1 r53;
 OUT create_self39;
 OUT ret_val114;
 OUT plus_self39;
 dSTR plus_s31;
 OUT ret_val115;
 FILE1 stdout_self32;
 FILE1 ret_val116;
 FILE1 r54;
 INT str_self = INT_zero;
 STR ret_val117;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val118;
 FSTR str_self1;
 STR ret_val119;
 OUT plus_self40;
 STR plus_s32;
 OUT ret_val120;
 FILE1 stdout_self33;
 FILE1 ret_val121;
 FILE1 r55;
 OUT plus_self41;
 STR plus_s33;
 FILE1 stdout_self34;
 FILE1 ret_val122;
 FILE1 r56;
 TIME start_self3;
 OUT create_self40;
 OUT ret_val123;
 OUT plus_self42;
 STR plus_s34;
 OUT ret_val124;
 FILE1 stdout_self35;
 FILE1 ret_val125;
 FILE1 r57;
 OUT flush_self6;
 FILE1 stdout_self36;
 FILE1 ret_val126;
 FILE1 r58;
 FILE1 flush_self7;
 OUT create_self41;
 OUT ret_val127;
 OUT plus_self43;
 dSTR plus_s35;
 OUT ret_val128;
 FILE1 stdout_self37;
 FILE1 ret_val129;
 FILE1 r59;
 TIME elapsed_self3;
 FLTD ret_val130 = FLTD_zero;
 OUT plus_self44;
 dSTR plus_s36;
 OUT ret_val131;
 FILE1 stdout_self38;
 FILE1 ret_val132;
 FILE1 r60;
 OUT plus_self45;
 STR plus_s37;
 FILE1 stdout_self39;
 FILE1 ret_val133;
 FILE1 r61;
 TIME start_self4;
 OUT create_self42;
 OUT ret_val134;
 OUT plus_self46;
 STR plus_s38;
 OUT ret_val135;
 FILE1 stdout_self40;
 FILE1 ret_val136;
 FILE1 r62;
 OUT flush_self8;
 FILE1 stdout_self41;
 FILE1 ret_val137;
 FILE1 r63;
 FILE1 flush_self9;
 OUT create_self43;
 OUT ret_val138;
 OUT plus_self47;
 dSTR plus_s39;
 OUT ret_val139;
 FILE1 stdout_self42;
 FILE1 ret_val140;
 FILE1 r64;
 TIME elapsed_self4;
 FLTD ret_val141 = FLTD_zero;
 OUT plus_self48;
 dSTR plus_s40;
 OUT ret_val142;
 FILE1 stdout_self43;
 FILE1 ret_val143;
 FILE1 r65;
 OUT plus_self49;
 STR plus_s41;
 FILE1 stdout_self44;
 FILE1 ret_val144;
 FILE1 r66;
 OUT create_self44;
 OUT ret_val145;
 OUT plus_self50;
 dSTR plus_s42;
 OUT ret_val146;
 FILE1 stdout_self45;
 FILE1 ret_val147;
 FILE1 r67;
 TIME elapsed_self5;
 FLTD ret_val148 = FLTD_zero;
 OUT plus_self51;
 dSTR plus_s43;
 OUT ret_val149;
 FILE1 stdout_self46;
 FILE1 ret_val150;
 FILE1 r68;
 OUT plus_self52;
 STR plus_s44;
 FILE1 stdout_self47;
 FILE1 ret_val151;
 FILE1 r69;
 OUT create_self45;
 OUT ret_val152;
 OUT plus_self53;
 dSTR plus_s45;
 OUT ret_val153;
 FILE1 stdout_self48;
 FILE1 ret_val154;
 FILE1 r70;
 TIME elapsed_self6;
 FLTD ret_val155 = FLTD_zero;
 OUT plus_self54;
 dSTR plus_s46;
 OUT ret_val156;
 FILE1 stdout_self49;
 FILE1 ret_val157;
 FILE1 r71;
 OUT plus_self55;
 STR plus_s47;
 FILE1 stdout_self50;
 FILE1 ret_val158;
 FILE1 r72;
 OUT create_self46;
 OUT ret_val159;
 OUT plus_self56;
 dSTR plus_s48;
 OUT ret_val160;
 FILE1 stdout_self51;
 FILE1 ret_val161;
 FILE1 r73;
 TIME elapsed_self7;
 FLTD ret_val162 = FLTD_zero;
 OUT plus_self57;
 dSTR plus_s49;
 OUT ret_val163;
 FILE1 stdout_self52;
 FILE1 ret_val164;
 FILE1 r74;
 OUT plus_self58;
 STR plus_s50;
 FILE1 stdout_self53;
 FILE1 ret_val165;
 FILE1 r75;
 OUT create_self47;
 OUT ret_val166;
 OUT plus_self59;
 dSTR plus_s51;
 OUT ret_val167;
 FILE1 stdout_self54;
 FILE1 ret_val168;
 FILE1 r76;
 TIME elapsed_self8;
 FLTD ret_val169 = FLTD_zero;
 OUT plus_self60;
 dSTR plus_s52;
 OUT ret_val170;
 FILE1 stdout_self55;
 FILE1 ret_val171;
 FILE1 r77;
 OUT plus_self61;
 STR plus_s53;
 FILE1 stdout_self56;
 FILE1 ret_val172;
 FILE1 r78;
 TIME start_self5;
 OUT create_self48;
 OUT ret_val173;
 OUT plus_self62;
 STR plus_s54;
 OUT ret_val174;
 FILE1 stdout_self57;
 FILE1 ret_val175;
 FILE1 r79;
 OUT flush_self10;
 FILE1 stdout_self58;
 FILE1 ret_val176;
 FILE1 r80;
 FILE1 flush_self11;
 OUT create_self49;
 OUT ret_val177;
 OUT plus_self63;
 dSTR plus_s55;
 OUT ret_val178;
 FILE1 stdout_self59;
 FILE1 ret_val179;
 FILE1 r81;
 TIME elapsed_self9;
 FLTD ret_val180 = FLTD_zero;
 OUT plus_self64;
 dSTR plus_s56;
 OUT ret_val181;
 FILE1 stdout_self60;
 FILE1 ret_val182;
 FILE1 r82;
 OUT plus_self65;
 STR plus_s57;
 FILE1 stdout_self61;
 FILE1 ret_val183;
 FILE1 r83;
 OUT create_self50;
 OUT ret_val184;
 OUT plus_self66;
 dSTR plus_s58;
 FILE1 stdout_self62;
 FILE1 ret_val185;
 FILE1 r84;
 TIME start_self6;
 OUT create_self51;
 OUT ret_val186;
 OUT plus_self67;
 STR plus_s59;
 OUT ret_val187;
 FILE1 stdout_self63;
 FILE1 ret_val188;
 FILE1 r85;
 OUT flush_self12;
 FILE1 stdout_self64;
 FILE1 ret_val189;
 FILE1 r86;
 FILE1 flush_self13;
 OUT create_self52;
 OUT ret_val190;
 OUT plus_self68;
 STR plus_s60;
 OUT ret_val191;
 FILE1 stdout_self65;
 FILE1 ret_val192;
 FILE1 r87;
 OUT flush_self14;
 FILE1 stdout_self66;
 FILE1 ret_val193;
 FILE1 r88;
 FILE1 flush_self15;
 OUT create_self53;
 OUT ret_val194;
 OUT plus_self69;
 STR plus_s61;
 OUT ret_val195;
 FILE1 stdout_self67;
 FILE1 ret_val196;
 FILE1 r89;
 TIME elapsed_self10;
 FLTD ret_val197 = FLTD_zero;
 OUT plus_self70;
 dSTR plus_s62;
 OUT ret_val198;
 FILE1 stdout_self68;
 FILE1 ret_val199;
 FILE1 r90;
 OUT plus_self71;
 STR plus_s63;
 FILE1 stdout_self69;
 FILE1 ret_val200;
 FILE1 r91;
 OUT create_self54;
 OUT ret_val201;
 OUT plus_self72;
 dSTR plus_s64;
 FILE1 stdout_self70;
 FILE1 ret_val202;
 FILE1 r92;
 TIME start_self7;
 OUT create_self55;
 OUT ret_val203;
 OUT plus_self73;
 STR plus_s65;
 OUT ret_val204;
 FILE1 stdout_self71;
 FILE1 ret_val205;
 FILE1 r93;
 OUT flush_self16;
 FILE1 stdout_self72;
 FILE1 ret_val206;
 FILE1 r94;
 FILE1 flush_self17;
 OUT create_self56;
 OUT ret_val207;
 OUT plus_self74;
 dSTR plus_s66;
 OUT ret_val208;
 FILE1 stdout_self73;
 FILE1 ret_val209;
 FILE1 r95;
 TIME elapsed_self11;
 FLTD ret_val210 = FLTD_zero;
 OUT plus_self75;
 dSTR plus_s67;
 OUT ret_val211;
 FILE1 stdout_self74;
 FILE1 ret_val212;
 FILE1 r96;
 OUT plus_self76;
 STR plus_s68;
 FILE1 stdout_self75;
 FILE1 ret_val213;
 FILE1 r97;
 OUT create_self57;
 OUT ret_val214;
 OUT plus_self77;
 dSTR plus_s69;
 FILE1 stdout_self76;
 FILE1 ret_val215;
 FILE1 r98;
 TIME start_self8;
 OUT create_self58;
 OUT ret_val216;
 OUT plus_self78;
 STR plus_s70;
 OUT ret_val217;
 FILE1 stdout_self77;
 FILE1 ret_val218;
 FILE1 r99;
 OUT flush_self18;
 FILE1 stdout_self78;
 FILE1 ret_val219;
 FILE1 r100;
 FILE1 flush_self19;
 OUT create_self59;
 OUT ret_val220;
 OUT plus_self79;
 dSTR plus_s71;
 OUT ret_val221;
 FILE1 stdout_self79;
 FILE1 ret_val222;
 FILE1 r101;
 TIME elapsed_self12;
 FLTD ret_val223 = FLTD_zero;
 OUT plus_self80;
 dSTR plus_s72;
 OUT ret_val224;
 FILE1 stdout_self80;
 FILE1 ret_val225;
 FILE1 r102;
 OUT plus_self81;
 STR plus_s73;
 FILE1 stdout_self81;
 FILE1 ret_val226;
 FILE1 r103;
 OUT create_self60;
 OUT ret_val227;
 OUT plus_self82;
 dSTR plus_s74;
 FILE1 stdout_self82;
 FILE1 ret_val228;
 FILE1 r104;
 OUT create_self61;
 OUT ret_val229;
 OUT plus_self83;
 STR plus_s75;
 FILE1 stdout_self83;
 FILE1 ret_val230;
 FILE1 r105;
 FLTD L81_;
 PROG L9;
 OB L10;
 PARSE L12;
 OB L13;
 FIND_TYPES L14;
 OB L15;
 FIND_GENERICS L16;
 OB L17;
 BUILD_TYPE_GRAPH L18;
 OB L19;
 CHECK_1174552075 L20;
 OB L22;
 GET_MAIN_SIG L23;
 OB L24;
 GENERATE_AM L25;
 OB L26;
 GLOBAL_TBL L27;
 OB L28;
 CHECK_AM L29;
 OB L30;
 CHECK_GENERICS L32;
 OB L33;
 INLINE L34;
 OB L35;
 CGEN L36;
 OB L37;
 dGET_OPTIONS L38;
 extern STR System1789613969;
 ARRAYSTR L39;
 OB L40;
 INT L42;
 extern STR name1;
 dGET_OPTIONS L43;
 dGET_OPTIONS L44;
 extern STR ZONES1;
 BOOL L45;
 BOOL L461_;
 ARRAYSTR L47;
 OB L48;
 INT L49;
 extern STR name1;
 extern STR zones;
 dGET_OPTIONS L50;
 extern STR System1287683407;
 CONFIG_TBL L52;
 OB L53;
 INT L54;
 BOOL L551_;
 extern STR SATHER_PLATFORM;
 extern STR DEFAULT_PLATFORM;
 extern STR SystemPlatforms;
 ARRAYSTR L56;
 OB L57;
 INT L58;
 extern STR name1;
 extern STR Platformmodule;
 dGET_OPTIONS L59;
 INT L60;
 BOOL L621_;
 BOOL L63;
 BOOL L641_;
 extern STR Moreth814769403;
 FILE1 L65;
 OB L66;
 STR L68;
 extern STR CONFIG;
 TP_TBL L69;
 OB L70;
 TP_GRAPH L72;
 OB L73;
 TP_GRAPH_ABS_DES L74;
 OB L75;
 extern STR Initializing;
 FILE1 L76;
 OB L77;
 FILE1 L79;
 OB L80;
 extern STR BOOT;
 BOOL L83;
 BOOL L84;
 BOOL L851_;
 ARRAYARRAYSTR L86;
 ARRAYSTR L871_;
 ARRAYSTR L88;
 STR L891_;
 STR L90;
 extern STR true1;
 extern STR DISTRIBUTED;
 BOOL L91;
 BOOL L92;
 BOOL L931_;
 ARRAYARRAYSTR L94;
 ARRAYSTR L951_;
 ARRAYSTR L96;
 STR L971_;
 STR L98;
 extern STR true1;
 extern STR THREADS;
 BOOL L99;
 BOOL L100;
 BOOL L1011_;
 ARRAYARRAYSTR L102;
 ARRAYSTR L1031_;
 ARRAYSTR L104;
 STR L1051_;
 STR L106;
 extern STR true1;
 extern STR TRACE11;
 BOOL L107;
 BOOL L108;
 BOOL L1091_;
 ARRAYARRAYSTR L110;
 ARRAYSTR L1111_;
 ARRAYSTR L112;
 STR L1131_;
 STR L114;
 extern STR true1;
 BOOL L115;
 extern STR ZONES1;
 BOOL L116;
 BOOL L117;
 BOOL L1181_;
 ARRAYARRAYSTR L119;
 ARRAYSTR L1201_;
 ARRAYSTR L121;
 STR L1221_;
 STR L123;
 extern STR true1;
 BOOL L124;
 BOOL L1251_;
 extern STR Zonesa995165525;
 FILE1 L126;
 OB L127;
 extern STR VERSION;
 extern STR ICSISa2111359522;
 FILE1 L129;
 OB L130;
 FILE1 L132;
 OB L133;
 extern STR name9;
 FILE1 L135;
 OB L136;
 extern STR Defaultplatform;
 FILE1 L138;
 OB L139;
 extern STR DEFAULT_PLATFORM;
 FILE1 L141;
 OB L142;
 dSTR L144;
 OB L145;
 FILE1 L146;
 OB L147;
 extern STR Instal1200957049;
 FILE1 L149;
 OB L150;
 extern STR PLATFORMS;
 ARRAYSTR L153;
 INT L1521_br;
 ARRAYSTR aL1521_;
 INT L1541_br;
 STR aL1541_;
 STR L155;
 extern STR name41;
 FILE1 L156;
 OB L157;
 FILE1 L159;
 OB L160;
 dSTR L162;
 OB L163;
 FILE1 L164;
 OB L165;
 INT L1671_;
 INT L1681_;
 extern STR Homedirectory1;
 FILE1 L169;
 OB L170;
 FILE1 L172;
 OB L173;
 dSTR L175;
 OB L176;
 FILE1 L177;
 OB L178;
 extern STR LIBRARY;
 ARRAYSTR L180;
 OB L181;
 INT L182;
 extern STR name1;
 extern STR name191;
 extern STR LIBRARY;
 dGET_OPTIONS L183;
 ARRAYSTR L184;
 OB L185;
 INT L186;
 extern STR name1;
 dGET_OPTIONS L187;
 dGET_OPTIONS L188;
 FILE1 L189;
 OB L190;
 extern STR name163;
 FILE1 L192;
 OB L193;
 STAT1 L195;
 OB L196;
 NULL_STAT L197;
 OB L198;
 dSTR L199;
 OB L200;
 FILE1 L201;
 OB L202;
 FLTD L2041_;
 FLTD L205;
 FLTD L206;
 FLTD L2071_;
 dSTR L208;
 OB L209;
 FILE1 L210;
 OB L211;
 extern STR seconds1;
 FILE1 L213;
 OB L214;
 FLTD L2161_;
 extern STR Parsin1988046872;
 FILE1 L217;
 OB L218;
 FILE1 L220;
 OB L221;
 STR L223;
 dPARSE L224;
 dSTR L225;
 OB L226;
 FILE1 L227;
 OB L228;
 FLTD L2301_;
 FLTD L231;
 FLTD L232;
 FLTD L2331_;
 dSTR L234;
 OB L235;
 FILE1 L236;
 OB L237;
 extern STR seconds1;
 FILE1 L239;
 OB L240;
 dSTR L242;
 OB L243;
 FILE1 L244;
 OB L245;
 dSTR L247;
 OB L248;
 FILE1 L249;
 OB L250;
 FLTD L2521_;
 extern STR Findin833949756;
 FILE1 L253;
 OB L254;
 FILE1 L256;
 OB L257;
 dFIND_TYPES L259;
 dSTR L260;
 OB L261;
 FILE1 L262;
 OB L263;
 FLTD L2651_;
 FLTD L266;
 FLTD L267;
 FLTD L2681_;
 dSTR L269;
 OB L270;
 FILE1 L271;
 OB L272;
 extern STR seconds1;
 FILE1 L274;
 OB L275;
 dSTR L277;
 OB L278;
 FILE1 L279;
 OB L280;
 BOOL L282;
 BOOL L2831_;
 FILE1 L284;
 OB L285;
 extern STR Classesfound;
 FILE1 L287;
 OB L288;
 FLTD L2901_;
 extern STR Genera1846288163;
 FILE1 L291;
 OB L292;
 FILE1 L294;
 OB L295;
 dSTR L297;
 OB L298;
 FILE1 L299;
 OB L300;
 FLTD L3021_;
 FLTD L303;
 FLTD L304;
 FLTD L3051_;
 dSTR L306;
 OB L307;
 FILE1 L308;
 OB L309;
 extern STR seconds1;
 FILE1 L311;
 OB L312;
 FLTD L3141_;
 extern STR Typegr495187748;
 FILE1 L315;
 OB L316;
 FILE1 L318;
 OB L319;
 dBUILD508976674 L321;
 dSTR L322;
 OB L323;
 FILE1 L324;
 OB L325;
 FLTD L3271_;
 FLTD L328;
 FLTD L329;
 FLTD L3301_;
 dSTR L331;
 OB L332;
 FILE1 L333;
 OB L334;
 extern STR seconds2;
 FILE1 L336;
 OB L337;
 dCHECK1877276428 L339;
 dSTR L340;
 OB L341;
 FILE1 L342;
 OB L343;
 FLTD L3451_;
 FLTD L346;
 FLTD L347;
 FLTD L3481_;
 dSTR L349;
 OB L350;
 FILE1 L351;
 OB L352;
 extern STR seconds2;
 FILE1 L354;
 OB L355;
 dGET_MAIN_SIG L357;
 dSTR L358;
 OB L359;
 FILE1 L360;
 OB L361;
 FLTD L3631_;
 FLTD L364;
 FLTD L365;
 FLTD L3661_;
 dSTR L367;
 OB L368;
 FILE1 L369;
 OB L370;
 extern STR seconds2;
 FILE1 L372;
 OB L373;
 BOOL L375;
 BOOL L3761_;
 dINLINE L377;
 dOPTIMIZE L378;
 dSTR L379;
 OB L380;
 FILE1 L381;
 OB L382;
 FLTD L3841_;
 FLTD L385;
 FLTD L386;
 FLTD L3871_;
 dSTR L388;
 OB L389;
 FILE1 L390;
 OB L391;
 extern STR seconds2;
 FILE1 L393;
 OB L394;
 dSTR L396;
 OB L397;
 FILE1 L398;
 OB L399;
 FLTD L4011_;
 FLTD L402;
 FLTD L403;
 FLTD L4041_;
 dSTR L405;
 OB L406;
 FILE1 L407;
 OB L408;
 extern STR seconds1;
 FILE1 L410;
 OB L411;
 FLTD L4131_;
 extern STR Typech1407750582;
 FILE1 L414;
 OB L415;
 FILE1 L417;
 OB L418;
 dGENERATE_AM L420;
 dSTR L421;
 OB L422;
 FILE1 L423;
 OB L424;
 FLTD L4261_;
 FLTD L427;
 FLTD L428;
 FLTD L4291_;
 dSTR L430;
 OB L431;
 FILE1 L432;
 OB L433;
 extern STR seconds1;
 FILE1 L435;
 OB L436;
 dSTR L438;
 OB L439;
 FILE1 L440;
 OB L441;
 BOOL L443;
 BOOL L4441_;
 FLTD L4451_;
 extern STR Header575149500;
 FILE1 L446;
 OB L447;
 FILE1 L449;
 OB L450;
 extern STR Header164288235;
 FILE1 L452;
 OB L453;
 FILE1 L455;
 OB L456;
 dOPTIMIZE L458;
 extern STR name24;
 FILE1 L459;
 OB L460;
 FLTD L4631_;
 FLTD L464;
 FLTD L465;
 FLTD L4661_;
 dSTR L467;
 OB L468;
 FILE1 L469;
 OB L470;
 extern STR seconds1;
 FILE1 L472;
 OB L473;
 dSTR L475;
 OB L476;
 FILE1 L477;
 OB L478;
 FLTD L4801_;
 extern STR Checki135017201;
 FILE1 L481;
 OB L482;
 FILE1 L484;
 OB L485;
 dSTR L487;
 OB L488;
 FILE1 L489;
 OB L490;
 FLTD L4921_;
 FLTD L493;
 FLTD L494;
 FLTD L4951_;
 dSTR L496;
 OB L497;
 FILE1 L498;
 OB L499;
 extern STR seconds1;
 FILE1 L501;
 OB L502;
 dSTR L504;
 OB L505;
 FILE1 L506;
 OB L507;
 BOOL L509;
 BOOL L5101_;
 FLTD L5111_;
 extern STR Checki974602838;
 FILE1 L512;
 OB L513;
 FILE1 L515;
 OB L516;
 dCHECK_AM L518;
 dSTR L519;
 OB L520;
 FILE1 L521;
 OB L522;
 FLTD L5241_;
 FLTD L525;
 FLTD L526;
 FLTD L5271_;
 dSTR L528;
 OB L529;
 FILE1 L530;
 OB L531;
 extern STR seconds1;
 FILE1 L533;
 OB L534;
 dSTR L536;
 OB L537;
 FILE1 L538;
 OB L539;
 extern STR Compil1872856259;
 FILE1 L541;
 OB L542;
 dSTAT L544;
 start_self = ((TIME) NULL);
 L81_=(FLTD)time(NULL); 
 TIME_t854034019 = L81_;
 create_self = ((PROG) NULL);
 L10=ZALLOC(sizeof(struct PROG_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=PROG_tag;
 L9 = ((PROG) L10);
 r = L9;
 IDENT_1047918280(((IDENT_BUILTIN) NULL));
 ret_val1 = r;
 p = ret_val1;
 create_self1 = ((PARSE) NULL);
 create_p = p;
 L13=ZALLOC(sizeof(struct PARSE_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=PARSE_tag;
 L12 = ((PARSE) L13);
 r1 = L12;
 SATTR(r1,prog,create_p);
 ret_val2 = r1;
 SATTR(p,parse,((dPARSE) ret_val2));
 create_self2 = ((FIND_TYPES) NULL);
 create_p1 = p;
 L15=ZALLOC(sizeof(struct FIND_TYPES_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FIND_TYPES_tag;
 L14 = ((FIND_TYPES) L15);
 r2 = L14;
 SATTR(r2,prog,create_p1);
 create_self3 = ((FLISTTUPIDENTINT) NULL);
 ret_val4 = ((FLISTTUPIDENTINT) NULL);
 FIND_T1407637984 = ret_val4;
 ret_val3 = r2;
 SATTR(p,find_types,((dFIND_TYPES) ret_val3));
 create_self4 = ((FIND_GENERICS) NULL);
 create_p2 = p;
 L17=ZALLOC(sizeof(struct FIND_GENERICS_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FIND_GENERICS_tag;
 L16 = ((FIND_GENERICS) L17);
 r3 = L16;
 SATTR(r3,prog,create_p2);
 create_self5 = ((FLISTTUPIDENTINT) NULL);
 ret_val6 = ((FLISTTUPIDENTINT) NULL);
 FIND_G1866376849 = ret_val6;
 ret_val5 = r3;
 SATTR(p,find_generics,ret_val5);
 create_self6 = ((BUILD_TYPE_GRAPH) NULL);
 create_p3 = p;
 L19=ZALLOC(sizeof(struct BUILD_TYPE_GRAPH_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=BUILD_TYPE_GRAPH_tag;
 L18 = ((BUILD_TYPE_GRAPH) L19);
 r4 = L18;
 SATTR(r4,prog,create_p3);
 ret_val7 = r4;
 SATTR(p,build_type_graph,((dBUILD508976674) ret_val7));
 create_self7 = ((CHECK_1174552075) NULL);
 create_p4 = p;
 L22=ZALLOC(sizeof(struct CHECK_1174552075_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=CHECK_1174552075_tag;
 L20 = ((CHECK_1174552075) L22);
 r5 = L20;
 SATTR(r5,prog,create_p4);
 ret_val8 = r5;
 SATTR(p,check_ifc_confor,((dCHECK1877276428) ret_val8));
 create_self8 = ((GET_MAIN_SIG) NULL);
 create_p5 = p;
 L24=ZALLOC(sizeof(struct GET_MAIN_SIG_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=GET_MAIN_SIG_tag;
 L23 = ((GET_MAIN_SIG) L24);
 r6 = L23;
 SATTR(r6,prog,create_p5);
 ret_val9 = r6;
 SATTR(p,get_main_sig,((dGET_MAIN_SIG) ret_val9));
 create_self9 = ((GENERATE_AM) NULL);
 create_p6 = p;
 L26=ZALLOC(sizeof(struct GENERATE_AM_struct));
 if (L26==NULL) FATAL("Unable to allocate more memory");
 ((OB)L26)->header.tag=GENERATE_AM_tag;
 L25 = ((GENERATE_AM) L26);
 r7 = L25;
 SATTR(r7,prog,create_p6);
 create_self10 = ((GLOBAL_TBL) NULL);
 create_p7 = create_p6;
 L28=ZALLOC(sizeof(struct GLOBAL_TBL_struct));
 if (L28==NULL) FATAL("Unable to allocate more memory");
 ((OB)L28)->header.tag=GLOBAL_TBL_tag;
 L27 = ((GLOBAL_TBL) L28);
 ret_val11 = L27;
 SATTR(r7,global_tbl,ret_val11);
 ret_val10 = r7;
 SATTR(p,generate_am,((dGENERATE_AM) ret_val10));
 create_self11 = ((CHECK_AM) NULL);
 create_p8 = p;
 L30=ZALLOC(sizeof(struct CHECK_AM_struct));
 if (L30==NULL) FATAL("Unable to allocate more memory");
 ((OB)L30)->header.tag=CHECK_AM_tag;
 L29 = ((CHECK_AM) L30);
 r8 = L29;
 SATTR(r8,prog,create_p8);
 ret_val12 = r8;
 SATTR(p,check_am,((dCHECK_AM) ret_val12));
 create_self12 = ((CHECK_GENERICS) NULL);
 create_p9 = p;
 L33=ZALLOC(sizeof(struct CHECK_GENERICS_struct));
 if (L33==NULL) FATAL("Unable to allocate more memory");
 ((OB)L33)->header.tag=CHECK_GENERICS_tag;
 L32 = ((CHECK_GENERICS) L33);
 r9 = L32;
 SATTR(r9,prog,create_p9);
 ret_val13 = r9;
 SATTR(p,check_gen,ret_val13);
 create_self13 = ((INLINE) NULL);
 create_p10 = p;
 L35=ZALLOC(sizeof(struct INLINE_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 ((OB)L35)->header.tag=INLINE_tag;
 L34 = ((INLINE) L35);
 r10 = L34;
 SATTR(r10,rtbl,INLINE1394857882(((INLINE_ROUT_TBL) NULL), 1024));
 SATTR(r10,itbl,INLINE785230726(((INLINE_ITER_TBL) NULL), 64));
 SATTR(r10,prog,create_p10);
 SATTR(r10,inline_routs,FALSE);
 SATTR(r10,inline_iters,FALSE);
 SATTR(r10,rout_thres,INLINE2125477179);
 SATTR(r10,iter_thres,INLINE142050909);
 ret_val14 = r10;
 SATTR(p,inliner,((dINLINE) ret_val14));
 create_self14 = ((CGEN) NULL);
 create_p11 = p;
 L37=ZALLOC(sizeof(struct CGEN_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=CGEN_tag;
 L36 = ((CGEN) L37);
 res = L36;
 SATTR(res,prog,create_p11);
 LAYOUT_TBL_prog = create_p11;
 LAYOUT_TBL_cgen = res;
 SATTR(res,threshold,150000);
 ret_val15 = res;
 cgen = ret_val15;
 SATTR(p,back_end,((dBACK_END) cgen));
 SATTR(p,get_options,((dGET_OPTIONS) CS_OPT1986127469(((CS_OPTIONS) NULL), p, self)));
 L38 = ATTR(p,get_options);
 (*dGET_O2013757500[TAG(L38)])(L38);
 plus_self = ATTR(p,home);
 plus_sarg = ((STR) &System1789613969);
 ret_val16 = STR_ap2004550586(plus_self, plus_sarg);
 common1 = ret_val16;
 L42=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(2)*sizeof(STR);
 L40=ZALLOC_BIG(L42);
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L40)->header.destroyed=0;
#endif

 L39 = ((ARRAYSTR) L40);
 L39->asize = 2;
 L39->arr_part[0] = ((STR) &name1);
 L39->arr_part[1] = common1;
 L43 = ATTR(p,get_options);
 (*dGET_O2036054845[TAG(L43)])(L43, L39);
 L44 = ATTR(p,get_options);
 (*dGET_O2036054845[TAG(L44)])(L44, a);
 L45 = (UNIX_g108606936(((UNIX) NULL), ((STR) &ZONES1))==((STR) NULL));
 L461_=!(L45); 
 if (L461_) {
  L49=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(2)*sizeof(STR);
  L48=ZALLOC_BIG(L49);
  if (L48==NULL) FATAL("Unable to allocate more memory");
  ((OB)L48)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L48)->header.destroyed=0;
#endif

  L47 = ((ARRAYSTR) L48);
  L47->asize = 2;
  L47->arr_part[0] = ((STR) &name1);
  L47->arr_part[1] = ((STR) &zones);
  L50 = ATTR(p,get_options);
  (*dGET_O2036054845[TAG(L50)])(L50, L47);
 }
 create_self15 = ((CONFIG_TBL) NULL);
 plus_self1 = ATTR(p,home);
 plus_sarg1 = ((STR) &System1287683407);
 ret_val17 = STR_ap2004550586(plus_self1, plus_sarg1);
 create_filename = ret_val17;
 L53=ZALLOC(sizeof(struct CONFIG_TBL_struct));
 if (L53==NULL) FATAL("Unable to allocate more memory");
 ((OB)L53)->header.tag=CONFIG_TBL_tag;
 L52 = ((CONFIG_TBL) L53);
 r11 = L52;
 CONFIG1796802473(r11, create_filename);
 ret_val18 = r11;
 SATTR(p,config,ret_val18);
 L54 = ATTR(p,genuine_platform);
 L551_=(L54)==(0); 
 if (L551_) {
  plt = UNIX_g108606936(((UNIX) NULL), ((STR) &SATHER_PLATFORM));
  if ((plt==((STR) NULL))) {
   plt = CONFIG1397479570(ATTR(p,config), ((STR) &DEFAULT_PLATFORM), 0);
  }
  plus_self2 = ATTR(p,home);
  plus_sarg2 = ((STR) &SystemPlatforms);
  ret_val19 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val19;
  plus_sarg3 = plt;
  ret_val20 = STR_ap2004550586(plus_self3, plus_sarg3);
  plt = ret_val20;
  SATTR(p,platforms,FLISTS1425610062(ATTR(p,platforms), plt));
  L58=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(2)*sizeof(STR);
  L57=ZALLOC_BIG(L58);
  if (L57==NULL) FATAL("Unable to allocate more memory");
  ((OB)L57)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L57)->header.destroyed=0;
#endif

  L56 = ((ARRAYSTR) L57);
  L56->asize = 2;
  L56->arr_part[0] = ((STR) &name1);
  plus_self4 = plt;
  plus_sarg4 = ((STR) &Platformmodule);
  ret_val21 = STR_ap2004550586(plus_self4, plus_sarg4);
  L56->arr_part[1] = ret_val21;
  L59 = ATTR(p,get_options);
  (*dGET_O2036054845[TAG(L59)])(L59, L56);
 }
 else {
  L60 = ATTR(p,genuine_platform);
  L621_=(L60)==(1); 
  L63 = L621_;
  L641_=!(L63); 
  if (L641_) {
   create_self16 = ((ERR1) NULL);
   ret_val22 = create_self16;
   plus_self5 = ret_val22;
   plus_s = ((dSTR) ((STR) &Moreth814769403));
   stderr_self = ((FILE1) NULL);
   L66=ZALLOC(sizeof(struct FILE1_struct));
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=FILE1_tag;
   L65 = ((FILE1) L66);
   r12 = L65;
   
   SATTR(r12,fp,stderr);
   ret_val23 = r12;
   FILE_plus_dSTR(ret_val23, plus_s);
   exit_self = ((UNIX) NULL);
   exit_code = -1;
   exit(exit_code);
  }
 }
 {
  struct FLISTS1171631134_frame_struct other1_0;
FLISTS1171631134_frame noname1 = &other1_0;
  L11 = ATTR(p,platforms);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L68 = FLISTS1171631134(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_self6 = L68;
   plus_sarg5 = ((STR) &CONFIG);
   ret_val24 = STR_ap2004550586(plus_self6, plus_sarg5);
   CONFIG1796802473(ATTR(p,config), ret_val24);
  }
 }
 after_loop: ;
 create_self17 = ((TP_TBL) NULL);
 create_prog = p;
 L70=ZALLOC(sizeof(struct TP_TBL_struct));
 if (L70==NULL) FATAL("Unable to allocate more memory");
 ((OB)L70)->header.tag=TP_TBL_tag;
 L69 = ((TP_TBL) L70);
 r13 = L69;
 SATTR(r13,prog,create_prog);
 TP_BUI670024966(((TP_BUILTIN) NULL), r13, create_prog);
 ret_val25 = r13;
 SATTR(p,tp_tbl,ret_val25);
 create_self18 = ((TP_GRAPH) NULL);
 create_prog1 = p;
 L73=ZALLOC(sizeof(struct TP_GRAPH_struct));
 if (L73==NULL) FATAL("Unable to allocate more memory");
 ((OB)L73)->header.tag=TP_GRAPH_tag;
 L72 = ((TP_GRAPH) L73);
 r14 = L72;
 SATTR(r14,prog,create_prog1);
 ret_val26 = r14;
 SATTR(p,tp_graph,ret_val26);
 create_self19 = ((TP_GRAPH_ABS_DES) NULL);
 create_prog2 = p;
 L75=ZALLOC(sizeof(struct TP_GRAPH_ABS_DES_struct));
 if (L75==NULL) FATAL("Unable to allocate more memory");
 ((OB)L75)->header.tag=TP_GRAPH_ABS_DES_tag;
 L74 = ((TP_GRAPH_ABS_DES) L75);
 r15 = L74;
 SATTR(r15,prog,create_prog2);
 ret_val27 = r15;
 SATTR(p,tp_graph_abs_des,ret_val27);
 create_self20 = ((PROG_AS_TBL) NULL);
 ret_val28 = ((PROG_AS_TBL) NULL);
 SATTR(p,as_tbl,ret_val28);
 SATTR(p,optimizer,((dOPTIMIZE) OPTIMI831024750(((OPTIMIZE) NULL), p)));
 if (ATTR(p,verbose)) {
  create_self21 = ((OUT) NULL);
  ret_val29 = create_self21;
  plus_self7 = ret_val29;
  plus_s1 = ((STR) &Initializing);
  stdout_self = ((FILE1) NULL);
  L77=ZALLOC(sizeof(struct FILE1_struct));
  if (L77==NULL) FATAL("Unable to allocate more memory");
  ((OB)L77)->header.tag=FILE1_tag;
  L76 = ((FILE1) L77);
  r16 = L76;
  
  SATTR(r16,fp,stdout);
  ret_val31 = r16;
  FILE_plus_STR(ret_val31, plus_s1);
  ret_val30 = plus_self7;
  flush_self = ret_val30;
  stdout_self1 = ((FILE1) NULL);
  L80=ZALLOC(sizeof(struct FILE1_struct));
  if (L80==NULL) FATAL("Unable to allocate more memory");
  ((OB)L80)->header.tag=FILE1_tag;
  L79 = ((FILE1) L80);
  r17 = L79;
  
  SATTR(r17,fp,stdout);
  ret_val32 = r17;
  flush_self1 = ret_val32;
  fflush(ATTR(flush_self1,fp));
 }
 get_bool_self = ATTR(p,config);
 get_bool_s = ((STR) &BOOT);
 r18 = CONFIG1152920950(get_bool_self, get_bool_s);
 L84 = (r18==((CONFIG_DEF) NULL));
 L851_=!(L84); 
 if (L851_) {
  aget_self = r18;
  aget_i = 0;
  L86 = ATTR(aget_self,expr);
  L871_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L86,aget_i); 
  ret_val34 = L871_;
  L88 = ret_val34;
  L891_=(STR)ARR((ARRAYSTR)L88,0); 
  L90 = L891_;
  L83 = STR_is111530248(L90, ((STR) &true1));
 } else {
  L83 = FALSE;
 }
 ret_val33 = L83;
 SATTR(p,boot,ret_val33);
 get_bool_self1 = ATTR(p,config);
 get_bool_s1 = ((STR) &DISTRIBUTED);
 r19 = CONFIG1152920950(get_bool_self1, get_bool_s1);
 L92 = (r19==((CONFIG_DEF) NULL));
 L931_=!(L92); 
 if (L931_) {
  aget_self1 = r19;
  aget_i1 = 0;
  L94 = ATTR(aget_self1,expr);
  L951_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L94,aget_i1); 
  ret_val36 = L951_;
  L96 = ret_val36;
  L971_=(STR)ARR((ARRAYSTR)L96,0); 
  L98 = L971_;
  L91 = STR_is111530248(L98, ((STR) &true1));
 } else {
  L91 = FALSE;
 }
 ret_val35 = L91;
 SATTR(p,distributed,ret_val35);
 get_bool_self2 = ATTR(p,config);
 get_bool_s2 = ((STR) &THREADS);
 r20 = CONFIG1152920950(get_bool_self2, get_bool_s2);
 L100 = (r20==((CONFIG_DEF) NULL));
 L1011_=!(L100); 
 if (L1011_) {
  aget_self2 = r20;
  aget_i2 = 0;
  L102 = ATTR(aget_self2,expr);
  L1031_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L102,aget_i2); 
  ret_val38 = L1031_;
  L104 = ret_val38;
  L1051_=(STR)ARR((ARRAYSTR)L104,0); 
  L106 = L1051_;
  L99 = STR_is111530248(L106, ((STR) &true1));
 } else {
  L99 = FALSE;
 }
 ret_val37 = L99;
 SATTR(p,threads,ret_val37);
 get_bool_self3 = ATTR(p,config);
 get_bool_s3 = ((STR) &TRACE11);
 r21 = CONFIG1152920950(get_bool_self3, get_bool_s3);
 L108 = (r21==((CONFIG_DEF) NULL));
 L1091_=!(L108); 
 if (L1091_) {
  aget_self3 = r21;
  aget_i3 = 0;
  L110 = ATTR(aget_self3,expr);
  L1111_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L110,aget_i3); 
  ret_val40 = L1111_;
  L112 = ret_val40;
  L1131_=(STR)ARR((ARRAYSTR)L112,0); 
  L114 = L1131_;
  L107 = STR_is111530248(L114, ((STR) &true1));
 } else {
  L107 = FALSE;
 }
 ret_val39 = L107;
 SATTR(p,trace1,ret_val39);
 if (ATTR(p,zones)) {
  get_bool_self4 = ATTR(p,config);
  get_bool_s4 = ((STR) &ZONES1);
  r22 = CONFIG1152920950(get_bool_self4, get_bool_s4);
  L117 = (r22==((CONFIG_DEF) NULL));
  L1181_=!(L117); 
  if (L1181_) {
   aget_self4 = r22;
   aget_i4 = 0;
   L119 = ATTR(aget_self4,expr);
   L1201_=(ARRAYSTR)ARR((ARRAYARRAYSTR)L119,aget_i4); 
   ret_val42 = L1201_;
   L121 = ret_val42;
   L1221_=(STR)ARR((ARRAYSTR)L121,0); 
   L123 = L1221_;
   L116 = STR_is111530248(L123, ((STR) &true1));
  } else {
   L116 = FALSE;
  }
  ret_val41 = L116;
  L124 = ret_val41;
  L1251_=!(L124); 
  L115 = L1251_;
 } else {
  L115 = FALSE;
 }
 if (L115) {
  create_self22 = ((ERR1) NULL);
  ret_val43 = create_self22;
  plus_self8 = ret_val43;
  plus_s2 = ((dSTR) ((STR) &Zonesa995165525));
  stderr_self1 = ((FILE1) NULL);
  L127=ZALLOC(sizeof(struct FILE1_struct));
  if (L127==NULL) FATAL("Unable to allocate more memory");
  ((OB)L127)->header.tag=FILE1_tag;
  L126 = ((FILE1) L127);
  r23 = L126;
  
  SATTR(r23,fp,stderr);
  ret_val44 = r23;
  FILE_plus_dSTR(ret_val44, plus_s2);
  SATTR(p,err_seen,TRUE);
 }
 if (ATTR(p,print_version)) {
  v = CONFIG1397479570(ATTR(p,config), ((STR) &VERSION), 0);
  create_self23 = ((OUT) NULL);
  ret_val45 = create_self23;
  plus_self9 = ret_val45;
  plus_s3 = ((STR) &ICSISa2111359522);
  stdout_self2 = ((FILE1) NULL);
  L130=ZALLOC(sizeof(struct FILE1_struct));
  if (L130==NULL) FATAL("Unable to allocate more memory");
  ((OB)L130)->header.tag=FILE1_tag;
  L129 = ((FILE1) L130);
  r24 = L129;
  
  SATTR(r24,fp,stdout);
  ret_val47 = r24;
  FILE_plus_STR(ret_val47, plus_s3);
  ret_val46 = plus_self9;
  plus_self10 = ret_val46;
  plus_s4 = v;
  stdout_self3 = ((FILE1) NULL);
  L133=ZALLOC(sizeof(struct FILE1_struct));
  if (L133==NULL) FATAL("Unable to allocate more memory");
  ((OB)L133)->header.tag=FILE1_tag;
  L132 = ((FILE1) L133);
  r25 = L132;
  
  SATTR(r25,fp,stdout);
  ret_val49 = r25;
  FILE_plus_STR(ret_val49, plus_s4);
  ret_val48 = plus_self10;
  plus_self11 = ret_val48;
  plus_s5 = ((STR) &name9);
  stdout_self4 = ((FILE1) NULL);
  L136=ZALLOC(sizeof(struct FILE1_struct));
  if (L136==NULL) FATAL("Unable to allocate more memory");
  ((OB)L136)->header.tag=FILE1_tag;
  L135 = ((FILE1) L136);
  r26 = L135;
  
  SATTR(r26,fp,stdout);
  ret_val50 = r26;
  FILE_plus_STR(ret_val50, plus_s5);
  create_self24 = ((OUT) NULL);
  ret_val51 = create_self24;
  plus_self12 = ret_val51;
  plus_s6 = ((STR) &Defaultplatform);
  stdout_self5 = ((FILE1) NULL);
  L139=ZALLOC(sizeof(struct FILE1_struct));
  if (L139==NULL) FATAL("Unable to allocate more memory");
  ((OB)L139)->header.tag=FILE1_tag;
  L138 = ((FILE1) L139);
  r27 = L138;
  
  SATTR(r27,fp,stdout);
  ret_val53 = r27;
  FILE_plus_STR(ret_val53, plus_s6);
  ret_val52 = plus_self12;
  plus_self13 = ret_val52;
  plus_s7 = CONFIG1397479570(ATTR(p,config), ((STR) &DEFAULT_PLATFORM), 0);
  stdout_self6 = ((FILE1) NULL);
  L142=ZALLOC(sizeof(struct FILE1_struct));
  if (L142==NULL) FATAL("Unable to allocate more memory");
  ((OB)L142)->header.tag=FILE1_tag;
  L141 = ((FILE1) L142);
  r28 = L141;
  
  SATTR(r28,fp,stdout);
  ret_val55 = r28;
  FILE_plus_STR(ret_val55, plus_s7);
  ret_val54 = plus_self13;
  plus_self14 = ret_val54;
  L145=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L145==NULL) FATAL("Unable to allocate more memory");
  memset(L145,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L145)->header.tag=CHAR_tag;
  L144 = (dSTR)((CHAR_boxed) L145);
  ((CHAR_boxed) L144)->immutable_part = '\n';
  plus_s8 = L144;
  stdout_self7 = ((FILE1) NULL);
  L147=ZALLOC(sizeof(struct FILE1_struct));
  if (L147==NULL) FATAL("Unable to allocate more memory");
  ((OB)L147)->header.tag=FILE1_tag;
  L146 = ((FILE1) L147);
  r29 = L146;
  
  SATTR(r29,fp,stdout);
  ret_val56 = r29;
  FILE_plus_dSTR(ret_val56, plus_s8);
  create_self25 = ((OUT) NULL);
  ret_val57 = create_self25;
  plus_self15 = ret_val57;
  plus_s9 = ((STR) &Instal1200957049);
  stdout_self8 = ((FILE1) NULL);
  L150=ZALLOC(sizeof(struct FILE1_struct));
  if (L150==NULL) FATAL("Unable to allocate more memory");
  ((OB)L150)->header.tag=FILE1_tag;
  L149 = ((FILE1) L150);
  r30 = L149;
  
  SATTR(r30,fp,stdout);
  ret_val58 = r30;
  FILE_plus_STR(ret_val58, plus_s9);
  {
   BOOL f_L1521_ = TRUE;
   /* loop index variable */
   L61 = 0;
   eltb_if_first = TRUE;
   while (1) {
    if (eltb_if_first) {
     L21 = CONFIG1152920950(ATTR(p,config), ((STR) &PLATFORMS));
     eltb_self = L21;
     eltb_if_first = FALSE;
    }
    if (f_L1521_) {
     f_L1521_ = FALSE;
     L51 = ATTR(eltb_self,expr);
     L1521_br=L51==NULL?0:ASIZE((ARRAYARRAYSTR)L51); 
    }
    if(L61>=L1521_br)  goto after_loop1; 
    aL1521_=ARR((ARRAYARRAYSTR)L51,L61); 
    strs = aL1521_;
    {
     BOOL f_L1541_ = TRUE;
     /* loop index variable */
     L71 = 0;
     L31 = strs;
     L1541_br=L31==NULL?0:ASIZE((ARRAYSTR)L31); 
     while (1) {
      if(L71>=L1541_br)  goto after_loop2; 
      aL1541_=ARR((ARRAYSTR)L31,L71); 
      ps = aL1541_;
      create_self26 = ((OUT) NULL);
      ret_val60 = create_self26;
      plus_self16 = ret_val60;
      plus_s10 = ((STR) &name41);
      stdout_self9 = ((FILE1) NULL);
      L157=ZALLOC(sizeof(struct FILE1_struct));
      if (L157==NULL) FATAL("Unable to allocate more memory");
      ((OB)L157)->header.tag=FILE1_tag;
      L156 = ((FILE1) L157);
      r31 = L156;
      
      SATTR(r31,fp,stdout);
      ret_val62 = r31;
      FILE_plus_STR(ret_val62, plus_s10);
      ret_val61 = plus_self16;
      plus_self17 = ret_val61;
      plus_s11 = ps;
      stdout_self10 = ((FILE1) NULL);
      L160=ZALLOC(sizeof(struct FILE1_struct));
      if (L160==NULL) FATAL("Unable to allocate more memory");
      ((OB)L160)->header.tag=FILE1_tag;
      L159 = ((FILE1) L160);
      r32 = L159;
      
      SATTR(r32,fp,stdout);
      ret_val64 = r32;
      FILE_plus_STR(ret_val64, plus_s11);
      ret_val63 = plus_self17;
      plus_self18 = ret_val63;
      L163=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L163==NULL) FATAL("Unable to allocate more memory");
      memset(L163,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L163)->header.tag=CHAR_tag;
      L162 = (dSTR)((CHAR_boxed) L163);
      ((CHAR_boxed) L162)->immutable_part = '\n';
      plus_s12 = L162;
      stdout_self11 = ((FILE1) NULL);
      L165=ZALLOC(sizeof(struct FILE1_struct));
      if (L165==NULL) FATAL("Unable to allocate more memory");
      ((OB)L165)->header.tag=FILE1_tag;
      L164 = ((FILE1) L165);
      r33 = L164;
      
      SATTR(r33,fp,stdout);
      ret_val65 = r33;
      FILE_plus_dSTR(ret_val65, plus_s12);
      L1671_=INTPLUS(L71,1); 
      L71 = L1671_;
     }
    }
    after_loop2: ;
    L1681_=INTPLUS(L61,1); 
    L61 = L1681_;
   }
  }
  after_loop1: ;
  create_self27 = ((OUT) NULL);
  ret_val66 = create_self27;
  plus_self19 = ret_val66;
  plus_s13 = ((STR) &Homedirectory1);
  stdout_self12 = ((FILE1) NULL);
  L170=ZALLOC(sizeof(struct FILE1_struct));
  if (L170==NULL) FATAL("Unable to allocate more memory");
  ((OB)L170)->header.tag=FILE1_tag;
  L169 = ((FILE1) L170);
  r34 = L169;
  
  SATTR(r34,fp,stdout);
  ret_val68 = r34;
  FILE_plus_STR(ret_val68, plus_s13);
  ret_val67 = plus_self19;
  plus_self20 = ret_val67;
  plus_s14 = ATTR(p,home);
  stdout_self13 = ((FILE1) NULL);
  L173=ZALLOC(sizeof(struct FILE1_struct));
  if (L173==NULL) FATAL("Unable to allocate more memory");
  ((OB)L173)->header.tag=FILE1_tag;
  L172 = ((FILE1) L173);
  r35 = L172;
  
  SATTR(r35,fp,stdout);
  ret_val70 = r35;
  FILE_plus_STR(ret_val70, plus_s14);
  ret_val69 = plus_self20;
  plus_self21 = ret_val69;
  L176=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L176==NULL) FATAL("Unable to allocate more memory");
  memset(L176,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L176)->header.tag=CHAR_tag;
  L175 = (dSTR)((CHAR_boxed) L176);
  ((CHAR_boxed) L175)->immutable_part = '\n';
  plus_s15 = L175;
  stdout_self14 = ((FILE1) NULL);
  L178=ZALLOC(sizeof(struct FILE1_struct));
  if (L178==NULL) FATAL("Unable to allocate more memory");
  ((OB)L178)->header.tag=FILE1_tag;
  L177 = ((FILE1) L178);
  r36 = L177;
  
  SATTR(r36,fp,stdout);
  ret_val71 = r36;
  FILE_plus_dSTR(ret_val71, plus_s15);
 }
 sl = UNIX_g108606936(((UNIX) NULL), CONFIG1397479570(ATTR(p,config), ((STR) &LIBRARY), 0));
 if ((sl==((STR) NULL))) {
  L182=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(2)*sizeof(STR);
  L181=ZALLOC_BIG(L182);
  if (L181==NULL) FATAL("Unable to allocate more memory");
  ((OB)L181)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L181)->header.destroyed=0;
#endif

  L180 = ((ARRAYSTR) L181);
  L180->asize = 2;
  L180->arr_part[0] = ((STR) &name1);
  plus_self22 = ATTR(p,home);
  plus_sarg6 = ((STR) &name191);
  ret_val72 = STR_ap2004550586(plus_self22, plus_sarg6);
  plus_self23 = ret_val72;
  plus_sarg7 = CONFIG1397479570(ATTR(p,config), ((STR) &LIBRARY), 1);
  ret_val73 = STR_ap2004550586(plus_self23, plus_sarg7);
  L180->arr_part[1] = ret_val73;
  L183 = ATTR(p,get_options);
  (*dGET_O2036054845[TAG(L183)])(L183, L180);
 }
 else {
  L186=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(2)*sizeof(STR);
  L185=ZALLOC_BIG(L186);
  if (L185==NULL) FATAL("Unable to allocate more memory");
  ((OB)L185)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L185)->header.destroyed=0;
#endif

  L184 = ((ARRAYSTR) L185);
  L184->asize = 2;
  L184->arr_part[0] = ((STR) &name1);
  L184->arr_part[1] = sl;
  L187 = ATTR(p,get_options);
  (*dGET_O2036054845[TAG(L187)])(L187, L184);
 }
 if (ATTR(p,prolix)) {
  create_self28 = ((OUT) NULL);
  ret_val74 = create_self28;
  plus_self24 = ret_val74;
  L188 = ATTR(p,get_options);
  plus_s16 = (*dGET_O723107324[TAG(L188)])(L188);
  stdout_self15 = ((FILE1) NULL);
  L190=ZALLOC(sizeof(struct FILE1_struct));
  if (L190==NULL) FATAL("Unable to allocate more memory");
  ((OB)L190)->header.tag=FILE1_tag;
  L189 = ((FILE1) L190);
  r37 = L189;
  
  SATTR(r37,fp,stdout);
  ret_val76 = r37;
  FILE_plus_STR(ret_val76, plus_s16);
  ret_val75 = plus_self24;
  plus_self25 = ret_val75;
  plus_s17 = ((STR) &name163);
  stdout_self16 = ((FILE1) NULL);
  L193=ZALLOC(sizeof(struct FILE1_struct));
  if (L193==NULL) FATAL("Unable to allocate more memory");
  ((OB)L193)->header.tag=FILE1_tag;
  L192 = ((FILE1) L193);
  r38 = L192;
  
  SATTR(r38,fp,stdout);
  ret_val77 = r38;
  FILE_plus_STR(ret_val77, plus_s17);
 }
 if (ATTR(p,prolix)) {
  create_self29 = ((STAT1) NULL);
  L196=ZALLOC(sizeof(struct STAT1_struct));
  if (L196==NULL) FATAL("Unable to allocate more memory");
  ((OB)L196)->header.tag=STAT1_tag;
  L195 = ((STAT1) L196);
  res1 = L195;
  create_self30 = ((FMAPSTRINT) NULL);
  ret_val79 = ((FMAPSTRINT) NULL);
  SATTR(res1,map,ret_val79);
  ret_val78 = res1;
  SATTR(p,stat1,((dSTAT) ret_val78));
 }
 else {
  create_self31 = ((NULL_STAT) NULL);
  L198=ZALLOC_LEAF(sizeof(struct NULL_STAT_struct));
  if (L198==NULL) FATAL("Unable to allocate more memory");
  memset(L198,0,sizeof(struct NULL_STAT_struct));
  ((OB)L198)->header.tag=NULL_STAT_tag;
  L197 = ((NULL_STAT) L198);
  ret_val80 = L197;
  SATTR(p,stat1,((dSTAT) ret_val80));
 }
 if (ATTR(p,verbose)) {
  create_self32 = ((OUT) NULL);
  ret_val81 = create_self32;
  plus_self26 = ret_val81;
  L200=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L200==NULL) FATAL("Unable to allocate more memory");
  memset(L200,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L200)->header.tag=CHAR_tag;
  L199 = (dSTR)((CHAR_boxed) L200);
  ((CHAR_boxed) L199)->immutable_part = '(';
  plus_s18 = L199;
  stdout_self17 = ((FILE1) NULL);
  L202=ZALLOC(sizeof(struct FILE1_struct));
  if (L202==NULL) FATAL("Unable to allocate more memory");
  ((OB)L202)->header.tag=FILE1_tag;
  L201 = ((FILE1) L202);
  r39 = L201;
  
  SATTR(r39,fp,stdout);
  ret_val83 = r39;
  FILE_plus_dSTR(ret_val83, plus_s18);
  ret_val82 = plus_self26;
  plus_self27 = ret_val82;
  elapsed_self = ((TIME) NULL);
  L2041_=(FLTD)time(NULL); 
  L205 = L2041_;
  L206 = TIME_t854034019;
  L2071_=L205-L206; 
  ret_val84 = L2071_;
  L209=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
  if (L209==NULL) FATAL("Unable to allocate more memory");
  memset(L209,0,sizeof(struct FLTD_boxed_struct));
  ((OB)L209)->header.tag=FLTD_tag;
  L208 = (dSTR)((FLTD_boxed) L209);
  ((FLTD_boxed) L208)->immutable_part = ret_val84;
  plus_s19 = L208;
  stdout_self18 = ((FILE1) NULL);
  L211=ZALLOC(sizeof(struct FILE1_struct));
  if (L211==NULL) FATAL("Unable to allocate more memory");
  ((OB)L211)->header.tag=FILE1_tag;
  L210 = ((FILE1) L211);
  r40 = L210;
  
  SATTR(r40,fp,stdout);
  ret_val86 = r40;
  FILE_plus_dSTR(ret_val86, plus_s19);
  ret_val85 = plus_self27;
  plus_self28 = ret_val85;
  plus_s20 = ((STR) &seconds1);
  stdout_self19 = ((FILE1) NULL);
  L214=ZALLOC(sizeof(struct FILE1_struct));
  if (L214==NULL) FATAL("Unable to allocate more memory");
  ((OB)L214)->header.tag=FILE1_tag;
  L213 = ((FILE1) L214);
  r41 = L213;
  
  SATTR(r41,fp,stdout);
  ret_val87 = r41;
  FILE_plus_STR(ret_val87, plus_s20);
 }
 start_self1 = ((TIME) NULL);
 L2161_=(FLTD)time(NULL); 
 TIME_t854034019 = L2161_;
 if (ATTR(p,verbose)) {
  create_self33 = ((OUT) NULL);
  ret_val88 = create_self33;
  plus_self29 = ret_val88;
  plus_s21 = ((STR) &Parsin1988046872);
  stdout_self20 = ((FILE1) NULL);
  L218=ZALLOC(sizeof(struct FILE1_struct));
  if (L218==NULL) FATAL("Unable to allocate more memory");
  ((OB)L218)->header.tag=FILE1_tag;
  L217 = ((FILE1) L218);
  r42 = L217;
  
  SATTR(r42,fp,stdout);
  ret_val90 = r42;
  FILE_plus_STR(ret_val90, plus_s21);
  ret_val89 = plus_self29;
  flush_self2 = ret_val89;
  stdout_self21 = ((FILE1) NULL);
  L221=ZALLOC(sizeof(struct FILE1_struct));
  if (L221==NULL) FATAL("Unable to allocate more memory");
  ((OB)L221)->header.tag=FILE1_tag;
  L220 = ((FILE1) L221);
  r43 = L220;
  
  SATTR(r43,fp,stdout);
  ret_val91 = r43;
  flush_self3 = ret_val91;
  fflush(ATTR(flush_self3,fp));
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other4_0;
FSETSTR_eltbrSTR_frame noname2 = &other4_0;
  L41 = ATTR(p,sather_files);
  noname2->self = L41;
  noname2->state = 0;
  while (1) {
   L223 = FSETSTR_eltbrSTR(noname2);
   if (noname2->state == -1) {
    goto after_loop3;
   }
   fn = L223;
   L224 = ATTR(p,parse);
   (*dPARSE_parse_STR[TAG(L224)])(L224, fn);
  }
 }
 after_loop3: ;
 if (ATTR(p,verbose)) {
  create_self34 = ((OUT) NULL);
  ret_val92 = create_self34;
  plus_self30 = ret_val92;
  L226=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L226==NULL) FATAL("Unable to allocate more memory");
  memset(L226,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L226)->header.tag=CHAR_tag;
  L225 = (dSTR)((CHAR_boxed) L226);
  ((CHAR_boxed) L225)->immutable_part = '(';
  plus_s22 = L225;
  stdout_self22 = ((FILE1) NULL);
  L228=ZALLOC(sizeof(struct FILE1_struct));
  if (L228==NULL) FATAL("Unable to allocate more memory");
  ((OB)L228)->header.tag=FILE1_tag;
  L227 = ((FILE1) L228);
  r44 = L227;
  
  SATTR(r44,fp,stdout);
  ret_val94 = r44;
  FILE_plus_dSTR(ret_val94, plus_s22);
  ret_val93 = plus_self30;
  plus_self31 = ret_val93;
  elapsed_self1 = ((TIME) NULL);
  L2301_=(FLTD)time(NULL); 
  L231 = L2301_;
  L232 = TIME_t854034019;
  L2331_=L231-L232; 
  ret_val95 = L2331_;
  L235=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
  if (L235==NULL) FATAL("Unable to allocate more memory");
  memset(L235,0,sizeof(struct FLTD_boxed_struct));
  ((OB)L235)->header.tag=FLTD_tag;
  L234 = (dSTR)((FLTD_boxed) L235);
  ((FLTD_boxed) L234)->immutable_part = ret_val95;
  plus_s23 = L234;
  stdout_self23 = ((FILE1) NULL);
  L237=ZALLOC(sizeof(struct FILE1_struct));
  if (L237==NULL) FATAL("Unable to allocate more memory");
  ((OB)L237)->header.tag=FILE1_tag;
  L236 = ((FILE1) L237);
  r45 = L236;
  
  SATTR(r45,fp,stdout);
  ret_val97 = r45;
  FILE_plus_dSTR(ret_val97, plus_s23);
  ret_val96 = plus_self31;
  plus_self32 = ret_val96;
  plus_s24 = ((STR) &seconds1);
  stdout_self24 = ((FILE1) NULL);
  L240=ZALLOC(sizeof(struct FILE1_struct));
  if (L240==NULL) FATAL("Unable to allocate more memory");
  ((OB)L240)->header.tag=FILE1_tag;
  L239 = ((FILE1) L240);
  r46 = L239;
  
  SATTR(r46,fp,stdout);
  ret_val98 = r46;
  FILE_plus_STR(ret_val98, plus_s24);
 }
 if (ATTR(p,err_seen)) {
  if (ATTR(p,verbose)) {
   create_self35 = ((OUT) NULL);
   ret_val99 = create_self35;
   plus_self33 = ret_val99;
   L243=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L243==NULL) FATAL("Unable to allocate more memory");
   memset(L243,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L243)->header.tag=CHAR_tag;
   L242 = (dSTR)((CHAR_boxed) L243);
   ((CHAR_boxed) L242)->immutable_part = '\n';
   plus_s25 = L242;
   stdout_self25 = ((FILE1) NULL);
   L245=ZALLOC(sizeof(struct FILE1_struct));
   if (L245==NULL) FATAL("Unable to allocate more memory");
   ((OB)L245)->header.tag=FILE1_tag;
   L244 = ((FILE1) L245);
   r47 = L244;
   
   SATTR(r47,fp,stdout);
   ret_val100 = r47;
   FILE_plus_dSTR(ret_val100, plus_s25);
  }
  ret_val = 1;
  return ret_val;
 }
 if (ATTR(self,only_parse)) {
  if (ATTR(p,verbose)) {
   create_self36 = ((OUT) NULL);
   ret_val101 = create_self36;
   plus_self34 = ret_val101;
   L248=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L248==NULL) FATAL("Unable to allocate more memory");
   memset(L248,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L248)->header.tag=CHAR_tag;
   L247 = (dSTR)((CHAR_boxed) L248);
   ((CHAR_boxed) L247)->immutable_part = '\n';
   plus_s26 = L247;
   stdout_self26 = ((FILE1) NULL);
   L250=ZALLOC(sizeof(struct FILE1_struct));
   if (L250==NULL) FATAL("Unable to allocate more memory");
   ((OB)L250)->header.tag=FILE1_tag;
   L249 = ((FILE1) L250);
   r48 = L249;
   
   SATTR(r48,fp,stdout);
   ret_val102 = r48;
   FILE_plus_dSTR(ret_val102, plus_s26);
  }
  ret_val = 0;
  return ret_val;
 }
 start_self2 = ((TIME) NULL);
 L2521_=(FLTD)time(NULL); 
 TIME_t854034019 = L2521_;
 if (ATTR(p,verbose)) {
  create_self37 = ((OUT) NULL);
  ret_val103 = create_self37;
  plus_self35 = ret_val103;
  plus_s27 = ((STR) &Findin833949756);
  stdout_self27 = ((FILE1) NULL);
  L254=ZALLOC(sizeof(struct FILE1_struct));
  if (L254==NULL) FATAL("Unable to allocate more memory");
  ((OB)L254)->header.tag=FILE1_tag;
  L253 = ((FILE1) L254);
  r49 = L253;
  
  SATTR(r49,fp,stdout);
  ret_val105 = r49;
  FILE_plus_STR(ret_val105, plus_s27);
  ret_val104 = plus_self35;
  flush_self4 = ret_val104;
  stdout_self28 = ((FILE1) NULL);
  L257=ZALLOC(sizeof(struct FILE1_struct));
  if (L257==NULL) FATAL("Unable to allocate more memory");
  ((OB)L257)->header.tag=FILE1_tag;
  L256 = ((FILE1) L257);
  r50 = L256;
  
  SATTR(r50,fp,stdout);
  ret_val106 = r50;
  flush_self5 = ret_val106;
  fflush(ATTR(flush_self5,fp));
 }
 L259 = ATTR(p,find_types);
 (*dFIND_896803978[TAG(L259)])(L259);
 if (ATTR(p,verbose)) {
  create_self38 = ((OUT) NULL);
  ret_val107 = create_self38;
  plus_self36 = ret_val107;
  L261=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L261==NULL) FATAL("Unable to allocate more memory");
  memset(L261,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L261)->header.tag=CHAR_tag;
  L260 = (dSTR)((CHAR_boxed) L261);
  ((CHAR_boxed) L260)->immutable_part = '(';
  plus_s28 = L260;
  stdout_self29 = ((FILE1) NULL);
  L263=ZALLOC(sizeof(struct FILE1_struct));
  if (L263==NULL) FATAL("Unable to allocate more memory");
  ((OB)L263)->header.tag=FILE1_tag;
  L262 = ((FILE1) L263);
  r51 = L262;
  
  SATTR(r51,fp,stdout);
  ret_val109 = r51;
  FILE_plus_dSTR(ret_val109, plus_s28);
  ret_val108 = plus_self36;
  plus_self37 = ret_val108;
  elapsed_self2 = ((TIME) NULL);
  L2651_=(FLTD)time(NULL); 
  L266 = L2651_;
  L267 = TIME_t854034019;
  L2681_=L266-L267; 
  ret_val110 = L2681_;
  L270=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
  if (L270==NULL) FATAL("Unable to allocate more memory");
  memset(L270,0,sizeof(struct FLTD_boxed_struct));
  ((OB)L270)->header.tag=FLTD_tag;
  L269 = (dSTR)((FLTD_boxed) L270);
  ((FLTD_boxed) L269)->immutable_part = ret_val110;
  plus_s29 = L269;
  stdout_self30 = ((FILE1) NULL);
  L272=ZALLOC(sizeof(struct FILE1_struct));
  if (L272==NULL) FATAL("Unable to allocate more memory");
  ((OB)L272)->header.tag=FILE1_tag;
  L271 = ((FILE1) L272);
  r52 = L271;
  
  SATTR(r52,fp,stdout);
  ret_val112 = r52;
  FILE_plus_dSTR(ret_val112, plus_s29);
  ret_val1111 = plus_self37;
  plus_self38 = ret_val1111;
  plus_s30 = ((STR) &seconds1);
  stdout_self31 = ((FILE1) NULL);
  L275=ZALLOC(sizeof(struct FILE1_struct));
  if (L275==NULL) FATAL("Unable to allocate more memory");
  ((OB)L275)->header.tag=FILE1_tag;
  L274 = ((FILE1) L275);
  r53 = L274;
  
  SATTR(r53,fp,stdout);
  ret_val113 = r53;
  FILE_plus_STR(ret_val113, plus_s30);
 }
 if (ATTR(p,prolix)) {
  create_self39 = ((OUT) NULL);
  ret_val114 = create_self39;
  plus_self39 = ret_val114;
  L278=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L278==NULL) FATAL("Unable to allocate more memory");
  memset(L278,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L278)->header.tag=CHAR_tag;
  L277 = (dSTR)((CHAR_boxed) L278);
  ((CHAR_boxed) L277)->immutable_part = '\n';
  plus_s31 = L277;
  stdout_self32 = ((FILE1) NULL);
  L280=ZALLOC(sizeof(struct FILE1_struct));
  if (L280==NULL) FATAL("Unable to allocate more memory");
  ((OB)L280)->header.tag=FILE1_tag;
  L279 = ((FILE1) L280);
  r54 = L279;
  
  SATTR(r54,fp,stdout);
  ret_val116 = r54;
  FILE_plus_dSTR(ret_val116, plus_s31);
  ret_val115 = plus_self39;
  plus_self40 = ret_val115;
  str_self = PROG_A864188141(ATTR(p,as_tbl));
  clear_self = buf1;
  L282 = (clear_self==((FSTR) NULL));
  L2831_=!(L282); 
  if (L2831_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self;
  str_in_s = buf1;
  ret_val118 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val118;
  str_self1 = buf1;
  ret_val119 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val117 = ret_val119;
  plus_s32 = ret_val117;
  stdout_self33 = ((FILE1) NULL);
  L285=ZALLOC(sizeof(struct FILE1_struct));
  if (L285==NULL) FATAL("Unable to allocate more memory");
  ((OB)L285)->header.tag=FILE1_tag;
  L284 = ((FILE1) L285);
  r55 = L284;
  
  SATTR(r55,fp,stdout);
  ret_val121 = r55;
  FILE_plus_STR(ret_val121, plus_s32);
  ret_val120 = plus_self40;
  plus_self41 = ret_val120;
  plus_s33 = ((STR) &Classesfound);
  stdout_self34 = ((FILE1) NULL);
  L288=ZALLOC(sizeof(struct FILE1_struct));
  if (L288==NULL) FATAL("Unable to allocate more memory");
  ((OB)L288)->header.tag=FILE1_tag;
  L287 = ((FILE1) L288);
  r56 = L287;
  
  SATTR(r56,fp,stdout);
  ret_val122 = r56;
  FILE_plus_STR(ret_val122, plus_s33);
  PROG_AS_TBL_dump(ATTR(p,as_tbl));
 }
 if (ATTR(p,err_seen)) {
  ret_val = 2;
  return ret_val;
 }
 SATTR(p,no_new_types,TRUE);
 if (ATTR(p,check_generics)) {
  start_self3 = ((TIME) NULL);
  L2901_=(FLTD)time(NULL); 
  TIME_t854034019 = L2901_;
  if (ATTR(p,verbose)) {
   create_self40 = ((OUT) NULL);
   ret_val123 = create_self40;
   plus_self42 = ret_val123;
   plus_s34 = ((STR) &Genera1846288163);
   stdout_self35 = ((FILE1) NULL);
   L292=ZALLOC(sizeof(struct FILE1_struct));
   if (L292==NULL) FATAL("Unable to allocate more memory");
   ((OB)L292)->header.tag=FILE1_tag;
   L291 = ((FILE1) L292);
   r57 = L291;
   
   SATTR(r57,fp,stdout);
   ret_val125 = r57;
   FILE_plus_STR(ret_val125, plus_s34);
   ret_val124 = plus_self42;
   flush_self6 = ret_val124;
   stdout_self36 = ((FILE1) NULL);
   L295=ZALLOC(sizeof(struct FILE1_struct));
   if (L295==NULL) FATAL("Unable to allocate more memory");
   ((OB)L295)->header.tag=FILE1_tag;
   L294 = ((FILE1) L295);
   r58 = L294;
   
   SATTR(r58,fp,stdout);
   ret_val126 = r58;
   flush_self7 = ret_val126;
   fflush(ATTR(flush_self7,fp));
  }
  FIND_G1788878638(ATTR(p,find_generics));
  if (ATTR(p,verbose)) {
   create_self41 = ((OUT) NULL);
   ret_val127 = create_self41;
   plus_self43 = ret_val127;
   L298=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L298==NULL) FATAL("Unable to allocate more memory");
   memset(L298,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L298)->header.tag=CHAR_tag;
   L297 = (dSTR)((CHAR_boxed) L298);
   ((CHAR_boxed) L297)->immutable_part = '(';
   plus_s35 = L297;
   stdout_self37 = ((FILE1) NULL);
   L300=ZALLOC(sizeof(struct FILE1_struct));
   if (L300==NULL) FATAL("Unable to allocate more memory");
   ((OB)L300)->header.tag=FILE1_tag;
   L299 = ((FILE1) L300);
   r59 = L299;
   
   SATTR(r59,fp,stdout);
   ret_val129 = r59;
   FILE_plus_dSTR(ret_val129, plus_s35);
   ret_val128 = plus_self43;
   plus_self44 = ret_val128;
   elapsed_self3 = ((TIME) NULL);
   L3021_=(FLTD)time(NULL); 
   L303 = L3021_;
   L304 = TIME_t854034019;
   L3051_=L303-L304; 
   ret_val130 = L3051_;
   L307=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L307==NULL) FATAL("Unable to allocate more memory");
   memset(L307,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L307)->header.tag=FLTD_tag;
   L306 = (dSTR)((FLTD_boxed) L307);
   ((FLTD_boxed) L306)->immutable_part = ret_val130;
   plus_s36 = L306;
   stdout_self38 = ((FILE1) NULL);
   L309=ZALLOC(sizeof(struct FILE1_struct));
   if (L309==NULL) FATAL("Unable to allocate more memory");
   ((OB)L309)->header.tag=FILE1_tag;
   L308 = ((FILE1) L309);
   r60 = L308;
   
   SATTR(r60,fp,stdout);
   ret_val132 = r60;
   FILE_plus_dSTR(ret_val132, plus_s36);
   ret_val131 = plus_self44;
   plus_self45 = ret_val131;
   plus_s37 = ((STR) &seconds1);
   stdout_self39 = ((FILE1) NULL);
   L312=ZALLOC(sizeof(struct FILE1_struct));
   if (L312==NULL) FATAL("Unable to allocate more memory");
   ((OB)L312)->header.tag=FILE1_tag;
   L311 = ((FILE1) L312);
   r61 = L311;
   
   SATTR(r61,fp,stdout);
   ret_val133 = r61;
   FILE_plus_STR(ret_val133, plus_s37);
  }
 }
 start_self4 = ((TIME) NULL);
 L3141_=(FLTD)time(NULL); 
 TIME_t854034019 = L3141_;
 if (ATTR(p,verbose)) {
  create_self42 = ((OUT) NULL);
  ret_val134 = create_self42;
  plus_self46 = ret_val134;
  plus_s38 = ((STR) &Typegr495187748);
  stdout_self40 = ((FILE1) NULL);
  L316=ZALLOC(sizeof(struct FILE1_struct));
  if (L316==NULL) FATAL("Unable to allocate more memory");
  ((OB)L316)->header.tag=FILE1_tag;
  L315 = ((FILE1) L316);
  r62 = L315;
  
  SATTR(r62,fp,stdout);
  ret_val136 = r62;
  FILE_plus_STR(ret_val136, plus_s38);
  ret_val135 = plus_self46;
  flush_self8 = ret_val135;
  stdout_self41 = ((FILE1) NULL);
  L319=ZALLOC(sizeof(struct FILE1_struct));
  if (L319==NULL) FATAL("Unable to allocate more memory");
  ((OB)L319)->header.tag=FILE1_tag;
  L318 = ((FILE1) L319);
  r63 = L318;
  
  SATTR(r63,fp,stdout);
  ret_val137 = r63;
  flush_self9 = ret_val137;
  fflush(ATTR(flush_self9,fp));
 }
 L321 = ATTR(p,build_type_graph);
 (*dBUILD408006298[TAG(L321)])(L321);
 if (ATTR(p,err_seen)) {
  if (ATTR(p,verbose)) {
   create_self43 = ((OUT) NULL);
   ret_val138 = create_self43;
   plus_self47 = ret_val138;
   L323=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L323==NULL) FATAL("Unable to allocate more memory");
   memset(L323,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L323)->header.tag=CHAR_tag;
   L322 = (dSTR)((CHAR_boxed) L323);
   ((CHAR_boxed) L322)->immutable_part = '(';
   plus_s39 = L322;
   stdout_self42 = ((FILE1) NULL);
   L325=ZALLOC(sizeof(struct FILE1_struct));
   if (L325==NULL) FATAL("Unable to allocate more memory");
   ((OB)L325)->header.tag=FILE1_tag;
   L324 = ((FILE1) L325);
   r64 = L324;
   
   SATTR(r64,fp,stdout);
   ret_val140 = r64;
   FILE_plus_dSTR(ret_val140, plus_s39);
   ret_val139 = plus_self47;
   plus_self48 = ret_val139;
   elapsed_self4 = ((TIME) NULL);
   L3271_=(FLTD)time(NULL); 
   L328 = L3271_;
   L329 = TIME_t854034019;
   L3301_=L328-L329; 
   ret_val141 = L3301_;
   L332=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L332==NULL) FATAL("Unable to allocate more memory");
   memset(L332,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L332)->header.tag=FLTD_tag;
   L331 = (dSTR)((FLTD_boxed) L332);
   ((FLTD_boxed) L331)->immutable_part = ret_val141;
   plus_s40 = L331;
   stdout_self43 = ((FILE1) NULL);
   L334=ZALLOC(sizeof(struct FILE1_struct));
   if (L334==NULL) FATAL("Unable to allocate more memory");
   ((OB)L334)->header.tag=FILE1_tag;
   L333 = ((FILE1) L334);
   r65 = L333;
   
   SATTR(r65,fp,stdout);
   ret_val143 = r65;
   FILE_plus_dSTR(ret_val143, plus_s40);
   ret_val142 = plus_self48;
   plus_self49 = ret_val142;
   plus_s41 = ((STR) &seconds2);
   stdout_self44 = ((FILE1) NULL);
   L337=ZALLOC(sizeof(struct FILE1_struct));
   if (L337==NULL) FATAL("Unable to allocate more memory");
   ((OB)L337)->header.tag=FILE1_tag;
   L336 = ((FILE1) L337);
   r66 = L336;
   
   SATTR(r66,fp,stdout);
   ret_val144 = r66;
   FILE_plus_STR(ret_val144, plus_s41);
  }
  ret_val = 3;
  return ret_val;
 }
 L339 = ATTR(p,check_ifc_confor);
 (*dCHECK450012483[TAG(L339)])(L339);
 if (ATTR(p,err_seen)) {
  if (ATTR(p,verbose)) {
   create_self44 = ((OUT) NULL);
   ret_val145 = create_self44;
   plus_self50 = ret_val145;
   L341=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L341==NULL) FATAL("Unable to allocate more memory");
   memset(L341,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L341)->header.tag=CHAR_tag;
   L340 = (dSTR)((CHAR_boxed) L341);
   ((CHAR_boxed) L340)->immutable_part = '(';
   plus_s42 = L340;
   stdout_self45 = ((FILE1) NULL);
   L343=ZALLOC(sizeof(struct FILE1_struct));
   if (L343==NULL) FATAL("Unable to allocate more memory");
   ((OB)L343)->header.tag=FILE1_tag;
   L342 = ((FILE1) L343);
   r67 = L342;
   
   SATTR(r67,fp,stdout);
   ret_val147 = r67;
   FILE_plus_dSTR(ret_val147, plus_s42);
   ret_val146 = plus_self50;
   plus_self51 = ret_val146;
   elapsed_self5 = ((TIME) NULL);
   L3451_=(FLTD)time(NULL); 
   L346 = L3451_;
   L347 = TIME_t854034019;
   L3481_=L346-L347; 
   ret_val148 = L3481_;
   L350=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L350==NULL) FATAL("Unable to allocate more memory");
   memset(L350,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L350)->header.tag=FLTD_tag;
   L349 = (dSTR)((FLTD_boxed) L350);
   ((FLTD_boxed) L349)->immutable_part = ret_val148;
   plus_s43 = L349;
   stdout_self46 = ((FILE1) NULL);
   L352=ZALLOC(sizeof(struct FILE1_struct));
   if (L352==NULL) FATAL("Unable to allocate more memory");
   ((OB)L352)->header.tag=FILE1_tag;
   L351 = ((FILE1) L352);
   r68 = L351;
   
   SATTR(r68,fp,stdout);
   ret_val150 = r68;
   FILE_plus_dSTR(ret_val150, plus_s43);
   ret_val149 = plus_self51;
   plus_self52 = ret_val149;
   plus_s44 = ((STR) &seconds2);
   stdout_self47 = ((FILE1) NULL);
   L355=ZALLOC(sizeof(struct FILE1_struct));
   if (L355==NULL) FATAL("Unable to allocate more memory");
   ((OB)L355)->header.tag=FILE1_tag;
   L354 = ((FILE1) L355);
   r69 = L354;
   
   SATTR(r69,fp,stdout);
   ret_val151 = r69;
   FILE_plus_STR(ret_val151, plus_s44);
  }
  ret_val = 4;
  return ret_val;
 }
 L357 = ATTR(p,get_main_sig);
 (*dGET_M1653483544[TAG(L357)])(L357, ATTR(p,main_class));
 if (ATTR(p,err_seen)) {
  if (ATTR(p,verbose)) {
   create_self45 = ((OUT) NULL);
   ret_val152 = create_self45;
   plus_self53 = ret_val152;
   L359=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L359==NULL) FATAL("Unable to allocate more memory");
   memset(L359,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L359)->header.tag=CHAR_tag;
   L358 = (dSTR)((CHAR_boxed) L359);
   ((CHAR_boxed) L358)->immutable_part = '(';
   plus_s45 = L358;
   stdout_self48 = ((FILE1) NULL);
   L361=ZALLOC(sizeof(struct FILE1_struct));
   if (L361==NULL) FATAL("Unable to allocate more memory");
   ((OB)L361)->header.tag=FILE1_tag;
   L360 = ((FILE1) L361);
   r70 = L360;
   
   SATTR(r70,fp,stdout);
   ret_val154 = r70;
   FILE_plus_dSTR(ret_val154, plus_s45);
   ret_val153 = plus_self53;
   plus_self54 = ret_val153;
   elapsed_self6 = ((TIME) NULL);
   L3631_=(FLTD)time(NULL); 
   L364 = L3631_;
   L365 = TIME_t854034019;
   L3661_=L364-L365; 
   ret_val155 = L3661_;
   L368=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L368==NULL) FATAL("Unable to allocate more memory");
   memset(L368,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L368)->header.tag=FLTD_tag;
   L367 = (dSTR)((FLTD_boxed) L368);
   ((FLTD_boxed) L367)->immutable_part = ret_val155;
   plus_s46 = L367;
   stdout_self49 = ((FILE1) NULL);
   L370=ZALLOC(sizeof(struct FILE1_struct));
   if (L370==NULL) FATAL("Unable to allocate more memory");
   ((OB)L370)->header.tag=FILE1_tag;
   L369 = ((FILE1) L370);
   r71 = L369;
   
   SATTR(r71,fp,stdout);
   ret_val157 = r71;
   FILE_plus_dSTR(ret_val157, plus_s46);
   ret_val156 = plus_self54;
   plus_self55 = ret_val156;
   plus_s47 = ((STR) &seconds2);
   stdout_self50 = ((FILE1) NULL);
   L373=ZALLOC(sizeof(struct FILE1_struct));
   if (L373==NULL) FATAL("Unable to allocate more memory");
   ((OB)L373)->header.tag=FILE1_tag;
   L372 = ((FILE1) L373);
   r72 = L372;
   
   SATTR(r72,fp,stdout);
   ret_val158 = r72;
   FILE_plus_STR(ret_val158, plus_s47);
  }
  ret_val = 5;
  return ret_val;
 }
 L375 = ATTR(cgen,only_check);
 L3761_=!(L375); 
 if (L3761_) {
  L377 = ATTR(p,inliner);
  (*dINLINE_init[TAG(L377)])(L377);
  L378 = ATTR(p,optimizer);
  (*dOPTIMIZE_init[TAG(L378)])(L378);
  if (ATTR(p,err_seen)) {
   if (ATTR(p,verbose)) {
    create_self46 = ((OUT) NULL);
    ret_val159 = create_self46;
    plus_self56 = ret_val159;
    L380=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L380==NULL) FATAL("Unable to allocate more memory");
    memset(L380,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L380)->header.tag=CHAR_tag;
    L379 = (dSTR)((CHAR_boxed) L380);
    ((CHAR_boxed) L379)->immutable_part = '(';
    plus_s48 = L379;
    stdout_self51 = ((FILE1) NULL);
    L382=ZALLOC(sizeof(struct FILE1_struct));
    if (L382==NULL) FATAL("Unable to allocate more memory");
    ((OB)L382)->header.tag=FILE1_tag;
    L381 = ((FILE1) L382);
    r73 = L381;
    
    SATTR(r73,fp,stdout);
    ret_val161 = r73;
    FILE_plus_dSTR(ret_val161, plus_s48);
    ret_val160 = plus_self56;
    plus_self57 = ret_val160;
    elapsed_self7 = ((TIME) NULL);
    L3841_=(FLTD)time(NULL); 
    L385 = L3841_;
    L386 = TIME_t854034019;
    L3871_=L385-L386; 
    ret_val162 = L3871_;
    L389=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
    if (L389==NULL) FATAL("Unable to allocate more memory");
    memset(L389,0,sizeof(struct FLTD_boxed_struct));
    ((OB)L389)->header.tag=FLTD_tag;
    L388 = (dSTR)((FLTD_boxed) L389);
    ((FLTD_boxed) L388)->immutable_part = ret_val162;
    plus_s49 = L388;
    stdout_self52 = ((FILE1) NULL);
    L391=ZALLOC(sizeof(struct FILE1_struct));
    if (L391==NULL) FATAL("Unable to allocate more memory");
    ((OB)L391)->header.tag=FILE1_tag;
    L390 = ((FILE1) L391);
    r74 = L390;
    
    SATTR(r74,fp,stdout);
    ret_val164 = r74;
    FILE_plus_dSTR(ret_val164, plus_s49);
    ret_val163 = plus_self57;
    plus_self58 = ret_val163;
    plus_s50 = ((STR) &seconds2);
    stdout_self53 = ((FILE1) NULL);
    L394=ZALLOC(sizeof(struct FILE1_struct));
    if (L394==NULL) FATAL("Unable to allocate more memory");
    ((OB)L394)->header.tag=FILE1_tag;
    L393 = ((FILE1) L394);
    r75 = L393;
    
    SATTR(r75,fp,stdout);
    ret_val165 = r75;
    FILE_plus_STR(ret_val165, plus_s50);
   }
   ret_val = 6;
   return ret_val;
  }
 }
 if (ATTR(p,verbose)) {
  create_self47 = ((OUT) NULL);
  ret_val166 = create_self47;
  plus_self59 = ret_val166;
  L397=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L397==NULL) FATAL("Unable to allocate more memory");
  memset(L397,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L397)->header.tag=CHAR_tag;
  L396 = (dSTR)((CHAR_boxed) L397);
  ((CHAR_boxed) L396)->immutable_part = '(';
  plus_s51 = L396;
  stdout_self54 = ((FILE1) NULL);
  L399=ZALLOC(sizeof(struct FILE1_struct));
  if (L399==NULL) FATAL("Unable to allocate more memory");
  ((OB)L399)->header.tag=FILE1_tag;
  L398 = ((FILE1) L399);
  r76 = L398;
  
  SATTR(r76,fp,stdout);
  ret_val168 = r76;
  FILE_plus_dSTR(ret_val168, plus_s51);
  ret_val167 = plus_self59;
  plus_self60 = ret_val167;
  elapsed_self8 = ((TIME) NULL);
  L4011_=(FLTD)time(NULL); 
  L402 = L4011_;
  L403 = TIME_t854034019;
  L4041_=L402-L403; 
  ret_val169 = L4041_;
  L406=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
  if (L406==NULL) FATAL("Unable to allocate more memory");
  memset(L406,0,sizeof(struct FLTD_boxed_struct));
  ((OB)L406)->header.tag=FLTD_tag;
  L405 = (dSTR)((FLTD_boxed) L406);
  ((FLTD_boxed) L405)->immutable_part = ret_val169;
  plus_s52 = L405;
  stdout_self55 = ((FILE1) NULL);
  L408=ZALLOC(sizeof(struct FILE1_struct));
  if (L408==NULL) FATAL("Unable to allocate more memory");
  ((OB)L408)->header.tag=FILE1_tag;
  L407 = ((FILE1) L408);
  r77 = L407;
  
  SATTR(r77,fp,stdout);
  ret_val171 = r77;
  FILE_plus_dSTR(ret_val171, plus_s52);
  ret_val170 = plus_self60;
  plus_self61 = ret_val170;
  plus_s53 = ((STR) &seconds1);
  stdout_self56 = ((FILE1) NULL);
  L411=ZALLOC(sizeof(struct FILE1_struct));
  if (L411==NULL) FATAL("Unable to allocate more memory");
  ((OB)L411)->header.tag=FILE1_tag;
  L410 = ((FILE1) L411);
  r78 = L410;
  
  SATTR(r78,fp,stdout);
  ret_val172 = r78;
  FILE_plus_STR(ret_val172, plus_s53);
 }
 start_self5 = ((TIME) NULL);
 L4131_=(FLTD)time(NULL); 
 TIME_t854034019 = L4131_;
 if (ATTR(p,verbose)) {
  create_self48 = ((OUT) NULL);
  ret_val173 = create_self48;
  plus_self62 = ret_val173;
  plus_s54 = ((STR) &Typech1407750582);
  stdout_self57 = ((FILE1) NULL);
  L415=ZALLOC(sizeof(struct FILE1_struct));
  if (L415==NULL) FATAL("Unable to allocate more memory");
  ((OB)L415)->header.tag=FILE1_tag;
  L414 = ((FILE1) L415);
  r79 = L414;
  
  SATTR(r79,fp,stdout);
  ret_val175 = r79;
  FILE_plus_STR(ret_val175, plus_s54);
  ret_val174 = plus_self62;
  flush_self10 = ret_val174;
  stdout_self58 = ((FILE1) NULL);
  L418=ZALLOC(sizeof(struct FILE1_struct));
  if (L418==NULL) FATAL("Unable to allocate more memory");
  ((OB)L418)->header.tag=FILE1_tag;
  L417 = ((FILE1) L418);
  r80 = L417;
  
  SATTR(r80,fp,stdout);
  ret_val176 = r80;
  flush_self11 = ret_val176;
  fflush(ATTR(flush_self11,fp));
 }
 L420 = ATTR(p,generate_am);
 (*dGENER1765432365[TAG(L420)])(L420);
 if (ATTR(p,verbose)) {
  create_self49 = ((OUT) NULL);
  ret_val177 = create_self49;
  plus_self63 = ret_val177;
  L422=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L422==NULL) FATAL("Unable to allocate more memory");
  memset(L422,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L422)->header.tag=CHAR_tag;
  L421 = (dSTR)((CHAR_boxed) L422);
  ((CHAR_boxed) L421)->immutable_part = '(';
  plus_s55 = L421;
  stdout_self59 = ((FILE1) NULL);
  L424=ZALLOC(sizeof(struct FILE1_struct));
  if (L424==NULL) FATAL("Unable to allocate more memory");
  ((OB)L424)->header.tag=FILE1_tag;
  L423 = ((FILE1) L424);
  r81 = L423;
  
  SATTR(r81,fp,stdout);
  ret_val179 = r81;
  FILE_plus_dSTR(ret_val179, plus_s55);
  ret_val178 = plus_self63;
  plus_self64 = ret_val178;
  elapsed_self9 = ((TIME) NULL);
  L4261_=(FLTD)time(NULL); 
  L427 = L4261_;
  L428 = TIME_t854034019;
  L4291_=L427-L428; 
  ret_val180 = L4291_;
  L431=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
  if (L431==NULL) FATAL("Unable to allocate more memory");
  memset(L431,0,sizeof(struct FLTD_boxed_struct));
  ((OB)L431)->header.tag=FLTD_tag;
  L430 = (dSTR)((FLTD_boxed) L431);
  ((FLTD_boxed) L430)->immutable_part = ret_val180;
  plus_s56 = L430;
  stdout_self60 = ((FILE1) NULL);
  L433=ZALLOC(sizeof(struct FILE1_struct));
  if (L433==NULL) FATAL("Unable to allocate more memory");
  ((OB)L433)->header.tag=FILE1_tag;
  L432 = ((FILE1) L433);
  r82 = L432;
  
  SATTR(r82,fp,stdout);
  ret_val182 = r82;
  FILE_plus_dSTR(ret_val182, plus_s56);
  ret_val181 = plus_self64;
  plus_self65 = ret_val181;
  plus_s57 = ((STR) &seconds1);
  stdout_self61 = ((FILE1) NULL);
  L436=ZALLOC(sizeof(struct FILE1_struct));
  if (L436==NULL) FATAL("Unable to allocate more memory");
  ((OB)L436)->header.tag=FILE1_tag;
  L435 = ((FILE1) L436);
  r83 = L435;
  
  SATTR(r83,fp,stdout);
  ret_val183 = r83;
  FILE_plus_STR(ret_val183, plus_s57);
 }
 if (ATTR(p,show_tr)) {
  PROG_A148914482(ATTR(p,as_tbl), p);
 }
 if (ATTR(p,err_seen)) {
  if (ATTR(p,verbose)) {
   create_self50 = ((OUT) NULL);
   ret_val184 = create_self50;
   plus_self66 = ret_val184;
   L439=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L439==NULL) FATAL("Unable to allocate more memory");
   memset(L439,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L439)->header.tag=CHAR_tag;
   L438 = (dSTR)((CHAR_boxed) L439);
   ((CHAR_boxed) L438)->immutable_part = '\n';
   plus_s58 = L438;
   stdout_self62 = ((FILE1) NULL);
   L441=ZALLOC(sizeof(struct FILE1_struct));
   if (L441==NULL) FATAL("Unable to allocate more memory");
   ((OB)L441)->header.tag=FILE1_tag;
   L440 = ((FILE1) L441);
   r84 = L440;
   
   SATTR(r84,fp,stdout);
   ret_val185 = r84;
   FILE_plus_dSTR(ret_val185, plus_s58);
  }
  ret_val = 7;
  return ret_val;
 }
 L443 = ATTR(cgen,only_check);
 L4441_=!(L443); 
 if (L4441_) {
  start_self6 = ((TIME) NULL);
  L4451_=(FLTD)time(NULL); 
  TIME_t854034019 = L4451_;
  if (ATTR(p,verbose)) {
   if (ATTR(cgen,only_C)) {
    create_self51 = ((OUT) NULL);
    ret_val186 = create_self51;
    plus_self67 = ret_val186;
    plus_s59 = ((STR) &Header575149500);
    stdout_self63 = ((FILE1) NULL);
    L447=ZALLOC(sizeof(struct FILE1_struct));
    if (L447==NULL) FATAL("Unable to allocate more memory");
    ((OB)L447)->header.tag=FILE1_tag;
    L446 = ((FILE1) L447);
    r85 = L446;
    
    SATTR(r85,fp,stdout);
    ret_val188 = r85;
    FILE_plus_STR(ret_val188, plus_s59);
    ret_val187 = plus_self67;
    flush_self12 = ret_val187;
    stdout_self64 = ((FILE1) NULL);
    L450=ZALLOC(sizeof(struct FILE1_struct));
    if (L450==NULL) FATAL("Unable to allocate more memory");
    ((OB)L450)->header.tag=FILE1_tag;
    L449 = ((FILE1) L450);
    r86 = L449;
    
    SATTR(r86,fp,stdout);
    ret_val189 = r86;
    flush_self13 = ret_val189;
    fflush(ATTR(flush_self13,fp));
   }
   else {
    create_self52 = ((OUT) NULL);
    ret_val190 = create_self52;
    plus_self68 = ret_val190;
    plus_s60 = ((STR) &Header164288235);
    stdout_self65 = ((FILE1) NULL);
    L453=ZALLOC(sizeof(struct FILE1_struct));
    if (L453==NULL) FATAL("Unable to allocate more memory");
    ((OB)L453)->header.tag=FILE1_tag;
    L452 = ((FILE1) L453);
    r87 = L452;
    
    SATTR(r87,fp,stdout);
    ret_val192 = r87;
    FILE_plus_STR(ret_val192, plus_s60);
    ret_val191 = plus_self68;
    flush_self14 = ret_val191;
    stdout_self66 = ((FILE1) NULL);
    L456=ZALLOC(sizeof(struct FILE1_struct));
    if (L456==NULL) FATAL("Unable to allocate more memory");
    ((OB)L456)->header.tag=FILE1_tag;
    L455 = ((FILE1) L456);
    r88 = L455;
    
    SATTR(r88,fp,stdout);
    ret_val193 = r88;
    flush_self15 = ret_val193;
    fflush(ATTR(flush_self15,fp));
   }
  }
  L458 = ATTR(p,optimizer);
  (*dOPTIM311966595[TAG(L458)])(L458);
  if (ATTR(p,verbose)) {
   create_self53 = ((OUT) NULL);
   ret_val194 = create_self53;
   plus_self69 = ret_val194;
   plus_s61 = ((STR) &name24);
   stdout_self67 = ((FILE1) NULL);
   L460=ZALLOC(sizeof(struct FILE1_struct));
   if (L460==NULL) FATAL("Unable to allocate more memory");
   ((OB)L460)->header.tag=FILE1_tag;
   L459 = ((FILE1) L460);
   r89 = L459;
   
   SATTR(r89,fp,stdout);
   ret_val196 = r89;
   FILE_plus_STR(ret_val196, plus_s61);
   ret_val195 = plus_self69;
   plus_self70 = ret_val195;
   elapsed_self10 = ((TIME) NULL);
   L4631_=(FLTD)time(NULL); 
   L464 = L4631_;
   L465 = TIME_t854034019;
   L4661_=L464-L465; 
   ret_val197 = L4661_;
   L468=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L468==NULL) FATAL("Unable to allocate more memory");
   memset(L468,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L468)->header.tag=FLTD_tag;
   L467 = (dSTR)((FLTD_boxed) L468);
   ((FLTD_boxed) L467)->immutable_part = ret_val197;
   plus_s62 = L467;
   stdout_self68 = ((FILE1) NULL);
   L470=ZALLOC(sizeof(struct FILE1_struct));
   if (L470==NULL) FATAL("Unable to allocate more memory");
   ((OB)L470)->header.tag=FILE1_tag;
   L469 = ((FILE1) L470);
   r90 = L469;
   
   SATTR(r90,fp,stdout);
   ret_val199 = r90;
   FILE_plus_dSTR(ret_val199, plus_s62);
   ret_val198 = plus_self70;
   plus_self71 = ret_val198;
   plus_s63 = ((STR) &seconds1);
   stdout_self69 = ((FILE1) NULL);
   L473=ZALLOC(sizeof(struct FILE1_struct));
   if (L473==NULL) FATAL("Unable to allocate more memory");
   ((OB)L473)->header.tag=FILE1_tag;
   L472 = ((FILE1) L473);
   r91 = L472;
   
   SATTR(r91,fp,stdout);
   ret_val200 = r91;
   FILE_plus_STR(ret_val200, plus_s63);
  }
  if (ATTR(p,err_seen)) {
   if (ATTR(p,verbose)) {
    create_self54 = ((OUT) NULL);
    ret_val201 = create_self54;
    plus_self72 = ret_val201;
    L476=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L476==NULL) FATAL("Unable to allocate more memory");
    memset(L476,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L476)->header.tag=CHAR_tag;
    L475 = (dSTR)((CHAR_boxed) L476);
    ((CHAR_boxed) L475)->immutable_part = '\n';
    plus_s64 = L475;
    stdout_self70 = ((FILE1) NULL);
    L478=ZALLOC(sizeof(struct FILE1_struct));
    if (L478==NULL) FATAL("Unable to allocate more memory");
    ((OB)L478)->header.tag=FILE1_tag;
    L477 = ((FILE1) L478);
    r92 = L477;
    
    SATTR(r92,fp,stdout);
    ret_val202 = r92;
    FILE_plus_dSTR(ret_val202, plus_s64);
   }
   ret_val = 8;
   return ret_val;
  }
 }
 if (ATTR(p,check_generics)) {
  start_self7 = ((TIME) NULL);
  L4801_=(FLTD)time(NULL); 
  TIME_t854034019 = L4801_;
  if (ATTR(p,verbose)) {
   create_self55 = ((OUT) NULL);
   ret_val203 = create_self55;
   plus_self73 = ret_val203;
   plus_s65 = ((STR) &Checki135017201);
   stdout_self71 = ((FILE1) NULL);
   L482=ZALLOC(sizeof(struct FILE1_struct));
   if (L482==NULL) FATAL("Unable to allocate more memory");
   ((OB)L482)->header.tag=FILE1_tag;
   L481 = ((FILE1) L482);
   r93 = L481;
   
   SATTR(r93,fp,stdout);
   ret_val205 = r93;
   FILE_plus_STR(ret_val205, plus_s65);
   ret_val204 = plus_self73;
   flush_self16 = ret_val204;
   stdout_self72 = ((FILE1) NULL);
   L485=ZALLOC(sizeof(struct FILE1_struct));
   if (L485==NULL) FATAL("Unable to allocate more memory");
   ((OB)L485)->header.tag=FILE1_tag;
   L484 = ((FILE1) L485);
   r94 = L484;
   
   SATTR(r94,fp,stdout);
   ret_val206 = r94;
   flush_self17 = ret_val206;
   fflush(ATTR(flush_self17,fp));
  }
  CHECK_452836548(ATTR(p,check_gen));
  if (ATTR(p,verbose)) {
   create_self56 = ((OUT) NULL);
   ret_val207 = create_self56;
   plus_self74 = ret_val207;
   L488=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L488==NULL) FATAL("Unable to allocate more memory");
   memset(L488,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L488)->header.tag=CHAR_tag;
   L487 = (dSTR)((CHAR_boxed) L488);
   ((CHAR_boxed) L487)->immutable_part = '(';
   plus_s66 = L487;
   stdout_self73 = ((FILE1) NULL);
   L490=ZALLOC(sizeof(struct FILE1_struct));
   if (L490==NULL) FATAL("Unable to allocate more memory");
   ((OB)L490)->header.tag=FILE1_tag;
   L489 = ((FILE1) L490);
   r95 = L489;
   
   SATTR(r95,fp,stdout);
   ret_val209 = r95;
   FILE_plus_dSTR(ret_val209, plus_s66);
   ret_val208 = plus_self74;
   plus_self75 = ret_val208;
   elapsed_self11 = ((TIME) NULL);
   L4921_=(FLTD)time(NULL); 
   L493 = L4921_;
   L494 = TIME_t854034019;
   L4951_=L493-L494; 
   ret_val210 = L4951_;
   L497=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L497==NULL) FATAL("Unable to allocate more memory");
   memset(L497,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L497)->header.tag=FLTD_tag;
   L496 = (dSTR)((FLTD_boxed) L497);
   ((FLTD_boxed) L496)->immutable_part = ret_val210;
   plus_s67 = L496;
   stdout_self74 = ((FILE1) NULL);
   L499=ZALLOC(sizeof(struct FILE1_struct));
   if (L499==NULL) FATAL("Unable to allocate more memory");
   ((OB)L499)->header.tag=FILE1_tag;
   L498 = ((FILE1) L499);
   r96 = L498;
   
   SATTR(r96,fp,stdout);
   ret_val212 = r96;
   FILE_plus_dSTR(ret_val212, plus_s67);
   ret_val211 = plus_self75;
   plus_self76 = ret_val211;
   plus_s68 = ((STR) &seconds1);
   stdout_self75 = ((FILE1) NULL);
   L502=ZALLOC(sizeof(struct FILE1_struct));
   if (L502==NULL) FATAL("Unable to allocate more memory");
   ((OB)L502)->header.tag=FILE1_tag;
   L501 = ((FILE1) L502);
   r97 = L501;
   
   SATTR(r97,fp,stdout);
   ret_val213 = r97;
   FILE_plus_STR(ret_val213, plus_s68);
  }
  if (ATTR(p,err_seen)) {
   if (ATTR(p,verbose)) {
    create_self57 = ((OUT) NULL);
    ret_val214 = create_self57;
    plus_self77 = ret_val214;
    L505=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L505==NULL) FATAL("Unable to allocate more memory");
    memset(L505,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L505)->header.tag=CHAR_tag;
    L504 = (dSTR)((CHAR_boxed) L505);
    ((CHAR_boxed) L504)->immutable_part = '\n';
    plus_s69 = L504;
    stdout_self76 = ((FILE1) NULL);
    L507=ZALLOC(sizeof(struct FILE1_struct));
    if (L507==NULL) FATAL("Unable to allocate more memory");
    ((OB)L507)->header.tag=FILE1_tag;
    L506 = ((FILE1) L507);
    r98 = L506;
    
    SATTR(r98,fp,stdout);
    ret_val215 = r98;
    FILE_plus_dSTR(ret_val215, plus_s69);
   }
   ret_val = 11;
   return ret_val;
  }
 }
 L509 = ATTR(self,only_reachable);
 L5101_=!(L509); 
 if (L5101_) {
  start_self8 = ((TIME) NULL);
  L5111_=(FLTD)time(NULL); 
  TIME_t854034019 = L5111_;
  if (ATTR(p,verbose)) {
   create_self58 = ((OUT) NULL);
   ret_val216 = create_self58;
   plus_self78 = ret_val216;
   plus_s70 = ((STR) &Checki974602838);
   stdout_self77 = ((FILE1) NULL);
   L513=ZALLOC(sizeof(struct FILE1_struct));
   if (L513==NULL) FATAL("Unable to allocate more memory");
   ((OB)L513)->header.tag=FILE1_tag;
   L512 = ((FILE1) L513);
   r99 = L512;
   
   SATTR(r99,fp,stdout);
   ret_val218 = r99;
   FILE_plus_STR(ret_val218, plus_s70);
   ret_val217 = plus_self78;
   flush_self18 = ret_val217;
   stdout_self78 = ((FILE1) NULL);
   L516=ZALLOC(sizeof(struct FILE1_struct));
   if (L516==NULL) FATAL("Unable to allocate more memory");
   ((OB)L516)->header.tag=FILE1_tag;
   L515 = ((FILE1) L516);
   r100 = L515;
   
   SATTR(r100,fp,stdout);
   ret_val219 = r100;
   flush_self19 = ret_val219;
   fflush(ATTR(flush_self19,fp));
  }
  L518 = ATTR(p,check_am);
  (*dCHECK1977119478[TAG(L518)])(L518);
  if (ATTR(p,verbose)) {
   create_self59 = ((OUT) NULL);
   ret_val220 = create_self59;
   plus_self79 = ret_val220;
   L520=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L520==NULL) FATAL("Unable to allocate more memory");
   memset(L520,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L520)->header.tag=CHAR_tag;
   L519 = (dSTR)((CHAR_boxed) L520);
   ((CHAR_boxed) L519)->immutable_part = '(';
   plus_s71 = L519;
   stdout_self79 = ((FILE1) NULL);
   L522=ZALLOC(sizeof(struct FILE1_struct));
   if (L522==NULL) FATAL("Unable to allocate more memory");
   ((OB)L522)->header.tag=FILE1_tag;
   L521 = ((FILE1) L522);
   r101 = L521;
   
   SATTR(r101,fp,stdout);
   ret_val222 = r101;
   FILE_plus_dSTR(ret_val222, plus_s71);
   ret_val221 = plus_self79;
   plus_self80 = ret_val221;
   elapsed_self12 = ((TIME) NULL);
   L5241_=(FLTD)time(NULL); 
   L525 = L5241_;
   L526 = TIME_t854034019;
   L5271_=L525-L526; 
   ret_val223 = L5271_;
   L529=ZALLOC_LEAF(sizeof(struct FLTD_boxed_struct));
   if (L529==NULL) FATAL("Unable to allocate more memory");
   memset(L529,0,sizeof(struct FLTD_boxed_struct));
   ((OB)L529)->header.tag=FLTD_tag;
   L528 = (dSTR)((FLTD_boxed) L529);
   ((FLTD_boxed) L528)->immutable_part = ret_val223;
   plus_s72 = L528;
   stdout_self80 = ((FILE1) NULL);
   L531=ZALLOC(sizeof(struct FILE1_struct));
   if (L531==NULL) FATAL("Unable to allocate more memory");
   ((OB)L531)->header.tag=FILE1_tag;
   L530 = ((FILE1) L531);
   r102 = L530;
   
   SATTR(r102,fp,stdout);
   ret_val225 = r102;
   FILE_plus_dSTR(ret_val225, plus_s72);
   ret_val224 = plus_self80;
   plus_self81 = ret_val224;
   plus_s73 = ((STR) &seconds1);
   stdout_self81 = ((FILE1) NULL);
   L534=ZALLOC(sizeof(struct FILE1_struct));
   if (L534==NULL) FATAL("Unable to allocate more memory");
   ((OB)L534)->header.tag=FILE1_tag;
   L533 = ((FILE1) L534);
   r103 = L533;
   
   SATTR(r103,fp,stdout);
   ret_val226 = r103;
   FILE_plus_STR(ret_val226, plus_s73);
  }
  if (ATTR(p,err_seen)) {
   if (ATTR(p,verbose)) {
    create_self60 = ((OUT) NULL);
    ret_val227 = create_self60;
    plus_self82 = ret_val227;
    L537=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L537==NULL) FATAL("Unable to allocate more memory");
    memset(L537,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L537)->header.tag=CHAR_tag;
    L536 = (dSTR)((CHAR_boxed) L537);
    ((CHAR_boxed) L536)->immutable_part = '\n';
    plus_s74 = L536;
    stdout_self82 = ((FILE1) NULL);
    L539=ZALLOC(sizeof(struct FILE1_struct));
    if (L539==NULL) FATAL("Unable to allocate more memory");
    ((OB)L539)->header.tag=FILE1_tag;
    L538 = ((FILE1) L539);
    r104 = L538;
    
    SATTR(r104,fp,stdout);
    ret_val228 = r104;
    FILE_plus_dSTR(ret_val228, plus_s74);
   }
   ret_val = 9;
   return ret_val;
  }
 }
 if (ATTR(p,verbose)) {
  create_self61 = ((OUT) NULL);
  ret_val229 = create_self61;
  plus_self83 = ret_val229;
  plus_s75 = ((STR) &Compil1872856259);
  stdout_self83 = ((FILE1) NULL);
  L542=ZALLOC(sizeof(struct FILE1_struct));
  if (L542==NULL) FATAL("Unable to allocate more memory");
  ((OB)L542)->header.tag=FILE1_tag;
  L541 = ((FILE1) L542);
  r105 = L541;
  
  SATTR(r105,fp,stdout);
  ret_val230 = r105;
  FILE_plus_STR(ret_val230, plus_s75);
  L544 = ATTR(p,stat1);
  (*dSTAT_print[TAG(L544)])(L544);
 }
 ret_val = 0;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

MS_RANDOM_GEN RND_ge1507420718(RND self) {
 MS_RANDOM_GEN ret_val;
 RND make_generator_s;
 MS_RANDOM_GEN ret_val1;
 MS_RANDOM_GEN g;
 MS_RANDOM_GEN create_self;
 MS_RANDOM_GEN ret_val2;
 MS_RANDOM_GEN res;
 MS_RANDOM_GEN L1;
 OB L2;
 if ((self==((RND) NULL))) {
  if ((RND_default_gen==((MS_RANDOM_GEN) NULL))) {
   make_generator_s = self;
   create_self = ((MS_RANDOM_GEN) NULL);
   L2=ZALLOC_LEAF(sizeof(struct MS_RANDOM_GEN_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   memset(L2,0,sizeof(struct MS_RANDOM_GEN_struct));
   ((OB)L2)->header.tag=MS_RANDOM_GEN_tag;
   L1 = ((MS_RANDOM_GEN) L2);
   res = L1;
   MS_RAN795181255(res, 1);
   ret_val2 = res;
   g = ret_val2;
   ret_val1 = g;
   RND_default_gen = ret_val1;
  }
  ret_val = RND_default_gen;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,own_gen);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

PROG_AS_TBL PROG_A1306680723(PROG_AS_TBL self) {
 PROG_AS_TBL ret_val;
 PROG_AS_TBL r;
 PROG_AS_TBL L11;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AS_CLASS_DEF L9;
 L21_=ASIZE((PROG_AS_TBL)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = PROG_A1754951656(self, L81_);
 {
  struct PROG_A1283019219_frame_struct other1_0;
PROG_A1283019219_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = PROG_A1283019219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = PROG_A2047269051(r, L9);
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

PROG_AS_TBL PROG_A1754951656(PROG_AS_TBL self, INT n) {
 PROG_AS_TBL ret_val;
 PROG_AS_TBL r;
 PROG_AS_TBL L11;
 PROG_AS_TBL elt_nil_self;
 AS_CLASS_DEF ret_val1;
 ELT_NI1411991819 elt_nil_self1;
 AS_CLASS_DEF ret_val2;
 AS_CLASS_DEF e = ((AS_CLASS_DEF) NULL);
 PROG_AS_TBL elt_nil_self2;
 AS_CLASS_DEF ret_val3;
 ELT_NI1411991819 elt_nil_self3;
 AS_CLASS_DEF ret_val4;
 AS_CLASS_DEF e1 = ((AS_CLASS_DEF) NULL);
 INT L21 = INT_zero;
 PROG_AS_TBL L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AS_CLASS_DEF L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct PROG_AS_TBL_struct)-sizeof(AS_CLASS_DEF))+(L4)*sizeof(AS_CLASS_DEF);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=PROG_AS_TBL_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((PROG_AS_TBL) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1411991819) NULL);
 ret_val2 = ((AS_CLASS_DEF) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AS_CLASS_DEF) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((PROG_AS_TBL)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1411991819) NULL);
    ret_val4 = ((AS_CLASS_DEF) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((PROG_AS_TBL)L11,L21,(AS_CLASS_DEF)L10); 
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

PROG_AS_TBL PROG_A2047269051(PROG_AS_TBL self, AS_CLASS_DEF e) {
 PROG_AS_TBL ret_val;
 PROG_AS_TBL r;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AS_CLASS_DEF te;
 AS_CLASS_DEF te1;
 PROG_AS_TBL is_elt_nil_self;
 AS_CLASS_DEF is_elt_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self1;
 AS_CLASS_DEF is_elt_nil_e1;
 BOOL ret_val2 = BOOL_zero;
 PROG_AS_TBL should_grow_self;
 BOOL ret_val3 = BOOL_zero;
 PROG_AS_TBL elt_hash_self;
 AS_CLASS_DEF elt_hash_e;
 INT ret_val4 = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val5 = INT_zero;
 INT x = INT_zero;
 PROG_AS_TBL is_elt_nil_self2;
 AS_CLASS_DEF is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self3;
 AS_CLASS_DEF is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 PROG_AS_TBL elt_eq_self;
 AS_CLASS_DEF elt_eq_e1;
 AS_CLASS_DEF elt_eq_e2;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ487127284 elt_eq_self1;
 AS_CLASS_DEF elt_eq_e11;
 AS_CLASS_DEF elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 PROG_AS_TBL is_elt_nil_self4;
 AS_CLASS_DEF is_elt_nil_e4;
 BOOL ret_val10 = BOOL_zero;
 ELT_NI1411991819 is_elt_nil_self5;
 AS_CLASS_DEF is_elt_nil_e5;
 BOOL ret_val11 = BOOL_zero;
 PROG_AS_TBL elt_eq_self2;
 AS_CLASS_DEF elt_eq_e12;
 AS_CLASS_DEF elt_eq_e22;
 BOOL ret_val12 = BOOL_zero;
 ELT_EQ487127284 elt_eq_self3;
 AS_CLASS_DEF elt_eq_e13;
 AS_CLASS_DEF elt_eq_e23;
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
 STR L12;
 INT L131_;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 AS_CLASS_DEF L261_;
 INT L281_;
 INT L291_;
 INT L30;
 BOOL L311_;
 AS_CLASS_DEF L321_;
 INT L341_;
 INT L36;
 INT L371_;
 r = self;
 is_elt_nil_self = self;
 is_elt_nil_e = e;
 is_elt_nil_self1 = ((ELT_NI1411991819) NULL);
 is_elt_nil_e1 = is_elt_nil_e;
 ret_val2 = (is_elt_nil_e1==((AS_CLASS_DEF) NULL));
 ret_val1 = ret_val2;
 if (ret_val1) {
  ret_val = r;
  return ret_val;
 }
 if ((r==((PROG_AS_TBL) NULL))) {
  r = PROG_A1754951656(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((PROG_AS_TBL)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = PROG_A1730220286;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val3 = L91_;
  if (ret_val3) {
   r = PROG_A1306680723(r);
  }
 }
 L101_=ASIZE((PROG_AS_TBL)r); 
 asz = L101_;
 elt_hash_self = r;
 elt_hash_e = e;
 hash_self = ATTR(elt_hash_e,name1);
 L12 = hash_self.str;
 L131_=SYSID(L12); 
 x = L131_;
 L141_=((unsigned)x*(unsigned)1664525); 
 L15 = L141_;
 L161_=((unsigned)L15+(unsigned)1013904223); 
 ret_val5 = L161_;
 L17 = ret_val5;
 L18 = AS_PAR401840644(ATTR(elt_hash_e,params));
 L191_=INTTIMES(1111,L18); 
 L20 = L191_;
 L211_=INTPLUS(L17,L20); 
 ret_val4 = L211_;
 L22 = ret_val4;
 L231_=INTMINUS(asz,2); 
 L24 = L231_;
 L251_=L22&L24; 
 orig_h = L251_;
 h = orig_h;
 while (1) {
  L261_=(AS_CLASS_DEF)ARR((PROG_AS_TBL)r,h); 
  te = L261_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te;
  is_elt_nil_self3 = ((ELT_NI1411991819) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val7 = (is_elt_nil_e3==((AS_CLASS_DEF) NULL));
  ret_val6 = ret_val7;
  if (ret_val6) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ487127284) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val9 = (elt_eq_e11==elt_eq_e21);
   ret_val8 = ret_val9;
   if (ret_val8) {
    SARR((PROG_AS_TBL)r,h,(AS_CLASS_DEF)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L281_=INTPLUS(h,1); 
  h = L281_;
 }
 after_loop: ;
 L291_=INTMINUS(asz,1); 
 L30 = L291_;
 L311_=(h)==(L30); 
 if (L311_) {
  h = 0;
  while (1) {
   L321_=(AS_CLASS_DEF)ARR((PROG_AS_TBL)r,h); 
   te1 = L321_;
   is_elt_nil_self4 = self;
   is_elt_nil_e4 = te1;
   is_elt_nil_self5 = ((ELT_NI1411991819) NULL);
   is_elt_nil_e5 = is_elt_nil_e4;
   ret_val11 = (is_elt_nil_e5==((AS_CLASS_DEF) NULL));
   ret_val10 = ret_val11;
   if (ret_val10) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ487127284) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val13 = (elt_eq_e13==elt_eq_e23);
    ret_val12 = ret_val13;
    if (ret_val12) {
     SARR((PROG_AS_TBL)r,h,(AS_CLASS_DEF)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 SARR((PROG_AS_TBL)r,h,(AS_CLASS_DEF)e); 
 ;
 L36 = ATTR(r,hsize);
 L371_=INTPLUS(L36,1); 
 SATTR(r,hsize,L371_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

RAT RAT_cr9703311(RAT self, INTI1 u, INTI1 v) {
 RAT ret_val = RAT_zero;
 INTI1 g;
 RAT r = RAT_zero;
 INTI1 is_zero_self;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_neg_self;
 BOOL ret_val2 = BOOL_zero;
 INT L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 RAT L5;
 RAT L6;
 is_zero_self = u;
 L1 = ATTR(is_zero_self,len1);
 L21_=(L1)==(0); 
 ret_val1 = L21_;
 if (ret_val1) {
  v = INTI_c1588968505(((INTI1) NULL), 1);
 }
 else {
  g = INTI_g1917565373(u, v);
  is_neg_self = v;
  L3 = ATTR(is_neg_self,len1);
  L41_=(L3)<(0); 
  ret_val2 = L41_;
  if (ret_val2) {
   g = INTI_negaterINTI(g);
  }
  u = INTI_d359767808(u, g);
  v = INTI_d359767808(v, g);
 }
 L5 = r;
 L5.u = u;
 L6 = L5;
 L6.v = v;
 ret_val = L6;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR RAT_str_INTrSTR(RAT self, INT n) {
 STR ret_val;
 TUPBOO1392923241 x = TUPBOO1392923241_zero;
 STR s;
 STR m;
 INT digits1 = INT_zero;
 INT trailing_zeros = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT after_decimal = INT_zero;
 INT e = INT_zero;
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
 INTI1 str_self1;
 STR ret_val4;
 FSTR clear_self;
 FSTR str_self2;
 STR ret_val5;
 STR plus_self1;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val6;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val7;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val8;
 STR r1;
 STR plus_self2;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val9;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val10;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val11;
 STR r2;
 STR plus_self3;
 STR plus_sarg;
 STR ret_val12;
 STR plus_self4;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val13;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val14;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val15;
 STR r3;
 STR plus_self5;
 INT plus_arg4 = INT_zero;
 STR ret_val16;
 STR s5;
 INT str_self6 = INT_zero;
 STR ret_val17;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val18;
 FSTR str_self7;
 STR ret_val19;
 extern STR name1;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L8;
 BOOL L91_;
 INT L121_;
 INT L101_,L101_m;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L17;
 CHAR L181_;
 CHAR L19;
 BOOL L201_;
 CHAR L221_;
 STR L23;
 INT L24;
 OB L25;
 INT L26;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 INT L331_;
 INT L34;
 INT L351_;
 BOOL L361_;
 INT L37;
 INT L381_;
 INT L39;
 INT L401_;
 BOOL L411_;
 BOOL L42;
 BOOL L431_;
 STR L44;
 INT L45;
 OB L46;
 INT L47;
 BOOL L49;
 BOOL L501_;
 x = RAT_fl2087165576(self, n, 10);
 s = ((STR) &name1);
 if (x.t1) {
  plus_self = s;
  plus_arg = '-';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L4 = 1;
  L6=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L4)*sizeof(CHAR);
  L5=ZALLOC_LEAF_BIG(L6);
  if (L5==NULL) FATAL("Unable to allocate more memory");
  memset(L5,0,L6);
  ((OB)L5)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L5)->header.destroyed=0;
#endif

  L3 = ((STR) L5);
  L3->asize = L4;
  r = L3;
  SARR((STR)r,0,create_c); 
  ;
  ret_val3 = r;
  ret_val2 = ret_val3;
  s1 = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
  s = ret_val1;
 }
 str_self1 = x.t2;
 clear_self = INTI_buf;
 L8 = (clear_self==((FSTR) NULL));
 L91_=!(L8); 
 if (L91_) {
  SATTR(clear_self,loc,0);
 }
 INTI_buf = INTI_s96356965(str_self1, INTI_buf);
 str_self2 = INTI_buf;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self2);
 ret_val4 = ret_val5;
 m = ret_val4;
 digits1 = STR_lengthrINT(m);
 {
  BOOL f_L101_ = TRUE;
  L11 = 0;
  L121_=INTMINUS(digits1,1); 
  L21 = L121_;
  L101_=L11-1;L101_m=L21; 
  while (1) {
   if(L101_++>=L101_m)  goto after_loop; 
   trailing_zeros = L101_;
   L141_=INTMINUS(digits1,1); 
   L15 = L141_;
   L161_=INTMINUS(L15,trailing_zeros); 
   L17 = L161_;
   L181_=ARR((STR)m,L17); 
   L19 = L181_;
   L201_=L19=='0'; 
   if (L201_) {
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 plus_self1 = s;
 L221_=ARR((STR)m,0); 
 plus_arg1 = L221_;
 str_self3 = plus_arg1;
 create_self1 = ((STR) NULL);
 create_c1 = str_self3;
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
 r1 = L23;
 SARR((STR)r1,0,create_c1); 
 ;
 ret_val8 = r1;
 ret_val7 = ret_val8;
 s2 = ret_val7;
 ret_val6 = STR_ap1077157958(plus_self1, s2, TRUE);
 plus_self2 = ret_val6;
 plus_arg2 = '.';
 str_self4 = plus_arg2;
 create_self2 = ((STR) NULL);
 create_c2 = str_self4;
 L29 = 1;
 L31=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L29)*sizeof(CHAR);
 L30=ZALLOC_LEAF_BIG(L31);
 if (L30==NULL) FATAL("Unable to allocate more memory");
 memset(L30,0,L31);
 ((OB)L30)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L30)->header.destroyed=0;
#endif

 L28 = ((STR) L30);
 L28->asize = L29;
 r2 = L28;
 SARR((STR)r2,0,create_c2); 
 ;
 ret_val11 = r2;
 ret_val10 = ret_val11;
 s3 = ret_val10;
 ret_val9 = STR_ap1077157958(plus_self2, s3, TRUE);
 s = ret_val9;
 L331_=INTMINUS(digits1,1); 
 L34 = L331_;
 L351_=INTMINUS(L34,trailing_zeros); 
 after_decimal = L351_;
 L361_=(0)<(after_decimal); 
 if (L361_) {
  plus_self3 = s;
  plus_sarg = STR_su203383946(m, 1, after_decimal);
  ret_val12 = STR_ap2004550586(plus_self3, plus_sarg);
  s = ret_val12;
 }
 L37 = x.t3;
 L381_=INTPLUS(L37,digits1); 
 L39 = L381_;
 L401_=INTMINUS(L39,1); 
 e = L401_;
 L411_=(e)==(0); 
 L42 = L411_;
 L431_=!(L42); 
 if (L431_) {
  plus_self4 = s;
  plus_arg3 = 'e';
  str_self5 = plus_arg3;
  create_self3 = ((STR) NULL);
  create_c3 = str_self5;
  L45 = 1;
  L47=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L45)*sizeof(CHAR);
  L46=ZALLOC_LEAF_BIG(L47);
  if (L46==NULL) FATAL("Unable to allocate more memory");
  memset(L46,0,L47);
  ((OB)L46)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L46)->header.destroyed=0;
#endif

  L44 = ((STR) L46);
  L44->asize = L45;
  r3 = L44;
  SARR((STR)r3,0,create_c3); 
  ;
  ret_val15 = r3;
  ret_val14 = ret_val15;
  s4 = ret_val14;
  ret_val13 = STR_ap1077157958(plus_self4, s4, TRUE);
  plus_self5 = ret_val13;
  plus_arg4 = e;
  str_self6 = plus_arg4;
  clear_self1 = buf1;
  L49 = (clear_self1==((FSTR) NULL));
  L501_=!(L49); 
  if (L501_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self = str_self6;
  str_in_s = buf1;
  ret_val18 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val18;
  str_self7 = buf1;
  ret_val19 = STR_fr1097270334(((STR) NULL), str_self7);
  ret_val17 = ret_val19;
  s5 = ret_val17;
  ret_val16 = STR_ap1077157958(plus_self5, s5, TRUE);
  s = ret_val16;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR RAT_strrSTR(RAT self) {
 STR ret_val;
 RAT is_int_self = RAT_zero;
 BOOL ret_val1 = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val2 = BOOL_zero;
 INTI1 str_self;
 STR ret_val3;
 FSTR clear_self;
 FSTR str_self1;
 STR ret_val4;
 INTI1 str_self2;
 STR ret_val5;
 FSTR clear_self1;
 FSTR str_self3;
 STR ret_val6;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self4 = CHAR_zero;
 STR ret_val8;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 INTI1 str_self5;
 STR ret_val10;
 FSTR clear_self2;
 FSTR str_self6;
 STR ret_val11;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val12;
 BOOL L1;
 INT L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 STR L8;
 INT L9;
 OB L10;
 INT L11;
 BOOL L13;
 BOOL L141_;
 is_int_self = self;
 is_eq_self = is_int_self.v;
 is_eq_y = INTI_c1588968505(((INTI1) NULL), 1);
 if ((is_eq_self==is_eq_y)) {
  L1 = TRUE;
 } else {
  L2 = INTI_c675035669(is_eq_self, is_eq_y);
  L31_=(L2)==(0); 
  L1 = L31_;
 }
 ret_val2 = L1;
 ret_val1 = ret_val2;
 if (ret_val1) {
  str_self = self.u;
  clear_self = INTI_buf;
  L4 = (clear_self==((FSTR) NULL));
  L51_=!(L4); 
  if (L51_) {
   SATTR(clear_self,loc,0);
  }
  INTI_buf = INTI_s96356965(str_self, INTI_buf);
  str_self1 = INTI_buf;
  ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
  ret_val3 = ret_val4;
  ret_val = ret_val3;
  return ret_val;
 }
 else {
  str_self2 = self.u;
  clear_self1 = INTI_buf;
  L6 = (clear_self1==((FSTR) NULL));
  L71_=!(L6); 
  if (L71_) {
   SATTR(clear_self1,loc,0);
  }
  INTI_buf = INTI_s96356965(str_self2, INTI_buf);
  str_self3 = INTI_buf;
  ret_val6 = STR_fr1097270334(((STR) NULL), str_self3);
  ret_val5 = ret_val6;
  plus_self = ret_val5;
  plus_arg = '/';
  str_self4 = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self4;
  L9 = 1;
  L11=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
  L10=ZALLOC_LEAF_BIG(L11);
  if (L10==NULL) FATAL("Unable to allocate more memory");
  memset(L10,0,L11);
  ((OB)L10)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L10)->header.destroyed=0;
#endif

  L8 = ((STR) L10);
  L8->asize = L9;
  r = L8;
  SARR((STR)r,0,create_c); 
  ;
  ret_val9 = r;
  ret_val8 = ret_val9;
  s = ret_val8;
  ret_val7 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val7;
  str_self5 = self.v;
  clear_self2 = INTI_buf;
  L13 = (clear_self2==((FSTR) NULL));
  L141_=!(L13); 
  if (L141_) {
   SATTR(clear_self2,loc,0);
  }
  INTI_buf = INTI_s96356965(str_self5, INTI_buf);
  str_self6 = INTI_buf;
  ret_val11 = STR_fr1097270334(((STR) NULL), str_self6);
  ret_val10 = ret_val11;
  plus_sarg = ret_val10;
  ret_val12 = STR_ap2004550586(plus_self1, plus_sarg);
  ret_val = ret_val12;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

TUPBOO1392923241 RAT_fl2087165576(RAT self, INT n, INT b1) {
 TUPBOO1392923241 ret_val = TUPBOO1392923241_zero;
 INTI1 beta;
 INTI1 beta_n1;
 INTI1 beta_n;
 BOOL s = BOOL_zero;
 INTI1 u;
 INTI1 v;
 INT e = INT_zero;
 INTI1 m = ((INTI1) NULL);
 INTI1 r;
 INTI1 v_r;
 INTI1 is_zero_self;
 BOOL ret_val1 = BOOL_zero;
 TUPBOO1392923241 create_self = TUPBOO1392923241_zero;
 BOOL create_at1 = BOOL_zero;
 INTI1 create_at2;
 INT create_at3 = INT_zero;
 BOOL create_at4 = BOOL_zero;
 TUPBOO1392923241 ret_val2 = TUPBOO1392923241_zero;
 INTI1 is_neg_self;
 BOOL ret_val3 = BOOL_zero;
 INTI1 log2_self;
 INT ret_val4 = INT_zero;
 INTI1 log2_self1;
 INT ret_val5 = INT_zero;
 FLT log2_self2 = FLT_zero;
 FLT ret_val6 = FLT_zero;
 INTI1 is_lt_self;
 INTI1 is_lt_y;
 BOOL ret_val7 = BOOL_zero;
 INTI1 is_lt_self1;
 INTI1 is_lt_y1;
 BOOL ret_val8 = BOOL_zero;
 INTI1 is_lt_self2;
 INTI1 is_lt_y2;
 BOOL ret_val9 = BOOL_zero;
 INTI1 is_eq_self;
 INTI1 is_eq_y;
 BOOL ret_val10 = BOOL_zero;
 INTI1 is_odd_self;
 BOOL ret_val11 = BOOL_zero;
 INTI1 is_eq_self1;
 INTI1 is_eq_y1;
 BOOL ret_val12 = BOOL_zero;
 INTI1 is_zero_self1;
 BOOL ret_val13 = BOOL_zero;
 TUPBOO1392923241 create_self1 = TUPBOO1392923241_zero;
 BOOL create_at11 = BOOL_zero;
 INTI1 create_at21;
 INT create_at31 = INT_zero;
 BOOL create_at41 = BOOL_zero;
 TUPBOO1392923241 ret_val14 = TUPBOO1392923241_zero;
 INT L1;
 BOOL L21_;
 TUPBOO1392923241 L3;
 TUPBOO1392923241 L4;
 TUPBOO1392923241 L5;
 TUPBOO1392923241 L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L111_;
 INT L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L16;
 INT L171_;
 INT L18;
 INT L191_;
 INT L20;
 INT L221_;
 INT L23;
 INT L24;
 INT L251_;
 INT L26;
 INT L27;
 INT L281_;
 INT L29;
 INT L30;
 INT L311_;
 INT L32;
 INT L331_;
 INT L34;
 INT L351_;
 INT L36;
 INT L371_;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 FLT L411_;
 FLT L42;
 FLT L431_;
 FLT L441_;
 FLT L45;
 FLT L46;
 FLT L471_;
 FLT L48;
 FLT L491_;
 FLT L50;
 FLT L511_;
 FLT L52;
 INT L531_;
 INT L541_;
 BOOL L551_;
 INT L561_;
 INT L57;
 BOOL L581_;
 INT L591_;
 INT L60;
 BOOL L611_;
 BOOL L62;
 BOOL L631_;
 INT L641_;
 BOOL L65;
 INT L66;
 BOOL L671_;
 BOOL L68;
 BOOL L69;
 INT L70;
 BOOL L721_;
 BOOL L73;
 INT L74;
 BOOL L751_;
 BOOL L76;
 BOOL L771_;
 INT L781_;
 INT L79;
 BOOL L801_;
 BOOL L81;
 INT L82;
 BOOL L831_;
 INT L841_;
 INT L85;
 BOOL L861_;
 TUPBOO1392923241 L87;
 TUPBOO1392923241 L88;
 TUPBOO1392923241 L89;
 TUPBOO1392923241 L90;
 is_zero_self = self.u;
 L1 = ATTR(is_zero_self,len1);
 L21_=(L1)==(0); 
 ret_val1 = L21_;
 if (ret_val1) {
  create_self = TUPBOO1392923241_zero;
  create_at1 = FALSE;
  create_at2 = INTI_c1588968505(((INTI1) NULL), 0);
  create_at3 = 0;
  create_at4 = TRUE;
  L3 = create_self;
  L3.t1 = create_at1;
  L4 = L3;
  L4.t2 = create_at2;
  L5 = L4;
  L5.t3 = create_at3;
  L6 = L5;
  L6.t4 = create_at4;
  ret_val2 = L6;
  ret_val = ret_val2;
  return ret_val;
 }
 else {
  beta = INTI_c1588968505(((INTI1) NULL), b1);
  L71_=INTMINUS(n,1); 
  beta_n1 = INTI_p1518979099(beta, L71_);
  beta_n = INTI_t1692605406(beta_n1, beta);
  is_neg_self = self.u;
  L8 = ATTR(is_neg_self,len1);
  L91_=(L8)<(0); 
  ret_val3 = L91_;
  s = ret_val3;
  u = INTI_absrINTI(self.u);
  v = self.v;
  log2_self = u;
  L10 = ATTR(log2_self,len1);
  L111_=INTMINUS(L10,1); 
  L12 = L111_;
  L13 = INTI_log2B;
  L141_=INTTIMES(L12,L13); 
  L15 = L141_;
  L16 = ATTR(log2_self,len1);
  L171_=INTMINUS(L16,1); 
  L18 = L171_;
  L191_=ARR((INTI1)log2_self,L18); 
  L20 = INT_hi740923800(L191_);
  L221_=INTPLUS(L15,L20); 
  ret_val4 = L221_;
  L23 = ret_val4;
  log2_self1 = v;
  L24 = ATTR(log2_self1,len1);
  L251_=INTMINUS(L24,1); 
  L26 = L251_;
  L27 = INTI_log2B;
  L281_=INTTIMES(L26,L27); 
  L29 = L281_;
  L30 = ATTR(log2_self1,len1);
  L311_=INTMINUS(L30,1); 
  L32 = L311_;
  L331_=ARR((INTI1)log2_self1,L32); 
  L34 = INT_hi740923800(L331_);
  L351_=INTPLUS(L29,L34); 
  ret_val5 = L351_;
  L36 = ret_val5;
  L371_=INTMINUS(L23,L36); 
  e = L371_;
  L381_=(b1)==(2); 
  L39 = L381_;
  L401_=!(L39); 
  if (L401_) {
   L411_=(FLT)e; 
   L42 = L411_;
   L431_=(FLT)b1; 
   log2_self2 = L431_;
   L441_=r_log(log2_self2); 
   L45 = L441_;
   L46 = FLT_log2_e;
   L471_=L45*L46; 
   ret_val6 = L471_;
   L48 = ret_val6;
   L491_=FLTDIV(L42,L48); 
   L50 = L491_;
   L511_=FLTROUND(L50); 
   L52 = L511_;
   L531_=FLTINT(L52); 
   e = L531_;
  }
  L541_=INTMINUS(e,n); 
  e = L541_;
  L551_=(e)<(0); 
  if (L551_) {
   L561_=INTMINUS(0,e); 
   u = INTI_t1692605406(u, INTI_p1518979099(beta, L561_));
  }
  else {
   v = INTI_t1692605406(v, INTI_p1518979099(beta, e));
  }
  while (1) {
   m = INTI_d359767808(u, v);
   is_lt_self = m;
   is_lt_y = beta_n1;
   L57 = INTI_c675035669(is_lt_self, is_lt_y);
   L581_=(L57)<(0); 
   ret_val7 = L581_;
   if (ret_val7) {
    u = INTI_t1692605406(u, beta);
    L591_=INTMINUS(e,1); 
    e = L591_;
   }
   else {
    is_lt_self1 = m;
    is_lt_y1 = beta_n;
    L60 = INTI_c675035669(is_lt_self1, is_lt_y1);
    L611_=(L60)<(0); 
    ret_val8 = L611_;
    L62 = ret_val8;
    L631_=!(L62); 
    if (L631_) {
     v = INTI_t1692605406(v, beta);
     L641_=INTPLUS(e,1); 
     e = L641_;
    }
    else {
     goto after_loop;
    }
   }
  }
  after_loop: ;
  r = INTI_m582155739(u, v);
  v_r = INTI_m436971372(v, r);
  is_lt_self2 = v_r;
  is_lt_y2 = r;
  L66 = INTI_c675035669(is_lt_self2, is_lt_y2);
  L671_=(L66)<(0); 
  ret_val9 = L671_;
  if (ret_val9) {
   L65 = TRUE;
  } else {
   is_eq_self = r;
   is_eq_y = v_r;
   if ((is_eq_self==is_eq_y)) {
    L69 = TRUE;
   } else {
    L70 = INTI_c675035669(is_eq_self, is_eq_y);
    L721_=(L70)==(0); 
    L69 = L721_;
   }
   ret_val10 = L69;
   if (ret_val10) {
    is_odd_self = m;
    L74 = ATTR(is_odd_self,len1);
    L751_=(L74)==(0); 
    L76 = L751_;
    L771_=!(L76); 
    if (L771_) {
     L781_=ARR((INTI1)is_odd_self,0); 
     L79 = L781_;
     L801_=(L79&1)!=0; 
     L73 = L801_;
    } else {
     L73 = FALSE;
    }
    ret_val11 = L73;
    L68 = ret_val11;
   } else {
    L68 = FALSE;
   }
   L65 = L68;
  }
  if (L65) {
   m = INTI_p1769163857(m, INTI_c1588968505(((INTI1) NULL), 1));
   is_eq_self1 = m;
   is_eq_y1 = beta_n;
   if ((is_eq_self1==is_eq_y1)) {
    L81 = TRUE;
   } else {
    L82 = INTI_c675035669(is_eq_self1, is_eq_y1);
    L831_=(L82)==(0); 
    L81 = L831_;
   }
   ret_val12 = L81;
   if (ret_val12) {
    m = beta_n1;
    L841_=INTPLUS(e,1); 
    e = L841_;
   }
  }
  create_self1 = TUPBOO1392923241_zero;
  create_at11 = s;
  create_at21 = m;
  create_at31 = e;
  is_zero_self1 = r;
  L85 = ATTR(is_zero_self1,len1);
  L861_=(L85)==(0); 
  ret_val13 = L861_;
  create_at41 = ret_val13;
  L87 = create_self1;
  L87.t1 = create_at11;
  L88 = L87;
  L88.t2 = create_at21;
  L89 = L88;
  L89.t3 = create_at31;
  L90 = L89;
  L90.t4 = create_at41;
  ret_val14 = L90;
  ret_val = ret_val14;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

void MS_RAN795181255(MS_RANDOM_GEN self, INT nseed) {
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 INT L61_;
 INT L7;
 FLTD L81_;
 FLTD L9;
 FLTD L101_;
 FLTD L12;
 FLTD L131_;
 L11_=INTMINUS(nseed,1); 
 L2 = L11_;
 L31_=INTMOD(L2,2147483645); 
 L4 = L31_;
 L51_=INTPLUS(1,L4); 
 SATTR(self,seed,L51_);
 L61_=((unsigned)1)<<((unsigned)31); 
 L7 = L61_;
 L81_=(FLTD)L7; 
 L9 = L81_;
 L101_=L9-1.; 
 MS_RAN393309279 = L101_;
 L12 = MS_RAN393309279;
 L131_=L12-1.; 
 MS_RAN1117060533 = L131_;
}


#undef IS_ITER
#define IS_ITER 0

void PROG_A148914482(PROG_AS_TBL self, PROG p) {
 PROG_AS_TBL L11;
 AS_CLASS_DEF classdef;
 STR fn;
 dPARSE parse;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val1;
 AS_CLASS_DEF L2;
 STR L31_;
 BOOL L4;
 BOOL L51_;
 AS_OUT_prog = p;
 {
  struct PROG_A1283019219_frame_struct other1_0;
PROG_A1283019219_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = PROG_A1283019219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   classdef = L2;
   file_in_self = ATTR(classdef,source1);
   aget_self = SFILE_ID_files;
   aget_ind = SFILE_1102877860(file_in_self);
   L31_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
   ret_val1 = L31_;
   ret_val = ret_val1;
   fn = ret_val;
   parse = ATTR(p,parse);
   if (parse==NULL) {
    goto other465;
   } else
   switch (TAG(parse)) {
    case PARSE_tag:
     L4 = FSETST1025458804(ATTR(((PARSE) parse),known_files), fn);
     L51_=!(L4); 
     if (L51_) {
      AS_OUT1171738231(((AS_OUT) NULL), ((dAS_NODE) classdef));
     } break;
    default: ;
    other465: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void PROG_AS_TBL_dump(PROG_AS_TBL self) {
 PROG_AS_TBL L11;
 AS_CLASS_DEF classdef;
 STR res;
 AS_PARAM_DEC param1;
 STR L21;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self2;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self3;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val5;
 FILE1 r1;
 AS_CLASS_DEF L3;
 extern STR name2;
 extern STR name3;
 STR L4;
 FILE1 L5;
 OB L6;
 extern STR name32;
 FILE1 L8;
 OB L9;
 {
  struct PROG_A1283019219_frame_struct other1_0;
PROG_A1283019219_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = PROG_A1283019219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   classdef = L3;
   plus_self = ATTR(classdef,name1).str;
   plus_sarg = ((STR) &name2);
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   res = ret_val;
   param1 = ATTR(classdef,params);
   {
    struct STR_se405450305_frame_struct other2_0;
STR_se405450305_frame noname2 = &other2_0;
    L21 = ((STR) &name3);
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     if ((param1==((AS_PARAM_DEC) NULL))) {
      goto after_loop1;
     }
     plus_self1 = res;
     noname2->arg1 = ((dSTR) ATTR(param1,name1).str);
     L4 = STR_se405450305(noname2);
     if (noname2->state == -1) {
      goto after_loop1;
     }
     plus_sarg1 = L4;
     ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
     res = ret_val1;
     param1 = ATTR(param1,next);
    }
   }
   after_loop1: ;
   create_self = ((OUT) NULL);
   ret_val2 = create_self;
   plus_self2 = ret_val2;
   plus_s = res;
   stdout_self = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r = L5;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self2;
   plus_self3 = ret_val3;
   plus_s1 = ((STR) &name32);
   stdout_self1 = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r1 = L8;
   
   SATTR(r1,fp,stdout);
   ret_val5 = r1;
   FILE_plus_STR(ret_val5, plus_s1);
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

AS_CLASS_DEF PROG_A1089081349(PROG_A1089081349_frame frame) {
 AS_CLASS_DEF dummy = ((AS_CLASS_DEF) NULL);
 AS_CLASS_DEF aI_u_I;
 AS_CLASS_DEF L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((PROG_AS_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((PROG_AS_TBL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
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

AS_CLASS_DEF PROG_A1283019219(PROG_A1283019219_frame frame) {
 AS_CLASS_DEF dummy = ((AS_CLASS_DEF) NULL);
 BOOL L1;
 BOOL L21_;
 AS_CLASS_DEF L5;
 AS_CLASS_DEF aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((PROG_AS_TBL) NULL));
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
     frame->L41_br=ASIZE((PROG_AS_TBL)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((PROG_AS_TBL)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_elt_nil_self = frame->self;
    frame->is_elt_nil_e = frame->r;
    frame->is_elt_nil_self1 = ((ELT_NI1411991819) NULL);
    frame->is_elt_nil_e1 = frame->is_elt_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e1==((AS_CLASS_DEF) NULL));
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

void PROG_A1919229787(PROG_A1919229787_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((PROG_AS_TBL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((PROG_AS_TBL)frame->self,frame->i_I_u_I,(AS_CLASS_DEF)frame->arg1); frame->i_I_u_I++;
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
