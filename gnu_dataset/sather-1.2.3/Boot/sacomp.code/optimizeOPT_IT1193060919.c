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

typedef struct dGENERATE_AM_struct {
 OB_HEADER header;
 } *dGENERATE_AM;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

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

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

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

typedef struct OPT_ITER_struct {/* layout for OPT_ITER */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_ITER;

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

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct TOKEN_struct {/* layout for TOKEN */
 INT val1;
 } TOKEN;
static TOKEN TOKEN_zero;

typedef struct TOKEN_boxed_struct {
 OB_HEADER header;
 TOKEN immutable_part;
 } *TOKEN_boxed;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

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

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

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

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

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

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

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

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYBOOL_struct {/* layout for ARRAY{BOOL} */
 OB_HEADER header;
 INT asize;
 BOOL arr_part[1];
 } *ARRAYBOOL;

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

typedef struct CONVERT_struct {/* layout for CONVERT */
 OB_HEADER header;
 struct BFILE_struct *file1;
 INT end_pos;
 INT start_pos;
 BOOL need_to_convert;
 } *CONVERT;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

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

typedef struct FSTR_eltbrCHAR_frame_struct {
 INT state;
 FSTR self;/* Formal argument: self */
 CHAR ret_val1;
 INT i;
 INT sz;
 FSTR aget_self;
 INT aget_ind;
 CHAR ret_val;
 } *FSTR_eltbrCHAR_frame;

typedef struct PARSER_struct {/* layout for PARSER */
 OB_HEADER header;
 struct CONVERT_struct *convert;
 struct FLISTSTR_struct *entered;
 struct PROG_struct *prog;
 struct SCANNER_struct *scanner;
 struct TOKEN_struct next;
 BOOL version_1_0;
 } *PARSER;

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

extern AM_INT_CONST OPT_CO232246741;
extern AM_INT_CONST OPT_CO238346998;
extern FSTR SFILE_ID_source;
extern IDENT IDENT_2085547498;
extern INT SFILE_ID_pos;
extern PROG OPT_CO233462019;
extern dTP OPT_CO1800765742;

/* Function declarations */


extern RETURNED_CONST SFILE_ID (**dAM_ST1805574914)(dAM_STMT);

extern RETURNED_CONST dAM_EXPR (**dINLIN1378053623)(dINLINE, AM_ROUT_DEF, AM_ROU1916046290);

extern RETURNED_CONST dAM_EXPR (**dINLIN961094327)(dINLINE, AM_ROUT_DEF, AM_LOOP_STMT, AM_ITE1809135850);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);

extern RETURNED_CONST void (**dAM_ST1372701974)(dAM_STMT, dAM_STMT);

extern RETURNED_CONST void (**dGENER651759373)(dGENERATE_AM, SIG);

extern RETURNED_CONST void (**dSTAT_incr_STR)(dSTAT, STR);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_CURSOR AM_CUR1358945253(AM_CURSOR, AM_CURSOR);
AM_LOOP_STMT AM_CUR153953493(AM_CURSOR);
AM_ROUT_DEF FMAPSI2016001247(FMAPSI518162669, SIG);
AS_CLASS_DEF PARSER2058766590(PARSER);
AS_CLASS_DEF PARSE_962387968(PARSE, IDENT, INT);
AS_CLASS_DEF PROG_A382977477(PROG_AS_TBL, TUPIDENTINT);
BOOL AM_CUR1097584212(AM_CURSOR);
BOOL AM_CUR1106436467(AM_CURSOR);
BOOL AM_CUR1926902782(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR2013152412(AM_CURSOR);
BOOL AM_CUR2135357807(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR262177482(AM_CURSOR);
BOOL AM_CUR45133320(AM_CURSOR, dAM);
BOOL AM_CUR750559803(AM_CURSOR, AM_CURSOR, dAM);
BOOL AM_CUR762735158(AM_CURSOR, dAM);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL OPT_IT1532669498(OPT_ITER, dAM_EXPR, AM_CURSOR, BOOL, BOOL);
BOOL OPT_IT2079146999(OPT_ITER, dAM);
BOOL OPT_IT24605408(OPT_ITER, dAM_EXPR, AM_CURSOR, BOOL, BOOL);
BOOL OPT_IT59245853(OPT_ITER, AM_ROUT_DEF, AM_ITE1809135850, AM_CURSOR, BOOL, BOOL, BOOL);
BOOL OPT_IT948201730(OPT_ITER, AM_ROUT_DEF, AM_ROU1916046290, AM_CURSOR);
BOOL PROG_A878808304(PROG_AS_TBL, TUPIDENTINT);
BOOL SIG_is418491101(SIG);
CHAR FSTR_eltbrCHAR(FSTR_eltbrCHAR_frame);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
CONVERT CONVER262703763(CONVERT, PARSER, BOOL, STR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTdAM_EXPR FLISTd805769956(FLISTdAM_EXPR, dAM_EXPR);
FMAPSI518162669 FMAPSI1625125906(FMAPSI518162669, SIG);
FSETAM_ROUT_DEF FSETAM2044524049(FSETAM_ROUT_DEF, AM_ROUT_DEF);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT AS_PAR401840644(AS_PARAM_DEC);
INT FLISTA1370145561(FLISTA725283029, AM_LOCAL_EXPR);
INT FSTR_sizerINT(FSTR);
INT INT_up418511718(INT_up418511718_frame);
INT STR_lengthrINT(STR);
INT STR_sizerINT(STR);
IN_MODE IN_MOD403789248(IN_MODE);
PARSER PARSER403165095(PARSER, PROG, STR, BOOL, BOOL, BOOL);
PROG_AS_TBL PROG_A2047269051(PROG_AS_TBL, AS_CLASS_DEF);
SFILE_ID SFILE_1594766561(SFILE_ID);
SIG SIG_ma1442297600(SIG);
STR FMAPST1518383807(FMAPSTRSTR, STR);
STR IDENT_1415966123(IDENT, INT);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM AM_CUR1623871191(AM_CURSOR);
dAM AM_CUR52795524(AM_CURSOR);
dAM_STMT AM_CUR1430796545(AM_CURSOR);
void AM_CUR1127255611(AM_CURSOR);
void AM_CUR1223064464(AM_CURSOR);
void AM_CUR1769624888(AM_CURSOR);
void AM_CUR1869307717(AM_CURSOR);
void AM_CUR1966744225(AM_CURSOR, dAM_STMT);
void AM_CUR352782528(AM_CURSOR, dAM_STMT);
void AM_CUR897865267(AM_CURSOR);
void AM_CUR915996644(AM_CURSOR, dAM_EXPR);
void AM_OUT1439666796(AM_OUT, dAM);
void BFILE_plus_FSTR(BFILE, FSTR);
void BFILE_plus_STR(BFILE, STR);
void CONVER649045550(CONVERT, IDENT);
void CONVER840793676(CONVERT, CHAR);
void CONVER911997526(CONVERT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void OPT_IT1323213400(OPT_ITER, AM_ROUT_DEF, AM_STMT_EXPR, AM_CURSOR, AM_ITE1809135850, BOOL, BOOL);
void OPT_IT222343591(OPT_ITER, dAM);
void OPT_IT505798087(OPT_ITER, AM_ROUT_DEF, AM_ITE1809135850, AM_CURSOR, ARRAYBOOL);
void PARSE_1609412102(PARSE, STR);
void PARSE_947379252(PARSE, AS_CLASS_DEF, AS_CLASS_DEF);
void PARSE_parse_STR(PARSE, STR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_CLASS_DEF PARSE_962387968(PARSE self, IDENT nm, INT num1) {
 AS_CLASS_DEF ret_val;
 TUPIDENTINT searchtag = TUPIDENTINT_zero;
 AS_CLASS_DEF r;
 STR fn;
 STR class_name;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val1 = TUPIDENTINT_zero;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r2;
 OUT flush_self;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r3;
 FILE1 flush_self1;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val9;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val11;
 FILE1 stdout_self4;
 FILE1 ret_val12;
 FILE1 r5;
 OUT flush_self2;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r6;
 FILE1 flush_self3;
 STR plus_self4;
 STR plus_sarg;
 STR ret_val14;
 STR plus_self5;
 STR plus_sarg1;
 STR ret_val15;
 TUPIDENTINT L1;
 TUPIDENTINT L2;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR Foundt1447281194;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L11;
 FILE1 L13;
 OB L14;
 extern STR Didnot42591324;
 FILE1 L16;
 OB L17;
 FILE1 L19;
 OB L20;
 FILE1 L22;
 OB L23;
 BOOL L25;
 BOOL L261_;
 extern STR Therei214122223;
 extern STR name18;
 create_self = TUPIDENTINT_zero;
 create_at1 = nm;
 create_at2 = num1;
 L1 = create_self;
 L1.t1 = create_at1;
 L2 = L1;
 L2.t2 = create_at2;
 ret_val1 = L2;
 searchtag = ret_val1;
 r = PROG_A382977477(ATTR(ATTR(self,prog),as_tbl), searchtag);
 L3 = (r==((AS_CLASS_DEF) NULL));
 L41_=!(L3); 
 if (L41_) {
  ret_val = r;
  return ret_val;
 }
 fn = FMAPST1518383807(ATTR(self,has), nm.str);
 L5 = (fn==((STR) NULL));
 L61_=!(L5); 
 if (L61_) {
  if (ATTR(ATTR(self,prog),show_types)) {
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self = ret_val2;
   plus_s = ((STR) &Foundt1447281194);
   stdout_self = ((FILE1) NULL);
   L8=ZALLOC(sizeof(struct FILE1_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=FILE1_tag;
   L7 = ((FILE1) L8);
   r1 = L7;
   
   SATTR(r1,fp,stdout);
   ret_val4 = r1;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = nm.str;
   stdout_self1 = ((FILE1) NULL);
   L11=ZALLOC(sizeof(struct FILE1_struct));
   if (L11==NULL) FATAL("Unable to allocate more memory");
   ((OB)L11)->header.tag=FILE1_tag;
   L10 = ((FILE1) L11);
   r2 = L10;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   flush_self = ret_val5;
   stdout_self2 = ((FILE1) NULL);
   L14=ZALLOC(sizeof(struct FILE1_struct));
   if (L14==NULL) FATAL("Unable to allocate more memory");
   ((OB)L14)->header.tag=FILE1_tag;
   L13 = ((FILE1) L14);
   r3 = L13;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   flush_self1 = ret_val7;
   fflush(ATTR(flush_self1,fp));
  }
  PARSE_1609412102(self, fn);
  r = PROG_A382977477(ATTR(ATTR(self,prog),as_tbl), searchtag);
 }
 if ((r==((AS_CLASS_DEF) NULL))) {
  if (ATTR(ATTR(self,prog),show_types)) {
   create_self2 = ((OUT) NULL);
   ret_val8 = create_self2;
   plus_self2 = ret_val8;
   plus_s2 = ((STR) &Didnot42591324);
   stdout_self3 = ((FILE1) NULL);
   L17=ZALLOC(sizeof(struct FILE1_struct));
   if (L17==NULL) FATAL("Unable to allocate more memory");
   ((OB)L17)->header.tag=FILE1_tag;
   L16 = ((FILE1) L17);
   r4 = L16;
   
   SATTR(r4,fp,stdout);
   ret_val10 = r4;
   FILE_plus_STR(ret_val10, plus_s2);
   ret_val9 = plus_self2;
   plus_self3 = ret_val9;
   plus_s3 = nm.str;
   stdout_self4 = ((FILE1) NULL);
   L20=ZALLOC(sizeof(struct FILE1_struct));
   if (L20==NULL) FATAL("Unable to allocate more memory");
   ((OB)L20)->header.tag=FILE1_tag;
   L19 = ((FILE1) L20);
   r5 = L19;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s3);
   ret_val11 = plus_self3;
   flush_self2 = ret_val11;
   stdout_self5 = ((FILE1) NULL);
   L23=ZALLOC(sizeof(struct FILE1_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=FILE1_tag;
   L22 = ((FILE1) L23);
   r6 = L22;
   
   SATTR(r6,fp,stdout);
   ret_val13 = r6;
   flush_self3 = ret_val13;
   fflush(ATTR(flush_self3,fp));
  }
  class_name = IDENT_1415966123(nm, num1);
  L25 = FSETST1025458804(ATTR(self,missing_classes), class_name);
  L261_=!(L25); 
  if (L261_) {
   plus_self4 = ((STR) &Therei214122223);
   plus_sarg = class_name;
   ret_val14 = STR_ap2004550586(plus_self4, plus_sarg);
   plus_self5 = ret_val14;
   plus_sarg1 = ((STR) &name18);
   ret_val15 = STR_ap2004550586(plus_self5, plus_sarg1);
   PROG_err_STR(ATTR(self,prog), ret_val15);
   SATTR(self,missing_classes,FSETST1404644833(ATTR(self,missing_classes), class_name));
  }
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_IT1532669498(OPT_ITER self, dAM_EXPR dest1, AM_CURSOR c, BOOL need_safe_attr, BOOL in_conditional) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR is_not_used_in_l;
 dAM is_not_used_in_l1;
 BOOL ret_val1 = BOOL_zero;
 AM_CURSOR c11;
 AM_CURSOR is_const_in_loop;
 dAM is_const_in_loop1;
 BOOL ret_val2 = BOOL_zero;
 AM_CURSOR c12;
 if (in_conditional) {
  ret_val = FALSE;
  return ret_val;
 }
 else {
  is_not_used_in_l = c;
  is_not_used_in_l1 = ((dAM) dest1);
  c11 = AM_CUR1358945253(((AM_CURSOR) NULL), is_not_used_in_l);
  AM_CUR1223064464(c11);
  AM_CUR897865267(c11);
  ret_val1 = AM_CUR1926902782(is_not_used_in_l, c11, is_not_used_in_l1);
  if (ret_val1) {
   if (need_safe_attr) {
    if (dest1==NULL) {
     goto other299;
    } else
    switch (TAG(dest1)) {
     case AM_LOCAL_EXPR_tag:
      ret_val = AM_CUR762735158(c, ((dAM) dest1));
      return ret_val; break;
     default: ;
     other299: ;
      ret_val = FALSE;
      return ret_val;
    }
   }
   else {
    is_const_in_loop = c;
    is_const_in_loop1 = ((dAM) dest1);
    c12 = AM_CUR1358945253(((AM_CURSOR) NULL), is_const_in_loop);
    AM_CUR1223064464(c12);
    AM_CUR897865267(c12);
    ret_val2 = AM_CUR750559803(is_const_in_loop, c12, is_const_in_loop1);
    ret_val = ret_val2;
    return ret_val;
   }
  }
  else {
   ret_val = FALSE;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_IT2079146999(OPT_ITER self, dAM am) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM a;
 OPT_ITER prog_self;
 PROG ret_val1;
 dAM L2;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val1, am);
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
   L2 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   a = L2;
   if (a==NULL) {
    goto other294;
   } else
   switch (TAG(a)) {
    case AM_ATTR_EXPR_tag:
     ret_val = TRUE;
     return ret_val; break;
    default: ;
    other294: ;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_IT24605408(OPT_ITER self, dAM_EXPR src11, AM_CURSOR c, BOOL need_safe_attr, BOOL in_conditional) {
 BOOL ret_val = BOOL_zero;
 AM_CURSOR is_non_fatal_and;
 dAM is_non_fatal_and1;
 BOOL ret_val1 = BOOL_zero;
 AM_CURSOR is_non_fatal_and2;
 dAM is_non_fatal_and3;
 BOOL ret_val2 = BOOL_zero;
 AM_CURSOR c11;
 AM_CURSOR is_const_in_loop;
 dAM is_const_in_loop1;
 BOOL ret_val3 = BOOL_zero;
 AM_CURSOR c12;
 AM_LOOP_STMT L1;
 AM_CURSOR L2;
 if (in_conditional) {
  is_non_fatal_and = c;
  is_non_fatal_and1 = ((dAM) src11);
  L1=AM_CUR153953493(is_non_fatal_and);
  L2 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_non_fatal_and,prog), ((dAM) ATTR(L1,body)));
  ret_val1 = AM_CUR2135357807(is_non_fatal_and, L2, is_non_fatal_and1);
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  if (need_safe_attr) {
   is_non_fatal_and2 = c;
   is_non_fatal_and3 = ((dAM) src11);
   c11 = AM_CUR1358945253(((AM_CURSOR) NULL), is_non_fatal_and2);
   AM_CUR1223064464(c11);
   AM_CUR897865267(c11);
   ret_val2 = AM_CUR2135357807(is_non_fatal_and2, c11, is_non_fatal_and3);
   ret_val = ret_val2;
   return ret_val;
  }
  else {
   is_const_in_loop = c;
   is_const_in_loop1 = ((dAM) src11);
   c12 = AM_CUR1358945253(((AM_CURSOR) NULL), is_const_in_loop);
   AM_CUR1223064464(c12);
   AM_CUR897865267(c12);
   ret_val3 = AM_CUR750559803(is_const_in_loop, c12, is_const_in_loop1);
   ret_val = ret_val3;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_IT59245853(OPT_ITER self, AM_ROUT_DEF func, AM_ITE1809135850 am, AM_CURSOR c, BOOL do_optimization, BOOL need_safe_attr, BOOL in_conditional) {
 BOOL ret_val = BOOL_zero;
 BOOL ok = BOOL_zero;
 dAM_EXPR n;
 OPT_ITER prog_self;
 PROG ret_val1;
 OPT_ITER prog_self1;
 PROG ret_val2;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 OUT ret_val4;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val6;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OPT_ITER prog_self2;
 PROG ret_val9;
 OPT_ITER prog_self3;
 PROG ret_val10;
 OPT_ITER prog_self4;
 PROG ret_val11;
 OUT create_self1;
 OUT ret_val12;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val13;
 FILE1 r3;
 OPT_ITER prog_self5;
 PROG ret_val14;
 OPT_ITER prog_self6;
 PROG ret_val15;
 OUT create_self2;
 OUT ret_val16;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val17;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val18;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val19;
 FILE1 r5;
 AM_IS_VOID_EXPR create_self4;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IS_VOID_EXPR ret_val20;
 AM_IS_VOID_EXPR r6;
 OPT_ITER prog_self7;
 PROG ret_val21;
 OUT create_self5;
 OUT ret_val22;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val23;
 FILE1 r7;
 OUT create_self6;
 OUT ret_val24;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val25;
 FILE1 r8;
 OPT_ITER prog_self8;
 PROG ret_val26;
 OUT create_self7;
 OUT ret_val27;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val28;
 FILE1 r9;
 PROG L1;
 BOOL L2;
 BOOL L31_;
 PROG L4;
 extern STR Trying1702284880;
 FILE1 L5;
 OB L6;
 FILE1 L8;
 OB L9;
 extern STR name21;
 FILE1 L11;
 OB L12;
 PROG L14;
 dINLINE L15;
 AM_LOOP_STMT L16;
 dINLINE L17;
 BOOL L18;
 BOOL L191_;
 PROG L20;
 extern STR Oofite1603034369;
 dSTAT L21;
 PROG L22;
 extern STR done2;
 FILE1 L23;
 OB L24;
 BOOL L26;
 PROG L27;
 PROG L28;
 extern STR iterco548215998;
 FILE1 L29;
 OB L30;
 extern STR name9;
 FILE1 L33;
 OB L34;
 AM_IS_VOID_EXPR L36;
 OB L37;
 PROG L38;
 extern STR iterco984484264;
 FILE1 L39;
 OB L40;
 extern STR name9;
 FILE1 L42;
 OB L43;
 PROG L45;
 extern STR noway;
 FILE1 L46;
 OB L47;
 prog_self = self;
 ret_val1 = OPT_CO233462019;
 L1=ret_val1;
 L2 = ATTR(L1,inline_iters);
 L31_=!(L2); 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 ok = FALSE;
 prog_self1 = self;
 ret_val2 = OPT_CO233462019;
 L4=ret_val2;
 if (ATTR(L4,opt_debug)) {
  create_self = ((OUT) NULL);
  ret_val3 = create_self;
  plus_self = ret_val3;
  plus_s = ((STR) &Trying1702284880);
  stdout_self = ((FILE1) NULL);
  L6=ZALLOC(sizeof(struct FILE1_struct));
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FILE1_tag;
  L5 = ((FILE1) L6);
  r = L5;
  
  SATTR(r,fp,stdout);
  ret_val5 = r;
  FILE_plus_STR(ret_val5, plus_s);
  ret_val4 = plus_self;
  plus_self1 = ret_val4;
  plus_s1 = ATTR(ATTR(am,fun),str);
  stdout_self1 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stdout);
  ret_val7 = r1;
  FILE_plus_STR(ret_val7, plus_s1);
  ret_val6 = plus_self1;
  plus_self2 = ret_val6;
  plus_s2 = ((STR) &name21);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L11 = ((FILE1) L12);
  r2 = L11;
  
  SATTR(r2,fp,stdout);
  ret_val8 = r2;
  FILE_plus_STR(ret_val8, plus_s2);
 }
 prog_self2 = self;
 ret_val9 = OPT_CO233462019;
 L14=ret_val9;
 L15 = ATTR(L14,inliner);
 L16 = AM_CUR153953493(c);
 L17 = L15;
 n = (*dINLIN961094327[TAG(L17)])(L17, func, L16, am);
 L18 = SYSOBEQ(((OB) n),((OB) am));
 L191_=!(L18); 
 if (L191_) {
  prog_self3 = self;
  ret_val10 = OPT_CO233462019;
  L20=ret_val10;
  L21 = ATTR(L20,stat1);
  (*dSTAT_incr_STR[TAG(L21)])(L21, ((STR) &Oofite1603034369));
  prog_self4 = self;
  ret_val11 = OPT_CO233462019;
  L22=ret_val11;
  if (ATTR(L22,opt_debug)) {
   create_self1 = ((OUT) NULL);
   ret_val12 = create_self1;
   plus_self3 = ret_val12;
   plus_s3 = ((STR) &done2);
   stdout_self3 = ((FILE1) NULL);
   L24=ZALLOC(sizeof(struct FILE1_struct));
   if (L24==NULL) FATAL("Unable to allocate more memory");
   ((OB)L24)->header.tag=FILE1_tag;
   L23 = ((FILE1) L24);
   r3 = L23;
   
   SATTR(r3,fp,stdout);
   ret_val13 = r3;
   FILE_plus_STR(ret_val13, plus_s3);
  }
  prog_self5 = self;
  ret_val14 = OPT_CO233462019;
  L27=ret_val14;
  if (ATTR(L27,hoist_iter_init)) {
   L26 = do_optimization;
  } else {
   L26 = FALSE;
  }
  if (L26) {
   if (n==NULL) {
    goto other318;
   } else
   switch (TAG(n)) {
    case AM_STMT_EXPR_tag:
     prog_self6 = self;
     ret_val15 = OPT_CO233462019;
     L28=ret_val15;
     if (ATTR(L28,opt_debug)) {
      create_self2 = ((OUT) NULL);
      ret_val16 = create_self2;
      plus_self4 = ret_val16;
      plus_s4 = ((STR) &iterco548215998);
      stdout_self4 = ((FILE1) NULL);
      L30=ZALLOC(sizeof(struct FILE1_struct));
      if (L30==NULL) FATAL("Unable to allocate more memory");
      ((OB)L30)->header.tag=FILE1_tag;
      L29 = ((FILE1) L30);
      r4 = L29;
      
      SATTR(r4,fp,stdout);
      ret_val17 = r4;
      FILE_plus_STR(ret_val17, plus_s4);
      AM_OUT1439666796(((AM_OUT) NULL), ((dAM) n));
      create_self3 = ((OUT) NULL);
      ret_val18 = create_self3;
      plus_self5 = ret_val18;
      plus_s5 = ((STR) &name9);
      stdout_self5 = ((FILE1) NULL);
      L34=ZALLOC(sizeof(struct FILE1_struct));
      if (L34==NULL) FATAL("Unable to allocate more memory");
      ((OB)L34)->header.tag=FILE1_tag;
      L33 = ((FILE1) L34);
      r5 = L33;
      
      SATTR(r5,fp,stdout);
      ret_val19 = r5;
      FILE_plus_STR(ret_val19, plus_s5);
     }
     create_self4 = ((AM_IS_VOID_EXPR) NULL);
     create_source = SFILE_ID_zero;
     L37=ZALLOC(sizeof(struct AM_IS_VOID_EXPR_struct));
     if (L37==NULL) FATAL("Unable to allocate more memory");
     ((OB)L37)->header.tag=AM_IS_VOID_EXPR_tag;
     L36 = ((AM_IS_VOID_EXPR) L37);
     r6 = L36;
     SATTR(r6,source1,create_source);
     ret_val20 = r6;
     AM_CUR915996644(c, ((dAM_EXPR) ret_val20));
     OPT_IT1323213400(self, func, ((AM_STMT_EXPR) n), c, am, need_safe_attr, in_conditional);
     prog_self7 = self;
     ret_val21 = OPT_CO233462019;
     L38=ret_val21;
     if (ATTR(L38,opt_debug)) {
      create_self5 = ((OUT) NULL);
      ret_val22 = create_self5;
      plus_self6 = ret_val22;
      plus_s6 = ((STR) &iterco984484264);
      stdout_self6 = ((FILE1) NULL);
      L40=ZALLOC(sizeof(struct FILE1_struct));
      if (L40==NULL) FATAL("Unable to allocate more memory");
      ((OB)L40)->header.tag=FILE1_tag;
      L39 = ((FILE1) L40);
      r7 = L39;
      
      SATTR(r7,fp,stdout);
      ret_val23 = r7;
      FILE_plus_STR(ret_val23, plus_s6);
      AM_OUT1439666796(((AM_OUT) NULL), ((dAM) n));
      create_self6 = ((OUT) NULL);
      ret_val24 = create_self6;
      plus_self7 = ret_val24;
      plus_s7 = ((STR) &name9);
      stdout_self7 = ((FILE1) NULL);
      L43=ZALLOC(sizeof(struct FILE1_struct));
      if (L43==NULL) FATAL("Unable to allocate more memory");
      ((OB)L43)->header.tag=FILE1_tag;
      L42 = ((FILE1) L43);
      r8 = L42;
      
      SATTR(r8,fp,stdout);
      ret_val25 = r8;
      FILE_plus_STR(ret_val25, plus_s7);
     } break;
    default: ;
    other318: ;
   }
  }
  AM_CUR915996644(c, n);
  ok = TRUE;
 }
 else {
  prog_self8 = self;
  ret_val26 = OPT_CO233462019;
  L45=ret_val26;
  if (ATTR(L45,opt_debug)) {
   create_self7 = ((OUT) NULL);
   ret_val27 = create_self7;
   plus_self8 = ret_val27;
   plus_s8 = ((STR) &noway);
   stdout_self8 = ((FILE1) NULL);
   L47=ZALLOC(sizeof(struct FILE1_struct));
   if (L47==NULL) FATAL("Unable to allocate more memory");
   ((OB)L47)->header.tag=FILE1_tag;
   L46 = ((FILE1) L47);
   r9 = L46;
   
   SATTR(r9,fp,stdout);
   ret_val28 = r9;
   FILE_plus_STR(ret_val28, plus_s8);
  }
 }
 ret_val = ok;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL OPT_IT948201730(OPT_ITER self, AM_ROUT_DEF func, AM_ROU1916046290 am, AM_CURSOR c) {
 BOOL ret_val = BOOL_zero;
 BOOL ok = BOOL_zero;
 dAM_EXPR n;
 OPT_ITER prog_self;
 PROG ret_val1;
 OPT_ITER make_sure_emitte;
 SIG make_sure_emitte1;
 dBACK_END cgen;
 OPT_ITER prog_self1;
 PROG ret_val2;
 CGEN make_sure_emitte2;
 SIG make_sure_emitte3;
 AM_ROUT_DEF ard;
 OPT_ITER prog_self2;
 PROG ret_val3;
 OUT create_self;
 OUT ret_val4;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r;
 OPT_ITER make_sure_emitte4;
 SIG make_sure_emitte5;
 dBACK_END cgen1;
 OPT_ITER prog_self3;
 PROG ret_val6;
 CGEN make_sure_emitte6;
 SIG make_sure_emitte7;
 AM_ROUT_DEF ard1;
 OPT_ITER prog_self4;
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
 OPT_ITER prog_self5;
 PROG ret_val14;
 OPT_ITER prog_self6;
 PROG ret_val15;
 OPT_ITER prog_self7;
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
 OPT_ITER prog_self8;
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
    FATAL("No applicable type in typecase\nin OPT_ITER::inline_func(AM_ROUT_DEF,AM_ROUT_CALL_EXPR,AM_CURSOR):BOOL\n./Back/optimize.sa:159:14");
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
     FATAL("No applicable type in typecase\nin OPT_ITER::inline_func(AM_ROUT_DEF,AM_ROUT_CALL_EXPR,AM_CURSOR):BOOL\n./Back/optimize.sa:159:14");
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
   goto other297;
  } else
  switch (TAG(n)) {
   case AM_STMT_EXPR_tag:
    SATTR(((AM_STMT_EXPR) n),replaced,((dAM_EXPR) am)); break;
   default: ;
   other297: ;
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

CONVERT CONVER262703763(CONVERT self, PARSER parser, BOOL conv, STR fname) {
 CONVERT ret_val;
 CONVERT res;
 STR header_label;
 BOOL already_converte = BOOL_zero;
 FSTR L11;
 STR L21;
 BFILE fp;
 FSTR length_self;
 INT ret_val1 = INT_zero;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 PARSER source_loc_self1;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 BFILE open_for_write_s;
 STR open_for_write_n;
 BFILE ret_val7;
 BFILE r;
 BFILE error_self;
 BOOL ret_val8 = BOOL_zero;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val10;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val11;
 UNIX exit_self;
 INT exit_code = INT_zero;
 BFILE close_self;
 BFILE open_for_write_s1;
 STR open_for_write_n1;
 BFILE ret_val12;
 BFILE r1;
 BFILE error_self1;
 BOOL ret_val13 = BOOL_zero;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val14;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val15;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self8;
 STR plus_sarg8;
 STR ret_val16;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 CONVERT L3;
 OB L4;
 extern STR Sather784374389;
 INT L5;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 CHAR L10;
 CHAR L12;
 CHAR L13;
 CHAR L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 BOOL L20;
 extern STR hasbee1196328164;
 BOOL L22;
 extern STR hasbee1352259740;
 extern STR S10;
 BFILE L23;
 OB L24;
 extern STR wb;
 STR L25;
 STR L26;
 BOOL L27;
 extern STR Cannotcreate;
 extern STR S10;
 extern STR Intern28965746;
 BFILE L28;
 OB L29;
 extern STR wb;
 STR L30;
 STR L31;
 BOOL L32;
 extern STR Cannotopen;
 extern STR forwriting;
 extern STR Intern28965746;
 L4=ZALLOC(sizeof(struct CONVERT_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=CONVERT_tag;
 L3 = ((CONVERT) L4);
 res = L3;
 header_label = ((STR) &Sather784374389);
 length_self = SFILE_ID_source;
 ret_val1 = FSTR_sizerINT(length_self);
 L5 = ret_val1;
 L6 = STR_lengthrINT(header_label);
 L71_=(L5)<(L6); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  already_converte = TRUE;
  {
   struct FSTR_eltbrCHAR_frame_struct other1_0;
FSTR_eltbrCHAR_frame noname1 = &other1_0;
   struct STR_eltbrCHAR_frame_struct other1_1;
STR_eltbrCHAR_frame noname2 = &other1_1;
   L11 = SFILE_ID_source;
   L21 = header_label;
   noname1->self = L11;
   noname1->state = 0;
   noname2->self = L21;
   noname2->state = 0;
   while (1) {
    L10 = FSTR_eltbrCHAR(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    L12 = L10;
    L13 = STR_eltbrCHAR(noname2);
    if (noname2->state == -1) {
     goto after_loop;
    }
    L14 = L13;
    L151_=L12==L14; 
    L16 = L151_;
    L171_=!(L16); 
    if (L171_) {
     already_converte = FALSE;
     goto after_loop;
    }
   }
  }
  after_loop: ;
 }
 else {
  already_converte = FALSE;
 }
 if (conv) {
  L191_=!(already_converte); 
  L18 = L191_;
 } else {
  L18 = FALSE;
 }
 SATTR(res,need_to_convert,L18);
 if (conv) {
  L20 = already_converte;
 } else {
  L20 = FALSE;
 }
 if (L20) {
  set_eloc_self = ATTR(parser,prog);
  source_loc_self = parser;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val2;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  plus_self = fname;
  plus_sarg = ((STR) &hasbee1196328164);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  PROG_warning_STR(ATTR(parser,prog), ret_val3);
  SATTR(parser,version_1_0,FALSE);
 }
 if (ATTR(parser,version_1_0)) {
  L22 = already_converte;
 } else {
  L22 = FALSE;
 }
 if (L22) {
  set_eloc_self1 = ATTR(parser,prog);
  source_loc_self1 = parser;
  ret_val4 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val4;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  plus_self1 = fname;
  plus_sarg1 = ((STR) &hasbee1352259740);
  ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
  PROG_warning_STR(ATTR(parser,prog), ret_val5);
  SATTR(parser,version_1_0,FALSE);
 }
 if (ATTR(res,need_to_convert)) {
  open_for_write_s = ((BFILE) NULL);
  plus_self2 = fname;
  plus_sarg2 = ((STR) &S10);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  open_for_write_n = ret_val6;
  L24=ZALLOC(sizeof(struct BFILE_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=BFILE_tag;
  L23 = ((BFILE) L24);
  r = L23;
  L25 = open_for_write_n;
  L26 = ((STR) &wb);
  SATTR(r,fp,fopen(((L25==NULL)?NULL:L25->arr_part), ((L26==NULL)?NULL:L26->arr_part)));
  ret_val7 = r;
  fp = ret_val7;
  error_self = fp;
  if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
   L27 = TRUE;
  } else {
   L27 = ferror(ATTR(error_self,fp));
  }
  ret_val8 = L27;
  if (ret_val8) {
   barf_self = ATTR(parser,prog);
   plus_self3 = ((STR) &Cannotcreate);
   plus_sarg3 = fname;
   ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val9;
   plus_sarg4 = ((STR) &S10);
   ret_val10 = STR_ap2004550586(plus_self4, plus_sarg4);
   barf_msg = ret_val10;
   barf_at_self = barf_self;
   barf_at_msg = barf_msg;
   barf_at_at = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self, barf_at_at);
   plus_self5 = ((STR) &Intern28965746);
   plus_sarg5 = barf_at_msg;
   ret_val11 = STR_ap2004550586(plus_self5, plus_sarg5);
   PROG_err_STR(barf_at_self, ret_val11);
   exit_self = ((UNIX) NULL);
   exit_code = 1;
   exit(exit_code);
   ret_val = ((CONVERT) NULL);
   return ret_val;
  }
  BFILE_plus_FSTR(fp, SFILE_ID_source);
  close_self = fp;
  fclose(ATTR(close_self,fp));
  open_for_write_s1 = ((BFILE) NULL);
  open_for_write_n1 = fname;
  L29=ZALLOC(sizeof(struct BFILE_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=BFILE_tag;
  L28 = ((BFILE) L29);
  r1 = L28;
  L30 = open_for_write_n1;
  L31 = ((STR) &wb);
  SATTR(r1,fp,fopen(((L30==NULL)?NULL:L30->arr_part), ((L31==NULL)?NULL:L31->arr_part)));
  ret_val12 = r1;
  SATTR(res,file1,ret_val12);
  error_self1 = ATTR(res,file1);
  if ((ATTR(error_self1,fp)==((EXT_OB) NULL))) {
   L32 = TRUE;
  } else {
   L32 = ferror(ATTR(error_self1,fp));
  }
  ret_val13 = L32;
  if (ret_val13) {
   barf_self1 = ATTR(parser,prog);
   plus_self6 = ((STR) &Cannotopen);
   plus_sarg6 = fname;
   ret_val14 = STR_ap2004550586(plus_self6, plus_sarg6);
   plus_self7 = ret_val14;
   plus_sarg7 = ((STR) &forwriting);
   ret_val15 = STR_ap2004550586(plus_self7, plus_sarg7);
   barf_msg1 = ret_val15;
   barf_at_self1 = barf_self1;
   barf_at_msg1 = barf_msg1;
   barf_at_at1 = ((dPROG_ERR) NULL);
   PROG_e176405615(barf_at_self1, barf_at_at1);
   plus_self8 = ((STR) &Intern28965746);
   plus_sarg8 = barf_at_msg1;
   ret_val16 = STR_ap2004550586(plus_self8, plus_sarg8);
   PROG_err_STR(barf_at_self1, ret_val16);
   exit_self1 = ((UNIX) NULL);
   exit_code1 = 1;
   exit(exit_code1);
   ret_val = ((CONVERT) NULL);
   return ret_val;
  }
  BFILE_plus_STR(ATTR(res,file1), header_label);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CONVER649045550(CONVERT self, IDENT feature_name) {
 INT i = INT_zero;
 BOOL done = BOOL_zero;
 INT start1 = INT_zero;
 BOOL hot = BOOL_zero;
 CHAR c = CHAR_zero;
 INT in_param = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT j = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val = CHAR_zero;
 CONVERT set_end_self;
 INT set_end_i = INT_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val2 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val3 = CHAR_zero;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val4 = CHAR_zero;
 BFILE plus_self;
 CHAR plus_c = CHAR_zero;
 BFILE flush_self;
 CHAR L31_;
 CHAR L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L81_;
 INT L91_;
 BOOL L101_;
 CHAR L121_;
 BOOL L131_;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 INT L171_;
 CHAR L181_;
 BOOL L191_;
 INT L201_;
 BOOL L221_;
 INT L231_;
 BOOL L24;
 BOOL L251_;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 BOOL L291_;
 INT L301_;
 BOOL L321_;
 BOOL L331_;
 extern STR once2;
 INT L341_,L341_m;
 INT L35;
 CHAR L361_;
 CHAR L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 CHAR L411_;
 INT L421_;
 if (ATTR(self,need_to_convert)) {
  i = SFILE_ID_pos;
  while (1) {
   aget_self = SFILE_ID_source;
   aget_ind = i;
   L31_=ARR((FSTR)aget_self,aget_ind); 
   ret_val = L31_;
   L4 = ret_val;
   L51_=L4=='('; 
   L6 = L51_;
   L71_=!(L6); 
   if (L71_) {
   }
   else {
    goto after_loop;
   }
   L81_=INTMINUS(i,1); 
   i = L81_;
  }
  after_loop: ;
  set_end_self = self;
  set_end_i = i;
  if (ATTR(set_end_self,need_to_convert)) {
   SATTR(set_end_self,end_pos,set_end_i);
   CONVER911997526(set_end_self);
  }
  L91_=INTPLUS(i,1); 
  i = L91_;
  done = FALSE;
  while (1) {
   L101_=!(done); 
   if (L101_) {
   }
   else {
    goto after_loop1;
   }
   start1 = i;
   hot = FALSE;
   while (1) {
    aget_self1 = SFILE_ID_source;
    aget_ind1 = i;
    L121_=ARR((FSTR)aget_self1,aget_ind1); 
    ret_val1 = L121_;
    c = ret_val1;
    L131_=c=='{'; 
    if (L131_) {
     in_param = 1;
     while (1) {
      L141_=(in_param)==(0); 
      L15 = L141_;
      L161_=!(L15); 
      if (L161_) {
      }
      else {
       goto after_loop3;
      }
      L171_=INTPLUS(i,1); 
      i = L171_;
      aget_self2 = SFILE_ID_source;
      aget_ind2 = i;
      L181_=ARR((FSTR)aget_self2,aget_ind2); 
      ret_val2 = L181_;
      c = ret_val2;
      L191_=c=='{'; 
      if (L191_) {
       L201_=INTPLUS(in_param,1); 
       in_param = L201_;
      }
      else {
       L221_=c=='}'; 
       if (L221_) {
        L231_=INTMINUS(in_param,1); 
        in_param = L231_;
       }
      }
     }
     after_loop3: ;
    }
    L251_=c==','; 
    if (L251_) {
     L24 = TRUE;
    } else {
     L261_=c==')'; 
     L24 = L261_;
    }
    L27 = L24;
    L281_=!(L27); 
    if (L281_) {
    }
    else {
     goto after_loop2;
    }
    L291_=c=='!'; 
    if (L291_) {
     hot = TRUE;
    }
    L301_=INTPLUS(i,1); 
    i = L301_;
   }
   after_loop2: ;
   L321_=c==')'; 
   if (L321_) {
    done = TRUE;
   }
   L331_=!(hot); 
   if (L331_) {
    BFILE_plus_STR(ATTR(self,file1), ((STR) &once2));
   }
   {
    BOOL f_L341_ = TRUE;
    L11 = start1;
    L21 = i;
    L341_=L11-1;L341_m=L21; 
    while (1) {
     if(L341_++>=L341_m)  goto after_loop4; 
     j = L341_;
     aget_self3 = SFILE_ID_source;
     aget_ind3 = j;
     L361_=ARR((FSTR)aget_self3,aget_ind3); 
     ret_val3 = L361_;
     L37 = ret_val3;
     L381_=L37=='!'; 
     L39 = L381_;
     L401_=!(L39); 
     if (L401_) {
      plus_self = ATTR(self,file1);
      aget_self4 = SFILE_ID_source;
      aget_ind4 = j;
      L411_=ARR((FSTR)aget_self4,aget_ind4); 
      ret_val4 = L411_;
      plus_c = ret_val4;
      fputc(plus_c, ATTR(plus_self,fp));
     }
    }
   }
   after_loop4: ;
   L421_=INTPLUS(i,1); 
   i = L421_;
  }
  after_loop1: ;
  flush_self = ATTR(self,file1);
  fflush(ATTR(flush_self,fp));
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONVER840793676(CONVERT self, CHAR ch) {
 INT i = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val = CHAR_zero;
 CONVERT set_end_self;
 INT set_end_i = INT_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 INT L61_;
 INT L71_;
 i = SFILE_ID_pos;
 while (1) {
  aget_self = SFILE_ID_source;
  aget_ind = i;
  L11_=ARR((FSTR)aget_self,aget_ind); 
  ret_val = L11_;
  L2 = ret_val;
  L31_=L2==ch; 
  L4 = L31_;
  L51_=!(L4); 
  if (L51_) {
  }
  else {
   goto after_loop;
  }
  L61_=INTMINUS(i,1); 
  i = L61_;
 }
 after_loop: ;
 set_end_self = self;
 L71_=INTMINUS(i,1); 
 set_end_i = L71_;
 if (ATTR(set_end_self,need_to_convert)) {
  SATTR(set_end_self,end_pos,set_end_i);
  CONVER911997526(set_end_self);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CONVER911997526(CONVERT self) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val = CHAR_zero;
 BFILE plus_self;
 CHAR plus_c = CHAR_zero;
 BFILE flush_self;
 INT L31_,L31_m;
 INT L4;
 CHAR L51_;
 {
  BOOL f_L31_ = TRUE;
  L11 = ATTR(self,start_pos);
  L21 = ATTR(self,end_pos);
  L31_=L11-1;L31_m=L21; 
  while (1) {
   if(L31_++>=L31_m)  goto after_loop; 
   i = L31_;
   plus_self = ATTR(self,file1);
   aget_self = SFILE_ID_source;
   aget_ind = i;
   L51_=ARR((FSTR)aget_self,aget_ind); 
   ret_val = L51_;
   plus_c = ret_val;
   fputc(plus_c, ATTR(plus_self,fp));
  }
 }
 after_loop: ;
 flush_self = ATTR(self,file1);
 fflush(ATTR(flush_self,fp));
}


#undef IS_ITER
#define IS_ITER 0

void OPT_IT1323213400(OPT_ITER self, AM_ROUT_DEF func, AM_STMT_EXPR n, AM_CURSOR c, AM_ITE1809135850 it, BOOL need_safe_attr, BOOL in_conditional) {
 AM_CURSOR cc;
 AM_CURSOR L11;
 dAM am;
 BOOL dummy1 = BOOL_zero;
 dAM_STMT am11;
 AM_LOCAL_EXPR lbool;
 BOOL move_lbool = BOOL_zero;
 dAM_EXPR ifx;
 AM_CURSOR c11;
 BOOL found_it = BOOL_zero;
 BOOL ignore_next_refe = BOOL_zero;
 AM_CURSOR L21;
 dAM d;
 dAM_EXPR dst;
 dAM_STMT s;
 dAM_EXPR t;
 dAM_STMT tr;
 dAM_EXPR src11;
 AM_COMMENT_STMT comment1;
 dAM_STMT pr;
 AM_CURSOR inlined;
 dAM_EXPR l;
 dAM_STMT ifs;
 dAM_STMT nifs;
 dAM_STMT p;
 BOOL do_move = BOOL_zero;
 BOOL ignore_stmt = BOOL_zero;
 BOOL delete_it = BOOL_zero;
 dAM_EXPR adest;
 dAM_EXPR adest1;
 dAM_STMT amm;
 dAM_EXPR l1;
 OPT_ITER prog_self;
 PROG ret_val;
 OPT_ITER prog_self1;
 PROG ret_val1;
 OUT create_self;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 AM_CURSOR is_const_in_loop;
 dAM is_const_in_loop1;
 BOOL ret_val4 = BOOL_zero;
 OPT_ITER prog_self2;
 PROG ret_val5;
 OUT create_self1;
 OUT ret_val6;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val9;
 FILE1 r2;
 OPT_ITER prog_self3;
 PROG ret_val10;
 OUT create_self3;
 OUT ret_val11;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val12;
 FILE1 r3;
 OPT_ITER prog_self4;
 PROG ret_val13;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val14 = BOOL_zero;
 OPT_ITER prog_self5;
 PROG ret_val15;
 OUT create_self4;
 OUT ret_val16;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val17;
 FILE1 r4;
 OPT_ITER prog_self6;
 PROG ret_val18;
 OUT create_self5;
 OUT ret_val19;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val20;
 FILE1 stdout_self5;
 FILE1 ret_val21;
 FILE1 r5;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val22;
 FILE1 stdout_self6;
 FILE1 ret_val23;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val24;
 FILE1 r7;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val25 = BOOL_zero;
 OPT_ITER prog_self7;
 PROG ret_val26;
 OUT create_self6;
 OUT ret_val27;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val28;
 FILE1 r8;
 OPT_ITER prog_self8;
 PROG ret_val29;
 OUT create_self7;
 OUT ret_val30;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val31;
 FILE1 r9;
 OPT_ITER prog_self9;
 PROG ret_val32;
 OUT create_self8;
 OUT ret_val33;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val34;
 FILE1 r10;
 OPT_ITER prog_self10;
 PROG ret_val35;
 OUT create_self9;
 OUT ret_val36;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val37;
 FILE1 r11;
 OPT_ITER prog_self11;
 PROG ret_val38;
 OUT create_self10;
 OUT ret_val39;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val40;
 FILE1 r12;
 OPT_ITER prog_self12;
 PROG ret_val41;
 OUT create_self11;
 OUT ret_val42;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val43;
 FILE1 r13;
 AM_LOCAL_EXPR is_eq_self2;
 OB is_eq_a2;
 BOOL ret_val44 = BOOL_zero;
 OPT_ITER prog_self13;
 PROG ret_val45;
 OUT create_self12;
 OUT ret_val46;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val47;
 FILE1 r14;
 OPT_ITER prog_self14;
 PROG ret_val48;
 OUT create_self13;
 OUT ret_val49;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val50;
 FILE1 r15;
 AM_COMMENT_STMT create_self14;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val51;
 AM_COMMENT_STMT r16;
 STR plus_self16;
 STR plus_sarg;
 STR ret_val52;
 OPT_ITER prog_self15;
 PROG ret_val53;
 AM_CURSOR next_self;
 dAM dummy2;
 AM_LOCAL_EXPR is_eq_self3;
 OB is_eq_a3;
 BOOL ret_val54 = BOOL_zero;
 AM_CURSOR next_self1;
 dAM dummy3;
 AM_CURSOR next_self2;
 dAM dummy4;
 OUT create_self15;
 OUT ret_val55;
 OUT plus_self17;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val56;
 FILE1 r17;
 OUT create_self16;
 OUT ret_val57;
 OUT plus_self18;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val58;
 FILE1 r18;
 OUT create_self17;
 OUT ret_val59;
 OUT plus_self19;
 STR plus_s18;
 FILE1 stdout_self18;
 FILE1 ret_val60;
 FILE1 r19;
 UNIX exit_self;
 INT exit_code = INT_zero;
 OPT_ITER prog_self16;
 PROG ret_val61;
 AM_CURSOR is_not_used_in_f;
 dAM is_not_used_in_f1;
 BOOL ret_val62 = BOOL_zero;
 AM_CURSOR c12;
 AM_CURSOR is_const_in_func;
 dAM is_const_in_func1;
 BOOL ret_val63 = BOOL_zero;
 AM_CURSOR c13;
 AM_LOCAL_EXPR is_eq_self4;
 OB is_eq_a4;
 BOOL ret_val64 = BOOL_zero;
 OPT_ITER prog_self17;
 PROG ret_val65;
 OUT create_self18;
 OUT ret_val66;
 OUT plus_self20;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val67;
 FILE1 r20;
 OUT create_self19;
 OUT ret_val68;
 OUT plus_self21;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val69;
 FILE1 r21;
 AM_CURSOR is_not_used_in_f2;
 dAM is_not_used_in_f3;
 BOOL ret_val70 = BOOL_zero;
 AM_CURSOR c14;
 OUT create_self20;
 OUT ret_val71;
 OUT plus_self22;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val72;
 FILE1 r22;
 AM_CURSOR is_const_in_func2;
 dAM is_const_in_func3;
 BOOL ret_val73 = BOOL_zero;
 AM_CURSOR c15;
 OUT create_self21;
 OUT ret_val74;
 OUT plus_self23;
 STR plus_s22;
 FILE1 stdout_self22;
 FILE1 ret_val75;
 FILE1 r23;
 OPT_ITER prog_self18;
 PROG ret_val76;
 OPT_ITER prog_self19;
 PROG ret_val77;
 OPT_ITER prog_self20;
 PROG ret_val78;
 OPT_ITER prog_self21;
 PROG ret_val79;
 OUT create_self22;
 OUT ret_val80;
 OUT plus_self24;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val81;
 FILE1 r24;
 AM_OUT AM_one_stmt_self;
 dAM AM_one_stmt_a;
 dAM_STMT n11;
 AM_COMMENT_STMT create_self23;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val82;
 AM_COMMENT_STMT r25;
 AM_COMMENT_STMT create_self24;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val83;
 AM_COMMENT_STMT r26;
 AM_CURSOR next_stmt_self;
 dAM d11;
 AM_CURSOR next_stmt_self1;
 dAM d21;
 AM_LOCAL_EXPR is_eq_self5;
 OB is_eq_a5;
 BOOL ret_val84 = BOOL_zero;
 OPT_ITER prog_self22;
 PROG ret_val85;
 dAM L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 PROG L7;
 extern STR couldn149797082;
 FILE1 L8;
 OB L9;
 AM_LOOP_STMT L12;
 AM_CURSOR L13;
 BOOL L14;
 BOOL L151_;
 PROG L16;
 extern STR sorryb377685381;
 FILE1 L17;
 OB L18;
 extern STR name9;
 FILE1 L20;
 OB L22;
 AM_LOOP_STMT L24;
 INT L25;
 BOOL L261_;
 PROG L27;
 extern STR theloc642778558;
 FILE1 L28;
 OB L29;
 dAM L31;
 PROG L32;
 extern STR foundt1136726481;
 FILE1 L33;
 OB L34;
 INT L36;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 PROG L40;
 extern STR indent2;
 FILE1 L41;
 OB L42;
 dSTR L44;
 OB L45;
 FILE1 L46;
 OB L47;
 extern STR soassi178775884;
 FILE1 L49;
 OB L50;
 BOOL L52;
 BOOL L531_;
 PROG L54;
 extern STR wrongt551266388;
 FILE1 L55;
 OB L56;
 PROG L58;
 extern STR wrongt1197886949;
 FILE1 L59;
 OB L60;
 BOOL L63;
 BOOL L641_;
 INT L651_;
 INT L66;
 INT L671_;
 INT L68;
 BOOL L691_;
 dAM_STMT L70;
 PROG L71;
 extern STR Assign1969083730;
 FILE1 L72;
 OB L73;
 BOOL L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 PROG L79;
 extern STR Assign248586026;
 FILE1 L80;
 OB L81;
 PROG L83;
 extern STR Assign439881173;
 FILE1 L84;
 OB L85;
 PROG L87;
 extern STR assign1831176839;
 FILE1 L88;
 OB L89;
 PROG L91;
 extern STR therei1547416579;
 FILE1 L92;
 OB L93;
 PROG L95;
 extern STR itseem1831037758;
 FILE1 L96;
 OB L97;
 AM_COMMENT_STMT L99;
 OB L100;
 extern STR Initia1351694393;
 BOOL L102;
 BOOL L1031_;
 BOOL L104;
 BOOL L1051_;
 BOOL L106;
 BOOL L1071_;
 dAM_STMT L108;
 INT L1091_;
 INT L110;
 dAM L111;
 INT L1121_;
 INT L113;
 BOOL L1141_;
 BOOL L115;
 BOOL L1161_;
 extern STR inline160970948;
 FILE1 L117;
 OB L118;
 extern STR ifssays;
 FILE1 L120;
 OB L121;
 extern STR andinlinedisat;
 FILE1 L123;
 OB L124;
 BOOL L126;
 BOOL L127;
 BOOL L128;
 BOOL L129;
 BOOL L130;
 BOOL L131;
 BOOL L1321_;
 PROG L133;
 BOOL L134;
 BOOL L1351_;
 BOOL L136;
 BOOL L1371_;
 BOOL L1381_;
 BOOL L139;
 PROG L140;
 BOOL L141;
 BOOL L1421_;
 extern STR cannot1775150384;
 FILE1 L143;
 OB L144;
 BOOL L146;
 BOOL L1471_;
 extern STR cannot68435663;
 FILE1 L148;
 OB L149;
 BOOL L152;
 BOOL L1531_;
 extern STR cannot56504486;
 FILE1 L154;
 OB L155;
 BOOL L157;
 BOOL L1581_;
 extern STR cannot972067817;
 FILE1 L159;
 OB L160;
 PROG L162;
 BOOL L163;
 BOOL L1641_;
 PROG L165;
 BOOL L166;
 BOOL L1671_;
 PROG L168;
 BOOL L169;
 BOOL L1701_;
 dAM_STMT L171;
 dAM_STMT L172;
 dAM_STMT L173;
 dAM_STMT L174;
 dAM_STMT L175;
 PROG L176;
 extern STR moving93892770;
 FILE1 L177;
 OB L178;
 dAM_STMT L180;
 dAM_STMT L181;
 dAM_STMT L182;
 AM_COMMENT_STMT L183;
 OB L184;
 dAM_STMT L185;
 dAM_STMT L186;
 dAM_STMT L187;
 dAM_STMT L188;
 dAM_STMT L189;
 AM_COMMENT_STMT L190;
 OB L192;
 BOOL L1931_;
 BOOL L194;
 BOOL L195;
 BOOL L196;
 BOOL L1971_;
 BOOL L1981_;
 BOOL L1991_;
 dAM_STMT L200;
 BOOL L201;
 BOOL L2021_;
 BOOL L203;
 BOOL L204;
 PROG L205;
 extern STR Oifstmtsremoved;
 dSTAT L206;
 dAM_STMT L207;
 dAM_STMT L208;
 dAM_STMT L209;
 dAM_STMT L210;
 dAM_STMT L211;
 dAM_STMT L212;
 dAM_STMT L213;
 dAM_STMT L214;
 prog_self = self;
 ret_val = OPT_CO233462019;
 cc = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, ((dAM) ATTR(n,stmts)));
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = cc;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L3;
   if (am==NULL) {
    goto other303;
   } else
   switch (TAG(am)) {
    case AM_ROU1916046290_tag:
     dummy1 = OPT_IT948201730(self, func, ((AM_ROU1916046290) am), cc); break;
    default: ;
    other303: ;
   }
  }
 }
 after_loop: ;
 am11 = ATTR(n,stmts);
 lbool = ((AM_LOCAL_EXPR) NULL);
 move_lbool = TRUE;
 while (1) {
  L5 = (am11==((dAM_STMT) NULL));
  L61_=!(L5); 
  if (L61_) {
   L4 = (lbool==((AM_LOCAL_EXPR) NULL));
  } else {
   L4 = FALSE;
  }
  if (L4) {
  }
  else {
   goto after_loop1;
  }
  if (am11==NULL) {
   goto other304;
  } else
  switch (TAG(am11)) {
   case AM_IF_STMT_tag:
    ifx = ATTR(((AM_IF_STMT) am11),test1);
    if (ifx==NULL) {
     goto other305;
    } else
    switch (TAG(ifx)) {
     case AM_LOCAL_EXPR_tag:
      lbool = ((AM_LOCAL_EXPR) ifx); break;
     default: ;
     other305: ;
    } break;
   default: ;
   other304: ;
  }
 }
 after_loop1: ;
 if ((lbool==((AM_LOCAL_EXPR) NULL))) {
  prog_self1 = self;
  ret_val1 = OPT_CO233462019;
  L7=ret_val1;
  if (ATTR(L7,opt_debug)) {
   create_self = ((OUT) NULL);
   ret_val2 = create_self;
   plus_self = ret_val2;
   plus_s = ((STR) &couldn149797082);
   stdout_self = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r = L8;
   
   SATTR(r,fp,stdout);
   ret_val3 = r;
   FILE_plus_STR(ret_val3, plus_s);
  }
  return;
 }
 is_const_in_loop = c;
 is_const_in_loop1 = ((dAM) lbool);
 L12=AM_CUR153953493(is_const_in_loop);
 L13 = AM_CUR1277440868(((AM_CURSOR) NULL), ATTR(is_const_in_loop,prog), ((dAM) ATTR(L12,body)));
 ret_val4 = AM_CUR750559803(is_const_in_loop, L13, is_const_in_loop1);
 L14 = ret_val4;
 L151_=!(L14); 
 if (L151_) {
  prog_self2 = self;
  ret_val5 = OPT_CO233462019;
  L16=ret_val5;
  if (ATTR(L16,opt_debug)) {
   create_self1 = ((OUT) NULL);
   ret_val6 = create_self1;
   plus_self1 = ret_val6;
   plus_s1 = ((STR) &sorryb377685381);
   stdout_self1 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r1 = L17;
   
   SATTR(r1,fp,stdout);
   ret_val7 = r1;
   FILE_plus_STR(ret_val7, plus_s1);
   AM_OUT1439666796(((AM_OUT) NULL), ((dAM) lbool));
   create_self2 = ((OUT) NULL);
   ret_val8 = create_self2;
   plus_self2 = ret_val8;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r2 = L20;
   
   SATTR(r2,fp,stdout);
   ret_val9 = r2;
   FILE_plus_STR(ret_val9, plus_s2);
  }
  return;
 }
 L24=AM_CUR153953493(c);
 L25 = FLISTA1370145561(ATTR(L24,firsts), lbool);
 L261_=(L25)<(0); 
 if (L261_) {
  prog_self3 = self;
  ret_val10 = OPT_CO233462019;
  L27=ret_val10;
  if (ATTR(L27,opt_debug)) {
   create_self3 = ((OUT) NULL);
   ret_val11 = create_self3;
   plus_self3 = ret_val11;
   plus_s3 = ((STR) &theloc642778558);
   stdout_self3 = ((FILE1) NULL);
   L29=ZALLOC(sizeof(struct FILE1_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=FILE1_tag;
   L28 = ((FILE1) L29);
   r3 = L28;
   
   SATTR(r3,fp,stdout);
   ret_val12 = r3;
   FILE_plus_STR(ret_val12, plus_s3);
  }
  return;
 }
 prog_self4 = self;
 ret_val13 = OPT_CO233462019;
 c11 = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val13, ((dAM) ATTR(n,stmts)));
 SATTR(c11,ignore_pre,TRUE);
 SATTR(c11,ignore_post,TRUE);
 SATTR(c11,ignore_assert,TRUE);
 found_it = FALSE;
 ignore_next_refe = TRUE;
 {
  struct AM_CUR1040670508_frame_struct other3_0;
AM_CUR1040670508_frame noname2 = &other3_0;
  L21 = c11;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L31 = AM_CUR1040670508(noname2);
   if (noname2->state == -1) {
    goto after_loop2;
   }
   d = L31;
   if (d==NULL) {
    goto other306;
   } else
   switch (TAG(d)) {
    case AM_ASSIGN_STMT_tag:
     dst = ATTR(((AM_ASSIGN_STMT) d),dest);
     if (dst==NULL) {
      goto other307;
     } else
     switch (TAG(dst)) {
      case AM_LOCAL_EXPR_tag:
       is_eq_self = ((AM_LOCAL_EXPR) dst);
       is_eq_a = ((OB) lbool);
       ret_val14 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
       if (ret_val14) {
        if (found_it) {
         prog_self5 = self;
         ret_val15 = OPT_CO233462019;
         L32=ret_val15;
         if (ATTR(L32,opt_debug)) {
          create_self4 = ((OUT) NULL);
          ret_val16 = create_self4;
          plus_self4 = ret_val16;
          plus_s4 = ((STR) &foundt1136726481);
          stdout_self4 = ((FILE1) NULL);
          L34=ZALLOC(sizeof(struct FILE1_struct));
          if (L34==NULL) FATAL("Unable to allocate more memory");
          ((OB)L34)->header.tag=FILE1_tag;
          L33 = ((FILE1) L34);
          r4 = L33;
          
          SATTR(r4,fp,stdout);
          ret_val17 = r4;
          FILE_plus_STR(ret_val17, plus_s4);
         }
         return;
        }
        else {
         found_it = TRUE;
         ignore_next_refe = TRUE;
         L36 = ATTR(c11,indent);
         L371_=(L36)==(1); 
         L38 = L371_;
         L391_=!(L38); 
         if (L391_) {
          prog_self6 = self;
          ret_val18 = OPT_CO233462019;
          L40=ret_val18;
          if (ATTR(L40,opt_debug)) {
           create_self5 = ((OUT) NULL);
           ret_val19 = create_self5;
           plus_self5 = ret_val19;
           plus_s5 = ((STR) &indent2);
           stdout_self5 = ((FILE1) NULL);
           L42=ZALLOC(sizeof(struct FILE1_struct));
           if (L42==NULL) FATAL("Unable to allocate more memory");
           ((OB)L42)->header.tag=FILE1_tag;
           L41 = ((FILE1) L42);
           r5 = L41;
           
           SATTR(r5,fp,stdout);
           ret_val21 = r5;
           FILE_plus_STR(ret_val21, plus_s5);
           ret_val20 = plus_self5;
           plus_self6 = ret_val20;
           L45=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
           if (L45==NULL) FATAL("Unable to allocate more memory");
           memset(L45,0,sizeof(struct INT_boxed_struct));
           ((OB)L45)->header.tag=INT_tag;
           L44 = (dSTR)((INT_boxed) L45);
           ((INT_boxed) L44)->immutable_part = ATTR(c11,indent);
           plus_s6 = L44;
           stdout_self6 = ((FILE1) NULL);
           L47=ZALLOC(sizeof(struct FILE1_struct));
           if (L47==NULL) FATAL("Unable to allocate more memory");
           ((OB)L47)->header.tag=FILE1_tag;
           L46 = ((FILE1) L47);
           r6 = L46;
           
           SATTR(r6,fp,stdout);
           ret_val23 = r6;
           FILE_plus_dSTR(ret_val23, plus_s6);
           ret_val22 = plus_self6;
           plus_self7 = ret_val22;
           plus_s7 = ((STR) &soassi178775884);
           stdout_self7 = ((FILE1) NULL);
           L50=ZALLOC(sizeof(struct FILE1_struct));
           if (L50==NULL) FATAL("Unable to allocate more memory");
           ((OB)L50)->header.tag=FILE1_tag;
           L49 = ((FILE1) L50);
           r7 = L49;
           
           SATTR(r7,fp,stdout);
           ret_val24 = r7;
           FILE_plus_STR(ret_val24, plus_s7);
          }
          return;
         }
         s = AM_CUR1430796545(c11);
         if (s==NULL) {
          goto other308;
         } else
         switch (TAG(s)) {
          case AM_IF_STMT_tag:
           t = ATTR(((AM_IF_STMT) s),test1);
           if (t==NULL) {
            goto other309;
           } else
           switch (TAG(t)) {
            case AM_LOCAL_EXPR_tag:
             is_eq_self1 = ((AM_LOCAL_EXPR) t);
             is_eq_a1 = ((OB) lbool);
             ret_val25 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
             L52 = ret_val25;
             L531_=!(L52); 
             if (L531_) {
              prog_self7 = self;
              ret_val26 = OPT_CO233462019;
              L54=ret_val26;
              if (ATTR(L54,opt_debug)) {
               create_self6 = ((OUT) NULL);
               ret_val27 = create_self6;
               plus_self8 = ret_val27;
               plus_s8 = ((STR) &wrongt551266388);
               stdout_self8 = ((FILE1) NULL);
               L56=ZALLOC(sizeof(struct FILE1_struct));
               if (L56==NULL) FATAL("Unable to allocate more memory");
               ((OB)L56)->header.tag=FILE1_tag;
               L55 = ((FILE1) L56);
               r8 = L55;
               
               SATTR(r8,fp,stdout);
               ret_val28 = r8;
               FILE_plus_STR(ret_val28, plus_s8);
              }
              return;
             } break;
            default: ;
            other309: ;
             prog_self8 = self;
             ret_val29 = OPT_CO233462019;
             L58=ret_val29;
             if (ATTR(L58,opt_debug)) {
              create_self7 = ((OUT) NULL);
              ret_val30 = create_self7;
              plus_self9 = ret_val30;
              plus_s9 = ((STR) &wrongt1197886949);
              stdout_self9 = ((FILE1) NULL);
              L60=ZALLOC(sizeof(struct FILE1_struct));
              if (L60==NULL) FATAL("Unable to allocate more memory");
              ((OB)L60)->header.tag=FILE1_tag;
              L59 = ((FILE1) L60);
              r9 = L59;
              
              SATTR(r9,fp,stdout);
              ret_val31 = r9;
              FILE_plus_STR(ret_val31, plus_s9);
             }
             return;
           }
           tr = ATTR(((AM_IF_STMT) s),if_true);
           while (1) {
            L63 = (tr==((dAM_STMT) NULL));
            L641_=!(L63); 
            if (L641_) {
            }
            else {
             goto after_loop3;
            }
            L651_=SYSID(tr); 
            L66 = L651_;
            L671_=SYSID(d); 
            L68 = L671_;
            L691_=(L66)==(L68); 
            if (L691_) {
             goto after_loop3;
            }
            L70 = tr;
            tr = (*dAM_ST775224435[TAG(L70)])(L70);
           }
           after_loop3: ;
           if ((tr==((dAM_STMT) NULL))) {
            prog_self9 = self;
            ret_val32 = OPT_CO233462019;
            L71=ret_val32;
            if (ATTR(L71,opt_debug)) {
             create_self8 = ((OUT) NULL);
             ret_val33 = create_self8;
             plus_self10 = ret_val33;
             plus_s10 = ((STR) &Assign1969083730);
             stdout_self10 = ((FILE1) NULL);
             L73=ZALLOC(sizeof(struct FILE1_struct));
             if (L73==NULL) FATAL("Unable to allocate more memory");
             ((OB)L73)->header.tag=FILE1_tag;
             L72 = ((FILE1) L73);
             r10 = L72;
             
             SATTR(r10,fp,stdout);
             ret_val34 = r10;
             FILE_plus_STR(ret_val34, plus_s10);
            }
           }
           src11 = ATTR(((AM_ASSIGN_STMT) d),src1);
           if (src11==NULL) {
            goto other310;
           } else
           switch (TAG(src11)) {
            case AM_BOOL_CONST_tag:
             L75 = ATTR(((AM_BOOL_CONST) src11),val1);
             L761_=L75==FALSE; 
             L77 = L761_;
             L781_=!(L77); 
             if (L781_) {
              prog_self10 = self;
              ret_val35 = OPT_CO233462019;
              L79=ret_val35;
              if (ATTR(L79,opt_debug)) {
               create_self9 = ((OUT) NULL);
               ret_val36 = create_self9;
               plus_self11 = ret_val36;
               plus_s11 = ((STR) &Assign248586026);
               stdout_self11 = ((FILE1) NULL);
               L81=ZALLOC(sizeof(struct FILE1_struct));
               if (L81==NULL) FATAL("Unable to allocate more memory");
               ((OB)L81)->header.tag=FILE1_tag;
               L80 = ((FILE1) L81);
               r11 = L80;
               
               SATTR(r11,fp,stdout);
               ret_val37 = r11;
               FILE_plus_STR(ret_val37, plus_s11);
              }
              return;
             } break;
            default: ;
            other310: ;
             prog_self11 = self;
             ret_val38 = OPT_CO233462019;
             L83=ret_val38;
             if (ATTR(L83,opt_debug)) {
              create_self10 = ((OUT) NULL);
              ret_val39 = create_self10;
              plus_self12 = ret_val39;
              plus_s12 = ((STR) &Assign439881173);
              stdout_self12 = ((FILE1) NULL);
              L85=ZALLOC(sizeof(struct FILE1_struct));
              if (L85==NULL) FATAL("Unable to allocate more memory");
              ((OB)L85)->header.tag=FILE1_tag;
              L84 = ((FILE1) L85);
              r12 = L84;
              
              SATTR(r12,fp,stdout);
              ret_val40 = r12;
              FILE_plus_STR(ret_val40, plus_s12);
             }
             return;
           } break;
          default: ;
          other308: ;
           prog_self12 = self;
           ret_val41 = OPT_CO233462019;
           L87=ret_val41;
           if (ATTR(L87,opt_debug)) {
            create_self11 = ((OUT) NULL);
            ret_val42 = create_self11;
            plus_self13 = ret_val42;
            plus_s13 = ((STR) &assign1831176839);
            stdout_self13 = ((FILE1) NULL);
            L89=ZALLOC(sizeof(struct FILE1_struct));
            if (L89==NULL) FATAL("Unable to allocate more memory");
            ((OB)L89)->header.tag=FILE1_tag;
            L88 = ((FILE1) L89);
            r13 = L88;
            
            SATTR(r13,fp,stdout);
            ret_val43 = r13;
            FILE_plus_STR(ret_val43, plus_s13);
           }
           return;
         }
        }
       } break;
      default: ;
      other307: ;
     } break;
    case AM_LOCAL_EXPR_tag:
     if (found_it) {
      if (ignore_next_refe) {
       ignore_next_refe = FALSE;
      }
      else {
       is_eq_self2 = ((AM_LOCAL_EXPR) d);
       is_eq_a2 = ((OB) lbool);
       ret_val44 = SYSOBEQ(((OB) is_eq_self2),is_eq_a2);
       if (ret_val44) {
        prog_self13 = self;
        ret_val45 = OPT_CO233462019;
        L91=ret_val45;
        if (ATTR(L91,opt_debug)) {
         create_self12 = ((OUT) NULL);
         ret_val46 = create_self12;
         plus_self14 = ret_val46;
         plus_s14 = ((STR) &therei1547416579);
         stdout_self14 = ((FILE1) NULL);
         L93=ZALLOC(sizeof(struct FILE1_struct));
         if (L93==NULL) FATAL("Unable to allocate more memory");
         ((OB)L93)->header.tag=FILE1_tag;
         L92 = ((FILE1) L93);
         r14 = L92;
         
         SATTR(r14,fp,stdout);
         ret_val47 = r14;
         FILE_plus_STR(ret_val47, plus_s14);
        }
        return;
       }
      }
     } break;
    default: ;
    other306: ;
   }
  }
 }
 after_loop2: ;
 prog_self14 = self;
 ret_val48 = OPT_CO233462019;
 L95=ret_val48;
 if (ATTR(L95,opt_debug)) {
  create_self13 = ((OUT) NULL);
  ret_val49 = create_self13;
  plus_self15 = ret_val49;
  plus_s15 = ((STR) &itseem1831037758);
  stdout_self15 = ((FILE1) NULL);
  L97=ZALLOC(sizeof(struct FILE1_struct));
  if (L97==NULL) FATAL("Unable to allocate more memory");
  ((OB)L97)->header.tag=FILE1_tag;
  L96 = ((FILE1) L97);
  r15 = L96;
  
  SATTR(r15,fp,stdout);
  ret_val50 = r15;
  FILE_plus_STR(ret_val50, plus_s15);
 }
 create_self14 = ((AM_COMMENT_STMT) NULL);
 create_source = ATTR(it,source1);
 L100=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
 if (L100==NULL) FATAL("Unable to allocate more memory");
 ((OB)L100)->header.tag=AM_COMMENT_STMT_tag;
 L99 = ((AM_COMMENT_STMT) L100);
 r16 = L99;
 SATTR(r16,source1,create_source);
 ret_val51 = r16;
 comment1 = ret_val51;
 plus_self16 = ((STR) &Initia1351694393);
 plus_sarg = ATTR(ATTR(it,fun),str);
 ret_val52 = STR_ap2004550586(plus_self16, plus_sarg);
 SATTR(comment1,comment1,ret_val52);
 AM_CUR352782528(c, ((dAM_STMT) comment1));
 pr = ((dAM_STMT) NULL);
 am11 = ATTR(n,stmts);
 prog_self15 = self;
 ret_val53 = OPT_CO233462019;
 inlined = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val53, ((dAM) ATTR(n,stmts)));
 SATTR(inlined,ignore_pre,TRUE);
 SATTR(inlined,ignore_post,TRUE);
 SATTR(inlined,ignore_assert,TRUE);
 AM_CUR1869307717(inlined);
 next_self = inlined;
 dummy2 = AM_CUR52795524(next_self);
 while (1) {
  L102 = (am11==((dAM_STMT) NULL));
  L1031_=!(L102); 
  if (L1031_) {
  }
  else {
   goto after_loop4;
  }
  if (am11==NULL) {
   goto other311;
  } else
  switch (TAG(am11)) {
   case AM_IF_STMT_tag:
    l = ATTR(((AM_IF_STMT) am11),test1);
    if (l==NULL) {
     goto other312;
    } else
    switch (TAG(l)) {
     case AM_LOCAL_EXPR_tag:
      is_eq_self3 = ((AM_LOCAL_EXPR) l);
      is_eq_a3 = ((OB) lbool);
      ret_val54 = SYSOBEQ(((OB) is_eq_self3),is_eq_a3);
      if (ret_val54) {
       pr = ((dAM_STMT) NULL);
       L104 = (ATTR(((AM_IF_STMT) am11),if_false)==((dAM_STMT) NULL));
       L1051_=!(L104); 
       if (L1051_) {
        move_lbool = FALSE;
       }
       ifs = ATTR(((AM_IF_STMT) am11),if_true);
       next_self1 = inlined;
       dummy3 = AM_CUR52795524(next_self1);
       next_self2 = inlined;
       dummy4 = AM_CUR52795524(next_self2);
       while (1) {
        L106 = (ifs==((dAM_STMT) NULL));
        L1071_=!(L106); 
        if (L1071_) {
        }
        else {
         goto after_loop5;
        }
        L108 = ifs;
        nifs = (*dAM_ST775224435[TAG(L108)])(L108);
        p = ifs;
        do_move = FALSE;
        ignore_stmt = FALSE;
        delete_it = FALSE;
        L1091_=SYSID(ifs); 
        L110 = L1091_;
        L111 = ATTR(inlined,cur);
        L1121_=SYSID(L111); 
        L113 = L1121_;
        L1141_=(L110)==(L113); 
        L115 = L1141_;
        L1161_=!(L115); 
        if (L1161_) {
         create_self15 = ((OUT) NULL);
         ret_val55 = create_self15;
         plus_self17 = ret_val55;
         plus_s16 = ((STR) &inline160970948);
         stdout_self16 = ((FILE1) NULL);
         L118=ZALLOC(sizeof(struct FILE1_struct));
         if (L118==NULL) FATAL("Unable to allocate more memory");
         ((OB)L118)->header.tag=FILE1_tag;
         L117 = ((FILE1) L118);
         r17 = L117;
         
         SATTR(r17,fp,stdout);
         ret_val56 = r17;
         FILE_plus_STR(ret_val56, plus_s16);
         create_self16 = ((OUT) NULL);
         ret_val57 = create_self16;
         plus_self18 = ret_val57;
         plus_s17 = ((STR) &ifssays);
         stdout_self17 = ((FILE1) NULL);
         L121=ZALLOC(sizeof(struct FILE1_struct));
         if (L121==NULL) FATAL("Unable to allocate more memory");
         ((OB)L121)->header.tag=FILE1_tag;
         L120 = ((FILE1) L121);
         r18 = L120;
         
         SATTR(r18,fp,stdout);
         ret_val58 = r18;
         FILE_plus_STR(ret_val58, plus_s17);
         AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ifs));
         create_self17 = ((OUT) NULL);
         ret_val59 = create_self17;
         plus_self19 = ret_val59;
         plus_s18 = ((STR) &andinlinedisat);
         stdout_self18 = ((FILE1) NULL);
         L124=ZALLOC(sizeof(struct FILE1_struct));
         if (L124==NULL) FATAL("Unable to allocate more memory");
         ((OB)L124)->header.tag=FILE1_tag;
         L123 = ((FILE1) L124);
         r19 = L123;
         
         SATTR(r19,fp,stdout);
         ret_val60 = r19;
         FILE_plus_STR(ret_val60, plus_s18);
         AM_OUT1439666796(((AM_OUT) NULL), ATTR(inlined,cur));
         exit_self = ((UNIX) NULL);
         exit_code = -1;
         exit(exit_code);
        }
        if (ifs==NULL) {
         goto other313;
        } else
        switch (TAG(ifs)) {
         case AM_ASSIGN_STMT_tag:
          L1321_=!(need_safe_attr); 
          if (L1321_) {
           L131 = TRUE;
          } else {
           prog_self16 = self;
           ret_val61 = OPT_CO233462019;
           L133=ret_val61;
           L134 = ATTR(L133,void_checks);
           L1351_=!(L134); 
           L131 = L1351_;
          }
          if (L131) {
           L130 = TRUE;
          } else {
           L136 = OPT_IT2079146999(self, ((dAM) ifs));
           L1371_=!(L136); 
           L130 = L1371_;
          }
          if (L130) {
           L129 = OPT_IT1532669498(self, ATTR(((AM_ASSIGN_STMT) ifs),dest), c, need_safe_attr, in_conditional);
          } else {
           L129 = FALSE;
          }
          if (L129) {
           L128 = OPT_IT24605408(self, ATTR(((AM_ASSIGN_STMT) ifs),src1), c, need_safe_attr, in_conditional);
          } else {
           L128 = FALSE;
          }
          if (L128) {
           is_not_used_in_f = inlined;
           is_not_used_in_f1 = ((dAM) ATTR(((AM_ASSIGN_STMT) ifs),dest));
           c12 = AM_CUR1358945253(((AM_CURSOR) NULL), is_not_used_in_f);
           AM_CUR1869307717(c12);
           ret_val62 = AM_CUR1926902782(is_not_used_in_f, c12, is_not_used_in_f1);
           L127 = ret_val62;
          } else {
           L127 = FALSE;
          }
          if (L127) {
           is_const_in_func = inlined;
           is_const_in_func1 = ((dAM) ATTR(((AM_ASSIGN_STMT) ifs),src1));
           c13 = AM_CUR1358945253(((AM_CURSOR) NULL), is_const_in_func);
           AM_CUR1869307717(c13);
           ret_val63 = AM_CUR750559803(is_const_in_func, c13, is_const_in_func1);
           L126 = ret_val63;
          } else {
           L126 = FALSE;
          }
          if (L126) {
           adest = ATTR(((AM_ASSIGN_STMT) ifs),dest);
           if (adest==NULL) {
            goto other314;
           } else
           switch (TAG(adest)) {
            case AM_LOCAL_EXPR_tag:
             do_move = TRUE; break;
            default: ;
            other314: ;
             L1381_=!(need_safe_attr); 
             if (L1381_) {
              do_move = TRUE;
             }
           }
           if (need_safe_attr) {
            L139 = do_move;
           } else {
            L139 = FALSE;
           }
           if (L139) {
            OPT_IT222343591(self, ((dAM) ifs));
           }
          }
          else {
           adest1 = ATTR(((AM_ASSIGN_STMT) ifs),dest);
           if (adest1==NULL) {
            goto other315;
           } else
           switch (TAG(adest1)) {
            case AM_LOCAL_EXPR_tag:
             is_eq_self4 = ((AM_LOCAL_EXPR) adest1);
             is_eq_a4 = ((OB) lbool);
             ret_val64 = SYSOBEQ(((OB) is_eq_self4),is_eq_a4);
             if (ret_val64) {
              ignore_stmt = TRUE;
             } break;
            default: ;
            other315: ;
           }
           prog_self17 = self;
           ret_val65 = OPT_CO233462019;
           L140=ret_val65;
           if (ATTR(L140,opt_debug)) {
            L141 = OPT_IT1532669498(self, ATTR(((AM_ASSIGN_STMT) ifs),dest), c, need_safe_attr, in_conditional);
            L1421_=!(L141); 
            if (L1421_) {
             create_self18 = ((OUT) NULL);
             ret_val66 = create_self18;
             plus_self20 = ret_val66;
             plus_s19 = ((STR) &cannot1775150384);
             stdout_self19 = ((FILE1) NULL);
             L144=ZALLOC(sizeof(struct FILE1_struct));
             if (L144==NULL) FATAL("Unable to allocate more memory");
             ((OB)L144)->header.tag=FILE1_tag;
             L143 = ((FILE1) L144);
             r20 = L143;
             
             SATTR(r20,fp,stdout);
             ret_val67 = r20;
             FILE_plus_STR(ret_val67, plus_s19);
            }
            L146 = OPT_IT24605408(self, ATTR(((AM_ASSIGN_STMT) ifs),src1), c, need_safe_attr, in_conditional);
            L1471_=!(L146); 
            if (L1471_) {
             create_self19 = ((OUT) NULL);
             ret_val68 = create_self19;
             plus_self21 = ret_val68;
             plus_s20 = ((STR) &cannot68435663);
             stdout_self20 = ((FILE1) NULL);
             L149=ZALLOC(sizeof(struct FILE1_struct));
             if (L149==NULL) FATAL("Unable to allocate more memory");
             ((OB)L149)->header.tag=FILE1_tag;
             L148 = ((FILE1) L149);
             r21 = L148;
             
             SATTR(r21,fp,stdout);
             ret_val69 = r21;
             FILE_plus_STR(ret_val69, plus_s20);
            }
            is_not_used_in_f2 = inlined;
            is_not_used_in_f3 = ((dAM) ATTR(((AM_ASSIGN_STMT) ifs),dest));
            c14 = AM_CUR1358945253(((AM_CURSOR) NULL), is_not_used_in_f2);
            AM_CUR1869307717(c14);
            ret_val70 = AM_CUR1926902782(is_not_used_in_f2, c14, is_not_used_in_f3);
            L152 = ret_val70;
            L1531_=!(L152); 
            if (L1531_) {
             create_self20 = ((OUT) NULL);
             ret_val71 = create_self20;
             plus_self22 = ret_val71;
             plus_s21 = ((STR) &cannot56504486);
             stdout_self21 = ((FILE1) NULL);
             L155=ZALLOC(sizeof(struct FILE1_struct));
             if (L155==NULL) FATAL("Unable to allocate more memory");
             ((OB)L155)->header.tag=FILE1_tag;
             L154 = ((FILE1) L155);
             r22 = L154;
             
             SATTR(r22,fp,stdout);
             ret_val72 = r22;
             FILE_plus_STR(ret_val72, plus_s21);
            }
            is_const_in_func2 = inlined;
            is_const_in_func3 = ((dAM) ATTR(((AM_ASSIGN_STMT) ifs),src1));
            c15 = AM_CUR1358945253(((AM_CURSOR) NULL), is_const_in_func2);
            AM_CUR1869307717(c15);
            ret_val73 = AM_CUR750559803(is_const_in_func2, c15, is_const_in_func3);
            L157 = ret_val73;
            L1581_=!(L157); 
            if (L1581_) {
             create_self21 = ((OUT) NULL);
             ret_val74 = create_self21;
             plus_self23 = ret_val74;
             plus_s22 = ((STR) &cannot972067817);
             stdout_self22 = ((FILE1) NULL);
             L160=ZALLOC(sizeof(struct FILE1_struct));
             if (L160==NULL) FATAL("Unable to allocate more memory");
             ((OB)L160)->header.tag=FILE1_tag;
             L159 = ((FILE1) L160);
             r23 = L159;
             
             SATTR(r23,fp,stdout);
             ret_val75 = r23;
             FILE_plus_STR(ret_val75, plus_s22);
            }
           }
          } break;
         case AM_PRE_STMT_tag:
          prog_self18 = self;
          ret_val76 = OPT_CO233462019;
          L162=ret_val76;
          L163 = ATTR(L162,pre_checks);
          L1641_=!(L163); 
          if (L1641_) {
           delete_it = TRUE;
          } break;
         case AM_POST_STMT_tag:
          prog_self19 = self;
          ret_val77 = OPT_CO233462019;
          L165=ret_val77;
          L166 = ATTR(L165,post_checks);
          L1671_=!(L166); 
          if (L1671_) {
           delete_it = TRUE;
          } break;
         case AM_ASSERT_STMT_tag:
          prog_self20 = self;
          ret_val78 = OPT_CO233462019;
          L168=ret_val78;
          L169 = ATTR(L168,assert_checks);
          L1701_=!(L169); 
          if (L1701_) {
           delete_it = TRUE;
          } break;
         default: ;
         other313: ;
        }
        if (do_move) {
         if ((pr==((dAM_STMT) NULL))) {
          L171 = ifs;
          SATTR(((AM_IF_STMT) am11),if_true,(*dAM_ST775224435[TAG(L171)])(L171));
         }
         else {
          L172 = ifs;
          L173 = pr;
          (*dAM_ST1372701974[TAG(L173)])(L173, (*dAM_ST775224435[TAG(L172)])(L172));
         }
         L174 = ifs;
         nifs = (*dAM_ST775224435[TAG(L174)])(L174);
         p = pr;
         L175 = ifs;
         (*dAM_ST1372701974[TAG(L175)])(L175, ((dAM_STMT) NULL));
         AM_CUR352782528(c, ifs);
         prog_self21 = self;
         ret_val79 = OPT_CO233462019;
         L176=ret_val79;
         if (ATTR(L176,opt_debug)) {
          create_self22 = ((OUT) NULL);
          ret_val80 = create_self22;
          plus_self24 = ret_val80;
          plus_s23 = ((STR) &moving93892770);
          stdout_self23 = ((FILE1) NULL);
          L178=ZALLOC(sizeof(struct FILE1_struct));
          if (L178==NULL) FATAL("Unable to allocate more memory");
          ((OB)L178)->header.tag=FILE1_tag;
          L177 = ((FILE1) L178);
          r24 = L177;
          
          SATTR(r24,fp,stdout);
          ret_val81 = r24;
          FILE_plus_STR(ret_val81, plus_s23);
          AM_one_stmt_self = ((AM_OUT) NULL);
          AM_one_stmt_a = ((dAM) ifs);
          switch (TAG(AM_one_stmt_a)) {
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
            L180 = ((dAM_STMT) AM_one_stmt_a);
            n11 = (*dAM_ST775224435[TAG(L180)])(L180);
            L181 = ((dAM_STMT) AM_one_stmt_a);
            (*dAM_ST1372701974[TAG(L181)])(L181, ((dAM_STMT) NULL));
            AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
            L182 = ((dAM_STMT) AM_one_stmt_a);
            (*dAM_ST1372701974[TAG(L182)])(L182, n11); break;
           default: ;
            AM_OUT1439666796(AM_one_stmt_self, AM_one_stmt_a);
          }
         }
         if ((nifs==((dAM_STMT) NULL))) {
          create_self23 = ((AM_COMMENT_STMT) NULL);
          create_source1 = SFILE_ID_zero;
          L184=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
          if (L184==NULL) FATAL("Unable to allocate more memory");
          ((OB)L184)->header.tag=AM_COMMENT_STMT_tag;
          L183 = ((AM_COMMENT_STMT) L184);
          r25 = L183;
          SATTR(r25,source1,create_source1);
          ret_val82 = r25;
          SATTR(inlined,cur,((dAM) ret_val82));
         }
         else {
          SATTR(inlined,cur,((dAM) nifs));
         }
        }
        else {
         if (delete_it) {
          if ((pr==((dAM_STMT) NULL))) {
           L185 = ifs;
           SATTR(((AM_IF_STMT) am11),if_true,(*dAM_ST775224435[TAG(L185)])(L185));
          }
          else {
           L186 = ifs;
           L187 = pr;
           (*dAM_ST1372701974[TAG(L187)])(L187, (*dAM_ST775224435[TAG(L186)])(L186));
          }
          L188 = ifs;
          nifs = (*dAM_ST775224435[TAG(L188)])(L188);
          p = pr;
          L189 = ifs;
          (*dAM_ST1372701974[TAG(L189)])(L189, ((dAM_STMT) NULL));
          if ((nifs==((dAM_STMT) NULL))) {
           create_self24 = ((AM_COMMENT_STMT) NULL);
           create_source2 = SFILE_ID_zero;
           L192=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
           if (L192==NULL) FATAL("Unable to allocate more memory");
           ((OB)L192)->header.tag=AM_COMMENT_STMT_tag;
           L190 = ((AM_COMMENT_STMT) L192);
           r26 = L190;
           SATTR(r26,source1,create_source2);
           ret_val83 = r26;
           SATTR(inlined,cur,((dAM) ret_val83));
          }
          else {
           SATTR(inlined,cur,((dAM) nifs));
          }
         }
         else {
          L1931_=!(ignore_stmt); 
          if (L1931_) {
           move_lbool = FALSE;
          }
         }
        }
        pr = p;
        ifs = nifs;
        L196 = (ifs==((dAM_STMT) NULL));
        L1971_=!(L196); 
        if (L1971_) {
         L1981_=!(do_move); 
         L195 = L1981_;
        } else {
         L195 = FALSE;
        }
        if (L195) {
         L1991_=!(delete_it); 
         L194 = L1991_;
        } else {
         L194 = FALSE;
        }
        if (L194) {
         next_stmt_self = inlined;
         d11 = AM_CUR1623871191(next_stmt_self);
        }
       }
       after_loop5: ;
       AM_CUR1769624888(inlined);
       AM_CUR1127255611(inlined);
      } break;
     default: ;
     other312: ;
    } break;
   default: ;
   other311: ;
  }
  L200 = am11;
  am11 = (*dAM_ST775224435[TAG(L200)])(L200);
  next_stmt_self1 = inlined;
  d21 = AM_CUR1623871191(next_stmt_self1);
 }
 after_loop4: ;
 pr = ((dAM_STMT) NULL);
 am11 = ATTR(n,stmts);
 while (1) {
  L201 = (am11==((dAM_STMT) NULL));
  L2021_=!(L201); 
  if (L2021_) {
  }
  else {
   goto after_loop6;
  }
  amm = am11;
  if (amm==NULL) {
   goto other316;
  } else
  switch (TAG(amm)) {
   case AM_IF_STMT_tag:
    l1 = ATTR(((AM_IF_STMT) amm),test1);
    if (l1==NULL) {
     goto other317;
    } else
    switch (TAG(l1)) {
     case AM_LOCAL_EXPR_tag:
      is_eq_self5 = ((AM_LOCAL_EXPR) l1);
      is_eq_a5 = ((OB) lbool);
      ret_val84 = SYSOBEQ(((OB) is_eq_self5),is_eq_a5);
      if (ret_val84) {
       if (move_lbool) {
        L203 = TRUE;
       } else {
        if ((ATTR(((AM_IF_STMT) amm),if_true)==((dAM_STMT) NULL))) {
         L204 = (ATTR(((AM_IF_STMT) amm),if_false)==((dAM_STMT) NULL));
        } else {
         L204 = FALSE;
        }
        L203 = L204;
       }
       if (L203) {
        prog_self22 = self;
        ret_val85 = OPT_CO233462019;
        L205=ret_val85;
        L206 = ATTR(L205,stat1);
        (*dSTAT_incr_STR[TAG(L206)])(L206, ((STR) &Oifstmtsremoved));
        if ((pr==((dAM_STMT) NULL))) {
         L207 = am11;
         SATTR(n,stmts,(*dAM_ST775224435[TAG(L207)])(L207));
         am11 = ATTR(n,stmts);
        }
        else {
         L208 = am11;
         L209 = pr;
         (*dAM_ST1372701974[TAG(L209)])(L209, (*dAM_ST775224435[TAG(L208)])(L208));
         L210 = am11;
         am11 = (*dAM_ST775224435[TAG(L210)])(L210);
        }
       }
       else {
        pr = am11;
        L211 = am11;
        am11 = (*dAM_ST775224435[TAG(L211)])(L211);
       }
      }
      else {
       pr = am11;
       L212 = am11;
       am11 = (*dAM_ST775224435[TAG(L212)])(L212);
      } break;
     default: ;
     other317: ;
      pr = am11;
      L213 = am11;
      am11 = (*dAM_ST775224435[TAG(L213)])(L213);
    } break;
   default: ;
   other316: ;
    pr = am11;
    L214 = am11;
    am11 = (*dAM_ST775224435[TAG(L214)])(L214);
  }
 }
 after_loop6: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_IT222343591(OPT_ITER self, dAM a) {
 AM_CURSOR c;
 AM_CURSOR L11;
 dAM am;
 OPT_ITER prog_self;
 PROG ret_val;
 dAM L2;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, a);
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
   am = L2;
   if (am==NULL) {
    goto other300;
   } else
   switch (TAG(am)) {
    case AM_ATTR_EXPR_tag:
     SATTR(((AM_ATTR_EXPR) am),secure1,TRUE); break;
    default: ;
    other300: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_IT505798087(OPT_ITER self, AM_ROUT_DEF func, AM_ITE1809135850 am, AM_CURSOR c, ARRAYBOOL after_loop_break) {
 dAM_STMT stmt1;
 BOOL do_optimize = BOOL_zero;
 BOOL need_safe_attr = BOOL_zero;
 BOOL has_attr = BOOL_zero;
 BOOL in_conditional = BOOL_zero;
 AM_ASSIGN_STMT as;
 AM_COMMENT_STMT comment1;
 AM_ASSIGN_STMT p;
 AM_ROU1916046290 mi;
 ARRAYARG args;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 OPT_ITER prog_self;
 PROG ret_val3;
 OPT_ITER prog_self1;
 PROG ret_val4;
 OPT_ITER tp_int_self;
 dTP ret_val5;
 OPT_ITER new_local_self;
 AM_ROUT_DEF new_local_func;
 dTP new_local_tp;
 AM_LOCAL_EXPR ret_val6;
 AM_LOCAL_EXPR lc;
 AM_LOCAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 IDENT create_name = IDENT_zero;
 dTP create_tp;
 AM_LOCAL_EXPR ret_val7;
 AM_LOCAL_EXPR r;
 AM_ASSIGN_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val8;
 AM_ASSIGN_STMT r1;
 OPT_ITER zero_self;
 AM_INT_CONST ret_val9;
 AM_COMMENT_STMT create_self2;
 SFILE_ID create_source1 = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val10;
 AM_COMMENT_STMT r2;
 AM_ASSIGN_STMT create_self3;
 SFILE_ID create_source2 = SFILE_ID_zero;
 AM_ASSIGN_STMT ret_val11;
 AM_ASSIGN_STMT r3;
 AM_ROU1916046290 create_self4;
 INT create_nargs = INT_zero;
 SFILE_ID create_source3 = SFILE_ID_zero;
 AM_ROU1916046290 ret_val12;
 AM_ROU1916046290 r4;
 AM_CALL_ARG create_self5;
 dAM_EXPR create_e;
 AM_CALL_ARG ret_val13;
 AM_CALL_ARG res;
 OPT_ITER one_self;
 AM_INT_CONST ret_val14;
 AM_CALL_ARG create_self6;
 dAM_EXPR create_e1;
 AM_CALL_ARG ret_val15;
 AM_CALL_ARG res1;
 ARRAYARG create_self7;
 INT create_n = INT_zero;
 ARRAYARG ret_val16;
 OPT_ITER tp_int_self1;
 dTP ret_val17;
 ARG create_self8;
 dTP create_t;
 ARG ret_val18;
 ARG init_self;
 dTP init_t;
 dMODE init_m;
 BOOL init_same = BOOL_zero;
 ARG ret_val19;
 OPT_ITER tp_int_self2;
 dTP ret_val20;
 ARRAYdTP create_self9;
 INT create_n1 = INT_zero;
 ARRAYdTP ret_val21;
 OPT_ITER tp_int_self3;
 dTP ret_val22;
 SIG create_self10;
 dTP create_ntp;
 ARRAYdTP create_tparr;
 IDENT create_nname = IDENT_zero;
 ARRAYARG create_nargs1;
 dTP create_nret;
 BOOL create_nsame = BOOL_zero;
 SIG ret_val23;
 SIG r5;
 OPT_ITER prog_self2;
 PROG ret_val24;
 OPT_ITER prog_self3;
 PROG ret_val25;
 OUT create_self11;
 OUT ret_val26;
 OUT plus_self;
 STR plus_s;
 OUT ret_val27;
 FILE1 stdout_self;
 FILE1 ret_val28;
 FILE1 r6;
 OUT plus_self1;
 dSTR plus_s1;
 OUT ret_val29;
 FILE1 stdout_self1;
 FILE1 ret_val30;
 FILE1 r7;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val31;
 FILE1 stdout_self2;
 FILE1 ret_val32;
 FILE1 r8;
 OUT plus_self3;
 dSTR plus_s3;
 OUT ret_val33;
 FILE1 stdout_self3;
 FILE1 ret_val34;
 FILE1 r9;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val35;
 FILE1 r10;
 OUT create_self12;
 OUT ret_val36;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val37;
 FILE1 r11;
 OPT_ITER prog_self4;
 PROG ret_val38;
 OUT create_self13;
 OUT ret_val39;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val40;
 FILE1 stdout_self6;
 FILE1 ret_val41;
 FILE1 r12;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val42;
 FILE1 stdout_self7;
 FILE1 ret_val43;
 FILE1 r13;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val44;
 FILE1 r14;
 OPT_ITER prog_self5;
 PROG ret_val45;
 OPT_ITER prog_self6;
 PROG ret_val46;
 OPT_ITER prog_self7;
 PROG ret_val47;
 OUT create_self14;
 OUT ret_val48;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val49;
 FILE1 stdout_self9;
 FILE1 ret_val50;
 FILE1 r15;
 OUT plus_self10;
 STR plus_s10;
 OUT ret_val51;
 FILE1 stdout_self10;
 FILE1 ret_val52;
 FILE1 r16;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val53;
 FILE1 stdout_self11;
 FILE1 ret_val54;
 FILE1 r17;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val55;
 FILE1 r18;
 OPT_ITER prog_self8;
 PROG ret_val56;
 OPT_ITER move_init_stmts_;
 AM_ROUT_DEF move_init_stmts_1;
 AM_ITE1809135850 move_init_stmts_2;
 AM_CURSOR move_init_stmts_3;
 OPT_ITER prog_self9;
 PROG ret_val57;
 AM_ROUT_DEF is_iter_self3;
 BOOL ret_val58 = BOOL_zero;
 SIG is_iter_self4;
 BOOL ret_val59 = BOOL_zero;
 IDENT is_iter_self5 = IDENT_zero;
 BOOL ret_val60 = BOOL_zero;
 OPT_ITER prog_self10;
 PROG ret_val61;
 OUT create_self15;
 OUT ret_val62;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val63;
 FILE1 stdout_self13;
 FILE1 ret_val64;
 FILE1 r19;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val65;
 FILE1 stdout_self14;
 FILE1 ret_val66;
 FILE1 r20;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val67;
 FILE1 stdout_self15;
 FILE1 ret_val68;
 FILE1 r21;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val69;
 FILE1 r22;
 OPT_ITER prog_self11;
 PROG ret_val70;
 OUT create_self16;
 OUT ret_val71;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val72;
 FILE1 stdout_self17;
 FILE1 ret_val73;
 FILE1 r23;
 OUT plus_self18;
 STR plus_s18;
 OUT ret_val74;
 FILE1 stdout_self18;
 FILE1 ret_val75;
 FILE1 r24;
 OUT plus_self19;
 STR plus_s19;
 OUT ret_val76;
 FILE1 stdout_self19;
 FILE1 ret_val77;
 FILE1 r25;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val78;
 FILE1 r26;
 OPT_ITER prog_self12;
 PROG ret_val79;
 OPT_ITER prog_self13;
 PROG ret_val80;
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
 BOOL L14;
 BOOL L151_;
 BOOL L16;
 BOOL L17;
 BOOL L181_;
 PROG L19;
 extern STR Oiters1510323475;
 dSTAT L20;
 BOOL L21;
 PROG L22;
 extern STR Oloops168062639;
 dSTAT L23;
 dAM_STMT L24;
 AM_LOCAL_EXPR L25;
 OB L26;
 AM_ASSIGN_STMT L27;
 OB L28;
 AM_COMMENT_STMT L29;
 OB L30;
 extern STR loopin1411911962;
 AM_ASSIGN_STMT L31;
 OB L32;
 AM_ROU1916046290 L33;
 INT L34;
 OB L35;
 INT L36;
 AM_CALL_ARG L37;
 OB L38;
 AM_CALL_ARG L39;
 AM_CALL_ARG L42;
 OB L43;
 AM_CALL_ARG L44;
 ARRAYARG L46;
 INT L47;
 OB L48;
 INT L49;
 ARG L50;
 OB L51;
 ARG L52;
 ARRAYdTP L54;
 INT L55;
 OB L56;
 INT L57;
 SIG L58;
 OB L59;
 PROG L60;
 dGENERATE_AM L61;
 BOOL L62;
 BOOL L63;
 BOOL L641_;
 BOOL L65;
 BOOL L661_;
 BOOL L67;
 INT L68;
 BOOL L691_;
 BOOL L70;
 BOOL L721_;
 PROG L73;
 extern STR thefol726840669;
 FILE1 L74;
 OB L75;
 dSTR L77;
 OB L78;
 FILE1 L79;
 OB L80;
 extern STR in_cond;
 FILE1 L82;
 OB L83;
 dSTR L85;
 OB L86;
 FILE1 L87;
 OB L88;
 extern STR name50;
 FILE1 L90;
 OB L92;
 extern STR name9;
 FILE1 L94;
 OB L95;
 PROG L97;
 extern STR ignoringiter;
 FILE1 L98;
 OB L99;
 FILE1 L101;
 OB L102;
 extern STR becaus2028109384;
 FILE1 L104;
 OB L105;
 dAM_STMT L107;
 BOOL L108;
 BOOL L109;
 BOOL L110;
 PROG L112;
 BOOL L113;
 BOOL L1141_;
 PROG L115;
 extern STR Oofite1407208962;
 dSTAT L116;
 BOOL L117;
 PROG L118;
 BOOL L119;
 BOOL L1201_;
 FILE1 L121;
 OB L122;
 extern STR INFOiter;
 FILE1 L124;
 OB L125;
 FILE1 L127;
 OB L128;
 extern STR ignoredbyinliner;
 FILE1 L130;
 OB L132;
 PROG L134;
 PROG L135;
 extern STR Oofini2113488966;
 dSTAT L136;
 BOOL L137;
 BOOL L138;
 BOOL L1391_;
 STR L140;
 INT L141;
 INT L1421_;
 INT L143;
 CHAR L1441_;
 CHAR L145;
 BOOL L1461_;
 BOOL L147;
 BOOL L1481_;
 BOOL L149;
 PROG L150;
 BOOL L152;
 BOOL L1531_;
 FILE1 L154;
 OB L155;
 extern STR INFOiter;
 FILE1 L157;
 OB L158;
 FILE1 L160;
 OB L161;
 extern STR ignore1571167589;
 FILE1 L163;
 OB L164;
 BOOL L166;
 BOOL L1671_;
 BOOL L168;
 PROG L169;
 BOOL L170;
 BOOL L1711_;
 FILE1 L172;
 OB L173;
 extern STR INFOiter;
 FILE1 L175;
 OB L176;
 FILE1 L178;
 OB L179;
 extern STR ignoredbyinliner;
 FILE1 L182;
 OB L183;
 PROG L185;
 extern STR Oofite2134272122;
 dSTAT L186;
 PROG L187;
 extern STR Oofite361277457;
 dSTAT L188;
 stmt1 = ATTR(am,init);
 is_iter_self = func;
 is_iter_self1 = ATTR(is_iter_self,sig1);
 is_iter_self2 = ATTR(is_iter_self1,name1);
 L3 = (is_iter_self2.str==((STR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = is_iter_self2.str;
  L6 = STR_sizerINT(is_iter_self2.str);
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
 ret_val = ret_val1;
 L12 = ret_val;
 L131_=!(L12); 
 if (L131_) {
  L1 = AM_CUR1097584212(c);
 } else {
  L1 = FALSE;
 }
 do_optimize = L1;
 need_safe_attr = FALSE;
 has_attr = FALSE;
 L14 = AM_CUR262177482(c);
 L151_=!(L14); 
 in_conditional = L151_;
 L181_=!(in_conditional); 
 if (L181_) {
  L17 = SIG_is418491101(ATTR(am,fun));
 } else {
  L17 = FALSE;
 }
 if (L17) {
  L16 = ATTR(ATTR(ATTR(am,fun),builtin_info),use_index);
 } else {
  L16 = FALSE;
 }
 SATTR(am,use_loop_index,L16);
 if (ATTR(am,use_loop_index)) {
  prog_self = self;
  ret_val3 = OPT_CO233462019;
  L19=ret_val3;
  L20 = ATTR(L19,stat1);
  (*dSTAT_incr_STR[TAG(L20)])(L20, ((STR) &Oiters1510323475));
 }
 if (ATTR(am,use_loop_index)) {
  L21 = (ATTR(ATTR(am,lp),loop_index_var)==((AM_LOCAL_EXPR) NULL));
 } else {
  L21 = FALSE;
 }
 if (L21) {
  prog_self1 = self;
  ret_val4 = OPT_CO233462019;
  L22=ret_val4;
  L23 = ATTR(L22,stat1);
  (*dSTAT_incr_STR[TAG(L23)])(L23, ((STR) &Oloops168062639));
  new_local_self = self;
  new_local_func = func;
  tp_int_self = self;
  ret_val5 = OPT_CO1800765742;
  new_local_tp = ret_val5;
  create_self = ((AM_LOCAL_EXPR) NULL);
  L24 = ATTR(new_local_func,code1);
  create_src = (*dAM_ST1805574914[TAG(L24)])(L24);
  create_name = IDENT_zero;
  create_tp = new_local_tp;
  L26=ZALLOC(sizeof(struct AM_LOCAL_EXPR_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=AM_LOCAL_EXPR_tag;
  L25 = ((AM_LOCAL_EXPR) L26);
  r = L25;
  SATTR(r,source1,create_src);
  SATTR(r,name1,create_name);
  SATTR(r,tp_at,create_tp);
  ret_val7 = r;
  lc = ret_val7;
  SATTR(new_local_func,locals1,FLISTA1817671564(ATTR(new_local_func,locals1), lc));
  ret_val6 = lc;
  SATTR(ATTR(am,lp),loop_index_var,ret_val6);
  create_self1 = ((AM_ASSIGN_STMT) NULL);
  create_source = ATTR(ATTR(am,lp),source1);
  L28=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=AM_ASSIGN_STMT_tag;
  L27 = ((AM_ASSIGN_STMT) L28);
  r1 = L27;
  SATTR(r1,source1,create_source);
  ret_val8 = r1;
  as = ret_val8;
  SATTR(as,dest,((dAM_EXPR) ATTR(ATTR(am,lp),loop_index_var)));
  zero_self = self;
  ret_val9 = OPT_CO232246741;
  SATTR(as,src1,((dAM_EXPR) ret_val9));
  create_self2 = ((AM_COMMENT_STMT) NULL);
  create_source1 = ATTR(am,source1);
  L30=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
  if (L30==NULL) FATAL("Unable to allocate more memory");
  ((OB)L30)->header.tag=AM_COMMENT_STMT_tag;
  L29 = ((AM_COMMENT_STMT) L30);
  r2 = L29;
  SATTR(r2,source1,create_source1);
  ret_val10 = r2;
  comment1 = ret_val10;
  SATTR(comment1,comment1,((STR) &loopin1411911962));
  AM_CUR352782528(c, ((dAM_STMT) comment1));
  AM_CUR352782528(c, ((dAM_STMT) as));
  create_self3 = ((AM_ASSIGN_STMT) NULL);
  create_source2 = ATTR(ATTR(am,lp),source1);
  L32=ZALLOC(sizeof(struct AM_ASSIGN_STMT_struct));
  if (L32==NULL) FATAL("Unable to allocate more memory");
  ((OB)L32)->header.tag=AM_ASSIGN_STMT_tag;
  L31 = ((AM_ASSIGN_STMT) L32);
  r3 = L31;
  SATTR(r3,source1,create_source2);
  ret_val11 = r3;
  p = ret_val11;
  SATTR(p,dest,((dAM_EXPR) ATTR(ATTR(am,lp),loop_index_var)));
  create_self4 = ((AM_ROU1916046290) NULL);
  create_nargs = 2;
  create_source3 = ATTR(ATTR(am,lp),source1);
  L34 = create_nargs;
  L36=(sizeof(struct AM_ROU1916046290_struct)-sizeof(AM_CALL_ARG))+(L34)*sizeof(AM_CALL_ARG);
  L35=ZALLOC_BIG(L36);
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=AM_ROU1916046290_tag;
#ifdef DESTROY_CHK

    ((OB)L35)->header.destroyed=0;
#endif

  L33 = ((AM_ROU1916046290) L35);
  L33->asize = L34;
  r4 = L33;
  SATTR(r4,source1,create_source3);
  ret_val12 = r4;
  mi = ret_val12;
  SATTR(func,calls1,FLISTd805769956(ATTR(func,calls1), ((dAM_EXPR) mi)));
  create_self5 = ((AM_CALL_ARG) NULL);
  create_e = ATTR(p,dest);
  L38=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=AM_CALL_ARG_tag;
  L37 = ((AM_CALL_ARG) L38);
  res = L37;
  SATTR(res,expr,create_e);
  SATTR(res,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val13 = res;
  L39 = ret_val13;
  SARR((AM_ROU1916046290)mi,0,(AM_CALL_ARG)L39); 
  ;
  create_self6 = ((AM_CALL_ARG) NULL);
  one_self = self;
  ret_val14 = OPT_CO238346998;
  create_e1 = ((dAM_EXPR) ret_val14);
  L43=ZALLOC(sizeof(struct AM_CALL_ARG_struct));
  if (L43==NULL) FATAL("Unable to allocate more memory");
  ((OB)L43)->header.tag=AM_CALL_ARG_tag;
  L42 = ((AM_CALL_ARG) L43);
  res1 = L42;
  SATTR(res1,expr,create_e1);
  SATTR(res1,mode1,((dMODE) IN_MOD403789248(((IN_MODE) NULL))));
  ret_val15 = res1;
  L44 = ret_val15;
  SARR((AM_ROU1916046290)mi,1,(AM_CALL_ARG)L44); 
  ;
  create_self7 = ((ARRAYARG) NULL);
  create_n = 1;
  L47 = create_n;
  L49=(sizeof(struct ARRAYARG_struct)-sizeof(ARG))+(L47)*sizeof(ARG);
  L48=ZALLOC_BIG(L49);
  if (L48==NULL) FATAL("Unable to allocate more memory");
  ((OB)L48)->header.tag=ARRAYARG_tag;
#ifdef DESTROY_CHK

    ((OB)L48)->header.destroyed=0;
#endif

  L46 = ((ARRAYARG) L48);
  L46->asize = L47;
  ret_val16 = L46;
  args = ret_val16;
  create_self8 = ((ARG) NULL);
  tp_int_self1 = self;
  ret_val17 = OPT_CO1800765742;
  create_t = ret_val17;
  L51=ZALLOC(sizeof(struct ARG_struct));
  if (L51==NULL) FATAL("Unable to allocate more memory");
  ((OB)L51)->header.tag=ARG_tag;
  L50 = ((ARG) L51);
  init_self = L50;
  init_t = create_t;
  init_m = ((dMODE) IN_MOD403789248(((IN_MODE) NULL)));
  init_same = FALSE;
  SATTR(init_self,tp,init_t);
  SATTR(init_self,mode1,init_m);
  SATTR(init_self,issame,init_same);
  ret_val19 = init_self;
  ret_val18 = ret_val19;
  L52 = ret_val18;
  SARR((ARRAYARG)args,0,(ARG)L52); 
  ;
  create_self10 = ((SIG) NULL);
  tp_int_self2 = self;
  ret_val20 = OPT_CO1800765742;
  create_ntp = ret_val20;
  create_self9 = ((ARRAYdTP) NULL);
  create_n1 = 0;
  L55 = create_n1;
  L57=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L55)*sizeof(dTP);
  L56=ZALLOC_BIG(L57);
  if (L56==NULL) FATAL("Unable to allocate more memory");
  ((OB)L56)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

    ((OB)L56)->header.destroyed=0;
#endif

  L54 = ((ARRAYdTP) L56);
  L54->asize = L55;
  ret_val21 = L54;
  create_tparr = ret_val21;
  create_nname = IDENT_2085547498;
  create_nargs1 = args;
  tp_int_self3 = self;
  ret_val22 = OPT_CO1800765742;
  create_nret = ret_val22;
  create_nsame = TRUE;
  L59=ZALLOC(sizeof(struct SIG_struct));
  if (L59==NULL) FATAL("Unable to allocate more memory");
  ((OB)L59)->header.tag=SIG_tag;
  L58 = ((SIG) L59);
  r5 = L58;
  SATTR(r5,tp,create_ntp);
  SATTR(r5,src_tparams,create_tparr);
  SATTR(r5,name1,create_nname);
  SATTR(r5,args,create_nargs1);
  SATTR(r5,ret,create_nret);
  SATTR(r5,returns_same,create_nsame);
  ret_val23 = SIG_ma1442297600(r5);
  SATTR(mi,fun,ret_val23);
  prog_self2 = self;
  ret_val24 = OPT_CO233462019;
  L60=ret_val24;
  L61 = ATTR(L60,generate_am);
  (*dGENER651759373[TAG(L61)])(L61, ATTR(mi,fun));
  SATTR(p,src1,((dAM_EXPR) mi));
  AM_CUR1966744225(c, ((dAM_STMT) p));
 }
 while (1) {
  L63 = (stmt1==((dAM_STMT) NULL));
  L641_=!(L63); 
  if (L641_) {
   L62 = do_optimize;
  } else {
   L62 = FALSE;
  }
  if (L62) {
  }
  else {
   goto after_loop;
  }
  if (stmt1==NULL) {
   goto other319;
  } else
  switch (TAG(stmt1)) {
   case AM_ASSIGN_STMT_tag:
    L65 = AM_CUR45133320(c, ((dAM) ATTR(((AM_ASSIGN_STMT) stmt1),src1)));
    L661_=!(L65); 
    if (L661_) {
     L68 = ATTR(c,loops);
     L691_=ARR((ARRAYBOOL)after_loop_break,L68); 
     if (L691_) {
      L67 = TRUE;
     } else {
      L67 = in_conditional;
     }
     need_safe_attr = L67;
     do_optimize = OPT_IT24605408(self, ATTR(((AM_ASSIGN_STMT) stmt1),src1), c, need_safe_attr, in_conditional);
     has_attr = OPT_IT2079146999(self, ((dAM) ATTR(((AM_ASSIGN_STMT) stmt1),src1)));
     L721_=!(do_optimize); 
     if (L721_) {
      prog_self3 = self;
      ret_val25 = OPT_CO233462019;
      L73=ret_val25;
      L70 = ATTR(L73,opt_debug);
     } else {
      L70 = FALSE;
     }
     if (L70) {
      create_self11 = ((OUT) NULL);
      ret_val26 = create_self11;
      plus_self = ret_val26;
      plus_s = ((STR) &thefol726840669);
      stdout_self = ((FILE1) NULL);
      L75=ZALLOC(sizeof(struct FILE1_struct));
      if (L75==NULL) FATAL("Unable to allocate more memory");
      ((OB)L75)->header.tag=FILE1_tag;
      L74 = ((FILE1) L75);
      r6 = L74;
      
      SATTR(r6,fp,stdout);
      ret_val28 = r6;
      FILE_plus_STR(ret_val28, plus_s);
      ret_val27 = plus_self;
      plus_self1 = ret_val27;
      L78=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
      if (L78==NULL) FATAL("Unable to allocate more memory");
      memset(L78,0,sizeof(struct BOOL_boxed_struct));
      ((OB)L78)->header.tag=BOOL_tag;
      L77 = (dSTR)((BOOL_boxed) L78);
      ((BOOL_boxed) L77)->immutable_part = need_safe_attr;
      plus_s1 = L77;
      stdout_self1 = ((FILE1) NULL);
      L80=ZALLOC(sizeof(struct FILE1_struct));
      if (L80==NULL) FATAL("Unable to allocate more memory");
      ((OB)L80)->header.tag=FILE1_tag;
      L79 = ((FILE1) L80);
      r7 = L79;
      
      SATTR(r7,fp,stdout);
      ret_val30 = r7;
      FILE_plus_dSTR(ret_val30, plus_s1);
      ret_val29 = plus_self1;
      plus_self2 = ret_val29;
      plus_s2 = ((STR) &in_cond);
      stdout_self2 = ((FILE1) NULL);
      L83=ZALLOC(sizeof(struct FILE1_struct));
      if (L83==NULL) FATAL("Unable to allocate more memory");
      ((OB)L83)->header.tag=FILE1_tag;
      L82 = ((FILE1) L83);
      r8 = L82;
      
      SATTR(r8,fp,stdout);
      ret_val32 = r8;
      FILE_plus_STR(ret_val32, plus_s2);
      ret_val31 = plus_self2;
      plus_self3 = ret_val31;
      L86=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
      if (L86==NULL) FATAL("Unable to allocate more memory");
      memset(L86,0,sizeof(struct BOOL_boxed_struct));
      ((OB)L86)->header.tag=BOOL_tag;
      L85 = (dSTR)((BOOL_boxed) L86);
      ((BOOL_boxed) L85)->immutable_part = in_conditional;
      plus_s3 = L85;
      stdout_self3 = ((FILE1) NULL);
      L88=ZALLOC(sizeof(struct FILE1_struct));
      if (L88==NULL) FATAL("Unable to allocate more memory");
      ((OB)L88)->header.tag=FILE1_tag;
      L87 = ((FILE1) L88);
      r9 = L87;
      
      SATTR(r9,fp,stdout);
      ret_val34 = r9;
      FILE_plus_dSTR(ret_val34, plus_s3);
      ret_val33 = plus_self3;
      plus_self4 = ret_val33;
      plus_s4 = ((STR) &name50);
      stdout_self4 = ((FILE1) NULL);
      L92=ZALLOC(sizeof(struct FILE1_struct));
      if (L92==NULL) FATAL("Unable to allocate more memory");
      ((OB)L92)->header.tag=FILE1_tag;
      L90 = ((FILE1) L92);
      r10 = L90;
      
      SATTR(r10,fp,stdout);
      ret_val35 = r10;
      FILE_plus_STR(ret_val35, plus_s4);
      AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(((AM_ASSIGN_STMT) stmt1),src1)));
      create_self12 = ((OUT) NULL);
      ret_val36 = create_self12;
      plus_self5 = ret_val36;
      plus_s5 = ((STR) &name9);
      stdout_self5 = ((FILE1) NULL);
      L95=ZALLOC(sizeof(struct FILE1_struct));
      if (L95==NULL) FATAL("Unable to allocate more memory");
      ((OB)L95)->header.tag=FILE1_tag;
      L94 = ((FILE1) L95);
      r11 = L94;
      
      SATTR(r11,fp,stdout);
      ret_val37 = r11;
      FILE_plus_STR(ret_val37, plus_s5);
     }
    } break;
   default: ;
   other319: ;
    prog_self4 = self;
    ret_val38 = OPT_CO233462019;
    L97=ret_val38;
    if (ATTR(L97,opt_debug)) {
     create_self13 = ((OUT) NULL);
     ret_val39 = create_self13;
     plus_self6 = ret_val39;
     plus_s6 = ((STR) &ignoringiter);
     stdout_self6 = ((FILE1) NULL);
     L99=ZALLOC(sizeof(struct FILE1_struct));
     if (L99==NULL) FATAL("Unable to allocate more memory");
     ((OB)L99)->header.tag=FILE1_tag;
     L98 = ((FILE1) L99);
     r12 = L98;
     
     SATTR(r12,fp,stdout);
     ret_val41 = r12;
     FILE_plus_STR(ret_val41, plus_s6);
     ret_val40 = plus_self6;
     plus_self7 = ret_val40;
     plus_s7 = ATTR(ATTR(am,fun),str);
     stdout_self7 = ((FILE1) NULL);
     L102=ZALLOC(sizeof(struct FILE1_struct));
     if (L102==NULL) FATAL("Unable to allocate more memory");
     ((OB)L102)->header.tag=FILE1_tag;
     L101 = ((FILE1) L102);
     r13 = L101;
     
     SATTR(r13,fp,stdout);
     ret_val43 = r13;
     FILE_plus_STR(ret_val43, plus_s7);
     ret_val42 = plus_self7;
     plus_self8 = ret_val42;
     plus_s8 = ((STR) &becaus2028109384);
     stdout_self8 = ((FILE1) NULL);
     L105=ZALLOC(sizeof(struct FILE1_struct));
     if (L105==NULL) FATAL("Unable to allocate more memory");
     ((OB)L105)->header.tag=FILE1_tag;
     L104 = ((FILE1) L105);
     r14 = L104;
     
     SATTR(r14,fp,stdout);
     ret_val44 = r14;
     FILE_plus_STR(ret_val44, plus_s8);
    }
    do_optimize = FALSE;
  }
  L107 = stmt1;
  stmt1 = (*dAM_ST775224435[TAG(L107)])(L107);
 }
 after_loop: ;
 if (do_optimize) {
  L110 = need_safe_attr;
 } else {
  L110 = FALSE;
 }
 if (L110) {
  L109 = has_attr;
 } else {
  L109 = FALSE;
 }
 if (L109) {
  prog_self5 = self;
  ret_val45 = OPT_CO233462019;
  L112=ret_val45;
  L108 = ATTR(L112,void_checks);
 } else {
  L108 = FALSE;
 }
 if (L108) {
  do_optimize = FALSE;
 }
 if (do_optimize) {
  L113 = OPT_IT59245853(self, func, am, c, TRUE, need_safe_attr, in_conditional);
  L1141_=!(L113); 
  if (L1141_) {
   prog_self6 = self;
   ret_val46 = OPT_CO233462019;
   L115=ret_val46;
   L116 = ATTR(L115,stat1);
   (*dSTAT_incr_STR[TAG(L116)])(L116, ((STR) &Oofite1407208962));
   prog_self7 = self;
   ret_val47 = OPT_CO233462019;
   L118=ret_val47;
   if (ATTR(L118,opt_verbose)) {
    L119 = SIG_is418491101(ATTR(am,fun));
    L1201_=!(L119); 
    L117 = L1201_;
   } else {
    L117 = FALSE;
   }
   if (L117) {
    create_self14 = ((OUT) NULL);
    ret_val48 = create_self14;
    plus_self9 = ret_val48;
    plus_s9 = SFILE_ID_strrSTR(ATTR(am,source1));
    stdout_self9 = ((FILE1) NULL);
    L122=ZALLOC(sizeof(struct FILE1_struct));
    if (L122==NULL) FATAL("Unable to allocate more memory");
    ((OB)L122)->header.tag=FILE1_tag;
    L121 = ((FILE1) L122);
    r15 = L121;
    
    SATTR(r15,fp,stdout);
    ret_val50 = r15;
    FILE_plus_STR(ret_val50, plus_s9);
    ret_val49 = plus_self9;
    plus_self10 = ret_val49;
    plus_s10 = ((STR) &INFOiter);
    stdout_self10 = ((FILE1) NULL);
    L125=ZALLOC(sizeof(struct FILE1_struct));
    if (L125==NULL) FATAL("Unable to allocate more memory");
    ((OB)L125)->header.tag=FILE1_tag;
    L124 = ((FILE1) L125);
    r16 = L124;
    
    SATTR(r16,fp,stdout);
    ret_val52 = r16;
    FILE_plus_STR(ret_val52, plus_s10);
    ret_val51 = plus_self10;
    plus_self11 = ret_val51;
    plus_s11 = ATTR(ATTR(am,fun),str);
    stdout_self11 = ((FILE1) NULL);
    L128=ZALLOC(sizeof(struct FILE1_struct));
    if (L128==NULL) FATAL("Unable to allocate more memory");
    ((OB)L128)->header.tag=FILE1_tag;
    L127 = ((FILE1) L128);
    r17 = L127;
    
    SATTR(r17,fp,stdout);
    ret_val54 = r17;
    FILE_plus_STR(ret_val54, plus_s11);
    ret_val53 = plus_self11;
    plus_self12 = ret_val53;
    plus_s12 = ((STR) &ignoredbyinliner);
    stdout_self12 = ((FILE1) NULL);
    L132=ZALLOC(sizeof(struct FILE1_struct));
    if (L132==NULL) FATAL("Unable to allocate more memory");
    ((OB)L132)->header.tag=FILE1_tag;
    L130 = ((FILE1) L132);
    r18 = L130;
    
    SATTR(r18,fp,stdout);
    ret_val55 = r18;
    FILE_plus_STR(ret_val55, plus_s12);
   }
   prog_self8 = self;
   ret_val56 = OPT_CO233462019;
   L134=ret_val56;
   if (ATTR(L134,hoist_iter_init)) {
    if (need_safe_attr) {
     OPT_IT222343591(self, ((dAM) ATTR(am,init)));
    }
    move_init_stmts_ = self;
    move_init_stmts_1 = func;
    move_init_stmts_2 = am;
    move_init_stmts_3 = c;
    prog_self9 = move_init_stmts_;
    ret_val57 = OPT_CO233462019;
    L135=ret_val57;
    L136 = ATTR(L135,stat1);
    (*dSTAT_incr_STR[TAG(L136)])(L136, ((STR) &Oofini2113488966));
    is_iter_self3 = move_init_stmts_1;
    is_iter_self4 = ATTR(is_iter_self3,sig1);
    is_iter_self5 = ATTR(is_iter_self4,name1);
    L138 = (is_iter_self5.str==((STR) NULL));
    L1391_=!(L138); 
    if (L1391_) {
     L140 = is_iter_self5.str;
     L141 = STR_sizerINT(is_iter_self5.str);
     L1421_=INTMINUS(L141,1); 
     L143 = L1421_;
     L1441_=ARR((STR)L140,L143); 
     L145 = L1441_;
     L1461_=L145=='!'; 
     L137 = L1461_;
    } else {
     L137 = FALSE;
    }
    ret_val60 = L137;
    ret_val59 = ret_val60;
    ret_val58 = ret_val59;
    L147 = ret_val58;
    L1481_=!(L147); 
    if (L1481_) {
     AM_CUR352782528(move_init_stmts_3, ATTR(move_init_stmts_2,init));
     SATTR(move_init_stmts_2,init,((dAM_STMT) NULL));
     SATTR(move_init_stmts_2,init_before_loop,TRUE);
    }
   }
  }
 }
 else {
  prog_self10 = self;
  ret_val61 = OPT_CO233462019;
  L150=ret_val61;
  if (ATTR(L150,opt_verbose)) {
   L152 = SIG_is418491101(ATTR(am,fun));
   L1531_=!(L152); 
   L149 = L1531_;
  } else {
   L149 = FALSE;
  }
  if (L149) {
   create_self15 = ((OUT) NULL);
   ret_val62 = create_self15;
   plus_self13 = ret_val62;
   plus_s13 = SFILE_ID_strrSTR(ATTR(am,source1));
   stdout_self13 = ((FILE1) NULL);
   L155=ZALLOC(sizeof(struct FILE1_struct));
   if (L155==NULL) FATAL("Unable to allocate more memory");
   ((OB)L155)->header.tag=FILE1_tag;
   L154 = ((FILE1) L155);
   r19 = L154;
   
   SATTR(r19,fp,stdout);
   ret_val64 = r19;
   FILE_plus_STR(ret_val64, plus_s13);
   ret_val63 = plus_self13;
   plus_self14 = ret_val63;
   plus_s14 = ((STR) &INFOiter);
   stdout_self14 = ((FILE1) NULL);
   L158=ZALLOC(sizeof(struct FILE1_struct));
   if (L158==NULL) FATAL("Unable to allocate more memory");
   ((OB)L158)->header.tag=FILE1_tag;
   L157 = ((FILE1) L158);
   r20 = L157;
   
   SATTR(r20,fp,stdout);
   ret_val66 = r20;
   FILE_plus_STR(ret_val66, plus_s14);
   ret_val65 = plus_self14;
   plus_self15 = ret_val65;
   plus_s15 = ATTR(ATTR(am,fun),str);
   stdout_self15 = ((FILE1) NULL);
   L161=ZALLOC(sizeof(struct FILE1_struct));
   if (L161==NULL) FATAL("Unable to allocate more memory");
   ((OB)L161)->header.tag=FILE1_tag;
   L160 = ((FILE1) L161);
   r21 = L160;
   
   SATTR(r21,fp,stdout);
   ret_val68 = r21;
   FILE_plus_STR(ret_val68, plus_s15);
   ret_val67 = plus_self15;
   plus_self16 = ret_val67;
   plus_s16 = ((STR) &ignore1571167589);
   stdout_self16 = ((FILE1) NULL);
   L164=ZALLOC(sizeof(struct FILE1_struct));
   if (L164==NULL) FATAL("Unable to allocate more memory");
   ((OB)L164)->header.tag=FILE1_tag;
   L163 = ((FILE1) L164);
   r22 = L163;
   
   SATTR(r22,fp,stdout);
   ret_val69 = r22;
   FILE_plus_STR(ret_val69, plus_s16);
  }
  L166 = OPT_IT59245853(self, func, am, c, FALSE, FALSE, in_conditional);
  L1671_=!(L166); 
  if (L1671_) {
   prog_self11 = self;
   ret_val70 = OPT_CO233462019;
   L169=ret_val70;
   if (ATTR(L169,opt_verbose)) {
    L170 = SIG_is418491101(ATTR(am,fun));
    L1711_=!(L170); 
    L168 = L1711_;
   } else {
    L168 = FALSE;
   }
   if (L168) {
    create_self16 = ((OUT) NULL);
    ret_val71 = create_self16;
    plus_self17 = ret_val71;
    plus_s17 = SFILE_ID_strrSTR(ATTR(am,source1));
    stdout_self17 = ((FILE1) NULL);
    L173=ZALLOC(sizeof(struct FILE1_struct));
    if (L173==NULL) FATAL("Unable to allocate more memory");
    ((OB)L173)->header.tag=FILE1_tag;
    L172 = ((FILE1) L173);
    r23 = L172;
    
    SATTR(r23,fp,stdout);
    ret_val73 = r23;
    FILE_plus_STR(ret_val73, plus_s17);
    ret_val72 = plus_self17;
    plus_self18 = ret_val72;
    plus_s18 = ((STR) &INFOiter);
    stdout_self18 = ((FILE1) NULL);
    L176=ZALLOC(sizeof(struct FILE1_struct));
    if (L176==NULL) FATAL("Unable to allocate more memory");
    ((OB)L176)->header.tag=FILE1_tag;
    L175 = ((FILE1) L176);
    r24 = L175;
    
    SATTR(r24,fp,stdout);
    ret_val75 = r24;
    FILE_plus_STR(ret_val75, plus_s18);
    ret_val74 = plus_self18;
    plus_self19 = ret_val74;
    plus_s19 = ATTR(ATTR(am,fun),str);
    stdout_self19 = ((FILE1) NULL);
    L179=ZALLOC(sizeof(struct FILE1_struct));
    if (L179==NULL) FATAL("Unable to allocate more memory");
    ((OB)L179)->header.tag=FILE1_tag;
    L178 = ((FILE1) L179);
    r25 = L178;
    
    SATTR(r25,fp,stdout);
    ret_val77 = r25;
    FILE_plus_STR(ret_val77, plus_s19);
    ret_val76 = plus_self19;
    plus_self20 = ret_val76;
    plus_s20 = ((STR) &ignoredbyinliner);
    stdout_self20 = ((FILE1) NULL);
    L183=ZALLOC(sizeof(struct FILE1_struct));
    if (L183==NULL) FATAL("Unable to allocate more memory");
    ((OB)L183)->header.tag=FILE1_tag;
    L182 = ((FILE1) L183);
    r26 = L182;
    
    SATTR(r26,fp,stdout);
    ret_val78 = r26;
    FILE_plus_STR(ret_val78, plus_s20);
   }
   prog_self12 = self;
   ret_val79 = OPT_CO233462019;
   L185=ret_val79;
   L186 = ATTR(L185,stat1);
   (*dSTAT_incr_STR[TAG(L186)])(L186, ((STR) &Oofite2134272122));
  }
  prog_self13 = self;
  ret_val80 = OPT_CO233462019;
  L187=ret_val80;
  L188 = ATTR(L187,stat1);
  (*dSTAT_incr_STR[TAG(L188)])(L188, ((STR) &Oofite361277457));
 }
}


#undef IS_ITER
#define IS_ITER 0

void PARSE_1609412102(PARSE self, STR f) {
 PARSER parser;
 AS_CLASS_DEF tcd;
 AS_CLASS_DEF ntcd;
 TUPIDENTINT typetag = TUPIDENTINT_zero;
 AS_CLASS_DEF other_class;
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
 PROG psather_self;
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
 OUT ret_val12;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val14;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val16;
 FILE1 r7;
 TUPIDENTINT create_self2 = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val17 = TUPIDENTINT_zero;
 extern STR Parse;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name23;
 FILE1 L7;
 OB L8;
 BOOL L10;
 BOOL L11;
 BOOL L12;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 extern STR Treefor;
 FILE1 L16;
 OB L17;
 FILE1 L19;
 OB L20;
 extern STR name24;
 FILE1 L22;
 OB L23;
 dSTR L25;
 OB L26;
 FILE1 L27;
 OB L28;
 extern STR name25;
 FILE1 L30;
 OB L32;
 TUPIDENTINT L34;
 TUPIDENTINT L35;
 if (FSETST1025458804(ATTR(self,parsed), f)) {
  return;
 }
 SATTR(self,parsed,FSETST1404644833(ATTR(self,parsed), f));
 if (ATTR(ATTR(self,prog),show_parse_file)) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &Parse);
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
  plus_s1 = f;
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
  plus_s2 = ((STR) &name23);
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
 psather_self = ATTR(self,prog);
 if (ATTR(psather_self,threads)) {
  L10 = TRUE;
 } else {
  L10 = ATTR(psather_self,distributed);
 }
 ret_val6 = L10;
 if (FSETST1025458804(ATTR(self,convert_files), f)) {
  L11 = TRUE;
 } else {
  L11 = ATTR(self,convert_all);
 }
 if (ATTR(self,version_1_0)) {
  L13 = TRUE;
 } else {
  L13 = FSETST1025458804(ATTR(self,convert_files), f);
 }
 if (L13) {
  L12 = TRUE;
 } else {
  L12 = ATTR(self,convert_all);
 }
 parser = PARSER403165095(((PARSER) NULL), ATTR(self,prog), f, ret_val6, L11, L12);
 L14 = (parser==((PARSER) NULL));
 L151_=!(L14); 
 if (L151_) {
  tcd = PARSER2058766590(parser);
  while (1) {
   if ((tcd==((AS_CLASS_DEF) NULL))) {
    goto after_loop;
   }
   if (ATTR(ATTR(self,prog),show_as_insert)) {
    create_self1 = ((OUT) NULL);
    ret_val7 = create_self1;
    plus_self3 = ret_val7;
    plus_s3 = ((STR) &Treefor);
    stdout_self3 = ((FILE1) NULL);
    L17=ZALLOC(sizeof(struct FILE1_struct));
    if (L17==NULL) FATAL("Unable to allocate more memory");
    ((OB)L17)->header.tag=FILE1_tag;
    L16 = ((FILE1) L17);
    r3 = L16;
    
    SATTR(r3,fp,stdout);
    ret_val9 = r3;
    FILE_plus_STR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    plus_s4 = ATTR(tcd,name1).str;
    stdout_self4 = ((FILE1) NULL);
    L20=ZALLOC(sizeof(struct FILE1_struct));
    if (L20==NULL) FATAL("Unable to allocate more memory");
    ((OB)L20)->header.tag=FILE1_tag;
    L19 = ((FILE1) L20);
    r4 = L19;
    
    SATTR(r4,fp,stdout);
    ret_val11 = r4;
    FILE_plus_STR(ret_val11, plus_s4);
    ret_val10 = plus_self4;
    plus_self5 = ret_val10;
    plus_s5 = ((STR) &name24);
    stdout_self5 = ((FILE1) NULL);
    L23=ZALLOC(sizeof(struct FILE1_struct));
    if (L23==NULL) FATAL("Unable to allocate more memory");
    ((OB)L23)->header.tag=FILE1_tag;
    L22 = ((FILE1) L23);
    r5 = L22;
    
    SATTR(r5,fp,stdout);
    ret_val13 = r5;
    FILE_plus_STR(ret_val13, plus_s5);
    ret_val12 = plus_self5;
    plus_self6 = ret_val12;
    L26=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L26==NULL) FATAL("Unable to allocate more memory");
    memset(L26,0,sizeof(struct INT_boxed_struct));
    ((OB)L26)->header.tag=INT_tag;
    L25 = (dSTR)((INT_boxed) L26);
    ((INT_boxed) L25)->immutable_part = AS_PAR401840644(ATTR(tcd,params));
    plus_s6 = L25;
    stdout_self6 = ((FILE1) NULL);
    L28=ZALLOC(sizeof(struct FILE1_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=FILE1_tag;
    L27 = ((FILE1) L28);
    r6 = L27;
    
    SATTR(r6,fp,stdout);
    ret_val15 = r6;
    FILE_plus_dSTR(ret_val15, plus_s6);
    ret_val14 = plus_self6;
    plus_self7 = ret_val14;
    plus_s7 = ((STR) &name25);
    stdout_self7 = ((FILE1) NULL);
    L32=ZALLOC(sizeof(struct FILE1_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    ((OB)L32)->header.tag=FILE1_tag;
    L30 = ((FILE1) L32);
    r7 = L30;
    
    SATTR(r7,fp,stdout);
    ret_val16 = r7;
    FILE_plus_STR(ret_val16, plus_s7);
   }
   ntcd = ATTR(tcd,next);
   SATTR(tcd,next,((AS_CLASS_DEF) NULL));
   create_self2 = TUPIDENTINT_zero;
   create_at1 = ATTR(tcd,name1);
   create_at2 = AS_PAR401840644(ATTR(tcd,params));
   L34 = create_self2;
   L34.t1 = create_at1;
   L35 = L34;
   L35.t2 = create_at2;
   ret_val17 = L35;
   typetag = ret_val17;
   if (PROG_A878808304(ATTR(ATTR(self,prog),as_tbl), typetag)) {
    other_class = PROG_A382977477(ATTR(ATTR(self,prog),as_tbl), typetag);
    PARSE_947379252(self, tcd, other_class);
   }
   else {
    SATTR(ATTR(self,prog),as_tbl,PROG_A2047269051(ATTR(ATTR(self,prog),as_tbl), tcd));
   }
   tcd = ntcd;
  }
  after_loop: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void PARSE_947379252(PARSE self, AS_CLASS_DEF this_class, AS_CLASS_DEF other_class) {
 PARSE err_loc_self;
 dPROG_ERR err_loc_t;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 INT plus_arg = INT_zero;
 STR ret_val2;
 STR s;
 INT str_self = INT_zero;
 STR ret_val3;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val4;
 FSTR str_self1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 PARSE err_self;
 STR err_s;
 extern STR Therea656944534;
 extern STR and3;
 BOOL L1;
 BOOL L21_;
 extern STR parame770948886;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) this_class);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 err_self = self;
 plus_self = ((STR) &Therea656944534);
 plus_sarg = ATTR(this_class,name1).str;
 ret_val = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val;
 plus_sarg1 = ((STR) &and3);
 ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val1;
 plus_arg = AS_PAR401840644(ATTR(this_class,params));
 str_self = plus_arg;
 clear_self = buf1;
 L1 = (clear_self==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  SATTR(clear_self,loc,0);
 }
 str_in_self = str_self;
 str_in_s = buf1;
 ret_val4 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
 buf1 = ret_val4;
 str_self1 = buf1;
 ret_val5 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val3 = ret_val5;
 s = ret_val3;
 ret_val2 = STR_ap1077157958(plus_self2, s, TRUE);
 plus_self3 = ret_val2;
 plus_sarg2 = ((STR) &parame770948886);
 ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
 plus_self4 = ret_val6;
 plus_sarg3 = SFILE_ID_strrSTR(ATTR(other_class,source1));
 ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
 err_s = ret_val7;
 PROG_err_STR(ATTR(err_self,prog), err_s);
}


#undef IS_ITER
#define IS_ITER 0

void PARSE_parse_STR(PARSE self, STR f) {
 if (FSETST1025458804(ATTR(self,known_files), f)) {
  return;
 }
 else {
  PARSE_1609412102(self, f);
 }
}
