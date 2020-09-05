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

typedef struct dMODE_struct {
 OB_HEADER header;
 } *dMODE;

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

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct AREFFL1698233817_struct {/* layout for AREF{FLIST{AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTA725283029_struct *arr_part[1];
 } *AREFFL1698233817;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYd1413132360_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1413132360_frame;

typedef struct ARRAYO570594127_struct {/* layout for ARRAY{OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT asize;
 struct OPT_LOCAL_EXPR_struct *arr_part[1];
 } *ARRAYO570594127;

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

typedef struct ELT_NI19483997_struct {/* layout for ELT_NIL{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI19483997;

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

typedef struct IN_MODE_struct {/* layout for IN_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *IN_MODE;

typedef struct OPT_LOCAL_struct {/* layout for OPT_LOCAL */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OPT_LOCAL;

typedef struct OPT_LOCAL_CALL_struct {/* layout for OPT_LOCAL_CALL */
 OB_HEADER header;
 struct AREFFL1698233817_struct *near_local;
 struct ARRAYINT_struct *attr_access;
 struct ARRAYINT_struct *consider;
 struct ARRAYINT_struct *near_depends_on_;
 BOOL is_near;
 } *OPT_LOCAL_CALL;

typedef struct OPT_LOCAL_EXPR_struct {/* layout for OPT_LOCAL_EXPR */
 OB_HEADER header;
 struct AM_LOCAL_EXPR_struct *local1;
 INT attr_access;
 INT index1;
 BOOL is_arg;
 BOOL is_sometimes_far;
 BOOL near_arg;
 } *OPT_LOCAL_EXPR;

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

typedef struct TUPAM_504653531_struct {/* layout for TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 struct AM_LOCAL_EXPR_struct *t1;
 struct OPT_LOCAL_EXPR_struct *t2;
 } TUPAM_504653531;
static TUPAM_504653531 TUPAM_504653531_zero;

typedef struct TUPAM_504653531_boxed_struct {
 OB_HEADER header;
 TUPAM_504653531 immutable_part;
 } *TUPAM_504653531_boxed;

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

typedef struct AM_ITE1159913222_frame_struct {
 INT state;
 AM_ITE1809135850 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ITE1159913222_frame;

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

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_RETURN_STMT_struct {/* layout for AM_RETURN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RETURN_STMT;

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

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

typedef struct AM_ROU948739923_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 AM_FORMAL_ARG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU948739923_frame;

typedef struct AM_ROU1569581452_frame_struct {
 INT state;
 AM_ROUT_DEF self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AM_ROU1569581452_frame;

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

typedef struct AM_WIT1996971603_struct {/* layout for AM_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_part;
 struct dAM_STMT_struct *near_part;
 struct dAM_STMT_struct *next;
 struct ARRAYdAM_EXPR_struct *objects1;
 struct SFILE_ID_struct source1;
 } *AM_WIT1996971603;

typedef struct AREFFL1603407436_frame_struct {
 INT state;
 AREFFL1698233817 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AREFFL1603407436_frame;

typedef struct ARRAY2BOOL_struct {/* layout for ARRAY2{BOOL} */
 OB_HEADER header;
 INT size1;
 INT size2;
 INT asize;
 BOOL arr_part[1];
 } *ARRAY2BOOL;

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

typedef struct ARRAYI180735351_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI180735351_frame;

typedef struct ARRAYI486979141_frame_struct {
 INT state;
 ARRAYINT self;/* Formal argument: self */
 INT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI486979141_frame;

typedef struct ARRAYO335724112_frame_struct {
 INT state;
 ARRAYO570594127 self;/* Formal argument: self */
 OPT_LOCAL_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYO335724112_frame;

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

typedef struct FLISTBOOL_struct {/* layout for FLIST{BOOL} */
 OB_HEADER header;
 INT loc;
 INT asize;
 BOOL arr_part[1];
 } *FLISTBOOL;

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FLISTI476396026_frame_struct {
 INT state;
 FLISTINT self;/* Formal argument: self */
 INT ret_val1;
 INT i;
 INT sz;
 FLISTINT aget_self;
 INT aget_ind;
 INT ret_val;
 } *FLISTI476396026_frame;

typedef struct FMAPAM418009930_struct {/* layout for FMAP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_504653531_struct arr_part[1];
 } *FMAPAM418009930;

typedef struct FMAPAM1518099927_frame_struct {
 INT state;
 FMAPAM418009930 self;/* Formal argument: self */
 TUPAM_504653531 ret_val2;
 FMAPAM418009930 L61;
 TUPAM_504653531 r;
 INT L31;
 FMAPAM418009930 is_key_nil_self;
 AM_LOCAL_EXPR is_key_nil_e;
 BOOL ret_val;
 ELT_NI19483997 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPAM1518099927_frame;

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

extern CS_OPTIONS OPT_CO1299251581;
extern INT INT_asize;
extern IN_MODE MODES_in_mode;
extern PROG OPT_CO233462019;
extern TP_CLASS TP_BUILTIN_sys;

/* Function declarations */


extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dMODE_814247358)(dMODE, dMODE);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST FLISTA725283029 (**dAM_EX1656723676)(dAM_EXPR);

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);
AM_CURSOR AM_CUR1277440868(AM_CURSOR, PROG, dAM);
AM_FORMAL_ARG AM_ROU948739923(AM_ROU948739923_frame);
AM_LOCAL_EXPR FLISTA2119642552(FLISTA2119642552_frame);
AM_WIT1996971603 AM_CUR554772257(AM_CURSOR);
ARRAYINT SIG_at89227380(SIG);
ARRAYINT SIG_ne271652128(SIG);
BOOL AM_CUR1778077145(AM_CURSOR);
BOOL CHAR_i1942379470(CHAR);
BOOL FLISTB466509411(FLISTBOOL);
BOOL SIG_is418491101(SIG);
BOOL STR_is111530248(STR, STR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTBOOL FLISTB1991913204(FLISTBOOL, BOOL);
FLISTINT FLISTI1369678776(FLISTINT, INT);
FMAPAM418009930 FMAPAM1345096032(FMAPAM418009930, AM_LOCAL_EXPR, OPT_LOCAL_EXPR);
INT AM_ITE1159913222(AM_ITE1159913222_frame);
INT AM_ROU1569581452(AM_ROU1569581452_frame);
INT AM_ROU2113525054(AM_ROU2113525054_frame);
INT AREFFL1603407436(AREFFL1603407436_frame);
INT ARRAYI180735351(ARRAYI180735351_frame);
INT ARRAYI69049459(ARRAYI69049459_frame);
INT FLISTA1151473122(FLISTA725283029);
INT FLISTI476396026(FLISTI476396026_frame);
INT INT_pow_INTrINT(INT, INT);
INT INT_timesbrINT(INT_timesbrINT_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT OPT_LO1727904116(OPT_LOCAL, AM_ROUT_DEF, FMAPAM418009930*, ARRAYO570594127);
INT STR_sizerINT(STR);
OPT_LOCAL_EXPR ARRAYO335724112(ARRAYO335724112_frame);
OPT_LOCAL_EXPR FMAPAM731742676(FMAPAM418009930, AM_LOCAL_EXPR);
STR ARRAYINT_strrSTR(ARRAYINT);
STR STR_ap1077157958(STR, STR, BOOL);
TUPAM_504653531 FMAPAM1518099927(FMAPAM1518099927_frame);
dAM AM_CUR1040670508(AM_CUR1040670508_frame);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
void AM_OUT1439666796(AM_OUT, dAM);
void ARRAYI486979141(ARRAYI486979141_frame);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void INT_timesb(INT_timesb_frame);
void OPT_LO1015703310(OPT_LOCAL, AM_ROUT_DEF);
void OPT_LO1196828642(OPT_LOCAL, ARRAY2BOOL);
void OPT_LO14752986(OPT_LOCAL, AM_ROUT_DEF, ARRAY2BOOL, FMAPAM418009930, ARRAYO570594127, OPT_LOCAL_EXPR);
void OPT_LO1523894656(OPT_LOCAL, FMAPAM418009930, ARRAY2BOOL, STR);
void OPT_LO238575330(OPT_LOCAL, AM_ROUT_DEF, ARRAY2BOOL, FMAPAM418009930, OPT_LOCAL_EXPR);
void OPT_LO447032529(OPT_LOCAL, AM_ROUT_DEF, ARRAY2BOOL, FMAPAM418009930, ARRAYO570594127);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

INT OPT_LO1727904116(OPT_LOCAL self, AM_ROUT_DEF func, FMAPAM418009930* m, ARRAYO570594127 ox_table) {
 FMAPAM418009930 L1 = *m;/*Local for arg*/
 INT ret_val = INT_zero;
 INT top_index = INT_zero;
 AM_ROUT_DEF L21;
 AM_LOCAL_EXPR l;
 OPT_LOCAL_EXPR ox;
 INT L31 = INT_zero;
 FLISTA725283029 L41;
 AM_LOCAL_EXPR l1;
 OPT_LOCAL_EXPR ox1;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val1;
 OPT_LOCAL_EXPR create_self;
 AM_LOCAL_EXPR create_am;
 OPT_LOCAL_EXPR ret_val2;
 OPT_LOCAL_EXPR r;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val3;
 OPT_LOCAL_EXPR create_self1;
 AM_LOCAL_EXPR create_am1;
 OPT_LOCAL_EXPR ret_val4;
 OPT_LOCAL_EXPR r1;
 INT L71_br;
 AM_FORMAL_ARG aL71_;
 AM_FORMAL_ARG L9;
 AM_FORMAL_ARG L10;
 dTP L11;
 BOOL L12;
 BOOL L131_;
 OPT_LOCAL_EXPR L14;
 OB L15;
 INT L16;
 INT L171_;
 INT L81_;
 INT L18;
 INT L201_;
 AM_LOCAL_EXPR L23;
 dTP L24;
 BOOL L25;
 BOOL L261_;
 OPT_LOCAL_EXPR L27;
 OB L28;
 INT L29;
 INT L301_;
 INT L221_;
 INT L32;
 top_index = 0;
 {
  BOOL f_L71_ = TRUE;
  BOOL f_L81_ = TRUE;
  /* loop index variable */
  L61 = 0;
  L21 = func;
  L71_br=L21==NULL?0:ASIZE((AM_ROUT_DEF)L21); 
  while (1) {
   if(L61>=L71_br)  goto after_loop; 
   aL71_=ARR((AM_ROUT_DEF)L21,L61); 
   L10=aL71_;
   l = ATTR(L10,expr);
   tp_self = l;
   ret_val1 = ATTR(tp_self,tp_at);
   L11 = ret_val1;
   L12 = (*dTP_is1334578382[TAG(L11)])(L11);
   L131_=!(L12); 
   if (L131_) {
    create_self = ((OPT_LOCAL_EXPR) NULL);
    create_am = l;
    L15=ZALLOC(sizeof(struct OPT_LOCAL_EXPR_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=OPT_LOCAL_EXPR_tag;
    L14 = ((OPT_LOCAL_EXPR) L15);
    r = L14;
    SATTR(r,local1,create_am);
    ret_val2 = r;
    ox = ret_val2;
    if (f_L81_) {
     f_L81_ = FALSE;
     L171_=INTPLUS(top_index,1); 
     L31 = L171_;
     L81_=L31-1; 
    }
    L81_++; 
    SATTR(ox,index1,L81_);
    SATTR(ox,is_arg,TRUE);
    L18 = ATTR(ox,index1);
    SARR((ARRAYO570594127)ox_table,L18,(OPT_LOCAL_EXPR)ox); 
    ;
    top_index = ATTR(ox,index1);
    L1 = FMAPAM1345096032(L1, l, ox);
   }
   L201_=INTPLUS(L61,1); 
   L61 = L201_;
  }
 }
 after_loop: ;
 {
  struct FLISTA2119642552_frame_struct other2_0;
FLISTA2119642552_frame noname1 = &other2_0;
  BOOL f_L221_ = TRUE;
  L41 = ATTR(func,locals1);
  noname1->self = L41;
  noname1->state = 0;
  while (1) {
   L23 = FLISTA2119642552(noname1);
   if (noname1->state == -1) {
    goto after_loop1;
   }
   l1 = L23;
   tp_self1 = l1;
   ret_val3 = ATTR(tp_self1,tp_at);
   L24 = ret_val3;
   L25 = (*dTP_is1334578382[TAG(L24)])(L24);
   L261_=!(L25); 
   if (L261_) {
    create_self1 = ((OPT_LOCAL_EXPR) NULL);
    create_am1 = l1;
    L28=ZALLOC(sizeof(struct OPT_LOCAL_EXPR_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=OPT_LOCAL_EXPR_tag;
    L27 = ((OPT_LOCAL_EXPR) L28);
    r1 = L27;
    SATTR(r1,local1,create_am1);
    ret_val4 = r1;
    ox1 = ret_val4;
    if (f_L221_) {
     f_L221_ = FALSE;
     L301_=INTPLUS(top_index,1); 
     L51 = L301_;
     L221_=L51-1; 
    }
    L221_++; 
    SATTR(ox1,index1,L221_);
    L32 = ATTR(ox1,index1);
    SARR((ARRAYO570594127)ox_table,L32,(OPT_LOCAL_EXPR)ox1); 
    ;
    top_index = ATTR(ox1,index1);
    L1 = FMAPAM1345096032(L1, l1, ox1);
   }
  }
 }
 after_loop1: ;
 ret_val = top_index;
 *m = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO1015703310(OPT_LOCAL self, AM_ROUT_DEF func) {
 FMAPAM418009930 m = ((FMAPAM418009930) NULL);
 ARRAYO570594127 ox_table;
 OPT_LOCAL_EXPR ret_ox;
 INT top_index = INT_zero;
 ARRAY2BOOL dep;
 INT L11 = INT_zero;
 INT i = INT_zero;
 INT L21 = INT_zero;
 INT i1 = INT_zero;
 INT L31 = INT_zero;
 INT i2 = INT_zero;
 INT L41 = INT_zero;
 INT j = INT_zero;
 INT L51 = INT_zero;
 INT i3 = INT_zero;
 INT L61 = INT_zero;
 INT j11 = INT_zero;
 AM_ROUT_DEF is_iter_self;
 BOOL ret_val = BOOL_zero;
 SIG is_iter_self1;
 BOOL ret_val1 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val2 = BOOL_zero;
 SIG is_forked_self;
 BOOL ret_val3 = BOOL_zero;
 OPT_LOCAL prog_self;
 PROG ret_val4;
 OUT create_self;
 OUT ret_val5;
 OUT plus_self;
 STR plus_s;
 OUT ret_val6;
 FILE1 stdout_self;
 FILE1 ret_val7;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val8;
 FILE1 stdout_self1;
 FILE1 ret_val9;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val10;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val11;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val12;
 FILE1 stdout_self3;
 FILE1 ret_val13;
 FILE1 r3;
 SIG is_near_self;
 BOOL ret_val14 = BOOL_zero;
 OUT plus_self4;
 dSTR plus_s4;
 OUT ret_val15;
 FILE1 stdout_self4;
 FILE1 ret_val16;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val17;
 FILE1 stdout_self5;
 FILE1 ret_val18;
 FILE1 r5;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val19;
 FILE1 stdout_self6;
 FILE1 ret_val20;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val21;
 FILE1 stdout_self7;
 FILE1 ret_val22;
 FILE1 r7;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val23;
 FILE1 stdout_self8;
 FILE1 ret_val24;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val25;
 FILE1 r9;
 ARRAYO570594127 create_self2;
 INT create_n = INT_zero;
 ARRAYO570594127 ret_val26;
 OPT_LOCAL_EXPR create_self3;
 AM_LOCAL_EXPR create_am;
 OPT_LOCAL_EXPR ret_val27;
 OPT_LOCAL_EXPR r10;
 OPT_LOCAL prog_self1;
 PROG ret_val28;
 OUT create_self4;
 OUT ret_val29;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val30;
 FILE1 r11;
 ARRAY2BOOL create_self5;
 INT create_d1 = INT_zero;
 INT create_d2 = INT_zero;
 ARRAY2BOOL ret_val31;
 ARRAY2BOOL res;
 OPT_LOCAL prog_self2;
 PROG ret_val32;
 OUT create_self6;
 OUT ret_val33;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val34;
 FILE1 r12;
 INT L71 = INT_zero;
 OUT create_self7;
 OUT ret_val35;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val36;
 FILE1 r13;
 OUT create_self8;
 OUT ret_val37;
 OUT plus_self13;
 dSTR plus_s13;
 OUT ret_val38;
 FILE1 stdout_self13;
 FILE1 ret_val39;
 FILE1 r14;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val40;
 FILE1 r15;
 OUT create_self9;
 OUT ret_val41;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val42;
 FILE1 r16;
 OUT create_self10;
 OUT ret_val43;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val44;
 FILE1 r17;
 INT L81 = INT_zero;
 OUT create_self11;
 OUT ret_val45;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val46;
 FILE1 r18;
 OUT create_self12;
 OUT ret_val47;
 OUT plus_self18;
 dSTR plus_s18;
 OUT ret_val48;
 FILE1 stdout_self18;
 FILE1 ret_val49;
 FILE1 r19;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val50;
 FILE1 r20;
 OUT create_self13;
 OUT ret_val51;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val52;
 FILE1 r21;
 INT L91 = INT_zero;
 OUT create_self14;
 OUT ret_val53;
 OUT plus_self21;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val54;
 FILE1 r22;
 OUT create_self15;
 OUT ret_val55;
 OUT plus_self22;
 dSTR plus_s22;
 OUT ret_val56;
 FILE1 stdout_self22;
 FILE1 ret_val57;
 FILE1 r23;
 OUT plus_self23;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val58;
 FILE1 r24;
 INT L101 = INT_zero;
 ARRAY2BOOL aget_self;
 INT aget_i1 = INT_zero;
 INT aget_i2 = INT_zero;
 BOOL ret_val59 = BOOL_zero;
 OUT create_self16;
 OUT ret_val60;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val61;
 FILE1 r25;
 OUT create_self17;
 OUT ret_val62;
 OUT plus_self25;
 STR plus_s25;
 FILE1 stdout_self25;
 FILE1 ret_val63;
 FILE1 r26;
 OUT create_self18;
 OUT ret_val64;
 OUT plus_self26;
 STR plus_s26;
 FILE1 stdout_self26;
 FILE1 ret_val65;
 FILE1 r27;
 OPT_LOCAL prog_self3;
 PROG ret_val66;
 OUT create_self19;
 OUT ret_val67;
 OUT plus_self27;
 STR plus_s27;
 FILE1 stdout_self27;
 FILE1 ret_val68;
 FILE1 r28;
 INT L121 = INT_zero;
 OUT create_self20;
 OUT ret_val69;
 OUT plus_self28;
 STR plus_s28;
 FILE1 stdout_self28;
 FILE1 ret_val70;
 FILE1 r29;
 OUT create_self21;
 OUT ret_val71;
 OUT plus_self29;
 dSTR plus_s29;
 OUT ret_val72;
 FILE1 stdout_self29;
 FILE1 ret_val73;
 FILE1 r30;
 OUT plus_self30;
 STR plus_s30;
 FILE1 stdout_self30;
 FILE1 ret_val74;
 FILE1 r31;
 INT L131 = INT_zero;
 ARRAY2BOOL aget_self1;
 INT aget_i11 = INT_zero;
 INT aget_i21 = INT_zero;
 BOOL ret_val75 = BOOL_zero;
 OUT create_self22;
 OUT ret_val76;
 OUT plus_self31;
 STR plus_s31;
 FILE1 stdout_self31;
 FILE1 ret_val77;
 FILE1 r32;
 OUT create_self23;
 OUT ret_val78;
 OUT plus_self32;
 STR plus_s32;
 FILE1 stdout_self32;
 FILE1 ret_val79;
 FILE1 r33;
 OUT create_self24;
 OUT ret_val80;
 OUT plus_self33;
 STR plus_s33;
 FILE1 stdout_self33;
 FILE1 ret_val81;
 FILE1 r34;
 OPT_LOCAL cs_options_self;
 CS_OPTIONS ret_val82;
 BOOL L14;
 BOOL L15;
 BOOL L16;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 STR L20;
 INT L22;
 INT L231_;
 INT L24;
 CHAR L251_;
 CHAR L26;
 BOOL L271_;
 BOOL L28;
 BOOL L29;
 PROG L30;
 extern STR RESULT1262569080;
 FILE1 L32;
 OB L33;
 FILE1 L35;
 OB L36;
 extern STR name11;
 FILE1 L38;
 OB L39;
 extern STR Functionis_near;
 FILE1 L42;
 OB L43;
 BOOL L45;
 BOOL L46;
 BOOL L47;
 dTP L48;
 BOOL L49;
 BOOL L50;
 BOOL L521_;
 dSTR L53;
 OB L54;
 FILE1 L55;
 OB L56;
 extern STR args3;
 FILE1 L58;
 OB L59;
 FILE1 L62;
 OB L63;
 extern STR access2;
 FILE1 L65;
 OB L66;
 FILE1 L68;
 OB L69;
 extern STR name9;
 FILE1 L72;
 OB L73;
 INT L75;
 INT L761_;
 INT L77;
 INT L781_;
 INT L79;
 INT L801_;
 ARRAYO570594127 L82;
 INT L83;
 OB L84;
 INT L85;
 OPT_LOCAL_EXPR L86;
 OB L87;
 FMAPAM418009930* L89;
 INT L90;
 BOOL L92;
 BOOL L931_;
 PROG L94;
 extern STR OPT_LO1160315301;
 FILE1 L95;
 OB L96;
 INT L981_;
 INT L991_;
 ARRAY2BOOL L100;
 INT L102;
 INT L1031_;
 OB L104;
 INT L105;
 PROG L106;
 extern STR depend1236761341;
 FILE1 L107;
 OB L108;
 INT aL1101_;
 INT L111;
 BOOL L1121_;
 extern STR name10;
 FILE1 L113;
 OB L114;
 dSTR L116;
 OB L117;
 FILE1 L118;
 OB L119;
 extern STR name68;
 FILE1 L122;
 OB L123;
 OPT_LOCAL_EXPR L1251_;
 OPT_LOCAL_EXPR L126;
 extern STR name9;
 FILE1 L127;
 OB L128;
 INT L1301_;
 extern STR name69;
 FILE1 L132;
 OB L133;
 INT aL1351_;
 INT L136;
 BOOL L1371_;
 extern STR name10;
 FILE1 L138;
 OB L139;
 dSTR L141;
 OB L142;
 FILE1 L143;
 OB L144;
 extern STR name10;
 FILE1 L146;
 OB L147;
 INT L1491_;
 extern STR name9;
 FILE1 L150;
 OB L151;
 INT aL1531_;
 INT L154;
 BOOL L1551_;
 extern STR name10;
 FILE1 L156;
 OB L157;
 dSTR L159;
 OB L160;
 FILE1 L161;
 OB L162;
 extern STR name68;
 FILE1 L164;
 OB L165;
 INT aL1671_;
 INT L168;
 INT L169;
 INT L1701_;
 INT L171;
 INT L1721_;
 INT L173;
 BOOL L1741_;
 extern STR name70;
 FILE1 L175;
 OB L176;
 extern STR name8;
 FILE1 L178;
 OB L179;
 INT L1811_;
 extern STR name9;
 FILE1 L182;
 OB L183;
 INT L1851_;
 PROG L186;
 extern STR AFTERflattening;
 FILE1 L187;
 OB L188;
 INT aL1901_;
 INT L192;
 BOOL L1931_;
 extern STR name10;
 FILE1 L194;
 OB L195;
 dSTR L197;
 OB L198;
 FILE1 L199;
 OB L200;
 extern STR name68;
 FILE1 L202;
 OB L203;
 INT aL2051_;
 INT L206;
 INT L207;
 INT L2081_;
 INT L209;
 INT L2101_;
 INT L211;
 BOOL L2121_;
 extern STR name70;
 FILE1 L213;
 OB L214;
 extern STR name8;
 FILE1 L216;
 OB L217;
 INT L2191_;
 extern STR name9;
 FILE1 L220;
 OB L221;
 INT L2231_;
 CS_OPTIONS L224;
 if (ATTR(func,is_abstract)) {
  L16 = TRUE;
 } else {
  L16 = ATTR(func,is_external);
 }
 if (L16) {
  L15 = TRUE;
 } else {
  is_iter_self = func;
  is_iter_self1 = ATTR(is_iter_self,sig1);
  is_iter_self2 = ATTR(is_iter_self1,name1);
  L18 = (is_iter_self2.str==((STR) NULL));
  L191_=!(L18); 
  if (L191_) {
   L20 = is_iter_self2.str;
   L22 = STR_sizerINT(is_iter_self2.str);
   L231_=INTMINUS(L22,1); 
   L24 = L231_;
   L251_=ARR((STR)L20,L24); 
   L26 = L251_;
   L271_=L26=='!'; 
   L17 = L271_;
  } else {
   L17 = FALSE;
  }
  ret_val2 = L17;
  ret_val1 = ret_val2;
  ret_val = ret_val1;
  L15 = ret_val;
 }
 if (L15) {
  L14 = TRUE;
 } else {
  is_forked_self = ATTR(func,sig1);
  if (ATTR(is_forked_self,is_par_routine)) {
   L29 = TRUE;
  } else {
   L29 = ATTR(is_forked_self,is_fork_routine);
  }
  if (L29) {
   L28 = TRUE;
  } else {
   L28 = ATTR(is_forked_self,is_attach_routin);
  }
  ret_val3 = L28;
  L14 = ret_val3;
 }
 if (L14) {
  return;
 }
 if (SIG_is418491101(ATTR(func,sig1))) {
  prog_self = self;
  ret_val4 = OPT_CO233462019;
  L30=ret_val4;
  if (ATTR(L30,opt_debug)) {
   create_self = ((OUT) NULL);
   ret_val5 = create_self;
   plus_self = ret_val5;
   plus_s = ((STR) &RESULT1262569080);
   stdout_self = ((FILE1) NULL);
   L33=ZALLOC(sizeof(struct FILE1_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   ((OB)L33)->header.tag=FILE1_tag;
   L32 = ((FILE1) L33);
   r = L32;
   
   SATTR(r,fp,stdout);
   ret_val7 = r;
   FILE_plus_STR(ret_val7, plus_s);
   ret_val6 = plus_self;
   plus_self1 = ret_val6;
   plus_s1 = ATTR(ATTR(func,sig1),str);
   stdout_self1 = ((FILE1) NULL);
   L36=ZALLOC(sizeof(struct FILE1_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   ((OB)L36)->header.tag=FILE1_tag;
   L35 = ((FILE1) L36);
   r1 = L35;
   
   SATTR(r1,fp,stdout);
   ret_val9 = r1;
   FILE_plus_STR(ret_val9, plus_s1);
   ret_val8 = plus_self1;
   plus_self2 = ret_val8;
   plus_s2 = ((STR) &name11);
   stdout_self2 = ((FILE1) NULL);
   L39=ZALLOC(sizeof(struct FILE1_struct));
   if (L39==NULL) FATAL("Unable to allocate more memory");
   ((OB)L39)->header.tag=FILE1_tag;
   L38 = ((FILE1) L39);
   r2 = L38;
   
   SATTR(r2,fp,stdout);
   ret_val10 = r2;
   FILE_plus_STR(ret_val10, plus_s2);
   create_self1 = ((OUT) NULL);
   ret_val11 = create_self1;
   plus_self3 = ret_val11;
   plus_s3 = ((STR) &Functionis_near);
   stdout_self3 = ((FILE1) NULL);
   L43=ZALLOC(sizeof(struct FILE1_struct));
   if (L43==NULL) FATAL("Unable to allocate more memory");
   ((OB)L43)->header.tag=FILE1_tag;
   L42 = ((FILE1) L43);
   r3 = L42;
   
   SATTR(r3,fp,stdout);
   ret_val13 = r3;
   FILE_plus_STR(ret_val13, plus_s3);
   ret_val12 = plus_self3;
   plus_self4 = ret_val12;
   is_near_self = ATTR(func,sig1);
   if (SIG_is418491101(is_near_self)) {
    L47 = ATTR(ATTR(is_near_self,builtin_info),is_near);
   } else {
    L47 = FALSE;
   }
   if (L47) {
    L46 = TRUE;
   } else {
    L48 = ATTR(is_near_self,ret);
    L46 = (*dTP_is1334578382[TAG(L48)])(L48);
   }
   if (L46) {
    L45 = TRUE;
   } else {
    L50 = (ATTR(is_near_self,opt_info)==((OPT_LOCAL_CALL) NULL));
    L521_=!(L50); 
    if (L521_) {
     L49 = ATTR(ATTR(is_near_self,opt_info),is_near);
    } else {
     L49 = FALSE;
    }
    L45 = L49;
   }
   ret_val14 = L45;
   L54=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
   if (L54==NULL) FATAL("Unable to allocate more memory");
   memset(L54,0,sizeof(struct BOOL_boxed_struct));
   ((OB)L54)->header.tag=BOOL_tag;
   L53 = (dSTR)((BOOL_boxed) L54);
   ((BOOL_boxed) L53)->immutable_part = ret_val14;
   plus_s4 = L53;
   stdout_self4 = ((FILE1) NULL);
   L56=ZALLOC(sizeof(struct FILE1_struct));
   if (L56==NULL) FATAL("Unable to allocate more memory");
   ((OB)L56)->header.tag=FILE1_tag;
   L55 = ((FILE1) L56);
   r4 = L55;
   
   SATTR(r4,fp,stdout);
   ret_val16 = r4;
   FILE_plus_dSTR(ret_val16, plus_s4);
   ret_val15 = plus_self4;
   plus_self5 = ret_val15;
   plus_s5 = ((STR) &args3);
   stdout_self5 = ((FILE1) NULL);
   L59=ZALLOC(sizeof(struct FILE1_struct));
   if (L59==NULL) FATAL("Unable to allocate more memory");
   ((OB)L59)->header.tag=FILE1_tag;
   L58 = ((FILE1) L59);
   r5 = L58;
   
   SATTR(r5,fp,stdout);
   ret_val18 = r5;
   FILE_plus_STR(ret_val18, plus_s5);
   ret_val17 = plus_self5;
   plus_self6 = ret_val17;
   plus_s6 = ARRAYINT_strrSTR(SIG_ne271652128(ATTR(func,sig1)));
   stdout_self6 = ((FILE1) NULL);
   L63=ZALLOC(sizeof(struct FILE1_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   ((OB)L63)->header.tag=FILE1_tag;
   L62 = ((FILE1) L63);
   r6 = L62;
   
   SATTR(r6,fp,stdout);
   ret_val20 = r6;
   FILE_plus_STR(ret_val20, plus_s6);
   ret_val19 = plus_self6;
   plus_self7 = ret_val19;
   plus_s7 = ((STR) &access2);
   stdout_self7 = ((FILE1) NULL);
   L66=ZALLOC(sizeof(struct FILE1_struct));
   if (L66==NULL) FATAL("Unable to allocate more memory");
   ((OB)L66)->header.tag=FILE1_tag;
   L65 = ((FILE1) L66);
   r7 = L65;
   
   SATTR(r7,fp,stdout);
   ret_val22 = r7;
   FILE_plus_STR(ret_val22, plus_s7);
   ret_val21 = plus_self7;
   plus_self8 = ret_val21;
   plus_s8 = ARRAYINT_strrSTR(SIG_at89227380(ATTR(func,sig1)));
   stdout_self8 = ((FILE1) NULL);
   L69=ZALLOC(sizeof(struct FILE1_struct));
   if (L69==NULL) FATAL("Unable to allocate more memory");
   ((OB)L69)->header.tag=FILE1_tag;
   L68 = ((FILE1) L69);
   r8 = L68;
   
   SATTR(r8,fp,stdout);
   ret_val24 = r8;
   FILE_plus_STR(ret_val24, plus_s8);
   ret_val23 = plus_self8;
   plus_self9 = ret_val23;
   plus_s9 = ((STR) &name9);
   stdout_self9 = ((FILE1) NULL);
   L73=ZALLOC(sizeof(struct FILE1_struct));
   if (L73==NULL) FATAL("Unable to allocate more memory");
   ((OB)L73)->header.tag=FILE1_tag;
   L72 = ((FILE1) L73);
   r9 = L72;
   
   SATTR(r9,fp,stdout);
   ret_val25 = r9;
   FILE_plus_STR(ret_val25, plus_s9);
  }
  return;
 }
 create_self2 = ((ARRAYO570594127) NULL);
 L75 = FLISTA1151473122(ATTR(func,locals1));
 L761_=INTPLUS(1,L75); 
 L77 = L761_;
 L781_=ASIZE((AM_ROUT_DEF)func); 
 L79 = L781_;
 L801_=INTPLUS(L77,L79); 
 create_n = L801_;
 L83 = create_n;
 L85=(sizeof(struct ARRAYO570594127_struct)-sizeof(OPT_LOCAL_EXPR))+(L83)*sizeof(OPT_LOCAL_EXPR);
 L84=ZALLOC_BIG(L85);
 if (L84==NULL) FATAL("Unable to allocate more memory");
 ((OB)L84)->header.tag=ARRAYO570594127_tag;
#ifdef DESTROY_CHK

   ((OB)L84)->header.destroyed=0;
#endif

 L82 = ((ARRAYO570594127) L84);
 L82->asize = L83;
 ret_val26 = L82;
 ox_table = ret_val26;
 create_self3 = ((OPT_LOCAL_EXPR) NULL);
 create_am = ((AM_LOCAL_EXPR) NULL);
 L87=ZALLOC(sizeof(struct OPT_LOCAL_EXPR_struct));
 if (L87==NULL) FATAL("Unable to allocate more memory");
 ((OB)L87)->header.tag=OPT_LOCAL_EXPR_tag;
 L86 = ((OPT_LOCAL_EXPR) L87);
 r10 = L86;
 SATTR(r10,local1,create_am);
 ret_val27 = r10;
 ret_ox = ret_val27;
 SATTR(ret_ox,index1,0);
 SARR((ARRAYO570594127)ox_table,0,(OPT_LOCAL_EXPR)ret_ox); 
 ;
 L89 = &m;
 L90 = OPT_LO1727904116(self, func, L89, ox_table);
 top_index = L90;
 L931_=(top_index)==(0); 
 if (L931_) {
  L92 = (ATTR(ATTR(func,sig1),tp)==((dTP) NULL));
 } else {
  L92 = FALSE;
 }
 if (L92) {
  prog_self1 = self;
  ret_val28 = OPT_CO233462019;
  L94=ret_val28;
  if (ATTR(L94,opt_debug)) {
   create_self4 = ((OUT) NULL);
   ret_val29 = create_self4;
   plus_self10 = ret_val29;
   plus_s10 = ((STR) &OPT_LO1160315301);
   stdout_self10 = ((FILE1) NULL);
   L96=ZALLOC(sizeof(struct FILE1_struct));
   if (L96==NULL) FATAL("Unable to allocate more memory");
   ((OB)L96)->header.tag=FILE1_tag;
   L95 = ((FILE1) L96);
   r11 = L95;
   
   SATTR(r11,fp,stdout);
   ret_val30 = r11;
   FILE_plus_STR(ret_val30, plus_s10);
  }
  return;
 }
 create_self5 = ((ARRAY2BOOL) NULL);
 L981_=INTPLUS(top_index,1); 
 create_d1 = L981_;
 L991_=INTPLUS(top_index,1); 
 create_d2 = L991_;
 L1031_=INTTIMES(create_d1,create_d2); 
 L102 = L1031_;
 L105=(sizeof(struct ARRAY2BOOL_struct)+1-sizeof(BOOL))+(L102)*sizeof(BOOL);
 L104=ZALLOC_LEAF_BIG(L105);
 if (L104==NULL) FATAL("Unable to allocate more memory");
 memset(L104,0,L105);
 ((OB)L104)->header.tag=ARRAY2BOOL_tag;
#ifdef DESTROY_CHK

   ((OB)L104)->header.destroyed=0;
#endif

 L100 = ((ARRAY2BOOL) L104);
 L100->asize = L102;
 res = L100;
 SATTR(res,size1,create_d1);
 SATTR(res,size2,create_d2);
 ret_val31 = res;
 dep = ret_val31;
 OPT_LO238575330(self, func, dep, m, ret_ox);
 prog_self2 = self;
 ret_val32 = OPT_CO233462019;
 L106=ret_val32;
 if (ATTR(L106,opt_debug)) {
  create_self6 = ((OUT) NULL);
  ret_val33 = create_self6;
  plus_self11 = ret_val33;
  plus_s11 = ((STR) &depend1236761341);
  stdout_self11 = ((FILE1) NULL);
  L108=ZALLOC(sizeof(struct FILE1_struct));
  if (L108==NULL) FATAL("Unable to allocate more memory");
  ((OB)L108)->header.tag=FILE1_tag;
  L107 = ((FILE1) L108);
  r12 = L107;
  
  SATTR(r12,fp,stdout);
  ret_val34 = r12;
  FILE_plus_STR(ret_val34, plus_s11);
  {
   BOOL f_L1101_ = TRUE;
   /* loop index variable */
   L71 = 0;
   L11 = ATTR(dep,size1);
   while (1) {
    if(L71>=L11)  goto after_loop; 
    aL1101_=L71; 
    i = aL1101_;
    L1121_=(i)<(10); 
    if (L1121_) {
     create_self7 = ((OUT) NULL);
     ret_val35 = create_self7;
     plus_self12 = ret_val35;
     plus_s12 = ((STR) &name10);
     stdout_self12 = ((FILE1) NULL);
     L114=ZALLOC(sizeof(struct FILE1_struct));
     if (L114==NULL) FATAL("Unable to allocate more memory");
     ((OB)L114)->header.tag=FILE1_tag;
     L113 = ((FILE1) L114);
     r13 = L113;
     
     SATTR(r13,fp,stdout);
     ret_val36 = r13;
     FILE_plus_STR(ret_val36, plus_s12);
    }
    create_self8 = ((OUT) NULL);
    ret_val37 = create_self8;
    plus_self13 = ret_val37;
    L117=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L117==NULL) FATAL("Unable to allocate more memory");
    memset(L117,0,sizeof(struct INT_boxed_struct));
    ((OB)L117)->header.tag=INT_tag;
    L116 = (dSTR)((INT_boxed) L117);
    ((INT_boxed) L116)->immutable_part = i;
    plus_s13 = L116;
    stdout_self13 = ((FILE1) NULL);
    L119=ZALLOC(sizeof(struct FILE1_struct));
    if (L119==NULL) FATAL("Unable to allocate more memory");
    ((OB)L119)->header.tag=FILE1_tag;
    L118 = ((FILE1) L119);
    r14 = L118;
    
    SATTR(r14,fp,stdout);
    ret_val39 = r14;
    FILE_plus_dSTR(ret_val39, plus_s13);
    ret_val38 = plus_self13;
    plus_self14 = ret_val38;
    plus_s14 = ((STR) &name68);
    stdout_self14 = ((FILE1) NULL);
    L123=ZALLOC(sizeof(struct FILE1_struct));
    if (L123==NULL) FATAL("Unable to allocate more memory");
    ((OB)L123)->header.tag=FILE1_tag;
    L122 = ((FILE1) L123);
    r15 = L122;
    
    SATTR(r15,fp,stdout);
    ret_val40 = r15;
    FILE_plus_STR(ret_val40, plus_s14);
    L1251_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,i); 
    L126=L1251_;
    AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(L126,local1)));
    create_self9 = ((OUT) NULL);
    ret_val41 = create_self9;
    plus_self15 = ret_val41;
    plus_s15 = ((STR) &name9);
    stdout_self15 = ((FILE1) NULL);
    L128=ZALLOC(sizeof(struct FILE1_struct));
    if (L128==NULL) FATAL("Unable to allocate more memory");
    ((OB)L128)->header.tag=FILE1_tag;
    L127 = ((FILE1) L128);
    r16 = L127;
    
    SATTR(r16,fp,stdout);
    ret_val42 = r16;
    FILE_plus_STR(ret_val42, plus_s15);
    L1301_=INTPLUS(L71,1); 
    L71 = L1301_;
   }
  }
  after_loop: ;
  create_self10 = ((OUT) NULL);
  ret_val43 = create_self10;
  plus_self16 = ret_val43;
  plus_s16 = ((STR) &name69);
  stdout_self16 = ((FILE1) NULL);
  L133=ZALLOC(sizeof(struct FILE1_struct));
  if (L133==NULL) FATAL("Unable to allocate more memory");
  ((OB)L133)->header.tag=FILE1_tag;
  L132 = ((FILE1) L133);
  r17 = L132;
  
  SATTR(r17,fp,stdout);
  ret_val44 = r17;
  FILE_plus_STR(ret_val44, plus_s16);
  {
   BOOL f_L1351_ = TRUE;
   /* loop index variable */
   L81 = 0;
   L21 = ATTR(dep,size1);
   while (1) {
    if(L81>=L21)  goto after_loop1; 
    aL1351_=L81; 
    i1 = aL1351_;
    L1371_=(i1)<(10); 
    if (L1371_) {
     create_self11 = ((OUT) NULL);
     ret_val45 = create_self11;
     plus_self17 = ret_val45;
     plus_s17 = ((STR) &name10);
     stdout_self17 = ((FILE1) NULL);
     L139=ZALLOC(sizeof(struct FILE1_struct));
     if (L139==NULL) FATAL("Unable to allocate more memory");
     ((OB)L139)->header.tag=FILE1_tag;
     L138 = ((FILE1) L139);
     r18 = L138;
     
     SATTR(r18,fp,stdout);
     ret_val46 = r18;
     FILE_plus_STR(ret_val46, plus_s17);
    }
    create_self12 = ((OUT) NULL);
    ret_val47 = create_self12;
    plus_self18 = ret_val47;
    L142=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L142==NULL) FATAL("Unable to allocate more memory");
    memset(L142,0,sizeof(struct INT_boxed_struct));
    ((OB)L142)->header.tag=INT_tag;
    L141 = (dSTR)((INT_boxed) L142);
    ((INT_boxed) L141)->immutable_part = i1;
    plus_s18 = L141;
    stdout_self18 = ((FILE1) NULL);
    L144=ZALLOC(sizeof(struct FILE1_struct));
    if (L144==NULL) FATAL("Unable to allocate more memory");
    ((OB)L144)->header.tag=FILE1_tag;
    L143 = ((FILE1) L144);
    r19 = L143;
    
    SATTR(r19,fp,stdout);
    ret_val49 = r19;
    FILE_plus_dSTR(ret_val49, plus_s18);
    ret_val48 = plus_self18;
    plus_self19 = ret_val48;
    plus_s19 = ((STR) &name10);
    stdout_self19 = ((FILE1) NULL);
    L147=ZALLOC(sizeof(struct FILE1_struct));
    if (L147==NULL) FATAL("Unable to allocate more memory");
    ((OB)L147)->header.tag=FILE1_tag;
    L146 = ((FILE1) L147);
    r20 = L146;
    
    SATTR(r20,fp,stdout);
    ret_val50 = r20;
    FILE_plus_STR(ret_val50, plus_s19);
    L1491_=INTPLUS(L81,1); 
    L81 = L1491_;
   }
  }
  after_loop1: ;
  create_self13 = ((OUT) NULL);
  ret_val51 = create_self13;
  plus_self20 = ret_val51;
  plus_s20 = ((STR) &name9);
  stdout_self20 = ((FILE1) NULL);
  L151=ZALLOC(sizeof(struct FILE1_struct));
  if (L151==NULL) FATAL("Unable to allocate more memory");
  ((OB)L151)->header.tag=FILE1_tag;
  L150 = ((FILE1) L151);
  r21 = L150;
  
  SATTR(r21,fp,stdout);
  ret_val52 = r21;
  FILE_plus_STR(ret_val52, plus_s20);
  {
   BOOL f_L1531_ = TRUE;
   /* loop index variable */
   L91 = 0;
   L31 = ATTR(dep,size1);
   while (1) {
    if(L91>=L31)  goto after_loop2; 
    aL1531_=L91; 
    i2 = aL1531_;
    L1551_=(i2)<(10); 
    if (L1551_) {
     create_self14 = ((OUT) NULL);
     ret_val53 = create_self14;
     plus_self21 = ret_val53;
     plus_s21 = ((STR) &name10);
     stdout_self21 = ((FILE1) NULL);
     L157=ZALLOC(sizeof(struct FILE1_struct));
     if (L157==NULL) FATAL("Unable to allocate more memory");
     ((OB)L157)->header.tag=FILE1_tag;
     L156 = ((FILE1) L157);
     r22 = L156;
     
     SATTR(r22,fp,stdout);
     ret_val54 = r22;
     FILE_plus_STR(ret_val54, plus_s21);
    }
    create_self15 = ((OUT) NULL);
    ret_val55 = create_self15;
    plus_self22 = ret_val55;
    L160=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L160==NULL) FATAL("Unable to allocate more memory");
    memset(L160,0,sizeof(struct INT_boxed_struct));
    ((OB)L160)->header.tag=INT_tag;
    L159 = (dSTR)((INT_boxed) L160);
    ((INT_boxed) L159)->immutable_part = i2;
    plus_s22 = L159;
    stdout_self22 = ((FILE1) NULL);
    L162=ZALLOC(sizeof(struct FILE1_struct));
    if (L162==NULL) FATAL("Unable to allocate more memory");
    ((OB)L162)->header.tag=FILE1_tag;
    L161 = ((FILE1) L162);
    r23 = L161;
    
    SATTR(r23,fp,stdout);
    ret_val57 = r23;
    FILE_plus_dSTR(ret_val57, plus_s22);
    ret_val56 = plus_self22;
    plus_self23 = ret_val56;
    plus_s23 = ((STR) &name68);
    stdout_self23 = ((FILE1) NULL);
    L165=ZALLOC(sizeof(struct FILE1_struct));
    if (L165==NULL) FATAL("Unable to allocate more memory");
    ((OB)L165)->header.tag=FILE1_tag;
    L164 = ((FILE1) L165);
    r24 = L164;
    
    SATTR(r24,fp,stdout);
    ret_val58 = r24;
    FILE_plus_STR(ret_val58, plus_s23);
    {
     BOOL f_L1671_ = TRUE;
     /* loop index variable */
     L101 = 0;
     L41 = ATTR(dep,size1);
     while (1) {
      if(L101>=L41)  goto after_loop3; 
      aL1671_=L101; 
      j = aL1671_;
      aget_self = dep;
      aget_i1 = i2;
      aget_i2 = j;
      L169 = ATTR(aget_self,size2);
      L1701_=INTTIMES(aget_i1,L169); 
      L171 = L1701_;
      L1721_=INTPLUS(L171,aget_i2); 
      L173 = L1721_;
      L1741_=ARR((ARRAY2BOOL)aget_self,L173); 
      ret_val59 = L1741_;
      if (ret_val59) {
       create_self16 = ((OUT) NULL);
       ret_val60 = create_self16;
       plus_self24 = ret_val60;
       plus_s24 = ((STR) &name70);
       stdout_self24 = ((FILE1) NULL);
       L176=ZALLOC(sizeof(struct FILE1_struct));
       if (L176==NULL) FATAL("Unable to allocate more memory");
       ((OB)L176)->header.tag=FILE1_tag;
       L175 = ((FILE1) L176);
       r25 = L175;
       
       SATTR(r25,fp,stdout);
       ret_val61 = r25;
       FILE_plus_STR(ret_val61, plus_s24);
      }
      else {
       create_self17 = ((OUT) NULL);
       ret_val62 = create_self17;
       plus_self25 = ret_val62;
       plus_s25 = ((STR) &name8);
       stdout_self25 = ((FILE1) NULL);
       L179=ZALLOC(sizeof(struct FILE1_struct));
       if (L179==NULL) FATAL("Unable to allocate more memory");
       ((OB)L179)->header.tag=FILE1_tag;
       L178 = ((FILE1) L179);
       r26 = L178;
       
       SATTR(r26,fp,stdout);
       ret_val63 = r26;
       FILE_plus_STR(ret_val63, plus_s25);
      }
      L1811_=INTPLUS(L101,1); 
      L101 = L1811_;
     }
    }
    after_loop3: ;
    create_self18 = ((OUT) NULL);
    ret_val64 = create_self18;
    plus_self26 = ret_val64;
    plus_s26 = ((STR) &name9);
    stdout_self26 = ((FILE1) NULL);
    L183=ZALLOC(sizeof(struct FILE1_struct));
    if (L183==NULL) FATAL("Unable to allocate more memory");
    ((OB)L183)->header.tag=FILE1_tag;
    L182 = ((FILE1) L183);
    r27 = L182;
    
    SATTR(r27,fp,stdout);
    ret_val65 = r27;
    FILE_plus_STR(ret_val65, plus_s26);
    L1851_=INTPLUS(L91,1); 
    L91 = L1851_;
   }
  }
  after_loop2: ;
 }
 OPT_LO1196828642(self, dep);
 prog_self3 = self;
 ret_val66 = OPT_CO233462019;
 L186=ret_val66;
 if (ATTR(L186,opt_debug)) {
  create_self19 = ((OUT) NULL);
  ret_val67 = create_self19;
  plus_self27 = ret_val67;
  plus_s27 = ((STR) &AFTERflattening);
  stdout_self27 = ((FILE1) NULL);
  L188=ZALLOC(sizeof(struct FILE1_struct));
  if (L188==NULL) FATAL("Unable to allocate more memory");
  ((OB)L188)->header.tag=FILE1_tag;
  L187 = ((FILE1) L188);
  r28 = L187;
  
  SATTR(r28,fp,stdout);
  ret_val68 = r28;
  FILE_plus_STR(ret_val68, plus_s27);
  {
   BOOL f_L1901_ = TRUE;
   /* loop index variable */
   L121 = 0;
   L51 = ATTR(dep,size1);
   while (1) {
    if(L121>=L51)  goto after_loop4; 
    aL1901_=L121; 
    i3 = aL1901_;
    L1931_=(i3)<(10); 
    if (L1931_) {
     create_self20 = ((OUT) NULL);
     ret_val69 = create_self20;
     plus_self28 = ret_val69;
     plus_s28 = ((STR) &name10);
     stdout_self28 = ((FILE1) NULL);
     L195=ZALLOC(sizeof(struct FILE1_struct));
     if (L195==NULL) FATAL("Unable to allocate more memory");
     ((OB)L195)->header.tag=FILE1_tag;
     L194 = ((FILE1) L195);
     r29 = L194;
     
     SATTR(r29,fp,stdout);
     ret_val70 = r29;
     FILE_plus_STR(ret_val70, plus_s28);
    }
    create_self21 = ((OUT) NULL);
    ret_val71 = create_self21;
    plus_self29 = ret_val71;
    L198=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L198==NULL) FATAL("Unable to allocate more memory");
    memset(L198,0,sizeof(struct INT_boxed_struct));
    ((OB)L198)->header.tag=INT_tag;
    L197 = (dSTR)((INT_boxed) L198);
    ((INT_boxed) L197)->immutable_part = i3;
    plus_s29 = L197;
    stdout_self29 = ((FILE1) NULL);
    L200=ZALLOC(sizeof(struct FILE1_struct));
    if (L200==NULL) FATAL("Unable to allocate more memory");
    ((OB)L200)->header.tag=FILE1_tag;
    L199 = ((FILE1) L200);
    r30 = L199;
    
    SATTR(r30,fp,stdout);
    ret_val73 = r30;
    FILE_plus_dSTR(ret_val73, plus_s29);
    ret_val72 = plus_self29;
    plus_self30 = ret_val72;
    plus_s30 = ((STR) &name68);
    stdout_self30 = ((FILE1) NULL);
    L203=ZALLOC(sizeof(struct FILE1_struct));
    if (L203==NULL) FATAL("Unable to allocate more memory");
    ((OB)L203)->header.tag=FILE1_tag;
    L202 = ((FILE1) L203);
    r31 = L202;
    
    SATTR(r31,fp,stdout);
    ret_val74 = r31;
    FILE_plus_STR(ret_val74, plus_s30);
    {
     BOOL f_L2051_ = TRUE;
     /* loop index variable */
     L131 = 0;
     L61 = ATTR(dep,size1);
     while (1) {
      if(L131>=L61)  goto after_loop5; 
      aL2051_=L131; 
      j11 = aL2051_;
      aget_self1 = dep;
      aget_i11 = i3;
      aget_i21 = j11;
      L207 = ATTR(aget_self1,size2);
      L2081_=INTTIMES(aget_i11,L207); 
      L209 = L2081_;
      L2101_=INTPLUS(L209,aget_i21); 
      L211 = L2101_;
      L2121_=ARR((ARRAY2BOOL)aget_self1,L211); 
      ret_val75 = L2121_;
      if (ret_val75) {
       create_self22 = ((OUT) NULL);
       ret_val76 = create_self22;
       plus_self31 = ret_val76;
       plus_s31 = ((STR) &name70);
       stdout_self31 = ((FILE1) NULL);
       L214=ZALLOC(sizeof(struct FILE1_struct));
       if (L214==NULL) FATAL("Unable to allocate more memory");
       ((OB)L214)->header.tag=FILE1_tag;
       L213 = ((FILE1) L214);
       r32 = L213;
       
       SATTR(r32,fp,stdout);
       ret_val77 = r32;
       FILE_plus_STR(ret_val77, plus_s31);
      }
      else {
       create_self23 = ((OUT) NULL);
       ret_val78 = create_self23;
       plus_self32 = ret_val78;
       plus_s32 = ((STR) &name8);
       stdout_self32 = ((FILE1) NULL);
       L217=ZALLOC(sizeof(struct FILE1_struct));
       if (L217==NULL) FATAL("Unable to allocate more memory");
       ((OB)L217)->header.tag=FILE1_tag;
       L216 = ((FILE1) L217);
       r33 = L216;
       
       SATTR(r33,fp,stdout);
       ret_val79 = r33;
       FILE_plus_STR(ret_val79, plus_s32);
      }
      L2191_=INTPLUS(L131,1); 
      L131 = L2191_;
     }
    }
    after_loop5: ;
    create_self24 = ((OUT) NULL);
    ret_val80 = create_self24;
    plus_self33 = ret_val80;
    plus_s33 = ((STR) &name9);
    stdout_self33 = ((FILE1) NULL);
    L221=ZALLOC(sizeof(struct FILE1_struct));
    if (L221==NULL) FATAL("Unable to allocate more memory");
    ((OB)L221)->header.tag=FILE1_tag;
    L220 = ((FILE1) L221);
    r34 = L220;
    
    SATTR(r34,fp,stdout);
    ret_val81 = r34;
    FILE_plus_STR(ret_val81, plus_s33);
    L2231_=INTPLUS(L121,1); 
    L121 = L2231_;
   }
  }
  after_loop4: ;
 }
 OPT_LO14752986(self, func, dep, m, ox_table, ret_ox);
 cs_options_self = self;
 ret_val82 = OPT_CO1299251581;
 L224=ret_val82;
 if (ATTR(L224,local_call)) {
  OPT_LO447032529(self, func, dep, m, ox_table);
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO1196828642(OPT_LOCAL self, ARRAY2BOOL dep) {
 INT L11 = INT_zero;
 INT k = INT_zero;
 INT L21 = INT_zero;
 INT i = INT_zero;
 INT L31 = INT_zero;
 INT j = INT_zero;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 ARRAY2BOOL aget_self;
 INT aget_i1 = INT_zero;
 INT aget_i2 = INT_zero;
 BOOL ret_val = BOOL_zero;
 ARRAY2BOOL aget_self1;
 INT aget_i11 = INT_zero;
 INT aget_i21 = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 ARRAY2BOOL aset_self;
 INT aset_i1 = INT_zero;
 INT aset_i2 = INT_zero;
 BOOL aset_val = BOOL_zero;
 INT aL71_;
 INT L8;
 INT aL91_;
 INT L10;
 INT aL121_;
 INT L13;
 BOOL L14;
 INT L15;
 INT L161_;
 INT L17;
 INT L181_;
 INT L19;
 BOOL L201_;
 INT L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 BOOL L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L321_;
 INT L33;
 INT L351_;
 INT L361_;
 INT L371_;
 {
  BOOL f_L71_ = TRUE;
  /* loop index variable */
  L41 = 0;
  L11 = ATTR(dep,size1);
  while (1) {
   if(L41>=L11)  goto after_loop; 
   aL71_=L41; 
   k = aL71_;
   {
    BOOL f_L91_ = TRUE;
    /* loop index variable */
    L51 = 0;
    L21 = ATTR(dep,size1);
    while (1) {
     if(L51>=L21)  goto after_loop1; 
     aL91_=L51; 
     i = aL91_;
     {
      BOOL f_L121_ = TRUE;
      /* loop index variable */
      L61 = 0;
      L31 = ATTR(dep,size1);
      while (1) {
       if(L61>=L31)  goto after_loop2; 
       aL121_=L61; 
       j = aL121_;
       aget_self = dep;
       aget_i1 = k;
       aget_i2 = i;
       L15 = ATTR(aget_self,size2);
       L161_=INTTIMES(aget_i1,L15); 
       L17 = L161_;
       L181_=INTPLUS(L17,aget_i2); 
       L19 = L181_;
       L201_=ARR((ARRAY2BOOL)aget_self,L19); 
       ret_val = L201_;
       if (ret_val) {
        aget_self1 = dep;
        aget_i11 = j;
        aget_i21 = k;
        L22 = ATTR(aget_self1,size2);
        L231_=INTTIMES(aget_i11,L22); 
        L24 = L231_;
        L251_=INTPLUS(L24,aget_i21); 
        L26 = L251_;
        L271_=ARR((ARRAY2BOOL)aget_self1,L26); 
        ret_val1 = L271_;
        L14 = ret_val1;
       } else {
        L14 = FALSE;
       }
       if (L14) {
        aset_self = dep;
        aset_i1 = j;
        aset_i2 = i;
        aset_val = TRUE;
        L28 = ATTR(aset_self,size2);
        L291_=INTTIMES(aset_i1,L28); 
        L30 = L291_;
        L321_=INTPLUS(L30,aset_i2); 
        L33 = L321_;
        SARR((ARRAY2BOOL)aset_self,L33,aset_val); 
        ;
       }
       L351_=INTPLUS(L61,1); 
       L61 = L351_;
      }
     }
     after_loop2: ;
     L361_=INTPLUS(L51,1); 
     L51 = L361_;
    }
   }
   after_loop1: ;
   L371_=INTPLUS(L41,1); 
   L41 = L371_;
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO14752986(OPT_LOCAL self, AM_ROUT_DEF func, ARRAY2BOOL dep, FMAPAM418009930 m, ARRAYO570594127 ox_table, OPT_LOCAL_EXPR ret_ox) {
 FMAPAM418009930 L11;
 TUPAM_504653531 p = TUPAM_504653531_zero;
 AM_LOCAL_EXPR lc;
 OPT_LOCAL_EXPR ox;
 BOOL is_far = BOOL_zero;
 INT depends_on_arg = INT_zero;
 INT L21 = INT_zero;
 INT ld = INT_zero;
 AM_ROUT_DEF L31;
 OPT_LOCAL_EXPR ox2;
 ARRAYINT L41;
 INT i = INT_zero;
 AM_ROUT_DEF L51;
 AM_FORMAL_ARG f;
 OPT_LOCAL_EXPR ox1;
 BOOL is_far1 = BOOL_zero;
 INT depends_on_arg1 = INT_zero;
 INT L61 = INT_zero;
 INT ld1 = INT_zero;
 ARRAYINT a;
 AM_ROUT_DEF L71;
 INT i1 = INT_zero;
 AM_LOCAL_EXPR l;
 OPT_LOCAL_EXPR ox3;
 ARRAYINT L81;
 FMAPAM418009930 L91;
 TUPAM_504653531 t = TUPAM_504653531_zero;
 AM_LOCAL_EXPR lc1;
 OPT_LOCAL_EXPR ox4;
 OPT_LOCAL_CALL create_self;
 OPT_LOCAL_CALL ret_val;
 OPT_LOCAL prog_self;
 PROG ret_val1;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val3;
 FILE1 r;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 dSTR plus_s2;
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val9;
 FILE1 stdout_self3;
 FILE1 ret_val10;
 FILE1 r3;
 OUT plus_self4;
 dSTR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val12;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val13;
 FILE1 stdout_self5;
 FILE1 ret_val14;
 FILE1 r5;
 AM_LOCAL_EXPR is_near_self;
 BOOL ret_val15 = BOOL_zero;
 OUT plus_self6;
 dSTR plus_s6;
 OUT ret_val16;
 FILE1 stdout_self6;
 FILE1 ret_val17;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val18;
 FILE1 stdout_self7;
 FILE1 ret_val19;
 FILE1 r7;
 OUT plus_self8;
 dSTR plus_s8;
 OUT ret_val20;
 FILE1 stdout_self8;
 FILE1 ret_val21;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val22;
 FILE1 r9;
 ARRAY2BOOL aget_self;
 INT aget_i1 = INT_zero;
 INT aget_i2 = INT_zero;
 BOOL ret_val23 = BOOL_zero;
 OPT_LOCAL prog_self1;
 PROG ret_val24;
 OUT create_self4;
 OUT ret_val25;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r10;
 OUT create_self5;
 OUT ret_val27;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val28;
 FILE1 r11;
 INT L101 = INT_zero;
 ARRAY2BOOL aget_self1;
 INT aget_i11 = INT_zero;
 INT aget_i21 = INT_zero;
 BOOL ret_val29 = BOOL_zero;
 ARRAYINT create_self6;
 INT create_n = INT_zero;
 ARRAYINT ret_val30;
 SIG attr_access_self;
 ARRAYINT attr_access_a;
 INT L121 = INT_zero;
 SIG is_near_self1;
 BOOL is_near_a = BOOL_zero;
 SIG near_depends_on_;
 ARRAYINT near_depends_on_1;
 INT L131 = INT_zero;
 ARRAY2BOOL aget_self2;
 INT aget_i12 = INT_zero;
 INT aget_i22 = INT_zero;
 BOOL ret_val31 = BOOL_zero;
 SIG is_near_self2;
 BOOL is_near_a1 = BOOL_zero;
 ARRAYINT create_self7;
 INT create_n1 = INT_zero;
 ARRAYINT ret_val32;
 INT L141 = INT_zero;
 ARRAY2BOOL aget_self3;
 INT aget_i13 = INT_zero;
 INT aget_i23 = INT_zero;
 BOOL ret_val33 = BOOL_zero;
 SIG near_depends_on_2;
 ARRAYINT near_depends_on_3;
 OPT_LOCAL prog_self2;
 PROG ret_val34;
 OUT create_self8;
 OUT ret_val35;
 OUT plus_self12;
 STR plus_s12;
 OUT ret_val36;
 FILE1 stdout_self12;
 FILE1 ret_val37;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val38;
 FILE1 stdout_self13;
 FILE1 ret_val39;
 FILE1 r13;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val40;
 FILE1 r14;
 OUT create_self9;
 OUT ret_val41;
 OUT plus_self15;
 STR plus_s15;
 OUT ret_val42;
 FILE1 stdout_self15;
 FILE1 ret_val43;
 FILE1 r15;
 SIG is_near_self3;
 BOOL ret_val44 = BOOL_zero;
 OUT plus_self16;
 dSTR plus_s16;
 OUT ret_val45;
 FILE1 stdout_self16;
 FILE1 ret_val46;
 FILE1 r16;
 OUT plus_self17;
 STR plus_s17;
 OUT ret_val47;
 FILE1 stdout_self17;
 FILE1 ret_val48;
 FILE1 r17;
 OUT plus_self18;
 STR plus_s18;
 FILE1 stdout_self18;
 FILE1 ret_val49;
 FILE1 r18;
 OUT create_self10;
 OUT ret_val50;
 OUT plus_self19;
 STR plus_s19;
 OUT ret_val51;
 FILE1 stdout_self19;
 FILE1 ret_val52;
 FILE1 r19;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val53;
 FILE1 stdout_self20;
 FILE1 ret_val54;
 FILE1 r20;
 OUT plus_self21;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val55;
 FILE1 r21;
 OUT create_self11;
 OUT ret_val56;
 OUT plus_self22;
 STR plus_s22;
 FILE1 stdout_self22;
 FILE1 ret_val57;
 FILE1 r22;
 AM_LOCAL_EXPR is_near_self4;
 BOOL ret_val58 = BOOL_zero;
 OUT create_self12;
 OUT ret_val59;
 OUT plus_self23;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val60;
 FILE1 r23;
 OUT create_self13;
 OUT ret_val61;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val62;
 FILE1 r24;
 OPT_LOCAL_CALL L15;
 OB L16;
 TUPAM_504653531 L17;
 PROG L18;
 extern STR workingon;
 FILE1 L19;
 OB L20;
 extern STR far3;
 FILE1 L23;
 OB L24;
 dSTR L26;
 OB L27;
 FILE1 L28;
 OB L29;
 extern STR attr_acces;
 FILE1 L32;
 OB L33;
 dSTR L35;
 OB L36;
 FILE1 L37;
 OB L38;
 extern STR lcis_near;
 FILE1 L40;
 OB L42;
 BOOL L44;
 dTP L45;
 dSTR L46;
 OB L47;
 FILE1 L48;
 OB L49;
 extern STR always1;
 FILE1 L52;
 OB L53;
 dSTR L55;
 OB L56;
 FILE1 L57;
 OB L58;
 extern STR name9;
 FILE1 L60;
 OB L62;
 INT L65;
INT i_L641_=0;
 INT aL641_;
 INT L66;
 INT L671_;
 INT L68;
 INT L691_;
 INT L70;
 BOOL L721_;
 OPT_LOCAL_EXPR L731_;
 OPT_LOCAL_EXPR L74;
 PROG L75;
 extern STR is_farbecauseof;
 FILE1 L76;
 OB L77;
 OPT_LOCAL_EXPR L791_;
 OPT_LOCAL_EXPR L80;
 extern STR name9;
 FILE1 L82;
 OB L83;
 OPT_LOCAL_EXPR L851_;
 OPT_LOCAL_EXPR L86;
 INT L871_;
 BOOL L881_;
 BOOL L891_;
 BOOL L90;
 BOOL L921_;
 INT L931_br;
 AM_FORMAL_ARG aL931_;
 AM_FORMAL_ARG L94;
 AM_FORMAL_ARG L95;
 BOOL L96;
 BOOL L97;
 BOOL L981_;
 INT L99;
 INT L1001_;
 INT L102;
 INT L1031_;
 INT L104;
 BOOL L1051_;
 INT L106;
 INT L107;
 INT L1081_;
 INT L109;
 INT L1101_;
 INT L1111_;
 INT L1121_;
 ARRAYINT L113;
 INT L114;
 OB L115;
 INT L116;
 INT L1171_br;
 INT rL1171_;
 INT L1181_br;
 AM_FORMAL_ARG aL1181_;
 INT L119;
 AM_FORMAL_ARG L120;
 BOOL L122;
 BOOL L1231_;
 BOOL L124;
 BOOL L1251_;
 ARRAYINT L126;
 INT L127;
 INT L1291_;
 BOOL L130;
 BOOL L1321_;
 INT aL1331_;
 INT L134;
 INT L135;
 INT L1361_;
 INT L137;
 INT L1381_;
 INT L139;
 BOOL L1401_;
 OPT_LOCAL_EXPR L1421_;
 OPT_LOCAL_EXPR L143;
 OPT_LOCAL_EXPR L1441_;
 OPT_LOCAL_EXPR L145;
 INT L1461_;
 INT L1471_;
 BOOL L1481_;
 BOOL L1491_;
 ARRAYINT L150;
 INT L151;
 OB L152;
 INT L153;
 INT L1541_br;
 INT rL1541_;
 INT L1551_br;
INT i_L1551_=0;
 INT L156;
 AM_FORMAL_ARG L1571_;
 AM_FORMAL_ARG L158;
 BOOL L159;
 BOOL L160;
 BOOL L1611_;
 INT L162;
 INT L1631_;
 INT L164;
 INT L1651_;
 INT L166;
 BOOL L1671_;
 INT L1681_;
 PROG L169;
 extern STR RESULT1809061834;
 FILE1 L170;
 OB L171;
 FILE1 L173;
 OB L174;
 extern STR name11;
 FILE1 L176;
 OB L177;
 BOOL L179;
 BOOL L1801_;
 extern STR Functionis_near;
 FILE1 L181;
 OB L182;
 BOOL L184;
 BOOL L185;
 BOOL L186;
 dTP L187;
 BOOL L188;
 BOOL L189;
 BOOL L1901_;
 dSTR L191;
 OB L192;
 FILE1 L193;
 OB L194;
 extern STR args3;
 FILE1 L196;
 OB L197;
 FILE1 L199;
 OB L200;
 extern STR access2;
 FILE1 L202;
 OB L203;
 FILE1 L205;
 OB L206;
 extern STR name9;
 FILE1 L208;
 OB L209;
 extern STR NEARLO1748192338;
 FILE1 L211;
 OB L212;
 TUPAM_504653531 L214;
 BOOL L215;
 dTP L216;
 extern STR name66;
 FILE1 L217;
 OB L218;
 extern STR name9;
 FILE1 L220;
 OB L222;
 create_self = ((OPT_LOCAL_CALL) NULL);
 L16=ZALLOC(sizeof(struct OPT_LOCAL_CALL_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=OPT_LOCAL_CALL_tag;
 L15 = ((OPT_LOCAL_CALL) L16);
 ret_val = L15;
 SATTR(ATTR(func,sig1),opt_info,ret_val);
 {
  struct FMAPAM1518099927_frame_struct other1_0;
FMAPAM1518099927_frame noname1 = &other1_0;
  L11 = m;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L17 = FMAPAM1518099927(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   p = L17;
   lc = p.t1;
   ox = p.t2;
   is_far = ATTR(ox,is_sometimes_far);
   prog_self = self;
   ret_val1 = OPT_CO233462019;
   L18=ret_val1;
   if (ATTR(L18,opt_debug)) {
    create_self1 = ((OUT) NULL);
    ret_val2 = create_self1;
    plus_self = ret_val2;
    plus_s = ((STR) &workingon);
    stdout_self = ((FILE1) NULL);
    L20=ZALLOC(sizeof(struct FILE1_struct));
    if (L20==NULL) FATAL("Unable to allocate more memory");
    ((OB)L20)->header.tag=FILE1_tag;
    L19 = ((FILE1) L20);
    r = L19;
    
    SATTR(r,fp,stdout);
    ret_val3 = r;
    FILE_plus_STR(ret_val3, plus_s);
    AM_OUT1439666796(((AM_OUT) NULL), ((dAM) lc));
    create_self2 = ((OUT) NULL);
    ret_val4 = create_self2;
    plus_self1 = ret_val4;
    plus_s1 = ((STR) &far3);
    stdout_self1 = ((FILE1) NULL);
    L24=ZALLOC(sizeof(struct FILE1_struct));
    if (L24==NULL) FATAL("Unable to allocate more memory");
    ((OB)L24)->header.tag=FILE1_tag;
    L23 = ((FILE1) L24);
    r1 = L23;
    
    SATTR(r1,fp,stdout);
    ret_val6 = r1;
    FILE_plus_STR(ret_val6, plus_s1);
    ret_val5 = plus_self1;
    plus_self2 = ret_val5;
    L27=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
    if (L27==NULL) FATAL("Unable to allocate more memory");
    memset(L27,0,sizeof(struct BOOL_boxed_struct));
    ((OB)L27)->header.tag=BOOL_tag;
    L26 = (dSTR)((BOOL_boxed) L27);
    ((BOOL_boxed) L26)->immutable_part = ATTR(ox,is_sometimes_far);
    plus_s2 = L26;
    stdout_self2 = ((FILE1) NULL);
    L29=ZALLOC(sizeof(struct FILE1_struct));
    if (L29==NULL) FATAL("Unable to allocate more memory");
    ((OB)L29)->header.tag=FILE1_tag;
    L28 = ((FILE1) L29);
    r2 = L28;
    
    SATTR(r2,fp,stdout);
    ret_val8 = r2;
    FILE_plus_dSTR(ret_val8, plus_s2);
    ret_val7 = plus_self2;
    plus_self3 = ret_val7;
    plus_s3 = ((STR) &attr_acces);
    stdout_self3 = ((FILE1) NULL);
    L33=ZALLOC(sizeof(struct FILE1_struct));
    if (L33==NULL) FATAL("Unable to allocate more memory");
    ((OB)L33)->header.tag=FILE1_tag;
    L32 = ((FILE1) L33);
    r3 = L32;
    
    SATTR(r3,fp,stdout);
    ret_val10 = r3;
    FILE_plus_STR(ret_val10, plus_s3);
    ret_val9 = plus_self3;
    plus_self4 = ret_val9;
    L36=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    memset(L36,0,sizeof(struct INT_boxed_struct));
    ((OB)L36)->header.tag=INT_tag;
    L35 = (dSTR)((INT_boxed) L36);
    ((INT_boxed) L35)->immutable_part = ATTR(ox,attr_access);
    plus_s4 = L35;
    stdout_self4 = ((FILE1) NULL);
    L38=ZALLOC(sizeof(struct FILE1_struct));
    if (L38==NULL) FATAL("Unable to allocate more memory");
    ((OB)L38)->header.tag=FILE1_tag;
    L37 = ((FILE1) L38);
    r4 = L37;
    
    SATTR(r4,fp,stdout);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    create_self3 = ((OUT) NULL);
    ret_val12 = create_self3;
    plus_self5 = ret_val12;
    plus_s5 = ((STR) &lcis_near);
    stdout_self5 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L40 = ((FILE1) L42);
    r5 = L40;
    
    SATTR(r5,fp,stdout);
    ret_val14 = r5;
    FILE_plus_STR(ret_val14, plus_s5);
    ret_val13 = plus_self5;
    plus_self6 = ret_val13;
    is_near_self = lc;
    if (ATTR(is_near_self,is_always_near)) {
     L44 = TRUE;
    } else {
     L45 = ATTR(is_near_self,tp_at);
     L44 = (*dTP_is1334578382[TAG(L45)])(L45);
    }
    ret_val15 = L44;
    L47=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
    if (L47==NULL) FATAL("Unable to allocate more memory");
    memset(L47,0,sizeof(struct BOOL_boxed_struct));
    ((OB)L47)->header.tag=BOOL_tag;
    L46 = (dSTR)((BOOL_boxed) L47);
    ((BOOL_boxed) L46)->immutable_part = ret_val15;
    plus_s6 = L46;
    stdout_self6 = ((FILE1) NULL);
    L49=ZALLOC(sizeof(struct FILE1_struct));
    if (L49==NULL) FATAL("Unable to allocate more memory");
    ((OB)L49)->header.tag=FILE1_tag;
    L48 = ((FILE1) L49);
    r6 = L48;
    
    SATTR(r6,fp,stdout);
    ret_val17 = r6;
    FILE_plus_dSTR(ret_val17, plus_s6);
    ret_val16 = plus_self6;
    plus_self7 = ret_val16;
    plus_s7 = ((STR) &always1);
    stdout_self7 = ((FILE1) NULL);
    L53=ZALLOC(sizeof(struct FILE1_struct));
    if (L53==NULL) FATAL("Unable to allocate more memory");
    ((OB)L53)->header.tag=FILE1_tag;
    L52 = ((FILE1) L53);
    r7 = L52;
    
    SATTR(r7,fp,stdout);
    ret_val19 = r7;
    FILE_plus_STR(ret_val19, plus_s7);
    ret_val18 = plus_self7;
    plus_self8 = ret_val18;
    L56=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
    if (L56==NULL) FATAL("Unable to allocate more memory");
    memset(L56,0,sizeof(struct BOOL_boxed_struct));
    ((OB)L56)->header.tag=BOOL_tag;
    L55 = (dSTR)((BOOL_boxed) L56);
    ((BOOL_boxed) L55)->immutable_part = ATTR(lc,is_always_near);
    plus_s8 = L55;
    stdout_self8 = ((FILE1) NULL);
    L58=ZALLOC(sizeof(struct FILE1_struct));
    if (L58==NULL) FATAL("Unable to allocate more memory");
    ((OB)L58)->header.tag=FILE1_tag;
    L57 = ((FILE1) L58);
    r8 = L57;
    
    SATTR(r8,fp,stdout);
    ret_val21 = r8;
    FILE_plus_dSTR(ret_val21, plus_s8);
    ret_val20 = plus_self8;
    plus_self9 = ret_val20;
    plus_s9 = ((STR) &name9);
    stdout_self9 = ((FILE1) NULL);
    L62=ZALLOC(sizeof(struct FILE1_struct));
    if (L62==NULL) FATAL("Unable to allocate more memory");
    ((OB)L62)->header.tag=FILE1_tag;
    L60 = ((FILE1) L62);
    r9 = L60;
    
    SATTR(r9,fp,stdout);
    ret_val22 = r9;
    FILE_plus_STR(ret_val22, plus_s9);
   }
   depends_on_arg = 0;
   {
    BOOL f_L641_ = TRUE;
    while (1) {
     if (is_far) {
      goto after_loop1;
     }
     if (f_L641_) {
      f_L641_ = FALSE;
      L21 = ATTR(dep,size1);
      i_L641_=0;
     }
     if(i_L641_>=L21)  goto after_loop1; 
     aL641_=i_L641_; i_L641_++;
     ld = aL641_;
     aget_self = dep;
     aget_i1 = ATTR(ox,index1);
     aget_i2 = ld;
     L66 = ATTR(aget_self,size2);
     L671_=INTTIMES(aget_i1,L66); 
     L68 = L671_;
     L691_=INTPLUS(L68,aget_i2); 
     L70 = L691_;
     L721_=ARR((ARRAY2BOOL)aget_self,L70); 
     ret_val23 = L721_;
     if (ret_val23) {
      L731_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,ld); 
      L74=L731_;
      if (ATTR(L74,is_sometimes_far)) {
       prog_self1 = self;
       ret_val24 = OPT_CO233462019;
       L75=ret_val24;
       if (ATTR(L75,opt_debug)) {
        create_self4 = ((OUT) NULL);
        ret_val25 = create_self4;
        plus_self10 = ret_val25;
        plus_s10 = ((STR) &is_farbecauseof);
        stdout_self10 = ((FILE1) NULL);
        L77=ZALLOC(sizeof(struct FILE1_struct));
        if (L77==NULL) FATAL("Unable to allocate more memory");
        ((OB)L77)->header.tag=FILE1_tag;
        L76 = ((FILE1) L77);
        r10 = L76;
        
        SATTR(r10,fp,stdout);
        ret_val26 = r10;
        FILE_plus_STR(ret_val26, plus_s10);
        L791_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,ld); 
        L80=L791_;
        AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(L80,local1)));
        create_self5 = ((OUT) NULL);
        ret_val27 = create_self5;
        plus_self11 = ret_val27;
        plus_s11 = ((STR) &name9);
        stdout_self11 = ((FILE1) NULL);
        L83=ZALLOC(sizeof(struct FILE1_struct));
        if (L83==NULL) FATAL("Unable to allocate more memory");
        ((OB)L83)->header.tag=FILE1_tag;
        L82 = ((FILE1) L83);
        r11 = L82;
        
        SATTR(r11,fp,stdout);
        ret_val28 = r11;
        FILE_plus_STR(ret_val28, plus_s11);
       }
       is_far = TRUE;
      }
      L851_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,ld); 
      L86=L851_;
      if (ATTR(L86,is_arg)) {
       L871_=INTPLUS(depends_on_arg,1); 
       depends_on_arg = L871_;
      }
     }
    }
   }
   after_loop1: ;
   L881_=!(is_far); 
   if (L881_) {
    L891_=(depends_on_arg)==(0); 
    if (L891_) {
     L90 = ATTR(ox,is_arg);
     L921_=!(L90); 
     if (L921_) {
      SATTR(lc,is_always_near,TRUE);
     }
    }
    else {
     {
      BOOL f_L931_ = TRUE;
      /* loop index variable */
      L101 = 0;
      L31 = func;
      L931_br=L31==NULL?0:ASIZE((AM_ROUT_DEF)L31); 
      while (1) {
       if(L101>=L931_br)  goto after_loop2; 
       aL931_=ARR((AM_ROUT_DEF)L31,L101); 
       L95=aL931_;
       ox2 = FMAPAM731742676(m, ATTR(L95,expr));
       L97 = (ox2==((OPT_LOCAL_EXPR) NULL));
       L981_=!(L97); 
       if (L981_) {
        aget_self1 = dep;
        aget_i11 = ATTR(ox,index1);
        aget_i21 = ATTR(ox2,index1);
        L99 = ATTR(aget_self1,size2);
        L1001_=INTTIMES(aget_i11,L99); 
        L102 = L1001_;
        L1031_=INTPLUS(L102,aget_i21); 
        L104 = L1031_;
        L1051_=ARR((ARRAY2BOOL)aget_self1,L104); 
        ret_val29 = L1051_;
        L96 = ret_val29;
       } else {
        L96 = FALSE;
       }
       if (L96) {
        L106 = ATTR(ox2,attr_access);
        L107 = ATTR(ox,attr_access);
        L1081_=INTDIV(L107,depends_on_arg); 
        L109 = L1081_;
        L1101_=INTPLUS(L106,L109); 
        SATTR(ox2,attr_access,L1101_);
       }
       L1111_=INTPLUS(L101,1); 
       L101 = L1111_;
      }
     }
     after_loop2: ;
    }
   }
  }
 }
 after_loop: ;
 attr_access_self = ATTR(func,sig1);
 create_self6 = ((ARRAYINT) NULL);
 L1121_=(func)==NULL?0:ASIZE((AM_ROUT_DEF)func); 
 create_n = L1121_;
 L114 = create_n;
 L116=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L114)*sizeof(INT);
 L115=ZALLOC_LEAF_BIG(L116);
 if (L115==NULL) FATAL("Unable to allocate more memory");
 memset(L115,0,L116);
 ((OB)L115)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

   ((OB)L115)->header.destroyed=0;
#endif

 L113 = ((ARRAYINT) L115);
 L113->asize = L114;
 ret_val30 = L113;
 attr_access_a = ret_val30;
 SATTR(ATTR(attr_access_self,opt_info),attr_access,attr_access_a);
 {
  BOOL f_L1171_ = TRUE;
  BOOL f_L1181_ = TRUE;
  /* loop index variable */
  L121 = 0;
  L41 = SIG_at89227380(ATTR(func,sig1));
  L51 = func;
  L1171_br=L41==NULL?0:ASIZE((ARRAYINT)L41); 
  L1181_br=L51==NULL?0:ASIZE((AM_ROUT_DEF)L51); 
  while (1) {
   if(L121>=L1171_br)  goto after_loop3; 
   rL1171_=L121; 
   i = rL1171_;
   if(L121>=L1181_br)  goto after_loop3; 
   aL1181_=ARR((AM_ROUT_DEF)L51,L121); 
   f = aL1181_;
   L122 = (f==((AM_FORMAL_ARG) NULL));
   L1231_=!(L122); 
   if (L1231_) {
    ox1 = FMAPAM731742676(m, ATTR(f,expr));
    L124 = (ox1==((OPT_LOCAL_EXPR) NULL));
    L1251_=!(L124); 
    if (L1251_) {
     L126 = SIG_at89227380(ATTR(func,sig1));
     L127 = ATTR(ox1,attr_access);
     SARR((ARRAYINT)L126,i,L127); 
     ;
    }
   }
   L1291_=INTPLUS(L121,1); 
   L121 = L1291_;
  }
 }
 after_loop3: ;
 L130 = (ATTR(ATTR(func,sig1),tp)==((dTP) NULL));
 L1321_=!(L130); 
 if (L1321_) {
  is_far1 = FALSE;
  depends_on_arg1 = 0;
  if (ATTR(ret_ox,is_sometimes_far)) {
   is_near_self1 = ATTR(func,sig1);
   is_near_a = FALSE;
   SATTR(ATTR(is_near_self1,opt_info),is_near,is_near_a);
   near_depends_on_ = ATTR(func,sig1);
   near_depends_on_1 = ((ARRAYINT) NULL);
   SATTR(ATTR(near_depends_on_,opt_info),near_depends_on_,near_depends_on_1);
  }
  else {
   {
    BOOL f_L1331_ = TRUE;
    /* loop index variable */
    L131 = 0;
    L61 = ATTR(dep,size1);
    while (1) {
     if(L131>=L61)  goto after_loop4; 
     aL1331_=L131; 
     ld1 = aL1331_;
     aget_self2 = dep;
     aget_i12 = 0;
     aget_i22 = ld1;
     L135 = ATTR(aget_self2,size2);
     L1361_=INTTIMES(aget_i12,L135); 
     L137 = L1361_;
     L1381_=INTPLUS(L137,aget_i22); 
     L139 = L1381_;
     L1401_=ARR((ARRAY2BOOL)aget_self2,L139); 
     ret_val31 = L1401_;
     if (ret_val31) {
      L1421_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,ld1); 
      L143=L1421_;
      if (ATTR(L143,is_sometimes_far)) {
       is_far1 = TRUE;
      }
      L1441_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,ld1); 
      L145=L1441_;
      if (ATTR(L145,is_arg)) {
       L1461_=INTPLUS(depends_on_arg1,1); 
       depends_on_arg1 = L1461_;
      }
     }
     L1471_=INTPLUS(L131,1); 
     L131 = L1471_;
    }
   }
   after_loop4: ;
   L1481_=!(is_far1); 
   if (L1481_) {
    L1491_=(depends_on_arg1)==(0); 
    if (L1491_) {
     is_near_self2 = ATTR(func,sig1);
     is_near_a1 = TRUE;
     SATTR(ATTR(is_near_self2,opt_info),is_near,is_near_a1);
    }
    else {
     create_self7 = ((ARRAYINT) NULL);
     create_n1 = depends_on_arg1;
     L151 = create_n1;
     L153=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L151)*sizeof(INT);
     L152=ZALLOC_LEAF_BIG(L153);
     if (L152==NULL) FATAL("Unable to allocate more memory");
     memset(L152,0,L153);
     ((OB)L152)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

       ((OB)L152)->header.destroyed=0;
#endif

     L150 = ((ARRAYINT) L152);
     L150->asize = L151;
     ret_val32 = L150;
     a = ret_val32;
     {
      BOOL f_L1541_ = TRUE;
      BOOL f_L1551_ = TRUE;
      /* loop index variable */
      L141 = 0;
      L71 = func;
      L81 = a;
      L1541_br=L71==NULL?0:ASIZE((AM_ROUT_DEF)L71); 
      L1551_br=L81==NULL?0:ASIZE((ARRAYINT)L81); 
      i_L1551_=0;
      while (1) {
       if(L141>=L1541_br)  goto after_loop5; 
       rL1541_=L141; 
       i1 = rL1541_;
       L1571_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,i1); 
       L158=L1571_;
       l = ATTR(L158,expr);
       ox3 = FMAPAM731742676(m, l);
       L160 = (ox3==((OPT_LOCAL_EXPR) NULL));
       L1611_=!(L160); 
       if (L1611_) {
        aget_self3 = dep;
        aget_i13 = 0;
        aget_i23 = ATTR(ox3,index1);
        L162 = ATTR(aget_self3,size2);
        L1631_=INTTIMES(aget_i13,L162); 
        L164 = L1631_;
        L1651_=INTPLUS(L164,aget_i23); 
        L166 = L1651_;
        L1671_=ARR((ARRAY2BOOL)aget_self3,L166); 
        ret_val33 = L1671_;
        L159 = ret_val33;
       } else {
        L159 = FALSE;
       }
       if (L159) {
        if(i_L1551_>=L1551_br)  goto after_loop5; 
        SARR((ARRAYINT)L81,i_L1551_,i1); i_L1551_++;
        ;
       }
       L1681_=INTPLUS(L141,1); 
       L141 = L1681_;
      }
     }
     after_loop5: ;
     near_depends_on_2 = ATTR(func,sig1);
     near_depends_on_3 = a;
     SATTR(ATTR(near_depends_on_2,opt_info),near_depends_on_,near_depends_on_3);
    }
   }
  }
 }
 prog_self2 = self;
 ret_val34 = OPT_CO233462019;
 L169=ret_val34;
 if (ATTR(L169,opt_debug)) {
  create_self8 = ((OUT) NULL);
  ret_val35 = create_self8;
  plus_self12 = ret_val35;
  plus_s12 = ((STR) &RESULT1809061834);
  stdout_self12 = ((FILE1) NULL);
  L171=ZALLOC(sizeof(struct FILE1_struct));
  if (L171==NULL) FATAL("Unable to allocate more memory");
  ((OB)L171)->header.tag=FILE1_tag;
  L170 = ((FILE1) L171);
  r12 = L170;
  
  SATTR(r12,fp,stdout);
  ret_val37 = r12;
  FILE_plus_STR(ret_val37, plus_s12);
  ret_val36 = plus_self12;
  plus_self13 = ret_val36;
  plus_s13 = ATTR(ATTR(func,sig1),str);
  stdout_self13 = ((FILE1) NULL);
  L174=ZALLOC(sizeof(struct FILE1_struct));
  if (L174==NULL) FATAL("Unable to allocate more memory");
  ((OB)L174)->header.tag=FILE1_tag;
  L173 = ((FILE1) L174);
  r13 = L173;
  
  SATTR(r13,fp,stdout);
  ret_val39 = r13;
  FILE_plus_STR(ret_val39, plus_s13);
  ret_val38 = plus_self13;
  plus_self14 = ret_val38;
  plus_s14 = ((STR) &name11);
  stdout_self14 = ((FILE1) NULL);
  L177=ZALLOC(sizeof(struct FILE1_struct));
  if (L177==NULL) FATAL("Unable to allocate more memory");
  ((OB)L177)->header.tag=FILE1_tag;
  L176 = ((FILE1) L177);
  r14 = L176;
  
  SATTR(r14,fp,stdout);
  ret_val40 = r14;
  FILE_plus_STR(ret_val40, plus_s14);
  L179 = (ATTR(ATTR(func,sig1),ret)==((dTP) NULL));
  L1801_=!(L179); 
  if (L1801_) {
   create_self9 = ((OUT) NULL);
   ret_val41 = create_self9;
   plus_self15 = ret_val41;
   plus_s15 = ((STR) &Functionis_near);
   stdout_self15 = ((FILE1) NULL);
   L182=ZALLOC(sizeof(struct FILE1_struct));
   if (L182==NULL) FATAL("Unable to allocate more memory");
   ((OB)L182)->header.tag=FILE1_tag;
   L181 = ((FILE1) L182);
   r15 = L181;
   
   SATTR(r15,fp,stdout);
   ret_val43 = r15;
   FILE_plus_STR(ret_val43, plus_s15);
   ret_val42 = plus_self15;
   plus_self16 = ret_val42;
   is_near_self3 = ATTR(func,sig1);
   if (SIG_is418491101(is_near_self3)) {
    L186 = ATTR(ATTR(is_near_self3,builtin_info),is_near);
   } else {
    L186 = FALSE;
   }
   if (L186) {
    L185 = TRUE;
   } else {
    L187 = ATTR(is_near_self3,ret);
    L185 = (*dTP_is1334578382[TAG(L187)])(L187);
   }
   if (L185) {
    L184 = TRUE;
   } else {
    L189 = (ATTR(is_near_self3,opt_info)==((OPT_LOCAL_CALL) NULL));
    L1901_=!(L189); 
    if (L1901_) {
     L188 = ATTR(ATTR(is_near_self3,opt_info),is_near);
    } else {
     L188 = FALSE;
    }
    L184 = L188;
   }
   ret_val44 = L184;
   L192=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
   if (L192==NULL) FATAL("Unable to allocate more memory");
   memset(L192,0,sizeof(struct BOOL_boxed_struct));
   ((OB)L192)->header.tag=BOOL_tag;
   L191 = (dSTR)((BOOL_boxed) L192);
   ((BOOL_boxed) L191)->immutable_part = ret_val44;
   plus_s16 = L191;
   stdout_self16 = ((FILE1) NULL);
   L194=ZALLOC(sizeof(struct FILE1_struct));
   if (L194==NULL) FATAL("Unable to allocate more memory");
   ((OB)L194)->header.tag=FILE1_tag;
   L193 = ((FILE1) L194);
   r16 = L193;
   
   SATTR(r16,fp,stdout);
   ret_val46 = r16;
   FILE_plus_dSTR(ret_val46, plus_s16);
   ret_val45 = plus_self16;
   plus_self17 = ret_val45;
   plus_s17 = ((STR) &args3);
   stdout_self17 = ((FILE1) NULL);
   L197=ZALLOC(sizeof(struct FILE1_struct));
   if (L197==NULL) FATAL("Unable to allocate more memory");
   ((OB)L197)->header.tag=FILE1_tag;
   L196 = ((FILE1) L197);
   r17 = L196;
   
   SATTR(r17,fp,stdout);
   ret_val48 = r17;
   FILE_plus_STR(ret_val48, plus_s17);
   ret_val47 = plus_self17;
   plus_self18 = ret_val47;
   plus_s18 = ARRAYINT_strrSTR(SIG_ne271652128(ATTR(func,sig1)));
   stdout_self18 = ((FILE1) NULL);
   L200=ZALLOC(sizeof(struct FILE1_struct));
   if (L200==NULL) FATAL("Unable to allocate more memory");
   ((OB)L200)->header.tag=FILE1_tag;
   L199 = ((FILE1) L200);
   r18 = L199;
   
   SATTR(r18,fp,stdout);
   ret_val49 = r18;
   FILE_plus_STR(ret_val49, plus_s18);
  }
  create_self10 = ((OUT) NULL);
  ret_val50 = create_self10;
  plus_self19 = ret_val50;
  plus_s19 = ((STR) &access2);
  stdout_self19 = ((FILE1) NULL);
  L203=ZALLOC(sizeof(struct FILE1_struct));
  if (L203==NULL) FATAL("Unable to allocate more memory");
  ((OB)L203)->header.tag=FILE1_tag;
  L202 = ((FILE1) L203);
  r19 = L202;
  
  SATTR(r19,fp,stdout);
  ret_val52 = r19;
  FILE_plus_STR(ret_val52, plus_s19);
  ret_val51 = plus_self19;
  plus_self20 = ret_val51;
  plus_s20 = ARRAYINT_strrSTR(SIG_at89227380(ATTR(func,sig1)));
  stdout_self20 = ((FILE1) NULL);
  L206=ZALLOC(sizeof(struct FILE1_struct));
  if (L206==NULL) FATAL("Unable to allocate more memory");
  ((OB)L206)->header.tag=FILE1_tag;
  L205 = ((FILE1) L206);
  r20 = L205;
  
  SATTR(r20,fp,stdout);
  ret_val54 = r20;
  FILE_plus_STR(ret_val54, plus_s20);
  ret_val53 = plus_self20;
  plus_self21 = ret_val53;
  plus_s21 = ((STR) &name9);
  stdout_self21 = ((FILE1) NULL);
  L209=ZALLOC(sizeof(struct FILE1_struct));
  if (L209==NULL) FATAL("Unable to allocate more memory");
  ((OB)L209)->header.tag=FILE1_tag;
  L208 = ((FILE1) L209);
  r21 = L208;
  
  SATTR(r21,fp,stdout);
  ret_val55 = r21;
  FILE_plus_STR(ret_val55, plus_s21);
  create_self11 = ((OUT) NULL);
  ret_val56 = create_self11;
  plus_self22 = ret_val56;
  plus_s22 = ((STR) &NEARLO1748192338);
  stdout_self22 = ((FILE1) NULL);
  L212=ZALLOC(sizeof(struct FILE1_struct));
  if (L212==NULL) FATAL("Unable to allocate more memory");
  ((OB)L212)->header.tag=FILE1_tag;
  L211 = ((FILE1) L212);
  r22 = L211;
  
  SATTR(r22,fp,stdout);
  ret_val57 = r22;
  FILE_plus_STR(ret_val57, plus_s22);
  {
   struct FMAPAM1518099927_frame_struct other7_0;
FMAPAM1518099927_frame noname2 = &other7_0;
   L91 = m;
   noname2->self = L91;
   noname2->state = 0;
   while (1) {
    L214 = FMAPAM1518099927(noname2);
    if (noname2->state == -1) {
     goto after_loop6;
    }
    t = L214;
    lc1 = t.t1;
    ox4 = t.t2;
    is_near_self4 = lc1;
    if (ATTR(is_near_self4,is_always_near)) {
     L215 = TRUE;
    } else {
     L216 = ATTR(is_near_self4,tp_at);
     L215 = (*dTP_is1334578382[TAG(L216)])(L216);
    }
    ret_val58 = L215;
    if (ret_val58) {
     create_self12 = ((OUT) NULL);
     ret_val59 = create_self12;
     plus_self23 = ret_val59;
     plus_s23 = ((STR) &name66);
     stdout_self23 = ((FILE1) NULL);
     L218=ZALLOC(sizeof(struct FILE1_struct));
     if (L218==NULL) FATAL("Unable to allocate more memory");
     ((OB)L218)->header.tag=FILE1_tag;
     L217 = ((FILE1) L218);
     r23 = L217;
     
     SATTR(r23,fp,stdout);
     ret_val60 = r23;
     FILE_plus_STR(ret_val60, plus_s23);
     AM_OUT1439666796(((AM_OUT) NULL), ((dAM) lc1));
     create_self13 = ((OUT) NULL);
     ret_val61 = create_self13;
     plus_self24 = ret_val61;
     plus_s24 = ((STR) &name9);
     stdout_self24 = ((FILE1) NULL);
     L222=ZALLOC(sizeof(struct FILE1_struct));
     if (L222==NULL) FATAL("Unable to allocate more memory");
     ((OB)L222)->header.tag=FILE1_tag;
     L220 = ((FILE1) L222);
     r24 = L220;
     
     SATTR(r24,fp,stdout);
     ret_val62 = r24;
     FILE_plus_STR(ret_val62, plus_s24);
    }
   }
  }
  after_loop6: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO1523894656(OPT_LOCAL self, FMAPAM418009930 m, ARRAY2BOOL dep, STR str) {
 INT i = INT_zero;
 FLISTINT used1 = ((FLISTINT) NULL);
 STR var;
 FMAPAM418009930 L11;
 TUPAM_504653531 p = TUPAM_504653531_zero;
 FLISTINT L21;
 FLISTINT L31;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val1;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val2;
 STR r;
 OPT_LOCAL prog_self;
 PROG ret_val3;
 OUT create_self1;
 OUT ret_val4;
 OUT plus_self1;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val5;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val6;
 OUT plus_self2;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val7;
 FILE1 r2;
 FLISTINT aget_self;
 INT aget_ind = INT_zero;
 INT ret_val8 = INT_zero;
 ARRAY2BOOL aset_self;
 INT aset_i1 = INT_zero;
 INT aset_i2 = INT_zero;
 BOOL aset_val = BOOL_zero;
 FLISTINT aget_self1;
 INT aget_ind1 = INT_zero;
 INT ret_val9 = INT_zero;
 ARRAY2BOOL aset_self1;
 INT aset_i11 = INT_zero;
 INT aset_i21 = INT_zero;
 BOOL aset_val1 = BOOL_zero;
 INT L4;
 BOOL L51_;
 CHAR L61_;
 CHAR L7;
 BOOL L81_;
 BOOL L9;
 INT L101_;
 INT L12;
 INT L13;
 BOOL L141_;
 INT L151_;
 INT L16;
 CHAR L171_;
 CHAR L18;
 BOOL L191_;
 INT L201_;
 extern STR name1;
 BOOL L22;
 INT L231_;
 INT L24;
 INT L25;
 BOOL L261_;
 BOOL L27;
 INT L281_;
 INT L29;
 CHAR L301_;
 INT L321_;
 INT L33;
 CHAR L341_;
 CHAR L35;
 BOOL L361_;
 INT L371_;
 CHAR L381_;
 STR L39;
 INT L40;
 OB L41;
 INT L42;
 TUPAM_504653531 L44;
 PROG L45;
 extern STR inline1440253858;
 FILE1 L46;
 OB L47;
 extern STR name9;
 FILE1 L49;
 OB L50;
 INT L53;
 INT L541_;
 INT L551_;
 BOOL L56;
 BOOL L571_;
 INT L581_;
 INT L59;
 INT L60;
 INT L621_;
 INT L63;
 INT L641_;
 INT L65;
 INT L67;
 INT L681_;
 INT L69;
 INT L701_;
 INT L71;
 INT L721_;
 INT L73;
 if ((str==((STR) NULL))) {
  return;
 }
 i = 0;
 while (1) {
  L4 = STR_sizerINT(str);
  L51_=(i)<(L4); 
  if (L51_) {
  }
  else {
   goto after_loop;
  }
  L61_=ARR((STR)str,i); 
  L7 = L61_;
  L81_=L7=='#'; 
  if (L81_) {
   L101_=INTPLUS(i,1); 
   L12 = L101_;
   L13 = STR_sizerINT(str);
   L141_=(L12)<(L13); 
   if (L141_) {
    L151_=INTPLUS(i,1); 
    L16 = L151_;
    L171_=ARR((STR)str,L16); 
    L18 = L171_;
    L191_=L18=='#'; 
    L9 = L191_;
   } else {
    L9 = FALSE;
   }
   if (L9) {
    L201_=INTPLUS(i,1); 
    i = L201_;
   }
   else {
    var = ((STR) &name1);
    while (1) {
     L231_=INTPLUS(i,1); 
     L24 = L231_;
     L25 = STR_sizerINT(str);
     L261_=(L24)<(L25); 
     if (L261_) {
      L281_=INTPLUS(i,1); 
      L29 = L281_;
      L301_=ARR((STR)str,L29); 
      if (CHAR_i1942379470(L301_)) {
       L27 = TRUE;
      } else {
       L321_=INTPLUS(i,1); 
       L33 = L321_;
       L341_=ARR((STR)str,L33); 
       L35 = L341_;
       L361_=L35=='_'; 
       L27 = L361_;
      }
      L22 = L27;
     } else {
      L22 = FALSE;
     }
     if (L22) {
     }
     else {
      goto after_loop1;
     }
     L371_=INTPLUS(i,1); 
     i = L371_;
     plus_self = var;
     L381_=ARR((STR)str,i); 
     plus_arg = L381_;
     str_self = plus_arg;
     create_self = ((STR) NULL);
     create_c = str_self;
     L40 = 1;
     L42=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L40)*sizeof(CHAR);
     L41=ZALLOC_LEAF_BIG(L42);
     if (L41==NULL) FATAL("Unable to allocate more memory");
     memset(L41,0,L42);
     ((OB)L41)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L41)->header.destroyed=0;
#endif

     L39 = ((STR) L41);
     L39->asize = L40;
     r = L39;
     SARR((STR)r,0,create_c); 
     ;
     ret_val2 = r;
     ret_val1 = ret_val2;
     s = ret_val1;
     ret_val = STR_ap1077157958(plus_self, s, TRUE);
     var = ret_val;
    }
    after_loop1: ;
    {
     struct FMAPAM1518099927_frame_struct other3_0;
FMAPAM1518099927_frame noname1 = &other3_0;
     L11 = m;
     noname1->self = L11;
     noname1->state = 0;
     while (1) {
      L44 = FMAPAM1518099927(noname1);
      if (noname1->state == -1) {
       goto after_loop2;
      }
      p = L44;
      if (STR_is111530248(ATTR(p.t1,name1).str, var)) {
       prog_self = self;
       ret_val3 = OPT_CO233462019;
       L45=ret_val3;
       if (ATTR(L45,opt_debug)) {
        create_self1 = ((OUT) NULL);
        ret_val4 = create_self1;
        plus_self1 = ret_val4;
        plus_s = ((STR) &inline1440253858);
        stdout_self = ((FILE1) NULL);
        L47=ZALLOC(sizeof(struct FILE1_struct));
        if (L47==NULL) FATAL("Unable to allocate more memory");
        ((OB)L47)->header.tag=FILE1_tag;
        L46 = ((FILE1) L47);
        r1 = L46;
        
        SATTR(r1,fp,stdout);
        ret_val5 = r1;
        FILE_plus_STR(ret_val5, plus_s);
        AM_OUT1439666796(((AM_OUT) NULL), ((dAM) p.t1));
        create_self2 = ((OUT) NULL);
        ret_val6 = create_self2;
        plus_self2 = ret_val6;
        plus_s1 = ((STR) &name9);
        stdout_self1 = ((FILE1) NULL);
        L50=ZALLOC(sizeof(struct FILE1_struct));
        if (L50==NULL) FATAL("Unable to allocate more memory");
        ((OB)L50)->header.tag=FILE1_tag;
        L49 = ((FILE1) L50);
        r2 = L49;
        
        SATTR(r2,fp,stdout);
        ret_val7 = r2;
        FILE_plus_STR(ret_val7, plus_s1);
       }
       SATTR(p.t2,is_sometimes_far,TRUE);
       L53 = ATTR(p.t2,attr_access);
       L541_=INTPLUS(L53,1); 
       SATTR(p.t2,attr_access,L541_);
       used1 = FLISTI1369678776(used1, ATTR(p.t2,index1));
      }
     }
    }
    after_loop2: ;
   }
  }
  L551_=INTPLUS(i,1); 
  i = L551_;
 }
 after_loop: ;
 L56 = (used1==((FLISTINT) NULL));
 L571_=!(L56); 
 if (L571_) {
  {
   struct FLISTI476396026_frame_struct other4_0;
FLISTI476396026_frame noname2 = &other4_0;
   struct FLISTI476396026_frame_struct other4_1;
FLISTI476396026_frame noname3 = &other4_1;
   L21 = used1;
   L31 = used1;
   noname2->self = L21;
   noname2->state = 0;
   noname3->self = L31;
   noname3->state = 0;
   while (1) {
    aset_self = dep;
    aget_self = used1;
    aget_ind = 0;
    L581_=ARR((FLISTINT)aget_self,aget_ind); 
    ret_val8 = L581_;
    aset_i1 = ret_val8;
    L59 = FLISTI476396026(noname2);
    if (noname2->state == -1) {
     goto after_loop3;
    }
    aset_i2 = L59;
    aset_val = TRUE;
    L60 = ATTR(aset_self,size2);
    L621_=INTTIMES(aset_i1,L60); 
    L63 = L621_;
    L641_=INTPLUS(L63,aset_i2); 
    L65 = L641_;
    SARR((ARRAY2BOOL)aset_self,L65,aset_val); 
    ;
    aset_self1 = dep;
    L67 = FLISTI476396026(noname3);
    if (noname3->state == -1) {
     goto after_loop3;
    }
    aset_i11 = L67;
    aget_self1 = used1;
    aget_ind1 = 0;
    L681_=ARR((FLISTINT)aget_self1,aget_ind1); 
    ret_val9 = L681_;
    aset_i21 = ret_val9;
    aset_val1 = TRUE;
    L69 = ATTR(aset_self1,size2);
    L701_=INTTIMES(aset_i11,L69); 
    L71 = L701_;
    L721_=INTPLUS(L71,aset_i21); 
    L73 = L721_;
    SARR((ARRAY2BOOL)aset_self1,L73,aset_val1); 
    ;
   }
  }
  after_loop3: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO238575330(OPT_LOCAL self, AM_ROUT_DEF func, ARRAY2BOOL dep, FMAPAM418009930 m, OPT_LOCAL_EXPR ret_ox) {
 AM_CURSOR c;
 FLISTBOOL wn;
 AM_CURSOR L11;
 dAM am;
 AM_WIT1996971603 w;
 ARRAYdAM_EXPR L21;
 dAM_EXPR a;
 ARRAYdAM_EXPR L31;
 dAM_EXPR a11;
 AM_COMMENT_STMT dummy1;
 dAM_EXPR ob;
 OPT_LOCAL_EXPR ox;
 dAM_EXPR ex1;
 AM_ROU1916046290 L41;
 INT i = INT_zero;
 AM_CALL_ARG ob11;
 dAM_EXPR l;
 OPT_LOCAL_EXPR ox1;
 AM_ITE1809135850 L51;
 INT i1 = INT_zero;
 AM_CALL_ARG ob2;
 dAM_EXPR l1;
 OPT_LOCAL_EXPR ox2;
 dAM_EXPR l11;
 OPT_LOCAL_EXPR ox3;
 FLISTA725283029 dpon;
 FLISTA725283029 L61;
 AM_LOCAL_EXPR d;
 OPT_LOCAL_EXPR m1;
 FLISTA725283029 L71;
 OPT_LOCAL_EXPR m2;
 FLISTA725283029 L81;
 dAM_EXPR l12;
 OPT_LOCAL_EXPR ox4;
 FLISTA725283029 dpon1;
 FLISTA725283029 L91;
 AM_LOCAL_EXPR d11;
 OPT_LOCAL_EXPR m11;
 FLISTA725283029 L101;
 OPT_LOCAL_EXPR m21;
 FLISTA725283029 L121;
 FLISTA725283029 dpon2;
 FLISTA725283029 L131;
 AM_LOCAL_EXPR d21;
 FLISTA725283029 L141;
 OPT_LOCAL prog_self;
 PROG ret_val;
 FLISTBOOL create_self;
 FLISTBOOL ret_val1;
 INT L151 = INT_zero;
 INT L161 = INT_zero;
 AM_COMMENT_STMT create_self1;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_COMMENT_STMT ret_val2;
 AM_COMMENT_STMT r;
 AM_LOCAL_EXPR tp_self;
 dTP ret_val3;
 OPT_LOCAL prog_self1;
 PROG ret_val4;
 OUT create_self2;
 OUT ret_val5;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val6;
 FILE1 r1;
 OUT create_self3;
 OUT ret_val7;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val8;
 FILE1 r2;
 INT L171 = INT_zero;
 AM_LOCAL_EXPR tp_self1;
 dTP ret_val9;
 OPT_LOCAL prog_self2;
 PROG ret_val10;
 OUT create_self4;
 OUT ret_val11;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val12;
 FILE1 stdout_self2;
 FILE1 ret_val13;
 FILE1 r3;
 OUT plus_self3;
 dSTR plus_s3;
 OUT ret_val14;
 FILE1 stdout_self3;
 FILE1 ret_val15;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val16;
 FILE1 r5;
 OUT create_self5;
 OUT ret_val17;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val18;
 FILE1 r6;
 OPT_LOCAL prog_self3;
 PROG ret_val19;
 OUT create_self6;
 OUT ret_val20;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val21;
 FILE1 r7;
 OUT create_self7;
 OUT ret_val22;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val23;
 FILE1 r8;
 INT L181 = INT_zero;
 AM_LOCAL_EXPR tp_self2;
 dTP ret_val24;
 OPT_LOCAL prog_self4;
 PROG ret_val25;
 OUT create_self8;
 OUT ret_val26;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val27;
 FILE1 stdout_self8;
 FILE1 ret_val28;
 FILE1 r9;
 OUT plus_self9;
 dSTR plus_s9;
 OUT ret_val29;
 FILE1 stdout_self9;
 FILE1 ret_val30;
 FILE1 r10;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val31;
 FILE1 r11;
 OUT create_self9;
 OUT ret_val32;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val33;
 FILE1 r12;
 OPT_LOCAL prog_self5;
 PROG ret_val34;
 OUT create_self10;
 OUT ret_val35;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val36;
 FILE1 r13;
 OUT create_self11;
 OUT ret_val37;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val38;
 FILE1 r14;
 OPT_LOCAL prog_self6;
 PROG ret_val39;
 OUT create_self12;
 OUT ret_val40;
 OUT plus_self14;
 STR plus_s14;
 OUT ret_val41;
 FILE1 stdout_self14;
 FILE1 ret_val42;
 FILE1 r15;
 OUT plus_self15;
 dSTR plus_s15;
 OUT ret_val43;
 FILE1 stdout_self15;
 FILE1 ret_val44;
 FILE1 r16;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val45;
 FILE1 r17;
 OPT_LOCAL prog_self7;
 PROG ret_val46;
 OUT create_self13;
 OUT ret_val47;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val48;
 FILE1 r18;
 OUT create_self14;
 OUT ret_val49;
 OUT plus_self18;
 STR plus_s18;
 FILE1 stdout_self18;
 FILE1 ret_val50;
 FILE1 r19;
 ARRAY2BOOL aset_self;
 INT aset_i1 = INT_zero;
 INT aset_i2 = INT_zero;
 BOOL aset_val = BOOL_zero;
 OPT_LOCAL prog_self8;
 PROG ret_val51;
 OUT create_self15;
 OUT ret_val52;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val53;
 FILE1 r20;
 OUT create_self16;
 OUT ret_val54;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val55;
 FILE1 r21;
 OPT_LOCAL prog_self9;
 PROG ret_val56;
 OUT create_self17;
 OUT ret_val57;
 OUT plus_self21;
 STR plus_s21;
 OUT ret_val58;
 FILE1 stdout_self21;
 FILE1 ret_val59;
 FILE1 r22;
 OUT plus_self22;
 dSTR plus_s22;
 OUT ret_val60;
 FILE1 stdout_self22;
 FILE1 ret_val61;
 FILE1 r23;
 OUT plus_self23;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val62;
 FILE1 r24;
 OPT_LOCAL prog_self10;
 PROG ret_val63;
 OUT create_self18;
 OUT ret_val64;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val65;
 FILE1 r25;
 OUT create_self19;
 OUT ret_val66;
 OUT plus_self25;
 STR plus_s25;
 FILE1 stdout_self25;
 FILE1 ret_val67;
 FILE1 r26;
 ARRAY2BOOL aset_self1;
 INT aset_i11 = INT_zero;
 INT aset_i21 = INT_zero;
 BOOL aset_val1 = BOOL_zero;
 OPT_LOCAL prog_self11;
 PROG ret_val68;
 OUT create_self20;
 OUT ret_val69;
 OUT plus_self26;
 STR plus_s26;
 FILE1 stdout_self26;
 FILE1 ret_val70;
 FILE1 r27;
 OUT create_self21;
 OUT ret_val71;
 OUT plus_self27;
 STR plus_s27;
 FILE1 stdout_self27;
 FILE1 ret_val72;
 FILE1 r28;
 OPT_LOCAL prog_self12;
 PROG ret_val73;
 OUT create_self22;
 OUT ret_val74;
 OUT plus_self28;
 STR plus_s28;
 FILE1 stdout_self28;
 FILE1 ret_val75;
 FILE1 r29;
 OPT_LOCAL prog_self13;
 PROG ret_val76;
 OUT create_self23;
 OUT ret_val77;
 OUT plus_self29;
 STR plus_s29;
 FILE1 stdout_self29;
 FILE1 ret_val78;
 FILE1 r30;
 OUT create_self24;
 OUT ret_val79;
 OUT plus_self30;
 STR plus_s30;
 FILE1 stdout_self30;
 FILE1 ret_val80;
 FILE1 r31;
 ARRAY2BOOL aset_self2;
 INT aset_i12 = INT_zero;
 INT aset_i22 = INT_zero;
 BOOL aset_val2 = BOOL_zero;
 dAM L19;
 INT L201_br;
 dAM_EXPR aL201_;
 dAM_EXPR L22;
 INT L231_;
 INT L241_br;
 dAM_EXPR aL241_;
 dAM_EXPR L25;
 INT L261_;
 AM_COMMENT_STMT L27;
 OB L28;
 dTP L29;
 BOOL L30;
 BOOL L321_;
 PROG L33;
 extern STR attraccessfor;
 FILE1 L34;
 OB L35;
 extern STR name9;
 FILE1 L37;
 OB L38;
 INT L40;
 INT L421_;
 INT L43;
 INT L44;
 INT L451_;
 INT L46;
 INT L471_;
 BOOL L48;
 dTP L49;
 extern STR inlined_C;
 AM_CALL_ARG L501_;
 AM_CALL_ARG L52;
 INT L531_br;
 INT rL531_;
 INT L54;
 AM_CALL_ARG L551_;
 dTP L56;
 BOOL L57;
 BOOL L581_;
 BOOL L59;
 BOOL L601_;
 INT L62;
 ARRAYINT L63;
 INT L641_;
 INT L65;
 INT L66;
 INT L671_;
 INT L68;
 INT L691_;
 INT L70;
 INT L721_;
 INT L73;
 INT L741_;
 PROG L75;
 extern STR routattraccess;
 FILE1 L76;
 OB L77;
 ARRAYINT L79;
 INT L801_;
 dSTR L82;
 OB L83;
 FILE1 L84;
 OB L85;
 extern STR for1;
 FILE1 L87;
 OB L88;
 extern STR name9;
 FILE1 L90;
 OB L92;
 dMODE L94;
 BOOL L95;
 BOOL L961_;
 PROG L97;
 extern STR is_sometimes_far;
 FILE1 L98;
 OB L99;
 extern STR name9;
 FILE1 L102;
 OB L103;
 INT L1051_;
 INT L1061_br;
 INT rL1061_;
 INT L107;
 AM_CALL_ARG L1081_;
 dTP L109;
 BOOL L110;
 BOOL L1111_;
 BOOL L112;
 BOOL L1131_;
 INT L114;
 ARRAYINT L115;
 INT L1161_;
 INT L117;
 INT L118;
 INT L1191_;
 INT L120;
 INT L1221_;
 INT L123;
 INT L1241_;
 INT L125;
 INT L1261_;
 PROG L127;
 extern STR routattraccess;
 FILE1 L128;
 OB L129;
 ARRAYINT L132;
 INT L1331_;
 dSTR L134;
 OB L135;
 FILE1 L136;
 OB L137;
 extern STR for1;
 FILE1 L139;
 OB L140;
 extern STR name9;
 FILE1 L143;
 OB L144;
 dMODE L146;
 BOOL L147;
 BOOL L1481_;
 PROG L149;
 extern STR is_sometimes_far;
 FILE1 L150;
 OB L152;
 extern STR name9;
 FILE1 L154;
 OB L155;
 INT L1571_;
 PROG L158;
 extern STR workin412447767;
 FILE1 L159;
 OB L160;
 INT L1631_;
 dSTR L164;
 OB L165;
 FILE1 L166;
 OB L167;
 extern STR name65;
 FILE1 L169;
 OB L170;
 dAM_EXPR L173;
 dTP L174;
 BOOL L175;
 BOOL L1761_;
 dAM_EXPR L177;
 BOOL L178;
 BOOL L1791_;
 dAM_EXPR L180;
 BOOL L182;
 BOOL L1831_;
 PROG L184;
 AM_LOCAL_EXPR L185;
 extern STR dependson;
 FILE1 L186;
 OB L187;
 extern STR name9;
 FILE1 L189;
 OB L190;
 AM_LOCAL_EXPR L192;
 BOOL L193;
 BOOL L194;
 BOOL L1951_;
 BOOL L196;
 BOOL L1971_;
 OPT_LOCAL_EXPR L198;
 AM_LOCAL_EXPR L199;
 OPT_LOCAL_EXPR L200;
 INT L202;
 INT L2031_;
 INT L204;
 INT L2051_;
 INT L206;
 PROG L208;
 extern STR sometimesfarfor;
 FILE1 L209;
 OB L210;
 extern STR name9;
 FILE1 L212;
 OB L213;
 PROG L215;
 extern STR workin1782648620;
 FILE1 L216;
 OB L217;
 INT L2191_;
 dSTR L220;
 OB L221;
 FILE1 L222;
 OB L223;
 extern STR name65;
 FILE1 L225;
 OB L226;
 dAM_EXPR L228;
 dTP L229;
 BOOL L230;
 BOOL L2321_;
 dAM_EXPR L233;
 BOOL L234;
 BOOL L2351_;
 dAM_EXPR L236;
 BOOL L237;
 BOOL L2381_;
 PROG L239;
 AM_LOCAL_EXPR L240;
 extern STR dependson;
 FILE1 L242;
 OB L243;
 extern STR name9;
 FILE1 L245;
 OB L246;
 AM_LOCAL_EXPR L248;
 BOOL L249;
 BOOL L250;
 BOOL L2511_;
 BOOL L252;
 BOOL L2531_;
 OPT_LOCAL_EXPR L254;
 AM_LOCAL_EXPR L255;
 OPT_LOCAL_EXPR L256;
 INT L257;
 INT L2581_;
 INT L259;
 INT L2601_;
 INT L262;
 PROG L264;
 extern STR sometimesfarfor;
 FILE1 L265;
 OB L266;
 extern STR name9;
 FILE1 L268;
 OB L269;
 BOOL L271;
 BOOL L2721_;
 dAM_EXPR L273;
 BOOL L274;
 dAM_EXPR L275;
 BOOL L276;
 BOOL L2771_;
 PROG L278;
 extern STR return463650331;
 FILE1 L279;
 OB L280;
 BOOL L282;
 BOOL L2831_;
 PROG L284;
 AM_LOCAL_EXPR L285;
 extern STR returndependson;
 FILE1 L286;
 OB L287;
 extern STR name9;
 FILE1 L289;
 OB L290;
 AM_LOCAL_EXPR L292;
 OPT_LOCAL_EXPR L293;
 INT L294;
 INT L2951_;
 INT L296;
 INT L2971_;
 INT L298;
 prog_self = self;
 ret_val = OPT_CO233462019;
 c = AM_CUR1277440868(((AM_CURSOR) NULL), ret_val, ((dAM) ATTR(func,code1)));
 create_self = ((FLISTBOOL) NULL);
 ret_val1 = ((FLISTBOOL) NULL);
 wn = ret_val1;
 {
  struct AM_CUR1040670508_frame_struct other1_0;
AM_CUR1040670508_frame noname1 = &other1_0;
  L11 = c;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L19 = AM_CUR1040670508(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   am = L19;
   if (AM_CUR1778077145(c)) {
    w = AM_CUR554772257(c);
    {
     BOOL f_L201_ = TRUE;
     /* loop index variable */
     L151 = 0;
     L21 = ATTR(w,objects1);
     L201_br=L21==NULL?0:ASIZE((ARRAYdAM_EXPR)L21); 
     while (1) {
      if(L151>=L201_br)  goto after_loop1; 
      aL201_=ARR((ARRAYdAM_EXPR)L21,L151); 
      a = aL201_;
      if (a==NULL) {
       goto other340;
      } else
      switch (TAG(a)) {
       case AM_LOCAL_EXPR_tag:
        SATTR(((AM_LOCAL_EXPR) a),is_always_near,FLISTB466509411(wn)); break;
       default: ;
       other340: ;
      }
      L231_=INTPLUS(L151,1); 
      L151 = L231_;
     }
    }
    after_loop1: ;
   }
   if (am==NULL) {
    goto other341;
   } else
   switch (TAG(am)) {
    case AM_WIT1996971603_tag:
     {
      BOOL f_L241_ = TRUE;
      /* loop index variable */
      L161 = 0;
      L31 = ATTR(((AM_WIT1996971603) am),objects1);
      L241_br=L31==NULL?0:ASIZE((ARRAYdAM_EXPR)L31); 
      while (1) {
       if(L161>=L241_br)  goto after_loop2; 
       aL241_=ARR((ARRAYdAM_EXPR)L31,L161); 
       a11 = aL241_;
       if (a11==NULL) {
        goto other342;
       } else
       switch (TAG(a11)) {
        case AM_LOCAL_EXPR_tag:
         wn = FLISTB1991913204(wn, ATTR(((AM_LOCAL_EXPR) a11),is_always_near));
         SATTR(((AM_LOCAL_EXPR) a11),is_always_near,TRUE); break;
        default: ;
        other342: ;
       }
       L261_=INTPLUS(L161,1); 
       L161 = L261_;
      }
     }
     after_loop2: ;
     create_self1 = ((AM_COMMENT_STMT) NULL);
     create_source = SFILE_ID_zero;
     L28=ZALLOC(sizeof(struct AM_COMMENT_STMT_struct));
     if (L28==NULL) FATAL("Unable to allocate more memory");
     ((OB)L28)->header.tag=AM_COMMENT_STMT_tag;
     L27 = ((AM_COMMENT_STMT) L28);
     r = L27;
     SATTR(r,source1,create_source);
     ret_val2 = r;
     dummy1 = ret_val2;
     SATTR(dummy1,next,ATTR(((AM_WIT1996971603) am),else_part));
     SATTR(((AM_WIT1996971603) am),else_part,((dAM_STMT) dummy1)); break;
    case AM_ATTR_EXPR_tag:
     ob = ATTR(((AM_ATTR_EXPR) am),ob);
     if (ob==NULL) {
      goto other343;
     } else
     switch (TAG(ob)) {
      case AM_LOCAL_EXPR_tag:
       tp_self = ((AM_LOCAL_EXPR) ob);
       ret_val3 = ATTR(tp_self,tp_at);
       L29 = ret_val3;
       L30 = (*dTP_is1334578382[TAG(L29)])(L29);
       L321_=!(L30); 
       if (L321_) {
        ox = FMAPAM731742676(m, ((AM_LOCAL_EXPR) ob));
        prog_self1 = self;
        ret_val4 = OPT_CO233462019;
        L33=ret_val4;
        if (ATTR(L33,opt_debug)) {
         create_self2 = ((OUT) NULL);
         ret_val5 = create_self2;
         plus_self = ret_val5;
         plus_s = ((STR) &attraccessfor);
         stdout_self = ((FILE1) NULL);
         L35=ZALLOC(sizeof(struct FILE1_struct));
         if (L35==NULL) FATAL("Unable to allocate more memory");
         ((OB)L35)->header.tag=FILE1_tag;
         L34 = ((FILE1) L35);
         r1 = L34;
         
         SATTR(r1,fp,stdout);
         ret_val6 = r1;
         FILE_plus_STR(ret_val6, plus_s);
         AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ob));
         create_self3 = ((OUT) NULL);
         ret_val7 = create_self3;
         plus_self1 = ret_val7;
         plus_s1 = ((STR) &name9);
         stdout_self1 = ((FILE1) NULL);
         L38=ZALLOC(sizeof(struct FILE1_struct));
         if (L38==NULL) FATAL("Unable to allocate more memory");
         ((OB)L38)->header.tag=FILE1_tag;
         L37 = ((FILE1) L38);
         r2 = L37;
         
         SATTR(r2,fp,stdout);
         ret_val8 = r2;
         FILE_plus_STR(ret_val8, plus_s1);
        }
        L40 = ATTR(ox,attr_access);
        L421_=INTPLUS(L40,1); 
        L43 = L421_;
        L44 = ATTR(c,loops);
        L451_=INTTIMES(L44,5); 
        L46 = L451_;
        L471_=INTPLUS(L43,L46); 
        SATTR(ox,attr_access,L471_);
       } break;
      default: ;
      other343: ;
     } break;
    case AM_ROU1916046290_tag:
     L49 = ATTR(ATTR(((AM_ROU1916046290) am),fun),tp);
     if ((*dTP_is242312711[TAG(L49)])(L49, ((OB) TP_BUILTIN_sys))) {
      L48 = STR_is111530248(ATTR(ATTR(((AM_ROU1916046290) am),fun),name1).str, ((STR) &inlined_C));
     } else {
      L48 = FALSE;
     }
     if (L48) {
      L501_=(AM_CALL_ARG)ARR((AM_ROU1916046290)am,1); 
      L52=L501_;
      ex1 = ATTR(L52,expr);
      if (ex1==NULL) {
       goto other344;
      } else
      switch (TAG(ex1)) {
       case AM_STR_CONST_tag:
        OPT_LO1523894656(self, m, dep, ATTR(((AM_STR_CONST) ex1),bval)); break;
       default: ;
       other344: ;
      }
     }
     else {
      {
       BOOL f_L531_ = TRUE;
       /* loop index variable */
       L171 = 0;
       L41 = ((AM_ROU1916046290) am);
       L531_br=L41==NULL?0:ASIZE((AM_ROU1916046290)L41); 
       while (1) {
        if(L171>=L531_br)  goto after_loop3; 
        rL531_=L171; 
        i = rL531_;
        L551_=(AM_CALL_ARG)ARR((AM_ROU1916046290)am,i); 
        ob11 = L551_;
        l = ATTR(ob11,expr);
        if (l==NULL) {
         goto other345;
        } else
        switch (TAG(l)) {
         case AM_LOCAL_EXPR_tag:
          tp_self1 = ((AM_LOCAL_EXPR) l);
          ret_val9 = ATTR(tp_self1,tp_at);
          L56 = ret_val9;
          L57 = (*dTP_is1334578382[TAG(L56)])(L56);
          L581_=!(L57); 
          if (L581_) {
           ox1 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l));
           L59 = (SIG_at89227380(ATTR(((AM_ROU1916046290) am),fun))==((ARRAYINT) NULL));
           L601_=!(L59); 
           if (L601_) {
            L62 = ATTR(ox1,attr_access);
            L63 = SIG_at89227380(ATTR(((AM_ROU1916046290) am),fun));
            L641_=ARR((ARRAYINT)L63,i); 
            L65 = L641_;
            L66 = ATTR(c,loops);
            L671_=INTTIMES(L66,5); 
            L68 = L671_;
            L691_=INTPLUS(1,L68); 
            L70 = L691_;
            L721_=INTTIMES(L65,L70); 
            L73 = L721_;
            L741_=INTPLUS(L62,L73); 
            SATTR(ox1,attr_access,L741_);
            prog_self2 = self;
            ret_val10 = OPT_CO233462019;
            L75=ret_val10;
            if (ATTR(L75,opt_debug)) {
             create_self4 = ((OUT) NULL);
             ret_val11 = create_self4;
             plus_self2 = ret_val11;
             plus_s2 = ((STR) &routattraccess);
             stdout_self2 = ((FILE1) NULL);
             L77=ZALLOC(sizeof(struct FILE1_struct));
             if (L77==NULL) FATAL("Unable to allocate more memory");
             ((OB)L77)->header.tag=FILE1_tag;
             L76 = ((FILE1) L77);
             r3 = L76;
             
             SATTR(r3,fp,stdout);
             ret_val13 = r3;
             FILE_plus_STR(ret_val13, plus_s2);
             ret_val12 = plus_self2;
             plus_self3 = ret_val12;
             L79 = SIG_at89227380(ATTR(((AM_ROU1916046290) am),fun));
             L801_=ARR((ARRAYINT)L79,i); 
             L83=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
             if (L83==NULL) FATAL("Unable to allocate more memory");
             memset(L83,0,sizeof(struct INT_boxed_struct));
             ((OB)L83)->header.tag=INT_tag;
             L82 = (dSTR)((INT_boxed) L83);
             ((INT_boxed) L82)->immutable_part = L801_;
             plus_s3 = L82;
             stdout_self3 = ((FILE1) NULL);
             L85=ZALLOC(sizeof(struct FILE1_struct));
             if (L85==NULL) FATAL("Unable to allocate more memory");
             ((OB)L85)->header.tag=FILE1_tag;
             L84 = ((FILE1) L85);
             r4 = L84;
             
             SATTR(r4,fp,stdout);
             ret_val15 = r4;
             FILE_plus_dSTR(ret_val15, plus_s3);
             ret_val14 = plus_self3;
             plus_self4 = ret_val14;
             plus_s4 = ((STR) &for1);
             stdout_self4 = ((FILE1) NULL);
             L88=ZALLOC(sizeof(struct FILE1_struct));
             if (L88==NULL) FATAL("Unable to allocate more memory");
             ((OB)L88)->header.tag=FILE1_tag;
             L87 = ((FILE1) L88);
             r5 = L87;
             
             SATTR(r5,fp,stdout);
             ret_val16 = r5;
             FILE_plus_STR(ret_val16, plus_s4);
             AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l));
             create_self5 = ((OUT) NULL);
             ret_val17 = create_self5;
             plus_self5 = ret_val17;
             plus_s5 = ((STR) &name9);
             stdout_self5 = ((FILE1) NULL);
             L92=ZALLOC(sizeof(struct FILE1_struct));
             if (L92==NULL) FATAL("Unable to allocate more memory");
             ((OB)L92)->header.tag=FILE1_tag;
             L90 = ((FILE1) L92);
             r6 = L90;
             
             SATTR(r6,fp,stdout);
             ret_val18 = r6;
             FILE_plus_STR(ret_val18, plus_s5);
            }
           }
           L94 = ATTR(ob11,mode1);
           L95 = (*dMODE_814247358[TAG(L94)])(L94, ((dMODE) MODES_in_mode));
           L961_=!(L95); 
           if (L961_) {
            prog_self3 = self;
            ret_val19 = OPT_CO233462019;
            L97=ret_val19;
            if (ATTR(L97,opt_debug)) {
             create_self6 = ((OUT) NULL);
             ret_val20 = create_self6;
             plus_self6 = ret_val20;
             plus_s6 = ((STR) &is_sometimes_far);
             stdout_self6 = ((FILE1) NULL);
             L99=ZALLOC(sizeof(struct FILE1_struct));
             if (L99==NULL) FATAL("Unable to allocate more memory");
             ((OB)L99)->header.tag=FILE1_tag;
             L98 = ((FILE1) L99);
             r7 = L98;
             
             SATTR(r7,fp,stdout);
             ret_val21 = r7;
             FILE_plus_STR(ret_val21, plus_s6);
             AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l));
             create_self7 = ((OUT) NULL);
             ret_val22 = create_self7;
             plus_self7 = ret_val22;
             plus_s7 = ((STR) &name9);
             stdout_self7 = ((FILE1) NULL);
             L103=ZALLOC(sizeof(struct FILE1_struct));
             if (L103==NULL) FATAL("Unable to allocate more memory");
             ((OB)L103)->header.tag=FILE1_tag;
             L102 = ((FILE1) L103);
             r8 = L102;
             
             SATTR(r8,fp,stdout);
             ret_val23 = r8;
             FILE_plus_STR(ret_val23, plus_s7);
            }
            SATTR(ox1,is_sometimes_far,TRUE);
           }
          } break;
         default: ;
         other345: ;
        }
        L1051_=INTPLUS(L171,1); 
        L171 = L1051_;
       }
      }
      after_loop3: ;
     } break;
    case AM_ITE1809135850_tag:
     {
      BOOL f_L1061_ = TRUE;
      /* loop index variable */
      L181 = 0;
      L51 = ((AM_ITE1809135850) am);
      L1061_br=L51==NULL?0:ASIZE((AM_ITE1809135850)L51); 
      while (1) {
       if(L181>=L1061_br)  goto after_loop4; 
       rL1061_=L181; 
       i1 = rL1061_;
       L1081_=(AM_CALL_ARG)ARR((AM_ITE1809135850)am,i1); 
       ob2 = L1081_;
       l1 = ATTR(ob2,expr);
       if (l1==NULL) {
        goto other346;
       } else
       switch (TAG(l1)) {
        case AM_LOCAL_EXPR_tag:
         tp_self2 = ((AM_LOCAL_EXPR) l1);
         ret_val24 = ATTR(tp_self2,tp_at);
         L109 = ret_val24;
         L110 = (*dTP_is1334578382[TAG(L109)])(L109);
         L1111_=!(L110); 
         if (L1111_) {
          ox2 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l1));
          L112 = (SIG_at89227380(ATTR(((AM_ITE1809135850) am),fun))==((ARRAYINT) NULL));
          L1131_=!(L112); 
          if (L1131_) {
           L114 = ATTR(ox2,attr_access);
           L115 = SIG_at89227380(ATTR(((AM_ITE1809135850) am),fun));
           L1161_=ARR((ARRAYINT)L115,i1); 
           L117 = L1161_;
           L118 = ATTR(c,loops);
           L1191_=INTTIMES(L118,5); 
           L120 = L1191_;
           L1221_=INTPLUS(1,L120); 
           L123 = L1221_;
           L1241_=INTTIMES(L117,L123); 
           L125 = L1241_;
           L1261_=INTPLUS(L114,L125); 
           SATTR(ox2,attr_access,L1261_);
           prog_self4 = self;
           ret_val25 = OPT_CO233462019;
           L127=ret_val25;
           if (ATTR(L127,opt_debug)) {
            create_self8 = ((OUT) NULL);
            ret_val26 = create_self8;
            plus_self8 = ret_val26;
            plus_s8 = ((STR) &routattraccess);
            stdout_self8 = ((FILE1) NULL);
            L129=ZALLOC(sizeof(struct FILE1_struct));
            if (L129==NULL) FATAL("Unable to allocate more memory");
            ((OB)L129)->header.tag=FILE1_tag;
            L128 = ((FILE1) L129);
            r9 = L128;
            
            SATTR(r9,fp,stdout);
            ret_val28 = r9;
            FILE_plus_STR(ret_val28, plus_s8);
            ret_val27 = plus_self8;
            plus_self9 = ret_val27;
            L132 = SIG_at89227380(ATTR(((AM_ITE1809135850) am),fun));
            L1331_=ARR((ARRAYINT)L132,i1); 
            L135=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
            if (L135==NULL) FATAL("Unable to allocate more memory");
            memset(L135,0,sizeof(struct INT_boxed_struct));
            ((OB)L135)->header.tag=INT_tag;
            L134 = (dSTR)((INT_boxed) L135);
            ((INT_boxed) L134)->immutable_part = L1331_;
            plus_s9 = L134;
            stdout_self9 = ((FILE1) NULL);
            L137=ZALLOC(sizeof(struct FILE1_struct));
            if (L137==NULL) FATAL("Unable to allocate more memory");
            ((OB)L137)->header.tag=FILE1_tag;
            L136 = ((FILE1) L137);
            r10 = L136;
            
            SATTR(r10,fp,stdout);
            ret_val30 = r10;
            FILE_plus_dSTR(ret_val30, plus_s9);
            ret_val29 = plus_self9;
            plus_self10 = ret_val29;
            plus_s10 = ((STR) &for1);
            stdout_self10 = ((FILE1) NULL);
            L140=ZALLOC(sizeof(struct FILE1_struct));
            if (L140==NULL) FATAL("Unable to allocate more memory");
            ((OB)L140)->header.tag=FILE1_tag;
            L139 = ((FILE1) L140);
            r11 = L139;
            
            SATTR(r11,fp,stdout);
            ret_val31 = r11;
            FILE_plus_STR(ret_val31, plus_s10);
            AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l1));
            create_self9 = ((OUT) NULL);
            ret_val32 = create_self9;
            plus_self11 = ret_val32;
            plus_s11 = ((STR) &name9);
            stdout_self11 = ((FILE1) NULL);
            L144=ZALLOC(sizeof(struct FILE1_struct));
            if (L144==NULL) FATAL("Unable to allocate more memory");
            ((OB)L144)->header.tag=FILE1_tag;
            L143 = ((FILE1) L144);
            r12 = L143;
            
            SATTR(r12,fp,stdout);
            ret_val33 = r12;
            FILE_plus_STR(ret_val33, plus_s11);
           }
          }
          L146 = ATTR(ob2,mode1);
          L147 = (*dMODE_814247358[TAG(L146)])(L146, ((dMODE) MODES_in_mode));
          L1481_=!(L147); 
          if (L1481_) {
           SATTR(ox2,is_sometimes_far,TRUE);
           prog_self5 = self;
           ret_val34 = OPT_CO233462019;
           L149=ret_val34;
           if (ATTR(L149,opt_debug)) {
            create_self10 = ((OUT) NULL);
            ret_val35 = create_self10;
            plus_self12 = ret_val35;
            plus_s12 = ((STR) &is_sometimes_far);
            stdout_self12 = ((FILE1) NULL);
            L152=ZALLOC(sizeof(struct FILE1_struct));
            if (L152==NULL) FATAL("Unable to allocate more memory");
            ((OB)L152)->header.tag=FILE1_tag;
            L150 = ((FILE1) L152);
            r13 = L150;
            
            SATTR(r13,fp,stdout);
            ret_val36 = r13;
            FILE_plus_STR(ret_val36, plus_s12);
            AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l1));
            create_self11 = ((OUT) NULL);
            ret_val37 = create_self11;
            plus_self13 = ret_val37;
            plus_s13 = ((STR) &name9);
            stdout_self13 = ((FILE1) NULL);
            L155=ZALLOC(sizeof(struct FILE1_struct));
            if (L155==NULL) FATAL("Unable to allocate more memory");
            ((OB)L155)->header.tag=FILE1_tag;
            L154 = ((FILE1) L155);
            r14 = L154;
            
            SATTR(r14,fp,stdout);
            ret_val38 = r14;
            FILE_plus_STR(ret_val38, plus_s13);
           }
          }
         } break;
        default: ;
        other346: ;
       }
       L1571_=INTPLUS(L181,1); 
       L181 = L1571_;
      }
     }
     after_loop4: ; break;
    case AM_PREFETCH_STMT_tag:
     prog_self6 = self;
     ret_val39 = OPT_CO233462019;
     L158=ret_val39;
     if (ATTR(L158,opt_debug)) {
      create_self12 = ((OUT) NULL);
      ret_val40 = create_self12;
      plus_self14 = ret_val40;
      plus_s14 = ((STR) &workin412447767);
      stdout_self14 = ((FILE1) NULL);
      L160=ZALLOC(sizeof(struct FILE1_struct));
      if (L160==NULL) FATAL("Unable to allocate more memory");
      ((OB)L160)->header.tag=FILE1_tag;
      L159 = ((FILE1) L160);
      r15 = L159;
      
      SATTR(r15,fp,stdout);
      ret_val42 = r15;
      FILE_plus_STR(ret_val42, plus_s14);
      ret_val41 = plus_self14;
      plus_self15 = ret_val41;
      L1631_=SYSID(am); 
      L165=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L165==NULL) FATAL("Unable to allocate more memory");
      memset(L165,0,sizeof(struct INT_boxed_struct));
      ((OB)L165)->header.tag=INT_tag;
      L164 = (dSTR)((INT_boxed) L165);
      ((INT_boxed) L164)->immutable_part = L1631_;
      plus_s15 = L164;
      stdout_self15 = ((FILE1) NULL);
      L167=ZALLOC(sizeof(struct FILE1_struct));
      if (L167==NULL) FATAL("Unable to allocate more memory");
      ((OB)L167)->header.tag=FILE1_tag;
      L166 = ((FILE1) L167);
      r16 = L166;
      
      SATTR(r16,fp,stdout);
      ret_val44 = r16;
      FILE_plus_dSTR(ret_val44, plus_s15);
      ret_val43 = plus_self15;
      plus_self16 = ret_val43;
      plus_s16 = ((STR) &name65);
      stdout_self16 = ((FILE1) NULL);
      L170=ZALLOC(sizeof(struct FILE1_struct));
      if (L170==NULL) FATAL("Unable to allocate more memory");
      ((OB)L170)->header.tag=FILE1_tag;
      L169 = ((FILE1) L170);
      r17 = L169;
      
      SATTR(r17,fp,stdout);
      ret_val45 = r17;
      FILE_plus_STR(ret_val45, plus_s16);
     }
     L173 = ATTR(((AM_PREFETCH_STMT) am),src1);
     L174 = (*dAM_EXPR_tprdTP[TAG(L173)])(L173);
     L175 = (*dTP_is1334578382[TAG(L174)])(L174);
     L1761_=!(L175); 
     if (L1761_) {
      l11 = ATTR(((AM_PREFETCH_STMT) am),dest);
      if (l11==NULL) {
       goto other347;
      } else
      switch (TAG(l11)) {
       case AM_LOCAL_EXPR_tag:
        L177 = ATTR(((AM_PREFETCH_STMT) am),src1);
        L178 = (*dAM_EX1117610980[TAG(L177)])(L177);
        L1791_=!(L178); 
        if (L1791_) {
         ox3 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l11));
         L180 = ATTR(((AM_PREFETCH_STMT) am),src1);
         dpon = (*dAM_EX1656723676[TAG(L180)])(L180);
         L182 = (dpon==((FLISTA725283029) NULL));
         L1831_=!(L182); 
         if (L1831_) {
          {
           struct FLISTA2119642552_frame_struct other6_0;
FLISTA2119642552_frame noname2 = &other6_0;
           struct FLISTA2119642552_frame_struct other6_1;
FLISTA2119642552_frame noname3 = &other6_1;
           struct FLISTA2119642552_frame_struct other6_2;
FLISTA2119642552_frame noname4 = &other6_2;
           L61 = dpon;
           L71 = dpon;
           L81 = dpon;
           noname2->self = L61;
           noname2->state = 0;
           noname3->self = L71;
           noname3->state = 0;
           noname4->self = L81;
           noname4->state = 0;
           while (1) {
            prog_self7 = self;
            ret_val46 = OPT_CO233462019;
            L184=ret_val46;
            if (ATTR(L184,opt_debug)) {
             L185 = FLISTA2119642552(noname2);
             if (noname2->state == -1) {
              goto after_loop5;
             }
             d = L185;
             create_self13 = ((OUT) NULL);
             ret_val47 = create_self13;
             plus_self17 = ret_val47;
             plus_s17 = ((STR) &dependson);
             stdout_self17 = ((FILE1) NULL);
             L187=ZALLOC(sizeof(struct FILE1_struct));
             if (L187==NULL) FATAL("Unable to allocate more memory");
             ((OB)L187)->header.tag=FILE1_tag;
             L186 = ((FILE1) L187);
             r18 = L186;
             
             SATTR(r18,fp,stdout);
             ret_val48 = r18;
             FILE_plus_STR(ret_val48, plus_s17);
             AM_OUT1439666796(((AM_OUT) NULL), ((dAM) d));
             create_self14 = ((OUT) NULL);
             ret_val49 = create_self14;
             plus_self18 = ret_val49;
             plus_s18 = ((STR) &name9);
             stdout_self18 = ((FILE1) NULL);
             L190=ZALLOC(sizeof(struct FILE1_struct));
             if (L190==NULL) FATAL("Unable to allocate more memory");
             ((OB)L190)->header.tag=FILE1_tag;
             L189 = ((FILE1) L190);
             r19 = L189;
             
             SATTR(r19,fp,stdout);
             ret_val50 = r19;
             FILE_plus_STR(ret_val50, plus_s18);
            }
            m1 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l11));
            L192 = FLISTA2119642552(noname3);
            if (noname3->state == -1) {
             goto after_loop5;
            }
            m2 = FMAPAM731742676(m, L192);
            L194 = (m1==((OPT_LOCAL_EXPR) NULL));
            L1951_=!(L194); 
            if (L1951_) {
             L196 = (m2==((OPT_LOCAL_EXPR) NULL));
             L1971_=!(L196); 
             L193 = L1971_;
            } else {
             L193 = FALSE;
            }
            if (L193) {
             aset_self = dep;
             L198=FMAPAM731742676(m, ((AM_LOCAL_EXPR) l11));
             aset_i1 = ATTR(L198,index1);
             L199 = FLISTA2119642552(noname4);
             if (noname4->state == -1) {
              goto after_loop5;
             }
             L200=FMAPAM731742676(m, L199);
             aset_i2 = ATTR(L200,index1);
             aset_val = TRUE;
             L202 = ATTR(aset_self,size2);
             L2031_=INTTIMES(aset_i1,L202); 
             L204 = L2031_;
             L2051_=INTPLUS(L204,aset_i2); 
             L206 = L2051_;
             SARR((ARRAY2BOOL)aset_self,L206,aset_val); 
             ;
            }
           }
          }
          after_loop5: ;
         }
         else {
          prog_self8 = self;
          ret_val51 = OPT_CO233462019;
          L208=ret_val51;
          if (ATTR(L208,opt_debug)) {
           create_self15 = ((OUT) NULL);
           ret_val52 = create_self15;
           plus_self19 = ret_val52;
           plus_s19 = ((STR) &sometimesfarfor);
           stdout_self19 = ((FILE1) NULL);
           L210=ZALLOC(sizeof(struct FILE1_struct));
           if (L210==NULL) FATAL("Unable to allocate more memory");
           ((OB)L210)->header.tag=FILE1_tag;
           L209 = ((FILE1) L210);
           r20 = L209;
           
           SATTR(r20,fp,stdout);
           ret_val53 = r20;
           FILE_plus_STR(ret_val53, plus_s19);
           AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l11));
           create_self16 = ((OUT) NULL);
           ret_val54 = create_self16;
           plus_self20 = ret_val54;
           plus_s20 = ((STR) &name9);
           stdout_self20 = ((FILE1) NULL);
           L213=ZALLOC(sizeof(struct FILE1_struct));
           if (L213==NULL) FATAL("Unable to allocate more memory");
           ((OB)L213)->header.tag=FILE1_tag;
           L212 = ((FILE1) L213);
           r21 = L212;
           
           SATTR(r21,fp,stdout);
           ret_val55 = r21;
           FILE_plus_STR(ret_val55, plus_s20);
          }
          SATTR(ox3,is_sometimes_far,TRUE);
         }
        } break;
       default: ;
       other347: ;
      }
     } break;
    case AM_ASSIGN_STMT_tag:
     prog_self9 = self;
     ret_val56 = OPT_CO233462019;
     L215=ret_val56;
     if (ATTR(L215,opt_debug)) {
      create_self17 = ((OUT) NULL);
      ret_val57 = create_self17;
      plus_self21 = ret_val57;
      plus_s21 = ((STR) &workin1782648620);
      stdout_self21 = ((FILE1) NULL);
      L217=ZALLOC(sizeof(struct FILE1_struct));
      if (L217==NULL) FATAL("Unable to allocate more memory");
      ((OB)L217)->header.tag=FILE1_tag;
      L216 = ((FILE1) L217);
      r22 = L216;
      
      SATTR(r22,fp,stdout);
      ret_val59 = r22;
      FILE_plus_STR(ret_val59, plus_s21);
      ret_val58 = plus_self21;
      plus_self22 = ret_val58;
      L2191_=SYSID(am); 
      L221=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
      if (L221==NULL) FATAL("Unable to allocate more memory");
      memset(L221,0,sizeof(struct INT_boxed_struct));
      ((OB)L221)->header.tag=INT_tag;
      L220 = (dSTR)((INT_boxed) L221);
      ((INT_boxed) L220)->immutable_part = L2191_;
      plus_s22 = L220;
      stdout_self22 = ((FILE1) NULL);
      L223=ZALLOC(sizeof(struct FILE1_struct));
      if (L223==NULL) FATAL("Unable to allocate more memory");
      ((OB)L223)->header.tag=FILE1_tag;
      L222 = ((FILE1) L223);
      r23 = L222;
      
      SATTR(r23,fp,stdout);
      ret_val61 = r23;
      FILE_plus_dSTR(ret_val61, plus_s22);
      ret_val60 = plus_self22;
      plus_self23 = ret_val60;
      plus_s23 = ((STR) &name65);
      stdout_self23 = ((FILE1) NULL);
      L226=ZALLOC(sizeof(struct FILE1_struct));
      if (L226==NULL) FATAL("Unable to allocate more memory");
      ((OB)L226)->header.tag=FILE1_tag;
      L225 = ((FILE1) L226);
      r24 = L225;
      
      SATTR(r24,fp,stdout);
      ret_val62 = r24;
      FILE_plus_STR(ret_val62, plus_s23);
     }
     L228 = ATTR(((AM_ASSIGN_STMT) am),src1);
     L229 = (*dAM_EXPR_tprdTP[TAG(L228)])(L228);
     L230 = (*dTP_is1334578382[TAG(L229)])(L229);
     L2321_=!(L230); 
     if (L2321_) {
      l12 = ATTR(((AM_ASSIGN_STMT) am),dest);
      if (l12==NULL) {
       goto other348;
      } else
      switch (TAG(l12)) {
       case AM_LOCAL_EXPR_tag:
        L233 = ATTR(((AM_ASSIGN_STMT) am),src1);
        L234 = (*dAM_EX1117610980[TAG(L233)])(L233);
        L2351_=!(L234); 
        if (L2351_) {
         ox4 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l12));
         L236 = ATTR(((AM_ASSIGN_STMT) am),src1);
         dpon1 = (*dAM_EX1656723676[TAG(L236)])(L236);
         L237 = (dpon1==((FLISTA725283029) NULL));
         L2381_=!(L237); 
         if (L2381_) {
          {
           struct FLISTA2119642552_frame_struct other7_0;
FLISTA2119642552_frame noname5 = &other7_0;
           struct FLISTA2119642552_frame_struct other7_1;
FLISTA2119642552_frame noname6 = &other7_1;
           struct FLISTA2119642552_frame_struct other7_2;
FLISTA2119642552_frame noname7 = &other7_2;
           L91 = dpon1;
           L101 = dpon1;
           L121 = dpon1;
           noname5->self = L91;
           noname5->state = 0;
           noname6->self = L101;
           noname6->state = 0;
           noname7->self = L121;
           noname7->state = 0;
           while (1) {
            prog_self10 = self;
            ret_val63 = OPT_CO233462019;
            L239=ret_val63;
            if (ATTR(L239,opt_debug)) {
             L240 = FLISTA2119642552(noname5);
             if (noname5->state == -1) {
              goto after_loop6;
             }
             d11 = L240;
             create_self18 = ((OUT) NULL);
             ret_val64 = create_self18;
             plus_self24 = ret_val64;
             plus_s24 = ((STR) &dependson);
             stdout_self24 = ((FILE1) NULL);
             L243=ZALLOC(sizeof(struct FILE1_struct));
             if (L243==NULL) FATAL("Unable to allocate more memory");
             ((OB)L243)->header.tag=FILE1_tag;
             L242 = ((FILE1) L243);
             r25 = L242;
             
             SATTR(r25,fp,stdout);
             ret_val65 = r25;
             FILE_plus_STR(ret_val65, plus_s24);
             AM_OUT1439666796(((AM_OUT) NULL), ((dAM) d11));
             create_self19 = ((OUT) NULL);
             ret_val66 = create_self19;
             plus_self25 = ret_val66;
             plus_s25 = ((STR) &name9);
             stdout_self25 = ((FILE1) NULL);
             L246=ZALLOC(sizeof(struct FILE1_struct));
             if (L246==NULL) FATAL("Unable to allocate more memory");
             ((OB)L246)->header.tag=FILE1_tag;
             L245 = ((FILE1) L246);
             r26 = L245;
             
             SATTR(r26,fp,stdout);
             ret_val67 = r26;
             FILE_plus_STR(ret_val67, plus_s25);
            }
            m11 = FMAPAM731742676(m, ((AM_LOCAL_EXPR) l12));
            L248 = FLISTA2119642552(noname6);
            if (noname6->state == -1) {
             goto after_loop6;
            }
            m21 = FMAPAM731742676(m, L248);
            L250 = (m11==((OPT_LOCAL_EXPR) NULL));
            L2511_=!(L250); 
            if (L2511_) {
             L252 = (m21==((OPT_LOCAL_EXPR) NULL));
             L2531_=!(L252); 
             L249 = L2531_;
            } else {
             L249 = FALSE;
            }
            if (L249) {
             aset_self1 = dep;
             L254=FMAPAM731742676(m, ((AM_LOCAL_EXPR) l12));
             aset_i11 = ATTR(L254,index1);
             L255 = FLISTA2119642552(noname7);
             if (noname7->state == -1) {
              goto after_loop6;
             }
             L256=FMAPAM731742676(m, L255);
             aset_i21 = ATTR(L256,index1);
             aset_val1 = TRUE;
             L257 = ATTR(aset_self1,size2);
             L2581_=INTTIMES(aset_i11,L257); 
             L259 = L2581_;
             L2601_=INTPLUS(L259,aset_i21); 
             L262 = L2601_;
             SARR((ARRAY2BOOL)aset_self1,L262,aset_val1); 
             ;
            }
           }
          }
          after_loop6: ;
         }
         else {
          prog_self11 = self;
          ret_val68 = OPT_CO233462019;
          L264=ret_val68;
          if (ATTR(L264,opt_debug)) {
           create_self20 = ((OUT) NULL);
           ret_val69 = create_self20;
           plus_self26 = ret_val69;
           plus_s26 = ((STR) &sometimesfarfor);
           stdout_self26 = ((FILE1) NULL);
           L266=ZALLOC(sizeof(struct FILE1_struct));
           if (L266==NULL) FATAL("Unable to allocate more memory");
           ((OB)L266)->header.tag=FILE1_tag;
           L265 = ((FILE1) L266);
           r27 = L265;
           
           SATTR(r27,fp,stdout);
           ret_val70 = r27;
           FILE_plus_STR(ret_val70, plus_s26);
           AM_OUT1439666796(((AM_OUT) NULL), ((dAM) l12));
           create_self21 = ((OUT) NULL);
           ret_val71 = create_self21;
           plus_self27 = ret_val71;
           plus_s27 = ((STR) &name9);
           stdout_self27 = ((FILE1) NULL);
           L269=ZALLOC(sizeof(struct FILE1_struct));
           if (L269==NULL) FATAL("Unable to allocate more memory");
           ((OB)L269)->header.tag=FILE1_tag;
           L268 = ((FILE1) L269);
           r28 = L268;
           
           SATTR(r28,fp,stdout);
           ret_val72 = r28;
           FILE_plus_STR(ret_val72, plus_s27);
          }
          SATTR(ox4,is_sometimes_far,TRUE);
         }
        } break;
       default: ;
       other348: ;
      }
     } break;
    case AM_RETURN_STMT_tag:
     L271 = (ATTR(((AM_RETURN_STMT) am),val1)==((dAM_EXPR) NULL));
     L2721_=!(L271); 
     if (L2721_) {
      L273 = ATTR(((AM_RETURN_STMT) am),val1);
      dpon2 = (*dAM_EX1656723676[TAG(L273)])(L273);
      L275 = ATTR(((AM_RETURN_STMT) am),val1);
      L276 = (*dAM_EX1117610980[TAG(L275)])(L275);
      L2771_=!(L276); 
      if (L2771_) {
       L274 = (dpon2==((FLISTA725283029) NULL));
      } else {
       L274 = FALSE;
      }
      if (L274) {
       SATTR(ret_ox,is_sometimes_far,TRUE);
       prog_self12 = self;
       ret_val73 = OPT_CO233462019;
       L278=ret_val73;
       if (ATTR(L278,opt_debug)) {
        create_self22 = ((OUT) NULL);
        ret_val74 = create_self22;
        plus_self28 = ret_val74;
        plus_s28 = ((STR) &return463650331);
        stdout_self28 = ((FILE1) NULL);
        L280=ZALLOC(sizeof(struct FILE1_struct));
        if (L280==NULL) FATAL("Unable to allocate more memory");
        ((OB)L280)->header.tag=FILE1_tag;
        L279 = ((FILE1) L280);
        r29 = L279;
        
        SATTR(r29,fp,stdout);
        ret_val75 = r29;
        FILE_plus_STR(ret_val75, plus_s28);
       }
      }
      else {
       L282 = (dpon2==((FLISTA725283029) NULL));
       L2831_=!(L282); 
       if (L2831_) {
        {
         struct FLISTA2119642552_frame_struct other8_0;
FLISTA2119642552_frame noname8 = &other8_0;
         struct FLISTA2119642552_frame_struct other8_1;
FLISTA2119642552_frame noname9 = &other8_1;
         L131 = dpon2;
         L141 = dpon2;
         noname8->self = L131;
         noname8->state = 0;
         noname9->self = L141;
         noname9->state = 0;
         while (1) {
          prog_self13 = self;
          ret_val76 = OPT_CO233462019;
          L284=ret_val76;
          if (ATTR(L284,opt_debug)) {
           L285 = FLISTA2119642552(noname8);
           if (noname8->state == -1) {
            goto after_loop7;
           }
           d21 = L285;
           create_self23 = ((OUT) NULL);
           ret_val77 = create_self23;
           plus_self29 = ret_val77;
           plus_s29 = ((STR) &returndependson);
           stdout_self29 = ((FILE1) NULL);
           L287=ZALLOC(sizeof(struct FILE1_struct));
           if (L287==NULL) FATAL("Unable to allocate more memory");
           ((OB)L287)->header.tag=FILE1_tag;
           L286 = ((FILE1) L287);
           r30 = L286;
           
           SATTR(r30,fp,stdout);
           ret_val78 = r30;
           FILE_plus_STR(ret_val78, plus_s29);
           AM_OUT1439666796(((AM_OUT) NULL), ((dAM) d21));
           create_self24 = ((OUT) NULL);
           ret_val79 = create_self24;
           plus_self30 = ret_val79;
           plus_s30 = ((STR) &name9);
           stdout_self30 = ((FILE1) NULL);
           L290=ZALLOC(sizeof(struct FILE1_struct));
           if (L290==NULL) FATAL("Unable to allocate more memory");
           ((OB)L290)->header.tag=FILE1_tag;
           L289 = ((FILE1) L290);
           r31 = L289;
           
           SATTR(r31,fp,stdout);
           ret_val80 = r31;
           FILE_plus_STR(ret_val80, plus_s30);
          }
          aset_self2 = dep;
          aset_i12 = ATTR(ret_ox,index1);
          L292 = FLISTA2119642552(noname9);
          if (noname9->state == -1) {
           goto after_loop7;
          }
          L293=FMAPAM731742676(m, L292);
          aset_i22 = ATTR(L293,index1);
          aset_val2 = TRUE;
          L294 = ATTR(aset_self2,size2);
          L2951_=INTTIMES(aset_i12,L294); 
          L296 = L2951_;
          L2971_=INTPLUS(L296,aset_i22); 
          L298 = L2971_;
          SARR((ARRAY2BOOL)aset_self2,L298,aset_val2); 
          ;
         }
        }
        after_loop7: ;
       }
      }
     } break;
    default: ;
    other341: ;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void OPT_LO447032529(OPT_LOCAL self, AM_ROUT_DEF func, ARRAY2BOOL dep, FMAPAM418009930 m, ARRAYO570594127 ox_table) {
 ARRAYINT consider;
 INT nlocals = INT_zero;
 AM_ROUT_DEF L11;
 INT i = INT_zero;
 OPT_LOCAL_EXPR ox;
 BOOL ignore1 = BOOL_zero;
 INT L21 = INT_zero;
 INT j = INT_zero;
 OPT_LOCAL_CALL oi;
 ARRAYINT L31;
 ARRAYINT L41;
 AREFFL1698233817 L51;
 INT i1 = INT_zero;
 ARRAYINT L61;
 OPT_LOCAL_EXPR a;
 INT L71 = INT_zero;
 ARRAYO570594127 L81;
 OPT_LOCAL_EXPR ox1;
 BOOL local_is_near = BOOL_zero;
 INT L91 = INT_zero;
 INT j11 = INT_zero;
 INT L101 = INT_zero;
 INT L121 = INT_zero;
 FLISTA725283029 L131;
 ARRAYINT create_self;
 INT create_n = INT_zero;
 ARRAYINT ret_val;
 INT L141 = INT_zero;
 OPT_LOCAL cs_options_self;
 CS_OPTIONS ret_val1;
 ARRAY2BOOL aget_self;
 INT aget_i1 = INT_zero;
 INT aget_i2 = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 ARRAYINT create_self1;
 INT create_n1 = INT_zero;
 ARRAYINT ret_val3;
 INT L151 = INT_zero;
 AREFFL1698233817 create_self2;
 INT create_n2 = INT_zero;
 AREFFL1698233817 ret_val4;
 OPT_LOCAL prog_self;
 PROG ret_val5;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self;
 STR plus_s;
 OUT ret_val7;
 FILE1 stdout_self;
 FILE1 ret_val8;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val9;
 FILE1 stdout_self1;
 FILE1 ret_val10;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val11;
 FILE1 r2;
 INT L161 = INT_zero;
 INT L171 = INT_zero;
 INT aeltb_self = INT_zero;
 BOOL ret_val12 = BOOL_zero;
 INT L181 = INT_zero;
 BOOL aeltb_if_first = BOOL_zero;
 OPT_LOCAL prog_self1;
 PROG ret_val13;
 OUT create_self4;
 OUT ret_val14;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val15;
 FILE1 r3;
 OUT create_self5;
 OUT ret_val16;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val17;
 FILE1 r4;
 INT L191 = INT_zero;
 OPT_LOCAL prog_self2;
 PROG ret_val18;
 OUT create_self6;
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
 OUT ret_val24;
 FILE1 stdout_self7;
 FILE1 ret_val25;
 FILE1 r7;
 ARRAY2BOOL aget_self1;
 INT aget_i11 = INT_zero;
 INT aget_i21 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 OUT plus_self8;
 dSTR plus_s8;
 OUT ret_val27;
 FILE1 stdout_self8;
 FILE1 ret_val28;
 FILE1 r8;
 OUT plus_self9;
 STR plus_s9;
 OUT ret_val29;
 FILE1 stdout_self9;
 FILE1 ret_val30;
 FILE1 r9;
 OUT plus_self10;
 dSTR plus_s10;
 OUT ret_val31;
 FILE1 stdout_self10;
 FILE1 ret_val32;
 FILE1 r10;
 OUT plus_self11;
 STR plus_s11;
 OUT ret_val33;
 FILE1 stdout_self11;
 FILE1 ret_val34;
 FILE1 r11;
 OUT plus_self12;
 dSTR plus_s12;
 OUT ret_val35;
 FILE1 stdout_self12;
 FILE1 ret_val36;
 FILE1 r12;
 OUT plus_self13;
 STR plus_s13;
 OUT ret_val37;
 FILE1 stdout_self13;
 FILE1 ret_val38;
 FILE1 r13;
 OUT plus_self14;
 dSTR plus_s14;
 OUT ret_val39;
 FILE1 stdout_self14;
 FILE1 ret_val40;
 FILE1 r14;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val41;
 FILE1 r15;
 ARRAY2BOOL aget_self2;
 INT aget_i12 = INT_zero;
 INT aget_i22 = INT_zero;
 BOOL ret_val42 = BOOL_zero;
 ARRAY2BOOL aget_self3;
 INT aget_i13 = INT_zero;
 INT aget_i23 = INT_zero;
 BOOL ret_val43 = BOOL_zero;
 OPT_LOCAL prog_self3;
 PROG ret_val44;
 OUT create_self7;
 OUT ret_val45;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val46;
 FILE1 r16;
 OUT create_self8;
 OUT ret_val47;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val48;
 FILE1 r17;
 OPT_LOCAL prog_self4;
 PROG ret_val49;
 OUT create_self9;
 OUT ret_val50;
 OUT plus_self18;
 STR plus_s18;
 FILE1 stdout_self18;
 FILE1 ret_val51;
 FILE1 r18;
 INT L201 = INT_zero;
 OUT create_self10;
 OUT ret_val52;
 INT aeltb_self1 = INT_zero;
 BOOL ret_val53 = BOOL_zero;
 INT L221 = INT_zero;
 BOOL aeltb_if_first1 = BOOL_zero;
 OUT plus_self19;
 dSTR plus_s19;
 OUT ret_val54;
 FILE1 stdout_self19;
 FILE1 ret_val55;
 FILE1 r19;
 OUT plus_self20;
 STR plus_s20;
 FILE1 stdout_self20;
 FILE1 ret_val56;
 FILE1 r20;
 OUT create_self11;
 OUT ret_val57;
 OUT plus_self21;
 STR plus_s21;
 FILE1 stdout_self21;
 FILE1 ret_val58;
 FILE1 r21;
 OUT create_self12;
 OUT ret_val59;
 OUT plus_self22;
 STR plus_s22;
 FILE1 stdout_self22;
 FILE1 ret_val60;
 FILE1 r22;
 OUT create_self13;
 OUT ret_val61;
 OUT plus_self23;
 STR plus_s23;
 FILE1 stdout_self23;
 FILE1 ret_val62;
 FILE1 r23;
 OUT create_self14;
 OUT ret_val63;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val64;
 FILE1 r24;
 INT L231_;
 ARRAYINT L24;
 INT L25;
 OB L26;
 INT L27;
 INT L281_br;
 INT rL281_;
 INT L29;
 AM_FORMAL_ARG L301_;
 AM_FORMAL_ARG L32;
 BOOL L33;
 BOOL L34;
 BOOL L35;
 AM_FORMAL_ARG L361_;
 AM_FORMAL_ARG L37;
 dMODE L38;
 BOOL L39;
 BOOL L401_;
 ARRAYINT L42;
 INT L431_;
 INT L44;
 CS_OPTIONS L45;
 INT L46;
 BOOL L471_;
INT i_L481_=0;
 INT aL481_;
 INT L49;
 BOOL L50;
 BOOL L52;
 INT L53;
 INT L541_;
 INT L55;
 INT L561_;
 INT L57;
 BOOL L581_;
 INT L59;
 BOOL L601_;
 BOOL L62;
 BOOL L631_;
 OPT_LOCAL_EXPR L641_;
 OPT_LOCAL_EXPR L65;
 BOOL L66;
 BOOL L671_;
 BOOL L681_;
 INT L701_;
 INT L721_;
 BOOL L731_;
 ARRAYINT L74;
 INT L75;
 OB L76;
 INT L77;
 INT L781_br;
 INT aL781_;
 INT L791_br;
 INT L80;
 INT L82;
 INT L831_;
 ARRAYINT L84;
 INT L851_;
 AREFFL1698233817 L86;
 INT L87;
 OB L88;
 INT L89;
 PROG L90;
 extern STR making525906926;
 FILE1 L92;
 OB L93;
 FILE1 L95;
 OB L96;
 extern STR arelocal;
 FILE1 L98;
 OB L99;
 INT L1021_br;
 INT rL1021_;
 INT L103;
 INT L1041_br;
 INT aL1041_;
 INT aL1051_;
 INT L106;
 INT L107;
 AM_FORMAL_ARG L1081_;
 AM_FORMAL_ARG L109;
 INT L110;
 INT L111;
 BOOL L1121_;
 AREFFL1698233817 L113;
 AREFFL1698233817 L114;
 FLISTA725283029 L1151_;
 FLISTA725283029 L116;
 FLISTA725283029 L117;
 PROG L119;
 extern STR localargument;
 FILE1 L120;
 OB L122;
 extern STR name9;
 FILE1 L124;
 OB L125;
 INT L1271_;
 INT L1281_br;
 OPT_LOCAL_EXPR aL1281_;
 OPT_LOCAL_EXPR L129;
 BOOL L130;
 BOOL L1321_;
 BOOL L133;
 BOOL L134;
 BOOL L135;
 BOOL L1361_;
 BOOL L137;
 BOOL L1381_;
 BOOL L139;
 BOOL L1401_;
 INT L143;
INT i_L1421_=0;
 INT aL1421_;
 PROG L144;
 extern STR comparingagainst;
 FILE1 L145;
 OB L146;
 dSTR L148;
 OB L149;
 FILE1 L150;
 OB L152;
 extern STR dep1;
 FILE1 L154;
 OB L155;
 INT L157;
 INT L1581_;
 INT L159;
 INT L1601_;
 INT L162;
 BOOL L1631_;
 dSTR L164;
 OB L165;
 FILE1 L166;
 OB L167;
 extern STR sometimes_far;
 FILE1 L169;
 OB L170;
 OPT_LOCAL_EXPR L1731_;
 OPT_LOCAL_EXPR L174;
 dSTR L175;
 OB L176;
 FILE1 L177;
 OB L178;
 extern STR is_arg;
 FILE1 L180;
 OB L182;
 OPT_LOCAL_EXPR L1841_;
 OPT_LOCAL_EXPR L185;
 dSTR L186;
 OB L187;
 FILE1 L188;
 OB L189;
 extern STR near_arg;
 FILE1 L192;
 OB L193;
 OPT_LOCAL_EXPR L1951_;
 OPT_LOCAL_EXPR L196;
 dSTR L197;
 OB L198;
 FILE1 L199;
 OB L200;
 extern STR name9;
 FILE1 L203;
 OB L204;
 BOOL L206;
 BOOL L207;
 BOOL L208;
 INT L209;
 BOOL L2101_;
 INT L211;
 INT L2121_;
 INT L213;
 INT L2141_;
 INT L215;
 BOOL L2161_;
 BOOL L217;
 BOOL L2181_;
 BOOL L219;
 BOOL L220;
 INT L222;
 INT L2231_;
 INT L224;
 INT L2251_;
 INT L226;
 BOOL L2271_;
 OPT_LOCAL_EXPR L2281_;
 OPT_LOCAL_EXPR L229;
 BOOL L230;
 BOOL L2321_;
 OPT_LOCAL_EXPR L2331_;
 OPT_LOCAL_EXPR L234;
 BOOL L235;
 BOOL L2361_;
 BOOL L237;
 OPT_LOCAL_EXPR L2381_;
 OPT_LOCAL_EXPR L239;
 OPT_LOCAL_EXPR L2401_;
 OPT_LOCAL_EXPR L241;
 AREFFL1698233817 L242;
 AREFFL1698233817 L243;
 FLISTA725283029 L2441_;
 FLISTA725283029 L245;
 FLISTA725283029 L246;
 PROG L248;
 extern STR local1;
 FILE1 L249;
 OB L250;
 extern STR name9;
 FILE1 L252;
 OB L253;
 INT L2551_;
 PROG L256;
 extern STR localargs;
 FILE1 L257;
 OB L258;
 ARRAYINT L262;
 INT L2631_;
 INT aL2611_;
 INT L264;
 INT L265;
 BOOL L2661_;
 dSTR L267;
 OB L268;
 FILE1 L269;
 OB L270;
 extern STR name10;
 FILE1 L272;
 OB L273;
 INT L2751_;
 extern STR name9;
 FILE1 L276;
 OB L277;
 extern STR locals2;
 FILE1 L279;
 OB L280;
 AM_LOCAL_EXPR L283;
 AREFFL1698233817 L284;
 FLISTA725283029 L2851_;
 extern STR name67;
 FILE1 L286;
 OB L287;
 extern STR name9;
 FILE1 L289;
 OB L290;
 INT L2921_;
 create_self = ((ARRAYINT) NULL);
 L231_=ASIZE((AM_ROUT_DEF)func); 
 create_n = L231_;
 L25 = create_n;
 L27=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L25)*sizeof(INT);
 L26=ZALLOC_LEAF_BIG(L27);
 if (L26==NULL) FATAL("Unable to allocate more memory");
 memset(L26,0,L27);
 ((OB)L26)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

   ((OB)L26)->header.destroyed=0;
#endif

 L24 = ((ARRAYINT) L26);
 L24->asize = L25;
 ret_val = L24;
 consider = ret_val;
 nlocals = 0;
 {
  BOOL f_L281_ = TRUE;
  /* loop index variable */
  L141 = 0;
  L11 = func;
  L281_br=L11==NULL?0:ASIZE((AM_ROUT_DEF)L11); 
  while (1) {
   if(L141>=L281_br)  goto after_loop; 
   rL281_=L141; 
   i = rL281_;
   L301_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,i); 
   L32=L301_;
   ox = FMAPAM731742676(m, ATTR(L32,expr));
   if ((ox==((OPT_LOCAL_EXPR) NULL))) {
    L35 = TRUE;
   } else {
    L361_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,i); 
    L37=L361_;
    L38 = ATTR(L37,mode1);
    L39 = (*dMODE_814247358[TAG(L38)])(L38, ((dMODE) MODES_in_mode));
    L401_=!(L39); 
    L35 = L401_;
   }
   if (L35) {
    L34 = TRUE;
   } else {
    L34 = (SIG_at89227380(ATTR(func,sig1))==((ARRAYINT) NULL));
   }
   if (L34) {
    L33 = TRUE;
   } else {
    L42 = SIG_at89227380(ATTR(func,sig1));
    L431_=ARR((ARRAYINT)L42,i); 
    L44 = L431_;
    cs_options_self = self;
    ret_val1 = OPT_CO1299251581;
    L45=ret_val1;
    L46 = ATTR(L45,local_call_acces);
    L471_=(L44)<(L46); 
    L33 = L471_;
   }
   ignore1 = L33;
   {
    BOOL f_L481_ = TRUE;
    L21 = ATTRs(dep,size2,INT_zero);
    i_L481_=0;
    while (1) {
     if (ignore1) {
      goto after_loop1;
     }
     if(i_L481_>=L21)  goto after_loop1; 
     aL481_=i_L481_; i_L481_++;
     j = aL481_;
     aget_self = dep;
     aget_i1 = ATTR(ox,index1);
     aget_i2 = j;
     L53 = ATTR(aget_self,size2);
     L541_=INTTIMES(aget_i1,L53); 
     L55 = L541_;
     L561_=INTPLUS(L55,aget_i2); 
     L57 = L561_;
     L581_=ARR((ARRAY2BOOL)aget_self,L57); 
     ret_val2 = L581_;
     if (ret_val2) {
      L59 = ATTR(ox,index1);
      L601_=(L59)==(j); 
      L62 = L601_;
      L631_=!(L62); 
      L52 = L631_;
     } else {
      L52 = FALSE;
     }
     if (L52) {
      L641_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j); 
      L65=L641_;
      L66 = ATTR(L65,is_sometimes_far);
      L671_=!(L66); 
      L50 = L671_;
     } else {
      L50 = FALSE;
     }
     ignore1 = L50;
    }
   }
   after_loop1: ;
   L681_=!(ignore1); 
   if (L681_) {
    SARR((ARRAYINT)consider,nlocals,i); 
    ;
    L701_=INTPLUS(nlocals,1); 
    nlocals = L701_;
   }
   L721_=INTPLUS(L141,1); 
   L141 = L721_;
  }
 }
 after_loop: ;
 L731_=(nlocals)==(0); 
 if (L731_) {
  return;
 }
 oi = ATTR(ATTR(func,sig1),opt_info);
 create_self1 = ((ARRAYINT) NULL);
 create_n1 = nlocals;
 L75 = create_n1;
 L77=(sizeof(struct ARRAYINT_struct)+1-sizeof(INT))+(L75)*sizeof(INT);
 L76=ZALLOC_LEAF_BIG(L77);
 if (L76==NULL) FATAL("Unable to allocate more memory");
 memset(L76,0,L77);
 ((OB)L76)->header.tag=ARRAYINT_tag;
#ifdef DESTROY_CHK

   ((OB)L76)->header.destroyed=0;
#endif

 L74 = ((ARRAYINT) L76);
 L74->asize = L75;
 ret_val3 = L74;
 SATTR(oi,consider,ret_val3);
 {
  BOOL f_L781_ = TRUE;
  BOOL f_L791_ = TRUE;
  /* loop index variable */
  L151 = 0;
  L31 = consider;
  L41 = ATTRs(oi,consider,((ARRAYINT) NULL));
  L781_br=L31==NULL?0:ASIZE((ARRAYINT)L31); 
  L791_br=L41==NULL?0:ASIZE((ARRAYINT)L41); 
  while (1) {
   if(L151>=L791_br)  goto after_loop2; 
   if(L151>=L781_br)  goto after_loop2; 
   aL781_=ARR((ARRAYINT)L31,L151); 
   L82 = aL781_;
   SARR((ARRAYINT)L41,L151,L82); 
   ;
   L831_=INTPLUS(L151,1); 
   L151 = L831_;
  }
 }
 after_loop2: ;
 create_self2 = ((AREFFL1698233817) NULL);
 L84 = ATTR(oi,consider);
 L851_=(L84)==NULL?0:ASIZE((ARRAYINT)L84); 
 create_n2 = INT_pow_INTrINT(2, L851_);
 L87 = create_n2;
 L89=(sizeof(struct AREFFL1698233817_struct)-sizeof(FLISTA725283029))+(L87)*sizeof(FLISTA725283029);
 L88=ZALLOC_BIG(L89);
 if (L88==NULL) FATAL("Unable to allocate more memory");
 ((OB)L88)->header.tag=AREFFL1698233817_tag;
#ifdef DESTROY_CHK

   ((OB)L88)->header.destroyed=0;
#endif

 L86 = ((AREFFL1698233817) L88);
 L86->asize = L87;
 ret_val4 = L86;
 SATTR(oi,near_local,ret_val4);
 prog_self = self;
 ret_val5 = OPT_CO233462019;
 L90=ret_val5;
 if (ATTR(L90,opt_debug)) {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self = ret_val6;
  plus_s = ((STR) &making525906926);
  stdout_self = ((FILE1) NULL);
  L93=ZALLOC(sizeof(struct FILE1_struct));
  if (L93==NULL) FATAL("Unable to allocate more memory");
  ((OB)L93)->header.tag=FILE1_tag;
  L92 = ((FILE1) L93);
  r = L92;
  
  SATTR(r,fp,stdout);
  ret_val8 = r;
  FILE_plus_STR(ret_val8, plus_s);
  ret_val7 = plus_self;
  plus_self1 = ret_val7;
  plus_s1 = ARRAYINT_strrSTR(ATTR(oi,consider));
  stdout_self1 = ((FILE1) NULL);
  L96=ZALLOC(sizeof(struct FILE1_struct));
  if (L96==NULL) FATAL("Unable to allocate more memory");
  ((OB)L96)->header.tag=FILE1_tag;
  L95 = ((FILE1) L96);
  r1 = L95;
  
  SATTR(r1,fp,stdout);
  ret_val10 = r1;
  FILE_plus_STR(ret_val10, plus_s1);
  ret_val9 = plus_self1;
  plus_self2 = ret_val9;
  plus_s2 = ((STR) &arelocal);
  stdout_self2 = ((FILE1) NULL);
  L99=ZALLOC(sizeof(struct FILE1_struct));
  if (L99==NULL) FATAL("Unable to allocate more memory");
  ((OB)L99)->header.tag=FILE1_tag;
  L98 = ((FILE1) L99);
  r2 = L98;
  
  SATTR(r2,fp,stdout);
  ret_val11 = r2;
  FILE_plus_STR(ret_val11, plus_s2);
 }
 {
  BOOL f_L1021_ = TRUE;
  /* loop index variable */
  L161 = 0;
  L51 = ATTR(oi,near_local);
  L1021_br=ASIZE((AREFFL1698233817)L51); 
  while (1) {
   if(L161>=L1021_br)  goto after_loop3; 
   rL1021_=L161; 
   i1 = rL1021_;
   {
    BOOL f_L1041_ = TRUE;
    BOOL f_L1051_ = TRUE;
    /* loop index variable */
    L171 = 0;
    L61 = ATTR(oi,consider);
    /* Initialization of inlined iter INT::aelt!:BOOL */
    L71 = i1;
    aeltb_self = L71;
    L181 = INT_asize;
    L1041_br=L61==NULL?0:ASIZE((ARRAYINT)L61); 
    aeltb_if_first = TRUE;
    while (1) {
     if(L171>=L1041_br)  goto after_loop4; 
     aL1041_=ARR((ARRAYINT)L61,L171); 
     L107 = aL1041_;
     L1081_=(AM_FORMAL_ARG)ARR((AM_ROUT_DEF)func,L107); 
     L109=L1081_;
     a = FMAPAM731742676(m, ATTR(L109,expr));
     if(L171>=L181)  goto after_loop4; 
     aL1051_=L171; 
     L111 = aL1051_;
     L1121_=((CHAR)((aeltb_self&(1<<L111))!=0)); 
     if (L1121_) {
      SATTR(a,near_arg,TRUE);
      L113 = ATTR(oi,near_local);
      L114 = ATTR(oi,near_local);
      L1151_=(FLISTA725283029)ARR((AREFFL1698233817)L114,i1); 
      L116 = L1151_;
      L117 = FLISTA1817671564(L116, ATTR(a,local1));
      SARR((AREFFL1698233817)L113,i1,(FLISTA725283029)L117); 
      ;
      prog_self1 = self;
      ret_val13 = OPT_CO233462019;
      L119=ret_val13;
      if (ATTR(L119,opt_debug)) {
       create_self4 = ((OUT) NULL);
       ret_val14 = create_self4;
       plus_self3 = ret_val14;
       plus_s3 = ((STR) &localargument);
       stdout_self3 = ((FILE1) NULL);
       L122=ZALLOC(sizeof(struct FILE1_struct));
       if (L122==NULL) FATAL("Unable to allocate more memory");
       ((OB)L122)->header.tag=FILE1_tag;
       L120 = ((FILE1) L122);
       r3 = L120;
       
       SATTR(r3,fp,stdout);
       ret_val15 = r3;
       FILE_plus_STR(ret_val15, plus_s3);
       AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(a,local1)));
       create_self5 = ((OUT) NULL);
       ret_val16 = create_self5;
       plus_self4 = ret_val16;
       plus_s4 = ((STR) &name9);
       stdout_self4 = ((FILE1) NULL);
       L125=ZALLOC(sizeof(struct FILE1_struct));
       if (L125==NULL) FATAL("Unable to allocate more memory");
       ((OB)L125)->header.tag=FILE1_tag;
       L124 = ((FILE1) L125);
       r4 = L124;
       
       SATTR(r4,fp,stdout);
       ret_val17 = r4;
       FILE_plus_STR(ret_val17, plus_s4);
      }
     }
     else {
      SATTR(a,near_arg,FALSE);
     }
     L1271_=INTPLUS(L171,1); 
     L171 = L1271_;
    }
   }
   after_loop4: ;
   {
    BOOL f_L1281_ = TRUE;
    /* loop index variable */
    L191 = 0;
    L81 = ox_table;
    L1281_br=L81==NULL?0:ASIZE((ARRAYO570594127)L81); 
    while (1) {
     if(L191>=L1281_br)  goto after_loop5; 
     aL1281_=ARR((ARRAYO570594127)L81,L191); 
     ox1 = aL1281_;
     L130 = (ox1==((OPT_LOCAL_EXPR) NULL));
     L1321_=!(L130); 
     if (L1321_) {
      L135 = ATTR(ox1,is_sometimes_far);
      L1361_=!(L135); 
      if (L1361_) {
       L137 = ATTR(ox1,is_arg);
       L1381_=!(L137); 
       L134 = L1381_;
      } else {
       L134 = FALSE;
      }
      if (L134) {
       L139 = (ATTR(ox1,local1)==((AM_LOCAL_EXPR) NULL));
       L1401_=!(L139); 
       L133 = L1401_;
      } else {
       L133 = FALSE;
      }
      local_is_near = L133;
      {
       BOOL f_L1421_ = TRUE;
       while (1) {
        if (local_is_near) {
        }
        else {
         goto after_loop6;
        }
        if (f_L1421_) {
         f_L1421_ = FALSE;
         L91 = ATTR(dep,size2);
         i_L1421_=0;
        }
        if(i_L1421_>=L91)  goto after_loop6; 
        aL1421_=i_L1421_; i_L1421_++;
        j11 = aL1421_;
        prog_self2 = self;
        ret_val18 = OPT_CO233462019;
        L144=ret_val18;
        if (ATTR(L144,opt_debug)) {
         create_self6 = ((OUT) NULL);
         ret_val19 = create_self6;
         plus_self5 = ret_val19;
         plus_s5 = ((STR) &comparingagainst);
         stdout_self5 = ((FILE1) NULL);
         L146=ZALLOC(sizeof(struct FILE1_struct));
         if (L146==NULL) FATAL("Unable to allocate more memory");
         ((OB)L146)->header.tag=FILE1_tag;
         L145 = ((FILE1) L146);
         r5 = L145;
         
         SATTR(r5,fp,stdout);
         ret_val21 = r5;
         FILE_plus_STR(ret_val21, plus_s5);
         ret_val20 = plus_self5;
         plus_self6 = ret_val20;
         L149=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
         if (L149==NULL) FATAL("Unable to allocate more memory");
         memset(L149,0,sizeof(struct INT_boxed_struct));
         ((OB)L149)->header.tag=INT_tag;
         L148 = (dSTR)((INT_boxed) L149);
         ((INT_boxed) L148)->immutable_part = j11;
         plus_s6 = L148;
         stdout_self6 = ((FILE1) NULL);
         L152=ZALLOC(sizeof(struct FILE1_struct));
         if (L152==NULL) FATAL("Unable to allocate more memory");
         ((OB)L152)->header.tag=FILE1_tag;
         L150 = ((FILE1) L152);
         r6 = L150;
         
         SATTR(r6,fp,stdout);
         ret_val23 = r6;
         FILE_plus_dSTR(ret_val23, plus_s6);
         ret_val22 = plus_self6;
         plus_self7 = ret_val22;
         plus_s7 = ((STR) &dep1);
         stdout_self7 = ((FILE1) NULL);
         L155=ZALLOC(sizeof(struct FILE1_struct));
         if (L155==NULL) FATAL("Unable to allocate more memory");
         ((OB)L155)->header.tag=FILE1_tag;
         L154 = ((FILE1) L155);
         r7 = L154;
         
         SATTR(r7,fp,stdout);
         ret_val25 = r7;
         FILE_plus_STR(ret_val25, plus_s7);
         ret_val24 = plus_self7;
         plus_self8 = ret_val24;
         aget_self1 = dep;
         aget_i11 = ATTR(ox1,index1);
         aget_i21 = j11;
         L157 = ATTR(aget_self1,size2);
         L1581_=INTTIMES(aget_i11,L157); 
         L159 = L1581_;
         L1601_=INTPLUS(L159,aget_i21); 
         L162 = L1601_;
         L1631_=ARR((ARRAY2BOOL)aget_self1,L162); 
         ret_val26 = L1631_;
         L165=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
         if (L165==NULL) FATAL("Unable to allocate more memory");
         memset(L165,0,sizeof(struct BOOL_boxed_struct));
         ((OB)L165)->header.tag=BOOL_tag;
         L164 = (dSTR)((BOOL_boxed) L165);
         ((BOOL_boxed) L164)->immutable_part = ret_val26;
         plus_s8 = L164;
         stdout_self8 = ((FILE1) NULL);
         L167=ZALLOC(sizeof(struct FILE1_struct));
         if (L167==NULL) FATAL("Unable to allocate more memory");
         ((OB)L167)->header.tag=FILE1_tag;
         L166 = ((FILE1) L167);
         r8 = L166;
         
         SATTR(r8,fp,stdout);
         ret_val28 = r8;
         FILE_plus_dSTR(ret_val28, plus_s8);
         ret_val27 = plus_self8;
         plus_self9 = ret_val27;
         plus_s9 = ((STR) &sometimes_far);
         stdout_self9 = ((FILE1) NULL);
         L170=ZALLOC(sizeof(struct FILE1_struct));
         if (L170==NULL) FATAL("Unable to allocate more memory");
         ((OB)L170)->header.tag=FILE1_tag;
         L169 = ((FILE1) L170);
         r9 = L169;
         
         SATTR(r9,fp,stdout);
         ret_val30 = r9;
         FILE_plus_STR(ret_val30, plus_s9);
         ret_val29 = plus_self9;
         plus_self10 = ret_val29;
         L1731_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
         L174=L1731_;
         L176=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
         if (L176==NULL) FATAL("Unable to allocate more memory");
         memset(L176,0,sizeof(struct BOOL_boxed_struct));
         ((OB)L176)->header.tag=BOOL_tag;
         L175 = (dSTR)((BOOL_boxed) L176);
         ((BOOL_boxed) L175)->immutable_part = ATTR(L174,is_sometimes_far);
         plus_s10 = L175;
         stdout_self10 = ((FILE1) NULL);
         L178=ZALLOC(sizeof(struct FILE1_struct));
         if (L178==NULL) FATAL("Unable to allocate more memory");
         ((OB)L178)->header.tag=FILE1_tag;
         L177 = ((FILE1) L178);
         r10 = L177;
         
         SATTR(r10,fp,stdout);
         ret_val32 = r10;
         FILE_plus_dSTR(ret_val32, plus_s10);
         ret_val31 = plus_self10;
         plus_self11 = ret_val31;
         plus_s11 = ((STR) &is_arg);
         stdout_self11 = ((FILE1) NULL);
         L182=ZALLOC(sizeof(struct FILE1_struct));
         if (L182==NULL) FATAL("Unable to allocate more memory");
         ((OB)L182)->header.tag=FILE1_tag;
         L180 = ((FILE1) L182);
         r11 = L180;
         
         SATTR(r11,fp,stdout);
         ret_val34 = r11;
         FILE_plus_STR(ret_val34, plus_s11);
         ret_val33 = plus_self11;
         plus_self12 = ret_val33;
         L1841_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
         L185=L1841_;
         L187=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
         if (L187==NULL) FATAL("Unable to allocate more memory");
         memset(L187,0,sizeof(struct BOOL_boxed_struct));
         ((OB)L187)->header.tag=BOOL_tag;
         L186 = (dSTR)((BOOL_boxed) L187);
         ((BOOL_boxed) L186)->immutable_part = ATTR(L185,is_arg);
         plus_s12 = L186;
         stdout_self12 = ((FILE1) NULL);
         L189=ZALLOC(sizeof(struct FILE1_struct));
         if (L189==NULL) FATAL("Unable to allocate more memory");
         ((OB)L189)->header.tag=FILE1_tag;
         L188 = ((FILE1) L189);
         r12 = L188;
         
         SATTR(r12,fp,stdout);
         ret_val36 = r12;
         FILE_plus_dSTR(ret_val36, plus_s12);
         ret_val35 = plus_self12;
         plus_self13 = ret_val35;
         plus_s13 = ((STR) &near_arg);
         stdout_self13 = ((FILE1) NULL);
         L193=ZALLOC(sizeof(struct FILE1_struct));
         if (L193==NULL) FATAL("Unable to allocate more memory");
         ((OB)L193)->header.tag=FILE1_tag;
         L192 = ((FILE1) L193);
         r13 = L192;
         
         SATTR(r13,fp,stdout);
         ret_val38 = r13;
         FILE_plus_STR(ret_val38, plus_s13);
         ret_val37 = plus_self13;
         plus_self14 = ret_val37;
         L1951_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
         L196=L1951_;
         L198=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
         if (L198==NULL) FATAL("Unable to allocate more memory");
         memset(L198,0,sizeof(struct BOOL_boxed_struct));
         ((OB)L198)->header.tag=BOOL_tag;
         L197 = (dSTR)((BOOL_boxed) L198);
         ((BOOL_boxed) L197)->immutable_part = ATTR(L196,near_arg);
         plus_s14 = L197;
         stdout_self14 = ((FILE1) NULL);
         L200=ZALLOC(sizeof(struct FILE1_struct));
         if (L200==NULL) FATAL("Unable to allocate more memory");
         ((OB)L200)->header.tag=FILE1_tag;
         L199 = ((FILE1) L200);
         r14 = L199;
         
         SATTR(r14,fp,stdout);
         ret_val40 = r14;
         FILE_plus_dSTR(ret_val40, plus_s14);
         ret_val39 = plus_self14;
         plus_self15 = ret_val39;
         plus_s15 = ((STR) &name9);
         stdout_self15 = ((FILE1) NULL);
         L204=ZALLOC(sizeof(struct FILE1_struct));
         if (L204==NULL) FATAL("Unable to allocate more memory");
         ((OB)L204)->header.tag=FILE1_tag;
         L203 = ((FILE1) L204);
         r15 = L203;
         
         SATTR(r15,fp,stdout);
         ret_val41 = r15;
         FILE_plus_STR(ret_val41, plus_s15);
        }
        L209 = ATTR(ox1,index1);
        L2101_=(j11)==(L209); 
        if (L2101_) {
         L208 = TRUE;
        } else {
         aget_self2 = dep;
         aget_i12 = ATTR(ox1,index1);
         aget_i22 = j11;
         L211 = ATTR(aget_self2,size2);
         L2121_=INTTIMES(aget_i12,L211); 
         L213 = L2121_;
         L2141_=INTPLUS(L213,aget_i22); 
         L215 = L2141_;
         L2161_=ARR((ARRAY2BOOL)aget_self2,L215); 
         ret_val42 = L2161_;
         L217 = ret_val42;
         L2181_=!(L217); 
         L208 = L2181_;
        }
        if (L208) {
         L207 = TRUE;
        } else {
         aget_self3 = dep;
         aget_i13 = ATTR(ox1,index1);
         aget_i23 = j11;
         L222 = ATTR(aget_self3,size2);
         L2231_=INTTIMES(aget_i13,L222); 
         L224 = L2231_;
         L2251_=INTPLUS(L224,aget_i23); 
         L226 = L2251_;
         L2271_=ARR((ARRAY2BOOL)aget_self3,L226); 
         ret_val43 = L2271_;
         if (ret_val43) {
          L2281_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
          L229=L2281_;
          L230 = ATTR(L229,is_sometimes_far);
          L2321_=!(L230); 
          L220 = L2321_;
         } else {
          L220 = FALSE;
         }
         if (L220) {
          L2331_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
          L234=L2331_;
          L235 = ATTR(L234,is_arg);
          L2361_=!(L235); 
          L219 = L2361_;
         } else {
          L219 = FALSE;
         }
         L207 = L219;
        }
        if (L207) {
         L206 = TRUE;
        } else {
         L2381_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
         L239=L2381_;
         if (ATTR(L239,is_arg)) {
          L2401_=(OPT_LOCAL_EXPR)ARR((ARRAYO570594127)ox_table,j11); 
          L241=L2401_;
          L237 = ATTR(L241,near_arg);
         } else {
          L237 = FALSE;
         }
         L206 = L237;
        }
        local_is_near = L206;
       }
      }
      after_loop6: ;
      if (local_is_near) {
       L242 = ATTR(oi,near_local);
       L243 = ATTR(oi,near_local);
       L2441_=(FLISTA725283029)ARR((AREFFL1698233817)L243,i1); 
       L245 = L2441_;
       L246 = FLISTA1817671564(L245, ATTR(ox1,local1));
       SARR((AREFFL1698233817)L242,i1,(FLISTA725283029)L246); 
       ;
       prog_self3 = self;
       ret_val44 = OPT_CO233462019;
       L248=ret_val44;
       if (ATTR(L248,opt_debug)) {
        create_self7 = ((OUT) NULL);
        ret_val45 = create_self7;
        plus_self16 = ret_val45;
        plus_s16 = ((STR) &local1);
        stdout_self16 = ((FILE1) NULL);
        L250=ZALLOC(sizeof(struct FILE1_struct));
        if (L250==NULL) FATAL("Unable to allocate more memory");
        ((OB)L250)->header.tag=FILE1_tag;
        L249 = ((FILE1) L250);
        r16 = L249;
        
        SATTR(r16,fp,stdout);
        ret_val46 = r16;
        FILE_plus_STR(ret_val46, plus_s16);
        AM_OUT1439666796(((AM_OUT) NULL), ((dAM) ATTR(ox1,local1)));
        create_self8 = ((OUT) NULL);
        ret_val47 = create_self8;
        plus_self17 = ret_val47;
        plus_s17 = ((STR) &name9);
        stdout_self17 = ((FILE1) NULL);
        L253=ZALLOC(sizeof(struct FILE1_struct));
        if (L253==NULL) FATAL("Unable to allocate more memory");
        ((OB)L253)->header.tag=FILE1_tag;
        L252 = ((FILE1) L253);
        r17 = L252;
        
        SATTR(r17,fp,stdout);
        ret_val48 = r17;
        FILE_plus_STR(ret_val48, plus_s17);
       }
      }
     }
     L2551_=INTPLUS(L191,1); 
     L191 = L2551_;
    }
   }
   after_loop5: ;
   prog_self4 = self;
   ret_val49 = OPT_CO233462019;
   L256=ret_val49;
   if (ATTR(L256,opt_debug)) {
    create_self9 = ((OUT) NULL);
    ret_val50 = create_self9;
    plus_self18 = ret_val50;
    plus_s18 = ((STR) &localargs);
    stdout_self18 = ((FILE1) NULL);
    L258=ZALLOC(sizeof(struct FILE1_struct));
    if (L258==NULL) FATAL("Unable to allocate more memory");
    ((OB)L258)->header.tag=FILE1_tag;
    L257 = ((FILE1) L258);
    r18 = L257;
    
    SATTR(r18,fp,stdout);
    ret_val51 = r18;
    FILE_plus_STR(ret_val51, plus_s18);
    {
     BOOL f_L2601_ = TRUE;
     BOOL f_L2611_ = TRUE;
     /* loop index variable */
     L201 = 0;
     L262 = ATTR(oi,consider);
     L2631_=(L262)==NULL?0:ASIZE((ARRAYINT)L262); 
     L101 = L2631_;
     /* Initialization of inlined iter INT::aelt!:BOOL */
     L121 = i1;
     aeltb_self1 = L121;
     L221 = INT_asize;
     aeltb_if_first1 = TRUE;
     while (1) {
      if(L201>=L101)  goto after_loop7; 
      ;
      create_self10 = ((OUT) NULL);
      ret_val52 = create_self10;
      plus_self19 = ret_val52;
      if(L201>=L221)  goto after_loop7; 
      aL2611_=L201; 
      L265 = aL2611_;
      L2661_=((CHAR)((aeltb_self1&(1<<L265))!=0)); 
      L268=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
      if (L268==NULL) FATAL("Unable to allocate more memory");
      memset(L268,0,sizeof(struct BOOL_boxed_struct));
      ((OB)L268)->header.tag=BOOL_tag;
      L267 = (dSTR)((BOOL_boxed) L268);
      ((BOOL_boxed) L267)->immutable_part = L2661_;
      plus_s19 = L267;
      stdout_self19 = ((FILE1) NULL);
      L270=ZALLOC(sizeof(struct FILE1_struct));
      if (L270==NULL) FATAL("Unable to allocate more memory");
      ((OB)L270)->header.tag=FILE1_tag;
      L269 = ((FILE1) L270);
      r19 = L269;
      
      SATTR(r19,fp,stdout);
      ret_val55 = r19;
      FILE_plus_dSTR(ret_val55, plus_s19);
      ret_val54 = plus_self19;
      plus_self20 = ret_val54;
      plus_s20 = ((STR) &name10);
      stdout_self20 = ((FILE1) NULL);
      L273=ZALLOC(sizeof(struct FILE1_struct));
      if (L273==NULL) FATAL("Unable to allocate more memory");
      ((OB)L273)->header.tag=FILE1_tag;
      L272 = ((FILE1) L273);
      r20 = L272;
      
      SATTR(r20,fp,stdout);
      ret_val56 = r20;
      FILE_plus_STR(ret_val56, plus_s20);
      L2751_=INTPLUS(L201,1); 
      L201 = L2751_;
     }
    }
    after_loop7: ;
    create_self11 = ((OUT) NULL);
    ret_val57 = create_self11;
    plus_self21 = ret_val57;
    plus_s21 = ((STR) &name9);
    stdout_self21 = ((FILE1) NULL);
    L277=ZALLOC(sizeof(struct FILE1_struct));
    if (L277==NULL) FATAL("Unable to allocate more memory");
    ((OB)L277)->header.tag=FILE1_tag;
    L276 = ((FILE1) L277);
    r21 = L276;
    
    SATTR(r21,fp,stdout);
    ret_val58 = r21;
    FILE_plus_STR(ret_val58, plus_s21);
    create_self12 = ((OUT) NULL);
    ret_val59 = create_self12;
    plus_self22 = ret_val59;
    plus_s22 = ((STR) &locals2);
    stdout_self22 = ((FILE1) NULL);
    L280=ZALLOC(sizeof(struct FILE1_struct));
    if (L280==NULL) FATAL("Unable to allocate more memory");
    ((OB)L280)->header.tag=FILE1_tag;
    L279 = ((FILE1) L280);
    r22 = L279;
    
    SATTR(r22,fp,stdout);
    ret_val60 = r22;
    FILE_plus_STR(ret_val60, plus_s22);
    {
     struct FLISTA2119642552_frame_struct other9_0;
FLISTA2119642552_frame noname1 = &other9_0;
     noname1->state = 0;
     while (1) {
      if (noname1->state == 0) {
       L284 = ATTR(oi,near_local);
       L2851_=(FLISTA725283029)ARR((AREFFL1698233817)L284,i1); 
       L131 = L2851_;
       noname1->self = L131;
      }
      L283 = FLISTA2119642552(noname1);
      if (noname1->state == -1) {
       goto after_loop8;
      }
      AM_OUT1439666796(((AM_OUT) NULL), ((dAM) L283));
      create_self13 = ((OUT) NULL);
      ret_val61 = create_self13;
      plus_self23 = ret_val61;
      plus_s23 = ((STR) &name67);
      stdout_self23 = ((FILE1) NULL);
      L287=ZALLOC(sizeof(struct FILE1_struct));
      if (L287==NULL) FATAL("Unable to allocate more memory");
      ((OB)L287)->header.tag=FILE1_tag;
      L286 = ((FILE1) L287);
      r23 = L286;
      
      SATTR(r23,fp,stdout);
      ret_val62 = r23;
      FILE_plus_STR(ret_val62, plus_s23);
     }
    }
    after_loop8: ;
    create_self14 = ((OUT) NULL);
    ret_val63 = create_self14;
    plus_self24 = ret_val63;
    plus_s24 = ((STR) &name9);
    stdout_self24 = ((FILE1) NULL);
    L290=ZALLOC(sizeof(struct FILE1_struct));
    if (L290==NULL) FATAL("Unable to allocate more memory");
    ((OB)L290)->header.tag=FILE1_tag;
    L289 = ((FILE1) L290);
    r24 = L289;
    
    SATTR(r24,fp,stdout);
    ret_val64 = r24;
    FILE_plus_STR(ret_val64, plus_s24);
   }
   L2921_=INTPLUS(L161,1); 
   L161 = L2921_;
  }
 }
 after_loop3: ;
}
