#include "sather.h"

/* Layouts */

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ARRAYTP_CLASS_struct {/* layout for ARRAY{TP_CLASS} */
 OB_HEADER header;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *ARRAYTP_CLASS;

typedef struct CALL_TP_ARRAY_struct {/* layout for CALL_TP_ARRAY */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_ARRAY;

typedef struct CALL_T561792367_struct {/* layout for CALL_TP_BOUND_CREATE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_T561792367;

typedef struct CALL_TP_CREATE_struct {/* layout for CALL_TP_CREATE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_CREATE;

typedef struct CALL_TP_VOID_struct {/* layout for CALL_TP_VOID */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *CALL_TP_VOID;

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

typedef struct MS_RANDOM_GEN_struct {/* layout for MS_RANDOM_GEN */
 OB_HEADER header;
 INT seed;
 } *MS_RANDOM_GEN;

typedef struct ONCE_MODE_struct {/* layout for ONCE_MODE */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ONCE_MODE;

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

typedef struct TUPdTPdLAYOUT_struct {/* layout for TUP{$TP,$LAYOUT} */
 struct dLAYOUT_struct *t2;
 struct dTP_struct *t1;
 } TUPdTPdLAYOUT;
static TUPdTPdLAYOUT TUPdTPdLAYOUT_zero;

typedef struct TUPdTPdLAYOUT_boxed_struct {
 OB_HEADER header;
 TUPdTPdLAYOUT immutable_part;
 } *TUPdTPdLAYOUT_boxed;

typedef struct TUPAM_304458649_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1007407331_struct *t2;
 } TUPAM_304458649;
static TUPAM_304458649 TUPAM_304458649_zero;

typedef struct TUPAM_304458649_boxed_struct {
 OB_HEADER header;
 TUPAM_304458649 immutable_part;
 } *TUPAM_304458649_boxed;

typedef struct TUPAM_667892060_struct {/* layout for TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 struct AM_BND1124877163_struct *t1;
 struct BOUND_1677815802_struct *t2;
 } TUPAM_667892060;
static TUPAM_667892060 TUPAM_667892060_zero;

typedef struct TUPAM_667892060_boxed_struct {
 OB_HEADER header;
 TUPAM_667892060 immutable_part;
 } *TUPAM_667892060_boxed;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

typedef struct TUPSIG1754078736_struct {/* layout for TUP{SIG,ABSTRACT_FRAME_LAYOUT} */
 struct ABSTRA151498155_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG1754078736;
static TUPSIG1754078736 TUPSIG1754078736_zero;

typedef struct TUPSIG1754078736_boxed_struct {
 OB_HEADER header;
 TUPSIG1754078736 immutable_part;
 } *TUPSIG1754078736_boxed;

typedef struct TUPSIG578692189_struct {/* layout for TUP{SIG,AM_ROUT_DEF} */
 struct AM_ROUT_DEF_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG578692189;
static TUPSIG578692189 TUPSIG578692189_zero;

typedef struct TUPSIG578692189_boxed_struct {
 OB_HEADER header;
 TUPSIG578692189 immutable_part;
 } *TUPSIG578692189_boxed;

typedef struct TUPSIGARG_LAYOUT_struct {/* layout for TUP{SIG,ARG_LAYOUT} */
 struct ARG_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIGARG_LAYOUT;
static TUPSIGARG_LAYOUT TUPSIGARG_LAYOUT_zero;

typedef struct TUPSIGARG_LAYOUT_boxed_struct {
 OB_HEADER header;
 TUPSIGARG_LAYOUT immutable_part;
 } *TUPSIGARG_LAYOUT_boxed;

typedef struct TUPSIG2023533247_struct {/* layout for TUP{SIG,FRAME_LAYOUT} */
 struct FRAME_LAYOUT_struct *t2;
 struct SIG_struct *t1;
 } TUPSIG2023533247;
static TUPSIG2023533247 TUPSIG2023533247_zero;

typedef struct TUPSIG2023533247_boxed_struct {
 OB_HEADER header;
 TUPSIG2023533247 immutable_part;
 } *TUPSIG2023533247_boxed;

typedef struct TUPSTRSIG_struct {/* layout for TUP{STR,SIG} */
 struct SIG_struct *t2;
 STR t1;
 } TUPSTRSIG;
static TUPSTRSIG TUPSTRSIG_zero;

typedef struct TUPSTRSIG_boxed_struct {
 OB_HEADER header;
 TUPSTRSIG immutable_part;
 } *TUPSTRSIG_boxed;

typedef struct TUPSTRSTR_struct {/* layout for TUP{STR,STR} */
 STR t1;
 STR t2;
 } TUPSTRSTR;
static TUPSTRSTR TUPSTRSTR_zero;

typedef struct TUPSTRSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRSTR immutable_part;
 } *TUPSTRSTR_boxed;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

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

typedef struct FLISTdLAYOUT_struct {/* layout for FLIST{$LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dLAYOUT_struct *arr_part[1];
 } *FLISTdLAYOUT;

typedef struct FLISTCODE_FILE_struct {/* layout for FLIST{CODE_FILE} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct CODE_FILE_struct *arr_part[1];
 } *FLISTCODE_FILE;

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

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

typedef struct FLISTTUPIDENTINT_struct {/* layout for FLIST{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FLISTTUPIDENTINT;

typedef struct FMAPdTPdLAYOUT_struct {/* layout for FMAP{$TP,$LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPdTPdLAYOUT_struct arr_part[1];
 } *FMAPdTPdLAYOUT;

typedef struct FMAPAM339652544_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_304458649_struct arr_part[1];
 } *FMAPAM339652544;

typedef struct FMAPAM1357596931_struct {/* layout for FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPAM_667892060_struct arr_part[1];
 } *FMAPAM1357596931;

typedef struct FMAPSI2103621588_struct {/* layout for FMAP{SIG,ABSTRACT_FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG1754078736_struct arr_part[1];
 } *FMAPSI2103621588;

typedef struct FMAPSI518162669_struct {/* layout for FMAP{SIG,AM_ROUT_DEF} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG578692189_struct arr_part[1];
 } *FMAPSI518162669;

typedef struct FMAPSI1349988702_struct {/* layout for FMAP{SIG,ARG_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIGARG_LAYOUT_struct arr_part[1];
 } *FMAPSI1349988702;

typedef struct FMAPSI1121373188_struct {/* layout for FMAP{SIG,FRAME_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSIG2023533247_struct arr_part[1];
 } *FMAPSI1121373188;

typedef struct FMAPSTRSIG_struct {/* layout for FMAP{STR,SIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSIG_struct arr_part[1];
 } *FMAPSTRSIG;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

typedef struct FSETAB1569698808_struct {/* layout for FSET{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FSETAB1569698808;

typedef struct FSETBO1548397318_struct {/* layout for FSET{BOUND_ITER_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_2140373124_struct *arr_part[1];
 } *FSETBO1548397318;

typedef struct FSETBO1147931784_struct {/* layout for FSET{BOUND_TYPE_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BOUND_809390774_struct *arr_part[1];
 } *FSETBO1147931784;

typedef struct FSETBU1446477505_struct {/* layout for FSET{BUILTIN_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct BUILTIN_LAYOUT_struct *arr_part[1];
 } *FSETBU1446477505;

typedef struct FSETCLASS_LAYOUT_struct {/* layout for FSET{CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct CLASS_LAYOUT_struct *arr_part[1];
 } *FSETCLASS_LAYOUT;

typedef struct FSETEX1260947371_struct {/* layout for FSET{EXTERNAL_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct EXTERNAL_LAYOUT_struct *arr_part[1];
 } *FSETEX1260947371;

typedef struct FSETIM184436961_struct {/* layout for FSET{IMMUTABLE_CLASS_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct IMMUTA421759893_struct *arr_part[1];
 } *FSETIM184436961;

typedef struct FSETSTR_struct {/* layout for FSET{STR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 STR arr_part[1];
 } *FSETSTR;

typedef struct FSETTUPIDENTINT_struct {/* layout for FSET{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FSETTUPIDENTINT;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

typedef struct IDENT_TBL_struct {/* layout for IDENT_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *IDENT_TBL;

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


/* Function declarations */


/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */


FLTD TIME_t854034019 = FLTD_zero;
IDENT_TBL IDENT_ident_tbl = ((IDENT_TBL) NULL);
const INT IDENT_105403148 = 4;
IDENT IDENT_959790938;
IDENT IDENT_428696930;
IDENT IDENT_1924458840;
IDENT IDENT_853591262;
IDENT IDENT_1067669005;
IDENT IDENT_12128454;
IDENT IDENT_1644121920;
IDENT IDENT_685437142;
IDENT IDENT_869381517;
IDENT IDENT_291726180;
IDENT IDENT_1834849979;
IDENT IDENT_1295303664;
IDENT IDENT_692534448;
IDENT IDENT_51715464;
IDENT IDENT_58546263;
IDENT IDENT_1418293603;
IDENT IDENT_1061837561;
IDENT IDENT_5303930;
IDENT IDENT_265373817;
IDENT IDENT_1280657180;
IDENT IDENT_386785441;
IDENT IDENT_2085547498;
IDENT IDENT_1129727818;
IDENT IDENT_1616879092;
IDENT IDENT_228725606;
IDENT IDENT_1974148927;
IDENT IDENT_475003333;
IDENT IDENT_421158771;
IDENT IDENT_2108657069;
IDENT IDENT_1837767448;
IDENT IDENT_1485568624;
IDENT IDENT_1666445894;
IDENT IDENT_1140439567;
IDENT IDENT_667688077;
IDENT IDENT_2110206590;
IDENT IDENT_327685657;
IDENT IDENT_458929583;
IDENT IDENT_190278447;
IDENT IDENT_1909070998;
IDENT IDENT_985337352;
IDENT IDENT_2063376192;
IDENT IDENT_1891587148;
IDENT IDENT_857412936;
IDENT IDENT_1137952589;
IDENT IDENT_196328811;
IDENT IDENT_1728630987;
IDENT IDENT_461800478;
IDENT IDENT_604723504;
IDENT IDENT_673807638;
IDENT IDENT_531080196;
FLISTTUPIDENTINT FIND_T1407637984 = ((FLISTTUPIDENTINT) NULL);
FLISTTUPIDENTINT FIND_G1866376849 = ((FLISTTUPIDENTINT) NULL);
const INT INLINE29411189 = 4;
const INT INT_asize = 32;
const INT INLINE494873411 = 4;
const INT INLINE2125477179 = 16;
const INT INLINE142050909 = 16;
PROG LAYOUT_TBL_prog = ((PROG) NULL);
CGEN LAYOUT_TBL_cgen = ((CGEN) NULL);
const INT ARRAYS852413528 = 10;
MS_RANDOM_GEN RND_default_gen = ((MS_RANDOM_GEN) NULL);
FLTD MS_RAN393309279 = FLTD_zero;
FLTD MS_RAN1117060533 = FLTD_zero;
FLTD MS_RAN393309507;
FLISTSTR SFILE_ID_files = ((FLISTSTR) NULL);
FLISTINT SFILE_ID_lines = ((FLISTINT) NULL);
const INT SFILE_ID_B = 1024;
FSTR STR_buf = ((FSTR) NULL);
FSTR INTI_buf = ((FSTR) NULL);
const INT INTI_log10D = 4;
INT INTI_D;
const INT INTI_log2B = 15;
INT INTI_B;
const INT FSETST1746314450 = 5;
const BOOL FSETST1011828476 = FALSE;
const INT FSETST854048553 = 4;
const BOOL FSETST846796035 = TRUE;
const INT FMAPST2041121537 = 4;
INT CGEN_m1427112264 = 24;
const INT FMAPST1192706365 = 4;
const INT STR_CU1921064154 = 0;
const INT STR_CU1114535871 = 2;
const INT STR_CU254621724 = 32;
const INT STR_CU1474621917 = 4;
const INT STR_CU610585082 = 1;
const INT STR_CU1107107574 = 3;
const INT STR_CU1719265904 = 9;
const INT FMAPST641873122 = 4;
const INT TP_KIN138039446 = 0;
const INT FSETTP752200044 = 5;
const BOOL FSETTP1720402111 = FALSE;
const INT FSETTP1014013453 = 4;
const BOOL FSETTP138222400 = TRUE;
const INT TP_CLA1631459432 = 4;
FSTR SFILE_ID_source = ((FSTR) NULL);
const INT SFILE_1180160528 = 2147483647;
INT SFILE_ID_pos = INT_zero;
INT SFILE_ID_column = INT_zero;
BOOL SFILE_ID_newline = BOOL_zero;
INT SFILE_ID_line = 0;
INT SFILE_1796395955 = INT_zero;
const CHAR SFILE_446927505 = '\0';
const INT SCANNER_null_tok = 1;
const INT SCANNER_eof_tok = 0;
const INT SCANNE589883380 = 2;
const INT SCANNE951940802 = 44;
const INT SCANNER_attr_tok = 6;
const INT SCANNER_and_tok = 4;
const INT SCANNE687089717 = 5;
const INT SCANNER_any_tok = 108;
const INT SCANNE661829813 = 7;
const INT SCANNE560979044 = 114;
const INT SCANNER_bind_tok = 91;
const INT SCANNE2139920629 = 10;
const INT SCANNE304035056 = 9;
const INT SCANNER_case_tok = 8;
const INT SCANNE438656140 = 101;
const INT SCANNE1099722568 = 93;
const INT SCANNE1499824177 = 92;
const INT SCANNE1021322009 = 109;
const INT SCANNER_do_tok = 95;
const INT SCANNER_end_tok = 13;
const INT SCANNE766559877 = 12;
const INT SCANNER_else_tok = 11;
const INT SCANNE114307641 = 14;
const INT SCANNE754612661 = 15;
const INT SCANNE463806109 = 16;
const INT SCANNER_far_tok = 106;
const INT SCANNER_fork_tok = 94;
const INT SCANNE1284590324 = 35;
const INT SCANNE105624607 = 103;
const INT SCANNE1233467974 = 113;
const INT SCANNER_here_tok = 111;
const INT SCANNER_is_tok = 20;
const INT SCANNER_if_tok = 17;
const INT SCANNE1874840845 = 89;
const INT SCANNE2137779101 = 18;
const INT SCANNE801235924 = 45;
const INT SCANNE1717311299 = 19;
const INT SCANNER_ITER_tok = 21;
const INT SCANNER_loop_tok = 22;
const INT SCANNER_lock_tok = 99;
const INT SCANNER_new_tok = 23;
const INT SCANNER_near_tok = 105;
const INT SCANNER_or_tok = 24;
const INT SCANNER_once_tok = 90;
const INT SCANNER_out_tok = 88;
const INT SCANNER_pre_tok = 26;
const INT SCANNE1553373348 = 27;
const INT SCANNER_post_tok = 25;
const INT SCANNE440197020 = 28;
const INT SCANNE1733136762 = 47;
const INT SCANNER_par_tok = 96;
const INT SCANNE1574122250 = 97;
const INT SCANNER_quit_tok = 29;
const INT SCANNE943336743 = 33;
const INT SCANNE269030544 = 30;
const INT SCANNE1090219034 = 31;
const INT SCANNE1014449292 = 32;
const INT SCANNER_ROUT_tok = 34;
const INT SCANNER_self_tok = 37;
const INT SCANNE983081836 = 38;
const INT SCANNE1277304122 = 107;
const INT SCANNER_stub_tok = 48;
const INT SCANNER_sync_tok = 102;
const INT SCANNER_SAME_tok = 36;
const INT SCANNER_then_tok = 39;
const INT SCANNER_true_tok = 40;
const INT SCANNE392475110 = 42;
const INT SCANNER_type_tok = 41;
const INT SCANNE202030842 = 43;
const INT SCANNE1785932597 = 100;
const INT SCANNER_void_tok = 49;
const INT SCANNE1010372543 = 46;
const INT SCANNER_when_tok = 50;
const INT SCANNE1391319271 = 51;
const INT SCANNE1901892399 = 112;
const INT SCANNER_with_tok = 104;
const INT SCANNE1252648769 = 52;
const INT SCANNE1412300811 = 57;
const INT SCANNE1414957377 = 58;
const INT SCANNE2045360664 = 59;
const INT SCANNE1290586306 = 60;
const INT SCANNE620595834 = 61;
const INT SCANNE623252400 = 62;
const INT SCANNE916727723 = 63;
const INT SCANNER_dot_tok = 64;
const INT SCANNER_semi_tok = 65;
const INT SCANNE2093995558 = 3;
const INT SCANNER_plus_tok = 68;
const INT SCANNE1421647682 = 85;
const INT SCANNE470944348 = 69;
const INT SCANNE1514574730 = 70;
const INT SCANNE879190010 = 74;
const INT SCANNER_pow_tok = 77;
const INT SCANNER_mod_tok = 78;
const INT SCANNER_vbar_tok = 79;
const INT SCANNER_bang_tok = 75;
const INT SCANNE274598975 = 76;
const INT SCANNE1388342024 = 67;
const INT SCANNE738612593 = 86;
const INT SCANNE149137033 = 66;
const INT SCANNE694354542 = 84;
const INT SCANNE1938931342 = 83;
const INT SCANNE334166344 = 98;
const INT SCANNE2078461540 = 80;
const INT SCANNE1180808401 = 71;
const INT SCANNE290718062 = 81;
const INT SCANNE401863129 = 72;
const INT SCANNE116326663 = 82;
const INT SCANNE637092534 = 73;
const INT SCANNER_not_tok = 87;
const INT SCANNE928026230 = 56;
const INT SCANNER_lstr_tok = 55;
const INT SCANNER_lint_tok = 53;
const INT SCANNER_lflt_tok = 54;
const INT AS_FLT20428321 = 0;
const INT AS_FLT388140455 = 4;
const INT AS_FLT1215265092 = 3;
const INT AS_FLT388140360 = 1;
const INT AS_FLT388140740 = 2;
const INT SCANNER_at_tok = 110;
const INT PARSER1701976116 = 44;
const INT PARSER_type_tok = 41;
const INT PARSER1344673316 = 107;
const INT PARSER1916422053 = 45;
const INT PARSER_value_tok = 46;
const INT PARSER11100551 = 47;
const INT PARSER886437717 = 15;
const INT PARSER_class_tok = 9;
const INT AS_CLASS_DEF_abs = 3;
const INT TOKEN_eof_tok = 0;
const INT TOKEN_null_tok = 1;
const INT TOKEN_ident_tok = 2;
const INT TOKEN_1684560144 = 3;
const INT TOKEN_and_tok = 4;
const INT TOKEN_assert_tok = 5;
const INT TOKEN_attr_tok = 6;
const INT TOKEN_break_tok = 7;
const INT TOKEN_case_tok = 8;
const INT TOKEN_class_tok = 9;
const INT TOKEN_const_tok = 10;
const INT TOKEN_else_tok = 11;
const INT TOKEN_elsif_tok = 12;
const INT TOKEN_end_tok = 13;
const INT TOKEN_402103953 = 14;
const INT TOKEN_348462331 = 15;
const INT TOKEN_false_tok = 16;
const INT TOKEN_if_tok = 17;
const INT TOKEN_387428395 = 18;
const INT TOKEN_827305291 = 19;
const INT TOKEN_is_tok = 20;
const INT TOKEN_ITER_tok = 21;
const INT TOKEN_loop_tok = 22;
const INT TOKEN_new_tok = 23;
const INT TOKEN_or_tok = 24;
const INT TOKEN_post_tok = 25;
const INT TOKEN_pre_tok = 26;
const INT TOKEN_991243242 = 27;
const INT TOKEN_1310153686 = 28;
const INT TOKEN_quit_tok = 29;
const INT TOKEN_raise_tok = 30;
const INT TOKEN_12855958 = 31;
const INT TOKEN_return_tok = 33;
const INT TOKEN_ROUT_tok = 34;
const INT TOKEN_F_ROUT_tok = 35;
const INT TOKEN_SAME_tok = 36;
const INT TOKEN_self_tok = 37;
const INT TOKEN_shared_tok = 38;
const INT TOKEN_stub_tok = 48;
const INT TOKEN_then_tok = 39;
const INT TOKEN_true_tok = 40;
const INT TOKEN_type_tok = 41;
const INT TOKEN_1495550102 = 42;
const INT TOKEN_2055015794 = 44;
const INT TOKEN_284824330 = 45;
const INT TOKEN_value_tok = 46;
const INT TOKEN_17213944 = 47;
const INT TOKEN_void_tok = 49;
const INT TOKEN_when_tok = 50;
const INT TOKEN_while_tok = 51;
const INT TOKEN_until_tok = 43;
const INT TOKEN_yield_tok = 52;
const INT TOKEN_lint_tok = 53;
const INT TOKEN_lflt_tok = 54;
const INT TOKEN_lstr_tok = 55;
const INT TOKEN_lchar_tok = 56;
const INT TOKEN_lparen_tok = 57;
const INT TOKEN_rparen_tok = 58;
const INT TOKEN_942285672 = 59;
const INT TOKEN_1901305998 = 60;
const INT TOKEN_lbrace_tok = 61;
const INT TOKEN_rbrace_tok = 62;
const INT TOKEN_comma_tok = 63;
const INT TOKEN_dot_tok = 64;
const INT TOKEN_semi_tok = 65;
const INT TOKEN_colon_tok = 66;
const INT TOKEN_under_tok = 67;
const INT TOKEN_plus_tok = 68;
const INT TOKEN_minus_tok = 69;
const INT TOKEN_times_tok = 70;
const INT TOKEN_77733409 = 71;
const INT TOKEN_is_lt_tok = 72;
const INT TOKEN_is_gt_tok = 73;
const INT TOKEN_sharp_tok = 74;
const INT TOKEN_bang_tok = 75;
const INT TOKEN_791010569 = 76;
const INT TOKEN_pow_tok = 77;
const INT TOKEN_mod_tok = 78;
const INT TOKEN_vbar_tok = 79;
const INT TOKEN_is_neq_tok = 80;
const INT TOKEN_is_leq_tok = 81;
const INT TOKEN_is_geq_tok = 82;
const INT TOKEN_assign_tok = 83;
const INT TOKEN_905236088 = 85;
const INT TOKEN_is_eq_tok = 86;
const INT TOKEN_not_tok = 87;
const INT TOKEN_dcolon_tok = 84;
const INT TOKEN_out_tok = 88;
const INT TOKEN_inout_tok = 89;
const INT TOKEN_once_tok = 90;
const INT TOKEN_bind_tok = 91;
const INT TOKEN_attach_tok = 98;
const INT TOKEN_fork_tok = 94;
const INT TOKEN_lock_tok = 99;
const INT TOKEN_unlock_tok = 100;
const INT TOKEN_with_tok = 104;
const INT TOKEN_at_tok = 110;
const INT TOKEN_here_tok = 111;
const INT TOKEN_where_tok = 112;
const INT TOKEN_near_tok = 105;
const INT TOKEN_far_tok = 106;
const INT TOKEN_spread_tok = 107;
const INT TOKEN_do_tok = 95;
const INT TOKEN_par_tok = 96;
const INT TOKEN_970494340 = 97;
const INT TOKEN_any_tok = 108;
const INT TOKEN_1044792413 = 92;
const INT TOKEN_259588423 = 109;
const INT TOKEN_181187864 = 93;
const INT TOKEN_cohort_tok = 101;
const INT TOKEN_sync_tok = 102;
const INT TOKEN_guard_tok = 103;
const INT TOKEN_global_tok = 113;
const INT TOKEN_1983637546 = 114;
const INT PARSER978809429 = 3;
const INT PARSER_ident_tok = 2;
const INT PARSER2001381604 = 61;
const INT PARSER_is_lt_tok = 72;
const INT PARSER_SAME_tok = 36;
const INT AS_TYP1853679690 = 3;
const INT AS_TYPE_SPEC_ord = 0;
const INT PARSER_comma_tok = 63;
const INT PARSER1998725038 = 62;
const INT PARSER_ROUT_tok = 34;
const INT AS_TYPE_SPEC_rt = 1;
const INT PARSER_ITER_tok = 21;
const INT AS_TYPE_SPEC_it = 2;
const INT PARSER1337387114 = 35;
const INT AS_TYP1851973889 = 4;
const INT PARSER_out_tok = 88;
INT AS_ARG685418392 = 2;
const INT PARSER_inout_tok = 89;
INT AS_ARG1872943786 = 3;
const INT PARSER_once_tok = 90;
INT AS_ARG551300469 = 4;
INT AS_ARG1300937332 = 1;
const INT PARSER_bang_tok = 75;
const INT PARSER840587154 = 76;
const INT PARSER_colon_tok = 66;
const INT PARSER_is_gt_tok = 73;
const INT PARSER_is_tok = 20;
const INT PARSER1209676627 = 57;
const INT PARSER1207020061 = 58;
const INT PARSER_semi_tok = 65;
const INT PARSER_end_tok = 13;
const INT PARSER_eof_tok = 0;
const INT AS_CLASS_DEF_spr = 7;
const INT AS_CLASS_DEF_imm = 1;
const INT AS_CLA86009053 = 4;
const INT AS_CLA2085573152 = 6;
const INT AS_CLA1666707132 = 5;
const INT AS_CLASS_DEF_ref = 2;
const INT PARSER1019557737 = 27;
const INT PARSER550831344 = 31;
const INT PARSER_const_tok = 10;
const INT PARSER689908022 = 38;
const INT PARSER_stub_tok = 48;
const INT PARSER_attr_tok = 6;
const INT PARSER415742890 = 18;
const INT PARSER_null_tok = 1;
const INT PARSER1758133485 = 85;
const INT PARSER265941484 = 83;
const INT PARSER_minus_tok = 69;
const INT PARSER_lint_tok = 53;
const INT PARSER_lflt_tok = 54;
const INT PARSER_not_tok = 87;
const INT PARSER_self_tok = 37;
const INT PARSER978635316 = 84;
const INT PARSER_void_tok = 49;
const INT PARSER_new_tok = 23;
const INT PARSER_sharp_tok = 74;
const INT PARSER_under_tok = 67;
const INT PARSER_dot_tok = 64;
const INT PARSER_bind_tok = 91;
const INT PARSER_vbar_tok = 79;
const INT PARSER1229493770 = 14;
const INT PARSER855619786 = 19;
const INT PARSER658540566 = 32;
const INT PARSER_while_tok = 51;
const INT PARSER_until_tok = 43;
const INT PARSER_break_tok = 7;
const INT PARSER_true_tok = 40;
const INT PARSER_false_tok = 16;
const INT PARSER_lchar_tok = 56;
const INT PARSER_lstr_tok = 55;
const INT PARSER404310286 = 59;
const INT PARSER1363330612 = 60;
const INT PARSER_here_tok = 111;
const INT PARSER1388509464 = 113;
const INT PARSER1073106908 = 92;
const INT PARSER1374810694 = 93;
const INT PARSER1296410135 = 109;
const INT PARSER1234333718 = 101;
const INT PARSER_where_tok = 112;
const INT PARSER_near_tok = 105;
const INT PARSER_far_tok = 106;
const INT PARSER_pow_tok = 77;
const INT PARSER_times_tok = 70;
const INT PARSER460241977 = 71;
const INT PARSER_mod_tok = 78;
const INT PARSER_plus_tok = 68;
const INT PARSER_is_eq_tok = 86;
const INT PARSER543515898 = 80;
const INT PARSER1789316521 = 82;
const INT PARSER1963707920 = 81;
const INT PARSER_and_tok = 4;
const INT PARSER_or_tok = 24;
const INT PARSER_at_tok = 110;
const INT PARSER_any_tok = 108;
const INT PARSER_pre_tok = 26;
const INT PARSER_post_tok = 25;
const INT PARSER2011952041 = 114;
const INT PARSER_if_tok = 17;
const INT PARSER_loop_tok = 22;
const INT PARSER_yield_tok = 52;
const INT PARSER_quit_tok = 29;
const INT PARSER729653115 = 33;
const INT PARSER_case_tok = 8;
const INT PARSER2033525488 = 42;
const INT PARSER985900141 = 5;
const INT PARSER1281839191 = 28;
const INT PARSER_raise_tok = 30;
const INT PARSER_fork_tok = 94;
const INT PARSER_lock_tok = 99;
const INT PARSER836044841 = 100;
const INT PARSER_with_tok = 104;
const INT PARSER_par_tok = 96;
const INT PARSER998808835 = 97;
const INT PARSER_sync_tok = 102;
const INT PARSER_then_tok = 39;
const INT PARSER_elsif_tok = 12;
const INT PARSER_else_tok = 11;
const INT PARSER_when_tok = 50;
const INT PARSER_do_tok = 95;
const INT PARSER_guard_tok = 103;
const INT PARSER1338823514 = 98;
const INT PROG_A1730220286 = 4;
const INT TP_KIND_part_tp = 4;
IDENT TP_BUI2068715177;
TP_CLASS TP_BUILTIN_fstr = ((TP_CLASS) NULL);
IDENT TP_BUI482029710;
TP_CLASS TP_BUI1686668470 = ((TP_CLASS) NULL);
IDENT TP_BUI238610581;
TP_CLASS TP_BUI233684457 = ((TP_CLASS) NULL);
IDENT TP_BUI243806180;
TP_CLASS TP_BUI403444172 = ((TP_CLASS) NULL);
IDENT TP_BUI640672586;
TP_CLASS TP_BUI374366290 = ((TP_CLASS) NULL);
IDENT TP_BUI66959111;
TP_CLASS TP_BUI1194559023 = ((TP_CLASS) NULL);
IDENT TP_BUI881541400;
TP_CLASS TP_BUI1805763264 = ((TP_CLASS) NULL);
IDENT TP_BUI620054994;
TP_CLASS TP_BUI1370948386 = ((TP_CLASS) NULL);
IDENT TP_BUI567029074;
TP_CLASS TP_BUI633637838 = ((TP_CLASS) NULL);
IDENT TP_BUI1390125097;
TP_CLASS TP_BUI1032249067 = ((TP_CLASS) NULL);
IDENT TP_BUI436262224;
TP_CLASS TP_BUI1116976640 = ((TP_CLASS) NULL);
IDENT TP_BUI354250336;
IDENT TP_BUI726964134;
TP_CLASS TP_BUI2088674906 = ((TP_CLASS) NULL);
IDENT TP_BUI764451214;
ARRAYTP_CLASS TP_BUI1517259338 = ((ARRAYTP_CLASS) NULL);
IDENT TP_BUI388121286;
ARRAYTP_CLASS TP_BUI1236844942 = ((ARRAYTP_CLASS) NULL);
IDENT TP_BUI505750453;
ARRAYTP_CLASS TP_BUI1236844961 = ((ARRAYTP_CLASS) NULL);
IDENT TP_BUI1636890711;
TP_CLASS TP_BUI231294523 = ((TP_CLASS) NULL);
IDENT TP_BUI1322220574;
TP_CLASS TP_BUI1769472218 = ((TP_CLASS) NULL);
IDENT TP_BUI1597109090;
TP_CLASS TP_BUI274781170 = ((TP_CLASS) NULL);
IDENT TP_BUI1626092738;
TP_CLASS TP_BUI139344778 = ((TP_CLASS) NULL);
IDENT TP_BUI1459211256;
TP_CLASS TP_BUILTIN_c_int = ((TP_CLASS) NULL);
IDENT TP_BUI345188431;
TP_CLASS TP_BUI232519909 = ((TP_CLASS) NULL);
IDENT TP_BUI57672398;
TP_CLASS TP_BUI779482534 = ((TP_CLASS) NULL);
IDENT TP_BUI591808532;
TP_CLASS TP_BUI997290216 = ((TP_CLASS) NULL);
IDENT TP_BUI659858568;
TP_CLASS TP_BUI1768246832 = ((TP_CLASS) NULL);
IDENT TP_BUI1586744698;
TP_CLASS TP_BUI107670638 = ((TP_CLASS) NULL);
IDENT TP_BUI222951651;
TP_CLASS TP_BUI1959866815 = ((TP_CLASS) NULL);
IDENT TP_BUI1031701849;
TP_CLASS TP_BUI1825364383 = ((TP_CLASS) NULL);
IDENT TP_BUI1845282323;
TP_CLASS TP_BUILTIN_c_ptr = ((TP_CLASS) NULL);
IDENT TP_BUI1928257270;
TP_CLASS TP_BUI468587458 = ((TP_CLASS) NULL);
IDENT TP_BUI2144653755;
TP_CLASS TP_BUI1716944501 = ((TP_CLASS) NULL);
IDENT TP_BUI391975957;
TP_CLASS TP_BUI1696960301 = ((TP_CLASS) NULL);
IDENT TP_BUI819938711;
TP_CLASS TP_BUI444762713 = ((TP_CLASS) NULL);
IDENT TP_BUI732557391;
TP_CLASS TP_BUI1292722345 = ((TP_CLASS) NULL);
IDENT TP_BUI979458576;
TP_CLASS TP_BUI1248326412 = ((TP_CLASS) NULL);
IDENT TP_BUI895836687;
TP_CLASS TP_BUI1606257075 = ((TP_CLASS) NULL);
IDENT TP_BUI1516264549;
TP_CLASS TP_BUI2131222993 = ((TP_CLASS) NULL);
IDENT TP_BUI757402305;
TP_CLASS TP_BUI1798283841 = ((TP_CLASS) NULL);
IDENT TP_BUI1825617811;
TP_CLASS TP_BUI102735229 = ((TP_CLASS) NULL);
IDENT TP_BUI1336772878;
TP_CLASS TP_BUI1296146890 = ((TP_CLASS) NULL);
IDENT TP_BUI370100142;
TP_CLASS TP_BUI2025364786 = ((TP_CLASS) NULL);
IDENT TP_BUI1247087282;
TP_CLASS TP_BUI1643601086 = ((TP_CLASS) NULL);
IDENT TP_BUI459983896;
TP_CLASS TP_BUI1609790832 = ((TP_CLASS) NULL);
TP_CLASS TP_BUI1325635093 = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_bool = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_char = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_int = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_inti = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_flt = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_fltd = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_str = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_sys = ((TP_CLASS) NULL);
TP_CLASS TP_BUI389447236 = ((TP_CLASS) NULL);
TP_CLASS TP_BUI191368816 = ((TP_CLASS) NULL);
TP_CLASS TP_BUI765623063 = ((TP_CLASS) NULL);
const INT TP_ROU558963694 = 4;
TP_ROUT TP_BUILTIN_rout = ((TP_ROUT) NULL);
TP_CLASS TP_BUI845074864 = ((TP_CLASS) NULL);
TP_CLASS TP_BUI1813155122 = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_zone = ((TP_CLASS) NULL);
PROG OPT_CO233462019 = ((PROG) NULL);
dTP OPT_CO1800765742 = ((dTP) NULL);
dTP OPT_CO146094031 = ((dTP) NULL);
dTP OPT_CO146016663 = ((dTP) NULL);
AM_INT_CONST OPT_CO232246741 = ((AM_INT_CONST) NULL);
AM_INT_CONST OPT_CO238346998 = ((AM_INT_CONST) NULL);
AM_INT_CONST OPT_CO238309264 = ((AM_INT_CONST) NULL);
AM_INT_CONST OPT_CO150710673 = ((AM_INT_CONST) NULL);
AM_INT_CONST OPT_CO150672939 = ((AM_INT_CONST) NULL);
const INT TP_KIND_abs_tp = 3;
const INT TP_GEN180842239 = 4;
const INT FSETdT2053737937 = 5;
const BOOL FSETdT1052485343 = FALSE;
const INT FSETdT1655792354 = 4;
const BOOL FSETdT806139168 = TRUE;
IN_MODE MODES_in_mode;
OUT_MODE MODES_out_mode;
INOUT_MODE MODES_inout_mode;
ONCE_MODE MODES_once_mode;
const INT TP_ITE34679094 = 4;
const INT FMAPTP676284949 = 4;
const INT FMAPTP642805238 = 4;
const INT TP_KIND_ext_c_tp = 5;
const INT TP_KIN462481068 = 6;
const INT FSETTU939302060 = 4;
const BOOL FSETTU574342446 = TRUE;
const INT FSETTU561674193 = 5;
const BOOL FSETTU1862000339 = FALSE;
FSETTUPIDENTINT IFC_abs_cur = ((FSETTUPIDENTINT) NULL);
FMAPSTRSIG SIG_sigs = ((FMAPSTRSIG) NULL);
const INT FMAPST1449505507 = 4;
const INT SIG_TB1720612202 = 4;
const INT TP_KIND_iter_tp = 9;
const INT TP_KIND_rout_tp = 8;
TP_CLASS TP_BUILTIN_fltx = ((TP_CLASS) NULL);
TP_CLASS TP_BUILTIN_fltdx = ((TP_CLASS) NULL);
const INT ELT_TB2133420318 = 4;
const INT TP_KIND_val_tp = 1;
const INT TP_KIND_ref_tp = 2;
const INT TP_KIND_spr_tp = 7;
const INT FSETAS1287498417 = 5;
const BOOL FSETAS1812528589 = FALSE;
const INT FSETAS1969137460 = 4;
const BOOL FSETAS46095922 = TRUE;
const INT FSETSI1350248406 = 5;
const BOOL FSETSI2024459664 = FALSE;
const INT FSETSI1658388805 = 4;
const BOOL FSETSI411883121 = TRUE;
INT INLINE1173790597 = INT_zero;
BOOL AM_CURSOR_debug = FALSE;
const INT FMAPdO802731536 = 4;
const INT NAMEMA2064441237 = 4;
PROG CODE_FILE_prog = ((PROG) NULL);
CGEN CODE_FILE_cgen = ((CGEN) NULL);
STR CODE_FILE_dir = ((STR) NULL);
FMAPSI518162669 CODE_F508943357 = ((FMAPSI518162669) NULL);
FMAPSTRSTR CODE_F1403139677 = ((FMAPSTRSTR) NULL);
FLISTCODE_FILE CODE_FILE_all = ((FLISTCODE_FILE) NULL);
const INT FMAPdT2042454662 = 4;
CS_OPTIONS OPT_CO1299251581 = ((CS_OPTIONS) NULL);
const INT TP_ARR899550402 = 10;
const INT FSETCS2145101147 = 5;
const BOOL FSETCS836086956 = FALSE;
const INT FSETCS1883401206 = 4;
const BOOL FSETCS1600255829 = TRUE;
PROG SE_CONTEXT_prog = ((PROG) NULL);
CS_OPTIONS SE_CON1373267262 = ((CS_OPTIONS) NULL);
const INT FSETSI1189203070 = 5;
const BOOL FSETSI1311217376 = FALSE;
const INT FSETSI232994243 = 4;
const BOOL FSETSI1125125409 = TRUE;
const INT FMAPST47057045 = 4;
const INT FSETAM1929715922 = 5;
const BOOL FSETAM501048668 = FALSE;
const INT FSETAM295466615 = 4;
const BOOL FSETAM1935294117 = TRUE;
const INT FMAPSI1253630331 = 4;
INT INLINE957944682 = INT_zero;
INT INLINE1258550073 = INT_zero;
INT INLINE1306635001 = INT_zero;
INT INLINE1181209094 = INT_zero;
INT INLINE1950426034 = INT_zero;
INT INLINE1232309177 = INT_zero;
const INT FMAPAM498940295 = 4;
INT INLINE1752510002 = INT_zero;
INT INLINE1301544351 = INT_zero;
const INT FSETAM2014782744 = 5;
const BOOL FSETAM994490756 = FALSE;
const INT FSETAM584174453 = 4;
const BOOL FSETAM1441852029 = TRUE;
const INT FMAPAM476780631 = 4;
FMAPdTPdLAYOUT LAYOUT1191461457 = ((FMAPdTPdLAYOUT) NULL);
FSETBU1446477505 BUILTI898649695 = ((FSETBU1446477505) NULL);
const INT FSETBU864102134 = 5;
const BOOL FSETBU1017592727 = FALSE;
const INT FSETBU1565274135 = 4;
const BOOL FSETBU841031784 = TRUE;
const INT FMAPdT861897287 = 4;
FSETEX1260947371 EXTERN1555805875 = ((FSETEX1260947371) NULL);
const INT FSETEX1632590957 = 5;
const BOOL FSETEX915536377 = FALSE;
const INT FSETEX2023113290 = 4;
const BOOL FSETEX1520806408 = TRUE;
FSTR FAST_I1322544971 = ((FSTR) NULL);
const INT FMAPID1835582514 = 4;
const INT ARRAYI56037128 = 10;
FSETAB1569698808 ABSTRA8148400 = ((FSETAB1569698808) NULL);
const INT FSETAB478854058 = 5;
const BOOL FSETAB582100884 = FALSE;
const INT FSETAB246578603 = 4;
const BOOL FSETAB1854241901 = TRUE;
FSETBO1147931784 BOUND_2075179152 = ((FSETBO1147931784) NULL);
const INT FSETBO157482060 = 5;
const BOOL FSETBO1572491060 = FALSE;
const INT FSETBO1040487207 = 4;
const BOOL FSETBO286133451 = TRUE;
FSETBO1548397318 BOUND_210358778 = ((FSETBO1548397318) NULL);
const INT FSETBO1172349319 = 5;
const BOOL FSETBO216310762 = FALSE;
const INT FSETBO717550434 = 4;
const BOOL FSETBO2074935273 = TRUE;
FSETIM184436961 IMMUTA489824193 = ((FSETIM184436961) NULL);
const INT FSETIM924207918 = 5;
const BOOL FSETIM742607465 = FALSE;
const INT FSETIM2015417903 = 4;
const BOOL FSETIM1693735320 = TRUE;
FSETCLASS_LAYOUT CLASS_368803185 = ((FSETCLASS_LAYOUT) NULL);
const INT FSETCL786981258 = 5;
const BOOL FSETCL2058817423 = FALSE;
const INT FSETCL497069731 = 4;
const BOOL FSETCL377525362 = TRUE;
const INT FSETdL344753731 = 5;
const BOOL FSETdL901472145 = FALSE;
const INT FSETdL1013454714 = 4;
const BOOL FSETdL1534870640 = TRUE;
FMAPSI2103621588 ABSTRA854284101 = ((FMAPSI2103621588) NULL);
FMAPSI1121373188 FRAME_1643038396 = ((FMAPSI1121373188) NULL);
INT FILE_M852907408 = 24;
FSETSTR FILE_M683825354 = ((FSETSTR) NULL);
const INT FMAPAM1649190656 = 4;
const INT FMAPAM673663981 = 4;
FLISTSIG PRINT_OB_funcs = ((FLISTSIG) NULL);
const INT FSETAM687490432 = 5;
const BOOL FSETAM40978445 = FALSE;
const INT FSETAM34126943 = 4;
const BOOL FSETAM1817646066 = TRUE;
FMAPAM1357596931 BOUND_2051717332 = ((FMAPAM1357596931) NULL);
const INT FMAPAM29230850 = 4;
const INT FSETAM744778340 = 5;
const BOOL FSETAM1208812133 = FALSE;
const INT FSETAM154189325 = 4;
const BOOL FSETAM649812378 = TRUE;
FLT FLT_log2_e;
const INT FMAPSI362068854 = 4;
const INT FMAPSI1801446319 = 4;
FMAPAM339652544 BOUND_982289559 = ((FMAPAM339652544) NULL);
const INT FMAPAM441967257 = 4;
const INT ARRAYd432394813 = 10;
MANGLE PRINT_OB_mangler = ((MANGLE) NULL);
INT PRINT_1541629256 = 0;
CALL_TP_VOID CALL_T2135634286 = ((CALL_TP_VOID) NULL);
CALL_TP_CREATE CALL_T1560699295 = ((CALL_TP_CREATE) NULL);
CALL_T561792367 CALL_T714618297 = ((CALL_T561792367) NULL);
CALL_TP_ARRAY CALL_T1969158839 = ((CALL_TP_ARRAY) NULL);
TP_CLASS TP_BUILTIN_flti = ((TP_CLASS) NULL);
const INT FMAPID1853260260 = 4;
const INT FMAPdT290960897 = 4;
const BOOL TRANS_89465360 = TRUE;
const BOOL TRANS_1975351488 = TRUE;
const INT TRANS_1924362320 = 1;
const INT TRANS_194341863 = 2;
const INT FSETSF1603235078 = 5;
const BOOL FSETSF1336677965 = FALSE;
const INT FSETSF1238115389 = 4;
const BOOL FSETSF521946546 = TRUE;
INT IDENT_tmp_count = INT_zero;
const INT TRANS_par_code = 12;
const INT TRANS_frk_code = 11;
const INT TRANS_att_code = 13;
const BOOL TRANS_2136297923 = FALSE;
const INT WEIGH_879105801 = 1;
const INT WEIGH_803917570 = 2;
const INT WEIGH_1480681287 = 1;
const INT WEIGH_743880746 = 4;
INT INLINE1401026874 = INT_zero;
INT INLINE598259372 = INT_zero;
INT INLINE1484868694 = INT_zero;
INT INLINE520918393 = INT_zero;
INT INLINE2014794524 = INT_zero;
INT INLINE545315961 = INT_zero;
INT INLINE1516217386 = INT_zero;
PROG AS_OUT_prog = ((PROG) NULL);
INT AS_OUT_indent = INT_zero;
const INT FMAPdT1555535134 = 4;
const INT FSETTU1908122889 = 5;
const BOOL FSETTU215930357 = FALSE;
const INT FSETTU697583666 = 4;
const BOOL FSETTU1642694154 = TRUE;
const INT FMAPSI22065355 = 4;
const INT ARRAYA876812706 = 10;
const INT CODE_F1127805475 = 10;
FLISTCODE_FILE CODE_FILE_todo = ((FLISTCODE_FILE) NULL);
FLISTdLAYOUT CODE_F19188927 = ((FLISTdLAYOUT) NULL);
FLISTSTR CODE_F1806827531 = ((FLISTSTR) NULL);
FMAPSI1349988702 ARG_LA2092120054 = ((FMAPSI1349988702) NULL);
const INT LAYOUT558382138 = 10;
const INT LAYOUT1889414835 = 10;
const INT ARRAYF881388824 = 10;
