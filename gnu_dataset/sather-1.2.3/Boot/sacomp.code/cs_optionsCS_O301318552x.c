#include "sather.h"

/* Layouts */

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct CS_struct {/* layout for CS */
 OB_HEADER header;
 BOOL only_parse;
 BOOL only_reachable;
 } *CS;

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

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

typedef struct TUPSTRFSETSTR_struct {/* layout for TUP{STR,FSET{STR}} */
 struct FSETSTR_struct *t2;
 STR t1;
 } TUPSTRFSETSTR;
static TUPSTRFSETSTR TUPSTRFSETSTR_zero;

typedef struct TUPSTRFSETSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRFSETSTR immutable_part;
 } *TUPSTRFSETSTR_boxed;

typedef struct UNIX_struct {/* layout for UNIX */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *UNIX;

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

typedef struct FMAPSTRFSETSTR_struct {/* layout for FMAP{STR,FSET{STR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FMAPSTRFSETSTR;

typedef struct FMAPST1155259492_frame_struct {
 INT state;
 FMAPSTRFSETSTR self;/* Formal argument: self */
 TUPSTRFSETSTR ret_val2;
 FMAPSTRFSETSTR L61;
 TUPSTRFSETSTR r;
 INT L31;
 FMAPSTRFSETSTR is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1155259492_frame;

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

BOOL STR_is111530248(STR, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
INT FLISTS326330076(FLISTSTR);
STR CS_OPT1597678447(CS_OPTIONS);
STR CS_OPT1922122788(CS_OPTIONS, STR, STR, STR);
STR CS_OPT1970940548(CS_OPTIONS);
STR CS_OPT499558595(CS_OPTIONS);
STR CS_OPT565500779(CS_OPTIONS);
STR FLISTS1171631134(FLISTS1171631134_frame);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_pl1270664985(STR, dSTR);
STR STR_se405450305(STR_se405450305_frame);
STR UNIX_g108606936(UNIX, STR);
STR UNIX_s517080793(UNIX);
TUPSTRFSETSTR FMAPST1155259492(FMAPST1155259492_frame);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void CS_OPT611018296(CS_OPTIONS);
void CS_OPT63514076(CS_OPTIONS, ARRAYSTR);
void CS_OPT65198073(CS_OPTIONS, STR);
void CS_OPT994959939(CS_OPTIONS);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1597678447(CS_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 FSETSTR L21;
 FSETSTR L31;
 FSETSTR L41;
 FSETSTR L51;
 FSETSTR L61;
 FSETSTR L71;
 FSETSTR L81;
 FSETSTR L91;
 FSETSTR L101;
 FSETSTR L121;
 FSETSTR L131;
 FSETSTR L141;
 FSETSTR L151;
 FSETSTR L161;
 FSETSTR L171;
 FSETSTR L181;
 FSETSTR L191;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 CHAR plus_arg = CHAR_zero;
 STR ret_val2;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val3;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val4;
 STR r1;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val6;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val7;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val8;
 STR r2;
 STR plus_self4;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self5;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self6;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val11;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val12;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val13;
 STR r3;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val14;
 STR plus_self8;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val15;
 STR s4;
 CHAR str_self3 = CHAR_zero;
 STR ret_val16;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val17;
 STR r4;
 STR plus_self9;
 STR plus_sarg5;
 STR ret_val18;
 STR plus_self10;
 STR plus_sarg6;
 STR ret_val19;
 STR plus_self11;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val20;
 STR s5;
 CHAR str_self4 = CHAR_zero;
 STR ret_val21;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val22;
 STR r5;
 STR plus_self12;
 STR plus_sarg7;
 STR ret_val23;
 STR plus_self13;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val24;
 STR s6;
 CHAR str_self5 = CHAR_zero;
 STR ret_val25;
 STR create_self5;
 CHAR create_c5 = CHAR_zero;
 STR ret_val26;
 STR r6;
 STR plus_self14;
 STR plus_sarg8;
 STR ret_val27;
 STR plus_self15;
 STR plus_sarg9;
 STR ret_val28;
 STR plus_self16;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val29;
 STR s7;
 CHAR str_self6 = CHAR_zero;
 STR ret_val30;
 STR create_self6;
 CHAR create_c6 = CHAR_zero;
 STR ret_val31;
 STR r7;
 STR plus_self17;
 STR plus_sarg10;
 STR ret_val32;
 STR plus_self18;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val33;
 STR s8;
 CHAR str_self7 = CHAR_zero;
 STR ret_val34;
 STR create_self7;
 CHAR create_c7 = CHAR_zero;
 STR ret_val35;
 STR r8;
 STR plus_self19;
 STR plus_sarg11;
 STR ret_val36;
 STR plus_self20;
 STR plus_sarg12;
 STR ret_val37;
 STR plus_self21;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val38;
 STR s9;
 CHAR str_self8 = CHAR_zero;
 STR ret_val39;
 STR create_self8;
 CHAR create_c8 = CHAR_zero;
 STR ret_val40;
 STR r9;
 STR plus_self22;
 STR plus_sarg13;
 STR ret_val41;
 STR plus_self23;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val42;
 STR s10;
 CHAR str_self9 = CHAR_zero;
 STR ret_val43;
 STR create_self9;
 CHAR create_c9 = CHAR_zero;
 STR ret_val44;
 STR r10;
 STR plus_self24;
 STR plus_sarg14;
 STR ret_val45;
 STR plus_self25;
 STR plus_sarg15;
 STR ret_val46;
 STR plus_self26;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val47;
 STR s11;
 CHAR str_self10 = CHAR_zero;
 STR ret_val48;
 STR create_self10;
 CHAR create_c10 = CHAR_zero;
 STR ret_val49;
 STR r11;
 STR plus_self27;
 STR plus_sarg16;
 STR ret_val50;
 STR plus_self28;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val51;
 STR s12;
 CHAR str_self11 = CHAR_zero;
 STR ret_val52;
 STR create_self11;
 CHAR create_c11 = CHAR_zero;
 STR ret_val53;
 STR r12;
 STR plus_self29;
 STR plus_sarg17;
 STR ret_val54;
 STR plus_self30;
 STR plus_sarg18;
 STR ret_val55;
 STR plus_self31;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val56;
 STR s13;
 CHAR str_self12 = CHAR_zero;
 STR ret_val57;
 STR create_self12;
 CHAR create_c12 = CHAR_zero;
 STR ret_val58;
 STR r13;
 STR plus_self32;
 STR plus_sarg19;
 STR ret_val59;
 STR plus_self33;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val60;
 STR s14;
 CHAR str_self13 = CHAR_zero;
 STR ret_val61;
 STR create_self13;
 CHAR create_c13 = CHAR_zero;
 STR ret_val62;
 STR r14;
 STR plus_self34;
 STR plus_sarg20;
 STR ret_val63;
 STR plus_self35;
 STR plus_sarg21;
 STR ret_val64;
 STR plus_self36;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val65;
 STR s15;
 CHAR str_self14 = CHAR_zero;
 STR ret_val66;
 STR create_self14;
 CHAR create_c14 = CHAR_zero;
 STR ret_val67;
 STR r15;
 STR plus_self37;
 STR plus_sarg22;
 STR ret_val68;
 STR plus_self38;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val69;
 STR s16;
 CHAR str_self15 = CHAR_zero;
 STR ret_val70;
 STR create_self15;
 CHAR create_c15 = CHAR_zero;
 STR ret_val71;
 STR r16;
 STR plus_self39;
 STR plus_sarg23;
 STR ret_val72;
 STR plus_self40;
 STR plus_sarg24;
 STR ret_val73;
 STR plus_self41;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val74;
 STR s17;
 CHAR str_self16 = CHAR_zero;
 STR ret_val75;
 STR create_self16;
 CHAR create_c16 = CHAR_zero;
 STR ret_val76;
 STR r17;
 STR plus_self42;
 STR plus_sarg25;
 STR ret_val77;
 STR plus_self43;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val78;
 STR s18;
 CHAR str_self17 = CHAR_zero;
 STR ret_val79;
 STR create_self17;
 CHAR create_c17 = CHAR_zero;
 STR ret_val80;
 STR r18;
 STR plus_self44;
 STR plus_sarg26;
 STR ret_val81;
 extern STR name1;
 extern STR All;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 STR L26;
 STR L27;
 INT L28;
 OB L29;
 INT L30;
 STR L33;
 extern STR Preconditions;
 extern STR name1;
 extern STR All;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 STR L39;
 STR L40;
 INT L42;
 OB L43;
 INT L44;
 STR L46;
 extern STR Postconditions;
 extern STR name1;
 extern STR All;
 STR L47;
 INT L48;
 OB L49;
 INT L50;
 STR L53;
 STR L54;
 INT L55;
 OB L56;
 INT L57;
 STR L59;
 extern STR Invariants;
 extern STR name1;
 extern STR All;
 STR L60;
 INT L62;
 OB L63;
 INT L64;
 STR L66;
 STR L67;
 INT L68;
 OB L69;
 INT L70;
 STR L73;
 extern STR Asserts;
 extern STR name1;
 extern STR All;
 STR L74;
 INT L75;
 OB L76;
 INT L77;
 STR L79;
 STR L80;
 INT L82;
 OB L83;
 INT L84;
 STR L86;
 extern STR Voidchecks;
 extern STR name1;
 extern STR All;
 STR L87;
 INT L88;
 OB L89;
 INT L90;
 STR L93;
 STR L94;
 INT L95;
 OB L96;
 INT L97;
 STR L99;
 extern STR Whenclause;
 extern STR name1;
 extern STR All;
 STR L100;
 INT L102;
 OB L103;
 INT L104;
 STR L106;
 STR L107;
 INT L108;
 OB L109;
 INT L110;
 STR L112;
 extern STR Bounds;
 extern STR On;
 extern STR Off;
 extern STR Destroy;
 extern STR On;
 extern STR Off;
 extern STR pSatherchecks;
 extern STR On;
 extern STR Off;
 extern STR Statis1655725615;
 extern STR On;
 extern STR Off;
 extern STR pSatherstats;
 extern STR On;
 extern STR Off;
 extern STR pSathertrace;
 extern STR name1;
 extern STR All;
 STR L113;
 INT L114;
 OB L115;
 INT L116;
 STR L118;
 STR L119;
 INT L120;
 OB L122;
 INT L123;
 STR L125;
 extern STR Arith;
 extern STR name1;
 extern STR All;
 STR L126;
 INT L127;
 OB L128;
 INT L129;
 STR L132;
 STR L133;
 INT L134;
 OB L135;
 INT L136;
 STR L138;
 extern STR Return;
 s = CS_OPT499558595(self);
 r = ((STR) &name1);
 if (ATTR(self,pre_all)) {
  plus_self = r;
  plus_sarg = ((STR) &All);
  ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
  r = ret_val1;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
   noname1->state = 0;
   while (1) {
    plus_self1 = r;
    plus_arg = '+';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L22 = 1;
    L24=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L22)*sizeof(CHAR);
    L23=ZALLOC_LEAF_BIG(L24);
    if (L23==NULL) FATAL("Unable to allocate more memory");
    memset(L23,0,L24);
    ((OB)L23)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L23)->header.destroyed=0;
#endif

    L20 = ((STR) L23);
    L20->asize = L22;
    r1 = L20;
    SARR((STR)r1,0,create_c); 
    ;
    ret_val4 = r1;
    ret_val3 = ret_val4;
    s1 = ret_val3;
    ret_val2 = STR_ap1077157958(plus_self1, s1, TRUE);
    plus_self2 = ret_val2;
    if (noname1->state == 0) {
     L11 = ATTR(self,pre_in);
     noname1->self = L11;
    }
    L26 = FSETSTR_eltbrSTR(noname1);
    if (noname1->state == -1) {
     goto after_loop;
    }
    plus_sarg1 = L26;
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
    r = ret_val5;
   }
  }
  after_loop: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other2_0;
FSETSTR_eltbrSTR_frame noname2 = &other2_0;
  noname2->state = 0;
  while (1) {
   plus_self3 = r;
   plus_arg1 = '-';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L28 = 1;
   L30=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L28)*sizeof(CHAR);
   L29=ZALLOC_LEAF_BIG(L30);
   if (L29==NULL) FATAL("Unable to allocate more memory");
   memset(L29,0,L30);
   ((OB)L29)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L29)->header.destroyed=0;
#endif

   L27 = ((STR) L29);
   L27->asize = L28;
   r2 = L27;
   SARR((STR)r2,0,create_c1); 
   ;
   ret_val8 = r2;
   ret_val7 = ret_val8;
   s2 = ret_val7;
   ret_val6 = STR_ap1077157958(plus_self3, s2, TRUE);
   plus_self4 = ret_val6;
   if (noname2->state == 0) {
    L21 = ATTR(self,pre_out);
    noname2->self = L21;
   }
   L33 = FSETSTR_eltbrSTR(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   plus_sarg2 = L33;
   ret_val9 = STR_ap2004550586(plus_self4, plus_sarg2);
   r = ret_val9;
  }
 }
 after_loop1: ;
 s = CS_OPT1922122788(self, ((STR) &Preconditions), s, r);
 r = ((STR) &name1);
 if (ATTR(self,post_all)) {
  plus_self5 = r;
  plus_sarg3 = ((STR) &All);
  ret_val10 = STR_ap2004550586(plus_self5, plus_sarg3);
  r = ret_val10;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other3_0;
FSETSTR_eltbrSTR_frame noname3 = &other3_0;
   noname3->state = 0;
   while (1) {
    plus_self6 = r;
    plus_arg2 = '+';
    str_self2 = plus_arg2;
    create_self2 = ((STR) NULL);
    create_c2 = str_self2;
    L35 = 1;
    L37=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L35)*sizeof(CHAR);
    L36=ZALLOC_LEAF_BIG(L37);
    if (L36==NULL) FATAL("Unable to allocate more memory");
    memset(L36,0,L37);
    ((OB)L36)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L36)->header.destroyed=0;
#endif

    L34 = ((STR) L36);
    L34->asize = L35;
    r3 = L34;
    SARR((STR)r3,0,create_c2); 
    ;
    ret_val13 = r3;
    ret_val12 = ret_val13;
    s3 = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self6, s3, TRUE);
    plus_self7 = ret_val11;
    if (noname3->state == 0) {
     L31 = ATTR(self,post_in);
     noname3->self = L31;
    }
    L39 = FSETSTR_eltbrSTR(noname3);
    if (noname3->state == -1) {
     goto after_loop2;
    }
    plus_sarg4 = L39;
    ret_val14 = STR_ap2004550586(plus_self7, plus_sarg4);
    r = ret_val14;
   }
  }
  after_loop2: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other4_0;
FSETSTR_eltbrSTR_frame noname4 = &other4_0;
  noname4->state = 0;
  while (1) {
   plus_self8 = r;
   plus_arg3 = '-';
   str_self3 = plus_arg3;
   create_self3 = ((STR) NULL);
   create_c3 = str_self3;
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
   r4 = L40;
   SARR((STR)r4,0,create_c3); 
   ;
   ret_val17 = r4;
   ret_val16 = ret_val17;
   s4 = ret_val16;
   ret_val15 = STR_ap1077157958(plus_self8, s4, TRUE);
   plus_self9 = ret_val15;
   if (noname4->state == 0) {
    L41 = ATTR(self,post_out);
    noname4->self = L41;
   }
   L46 = FSETSTR_eltbrSTR(noname4);
   if (noname4->state == -1) {
    goto after_loop3;
   }
   plus_sarg5 = L46;
   ret_val18 = STR_ap2004550586(plus_self9, plus_sarg5);
   r = ret_val18;
  }
 }
 after_loop3: ;
 s = CS_OPT1922122788(self, ((STR) &Postconditions), s, r);
 r = ((STR) &name1);
 if (ATTR(self,invariant_all)) {
  plus_self10 = r;
  plus_sarg6 = ((STR) &All);
  ret_val19 = STR_ap2004550586(plus_self10, plus_sarg6);
  r = ret_val19;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other5_0;
FSETSTR_eltbrSTR_frame noname5 = &other5_0;
   noname5->state = 0;
   while (1) {
    plus_self11 = r;
    plus_arg4 = '+';
    str_self4 = plus_arg4;
    create_self4 = ((STR) NULL);
    create_c4 = str_self4;
    L48 = 1;
    L50=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L48)*sizeof(CHAR);
    L49=ZALLOC_LEAF_BIG(L50);
    if (L49==NULL) FATAL("Unable to allocate more memory");
    memset(L49,0,L50);
    ((OB)L49)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L49)->header.destroyed=0;
#endif

    L47 = ((STR) L49);
    L47->asize = L48;
    r5 = L47;
    SARR((STR)r5,0,create_c4); 
    ;
    ret_val22 = r5;
    ret_val21 = ret_val22;
    s5 = ret_val21;
    ret_val20 = STR_ap1077157958(plus_self11, s5, TRUE);
    plus_self12 = ret_val20;
    if (noname5->state == 0) {
     L51 = ATTR(self,invariant_in);
     noname5->self = L51;
    }
    L53 = FSETSTR_eltbrSTR(noname5);
    if (noname5->state == -1) {
     goto after_loop4;
    }
    plus_sarg7 = L53;
    ret_val23 = STR_ap2004550586(plus_self12, plus_sarg7);
    r = ret_val23;
   }
  }
  after_loop4: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other6_0;
FSETSTR_eltbrSTR_frame noname6 = &other6_0;
  noname6->state = 0;
  while (1) {
   plus_self13 = r;
   plus_arg5 = '-';
   str_self5 = plus_arg5;
   create_self5 = ((STR) NULL);
   create_c5 = str_self5;
   L55 = 1;
   L57=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L55)*sizeof(CHAR);
   L56=ZALLOC_LEAF_BIG(L57);
   if (L56==NULL) FATAL("Unable to allocate more memory");
   memset(L56,0,L57);
   ((OB)L56)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L56)->header.destroyed=0;
#endif

   L54 = ((STR) L56);
   L54->asize = L55;
   r6 = L54;
   SARR((STR)r6,0,create_c5); 
   ;
   ret_val26 = r6;
   ret_val25 = ret_val26;
   s6 = ret_val25;
   ret_val24 = STR_ap1077157958(plus_self13, s6, TRUE);
   plus_self14 = ret_val24;
   if (noname6->state == 0) {
    L61 = ATTR(self,invariant_out);
    noname6->self = L61;
   }
   L59 = FSETSTR_eltbrSTR(noname6);
   if (noname6->state == -1) {
    goto after_loop5;
   }
   plus_sarg8 = L59;
   ret_val27 = STR_ap2004550586(plus_self14, plus_sarg8);
   r = ret_val27;
  }
 }
 after_loop5: ;
 s = CS_OPT1922122788(self, ((STR) &Invariants), s, r);
 r = ((STR) &name1);
 if (ATTR(self,assert_all)) {
  plus_self15 = r;
  plus_sarg9 = ((STR) &All);
  ret_val28 = STR_ap2004550586(plus_self15, plus_sarg9);
  r = ret_val28;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other7_0;
FSETSTR_eltbrSTR_frame noname7 = &other7_0;
   noname7->state = 0;
   while (1) {
    plus_self16 = r;
    plus_arg6 = '+';
    str_self6 = plus_arg6;
    create_self6 = ((STR) NULL);
    create_c6 = str_self6;
    L62 = 1;
    L64=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L62)*sizeof(CHAR);
    L63=ZALLOC_LEAF_BIG(L64);
    if (L63==NULL) FATAL("Unable to allocate more memory");
    memset(L63,0,L64);
    ((OB)L63)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L63)->header.destroyed=0;
#endif

    L60 = ((STR) L63);
    L60->asize = L62;
    r7 = L60;
    SARR((STR)r7,0,create_c6); 
    ;
    ret_val31 = r7;
    ret_val30 = ret_val31;
    s7 = ret_val30;
    ret_val29 = STR_ap1077157958(plus_self16, s7, TRUE);
    plus_self17 = ret_val29;
    if (noname7->state == 0) {
     L71 = ATTR(self,assert_in);
     noname7->self = L71;
    }
    L66 = FSETSTR_eltbrSTR(noname7);
    if (noname7->state == -1) {
     goto after_loop6;
    }
    plus_sarg10 = L66;
    ret_val32 = STR_ap2004550586(plus_self17, plus_sarg10);
    r = ret_val32;
   }
  }
  after_loop6: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other8_0;
FSETSTR_eltbrSTR_frame noname8 = &other8_0;
  noname8->state = 0;
  while (1) {
   plus_self18 = r;
   plus_arg7 = '-';
   str_self7 = plus_arg7;
   create_self7 = ((STR) NULL);
   create_c7 = str_self7;
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
   r8 = L67;
   SARR((STR)r8,0,create_c7); 
   ;
   ret_val35 = r8;
   ret_val34 = ret_val35;
   s8 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self18, s8, TRUE);
   plus_self19 = ret_val33;
   if (noname8->state == 0) {
    L81 = ATTR(self,assert_out);
    noname8->self = L81;
   }
   L73 = FSETSTR_eltbrSTR(noname8);
   if (noname8->state == -1) {
    goto after_loop7;
   }
   plus_sarg11 = L73;
   ret_val36 = STR_ap2004550586(plus_self19, plus_sarg11);
   r = ret_val36;
  }
 }
 after_loop7: ;
 s = CS_OPT1922122788(self, ((STR) &Asserts), s, r);
 r = ((STR) &name1);
 if (ATTR(self,void_all)) {
  plus_self20 = r;
  plus_sarg12 = ((STR) &All);
  ret_val37 = STR_ap2004550586(plus_self20, plus_sarg12);
  r = ret_val37;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other9_0;
FSETSTR_eltbrSTR_frame noname9 = &other9_0;
   noname9->state = 0;
   while (1) {
    plus_self21 = r;
    plus_arg8 = '+';
    str_self8 = plus_arg8;
    create_self8 = ((STR) NULL);
    create_c8 = str_self8;
    L75 = 1;
    L77=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L75)*sizeof(CHAR);
    L76=ZALLOC_LEAF_BIG(L77);
    if (L76==NULL) FATAL("Unable to allocate more memory");
    memset(L76,0,L77);
    ((OB)L76)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L76)->header.destroyed=0;
#endif

    L74 = ((STR) L76);
    L74->asize = L75;
    r9 = L74;
    SARR((STR)r9,0,create_c8); 
    ;
    ret_val40 = r9;
    ret_val39 = ret_val40;
    s9 = ret_val39;
    ret_val38 = STR_ap1077157958(plus_self21, s9, TRUE);
    plus_self22 = ret_val38;
    if (noname9->state == 0) {
     L91 = ATTR(self,void_in);
     noname9->self = L91;
    }
    L79 = FSETSTR_eltbrSTR(noname9);
    if (noname9->state == -1) {
     goto after_loop8;
    }
    plus_sarg13 = L79;
    ret_val41 = STR_ap2004550586(plus_self22, plus_sarg13);
    r = ret_val41;
   }
  }
  after_loop8: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other10_0;
FSETSTR_eltbrSTR_frame noname10 = &other10_0;
  noname10->state = 0;
  while (1) {
   plus_self23 = r;
   plus_arg9 = '-';
   str_self9 = plus_arg9;
   create_self9 = ((STR) NULL);
   create_c9 = str_self9;
   L82 = 1;
   L84=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L82)*sizeof(CHAR);
   L83=ZALLOC_LEAF_BIG(L84);
   if (L83==NULL) FATAL("Unable to allocate more memory");
   memset(L83,0,L84);
   ((OB)L83)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L83)->header.destroyed=0;
#endif

   L80 = ((STR) L83);
   L80->asize = L82;
   r10 = L80;
   SARR((STR)r10,0,create_c9); 
   ;
   ret_val44 = r10;
   ret_val43 = ret_val44;
   s10 = ret_val43;
   ret_val42 = STR_ap1077157958(plus_self23, s10, TRUE);
   plus_self24 = ret_val42;
   if (noname10->state == 0) {
    L101 = ATTR(self,void_out);
    noname10->self = L101;
   }
   L86 = FSETSTR_eltbrSTR(noname10);
   if (noname10->state == -1) {
    goto after_loop9;
   }
   plus_sarg14 = L86;
   ret_val45 = STR_ap2004550586(plus_self24, plus_sarg14);
   r = ret_val45;
  }
 }
 after_loop9: ;
 s = CS_OPT1922122788(self, ((STR) &Voidchecks), s, r);
 r = ((STR) &name1);
 if (ATTR(self,when_all)) {
  plus_self25 = r;
  plus_sarg15 = ((STR) &All);
  ret_val46 = STR_ap2004550586(plus_self25, plus_sarg15);
  r = ret_val46;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other11_0;
FSETSTR_eltbrSTR_frame noname11 = &other11_0;
   noname11->state = 0;
   while (1) {
    plus_self26 = r;
    plus_arg10 = '+';
    str_self10 = plus_arg10;
    create_self10 = ((STR) NULL);
    create_c10 = str_self10;
    L88 = 1;
    L90=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L88)*sizeof(CHAR);
    L89=ZALLOC_LEAF_BIG(L90);
    if (L89==NULL) FATAL("Unable to allocate more memory");
    memset(L89,0,L90);
    ((OB)L89)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L89)->header.destroyed=0;
#endif

    L87 = ((STR) L89);
    L87->asize = L88;
    r11 = L87;
    SARR((STR)r11,0,create_c10); 
    ;
    ret_val49 = r11;
    ret_val48 = ret_val49;
    s11 = ret_val48;
    ret_val47 = STR_ap1077157958(plus_self26, s11, TRUE);
    plus_self27 = ret_val47;
    if (noname11->state == 0) {
     L121 = ATTR(self,when_in);
     noname11->self = L121;
    }
    L93 = FSETSTR_eltbrSTR(noname11);
    if (noname11->state == -1) {
     goto after_loop10;
    }
    plus_sarg16 = L93;
    ret_val50 = STR_ap2004550586(plus_self27, plus_sarg16);
    r = ret_val50;
   }
  }
  after_loop10: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other12_0;
FSETSTR_eltbrSTR_frame noname12 = &other12_0;
  noname12->state = 0;
  while (1) {
   plus_self28 = r;
   plus_arg11 = '-';
   str_self11 = plus_arg11;
   create_self11 = ((STR) NULL);
   create_c11 = str_self11;
   L95 = 1;
   L97=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L95)*sizeof(CHAR);
   L96=ZALLOC_LEAF_BIG(L97);
   if (L96==NULL) FATAL("Unable to allocate more memory");
   memset(L96,0,L97);
   ((OB)L96)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L96)->header.destroyed=0;
#endif

   L94 = ((STR) L96);
   L94->asize = L95;
   r12 = L94;
   SARR((STR)r12,0,create_c11); 
   ;
   ret_val53 = r12;
   ret_val52 = ret_val53;
   s12 = ret_val52;
   ret_val51 = STR_ap1077157958(plus_self28, s12, TRUE);
   plus_self29 = ret_val51;
   if (noname12->state == 0) {
    L131 = ATTR(self,when_out);
    noname12->self = L131;
   }
   L99 = FSETSTR_eltbrSTR(noname12);
   if (noname12->state == -1) {
    goto after_loop11;
   }
   plus_sarg17 = L99;
   ret_val54 = STR_ap2004550586(plus_self29, plus_sarg17);
   r = ret_val54;
  }
 }
 after_loop11: ;
 s = CS_OPT1922122788(self, ((STR) &Whenclause), s, r);
 r = ((STR) &name1);
 if (ATTR(self,bounds_all)) {
  plus_self30 = r;
  plus_sarg18 = ((STR) &All);
  ret_val55 = STR_ap2004550586(plus_self30, plus_sarg18);
  r = ret_val55;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other13_0;
FSETSTR_eltbrSTR_frame noname13 = &other13_0;
   noname13->state = 0;
   while (1) {
    plus_self31 = r;
    plus_arg12 = '+';
    str_self12 = plus_arg12;
    create_self12 = ((STR) NULL);
    create_c12 = str_self12;
    L102 = 1;
    L104=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L102)*sizeof(CHAR);
    L103=ZALLOC_LEAF_BIG(L104);
    if (L103==NULL) FATAL("Unable to allocate more memory");
    memset(L103,0,L104);
    ((OB)L103)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L103)->header.destroyed=0;
#endif

    L100 = ((STR) L103);
    L100->asize = L102;
    r13 = L100;
    SARR((STR)r13,0,create_c12); 
    ;
    ret_val58 = r13;
    ret_val57 = ret_val58;
    s13 = ret_val57;
    ret_val56 = STR_ap1077157958(plus_self31, s13, TRUE);
    plus_self32 = ret_val56;
    if (noname13->state == 0) {
     L141 = ATTR(self,bounds_in);
     noname13->self = L141;
    }
    L106 = FSETSTR_eltbrSTR(noname13);
    if (noname13->state == -1) {
     goto after_loop12;
    }
    plus_sarg19 = L106;
    ret_val59 = STR_ap2004550586(plus_self32, plus_sarg19);
    r = ret_val59;
   }
  }
  after_loop12: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other14_0;
FSETSTR_eltbrSTR_frame noname14 = &other14_0;
  noname14->state = 0;
  while (1) {
   plus_self33 = r;
   plus_arg13 = '-';
   str_self13 = plus_arg13;
   create_self13 = ((STR) NULL);
   create_c13 = str_self13;
   L108 = 1;
   L110=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L108)*sizeof(CHAR);
   L109=ZALLOC_LEAF_BIG(L110);
   if (L109==NULL) FATAL("Unable to allocate more memory");
   memset(L109,0,L110);
   ((OB)L109)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L109)->header.destroyed=0;
#endif

   L107 = ((STR) L109);
   L107->asize = L108;
   r14 = L107;
   SARR((STR)r14,0,create_c13); 
   ;
   ret_val62 = r14;
   ret_val61 = ret_val62;
   s14 = ret_val61;
   ret_val60 = STR_ap1077157958(plus_self33, s14, TRUE);
   plus_self34 = ret_val60;
   if (noname14->state == 0) {
    L151 = ATTR(self,bounds_out);
    noname14->self = L151;
   }
   L112 = FSETSTR_eltbrSTR(noname14);
   if (noname14->state == -1) {
    goto after_loop13;
   }
   plus_sarg20 = L112;
   ret_val63 = STR_ap2004550586(plus_self34, plus_sarg20);
   r = ret_val63;
  }
 }
 after_loop13: ;
 s = CS_OPT1922122788(self, ((STR) &Bounds), s, r);
 if (ATTR(self,destroy_chk)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Destroy), s, r);
 if (ATTR(self,psather_chk)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &pSatherchecks), s, r);
 if (ATTR(self,stats)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Statis1655725615), s, r);
 if (ATTR(self,psather_stats)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &pSatherstats), s, r);
 if (ATTR(self,psather_trace)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &pSathertrace), s, r);
 r = ((STR) &name1);
 if (ATTR(self,arith_all)) {
  plus_self35 = r;
  plus_sarg21 = ((STR) &All);
  ret_val64 = STR_ap2004550586(plus_self35, plus_sarg21);
  r = ret_val64;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other15_0;
FSETSTR_eltbrSTR_frame noname15 = &other15_0;
   noname15->state = 0;
   while (1) {
    plus_self36 = r;
    plus_arg14 = '+';
    str_self14 = plus_arg14;
    create_self14 = ((STR) NULL);
    create_c14 = str_self14;
    L114 = 1;
    L116=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L114)*sizeof(CHAR);
    L115=ZALLOC_LEAF_BIG(L116);
    if (L115==NULL) FATAL("Unable to allocate more memory");
    memset(L115,0,L116);
    ((OB)L115)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L115)->header.destroyed=0;
#endif

    L113 = ((STR) L115);
    L113->asize = L114;
    r15 = L113;
    SARR((STR)r15,0,create_c14); 
    ;
    ret_val67 = r15;
    ret_val66 = ret_val67;
    s15 = ret_val66;
    ret_val65 = STR_ap1077157958(plus_self36, s15, TRUE);
    plus_self37 = ret_val65;
    if (noname15->state == 0) {
     L161 = ATTR(self,arith_in);
     noname15->self = L161;
    }
    L118 = FSETSTR_eltbrSTR(noname15);
    if (noname15->state == -1) {
     goto after_loop14;
    }
    plus_sarg22 = L118;
    ret_val68 = STR_ap2004550586(plus_self37, plus_sarg22);
    r = ret_val68;
   }
  }
  after_loop14: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other16_0;
FSETSTR_eltbrSTR_frame noname16 = &other16_0;
  noname16->state = 0;
  while (1) {
   plus_self38 = r;
   plus_arg15 = '-';
   str_self15 = plus_arg15;
   create_self15 = ((STR) NULL);
   create_c15 = str_self15;
   L120 = 1;
   L123=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L120)*sizeof(CHAR);
   L122=ZALLOC_LEAF_BIG(L123);
   if (L122==NULL) FATAL("Unable to allocate more memory");
   memset(L122,0,L123);
   ((OB)L122)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L122)->header.destroyed=0;
#endif

   L119 = ((STR) L122);
   L119->asize = L120;
   r16 = L119;
   SARR((STR)r16,0,create_c15); 
   ;
   ret_val71 = r16;
   ret_val70 = ret_val71;
   s16 = ret_val70;
   ret_val69 = STR_ap1077157958(plus_self38, s16, TRUE);
   plus_self39 = ret_val69;
   if (noname16->state == 0) {
    L171 = ATTR(self,arith_out);
    noname16->self = L171;
   }
   L125 = FSETSTR_eltbrSTR(noname16);
   if (noname16->state == -1) {
    goto after_loop15;
   }
   plus_sarg23 = L125;
   ret_val72 = STR_ap2004550586(plus_self39, plus_sarg23);
   r = ret_val72;
  }
 }
 after_loop15: ;
 s = CS_OPT1922122788(self, ((STR) &Arith), s, r);
 r = ((STR) &name1);
 if (ATTR(self,return_all)) {
  plus_self40 = r;
  plus_sarg24 = ((STR) &All);
  ret_val73 = STR_ap2004550586(plus_self40, plus_sarg24);
  r = ret_val73;
 }
 else {
  {
   struct FSETSTR_eltbrSTR_frame_struct other17_0;
FSETSTR_eltbrSTR_frame noname17 = &other17_0;
   noname17->state = 0;
   while (1) {
    plus_self41 = r;
    plus_arg16 = '+';
    str_self16 = plus_arg16;
    create_self16 = ((STR) NULL);
    create_c16 = str_self16;
    L127 = 1;
    L129=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L127)*sizeof(CHAR);
    L128=ZALLOC_LEAF_BIG(L129);
    if (L128==NULL) FATAL("Unable to allocate more memory");
    memset(L128,0,L129);
    ((OB)L128)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L128)->header.destroyed=0;
#endif

    L126 = ((STR) L128);
    L126->asize = L127;
    r17 = L126;
    SARR((STR)r17,0,create_c16); 
    ;
    ret_val76 = r17;
    ret_val75 = ret_val76;
    s17 = ret_val75;
    ret_val74 = STR_ap1077157958(plus_self41, s17, TRUE);
    plus_self42 = ret_val74;
    if (noname17->state == 0) {
     L181 = ATTR(self,return_in);
     noname17->self = L181;
    }
    L132 = FSETSTR_eltbrSTR(noname17);
    if (noname17->state == -1) {
     goto after_loop16;
    }
    plus_sarg25 = L132;
    ret_val77 = STR_ap2004550586(plus_self42, plus_sarg25);
    r = ret_val77;
   }
  }
  after_loop16: ;
 }
 {
  struct FSETSTR_eltbrSTR_frame_struct other18_0;
FSETSTR_eltbrSTR_frame noname18 = &other18_0;
  noname18->state = 0;
  while (1) {
   plus_self43 = r;
   plus_arg17 = '-';
   str_self17 = plus_arg17;
   create_self17 = ((STR) NULL);
   create_c17 = str_self17;
   L134 = 1;
   L136=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L134)*sizeof(CHAR);
   L135=ZALLOC_LEAF_BIG(L136);
   if (L135==NULL) FATAL("Unable to allocate more memory");
   memset(L135,0,L136);
   ((OB)L135)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L135)->header.destroyed=0;
#endif

   L133 = ((STR) L135);
   L133->asize = L134;
   r18 = L133;
   SARR((STR)r18,0,create_c17); 
   ;
   ret_val80 = r18;
   ret_val79 = ret_val80;
   s18 = ret_val79;
   ret_val78 = STR_ap1077157958(plus_self43, s18, TRUE);
   plus_self44 = ret_val78;
   if (noname18->state == 0) {
    L191 = ATTR(self,return_out);
    noname18->self = L191;
   }
   L138 = FSETSTR_eltbrSTR(noname18);
   if (noname18->state == -1) {
    goto after_loop17;
   }
   plus_sarg26 = L138;
   ret_val81 = STR_ap2004550586(plus_self44, plus_sarg26);
   r = ret_val81;
  }
 }
 after_loop17: ;
 s = CS_OPT1922122788(self, ((STR) &Return), s, r);
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1922122788(CS_OPTIONS self, STR name111, STR s1, STR s2) {
 STR ret_val;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 extern STR name1;
 BOOL L1;
 BOOL L21_;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 extern STR name20;
 L1 = STR_is111530248(s2, ((STR) &name1));
 L21_=!(L1); 
 if (L21_) {
  plus_self = s1;
  plus_arg = '\n';
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
  s = ret_val2;
  ret_val1 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val1;
  plus_sarg = name111;
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
  plus_self2 = ret_val4;
  plus_sarg1 = ((STR) &name20);
  ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
  plus_self3 = ret_val5;
  plus_sarg2 = s2;
  ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
  ret_val = ret_val6;
  return ret_val;
 }
 else {
  ret_val = s1;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT499558595(CS_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r1;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val8;
 extern STR name1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 STR L7;
 extern STR Satherfiles;
 extern STR name1;
 extern STR Mainclass;
 extern STR Homedirectory;
 extern STR Prolix;
 extern STR Verbose;
 r = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   plus_self = r;
   plus_arg = ' ';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L3 = 1;
   L5=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L3)*sizeof(CHAR);
   L4=ZALLOC_LEAF_BIG(L5);
   if (L4==NULL) FATAL("Unable to allocate more memory");
   memset(L4,0,L5);
   ((OB)L4)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L4)->header.destroyed=0;
#endif

   L2 = ((STR) L4);
   L2->asize = L3;
   r1 = L2;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val3 = r1;
   ret_val2 = ret_val3;
   s1 = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
   plus_self1 = ret_val1;
   if (noname1->state == 0) {
    L11 = ATTR(ATTR(self,prog),sather_files);
    noname1->self = L11;
   }
   L7 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L7;
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
   r = ret_val4;
  }
 }
 after_loop: ;
 s = CS_OPT1922122788(self, ((STR) &Satherfiles), ((STR) &name1), r);
 s = CS_OPT1922122788(self, ((STR) &Mainclass), s, ATTR(ATTR(self,prog),main_class));
 plus_self2 = s;
 plus_sarg1 = ((STR) &Homedirectory);
 ret_val5 = STR_ap2004550586(plus_self2, plus_sarg1);
 plus_self3 = ret_val5;
 plus_sarg2 = ATTR(ATTR(self,prog),home);
 ret_val6 = STR_ap2004550586(plus_self3, plus_sarg2);
 s = ret_val6;
 if (ATTR(ATTR(self,prog),prolix)) {
  plus_self4 = s;
  plus_sarg3 = ((STR) &Prolix);
  ret_val7 = STR_ap2004550586(plus_self4, plus_sarg3);
  s = ret_val7;
 }
 if (ATTR(ATTR(self,prog),verbose)) {
  plus_self5 = s;
  plus_sarg4 = ((STR) &Verbose);
  ret_val8 = STR_ap2004550586(plus_self5, plus_sarg4);
  s = ret_val8;
 }
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT565500779(CS_OPTIONS self) {
 STR ret_val;
 STR r = ((STR) NULL);
 STR s = ((STR) NULL);
 FSETSTR L11;
 FSETSTR L21;
 FLISTSTR L31;
 FLISTSTR L41;
 FMAPSTRFSETSTR L51;
 TUPSTRFSETSTR p = TUPSTRFSETSTR_zero;
 FSETSTR L61;
 STR L71;
 FSETSTR L81;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val1;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val2;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val3;
 STR r1;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self2;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val5;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val6;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val7;
 STR r2;
 STR plus_self3;
 STR plus_sarg1;
 STR ret_val8;
 STR plus_self4;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val9;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val10;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val11;
 STR r3;
 STR plus_self5;
 STR plus_sarg2;
 STR ret_val12;
 STR plus_self6;
 STR plus_sarg3;
 STR ret_val13;
 STR plus_self7;
 STR plus_sarg4;
 STR ret_val14;
 STR plus_self8;
 STR plus_sarg5;
 STR ret_val15;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val16;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val17;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val18;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val19;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val20;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val21;
 STR plus_self15;
 INT plus_arg3 = INT_zero;
 STR ret_val22;
 STR s4;
 INT str_self3 = INT_zero;
 STR ret_val23;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val24;
 FSTR str_self4;
 STR ret_val25;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val26;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val27;
 STR plus_self18;
 INT plus_arg4 = INT_zero;
 STR ret_val28;
 STR s5;
 INT str_self5 = INT_zero;
 STR ret_val29;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val30;
 FSTR str_self6;
 STR ret_val31;
 STR plus_self19;
 STR plus_sarg14;
 STR ret_val32;
 STR plus_self20;
 STR plus_sarg15;
 STR ret_val33;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val34;
 STR plus_self22;
 STR plus_sarg17;
 STR ret_val35;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val36;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val37;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val38;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val39;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val40;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val41;
 STR plus_self29;
 STR plus_sarg24;
 STR ret_val42;
 STR plus_self30;
 STR plus_sarg25;
 STR ret_val43;
 STR plus_self31;
 STR plus_sarg26;
 STR ret_val44;
 STR plus_self32;
 INT plus_arg5 = INT_zero;
 STR ret_val45;
 STR s6;
 INT str_self7 = INT_zero;
 STR ret_val46;
 FSTR buf3 = ((FSTR) NULL);
 FSTR clear_self2;
 INT str_in_self2 = INT_zero;
 FSTR str_in_s2;
 FSTR ret_val47;
 FSTR str_self8;
 STR ret_val48;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val49;
 STR plus_self34;
 INT plus_arg6 = INT_zero;
 STR ret_val50;
 STR s7;
 INT str_self9 = INT_zero;
 STR ret_val51;
 FSTR buf4 = ((FSTR) NULL);
 FSTR clear_self3;
 INT str_in_self3 = INT_zero;
 FSTR str_in_s3;
 FSTR ret_val52;
 FSTR str_self10;
 STR ret_val53;
 STR plus_self35;
 STR plus_sarg28;
 STR ret_val54;
 STR plus_self36;
 STR plus_sarg29;
 STR ret_val55;
 STR plus_self37;
 INT plus_arg7 = INT_zero;
 STR ret_val56;
 STR s8;
 INT str_self11 = INT_zero;
 STR ret_val57;
 FSTR buf5 = ((FSTR) NULL);
 FSTR clear_self4;
 INT str_in_self4 = INT_zero;
 FSTR str_in_s4;
 FSTR ret_val58;
 FSTR str_self12;
 STR ret_val59;
 STR plus_self38;
 STR plus_sarg30;
 STR ret_val60;
 STR plus_self39;
 INT plus_arg8 = INT_zero;
 STR ret_val61;
 STR s9;
 INT str_self13 = INT_zero;
 STR ret_val62;
 FSTR buf6 = ((FSTR) NULL);
 FSTR clear_self5;
 INT str_in_self5 = INT_zero;
 FSTR str_in_s5;
 FSTR ret_val63;
 FSTR str_self14;
 STR ret_val64;
 STR plus_self40;
 STR plus_sarg31;
 STR ret_val65;
 STR plus_self41;
 INT plus_arg9 = INT_zero;
 STR ret_val66;
 STR s10;
 INT str_self15 = INT_zero;
 STR ret_val67;
 FSTR buf7 = ((FSTR) NULL);
 FSTR clear_self6;
 INT str_in_self6 = INT_zero;
 FSTR str_in_s6;
 FSTR ret_val68;
 FSTR str_self16;
 STR ret_val69;
 STR plus_self42;
 STR plus_sarg32;
 STR ret_val70;
 STR plus_self43;
 INT plus_arg10 = INT_zero;
 STR ret_val71;
 STR s11;
 INT str_self17 = INT_zero;
 STR ret_val72;
 FSTR buf8 = ((FSTR) NULL);
 FSTR clear_self7;
 INT str_in_self7 = INT_zero;
 FSTR str_in_s7;
 FSTR ret_val73;
 FSTR str_self18;
 STR ret_val74;
 STR plus_self44;
 STR plus_sarg33;
 STR ret_val75;
 STR plus_self45;
 STR plus_sarg34;
 STR ret_val76;
 FLISTSTR is_empty_self;
 BOOL ret_val77 = BOOL_zero;
 STR plus_self46;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val78;
 STR s12;
 CHAR str_self19 = CHAR_zero;
 STR ret_val79;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val80;
 STR r4;
 STR plus_self47;
 STR plus_sarg35;
 STR ret_val81;
 FMAPSTRFSETSTR is_empty_self1;
 BOOL ret_val82 = BOOL_zero;
 STR plus_self48;
 STR plus_sarg36;
 STR ret_val83;
 STR plus_self49;
 STR plus_sarg37;
 STR ret_val84;
 STR plus_self50;
 STR plus_sarg38;
 STR ret_val85;
 STR plus_self51;
 STR plus_sarg39;
 STR ret_val86;
 STR plus_self52;
 STR plus_sarg40;
 STR ret_val87;
 STR plus_self53;
 STR plus_sarg41;
 STR ret_val88;
 STR plus_self54;
 STR plus_sarg42;
 STR ret_val89;
 FSETSTR is_empty_self2;
 BOOL ret_val90 = BOOL_zero;
 STR plus_self55;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val91;
 STR s13;
 CHAR str_self20 = CHAR_zero;
 STR ret_val92;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val93;
 STR r5;
 STR plus_self56;
 STR plus_sarg43;
 STR ret_val94;
 extern STR name1;
 STR L9;
 INT L10;
 OB L12;
 INT L13;
 STR L15;
 extern STR Cfiles;
 extern STR name1;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 STR L22;
 extern STR objectfiles;
 extern STR name1;
 STR L23;
 INT L24;
 OB L25;
 INT L26;
 STR L28;
 extern STR archivefiles;
 extern STR Executable;
 extern STR GenerateC;
 extern STR PrettyC;
 extern STR Optimize;
 extern STR BenchmarkRunning;
 extern STR PrintO66464500;
 extern STR PrintO279946773;
 extern STR HoistC264675446;
 extern STR Movewh1408405784;
 extern STR Inline1063542453;
 BOOL L29;
 BOOL L301_;
 extern STR stmts;
 extern STR Inline930181741;
 BOOL L32;
 BOOL L331_;
 extern STR stmts;
 extern STR HoistI1762649816;
 extern STR Genera1591585901;
 extern STR Genera181435489;
 extern STR Genera180101529;
 extern STR Genera327357581;
 extern STR Genera2127746521;
 extern STR Genera1023711096;
 extern STR Stopafterparsing;
 extern STR Dontgeneratecode;
 extern STR DontcompileC;
 extern STR Checkg569510162;
 extern STR Dontch1649120098;
 extern STR On;
 extern STR Off;
 extern STR Sideeffects;
 extern STR On;
 extern STR Off;
 extern STR Sidedebug;
 extern STR On;
 extern STR Off;
 extern STR locala1938382166;
 extern STR On;
 extern STR Off;
 extern STR localc573507544;
 BOOL L34;
 BOOL L351_;
 extern STR name26;
 extern STR On;
 extern STR Off;
 extern STR dynami1412565585;
 BOOL L36;
 BOOL L371_;
 extern STR name26;
 extern STR On;
 extern STR Off;
 extern STR remote1183942979;
 dSTR L38;
 OB L39;
 extern STR access1;
 BOOL L40;
 BOOL L421_;
 extern STR name26;
 extern STR On;
 extern STR Off;
 extern STR prefet1416865910;
 BOOL L43;
 BOOL L441_;
 extern STR name26;
 extern STR On;
 extern STR Off;
 extern STR specul343356098;
 extern STR On;
 extern STR Off;
 extern STR looppr1607790851;
 extern STR On;
 extern STR Off;
 extern STR cachep748851792;
 BOOL L45;
 BOOL L461_;
 extern STR a3;
 BOOL L47;
 BOOL L481_;
 extern STR name26;
 extern STR On;
 extern STR Off;
 extern STR pollin1758784609;
 extern STR On;
 extern STR Off;
 extern STR Optimi454106873;
 extern STR On;
 extern STR Off;
 extern STR Postwr1435517258;
 extern STR On;
 extern STR Off;
 extern STR Common1293016579;
 extern STR On;
 extern STR Off;
 extern STR CSEdebug;
 extern STR Experi1633589212;
 extern STR name1;
 INT L49;
 BOOL L501_;
 BOOL L52;
 BOOL L531_;
 STR L54;
 INT L55;
 OB L56;
 INT L57;
 STR L59;
 extern STR Cflags;
 BOOL L60;
 INT L62;
 BOOL L631_;
 BOOL L64;
 BOOL L651_;
 extern STR Externals;
 TUPSTRFSETSTR L66;
 extern STR name27;
 extern STR name28;
 extern STR name29;
 STR L67;
 STR L68;
 extern STR name4;
 extern STR name1;
 extern STR All;
 BOOL L69;
 INT L70;
 BOOL L721_;
 BOOL L73;
 BOOL L741_;
 STR L75;
 INT L76;
 OB L77;
 INT L78;
 STR L80;
 extern STR Forcedroutines;
 s = CS_OPT1597678447(self);
 r = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
  noname1->state = 0;
  while (1) {
   plus_self = r;
   plus_arg = ' ';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L10 = 1;
   L13=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L10)*sizeof(CHAR);
   L12=ZALLOC_LEAF_BIG(L13);
   if (L12==NULL) FATAL("Unable to allocate more memory");
   memset(L12,0,L13);
   ((OB)L12)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L12)->header.destroyed=0;
#endif

   L9 = ((STR) L12);
   L9->asize = L10;
   r1 = L9;
   SARR((STR)r1,0,create_c); 
   ;
   ret_val3 = r1;
   ret_val2 = ret_val3;
   s1 = ret_val2;
   ret_val1 = STR_ap1077157958(plus_self, s1, TRUE);
   plus_self1 = ret_val1;
   if (noname1->state == 0) {
    L11 = ATTR(ATTR(self,cgen),c_files);
    noname1->self = L11;
   }
   L15 = FSETSTR_eltbrSTR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   plus_sarg = L15;
   ret_val4 = STR_ap2004550586(plus_self1, plus_sarg);
   r = ret_val4;
  }
 }
 after_loop: ;
 s = CS_OPT1922122788(self, ((STR) &Cfiles), s, r);
 r = ((STR) &name1);
 {
  struct FSETSTR_eltbrSTR_frame_struct other2_0;
FSETSTR_eltbrSTR_frame noname2 = &other2_0;
  noname2->state = 0;
  while (1) {
   plus_self2 = r;
   plus_arg1 = ' ';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L17 = 1;
   L19=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L17)*sizeof(CHAR);
   L18=ZALLOC_LEAF_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   memset(L18,0,L19);
   ((OB)L18)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L16 = ((STR) L18);
   L16->asize = L17;
   r2 = L16;
   SARR((STR)r2,0,create_c1); 
   ;
   ret_val7 = r2;
   ret_val6 = ret_val7;
   s2 = ret_val6;
   ret_val5 = STR_ap1077157958(plus_self2, s2, TRUE);
   plus_self3 = ret_val5;
   if (noname2->state == 0) {
    L21 = ATTR(ATTR(self,cgen),object_files);
    noname2->self = L21;
   }
   L22 = FSETSTR_eltbrSTR(noname2);
   if (noname2->state == -1) {
    goto after_loop1;
   }
   plus_sarg1 = L22;
   ret_val8 = STR_ap2004550586(plus_self3, plus_sarg1);
   r = ret_val8;
  }
 }
 after_loop1: ;
 s = CS_OPT1922122788(self, ((STR) &objectfiles), s, r);
 r = ((STR) &name1);
 {
  struct FLISTS1171631134_frame_struct other3_0;
FLISTS1171631134_frame noname3 = &other3_0;
  noname3->state = 0;
  while (1) {
   plus_self4 = r;
   plus_arg2 = ' ';
   str_self2 = plus_arg2;
   create_self2 = ((STR) NULL);
   create_c2 = str_self2;
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
   r3 = L23;
   SARR((STR)r3,0,create_c2); 
   ;
   ret_val11 = r3;
   ret_val10 = ret_val11;
   s3 = ret_val10;
   ret_val9 = STR_ap1077157958(plus_self4, s3, TRUE);
   plus_self5 = ret_val9;
   if (noname3->state == 0) {
    L31 = ATTR(ATTR(self,cgen),archive_files);
    noname3->self = L31;
   }
   L28 = FLISTS1171631134(noname3);
   if (noname3->state == -1) {
    goto after_loop2;
   }
   plus_sarg2 = L28;
   ret_val12 = STR_ap2004550586(plus_self5, plus_sarg2);
   r = ret_val12;
  }
 }
 after_loop2: ;
 s = CS_OPT1922122788(self, ((STR) &archivefiles), s, r);
 s = CS_OPT1922122788(self, ((STR) &Executable), s, ATTR(ATTR(self,cgen),executable1));
 if (ATTR(ATTR(self,cgen),gen_c)) {
  plus_self6 = s;
  plus_sarg3 = ((STR) &GenerateC);
  ret_val13 = STR_ap2004550586(plus_self6, plus_sarg3);
  s = ret_val13;
 }
 if (ATTR(ATTR(self,cgen),pretty)) {
  plus_self7 = s;
  plus_sarg4 = ((STR) &PrettyC);
  ret_val14 = STR_ap2004550586(plus_self7, plus_sarg4);
  s = ret_val14;
 }
 if (ATTR(ATTR(self,cgen),optimize)) {
  plus_self8 = s;
  plus_sarg5 = ((STR) &Optimize);
  ret_val15 = STR_ap2004550586(plus_self8, plus_sarg5);
  s = ret_val15;
 }
 if (ATTR(ATTR(self,prog),benchmark)) {
  plus_self9 = s;
  plus_sarg6 = ((STR) &BenchmarkRunning);
  ret_val16 = STR_ap2004550586(plus_self9, plus_sarg6);
  s = ret_val16;
 }
 if (ATTR(ATTR(self,prog),opt_debug)) {
  plus_self10 = s;
  plus_sarg7 = ((STR) &PrintO66464500);
  ret_val17 = STR_ap2004550586(plus_self10, plus_sarg7);
  s = ret_val17;
 }
 if (ATTR(ATTR(self,prog),opt_verbose)) {
  plus_self11 = s;
  plus_sarg8 = ((STR) &PrintO279946773);
  ret_val18 = STR_ap2004550586(plus_self11, plus_sarg8);
  s = ret_val18;
 }
 if (ATTR(ATTR(self,prog),hoist_const)) {
  plus_self12 = s;
  plus_sarg9 = ((STR) &HoistC264675446);
  ret_val19 = STR_ap2004550586(plus_self12, plus_sarg9);
  s = ret_val19;
 }
 if (ATTR(ATTR(self,prog),move_while)) {
  plus_self13 = s;
  plus_sarg10 = ((STR) &Movewh1408405784);
  ret_val20 = STR_ap2004550586(plus_self13, plus_sarg10);
  s = ret_val20;
 }
 if (ATTR(ATTR(self,inliner),inline_routs)) {
  plus_self14 = s;
  plus_sarg11 = ((STR) &Inline1063542453);
  ret_val21 = STR_ap2004550586(plus_self14, plus_sarg11);
  plus_self15 = ret_val21;
  plus_arg3 = ATTR(ATTR(self,inliner),rout_thres);
  str_self3 = plus_arg3;
  clear_self = buf1;
  L29 = (clear_self==((FSTR) NULL));
  L301_=!(L29); 
  if (L301_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self3;
  str_in_s = buf1;
  ret_val24 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val24;
  str_self4 = buf1;
  ret_val25 = STR_fr1097270334(((STR) NULL), str_self4);
  ret_val23 = ret_val25;
  s4 = ret_val23;
  ret_val22 = STR_ap1077157958(plus_self15, s4, TRUE);
  plus_self16 = ret_val22;
  plus_sarg12 = ((STR) &stmts);
  ret_val26 = STR_ap2004550586(plus_self16, plus_sarg12);
  s = ret_val26;
 }
 if (ATTR(ATTR(self,inliner),inline_iters)) {
  plus_self17 = s;
  plus_sarg13 = ((STR) &Inline930181741);
  ret_val27 = STR_ap2004550586(plus_self17, plus_sarg13);
  plus_self18 = ret_val27;
  plus_arg4 = ATTR(ATTR(self,inliner),iter_thres);
  str_self5 = plus_arg4;
  clear_self1 = buf2;
  L32 = (clear_self1==((FSTR) NULL));
  L331_=!(L32); 
  if (L331_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self5;
  str_in_s1 = buf2;
  ret_val30 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val30;
  str_self6 = buf2;
  ret_val31 = STR_fr1097270334(((STR) NULL), str_self6);
  ret_val29 = ret_val31;
  s5 = ret_val29;
  ret_val28 = STR_ap1077157958(plus_self18, s5, TRUE);
  plus_self19 = ret_val28;
  plus_sarg14 = ((STR) &stmts);
  ret_val32 = STR_ap2004550586(plus_self19, plus_sarg14);
  s = ret_val32;
 }
 if (ATTR(ATTR(self,prog),hoist_iter_init)) {
  plus_self20 = s;
  plus_sarg15 = ((STR) &HoistI1762649816);
  ret_val33 = STR_ap2004550586(plus_self20, plus_sarg15);
  s = ret_val33;
 }
 if (ATTR(ATTR(self,cgen),debug1)) {
  plus_self21 = s;
  plus_sarg16 = ((STR) &Genera1591585901);
  ret_val34 = STR_ap2004550586(plus_self21, plus_sarg16);
  s = ret_val34;
 }
 if (ATTR(ATTR(self,cgen),deterministic1)) {
  plus_self22 = s;
  plus_sarg17 = ((STR) &Genera181435489);
  ret_val35 = STR_ap2004550586(plus_self22, plus_sarg17);
  s = ret_val35;
 }
 if (ATTR(ATTR(self,cgen),type_tables)) {
  plus_self23 = s;
  plus_sarg18 = ((STR) &Genera180101529);
  ret_val36 = STR_ap2004550586(plus_self23, plus_sarg18);
  s = ret_val36;
 }
 if (ATTR(ATTR(self,cgen),func_tables)) {
  plus_self24 = s;
  plus_sarg19 = ((STR) &Genera327357581);
  ret_val37 = STR_ap2004550586(plus_self24, plus_sarg19);
  s = ret_val37;
 }
 if (ATTR(ATTR(self,cgen),print_ob)) {
  plus_self25 = s;
  plus_sarg20 = ((STR) &Genera2127746521);
  ret_val38 = STR_ap2004550586(plus_self25, plus_sarg20);
  s = ret_val38;
 }
 if (ATTR(ATTR(self,cgen),print_ob_g)) {
  plus_self26 = s;
  plus_sarg21 = ((STR) &Genera1023711096);
  ret_val39 = STR_ap2004550586(plus_self26, plus_sarg21);
  s = ret_val39;
 }
 if (ATTR(ATTR(self,cs),only_parse)) {
  plus_self27 = s;
  plus_sarg22 = ((STR) &Stopafterparsing);
  ret_val40 = STR_ap2004550586(plus_self27, plus_sarg22);
  s = ret_val40;
 }
 if (ATTR(ATTR(self,cgen),only_check)) {
  plus_self28 = s;
  plus_sarg23 = ((STR) &Dontgeneratecode);
  ret_val41 = STR_ap2004550586(plus_self28, plus_sarg23);
  s = ret_val41;
 }
 if (ATTR(ATTR(self,cgen),only_C)) {
  plus_self29 = s;
  plus_sarg24 = ((STR) &DontcompileC);
  ret_val42 = STR_ap2004550586(plus_self29, plus_sarg24);
  s = ret_val42;
 }
 if (ATTR(ATTR(self,prog),check_generics)) {
  plus_self30 = s;
  plus_sarg25 = ((STR) &Checkg569510162);
  ret_val43 = STR_ap2004550586(plus_self30, plus_sarg25);
  s = ret_val43;
 }
 if (ATTR(ATTR(self,cs),only_reachable)) {
  plus_self31 = s;
  plus_sarg26 = ((STR) &Dontch1649120098);
  ret_val44 = STR_ap2004550586(plus_self31, plus_sarg26);
  s = ret_val44;
 }
 if (ATTR(self,side_effects)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Sideeffects), s, r);
 if (ATTR(self,side_debug)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Sidedebug), s, r);
 if (ATTR(self,local_opt)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &locala1938382166), s, r);
 if (ATTR(self,local_call)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 plus_self32 = ((STR) &localc573507544);
 plus_arg5 = ATTR(self,local_call_acces);
 str_self7 = plus_arg5;
 clear_self2 = buf3;
 L34 = (clear_self2==((FSTR) NULL));
 L351_=!(L34); 
 if (L351_) {
  SATTR(clear_self2,loc,0);
 }
 str_in_self2 = str_self7;
 str_in_s2 = buf3;
 ret_val47 = INT_st367594495(str_in_self2, str_in_s2, 0, 10, ' ');
 buf3 = ret_val47;
 str_self8 = buf3;
 ret_val48 = STR_fr1097270334(((STR) NULL), str_self8);
 ret_val46 = ret_val48;
 s6 = ret_val46;
 ret_val45 = STR_ap1077157958(plus_self32, s6, TRUE);
 plus_self33 = ret_val45;
 plus_sarg27 = ((STR) &name26);
 ret_val49 = STR_ap2004550586(plus_self33, plus_sarg27);
 s = CS_OPT1922122788(self, ret_val49, s, r);
 if (ATTR(self,local_call_dynam1)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 plus_self34 = ((STR) &dynami1412565585);
 plus_arg6 = ATTR(self,local_call_dynam);
 str_self9 = plus_arg6;
 clear_self3 = buf4;
 L36 = (clear_self3==((FSTR) NULL));
 L371_=!(L36); 
 if (L371_) {
  SATTR(clear_self3,loc,0);
 }
 str_in_self3 = str_self9;
 str_in_s3 = buf4;
 ret_val52 = INT_st367594495(str_in_self3, str_in_s3, 0, 10, ' ');
 buf4 = ret_val52;
 str_self10 = buf4;
 ret_val53 = STR_fr1097270334(((STR) NULL), str_self10);
 ret_val51 = ret_val53;
 s7 = ret_val51;
 ret_val50 = STR_ap1077157958(plus_self34, s7, TRUE);
 plus_self35 = ret_val50;
 plus_sarg28 = ((STR) &name26);
 ret_val54 = STR_ap2004550586(plus_self35, plus_sarg28);
 s = CS_OPT1922122788(self, ret_val54, s, r);
 if (ATTR(self,remote_call)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 L39=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L39==NULL) FATAL("Unable to allocate more memory");
 memset(L39,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L39)->header.tag=BOOL_tag;
 L38 = (dSTR)((BOOL_boxed) L39);
 ((BOOL_boxed) L38)->immutable_part = ATTR(self,remote_call_crea);
 plus_self36 = STR_pl1270664985(((STR) &remote1183942979), L38);
 plus_sarg29 = ((STR) &access1);
 ret_val55 = STR_ap2004550586(plus_self36, plus_sarg29);
 plus_self37 = ret_val55;
 plus_arg7 = ATTR(self,remote_call_acce);
 str_self11 = plus_arg7;
 clear_self4 = buf5;
 L40 = (clear_self4==((FSTR) NULL));
 L421_=!(L40); 
 if (L421_) {
  SATTR(clear_self4,loc,0);
 }
 str_in_self4 = str_self11;
 str_in_s4 = buf5;
 ret_val58 = INT_st367594495(str_in_self4, str_in_s4, 0, 10, ' ');
 buf5 = ret_val58;
 str_self12 = buf5;
 ret_val59 = STR_fr1097270334(((STR) NULL), str_self12);
 ret_val57 = ret_val59;
 s8 = ret_val57;
 ret_val56 = STR_ap1077157958(plus_self37, s8, TRUE);
 plus_self38 = ret_val56;
 plus_sarg30 = ((STR) &name26);
 ret_val60 = STR_ap2004550586(plus_self38, plus_sarg30);
 s = CS_OPT1922122788(self, ret_val60, s, r);
 if (ATTR(self,prefetch)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 plus_self39 = ((STR) &prefet1416865910);
 plus_arg8 = ATTR(self,prefetch_weight);
 str_self13 = plus_arg8;
 clear_self5 = buf6;
 L43 = (clear_self5==((FSTR) NULL));
 L441_=!(L43); 
 if (L441_) {
  SATTR(clear_self5,loc,0);
 }
 str_in_self5 = str_self13;
 str_in_s5 = buf6;
 ret_val63 = INT_st367594495(str_in_self5, str_in_s5, 0, 10, ' ');
 buf6 = ret_val63;
 str_self14 = buf6;
 ret_val64 = STR_fr1097270334(((STR) NULL), str_self14);
 ret_val62 = ret_val64;
 s9 = ret_val62;
 ret_val61 = STR_ap1077157958(plus_self39, s9, TRUE);
 plus_self40 = ret_val61;
 plus_sarg31 = ((STR) &name26);
 ret_val65 = STR_ap2004550586(plus_self40, plus_sarg31);
 s = CS_OPT1922122788(self, ret_val65, s, r);
 if (ATTR(self,specul_prefetch)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &specul343356098), s, r);
 if (ATTR(self,loop_prefetch)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &looppr1607790851), s, r);
 if (ATTR(self,cache1)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 plus_self41 = ((STR) &cachep748851792);
 plus_arg9 = ATTR(self,cache_size);
 str_self15 = plus_arg9;
 clear_self6 = buf7;
 L45 = (clear_self6==((FSTR) NULL));
 L461_=!(L45); 
 if (L461_) {
  SATTR(clear_self6,loc,0);
 }
 str_in_self6 = str_self15;
 str_in_s6 = buf7;
 ret_val68 = INT_st367594495(str_in_self6, str_in_s6, 0, 10, ' ');
 buf7 = ret_val68;
 str_self16 = buf7;
 ret_val69 = STR_fr1097270334(((STR) NULL), str_self16);
 ret_val67 = ret_val69;
 s10 = ret_val67;
 ret_val66 = STR_ap1077157958(plus_self41, s10, TRUE);
 plus_self42 = ret_val66;
 plus_sarg32 = ((STR) &a3);
 ret_val70 = STR_ap2004550586(plus_self42, plus_sarg32);
 plus_self43 = ret_val70;
 plus_arg10 = ATTR(self,cache_slot_size);
 str_self17 = plus_arg10;
 clear_self7 = buf8;
 L47 = (clear_self7==((FSTR) NULL));
 L481_=!(L47); 
 if (L481_) {
  SATTR(clear_self7,loc,0);
 }
 str_in_self7 = str_self17;
 str_in_s7 = buf8;
 ret_val73 = INT_st367594495(str_in_self7, str_in_s7, 0, 10, ' ');
 buf8 = ret_val73;
 str_self18 = buf8;
 ret_val74 = STR_fr1097270334(((STR) NULL), str_self18);
 ret_val72 = ret_val74;
 s11 = ret_val72;
 ret_val71 = STR_ap1077157958(plus_self43, s11, TRUE);
 plus_self44 = ret_val71;
 plus_sarg33 = ((STR) &name26);
 ret_val75 = STR_ap2004550586(plus_self44, plus_sarg33);
 s = CS_OPT1922122788(self, ret_val75, s, r);
 if (ATTR(self,loop_poll)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &pollin1758784609), s, r);
 if (ATTR(self,parloops)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Optimi454106873), s, r);
 if (ATTR(self,post_write)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Postwr1435517258), s, r);
 if (ATTR(self,cse)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &Common1293016579), s, r);
 if (ATTR(self,cse_debug)) {
  r = ((STR) &On);
 }
 else {
  r = ((STR) &Off);
 }
 s = CS_OPT1922122788(self, ((STR) &CSEdebug), s, r);
 if (ATTR(ATTR(self,prog),zones)) {
  plus_self45 = s;
  plus_sarg34 = ((STR) &Experi1633589212);
  ret_val76 = STR_ap2004550586(plus_self45, plus_sarg34);
  s = ret_val76;
 }
 r = ((STR) &name1);
 is_empty_self = ATTR(ATTR(self,cgen),c_flags);
 L49 = FLISTS326330076(is_empty_self);
 L501_=(L49)==(0); 
 ret_val77 = L501_;
 L52 = ret_val77;
 L531_=!(L52); 
 if (L531_) {
  {
   struct FLISTS1171631134_frame_struct other4_0;
FLISTS1171631134_frame noname4 = &other4_0;
   noname4->state = 0;
   while (1) {
    plus_self46 = r;
    plus_arg11 = ' ';
    str_self19 = plus_arg11;
    create_self3 = ((STR) NULL);
    create_c3 = str_self19;
    L55 = 1;
    L57=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L55)*sizeof(CHAR);
    L56=ZALLOC_LEAF_BIG(L57);
    if (L56==NULL) FATAL("Unable to allocate more memory");
    memset(L56,0,L57);
    ((OB)L56)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L56)->header.destroyed=0;
#endif

    L54 = ((STR) L56);
    L54->asize = L55;
    r4 = L54;
    SARR((STR)r4,0,create_c3); 
    ;
    ret_val80 = r4;
    ret_val79 = ret_val80;
    s12 = ret_val79;
    ret_val78 = STR_ap1077157958(plus_self46, s12, TRUE);
    plus_self47 = ret_val78;
    if (noname4->state == 0) {
     L41 = ATTR(ATTR(self,cgen),c_flags);
     noname4->self = L41;
    }
    L59 = FLISTS1171631134(noname4);
    if (noname4->state == -1) {
     goto after_loop3;
    }
    plus_sarg35 = L59;
    ret_val81 = STR_ap2004550586(plus_self47, plus_sarg35);
    r = ret_val81;
   }
  }
  after_loop3: ;
 }
 s = CS_OPT1922122788(self, ((STR) &Cflags), s, r);
 is_empty_self1 = ATTR(ATTR(self,cgen),externals);
 if ((is_empty_self1==((FMAPSTRFSETSTR) NULL))) {
  L60 = TRUE;
 } else {
  L62 = ATTR(is_empty_self1,hsize);
  L631_=(L62)==(0); 
  L60 = L631_;
 }
 ret_val82 = L60;
 L64 = ret_val82;
 L651_=!(L64); 
 if (L651_) {
  plus_self48 = s;
  plus_sarg36 = ((STR) &Externals);
  ret_val83 = STR_ap2004550586(plus_self48, plus_sarg36);
  s = ret_val83;
  {
   struct FMAPST1155259492_frame_struct other5_0;
FMAPST1155259492_frame noname5 = &other5_0;
   L51 = ATTR(ATTR(self,cgen),externals);
   noname5->self = L51;
   noname5->state = 0;
   while (1) {
    L66 = FMAPST1155259492(noname5);
    if (noname5->state == -1) {
     goto after_loop4;
    }
    p = L66;
    plus_self49 = s;
    plus_sarg37 = ((STR) &name27);
    ret_val84 = STR_ap2004550586(plus_self49, plus_sarg37);
    plus_self50 = ret_val84;
    plus_sarg38 = p.t1;
    ret_val85 = STR_ap2004550586(plus_self50, plus_sarg38);
    plus_self51 = ret_val85;
    plus_sarg39 = ((STR) &name28);
    ret_val86 = STR_ap2004550586(plus_self51, plus_sarg39);
    s = ret_val86;
    {
     struct FSETSTR_eltbrSTR_frame_struct other6_0;
FSETSTR_eltbrSTR_frame noname6 = &other6_0;
     struct STR_se405450305_frame_struct other6_1;
STR_se405450305_frame noname7 = &other6_1;
     L61 = p.t2;
     L71 = ((STR) &name29);
     noname6->self = L61;
     noname6->state = 0;
     noname7->self = L71;
     noname7->state = 0;
     while (1) {
      plus_self52 = s;
      L68 = FSETSTR_eltbrSTR(noname6);
      if (noname6->state == -1) {
       goto after_loop5;
      }
      noname7->arg1 = ((dSTR) L68);
      L67 = STR_se405450305(noname7);
      if (noname7->state == -1) {
       goto after_loop5;
      }
      plus_sarg40 = L67;
      ret_val87 = STR_ap2004550586(plus_self52, plus_sarg40);
      s = ret_val87;
     }
    }
    after_loop5: ;
    plus_self53 = s;
    plus_sarg41 = ((STR) &name4);
    ret_val88 = STR_ap2004550586(plus_self53, plus_sarg41);
    s = ret_val88;
   }
  }
  after_loop4: ;
 }
 r = ((STR) &name1);
 if (ATTR(ATTR(self,cgen),force_all)) {
  plus_self54 = r;
  plus_sarg42 = ((STR) &All);
  ret_val89 = STR_ap2004550586(plus_self54, plus_sarg42);
  r = ret_val89;
 }
 else {
  is_empty_self2 = ATTR(ATTR(self,cgen),force_routines);
  if ((is_empty_self2==((FSETSTR) NULL))) {
   L69 = TRUE;
  } else {
   L70 = ATTR(is_empty_self2,hsize);
   L721_=(L70)==(0); 
   L69 = L721_;
  }
  ret_val90 = L69;
  L73 = ret_val90;
  L741_=!(L73); 
  if (L741_) {
   {
    struct FSETSTR_eltbrSTR_frame_struct other7_0;
FSETSTR_eltbrSTR_frame noname8 = &other7_0;
    noname8->state = 0;
    while (1) {
     plus_self55 = r;
     plus_arg12 = ' ';
     str_self20 = plus_arg12;
     create_self4 = ((STR) NULL);
     create_c4 = str_self20;
     L76 = 1;
     L78=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L76)*sizeof(CHAR);
     L77=ZALLOC_LEAF_BIG(L78);
     if (L77==NULL) FATAL("Unable to allocate more memory");
     memset(L77,0,L78);
     ((OB)L77)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L77)->header.destroyed=0;
#endif

     L75 = ((STR) L77);
     L75->asize = L76;
     r5 = L75;
     SARR((STR)r5,0,create_c4); 
     ;
     ret_val93 = r5;
     ret_val92 = ret_val93;
     s13 = ret_val92;
     ret_val91 = STR_ap1077157958(plus_self55, s13, TRUE);
     plus_self56 = ret_val91;
     if (noname8->state == 0) {
      L81 = ATTR(ATTR(self,cgen),force_routines);
      noname8->self = L81;
     }
     L80 = FSETSTR_eltbrSTR(noname8);
     if (noname8->state == -1) {
      goto after_loop6;
     }
     plus_sarg43 = L80;
     ret_val94 = STR_ap2004550586(plus_self56, plus_sarg43);
     r = ret_val94;
    }
   }
   after_loop6: ;
  }
 }
 s = CS_OPT1922122788(self, ((STR) &Forcedroutines), s, r);
 ret_val = s;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT63514076(CS_OPTIONS self, ARRAYSTR a) {
 ARRAYSTR L11;
 STR s;
 STR L21;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val;
 INT L31 = INT_zero;
 CS_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self;
 STR plus_s;
 OUT ret_val3;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val5;
 FILE1 stdout_self1;
 FILE1 ret_val6;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val7;
 FILE1 r2;
 CS_OPTIONS barf_self;
 STR barf_msg;
 ERR1 create_self2;
 ERR1 ret_val8;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val9;
 FILE1 stderr_self;
 FILE1 ret_val10;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self1;
 FILE1 ret_val11;
 FILE1 r4;
 UNIX exit_self;
 INT exit_code = INT_zero;
 CS_OPTIONS barf_self1;
 STR barf_msg1;
 ERR1 create_self3;
 ERR1 ret_val12;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val13;
 FILE1 stderr_self2;
 FILE1 ret_val14;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 FILE1 stderr_self3;
 FILE1 ret_val15;
 FILE1 r6;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 INT L41_;
 ARRAYSTR L5;
 INT L6;
 OB L7;
 INT L8;
 INT L91_br;
 extern STR commandline;
 INT L101_;
 INT L12;
 ARRAYSTR L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 extern STR Processingoption;
 FILE1 L17;
 OB L18;
 FILE1 L20;
 OB L22;
 extern STR name9;
 FILE1 L24;
 OB L25;
 extern STR main2;
 extern STR home;
 extern STR homeis1034977208;
 FILE1 L27;
 OB L28;
 dSTR L30;
 OB L32;
 FILE1 L33;
 OB L34;
 extern STR verbose;
 extern STR prolix;
 extern STR prolix706342420;
 extern STR O_debug;
 extern STR O_debug_func;
 FLISTSTR L36;
 extern STR O_verbose;
 extern STR end2;
 extern STR Nohome1873319144;
 FILE1 L37;
 OB L38;
 dSTR L40;
 OB L42;
 FILE1 L43;
 OB L44;
 SATTR(self,args,a);
 create_self = ((ARRAYSTR) NULL);
 L41_=(a)==NULL?0:ASIZE((ARRAYSTR)a); 
 create_n = L41_;
 L6 = create_n;
 L8=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L6)*sizeof(STR);
 L7=ZALLOC_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((ARRAYSTR) L7);
 L5->asize = L6;
 ret_val = L5;
 SATTR(self,arg_locations,ret_val);
 {
  BOOL f_L91_ = TRUE;
  /* loop index variable */
  L31 = 0;
  L11 = ATTR(self,arg_locations);
  L91_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L31>=L91_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L31,(STR)((STR) &commandline)); 
   ;
   L101_=INTPLUS(L31,1); 
   L31 = L101_;
  }
 }
 after_loop: ;
 SATTR(self,next,1);
 CS_OPT611018296(self);
 while (1) {
  more_args_self = self;
  L12 = ATTR(more_args_self,next);
  L13 = ATTR(more_args_self,args);
  L141_=(L13)==NULL?0:ASIZE((ARRAYSTR)L13); 
  L15 = L141_;
  L161_=(L12)<(L15); 
  ret_val1 = L161_;
  if (ret_val1) {
  }
  else {
   goto after_loop1;
  }
  s = CS_OPT1970940548(self);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self = ret_val2;
   plus_s = ((STR) &Processingoption);
   stdout_self = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r = L17;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = s;
   stdout_self1 = ((FILE1) NULL);
   L22=ZALLOC(sizeof(struct FILE1_struct));
   if (L22==NULL) FATAL("Unable to allocate more memory");
   ((OB)L22)->header.tag=FILE1_tag;
   L20 = ((FILE1) L22);
   r1 = L20;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L25=ZALLOC(sizeof(struct FILE1_struct));
   if (L25==NULL) FATAL("Unable to allocate more memory");
   ((OB)L25)->header.tag=FILE1_tag;
   L24 = ((FILE1) L25);
   r2 = L24;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  L21 = s;
  if (STR_is111530248(L21, ((STR) &main2))) {
   SATTR(ATTR(self,prog),main_class,CS_OPT1970940548(self));
  }
  else {
   if (STR_is111530248(L21, ((STR) &home))) {
    barf_self = self;
    barf_msg = ((STR) &homeis1034977208);
    create_self2 = ((ERR1) NULL);
    ret_val8 = create_self2;
    plus_self3 = ret_val8;
    plus_s3 = ((dSTR) barf_msg);
    stderr_self = ((FILE1) NULL);
    L28=ZALLOC(sizeof(struct FILE1_struct));
    if (L28==NULL) FATAL("Unable to allocate more memory");
    ((OB)L28)->header.tag=FILE1_tag;
    L27 = ((FILE1) L28);
    r3 = L27;
    
    SATTR(r3,fp,stderr);
    ret_val10 = r3;
    FILE_plus_dSTR(ret_val10, plus_s3);
    ret_val9 = plus_self3;
    plus_self4 = ret_val9;
    L32=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L32==NULL) FATAL("Unable to allocate more memory");
    memset(L32,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L32)->header.tag=CHAR_tag;
    L30 = (dSTR)((CHAR_boxed) L32);
    ((CHAR_boxed) L30)->immutable_part = '\n';
    plus_s4 = L30;
    stderr_self1 = ((FILE1) NULL);
    L34=ZALLOC(sizeof(struct FILE1_struct));
    if (L34==NULL) FATAL("Unable to allocate more memory");
    ((OB)L34)->header.tag=FILE1_tag;
    L33 = ((FILE1) L34);
    r4 = L33;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    exit_self = ((UNIX) NULL);
    exit_code = 1;
    exit(exit_code);
   }
   else {
    if (STR_is111530248(L21, ((STR) &verbose))) {
     SATTR(ATTR(self,prog),verbose,TRUE);
    }
    else {
     if (STR_is111530248(L21, ((STR) &prolix))) {
      SATTR(ATTR(self,prog),verbose,TRUE);
      SATTR(ATTR(self,prog),prolix,TRUE);
     }
     else {
      if (STR_is111530248(L21, ((STR) &prolix706342420))) {
       SATTR(ATTR(self,prog),show_types,TRUE);
       SATTR(ATTR(self,prog),prolix,TRUE);
      }
      else {
       if (STR_is111530248(L21, ((STR) &O_debug))) {
        SATTR(ATTR(self,prog),opt_debug,TRUE);
        SATTR(ATTR(self,prog),opt_verbose,TRUE);
       }
       else {
        if (STR_is111530248(L21, ((STR) &O_debug_func))) {
         L36 = ATTR(ATTR(self,prog),opt_debug_func);
         SATTR(ATTR(self,prog),opt_debug_func,FLISTS1425610062(L36, CS_OPT1970940548(self)));
         SATTR(ATTR(self,prog),opt_verbose,TRUE);
        }
        else {
         if (STR_is111530248(L21, ((STR) &O_verbose))) {
          SATTR(ATTR(self,prog),opt_verbose,TRUE);
         }
         else {
          if (STR_is111530248(L21, ((STR) &end2))) {
          }
          else {
           CS_OPT65198073(self, s);
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
 after_loop1: ;
 if ((ATTR(ATTR(self,prog),home)==((STR) NULL))) {
  barf_self1 = self;
  barf_msg1 = ((STR) &Nohome1873319144);
  create_self3 = ((ERR1) NULL);
  ret_val12 = create_self3;
  plus_self5 = ret_val12;
  plus_s5 = ((dSTR) barf_msg1);
  stderr_self2 = ((FILE1) NULL);
  L38=ZALLOC(sizeof(struct FILE1_struct));
  if (L38==NULL) FATAL("Unable to allocate more memory");
  ((OB)L38)->header.tag=FILE1_tag;
  L37 = ((FILE1) L38);
  r5 = L37;
  
  SATTR(r5,fp,stderr);
  ret_val14 = r5;
  FILE_plus_dSTR(ret_val14, plus_s5);
  ret_val13 = plus_self5;
  plus_self6 = ret_val13;
  L42=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L42==NULL) FATAL("Unable to allocate more memory");
  memset(L42,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L42)->header.tag=CHAR_tag;
  L40 = (dSTR)((CHAR_boxed) L42);
  ((CHAR_boxed) L40)->immutable_part = '\n';
  plus_s6 = L40;
  stderr_self3 = ((FILE1) NULL);
  L44=ZALLOC(sizeof(struct FILE1_struct));
  if (L44==NULL) FATAL("Unable to allocate more memory");
  ((OB)L44)->header.tag=FILE1_tag;
  L43 = ((FILE1) L44);
  r6 = L43;
  
  SATTR(r6,fp,stderr);
  ret_val15 = r6;
  FILE_plus_dSTR(ret_val15, plus_s6);
  exit_self1 = ((UNIX) NULL);
  exit_code1 = 1;
  exit(exit_code1);
 }
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT994959939(CS_OPTIONS self) {
 STR sc;
 STR sh;
 CS_OPTIONS barf_self;
 STR barf_msg;
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val1;
 FILE1 stderr_self;
 FILE1 ret_val2;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 FILE1 stderr_self1;
 FILE1 ret_val3;
 FILE1 r1;
 UNIX exit_self;
 INT exit_code = INT_zero;
 extern STR SATHER_COMMANDS;
 BOOL L1;
 BOOL L21_;
 extern STR TheSAT1577550343;
 FILE1 L3;
 OB L4;
 dSTR L6;
 OB L7;
 FILE1 L8;
 OB L9;
 sc = UNIX_g108606936(((UNIX) NULL), ((STR) &SATHER_COMMANDS));
 L1 = (sc==((STR) NULL));
 L21_=!(L1); 
 if (L21_) {
  barf_self = self;
  barf_msg = ((STR) &TheSAT1577550343);
  create_self = ((ERR1) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((dSTR) barf_msg);
  stderr_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stderr);
  ret_val2 = r;
  FILE_plus_dSTR(ret_val2, plus_s);
  ret_val1 = plus_self;
  plus_self1 = ret_val1;
  L7=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  memset(L7,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L7)->header.tag=CHAR_tag;
  L6 = (dSTR)((CHAR_boxed) L7);
  ((CHAR_boxed) L6)->immutable_part = '\n';
  plus_s1 = L6;
  stderr_self1 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r1 = L8;
  
  SATTR(r1,fp,stderr);
  ret_val3 = r1;
  FILE_plus_dSTR(ret_val3, plus_s1);
  exit_self = ((UNIX) NULL);
  exit_code = 1;
  exit(exit_code);
 }
 sh = UNIX_s517080793(((UNIX) NULL));
 SATTR(ATTR(self,prog),home,sh);
}
