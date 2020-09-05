#include "sather.h"

/* Layouts */

typedef struct dBACK_END_struct {
 OB_HEADER header;
 } *dBACK_END;

typedef struct dINLINE_struct {
 OB_HEADER header;
 } *dINLINE;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct ARRAYSTR_struct {/* layout for ARRAY{STR} */
 OB_HEADER header;
 INT asize;
 STR arr_part[1];
 } *ARRAYSTR;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

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

typedef struct INT_do676118316_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_do676118316_frame;

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

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct STR_el1843619312_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 INT arg1;/* Formal argument: beg */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_el1843619312_frame;

typedef struct STR_sp663650766_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR arg1;/* Formal argument: c */
 STR ret_val;
 INT cur_loc;
 INT next_loc;
 } *STR_sp663650766_frame;

typedef struct STR_CURSOR_struct {/* layout for STR_CURSOR */
 OB_HEADER header;
 INT error1;
 INT index1;
 INT line_no;
 STR buf1;
 CHAR comment_char1;
 CHAR comment_char2;
 BOOL is_done;
 } *STR_CURSOR;

typedef struct TUPSTRFSETSTR_struct {/* layout for TUP{STR,FSET{STR}} */
 struct FSETSTR_struct *t2;
 STR t1;
 } TUPSTRFSETSTR;
static TUPSTRFSETSTR TUPSTRFSETSTR_zero;

typedef struct TUPSTRFSETSTR_boxed_struct {
 OB_HEADER header;
 TUPSTRFSETSTR immutable_part;
 } *TUPSTRFSETSTR_boxed;

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

typedef struct FMAPSTRFSETSTR_struct {/* layout for FMAP{STR,FSET{STR}} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRFSETSTR_struct arr_part[1];
 } *FMAPSTRFSETSTR;

typedef struct FMAPSTRSTR_struct {/* layout for FMAP{STR,STR} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRSTR_struct arr_part[1];
 } *FMAPSTRSTR;

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

extern INT CGEN_m1427112264;

/* Function declarations */


extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);
ARRAYSTR ARRAYS417083844(ARRAYSTR, ARRAYSTR);
ARRAYSTR FLISTS533235694(FLISTSTR);
BOOL CHAR_i629101715(CHAR);
BOOL CHAR_i84735023(CHAR);
BOOL CS_OPT1373530461(CS_OPTIONS, STR);
BOOL CS_OPT1443087191(CS_OPTIONS, STR);
BOOL FSETST1025458804(FSETSTR, STR);
BOOL STR_is111530248(STR, STR);
CHAR STR_el1843619312(STR_el1843619312_frame);
CS_OPTIONS CS_OPT1986127469(CS_OPTIONS, PROG, CS);
FLISTSTR CS_OPT975854719(CS_OPTIONS, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FMAPSTRFSETSTR FMAPST935488038(FMAPSTRFSETSTR, STR, FSETSTR);
FMAPSTRSTR FMAPST859022540(FMAPSTRSTR, STR, STR);
FSETSTR CS_OPT595032350(CS_OPTIONS);
FSETSTR FSETST1404644833(FSETSTR, STR);
FSETSTR FSETST1884046860(FSETSTR);
FSETSTR FSETST866022026(FSETSTR, FSETSTR);
FSETSTR FSETST897681893(FSETSTR, FSETSTR);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR FSTR_s1920457899(FSTR, INT, INT);
INT FLISTS326330076(FLISTSTR);
INT FSTR_sizerINT(FSTR);
INT INT_do676118316(INT_do676118316_frame);
INT STR_CU1297585490(STR_CURSOR);
INT STR_lengthrINT(STR);
INT STR_se1725540125(STR, CHAR);
INT STR_sizerINT(STR);
STR CS_OPT1413653650(CS_OPTIONS, STR, dSTR);
STR CS_OPT1526155198(CS_OPTIONS, STR);
STR CS_OPT1942242757(CS_OPTIONS, STR, dSTR, INT*);
STR CS_OPT1970940548(CS_OPTIONS);
STR CS_OPT619607024(CS_OPTIONS, STR);
STR FSETSTR_eltbrSTR(FSETSTR_eltbrSTR_frame);
STR FSETSTR_strrSTR(FSETSTR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_as1755591216(STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_head_INTrSTR(STR, INT);
STR STR_sp663650766(STR_sp663650766_frame);
STR STR_tail_INTrSTR(STR, INT);
STR UNIX_g108606936(UNIX, STR);
void ARRAYS1161048019(ARRAYS1161048019_frame);
void CS_OPT1644060403(CS_OPTIONS, FLISTSTR, STR);
void CS_OPT194850632(CS_OPTIONS);
void CS_OPT287833045(CS_OPTIONS, STR, STR);
void CS_OPT390587743(CS_OPTIONS, STR);
void CS_OPT479416027(CS_OPTIONS, STR);
void CS_OPT611018296(CS_OPTIONS);
void CS_OPT65198073(CS_OPTIONS, STR);
void CS_OPTIONS_usage(CS_OPTIONS);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void STR_CU1355884946(STR_CURSOR, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL CS_OPT1373530461(CS_OPTIONS self, STR fn) {
 BOOL ret_val = BOOL_zero;
 CHAR L11_;
 CHAR L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 L11_=ARR((STR)fn,0); 
 L2 = L11_;
 L31_=L2=='-'; 
 if (L31_) {
  ret_val = FALSE;
  return ret_val;
 }
 L41_=ARR((STR)fn,0); 
 L5 = L41_;
 L61_=L5=='/'; 
 if (L61_) {
  ret_val = TRUE;
  return ret_val;
 }
 L7 = STR_se1725540125(fn, '.');
 L81_=(L7)==(-1); 
 L9 = L81_;
 L101_=!(L9); 
 ret_val = L101_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL CS_OPT1443087191(CS_OPTIONS self, STR nm) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 INT L21 = INT_zero;
 CHAR c = CHAR_zero;
 CHAR L31 = CHAR_zero;
 BOOL L4;
 CHAR L51_;
 CHAR L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 CHAR L101_;
 BOOL L12;
 BOOL L131_;
 CHAR L14;
 L51_=ARR((STR)nm,0); 
 L6 = L51_;
 L71_=L6=='$'; 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L101_=ARR((STR)nm,0); 
  L12 = CHAR_i629101715(L101_);
  L131_=!(L12); 
  L4 = L131_;
 } else {
  L4 = FALSE;
 }
 if (L4) {
  ret_val = FALSE;
  return ret_val;
 }
 {
  struct STR_el1843619312_frame_struct other1_0;
STR_el1843619312_frame noname1 = &other1_0;
  L11 = nm;
  L21 = 1;
  noname1->self = L11;
  noname1->arg1 = L21;
  noname1->state = 0;
  while (1) {
   L14 = STR_el1843619312(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   c = L14;
   L31 = c;
   switch (L31) {
    case 'A': 
     break;
    case 'B': 
     break;
    case 'C': 
     break;
    case 'D': 
     break;
    case 'E': 
     break;
    case 'F': 
     break;
    case 'G': 
     break;
    case 'H': 
     break;
    case 'I': 
     break;
    case 'J': 
     break;
    case 'K': 
     break;
    case 'L': 
     break;
    case 'M': 
     break;
    case 'N': 
     break;
    case 'O': 
     break;
    case 'P': 
     break;
    case 'Q': 
     break;
    case 'R': 
     break;
    case 'S': 
     break;
    case 'T': 
     break;
    case 'U': 
     break;
    case 'V': 
     break;
    case 'W': 
     break;
    case 'X': 
     break;
    case 'Y': 
     break;
    case 'Z': 
     break;
    case '0': 
     break;
    case '1': 
     break;
    case '2': 
     break;
    case '3': 
     break;
    case '4': 
     break;
    case '5': 
     break;
    case '6': 
     break;
    case '7': 
     break;
    case '8': 
     break;
    case '9': 
     break;
    case '_': 
     break;
    default: ;
     ret_val = FALSE;
     return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CS_OPTIONS CS_OPT1986127469(CS_OPTIONS self, PROG p, CS cs) {
 CS_OPTIONS ret_val;
 CS_OPTIONS r;
 dBACK_END be;
 dPARSE pr;
 dINLINE il;
 CS_OPTIONS was_create_self;
 PROG was_create_p;
 CS_OPTIONS ret_val1;
 CS_OPTIONS r1;
 CS_OPTIONS old_create_self;
 PROG old_create_p;
 CS_OPTIONS ret_val2;
 CS_OPTIONS r2;
 FMAPSTRSTR create_self;
 FMAPSTRSTR ret_val3;
 CS_OPTIONS L1;
 OB L2;
 extern STR MAIN;
 extern STR aout;
 was_create_self = self;
 was_create_p = p;
 old_create_self = was_create_self;
 old_create_p = was_create_p;
 L2=ZALLOC(sizeof(struct CS_OPTIONS_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=CS_OPTIONS_tag;
 L1 = ((CS_OPTIONS) L2);
 r2 = L1;
 SATTR(r2,prog,old_create_p);
 SATTR(old_create_p,main_class,((STR) &MAIN));
 create_self = ((FMAPSTRSTR) NULL);
 ret_val3 = ((FMAPSTRSTR) NULL);
 SATTR(r2,module_inclusion,ret_val3);
 ret_val2 = r2;
 r1 = ret_val2;
 SATTR(r1,bounds_all,TRUE);
 SATTR(r1,void_all,TRUE);
 SATTR(was_create_p,void_checks,TRUE);
 SATTR(r1,when_all,TRUE);
 SATTR(r1,return_all,TRUE);
 SATTR(r1,psather_chk,TRUE);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,loop_poll,TRUE);
 SATTR(r,prefetch_weight,15);
 SATTR(r,remote_call_acce,5);
 SATTR(r,local_call_acces,1);
 SATTR(r,local_call_dynam,3);
 SATTR(r,cache_size,1024);
 SATTR(r,cache_slot_size,8);
 SATTR(r,cs,cs);
 be = ATTR(p,back_end);
 switch (TAG(be)) {
  case CGEN_tag:
   SATTR(r,cgen,((CGEN) be)); break;
  default: ;
   FATAL("No applicable type in typecase\nin CS_OPTIONS::create(PROG,CS):SAME\n./Configuration/cs_options.sa:455:14");
 }
 pr = ATTR(p,parse);
 switch (TAG(pr)) {
  case PARSE_tag:
   SATTR(r,parser,((PARSE) pr)); break;
  default: ;
   FATAL("No applicable type in typecase\nin CS_OPTIONS::create(PROG,CS):SAME\n./Configuration/cs_options.sa:457:14");
 }
 il = ATTR(p,inliner);
 switch (TAG(il)) {
  case INLINE_tag:
   SATTR(r,inliner,((INLINE) il)); break;
  default: ;
   FATAL("No applicable type in typecase\nin CS_OPTIONS::create(PROG,CS):SAME\n./Configuration/cs_options.sa:459:14");
 }
 SATTR(ATTR(r,cgen),executable1,((STR) &aout));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTSTR CS_OPT975854719(CS_OPTIONS self, STR name111) {
 FLISTSTR ret_val;
 STR wd;
 FLISTSTR cl;
 BFILE f;
 FSTR fs;
 STR tok;
 INT pos = INT_zero;
 CHAR c = CHAR_zero;
 INT start1 = INT_zero;
 FLISTSTR create_self;
 FLISTSTR ret_val1;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val2;
 BFILE r;
 BFILE error_self;
 BOOL ret_val3 = BOOL_zero;
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val5;
 FILE1 stderr_self;
 FILE1 ret_val6;
 FILE1 r1;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val7;
 FILE1 stderr_self1;
 FILE1 ret_val8;
 FILE1 r2;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val9;
 FILE1 r3;
 FSTR plus_self3;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val10;
 BFILE close_self;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val11 = CHAR_zero;
 FSTR aget_self1;
 INT aget_ind1 = INT_zero;
 CHAR ret_val12 = CHAR_zero;
 FSTR aget_self2;
 INT aget_ind2 = INT_zero;
 CHAR ret_val13 = CHAR_zero;
 FSTR aget_self3;
 INT aget_ind3 = INT_zero;
 CHAR ret_val14 = CHAR_zero;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val15;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val17;
 STR r4;
 FSTR aget_self4;
 INT aget_ind4 = INT_zero;
 CHAR ret_val18 = CHAR_zero;
 ERR1 create_self3;
 ERR1 ret_val19;
 ERR1 plus_self5;
 dSTR plus_s3;
 ERR1 ret_val20;
 FILE1 stderr_self3;
 FILE1 ret_val21;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s4;
 ERR1 ret_val22;
 FILE1 stderr_self4;
 FILE1 ret_val23;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val24;
 FILE1 r7;
 FSTR aget_self5;
 INT aget_ind5 = INT_zero;
 CHAR ret_val25 = CHAR_zero;
 FSTR aget_self6;
 INT aget_ind6 = INT_zero;
 CHAR ret_val26 = CHAR_zero;
 STR plus_self8;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val27;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val28;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val29;
 STR r8;
 ERR1 create_self5;
 ERR1 ret_val30;
 ERR1 plus_self9;
 dSTR plus_s6;
 ERR1 ret_val31;
 FILE1 stderr_self6;
 FILE1 ret_val32;
 FILE1 r9;
 ERR1 plus_self10;
 dSTR plus_s7;
 ERR1 ret_val33;
 FILE1 stderr_self7;
 FILE1 ret_val34;
 FILE1 r10;
 ERR1 plus_self11;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val35;
 FILE1 r11;
 FSTR aget_self7;
 INT aget_ind7 = INT_zero;
 CHAR ret_val36 = CHAR_zero;
 FSTR str_self2;
 STR ret_val37;
 FSTR aget_self8;
 INT aget_ind8 = INT_zero;
 CHAR ret_val38 = CHAR_zero;
 FSTR aget_self9;
 INT aget_ind9 = INT_zero;
 CHAR ret_val39 = CHAR_zero;
 STR plus_self12;
 STR plus_sarg;
 STR ret_val40;
 FSTR aget_self10;
 INT aget_ind10 = INT_zero;
 CHAR ret_val41 = CHAR_zero;
 FSTR aget_self11;
 INT aget_ind11 = INT_zero;
 CHAR ret_val42 = CHAR_zero;
 STR plus_self13;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val43;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val45;
 STR r12;
 STR plus_self14;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val46;
 STR s3;
 CHAR str_self4 = CHAR_zero;
 STR ret_val47;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val48;
 STR r13;
 STR plus_self15;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val49;
 STR s4;
 CHAR str_self5 = CHAR_zero;
 STR ret_val50;
 STR create_self8;
 CHAR create_c4 = CHAR_zero;
 STR ret_val51;
 STR r14;
 STR plus_self16;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val52;
 STR s5;
 CHAR str_self6 = CHAR_zero;
 STR ret_val53;
 STR create_self9;
 CHAR create_c5 = CHAR_zero;
 STR ret_val54;
 STR r15;
 STR plus_self17;
 STR plus_sarg1;
 STR ret_val55;
 BFILE L1;
 OB L2;
 extern STR rb;
 STR L3;
 STR L4;
 BOOL L5;
 extern STR Couldntopenfile;
 FILE1 L6;
 OB L7;
 FILE1 L9;
 OB L10;
 dSTR L12;
 OB L13;
 FILE1 L14;
 OB L15;
 extern STR name1;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 CHAR L211_;
 BOOL L221_;
 INT L231_;
 CHAR L241_;
 CHAR L25;
 BOOL L261_;
 INT L271_;
 BOOL L28;
 BOOL L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 CHAR L341_;
 CHAR L35;
 BOOL L361_;
 CHAR L371_;
 CHAR L38;
 BOOL L391_;
 STR L40;
 INT L41;
 OB L42;
 INT L43;
 BOOL L451_;
 INT L461_;
 CHAR L471_;
 CHAR L48;
 BOOL L491_;
 INT L501_;
 INT L51;
 INT L521_;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 extern STR Unterm820051874;
 FILE1 L57;
 OB L58;
 FILE1 L60;
 OB L61;
 dSTR L63;
 OB L64;
 FILE1 L65;
 OB L66;
 BOOL L68;
 CHAR L691_;
 CHAR L70;
 BOOL L711_;
 INT L721_;
 CHAR L731_;
 CHAR L74;
 BOOL L751_;
 INT L761_;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 BOOL L831_;
 INT L841_;
 INT L85;
 INT L861_;
 INT L87;
 BOOL L881_;
 BOOL L89;
 BOOL L901_;
 extern STR Unterm919914180;
 FILE1 L91;
 OB L92;
 FILE1 L94;
 OB L95;
 dSTR L97;
 OB L98;
 FILE1 L99;
 OB L100;
 CHAR L1021_;
 CHAR L103;
 BOOL L1041_;
 INT L1051_;
 INT L106;
 INT L1071_;
 extern STR name1;
 INT L1081_;
 BOOL L1091_;
 BOOL L110;
 INT L1121_;
 CHAR L1131_;
 CHAR L114;
 BOOL L1151_;
 INT L1161_;
 CHAR L1171_;
 CHAR L118;
 BOOL L1191_;
 INT* L120;
 STR L121;
 BOOL L122;
 BOOL L1231_;
 BOOL L124;
 CHAR L1251_;
 BOOL L126;
 BOOL L1271_;
 INT L128;
 BOOL L1291_;
 CHAR L1301_;
 STR L131;
 INT L132;
 OB L133;
 INT L134;
 INT L1361_;
 extern STR name1;
 STR L137;
 INT L138;
 OB L139;
 INT L140;
 BOOL L142;
 BOOL L1431_;
 STR L144;
 INT L145;
 OB L146;
 INT L147;
 extern STR name1;
 BOOL L149;
 BOOL L1501_;
 BOOL L151;
 CHAR L1521_;
 CHAR L153;
 BOOL L1541_;
 BOOL L155;
 BOOL L1561_;
 STR L157;
 INT L158;
 OB L159;
 INT L160;
 extern STR name1;
 INT L1631_;
 wd = CS_OPT619607024(self, name111);
 create_self = ((FLISTSTR) NULL);
 ret_val1 = ((FLISTSTR) NULL);
 cl = ret_val1;
 open_for_read_se = ((BFILE) NULL);
 open_for_read_nm = name111;
 L2=ZALLOC(sizeof(struct BFILE_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=BFILE_tag;
 L1 = ((BFILE) L2);
 r = L1;
 L3 = open_for_read_nm;
 L4 = ((STR) &rb);
 SATTR(r,fp,fopen(((L3==NULL)?NULL:L3->arr_part), ((L4==NULL)?NULL:L4->arr_part)));
 ret_val2 = r;
 f = ret_val2;
 error_self = f;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L5 = TRUE;
 } else {
  L5 = ferror(ATTR(error_self,fp));
 }
 ret_val3 = L5;
 if (ret_val3) {
  create_self1 = ((ERR1) NULL);
  ret_val4 = create_self1;
  plus_self = ret_val4;
  plus_s = ((dSTR) ((STR) &Couldntopenfile));
  stderr_self = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stderr);
  ret_val6 = r1;
  FILE_plus_dSTR(ret_val6, plus_s);
  ret_val5 = plus_self;
  plus_self1 = ret_val5;
  plus_s1 = ((dSTR) name111);
  stderr_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stderr);
  ret_val8 = r2;
  FILE_plus_dSTR(ret_val8, plus_s1);
  ret_val7 = plus_self1;
  plus_self2 = ret_val7;
  L13=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  memset(L13,0,sizeof(struct CHAR_boxed_struct));
  ((OB)L13)->header.tag=CHAR_tag;
  L12 = (dSTR)((CHAR_boxed) L13);
  ((CHAR_boxed) L12)->immutable_part = '\n';
  plus_s2 = L12;
  stderr_self2 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stderr);
  ret_val9 = r3;
  FILE_plus_dSTR(ret_val9, plus_s2);
  CS_OPTIONS_usage(self);
 }
 plus_self3 = BFILE_fstrrFSTR(f);
 plus_c = ' ';
 ret_val10 = FSTR_p399773021(plus_self3, plus_c);
 fs = ret_val10;
 close_self = f;
 fclose(ATTR(close_self,fp));
 tok = ((STR) &name1);
 pos = 0;
 while (1) {
  L17 = FSTR_sizerINT(fs);
  L181_=(pos)<(L17); 
  L19 = L181_;
  L201_=!(L19); 
  if (L201_) {
   goto after_loop;
  }
  aget_self = fs;
  aget_ind = pos;
  L211_=ARR((FSTR)aget_self,aget_ind); 
  ret_val11 = L211_;
  c = ret_val11;
  L221_=c=='-'; 
  if (L221_) {
   aget_self1 = fs;
   L231_=INTPLUS(pos,1); 
   aget_ind1 = L231_;
   L241_=ARR((FSTR)aget_self1,aget_ind1); 
   ret_val12 = L241_;
   L25 = ret_val12;
   L261_=L25=='-'; 
   if (L261_) {
    while (1) {
     L271_=INTPLUS(pos,1); 
     pos = L271_;
     L30 = FSTR_sizerINT(fs);
     L311_=(pos)<(L30); 
     L32 = L311_;
     L331_=!(L32); 
     if (L331_) {
      L29 = TRUE;
     } else {
      aget_self2 = fs;
      aget_ind2 = pos;
      L341_=ARR((FSTR)aget_self2,aget_ind2); 
      ret_val13 = L341_;
      L35 = ret_val13;
      L361_=L35=='\n'; 
      L29 = L361_;
     }
     if (L29) {
      L28 = TRUE;
     } else {
      aget_self3 = fs;
      aget_ind3 = pos;
      L371_=ARR((FSTR)aget_self3,aget_ind3); 
      ret_val14 = L371_;
      L38 = ret_val14;
      L391_=L38=='\r'; 
      L28 = L391_;
     }
     if (L28) {
      goto after_loop1;
     }
    }
    after_loop1: ;
   }
   else {
    plus_self4 = tok;
    plus_arg = '-';
    str_self = plus_arg;
    create_self2 = ((STR) NULL);
    create_c = str_self;
    L41 = 1;
    L43=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L41)*sizeof(CHAR);
    L42=ZALLOC_LEAF_BIG(L43);
    if (L42==NULL) FATAL("Unable to allocate more memory");
    memset(L42,0,L43);
    ((OB)L42)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L42)->header.destroyed=0;
#endif

    L40 = ((STR) L42);
    L40->asize = L41;
    r4 = L40;
    SARR((STR)r4,0,create_c); 
    ;
    ret_val17 = r4;
    ret_val16 = ret_val17;
    s = ret_val16;
    ret_val15 = STR_ap1077157958(plus_self4, s, TRUE);
    tok = ret_val15;
   }
  }
  else {
   L451_=c=='('; 
   if (L451_) {
    aget_self4 = fs;
    L461_=INTPLUS(pos,1); 
    aget_ind4 = L461_;
    L471_=ARR((FSTR)aget_self4,aget_ind4); 
    ret_val18 = L471_;
    L48 = ret_val18;
    L491_=L48=='*'; 
    if (L491_) {
     while (1) {
      L501_=INTPLUS(pos,1); 
      pos = L501_;
      L51 = FSTR_sizerINT(fs);
      L521_=INTMINUS(L51,1); 
      L53 = L521_;
      L541_=(pos)<(L53); 
      L55 = L541_;
      L561_=!(L55); 
      if (L561_) {
       create_self3 = ((ERR1) NULL);
       ret_val19 = create_self3;
       plus_self5 = ret_val19;
       plus_s3 = ((dSTR) ((STR) &Unterm820051874));
       stderr_self3 = ((FILE1) NULL);
       L58=ZALLOC(sizeof(struct FILE1_struct));
       if (L58==NULL) FATAL("Unable to allocate more memory");
       ((OB)L58)->header.tag=FILE1_tag;
       L57 = ((FILE1) L58);
       r5 = L57;
       
       SATTR(r5,fp,stderr);
       ret_val21 = r5;
       FILE_plus_dSTR(ret_val21, plus_s3);
       ret_val20 = plus_self5;
       plus_self6 = ret_val20;
       plus_s4 = ((dSTR) name111);
       stderr_self4 = ((FILE1) NULL);
       L61=ZALLOC(sizeof(struct FILE1_struct));
       if (L61==NULL) FATAL("Unable to allocate more memory");
       ((OB)L61)->header.tag=FILE1_tag;
       L60 = ((FILE1) L61);
       r6 = L60;
       
       SATTR(r6,fp,stderr);
       ret_val23 = r6;
       FILE_plus_dSTR(ret_val23, plus_s4);
       ret_val22 = plus_self6;
       plus_self7 = ret_val22;
       L64=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L64==NULL) FATAL("Unable to allocate more memory");
       memset(L64,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L64)->header.tag=CHAR_tag;
       L63 = (dSTR)((CHAR_boxed) L64);
       ((CHAR_boxed) L63)->immutable_part = '\n';
       plus_s5 = L63;
       stderr_self5 = ((FILE1) NULL);
       L66=ZALLOC(sizeof(struct FILE1_struct));
       if (L66==NULL) FATAL("Unable to allocate more memory");
       ((OB)L66)->header.tag=FILE1_tag;
       L65 = ((FILE1) L66);
       r7 = L65;
       
       SATTR(r7,fp,stderr);
       ret_val24 = r7;
       FILE_plus_dSTR(ret_val24, plus_s5);
       goto after_loop2;
      }
      aget_self5 = fs;
      aget_ind5 = pos;
      L691_=ARR((FSTR)aget_self5,aget_ind5); 
      ret_val25 = L691_;
      L70 = ret_val25;
      L711_=L70=='*'; 
      if (L711_) {
       aget_self6 = fs;
       L721_=INTPLUS(pos,1); 
       aget_ind6 = L721_;
       L731_=ARR((FSTR)aget_self6,aget_ind6); 
       ret_val26 = L731_;
       L74 = ret_val26;
       L751_=L74==')'; 
       L68 = L751_;
      } else {
       L68 = FALSE;
      }
      if (L68) {
       goto after_loop2;
      }
     }
     after_loop2: ;
     L761_=INTPLUS(pos,1); 
     pos = L761_;
    }
    else {
     plus_self8 = tok;
     plus_arg1 = '(';
     str_self1 = plus_arg1;
     create_self4 = ((STR) NULL);
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
     r8 = L77;
     SARR((STR)r8,0,create_c1); 
     ;
     ret_val29 = r8;
     ret_val28 = ret_val29;
     s1 = ret_val28;
     ret_val27 = STR_ap1077157958(plus_self8, s1, TRUE);
     tok = ret_val27;
    }
   }
   else {
    L831_=c=='\"'; 
    if (L831_) {
     start1 = pos;
     while (1) {
      L841_=INTPLUS(pos,1); 
      pos = L841_;
      L85 = FSTR_sizerINT(fs);
      L861_=INTMINUS(L85,1); 
      L87 = L861_;
      L881_=(pos)<(L87); 
      L89 = L881_;
      L901_=!(L89); 
      if (L901_) {
       create_self5 = ((ERR1) NULL);
       ret_val30 = create_self5;
       plus_self9 = ret_val30;
       plus_s6 = ((dSTR) ((STR) &Unterm919914180));
       stderr_self6 = ((FILE1) NULL);
       L92=ZALLOC(sizeof(struct FILE1_struct));
       if (L92==NULL) FATAL("Unable to allocate more memory");
       ((OB)L92)->header.tag=FILE1_tag;
       L91 = ((FILE1) L92);
       r9 = L91;
       
       SATTR(r9,fp,stderr);
       ret_val32 = r9;
       FILE_plus_dSTR(ret_val32, plus_s6);
       ret_val31 = plus_self9;
       plus_self10 = ret_val31;
       plus_s7 = ((dSTR) name111);
       stderr_self7 = ((FILE1) NULL);
       L95=ZALLOC(sizeof(struct FILE1_struct));
       if (L95==NULL) FATAL("Unable to allocate more memory");
       ((OB)L95)->header.tag=FILE1_tag;
       L94 = ((FILE1) L95);
       r10 = L94;
       
       SATTR(r10,fp,stderr);
       ret_val34 = r10;
       FILE_plus_dSTR(ret_val34, plus_s7);
       ret_val33 = plus_self10;
       plus_self11 = ret_val33;
       L98=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
       if (L98==NULL) FATAL("Unable to allocate more memory");
       memset(L98,0,sizeof(struct CHAR_boxed_struct));
       ((OB)L98)->header.tag=CHAR_tag;
       L97 = (dSTR)((CHAR_boxed) L98);
       ((CHAR_boxed) L97)->immutable_part = '\n';
       plus_s8 = L97;
       stderr_self8 = ((FILE1) NULL);
       L100=ZALLOC(sizeof(struct FILE1_struct));
       if (L100==NULL) FATAL("Unable to allocate more memory");
       ((OB)L100)->header.tag=FILE1_tag;
       L99 = ((FILE1) L100);
       r11 = L99;
       
       SATTR(r11,fp,stderr);
       ret_val35 = r11;
       FILE_plus_dSTR(ret_val35, plus_s8);
       goto after_loop3;
      }
      aget_self7 = fs;
      aget_ind7 = pos;
      L1021_=ARR((FSTR)aget_self7,aget_ind7); 
      ret_val36 = L1021_;
      L103 = ret_val36;
      L1041_=L103=='\"'; 
      if (L1041_) {
       goto after_loop3;
      }
     }
     after_loop3: ;
     L1051_=INTMINUS(pos,start1); 
     L106 = L1051_;
     L1071_=INTPLUS(L106,1); 
     str_self2 = FSTR_s1920457899(fs, start1, L1071_);
     ret_val37 = STR_fr1097270334(((STR) NULL), str_self2);
     tok = ret_val37;
     tok = STR_as1755591216(tok);
     tok = CS_OPT1413653650(self, name111, ((dSTR) tok));
     cl = FLISTS1425610062(cl, tok);
     tok = ((STR) &name1);
     L1081_=INTPLUS(pos,1); 
     pos = L1081_;
    }
    else {
     L1091_=c=='$'; 
     if (L1091_) {
      aget_self8 = fs;
      L1121_=INTPLUS(pos,1); 
      aget_ind8 = L1121_;
      L1131_=ARR((FSTR)aget_self8,aget_ind8); 
      ret_val38 = L1131_;
      L114 = ret_val38;
      L1151_=L114=='('; 
      if (L1151_) {
       L110 = TRUE;
      } else {
       aget_self9 = fs;
       L1161_=INTPLUS(pos,1); 
       aget_ind9 = L1161_;
       L1171_=ARR((FSTR)aget_self9,aget_ind9); 
       ret_val39 = L1171_;
       L118 = ret_val39;
       L1191_=L118=='{'; 
       L110 = L1191_;
      }
      if (L110) {
       plus_self12 = tok;
       L120 = &pos;
       L121 = CS_OPT1942242757(self, name111, ((dSTR) fs), L120);
       plus_sarg = L121;
       ret_val40 = STR_ap2004550586(plus_self12, plus_sarg);
       tok = ret_val40;
       L122 = (tok==((STR) NULL));
       L1231_=!(L122); 
       if (L1231_) {
        while (1) {
         aget_self10 = fs;
         aget_ind10 = pos;
         L1251_=ARR((FSTR)aget_self10,aget_ind10); 
         ret_val41 = L1251_;
         L126 = CHAR_i84735023(ret_val41);
         L1271_=!(L126); 
         if (L1271_) {
          L128 = FSTR_sizerINT(fs);
          L1291_=(pos)<(L128); 
          L124 = L1291_;
         } else {
          L124 = FALSE;
         }
         if (L124) {
         }
         else {
          goto after_loop4;
         }
         plus_self13 = tok;
         aget_self11 = fs;
         aget_ind11 = pos;
         L1301_=ARR((FSTR)aget_self11,aget_ind11); 
         ret_val42 = L1301_;
         plus_arg2 = ret_val42;
         str_self3 = plus_arg2;
         create_self6 = ((STR) NULL);
         create_c2 = str_self3;
         L132 = 1;
         L134=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L132)*sizeof(CHAR);
         L133=ZALLOC_LEAF_BIG(L134);
         if (L133==NULL) FATAL("Unable to allocate more memory");
         memset(L133,0,L134);
         ((OB)L133)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L133)->header.destroyed=0;
#endif

         L131 = ((STR) L133);
         L131->asize = L132;
         r12 = L131;
         SARR((STR)r12,0,create_c2); 
         ;
         ret_val45 = r12;
         ret_val44 = ret_val45;
         s2 = ret_val44;
         ret_val43 = STR_ap1077157958(plus_self13, s2, TRUE);
         tok = ret_val43;
         L1361_=INTPLUS(pos,1); 
         pos = L1361_;
        }
        after_loop4: ;
        cl = FLISTS1425610062(cl, tok);
       }
       tok = ((STR) &name1);
      }
      else {
       plus_self14 = tok;
       plus_arg3 = '$';
       str_self4 = plus_arg3;
       create_self7 = ((STR) NULL);
       create_c3 = str_self4;
       L138 = 1;
       L140=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L138)*sizeof(CHAR);
       L139=ZALLOC_LEAF_BIG(L140);
       if (L139==NULL) FATAL("Unable to allocate more memory");
       memset(L139,0,L140);
       ((OB)L139)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L139)->header.destroyed=0;
#endif

       L137 = ((STR) L139);
       L137->asize = L138;
       r13 = L137;
       SARR((STR)r13,0,create_c3); 
       ;
       ret_val48 = r13;
       ret_val47 = ret_val48;
       s3 = ret_val47;
       ret_val46 = STR_ap1077157958(plus_self14, s3, TRUE);
       tok = ret_val46;
      }
     }
     else {
      L142 = CHAR_i84735023(c);
      L1431_=!(L142); 
      if (L1431_) {
       plus_self15 = tok;
       plus_arg4 = c;
       str_self5 = plus_arg4;
       create_self8 = ((STR) NULL);
       create_c4 = str_self5;
       L145 = 1;
       L147=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L145)*sizeof(CHAR);
       L146=ZALLOC_LEAF_BIG(L147);
       if (L146==NULL) FATAL("Unable to allocate more memory");
       memset(L146,0,L147);
       ((OB)L146)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L146)->header.destroyed=0;
#endif

       L144 = ((STR) L146);
       L144->asize = L145;
       r14 = L144;
       SARR((STR)r14,0,create_c4); 
       ;
       ret_val51 = r14;
       ret_val50 = ret_val51;
       s4 = ret_val50;
       ret_val49 = STR_ap1077157958(plus_self15, s4, TRUE);
       tok = ret_val49;
      }
      else {
       L149 = STR_is111530248(tok, ((STR) &name1));
       L1501_=!(L149); 
       if (L1501_) {
        if (CS_OPT1373530461(self, tok)) {
         L1521_=ARR((STR)tok,0); 
         L153 = L1521_;
         L1541_=L153=='/'; 
         L155 = L1541_;
         L1561_=!(L155); 
         L151 = L1561_;
        } else {
         L151 = FALSE;
        }
        if (L151) {
         plus_self16 = wd;
         plus_arg5 = '/';
         str_self6 = plus_arg5;
         create_self9 = ((STR) NULL);
         create_c5 = str_self6;
         L158 = 1;
         L160=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L158)*sizeof(CHAR);
         L159=ZALLOC_LEAF_BIG(L160);
         if (L159==NULL) FATAL("Unable to allocate more memory");
         memset(L159,0,L160);
         ((OB)L159)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L159)->header.destroyed=0;
#endif

         L157 = ((STR) L159);
         L157->asize = L158;
         r15 = L157;
         SARR((STR)r15,0,create_c5); 
         ;
         ret_val54 = r15;
         ret_val53 = ret_val54;
         s5 = ret_val53;
         ret_val52 = STR_ap1077157958(plus_self16, s5, TRUE);
         plus_self17 = ret_val52;
         plus_sarg1 = tok;
         ret_val55 = STR_ap2004550586(plus_self17, plus_sarg1);
         tok = ret_val55;
        }
        if (CS_OPT1373530461(self, tok)) {
         SATTR(self,module_inclusion,FMAPST859022540(ATTR(self,module_inclusion), tok, name111));
        }
        cl = FLISTS1425610062(cl, tok);
        tok = ((STR) &name1);
       }
      }
     }
    }
   }
  }
  L1631_=INTPLUS(pos,1); 
  pos = L1631_;
 }
 after_loop: ;
 ret_val = cl;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETSTR CS_OPT595032350(CS_OPTIONS self) {
 FSETSTR ret_val;
 FSETSTR r;
 STR arg;
 STR L11;
 CHAR L21 = CHAR_zero;
 STR piece;
 FSETSTR create_self;
 FSETSTR ret_val1;
 CS_OPTIONS more_args_self;
 BOOL ret_val2 = BOOL_zero;
 CS_OPTIONS rewind_self;
 INT L3;
 ARRAYSTR L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 STR L10;
 BOOL L12;
 CHAR L131_;
 CHAR L14;
 BOOL L151_;
 BOOL L16;
 CHAR L171_;
 CHAR L18;
 BOOL L191_;
 CHAR L201_;
 CHAR L22;
 BOOL L231_;
 INT L24;
 INT L251_;
 create_self = ((FSETSTR) NULL);
 ret_val1 = ((FSETSTR) NULL);
 r = ret_val1;
 while (1) {
  more_args_self = self;
  L3 = ATTR(more_args_self,next);
  L4 = ATTR(more_args_self,args);
  L51_=(L4)==NULL?0:ASIZE((ARRAYSTR)L4); 
  L6 = L51_;
  L71_=(L3)<(L6); 
  ret_val2 = L71_;
  if (ret_val2) {
  }
  else {
   goto after_loop;
  }
  arg = CS_OPT1970940548(self);
  L8 = (arg==((STR) NULL));
  L91_=!(L8); 
  if (L91_) {
   {
    struct STR_sp663650766_frame_struct other2_0;
STR_sp663650766_frame noname1 = &other2_0;
    L11 = arg;
    L21 = ' ';
    noname1->self = L11;
    noname1->arg1 = L21;
    noname1->state = 0;
    while (1) {
     L10 = STR_sp663650766(noname1);
     if (noname1->state == -1) {
      goto after_loop1;
     }
     piece = L10;
     L131_=ARR((STR)piece,0); 
     L14 = L131_;
     L151_=L14=='-'; 
     if (L151_) {
      L171_=ARR((STR)piece,1); 
      L18 = L171_;
      L191_=L18=='l'; 
      if (L191_) {
       L16 = TRUE;
      } else {
       L201_=ARR((STR)piece,1); 
       L22 = L201_;
       L231_=L22=='L'; 
       L16 = L231_;
      }
      L12 = L16;
     } else {
      L12 = FALSE;
     }
     if (L12) {
      r = FSETST1404644833(r, piece);
     }
     else {
      if (CS_OPT1373530461(self, piece)) {
       r = FSETST1404644833(r, piece);
      }
      else {
       rewind_self = self;
       L24 = ATTR(rewind_self,next);
       L251_=INTMINUS(L24,1); 
       SATTR(rewind_self,next,L251_);
       ret_val = r;
       return ret_val;
      }
     }
    }
   }
   after_loop1: ;
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1413653650(CS_OPTIONS self, STR fname, dSTR arg_str) {
 STR ret_val;
 STR s;
 INT i = INT_zero;
 STR res = ((STR) NULL);
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
 STR r;
 dSTR L1;
 INT L2;
 BOOL L31_;
 CHAR L41_;
 CHAR L5;
 BOOL L61_;
 INT* L7;
 STR L8;
 CHAR L91_;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 INT L151_;
 L1 = arg_str;
 s = (*dSTR_strrSTR[TAG(L1)])(L1);
 i = 0;
 while (1) {
  L2 = STR_sizerINT(s);
  L31_=(i)<(L2); 
  if (L31_) {
  }
  else {
   goto after_loop;
  }
  L41_=ARR((STR)s,i); 
  L5 = L41_;
  L61_=L5=='$'; 
  if (L61_) {
   plus_self = res;
   L7 = &i;
   L8 = CS_OPT1942242757(self, fname, ((dSTR) s), L7);
   plus_sarg = L8;
   ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
   res = ret_val1;
  }
  else {
   plus_self1 = res;
   L91_=ARR((STR)s,i); 
   plus_arg = L91_;
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L11 = 1;
   L13=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L11)*sizeof(CHAR);
   L12=ZALLOC_LEAF_BIG(L13);
   if (L12==NULL) FATAL("Unable to allocate more memory");
   memset(L12,0,L13);
   ((OB)L12)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L12)->header.destroyed=0;
#endif

   L10 = ((STR) L12);
   L10->asize = L11;
   r = L10;
   SARR((STR)r,0,create_c); 
   ;
   ret_val4 = r;
   ret_val3 = ret_val4;
   s1 = ret_val3;
   ret_val2 = STR_ap1077157958(plus_self1, s1, TRUE);
   res = ret_val2;
   L151_=INTPLUS(i,1); 
   i = L151_;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1526155198(CS_OPTIONS self, STR a) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_lengthrINT(a);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)a,pos); 
   L8 = L71_;
   L91_=L8=='.'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 L10 = STR_lengthrINT(a);
 L121_=INTMINUS(L10,pos); 
 ret_val = STR_tail_INTrSTR(a, L121_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1942242757(CS_OPTIONS self, STR fname, dSTR arg_str, INT* pos) {
 INT L1 = *pos;/*Local for arg*/
 STR ret_val;
 STR s;
 STR e = ((STR) NULL);
 CHAR op = CHAR_zero;
 BOOL skip1 = BOOL_zero;
 CHAR cp = CHAR_zero;
 STR tenv;
 ERR1 create_self;
 ERR1 ret_val1;
 ERR1 plus_self;
 dSTR plus_s;
 ERR1 ret_val2;
 FILE1 stderr_self;
 FILE1 ret_val3;
 FILE1 r;
 ERR1 plus_self1;
 dSTR plus_s1;
 ERR1 ret_val4;
 FILE1 stderr_self1;
 FILE1 ret_val5;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val6;
 FILE1 stderr_self2;
 FILE1 ret_val7;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val8;
 FILE1 stderr_self3;
 FILE1 ret_val9;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 ERR1 ret_val10;
 FILE1 stderr_self4;
 FILE1 ret_val11;
 FILE1 r4;
 ERR1 plus_self5;
 dSTR plus_s5;
 ERR1 ret_val12;
 FILE1 stderr_self5;
 FILE1 ret_val13;
 FILE1 r5;
 ERR1 plus_self6;
 dSTR plus_s6;
 ERR1 ret_val14;
 FILE1 stderr_self6;
 FILE1 ret_val15;
 FILE1 r6;
 ERR1 plus_self7;
 dSTR plus_s7;
 ERR1 ret_val16;
 FILE1 stderr_self7;
 FILE1 ret_val17;
 FILE1 r7;
 ERR1 plus_self8;
 dSTR plus_s8;
 FILE1 stderr_self8;
 FILE1 ret_val18;
 FILE1 r8;
 STR plus_self9;
 CHAR plus_arg = CHAR_zero;
 STR ret_val19;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val20;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val21;
 STR r9;
 STR plus_self10;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val22;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val23;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r10;
 STR plus_self11;
 STR plus_sarg;
 STR ret_val25;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val26;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val27;
 STR create_self3;
 CHAR create_c2 = CHAR_zero;
 STR ret_val28;
 STR r11;
 dSTR L2;
 INT L31_;
 INT L4;
 CHAR L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L81_;
 BOOL L91_;
 extern STR name1;
 INT L101_;
 BOOL L11;
 INT L12;
 BOOL L131_;
 CHAR L141_;
 CHAR L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 BOOL L19;
 BOOL L20;
 BOOL L21;
 CHAR L221_;
 CHAR L23;
 BOOL L241_;
 CHAR L251_;
 CHAR L26;
 BOOL L271_;
 CHAR L281_;
 CHAR L29;
 BOOL L301_;
 CHAR L321_;
 CHAR L33;
 BOOL L341_;
 extern STR Unterm889448459;
 FILE1 L35;
 OB L36;
 extern STR name8;
 FILE1 L38;
 OB L39;
 FILE1 L41;
 OB L42;
 extern STR in;
 FILE1 L44;
 OB L45;
 FILE1 L47;
 OB L48;
 extern STR assume;
 FILE1 L50;
 OB L52;
 dSTR L54;
 OB L55;
 FILE1 L56;
 OB L57;
 extern STR name19;
 FILE1 L59;
 OB L60;
 extern STR skipthistoken;
 FILE1 L62;
 OB L63;
 CHAR L651_;
 STR L66;
 INT L67;
 OB L68;
 INT L69;
 INT L721_;
 BOOL L731_;
 INT L74;
 BOOL L751_;
 INT L761_;
 extern STR d1;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 STR L83;
 INT L84;
 OB L85;
 INT L86;
 L2 = arg_str;
 s = (*dSTR_strrSTR[TAG(L2)])(L2);
 L31_=INTPLUS(L1,1); 
 L4 = L31_;
 L51_=ARR((STR)s,L4); 
 op = L51_;
 L71_=op=='{'; 
 if (L71_) {
  L6 = TRUE;
 } else {
  L81_=op=='('; 
  L6 = L81_;
 }
 if (L6) {
  skip1 = FALSE;
  cp = '}';
  L91_=op=='('; 
  if (L91_) {
   cp = ')';
  }
  tenv = ((STR) &name1);
  L101_=INTPLUS(L1,2); 
  L1 = L101_;
  while (1) {
   L12 = STR_sizerINT(s);
   L131_=(L1)<(L12); 
   if (L131_) {
    L141_=ARR((STR)s,L1); 
    L15 = L141_;
    L161_=L15==cp; 
    L17 = L161_;
    L181_=!(L17); 
    L11 = L181_;
   } else {
    L11 = FALSE;
   }
   if (L11) {
   }
   else {
    goto after_loop;
   }
   L221_=ARR((STR)s,L1); 
   L23 = L221_;
   L241_=L23==')'; 
   if (L241_) {
    L21 = TRUE;
   } else {
    L251_=ARR((STR)s,L1); 
    L26 = L251_;
    L271_=L26=='}'; 
    L21 = L271_;
   }
   if (L21) {
    L20 = TRUE;
   } else {
    L281_=ARR((STR)s,L1); 
    L29 = L281_;
    L301_=L29=='\n'; 
    L20 = L301_;
   }
   if (L20) {
    L19 = TRUE;
   } else {
    L321_=ARR((STR)s,L1); 
    L33 = L321_;
    L341_=L33=='\r'; 
    L19 = L341_;
   }
   if (L19) {
    create_self = ((ERR1) NULL);
    ret_val1 = create_self;
    plus_self = ret_val1;
    plus_s = ((dSTR) ((STR) &Unterm889448459));
    stderr_self = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r = L35;
    
    SATTR(r,fp,stderr);
    ret_val3 = r;
    FILE_plus_dSTR(ret_val3, plus_s);
    ret_val2 = plus_self;
    plus_self1 = ret_val2;
    plus_s1 = ((dSTR) ((STR) &name8));
    stderr_self1 = ((FILE1) NULL);
    L39=ZALLOC(sizeof(struct FILE1_struct));
    if (L39==NULL) FATAL("Unable to allocate more memory");
    ((OB)L39)->header.tag=FILE1_tag;
    L38 = ((FILE1) L39);
    r1 = L38;
    
    SATTR(r1,fp,stderr);
    ret_val5 = r1;
    FILE_plus_dSTR(ret_val5, plus_s1);
    ret_val4 = plus_self1;
    plus_self2 = ret_val4;
    plus_s2 = ((dSTR) tenv);
    stderr_self2 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L41 = ((FILE1) L42);
    r2 = L41;
    
    SATTR(r2,fp,stderr);
    ret_val7 = r2;
    FILE_plus_dSTR(ret_val7, plus_s2);
    ret_val6 = plus_self2;
    plus_self3 = ret_val6;
    plus_s3 = ((dSTR) ((STR) &in));
    stderr_self3 = ((FILE1) NULL);
    L45=ZALLOC(sizeof(struct FILE1_struct));
    if (L45==NULL) FATAL("Unable to allocate more memory");
    ((OB)L45)->header.tag=FILE1_tag;
    L44 = ((FILE1) L45);
    r3 = L44;
    
    SATTR(r3,fp,stderr);
    ret_val9 = r3;
    FILE_plus_dSTR(ret_val9, plus_s3);
    ret_val8 = plus_self3;
    plus_self4 = ret_val8;
    plus_s4 = ((dSTR) fname);
    stderr_self4 = ((FILE1) NULL);
    L48=ZALLOC(sizeof(struct FILE1_struct));
    if (L48==NULL) FATAL("Unable to allocate more memory");
    ((OB)L48)->header.tag=FILE1_tag;
    L47 = ((FILE1) L48);
    r4 = L47;
    
    SATTR(r4,fp,stderr);
    ret_val11 = r4;
    FILE_plus_dSTR(ret_val11, plus_s4);
    ret_val10 = plus_self4;
    plus_self5 = ret_val10;
    plus_s5 = ((dSTR) ((STR) &assume));
    stderr_self5 = ((FILE1) NULL);
    L52=ZALLOC(sizeof(struct FILE1_struct));
    if (L52==NULL) FATAL("Unable to allocate more memory");
    ((OB)L52)->header.tag=FILE1_tag;
    L50 = ((FILE1) L52);
    r5 = L50;
    
    SATTR(r5,fp,stderr);
    ret_val13 = r5;
    FILE_plus_dSTR(ret_val13, plus_s5);
    ret_val12 = plus_self5;
    plus_self6 = ret_val12;
    L55=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
    if (L55==NULL) FATAL("Unable to allocate more memory");
    memset(L55,0,sizeof(struct CHAR_boxed_struct));
    ((OB)L55)->header.tag=CHAR_tag;
    L54 = (dSTR)((CHAR_boxed) L55);
    ((CHAR_boxed) L54)->immutable_part = cp;
    plus_s6 = L54;
    stderr_self6 = ((FILE1) NULL);
    L57=ZALLOC(sizeof(struct FILE1_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=FILE1_tag;
    L56 = ((FILE1) L57);
    r6 = L56;
    
    SATTR(r6,fp,stderr);
    ret_val15 = r6;
    FILE_plus_dSTR(ret_val15, plus_s6);
    ret_val14 = plus_self6;
    plus_self7 = ret_val14;
    plus_s7 = ((dSTR) ((STR) &name19));
    stderr_self7 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r7 = L59;
    
    SATTR(r7,fp,stderr);
    ret_val17 = r7;
    FILE_plus_dSTR(ret_val17, plus_s7);
    ret_val16 = plus_self7;
    plus_self8 = ret_val16;
    plus_s8 = ((dSTR) ((STR) &skipthistoken));
    stderr_self8 = ((FILE1) NULL);
    L63=ZALLOC(sizeof(struct FILE1_struct));
    if (L63==NULL) FATAL("Unable to allocate more memory");
    ((OB)L63)->header.tag=FILE1_tag;
    L62 = ((FILE1) L63);
    r8 = L62;
    
    SATTR(r8,fp,stderr);
    ret_val18 = r8;
    FILE_plus_dSTR(ret_val18, plus_s8);
    skip1 = TRUE;
    goto after_loop;
   }
   plus_self9 = tenv;
   L651_=ARR((STR)s,L1); 
   plus_arg = L651_;
   str_self = plus_arg;
   create_self1 = ((STR) NULL);
   create_c = str_self;
   L67 = 1;
   L69=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L67)*sizeof(CHAR);
   L68=ZALLOC_LEAF_BIG(L69);
   if (L68==NULL) FATAL("Unable to allocate more memory");
   memset(L68,0,L69);
   ((OB)L68)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L68)->header.destroyed=0;
#endif

   L66 = ((STR) L68);
   L66->asize = L67;
   r9 = L66;
   SARR((STR)r9,0,create_c); 
   ;
   ret_val21 = r9;
   ret_val20 = ret_val21;
   s1 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self9, s1, TRUE);
   tenv = ret_val19;
   L721_=INTPLUS(L1,1); 
   L1 = L721_;
  }
  after_loop: ;
  L731_=!(skip1); 
  if (L731_) {
   L74 = STR_sizerINT(s);
   L751_=(L1)<(L74); 
   if (L751_) {
    L761_=INTPLUS(L1,1); 
    L1 = L761_;
   }
   e = UNIX_g108606936(((UNIX) NULL), tenv);
   if ((e==((STR) NULL))) {
    plus_self10 = ((STR) &d1);
    plus_arg1 = op;
    str_self1 = plus_arg1;
    create_self2 = ((STR) NULL);
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
    r10 = L77;
    SARR((STR)r10,0,create_c1); 
    ;
    ret_val24 = r10;
    ret_val23 = ret_val24;
    s2 = ret_val23;
    ret_val22 = STR_ap1077157958(plus_self10, s2, TRUE);
    plus_self11 = ret_val22;
    plus_sarg = tenv;
    ret_val25 = STR_ap2004550586(plus_self11, plus_sarg);
    plus_self12 = ret_val25;
    plus_arg2 = cp;
    str_self2 = plus_arg2;
    create_self3 = ((STR) NULL);
    create_c2 = str_self2;
    L84 = 1;
    L86=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L84)*sizeof(CHAR);
    L85=ZALLOC_LEAF_BIG(L86);
    if (L85==NULL) FATAL("Unable to allocate more memory");
    memset(L85,0,L86);
    ((OB)L85)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L85)->header.destroyed=0;
#endif

    L83 = ((STR) L85);
    L83->asize = L84;
    r11 = L83;
    SARR((STR)r11,0,create_c2); 
    ;
    ret_val28 = r11;
    ret_val27 = ret_val28;
    s3 = ret_val27;
    ret_val26 = STR_ap1077157958(plus_self12, s3, TRUE);
    e = ret_val26;
   }
  }
 }
 ret_val = e;
 *pos = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT1970940548(CS_OPTIONS self) {
 STR ret_val;
 STR res;
 CS_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 OUT create_self;
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
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 ARRAYSTR L6;
 INT L7;
 STR L81_;
 INT L9;
 INT L101_;
 extern STR Arg;
 FILE1 L11;
 OB L12;
 FILE1 L14;
 OB L15;
 extern STR name9;
 FILE1 L17;
 OB L18;
 extern STR name1;
 more_args_self = self;
 L1 = ATTR(more_args_self,next);
 L2 = ATTR(more_args_self,args);
 L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
 L4 = L31_;
 L51_=(L1)<(L4); 
 ret_val1 = L51_;
 if (ret_val1) {
  L6 = ATTR(self,args);
  L7 = ATTR(self,next);
  L81_=(STR)ARR((ARRAYSTR)L6,L7); 
  res = L81_;
  L9 = ATTR(self,next);
  L101_=INTPLUS(L9,1); 
  SATTR(self,next,L101_);
  if (ATTR(ATTR(self,prog),prolix)) {
   create_self = ((OUT) NULL);
   ret_val2 = create_self;
   plus_self = ret_val2;
   plus_s = ((STR) &Arg);
   stdout_self = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L11 = ((FILE1) L12);
   r = L11;
   
   SATTR(r,fp,stdout);
   ret_val4 = r;
   FILE_plus_STR(ret_val4, plus_s);
   ret_val3 = plus_self;
   plus_self1 = ret_val3;
   plus_s1 = res;
   stdout_self1 = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r1 = L14;
   
   SATTR(r1,fp,stdout);
   ret_val6 = r1;
   FILE_plus_STR(ret_val6, plus_s1);
   ret_val5 = plus_self1;
   plus_self2 = ret_val5;
   plus_s2 = ((STR) &name9);
   stdout_self2 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r2 = L17;
   
   SATTR(r2,fp,stdout);
   ret_val7 = r2;
   FILE_plus_STR(ret_val7, plus_s2);
  }
  ret_val = res;
  return ret_val;
 }
 else {
  CS_OPTIONS_usage(self);
  ret_val = ((STR) &name1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR CS_OPT619607024(CS_OPTIONS self, STR nm) {
 STR ret_val;
 INT pos = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 STR r;
 INT L4;
 INT L51_;
 INT L31_,L31_m;
 INT L6;
 CHAR L71_;
 CHAR L8;
 BOOL L91_;
 extern STR name1;
 extern STR name18;
 {
  BOOL f_L31_ = TRUE;
  L4 = STR_sizerINT(nm);
  L51_=INTMINUS(L4,1); 
  L11 = L51_;
  L21 = 0;
  L31_=L11+1;L31_m=L21; 
  while (1) {
   if(L31_--<=L31_m)  goto after_loop; 
   pos = L31_;
   L71_=ARR((STR)nm,pos); 
   L8 = L71_;
   L91_=L8=='/'; 
   if (L91_) {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 r = STR_head_INTrSTR(nm, pos);
 if (STR_is111530248(r, ((STR) &name1))) {
  r = ((STR) &name18);
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT1644060403(CS_OPTIONS self, FLISTSTR cl, STR filename) {
 ARRAYSTR tail = ((ARRAYSTR) NULL);
 ARRAYSTR tail_names = ((ARRAYSTR) NULL);
 ARRAYSTR L11;
 CS_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 ARRAYSTR subarr_self;
 INT subarr_beg = INT_zero;
 INT subarr_num = INT_zero;
 ARRAYSTR ret_val1;
 ARRAYSTR r;
 ARRAYSTR create_self;
 INT create_n = INT_zero;
 ARRAYSTR ret_val2;
 ARRAYSTR create_self1;
 INT create_n1 = INT_zero;
 ARRAYSTR ret_val3;
 ARRAYSTR create_self2;
 INT create_n2 = INT_zero;
 ARRAYSTR ret_val4;
 ARRAYSTR create_self3;
 INT create_n3 = INT_zero;
 ARRAYSTR ret_val5;
 INT L21 = INT_zero;
 INT L3;
 ARRAYSTR L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 ARRAYSTR L8;
 INT L91_;
 INT L10;
 INT L12;
 INT L131_;
 ARRAYSTR L14;
 INT L15;
 OB L16;
 INT L17;
 ARRAYSTR L19;
 INT L201_;
 INT L22;
 INT L23;
 INT L241_;
 ARRAYSTR L25;
 INT L26;
 OB L27;
 INT L28;
 ARRAYSTR L29;
 INT L30;
 OB L31;
 INT L32;
 ARRAYSTR L33;
 INT L34;
 OB L35;
 INT L36;
 ARRAYSTR L37;
 ARRAYSTR L38;
 INT L39;
 OB L40;
 INT L41;
 INT L421_br;
 INT L431_;
 more_args_self = self;
 L3 = ATTR(more_args_self,next);
 L4 = ATTR(more_args_self,args);
 L51_=(L4)==NULL?0:ASIZE((ARRAYSTR)L4); 
 L6 = L51_;
 L71_=(L3)<(L6); 
 ret_val = L71_;
 if (ret_val) {
  subarr_self = ATTR(self,args);
  subarr_beg = ATTR(self,next);
  L8 = ATTR(self,args);
  L91_=(L8)==NULL?0:ASIZE((ARRAYSTR)L8); 
  L10 = L91_;
  L12 = ATTR(self,next);
  L131_=INTMINUS(L10,L12); 
  subarr_num = L131_;
  L15 = subarr_num;
  L17=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L15)*sizeof(STR);
  L16=ZALLOC_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((ARRAYSTR) L16);
  L14->asize = L15;
  r = L14;
  AREFACOPYBNS(r,0,subarr_num,subarr_beg,subarr_self); 
  ;
  ret_val1 = r;
  tail = ret_val1;
  create_self = ((ARRAYSTR) NULL);
  L19 = ATTR(self,args);
  L201_=(L19)==NULL?0:ASIZE((ARRAYSTR)L19); 
  L22 = L201_;
  L23 = ATTR(self,next);
  L241_=INTMINUS(L22,L23); 
  create_n = L241_;
  L26 = create_n;
  L28=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L26)*sizeof(STR);
  L27=ZALLOC_BIG(L28);
  if (L27==NULL) FATAL("Unable to allocate more memory");
  ((OB)L27)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L27)->header.destroyed=0;
#endif

  L25 = ((ARRAYSTR) L27);
  L25->asize = L26;
  ret_val2 = L25;
  tail_names = ret_val2;
 }
 else {
  create_self1 = ((ARRAYSTR) NULL);
  create_n1 = 0;
  L30 = create_n1;
  L32=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L30)*sizeof(STR);
  L31=ZALLOC_BIG(L32);
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L31)->header.destroyed=0;
#endif

  L29 = ((ARRAYSTR) L31);
  L29->asize = L30;
  ret_val3 = L29;
  tail = ret_val3;
  create_self2 = ((ARRAYSTR) NULL);
  create_n2 = 0;
  L34 = create_n2;
  L36=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L34)*sizeof(STR);
  L35=ZALLOC_BIG(L36);
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

    ((OB)L35)->header.destroyed=0;
#endif

  L33 = ((ARRAYSTR) L35);
  L33->asize = L34;
  ret_val4 = L33;
  tail_names = ret_val4;
 }
 L37 = FLISTS533235694(cl);
 SATTR(self,args,ARRAYS417083844(L37, tail));
 create_self3 = ((ARRAYSTR) NULL);
 create_n3 = FLISTS326330076(cl);
 L39 = create_n3;
 L41=(sizeof(struct ARRAYSTR_struct)-sizeof(STR))+(L39)*sizeof(STR);
 L40=ZALLOC_BIG(L41);
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=ARRAYSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L40)->header.destroyed=0;
#endif

 L38 = ((ARRAYSTR) L40);
 L38->asize = L39;
 ret_val5 = L38;
 SATTR(self,arg_locations,ret_val5);
 {
  BOOL f_L421_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = ATTR(self,arg_locations);
  L421_br=L11==NULL?0:ASIZE((ARRAYSTR)L11); 
  while (1) {
   if(L21>=L421_br)  goto after_loop; 
   SARR((ARRAYSTR)L11,L21,(STR)filename); 
   ;
   L431_=INTPLUS(L21,1); 
   L21 = L431_;
  }
 }
 after_loop: ;
 SATTR(self,arg_locations,ARRAYS417083844(ATTR(self,arg_locations), tail_names));
 SATTR(self,next,0);
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT194850632(CS_OPTIONS self) {
 STR arg;
 FSETSTR create_self;
 FSETSTR ret_val;
 CS_OPTIONS more_args_self;
 BOOL ret_val1 = BOOL_zero;
 CS_OPTIONS rewind_self;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 extern STR all1;
 INT L6;
 INT L71_;
 create_self = ((FSETSTR) NULL);
 ret_val = ((FSETSTR) NULL);
 SATTR(self,classes1,ret_val);
 while (1) {
  more_args_self = self;
  L1 = ATTR(more_args_self,next);
  L2 = ATTR(more_args_self,args);
  L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
  L4 = L31_;
  L51_=(L1)<(L4); 
  ret_val1 = L51_;
  if (ret_val1) {
  }
  else {
   goto after_loop;
  }
  arg = CS_OPT1970940548(self);
  SATTR(self,all1,STR_is111530248(arg, ((STR) &all1)));
  if (ATTR(self,all1)) {
   return;
  }
  else {
   if (CS_OPT1443087191(self, arg)) {
    SATTR(self,classes1,FSETST1404644833(ATTR(self,classes1), arg));
   }
   else {
    rewind_self = self;
    L6 = ATTR(rewind_self,next);
    L71_=INTMINUS(L6,1); 
    SATTR(rewind_self,next,L71_);
    return;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT287833045(CS_OPTIONS self, STR arg, STR suf) {
 STR L11;
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
 ERR1 ret_val3;
 FILE1 stderr_self1;
 FILE1 ret_val4;
 FILE1 r1;
 ERR1 plus_self2;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR c1;
 extern STR o;
 extern STR obj;
 extern STR a1;
 extern STR lib1;
 extern STR Didntu1702411602;
 FILE1 L2;
 OB L3;
 FILE1 L5;
 OB L6;
 dSTR L8;
 OB L9;
 FILE1 L10;
 OB L12;
 L11 = suf;
 if (STR_is111530248(L11, ((STR) &c1))) {
  SATTR(ATTR(self,cgen),c_files,FSETST1404644833(ATTR(ATTR(self,cgen),c_files), arg));
 }
 else {
  if (STR_is111530248(L11, ((STR) &o))) {
   SATTR(ATTR(self,cgen),object_files,FSETST1404644833(ATTR(ATTR(self,cgen),object_files), arg));
  }
  else {
   if (STR_is111530248(L11, ((STR) &obj))) {
    SATTR(ATTR(self,cgen),object_files,FSETST1404644833(ATTR(ATTR(self,cgen),object_files), arg));
   }
   else {
    if (STR_is111530248(L11, ((STR) &a1))) {
     SATTR(ATTR(self,cgen),archive_files,FLISTS1425610062(ATTR(ATTR(self,cgen),archive_files), arg));
    }
    else {
     if (STR_is111530248(L11, ((STR) &lib1))) {
      SATTR(ATTR(self,cgen),archive_files,FLISTS1425610062(ATTR(ATTR(self,cgen),archive_files), arg));
     }
     else {
      create_self = ((ERR1) NULL);
      ret_val = create_self;
      plus_self = ret_val;
      plus_s = ((dSTR) ((STR) &Didntu1702411602));
      stderr_self = ((FILE1) NULL);
      L3=ZALLOC(sizeof(struct FILE1_struct));
      if (L3==NULL) FATAL("Unable to allocate more memory");
      ((OB)L3)->header.tag=FILE1_tag;
      L2 = ((FILE1) L3);
      r = L2;
      
      SATTR(r,fp,stderr);
      ret_val2 = r;
      FILE_plus_dSTR(ret_val2, plus_s);
      ret_val1 = plus_self;
      plus_self1 = ret_val1;
      plus_s1 = ((dSTR) arg);
      stderr_self1 = ((FILE1) NULL);
      L6=ZALLOC(sizeof(struct FILE1_struct));
      if (L6==NULL) FATAL("Unable to allocate more memory");
      ((OB)L6)->header.tag=FILE1_tag;
      L5 = ((FILE1) L6);
      r1 = L5;
      
      SATTR(r1,fp,stderr);
      ret_val4 = r1;
      FILE_plus_dSTR(ret_val4, plus_s1);
      ret_val3 = plus_self1;
      plus_self2 = ret_val3;
      L9=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
      if (L9==NULL) FATAL("Unable to allocate more memory");
      memset(L9,0,sizeof(struct CHAR_boxed_struct));
      ((OB)L9)->header.tag=CHAR_tag;
      L8 = (dSTR)((CHAR_boxed) L9);
      ((CHAR_boxed) L8)->immutable_part = '\n';
      plus_s2 = L8;
      stderr_self2 = ((FILE1) NULL);
      L12=ZALLOC(sizeof(struct FILE1_struct));
      if (L12==NULL) FATAL("Unable to allocate more memory");
      ((OB)L12)->header.tag=FILE1_tag;
      L10 = ((FILE1) L12);
      r2 = L10;
      
      SATTR(r2,fp,stderr);
      ret_val5 = r2;
      FILE_plus_dSTR(ret_val5, plus_s2);
      CS_OPTIONS_usage(self);
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT390587743(CS_OPTIONS self, STR s) {
 CS_OPTIONS rewind_self;
 CS_OPTIONS rewind_self1;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 if (CS_OPT1373530461(self, s)) {
  rewind_self = self;
  L1 = ATTR(rewind_self,next);
  L21_=INTMINUS(L1,1); 
  SATTR(rewind_self,next,L21_);
  CS_OPT611018296(self);
 }
 else {
  rewind_self1 = self;
  L3 = ATTR(rewind_self1,next);
  L41_=INTMINUS(L3,1); 
  SATTR(rewind_self1,next,L41_);
  CS_OPTIONS_usage(self);
  return;
 }
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT479416027(CS_OPTIONS self, STR s) {
 STR L11;
 STR cfs;
 STR platform_module;
 BFILE f;
 FSETSTR create_self;
 FSETSTR ret_val;
 FSETSTR create_self1;
 FSETSTR ret_val1;
 FSETSTR create_self2;
 FSETSTR ret_val2;
 FSETSTR create_self3;
 FSETSTR ret_val3;
 FSETSTR create_self4;
 FSETSTR ret_val4;
 FSETSTR create_self5;
 FSETSTR ret_val5;
 FSETSTR create_self6;
 FSETSTR ret_val6;
 FSETSTR create_self7;
 FSETSTR ret_val7;
 FSETSTR create_self8;
 FSETSTR ret_val8;
 FSETSTR union_self;
 FSETSTR union_s;
 FSETSTR ret_val9;
 FSETSTR union_self1;
 FSETSTR union_s1;
 FSETSTR ret_val10;
 FSETSTR union_self2;
 FSETSTR union_s2;
 FSETSTR ret_val11;
 FSETSTR union_self3;
 FSETSTR union_s3;
 FSETSTR ret_val12;
 FSETSTR union_self4;
 FSETSTR union_s4;
 FSETSTR ret_val13;
 FSETSTR union_self5;
 FSETSTR union_s5;
 FSETSTR ret_val14;
 FSETSTR union_self6;
 FSETSTR union_s6;
 FSETSTR ret_val15;
 FSETSTR union_self7;
 FSETSTR union_s7;
 FSETSTR ret_val16;
 FSETSTR union_self8;
 FSETSTR union_s8;
 FSETSTR ret_val17;
 FSETSTR create_self9;
 FSETSTR ret_val18;
 FSETSTR create_self10;
 FSETSTR ret_val19;
 FSETSTR create_self11;
 FSETSTR ret_val20;
 FSETSTR create_self12;
 FSETSTR ret_val21;
 FSETSTR create_self13;
 FSETSTR ret_val22;
 FSETSTR create_self14;
 FSETSTR ret_val23;
 FSETSTR create_self15;
 FSETSTR ret_val24;
 FSETSTR create_self16;
 FSETSTR ret_val25;
 FSETSTR create_self17;
 FSETSTR ret_val26;
 FSETSTR union_self9;
 FSETSTR union_s9;
 FSETSTR ret_val27;
 FSETSTR union_self10;
 FSETSTR union_s10;
 FSETSTR ret_val28;
 FSETSTR union_self11;
 FSETSTR union_s11;
 FSETSTR ret_val29;
 FSETSTR union_self12;
 FSETSTR union_s12;
 FSETSTR ret_val30;
 FSETSTR union_self13;
 FSETSTR union_s13;
 FSETSTR ret_val31;
 FSETSTR union_self14;
 FSETSTR union_s14;
 FSETSTR ret_val32;
 FSETSTR union_self15;
 FSETSTR union_s15;
 FSETSTR ret_val33;
 FSETSTR union_self16;
 FSETSTR union_s16;
 FSETSTR ret_val34;
 FSETSTR union_self17;
 FSETSTR union_s17;
 FSETSTR ret_val35;
 FSETSTR create_self18;
 FSETSTR ret_val36;
 FSETSTR union_self18;
 FSETSTR union_s18;
 FSETSTR ret_val37;
 FSETSTR create_self19;
 FSETSTR ret_val38;
 FSETSTR union_self19;
 FSETSTR union_s19;
 FSETSTR ret_val39;
 FSETSTR create_self20;
 FSETSTR ret_val40;
 FSETSTR union_self20;
 FSETSTR union_s20;
 FSETSTR ret_val41;
 FSETSTR create_self21;
 FSETSTR ret_val42;
 FSETSTR union_self21;
 FSETSTR union_s21;
 FSETSTR ret_val43;
 FSETSTR create_self22;
 FSETSTR ret_val44;
 FSETSTR union_self22;
 FSETSTR union_s22;
 FSETSTR ret_val45;
 FSETSTR create_self23;
 FSETSTR ret_val46;
 FSETSTR union_self23;
 FSETSTR union_s23;
 FSETSTR ret_val47;
 FSETSTR create_self24;
 FSETSTR ret_val48;
 FSETSTR union_self24;
 FSETSTR union_s24;
 FSETSTR ret_val49;
 FSETSTR create_self25;
 FSETSTR ret_val50;
 FSETSTR union_self25;
 FSETSTR union_s25;
 FSETSTR ret_val51;
 FSETSTR create_self26;
 FSETSTR ret_val52;
 FSETSTR union_self26;
 FSETSTR union_s26;
 FSETSTR ret_val53;
 FSETSTR create_self27;
 FSETSTR ret_val54;
 FSETSTR create_self28;
 FSETSTR ret_val55;
 FSETSTR create_self29;
 FSETSTR ret_val56;
 FSETSTR create_self30;
 FSETSTR ret_val57;
 FSETSTR create_self31;
 FSETSTR ret_val58;
 FSETSTR create_self32;
 FSETSTR ret_val59;
 FSETSTR create_self33;
 FSETSTR ret_val60;
 FSETSTR create_self34;
 FSETSTR ret_val61;
 FSETSTR create_self35;
 FSETSTR ret_val62;
 FSETSTR union_self27;
 FSETSTR union_s27;
 FSETSTR ret_val63;
 FSETSTR union_self28;
 FSETSTR union_s28;
 FSETSTR ret_val64;
 FSETSTR union_self29;
 FSETSTR union_s29;
 FSETSTR ret_val65;
 FSETSTR union_self30;
 FSETSTR union_s30;
 FSETSTR ret_val66;
 FSETSTR union_self31;
 FSETSTR union_s31;
 FSETSTR ret_val67;
 FSETSTR union_self32;
 FSETSTR union_s32;
 FSETSTR ret_val68;
 FSETSTR union_self33;
 FSETSTR union_s33;
 FSETSTR ret_val69;
 FSETSTR union_self34;
 FSETSTR union_s34;
 FSETSTR ret_val70;
 FSETSTR union_self35;
 FSETSTR union_s35;
 FSETSTR ret_val71;
 FSETSTR create_self36;
 FSETSTR ret_val72;
 FSETSTR create_self37;
 FSETSTR ret_val73;
 FSETSTR create_self38;
 FSETSTR ret_val74;
 FSETSTR create_self39;
 FSETSTR ret_val75;
 FSETSTR create_self40;
 FSETSTR ret_val76;
 FSETSTR create_self41;
 FSETSTR ret_val77;
 FSETSTR create_self42;
 FSETSTR ret_val78;
 FSETSTR create_self43;
 FSETSTR ret_val79;
 FSETSTR create_self44;
 FSETSTR ret_val80;
 FSETSTR union_self36;
 FSETSTR union_s36;
 FSETSTR ret_val81;
 FSETSTR union_self37;
 FSETSTR union_s37;
 FSETSTR ret_val82;
 FSETSTR union_self38;
 FSETSTR union_s38;
 FSETSTR ret_val83;
 FSETSTR union_self39;
 FSETSTR union_s39;
 FSETSTR ret_val84;
 FSETSTR union_self40;
 FSETSTR union_s40;
 FSETSTR ret_val85;
 FSETSTR union_self41;
 FSETSTR union_s41;
 FSETSTR ret_val86;
 FSETSTR union_self42;
 FSETSTR union_s42;
 FSETSTR ret_val87;
 FSETSTR union_self43;
 FSETSTR union_s43;
 FSETSTR ret_val88;
 FSETSTR union_self44;
 FSETSTR union_s44;
 FSETSTR ret_val89;
 FSETSTR create_self45;
 FSETSTR ret_val90;
 FSETSTR union_self45;
 FSETSTR union_s45;
 FSETSTR ret_val91;
 FSETSTR create_self46;
 FSETSTR ret_val92;
 FSETSTR union_self46;
 FSETSTR union_s46;
 FSETSTR ret_val93;
 FSETSTR create_self47;
 FSETSTR ret_val94;
 FSETSTR union_self47;
 FSETSTR union_s47;
 FSETSTR ret_val95;
 FSETSTR create_self48;
 FSETSTR ret_val96;
 FSETSTR union_self48;
 FSETSTR union_s48;
 FSETSTR ret_val97;
 FSETSTR create_self49;
 FSETSTR ret_val98;
 FSETSTR union_self49;
 FSETSTR union_s49;
 FSETSTR ret_val99;
 FSETSTR create_self50;
 FSETSTR ret_val100;
 FSETSTR union_self50;
 FSETSTR union_s50;
 FSETSTR ret_val101;
 FSETSTR create_self51;
 FSETSTR ret_val102;
 FSETSTR union_self51;
 FSETSTR union_s51;
 FSETSTR ret_val103;
 FSETSTR create_self52;
 FSETSTR ret_val104;
 FSETSTR union_self52;
 FSETSTR union_s52;
 FSETSTR ret_val105;
 FSETSTR create_self53;
 FSETSTR ret_val106;
 FSETSTR union_self53;
 FSETSTR union_s53;
 FSETSTR ret_val107;
 STR plus_self;
 STR plus_sarg;
 STR ret_val108;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val109;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val110;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val1111;
 BFILE r;
 BFILE error_self;
 BOOL ret_val112 = BOOL_zero;
 ERR1 create_self54;
 ERR1 ret_val113;
 ERR1 plus_self3;
 dSTR plus_s;
 ERR1 ret_val114;
 FILE1 stderr_self;
 FILE1 ret_val115;
 FILE1 r1;
 ERR1 plus_self4;
 dSTR plus_s1;
 ERR1 ret_val116;
 FILE1 stderr_self1;
 FILE1 ret_val117;
 FILE1 r2;
 ERR1 plus_self5;
 dSTR plus_s2;
 ERR1 ret_val118;
 FILE1 stderr_self2;
 FILE1 ret_val119;
 FILE1 r3;
 ERR1 plus_self6;
 dSTR plus_s3;
 ERR1 ret_val120;
 FILE1 stderr_self3;
 FILE1 ret_val121;
 FILE1 r4;
 ERR1 plus_self7;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val122;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 STR plus_self8;
 STR plus_sarg3;
 STR ret_val123;
 extern STR chk_all;
 extern STR chk;
 FSETSTR L2;
 FSETSTR L3;
 FSETSTR L4;
 FSETSTR L5;
 FSETSTR L6;
 FSETSTR L7;
 FSETSTR L8;
 FSETSTR L9;
 FSETSTR L10;
 extern STR chk;
 extern STR chk;
 FSETSTR L12;
 FSETSTR L13;
 FSETSTR L14;
 FSETSTR L15;
 FSETSTR L16;
 FSETSTR L17;
 FSETSTR L18;
 FSETSTR L19;
 FSETSTR L20;
 extern STR chk_arith;
 FSETSTR L21;
 extern STR chk_bounds;
 FSETSTR L22;
 extern STR chk_void;
 FSETSTR L23;
 extern STR chk_pre;
 FSETSTR L24;
 extern STR chk_post;
 FSETSTR L25;
 extern STR chk_assert;
 FSETSTR L26;
 extern STR chk_invariant;
 FSETSTR L27;
 extern STR chk_when;
 FSETSTR L28;
 extern STR chk_destroy;
 extern STR chk_psather;
 extern STR stats;
 extern STR psather_stats;
 extern STR psather_trace;
 extern STR show_am;
 extern STR return1;
 FSETSTR L29;
 extern STR chk_no_all;
 extern STR chk_no;
 FSETSTR L30;
 FSETSTR L31;
 FSETSTR L32;
 FSETSTR L33;
 FSETSTR L34;
 FSETSTR L35;
 FSETSTR L36;
 FSETSTR L37;
 FSETSTR L38;
 extern STR chk_no;
 extern STR chk_no;
 FSETSTR L39;
 FSETSTR L40;
 FSETSTR L41;
 FSETSTR L42;
 FSETSTR L43;
 FSETSTR L44;
 FSETSTR L45;
 FSETSTR L46;
 FSETSTR L47;
 extern STR chk_no_arith;
 FSETSTR L48;
 extern STR chk_no_bounds;
 FSETSTR L49;
 extern STR chk_no_void;
 FSETSTR L50;
 extern STR chk_no_pre;
 FSETSTR L51;
 extern STR chk_no_post;
 FSETSTR L52;
 extern STR chk_no_assert;
 FSETSTR L53;
 extern STR chk_no_invariant;
 FSETSTR L54;
 extern STR chk_no_destroy;
 extern STR chk_no_psather;
 extern STR no_stats;
 extern STR no_psather_stats;
 extern STR no_psather_trace;
 extern STR chk_no_return;
 FSETSTR L55;
 extern STR chk_no_when;
 FSETSTR L56;
 extern STR not_a_42245793;
 INT L57;
 INT L581_;
 CHAR L591_;
 CHAR L60;
 BOOL L611_;
 extern STR SystemPlatforms;
 INT L62;
 INT L631_;
 extern STR Platformmodule;
 BFILE L64;
 OB L65;
 extern STR rb;
 STR L66;
 STR L67;
 BOOL L68;
 extern STR Unreco482634771;
 FILE1 L69;
 OB L70;
 FILE1 L72;
 OB L73;
 extern STR orplatform;
 FILE1 L75;
 OB L76;
 INT L78;
 INT L791_;
 FILE1 L80;
 OB L81;
 extern STR missing1;
 FILE1 L83;
 OB L84;
 INT L86;
 INT L871_;
 FLISTSTR L88;
 extern STR Platform;
 L11 = s;
 if (STR_is111530248(L11, ((STR) &chk_all))) {
  if (STR_is111530248(s, ((STR) &chk))) {
   SATTR(self,all1,TRUE);
  }
  else {
   CS_OPT194850632(self);
  }
  SATTR(ATTR(self,prog),arith_checks,TRUE);
  SATTR(ATTR(self,prog),pre_checks,TRUE);
  SATTR(ATTR(self,prog),post_checks,TRUE);
  SATTR(ATTR(self,prog),assert_checks,TRUE);
  SATTR(ATTR(self,prog),void_checks,TRUE);
  if (ATTR(self,all1)) {
   SATTR(self,arith_all,TRUE);
   create_self = ((FSETSTR) NULL);
   ret_val = ((FSETSTR) NULL);
   SATTR(self,arith_out,ret_val);
   SATTR(self,bounds_all,TRUE);
   create_self1 = ((FSETSTR) NULL);
   ret_val1 = ((FSETSTR) NULL);
   SATTR(self,bounds_out,ret_val1);
   SATTR(self,void_all,TRUE);
   create_self2 = ((FSETSTR) NULL);
   ret_val2 = ((FSETSTR) NULL);
   SATTR(self,void_out,ret_val2);
   SATTR(self,pre_all,TRUE);
   create_self3 = ((FSETSTR) NULL);
   ret_val3 = ((FSETSTR) NULL);
   SATTR(self,pre_out,ret_val3);
   SATTR(self,post_all,TRUE);
   create_self4 = ((FSETSTR) NULL);
   ret_val4 = ((FSETSTR) NULL);
   SATTR(self,post_out,ret_val4);
   SATTR(self,assert_all,TRUE);
   create_self5 = ((FSETSTR) NULL);
   ret_val5 = ((FSETSTR) NULL);
   SATTR(self,assert_out,ret_val5);
   SATTR(self,invariant_all,TRUE);
   create_self6 = ((FSETSTR) NULL);
   ret_val6 = ((FSETSTR) NULL);
   SATTR(self,invariant_out,ret_val6);
   SATTR(self,when_all,TRUE);
   create_self7 = ((FSETSTR) NULL);
   ret_val7 = ((FSETSTR) NULL);
   SATTR(self,when_out,ret_val7);
   SATTR(self,return_all,TRUE);
   create_self8 = ((FSETSTR) NULL);
   ret_val8 = ((FSETSTR) NULL);
   SATTR(self,return_out,ret_val8);
   SATTR(self,destroy_chk,TRUE);
   SATTR(self,psather_chk,TRUE);
  }
  else {
   union_self = ATTR(self,arith_in);
   union_s = ATTR(self,classes1);
   L2 = FSETST1884046860(union_self);
   ret_val9 = FSETST866022026(L2, union_s);
   SATTR(self,arith_in,ret_val9);
   union_self1 = ATTR(self,bounds_in);
   union_s1 = ATTR(self,classes1);
   L3 = FSETST1884046860(union_self1);
   ret_val10 = FSETST866022026(L3, union_s1);
   SATTR(self,bounds_in,ret_val10);
   union_self2 = ATTR(self,void_in);
   union_s2 = ATTR(self,classes1);
   L4 = FSETST1884046860(union_self2);
   ret_val11 = FSETST866022026(L4, union_s2);
   SATTR(self,void_in,ret_val11);
   union_self3 = ATTR(self,pre_in);
   union_s3 = ATTR(self,classes1);
   L5 = FSETST1884046860(union_self3);
   ret_val12 = FSETST866022026(L5, union_s3);
   SATTR(self,pre_in,ret_val12);
   union_self4 = ATTR(self,post_in);
   union_s4 = ATTR(self,classes1);
   L6 = FSETST1884046860(union_self4);
   ret_val13 = FSETST866022026(L6, union_s4);
   SATTR(self,post_in,ret_val13);
   union_self5 = ATTR(self,assert_in);
   union_s5 = ATTR(self,classes1);
   L7 = FSETST1884046860(union_self5);
   ret_val14 = FSETST866022026(L7, union_s5);
   SATTR(self,assert_in,ret_val14);
   union_self6 = ATTR(self,invariant_in);
   union_s6 = ATTR(self,classes1);
   L8 = FSETST1884046860(union_self6);
   ret_val15 = FSETST866022026(L8, union_s6);
   SATTR(self,invariant_in,ret_val15);
   union_self7 = ATTR(self,when_in);
   union_s7 = ATTR(self,classes1);
   L9 = FSETST1884046860(union_self7);
   ret_val16 = FSETST866022026(L9, union_s7);
   SATTR(self,when_in,ret_val16);
   union_self8 = ATTR(self,return_in);
   union_s8 = ATTR(self,classes1);
   L10 = FSETST1884046860(union_self8);
   ret_val17 = FSETST866022026(L10, union_s8);
   SATTR(self,return_in,ret_val17);
   SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
   SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
   SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
   SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
   SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
   SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
   SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
   SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
   SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
  }
 }
 else {
  if (STR_is111530248(L11, ((STR) &chk))) {
   if (STR_is111530248(s, ((STR) &chk))) {
    SATTR(self,all1,TRUE);
   }
   else {
    CS_OPT194850632(self);
   }
   SATTR(ATTR(self,prog),arith_checks,TRUE);
   SATTR(ATTR(self,prog),pre_checks,TRUE);
   SATTR(ATTR(self,prog),post_checks,TRUE);
   SATTR(ATTR(self,prog),assert_checks,TRUE);
   SATTR(ATTR(self,prog),void_checks,TRUE);
   if (ATTR(self,all1)) {
    SATTR(self,arith_all,TRUE);
    create_self9 = ((FSETSTR) NULL);
    ret_val18 = ((FSETSTR) NULL);
    SATTR(self,arith_out,ret_val18);
    SATTR(self,bounds_all,TRUE);
    create_self10 = ((FSETSTR) NULL);
    ret_val19 = ((FSETSTR) NULL);
    SATTR(self,bounds_out,ret_val19);
    SATTR(self,void_all,TRUE);
    create_self11 = ((FSETSTR) NULL);
    ret_val20 = ((FSETSTR) NULL);
    SATTR(self,void_out,ret_val20);
    SATTR(self,pre_all,TRUE);
    create_self12 = ((FSETSTR) NULL);
    ret_val21 = ((FSETSTR) NULL);
    SATTR(self,pre_out,ret_val21);
    SATTR(self,post_all,TRUE);
    create_self13 = ((FSETSTR) NULL);
    ret_val22 = ((FSETSTR) NULL);
    SATTR(self,post_out,ret_val22);
    SATTR(self,assert_all,TRUE);
    create_self14 = ((FSETSTR) NULL);
    ret_val23 = ((FSETSTR) NULL);
    SATTR(self,assert_out,ret_val23);
    SATTR(self,invariant_all,TRUE);
    create_self15 = ((FSETSTR) NULL);
    ret_val24 = ((FSETSTR) NULL);
    SATTR(self,invariant_out,ret_val24);
    SATTR(self,when_all,TRUE);
    create_self16 = ((FSETSTR) NULL);
    ret_val25 = ((FSETSTR) NULL);
    SATTR(self,when_out,ret_val25);
    SATTR(self,return_all,TRUE);
    create_self17 = ((FSETSTR) NULL);
    ret_val26 = ((FSETSTR) NULL);
    SATTR(self,return_out,ret_val26);
    SATTR(self,destroy_chk,TRUE);
    SATTR(self,psather_chk,TRUE);
   }
   else {
    union_self9 = ATTR(self,arith_in);
    union_s9 = ATTR(self,classes1);
    L12 = FSETST1884046860(union_self9);
    ret_val27 = FSETST866022026(L12, union_s9);
    SATTR(self,arith_in,ret_val27);
    union_self10 = ATTR(self,bounds_in);
    union_s10 = ATTR(self,classes1);
    L13 = FSETST1884046860(union_self10);
    ret_val28 = FSETST866022026(L13, union_s10);
    SATTR(self,bounds_in,ret_val28);
    union_self11 = ATTR(self,void_in);
    union_s11 = ATTR(self,classes1);
    L14 = FSETST1884046860(union_self11);
    ret_val29 = FSETST866022026(L14, union_s11);
    SATTR(self,void_in,ret_val29);
    union_self12 = ATTR(self,pre_in);
    union_s12 = ATTR(self,classes1);
    L15 = FSETST1884046860(union_self12);
    ret_val30 = FSETST866022026(L15, union_s12);
    SATTR(self,pre_in,ret_val30);
    union_self13 = ATTR(self,post_in);
    union_s13 = ATTR(self,classes1);
    L16 = FSETST1884046860(union_self13);
    ret_val31 = FSETST866022026(L16, union_s13);
    SATTR(self,post_in,ret_val31);
    union_self14 = ATTR(self,assert_in);
    union_s14 = ATTR(self,classes1);
    L17 = FSETST1884046860(union_self14);
    ret_val32 = FSETST866022026(L17, union_s14);
    SATTR(self,assert_in,ret_val32);
    union_self15 = ATTR(self,invariant_in);
    union_s15 = ATTR(self,classes1);
    L18 = FSETST1884046860(union_self15);
    ret_val33 = FSETST866022026(L18, union_s15);
    SATTR(self,invariant_in,ret_val33);
    union_self16 = ATTR(self,when_in);
    union_s16 = ATTR(self,classes1);
    L19 = FSETST1884046860(union_self16);
    ret_val34 = FSETST866022026(L19, union_s16);
    SATTR(self,when_in,ret_val34);
    union_self17 = ATTR(self,return_in);
    union_s17 = ATTR(self,classes1);
    L20 = FSETST1884046860(union_self17);
    ret_val35 = FSETST866022026(L20, union_s17);
    SATTR(self,return_in,ret_val35);
    SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
    SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
    SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
    SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
    SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
    SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
    SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
    SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
    SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
   }
  }
  else {
   if (STR_is111530248(L11, ((STR) &chk_arith))) {
    CS_OPT194850632(self);
    SATTR(ATTR(self,prog),arith_checks,TRUE);
    if (ATTR(self,all1)) {
     SATTR(self,arith_all,TRUE);
     create_self18 = ((FSETSTR) NULL);
     ret_val36 = ((FSETSTR) NULL);
     SATTR(self,arith_out,ret_val36);
    }
    else {
     union_self18 = ATTR(self,arith_in);
     union_s18 = ATTR(self,classes1);
     L21 = FSETST1884046860(union_self18);
     ret_val37 = FSETST866022026(L21, union_s18);
     SATTR(self,arith_in,ret_val37);
     SATTR(self,arith_out,FSETST897681893(ATTR(self,arith_out), ATTR(self,classes1)));
    }
   }
   else {
    if (STR_is111530248(L11, ((STR) &chk_bounds))) {
     CS_OPT194850632(self);
     if (ATTR(self,all1)) {
      SATTR(self,bounds_all,TRUE);
      create_self19 = ((FSETSTR) NULL);
      ret_val38 = ((FSETSTR) NULL);
      SATTR(self,bounds_out,ret_val38);
     }
     else {
      union_self19 = ATTR(self,bounds_in);
      union_s19 = ATTR(self,classes1);
      L22 = FSETST1884046860(union_self19);
      ret_val39 = FSETST866022026(L22, union_s19);
      SATTR(self,bounds_in,ret_val39);
      SATTR(self,bounds_out,FSETST897681893(ATTR(self,bounds_out), ATTR(self,classes1)));
     }
    }
    else {
     if (STR_is111530248(L11, ((STR) &chk_void))) {
      CS_OPT194850632(self);
      SATTR(ATTR(self,prog),void_checks,TRUE);
      if (ATTR(self,all1)) {
       SATTR(self,void_all,TRUE);
       create_self20 = ((FSETSTR) NULL);
       ret_val40 = ((FSETSTR) NULL);
       SATTR(self,void_out,ret_val40);
      }
      else {
       union_self20 = ATTR(self,void_in);
       union_s20 = ATTR(self,classes1);
       L23 = FSETST1884046860(union_self20);
       ret_val41 = FSETST866022026(L23, union_s20);
       SATTR(self,void_in,ret_val41);
       SATTR(self,void_out,FSETST897681893(ATTR(self,void_out), ATTR(self,classes1)));
      }
     }
     else {
      if (STR_is111530248(L11, ((STR) &chk_pre))) {
       SATTR(ATTR(self,prog),pre_checks,TRUE);
       CS_OPT194850632(self);
       if (ATTR(self,all1)) {
        SATTR(self,pre_all,TRUE);
        create_self21 = ((FSETSTR) NULL);
        ret_val42 = ((FSETSTR) NULL);
        SATTR(self,pre_out,ret_val42);
       }
       else {
        union_self21 = ATTR(self,pre_in);
        union_s21 = ATTR(self,classes1);
        L24 = FSETST1884046860(union_self21);
        ret_val43 = FSETST866022026(L24, union_s21);
        SATTR(self,pre_in,ret_val43);
        SATTR(self,pre_out,FSETST897681893(ATTR(self,pre_out), ATTR(self,classes1)));
       }
      }
      else {
       if (STR_is111530248(L11, ((STR) &chk_post))) {
        SATTR(ATTR(self,prog),post_checks,TRUE);
        CS_OPT194850632(self);
        if (ATTR(self,all1)) {
         SATTR(self,post_all,TRUE);
         create_self22 = ((FSETSTR) NULL);
         ret_val44 = ((FSETSTR) NULL);
         SATTR(self,post_out,ret_val44);
        }
        else {
         union_self22 = ATTR(self,post_in);
         union_s22 = ATTR(self,classes1);
         L25 = FSETST1884046860(union_self22);
         ret_val45 = FSETST866022026(L25, union_s22);
         SATTR(self,post_in,ret_val45);
         SATTR(self,post_out,FSETST897681893(ATTR(self,post_out), ATTR(self,classes1)));
        }
       }
       else {
        if (STR_is111530248(L11, ((STR) &chk_assert))) {
         SATTR(ATTR(self,prog),assert_checks,TRUE);
         CS_OPT194850632(self);
         if (ATTR(self,all1)) {
          SATTR(self,assert_all,TRUE);
          create_self23 = ((FSETSTR) NULL);
          ret_val46 = ((FSETSTR) NULL);
          SATTR(self,assert_out,ret_val46);
         }
         else {
          union_self23 = ATTR(self,assert_in);
          union_s23 = ATTR(self,classes1);
          L26 = FSETST1884046860(union_self23);
          ret_val47 = FSETST866022026(L26, union_s23);
          SATTR(self,assert_in,ret_val47);
          SATTR(self,assert_out,FSETST897681893(ATTR(self,assert_out), ATTR(self,classes1)));
         }
        }
        else {
         if (STR_is111530248(L11, ((STR) &chk_invariant))) {
          CS_OPT194850632(self);
          if (ATTR(self,all1)) {
           SATTR(self,invariant_all,TRUE);
           create_self24 = ((FSETSTR) NULL);
           ret_val48 = ((FSETSTR) NULL);
           SATTR(self,invariant_out,ret_val48);
          }
          else {
           union_self24 = ATTR(self,invariant_in);
           union_s24 = ATTR(self,classes1);
           L27 = FSETST1884046860(union_self24);
           ret_val49 = FSETST866022026(L27, union_s24);
           SATTR(self,invariant_in,ret_val49);
           SATTR(self,invariant_out,FSETST897681893(ATTR(self,invariant_out), ATTR(self,classes1)));
          }
         }
         else {
          if (STR_is111530248(L11, ((STR) &chk_when))) {
           CS_OPT194850632(self);
           if (ATTR(self,all1)) {
            SATTR(self,when_all,TRUE);
            create_self25 = ((FSETSTR) NULL);
            ret_val50 = ((FSETSTR) NULL);
            SATTR(self,when_out,ret_val50);
           }
           else {
            union_self25 = ATTR(self,when_in);
            union_s25 = ATTR(self,classes1);
            L28 = FSETST1884046860(union_self25);
            ret_val51 = FSETST866022026(L28, union_s25);
            SATTR(self,when_in,ret_val51);
            SATTR(self,when_out,FSETST897681893(ATTR(self,when_out), ATTR(self,classes1)));
           }
          }
          else {
           if (STR_is111530248(L11, ((STR) &chk_destroy))) {
            SATTR(self,destroy_chk,TRUE);
           }
           else {
            if (STR_is111530248(L11, ((STR) &chk_psather))) {
             SATTR(self,psather_chk,TRUE);
            }
            else {
             if (STR_is111530248(L11, ((STR) &stats))) {
              SATTR(self,stats,TRUE);
             }
             else {
              if (STR_is111530248(L11, ((STR) &psather_stats))) {
               SATTR(self,psather_stats,TRUE);
              }
              else {
               if (STR_is111530248(L11, ((STR) &psather_trace))) {
                SATTR(self,psather_trace,TRUE);
               }
               else {
                if (STR_is111530248(L11, ((STR) &show_am))) {
                 SATTR(ATTR(self,prog),show_am,TRUE);
                }
                else {
                 if (STR_is111530248(L11, ((STR) &return1))) {
                  CS_OPT194850632(self);
                  if (ATTR(self,all1)) {
                   SATTR(self,return_all,TRUE);
                   create_self26 = ((FSETSTR) NULL);
                   ret_val52 = ((FSETSTR) NULL);
                   SATTR(self,return_out,ret_val52);
                  }
                  else {
                   union_self26 = ATTR(self,return_in);
                   union_s26 = ATTR(self,classes1);
                   L29 = FSETST1884046860(union_self26);
                   ret_val53 = FSETST866022026(L29, union_s26);
                   SATTR(self,return_in,ret_val53);
                   SATTR(self,return_out,FSETST897681893(ATTR(self,return_out), ATTR(self,classes1)));
                  }
                 }
                 else {
                  if (STR_is111530248(L11, ((STR) &chk_no_all))) {
                   if (STR_is111530248(s, ((STR) &chk_no))) {
                    SATTR(self,all1,TRUE);
                   }
                   else {
                    CS_OPT194850632(self);
                   }
                   if (ATTR(self,all1)) {
                    SATTR(ATTR(self,prog),void_checks,FALSE);
                    SATTR(ATTR(self,prog),pre_checks,FALSE);
                    SATTR(ATTR(self,prog),post_checks,FALSE);
                    SATTR(ATTR(self,prog),assert_checks,FALSE);
                    SATTR(ATTR(self,prog),arith_checks,FALSE);
                    create_self27 = ((FSETSTR) NULL);
                    ret_val54 = ((FSETSTR) NULL);
                    SATTR(self,arith_in,ret_val54);
                    SATTR(self,arith_all,FALSE);
                    create_self28 = ((FSETSTR) NULL);
                    ret_val55 = ((FSETSTR) NULL);
                    SATTR(self,bounds_in,ret_val55);
                    SATTR(self,bounds_all,FALSE);
                    create_self29 = ((FSETSTR) NULL);
                    ret_val56 = ((FSETSTR) NULL);
                    SATTR(self,void_in,ret_val56);
                    SATTR(self,void_all,FALSE);
                    create_self30 = ((FSETSTR) NULL);
                    ret_val57 = ((FSETSTR) NULL);
                    SATTR(self,pre_in,ret_val57);
                    SATTR(self,pre_all,FALSE);
                    create_self31 = ((FSETSTR) NULL);
                    ret_val58 = ((FSETSTR) NULL);
                    SATTR(self,post_in,ret_val58);
                    SATTR(self,post_all,FALSE);
                    create_self32 = ((FSETSTR) NULL);
                    ret_val59 = ((FSETSTR) NULL);
                    SATTR(self,assert_in,ret_val59);
                    SATTR(self,assert_all,FALSE);
                    create_self33 = ((FSETSTR) NULL);
                    ret_val60 = ((FSETSTR) NULL);
                    SATTR(self,invariant_in,ret_val60);
                    SATTR(self,invariant_all,FALSE);
                    create_self34 = ((FSETSTR) NULL);
                    ret_val61 = ((FSETSTR) NULL);
                    SATTR(self,when_in,ret_val61);
                    SATTR(self,when_all,FALSE);
                    SATTR(self,destroy_chk,FALSE);
                    create_self35 = ((FSETSTR) NULL);
                    ret_val62 = ((FSETSTR) NULL);
                    SATTR(self,return_in,ret_val62);
                    SATTR(self,return_all,FALSE);
                    SATTR(self,psather_chk,FALSE);
                   }
                   else {
                    union_self27 = ATTR(self,arith_out);
                    union_s27 = ATTR(self,classes1);
                    L30 = FSETST1884046860(union_self27);
                    ret_val63 = FSETST866022026(L30, union_s27);
                    SATTR(self,arith_out,ret_val63);
                    union_self28 = ATTR(self,bounds_out);
                    union_s28 = ATTR(self,classes1);
                    L31 = FSETST1884046860(union_self28);
                    ret_val64 = FSETST866022026(L31, union_s28);
                    SATTR(self,bounds_out,ret_val64);
                    union_self29 = ATTR(self,void_out);
                    union_s29 = ATTR(self,classes1);
                    L32 = FSETST1884046860(union_self29);
                    ret_val65 = FSETST866022026(L32, union_s29);
                    SATTR(self,void_out,ret_val65);
                    union_self30 = ATTR(self,pre_out);
                    union_s30 = ATTR(self,classes1);
                    L33 = FSETST1884046860(union_self30);
                    ret_val66 = FSETST866022026(L33, union_s30);
                    SATTR(self,pre_out,ret_val66);
                    union_self31 = ATTR(self,post_out);
                    union_s31 = ATTR(self,classes1);
                    L34 = FSETST1884046860(union_self31);
                    ret_val67 = FSETST866022026(L34, union_s31);
                    SATTR(self,post_out,ret_val67);
                    union_self32 = ATTR(self,assert_out);
                    union_s32 = ATTR(self,classes1);
                    L35 = FSETST1884046860(union_self32);
                    ret_val68 = FSETST866022026(L35, union_s32);
                    SATTR(self,assert_out,ret_val68);
                    union_self33 = ATTR(self,invariant_out);
                    union_s33 = ATTR(self,classes1);
                    L36 = FSETST1884046860(union_self33);
                    ret_val69 = FSETST866022026(L36, union_s33);
                    SATTR(self,invariant_out,ret_val69);
                    union_self34 = ATTR(self,when_out);
                    union_s34 = ATTR(self,classes1);
                    L37 = FSETST1884046860(union_self34);
                    ret_val70 = FSETST866022026(L37, union_s34);
                    SATTR(self,when_out,ret_val70);
                    union_self35 = ATTR(self,return_out);
                    union_s35 = ATTR(self,classes1);
                    L38 = FSETST1884046860(union_self35);
                    ret_val71 = FSETST866022026(L38, union_s35);
                    SATTR(self,return_out,ret_val71);
                   }
                  }
                  else {
                   if (STR_is111530248(L11, ((STR) &chk_no))) {
                    if (STR_is111530248(s, ((STR) &chk_no))) {
                     SATTR(self,all1,TRUE);
                    }
                    else {
                     CS_OPT194850632(self);
                    }
                    if (ATTR(self,all1)) {
                     SATTR(ATTR(self,prog),void_checks,FALSE);
                     SATTR(ATTR(self,prog),pre_checks,FALSE);
                     SATTR(ATTR(self,prog),post_checks,FALSE);
                     SATTR(ATTR(self,prog),assert_checks,FALSE);
                     SATTR(ATTR(self,prog),arith_checks,FALSE);
                     create_self36 = ((FSETSTR) NULL);
                     ret_val72 = ((FSETSTR) NULL);
                     SATTR(self,arith_in,ret_val72);
                     SATTR(self,arith_all,FALSE);
                     create_self37 = ((FSETSTR) NULL);
                     ret_val73 = ((FSETSTR) NULL);
                     SATTR(self,bounds_in,ret_val73);
                     SATTR(self,bounds_all,FALSE);
                     create_self38 = ((FSETSTR) NULL);
                     ret_val74 = ((FSETSTR) NULL);
                     SATTR(self,void_in,ret_val74);
                     SATTR(self,void_all,FALSE);
                     create_self39 = ((FSETSTR) NULL);
                     ret_val75 = ((FSETSTR) NULL);
                     SATTR(self,pre_in,ret_val75);
                     SATTR(self,pre_all,FALSE);
                     create_self40 = ((FSETSTR) NULL);
                     ret_val76 = ((FSETSTR) NULL);
                     SATTR(self,post_in,ret_val76);
                     SATTR(self,post_all,FALSE);
                     create_self41 = ((FSETSTR) NULL);
                     ret_val77 = ((FSETSTR) NULL);
                     SATTR(self,assert_in,ret_val77);
                     SATTR(self,assert_all,FALSE);
                     create_self42 = ((FSETSTR) NULL);
                     ret_val78 = ((FSETSTR) NULL);
                     SATTR(self,invariant_in,ret_val78);
                     SATTR(self,invariant_all,FALSE);
                     create_self43 = ((FSETSTR) NULL);
                     ret_val79 = ((FSETSTR) NULL);
                     SATTR(self,when_in,ret_val79);
                     SATTR(self,when_all,FALSE);
                     SATTR(self,destroy_chk,FALSE);
                     create_self44 = ((FSETSTR) NULL);
                     ret_val80 = ((FSETSTR) NULL);
                     SATTR(self,return_in,ret_val80);
                     SATTR(self,return_all,FALSE);
                     SATTR(self,psather_chk,FALSE);
                    }
                    else {
                     union_self36 = ATTR(self,arith_out);
                     union_s36 = ATTR(self,classes1);
                     L39 = FSETST1884046860(union_self36);
                     ret_val81 = FSETST866022026(L39, union_s36);
                     SATTR(self,arith_out,ret_val81);
                     union_self37 = ATTR(self,bounds_out);
                     union_s37 = ATTR(self,classes1);
                     L40 = FSETST1884046860(union_self37);
                     ret_val82 = FSETST866022026(L40, union_s37);
                     SATTR(self,bounds_out,ret_val82);
                     union_self38 = ATTR(self,void_out);
                     union_s38 = ATTR(self,classes1);
                     L41 = FSETST1884046860(union_self38);
                     ret_val83 = FSETST866022026(L41, union_s38);
                     SATTR(self,void_out,ret_val83);
                     union_self39 = ATTR(self,pre_out);
                     union_s39 = ATTR(self,classes1);
                     L42 = FSETST1884046860(union_self39);
                     ret_val84 = FSETST866022026(L42, union_s39);
                     SATTR(self,pre_out,ret_val84);
                     union_self40 = ATTR(self,post_out);
                     union_s40 = ATTR(self,classes1);
                     L43 = FSETST1884046860(union_self40);
                     ret_val85 = FSETST866022026(L43, union_s40);
                     SATTR(self,post_out,ret_val85);
                     union_self41 = ATTR(self,assert_out);
                     union_s41 = ATTR(self,classes1);
                     L44 = FSETST1884046860(union_self41);
                     ret_val86 = FSETST866022026(L44, union_s41);
                     SATTR(self,assert_out,ret_val86);
                     union_self42 = ATTR(self,invariant_out);
                     union_s42 = ATTR(self,classes1);
                     L45 = FSETST1884046860(union_self42);
                     ret_val87 = FSETST866022026(L45, union_s42);
                     SATTR(self,invariant_out,ret_val87);
                     union_self43 = ATTR(self,when_out);
                     union_s43 = ATTR(self,classes1);
                     L46 = FSETST1884046860(union_self43);
                     ret_val88 = FSETST866022026(L46, union_s43);
                     SATTR(self,when_out,ret_val88);
                     union_self44 = ATTR(self,return_out);
                     union_s44 = ATTR(self,classes1);
                     L47 = FSETST1884046860(union_self44);
                     ret_val89 = FSETST866022026(L47, union_s44);
                     SATTR(self,return_out,ret_val89);
                    }
                   }
                   else {
                    if (STR_is111530248(L11, ((STR) &chk_no_arith))) {
                     CS_OPT194850632(self);
                     if (ATTR(self,all1)) {
                      SATTR(ATTR(self,prog),arith_checks,FALSE);
                      create_self45 = ((FSETSTR) NULL);
                      ret_val90 = ((FSETSTR) NULL);
                      SATTR(self,arith_in,ret_val90);
                      SATTR(self,arith_all,FALSE);
                     }
                     else {
                      union_self45 = ATTR(self,arith_out);
                      union_s45 = ATTR(self,classes1);
                      L48 = FSETST1884046860(union_self45);
                      ret_val91 = FSETST866022026(L48, union_s45);
                      SATTR(self,arith_out,ret_val91);
                     }
                    }
                    else {
                     if (STR_is111530248(L11, ((STR) &chk_no_bounds))) {
                      CS_OPT194850632(self);
                      if (ATTR(self,all1)) {
                       create_self46 = ((FSETSTR) NULL);
                       ret_val92 = ((FSETSTR) NULL);
                       SATTR(self,bounds_in,ret_val92);
                       SATTR(self,bounds_all,FALSE);
                      }
                      else {
                       union_self46 = ATTR(self,bounds_out);
                       union_s46 = ATTR(self,classes1);
                       L49 = FSETST1884046860(union_self46);
                       ret_val93 = FSETST866022026(L49, union_s46);
                       SATTR(self,bounds_out,ret_val93);
                      }
                     }
                     else {
                      if (STR_is111530248(L11, ((STR) &chk_no_void))) {
                       CS_OPT194850632(self);
                       if (ATTR(self,all1)) {
                        SATTR(ATTR(self,prog),void_checks,FALSE);
                        create_self47 = ((FSETSTR) NULL);
                        ret_val94 = ((FSETSTR) NULL);
                        SATTR(self,void_in,ret_val94);
                        SATTR(self,void_all,FALSE);
                       }
                       else {
                        union_self47 = ATTR(self,void_out);
                        union_s47 = ATTR(self,classes1);
                        L50 = FSETST1884046860(union_self47);
                        ret_val95 = FSETST866022026(L50, union_s47);
                        SATTR(self,void_out,ret_val95);
                       }
                      }
                      else {
                       if (STR_is111530248(L11, ((STR) &chk_no_pre))) {
                        CS_OPT194850632(self);
                        if (ATTR(self,all1)) {
                         SATTR(ATTR(self,prog),pre_checks,FALSE);
                         create_self48 = ((FSETSTR) NULL);
                         ret_val96 = ((FSETSTR) NULL);
                         SATTR(self,pre_in,ret_val96);
                         SATTR(self,pre_all,FALSE);
                        }
                        else {
                         union_self48 = ATTR(self,pre_out);
                         union_s48 = ATTR(self,classes1);
                         L51 = FSETST1884046860(union_self48);
                         ret_val97 = FSETST866022026(L51, union_s48);
                         SATTR(self,pre_out,ret_val97);
                        }
                       }
                       else {
                        if (STR_is111530248(L11, ((STR) &chk_no_post))) {
                         CS_OPT194850632(self);
                         if (ATTR(self,all1)) {
                          SATTR(ATTR(self,prog),post_checks,FALSE);
                          create_self49 = ((FSETSTR) NULL);
                          ret_val98 = ((FSETSTR) NULL);
                          SATTR(self,post_in,ret_val98);
                          SATTR(self,post_all,FALSE);
                         }
                         else {
                          union_self49 = ATTR(self,post_out);
                          union_s49 = ATTR(self,classes1);
                          L52 = FSETST1884046860(union_self49);
                          ret_val99 = FSETST866022026(L52, union_s49);
                          SATTR(self,post_out,ret_val99);
                         }
                        }
                        else {
                         if (STR_is111530248(L11, ((STR) &chk_no_assert))) {
                          CS_OPT194850632(self);
                          if (ATTR(self,all1)) {
                           SATTR(ATTR(self,prog),assert_checks,FALSE);
                           create_self50 = ((FSETSTR) NULL);
                           ret_val100 = ((FSETSTR) NULL);
                           SATTR(self,assert_in,ret_val100);
                           SATTR(self,assert_all,FALSE);
                          }
                          else {
                           union_self50 = ATTR(self,assert_out);
                           union_s50 = ATTR(self,classes1);
                           L53 = FSETST1884046860(union_self50);
                           ret_val101 = FSETST866022026(L53, union_s50);
                           SATTR(self,assert_out,ret_val101);
                          }
                         }
                         else {
                          if (STR_is111530248(L11, ((STR) &chk_no_invariant))) {
                           CS_OPT194850632(self);
                           if (ATTR(self,all1)) {
                            create_self51 = ((FSETSTR) NULL);
                            ret_val102 = ((FSETSTR) NULL);
                            SATTR(self,invariant_in,ret_val102);
                            SATTR(self,invariant_all,FALSE);
                           }
                           else {
                            union_self51 = ATTR(self,invariant_out);
                            union_s51 = ATTR(self,classes1);
                            L54 = FSETST1884046860(union_self51);
                            ret_val103 = FSETST866022026(L54, union_s51);
                            SATTR(self,invariant_out,ret_val103);
                           }
                          }
                          else {
                           if (STR_is111530248(L11, ((STR) &chk_no_destroy))) {
                            SATTR(self,destroy_chk,FALSE);
                           }
                           else {
                            if (STR_is111530248(L11, ((STR) &chk_no_psather))) {
                             SATTR(self,psather_chk,FALSE);
                            }
                            else {
                             if (STR_is111530248(L11, ((STR) &no_stats))) {
                              SATTR(self,stats,FALSE);
                             }
                             else {
                              if (STR_is111530248(L11, ((STR) &no_psather_stats))) {
                               SATTR(self,psather_stats,FALSE);
                              }
                              else {
                               if (STR_is111530248(L11, ((STR) &no_psather_trace))) {
                                SATTR(self,psather_trace,FALSE);
                               }
                               else {
                                if (STR_is111530248(L11, ((STR) &chk_no_return))) {
                                 CS_OPT194850632(self);
                                 if (ATTR(self,all1)) {
                                  create_self52 = ((FSETSTR) NULL);
                                  ret_val104 = ((FSETSTR) NULL);
                                  SATTR(self,return_in,ret_val104);
                                  SATTR(self,return_all,FALSE);
                                 }
                                 else {
                                  union_self52 = ATTR(self,return_out);
                                  union_s52 = ATTR(self,classes1);
                                  L55 = FSETST1884046860(union_self52);
                                  ret_val105 = FSETST866022026(L55, union_s52);
                                  SATTR(self,return_out,ret_val105);
                                 }
                                }
                                else {
                                 if (STR_is111530248(L11, ((STR) &chk_no_when))) {
                                  CS_OPT194850632(self);
                                  if (ATTR(self,all1)) {
                                   create_self53 = ((FSETSTR) NULL);
                                   ret_val106 = ((FSETSTR) NULL);
                                   SATTR(self,when_in,ret_val106);
                                   SATTR(self,when_all,FALSE);
                                  }
                                  else {
                                   union_self53 = ATTR(self,when_out);
                                   union_s53 = ATTR(self,classes1);
                                   L56 = FSETST1884046860(union_self53);
                                   ret_val107 = FSETST866022026(L56, union_s53);
                                   SATTR(self,when_out,ret_val107);
                                  }
                                 }
                                 else {
                                  if (STR_is111530248(L11, ((STR) &not_a_42245793))) {
                                   L57 = ATTR(ATTR(self,prog),genuine_platform);
                                   L581_=INTMINUS(L57,1); 
                                   SATTR(ATTR(self,prog),genuine_platform,L581_);
                                  }
                                  else {
                                   L591_=ARR((STR)s,0); 
                                   L60 = L591_;
                                   L611_=L60=='-'; 
                                   if (L611_) {
                                    plus_self = ATTR(ATTR(self,prog),home);
                                    plus_sarg = ((STR) &SystemPlatforms);
                                    ret_val108 = STR_ap2004550586(plus_self, plus_sarg);
                                    plus_self1 = ret_val108;
                                    L62 = STR_lengthrINT(s);
                                    L631_=INTMINUS(L62,1); 
                                    plus_sarg1 = STR_tail_INTrSTR(s, L631_);
                                    ret_val109 = STR_ap2004550586(plus_self1, plus_sarg1);
                                    cfs = ret_val109;
                                    plus_self2 = cfs;
                                    plus_sarg2 = ((STR) &Platformmodule);
                                    ret_val110 = STR_ap2004550586(plus_self2, plus_sarg2);
                                    platform_module = ret_val110;
                                    open_for_read_se = ((BFILE) NULL);
                                    open_for_read_nm = platform_module;
                                    L65=ZALLOC(sizeof(struct BFILE_struct));
                                    if (L65==NULL) FATAL("Unable to allocate more memory");
                                    ((OB)L65)->header.tag=BFILE_tag;
                                    L64 = ((BFILE) L65);
                                    r = L64;
                                    L66 = open_for_read_nm;
                                    L67 = ((STR) &rb);
                                    SATTR(r,fp,fopen(((L66==NULL)?NULL:L66->arr_part), ((L67==NULL)?NULL:L67->arr_part)));
                                    ret_val1111 = r;
                                    f = ret_val1111;
                                    error_self = f;
                                    if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
                                     L68 = TRUE;
                                    } else {
                                     L68 = ferror(ATTR(error_self,fp));
                                    }
                                    ret_val112 = L68;
                                    if (ret_val112) {
                                     create_self54 = ((ERR1) NULL);
                                     ret_val113 = create_self54;
                                     plus_self3 = ret_val113;
                                     plus_s = ((dSTR) ((STR) &Unreco482634771));
                                     stderr_self = ((FILE1) NULL);
                                     L70=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L70==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L70)->header.tag=FILE1_tag;
                                     L69 = ((FILE1) L70);
                                     r1 = L69;
                                     
                                     SATTR(r1,fp,stderr);
                                     ret_val115 = r1;
                                     FILE_plus_dSTR(ret_val115, plus_s);
                                     ret_val114 = plus_self3;
                                     plus_self4 = ret_val114;
                                     plus_s1 = ((dSTR) s);
                                     stderr_self1 = ((FILE1) NULL);
                                     L73=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L73==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L73)->header.tag=FILE1_tag;
                                     L72 = ((FILE1) L73);
                                     r2 = L72;
                                     
                                     SATTR(r2,fp,stderr);
                                     ret_val117 = r2;
                                     FILE_plus_dSTR(ret_val117, plus_s1);
                                     ret_val116 = plus_self4;
                                     plus_self5 = ret_val116;
                                     plus_s2 = ((dSTR) ((STR) &orplatform));
                                     stderr_self2 = ((FILE1) NULL);
                                     L76=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L76==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L76)->header.tag=FILE1_tag;
                                     L75 = ((FILE1) L76);
                                     r3 = L75;
                                     
                                     SATTR(r3,fp,stderr);
                                     ret_val119 = r3;
                                     FILE_plus_dSTR(ret_val119, plus_s2);
                                     ret_val118 = plus_self5;
                                     plus_self6 = ret_val118;
                                     L78 = STR_lengthrINT(s);
                                     L791_=INTMINUS(L78,1); 
                                     plus_s3 = ((dSTR) STR_tail_INTrSTR(s, L791_));
                                     stderr_self3 = ((FILE1) NULL);
                                     L81=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L81==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L81)->header.tag=FILE1_tag;
                                     L80 = ((FILE1) L81);
                                     r4 = L80;
                                     
                                     SATTR(r4,fp,stderr);
                                     ret_val121 = r4;
                                     FILE_plus_dSTR(ret_val121, plus_s3);
                                     ret_val120 = plus_self6;
                                     plus_self7 = ret_val120;
                                     plus_s4 = ((dSTR) ((STR) &missing1));
                                     stderr_self4 = ((FILE1) NULL);
                                     L84=ZALLOC(sizeof(struct FILE1_struct));
                                     if (L84==NULL) FATAL("Unable to allocate more memory");
                                     ((OB)L84)->header.tag=FILE1_tag;
                                     L83 = ((FILE1) L84);
                                     r5 = L83;
                                     
                                     SATTR(r5,fp,stderr);
                                     ret_val122 = r5;
                                     FILE_plus_dSTR(ret_val122, plus_s4);
                                     exit_self = ((UNIX) NULL);
                                     exit_code = 1;
                                     exit(exit_code);
                                    }
                                    else {
                                     SATTR(ATTR(self,prog),platforms,FLISTS1425610062(ATTR(ATTR(self,prog),platforms), cfs));
                                     L86 = ATTR(ATTR(self,prog),genuine_platform);
                                     L871_=INTPLUS(L86,1); 
                                     SATTR(ATTR(self,prog),genuine_platform,L871_);
                                     L88 = CS_OPT975854719(self, platform_module);
                                     plus_self8 = ((STR) &Platform);
                                     plus_sarg3 = s;
                                     ret_val123 = STR_ap2004550586(plus_self8, plus_sarg3);
                                     CS_OPT1644060403(self, L88, ret_val123);
                                    }
                                   }
                                   else {
                                    CS_OPT390587743(self, s);
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
     }
    }
   }
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT611018296(CS_OPTIONS self) {
 STR arg;
 STR suf;
 STR path11;
 CS_OPTIONS more_args_self;
 BOOL ret_val = BOOL_zero;
 CS_OPTIONS rewind_self;
 BFILE resolve_path_sel;
 STR resolve_path_fn;
 STR ret_val1;
 INT L1;
 ARRAYSTR L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 INT L8;
 INT L91_;
 extern STR sa;
 BOOL L10;
 BOOL L111_;
 extern STR module;
 FLISTSTR L12;
 while (1) {
  more_args_self = self;
  L1 = ATTR(more_args_self,next);
  L2 = ATTR(more_args_self,args);
  L31_=(L2)==NULL?0:ASIZE((ARRAYSTR)L2); 
  L4 = L31_;
  L51_=(L1)<(L4); 
  ret_val = L51_;
  if (ret_val) {
  }
  else {
   goto after_loop;
  }
  arg = CS_OPT1970940548(self);
  L6 = CS_OPT1373530461(self, arg);
  L71_=!(L6); 
  if (L71_) {
   rewind_self = self;
   L8 = ATTR(rewind_self,next);
   L91_=INTMINUS(L8,1); 
   SATTR(rewind_self,next,L91_);
   goto after_loop;
  }
  suf = CS_OPT1526155198(self, arg);
  if (STR_is111530248(suf, ((STR) &sa))) {
   resolve_path_sel = ((BFILE) NULL);
   resolve_path_fn = arg;
   ret_val1 = resolve_path_fn;
   path11 = ret_val1;
   L10 = FSETST1025458804(ATTR(self,paths), path11);
   L111_=!(L10); 
   if (L111_) {
    SATTR(ATTR(self,prog),sather_files,FSETST1404644833(ATTR(ATTR(self,prog),sather_files), arg));
    SATTR(self,paths,FSETST1404644833(ATTR(self,paths), path11));
   }
  }
  else {
   if (STR_is111530248(suf, ((STR) &module))) {
    L12 = CS_OPT975854719(self, arg);
    CS_OPT1644060403(self, L12, arg);
   }
   else {
    CS_OPT287833045(self, arg, suf);
    return;
   }
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPT65198073(CS_OPTIONS self, STR s) {
 STR L11;
 INT len1 = INT_zero;
 STR fn;
 FSETSTR L21;
 STR arg;
 STR path11;
 PROG barf_self;
 STR barf_msg;
 PROG barf_at_self;
 STR barf_at_msg;
 dPROG_ERR barf_at_at;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 UNIX exit_self;
 INT exit_code = INT_zero;
 PROG barf_self1;
 STR barf_msg1;
 PROG barf_at_self1;
 STR barf_at_msg1;
 dPROG_ERR barf_at_at1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 UNIX exit_self1;
 INT exit_code1 = INT_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 PROG barf_self2;
 STR barf_msg2;
 PROG barf_at_self2;
 STR barf_at_msg2;
 dPROG_ERR barf_at_at2;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val3;
 UNIX exit_self2;
 INT exit_code2 = INT_zero;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val4;
 PROG barf_self3;
 STR barf_msg3;
 PROG barf_at_self3;
 STR barf_at_msg3;
 dPROG_ERR barf_at_at3;
 STR plus_self5;
 STR plus_sarg5;
 STR ret_val5;
 UNIX exit_self3;
 INT exit_code3 = INT_zero;
 STR plus_self6;
 STR plus_sarg6;
 STR ret_val6;
 STR plus_self7;
 STR plus_sarg7;
 STR ret_val7;
 ERR1 create_self;
 ERR1 ret_val8;
 ERR1 plus_self8;
 dSTR plus_s;
 ERR1 ret_val9;
 FILE1 stderr_self;
 FILE1 ret_val10;
 FILE1 r;
 ERR1 plus_self9;
 dSTR plus_s1;
 ERR1 ret_val11;
 FILE1 stderr_self1;
 FILE1 ret_val12;
 FILE1 r1;
 ERR1 plus_self10;
 dSTR plus_s2;
 FILE1 stderr_self2;
 FILE1 ret_val13;
 FILE1 r2;
 OUT create_self1;
 OUT ret_val14;
 OUT plus_self11;
 STR plus_s3;
 OUT ret_val15;
 FILE1 stdout_self;
 FILE1 ret_val16;
 FILE1 r3;
 OUT plus_self12;
 STR plus_s4;
 OUT ret_val17;
 FILE1 stdout_self1;
 FILE1 ret_val18;
 FILE1 r4;
 OUT plus_self13;
 STR plus_s5;
 OUT ret_val19;
 FILE1 stdout_self2;
 FILE1 ret_val20;
 FILE1 r5;
 OUT flush_self;
 FILE1 stdout_self3;
 FILE1 ret_val21;
 FILE1 r6;
 FILE1 flush_self1;
 FSETSTR create_self2;
 FSETSTR ret_val22;
 FSETSTR create_self3;
 FSETSTR ret_val23;
 FSETSTR create_self4;
 FSETSTR ret_val24;
 FSETSTR create_self5;
 FSETSTR ret_val25;
 FSETSTR create_self6;
 FSETSTR ret_val26;
 FSETSTR create_self7;
 FSETSTR ret_val27;
 FSETSTR create_self8;
 FSETSTR ret_val28;
 FSETSTR create_self9;
 FSETSTR ret_val29;
 FSETSTR create_self10;
 FSETSTR ret_val30;
 STR_CURSOR create_self11;
 STR create_s;
 STR_CURSOR ret_val31;
 STR_CURSOR res;
 BFILE resolve_path_sel;
 STR resolve_path_fn;
 STR ret_val32;
 extern STR temp_o1738599304;
 extern STR benchmark;
 extern STR o1;
 extern STR output_C;
 extern STR pretty;
 extern STR only_parse;
 extern STR only_check;
 extern STR only_C;
 extern STR only_reachable;
 extern STR chk_generics;
 extern STR O;
 extern STR chk_no175010891;
 extern STR debug_source;
 extern STR Youmay2007887531;
 extern STR Intern28965746;
 extern STR debug_1188458465;
 extern STR debug1;
 extern STR Youmay1813826976;
 extern STR Intern28965746;
 BOOL L3;
 BOOL L41_;
 extern STR System752569864;
 FLISTSTR L5;
 extern STR system1545422995;
 extern STR debug_817185317;
 extern STR debug_graphical;
 BOOL L6;
 BOOL L7;
 BOOL L81_;
 extern STR Youmay1813826976;
 extern STR Intern28965746;
 BOOL L9;
 BOOL L101_;
 extern STR System1737534222;
 FLISTSTR L12;
 extern STR system1025342899;
 extern STR debug_C;
 extern STR Youmay2007887531;
 extern STR Intern28965746;
 extern STR g1;
 BOOL L13;
 BOOL L141_;
 extern STR System752569864;
 FLISTSTR L15;
 extern STR system1545422995;
 extern STR reflect1;
 extern STR System992100435;
 FLISTSTR L16;
 extern STR system804335861;
 extern STR P_type;
 extern STR P_func;
 extern STR C_flag;
 FLISTSTR L17;
 extern STR mangle_debug;
 extern STR mangle1996057645;
 STR L18;
 INT L191_;
 extern STR external1;
 FMAPSTRFSETSTR L20;
 STR L22;
 extern STR has;
 BOOL L23;
 BOOL L241_;
 extern STR hascla2025698242;
 FILE1 L25;
 OB L26;
 FILE1 L28;
 OB L29;
 extern STR instea1664484879;
 FILE1 L31;
 OB L32;
 extern STR hasclasses;
 FILE1 L34;
 OB L35;
 FILE1 L37;
 OB L38;
 extern STR name7;
 FILE1 L40;
 OB L42;
 FILE1 L44;
 OB L45;
 FMAPSTRSTR L47;
 STR L48;
 STR L49;
 extern STR O_hois1744329543;
 extern STR O_no_h899352895;
 extern STR O_move_while;
 extern STR O_no_move_while;
 extern STR O_hoist_const;
 extern STR O_no_hoist_const;
 extern STR O_no_inline;
 extern STR O_inline;
 extern STR O_inli1907354513;
 STR L50;
 INT L511_;
 INT L52;
 BOOL L531_;
 extern STR O_inline_iters;
 STR L54;
 INT L551_;
 INT L56;
 BOOL L571_;
 extern STR O_yiel409239953;
 extern STR O_no_y1254216601;
 extern STR O_locks_on_stack;
 extern STR O_no_l1217538254;
 extern STR O_fork;
 extern STR O_at1;
 extern STR O_at2;
 extern STR O_fast;
 extern STR force1;
 FSETSTR L58;
 extern STR force_all;
 extern STR threshold;
 STR_CURSOR L59;
 OB L60;
 extern STR no_loop_poll;
 extern STR O_parloops;
 extern STR O_post_write;
 extern STR O_cache;
 extern STR O_cache_size;
 STR L61;
 INT L621_;
 extern STR O_cach734630041;
 STR L63;
 INT L641_;
 extern STR O_side_effects;
 extern STR O_side_debug;
 extern STR O_local;
 extern STR O_local_call;
 extern STR O_loca312924046;
 STR L65;
 INT L661_;
 extern STR O_loca2092708304;
 extern STR O_loca398973374;
 STR L67;
 INT L681_;
 extern STR O_remote_call;
 extern STR O_remo1835005866;
 extern STR O_remo1703539318;
 STR L69;
 INT L701_;
 extern STR O_pref781540580;
 STR L71;
 INT L721_;
 extern STR O_spec566822832;
 extern STR O_loop_prefetch;
 extern STR O_prefetch;
 extern STR O_cse;
 extern STR O_cse_debug;
 extern STR O_no_local;
 extern STR O_no_local_call;
 extern STR O_no_l418290947;
 extern STR O_no_remote_call;
 extern STR O_no_r675993385;
 extern STR O_no_s1500643180;
 extern STR O_no_side_debug;
 extern STR O_no_cache;
 extern STR O_no_prefetch;
 extern STR O_no_s278153816;
 extern STR O_no_l1800759415;
 extern STR O_no_post_write;
 extern STR O_no_parloops;
 extern STR O_no_cse;
 extern STR O_no_cse_debug;
 extern STR convert;
 BOOL L73;
 BOOL L741_;
 extern STR convert_all;
 extern STR V10;
 extern STR version1;
 extern STR V;
 extern STR zones;
 extern STR W;
 L11 = s;
 if (STR_is111530248(L11, ((STR) &temp_o1738599304))) {
  SATTR(ATTR(self,prog),temp_option_fast,TRUE);
 }
 else {
  if (STR_is111530248(L11, ((STR) &benchmark))) {
   SATTR(ATTR(self,prog),benchmark,TRUE);
  }
  else {
   if (STR_is111530248(L11, ((STR) &o1))) {
    SATTR(ATTR(self,cgen),executable1,CS_OPT1970940548(self));
   }
   else {
    if (STR_is111530248(L11, ((STR) &output_C))) {
     SATTR(ATTR(self,cgen),gen_c,TRUE);
    }
    else {
     if (STR_is111530248(L11, ((STR) &pretty))) {
      SATTR(ATTR(self,cgen),pretty,TRUE);
     }
     else {
      if (STR_is111530248(L11, ((STR) &only_parse))) {
       SATTR(ATTR(self,cs),only_parse,TRUE);
      }
      else {
       if (STR_is111530248(L11, ((STR) &only_check))) {
        SATTR(ATTR(self,cgen),only_check,TRUE);
       }
       else {
        if (STR_is111530248(L11, ((STR) &only_C))) {
         SATTR(ATTR(self,cgen),only_C,TRUE);
         SATTR(ATTR(self,cgen),gen_c,TRUE);
        }
        else {
         if (STR_is111530248(L11, ((STR) &only_reachable))) {
          SATTR(ATTR(self,cs),only_reachable,TRUE);
         }
         else {
          if (STR_is111530248(L11, ((STR) &chk_generics))) {
           SATTR(ATTR(self,prog),check_generics,TRUE);
          }
          else {
           if (STR_is111530248(L11, ((STR) &O))) {
            SATTR(ATTR(self,prog),inline_routs,TRUE);
            SATTR(ATTR(self,prog),inline_iters,TRUE);
            SATTR(ATTR(self,prog),move_while,TRUE);
            SATTR(ATTR(self,prog),hoist_iter_init,TRUE);
            SATTR(ATTR(self,prog),hoist_const,TRUE);
            SATTR(ATTR(self,cgen),optimize,TRUE);
            SATTR(ATTR(self,inliner),inline_routs,TRUE);
            SATTR(ATTR(self,inliner),inline_iters,TRUE);
            SATTR(self,side_effects,TRUE);
            SATTR(self,cse,TRUE);
           }
           else {
            if (STR_is111530248(L11, ((STR) &chk_no175010891))) {
             SATTR(ATTR(self,prog),chk_no_line_numb,TRUE);
            }
            else {
             if (STR_is111530248(L11, ((STR) &debug_source))) {
              if (ATTR(self,debug_C_seen)) {
               barf_self = ATTR(self,prog);
               barf_msg = ((STR) &Youmay2007887531);
               barf_at_self = barf_self;
               barf_at_msg = barf_msg;
               barf_at_at = ((dPROG_ERR) NULL);
               PROG_e176405615(barf_at_self, barf_at_at);
               plus_self = ((STR) &Intern28965746);
               plus_sarg = barf_at_msg;
               ret_val = STR_ap2004550586(plus_self, plus_sarg);
               PROG_err_STR(barf_at_self, ret_val);
               exit_self = ((UNIX) NULL);
               exit_code = 1;
               exit(exit_code);
              }
              SATTR(ATTR(self,cgen),debug1,TRUE);
              SATTR(ATTR(self,cgen),gen_c,TRUE);
              SATTR(self,debug_source_see,TRUE);
             }
             else {
              if (STR_is111530248(L11, ((STR) &debug_1188458465))) {
               SATTR(ATTR(self,cgen),deterministic1,TRUE);
              }
              else {
               if (STR_is111530248(L11, ((STR) &debug1))) {
                if (ATTR(ATTR(self,cgen),print_ob_g)) {
                 barf_self1 = ATTR(self,prog);
                 barf_msg1 = ((STR) &Youmay1813826976);
                 barf_at_self1 = barf_self1;
                 barf_at_msg1 = barf_msg1;
                 barf_at_at1 = ((dPROG_ERR) NULL);
                 PROG_e176405615(barf_at_self1, barf_at_at1);
                 plus_self1 = ((STR) &Intern28965746);
                 plus_sarg1 = barf_at_msg1;
                 ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
                 PROG_err_STR(barf_at_self1, ret_val1);
                 exit_self1 = ((UNIX) NULL);
                 exit_code1 = 1;
                 exit(exit_code1);
                }
                L3 = ATTR(ATTR(self,cgen),print_ob);
                L41_=!(L3); 
                if (L41_) {
                 SATTR(ATTR(self,cgen),print_ob,TRUE);
                 SATTR(ATTR(self,cgen),func_tables,TRUE);
                 SATTR(ATTR(self,cgen),type_tables,TRUE);
                 plus_self2 = ATTR(ATTR(self,prog),home);
                 plus_sarg2 = ((STR) &System752569864);
                 ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
                 L5 = CS_OPT975854719(self, ret_val2);
                 CS_OPT1644060403(self, L5, ((STR) &system1545422995));
                }
                SATTR(ATTR(self,cgen),debug1,TRUE);
                SATTR(ATTR(self,cgen),gen_c,TRUE);
               }
               else {
                if (STR_is111530248(L11, ((STR) &debug_817185317))) {
                 SATTR(ATTR(self,cgen),func_tables,FALSE);
                }
                else {
                 if (STR_is111530248(L11, ((STR) &debug_graphical))) {
                  if (ATTR(ATTR(self,cgen),print_ob)) {
                   L7 = ATTR(ATTR(self,cgen),print_ob_g);
                   L81_=!(L7); 
                   L6 = L81_;
                  } else {
                   L6 = FALSE;
                  }
                  if (L6) {
                   barf_self2 = ATTR(self,prog);
                   barf_msg2 = ((STR) &Youmay1813826976);
                   barf_at_self2 = barf_self2;
                   barf_at_msg2 = barf_msg2;
                   barf_at_at2 = ((dPROG_ERR) NULL);
                   PROG_e176405615(barf_at_self2, barf_at_at2);
                   plus_self3 = ((STR) &Intern28965746);
                   plus_sarg3 = barf_at_msg2;
                   ret_val3 = STR_ap2004550586(plus_self3, plus_sarg3);
                   PROG_err_STR(barf_at_self2, ret_val3);
                   exit_self2 = ((UNIX) NULL);
                   exit_code2 = 1;
                   exit(exit_code2);
                  }
                  L9 = ATTR(ATTR(self,cgen),print_ob_g);
                  L101_=!(L9); 
                  if (L101_) {
                   SATTR(ATTR(self,cgen),print_ob_g,TRUE);
                   SATTR(ATTR(self,cgen),func_tables,TRUE);
                   SATTR(ATTR(self,cgen),type_tables,TRUE);
                   plus_self4 = ATTR(ATTR(self,prog),home);
                   plus_sarg4 = ((STR) &System1737534222);
                   ret_val4 = STR_ap2004550586(plus_self4, plus_sarg4);
                   L12 = CS_OPT975854719(self, ret_val4);
                   CS_OPT1644060403(self, L12, ((STR) &system1025342899));
                  }
                  SATTR(ATTR(self,cgen),debug1,TRUE);
                  SATTR(ATTR(self,cgen),gen_c,TRUE);
                 }
                 else {
                  if (STR_is111530248(L11, ((STR) &debug_C))) {
                   if (ATTR(self,debug_source_see)) {
                    barf_self3 = ATTR(self,prog);
                    barf_msg3 = ((STR) &Youmay2007887531);
                    barf_at_self3 = barf_self3;
                    barf_at_msg3 = barf_msg3;
                    barf_at_at3 = ((dPROG_ERR) NULL);
                    PROG_e176405615(barf_at_self3, barf_at_at3);
                    plus_self5 = ((STR) &Intern28965746);
                    plus_sarg5 = barf_at_msg3;
                    ret_val5 = STR_ap2004550586(plus_self5, plus_sarg5);
                    PROG_err_STR(barf_at_self3, ret_val5);
                    exit_self3 = ((UNIX) NULL);
                    exit_code3 = 1;
                    exit(exit_code3);
                   }
                   SATTR(self,debug_C_seen,TRUE);
                   SATTR(ATTR(self,cgen),c_flags,FLISTS1425610062(ATTR(ATTR(self,cgen),c_flags), ((STR) &g1)));
                   SATTR(ATTR(self,cgen),debug1,FALSE);
                   SATTR(ATTR(self,cgen),gen_c,TRUE);
                   SATTR(ATTR(self,cgen),pretty,TRUE);
                   L13 = ATTR(ATTR(self,cgen),print_ob);
                   L141_=!(L13); 
                   if (L141_) {
                    SATTR(ATTR(self,cgen),print_ob,TRUE);
                    SATTR(ATTR(self,cgen),func_tables,TRUE);
                    SATTR(ATTR(self,cgen),type_tables,TRUE);
                    plus_self6 = ATTR(ATTR(self,prog),home);
                    plus_sarg6 = ((STR) &System752569864);
                    ret_val6 = STR_ap2004550586(plus_self6, plus_sarg6);
                    L15 = CS_OPT975854719(self, ret_val6);
                    CS_OPT1644060403(self, L15, ((STR) &system1545422995));
                   }
                  }
                  else {
                   if (STR_is111530248(L11, ((STR) &reflect1))) {
                    SATTR(ATTR(self,cgen),type_tables,TRUE);
                    SATTR(ATTR(self,cgen),func_tables,TRUE);
                    plus_self7 = ATTR(ATTR(self,prog),home);
                    plus_sarg7 = ((STR) &System992100435);
                    ret_val7 = STR_ap2004550586(plus_self7, plus_sarg7);
                    L16 = CS_OPT975854719(self, ret_val7);
                    CS_OPT1644060403(self, L16, ((STR) &system804335861));
                   }
                   else {
                    if (STR_is111530248(L11, ((STR) &P_type))) {
                     SATTR(ATTR(self,cgen),type_tables,TRUE);
                    }
                    else {
                     if (STR_is111530248(L11, ((STR) &P_func))) {
                      SATTR(ATTR(self,cgen),type_tables,TRUE);
                      SATTR(ATTR(self,cgen),func_tables,TRUE);
                     }
                     else {
                      if (STR_is111530248(L11, ((STR) &C_flag))) {
                       L17 = ATTR(ATTR(self,cgen),c_flags);
                       SATTR(ATTR(self,cgen),c_flags,FLISTS1425610062(L17, CS_OPT1970940548(self)));
                      }
                      else {
                       if (STR_is111530248(L11, ((STR) &mangle_debug))) {
                        SATTR(ATTR(self,prog),debug_mangle,TRUE);
                       }
                       else {
                        if (STR_is111530248(L11, ((STR) &mangle1996057645))) {
                         L18 = CS_OPT1970940548(self);
                         L191_=atoi(((STR)L18)->arr_part); 
                         len1 = L191_;
                         CGEN_m1427112264 = len1;
                        }
                        else {
                         if (STR_is111530248(L11, ((STR) &external1))) {
                          L20 = ATTR(ATTR(self,cgen),externals);
                          L22 = CS_OPT1970940548(self);
                          SATTR(ATTR(self,cgen),externals,FMAPST935488038(L20, L22, CS_OPT595032350(self)));
                         }
                         else {
                          if (STR_is111530248(L11, ((STR) &has))) {
                           fn = CS_OPT1970940548(self);
                           L23 = CS_OPT1373530461(self, fn);
                           L241_=!(L23); 
                           if (L241_) {
                            create_self = ((ERR1) NULL);
                            ret_val8 = create_self;
                            plus_self8 = ret_val8;
                            plus_s = ((dSTR) ((STR) &hascla2025698242));
                            stderr_self = ((FILE1) NULL);
                            L26=ZALLOC(sizeof(struct FILE1_struct));
                            if (L26==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L26)->header.tag=FILE1_tag;
                            L25 = ((FILE1) L26);
                            r = L25;
                            
                            SATTR(r,fp,stderr);
                            ret_val10 = r;
                            FILE_plus_dSTR(ret_val10, plus_s);
                            ret_val9 = plus_self8;
                            plus_self9 = ret_val9;
                            plus_s1 = ((dSTR) fn);
                            stderr_self1 = ((FILE1) NULL);
                            L29=ZALLOC(sizeof(struct FILE1_struct));
                            if (L29==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L29)->header.tag=FILE1_tag;
                            L28 = ((FILE1) L29);
                            r1 = L28;
                            
                            SATTR(r1,fp,stderr);
                            ret_val12 = r1;
                            FILE_plus_dSTR(ret_val12, plus_s1);
                            ret_val11 = plus_self9;
                            plus_self10 = ret_val11;
                            plus_s2 = ((dSTR) ((STR) &instea1664484879));
                            stderr_self2 = ((FILE1) NULL);
                            L32=ZALLOC(sizeof(struct FILE1_struct));
                            if (L32==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L32)->header.tag=FILE1_tag;
                            L31 = ((FILE1) L32);
                            r2 = L31;
                            
                            SATTR(r2,fp,stderr);
                            ret_val13 = r2;
                            FILE_plus_dSTR(ret_val13, plus_s2);
                           }
                           SATTR(ATTR(self,parser),known_files,FSETST1404644833(ATTR(ATTR(self,parser),known_files), fn));
                           CS_OPT194850632(self);
                           if (ATTR(ATTR(self,prog),show_types)) {
                            create_self1 = ((OUT) NULL);
                            ret_val14 = create_self1;
                            plus_self11 = ret_val14;
                            plus_s3 = ((STR) &hasclasses);
                            stdout_self = ((FILE1) NULL);
                            L35=ZALLOC(sizeof(struct FILE1_struct));
                            if (L35==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L35)->header.tag=FILE1_tag;
                            L34 = ((FILE1) L35);
                            r3 = L34;
                            
                            SATTR(r3,fp,stdout);
                            ret_val16 = r3;
                            FILE_plus_STR(ret_val16, plus_s3);
                            ret_val15 = plus_self11;
                            plus_self12 = ret_val15;
                            plus_s4 = FSETSTR_strrSTR(ATTR(self,classes1));
                            stdout_self1 = ((FILE1) NULL);
                            L38=ZALLOC(sizeof(struct FILE1_struct));
                            if (L38==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L38)->header.tag=FILE1_tag;
                            L37 = ((FILE1) L38);
                            r4 = L37;
                            
                            SATTR(r4,fp,stdout);
                            ret_val18 = r4;
                            FILE_plus_STR(ret_val18, plus_s4);
                            ret_val17 = plus_self12;
                            plus_self13 = ret_val17;
                            plus_s5 = ((STR) &name7);
                            stdout_self2 = ((FILE1) NULL);
                            L42=ZALLOC(sizeof(struct FILE1_struct));
                            if (L42==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L42)->header.tag=FILE1_tag;
                            L40 = ((FILE1) L42);
                            r5 = L40;
                            
                            SATTR(r5,fp,stdout);
                            ret_val20 = r5;
                            FILE_plus_STR(ret_val20, plus_s5);
                            ret_val19 = plus_self13;
                            flush_self = ret_val19;
                            stdout_self3 = ((FILE1) NULL);
                            L45=ZALLOC(sizeof(struct FILE1_struct));
                            if (L45==NULL) FATAL("Unable to allocate more memory");
                            ((OB)L45)->header.tag=FILE1_tag;
                            L44 = ((FILE1) L45);
                            r6 = L44;
                            
                            SATTR(r6,fp,stdout);
                            ret_val21 = r6;
                            flush_self1 = ret_val21;
                            fflush(ATTR(flush_self1,fp));
                           }
                           {
                            struct FSETSTR_eltbrSTR_frame_struct other1_0;
FSETSTR_eltbrSTR_frame noname1 = &other1_0;
                            L21 = ATTR(self,classes1);
                            noname1->self = L21;
                            noname1->state = 0;
                            while (1) {
                             L47 = ATTR(ATTR(self,parser),has);
                             L48 = FSETSTR_eltbrSTR(noname1);
                             if (noname1->state == -1) {
                              goto after_loop;
                             }
                             L49 = L48;
                             SATTR(ATTR(self,parser),has,FMAPST859022540(L47, L49, fn));
                            }
                           }
                           after_loop: ;
                          }
                          else {
                           if (STR_is111530248(L11, ((STR) &O_hois1744329543))) {
                            SATTR(ATTR(self,cgen),optimize,TRUE);
                            SATTR(ATTR(self,prog),hoist_iter_init,TRUE);
                           }
                           else {
                            if (STR_is111530248(L11, ((STR) &O_no_h899352895))) {
                             SATTR(ATTR(self,prog),hoist_iter_init,FALSE);
                            }
                            else {
                             if (STR_is111530248(L11, ((STR) &O_move_while))) {
                              SATTR(ATTR(self,prog),move_while,TRUE);
                              SATTR(ATTR(self,cgen),optimize,TRUE);
                             }
                             else {
                              if (STR_is111530248(L11, ((STR) &O_no_move_while))) {
                               SATTR(ATTR(self,prog),move_while,FALSE);
                              }
                              else {
                               if (STR_is111530248(L11, ((STR) &O_hoist_const))) {
                                SATTR(ATTR(self,prog),hoist_const,TRUE);
                                SATTR(ATTR(self,cgen),optimize,TRUE);
                               }
                               else {
                                if (STR_is111530248(L11, ((STR) &O_no_hoist_const))) {
                                 SATTR(ATTR(self,prog),hoist_const,FALSE);
                                }
                                else {
                                 if (STR_is111530248(L11, ((STR) &O_no_inline))) {
                                  SATTR(ATTR(self,inliner),inline_routs,FALSE);
                                  SATTR(ATTR(self,inliner),inline_iters,FALSE);
                                  SATTR(ATTR(self,prog),inline_routs,FALSE);
                                  SATTR(ATTR(self,prog),inline_iters,FALSE);
                                 }
                                 else {
                                  if (STR_is111530248(L11, ((STR) &O_inline))) {
                                   SATTR(ATTR(self,inliner),inline_routs,TRUE);
                                   SATTR(ATTR(self,inliner),inline_iters,TRUE);
                                   SATTR(ATTR(self,prog),inline_routs,TRUE);
                                   SATTR(ATTR(self,prog),inline_iters,TRUE);
                                   SATTR(ATTR(self,cgen),optimize,TRUE);
                                  }
                                  else {
                                   if (STR_is111530248(L11, ((STR) &O_inli1907354513))) {
                                    L50 = CS_OPT1970940548(self);
                                    L511_=atoi(((STR)L50)->arr_part); 
                                    SATTR(ATTR(self,inliner),rout_thres,L511_);
                                    SATTR(ATTR(self,prog),inline_routs,TRUE);
                                    L52 = ATTR(ATTR(self,inliner),rout_thres);
                                    L531_=(0)<(L52); 
                                    SATTR(ATTR(self,inliner),inline_routs,L531_);
                                    SATTR(ATTR(self,cgen),optimize,TRUE);
                                   }
                                   else {
                                    if (STR_is111530248(L11, ((STR) &O_inline_iters))) {
                                     L54 = CS_OPT1970940548(self);
                                     L551_=atoi(((STR)L54)->arr_part); 
                                     SATTR(ATTR(self,inliner),iter_thres,L551_);
                                     SATTR(ATTR(self,prog),inline_iters,TRUE);
                                     L56 = ATTR(ATTR(self,inliner),iter_thres);
                                     L571_=(0)<(L56); 
                                     SATTR(ATTR(self,inliner),inline_iters,L571_);
                                     SATTR(ATTR(self,cgen),optimize,TRUE);
                                    }
                                    else {
                                     if (STR_is111530248(L11, ((STR) &O_yiel409239953))) {
                                      SATTR(ATTR(self,prog),yields_in_locks,TRUE);
                                      SATTR(self,side_effects,TRUE);
                                     }
                                     else {
                                      if (STR_is111530248(L11, ((STR) &O_no_y1254216601))) {
                                       SATTR(ATTR(self,prog),yields_in_locks,FALSE);
                                      }
                                      else {
                                       if (STR_is111530248(L11, ((STR) &O_locks_on_stack))) {
                                        SATTR(ATTR(self,prog),locks_on_stack,TRUE);
                                        SATTR(self,side_effects,TRUE);
                                       }
                                       else {
                                        if (STR_is111530248(L11, ((STR) &O_no_l1217538254))) {
                                         SATTR(ATTR(self,prog),locks_on_stack,FALSE);
                                        }
                                        else {
                                         if (STR_is111530248(L11, ((STR) &O_fork))) {
                                          SATTR(ATTR(self,prog),fast_fork,TRUE);
                                         }
                                         else {
                                          if (STR_is111530248(L11, ((STR) &O_at1))) {
                                           SATTR(ATTR(self,prog),fast_at1,TRUE);
                                           SATTR(self,side_effects,TRUE);
                                          }
                                          else {
                                           if (STR_is111530248(L11, ((STR) &O_at2))) {
                                            SATTR(ATTR(self,prog),fast_at2,TRUE);
                                            SATTR(self,side_effects,TRUE);
                                           }
                                           else {
                                            if (STR_is111530248(L11, ((STR) &O_fast))) {
                                             SATTR(ATTR(self,cgen),optimize,TRUE);
                                             SATTR(ATTR(self,inliner),inline_routs,TRUE);
                                             SATTR(ATTR(self,inliner),inline_iters,TRUE);
                                             SATTR(ATTR(self,prog),inline_routs,TRUE);
                                             SATTR(ATTR(self,prog),inline_iters,TRUE);
                                             SATTR(ATTR(self,prog),move_while,TRUE);
                                             SATTR(ATTR(self,prog),hoist_iter_init,TRUE);
                                             SATTR(ATTR(self,prog),hoist_const,TRUE);
                                             SATTR(ATTR(self,prog),yields_in_locks,TRUE);
                                             SATTR(ATTR(self,prog),locks_on_stack,TRUE);
                                             SATTR(self,side_effects,TRUE);
                                             SATTR(self,cse,TRUE);
                                             create_self2 = ((FSETSTR) NULL);
                                             ret_val22 = ((FSETSTR) NULL);
                                             SATTR(self,arith_in,ret_val22);
                                             SATTR(self,arith_all,FALSE);
                                             create_self3 = ((FSETSTR) NULL);
                                             ret_val23 = ((FSETSTR) NULL);
                                             SATTR(self,bounds_in,ret_val23);
                                             SATTR(self,bounds_all,FALSE);
                                             create_self4 = ((FSETSTR) NULL);
                                             ret_val24 = ((FSETSTR) NULL);
                                             SATTR(self,void_in,ret_val24);
                                             SATTR(self,void_all,FALSE);
                                             create_self5 = ((FSETSTR) NULL);
                                             ret_val25 = ((FSETSTR) NULL);
                                             SATTR(self,pre_in,ret_val25);
                                             SATTR(self,pre_all,FALSE);
                                             create_self6 = ((FSETSTR) NULL);
                                             ret_val26 = ((FSETSTR) NULL);
                                             SATTR(self,post_in,ret_val26);
                                             SATTR(self,post_all,FALSE);
                                             create_self7 = ((FSETSTR) NULL);
                                             ret_val27 = ((FSETSTR) NULL);
                                             SATTR(self,assert_in,ret_val27);
                                             SATTR(self,assert_all,FALSE);
                                             create_self8 = ((FSETSTR) NULL);
                                             ret_val28 = ((FSETSTR) NULL);
                                             SATTR(self,invariant_in,ret_val28);
                                             SATTR(self,invariant_all,FALSE);
                                             create_self9 = ((FSETSTR) NULL);
                                             ret_val29 = ((FSETSTR) NULL);
                                             SATTR(self,when_in,ret_val29);
                                             SATTR(self,when_all,FALSE);
                                             SATTR(self,psather_chk,FALSE);
                                             SATTR(self,destroy_chk,FALSE);
                                             create_self10 = ((FSETSTR) NULL);
                                             ret_val30 = ((FSETSTR) NULL);
                                             SATTR(self,return_in,ret_val30);
                                             SATTR(self,return_all,FALSE);
                                             SATTR(ATTR(self,prog),arith_checks,FALSE);
                                             SATTR(ATTR(self,prog),assert_checks,FALSE);
                                             SATTR(ATTR(self,prog),void_checks,FALSE);
                                             SATTR(ATTR(self,prog),pre_checks,FALSE);
                                             SATTR(ATTR(self,prog),post_checks,FALSE);
                                            }
                                            else {
                                             if (STR_is111530248(L11, ((STR) &force1))) {
                                              L58 = ATTR(ATTR(self,cgen),force_routines);
                                              SATTR(ATTR(self,cgen),force_routines,FSETST1404644833(L58, CS_OPT1970940548(self)));
                                             }
                                             else {
                                              if (STR_is111530248(L11, ((STR) &force_all))) {
                                               SATTR(ATTR(self,cgen),force_all,TRUE);
                                              }
                                              else {
                                               if (STR_is111530248(L11, ((STR) &threshold))) {
                                                create_self11 = ((STR_CURSOR) NULL);
                                                create_s = CS_OPT1970940548(self);
                                                L60=ZALLOC(sizeof(struct STR_CURSOR_struct));
                                                if (L60==NULL) FATAL("Unable to allocate more memory");
                                                ((OB)L60)->header.tag=STR_CURSOR_tag;
                                                L59 = ((STR_CURSOR) L60);
                                                res = L59;
                                                if ((create_s==((STR) NULL))) {
                                                 res = ((STR_CURSOR) NULL);
                                                }
                                                else {
                                                 STR_CU1355884946(res, create_s);
                                                }
                                                SATTR(res,comment_char1,' ');
                                                SATTR(res,comment_char2,' ');
                                                ret_val31 = res;
                                                SATTR(ATTR(self,cgen),threshold,STR_CU1297585490(ret_val31));
                                               }
                                               else {
                                                if (STR_is111530248(L11, ((STR) &no_loop_poll))) {
                                                 SATTR(self,loop_poll,FALSE);
                                                }
                                                else {
                                                 if (STR_is111530248(L11, ((STR) &O_parloops))) {
                                                  SATTR(self,parloops,TRUE);
                                                  SATTR(ATTR(self,cgen),optimize,TRUE);
                                                  SATTR(self,side_effects,TRUE);
                                                 }
                                                 else {
                                                  if (STR_is111530248(L11, ((STR) &O_post_write))) {
                                                   SATTR(self,post_write,TRUE);
                                                   SATTR(ATTR(self,cgen),optimize,TRUE);
                                                  }
                                                  else {
                                                   if (STR_is111530248(L11, ((STR) &O_cache))) {
                                                    SATTR(self,cache1,TRUE);
                                                   }
                                                   else {
                                                    if (STR_is111530248(L11, ((STR) &O_cache_size))) {
                                                     L61 = CS_OPT1970940548(self);
                                                     L621_=atoi(((STR)L61)->arr_part); 
                                                     SATTR(self,cache_size,L621_);
                                                    }
                                                    else {
                                                     if (STR_is111530248(L11, ((STR) &O_cach734630041))) {
                                                      L63 = CS_OPT1970940548(self);
                                                      L641_=atoi(((STR)L63)->arr_part); 
                                                      SATTR(self,cache_slot_size,L641_);
                                                     }
                                                     else {
                                                      if (STR_is111530248(L11, ((STR) &O_side_effects))) {
                                                       SATTR(self,side_effects,TRUE);
                                                      }
                                                      else {
                                                       if (STR_is111530248(L11, ((STR) &O_side_debug))) {
                                                        SATTR(self,side_effects,TRUE);
                                                        SATTR(self,side_debug,TRUE);
                                                       }
                                                       else {
                                                        if (STR_is111530248(L11, ((STR) &O_local))) {
                                                         SATTR(ATTR(self,cgen),optimize,TRUE);
                                                         SATTR(self,local_opt,TRUE);
                                                         SATTR(self,side_effects,TRUE);
                                                        }
                                                        else {
                                                         if (STR_is111530248(L11, ((STR) &O_local_call))) {
                                                          SATTR(ATTR(self,cgen),optimize,TRUE);
                                                          SATTR(self,local_opt,TRUE);
                                                          SATTR(self,local_call,TRUE);
                                                          SATTR(self,side_effects,TRUE);
                                                         }
                                                         else {
                                                          if (STR_is111530248(L11, ((STR) &O_loca312924046))) {
                                                           L65 = CS_OPT1970940548(self);
                                                           L661_=atoi(((STR)L65)->arr_part); 
                                                           SATTR(self,local_call_acces,L661_);
                                                          }
                                                          else {
                                                           if (STR_is111530248(L11, ((STR) &O_loca2092708304))) {
                                                            SATTR(ATTR(self,cgen),optimize,TRUE);
                                                            SATTR(self,local_opt,TRUE);
                                                            SATTR(self,local_call,TRUE);
                                                            SATTR(self,local_call_dynam1,TRUE);
                                                            SATTR(self,side_effects,TRUE);
                                                           }
                                                           else {
                                                            if (STR_is111530248(L11, ((STR) &O_loca398973374))) {
                                                             L67 = CS_OPT1970940548(self);
                                                             L681_=atoi(((STR)L67)->arr_part); 
                                                             SATTR(self,local_call_dynam,L681_);
                                                            }
                                                            else {
                                                             if (STR_is111530248(L11, ((STR) &O_remote_call))) {
                                                              SATTR(ATTR(self,cgen),optimize,TRUE);
                                                              SATTR(self,local_opt,TRUE);
                                                              SATTR(self,side_effects,TRUE);
                                                              SATTR(self,local_call,TRUE);
                                                              SATTR(self,local_call_dynam1,TRUE);
                                                              SATTR(self,remote_call,TRUE);
                                                             }
                                                             else {
                                                              if (STR_is111530248(L11, ((STR) &O_remo1835005866))) {
                                                               SATTR(ATTR(self,cgen),optimize,TRUE);
                                                               SATTR(self,side_effects,TRUE);
                                                               SATTR(self,local_opt,TRUE);
                                                               SATTR(self,local_call_dynam1,TRUE);
                                                               SATTR(self,local_call,TRUE);
                                                               SATTR(self,remote_call,TRUE);
                                                               SATTR(self,remote_call_crea,TRUE);
                                                              }
                                                              else {
                                                               if (STR_is111530248(L11, ((STR) &O_remo1703539318))) {
                                                                SATTR(self,side_effects,TRUE);
                                                                L69 = CS_OPT1970940548(self);
                                                                L701_=atoi(((STR)L69)->arr_part); 
                                                                SATTR(self,remote_call_acce,L701_);
                                                               }
                                                               else {
                                                                if (STR_is111530248(L11, ((STR) &O_pref781540580))) {
                                                                 L71 = CS_OPT1970940548(self);
                                                                 L721_=atoi(((STR)L71)->arr_part); 
                                                                 SATTR(self,prefetch_weight,L721_);
                                                                }
                                                                else {
                                                                 if (STR_is111530248(L11, ((STR) &O_spec566822832))) {
                                                                  SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                  SATTR(self,loop_prefetch,TRUE);
                                                                  SATTR(self,specul_prefetch,TRUE);
                                                                  SATTR(self,prefetch,TRUE);
                                                                  SATTR(self,side_effects,TRUE);
                                                                  SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                 }
                                                                 else {
                                                                  if (STR_is111530248(L11, ((STR) &O_loop_prefetch))) {
                                                                   SATTR(self,loop_prefetch,TRUE);
                                                                   SATTR(self,prefetch,TRUE);
                                                                   SATTR(self,side_effects,TRUE);
                                                                   SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                  }
                                                                  else {
                                                                   if (STR_is111530248(L11, ((STR) &O_prefetch))) {
                                                                    SATTR(self,prefetch,TRUE);
                                                                    SATTR(self,side_effects,TRUE);
                                                                    SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                   }
                                                                   else {
                                                                    if (STR_is111530248(L11, ((STR) &O_cse))) {
                                                                     SATTR(self,cse,TRUE);
                                                                     SATTR(self,side_effects,TRUE);
                                                                     SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                    }
                                                                    else {
                                                                     if (STR_is111530248(L11, ((STR) &O_cse_debug))) {
                                                                      SATTR(self,cse,TRUE);
                                                                      SATTR(self,cse_debug,TRUE);
                                                                      SATTR(self,side_effects,TRUE);
                                                                      SATTR(ATTR(self,cgen),optimize,TRUE);
                                                                     }
                                                                     else {
                                                                      if (STR_is111530248(L11, ((STR) &O_no_local))) {
                                                                       SATTR(self,local_opt,FALSE);
                                                                      }
                                                                      else {
                                                                       if (STR_is111530248(L11, ((STR) &O_no_local_call))) {
                                                                        SATTR(self,local_call,FALSE);
                                                                       }
                                                                       else {
                                                                        if (STR_is111530248(L11, ((STR) &O_no_l418290947))) {
                                                                         SATTR(self,local_call_dynam1,FALSE);
                                                                        }
                                                                        else {
                                                                         if (STR_is111530248(L11, ((STR) &O_no_remote_call))) {
                                                                          SATTR(self,remote_call,FALSE);
                                                                         }
                                                                         else {
                                                                          if (STR_is111530248(L11, ((STR) &O_no_r675993385))) {
                                                                           SATTR(self,remote_call_crea,FALSE);
                                                                          }
                                                                          else {
                                                                           if (STR_is111530248(L11, ((STR) &O_no_s1500643180))) {
                                                                            SATTR(self,side_effects,FALSE);
                                                                           }
                                                                           else {
                                                                            if (STR_is111530248(L11, ((STR) &O_no_side_debug))) {
                                                                             SATTR(self,side_debug,FALSE);
                                                                            }
                                                                            else {
                                                                             if (STR_is111530248(L11, ((STR) &O_no_cache))) {
                                                                              SATTR(self,cache1,FALSE);
                                                                             }
                                                                             else {
                                                                              if (STR_is111530248(L11, ((STR) &O_no_prefetch))) {
                                                                               SATTR(self,prefetch,FALSE);
                                                                              }
                                                                              else {
                                                                               if (STR_is111530248(L11, ((STR) &O_no_s278153816))) {
                                                                                SATTR(self,specul_prefetch,FALSE);
                                                                               }
                                                                               else {
                                                                                if (STR_is111530248(L11, ((STR) &O_no_l1800759415))) {
                                                                                 SATTR(self,loop_prefetch,FALSE);
                                                                                }
                                                                                else {
                                                                                 if (STR_is111530248(L11, ((STR) &O_no_post_write))) {
                                                                                  SATTR(self,post_write,FALSE);
                                                                                 }
                                                                                 else {
                                                                                  if (STR_is111530248(L11, ((STR) &O_no_parloops))) {
                                                                                   SATTR(self,parloops,FALSE);
                                                                                  }
                                                                                  else {
                                                                                   if (STR_is111530248(L11, ((STR) &O_no_cse))) {
                                                                                    SATTR(self,cse,FALSE);
                                                                                   }
                                                                                   else {
                                                                                    if (STR_is111530248(L11, ((STR) &O_no_cse_debug))) {
                                                                                     SATTR(self,cse_debug,FALSE);
                                                                                    }
                                                                                    else {
                                                                                     if (STR_is111530248(L11, ((STR) &convert))) {
                                                                                      arg = CS_OPT1970940548(self);
                                                                                      SATTR(ATTR(self,parser),convert_files,FSETST1404644833(ATTR(ATTR(self,parser),convert_files), arg));
                                                                                      resolve_path_sel = ((BFILE) NULL);
                                                                                      resolve_path_fn = arg;
                                                                                      ret_val32 = resolve_path_fn;
                                                                                      path11 = ret_val32;
                                                                                      L73 = FSETST1025458804(ATTR(self,paths), path11);
                                                                                      L741_=!(L73); 
                                                                                      if (L741_) {
                                                                                       SATTR(ATTR(self,prog),sather_files,FSETST1404644833(ATTR(ATTR(self,prog),sather_files), arg));
                                                                                       SATTR(self,paths,FSETST1404644833(ATTR(self,paths), path11));
                                                                                      }
                                                                                     }
                                                                                     else {
                                                                                      if (STR_is111530248(L11, ((STR) &convert_all))) {
                                                                                       SATTR(ATTR(self,parser),convert_all,TRUE);
                                                                                       SATTR(ATTR(self,parser),version_1_0,TRUE);
                                                                                      }
                                                                                      else {
                                                                                       if (STR_is111530248(L11, ((STR) &V10))) {
                                                                                        SATTR(ATTR(self,parser),version_1_0,TRUE);
                                                                                       }
                                                                                       else {
                                                                                        if (STR_is111530248(L11, ((STR) &version1))) {
                                                                                         SATTR(ATTR(self,prog),print_version,TRUE);
                                                                                        }
                                                                                        else {
                                                                                         if (STR_is111530248(L11, ((STR) &V))) {
                                                                                          SATTR(ATTR(self,prog),print_version,TRUE);
                                                                                         }
                                                                                         else {
                                                                                          if (STR_is111530248(L11, ((STR) &zones))) {
                                                                                           SATTR(ATTR(self,prog),zones,TRUE);
                                                                                          }
                                                                                          else {
                                                                                           if (STR_is111530248(L11, ((STR) &W))) {
                                                                                            SATTR(ATTR(self,prog),warnings,TRUE);
                                                                                           }
                                                                                           else {
                                                                                            CS_OPT479416027(self, s);
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
 }
}


#undef IS_ITER
#define IS_ITER 0

void CS_OPTIONS_usage(CS_OPTIONS self) {
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
 ERR1 create_self1;
 ERR1 ret_val4;
 ERR1 plus_self2;
 dSTR plus_s2;
 ERR1 ret_val5;
 FILE1 stderr_self2;
 FILE1 ret_val6;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val7;
 FILE1 stderr_self3;
 FILE1 ret_val8;
 FILE1 r3;
 ERR1 plus_self4;
 dSTR plus_s4;
 FILE1 stderr_self4;
 FILE1 ret_val9;
 FILE1 r4;
 ERR1 create_self2;
 ERR1 ret_val10;
 ERR1 plus_self5;
 dSTR plus_s5;
 FILE1 stderr_self5;
 FILE1 ret_val11;
 FILE1 r5;
 UNIX exit_self;
 INT exit_code = INT_zero;
 extern STR Comman1382430749;
 FILE1 L1;
 OB L2;
 ARRAYSTR L4;
 INT L5;
 ARRAYSTR L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 STR L131_;
 FILE1 L14;
 OB L15;
 extern STR in;
 FILE1 L17;
 OB L18;
 ARRAYSTR L20;
 INT L21;
 ARRAYSTR L22;
 INT L231_;
 INT L24;
 INT L251_;
 INT L26;
 INT L271_;
 INT L28;
 STR L291_;
 FILE1 L30;
 OB L32;
 dSTR L34;
 OB L35;
 FILE1 L36;
 OB L37;
 extern STR Seemanpage;
 FILE1 L39;
 OB L40;
 create_self = ((ERR1) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((dSTR) ((STR) &Comman1382430749));
 stderr_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stderr);
 ret_val2 = r;
 FILE_plus_dSTR(ret_val2, plus_s);
 ret_val1 = plus_self;
 plus_self1 = ret_val1;
 L4 = ATTR(self,args);
 L5 = ATTR(self,next);
 L6 = ATTR(self,args);
 L71_=(L6)==NULL?0:ASIZE((ARRAYSTR)L6); 
 L8 = L71_;
 L91_=INTMINUS(L8,1); 
 L10 = L91_;
 L111_=(L5<L10)?L5:L10; 
 L12 = L111_;
 L131_=(STR)ARR((ARRAYSTR)L4,L12); 
 plus_s1 = ((dSTR) L131_);
 stderr_self1 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r1 = L14;
 
 SATTR(r1,fp,stderr);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
 create_self1 = ((ERR1) NULL);
 ret_val4 = create_self1;
 plus_self2 = ret_val4;
 plus_s2 = ((dSTR) ((STR) &in));
 stderr_self2 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r2 = L17;
 
 SATTR(r2,fp,stderr);
 ret_val6 = r2;
 FILE_plus_dSTR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L20 = ATTR(self,arg_locations);
 L21 = ATTR(self,next);
 L22 = ATTR(self,args);
 L231_=(L22)==NULL?0:ASIZE((ARRAYSTR)L22); 
 L24 = L231_;
 L251_=INTMINUS(L24,1); 
 L26 = L251_;
 L271_=(L21<L26)?L21:L26; 
 L28 = L271_;
 L291_=(STR)ARR((ARRAYSTR)L20,L28); 
 plus_s3 = ((dSTR) L291_);
 stderr_self3 = ((FILE1) NULL);
 L32=ZALLOC(sizeof(struct FILE1_struct));
 if (L32==NULL) FATAL("Unable to allocate more memory");
 ((OB)L32)->header.tag=FILE1_tag;
 L30 = ((FILE1) L32);
 r3 = L30;
 
 SATTR(r3,fp,stderr);
 ret_val8 = r3;
 FILE_plus_dSTR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 L35=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L35==NULL) FATAL("Unable to allocate more memory");
 memset(L35,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L35)->header.tag=CHAR_tag;
 L34 = (dSTR)((CHAR_boxed) L35);
 ((CHAR_boxed) L34)->immutable_part = '\n';
 plus_s4 = L34;
 stderr_self4 = ((FILE1) NULL);
 L37=ZALLOC(sizeof(struct FILE1_struct));
 if (L37==NULL) FATAL("Unable to allocate more memory");
 ((OB)L37)->header.tag=FILE1_tag;
 L36 = ((FILE1) L37);
 r4 = L36;
 
 SATTR(r4,fp,stderr);
 ret_val9 = r4;
 FILE_plus_dSTR(ret_val9, plus_s4);
 create_self2 = ((ERR1) NULL);
 ret_val10 = create_self2;
 plus_self5 = ret_val10;
 plus_s5 = ((dSTR) ((STR) &Seemanpage));
 stderr_self5 = ((FILE1) NULL);
 L40=ZALLOC(sizeof(struct FILE1_struct));
 if (L40==NULL) FATAL("Unable to allocate more memory");
 ((OB)L40)->header.tag=FILE1_tag;
 L39 = ((FILE1) L40);
 r5 = L39;
 
 SATTR(r5,fp,stderr);
 ret_val11 = r5;
 FILE_plus_dSTR(ret_val11, plus_s5);
 exit_self = ((UNIX) NULL);
 exit_code = 1;
 exit(exit_code);
}
