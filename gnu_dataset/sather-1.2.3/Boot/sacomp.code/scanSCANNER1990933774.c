#include "sather.h"

/* Layouts */

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

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

typedef struct INTI1_struct {/* layout for INTI */
 OB_HEADER header;
 INT len1;
 INT asize;
 INT arr_part[1];
 } *INTI1;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct RAT_struct {/* layout for RAT */
 struct INTI1_struct *u;
 struct INTI1_struct *v;
 } RAT;
static RAT RAT_zero;

typedef struct RAT_boxed_struct {
 OB_HEADER header;
 RAT immutable_part;
 } *RAT_boxed;

typedef struct SCANNER_struct {/* layout for SCANNER */
 OB_HEADER header;
 struct FSTR_struct *buf1;
 struct IDENT_struct lex_value;
 INT value_type;
 struct PROG_struct *prog;
 struct RAT_struct num_value;
 CHAR char_value;
 CHAR last_char;
 CHAR next;
 BOOL backed_up;
 BOOL pSather;
 BOOL zones;
 } *SCANNER;

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct STR_eltbrCHAR_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 CHAR ret_val;
 INT i;
 INT sz;
 } *STR_eltbrCHAR_frame;

typedef struct TOKEN_struct {/* layout for TOKEN */
 INT val1;
 } TOKEN;
static TOKEN TOKEN_zero;

typedef struct TOKEN_boxed_struct {
 OB_HEADER header;
 TOKEN immutable_part;
 } *TOKEN_boxed;

typedef struct FLISTINT_struct {/* layout for FLIST{INT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 INT arr_part[1];
 } *FLISTINT;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

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

extern BOOL SFILE_ID_newline;
extern CHAR SFILE_446927505;
extern FLISTINT SFILE_ID_lines;
extern FLISTSTR SFILE_ID_files;
extern FSTR SFILE_ID_source;
extern INT AS_FLT1215265092;
extern INT AS_FLT20428321;
extern INT AS_FLT388140360;
extern INT AS_FLT388140455;
extern INT AS_FLT388140740;
extern INT SCANNE1010372543;
extern INT SCANNE1014449292;
extern INT SCANNE1021322009;
extern INT SCANNE105624607;
extern INT SCANNE1090219034;
extern INT SCANNE1099722568;
extern INT SCANNE114307641;
extern INT SCANNE116326663;
extern INT SCANNE1180808401;
extern INT SCANNE1233467974;
extern INT SCANNE1252648769;
extern INT SCANNE1277304122;
extern INT SCANNE1284590324;
extern INT SCANNE1290586306;
extern INT SCANNE1388342024;
extern INT SCANNE1391319271;
extern INT SCANNE1412300811;
extern INT SCANNE1414957377;
extern INT SCANNE1421647682;
extern INT SCANNE149137033;
extern INT SCANNE1499824177;
extern INT SCANNE1514574730;
extern INT SCANNE1553373348;
extern INT SCANNE1574122250;
extern INT SCANNE1717311299;
extern INT SCANNE1733136762;
extern INT SCANNE1785932597;
extern INT SCANNE1874840845;
extern INT SCANNE1901892399;
extern INT SCANNE1938931342;
extern INT SCANNE202030842;
extern INT SCANNE2045360664;
extern INT SCANNE2078461540;
extern INT SCANNE2093995558;
extern INT SCANNE2137779101;
extern INT SCANNE2139920629;
extern INT SCANNE269030544;
extern INT SCANNE274598975;
extern INT SCANNE290718062;
extern INT SCANNE304035056;
extern INT SCANNE334166344;
extern INT SCANNE392475110;
extern INT SCANNE401863129;
extern INT SCANNE438656140;
extern INT SCANNE440197020;
extern INT SCANNE463806109;
extern INT SCANNE470944348;
extern INT SCANNE560979044;
extern INT SCANNE589883380;
extern INT SCANNE620595834;
extern INT SCANNE623252400;
extern INT SCANNE637092534;
extern INT SCANNE661829813;
extern INT SCANNE687089717;
extern INT SCANNE694354542;
extern INT SCANNE738612593;
extern INT SCANNE754612661;
extern INT SCANNE766559877;
extern INT SCANNE801235924;
extern INT SCANNE879190010;
extern INT SCANNE916727723;
extern INT SCANNE928026230;
extern INT SCANNE943336743;
extern INT SCANNE951940802;
extern INT SCANNE983081836;
extern INT SCANNER_ITER_tok;
extern INT SCANNER_ROUT_tok;
extern INT SCANNER_SAME_tok;
extern INT SCANNER_and_tok;
extern INT SCANNER_any_tok;
extern INT SCANNER_at_tok;
extern INT SCANNER_attr_tok;
extern INT SCANNER_bang_tok;
extern INT SCANNER_bind_tok;
extern INT SCANNER_case_tok;
extern INT SCANNER_do_tok;
extern INT SCANNER_dot_tok;
extern INT SCANNER_else_tok;
extern INT SCANNER_end_tok;
extern INT SCANNER_eof_tok;
extern INT SCANNER_far_tok;
extern INT SCANNER_fork_tok;
extern INT SCANNER_here_tok;
extern INT SCANNER_if_tok;
extern INT SCANNER_is_tok;
extern INT SCANNER_lflt_tok;
extern INT SCANNER_lint_tok;
extern INT SCANNER_lock_tok;
extern INT SCANNER_loop_tok;
extern INT SCANNER_lstr_tok;
extern INT SCANNER_mod_tok;
extern INT SCANNER_near_tok;
extern INT SCANNER_new_tok;
extern INT SCANNER_not_tok;
extern INT SCANNER_null_tok;
extern INT SCANNER_once_tok;
extern INT SCANNER_or_tok;
extern INT SCANNER_out_tok;
extern INT SCANNER_par_tok;
extern INT SCANNER_plus_tok;
extern INT SCANNER_post_tok;
extern INT SCANNER_pow_tok;
extern INT SCANNER_pre_tok;
extern INT SCANNER_quit_tok;
extern INT SCANNER_self_tok;
extern INT SCANNER_semi_tok;
extern INT SCANNER_stub_tok;
extern INT SCANNER_sync_tok;
extern INT SCANNER_then_tok;
extern INT SCANNER_true_tok;
extern INT SCANNER_type_tok;
extern INT SCANNER_vbar_tok;
extern INT SCANNER_void_tok;
extern INT SCANNER_when_tok;
extern INT SCANNER_with_tok;
extern INT SFILE_1180160528;
extern INT SFILE_1796395955;
extern INT SFILE_ID_B;
extern INT SFILE_ID_column;
extern INT SFILE_ID_line;
extern INT SFILE_ID_pos;

/* Function declarations */

BOOL CHAR_i1942379470(CHAR);
BOOL CHAR_i424896496(CHAR);
BOOL CHAR_i629101715(CHAR);
BOOL CHAR_i84735023(CHAR);
BOOL FSTR_i104531942(FSTR, STR);
BOOL SCANNE363215919(SCANNER, STR);
BOOL SFILE_2094878322(SFILE_ID, PROG, STR);
BOOL SFILE_947438414(SFILE_ID, OB);
CHAR SCANNE2120205373(SCANNER);
CHAR SFILE_1512288010(SFILE_ID);
CHAR STR_eltbrCHAR(STR_eltbrCHAR_frame);
FLISTINT FLISTI1369678776(FLISTINT, INT);
FLISTINT FLISTI55819225(FLISTINT, INT);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTSTR FLISTS1900495285(FLISTSTR, INT);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT CHAR_d198943415(CHAR);
INT CHAR_h840494345(CHAR);
INT FLISTI1321626366(FLISTINT);
INT FSTR_sizerINT(FSTR);
INT INTI_intrINT(INTI1);
INT INT_hashrINT(INT);
INT SFILE_1102877860(SFILE_ID);
INT SFILE_2070755978(SFILE_ID);
INT SFILE_29532291(SFILE_ID);
INTI1 INTI_c1588968505(INTI1, INT);
INTI1 INTI_m436971372(INTI1, INTI1);
INTI1 INTI_p1518979099(INTI1, INT);
INTI1 INTI_p1769163857(INTI1, INTI1);
INTI1 INTI_t1692605406(INTI1, INTI1);
INTI1 SCANNE1500285552(SCANNER, INT);
RAT RAT_cr9703311(RAT, INTI1, INTI1);
SCANNER SCANNE1298383531(SCANNER, PROG, STR, BOOL);
SFILE_ID SFILE_1594766561(SFILE_ID);
STR SFILE_ID_strrSTR(SFILE_ID);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR TOKEN_strrSTR(TOKEN);
TOKEN SCANNE1018171979(SCANNER);
TOKEN SCANNE1792387248(SCANNER);
TOKEN SCANNE892342113(SCANNER);
void FILE_plus_STR(FILE1, STR);
void PROG_err_STR(PROG, STR);
void SCANNE1781940665(SCANNER);
void SCANNE297868915(SCANNER);
void SCANNER_comment(SCANNER);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL SCANNE363215919(SCANNER self, STR s) {
 BOOL ret_val = BOOL_zero;
 STR L11;
 CHAR L2;
 {
  struct STR_eltbrCHAR_frame_struct other1_0;
STR_eltbrCHAR_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = STR_eltbrCHAR(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   if (CHAR_i424896496(L2)) {
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

BOOL SFILE_2094878322(SFILE_ID self, PROG p, STR name111) {
 BOOL ret_val = BOOL_zero;
 BFILE file1;
 BFILE open_for_read_se;
 STR open_for_read_nm;
 BFILE ret_val1;
 BFILE r;
 BFILE error_self;
 BOOL ret_val2 = BOOL_zero;
 BFILE close_self;
 SFILE_ID no_location_self = SFILE_ID_zero;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 SFILE_ID create_self = SFILE_ID_zero;
 INT create_loc = INT_zero;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 SFILE_ID r1 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 BFILE L1;
 OB L2;
 extern STR rb;
 STR L3;
 STR L4;
 BOOL L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 extern STR couldntreadfile;
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
 ret_val1 = r;
 file1 = ret_val1;
 error_self = file1;
 if ((ATTR(error_self,fp)==((EXT_OB) NULL))) {
  L5 = TRUE;
 } else {
  L5 = ferror(ATTR(error_self,fp));
 }
 ret_val2 = L5;
 L6 = ret_val2;
 L71_=!(L6); 
 if (L71_) {
  SFILE_ID_source = BFILE_fstrrFSTR(file1);
  close_self = file1;
  fclose(ATTR(close_self,fp));
 }
 else {
  SFILE_ID_source = ((FSTR) NULL);
 }
 L8 = (SFILE_ID_source==((FSTR) NULL));
 L91_=!(L8); 
 if (L91_) {
  if ((SFILE_ID_files==((FLISTSTR) NULL))) {
   SFILE_ID_files = FLISTS1900495285(((FLISTSTR) NULL), 64);
   SFILE_ID_lines = FLISTI55819225(((FLISTINT) NULL), 64);
  }
  SFILE_ID_files = FLISTS1425610062(SFILE_ID_files, name111);
  SFILE_ID_lines = FLISTI1369678776(SFILE_ID_lines, SFILE_1180160528);
  SFILE_ID_pos = 0;
  SFILE_ID_column = 1;
  SFILE_ID_newline = TRUE;
  ret_val = TRUE;
  return ret_val;
 }
 else {
  set_eloc_self = p;
  no_location_self = self;
  create_self = SFILE_ID_zero;
  create_loc = -1;
  r1.loc = create_loc;
  ret_val4 = r1;
  ret_val3 = ret_val4;
  set_eloc_l = ret_val3;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  plus_self = ((STR) &couldntreadfile);
  plus_sarg = name111;
  ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
  PROG_err_STR(p, ret_val5);
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL SFILE_947438414(SFILE_ID self, OB arg) {
 BOOL ret_val = BOOL_zero;
 SFILE_ID is_eq_self = SFILE_ID_zero;
 SFILE_ID is_eq_y = SFILE_ID_zero;
 BOOL ret_val1 = BOOL_zero;
 SFILE_ID L1;
 INT L2;
 INT L3;
 BOOL L41_;
 if (arg==NULL) {
  goto other171;
 } else
 switch (TAG(arg)) {
  case SFILE_ID_tag:
   is_eq_self = self;
   L1 = ((SFILE_ID_boxed) arg)->immutable_part;
   is_eq_y = L1;
   L2 = is_eq_self.loc;
   L3 = is_eq_y.loc;
   L41_=(L2)==(L3); 
   ret_val1 = L41_;
   ret_val = ret_val1;
   return ret_val; break;
  default: ;
  other171: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

CHAR SCANNE2120205373(SCANNER self) {
 CHAR ret_val = CHAR_zero;
 CHAR res = CHAR_zero;
 CHAR L11 = CHAR_zero;
 INT v = INT_zero;
 INT d = INT_zero;
 INT v11 = INT_zero;
 INT d11 = INT_zero;
 INT v21 = INT_zero;
 INT d21 = INT_zero;
 INT v3 = INT_zero;
 INT d3 = INT_zero;
 INT v4 = INT_zero;
 INT d4 = INT_zero;
 INT v5 = INT_zero;
 INT d5 = INT_zero;
 INT v6 = INT_zero;
 INT d6 = INT_zero;
 INT v7 = INT_zero;
 INT d7 = INT_zero;
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c11 = CHAR_zero;
 SCANNER fetch_self2;
 CHAR c2 = CHAR_zero;
 SCANNER fetch_self3;
 CHAR c3 = CHAR_zero;
 SCANNER fetch_self4;
 CHAR c4 = CHAR_zero;
 SCANNER fetch_self5;
 CHAR c5 = CHAR_zero;
 SCANNER fetch_self6;
 CHAR c6 = CHAR_zero;
 SCANNER fetch_self7;
 CHAR c7 = CHAR_zero;
 SCANNER fetch_self8;
 CHAR c8 = CHAR_zero;
 SCANNER fetch_self9;
 CHAR c9 = CHAR_zero;
 SCANNER fetch_self10;
 CHAR c10 = CHAR_zero;
 SCANNER fetch_self11;
 CHAR c12 = CHAR_zero;
 SCANNER fetch_self12;
 CHAR c13 = CHAR_zero;
 SCANNER fetch_self13;
 CHAR c14 = CHAR_zero;
 SCANNER fetch_self14;
 CHAR c15 = CHAR_zero;
 SCANNER fetch_self15;
 CHAR c16 = CHAR_zero;
 SCANNER fetch_self16;
 CHAR c17 = CHAR_zero;
 SCANNER fetch_self17;
 CHAR c18 = CHAR_zero;
 SCANNER fetch_self18;
 CHAR c19 = CHAR_zero;
 SCANNER fetch_self19;
 CHAR c20 = CHAR_zero;
 BOOL L2;
 BOOL L31_;
 BOOL L4;
 BOOL L51_;
 BOOL L61_;
 INT L71_;
 INT L8;
 INT L91_;
 CHAR L101_;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 BOOL L161_;
 INT L171_;
 INT L18;
 INT L191_;
 CHAR L201_;
 BOOL L21;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 BOOL L251_;
 INT L261_;
 INT L27;
 INT L281_;
 CHAR L291_;
 BOOL L30;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L351_;
 INT L361_;
 INT L37;
 INT L381_;
 CHAR L391_;
 BOOL L40;
 BOOL L411_;
 BOOL L42;
 BOOL L431_;
 BOOL L441_;
 INT L451_;
 INT L46;
 INT L471_;
 CHAR L481_;
 BOOL L49;
 BOOL L501_;
 BOOL L52;
 BOOL L531_;
 BOOL L541_;
 INT L551_;
 INT L56;
 INT L571_;
 CHAR L581_;
 BOOL L59;
 BOOL L601_;
 BOOL L62;
 BOOL L631_;
 BOOL L641_;
 INT L651_;
 INT L66;
 INT L671_;
 CHAR L681_;
 BOOL L69;
 BOOL L701_;
 BOOL L72;
 BOOL L731_;
 BOOL L741_;
 INT L751_;
 INT L76;
 INT L771_;
 CHAR L781_;
 fetch_self = self;
 if (ATTR(fetch_self,backed_up)) {
  c = ATTR(fetch_self,next);
  SATTR(fetch_self,next,ATTR(fetch_self,last_char));
  SATTR(fetch_self,last_char,c);
  SATTR(fetch_self,backed_up,FALSE);
 }
 else {
  SATTR(fetch_self,last_char,ATTR(fetch_self,next));
  SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
 }
 L11 = ATTR(self,next);
 switch (L11) {
  case '0': 
   v = 0;
   while (1) {
    d = CHAR_h840494345(ATTR(self,next));
    L31_=(d)<(0); 
    L4 = L31_;
    L51_=!(L4); 
    if (L51_) {
     L61_=(d)<(8); 
     L2 = L61_;
    } else {
     L2 = FALSE;
    }
    if (L2) {
    }
    else {
     goto after_loop;
    }
    L71_=INTTIMES(v,8); 
    L8 = L71_;
    L91_=INTPLUS(L8,d); 
    v = L91_;
    fetch_self1 = self;
    if (ATTR(fetch_self1,backed_up)) {
     c11 = ATTR(fetch_self1,next);
     SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
     SATTR(fetch_self1,last_char,c11);
     SATTR(fetch_self1,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
     SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop: ;
   L101_=(CHAR)v; 
   res = L101_;
   break;
  case '1': 
   v11 = 0;
   while (1) {
    d11 = CHAR_h840494345(ATTR(self,next));
    L131_=(d11)<(0); 
    L14 = L131_;
    L151_=!(L14); 
    if (L151_) {
     L161_=(d11)<(8); 
     L12 = L161_;
    } else {
     L12 = FALSE;
    }
    if (L12) {
    }
    else {
     goto after_loop1;
    }
    L171_=INTTIMES(v11,8); 
    L18 = L171_;
    L191_=INTPLUS(L18,d11); 
    v11 = L191_;
    fetch_self2 = self;
    if (ATTR(fetch_self2,backed_up)) {
     c2 = ATTR(fetch_self2,next);
     SATTR(fetch_self2,next,ATTR(fetch_self2,last_char));
     SATTR(fetch_self2,last_char,c2);
     SATTR(fetch_self2,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self2,last_char,ATTR(fetch_self2,next));
     SATTR(fetch_self2,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop1: ;
   L201_=(CHAR)v11; 
   res = L201_;
   break;
  case '2': 
   v21 = 0;
   while (1) {
    d21 = CHAR_h840494345(ATTR(self,next));
    L221_=(d21)<(0); 
    L23 = L221_;
    L241_=!(L23); 
    if (L241_) {
     L251_=(d21)<(8); 
     L21 = L251_;
    } else {
     L21 = FALSE;
    }
    if (L21) {
    }
    else {
     goto after_loop2;
    }
    L261_=INTTIMES(v21,8); 
    L27 = L261_;
    L281_=INTPLUS(L27,d21); 
    v21 = L281_;
    fetch_self3 = self;
    if (ATTR(fetch_self3,backed_up)) {
     c3 = ATTR(fetch_self3,next);
     SATTR(fetch_self3,next,ATTR(fetch_self3,last_char));
     SATTR(fetch_self3,last_char,c3);
     SATTR(fetch_self3,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self3,last_char,ATTR(fetch_self3,next));
     SATTR(fetch_self3,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop2: ;
   L291_=(CHAR)v21; 
   res = L291_;
   break;
  case '3': 
   v3 = 0;
   while (1) {
    d3 = CHAR_h840494345(ATTR(self,next));
    L321_=(d3)<(0); 
    L33 = L321_;
    L341_=!(L33); 
    if (L341_) {
     L351_=(d3)<(8); 
     L30 = L351_;
    } else {
     L30 = FALSE;
    }
    if (L30) {
    }
    else {
     goto after_loop3;
    }
    L361_=INTTIMES(v3,8); 
    L37 = L361_;
    L381_=INTPLUS(L37,d3); 
    v3 = L381_;
    fetch_self4 = self;
    if (ATTR(fetch_self4,backed_up)) {
     c4 = ATTR(fetch_self4,next);
     SATTR(fetch_self4,next,ATTR(fetch_self4,last_char));
     SATTR(fetch_self4,last_char,c4);
     SATTR(fetch_self4,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self4,last_char,ATTR(fetch_self4,next));
     SATTR(fetch_self4,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop3: ;
   L391_=(CHAR)v3; 
   res = L391_;
   break;
  case '4': 
   v4 = 0;
   while (1) {
    d4 = CHAR_h840494345(ATTR(self,next));
    L411_=(d4)<(0); 
    L42 = L411_;
    L431_=!(L42); 
    if (L431_) {
     L441_=(d4)<(8); 
     L40 = L441_;
    } else {
     L40 = FALSE;
    }
    if (L40) {
    }
    else {
     goto after_loop4;
    }
    L451_=INTTIMES(v4,8); 
    L46 = L451_;
    L471_=INTPLUS(L46,d4); 
    v4 = L471_;
    fetch_self5 = self;
    if (ATTR(fetch_self5,backed_up)) {
     c5 = ATTR(fetch_self5,next);
     SATTR(fetch_self5,next,ATTR(fetch_self5,last_char));
     SATTR(fetch_self5,last_char,c5);
     SATTR(fetch_self5,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self5,last_char,ATTR(fetch_self5,next));
     SATTR(fetch_self5,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop4: ;
   L481_=(CHAR)v4; 
   res = L481_;
   break;
  case '5': 
   v5 = 0;
   while (1) {
    d5 = CHAR_h840494345(ATTR(self,next));
    L501_=(d5)<(0); 
    L52 = L501_;
    L531_=!(L52); 
    if (L531_) {
     L541_=(d5)<(8); 
     L49 = L541_;
    } else {
     L49 = FALSE;
    }
    if (L49) {
    }
    else {
     goto after_loop5;
    }
    L551_=INTTIMES(v5,8); 
    L56 = L551_;
    L571_=INTPLUS(L56,d5); 
    v5 = L571_;
    fetch_self6 = self;
    if (ATTR(fetch_self6,backed_up)) {
     c6 = ATTR(fetch_self6,next);
     SATTR(fetch_self6,next,ATTR(fetch_self6,last_char));
     SATTR(fetch_self6,last_char,c6);
     SATTR(fetch_self6,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self6,last_char,ATTR(fetch_self6,next));
     SATTR(fetch_self6,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop5: ;
   L581_=(CHAR)v5; 
   res = L581_;
   break;
  case '6': 
   v6 = 0;
   while (1) {
    d6 = CHAR_h840494345(ATTR(self,next));
    L601_=(d6)<(0); 
    L62 = L601_;
    L631_=!(L62); 
    if (L631_) {
     L641_=(d6)<(8); 
     L59 = L641_;
    } else {
     L59 = FALSE;
    }
    if (L59) {
    }
    else {
     goto after_loop6;
    }
    L651_=INTTIMES(v6,8); 
    L66 = L651_;
    L671_=INTPLUS(L66,d6); 
    v6 = L671_;
    fetch_self7 = self;
    if (ATTR(fetch_self7,backed_up)) {
     c7 = ATTR(fetch_self7,next);
     SATTR(fetch_self7,next,ATTR(fetch_self7,last_char));
     SATTR(fetch_self7,last_char,c7);
     SATTR(fetch_self7,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self7,last_char,ATTR(fetch_self7,next));
     SATTR(fetch_self7,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop6: ;
   L681_=(CHAR)v6; 
   res = L681_;
   break;
  case '7': 
   v7 = 0;
   while (1) {
    d7 = CHAR_h840494345(ATTR(self,next));
    L701_=(d7)<(0); 
    L72 = L701_;
    L731_=!(L72); 
    if (L731_) {
     L741_=(d7)<(8); 
     L69 = L741_;
    } else {
     L69 = FALSE;
    }
    if (L69) {
    }
    else {
     goto after_loop7;
    }
    L751_=INTTIMES(v7,8); 
    L76 = L751_;
    L771_=INTPLUS(L76,d7); 
    v7 = L771_;
    fetch_self8 = self;
    if (ATTR(fetch_self8,backed_up)) {
     c8 = ATTR(fetch_self8,next);
     SATTR(fetch_self8,next,ATTR(fetch_self8,last_char));
     SATTR(fetch_self8,last_char,c8);
     SATTR(fetch_self8,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self8,last_char,ATTR(fetch_self8,next));
     SATTR(fetch_self8,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
   after_loop7: ;
   L781_=(CHAR)v7; 
   res = L781_;
   break;
  case 'a': 
   res = '\7';
   fetch_self9 = self;
   if (ATTR(fetch_self9,backed_up)) {
    c9 = ATTR(fetch_self9,next);
    SATTR(fetch_self9,next,ATTR(fetch_self9,last_char));
    SATTR(fetch_self9,last_char,c9);
    SATTR(fetch_self9,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self9,last_char,ATTR(fetch_self9,next));
    SATTR(fetch_self9,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 'b': 
   res = '\b';
   fetch_self10 = self;
   if (ATTR(fetch_self10,backed_up)) {
    c10 = ATTR(fetch_self10,next);
    SATTR(fetch_self10,next,ATTR(fetch_self10,last_char));
    SATTR(fetch_self10,last_char,c10);
    SATTR(fetch_self10,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self10,last_char,ATTR(fetch_self10,next));
    SATTR(fetch_self10,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 'f': 
   res = '\f';
   fetch_self11 = self;
   if (ATTR(fetch_self11,backed_up)) {
    c12 = ATTR(fetch_self11,next);
    SATTR(fetch_self11,next,ATTR(fetch_self11,last_char));
    SATTR(fetch_self11,last_char,c12);
    SATTR(fetch_self11,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self11,last_char,ATTR(fetch_self11,next));
    SATTR(fetch_self11,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 'n': 
   res = '\n';
   fetch_self12 = self;
   if (ATTR(fetch_self12,backed_up)) {
    c13 = ATTR(fetch_self12,next);
    SATTR(fetch_self12,next,ATTR(fetch_self12,last_char));
    SATTR(fetch_self12,last_char,c13);
    SATTR(fetch_self12,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self12,last_char,ATTR(fetch_self12,next));
    SATTR(fetch_self12,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 'r': 
   res = '\r';
   fetch_self13 = self;
   if (ATTR(fetch_self13,backed_up)) {
    c14 = ATTR(fetch_self13,next);
    SATTR(fetch_self13,next,ATTR(fetch_self13,last_char));
    SATTR(fetch_self13,last_char,c14);
    SATTR(fetch_self13,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self13,last_char,ATTR(fetch_self13,next));
    SATTR(fetch_self13,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 't': 
   res = '\t';
   fetch_self14 = self;
   if (ATTR(fetch_self14,backed_up)) {
    c15 = ATTR(fetch_self14,next);
    SATTR(fetch_self14,next,ATTR(fetch_self14,last_char));
    SATTR(fetch_self14,last_char,c15);
    SATTR(fetch_self14,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self14,last_char,ATTR(fetch_self14,next));
    SATTR(fetch_self14,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case 'v': 
   res = '\v';
   fetch_self15 = self;
   if (ATTR(fetch_self15,backed_up)) {
    c16 = ATTR(fetch_self15,next);
    SATTR(fetch_self15,next,ATTR(fetch_self15,last_char));
    SATTR(fetch_self15,last_char,c16);
    SATTR(fetch_self15,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self15,last_char,ATTR(fetch_self15,next));
    SATTR(fetch_self15,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case '\\': 
   res = '\\';
   fetch_self16 = self;
   if (ATTR(fetch_self16,backed_up)) {
    c17 = ATTR(fetch_self16,next);
    SATTR(fetch_self16,next,ATTR(fetch_self16,last_char));
    SATTR(fetch_self16,last_char,c17);
    SATTR(fetch_self16,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self16,last_char,ATTR(fetch_self16,next));
    SATTR(fetch_self16,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case '\'': 
   res = '\'';
   fetch_self17 = self;
   if (ATTR(fetch_self17,backed_up)) {
    c18 = ATTR(fetch_self17,next);
    SATTR(fetch_self17,next,ATTR(fetch_self17,last_char));
    SATTR(fetch_self17,last_char,c18);
    SATTR(fetch_self17,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self17,last_char,ATTR(fetch_self17,next));
    SATTR(fetch_self17,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  case '\"': 
   res = '\"';
   fetch_self18 = self;
   if (ATTR(fetch_self18,backed_up)) {
    c19 = ATTR(fetch_self18,next);
    SATTR(fetch_self18,next,ATTR(fetch_self18,last_char));
    SATTR(fetch_self18,last_char,c19);
    SATTR(fetch_self18,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self18,last_char,ATTR(fetch_self18,next));
    SATTR(fetch_self18,next,SFILE_1512288010(SFILE_ID_zero));
   }
   break;
  default: ;
   res = ATTR(self,next);
   fetch_self19 = self;
   if (ATTR(fetch_self19,backed_up)) {
    c20 = ATTR(fetch_self19,next);
    SATTR(fetch_self19,next,ATTR(fetch_self19,last_char));
    SATTR(fetch_self19,last_char,c20);
    SATTR(fetch_self19,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self19,last_char,ATTR(fetch_self19,next));
    SATTR(fetch_self19,next,SFILE_1512288010(SFILE_ID_zero));
   }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

CHAR SFILE_1512288010(SFILE_ID self) {
 CHAR ret_val = CHAR_zero;
 CHAR ch = CHAR_zero;
 CHAR L11 = CHAR_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 INT L2;
 INT L3;
 BOOL L41_;
 INT L5;
 INT L61_;
 CHAR L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 L2 = SFILE_ID_pos;
 L3 = FSTR_sizerINT(SFILE_ID_source);
 L41_=(L2)<(L3); 
 if (L41_) {
  if (SFILE_ID_newline) {
   L5 = SFILE_ID_line;
   L61_=INTPLUS(L5,1); 
   SFILE_ID_line = L61_;
   SFILE_1796395955 = SFILE_ID_pos;
   SFILE_ID_newline = FALSE;
   SFILE_ID_column = 1;
  }
  aget_self = SFILE_ID_source;
  aget_ind = SFILE_ID_pos;
  L71_=ARR((FSTR)aget_self,aget_ind); 
  ret_val1 = L71_;
  ch = ret_val1;
  L8 = SFILE_ID_pos;
  L91_=INTPLUS(L8,1); 
  SFILE_ID_pos = L91_;
  L11 = ch;
  switch (L11) {
   case '\t': 
    L10 = SFILE_ID_column;
    L121_=INTPLUS(L10,8); 
    L13 = L121_;
    L14 = SFILE_ID_column;
    L151_=INTMINUS(L14,1); 
    L16 = L151_;
    L171_=INTMOD(L16,8); 
    L18 = L171_;
    L191_=INTMINUS(L13,L18); 
    SFILE_ID_column = L191_;
    break;
   case '\0': 
    ch = ' ';
    break;
   case '\n': 
    SFILE_ID_newline = TRUE;
    break;
   default: ;
    SFILE_ID_newline = FALSE;
    L20 = SFILE_ID_column;
    L211_=INTPLUS(L20,1); 
    SFILE_ID_column = L211_;
  }
  ret_val = ch;
  return ret_val;
 }
 else {
  ret_val = SFILE_446927505;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT SFILE_1102877860(SFILE_ID self) {
 INT ret_val = INT_zero;
 INT i = INT_zero;
 INT l = INT_zero;
 INT p = INT_zero;
 FLISTINT aget_self;
 INT aget_ind = INT_zero;
 INT ret_val1 = INT_zero;
 FLISTINT aget_self1;
 INT aget_ind1 = INT_zero;
 INT ret_val2 = INT_zero;
 OUT create_self;
 OUT ret_val3;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val4;
 FILE1 r;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L101_;
 INT L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 INT L151_;
 INT L161_;
 extern STR compil1635961390;
 FILE1 L17;
 OB L18;
 L1 = (SFILE_ID_lines==((FLISTINT) NULL));
 L21_=!(L1); 
 if (L21_) {
  i = 0;
  L3 = self.loc;
  L4 = SFILE_ID_B;
  L51_=INTDIV(L3,L4); 
  l = L51_;
  p = -1;
  while (1) {
   L6 = FLISTI1321626366(SFILE_ID_lines);
   L71_=(i)<(L6); 
   if (L71_) {
   }
   else {
    goto after_loop;
   }
   L91_=(p)<(l); 
   if (L91_) {
    aget_self = SFILE_ID_lines;
    aget_ind = i;
    L101_=ARR((FLISTINT)aget_self,aget_ind); 
    ret_val1 = L101_;
    L11 = ret_val1;
    L121_=(L11)<(l); 
    L13 = L121_;
    L141_=!(L13); 
    L8 = L141_;
   } else {
    L8 = FALSE;
   }
   if (L8) {
    goto after_loop;
   }
   aget_self1 = SFILE_ID_lines;
   aget_ind1 = i;
   L151_=ARR((FLISTINT)aget_self1,aget_ind1); 
   ret_val2 = L151_;
   p = ret_val2;
   L161_=INTPLUS(i,1); 
   i = L161_;
  }
  after_loop: ;
  ret_val = i;
  return ret_val;
 }
 else {
  create_self = ((OUT) NULL);
  ret_val3 = create_self;
  plus_self = ret_val3;
  plus_s = ((STR) &compil1635961390);
  stdout_self = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r = L17;
  
  SATTR(r,fp,stdout);
  ret_val4 = r;
  FILE_plus_STR(ret_val4, plus_s);
  ret_val = 0;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT SFILE_2070755978(SFILE_ID self) {
 INT ret_val = INT_zero;
 INT i = INT_zero;
 FLISTINT aget_self;
 INT aget_ind = INT_zero;
 INT ret_val1 = INT_zero;
 BOOL L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L12;
 INT L131_;
 i = SFILE_1102877860(self);
 L11_=(0)<(i); 
 if (L11_) {
  L2 = self.loc;
  L3 = SFILE_ID_B;
  L41_=INTDIV(L2,L3); 
  L5 = L41_;
  aget_self = SFILE_ID_lines;
  L61_=INTMINUS(i,1); 
  aget_ind = L61_;
  L71_=ARR((FLISTINT)aget_self,aget_ind); 
  ret_val1 = L71_;
  L8 = ret_val1;
  L91_=INTMINUS(L5,L8); 
  ret_val = L91_;
  return ret_val;
 }
 else {
  L10 = self.loc;
  L12 = SFILE_ID_B;
  L131_=INTDIV(L10,L12); 
  ret_val = L131_;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT SFILE_29532291(SFILE_ID self) {
 INT ret_val = INT_zero;
 ret_val = INT_hashrINT(self.loc);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INTI1 SCANNE1500285552(SCANNER self, INT base1) {
 INTI1 ret_val;
 INTI1 b1;
 INTI1 x;
 INT d = INT_zero;
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c11 = CHAR_zero;
 CHAR L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 INTI1 L8;
 b1 = INTI_c1588968505(((INTI1) NULL), base1);
 x = INTI_c1588968505(((INTI1) NULL), 0);
 while (1) {
  d = CHAR_h840494345(ATTR(self,next));
  L1 = ATTR(self,next);
  L21_=L1=='_'; 
  if (L21_) {
   fetch_self = self;
   if (ATTR(fetch_self,backed_up)) {
    c = ATTR(fetch_self,next);
    SATTR(fetch_self,next,ATTR(fetch_self,last_char));
    SATTR(fetch_self,last_char,c);
    SATTR(fetch_self,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self,last_char,ATTR(fetch_self,next));
    SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
   }
  }
  else {
   L41_=(d)<(0); 
   L5 = L41_;
   L61_=!(L5); 
   if (L61_) {
    L71_=(d)<(base1); 
    L3 = L71_;
   } else {
    L3 = FALSE;
   }
   if (L3) {
    fetch_self1 = self;
    if (ATTR(fetch_self1,backed_up)) {
     c11 = ATTR(fetch_self1,next);
     SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
     SATTR(fetch_self1,last_char,c11);
     SATTR(fetch_self1,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
     SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L8 = INTI_t1692605406(x, b1);
    x = INTI_p1769163857(L8, INTI_c1588968505(((INTI1) NULL), d));
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 ret_val = x;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SCANNER SCANNE1298383531(SCANNER self, PROG p, STR file1, BOOL pSather) {
 SCANNER ret_val;
 SCANNER res = ((SCANNER) NULL);
 SCANNER L1;
 OB L2;
 if (SFILE_2094878322(SFILE_ID_zero, p, file1)) {
  L2=ZALLOC(sizeof(struct SCANNER_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=SCANNER_tag;
  L1 = ((SCANNER) L2);
  res = L1;
  SATTR(res,prog,p);
  SATTR(res,next,SFILE_1512288010(SFILE_ID_zero));
  SATTR(res,buf1,FSTR_c1998740379(((FSTR) NULL), 256));
  SATTR(res,backed_up,FALSE);
  SATTR(res,pSather,pSather);
  SATTR(res,zones,ATTR(p,zones));
 }
 else {
  res = ((SCANNER) NULL);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID SFILE_1594766561(SFILE_ID self) {
 SFILE_ID ret_val = SFILE_ID_zero;
 INT c = INT_zero;
 SFILE_ID create_self = SFILE_ID_zero;
 INT create_loc = INT_zero;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 SFILE_ID r = SFILE_ID_zero;
 SFILE_ID no_location_self = SFILE_ID_zero;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 SFILE_ID create_self1 = SFILE_ID_zero;
 INT create_loc1 = INT_zero;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 SFILE_ID r1 = SFILE_ID_zero;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 L1 = (SFILE_ID_source==((FSTR) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = SFILE_ID_column;
  L41_=INTMINUS(L3,2); 
  L5 = L41_;
  L6 = SFILE_ID_B;
  L71_=INTMINUS(L6,1); 
  L8 = L71_;
  L91_=(L5<L8)?L5:L8; 
  L10 = L91_;
  L111_=(L10>1)?L10:1; 
  c = L111_;
  create_self = SFILE_ID_zero;
  L12 = SFILE_ID_line;
  L13 = SFILE_ID_B;
  L141_=INTTIMES(L12,L13); 
  L15 = L141_;
  L161_=INTPLUS(L15,c); 
  create_loc = L161_;
  r.loc = create_loc;
  ret_val1 = r;
  ret_val = ret_val1;
  return ret_val;
 }
 else {
  no_location_self = self;
  create_self1 = SFILE_ID_zero;
  create_loc1 = -1;
  r1.loc = create_loc1;
  ret_val3 = r1;
  ret_val2 = ret_val3;
  ret_val = ret_val2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR SFILE_ID_strrSTR(SFILE_ID self) {
 STR ret_val;
 SFILE_ID file_in_self = SFILE_ID_zero;
 STR ret_val1;
 FLISTSTR aget_self;
 INT aget_ind = INT_zero;
 STR ret_val2;
 STR plus_self;
 CHAR plus_arg = CHAR_zero;
 STR ret_val3;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val4;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val5;
 STR r;
 STR plus_self1;
 INT plus_arg1 = INT_zero;
 STR ret_val6;
 STR s1;
 INT str_self1 = INT_zero;
 STR ret_val7;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val8;
 FSTR str_self2;
 STR ret_val9;
 STR plus_self2;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val10;
 STR s2;
 CHAR str_self3 = CHAR_zero;
 STR ret_val11;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val12;
 STR r1;
 SFILE_ID col_num_in_self = SFILE_ID_zero;
 INT ret_val13 = INT_zero;
 STR plus_self3;
 INT plus_arg3 = INT_zero;
 STR ret_val14;
 STR s3;
 INT str_self4 = INT_zero;
 STR ret_val15;
 FSTR buf2 = ((FSTR) NULL);
 FSTR clear_self1;
 INT str_in_self1 = INT_zero;
 FSTR str_in_s1;
 FSTR ret_val16;
 FSTR str_self5;
 STR ret_val17;
 INT L1;
 BOOL L21_;
 extern STR atunkn223829240;
 STR L31_;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 BOOL L9;
 BOOL L101_;
 STR L11;
 INT L12;
 OB L13;
 INT L14;
 INT L16;
 INT L17;
 INT L181_;
 BOOL L19;
 BOOL L201_;
 L1 = self.loc;
 L21_=(L1)==(-1); 
 if (L21_) {
  ret_val = ((STR) &atunkn223829240);
  return ret_val;
 }
 else {
  file_in_self = self;
  aget_self = SFILE_ID_files;
  aget_ind = SFILE_1102877860(file_in_self);
  L31_=(STR)ARR((FLISTSTR)aget_self,aget_ind); 
  ret_val2 = L31_;
  ret_val1 = ret_val2;
  plus_self = ret_val1;
  plus_arg = ':';
  str_self = plus_arg;
  create_self = ((STR) NULL);
  create_c = str_self;
  L5 = 1;
  L7=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L5)*sizeof(CHAR);
  L6=ZALLOC_LEAF_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  memset(L6,0,L7);
  ((OB)L6)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((STR) L6);
  L4->asize = L5;
  r = L4;
  SARR((STR)r,0,create_c); 
  ;
  ret_val5 = r;
  ret_val4 = ret_val5;
  s = ret_val4;
  ret_val3 = STR_ap1077157958(plus_self, s, TRUE);
  plus_self1 = ret_val3;
  plus_arg1 = SFILE_2070755978(self);
  str_self1 = plus_arg1;
  clear_self = buf1;
  L9 = (clear_self==((FSTR) NULL));
  L101_=!(L9); 
  if (L101_) {
   SATTR(clear_self,loc,0);
  }
  str_in_self = str_self1;
  str_in_s = buf1;
  ret_val8 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
  buf1 = ret_val8;
  str_self2 = buf1;
  ret_val9 = STR_fr1097270334(((STR) NULL), str_self2);
  ret_val7 = ret_val9;
  s1 = ret_val7;
  ret_val6 = STR_ap1077157958(plus_self1, s1, TRUE);
  plus_self2 = ret_val6;
  plus_arg2 = ':';
  str_self3 = plus_arg2;
  create_self1 = ((STR) NULL);
  create_c1 = str_self3;
  L12 = 1;
  L14=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L12)*sizeof(CHAR);
  L13=ZALLOC_LEAF_BIG(L14);
  if (L13==NULL) FATAL("Unable to allocate more memory");
  memset(L13,0,L14);
  ((OB)L13)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L13)->header.destroyed=0;
#endif

  L11 = ((STR) L13);
  L11->asize = L12;
  r1 = L11;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val12 = r1;
  ret_val11 = ret_val12;
  s2 = ret_val11;
  ret_val10 = STR_ap1077157958(plus_self2, s2, TRUE);
  plus_self3 = ret_val10;
  col_num_in_self = self;
  L16 = col_num_in_self.loc;
  L17 = SFILE_ID_B;
  L181_=INTMOD(L16,L17); 
  ret_val13 = L181_;
  plus_arg3 = ret_val13;
  str_self4 = plus_arg3;
  clear_self1 = buf2;
  L19 = (clear_self1==((FSTR) NULL));
  L201_=!(L19); 
  if (L201_) {
   SATTR(clear_self1,loc,0);
  }
  str_in_self1 = str_self4;
  str_in_s1 = buf2;
  ret_val16 = INT_st367594495(str_in_self1, str_in_s1, 0, 10, ' ');
  buf2 = ret_val16;
  str_self5 = buf2;
  ret_val17 = STR_fr1097270334(((STR) NULL), str_self5);
  ret_val15 = ret_val17;
  s3 = ret_val15;
  ret_val14 = STR_ap1077157958(plus_self3, s3, TRUE);
  ret_val = ret_val14;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR TOKEN_strrSTR(TOKEN self) {
 STR ret_val;
 STR res = ((STR) NULL);
 INT L11 = INT_zero;
 extern STR endoffile;
 extern STR nullcharacter;
 extern STR anidentifier;
 extern STR anabst713625283;
 extern STR and2;
 extern STR assert2;
 extern STR attr2;
 extern STR breakb;
 extern STR case2;
 extern STR class2;
 extern STR const2;
 extern STR else2;
 extern STR elsif1;
 extern STR end4;
 extern STR exception2;
 extern STR external3;
 extern STR false1;
 extern STR if2;
 extern STR include2;
 extern STR initial2;
 extern STR is2;
 extern STR ITER1;
 extern STR loop1;
 extern STR new2;
 extern STR or1;
 extern STR post1;
 extern STR pre1;
 extern STR private2;
 extern STR protect1;
 extern STR quit2;
 extern STR raise2;
 extern STR readonly1;
 extern STR return3;
 extern STR ROUT2;
 extern STR F_ROUT2;
 extern STR SAME1;
 extern STR self2;
 extern STR shared2;
 extern STR stub2;
 extern STR then2;
 extern STR true1;
 extern STR type1;
 extern STR typecase1;
 extern STR abstract2;
 extern STR immutable2;
 extern STR value1;
 extern STR partial2;
 extern STR void2;
 extern STR when2;
 extern STR whileb;
 extern STR untilb;
 extern STR yield2;
 extern STR anintegerliteral;
 extern STR afloat1178951116;
 extern STR astringliteral;
 extern STR achara2073972235;
 extern STR aleftparenthesis;
 extern STR aright1741926459;
 extern STR aleftbracket;
 extern STR arightbracket;
 extern STR aleftbrace;
 extern STR arightbrace;
 extern STR acomma;
 extern STR adot;
 extern STR asemicolon;
 extern STR acolon;
 extern STR anunderscore_;
 extern STR aplus;
 extern STR aminus;
 extern STR anasterisk;
 extern STR aslash;
 extern STR alessthan;
 extern STR agreaterthan;
 extern STR asharp;
 extern STR anexcl119280930;
 extern STR anexcl119280930;
 extern STR acarat;
 extern STR apercent;
 extern STR averticalbar;
 extern STR anotequals;
 extern STR alesst1919458011;
 extern STR agreat925623686;
 extern STR anassignoperator;
 extern STR atransform;
 extern STR aequals;
 extern STR anot;
 extern STR adoublecolon;
 extern STR out2;
 extern STR inout1;
 extern STR once1;
 extern STR bind2;
 extern STR anattach;
 extern STR fork2;
 extern STR lock2;
 extern STR unlock1;
 extern STR with2;
 extern STR anat;
 extern STR here2;
 extern STR where2;
 extern STR near2;
 extern STR far2;
 extern STR spread1;
 extern STR do2;
 extern STR par1;
 extern STR parloop1;
 extern STR any2;
 extern STR clusters2;
 extern STR clustersb;
 extern STR cluster_size1;
 extern STR cohort1;
 extern STR sync2;
 extern STR guard1;
 extern STR global2;
 extern STR builtin1;
 extern STR unknowntoken;
 L11 = self.val1;
 switch (L11) {
  case 0: 
   res = ((STR) &endoffile);
   break;
  case 1: 
   res = ((STR) &nullcharacter);
   break;
  case 2: 
   res = ((STR) &anidentifier);
   break;
  case 3: 
   res = ((STR) &anabst713625283);
   break;
  case 4: 
   res = ((STR) &and2);
   break;
  case 5: 
   res = ((STR) &assert2);
   break;
  case 6: 
   res = ((STR) &attr2);
   break;
  case 7: 
   res = ((STR) &breakb);
   break;
  case 8: 
   res = ((STR) &case2);
   break;
  case 9: 
   res = ((STR) &class2);
   break;
  case 10: 
   res = ((STR) &const2);
   break;
  case 11: 
   res = ((STR) &else2);
   break;
  case 12: 
   res = ((STR) &elsif1);
   break;
  case 13: 
   res = ((STR) &end4);
   break;
  case 14: 
   res = ((STR) &exception2);
   break;
  case 15: 
   res = ((STR) &external3);
   break;
  case 16: 
   res = ((STR) &false1);
   break;
  case 17: 
   res = ((STR) &if2);
   break;
  case 18: 
   res = ((STR) &include2);
   break;
  case 19: 
   res = ((STR) &initial2);
   break;
  case 20: 
   res = ((STR) &is2);
   break;
  case 21: 
   res = ((STR) &ITER1);
   break;
  case 22: 
   res = ((STR) &loop1);
   break;
  case 23: 
   res = ((STR) &new2);
   break;
  case 24: 
   res = ((STR) &or1);
   break;
  case 25: 
   res = ((STR) &post1);
   break;
  case 26: 
   res = ((STR) &pre1);
   break;
  case 27: 
   res = ((STR) &private2);
   break;
  case 28: 
   res = ((STR) &protect1);
   break;
  case 29: 
   res = ((STR) &quit2);
   break;
  case 30: 
   res = ((STR) &raise2);
   break;
  case 31: 
   res = ((STR) &readonly1);
   break;
  case 33: 
   res = ((STR) &return3);
   break;
  case 34: 
   res = ((STR) &ROUT2);
   break;
  case 35: 
   res = ((STR) &F_ROUT2);
   break;
  case 36: 
   res = ((STR) &SAME1);
   break;
  case 37: 
   res = ((STR) &self2);
   break;
  case 38: 
   res = ((STR) &shared2);
   break;
  case 48: 
   res = ((STR) &stub2);
   break;
  case 39: 
   res = ((STR) &then2);
   break;
  case 40: 
   res = ((STR) &true1);
   break;
  case 41: 
   res = ((STR) &type1);
   break;
  case 42: 
   res = ((STR) &typecase1);
   break;
  case 44: 
   res = ((STR) &abstract2);
   break;
  case 45: 
   res = ((STR) &immutable2);
   break;
  case 46: 
   res = ((STR) &value1);
   break;
  case 47: 
   res = ((STR) &partial2);
   break;
  case 49: 
   res = ((STR) &void2);
   break;
  case 50: 
   res = ((STR) &when2);
   break;
  case 51: 
   res = ((STR) &whileb);
   break;
  case 43: 
   res = ((STR) &untilb);
   break;
  case 52: 
   res = ((STR) &yield2);
   break;
  case 53: 
   res = ((STR) &anintegerliteral);
   break;
  case 54: 
   res = ((STR) &afloat1178951116);
   break;
  case 55: 
   res = ((STR) &astringliteral);
   break;
  case 56: 
   res = ((STR) &achara2073972235);
   break;
  case 57: 
   res = ((STR) &aleftparenthesis);
   break;
  case 58: 
   res = ((STR) &aright1741926459);
   break;
  case 59: 
   res = ((STR) &aleftbracket);
   break;
  case 60: 
   res = ((STR) &arightbracket);
   break;
  case 61: 
   res = ((STR) &aleftbrace);
   break;
  case 62: 
   res = ((STR) &arightbrace);
   break;
  case 63: 
   res = ((STR) &acomma);
   break;
  case 64: 
   res = ((STR) &adot);
   break;
  case 65: 
   res = ((STR) &asemicolon);
   break;
  case 66: 
   res = ((STR) &acolon);
   break;
  case 67: 
   res = ((STR) &anunderscore_);
   break;
  case 68: 
   res = ((STR) &aplus);
   break;
  case 69: 
   res = ((STR) &aminus);
   break;
  case 70: 
   res = ((STR) &anasterisk);
   break;
  case 71: 
   res = ((STR) &aslash);
   break;
  case 72: 
   res = ((STR) &alessthan);
   break;
  case 73: 
   res = ((STR) &agreaterthan);
   break;
  case 74: 
   res = ((STR) &asharp);
   break;
  case 75: 
   res = ((STR) &anexcl119280930);
   break;
  case 76: 
   res = ((STR) &anexcl119280930);
   break;
  case 77: 
   res = ((STR) &acarat);
   break;
  case 78: 
   res = ((STR) &apercent);
   break;
  case 79: 
   res = ((STR) &averticalbar);
   break;
  case 80: 
   res = ((STR) &anotequals);
   break;
  case 81: 
   res = ((STR) &alesst1919458011);
   break;
  case 82: 
   res = ((STR) &agreat925623686);
   break;
  case 83: 
   res = ((STR) &anassignoperator);
   break;
  case 85: 
   res = ((STR) &atransform);
   break;
  case 86: 
   res = ((STR) &aequals);
   break;
  case 87: 
   res = ((STR) &anot);
   break;
  case 84: 
   res = ((STR) &adoublecolon);
   break;
  case 88: 
   res = ((STR) &out2);
   break;
  case 89: 
   res = ((STR) &inout1);
   break;
  case 90: 
   res = ((STR) &once1);
   break;
  case 91: 
   res = ((STR) &bind2);
   break;
  case 98: 
   res = ((STR) &anattach);
   break;
  case 94: 
   res = ((STR) &fork2);
   break;
  case 99: 
   res = ((STR) &lock2);
   break;
  case 100: 
   res = ((STR) &unlock1);
   break;
  case 104: 
   res = ((STR) &with2);
   break;
  case 110: 
   res = ((STR) &anat);
   break;
  case 111: 
   res = ((STR) &here2);
   break;
  case 112: 
   res = ((STR) &where2);
   break;
  case 105: 
   res = ((STR) &near2);
   break;
  case 106: 
   res = ((STR) &far2);
   break;
  case 107: 
   res = ((STR) &spread1);
   break;
  case 95: 
   res = ((STR) &do2);
   break;
  case 96: 
   res = ((STR) &par1);
   break;
  case 97: 
   res = ((STR) &parloop1);
   break;
  case 108: 
   res = ((STR) &any2);
   break;
  case 92: 
   res = ((STR) &clusters2);
   break;
  case 109: 
   res = ((STR) &clustersb);
   break;
  case 93: 
   res = ((STR) &cluster_size1);
   break;
  case 101: 
   res = ((STR) &cohort1);
   break;
  case 102: 
   res = ((STR) &sync2);
   break;
  case 103: 
   res = ((STR) &guard1);
   break;
  case 113: 
   res = ((STR) &global2);
   break;
  case 114: 
   res = ((STR) &builtin1);
   break;
  default: ;
   res = ((STR) &unknowntoken);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TOKEN SCANNE1018171979(SCANNER self) {
 TOKEN ret_val = TOKEN_zero;
 CHAR c = CHAR_zero;
 INT res = INT_zero;
 CHAR L11 = CHAR_zero;
 FSTR L21;
 FSTR L31;
 FSTR L41;
 CHAR nextc = CHAR_zero;
 FSTR L51;
 FSTR L61;
 FSTR L71;
 FSTR L81;
 FSTR L91;
 FSTR L101;
 FSTR L121;
 FSTR L131;
 FSTR L141;
 FSTR L151;
 FSTR L161;
 FSTR L171;
 FSTR L181;
 FSTR L191;
 FSTR L201;
 FSTR clear_self;
 SCANNER fetch_self;
 CHAR c11 = CHAR_zero;
 FSTR aget_self;
 INT aget_ind = INT_zero;
 CHAR ret_val1 = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c2 = CHAR_zero;
 SCANNER fetch_self2;
 CHAR c3 = CHAR_zero;
 SCANNER fetch_self3;
 CHAR c4 = CHAR_zero;
 SCANNER fetch_self4;
 CHAR c5 = CHAR_zero;
 SCANNER add_buf_to_sym_t;
 FSTR str_self;
 STR ret_val2;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val3 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 BOOL L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 CHAR L261_;
 extern STR abstract1;
 extern STR attr1;
 extern STR and1;
 extern STR assert1;
 extern STR any1;
 BOOL L27;
 BOOL L28;
 BOOL L291_;
 BOOL L30;
 extern STR break2;
 CHAR L32;
 BOOL L331_;
 extern STR builtin;
 extern STR bind1;
 extern STR const1;
 extern STR class1;
 extern STR case1;
 extern STR cohort;
 extern STR cluster_size;
 extern STR clusters1;
 BOOL L34;
 BOOL L35;
 BOOL L36;
 BOOL L371_;
 BOOL L381_;
 extern STR do1;
 extern STR end3;
 extern STR elsif;
 extern STR else1;
 extern STR exception1;
 extern STR external2;
 extern STR false;
 extern STR far1;
 BOOL L39;
 BOOL L40;
 BOOL L421_;
 extern STR fork11;
 extern STR F_ROUT1;
 extern STR guard;
 extern STR global1;
 BOOL L43;
 extern STR here1;
 BOOL L44;
 extern STR is1;
 extern STR if1;
 extern STR inout;
 extern STR include1;
 extern STR immutable1;
 extern STR initial1;
 extern STR ITER;
 extern STR loop;
 extern STR lock1;
 extern STR new1;
 extern STR near1;
 BOOL L45;
 BOOL L46;
 BOOL L471_;
 extern STR or;
 extern STR once;
 extern STR out1;
 extern STR pre;
 extern STR private1;
 extern STR post;
 extern STR protect;
 extern STR partial1;
 extern STR par;
 extern STR parloop;
 extern STR quit1;
 extern STR return2;
 extern STR raise1;
 extern STR readonly;
 extern STR result1;
 extern STR ROUT1;
 extern STR self1;
 extern STR shared1;
 extern STR spread;
 BOOL L48;
 BOOL L49;
 BOOL L501_;
 extern STR stub1;
 extern STR sync1;
 extern STR SAME;
 extern STR then1;
 extern STR true1;
 extern STR typecase;
 extern STR type1;
 extern STR until1;
 CHAR L52;
 BOOL L531_;
 extern STR unlock;
 extern STR void1;
 extern STR value;
 extern STR when1;
 extern STR while1;
 CHAR L54;
 BOOL L551_;
 extern STR where1;
 BOOL L56;
 extern STR with1;
 BOOL L57;
 BOOL L58;
 BOOL L591_;
 extern STR yield1;
 INT L60;
 BOOL L621_;
 clear_self = ATTR(self,buf1);
 L22 = (clear_self==((FSTR) NULL));
 L231_=!(L22); 
 if (L231_) {
  SATTR(clear_self,loc,0);
 }
 while (1) {
  c = ATTR(self,next);
  if (CHAR_i1942379470(c)) {
   L24 = TRUE;
  } else {
   L251_=c=='_'; 
   L24 = L251_;
  }
  if (L24) {
  }
  else {
   goto after_loop;
  }
  SATTR(self,buf1,FSTR_p399773021(ATTR(self,buf1), c));
  fetch_self = self;
  if (ATTR(fetch_self,backed_up)) {
   c11 = ATTR(fetch_self,next);
   SATTR(fetch_self,next,ATTR(fetch_self,last_char));
   SATTR(fetch_self,last_char,c11);
   SATTR(fetch_self,backed_up,FALSE);
  }
  else {
   SATTR(fetch_self,last_char,ATTR(fetch_self,next));
   SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
  }
 }
 after_loop: ;
 res = SCANNE589883380;
 aget_self = ATTR(self,buf1);
 aget_ind = 0;
 L261_=ARR((FSTR)aget_self,aget_ind); 
 ret_val1 = L261_;
 L11 = ret_val1;
 switch (L11) {
  case 'a': 
   L21 = ATTR(self,buf1);
   if (FSTR_i104531942(L21, ((STR) &abstract1))) {
    res = SCANNE951940802;
   }
   else {
    if (FSTR_i104531942(L21, ((STR) &attr1))) {
     res = SCANNER_attr_tok;
    }
    else {
     if (FSTR_i104531942(L21, ((STR) &and1))) {
      res = SCANNER_and_tok;
     }
     else {
      if (FSTR_i104531942(L21, ((STR) &assert1))) {
       res = SCANNE687089717;
      }
      else {
       if (FSTR_i104531942(L21, ((STR) &any1))) {
        if (ATTR(self,pSather)) {
         L28 = ATTR(self,zones);
         L291_=!(L28); 
         L27 = L291_;
        } else {
         L27 = FALSE;
        }
        if (L27) {
         res = SCANNER_any_tok;
        }
       }
      }
     }
    }
   }
   break;
  case 'b': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &break2))) {
    L32 = ATTR(self,next);
    L331_=L32=='!'; 
    L30 = L331_;
   } else {
    L30 = FALSE;
   }
   if (L30) {
    fetch_self1 = self;
    if (ATTR(fetch_self1,backed_up)) {
     c2 = ATTR(fetch_self1,next);
     SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
     SATTR(fetch_self1,last_char,c2);
     SATTR(fetch_self1,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
     SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE661829813;
   }
   L31 = ATTR(self,buf1);
   if (FSTR_i104531942(L31, ((STR) &builtin))) {
    res = SCANNE560979044;
   }
   else {
    if (FSTR_i104531942(L31, ((STR) &bind1))) {
     res = SCANNER_bind_tok;
    }
   }
   break;
  case 'c': 
   L41 = ATTR(self,buf1);
   if (FSTR_i104531942(L41, ((STR) &const1))) {
    res = SCANNE2139920629;
   }
   else {
    if (FSTR_i104531942(L41, ((STR) &class1))) {
     res = SCANNE304035056;
    }
    else {
     if (FSTR_i104531942(L41, ((STR) &case1))) {
      res = SCANNER_case_tok;
     }
     else {
      if (FSTR_i104531942(L41, ((STR) &cohort))) {
       res = SCANNE438656140;
      }
      else {
       if (FSTR_i104531942(L41, ((STR) &cluster_size))) {
        res = SCANNE1099722568;
       }
       else {
        if (FSTR_i104531942(L41, ((STR) &clusters1))) {
         res = SCANNE1499824177;
         nextc = ATTR(self,next);
         if (ATTR(self,pSather)) {
          L36 = ATTR(self,zones);
          L371_=!(L36); 
          L35 = L371_;
         } else {
          L35 = FALSE;
         }
         if (L35) {
          L381_=nextc=='!'; 
          L34 = L381_;
         } else {
          L34 = FALSE;
         }
         if (L34) {
          fetch_self2 = self;
          if (ATTR(fetch_self2,backed_up)) {
           c3 = ATTR(fetch_self2,next);
           SATTR(fetch_self2,next,ATTR(fetch_self2,last_char));
           SATTR(fetch_self2,last_char,c3);
           SATTR(fetch_self2,backed_up,FALSE);
          }
          else {
           SATTR(fetch_self2,last_char,ATTR(fetch_self2,next));
           SATTR(fetch_self2,next,SFILE_1512288010(SFILE_ID_zero));
          }
          res = SCANNE1021322009;
         }
        }
       }
      }
     }
    }
   }
   break;
  case 'd': 
   L51 = ATTR(self,buf1);
   if (FSTR_i104531942(L51, ((STR) &do1))) {
    res = SCANNER_do_tok;
   }
   break;
  case 'e': 
   L61 = ATTR(self,buf1);
   if (FSTR_i104531942(L61, ((STR) &end3))) {
    res = SCANNER_end_tok;
   }
   else {
    if (FSTR_i104531942(L61, ((STR) &elsif))) {
     res = SCANNE766559877;
    }
    else {
     if (FSTR_i104531942(L61, ((STR) &else1))) {
      res = SCANNER_else_tok;
     }
     else {
      if (FSTR_i104531942(L61, ((STR) &exception1))) {
       res = SCANNE114307641;
      }
      else {
       if (FSTR_i104531942(L61, ((STR) &external2))) {
        res = SCANNE754612661;
       }
      }
     }
    }
   }
   break;
  case 'f': 
   L71 = ATTR(self,buf1);
   if (FSTR_i104531942(L71, ((STR) &false))) {
    res = SCANNE463806109;
   }
   else {
    if (FSTR_i104531942(L71, ((STR) &far1))) {
     if (ATTR(self,pSather)) {
      L40 = ATTR(self,zones);
      L421_=!(L40); 
      L39 = L421_;
     } else {
      L39 = FALSE;
     }
     if (L39) {
      res = SCANNER_far_tok;
     }
    }
    else {
     if (FSTR_i104531942(L71, ((STR) &fork11))) {
      res = SCANNER_fork_tok;
     }
    }
   }
   break;
  case 'F': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &F_ROUT1))) {
    res = SCANNE1284590324;
   }
   break;
  case 'g': 
   L81 = ATTR(self,buf1);
   if (FSTR_i104531942(L81, ((STR) &guard))) {
    if (ATTR(self,pSather)) {
     res = SCANNE105624607;
    }
   }
   else {
    if (FSTR_i104531942(L81, ((STR) &global1))) {
     if (ATTR(self,zones)) {
      res = SCANNE1233467974;
     }
    }
   }
   break;
  case 'h': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &here1))) {
    if (ATTR(self,pSather)) {
     L44 = TRUE;
    } else {
     L44 = ATTR(self,zones);
    }
    L43 = L44;
   } else {
    L43 = FALSE;
   }
   if (L43) {
    res = SCANNER_here_tok;
   }
   break;
  case 'i': 
   L91 = ATTR(self,buf1);
   if (FSTR_i104531942(L91, ((STR) &is1))) {
    res = SCANNER_is_tok;
   }
   else {
    if (FSTR_i104531942(L91, ((STR) &if1))) {
     res = SCANNER_if_tok;
    }
    else {
     if (FSTR_i104531942(L91, ((STR) &inout))) {
      res = SCANNE1874840845;
     }
     else {
      if (FSTR_i104531942(L91, ((STR) &include1))) {
       res = SCANNE2137779101;
      }
      else {
       if (FSTR_i104531942(L91, ((STR) &immutable1))) {
        res = SCANNE801235924;
       }
       else {
        if (FSTR_i104531942(L91, ((STR) &initial1))) {
         res = SCANNE1717311299;
        }
       }
      }
     }
    }
   }
   break;
  case 'I': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &ITER))) {
    res = SCANNER_ITER_tok;
   }
   break;
  case 'l': 
   L101 = ATTR(self,buf1);
   if (FSTR_i104531942(L101, ((STR) &loop))) {
    res = SCANNER_loop_tok;
   }
   else {
    if (FSTR_i104531942(L101, ((STR) &lock1))) {
     if (ATTR(self,pSather)) {
      res = SCANNER_lock_tok;
     }
    }
   }
   break;
  case 'n': 
   L121 = ATTR(self,buf1);
   if (FSTR_i104531942(L121, ((STR) &new1))) {
    res = SCANNER_new_tok;
   }
   else {
    if (FSTR_i104531942(L121, ((STR) &near1))) {
     if (ATTR(self,pSather)) {
      L46 = ATTR(self,zones);
      L471_=!(L46); 
      L45 = L471_;
     } else {
      L45 = FALSE;
     }
     if (L45) {
      res = SCANNER_near_tok;
     }
    }
   }
   break;
  case 'o': 
   L131 = ATTR(self,buf1);
   if (FSTR_i104531942(L131, ((STR) &or))) {
    res = SCANNER_or_tok;
   }
   else {
    if (FSTR_i104531942(L131, ((STR) &once))) {
     res = SCANNER_once_tok;
    }
    else {
     if (FSTR_i104531942(L131, ((STR) &out1))) {
      res = SCANNER_out_tok;
     }
    }
   }
   break;
  case 'p': 
   L141 = ATTR(self,buf1);
   if (FSTR_i104531942(L141, ((STR) &pre))) {
    res = SCANNER_pre_tok;
   }
   else {
    if (FSTR_i104531942(L141, ((STR) &private1))) {
     res = SCANNE1553373348;
    }
    else {
     if (FSTR_i104531942(L141, ((STR) &post))) {
      res = SCANNER_post_tok;
     }
     else {
      if (FSTR_i104531942(L141, ((STR) &protect))) {
       res = SCANNE440197020;
      }
      else {
       if (FSTR_i104531942(L141, ((STR) &partial1))) {
        res = SCANNE1733136762;
       }
       else {
        if (FSTR_i104531942(L141, ((STR) &par))) {
         res = SCANNER_par_tok;
        }
        else {
         if (FSTR_i104531942(L141, ((STR) &parloop))) {
          res = SCANNE1574122250;
         }
        }
       }
      }
     }
    }
   }
   break;
  case 'q': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &quit1))) {
    res = SCANNER_quit_tok;
   }
   break;
  case 'r': 
   L151 = ATTR(self,buf1);
   if (FSTR_i104531942(L151, ((STR) &return2))) {
    res = SCANNE943336743;
   }
   else {
    if (FSTR_i104531942(L151, ((STR) &raise1))) {
     res = SCANNE269030544;
    }
    else {
     if (FSTR_i104531942(L151, ((STR) &readonly))) {
      res = SCANNE1090219034;
     }
     else {
      if (FSTR_i104531942(L151, ((STR) &result1))) {
       res = SCANNE1014449292;
      }
     }
    }
   }
   break;
  case 'R': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &ROUT1))) {
    res = SCANNER_ROUT_tok;
   }
   break;
  case 's': 
   L161 = ATTR(self,buf1);
   if (FSTR_i104531942(L161, ((STR) &self1))) {
    res = SCANNER_self_tok;
   }
   else {
    if (FSTR_i104531942(L161, ((STR) &shared1))) {
     res = SCANNE983081836;
    }
    else {
     if (FSTR_i104531942(L161, ((STR) &spread))) {
      if (ATTR(self,pSather)) {
       L49 = ATTR(self,zones);
       L501_=!(L49); 
       L48 = L501_;
      } else {
       L48 = FALSE;
      }
      if (L48) {
       res = SCANNE1277304122;
      }
     }
     else {
      if (FSTR_i104531942(L161, ((STR) &stub1))) {
       res = SCANNER_stub_tok;
      }
      else {
       if (FSTR_i104531942(L161, ((STR) &sync1))) {
        if (ATTR(self,pSather)) {
         res = SCANNER_sync_tok;
        }
       }
      }
     }
    }
   }
   break;
  case 'S': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &SAME))) {
    res = SCANNER_SAME_tok;
   }
   break;
  case 't': 
   L171 = ATTR(self,buf1);
   if (FSTR_i104531942(L171, ((STR) &then1))) {
    res = SCANNER_then_tok;
   }
   else {
    if (FSTR_i104531942(L171, ((STR) &true1))) {
     res = SCANNER_true_tok;
    }
    else {
     if (FSTR_i104531942(L171, ((STR) &typecase))) {
      res = SCANNE392475110;
     }
     else {
      if (FSTR_i104531942(L171, ((STR) &type1))) {
       res = SCANNER_type_tok;
      }
     }
    }
   }
   break;
  case 'u': 
   L181 = ATTR(self,buf1);
   if (FSTR_i104531942(L181, ((STR) &until1))) {
    L52 = ATTR(self,next);
    L531_=L52=='!'; 
    if (L531_) {
     fetch_self3 = self;
     if (ATTR(fetch_self3,backed_up)) {
      c4 = ATTR(fetch_self3,next);
      SATTR(fetch_self3,next,ATTR(fetch_self3,last_char));
      SATTR(fetch_self3,last_char,c4);
      SATTR(fetch_self3,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self3,last_char,ATTR(fetch_self3,next));
      SATTR(fetch_self3,next,SFILE_1512288010(SFILE_ID_zero));
     }
     res = SCANNE202030842;
    }
   }
   else {
    if (FSTR_i104531942(L181, ((STR) &unlock))) {
     if (ATTR(self,pSather)) {
      res = SCANNE1785932597;
     }
    }
   }
   break;
  case 'v': 
   L191 = ATTR(self,buf1);
   if (FSTR_i104531942(L191, ((STR) &void1))) {
    res = SCANNER_void_tok;
   }
   else {
    if (FSTR_i104531942(L191, ((STR) &value))) {
     res = SCANNE1010372543;
    }
   }
   break;
  case 'w': 
   L201 = ATTR(self,buf1);
   if (FSTR_i104531942(L201, ((STR) &when1))) {
    res = SCANNER_when_tok;
   }
   else {
    if (FSTR_i104531942(L201, ((STR) &while1))) {
     L54 = ATTR(self,next);
     L551_=L54=='!'; 
     if (L551_) {
      fetch_self4 = self;
      if (ATTR(fetch_self4,backed_up)) {
       c5 = ATTR(fetch_self4,next);
       SATTR(fetch_self4,next,ATTR(fetch_self4,last_char));
       SATTR(fetch_self4,last_char,c5);
       SATTR(fetch_self4,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self4,last_char,ATTR(fetch_self4,next));
       SATTR(fetch_self4,next,SFILE_1512288010(SFILE_ID_zero));
      }
      res = SCANNE1391319271;
     }
    }
    else {
     if (FSTR_i104531942(L201, ((STR) &where1))) {
      if (ATTR(self,pSather)) {
       L56 = TRUE;
      } else {
       L56 = ATTR(self,zones);
      }
      if (L56) {
       res = SCANNE1901892399;
      }
     }
     else {
      if (FSTR_i104531942(L201, ((STR) &with1))) {
       if (ATTR(self,pSather)) {
        L58 = ATTR(self,zones);
        L591_=!(L58); 
        L57 = L591_;
       } else {
        L57 = FALSE;
       }
       if (L57) {
        res = SCANNER_with_tok;
       }
      }
     }
    }
   }
   break;
  case 'y': 
   if (FSTR_i104531942(ATTR(self,buf1), ((STR) &yield1))) {
    res = SCANNE1252648769;
   }
   break;
  default: ;
 }
 L60 = SCANNE589883380;
 L621_=(res)==(L60); 
 if (L621_) {
  add_buf_to_sym_t = self;
  str_self = ATTR(add_buf_to_sym_t,buf1);
  ret_val2 = STR_fr1097270334(((STR) NULL), str_self);
  SATTR(add_buf_to_sym_t,lex_value,IDENT_1150413730(IDENT_zero, ret_val2));
 }
 create_self = TOKEN_zero;
 create_val = res;
 t.val1 = create_val;
 ret_val3 = t;
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TOKEN SCANNE1792387248(SCANNER self) {
 TOKEN ret_val = TOKEN_zero;
 BOOL whitespace = BOOL_zero;
 INT res = INT_zero;
 CHAR L11 = CHAR_zero;
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c11 = CHAR_zero;
 SCANNER fetch_self2;
 CHAR c2 = CHAR_zero;
 SCANNER fetch_self3;
 CHAR c3 = CHAR_zero;
 SCANNER fetch_self4;
 CHAR c4 = CHAR_zero;
 SCANNER fetch_self5;
 CHAR c5 = CHAR_zero;
 SCANNER fetch_self6;
 CHAR c6 = CHAR_zero;
 SCANNER fetch_self7;
 CHAR c7 = CHAR_zero;
 SCANNER fetch_self8;
 CHAR c8 = CHAR_zero;
 SCANNER fetch_self9;
 CHAR c9 = CHAR_zero;
 SCANNER fetch_self10;
 CHAR c10 = CHAR_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 SCANNER error_self;
 STR error_msg;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 SCANNER err_self;
 STR err_s;
 SCANNER fetch_self11;
 CHAR c12 = CHAR_zero;
 SCANNER fetch_self12;
 CHAR c13 = CHAR_zero;
 SCANNER fetch_self13;
 CHAR c14 = CHAR_zero;
 SCANNER fetch_self14;
 CHAR c15 = CHAR_zero;
 SCANNER fetch_self15;
 CHAR c16 = CHAR_zero;
 SCANNER fetch_self16;
 CHAR c17 = CHAR_zero;
 SCANNER fetch_self17;
 CHAR c18 = CHAR_zero;
 SCANNER fetch_self18;
 CHAR c19 = CHAR_zero;
 SCANNER fetch_self19;
 CHAR c20 = CHAR_zero;
 SCANNER fetch_self20;
 CHAR c21 = CHAR_zero;
 SCANNER fetch_self21;
 CHAR c22 = CHAR_zero;
 SCANNER fetch_self22;
 CHAR c23 = CHAR_zero;
 SCANNER fetch_self23;
 CHAR c24 = CHAR_zero;
 SCANNER fetch_self24;
 CHAR c25 = CHAR_zero;
 SCANNER fetch_self25;
 CHAR c26 = CHAR_zero;
 SCANNER backup_self;
 CHAR c27 = CHAR_zero;
 SCANNER fetch_self26;
 CHAR c28 = CHAR_zero;
 SCANNER fetch_self27;
 CHAR c29 = CHAR_zero;
 SCANNER fetch_self28;
 CHAR c30 = CHAR_zero;
 SCANNER fetch_self29;
 CHAR c31 = CHAR_zero;
 SCANNER fetch_self30;
 CHAR c32 = CHAR_zero;
 SCANNER fetch_self31;
 CHAR c33 = CHAR_zero;
 SCANNER fetch_self32;
 CHAR c34 = CHAR_zero;
 SCANNER fetch_self33;
 CHAR c35 = CHAR_zero;
 SCANNER fetch_self34;
 CHAR c36 = CHAR_zero;
 SCANNER fetch_self35;
 CHAR c37 = CHAR_zero;
 SCANNER fetch_self36;
 CHAR c38 = CHAR_zero;
 SCANNER error_self1;
 STR error_msg1;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 SCANNER err_self1;
 STR err_s1;
 SCANNER fetch_self37;
 CHAR c39 = CHAR_zero;
 SCANNER fetch_self38;
 CHAR c40 = CHAR_zero;
 FSTR clear_self;
 FSTR plus_self1;
 CHAR plus_c = CHAR_zero;
 FSTR ret_val2;
 FSTR plus_self2;
 CHAR plus_c1 = CHAR_zero;
 FSTR ret_val3;
 SCANNER fetch_self39;
 CHAR c41 = CHAR_zero;
 SCANNER error_self2;
 STR error_msg2;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 SCANNER err_self2;
 STR err_s2;
 SCANNER fetch_self40;
 CHAR c42 = CHAR_zero;
 SCANNER fetch_self41;
 CHAR c43 = CHAR_zero;
 SCANNER add_buf_to_sym_t;
 FSTR str_self;
 STR ret_val4;
 SCANNER fetch_self42;
 CHAR c44 = CHAR_zero;
 STR plus_self3;
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s;
 CHAR str_self1 = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 STR plus_self4;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val8;
 STR s1;
 CHAR str_self2 = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val10;
 STR r1;
 SCANNER error_self3;
 STR error_msg3;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 SCANNER err_self3;
 STR err_s3;
 SCANNER fetch_self43;
 CHAR c45 = CHAR_zero;
 TOKEN create_self2 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val11 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 TOKEN L2;
 TOKEN L3;
 TOKEN L4;
 TOKEN L5;
 TOKEN L6;
 TOKEN L7;
 TOKEN L8;
 TOKEN L9;
 TOKEN L10;
 TOKEN L12;
 TOKEN L13;
 TOKEN L14;
 TOKEN L15;
 TOKEN L16;
 TOKEN L17;
 TOKEN L18;
 TOKEN L19;
 TOKEN L20;
 TOKEN L21;
 TOKEN L22;
 TOKEN L23;
 TOKEN L24;
 TOKEN L25;
 TOKEN L26;
 TOKEN L27;
 TOKEN L28;
 TOKEN L29;
 TOKEN L30;
 TOKEN L31;
 TOKEN L32;
 TOKEN L33;
 TOKEN L34;
 TOKEN L35;
 TOKEN L36;
 TOKEN L37;
 TOKEN L38;
 TOKEN L39;
 TOKEN L40;
 TOKEN L41;
 TOKEN L42;
 TOKEN L43;
 TOKEN L44;
 TOKEN L45;
 TOKEN L46;
 TOKEN L47;
 TOKEN L48;
 TOKEN L49;
 TOKEN L50;
 TOKEN L51;
 TOKEN L52;
 TOKEN L53;
 TOKEN L54;
 CHAR L55;
 BOOL L561_;
 TOKEN L57;
 BOOL L58;
 INT L59;
 BOOL L601_;
 extern STR d1;
 INT L61;
 BOOL L621_;
 BOOL L63;
 BOOL L641_;
 extern STR dwithouttypename;
 CHAR L65;
 BOOL L661_;
 BOOL L67;
 CHAR L68;
 BOOL L691_;
 CHAR L70;
 CHAR L71;
 BOOL L721_;
 CHAR L73;
 BOOL L741_;
 CHAR L75;
 BOOL L761_;
 CHAR L77;
 BOOL L781_;
 CHAR L79;
 BOOL L801_;
 CHAR L81;
 BOOL L821_;
 BOOL L83;
 CHAR L84;
 BOOL L851_;
 CHAR L86;
 BOOL L871_;
 CHAR L88;
 BOOL L891_;
 CHAR L90;
 BOOL L911_;
 CHAR L92;
 BOOL L931_;
 CHAR L94;
 BOOL L951_;
 BOOL L96;
 BOOL L971_;
 extern STR malfor1193176489;
 BOOL L98;
 BOOL L991_;
 BOOL L100;
 CHAR L101;
 BOOL L1021_;
 CHAR L103;
 BOOL L1041_;
 CHAR L105;
 BOOL L1061_;
 CHAR L107;
 BOOL L1081_;
 extern STR unterm1032969897;
 CHAR L109;
 BOOL L1101_;
 BOOL L111;
 BOOL L1121_;
 TOKEN L113;
 TOKEN L114;
 TOKEN L115;
 TOKEN L116;
 TOKEN L117;
 TOKEN L118;
 TOKEN L119;
 TOKEN L120;
 TOKEN L121;
 TOKEN L122;
 extern STR unknowncharacter;
 STR L123;
 INT L124;
 OB L125;
 INT L126;
 STR L128;
 INT L129;
 OB L130;
 INT L131;
 INT L133;
 BOOL L1341_;
 whitespace = FALSE;
 res = SCANNER_null_tok;
 while (1) {
  L11 = ATTR(self,next);
  switch (L11) {
   case '\0': 
    res = SCANNER_eof_tok;
    break;
   case '\n': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case ' ': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case '\t': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case '\f': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case '\b': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case '\r': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case '\v': 
    whitespace = TRUE;
    SCANNE1781940665(self);
    break;
   case 'a': 
    L2=SCANNE1018171979(self);
    res = L2.val1;
    break;
   case 'b': 
    L3=SCANNE1018171979(self);
    res = L3.val1;
    break;
   case 'c': 
    L4=SCANNE1018171979(self);
    res = L4.val1;
    break;
   case 'd': 
    L5=SCANNE1018171979(self);
    res = L5.val1;
    break;
   case 'e': 
    L6=SCANNE1018171979(self);
    res = L6.val1;
    break;
   case 'f': 
    L7=SCANNE1018171979(self);
    res = L7.val1;
    break;
   case 'g': 
    L8=SCANNE1018171979(self);
    res = L8.val1;
    break;
   case 'h': 
    L9=SCANNE1018171979(self);
    res = L9.val1;
    break;
   case 'i': 
    L10=SCANNE1018171979(self);
    res = L10.val1;
    break;
   case 'j': 
    L12=SCANNE1018171979(self);
    res = L12.val1;
    break;
   case 'k': 
    L13=SCANNE1018171979(self);
    res = L13.val1;
    break;
   case 'l': 
    L14=SCANNE1018171979(self);
    res = L14.val1;
    break;
   case 'm': 
    L15=SCANNE1018171979(self);
    res = L15.val1;
    break;
   case 'n': 
    L16=SCANNE1018171979(self);
    res = L16.val1;
    break;
   case 'o': 
    L17=SCANNE1018171979(self);
    res = L17.val1;
    break;
   case 'p': 
    L18=SCANNE1018171979(self);
    res = L18.val1;
    break;
   case 'q': 
    L19=SCANNE1018171979(self);
    res = L19.val1;
    break;
   case 'r': 
    L20=SCANNE1018171979(self);
    res = L20.val1;
    break;
   case 's': 
    L21=SCANNE1018171979(self);
    res = L21.val1;
    break;
   case 't': 
    L22=SCANNE1018171979(self);
    res = L22.val1;
    break;
   case 'u': 
    L23=SCANNE1018171979(self);
    res = L23.val1;
    break;
   case 'v': 
    L24=SCANNE1018171979(self);
    res = L24.val1;
    break;
   case 'w': 
    L25=SCANNE1018171979(self);
    res = L25.val1;
    break;
   case 'x': 
    L26=SCANNE1018171979(self);
    res = L26.val1;
    break;
   case 'y': 
    L27=SCANNE1018171979(self);
    res = L27.val1;
    break;
   case 'z': 
    L28=SCANNE1018171979(self);
    res = L28.val1;
    break;
   case 'A': 
    L29=SCANNE1018171979(self);
    res = L29.val1;
    break;
   case 'B': 
    L30=SCANNE1018171979(self);
    res = L30.val1;
    break;
   case 'C': 
    L31=SCANNE1018171979(self);
    res = L31.val1;
    break;
   case 'D': 
    L32=SCANNE1018171979(self);
    res = L32.val1;
    break;
   case 'E': 
    L33=SCANNE1018171979(self);
    res = L33.val1;
    break;
   case 'F': 
    L34=SCANNE1018171979(self);
    res = L34.val1;
    break;
   case 'G': 
    L35=SCANNE1018171979(self);
    res = L35.val1;
    break;
   case 'H': 
    L36=SCANNE1018171979(self);
    res = L36.val1;
    break;
   case 'I': 
    L37=SCANNE1018171979(self);
    res = L37.val1;
    break;
   case 'J': 
    L38=SCANNE1018171979(self);
    res = L38.val1;
    break;
   case 'K': 
    L39=SCANNE1018171979(self);
    res = L39.val1;
    break;
   case 'L': 
    L40=SCANNE1018171979(self);
    res = L40.val1;
    break;
   case 'M': 
    L41=SCANNE1018171979(self);
    res = L41.val1;
    break;
   case 'N': 
    L42=SCANNE1018171979(self);
    res = L42.val1;
    break;
   case 'O': 
    L43=SCANNE1018171979(self);
    res = L43.val1;
    break;
   case 'P': 
    L44=SCANNE1018171979(self);
    res = L44.val1;
    break;
   case 'Q': 
    L45=SCANNE1018171979(self);
    res = L45.val1;
    break;
   case 'R': 
    L46=SCANNE1018171979(self);
    res = L46.val1;
    break;
   case 'S': 
    L47=SCANNE1018171979(self);
    res = L47.val1;
    break;
   case 'T': 
    L48=SCANNE1018171979(self);
    res = L48.val1;
    break;
   case 'U': 
    L49=SCANNE1018171979(self);
    res = L49.val1;
    break;
   case 'V': 
    L50=SCANNE1018171979(self);
    res = L50.val1;
    break;
   case 'W': 
    L51=SCANNE1018171979(self);
    res = L51.val1;
    break;
   case 'X': 
    L52=SCANNE1018171979(self);
    res = L52.val1;
    break;
   case 'Y': 
    L53=SCANNE1018171979(self);
    res = L53.val1;
    break;
   case 'Z': 
    L54=SCANNE1018171979(self);
    res = L54.val1;
    break;
   case '(': 
    fetch_self = self;
    if (ATTR(fetch_self,backed_up)) {
     c = ATTR(fetch_self,next);
     SATTR(fetch_self,next,ATTR(fetch_self,last_char));
     SATTR(fetch_self,last_char,c);
     SATTR(fetch_self,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self,last_char,ATTR(fetch_self,next));
     SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L55 = ATTR(self,next);
    L561_=L55=='*'; 
    if (L561_) {
     SCANNE297868915(self);
     fetch_self1 = self;
     if (ATTR(fetch_self1,backed_up)) {
      c11 = ATTR(fetch_self1,next);
      SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
      SATTR(fetch_self1,last_char,c11);
      SATTR(fetch_self1,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
      SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
     }
     SCANNER_comment(self);
    }
    else {
     res = SCANNE1412300811;
    }
    break;
   case ')': 
    fetch_self2 = self;
    if (ATTR(fetch_self2,backed_up)) {
     c2 = ATTR(fetch_self2,next);
     SATTR(fetch_self2,next,ATTR(fetch_self2,last_char));
     SATTR(fetch_self2,last_char,c2);
     SATTR(fetch_self2,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self2,last_char,ATTR(fetch_self2,next));
     SATTR(fetch_self2,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE1414957377;
    break;
   case '[': 
    fetch_self3 = self;
    if (ATTR(fetch_self3,backed_up)) {
     c3 = ATTR(fetch_self3,next);
     SATTR(fetch_self3,next,ATTR(fetch_self3,last_char));
     SATTR(fetch_self3,last_char,c3);
     SATTR(fetch_self3,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self3,last_char,ATTR(fetch_self3,next));
     SATTR(fetch_self3,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE2045360664;
    break;
   case ']': 
    fetch_self4 = self;
    if (ATTR(fetch_self4,backed_up)) {
     c4 = ATTR(fetch_self4,next);
     SATTR(fetch_self4,next,ATTR(fetch_self4,last_char));
     SATTR(fetch_self4,last_char,c4);
     SATTR(fetch_self4,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self4,last_char,ATTR(fetch_self4,next));
     SATTR(fetch_self4,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE1290586306;
    break;
   case '{': 
    fetch_self5 = self;
    if (ATTR(fetch_self5,backed_up)) {
     c5 = ATTR(fetch_self5,next);
     SATTR(fetch_self5,next,ATTR(fetch_self5,last_char));
     SATTR(fetch_self5,last_char,c5);
     SATTR(fetch_self5,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self5,last_char,ATTR(fetch_self5,next));
     SATTR(fetch_self5,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE620595834;
    break;
   case '}': 
    fetch_self6 = self;
    if (ATTR(fetch_self6,backed_up)) {
     c6 = ATTR(fetch_self6,next);
     SATTR(fetch_self6,next,ATTR(fetch_self6,last_char));
     SATTR(fetch_self6,last_char,c6);
     SATTR(fetch_self6,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self6,last_char,ATTR(fetch_self6,next));
     SATTR(fetch_self6,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE623252400;
    break;
   case ',': 
    fetch_self7 = self;
    if (ATTR(fetch_self7,backed_up)) {
     c7 = ATTR(fetch_self7,next);
     SATTR(fetch_self7,next,ATTR(fetch_self7,last_char));
     SATTR(fetch_self7,last_char,c7);
     SATTR(fetch_self7,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self7,last_char,ATTR(fetch_self7,next));
     SATTR(fetch_self7,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE916727723;
    break;
   case '.': 
    fetch_self8 = self;
    if (ATTR(fetch_self8,backed_up)) {
     c8 = ATTR(fetch_self8,next);
     SATTR(fetch_self8,next,ATTR(fetch_self8,last_char));
     SATTR(fetch_self8,last_char,c8);
     SATTR(fetch_self8,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self8,last_char,ATTR(fetch_self8,next));
     SATTR(fetch_self8,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_dot_tok;
    break;
   case ';': 
    fetch_self9 = self;
    if (ATTR(fetch_self9,backed_up)) {
     c9 = ATTR(fetch_self9,next);
     SATTR(fetch_self9,next,ATTR(fetch_self9,last_char));
     SATTR(fetch_self9,last_char,c9);
     SATTR(fetch_self9,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self9,last_char,ATTR(fetch_self9,next));
     SATTR(fetch_self9,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_semi_tok;
    break;
   case '$': 
    fetch_self10 = self;
    if (ATTR(fetch_self10,backed_up)) {
     c10 = ATTR(fetch_self10,next);
     SATTR(fetch_self10,next,ATTR(fetch_self10,last_char));
     SATTR(fetch_self10,last_char,c10);
     SATTR(fetch_self10,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self10,last_char,ATTR(fetch_self10,next));
     SATTR(fetch_self10,next,SFILE_1512288010(SFILE_ID_zero));
    }
    if (CHAR_i629101715(ATTR(self,next))) {
     L57=SCANNE1018171979(self);
     res = L57.val1;
     L59 = SCANNE589883380;
     L601_=(res)==(L59); 
     if (L601_) {
      L58 = SCANNE363215919(self, ATTR(self,lex_value).str);
     } else {
      L58 = FALSE;
     }
     if (L58) {
      plus_self = ((STR) &d1);
      plus_sarg = ATTR(self,lex_value).str;
      ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
      SATTR(self,lex_value,IDENT_1150413730(IDENT_zero, ret_val1));
      res = SCANNE2093995558;
     }
    }
    L61 = SCANNE2093995558;
    L621_=(res)==(L61); 
    L63 = L621_;
    L641_=!(L63); 
    if (L641_) {
     error_self = self;
     error_msg = ((STR) &dwithouttypename);
     set_eloc_self = ATTR(error_self,prog);
     set_eloc_l = SFILE_1594766561(SFILE_ID_zero);
     SATTR(set_eloc_self,eloc,set_eloc_l);
     err_self = error_self;
     err_s = error_msg;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    break;
   case '+': 
    fetch_self11 = self;
    if (ATTR(fetch_self11,backed_up)) {
     c12 = ATTR(fetch_self11,next);
     SATTR(fetch_self11,next,ATTR(fetch_self11,last_char));
     SATTR(fetch_self11,last_char,c12);
     SATTR(fetch_self11,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self11,last_char,ATTR(fetch_self11,next));
     SATTR(fetch_self11,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_plus_tok;
    break;
   case '-': 
    fetch_self12 = self;
    if (ATTR(fetch_self12,backed_up)) {
     c13 = ATTR(fetch_self12,next);
     SATTR(fetch_self12,next,ATTR(fetch_self12,last_char));
     SATTR(fetch_self12,last_char,c13);
     SATTR(fetch_self12,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self12,last_char,ATTR(fetch_self12,next));
     SATTR(fetch_self12,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L65 = ATTR(self,next);
    L661_=L65=='-'; 
    if (L661_) {
     SCANNE297868915(self);
     while (1) {
      fetch_self13 = self;
      if (ATTR(fetch_self13,backed_up)) {
       c14 = ATTR(fetch_self13,next);
       SATTR(fetch_self13,next,ATTR(fetch_self13,last_char));
       SATTR(fetch_self13,last_char,c14);
       SATTR(fetch_self13,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self13,last_char,ATTR(fetch_self13,next));
       SATTR(fetch_self13,next,SFILE_1512288010(SFILE_ID_zero));
      }
      L68 = ATTR(self,next);
      L691_=L68=='\n'; 
      if (L691_) {
       L67 = TRUE;
      } else {
       L70 = ATTR(self,next);
       L71 = SFILE_446927505;
       L721_=L70==L71; 
       L67 = L721_;
      }
      if (L67) {
       goto after_loop1;
      }
     }
     after_loop1: ;
     L73 = ATTR(self,next);
     L741_=L73=='\n'; 
     if (L741_) {
      fetch_self14 = self;
      if (ATTR(fetch_self14,backed_up)) {
       c15 = ATTR(fetch_self14,next);
       SATTR(fetch_self14,next,ATTR(fetch_self14,last_char));
       SATTR(fetch_self14,last_char,c15);
       SATTR(fetch_self14,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self14,last_char,ATTR(fetch_self14,next));
       SATTR(fetch_self14,next,SFILE_1512288010(SFILE_ID_zero));
      }
     }
     else {
      res = SCANNER_eof_tok;
     }
    }
    else {
     L75 = ATTR(self,next);
     L761_=L75=='>'; 
     if (L761_) {
      fetch_self15 = self;
      if (ATTR(fetch_self15,backed_up)) {
       c16 = ATTR(fetch_self15,next);
       SATTR(fetch_self15,next,ATTR(fetch_self15,last_char));
       SATTR(fetch_self15,last_char,c16);
       SATTR(fetch_self15,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self15,last_char,ATTR(fetch_self15,next));
       SATTR(fetch_self15,next,SFILE_1512288010(SFILE_ID_zero));
      }
      res = SCANNE1421647682;
     }
     else {
      res = SCANNE470944348;
     }
    }
    break;
   case '*': 
    fetch_self16 = self;
    if (ATTR(fetch_self16,backed_up)) {
     c17 = ATTR(fetch_self16,next);
     SATTR(fetch_self16,next,ATTR(fetch_self16,last_char));
     SATTR(fetch_self16,last_char,c17);
     SATTR(fetch_self16,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self16,last_char,ATTR(fetch_self16,next));
     SATTR(fetch_self16,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE1514574730;
    break;
   case '#': 
    fetch_self17 = self;
    if (ATTR(fetch_self17,backed_up)) {
     c18 = ATTR(fetch_self17,next);
     SATTR(fetch_self17,next,ATTR(fetch_self17,last_char));
     SATTR(fetch_self17,last_char,c18);
     SATTR(fetch_self17,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self17,last_char,ATTR(fetch_self17,next));
     SATTR(fetch_self17,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE879190010;
    break;
   case '^': 
    fetch_self18 = self;
    if (ATTR(fetch_self18,backed_up)) {
     c19 = ATTR(fetch_self18,next);
     SATTR(fetch_self18,next,ATTR(fetch_self18,last_char));
     SATTR(fetch_self18,last_char,c19);
     SATTR(fetch_self18,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self18,last_char,ATTR(fetch_self18,next));
     SATTR(fetch_self18,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_pow_tok;
    break;
   case '%': 
    fetch_self19 = self;
    if (ATTR(fetch_self19,backed_up)) {
     c20 = ATTR(fetch_self19,next);
     SATTR(fetch_self19,next,ATTR(fetch_self19,last_char));
     SATTR(fetch_self19,last_char,c20);
     SATTR(fetch_self19,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self19,last_char,ATTR(fetch_self19,next));
     SATTR(fetch_self19,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_mod_tok;
    break;
   case '|': 
    fetch_self20 = self;
    if (ATTR(fetch_self20,backed_up)) {
     c21 = ATTR(fetch_self20,next);
     SATTR(fetch_self20,next,ATTR(fetch_self20,last_char));
     SATTR(fetch_self20,last_char,c21);
     SATTR(fetch_self20,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self20,last_char,ATTR(fetch_self20,next));
     SATTR(fetch_self20,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_vbar_tok;
    break;
   case '!': 
    fetch_self21 = self;
    if (ATTR(fetch_self21,backed_up)) {
     c22 = ATTR(fetch_self21,next);
     SATTR(fetch_self21,next,ATTR(fetch_self21,last_char));
     SATTR(fetch_self21,last_char,c22);
     SATTR(fetch_self21,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self21,last_char,ATTR(fetch_self21,next));
     SATTR(fetch_self21,next,SFILE_1512288010(SFILE_ID_zero));
    }
    if (whitespace) {
     res = SCANNER_bang_tok;
    }
    else {
     res = SCANNE274598975;
    }
    break;
   case '_': 
    fetch_self22 = self;
    if (ATTR(fetch_self22,backed_up)) {
     c23 = ATTR(fetch_self22,next);
     SATTR(fetch_self22,next,ATTR(fetch_self22,last_char));
     SATTR(fetch_self22,last_char,c23);
     SATTR(fetch_self22,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self22,last_char,ATTR(fetch_self22,next));
     SATTR(fetch_self22,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE1388342024;
    break;
   case '=': 
    fetch_self23 = self;
    if (ATTR(fetch_self23,backed_up)) {
     c24 = ATTR(fetch_self23,next);
     SATTR(fetch_self23,next,ATTR(fetch_self23,last_char));
     SATTR(fetch_self23,last_char,c24);
     SATTR(fetch_self23,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self23,last_char,ATTR(fetch_self23,next));
     SATTR(fetch_self23,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE738612593;
    break;
   case ':': 
    fetch_self24 = self;
    if (ATTR(fetch_self24,backed_up)) {
     c25 = ATTR(fetch_self24,next);
     SATTR(fetch_self24,next,ATTR(fetch_self24,last_char));
     SATTR(fetch_self24,last_char,c25);
     SATTR(fetch_self24,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self24,last_char,ATTR(fetch_self24,next));
     SATTR(fetch_self24,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L77 = ATTR(self,next);
    L781_=L77==':'; 
    if (L781_) {
     fetch_self25 = self;
     if (ATTR(fetch_self25,backed_up)) {
      c26 = ATTR(fetch_self25,next);
      SATTR(fetch_self25,next,ATTR(fetch_self25,last_char));
      SATTR(fetch_self25,last_char,c26);
      SATTR(fetch_self25,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self25,last_char,ATTR(fetch_self25,next));
      SATTR(fetch_self25,next,SFILE_1512288010(SFILE_ID_zero));
     }
     L79 = ATTR(self,next);
     L801_=L79=='='; 
     if (L801_) {
      res = SCANNE149137033;
      backup_self = self;
      c27 = ATTR(backup_self,next);
      SATTR(backup_self,next,ATTR(backup_self,last_char));
      SATTR(backup_self,last_char,c27);
      SATTR(backup_self,backed_up,TRUE);
     }
     else {
      res = SCANNE694354542;
     }
    }
    else {
     L81 = ATTR(self,next);
     L821_=L81=='='; 
     if (L821_) {
      fetch_self26 = self;
      if (ATTR(fetch_self26,backed_up)) {
       c28 = ATTR(fetch_self26,next);
       SATTR(fetch_self26,next,ATTR(fetch_self26,last_char));
       SATTR(fetch_self26,last_char,c28);
       SATTR(fetch_self26,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self26,last_char,ATTR(fetch_self26,next));
       SATTR(fetch_self26,next,SFILE_1512288010(SFILE_ID_zero));
      }
      res = SCANNE1938931342;
     }
     else {
      L84 = ATTR(self,next);
      L851_=L84=='-'; 
      if (L851_) {
       L83 = ATTR(self,pSather);
      } else {
       L83 = FALSE;
      }
      if (L83) {
       fetch_self27 = self;
       if (ATTR(fetch_self27,backed_up)) {
        c29 = ATTR(fetch_self27,next);
        SATTR(fetch_self27,next,ATTR(fetch_self27,last_char));
        SATTR(fetch_self27,last_char,c29);
        SATTR(fetch_self27,backed_up,FALSE);
       }
       else {
        SATTR(fetch_self27,last_char,ATTR(fetch_self27,next));
        SATTR(fetch_self27,next,SFILE_1512288010(SFILE_ID_zero));
       }
       res = SCANNE334166344;
      }
      else {
       res = SCANNE149137033;
      }
     }
    }
    break;
   case '/': 
    fetch_self28 = self;
    if (ATTR(fetch_self28,backed_up)) {
     c30 = ATTR(fetch_self28,next);
     SATTR(fetch_self28,next,ATTR(fetch_self28,last_char));
     SATTR(fetch_self28,last_char,c30);
     SATTR(fetch_self28,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self28,last_char,ATTR(fetch_self28,next));
     SATTR(fetch_self28,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L86 = ATTR(self,next);
    L871_=L86=='='; 
    if (L871_) {
     fetch_self29 = self;
     if (ATTR(fetch_self29,backed_up)) {
      c31 = ATTR(fetch_self29,next);
      SATTR(fetch_self29,next,ATTR(fetch_self29,last_char));
      SATTR(fetch_self29,last_char,c31);
      SATTR(fetch_self29,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self29,last_char,ATTR(fetch_self29,next));
      SATTR(fetch_self29,next,SFILE_1512288010(SFILE_ID_zero));
     }
     res = SCANNE2078461540;
    }
    else {
     res = SCANNE1180808401;
    }
    break;
   case '<': 
    fetch_self30 = self;
    if (ATTR(fetch_self30,backed_up)) {
     c32 = ATTR(fetch_self30,next);
     SATTR(fetch_self30,next,ATTR(fetch_self30,last_char));
     SATTR(fetch_self30,last_char,c32);
     SATTR(fetch_self30,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self30,last_char,ATTR(fetch_self30,next));
     SATTR(fetch_self30,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L88 = ATTR(self,next);
    L891_=L88=='='; 
    if (L891_) {
     fetch_self31 = self;
     if (ATTR(fetch_self31,backed_up)) {
      c33 = ATTR(fetch_self31,next);
      SATTR(fetch_self31,next,ATTR(fetch_self31,last_char));
      SATTR(fetch_self31,last_char,c33);
      SATTR(fetch_self31,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self31,last_char,ATTR(fetch_self31,next));
      SATTR(fetch_self31,next,SFILE_1512288010(SFILE_ID_zero));
     }
     res = SCANNE290718062;
    }
    else {
     res = SCANNE401863129;
    }
    break;
   case '>': 
    fetch_self32 = self;
    if (ATTR(fetch_self32,backed_up)) {
     c34 = ATTR(fetch_self32,next);
     SATTR(fetch_self32,next,ATTR(fetch_self32,last_char));
     SATTR(fetch_self32,last_char,c34);
     SATTR(fetch_self32,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self32,last_char,ATTR(fetch_self32,next));
     SATTR(fetch_self32,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L90 = ATTR(self,next);
    L911_=L90=='='; 
    if (L911_) {
     fetch_self33 = self;
     if (ATTR(fetch_self33,backed_up)) {
      c35 = ATTR(fetch_self33,next);
      SATTR(fetch_self33,next,ATTR(fetch_self33,last_char));
      SATTR(fetch_self33,last_char,c35);
      SATTR(fetch_self33,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self33,last_char,ATTR(fetch_self33,next));
      SATTR(fetch_self33,next,SFILE_1512288010(SFILE_ID_zero));
     }
     res = SCANNE116326663;
    }
    else {
     res = SCANNE637092534;
    }
    break;
   case '~': 
    fetch_self34 = self;
    if (ATTR(fetch_self34,backed_up)) {
     c36 = ATTR(fetch_self34,next);
     SATTR(fetch_self34,next,ATTR(fetch_self34,last_char));
     SATTR(fetch_self34,last_char,c36);
     SATTR(fetch_self34,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self34,last_char,ATTR(fetch_self34,next));
     SATTR(fetch_self34,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_not_tok;
    break;
   case '\'': 
    fetch_self35 = self;
    if (ATTR(fetch_self35,backed_up)) {
     c37 = ATTR(fetch_self35,next);
     SATTR(fetch_self35,next,ATTR(fetch_self35,last_char));
     SATTR(fetch_self35,last_char,c37);
     SATTR(fetch_self35,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self35,last_char,ATTR(fetch_self35,next));
     SATTR(fetch_self35,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L92 = ATTR(self,next);
    L931_=L92=='\\'; 
    if (L931_) {
     SATTR(self,char_value,SCANNE2120205373(self));
    }
    else {
     SATTR(self,char_value,ATTR(self,next));
     fetch_self36 = self;
     if (ATTR(fetch_self36,backed_up)) {
      c38 = ATTR(fetch_self36,next);
      SATTR(fetch_self36,next,ATTR(fetch_self36,last_char));
      SATTR(fetch_self36,last_char,c38);
      SATTR(fetch_self36,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self36,last_char,ATTR(fetch_self36,next));
      SATTR(fetch_self36,next,SFILE_1512288010(SFILE_ID_zero));
     }
    }
    L94 = ATTR(self,next);
    L951_=L94=='\''; 
    L96 = L951_;
    L971_=!(L96); 
    if (L971_) {
     error_self1 = self;
     error_msg1 = ((STR) &malfor1193176489);
     set_eloc_self1 = ATTR(error_self1,prog);
     set_eloc_l1 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(set_eloc_self1,eloc,set_eloc_l1);
     err_self1 = error_self1;
     err_s1 = error_msg1;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    fetch_self37 = self;
    if (ATTR(fetch_self37,backed_up)) {
     c39 = ATTR(fetch_self37,next);
     SATTR(fetch_self37,next,ATTR(fetch_self37,last_char));
     SATTR(fetch_self37,last_char,c39);
     SATTR(fetch_self37,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self37,last_char,ATTR(fetch_self37,next));
     SATTR(fetch_self37,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNE928026230;
    break;
   case '\"': 
    fetch_self38 = self;
    if (ATTR(fetch_self38,backed_up)) {
     c40 = ATTR(fetch_self38,next);
     SATTR(fetch_self38,next,ATTR(fetch_self38,last_char));
     SATTR(fetch_self38,last_char,c40);
     SATTR(fetch_self38,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self38,last_char,ATTR(fetch_self38,next));
     SATTR(fetch_self38,next,SFILE_1512288010(SFILE_ID_zero));
    }
    clear_self = ATTR(self,buf1);
    L98 = (clear_self==((FSTR) NULL));
    L991_=!(L98); 
    if (L991_) {
     SATTR(clear_self,loc,0);
    }
    while (1) {
     while (1) {
      L101 = ATTR(self,next);
      L1021_=L101=='\"'; 
      if (L1021_) {
       L100 = TRUE;
      } else {
       L103 = ATTR(self,next);
       L1041_=L103=='\n'; 
       L100 = L1041_;
      }
      if (L100) {
       goto after_loop3;
      }
      L105 = ATTR(self,next);
      L1061_=L105=='\\'; 
      if (L1061_) {
       plus_self1 = ATTR(self,buf1);
       plus_c = SCANNE2120205373(self);
       ret_val2 = FSTR_p399773021(plus_self1, plus_c);
       SATTR(self,buf1,ret_val2);
      }
      else {
       plus_self2 = ATTR(self,buf1);
       plus_c1 = ATTR(self,next);
       ret_val3 = FSTR_p399773021(plus_self2, plus_c1);
       SATTR(self,buf1,ret_val3);
       fetch_self39 = self;
       if (ATTR(fetch_self39,backed_up)) {
        c41 = ATTR(fetch_self39,next);
        SATTR(fetch_self39,next,ATTR(fetch_self39,last_char));
        SATTR(fetch_self39,last_char,c41);
        SATTR(fetch_self39,backed_up,FALSE);
       }
       else {
        SATTR(fetch_self39,last_char,ATTR(fetch_self39,next));
        SATTR(fetch_self39,next,SFILE_1512288010(SFILE_ID_zero));
       }
      }
     }
     after_loop3: ;
     L107 = ATTR(self,next);
     L1081_=L107=='\n'; 
     if (L1081_) {
      error_self2 = self;
      error_msg2 = ((STR) &unterm1032969897);
      set_eloc_self2 = ATTR(error_self2,prog);
      set_eloc_l2 = SFILE_1594766561(SFILE_ID_zero);
      SATTR(set_eloc_self2,eloc,set_eloc_l2);
      err_self2 = error_self2;
      err_s2 = error_msg2;
      PROG_err_STR(ATTR(err_self2,prog), err_s2);
      goto after_loop2;
     }
     fetch_self40 = self;
     if (ATTR(fetch_self40,backed_up)) {
      c42 = ATTR(fetch_self40,next);
      SATTR(fetch_self40,next,ATTR(fetch_self40,last_char));
      SATTR(fetch_self40,last_char,c42);
      SATTR(fetch_self40,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self40,last_char,ATTR(fetch_self40,next));
      SATTR(fetch_self40,next,SFILE_1512288010(SFILE_ID_zero));
     }
     SCANNE1781940665(self);
     L109 = ATTR(self,next);
     L1101_=L109=='\"'; 
     L111 = L1101_;
     L1121_=!(L111); 
     if (L1121_) {
      goto after_loop2;
     }
     fetch_self41 = self;
     if (ATTR(fetch_self41,backed_up)) {
      c43 = ATTR(fetch_self41,next);
      SATTR(fetch_self41,next,ATTR(fetch_self41,last_char));
      SATTR(fetch_self41,last_char,c43);
      SATTR(fetch_self41,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self41,last_char,ATTR(fetch_self41,next));
      SATTR(fetch_self41,next,SFILE_1512288010(SFILE_ID_zero));
     }
    }
    after_loop2: ;
    add_buf_to_sym_t = self;
    str_self = ATTR(add_buf_to_sym_t,buf1);
    ret_val4 = STR_fr1097270334(((STR) NULL), str_self);
    SATTR(add_buf_to_sym_t,lex_value,IDENT_1150413730(IDENT_zero, ret_val4));
    res = SCANNER_lstr_tok;
    break;
   case '0': 
    L113=SCANNE892342113(self);
    res = L113.val1;
    break;
   case '1': 
    L114=SCANNE892342113(self);
    res = L114.val1;
    break;
   case '2': 
    L115=SCANNE892342113(self);
    res = L115.val1;
    break;
   case '3': 
    L116=SCANNE892342113(self);
    res = L116.val1;
    break;
   case '4': 
    L117=SCANNE892342113(self);
    res = L117.val1;
    break;
   case '5': 
    L118=SCANNE892342113(self);
    res = L118.val1;
    break;
   case '6': 
    L119=SCANNE892342113(self);
    res = L119.val1;
    break;
   case '7': 
    L120=SCANNE892342113(self);
    res = L120.val1;
    break;
   case '8': 
    L121=SCANNE892342113(self);
    res = L121.val1;
    break;
   case '9': 
    L122=SCANNE892342113(self);
    res = L122.val1;
    break;
   case '@': 
    fetch_self42 = self;
    if (ATTR(fetch_self42,backed_up)) {
     c44 = ATTR(fetch_self42,next);
     SATTR(fetch_self42,next,ATTR(fetch_self42,last_char));
     SATTR(fetch_self42,last_char,c44);
     SATTR(fetch_self42,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self42,last_char,ATTR(fetch_self42,next));
     SATTR(fetch_self42,next,SFILE_1512288010(SFILE_ID_zero));
    }
    res = SCANNER_at_tok;
    break;
   default: ;
    error_self3 = self;
    plus_self3 = ((STR) &unknowncharacter);
    plus_arg = ATTR(self,next);
    str_self1 = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self1;
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
    ret_val7 = r;
    ret_val6 = ret_val7;
    s = ret_val6;
    ret_val5 = STR_ap1077157958(plus_self3, s, TRUE);
    plus_self4 = ret_val5;
    plus_arg1 = '\'';
    str_self2 = plus_arg1;
    create_self1 = ((STR) NULL);
    create_c1 = str_self2;
    L129 = 1;
    L131=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L129)*sizeof(CHAR);
    L130=ZALLOC_LEAF_BIG(L131);
    if (L130==NULL) FATAL("Unable to allocate more memory");
    memset(L130,0,L131);
    ((OB)L130)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L130)->header.destroyed=0;
#endif

    L128 = ((STR) L130);
    L128->asize = L129;
    r1 = L128;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val10 = r1;
    ret_val9 = ret_val10;
    s1 = ret_val9;
    ret_val8 = STR_ap1077157958(plus_self4, s1, TRUE);
    error_msg3 = ret_val8;
    set_eloc_self3 = ATTR(error_self3,prog);
    set_eloc_l3 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(set_eloc_self3,eloc,set_eloc_l3);
    err_self3 = error_self3;
    err_s3 = error_msg3;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
    fetch_self43 = self;
    if (ATTR(fetch_self43,backed_up)) {
     c45 = ATTR(fetch_self43,next);
     SATTR(fetch_self43,next,ATTR(fetch_self43,last_char));
     SATTR(fetch_self43,last_char,c45);
     SATTR(fetch_self43,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self43,last_char,ATTR(fetch_self43,next));
     SATTR(fetch_self43,next,SFILE_1512288010(SFILE_ID_zero));
    }
  }
  L133 = SCANNER_null_tok;
  L1341_=(res)==(L133); 
  if (L1341_) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 create_self2 = TOKEN_zero;
 create_val = res;
 t.val1 = create_val;
 ret_val11 = t;
 ret_val = ret_val11;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TOKEN SCANNE892342113(SCANNER self) {
 TOKEN ret_val = TOKEN_zero;
 INT res = INT_zero;
 INT d = INT_zero;
 INTI1 m;
 INTI1 e;
 INT ec = INT_zero;
 INTI1 ten;
 BOOL neg1 = BOOL_zero;
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c11 = CHAR_zero;
 SCANNER fetch_self2;
 CHAR c2 = CHAR_zero;
 SCANNER fetch_self3;
 CHAR c3 = CHAR_zero;
 SCANNER fetch_self4;
 CHAR c4 = CHAR_zero;
 SCANNER fetch_self5;
 CHAR c5 = CHAR_zero;
 SCANNER fetch_self6;
 CHAR c6 = CHAR_zero;
 SCANNER fetch_self7;
 CHAR c7 = CHAR_zero;
 SCANNER fetch_self8;
 CHAR c8 = CHAR_zero;
 SCANNER error_self;
 STR error_msg;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 SCANNER err_self;
 STR err_s;
 SCANNER backup_self;
 CHAR c9 = CHAR_zero;
 INTI1 is_neg_self;
 BOOL ret_val1 = BOOL_zero;
 RAT create_self = RAT_zero;
 INTI1 create_u;
 RAT ret_val2 = RAT_zero;
 RAT r = RAT_zero;
 SCANNER fetch_self9;
 CHAR c10 = CHAR_zero;
 SCANNER fetch_self10;
 CHAR c12 = CHAR_zero;
 SCANNER fetch_self11;
 CHAR c13 = CHAR_zero;
 SCANNER fetch_self12;
 CHAR c14 = CHAR_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val3 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 CHAR L1;
 BOOL L21_;
 CHAR L3;
 BOOL L41_;
 CHAR L5;
 BOOL L61_;
 CHAR L7;
 BOOL L81_;
 BOOL L9;
 CHAR L10;
 BOOL L111_;
 BOOL L121_;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 CHAR L16;
 BOOL L171_;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 INTI1 L22;
 INT L231_;
 CHAR L24;
 BOOL L251_;
 CHAR L26;
 BOOL L271_;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 extern STR malfor445865812;
 INT L31;
 BOOL L321_;
 INTI1 L33;
 INT L34;
 INT L351_;
 INTI1 L36;
 RAT L37;
 RAT L38;
 CHAR L39;
 BOOL L401_;
 INT L42;
 BOOL L431_;
 CHAR L44;
 BOOL L451_;
 CHAR L46;
 BOOL L471_;
 CHAR L48;
 BOOL L491_;
 res = SCANNER_lint_tok;
 d = 10;
 L1 = ATTR(self,next);
 L21_=L1=='0'; 
 if (L21_) {
  fetch_self = self;
  if (ATTR(fetch_self,backed_up)) {
   c = ATTR(fetch_self,next);
   SATTR(fetch_self,next,ATTR(fetch_self,last_char));
   SATTR(fetch_self,last_char,c);
   SATTR(fetch_self,backed_up,FALSE);
  }
  else {
   SATTR(fetch_self,last_char,ATTR(fetch_self,next));
   SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
  }
  L3 = ATTR(self,next);
  L41_=L3=='b'; 
  if (L41_) {
   fetch_self1 = self;
   if (ATTR(fetch_self1,backed_up)) {
    c11 = ATTR(fetch_self1,next);
    SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
    SATTR(fetch_self1,last_char,c11);
    SATTR(fetch_self1,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
    SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
   }
   d = 2;
  }
  else {
   L5 = ATTR(self,next);
   L61_=L5=='o'; 
   if (L61_) {
    fetch_self2 = self;
    if (ATTR(fetch_self2,backed_up)) {
     c2 = ATTR(fetch_self2,next);
     SATTR(fetch_self2,next,ATTR(fetch_self2,last_char));
     SATTR(fetch_self2,last_char,c2);
     SATTR(fetch_self2,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self2,last_char,ATTR(fetch_self2,next));
     SATTR(fetch_self2,next,SFILE_1512288010(SFILE_ID_zero));
    }
    d = 8;
   }
   else {
    L7 = ATTR(self,next);
    L81_=L7=='x'; 
    if (L81_) {
     fetch_self3 = self;
     if (ATTR(fetch_self3,backed_up)) {
      c3 = ATTR(fetch_self3,next);
      SATTR(fetch_self3,next,ATTR(fetch_self3,last_char));
      SATTR(fetch_self3,last_char,c3);
      SATTR(fetch_self3,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self3,last_char,ATTR(fetch_self3,next));
      SATTR(fetch_self3,next,SFILE_1512288010(SFILE_ID_zero));
     }
     d = 16;
    }
   }
  }
 }
 m = SCANNE1500285552(self, d);
 e = INTI_c1588968505(((INTI1) NULL), 0);
 L10 = ATTR(self,next);
 L111_=L10=='.'; 
 if (L111_) {
  L121_=(d)==(10); 
  L9 = L121_;
 } else {
  L9 = FALSE;
 }
 if (L9) {
  fetch_self4 = self;
  if (ATTR(fetch_self4,backed_up)) {
   c4 = ATTR(fetch_self4,next);
   SATTR(fetch_self4,next,ATTR(fetch_self4,last_char));
   SATTR(fetch_self4,last_char,c4);
   SATTR(fetch_self4,backed_up,FALSE);
  }
  else {
   SATTR(fetch_self4,last_char,ATTR(fetch_self4,next));
   SATTR(fetch_self4,next,SFILE_1512288010(SFILE_ID_zero));
  }
  d = CHAR_d198943415(ATTR(self,next));
  L131_=(d)<(0); 
  L14 = L131_;
  L151_=!(L14); 
  if (L151_) {
   ec = 0;
   ten = INTI_c1588968505(((INTI1) NULL), 10);
   while (1) {
    L16 = ATTR(self,next);
    L171_=L16=='_'; 
    if (L171_) {
     fetch_self5 = self;
     if (ATTR(fetch_self5,backed_up)) {
      c5 = ATTR(fetch_self5,next);
      SATTR(fetch_self5,next,ATTR(fetch_self5,last_char));
      SATTR(fetch_self5,last_char,c5);
      SATTR(fetch_self5,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self5,last_char,ATTR(fetch_self5,next));
      SATTR(fetch_self5,next,SFILE_1512288010(SFILE_ID_zero));
     }
    }
    else {
     L181_=(d)<(0); 
     L19 = L181_;
     L201_=!(L19); 
     if (L201_) {
      fetch_self6 = self;
      if (ATTR(fetch_self6,backed_up)) {
       c6 = ATTR(fetch_self6,next);
       SATTR(fetch_self6,next,ATTR(fetch_self6,last_char));
       SATTR(fetch_self6,last_char,c6);
       SATTR(fetch_self6,backed_up,FALSE);
      }
      else {
       SATTR(fetch_self6,last_char,ATTR(fetch_self6,next));
       SATTR(fetch_self6,next,SFILE_1512288010(SFILE_ID_zero));
      }
      L22 = INTI_t1692605406(m, ten);
      m = INTI_p1769163857(L22, INTI_c1588968505(((INTI1) NULL), d));
      L231_=INTMINUS(ec,1); 
      ec = L231_;
     }
     else {
      goto after_loop;
     }
    }
    d = CHAR_d198943415(ATTR(self,next));
   }
   after_loop: ;
   e = INTI_c1588968505(((INTI1) NULL), ec);
   L24 = ATTR(self,next);
   L251_=L24=='e'; 
   if (L251_) {
    fetch_self7 = self;
    if (ATTR(fetch_self7,backed_up)) {
     c7 = ATTR(fetch_self7,next);
     SATTR(fetch_self7,next,ATTR(fetch_self7,last_char));
     SATTR(fetch_self7,last_char,c7);
     SATTR(fetch_self7,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self7,last_char,ATTR(fetch_self7,next));
     SATTR(fetch_self7,next,SFILE_1512288010(SFILE_ID_zero));
    }
    neg1 = FALSE;
    L26 = ATTR(self,next);
    L271_=L26=='-'; 
    if (L271_) {
     fetch_self8 = self;
     if (ATTR(fetch_self8,backed_up)) {
      c8 = ATTR(fetch_self8,next);
      SATTR(fetch_self8,next,ATTR(fetch_self8,last_char));
      SATTR(fetch_self8,last_char,c8);
      SATTR(fetch_self8,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self8,last_char,ATTR(fetch_self8,next));
      SATTR(fetch_self8,next,SFILE_1512288010(SFILE_ID_zero));
     }
     neg1 = TRUE;
    }
    d = CHAR_d198943415(ATTR(self,next));
    L281_=(d)<(0); 
    L29 = L281_;
    L301_=!(L29); 
    if (L301_) {
     if (neg1) {
      e = INTI_m436971372(e, SCANNE1500285552(self, 10));
     }
     else {
      e = INTI_p1769163857(e, SCANNE1500285552(self, 10));
     }
    }
    else {
     error_self = self;
     error_msg = ((STR) &malfor445865812);
     set_eloc_self = ATTR(error_self,prog);
     set_eloc_l = SFILE_1594766561(SFILE_ID_zero);
     SATTR(set_eloc_self,eloc,set_eloc_l);
     err_self = error_self;
     err_s = error_msg;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
   }
   res = SCANNER_lflt_tok;
  }
  else {
   backup_self = self;
   c9 = ATTR(backup_self,next);
   SATTR(backup_self,next,ATTR(backup_self,last_char));
   SATTR(backup_self,last_char,c9);
   SATTR(backup_self,backed_up,TRUE);
  }
 }
 is_neg_self = e;
 L31 = ATTR(is_neg_self,len1);
 L321_=(L31)<(0); 
 ret_val1 = L321_;
 if (ret_val1) {
  L33 = INTI_c1588968505(((INTI1) NULL), 10);
  L34 = INTI_intrINT(e);
  L351_=INTMINUS(0,L34); 
  SATTR(self,num_value,RAT_cr9703311(RAT_zero, m, INTI_p1518979099(L33, L351_)));
 }
 else {
  create_self = RAT_zero;
  L36 = INTI_c1588968505(((INTI1) NULL), 10);
  create_u = INTI_t1692605406(m, INTI_p1518979099(L36, INTI_intrINT(e)));
  L37 = r;
  L37.u = create_u;
  L38 = L37;
  L38.v = INTI_c1588968505(((INTI1) NULL), 1);
  ret_val2 = L38;
  SATTR(self,num_value,ret_val2);
 }
 SATTR(self,value_type,AS_FLT20428321);
 L39 = ATTR(self,next);
 L401_=L39=='i'; 
 if (L401_) {
  fetch_self9 = self;
  if (ATTR(fetch_self9,backed_up)) {
   c10 = ATTR(fetch_self9,next);
   SATTR(fetch_self9,next,ATTR(fetch_self9,last_char));
   SATTR(fetch_self9,last_char,c10);
   SATTR(fetch_self9,backed_up,FALSE);
  }
  else {
   SATTR(fetch_self9,last_char,ATTR(fetch_self9,next));
   SATTR(fetch_self9,next,SFILE_1512288010(SFILE_ID_zero));
  }
  SATTR(self,value_type,AS_FLT388140455);
 }
 else {
  L42 = SCANNER_lflt_tok;
  L431_=(res)==(L42); 
  if (L431_) {
   L44 = ATTR(self,next);
   L451_=L44=='d'; 
   if (L451_) {
    fetch_self10 = self;
    if (ATTR(fetch_self10,backed_up)) {
     c12 = ATTR(fetch_self10,next);
     SATTR(fetch_self10,next,ATTR(fetch_self10,last_char));
     SATTR(fetch_self10,last_char,c12);
     SATTR(fetch_self10,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self10,last_char,ATTR(fetch_self10,next));
     SATTR(fetch_self10,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L46 = ATTR(self,next);
    L471_=L46=='x'; 
    if (L471_) {
     fetch_self11 = self;
     if (ATTR(fetch_self11,backed_up)) {
      c13 = ATTR(fetch_self11,next);
      SATTR(fetch_self11,next,ATTR(fetch_self11,last_char));
      SATTR(fetch_self11,last_char,c13);
      SATTR(fetch_self11,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self11,last_char,ATTR(fetch_self11,next));
      SATTR(fetch_self11,next,SFILE_1512288010(SFILE_ID_zero));
     }
     SATTR(self,value_type,AS_FLT1215265092);
    }
    else {
     SATTR(self,value_type,AS_FLT388140360);
    }
   }
   else {
    L48 = ATTR(self,next);
    L491_=L48=='x'; 
    if (L491_) {
     fetch_self12 = self;
     if (ATTR(fetch_self12,backed_up)) {
      c14 = ATTR(fetch_self12,next);
      SATTR(fetch_self12,next,ATTR(fetch_self12,last_char));
      SATTR(fetch_self12,last_char,c14);
      SATTR(fetch_self12,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self12,last_char,ATTR(fetch_self12,next));
      SATTR(fetch_self12,next,SFILE_1512288010(SFILE_ID_zero));
     }
     SATTR(self,value_type,AS_FLT388140740);
    }
   }
  }
 }
 create_self1 = TOKEN_zero;
 create_val = res;
 t.val1 = create_val;
 ret_val3 = t;
 ret_val = ret_val3;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void SCANNE1781940665(SCANNER self) {
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 while (1) {
  if (CHAR_i84735023(ATTR(self,next))) {
  }
  else {
   goto after_loop;
  }
  fetch_self = self;
  if (ATTR(fetch_self,backed_up)) {
   c = ATTR(fetch_self,next);
   SATTR(fetch_self,next,ATTR(fetch_self,last_char));
   SATTR(fetch_self,last_char,c);
   SATTR(fetch_self,backed_up,FALSE);
  }
  else {
   SATTR(fetch_self,last_char,ATTR(fetch_self,next));
   SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
  }
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void SCANNE297868915(SCANNER self) {
}


#undef IS_ITER
#define IS_ITER 0

void SCANNER_comment(SCANNER self) {
 INT n = INT_zero;
 SCANNER fetch_self;
 CHAR c = CHAR_zero;
 SCANNER fetch_self1;
 CHAR c11 = CHAR_zero;
 SCANNER fetch_self2;
 CHAR c2 = CHAR_zero;
 SCANNER fetch_self3;
 CHAR c3 = CHAR_zero;
 SCANNER fetch_self4;
 CHAR c4 = CHAR_zero;
 BOOL L1;
 CHAR L2;
 CHAR L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 BOOL L71_;
 CHAR L8;
 BOOL L91_;
 CHAR L10;
 BOOL L111_;
 INT L121_;
 CHAR L13;
 BOOL L141_;
 CHAR L15;
 BOOL L161_;
 INT L171_;
 n = 1;
 while (1) {
  L2 = ATTR(self,next);
  L3 = SFILE_446927505;
  L41_=L2==L3; 
  L5 = L41_;
  L61_=!(L5); 
  if (L61_) {
   L71_=(0)<(n); 
   L1 = L71_;
  } else {
   L1 = FALSE;
  }
  if (L1) {
  }
  else {
   goto after_loop;
  }
  L8 = ATTR(self,next);
  L91_=L8=='('; 
  if (L91_) {
   fetch_self = self;
   if (ATTR(fetch_self,backed_up)) {
    c = ATTR(fetch_self,next);
    SATTR(fetch_self,next,ATTR(fetch_self,last_char));
    SATTR(fetch_self,last_char,c);
    SATTR(fetch_self,backed_up,FALSE);
   }
   else {
    SATTR(fetch_self,last_char,ATTR(fetch_self,next));
    SATTR(fetch_self,next,SFILE_1512288010(SFILE_ID_zero));
   }
   L10 = ATTR(self,next);
   L111_=L10=='*'; 
   if (L111_) {
    fetch_self1 = self;
    if (ATTR(fetch_self1,backed_up)) {
     c11 = ATTR(fetch_self1,next);
     SATTR(fetch_self1,next,ATTR(fetch_self1,last_char));
     SATTR(fetch_self1,last_char,c11);
     SATTR(fetch_self1,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self1,last_char,ATTR(fetch_self1,next));
     SATTR(fetch_self1,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L121_=INTPLUS(n,1); 
    n = L121_;
   }
  }
  else {
   L13 = ATTR(self,next);
   L141_=L13=='*'; 
   if (L141_) {
    fetch_self2 = self;
    if (ATTR(fetch_self2,backed_up)) {
     c2 = ATTR(fetch_self2,next);
     SATTR(fetch_self2,next,ATTR(fetch_self2,last_char));
     SATTR(fetch_self2,last_char,c2);
     SATTR(fetch_self2,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self2,last_char,ATTR(fetch_self2,next));
     SATTR(fetch_self2,next,SFILE_1512288010(SFILE_ID_zero));
    }
    L15 = ATTR(self,next);
    L161_=L15==')'; 
    if (L161_) {
     fetch_self3 = self;
     if (ATTR(fetch_self3,backed_up)) {
      c3 = ATTR(fetch_self3,next);
      SATTR(fetch_self3,next,ATTR(fetch_self3,last_char));
      SATTR(fetch_self3,last_char,c3);
      SATTR(fetch_self3,backed_up,FALSE);
     }
     else {
      SATTR(fetch_self3,last_char,ATTR(fetch_self3,next));
      SATTR(fetch_self3,next,SFILE_1512288010(SFILE_ID_zero));
     }
     L171_=INTMINUS(n,1); 
     n = L171_;
    }
   }
   else {
    fetch_self4 = self;
    if (ATTR(fetch_self4,backed_up)) {
     c4 = ATTR(fetch_self4,next);
     SATTR(fetch_self4,next,ATTR(fetch_self4,last_char));
     SATTR(fetch_self4,last_char,c4);
     SATTR(fetch_self4,backed_up,FALSE);
    }
    else {
     SATTR(fetch_self4,last_char,ATTR(fetch_self4,next));
     SATTR(fetch_self4,next,SFILE_1512288010(SFILE_ID_zero));
    }
   }
  }
 }
 after_loop: ;
}
