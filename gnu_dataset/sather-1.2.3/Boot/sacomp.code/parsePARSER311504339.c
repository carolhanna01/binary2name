#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

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

typedef struct TOKEN_struct {/* layout for TOKEN */
 INT val1;
 } TOKEN;
static TOKEN TOKEN_zero;

typedef struct TOKEN_boxed_struct {
 OB_HEADER header;
 TOKEN immutable_part;
 } *TOKEN_boxed;

typedef struct TUPdAS268679779_struct {/* layout for TUP{$AS_EXPR,AS_ARG_MODE} */
 struct dAS_EXPR_struct *t1;
 struct AS_ARG_MODE_struct *t2;
 } TUPdAS268679779;
static TUPdAS268679779 TUPdAS268679779_zero;

typedef struct TUPdAS268679779_boxed_struct {
 OB_HEADER header;
 TUPdAS268679779 immutable_part;
 } *TUPdAS268679779_boxed;

typedef struct AS_ARG_DEC_struct {/* layout for AS_ARG_DEC */
 OB_HEADER header;
 struct AS_ARG_DEC_struct *next;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_ARG_DEC;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

typedef struct AS_BOU14269336_struct {/* layout for AS_BOUND_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct AS_CALL_EXPR_struct *call1;
 struct AS_TYPE_SPEC_struct *ret;
 struct SFILE_ID_struct source1;
 BOOL is_f_rout;
 BOOL is_iter;
 } *AS_BOU14269336;

typedef struct AS_CALL_EXPR_struct {/* layout for AS_CALL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *args;
 struct dAS_EXPR_struct *next;
 struct dAS_EXPR_struct *ob;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL flip;
 BOOL is_array;
 } *AS_CALL_EXPR;

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

typedef struct AS_CREATE_EXPR_struct {/* layout for AS_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_CREATE_EXPR;

typedef struct AS_FEAT_MOD_struct {/* layout for AS_FEAT_MOD */
 OB_HEADER header;
 struct AS_FEAT_MOD_struct *next;
 struct IDENT_struct name1;
 struct IDENT_struct new_name;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_FEAT_MOD;

typedef struct AS_INC1416917001_struct {/* layout for AS_INCLUDE_CLAUSE */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_FEAT_MOD_struct *mods;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_INC1416917001;

typedef struct AS_PARAM_DEC_struct {/* layout for AS_PARAM_DEC */
 OB_HEADER header;
 struct AS_PARAM_DEC_struct *next;
 struct AS_TYPE_SPEC_struct *type_constraint;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_PARAM_DEC;

typedef struct AS_ROUT_DEF_struct {/* layout for AS_ROUT_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *post_e;
 struct dAS_EXPR_struct *pre_e;
 struct AS_ARG_DEC_struct *args_dec;
 struct AS_STMT_LIST_struct *body;
 struct AS_TYPE_SPEC_struct *ret_dec;
 struct IDENT_struct builtin_name;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_abstract;
 BOOL is_attach_routin;
 BOOL is_builtin;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL is_private;
 } *AS_ROUT_DEF;

typedef struct AS_TYPE_SPEC_struct {/* layout for AS_TYPE_SPEC */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *next;
 struct AS_TYPE_SPEC_struct *params;
 struct AS_TYPE_SPEC_struct *ret;
 struct IDENT_struct name1;
 INT kind;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_TYPE_SPEC;

typedef struct AS_UND152986614_struct {/* layout for AS_UNDERSCORE_ARG */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_UND152986614;

typedef struct CONVERT_struct {/* layout for CONVERT */
 OB_HEADER header;
 struct BFILE_struct *file1;
 INT end_pos;
 INT start_pos;
 BOOL need_to_convert;
 } *CONVERT;

typedef struct FLISTSTR_struct {/* layout for FLIST{STR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 STR arr_part[1];
 } *FLISTSTR;

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

extern IDENT IDENT_604723504;
extern INT AS_ARG1300937332;
extern INT AS_ARG1872943786;
extern INT AS_ARG551300469;
extern INT AS_ARG685418392;
extern INT AS_CLASS_DEF_abs;
extern INT AS_TYP1851973889;
extern INT AS_TYP1853679690;
extern INT AS_TYPE_SPEC_it;
extern INT AS_TYPE_SPEC_ord;
extern INT AS_TYPE_SPEC_rt;
extern INT PARSER1019557737;
extern INT PARSER1207020061;
extern INT PARSER1209676627;
extern INT PARSER1337387114;
extern INT PARSER1701976116;
extern INT PARSER1758133485;
extern INT PARSER1998725038;
extern INT PARSER2001381604;
extern INT PARSER415742890;
extern INT PARSER550831344;
extern INT PARSER840587154;
extern INT PARSER978809429;
extern INT PARSER_ITER_tok;
extern INT PARSER_ROUT_tok;
extern INT PARSER_SAME_tok;
extern INT PARSER_bang_tok;
extern INT PARSER_class_tok;
extern INT PARSER_colon_tok;
extern INT PARSER_comma_tok;
extern INT PARSER_dot_tok;
extern INT PARSER_end_tok;
extern INT PARSER_eof_tok;
extern INT PARSER_ident_tok;
extern INT PARSER_inout_tok;
extern INT PARSER_is_gt_tok;
extern INT PARSER_is_lt_tok;
extern INT PARSER_is_tok;
extern INT PARSER_once_tok;
extern INT PARSER_out_tok;
extern INT PARSER_semi_tok;
extern INT PARSER_type_tok;
extern INT PARSER_under_tok;
extern INT SFILE_ID_pos;

/* Function declarations */


extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_EX1889860882)(dAS_EXPR, dAS_EXPR);
AS_ARG_DEC PARSER994907623(PARSER, BOOL);
AS_ARG_MODE PARSER930791008(PARSER, BOOL);
AS_BOU14269336 PARSER1814172757(PARSER);
AS_CALL_EXPR PARSER716940617(PARSER, dAS_EXPR, AS_TYPE_SPEC, BOOL);
AS_CLASS_DEF PARSER1968454579(PARSER);
AS_CREATE_EXPR PARSER2102248401(PARSER);
AS_PARAM_DEC PARSER1105068487(PARSER);
AS_ROUT_DEF PARSER2039100746(PARSER);
AS_TYPE_SPEC PARSER2028498823(PARSER);
AS_TYPE_SPEC PARSER532091533(PARSER);
BOOL AS_ARG1391068469(AS_ARG_MODE);
BOOL PARSER1234876012(PARSER, TOKEN);
BOOL PARSER1883080161(PARSER, TOKEN);
BOOL PARSER2128430730(PARSER, INT);
BOOL SCANNE363215919(SCANNER, STR);
CONVERT CONVER262703763(CONVERT, PARSER, BOOL, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
FLISTSTR FLISTS1900495285(FLISTSTR, INT);
IDENT IDENT_1150413730(IDENT, STR);
IDENT PARSER814698330(PARSER);
INT STR_sizerINT(STR);
PARSER PARSER403165095(PARSER, PROG, STR, BOOL, BOOL, BOOL);
SCANNER SCANNE1298383531(SCANNER, PROG, STR, BOOL);
SFILE_ID SFILE_1594766561(SFILE_ID);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR TOKEN_strrSTR(TOKEN);
TOKEN SCANNE1792387248(SCANNER);
TUPdAS268679779 PARSER851452257(PARSER, BOOL, BOOL);
dAS_CLASS_ELT PARSER1637480411(PARSER, TOKEN);
dAS_CLASS_ELT PARSER800575691(PARSER);
dAS_EXPR PARSER1259074744(PARSER, BOOL);
dAS_EXPR PARSER403674127(PARSER);
void AS_ARG1328804463(AS_ARG_MODE, AS_ARG_MODE);
void AS_ARG2059479532(AS_ARG_DEC, AS_ARG_DEC);
void AS_FEA814213709(AS_FEAT_MOD, AS_FEAT_MOD);
void AS_PAR769375844(AS_PARAM_DEC, AS_PARAM_DEC);
void AS_TYP1027254244(AS_TYPE_SPEC, AS_TYPE_SPEC);
void BFILE_plus_STR(BFILE, STR);
void CONVER649045550(CONVERT, IDENT);
void CONVER840793676(CONVERT, CHAR);
void PARSER164019780(PARSER, AS_CALL_EXPR, BOOL);
void PROG_err_STR(PROG, STR);
void PROG_warning_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_ARG_DEC PARSER994907623(PARSER self, BOOL is_iter) {
 AS_ARG_DEC ret_val;
 AS_ARG_DEC res = ((AS_ARG_DEC) NULL);
 AS_ARG_MODE mode1 = ((AS_ARG_MODE) NULL);
 AS_ARG_DEC newa;
 AS_TYPE_SPEC tp;
 BOOL hot = BOOL_zero;
 AS_ARG_DEC p;
 PARSER enter_self;
 STR enter_s;
 AS_ARG_DEC create_self;
 AS_ARG_DEC ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val4 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val10;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val11;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val12;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 PARSER ident_self;
 IDENT ret_val13 = IDENT_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val14 = BOOL_zero;
 TOKEN create_self3 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val15 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val16;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val17;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val18 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val19;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val20;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val21;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val22;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val23;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val24 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val25;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val26;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val27;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val28;
 STR create_self5;
 CHAR create_c2 = CHAR_zero;
 STR ret_val29;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER exit_self;
 STR s3;
 extern STR abstractargument;
 AS_ARG_DEC L1;
 OB L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 BOOL L21;
 BOOL L22;
 BOOL L231_;
 extern STR hotarg180607827;
 extern STR in1;
 STR L24;
 INT L25;
 OB L26;
 INT L27;
 INT L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 enter_self = self;
 enter_s = ((STR) &abstractargument);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
  mode1 = PARSER930791008(self, is_iter);
  create_self = ((AS_ARG_DEC) NULL);
  L2=ZALLOC(sizeof(struct AS_ARG_DEC_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=AS_ARG_DEC_tag;
  L1 = ((AS_ARG_DEC) L2);
  ret_val1 = L1;
  newa = ret_val1;
  source_loc_self = self;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(newa,source1,ret_val2);
  match_self = self;
  match_t = PARSER_ident_tok;
  is_eq_self = ATTR(match_self,next);
  is_eq_y = match_t;
  L3 = is_eq_self.val1;
  L41_=(L3)==(is_eq_y); 
  ret_val3 = L41_;
  L5 = ret_val3;
  L61_=!(L5); 
  if (L61_) {
   exp_error_self = match_self;
   create_self1 = TOKEN_zero;
   create_val = match_t;
   t.val1 = create_val;
   ret_val4 = t;
   exp_error_msg = TOKEN_strrSTR(ret_val4);
   error_self = exp_error_self;
   plus_self = exp_error_msg;
   plus_sarg = ((STR) &expectedbutfound);
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val5;
   plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
   error_msg = ret_val6;
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self1 = error_self;
   ret_val7 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val7;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self2 = error_msg;
   plus_sarg2 = ((STR) &in1);
   ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val8;
   plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val9;
   plus_arg = ')';
   str_self = plus_arg;
   create_self2 = ((STR) NULL);
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
   ret_val12 = r;
   ret_val11 = ret_val12;
   s = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
   err_s = ret_val10;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  ident_self = self;
  ret_val13 = ATTR(ATTR(ident_self,scanner),lex_value);
  SATTR(newa,name1,ret_val13);
  SATTR(newa,mode1,mode1);
  if ((res==((AS_ARG_DEC) NULL))) {
   res = newa;
  }
  else {
   AS_ARG2059479532(res, newa);
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 match_self1 = self;
 match_t1 = PARSER_colon_tok;
 is_eq_self1 = ATTR(match_self1,next);
 is_eq_y1 = match_t1;
 L12 = is_eq_self1.val1;
 L131_=(L12)==(is_eq_y1); 
 ret_val14 = L131_;
 L14 = ret_val14;
 L151_=!(L14); 
 if (L151_) {
  exp_error_self1 = match_self1;
  create_self3 = TOKEN_zero;
  create_val1 = match_t1;
  t1.val1 = create_val1;
  ret_val15 = t1;
  exp_error_msg1 = TOKEN_strrSTR(ret_val15);
  error_self1 = exp_error_self1;
  plus_self5 = exp_error_msg1;
  plus_sarg4 = ((STR) &expectedbutfound);
  ret_val16 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val16;
  plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val17 = STR_ap2004550586(plus_self6, plus_sarg5);
  error_msg1 = ret_val17;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self2 = error_self1;
  ret_val18 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val18;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self7 = error_msg1;
  plus_sarg6 = ((STR) &in1);
  ret_val19 = STR_ap2004550586(plus_self7, plus_sarg6);
  plus_self8 = ret_val19;
  plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val20 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val20;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self4 = ((STR) NULL);
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
  r1 = L16;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val23 = r1;
  ret_val22 = ret_val23;
  s1 = ret_val22;
  ret_val21 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val21;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
 }
 fetch_self1 = match_self1;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 tp = PARSER2028498823(self);
 if (ATTR(self,version_1_0)) {
  if (PARSER2128430730(self, PARSER_bang_tok)) {
   L21 = TRUE;
  } else {
   L21 = PARSER2128430730(self, PARSER840587154);
  }
  hot = L21;
  if (hot) {
   L231_=!(is_iter); 
   L22 = L231_;
  } else {
   L22 = FALSE;
  }
  if (L22) {
   error_self2 = self;
   error_msg2 = ((STR) &hotarg180607827);
   set_eloc_self2 = ATTR(error_self2,prog);
   source_loc_self3 = error_self2;
   ret_val24 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l2 = ret_val24;
   SATTR(set_eloc_self2,eloc,set_eloc_l2);
   err_self2 = error_self2;
   plus_self10 = error_msg2;
   plus_sarg8 = ((STR) &in1);
   ret_val25 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val25;
   plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
   ret_val26 = STR_ap2004550586(plus_self11, plus_sarg9);
   plus_self12 = ret_val26;
   plus_arg2 = ')';
   str_self2 = plus_arg2;
   create_self5 = ((STR) NULL);
   create_c2 = str_self2;
   L25 = 1;
   L27=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L25)*sizeof(CHAR);
   L26=ZALLOC_LEAF_BIG(L27);
   if (L26==NULL) FATAL("Unable to allocate more memory");
   memset(L26,0,L27);
   ((OB)L26)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L26)->header.destroyed=0;
#endif

   L24 = ((STR) L26);
   L24->asize = L25;
   r2 = L24;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val29 = r2;
   ret_val28 = ret_val29;
   s2 = ret_val28;
   ret_val27 = STR_ap1077157958(plus_self12, s2, TRUE);
   err_s2 = ret_val27;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
 }
 p = res;
 while (1) {
  if ((p==((AS_ARG_DEC) NULL))) {
   goto after_loop1;
  }
  SATTR(p,tp,tp);
  if (ATTR(self,version_1_0)) {
   SATTR(p,is_hot,hot);
  }
  else {
   if (is_iter) {
    L29 = ATTR(ATTR(p,mode1),mod);
    L30 = AS_ARG551300469;
    L311_=(L29)==(L30); 
    L32 = L311_;
    L331_=!(L32); 
    SATTR(p,is_hot,L331_);
   }
  }
  p = ATTR(p,next);
 }
 after_loop1: ;
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ARG_MODE PARSER930791008(PARSER self, BOOL is_iter) {
 AS_ARG_MODE ret_val;
 INT mode1 = INT_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 PARSER err_self;
 STR err_s;
 AS_ARG_MODE create_self1;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val7;
 AS_ARG_MODE res;
 BOOL L11_;
 extern STR oncear1665539857;
 extern STR in1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 AS_ARG_MODE L7;
 OB L8;
 if (PARSER2128430730(self, PARSER_out_tok)) {
  mode1 = AS_ARG685418392;
 }
 else {
  if (PARSER2128430730(self, PARSER_inout_tok)) {
   mode1 = AS_ARG1872943786;
  }
  else {
   if (PARSER2128430730(self, PARSER_once_tok)) {
    L11_=!(is_iter); 
    if (L11_) {
     error_self = self;
     error_msg = ((STR) &oncear1665539857);
     set_eloc_self = ATTR(error_self,prog);
     source_loc_self = error_self;
     ret_val1 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l = ret_val1;
     SATTR(set_eloc_self,eloc,set_eloc_l);
     err_self = error_self;
     plus_self = error_msg;
     plus_sarg = ((STR) &in1);
     ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val2;
     plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
     ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val3;
     plus_arg = ')';
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
     r = L2;
     SARR((STR)r,0,create_c); 
     ;
     ret_val6 = r;
     ret_val5 = ret_val6;
     s = ret_val5;
     ret_val4 = STR_ap1077157958(plus_self2, s, TRUE);
     err_s = ret_val4;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    mode1 = AS_ARG551300469;
   }
   else {
    mode1 = AS_ARG1300937332;
   }
  }
 }
 create_self1 = ((AS_ARG_MODE) NULL);
 create_m = mode1;
 L8=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=AS_ARG_MODE_tag;
 L7 = ((AS_ARG_MODE) L8);
 res = L7;
 SATTR(res,mod,create_m);
 ret_val7 = res;
 ret_val = ret_val7;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_BOU14269336 PARSER1814172757(PARSER self) {
 AS_BOU14269336 ret_val;
 AS_BOU14269336 res;
 AS_UND152986614 u;
 dAS_EXPR x;
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 CONVERT convert_bind_sel;
 CONVERT set_start_self;
 BFILE flush_self;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 CONVERT convert_bind_sel1;
 CONVERT set_start_self1;
 BFILE flush_self1;
 PARSER source_loc_self1;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 AS_BOU14269336 create_self;
 AS_BOU14269336 ret_val5;
 PARSER source_loc_self2;
 SFILE_ID ret_val6 = SFILE_ID_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val7 = BOOL_zero;
 PARSER fetch_self;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val8 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val9 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val10;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val11;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self3;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val13;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val14;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val15;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val17;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self1;
 AS_UND152986614 create_self3;
 AS_UND152986614 ret_val18;
 PARSER source_loc_self4;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PARSER warning_self;
 STR warning_msg;
 PARSER source_loc_self5;
 SFILE_ID ret_val20 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val21;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val22;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val23;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val24;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val25;
 STR r1;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val28;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val29;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self6;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val31;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val32;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val33;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val34;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val35;
 STR r2;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self2;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self7;
 SFILE_ID ret_val36 = SFILE_ID_zero;
 PROG set_eloc_self5;
 SFILE_ID set_eloc_l5 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val37;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val38;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val39;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val40;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val41;
 STR r3;
 PARSER err_self2;
 STR err_s2;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val42 = BOOL_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val43 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val44 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val45;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val46;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self8;
 SFILE_ID ret_val47 = SFILE_ID_zero;
 PROG set_eloc_self6;
 SFILE_ID set_eloc_l6 = SFILE_ID_zero;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val48;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val49;
 STR plus_self20;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val50;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val51;
 STR create_self9;
 CHAR create_c4 = CHAR_zero;
 STR ret_val52;
 STR r4;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self3;
 PARSER exit_self;
 STR s5;
 extern STR boundc1231439672;
 INT L1;
 BOOL L21_;
 extern STR bind1;
 INT L3;
 INT L41_;
 BOOL L5;
 BOOL L61_;
 extern STR ITERis517564929;
 INT L7;
 BOOL L81_;
 extern STR bind1;
 INT L9;
 INT L101_;
 BOOL L11;
 BOOL L121_;
 extern STR ROUTis1205651111;
 AS_BOU14269336 L13;
 OB L14;
 INT L15;
 BOOL L161_;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 AS_UND152986614 L27;
 OB L28;
 extern STR Sather1601374408;
 extern STR in1;
 STR L29;
 INT L30;
 OB L31;
 INT L32;
 INT L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L38;
 INT L39;
 OB L40;
 INT L42;
 extern STR boundc883440607;
 extern STR in1;
 STR L44;
 INT L45;
 OB L46;
 INT L47;
 BOOL L49;
 BOOL L50;
 BOOL L511_;
 STR L52;
 INT L53;
 INT L541_;
 INT L55;
 CHAR L561_;
 CHAR L57;
 BOOL L581_;
 INT L59;
 BOOL L601_;
 BOOL L62;
 BOOL L631_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L64;
 INT L65;
 OB L66;
 INT L67;
 enter_self = self;
 enter_s = ((STR) &boundc1231439672);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER_ITER_tok;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  convert_bind_sel = ATTR(self,convert);
  if (ATTR(convert_bind_sel,need_to_convert)) {
   CONVER840793676(convert_bind_sel, '#');
   BFILE_plus_STR(ATTR(convert_bind_sel,file1), ((STR) &bind1));
   set_start_self = convert_bind_sel;
   if (ATTR(set_start_self,need_to_convert)) {
    L3 = SFILE_ID_pos;
    L41_=INTMINUS(L3,1); 
    SATTR(set_start_self,start_pos,L41_);
   }
   flush_self = ATTR(convert_bind_sel,file1);
   fflush(ATTR(flush_self,fp));
  }
  L5 = ATTR(self,version_1_0);
  L61_=!(L5); 
  if (L61_) {
   set_eloc_self = ATTR(self,prog);
   source_loc_self = self;
   ret_val2 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val2;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   PROG_warning_STR(ATTR(self,prog), ((STR) &ITERis517564929));
  }
 }
 else {
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER_ROUT_tok;
  L7 = is_eq_self1.val1;
  L81_=(L7)==(is_eq_y1); 
  ret_val3 = L81_;
  if (ret_val3) {
   convert_bind_sel1 = ATTR(self,convert);
   if (ATTR(convert_bind_sel1,need_to_convert)) {
    CONVER840793676(convert_bind_sel1, '#');
    BFILE_plus_STR(ATTR(convert_bind_sel1,file1), ((STR) &bind1));
    set_start_self1 = convert_bind_sel1;
    if (ATTR(set_start_self1,need_to_convert)) {
     L9 = SFILE_ID_pos;
     L101_=INTMINUS(L9,1); 
     SATTR(set_start_self1,start_pos,L101_);
    }
    flush_self1 = ATTR(convert_bind_sel1,file1);
    fflush(ATTR(flush_self1,fp));
   }
   L11 = ATTR(self,version_1_0);
   L121_=!(L11); 
   if (L121_) {
    set_eloc_self1 = ATTR(self,prog);
    source_loc_self1 = self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l1 = ret_val4;
    SATTR(set_eloc_self1,eloc,set_eloc_l1);
    PROG_warning_STR(ATTR(self,prog), ((STR) &ROUTis1205651111));
   }
  }
 }
 create_self = ((AS_BOU14269336) NULL);
 L14=ZALLOC(sizeof(struct AS_BOU14269336_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=AS_BOU14269336_tag;
 L13 = ((AS_BOU14269336) L14);
 ret_val5 = L13;
 res = ret_val5;
 source_loc_self2 = self;
 ret_val6 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val6);
 is_eq_self2 = ATTR(self,next);
 is_eq_y2 = PARSER1337387114;
 L15 = is_eq_self2.val1;
 L161_=(L15)==(is_eq_y2); 
 ret_val7 = L161_;
 if (ret_val7) {
  SATTR(res,is_f_rout,TRUE);
 }
 fetch_self = self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 match_self = self;
 match_t = PARSER1209676627;
 is_eq_self3 = ATTR(match_self,next);
 is_eq_y3 = match_t;
 L17 = is_eq_self3.val1;
 L181_=(L17)==(is_eq_y3); 
 ret_val8 = L181_;
 L19 = ret_val8;
 L201_=!(L19); 
 if (L201_) {
  exp_error_self = match_self;
  create_self1 = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val9 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val9);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val10 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val10;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val11 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val11;
  set_eloc_self2 = ATTR(error_self,prog);
  source_loc_self3 = error_self;
  ret_val12 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val12;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val13 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val13;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val14 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val14;
  plus_arg = ')';
  str_self = plus_arg;
  create_self2 = ((STR) NULL);
  create_c = str_self;
  L23 = 1;
  L25=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L23)*sizeof(CHAR);
  L24=ZALLOC_LEAF_BIG(L25);
  if (L24==NULL) FATAL("Unable to allocate more memory");
  memset(L24,0,L25);
  ((OB)L24)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L24)->header.destroyed=0;
#endif

  L22 = ((STR) L24);
  L22->asize = L23;
  r = L22;
  SARR((STR)r,0,create_c); 
  ;
  ret_val17 = r;
  ret_val16 = ret_val17;
  s = ret_val16;
  ret_val15 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val15;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self1 = match_self;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 if (PARSER2128430730(self, PARSER_under_tok)) {
  create_self3 = ((AS_UND152986614) NULL);
  L28=ZALLOC(sizeof(struct AS_UND152986614_struct));
  if (L28==NULL) FATAL("Unable to allocate more memory");
  ((OB)L28)->header.tag=AS_UND152986614_tag;
  L27 = ((AS_UND152986614) L28);
  ret_val18 = L27;
  u = ret_val18;
  source_loc_self4 = self;
  ret_val19 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(u,source1,ret_val19);
  if (PARSER2128430730(self, PARSER_colon_tok)) {
   SATTR(u,tp,PARSER2028498823(self));
   warning_self = self;
   warning_msg = ((STR) &Sather1601374408);
   set_eloc_self3 = ATTR(warning_self,prog);
   source_loc_self5 = warning_self;
   ret_val20 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l3 = ret_val20;
   SATTR(set_eloc_self3,eloc,set_eloc_l3);
   plus_self5 = warning_msg;
   plus_sarg4 = ((STR) &in1);
   ret_val21 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val21;
   plus_sarg5 = FLISTSTR_toprSTR(ATTR(warning_self,entered));
   ret_val22 = STR_ap2004550586(plus_self6, plus_sarg5);
   plus_self7 = ret_val22;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self4 = ((STR) NULL);
   create_c1 = str_self1;
   L30 = 1;
   L32=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L30)*sizeof(CHAR);
   L31=ZALLOC_LEAF_BIG(L32);
   if (L31==NULL) FATAL("Unable to allocate more memory");
   memset(L31,0,L32);
   ((OB)L31)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L31)->header.destroyed=0;
#endif

   L29 = ((STR) L31);
   L29->asize = L30;
   r1 = L29;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val25 = r1;
   ret_val24 = ret_val25;
   s1 = ret_val24;
   ret_val23 = STR_ap1077157958(plus_self7, s1, TRUE);
   PROG_warning_STR(ATTR(warning_self,prog), ret_val23);
  }
  match_self1 = self;
  match_t1 = PARSER_dot_tok;
  is_eq_self4 = ATTR(match_self1,next);
  is_eq_y4 = match_t1;
  L34 = is_eq_self4.val1;
  L351_=(L34)==(is_eq_y4); 
  ret_val26 = L351_;
  L36 = ret_val26;
  L371_=!(L36); 
  if (L371_) {
   exp_error_self1 = match_self1;
   create_self5 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val27 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val27);
   error_self1 = exp_error_self1;
   plus_self8 = exp_error_msg1;
   plus_sarg6 = ((STR) &expectedbutfound);
   ret_val28 = STR_ap2004550586(plus_self8, plus_sarg6);
   plus_self9 = ret_val28;
   plus_sarg7 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val29 = STR_ap2004550586(plus_self9, plus_sarg7);
   error_msg1 = ret_val29;
   set_eloc_self4 = ATTR(error_self1,prog);
   source_loc_self6 = error_self1;
   ret_val30 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l4 = ret_val30;
   SATTR(set_eloc_self4,eloc,set_eloc_l4);
   err_self1 = error_self1;
   plus_self10 = error_msg1;
   plus_sarg8 = ((STR) &in1);
   ret_val31 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val31;
   plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val32 = STR_ap2004550586(plus_self11, plus_sarg9);
   plus_self12 = ret_val32;
   plus_arg2 = ')';
   str_self2 = plus_arg2;
   create_self6 = ((STR) NULL);
   create_c2 = str_self2;
   L39 = 1;
   L42=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L39)*sizeof(CHAR);
   L40=ZALLOC_LEAF_BIG(L42);
   if (L40==NULL) FATAL("Unable to allocate more memory");
   memset(L40,0,L42);
   ((OB)L40)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L40)->header.destroyed=0;
#endif

   L38 = ((STR) L40);
   L38->asize = L39;
   r2 = L38;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val35 = r2;
   ret_val34 = ret_val35;
   s2 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self12, s2, TRUE);
   err_s1 = ret_val33;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self2 = match_self1;
  SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
  SATTR(res,call1,PARSER716940617(self, ((dAS_EXPR) u), ((AS_TYPE_SPEC) NULL), TRUE));
 }
 else {
  x = PARSER1259074744(self, TRUE);
  if (x==NULL) {
   goto other103;
  } else
  switch (TAG(x)) {
   case AS_CALL_EXPR_tag:
    SATTR(res,call1,((AS_CALL_EXPR) x)); break;
   default: ;
   other103: ;
    error_self2 = self;
    error_msg2 = ((STR) &boundc883440607);
    set_eloc_self5 = ATTR(error_self2,prog);
    source_loc_self7 = error_self2;
    ret_val36 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l5 = ret_val36;
    SATTR(set_eloc_self5,eloc,set_eloc_l5);
    err_self2 = error_self2;
    plus_self13 = error_msg2;
    plus_sarg10 = ((STR) &in1);
    ret_val37 = STR_ap2004550586(plus_self13, plus_sarg10);
    plus_self14 = ret_val37;
    plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
    ret_val38 = STR_ap2004550586(plus_self14, plus_sarg11);
    plus_self15 = ret_val38;
    plus_arg3 = ')';
    str_self3 = plus_arg3;
    create_self7 = ((STR) NULL);
    create_c3 = str_self3;
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
    ret_val41 = r3;
    ret_val40 = ret_val41;
    s3 = ret_val40;
    ret_val39 = STR_ap1077157958(plus_self15, s3, TRUE);
    err_s2 = ret_val39;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
 }
 is_iter_self = ATTR(ATTR(res,call1),name1);
 L50 = (is_iter_self.str==((STR) NULL));
 L511_=!(L50); 
 if (L511_) {
  L52 = is_iter_self.str;
  L53 = STR_sizerINT(is_iter_self.str);
  L541_=INTMINUS(L53,1); 
  L55 = L541_;
  L561_=ARR((STR)L52,L55); 
  L57 = L561_;
  L581_=L57=='!'; 
  L49 = L581_;
 } else {
  L49 = FALSE;
 }
 ret_val42 = L49;
 SATTR(res,is_iter,ret_val42);
 PARSER164019780(self, ATTR(res,call1), ATTR(res,is_iter));
 if (PARSER2128430730(self, PARSER_colon_tok)) {
  SATTR(res,ret,PARSER2028498823(self));
 }
 match_self2 = self;
 match_t2 = PARSER1207020061;
 is_eq_self5 = ATTR(match_self2,next);
 is_eq_y5 = match_t2;
 L59 = is_eq_self5.val1;
 L601_=(L59)==(is_eq_y5); 
 ret_val43 = L601_;
 L62 = ret_val43;
 L631_=!(L62); 
 if (L631_) {
  exp_error_self2 = match_self2;
  create_self8 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val44 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val44);
  error_self3 = exp_error_self2;
  plus_self16 = exp_error_msg2;
  plus_sarg12 = ((STR) &expectedbutfound);
  ret_val45 = STR_ap2004550586(plus_self16, plus_sarg12);
  plus_self17 = ret_val45;
  plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val46 = STR_ap2004550586(plus_self17, plus_sarg13);
  error_msg3 = ret_val46;
  set_eloc_self6 = ATTR(error_self3,prog);
  source_loc_self8 = error_self3;
  ret_val47 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l6 = ret_val47;
  SATTR(set_eloc_self6,eloc,set_eloc_l6);
  err_self3 = error_self3;
  plus_self18 = error_msg3;
  plus_sarg14 = ((STR) &in1);
  ret_val48 = STR_ap2004550586(plus_self18, plus_sarg14);
  plus_self19 = ret_val48;
  plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
  ret_val49 = STR_ap2004550586(plus_self19, plus_sarg15);
  plus_self20 = ret_val49;
  plus_arg4 = ')';
  str_self4 = plus_arg4;
  create_self9 = ((STR) NULL);
  create_c4 = str_self4;
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
  r4 = L64;
  SARR((STR)r4,0,create_c4); 
  ;
  ret_val52 = r4;
  ret_val51 = ret_val52;
  s4 = ret_val51;
  ret_val50 = STR_ap1077157958(plus_self20, s4, TRUE);
  err_s3 = ret_val50;
  PROG_err_STR(ATTR(err_self3,prog), err_s3);
 }
 fetch_self3 = match_self2;
 SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
 exit_self = self;
 s5 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_CALL_EXPR PARSER716940617(PARSER self, dAS_EXPR ob, AS_TYPE_SPEC tp, BOOL underscore_args) {
 AS_CALL_EXPR ret_val;
 AS_CALL_EXPR res = ((AS_CALL_EXPR) NULL);
 TUPdAS268679779 t = TUPdAS268679779_zero;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val5 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val6;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val8 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val9;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val11;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val12;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val13;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 PARSER exit_self;
 STR s1;
 extern STR callexpressions;
 AS_CALL_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 STR L6;
 INT L7;
 INT L81_;
 INT L9;
 CHAR L101_;
 CHAR L11;
 BOOL L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
 enter_self = self;
 enter_s = ((STR) &callexpressions);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_CALL_EXPR) NULL);
 L2=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_CALL_EXPR_tag;
 L1 = ((AS_CALL_EXPR) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 SATTR(res,ob,ob);
 SATTR(res,tp,tp);
 SATTR(res,name1,PARSER814698330(self));
 if (PARSER2128430730(self, PARSER1209676627)) {
  is_iter_self = ATTR(res,name1);
  L4 = (is_iter_self.str==((STR) NULL));
  L51_=!(L4); 
  if (L51_) {
   L6 = is_iter_self.str;
   L7 = STR_sizerINT(is_iter_self.str);
   L81_=INTMINUS(L7,1); 
   L9 = L81_;
   L101_=ARR((STR)L6,L9); 
   L11 = L101_;
   L121_=L11=='!'; 
   L3 = L121_;
  } else {
   L3 = FALSE;
  }
  ret_val3 = L3;
  t = PARSER851452257(self, underscore_args, ret_val3);
  SATTR(res,args,t.t1);
  SATTR(res,modes1,t.t2);
  match_self = self;
  match_t = PARSER1207020061;
  is_eq_self = ATTR(match_self,next);
  is_eq_y = match_t;
  L13 = is_eq_self.val1;
  L141_=(L13)==(is_eq_y); 
  ret_val4 = L141_;
  L15 = ret_val4;
  L161_=!(L15); 
  if (L161_) {
   exp_error_self = match_self;
   create_self1 = TOKEN_zero;
   create_val = match_t;
   t1.val1 = create_val;
   ret_val5 = t1;
   exp_error_msg = TOKEN_strrSTR(ret_val5);
   error_self = exp_error_self;
   plus_self = exp_error_msg;
   plus_sarg = ((STR) &expectedbutfound);
   ret_val6 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val6;
   plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
   error_msg = ret_val7;
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self1 = error_self;
   ret_val8 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val8;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self2 = error_msg;
   plus_sarg2 = ((STR) &in1);
   ret_val9 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val9;
   plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val10 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val10;
   plus_arg = ')';
   str_self = plus_arg;
   create_self2 = ((STR) NULL);
   create_c = str_self;
   L18 = 1;
   L20=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L18)*sizeof(CHAR);
   L19=ZALLOC_LEAF_BIG(L20);
   if (L19==NULL) FATAL("Unable to allocate more memory");
   memset(L19,0,L20);
   ((OB)L19)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L19)->header.destroyed=0;
#endif

   L17 = ((STR) L19);
   L17->asize = L18;
   r = L17;
   SARR((STR)r,0,create_c); 
   ;
   ret_val13 = r;
   ret_val12 = ret_val13;
   s = ret_val12;
   ret_val11 = STR_ap1077157958(plus_self4, s, TRUE);
   err_s = ret_val11;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 }
 exit_self = self;
 s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_CLASS_DEF PARSER1968454579(PARSER self) {
 AS_CLASS_DEF ret_val;
 AS_CLASS_DEF res;
 PARSER enter_self;
 STR enter_s;
 AS_CLASS_DEF create_self;
 AS_CLASS_DEF ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val4 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val10;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val11;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val12;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val13 = BOOL_zero;
 TOKEN create_self3 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val14 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val16;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val17 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val18;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val19;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val20;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val21;
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val22;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val23 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val24 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val25;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val26;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val27 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val28;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val29;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val30;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val31;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val32;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val33;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val34;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val35 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val36;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val37;
 STR plus_self19;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val38;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val39;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val40;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val41 = BOOL_zero;
 PARSER fetch_self3;
 PARSER ident_self;
 IDENT ret_val42 = IDENT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val43 = BOOL_zero;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val44 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val45 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self4;
 STR exp_error_msg4;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val46;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val47;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self5;
 SFILE_ID ret_val48 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self22;
 STR plus_sarg18;
 STR ret_val49;
 STR plus_self23;
 STR plus_sarg19;
 STR ret_val50;
 STR plus_self24;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val51;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val52;
 STR create_self9;
 CHAR create_c4 = CHAR_zero;
 STR ret_val53;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self4;
 PARSER match_self4;
 INT match_t4 = INT_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val54 = BOOL_zero;
 TOKEN create_self10 = TOKEN_zero;
 INT create_val4 = INT_zero;
 TOKEN ret_val55 = TOKEN_zero;
 TOKEN t4 = TOKEN_zero;
 PARSER exp_error_self5;
 STR exp_error_msg5;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val56;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val57;
 PARSER error_self5;
 STR error_msg5;
 PARSER source_loc_self6;
 SFILE_ID ret_val58 = SFILE_ID_zero;
 PROG set_eloc_self5;
 SFILE_ID set_eloc_l5 = SFILE_ID_zero;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val59;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val60;
 STR plus_self29;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val61;
 STR s5;
 CHAR str_self5 = CHAR_zero;
 STR ret_val62;
 STR create_self11;
 CHAR create_c5 = CHAR_zero;
 STR ret_val63;
 STR r5;
 PARSER err_self5;
 STR err_s5;
 PARSER fetch_self5;
 PARSER match_self5;
 INT match_t5 = INT_zero;
 TOKEN is_eq_self7 = TOKEN_zero;
 INT is_eq_y7 = INT_zero;
 BOOL ret_val64 = BOOL_zero;
 TOKEN create_self12 = TOKEN_zero;
 INT create_val5 = INT_zero;
 TOKEN ret_val65 = TOKEN_zero;
 TOKEN t5 = TOKEN_zero;
 PARSER exp_error_self6;
 STR exp_error_msg6;
 STR plus_self30;
 STR plus_sarg24;
 STR ret_val66;
 STR plus_self31;
 STR plus_sarg25;
 STR ret_val67;
 PARSER error_self6;
 STR error_msg6;
 PARSER source_loc_self7;
 SFILE_ID ret_val68 = SFILE_ID_zero;
 PROG set_eloc_self6;
 SFILE_ID set_eloc_l6 = SFILE_ID_zero;
 STR plus_self32;
 STR plus_sarg26;
 STR ret_val69;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val70;
 STR plus_self34;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val71;
 STR s6;
 CHAR str_self6 = CHAR_zero;
 STR ret_val72;
 STR create_self13;
 CHAR create_c6 = CHAR_zero;
 STR ret_val73;
 STR r6;
 PARSER err_self6;
 STR err_s6;
 PARSER fetch_self6;
 PARSER exit_self;
 STR s7;
 extern STR abstra448905526;
 AS_CLASS_DEF L1;
 OB L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 INT L12;
 BOOL L131_;
 BOOL L14;
 BOOL L151_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L16;
 INT L17;
 OB L18;
 INT L19;
 INT L21;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L25;
 INT L26;
 OB L27;
 INT L28;
 extern STR abstracttypename;
 extern STR expectedbutfound;
 extern STR in1;
 STR L30;
 INT L31;
 OB L32;
 INT L33;
 INT L35;
 BOOL L361_;
 INT L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 AS_PARAM_DEC L42;
 BOOL L43;
 BOOL L441_;
 INT L45;
 BOOL L461_;
 BOOL L47;
 BOOL L481_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L49;
 INT L50;
 OB L51;
 INT L52;
 INT L54;
 BOOL L551_;
 BOOL L56;
 BOOL L571_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L58;
 INT L59;
 OB L60;
 INT L62;
 INT L64;
 BOOL L651_;
 BOOL L66;
 BOOL L671_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L68;
 INT L69;
 OB L70;
 INT L71;
 enter_self = self;
 enter_s = ((STR) &abstra448905526);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_CLASS_DEF) NULL);
 L2=ZALLOC(sizeof(struct AS_CLASS_DEF_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_CLASS_DEF_tag;
 L1 = ((AS_CLASS_DEF) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 SATTR(res,kind,AS_CLASS_DEF_abs);
 if (ATTR(self,version_1_0)) {
  match_self = self;
  match_t = PARSER_type_tok;
  is_eq_self = ATTR(match_self,next);
  is_eq_y = match_t;
  L3 = is_eq_self.val1;
  L41_=(L3)==(is_eq_y); 
  ret_val3 = L41_;
  L5 = ret_val3;
  L61_=!(L5); 
  if (L61_) {
   exp_error_self = match_self;
   create_self1 = TOKEN_zero;
   create_val = match_t;
   t.val1 = create_val;
   ret_val4 = t;
   exp_error_msg = TOKEN_strrSTR(ret_val4);
   error_self = exp_error_self;
   plus_self = exp_error_msg;
   plus_sarg = ((STR) &expectedbutfound);
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val5;
   plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
   error_msg = ret_val6;
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self1 = error_self;
   ret_val7 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val7;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self2 = error_msg;
   plus_sarg2 = ((STR) &in1);
   ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val8;
   plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val9;
   plus_arg = ')';
   str_self = plus_arg;
   create_self2 = ((STR) NULL);
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
   ret_val12 = r;
   ret_val11 = ret_val12;
   s = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
   err_s = ret_val10;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 }
 else {
  match_self1 = self;
  match_t1 = PARSER1701976116;
  is_eq_self1 = ATTR(match_self1,next);
  is_eq_y1 = match_t1;
  L12 = is_eq_self1.val1;
  L131_=(L12)==(is_eq_y1); 
  ret_val13 = L131_;
  L14 = ret_val13;
  L151_=!(L14); 
  if (L151_) {
   exp_error_self1 = match_self1;
   create_self3 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val14 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val14);
   error_self1 = exp_error_self1;
   plus_self5 = exp_error_msg1;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val15 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val15;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val16 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val16;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self2 = error_self1;
   ret_val17 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l1 = ret_val17;
   SATTR(set_eloc_self1,eloc,set_eloc_l1);
   err_self1 = error_self1;
   plus_self7 = error_msg1;
   plus_sarg6 = ((STR) &in1);
   ret_val18 = STR_ap2004550586(plus_self7, plus_sarg6);
   plus_self8 = ret_val18;
   plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val19 = STR_ap2004550586(plus_self8, plus_sarg7);
   plus_self9 = ret_val19;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self4 = ((STR) NULL);
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
   r1 = L16;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val22 = r1;
   ret_val21 = ret_val22;
   s1 = ret_val21;
   ret_val20 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val20;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self1 = match_self1;
  SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  match_self2 = self;
  match_t2 = PARSER_class_tok;
  is_eq_self2 = ATTR(match_self2,next);
  is_eq_y2 = match_t2;
  L21 = is_eq_self2.val1;
  L221_=(L21)==(is_eq_y2); 
  ret_val23 = L221_;
  L23 = ret_val23;
  L241_=!(L23); 
  if (L241_) {
   exp_error_self2 = match_self2;
   create_self5 = TOKEN_zero;
   create_val2 = match_t2;
   t2.val1 = create_val2;
   ret_val24 = t2;
   exp_error_msg2 = TOKEN_strrSTR(ret_val24);
   error_self2 = exp_error_self2;
   plus_self10 = exp_error_msg2;
   plus_sarg8 = ((STR) &expectedbutfound);
   ret_val25 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val25;
   plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
   ret_val26 = STR_ap2004550586(plus_self11, plus_sarg9);
   error_msg2 = ret_val26;
   set_eloc_self2 = ATTR(error_self2,prog);
   source_loc_self3 = error_self2;
   ret_val27 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l2 = ret_val27;
   SATTR(set_eloc_self2,eloc,set_eloc_l2);
   err_self2 = error_self2;
   plus_self12 = error_msg2;
   plus_sarg10 = ((STR) &in1);
   ret_val28 = STR_ap2004550586(plus_self12, plus_sarg10);
   plus_self13 = ret_val28;
   plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
   ret_val29 = STR_ap2004550586(plus_self13, plus_sarg11);
   plus_self14 = ret_val29;
   plus_arg2 = ')';
   str_self2 = plus_arg2;
   create_self6 = ((STR) NULL);
   create_c2 = str_self2;
   L26 = 1;
   L28=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L26)*sizeof(CHAR);
   L27=ZALLOC_LEAF_BIG(L28);
   if (L27==NULL) FATAL("Unable to allocate more memory");
   memset(L27,0,L28);
   ((OB)L27)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L27)->header.destroyed=0;
#endif

   L25 = ((STR) L27);
   L25->asize = L26;
   r2 = L25;
   SARR((STR)r2,0,create_c2); 
   ;
   ret_val32 = r2;
   ret_val31 = ret_val32;
   s2 = ret_val31;
   ret_val30 = STR_ap1077157958(plus_self14, s2, TRUE);
   err_s2 = ret_val30;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
  fetch_self2 = match_self2;
  SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 }
 if (PARSER2128430730(self, PARSER978809429)) {
 }
 else {
  exp_error_self3 = self;
  exp_error_msg3 = ((STR) &abstracttypename);
  error_self3 = exp_error_self3;
  plus_self15 = exp_error_msg3;
  plus_sarg12 = ((STR) &expectedbutfound);
  ret_val33 = STR_ap2004550586(plus_self15, plus_sarg12);
  plus_self16 = ret_val33;
  plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
  ret_val34 = STR_ap2004550586(plus_self16, plus_sarg13);
  error_msg3 = ret_val34;
  set_eloc_self3 = ATTR(error_self3,prog);
  source_loc_self4 = error_self3;
  ret_val35 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l3 = ret_val35;
  SATTR(set_eloc_self3,eloc,set_eloc_l3);
  err_self3 = error_self3;
  plus_self17 = error_msg3;
  plus_sarg14 = ((STR) &in1);
  ret_val36 = STR_ap2004550586(plus_self17, plus_sarg14);
  plus_self18 = ret_val36;
  plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
  ret_val37 = STR_ap2004550586(plus_self18, plus_sarg15);
  plus_self19 = ret_val37;
  plus_arg3 = ')';
  str_self3 = plus_arg3;
  create_self7 = ((STR) NULL);
  create_c3 = str_self3;
  L31 = 1;
  L33=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L31)*sizeof(CHAR);
  L32=ZALLOC_LEAF_BIG(L33);
  if (L32==NULL) FATAL("Unable to allocate more memory");
  memset(L32,0,L33);
  ((OB)L32)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L32)->header.destroyed=0;
#endif

  L30 = ((STR) L32);
  L30->asize = L31;
  r3 = L30;
  SARR((STR)r3,0,create_c3); 
  ;
  ret_val40 = r3;
  ret_val39 = ret_val40;
  s3 = ret_val39;
  ret_val38 = STR_ap1077157958(plus_self19, s3, TRUE);
  err_s3 = ret_val38;
  PROG_err_STR(ATTR(err_self3,prog), err_s3);
  is_eq_self3 = ATTR(self,next);
  is_eq_y3 = PARSER_ident_tok;
  L35 = is_eq_self3.val1;
  L361_=(L35)==(is_eq_y3); 
  ret_val41 = L361_;
  if (ret_val41) {
   fetch_self3 = self;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
  }
 }
 ident_self = self;
 ret_val42 = ATTR(ATTR(ident_self,scanner),lex_value);
 SATTR(res,name1,ret_val42);
 if (PARSER2128430730(self, PARSER2001381604)) {
  while (1) {
   is_eq_self4 = ATTR(self,next);
   is_eq_y4 = PARSER_ident_tok;
   L37 = is_eq_self4.val1;
   L381_=(L37)==(is_eq_y4); 
   ret_val43 = L381_;
   L39 = ret_val43;
   L401_=!(L39); 
   if (L401_) {
    goto after_loop;
   }
   if ((ATTR(res,params)==((AS_PARAM_DEC) NULL))) {
    SATTR(res,params,PARSER1105068487(self));
   }
   else {
    L42 = ATTR(res,params);
    AS_PAR769375844(L42, PARSER1105068487(self));
   }
   L43 = PARSER2128430730(self, PARSER_comma_tok);
   L441_=!(L43); 
   if (L441_) {
    goto after_loop;
   }
  }
  after_loop: ;
  match_self3 = self;
  match_t3 = PARSER1998725038;
  is_eq_self5 = ATTR(match_self3,next);
  is_eq_y5 = match_t3;
  L45 = is_eq_self5.val1;
  L461_=(L45)==(is_eq_y5); 
  ret_val44 = L461_;
  L47 = ret_val44;
  L481_=!(L47); 
  if (L481_) {
   exp_error_self4 = match_self3;
   create_self8 = TOKEN_zero;
   create_val3 = match_t3;
   t3.val1 = create_val3;
   ret_val45 = t3;
   exp_error_msg4 = TOKEN_strrSTR(ret_val45);
   error_self4 = exp_error_self4;
   plus_self20 = exp_error_msg4;
   plus_sarg16 = ((STR) &expectedbutfound);
   ret_val46 = STR_ap2004550586(plus_self20, plus_sarg16);
   plus_self21 = ret_val46;
   plus_sarg17 = TOKEN_strrSTR(ATTR(exp_error_self4,next));
   ret_val47 = STR_ap2004550586(plus_self21, plus_sarg17);
   error_msg4 = ret_val47;
   set_eloc_self4 = ATTR(error_self4,prog);
   source_loc_self5 = error_self4;
   ret_val48 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l4 = ret_val48;
   SATTR(set_eloc_self4,eloc,set_eloc_l4);
   err_self4 = error_self4;
   plus_self22 = error_msg4;
   plus_sarg18 = ((STR) &in1);
   ret_val49 = STR_ap2004550586(plus_self22, plus_sarg18);
   plus_self23 = ret_val49;
   plus_sarg19 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
   ret_val50 = STR_ap2004550586(plus_self23, plus_sarg19);
   plus_self24 = ret_val50;
   plus_arg4 = ')';
   str_self4 = plus_arg4;
   create_self9 = ((STR) NULL);
   create_c4 = str_self4;
   L50 = 1;
   L52=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L50)*sizeof(CHAR);
   L51=ZALLOC_LEAF_BIG(L52);
   if (L51==NULL) FATAL("Unable to allocate more memory");
   memset(L51,0,L52);
   ((OB)L51)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L51)->header.destroyed=0;
#endif

   L49 = ((STR) L51);
   L49->asize = L50;
   r4 = L49;
   SARR((STR)r4,0,create_c4); 
   ;
   ret_val53 = r4;
   ret_val52 = ret_val53;
   s4 = ret_val52;
   ret_val51 = STR_ap1077157958(plus_self24, s4, TRUE);
   err_s4 = ret_val51;
   PROG_err_STR(ATTR(err_self4,prog), err_s4);
  }
  fetch_self4 = match_self3;
  SATTR(fetch_self4,next,SCANNE1792387248(ATTR(fetch_self4,scanner)));
 }
 if (PARSER2128430730(self, PARSER_is_lt_tok)) {
  SATTR(res,under,PARSER532091533(self));
 }
 if (PARSER2128430730(self, PARSER_is_gt_tok)) {
  SATTR(res,over,PARSER532091533(self));
 }
 match_self4 = self;
 match_t4 = PARSER_is_tok;
 is_eq_self6 = ATTR(match_self4,next);
 is_eq_y6 = match_t4;
 L54 = is_eq_self6.val1;
 L551_=(L54)==(is_eq_y6); 
 ret_val54 = L551_;
 L56 = ret_val54;
 L571_=!(L56); 
 if (L571_) {
  exp_error_self5 = match_self4;
  create_self10 = TOKEN_zero;
  create_val4 = match_t4;
  t4.val1 = create_val4;
  ret_val55 = t4;
  exp_error_msg5 = TOKEN_strrSTR(ret_val55);
  error_self5 = exp_error_self5;
  plus_self25 = exp_error_msg5;
  plus_sarg20 = ((STR) &expectedbutfound);
  ret_val56 = STR_ap2004550586(plus_self25, plus_sarg20);
  plus_self26 = ret_val56;
  plus_sarg21 = TOKEN_strrSTR(ATTR(exp_error_self5,next));
  ret_val57 = STR_ap2004550586(plus_self26, plus_sarg21);
  error_msg5 = ret_val57;
  set_eloc_self5 = ATTR(error_self5,prog);
  source_loc_self6 = error_self5;
  ret_val58 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l5 = ret_val58;
  SATTR(set_eloc_self5,eloc,set_eloc_l5);
  err_self5 = error_self5;
  plus_self27 = error_msg5;
  plus_sarg22 = ((STR) &in1);
  ret_val59 = STR_ap2004550586(plus_self27, plus_sarg22);
  plus_self28 = ret_val59;
  plus_sarg23 = FLISTSTR_toprSTR(ATTR(error_self5,entered));
  ret_val60 = STR_ap2004550586(plus_self28, plus_sarg23);
  plus_self29 = ret_val60;
  plus_arg5 = ')';
  str_self5 = plus_arg5;
  create_self11 = ((STR) NULL);
  create_c5 = str_self5;
  L59 = 1;
  L62=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L59)*sizeof(CHAR);
  L60=ZALLOC_LEAF_BIG(L62);
  if (L60==NULL) FATAL("Unable to allocate more memory");
  memset(L60,0,L62);
  ((OB)L60)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L60)->header.destroyed=0;
#endif

  L58 = ((STR) L60);
  L58->asize = L59;
  r5 = L58;
  SARR((STR)r5,0,create_c5); 
  ;
  ret_val63 = r5;
  ret_val62 = ret_val63;
  s5 = ret_val62;
  ret_val61 = STR_ap1077157958(plus_self29, s5, TRUE);
  err_s5 = ret_val61;
  PROG_err_STR(ATTR(err_self5,prog), err_s5);
 }
 fetch_self5 = match_self4;
 SATTR(fetch_self5,next,SCANNE1792387248(ATTR(fetch_self5,scanner)));
 SATTR(res,body,PARSER800575691(self));
 match_self5 = self;
 match_t5 = PARSER_end_tok;
 is_eq_self7 = ATTR(match_self5,next);
 is_eq_y7 = match_t5;
 L64 = is_eq_self7.val1;
 L651_=(L64)==(is_eq_y7); 
 ret_val64 = L651_;
 L66 = ret_val64;
 L671_=!(L66); 
 if (L671_) {
  exp_error_self6 = match_self5;
  create_self12 = TOKEN_zero;
  create_val5 = match_t5;
  t5.val1 = create_val5;
  ret_val65 = t5;
  exp_error_msg6 = TOKEN_strrSTR(ret_val65);
  error_self6 = exp_error_self6;
  plus_self30 = exp_error_msg6;
  plus_sarg24 = ((STR) &expectedbutfound);
  ret_val66 = STR_ap2004550586(plus_self30, plus_sarg24);
  plus_self31 = ret_val66;
  plus_sarg25 = TOKEN_strrSTR(ATTR(exp_error_self6,next));
  ret_val67 = STR_ap2004550586(plus_self31, plus_sarg25);
  error_msg6 = ret_val67;
  set_eloc_self6 = ATTR(error_self6,prog);
  source_loc_self7 = error_self6;
  ret_val68 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l6 = ret_val68;
  SATTR(set_eloc_self6,eloc,set_eloc_l6);
  err_self6 = error_self6;
  plus_self32 = error_msg6;
  plus_sarg26 = ((STR) &in1);
  ret_val69 = STR_ap2004550586(plus_self32, plus_sarg26);
  plus_self33 = ret_val69;
  plus_sarg27 = FLISTSTR_toprSTR(ATTR(error_self6,entered));
  ret_val70 = STR_ap2004550586(plus_self33, plus_sarg27);
  plus_self34 = ret_val70;
  plus_arg6 = ')';
  str_self6 = plus_arg6;
  create_self13 = ((STR) NULL);
  create_c6 = str_self6;
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
  r6 = L68;
  SARR((STR)r6,0,create_c6); 
  ;
  ret_val73 = r6;
  ret_val72 = ret_val73;
  s6 = ret_val72;
  ret_val71 = STR_ap1077157958(plus_self34, s6, TRUE);
  err_s6 = ret_val71;
  PROG_err_STR(ATTR(err_self6,prog), err_s6);
 }
 fetch_self6 = match_self5;
 SATTR(fetch_self6,next,SCANNE1792387248(ATTR(fetch_self6,scanner)));
 exit_self = self;
 s7 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_CREATE_EXPR PARSER2102248401(PARSER self) {
 AS_CREATE_EXPR ret_val;
 AS_CREATE_EXPR res;
 TUPdAS268679779 t = TUPdAS268679779_zero;
 PARSER enter_self;
 STR enter_s;
 AS_CREATE_EXPR create_self;
 AS_CREATE_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val5 = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val6 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val7;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val9;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val10;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val11;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val12 = BOOL_zero;
 TOKEN create_self2 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val13 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val14;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val15;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val19;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val20;
 STR create_self3;
 CHAR create_c1 = CHAR_zero;
 STR ret_val21;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self;
 PARSER exit_self;
 STR s2;
 extern STR createexpression;
 AS_CREATE_EXPR L1;
 OB L2;
 BOOL L3;
 INT L4;
 BOOL L51_;
 INT L6;
 BOOL L71_;
 INT L8;
 BOOL L91_;
 extern STR noabst1006226423;
 extern STR in1;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 INT L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L19;
 INT L20;
 OB L21;
 INT L22;
 enter_self = self;
 enter_s = ((STR) &createexpression);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_CREATE_EXPR) NULL);
 L2=ZALLOC(sizeof(struct AS_CREATE_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_CREATE_EXPR_tag;
 L1 = ((AS_CREATE_EXPR) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER_ident_tok;
 L4 = is_eq_self.val1;
 L51_=(L4)==(is_eq_y); 
 ret_val3 = L51_;
 if (ret_val3) {
  L3 = TRUE;
 } else {
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER_SAME_tok;
  L6 = is_eq_self1.val1;
  L71_=(L6)==(is_eq_y1); 
  ret_val4 = L71_;
  L3 = ret_val4;
 }
 if (L3) {
  SATTR(res,tp,PARSER2028498823(self));
 }
 else {
  is_eq_self2 = ATTR(self,next);
  is_eq_y2 = PARSER978809429;
  L8 = is_eq_self2.val1;
  L91_=(L8)==(is_eq_y2); 
  ret_val5 = L91_;
  if (ret_val5) {
   SATTR(res,tp,PARSER2028498823(self));
   error_self = self;
   error_msg = ((STR) &noabst1006226423);
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self1 = error_self;
   ret_val6 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val6;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self = error_msg;
   plus_sarg = ((STR) &in1);
   ret_val7 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val7;
   plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val8;
   plus_arg = ')';
   str_self = plus_arg;
   create_self1 = ((STR) NULL);
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
   ret_val11 = r;
   ret_val10 = ret_val11;
   s = ret_val10;
   ret_val9 = STR_ap1077157958(plus_self2, s, TRUE);
   err_s = ret_val9;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
 }
 if (PARSER2128430730(self, PARSER1209676627)) {
  t = PARSER851452257(self, FALSE, FALSE);
  SATTR(res,elts,t.t1);
  SATTR(res,modes1,t.t2);
  match_self = self;
  match_t = PARSER1207020061;
  is_eq_self3 = ATTR(match_self,next);
  is_eq_y3 = match_t;
  L15 = is_eq_self3.val1;
  L161_=(L15)==(is_eq_y3); 
  ret_val12 = L161_;
  L17 = ret_val12;
  L181_=!(L17); 
  if (L181_) {
   exp_error_self = match_self;
   create_self2 = TOKEN_zero;
   create_val = match_t;
   t1.val1 = create_val;
   ret_val13 = t1;
   exp_error_msg = TOKEN_strrSTR(ret_val13);
   error_self1 = exp_error_self;
   plus_self3 = exp_error_msg;
   plus_sarg2 = ((STR) &expectedbutfound);
   ret_val14 = STR_ap2004550586(plus_self3, plus_sarg2);
   plus_self4 = ret_val14;
   plus_sarg3 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val15 = STR_ap2004550586(plus_self4, plus_sarg3);
   error_msg1 = ret_val15;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self2 = error_self1;
   ret_val16 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l1 = ret_val16;
   SATTR(set_eloc_self1,eloc,set_eloc_l1);
   err_self1 = error_self1;
   plus_self5 = error_msg1;
   plus_sarg4 = ((STR) &in1);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   plus_self7 = ret_val18;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self3 = ((STR) NULL);
   create_c1 = str_self1;
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
   r1 = L19;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val21 = r1;
   ret_val20 = ret_val21;
   s1 = ret_val20;
   ret_val19 = STR_ap1077157958(plus_self7, s1, TRUE);
   err_s1 = ret_val19;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 }
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_PARAM_DEC PARSER1105068487(PARSER self) {
 AS_PARAM_DEC ret_val;
 AS_PARAM_DEC res;
 PARSER enter_self;
 STR enter_s;
 AS_PARAM_DEC create_self;
 AS_PARAM_DEC ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val4 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val10;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val11;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val12;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 PARSER ident_self;
 IDENT ret_val13 = IDENT_zero;
 PARSER is_class_name_se;
 IDENT is_class_name_x = IDENT_zero;
 BOOL ret_val14 = BOOL_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val16;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val17 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val18;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val19;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val20;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val21;
 STR create_self3;
 CHAR create_c1 = CHAR_zero;
 STR ret_val22;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER exit_self;
 STR s2;
 extern STR parame312373581;
 AS_PARAM_DEC L1;
 OB L2;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 BOOL L12;
 BOOL L131_;
 extern STR classname;
 extern STR expectedbutfound;
 extern STR in1;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 enter_self = self;
 enter_s = ((STR) &parame312373581);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_PARAM_DEC) NULL);
 L2=ZALLOC(sizeof(struct AS_PARAM_DEC_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_PARAM_DEC_tag;
 L1 = ((AS_PARAM_DEC) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 match_self = self;
 match_t = PARSER_ident_tok;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L3 = is_eq_self.val1;
 L41_=(L3)==(is_eq_y); 
 ret_val3 = L41_;
 L5 = ret_val3;
 L61_=!(L5); 
 if (L61_) {
  exp_error_self = match_self;
  create_self1 = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val4 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val4);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val5;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val6;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self1 = error_self;
  ret_val7 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val7;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val8;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val9;
  plus_arg = ')';
  str_self = plus_arg;
  create_self2 = ((STR) NULL);
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
  ret_val12 = r;
  ret_val11 = ret_val12;
  s = ret_val11;
  ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val10;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 ident_self = self;
 ret_val13 = ATTR(ATTR(ident_self,scanner),lex_value);
 SATTR(res,name1,ret_val13);
 is_class_name_se = self;
 is_class_name_x = ATTR(res,name1);
 ret_val14 = SCANNE363215919(ATTR(is_class_name_se,scanner), is_class_name_x.str);
 L12 = ret_val14;
 L131_=!(L12); 
 if (L131_) {
  exp_error_self1 = self;
  exp_error_msg1 = ((STR) &classname);
  error_self1 = exp_error_self1;
  plus_self5 = exp_error_msg1;
  plus_sarg4 = ((STR) &expectedbutfound);
  ret_val15 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val15;
  plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val16 = STR_ap2004550586(plus_self6, plus_sarg5);
  error_msg1 = ret_val16;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self2 = error_self1;
  ret_val17 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val17;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self7 = error_msg1;
  plus_sarg6 = ((STR) &in1);
  ret_val18 = STR_ap2004550586(plus_self7, plus_sarg6);
  plus_self8 = ret_val18;
  plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val19 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val19;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self3 = ((STR) NULL);
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
  ret_val22 = r1;
  ret_val21 = ret_val22;
  s1 = ret_val21;
  ret_val20 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val20;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
 }
 if (PARSER2128430730(self, PARSER_is_lt_tok)) {
  SATTR(res,type_constraint,PARSER2028498823(self));
 }
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ROUT_DEF PARSER2039100746(PARSER self) {
 AS_ROUT_DEF ret_val;
 AS_ROUT_DEF res;
 PARSER enter_self;
 STR enter_s;
 AS_ROUT_DEF create_self;
 AS_ROUT_DEF ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER enter_self1;
 STR enter_s1;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val3 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val4 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val5 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 CONVERT set_start_self;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val7 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val8 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val11 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val12;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val13;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val14;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val15;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val16;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 PARSER exit_self;
 STR s1;
 PARSER enter_self2;
 STR enter_s2;
 PARSER exit_self1;
 STR s2;
 PARSER exit_self2;
 STR s3;
 extern STR abstra2044286314;
 AS_ROUT_DEF L1;
 OB L2;
 extern STR abstra948212288;
 BOOL L3;
 BOOL L4;
 BOOL L51_;
 STR L6;
 INT L7;
 INT L81_;
 INT L9;
 CHAR L101_;
 CHAR L11;
 BOOL L121_;
 BOOL L13;
 BOOL L14;
 BOOL L151_;
 STR L16;
 INT L17;
 INT L181_;
 INT L19;
 CHAR L201_;
 CHAR L21;
 BOOL L221_;
 AS_ARG_DEC L23;
 BOOL L24;
 BOOL L25;
 BOOL L261_;
 STR L27;
 INT L28;
 INT L291_;
 INT L30;
 CHAR L311_;
 CHAR L32;
 BOOL L331_;
 BOOL L34;
 BOOL L35;
 BOOL L361_;
 STR L37;
 INT L38;
 INT L391_;
 INT L40;
 CHAR L411_;
 CHAR L42;
 BOOL L431_;
 INT L44;
 INT L451_;
 INT L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L50;
 INT L52;
 OB L53;
 INT L54;
 extern STR return1163638156;
 enter_self = self;
 enter_s = ((STR) &abstra2044286314);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_ROUT_DEF) NULL);
 L2=ZALLOC(sizeof(struct AS_ROUT_DEF_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_ROUT_DEF_tag;
 L1 = ((AS_ROUT_DEF) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 SATTR(res,is_abstract,TRUE);
 SATTR(res,name1,PARSER814698330(self));
 if (PARSER2128430730(self, PARSER1209676627)) {
  enter_self1 = self;
  enter_s1 = ((STR) &abstra948212288);
  SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
  is_iter_self = ATTR(res,name1);
  L4 = (is_iter_self.str==((STR) NULL));
  L51_=!(L4); 
  if (L51_) {
   L6 = is_iter_self.str;
   L7 = STR_sizerINT(is_iter_self.str);
   L81_=INTMINUS(L7,1); 
   L9 = L81_;
   L101_=ARR((STR)L6,L9); 
   L11 = L101_;
   L121_=L11=='!'; 
   L3 = L121_;
  } else {
   L3 = FALSE;
  }
  ret_val3 = L3;
  if (ret_val3) {
   CONVER649045550(ATTR(self,convert), ATTR(res,name1));
  }
  while (1) {
   if ((ATTR(res,args_dec)==((AS_ARG_DEC) NULL))) {
    is_iter_self1 = ATTR(res,name1);
    L14 = (is_iter_self1.str==((STR) NULL));
    L151_=!(L14); 
    if (L151_) {
     L16 = is_iter_self1.str;
     L17 = STR_sizerINT(is_iter_self1.str);
     L181_=INTMINUS(L17,1); 
     L19 = L181_;
     L201_=ARR((STR)L16,L19); 
     L21 = L201_;
     L221_=L21=='!'; 
     L13 = L221_;
    } else {
     L13 = FALSE;
    }
    ret_val4 = L13;
    SATTR(res,args_dec,PARSER994907623(self, ret_val4));
   }
   else {
    L23 = ATTR(res,args_dec);
    is_iter_self2 = ATTR(res,name1);
    L25 = (is_iter_self2.str==((STR) NULL));
    L261_=!(L25); 
    if (L261_) {
     L27 = is_iter_self2.str;
     L28 = STR_sizerINT(is_iter_self2.str);
     L291_=INTMINUS(L28,1); 
     L30 = L291_;
     L311_=ARR((STR)L27,L30); 
     L32 = L311_;
     L331_=L32=='!'; 
     L24 = L331_;
    } else {
     L24 = FALSE;
    }
    ret_val5 = L24;
    AS_ARG2059479532(L23, PARSER994907623(self, ret_val5));
   }
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop;
   }
  }
  after_loop: ;
  is_iter_self3 = ATTR(res,name1);
  L35 = (is_iter_self3.str==((STR) NULL));
  L361_=!(L35); 
  if (L361_) {
   L37 = is_iter_self3.str;
   L38 = STR_sizerINT(is_iter_self3.str);
   L391_=INTMINUS(L38,1); 
   L40 = L391_;
   L411_=ARR((STR)L37,L40); 
   L42 = L411_;
   L431_=L42=='!'; 
   L34 = L431_;
  } else {
   L34 = FALSE;
  }
  ret_val6 = L34;
  if (ret_val6) {
   set_start_self = ATTR(self,convert);
   if (ATTR(set_start_self,need_to_convert)) {
    L44 = SFILE_ID_pos;
    L451_=INTMINUS(L44,1); 
    SATTR(set_start_self,start_pos,L451_);
   }
  }
  match_self = self;
  match_t = PARSER1207020061;
  is_eq_self = ATTR(match_self,next);
  is_eq_y = match_t;
  L46 = is_eq_self.val1;
  L471_=(L46)==(is_eq_y); 
  ret_val7 = L471_;
  L48 = ret_val7;
  L491_=!(L48); 
  if (L491_) {
   exp_error_self = match_self;
   create_self1 = TOKEN_zero;
   create_val = match_t;
   t.val1 = create_val;
   ret_val8 = t;
   exp_error_msg = TOKEN_strrSTR(ret_val8);
   error_self = exp_error_self;
   plus_self = exp_error_msg;
   plus_sarg = ((STR) &expectedbutfound);
   ret_val9 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val9;
   plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
   error_msg = ret_val10;
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self1 = error_self;
   ret_val11 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val11;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self2 = error_msg;
   plus_sarg2 = ((STR) &in1);
   ret_val12 = STR_ap2004550586(plus_self2, plus_sarg2);
   plus_self3 = ret_val12;
   plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val13 = STR_ap2004550586(plus_self3, plus_sarg3);
   plus_self4 = ret_val13;
   plus_arg = ')';
   str_self = plus_arg;
   create_self2 = ((STR) NULL);
   create_c = str_self;
   L52 = 1;
   L54=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L52)*sizeof(CHAR);
   L53=ZALLOC_LEAF_BIG(L54);
   if (L53==NULL) FATAL("Unable to allocate more memory");
   memset(L53,0,L54);
   ((OB)L53)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L53)->header.destroyed=0;
#endif

   L50 = ((STR) L53);
   L50->asize = L52;
   r = L50;
   SARR((STR)r,0,create_c); 
   ;
   ret_val16 = r;
   ret_val15 = ret_val16;
   s = ret_val15;
   ret_val14 = STR_ap1077157958(plus_self4, s, TRUE);
   err_s = ret_val14;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  exit_self = self;
  s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 }
 if (PARSER2128430730(self, PARSER_colon_tok)) {
  enter_self2 = self;
  enter_s2 = ((STR) &return1163638156);
  SATTR(enter_self2,entered,FLISTS1425610062(ATTR(enter_self2,entered), enter_s2));
  SATTR(res,ret_dec,PARSER2028498823(self));
  exit_self1 = self;
  s2 = FLISTSTR_poprSTR(ATTR(exit_self1,entered));
 }
 exit_self2 = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self2,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC PARSER2028498823(PARSER self) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC res;
 BOOL isiter = BOOL_zero;
 AS_ARG_MODE m;
 AS_TYPE_SPEC tp;
 PARSER enter_self;
 STR enter_s;
 AS_TYPE_SPEC create_self;
 AS_TYPE_SPEC ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val5 = BOOL_zero;
 PARSER ident_self;
 IDENT ret_val6 = IDENT_zero;
 PARSER is_class_name_se;
 IDENT is_class_name_x = IDENT_zero;
 BOOL ret_val7 = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val8 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val9;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val10;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val11;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val12;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val13;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER ident_self1;
 IDENT ret_val14 = IDENT_zero;
 PARSER fetch_self;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN create_self2 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val16 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val17;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val20;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val21;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val22;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val23;
 STR create_self3;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val25;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val26;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val27 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val28;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val29;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val30;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val31;
 STR create_self4;
 CHAR create_c2 = CHAR_zero;
 STR ret_val32;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val33 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val34;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val35;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val36;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val37;
 STR create_self5;
 CHAR create_c3 = CHAR_zero;
 STR ret_val38;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val39 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val40 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val41;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val42;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self5;
 SFILE_ID ret_val43 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val44;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val45;
 STR plus_self20;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val46;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val47;
 STR create_self7;
 CHAR create_c4 = CHAR_zero;
 STR ret_val48;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self2;
 PARSER exit_self;
 STR s5;
 extern STR typesp2007762927;
 AS_TYPE_SPEC L1;
 OB L2;
 BOOL L3;
 INT L4;
 BOOL L51_;
 INT L6;
 BOOL L71_;
 BOOL L8;
 INT L9;
 BOOL L101_;
 BOOL L11;
 BOOL L121_;
 extern STR classn393814273;
 extern STR in1;
 STR L13;
 INT L14;
 OB L15;
 INT L16;
 INT L18;
 BOOL L191_;
 BOOL L20;
 BOOL L211_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L22;
 INT L23;
 OB L24;
 INT L25;
 extern STR typespecifier;
 extern STR expectedbutfound;
 extern STR in1;
 STR L27;
 INT L28;
 OB L29;
 INT L30;
 INT L32;
 INT L33;
 BOOL L341_;
 BOOL L35;
 INT L36;
 INT L37;
 BOOL L381_;
 extern STR nohota2092101541;
 extern STR in1;
 STR L39;
 INT L40;
 OB L41;
 INT L42;
 INT L44;
 INT L45;
 BOOL L461_;
 BOOL L47;
 BOOL L481_;
 INT L49;
 BOOL L501_;
 BOOL L52;
 BOOL L531_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L54;
 INT L55;
 OB L56;
 INT L57;
 enter_self = self;
 enter_s = ((STR) &typesp2007762927);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_TYPE_SPEC) NULL);
 L2=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_TYPE_SPEC_tag;
 L1 = ((AS_TYPE_SPEC) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 if (PARSER2128430730(self, PARSER_SAME_tok)) {
  SATTR(res,kind,AS_TYP1853679690);
 }
 else {
  is_eq_self = ATTR(self,next);
  is_eq_y = PARSER978809429;
  L4 = is_eq_self.val1;
  L51_=(L4)==(is_eq_y); 
  ret_val3 = L51_;
  if (ret_val3) {
   L3 = TRUE;
  } else {
   is_eq_self1 = ATTR(self,next);
   is_eq_y1 = PARSER_ident_tok;
   L6 = is_eq_self1.val1;
   L71_=(L6)==(is_eq_y1); 
   ret_val4 = L71_;
   L3 = ret_val4;
  }
  if (L3) {
   is_eq_self2 = ATTR(self,next);
   is_eq_y2 = PARSER_ident_tok;
   L9 = is_eq_self2.val1;
   L101_=(L9)==(is_eq_y2); 
   ret_val5 = L101_;
   if (ret_val5) {
    is_class_name_se = self;
    ident_self = self;
    ret_val6 = ATTR(ATTR(ident_self,scanner),lex_value);
    is_class_name_x = ret_val6;
    ret_val7 = SCANNE363215919(ATTR(is_class_name_se,scanner), is_class_name_x.str);
    L11 = ret_val7;
    L121_=!(L11); 
    L8 = L121_;
   } else {
    L8 = FALSE;
   }
   if (L8) {
    error_self = self;
    error_msg = ((STR) &classn393814273);
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self1 = error_self;
    ret_val8 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val8;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self = error_msg;
    plus_sarg = ((STR) &in1);
    ret_val9 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val9;
    plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val10 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val10;
    plus_arg = ')';
    str_self = plus_arg;
    create_self1 = ((STR) NULL);
    create_c = str_self;
    L14 = 1;
    L16=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L14)*sizeof(CHAR);
    L15=ZALLOC_LEAF_BIG(L16);
    if (L15==NULL) FATAL("Unable to allocate more memory");
    memset(L15,0,L16);
    ((OB)L15)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L15)->header.destroyed=0;
#endif

    L13 = ((STR) L15);
    L13->asize = L14;
    r = L13;
    SARR((STR)r,0,create_c); 
    ;
    ret_val13 = r;
    ret_val12 = ret_val13;
    s = ret_val12;
    ret_val11 = STR_ap1077157958(plus_self2, s, TRUE);
    err_s = ret_val11;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   SATTR(res,kind,AS_TYPE_SPEC_ord);
   ident_self1 = self;
   ret_val14 = ATTR(ATTR(ident_self1,scanner),lex_value);
   SATTR(res,name1,ret_val14);
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
   if (PARSER2128430730(self, PARSER2001381604)) {
    SATTR(res,params,PARSER532091533(self));
    match_self = self;
    match_t = PARSER1998725038;
    is_eq_self3 = ATTR(match_self,next);
    is_eq_y3 = match_t;
    L18 = is_eq_self3.val1;
    L191_=(L18)==(is_eq_y3); 
    ret_val15 = L191_;
    L20 = ret_val15;
    L211_=!(L20); 
    if (L211_) {
     exp_error_self = match_self;
     create_self2 = TOKEN_zero;
     create_val = match_t;
     t.val1 = create_val;
     ret_val16 = t;
     exp_error_msg = TOKEN_strrSTR(ret_val16);
     error_self1 = exp_error_self;
     plus_self3 = exp_error_msg;
     plus_sarg2 = ((STR) &expectedbutfound);
     ret_val17 = STR_ap2004550586(plus_self3, plus_sarg2);
     plus_self4 = ret_val17;
     plus_sarg3 = TOKEN_strrSTR(ATTR(exp_error_self,next));
     ret_val18 = STR_ap2004550586(plus_self4, plus_sarg3);
     error_msg1 = ret_val18;
     set_eloc_self1 = ATTR(error_self1,prog);
     source_loc_self2 = error_self1;
     ret_val19 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l1 = ret_val19;
     SATTR(set_eloc_self1,eloc,set_eloc_l1);
     err_self1 = error_self1;
     plus_self5 = error_msg1;
     plus_sarg4 = ((STR) &in1);
     ret_val20 = STR_ap2004550586(plus_self5, plus_sarg4);
     plus_self6 = ret_val20;
     plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
     ret_val21 = STR_ap2004550586(plus_self6, plus_sarg5);
     plus_self7 = ret_val21;
     plus_arg1 = ')';
     str_self1 = plus_arg1;
     create_self3 = ((STR) NULL);
     create_c1 = str_self1;
     L23 = 1;
     L25=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L23)*sizeof(CHAR);
     L24=ZALLOC_LEAF_BIG(L25);
     if (L24==NULL) FATAL("Unable to allocate more memory");
     memset(L24,0,L25);
     ((OB)L24)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L24)->header.destroyed=0;
#endif

     L22 = ((STR) L24);
     L22->asize = L23;
     r1 = L22;
     SARR((STR)r1,0,create_c1); 
     ;
     ret_val24 = r1;
     ret_val23 = ret_val24;
     s1 = ret_val23;
     ret_val22 = STR_ap1077157958(plus_self7, s1, TRUE);
     err_s1 = ret_val22;
     PROG_err_STR(ATTR(err_self1,prog), err_s1);
    }
    fetch_self1 = match_self;
    SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   }
  }
  else {
   if (PARSER2128430730(self, PARSER_ROUT_tok)) {
    SATTR(res,kind,AS_TYPE_SPEC_rt);
   }
   else {
    if (PARSER2128430730(self, PARSER_ITER_tok)) {
     SATTR(res,kind,AS_TYPE_SPEC_it);
    }
    else {
     if (PARSER2128430730(self, PARSER1337387114)) {
      SATTR(res,kind,AS_TYP1851973889);
     }
     else {
      exp_error_self1 = self;
      exp_error_msg1 = ((STR) &typespecifier);
      error_self2 = exp_error_self1;
      plus_self8 = exp_error_msg1;
      plus_sarg6 = ((STR) &expectedbutfound);
      ret_val25 = STR_ap2004550586(plus_self8, plus_sarg6);
      plus_self9 = ret_val25;
      plus_sarg7 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
      ret_val26 = STR_ap2004550586(plus_self9, plus_sarg7);
      error_msg2 = ret_val26;
      set_eloc_self2 = ATTR(error_self2,prog);
      source_loc_self3 = error_self2;
      ret_val27 = SFILE_1594766561(SFILE_ID_zero);
      set_eloc_l2 = ret_val27;
      SATTR(set_eloc_self2,eloc,set_eloc_l2);
      err_self2 = error_self2;
      plus_self10 = error_msg2;
      plus_sarg8 = ((STR) &in1);
      ret_val28 = STR_ap2004550586(plus_self10, plus_sarg8);
      plus_self11 = ret_val28;
      plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
      ret_val29 = STR_ap2004550586(plus_self11, plus_sarg9);
      plus_self12 = ret_val29;
      plus_arg2 = ')';
      str_self2 = plus_arg2;
      create_self4 = ((STR) NULL);
      create_c2 = str_self2;
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
      SARR((STR)r2,0,create_c2); 
      ;
      ret_val32 = r2;
      ret_val31 = ret_val32;
      s2 = ret_val31;
      ret_val30 = STR_ap1077157958(plus_self12, s2, TRUE);
      err_s2 = ret_val30;
      PROG_err_STR(ATTR(err_self2,prog), err_s2);
     }
    }
   }
   if (PARSER2128430730(self, PARSER2001381604)) {
    while (1) {
     L32 = ATTR(res,kind);
     L33 = AS_TYPE_SPEC_it;
     L341_=(L32)==(L33); 
     isiter = L341_;
     m = PARSER930791008(self, isiter);
     tp = PARSER2028498823(self);
     SATTR(tp,mode1,m);
     if (ATTR(self,version_1_0)) {
      if (PARSER2128430730(self, PARSER_bang_tok)) {
       L35 = TRUE;
      } else {
       L35 = PARSER2128430730(self, PARSER840587154);
      }
      if (L35) {
       L36 = ATTR(res,kind);
       L37 = AS_TYPE_SPEC_it;
       L381_=(L36)==(L37); 
       if (L381_) {
        SATTR(tp,is_hot,TRUE);
       }
       else {
        error_self3 = self;
        error_msg3 = ((STR) &nohota2092101541);
        set_eloc_self3 = ATTR(error_self3,prog);
        source_loc_self4 = error_self3;
        ret_val33 = SFILE_1594766561(SFILE_ID_zero);
        set_eloc_l3 = ret_val33;
        SATTR(set_eloc_self3,eloc,set_eloc_l3);
        err_self3 = error_self3;
        plus_self13 = error_msg3;
        plus_sarg10 = ((STR) &in1);
        ret_val34 = STR_ap2004550586(plus_self13, plus_sarg10);
        plus_self14 = ret_val34;
        plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
        ret_val35 = STR_ap2004550586(plus_self14, plus_sarg11);
        plus_self15 = ret_val35;
        plus_arg3 = ')';
        str_self3 = plus_arg3;
        create_self5 = ((STR) NULL);
        create_c3 = str_self3;
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
        r3 = L39;
        SARR((STR)r3,0,create_c3); 
        ;
        ret_val38 = r3;
        ret_val37 = ret_val38;
        s3 = ret_val37;
        ret_val36 = STR_ap1077157958(plus_self15, s3, TRUE);
        err_s3 = ret_val36;
        PROG_err_STR(ATTR(err_self3,prog), err_s3);
       }
      }
     }
     else {
      L44 = ATTR(ATTR(tp,mode1),mod);
      L45 = AS_ARG551300469;
      L461_=(L44)==(L45); 
      L47 = L461_;
      L481_=!(L47); 
      SATTR(tp,is_hot,L481_);
     }
     if ((ATTR(res,params)==((AS_TYPE_SPEC) NULL))) {
      SATTR(res,params,tp);
     }
     else {
      AS_TYP1027254244(ATTR(res,params), tp);
     }
     if (PARSER2128430730(self, PARSER_comma_tok)) {
     }
     else {
      goto after_loop;
     }
    }
    after_loop: ;
    match_self1 = self;
    match_t1 = PARSER1998725038;
    is_eq_self4 = ATTR(match_self1,next);
    is_eq_y4 = match_t1;
    L49 = is_eq_self4.val1;
    L501_=(L49)==(is_eq_y4); 
    ret_val39 = L501_;
    L52 = ret_val39;
    L531_=!(L52); 
    if (L531_) {
     exp_error_self2 = match_self1;
     create_self6 = TOKEN_zero;
     create_val1 = match_t1;
     t1.val1 = create_val1;
     ret_val40 = t1;
     exp_error_msg2 = TOKEN_strrSTR(ret_val40);
     error_self4 = exp_error_self2;
     plus_self16 = exp_error_msg2;
     plus_sarg12 = ((STR) &expectedbutfound);
     ret_val41 = STR_ap2004550586(plus_self16, plus_sarg12);
     plus_self17 = ret_val41;
     plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
     ret_val42 = STR_ap2004550586(plus_self17, plus_sarg13);
     error_msg4 = ret_val42;
     set_eloc_self4 = ATTR(error_self4,prog);
     source_loc_self5 = error_self4;
     ret_val43 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l4 = ret_val43;
     SATTR(set_eloc_self4,eloc,set_eloc_l4);
     err_self4 = error_self4;
     plus_self18 = error_msg4;
     plus_sarg14 = ((STR) &in1);
     ret_val44 = STR_ap2004550586(plus_self18, plus_sarg14);
     plus_self19 = ret_val44;
     plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
     ret_val45 = STR_ap2004550586(plus_self19, plus_sarg15);
     plus_self20 = ret_val45;
     plus_arg4 = ')';
     str_self4 = plus_arg4;
     create_self7 = ((STR) NULL);
     create_c4 = str_self4;
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
     SARR((STR)r4,0,create_c4); 
     ;
     ret_val48 = r4;
     ret_val47 = ret_val48;
     s4 = ret_val47;
     ret_val46 = STR_ap1077157958(plus_self20, s4, TRUE);
     err_s4 = ret_val46;
     PROG_err_STR(ATTR(err_self4,prog), err_s4);
    }
    fetch_self2 = match_self1;
    SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
   }
   if (PARSER2128430730(self, PARSER_colon_tok)) {
    SATTR(res,ret,PARSER2028498823(self));
   }
  }
 }
 exit_self = self;
 s5 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC PARSER532091533(PARSER self) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC res;
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER fetch_self;
 PARSER exit_self;
 STR s;
 extern STR listof387379961;
 INT L1;
 BOOL L21_;
 enter_self = self;
 enter_s = ((STR) &listof387379961);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER2028498823(self);
 while (1) {
  is_eq_self = ATTR(self,next);
  is_eq_y = PARSER_comma_tok;
  L1 = is_eq_self.val1;
  L21_=(L1)==(is_eq_y); 
  ret_val1 = L21_;
  if (ret_val1) {
  }
  else {
   goto after_loop;
  }
  fetch_self = self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  AS_TYP1027254244(res, PARSER2028498823(self));
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PARSER1234876012(PARSER self, TOKEN t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 L11 = t.val1;
 switch (L11) {
  case 27: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 31: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 10: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 38: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 48: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 6: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 18: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 2: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 75: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 76: 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL PARSER1883080161(PARSER self, TOKEN t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 L11 = t.val1;
 switch (L11) {
  case 44: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 41: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 107: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 45: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 46: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 47: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 15: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 9: 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL PARSER2128430730(PARSER self, INT t) {
 BOOL ret_val = BOOL_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER fetch_self;
 INT L1;
 BOOL L21_;
 is_eq_self = ATTR(self,next);
 is_eq_y = t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  fetch_self = self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  ret_val = TRUE;
  return ret_val;
 }
 else {
  ret_val = FALSE;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

IDENT PARSER814698330(PARSER self) {
 IDENT ret_val = IDENT_zero;
 IDENT res = IDENT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER ident_self;
 IDENT ret_val2 = IDENT_zero;
 PARSER fetch_self;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 PARSER fetch_self1;
 PARSER append_bang_self;
 IDENT append_bang_arg = IDENT_zero;
 IDENT ret_val4 = IDENT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val6 = BOOL_zero;
 PARSER fetch_self2;
 PARSER append_bang_self1;
 IDENT append_bang_arg1 = IDENT_zero;
 IDENT ret_val7 = IDENT_zero;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val8;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val9 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val10;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val11;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val12;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val13;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val14;
 STR r;
 PARSER err_self;
 STR err_s;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val16 = BOOL_zero;
 PARSER fetch_self3;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val20;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val21;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val22;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val23;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 INT L1;
 BOOL L21_;
 INT L3;
 BOOL L41_;
 extern STR b;
 INT L5;
 BOOL L61_;
 extern STR b;
 extern STR notaco462819084;
 extern STR in1;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 BOOL L12;
 INT L13;
 BOOL L141_;
 INT L15;
 BOOL L161_;
 extern STR routin837663754;
 extern STR expectedbutfound;
 extern STR in1;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
 extern STR a2;
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER_ident_tok;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  ident_self = self;
  ret_val2 = ATTR(ATTR(ident_self,scanner),lex_value);
  res = ret_val2;
  fetch_self = self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER840587154;
  L3 = is_eq_self1.val1;
  L41_=(L3)==(is_eq_y1); 
  ret_val3 = L41_;
  if (ret_val3) {
   fetch_self1 = self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   append_bang_self = self;
   append_bang_arg = res;
   plus_self = append_bang_arg.str;
   plus_sarg = ((STR) &b);
   ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val4 = IDENT_1150413730(IDENT_zero, ret_val5);
   res = ret_val4;
  }
  else {
   is_eq_self2 = ATTR(self,next);
   is_eq_y2 = PARSER_bang_tok;
   L5 = is_eq_self2.val1;
   L61_=(L5)==(is_eq_y2); 
   ret_val6 = L61_;
   if (ret_val6) {
    fetch_self2 = self;
    SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
    append_bang_self1 = self;
    append_bang_arg1 = res;
    plus_self1 = append_bang_arg1.str;
    plus_sarg1 = ((STR) &b);
    ret_val8 = STR_ap2004550586(plus_self1, plus_sarg1);
    ret_val7 = IDENT_1150413730(IDENT_zero, ret_val8);
    res = ret_val7;
    error_self = self;
    error_msg = ((STR) &notaco462819084);
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self = error_self;
    ret_val9 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val9;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self2 = error_msg;
    plus_sarg2 = ((STR) &in1);
    ret_val10 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val10;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val11 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val11;
    plus_arg = ')';
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
    ret_val14 = r;
    ret_val13 = ret_val14;
    s = ret_val13;
    ret_val12 = STR_ap1077157958(plus_self4, s, TRUE);
    err_s = ret_val12;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
  }
 }
 else {
  is_eq_self3 = ATTR(self,next);
  is_eq_y3 = PARSER_bang_tok;
  L13 = is_eq_self3.val1;
  L141_=(L13)==(is_eq_y3); 
  ret_val15 = L141_;
  if (ret_val15) {
   L12 = TRUE;
  } else {
   is_eq_self4 = ATTR(self,next);
   is_eq_y4 = PARSER840587154;
   L15 = is_eq_self4.val1;
   L161_=(L15)==(is_eq_y4); 
   ret_val16 = L161_;
   L12 = ret_val16;
  }
  if (L12) {
   fetch_self3 = self;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   res = IDENT_604723504;
  }
  else {
   exp_error_self = self;
   exp_error_msg = ((STR) &routin837663754);
   error_self1 = exp_error_self;
   plus_self5 = exp_error_msg;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self,next));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val18;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self1 = error_self1;
   ret_val19 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l1 = ret_val19;
   SATTR(set_eloc_self1,eloc,set_eloc_l1);
   err_self1 = error_self1;
   plus_self7 = error_msg1;
   plus_sarg6 = ((STR) &in1);
   ret_val20 = STR_ap2004550586(plus_self7, plus_sarg6);
   plus_self8 = ret_val20;
   plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val21 = STR_ap2004550586(plus_self8, plus_sarg7);
   plus_self9 = ret_val21;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L18 = 1;
   L20=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L18)*sizeof(CHAR);
   L19=ZALLOC_LEAF_BIG(L20);
   if (L19==NULL) FATAL("Unable to allocate more memory");
   memset(L19,0,L20);
   ((OB)L19)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L19)->header.destroyed=0;
#endif

   L17 = ((STR) L19);
   L17->asize = L18;
   r1 = L17;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
   res = IDENT_1150413730(IDENT_zero, ((STR) &a2));
  }
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

PARSER PARSER403165095(PARSER self, PROG p, STR file1, BOOL pSather, BOOL conv_to_1_1, BOOL version_1_0) {
 PARSER ret_val;
 PARSER res = ((PARSER) NULL);
 SCANNER s;
 BOOL L1;
 BOOL L21_;
 PARSER L3;
 OB L4;
 s = SCANNE1298383531(((SCANNER) NULL), p, file1, pSather);
 L1 = (s==((SCANNER) NULL));
 L21_=!(L1); 
 if (L21_) {
  L4=ZALLOC(sizeof(struct PARSER_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=PARSER_tag;
  L3 = ((PARSER) L4);
  res = L3;
  SATTR(res,prog,p);
  SATTR(res,scanner,s);
  SATTR(res,next,SCANNE1792387248(ATTR(res,scanner)));
  SATTR(res,entered,FLISTS1900495285(((FLISTSTR) NULL), 64));
  SATTR(res,version_1_0,version_1_0);
  SATTR(res,convert,CONVER262703763(((CONVERT) NULL), res, conv_to_1_1, file1));
  if ((ATTR(res,convert)==((CONVERT) NULL))) {
   res = ((PARSER) NULL);
  }
 }
 else {
  res = ((PARSER) NULL);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

TUPdAS268679779 PARSER851452257(PARSER self, BOOL underscore_args, BOOL is_iter) {
 TUPdAS268679779 ret_val = TUPdAS268679779_zero;
 dAS_EXPR exprs = ((dAS_EXPR) NULL);
 AS_ARG_MODE modes1 = ((AS_ARG_MODE) NULL);
 AS_ARG_MODE m = ((AS_ARG_MODE) NULL);
 dAS_EXPR x = ((dAS_EXPR) NULL);
 AS_UND152986614 u;
 TUPdAS268679779 t = TUPdAS268679779_zero;
 PARSER enter_self;
 STR enter_s;
 PARSER enter_self1;
 STR enter_s1;
 AS_UND152986614 create_self;
 AS_UND152986614 ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER source_loc_self1;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 PARSER warning_self;
 STR warning_msg;
 PARSER source_loc_self2;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self3;
 SFILE_ID ret_val10 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val11;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val12;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val13;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val14;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val15;
 STR r1;
 PARSER err_self;
 STR err_s;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self4;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val18;
 STR plus_self8;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val19;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val20;
 STR create_self3;
 CHAR create_c2 = CHAR_zero;
 STR ret_val21;
 STR r2;
 PARSER err_self1;
 STR err_s1;
 PARSER exit_self;
 STR s3;
 TUPdAS268679779 create_self4 = TUPdAS268679779_zero;
 dAS_EXPR create_at1;
 AS_ARG_MODE create_at2;
 TUPdAS268679779 ret_val22 = TUPdAS268679779_zero;
 extern STR listof717650279;
 extern STR listof432662797;
 AS_UND152986614 L1;
 OB L2;
 extern STR Sather1601374408;
 extern STR in1;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L81_;
 extern STR nounde75550121;
 extern STR in1;
 STR L9;
 INT L10;
 OB L11;
 INT L12;
 BOOL L14;
 extern STR outino107624499;
 extern STR in1;
 STR L15;
 INT L16;
 OB L17;
 INT L18;
 dAS_EXPR L20;
 TUPdAS268679779 L21;
 TUPdAS268679779 L22;
 if (underscore_args) {
  enter_self = self;
  enter_s = ((STR) &listof717650279);
  SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 }
 else {
  enter_self1 = self;
  enter_s1 = ((STR) &listof432662797);
  SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
 }
 while (1) {
  m = PARSER930791008(self, is_iter);
  if (PARSER2128430730(self, PARSER_under_tok)) {
   create_self = ((AS_UND152986614) NULL);
   L2=ZALLOC(sizeof(struct AS_UND152986614_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=AS_UND152986614_tag;
   L1 = ((AS_UND152986614) L2);
   ret_val1 = L1;
   u = ret_val1;
   source_loc_self = self;
   ret_val2 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(u,source1,ret_val2);
   source_loc_self1 = self;
   ret_val3 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(u,source1,ret_val3);
   x = ((dAS_EXPR) u);
   if (PARSER2128430730(self, PARSER_colon_tok)) {
    SATTR(u,tp,PARSER2028498823(self));
    SATTR(ATTR(u,tp),mode1,m);
    warning_self = self;
    warning_msg = ((STR) &Sather1601374408);
    set_eloc_self = ATTR(warning_self,prog);
    source_loc_self2 = warning_self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val4;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    plus_self = warning_msg;
    plus_sarg = ((STR) &in1);
    ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val5;
    plus_sarg1 = FLISTSTR_toprSTR(ATTR(warning_self,entered));
    ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val6;
    plus_arg = ')';
    str_self = plus_arg;
    create_self1 = ((STR) NULL);
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
    ret_val9 = r;
    ret_val8 = ret_val9;
    s = ret_val8;
    ret_val7 = STR_ap1077157958(plus_self2, s, TRUE);
    PROG_warning_STR(ATTR(warning_self,prog), ret_val7);
   }
   L81_=!(underscore_args); 
   if (L81_) {
    error_self = self;
    error_msg = ((STR) &nounde75550121);
    set_eloc_self1 = ATTR(error_self,prog);
    source_loc_self3 = error_self;
    ret_val10 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l1 = ret_val10;
    SATTR(set_eloc_self1,eloc,set_eloc_l1);
    err_self = error_self;
    plus_self3 = error_msg;
    plus_sarg2 = ((STR) &in1);
    ret_val11 = STR_ap2004550586(plus_self3, plus_sarg2);
    plus_self4 = ret_val11;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val12 = STR_ap2004550586(plus_self4, plus_sarg3);
    plus_self5 = ret_val12;
    plus_arg1 = ')';
    str_self1 = plus_arg1;
    create_self2 = ((STR) NULL);
    create_c1 = str_self1;
    L10 = 1;
    L12=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L10)*sizeof(CHAR);
    L11=ZALLOC_LEAF_BIG(L12);
    if (L11==NULL) FATAL("Unable to allocate more memory");
    memset(L11,0,L12);
    ((OB)L11)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L11)->header.destroyed=0;
#endif

    L9 = ((STR) L11);
    L9->asize = L10;
    r1 = L9;
    SARR((STR)r1,0,create_c1); 
    ;
    ret_val15 = r1;
    ret_val14 = ret_val15;
    s1 = ret_val14;
    ret_val13 = STR_ap1077157958(plus_self5, s1, TRUE);
    err_s = ret_val13;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
  }
  else {
   x = PARSER403674127(self);
   if (underscore_args) {
    L14 = AS_ARG1391068469(m);
   } else {
    L14 = FALSE;
   }
   if (L14) {
    error_self1 = self;
    error_msg1 = ((STR) &outino107624499);
    set_eloc_self2 = ATTR(error_self1,prog);
    source_loc_self4 = error_self1;
    ret_val16 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l2 = ret_val16;
    SATTR(set_eloc_self2,eloc,set_eloc_l2);
    err_self1 = error_self1;
    plus_self6 = error_msg1;
    plus_sarg4 = ((STR) &in1);
    ret_val17 = STR_ap2004550586(plus_self6, plus_sarg4);
    plus_self7 = ret_val17;
    plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
    ret_val18 = STR_ap2004550586(plus_self7, plus_sarg5);
    plus_self8 = ret_val18;
    plus_arg2 = ')';
    str_self2 = plus_arg2;
    create_self3 = ((STR) NULL);
    create_c2 = str_self2;
    L16 = 1;
    L18=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L16)*sizeof(CHAR);
    L17=ZALLOC_LEAF_BIG(L18);
    if (L17==NULL) FATAL("Unable to allocate more memory");
    memset(L17,0,L18);
    ((OB)L17)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L17)->header.destroyed=0;
#endif

    L15 = ((STR) L17);
    L15->asize = L16;
    r2 = L15;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val21 = r2;
    ret_val20 = ret_val21;
    s2 = ret_val20;
    ret_val19 = STR_ap1077157958(plus_self8, s2, TRUE);
    err_s1 = ret_val19;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  if ((exprs==((dAS_EXPR) NULL))) {
   exprs = x;
   modes1 = m;
  }
  else {
   L20 = exprs;
   (*dAS_EX1889860882[TAG(L20)])(L20, x);
   AS_ARG1328804463(modes1, m);
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 create_self4 = TUPdAS268679779_zero;
 create_at1 = exprs;
 create_at2 = modes1;
 L21 = create_self4;
 L21.t1 = create_at1;
 L22 = L21;
 L22.t2 = create_at2;
 ret_val22 = L22;
 t = ret_val22;
 ret_val = t;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT PARSER1637480411(PARSER self, TOKEN mode1) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 AS_INC1416917001 incl;
 AS_FEAT_MOD newm;
 INT L11 = INT_zero;
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val5;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val6;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val7;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val8 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val9 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val10;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val11;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val13;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val14;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val15;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val16;
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val17;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self;
 AS_INC1416917001 create_self3;
 AS_INC1416917001 ret_val18;
 PARSER source_loc_self2;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val20 = BOOL_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val21 = BOOL_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val22 = BOOL_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val23 = BOOL_zero;
 AS_FEAT_MOD create_self4;
 AS_FEAT_MOD ret_val24;
 PARSER source_loc_self3;
 SFILE_ID ret_val25 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val28;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val29;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self4;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val31;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val32;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val33;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val34;
 STR create_self6;
 CHAR create_c2 = CHAR_zero;
 STR ret_val35;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self1;
 PARSER fetch_self2;
 PARSER fetch_self3;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val36 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val37 = BOOL_zero;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self5;
 SFILE_ID ret_val38 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val39;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val40;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val41;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val42;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val43;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER exit_self;
 STR s4;
 extern STR includeclause;
 INT L2;
 BOOL L31_;
 extern STR readon2082396409;
 extern STR in1;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 INT L9;
 BOOL L101_;
 BOOL L12;
 BOOL L131_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 AS_INC1416917001 L19;
 OB L20;
 INT L21;
 BOOL L221_;
 BOOL L23;
 BOOL L24;
 INT L25;
 BOOL L261_;
 INT L27;
 BOOL L281_;
 INT L29;
 BOOL L301_;
 AS_FEAT_MOD L32;
 OB L33;
 INT L34;
 BOOL L351_;
 BOOL L36;
 BOOL L371_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L38;
 INT L39;
 OB L40;
 INT L41;
 BOOL L43;
 BOOL L441_;
 BOOL L45;
 BOOL L46;
 BOOL L471_;
 STR L48;
 INT L49;
 INT L501_;
 INT L51;
 CHAR L521_;
 CHAR L53;
 BOOL L541_;
 BOOL L55;
 BOOL L56;
 BOOL L57;
 BOOL L581_;
 STR L59;
 INT L60;
 INT L611_;
 INT L62;
 CHAR L631_;
 CHAR L64;
 BOOL L651_;
 BOOL L66;
 BOOL L671_;
 BOOL L68;
 BOOL L691_;
 extern STR routin1880910518;
 extern STR in1;
 STR L70;
 INT L71;
 OB L72;
 INT L73;
 enter_self = self;
 enter_s = ((STR) &includeclause);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = mode1;
 is_eq_y = PARSER550831344;
 L2 = is_eq_self.val1;
 L31_=(L2)==(is_eq_y); 
 ret_val1 = L31_;
 if (ret_val1) {
  error_self = self;
  error_msg = ((STR) &readon2082396409);
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val2;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self = error_msg;
  plus_sarg = ((STR) &in1);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  plus_self2 = ret_val4;
  plus_arg = ')';
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
  ret_val7 = r;
  ret_val6 = ret_val7;
  s = ret_val6;
  ret_val5 = STR_ap1077157958(plus_self2, s, TRUE);
  err_s = ret_val5;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 match_self = self;
 match_t = PARSER415742890;
 is_eq_self1 = ATTR(match_self,next);
 is_eq_y1 = match_t;
 L9 = is_eq_self1.val1;
 L101_=(L9)==(is_eq_y1); 
 ret_val8 = L101_;
 L12 = ret_val8;
 L131_=!(L12); 
 if (L131_) {
  exp_error_self = match_self;
  create_self1 = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val9 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val9);
  error_self1 = exp_error_self;
  plus_self3 = exp_error_msg;
  plus_sarg2 = ((STR) &expectedbutfound);
  ret_val10 = STR_ap2004550586(plus_self3, plus_sarg2);
  plus_self4 = ret_val10;
  plus_sarg3 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val11 = STR_ap2004550586(plus_self4, plus_sarg3);
  error_msg1 = ret_val11;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self1 = error_self1;
  ret_val12 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val12;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self5 = error_msg1;
  plus_sarg4 = ((STR) &in1);
  ret_val13 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val13;
  plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val14 = STR_ap2004550586(plus_self6, plus_sarg5);
  plus_self7 = ret_val14;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self2 = ((STR) NULL);
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
  ret_val17 = r1;
  ret_val16 = ret_val17;
  s1 = ret_val16;
  ret_val15 = STR_ap1077157958(plus_self7, s1, TRUE);
  err_s1 = ret_val15;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self3 = ((AS_INC1416917001) NULL);
 L20=ZALLOC(sizeof(struct AS_INC1416917001_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=AS_INC1416917001_tag;
 L19 = ((AS_INC1416917001) L20);
 ret_val18 = L19;
 incl = ret_val18;
 source_loc_self2 = self;
 ret_val19 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(incl,source1,ret_val19);
 is_eq_self2 = mode1;
 is_eq_y2 = PARSER1019557737;
 L21 = is_eq_self2.val1;
 L221_=(L21)==(is_eq_y2); 
 ret_val20 = L221_;
 SATTR(incl,is_private,ret_val20);
 SATTR(incl,tp,PARSER2028498823(self));
 res = ((dAS_CLASS_ELT) incl);
 is_eq_self3 = ATTR(self,next);
 is_eq_y3 = PARSER_ident_tok;
 L25 = is_eq_self3.val1;
 L261_=(L25)==(is_eq_y3); 
 ret_val21 = L261_;
 if (ret_val21) {
  L24 = TRUE;
 } else {
  is_eq_self4 = ATTR(self,next);
  is_eq_y4 = PARSER_bang_tok;
  L27 = is_eq_self4.val1;
  L281_=(L27)==(is_eq_y4); 
  ret_val22 = L281_;
  L24 = ret_val22;
 }
 if (L24) {
  L23 = TRUE;
 } else {
  is_eq_self5 = ATTR(self,next);
  is_eq_y5 = PARSER840587154;
  L29 = is_eq_self5.val1;
  L301_=(L29)==(is_eq_y5); 
  ret_val23 = L301_;
  L23 = ret_val23;
 }
 if (L23) {
  while (1) {
   create_self4 = ((AS_FEAT_MOD) NULL);
   L33=ZALLOC(sizeof(struct AS_FEAT_MOD_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   ((OB)L33)->header.tag=AS_FEAT_MOD_tag;
   L32 = ((AS_FEAT_MOD) L33);
   ret_val24 = L32;
   newm = ret_val24;
   source_loc_self3 = self;
   ret_val25 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(newm,source1,ret_val25);
   SATTR(newm,name1,PARSER814698330(self));
   match_self1 = self;
   match_t1 = PARSER1758133485;
   is_eq_self6 = ATTR(match_self1,next);
   is_eq_y6 = match_t1;
   L34 = is_eq_self6.val1;
   L351_=(L34)==(is_eq_y6); 
   ret_val26 = L351_;
   L36 = ret_val26;
   L371_=!(L36); 
   if (L371_) {
    exp_error_self1 = match_self1;
    create_self5 = TOKEN_zero;
    create_val1 = match_t1;
    t1.val1 = create_val1;
    ret_val27 = t1;
    exp_error_msg1 = TOKEN_strrSTR(ret_val27);
    error_self2 = exp_error_self1;
    plus_self8 = exp_error_msg1;
    plus_sarg6 = ((STR) &expectedbutfound);
    ret_val28 = STR_ap2004550586(plus_self8, plus_sarg6);
    plus_self9 = ret_val28;
    plus_sarg7 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
    ret_val29 = STR_ap2004550586(plus_self9, plus_sarg7);
    error_msg2 = ret_val29;
    set_eloc_self2 = ATTR(error_self2,prog);
    source_loc_self4 = error_self2;
    ret_val30 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l2 = ret_val30;
    SATTR(set_eloc_self2,eloc,set_eloc_l2);
    err_self2 = error_self2;
    plus_self10 = error_msg2;
    plus_sarg8 = ((STR) &in1);
    ret_val31 = STR_ap2004550586(plus_self10, plus_sarg8);
    plus_self11 = ret_val31;
    plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
    ret_val32 = STR_ap2004550586(plus_self11, plus_sarg9);
    plus_self12 = ret_val32;
    plus_arg2 = ')';
    str_self2 = plus_arg2;
    create_self6 = ((STR) NULL);
    create_c2 = str_self2;
    L39 = 1;
    L41=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L39)*sizeof(CHAR);
    L40=ZALLOC_LEAF_BIG(L41);
    if (L40==NULL) FATAL("Unable to allocate more memory");
    memset(L40,0,L41);
    ((OB)L40)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L40)->header.destroyed=0;
#endif

    L38 = ((STR) L40);
    L38->asize = L39;
    r2 = L38;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val35 = r2;
    ret_val34 = ret_val35;
    s2 = ret_val34;
    ret_val33 = STR_ap1077157958(plus_self12, s2, TRUE);
    err_s2 = ret_val33;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   fetch_self1 = match_self1;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   L11 = ATTR(self,next).val1;
   switch (L11) {
    case 27: 
     fetch_self2 = self;
     SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
     SATTR(newm,is_private,TRUE);
     SATTR(newm,new_name,PARSER814698330(self));
     break;
    case 31: 
     fetch_self3 = self;
     SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
     SATTR(newm,is_readonly,TRUE);
     SATTR(newm,new_name,PARSER814698330(self));
     break;
    case 2: 
     SATTR(newm,new_name,PARSER814698330(self));
     break;
    case 75: 
     SATTR(newm,new_name,PARSER814698330(self));
     break;
    case 76: 
     SATTR(newm,new_name,PARSER814698330(self));
     break;
    default: ;
   }
   L43 = (ATTR(newm,new_name).str==(STR)0);
   L441_=!(L43); 
   if (L441_) {
    is_iter_self = ATTR(newm,name1);
    L46 = (is_iter_self.str==((STR) NULL));
    L471_=!(L46); 
    if (L471_) {
     L48 = is_iter_self.str;
     L49 = STR_sizerINT(is_iter_self.str);
     L501_=INTMINUS(L49,1); 
     L51 = L501_;
     L521_=ARR((STR)L48,L51); 
     L53 = L521_;
     L541_=L53=='!'; 
     L45 = L541_;
    } else {
     L45 = FALSE;
    }
    ret_val36 = L45;
    L55 = ret_val36;
    is_iter_self1 = ATTR(newm,new_name);
    L57 = (is_iter_self1.str==((STR) NULL));
    L581_=!(L57); 
    if (L581_) {
     L59 = is_iter_self1.str;
     L60 = STR_sizerINT(is_iter_self1.str);
     L611_=INTMINUS(L60,1); 
     L62 = L611_;
     L631_=ARR((STR)L59,L62); 
     L64 = L631_;
     L651_=L64=='!'; 
     L56 = L651_;
    } else {
     L56 = FALSE;
    }
    ret_val37 = L56;
    L66 = ret_val37;
    L671_=L55==L66; 
    L68 = L671_;
    L691_=!(L68); 
    if (L691_) {
     error_self3 = self;
     error_msg3 = ((STR) &routin1880910518);
     set_eloc_self3 = ATTR(error_self3,prog);
     source_loc_self5 = error_self3;
     ret_val38 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l3 = ret_val38;
     SATTR(set_eloc_self3,eloc,set_eloc_l3);
     err_self3 = error_self3;
     plus_self13 = error_msg3;
     plus_sarg10 = ((STR) &in1);
     ret_val39 = STR_ap2004550586(plus_self13, plus_sarg10);
     plus_self14 = ret_val39;
     plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
     ret_val40 = STR_ap2004550586(plus_self14, plus_sarg11);
     plus_self15 = ret_val40;
     plus_arg3 = ')';
     str_self3 = plus_arg3;
     create_self7 = ((STR) NULL);
     create_c3 = str_self3;
     L71 = 1;
     L73=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L71)*sizeof(CHAR);
     L72=ZALLOC_LEAF_BIG(L73);
     if (L72==NULL) FATAL("Unable to allocate more memory");
     memset(L72,0,L73);
     ((OB)L72)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L72)->header.destroyed=0;
#endif

     L70 = ((STR) L72);
     L70->asize = L71;
     r3 = L70;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val43 = r3;
     ret_val42 = ret_val43;
     s3 = ret_val42;
     ret_val41 = STR_ap1077157958(plus_self15, s3, TRUE);
     err_s3 = ret_val41;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
   }
   if ((ATTR(incl,mods)==((AS_FEAT_MOD) NULL))) {
    SATTR(incl,mods,newm);
   }
   else {
    AS_FEA814213709(ATTR(incl,mods), newm);
   }
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop;
   }
  }
  after_loop: ;
 }
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT PARSER800575691(PARSER self) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val5;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val6;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val10;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val11;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val12;
 STR r;
 PARSER err_self;
 STR err_s;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val13 = BOOL_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val14;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val15;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val17;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val18;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val19;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val20;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val21;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val22 = BOOL_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val23 = BOOL_zero;
 PARSER fetch_self;
 PARSER exit_self;
 STR s2;
 extern STR listof1081324314;
 BOOL L1;
 BOOL L2;
 INT L3;
 BOOL L41_;
 INT L5;
 BOOL L61_;
 INT L7;
 BOOL L81_;
 dAS_CLASS_ELT L9;
 INT L10;
 BOOL L111_;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L21;
 INT L22;
 OB L23;
 INT L24;
 BOOL L26;
 INT L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 INT L31;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 enter_self = self;
 enter_s = ((STR) &listof1081324314);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
  is_eq_self = ATTR(self,next);
  is_eq_y = PARSER_ident_tok;
  L3 = is_eq_self.val1;
  L41_=(L3)==(is_eq_y); 
  ret_val1 = L41_;
  if (ret_val1) {
   L2 = TRUE;
  } else {
   is_eq_self1 = ATTR(self,next);
   is_eq_y1 = PARSER_bang_tok;
   L5 = is_eq_self1.val1;
   L61_=(L5)==(is_eq_y1); 
   ret_val2 = L61_;
   L2 = ret_val2;
  }
  if (L2) {
   L1 = TRUE;
  } else {
   is_eq_self2 = ATTR(self,next);
   is_eq_y2 = PARSER840587154;
   L7 = is_eq_self2.val1;
   L81_=(L7)==(is_eq_y2); 
   ret_val3 = L81_;
   L1 = ret_val3;
  }
  if (L1) {
   if ((res==((dAS_CLASS_ELT) NULL))) {
    res = ((dAS_CLASS_ELT) PARSER2039100746(self));
   }
   else {
    L9 = res;
    (*dAS_CL1056554733[TAG(L9)])(L9, ((dAS_CLASS_ELT) PARSER2039100746(self)));
   }
  }
  if (PARSER2128430730(self, PARSER_semi_tok)) {
  }
  else {
   is_eq_self3 = ATTR(self,next);
   is_eq_y3 = PARSER_ident_tok;
   L10 = is_eq_self3.val1;
   L111_=(L10)==(is_eq_y3); 
   ret_val4 = L111_;
   if (ret_val4) {
    exp_error_self = self;
    exp_error_msg = ((STR) &semicolon);
    error_self = exp_error_self;
    plus_self = exp_error_msg;
    plus_sarg = ((STR) &expectedbutfound);
    ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val5;
    plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
    ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
    error_msg = ret_val6;
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self = error_self;
    ret_val7 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val7;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self2 = error_msg;
    plus_sarg2 = ((STR) &in1);
    ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val8;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val9 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val9;
    plus_arg = ')';
    str_self = plus_arg;
    create_self = ((STR) NULL);
    create_c = str_self;
    L13 = 1;
    L15=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L13)*sizeof(CHAR);
    L14=ZALLOC_LEAF_BIG(L15);
    if (L14==NULL) FATAL("Unable to allocate more memory");
    memset(L14,0,L15);
    ((OB)L14)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L14)->header.destroyed=0;
#endif

    L12 = ((STR) L14);
    L12->asize = L13;
    r = L12;
    SARR((STR)r,0,create_c); 
    ;
    ret_val12 = r;
    ret_val11 = ret_val12;
    s = ret_val11;
    ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
    err_s = ret_val10;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 is_eq_self4 = ATTR(self,next);
 is_eq_y4 = PARSER_end_tok;
 L17 = is_eq_self4.val1;
 L181_=(L17)==(is_eq_y4); 
 ret_val13 = L181_;
 L19 = ret_val13;
 L201_=!(L19); 
 if (L201_) {
  exp_error_self1 = self;
  exp_error_msg1 = ((STR) &semicolon);
  error_self1 = exp_error_self1;
  plus_self5 = exp_error_msg1;
  plus_sarg4 = ((STR) &expectedbutfound);
  ret_val14 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val14;
  plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val15 = STR_ap2004550586(plus_self6, plus_sarg5);
  error_msg1 = ret_val15;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self1 = error_self1;
  ret_val16 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val16;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self7 = error_msg1;
  plus_sarg6 = ((STR) &in1);
  ret_val17 = STR_ap2004550586(plus_self7, plus_sarg6);
  plus_self8 = ret_val17;
  plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val18 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val18;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self1 = ((STR) NULL);
  create_c1 = str_self1;
  L22 = 1;
  L24=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L22)*sizeof(CHAR);
  L23=ZALLOC_LEAF_BIG(L24);
  if (L23==NULL) FATAL("Unable to allocate more memory");
  memset(L23,0,L24);
  ((OB)L23)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L23)->header.destroyed=0;
#endif

  L21 = ((STR) L23);
  L21->asize = L22;
  r1 = L21;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val21 = r1;
  ret_val20 = ret_val21;
  s1 = ret_val20;
  ret_val19 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val19;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
  while (1) {
   is_eq_self5 = ATTR(self,next);
   is_eq_y5 = PARSER_end_tok;
   L27 = is_eq_self5.val1;
   L281_=(L27)==(is_eq_y5); 
   ret_val22 = L281_;
   L29 = ret_val22;
   L301_=!(L29); 
   if (L301_) {
    is_eq_self6 = ATTR(self,next);
    is_eq_y6 = PARSER_eof_tok;
    L31 = is_eq_self6.val1;
    L321_=(L31)==(is_eq_y6); 
    ret_val23 = L321_;
    L33 = ret_val23;
    L341_=!(L33); 
    L26 = L341_;
   } else {
    L26 = FALSE;
   }
   if (L26) {
   }
   else {
    goto after_loop1;
   }
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  }
  after_loop1: ;
 }
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void PARSER164019780(PARSER self, AS_CALL_EXPR call11, BOOL is_iter) {
 dAS_EXPR ob;
 dAS_EXPR this1;
 dAS_EXPR arg;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val4;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val5;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val6;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val8;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val9;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val10;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val11;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val12;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self2;
 SFILE_ID ret_val13 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val14;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val15;
 STR plus_self8;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val16;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val17;
 STR create_self2;
 CHAR create_c2 = CHAR_zero;
 STR ret_val18;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self3;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self9;
 STR plus_sarg6;
 STR ret_val20;
 STR plus_self10;
 STR plus_sarg7;
 STR ret_val21;
 STR plus_self11;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val22;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val23;
 STR create_self3;
 CHAR create_c3 = CHAR_zero;
 STR ret_val24;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self4;
 SFILE_ID ret_val25 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg8;
 STR ret_val26;
 STR plus_self13;
 STR plus_sarg9;
 STR ret_val27;
 STR plus_self14;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val28;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val29;
 STR create_self4;
 CHAR create_c4 = CHAR_zero;
 STR ret_val30;
 STR r4;
 PARSER err_self4;
 STR err_s4;
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
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR boundr2019491019;
 extern STR in1;
 STR L15;
 INT L16;
 OB L17;
 INT L18;
 extern STR boundr257630765;
 extern STR in1;
 STR L20;
 INT L21;
 OB L22;
 INT L23;
 extern STR onlyca1619932192;
 extern STR in1;
 STR L25;
 INT L26;
 OB L27;
 INT L28;
 extern STR illega1189754735;
 extern STR in1;
 STR L30;
 INT L32;
 OB L33;
 INT L34;
 dAS_EXPR L36;
 BOOL L37;
 BOOL L381_;
 extern STR illega1189754735;
 extern STR in1;
 STR L39;
 INT L40;
 OB L41;
 INT L42;
 if ((call11==((AS_CALL_EXPR) NULL))) {
  return;
 }
 is_iter_self = ATTR(call11,name1);
 L2 = (is_iter_self.str==((STR) NULL));
 L31_=!(L2); 
 if (L31_) {
  L4 = is_iter_self.str;
  L5 = STR_sizerINT(is_iter_self.str);
  L61_=INTMINUS(L5,1); 
  L7 = L61_;
  L81_=ARR((STR)L4,L7); 
  L9 = L81_;
  L101_=L9=='!'; 
  L1 = L101_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 L11 = ret_val;
 L121_=L11==is_iter; 
 L13 = L121_;
 L141_=!(L13); 
 if (L141_) {
  if (is_iter) {
   error_self = self;
   error_msg = ((STR) &boundr2019491019);
   set_eloc_self = ATTR(error_self,prog);
   source_loc_self = error_self;
   ret_val1 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l = ret_val1;
   SATTR(set_eloc_self,eloc,set_eloc_l);
   err_self = error_self;
   plus_self = error_msg;
   plus_sarg = ((STR) &in1);
   ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
   plus_self1 = ret_val2;
   plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
   ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
   plus_self2 = ret_val3;
   plus_arg = ')';
   str_self = plus_arg;
   create_self = ((STR) NULL);
   create_c = str_self;
   L16 = 1;
   L18=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L16)*sizeof(CHAR);
   L17=ZALLOC_LEAF_BIG(L18);
   if (L17==NULL) FATAL("Unable to allocate more memory");
   memset(L17,0,L18);
   ((OB)L17)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L17)->header.destroyed=0;
#endif

   L15 = ((STR) L17);
   L15->asize = L16;
   r = L15;
   SARR((STR)r,0,create_c); 
   ;
   ret_val6 = r;
   ret_val5 = ret_val6;
   s = ret_val5;
   ret_val4 = STR_ap1077157958(plus_self2, s, TRUE);
   err_s = ret_val4;
   PROG_err_STR(ATTR(err_self,prog), err_s);
  }
  else {
   error_self1 = self;
   error_msg1 = ((STR) &boundr257630765);
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self1 = error_self1;
   ret_val7 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l1 = ret_val7;
   SATTR(set_eloc_self1,eloc,set_eloc_l1);
   err_self1 = error_self1;
   plus_self3 = error_msg1;
   plus_sarg2 = ((STR) &in1);
   ret_val8 = STR_ap2004550586(plus_self3, plus_sarg2);
   plus_self4 = ret_val8;
   plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val9 = STR_ap2004550586(plus_self4, plus_sarg3);
   plus_self5 = ret_val9;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self1 = ((STR) NULL);
   create_c1 = str_self1;
   L21 = 1;
   L23=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L21)*sizeof(CHAR);
   L22=ZALLOC_LEAF_BIG(L23);
   if (L22==NULL) FATAL("Unable to allocate more memory");
   memset(L22,0,L23);
   ((OB)L22)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L22)->header.destroyed=0;
#endif

   L20 = ((STR) L22);
   L20->asize = L21;
   r1 = L20;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val12 = r1;
   ret_val11 = ret_val12;
   s1 = ret_val11;
   ret_val10 = STR_ap1077157958(plus_self5, s1, TRUE);
   err_s1 = ret_val10;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
 }
 if (ATTR(call11,is_array)) {
  error_self2 = self;
  error_msg2 = ((STR) &onlyca1619932192);
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self2 = error_self2;
  ret_val13 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val13;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self6 = error_msg2;
  plus_sarg4 = ((STR) &in1);
  ret_val14 = STR_ap2004550586(plus_self6, plus_sarg4);
  plus_self7 = ret_val14;
  plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val15 = STR_ap2004550586(plus_self7, plus_sarg5);
  plus_self8 = ret_val15;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self2 = ((STR) NULL);
  create_c2 = str_self2;
  L26 = 1;
  L28=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L26)*sizeof(CHAR);
  L27=ZALLOC_LEAF_BIG(L28);
  if (L27==NULL) FATAL("Unable to allocate more memory");
  memset(L27,0,L28);
  ((OB)L27)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L27)->header.destroyed=0;
#endif

  L25 = ((STR) L27);
  L25->asize = L26;
  r2 = L25;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val18 = r2;
  ret_val17 = ret_val18;
  s2 = ret_val17;
  ret_val16 = STR_ap1077157958(plus_self8, s2, TRUE);
  err_s2 = ret_val16;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 ob = ATTR(call11,ob);
 while (1) {
  if ((ob==((dAS_EXPR) NULL))) {
   goto after_loop;
  }
  this1 = ob;
  if (this1==NULL) {
   goto other100;
  } else
  switch (TAG(this1)) {
   case AS_CALL_EXPR_tag:
    ob = ATTR(((AS_CALL_EXPR) this1),ob);
    arg = ATTR(((AS_CALL_EXPR) this1),args);
    while (1) {
     if ((arg==((dAS_EXPR) NULL))) {
      goto after_loop1;
     }
     if (arg==NULL) {
      goto other101;
     } else
     switch (TAG(arg)) {
      case AS_UND152986614_tag:
       error_self3 = self;
       error_msg3 = ((STR) &illega1189754735);
       set_eloc_self3 = ATTR(error_self3,prog);
       source_loc_self3 = error_self3;
       ret_val19 = SFILE_1594766561(SFILE_ID_zero);
       set_eloc_l3 = ret_val19;
       SATTR(set_eloc_self3,eloc,set_eloc_l3);
       err_self3 = error_self3;
       plus_self9 = error_msg3;
       plus_sarg6 = ((STR) &in1);
       ret_val20 = STR_ap2004550586(plus_self9, plus_sarg6);
       plus_self10 = ret_val20;
       plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
       ret_val21 = STR_ap2004550586(plus_self10, plus_sarg7);
       plus_self11 = ret_val21;
       plus_arg3 = ')';
       str_self3 = plus_arg3;
       create_self3 = ((STR) NULL);
       create_c3 = str_self3;
       L32 = 1;
       L34=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L32)*sizeof(CHAR);
       L33=ZALLOC_LEAF_BIG(L34);
       if (L33==NULL) FATAL("Unable to allocate more memory");
       memset(L33,0,L34);
       ((OB)L33)->header.tag=STR_tag;
#ifdef DESTROY_CHK

         ((OB)L33)->header.destroyed=0;
#endif

       L30 = ((STR) L33);
       L30->asize = L32;
       r3 = L30;
       SARR((STR)r3,0,create_c3); 
       ;
       ret_val24 = r3;
       ret_val23 = ret_val24;
       s3 = ret_val23;
       ret_val22 = STR_ap1077157958(plus_self11, s3, TRUE);
       err_s3 = ret_val22;
       PROG_err_STR(ATTR(err_self3,prog), err_s3);
       return; break;
      default: ;
      other101: ;
     }
     L36 = arg;
     arg = (*dAS_EX337431651[TAG(L36)])(L36);
    }
    after_loop1: ; break;
   default: ;
   other100: ;
    if (ob==NULL) {
     goto other102;
    } else
    switch (TAG(ob)) {
     case AS_UND152986614_tag:
      L37 = SYSOBEQ(((OB) ob),((OB) ATTR(call11,ob)));
      L381_=!(L37); 
      if (L381_) {
       error_self4 = self;
       error_msg4 = ((STR) &illega1189754735);
       set_eloc_self4 = ATTR(error_self4,prog);
       source_loc_self4 = error_self4;
       ret_val25 = SFILE_1594766561(SFILE_ID_zero);
       set_eloc_l4 = ret_val25;
       SATTR(set_eloc_self4,eloc,set_eloc_l4);
       err_self4 = error_self4;
       plus_self12 = error_msg4;
       plus_sarg8 = ((STR) &in1);
       ret_val26 = STR_ap2004550586(plus_self12, plus_sarg8);
       plus_self13 = ret_val26;
       plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
       ret_val27 = STR_ap2004550586(plus_self13, plus_sarg9);
       plus_self14 = ret_val27;
       plus_arg4 = ')';
       str_self4 = plus_arg4;
       create_self4 = ((STR) NULL);
       create_c4 = str_self4;
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
       r4 = L39;
       SARR((STR)r4,0,create_c4); 
       ;
       ret_val30 = r4;
       ret_val29 = ret_val30;
       s4 = ret_val29;
       ret_val28 = STR_ap1077157958(plus_self14, s4, TRUE);
       err_s4 = ret_val28;
       PROG_err_STR(ATTR(err_self4,prog), err_s4);
      } break;
     default: ;
     other102: ;
    }
    return;
  }
 }
 after_loop: ;
}
