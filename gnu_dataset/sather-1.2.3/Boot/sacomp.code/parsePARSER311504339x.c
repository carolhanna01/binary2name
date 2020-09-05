#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

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

typedef struct AS_AND_EXPR_struct {/* layout for AS_AND_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AND_EXPR;

typedef struct AS_ANY_EXPR_struct {/* layout for AS_ANY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ANY_EXPR;

typedef struct AS_ARG_MODE_struct {/* layout for AS_ARG_MODE */
 OB_HEADER header;
 struct AS_ARG_MODE_struct *next;
 INT mod;
 } *AS_ARG_MODE;

typedef struct AS_ARRAY_EXPR_struct {/* layout for AS_ARRAY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ARRAY_EXPR;

typedef struct AS_AT_EXPR_struct {/* layout for AS_AT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_AT_EXPR;

typedef struct AS_BOOL_LIT_EXPR_struct {/* layout for AS_BOOL_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AS_BOOL_LIT_EXPR;

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

typedef struct AS_CHAR_LIT_EXPR_struct {/* layout for AS_CHAR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT val1;
 struct SFILE_ID_struct source1;
 } *AS_CHAR_LIT_EXPR;

typedef struct AS_CLUSTER_EXPR_struct {/* layout for AS_CLUSTER_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLUSTER_EXPR;

typedef struct AS_CLU553570599_struct {/* layout for AS_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_CLU553570599;

typedef struct AS_COHORT_EXPR_struct {/* layout for AS_COHORT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct IDENT_struct name_after_pSath;
 struct SFILE_ID_struct source1;
 } *AS_COHORT_EXPR;

typedef struct AS_CONST_DEF_struct {/* layout for AS_CONST_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_CONST_DEF;

typedef struct AS_CREATE_EXPR_struct {/* layout for AS_CREATE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct AS_ARG_MODE_struct *modes1;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_CREATE_EXPR;

typedef struct AS_EXCEPT_EXPR_struct {/* layout for AS_EXCEPT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_EXCEPT_EXPR;

typedef struct AS_FAR_EXPR_struct {/* layout for AS_FAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_FAR_EXPR;

typedef struct AS_FLT_LIT_EXPR_struct {/* layout for AS_FLT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 INT tp;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AS_FLT_LIT_EXPR;

typedef struct AS_GLOBAL_EXPR_struct {/* layout for AS_GLOBAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_GLOBAL_EXPR;

typedef struct AS_HERE_EXPR_struct {/* layout for AS_HERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_HERE_EXPR;

typedef struct AS_INITIAL_EXPR_struct {/* layout for AS_INITIAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_INITIAL_EXPR;

typedef struct AS_INT_LIT_EXPR_struct {/* layout for AS_INT_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 BOOL is_inti;
 } *AS_INT_LIT_EXPR;

typedef struct AS_IS_VOID_EXPR_struct {/* layout for AS_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *arg;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_IS_VOID_EXPR;

typedef struct AS_NEAR_EXPR_struct {/* layout for AS_NEAR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEAR_EXPR;

typedef struct AS_NEW_EXPR_struct {/* layout for AS_NEW_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *arg;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_NEW_EXPR;

typedef struct AS_OR_EXPR_struct {/* layout for AS_OR_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e1;
 struct dAS_EXPR_struct *e2;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_OR_EXPR;

typedef struct AS_RESULT_EXPR_struct {/* layout for AS_RESULT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_RESULT_EXPR;

typedef struct AS_SELF_EXPR_struct {/* layout for AS_SELF_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_SELF_EXPR;

typedef struct AS_STR_LIT_EXPR_struct {/* layout for AS_STR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 STR s;
 } *AS_STR_LIT_EXPR;

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

typedef struct AS_VOID_EXPR_struct {/* layout for AS_VOID_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_VOID_EXPR;

typedef struct AS_WHERE_EXPR_struct {/* layout for AS_WHERE_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_WHERE_EXPR;

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

extern IDENT IDENT_1129727818;
extern IDENT IDENT_1616879092;
extern IDENT IDENT_1728630987;
extern IDENT IDENT_1837767448;
extern IDENT IDENT_1974148927;
extern IDENT IDENT_2085547498;
extern IDENT IDENT_2108657069;
extern IDENT IDENT_228725606;
extern IDENT IDENT_421158771;
extern IDENT IDENT_475003333;
extern IDENT IDENT_604723504;
extern IDENT IDENT_673807638;
extern INT AS_ARG1300937332;
extern INT AS_FLT388140455;
extern INT AS_TYPE_SPEC_ord;
extern INT PARSER1019557737;
extern INT PARSER1207020061;
extern INT PARSER1209676627;
extern INT PARSER1337387114;
extern INT PARSER1363330612;
extern INT PARSER1789316521;
extern INT PARSER1963707920;
extern INT PARSER265941484;
extern INT PARSER404310286;
extern INT PARSER460241977;
extern INT PARSER543515898;
extern INT PARSER550831344;
extern INT PARSER840587154;
extern INT PARSER978635316;
extern INT PARSER_ITER_tok;
extern INT PARSER_ROUT_tok;
extern INT PARSER_and_tok;
extern INT PARSER_any_tok;
extern INT PARSER_at_tok;
extern INT PARSER_colon_tok;
extern INT PARSER_comma_tok;
extern INT PARSER_const_tok;
extern INT PARSER_dot_tok;
extern INT PARSER_ident_tok;
extern INT PARSER_is_eq_tok;
extern INT PARSER_is_gt_tok;
extern INT PARSER_is_lt_tok;
extern INT PARSER_lflt_tok;
extern INT PARSER_lint_tok;
extern INT PARSER_minus_tok;
extern INT PARSER_mod_tok;
extern INT PARSER_not_tok;
extern INT PARSER_or_tok;
extern INT PARSER_plus_tok;
extern INT PARSER_pow_tok;
extern INT PARSER_times_tok;
extern INT PARSER_true_tok;
extern INT PARSER_under_tok;
extern INT PARSER_vbar_tok;

/* Function declarations */


extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_EX1889860882)(dAS_EXPR, dAS_EXPR);

extern RETURNED_CONST void (**dAS_EX753042462)(dAS_EXPR, SFILE_ID);
AS_ARG_MODE PARSER87164415(PARSER, AS_CALL_EXPR, INT);
AS_BOU14269336 PARSER1814172757(PARSER);
AS_CALL_EXPR PARSER716940617(PARSER, dAS_EXPR, AS_TYPE_SPEC, BOOL);
AS_CREATE_EXPR PARSER2102248401(PARSER);
AS_TYPE_SPEC PARSER1887452538(PARSER, dAS_EXPR);
AS_TYPE_SPEC PARSER2028498823(PARSER);
BOOL PARSER2128430730(PARSER, INT);
BOOL SCANNE363215919(SCANNER, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
IDENT IDENT_1150413730(IDENT, STR);
IDENT PARSER2046006032(PARSER, dAS_EXPR);
INTI1 INTI_c1588968505(INTI1, INT);
INTI1 INTI_d359767808(INTI1, INTI1);
INTI1 INTI_negaterINTI(INTI1);
SFILE_ID SFILE_1594766561(SFILE_ID);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR TOKEN_strrSTR(TOKEN);
TOKEN SCANNE1792387248(SCANNER);
TUPdAS268679779 PARSER851452257(PARSER, BOOL, BOOL);
dAS_CLASS_ELT PARSER576720148(PARSER, TOKEN);
dAS_EXPR PARSER1134972014(PARSER, BOOL);
dAS_EXPR PARSER1259074744(PARSER, BOOL);
dAS_EXPR PARSER1464753752(PARSER);
dAS_EXPR PARSER1465639274(PARSER);
dAS_EXPR PARSER1466524796(PARSER);
dAS_EXPR PARSER403674127(PARSER);
dAS_EXPR PARSER679630569(PARSER);
dAS_EXPR PARSER680516091(PARSER);
dAS_EXPR PARSER681401613(PARSER);
dAS_EXPR PARSER682287135(PARSER);
void AS_ARG1328804463(AS_ARG_MODE, AS_ARG_MODE);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_ARG_MODE PARSER87164415(PARSER self, AS_CALL_EXPR c, INT m) {
 AS_ARG_MODE ret_val;
 AS_ARG_MODE res = ((AS_ARG_MODE) NULL);
 AS_ARG_MODE mode1 = ((AS_ARG_MODE) NULL);
 dAS_EXPR a;
 AS_ARG_MODE create_self;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val1;
 AS_ARG_MODE res1;
 BOOL L1;
 BOOL L21_;
 AS_ARG_MODE L3;
 OB L4;
 dAS_EXPR L5;
 a = ATTR(c,args);
 while (1) {
  L1 = (a==((dAS_EXPR) NULL));
  L21_=!(L1); 
  if (L21_) {
  }
  else {
   goto after_loop;
  }
  create_self = ((AS_ARG_MODE) NULL);
  create_m = m;
  L4=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=AS_ARG_MODE_tag;
  L3 = ((AS_ARG_MODE) L4);
  res1 = L3;
  SATTR(res1,mod,create_m);
  ret_val1 = res1;
  mode1 = ret_val1;
  if ((res==((AS_ARG_MODE) NULL))) {
   res = mode1;
  }
  else {
   AS_ARG1328804463(res, mode1);
  }
  L5 = a;
  a = (*dAS_EX337431651[TAG(L5)])(L5);
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPE_SPEC PARSER1887452538(PARSER self, dAS_EXPR x) {
 AS_TYPE_SPEC ret_val;
 AS_TYPE_SPEC tp;
 PARSER is_class_name_se;
 IDENT is_class_name_x = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 AS_TYPE_SPEC create_self;
 AS_TYPE_SPEC ret_val2;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val5;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 PARSER err_self;
 STR err_s;
 AS_TYPE_SPEC L1;
 OB L2;
 extern STR typesp201922702;
 extern STR in1;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 if (x==NULL) {
  goto other104;
 } else
 switch (TAG(x)) {
  case AS_CALL_EXPR_tag:
   if ((ATTR(((AS_CALL_EXPR) x),ob)==((dAS_EXPR) NULL))) {
    if ((ATTR(((AS_CALL_EXPR) x),tp)==((AS_TYPE_SPEC) NULL))) {
     is_class_name_se = self;
     is_class_name_x = ATTR(((AS_CALL_EXPR) x),name1);
     ret_val1 = SCANNE363215919(ATTR(is_class_name_se,scanner), is_class_name_x.str);
     if (ret_val1) {
      create_self = ((AS_TYPE_SPEC) NULL);
      L2=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
      if (L2==NULL) FATAL("Unable to allocate more memory");
      ((OB)L2)->header.tag=AS_TYPE_SPEC_tag;
      L1 = ((AS_TYPE_SPEC) L2);
      ret_val2 = L1;
      tp = ret_val2;
      SATTR(tp,source1,ATTR(((AS_CALL_EXPR) x),source1));
      SATTR(tp,kind,AS_TYPE_SPEC_ord);
      SATTR(tp,is_hot,FALSE);
      SATTR(tp,name1,ATTR(((AS_CALL_EXPR) x),name1));
      SATTR(tp,params,((AS_TYPE_SPEC) NULL));
      SATTR(tp,ret,((AS_TYPE_SPEC) NULL));
      ret_val = tp;
      return ret_val;
     }
    }
    else {
     ret_val = ATTR(((AS_CALL_EXPR) x),tp);
     return ret_val;
    }
   } break;
  default: ;
  other104: ;
 }
 error_self = self;
 error_msg = ((STR) &typesp201922702);
 set_eloc_self = ATTR(error_self,prog);
 source_loc_self = error_self;
 ret_val3 = SFILE_1594766561(SFILE_ID_zero);
 set_eloc_l = ret_val3;
 SATTR(set_eloc_self,eloc,set_eloc_l);
 err_self = error_self;
 plus_self = error_msg;
 plus_sarg = ((STR) &in1);
 ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
 plus_self1 = ret_val4;
 plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
 ret_val5 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val5;
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
 ret_val8 = r;
 ret_val7 = ret_val8;
 s = ret_val7;
 ret_val6 = STR_ap1077157958(plus_self2, s, TRUE);
 err_s = ret_val6;
 PROG_err_STR(ATTR(err_self,prog), err_s);
 ret_val = ((AS_TYPE_SPEC) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

IDENT PARSER2046006032(PARSER self, dAS_EXPR x) {
 IDENT ret_val = IDENT_zero;
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
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L41_;
 extern STR identi1145332996;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 if (x==NULL) {
  goto other105;
 } else
 switch (TAG(x)) {
  case AS_CALL_EXPR_tag:
   if ((ATTR(((AS_CALL_EXPR) x),ob)==((dAS_EXPR) NULL))) {
    L3 = (ATTR(((AS_CALL_EXPR) x),name1).str==(STR)0);
    L41_=!(L3); 
    L2 = L41_;
   } else {
    L2 = FALSE;
   }
   if (L2) {
    L1 = (ATTR(((AS_CALL_EXPR) x),args)==((dAS_EXPR) NULL));
   } else {
    L1 = FALSE;
   }
   if (L1) {
    ret_val = ATTR(((AS_CALL_EXPR) x),name1);
    return ret_val;
   } break;
  default: ;
  other105: ;
 }
 error_self = self;
 error_msg = ((STR) &identi1145332996);
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
 L6 = 1;
 L8=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L6)*sizeof(CHAR);
 L7=ZALLOC_LEAF_BIG(L8);
 if (L7==NULL) FATAL("Unable to allocate more memory");
 memset(L7,0,L8);
 ((OB)L7)->header.tag=STR_tag;
#ifdef DESTROY_CHK

   ((OB)L7)->header.destroyed=0;
#endif

 L5 = ((STR) L7);
 L5->asize = L6;
 r = L5;
 SARR((STR)r,0,create_c); 
 ;
 ret_val6 = r;
 ret_val5 = ret_val6;
 s = ret_val5;
 ret_val4 = STR_ap1077157958(plus_self2, s, TRUE);
 err_s = ret_val4;
 PROG_err_STR(ATTR(err_self,prog), err_s);
 ret_val = IDENT_zero;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT PARSER576720148(PARSER self, TOKEN mode1) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 AS_CONST_DEF con;
 AS_INT_LIT_EXPR zero1;
 INT counter = INT_zero;
 AS_CONST_DEF newc;
 AS_INT_LIT_EXPR arg;
 AS_CALL_EXPR ex1;
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
 AS_CONST_DEF create_self3;
 AS_CONST_DEF ret_val18;
 PARSER source_loc_self2;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val20 = BOOL_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val21 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val22 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val23;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val24;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val25 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val26;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val27;
 STR plus_self12;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val28;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val29;
 STR create_self5;
 CHAR create_c2 = CHAR_zero;
 STR ret_val30;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self1;
 PARSER ident_self;
 IDENT ret_val31 = IDENT_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val32 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val33 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val34;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val35;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val36 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val37;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val38;
 STR plus_self17;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val39;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val40;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val41;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self2;
 AS_INT_LIT_EXPR create_self8;
 AS_INT_LIT_EXPR ret_val42;
 PARSER source_loc_self5;
 SFILE_ID ret_val43 = SFILE_ID_zero;
 AS_CONST_DEF create_self9;
 AS_CONST_DEF ret_val44;
 PARSER source_loc_self6;
 SFILE_ID ret_val45 = SFILE_ID_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val46 = BOOL_zero;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val47 = BOOL_zero;
 TOKEN create_self10 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val48 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val49;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val50;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self7;
 SFILE_ID ret_val51 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val52;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val53;
 STR plus_self22;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val54;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val55;
 STR create_self11;
 CHAR create_c4 = CHAR_zero;
 STR ret_val56;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self3;
 PARSER ident_self1;
 IDENT ret_val57 = IDENT_zero;
 AS_INT_LIT_EXPR create_self12;
 AS_INT_LIT_EXPR ret_val58;
 PARSER source_loc_self8;
 SFILE_ID ret_val59 = SFILE_ID_zero;
 AS_CALL_EXPR create_self13;
 AS_CALL_EXPR ret_val60;
 PARSER source_loc_self9;
 SFILE_ID ret_val61 = SFILE_ID_zero;
 AS_ARG_MODE create_self14;
 INT create_m = INT_zero;
 AS_ARG_MODE ret_val62;
 AS_ARG_MODE res1;
 PARSER exit_self;
 STR s5;
 extern STR constdefinition;
 INT L1;
 BOOL L21_;
 extern STR readon1107210040;
 extern STR in1;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 INT L8;
 BOOL L91_;
 BOOL L10;
 BOOL L111_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 AS_CONST_DEF L17;
 OB L18;
 INT L19;
 BOOL L201_;
 INT L22;
 BOOL L231_;
 BOOL L24;
 BOOL L251_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L26;
 INT L27;
 OB L28;
 INT L29;
 INT L31;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L35;
 INT L36;
 OB L37;
 INT L38;
 AS_INT_LIT_EXPR L40;
 OB L41;
 AS_CONST_DEF L42;
 OB L43;
 INT L44;
 BOOL L451_;
 INT L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L50;
 INT L51;
 OB L52;
 INT L53;
 AS_INT_LIT_EXPR L55;
 OB L56;
 AS_CALL_EXPR L57;
 OB L58;
 AS_ARG_MODE L59;
 OB L60;
 dAS_CLASS_ELT L61;
 INT L621_;
 enter_self = self;
 enter_s = ((STR) &constdefinition);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = mode1;
 is_eq_y = PARSER550831344;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  error_self = self;
  error_msg = ((STR) &readon1107210040);
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
  ret_val7 = r;
  ret_val6 = ret_val7;
  s = ret_val6;
  ret_val5 = STR_ap1077157958(plus_self2, s, TRUE);
  err_s = ret_val5;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 match_self = self;
 match_t = PARSER_const_tok;
 is_eq_self1 = ATTR(match_self,next);
 is_eq_y1 = match_t;
 L8 = is_eq_self1.val1;
 L91_=(L8)==(is_eq_y1); 
 ret_val8 = L91_;
 L10 = ret_val8;
 L111_=!(L10); 
 if (L111_) {
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
  r1 = L12;
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
 create_self3 = ((AS_CONST_DEF) NULL);
 L18=ZALLOC(sizeof(struct AS_CONST_DEF_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=AS_CONST_DEF_tag;
 L17 = ((AS_CONST_DEF) L18);
 ret_val18 = L17;
 con = ret_val18;
 source_loc_self2 = self;
 ret_val19 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(con,source1,ret_val19);
 is_eq_self2 = mode1;
 is_eq_y2 = PARSER1019557737;
 L19 = is_eq_self2.val1;
 L201_=(L19)==(is_eq_y2); 
 ret_val20 = L201_;
 SATTR(con,is_private,ret_val20);
 res = ((dAS_CLASS_ELT) con);
 match_self1 = self;
 match_t1 = PARSER_ident_tok;
 is_eq_self3 = ATTR(match_self1,next);
 is_eq_y3 = match_t1;
 L22 = is_eq_self3.val1;
 L231_=(L22)==(is_eq_y3); 
 ret_val21 = L231_;
 L24 = ret_val21;
 L251_=!(L24); 
 if (L251_) {
  exp_error_self1 = match_self1;
  create_self4 = TOKEN_zero;
  create_val1 = match_t1;
  t1.val1 = create_val1;
  ret_val22 = t1;
  exp_error_msg1 = TOKEN_strrSTR(ret_val22);
  error_self2 = exp_error_self1;
  plus_self8 = exp_error_msg1;
  plus_sarg6 = ((STR) &expectedbutfound);
  ret_val23 = STR_ap2004550586(plus_self8, plus_sarg6);
  plus_self9 = ret_val23;
  plus_sarg7 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val24 = STR_ap2004550586(plus_self9, plus_sarg7);
  error_msg2 = ret_val24;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self3 = error_self2;
  ret_val25 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val25;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self10 = error_msg2;
  plus_sarg8 = ((STR) &in1);
  ret_val26 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val26;
  plus_sarg9 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val27 = STR_ap2004550586(plus_self11, plus_sarg9);
  plus_self12 = ret_val27;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self5 = ((STR) NULL);
  create_c2 = str_self2;
  L27 = 1;
  L29=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L27)*sizeof(CHAR);
  L28=ZALLOC_LEAF_BIG(L29);
  if (L28==NULL) FATAL("Unable to allocate more memory");
  memset(L28,0,L29);
  ((OB)L28)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L28)->header.destroyed=0;
#endif

  L26 = ((STR) L28);
  L26->asize = L27;
  r2 = L26;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val30 = r2;
  ret_val29 = ret_val30;
  s2 = ret_val29;
  ret_val28 = STR_ap1077157958(plus_self12, s2, TRUE);
  err_s2 = ret_val28;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self1 = match_self1;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 ident_self = self;
 ret_val31 = ATTR(ATTR(ident_self,scanner),lex_value);
 SATTR(con,name1,ret_val31);
 if (PARSER2128430730(self, PARSER_colon_tok)) {
  SATTR(con,tp,PARSER2028498823(self));
  match_self2 = self;
  match_t2 = PARSER265941484;
  is_eq_self4 = ATTR(match_self2,next);
  is_eq_y4 = match_t2;
  L31 = is_eq_self4.val1;
  L321_=(L31)==(is_eq_y4); 
  ret_val32 = L321_;
  L33 = ret_val32;
  L341_=!(L33); 
  if (L341_) {
   exp_error_self2 = match_self2;
   create_self6 = TOKEN_zero;
   create_val2 = match_t2;
   t2.val1 = create_val2;
   ret_val33 = t2;
   exp_error_msg2 = TOKEN_strrSTR(ret_val33);
   error_self3 = exp_error_self2;
   plus_self13 = exp_error_msg2;
   plus_sarg10 = ((STR) &expectedbutfound);
   ret_val34 = STR_ap2004550586(plus_self13, plus_sarg10);
   plus_self14 = ret_val34;
   plus_sarg11 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
   ret_val35 = STR_ap2004550586(plus_self14, plus_sarg11);
   error_msg3 = ret_val35;
   set_eloc_self3 = ATTR(error_self3,prog);
   source_loc_self4 = error_self3;
   ret_val36 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l3 = ret_val36;
   SATTR(set_eloc_self3,eloc,set_eloc_l3);
   err_self3 = error_self3;
   plus_self15 = error_msg3;
   plus_sarg12 = ((STR) &in1);
   ret_val37 = STR_ap2004550586(plus_self15, plus_sarg12);
   plus_self16 = ret_val37;
   plus_sarg13 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
   ret_val38 = STR_ap2004550586(plus_self16, plus_sarg13);
   plus_self17 = ret_val38;
   plus_arg3 = ')';
   str_self3 = plus_arg3;
   create_self7 = ((STR) NULL);
   create_c3 = str_self3;
   L36 = 1;
   L38=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L36)*sizeof(CHAR);
   L37=ZALLOC_LEAF_BIG(L38);
   if (L37==NULL) FATAL("Unable to allocate more memory");
   memset(L37,0,L38);
   ((OB)L37)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L37)->header.destroyed=0;
#endif

   L35 = ((STR) L37);
   L35->asize = L36;
   r3 = L35;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val41 = r3;
   ret_val40 = ret_val41;
   s3 = ret_val40;
   ret_val39 = STR_ap1077157958(plus_self17, s3, TRUE);
   err_s3 = ret_val39;
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
  }
  fetch_self2 = match_self2;
  SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
  SATTR(con,init,PARSER403674127(self));
 }
 else {
  if (PARSER2128430730(self, PARSER265941484)) {
   SATTR(con,init,PARSER403674127(self));
  }
  else {
   create_self8 = ((AS_INT_LIT_EXPR) NULL);
   L41=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
   if (L41==NULL) FATAL("Unable to allocate more memory");
   ((OB)L41)->header.tag=AS_INT_LIT_EXPR_tag;
   L40 = ((AS_INT_LIT_EXPR) L41);
   ret_val42 = L40;
   zero1 = ret_val42;
   source_loc_self5 = self;
   ret_val43 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(zero1,source1,ret_val43);
   SATTR(zero1,val1,INTI_c1588968505(((INTI1) NULL), 0));
   SATTR(con,init,((dAS_EXPR) zero1));
  }
  counter = 1;
  while (1) {
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop;
   }
   create_self9 = ((AS_CONST_DEF) NULL);
   L43=ZALLOC(sizeof(struct AS_CONST_DEF_struct));
   if (L43==NULL) FATAL("Unable to allocate more memory");
   ((OB)L43)->header.tag=AS_CONST_DEF_tag;
   L42 = ((AS_CONST_DEF) L43);
   ret_val44 = L42;
   newc = ret_val44;
   source_loc_self6 = self;
   ret_val45 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(newc,source1,ret_val45);
   is_eq_self5 = mode1;
   is_eq_y5 = PARSER1019557737;
   L44 = is_eq_self5.val1;
   L451_=(L44)==(is_eq_y5); 
   ret_val46 = L451_;
   SATTR(newc,is_private,ret_val46);
   match_self3 = self;
   match_t3 = PARSER_ident_tok;
   is_eq_self6 = ATTR(match_self3,next);
   is_eq_y6 = match_t3;
   L46 = is_eq_self6.val1;
   L471_=(L46)==(is_eq_y6); 
   ret_val47 = L471_;
   L48 = ret_val47;
   L491_=!(L48); 
   if (L491_) {
    exp_error_self3 = match_self3;
    create_self10 = TOKEN_zero;
    create_val3 = match_t3;
    t3.val1 = create_val3;
    ret_val48 = t3;
    exp_error_msg3 = TOKEN_strrSTR(ret_val48);
    error_self4 = exp_error_self3;
    plus_self18 = exp_error_msg3;
    plus_sarg14 = ((STR) &expectedbutfound);
    ret_val49 = STR_ap2004550586(plus_self18, plus_sarg14);
    plus_self19 = ret_val49;
    plus_sarg15 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
    ret_val50 = STR_ap2004550586(plus_self19, plus_sarg15);
    error_msg4 = ret_val50;
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self7 = error_self4;
    ret_val51 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val51;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self20 = error_msg4;
    plus_sarg16 = ((STR) &in1);
    ret_val52 = STR_ap2004550586(plus_self20, plus_sarg16);
    plus_self21 = ret_val52;
    plus_sarg17 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val53 = STR_ap2004550586(plus_self21, plus_sarg17);
    plus_self22 = ret_val53;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self11 = ((STR) NULL);
    create_c4 = str_self4;
    L51 = 1;
    L53=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L51)*sizeof(CHAR);
    L52=ZALLOC_LEAF_BIG(L53);
    if (L52==NULL) FATAL("Unable to allocate more memory");
    memset(L52,0,L53);
    ((OB)L52)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L52)->header.destroyed=0;
#endif

    L50 = ((STR) L52);
    L50->asize = L51;
    r4 = L50;
    SARR((STR)r4,0,create_c4); 
    ;
    ret_val56 = r4;
    ret_val55 = ret_val56;
    s4 = ret_val55;
    ret_val54 = STR_ap1077157958(plus_self22, s4, TRUE);
    err_s4 = ret_val54;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   fetch_self3 = match_self3;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   ident_self1 = self;
   ret_val57 = ATTR(ATTR(ident_self1,scanner),lex_value);
   SATTR(newc,name1,ret_val57);
   create_self12 = ((AS_INT_LIT_EXPR) NULL);
   L56=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
   if (L56==NULL) FATAL("Unable to allocate more memory");
   ((OB)L56)->header.tag=AS_INT_LIT_EXPR_tag;
   L55 = ((AS_INT_LIT_EXPR) L56);
   ret_val58 = L55;
   arg = ret_val58;
   source_loc_self8 = self;
   ret_val59 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(arg,source1,ret_val59);
   SATTR(arg,val1,INTI_c1588968505(((INTI1) NULL), counter));
   create_self13 = ((AS_CALL_EXPR) NULL);
   L58=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L58==NULL) FATAL("Unable to allocate more memory");
   ((OB)L58)->header.tag=AS_CALL_EXPR_tag;
   L57 = ((AS_CALL_EXPR) L58);
   ret_val60 = L57;
   ex1 = ret_val60;
   source_loc_self9 = self;
   ret_val61 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(ex1,source1,ret_val61);
   SATTR(ex1,ob,ATTR(con,init));
   SATTR(ex1,name1,IDENT_2085547498);
   SATTR(ex1,args,((dAS_EXPR) arg));
   create_self14 = ((AS_ARG_MODE) NULL);
   create_m = AS_ARG1300937332;
   L60=ZALLOC(sizeof(struct AS_ARG_MODE_struct));
   if (L60==NULL) FATAL("Unable to allocate more memory");
   ((OB)L60)->header.tag=AS_ARG_MODE_tag;
   L59 = ((AS_ARG_MODE) L60);
   res1 = L59;
   SATTR(res1,mod,create_m);
   ret_val62 = res1;
   SATTR(ex1,modes1,ret_val62);
   SATTR(newc,init,((dAS_EXPR) ex1));
   if ((res==((dAS_CLASS_ELT) NULL))) {
    res = ((dAS_CLASS_ELT) newc);
   }
   else {
    L61 = res;
    (*dAS_CL1056554733[TAG(L61)])(L61, ((dAS_CLASS_ELT) newc));
   }
   L621_=INTPLUS(counter,1); 
   counter = L621_;
  }
  after_loop: ;
 }
 exit_self = self;
 s5 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER1134972014(PARSER self, BOOL underscore_args) {
 dAS_EXPR ret_val;
 dAS_EXPR res = ((dAS_EXPR) NULL);
 dAS_EXPR x = ((dAS_EXPR) NULL);
 AS_UND152986614 u;
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
 PARSER error_self;
 STR error_msg;
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
 PARSER err_self;
 STR err_s;
 PARSER exit_self;
 STR s1;
 extern STR listof717650279;
 extern STR listof1135892450;
 AS_UND152986614 L1;
 OB L2;
 BOOL L31_;
 extern STR nounde75550121;
 extern STR in1;
 STR L4;
 INT L5;
 OB L6;
 INT L7;
 dAS_EXPR L9;
 if (underscore_args) {
  enter_self = self;
  enter_s = ((STR) &listof717650279);
  SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 }
 else {
  enter_self1 = self;
  enter_s1 = ((STR) &listof1135892450);
  SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
 }
 while (1) {
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
   }
   L31_=!(underscore_args); 
   if (L31_) {
    error_self = self;
    error_msg = ((STR) &nounde75550121);
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self2 = error_self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val4;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self = error_msg;
    plus_sarg = ((STR) &in1);
    ret_val5 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val5;
    plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val6 = STR_ap2004550586(plus_self1, plus_sarg1);
    plus_self2 = ret_val6;
    plus_arg = ')';
    str_self = plus_arg;
    create_self1 = ((STR) NULL);
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
    ret_val9 = r;
    ret_val8 = ret_val9;
    s = ret_val8;
    ret_val7 = STR_ap1077157958(plus_self2, s, TRUE);
    err_s = ret_val7;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
  }
  else {
   x = PARSER403674127(self);
  }
  if ((res==((dAS_EXPR) NULL))) {
   res = x;
  }
  else {
   L9 = res;
   (*dAS_EX1889860882[TAG(L9)])(L9, x);
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 exit_self = self;
 s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER1259074744(PARSER self, BOOL underscore_args) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 AS_CALL_EXPR c = ((AS_CALL_EXPR) NULL);
 TUPdAS268679779 t = TUPdAS268679779_zero;
 TUPdAS268679779 t1 = TUPdAS268679779_zero;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER append_bang_self;
 IDENT append_bang_arg = IDENT_zero;
 IDENT ret_val3 = IDENT_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val5 = BOOL_zero;
 TOKEN create_self1 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val6 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val7;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val8;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self1;
 SFILE_ID ret_val9 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val10;
 STR plus_self4;
 STR plus_sarg4;
 STR ret_val11;
 STR plus_self5;
 CHAR plus_arg = CHAR_zero;
 STR ret_val12;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val13;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val14;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val15;
 PARSER source_loc_self2;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val17 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val18 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val19;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val20;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self3;
 SFILE_ID ret_val21 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val22;
 STR plus_self9;
 STR plus_sarg8;
 STR ret_val23;
 STR plus_self10;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val24;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val25;
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val26;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 AS_CALL_EXPR create_self6;
 AS_CALL_EXPR ret_val27;
 PARSER source_loc_self4;
 SFILE_ID ret_val28 = SFILE_ID_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val29 = BOOL_zero;
 TOKEN create_self7 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val30 = TOKEN_zero;
 TOKEN t4 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val31;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val32;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self5;
 SFILE_ID ret_val33 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val34;
 STR plus_self14;
 STR plus_sarg12;
 STR ret_val35;
 STR plus_self15;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val36;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val37;
 STR create_self8;
 CHAR create_c2 = CHAR_zero;
 STR ret_val38;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 AS_CALL_EXPR create_self9;
 AS_CALL_EXPR ret_val39;
 PARSER source_loc_self6;
 SFILE_ID ret_val40 = SFILE_ID_zero;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val41 = BOOL_zero;
 TOKEN create_self10 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val42 = TOKEN_zero;
 TOKEN t5 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val43;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val44;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self7;
 SFILE_ID ret_val45 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val46;
 STR plus_self19;
 STR plus_sarg16;
 STR ret_val47;
 STR plus_self20;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val48;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val49;
 STR create_self11;
 CHAR create_c3 = CHAR_zero;
 STR ret_val50;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self3;
 PARSER exit_self;
 STR s4;
 extern STR expressionprec1;
 AS_TYPE_SPEC L1;
 AS_CALL_EXPR L2;
 OB L3;
 extern STR b;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L8;
 INT L9;
 OB L10;
 INT L11;
 AS_CALL_EXPR L13;
 OB L14;
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
 AS_CALL_EXPR L24;
 OB L25;
 INT L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L30;
 INT L31;
 OB L32;
 INT L33;
 AS_CALL_EXPR L35;
 OB L36;
 INT L37;
 BOOL L381_;
 BOOL L39;
 BOOL L401_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L41;
 INT L42;
 OB L43;
 INT L44;
 enter_self = self;
 enter_s = ((STR) &expressionprec1);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER679630569(self);
 if (PARSER2128430730(self, PARSER_dot_tok)) {
  res = ((dAS_EXPR) PARSER716940617(self, res, ((AS_TYPE_SPEC) NULL), underscore_args));
 }
 else {
  if (PARSER2128430730(self, PARSER978635316)) {
   L1 = PARSER1887452538(self, res);
   res = ((dAS_EXPR) PARSER716940617(self, ((dAS_EXPR) NULL), L1, underscore_args));
  }
  else {
   if (PARSER2128430730(self, PARSER840587154)) {
    create_self = ((AS_CALL_EXPR) NULL);
    L3=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L3==NULL) FATAL("Unable to allocate more memory");
    ((OB)L3)->header.tag=AS_CALL_EXPR_tag;
    L2 = ((AS_CALL_EXPR) L3);
    ret_val1 = L2;
    c = ret_val1;
    source_loc_self = self;
    ret_val2 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(c,source1,ret_val2);
    append_bang_self = self;
    append_bang_arg = PARSER2046006032(self, res);
    plus_self = append_bang_arg.str;
    plus_sarg = ((STR) &b);
    ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
    ret_val3 = IDENT_1150413730(IDENT_zero, ret_val4);
    SATTR(c,name1,ret_val3);
    if (PARSER2128430730(self, PARSER1209676627)) {
     t = PARSER851452257(self, underscore_args, TRUE);
     SATTR(c,args,t.t1);
     SATTR(c,modes1,t.t2);
     match_self = self;
     match_t = PARSER1207020061;
     is_eq_self = ATTR(match_self,next);
     is_eq_y = match_t;
     L4 = is_eq_self.val1;
     L51_=(L4)==(is_eq_y); 
     ret_val5 = L51_;
     L6 = ret_val5;
     L71_=!(L6); 
     if (L71_) {
      exp_error_self = match_self;
      create_self1 = TOKEN_zero;
      create_val = match_t;
      t2.val1 = create_val;
      ret_val6 = t2;
      exp_error_msg = TOKEN_strrSTR(ret_val6);
      error_self = exp_error_self;
      plus_self1 = exp_error_msg;
      plus_sarg1 = ((STR) &expectedbutfound);
      ret_val7 = STR_ap2004550586(plus_self1, plus_sarg1);
      plus_self2 = ret_val7;
      plus_sarg2 = TOKEN_strrSTR(ATTR(exp_error_self,next));
      ret_val8 = STR_ap2004550586(plus_self2, plus_sarg2);
      error_msg = ret_val8;
      set_eloc_self = ATTR(error_self,prog);
      source_loc_self1 = error_self;
      ret_val9 = SFILE_1594766561(SFILE_ID_zero);
      set_eloc_l = ret_val9;
      SATTR(set_eloc_self,eloc,set_eloc_l);
      err_self = error_self;
      plus_self3 = error_msg;
      plus_sarg3 = ((STR) &in1);
      ret_val10 = STR_ap2004550586(plus_self3, plus_sarg3);
      plus_self4 = ret_val10;
      plus_sarg4 = FLISTSTR_toprSTR(ATTR(error_self,entered));
      ret_val11 = STR_ap2004550586(plus_self4, plus_sarg4);
      plus_self5 = ret_val11;
      plus_arg = ')';
      str_self = plus_arg;
      create_self2 = ((STR) NULL);
      create_c = str_self;
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
      ret_val14 = r;
      ret_val13 = ret_val14;
      s = ret_val13;
      ret_val12 = STR_ap1077157958(plus_self5, s, TRUE);
      err_s = ret_val12;
      PROG_err_STR(ATTR(err_self,prog), err_s);
     }
     fetch_self = match_self;
     SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
    }
    res = ((dAS_EXPR) c);
   }
   else {
    if (PARSER2128430730(self, PARSER1209676627)) {
     create_self3 = ((AS_CALL_EXPR) NULL);
     L14=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
     if (L14==NULL) FATAL("Unable to allocate more memory");
     ((OB)L14)->header.tag=AS_CALL_EXPR_tag;
     L13 = ((AS_CALL_EXPR) L14);
     ret_val15 = L13;
     c = ret_val15;
     source_loc_self2 = self;
     ret_val16 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(c,source1,ret_val16);
     SATTR(c,name1,PARSER2046006032(self, res));
     t1 = PARSER851452257(self, underscore_args, FALSE);
     SATTR(c,args,t1.t1);
     SATTR(c,modes1,t1.t2);
     match_self1 = self;
     match_t1 = PARSER1207020061;
     is_eq_self1 = ATTR(match_self1,next);
     is_eq_y1 = match_t1;
     L15 = is_eq_self1.val1;
     L161_=(L15)==(is_eq_y1); 
     ret_val17 = L161_;
     L17 = ret_val17;
     L181_=!(L17); 
     if (L181_) {
      exp_error_self1 = match_self1;
      create_self4 = TOKEN_zero;
      create_val1 = match_t1;
      t3.val1 = create_val1;
      ret_val18 = t3;
      exp_error_msg1 = TOKEN_strrSTR(ret_val18);
      error_self1 = exp_error_self1;
      plus_self6 = exp_error_msg1;
      plus_sarg5 = ((STR) &expectedbutfound);
      ret_val19 = STR_ap2004550586(plus_self6, plus_sarg5);
      plus_self7 = ret_val19;
      plus_sarg6 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
      ret_val20 = STR_ap2004550586(plus_self7, plus_sarg6);
      error_msg1 = ret_val20;
      set_eloc_self1 = ATTR(error_self1,prog);
      source_loc_self3 = error_self1;
      ret_val21 = SFILE_1594766561(SFILE_ID_zero);
      set_eloc_l1 = ret_val21;
      SATTR(set_eloc_self1,eloc,set_eloc_l1);
      err_self1 = error_self1;
      plus_self8 = error_msg1;
      plus_sarg7 = ((STR) &in1);
      ret_val22 = STR_ap2004550586(plus_self8, plus_sarg7);
      plus_self9 = ret_val22;
      plus_sarg8 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
      ret_val23 = STR_ap2004550586(plus_self9, plus_sarg8);
      plus_self10 = ret_val23;
      plus_arg1 = ')';
      str_self1 = plus_arg1;
      create_self5 = ((STR) NULL);
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
      ret_val26 = r1;
      ret_val25 = ret_val26;
      s1 = ret_val25;
      ret_val24 = STR_ap1077157958(plus_self10, s1, TRUE);
      err_s1 = ret_val24;
      PROG_err_STR(ATTR(err_self1,prog), err_s1);
     }
     fetch_self1 = match_self1;
     SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
     res = ((dAS_EXPR) c);
    }
    else {
     if (PARSER2128430730(self, PARSER404310286)) {
      create_self6 = ((AS_CALL_EXPR) NULL);
      L25=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
      if (L25==NULL) FATAL("Unable to allocate more memory");
      ((OB)L25)->header.tag=AS_CALL_EXPR_tag;
      L24 = ((AS_CALL_EXPR) L25);
      ret_val27 = L24;
      c = ret_val27;
      source_loc_self4 = self;
      ret_val28 = SFILE_1594766561(SFILE_ID_zero);
      SATTR(c,source1,ret_val28);
      SATTR(c,ob,res);
      SATTR(c,args,PARSER1134972014(self, FALSE));
      SATTR(c,is_array,TRUE);
      SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
      match_self2 = self;
      match_t2 = PARSER1363330612;
      is_eq_self2 = ATTR(match_self2,next);
      is_eq_y2 = match_t2;
      L26 = is_eq_self2.val1;
      L271_=(L26)==(is_eq_y2); 
      ret_val29 = L271_;
      L28 = ret_val29;
      L291_=!(L28); 
      if (L291_) {
       exp_error_self2 = match_self2;
       create_self7 = TOKEN_zero;
       create_val2 = match_t2;
       t4.val1 = create_val2;
       ret_val30 = t4;
       exp_error_msg2 = TOKEN_strrSTR(ret_val30);
       error_self2 = exp_error_self2;
       plus_self11 = exp_error_msg2;
       plus_sarg9 = ((STR) &expectedbutfound);
       ret_val31 = STR_ap2004550586(plus_self11, plus_sarg9);
       plus_self12 = ret_val31;
       plus_sarg10 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
       ret_val32 = STR_ap2004550586(plus_self12, plus_sarg10);
       error_msg2 = ret_val32;
       set_eloc_self2 = ATTR(error_self2,prog);
       source_loc_self5 = error_self2;
       ret_val33 = SFILE_1594766561(SFILE_ID_zero);
       set_eloc_l2 = ret_val33;
       SATTR(set_eloc_self2,eloc,set_eloc_l2);
       err_self2 = error_self2;
       plus_self13 = error_msg2;
       plus_sarg11 = ((STR) &in1);
       ret_val34 = STR_ap2004550586(plus_self13, plus_sarg11);
       plus_self14 = ret_val34;
       plus_sarg12 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
       ret_val35 = STR_ap2004550586(plus_self14, plus_sarg12);
       plus_self15 = ret_val35;
       plus_arg2 = ')';
       str_self2 = plus_arg2;
       create_self8 = ((STR) NULL);
       create_c2 = str_self2;
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
       r2 = L30;
       SARR((STR)r2,0,create_c2); 
       ;
       ret_val38 = r2;
       ret_val37 = ret_val38;
       s2 = ret_val37;
       ret_val36 = STR_ap1077157958(plus_self15, s2, TRUE);
       err_s2 = ret_val36;
       PROG_err_STR(ATTR(err_self2,prog), err_s2);
      }
      fetch_self2 = match_self2;
      SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
      res = ((dAS_EXPR) c);
     }
    }
   }
  }
 }
 while (1) {
  if (PARSER2128430730(self, PARSER_dot_tok)) {
   res = ((dAS_EXPR) PARSER716940617(self, res, ((AS_TYPE_SPEC) NULL), underscore_args));
  }
  else {
   if (PARSER2128430730(self, PARSER404310286)) {
    create_self9 = ((AS_CALL_EXPR) NULL);
    L36=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=AS_CALL_EXPR_tag;
    L35 = ((AS_CALL_EXPR) L36);
    ret_val39 = L35;
    c = ret_val39;
    source_loc_self6 = self;
    ret_val40 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(c,source1,ret_val40);
    SATTR(c,ob,res);
    SATTR(c,args,PARSER1134972014(self, FALSE));
    SATTR(c,is_array,TRUE);
    SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
    match_self3 = self;
    match_t3 = PARSER1363330612;
    is_eq_self3 = ATTR(match_self3,next);
    is_eq_y3 = match_t3;
    L37 = is_eq_self3.val1;
    L381_=(L37)==(is_eq_y3); 
    ret_val41 = L381_;
    L39 = ret_val41;
    L401_=!(L39); 
    if (L401_) {
     exp_error_self3 = match_self3;
     create_self10 = TOKEN_zero;
     create_val3 = match_t3;
     t5.val1 = create_val3;
     ret_val42 = t5;
     exp_error_msg3 = TOKEN_strrSTR(ret_val42);
     error_self3 = exp_error_self3;
     plus_self16 = exp_error_msg3;
     plus_sarg13 = ((STR) &expectedbutfound);
     ret_val43 = STR_ap2004550586(plus_self16, plus_sarg13);
     plus_self17 = ret_val43;
     plus_sarg14 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
     ret_val44 = STR_ap2004550586(plus_self17, plus_sarg14);
     error_msg3 = ret_val44;
     set_eloc_self3 = ATTR(error_self3,prog);
     source_loc_self7 = error_self3;
     ret_val45 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l3 = ret_val45;
     SATTR(set_eloc_self3,eloc,set_eloc_l3);
     err_self3 = error_self3;
     plus_self18 = error_msg3;
     plus_sarg15 = ((STR) &in1);
     ret_val46 = STR_ap2004550586(plus_self18, plus_sarg15);
     plus_self19 = ret_val46;
     plus_sarg16 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
     ret_val47 = STR_ap2004550586(plus_self19, plus_sarg16);
     plus_self20 = ret_val47;
     plus_arg3 = ')';
     str_self3 = plus_arg3;
     create_self11 = ((STR) NULL);
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

     L41 = ((STR) L43);
     L41->asize = L42;
     r3 = L41;
     SARR((STR)r3,0,create_c3); 
     ;
     ret_val50 = r3;
     ret_val49 = ret_val50;
     s3 = ret_val49;
     ret_val48 = STR_ap1077157958(plus_self20, s3, TRUE);
     err_s3 = ret_val48;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    fetch_self3 = match_self3;
    SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
    res = ((dAS_EXPR) c);
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER1464753752(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 AS_AND_EXPR a;
 AS_OR_EXPR o11;
 PARSER enter_self;
 STR enter_s;
 AS_AND_EXPR create_self;
 AS_AND_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 AS_OR_EXPR create_self1;
 AS_OR_EXPR ret_val3;
 PARSER source_loc_self1;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PARSER exit_self;
 STR s;
 extern STR expressionprec7;
 AS_AND_EXPR L1;
 OB L2;
 AS_OR_EXPR L3;
 OB L4;
 enter_self = self;
 enter_s = ((STR) &expressionprec7);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER682287135(self);
 while (1) {
  if (PARSER2128430730(self, PARSER_and_tok)) {
   create_self = ((AS_AND_EXPR) NULL);
   L2=ZALLOC(sizeof(struct AS_AND_EXPR_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=AS_AND_EXPR_tag;
   L1 = ((AS_AND_EXPR) L2);
   ret_val1 = L1;
   a = ret_val1;
   source_loc_self = self;
   ret_val2 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(a,source1,ret_val2);
   SATTR(a,e1,res);
   SATTR(a,e2,PARSER682287135(self));
   res = ((dAS_EXPR) a);
  }
  else {
   if (PARSER2128430730(self, PARSER_or_tok)) {
    create_self1 = ((AS_OR_EXPR) NULL);
    L4=ZALLOC(sizeof(struct AS_OR_EXPR_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    ((OB)L4)->header.tag=AS_OR_EXPR_tag;
    L3 = ((AS_OR_EXPR) L4);
    ret_val3 = L3;
    o11 = ret_val3;
    source_loc_self1 = self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(o11,source1,ret_val4);
    SATTR(o11,e1,res);
    SATTR(o11,e2,PARSER682287135(self));
    res = ((dAS_EXPR) o11);
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER1465639274(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 IDENT name111 = IDENT_zero;
 AS_CALL_EXPR c;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER exit_self;
 STR s;
 extern STR expressionprec5;
 AS_CALL_EXPR L1;
 OB L2;
 enter_self = self;
 enter_s = ((STR) &expressionprec5);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER681401613(self);
 while (1) {
  if (PARSER2128430730(self, PARSER_plus_tok)) {
   name111 = IDENT_2085547498;
  }
  else {
   if (PARSER2128430730(self, PARSER_minus_tok)) {
    name111 = IDENT_1129727818;
   }
   else {
    goto after_loop;
   }
  }
  create_self = ((AS_CALL_EXPR) NULL);
  L2=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=AS_CALL_EXPR_tag;
  L1 = ((AS_CALL_EXPR) L2);
  ret_val1 = L1;
  c = ret_val1;
  source_loc_self = self;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(c,source1,ret_val2);
  SATTR(c,name1,name111);
  SATTR(c,ob,res);
  SATTR(c,args,PARSER681401613(self));
  SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
  res = ((dAS_EXPR) c);
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER1466524796(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR x = ((dAS_EXPR) NULL);
 AS_CALL_EXPR c = ((AS_CALL_EXPR) NULL);
 dAS_EXPR res = ((dAS_EXPR) NULL);
 BOOL followed_by_lite = BOOL_zero;
 AS_INT_LIT_EXPR i;
 AS_FLT_LIT_EXPR f;
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER fetch_self;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 AS_INT_LIT_EXPR create_self;
 AS_INT_LIT_EXPR ret_val4;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 AS_FLT_LIT_EXPR create_self1;
 AS_FLT_LIT_EXPR ret_val6;
 PARSER source_loc_self1;
 SFILE_ID ret_val7 = SFILE_ID_zero;
 RAT negate_self = RAT_zero;
 RAT ret_val8 = RAT_zero;
 RAT r = RAT_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self2;
 SFILE_ID ret_val9 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self;
 STR plus_sarg;
 STR ret_val10;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val11;
 STR plus_self2;
 CHAR plus_arg = CHAR_zero;
 STR ret_val12;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val13;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val14;
 STR r1;
 PARSER err_self;
 STR err_s;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val15;
 PARSER source_loc_self3;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val17 = BOOL_zero;
 PARSER fetch_self1;
 AS_CALL_EXPR create_self4;
 AS_CALL_EXPR ret_val18;
 PARSER source_loc_self4;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PARSER exit_self;
 STR s1;
 extern STR expressionprec3;
 INT L1;
 BOOL L21_;
 BOOL L3;
 INT L4;
 BOOL L51_;
 INT L6;
 BOOL L71_;
 AS_INT_LIT_EXPR L8;
 OB L9;
 AS_FLT_LIT_EXPR L10;
 OB L11;
 RAT L12;
 RAT L13;
 extern STR Usepar158977156;
 extern STR in1;
 STR L14;
 INT L15;
 OB L16;
 INT L17;
 AS_CALL_EXPR L19;
 OB L20;
 INT L22;
 BOOL L231_;
 AS_CALL_EXPR L24;
 OB L25;
 enter_self = self;
 enter_s = ((STR) &expressionprec3);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER_minus_tok;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  fetch_self = self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER_lint_tok;
  L4 = is_eq_self1.val1;
  L51_=(L4)==(is_eq_y1); 
  ret_val2 = L51_;
  if (ret_val2) {
   L3 = TRUE;
  } else {
   is_eq_self2 = ATTR(self,next);
   is_eq_y2 = PARSER_lflt_tok;
   L6 = is_eq_self2.val1;
   L71_=(L6)==(is_eq_y2); 
   ret_val3 = L71_;
   L3 = ret_val3;
  }
  followed_by_lite = L3;
  x = PARSER1466524796(self);
  if (x==NULL) {
   goto other106;
  } else
  switch (TAG(x)) {
   case AS_INT_LIT_EXPR_tag:
    create_self = ((AS_INT_LIT_EXPR) NULL);
    L9=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=AS_INT_LIT_EXPR_tag;
    L8 = ((AS_INT_LIT_EXPR) L9);
    ret_val4 = L8;
    i = ret_val4;
    source_loc_self = self;
    ret_val5 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(i,source1,ret_val5);
    SATTR(i,val1,INTI_negaterINTI(ATTR(((AS_INT_LIT_EXPR) x),val1)));
    res = ((dAS_EXPR) i); break;
   case AS_FLT_LIT_EXPR_tag:
    create_self1 = ((AS_FLT_LIT_EXPR) NULL);
    L11=ZALLOC(sizeof(struct AS_FLT_LIT_EXPR_struct));
    if (L11==NULL) FATAL("Unable to allocate more memory");
    ((OB)L11)->header.tag=AS_FLT_LIT_EXPR_tag;
    L10 = ((AS_FLT_LIT_EXPR) L11);
    ret_val6 = L10;
    f = ret_val6;
    source_loc_self1 = self;
    ret_val7 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(f,source1,ret_val7);
    negate_self = ATTR(((AS_FLT_LIT_EXPR) x),val1);
    L12 = r;
    L12.u = INTI_negaterINTI(negate_self.u);
    L13 = L12;
    L13.v = negate_self.v;
    ret_val8 = L13;
    SATTR(f,val1,ret_val8);
    SATTR(f,tp,ATTR(((AS_FLT_LIT_EXPR) x),tp));
    res = ((dAS_EXPR) f); break;
   default: ;
   other106: ;
    if (followed_by_lite) {
     error_self = self;
     error_msg = ((STR) &Usepar158977156);
     set_eloc_self = ATTR(error_self,prog);
     source_loc_self2 = error_self;
     ret_val9 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l = ret_val9;
     SATTR(set_eloc_self,eloc,set_eloc_l);
     err_self = error_self;
     plus_self = error_msg;
     plus_sarg = ((STR) &in1);
     ret_val10 = STR_ap2004550586(plus_self, plus_sarg);
     plus_self1 = ret_val10;
     plus_sarg1 = FLISTSTR_toprSTR(ATTR(error_self,entered));
     ret_val11 = STR_ap2004550586(plus_self1, plus_sarg1);
     plus_self2 = ret_val11;
     plus_arg = ')';
     str_self = plus_arg;
     create_self2 = ((STR) NULL);
     create_c = str_self;
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
     SARR((STR)r1,0,create_c); 
     ;
     ret_val14 = r1;
     ret_val13 = ret_val14;
     s = ret_val13;
     ret_val12 = STR_ap1077157958(plus_self2, s, TRUE);
     err_s = ret_val12;
     PROG_err_STR(ATTR(err_self,prog), err_s);
    }
    create_self3 = ((AS_CALL_EXPR) NULL);
    L20=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L20==NULL) FATAL("Unable to allocate more memory");
    ((OB)L20)->header.tag=AS_CALL_EXPR_tag;
    L19 = ((AS_CALL_EXPR) L20);
    ret_val15 = L19;
    c = ret_val15;
    source_loc_self3 = self;
    ret_val16 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(c,source1,ret_val16);
    SATTR(c,name1,IDENT_2108657069);
    SATTR(c,ob,x);
    res = ((dAS_EXPR) c);
  }
 }
 else {
  is_eq_self3 = ATTR(self,next);
  is_eq_y3 = PARSER_not_tok;
  L22 = is_eq_self3.val1;
  L231_=(L22)==(is_eq_y3); 
  ret_val17 = L231_;
  if (ret_val17) {
   fetch_self1 = self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   x = PARSER1466524796(self);
   create_self4 = ((AS_CALL_EXPR) NULL);
   L25=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L25==NULL) FATAL("Unable to allocate more memory");
   ((OB)L25)->header.tag=AS_CALL_EXPR_tag;
   L24 = ((AS_CALL_EXPR) L25);
   ret_val18 = L24;
   c = ret_val18;
   source_loc_self4 = self;
   ret_val19 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(c,source1,ret_val19);
   SATTR(c,name1,IDENT_1837767448);
   SATTR(c,ob,x);
   res = ((dAS_EXPR) c);
  }
  else {
   res = PARSER680516091(self);
  }
 }
 exit_self = self;
 s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER403674127(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 AS_AT_EXPR h;
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 PARSER fetch_self;
 AS_AT_EXPR create_self;
 AS_AT_EXPR ret_val2;
 PARSER source_loc_self;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val4 = BOOL_zero;
 AS_ANY_EXPR create_self1;
 AS_ANY_EXPR ret_val5;
 PARSER fetch_self1;
 PARSER exit_self;
 STR s;
 extern STR expressionprec8;
 INT L1;
 BOOL L21_;
 AS_AT_EXPR L3;
 OB L4;
 INT L5;
 BOOL L61_;
 AS_ANY_EXPR L7;
 OB L8;
 enter_self = self;
 enter_s = ((STR) &expressionprec8);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER1464753752(self);
 while (1) {
  is_eq_self = ATTR(self,next);
  is_eq_y = PARSER_at_tok;
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
  create_self = ((AS_AT_EXPR) NULL);
  L4=ZALLOC(sizeof(struct AS_AT_EXPR_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=AS_AT_EXPR_tag;
  L3 = ((AS_AT_EXPR) L4);
  ret_val2 = L3;
  h = ret_val2;
  source_loc_self = self;
  ret_val3 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(h,source1,ret_val3);
  SATTR(h,e,res);
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER_any_tok;
  L5 = is_eq_self1.val1;
  L61_=(L5)==(is_eq_y1); 
  ret_val4 = L61_;
  if (ret_val4) {
   create_self1 = ((AS_ANY_EXPR) NULL);
   L8=ZALLOC(sizeof(struct AS_ANY_EXPR_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=AS_ANY_EXPR_tag;
   L7 = ((AS_ANY_EXPR) L8);
   ret_val5 = L7;
   SATTR(h,at,((dAS_EXPR) ret_val5));
   fetch_self1 = self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  }
  else {
   SATTR(h,at,PARSER1464753752(self));
  }
  res = ((dAS_EXPR) h);
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER679630569(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res = ((dAS_EXPR) NULL);
 INT L11 = INT_zero;
 AS_CALL_EXPR call_exp;
 AS_CALL_EXPR r;
 AS_CALL_EXPR r1;
 AS_CALL_EXPR r2;
 AS_IS_VOID_EXPR vtest;
 AS_NEW_EXPR new_ex;
 AS_ARRAY_EXPR arr_ex;
 AS_INITIAL_EXPR init_ex;
 AS_BOOL_LIT_EXPR r3;
 AS_BOOL_LIT_EXPR r4;
 AS_CHAR_LIT_EXPR c;
 AS_STR_LIT_EXPR s;
 AS_INT_LIT_EXPR i;
 AS_FLT_LIT_EXPR f;
 AS_CALL_EXPR a;
 AS_CALL_EXPR r5;
 AS_WHERE_EXPR r6;
 AS_NEAR_EXPR r7;
 AS_FAR_EXPR r8;
 PARSER enter_self;
 STR enter_s;
 PARSER fetch_self;
 AS_SELF_EXPR create_self;
 AS_SELF_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val3;
 PARSER source_loc_self1;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PARSER ident_self;
 IDENT ret_val5 = IDENT_zero;
 PARSER is_class_name_se;
 IDENT is_class_name_x = IDENT_zero;
 BOOL ret_val6 = BOOL_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val7 = BOOL_zero;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self2;
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
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val12;
 STR create_self2;
 CHAR create_c = CHAR_zero;
 STR ret_val13;
 STR r9;
 PARSER err_self;
 STR err_s;
 PARSER ident_self1;
 IDENT ret_val14 = IDENT_zero;
 PARSER fetch_self1;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val15;
 PARSER source_loc_self3;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 PARSER fetch_self2;
 AS_CALL_EXPR create_self4;
 AS_CALL_EXPR ret_val17;
 PARSER source_loc_self4;
 SFILE_ID ret_val18 = SFILE_ID_zero;
 PARSER fetch_self3;
 AS_CALL_EXPR create_self5;
 AS_CALL_EXPR ret_val19;
 PARSER source_loc_self5;
 SFILE_ID ret_val20 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val21 = BOOL_zero;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self6;
 SFILE_ID ret_val22 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val23;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val24;
 STR plus_self5;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val25;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val26;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val27;
 STR r10;
 PARSER err_self1;
 STR err_s1;
 PARSER enter_self1;
 STR enter_s1;
 PARSER fetch_self4;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val28 = BOOL_zero;
 PARSER fetch_self5;
 AS_IS_VOID_EXPR create_self7;
 AS_IS_VOID_EXPR ret_val29;
 PARSER source_loc_self7;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val31 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val32 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self6;
 STR plus_sarg4;
 STR ret_val33;
 STR plus_self7;
 STR plus_sarg5;
 STR ret_val34;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self8;
 SFILE_ID ret_val35 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val36;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val37;
 STR plus_self10;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val38;
 STR s3;
 CHAR str_self2 = CHAR_zero;
 STR ret_val39;
 STR create_self9;
 CHAR create_c2 = CHAR_zero;
 STR ret_val40;
 STR r11;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self6;
 AS_VOID_EXPR create_self10;
 AS_VOID_EXPR ret_val41;
 PARSER source_loc_self9;
 SFILE_ID ret_val42 = SFILE_ID_zero;
 PARSER exit_self;
 STR s4;
 PARSER enter_self2;
 STR enter_s2;
 PARSER fetch_self7;
 AS_NEW_EXPR create_self11;
 AS_NEW_EXPR ret_val43;
 PARSER source_loc_self10;
 SFILE_ID ret_val44 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val45 = BOOL_zero;
 TOKEN create_self12 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val46 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self11;
 STR plus_sarg8;
 STR ret_val47;
 STR plus_self12;
 STR plus_sarg9;
 STR ret_val48;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self11;
 SFILE_ID ret_val49 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val50;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val51;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val52;
 STR s5;
 CHAR str_self3 = CHAR_zero;
 STR ret_val53;
 STR create_self13;
 CHAR create_c3 = CHAR_zero;
 STR ret_val54;
 STR r12;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self8;
 PARSER exit_self1;
 STR s6;
 PARSER fetch_self9;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val55 = BOOL_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val56 = BOOL_zero;
 TOKEN is_eq_self7 = TOKEN_zero;
 INT is_eq_y7 = INT_zero;
 BOOL ret_val57 = BOOL_zero;
 PARSER enter_self3;
 STR enter_s3;
 PARSER fetch_self10;
 AS_ARRAY_EXPR create_self14;
 AS_ARRAY_EXPR ret_val58;
 PARSER source_loc_self12;
 SFILE_ID ret_val59 = SFILE_ID_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self8 = TOKEN_zero;
 INT is_eq_y8 = INT_zero;
 BOOL ret_val60 = BOOL_zero;
 TOKEN create_self15 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val61 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val62;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val63;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self13;
 SFILE_ID ret_val64 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val65;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val66;
 STR plus_self20;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val67;
 STR s7;
 CHAR str_self4 = CHAR_zero;
 STR ret_val68;
 STR create_self16;
 CHAR create_c4 = CHAR_zero;
 STR ret_val69;
 STR r13;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self11;
 PARSER exit_self2;
 STR s8;
 PARSER fetch_self12;
 AS_EXCEPT_EXPR create_self17;
 AS_EXCEPT_EXPR ret_val70;
 PARSER source_loc_self14;
 SFILE_ID ret_val71 = SFILE_ID_zero;
 PARSER enter_self4;
 STR enter_s4;
 PARSER fetch_self13;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self9 = TOKEN_zero;
 INT is_eq_y9 = INT_zero;
 BOOL ret_val72 = BOOL_zero;
 TOKEN create_self18 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val73 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self21;
 STR plus_sarg16;
 STR ret_val74;
 STR plus_self22;
 STR plus_sarg17;
 STR ret_val75;
 PARSER error_self5;
 STR error_msg5;
 PARSER source_loc_self15;
 SFILE_ID ret_val76 = SFILE_ID_zero;
 PROG set_eloc_self5;
 SFILE_ID set_eloc_l5 = SFILE_ID_zero;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val77;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val78;
 STR plus_self25;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val79;
 STR s9;
 CHAR str_self5 = CHAR_zero;
 STR ret_val80;
 STR create_self19;
 CHAR create_c5 = CHAR_zero;
 STR ret_val81;
 STR r14;
 PARSER err_self5;
 STR err_s5;
 PARSER fetch_self14;
 AS_INITIAL_EXPR create_self20;
 AS_INITIAL_EXPR ret_val82;
 PARSER source_loc_self16;
 SFILE_ID ret_val83 = SFILE_ID_zero;
 PARSER match_self4;
 INT match_t4 = INT_zero;
 TOKEN is_eq_self10 = TOKEN_zero;
 INT is_eq_y10 = INT_zero;
 BOOL ret_val84 = BOOL_zero;
 TOKEN create_self21 = TOKEN_zero;
 INT create_val4 = INT_zero;
 TOKEN ret_val85 = TOKEN_zero;
 TOKEN t4 = TOKEN_zero;
 PARSER exp_error_self4;
 STR exp_error_msg4;
 STR plus_self26;
 STR plus_sarg20;
 STR ret_val86;
 STR plus_self27;
 STR plus_sarg21;
 STR ret_val87;
 PARSER error_self6;
 STR error_msg6;
 PARSER source_loc_self17;
 SFILE_ID ret_val88 = SFILE_ID_zero;
 PROG set_eloc_self6;
 SFILE_ID set_eloc_l6 = SFILE_ID_zero;
 STR plus_self28;
 STR plus_sarg22;
 STR ret_val89;
 STR plus_self29;
 STR plus_sarg23;
 STR ret_val90;
 STR plus_self30;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val91;
 STR s10;
 CHAR str_self6 = CHAR_zero;
 STR ret_val92;
 STR create_self22;
 CHAR create_c6 = CHAR_zero;
 STR ret_val93;
 STR r15;
 PARSER err_self6;
 STR err_s6;
 PARSER fetch_self15;
 PARSER exit_self3;
 STR s11;
 PARSER fetch_self16;
 AS_RESULT_EXPR create_self23;
 AS_RESULT_EXPR ret_val94;
 PARSER source_loc_self18;
 SFILE_ID ret_val95 = SFILE_ID_zero;
 PARSER enter_self5;
 STR enter_s5;
 PARSER fetch_self17;
 PARSER match_self5;
 INT match_t5 = INT_zero;
 TOKEN is_eq_self11 = TOKEN_zero;
 INT is_eq_y11 = INT_zero;
 BOOL ret_val96 = BOOL_zero;
 TOKEN create_self24 = TOKEN_zero;
 INT create_val5 = INT_zero;
 TOKEN ret_val97 = TOKEN_zero;
 TOKEN t5 = TOKEN_zero;
 PARSER exp_error_self5;
 STR exp_error_msg5;
 STR plus_self31;
 STR plus_sarg24;
 STR ret_val98;
 STR plus_self32;
 STR plus_sarg25;
 STR ret_val99;
 PARSER error_self7;
 STR error_msg7;
 PARSER source_loc_self19;
 SFILE_ID ret_val100 = SFILE_ID_zero;
 PROG set_eloc_self7;
 SFILE_ID set_eloc_l7 = SFILE_ID_zero;
 STR plus_self33;
 STR plus_sarg26;
 STR ret_val101;
 STR plus_self34;
 STR plus_sarg27;
 STR ret_val102;
 STR plus_self35;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val103;
 STR s12;
 CHAR str_self7 = CHAR_zero;
 STR ret_val104;
 STR create_self25;
 CHAR create_c7 = CHAR_zero;
 STR ret_val105;
 STR r16;
 PARSER err_self7;
 STR err_s7;
 PARSER fetch_self18;
 PARSER match_self6;
 INT match_t6 = INT_zero;
 TOKEN is_eq_self12 = TOKEN_zero;
 INT is_eq_y12 = INT_zero;
 BOOL ret_val106 = BOOL_zero;
 TOKEN create_self26 = TOKEN_zero;
 INT create_val6 = INT_zero;
 TOKEN ret_val107 = TOKEN_zero;
 TOKEN t6 = TOKEN_zero;
 PARSER exp_error_self6;
 STR exp_error_msg6;
 STR plus_self36;
 STR plus_sarg28;
 STR ret_val108;
 STR plus_self37;
 STR plus_sarg29;
 STR ret_val109;
 PARSER error_self8;
 STR error_msg8;
 PARSER source_loc_self20;
 SFILE_ID ret_val110 = SFILE_ID_zero;
 PROG set_eloc_self8;
 SFILE_ID set_eloc_l8 = SFILE_ID_zero;
 STR plus_self38;
 STR plus_sarg30;
 STR ret_val1111;
 STR plus_self39;
 STR plus_sarg31;
 STR ret_val112;
 STR plus_self40;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val113;
 STR s13;
 CHAR str_self8 = CHAR_zero;
 STR ret_val114;
 STR create_self27;
 CHAR create_c8 = CHAR_zero;
 STR ret_val115;
 STR r17;
 PARSER err_self8;
 STR err_s8;
 PARSER fetch_self19;
 PARSER error_self9;
 STR error_msg9;
 PARSER source_loc_self21;
 SFILE_ID ret_val116 = SFILE_ID_zero;
 PROG set_eloc_self9;
 SFILE_ID set_eloc_l9 = SFILE_ID_zero;
 STR plus_self41;
 STR plus_sarg32;
 STR ret_val117;
 STR plus_self42;
 STR plus_sarg33;
 STR ret_val118;
 STR plus_self43;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val119;
 STR s14;
 CHAR str_self9 = CHAR_zero;
 STR ret_val120;
 STR create_self28;
 CHAR create_c9 = CHAR_zero;
 STR ret_val121;
 STR r18;
 PARSER err_self9;
 STR err_s9;
 PARSER exit_self4;
 STR s15;
 PARSER enter_self6;
 STR enter_s6;
 PARSER fetch_self20;
 PARSER match_self7;
 INT match_t7 = INT_zero;
 TOKEN is_eq_self13 = TOKEN_zero;
 INT is_eq_y13 = INT_zero;
 BOOL ret_val122 = BOOL_zero;
 TOKEN create_self29 = TOKEN_zero;
 INT create_val7 = INT_zero;
 TOKEN ret_val123 = TOKEN_zero;
 TOKEN t7 = TOKEN_zero;
 PARSER exp_error_self7;
 STR exp_error_msg7;
 STR plus_self44;
 STR plus_sarg34;
 STR ret_val124;
 STR plus_self45;
 STR plus_sarg35;
 STR ret_val125;
 PARSER error_self10;
 STR error_msg10;
 PARSER source_loc_self22;
 SFILE_ID ret_val126 = SFILE_ID_zero;
 PROG set_eloc_self10;
 SFILE_ID set_eloc_l10 = SFILE_ID_zero;
 STR plus_self46;
 STR plus_sarg36;
 STR ret_val127;
 STR plus_self47;
 STR plus_sarg37;
 STR ret_val128;
 STR plus_self48;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val129;
 STR s16;
 CHAR str_self10 = CHAR_zero;
 STR ret_val130;
 STR create_self30;
 CHAR create_c10 = CHAR_zero;
 STR ret_val131;
 STR r19;
 PARSER err_self10;
 STR err_s10;
 PARSER fetch_self21;
 PARSER match_self8;
 INT match_t8 = INT_zero;
 TOKEN is_eq_self14 = TOKEN_zero;
 INT is_eq_y14 = INT_zero;
 BOOL ret_val132 = BOOL_zero;
 TOKEN create_self31 = TOKEN_zero;
 INT create_val8 = INT_zero;
 TOKEN ret_val133 = TOKEN_zero;
 TOKEN t8 = TOKEN_zero;
 PARSER exp_error_self8;
 STR exp_error_msg8;
 STR plus_self49;
 STR plus_sarg38;
 STR ret_val134;
 STR plus_self50;
 STR plus_sarg39;
 STR ret_val135;
 PARSER error_self11;
 STR error_msg11;
 PARSER source_loc_self23;
 SFILE_ID ret_val136 = SFILE_ID_zero;
 PROG set_eloc_self11;
 SFILE_ID set_eloc_l11 = SFILE_ID_zero;
 STR plus_self51;
 STR plus_sarg40;
 STR ret_val137;
 STR plus_self52;
 STR plus_sarg41;
 STR ret_val138;
 STR plus_self53;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val139;
 STR s17;
 CHAR str_self11 = CHAR_zero;
 STR ret_val140;
 STR create_self32;
 CHAR create_c11 = CHAR_zero;
 STR ret_val141;
 STR r20;
 PARSER err_self11;
 STR err_s11;
 PARSER fetch_self22;
 PARSER error_self12;
 STR error_msg12;
 PARSER source_loc_self24;
 SFILE_ID ret_val142 = SFILE_ID_zero;
 PROG set_eloc_self12;
 SFILE_ID set_eloc_l12 = SFILE_ID_zero;
 STR plus_self54;
 STR plus_sarg42;
 STR ret_val143;
 STR plus_self55;
 STR plus_sarg43;
 STR ret_val144;
 STR plus_self56;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val145;
 STR s18;
 CHAR str_self12 = CHAR_zero;
 STR ret_val146;
 STR create_self33;
 CHAR create_c12 = CHAR_zero;
 STR ret_val147;
 STR r21;
 PARSER err_self12;
 STR err_s12;
 PARSER exit_self5;
 STR s19;
 PARSER fetch_self23;
 AS_BOOL_LIT_EXPR create_self34;
 AS_BOOL_LIT_EXPR ret_val148;
 PARSER source_loc_self25;
 SFILE_ID ret_val149 = SFILE_ID_zero;
 PARSER error_self13;
 STR error_msg13;
 PARSER source_loc_self26;
 SFILE_ID ret_val150 = SFILE_ID_zero;
 PROG set_eloc_self13;
 SFILE_ID set_eloc_l13 = SFILE_ID_zero;
 STR plus_self57;
 STR plus_sarg44;
 STR ret_val151;
 STR plus_self58;
 STR plus_sarg45;
 STR ret_val152;
 STR plus_self59;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val153;
 STR s20;
 CHAR str_self13 = CHAR_zero;
 STR ret_val154;
 STR create_self35;
 CHAR create_c13 = CHAR_zero;
 STR ret_val155;
 STR r22;
 PARSER err_self13;
 STR err_s13;
 AS_BOOL_LIT_EXPR create_self36;
 AS_BOOL_LIT_EXPR ret_val156;
 PARSER source_loc_self27;
 SFILE_ID ret_val157 = SFILE_ID_zero;
 TOKEN is_eq_self15 = TOKEN_zero;
 INT is_eq_y15 = INT_zero;
 BOOL ret_val158 = BOOL_zero;
 PARSER fetch_self24;
 AS_BOOL_LIT_EXPR create_self37;
 AS_BOOL_LIT_EXPR ret_val159;
 PARSER source_loc_self28;
 SFILE_ID ret_val160 = SFILE_ID_zero;
 TOKEN is_eq_self16 = TOKEN_zero;
 INT is_eq_y16 = INT_zero;
 BOOL ret_val161 = BOOL_zero;
 PARSER fetch_self25;
 AS_CHAR_LIT_EXPR create_self38;
 AS_CHAR_LIT_EXPR ret_val162;
 PARSER source_loc_self29;
 SFILE_ID ret_val163 = SFILE_ID_zero;
 PARSER fetch_self26;
 AS_STR_LIT_EXPR create_self39;
 AS_STR_LIT_EXPR ret_val164;
 PARSER source_loc_self30;
 SFILE_ID ret_val165 = SFILE_ID_zero;
 PARSER ident_self2;
 IDENT ret_val166 = IDENT_zero;
 PARSER fetch_self27;
 AS_INT_LIT_EXPR create_self40;
 AS_INT_LIT_EXPR ret_val167;
 PARSER source_loc_self31;
 SFILE_ID ret_val168 = SFILE_ID_zero;
 RAT floor_self = RAT_zero;
 INTI1 ret_val169;
 PARSER fetch_self28;
 AS_FLT_LIT_EXPR create_self41;
 AS_FLT_LIT_EXPR ret_val170;
 PARSER source_loc_self32;
 SFILE_ID ret_val171 = SFILE_ID_zero;
 PARSER fetch_self29;
 PARSER fetch_self30;
 PARSER match_self9;
 INT match_t9 = INT_zero;
 TOKEN is_eq_self17 = TOKEN_zero;
 INT is_eq_y17 = INT_zero;
 BOOL ret_val172 = BOOL_zero;
 TOKEN create_self42 = TOKEN_zero;
 INT create_val9 = INT_zero;
 TOKEN ret_val173 = TOKEN_zero;
 TOKEN t9 = TOKEN_zero;
 PARSER exp_error_self9;
 STR exp_error_msg9;
 STR plus_self60;
 STR plus_sarg46;
 STR ret_val174;
 STR plus_self61;
 STR plus_sarg47;
 STR ret_val175;
 PARSER error_self14;
 STR error_msg14;
 PARSER source_loc_self33;
 SFILE_ID ret_val176 = SFILE_ID_zero;
 PROG set_eloc_self14;
 SFILE_ID set_eloc_l14 = SFILE_ID_zero;
 STR plus_self62;
 STR plus_sarg48;
 STR ret_val177;
 STR plus_self63;
 STR plus_sarg49;
 STR ret_val178;
 STR plus_self64;
 CHAR plus_arg14 = CHAR_zero;
 STR ret_val179;
 STR s21;
 CHAR str_self14 = CHAR_zero;
 STR ret_val180;
 STR create_self43;
 CHAR create_c14 = CHAR_zero;
 STR ret_val181;
 STR r23;
 PARSER err_self14;
 STR err_s14;
 PARSER fetch_self31;
 PARSER fetch_self32;
 AS_CALL_EXPR create_self44;
 AS_CALL_EXPR ret_val182;
 PARSER source_loc_self34;
 SFILE_ID ret_val183 = SFILE_ID_zero;
 PARSER match_self10;
 INT match_t10 = INT_zero;
 TOKEN is_eq_self18 = TOKEN_zero;
 INT is_eq_y18 = INT_zero;
 BOOL ret_val184 = BOOL_zero;
 TOKEN create_self45 = TOKEN_zero;
 INT create_val10 = INT_zero;
 TOKEN ret_val185 = TOKEN_zero;
 TOKEN t10 = TOKEN_zero;
 PARSER exp_error_self10;
 STR exp_error_msg10;
 STR plus_self65;
 STR plus_sarg50;
 STR ret_val186;
 STR plus_self66;
 STR plus_sarg51;
 STR ret_val187;
 PARSER error_self15;
 STR error_msg15;
 PARSER source_loc_self35;
 SFILE_ID ret_val188 = SFILE_ID_zero;
 PROG set_eloc_self15;
 SFILE_ID set_eloc_l15 = SFILE_ID_zero;
 STR plus_self67;
 STR plus_sarg52;
 STR ret_val189;
 STR plus_self68;
 STR plus_sarg53;
 STR ret_val190;
 STR plus_self69;
 CHAR plus_arg15 = CHAR_zero;
 STR ret_val191;
 STR s22;
 CHAR str_self15 = CHAR_zero;
 STR ret_val192;
 STR create_self46;
 CHAR create_c15 = CHAR_zero;
 STR ret_val193;
 STR r24;
 PARSER err_self15;
 STR err_s15;
 PARSER fetch_self33;
 PARSER fetch_self34;
 AS_HERE_EXPR create_self47;
 AS_HERE_EXPR ret_val194;
 PARSER source_loc_self36;
 SFILE_ID ret_val195 = SFILE_ID_zero;
 PARSER fetch_self35;
 AS_GLOBAL_EXPR create_self48;
 AS_GLOBAL_EXPR ret_val196;
 PARSER source_loc_self37;
 SFILE_ID ret_val197 = SFILE_ID_zero;
 PARSER fetch_self36;
 AS_CLUSTER_EXPR create_self49;
 AS_CLUSTER_EXPR ret_val198;
 PARSER source_loc_self38;
 SFILE_ID ret_val199 = SFILE_ID_zero;
 PARSER fetch_self37;
 AS_CLU553570599 create_self50;
 AS_CLU553570599 ret_val200;
 PARSER source_loc_self39;
 SFILE_ID ret_val201 = SFILE_ID_zero;
 PARSER fetch_self38;
 AS_CALL_EXPR create_self51;
 AS_CALL_EXPR ret_val202;
 PARSER source_loc_self40;
 SFILE_ID ret_val203 = SFILE_ID_zero;
 AS_TYPE_SPEC create_self52;
 AS_TYPE_SPEC ret_val204;
 PARSER fetch_self39;
 AS_COHORT_EXPR create_self53;
 AS_COHORT_EXPR ret_val205;
 PARSER source_loc_self41;
 SFILE_ID ret_val206 = SFILE_ID_zero;
 PARSER enter_self7;
 STR enter_s7;
 AS_WHERE_EXPR create_self54;
 AS_WHERE_EXPR ret_val207;
 PARSER source_loc_self42;
 SFILE_ID ret_val208 = SFILE_ID_zero;
 PARSER fetch_self40;
 PARSER match_self11;
 INT match_t11 = INT_zero;
 TOKEN is_eq_self19 = TOKEN_zero;
 INT is_eq_y19 = INT_zero;
 BOOL ret_val209 = BOOL_zero;
 TOKEN create_self55 = TOKEN_zero;
 INT create_val11 = INT_zero;
 TOKEN ret_val210 = TOKEN_zero;
 TOKEN t11 = TOKEN_zero;
 PARSER exp_error_self11;
 STR exp_error_msg11;
 STR plus_self70;
 STR plus_sarg54;
 STR ret_val211;
 STR plus_self71;
 STR plus_sarg55;
 STR ret_val212;
 PARSER error_self16;
 STR error_msg16;
 PARSER source_loc_self43;
 SFILE_ID ret_val213 = SFILE_ID_zero;
 PROG set_eloc_self16;
 SFILE_ID set_eloc_l16 = SFILE_ID_zero;
 STR plus_self72;
 STR plus_sarg56;
 STR ret_val214;
 STR plus_self73;
 STR plus_sarg57;
 STR ret_val215;
 STR plus_self74;
 CHAR plus_arg16 = CHAR_zero;
 STR ret_val216;
 STR s23;
 CHAR str_self16 = CHAR_zero;
 STR ret_val217;
 STR create_self56;
 CHAR create_c16 = CHAR_zero;
 STR ret_val218;
 STR r25;
 PARSER err_self16;
 STR err_s16;
 PARSER fetch_self41;
 PARSER match_self12;
 INT match_t12 = INT_zero;
 TOKEN is_eq_self20 = TOKEN_zero;
 INT is_eq_y20 = INT_zero;
 BOOL ret_val219 = BOOL_zero;
 TOKEN create_self57 = TOKEN_zero;
 INT create_val12 = INT_zero;
 TOKEN ret_val220 = TOKEN_zero;
 TOKEN t12 = TOKEN_zero;
 PARSER exp_error_self12;
 STR exp_error_msg12;
 STR plus_self75;
 STR plus_sarg58;
 STR ret_val221;
 STR plus_self76;
 STR plus_sarg59;
 STR ret_val222;
 PARSER error_self17;
 STR error_msg17;
 PARSER source_loc_self44;
 SFILE_ID ret_val223 = SFILE_ID_zero;
 PROG set_eloc_self17;
 SFILE_ID set_eloc_l17 = SFILE_ID_zero;
 STR plus_self77;
 STR plus_sarg60;
 STR ret_val224;
 STR plus_self78;
 STR plus_sarg61;
 STR ret_val225;
 STR plus_self79;
 CHAR plus_arg17 = CHAR_zero;
 STR ret_val226;
 STR s24;
 CHAR str_self17 = CHAR_zero;
 STR ret_val227;
 STR create_self58;
 CHAR create_c17 = CHAR_zero;
 STR ret_val228;
 STR r26;
 PARSER err_self17;
 STR err_s17;
 PARSER fetch_self42;
 PARSER exit_self6;
 STR s25;
 PARSER enter_self8;
 STR enter_s8;
 AS_NEAR_EXPR create_self59;
 AS_NEAR_EXPR ret_val229;
 PARSER source_loc_self45;
 SFILE_ID ret_val230 = SFILE_ID_zero;
 PARSER fetch_self43;
 PARSER match_self13;
 INT match_t13 = INT_zero;
 TOKEN is_eq_self21 = TOKEN_zero;
 INT is_eq_y21 = INT_zero;
 BOOL ret_val231 = BOOL_zero;
 TOKEN create_self60 = TOKEN_zero;
 INT create_val13 = INT_zero;
 TOKEN ret_val232 = TOKEN_zero;
 TOKEN t13 = TOKEN_zero;
 PARSER exp_error_self13;
 STR exp_error_msg13;
 STR plus_self80;
 STR plus_sarg62;
 STR ret_val233;
 STR plus_self81;
 STR plus_sarg63;
 STR ret_val234;
 PARSER error_self18;
 STR error_msg18;
 PARSER source_loc_self46;
 SFILE_ID ret_val235 = SFILE_ID_zero;
 PROG set_eloc_self18;
 SFILE_ID set_eloc_l18 = SFILE_ID_zero;
 STR plus_self82;
 STR plus_sarg64;
 STR ret_val236;
 STR plus_self83;
 STR plus_sarg65;
 STR ret_val237;
 STR plus_self84;
 CHAR plus_arg18 = CHAR_zero;
 STR ret_val238;
 STR s26;
 CHAR str_self18 = CHAR_zero;
 STR ret_val239;
 STR create_self61;
 CHAR create_c18 = CHAR_zero;
 STR ret_val240;
 STR r27;
 PARSER err_self18;
 STR err_s18;
 PARSER fetch_self44;
 PARSER match_self14;
 INT match_t14 = INT_zero;
 TOKEN is_eq_self22 = TOKEN_zero;
 INT is_eq_y22 = INT_zero;
 BOOL ret_val241 = BOOL_zero;
 TOKEN create_self62 = TOKEN_zero;
 INT create_val14 = INT_zero;
 TOKEN ret_val242 = TOKEN_zero;
 TOKEN t14 = TOKEN_zero;
 PARSER exp_error_self14;
 STR exp_error_msg14;
 STR plus_self85;
 STR plus_sarg66;
 STR ret_val243;
 STR plus_self86;
 STR plus_sarg67;
 STR ret_val244;
 PARSER error_self19;
 STR error_msg19;
 PARSER source_loc_self47;
 SFILE_ID ret_val245 = SFILE_ID_zero;
 PROG set_eloc_self19;
 SFILE_ID set_eloc_l19 = SFILE_ID_zero;
 STR plus_self87;
 STR plus_sarg68;
 STR ret_val246;
 STR plus_self88;
 STR plus_sarg69;
 STR ret_val247;
 STR plus_self89;
 CHAR plus_arg19 = CHAR_zero;
 STR ret_val248;
 STR s27;
 CHAR str_self19 = CHAR_zero;
 STR ret_val249;
 STR create_self63;
 CHAR create_c19 = CHAR_zero;
 STR ret_val250;
 STR r28;
 PARSER err_self19;
 STR err_s19;
 PARSER fetch_self45;
 PARSER exit_self7;
 STR s28;
 PARSER enter_self9;
 STR enter_s9;
 AS_FAR_EXPR create_self64;
 AS_FAR_EXPR ret_val251;
 PARSER source_loc_self48;
 SFILE_ID ret_val252 = SFILE_ID_zero;
 PARSER fetch_self46;
 PARSER match_self15;
 INT match_t15 = INT_zero;
 TOKEN is_eq_self23 = TOKEN_zero;
 INT is_eq_y23 = INT_zero;
 BOOL ret_val253 = BOOL_zero;
 TOKEN create_self65 = TOKEN_zero;
 INT create_val15 = INT_zero;
 TOKEN ret_val254 = TOKEN_zero;
 TOKEN t15 = TOKEN_zero;
 PARSER exp_error_self15;
 STR exp_error_msg15;
 STR plus_self90;
 STR plus_sarg70;
 STR ret_val255;
 STR plus_self91;
 STR plus_sarg71;
 STR ret_val256;
 PARSER error_self20;
 STR error_msg20;
 PARSER source_loc_self49;
 SFILE_ID ret_val257 = SFILE_ID_zero;
 PROG set_eloc_self20;
 SFILE_ID set_eloc_l20 = SFILE_ID_zero;
 STR plus_self92;
 STR plus_sarg72;
 STR ret_val258;
 STR plus_self93;
 STR plus_sarg73;
 STR ret_val259;
 STR plus_self94;
 CHAR plus_arg20 = CHAR_zero;
 STR ret_val260;
 STR s29;
 CHAR str_self20 = CHAR_zero;
 STR ret_val261;
 STR create_self66;
 CHAR create_c20 = CHAR_zero;
 STR ret_val262;
 STR r29;
 PARSER err_self20;
 STR err_s20;
 PARSER fetch_self47;
 PARSER match_self16;
 INT match_t16 = INT_zero;
 TOKEN is_eq_self24 = TOKEN_zero;
 INT is_eq_y24 = INT_zero;
 BOOL ret_val263 = BOOL_zero;
 TOKEN create_self67 = TOKEN_zero;
 INT create_val16 = INT_zero;
 TOKEN ret_val264 = TOKEN_zero;
 TOKEN t16 = TOKEN_zero;
 PARSER exp_error_self16;
 STR exp_error_msg16;
 STR plus_self95;
 STR plus_sarg74;
 STR ret_val265;
 STR plus_self96;
 STR plus_sarg75;
 STR ret_val266;
 PARSER error_self21;
 STR error_msg21;
 PARSER source_loc_self50;
 SFILE_ID ret_val267 = SFILE_ID_zero;
 PROG set_eloc_self21;
 SFILE_ID set_eloc_l21 = SFILE_ID_zero;
 STR plus_self97;
 STR plus_sarg76;
 STR ret_val268;
 STR plus_self98;
 STR plus_sarg77;
 STR ret_val269;
 STR plus_self99;
 CHAR plus_arg21 = CHAR_zero;
 STR ret_val270;
 STR s30;
 CHAR str_self21 = CHAR_zero;
 STR ret_val271;
 STR create_self68;
 CHAR create_c21 = CHAR_zero;
 STR ret_val272;
 STR r30;
 PARSER err_self21;
 STR err_s21;
 PARSER fetch_self48;
 PARSER exit_self8;
 STR s31;
 PARSER exp_error_self17;
 STR exp_error_msg17;
 STR plus_self100;
 STR plus_sarg78;
 STR ret_val273;
 STR plus_self101;
 STR plus_sarg79;
 STR ret_val274;
 PARSER error_self22;
 STR error_msg22;
 PARSER source_loc_self51;
 SFILE_ID ret_val275 = SFILE_ID_zero;
 PROG set_eloc_self22;
 SFILE_ID set_eloc_l22 = SFILE_ID_zero;
 STR plus_self102;
 STR plus_sarg80;
 STR ret_val276;
 STR plus_self103;
 STR plus_sarg81;
 STR ret_val277;
 STR plus_self104;
 CHAR plus_arg22 = CHAR_zero;
 STR ret_val278;
 STR s32;
 CHAR str_self22 = CHAR_zero;
 STR ret_val279;
 STR create_self69;
 CHAR create_c22 = CHAR_zero;
 STR ret_val280;
 STR r31;
 PARSER err_self22;
 STR err_s22;
 AS_VOID_EXPR create_self70;
 AS_VOID_EXPR ret_val281;
 PARSER source_loc_self52;
 SFILE_ID ret_val282 = SFILE_ID_zero;
 PARSER exit_self9;
 STR s33;
 extern STR expressionprec0;
 AS_SELF_EXPR L2;
 OB L3;
 dAS_EXPR L4;
 AS_CALL_EXPR L5;
 OB L6;
 INT L7;
 INT L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR Thisty2067930150;
 extern STR in1;
 STR L15;
 INT L16;
 OB L17;
 INT L18;
 AS_CALL_EXPR L20;
 OB L21;
 AS_CALL_EXPR L22;
 OB L23;
 AS_CALL_EXPR L24;
 OB L25;
 INT L26;
 BOOL L271_;
 BOOL L28;
 BOOL L291_;
 extern STR SAMEne778844289;
 extern STR in1;
 STR L30;
 INT L31;
 OB L32;
 INT L33;
 extern STR voidexpressions;
 INT L35;
 BOOL L361_;
 AS_IS_VOID_EXPR L37;
 OB L38;
 INT L39;
 BOOL L401_;
 BOOL L41;
 BOOL L421_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L43;
 INT L44;
 OB L45;
 INT L46;
 AS_VOID_EXPR L48;
 OB L49;
 dAS_EXPR L50;
 extern STR newexpression;
 AS_NEW_EXPR L51;
 OB L52;
 INT L53;
 BOOL L541_;
 BOOL L55;
 BOOL L561_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L57;
 INT L58;
 OB L59;
 INT L60;
 BOOL L62;
 BOOL L63;
 INT L64;
 BOOL L651_;
 INT L66;
 BOOL L671_;
 INT L68;
 BOOL L691_;
 extern STR arrayexpression;
 AS_ARRAY_EXPR L70;
 OB L71;
 INT L72;
 BOOL L731_;
 BOOL L74;
 BOOL L751_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L76;
 INT L77;
 OB L78;
 INT L79;
 AS_EXCEPT_EXPR L81;
 OB L82;
 dAS_EXPR L83;
 extern STR initia522819456;
 INT L84;
 BOOL L851_;
 BOOL L86;
 BOOL L871_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L88;
 INT L89;
 OB L90;
 INT L92;
 AS_INITIAL_EXPR L94;
 OB L95;
 INT L96;
 BOOL L971_;
 BOOL L98;
 BOOL L991_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L100;
 INT L101;
 OB L102;
 INT L103;
 AS_RESULT_EXPR L105;
 OB L106;
 dAS_EXPR L107;
 extern STR whilebexpression;
 INT L108;
 BOOL L1091_;
 BOOL L110;
 BOOL L1111_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L112;
 INT L113;
 OB L114;
 INT L115;
 INT L117;
 BOOL L1181_;
 BOOL L119;
 BOOL L1201_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L122;
 INT L123;
 OB L124;
 INT L125;
 extern STR whileb1208041510;
 extern STR in1;
 STR L127;
 INT L128;
 OB L129;
 INT L130;
 extern STR untilbexpression;
 INT L132;
 BOOL L1331_;
 BOOL L134;
 BOOL L1351_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L136;
 INT L137;
 OB L138;
 INT L139;
 INT L142;
 BOOL L1431_;
 BOOL L144;
 BOOL L1451_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L146;
 INT L147;
 OB L148;
 INT L149;
 extern STR untilb1242183519;
 extern STR in1;
 STR L151;
 INT L152;
 OB L153;
 INT L154;
 AS_BOOL_LIT_EXPR L156;
 OB L157;
 dAS_EXPR L158;
 extern STR breakb781189520;
 extern STR in1;
 STR L159;
 INT L160;
 OB L161;
 INT L162;
 AS_BOOL_LIT_EXPR L164;
 OB L165;
 INT L166;
 BOOL L1671_;
 AS_BOOL_LIT_EXPR L168;
 OB L169;
 INT L170;
 BOOL L1711_;
 AS_CHAR_LIT_EXPR L172;
 OB L173;
 CHAR L174;
 INT L1751_;
 AS_STR_LIT_EXPR L176;
 OB L177;
 IDENT L178;
 AS_INT_LIT_EXPR L179;
 OB L180;
 INT L181;
 INT L182;
 BOOL L1831_;
 AS_FLT_LIT_EXPR L184;
 OB L185;
 INT L186;
 BOOL L1871_;
 BOOL L188;
 BOOL L1891_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L190;
 INT L192;
 OB L193;
 INT L194;
 AS_CALL_EXPR L196;
 OB L197;
 INT L198;
 BOOL L1991_;
 BOOL L200;
 BOOL L2011_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L202;
 INT L203;
 OB L204;
 INT L205;
 AS_HERE_EXPR L207;
 OB L208;
 dAS_EXPR L209;
 AS_GLOBAL_EXPR L210;
 OB L211;
 dAS_EXPR L212;
 AS_CLUSTER_EXPR L213;
 OB L214;
 dAS_EXPR L215;
 AS_CLU553570599 L216;
 OB L217;
 dAS_EXPR L218;
 AS_CALL_EXPR L219;
 OB L220;
 AS_TYPE_SPEC L221;
 OB L222;
 extern STR builti1654399671;
 AS_COHORT_EXPR L223;
 OB L224;
 dAS_EXPR L225;
 extern STR whereexpression;
 AS_WHERE_EXPR L226;
 OB L227;
 INT L228;
 BOOL L2291_;
 BOOL L230;
 BOOL L2311_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L232;
 INT L233;
 OB L234;
 INT L235;
 INT L237;
 BOOL L2381_;
 BOOL L239;
 BOOL L2401_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L241;
 INT L242;
 OB L243;
 INT L244;
 extern STR nearexpression;
 AS_NEAR_EXPR L246;
 OB L247;
 INT L248;
 BOOL L2491_;
 BOOL L250;
 BOOL L2511_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L252;
 INT L253;
 OB L254;
 INT L255;
 INT L257;
 BOOL L2581_;
 BOOL L259;
 BOOL L2601_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L261;
 INT L262;
 OB L263;
 INT L264;
 extern STR farexpression;
 AS_FAR_EXPR L266;
 OB L267;
 INT L268;
 BOOL L2691_;
 BOOL L270;
 BOOL L2721_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L273;
 INT L274;
 OB L275;
 INT L276;
 INT L278;
 BOOL L2791_;
 BOOL L280;
 BOOL L2811_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L282;
 INT L283;
 OB L284;
 INT L285;
 extern STR expression;
 extern STR expectedbutfound;
 extern STR in1;
 STR L287;
 INT L288;
 OB L289;
 INT L290;
 AS_VOID_EXPR L293;
 OB L294;
 dAS_EXPR L295;
 enter_self = self;
 enter_s = ((STR) &expressionprec0);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 L11 = ATTR(self,next).val1;
 switch (L11) {
  case 37: 
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
   create_self = ((AS_SELF_EXPR) NULL);
   L3=ZALLOC(sizeof(struct AS_SELF_EXPR_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=AS_SELF_EXPR_tag;
   L2 = ((AS_SELF_EXPR) L3);
   ret_val1 = L2;
   res = ((dAS_EXPR) ret_val1);
   source_loc_self = self;
   ret_val2 = SFILE_1594766561(SFILE_ID_zero);
   L4 = res;
   (*dAS_EX753042462[TAG(L4)])(L4, ret_val2);
   break;
  case 2: 
   create_self1 = ((AS_CALL_EXPR) NULL);
   L6=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AS_CALL_EXPR_tag;
   L5 = ((AS_CALL_EXPR) L6);
   ret_val3 = L5;
   call_exp = ret_val3;
   source_loc_self1 = self;
   ret_val4 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(call_exp,source1,ret_val4);
   is_class_name_se = self;
   ident_self = self;
   ret_val5 = ATTR(ATTR(ident_self,scanner),lex_value);
   is_class_name_x = ret_val5;
   ret_val6 = SCANNE363215919(ATTR(is_class_name_se,scanner), is_class_name_x.str);
   if (ret_val6) {
    SATTR(call_exp,tp,PARSER2028498823(self));
    L7 = ATTR(ATTR(call_exp,tp),kind);
    L8 = AS_TYPE_SPEC_ord;
    L91_=(L7)==(L8); 
    if (L91_) {
     if ((ATTR(ATTR(call_exp,tp),params)==((AS_TYPE_SPEC) NULL))) {
      SATTR(call_exp,name1,ATTR(ATTR(call_exp,tp),name1));
      SATTR(call_exp,tp,((AS_TYPE_SPEC) NULL));
     }
     else {
      is_eq_self = ATTR(self,next);
      is_eq_y = PARSER978635316;
      L10 = is_eq_self.val1;
      L121_=(L10)==(is_eq_y); 
      ret_val7 = L121_;
      L13 = ret_val7;
      L141_=!(L13); 
      if (L141_) {
       error_self = self;
       error_msg = ((STR) &Thisty2067930150);
       set_eloc_self = ATTR(error_self,prog);
       source_loc_self2 = error_self;
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
       create_self2 = ((STR) NULL);
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
       r9 = L15;
       SARR((STR)r9,0,create_c); 
       ;
       ret_val13 = r9;
       ret_val12 = ret_val13;
       s1 = ret_val12;
       ret_val11 = STR_ap1077157958(plus_self2, s1, TRUE);
       err_s = ret_val11;
       PROG_err_STR(ATTR(err_self,prog), err_s);
      }
     }
    }
   }
   else {
    ident_self1 = self;
    ret_val14 = ATTR(ATTR(ident_self1,scanner),lex_value);
    SATTR(call_exp,name1,ret_val14);
    fetch_self1 = self;
    SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   }
   res = ((dAS_EXPR) call_exp);
   break;
  case 75: 
   create_self3 = ((AS_CALL_EXPR) NULL);
   L21=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L21==NULL) FATAL("Unable to allocate more memory");
   ((OB)L21)->header.tag=AS_CALL_EXPR_tag;
   L20 = ((AS_CALL_EXPR) L21);
   ret_val15 = L20;
   r = ret_val15;
   source_loc_self3 = self;
   ret_val16 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r,source1,ret_val16);
   SATTR(r,name1,IDENT_604723504);
   fetch_self2 = self;
   SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
   res = ((dAS_EXPR) r);
   break;
  case 76: 
   create_self4 = ((AS_CALL_EXPR) NULL);
   L23=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=AS_CALL_EXPR_tag;
   L22 = ((AS_CALL_EXPR) L23);
   ret_val17 = L22;
   r1 = ret_val17;
   source_loc_self4 = self;
   ret_val18 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r1,source1,ret_val18);
   SATTR(r1,name1,IDENT_604723504);
   fetch_self3 = self;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   res = ((dAS_EXPR) r1);
   break;
  case 36: 
   create_self5 = ((AS_CALL_EXPR) NULL);
   L25=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L25==NULL) FATAL("Unable to allocate more memory");
   ((OB)L25)->header.tag=AS_CALL_EXPR_tag;
   L24 = ((AS_CALL_EXPR) L25);
   ret_val19 = L24;
   r2 = ret_val19;
   source_loc_self5 = self;
   ret_val20 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r2,source1,ret_val20);
   SATTR(r2,tp,PARSER2028498823(self));
   is_eq_self1 = ATTR(self,next);
   is_eq_y1 = PARSER978635316;
   L26 = is_eq_self1.val1;
   L271_=(L26)==(is_eq_y1); 
   ret_val21 = L271_;
   L28 = ret_val21;
   L291_=!(L28); 
   if (L291_) {
    error_self1 = self;
    error_msg1 = ((STR) &SAMEne778844289);
    set_eloc_self1 = ATTR(error_self1,prog);
    source_loc_self6 = error_self1;
    ret_val22 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l1 = ret_val22;
    SATTR(set_eloc_self1,eloc,set_eloc_l1);
    err_self1 = error_self1;
    plus_self3 = error_msg1;
    plus_sarg2 = ((STR) &in1);
    ret_val23 = STR_ap2004550586(plus_self3, plus_sarg2);
    plus_self4 = ret_val23;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
    ret_val24 = STR_ap2004550586(plus_self4, plus_sarg3);
    plus_self5 = ret_val24;
    plus_arg1 = ')';
    str_self1 = plus_arg1;
    create_self6 = ((STR) NULL);
    create_c1 = str_self1;
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
    r10 = L30;
    SARR((STR)r10,0,create_c1); 
    ;
    ret_val27 = r10;
    ret_val26 = ret_val27;
    s2 = ret_val26;
    ret_val25 = STR_ap1077157958(plus_self5, s2, TRUE);
    err_s1 = ret_val25;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   res = ((dAS_EXPR) r2);
   break;
  case 49: 
   enter_self1 = self;
   enter_s1 = ((STR) &voidexpressions);
   SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
   fetch_self4 = self;
   SATTR(fetch_self4,next,SCANNE1792387248(ATTR(fetch_self4,scanner)));
   is_eq_self2 = ATTR(self,next);
   is_eq_y2 = PARSER1209676627;
   L35 = is_eq_self2.val1;
   L361_=(L35)==(is_eq_y2); 
   ret_val28 = L361_;
   if (ret_val28) {
    fetch_self5 = self;
    SATTR(fetch_self5,next,SCANNE1792387248(ATTR(fetch_self5,scanner)));
    create_self7 = ((AS_IS_VOID_EXPR) NULL);
    L38=ZALLOC(sizeof(struct AS_IS_VOID_EXPR_struct));
    if (L38==NULL) FATAL("Unable to allocate more memory");
    ((OB)L38)->header.tag=AS_IS_VOID_EXPR_tag;
    L37 = ((AS_IS_VOID_EXPR) L38);
    ret_val29 = L37;
    vtest = ret_val29;
    source_loc_self7 = self;
    ret_val30 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(vtest,source1,ret_val30);
    SATTR(vtest,arg,PARSER403674127(self));
    res = ((dAS_EXPR) vtest);
    match_self = self;
    match_t = PARSER1207020061;
    is_eq_self3 = ATTR(match_self,next);
    is_eq_y3 = match_t;
    L39 = is_eq_self3.val1;
    L401_=(L39)==(is_eq_y3); 
    ret_val31 = L401_;
    L41 = ret_val31;
    L421_=!(L41); 
    if (L421_) {
     exp_error_self = match_self;
     create_self8 = TOKEN_zero;
     create_val = match_t;
     t.val1 = create_val;
     ret_val32 = t;
     exp_error_msg = TOKEN_strrSTR(ret_val32);
     error_self2 = exp_error_self;
     plus_self6 = exp_error_msg;
     plus_sarg4 = ((STR) &expectedbutfound);
     ret_val33 = STR_ap2004550586(plus_self6, plus_sarg4);
     plus_self7 = ret_val33;
     plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self,next));
     ret_val34 = STR_ap2004550586(plus_self7, plus_sarg5);
     error_msg2 = ret_val34;
     set_eloc_self2 = ATTR(error_self2,prog);
     source_loc_self8 = error_self2;
     ret_val35 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l2 = ret_val35;
     SATTR(set_eloc_self2,eloc,set_eloc_l2);
     err_self2 = error_self2;
     plus_self8 = error_msg2;
     plus_sarg6 = ((STR) &in1);
     ret_val36 = STR_ap2004550586(plus_self8, plus_sarg6);
     plus_self9 = ret_val36;
     plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
     ret_val37 = STR_ap2004550586(plus_self9, plus_sarg7);
     plus_self10 = ret_val37;
     plus_arg2 = ')';
     str_self2 = plus_arg2;
     create_self9 = ((STR) NULL);
     create_c2 = str_self2;
     L44 = 1;
     L46=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L44)*sizeof(CHAR);
     L45=ZALLOC_LEAF_BIG(L46);
     if (L45==NULL) FATAL("Unable to allocate more memory");
     memset(L45,0,L46);
     ((OB)L45)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L45)->header.destroyed=0;
#endif

     L43 = ((STR) L45);
     L43->asize = L44;
     r11 = L43;
     SARR((STR)r11,0,create_c2); 
     ;
     ret_val40 = r11;
     ret_val39 = ret_val40;
     s3 = ret_val39;
     ret_val38 = STR_ap1077157958(plus_self10, s3, TRUE);
     err_s2 = ret_val38;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    fetch_self6 = match_self;
    SATTR(fetch_self6,next,SCANNE1792387248(ATTR(fetch_self6,scanner)));
   }
   else {
    create_self10 = ((AS_VOID_EXPR) NULL);
    L49=ZALLOC(sizeof(struct AS_VOID_EXPR_struct));
    if (L49==NULL) FATAL("Unable to allocate more memory");
    ((OB)L49)->header.tag=AS_VOID_EXPR_tag;
    L48 = ((AS_VOID_EXPR) L49);
    ret_val41 = L48;
    res = ((dAS_EXPR) ret_val41);
    source_loc_self9 = self;
    ret_val42 = SFILE_1594766561(SFILE_ID_zero);
    L50 = res;
    (*dAS_EX753042462[TAG(L50)])(L50, ret_val42);
   }
   exit_self = self;
   s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
   break;
  case 23: 
   enter_self2 = self;
   enter_s2 = ((STR) &newexpression);
   SATTR(enter_self2,entered,FLISTS1425610062(ATTR(enter_self2,entered), enter_s2));
   fetch_self7 = self;
   SATTR(fetch_self7,next,SCANNE1792387248(ATTR(fetch_self7,scanner)));
   create_self11 = ((AS_NEW_EXPR) NULL);
   L52=ZALLOC(sizeof(struct AS_NEW_EXPR_struct));
   if (L52==NULL) FATAL("Unable to allocate more memory");
   ((OB)L52)->header.tag=AS_NEW_EXPR_tag;
   L51 = ((AS_NEW_EXPR) L52);
   ret_val43 = L51;
   new_ex = ret_val43;
   source_loc_self10 = self;
   ret_val44 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(new_ex,source1,ret_val44);
   res = ((dAS_EXPR) new_ex);
   if (PARSER2128430730(self, PARSER1209676627)) {
    SATTR(new_ex,arg,PARSER403674127(self));
    match_self1 = self;
    match_t1 = PARSER1207020061;
    is_eq_self4 = ATTR(match_self1,next);
    is_eq_y4 = match_t1;
    L53 = is_eq_self4.val1;
    L541_=(L53)==(is_eq_y4); 
    ret_val45 = L541_;
    L55 = ret_val45;
    L561_=!(L55); 
    if (L561_) {
     exp_error_self1 = match_self1;
     create_self12 = TOKEN_zero;
     create_val1 = match_t1;
     t1.val1 = create_val1;
     ret_val46 = t1;
     exp_error_msg1 = TOKEN_strrSTR(ret_val46);
     error_self3 = exp_error_self1;
     plus_self11 = exp_error_msg1;
     plus_sarg8 = ((STR) &expectedbutfound);
     ret_val47 = STR_ap2004550586(plus_self11, plus_sarg8);
     plus_self12 = ret_val47;
     plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
     ret_val48 = STR_ap2004550586(plus_self12, plus_sarg9);
     error_msg3 = ret_val48;
     set_eloc_self3 = ATTR(error_self3,prog);
     source_loc_self11 = error_self3;
     ret_val49 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l3 = ret_val49;
     SATTR(set_eloc_self3,eloc,set_eloc_l3);
     err_self3 = error_self3;
     plus_self13 = error_msg3;
     plus_sarg10 = ((STR) &in1);
     ret_val50 = STR_ap2004550586(plus_self13, plus_sarg10);
     plus_self14 = ret_val50;
     plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
     ret_val51 = STR_ap2004550586(plus_self14, plus_sarg11);
     plus_self15 = ret_val51;
     plus_arg3 = ')';
     str_self3 = plus_arg3;
     create_self13 = ((STR) NULL);
     create_c3 = str_self3;
     L58 = 1;
     L60=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L58)*sizeof(CHAR);
     L59=ZALLOC_LEAF_BIG(L60);
     if (L59==NULL) FATAL("Unable to allocate more memory");
     memset(L59,0,L60);
     ((OB)L59)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L59)->header.destroyed=0;
#endif

     L57 = ((STR) L59);
     L57->asize = L58;
     r12 = L57;
     SARR((STR)r12,0,create_c3); 
     ;
     ret_val54 = r12;
     ret_val53 = ret_val54;
     s5 = ret_val53;
     ret_val52 = STR_ap1077157958(plus_self15, s5, TRUE);
     err_s3 = ret_val52;
     PROG_err_STR(ATTR(err_self3,prog), err_s3);
    }
    fetch_self8 = match_self1;
    SATTR(fetch_self8,next,SCANNE1792387248(ATTR(fetch_self8,scanner)));
   }
   exit_self1 = self;
   s6 = FLISTSTR_poprSTR(ATTR(exit_self1,entered));
   break;
  case 74: 
   fetch_self9 = self;
   SATTR(fetch_self9,next,SCANNE1792387248(ATTR(fetch_self9,scanner)));
   is_eq_self5 = ATTR(self,next);
   is_eq_y5 = PARSER_ROUT_tok;
   L64 = is_eq_self5.val1;
   L651_=(L64)==(is_eq_y5); 
   ret_val55 = L651_;
   if (ret_val55) {
    L63 = TRUE;
   } else {
    is_eq_self6 = ATTR(self,next);
    is_eq_y6 = PARSER_ITER_tok;
    L66 = is_eq_self6.val1;
    L671_=(L66)==(is_eq_y6); 
    ret_val56 = L671_;
    L63 = ret_val56;
   }
   if (L63) {
    L62 = TRUE;
   } else {
    is_eq_self7 = ATTR(self,next);
    is_eq_y7 = PARSER1337387114;
    L68 = is_eq_self7.val1;
    L691_=(L68)==(is_eq_y7); 
    ret_val57 = L691_;
    L62 = ret_val57;
   }
   if (L62) {
    res = ((dAS_EXPR) PARSER1814172757(self));
   }
   else {
    res = ((dAS_EXPR) PARSER2102248401(self));
   }
   break;
  case 91: 
   res = ((dAS_EXPR) PARSER1814172757(self));
   break;
  case 79: 
   enter_self3 = self;
   enter_s3 = ((STR) &arrayexpression);
   SATTR(enter_self3,entered,FLISTS1425610062(ATTR(enter_self3,entered), enter_s3));
   fetch_self10 = self;
   SATTR(fetch_self10,next,SCANNE1792387248(ATTR(fetch_self10,scanner)));
   create_self14 = ((AS_ARRAY_EXPR) NULL);
   L71=ZALLOC(sizeof(struct AS_ARRAY_EXPR_struct));
   if (L71==NULL) FATAL("Unable to allocate more memory");
   ((OB)L71)->header.tag=AS_ARRAY_EXPR_tag;
   L70 = ((AS_ARRAY_EXPR) L71);
   ret_val58 = L70;
   arr_ex = ret_val58;
   source_loc_self12 = self;
   ret_val59 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(arr_ex,source1,ret_val59);
   res = ((dAS_EXPR) arr_ex);
   SATTR(arr_ex,elts,PARSER1134972014(self, FALSE));
   match_self2 = self;
   match_t2 = PARSER_vbar_tok;
   is_eq_self8 = ATTR(match_self2,next);
   is_eq_y8 = match_t2;
   L72 = is_eq_self8.val1;
   L731_=(L72)==(is_eq_y8); 
   ret_val60 = L731_;
   L74 = ret_val60;
   L751_=!(L74); 
   if (L751_) {
    exp_error_self2 = match_self2;
    create_self15 = TOKEN_zero;
    create_val2 = match_t2;
    t2.val1 = create_val2;
    ret_val61 = t2;
    exp_error_msg2 = TOKEN_strrSTR(ret_val61);
    error_self4 = exp_error_self2;
    plus_self16 = exp_error_msg2;
    plus_sarg12 = ((STR) &expectedbutfound);
    ret_val62 = STR_ap2004550586(plus_self16, plus_sarg12);
    plus_self17 = ret_val62;
    plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
    ret_val63 = STR_ap2004550586(plus_self17, plus_sarg13);
    error_msg4 = ret_val63;
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self13 = error_self4;
    ret_val64 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val64;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self18 = error_msg4;
    plus_sarg14 = ((STR) &in1);
    ret_val65 = STR_ap2004550586(plus_self18, plus_sarg14);
    plus_self19 = ret_val65;
    plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val66 = STR_ap2004550586(plus_self19, plus_sarg15);
    plus_self20 = ret_val66;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self16 = ((STR) NULL);
    create_c4 = str_self4;
    L77 = 1;
    L79=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L77)*sizeof(CHAR);
    L78=ZALLOC_LEAF_BIG(L79);
    if (L78==NULL) FATAL("Unable to allocate more memory");
    memset(L78,0,L79);
    ((OB)L78)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L78)->header.destroyed=0;
#endif

    L76 = ((STR) L78);
    L76->asize = L77;
    r13 = L76;
    SARR((STR)r13,0,create_c4); 
    ;
    ret_val69 = r13;
    ret_val68 = ret_val69;
    s7 = ret_val68;
    ret_val67 = STR_ap1077157958(plus_self20, s7, TRUE);
    err_s4 = ret_val67;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   fetch_self11 = match_self2;
   SATTR(fetch_self11,next,SCANNE1792387248(ATTR(fetch_self11,scanner)));
   exit_self2 = self;
   s8 = FLISTSTR_poprSTR(ATTR(exit_self2,entered));
   break;
  case 14: 
   fetch_self12 = self;
   SATTR(fetch_self12,next,SCANNE1792387248(ATTR(fetch_self12,scanner)));
   create_self17 = ((AS_EXCEPT_EXPR) NULL);
   L82=ZALLOC(sizeof(struct AS_EXCEPT_EXPR_struct));
   if (L82==NULL) FATAL("Unable to allocate more memory");
   ((OB)L82)->header.tag=AS_EXCEPT_EXPR_tag;
   L81 = ((AS_EXCEPT_EXPR) L82);
   ret_val70 = L81;
   res = ((dAS_EXPR) ret_val70);
   source_loc_self14 = self;
   ret_val71 = SFILE_1594766561(SFILE_ID_zero);
   L83 = res;
   (*dAS_EX753042462[TAG(L83)])(L83, ret_val71);
   break;
  case 19: 
   enter_self4 = self;
   enter_s4 = ((STR) &initia522819456);
   SATTR(enter_self4,entered,FLISTS1425610062(ATTR(enter_self4,entered), enter_s4));
   fetch_self13 = self;
   SATTR(fetch_self13,next,SCANNE1792387248(ATTR(fetch_self13,scanner)));
   match_self3 = self;
   match_t3 = PARSER1209676627;
   is_eq_self9 = ATTR(match_self3,next);
   is_eq_y9 = match_t3;
   L84 = is_eq_self9.val1;
   L851_=(L84)==(is_eq_y9); 
   ret_val72 = L851_;
   L86 = ret_val72;
   L871_=!(L86); 
   if (L871_) {
    exp_error_self3 = match_self3;
    create_self18 = TOKEN_zero;
    create_val3 = match_t3;
    t3.val1 = create_val3;
    ret_val73 = t3;
    exp_error_msg3 = TOKEN_strrSTR(ret_val73);
    error_self5 = exp_error_self3;
    plus_self21 = exp_error_msg3;
    plus_sarg16 = ((STR) &expectedbutfound);
    ret_val74 = STR_ap2004550586(plus_self21, plus_sarg16);
    plus_self22 = ret_val74;
    plus_sarg17 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
    ret_val75 = STR_ap2004550586(plus_self22, plus_sarg17);
    error_msg5 = ret_val75;
    set_eloc_self5 = ATTR(error_self5,prog);
    source_loc_self15 = error_self5;
    ret_val76 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l5 = ret_val76;
    SATTR(set_eloc_self5,eloc,set_eloc_l5);
    err_self5 = error_self5;
    plus_self23 = error_msg5;
    plus_sarg18 = ((STR) &in1);
    ret_val77 = STR_ap2004550586(plus_self23, plus_sarg18);
    plus_self24 = ret_val77;
    plus_sarg19 = FLISTSTR_toprSTR(ATTR(error_self5,entered));
    ret_val78 = STR_ap2004550586(plus_self24, plus_sarg19);
    plus_self25 = ret_val78;
    plus_arg5 = ')';
    str_self5 = plus_arg5;
    create_self19 = ((STR) NULL);
    create_c5 = str_self5;
    L89 = 1;
    L92=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L89)*sizeof(CHAR);
    L90=ZALLOC_LEAF_BIG(L92);
    if (L90==NULL) FATAL("Unable to allocate more memory");
    memset(L90,0,L92);
    ((OB)L90)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L90)->header.destroyed=0;
#endif

    L88 = ((STR) L90);
    L88->asize = L89;
    r14 = L88;
    SARR((STR)r14,0,create_c5); 
    ;
    ret_val81 = r14;
    ret_val80 = ret_val81;
    s9 = ret_val80;
    ret_val79 = STR_ap1077157958(plus_self25, s9, TRUE);
    err_s5 = ret_val79;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   fetch_self14 = match_self3;
   SATTR(fetch_self14,next,SCANNE1792387248(ATTR(fetch_self14,scanner)));
   create_self20 = ((AS_INITIAL_EXPR) NULL);
   L95=ZALLOC(sizeof(struct AS_INITIAL_EXPR_struct));
   if (L95==NULL) FATAL("Unable to allocate more memory");
   ((OB)L95)->header.tag=AS_INITIAL_EXPR_tag;
   L94 = ((AS_INITIAL_EXPR) L95);
   ret_val82 = L94;
   init_ex = ret_val82;
   source_loc_self16 = self;
   ret_val83 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(init_ex,source1,ret_val83);
   res = ((dAS_EXPR) init_ex);
   SATTR(init_ex,e,PARSER403674127(self));
   match_self4 = self;
   match_t4 = PARSER1207020061;
   is_eq_self10 = ATTR(match_self4,next);
   is_eq_y10 = match_t4;
   L96 = is_eq_self10.val1;
   L971_=(L96)==(is_eq_y10); 
   ret_val84 = L971_;
   L98 = ret_val84;
   L991_=!(L98); 
   if (L991_) {
    exp_error_self4 = match_self4;
    create_self21 = TOKEN_zero;
    create_val4 = match_t4;
    t4.val1 = create_val4;
    ret_val85 = t4;
    exp_error_msg4 = TOKEN_strrSTR(ret_val85);
    error_self6 = exp_error_self4;
    plus_self26 = exp_error_msg4;
    plus_sarg20 = ((STR) &expectedbutfound);
    ret_val86 = STR_ap2004550586(plus_self26, plus_sarg20);
    plus_self27 = ret_val86;
    plus_sarg21 = TOKEN_strrSTR(ATTR(exp_error_self4,next));
    ret_val87 = STR_ap2004550586(plus_self27, plus_sarg21);
    error_msg6 = ret_val87;
    set_eloc_self6 = ATTR(error_self6,prog);
    source_loc_self17 = error_self6;
    ret_val88 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l6 = ret_val88;
    SATTR(set_eloc_self6,eloc,set_eloc_l6);
    err_self6 = error_self6;
    plus_self28 = error_msg6;
    plus_sarg22 = ((STR) &in1);
    ret_val89 = STR_ap2004550586(plus_self28, plus_sarg22);
    plus_self29 = ret_val89;
    plus_sarg23 = FLISTSTR_toprSTR(ATTR(error_self6,entered));
    ret_val90 = STR_ap2004550586(plus_self29, plus_sarg23);
    plus_self30 = ret_val90;
    plus_arg6 = ')';
    str_self6 = plus_arg6;
    create_self22 = ((STR) NULL);
    create_c6 = str_self6;
    L101 = 1;
    L103=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L101)*sizeof(CHAR);
    L102=ZALLOC_LEAF_BIG(L103);
    if (L102==NULL) FATAL("Unable to allocate more memory");
    memset(L102,0,L103);
    ((OB)L102)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L102)->header.destroyed=0;
#endif

    L100 = ((STR) L102);
    L100->asize = L101;
    r15 = L100;
    SARR((STR)r15,0,create_c6); 
    ;
    ret_val93 = r15;
    ret_val92 = ret_val93;
    s10 = ret_val92;
    ret_val91 = STR_ap1077157958(plus_self30, s10, TRUE);
    err_s6 = ret_val91;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   fetch_self15 = match_self4;
   SATTR(fetch_self15,next,SCANNE1792387248(ATTR(fetch_self15,scanner)));
   exit_self3 = self;
   s11 = FLISTSTR_poprSTR(ATTR(exit_self3,entered));
   break;
  case 32: 
   fetch_self16 = self;
   SATTR(fetch_self16,next,SCANNE1792387248(ATTR(fetch_self16,scanner)));
   create_self23 = ((AS_RESULT_EXPR) NULL);
   L106=ZALLOC(sizeof(struct AS_RESULT_EXPR_struct));
   if (L106==NULL) FATAL("Unable to allocate more memory");
   ((OB)L106)->header.tag=AS_RESULT_EXPR_tag;
   L105 = ((AS_RESULT_EXPR) L106);
   ret_val94 = L105;
   res = ((dAS_EXPR) ret_val94);
   source_loc_self18 = self;
   ret_val95 = SFILE_1594766561(SFILE_ID_zero);
   L107 = res;
   (*dAS_EX753042462[TAG(L107)])(L107, ret_val95);
   break;
  case 51: 
   enter_self5 = self;
   enter_s5 = ((STR) &whilebexpression);
   SATTR(enter_self5,entered,FLISTS1425610062(ATTR(enter_self5,entered), enter_s5));
   fetch_self17 = self;
   SATTR(fetch_self17,next,SCANNE1792387248(ATTR(fetch_self17,scanner)));
   match_self5 = self;
   match_t5 = PARSER1209676627;
   is_eq_self11 = ATTR(match_self5,next);
   is_eq_y11 = match_t5;
   L108 = is_eq_self11.val1;
   L1091_=(L108)==(is_eq_y11); 
   ret_val96 = L1091_;
   L110 = ret_val96;
   L1111_=!(L110); 
   if (L1111_) {
    exp_error_self5 = match_self5;
    create_self24 = TOKEN_zero;
    create_val5 = match_t5;
    t5.val1 = create_val5;
    ret_val97 = t5;
    exp_error_msg5 = TOKEN_strrSTR(ret_val97);
    error_self7 = exp_error_self5;
    plus_self31 = exp_error_msg5;
    plus_sarg24 = ((STR) &expectedbutfound);
    ret_val98 = STR_ap2004550586(plus_self31, plus_sarg24);
    plus_self32 = ret_val98;
    plus_sarg25 = TOKEN_strrSTR(ATTR(exp_error_self5,next));
    ret_val99 = STR_ap2004550586(plus_self32, plus_sarg25);
    error_msg7 = ret_val99;
    set_eloc_self7 = ATTR(error_self7,prog);
    source_loc_self19 = error_self7;
    ret_val100 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l7 = ret_val100;
    SATTR(set_eloc_self7,eloc,set_eloc_l7);
    err_self7 = error_self7;
    plus_self33 = error_msg7;
    plus_sarg26 = ((STR) &in1);
    ret_val101 = STR_ap2004550586(plus_self33, plus_sarg26);
    plus_self34 = ret_val101;
    plus_sarg27 = FLISTSTR_toprSTR(ATTR(error_self7,entered));
    ret_val102 = STR_ap2004550586(plus_self34, plus_sarg27);
    plus_self35 = ret_val102;
    plus_arg7 = ')';
    str_self7 = plus_arg7;
    create_self25 = ((STR) NULL);
    create_c7 = str_self7;
    L113 = 1;
    L115=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L113)*sizeof(CHAR);
    L114=ZALLOC_LEAF_BIG(L115);
    if (L114==NULL) FATAL("Unable to allocate more memory");
    memset(L114,0,L115);
    ((OB)L114)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L114)->header.destroyed=0;
#endif

    L112 = ((STR) L114);
    L112->asize = L113;
    r16 = L112;
    SARR((STR)r16,0,create_c7); 
    ;
    ret_val105 = r16;
    ret_val104 = ret_val105;
    s12 = ret_val104;
    ret_val103 = STR_ap1077157958(plus_self35, s12, TRUE);
    err_s7 = ret_val103;
    PROG_err_STR(ATTR(err_self7,prog), err_s7);
   }
   fetch_self18 = match_self5;
   SATTR(fetch_self18,next,SCANNE1792387248(ATTR(fetch_self18,scanner)));
   res = PARSER403674127(self);
   match_self6 = self;
   match_t6 = PARSER1207020061;
   is_eq_self12 = ATTR(match_self6,next);
   is_eq_y12 = match_t6;
   L117 = is_eq_self12.val1;
   L1181_=(L117)==(is_eq_y12); 
   ret_val106 = L1181_;
   L119 = ret_val106;
   L1201_=!(L119); 
   if (L1201_) {
    exp_error_self6 = match_self6;
    create_self26 = TOKEN_zero;
    create_val6 = match_t6;
    t6.val1 = create_val6;
    ret_val107 = t6;
    exp_error_msg6 = TOKEN_strrSTR(ret_val107);
    error_self8 = exp_error_self6;
    plus_self36 = exp_error_msg6;
    plus_sarg28 = ((STR) &expectedbutfound);
    ret_val108 = STR_ap2004550586(plus_self36, plus_sarg28);
    plus_self37 = ret_val108;
    plus_sarg29 = TOKEN_strrSTR(ATTR(exp_error_self6,next));
    ret_val109 = STR_ap2004550586(plus_self37, plus_sarg29);
    error_msg8 = ret_val109;
    set_eloc_self8 = ATTR(error_self8,prog);
    source_loc_self20 = error_self8;
    ret_val110 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l8 = ret_val110;
    SATTR(set_eloc_self8,eloc,set_eloc_l8);
    err_self8 = error_self8;
    plus_self38 = error_msg8;
    plus_sarg30 = ((STR) &in1);
    ret_val1111 = STR_ap2004550586(plus_self38, plus_sarg30);
    plus_self39 = ret_val1111;
    plus_sarg31 = FLISTSTR_toprSTR(ATTR(error_self8,entered));
    ret_val112 = STR_ap2004550586(plus_self39, plus_sarg31);
    plus_self40 = ret_val112;
    plus_arg8 = ')';
    str_self8 = plus_arg8;
    create_self27 = ((STR) NULL);
    create_c8 = str_self8;
    L123 = 1;
    L125=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L123)*sizeof(CHAR);
    L124=ZALLOC_LEAF_BIG(L125);
    if (L124==NULL) FATAL("Unable to allocate more memory");
    memset(L124,0,L125);
    ((OB)L124)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L124)->header.destroyed=0;
#endif

    L122 = ((STR) L124);
    L122->asize = L123;
    r17 = L122;
    SARR((STR)r17,0,create_c8); 
    ;
    ret_val115 = r17;
    ret_val114 = ret_val115;
    s13 = ret_val114;
    ret_val113 = STR_ap1077157958(plus_self40, s13, TRUE);
    err_s8 = ret_val113;
    PROG_err_STR(ATTR(err_self8,prog), err_s8);
   }
   fetch_self19 = match_self6;
   SATTR(fetch_self19,next,SCANNE1792387248(ATTR(fetch_self19,scanner)));
   error_self9 = self;
   error_msg9 = ((STR) &whileb1208041510);
   set_eloc_self9 = ATTR(error_self9,prog);
   source_loc_self21 = error_self9;
   ret_val116 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l9 = ret_val116;
   SATTR(set_eloc_self9,eloc,set_eloc_l9);
   err_self9 = error_self9;
   plus_self41 = error_msg9;
   plus_sarg32 = ((STR) &in1);
   ret_val117 = STR_ap2004550586(plus_self41, plus_sarg32);
   plus_self42 = ret_val117;
   plus_sarg33 = FLISTSTR_toprSTR(ATTR(error_self9,entered));
   ret_val118 = STR_ap2004550586(plus_self42, plus_sarg33);
   plus_self43 = ret_val118;
   plus_arg9 = ')';
   str_self9 = plus_arg9;
   create_self28 = ((STR) NULL);
   create_c9 = str_self9;
   L128 = 1;
   L130=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L128)*sizeof(CHAR);
   L129=ZALLOC_LEAF_BIG(L130);
   if (L129==NULL) FATAL("Unable to allocate more memory");
   memset(L129,0,L130);
   ((OB)L129)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L129)->header.destroyed=0;
#endif

   L127 = ((STR) L129);
   L127->asize = L128;
   r18 = L127;
   SARR((STR)r18,0,create_c9); 
   ;
   ret_val121 = r18;
   ret_val120 = ret_val121;
   s14 = ret_val120;
   ret_val119 = STR_ap1077157958(plus_self43, s14, TRUE);
   err_s9 = ret_val119;
   PROG_err_STR(ATTR(err_self9,prog), err_s9);
   exit_self4 = self;
   s15 = FLISTSTR_poprSTR(ATTR(exit_self4,entered));
   break;
  case 43: 
   enter_self6 = self;
   enter_s6 = ((STR) &untilbexpression);
   SATTR(enter_self6,entered,FLISTS1425610062(ATTR(enter_self6,entered), enter_s6));
   fetch_self20 = self;
   SATTR(fetch_self20,next,SCANNE1792387248(ATTR(fetch_self20,scanner)));
   match_self7 = self;
   match_t7 = PARSER1209676627;
   is_eq_self13 = ATTR(match_self7,next);
   is_eq_y13 = match_t7;
   L132 = is_eq_self13.val1;
   L1331_=(L132)==(is_eq_y13); 
   ret_val122 = L1331_;
   L134 = ret_val122;
   L1351_=!(L134); 
   if (L1351_) {
    exp_error_self7 = match_self7;
    create_self29 = TOKEN_zero;
    create_val7 = match_t7;
    t7.val1 = create_val7;
    ret_val123 = t7;
    exp_error_msg7 = TOKEN_strrSTR(ret_val123);
    error_self10 = exp_error_self7;
    plus_self44 = exp_error_msg7;
    plus_sarg34 = ((STR) &expectedbutfound);
    ret_val124 = STR_ap2004550586(plus_self44, plus_sarg34);
    plus_self45 = ret_val124;
    plus_sarg35 = TOKEN_strrSTR(ATTR(exp_error_self7,next));
    ret_val125 = STR_ap2004550586(plus_self45, plus_sarg35);
    error_msg10 = ret_val125;
    set_eloc_self10 = ATTR(error_self10,prog);
    source_loc_self22 = error_self10;
    ret_val126 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l10 = ret_val126;
    SATTR(set_eloc_self10,eloc,set_eloc_l10);
    err_self10 = error_self10;
    plus_self46 = error_msg10;
    plus_sarg36 = ((STR) &in1);
    ret_val127 = STR_ap2004550586(plus_self46, plus_sarg36);
    plus_self47 = ret_val127;
    plus_sarg37 = FLISTSTR_toprSTR(ATTR(error_self10,entered));
    ret_val128 = STR_ap2004550586(plus_self47, plus_sarg37);
    plus_self48 = ret_val128;
    plus_arg10 = ')';
    str_self10 = plus_arg10;
    create_self30 = ((STR) NULL);
    create_c10 = str_self10;
    L137 = 1;
    L139=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L137)*sizeof(CHAR);
    L138=ZALLOC_LEAF_BIG(L139);
    if (L138==NULL) FATAL("Unable to allocate more memory");
    memset(L138,0,L139);
    ((OB)L138)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L138)->header.destroyed=0;
#endif

    L136 = ((STR) L138);
    L136->asize = L137;
    r19 = L136;
    SARR((STR)r19,0,create_c10); 
    ;
    ret_val131 = r19;
    ret_val130 = ret_val131;
    s16 = ret_val130;
    ret_val129 = STR_ap1077157958(plus_self48, s16, TRUE);
    err_s10 = ret_val129;
    PROG_err_STR(ATTR(err_self10,prog), err_s10);
   }
   fetch_self21 = match_self7;
   SATTR(fetch_self21,next,SCANNE1792387248(ATTR(fetch_self21,scanner)));
   res = PARSER403674127(self);
   match_self8 = self;
   match_t8 = PARSER1207020061;
   is_eq_self14 = ATTR(match_self8,next);
   is_eq_y14 = match_t8;
   L142 = is_eq_self14.val1;
   L1431_=(L142)==(is_eq_y14); 
   ret_val132 = L1431_;
   L144 = ret_val132;
   L1451_=!(L144); 
   if (L1451_) {
    exp_error_self8 = match_self8;
    create_self31 = TOKEN_zero;
    create_val8 = match_t8;
    t8.val1 = create_val8;
    ret_val133 = t8;
    exp_error_msg8 = TOKEN_strrSTR(ret_val133);
    error_self11 = exp_error_self8;
    plus_self49 = exp_error_msg8;
    plus_sarg38 = ((STR) &expectedbutfound);
    ret_val134 = STR_ap2004550586(plus_self49, plus_sarg38);
    plus_self50 = ret_val134;
    plus_sarg39 = TOKEN_strrSTR(ATTR(exp_error_self8,next));
    ret_val135 = STR_ap2004550586(plus_self50, plus_sarg39);
    error_msg11 = ret_val135;
    set_eloc_self11 = ATTR(error_self11,prog);
    source_loc_self23 = error_self11;
    ret_val136 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l11 = ret_val136;
    SATTR(set_eloc_self11,eloc,set_eloc_l11);
    err_self11 = error_self11;
    plus_self51 = error_msg11;
    plus_sarg40 = ((STR) &in1);
    ret_val137 = STR_ap2004550586(plus_self51, plus_sarg40);
    plus_self52 = ret_val137;
    plus_sarg41 = FLISTSTR_toprSTR(ATTR(error_self11,entered));
    ret_val138 = STR_ap2004550586(plus_self52, plus_sarg41);
    plus_self53 = ret_val138;
    plus_arg11 = ')';
    str_self11 = plus_arg11;
    create_self32 = ((STR) NULL);
    create_c11 = str_self11;
    L147 = 1;
    L149=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L147)*sizeof(CHAR);
    L148=ZALLOC_LEAF_BIG(L149);
    if (L148==NULL) FATAL("Unable to allocate more memory");
    memset(L148,0,L149);
    ((OB)L148)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L148)->header.destroyed=0;
#endif

    L146 = ((STR) L148);
    L146->asize = L147;
    r20 = L146;
    SARR((STR)r20,0,create_c11); 
    ;
    ret_val141 = r20;
    ret_val140 = ret_val141;
    s17 = ret_val140;
    ret_val139 = STR_ap1077157958(plus_self53, s17, TRUE);
    err_s11 = ret_val139;
    PROG_err_STR(ATTR(err_self11,prog), err_s11);
   }
   fetch_self22 = match_self8;
   SATTR(fetch_self22,next,SCANNE1792387248(ATTR(fetch_self22,scanner)));
   error_self12 = self;
   error_msg12 = ((STR) &untilb1242183519);
   set_eloc_self12 = ATTR(error_self12,prog);
   source_loc_self24 = error_self12;
   ret_val142 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l12 = ret_val142;
   SATTR(set_eloc_self12,eloc,set_eloc_l12);
   err_self12 = error_self12;
   plus_self54 = error_msg12;
   plus_sarg42 = ((STR) &in1);
   ret_val143 = STR_ap2004550586(plus_self54, plus_sarg42);
   plus_self55 = ret_val143;
   plus_sarg43 = FLISTSTR_toprSTR(ATTR(error_self12,entered));
   ret_val144 = STR_ap2004550586(plus_self55, plus_sarg43);
   plus_self56 = ret_val144;
   plus_arg12 = ')';
   str_self12 = plus_arg12;
   create_self33 = ((STR) NULL);
   create_c12 = str_self12;
   L152 = 1;
   L154=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L152)*sizeof(CHAR);
   L153=ZALLOC_LEAF_BIG(L154);
   if (L153==NULL) FATAL("Unable to allocate more memory");
   memset(L153,0,L154);
   ((OB)L153)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L153)->header.destroyed=0;
#endif

   L151 = ((STR) L153);
   L151->asize = L152;
   r21 = L151;
   SARR((STR)r21,0,create_c12); 
   ;
   ret_val147 = r21;
   ret_val146 = ret_val147;
   s18 = ret_val146;
   ret_val145 = STR_ap1077157958(plus_self56, s18, TRUE);
   err_s12 = ret_val145;
   PROG_err_STR(ATTR(err_self12,prog), err_s12);
   exit_self5 = self;
   s19 = FLISTSTR_poprSTR(ATTR(exit_self5,entered));
   break;
  case 7: 
   fetch_self23 = self;
   SATTR(fetch_self23,next,SCANNE1792387248(ATTR(fetch_self23,scanner)));
   create_self34 = ((AS_BOOL_LIT_EXPR) NULL);
   L157=ZALLOC(sizeof(struct AS_BOOL_LIT_EXPR_struct));
   if (L157==NULL) FATAL("Unable to allocate more memory");
   ((OB)L157)->header.tag=AS_BOOL_LIT_EXPR_tag;
   L156 = ((AS_BOOL_LIT_EXPR) L157);
   ret_val148 = L156;
   res = ((dAS_EXPR) ret_val148);
   source_loc_self25 = self;
   ret_val149 = SFILE_1594766561(SFILE_ID_zero);
   L158 = res;
   (*dAS_EX753042462[TAG(L158)])(L158, ret_val149);
   error_self13 = self;
   error_msg13 = ((STR) &breakb781189520);
   set_eloc_self13 = ATTR(error_self13,prog);
   source_loc_self26 = error_self13;
   ret_val150 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l13 = ret_val150;
   SATTR(set_eloc_self13,eloc,set_eloc_l13);
   err_self13 = error_self13;
   plus_self57 = error_msg13;
   plus_sarg44 = ((STR) &in1);
   ret_val151 = STR_ap2004550586(plus_self57, plus_sarg44);
   plus_self58 = ret_val151;
   plus_sarg45 = FLISTSTR_toprSTR(ATTR(error_self13,entered));
   ret_val152 = STR_ap2004550586(plus_self58, plus_sarg45);
   plus_self59 = ret_val152;
   plus_arg13 = ')';
   str_self13 = plus_arg13;
   create_self35 = ((STR) NULL);
   create_c13 = str_self13;
   L160 = 1;
   L162=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L160)*sizeof(CHAR);
   L161=ZALLOC_LEAF_BIG(L162);
   if (L161==NULL) FATAL("Unable to allocate more memory");
   memset(L161,0,L162);
   ((OB)L161)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L161)->header.destroyed=0;
#endif

   L159 = ((STR) L161);
   L159->asize = L160;
   r22 = L159;
   SARR((STR)r22,0,create_c13); 
   ;
   ret_val155 = r22;
   ret_val154 = ret_val155;
   s20 = ret_val154;
   ret_val153 = STR_ap1077157958(plus_self59, s20, TRUE);
   err_s13 = ret_val153;
   PROG_err_STR(ATTR(err_self13,prog), err_s13);
   break;
  case 40: 
   create_self36 = ((AS_BOOL_LIT_EXPR) NULL);
   L165=ZALLOC(sizeof(struct AS_BOOL_LIT_EXPR_struct));
   if (L165==NULL) FATAL("Unable to allocate more memory");
   ((OB)L165)->header.tag=AS_BOOL_LIT_EXPR_tag;
   L164 = ((AS_BOOL_LIT_EXPR) L165);
   ret_val156 = L164;
   r3 = ret_val156;
   source_loc_self27 = self;
   ret_val157 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r3,source1,ret_val157);
   is_eq_self15 = ATTR(self,next);
   is_eq_y15 = PARSER_true_tok;
   L166 = is_eq_self15.val1;
   L1671_=(L166)==(is_eq_y15); 
   ret_val158 = L1671_;
   SATTR(r3,val1,ret_val158);
   res = ((dAS_EXPR) r3);
   fetch_self24 = self;
   SATTR(fetch_self24,next,SCANNE1792387248(ATTR(fetch_self24,scanner)));
   break;
  case 16: 
   create_self37 = ((AS_BOOL_LIT_EXPR) NULL);
   L169=ZALLOC(sizeof(struct AS_BOOL_LIT_EXPR_struct));
   if (L169==NULL) FATAL("Unable to allocate more memory");
   ((OB)L169)->header.tag=AS_BOOL_LIT_EXPR_tag;
   L168 = ((AS_BOOL_LIT_EXPR) L169);
   ret_val159 = L168;
   r4 = ret_val159;
   source_loc_self28 = self;
   ret_val160 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r4,source1,ret_val160);
   is_eq_self16 = ATTR(self,next);
   is_eq_y16 = PARSER_true_tok;
   L170 = is_eq_self16.val1;
   L1711_=(L170)==(is_eq_y16); 
   ret_val161 = L1711_;
   SATTR(r4,val1,ret_val161);
   res = ((dAS_EXPR) r4);
   fetch_self25 = self;
   SATTR(fetch_self25,next,SCANNE1792387248(ATTR(fetch_self25,scanner)));
   break;
  case 56: 
   create_self38 = ((AS_CHAR_LIT_EXPR) NULL);
   L173=ZALLOC(sizeof(struct AS_CHAR_LIT_EXPR_struct));
   if (L173==NULL) FATAL("Unable to allocate more memory");
   ((OB)L173)->header.tag=AS_CHAR_LIT_EXPR_tag;
   L172 = ((AS_CHAR_LIT_EXPR) L173);
   ret_val162 = L172;
   c = ret_val162;
   source_loc_self29 = self;
   ret_val163 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(c,source1,ret_val163);
   L174 = ATTR(ATTR(self,scanner),char_value);
   L1751_=(INT)((unsigned char)L174); 
   SATTR(c,val1,L1751_);
   res = ((dAS_EXPR) c);
   fetch_self26 = self;
   SATTR(fetch_self26,next,SCANNE1792387248(ATTR(fetch_self26,scanner)));
   break;
  case 55: 
   create_self39 = ((AS_STR_LIT_EXPR) NULL);
   L177=ZALLOC(sizeof(struct AS_STR_LIT_EXPR_struct));
   if (L177==NULL) FATAL("Unable to allocate more memory");
   ((OB)L177)->header.tag=AS_STR_LIT_EXPR_tag;
   L176 = ((AS_STR_LIT_EXPR) L177);
   ret_val164 = L176;
   s = ret_val164;
   source_loc_self30 = self;
   ret_val165 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(s,source1,ret_val165);
   ident_self2 = self;
   ret_val166 = ATTR(ATTR(ident_self2,scanner),lex_value);
   L178=ret_val166;
   SATTR(s,s,L178.str);
   res = ((dAS_EXPR) s);
   fetch_self27 = self;
   SATTR(fetch_self27,next,SCANNE1792387248(ATTR(fetch_self27,scanner)));
   break;
  case 53: 
   create_self40 = ((AS_INT_LIT_EXPR) NULL);
   L180=ZALLOC(sizeof(struct AS_INT_LIT_EXPR_struct));
   if (L180==NULL) FATAL("Unable to allocate more memory");
   ((OB)L180)->header.tag=AS_INT_LIT_EXPR_tag;
   L179 = ((AS_INT_LIT_EXPR) L180);
   ret_val167 = L179;
   i = ret_val167;
   source_loc_self31 = self;
   ret_val168 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(i,source1,ret_val168);
   floor_self = ATTR(ATTR(self,scanner),num_value);
   ret_val169 = INTI_d359767808(floor_self.u, floor_self.v);
   SATTR(i,val1,ret_val169);
   L181 = ATTR(ATTR(self,scanner),value_type);
   L182 = AS_FLT388140455;
   L1831_=(L181)==(L182); 
   SATTR(i,is_inti,L1831_);
   res = ((dAS_EXPR) i);
   fetch_self28 = self;
   SATTR(fetch_self28,next,SCANNE1792387248(ATTR(fetch_self28,scanner)));
   break;
  case 54: 
   create_self41 = ((AS_FLT_LIT_EXPR) NULL);
   L185=ZALLOC(sizeof(struct AS_FLT_LIT_EXPR_struct));
   if (L185==NULL) FATAL("Unable to allocate more memory");
   ((OB)L185)->header.tag=AS_FLT_LIT_EXPR_tag;
   L184 = ((AS_FLT_LIT_EXPR) L185);
   ret_val170 = L184;
   f = ret_val170;
   source_loc_self32 = self;
   ret_val171 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(f,source1,ret_val171);
   SATTR(f,val1,ATTR(ATTR(self,scanner),num_value));
   SATTR(f,tp,ATTR(ATTR(self,scanner),value_type));
   res = ((dAS_EXPR) f);
   fetch_self29 = self;
   SATTR(fetch_self29,next,SCANNE1792387248(ATTR(fetch_self29,scanner)));
   break;
  case 57: 
   fetch_self30 = self;
   SATTR(fetch_self30,next,SCANNE1792387248(ATTR(fetch_self30,scanner)));
   res = PARSER403674127(self);
   match_self9 = self;
   match_t9 = PARSER1207020061;
   is_eq_self17 = ATTR(match_self9,next);
   is_eq_y17 = match_t9;
   L186 = is_eq_self17.val1;
   L1871_=(L186)==(is_eq_y17); 
   ret_val172 = L1871_;
   L188 = ret_val172;
   L1891_=!(L188); 
   if (L1891_) {
    exp_error_self9 = match_self9;
    create_self42 = TOKEN_zero;
    create_val9 = match_t9;
    t9.val1 = create_val9;
    ret_val173 = t9;
    exp_error_msg9 = TOKEN_strrSTR(ret_val173);
    error_self14 = exp_error_self9;
    plus_self60 = exp_error_msg9;
    plus_sarg46 = ((STR) &expectedbutfound);
    ret_val174 = STR_ap2004550586(plus_self60, plus_sarg46);
    plus_self61 = ret_val174;
    plus_sarg47 = TOKEN_strrSTR(ATTR(exp_error_self9,next));
    ret_val175 = STR_ap2004550586(plus_self61, plus_sarg47);
    error_msg14 = ret_val175;
    set_eloc_self14 = ATTR(error_self14,prog);
    source_loc_self33 = error_self14;
    ret_val176 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l14 = ret_val176;
    SATTR(set_eloc_self14,eloc,set_eloc_l14);
    err_self14 = error_self14;
    plus_self62 = error_msg14;
    plus_sarg48 = ((STR) &in1);
    ret_val177 = STR_ap2004550586(plus_self62, plus_sarg48);
    plus_self63 = ret_val177;
    plus_sarg49 = FLISTSTR_toprSTR(ATTR(error_self14,entered));
    ret_val178 = STR_ap2004550586(plus_self63, plus_sarg49);
    plus_self64 = ret_val178;
    plus_arg14 = ')';
    str_self14 = plus_arg14;
    create_self43 = ((STR) NULL);
    create_c14 = str_self14;
    L192 = 1;
    L194=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L192)*sizeof(CHAR);
    L193=ZALLOC_LEAF_BIG(L194);
    if (L193==NULL) FATAL("Unable to allocate more memory");
    memset(L193,0,L194);
    ((OB)L193)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L193)->header.destroyed=0;
#endif

    L190 = ((STR) L193);
    L190->asize = L192;
    r23 = L190;
    SARR((STR)r23,0,create_c14); 
    ;
    ret_val181 = r23;
    ret_val180 = ret_val181;
    s21 = ret_val180;
    ret_val179 = STR_ap1077157958(plus_self64, s21, TRUE);
    err_s14 = ret_val179;
    PROG_err_STR(ATTR(err_self14,prog), err_s14);
   }
   fetch_self31 = match_self9;
   SATTR(fetch_self31,next,SCANNE1792387248(ATTR(fetch_self31,scanner)));
   break;
  case 59: 
   fetch_self32 = self;
   SATTR(fetch_self32,next,SCANNE1792387248(ATTR(fetch_self32,scanner)));
   create_self44 = ((AS_CALL_EXPR) NULL);
   L197=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L197==NULL) FATAL("Unable to allocate more memory");
   ((OB)L197)->header.tag=AS_CALL_EXPR_tag;
   L196 = ((AS_CALL_EXPR) L197);
   ret_val182 = L196;
   a = ret_val182;
   source_loc_self34 = self;
   ret_val183 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(a,source1,ret_val183);
   SATTR(a,args,PARSER1134972014(self, FALSE));
   SATTR(a,is_array,TRUE);
   SATTR(a,modes1,PARSER87164415(self, a, AS_ARG1300937332));
   res = ((dAS_EXPR) a);
   match_self10 = self;
   match_t10 = PARSER1363330612;
   is_eq_self18 = ATTR(match_self10,next);
   is_eq_y18 = match_t10;
   L198 = is_eq_self18.val1;
   L1991_=(L198)==(is_eq_y18); 
   ret_val184 = L1991_;
   L200 = ret_val184;
   L2011_=!(L200); 
   if (L2011_) {
    exp_error_self10 = match_self10;
    create_self45 = TOKEN_zero;
    create_val10 = match_t10;
    t10.val1 = create_val10;
    ret_val185 = t10;
    exp_error_msg10 = TOKEN_strrSTR(ret_val185);
    error_self15 = exp_error_self10;
    plus_self65 = exp_error_msg10;
    plus_sarg50 = ((STR) &expectedbutfound);
    ret_val186 = STR_ap2004550586(plus_self65, plus_sarg50);
    plus_self66 = ret_val186;
    plus_sarg51 = TOKEN_strrSTR(ATTR(exp_error_self10,next));
    ret_val187 = STR_ap2004550586(plus_self66, plus_sarg51);
    error_msg15 = ret_val187;
    set_eloc_self15 = ATTR(error_self15,prog);
    source_loc_self35 = error_self15;
    ret_val188 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l15 = ret_val188;
    SATTR(set_eloc_self15,eloc,set_eloc_l15);
    err_self15 = error_self15;
    plus_self67 = error_msg15;
    plus_sarg52 = ((STR) &in1);
    ret_val189 = STR_ap2004550586(plus_self67, plus_sarg52);
    plus_self68 = ret_val189;
    plus_sarg53 = FLISTSTR_toprSTR(ATTR(error_self15,entered));
    ret_val190 = STR_ap2004550586(plus_self68, plus_sarg53);
    plus_self69 = ret_val190;
    plus_arg15 = ')';
    str_self15 = plus_arg15;
    create_self46 = ((STR) NULL);
    create_c15 = str_self15;
    L203 = 1;
    L205=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L203)*sizeof(CHAR);
    L204=ZALLOC_LEAF_BIG(L205);
    if (L204==NULL) FATAL("Unable to allocate more memory");
    memset(L204,0,L205);
    ((OB)L204)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L204)->header.destroyed=0;
#endif

    L202 = ((STR) L204);
    L202->asize = L203;
    r24 = L202;
    SARR((STR)r24,0,create_c15); 
    ;
    ret_val193 = r24;
    ret_val192 = ret_val193;
    s22 = ret_val192;
    ret_val191 = STR_ap1077157958(plus_self69, s22, TRUE);
    err_s15 = ret_val191;
    PROG_err_STR(ATTR(err_self15,prog), err_s15);
   }
   fetch_self33 = match_self10;
   SATTR(fetch_self33,next,SCANNE1792387248(ATTR(fetch_self33,scanner)));
   break;
  case 111: 
   fetch_self34 = self;
   SATTR(fetch_self34,next,SCANNE1792387248(ATTR(fetch_self34,scanner)));
   create_self47 = ((AS_HERE_EXPR) NULL);
   L208=ZALLOC(sizeof(struct AS_HERE_EXPR_struct));
   if (L208==NULL) FATAL("Unable to allocate more memory");
   ((OB)L208)->header.tag=AS_HERE_EXPR_tag;
   L207 = ((AS_HERE_EXPR) L208);
   ret_val194 = L207;
   res = ((dAS_EXPR) ret_val194);
   source_loc_self36 = self;
   ret_val195 = SFILE_1594766561(SFILE_ID_zero);
   L209 = res;
   (*dAS_EX753042462[TAG(L209)])(L209, ret_val195);
   break;
  case 113: 
   fetch_self35 = self;
   SATTR(fetch_self35,next,SCANNE1792387248(ATTR(fetch_self35,scanner)));
   create_self48 = ((AS_GLOBAL_EXPR) NULL);
   L211=ZALLOC(sizeof(struct AS_GLOBAL_EXPR_struct));
   if (L211==NULL) FATAL("Unable to allocate more memory");
   ((OB)L211)->header.tag=AS_GLOBAL_EXPR_tag;
   L210 = ((AS_GLOBAL_EXPR) L211);
   ret_val196 = L210;
   res = ((dAS_EXPR) ret_val196);
   source_loc_self37 = self;
   ret_val197 = SFILE_1594766561(SFILE_ID_zero);
   L212 = res;
   (*dAS_EX753042462[TAG(L212)])(L212, ret_val197);
   break;
  case 92: 
   fetch_self36 = self;
   SATTR(fetch_self36,next,SCANNE1792387248(ATTR(fetch_self36,scanner)));
   create_self49 = ((AS_CLUSTER_EXPR) NULL);
   L214=ZALLOC(sizeof(struct AS_CLUSTER_EXPR_struct));
   if (L214==NULL) FATAL("Unable to allocate more memory");
   ((OB)L214)->header.tag=AS_CLUSTER_EXPR_tag;
   L213 = ((AS_CLUSTER_EXPR) L214);
   ret_val198 = L213;
   res = ((dAS_EXPR) ret_val198);
   source_loc_self38 = self;
   ret_val199 = SFILE_1594766561(SFILE_ID_zero);
   L215 = res;
   (*dAS_EX753042462[TAG(L215)])(L215, ret_val199);
   break;
  case 93: 
   fetch_self37 = self;
   SATTR(fetch_self37,next,SCANNE1792387248(ATTR(fetch_self37,scanner)));
   create_self50 = ((AS_CLU553570599) NULL);
   L217=ZALLOC(sizeof(struct AS_CLU553570599_struct));
   if (L217==NULL) FATAL("Unable to allocate more memory");
   ((OB)L217)->header.tag=AS_CLU553570599_tag;
   L216 = ((AS_CLU553570599) L217);
   ret_val200 = L216;
   res = ((dAS_EXPR) ret_val200);
   source_loc_self39 = self;
   ret_val201 = SFILE_1594766561(SFILE_ID_zero);
   L218 = res;
   (*dAS_EX753042462[TAG(L218)])(L218, ret_val201);
   break;
  case 109: 
   fetch_self38 = self;
   SATTR(fetch_self38,next,SCANNE1792387248(ATTR(fetch_self38,scanner)));
   create_self51 = ((AS_CALL_EXPR) NULL);
   L220=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L220==NULL) FATAL("Unable to allocate more memory");
   ((OB)L220)->header.tag=AS_CALL_EXPR_tag;
   L219 = ((AS_CALL_EXPR) L220);
   ret_val202 = L219;
   r5 = ret_val202;
   source_loc_self40 = self;
   ret_val203 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r5,source1,ret_val203);
   create_self52 = ((AS_TYPE_SPEC) NULL);
   L222=ZALLOC(sizeof(struct AS_TYPE_SPEC_struct));
   if (L222==NULL) FATAL("Unable to allocate more memory");
   ((OB)L222)->header.tag=AS_TYPE_SPEC_tag;
   L221 = ((AS_TYPE_SPEC) L222);
   ret_val204 = L221;
   SATTR(r5,tp,ret_val204);
   SATTR(ATTR(r5,tp),name1,IDENT_673807638);
   SATTR(r5,name1,IDENT_1150413730(IDENT_zero, ((STR) &builti1654399671)));
   res = ((dAS_EXPR) r5);
   break;
  case 101: 
   fetch_self39 = self;
   SATTR(fetch_self39,next,SCANNE1792387248(ATTR(fetch_self39,scanner)));
   create_self53 = ((AS_COHORT_EXPR) NULL);
   L224=ZALLOC(sizeof(struct AS_COHORT_EXPR_struct));
   if (L224==NULL) FATAL("Unable to allocate more memory");
   ((OB)L224)->header.tag=AS_COHORT_EXPR_tag;
   L223 = ((AS_COHORT_EXPR) L224);
   ret_val205 = L223;
   res = ((dAS_EXPR) ret_val205);
   source_loc_self41 = self;
   ret_val206 = SFILE_1594766561(SFILE_ID_zero);
   L225 = res;
   (*dAS_EX753042462[TAG(L225)])(L225, ret_val206);
   break;
  case 112: 
   enter_self7 = self;
   enter_s7 = ((STR) &whereexpression);
   SATTR(enter_self7,entered,FLISTS1425610062(ATTR(enter_self7,entered), enter_s7));
   create_self54 = ((AS_WHERE_EXPR) NULL);
   L227=ZALLOC(sizeof(struct AS_WHERE_EXPR_struct));
   if (L227==NULL) FATAL("Unable to allocate more memory");
   ((OB)L227)->header.tag=AS_WHERE_EXPR_tag;
   L226 = ((AS_WHERE_EXPR) L227);
   ret_val207 = L226;
   r6 = ret_val207;
   source_loc_self42 = self;
   ret_val208 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r6,source1,ret_val208);
   res = ((dAS_EXPR) r6);
   fetch_self40 = self;
   SATTR(fetch_self40,next,SCANNE1792387248(ATTR(fetch_self40,scanner)));
   match_self11 = self;
   match_t11 = PARSER1209676627;
   is_eq_self19 = ATTR(match_self11,next);
   is_eq_y19 = match_t11;
   L228 = is_eq_self19.val1;
   L2291_=(L228)==(is_eq_y19); 
   ret_val209 = L2291_;
   L230 = ret_val209;
   L2311_=!(L230); 
   if (L2311_) {
    exp_error_self11 = match_self11;
    create_self55 = TOKEN_zero;
    create_val11 = match_t11;
    t11.val1 = create_val11;
    ret_val210 = t11;
    exp_error_msg11 = TOKEN_strrSTR(ret_val210);
    error_self16 = exp_error_self11;
    plus_self70 = exp_error_msg11;
    plus_sarg54 = ((STR) &expectedbutfound);
    ret_val211 = STR_ap2004550586(plus_self70, plus_sarg54);
    plus_self71 = ret_val211;
    plus_sarg55 = TOKEN_strrSTR(ATTR(exp_error_self11,next));
    ret_val212 = STR_ap2004550586(plus_self71, plus_sarg55);
    error_msg16 = ret_val212;
    set_eloc_self16 = ATTR(error_self16,prog);
    source_loc_self43 = error_self16;
    ret_val213 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l16 = ret_val213;
    SATTR(set_eloc_self16,eloc,set_eloc_l16);
    err_self16 = error_self16;
    plus_self72 = error_msg16;
    plus_sarg56 = ((STR) &in1);
    ret_val214 = STR_ap2004550586(plus_self72, plus_sarg56);
    plus_self73 = ret_val214;
    plus_sarg57 = FLISTSTR_toprSTR(ATTR(error_self16,entered));
    ret_val215 = STR_ap2004550586(plus_self73, plus_sarg57);
    plus_self74 = ret_val215;
    plus_arg16 = ')';
    str_self16 = plus_arg16;
    create_self56 = ((STR) NULL);
    create_c16 = str_self16;
    L233 = 1;
    L235=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L233)*sizeof(CHAR);
    L234=ZALLOC_LEAF_BIG(L235);
    if (L234==NULL) FATAL("Unable to allocate more memory");
    memset(L234,0,L235);
    ((OB)L234)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L234)->header.destroyed=0;
#endif

    L232 = ((STR) L234);
    L232->asize = L233;
    r25 = L232;
    SARR((STR)r25,0,create_c16); 
    ;
    ret_val218 = r25;
    ret_val217 = ret_val218;
    s23 = ret_val217;
    ret_val216 = STR_ap1077157958(plus_self74, s23, TRUE);
    err_s16 = ret_val216;
    PROG_err_STR(ATTR(err_self16,prog), err_s16);
   }
   fetch_self41 = match_self11;
   SATTR(fetch_self41,next,SCANNE1792387248(ATTR(fetch_self41,scanner)));
   SATTR(r6,e,PARSER403674127(self));
   match_self12 = self;
   match_t12 = PARSER1207020061;
   is_eq_self20 = ATTR(match_self12,next);
   is_eq_y20 = match_t12;
   L237 = is_eq_self20.val1;
   L2381_=(L237)==(is_eq_y20); 
   ret_val219 = L2381_;
   L239 = ret_val219;
   L2401_=!(L239); 
   if (L2401_) {
    exp_error_self12 = match_self12;
    create_self57 = TOKEN_zero;
    create_val12 = match_t12;
    t12.val1 = create_val12;
    ret_val220 = t12;
    exp_error_msg12 = TOKEN_strrSTR(ret_val220);
    error_self17 = exp_error_self12;
    plus_self75 = exp_error_msg12;
    plus_sarg58 = ((STR) &expectedbutfound);
    ret_val221 = STR_ap2004550586(plus_self75, plus_sarg58);
    plus_self76 = ret_val221;
    plus_sarg59 = TOKEN_strrSTR(ATTR(exp_error_self12,next));
    ret_val222 = STR_ap2004550586(plus_self76, plus_sarg59);
    error_msg17 = ret_val222;
    set_eloc_self17 = ATTR(error_self17,prog);
    source_loc_self44 = error_self17;
    ret_val223 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l17 = ret_val223;
    SATTR(set_eloc_self17,eloc,set_eloc_l17);
    err_self17 = error_self17;
    plus_self77 = error_msg17;
    plus_sarg60 = ((STR) &in1);
    ret_val224 = STR_ap2004550586(plus_self77, plus_sarg60);
    plus_self78 = ret_val224;
    plus_sarg61 = FLISTSTR_toprSTR(ATTR(error_self17,entered));
    ret_val225 = STR_ap2004550586(plus_self78, plus_sarg61);
    plus_self79 = ret_val225;
    plus_arg17 = ')';
    str_self17 = plus_arg17;
    create_self58 = ((STR) NULL);
    create_c17 = str_self17;
    L242 = 1;
    L244=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L242)*sizeof(CHAR);
    L243=ZALLOC_LEAF_BIG(L244);
    if (L243==NULL) FATAL("Unable to allocate more memory");
    memset(L243,0,L244);
    ((OB)L243)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L243)->header.destroyed=0;
#endif

    L241 = ((STR) L243);
    L241->asize = L242;
    r26 = L241;
    SARR((STR)r26,0,create_c17); 
    ;
    ret_val228 = r26;
    ret_val227 = ret_val228;
    s24 = ret_val227;
    ret_val226 = STR_ap1077157958(plus_self79, s24, TRUE);
    err_s17 = ret_val226;
    PROG_err_STR(ATTR(err_self17,prog), err_s17);
   }
   fetch_self42 = match_self12;
   SATTR(fetch_self42,next,SCANNE1792387248(ATTR(fetch_self42,scanner)));
   exit_self6 = self;
   s25 = FLISTSTR_poprSTR(ATTR(exit_self6,entered));
   break;
  case 105: 
   enter_self8 = self;
   enter_s8 = ((STR) &nearexpression);
   SATTR(enter_self8,entered,FLISTS1425610062(ATTR(enter_self8,entered), enter_s8));
   create_self59 = ((AS_NEAR_EXPR) NULL);
   L247=ZALLOC(sizeof(struct AS_NEAR_EXPR_struct));
   if (L247==NULL) FATAL("Unable to allocate more memory");
   ((OB)L247)->header.tag=AS_NEAR_EXPR_tag;
   L246 = ((AS_NEAR_EXPR) L247);
   ret_val229 = L246;
   r7 = ret_val229;
   source_loc_self45 = self;
   ret_val230 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r7,source1,ret_val230);
   res = ((dAS_EXPR) r7);
   fetch_self43 = self;
   SATTR(fetch_self43,next,SCANNE1792387248(ATTR(fetch_self43,scanner)));
   match_self13 = self;
   match_t13 = PARSER1209676627;
   is_eq_self21 = ATTR(match_self13,next);
   is_eq_y21 = match_t13;
   L248 = is_eq_self21.val1;
   L2491_=(L248)==(is_eq_y21); 
   ret_val231 = L2491_;
   L250 = ret_val231;
   L2511_=!(L250); 
   if (L2511_) {
    exp_error_self13 = match_self13;
    create_self60 = TOKEN_zero;
    create_val13 = match_t13;
    t13.val1 = create_val13;
    ret_val232 = t13;
    exp_error_msg13 = TOKEN_strrSTR(ret_val232);
    error_self18 = exp_error_self13;
    plus_self80 = exp_error_msg13;
    plus_sarg62 = ((STR) &expectedbutfound);
    ret_val233 = STR_ap2004550586(plus_self80, plus_sarg62);
    plus_self81 = ret_val233;
    plus_sarg63 = TOKEN_strrSTR(ATTR(exp_error_self13,next));
    ret_val234 = STR_ap2004550586(plus_self81, plus_sarg63);
    error_msg18 = ret_val234;
    set_eloc_self18 = ATTR(error_self18,prog);
    source_loc_self46 = error_self18;
    ret_val235 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l18 = ret_val235;
    SATTR(set_eloc_self18,eloc,set_eloc_l18);
    err_self18 = error_self18;
    plus_self82 = error_msg18;
    plus_sarg64 = ((STR) &in1);
    ret_val236 = STR_ap2004550586(plus_self82, plus_sarg64);
    plus_self83 = ret_val236;
    plus_sarg65 = FLISTSTR_toprSTR(ATTR(error_self18,entered));
    ret_val237 = STR_ap2004550586(plus_self83, plus_sarg65);
    plus_self84 = ret_val237;
    plus_arg18 = ')';
    str_self18 = plus_arg18;
    create_self61 = ((STR) NULL);
    create_c18 = str_self18;
    L253 = 1;
    L255=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L253)*sizeof(CHAR);
    L254=ZALLOC_LEAF_BIG(L255);
    if (L254==NULL) FATAL("Unable to allocate more memory");
    memset(L254,0,L255);
    ((OB)L254)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L254)->header.destroyed=0;
#endif

    L252 = ((STR) L254);
    L252->asize = L253;
    r27 = L252;
    SARR((STR)r27,0,create_c18); 
    ;
    ret_val240 = r27;
    ret_val239 = ret_val240;
    s26 = ret_val239;
    ret_val238 = STR_ap1077157958(plus_self84, s26, TRUE);
    err_s18 = ret_val238;
    PROG_err_STR(ATTR(err_self18,prog), err_s18);
   }
   fetch_self44 = match_self13;
   SATTR(fetch_self44,next,SCANNE1792387248(ATTR(fetch_self44,scanner)));
   SATTR(r7,e,PARSER403674127(self));
   match_self14 = self;
   match_t14 = PARSER1207020061;
   is_eq_self22 = ATTR(match_self14,next);
   is_eq_y22 = match_t14;
   L257 = is_eq_self22.val1;
   L2581_=(L257)==(is_eq_y22); 
   ret_val241 = L2581_;
   L259 = ret_val241;
   L2601_=!(L259); 
   if (L2601_) {
    exp_error_self14 = match_self14;
    create_self62 = TOKEN_zero;
    create_val14 = match_t14;
    t14.val1 = create_val14;
    ret_val242 = t14;
    exp_error_msg14 = TOKEN_strrSTR(ret_val242);
    error_self19 = exp_error_self14;
    plus_self85 = exp_error_msg14;
    plus_sarg66 = ((STR) &expectedbutfound);
    ret_val243 = STR_ap2004550586(plus_self85, plus_sarg66);
    plus_self86 = ret_val243;
    plus_sarg67 = TOKEN_strrSTR(ATTR(exp_error_self14,next));
    ret_val244 = STR_ap2004550586(plus_self86, plus_sarg67);
    error_msg19 = ret_val244;
    set_eloc_self19 = ATTR(error_self19,prog);
    source_loc_self47 = error_self19;
    ret_val245 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l19 = ret_val245;
    SATTR(set_eloc_self19,eloc,set_eloc_l19);
    err_self19 = error_self19;
    plus_self87 = error_msg19;
    plus_sarg68 = ((STR) &in1);
    ret_val246 = STR_ap2004550586(plus_self87, plus_sarg68);
    plus_self88 = ret_val246;
    plus_sarg69 = FLISTSTR_toprSTR(ATTR(error_self19,entered));
    ret_val247 = STR_ap2004550586(plus_self88, plus_sarg69);
    plus_self89 = ret_val247;
    plus_arg19 = ')';
    str_self19 = plus_arg19;
    create_self63 = ((STR) NULL);
    create_c19 = str_self19;
    L262 = 1;
    L264=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L262)*sizeof(CHAR);
    L263=ZALLOC_LEAF_BIG(L264);
    if (L263==NULL) FATAL("Unable to allocate more memory");
    memset(L263,0,L264);
    ((OB)L263)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L263)->header.destroyed=0;
#endif

    L261 = ((STR) L263);
    L261->asize = L262;
    r28 = L261;
    SARR((STR)r28,0,create_c19); 
    ;
    ret_val250 = r28;
    ret_val249 = ret_val250;
    s27 = ret_val249;
    ret_val248 = STR_ap1077157958(plus_self89, s27, TRUE);
    err_s19 = ret_val248;
    PROG_err_STR(ATTR(err_self19,prog), err_s19);
   }
   fetch_self45 = match_self14;
   SATTR(fetch_self45,next,SCANNE1792387248(ATTR(fetch_self45,scanner)));
   exit_self7 = self;
   s28 = FLISTSTR_poprSTR(ATTR(exit_self7,entered));
   break;
  case 106: 
   enter_self9 = self;
   enter_s9 = ((STR) &farexpression);
   SATTR(enter_self9,entered,FLISTS1425610062(ATTR(enter_self9,entered), enter_s9));
   create_self64 = ((AS_FAR_EXPR) NULL);
   L267=ZALLOC(sizeof(struct AS_FAR_EXPR_struct));
   if (L267==NULL) FATAL("Unable to allocate more memory");
   ((OB)L267)->header.tag=AS_FAR_EXPR_tag;
   L266 = ((AS_FAR_EXPR) L267);
   ret_val251 = L266;
   r8 = ret_val251;
   source_loc_self48 = self;
   ret_val252 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(r8,source1,ret_val252);
   res = ((dAS_EXPR) r8);
   fetch_self46 = self;
   SATTR(fetch_self46,next,SCANNE1792387248(ATTR(fetch_self46,scanner)));
   match_self15 = self;
   match_t15 = PARSER1209676627;
   is_eq_self23 = ATTR(match_self15,next);
   is_eq_y23 = match_t15;
   L268 = is_eq_self23.val1;
   L2691_=(L268)==(is_eq_y23); 
   ret_val253 = L2691_;
   L270 = ret_val253;
   L2721_=!(L270); 
   if (L2721_) {
    exp_error_self15 = match_self15;
    create_self65 = TOKEN_zero;
    create_val15 = match_t15;
    t15.val1 = create_val15;
    ret_val254 = t15;
    exp_error_msg15 = TOKEN_strrSTR(ret_val254);
    error_self20 = exp_error_self15;
    plus_self90 = exp_error_msg15;
    plus_sarg70 = ((STR) &expectedbutfound);
    ret_val255 = STR_ap2004550586(plus_self90, plus_sarg70);
    plus_self91 = ret_val255;
    plus_sarg71 = TOKEN_strrSTR(ATTR(exp_error_self15,next));
    ret_val256 = STR_ap2004550586(plus_self91, plus_sarg71);
    error_msg20 = ret_val256;
    set_eloc_self20 = ATTR(error_self20,prog);
    source_loc_self49 = error_self20;
    ret_val257 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l20 = ret_val257;
    SATTR(set_eloc_self20,eloc,set_eloc_l20);
    err_self20 = error_self20;
    plus_self92 = error_msg20;
    plus_sarg72 = ((STR) &in1);
    ret_val258 = STR_ap2004550586(plus_self92, plus_sarg72);
    plus_self93 = ret_val258;
    plus_sarg73 = FLISTSTR_toprSTR(ATTR(error_self20,entered));
    ret_val259 = STR_ap2004550586(plus_self93, plus_sarg73);
    plus_self94 = ret_val259;
    plus_arg20 = ')';
    str_self20 = plus_arg20;
    create_self66 = ((STR) NULL);
    create_c20 = str_self20;
    L274 = 1;
    L276=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L274)*sizeof(CHAR);
    L275=ZALLOC_LEAF_BIG(L276);
    if (L275==NULL) FATAL("Unable to allocate more memory");
    memset(L275,0,L276);
    ((OB)L275)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L275)->header.destroyed=0;
#endif

    L273 = ((STR) L275);
    L273->asize = L274;
    r29 = L273;
    SARR((STR)r29,0,create_c20); 
    ;
    ret_val262 = r29;
    ret_val261 = ret_val262;
    s29 = ret_val261;
    ret_val260 = STR_ap1077157958(plus_self94, s29, TRUE);
    err_s20 = ret_val260;
    PROG_err_STR(ATTR(err_self20,prog), err_s20);
   }
   fetch_self47 = match_self15;
   SATTR(fetch_self47,next,SCANNE1792387248(ATTR(fetch_self47,scanner)));
   SATTR(r8,e,PARSER403674127(self));
   match_self16 = self;
   match_t16 = PARSER1207020061;
   is_eq_self24 = ATTR(match_self16,next);
   is_eq_y24 = match_t16;
   L278 = is_eq_self24.val1;
   L2791_=(L278)==(is_eq_y24); 
   ret_val263 = L2791_;
   L280 = ret_val263;
   L2811_=!(L280); 
   if (L2811_) {
    exp_error_self16 = match_self16;
    create_self67 = TOKEN_zero;
    create_val16 = match_t16;
    t16.val1 = create_val16;
    ret_val264 = t16;
    exp_error_msg16 = TOKEN_strrSTR(ret_val264);
    error_self21 = exp_error_self16;
    plus_self95 = exp_error_msg16;
    plus_sarg74 = ((STR) &expectedbutfound);
    ret_val265 = STR_ap2004550586(plus_self95, plus_sarg74);
    plus_self96 = ret_val265;
    plus_sarg75 = TOKEN_strrSTR(ATTR(exp_error_self16,next));
    ret_val266 = STR_ap2004550586(plus_self96, plus_sarg75);
    error_msg21 = ret_val266;
    set_eloc_self21 = ATTR(error_self21,prog);
    source_loc_self50 = error_self21;
    ret_val267 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l21 = ret_val267;
    SATTR(set_eloc_self21,eloc,set_eloc_l21);
    err_self21 = error_self21;
    plus_self97 = error_msg21;
    plus_sarg76 = ((STR) &in1);
    ret_val268 = STR_ap2004550586(plus_self97, plus_sarg76);
    plus_self98 = ret_val268;
    plus_sarg77 = FLISTSTR_toprSTR(ATTR(error_self21,entered));
    ret_val269 = STR_ap2004550586(plus_self98, plus_sarg77);
    plus_self99 = ret_val269;
    plus_arg21 = ')';
    str_self21 = plus_arg21;
    create_self68 = ((STR) NULL);
    create_c21 = str_self21;
    L283 = 1;
    L285=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L283)*sizeof(CHAR);
    L284=ZALLOC_LEAF_BIG(L285);
    if (L284==NULL) FATAL("Unable to allocate more memory");
    memset(L284,0,L285);
    ((OB)L284)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L284)->header.destroyed=0;
#endif

    L282 = ((STR) L284);
    L282->asize = L283;
    r30 = L282;
    SARR((STR)r30,0,create_c21); 
    ;
    ret_val272 = r30;
    ret_val271 = ret_val272;
    s30 = ret_val271;
    ret_val270 = STR_ap1077157958(plus_self99, s30, TRUE);
    err_s21 = ret_val270;
    PROG_err_STR(ATTR(err_self21,prog), err_s21);
   }
   fetch_self48 = match_self16;
   SATTR(fetch_self48,next,SCANNE1792387248(ATTR(fetch_self48,scanner)));
   exit_self8 = self;
   s31 = FLISTSTR_poprSTR(ATTR(exit_self8,entered));
   break;
  default: ;
   exp_error_self17 = self;
   exp_error_msg17 = ((STR) &expression);
   error_self22 = exp_error_self17;
   plus_self100 = exp_error_msg17;
   plus_sarg78 = ((STR) &expectedbutfound);
   ret_val273 = STR_ap2004550586(plus_self100, plus_sarg78);
   plus_self101 = ret_val273;
   plus_sarg79 = TOKEN_strrSTR(ATTR(exp_error_self17,next));
   ret_val274 = STR_ap2004550586(plus_self101, plus_sarg79);
   error_msg22 = ret_val274;
   set_eloc_self22 = ATTR(error_self22,prog);
   source_loc_self51 = error_self22;
   ret_val275 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l22 = ret_val275;
   SATTR(set_eloc_self22,eloc,set_eloc_l22);
   err_self22 = error_self22;
   plus_self102 = error_msg22;
   plus_sarg80 = ((STR) &in1);
   ret_val276 = STR_ap2004550586(plus_self102, plus_sarg80);
   plus_self103 = ret_val276;
   plus_sarg81 = FLISTSTR_toprSTR(ATTR(error_self22,entered));
   ret_val277 = STR_ap2004550586(plus_self103, plus_sarg81);
   plus_self104 = ret_val277;
   plus_arg22 = ')';
   str_self22 = plus_arg22;
   create_self69 = ((STR) NULL);
   create_c22 = str_self22;
   L288 = 1;
   L290=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L288)*sizeof(CHAR);
   L289=ZALLOC_LEAF_BIG(L290);
   if (L289==NULL) FATAL("Unable to allocate more memory");
   memset(L289,0,L290);
   ((OB)L289)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L289)->header.destroyed=0;
#endif

   L287 = ((STR) L289);
   L287->asize = L288;
   r31 = L287;
   SARR((STR)r31,0,create_c22); 
   ;
   ret_val280 = r31;
   ret_val279 = ret_val280;
   s32 = ret_val279;
   ret_val278 = STR_ap1077157958(plus_self104, s32, TRUE);
   err_s22 = ret_val278;
   PROG_err_STR(ATTR(err_self22,prog), err_s22);
   create_self70 = ((AS_VOID_EXPR) NULL);
   L294=ZALLOC(sizeof(struct AS_VOID_EXPR_struct));
   if (L294==NULL) FATAL("Unable to allocate more memory");
   ((OB)L294)->header.tag=AS_VOID_EXPR_tag;
   L293 = ((AS_VOID_EXPR) L294);
   ret_val281 = L293;
   res = ((dAS_EXPR) ret_val281);
   source_loc_self52 = self;
   ret_val282 = SFILE_1594766561(SFILE_ID_zero);
   L295 = res;
   (*dAS_EX753042462[TAG(L295)])(L295, ret_val282);
 }
 exit_self9 = self;
 s33 = FLISTSTR_poprSTR(ATTR(exit_self9,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER680516091(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 AS_CALL_EXPR c;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER exit_self;
 STR s;
 extern STR expressionprec2;
 AS_CALL_EXPR L1;
 OB L2;
 enter_self = self;
 enter_s = ((STR) &expressionprec2);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER1259074744(self, FALSE);
 if (PARSER2128430730(self, PARSER_pow_tok)) {
  create_self = ((AS_CALL_EXPR) NULL);
  L2=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=AS_CALL_EXPR_tag;
  L1 = ((AS_CALL_EXPR) L2);
  ret_val1 = L1;
  c = ret_val1;
  source_loc_self = self;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(c,source1,ret_val2);
  SATTR(c,name1,IDENT_1974148927);
  SATTR(c,ob,res);
  SATTR(c,args,PARSER680516091(self));
  SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
  res = ((dAS_EXPR) c);
 }
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER681401613(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 IDENT name111 = IDENT_zero;
 AS_CALL_EXPR c;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER exit_self;
 STR s;
 extern STR expressionprec4;
 AS_CALL_EXPR L1;
 OB L2;
 enter_self = self;
 enter_s = ((STR) &expressionprec4);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER1466524796(self);
 while (1) {
  if (PARSER2128430730(self, PARSER_times_tok)) {
   name111 = IDENT_1616879092;
  }
  else {
   if (PARSER2128430730(self, PARSER460241977)) {
    name111 = IDENT_228725606;
   }
   else {
    if (PARSER2128430730(self, PARSER_mod_tok)) {
     name111 = IDENT_475003333;
    }
    else {
     goto after_loop;
    }
   }
  }
  create_self = ((AS_CALL_EXPR) NULL);
  L2=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=AS_CALL_EXPR_tag;
  L1 = ((AS_CALL_EXPR) L2);
  ret_val1 = L1;
  c = ret_val1;
  source_loc_self = self;
  ret_val2 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(c,source1,ret_val2);
  SATTR(c,name1,name111);
  SATTR(c,ob,res);
  SATTR(c,args,PARSER1466524796(self));
  SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
  res = ((dAS_EXPR) c);
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_EXPR PARSER682287135(PARSER self) {
 dAS_EXPR ret_val;
 dAS_EXPR res;
 IDENT name111 = IDENT_zero;
 AS_CALL_EXPR c;
 AS_CALL_EXPR c11;
 AS_CALL_EXPR c2;
 AS_CALL_EXPR c3;
 AS_CALL_EXPR c4;
 AS_CALL_EXPR c21;
 AS_CALL_EXPR c5;
 AS_CALL_EXPR c6;
 AS_CALL_EXPR c22;
 PARSER enter_self;
 STR enter_s;
 AS_CALL_EXPR create_self;
 AS_CALL_EXPR ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 AS_CALL_EXPR create_self1;
 AS_CALL_EXPR ret_val3;
 PARSER source_loc_self1;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 AS_CALL_EXPR create_self2;
 AS_CALL_EXPR ret_val5;
 PARSER source_loc_self2;
 SFILE_ID ret_val6 = SFILE_ID_zero;
 AS_CALL_EXPR create_self3;
 AS_CALL_EXPR ret_val7;
 PARSER source_loc_self3;
 SFILE_ID ret_val8 = SFILE_ID_zero;
 AS_CALL_EXPR create_self4;
 AS_CALL_EXPR ret_val9;
 PARSER source_loc_self4;
 SFILE_ID ret_val10 = SFILE_ID_zero;
 AS_CALL_EXPR create_self5;
 AS_CALL_EXPR ret_val11;
 PARSER source_loc_self5;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 AS_CALL_EXPR create_self6;
 AS_CALL_EXPR ret_val13;
 PARSER source_loc_self6;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 AS_CALL_EXPR create_self7;
 AS_CALL_EXPR ret_val15;
 PARSER source_loc_self7;
 SFILE_ID ret_val16 = SFILE_ID_zero;
 AS_CALL_EXPR create_self8;
 AS_CALL_EXPR ret_val17;
 PARSER source_loc_self8;
 SFILE_ID ret_val18 = SFILE_ID_zero;
 PARSER exit_self;
 STR s;
 extern STR expressionprec6;
 AS_CALL_EXPR L1;
 OB L2;
 AS_CALL_EXPR L3;
 OB L4;
 AS_CALL_EXPR L5;
 OB L6;
 AS_CALL_EXPR L7;
 OB L8;
 AS_CALL_EXPR L9;
 OB L10;
 AS_CALL_EXPR L11;
 OB L12;
 AS_CALL_EXPR L13;
 OB L14;
 AS_CALL_EXPR L15;
 OB L16;
 AS_CALL_EXPR L17;
 OB L18;
 enter_self = self;
 enter_s = ((STR) &expressionprec6);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 res = PARSER1465639274(self);
 while (1) {
  if (PARSER2128430730(self, PARSER_is_eq_tok)) {
   create_self = ((AS_CALL_EXPR) NULL);
   L2=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
   if (L2==NULL) FATAL("Unable to allocate more memory");
   ((OB)L2)->header.tag=AS_CALL_EXPR_tag;
   L1 = ((AS_CALL_EXPR) L2);
   ret_val1 = L1;
   c = ret_val1;
   source_loc_self = self;
   ret_val2 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(c,source1,ret_val2);
   SATTR(c,name1,IDENT_1728630987);
   SATTR(c,ob,res);
   SATTR(c,args,PARSER1465639274(self));
   SATTR(c,modes1,PARSER87164415(self, c, AS_ARG1300937332));
   res = ((dAS_EXPR) c);
  }
  else {
   if (PARSER2128430730(self, PARSER543515898)) {
    create_self1 = ((AS_CALL_EXPR) NULL);
    L4=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L4==NULL) FATAL("Unable to allocate more memory");
    ((OB)L4)->header.tag=AS_CALL_EXPR_tag;
    L3 = ((AS_CALL_EXPR) L4);
    ret_val3 = L3;
    c11 = ret_val3;
    source_loc_self1 = self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(c11,source1,ret_val4);
    SATTR(c11,name1,IDENT_1728630987);
    SATTR(c11,ob,res);
    SATTR(c11,args,PARSER1465639274(self));
    SATTR(c11,modes1,PARSER87164415(self, c11, AS_ARG1300937332));
    create_self2 = ((AS_CALL_EXPR) NULL);
    L6=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
    if (L6==NULL) FATAL("Unable to allocate more memory");
    ((OB)L6)->header.tag=AS_CALL_EXPR_tag;
    L5 = ((AS_CALL_EXPR) L6);
    ret_val5 = L5;
    c2 = ret_val5;
    source_loc_self2 = self;
    ret_val6 = SFILE_1594766561(SFILE_ID_zero);
    SATTR(c2,source1,ret_val6);
    SATTR(c2,name1,IDENT_1837767448);
    SATTR(c2,ob,((dAS_EXPR) c11));
    res = ((dAS_EXPR) c2);
   }
   else {
    if (PARSER2128430730(self, PARSER_is_lt_tok)) {
     create_self3 = ((AS_CALL_EXPR) NULL);
     L8=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
     if (L8==NULL) FATAL("Unable to allocate more memory");
     ((OB)L8)->header.tag=AS_CALL_EXPR_tag;
     L7 = ((AS_CALL_EXPR) L8);
     ret_val7 = L7;
     c3 = ret_val7;
     source_loc_self3 = self;
     ret_val8 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(c3,source1,ret_val8);
     SATTR(c3,name1,IDENT_421158771);
     SATTR(c3,ob,res);
     SATTR(c3,args,PARSER1465639274(self));
     SATTR(c3,modes1,PARSER87164415(self, c3, AS_ARG1300937332));
     res = ((dAS_EXPR) c3);
    }
    else {
     if (PARSER2128430730(self, PARSER1789316521)) {
      create_self4 = ((AS_CALL_EXPR) NULL);
      L10=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
      if (L10==NULL) FATAL("Unable to allocate more memory");
      ((OB)L10)->header.tag=AS_CALL_EXPR_tag;
      L9 = ((AS_CALL_EXPR) L10);
      ret_val9 = L9;
      c4 = ret_val9;
      source_loc_self4 = self;
      ret_val10 = SFILE_1594766561(SFILE_ID_zero);
      SATTR(c4,source1,ret_val10);
      SATTR(c4,name1,IDENT_421158771);
      SATTR(c4,ob,res);
      SATTR(c4,args,PARSER1465639274(self));
      SATTR(c4,modes1,PARSER87164415(self, c4, AS_ARG1300937332));
      create_self5 = ((AS_CALL_EXPR) NULL);
      L12=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
      if (L12==NULL) FATAL("Unable to allocate more memory");
      ((OB)L12)->header.tag=AS_CALL_EXPR_tag;
      L11 = ((AS_CALL_EXPR) L12);
      ret_val11 = L11;
      c21 = ret_val11;
      source_loc_self5 = self;
      ret_val12 = SFILE_1594766561(SFILE_ID_zero);
      SATTR(c21,source1,ret_val12);
      SATTR(c21,name1,IDENT_1837767448);
      SATTR(c21,ob,((dAS_EXPR) c4));
      res = ((dAS_EXPR) c21);
     }
     else {
      if (PARSER2128430730(self, PARSER_is_gt_tok)) {
       create_self6 = ((AS_CALL_EXPR) NULL);
       L14=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
       if (L14==NULL) FATAL("Unable to allocate more memory");
       ((OB)L14)->header.tag=AS_CALL_EXPR_tag;
       L13 = ((AS_CALL_EXPR) L14);
       ret_val13 = L13;
       c5 = ret_val13;
       source_loc_self6 = self;
       ret_val14 = SFILE_1594766561(SFILE_ID_zero);
       SATTR(c5,source1,ret_val14);
       SATTR(c5,name1,IDENT_421158771);
       SATTR(c5,ob,res);
       SATTR(c5,args,PARSER1465639274(self));
       SATTR(c5,modes1,PARSER87164415(self, c5, AS_ARG1300937332));
       SATTR(c5,flip,TRUE);
       res = ((dAS_EXPR) c5);
      }
      else {
       if (PARSER2128430730(self, PARSER1963707920)) {
        create_self7 = ((AS_CALL_EXPR) NULL);
        L16=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
        if (L16==NULL) FATAL("Unable to allocate more memory");
        ((OB)L16)->header.tag=AS_CALL_EXPR_tag;
        L15 = ((AS_CALL_EXPR) L16);
        ret_val15 = L15;
        c6 = ret_val15;
        source_loc_self7 = self;
        ret_val16 = SFILE_1594766561(SFILE_ID_zero);
        SATTR(c6,source1,ret_val16);
        SATTR(c6,name1,IDENT_421158771);
        SATTR(c6,ob,res);
        SATTR(c6,args,PARSER1465639274(self));
        SATTR(c6,modes1,PARSER87164415(self, c6, AS_ARG1300937332));
        SATTR(c6,flip,TRUE);
        create_self8 = ((AS_CALL_EXPR) NULL);
        L18=ZALLOC(sizeof(struct AS_CALL_EXPR_struct));
        if (L18==NULL) FATAL("Unable to allocate more memory");
        ((OB)L18)->header.tag=AS_CALL_EXPR_tag;
        L17 = ((AS_CALL_EXPR) L18);
        ret_val17 = L17;
        c22 = ret_val17;
        source_loc_self8 = self;
        ret_val18 = SFILE_1594766561(SFILE_ID_zero);
        SATTR(c22,source1,ret_val18);
        SATTR(c22,name1,IDENT_1837767448);
        SATTR(c22,ob,((dAS_EXPR) c6));
        res = ((dAS_EXPR) c22);
       }
       else {
        goto after_loop;
       }
      }
     }
    }
   }
  }
 }
 after_loop: ;
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}
