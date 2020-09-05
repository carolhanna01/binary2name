#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dAS_NODE_struct {
 OB_HEADER header;
 } *dAS_NODE;

typedef struct dAS_STMT_struct {
 OB_HEADER header;
 } *dAS_STMT;

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

typedef struct AS_ATTR_DEF_struct {/* layout for AS_ATTR_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_ATTR_DEF;

typedef struct AS_BOOL_LIT_EXPR_struct {/* layout for AS_BOOL_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AS_BOOL_LIT_EXPR;

typedef struct AS_CASE_STMT_struct {/* layout for AS_CASE_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_CASE_WHEN_struct *when_part;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_CASE_STMT;

typedef struct AS_CASE_WHEN_struct {/* layout for AS_CASE_WHEN */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct AS_CASE_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 } *AS_CASE_WHEN;

typedef struct AS_FORK_STMT_struct {/* layout for AS_FORK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_FORK_STMT;

typedef struct AS_IDENT_LIST_struct {/* layout for AS_IDENT_LIST */
 OB_HEADER header;
 struct AS_IDENT_LIST_struct *next;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_IDENT_LIST;

typedef struct AS_IF_STMT_struct {/* layout for AS_IF_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_IF_STMT;

typedef struct AS_LOCK_IF_WHEN_struct {/* layout for AS_LOCK_IF_WHEN */
 OB_HEADER header;
 struct dAS_EXPR_struct *e_list;
 struct dAS_EXPR_struct *val1;
 struct AS_LOCK_IF_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 } *AS_LOCK_IF_WHEN;

typedef struct AS_LOCK_STMT_struct {/* layout for AS_LOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e_list;
 struct dAS_STMT_struct *next;
 struct AS_LOCK_IF_WHEN_struct *if_when_part;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_LOCK_STMT;

typedef struct AS_LOOP_STMT_struct {/* layout for AS_LOOP_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_LOOP_STMT;

typedef struct AS_PAR_STMT_struct {/* layout for AS_PAR_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_PAR_STMT;

typedef struct AS_PROTECT_STMT_struct {/* layout for AS_PROTECT_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_PROTECT_WHEN_struct *when_part;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_PROTECT_STMT;

typedef struct AS_PROTECT_WHEN_struct {/* layout for AS_PROTECT_WHEN */
 OB_HEADER header;
 struct AS_PROTECT_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_PROTECT_WHEN;

typedef struct AS_RETURN_STMT_struct {/* layout for AS_RETURN_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_RETURN_STMT;

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

typedef struct AS_SHARED_DEF_struct {/* layout for AS_SHARED_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct dAS_EXPR_struct *init;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_SHARED_DEF;

typedef struct AS_STMT_LIST_struct {/* layout for AS_STMT_LIST */
 OB_HEADER header;
 struct dAS_STMT_struct *stmts;
 struct SFILE_ID_struct source1;
 } *AS_STMT_LIST;

typedef struct AS_TYPECASE_STMT_struct {/* layout for AS_TYPECASE_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPECASE_WHEN_struct *when_part;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 BOOL transformed;
 } *AS_TYPECASE_STMT;

typedef struct AS_TYPECASE_WHEN_struct {/* layout for AS_TYPECASE_WHEN */
 OB_HEADER header;
 struct AS_STMT_LIST_struct *then_part;
 struct AS_TYPECASE_WHEN_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_TYPECASE_WHEN;

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

typedef struct AS_WIT1063437351_struct {/* layout for AS_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_IDENT_LIST_struct *idents;
 struct AS_STMT_LIST_struct *else_part;
 struct AS_STMT_LIST_struct *near_part;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 INT elts_size;
 struct SFILE_ID_struct source1;
 BOOL self_occurred;
 BOOL transformed;
 } *AS_WIT1063437351;

typedef struct AS_YIELD_STMT_struct {/* layout for AS_YIELD_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_YIELD_STMT;

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

extern INT AS_ARG551300469;
extern INT PARSER1019557737;
extern INT PARSER1281839191;
extern INT PARSER2033525488;
extern INT PARSER265941484;
extern INT PARSER550831344;
extern INT PARSER689908022;
extern INT PARSER729653115;
extern INT PARSER840587154;
extern INT PARSER998808835;
extern INT PARSER_at_tok;
extern INT PARSER_attr_tok;
extern INT PARSER_bang_tok;
extern INT PARSER_case_tok;
extern INT PARSER_colon_tok;
extern INT PARSER_comma_tok;
extern INT PARSER_do_tok;
extern INT PARSER_else_tok;
extern INT PARSER_elsif_tok;
extern INT PARSER_end_tok;
extern INT PARSER_guard_tok;
extern INT PARSER_ident_tok;
extern INT PARSER_lock_tok;
extern INT PARSER_loop_tok;
extern INT PARSER_par_tok;
extern INT PARSER_self_tok;
extern INT PARSER_stub_tok;
extern INT PARSER_then_tok;
extern INT PARSER_when_tok;
extern INT PARSER_yield_tok;

/* Function declarations */


extern RETURNED_CONST dAS_CLASS_ELT (**dAS_CL760354380)(dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_CL1134203643)(dAS_CLASS_ELT, BOOL);

extern RETURNED_CONST void (**dAS_ST1454156204)(dAS_STMT, dAS_STMT);
AS_ARG_DEC PARSER13880807(PARSER, BOOL);
AS_ARG_MODE PARSER930791008(PARSER, BOOL);
AS_CASE_STMT PARSER552741043(PARSER);
AS_IDENT_LIST PARSER1448067791(PARSER, dAS_NODE);
AS_IF_STMT PARSER164187557(PARSER);
AS_LOCK_STMT PARSER1287362985(PARSER);
AS_LOOP_STMT PARSER931477965(PARSER);
AS_PAR_STMT PARSER1651016500(PARSER);
AS_PAR_STMT PARSER2032164458(PARSER);
AS_PROTECT_STMT PARSER1254758060(PARSER);
AS_RETURN_STMT PARSER1796842837(PARSER);
AS_ROUT_DEF PARSER2039100746(PARSER);
AS_STMT_LIST PARSER680097147(PARSER);
AS_TYPECASE_STMT PARSER670949263(PARSER);
AS_TYPE_SPEC PARSER2028498823(PARSER);
AS_YIELD_STMT PARSER1414885906(PARSER);
BOOL AS_ARG1391068469(AS_ARG_MODE);
BOOL PARSER1152230473(PARSER, TOKEN);
BOOL PARSER1429651936(PARSER, TOKEN);
BOOL PARSER2128430730(PARSER, INT);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
SFILE_ID SFILE_1594766561(SFILE_ID);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR TOKEN_strrSTR(TOKEN);
TOKEN SCANNE1792387248(SCANNER);
dAS_CLASS_ELT PARSER1594550222(PARSER, TOKEN);
dAS_CLASS_ELT PARSER1886860611(PARSER, TOKEN);
dAS_CLASS_ELT PARSER252822802(PARSER, TOKEN);
dAS_EXPR PARSER1134972014(PARSER, BOOL);
dAS_EXPR PARSER403674127(PARSER);
void AS_ARG2059479532(AS_ARG_DEC, AS_ARG_DEC);
void AS_CAS1219409394(AS_CASE_WHEN, AS_CASE_WHEN);
void AS_IDE1407507383(AS_IDENT_LIST, AS_IDENT_LIST);
void AS_LOC1037771083(AS_LOCK_IF_WHEN, AS_LOCK_IF_WHEN);
void AS_PRO1103329709(AS_PROTECT_WHEN, AS_PROTECT_WHEN);
void AS_TYP1805639138(AS_TYPECASE_WHEN, AS_TYPECASE_WHEN);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_ARG_DEC PARSER13880807(PARSER self, BOOL is_iter) {
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
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val15;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val16;
 STR plus_self7;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val17;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val18;
 STR create_self3;
 CHAR create_c1 = CHAR_zero;
 STR ret_val19;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val20 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val21 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self8;
 STR plus_sarg6;
 STR ret_val22;
 STR plus_self9;
 STR plus_sarg7;
 STR ret_val23;
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
 PARSER fetch_self1;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val32;
 STR plus_self15;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val33;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val34;
 STR create_self6;
 CHAR create_c3 = CHAR_zero;
 STR ret_val35;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self5;
 SFILE_ID ret_val36 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self16;
 STR plus_sarg12;
 STR ret_val37;
 STR plus_self17;
 STR plus_sarg13;
 STR ret_val38;
 STR plus_self18;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val39;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val40;
 STR create_self7;
 CHAR create_c4 = CHAR_zero;
 STR ret_val41;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER exit_self;
 STR s5;
 extern STR routin1936166156;
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
 BOOL L12;
 INT L13;
 INT L14;
 BOOL L151_;
 BOOL L161_;
 extern STR oncear1799949476;
 extern STR in1;
 STR L17;
 INT L18;
 OB L19;
 INT L20;
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
 BOOL L31;
 BOOL L32;
 BOOL L331_;
 extern STR hotarg180607827;
 extern STR in1;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 INT L39;
 INT L40;
 BOOL L421_;
 BOOL L43;
 BOOL L441_;
 BOOL L45;
 BOOL L46;
 BOOL L471_;
 extern STR oncear558871975;
 extern STR in1;
 STR L48;
 INT L49;
 OB L50;
 INT L51;
 enter_self = self;
 enter_s = ((STR) &routin1936166156);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
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
  mode1 = PARSER930791008(self, is_iter);
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
  L13 = ATTR(ATTR(newa,mode1),mod);
  L14 = AS_ARG551300469;
  L151_=(L13)==(L14); 
  if (L151_) {
   L161_=!(is_iter); 
   L12 = L161_;
  } else {
   L12 = FALSE;
  }
  if (L12) {
   error_self1 = self;
   error_msg1 = ((STR) &oncear1799949476);
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self2 = error_self1;
   ret_val14 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l1 = ret_val14;
   SATTR(set_eloc_self1,eloc,set_eloc_l1);
   err_self1 = error_self1;
   plus_self5 = error_msg1;
   plus_sarg4 = ((STR) &in1);
   ret_val15 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val15;
   plus_sarg5 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
   ret_val16 = STR_ap2004550586(plus_self6, plus_sarg5);
   plus_self7 = ret_val16;
   plus_arg1 = ')';
   str_self1 = plus_arg1;
   create_self3 = ((STR) NULL);
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
   ret_val19 = r1;
   ret_val18 = ret_val19;
   s1 = ret_val18;
   ret_val17 = STR_ap1077157958(plus_self7, s1, TRUE);
   err_s1 = ret_val17;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
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
 L22 = is_eq_self1.val1;
 L231_=(L22)==(is_eq_y1); 
 ret_val20 = L231_;
 L24 = ret_val20;
 L251_=!(L24); 
 if (L251_) {
  exp_error_self1 = match_self1;
  create_self4 = TOKEN_zero;
  create_val1 = match_t1;
  t1.val1 = create_val1;
  ret_val21 = t1;
  exp_error_msg1 = TOKEN_strrSTR(ret_val21);
  error_self2 = exp_error_self1;
  plus_self8 = exp_error_msg1;
  plus_sarg6 = ((STR) &expectedbutfound);
  ret_val22 = STR_ap2004550586(plus_self8, plus_sarg6);
  plus_self9 = ret_val22;
  plus_sarg7 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val23 = STR_ap2004550586(plus_self9, plus_sarg7);
  error_msg2 = ret_val23;
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
  ret_val29 = r2;
  ret_val28 = ret_val29;
  s2 = ret_val28;
  ret_val27 = STR_ap1077157958(plus_self12, s2, TRUE);
  err_s2 = ret_val27;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self1 = match_self1;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 tp = PARSER2028498823(self);
 if (ATTR(self,version_1_0)) {
  if (PARSER2128430730(self, PARSER_bang_tok)) {
   L31 = TRUE;
  } else {
   L31 = PARSER2128430730(self, PARSER840587154);
  }
  hot = L31;
  if (hot) {
   L331_=!(is_iter); 
   L32 = L331_;
  } else {
   L32 = FALSE;
  }
  if (L32) {
   error_self3 = self;
   error_msg3 = ((STR) &hotarg180607827);
   set_eloc_self3 = ATTR(error_self3,prog);
   source_loc_self4 = error_self3;
   ret_val30 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l3 = ret_val30;
   SATTR(set_eloc_self3,eloc,set_eloc_l3);
   err_self3 = error_self3;
   plus_self13 = error_msg3;
   plus_sarg10 = ((STR) &in1);
   ret_val31 = STR_ap2004550586(plus_self13, plus_sarg10);
   plus_self14 = ret_val31;
   plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
   ret_val32 = STR_ap2004550586(plus_self14, plus_sarg11);
   plus_self15 = ret_val32;
   plus_arg3 = ')';
   str_self3 = plus_arg3;
   create_self6 = ((STR) NULL);
   create_c3 = str_self3;
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
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val35 = r3;
   ret_val34 = ret_val35;
   s3 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self15, s3, TRUE);
   err_s3 = ret_val33;
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
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
    L39 = ATTR(ATTR(p,mode1),mod);
    L40 = AS_ARG551300469;
    L421_=(L39)==(L40); 
    L43 = L421_;
    L441_=!(L43); 
    SATTR(p,is_hot,L441_);
   }
  }
  if (is_iter) {
   L46 = ATTR(p,is_hot);
   L471_=!(L46); 
   if (L471_) {
    L45 = AS_ARG1391068469(ATTR(p,mode1));
   } else {
    L45 = FALSE;
   }
   if (L45) {
    error_self4 = self;
    error_msg4 = ((STR) &oncear558871975);
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self5 = error_self4;
    ret_val36 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val36;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self16 = error_msg4;
    plus_sarg12 = ((STR) &in1);
    ret_val37 = STR_ap2004550586(plus_self16, plus_sarg12);
    plus_self17 = ret_val37;
    plus_sarg13 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val38 = STR_ap2004550586(plus_self17, plus_sarg13);
    plus_self18 = ret_val38;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self7 = ((STR) NULL);
    create_c4 = str_self4;
    L49 = 1;
    L51=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L49)*sizeof(CHAR);
    L50=ZALLOC_LEAF_BIG(L51);
    if (L50==NULL) FATAL("Unable to allocate more memory");
    memset(L50,0,L51);
    ((OB)L50)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L50)->header.destroyed=0;
#endif

    L48 = ((STR) L50);
    L48->asize = L49;
    r4 = L48;
    SARR((STR)r4,0,create_c4); 
    ;
    ret_val41 = r4;
    ret_val40 = ret_val41;
    s4 = ret_val40;
    ret_val39 = STR_ap1077157958(plus_self18, s4, TRUE);
    err_s4 = ret_val39;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
  }
  p = ATTR(p,next);
 }
 after_loop1: ;
 exit_self = self;
 s5 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_CASE_STMT PARSER552741043(PARSER self) {
 AS_CASE_STMT ret_val;
 AS_CASE_STMT res;
 AS_CASE_WHEN first1 = ((AS_CASE_WHEN) NULL);
 AS_CASE_WHEN this1 = ((AS_CASE_WHEN) NULL);
 AS_STMT_LIST st;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_CASE_STMT create_self2;
 AS_CASE_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 AS_CASE_WHEN create_self3;
 AS_CASE_WHEN ret_val13;
 PARSER source_loc_self2;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val16 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self3;
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
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val25 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val26 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val27;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val28;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self4;
 SFILE_ID ret_val29 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val30;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val31;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val32;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val33;
 STR create_self7;
 CHAR create_c2 = CHAR_zero;
 STR ret_val34;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER exit_self;
 STR s3;
 extern STR casestatement;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_CASE_STMT L10;
 OB L11;
 AS_CASE_WHEN L12;
 OB L13;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 enter_self = self;
 enter_s = ((STR) &casestatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER_case_tok;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_CASE_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_CASE_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_CASE_STMT_tag;
 L10 = ((AS_CASE_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 SATTR(res,test1,PARSER403674127(self));
 while (1) {
  if (PARSER2128430730(self, PARSER_when_tok)) {
  }
  else {
   goto after_loop;
  }
  first1 = ((AS_CASE_WHEN) NULL);
  while (1) {
   create_self3 = ((AS_CASE_WHEN) NULL);
   L13=ZALLOC(sizeof(struct AS_CASE_WHEN_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=AS_CASE_WHEN_tag;
   L12 = ((AS_CASE_WHEN) L13);
   ret_val13 = L12;
   this1 = ret_val13;
   source_loc_self2 = self;
   ret_val14 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(this1,source1,ret_val14);
   if ((first1==((AS_CASE_WHEN) NULL))) {
    first1 = this1;
   }
   SATTR(this1,val1,PARSER403674127(self));
   if ((ATTR(res,when_part)==((AS_CASE_WHEN) NULL))) {
    SATTR(res,when_part,this1);
   }
   else {
    AS_CAS1219409394(ATTR(res,when_part), this1);
   }
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop1;
   }
  }
  after_loop1: ;
  match_self1 = self;
  match_t1 = PARSER_then_tok;
  is_eq_self1 = ATTR(match_self1,next);
  is_eq_y1 = match_t1;
  L14 = is_eq_self1.val1;
  L151_=(L14)==(is_eq_y1); 
  ret_val15 = L151_;
  L16 = ret_val15;
  L171_=!(L16); 
  if (L171_) {
   exp_error_self1 = match_self1;
   create_self4 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val16 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val16);
   error_self1 = exp_error_self1;
   plus_self5 = exp_error_msg1;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val18;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self3 = error_self1;
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
   create_self5 = ((STR) NULL);
   create_c1 = str_self1;
   L19 = 1;
   L22=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L19)*sizeof(CHAR);
   L20=ZALLOC_LEAF_BIG(L22);
   if (L20==NULL) FATAL("Unable to allocate more memory");
   memset(L20,0,L22);
   ((OB)L20)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L20)->header.destroyed=0;
#endif

   L18 = ((STR) L20);
   L18->asize = L19;
   r1 = L18;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self1 = match_self1;
  SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  st = PARSER680097147(self);
  this1 = first1;
  while (1) {
   if ((this1==((AS_CASE_WHEN) NULL))) {
    goto after_loop2;
   }
   SATTR(this1,then_part,st);
   this1 = ATTR(this1,next);
  }
  after_loop2: ;
 }
 after_loop: ;
 if (PARSER2128430730(self, PARSER_else_tok)) {
  SATTR(res,else_part,PARSER680097147(self));
  SATTR(res,no_else,FALSE);
 }
 else {
  SATTR(res,no_else,TRUE);
 }
 match_self2 = self;
 match_t2 = PARSER_end_tok;
 is_eq_self2 = ATTR(match_self2,next);
 is_eq_y2 = match_t2;
 L24 = is_eq_self2.val1;
 L251_=(L24)==(is_eq_y2); 
 ret_val25 = L251_;
 L26 = ret_val25;
 L271_=!(L26); 
 if (L271_) {
  exp_error_self2 = match_self2;
  create_self6 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val26 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val26);
  error_self2 = exp_error_self2;
  plus_self10 = exp_error_msg2;
  plus_sarg8 = ((STR) &expectedbutfound);
  ret_val27 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val27;
  plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val28 = STR_ap2004550586(plus_self11, plus_sarg9);
  error_msg2 = ret_val28;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self4 = error_self2;
  ret_val29 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val29;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self12 = error_msg2;
  plus_sarg10 = ((STR) &in1);
  ret_val30 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val30;
  plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val31 = STR_ap2004550586(plus_self13, plus_sarg11);
  plus_self14 = ret_val31;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self7 = ((STR) NULL);
  create_c2 = str_self2;
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
  ret_val34 = r2;
  ret_val33 = ret_val34;
  s2 = ret_val33;
  ret_val32 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val32;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self2 = match_self2;
 SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_IDENT_LIST PARSER1448067791(PARSER self, dAS_NODE t) {
 AS_IDENT_LIST ret_val;
 AS_IDENT_LIST res = ((AS_IDENT_LIST) NULL);
 AS_IDENT_LIST id = ((AS_IDENT_LIST) NULL);
 PARSER enter_self;
 STR enter_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 AS_IDENT_LIST create_self;
 AS_IDENT_LIST ret_val2;
 PARSER ident_self;
 IDENT ret_val3 = IDENT_zero;
 PARSER source_loc_self;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val5 = BOOL_zero;
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
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val13;
 STR r;
 PARSER err_self;
 STR err_s;
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
 PARSER source_loc_self2;
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
 STR create_self2;
 CHAR create_c1 = CHAR_zero;
 STR ret_val21;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val22 = BOOL_zero;
 PARSER fetch_self;
 PARSER fetch_self1;
 PARSER exit_self;
 STR s2;
 extern STR identifierlist;
 INT L1;
 BOOL L21_;
 AS_IDENT_LIST L3;
 OB L4;
 INT L5;
 BOOL L61_;
 extern STR identifier;
 extern STR expectedbutfound;
 extern STR in1;
 STR L7;
 INT L8;
 OB L9;
 INT L10;
 extern STR identifier;
 extern STR expectedbutfound;
 extern STR in1;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 INT L17;
 BOOL L181_;
 enter_self = self;
 enter_s = ((STR) &identifierlist);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
  is_eq_self = ATTR(self,next);
  is_eq_y = PARSER_ident_tok;
  L1 = is_eq_self.val1;
  L21_=(L1)==(is_eq_y); 
  ret_val1 = L21_;
  if (ret_val1) {
   create_self = ((AS_IDENT_LIST) NULL);
   L4=ZALLOC(sizeof(struct AS_IDENT_LIST_struct));
   if (L4==NULL) FATAL("Unable to allocate more memory");
   ((OB)L4)->header.tag=AS_IDENT_LIST_tag;
   L3 = ((AS_IDENT_LIST) L4);
   ret_val2 = L3;
   id = ret_val2;
   ident_self = self;
   ret_val3 = ATTR(ATTR(ident_self,scanner),lex_value);
   SATTR(id,name1,ret_val3);
   source_loc_self = self;
   ret_val4 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(id,source1,ret_val4);
  }
  else {
   is_eq_self1 = ATTR(self,next);
   is_eq_y1 = PARSER_self_tok;
   L5 = is_eq_self1.val1;
   L61_=(L5)==(is_eq_y1); 
   ret_val5 = L61_;
   if (ret_val5) {
    if (t==NULL) {
     goto other107;
    } else
    switch (TAG(t)) {
     case AS_WIT1063437351_tag:
      SATTR(((AS_WIT1063437351) t),self_occurred,TRUE); break;
     default: ;
     other107: ;
      exp_error_self = self;
      exp_error_msg = ((STR) &identifier);
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
      create_self1 = ((STR) NULL);
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
      ret_val13 = r;
      ret_val12 = ret_val13;
      s = ret_val12;
      ret_val11 = STR_ap1077157958(plus_self4, s, TRUE);
      err_s = ret_val11;
      PROG_err_STR(ATTR(err_self,prog), err_s);
    }
   }
   else {
    exp_error_self1 = self;
    exp_error_msg1 = ((STR) &identifier);
    error_self1 = exp_error_self1;
    plus_self5 = exp_error_msg1;
    plus_sarg4 = ((STR) &expectedbutfound);
    ret_val14 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val14;
    plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
    ret_val15 = STR_ap2004550586(plus_self6, plus_sarg5);
    error_msg1 = ret_val15;
    set_eloc_self1 = ATTR(error_self1,prog);
    source_loc_self2 = error_self1;
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
    ret_val21 = r1;
    ret_val20 = ret_val21;
    s1 = ret_val20;
    ret_val19 = STR_ap1077157958(plus_self9, s1, TRUE);
    err_s1 = ret_val19;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
  }
  is_eq_self2 = ATTR(self,next);
  is_eq_y2 = PARSER_ident_tok;
  L17 = is_eq_self2.val1;
  L181_=(L17)==(is_eq_y2); 
  ret_val22 = L181_;
  if (ret_val22) {
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
   if ((res==((AS_IDENT_LIST) NULL))) {
    res = id;
   }
   else {
    AS_IDE1407507383(res, id);
   }
  }
  else {
   fetch_self1 = self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_IF_STMT PARSER164187557(PARSER self) {
 AS_IF_STMT ret_val;
 AS_IF_STMT res;
 AS_IF_STMT ifstmt;
 PARSER enter_self;
 STR enter_s;
 AS_IF_STMT create_self;
 AS_IF_STMT ret_val1;
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
 AS_STMT_LIST create_self3;
 AS_STMT_LIST ret_val13;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val14 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
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
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val23;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER exit_self;
 STR s2;
 extern STR ifstatement;
 AS_IF_STMT L1;
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
 AS_STMT_LIST L12;
 OB L13;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L18;
 INT L19;
 OB L20;
 INT L21;
 enter_self = self;
 enter_s = ((STR) &ifstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_IF_STMT) NULL);
 L2=ZALLOC(sizeof(struct AS_IF_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_IF_STMT_tag;
 L1 = ((AS_IF_STMT) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 SATTR(res,test1,PARSER403674127(self));
 match_self = self;
 match_t = PARSER_then_tok;
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
 SATTR(res,then_part,PARSER680097147(self));
 if (PARSER2128430730(self, PARSER_elsif_tok)) {
  create_self3 = ((AS_STMT_LIST) NULL);
  L13=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=AS_STMT_LIST_tag;
  L12 = ((AS_STMT_LIST) L13);
  ret_val13 = L12;
  SATTR(res,else_part,ret_val13);
  ifstmt = PARSER164187557(self);
  SATTR(ifstmt,surr_stmt_list,ATTR(res,else_part));
  SATTR(ATTR(res,else_part),stmts,((dAS_STMT) ifstmt));
 }
 else {
  if (PARSER2128430730(self, PARSER_else_tok)) {
   SATTR(res,else_part,PARSER680097147(self));
  }
  match_self1 = self;
  match_t1 = PARSER_end_tok;
  is_eq_self1 = ATTR(match_self1,next);
  is_eq_y1 = match_t1;
  L14 = is_eq_self1.val1;
  L151_=(L14)==(is_eq_y1); 
  ret_val14 = L151_;
  L16 = ret_val14;
  L171_=!(L16); 
  if (L171_) {
   exp_error_self1 = match_self1;
   create_self4 = TOKEN_zero;
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
   create_self5 = ((STR) NULL);
   create_c1 = str_self1;
   L19 = 1;
   L21=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L19)*sizeof(CHAR);
   L20=ZALLOC_LEAF_BIG(L21);
   if (L20==NULL) FATAL("Unable to allocate more memory");
   memset(L20,0,L21);
   ((OB)L20)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L20)->header.destroyed=0;
#endif

   L18 = ((STR) L20);
   L18->asize = L19;
   r1 = L18;
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
 }
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_LOCK_STMT PARSER1287362985(PARSER self) {
 AS_LOCK_STMT ret_val;
 AS_LOCK_STMT res;
 BOOL with_guard = BOOL_zero;
 BOOL only_one = BOOL_zero;
 AS_LOCK_IF_WHEN this1;
 AS_BOOL_LIT_EXPR r;
 PARSER enter_self;
 STR enter_s;
 AS_LOCK_STMT create_self;
 AS_LOCK_STMT ret_val1;
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
 STR r1;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_LOCK_IF_WHEN create_self3;
 AS_LOCK_IF_WHEN ret_val13;
 PARSER source_loc_self2;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 AS_BOOL_LIT_EXPR create_self4;
 AS_BOOL_LIT_EXPR ret_val15;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val16 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val17 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val18;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val19;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self3;
 SFILE_ID ret_val20 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val21;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val22;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val23;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val24;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val25;
 STR r2;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self7 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val28;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val29;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self4;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val32;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val33;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val34;
 STR create_self8;
 CHAR create_c2 = CHAR_zero;
 STR ret_val35;
 STR r3;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val36 = BOOL_zero;
 TOKEN create_self9 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val37 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val38;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val39;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self5;
 SFILE_ID ret_val40 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val41;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val42;
 STR plus_self19;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val43;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self10;
 CHAR create_c3 = CHAR_zero;
 STR ret_val45;
 STR r4;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self3;
 PARSER exit_self;
 STR s4;
 extern STR lockstatement;
 AS_LOCK_STMT L1;
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
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 AS_LOCK_IF_WHEN L15;
 OB L16;
 AS_BOOL_LIT_EXPR L17;
 OB L18;
 INT L19;
 BOOL L201_;
 BOOL L21;
 BOOL L221_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L23;
 INT L24;
 OB L25;
 INT L26;
 INT L28;
 BOOL L291_;
 BOOL L30;
 BOOL L311_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L32;
 INT L33;
 OB L34;
 INT L35;
 BOOL L37;
 BOOL L38;
 BOOL L39;
 BOOL L401_;
 INT L42;
 BOOL L431_;
 BOOL L44;
 BOOL L451_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L46;
 INT L47;
 OB L48;
 INT L49;
 enter_self = self;
 enter_s = ((STR) &lockstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_LOCK_STMT) NULL);
 L2=ZALLOC(sizeof(struct AS_LOCK_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_LOCK_STMT_tag;
 L1 = ((AS_LOCK_STMT) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 match_self = self;
 match_t = PARSER_lock_tok;
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
  r1 = L7;
  SARR((STR)r1,0,create_c); 
  ;
  ret_val12 = r1;
  ret_val11 = ret_val12;
  s = ret_val11;
  ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val10;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 with_guard = FALSE;
 if (PARSER2128430730(self, PARSER_guard_tok)) {
  with_guard = TRUE;
 }
 only_one = FALSE;
 L121_=!(with_guard); 
 if (L121_) {
  L13 = PARSER2128430730(self, PARSER_when_tok);
  L141_=!(L13); 
  if (L141_) {
   only_one = TRUE;
  }
 }
 while (1) {
  create_self3 = ((AS_LOCK_IF_WHEN) NULL);
  L16=ZALLOC(sizeof(struct AS_LOCK_IF_WHEN_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=AS_LOCK_IF_WHEN_tag;
  L15 = ((AS_LOCK_IF_WHEN) L16);
  ret_val13 = L15;
  this1 = ret_val13;
  source_loc_self2 = self;
  ret_val14 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(this1,source1,ret_val14);
  if (with_guard) {
   SATTR(this1,val1,PARSER403674127(self));
  }
  else {
   create_self4 = ((AS_BOOL_LIT_EXPR) NULL);
   L18=ZALLOC(sizeof(struct AS_BOOL_LIT_EXPR_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=AS_BOOL_LIT_EXPR_tag;
   L17 = ((AS_BOOL_LIT_EXPR) L18);
   ret_val15 = L17;
   r = ret_val15;
   SATTR(r,val1,TRUE);
   SATTR(this1,val1,((dAS_EXPR) r));
  }
  if ((ATTR(res,if_when_part)==((AS_LOCK_IF_WHEN) NULL))) {
   SATTR(res,if_when_part,this1);
  }
  else {
   AS_LOC1037771083(ATTR(res,if_when_part), this1);
  }
  if (with_guard) {
   match_self1 = self;
   match_t1 = PARSER_when_tok;
   is_eq_self1 = ATTR(match_self1,next);
   is_eq_y1 = match_t1;
   L19 = is_eq_self1.val1;
   L201_=(L19)==(is_eq_y1); 
   ret_val16 = L201_;
   L21 = ret_val16;
   L221_=!(L21); 
   if (L221_) {
    exp_error_self1 = match_self1;
    create_self5 = TOKEN_zero;
    create_val1 = match_t1;
    t1.val1 = create_val1;
    ret_val17 = t1;
    exp_error_msg1 = TOKEN_strrSTR(ret_val17);
    error_self1 = exp_error_self1;
    plus_self5 = exp_error_msg1;
    plus_sarg4 = ((STR) &expectedbutfound);
    ret_val18 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val18;
    plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
    ret_val19 = STR_ap2004550586(plus_self6, plus_sarg5);
    error_msg1 = ret_val19;
    set_eloc_self1 = ATTR(error_self1,prog);
    source_loc_self3 = error_self1;
    ret_val20 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l1 = ret_val20;
    SATTR(set_eloc_self1,eloc,set_eloc_l1);
    err_self1 = error_self1;
    plus_self7 = error_msg1;
    plus_sarg6 = ((STR) &in1);
    ret_val21 = STR_ap2004550586(plus_self7, plus_sarg6);
    plus_self8 = ret_val21;
    plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
    ret_val22 = STR_ap2004550586(plus_self8, plus_sarg7);
    plus_self9 = ret_val22;
    plus_arg1 = ')';
    str_self1 = plus_arg1;
    create_self6 = ((STR) NULL);
    create_c1 = str_self1;
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
    r2 = L23;
    SARR((STR)r2,0,create_c1); 
    ;
    ret_val25 = r2;
    ret_val24 = ret_val25;
    s1 = ret_val24;
    ret_val23 = STR_ap1077157958(plus_self9, s1, TRUE);
    err_s1 = ret_val23;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   fetch_self1 = match_self1;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  }
  SATTR(this1,e_list,PARSER1134972014(self, FALSE));
  match_self2 = self;
  match_t2 = PARSER_then_tok;
  is_eq_self2 = ATTR(match_self2,next);
  is_eq_y2 = match_t2;
  L28 = is_eq_self2.val1;
  L291_=(L28)==(is_eq_y2); 
  ret_val26 = L291_;
  L30 = ret_val26;
  L311_=!(L30); 
  if (L311_) {
   exp_error_self2 = match_self2;
   create_self7 = TOKEN_zero;
   create_val2 = match_t2;
   t2.val1 = create_val2;
   ret_val27 = t2;
   exp_error_msg2 = TOKEN_strrSTR(ret_val27);
   error_self2 = exp_error_self2;
   plus_self10 = exp_error_msg2;
   plus_sarg8 = ((STR) &expectedbutfound);
   ret_val28 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val28;
   plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
   ret_val29 = STR_ap2004550586(plus_self11, plus_sarg9);
   error_msg2 = ret_val29;
   set_eloc_self2 = ATTR(error_self2,prog);
   source_loc_self4 = error_self2;
   ret_val30 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l2 = ret_val30;
   SATTR(set_eloc_self2,eloc,set_eloc_l2);
   err_self2 = error_self2;
   plus_self12 = error_msg2;
   plus_sarg10 = ((STR) &in1);
   ret_val31 = STR_ap2004550586(plus_self12, plus_sarg10);
   plus_self13 = ret_val31;
   plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
   ret_val32 = STR_ap2004550586(plus_self13, plus_sarg11);
   plus_self14 = ret_val32;
   plus_arg2 = ')';
   str_self2 = plus_arg2;
   create_self8 = ((STR) NULL);
   create_c2 = str_self2;
   L33 = 1;
   L35=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L33)*sizeof(CHAR);
   L34=ZALLOC_LEAF_BIG(L35);
   if (L34==NULL) FATAL("Unable to allocate more memory");
   memset(L34,0,L35);
   ((OB)L34)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L34)->header.destroyed=0;
#endif

   L32 = ((STR) L34);
   L32->asize = L33;
   r3 = L32;
   SARR((STR)r3,0,create_c2); 
   ;
   ret_val35 = r3;
   ret_val34 = ret_val35;
   s2 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self14, s2, TRUE);
   err_s2 = ret_val33;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
  fetch_self2 = match_self2;
  SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
  SATTR(this1,then_part,PARSER680097147(self));
  with_guard = FALSE;
  if (PARSER2128430730(self, PARSER_guard_tok)) {
   with_guard = TRUE;
  }
  if (only_one) {
   L37 = TRUE;
  } else {
   if (with_guard) {
    L38 = TRUE;
   } else {
    L38 = PARSER2128430730(self, PARSER_when_tok);
   }
   L39 = L38;
   L401_=!(L39); 
   L37 = L401_;
  }
  if (L37) {
   goto after_loop;
  }
 }
 after_loop: ;
 if (PARSER2128430730(self, PARSER_else_tok)) {
  SATTR(res,else_part,PARSER680097147(self));
  SATTR(res,no_else,FALSE);
 }
 else {
  SATTR(res,no_else,TRUE);
 }
 match_self3 = self;
 match_t3 = PARSER_end_tok;
 is_eq_self3 = ATTR(match_self3,next);
 is_eq_y3 = match_t3;
 L42 = is_eq_self3.val1;
 L431_=(L42)==(is_eq_y3); 
 ret_val36 = L431_;
 L44 = ret_val36;
 L451_=!(L44); 
 if (L451_) {
  exp_error_self3 = match_self3;
  create_self9 = TOKEN_zero;
  create_val3 = match_t3;
  t3.val1 = create_val3;
  ret_val37 = t3;
  exp_error_msg3 = TOKEN_strrSTR(ret_val37);
  error_self3 = exp_error_self3;
  plus_self15 = exp_error_msg3;
  plus_sarg12 = ((STR) &expectedbutfound);
  ret_val38 = STR_ap2004550586(plus_self15, plus_sarg12);
  plus_self16 = ret_val38;
  plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
  ret_val39 = STR_ap2004550586(plus_self16, plus_sarg13);
  error_msg3 = ret_val39;
  set_eloc_self3 = ATTR(error_self3,prog);
  source_loc_self5 = error_self3;
  ret_val40 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l3 = ret_val40;
  SATTR(set_eloc_self3,eloc,set_eloc_l3);
  err_self3 = error_self3;
  plus_self17 = error_msg3;
  plus_sarg14 = ((STR) &in1);
  ret_val41 = STR_ap2004550586(plus_self17, plus_sarg14);
  plus_self18 = ret_val41;
  plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
  ret_val42 = STR_ap2004550586(plus_self18, plus_sarg15);
  plus_self19 = ret_val42;
  plus_arg3 = ')';
  str_self3 = plus_arg3;
  create_self10 = ((STR) NULL);
  create_c3 = str_self3;
  L47 = 1;
  L49=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L47)*sizeof(CHAR);
  L48=ZALLOC_LEAF_BIG(L49);
  if (L48==NULL) FATAL("Unable to allocate more memory");
  memset(L48,0,L49);
  ((OB)L48)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L48)->header.destroyed=0;
#endif

  L46 = ((STR) L48);
  L46->asize = L47;
  r4 = L46;
  SARR((STR)r4,0,create_c3); 
  ;
  ret_val45 = r4;
  ret_val44 = ret_val45;
  s3 = ret_val44;
  ret_val43 = STR_ap1077157958(plus_self19, s3, TRUE);
  err_s3 = ret_val43;
  PROG_err_STR(ATTR(err_self3,prog), err_s3);
 }
 fetch_self3 = match_self3;
 SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_LOOP_STMT PARSER931477965(PARSER self) {
 AS_LOOP_STMT ret_val;
 AS_LOOP_STMT res;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_LOOP_STMT create_self2;
 AS_LOOP_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
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
 PARSER exit_self;
 STR s2;
 extern STR loopstatement;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_LOOP_STMT L10;
 OB L11;
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
 enter_self = self;
 enter_s = ((STR) &loopstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER_loop_tok;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_LOOP_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_LOOP_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_LOOP_STMT_tag;
 L10 = ((AS_LOOP_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 SATTR(res,body,PARSER680097147(self));
 match_self1 = self;
 match_t1 = PARSER_end_tok;
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
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_PAR_STMT PARSER1651016500(PARSER self) {
 AS_PAR_STMT ret_val;
 AS_PAR_STMT res;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_PAR_STMT create_self2;
 AS_PAR_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
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
 PARSER exit_self;
 STR s2;
 extern STR parstatement;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_PAR_STMT L10;
 OB L11;
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
 enter_self = self;
 enter_s = ((STR) &parstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER_par_tok;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_PAR_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_PAR_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_PAR_STMT_tag;
 L10 = ((AS_PAR_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 SATTR(res,body,PARSER680097147(self));
 match_self1 = self;
 match_t1 = PARSER_end_tok;
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
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_PAR_STMT PARSER2032164458(PARSER self) {
 AS_PAR_STMT ret_val;
 AS_PAR_STMT res;
 AS_LOOP_STMT lp;
 AS_FORK_STMT forkt;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_PAR_STMT create_self2;
 AS_PAR_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 AS_LOOP_STMT create_self3;
 AS_LOOP_STMT ret_val13;
 PARSER source_loc_self2;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 AS_STMT_LIST create_self4;
 AS_STMT_LIST ret_val15;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val16 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val17 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val18;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val19;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self3;
 SFILE_ID ret_val20 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val21;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val22;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val23;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val24;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val25;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 AS_FORK_STMT create_self7;
 AS_FORK_STMT ret_val26;
 PARSER source_loc_self4;
 SFILE_ID ret_val27 = SFILE_ID_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val28 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val29 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val30;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val31;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self5;
 SFILE_ID ret_val32 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val33;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val34;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val35;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val36;
 STR create_self9;
 CHAR create_c2 = CHAR_zero;
 STR ret_val37;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER exit_self;
 STR s3;
 extern STR parloopstatement;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_PAR_STMT L10;
 OB L11;
 AS_LOOP_STMT L12;
 OB L13;
 AS_STMT_LIST L14;
 OB L15;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 AS_FORK_STMT L26;
 OB L27;
 INT L28;
 BOOL L291_;
 BOOL L30;
 BOOL L311_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L32;
 INT L33;
 OB L34;
 INT L35;
 dAS_STMT L37;
 enter_self = self;
 enter_s = ((STR) &parloopstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER998808835;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_PAR_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_PAR_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_PAR_STMT_tag;
 L10 = ((AS_PAR_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 create_self3 = ((AS_LOOP_STMT) NULL);
 L13=ZALLOC(sizeof(struct AS_LOOP_STMT_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=AS_LOOP_STMT_tag;
 L12 = ((AS_LOOP_STMT) L13);
 ret_val13 = L12;
 lp = ret_val13;
 source_loc_self2 = self;
 ret_val14 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(lp,source1,ret_val14);
 create_self4 = ((AS_STMT_LIST) NULL);
 L15=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=AS_STMT_LIST_tag;
 L14 = ((AS_STMT_LIST) L15);
 ret_val15 = L14;
 SATTR(res,body,ret_val15);
 SATTR(lp,surr_stmt_list,ATTR(res,body));
 SATTR(ATTR(res,body),stmts,((dAS_STMT) lp));
 SATTR(lp,body,PARSER680097147(self));
 match_self1 = self;
 match_t1 = PARSER_do_tok;
 is_eq_self1 = ATTR(match_self1,next);
 is_eq_y1 = match_t1;
 L16 = is_eq_self1.val1;
 L171_=(L16)==(is_eq_y1); 
 ret_val16 = L171_;
 L18 = ret_val16;
 L191_=!(L18); 
 if (L191_) {
  exp_error_self1 = match_self1;
  create_self5 = TOKEN_zero;
  create_val1 = match_t1;
  t1.val1 = create_val1;
  ret_val17 = t1;
  exp_error_msg1 = TOKEN_strrSTR(ret_val17);
  error_self1 = exp_error_self1;
  plus_self5 = exp_error_msg1;
  plus_sarg4 = ((STR) &expectedbutfound);
  ret_val18 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val18;
  plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val19 = STR_ap2004550586(plus_self6, plus_sarg5);
  error_msg1 = ret_val19;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self3 = error_self1;
  ret_val20 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val20;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self7 = error_msg1;
  plus_sarg6 = ((STR) &in1);
  ret_val21 = STR_ap2004550586(plus_self7, plus_sarg6);
  plus_self8 = ret_val21;
  plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val22 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val22;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self6 = ((STR) NULL);
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

  L20 = ((STR) L23);
  L20->asize = L22;
  r1 = L20;
  SARR((STR)r1,0,create_c1); 
  ;
  ret_val25 = r1;
  ret_val24 = ret_val25;
  s1 = ret_val24;
  ret_val23 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val23;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
 }
 fetch_self1 = match_self1;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 create_self7 = ((AS_FORK_STMT) NULL);
 L27=ZALLOC(sizeof(struct AS_FORK_STMT_struct));
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=AS_FORK_STMT_tag;
 L26 = ((AS_FORK_STMT) L27);
 ret_val26 = L26;
 forkt = ret_val26;
 SATTR(forkt,surr_stmt_list,ATTR(lp,body));
 source_loc_self4 = self;
 ret_val27 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(forkt,source1,ret_val27);
 if (PARSER2128430730(self, PARSER_at_tok)) {
  SATTR(forkt,at,PARSER403674127(self));
 }
 SATTR(forkt,body,PARSER680097147(self));
 match_self2 = self;
 match_t2 = PARSER_end_tok;
 is_eq_self2 = ATTR(match_self2,next);
 is_eq_y2 = match_t2;
 L28 = is_eq_self2.val1;
 L291_=(L28)==(is_eq_y2); 
 ret_val28 = L291_;
 L30 = ret_val28;
 L311_=!(L30); 
 if (L311_) {
  exp_error_self2 = match_self2;
  create_self8 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val29 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val29);
  error_self2 = exp_error_self2;
  plus_self10 = exp_error_msg2;
  plus_sarg8 = ((STR) &expectedbutfound);
  ret_val30 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val30;
  plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val31 = STR_ap2004550586(plus_self11, plus_sarg9);
  error_msg2 = ret_val31;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self5 = error_self2;
  ret_val32 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val32;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self12 = error_msg2;
  plus_sarg10 = ((STR) &in1);
  ret_val33 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val33;
  plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val34 = STR_ap2004550586(plus_self13, plus_sarg11);
  plus_self14 = ret_val34;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self9 = ((STR) NULL);
  create_c2 = str_self2;
  L33 = 1;
  L35=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L33)*sizeof(CHAR);
  L34=ZALLOC_LEAF_BIG(L35);
  if (L34==NULL) FATAL("Unable to allocate more memory");
  memset(L34,0,L35);
  ((OB)L34)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L34)->header.destroyed=0;
#endif

  L32 = ((STR) L34);
  L32->asize = L33;
  r2 = L32;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val37 = r2;
  ret_val36 = ret_val37;
  s2 = ret_val36;
  ret_val35 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val35;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self2 = match_self2;
 SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 if ((ATTR(ATTR(lp,body),stmts)==((dAS_STMT) NULL))) {
  SATTR(ATTR(lp,body),stmts,((dAS_STMT) forkt));
 }
 else {
  L37 = ATTR(ATTR(lp,body),stmts);
  (*dAS_ST1454156204[TAG(L37)])(L37, ((dAS_STMT) forkt));
 }
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_PROTECT_STMT PARSER1254758060(PARSER self) {
 AS_PROTECT_STMT ret_val;
 AS_PROTECT_STMT res;
 AS_PROTECT_WHEN first1 = ((AS_PROTECT_WHEN) NULL);
 AS_PROTECT_WHEN this1 = ((AS_PROTECT_WHEN) NULL);
 AS_STMT_LIST st;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_PROTECT_STMT create_self2;
 AS_PROTECT_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 AS_PROTECT_WHEN create_self3;
 AS_PROTECT_WHEN ret_val13;
 PARSER source_loc_self2;
 SFILE_ID ret_val14 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val16 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self3;
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
 STR create_self5;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val25 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val26 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val27;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val28;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self4;
 SFILE_ID ret_val29 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val30;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val31;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val32;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val33;
 STR create_self7;
 CHAR create_c2 = CHAR_zero;
 STR ret_val34;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER exit_self;
 STR s3;
 extern STR protectstatement;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_PROTECT_STMT L10;
 OB L11;
 AS_PROTECT_WHEN L12;
 OB L13;
 INT L14;
 BOOL L151_;
 BOOL L16;
 BOOL L171_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L18;
 INT L19;
 OB L20;
 INT L22;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 enter_self = self;
 enter_s = ((STR) &protectstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER1281839191;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_PROTECT_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_PROTECT_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_PROTECT_STMT_tag;
 L10 = ((AS_PROTECT_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 SATTR(res,body,PARSER680097147(self));
 while (1) {
  if (PARSER2128430730(self, PARSER_when_tok)) {
  }
  else {
   goto after_loop;
  }
  first1 = ((AS_PROTECT_WHEN) NULL);
  while (1) {
   create_self3 = ((AS_PROTECT_WHEN) NULL);
   L13=ZALLOC(sizeof(struct AS_PROTECT_WHEN_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=AS_PROTECT_WHEN_tag;
   L12 = ((AS_PROTECT_WHEN) L13);
   ret_val13 = L12;
   this1 = ret_val13;
   source_loc_self2 = self;
   ret_val14 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(this1,source1,ret_val14);
   if ((first1==((AS_PROTECT_WHEN) NULL))) {
    first1 = this1;
   }
   SATTR(this1,tp,PARSER2028498823(self));
   if ((ATTR(res,when_part)==((AS_PROTECT_WHEN) NULL))) {
    SATTR(res,when_part,this1);
   }
   else {
    AS_PRO1103329709(ATTR(res,when_part), this1);
   }
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop1;
   }
  }
  after_loop1: ;
  match_self1 = self;
  match_t1 = PARSER_then_tok;
  is_eq_self1 = ATTR(match_self1,next);
  is_eq_y1 = match_t1;
  L14 = is_eq_self1.val1;
  L151_=(L14)==(is_eq_y1); 
  ret_val15 = L151_;
  L16 = ret_val15;
  L171_=!(L16); 
  if (L171_) {
   exp_error_self1 = match_self1;
   create_self4 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val16 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val16);
   error_self1 = exp_error_self1;
   plus_self5 = exp_error_msg1;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val18;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self3 = error_self1;
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
   create_self5 = ((STR) NULL);
   create_c1 = str_self1;
   L19 = 1;
   L22=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L19)*sizeof(CHAR);
   L20=ZALLOC_LEAF_BIG(L22);
   if (L20==NULL) FATAL("Unable to allocate more memory");
   memset(L20,0,L22);
   ((OB)L20)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L20)->header.destroyed=0;
#endif

   L18 = ((STR) L20);
   L18->asize = L19;
   r1 = L18;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self1 = match_self1;
  SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  st = PARSER680097147(self);
  this1 = first1;
  while (1) {
   if ((this1==((AS_PROTECT_WHEN) NULL))) {
    goto after_loop2;
   }
   SATTR(this1,then_part,st);
   this1 = ATTR(this1,next);
  }
  after_loop2: ;
 }
 after_loop: ;
 if (PARSER2128430730(self, PARSER_else_tok)) {
  SATTR(res,else_part,PARSER680097147(self));
  SATTR(res,no_else,FALSE);
 }
 else {
  SATTR(res,no_else,TRUE);
 }
 match_self2 = self;
 match_t2 = PARSER_end_tok;
 is_eq_self2 = ATTR(match_self2,next);
 is_eq_y2 = match_t2;
 L24 = is_eq_self2.val1;
 L251_=(L24)==(is_eq_y2); 
 ret_val25 = L251_;
 L26 = ret_val25;
 L271_=!(L26); 
 if (L271_) {
  exp_error_self2 = match_self2;
  create_self6 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val26 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val26);
  error_self2 = exp_error_self2;
  plus_self10 = exp_error_msg2;
  plus_sarg8 = ((STR) &expectedbutfound);
  ret_val27 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val27;
  plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val28 = STR_ap2004550586(plus_self11, plus_sarg9);
  error_msg2 = ret_val28;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self4 = error_self2;
  ret_val29 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val29;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self12 = error_msg2;
  plus_sarg10 = ((STR) &in1);
  ret_val30 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val30;
  plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val31 = STR_ap2004550586(plus_self13, plus_sarg11);
  plus_self14 = ret_val31;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self7 = ((STR) NULL);
  create_c2 = str_self2;
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
  ret_val34 = r2;
  ret_val33 = ret_val34;
  s2 = ret_val33;
  ret_val32 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val32;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self2 = match_self2;
 SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_RETURN_STMT PARSER1796842837(PARSER self) {
 AS_RETURN_STMT ret_val;
 AS_RETURN_STMT res;
 PARSER enter_self;
 STR enter_s;
 AS_RETURN_STMT create_self;
 AS_RETURN_STMT ret_val1;
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
 PARSER exit_self;
 STR s1;
 extern STR returnstatement;
 AS_RETURN_STMT L1;
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
 enter_self = self;
 enter_s = ((STR) &returnstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_RETURN_STMT) NULL);
 L2=ZALLOC(sizeof(struct AS_RETURN_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_RETURN_STMT_tag;
 L1 = ((AS_RETURN_STMT) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 match_self = self;
 match_t = PARSER729653115;
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
 if (PARSER1429651936(self, ATTR(self,next))) {
  SATTR(res,val1,PARSER403674127(self));
 }
 exit_self = self;
 s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_TYPECASE_STMT PARSER670949263(PARSER self) {
 AS_TYPECASE_STMT ret_val;
 AS_TYPECASE_STMT res;
 AS_TYPECASE_WHEN this1;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_TYPECASE_STMT create_self2;
 AS_TYPECASE_STMT ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
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
 PARSER ident_self;
 IDENT ret_val23 = IDENT_zero;
 AS_TYPECASE_WHEN create_self5;
 AS_TYPECASE_WHEN ret_val24;
 PARSER source_loc_self3;
 SFILE_ID ret_val25 = SFILE_ID_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val28;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val29;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self4;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val32;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val33;
 STR s2;
 CHAR str_self2 = CHAR_zero;
 STR ret_val34;
 STR create_self7;
 CHAR create_c2 = CHAR_zero;
 STR ret_val35;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self2;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val36 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val37 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val38;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val39;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self5;
 SFILE_ID ret_val40 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val41;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val42;
 STR plus_self19;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val43;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self9;
 CHAR create_c3 = CHAR_zero;
 STR ret_val45;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self3;
 PARSER exit_self;
 STR s4;
 extern STR typeca1386590395;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_TYPECASE_STMT L10;
 OB L11;
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
 AS_TYPECASE_WHEN L22;
 OB L23;
 INT L24;
 BOOL L251_;
 BOOL L26;
 BOOL L271_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L28;
 INT L29;
 OB L30;
 INT L31;
 INT L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L37;
 INT L38;
 OB L39;
 INT L40;
 enter_self = self;
 enter_s = ((STR) &typeca1386590395);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER2033525488;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 create_self2 = ((AS_TYPECASE_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_TYPECASE_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_TYPECASE_STMT_tag;
 L10 = ((AS_TYPECASE_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 match_self1 = self;
 match_t1 = PARSER_ident_tok;
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
 ident_self = self;
 ret_val23 = ATTR(ATTR(ident_self,scanner),lex_value);
 SATTR(res,name1,ret_val23);
 while (1) {
  if (PARSER2128430730(self, PARSER_when_tok)) {
  }
  else {
   goto after_loop;
  }
  create_self5 = ((AS_TYPECASE_WHEN) NULL);
  L23=ZALLOC(sizeof(struct AS_TYPECASE_WHEN_struct));
  if (L23==NULL) FATAL("Unable to allocate more memory");
  ((OB)L23)->header.tag=AS_TYPECASE_WHEN_tag;
  L22 = ((AS_TYPECASE_WHEN) L23);
  ret_val24 = L22;
  this1 = ret_val24;
  source_loc_self3 = self;
  ret_val25 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(this1,source1,ret_val25);
  SATTR(this1,tp,PARSER2028498823(self));
  match_self2 = self;
  match_t2 = PARSER_then_tok;
  is_eq_self2 = ATTR(match_self2,next);
  is_eq_y2 = match_t2;
  L24 = is_eq_self2.val1;
  L251_=(L24)==(is_eq_y2); 
  ret_val26 = L251_;
  L26 = ret_val26;
  L271_=!(L26); 
  if (L271_) {
   exp_error_self2 = match_self2;
   create_self6 = TOKEN_zero;
   create_val2 = match_t2;
   t2.val1 = create_val2;
   ret_val27 = t2;
   exp_error_msg2 = TOKEN_strrSTR(ret_val27);
   error_self2 = exp_error_self2;
   plus_self10 = exp_error_msg2;
   plus_sarg8 = ((STR) &expectedbutfound);
   ret_val28 = STR_ap2004550586(plus_self10, plus_sarg8);
   plus_self11 = ret_val28;
   plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
   ret_val29 = STR_ap2004550586(plus_self11, plus_sarg9);
   error_msg2 = ret_val29;
   set_eloc_self2 = ATTR(error_self2,prog);
   source_loc_self4 = error_self2;
   ret_val30 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l2 = ret_val30;
   SATTR(set_eloc_self2,eloc,set_eloc_l2);
   err_self2 = error_self2;
   plus_self12 = error_msg2;
   plus_sarg10 = ((STR) &in1);
   ret_val31 = STR_ap2004550586(plus_self12, plus_sarg10);
   plus_self13 = ret_val31;
   plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
   ret_val32 = STR_ap2004550586(plus_self13, plus_sarg11);
   plus_self14 = ret_val32;
   plus_arg2 = ')';
   str_self2 = plus_arg2;
   create_self7 = ((STR) NULL);
   create_c2 = str_self2;
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
   ret_val35 = r2;
   ret_val34 = ret_val35;
   s2 = ret_val34;
   ret_val33 = STR_ap1077157958(plus_self14, s2, TRUE);
   err_s2 = ret_val33;
   PROG_err_STR(ATTR(err_self2,prog), err_s2);
  }
  fetch_self2 = match_self2;
  SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
  SATTR(this1,then_part,PARSER680097147(self));
  if ((ATTR(res,when_part)==((AS_TYPECASE_WHEN) NULL))) {
   SATTR(res,when_part,this1);
  }
  else {
   AS_TYP1805639138(ATTR(res,when_part), this1);
  }
 }
 after_loop: ;
 if (PARSER2128430730(self, PARSER_else_tok)) {
  SATTR(res,else_part,PARSER680097147(self));
  SATTR(res,no_else,FALSE);
 }
 else {
  SATTR(res,no_else,TRUE);
 }
 match_self3 = self;
 match_t3 = PARSER_end_tok;
 is_eq_self3 = ATTR(match_self3,next);
 is_eq_y3 = match_t3;
 L33 = is_eq_self3.val1;
 L341_=(L33)==(is_eq_y3); 
 ret_val36 = L341_;
 L35 = ret_val36;
 L361_=!(L35); 
 if (L361_) {
  exp_error_self3 = match_self3;
  create_self8 = TOKEN_zero;
  create_val3 = match_t3;
  t3.val1 = create_val3;
  ret_val37 = t3;
  exp_error_msg3 = TOKEN_strrSTR(ret_val37);
  error_self3 = exp_error_self3;
  plus_self15 = exp_error_msg3;
  plus_sarg12 = ((STR) &expectedbutfound);
  ret_val38 = STR_ap2004550586(plus_self15, plus_sarg12);
  plus_self16 = ret_val38;
  plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
  ret_val39 = STR_ap2004550586(plus_self16, plus_sarg13);
  error_msg3 = ret_val39;
  set_eloc_self3 = ATTR(error_self3,prog);
  source_loc_self5 = error_self3;
  ret_val40 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l3 = ret_val40;
  SATTR(set_eloc_self3,eloc,set_eloc_l3);
  err_self3 = error_self3;
  plus_self17 = error_msg3;
  plus_sarg14 = ((STR) &in1);
  ret_val41 = STR_ap2004550586(plus_self17, plus_sarg14);
  plus_self18 = ret_val41;
  plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
  ret_val42 = STR_ap2004550586(plus_self18, plus_sarg15);
  plus_self19 = ret_val42;
  plus_arg3 = ')';
  str_self3 = plus_arg3;
  create_self9 = ((STR) NULL);
  create_c3 = str_self3;
  L38 = 1;
  L40=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L38)*sizeof(CHAR);
  L39=ZALLOC_LEAF_BIG(L40);
  if (L39==NULL) FATAL("Unable to allocate more memory");
  memset(L39,0,L40);
  ((OB)L39)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L39)->header.destroyed=0;
#endif

  L37 = ((STR) L39);
  L37->asize = L38;
  r3 = L37;
  SARR((STR)r3,0,create_c3); 
  ;
  ret_val45 = r3;
  ret_val44 = ret_val45;
  s3 = ret_val44;
  ret_val43 = STR_ap1077157958(plus_self19, s3, TRUE);
  err_s3 = ret_val43;
  PROG_err_STR(ATTR(err_self3,prog), err_s3);
 }
 fetch_self3 = match_self3;
 SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_YIELD_STMT PARSER1414885906(PARSER self) {
 AS_YIELD_STMT ret_val;
 AS_YIELD_STMT res;
 PARSER enter_self;
 STR enter_s;
 AS_YIELD_STMT create_self;
 AS_YIELD_STMT ret_val1;
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
 PARSER exit_self;
 STR s1;
 extern STR yieldstatement;
 AS_YIELD_STMT L1;
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
 enter_self = self;
 enter_s = ((STR) &yieldstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_YIELD_STMT) NULL);
 L2=ZALLOC(sizeof(struct AS_YIELD_STMT_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_YIELD_STMT_tag;
 L1 = ((AS_YIELD_STMT) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 match_self = self;
 match_t = PARSER_yield_tok;
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
 if (PARSER1429651936(self, ATTR(self,next))) {
  SATTR(res,val1,PARSER403674127(self));
 }
 exit_self = self;
 s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL PARSER1152230473(PARSER self, TOKEN t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 L11 = t.val1;
 switch (L11) {
  case 2: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 17: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 22: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 52: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 29: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 33: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 8: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 42: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 5: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 28: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 30: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 51: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 43: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 7: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 94: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 99: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 100: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 104: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 96: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 97: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 101: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 102: 
   ret_val = TRUE;
   return ret_val;
   break;
  default: ;
   ret_val = PARSER1429651936(self, t);
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL PARSER1429651936(PARSER self, TOKEN t) {
 BOOL ret_val = BOOL_zero;
 INT L11 = INT_zero;
 L11 = t.val1;
 switch (L11) {
  case 37: 
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
  case 36: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 49: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 69: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 87: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 23: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 74: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 79: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 14: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 19: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 32: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 51: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 43: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 7: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 40: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 16: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 56: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 55: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 53: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 54: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 57: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 59: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 111: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 112: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 105: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 106: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 92: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 109: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 93: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 101: 
   ret_val = TRUE;
   return ret_val;
   break;
  case 113: 
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

dAS_CLASS_ELT PARSER1594550222(PARSER self, TOKEN mode1) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
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
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val18 = BOOL_zero;
 PARSER exit_self;
 STR s2;
 extern STR stubfe1671934871;
 INT L1;
 BOOL L21_;
 extern STR readon1548835488;
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
 INT L17;
 BOOL L181_;
 dAS_CLASS_ELT L19;
 enter_self = self;
 enter_s = ((STR) &stubfe1671934871);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = mode1;
 is_eq_y = PARSER550831344;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  error_self = self;
  error_msg = ((STR) &readon1548835488);
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
 match_t = PARSER_stub_tok;
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
 res = ((dAS_CLASS_ELT) PARSER2039100746(self));
 is_eq_self2 = mode1;
 is_eq_y2 = PARSER1019557737;
 L17 = is_eq_self2.val1;
 L181_=(L17)==(is_eq_y2); 
 ret_val18 = L181_;
 L19 = res;
 (*dAS_CL1134203643[TAG(L19)])(L19, ret_val18);
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT PARSER1886860611(PARSER self, TOKEN mode1) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 AS_ATTR_DEF newid;
 AS_TYPE_SPEC tp;
 dAS_CLASS_ELT p;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_ATTR_DEF create_self2;
 AS_ATTR_DEF ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val13 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val14 = BOOL_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN create_self3 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val16 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
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
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER ident_self;
 IDENT ret_val25 = IDENT_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val28;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val29;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val32;
 STR plus_self14;
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
 PARSER fetch_self2;
 PARSER exit_self;
 STR s3;
 extern STR attrib1611516992;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_ATTR_DEF L10;
 OB L11;
 INT L12;
 BOOL L131_;
 INT L14;
 BOOL L151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 dAS_CLASS_ELT L26;
 INT L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L31;
 INT L32;
 OB L33;
 INT L34;
 dAS_CLASS_ELT L36;
 enter_self = self;
 enter_s = ((STR) &attrib1611516992);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER_attr_tok;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 while (1) {
  create_self2 = ((AS_ATTR_DEF) NULL);
  L11=ZALLOC(sizeof(struct AS_ATTR_DEF_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=AS_ATTR_DEF_tag;
  L10 = ((AS_ATTR_DEF) L11);
  ret_val11 = L10;
  newid = ret_val11;
  source_loc_self1 = self;
  ret_val12 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(newid,source1,ret_val12);
  is_eq_self1 = mode1;
  is_eq_y1 = PARSER1019557737;
  L12 = is_eq_self1.val1;
  L131_=(L12)==(is_eq_y1); 
  ret_val13 = L131_;
  SATTR(newid,is_private,ret_val13);
  is_eq_self2 = mode1;
  is_eq_y2 = PARSER550831344;
  L14 = is_eq_self2.val1;
  L151_=(L14)==(is_eq_y2); 
  ret_val14 = L151_;
  SATTR(newid,is_readonly,ret_val14);
  match_self1 = self;
  match_t1 = PARSER_ident_tok;
  is_eq_self3 = ATTR(match_self1,next);
  is_eq_y3 = match_t1;
  L16 = is_eq_self3.val1;
  L171_=(L16)==(is_eq_y3); 
  ret_val15 = L171_;
  L18 = ret_val15;
  L191_=!(L18); 
  if (L191_) {
   exp_error_self1 = match_self1;
   create_self3 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val16 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val16);
   error_self1 = exp_error_self1;
   plus_self5 = exp_error_msg1;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val18;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self2 = error_self1;
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
   create_self4 = ((STR) NULL);
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

   L20 = ((STR) L23);
   L20->asize = L22;
   r1 = L20;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self1 = match_self1;
  SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  ident_self = self;
  ret_val25 = ATTR(ATTR(ident_self,scanner),lex_value);
  SATTR(newid,name1,ret_val25);
  if ((res==((dAS_CLASS_ELT) NULL))) {
   res = ((dAS_CLASS_ELT) newid);
  }
  else {
   L26 = res;
   (*dAS_CL1056554733[TAG(L26)])(L26, ((dAS_CLASS_ELT) newid));
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 match_self2 = self;
 match_t2 = PARSER_colon_tok;
 is_eq_self4 = ATTR(match_self2,next);
 is_eq_y4 = match_t2;
 L27 = is_eq_self4.val1;
 L281_=(L27)==(is_eq_y4); 
 ret_val26 = L281_;
 L29 = ret_val26;
 L301_=!(L29); 
 if (L301_) {
  exp_error_self2 = match_self2;
  create_self5 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val27 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val27);
  error_self2 = exp_error_self2;
  plus_self10 = exp_error_msg2;
  plus_sarg8 = ((STR) &expectedbutfound);
  ret_val28 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val28;
  plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val29 = STR_ap2004550586(plus_self11, plus_sarg9);
  error_msg2 = ret_val29;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self3 = error_self2;
  ret_val30 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val30;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self12 = error_msg2;
  plus_sarg10 = ((STR) &in1);
  ret_val31 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val31;
  plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val32 = STR_ap2004550586(plus_self13, plus_sarg11);
  plus_self14 = ret_val32;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self6 = ((STR) NULL);
  create_c2 = str_self2;
  L32 = 1;
  L34=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L32)*sizeof(CHAR);
  L33=ZALLOC_LEAF_BIG(L34);
  if (L33==NULL) FATAL("Unable to allocate more memory");
  memset(L33,0,L34);
  ((OB)L33)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L33)->header.destroyed=0;
#endif

  L31 = ((STR) L33);
  L31->asize = L32;
  r2 = L31;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val35 = r2;
  ret_val34 = ret_val35;
  s2 = ret_val34;
  ret_val33 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val33;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self2 = match_self2;
 SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 tp = PARSER2028498823(self);
 p = res;
 while (1) {
  if ((p==((dAS_CLASS_ELT) NULL))) {
   goto after_loop1;
  }
  switch (TAG(p)) {
   case AS_ATTR_DEF_tag:
    SATTR(((AS_ATTR_DEF) p),tp,tp); break;
   default: ;
    FATAL("No applicable type in typecase\nin PARSER::attr_def(TOKEN):$AS_CLASS_ELT\n./Front/parse.sa:836:17");
  }
  L36 = p;
  p = (*dAS_CL760354380[TAG(L36)])(L36);
 }
 after_loop1: ;
 exit_self = self;
 s3 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_CLASS_ELT PARSER252822802(PARSER self, TOKEN mode1) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 AS_SHARED_DEF newid;
 AS_TYPE_SPEC tp;
 dAS_CLASS_ELT p;
 PARSER enter_self;
 STR enter_s;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val1 = BOOL_zero;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val2 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val3;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val4;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val5 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val6;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val7;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val8;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val9;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val10;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self;
 AS_SHARED_DEF create_self2;
 AS_SHARED_DEF ret_val11;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val13 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val14 = BOOL_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val15 = BOOL_zero;
 TOKEN create_self3 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val16 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val17;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val18;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self2;
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
 STR create_self4;
 CHAR create_c1 = CHAR_zero;
 STR ret_val24;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self1;
 PARSER ident_self;
 IDENT ret_val25 = IDENT_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val27 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val28;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val29;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self3;
 SFILE_ID ret_val30 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val31;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val32;
 STR plus_self14;
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
 PARSER fetch_self2;
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
 PARSER exit_self;
 STR s4;
 extern STR shareddefinition;
 INT L1;
 BOOL L21_;
 BOOL L3;
 BOOL L41_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L5;
 INT L6;
 OB L7;
 INT L8;
 AS_SHARED_DEF L10;
 OB L11;
 INT L12;
 BOOL L131_;
 INT L14;
 BOOL L151_;
 INT L16;
 BOOL L171_;
 BOOL L18;
 BOOL L191_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L20;
 INT L22;
 OB L23;
 INT L24;
 dAS_CLASS_ELT L26;
 INT L27;
 BOOL L281_;
 BOOL L29;
 BOOL L301_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L31;
 INT L32;
 OB L33;
 INT L34;
 dAS_CLASS_ELT L36;
 dAS_CLASS_ELT L37;
 BOOL L38;
 BOOL L391_;
 extern STR onlysi482809466;
 extern STR in1;
 STR L40;
 INT L42;
 OB L43;
 INT L44;
 enter_self = self;
 enter_s = ((STR) &shareddefinition);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER689908022;
 is_eq_self = ATTR(match_self,next);
 is_eq_y = match_t;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 L3 = ret_val1;
 L41_=!(L3); 
 if (L41_) {
  exp_error_self = match_self;
  create_self = TOKEN_zero;
  create_val = match_t;
  t.val1 = create_val;
  ret_val2 = t;
  exp_error_msg = TOKEN_strrSTR(ret_val2);
  error_self = exp_error_self;
  plus_self = exp_error_msg;
  plus_sarg = ((STR) &expectedbutfound);
  ret_val3 = STR_ap2004550586(plus_self, plus_sarg);
  plus_self1 = ret_val3;
  plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
  ret_val4 = STR_ap2004550586(plus_self1, plus_sarg1);
  error_msg = ret_val4;
  set_eloc_self = ATTR(error_self,prog);
  source_loc_self = error_self;
  ret_val5 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l = ret_val5;
  SATTR(set_eloc_self,eloc,set_eloc_l);
  err_self = error_self;
  plus_self2 = error_msg;
  plus_sarg2 = ((STR) &in1);
  ret_val6 = STR_ap2004550586(plus_self2, plus_sarg2);
  plus_self3 = ret_val6;
  plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
  ret_val7 = STR_ap2004550586(plus_self3, plus_sarg3);
  plus_self4 = ret_val7;
  plus_arg = ')';
  str_self = plus_arg;
  create_self1 = ((STR) NULL);
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
  ret_val10 = r;
  ret_val9 = ret_val10;
  s = ret_val9;
  ret_val8 = STR_ap1077157958(plus_self4, s, TRUE);
  err_s = ret_val8;
  PROG_err_STR(ATTR(err_self,prog), err_s);
 }
 fetch_self = match_self;
 SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 while (1) {
  create_self2 = ((AS_SHARED_DEF) NULL);
  L11=ZALLOC(sizeof(struct AS_SHARED_DEF_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=AS_SHARED_DEF_tag;
  L10 = ((AS_SHARED_DEF) L11);
  ret_val11 = L10;
  newid = ret_val11;
  source_loc_self1 = self;
  ret_val12 = SFILE_1594766561(SFILE_ID_zero);
  SATTR(newid,source1,ret_val12);
  is_eq_self1 = mode1;
  is_eq_y1 = PARSER1019557737;
  L12 = is_eq_self1.val1;
  L131_=(L12)==(is_eq_y1); 
  ret_val13 = L131_;
  SATTR(newid,is_private,ret_val13);
  is_eq_self2 = mode1;
  is_eq_y2 = PARSER550831344;
  L14 = is_eq_self2.val1;
  L151_=(L14)==(is_eq_y2); 
  ret_val14 = L151_;
  SATTR(newid,is_readonly,ret_val14);
  match_self1 = self;
  match_t1 = PARSER_ident_tok;
  is_eq_self3 = ATTR(match_self1,next);
  is_eq_y3 = match_t1;
  L16 = is_eq_self3.val1;
  L171_=(L16)==(is_eq_y3); 
  ret_val15 = L171_;
  L18 = ret_val15;
  L191_=!(L18); 
  if (L191_) {
   exp_error_self1 = match_self1;
   create_self3 = TOKEN_zero;
   create_val1 = match_t1;
   t1.val1 = create_val1;
   ret_val16 = t1;
   exp_error_msg1 = TOKEN_strrSTR(ret_val16);
   error_self1 = exp_error_self1;
   plus_self5 = exp_error_msg1;
   plus_sarg4 = ((STR) &expectedbutfound);
   ret_val17 = STR_ap2004550586(plus_self5, plus_sarg4);
   plus_self6 = ret_val17;
   plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
   ret_val18 = STR_ap2004550586(plus_self6, plus_sarg5);
   error_msg1 = ret_val18;
   set_eloc_self1 = ATTR(error_self1,prog);
   source_loc_self2 = error_self1;
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
   create_self4 = ((STR) NULL);
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

   L20 = ((STR) L23);
   L20->asize = L22;
   r1 = L20;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self9, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self1 = match_self1;
  SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
  ident_self = self;
  ret_val25 = ATTR(ATTR(ident_self,scanner),lex_value);
  SATTR(newid,name1,ret_val25);
  if ((res==((dAS_CLASS_ELT) NULL))) {
   res = ((dAS_CLASS_ELT) newid);
  }
  else {
   L26 = res;
   (*dAS_CL1056554733[TAG(L26)])(L26, ((dAS_CLASS_ELT) newid));
  }
  if (PARSER2128430730(self, PARSER_comma_tok)) {
  }
  else {
   goto after_loop;
  }
 }
 after_loop: ;
 match_self2 = self;
 match_t2 = PARSER_colon_tok;
 is_eq_self4 = ATTR(match_self2,next);
 is_eq_y4 = match_t2;
 L27 = is_eq_self4.val1;
 L281_=(L27)==(is_eq_y4); 
 ret_val26 = L281_;
 L29 = ret_val26;
 L301_=!(L29); 
 if (L301_) {
  exp_error_self2 = match_self2;
  create_self5 = TOKEN_zero;
  create_val2 = match_t2;
  t2.val1 = create_val2;
  ret_val27 = t2;
  exp_error_msg2 = TOKEN_strrSTR(ret_val27);
  error_self2 = exp_error_self2;
  plus_self10 = exp_error_msg2;
  plus_sarg8 = ((STR) &expectedbutfound);
  ret_val28 = STR_ap2004550586(plus_self10, plus_sarg8);
  plus_self11 = ret_val28;
  plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
  ret_val29 = STR_ap2004550586(plus_self11, plus_sarg9);
  error_msg2 = ret_val29;
  set_eloc_self2 = ATTR(error_self2,prog);
  source_loc_self3 = error_self2;
  ret_val30 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l2 = ret_val30;
  SATTR(set_eloc_self2,eloc,set_eloc_l2);
  err_self2 = error_self2;
  plus_self12 = error_msg2;
  plus_sarg10 = ((STR) &in1);
  ret_val31 = STR_ap2004550586(plus_self12, plus_sarg10);
  plus_self13 = ret_val31;
  plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
  ret_val32 = STR_ap2004550586(plus_self13, plus_sarg11);
  plus_self14 = ret_val32;
  plus_arg2 = ')';
  str_self2 = plus_arg2;
  create_self6 = ((STR) NULL);
  create_c2 = str_self2;
  L32 = 1;
  L34=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L32)*sizeof(CHAR);
  L33=ZALLOC_LEAF_BIG(L34);
  if (L33==NULL) FATAL("Unable to allocate more memory");
  memset(L33,0,L34);
  ((OB)L33)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L33)->header.destroyed=0;
#endif

  L31 = ((STR) L33);
  L31->asize = L32;
  r2 = L31;
  SARR((STR)r2,0,create_c2); 
  ;
  ret_val35 = r2;
  ret_val34 = ret_val35;
  s2 = ret_val34;
  ret_val33 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val33;
  PROG_err_STR(ATTR(err_self2,prog), err_s2);
 }
 fetch_self2 = match_self2;
 SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
 tp = PARSER2028498823(self);
 p = res;
 while (1) {
  if ((p==((dAS_CLASS_ELT) NULL))) {
   goto after_loop1;
  }
  switch (TAG(p)) {
   case AS_SHARED_DEF_tag:
    SATTR(((AS_SHARED_DEF) p),tp,tp); break;
   default: ;
    FATAL("No applicable type in typecase\nin PARSER::shared_def(TOKEN):$AS_CLASS_ELT\n./Front/parse.sa:803:17");
  }
  L36 = p;
  p = (*dAS_CL760354380[TAG(L36)])(L36);
 }
 after_loop1: ;
 if (PARSER2128430730(self, PARSER265941484)) {
  switch (TAG(res)) {
   case AS_SHARED_DEF_tag:
    SATTR(((AS_SHARED_DEF) res),init,PARSER403674127(self)); break;
   default: ;
    FATAL("No applicable type in typecase\nin PARSER::shared_def(TOKEN):$AS_CLASS_ELT\n./Front/parse.sa:807:17");
  }
  L37 = res;
  L38 = ((*dAS_CL760354380[TAG(L37)])(L37)==((dAS_CLASS_ELT) NULL));
  L391_=!(L38); 
  if (L391_) {
   error_self3 = self;
   error_msg3 = ((STR) &onlysi482809466);
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
   r3 = L40;
   SARR((STR)r3,0,create_c3); 
   ;
   ret_val41 = r3;
   ret_val40 = ret_val41;
   s3 = ret_val40;
   ret_val39 = STR_ap1077157958(plus_self17, s3, TRUE);
   err_s3 = ret_val39;
   PROG_err_STR(ATTR(err_self3,prog), err_s3);
  }
 }
 exit_self = self;
 s4 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}
