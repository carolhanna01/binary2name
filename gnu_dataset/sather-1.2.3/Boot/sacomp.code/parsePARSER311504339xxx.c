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

typedef struct AS_ARG_DEC_struct {/* layout for AS_ARG_DEC */
 OB_HEADER header;
 struct AS_ARG_DEC_struct *next;
 struct AS_ARG_MODE_struct *mode1;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_hot;
 } *AS_ARG_DEC;

typedef struct AS_ASSERT_STMT_struct {/* layout for AS_ASSERT_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *test1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_ASSERT_STMT;

typedef struct AS_ASSIGN_STMT_struct {/* layout for AS_ASSIGN_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *lhs_expr;
 struct dAS_EXPR_struct *rhs;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_ASSIGN_STMT;

typedef struct AS_ATTACH_STMT_struct {/* layout for AS_ATTACH_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *lhs;
 struct dAS_EXPR_struct *rhs;
 struct dAS_STMT_struct *next;
 struct AS_ROUT_DEF_struct *rout;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct helper_class;
 struct SFILE_ID_struct source1;
 BOOL dont_print;
 BOOL transformed;
 } *AS_ATTACH_STMT;

typedef struct AS_BREAK_EXPR_struct {/* layout for AS_BREAK_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_BREAK_EXPR;

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

typedef struct AS_DEC_STMT_struct {/* layout for AS_DEC_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_DEC_STMT;

typedef struct AS_EXPR_STMT_struct {/* layout for AS_EXPR_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_EXPR_STMT;

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

typedef struct AS_PARAM_DEC_struct {/* layout for AS_PARAM_DEC */
 OB_HEADER header;
 struct AS_PARAM_DEC_struct *next;
 struct AS_TYPE_SPEC_struct *type_constraint;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 } *AS_PARAM_DEC;

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

typedef struct AS_QUIT_STMT_struct {/* layout for AS_QUIT_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_QUIT_STMT;

typedef struct AS_RAISE_STMT_struct {/* layout for AS_RAISE_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_RAISE_STMT;

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

typedef struct AS_STMT_LIST_struct {/* layout for AS_STMT_LIST */
 OB_HEADER header;
 struct dAS_STMT_struct *stmts;
 struct SFILE_ID_struct source1;
 } *AS_STMT_LIST;

typedef struct AS_SYNC_STMT_struct {/* layout for AS_SYNC_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_SYNC_STMT;

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

typedef struct AS_UNLOCK_STMT_struct {/* layout for AS_UNLOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_UNLOCK_STMT;

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

extern INT AS_CLA1666707132;
extern INT AS_CLA2085573152;
extern INT AS_CLA86009053;
extern INT AS_CLASS_DEF_imm;
extern INT AS_CLASS_DEF_ref;
extern INT AS_CLASS_DEF_spr;
extern INT PARSER1019557737;
extern INT PARSER1207020061;
extern INT PARSER1209676627;
extern INT PARSER1338823514;
extern INT PARSER1998725038;
extern INT PARSER2001381604;
extern INT PARSER2011952041;
extern INT PARSER265941484;
extern INT PARSER550831344;
extern INT PARSER836044841;
extern INT PARSER978809429;
extern INT PARSER985900141;
extern INT PARSER_at_tok;
extern INT PARSER_class_tok;
extern INT PARSER_colon_tok;
extern INT PARSER_comma_tok;
extern INT PARSER_else_tok;
extern INT PARSER_end_tok;
extern INT PARSER_eof_tok;
extern INT PARSER_fork_tok;
extern INT PARSER_ident_tok;
extern INT PARSER_is_lt_tok;
extern INT PARSER_is_tok;
extern INT PARSER_near_tok;
extern INT PARSER_null_tok;
extern INT PARSER_post_tok;
extern INT PARSER_pre_tok;
extern INT PARSER_raise_tok;
extern INT PARSER_semi_tok;
extern INT PARSER_sync_tok;
extern INT PARSER_with_tok;
extern INT SFILE_ID_pos;

/* Function declarations */


extern RETURNED_CONST dAS_STMT (**dAS_ST460279343)(dAS_STMT);

extern RETURNED_CONST void (**dAS_CL1056554733)(dAS_CLASS_ELT, dAS_CLASS_ELT);

extern RETURNED_CONST void (**dAS_ST1190543153)(dAS_STMT, SFILE_ID);

extern RETURNED_CONST void (**dAS_ST1454156204)(dAS_STMT, dAS_STMT);

extern RETURNED_CONST void (**dAS_ST164863607)(dAS_STMT, AS_STMT_LIST);
AS_ARG_DEC PARSER13880807(PARSER, BOOL);
AS_CASE_STMT PARSER552741043(PARSER);
AS_CLASS_DEF PARSER525553873(PARSER);
AS_FORK_STMT PARSER2069741717(PARSER);
AS_IDENT_LIST PARSER1448067791(PARSER, dAS_NODE);
AS_IF_STMT PARSER1465700283(PARSER, dAS_EXPR, dAS_STMT, dAS_STMT);
AS_IF_STMT PARSER164187557(PARSER);
AS_LOCK_STMT PARSER1287362985(PARSER);
AS_LOOP_STMT PARSER931477965(PARSER);
AS_PARAM_DEC PARSER1105068487(PARSER);
AS_PAR_STMT PARSER1651016500(PARSER);
AS_PAR_STMT PARSER2032164458(PARSER);
AS_PROTECT_STMT PARSER1254758060(PARSER);
AS_RETURN_STMT PARSER1796842837(PARSER);
AS_ROUT_DEF PARSER1732860771(PARSER, TOKEN);
AS_STMT_LIST PARSER680097147(PARSER);
AS_TYPECASE_STMT PARSER670949263(PARSER);
AS_TYPE_SPEC PARSER2028498823(PARSER);
AS_TYPE_SPEC PARSER532091533(PARSER);
AS_WIT1063437351 PARSER1613260578(PARSER);
AS_YIELD_STMT PARSER1414885906(PARSER);
BOOL PARSER1152230473(PARSER, TOKEN);
BOOL PARSER1234876012(PARSER, TOKEN);
BOOL PARSER2128430730(PARSER, INT);
BOOL SCANNE363215919(SCANNER, STR);
BOOL STR_is111530248(STR, STR);
FLISTSTR FLISTS1425610062(FLISTSTR, STR);
IDENT PARSER2046006032(PARSER, dAS_EXPR);
IDENT PARSER814698330(PARSER);
INT STR_sizerINT(STR);
SFILE_ID SFILE_1594766561(SFILE_ID);
STR FLISTSTR_poprSTR(FLISTSTR);
STR FLISTSTR_toprSTR(FLISTSTR);
STR STR_ap1077157958(STR, STR, BOOL);
STR STR_ap2004550586(STR, STR);
STR TOKEN_strrSTR(TOKEN);
TOKEN SCANNE1792387248(SCANNER);
dAS_CLASS_ELT PARSER1583311295(PARSER);
dAS_CLASS_ELT PARSER1594550222(PARSER, TOKEN);
dAS_CLASS_ELT PARSER1637480411(PARSER, TOKEN);
dAS_CLASS_ELT PARSER1886860611(PARSER, TOKEN);
dAS_CLASS_ELT PARSER206119392(PARSER);
dAS_CLASS_ELT PARSER252822802(PARSER, TOKEN);
dAS_CLASS_ELT PARSER576720148(PARSER, TOKEN);
dAS_EXPR PARSER403674127(PARSER);
dAS_STMT PARSER1456149875(PARSER);
void AS_ARG2059479532(AS_ARG_DEC, AS_ARG_DEC);
void AS_PAR769375844(AS_PARAM_DEC, AS_PARAM_DEC);
void BFILE_plus_STR(BFILE, STR);
void CONVER649045550(CONVERT, IDENT);
void CONVER840793676(CONVERT, CHAR);
void PROG_err_STR(PROG, STR);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */




#undef IS_ITER
#define IS_ITER 0

AS_CLASS_DEF PARSER525553873(PARSER self) {
 AS_CLASS_DEF ret_val;
 AS_CLASS_DEF res;
 INT L11 = INT_zero;
 PARSER enter_self;
 STR enter_s;
 AS_CLASS_DEF create_self;
 AS_CLASS_DEF ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 PARSER fetch_self;
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
 PARSER fetch_self1;
 PARSER fetch_self2;
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
 PARSER fetch_self3;
 PARSER fetch_self4;
 CONVERT convert_value_se;
 CONVERT set_start_self;
 BFILE flush_self;
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
 PARSER fetch_self5;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val33 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val34;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val35;
 STR plus_self17;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val36;
 STR s3;
 CHAR str_self3 = CHAR_zero;
 STR ret_val37;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val38;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self6;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val39 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val40 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val41;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val42;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self5;
 SFILE_ID ret_val43 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val44;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val45;
 STR plus_self22;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val46;
 STR s4;
 CHAR str_self4 = CHAR_zero;
 STR ret_val47;
 STR create_self9;
 CHAR create_c4 = CHAR_zero;
 STR ret_val48;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self7;
 PARSER fetch_self8;
 PARSER ident_self;
 IDENT ret_val49 = IDENT_zero;
 PARSER match_self4;
 INT match_t4 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val50 = BOOL_zero;
 TOKEN create_self10 = TOKEN_zero;
 INT create_val4 = INT_zero;
 TOKEN ret_val51 = TOKEN_zero;
 TOKEN t4 = TOKEN_zero;
 PARSER exp_error_self4;
 STR exp_error_msg4;
 STR plus_self23;
 STR plus_sarg18;
 STR ret_val52;
 STR plus_self24;
 STR plus_sarg19;
 STR ret_val53;
 PARSER error_self5;
 STR error_msg5;
 PARSER source_loc_self6;
 SFILE_ID ret_val54 = SFILE_ID_zero;
 PROG set_eloc_self5;
 SFILE_ID set_eloc_l5 = SFILE_ID_zero;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val55;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val56;
 STR plus_self27;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val57;
 STR s5;
 CHAR str_self5 = CHAR_zero;
 STR ret_val58;
 STR create_self11;
 CHAR create_c5 = CHAR_zero;
 STR ret_val59;
 STR r5;
 PARSER err_self5;
 STR err_s5;
 PARSER fetch_self9;
 PARSER ident_self1;
 IDENT ret_val60 = IDENT_zero;
 PARSER match_self5;
 INT match_t5 = INT_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val61 = BOOL_zero;
 TOKEN create_self12 = TOKEN_zero;
 INT create_val5 = INT_zero;
 TOKEN ret_val62 = TOKEN_zero;
 TOKEN t5 = TOKEN_zero;
 PARSER exp_error_self5;
 STR exp_error_msg5;
 STR plus_self28;
 STR plus_sarg22;
 STR ret_val63;
 STR plus_self29;
 STR plus_sarg23;
 STR ret_val64;
 PARSER error_self6;
 STR error_msg6;
 PARSER source_loc_self7;
 SFILE_ID ret_val65 = SFILE_ID_zero;
 PROG set_eloc_self6;
 SFILE_ID set_eloc_l6 = SFILE_ID_zero;
 STR plus_self30;
 STR plus_sarg24;
 STR ret_val66;
 STR plus_self31;
 STR plus_sarg25;
 STR ret_val67;
 STR plus_self32;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val68;
 STR s6;
 CHAR str_self6 = CHAR_zero;
 STR ret_val69;
 STR create_self13;
 CHAR create_c6 = CHAR_zero;
 STR ret_val70;
 STR r6;
 PARSER err_self6;
 STR err_s6;
 PARSER fetch_self10;
 PARSER exp_error_self6;
 STR exp_error_msg6;
 STR plus_self33;
 STR plus_sarg26;
 STR ret_val71;
 STR plus_self34;
 STR plus_sarg27;
 STR ret_val72;
 PARSER error_self7;
 STR error_msg7;
 PARSER source_loc_self8;
 SFILE_ID ret_val73 = SFILE_ID_zero;
 PROG set_eloc_self7;
 SFILE_ID set_eloc_l7 = SFILE_ID_zero;
 STR plus_self35;
 STR plus_sarg28;
 STR ret_val74;
 STR plus_self36;
 STR plus_sarg29;
 STR ret_val75;
 STR plus_self37;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val76;
 STR s7;
 CHAR str_self7 = CHAR_zero;
 STR ret_val77;
 STR create_self14;
 CHAR create_c7 = CHAR_zero;
 STR ret_val78;
 STR r7;
 PARSER err_self7;
 STR err_s7;
 PARSER match_self6;
 INT match_t6 = INT_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val79 = BOOL_zero;
 TOKEN create_self15 = TOKEN_zero;
 INT create_val6 = INT_zero;
 TOKEN ret_val80 = TOKEN_zero;
 TOKEN t6 = TOKEN_zero;
 PARSER exp_error_self7;
 STR exp_error_msg7;
 STR plus_self38;
 STR plus_sarg30;
 STR ret_val81;
 STR plus_self39;
 STR plus_sarg31;
 STR ret_val82;
 PARSER error_self8;
 STR error_msg8;
 PARSER source_loc_self9;
 SFILE_ID ret_val83 = SFILE_ID_zero;
 PROG set_eloc_self8;
 SFILE_ID set_eloc_l8 = SFILE_ID_zero;
 STR plus_self40;
 STR plus_sarg32;
 STR ret_val84;
 STR plus_self41;
 STR plus_sarg33;
 STR ret_val85;
 STR plus_self42;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val86;
 STR s8;
 CHAR str_self8 = CHAR_zero;
 STR ret_val87;
 STR create_self16;
 CHAR create_c8 = CHAR_zero;
 STR ret_val88;
 STR r8;
 PARSER err_self8;
 STR err_s8;
 PARSER fetch_self11;
 PARSER ident_self2;
 IDENT ret_val89 = IDENT_zero;
 PARSER is_class_name_se;
 IDENT is_class_name_x = IDENT_zero;
 BOOL ret_val90 = BOOL_zero;
 PARSER exp_error_self8;
 STR exp_error_msg8;
 STR plus_self43;
 STR plus_sarg34;
 STR ret_val91;
 STR plus_self44;
 STR plus_sarg35;
 STR ret_val92;
 PARSER error_self9;
 STR error_msg9;
 PARSER source_loc_self10;
 SFILE_ID ret_val93 = SFILE_ID_zero;
 PROG set_eloc_self9;
 SFILE_ID set_eloc_l9 = SFILE_ID_zero;
 STR plus_self45;
 STR plus_sarg36;
 STR ret_val94;
 STR plus_self46;
 STR plus_sarg37;
 STR ret_val95;
 STR plus_self47;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val96;
 STR s9;
 CHAR str_self9 = CHAR_zero;
 STR ret_val97;
 STR create_self17;
 CHAR create_c9 = CHAR_zero;
 STR ret_val98;
 STR r9;
 PARSER err_self9;
 STR err_s9;
 PARSER exp_error_self9;
 STR exp_error_msg9;
 STR plus_self48;
 STR plus_sarg38;
 STR ret_val99;
 STR plus_self49;
 STR plus_sarg39;
 STR ret_val100;
 PARSER error_self10;
 STR error_msg10;
 PARSER source_loc_self11;
 SFILE_ID ret_val101 = SFILE_ID_zero;
 PROG set_eloc_self10;
 SFILE_ID set_eloc_l10 = SFILE_ID_zero;
 STR plus_self50;
 STR plus_sarg40;
 STR ret_val102;
 STR plus_self51;
 STR plus_sarg41;
 STR ret_val103;
 STR plus_self52;
 CHAR plus_arg10 = CHAR_zero;
 STR ret_val104;
 STR s10;
 CHAR str_self10 = CHAR_zero;
 STR ret_val105;
 STR create_self18;
 CHAR create_c10 = CHAR_zero;
 STR ret_val106;
 STR r10;
 PARSER err_self10;
 STR err_s10;
 TOKEN is_eq_self7 = TOKEN_zero;
 INT is_eq_y7 = INT_zero;
 BOOL ret_val107 = BOOL_zero;
 PARSER fetch_self12;
 PARSER ident_self3;
 IDENT ret_val108 = IDENT_zero;
 TOKEN is_eq_self8 = TOKEN_zero;
 INT is_eq_y8 = INT_zero;
 BOOL ret_val109 = BOOL_zero;
 PARSER match_self7;
 INT match_t7 = INT_zero;
 TOKEN is_eq_self9 = TOKEN_zero;
 INT is_eq_y9 = INT_zero;
 BOOL ret_val110 = BOOL_zero;
 TOKEN create_self19 = TOKEN_zero;
 INT create_val7 = INT_zero;
 TOKEN ret_val1111 = TOKEN_zero;
 TOKEN t7 = TOKEN_zero;
 PARSER exp_error_self10;
 STR exp_error_msg10;
 STR plus_self53;
 STR plus_sarg42;
 STR ret_val112;
 STR plus_self54;
 STR plus_sarg43;
 STR ret_val113;
 PARSER error_self11;
 STR error_msg11;
 PARSER source_loc_self12;
 SFILE_ID ret_val114 = SFILE_ID_zero;
 PROG set_eloc_self11;
 SFILE_ID set_eloc_l11 = SFILE_ID_zero;
 STR plus_self55;
 STR plus_sarg44;
 STR ret_val115;
 STR plus_self56;
 STR plus_sarg45;
 STR ret_val116;
 STR plus_self57;
 CHAR plus_arg11 = CHAR_zero;
 STR ret_val117;
 STR s11;
 CHAR str_self11 = CHAR_zero;
 STR ret_val118;
 STR create_self20;
 CHAR create_c11 = CHAR_zero;
 STR ret_val119;
 STR r11;
 PARSER err_self11;
 STR err_s11;
 PARSER fetch_self13;
 PARSER match_self8;
 INT match_t8 = INT_zero;
 TOKEN is_eq_self10 = TOKEN_zero;
 INT is_eq_y10 = INT_zero;
 BOOL ret_val120 = BOOL_zero;
 TOKEN create_self21 = TOKEN_zero;
 INT create_val8 = INT_zero;
 TOKEN ret_val121 = TOKEN_zero;
 TOKEN t8 = TOKEN_zero;
 PARSER exp_error_self11;
 STR exp_error_msg11;
 STR plus_self58;
 STR plus_sarg46;
 STR ret_val122;
 STR plus_self59;
 STR plus_sarg47;
 STR ret_val123;
 PARSER error_self12;
 STR error_msg12;
 PARSER source_loc_self13;
 SFILE_ID ret_val124 = SFILE_ID_zero;
 PROG set_eloc_self12;
 SFILE_ID set_eloc_l12 = SFILE_ID_zero;
 STR plus_self60;
 STR plus_sarg48;
 STR ret_val125;
 STR plus_self61;
 STR plus_sarg49;
 STR ret_val126;
 STR plus_self62;
 CHAR plus_arg12 = CHAR_zero;
 STR ret_val127;
 STR s12;
 CHAR str_self12 = CHAR_zero;
 STR ret_val128;
 STR create_self22;
 CHAR create_c12 = CHAR_zero;
 STR ret_val129;
 STR r12;
 PARSER err_self12;
 STR err_s12;
 PARSER fetch_self14;
 PARSER match_self9;
 INT match_t9 = INT_zero;
 TOKEN is_eq_self11 = TOKEN_zero;
 INT is_eq_y11 = INT_zero;
 BOOL ret_val130 = BOOL_zero;
 TOKEN create_self23 = TOKEN_zero;
 INT create_val9 = INT_zero;
 TOKEN ret_val131 = TOKEN_zero;
 TOKEN t9 = TOKEN_zero;
 PARSER exp_error_self12;
 STR exp_error_msg12;
 STR plus_self63;
 STR plus_sarg50;
 STR ret_val132;
 STR plus_self64;
 STR plus_sarg51;
 STR ret_val133;
 PARSER error_self13;
 STR error_msg13;
 PARSER source_loc_self14;
 SFILE_ID ret_val134 = SFILE_ID_zero;
 PROG set_eloc_self13;
 SFILE_ID set_eloc_l13 = SFILE_ID_zero;
 STR plus_self65;
 STR plus_sarg52;
 STR ret_val135;
 STR plus_self66;
 STR plus_sarg53;
 STR ret_val136;
 STR plus_self67;
 CHAR plus_arg13 = CHAR_zero;
 STR ret_val137;
 STR s13;
 CHAR str_self13 = CHAR_zero;
 STR ret_val138;
 STR create_self24;
 CHAR create_c13 = CHAR_zero;
 STR ret_val139;
 STR r13;
 PARSER err_self13;
 STR err_s13;
 PARSER fetch_self15;
 PARSER exit_self;
 STR s14;
 extern STR class1;
 AS_CLASS_DEF L2;
 OB L3;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L8;
 INT L9;
 OB L10;
 INT L12;
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
 extern STR immutableclass;
 INT L23;
 INT L241_;
 INT L25;
 BOOL L261_;
 BOOL L27;
 BOOL L281_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L29;
 INT L30;
 OB L31;
 INT L32;
 extern STR keywor1136581276;
 extern STR in1;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
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
 IDENT L48;
 extern STR FORTRAN;
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
 IDENT L59;
 extern STR C1;
 INT L60;
 BOOL L611_;
 BOOL L62;
 BOOL L631_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L64;
 INT L65;
 OB L66;
 INT L67;
 extern STR externclassname;
 extern STR expectedbutfound;
 extern STR in1;
 STR L69;
 INT L70;
 OB L72;
 INT L73;
 INT L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L79;
 INT L80;
 OB L81;
 INT L82;
 BOOL L84;
 BOOL L851_;
 extern STR classname;
 extern STR expectedbutfound;
 extern STR in1;
 STR L86;
 INT L87;
 OB L88;
 INT L89;
 extern STR concre5546650;
 extern STR expectedbutfound;
 extern STR in1;
 STR L91;
 INT L92;
 OB L93;
 INT L94;
 INT L96;
 BOOL L971_;
 INT L98;
 BOOL L991_;
 BOOL L100;
 BOOL L1011_;
 AS_PARAM_DEC L102;
 BOOL L103;
 BOOL L1041_;
 INT L105;
 BOOL L1061_;
 BOOL L107;
 BOOL L1081_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L109;
 INT L110;
 OB L111;
 INT L112;
 INT L114;
 BOOL L1151_;
 BOOL L116;
 BOOL L1171_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L118;
 INT L119;
 OB L120;
 INT L121;
 INT L123;
 BOOL L1241_;
 BOOL L125;
 BOOL L1261_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L127;
 INT L128;
 OB L129;
 INT L130;
 enter_self = self;
 enter_s = ((STR) &class1);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_CLASS_DEF) NULL);
 L3=ZALLOC(sizeof(struct AS_CLASS_DEF_struct));
 if (L3==NULL) FATAL("Unable to allocate more memory");
 ((OB)L3)->header.tag=AS_CLASS_DEF_tag;
 L2 = ((AS_CLASS_DEF) L3);
 ret_val1 = L2;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 L11 = ATTR(self,next).val1;
 switch (L11) {
  case 107: 
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
   SATTR(res,kind,AS_CLASS_DEF_spr);
   match_self = self;
   match_t = PARSER_class_tok;
   is_eq_self = ATTR(match_self,next);
   is_eq_y = match_t;
   L4 = is_eq_self.val1;
   L51_=(L4)==(is_eq_y); 
   ret_val3 = L51_;
   L6 = ret_val3;
   L71_=!(L6); 
   if (L71_) {
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
    L9 = 1;
    L12=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L9)*sizeof(CHAR);
    L10=ZALLOC_LEAF_BIG(L12);
    if (L10==NULL) FATAL("Unable to allocate more memory");
    memset(L10,0,L12);
    ((OB)L10)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L10)->header.destroyed=0;
#endif

    L8 = ((STR) L10);
    L8->asize = L9;
    r = L8;
    SARR((STR)r,0,create_c); 
    ;
    ret_val12 = r;
    ret_val11 = ret_val12;
    s = ret_val11;
    ret_val10 = STR_ap1077157958(plus_self4, s, TRUE);
    err_s = ret_val10;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   fetch_self1 = match_self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   break;
  case 45: 
   fetch_self2 = self;
   SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
   SATTR(res,kind,AS_CLASS_DEF_imm);
   match_self1 = self;
   match_t1 = PARSER_class_tok;
   is_eq_self1 = ATTR(match_self1,next);
   is_eq_y1 = match_t1;
   L14 = is_eq_self1.val1;
   L151_=(L14)==(is_eq_y1); 
   ret_val13 = L151_;
   L16 = ret_val13;
   L171_=!(L16); 
   if (L171_) {
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
    ret_val22 = r1;
    ret_val21 = ret_val22;
    s1 = ret_val21;
    ret_val20 = STR_ap1077157958(plus_self9, s1, TRUE);
    err_s1 = ret_val20;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   fetch_self3 = match_self1;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   break;
  case 46: 
   if (ATTR(self,version_1_0)) {
    fetch_self4 = self;
    SATTR(fetch_self4,next,SCANNE1792387248(ATTR(fetch_self4,scanner)));
    SATTR(res,kind,AS_CLASS_DEF_imm);
    convert_value_se = ATTR(self,convert);
    if (ATTR(convert_value_se,need_to_convert)) {
     CONVER840793676(convert_value_se, 'v');
     BFILE_plus_STR(ATTR(convert_value_se,file1), ((STR) &immutableclass));
     set_start_self = convert_value_se;
     if (ATTR(set_start_self,need_to_convert)) {
      L23 = SFILE_ID_pos;
      L241_=INTMINUS(L23,1); 
      SATTR(set_start_self,start_pos,L241_);
     }
     flush_self = ATTR(convert_value_se,file1);
     fflush(ATTR(flush_self,fp));
    }
    match_self2 = self;
    match_t2 = PARSER_class_tok;
    is_eq_self2 = ATTR(match_self2,next);
    is_eq_y2 = match_t2;
    L25 = is_eq_self2.val1;
    L261_=(L25)==(is_eq_y2); 
    ret_val23 = L261_;
    L27 = ret_val23;
    L281_=!(L27); 
    if (L281_) {
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
     r2 = L29;
     SARR((STR)r2,0,create_c2); 
     ;
     ret_val32 = r2;
     ret_val31 = ret_val32;
     s2 = ret_val31;
     ret_val30 = STR_ap1077157958(plus_self14, s2, TRUE);
     err_s2 = ret_val30;
     PROG_err_STR(ATTR(err_self2,prog), err_s2);
    }
    fetch_self5 = match_self2;
    SATTR(fetch_self5,next,SCANNE1792387248(ATTR(fetch_self5,scanner)));
   }
   else {
    error_self3 = self;
    error_msg3 = ((STR) &keywor1136581276);
    set_eloc_self3 = ATTR(error_self3,prog);
    source_loc_self4 = error_self3;
    ret_val33 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l3 = ret_val33;
    SATTR(set_eloc_self3,eloc,set_eloc_l3);
    err_self3 = error_self3;
    plus_self15 = error_msg3;
    plus_sarg12 = ((STR) &in1);
    ret_val34 = STR_ap2004550586(plus_self15, plus_sarg12);
    plus_self16 = ret_val34;
    plus_sarg13 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
    ret_val35 = STR_ap2004550586(plus_self16, plus_sarg13);
    plus_self17 = ret_val35;
    plus_arg3 = ')';
    str_self3 = plus_arg3;
    create_self7 = ((STR) NULL);
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
    ret_val38 = r3;
    ret_val37 = ret_val38;
    s3 = ret_val37;
    ret_val36 = STR_ap1077157958(plus_self17, s3, TRUE);
    err_s3 = ret_val36;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   break;
  case 47: 
   fetch_self6 = self;
   SATTR(fetch_self6,next,SCANNE1792387248(ATTR(fetch_self6,scanner)));
   SATTR(res,kind,AS_CLA86009053);
   match_self3 = self;
   match_t3 = PARSER_class_tok;
   is_eq_self3 = ATTR(match_self3,next);
   is_eq_y3 = match_t3;
   L39 = is_eq_self3.val1;
   L401_=(L39)==(is_eq_y3); 
   ret_val39 = L401_;
   L41 = ret_val39;
   L421_=!(L41); 
   if (L421_) {
    exp_error_self3 = match_self3;
    create_self8 = TOKEN_zero;
    create_val3 = match_t3;
    t3.val1 = create_val3;
    ret_val40 = t3;
    exp_error_msg3 = TOKEN_strrSTR(ret_val40);
    error_self4 = exp_error_self3;
    plus_self18 = exp_error_msg3;
    plus_sarg14 = ((STR) &expectedbutfound);
    ret_val41 = STR_ap2004550586(plus_self18, plus_sarg14);
    plus_self19 = ret_val41;
    plus_sarg15 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
    ret_val42 = STR_ap2004550586(plus_self19, plus_sarg15);
    error_msg4 = ret_val42;
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self5 = error_self4;
    ret_val43 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val43;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self20 = error_msg4;
    plus_sarg16 = ((STR) &in1);
    ret_val44 = STR_ap2004550586(plus_self20, plus_sarg16);
    plus_self21 = ret_val44;
    plus_sarg17 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val45 = STR_ap2004550586(plus_self21, plus_sarg17);
    plus_self22 = ret_val45;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self9 = ((STR) NULL);
    create_c4 = str_self4;
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
    r4 = L43;
    SARR((STR)r4,0,create_c4); 
    ;
    ret_val48 = r4;
    ret_val47 = ret_val48;
    s4 = ret_val47;
    ret_val46 = STR_ap1077157958(plus_self22, s4, TRUE);
    err_s4 = ret_val46;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   fetch_self7 = match_self3;
   SATTR(fetch_self7,next,SCANNE1792387248(ATTR(fetch_self7,scanner)));
   break;
  case 15: 
   fetch_self8 = self;
   SATTR(fetch_self8,next,SCANNE1792387248(ATTR(fetch_self8,scanner)));
   if (PARSER2128430730(self, PARSER_ident_tok)) {
    ident_self = self;
    ret_val49 = ATTR(ATTR(ident_self,scanner),lex_value);
    L48=ret_val49;
    if (STR_is111530248(L48.str, ((STR) &FORTRAN))) {
     SATTR(res,kind,AS_CLA2085573152);
     match_self4 = self;
     match_t4 = PARSER_class_tok;
     is_eq_self4 = ATTR(match_self4,next);
     is_eq_y4 = match_t4;
     L49 = is_eq_self4.val1;
     L501_=(L49)==(is_eq_y4); 
     ret_val50 = L501_;
     L52 = ret_val50;
     L531_=!(L52); 
     if (L531_) {
      exp_error_self4 = match_self4;
      create_self10 = TOKEN_zero;
      create_val4 = match_t4;
      t4.val1 = create_val4;
      ret_val51 = t4;
      exp_error_msg4 = TOKEN_strrSTR(ret_val51);
      error_self5 = exp_error_self4;
      plus_self23 = exp_error_msg4;
      plus_sarg18 = ((STR) &expectedbutfound);
      ret_val52 = STR_ap2004550586(plus_self23, plus_sarg18);
      plus_self24 = ret_val52;
      plus_sarg19 = TOKEN_strrSTR(ATTR(exp_error_self4,next));
      ret_val53 = STR_ap2004550586(plus_self24, plus_sarg19);
      error_msg5 = ret_val53;
      set_eloc_self5 = ATTR(error_self5,prog);
      source_loc_self6 = error_self5;
      ret_val54 = SFILE_1594766561(SFILE_ID_zero);
      set_eloc_l5 = ret_val54;
      SATTR(set_eloc_self5,eloc,set_eloc_l5);
      err_self5 = error_self5;
      plus_self25 = error_msg5;
      plus_sarg20 = ((STR) &in1);
      ret_val55 = STR_ap2004550586(plus_self25, plus_sarg20);
      plus_self26 = ret_val55;
      plus_sarg21 = FLISTSTR_toprSTR(ATTR(error_self5,entered));
      ret_val56 = STR_ap2004550586(plus_self26, plus_sarg21);
      plus_self27 = ret_val56;
      plus_arg5 = ')';
      str_self5 = plus_arg5;
      create_self11 = ((STR) NULL);
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
      r5 = L54;
      SARR((STR)r5,0,create_c5); 
      ;
      ret_val59 = r5;
      ret_val58 = ret_val59;
      s5 = ret_val58;
      ret_val57 = STR_ap1077157958(plus_self27, s5, TRUE);
      err_s5 = ret_val57;
      PROG_err_STR(ATTR(err_self5,prog), err_s5);
     }
     fetch_self9 = match_self4;
     SATTR(fetch_self9,next,SCANNE1792387248(ATTR(fetch_self9,scanner)));
    }
    else {
     ident_self1 = self;
     ret_val60 = ATTR(ATTR(ident_self1,scanner),lex_value);
     L59=ret_val60;
     if (STR_is111530248(L59.str, ((STR) &C1))) {
      SATTR(res,kind,AS_CLA1666707132);
      match_self5 = self;
      match_t5 = PARSER_class_tok;
      is_eq_self5 = ATTR(match_self5,next);
      is_eq_y5 = match_t5;
      L60 = is_eq_self5.val1;
      L611_=(L60)==(is_eq_y5); 
      ret_val61 = L611_;
      L62 = ret_val61;
      L631_=!(L62); 
      if (L631_) {
       exp_error_self5 = match_self5;
       create_self12 = TOKEN_zero;
       create_val5 = match_t5;
       t5.val1 = create_val5;
       ret_val62 = t5;
       exp_error_msg5 = TOKEN_strrSTR(ret_val62);
       error_self6 = exp_error_self5;
       plus_self28 = exp_error_msg5;
       plus_sarg22 = ((STR) &expectedbutfound);
       ret_val63 = STR_ap2004550586(plus_self28, plus_sarg22);
       plus_self29 = ret_val63;
       plus_sarg23 = TOKEN_strrSTR(ATTR(exp_error_self5,next));
       ret_val64 = STR_ap2004550586(plus_self29, plus_sarg23);
       error_msg6 = ret_val64;
       set_eloc_self6 = ATTR(error_self6,prog);
       source_loc_self7 = error_self6;
       ret_val65 = SFILE_1594766561(SFILE_ID_zero);
       set_eloc_l6 = ret_val65;
       SATTR(set_eloc_self6,eloc,set_eloc_l6);
       err_self6 = error_self6;
       plus_self30 = error_msg6;
       plus_sarg24 = ((STR) &in1);
       ret_val66 = STR_ap2004550586(plus_self30, plus_sarg24);
       plus_self31 = ret_val66;
       plus_sarg25 = FLISTSTR_toprSTR(ATTR(error_self6,entered));
       ret_val67 = STR_ap2004550586(plus_self31, plus_sarg25);
       plus_self32 = ret_val67;
       plus_arg6 = ')';
       str_self6 = plus_arg6;
       create_self13 = ((STR) NULL);
       create_c6 = str_self6;
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
       r6 = L64;
       SARR((STR)r6,0,create_c6); 
       ;
       ret_val70 = r6;
       ret_val69 = ret_val70;
       s6 = ret_val69;
       ret_val68 = STR_ap1077157958(plus_self32, s6, TRUE);
       err_s6 = ret_val68;
       PROG_err_STR(ATTR(err_self6,prog), err_s6);
      }
      fetch_self10 = match_self5;
      SATTR(fetch_self10,next,SCANNE1792387248(ATTR(fetch_self10,scanner)));
     }
    }
   }
   else {
    if (PARSER2128430730(self, PARSER_class_tok)) {
     SATTR(res,kind,AS_CLA1666707132);
    }
    else {
     exp_error_self6 = self;
     exp_error_msg6 = ((STR) &externclassname);
     error_self7 = exp_error_self6;
     plus_self33 = exp_error_msg6;
     plus_sarg26 = ((STR) &expectedbutfound);
     ret_val71 = STR_ap2004550586(plus_self33, plus_sarg26);
     plus_self34 = ret_val71;
     plus_sarg27 = TOKEN_strrSTR(ATTR(exp_error_self6,next));
     ret_val72 = STR_ap2004550586(plus_self34, plus_sarg27);
     error_msg7 = ret_val72;
     set_eloc_self7 = ATTR(error_self7,prog);
     source_loc_self8 = error_self7;
     ret_val73 = SFILE_1594766561(SFILE_ID_zero);
     set_eloc_l7 = ret_val73;
     SATTR(set_eloc_self7,eloc,set_eloc_l7);
     err_self7 = error_self7;
     plus_self35 = error_msg7;
     plus_sarg28 = ((STR) &in1);
     ret_val74 = STR_ap2004550586(plus_self35, plus_sarg28);
     plus_self36 = ret_val74;
     plus_sarg29 = FLISTSTR_toprSTR(ATTR(error_self7,entered));
     ret_val75 = STR_ap2004550586(plus_self36, plus_sarg29);
     plus_self37 = ret_val75;
     plus_arg7 = ')';
     str_self7 = plus_arg7;
     create_self14 = ((STR) NULL);
     create_c7 = str_self7;
     L70 = 1;
     L73=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L70)*sizeof(CHAR);
     L72=ZALLOC_LEAF_BIG(L73);
     if (L72==NULL) FATAL("Unable to allocate more memory");
     memset(L72,0,L73);
     ((OB)L72)->header.tag=STR_tag;
#ifdef DESTROY_CHK

       ((OB)L72)->header.destroyed=0;
#endif

     L69 = ((STR) L72);
     L69->asize = L70;
     r7 = L69;
     SARR((STR)r7,0,create_c7); 
     ;
     ret_val78 = r7;
     ret_val77 = ret_val78;
     s7 = ret_val77;
     ret_val76 = STR_ap1077157958(plus_self37, s7, TRUE);
     err_s7 = ret_val76;
     PROG_err_STR(ATTR(err_self7,prog), err_s7);
    }
   }
   break;
  default: ;
   SATTR(res,kind,AS_CLASS_DEF_ref);
   match_self6 = self;
   match_t6 = PARSER_class_tok;
   is_eq_self6 = ATTR(match_self6,next);
   is_eq_y6 = match_t6;
   L75 = is_eq_self6.val1;
   L761_=(L75)==(is_eq_y6); 
   ret_val79 = L761_;
   L77 = ret_val79;
   L781_=!(L77); 
   if (L781_) {
    exp_error_self7 = match_self6;
    create_self15 = TOKEN_zero;
    create_val6 = match_t6;
    t6.val1 = create_val6;
    ret_val80 = t6;
    exp_error_msg7 = TOKEN_strrSTR(ret_val80);
    error_self8 = exp_error_self7;
    plus_self38 = exp_error_msg7;
    plus_sarg30 = ((STR) &expectedbutfound);
    ret_val81 = STR_ap2004550586(plus_self38, plus_sarg30);
    plus_self39 = ret_val81;
    plus_sarg31 = TOKEN_strrSTR(ATTR(exp_error_self7,next));
    ret_val82 = STR_ap2004550586(plus_self39, plus_sarg31);
    error_msg8 = ret_val82;
    set_eloc_self8 = ATTR(error_self8,prog);
    source_loc_self9 = error_self8;
    ret_val83 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l8 = ret_val83;
    SATTR(set_eloc_self8,eloc,set_eloc_l8);
    err_self8 = error_self8;
    plus_self40 = error_msg8;
    plus_sarg32 = ((STR) &in1);
    ret_val84 = STR_ap2004550586(plus_self40, plus_sarg32);
    plus_self41 = ret_val84;
    plus_sarg33 = FLISTSTR_toprSTR(ATTR(error_self8,entered));
    ret_val85 = STR_ap2004550586(plus_self41, plus_sarg33);
    plus_self42 = ret_val85;
    plus_arg8 = ')';
    str_self8 = plus_arg8;
    create_self16 = ((STR) NULL);
    create_c8 = str_self8;
    L80 = 1;
    L82=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L80)*sizeof(CHAR);
    L81=ZALLOC_LEAF_BIG(L82);
    if (L81==NULL) FATAL("Unable to allocate more memory");
    memset(L81,0,L82);
    ((OB)L81)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L81)->header.destroyed=0;
#endif

    L79 = ((STR) L81);
    L79->asize = L80;
    r8 = L79;
    SARR((STR)r8,0,create_c8); 
    ;
    ret_val88 = r8;
    ret_val87 = ret_val88;
    s8 = ret_val87;
    ret_val86 = STR_ap1077157958(plus_self42, s8, TRUE);
    err_s8 = ret_val86;
    PROG_err_STR(ATTR(err_self8,prog), err_s8);
   }
   fetch_self11 = match_self6;
   SATTR(fetch_self11,next,SCANNE1792387248(ATTR(fetch_self11,scanner)));
 }
 if (PARSER2128430730(self, PARSER_ident_tok)) {
  is_class_name_se = self;
  ident_self2 = self;
  ret_val89 = ATTR(ATTR(ident_self2,scanner),lex_value);
  is_class_name_x = ret_val89;
  ret_val90 = SCANNE363215919(ATTR(is_class_name_se,scanner), is_class_name_x.str);
  L84 = ret_val90;
  L851_=!(L84); 
  if (L851_) {
   exp_error_self8 = self;
   exp_error_msg8 = ((STR) &classname);
   error_self9 = exp_error_self8;
   plus_self43 = exp_error_msg8;
   plus_sarg34 = ((STR) &expectedbutfound);
   ret_val91 = STR_ap2004550586(plus_self43, plus_sarg34);
   plus_self44 = ret_val91;
   plus_sarg35 = TOKEN_strrSTR(ATTR(exp_error_self8,next));
   ret_val92 = STR_ap2004550586(plus_self44, plus_sarg35);
   error_msg9 = ret_val92;
   set_eloc_self9 = ATTR(error_self9,prog);
   source_loc_self10 = error_self9;
   ret_val93 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l9 = ret_val93;
   SATTR(set_eloc_self9,eloc,set_eloc_l9);
   err_self9 = error_self9;
   plus_self45 = error_msg9;
   plus_sarg36 = ((STR) &in1);
   ret_val94 = STR_ap2004550586(plus_self45, plus_sarg36);
   plus_self46 = ret_val94;
   plus_sarg37 = FLISTSTR_toprSTR(ATTR(error_self9,entered));
   ret_val95 = STR_ap2004550586(plus_self46, plus_sarg37);
   plus_self47 = ret_val95;
   plus_arg9 = ')';
   str_self9 = plus_arg9;
   create_self17 = ((STR) NULL);
   create_c9 = str_self9;
   L87 = 1;
   L89=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L87)*sizeof(CHAR);
   L88=ZALLOC_LEAF_BIG(L89);
   if (L88==NULL) FATAL("Unable to allocate more memory");
   memset(L88,0,L89);
   ((OB)L88)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L88)->header.destroyed=0;
#endif

   L86 = ((STR) L88);
   L86->asize = L87;
   r9 = L86;
   SARR((STR)r9,0,create_c9); 
   ;
   ret_val98 = r9;
   ret_val97 = ret_val98;
   s9 = ret_val97;
   ret_val96 = STR_ap1077157958(plus_self47, s9, TRUE);
   err_s9 = ret_val96;
   PROG_err_STR(ATTR(err_self9,prog), err_s9);
  }
 }
 else {
  exp_error_self9 = self;
  exp_error_msg9 = ((STR) &concre5546650);
  error_self10 = exp_error_self9;
  plus_self48 = exp_error_msg9;
  plus_sarg38 = ((STR) &expectedbutfound);
  ret_val99 = STR_ap2004550586(plus_self48, plus_sarg38);
  plus_self49 = ret_val99;
  plus_sarg39 = TOKEN_strrSTR(ATTR(exp_error_self9,next));
  ret_val100 = STR_ap2004550586(plus_self49, plus_sarg39);
  error_msg10 = ret_val100;
  set_eloc_self10 = ATTR(error_self10,prog);
  source_loc_self11 = error_self10;
  ret_val101 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l10 = ret_val101;
  SATTR(set_eloc_self10,eloc,set_eloc_l10);
  err_self10 = error_self10;
  plus_self50 = error_msg10;
  plus_sarg40 = ((STR) &in1);
  ret_val102 = STR_ap2004550586(plus_self50, plus_sarg40);
  plus_self51 = ret_val102;
  plus_sarg41 = FLISTSTR_toprSTR(ATTR(error_self10,entered));
  ret_val103 = STR_ap2004550586(plus_self51, plus_sarg41);
  plus_self52 = ret_val103;
  plus_arg10 = ')';
  str_self10 = plus_arg10;
  create_self18 = ((STR) NULL);
  create_c10 = str_self10;
  L92 = 1;
  L94=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L92)*sizeof(CHAR);
  L93=ZALLOC_LEAF_BIG(L94);
  if (L93==NULL) FATAL("Unable to allocate more memory");
  memset(L93,0,L94);
  ((OB)L93)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L93)->header.destroyed=0;
#endif

  L91 = ((STR) L93);
  L91->asize = L92;
  r10 = L91;
  SARR((STR)r10,0,create_c10); 
  ;
  ret_val106 = r10;
  ret_val105 = ret_val106;
  s10 = ret_val105;
  ret_val104 = STR_ap1077157958(plus_self52, s10, TRUE);
  err_s10 = ret_val104;
  PROG_err_STR(ATTR(err_self10,prog), err_s10);
  is_eq_self7 = ATTR(self,next);
  is_eq_y7 = PARSER978809429;
  L96 = is_eq_self7.val1;
  L971_=(L96)==(is_eq_y7); 
  ret_val107 = L971_;
  if (ret_val107) {
   fetch_self12 = self;
   SATTR(fetch_self12,next,SCANNE1792387248(ATTR(fetch_self12,scanner)));
  }
 }
 ident_self3 = self;
 ret_val108 = ATTR(ATTR(ident_self3,scanner),lex_value);
 SATTR(res,name1,ret_val108);
 if (PARSER2128430730(self, PARSER2001381604)) {
  while (1) {
   is_eq_self8 = ATTR(self,next);
   is_eq_y8 = PARSER_ident_tok;
   L98 = is_eq_self8.val1;
   L991_=(L98)==(is_eq_y8); 
   ret_val109 = L991_;
   L100 = ret_val109;
   L1011_=!(L100); 
   if (L1011_) {
    goto after_loop;
   }
   if ((ATTR(res,params)==((AS_PARAM_DEC) NULL))) {
    SATTR(res,params,PARSER1105068487(self));
   }
   else {
    L102 = ATTR(res,params);
    AS_PAR769375844(L102, PARSER1105068487(self));
   }
   L103 = PARSER2128430730(self, PARSER_comma_tok);
   L1041_=!(L103); 
   if (L1041_) {
    goto after_loop;
   }
  }
  after_loop: ;
  match_self7 = self;
  match_t7 = PARSER1998725038;
  is_eq_self9 = ATTR(match_self7,next);
  is_eq_y9 = match_t7;
  L105 = is_eq_self9.val1;
  L1061_=(L105)==(is_eq_y9); 
  ret_val110 = L1061_;
  L107 = ret_val110;
  L1081_=!(L107); 
  if (L1081_) {
   exp_error_self10 = match_self7;
   create_self19 = TOKEN_zero;
   create_val7 = match_t7;
   t7.val1 = create_val7;
   ret_val1111 = t7;
   exp_error_msg10 = TOKEN_strrSTR(ret_val1111);
   error_self11 = exp_error_self10;
   plus_self53 = exp_error_msg10;
   plus_sarg42 = ((STR) &expectedbutfound);
   ret_val112 = STR_ap2004550586(plus_self53, plus_sarg42);
   plus_self54 = ret_val112;
   plus_sarg43 = TOKEN_strrSTR(ATTR(exp_error_self10,next));
   ret_val113 = STR_ap2004550586(plus_self54, plus_sarg43);
   error_msg11 = ret_val113;
   set_eloc_self11 = ATTR(error_self11,prog);
   source_loc_self12 = error_self11;
   ret_val114 = SFILE_1594766561(SFILE_ID_zero);
   set_eloc_l11 = ret_val114;
   SATTR(set_eloc_self11,eloc,set_eloc_l11);
   err_self11 = error_self11;
   plus_self55 = error_msg11;
   plus_sarg44 = ((STR) &in1);
   ret_val115 = STR_ap2004550586(plus_self55, plus_sarg44);
   plus_self56 = ret_val115;
   plus_sarg45 = FLISTSTR_toprSTR(ATTR(error_self11,entered));
   ret_val116 = STR_ap2004550586(plus_self56, plus_sarg45);
   plus_self57 = ret_val116;
   plus_arg11 = ')';
   str_self11 = plus_arg11;
   create_self20 = ((STR) NULL);
   create_c11 = str_self11;
   L110 = 1;
   L112=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L110)*sizeof(CHAR);
   L111=ZALLOC_LEAF_BIG(L112);
   if (L111==NULL) FATAL("Unable to allocate more memory");
   memset(L111,0,L112);
   ((OB)L111)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L111)->header.destroyed=0;
#endif

   L109 = ((STR) L111);
   L109->asize = L110;
   r11 = L109;
   SARR((STR)r11,0,create_c11); 
   ;
   ret_val119 = r11;
   ret_val118 = ret_val119;
   s11 = ret_val118;
   ret_val117 = STR_ap1077157958(plus_self57, s11, TRUE);
   err_s11 = ret_val117;
   PROG_err_STR(ATTR(err_self11,prog), err_s11);
  }
  fetch_self13 = match_self7;
  SATTR(fetch_self13,next,SCANNE1792387248(ATTR(fetch_self13,scanner)));
 }
 if (PARSER2128430730(self, PARSER_is_lt_tok)) {
  SATTR(res,under,PARSER532091533(self));
 }
 match_self8 = self;
 match_t8 = PARSER_is_tok;
 is_eq_self10 = ATTR(match_self8,next);
 is_eq_y10 = match_t8;
 L114 = is_eq_self10.val1;
 L1151_=(L114)==(is_eq_y10); 
 ret_val120 = L1151_;
 L116 = ret_val120;
 L1171_=!(L116); 
 if (L1171_) {
  exp_error_self11 = match_self8;
  create_self21 = TOKEN_zero;
  create_val8 = match_t8;
  t8.val1 = create_val8;
  ret_val121 = t8;
  exp_error_msg11 = TOKEN_strrSTR(ret_val121);
  error_self12 = exp_error_self11;
  plus_self58 = exp_error_msg11;
  plus_sarg46 = ((STR) &expectedbutfound);
  ret_val122 = STR_ap2004550586(plus_self58, plus_sarg46);
  plus_self59 = ret_val122;
  plus_sarg47 = TOKEN_strrSTR(ATTR(exp_error_self11,next));
  ret_val123 = STR_ap2004550586(plus_self59, plus_sarg47);
  error_msg12 = ret_val123;
  set_eloc_self12 = ATTR(error_self12,prog);
  source_loc_self13 = error_self12;
  ret_val124 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l12 = ret_val124;
  SATTR(set_eloc_self12,eloc,set_eloc_l12);
  err_self12 = error_self12;
  plus_self60 = error_msg12;
  plus_sarg48 = ((STR) &in1);
  ret_val125 = STR_ap2004550586(plus_self60, plus_sarg48);
  plus_self61 = ret_val125;
  plus_sarg49 = FLISTSTR_toprSTR(ATTR(error_self12,entered));
  ret_val126 = STR_ap2004550586(plus_self61, plus_sarg49);
  plus_self62 = ret_val126;
  plus_arg12 = ')';
  str_self12 = plus_arg12;
  create_self22 = ((STR) NULL);
  create_c12 = str_self12;
  L119 = 1;
  L121=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L119)*sizeof(CHAR);
  L120=ZALLOC_LEAF_BIG(L121);
  if (L120==NULL) FATAL("Unable to allocate more memory");
  memset(L120,0,L121);
  ((OB)L120)->header.tag=STR_tag;
#ifdef DESTROY_CHK

    ((OB)L120)->header.destroyed=0;
#endif

  L118 = ((STR) L120);
  L118->asize = L119;
  r12 = L118;
  SARR((STR)r12,0,create_c12); 
  ;
  ret_val129 = r12;
  ret_val128 = ret_val129;
  s12 = ret_val128;
  ret_val127 = STR_ap1077157958(plus_self62, s12, TRUE);
  err_s12 = ret_val127;
  PROG_err_STR(ATTR(err_self12,prog), err_s12);
 }
 fetch_self14 = match_self8;
 SATTR(fetch_self14,next,SCANNE1792387248(ATTR(fetch_self14,scanner)));
 SATTR(res,body,PARSER1583311295(self));
 match_self9 = self;
 match_t9 = PARSER_end_tok;
 is_eq_self11 = ATTR(match_self9,next);
 is_eq_y11 = match_t9;
 L123 = is_eq_self11.val1;
 L1241_=(L123)==(is_eq_y11); 
 ret_val130 = L1241_;
 L125 = ret_val130;
 L1261_=!(L125); 
 if (L1261_) {
  exp_error_self12 = match_self9;
  create_self23 = TOKEN_zero;
  create_val9 = match_t9;
  t9.val1 = create_val9;
  ret_val131 = t9;
  exp_error_msg12 = TOKEN_strrSTR(ret_val131);
  error_self13 = exp_error_self12;
  plus_self63 = exp_error_msg12;
  plus_sarg50 = ((STR) &expectedbutfound);
  ret_val132 = STR_ap2004550586(plus_self63, plus_sarg50);
  plus_self64 = ret_val132;
  plus_sarg51 = TOKEN_strrSTR(ATTR(exp_error_self12,next));
  ret_val133 = STR_ap2004550586(plus_self64, plus_sarg51);
  error_msg13 = ret_val133;
  set_eloc_self13 = ATTR(error_self13,prog);
  source_loc_self14 = error_self13;
  ret_val134 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l13 = ret_val134;
  SATTR(set_eloc_self13,eloc,set_eloc_l13);
  err_self13 = error_self13;
  plus_self65 = error_msg13;
  plus_sarg52 = ((STR) &in1);
  ret_val135 = STR_ap2004550586(plus_self65, plus_sarg52);
  plus_self66 = ret_val135;
  plus_sarg53 = FLISTSTR_toprSTR(ATTR(error_self13,entered));
  ret_val136 = STR_ap2004550586(plus_self66, plus_sarg53);
  plus_self67 = ret_val136;
  plus_arg13 = ')';
  str_self13 = plus_arg13;
  create_self24 = ((STR) NULL);
  create_c13 = str_self13;
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
  r13 = L127;
  SARR((STR)r13,0,create_c13); 
  ;
  ret_val139 = r13;
  ret_val138 = ret_val139;
  s13 = ret_val138;
  ret_val137 = STR_ap1077157958(plus_self67, s13, TRUE);
  err_s13 = ret_val137;
  PROG_err_STR(ATTR(err_self13,prog), err_s13);
 }
 fetch_self15 = match_self9;
 SATTR(fetch_self15,next,SCANNE1792387248(ATTR(fetch_self15,scanner)));
 exit_self = self;
 s14 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_FORK_STMT PARSER2069741717(PARSER self) {
 AS_FORK_STMT ret_val;
 AS_FORK_STMT res;
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
 AS_FORK_STMT create_self2;
 AS_FORK_STMT ret_val11;
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
 extern STR forkstatement;
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
 AS_FORK_STMT L10;
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
 enter_s = ((STR) &forkstatement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 match_self = self;
 match_t = PARSER_fork_tok;
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
 create_self2 = ((AS_FORK_STMT) NULL);
 L11=ZALLOC(sizeof(struct AS_FORK_STMT_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=AS_FORK_STMT_tag;
 L10 = ((AS_FORK_STMT) L11);
 ret_val11 = L10;
 res = ret_val11;
 source_loc_self1 = self;
 ret_val12 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val12);
 if (PARSER2128430730(self, PARSER_at_tok)) {
  SATTR(res,at,PARSER403674127(self));
 }
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

AS_IF_STMT PARSER1465700283(PARSER self, dAS_EXPR test1, dAS_STMT then_part, dAS_STMT else_part) {
 AS_IF_STMT ret_val;
 AS_IF_STMT res;
 AS_IF_STMT create_self;
 AS_IF_STMT ret_val1;
 PARSER source_loc_self;
 SFILE_ID ret_val2 = SFILE_ID_zero;
 AS_STMT_LIST create_self1;
 AS_STMT_LIST ret_val3;
 AS_STMT_LIST create_self2;
 AS_STMT_LIST ret_val4;
 AS_IF_STMT L1;
 OB L2;
 AS_STMT_LIST L3;
 OB L4;
 BOOL L5;
 BOOL L61_;
 dAS_STMT L7;
 BOOL L8;
 BOOL L91_;
 AS_STMT_LIST L10;
 OB L11;
 dAS_STMT L12;
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
 SATTR(res,test1,test1);
 create_self1 = ((AS_STMT_LIST) NULL);
 L4=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=AS_STMT_LIST_tag;
 L3 = ((AS_STMT_LIST) L4);
 ret_val3 = L3;
 SATTR(res,then_part,ret_val3);
 L5 = (then_part==((dAS_STMT) NULL));
 L61_=!(L5); 
 if (L61_) {
  L7 = then_part;
  (*dAS_ST164863607[TAG(L7)])(L7, ATTR(res,then_part));
  SATTR(ATTR(res,then_part),stmts,then_part);
 }
 L8 = (else_part==((dAS_STMT) NULL));
 L91_=!(L8); 
 if (L91_) {
  create_self2 = ((AS_STMT_LIST) NULL);
  L11=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=AS_STMT_LIST_tag;
  L10 = ((AS_STMT_LIST) L11);
  ret_val4 = L10;
  SATTR(res,else_part,ret_val4);
  L12 = else_part;
  (*dAS_ST164863607[TAG(L12)])(L12, ATTR(res,else_part));
  SATTR(ATTR(res,else_part),stmts,else_part);
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_ROUT_DEF PARSER1732860771(PARSER self, TOKEN mode1) {
 AS_ROUT_DEF ret_val;
 AS_ROUT_DEF res = ((AS_ROUT_DEF) NULL);
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
 AS_ROUT_DEF create_self1;
 AS_ROUT_DEF ret_val8;
 PARSER source_loc_self1;
 SFILE_ID ret_val9 = SFILE_ID_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val10 = BOOL_zero;
 PARSER enter_self1;
 STR enter_s1;
 IDENT is_iter_self = IDENT_zero;
 BOOL ret_val11 = BOOL_zero;
 IDENT is_iter_self1 = IDENT_zero;
 BOOL ret_val12 = BOOL_zero;
 IDENT is_iter_self2 = IDENT_zero;
 BOOL ret_val13 = BOOL_zero;
 IDENT is_iter_self3 = IDENT_zero;
 BOOL ret_val14 = BOOL_zero;
 CONVERT set_start_self;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
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
 PARSER fetch_self;
 PARSER exit_self;
 STR s2;
 PARSER enter_self2;
 STR enter_s2;
 PARSER exit_self1;
 STR s3;
 PARSER enter_self3;
 STR enter_s3;
 PARSER exit_self2;
 STR s4;
 PARSER enter_self4;
 STR enter_s4;
 PARSER exit_self3;
 STR s5;
 PARSER enter_self5;
 STR enter_s5;
 PARSER ident_self;
 IDENT ret_val25 = IDENT_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val26 = BOOL_zero;
 TOKEN create_self4 = TOKEN_zero;
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
 PARSER source_loc_self3;
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
 STR s6;
 CHAR str_self2 = CHAR_zero;
 STR ret_val34;
 STR create_self5;
 CHAR create_c2 = CHAR_zero;
 STR ret_val35;
 STR r2;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self1;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val36 = BOOL_zero;
 TOKEN create_self6 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val37 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self13;
 STR plus_sarg10;
 STR ret_val38;
 STR plus_self14;
 STR plus_sarg11;
 STR ret_val39;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self4;
 SFILE_ID ret_val40 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val41;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val42;
 STR plus_self17;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val43;
 STR s7;
 CHAR str_self3 = CHAR_zero;
 STR ret_val44;
 STR create_self7;
 CHAR create_c3 = CHAR_zero;
 STR ret_val45;
 STR r3;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self2;
 PARSER exit_self4;
 STR s8;
 PARSER enter_self6;
 STR enter_s6;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val46 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val47 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self18;
 STR plus_sarg14;
 STR ret_val48;
 STR plus_self19;
 STR plus_sarg15;
 STR ret_val49;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self5;
 SFILE_ID ret_val50 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val51;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val52;
 STR plus_self22;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val53;
 STR s9;
 CHAR str_self4 = CHAR_zero;
 STR ret_val54;
 STR create_self9;
 CHAR create_c4 = CHAR_zero;
 STR ret_val55;
 STR r4;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self3;
 PARSER exit_self5;
 STR s10;
 PARSER exit_self6;
 STR s11;
 extern STR routin1430749969;
 INT L1;
 BOOL L21_;
 extern STR readon861021595;
 extern STR in1;
 STR L3;
 INT L4;
 OB L5;
 INT L6;
 AS_ROUT_DEF L8;
 OB L9;
 INT L10;
 BOOL L111_;
 extern STR arguments1;
 BOOL L12;
 BOOL L13;
 BOOL L141_;
 STR L15;
 INT L16;
 INT L171_;
 INT L18;
 CHAR L191_;
 CHAR L20;
 BOOL L221_;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 STR L26;
 INT L27;
 INT L281_;
 INT L29;
 CHAR L301_;
 CHAR L31;
 BOOL L321_;
 AS_ARG_DEC L33;
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
 BOOL L44;
 BOOL L45;
 BOOL L461_;
 STR L47;
 INT L48;
 INT L491_;
 INT L50;
 CHAR L511_;
 CHAR L52;
 BOOL L531_;
 INT L54;
 INT L551_;
 INT L56;
 BOOL L571_;
 BOOL L58;
 BOOL L591_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L60;
 INT L61;
 OB L62;
 INT L63;
 extern STR return1163638156;
 extern STR precon1094831807;
 extern STR postco1748199031;
 extern STR builti1891652362;
 INT L65;
 BOOL L661_;
 BOOL L67;
 BOOL L681_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L69;
 INT L70;
 OB L72;
 INT L73;
 INT L75;
 BOOL L761_;
 BOOL L77;
 BOOL L781_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L79;
 INT L80;
 OB L81;
 INT L82;
 extern STR routineiterbody;
 INT L84;
 BOOL L851_;
 BOOL L86;
 BOOL L871_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L88;
 INT L89;
 OB L90;
 INT L91;
 enter_self = self;
 enter_s = ((STR) &routin1430749969);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 is_eq_self = mode1;
 is_eq_y = PARSER550831344;
 L1 = is_eq_self.val1;
 L21_=(L1)==(is_eq_y); 
 ret_val1 = L21_;
 if (ret_val1) {
  error_self = self;
  error_msg = ((STR) &readon861021595);
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
 create_self1 = ((AS_ROUT_DEF) NULL);
 L9=ZALLOC(sizeof(struct AS_ROUT_DEF_struct));
 if (L9==NULL) FATAL("Unable to allocate more memory");
 ((OB)L9)->header.tag=AS_ROUT_DEF_tag;
 L8 = ((AS_ROUT_DEF) L9);
 ret_val8 = L8;
 res = ret_val8;
 source_loc_self1 = self;
 ret_val9 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val9);
 SATTR(res,name1,PARSER814698330(self));
 is_eq_self1 = mode1;
 is_eq_y1 = PARSER1019557737;
 L10 = is_eq_self1.val1;
 L111_=(L10)==(is_eq_y1); 
 ret_val10 = L111_;
 SATTR(res,is_private,ret_val10);
 if (PARSER2128430730(self, PARSER1209676627)) {
  enter_self1 = self;
  enter_s1 = ((STR) &arguments1);
  SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
  is_iter_self = ATTR(res,name1);
  L13 = (is_iter_self.str==((STR) NULL));
  L141_=!(L13); 
  if (L141_) {
   L15 = is_iter_self.str;
   L16 = STR_sizerINT(is_iter_self.str);
   L171_=INTMINUS(L16,1); 
   L18 = L171_;
   L191_=ARR((STR)L15,L18); 
   L20 = L191_;
   L221_=L20=='!'; 
   L12 = L221_;
  } else {
   L12 = FALSE;
  }
  ret_val11 = L12;
  if (ret_val11) {
   CONVER649045550(ATTR(self,convert), ATTR(res,name1));
  }
  while (1) {
   if ((ATTR(res,args_dec)==((AS_ARG_DEC) NULL))) {
    is_iter_self1 = ATTR(res,name1);
    L24 = (is_iter_self1.str==((STR) NULL));
    L251_=!(L24); 
    if (L251_) {
     L26 = is_iter_self1.str;
     L27 = STR_sizerINT(is_iter_self1.str);
     L281_=INTMINUS(L27,1); 
     L29 = L281_;
     L301_=ARR((STR)L26,L29); 
     L31 = L301_;
     L321_=L31=='!'; 
     L23 = L321_;
    } else {
     L23 = FALSE;
    }
    ret_val12 = L23;
    SATTR(res,args_dec,PARSER13880807(self, ret_val12));
   }
   else {
    L33 = ATTR(res,args_dec);
    is_iter_self2 = ATTR(res,name1);
    L35 = (is_iter_self2.str==((STR) NULL));
    L361_=!(L35); 
    if (L361_) {
     L37 = is_iter_self2.str;
     L38 = STR_sizerINT(is_iter_self2.str);
     L391_=INTMINUS(L38,1); 
     L40 = L391_;
     L411_=ARR((STR)L37,L40); 
     L42 = L411_;
     L431_=L42=='!'; 
     L34 = L431_;
    } else {
     L34 = FALSE;
    }
    ret_val13 = L34;
    AS_ARG2059479532(L33, PARSER13880807(self, ret_val13));
   }
   if (PARSER2128430730(self, PARSER_comma_tok)) {
   }
   else {
    goto after_loop;
   }
  }
  after_loop: ;
  is_iter_self3 = ATTR(res,name1);
  L45 = (is_iter_self3.str==((STR) NULL));
  L461_=!(L45); 
  if (L461_) {
   L47 = is_iter_self3.str;
   L48 = STR_sizerINT(is_iter_self3.str);
   L491_=INTMINUS(L48,1); 
   L50 = L491_;
   L511_=ARR((STR)L47,L50); 
   L52 = L511_;
   L531_=L52=='!'; 
   L44 = L531_;
  } else {
   L44 = FALSE;
  }
  ret_val14 = L44;
  if (ret_val14) {
   set_start_self = ATTR(self,convert);
   if (ATTR(set_start_self,need_to_convert)) {
    L54 = SFILE_ID_pos;
    L551_=INTMINUS(L54,1); 
    SATTR(set_start_self,start_pos,L551_);
   }
  }
  match_self = self;
  match_t = PARSER1207020061;
  is_eq_self2 = ATTR(match_self,next);
  is_eq_y2 = match_t;
  L56 = is_eq_self2.val1;
  L571_=(L56)==(is_eq_y2); 
  ret_val15 = L571_;
  L58 = ret_val15;
  L591_=!(L58); 
  if (L591_) {
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
   L61 = 1;
   L63=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L61)*sizeof(CHAR);
   L62=ZALLOC_LEAF_BIG(L63);
   if (L62==NULL) FATAL("Unable to allocate more memory");
   memset(L62,0,L63);
   ((OB)L62)->header.tag=STR_tag;
#ifdef DESTROY_CHK

     ((OB)L62)->header.destroyed=0;
#endif

   L60 = ((STR) L62);
   L60->asize = L61;
   r1 = L60;
   SARR((STR)r1,0,create_c1); 
   ;
   ret_val24 = r1;
   ret_val23 = ret_val24;
   s1 = ret_val23;
   ret_val22 = STR_ap1077157958(plus_self7, s1, TRUE);
   err_s1 = ret_val22;
   PROG_err_STR(ATTR(err_self1,prog), err_s1);
  }
  fetch_self = match_self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
  exit_self = self;
  s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 }
 if (PARSER2128430730(self, PARSER_colon_tok)) {
  enter_self2 = self;
  enter_s2 = ((STR) &return1163638156);
  SATTR(enter_self2,entered,FLISTS1425610062(ATTR(enter_self2,entered), enter_s2));
  SATTR(res,ret_dec,PARSER2028498823(self));
  exit_self1 = self;
  s3 = FLISTSTR_poprSTR(ATTR(exit_self1,entered));
 }
 if (PARSER2128430730(self, PARSER_pre_tok)) {
  enter_self3 = self;
  enter_s3 = ((STR) &precon1094831807);
  SATTR(enter_self3,entered,FLISTS1425610062(ATTR(enter_self3,entered), enter_s3));
  SATTR(res,pre_e,PARSER403674127(self));
  exit_self2 = self;
  s4 = FLISTSTR_poprSTR(ATTR(exit_self2,entered));
 }
 if (PARSER2128430730(self, PARSER_post_tok)) {
  enter_self4 = self;
  enter_s4 = ((STR) &postco1748199031);
  SATTR(enter_self4,entered,FLISTS1425610062(ATTR(enter_self4,entered), enter_s4));
  SATTR(res,post_e,PARSER403674127(self));
  exit_self3 = self;
  s5 = FLISTSTR_poprSTR(ATTR(exit_self3,entered));
 }
 if (PARSER2128430730(self, PARSER_is_tok)) {
  if (PARSER2128430730(self, PARSER2011952041)) {
   enter_self5 = self;
   enter_s5 = ((STR) &builti1891652362);
   SATTR(enter_self5,entered,FLISTS1425610062(ATTR(enter_self5,entered), enter_s5));
   SATTR(res,is_builtin,TRUE);
   ident_self = self;
   ret_val25 = ATTR(ATTR(ident_self,scanner),lex_value);
   SATTR(res,builtin_name,ret_val25);
   match_self1 = self;
   match_t1 = PARSER_ident_tok;
   is_eq_self3 = ATTR(match_self1,next);
   is_eq_y3 = match_t1;
   L65 = is_eq_self3.val1;
   L661_=(L65)==(is_eq_y3); 
   ret_val26 = L661_;
   L67 = ret_val26;
   L681_=!(L67); 
   if (L681_) {
    exp_error_self1 = match_self1;
    create_self4 = TOKEN_zero;
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
    source_loc_self3 = error_self2;
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
    create_self5 = ((STR) NULL);
    create_c2 = str_self2;
    L70 = 1;
    L73=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L70)*sizeof(CHAR);
    L72=ZALLOC_LEAF_BIG(L73);
    if (L72==NULL) FATAL("Unable to allocate more memory");
    memset(L72,0,L73);
    ((OB)L72)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L72)->header.destroyed=0;
#endif

    L69 = ((STR) L72);
    L69->asize = L70;
    r2 = L69;
    SARR((STR)r2,0,create_c2); 
    ;
    ret_val35 = r2;
    ret_val34 = ret_val35;
    s6 = ret_val34;
    ret_val33 = STR_ap1077157958(plus_self12, s6, TRUE);
    err_s2 = ret_val33;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   fetch_self1 = match_self1;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   if (PARSER2128430730(self, PARSER_semi_tok)) {
   }
   match_self2 = self;
   match_t2 = PARSER_end_tok;
   is_eq_self4 = ATTR(match_self2,next);
   is_eq_y4 = match_t2;
   L75 = is_eq_self4.val1;
   L761_=(L75)==(is_eq_y4); 
   ret_val36 = L761_;
   L77 = ret_val36;
   L781_=!(L77); 
   if (L781_) {
    exp_error_self2 = match_self2;
    create_self6 = TOKEN_zero;
    create_val2 = match_t2;
    t2.val1 = create_val2;
    ret_val37 = t2;
    exp_error_msg2 = TOKEN_strrSTR(ret_val37);
    error_self3 = exp_error_self2;
    plus_self13 = exp_error_msg2;
    plus_sarg10 = ((STR) &expectedbutfound);
    ret_val38 = STR_ap2004550586(plus_self13, plus_sarg10);
    plus_self14 = ret_val38;
    plus_sarg11 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
    ret_val39 = STR_ap2004550586(plus_self14, plus_sarg11);
    error_msg3 = ret_val39;
    set_eloc_self3 = ATTR(error_self3,prog);
    source_loc_self4 = error_self3;
    ret_val40 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l3 = ret_val40;
    SATTR(set_eloc_self3,eloc,set_eloc_l3);
    err_self3 = error_self3;
    plus_self15 = error_msg3;
    plus_sarg12 = ((STR) &in1);
    ret_val41 = STR_ap2004550586(plus_self15, plus_sarg12);
    plus_self16 = ret_val41;
    plus_sarg13 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
    ret_val42 = STR_ap2004550586(plus_self16, plus_sarg13);
    plus_self17 = ret_val42;
    plus_arg3 = ')';
    str_self3 = plus_arg3;
    create_self7 = ((STR) NULL);
    create_c3 = str_self3;
    L80 = 1;
    L82=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L80)*sizeof(CHAR);
    L81=ZALLOC_LEAF_BIG(L82);
    if (L81==NULL) FATAL("Unable to allocate more memory");
    memset(L81,0,L82);
    ((OB)L81)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L81)->header.destroyed=0;
#endif

    L79 = ((STR) L81);
    L79->asize = L80;
    r3 = L79;
    SARR((STR)r3,0,create_c3); 
    ;
    ret_val45 = r3;
    ret_val44 = ret_val45;
    s7 = ret_val44;
    ret_val43 = STR_ap1077157958(plus_self17, s7, TRUE);
    err_s3 = ret_val43;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   fetch_self2 = match_self2;
   SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
   exit_self4 = self;
   s8 = FLISTSTR_poprSTR(ATTR(exit_self4,entered));
  }
  else {
   enter_self6 = self;
   enter_s6 = ((STR) &routineiterbody);
   SATTR(enter_self6,entered,FLISTS1425610062(ATTR(enter_self6,entered), enter_s6));
   SATTR(res,body,PARSER680097147(self));
   SATTR(res,is_abstract,FALSE);
   match_self3 = self;
   match_t3 = PARSER_end_tok;
   is_eq_self5 = ATTR(match_self3,next);
   is_eq_y5 = match_t3;
   L84 = is_eq_self5.val1;
   L851_=(L84)==(is_eq_y5); 
   ret_val46 = L851_;
   L86 = ret_val46;
   L871_=!(L86); 
   if (L871_) {
    exp_error_self3 = match_self3;
    create_self8 = TOKEN_zero;
    create_val3 = match_t3;
    t3.val1 = create_val3;
    ret_val47 = t3;
    exp_error_msg3 = TOKEN_strrSTR(ret_val47);
    error_self4 = exp_error_self3;
    plus_self18 = exp_error_msg3;
    plus_sarg14 = ((STR) &expectedbutfound);
    ret_val48 = STR_ap2004550586(plus_self18, plus_sarg14);
    plus_self19 = ret_val48;
    plus_sarg15 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
    ret_val49 = STR_ap2004550586(plus_self19, plus_sarg15);
    error_msg4 = ret_val49;
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self5 = error_self4;
    ret_val50 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val50;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self20 = error_msg4;
    plus_sarg16 = ((STR) &in1);
    ret_val51 = STR_ap2004550586(plus_self20, plus_sarg16);
    plus_self21 = ret_val51;
    plus_sarg17 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val52 = STR_ap2004550586(plus_self21, plus_sarg17);
    plus_self22 = ret_val52;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self9 = ((STR) NULL);
    create_c4 = str_self4;
    L89 = 1;
    L91=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L89)*sizeof(CHAR);
    L90=ZALLOC_LEAF_BIG(L91);
    if (L90==NULL) FATAL("Unable to allocate more memory");
    memset(L90,0,L91);
    ((OB)L90)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L90)->header.destroyed=0;
#endif

    L88 = ((STR) L90);
    L88->asize = L89;
    r4 = L88;
    SARR((STR)r4,0,create_c4); 
    ;
    ret_val55 = r4;
    ret_val54 = ret_val55;
    s9 = ret_val54;
    ret_val53 = STR_ap1077157958(plus_self22, s9, TRUE);
    err_s4 = ret_val53;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   fetch_self3 = match_self3;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   exit_self5 = self;
   s10 = FLISTSTR_poprSTR(ATTR(exit_self5,entered));
  }
 }
 else {
  SATTR(res,is_abstract,TRUE);
 }
 exit_self6 = self;
 s11 = FLISTSTR_poprSTR(ATTR(exit_self6,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_STMT_LIST PARSER680097147(PARSER self) {
 AS_STMT_LIST ret_val;
 AS_STMT_LIST list1;
 dAS_STMT res = ((dAS_STMT) NULL);
 dAS_STMT s;
 dAS_STMT nx;
 AS_STMT_LIST create_self;
 AS_STMT_LIST ret_val1;
 PARSER enter_self;
 STR enter_s;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val3;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val4 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val5;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val6;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val7;
 STR s1;
 CHAR str_self = CHAR_zero;
 STR ret_val8;
 STR create_self1;
 CHAR create_c = CHAR_zero;
 STR ret_val9;
 STR r;
 PARSER err_self;
 STR err_s;
 PARSER exit_self;
 STR s2;
 AS_STMT_LIST L1;
 OB L2;
 extern STR listofstatements;
 dAS_STMT L3;
 dAS_STMT L4;
 BOOL L5;
 BOOL L61_;
 dAS_STMT L7;
 dAS_STMT L8;
 dAS_STMT L9;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L10;
 INT L11;
 OB L12;
 INT L13;
 create_self = ((AS_STMT_LIST) NULL);
 L2=ZALLOC(sizeof(struct AS_STMT_LIST_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_STMT_LIST_tag;
 L1 = ((AS_STMT_LIST) L2);
 ret_val1 = L1;
 list1 = ret_val1;
 enter_self = self;
 enter_s = ((STR) &listofstatements);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
  if (PARSER1152230473(self, ATTR(self,next))) {
   s = PARSER1456149875(self);
   L3 = s;
   (*dAS_ST164863607[TAG(L3)])(L3, list1);
   L4 = s;
   nx = (*dAS_ST460279343[TAG(L4)])(L4);
   while (1) {
    L5 = (nx==((dAS_STMT) NULL));
    L61_=!(L5); 
    if (L61_) {
    }
    else {
     goto after_loop1;
    }
    L7 = nx;
    (*dAS_ST164863607[TAG(L7)])(L7, list1);
    L8 = nx;
    nx = (*dAS_ST460279343[TAG(L8)])(L8);
   }
   after_loop1: ;
   if ((res==((dAS_STMT) NULL))) {
    res = s;
   }
   else {
    L9 = res;
    (*dAS_ST1454156204[TAG(L9)])(L9, s);
   }
  }
  if (PARSER2128430730(self, PARSER_semi_tok)) {
  }
  else {
   if (PARSER1152230473(self, ATTR(self,next))) {
    exp_error_self = self;
    exp_error_msg = ((STR) &semicolon);
    error_self = exp_error_self;
    plus_self = exp_error_msg;
    plus_sarg = ((STR) &expectedbutfound);
    ret_val2 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val2;
    plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
    ret_val3 = STR_ap2004550586(plus_self1, plus_sarg1);
    error_msg = ret_val3;
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self = error_self;
    ret_val4 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val4;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self2 = error_msg;
    plus_sarg2 = ((STR) &in1);
    ret_val5 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val5;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val6 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val6;
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
    ret_val9 = r;
    ret_val8 = ret_val9;
    s1 = ret_val8;
    ret_val7 = STR_ap1077157958(plus_self4, s1, TRUE);
    err_s = ret_val7;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 exit_self = self;
 s2 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 SATTR(list1,stmts,res);
 ret_val = list1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AS_WIT1063437351 PARSER1613260578(PARSER self) {
 AS_WIT1063437351 ret_val;
 AS_WIT1063437351 res;
 AS_IDENT_LIST id;
 PARSER enter_self;
 STR enter_s;
 AS_WIT1063437351 create_self;
 AS_WIT1063437351 ret_val1;
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
 PARSER exit_self;
 STR s3;
 extern STR with_n199596397;
 AS_WIT1063437351 L1;
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
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
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
 enter_self = self;
 enter_s = ((STR) &with_n199596397);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = ((AS_WIT1063437351) NULL);
 L2=ZALLOC(sizeof(struct AS_WIT1063437351_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AS_WIT1063437351_tag;
 L1 = ((AS_WIT1063437351) L2);
 ret_val1 = L1;
 res = ret_val1;
 source_loc_self = self;
 ret_val2 = SFILE_1594766561(SFILE_ID_zero);
 SATTR(res,source1,ret_val2);
 match_self = self;
 match_t = PARSER_with_tok;
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
 SATTR(res,idents,PARSER1448067791(self, ((dAS_NODE) res)));
 SATTR(res,elts_size,0);
 id = ATTR(res,idents);
 while (1) {
  L12 = (id==((AS_IDENT_LIST) NULL));
  L131_=!(L12); 
  if (L131_) {
  }
  else {
   goto after_loop;
  }
  L14 = ATTR(res,elts_size);
  L151_=INTPLUS(L14,1); 
  SATTR(res,elts_size,L151_);
  id = ATTR(id,next);
 }
 after_loop: ;
 if (ATTR(res,self_occurred)) {
  L16 = ATTR(res,elts_size);
  L171_=INTPLUS(L16,1); 
  SATTR(res,elts_size,L171_);
 }
 match_self1 = self;
 match_t1 = PARSER_near_tok;
 is_eq_self1 = ATTR(match_self1,next);
 is_eq_y1 = match_t1;
 L18 = is_eq_self1.val1;
 L191_=(L18)==(is_eq_y1); 
 ret_val13 = L191_;
 L20 = ret_val13;
 L211_=!(L20); 
 if (L211_) {
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
  ret_val22 = r1;
  ret_val21 = ret_val22;
  s1 = ret_val21;
  ret_val20 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val20;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
 }
 fetch_self1 = match_self1;
 SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
 SATTR(res,near_part,PARSER680097147(self));
 if (PARSER2128430730(self, PARSER_else_tok)) {
  SATTR(res,else_part,PARSER680097147(self));
 }
 match_self2 = self;
 match_t2 = PARSER_end_tok;
 is_eq_self2 = ATTR(match_self2,next);
 is_eq_y2 = match_t2;
 L27 = is_eq_self2.val1;
 L281_=(L27)==(is_eq_y2); 
 ret_val23 = L281_;
 L29 = ret_val23;
 L301_=!(L29); 
 if (L301_) {
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
  ret_val32 = r2;
  ret_val31 = ret_val32;
  s2 = ret_val31;
  ret_val30 = STR_ap1077157958(plus_self14, s2, TRUE);
  err_s2 = ret_val30;
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

dAS_CLASS_ELT PARSER1583311295(PARSER self) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 PARSER enter_self;
 STR enter_s;
 PARSER exp_error_self;
 STR exp_error_msg;
 STR plus_self;
 STR plus_sarg;
 STR ret_val1;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val2;
 PARSER error_self;
 STR error_msg;
 PARSER source_loc_self;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 PROG set_eloc_self;
 SFILE_ID set_eloc_l = SFILE_ID_zero;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self3;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self4;
 CHAR plus_arg = CHAR_zero;
 STR ret_val6;
 STR s;
 CHAR str_self = CHAR_zero;
 STR ret_val7;
 STR create_self;
 CHAR create_c = CHAR_zero;
 STR ret_val8;
 STR r;
 PARSER err_self;
 STR err_s;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val9 = BOOL_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val10;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val11;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self1;
 SFILE_ID ret_val12 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val13;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val14;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val15;
 STR s1;
 CHAR str_self1 = CHAR_zero;
 STR ret_val16;
 STR create_self1;
 CHAR create_c1 = CHAR_zero;
 STR ret_val17;
 STR r1;
 PARSER err_self1;
 STR err_s1;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val18 = BOOL_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val19 = BOOL_zero;
 PARSER fetch_self;
 PARSER exit_self;
 STR s2;
 extern STR listof1648894639;
 dAS_CLASS_ELT L1;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L2;
 INT L3;
 OB L4;
 INT L5;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR semicolon;
 extern STR expectedbutfound;
 extern STR in1;
 STR L11;
 INT L12;
 OB L13;
 INT L14;
 BOOL L16;
 INT L17;
 BOOL L181_;
 BOOL L19;
 BOOL L201_;
 INT L21;
 BOOL L221_;
 BOOL L23;
 BOOL L241_;
 enter_self = self;
 enter_s = ((STR) &listof1648894639);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 while (1) {
  if (PARSER1234876012(self, ATTR(self,next))) {
   if ((res==((dAS_CLASS_ELT) NULL))) {
    res = PARSER206119392(self);
   }
   else {
    L1 = res;
    (*dAS_CL1056554733[TAG(L1)])(L1, PARSER206119392(self));
   }
  }
  if (PARSER2128430730(self, PARSER_semi_tok)) {
  }
  else {
   if (PARSER1234876012(self, ATTR(self,next))) {
    exp_error_self = self;
    exp_error_msg = ((STR) &semicolon);
    error_self = exp_error_self;
    plus_self = exp_error_msg;
    plus_sarg = ((STR) &expectedbutfound);
    ret_val1 = STR_ap2004550586(plus_self, plus_sarg);
    plus_self1 = ret_val1;
    plus_sarg1 = TOKEN_strrSTR(ATTR(exp_error_self,next));
    ret_val2 = STR_ap2004550586(plus_self1, plus_sarg1);
    error_msg = ret_val2;
    set_eloc_self = ATTR(error_self,prog);
    source_loc_self = error_self;
    ret_val3 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l = ret_val3;
    SATTR(set_eloc_self,eloc,set_eloc_l);
    err_self = error_self;
    plus_self2 = error_msg;
    plus_sarg2 = ((STR) &in1);
    ret_val4 = STR_ap2004550586(plus_self2, plus_sarg2);
    plus_self3 = ret_val4;
    plus_sarg3 = FLISTSTR_toprSTR(ATTR(error_self,entered));
    ret_val5 = STR_ap2004550586(plus_self3, plus_sarg3);
    plus_self4 = ret_val5;
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
    ret_val8 = r;
    ret_val7 = ret_val8;
    s = ret_val7;
    ret_val6 = STR_ap1077157958(plus_self4, s, TRUE);
    err_s = ret_val6;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   else {
    goto after_loop;
   }
  }
 }
 after_loop: ;
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER_end_tok;
 L7 = is_eq_self.val1;
 L81_=(L7)==(is_eq_y); 
 ret_val9 = L81_;
 L9 = ret_val9;
 L101_=!(L9); 
 if (L101_) {
  exp_error_self1 = self;
  exp_error_msg1 = ((STR) &semicolon);
  error_self1 = exp_error_self1;
  plus_self5 = exp_error_msg1;
  plus_sarg4 = ((STR) &expectedbutfound);
  ret_val10 = STR_ap2004550586(plus_self5, plus_sarg4);
  plus_self6 = ret_val10;
  plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
  ret_val11 = STR_ap2004550586(plus_self6, plus_sarg5);
  error_msg1 = ret_val11;
  set_eloc_self1 = ATTR(error_self1,prog);
  source_loc_self1 = error_self1;
  ret_val12 = SFILE_1594766561(SFILE_ID_zero);
  set_eloc_l1 = ret_val12;
  SATTR(set_eloc_self1,eloc,set_eloc_l1);
  err_self1 = error_self1;
  plus_self7 = error_msg1;
  plus_sarg6 = ((STR) &in1);
  ret_val13 = STR_ap2004550586(plus_self7, plus_sarg6);
  plus_self8 = ret_val13;
  plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
  ret_val14 = STR_ap2004550586(plus_self8, plus_sarg7);
  plus_self9 = ret_val14;
  plus_arg1 = ')';
  str_self1 = plus_arg1;
  create_self1 = ((STR) NULL);
  create_c1 = str_self1;
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
  ret_val17 = r1;
  ret_val16 = ret_val17;
  s1 = ret_val16;
  ret_val15 = STR_ap1077157958(plus_self9, s1, TRUE);
  err_s1 = ret_val15;
  PROG_err_STR(ATTR(err_self1,prog), err_s1);
  while (1) {
   is_eq_self1 = ATTR(self,next);
   is_eq_y1 = PARSER_end_tok;
   L17 = is_eq_self1.val1;
   L181_=(L17)==(is_eq_y1); 
   ret_val18 = L181_;
   L19 = ret_val18;
   L201_=!(L19); 
   if (L201_) {
    is_eq_self2 = ATTR(self,next);
    is_eq_y2 = PARSER_eof_tok;
    L21 = is_eq_self2.val1;
    L221_=(L21)==(is_eq_y2); 
    ret_val19 = L221_;
    L23 = ret_val19;
    L241_=!(L23); 
    L16 = L241_;
   } else {
    L16 = FALSE;
   }
   if (L16) {
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

dAS_CLASS_ELT PARSER206119392(PARSER self) {
 dAS_CLASS_ELT ret_val;
 dAS_CLASS_ELT res = ((dAS_CLASS_ELT) NULL);
 TOKEN mode1 = TOKEN_zero;
 INT L11 = INT_zero;
 PARSER enter_self;
 STR enter_s;
 TOKEN create_self = TOKEN_zero;
 INT create_val = INT_zero;
 TOKEN ret_val1 = TOKEN_zero;
 TOKEN t = TOKEN_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val2 = BOOL_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val3 = BOOL_zero;
 PARSER fetch_self;
 PARSER exit_self;
 STR s;
 extern STR classelement;
 BOOL L2;
 INT L3;
 BOOL L41_;
 INT L5;
 BOOL L61_;
 enter_self = self;
 enter_s = ((STR) &classelement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 create_self = TOKEN_zero;
 create_val = PARSER_null_tok;
 t.val1 = create_val;
 ret_val1 = t;
 mode1 = ret_val1;
 is_eq_self = ATTR(self,next);
 is_eq_y = PARSER1019557737;
 L3 = is_eq_self.val1;
 L41_=(L3)==(is_eq_y); 
 ret_val2 = L41_;
 if (ret_val2) {
  L2 = TRUE;
 } else {
  is_eq_self1 = ATTR(self,next);
  is_eq_y1 = PARSER550831344;
  L5 = is_eq_self1.val1;
  L61_=(L5)==(is_eq_y1); 
  ret_val3 = L61_;
  L2 = ret_val3;
 }
 if (L2) {
  mode1 = ATTR(self,next);
  fetch_self = self;
  SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
 }
 L11 = ATTR(self,next).val1;
 switch (L11) {
  case 18: 
   res = PARSER1637480411(self, mode1);
   break;
  case 10: 
   res = PARSER576720148(self, mode1);
   break;
  case 38: 
   res = PARSER252822802(self, mode1);
   break;
  case 6: 
   res = PARSER1886860611(self, mode1);
   break;
  case 48: 
   res = PARSER1594550222(self, mode1);
   break;
  default: ;
   res = ((dAS_CLASS_ELT) PARSER1732860771(self, mode1));
 }
 exit_self = self;
 s = FLISTSTR_poprSTR(ATTR(exit_self,entered));
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dAS_STMT PARSER1456149875(PARSER self) {
 dAS_STMT ret_val;
 dAS_STMT res = ((dAS_STMT) NULL);
 SFILE_ID was_at = SFILE_ID_zero;
 INT L11 = INT_zero;
 dAS_EXPR x;
 AS_TYPE_SPEC tp = ((AS_TYPE_SPEC) NULL);
 AS_ASSIGN_STMT r;
 AS_DEC_STMT sdecl_res;
 AS_ASSIGN_STMT r1;
 AS_ATTACH_STMT r2;
 AS_DEC_STMT decl_res;
 AS_DEC_STMT newdec;
 AS_TYPE_SPEC tp2;
 dAS_STMT p;
 AS_EXPR_STMT r3;
 PARSER enter_self;
 STR enter_s;
 PARSER source_loc_self;
 SFILE_ID ret_val1 = SFILE_ID_zero;
 PARSER fetch_self;
 PARSER fetch_self1;
 AS_QUIT_STMT create_self;
 AS_QUIT_STMT ret_val2;
 PARSER source_loc_self1;
 SFILE_ID ret_val3 = SFILE_ID_zero;
 PARSER assert_stmt_self;
 AS_ASSERT_STMT ret_val4;
 AS_ASSERT_STMT res1;
 PARSER enter_self1;
 STR enter_s1;
 AS_ASSERT_STMT create_self1;
 AS_ASSERT_STMT ret_val5;
 PARSER source_loc_self2;
 SFILE_ID ret_val6 = SFILE_ID_zero;
 PARSER match_self;
 INT match_t = INT_zero;
 TOKEN is_eq_self = TOKEN_zero;
 INT is_eq_y = INT_zero;
 BOOL ret_val7 = BOOL_zero;
 TOKEN create_self2 = TOKEN_zero;
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
 PARSER source_loc_self3;
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
 STR create_self3;
 CHAR create_c = CHAR_zero;
 STR ret_val16;
 STR r4;
 PARSER err_self;
 STR err_s;
 PARSER fetch_self2;
 PARSER exit_self;
 STR s1;
 PARSER raise_stmt_self;
 AS_RAISE_STMT ret_val17;
 AS_RAISE_STMT res2;
 PARSER enter_self2;
 STR enter_s2;
 AS_RAISE_STMT create_self4;
 AS_RAISE_STMT ret_val18;
 PARSER source_loc_self4;
 SFILE_ID ret_val19 = SFILE_ID_zero;
 PARSER match_self1;
 INT match_t1 = INT_zero;
 TOKEN is_eq_self1 = TOKEN_zero;
 INT is_eq_y1 = INT_zero;
 BOOL ret_val20 = BOOL_zero;
 TOKEN create_self5 = TOKEN_zero;
 INT create_val1 = INT_zero;
 TOKEN ret_val21 = TOKEN_zero;
 TOKEN t1 = TOKEN_zero;
 PARSER exp_error_self1;
 STR exp_error_msg1;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val22;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val23;
 PARSER error_self1;
 STR error_msg1;
 PARSER source_loc_self5;
 SFILE_ID ret_val24 = SFILE_ID_zero;
 PROG set_eloc_self1;
 SFILE_ID set_eloc_l1 = SFILE_ID_zero;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val25;
 STR plus_self8;
 STR plus_sarg7;
 STR ret_val26;
 STR plus_self9;
 CHAR plus_arg1 = CHAR_zero;
 STR ret_val27;
 STR s2;
 CHAR str_self1 = CHAR_zero;
 STR ret_val28;
 STR create_self6;
 CHAR create_c1 = CHAR_zero;
 STR ret_val29;
 STR r5;
 PARSER err_self1;
 STR err_s1;
 PARSER fetch_self3;
 PARSER exit_self1;
 STR s3;
 PARSER unlock_stmt_self;
 AS_UNLOCK_STMT ret_val30;
 AS_UNLOCK_STMT res3;
 PARSER enter_self3;
 STR enter_s3;
 AS_UNLOCK_STMT create_self7;
 AS_UNLOCK_STMT ret_val31;
 PARSER source_loc_self6;
 SFILE_ID ret_val32 = SFILE_ID_zero;
 PARSER match_self2;
 INT match_t2 = INT_zero;
 TOKEN is_eq_self2 = TOKEN_zero;
 INT is_eq_y2 = INT_zero;
 BOOL ret_val33 = BOOL_zero;
 TOKEN create_self8 = TOKEN_zero;
 INT create_val2 = INT_zero;
 TOKEN ret_val34 = TOKEN_zero;
 TOKEN t2 = TOKEN_zero;
 PARSER exp_error_self2;
 STR exp_error_msg2;
 STR plus_self10;
 STR plus_sarg8;
 STR ret_val35;
 STR plus_self11;
 STR plus_sarg9;
 STR ret_val36;
 PARSER error_self2;
 STR error_msg2;
 PARSER source_loc_self7;
 SFILE_ID ret_val37 = SFILE_ID_zero;
 PROG set_eloc_self2;
 SFILE_ID set_eloc_l2 = SFILE_ID_zero;
 STR plus_self12;
 STR plus_sarg10;
 STR ret_val38;
 STR plus_self13;
 STR plus_sarg11;
 STR ret_val39;
 STR plus_self14;
 CHAR plus_arg2 = CHAR_zero;
 STR ret_val40;
 STR s4;
 CHAR str_self2 = CHAR_zero;
 STR ret_val41;
 STR create_self9;
 CHAR create_c2 = CHAR_zero;
 STR ret_val42;
 STR r6;
 PARSER err_self2;
 STR err_s2;
 PARSER fetch_self4;
 PARSER exit_self2;
 STR s5;
 PARSER sync_stmt_self;
 AS_SYNC_STMT ret_val43;
 AS_SYNC_STMT res4;
 PARSER enter_self4;
 STR enter_s4;
 PARSER match_self3;
 INT match_t3 = INT_zero;
 TOKEN is_eq_self3 = TOKEN_zero;
 INT is_eq_y3 = INT_zero;
 BOOL ret_val44 = BOOL_zero;
 TOKEN create_self10 = TOKEN_zero;
 INT create_val3 = INT_zero;
 TOKEN ret_val45 = TOKEN_zero;
 TOKEN t3 = TOKEN_zero;
 PARSER exp_error_self3;
 STR exp_error_msg3;
 STR plus_self15;
 STR plus_sarg12;
 STR ret_val46;
 STR plus_self16;
 STR plus_sarg13;
 STR ret_val47;
 PARSER error_self3;
 STR error_msg3;
 PARSER source_loc_self8;
 SFILE_ID ret_val48 = SFILE_ID_zero;
 PROG set_eloc_self3;
 SFILE_ID set_eloc_l3 = SFILE_ID_zero;
 STR plus_self17;
 STR plus_sarg14;
 STR ret_val49;
 STR plus_self18;
 STR plus_sarg15;
 STR ret_val50;
 STR plus_self19;
 CHAR plus_arg3 = CHAR_zero;
 STR ret_val51;
 STR s6;
 CHAR str_self3 = CHAR_zero;
 STR ret_val52;
 STR create_self11;
 CHAR create_c3 = CHAR_zero;
 STR ret_val53;
 STR r7;
 PARSER err_self3;
 STR err_s3;
 PARSER fetch_self5;
 AS_SYNC_STMT create_self12;
 AS_SYNC_STMT ret_val54;
 PARSER source_loc_self9;
 SFILE_ID ret_val55 = SFILE_ID_zero;
 PARSER exit_self3;
 STR s7;
 PARSER enter_self5;
 STR enter_s5;
 PARSER fetch_self6;
 PARSER match_self4;
 INT match_t4 = INT_zero;
 TOKEN is_eq_self4 = TOKEN_zero;
 INT is_eq_y4 = INT_zero;
 BOOL ret_val56 = BOOL_zero;
 TOKEN create_self13 = TOKEN_zero;
 INT create_val4 = INT_zero;
 TOKEN ret_val57 = TOKEN_zero;
 TOKEN t4 = TOKEN_zero;
 PARSER exp_error_self4;
 STR exp_error_msg4;
 STR plus_self20;
 STR plus_sarg16;
 STR ret_val58;
 STR plus_self21;
 STR plus_sarg17;
 STR ret_val59;
 PARSER error_self4;
 STR error_msg4;
 PARSER source_loc_self10;
 SFILE_ID ret_val60 = SFILE_ID_zero;
 PROG set_eloc_self4;
 SFILE_ID set_eloc_l4 = SFILE_ID_zero;
 STR plus_self22;
 STR plus_sarg18;
 STR ret_val61;
 STR plus_self23;
 STR plus_sarg19;
 STR ret_val62;
 STR plus_self24;
 CHAR plus_arg4 = CHAR_zero;
 STR ret_val63;
 STR s8;
 CHAR str_self4 = CHAR_zero;
 STR ret_val64;
 STR create_self14;
 CHAR create_c4 = CHAR_zero;
 STR ret_val65;
 STR r8;
 PARSER err_self4;
 STR err_s4;
 PARSER fetch_self7;
 PARSER break_stmt_self;
 AS_EXPR_STMT ret_val66;
 AS_EXPR_STMT res5;
 AS_EXPR_STMT create_self15;
 AS_EXPR_STMT ret_val67;
 PARSER source_loc_self11;
 SFILE_ID ret_val68 = SFILE_ID_zero;
 AS_BREAK_EXPR create_self16;
 AS_BREAK_EXPR ret_val69;
 PARSER match_self5;
 INT match_t5 = INT_zero;
 TOKEN is_eq_self5 = TOKEN_zero;
 INT is_eq_y5 = INT_zero;
 BOOL ret_val70 = BOOL_zero;
 TOKEN create_self17 = TOKEN_zero;
 INT create_val5 = INT_zero;
 TOKEN ret_val71 = TOKEN_zero;
 TOKEN t5 = TOKEN_zero;
 PARSER exp_error_self5;
 STR exp_error_msg5;
 STR plus_self25;
 STR plus_sarg20;
 STR ret_val72;
 STR plus_self26;
 STR plus_sarg21;
 STR ret_val73;
 PARSER error_self5;
 STR error_msg5;
 PARSER source_loc_self12;
 SFILE_ID ret_val74 = SFILE_ID_zero;
 PROG set_eloc_self5;
 SFILE_ID set_eloc_l5 = SFILE_ID_zero;
 STR plus_self27;
 STR plus_sarg22;
 STR ret_val75;
 STR plus_self28;
 STR plus_sarg23;
 STR ret_val76;
 STR plus_self29;
 CHAR plus_arg5 = CHAR_zero;
 STR ret_val77;
 STR s9;
 CHAR str_self5 = CHAR_zero;
 STR ret_val78;
 STR create_self18;
 CHAR create_c5 = CHAR_zero;
 STR ret_val79;
 STR r9;
 PARSER err_self5;
 STR err_s5;
 PARSER fetch_self8;
 PARSER exit_self4;
 STR s10;
 PARSER enter_self6;
 STR enter_s6;
 PARSER fetch_self9;
 PARSER match_self6;
 INT match_t6 = INT_zero;
 TOKEN is_eq_self6 = TOKEN_zero;
 INT is_eq_y6 = INT_zero;
 BOOL ret_val80 = BOOL_zero;
 TOKEN create_self19 = TOKEN_zero;
 INT create_val6 = INT_zero;
 TOKEN ret_val81 = TOKEN_zero;
 TOKEN t6 = TOKEN_zero;
 PARSER exp_error_self6;
 STR exp_error_msg6;
 STR plus_self30;
 STR plus_sarg24;
 STR ret_val82;
 STR plus_self31;
 STR plus_sarg25;
 STR ret_val83;
 PARSER error_self6;
 STR error_msg6;
 PARSER source_loc_self13;
 SFILE_ID ret_val84 = SFILE_ID_zero;
 PROG set_eloc_self6;
 SFILE_ID set_eloc_l6 = SFILE_ID_zero;
 STR plus_self32;
 STR plus_sarg26;
 STR ret_val85;
 STR plus_self33;
 STR plus_sarg27;
 STR ret_val86;
 STR plus_self34;
 CHAR plus_arg6 = CHAR_zero;
 STR ret_val87;
 STR s11;
 CHAR str_self6 = CHAR_zero;
 STR ret_val88;
 STR create_self20;
 CHAR create_c6 = CHAR_zero;
 STR ret_val89;
 STR r10;
 PARSER err_self6;
 STR err_s6;
 PARSER fetch_self10;
 PARSER break_stmt_self1;
 AS_EXPR_STMT ret_val90;
 AS_EXPR_STMT res6;
 AS_EXPR_STMT create_self21;
 AS_EXPR_STMT ret_val91;
 PARSER source_loc_self14;
 SFILE_ID ret_val92 = SFILE_ID_zero;
 AS_BREAK_EXPR create_self22;
 AS_BREAK_EXPR ret_val93;
 PARSER match_self7;
 INT match_t7 = INT_zero;
 TOKEN is_eq_self7 = TOKEN_zero;
 INT is_eq_y7 = INT_zero;
 BOOL ret_val94 = BOOL_zero;
 TOKEN create_self23 = TOKEN_zero;
 INT create_val7 = INT_zero;
 TOKEN ret_val95 = TOKEN_zero;
 TOKEN t7 = TOKEN_zero;
 PARSER exp_error_self7;
 STR exp_error_msg7;
 STR plus_self35;
 STR plus_sarg28;
 STR ret_val96;
 STR plus_self36;
 STR plus_sarg29;
 STR ret_val97;
 PARSER error_self7;
 STR error_msg7;
 PARSER source_loc_self15;
 SFILE_ID ret_val98 = SFILE_ID_zero;
 PROG set_eloc_self7;
 SFILE_ID set_eloc_l7 = SFILE_ID_zero;
 STR plus_self37;
 STR plus_sarg30;
 STR ret_val99;
 STR plus_self38;
 STR plus_sarg31;
 STR ret_val100;
 STR plus_self39;
 CHAR plus_arg7 = CHAR_zero;
 STR ret_val101;
 STR s12;
 CHAR str_self7 = CHAR_zero;
 STR ret_val102;
 STR create_self24;
 CHAR create_c7 = CHAR_zero;
 STR ret_val103;
 STR r11;
 PARSER err_self7;
 STR err_s7;
 PARSER fetch_self11;
 PARSER exit_self5;
 STR s13;
 PARSER fetch_self12;
 PARSER break_stmt_self2;
 AS_EXPR_STMT ret_val104;
 AS_EXPR_STMT res7;
 AS_EXPR_STMT create_self25;
 AS_EXPR_STMT ret_val105;
 PARSER source_loc_self16;
 SFILE_ID ret_val106 = SFILE_ID_zero;
 AS_BREAK_EXPR create_self26;
 AS_BREAK_EXPR ret_val107;
 TOKEN is_eq_self8 = TOKEN_zero;
 INT is_eq_y8 = INT_zero;
 BOOL ret_val108 = BOOL_zero;
 PARSER enter_self7;
 STR enter_s7;
 AS_ASSIGN_STMT create_self27;
 AS_ASSIGN_STMT ret_val109;
 PARSER source_loc_self17;
 SFILE_ID ret_val110 = SFILE_ID_zero;
 PARSER exit_self6;
 STR s14;
 PARSER enter_self8;
 STR enter_s8;
 AS_DEC_STMT create_self28;
 AS_DEC_STMT ret_val1111;
 PARSER source_loc_self18;
 SFILE_ID ret_val112 = SFILE_ID_zero;
 PARSER exit_self7;
 STR s15;
 PARSER enter_self9;
 STR enter_s9;
 AS_ASSIGN_STMT create_self29;
 AS_ASSIGN_STMT ret_val113;
 PARSER source_loc_self19;
 SFILE_ID ret_val114 = SFILE_ID_zero;
 PARSER exit_self8;
 STR s16;
 PARSER enter_self10;
 STR enter_s10;
 AS_ATTACH_STMT create_self30;
 AS_ATTACH_STMT ret_val115;
 PARSER source_loc_self20;
 SFILE_ID ret_val116 = SFILE_ID_zero;
 PARSER exit_self9;
 STR s17;
 TOKEN is_eq_self9 = TOKEN_zero;
 INT is_eq_y9 = INT_zero;
 BOOL ret_val117 = BOOL_zero;
 PARSER enter_self11;
 STR enter_s11;
 AS_DEC_STMT create_self31;
 AS_DEC_STMT ret_val118;
 PARSER source_loc_self21;
 SFILE_ID ret_val119 = SFILE_ID_zero;
 AS_DEC_STMT create_self32;
 AS_DEC_STMT ret_val120;
 PARSER source_loc_self22;
 SFILE_ID ret_val121 = SFILE_ID_zero;
 PARSER match_self8;
 INT match_t8 = INT_zero;
 TOKEN is_eq_self10 = TOKEN_zero;
 INT is_eq_y10 = INT_zero;
 BOOL ret_val122 = BOOL_zero;
 TOKEN create_self33 = TOKEN_zero;
 INT create_val8 = INT_zero;
 TOKEN ret_val123 = TOKEN_zero;
 TOKEN t8 = TOKEN_zero;
 PARSER exp_error_self8;
 STR exp_error_msg8;
 STR plus_self40;
 STR plus_sarg32;
 STR ret_val124;
 STR plus_self41;
 STR plus_sarg33;
 STR ret_val125;
 PARSER error_self8;
 STR error_msg8;
 PARSER source_loc_self23;
 SFILE_ID ret_val126 = SFILE_ID_zero;
 PROG set_eloc_self8;
 SFILE_ID set_eloc_l8 = SFILE_ID_zero;
 STR plus_self42;
 STR plus_sarg34;
 STR ret_val127;
 STR plus_self43;
 STR plus_sarg35;
 STR ret_val128;
 STR plus_self44;
 CHAR plus_arg8 = CHAR_zero;
 STR ret_val129;
 STR s18;
 CHAR str_self8 = CHAR_zero;
 STR ret_val130;
 STR create_self34;
 CHAR create_c8 = CHAR_zero;
 STR ret_val131;
 STR r12;
 PARSER err_self8;
 STR err_s8;
 PARSER fetch_self13;
 PARSER ident_self;
 IDENT ret_val132 = IDENT_zero;
 PARSER match_self9;
 INT match_t9 = INT_zero;
 TOKEN is_eq_self11 = TOKEN_zero;
 INT is_eq_y11 = INT_zero;
 BOOL ret_val133 = BOOL_zero;
 TOKEN create_self35 = TOKEN_zero;
 INT create_val9 = INT_zero;
 TOKEN ret_val134 = TOKEN_zero;
 TOKEN t9 = TOKEN_zero;
 PARSER exp_error_self9;
 STR exp_error_msg9;
 STR plus_self45;
 STR plus_sarg36;
 STR ret_val135;
 STR plus_self46;
 STR plus_sarg37;
 STR ret_val136;
 PARSER error_self9;
 STR error_msg9;
 PARSER source_loc_self24;
 SFILE_ID ret_val137 = SFILE_ID_zero;
 PROG set_eloc_self9;
 SFILE_ID set_eloc_l9 = SFILE_ID_zero;
 STR plus_self47;
 STR plus_sarg38;
 STR ret_val138;
 STR plus_self48;
 STR plus_sarg39;
 STR ret_val139;
 STR plus_self49;
 CHAR plus_arg9 = CHAR_zero;
 STR ret_val140;
 STR s19;
 CHAR str_self9 = CHAR_zero;
 STR ret_val141;
 STR create_self36;
 CHAR create_c9 = CHAR_zero;
 STR ret_val142;
 STR r13;
 PARSER err_self9;
 STR err_s9;
 PARSER fetch_self14;
 PARSER exit_self10;
 STR s20;
 AS_EXPR_STMT create_self37;
 AS_EXPR_STMT ret_val143;
 PARSER source_loc_self25;
 SFILE_ID ret_val144 = SFILE_ID_zero;
 PARSER exit_self11;
 STR s21;
 extern STR statement;
 AS_QUIT_STMT L2;
 OB L3;
 dAS_STMT L4;
 extern STR assertstatement;
 AS_ASSERT_STMT L5;
 OB L6;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L12;
 INT L13;
 OB L14;
 INT L15;
 extern STR raisestatement;
 AS_RAISE_STMT L17;
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
 extern STR unlockstatement;
 AS_UNLOCK_STMT L28;
 OB L29;
 INT L30;
 BOOL L311_;
 BOOL L32;
 BOOL L331_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L34;
 INT L35;
 OB L36;
 INT L37;
 extern STR syncstatement;
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
 AS_SYNC_STMT L48;
 OB L49;
 extern STR whilebexpression;
 INT L50;
 BOOL L511_;
 BOOL L52;
 BOOL L531_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L54;
 INT L55;
 OB L56;
 INT L57;
 dAS_EXPR L59;
 AS_EXPR_STMT L60;
 OB L61;
 AS_BREAK_EXPR L62;
 OB L63;
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
 extern STR untilbexpression;
 INT L73;
 BOOL L741_;
 BOOL L75;
 BOOL L761_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L77;
 INT L78;
 OB L79;
 INT L80;
 dAS_EXPR L83;
 AS_EXPR_STMT L84;
 OB L85;
 AS_BREAK_EXPR L86;
 OB L87;
 INT L88;
 BOOL L891_;
 BOOL L90;
 BOOL L911_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L92;
 INT L93;
 OB L94;
 INT L95;
 AS_EXPR_STMT L97;
 OB L98;
 AS_BREAK_EXPR L99;
 OB L100;
 INT L102;
 BOOL L1031_;
 BOOL L104;
 BOOL L1051_;
 extern STR assign642132529;
 AS_ASSIGN_STMT L106;
 OB L107;
 extern STR single1509197358;
 AS_DEC_STMT L108;
 OB L109;
 extern STR assignment;
 AS_ASSIGN_STMT L110;
 OB L111;
 extern STR forkst732695814;
 AS_ATTACH_STMT L112;
 OB L113;
 INT L114;
 BOOL L1151_;
 extern STR declaration1;
 AS_DEC_STMT L116;
 OB L117;
 AS_DEC_STMT L118;
 OB L119;
 INT L120;
 BOOL L1211_;
 BOOL L122;
 BOOL L1231_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L124;
 INT L125;
 OB L126;
 INT L127;
 dAS_STMT L129;
 INT L130;
 BOOL L1311_;
 BOOL L132;
 BOOL L1331_;
 extern STR expectedbutfound;
 extern STR in1;
 STR L134;
 INT L135;
 OB L136;
 INT L137;
 dAS_STMT L139;
 AS_EXPR_STMT L140;
 OB L141;
 dAS_STMT L142;
 enter_self = self;
 enter_s = ((STR) &statement);
 SATTR(enter_self,entered,FLISTS1425610062(ATTR(enter_self,entered), enter_s));
 source_loc_self = self;
 ret_val1 = SFILE_1594766561(SFILE_ID_zero);
 was_at = ret_val1;
 L11 = ATTR(self,next).val1;
 switch (L11) {
  case 17: 
   fetch_self = self;
   SATTR(fetch_self,next,SCANNE1792387248(ATTR(fetch_self,scanner)));
   res = ((dAS_STMT) PARSER164187557(self));
   break;
  case 22: 
   res = ((dAS_STMT) PARSER931477965(self));
   break;
  case 33: 
   res = ((dAS_STMT) PARSER1796842837(self));
   break;
  case 52: 
   res = ((dAS_STMT) PARSER1414885906(self));
   break;
  case 29: 
   fetch_self1 = self;
   SATTR(fetch_self1,next,SCANNE1792387248(ATTR(fetch_self1,scanner)));
   create_self = ((AS_QUIT_STMT) NULL);
   L3=ZALLOC(sizeof(struct AS_QUIT_STMT_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=AS_QUIT_STMT_tag;
   L2 = ((AS_QUIT_STMT) L3);
   ret_val2 = L2;
   res = ((dAS_STMT) ret_val2);
   source_loc_self1 = self;
   ret_val3 = SFILE_1594766561(SFILE_ID_zero);
   L4 = res;
   (*dAS_ST1190543153[TAG(L4)])(L4, ret_val3);
   break;
  case 8: 
   res = ((dAS_STMT) PARSER552741043(self));
   break;
  case 42: 
   res = ((dAS_STMT) PARSER670949263(self));
   break;
  case 5: 
   assert_stmt_self = self;
   enter_self1 = assert_stmt_self;
   enter_s1 = ((STR) &assertstatement);
   SATTR(enter_self1,entered,FLISTS1425610062(ATTR(enter_self1,entered), enter_s1));
   create_self1 = ((AS_ASSERT_STMT) NULL);
   L6=ZALLOC(sizeof(struct AS_ASSERT_STMT_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=AS_ASSERT_STMT_tag;
   L5 = ((AS_ASSERT_STMT) L6);
   ret_val5 = L5;
   res1 = ret_val5;
   source_loc_self2 = assert_stmt_self;
   ret_val6 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res1,source1,ret_val6);
   match_self = assert_stmt_self;
   match_t = PARSER985900141;
   is_eq_self = ATTR(match_self,next);
   is_eq_y = match_t;
   L7 = is_eq_self.val1;
   L81_=(L7)==(is_eq_y); 
   ret_val7 = L81_;
   L9 = ret_val7;
   L101_=!(L9); 
   if (L101_) {
    exp_error_self = match_self;
    create_self2 = TOKEN_zero;
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
    source_loc_self3 = error_self;
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
    create_self3 = ((STR) NULL);
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
    r4 = L12;
    SARR((STR)r4,0,create_c); 
    ;
    ret_val16 = r4;
    ret_val15 = ret_val16;
    s = ret_val15;
    ret_val14 = STR_ap1077157958(plus_self4, s, TRUE);
    err_s = ret_val14;
    PROG_err_STR(ATTR(err_self,prog), err_s);
   }
   fetch_self2 = match_self;
   SATTR(fetch_self2,next,SCANNE1792387248(ATTR(fetch_self2,scanner)));
   SATTR(res1,test1,PARSER403674127(assert_stmt_self));
   exit_self = assert_stmt_self;
   s1 = FLISTSTR_poprSTR(ATTR(exit_self,entered));
   ret_val4 = res1;
   res = ((dAS_STMT) ret_val4);
   break;
  case 28: 
   res = ((dAS_STMT) PARSER1254758060(self));
   break;
  case 30: 
   raise_stmt_self = self;
   enter_self2 = raise_stmt_self;
   enter_s2 = ((STR) &raisestatement);
   SATTR(enter_self2,entered,FLISTS1425610062(ATTR(enter_self2,entered), enter_s2));
   create_self4 = ((AS_RAISE_STMT) NULL);
   L18=ZALLOC(sizeof(struct AS_RAISE_STMT_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=AS_RAISE_STMT_tag;
   L17 = ((AS_RAISE_STMT) L18);
   ret_val18 = L17;
   res2 = ret_val18;
   source_loc_self4 = raise_stmt_self;
   ret_val19 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res2,source1,ret_val19);
   match_self1 = raise_stmt_self;
   match_t1 = PARSER_raise_tok;
   is_eq_self1 = ATTR(match_self1,next);
   is_eq_y1 = match_t1;
   L19 = is_eq_self1.val1;
   L201_=(L19)==(is_eq_y1); 
   ret_val20 = L201_;
   L21 = ret_val20;
   L221_=!(L21); 
   if (L221_) {
    exp_error_self1 = match_self1;
    create_self5 = TOKEN_zero;
    create_val1 = match_t1;
    t1.val1 = create_val1;
    ret_val21 = t1;
    exp_error_msg1 = TOKEN_strrSTR(ret_val21);
    error_self1 = exp_error_self1;
    plus_self5 = exp_error_msg1;
    plus_sarg4 = ((STR) &expectedbutfound);
    ret_val22 = STR_ap2004550586(plus_self5, plus_sarg4);
    plus_self6 = ret_val22;
    plus_sarg5 = TOKEN_strrSTR(ATTR(exp_error_self1,next));
    ret_val23 = STR_ap2004550586(plus_self6, plus_sarg5);
    error_msg1 = ret_val23;
    set_eloc_self1 = ATTR(error_self1,prog);
    source_loc_self5 = error_self1;
    ret_val24 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l1 = ret_val24;
    SATTR(set_eloc_self1,eloc,set_eloc_l1);
    err_self1 = error_self1;
    plus_self7 = error_msg1;
    plus_sarg6 = ((STR) &in1);
    ret_val25 = STR_ap2004550586(plus_self7, plus_sarg6);
    plus_self8 = ret_val25;
    plus_sarg7 = FLISTSTR_toprSTR(ATTR(error_self1,entered));
    ret_val26 = STR_ap2004550586(plus_self8, plus_sarg7);
    plus_self9 = ret_val26;
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
    r5 = L23;
    SARR((STR)r5,0,create_c1); 
    ;
    ret_val29 = r5;
    ret_val28 = ret_val29;
    s2 = ret_val28;
    ret_val27 = STR_ap1077157958(plus_self9, s2, TRUE);
    err_s1 = ret_val27;
    PROG_err_STR(ATTR(err_self1,prog), err_s1);
   }
   fetch_self3 = match_self1;
   SATTR(fetch_self3,next,SCANNE1792387248(ATTR(fetch_self3,scanner)));
   SATTR(res2,val1,PARSER403674127(raise_stmt_self));
   exit_self1 = raise_stmt_self;
   s3 = FLISTSTR_poprSTR(ATTR(exit_self1,entered));
   ret_val17 = res2;
   res = ((dAS_STMT) ret_val17);
   break;
  case 96: 
   res = ((dAS_STMT) PARSER1651016500(self));
   break;
  case 97: 
   res = ((dAS_STMT) PARSER2032164458(self));
   break;
  case 99: 
   res = ((dAS_STMT) PARSER1287362985(self));
   break;
  case 100: 
   unlock_stmt_self = self;
   enter_self3 = unlock_stmt_self;
   enter_s3 = ((STR) &unlockstatement);
   SATTR(enter_self3,entered,FLISTS1425610062(ATTR(enter_self3,entered), enter_s3));
   create_self7 = ((AS_UNLOCK_STMT) NULL);
   L29=ZALLOC(sizeof(struct AS_UNLOCK_STMT_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=AS_UNLOCK_STMT_tag;
   L28 = ((AS_UNLOCK_STMT) L29);
   ret_val31 = L28;
   res3 = ret_val31;
   source_loc_self6 = unlock_stmt_self;
   ret_val32 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res3,source1,ret_val32);
   match_self2 = unlock_stmt_self;
   match_t2 = PARSER836044841;
   is_eq_self2 = ATTR(match_self2,next);
   is_eq_y2 = match_t2;
   L30 = is_eq_self2.val1;
   L311_=(L30)==(is_eq_y2); 
   ret_val33 = L311_;
   L32 = ret_val33;
   L331_=!(L32); 
   if (L331_) {
    exp_error_self2 = match_self2;
    create_self8 = TOKEN_zero;
    create_val2 = match_t2;
    t2.val1 = create_val2;
    ret_val34 = t2;
    exp_error_msg2 = TOKEN_strrSTR(ret_val34);
    error_self2 = exp_error_self2;
    plus_self10 = exp_error_msg2;
    plus_sarg8 = ((STR) &expectedbutfound);
    ret_val35 = STR_ap2004550586(plus_self10, plus_sarg8);
    plus_self11 = ret_val35;
    plus_sarg9 = TOKEN_strrSTR(ATTR(exp_error_self2,next));
    ret_val36 = STR_ap2004550586(plus_self11, plus_sarg9);
    error_msg2 = ret_val36;
    set_eloc_self2 = ATTR(error_self2,prog);
    source_loc_self7 = error_self2;
    ret_val37 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l2 = ret_val37;
    SATTR(set_eloc_self2,eloc,set_eloc_l2);
    err_self2 = error_self2;
    plus_self12 = error_msg2;
    plus_sarg10 = ((STR) &in1);
    ret_val38 = STR_ap2004550586(plus_self12, plus_sarg10);
    plus_self13 = ret_val38;
    plus_sarg11 = FLISTSTR_toprSTR(ATTR(error_self2,entered));
    ret_val39 = STR_ap2004550586(plus_self13, plus_sarg11);
    plus_self14 = ret_val39;
    plus_arg2 = ')';
    str_self2 = plus_arg2;
    create_self9 = ((STR) NULL);
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
    r6 = L34;
    SARR((STR)r6,0,create_c2); 
    ;
    ret_val42 = r6;
    ret_val41 = ret_val42;
    s4 = ret_val41;
    ret_val40 = STR_ap1077157958(plus_self14, s4, TRUE);
    err_s2 = ret_val40;
    PROG_err_STR(ATTR(err_self2,prog), err_s2);
   }
   fetch_self4 = match_self2;
   SATTR(fetch_self4,next,SCANNE1792387248(ATTR(fetch_self4,scanner)));
   SATTR(res3,e,PARSER403674127(unlock_stmt_self));
   exit_self2 = unlock_stmt_self;
   s5 = FLISTSTR_poprSTR(ATTR(exit_self2,entered));
   ret_val30 = res3;
   res = ((dAS_STMT) ret_val30);
   break;
  case 104: 
   res = ((dAS_STMT) PARSER1613260578(self));
   break;
  case 94: 
   res = ((dAS_STMT) PARSER2069741717(self));
   break;
  case 102: 
   sync_stmt_self = self;
   enter_self4 = sync_stmt_self;
   enter_s4 = ((STR) &syncstatement);
   SATTR(enter_self4,entered,FLISTS1425610062(ATTR(enter_self4,entered), enter_s4));
   match_self3 = sync_stmt_self;
   match_t3 = PARSER_sync_tok;
   is_eq_self3 = ATTR(match_self3,next);
   is_eq_y3 = match_t3;
   L39 = is_eq_self3.val1;
   L401_=(L39)==(is_eq_y3); 
   ret_val44 = L401_;
   L41 = ret_val44;
   L421_=!(L41); 
   if (L421_) {
    exp_error_self3 = match_self3;
    create_self10 = TOKEN_zero;
    create_val3 = match_t3;
    t3.val1 = create_val3;
    ret_val45 = t3;
    exp_error_msg3 = TOKEN_strrSTR(ret_val45);
    error_self3 = exp_error_self3;
    plus_self15 = exp_error_msg3;
    plus_sarg12 = ((STR) &expectedbutfound);
    ret_val46 = STR_ap2004550586(plus_self15, plus_sarg12);
    plus_self16 = ret_val46;
    plus_sarg13 = TOKEN_strrSTR(ATTR(exp_error_self3,next));
    ret_val47 = STR_ap2004550586(plus_self16, plus_sarg13);
    error_msg3 = ret_val47;
    set_eloc_self3 = ATTR(error_self3,prog);
    source_loc_self8 = error_self3;
    ret_val48 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l3 = ret_val48;
    SATTR(set_eloc_self3,eloc,set_eloc_l3);
    err_self3 = error_self3;
    plus_self17 = error_msg3;
    plus_sarg14 = ((STR) &in1);
    ret_val49 = STR_ap2004550586(plus_self17, plus_sarg14);
    plus_self18 = ret_val49;
    plus_sarg15 = FLISTSTR_toprSTR(ATTR(error_self3,entered));
    ret_val50 = STR_ap2004550586(plus_self18, plus_sarg15);
    plus_self19 = ret_val50;
    plus_arg3 = ')';
    str_self3 = plus_arg3;
    create_self11 = ((STR) NULL);
    create_c3 = str_self3;
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
    r7 = L43;
    SARR((STR)r7,0,create_c3); 
    ;
    ret_val53 = r7;
    ret_val52 = ret_val53;
    s6 = ret_val52;
    ret_val51 = STR_ap1077157958(plus_self19, s6, TRUE);
    err_s3 = ret_val51;
    PROG_err_STR(ATTR(err_self3,prog), err_s3);
   }
   fetch_self5 = match_self3;
   SATTR(fetch_self5,next,SCANNE1792387248(ATTR(fetch_self5,scanner)));
   create_self12 = ((AS_SYNC_STMT) NULL);
   L49=ZALLOC(sizeof(struct AS_SYNC_STMT_struct));
   if (L49==NULL) FATAL("Unable to allocate more memory");
   ((OB)L49)->header.tag=AS_SYNC_STMT_tag;
   L48 = ((AS_SYNC_STMT) L49);
   ret_val54 = L48;
   res4 = ret_val54;
   source_loc_self9 = sync_stmt_self;
   ret_val55 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res4,source1,ret_val55);
   exit_self3 = sync_stmt_self;
   s7 = FLISTSTR_poprSTR(ATTR(exit_self3,entered));
   ret_val43 = res4;
   res = ((dAS_STMT) ret_val43);
   break;
  case 51: 
   enter_self5 = self;
   enter_s5 = ((STR) &whilebexpression);
   SATTR(enter_self5,entered,FLISTS1425610062(ATTR(enter_self5,entered), enter_s5));
   fetch_self6 = self;
   SATTR(fetch_self6,next,SCANNE1792387248(ATTR(fetch_self6,scanner)));
   match_self4 = self;
   match_t4 = PARSER1209676627;
   is_eq_self4 = ATTR(match_self4,next);
   is_eq_y4 = match_t4;
   L50 = is_eq_self4.val1;
   L511_=(L50)==(is_eq_y4); 
   ret_val56 = L511_;
   L52 = ret_val56;
   L531_=!(L52); 
   if (L531_) {
    exp_error_self4 = match_self4;
    create_self13 = TOKEN_zero;
    create_val4 = match_t4;
    t4.val1 = create_val4;
    ret_val57 = t4;
    exp_error_msg4 = TOKEN_strrSTR(ret_val57);
    error_self4 = exp_error_self4;
    plus_self20 = exp_error_msg4;
    plus_sarg16 = ((STR) &expectedbutfound);
    ret_val58 = STR_ap2004550586(plus_self20, plus_sarg16);
    plus_self21 = ret_val58;
    plus_sarg17 = TOKEN_strrSTR(ATTR(exp_error_self4,next));
    ret_val59 = STR_ap2004550586(plus_self21, plus_sarg17);
    error_msg4 = ret_val59;
    set_eloc_self4 = ATTR(error_self4,prog);
    source_loc_self10 = error_self4;
    ret_val60 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l4 = ret_val60;
    SATTR(set_eloc_self4,eloc,set_eloc_l4);
    err_self4 = error_self4;
    plus_self22 = error_msg4;
    plus_sarg18 = ((STR) &in1);
    ret_val61 = STR_ap2004550586(plus_self22, plus_sarg18);
    plus_self23 = ret_val61;
    plus_sarg19 = FLISTSTR_toprSTR(ATTR(error_self4,entered));
    ret_val62 = STR_ap2004550586(plus_self23, plus_sarg19);
    plus_self24 = ret_val62;
    plus_arg4 = ')';
    str_self4 = plus_arg4;
    create_self14 = ((STR) NULL);
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
    r8 = L54;
    SARR((STR)r8,0,create_c4); 
    ;
    ret_val65 = r8;
    ret_val64 = ret_val65;
    s8 = ret_val64;
    ret_val63 = STR_ap1077157958(plus_self24, s8, TRUE);
    err_s4 = ret_val63;
    PROG_err_STR(ATTR(err_self4,prog), err_s4);
   }
   fetch_self7 = match_self4;
   SATTR(fetch_self7,next,SCANNE1792387248(ATTR(fetch_self7,scanner)));
   L59 = PARSER403674127(self);
   break_stmt_self = self;
   create_self15 = ((AS_EXPR_STMT) NULL);
   L61=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
   if (L61==NULL) FATAL("Unable to allocate more memory");
   ((OB)L61)->header.tag=AS_EXPR_STMT_tag;
   L60 = ((AS_EXPR_STMT) L61);
   ret_val67 = L60;
   res5 = ret_val67;
   source_loc_self11 = break_stmt_self;
   ret_val68 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res5,source1,ret_val68);
   create_self16 = ((AS_BREAK_EXPR) NULL);
   L63=ZALLOC(sizeof(struct AS_BREAK_EXPR_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   ((OB)L63)->header.tag=AS_BREAK_EXPR_tag;
   L62 = ((AS_BREAK_EXPR) L63);
   ret_val69 = L62;
   SATTR(res5,e,((dAS_EXPR) ret_val69));
   ret_val66 = res5;
   res = ((dAS_STMT) PARSER1465700283(self, L59, ((dAS_STMT) NULL), ((dAS_STMT) ret_val66)));
   match_self5 = self;
   match_t5 = PARSER1207020061;
   is_eq_self5 = ATTR(match_self5,next);
   is_eq_y5 = match_t5;
   L64 = is_eq_self5.val1;
   L651_=(L64)==(is_eq_y5); 
   ret_val70 = L651_;
   L66 = ret_val70;
   L671_=!(L66); 
   if (L671_) {
    exp_error_self5 = match_self5;
    create_self17 = TOKEN_zero;
    create_val5 = match_t5;
    t5.val1 = create_val5;
    ret_val71 = t5;
    exp_error_msg5 = TOKEN_strrSTR(ret_val71);
    error_self5 = exp_error_self5;
    plus_self25 = exp_error_msg5;
    plus_sarg20 = ((STR) &expectedbutfound);
    ret_val72 = STR_ap2004550586(plus_self25, plus_sarg20);
    plus_self26 = ret_val72;
    plus_sarg21 = TOKEN_strrSTR(ATTR(exp_error_self5,next));
    ret_val73 = STR_ap2004550586(plus_self26, plus_sarg21);
    error_msg5 = ret_val73;
    set_eloc_self5 = ATTR(error_self5,prog);
    source_loc_self12 = error_self5;
    ret_val74 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l5 = ret_val74;
    SATTR(set_eloc_self5,eloc,set_eloc_l5);
    err_self5 = error_self5;
    plus_self27 = error_msg5;
    plus_sarg22 = ((STR) &in1);
    ret_val75 = STR_ap2004550586(plus_self27, plus_sarg22);
    plus_self28 = ret_val75;
    plus_sarg23 = FLISTSTR_toprSTR(ATTR(error_self5,entered));
    ret_val76 = STR_ap2004550586(plus_self28, plus_sarg23);
    plus_self29 = ret_val76;
    plus_arg5 = ')';
    str_self5 = plus_arg5;
    create_self18 = ((STR) NULL);
    create_c5 = str_self5;
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
    r9 = L68;
    SARR((STR)r9,0,create_c5); 
    ;
    ret_val79 = r9;
    ret_val78 = ret_val79;
    s9 = ret_val78;
    ret_val77 = STR_ap1077157958(plus_self29, s9, TRUE);
    err_s5 = ret_val77;
    PROG_err_STR(ATTR(err_self5,prog), err_s5);
   }
   fetch_self8 = match_self5;
   SATTR(fetch_self8,next,SCANNE1792387248(ATTR(fetch_self8,scanner)));
   exit_self4 = self;
   s10 = FLISTSTR_poprSTR(ATTR(exit_self4,entered));
   break;
  case 43: 
   enter_self6 = self;
   enter_s6 = ((STR) &untilbexpression);
   SATTR(enter_self6,entered,FLISTS1425610062(ATTR(enter_self6,entered), enter_s6));
   fetch_self9 = self;
   SATTR(fetch_self9,next,SCANNE1792387248(ATTR(fetch_self9,scanner)));
   match_self6 = self;
   match_t6 = PARSER1209676627;
   is_eq_self6 = ATTR(match_self6,next);
   is_eq_y6 = match_t6;
   L73 = is_eq_self6.val1;
   L741_=(L73)==(is_eq_y6); 
   ret_val80 = L741_;
   L75 = ret_val80;
   L761_=!(L75); 
   if (L761_) {
    exp_error_self6 = match_self6;
    create_self19 = TOKEN_zero;
    create_val6 = match_t6;
    t6.val1 = create_val6;
    ret_val81 = t6;
    exp_error_msg6 = TOKEN_strrSTR(ret_val81);
    error_self6 = exp_error_self6;
    plus_self30 = exp_error_msg6;
    plus_sarg24 = ((STR) &expectedbutfound);
    ret_val82 = STR_ap2004550586(plus_self30, plus_sarg24);
    plus_self31 = ret_val82;
    plus_sarg25 = TOKEN_strrSTR(ATTR(exp_error_self6,next));
    ret_val83 = STR_ap2004550586(plus_self31, plus_sarg25);
    error_msg6 = ret_val83;
    set_eloc_self6 = ATTR(error_self6,prog);
    source_loc_self13 = error_self6;
    ret_val84 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l6 = ret_val84;
    SATTR(set_eloc_self6,eloc,set_eloc_l6);
    err_self6 = error_self6;
    plus_self32 = error_msg6;
    plus_sarg26 = ((STR) &in1);
    ret_val85 = STR_ap2004550586(plus_self32, plus_sarg26);
    plus_self33 = ret_val85;
    plus_sarg27 = FLISTSTR_toprSTR(ATTR(error_self6,entered));
    ret_val86 = STR_ap2004550586(plus_self33, plus_sarg27);
    plus_self34 = ret_val86;
    plus_arg6 = ')';
    str_self6 = plus_arg6;
    create_self20 = ((STR) NULL);
    create_c6 = str_self6;
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
    SARR((STR)r10,0,create_c6); 
    ;
    ret_val89 = r10;
    ret_val88 = ret_val89;
    s11 = ret_val88;
    ret_val87 = STR_ap1077157958(plus_self34, s11, TRUE);
    err_s6 = ret_val87;
    PROG_err_STR(ATTR(err_self6,prog), err_s6);
   }
   fetch_self10 = match_self6;
   SATTR(fetch_self10,next,SCANNE1792387248(ATTR(fetch_self10,scanner)));
   L83 = PARSER403674127(self);
   break_stmt_self1 = self;
   create_self21 = ((AS_EXPR_STMT) NULL);
   L85=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
   if (L85==NULL) FATAL("Unable to allocate more memory");
   ((OB)L85)->header.tag=AS_EXPR_STMT_tag;
   L84 = ((AS_EXPR_STMT) L85);
   ret_val91 = L84;
   res6 = ret_val91;
   source_loc_self14 = break_stmt_self1;
   ret_val92 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res6,source1,ret_val92);
   create_self22 = ((AS_BREAK_EXPR) NULL);
   L87=ZALLOC(sizeof(struct AS_BREAK_EXPR_struct));
   if (L87==NULL) FATAL("Unable to allocate more memory");
   ((OB)L87)->header.tag=AS_BREAK_EXPR_tag;
   L86 = ((AS_BREAK_EXPR) L87);
   ret_val93 = L86;
   SATTR(res6,e,((dAS_EXPR) ret_val93));
   ret_val90 = res6;
   res = ((dAS_STMT) PARSER1465700283(self, L83, ((dAS_STMT) ret_val90), ((dAS_STMT) NULL)));
   match_self7 = self;
   match_t7 = PARSER1207020061;
   is_eq_self7 = ATTR(match_self7,next);
   is_eq_y7 = match_t7;
   L88 = is_eq_self7.val1;
   L891_=(L88)==(is_eq_y7); 
   ret_val94 = L891_;
   L90 = ret_val94;
   L911_=!(L90); 
   if (L911_) {
    exp_error_self7 = match_self7;
    create_self23 = TOKEN_zero;
    create_val7 = match_t7;
    t7.val1 = create_val7;
    ret_val95 = t7;
    exp_error_msg7 = TOKEN_strrSTR(ret_val95);
    error_self7 = exp_error_self7;
    plus_self35 = exp_error_msg7;
    plus_sarg28 = ((STR) &expectedbutfound);
    ret_val96 = STR_ap2004550586(plus_self35, plus_sarg28);
    plus_self36 = ret_val96;
    plus_sarg29 = TOKEN_strrSTR(ATTR(exp_error_self7,next));
    ret_val97 = STR_ap2004550586(plus_self36, plus_sarg29);
    error_msg7 = ret_val97;
    set_eloc_self7 = ATTR(error_self7,prog);
    source_loc_self15 = error_self7;
    ret_val98 = SFILE_1594766561(SFILE_ID_zero);
    set_eloc_l7 = ret_val98;
    SATTR(set_eloc_self7,eloc,set_eloc_l7);
    err_self7 = error_self7;
    plus_self37 = error_msg7;
    plus_sarg30 = ((STR) &in1);
    ret_val99 = STR_ap2004550586(plus_self37, plus_sarg30);
    plus_self38 = ret_val99;
    plus_sarg31 = FLISTSTR_toprSTR(ATTR(error_self7,entered));
    ret_val100 = STR_ap2004550586(plus_self38, plus_sarg31);
    plus_self39 = ret_val100;
    plus_arg7 = ')';
    str_self7 = plus_arg7;
    create_self24 = ((STR) NULL);
    create_c7 = str_self7;
    L93 = 1;
    L95=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L93)*sizeof(CHAR);
    L94=ZALLOC_LEAF_BIG(L95);
    if (L94==NULL) FATAL("Unable to allocate more memory");
    memset(L94,0,L95);
    ((OB)L94)->header.tag=STR_tag;
#ifdef DESTROY_CHK

      ((OB)L94)->header.destroyed=0;
#endif

    L92 = ((STR) L94);
    L92->asize = L93;
    r11 = L92;
    SARR((STR)r11,0,create_c7); 
    ;
    ret_val103 = r11;
    ret_val102 = ret_val103;
    s12 = ret_val102;
    ret_val101 = STR_ap1077157958(plus_self39, s12, TRUE);
    err_s7 = ret_val101;
    PROG_err_STR(ATTR(err_self7,prog), err_s7);
   }
   fetch_self11 = match_self7;
   SATTR(fetch_self11,next,SCANNE1792387248(ATTR(fetch_self11,scanner)));
   exit_self5 = self;
   s13 = FLISTSTR_poprSTR(ATTR(exit_self5,entered));
   break;
  case 7: 
   fetch_self12 = self;
   SATTR(fetch_self12,next,SCANNE1792387248(ATTR(fetch_self12,scanner)));
   break_stmt_self2 = self;
   create_self25 = ((AS_EXPR_STMT) NULL);
   L98=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
   if (L98==NULL) FATAL("Unable to allocate more memory");
   ((OB)L98)->header.tag=AS_EXPR_STMT_tag;
   L97 = ((AS_EXPR_STMT) L98);
   ret_val105 = L97;
   res7 = ret_val105;
   source_loc_self16 = break_stmt_self2;
   ret_val106 = SFILE_1594766561(SFILE_ID_zero);
   SATTR(res7,source1,ret_val106);
   create_self26 = ((AS_BREAK_EXPR) NULL);
   L100=ZALLOC(sizeof(struct AS_BREAK_EXPR_struct));
   if (L100==NULL) FATAL("Unable to allocate more memory");
   ((OB)L100)->header.tag=AS_BREAK_EXPR_tag;
   L99 = ((AS_BREAK_EXPR) L100);
   ret_val107 = L99;
   SATTR(res7,e,((dAS_EXPR) ret_val107));
   ret_val104 = res7;
   res = ((dAS_STMT) ret_val104);
   break;
  default: ;
   x = PARSER403674127(self);
   if (PARSER2128430730(self, PARSER_colon_tok)) {
    is_eq_self8 = ATTR(self,next);
    is_eq_y8 = PARSER265941484;
    L102 = is_eq_self8.val1;
    L1031_=(L102)==(is_eq_y8); 
    ret_val108 = L1031_;
    L104 = ret_val108;
    L1051_=!(L104); 
    if (L1051_) {
     tp = PARSER2028498823(self);
    }
    if (PARSER2128430730(self, PARSER265941484)) {
     enter_self7 = self;
     enter_s7 = ((STR) &assign642132529);
     SATTR(enter_self7,entered,FLISTS1425610062(ATTR(enter_self7,entered), enter_s7));
     create_self27 = ((AS_ASSIGN_STMT) NULL);
     L107=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
     if (L107==NULL) FATAL("Unable to allocate more memory");
     ((OB)L107)->header.tag=AS_ASSIGN_STMT_tag;
     L106 = ((AS_ASSIGN_STMT) L107);
     ret_val109 = L106;
     r = ret_val109;
     source_loc_self17 = self;
     ret_val110 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(r,source1,ret_val110);
     SATTR(r,name1,PARSER2046006032(self, x));
     SATTR(r,tp,tp);
     SATTR(r,rhs,PARSER403674127(self));
     res = ((dAS_STMT) r);
     exit_self6 = self;
     s14 = FLISTSTR_poprSTR(ATTR(exit_self6,entered));
    }
    else {
     enter_self8 = self;
     enter_s8 = ((STR) &single1509197358);
     SATTR(enter_self8,entered,FLISTS1425610062(ATTR(enter_self8,entered), enter_s8));
     create_self28 = ((AS_DEC_STMT) NULL);
     L109=ZALLOC(sizeof(struct AS_DEC_STMT_struct));
     if (L109==NULL) FATAL("Unable to allocate more memory");
     ((OB)L109)->header.tag=AS_DEC_STMT_tag;
     L108 = ((AS_DEC_STMT) L109);
     ret_val1111 = L108;
     sdecl_res = ret_val1111;
     source_loc_self18 = self;
     ret_val112 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(sdecl_res,source1,ret_val112);
     SATTR(sdecl_res,name1,PARSER2046006032(self, x));
     SATTR(sdecl_res,tp,tp);
     res = ((dAS_STMT) sdecl_res);
     exit_self7 = self;
     s15 = FLISTSTR_poprSTR(ATTR(exit_self7,entered));
    }
   }
   else {
    if (PARSER2128430730(self, PARSER265941484)) {
     enter_self9 = self;
     enter_s9 = ((STR) &assignment);
     SATTR(enter_self9,entered,FLISTS1425610062(ATTR(enter_self9,entered), enter_s9));
     create_self29 = ((AS_ASSIGN_STMT) NULL);
     L111=ZALLOC(sizeof(struct AS_ASSIGN_STMT_struct));
     if (L111==NULL) FATAL("Unable to allocate more memory");
     ((OB)L111)->header.tag=AS_ASSIGN_STMT_tag;
     L110 = ((AS_ASSIGN_STMT) L111);
     ret_val113 = L110;
     r1 = ret_val113;
     source_loc_self19 = self;
     ret_val114 = SFILE_1594766561(SFILE_ID_zero);
     SATTR(r1,source1,ret_val114);
     SATTR(r1,lhs_expr,x);
     SATTR(r1,rhs,PARSER403674127(self));
     res = ((dAS_STMT) r1);
     exit_self8 = self;
     s16 = FLISTSTR_poprSTR(ATTR(exit_self8,entered));
    }
    else {
     if (PARSER2128430730(self, PARSER1338823514)) {
      enter_self10 = self;
      enter_s10 = ((STR) &forkst732695814);
      SATTR(enter_self10,entered,FLISTS1425610062(ATTR(enter_self10,entered), enter_s10));
      create_self30 = ((AS_ATTACH_STMT) NULL);
      L113=ZALLOC(sizeof(struct AS_ATTACH_STMT_struct));
      if (L113==NULL) FATAL("Unable to allocate more memory");
      ((OB)L113)->header.tag=AS_ATTACH_STMT_tag;
      L112 = ((AS_ATTACH_STMT) L113);
      ret_val115 = L112;
      r2 = ret_val115;
      source_loc_self20 = self;
      ret_val116 = SFILE_1594766561(SFILE_ID_zero);
      SATTR(r2,source1,ret_val116);
      SATTR(r2,lhs,x);
      SATTR(r2,rhs,PARSER403674127(self));
      res = ((dAS_STMT) r2);
      exit_self9 = self;
      s17 = FLISTSTR_poprSTR(ATTR(exit_self9,entered));
     }
     else {
      is_eq_self9 = ATTR(self,next);
      is_eq_y9 = PARSER_comma_tok;
      L114 = is_eq_self9.val1;
      L1151_=(L114)==(is_eq_y9); 
      ret_val117 = L1151_;
      if (ret_val117) {
       enter_self11 = self;
       enter_s11 = ((STR) &declaration1);
       SATTR(enter_self11,entered,FLISTS1425610062(ATTR(enter_self11,entered), enter_s11));
       create_self31 = ((AS_DEC_STMT) NULL);
       L117=ZALLOC(sizeof(struct AS_DEC_STMT_struct));
       if (L117==NULL) FATAL("Unable to allocate more memory");
       ((OB)L117)->header.tag=AS_DEC_STMT_tag;
       L116 = ((AS_DEC_STMT) L117);
       ret_val118 = L116;
       decl_res = ret_val118;
       source_loc_self21 = self;
       ret_val119 = SFILE_1594766561(SFILE_ID_zero);
       SATTR(decl_res,source1,ret_val119);
       SATTR(decl_res,name1,PARSER2046006032(self, x));
       res = ((dAS_STMT) decl_res);
       while (1) {
        if (PARSER2128430730(self, PARSER_comma_tok)) {
        }
        else {
         goto after_loop;
        }
        create_self32 = ((AS_DEC_STMT) NULL);
        L119=ZALLOC(sizeof(struct AS_DEC_STMT_struct));
        if (L119==NULL) FATAL("Unable to allocate more memory");
        ((OB)L119)->header.tag=AS_DEC_STMT_tag;
        L118 = ((AS_DEC_STMT) L119);
        ret_val120 = L118;
        newdec = ret_val120;
        source_loc_self22 = self;
        ret_val121 = SFILE_1594766561(SFILE_ID_zero);
        SATTR(newdec,source1,ret_val121);
        match_self8 = self;
        match_t8 = PARSER_ident_tok;
        is_eq_self10 = ATTR(match_self8,next);
        is_eq_y10 = match_t8;
        L120 = is_eq_self10.val1;
        L1211_=(L120)==(is_eq_y10); 
        ret_val122 = L1211_;
        L122 = ret_val122;
        L1231_=!(L122); 
        if (L1231_) {
         exp_error_self8 = match_self8;
         create_self33 = TOKEN_zero;
         create_val8 = match_t8;
         t8.val1 = create_val8;
         ret_val123 = t8;
         exp_error_msg8 = TOKEN_strrSTR(ret_val123);
         error_self8 = exp_error_self8;
         plus_self40 = exp_error_msg8;
         plus_sarg32 = ((STR) &expectedbutfound);
         ret_val124 = STR_ap2004550586(plus_self40, plus_sarg32);
         plus_self41 = ret_val124;
         plus_sarg33 = TOKEN_strrSTR(ATTR(exp_error_self8,next));
         ret_val125 = STR_ap2004550586(plus_self41, plus_sarg33);
         error_msg8 = ret_val125;
         set_eloc_self8 = ATTR(error_self8,prog);
         source_loc_self23 = error_self8;
         ret_val126 = SFILE_1594766561(SFILE_ID_zero);
         set_eloc_l8 = ret_val126;
         SATTR(set_eloc_self8,eloc,set_eloc_l8);
         err_self8 = error_self8;
         plus_self42 = error_msg8;
         plus_sarg34 = ((STR) &in1);
         ret_val127 = STR_ap2004550586(plus_self42, plus_sarg34);
         plus_self43 = ret_val127;
         plus_sarg35 = FLISTSTR_toprSTR(ATTR(error_self8,entered));
         ret_val128 = STR_ap2004550586(plus_self43, plus_sarg35);
         plus_self44 = ret_val128;
         plus_arg8 = ')';
         str_self8 = plus_arg8;
         create_self34 = ((STR) NULL);
         create_c8 = str_self8;
         L125 = 1;
         L127=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L125)*sizeof(CHAR);
         L126=ZALLOC_LEAF_BIG(L127);
         if (L126==NULL) FATAL("Unable to allocate more memory");
         memset(L126,0,L127);
         ((OB)L126)->header.tag=STR_tag;
#ifdef DESTROY_CHK

           ((OB)L126)->header.destroyed=0;
#endif

         L124 = ((STR) L126);
         L124->asize = L125;
         r12 = L124;
         SARR((STR)r12,0,create_c8); 
         ;
         ret_val131 = r12;
         ret_val130 = ret_val131;
         s18 = ret_val130;
         ret_val129 = STR_ap1077157958(plus_self44, s18, TRUE);
         err_s8 = ret_val129;
         PROG_err_STR(ATTR(err_self8,prog), err_s8);
        }
        fetch_self13 = match_self8;
        SATTR(fetch_self13,next,SCANNE1792387248(ATTR(fetch_self13,scanner)));
        ident_self = self;
        ret_val132 = ATTR(ATTR(ident_self,scanner),lex_value);
        SATTR(newdec,name1,ret_val132);
        if ((res==((dAS_STMT) NULL))) {
         res = ((dAS_STMT) newdec);
        }
        else {
         L129 = res;
         (*dAS_ST1454156204[TAG(L129)])(L129, ((dAS_STMT) newdec));
        }
       }
       after_loop: ;
       match_self9 = self;
       match_t9 = PARSER_colon_tok;
       is_eq_self11 = ATTR(match_self9,next);
       is_eq_y11 = match_t9;
       L130 = is_eq_self11.val1;
       L1311_=(L130)==(is_eq_y11); 
       ret_val133 = L1311_;
       L132 = ret_val133;
       L1331_=!(L132); 
       if (L1331_) {
        exp_error_self9 = match_self9;
        create_self35 = TOKEN_zero;
        create_val9 = match_t9;
        t9.val1 = create_val9;
        ret_val134 = t9;
        exp_error_msg9 = TOKEN_strrSTR(ret_val134);
        error_self9 = exp_error_self9;
        plus_self45 = exp_error_msg9;
        plus_sarg36 = ((STR) &expectedbutfound);
        ret_val135 = STR_ap2004550586(plus_self45, plus_sarg36);
        plus_self46 = ret_val135;
        plus_sarg37 = TOKEN_strrSTR(ATTR(exp_error_self9,next));
        ret_val136 = STR_ap2004550586(plus_self46, plus_sarg37);
        error_msg9 = ret_val136;
        set_eloc_self9 = ATTR(error_self9,prog);
        source_loc_self24 = error_self9;
        ret_val137 = SFILE_1594766561(SFILE_ID_zero);
        set_eloc_l9 = ret_val137;
        SATTR(set_eloc_self9,eloc,set_eloc_l9);
        err_self9 = error_self9;
        plus_self47 = error_msg9;
        plus_sarg38 = ((STR) &in1);
        ret_val138 = STR_ap2004550586(plus_self47, plus_sarg38);
        plus_self48 = ret_val138;
        plus_sarg39 = FLISTSTR_toprSTR(ATTR(error_self9,entered));
        ret_val139 = STR_ap2004550586(plus_self48, plus_sarg39);
        plus_self49 = ret_val139;
        plus_arg9 = ')';
        str_self9 = plus_arg9;
        create_self36 = ((STR) NULL);
        create_c9 = str_self9;
        L135 = 1;
        L137=(sizeof(struct STR_struct)+1-sizeof(CHAR))+(L135)*sizeof(CHAR);
        L136=ZALLOC_LEAF_BIG(L137);
        if (L136==NULL) FATAL("Unable to allocate more memory");
        memset(L136,0,L137);
        ((OB)L136)->header.tag=STR_tag;
#ifdef DESTROY_CHK

          ((OB)L136)->header.destroyed=0;
#endif

        L134 = ((STR) L136);
        L134->asize = L135;
        r13 = L134;
        SARR((STR)r13,0,create_c9); 
        ;
        ret_val142 = r13;
        ret_val141 = ret_val142;
        s19 = ret_val141;
        ret_val140 = STR_ap1077157958(plus_self49, s19, TRUE);
        err_s9 = ret_val140;
        PROG_err_STR(ATTR(err_self9,prog), err_s9);
       }
       fetch_self14 = match_self9;
       SATTR(fetch_self14,next,SCANNE1792387248(ATTR(fetch_self14,scanner)));
       tp2 = PARSER2028498823(self);
       p = ((dAS_STMT) decl_res);
       while (1) {
        if ((p==((dAS_STMT) NULL))) {
         goto after_loop1;
        }
        switch (TAG(p)) {
         case AS_DEC_STMT_tag:
          SATTR(((AS_DEC_STMT) p),tp,tp2); break;
         default: ;
          FATAL("No applicable type in typecase\nin PARSER::stmt:$AS_STMT\n./Front/parse.sa:1184:23");
        }
        L139 = p;
        p = (*dAS_ST460279343[TAG(L139)])(L139);
       }
       after_loop1: ;
       exit_self10 = self;
       s20 = FLISTSTR_poprSTR(ATTR(exit_self10,entered));
      }
      else {
       create_self37 = ((AS_EXPR_STMT) NULL);
       L141=ZALLOC(sizeof(struct AS_EXPR_STMT_struct));
       if (L141==NULL) FATAL("Unable to allocate more memory");
       ((OB)L141)->header.tag=AS_EXPR_STMT_tag;
       L140 = ((AS_EXPR_STMT) L141);
       ret_val143 = L140;
       r3 = ret_val143;
       source_loc_self25 = self;
       ret_val144 = SFILE_1594766561(SFILE_ID_zero);
       SATTR(r3,source1,ret_val144);
       SATTR(r3,e,x);
       res = ((dAS_STMT) r3);
      }
     }
    }
   }
 }
 L142 = res;
 (*dAS_ST1190543153[TAG(L142)])(L142, was_at);
 exit_self11 = self;
 s21 = FLISTSTR_poprSTR(ATTR(exit_self11,entered));
 ret_val = res;
 return ret_val;
}
