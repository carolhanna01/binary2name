#include "sather.h"

/* Layouts */

typedef struct dAS_CLASS_ELT_struct {
 OB_HEADER header;
 } *dAS_CLASS_ELT;

typedef struct dAS_EXPR_struct {
 OB_HEADER header;
 } *dAS_EXPR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct AS_OUT_struct {/* layout for AS_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AS_OUT;

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

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

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

typedef struct AS_ARRAY_EXPR_struct {/* layout for AS_ARRAY_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *elts;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_ARRAY_EXPR;

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

typedef struct AS_ATTR_DEF_struct {/* layout for AS_ATTR_DEF */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 BOOL is_readonly;
 } *AS_ATTR_DEF;

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

typedef struct AS_BREAK_EXPR_struct {/* layout for AS_BREAK_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_BREAK_EXPR;

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

typedef struct AS_DEC_STMT_struct {/* layout for AS_DEC_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct AS_TYPE_SPEC_struct *tp;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_DEC_STMT;

typedef struct AS_EXCEPT_EXPR_struct {/* layout for AS_EXCEPT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_EXCEPT_EXPR;

typedef struct AS_EXPR_STMT_struct {/* layout for AS_EXPR_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_EXPR_STMT;

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

typedef struct AS_INITIAL_EXPR_struct {/* layout for AS_INITIAL_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_INITIAL_EXPR;

typedef struct AS_INT2031239268_struct {/* layout for AS_INTERF_ATTACH_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *at;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct IDENT_struct gatename;
 struct IDENT_struct helpername;
 struct IDENT_struct routname;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_INT2031239268;

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

typedef struct AS_LOCK_IF_WHEN_struct {/* layout for AS_LOCK_IF_WHEN */
 OB_HEADER header;
 struct dAS_EXPR_struct *e_list;
 struct dAS_EXPR_struct *val1;
 struct AS_LOCK_IF_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct SFILE_ID_struct source1;
 } *AS_LOCK_IF_WHEN;

typedef struct AS_LOOP_STMT_struct {/* layout for AS_LOOP_STMT */
 OB_HEADER header;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *body;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_LOOP_STMT;

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

typedef struct AS_PROTECT_WHEN_struct {/* layout for AS_PROTECT_WHEN */
 OB_HEADER header;
 struct AS_PROTECT_WHEN_struct *next;
 struct AS_STMT_LIST_struct *then_part;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 } *AS_PROTECT_WHEN;

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

typedef struct AS_RESULT_EXPR_struct {/* layout for AS_RESULT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_RESULT_EXPR;

typedef struct AS_RETURN_STMT_struct {/* layout for AS_RETURN_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_RETURN_STMT;

typedef struct AS_SELF_EXPR_struct {/* layout for AS_SELF_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 } *AS_SELF_EXPR;

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

typedef struct AS_STR_LIT_EXPR_struct {/* layout for AS_STR_LIT_EXPR */
 OB_HEADER header;
 struct dAS_EXPR_struct *next;
 struct SFILE_ID_struct source1;
 STR s;
 } *AS_STR_LIT_EXPR;

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

typedef struct AS_YIELD_STMT_struct {/* layout for AS_YIELD_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *val1;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_YIELD_STMT;

#include "tags.h"

/* Globals */

extern INT AS_OUT_indent;

/* Function declarations */


extern RETURNED_CONST dAS_EXPR (**dAS_EX337431651)(dAS_EXPR);
BOOL STR_is111530248(STR, STR);
INT STR_lengthrINT(STR);
INT STR_se1725540125(STR, CHAR);
INT STR_sizerINT(STR);
STR AS_OUT422425760(AS_OUT, STR);
STR CHAR_prettyrSTR(CHAR);
STR RAT_str_INTrSTR(RAT, INT);
STR STR_ap2004550586(STR, STR);
STR STR_head_INTrSTR(STR, INT);
STR STR_prettyrSTR(STR);
STR STR_tail_INTrSTR(STR, INT);
STR STR_upperrSTR(STR);
void AS_OUT1019461868(AS_OUT, AS_VOID_EXPR);
void AS_OUT1024144924(AS_OUT, AS_ARG_MODE, dAS_EXPR);
void AS_OUT1034803282(AS_OUT, AS_GLOBAL_EXPR);
void AS_OUT1070397350(AS_OUT, AS_RESULT_EXPR);
void AS_OUT1082396938(AS_OUT, AS_ARG_MODE);
void AS_OUT1170292156(AS_OUT, AS_NEAR_EXPR);
void AS_OUT1189782454(AS_OUT, AS_RETURN_STMT);
void AS_OUT1198591538(AS_OUT, AS_ARG_DEC);
void AS_OUT1211286518(AS_OUT, AS_CREATE_EXPR);
void AS_OUT1240003398(AS_OUT, AS_IF_STMT);
void AS_OUT1255290752(AS_OUT, AS_BOOL_LIT_EXPR);
void AS_OUT1317811396(AS_OUT, AS_QUIT_STMT);
void AS_OUT1331819376(AS_OUT, AS_LOOP_STMT);
void AS_OUT1347508463(AS_OUT, AS_INITIAL_EXPR);
void AS_OUT1372724668(AS_OUT, AS_TYPECASE_STMT);
void AS_OUT1434191198(AS_OUT, AS_OR_EXPR);
void AS_OUT1499224135(AS_OUT, AS_PAR_STMT);
void AS_OUT1538624491(AS_OUT, AS_RAISE_STMT);
void AS_OUT1577197339(AS_OUT, AS_PROTECT_WHEN);
void AS_OUT160973005(AS_OUT, AS_INT_LIT_EXPR);
void AS_OUT1693873817(AS_OUT, AS_FAR_EXPR);
void AS_OUT1790419101(AS_OUT, AS_CLUSTER_EXPR);
void AS_OUT1804680247(AS_OUT, AS_INT2031239268);
void AS_OUT1954367915(AS_OUT, AS_ANY_EXPR);
void AS_OUT1983800340(AS_OUT, AS_CALL_EXPR);
void AS_OUT2042091545(AS_OUT, AS_DEC_STMT);
void AS_OUT2138884099(AS_OUT, AS_UND152986614);
void AS_OUT2139428914(AS_OUT, dAS_CLASS_ELT);
void AS_OUT26552353(AS_OUT, AS_BREAK_EXPR);
void AS_OUT359892079(AS_OUT, AS_AND_EXPR);
void AS_OUT387909983(AS_OUT, AS_STR_LIT_EXPR);
void AS_OUT422363112(AS_OUT, AS_CASE_WHEN);
void AS_OUT423700572(AS_OUT, AS_TYPECASE_WHEN);
void AS_OUT454374187(AS_OUT, AS_FLT_LIT_EXPR);
void AS_OUT460718931(AS_OUT, AS_SHARED_DEF);
void AS_OUT474809072(AS_OUT, AS_CASE_STMT);
void AS_OUT48907654(AS_OUT, AS_ASSIGN_STMT);
void AS_OUT516254387(AS_OUT, AS_ARRAY_EXPR);
void AS_OUT57117984(AS_OUT, AS_CHAR_LIT_EXPR);
void AS_OUT599192453(AS_OUT, AS_NEW_EXPR);
void AS_OUT600758956(AS_OUT, AS_TYPE_SPEC);
void AS_OUT629983961(AS_OUT, AS_WHERE_EXPR);
void AS_OUT662922118(AS_OUT, AS_EXCEPT_EXPR);
void AS_OUT749907512(AS_OUT, AS_STMT_LIST);
void AS_OUT767507057(AS_OUT, AS_IS_VOID_EXPR);
void AS_OUT774387720(AS_OUT, AS_CONST_DEF);
void AS_OUT778273489(AS_OUT, AS_PROTECT_STMT);
void AS_OUT801616852(AS_OUT, AS_CLU553570599);
void AS_OUT804297708(AS_OUT, AS_SELF_EXPR);
void AS_OUT826513318(AS_OUT, AS_COHORT_EXPR);
void AS_OUT851761245(AS_OUT, dAS_EXPR);
void AS_OUT895691444(AS_OUT, AS_HERE_EXPR);
void AS_OUT898752312(AS_OUT, AS_BOU14269336);
void AS_OUT90627778(AS_OUT, AS_AT_EXPR);
void AS_OUT916258175(AS_OUT, AS_LOCK_IF_WHEN);
void AS_OUT918185922(AS_OUT, AS_ASSERT_STMT);
void AS_OUT962799085(AS_OUT, AS_YIELD_STMT);
void AS_OUT964805355(AS_OUT, AS_ATTR_DEF);
void AS_OUT966193580(AS_OUT, AS_PARAM_DEC);
void AS_OUT995003644(AS_OUT, AS_EXPR_STMT);
void AS_OUT_ind(AS_OUT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void INT_timesb(INT_timesb_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

STR AS_OUT422425760(AS_OUT self, STR s) {
 STR ret_val;
 STR L1;
 extern STR S_;
 INT L2;
 INT L31_;
 if ((s==((STR) NULL))) {
  ret_val = ((STR) NULL);
  return ret_val;
 }
 else {
  L1 = STR_head_INTrSTR(s, 1);
  if (STR_is111530248(L1, ((STR) &S_))) {
   L2 = STR_sizerINT(s);
   L31_=INTMINUS(L2,1); 
   ret_val = STR_tail_INTrSTR(s, L31_);
   return ret_val;
  }
  else {
   ret_val = s;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1019461868(AS_OUT self, AS_VOID_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR void1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_VOID_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &void1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1024144924(AS_OUT self, AS_ARG_MODE m, dAS_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR dAS_EXPRvoid;
 FILE1 L1;
 OB L2;
 dAS_EXPR L4;
 BOOL L5;
 BOOL L61_;
 extern STR name29;
 FILE1 L7;
 OB L8;
 AS_ARG_MODE L10;
 dAS_EXPR L11;
 if ((t==((dAS_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &dAS_EXPRvoid);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT1082396938(self, m);
 switch (TAG(t)) {
  case AS_SELF_EXPR_tag:
   AS_OUT804297708(self, ((AS_SELF_EXPR) t)); break;
  case AS_CALL_EXPR_tag:
   AS_OUT1983800340(self, ((AS_CALL_EXPR) t)); break;
  case AS_VOID_EXPR_tag:
   AS_OUT1019461868(self, ((AS_VOID_EXPR) t)); break;
  case AS_IS_VOID_EXPR_tag:
   AS_OUT767507057(self, ((AS_IS_VOID_EXPR) t)); break;
  case AS_ARRAY_EXPR_tag:
   AS_OUT516254387(self, ((AS_ARRAY_EXPR) t)); break;
  case AS_CREATE_EXPR_tag:
   AS_OUT1211286518(self, ((AS_CREATE_EXPR) t)); break;
  case AS_BOU14269336_tag:
   AS_OUT898752312(self, ((AS_BOU14269336) t)); break;
  case AS_UND152986614_tag:
   AS_OUT2138884099(self, ((AS_UND152986614) t)); break;
  case AS_AND_EXPR_tag:
   AS_OUT359892079(self, ((AS_AND_EXPR) t)); break;
  case AS_OR_EXPR_tag:
   AS_OUT1434191198(self, ((AS_OR_EXPR) t)); break;
  case AS_EXCEPT_EXPR_tag:
   AS_OUT662922118(self, ((AS_EXCEPT_EXPR) t)); break;
  case AS_NEW_EXPR_tag:
   AS_OUT599192453(self, ((AS_NEW_EXPR) t)); break;
  case AS_INITIAL_EXPR_tag:
   AS_OUT1347508463(self, ((AS_INITIAL_EXPR) t)); break;
  case AS_BREAK_EXPR_tag:
   AS_OUT26552353(self, ((AS_BREAK_EXPR) t)); break;
  case AS_RESULT_EXPR_tag:
   AS_OUT1070397350(self, ((AS_RESULT_EXPR) t)); break;
  case AS_BOOL_LIT_EXPR_tag:
   AS_OUT1255290752(self, ((AS_BOOL_LIT_EXPR) t)); break;
  case AS_CHAR_LIT_EXPR_tag:
   AS_OUT57117984(self, ((AS_CHAR_LIT_EXPR) t)); break;
  case AS_STR_LIT_EXPR_tag:
   AS_OUT387909983(self, ((AS_STR_LIT_EXPR) t)); break;
  case AS_INT_LIT_EXPR_tag:
   AS_OUT160973005(self, ((AS_INT_LIT_EXPR) t)); break;
  case AS_FLT_LIT_EXPR_tag:
   AS_OUT454374187(self, ((AS_FLT_LIT_EXPR) t)); break;
  case AS_HERE_EXPR_tag:
   AS_OUT895691444(self, ((AS_HERE_EXPR) t)); break;
  case AS_ANY_EXPR_tag:
   AS_OUT1954367915(self, ((AS_ANY_EXPR) t)); break;
  case AS_WHERE_EXPR_tag:
   AS_OUT629983961(self, ((AS_WHERE_EXPR) t)); break;
  case AS_NEAR_EXPR_tag:
   AS_OUT1170292156(self, ((AS_NEAR_EXPR) t)); break;
  case AS_FAR_EXPR_tag:
   AS_OUT1693873817(self, ((AS_FAR_EXPR) t)); break;
  case AS_AT_EXPR_tag:
   AS_OUT90627778(self, ((AS_AT_EXPR) t)); break;
  case AS_CLUSTER_EXPR_tag:
   AS_OUT1790419101(self, ((AS_CLUSTER_EXPR) t)); break;
  case AS_CLU553570599_tag:
   AS_OUT801616852(self, ((AS_CLU553570599) t)); break;
  case AS_COHORT_EXPR_tag:
   AS_OUT826513318(self, ((AS_COHORT_EXPR) t)); break;
  case AS_GLOBAL_EXPR_tag:
   AS_OUT1034803282(self, ((AS_GLOBAL_EXPR) t)); break;
  default: ;
   FATAL("No applicable type in typecase\nin AS_OUT::AS_MODE_EXPR_out(AS_ARG_MODE,$AS_EXPR)\n./Abstract_Syntax/as_out.sa:195:14");
 }
 L4 = t;
 L5 = ((*dAS_EX337431651[TAG(L4)])(L4)==((dAS_EXPR) NULL));
 L61_=!(L5); 
 if (L61_) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &name29);
  stdout_self1 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r1 = L7;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  L10 = ATTR(m,next);
  L11 = t;
  AS_OUT1024144924(self, L10, (*dAS_EX337431651[TAG(L11)])(L11));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1034803282(AS_OUT self, AS_GLOBAL_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR global1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_GLOBAL_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &global1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1070397350(AS_OUT self, AS_RESULT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR result1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_RESULT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &result1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1082396938(AS_OUT self, AS_ARG_MODE m) {
 INT L11 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR out3;
 FILE1 L2;
 OB L3;
 extern STR inout2;
 FILE1 L5;
 OB L6;
 extern STR once2;
 FILE1 L8;
 OB L9;
 extern STR in4;
 FILE1 L12;
 OB L13;
 L11 = ATTR(m,mod);
 switch (L11) {
  case 2: 
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &out3);
   stdout_self = ((FILE1) NULL);
   L3=ZALLOC(sizeof(struct FILE1_struct));
   if (L3==NULL) FATAL("Unable to allocate more memory");
   ((OB)L3)->header.tag=FILE1_tag;
   L2 = ((FILE1) L3);
   r = L2;
   
   SATTR(r,fp,stdout);
   ret_val1 = r;
   FILE_plus_STR(ret_val1, plus_s);
   break;
  case 3: 
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self1 = ret_val2;
   plus_s1 = ((STR) &inout2);
   stdout_self1 = ((FILE1) NULL);
   L6=ZALLOC(sizeof(struct FILE1_struct));
   if (L6==NULL) FATAL("Unable to allocate more memory");
   ((OB)L6)->header.tag=FILE1_tag;
   L5 = ((FILE1) L6);
   r1 = L5;
   
   SATTR(r1,fp,stdout);
   ret_val3 = r1;
   FILE_plus_STR(ret_val3, plus_s1);
   break;
  case 4: 
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   plus_s2 = ((STR) &once2);
   stdout_self2 = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r2 = L8;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
   break;
  case 1: 
   create_self3 = ((OUT) NULL);
   ret_val6 = create_self3;
   plus_self3 = ret_val6;
   plus_s3 = ((STR) &in4);
   stdout_self3 = ((FILE1) NULL);
   L13=ZALLOC(sizeof(struct FILE1_struct));
   if (L13==NULL) FATAL("Unable to allocate more memory");
   ((OB)L13)->header.tag=FILE1_tag;
   L12 = ((FILE1) L13);
   r3 = L12;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   FILE_plus_STR(ret_val7, plus_s3);
   break;
  default: ;
   FATAL("No applicable target in case statement\nin AS_OUT::AS_MODE_out(AS_ARG_MODE)\n./Abstract_Syntax/as_out.sa:1089:21");
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1170292156(AS_OUT self, AS_NEAR_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR near4;
 FILE1 L4;
 OB L5;
 extern STR name26;
 FILE1 L7;
 OB L8;
 if ((t==((AS_NEAR_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &near4);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name26);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1189782454(AS_OUT self, AS_RETURN_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR return4;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name13;
 FILE1 L9;
 OB L10;
 if ((t==((AS_RETURN_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &return4);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,val1)==((dAS_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  AS_OUT851761245(self, ATTR(t,val1));
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1198591538(AS_OUT self, AS_ARG_DEC t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
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
 OUT create_self2;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self3;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name21;
 FILE1 L7;
 OB L8;
 extern STR b;
 FILE1 L10;
 OB L11;
 BOOL L13;
 BOOL L141_;
 extern STR name29;
 FILE1 L15;
 OB L16;
 if ((t==((AS_ARG_DEC) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT1082396938(self, ATTR(t,mode1));
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
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
 plus_s2 = ((STR) &name21);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 AS_OUT600758956(self, ATTR(t,tp));
 if (ATTR(t,is_hot)) {
  create_self2 = ((OUT) NULL);
  ret_val6 = create_self2;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &b);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 L13 = (ATTR(t,next)==((AS_ARG_DEC) NULL));
 L141_=!(L13); 
 if (L141_) {
  create_self3 = ((OUT) NULL);
  ret_val8 = create_self3;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name29);
  stdout_self4 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r4 = L15;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
  AS_OUT1198591538(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1211286518(AS_OUT self, AS_CREATE_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name1111;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 extern STR name24;
 FILE1 L11;
 OB L12;
 extern STR name26;
 FILE1 L14;
 OB L15;
 if ((t==((AS_CREATE_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name1111);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L81_=!(L7); 
 if (L81_) {
  AS_OUT600758956(self, ATTR(t,tp));
 }
 L9 = (ATTR(t,elts)==((dAS_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name24);
  stdout_self2 = ((FILE1) NULL);
  L12=ZALLOC(sizeof(struct FILE1_struct));
  if (L12==NULL) FATAL("Unable to allocate more memory");
  ((OB)L12)->header.tag=FILE1_tag;
  L11 = ((FILE1) L12);
  r2 = L11;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT1024144924(self, ATTR(t,modes1), ATTR(t,elts));
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name26);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1240003398(AS_OUT self, AS_IF_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 AS_OUT iinc_self1;
 AS_OUT idec_self1;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR if5;
 FILE1 L4;
 OB L5;
 extern STR then3;
 FILE1 L7;
 OB L8;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 extern STR else8;
 FILE1 L16;
 OB L17;
 INT L19;
 INT L201_;
 INT L21;
 INT L221_;
 extern STR endofif;
 FILE1 L23;
 OB L24;
 if ((t==((AS_IF_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &if5);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,test1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &then3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 iinc_self = self;
 L10 = AS_OUT_indent;
 L111_=INTPLUS(L10,3); 
 AS_OUT_indent = L111_;
 AS_OUT749907512(self, ATTR(t,then_part));
 idec_self = self;
 L12 = AS_OUT_indent;
 L131_=INTMINUS(L12,3); 
 AS_OUT_indent = L131_;
 L14 = (ATTR(t,else_part)==((AS_STMT_LIST) NULL));
 L151_=!(L14); 
 if (L151_) {
  AS_OUT_ind(self);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &else8);
  stdout_self3 = ((FILE1) NULL);
  L17=ZALLOC(sizeof(struct FILE1_struct));
  if (L17==NULL) FATAL("Unable to allocate more memory");
  ((OB)L17)->header.tag=FILE1_tag;
  L16 = ((FILE1) L17);
  r3 = L16;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  iinc_self1 = self;
  L19 = AS_OUT_indent;
  L201_=INTPLUS(L19,3); 
  AS_OUT_indent = L201_;
  AS_OUT749907512(self, ATTR(t,else_part));
  idec_self1 = self;
  L21 = AS_OUT_indent;
  L221_=INTMINUS(L21,3); 
  AS_OUT_indent = L221_;
 }
 AS_OUT_ind(self);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &endofif);
 stdout_self4 = ((FILE1) NULL);
 L24=ZALLOC(sizeof(struct FILE1_struct));
 if (L24==NULL) FATAL("Unable to allocate more memory");
 ((OB)L24)->header.tag=FILE1_tag;
 L23 = ((FILE1) L24);
 r4 = L23;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1255290752(AS_OUT self, AS_BOOL_LIT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 dSTR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 dSTR L4;
 OB L5;
 FILE1 L6;
 OB L7;
 if ((t==((AS_BOOL_LIT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 L5=ZALLOC_LEAF(sizeof(struct BOOL_boxed_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 memset(L5,0,sizeof(struct BOOL_boxed_struct));
 ((OB)L5)->header.tag=BOOL_tag;
 L4 = (dSTR)((BOOL_boxed) L5);
 ((BOOL_boxed) L4)->immutable_part = ATTR(t,val1);
 plus_s1 = L4;
 stdout_self1 = ((FILE1) NULL);
 L7=ZALLOC(sizeof(struct FILE1_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=FILE1_tag;
 L6 = ((FILE1) L7);
 r1 = L6;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1317811396(AS_OUT self, AS_QUIT_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR quit3;
 FILE1 L4;
 OB L5;
 if ((t==((AS_QUIT_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &quit3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1331819376(AS_OUT self, AS_LOOP_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR loop2;
 FILE1 L4;
 OB L5;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 extern STR endofloop;
 FILE1 L11;
 OB L12;
 if ((t==((AS_LOOP_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &loop2);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 iinc_self = self;
 L7 = AS_OUT_indent;
 L81_=INTPLUS(L7,3); 
 AS_OUT_indent = L81_;
 AS_OUT749907512(self, ATTR(t,body));
 idec_self = self;
 L9 = AS_OUT_indent;
 L101_=INTMINUS(L9,3); 
 AS_OUT_indent = L101_;
 AS_OUT_ind(self);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &endofloop);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r2 = L11;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1347508463(AS_OUT self, AS_INITIAL_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR initial3;
 FILE1 L4;
 OB L5;
 extern STR name26;
 FILE1 L7;
 OB L8;
 if ((t==((AS_INITIAL_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &initial3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name26);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1372724668(AS_OUT self, AS_TYPECASE_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR typecase2;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name9;
 FILE1 L10;
 OB L11;
 BOOL L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 extern STR else8;
 FILE1 L17;
 OB L18;
 INT L20;
 INT L211_;
 INT L22;
 INT L231_;
 extern STR endoftypecase;
 FILE1 L24;
 OB L25;
 if ((t==((AS_TYPECASE_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &typecase2);
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
 plus_s2 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 plus_s3 = ((STR) &name9);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L13 = (ATTR(t,when_part)==((AS_TYPECASE_WHEN) NULL));
 L141_=!(L13); 
 if (L141_) {
  AS_OUT423700572(self, ATTR(t,when_part));
 }
 L15 = ATTR(t,no_else);
 L161_=!(L15); 
 if (L161_) {
  AS_OUT_ind(self);
  create_self2 = ((OUT) NULL);
  ret_val8 = create_self2;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &else8);
  stdout_self4 = ((FILE1) NULL);
  L18=ZALLOC(sizeof(struct FILE1_struct));
  if (L18==NULL) FATAL("Unable to allocate more memory");
  ((OB)L18)->header.tag=FILE1_tag;
  L17 = ((FILE1) L18);
  r4 = L17;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
  iinc_self = self;
  L20 = AS_OUT_indent;
  L211_=INTPLUS(L20,3); 
  AS_OUT_indent = L211_;
  AS_OUT749907512(self, ATTR(t,else_part));
  idec_self = self;
  L22 = AS_OUT_indent;
  L231_=INTMINUS(L22,3); 
  AS_OUT_indent = L231_;
 }
 AS_OUT_ind(self);
 create_self3 = ((OUT) NULL);
 ret_val10 = create_self3;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &endoftypecase);
 stdout_self5 = ((FILE1) NULL);
 L25=ZALLOC(sizeof(struct FILE1_struct));
 if (L25==NULL) FATAL("Unable to allocate more memory");
 ((OB)L25)->header.tag=FILE1_tag;
 L24 = ((FILE1) L25);
 r5 = L24;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1434191198(AS_OUT self, AS_OR_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name24;
 FILE1 L4;
 OB L5;
 extern STR or3;
 FILE1 L7;
 OB L8;
 extern STR name26;
 FILE1 L10;
 OB L11;
 if ((t==((AS_OR_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name24);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &or3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 AS_OUT851761245(self, ATTR(t,e2));
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name26);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1499224135(AS_OUT self, AS_PAR_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR par3;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L121_;
 extern STR endofpar;
 FILE1 L13;
 OB L14;
 if ((t==((AS_PAR_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &par3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,body)==((AS_STMT_LIST) NULL));
 L81_=!(L7); 
 if (L81_) {
  iinc_self = self;
  L9 = AS_OUT_indent;
  L101_=INTPLUS(L9,3); 
  AS_OUT_indent = L101_;
  AS_OUT749907512(self, ATTR(t,body));
  idec_self = self;
  L11 = AS_OUT_indent;
  L121_=INTMINUS(L11,3); 
  AS_OUT_indent = L121_;
  AS_OUT_ind(self);
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &endofpar);
  stdout_self2 = ((FILE1) NULL);
  L14=ZALLOC(sizeof(struct FILE1_struct));
  if (L14==NULL) FATAL("Unable to allocate more memory");
  ((OB)L14)->header.tag=FILE1_tag;
  L13 = ((FILE1) L14);
  r2 = L13;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1538624491(AS_OUT self, AS_RAISE_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR raise3;
 FILE1 L4;
 OB L5;
 extern STR name13;
 FILE1 L7;
 OB L8;
 if ((t==((AS_RAISE_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &raise3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,val1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1577197339(AS_OUT self, AS_PROTECT_WHEN t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR when3;
 FILE1 L4;
 OB L5;
 extern STR then3;
 FILE1 L7;
 OB L8;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 if ((t==((AS_PROTECT_WHEN) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &when3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT600758956(self, ATTR(t,tp));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &then3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 iinc_self = self;
 L10 = AS_OUT_indent;
 L111_=INTPLUS(L10,3); 
 AS_OUT_indent = L111_;
 AS_OUT749907512(self, ATTR(t,then_part));
 idec_self = self;
 L12 = AS_OUT_indent;
 L131_=INTMINUS(L12,3); 
 AS_OUT_indent = L131_;
 L14 = (ATTR(t,next)==((AS_PROTECT_WHEN) NULL));
 L151_=!(L14); 
 if (L151_) {
  AS_OUT1577197339(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT160973005(AS_OUT self, AS_INT_LIT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 dSTR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR i21;
 FILE1 L7;
 OB L8;
 if ((t==((AS_INT_LIT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((dSTR) ATTR(t,val1));
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_dSTR(ret_val3, plus_s1);
 if (ATTR(t,is_inti)) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &i21);
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
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1693873817(AS_OUT self, AS_FAR_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR far4;
 FILE1 L4;
 OB L5;
 extern STR name26;
 FILE1 L7;
 OB L8;
 if ((t==((AS_FAR_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &far4);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name26);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1790419101(AS_OUT self, AS_CLUSTER_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR clusters1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_CLUSTER_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &clusters1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1804680247(AS_OUT self, AS_INT2031239268 t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 OUT ret_val3;
 FILE1 stdout_self1;
 FILE1 ret_val4;
 FILE1 r1;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self2;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self3;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val13;
 FILE1 stdout_self6;
 FILE1 ret_val14;
 FILE1 r6;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self4;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self5;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR THREAD;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 extern STR name29;
 FILE1 L10;
 OB L11;
 FILE1 L13;
 OB L14;
 extern STR name29;
 FILE1 L16;
 OB L17;
 FILE1 L19;
 OB L20;
 extern STR name29;
 FILE1 L22;
 OB L23;
 extern STR void1;
 FILE1 L25;
 OB L26;
 extern STR conc;
 FILE1 L28;
 OB L29;
 if ((t==((AS_INT2031239268) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &THREAD);
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
 plus_s2 = AS_OUT422425760(self, ATTR(t,routname).str);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 plus_s3 = ((STR) &name29);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 create_self2 = ((OUT) NULL);
 ret_val8 = create_self2;
 plus_self4 = ret_val8;
 plus_s4 = AS_OUT422425760(self, ATTR(t,helpername).str);
 stdout_self4 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r4 = L13;
 
 SATTR(r4,fp,stdout);
 ret_val10 = r4;
 FILE_plus_STR(ret_val10, plus_s4);
 ret_val9 = plus_self4;
 plus_self5 = ret_val9;
 plus_s5 = ((STR) &name29);
 stdout_self5 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r5 = L16;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
 create_self3 = ((OUT) NULL);
 ret_val12 = create_self3;
 plus_self6 = ret_val12;
 plus_s6 = AS_OUT422425760(self, ATTR(t,gatename).str);
 stdout_self6 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r6 = L19;
 
 SATTR(r6,fp,stdout);
 ret_val14 = r6;
 FILE_plus_STR(ret_val14, plus_s6);
 ret_val13 = plus_self6;
 plus_self7 = ret_val13;
 plus_s7 = ((STR) &name29);
 stdout_self7 = ((FILE1) NULL);
 L23=ZALLOC(sizeof(struct FILE1_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=FILE1_tag;
 L22 = ((FILE1) L23);
 r7 = L22;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
 if ((ATTR(t,at)==((dAS_EXPR) NULL))) {
  create_self4 = ((OUT) NULL);
  ret_val16 = create_self4;
  plus_self8 = ret_val16;
  plus_s8 = ((STR) &void1);
  stdout_self8 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r8 = L25;
  
  SATTR(r8,fp,stdout);
  ret_val17 = r8;
  FILE_plus_STR(ret_val17, plus_s8);
 }
 else {
  AS_OUT851761245(self, ATTR(t,at));
 }
 create_self5 = ((OUT) NULL);
 ret_val18 = create_self5;
 plus_self9 = ret_val18;
 plus_s9 = ((STR) &conc);
 stdout_self9 = ((FILE1) NULL);
 L29=ZALLOC(sizeof(struct FILE1_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=FILE1_tag;
 L28 = ((FILE1) L29);
 r9 = L28;
 
 SATTR(r9,fp,stdout);
 ret_val19 = r9;
 FILE_plus_STR(ret_val19, plus_s9);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1954367915(AS_OUT self, AS_ANY_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR any1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_ANY_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &any1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT1983800340(AS_OUT self, AS_CALL_EXPR t) {
 dAS_EXPR e;
 BOOL close_it = BOOL_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self7;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self8;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self9;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 extern STR void1;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 extern STR name24;
 FILE1 L6;
 OB L7;
 extern STR name26;
 FILE1 L9;
 OB L10;
 BOOL L12;
 BOOL L131_;
 extern STR name18;
 FILE1 L14;
 OB L15;
 BOOL L17;
 BOOL L18;
 BOOL L191_;
 BOOL L20;
 extern STR SYS1;
 BOOL L21;
 BOOL L221_;
 BOOL L23;
 BOOL L24;
 BOOL L251_;
 extern STR clusterb;
 BOOL L26;
 BOOL L271_;
 extern STR name34;
 FILE1 L28;
 OB L29;
 BOOL L32;
 BOOL L331_;
 FILE1 L34;
 OB L35;
 BOOL L37;
 BOOL L381_;
 extern STR name14;
 FILE1 L39;
 OB L40;
 extern STR name7;
 FILE1 L42;
 OB L43;
 extern STR name24;
 FILE1 L45;
 OB L46;
 extern STR name26;
 FILE1 L48;
 OB L49;
 if ((t==((AS_CALL_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 L4 = (ATTR(t,ob)==((dAS_EXPR) NULL));
 L51_=!(L4); 
 if (L51_) {
  e = ATTR(t,ob);
  close_it = FALSE;
  if (e==NULL) {
   goto other463;
  } else
  switch (TAG(e)) {
   case AS_VOID_EXPR_tag: break;
   case AS_CREATE_EXPR_tag: break;
   case AS_ARRAY_EXPR_tag: break;
   case AS_UND152986614_tag: break;
   default: ;
   other463: ;
    create_self1 = ((OUT) NULL);
    ret_val2 = create_self1;
    plus_self1 = ret_val2;
    plus_s1 = ((STR) &name24);
    stdout_self1 = ((FILE1) NULL);
    L7=ZALLOC(sizeof(struct FILE1_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=FILE1_tag;
    L6 = ((FILE1) L7);
    r1 = L6;
    
    SATTR(r1,fp,stdout);
    ret_val3 = r1;
    FILE_plus_STR(ret_val3, plus_s1);
    close_it = TRUE;
  }
  AS_OUT851761245(self, ATTR(t,ob));
  if (close_it) {
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   plus_s2 = ((STR) &name26);
   stdout_self2 = ((FILE1) NULL);
   L10=ZALLOC(sizeof(struct FILE1_struct));
   if (L10==NULL) FATAL("Unable to allocate more memory");
   ((OB)L10)->header.tag=FILE1_tag;
   L9 = ((FILE1) L10);
   r2 = L9;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
  }
  L12 = ATTR(t,is_array);
  L131_=!(L12); 
  if (L131_) {
   create_self3 = ((OUT) NULL);
   ret_val6 = create_self3;
   plus_self3 = ret_val6;
   plus_s3 = ((STR) &name18);
   stdout_self3 = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r3 = L14;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   FILE_plus_STR(ret_val7, plus_s3);
  }
 }
 L18 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L191_=!(L18); 
 if (L191_) {
  L21 = STR_is111530248(ATTR(ATTR(t,tp),name1).str, ((STR) &SYS1));
  L221_=!(L21); 
  if (L221_) {
   L20 = TRUE;
  } else {
   L24 = (ATTR(t,name1).str==(STR)0);
   L251_=!(L24); 
   if (L251_) {
    L26 = STR_is111530248(ATTR(t,name1).str, ((STR) &clusterb));
    L271_=!(L26); 
    L23 = L271_;
   } else {
    L23 = FALSE;
   }
   L20 = L23;
  }
  L17 = L20;
 } else {
  L17 = FALSE;
 }
 if (L17) {
  AS_OUT600758956(self, ATTR(t,tp));
  create_self4 = ((OUT) NULL);
  ret_val8 = create_self4;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name34);
  stdout_self4 = ((FILE1) NULL);
  L29=ZALLOC(sizeof(struct FILE1_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=FILE1_tag;
  L28 = ((FILE1) L29);
  r4 = L28;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
 }
 L32 = (ATTR(t,name1).str==(STR)0);
 L331_=!(L32); 
 if (L331_) {
  create_self5 = ((OUT) NULL);
  ret_val10 = create_self5;
  plus_self5 = ret_val10;
  plus_s5 = AS_OUT422425760(self, ATTR(t,name1).str);
  stdout_self5 = ((FILE1) NULL);
  L35=ZALLOC(sizeof(struct FILE1_struct));
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=FILE1_tag;
  L34 = ((FILE1) L35);
  r5 = L34;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
 }
 L37 = (ATTR(t,args)==((dAS_EXPR) NULL));
 L381_=!(L37); 
 if (L381_) {
  if (ATTR(t,is_array)) {
   create_self6 = ((OUT) NULL);
   ret_val12 = create_self6;
   plus_self6 = ret_val12;
   plus_s6 = ((STR) &name14);
   stdout_self6 = ((FILE1) NULL);
   L40=ZALLOC(sizeof(struct FILE1_struct));
   if (L40==NULL) FATAL("Unable to allocate more memory");
   ((OB)L40)->header.tag=FILE1_tag;
   L39 = ((FILE1) L40);
   r6 = L39;
   
   SATTR(r6,fp,stdout);
   ret_val13 = r6;
   FILE_plus_STR(ret_val13, plus_s6);
   AS_OUT851761245(self, ATTR(t,args));
   create_self7 = ((OUT) NULL);
   ret_val14 = create_self7;
   plus_self7 = ret_val14;
   plus_s7 = ((STR) &name7);
   stdout_self7 = ((FILE1) NULL);
   L43=ZALLOC(sizeof(struct FILE1_struct));
   if (L43==NULL) FATAL("Unable to allocate more memory");
   ((OB)L43)->header.tag=FILE1_tag;
   L42 = ((FILE1) L43);
   r7 = L42;
   
   SATTR(r7,fp,stdout);
   ret_val15 = r7;
   FILE_plus_STR(ret_val15, plus_s7);
  }
  else {
   create_self8 = ((OUT) NULL);
   ret_val16 = create_self8;
   plus_self8 = ret_val16;
   plus_s8 = ((STR) &name24);
   stdout_self8 = ((FILE1) NULL);
   L46=ZALLOC(sizeof(struct FILE1_struct));
   if (L46==NULL) FATAL("Unable to allocate more memory");
   ((OB)L46)->header.tag=FILE1_tag;
   L45 = ((FILE1) L46);
   r8 = L45;
   
   SATTR(r8,fp,stdout);
   ret_val17 = r8;
   FILE_plus_STR(ret_val17, plus_s8);
   AS_OUT1024144924(self, ATTR(t,modes1), ATTR(t,args));
   create_self9 = ((OUT) NULL);
   ret_val18 = create_self9;
   plus_self9 = ret_val18;
   plus_s9 = ((STR) &name26);
   stdout_self9 = ((FILE1) NULL);
   L49=ZALLOC(sizeof(struct FILE1_struct));
   if (L49==NULL) FATAL("Unable to allocate more memory");
   ((OB)L49)->header.tag=FILE1_tag;
   L48 = ((FILE1) L49);
   r9 = L48;
   
   SATTR(r9,fp,stdout);
   ret_val19 = r9;
   FILE_plus_STR(ret_val19, plus_s9);
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT2042091545(AS_OUT self, AS_DEC_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
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
 OUT create_self2;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 extern STR name20;
 FILE1 L7;
 OB L8;
 extern STR name13;
 FILE1 L10;
 OB L11;
 if ((t==((AS_DEC_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
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
 plus_s2 = ((STR) &name20);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 AS_OUT600758956(self, ATTR(t,tp));
 create_self2 = ((OUT) NULL);
 ret_val6 = create_self2;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name13);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT2138884099(AS_OUT self, AS_UND152986614 t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 extern STR S_;
 FILE1 L6;
 OB L7;
 BOOL L9;
 BOOL L101_;
 if ((t==((AS_UND152986614) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 L4 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L51_=!(L4); 
 if (L51_) {
  AS_OUT1082396938(self, ATTR(ATTR(t,tp),mode1));
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &S_);
 stdout_self1 = ((FILE1) NULL);
 L7=ZALLOC(sizeof(struct FILE1_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=FILE1_tag;
 L6 = ((FILE1) L7);
 r1 = L6;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L9 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L101_=!(L9); 
 if (L101_) {
  AS_OUT600758956(self, ATTR(t,tp));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT26552353(AS_OUT self, AS_BREAK_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR breakb1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_BREAK_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &breakb1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT359892079(AS_OUT self, AS_AND_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name24;
 FILE1 L4;
 OB L5;
 extern STR and5;
 FILE1 L7;
 OB L8;
 extern STR name26;
 FILE1 L10;
 OB L11;
 if ((t==((AS_AND_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name24);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &and5);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 AS_OUT851761245(self, ATTR(t,e2));
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name26);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT387909983(AS_OUT self, AS_STR_LIT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 if ((t==((AS_STR_LIT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = STR_prettyrSTR(ATTR(t,s));
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT422363112(AS_OUT self, AS_CASE_WHEN t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR when3;
 FILE1 L4;
 OB L5;
 extern STR then3;
 FILE1 L7;
 OB L8;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 if ((t==((AS_CASE_WHEN) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &when3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,val1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &then3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 iinc_self = self;
 L10 = AS_OUT_indent;
 L111_=INTPLUS(L10,3); 
 AS_OUT_indent = L111_;
 AS_OUT749907512(self, ATTR(t,then_part));
 idec_self = self;
 L12 = AS_OUT_indent;
 L131_=INTMINUS(L12,3); 
 AS_OUT_indent = L131_;
 L14 = (ATTR(t,next)==((AS_CASE_WHEN) NULL));
 L151_=!(L14); 
 if (L151_) {
  AS_OUT422363112(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT423700572(AS_OUT self, AS_TYPECASE_WHEN t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR when3;
 FILE1 L4;
 OB L5;
 extern STR then3;
 FILE1 L7;
 OB L8;
 INT L10;
 INT L111_;
 INT L12;
 INT L131_;
 BOOL L14;
 BOOL L151_;
 if ((t==((AS_TYPECASE_WHEN) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &when3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT600758956(self, ATTR(t,tp));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &then3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 iinc_self = self;
 L10 = AS_OUT_indent;
 L111_=INTPLUS(L10,3); 
 AS_OUT_indent = L111_;
 AS_OUT749907512(self, ATTR(t,then_part));
 idec_self = self;
 L12 = AS_OUT_indent;
 L131_=INTMINUS(L12,3); 
 AS_OUT_indent = L131_;
 L14 = (ATTR(t,next)==((AS_TYPECASE_WHEN) NULL));
 L151_=!(L14); 
 if (L151_) {
  AS_OUT423700572(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT454374187(AS_OUT self, AS_FLT_LIT_EXPR t) {
 STR s = ((STR) NULL);
 STR s2 = ((STR) NULL);
 STR s3 = ((STR) NULL);
 INT L11 = INT_zero;
 INT posi = INT_zero;
 INT L21 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 STR plus_self1;
 STR plus_sarg;
 STR ret_val2;
 STR plus_self2;
 STR plus_sarg1;
 STR ret_val3;
 STR plus_self3;
 STR plus_sarg2;
 STR ret_val4;
 STR plus_self4;
 STR plus_sarg3;
 STR ret_val5;
 STR plus_self5;
 STR plus_sarg4;
 STR ret_val6;
 STR plus_self6;
 STR plus_sarg5;
 STR ret_val7;
 STR plus_self7;
 STR plus_sarg6;
 STR ret_val8;
 OUT create_self1;
 OUT ret_val9;
 OUT plus_self8;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val10;
 FILE1 r1;
 extern STR void1;
 FILE1 L3;
 OB L4;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 extern STR S06;
 INT L9;
 INT L101_;
 extern STR S06;
 extern STR i21;
 extern STR dx;
 extern STR d41;
 extern STR x11;
 FILE1 L12;
 OB L13;
 if ((t==((AS_FLT_LIT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L4=ZALLOC(sizeof(struct FILE1_struct));
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FILE1_tag;
  L3 = ((FILE1) L4);
  r = L3;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 L11 = ATTR(t,tp);
 switch (L11) {
  case 0: 
   s = RAT_str_INTrSTR(ATTR(t,val1), 8);
   break;
  default: ;
   s = RAT_str_INTrSTR(ATTR(t,val1), 17);
 }
 posi = STR_se1725540125(s, 'e');
 L61_=(posi)==(-1); 
 L7 = L61_;
 L81_=!(L7); 
 if (L81_) {
  plus_self1 = STR_head_INTrSTR(s, posi);
  plus_sarg = ((STR) &S06);
  ret_val2 = STR_ap2004550586(plus_self1, plus_sarg);
  plus_self2 = ret_val2;
  L9 = STR_lengthrINT(s);
  L101_=INTMINUS(L9,posi); 
  plus_sarg1 = STR_tail_INTrSTR(s, L101_);
  ret_val3 = STR_ap2004550586(plus_self2, plus_sarg1);
  s2 = ret_val3;
 }
 else {
  plus_self3 = s;
  plus_sarg2 = ((STR) &S06);
  ret_val4 = STR_ap2004550586(plus_self3, plus_sarg2);
  s2 = ret_val4;
 }
 L21 = ATTR(t,tp);
 switch (L21) {
  case 4: 
   plus_self4 = s2;
   plus_sarg3 = ((STR) &i21);
   ret_val5 = STR_ap2004550586(plus_self4, plus_sarg3);
   s3 = ret_val5;
   break;
  case 3: 
   plus_self5 = s2;
   plus_sarg4 = ((STR) &dx);
   ret_val6 = STR_ap2004550586(plus_self5, plus_sarg4);
   s3 = ret_val6;
   break;
  case 1: 
   plus_self6 = s2;
   plus_sarg5 = ((STR) &d41);
   ret_val7 = STR_ap2004550586(plus_self6, plus_sarg5);
   s3 = ret_val7;
   break;
  case 2: 
   plus_self7 = s2;
   plus_sarg6 = ((STR) &x11);
   ret_val8 = STR_ap2004550586(plus_self7, plus_sarg6);
   s3 = ret_val8;
   break;
  default: ;
   s3 = s2;
 }
 create_self1 = ((OUT) NULL);
 ret_val9 = create_self1;
 plus_self8 = ret_val9;
 plus_s1 = s3;
 stdout_self1 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r1 = L12;
 
 SATTR(r1,fp,stdout);
 ret_val10 = r1;
 FILE_plus_STR(ret_val10, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT460718931(AS_OUT self, AS_SHARED_DEF t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self5;
 dSTR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self6;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 extern STR AS_SHA1798301576;
 FILE1 L1;
 OB L2;
 extern STR private3;
 FILE1 L4;
 OB L5;
 extern STR readonly2;
 FILE1 L7;
 OB L8;
 extern STR shared3;
 FILE1 L10;
 OB L11;
 FILE1 L13;
 OB L14;
 dSTR L16;
 OB L17;
 FILE1 L18;
 OB L19;
 BOOL L21;
 BOOL L221_;
 extern STR name20;
 FILE1 L23;
 OB L24;
 BOOL L26;
 BOOL L271_;
 extern STR name154;
 FILE1 L28;
 OB L29;
 extern STR name13;
 FILE1 L32;
 OB L33;
 BOOL L35;
 BOOL L361_;
 if ((t==((AS_SHARED_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_SHA1798301576);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 if (ATTR(t,is_private)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &private3);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 if (ATTR(t,is_readonly)) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &readonly2);
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
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &shared3);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self4 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r4 = L13;
 
 SATTR(r4,fp,stdout);
 ret_val10 = r4;
 FILE_plus_STR(ret_val10, plus_s4);
 ret_val9 = plus_self4;
 plus_self5 = ret_val9;
 L17=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 memset(L17,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L17)->header.tag=CHAR_tag;
 L16 = (dSTR)((CHAR_boxed) L17);
 ((CHAR_boxed) L16)->immutable_part = ' ';
 plus_s5 = L16;
 stdout_self5 = ((FILE1) NULL);
 L19=ZALLOC(sizeof(struct FILE1_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=FILE1_tag;
 L18 = ((FILE1) L19);
 r5 = L18;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_dSTR(ret_val11, plus_s5);
 L21 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L221_=!(L21); 
 if (L221_) {
  create_self4 = ((OUT) NULL);
  ret_val12 = create_self4;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name20);
  stdout_self6 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r6 = L23;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AS_OUT600758956(self, ATTR(t,tp));
 }
 L26 = (ATTR(t,init)==((dAS_EXPR) NULL));
 L271_=!(L26); 
 if (L271_) {
  create_self5 = ((OUT) NULL);
  ret_val14 = create_self5;
  plus_self7 = ret_val14;
  plus_s7 = ((STR) &name154);
  stdout_self7 = ((FILE1) NULL);
  L29=ZALLOC(sizeof(struct FILE1_struct));
  if (L29==NULL) FATAL("Unable to allocate more memory");
  ((OB)L29)->header.tag=FILE1_tag;
  L28 = ((FILE1) L29);
  r7 = L28;
  
  SATTR(r7,fp,stdout);
  ret_val15 = r7;
  FILE_plus_STR(ret_val15, plus_s7);
 }
 create_self6 = ((OUT) NULL);
 ret_val16 = create_self6;
 plus_self8 = ret_val16;
 plus_s8 = ((STR) &name13);
 stdout_self8 = ((FILE1) NULL);
 L33=ZALLOC(sizeof(struct FILE1_struct));
 if (L33==NULL) FATAL("Unable to allocate more memory");
 ((OB)L33)->header.tag=FILE1_tag;
 L32 = ((FILE1) L33);
 r8 = L32;
 
 SATTR(r8,fp,stdout);
 ret_val17 = r8;
 FILE_plus_STR(ret_val17, plus_s8);
 L35 = (ATTR(t,next)==((dAS_CLASS_ELT) NULL));
 L361_=!(L35); 
 if (L361_) {
  AS_OUT2139428914(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT474809072(AS_OUT self, AS_CASE_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR case3;
 FILE1 L4;
 OB L5;
 extern STR name9;
 FILE1 L7;
 OB L8;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 extern STR else8;
 FILE1 L14;
 OB L15;
 INT L17;
 INT L181_;
 INT L19;
 INT L201_;
 extern STR endofcase;
 FILE1 L21;
 OB L22;
 if ((t==((AS_CASE_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &case3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,test1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name9);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L10 = (ATTR(t,when_part)==((AS_CASE_WHEN) NULL));
 L111_=!(L10); 
 if (L111_) {
  AS_OUT422363112(self, ATTR(t,when_part));
 }
 L12 = ATTR(t,no_else);
 L131_=!(L12); 
 if (L131_) {
  AS_OUT_ind(self);
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &else8);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  iinc_self = self;
  L17 = AS_OUT_indent;
  L181_=INTPLUS(L17,3); 
  AS_OUT_indent = L181_;
  AS_OUT749907512(self, ATTR(t,else_part));
  idec_self = self;
  L19 = AS_OUT_indent;
  L201_=INTMINUS(L19,3); 
  AS_OUT_indent = L201_;
 }
 AS_OUT_ind(self);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &endofcase);
 stdout_self4 = ((FILE1) NULL);
 L22=ZALLOC(sizeof(struct FILE1_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=FILE1_tag;
 L21 = ((FILE1) L22);
 r4 = L21;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT48907654(AS_OUT self, AS_ASSIGN_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 extern STR void1;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 FILE1 L6;
 OB L7;
 extern STR name21;
 FILE1 L9;
 OB L10;
 BOOL L12;
 BOOL L131_;
 extern STR name155;
 FILE1 L14;
 OB L15;
 extern STR name13;
 FILE1 L17;
 OB L18;
 if ((t==((AS_ASSIGN_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 L4 = (ATTR(t,lhs_expr)==((dAS_EXPR) NULL));
 L51_=!(L4); 
 if (L51_) {
  AS_OUT851761245(self, ATTR(t,lhs_expr));
 }
 else {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name21);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 L12 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L131_=!(L12); 
 if (L131_) {
  AS_OUT600758956(self, ATTR(t,tp));
 }
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name155);
 stdout_self3 = ((FILE1) NULL);
 L15=ZALLOC(sizeof(struct FILE1_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 ((OB)L15)->header.tag=FILE1_tag;
 L14 = ((FILE1) L15);
 r3 = L14;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 AS_OUT851761245(self, ATTR(t,rhs));
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &name13);
 stdout_self4 = ((FILE1) NULL);
 L18=ZALLOC(sizeof(struct FILE1_struct));
 if (L18==NULL) FATAL("Unable to allocate more memory");
 ((OB)L18)->header.tag=FILE1_tag;
 L17 = ((FILE1) L18);
 r4 = L17;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT516254387(AS_OUT self, AS_ARRAY_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name152;
 FILE1 L4;
 OB L5;
 extern STR name152;
 FILE1 L7;
 OB L8;
 if ((t==((AS_ARRAY_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name152);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,elts));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name152);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT57117984(AS_OUT self, AS_CHAR_LIT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 INT L4;
 CHAR L51_;
 FILE1 L6;
 OB L7;
 if ((t==((AS_CHAR_LIT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 L4 = ATTR(t,val1);
 L51_=(CHAR)L4; 
 plus_s1 = CHAR_prettyrSTR(L51_);
 stdout_self1 = ((FILE1) NULL);
 L7=ZALLOC(sizeof(struct FILE1_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=FILE1_tag;
 L6 = ((FILE1) L7);
 r1 = L6;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT599192453(AS_OUT self, AS_NEW_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR new1;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name24;
 FILE1 L9;
 OB L10;
 extern STR name26;
 FILE1 L12;
 OB L13;
 if ((t==((AS_NEW_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &new1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,arg)==((dAS_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name24);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT851761245(self, ATTR(t,arg));
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name26);
  stdout_self3 = ((FILE1) NULL);
  L13=ZALLOC(sizeof(struct FILE1_struct));
  if (L13==NULL) FATAL("Unable to allocate more memory");
  ((OB)L13)->header.tag=FILE1_tag;
  L12 = ((FILE1) L13);
  r3 = L12;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT600758956(AS_OUT self, AS_TYPE_SPEC t) {
 INT L11 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self7;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 OUT create_self8;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 OUT create_self9;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self10;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 OUT create_self11;
 OUT ret_val22;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val23;
 FILE1 r11;
 OUT create_self12;
 OUT ret_val24;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self13;
 OUT ret_val26;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val27;
 FILE1 r13;
 OUT create_self14;
 OUT ret_val28;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val29;
 FILE1 r14;
 extern STR void1;
 FILE1 L2;
 OB L3;
 BOOL L5;
 BOOL L61_;
 extern STR SAME;
 FILE1 L7;
 OB L8;
 extern STR ROUT1;
 FILE1 L10;
 OB L12;
 BOOL L14;
 BOOL L151_;
 extern STR name2;
 FILE1 L16;
 OB L17;
 extern STR name4;
 FILE1 L19;
 OB L20;
 BOOL L22;
 BOOL L231_;
 extern STR name21;
 FILE1 L24;
 OB L25;
 extern STR ITER;
 FILE1 L27;
 OB L28;
 BOOL L30;
 BOOL L311_;
 extern STR name2;
 FILE1 L32;
 OB L33;
 extern STR name4;
 FILE1 L35;
 OB L36;
 BOOL L38;
 BOOL L391_;
 extern STR name21;
 FILE1 L40;
 OB L42;
 FILE1 L44;
 OB L45;
 BOOL L47;
 BOOL L481_;
 extern STR name2;
 FILE1 L49;
 OB L50;
 extern STR name4;
 FILE1 L52;
 OB L53;
 extern STR b;
 FILE1 L55;
 OB L56;
 BOOL L58;
 BOOL L591_;
 extern STR name29;
 FILE1 L60;
 OB L62;
 if ((t==((AS_TYPE_SPEC) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L3=ZALLOC(sizeof(struct FILE1_struct));
  if (L3==NULL) FATAL("Unable to allocate more memory");
  ((OB)L3)->header.tag=FILE1_tag;
  L2 = ((FILE1) L3);
  r = L2;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 L5 = (ATTR(t,mode1)==((AS_ARG_MODE) NULL));
 L61_=!(L5); 
 if (L61_) {
  AS_OUT1082396938(self, ATTR(t,mode1));
 }
 L11 = ATTR(t,kind);
 switch (L11) {
  case 3: 
   create_self1 = ((OUT) NULL);
   ret_val2 = create_self1;
   plus_self1 = ret_val2;
   plus_s1 = ((STR) &SAME);
   stdout_self1 = ((FILE1) NULL);
   L8=ZALLOC(sizeof(struct FILE1_struct));
   if (L8==NULL) FATAL("Unable to allocate more memory");
   ((OB)L8)->header.tag=FILE1_tag;
   L7 = ((FILE1) L8);
   r1 = L7;
   
   SATTR(r1,fp,stdout);
   ret_val3 = r1;
   FILE_plus_STR(ret_val3, plus_s1);
   break;
  case 1: 
   create_self2 = ((OUT) NULL);
   ret_val4 = create_self2;
   plus_self2 = ret_val4;
   plus_s2 = ((STR) &ROUT1);
   stdout_self2 = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L10 = ((FILE1) L12);
   r2 = L10;
   
   SATTR(r2,fp,stdout);
   ret_val5 = r2;
   FILE_plus_STR(ret_val5, plus_s2);
   L14 = (ATTR(t,params)==((AS_TYPE_SPEC) NULL));
   L151_=!(L14); 
   if (L151_) {
    create_self3 = ((OUT) NULL);
    ret_val6 = create_self3;
    plus_self3 = ret_val6;
    plus_s3 = ((STR) &name2);
    stdout_self3 = ((FILE1) NULL);
    L17=ZALLOC(sizeof(struct FILE1_struct));
    if (L17==NULL) FATAL("Unable to allocate more memory");
    ((OB)L17)->header.tag=FILE1_tag;
    L16 = ((FILE1) L17);
    r3 = L16;
    
    SATTR(r3,fp,stdout);
    ret_val7 = r3;
    FILE_plus_STR(ret_val7, plus_s3);
    AS_OUT600758956(self, ATTR(t,params));
    create_self4 = ((OUT) NULL);
    ret_val8 = create_self4;
    plus_self4 = ret_val8;
    plus_s4 = ((STR) &name4);
    stdout_self4 = ((FILE1) NULL);
    L20=ZALLOC(sizeof(struct FILE1_struct));
    if (L20==NULL) FATAL("Unable to allocate more memory");
    ((OB)L20)->header.tag=FILE1_tag;
    L19 = ((FILE1) L20);
    r4 = L19;
    
    SATTR(r4,fp,stdout);
    ret_val9 = r4;
    FILE_plus_STR(ret_val9, plus_s4);
   }
   L22 = (ATTR(t,ret)==((AS_TYPE_SPEC) NULL));
   L231_=!(L22); 
   if (L231_) {
    create_self5 = ((OUT) NULL);
    ret_val10 = create_self5;
    plus_self5 = ret_val10;
    plus_s5 = ((STR) &name21);
    stdout_self5 = ((FILE1) NULL);
    L25=ZALLOC(sizeof(struct FILE1_struct));
    if (L25==NULL) FATAL("Unable to allocate more memory");
    ((OB)L25)->header.tag=FILE1_tag;
    L24 = ((FILE1) L25);
    r5 = L24;
    
    SATTR(r5,fp,stdout);
    ret_val11 = r5;
    FILE_plus_STR(ret_val11, plus_s5);
    AS_OUT600758956(self, ATTR(t,ret));
   }
   break;
  case 2: 
   create_self6 = ((OUT) NULL);
   ret_val12 = create_self6;
   plus_self6 = ret_val12;
   plus_s6 = ((STR) &ITER);
   stdout_self6 = ((FILE1) NULL);
   L28=ZALLOC(sizeof(struct FILE1_struct));
   if (L28==NULL) FATAL("Unable to allocate more memory");
   ((OB)L28)->header.tag=FILE1_tag;
   L27 = ((FILE1) L28);
   r6 = L27;
   
   SATTR(r6,fp,stdout);
   ret_val13 = r6;
   FILE_plus_STR(ret_val13, plus_s6);
   L30 = (ATTR(t,params)==((AS_TYPE_SPEC) NULL));
   L311_=!(L30); 
   if (L311_) {
    create_self7 = ((OUT) NULL);
    ret_val14 = create_self7;
    plus_self7 = ret_val14;
    plus_s7 = ((STR) &name2);
    stdout_self7 = ((FILE1) NULL);
    L33=ZALLOC(sizeof(struct FILE1_struct));
    if (L33==NULL) FATAL("Unable to allocate more memory");
    ((OB)L33)->header.tag=FILE1_tag;
    L32 = ((FILE1) L33);
    r7 = L32;
    
    SATTR(r7,fp,stdout);
    ret_val15 = r7;
    FILE_plus_STR(ret_val15, plus_s7);
    AS_OUT600758956(self, ATTR(t,params));
    create_self8 = ((OUT) NULL);
    ret_val16 = create_self8;
    plus_self8 = ret_val16;
    plus_s8 = ((STR) &name4);
    stdout_self8 = ((FILE1) NULL);
    L36=ZALLOC(sizeof(struct FILE1_struct));
    if (L36==NULL) FATAL("Unable to allocate more memory");
    ((OB)L36)->header.tag=FILE1_tag;
    L35 = ((FILE1) L36);
    r8 = L35;
    
    SATTR(r8,fp,stdout);
    ret_val17 = r8;
    FILE_plus_STR(ret_val17, plus_s8);
   }
   L38 = (ATTR(t,ret)==((AS_TYPE_SPEC) NULL));
   L391_=!(L38); 
   if (L391_) {
    create_self9 = ((OUT) NULL);
    ret_val18 = create_self9;
    plus_self9 = ret_val18;
    plus_s9 = ((STR) &name21);
    stdout_self9 = ((FILE1) NULL);
    L42=ZALLOC(sizeof(struct FILE1_struct));
    if (L42==NULL) FATAL("Unable to allocate more memory");
    ((OB)L42)->header.tag=FILE1_tag;
    L40 = ((FILE1) L42);
    r9 = L40;
    
    SATTR(r9,fp,stdout);
    ret_val19 = r9;
    FILE_plus_STR(ret_val19, plus_s9);
    AS_OUT600758956(self, ATTR(t,ret));
   }
   break;
  default: ;
   create_self10 = ((OUT) NULL);
   ret_val20 = create_self10;
   plus_self10 = ret_val20;
   plus_s10 = AS_OUT422425760(self, STR_upperrSTR(ATTR(t,name1).str));
   stdout_self10 = ((FILE1) NULL);
   L45=ZALLOC(sizeof(struct FILE1_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   ((OB)L45)->header.tag=FILE1_tag;
   L44 = ((FILE1) L45);
   r10 = L44;
   
   SATTR(r10,fp,stdout);
   ret_val21 = r10;
   FILE_plus_STR(ret_val21, plus_s10);
   L47 = (ATTR(t,params)==((AS_TYPE_SPEC) NULL));
   L481_=!(L47); 
   if (L481_) {
    create_self11 = ((OUT) NULL);
    ret_val22 = create_self11;
    plus_self11 = ret_val22;
    plus_s11 = ((STR) &name2);
    stdout_self11 = ((FILE1) NULL);
    L50=ZALLOC(sizeof(struct FILE1_struct));
    if (L50==NULL) FATAL("Unable to allocate more memory");
    ((OB)L50)->header.tag=FILE1_tag;
    L49 = ((FILE1) L50);
    r11 = L49;
    
    SATTR(r11,fp,stdout);
    ret_val23 = r11;
    FILE_plus_STR(ret_val23, plus_s11);
    AS_OUT600758956(self, ATTR(t,params));
    create_self12 = ((OUT) NULL);
    ret_val24 = create_self12;
    plus_self12 = ret_val24;
    plus_s12 = ((STR) &name4);
    stdout_self12 = ((FILE1) NULL);
    L53=ZALLOC(sizeof(struct FILE1_struct));
    if (L53==NULL) FATAL("Unable to allocate more memory");
    ((OB)L53)->header.tag=FILE1_tag;
    L52 = ((FILE1) L53);
    r12 = L52;
    
    SATTR(r12,fp,stdout);
    ret_val25 = r12;
    FILE_plus_STR(ret_val25, plus_s12);
   }
 }
 if (ATTR(t,is_hot)) {
  create_self13 = ((OUT) NULL);
  ret_val26 = create_self13;
  plus_self13 = ret_val26;
  plus_s13 = ((STR) &b);
  stdout_self13 = ((FILE1) NULL);
  L56=ZALLOC(sizeof(struct FILE1_struct));
  if (L56==NULL) FATAL("Unable to allocate more memory");
  ((OB)L56)->header.tag=FILE1_tag;
  L55 = ((FILE1) L56);
  r13 = L55;
  
  SATTR(r13,fp,stdout);
  ret_val27 = r13;
  FILE_plus_STR(ret_val27, plus_s13);
 }
 L58 = (ATTR(t,next)==((AS_TYPE_SPEC) NULL));
 L591_=!(L58); 
 if (L591_) {
  create_self14 = ((OUT) NULL);
  ret_val28 = create_self14;
  plus_self14 = ret_val28;
  plus_s14 = ((STR) &name29);
  stdout_self14 = ((FILE1) NULL);
  L62=ZALLOC(sizeof(struct FILE1_struct));
  if (L62==NULL) FATAL("Unable to allocate more memory");
  ((OB)L62)->header.tag=FILE1_tag;
  L60 = ((FILE1) L62);
  r14 = L60;
  
  SATTR(r14,fp,stdout);
  ret_val29 = r14;
  FILE_plus_STR(ret_val29, plus_s14);
  AS_OUT600758956(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT629983961(AS_OUT self, AS_WHERE_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR where3;
 FILE1 L4;
 OB L5;
 extern STR name26;
 FILE1 L7;
 OB L8;
 if ((t==((AS_WHERE_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &where3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,e));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name26);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT662922118(AS_OUT self, AS_EXCEPT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR exception1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_EXCEPT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &exception1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT767507057(AS_OUT self, AS_IS_VOID_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR void11;
 FILE1 L4;
 OB L5;
 extern STR name26;
 FILE1 L7;
 OB L8;
 if ((t==((AS_IS_VOID_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &void11);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,arg));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name26);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT774387720(AS_OUT self, AS_CONST_DEF t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 OUT ret_val5;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 dSTR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self3;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 extern STR AS_CONST_DEFvoid;
 FILE1 L1;
 OB L2;
 extern STR private3;
 FILE1 L4;
 OB L5;
 extern STR const3;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L11;
 dSTR L13;
 OB L14;
 FILE1 L15;
 OB L16;
 BOOL L18;
 BOOL L191_;
 extern STR name20;
 FILE1 L20;
 OB L21;
 BOOL L23;
 BOOL L241_;
 extern STR name154;
 FILE1 L25;
 OB L26;
 extern STR name13;
 FILE1 L28;
 OB L29;
 BOOL L32;
 BOOL L331_;
 if ((t==((AS_CONST_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_CONST_DEFvoid);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 if (ATTR(t,is_private)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &private3);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &const3);
 stdout_self2 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r2 = L7;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 plus_s3 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 L14=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 memset(L14,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L14)->header.tag=CHAR_tag;
 L13 = (dSTR)((CHAR_boxed) L14);
 ((CHAR_boxed) L13)->immutable_part = ' ';
 plus_s4 = L13;
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_dSTR(ret_val9, plus_s4);
 L18 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L191_=!(L18); 
 if (L191_) {
  create_self3 = ((OUT) NULL);
  ret_val10 = create_self3;
  plus_self5 = ret_val10;
  plus_s5 = ((STR) &name20);
  stdout_self5 = ((FILE1) NULL);
  L21=ZALLOC(sizeof(struct FILE1_struct));
  if (L21==NULL) FATAL("Unable to allocate more memory");
  ((OB)L21)->header.tag=FILE1_tag;
  L20 = ((FILE1) L21);
  r5 = L20;
  
  SATTR(r5,fp,stdout);
  ret_val11 = r5;
  FILE_plus_STR(ret_val11, plus_s5);
  AS_OUT600758956(self, ATTR(t,tp));
 }
 L23 = (ATTR(t,init)==((dAS_EXPR) NULL));
 L241_=!(L23); 
 if (L241_) {
  create_self4 = ((OUT) NULL);
  ret_val12 = create_self4;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name154);
  stdout_self6 = ((FILE1) NULL);
  L26=ZALLOC(sizeof(struct FILE1_struct));
  if (L26==NULL) FATAL("Unable to allocate more memory");
  ((OB)L26)->header.tag=FILE1_tag;
  L25 = ((FILE1) L26);
  r6 = L25;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AS_OUT851761245(self, ATTR(t,init));
 }
 create_self5 = ((OUT) NULL);
 ret_val14 = create_self5;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &name13);
 stdout_self7 = ((FILE1) NULL);
 L29=ZALLOC(sizeof(struct FILE1_struct));
 if (L29==NULL) FATAL("Unable to allocate more memory");
 ((OB)L29)->header.tag=FILE1_tag;
 L28 = ((FILE1) L29);
 r7 = L28;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
 L32 = (ATTR(t,next)==((dAS_CLASS_ELT) NULL));
 L331_=!(L32); 
 if (L331_) {
  AS_OUT2139428914(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT778273489(AS_OUT self, AS_PROTECT_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 AS_OUT iinc_self1;
 AS_OUT idec_self1;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR protect2;
 FILE1 L4;
 OB L5;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 BOOL L11;
 BOOL L121_;
 BOOL L13;
 BOOL L141_;
 extern STR else8;
 FILE1 L15;
 OB L16;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 extern STR endofprotect;
 FILE1 L22;
 OB L23;
 if ((t==((AS_PROTECT_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &protect2);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 iinc_self = self;
 L7 = AS_OUT_indent;
 L81_=INTPLUS(L7,3); 
 AS_OUT_indent = L81_;
 AS_OUT749907512(self, ATTR(t,body));
 idec_self = self;
 L9 = AS_OUT_indent;
 L101_=INTMINUS(L9,3); 
 AS_OUT_indent = L101_;
 L11 = (ATTR(t,when_part)==((AS_PROTECT_WHEN) NULL));
 L121_=!(L11); 
 if (L121_) {
  AS_OUT1577197339(self, ATTR(t,when_part));
 }
 L13 = ATTR(t,no_else);
 L141_=!(L13); 
 if (L141_) {
  AS_OUT_ind(self);
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &else8);
  stdout_self2 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r2 = L15;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  iinc_self1 = self;
  L18 = AS_OUT_indent;
  L191_=INTPLUS(L18,3); 
  AS_OUT_indent = L191_;
  AS_OUT749907512(self, ATTR(t,else_part));
  idec_self1 = self;
  L20 = AS_OUT_indent;
  L211_=INTMINUS(L20,3); 
  AS_OUT_indent = L211_;
 }
 AS_OUT_ind(self);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &endofprotect);
 stdout_self3 = ((FILE1) NULL);
 L23=ZALLOC(sizeof(struct FILE1_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=FILE1_tag;
 L22 = ((FILE1) L23);
 r3 = L22;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT801616852(AS_OUT self, AS_CLU553570599 t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR cluster_size;
 FILE1 L4;
 OB L5;
 if ((t==((AS_CLU553570599) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &cluster_size);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT804297708(AS_OUT self, AS_SELF_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR self1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_SELF_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &self1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT826513318(AS_OUT self, AS_COHORT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR cohort;
 FILE1 L4;
 OB L5;
 FILE1 L7;
 OB L8;
 if ((t==((AS_COHORT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 if ((ATTR(t,name_after_pSath).str==(STR)0)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &cohort);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 else {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ATTR(t,name_after_pSath).str;
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
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT851761245(AS_OUT self, dAS_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR dAS_EXPRvoid;
 FILE1 L1;
 OB L2;
 dAS_EXPR L4;
 BOOL L5;
 BOOL L61_;
 extern STR name29;
 FILE1 L7;
 OB L8;
 dAS_EXPR L10;
 if ((t==((dAS_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &dAS_EXPRvoid);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 switch (TAG(t)) {
  case AS_SELF_EXPR_tag:
   AS_OUT804297708(self, ((AS_SELF_EXPR) t)); break;
  case AS_CALL_EXPR_tag:
   AS_OUT1983800340(self, ((AS_CALL_EXPR) t)); break;
  case AS_VOID_EXPR_tag:
   AS_OUT1019461868(self, ((AS_VOID_EXPR) t)); break;
  case AS_IS_VOID_EXPR_tag:
   AS_OUT767507057(self, ((AS_IS_VOID_EXPR) t)); break;
  case AS_ARRAY_EXPR_tag:
   AS_OUT516254387(self, ((AS_ARRAY_EXPR) t)); break;
  case AS_CREATE_EXPR_tag:
   AS_OUT1211286518(self, ((AS_CREATE_EXPR) t)); break;
  case AS_BOU14269336_tag:
   AS_OUT898752312(self, ((AS_BOU14269336) t)); break;
  case AS_UND152986614_tag:
   AS_OUT2138884099(self, ((AS_UND152986614) t)); break;
  case AS_AND_EXPR_tag:
   AS_OUT359892079(self, ((AS_AND_EXPR) t)); break;
  case AS_OR_EXPR_tag:
   AS_OUT1434191198(self, ((AS_OR_EXPR) t)); break;
  case AS_EXCEPT_EXPR_tag:
   AS_OUT662922118(self, ((AS_EXCEPT_EXPR) t)); break;
  case AS_NEW_EXPR_tag:
   AS_OUT599192453(self, ((AS_NEW_EXPR) t)); break;
  case AS_INITIAL_EXPR_tag:
   AS_OUT1347508463(self, ((AS_INITIAL_EXPR) t)); break;
  case AS_BREAK_EXPR_tag:
   AS_OUT26552353(self, ((AS_BREAK_EXPR) t)); break;
  case AS_RESULT_EXPR_tag:
   AS_OUT1070397350(self, ((AS_RESULT_EXPR) t)); break;
  case AS_BOOL_LIT_EXPR_tag:
   AS_OUT1255290752(self, ((AS_BOOL_LIT_EXPR) t)); break;
  case AS_CHAR_LIT_EXPR_tag:
   AS_OUT57117984(self, ((AS_CHAR_LIT_EXPR) t)); break;
  case AS_STR_LIT_EXPR_tag:
   AS_OUT387909983(self, ((AS_STR_LIT_EXPR) t)); break;
  case AS_INT_LIT_EXPR_tag:
   AS_OUT160973005(self, ((AS_INT_LIT_EXPR) t)); break;
  case AS_FLT_LIT_EXPR_tag:
   AS_OUT454374187(self, ((AS_FLT_LIT_EXPR) t)); break;
  case AS_HERE_EXPR_tag:
   AS_OUT895691444(self, ((AS_HERE_EXPR) t)); break;
  case AS_ANY_EXPR_tag:
   AS_OUT1954367915(self, ((AS_ANY_EXPR) t)); break;
  case AS_WHERE_EXPR_tag:
   AS_OUT629983961(self, ((AS_WHERE_EXPR) t)); break;
  case AS_NEAR_EXPR_tag:
   AS_OUT1170292156(self, ((AS_NEAR_EXPR) t)); break;
  case AS_FAR_EXPR_tag:
   AS_OUT1693873817(self, ((AS_FAR_EXPR) t)); break;
  case AS_AT_EXPR_tag:
   AS_OUT90627778(self, ((AS_AT_EXPR) t)); break;
  case AS_CLUSTER_EXPR_tag:
   AS_OUT1790419101(self, ((AS_CLUSTER_EXPR) t)); break;
  case AS_CLU553570599_tag:
   AS_OUT801616852(self, ((AS_CLU553570599) t)); break;
  case AS_COHORT_EXPR_tag:
   AS_OUT826513318(self, ((AS_COHORT_EXPR) t)); break;
  case AS_GLOBAL_EXPR_tag:
   AS_OUT1034803282(self, ((AS_GLOBAL_EXPR) t)); break;
  default: ;
   FATAL("No applicable type in typecase\nin AS_OUT::AS_EXPR_out($AS_EXPR)\n./Abstract_Syntax/as_out.sa:150:14");
 }
 L4 = t;
 L5 = ((*dAS_EX337431651[TAG(L4)])(L4)==((dAS_EXPR) NULL));
 L61_=!(L5); 
 if (L61_) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &name29);
  stdout_self1 = ((FILE1) NULL);
  L8=ZALLOC(sizeof(struct FILE1_struct));
  if (L8==NULL) FATAL("Unable to allocate more memory");
  ((OB)L8)->header.tag=FILE1_tag;
  L7 = ((FILE1) L8);
  r1 = L7;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  L10 = t;
  AS_OUT851761245(self, (*dAS_EX337431651[TAG(L10)])(L10));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT895691444(AS_OUT self, AS_HERE_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR here1;
 FILE1 L4;
 OB L5;
 if ((t==((AS_HERE_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &here1);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT898752312(AS_OUT self, AS_BOU14269336 t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name1111;
 FILE1 L4;
 OB L5;
 extern STR ITER2;
 FILE1 L7;
 OB L8;
 extern STR ROUT3;
 FILE1 L10;
 OB L11;
 BOOL L13;
 BOOL L141_;
 extern STR name21;
 FILE1 L15;
 OB L16;
 extern STR name26;
 FILE1 L18;
 OB L19;
 if ((t==((AS_BOU14269336) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name1111);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 if (ATTR(t,is_iter)) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &ITER2);
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
 else {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &ROUT3);
  stdout_self3 = ((FILE1) NULL);
  L11=ZALLOC(sizeof(struct FILE1_struct));
  if (L11==NULL) FATAL("Unable to allocate more memory");
  ((OB)L11)->header.tag=FILE1_tag;
  L10 = ((FILE1) L11);
  r3 = L10;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
 }
 AS_OUT1983800340(self, ATTR(t,call1));
 L13 = (ATTR(t,ret)==((AS_TYPE_SPEC) NULL));
 L141_=!(L13); 
 if (L141_) {
  create_self4 = ((OUT) NULL);
  ret_val8 = create_self4;
  plus_self4 = ret_val8;
  plus_s4 = ((STR) &name21);
  stdout_self4 = ((FILE1) NULL);
  L16=ZALLOC(sizeof(struct FILE1_struct));
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=FILE1_tag;
  L15 = ((FILE1) L16);
  r4 = L15;
  
  SATTR(r4,fp,stdout);
  ret_val9 = r4;
  FILE_plus_STR(ret_val9, plus_s4);
  AS_OUT600758956(self, ATTR(t,ret));
 }
 create_self5 = ((OUT) NULL);
 ret_val10 = create_self5;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name26);
 stdout_self5 = ((FILE1) NULL);
 L19=ZALLOC(sizeof(struct FILE1_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=FILE1_tag;
 L18 = ((FILE1) L19);
 r5 = L18;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT90627778(AS_OUT self, AS_AT_EXPR t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR name153;
 FILE1 L4;
 OB L5;
 if ((t==((AS_AT_EXPR) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT851761245(self, ATTR(t,e));
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name153);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,at));
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT916258175(AS_OUT self, AS_LOCK_IF_WHEN t) {
 BOOL with_guard = BOOL_zero;
 dAS_EXPR tv;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 OUT create_self4;
 OUT ret_val8;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 AS_OUT iinc_self;
 AS_OUT idec_self;
 OUT create_self5;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 extern STR void1;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 extern STR if5;
 FILE1 L6;
 OB L7;
 extern STR name10;
 FILE1 L9;
 OB L10;
 extern STR when3;
 FILE1 L12;
 OB L13;
 extern STR then3;
 FILE1 L15;
 OB L16;
 INT L18;
 INT L191_;
 INT L20;
 INT L211_;
 extern STR endlockclause;
 FILE1 L22;
 OB L23;
 BOOL L25;
 BOOL L261_;
 if ((t==((AS_LOCK_IF_WHEN) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 with_guard = TRUE;
 tv = ATTR(t,val1);
 if (tv==NULL) {
  goto other464;
 } else
 switch (TAG(tv)) {
  case AS_BOOL_LIT_EXPR_tag:
   L4 = ATTR(((AS_BOOL_LIT_EXPR) tv),val1);
   L51_=L4==TRUE; 
   if (L51_) {
    with_guard = FALSE;
   } break;
  default: ;
  other464: ;
 }
 if (with_guard) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &if5);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  AS_OUT851761245(self, ATTR(t,val1));
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name10);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &when3);
 stdout_self3 = ((FILE1) NULL);
 L13=ZALLOC(sizeof(struct FILE1_struct));
 if (L13==NULL) FATAL("Unable to allocate more memory");
 ((OB)L13)->header.tag=FILE1_tag;
 L12 = ((FILE1) L13);
 r3 = L12;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 AS_OUT851761245(self, ATTR(t,e_list));
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &then3);
 stdout_self4 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r4 = L15;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 iinc_self = self;
 L18 = AS_OUT_indent;
 L191_=INTPLUS(L18,3); 
 AS_OUT_indent = L191_;
 AS_OUT749907512(self, ATTR(t,then_part));
 idec_self = self;
 L20 = AS_OUT_indent;
 L211_=INTMINUS(L20,3); 
 AS_OUT_indent = L211_;
 AS_OUT_ind(self);
 create_self5 = ((OUT) NULL);
 ret_val10 = create_self5;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &endlockclause);
 stdout_self5 = ((FILE1) NULL);
 L23=ZALLOC(sizeof(struct FILE1_struct));
 if (L23==NULL) FATAL("Unable to allocate more memory");
 ((OB)L23)->header.tag=FILE1_tag;
 L22 = ((FILE1) L23);
 r5 = L22;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_STR(ret_val11, plus_s5);
 L25 = (ATTR(t,next)==((AS_LOCK_IF_WHEN) NULL));
 L261_=!(L25); 
 if (L261_) {
  AS_OUT916258175(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT918185922(AS_OUT self, AS_ASSERT_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR assert3;
 FILE1 L4;
 OB L5;
 extern STR name13;
 FILE1 L7;
 OB L8;
 if ((t==((AS_ASSERT_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &assert3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 AS_OUT851761245(self, ATTR(t,test1));
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
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


#undef IS_ITER
#define IS_ITER 0

void AS_OUT962799085(AS_OUT self, AS_YIELD_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 extern STR yield3;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name13;
 FILE1 L9;
 OB L10;
 if ((t==((AS_YIELD_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &yield3);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,val1)==((dAS_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  AS_OUT851761245(self, ATTR(t,val1));
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT964805355(AS_OUT self, AS_ATTR_DEF t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val9;
 FILE1 stdout_self4;
 FILE1 ret_val10;
 FILE1 r4;
 OUT plus_self5;
 dSTR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val11;
 FILE1 r5;
 OUT create_self4;
 OUT ret_val12;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val13;
 FILE1 r6;
 OUT create_self5;
 OUT ret_val14;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val15;
 FILE1 r7;
 extern STR AS_ATTR_DEFvoid;
 FILE1 L1;
 OB L2;
 extern STR private3;
 FILE1 L4;
 OB L5;
 extern STR readonly2;
 FILE1 L7;
 OB L8;
 extern STR attr3;
 FILE1 L10;
 OB L11;
 FILE1 L13;
 OB L14;
 dSTR L16;
 OB L17;
 FILE1 L18;
 OB L19;
 BOOL L21;
 BOOL L221_;
 extern STR name20;
 FILE1 L23;
 OB L24;
 extern STR name13;
 FILE1 L26;
 OB L27;
 BOOL L29;
 BOOL L301_;
 if ((t==((AS_ATTR_DEF) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &AS_ATTR_DEFvoid);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 if (ATTR(t,is_private)) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &private3);
  stdout_self1 = ((FILE1) NULL);
  L5=ZALLOC(sizeof(struct FILE1_struct));
  if (L5==NULL) FATAL("Unable to allocate more memory");
  ((OB)L5)->header.tag=FILE1_tag;
  L4 = ((FILE1) L5);
  r1 = L4;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
 }
 if (ATTR(t,is_readonly)) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &readonly2);
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
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &attr3);
 stdout_self3 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r3 = L10;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_STR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self4 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r4 = L13;
 
 SATTR(r4,fp,stdout);
 ret_val10 = r4;
 FILE_plus_STR(ret_val10, plus_s4);
 ret_val9 = plus_self4;
 plus_self5 = ret_val9;
 L17=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 memset(L17,0,sizeof(struct CHAR_boxed_struct));
 ((OB)L17)->header.tag=CHAR_tag;
 L16 = (dSTR)((CHAR_boxed) L17);
 ((CHAR_boxed) L16)->immutable_part = ' ';
 plus_s5 = L16;
 stdout_self5 = ((FILE1) NULL);
 L19=ZALLOC(sizeof(struct FILE1_struct));
 if (L19==NULL) FATAL("Unable to allocate more memory");
 ((OB)L19)->header.tag=FILE1_tag;
 L18 = ((FILE1) L19);
 r5 = L18;
 
 SATTR(r5,fp,stdout);
 ret_val11 = r5;
 FILE_plus_dSTR(ret_val11, plus_s5);
 L21 = (ATTR(t,tp)==((AS_TYPE_SPEC) NULL));
 L221_=!(L21); 
 if (L221_) {
  create_self4 = ((OUT) NULL);
  ret_val12 = create_self4;
  plus_self6 = ret_val12;
  plus_s6 = ((STR) &name20);
  stdout_self6 = ((FILE1) NULL);
  L24=ZALLOC(sizeof(struct FILE1_struct));
  if (L24==NULL) FATAL("Unable to allocate more memory");
  ((OB)L24)->header.tag=FILE1_tag;
  L23 = ((FILE1) L24);
  r6 = L23;
  
  SATTR(r6,fp,stdout);
  ret_val13 = r6;
  FILE_plus_STR(ret_val13, plus_s6);
  AS_OUT600758956(self, ATTR(t,tp));
 }
 create_self5 = ((OUT) NULL);
 ret_val14 = create_self5;
 plus_self7 = ret_val14;
 plus_s7 = ((STR) &name13);
 stdout_self7 = ((FILE1) NULL);
 L27=ZALLOC(sizeof(struct FILE1_struct));
 if (L27==NULL) FATAL("Unable to allocate more memory");
 ((OB)L27)->header.tag=FILE1_tag;
 L26 = ((FILE1) L27);
 r7 = L26;
 
 SATTR(r7,fp,stdout);
 ret_val15 = r7;
 FILE_plus_STR(ret_val15, plus_s7);
 L29 = (ATTR(t,next)==((dAS_CLASS_ELT) NULL));
 L301_=!(L29); 
 if (L301_) {
  AS_OUT2139428914(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT966193580(AS_OUT self, AS_PARAM_DEC t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val6;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 extern STR void1;
 FILE1 L1;
 OB L2;
 FILE1 L4;
 OB L5;
 BOOL L7;
 BOOL L81_;
 extern STR name151;
 FILE1 L9;
 OB L10;
 BOOL L12;
 BOOL L131_;
 extern STR name29;
 FILE1 L14;
 OB L15;
 if ((t==((AS_PARAM_DEC) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = AS_OUT422425760(self, ATTR(t,name1).str);
 stdout_self1 = ((FILE1) NULL);
 L5=ZALLOC(sizeof(struct FILE1_struct));
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FILE1_tag;
 L4 = ((FILE1) L5);
 r1 = L4;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L7 = (ATTR(t,type_constraint)==((AS_TYPE_SPEC) NULL));
 L81_=!(L7); 
 if (L81_) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &name151);
  stdout_self2 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r2 = L9;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
  AS_OUT600758956(self, ATTR(t,type_constraint));
 }
 L12 = (ATTR(t,next)==((AS_PARAM_DEC) NULL));
 L131_=!(L12); 
 if (L131_) {
  create_self3 = ((OUT) NULL);
  ret_val6 = create_self3;
  plus_self3 = ret_val6;
  plus_s3 = ((STR) &name29);
  stdout_self3 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r3 = L14;
  
  SATTR(r3,fp,stdout);
  ret_val7 = r3;
  FILE_plus_STR(ret_val7, plus_s3);
  AS_OUT966193580(self, ATTR(t,next));
 }
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT995003644(AS_OUT self, AS_EXPR_STMT t) {
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 OUT create_self1;
 OUT ret_val2;
 OUT plus_self1;
 STR plus_s1;
 FILE1 stdout_self1;
 FILE1 ret_val3;
 FILE1 r1;
 OUT create_self2;
 OUT ret_val4;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val5;
 FILE1 r2;
 extern STR void1;
 FILE1 L1;
 OB L2;
 BOOL L4;
 BOOL L51_;
 extern STR name153;
 FILE1 L6;
 OB L7;
 extern STR name13;
 FILE1 L9;
 OB L10;
 if ((t==((AS_EXPR_STMT) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &void1);
  stdout_self = ((FILE1) NULL);
  L2=ZALLOC(sizeof(struct FILE1_struct));
  if (L2==NULL) FATAL("Unable to allocate more memory");
  ((OB)L2)->header.tag=FILE1_tag;
  L1 = ((FILE1) L2);
  r = L1;
  
  SATTR(r,fp,stdout);
  ret_val1 = r;
  FILE_plus_STR(ret_val1, plus_s);
  return;
 }
 AS_OUT_ind(self);
 AS_OUT851761245(self, ATTR(t,e));
 L4 = (ATTR(t,at)==((dAS_EXPR) NULL));
 L51_=!(L4); 
 if (L51_) {
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &name153);
  stdout_self1 = ((FILE1) NULL);
  L7=ZALLOC(sizeof(struct FILE1_struct));
  if (L7==NULL) FATAL("Unable to allocate more memory");
  ((OB)L7)->header.tag=FILE1_tag;
  L6 = ((FILE1) L7);
  r1 = L6;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  AS_OUT851761245(self, ATTR(t,at));
 }
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &name13);
 stdout_self2 = ((FILE1) NULL);
 L10=ZALLOC(sizeof(struct FILE1_struct));
 if (L10==NULL) FATAL("Unable to allocate more memory");
 ((OB)L10)->header.tag=FILE1_tag;
 L9 = ((FILE1) L10);
 r2 = L9;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
}


#undef IS_ITER
#define IS_ITER 0

void AS_OUT_ind(AS_OUT self) {
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 FILE1 stdout_self;
 FILE1 ret_val1;
 FILE1 r;
 extern STR name10;
 FILE1 L4;
 OB L5;
 INT L71_;
 {
  BOOL f_L31_ = TRUE;
  /* loop index variable */
  L21 = 0;
  L11 = AS_OUT_indent;
  while (1) {
   if(L21>=L11)  goto after_loop; 
   ;
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &name10);
   stdout_self = ((FILE1) NULL);
   L5=ZALLOC(sizeof(struct FILE1_struct));
   if (L5==NULL) FATAL("Unable to allocate more memory");
   ((OB)L5)->header.tag=FILE1_tag;
   L4 = ((FILE1) L5);
   r = L4;
   
   SATTR(r,fp,stdout);
   ret_val1 = r;
   FILE_plus_STR(ret_val1, plus_s);
   L71_=INTPLUS(L21,1); 
   L21 = L71_;
  }
 }
 after_loop: ;
}
