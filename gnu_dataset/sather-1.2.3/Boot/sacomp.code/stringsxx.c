#include "sather.h"

/* Layouts */

#include "tags.h"

/* Globals */


/* Function declarations */


/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */


/* Code */

struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } AM_CURSOR2 = { {STR_tag}, 9, "AM_CURSOR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } AM_OB_DEF1 = { {STR_tag}, 9, "AM_OB_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } CODE_FILE1 = { {STR_tag}, 9, "CODE_FILE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } Checking = { {STR_tag}, 9, "Checking " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FMT_ERROR1 = { {STR_tag}, 9, "FMT_ERROR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FSETSIG1 = { {STR_tag}, 9, "FSET{SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FSETSTR1 = { {STR_tag}, 9, "FSET{STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FSETdTP1 = { {STR_tag}, 9, "FSET{$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } F_LENGTH2 = { {STR_tag}, 9, ",F_LENGTH" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } IDENT_TBL1 = { {STR_tag}, 9, "IDENT_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } NAMESPACE1 = { {STR_tag}, 9, "NAMESPACE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } NULL_STAT1 = { {STR_tag}, 9, "NULL_STAT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } ONCE_MODE1 = { {STR_tag}, 9, "ONCE_MODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_boxed3 = { {STR_tag}, 9, "_boxed;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_entry2 = { {STR_tag}, 9, "_entry {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_entry3 = { {STR_tag}, 9, "_entry;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_frame5 = { {STR_tag}, 9, "_frame);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_frame6 = { {STR_tag}, 9, "_frame;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_iframe1 = { {STR_tag}, 9, "_iframe;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_iframe2 = { {STR_tag}, 9, "_iframe,\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_struct4 = { {STR_tag}, 9, "_struct *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_struct5 = { {STR_tag}, 9, "_struct {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } TP_LAYOUT1 = { {STR_tag}, 9, "TP_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } arr_part4 = { {STR_tag}, 9, "arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } dOTHER = { {STR_tag}, 9, " $(OTHER)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } define1 = { {STR_tag}, 9, "\n#define " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } include5 = { {STR_tag}, 9, "#include " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } int6 = { {STR_tag}, 9, "(int)&(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } int7 = { {STR_tag}, 9, ",(int)(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } iter4 = { {STR_tag}, 9, " (*iter)(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } seconds1 = { {STR_tag}, 9, " seconds)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } sizeof2 = { {STR_tag}, 9, "  sizeof(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AM_AT_EXPR1 = { {STR_tag}, 10, "AM_AT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AM_IF_EXPR1 = { {STR_tag}, 10, "AM_IF_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AM_IF_STMT1 = { {STR_tag}, 10, "AM_IF_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARG_LAYOUT1 = { {STR_tag}, 10, "ARG_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYARG1 = { {STR_tag}, 10, "ARRAY{ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYIFC1 = { {STR_tag}, 10, "ARRAY{IFC}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYINT1 = { {STR_tag}, 10, "ARRAY{INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYSIG1 = { {STR_tag}, 10, "ARRAY{SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYSTR1 = { {STR_tag}, 10, "ARRAY{STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ARRAYdTP1 = { {STR_tag}, 10, "ARRAY{$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AS_ARG_DEC1 = { {STR_tag}, 10, "AS_ARG_DEC" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AS_AT_EXPR1 = { {STR_tag}, 10, "AS_AT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AS_IF_STMT1 = { {STR_tag}, 10, "AS_IF_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } AS_OR_EXPR1 = { {STR_tag}, 10, "AS_OR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } CC_OPTIONS = { {STR_tag}, 10, "CC_OPTIONS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } CONFIG_DEF1 = { {STR_tag}, 10, "CONFIG_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } CONFIG_TBL1 = { {STR_tag}, 10, "CONFIG_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } CS_OPTIONS1 = { {STR_tag}, 10, "CS_OPTIONS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } Checksig = { {STR_tag}, 10, "Check sig " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FIND_TYPES1 = { {STR_tag}, 10, "FIND_TYPES" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTARG1 = { {STR_tag}, 10, "FLIST{ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTIFC1 = { {STR_tag}, 10, "FLIST{IFC}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTINT1 = { {STR_tag}, 10, "FLIST{INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTSIG1 = { {STR_tag}, 10, "FLIST{SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTSTR1 = { {STR_tag}, 10, "FLIST{STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } FLISTdTP1 = { {STR_tag}, 10, "FLIST{$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } GLOBAL_TBL1 = { {STR_tag}, 10, "GLOBAL_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } INOUT_MODE1 = { {STR_tag}, 10, "INOUT_MODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } Including1 = { {STR_tag}, 10, "Including " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } LAYOUT_ARR1 = { {STR_tag}, 10, "LAYOUT_ARR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } NULL8 = { {STR_tag}, 10, "  NULL };\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } NULL9 = { {STR_tag}, 10, "  NULL\n};\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S0asize = { {STR_tag}, 10, ")0).asize," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } SE_CONTEXT1 = { {STR_tag}, 10, "SE_CONTEXT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } STR_CURSOR1 = { {STR_tag}, 10, "STR_CURSOR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } SYSTEM_LEX1 = { {STR_tag}, 10, "SYSTEM_LEX" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_struct3 = { {STR_tag}, 10, "_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } TP_CONTEXT1 = { {STR_tag}, 10, "TP_CONTEXT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } TP_GEN_TBL1 = { {STR_tag}, 10, "TP_GEN_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } WEIGH_CODE1 = { {STR_tag}, 10, "WEIGH_CODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } XFORM_CODE1 = { {STR_tag}, 10, "XFORM_CODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } arr_part6 = { {STR_tag}, 10, " arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } bound_arg2 = { {STR_tag}, 10, " bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } files1 = { {STR_tag}, 10, " files... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } givingup = { {STR_tag}, 10, "giving up." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } int4 = { {STR_tag}, 10, "  (int)(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } int5 = { {STR_tag}, 10, ",(int)&(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ret_arg2 = { {STR_tag}, 10, " ret_arg;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ret_val411 = { {STR_tag}, 10, " ret_val;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } seconds2 = { {STR_tag}, 10, " seconds)\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_ANY_EXPR1 = { {STR_tag}, 11, "AM_ANY_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_CALL_ARG1 = { {STR_tag}, 11, "AM_CALL_ARG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_FAR_EXPR1 = { {STR_tag}, 11, "AM_FAR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_NEW_EXPR1 = { {STR_tag}, 11, "AM_NEW_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_PAR_STMT1 = { {STR_tag}, 11, "AM_PAR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AM_ROUT_DEF1 = { {STR_tag}, 11, "AM_ROUT_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } ARRAYBOOL1 = { {STR_tag}, 11, "ARRAY{BOOL}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } ARRAYCHAR1 = { {STR_tag}, 11, "ARRAY{CHAR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } ARRAYFSTR1 = { {STR_tag}, 11, "ARRAY{FSTR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_AND_EXPR1 = { {STR_tag}, 11, "AS_AND_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_ANY_EXPR1 = { {STR_tag}, 11, "AS_ANY_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_ARG_MODE1 = { {STR_tag}, 11, "AS_ARG_MODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_ATTR_DEF1 = { {STR_tag}, 11, "AS_ATTR_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_DEC_STMT1 = { {STR_tag}, 11, "AS_DEC_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_FAR_EXPR1 = { {STR_tag}, 11, "AS_FAR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_FEAT_MOD1 = { {STR_tag}, 11, "AS_FEAT_MOD" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_NEW_EXPR1 = { {STR_tag}, 11, "AS_NEW_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_PAR_STMT1 = { {STR_tag}, 11, "AS_PAR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } AS_ROUT_DEF1 = { {STR_tag}, 11, "AS_ROUT_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } CHANGE_VARS1 = { {STR_tag}, 11, "CHANGE_VARS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } CONFIG_ROUT1 = { {STR_tag}, 11, "CONFIG_ROUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } EXEC_SUFFIX = { {STR_tag}, 11, "EXEC_SUFFIX" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } FLISTBOOL1 = { {STR_tag}, 11, "FLIST{BOOL}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } FLISTCHAR1 = { {STR_tag}, 11, "FLIST{CHAR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } FLISTFSTR1 = { {STR_tag}, 11, "FLIST{FSTR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } GENERATE_AM1 = { {STR_tag}, 11, "GENERATE_AM" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } IMPL_CREATE1 = { {STR_tag}, 11, "IMPL_CREATE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } INLINE_ITER1 = { {STR_tag}, 11, "INLINE_ITER" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } INLINE_ROUT1 = { {STR_tag}, 11, "INLINE_ROUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } PROG_AS_TBL1 = { {STR_tag}, 11, "PROG_AS_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } SIDE_EFFECT1 = { {STR_tag}, 11, "SIDE_EFFECT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } S_iter_frame = { {STR_tag}, 11, "_iter_frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } S_iter_ob3 = { {STR_tag}, 11, "_iter_ob;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } TP_ITER_TBL1 = { {STR_tag}, 11, "TP_ITER_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } TP_ROUT_TBL1 = { {STR_tag}, 11, "TP_ROUT_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } sizeof3 = { {STR_tag}, 11, "  sizeof(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } staticint = { {STR_tag}, 11, "static int " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } undefFF = { {STR_tag}, 11, "\n#undef FF\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } voidex = { {STR_tag}, 11, " void *ex;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_ARR_CONST1 = { {STR_tag}, 12, "AM_ARR_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_ATTR_EXPR1 = { {STR_tag}, 12, "AM_ATTR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_CASE_STMT1 = { {STR_tag}, 12, "AM_CASE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_EXPR_STMT1 = { {STR_tag}, 12, "AM_EXPR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_FLT_CONST1 = { {STR_tag}, 12, "AM_FLT_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_FORK_STMT1 = { {STR_tag}, 12, "AM_FORK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_HERE_EXPR1 = { {STR_tag}, 12, "AM_HERE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_INT_CONST1 = { {STR_tag}, 12, "AM_INT_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_LOCK_STMT1 = { {STR_tag}, 12, "AM_LOCK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_LOOP_STMT1 = { {STR_tag}, 12, "AM_LOOP_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_NEAR_EXPR1 = { {STR_tag}, 12, "AM_NEAR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_POST_STMT1 = { {STR_tag}, 12, "AM_POST_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_STMT_EXPR1 = { {STR_tag}, 12, "AM_STMT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_STR_CONST1 = { {STR_tag}, 12, "AM_STR_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AM_SYNC_STMT1 = { {STR_tag}, 12, "AM_SYNC_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } ARRAY2BOOL1 = { {STR_tag}, 12, "ARRAY2{BOOL}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } ARRAYIDENT1 = { {STR_tag}, 12, "ARRAY{IDENT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_CALL_EXPR1 = { {STR_tag}, 12, "AS_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_CASE_STMT1 = { {STR_tag}, 12, "AS_CASE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_CASE_WHEN1 = { {STR_tag}, 12, "AS_CASE_WHEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_CLASS_DEF1 = { {STR_tag}, 12, "AS_CLASS_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_CONST_DEF1 = { {STR_tag}, 12, "AS_CONST_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_EXPR_STMT1 = { {STR_tag}, 12, "AS_EXPR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_FORK_STMT1 = { {STR_tag}, 12, "AS_FORK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_HERE_EXPR1 = { {STR_tag}, 12, "AS_HERE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_LOCK_STMT1 = { {STR_tag}, 12, "AS_LOCK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_LOOP_STMT1 = { {STR_tag}, 12, "AS_LOOP_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_NEAR_EXPR1 = { {STR_tag}, 12, "AS_NEAR_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_PARAM_DEC1 = { {STR_tag}, 12, "AS_PARAM_DEC" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_QUIT_STMT1 = { {STR_tag}, 12, "AS_QUIT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_SELF_EXPR1 = { {STR_tag}, 12, "AS_SELF_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_STMT_LIST1 = { {STR_tag}, 12, "AS_STMT_LIST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_SYNC_STMT1 = { {STR_tag}, 12, "AS_SYNC_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_TYPE_SPEC1 = { {STR_tag}, 12, "AS_TYPE_SPEC" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } AS_VOID_EXPR1 = { {STR_tag}, 12, "AS_VOID_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } CALL_TP_VOID1 = { {STR_tag}, 12, "CALL_TP_VOID" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } CLASS_LAYOUT1 = { {STR_tag}, 12, "CLASS_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } Considering = { {STR_tag}, 12, "Considering:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } EXPR_HOISTED1 = { {STR_tag}, 12, "EXPR_HOISTED" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } FLISTIDENT1 = { {STR_tag}, 12, "FLIST{IDENT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } FRAME_LAYOUT1 = { {STR_tag}, 12, "FRAME_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } GET_MAIN_SIG1 = { {STR_tag}, 12, "GET_MAIN_SIG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } IMPL_INCLUDE1 = { {STR_tag}, 12, "IMPL_INCLUDE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } INTstate = { {STR_tag}, 12, " INT state;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } LAYOUT_ARRAY1 = { {STR_tag}, 12, "LAYOUT_ARRAY" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } Makefailed = { {STR_tag}, 12, "Make failed." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } Regenerated = { {STR_tag}, 12, "Regenerated " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_frame_alloc = { {STR_tag}, 12, "_frame_alloc" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_ob_struct3 = { {STR_tag}, 12, "_ob_struct *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TP_CLASS_TBL1 = { {STR_tag}, 12, "TP_CLASS_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPINTINT1 = { {STR_tag}, 12, "TUP{INT,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSIGINT1 = { {STR_tag}, 12, "TUP{SIG,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSIGSIG1 = { {STR_tag}, 12, "TUP{SIG,SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSIGSTR1 = { {STR_tag}, 12, "TUP{SIG,STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSTRINT1 = { {STR_tag}, 12, "TUP{STR,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSTRSIG1 = { {STR_tag}, 12, "TUP{STR,SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPSTRSTR1 = { {STR_tag}, 12, "TUP{STR,STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPdOBSTR1 = { {STR_tag}, 12, "TUP{$OB,STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } TUPdTPINT1 = { {STR_tag}, 12, "TUP{$TP,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } int3 = { {STR_tag}, 12, "),(int)&((*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } nested_biter = { {STR_tag}, 12, "nested_biter" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_ARRAY_EXPR1 = { {STR_tag}, 13, "AM_ARRAY_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_BOOL_CONST1 = { {STR_tag}, 13, "AM_BOOL_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_BREAK_STMT1 = { {STR_tag}, 13, "AM_BREAK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_CHAR_CONST1 = { {STR_tag}, 13, "AM_CHAR_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_CURSOR_POS1 = { {STR_tag}, 13, "AM_CURSOR_POS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_FLTD_CONST1 = { {STR_tag}, 13, "AM_FLTD_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_FLTI_CONST1 = { {STR_tag}, 13, "AM_FLTI_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_FLTX_CONST1 = { {STR_tag}, 13, "AM_FLTX_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_FORMAL_ARG1 = { {STR_tag}, 13, "AM_FORMAL_ARG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_INTI_CONST1 = { {STR_tag}, 13, "AM_INTI_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_LOCAL_EXPR1 = { {STR_tag}, 13, "AM_LOCAL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_RAISE_STMT1 = { {STR_tag}, 13, "AM_RAISE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_VOID_CONST1 = { {STR_tag}, 13, "AM_VOID_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_WHERE_EXPR1 = { {STR_tag}, 13, "AM_WHERE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AM_YIELD_STMT1 = { {STR_tag}, 13, "AM_YIELD_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_ARRAY_EXPR1 = { {STR_tag}, 13, "AS_ARRAY_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_BREAK_EXPR1 = { {STR_tag}, 13, "AS_BREAK_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_IDENT_LIST1 = { {STR_tag}, 13, "AS_IDENT_LIST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_RAISE_STMT1 = { {STR_tag}, 13, "AS_RAISE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_SHARED_DEF1 = { {STR_tag}, 13, "AS_SHARED_DEF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_WHERE_EXPR1 = { {STR_tag}, 13, "AS_WHERE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } AS_YIELD_STMT1 = { {STR_tag}, 13, "AS_YIELD_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } CALL_TP_ARRAY1 = { {STR_tag}, 13, "CALL_TP_ARRAY" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } Code = { {STR_tag}, 13, "\n/* Code */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FIND_GENERICS1 = { {STR_tag}, 13, "FIND_GENERICS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FMAPSIGSTR1 = { {STR_tag}, 13, "FMAP{SIG,STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FMAPSTRINT1 = { {STR_tag}, 13, "FMAP{STR,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FMAPSTRSIG1 = { {STR_tag}, 13, "FMAP{STR,SIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FMAPSTRSTR1 = { {STR_tag}, 13, "FMAP{STR,STR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FMAPdTPINT1 = { {STR_tag}, 13, "FMAP{$TP,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FSETdLAYOUT1 = { {STR_tag}, 13, "FSET{$LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } INTstate1 = { {STR_tag}, 13, " INT state; \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } MS_RANDOM_GEN1 = { {STR_tag}, 13, "MS_RANDOM_GEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } NULLNULL1 = { {STR_tag}, 13, " {NULL, NULL}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_arg_frame = { {STR_tag}, 13, "_arg_frame;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_boxed_struct2 = { {STR_tag}, 13, "_boxed_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_frame_struct2 = { {STR_tag}, 13, "_frame_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_ob_struct2 = { {STR_tag}, 13, "_ob_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_tagreturn = { {STR_tag}, 13, "_tag: return " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_type_table = { {STR_tag}, 13, "_type_table;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_type_table1 = { {STR_tag}, 13, "_type_table,\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } int2 = { {STR_tag}, 13, "\n   (int)&(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } iter_frame = { {STR_tag}, 13, " iter_frame;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } sather_funcc = { {STR_tag}, 13, "sather_func.c" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } sather_typec = { {STR_tag}, 13, "sather_type.c" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } switchi = { {STR_tag}, 13, " switch (i) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } wasreached = { {STR_tag}, 13, " was reached\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_ASSERT_STMT1 = { {STR_tag}, 14, "AM_ASSERT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_ASSIGN_STMT1 = { {STR_tag}, 14, "AM_ASSIGN_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_ATTACH_STMT1 = { {STR_tag}, 14, "AM_ATTACH_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_EXCEPT_EXPR1 = { {STR_tag}, 14, "AM_EXCEPT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_FLTDX_CONST1 = { {STR_tag}, 14, "AM_FLTDX_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_GLOBAL_EXPR1 = { {STR_tag}, 14, "AM_GLOBAL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_RETURN_STMT1 = { {STR_tag}, 14, "AM_RETURN_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_SHARED_EXPR1 = { {STR_tag}, 14, "AM_SHARED_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AM_UNLOCK_STMT1 = { {STR_tag}, 14, "AM_UNLOCK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } ARRAYdLAYOUT1 = { {STR_tag}, 14, "ARRAY{$LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_ASSERT_STMT1 = { {STR_tag}, 14, "AS_ASSERT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_ASSIGN_STMT1 = { {STR_tag}, 14, "AS_ASSIGN_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_ATTACH_STMT1 = { {STR_tag}, 14, "AS_ATTACH_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_COHORT_EXPR1 = { {STR_tag}, 14, "AS_COHORT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_CREATE_EXPR1 = { {STR_tag}, 14, "AS_CREATE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_EXCEPT_EXPR1 = { {STR_tag}, 14, "AS_EXCEPT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_GLOBAL_EXPR1 = { {STR_tag}, 14, "AS_GLOBAL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_RESULT_EXPR1 = { {STR_tag}, 14, "AS_RESULT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_RETURN_STMT1 = { {STR_tag}, 14, "AS_RETURN_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_STMT_CURSOR1 = { {STR_tag}, 14, "AS_STMT_CURSOR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } AS_UNLOCK_STMT1 = { {STR_tag}, 14, "AS_UNLOCK_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } BUILTIN_LAYOUT1 = { {STR_tag}, 14, "BUILTIN_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } CALL_TP_CREATE1 = { {STR_tag}, 14, "CALL_TP_CREATE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } CHECK_GENERICS1 = { {STR_tag}, 14, "CHECK_GENERICS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } CONFIG_SCANNER1 = { {STR_tag}, 14, "CONFIG_SCANNER" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } CheckAMfor = { {STR_tag}, 14, "\nCheck AM for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } DPSATHER_CHK = { {STR_tag}, 14, " -DPSATHER_CHK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } FLISTdLAYOUT1 = { {STR_tag}, 14, "FLIST{$LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } FSETSFILE_ID1 = { {STR_tag}, 14, "FSET{SFILE_ID}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } FSETTP_CLASS1 = { {STR_tag}, 14, "FSET{TP_CLASS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } IFC_ABS_CREATE1 = { {STR_tag}, 14, "IFC_ABS_CREATE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } OPT_LOCAL_CALL1 = { {STR_tag}, 14, "OPT_LOCAL_CALL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } OPT_LOCAL_EXPR1 = { {STR_tag}, 14, "OPT_LOCAL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } S_ref_offsets1 = { {STR_tag}, 14, "_ref_offsets,\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } TUPIDENTINT1 = { {STR_tag}, 14, "TUP{IDENT,INT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } TUPIDENTdTP1 = { {STR_tag}, 14, "TUP{IDENT,$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } defineF_tag_ = { {STR_tag}, 14, "#define F_tag_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } layoutfor = { {STR_tag}, 14, "/* layout for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } size_tsize = { {STR_tag}, 14, " size_t size;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } switcht1 = { {STR_tag}, 14, " switch (t1) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ABSTRACT_LAYOUT1 = { {STR_tag}, 15, "ABSTRACT_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_CLUSTER_EXPR1 = { {STR_tag}, 15, "AM_CLUSTER_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_COMMENT_STMT1 = { {STR_tag}, 15, "AM_COMMENT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_INITIAL_STMT1 = { {STR_tag}, 15, "AM_INITIAL_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_IS_VOID_EXPR1 = { {STR_tag}, 15, "AM_IS_VOID_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_PROTECT_STMT1 = { {STR_tag}, 15, "AM_PROTECT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AM_WAITFOR_STMT1 = { {STR_tag}, 15, "AM_WAITFOR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYATTRSORT1 = { {STR_tag}, 15, "ARRAY{ATTRSORT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYCALL_ARG1 = { {STR_tag}, 15, "ARRAY{CALL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYSFILE_ID1 = { {STR_tag}, 15, "ARRAY{SFILE_ID}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYTP_CLASS1 = { {STR_tag}, 15, "ARRAY{TP_CLASS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYdAM_EXPR1 = { {STR_tag}, 15, "ARRAY{$AM_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ARRAYdAM_STMT1 = { {STR_tag}, 15, "ARRAY{$AM_STMT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_CLUSTER_EXPR1 = { {STR_tag}, 15, "AS_CLUSTER_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_FLT_LIT_EXPR1 = { {STR_tag}, 15, "AS_FLT_LIT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_INITIAL_EXPR1 = { {STR_tag}, 15, "AS_INITIAL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_INT_LIT_EXPR1 = { {STR_tag}, 15, "AS_INT_LIT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_IS_VOID_EXPR1 = { {STR_tag}, 15, "AS_IS_VOID_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_LOCK_IF_WHEN1 = { {STR_tag}, 15, "AS_LOCK_IF_WHEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_PROTECT_STMT1 = { {STR_tag}, 15, "AS_PROTECT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_PROTECT_WHEN1 = { {STR_tag}, 15, "AS_PROTECT_WHEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AS_STR_LIT_EXPR1 = { {STR_tag}, 15, "AS_STR_LIT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } CODE_FILE_ARRAY1 = { {STR_tag}, 15, "CODE_FILE_ARRAY" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } Couldntwrite = { {STR_tag}, 15, "Couldn\'t write " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } EXTERNAL_LAYOUT1 = { {STR_tag}, 15, "EXTERNAL_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTATTRSORT1 = { {STR_tag}, 15, "FLIST{ATTRSORT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTCALL_ARG1 = { {STR_tag}, 15, "FLIST{CALL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTSFILE_ID1 = { {STR_tag}, 15, "FLIST{SFILE_ID}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTTP_CLASS1 = { {STR_tag}, 15, "FLIST{TP_CLASS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTdAM_EXPR1 = { {STR_tag}, 15, "FLIST{$AM_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FLISTdAM_STMT1 = { {STR_tag}, 15, "FLIST{$AM_STMT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } FMAPIDENTdTP1 = { {STR_tag}, 15, "FMAP{IDENT,$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } INLINE_INT_FOLD1 = { {STR_tag}, 15, "INLINE_INT_FOLD" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } INLINE_ITER_TBL1 = { {STR_tag}, 15, "INLINE_ITER_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } INLINE_ROUT_TBL1 = { {STR_tag}, 15, "INLINE_ROUT_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } Layouts = { {STR_tag}, 15, "/* Layouts */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } RETURNED_CONST1 = { {STR_tag}, 15, "RETURNED_CONST " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } SATHER_COMMANDS = { {STR_tag}, 15, "SATHER_COMMANDS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } SATHER_PLATFORM = { {STR_tag}, 15, "SATHER_PLATFORM" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } S_frame_struct3 = { {STR_tag}, 15, "_frame_struct *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } dCSdOBJ = { {STR_tag}, 15, "../$(CS) $(OBJ)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } returnr = { {STR_tag}, 15, "     return r&&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } typedefstruct = { {STR_tag}, 15, "typedef struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } voidexpression = { {STR_tag}, 15, "void-expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AM_EXT_CALL_EXPR1 = { {STR_tag}, 16, "AM_EXT_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AM_PREFETCH_STMT1 = { {STR_tag}, 16, "AM_PREFETCH_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AM_TYPECASE_STMT1 = { {STR_tag}, 16, "AM_TYPECASE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } ARRAYAM_CURSOR1 = { {STR_tag}, 16, "ARRAY{AM_CURSOR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } ARRAYCODE_FILE1 = { {STR_tag}, 16, "ARRAY{CODE_FILE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } ARRAYTP_LAYOUT1 = { {STR_tag}, 16, "ARRAY{TP_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } ARRAYdAM_CONST1 = { {STR_tag}, 16, "ARRAY{$AM_CONST}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AS_BOOL_LIT_EXPR1 = { {STR_tag}, 16, "AS_BOOL_LIT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AS_CHAR_LIT_EXPR1 = { {STR_tag}, 16, "AS_CHAR_LIT_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AS_TYPECASE_STMT1 = { {STR_tag}, 16, "AS_TYPECASE_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AS_TYPECASE_WHEN1 = { {STR_tag}, 16, "AS_TYPECASE_WHEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } BUILD_TYPE_GRAPH1 = { {STR_tag}, 16, "BUILD_TYPE_GRAPH" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } CSE_DOUBLE_EXPRS1 = { {STR_tag}, 16, "CSE_DOUBLE_EXPRS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Classesfound = { {STR_tag}, 16, " Classes found:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Currentlength = { {STR_tag}, 16, " Current length=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } FLISTAM_CURSOR1 = { {STR_tag}, 16, "FLIST{AM_CURSOR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } FLISTCODE_FILE1 = { {STR_tag}, 16, "FLIST{CODE_FILE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } FLISTTP_LAYOUT1 = { {STR_tag}, 16, "FLIST{TP_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } FLISTdAM_CONST1 = { {STR_tag}, 16, "FLIST{$AM_CONST}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Globals = { {STR_tag}, 16, "\n/* Globals */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Homedirectory1 = { {STR_tag}, 16, "Home directory: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } INLINE_ATTR_READ1 = { {STR_tag}, 16, "INLINE_ATTR_READ" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Initializing = { {STR_tag}, 16, "Initializing... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Maketargetinc = { {STR_tag}, 16, "/Make.target.inc" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } RETURNED_CONST2 = { {STR_tag}, 16, "(RETURNED_CONST " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } S0arr_part0 = { {STR_tag}, 16, ")0).arr_part,0 }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } S_boxed_struct1 = { {STR_tag}, 16, "_boxed_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } S_frame_alloc1 = { {STR_tag}, 16, "_frame_alloc();\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } S_frame_struct1 = { {STR_tag}, 16, "_frame_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } S_type_table2 = { {STR_tag}, 16, "_type_table = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } Skippingmake = { {STR_tag}, 16, "\nSkipping make.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } TP_GRAPH_ABS_DES1 = { {STR_tag}, 16, "TP_GRAPH_ABS_DES" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } TUPSTRdCONFIG1 = { {STR_tag}, 16, "TUP{STR,$CONFIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } TUPdTPdLAYOUT1 = { {STR_tag}, 16, "TUP{$TP,$LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } arrayexpression1 = { {STR_tag}, 16, "array-expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } callstruct = { {STR_tag}, 16, " (*call)(struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } dCCdCFLAGS = { {STR_tag}, 16, "$(CC) $(CFLAGS) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } definePRE_CHK = { {STR_tag}, 16, "#define PRE_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } returnOBALLOC = { {STR_tag}, 16, " return OBALLOC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AM_INV11678372301 = { {STR_tag}, 17, "AM_INVARIANT_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AM_ITE18091358501 = { {STR_tag}, 17, "AM_ITER_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AM_ROU19160462901 = { {STR_tag}, 17, "AM_ROUT_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AM_WIT19969716031 = { {STR_tag}, 17, "AM_WITH_NEAR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } ARRAYARRAYSTR1 = { {STR_tag}, 17, "ARRAY{ARRAY{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } ARRAYFLISTSTR1 = { {STR_tag}, 17, "ARRAY{FLIST{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AS_INC14169170011 = { {STR_tag}, 17, "AS_INCLUDE_CLAUSE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AS_UND1529866141 = { {STR_tag}, 17, "AS_UNDERSCORE_ARG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AS_WIT10634373511 = { {STR_tag}, 17, "AS_WITH_NEAR_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } BOUND_8093907741 = { {STR_tag}, 17, "BOUND_TYPE_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } DPSATHER_TRACE = { {STR_tag}, 17, " -DPSATHER_TRACE\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FLISTARRAYSTR1 = { {STR_tag}, 17, "FLIST{ARRAY{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FLISTFLISTSTR1 = { {STR_tag}, 17, "FLIST{FLIST{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FMAPSTRdCONFIG1 = { {STR_tag}, 17, "FMAP{STR,$CONFIG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FMAPdTPdLAYOUT1 = { {STR_tag}, 17, "FMAP{$TP,$LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FSETAM_ROUT_DEF1 = { {STR_tag}, 17, "FSET{AM_ROUT_DEF}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FSETAS_FEAT_MOD1 = { {STR_tag}, 17, "FSET{AS_FEAT_MOD}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } FSETSIDE_EFFECT1 = { {STR_tag}, 17, "FSET{SIDE_EFFECT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } INLINE15963118111 = { {STR_tag}, 17, "INLINE_ATTR_WRITE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } S_iter_ob_struct3 = { {STR_tag}, 17, "_iter_ob_struct *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } S_ref_offsets = { {STR_tag}, 17, "_ref_offsets[]= {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } S_structarr_part1 = { {STR_tag}, 17, "_struct arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } TUPdAM_EXPRdTP1 = { {STR_tag}, 17, "TUP{$AM_EXPR,$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } createexpression1 = { {STR_tag}, 17, "create-expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } definePOST_CHK = { {STR_tag}, 17, "#define POST_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } defineVOID_CHK = { {STR_tag}, 17, "#define VOID_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } defineWHEN_CHK = { {STR_tag}, 17, "#define WHEN_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } immutable_part5 = { {STR_tag}, 17, " immutable_part;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } AM_BND11248771631 = { {STR_tag}, 18, "AM_BND_CREATE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } ARRAYAM_CALL_ARG1 = { {STR_tag}, 18, "ARRAY{AM_CALL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } ARRAYAM_ROUT_DEF1 = { {STR_tag}, 18, "ARRAY{AM_ROUT_DEF}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } ARRAYARRAYBOOL1 = { {STR_tag}, 18, "ARRAY{ARRAY{BOOL}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } ARRAYAS_FEAT_MOD1 = { {STR_tag}, 18, "ARRAY{AS_FEAT_MOD}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } ARRAYSIDE_EFFECT1 = { {STR_tag}, 18, "ARRAY{SIDE_EFFECT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } CALL_T6663890791 = { {STR_tag}, 18, "CALL_TP_UNDERSCORE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Defaultplatform = { {STR_tag}, 18, "Default platform: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FLISTAM_CALL_ARG1 = { {STR_tag}, 18, "FLIST{AM_CALL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FLISTAM_ROUT_DEF1 = { {STR_tag}, 18, "FLIST{AM_ROUT_DEF}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FLISTARRAYBOOL1 = { {STR_tag}, 18, "FLIST{ARRAY{BOOL}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FLISTAS_FEAT_MOD1 = { {STR_tag}, 18, "FLIST{AS_FEAT_MOD}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FLISTSIDE_EFFECT1 = { {STR_tag}, 18, "FLIST{SIDE_EFFECT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FSETCLASS_LAYOUT1 = { {STR_tag}, 18, "FSET{CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } FSETTUPSIGSIG1 = { {STR_tag}, 18, "FSET{TUP{SIG,SIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } F_V_RATTR_LP2 = { {STR_tag}, 18, "     F_V_RATTR_LP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } INLINE11649048701 = { {STR_tag}, 18, "INLINE_VATTR_WRITE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } INLINE14336689011 = { {STR_tag}, 18, "INLINE_GLOBAL_READ" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } S_iter2041871633 = { {STR_tag}, 18, "_iter_frame_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } S_iter_ob_struct2 = { {STR_tag}, 18, "_iter_ob_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } S_structarr_part = { {STR_tag}, 18, "_struct *arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } TUPSTRFSETSTR1 = { {STR_tag}, 18, "TUP{STR,FSET{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } TUPdOBNAMESPACE1 = { {STR_tag}, 18, "TUP{$OB,NAMESPACE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } TUPdTPCODE_FILE1 = { {STR_tag}, 18, "TUP{$TP,CODE_FILE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } defineARITH_CHK = { {STR_tag}, 18, "#define ARITH_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } funcptrstruct = { {STR_tag}, 18, "(*funcptr)(struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } funcptrvoid = { {STR_tag}, 18, " (*funcptr)(void *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } includetagsh = { {STR_tag}, 18, "#include \"tags.h\"\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } AM_VAR7444700971 = { {STR_tag}, 19, "AM_VARR_ASSIGN_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYC14386558381 = { {STR_tag}, 19, "ARRAY{CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYE21264085331 = { {STR_tag}, 19, "ARRAY{EXPR_HOISTED}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYI19919549901 = { {STR_tag}, 19, "ARRAY{IMPL_INCLUDE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSIGINT1 = { {STR_tag}, 19, "ARRAY{TUP{SIG,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSIGSIG1 = { {STR_tag}, 19, "ARRAY{TUP{SIG,SIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSIGSTR1 = { {STR_tag}, 19, "ARRAY{TUP{SIG,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSTRINT1 = { {STR_tag}, 19, "ARRAY{TUP{STR,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSTRSIG1 = { {STR_tag}, 19, "ARRAY{TUP{STR,SIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPSTRSTR1 = { {STR_tag}, 19, "ARRAY{TUP{STR,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPdOBSTR1 = { {STR_tag}, 19, "ARRAY{TUP{$OB,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ARRAYTUPdTPINT1 = { {STR_tag}, 19, "ARRAY{TUP{$TP,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } BOUND_16778158021 = { {STR_tag}, 19, "BOUND_OBJECT_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTC10313092711 = { {STR_tag}, 19, "FLIST{CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTE17612121961 = { {STR_tag}, 19, "FLIST{EXPR_HOISTED}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTI18956657391 = { {STR_tag}, 19, "FLIST{IMPL_INCLUDE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSIGINT1 = { {STR_tag}, 19, "FLIST{TUP{SIG,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSIGSIG1 = { {STR_tag}, 19, "FLIST{TUP{SIG,SIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSIGSTR1 = { {STR_tag}, 19, "FLIST{TUP{SIG,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSTRINT1 = { {STR_tag}, 19, "FLIST{TUP{STR,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSTRSIG1 = { {STR_tag}, 19, "FLIST{TUP{STR,SIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPSTRSTR1 = { {STR_tag}, 19, "FLIST{TUP{STR,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPdOBSTR1 = { {STR_tag}, 19, "FLIST{TUP{$OB,STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FLISTTUPdTPINT1 = { {STR_tag}, 19, "FLIST{TUP{$TP,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FMAPSTRFSETSTR1 = { {STR_tag}, 19, "FMAP{STR,FSET{STR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FMAPdOBNAMESPACE1 = { {STR_tag}, 19, "FMAP{$OB,NAMESPACE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FMAPdTPCODE_FILE1 = { {STR_tag}, 19, "FMAP{$TP,CODE_FILE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } FSETAM11584059841 = { {STR_tag}, 19, "FSET{AM_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } INLINE14557683741 = { {STR_tag}, 19, "INLINE_GLOBAL_WRITE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } S_fram362955677 = { {STR_tag}, 19, "_frame iter_frame;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } S_ob_structlocal = { {STR_tag}, 19, "_ob_struct *local;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } TUPARRAYARGdTP1 = { {STR_tag}, 19, "TUP{ARRAY{ARG},$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } TUPSIGARG_LAYOUT1 = { {STR_tag}, 19, "TUP{SIG,ARG_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } constvoid = { {STR_tag}, 19, "(const void* (*)())" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } defineASSERT_CHK = { {STR_tag}, 19, "#define ASSERT_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } defineBOUNDS_CHK = { {STR_tag}, 19, "#define BOUNDS_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } defineRETURN_CHK = { {STR_tag}, 19, "#define RETURN_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AM_CLU12862693351 = { {STR_tag}, 20, "AM_CLUSTER_SIZE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AM_VAT3199825281 = { {STR_tag}, 20, "AM_VATTR_ASSIGN_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } ARRAYA13472925431 = { {STR_tag}, 20, "ARRAY{AM_FORMAL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } ARRAYA15756328481 = { {STR_tag}, 20, "ARRAY{AM_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } ARRAYA362808861 = { {STR_tag}, 20, "ARRAY{AM_CURSOR_POS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AS_BOU142693361 = { {STR_tag}, 20, "AS_BOUND_CREATE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AS_CLU5535705991 = { {STR_tag}, 20, "AS_CLUSTER_SIZE_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } CALL_T5617923671 = { {STR_tag}, 20, "CALL_TP_BOUND_CREATE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FLISTA20973249341 = { {STR_tag}, 20, "FLIST{AM_FORMAL_ARG}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FLISTA7252830291 = { {STR_tag}, 20, "FLIST{AM_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FLISTA8866307051 = { {STR_tag}, 20, "FLIST{AM_CURSOR_POS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FMAPSI13499887021 = { {STR_tag}, 20, "FMAP{SIG,ARG_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FSETAM15426789471 = { {STR_tag}, 20, "FSET{AM_SHARED_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FSETBU14464775051 = { {STR_tag}, 20, "FSET{BUILTIN_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } FSETTUPIDENTINT1 = { {STR_tag}, 20, "FSET{TUP{IDENT,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Formalargument = { {STR_tag}, 20, "/* Formal argument: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Heresonecycle = { {STR_tag}, 20, "Here\'s one cycle: \n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } LOCK_H1410610176 = { {STR_tag}, 20, " LOCK_HEADER_STRUCT\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } S_arg_1067136973 = { {STR_tag}, 20, "_arg_frame_struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } TUPSIG5786921891 = { {STR_tag}, 20, "TUP{SIG,AM_ROUT_DEF}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Unboxdecls = { {STR_tag}, 20, "\n/* Unbox decls */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } abort2 = { {STR_tag}, 20, "           abort();\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } v1v2F_V_RATTR_LP = { {STR_tag}, 20, " v1,v2;F_V_RATTR_LP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ABSTRA1514981551 = { {STR_tag}, 21, "ABSTRACT_FRAME_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } AM_BND2603013291 = { {STR_tag}, 21, "AM_BND_ROUT_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } AM_BND3672117691 = { {STR_tag}, 21, "AM_BND_ITER_CALL_EXPR" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ARRAYA11355487101 = { {STR_tag}, 21, "ARRAY{AM_SHARED_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ARRAYB10393472681 = { {STR_tag}, 21, "ARRAY{BUILTIN_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ARRAYO5705941271 = { {STR_tag}, 21, "ARRAY{OPT_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ARRAYTUPIDENTINT1 = { {STR_tag}, 21, "ARRAY{TUP{IDENT,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ARRAYTUPIDENTdTP1 = { {STR_tag}, 21, "ARRAY{TUP{IDENT,$TP}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } AS_INT20312392681 = { {STR_tag}, 21, "AS_INTERF_ATTACH_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } CHECK_11745520751 = { {STR_tag}, 21, "CHECK_IFC_CONFORMANCE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } DPSATH545432352 = { {STR_tag}, 21, " -DPSATHER_STATISTICS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FLISTA1123260871 = { {STR_tag}, 21, "FLIST{AM_SHARED_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FLISTB161246451 = { {STR_tag}, 21, "FLIST{BUILTIN_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FLISTO15938167501 = { {STR_tag}, 21, "FLIST{OPT_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FLISTTUPIDENTINT1 = { {STR_tag}, 21, "FLIST{TUP{IDENT,INT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FLISTTUPIDENTdTP1 = { {STR_tag}, 21, "FLIST{TUP{IDENT,$TP}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FMAPSI5181626691 = { {STR_tag}, 21, "FMAP{SIG,AM_ROUT_DEF}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FSETAB15696988081 = { {STR_tag}, 21, "FSET{ABSTRACT_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FSETEX12609473711 = { {STR_tag}, 21, "FSET{EXTERNAL_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } INTt1t2BOOLr1 = { {STR_tag}, 21, " INT t1,t2;BOOL r=1;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } Instal1200957049 = { {STR_tag}, 21, "Installed platforms:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } System1287683407 = { {STR_tag}, 21, "/System/Common/CONFIG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } TUPIDENTARRAYdTP1 = { {STR_tag}, 21, "TUP{IDENT,ARRAY{$TP}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } TUPSIG20235332471 = { {STR_tag}, 21, "TUP{SIG,FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } define2029181961 = { {STR_tag}, 21, "\n#define NUM_OF_ATTR " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } includesatherh = { {STR_tag}, 21, "#include \"sather.h\"\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } ARRAYA18708083801 = { {STR_tag}, 22, "ARRAY{ABSTRACT_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } ARRAYA4772510191 = { {STR_tag}, 22, "ARRAY{ARRAY{$AM_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } ARRAYE4064872631 = { {STR_tag}, 22, "ARRAY{EXTERNAL_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } A_STAC12449660981 = { {STR_tag}, 22, "A_STACK{AM_CURSOR_POS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } BOUND_21403731241 = { {STR_tag}, 22, "BOUND_ITER_TYPE_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } FLISTA13276008381 = { {STR_tag}, 22, "FLIST{ARRAY{$AM_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } FLISTA1627982631 = { {STR_tag}, 22, "FLIST{ABSTRACT_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } FLISTE16271193801 = { {STR_tag}, 22, "FLIST{EXTERNAL_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } FMAPSI11213731881 = { {STR_tag}, 22, "FMAP{SIG,FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } FSETCS15614182841 = { {STR_tag}, 22, "FSET{CSE_DOUBLE_EXPRS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } F_VA_RATTR_NAv2 = { {STR_tag}, 22, "     F_VA_RATTR_NA(v2," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } HotsItersloops = { {STR_tag}, 22, "\n\nHots:\tIters\t# loops\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } IMMUTA4217598931 = { {STR_tag}, 22, "IMMUTABLE_CLASS_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } ItersItersloops = { {STR_tag}, 22, "\nIters:\tIters\t# loops\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } define1369334899 = { {STR_tag}, 22, "#define INVARIANT_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } ARRAYC17884218991 = { {STR_tag}, 23, "ARRAY{CSE_DOUBLE_EXPRS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } ARRAYF10308174421 = { {STR_tag}, 23, "ARRAY{FLIST{$AM_CONST}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } ARRAYT14182091731 = { {STR_tag}, 23, "ARRAY{TUP{$TP,$LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } ARRAYT611121321 = { {STR_tag}, 23, "ARRAY{TUP{STR,$CONFIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } BOUND_10074073311 = { {STR_tag}, 23, "BOUND_ITER_FRAME_LAYOUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Compil1872856259 = { {STR_tag}, 23, "\nCompilation complete.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Externalcalls = { {STR_tag}, 23, "\n/* External calls */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } FLISTC17722424521 = { {STR_tag}, 23, "FLIST{CSE_DOUBLE_EXPRS}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } FLISTF75948191 = { {STR_tag}, 23, "FLIST{FLIST{$AM_CONST}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } FLISTT5678593541 = { {STR_tag}, 23, "FLIST{TUP{$TP,$LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } FLISTT7892376871 = { {STR_tag}, 23, "FLIST{TUP{STR,$CONFIG}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } FSETBO11479317841 = { {STR_tag}, 23, "FSET{BOUND_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } TUPBOO13929232411 = { {STR_tag}, 23, "TUP{BOOL,INTI,INT,BOOL}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } TUPTP_8583212671 = { {STR_tag}, 23, "TUP{TP_CLASS,FSET{$TP}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } boundc12314396721 = { {STR_tag}, 23, "bound-create-expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } ARRAYA13697444921 = { {STR_tag}, 24, "ARRAY{AM_ITER_CALL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } ARRAYB19254777281 = { {STR_tag}, 24, "ARRAY{BOUND_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } FLISTA10623349991 = { {STR_tag}, 24, "FLIST{AM_ITER_CALL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } FLISTB20620800751 = { {STR_tag}, 24, "FLIST{BOUND_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } FMAPTP2917395941 = { {STR_tag}, 24, "FMAP{TP_CLASS,FSET{$TP}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } FSETAM10327783151 = { {STR_tag}, 24, "FSET{AM_BND_CREATE_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } HDRsatherhtagsh = { {STR_tag}, 24, "HDR    =sather.h tags.h\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } LOCK_H1013548551 = { {STR_tag}, 24, "LOCK_HEADER_STRUCT_REFS " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } defaul1217511725 = { {STR_tag}, 24, "  default: return FALSE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } v1v2F_1821257885 = { {STR_tag}, 24, " v1,v2;F_VA_RATTR_NA(v1," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ARRAYA8556927141 = { {STR_tag}, 25, "ARRAY{AM_BND_CREATE_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ARRAYT11531010581 = { {STR_tag}, 25, "ARRAY{TUP{$TP,CODE_FILE}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ARRAYT3786121471 = { {STR_tag}, 25, "ARRAY{TUP{STR,FSET{STR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ARRAYT7475723801 = { {STR_tag}, 25, "ARRAY{TUP{$OB,NAMESPACE}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } FLISTA18934615111 = { {STR_tag}, 25, "FLIST{AM_BND_CREATE_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } FLISTT15137882731 = { {STR_tag}, 25, "FLIST{TUP{$OB,NAMESPACE}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } FLISTT4717376721 = { {STR_tag}, 25, "FLIST{TUP{STR,FSET{STR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } FLISTT8805055851 = { {STR_tag}, 25, "FLIST{TUP{$TP,CODE_FILE}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Header164288235 = { {STR_tag}, 25, "\nHeaders, compiling C... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } TUPIDE6416541241 = { {STR_tag}, 25, "TUP{IDENT,AM_SHARED_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } TUPdAS2686797791 = { {STR_tag}, 25, "TUP{$AS_EXPR,AS_ARG_MODE}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } arr_part5 = { {STR_tag}, 25, ",\n   { \"[]\",\"arr_part[]\"," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } charsource_files = { {STR_tag}, 25, "char *source_files[] = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ifo1o2returnTRUE = { {STR_tag}, 25, " if (o1==o2) return TRUE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } typein400820632 = { {STR_tag}, 25, "\n/* type information for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } AREFFL16982338171 = { {STR_tag}, 26, "AREF{FLIST{AM_LOCAL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } ARRAYT17591683841 = { {STR_tag}, 26, "ARRAY{TUP{SIG,ARG_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } CHARfi1162829196 = { {STR_tag}, 26, " CHAR field_so_not_empty;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Checki135017201 = { {STR_tag}, 26, "\nChecking generic code... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Couldn1439942807 = { {STR_tag}, 26, "Couldn\'t write thumbprints" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } FLISTT17429889371 = { {STR_tag}, 26, "FLIST{TUP{SIG,ARG_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } FMAPID7390865191 = { {STR_tag}, 26, "FMAP{IDENT,AM_SHARED_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } S_boxe1015469271 = { {STR_tag}, 26, "_boxed,o1,immutable_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } S_boxe2059544060 = { {STR_tag}, 26, "_boxed,o2,immutable_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } ift1bt496219238 = { {STR_tag}, 26, " if (t1!=t2) return FALSE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } nestediterframe = { {STR_tag}, 26, "; /* nested iter frame */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } ARRAYT5373947651 = { {STR_tag}, 27, "ARRAY{TUP{SIG,AM_ROUT_DEF}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } FLISTT8448042581 = { {STR_tag}, 27, "FLIST{TUP{SIG,AM_ROUT_DEF}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } PSATHER_ABORT2 = { {STR_tag}, 27, "            PSATHER_ABORT;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } S_boxe10154692711 = { {STR_tag}, 27, "_boxed)o1)->immutable_part;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } S_boxe20595440601 = { {STR_tag}, 27, "_boxed)o2)->immutable_part;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } nestedbiterframe = { {STR_tag}, 27, "; /* nested biter frame */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } ARRAYA1325536251 = { {STR_tag}, 28, "ARRAY{AM_BND_ITER_CALL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } ARRAYT17543487981 = { {STR_tag}, 28, "ARRAY{TUP{SIG,FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } Dispat1213124981 = { {STR_tag}, 28, "/* Dispatch table entry for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } FLISTA14098462101 = { {STR_tag}, 28, "FLIST{AM_BND_ITER_CALL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } FLISTT9948054271 = { {STR_tag}, 28, "FLIST{TUP{SIG,FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } FSETBO15483973181 = { {STR_tag}, 28, "FSET{BOUND_ITER_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } FSETIM1844369611 = { {STR_tag}, 28, "FSET{IMMUTABLE_CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } Iterat1041410143 = { {STR_tag}, 28, "Iterator dispatch table for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } System1789613969 = { {STR_tag}, 28, "/System/Common/Common.module" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } TUPTP_16684541721 = { {STR_tag}, 28, "TUP{TP_CLASS,FSET{TP_CLASS}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } usedby2642972771 = { {STR_tag}, 28, " /* used by builtin iter */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } name191 = { {STR_tag}, 1, "/" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } w1 = { {STR_tag}, 1, "w" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } ARRAYB12347653251 = { {STR_tag}, 29, "ARRAY{BOUND_ITER_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } ARRAYI13273676921 = { {STR_tag}, 29, "ARRAY{IMMUTABLE_CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } FLISTB4755911181 = { {STR_tag}, 29, "FLIST{BOUND_ITER_TYPE_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } FLISTI20865418991 = { {STR_tag}, 29, "FLIST{IMMUTABLE_CLASS_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } FMAPTP7752976001 = { {STR_tag}, 29, "FMAP{TP_CLASS,FSET{TP_CLASS}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } Header575149500 = { {STR_tag}, 29, "\nHeaders, writing C files... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } S_boxe758056869 = { {STR_tag}, 29, "_boxed)NULL).immutable_part)," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } asizea1439666166 = { {STR_tag}, 29, "   { \"asize\",\"asize\",INT_tag," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } iter_f1092307779 = { {STR_tag}, 29, " iter_frame_stub iter_frame;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } usedby264297277 = { {STR_tag}, 29, "; /* used by builtin iter */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } ARRAYT11275057161 = { {STR_tag}, 30, "ARRAY{TUP{TP_CLASS,FSET{$TP}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Abstractcalls = { {STR_tag}, 30, "\nAbstract calls:              " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Checki974602838 = { {STR_tag}, 30, "\nChecking unreachable code... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Concretecalls = { {STR_tag}, 30, "\nConcrete calls:              " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } FLISTT4183073551 = { {STR_tag}, 30, "FLIST{TUP{TP_CLASS,FSET{$TP}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } FoldedINTcalls = { {STR_tag}, 30, "\nFolded INT calls:            " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Functi2098237195 = { {STR_tag}, 30, "\n/* Function declarations */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } ICSISa2111359522 = { {STR_tag}, 30, "ICSI Sather compiler version: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline1146142148 = { {STR_tag}, 30, "\nInlined builtin iter calls:  " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline1184875661 = { {STR_tag}, 30, "\nInlined pointer ob_eq calls: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline1346595549 = { {STR_tag}, 30, "\nInlined read attr calls:     " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline1829321774 = { {STR_tag}, 30, "\nInlined short rout calls:    " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline311198520 = { {STR_tag}, 30, "\nInlined global calls:        " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline453141559 = { {STR_tag}, 30, "\nInlined flat iter calls:     " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline469134742 = { {STR_tag}, 30, "\nInlined value ob_eq calls:   " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Inline778953810 = { {STR_tag}, 30, "\nInlined write attr calls:    " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Marked1264672611 = { {STR_tag}, 30, "\nMarked read attr routs:      " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Marked2085347308 = { {STR_tag}, 30, "\nMarked global routs:         " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Marked887950191 = { {STR_tag}, 30, "\nMarked write attr routs:     " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } MarkedINTrouts = { {STR_tag}, 30, "\nMarked INT routs:            " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Markedflatiters = { {STR_tag}, 30, "\nMarked flat iters:           " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Markedshortrouts = { {STR_tag}, 30, "\nMarked short routs:          " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Parsin1988046872 = { {STR_tag}, 30, "\nParsing non-library files... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } TUPSIG17540787361 = { {STR_tag}, 30, "TUP{SIG,ABSTRACT_FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } constv18267182 = { {STR_tag}, 30, "const void* (*alloc_frame)();\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Boundi775344439 = { {STR_tag}, 31, "\n/* Bound iter stubs decls */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Boundr882254879 = { {STR_tag}, 31, "\n/* Bound rout stubs decls */\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } FMAPSI21036215881 = { {STR_tag}, 31, "FMAP{SIG,ABSTRACT_FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Genera1846288163 = { {STR_tag}, 31, "\nGenerating generic classes... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } TUPARR17599877761 = { {STR_tag}, 31, "TUP{ARRAY{ARG},ARRAY{BOOL},$TP}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } dOBJCO818039632 = { {STR_tag}, 31, "$(OBJ): COMPILE_OPTIONS $(HDR)\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } ARRAYT2639536751 = { {STR_tag}, 32, "ARRAY{TUP{IDENT,AM_SHARED_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } FLISTT10231278821 = { {STR_tag}, 32, "FLIST{TUP{IDENT,AM_SHARED_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } TUPAM_15190408771 = { {STR_tag}, 32, "TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } TUPAM_9274374811 = { {STR_tag}, 32, "TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } t1F_TA1146024679 = { {STR_tag}, 32, " t1 = F_TAG(o1); t2 = F_TAG(o2);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } FMAPAM11450088111 = { {STR_tag}, 33, "FMAP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } NULLno1802593478 = { {STR_tag}, 33, "  NULL,  /* no type has tag 0 */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } STRgen663879124 = { {STR_tag}, 33, "\nSTR gen_SYS_str_for_tp(INT i) {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } TUPAM_5046535311 = { {STR_tag}, 33, "TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } FMAPAM4180099301 = { {STR_tag}, 34, "FMAP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } theres458619408 = { {STR_tag}, 34, "there seems to be no type for tag " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } ARRAYT16417752701 = { {STR_tag}, 35, "ARRAY{TUP{TP_CLASS,FSET{TP_CLASS}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } BOOLge1425058140 = { {STR_tag}, 35, "\nBOOL gen_SYS_ob_eq(OB o1,OB o2) {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } FLISTT1023672251 = { {STR_tag}, 35, "FLIST{TUP{TP_CLASS,FSET{TP_CLASS}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } TUPdTP17102330511 = { {STR_tag}, 35, "TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } FMAPdT9538166371 = { {STR_tag}, 36, "FMAP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } ROUTTU4672478931 = { {STR_tag}, 36, "ROUT{TUP{SIG,INT},TUP{SIG,INT}}:BOOL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } S00ext1705215522 = { {STR_tag}, 36, "  0,0, /* externals have no size */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } ARRAYT3489312821 = { {STR_tag}, 37, "ARRAY{TUP{SIG,ABSTRACT_FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } FLISTT10754949971 = { {STR_tag}, 37, "FLIST{TUP{SIG,ABSTRACT_FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Findin833949756 = { {STR_tag}, 38, "\nFinding types and parsing library... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } S00siz916295349 = { {STR_tag}, 38, "  0,0, /* sizeof *EXT_OB is unkown */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } dCSLIN1806998169 = { {STR_tag}, 38, "\n\n../$(CS): LINK_OPTIONS $(OBJ) $(HDR)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } CS2 = { {STR_tag}, 2, "CS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S15 = { {STR_tag}, 2, ",1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } l111 = { {STR_tag}, 2, "-l" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name167 = { {STR_tag}, 2, ")(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name169 = { {STR_tag}, 2, ")=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name173 = { {STR_tag}, 2, " {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name174 = { {STR_tag}, 2, "]." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name184 = { {STR_tag}, 2, "} " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name189 = { {STR_tag}, 2, "; " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } v12 = { {STR_tag}, 2, "v1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } v22 = { {STR_tag}, 2, "v2" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } ARRAYT17533334251 = { {STR_tag}, 39, "ARRAY{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } ARRAYT20650450421 = { {STR_tag}, 39, "ARRAY{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } FLISTT11452363451 = { {STR_tag}, 39, "FLIST{TUP{AM_CURSOR,$AM_EXPR,$AM_STMT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } FLISTT20498294841 = { {STR_tag}, 39, "FLIST{TUP{AM_LOCAL_EXPR,AM_LOCAL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } ARRAYT17447440761 = { {STR_tag}, 40, "ARRAY{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } FLISTT4062862511 = { {STR_tag}, 40, "FLIST{TUP{AM_LOCAL_EXPR,OPT_LOCAL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } Typegr495187748 = { {STR_tag}, 40, "\nType graph, conformance, main, init... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } Zonesa995165525 = { {STR_tag}, 40, "Zones are not supported on this platform" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } ifo1NU2130326399 = { {STR_tag}, 40, " if (o1==NULL || o2==NULL) return FALSE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } S0arr_1958137105 = { {STR_tag}, 41, ")0).arr_part,0, /* size without array */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } TUPAM_6534695741 = { {STR_tag}, 41, "TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } ifFVOI1051551677 = { {STR_tag}, 41, " if (FVOID(o1) && FVOID(o2)) return TRUE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } ARRAYT15621412511 = { {STR_tag}, 42, "ARRAY{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } FLISTT18586373101 = { {STR_tag}, 42, "FLIST{TUP{$TP,FMAP{IDENT,AM_SHARED_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } FMAPAM17876285731 = { {STR_tag}, 42, "FMAP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } ifFVOI1458418902 = { {STR_tag}, 42, " if (FVOID(o1) || FVOID(o2)) return FALSE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } t1o1he313626905 = { {STR_tag}, 42, " t1 = o1->header.tag; t2 = o2->header.tag;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } TUPAM_6678920601 = { {STR_tag}, 43, "TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } COMPIL44583114 = { {STR_tag}, 44, " COMPILE_OPTIONS $(HDR)\n\t$(CC) $(CFLAGS) -c " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } FMAPAM13575969311 = { {STR_tag}, 44, "FMAP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Itisno832547114 = { {STR_tag}, 44, "It is not possible to copy iter expressions\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Moreth814769403 = { {STR_tag}, 44, "More than one Platform specified, aborting.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } NULLNo1888211653 = { {STR_tag}, 44, " NULL /* No descendents found - how odd. */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } TUPAM_10249948011 = { {STR_tag}, 45, "TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } FMAPAM12444835041 = { {STR_tag}, 46, "FMAP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } TUPAM_3044586491 = { {STR_tag}, 47, "TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } ARRAYT19682127951 = { {STR_tag}, 48, "ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } FLISTT16831652041 = { {STR_tag}, 48, "FLIST{TUP{AM_ROUT_DEF,FLIST{AM_ITER_CALL_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } FMAPAM3396525441 = { {STR_tag}, 48, "FMAP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } ARG1 = { {STR_tag}, 3, "ARG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } ELT1 = { {STR_tag}, 3, "ELT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } ES = { {STR_tag}, 3, "ES " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } IFC1 = { {STR_tag}, 3, "IFC" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } OB4 = { {STR_tag}, 3, "(OB" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } RAT1 = { {STR_tag}, 3, "RAT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S011 = { {STR_tag}, 3, "0 }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } SIG1 = { {STR_tag}, 3, "SIG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } cd = { {STR_tag}, 3, "cd " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } hot11 = { {STR_tag}, 3, "hot" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name165 = { {STR_tag}, 3, " (*" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name171 = { {STR_tag}, 3, "};\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name175 = { {STR_tag}, 3, "  &" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name185 = { {STR_tag}, 3, ";\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name186 = { {STR_tag}, 3, "} *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name188 = { {STR_tag}, 3, "];\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } ARRAYT11462396471 = { {STR_tag}, 50, "ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } FLISTT16612032541 = { {STR_tag}, 50, "FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_OBJECT_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } define2130232346 = { {STR_tag}, 50, "#define ESS extern struct sather_type_description\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Therea756358858 = { {STR_tag}, 51, "There appears to be cycle(s) in the layouts usage.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } ARRAYT5466472141 = { {STR_tag}, 52, "ARRAY{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } FLISTT316836071 = { {STR_tag}, 52, "FLIST{TUP{AM_ROUT_DEF,FLIST{AM_BND_ITER_CALL_EXPR}}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } NULLNU651700710 = { {STR_tag}, 52, " {NULL, NULL} /* No descendents found - how odd. */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } define1568744415 = { {STR_tag}, 52, "#define ES extern struct sather_function_definition\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } ARRAYT12754048341 = { {STR_tag}, 54, "ARRAY{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } FLISTT13113346151 = { {STR_tag}, 54, "FLIST{TUP{AM_BND_CREATE_EXPR,BOUND_ITER_FRAME_LAYOUT}}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } Typech1407750582 = { {STR_tag}, 55, "\nType check, generate, inline, optimize, and make C... " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } struct2095330783 = { {STR_tag}, 55, "struct { int *refs;char *sn,*cn;long src;int sz,bx,at;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[57];
  } Thefol1627053373 = { {STR_tag}, 56, "\nThe following do not include break!, while! or until!.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } struct1935008194 = { {STR_tag}, 58, "struct sather_function_definition *sather_functions[] = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } BOOT = { {STR_tag}, 4, "BOOT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } CGEN1 = { {STR_tag}, 4, "CGEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } ESS = { {STR_tag}, 4, "ESS " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } FILE2 = { {STR_tag}, 4, "FILE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } I = { {STR_tag}, 4, " -I." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } IMPL1 = { {STR_tag}, 4, "IMPL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } PROG1 = { {STR_tag}, 4, "PROG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S010 = { {STR_tag}, 4, "  ,0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S08 = { {STR_tag}, 4, ")0)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S14 = { {STR_tag}, 4, "  ,1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } STAT2 = { {STR_tag}, 4, "STAT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name166 = { {STR_tag}, 4, " (**" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name176 = { {STR_tag}, 4, "\", \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name182 = { {STR_tag}, 4, "   }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name183 = { {STR_tag}, 4, " } *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name190 = { {STR_tag}, 4, " ../" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } of = { {STR_tag}, 4, " of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } v13 = { {STR_tag}, 4, ",v1," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } v23 = { {STR_tag}, 4, ",v2," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } char_d202626486 = { {STR_tag}, 60, " char _dummy_to_pad; /* make struct size a multiple of 2 */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } defaul1261471675 = { {STR_tag}, 60, "  default: fprintf(stderr,\"Internal error: unknown tag?\\n\");" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[66];
  } static1525151245 = { {STR_tag}, 65, "static struct sather_type_description *sather_types_static[] = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[69];
  } struct254901963 = { {STR_tag}, 68, "struct sather_type_description **sather_types = sather_types_static+" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } BFILE1 = { {STR_tag}, 5, "BFILE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } IDENT1 = { {STR_tag}, 5, "IDENT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } NULL7 = { {STR_tag}, 5, " NULL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } PARSE1 = { {STR_tag}, 5, "PARSE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } Print = { {STR_tag}, 5, "Print" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } S07 = { {STR_tag}, 5, "*)0)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } S_tag1 = { {STR_tag}, 5, "_tag " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } S_tag2 = { {STR_tag}, 5, "_tag:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } TRANS1 = { {STR_tag}, 5, "TRANS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name168 = { {STR_tag}, 5, " (*)(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name170 = { {STR_tag}, 5, " = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name172 = { {STR_tag}, 5, "() {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name177 = { {STR_tag}, 5, "\",\n  " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name180 = { {STR_tag}, 5, "    {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name181 = { {STR_tag}, 5, "     " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } void13 = { {STR_tag}, 5, " void" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[74];
  } unsign1349438557 = { {STR_tag}, 73, "         unsigned is_val:1,is_ref:1,is_abs:1,is_bnd:1,is_ext:1,is_aref:1;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } INLINE1 = { {STR_tag}, 6, "INLINE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } MANGLE1 = { {STR_tag}, 6, "MANGLE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } PARSER1 = { {STR_tag}, 6, "PARSER" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S13 = { {STR_tag}, 6, "\t-1};\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_entry1 = { {STR_tag}, 6, "_entry" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_ob2 = { {STR_tag}, 6, "_ob;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_tbl1 = { {STR_tag}, 6, "_tbl+(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_tbl2 = { {STR_tag}, 6, "_tbl[]" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } TP_TBL1 = { {STR_tag}, 6, "TP_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name178 = { {STR_tag}, 6, ",\n  {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name179 = { {STR_tag}, 6, "   { \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name187 = { {STR_tag}, 6, ");\n} *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } nested = { {STR_tag}, 6, "nested" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } size6 = { {STR_tag}, 6, " size=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } void12 = { {STR_tag}, 6, "void* " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } void14 = { {STR_tag}, 6, " void " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } with4 = { {STR_tag}, 6, " with:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[89];
  } TheSAT1577550343 = { {STR_tag}, 88, "The SATHER_COMMANDS variable has been replaced by SATHER_HOME;\nsee man page for details." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } AS_STMT1 = { {STR_tag}, 7, "AS_STMT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } CONFIG = { {STR_tag}, 7, "/CONFIG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } CONVERT1 = { {STR_tag}, 7, "CONVERT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } ELT_TBL1 = { {STR_tag}, 7, "ELT_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } F_ROUT5 = { {STR_tag}, 7, "F_ROUT " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } INTd = { {STR_tag}, 7, "INT $#;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } IN_MODE1 = { {STR_tag}, 7, "IN_MODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } NAMEMAP1 = { {STR_tag}, 7, "NAMEMAP" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S00 = { {STR_tag}, 7, ")0),0,\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } SCANNER1 = { {STR_tag}, 7, "SCANNER" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } SIG_TBL1 = { {STR_tag}, 7, "SIG_TBL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_ASIZE = { {STR_tag}, 7, "_ASIZE," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_ASIZE1 = { {STR_tag}, 7, "_ASIZE " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_entry = { {STR_tag}, 7, "_entry " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_struct2 = { {STR_tag}, 7, "_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } TP_ITER1 = { {STR_tag}, 7, "TP_ITER" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } TP_ROUT1 = { {STR_tag}, 7, "TP_ROUT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } VERSION = { {STR_tag}, 7, "VERSION" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } case4 = { {STR_tag}, 7, "  case " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } oncearg1 = { {STR_tag}, 7, "oncearg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } rmfr = { {STR_tag}, 7, "rm -fr " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } self9 = { {STR_tag}, 7, " self;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } static1 = { {STR_tag}, 7, "static " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } AM_CONST1 = { {STR_tag}, 8, "AM_CONST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } ATTRSORT1 = { {STR_tag}, 8, "ATTRSORT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } BOOLf_ = { {STR_tag}, 8, " BOOL f_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CALL_ARG1 = { {STR_tag}, 8, "CALL_ARG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CALL_SIG1 = { {STR_tag}, 8, "CALL_SIG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CC = { {STR_tag}, 8, "CC     =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CFLAGS = { {STR_tag}, 8, "CFLAGS =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CHECK_AM1 = { {STR_tag}, 8, "CHECK_AM" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CS1 = { {STR_tag}, 8, "CS     =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } LIBS = { {STR_tag}, 8, "LIBS   =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } Merging = { {STR_tag}, 8, "Merging " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } OBJ = { {STR_tag}, 8, "OBJ    =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } OPTIMIZE1 = { {STR_tag}, 8, "OPTIMIZE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } OTHER = { {STR_tag}, 8, "OTHER  =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } OUT_MODE1 = { {STR_tag}, 8, "OUT_MODE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S09 = { {STR_tag}, 8, "\",\n  0,\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } SFILE_ID1 = { {STR_tag}, 8, "SFILE_ID" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } SHOME = { {STR_tag}, 8, "SHOME  =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_entry4 = { {STR_tag}, 8, "_entry *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_tbl = { {STR_tag}, 8, "_tbl[])(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_zero2 = { {STR_tag}, 8, "_zero;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } TP_ARRAY1 = { {STR_tag}, 8, "TP_ARRAY" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } TP_CLASS1 = { {STR_tag}, 8, "TP_CLASS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } TP_GRAPH1 = { {STR_tag}, 8, "TP_GRAPH" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } dLIBS = { {STR_tag}, 8, " $(LIBS)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } extern3 = { {STR_tag}, 8, "\nextern " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } struct3 = { {STR_tag}, 8, " struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } syscom1 = { {STR_tag}, 8, "syscom: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } typedef1 = { {STR_tag}, 8, "typedef " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } v14 = { {STR_tag}, 8, " v1 = ((" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } v24 = { {STR_tag}, 8, " v2 = ((" };

