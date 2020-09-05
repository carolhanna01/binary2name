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
 CHAR arr_part[100];
  } sSTRrt1487513575 = { {STR_tag}, 99, " s=(STR)rt_arr_alloc_atomic(sizeof(struct STR_struct),STR_tag,sizeof(CHAR),F_ASIZE(STR,EXCEPTION));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[103];
  } elsefp1645913891 = { {STR_tag}, 102, "} else fprintf(stderr,\"Uncaught exception of type %s\\n\",gen_SYS_str_for_tp(TAG(EXCEPTION))->arr_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[105];
  } elsefp1785202769 = { {STR_tag}, 104, "} else fprintf(stderr,\"Uncaught exception of type %s\\n\",gen_SYS_str_for_tp(F_TAG(EXCEPTION))->arr_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[106];
  } newexp1208830217 = { {STR_tag}, 105, "`new\' expressions must take an argument specifying `asize\' in classes which have an include path to AREF." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } ADD_LOCK = { {STR_tag}, 9, "ADD_LOCK(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } AM_OUT1 = { {STR_tag}, 9, "AM_OUT {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } Argument = { {STR_tag}, 9, "Argument " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } BR_HERE1 = { {STR_tag}, 9, "BR_HERE()" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } EXCEPTION1 = { {STR_tag}, 9, "EXCEPTION" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FALSE2 = { {STR_tag}, 9, " = FALSE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } FF_NULL = { {STR_tag}, 9, ",&FF_NULL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } F_ROUT4 = { {STR_tag}, 9, "=(F_ROUT)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } HOISTING = { {STR_tag}, 9, "HOISTING\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } INTd0 = { {STR_tag}, 9, "INT $#=0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } MESSAGE = { {STR_tag}, 9, "MESSAGE: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } NULL3 = { {STR_tag}, 9, "==NULL) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } PARstart = { {STR_tag}, 9, "PAR start" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } PSR_NEAR = { {STR_tag}, 9, "PSR_NEAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } PS_WHERE = { {STR_tag}, 9, "PS_WHERE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } RECVOB2 = { {STR_tag}, 9, "  RECVOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } SI_HERE2 = { {STR_tag}, 9, "SI_HERE()" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } SI_WHERE1 = { {STR_tag}, 9, "SI_WHERE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } SI_global1 = { {STR_tag}, 9, "SI_global" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_IS_VOID = { {STR_tag}, 9, "_IS_VOID(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } S_iter_ob = { {STR_tag}, 9, "_iter_ob " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } Suggest = { {STR_tag}, 9, "\tSuggest:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } Thecall = { {STR_tag}, 9, "The call " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } UNLOCKV1 = { {STR_tag}, 9, "UNLOCKV(&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } address2 = { {STR_tag}, 9, "->address" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } always1 = { {STR_tag}, 9, "  always=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } attach_cl = { {STR_tag}, 9, "attach_cl" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } attach_ob = { {STR_tag}, 9, "attach_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } bound_arg1 = { {STR_tag}, 9, "bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } call3 = { {STR_tag}, 9, "->call = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } calliter = { {STR_tag}, 9, "call iter" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } default1 = { {STR_tag}, 9, "default: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } expr3 = { {STR_tag}, 9, "\n   expr=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } extern1 = { {STR_tag}, 9, "\n extern " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } has_new = { {STR_tag}, 9, ", has_new" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } in_cond = { {STR_tag}, 9, " in_cond=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } inlined_C = { {STR_tag}, 9, "inlined_C" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } is_arg = { {STR_tag}, 9, "  is_arg=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } islocal = { {STR_tag}, 9, " is local" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } main_ob1 = { {STR_tag}, 9, " main_ob;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } memcpy1 = { {STR_tag}, 9, ") memcpy(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } name145 = { {STR_tag}, 9, "\n     { \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } name67 = { {STR_tag}, 9, "\n        " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } obobn = { {STR_tag}, 9, "  ob=obn;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } oncearg = { {STR_tag}, 9, "->oncearg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } par2 = { {STR_tag}, 9, "/* par */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } params_cl = { {STR_tag}, 9, "params_cl" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } params_ob = { {STR_tag}, 9, "params_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } readonly2 = { {STR_tag}, 9, "readonly " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } ret_arg1 = { {STR_tag}, 9, "->ret_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } return6 = { {STR_tag}, 9, "\n return " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } return7 = { {STR_tag}, 9, "return;\n}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } retval1 = { {STR_tag}, 9, " retval;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } retval2 = { {STR_tag}, 9, "\n retval=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } self6 = { {STR_tag}, 9, "->self = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } self7 = { {STR_tag}, 9, "(self)) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } struct1 = { {STR_tag}, 9, "struct {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } typecase2 = { {STR_tag}, 9, "typecase " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } void7 = { {STR_tag}, 9, "(void *)&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[10];
  } volatile3 = { {STR_tag}, 9, "volatile " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[113];
  } Boundc1977295681 = { {STR_tag}, 112, "Bound creation calls may not be made on create expressions which don\'t specify the type of object being created." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[115];
  } Dispat1077094544 = { {STR_tag}, 114, "Dispatched bound routines/iters have not yet been implemented, formal and actual bound types have to be the same. " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } Builtin = { {STR_tag}, 10, " - Builtin" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } F_SYSOBEQ1 = { {STR_tag}, 10, "F_SYSOBEQ(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } LM_UNLOCK = { {STR_tag}, 10, "LM_UNLOCK(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } PAR_ATTACH = { {STR_tag}, 10, "PAR_ATTACH" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } PAR_START = { {STR_tag}, 10, "PAR_START(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } PSR_WHERE = { {STR_tag}, 10, "PSR_WHERE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_iter_ob1 = { {STR_tag}, 10, "_iter_ob);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_iter_obf = { {STR_tag}, 10, "_iter_ob f" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_ob_struct1 = { {STR_tag}, 10, "_ob_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_pS_cohort = { {STR_tag}, 10, "_pS_cohort" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } S_pS_par_ob = { {STR_tag}, 10, "_pS_par_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } Thename = { {STR_tag}, 10, "The name `" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } access2 = { {STR_tag}, 10, "   access:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } address1 = { {STR_tag}, 10, "->address=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } address3 = { {STR_tag}, 10, "->address," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } after_loop = { {STR_tag}, 10, "after_loop" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } arr_part1 = { {STR_tag}, 10, ".arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } asize1 = { {STR_tag}, 10, "->asize = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } bound_arg0 = { {STR_tag}, 10, "bound_arg0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } branch2 = { {STR_tag}, 10, "   branch=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } default2 = { {STR_tag}, 10, "default: ;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } fork3 = { {STR_tag}, 10, "-- fork @ " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ifFVOID = { {STR_tag}, 10, "if (FVOID(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } intres0 = { {STR_tag}, 10, "int res=0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } isbound = { {STR_tag}, 10, " is bound\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } line1 = { {STR_tag}, 10, "\n/* #line " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } localfor = { {STR_tag}, 10, "local for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } main_ob2 = { {STR_tag}, 10, "main_ob = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } near_arg = { {STR_tag}, 10, " near_arg=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } no_threads = { {STR_tag}, 10, "no_threads" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ret_arg = { {STR_tag}, 10, "->ret_arg," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } ret_val3111 = { {STR_tag}, 10, "ret_val = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } rt_stop1 = { {STR_tag}, 10, "rt_stop();" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } self_to_be = { {STR_tag}, 10, "self_to_be" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } unknown1 = { {STR_tag}, 10, "unknown \'@" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } unknown2 = { {STR_tag}, 10, "unknown \'%" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } unknownd = { {STR_tag}, 10, "unknown \'$" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[11];
  } while02 = { {STR_tag}, 10, "}while(0);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[129];
  } bd01232683264 = { {STR_tag}, 128, "\0\1\2\3\4\5\6\7\b\t\n\v\f\r\16\17\20\21\22\23\24\25\26\27\30\31\32\33\34\35\36\37 !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } Abstract = { {STR_tag}, 11, " - Abstract" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } BR_REQUEST_ = { {STR_tag}, 11, "BR_REQUEST_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } BR_word_t3 = { {STR_tag}, 11, "(BR_word_t)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } External = { {STR_tag}, 11, " - External" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } LOOP_BEGIN1 = { {STR_tag}, 11, "LOOP_BEGIN(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } LOOPself = { {STR_tag}, 11, "LOOP, self=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } NULL6 = { {STR_tag}, 11, " == NULL) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } OBALLOC1 = { {STR_tag}, 11, " = OBALLOC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } Outputsig = { {STR_tag}, 11, "Output sig " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } PROTECT_END1 = { {STR_tag}, 11, "PROTECT_END" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } RECVFOB1 = { {STR_tag}, 11, " = RECVFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } SENDFOB2 = { {STR_tag}, 11, " = SENDFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } SI_HERE1 = { {STR_tag}, 11, "=SI_HERE();" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } S_boxedself = { {STR_tag}, 11, "_boxed self" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } ZALLOC2 = { {STR_tag}, 11, " =  ZALLOC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } arelocal = { {STR_tag}, 11, " are local\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } arr_part2 = { {STR_tag}, 11, "->arr_part)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } arr_part3 = { {STR_tag}, 11, "->arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } attach1 = { {STR_tag}, 11, "-- attach:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } bound_arg = { {STR_tag}, 11, "->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } dest_tp1 = { {STR_tag}, 11, "   dest_tp=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } externINT = { {STR_tag}, 11, "extern INT " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } has_raise = { {STR_tag}, 11, ", has_raise" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } iscyclic = { {STR_tag}, 11, " is cyclic\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } main_args1 = { {STR_tag}, 11, " main_args;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } may_block1 = { {STR_tag}, 11, ", may_block" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } ret_val2111 = { {STR_tag}, 11, "  ret_val;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } returnres = { {STR_tag}, 11, "return res;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } sather_main1 = { {STR_tag}, 11, "sather_main" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } src_tp1 = { {STR_tag}, 11, "\n   src_tp=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } state111 = { {STR_tag}, 11, "state1:; \n " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } unbound_arg1 = { {STR_tag}, 11, "unbound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } while0 = { {STR_tag}, 11, ";} while(0)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } while11 = { {STR_tag}, 11, "while (1) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[12];
  } workingon = { {STR_tag}, 11, "working on " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } BR_word_t1 = { {STR_tag}, 12, ", BR_word_t " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } BR_word_t2 = { {STR_tag}, 12, ",(BR_word_t)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } BR_word_t4 = { {STR_tag}, 12, "(BR_word_t)&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } INTasize = { {STR_tag}, 12, " INT asize;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } LOOP2self = { {STR_tag}, 12, "LOOP2, self=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } PROTECT_WHEN1 = { {STR_tag}, 12, "PROTECT_WHEN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } PSATHER_STOP1 = { {STR_tag}, 12, "PSATHER_STOP" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } REMOTE_EXEC = { {STR_tag}, 12, "REMOTE_EXEC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } SI_SET_HERE1 = { {STR_tag}, 12, "SI_SET_HERE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_R_WATTR_AA = { {STR_tag}, 12, "_R_WATTR_AA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_V_WATTR_PP = { {STR_tag}, 12, "_V_WATTR_PP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_frameframe = { {STR_tag}, 12, "_frame frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } S_iframe = { {STR_tag}, 12, "_iframe = {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } WHEREob = { {STR_tag}, 12, ",WHERE(ob));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } ZALLOC_BIG1 = { {STR_tag}, 12, "=ZALLOC_BIG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } asitmust = { {STR_tag}, 12, " as it must." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } endof = { {STR_tag}, 12, "end;  -- of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } exprvoid = { {STR_tag}, 12, "].expr=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } externSTR = { {STR_tag}, 12, " extern STR " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } fhotarg = { {STR_tag}, 12, "   f->hotarg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } forked = { {STR_tag}, 12, "/* forked */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } funcptr1 = { {STR_tag}, 12, "->funcptr = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } funcptr2 = { {STR_tag}, 12, "->funcptr))(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } gotostate = { {STR_tag}, 12, ": goto state" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } has_export = { {STR_tag}, 12, ", has_export" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } has_import = { {STR_tag}, 12, ", has_import" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } hotargument = { {STR_tag}, 12, "hot argument" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } ignoring = { {STR_tag}, 12, ", ignoring @" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } immutable3 = { {STR_tag}, 12, " - immutable" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } localargs = { {STR_tag}, 12, "local args: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } main_args2 = { {STR_tag}, 12, "main_args = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } state01 = { {STR_tag}, 12, "->state = 0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } switchTAG = { {STR_tag}, 12, "switch (TAG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } unbound_arg = { {STR_tag}, 12, " unbound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[13];
  } void8 = { {STR_tag}, 12, "( void (*) (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } DECLARE_LOCK1 = { {STR_tag}, 13, "DECLARE_LOCK(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } FFprevNULL = { {STR_tag}, 13, "FF.prev=NULL;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } F_R_RATTR_NA1 = { {STR_tag}, 13, "F_R_RATTR_NA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } F_V_RATTR_LL1 = { {STR_tag}, 13, "F_V_RATTR_LL(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } F_V_RATTR_LP1 = { {STR_tag}, 13, "F_V_RATTR_LP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } INFOiter = { {STR_tag}, 13, ": INFO: iter " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } NULLNULL = { {STR_tag}, 13, "==NULL)?NULL:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } PARLOOP_FORK_ = { {STR_tag}, 13, "PARLOOP_FORK_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } PROTECT_BEGIN1 = { {STR_tag}, 13, "PROTECT_BEGIN" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_VA_WATTR_AA = { {STR_tag}, 13, "_VA_WATTR_AA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_frameframe1 = { {STR_tag}, 13, "_frame) frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_local_frame = { {STR_tag}, 13, "_local_frame[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_ob_struct = { {STR_tag}, 13, "_ob_struct));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } S_obobnNULL = { {STR_tag}, 13, "_ob obn=NULL;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } Staringwith = { {STR_tag}, 13, "Staring with:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } ZALLOC_LEAF1 = { {STR_tag}, 13, "=ZALLOC_LEAF(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } ZALLOCf = { {STR_tag}, 13, " = ZALLOC(f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } boundtype = { {STR_tag}, 13, " - bound type" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } conc = { {STR_tag}, 13, ");  -- conc.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } dAS_EXPRvoid = { {STR_tag}, 13, "$AS_EXPR=void" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } dependson = { {STR_tag}, 13, "  depends on " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } fiter_frame1 = { {STR_tag}, 13, "f->iter_frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } foncearg = { {STR_tag}, 13, "   f->oncearg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } foncearg1 = { {STR_tag}, 13, " = f->oncearg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } framenested = { {STR_tag}, 13, "frame->nested" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } inthecall = { {STR_tag}, 13, " in the call " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } isbuiltin = { {STR_tag}, 13, " is builtin.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } name61 = { {STR_tag}, 13, "============\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } obbound_arg1 = { {STR_tag}, 13, "ob->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } pS_at_handler = { {STR_tag}, 13, "pS_at_handler" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } ratherthan = { {STR_tag}, 13, " rather than " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } returndummy = { {STR_tag}, 13, "return dummy;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } returnsvoid = { {STR_tag}, 13, "returns void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } selfFF = { {STR_tag}, 13, "(self,&FF)) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } spreadclass = { {STR_tag}, 13, "spread class " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } usingfull = { {STR_tag}, 13, ", using full\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } voidsurr_ptr = { {STR_tag}, 13, "void surr_ptr" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[14];
  } while01 = { {STR_tag}, 13, "  }while(0);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } Definitionof = { {STR_tag}, 14, "Definition of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } F_VA_RATTR_NA1 = { {STR_tag}, 14, "F_VA_RATTR_NA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } F_VA_RATTR_NN1 = { {STR_tag}, 14, "F_VA_RATTR_NN(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } LM_ELSE_CLAUSE = { {STR_tag}, 14, "LM_ELSE_CLAUSE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } LOCK_NO_STACK = { {STR_tag}, 14, "|LOCK_NO_STACK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } PAR_INCREMENT = { {STR_tag}, 14, "PAR_INCREMENT(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } POP_EXCEPTION11 = { {STR_tag}, 14, "POP_EXCEPTION(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } PREFETCH_WAIT = { {STR_tag}, 14, "PREFETCH_WAIT(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } PSATHER_ABORT1 = { {STR_tag}, 14, "PSATHER_ABORT;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } S_boxed_struct = { {STR_tag}, 14, "_boxed_struct)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } S_call_function1 = { {STR_tag}, 14, "_call_function" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } S_frame_struct = { {STR_tag}, 14, "_frame_struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } Temporaryfor = { {STR_tag}, 14, "Temporary for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } attached = { {STR_tag}, 14, "/* attached */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } attr_acces = { {STR_tag}, 14, "   attr_acces:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } attr_access1 = { {STR_tag}, 14, "  attr_access:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } dAS_NODEvoid = { {STR_tag}, 14, "$AS_NODE=void " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } endof1 = { {STR_tag}, 14, "end;    -- of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } headertag = { {STR_tag}, 14, ")->header.tag=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } ifFARob = { {STR_tag}, 14, " if(FAR(ob)) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } ignoringiter = { {STR_tag}, 14, "ignoring iter " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } lcis_near = { {STR_tag}, 14, "\n  lc.is_near=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } locked_lock = { {STR_tag}, 14, "locked_lock = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } obbound_arg = { {STR_tag}, 14, "&ob->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } obret_arg = { {STR_tag}, 14, "\n ob->ret_arg=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } returnret_val = { {STR_tag}, 14, "return ret_val" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } sizeofstruct = { {STR_tag}, 14, "sizeof(struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } switchF_TAG = { {STR_tag}, 14, "switch (F_TAG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[15];
  } void10 = { {STR_tag}, 14, "  (void (*)())" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } AFTERHOISTING = { {STR_tag}, 15, "AFTER HOISTING\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } CHARarr_part = { {STR_tag}, 15, " CHAR arr_part[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } COUNT_DISPATCH1 = { {STR_tag}, 15, "COUNT_DISPATCH;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } LM_LOCK_CLAUSE = { {STR_tag}, 15, "LM_LOCK_CLAUSE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } LOCK_WITH_ELSE1 = { {STR_tag}, 15, "|LOCK_WITH_ELSE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } MY_CLUSTER_SIZE1 = { {STR_tag}, 15, "MY_CLUSTER_SIZE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } POP_EXCEPTION12 = { {STR_tag}, 15, "POP_EXCEPTION1;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } PRE_R_RATTR_NA = { {STR_tag}, 15, "PRE_R_RATTR_NA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } PRE_V_RATTR_LP = { {STR_tag}, 15, "PRE_V_RATTR_LP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } S_call_function = { {STR_tag}, 15, "_call_function;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } S_iter_ob_struct1 = { {STR_tag}, 15, "_iter_ob_struct" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } Thebounditer = { {STR_tag}, 15, "The bound iter " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } boundobject = { {STR_tag}, 15, " - bound object" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } endofif = { {STR_tag}, 15, "end;  -- of if\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } fbound_arg = { {STR_tag}, 15, " = f->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } framestate = { {STR_tag}, 15, "frame->state = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } fstatef = { {STR_tag}, 15, "f->state = (f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } headertag1 = { {STR_tag}, 15, "->header.tag = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } ifBR_HERE = { {STR_tag}, 15, "if (BR_HERE()==" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } intijlength = { {STR_tag}, 15, "int i,j,length;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } isrecursive = { {STR_tag}, 15, " is recursive.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } pS_fork_handler = { {STR_tag}, 15, "pS_fork_handler" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } readswrites = { {STR_tag}, 15, " (reads/writes)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } state0 = { {STR_tag}, 15, "->state == 0) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[16];
  } toinlinein = { {STR_tag}, 15, "\' to inline in " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } AM_POLL_NETWORK1 = { {STR_tag}, 16, "AM_POLL_NETWORK;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } CASTINGERROR = { {STR_tag}, 16, "CASTING ERROR: \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } EXT_OB_boxed1 = { {STR_tag}, 16, "((EXT_OB_boxed) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } LOCK_UNLOCK_NOW = { {STR_tag}, 16, "LOCK_UNLOCK_NOW;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } PRE_VA_RATTR_NA = { {STR_tag}, 16, "PRE_VA_RATTR_NA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } PS_NEAR_OR_VOID = { {STR_tag}, 16, "PS_NEAR_OR_VOID(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } attraccessfor = { {STR_tag}, 16, "attr access for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } forthisarray = { {STR_tag}, 16, " for this array " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } frameself = { {STR_tag}, 16, "(frame->self)) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } immutableclass1 = { {STR_tag}, 16, "immutable class " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } inCONFIGfor = { {STR_tag}, 16, ") in CONFIG for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } localSENDFOB = { {STR_tag}, 16, "->local=SENDFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } localargument = { {STR_tag}, 16, "local argument: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } localforboxed = { {STR_tag}, 16, "local for boxed " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } notsubtypesof = { {STR_tag}, 16, "not subtypes of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } obnbound_arg = { {STR_tag}, 16, ",&obn->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } obnbound_arg1 = { {STR_tag}, 16, "  obn->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } pS_simple_par_ob = { {STR_tag}, 16, "pS_simple_par_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } returnretval = { {STR_tag}, 16, " return retval;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } sometimes_far = { {STR_tag}, 16, "  sometimes_far=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } specul_prefetch = { {STR_tag}, 16, "specul_prefetch=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } state1 = { {STR_tag}, 16, "->state == -1) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[17];
  } voidframe = { {STR_tag}, 16, "(void *)&frame->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } AM_POLL_NETWORK = { {STR_tag}, 17, "AM_POLL_NETWORK;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } F_STRING_COPY_TO = { {STR_tag}, 17, "F_STRING_COPY_TO(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } Failedinvariant = { {STR_tag}, 17, "Failed invariant " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } GeneratingCfor = { {STR_tag}, 17, "Generating C for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } INIT_LOCK_HEADER1 = { {STR_tag}, 17, "INIT_LOCK_HEADER(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } INTdummy_INT_ = { {STR_tag}, 17, "{ INT dummy_INT_=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } Localforarg = { {STR_tag}, 17, "/*Local for arg*/" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } PREFETCHINGDONE = { {STR_tag}, 17, "PREFETCHING DONE\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } RAISEEXCEPTION = { {STR_tag}, 17, "RAISE(EXCEPTION);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } S_iter_ob_struct = { {STR_tag}, 17, "_iter_ob_struct);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } Sideeffectsfor = { {STR_tag}, 17, "Side effects for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } Wrappertounbox = { {STR_tag}, 17, "Wrapper to unbox " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } ZALLOC_LEAF_BIG = { {STR_tag}, 17, "=ZALLOC_LEAF_BIG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } abstractframe = { {STR_tag}, 17, " - abstract frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } at_attach_result = { {STR_tag}, 17, "at_attach_result;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } at_attachresult = { {STR_tag}, 17, "at_attach.result," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } endofcase = { {STR_tag}, 17, "end;  -- of case\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } endoflock = { {STR_tag}, 17, "end;  -- of lock\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } endofloop = { {STR_tag}, 17, "end;  -- of loop\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } externalCclass = { {STR_tag}, 17, "external C class " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } fiter_frame = { {STR_tag}, 17, "f->iter_frame = (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } has_arith_error = { {STR_tag}, 17, ", has_arith_error" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } has_fatal_error = { {STR_tag}, 17, ", has_fatal_error" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } immutable_part2 = { {STR_tag}, 17, ",immutable_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } immutable_part3 = { {STR_tag}, 17, ")->immutable_part" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } inCONFIGfor1 = { {STR_tag}, 17, "\', in CONFIG for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } localfortest = { {STR_tag}, 17, "local for :? test" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } onvoidselfin = { {STR_tag}, 17, " on void self in " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } optimizedcode = { {STR_tag}, 17, "optimized code {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[18];
  } routattraccess = { {STR_tag}, 17, "rout attr access " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } AFTERflattening = { {STR_tag}, 18, "AFTER flattening:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } AS_ATTR_DEFvoid = { {STR_tag}, 18, "AS_ATTR_DEF=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } AS_ROUT_DEFvoid = { {STR_tag}, 18, "AS_ROUT_DEF=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Frameforcallto = { {STR_tag}, 18, "Frame for call to " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Functionis_near = { {STR_tag}, 18, "Function: is_near=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Initializeshared = { {STR_tag}, 18, "Initialize shared " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } LM_TRY563339091 = { {STR_tag}, 18, "LM_TRY_SINGLE_LOCK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Theargumentmode = { {STR_tag}, 18, "The argument mode " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Theargumenttype = { {STR_tag}, 18, "The argument type " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Theboundroutine = { {STR_tag}, 18, "The bound routine " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Thetypeofself = { {STR_tag}, 18, "The type of self: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Unknownsignature = { {STR_tag}, 18, "Unknown signature " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } Unknownvariable = { {STR_tag}, 18, "Unknown variable \'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } after_prefetch1 = { {STR_tag}, 18, "   after_prefetch=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } alloc_frame = { {STR_tag}, 18, ")].alloc_frame)();" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } bounditertype = { {STR_tag}, 18, " - bound iter type" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } callvoid = { {STR_tag}, 18, "->call))((void *) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } comparingagainst = { {STR_tag}, 18, "comparing against " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } fiter_framearg = { {STR_tag}, 18, "f->iter_frame->arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } framestate1 = { {STR_tag}, 18, "frame->state = -1;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } is_farbecauseof = { {STR_tag}, 18, "is_far because of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } is_sometimes_far = { {STR_tag}, 18, "is_sometimes_far: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } localforunboxed = { {STR_tag}, 18, "local for unboxed " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[19];
  } sometimesfarfor = { {STR_tag}, 18, "sometimes far for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } AS_CLASS_DEFvoid = { {STR_tag}, 19, "AS_CLASS_DEF=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } AS_CONST_DEFvoid = { {STR_tag}, 19, "AS_CONST_DEF=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } BROADC1443310781 = { {STR_tag}, 19, "BROADCAST_GLOBAL_V(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } BROADC1443310857 = { {STR_tag}, 19, "BROADCAST_GLOBAL_R(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } BR_word_tSENDFOB1 = { {STR_tag}, 19, "(BR_word_t)SENDFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } Dispatchedcallto = { {STR_tag}, 19, "Dispatched call to " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } LM_LOC1545209594 = { {STR_tag}, 19, "LM_LOCK_UNLOCK_NOW;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } OB_HEADERheader = { {STR_tag}, 19, " OB_HEADER header;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } PREFETCHNULL = { {STR_tag}, 19, "(*(PREFETCH *)NULL)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } RESTOR712275783 = { {STR_tag}, 19, "RESTORE_CURRENT_EX;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } SI_HEREcluster = { {STR_tag}, 19, ",SI_HERE()->cluster" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } Theinferredtype = { {STR_tag}, 19, "The inferred type: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } beforeinserting = { {STR_tag}, 19, "\nbefore inserting:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } endlockclause = { {STR_tag}, 19, "end -- lock clause\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } endofpar = { {STR_tag}, 19, "-- end;  -- of par\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } framen219781171 = { {STR_tag}, 19, "frame->nested_biter" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } haswrongindent = { {STR_tag}, 19, " has wrong indent: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } ifTAGEXCEPTION = { {STR_tag}, 19, "if(TAG(EXCEPTION)==" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } immutable_part4 = { {STR_tag}, 19, ")->immutable_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } loopin1411911962 = { {STR_tag}, 19, "loop index variable" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } mustbespecified = { {STR_tag}, 19, " must be specified." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } sasizelength = { {STR_tag}, 19, " s->asize = length;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } sather1842900085 = { {STR_tag}, 19, "sather_main(main_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } setbou495125082 = { {STR_tag}, 19, "set bound arguments" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } void_isillegal = { {STR_tag}, 19, "void(_) is illegal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[20];
  } void_local_frame = { {STR_tag}, 19, "void *_local_frame[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AS_SHA1798301576 = { {STR_tag}, 20, "AS_SHARED_DEF=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } AT_ATT1332028378 = { {STR_tag}, 20, " AT_ATTACH_FORK_DONE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } BROADC1653099486 = { {STR_tag}, 20, "BROADCAST_GLOBAL_VA(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } BR_word_tSENDFOB = { {STR_tag}, 20, ",(BR_word_t)SENDFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Forkhandler = { {STR_tag}, 20, "\n/* Fork handler */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Optimi440073291 = { {STR_tag}, 20, "Optimizing function " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } Pnbounderrorin = { {STR_tag}, 20, "@Pn bound error in \'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } bounditerobject = { {STR_tag}, 20, " - bound iter object" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } doesntconformto = { {STR_tag}, 20, " doesn\'t conform to " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } endoffork = { {STR_tag}, 20, "-- end;  -- of fork\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } endofprotect = { {STR_tag}, 20, "end;  -- of protect\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } frameselfFF = { {STR_tag}, 20, "(frame->self,&FF)) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } ifdefDESTROY_CHK = { {STR_tag}, 20, "\n#ifdef DESTROY_CHK\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } immutable_part1 = { {STR_tag}, 20, ")->immutable_part = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } inline1440253858 = { {STR_tag}, 20, "inlined_C: touching " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } locala767931605 = { {STR_tag}, 20, "local analysis done\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } returndependson = { {STR_tag}, 20, "  return depends on " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[21];
  } unrecognizedtype = { {STR_tag}, 20, " unrecognized type \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } BR_word_t__res0 = { {STR_tag}, 21, "BR_word_t __res = 0;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } FFargs931318564 = { {STR_tag}, 21, "FF.args=_local_frame;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } Localf164518964 = { {STR_tag}, 21, "Local for ext arr arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } POP_EX889458020 = { {STR_tag}, 21, "POP_EXCEPTION1; goto " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } PREFET1824934344 = { {STR_tag}, 21, "PREFETCH_SPECUL_WAIT(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } THREAD1879362277 = { {STR_tag}, 21, " THREAD_PROTECT_END\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } endoftypecase = { {STR_tag}, 21, "end;  -- of typecase\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ifF_TAGEXCEPTION = { {STR_tag}, 21, "if(F_TAG(EXCEPTION)==" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } ignoredbyinliner = { {STR_tag}, 21, " ignored by inliner.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } inCONFIGmacrofor = { {STR_tag}, 21, " in CONFIG macro for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } isnotasubtypeof = { {STR_tag}, 21, " is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } nonoptimizedcode = { {STR_tag}, 21, "non optimized code {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } parandparloop = { {STR_tag}, 21, "`par\', and `parloop\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } struct1392482001 = { {STR_tag}, 21, ",struct _func_frame *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[22];
  } unders1197384132 = { {STR_tag}, 21, "underscore-expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Cannot457229582 = { {STR_tag}, 22, "Cannot translate type." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Fillin1000309002 = { {STR_tag}, 22, "Fill in hot arguments." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Initia393638030 = { {STR_tag}, 22, "Initialize iter state." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } LM_ACQ898286306 = { {STR_tag}, 22, "LM_ACQUIRE_SINGLE_LOCK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } LM_POP1734626182 = { {STR_tag}, 22, "LM_POP_OFF_EXEPT_STACK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } LM_PUS2132113989 = { {STR_tag}, 22, "LM_PUSH_ON_EXEPT_STACK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } LM_REL804480895 = { {STR_tag}, 22, "LM_RELEASE_SINGLE_LOCK" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Lookin852250521 = { {STR_tag}, 22, "Looking for signature " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Ooflocalaccesses = { {STR_tag}, 22, "O: # of local accesses" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } SET_CU1561499530 = { {STR_tag}, 22, "SET_CURRENT_EXCEPTION;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Thetypeofthecall = { {STR_tag}, 22, "The type of the call: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } Violat167861416 = { {STR_tag}, 22, "Violation of assertion" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } access955627674 = { {STR_tag}, 22, "access_try_single_lock" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } endofwithnear = { {STR_tag}, 22, "end;  -- of with-near\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } fori0iargci = { {STR_tag}, 22, "for (i=0;i<argc;i++) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } headerdestroyed0 = { {STR_tag}, 22, ")->header.destroyed=0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } intheb890863962 = { {STR_tag}, 22, " in the bound routine " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } localp1561414680 = { {STR_tag}, 22, "local ptr for unboxed " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } macroinCONFIGfor = { {STR_tag}, 22, "\' macro in CONFIG for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } outofboundsmacro = { {STR_tag}, 22, " out of bounds (macro " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } rt_sta483296400 = { {STR_tag}, 22, "rt_start(0,argc,argv);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } struct1270974273 = { {STR_tag}, 22, "struct _func_frame FF;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } struct139248200 = { {STR_tag}, 22, ", struct _func_frame *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } unders11973841321 = { {STR_tag}, 22, "underscore-expression:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } withnearcantuse = { {STR_tag}, 22, "`with near\' can\'t use " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[23];
  } withnearcantuse1 = { {STR_tag}, 22, "\'with near\' can\'t use " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } AT_ATT1665390693 = { {STR_tag}, 23, " AT_ATTACH_REQUEST_DONE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Ayield1696661686 = { {STR_tag}, 23, "A yield value of type: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Fillin1044524838 = { {STR_tag}, 23, "Fill in once arguments." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Gottoe704218937 = { {STR_tag}, 23, "Got to end of emit_expr" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } INIT_T802300136 = { {STR_tag}, 23, " INIT_THREAD_MEMORY();\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } LM_CLO1780161327 = { {STR_tag}, 23, "LM_CLOSE_LOCK_STATEMENT" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } RECVFO1140193010 = { {STR_tag}, 23, "=RECVFOB(obn->bound_arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Simple512463066 = { {STR_tag}, 23, "Simple @ expression end" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } THREAD163596686 = { {STR_tag}, 23, " THREAD_PROTECT_BEGIN\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } Voidob1946594598 = { {STR_tag}, 23, "Void object of typecase" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } extern2035574927 = { {STR_tag}, 23, "external FORTRAN class " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } fiter_1590862403 = { {STR_tag}, 23, "f->iter_frame->state=0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } ignore1571167589 = { {STR_tag}, 23, " ignored by optimizer.\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } sizesizeofstruct = { {STR_tag}, 23, "->size = sizeof(struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[24];
  } switchframestate = { {STR_tag}, 23, "switch (frame->state) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } AS_INC530055583 = { {STR_tag}, 24, "AS_INCLUDE_CLAUSE=void\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } Aretur964039722 = { {STR_tag}, 24, "A return value of type: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } FFprev530765537 = { {STR_tag}, 24, "FF.prev=prev_func_frame;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } F_HAND989235039 = { {STR_tag}, 24, "F_HANDLERINVOKE_HANDLER(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } Holdsr2142202862 = { {STR_tag}, 24, "Holds result of call to " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } LM_TRY1921320459 = { {STR_tag}, 24, "LM_TRY_CONJUNCTIVE_LOCKS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } PSATHE958595919 = { {STR_tag}, 24, "PSATHER_START(argc,argv)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } Ranoff1276757503 = { {STR_tag}, 24, "Ran off unbound arg list" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } headeridob_count = { {STR_tag}, 24, ")->header.id=ob_count++;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } initializestate = { {STR_tag}, 24, "  /* initialize state */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } main_a1626095403 = { {STR_tag}, 24, "main_args->asize = argc;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } return463650331 = { {STR_tag}, 24, "return is sometimes far\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } starti1899675989 = { {STR_tag}, 24, "starting local analysis\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } state1111 = { {STR_tag}, 24, "->state == -1 ? -1 : 1);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } struct1621582358 = { {STR_tag}, 24, ",struct _func_frame *pFF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[25];
  } unknwo513321933 = { {STR_tag}, 24, "unknwon attribute name ." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } AT_ATT2128087939 = { {STR_tag}, 25, "AT_ATTACH_END(at_attach);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } AT_ATT216595904 = { {STR_tag}, 25, "AT_ATTACH_DEC(at_attach);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Errore497477243 = { {STR_tag}, 25, "Error: emit_am_fork_stmt\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Oofite1407208962 = { {STR_tag}, 25, "O: # of iters not inlined" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Simple318344970 = { {STR_tag}, 25, "Simple @ expression found" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Simple351050430 = { {STR_tag}, 25, "Simple @ expression start" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Thetyp933302920 = { {STR_tag}, 25, "The typecase test local `" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } Violat835114313 = { {STR_tag}, 25, "Violation of precondition" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } fiter_2029027530 = { {STR_tag}, 25, "f->iter_frame = OB_ALLOC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } foundi391347248 = { {STR_tag}, 25, "found import for routine " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } isntal923684775 = { {STR_tag}, 25, "\' isn\'t a local variable." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } ressat1527183294 = { {STR_tag}, 25, "res = sather_main(main_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } sather540173678 = { {STR_tag}, 25, "sather_prog_name=argv[0];" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } struct16215823581 = { {STR_tag}, 25, ", struct _func_frame *pFF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[26];
  } whichh1927473166 = { {STR_tag}, 25, " which has the same name." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } AT_ATT1091925463 = { {STR_tag}, 26, "AT_ATTACH_WAIT(at_attach);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Bofbui227171716 = { {STR_tag}, 26, "B: # of builtin iter calls" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Comple1197394349 = { {STR_tag}, 26, "Complex @ expression found" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } S_obZA389105886 = { {STR_tag}, 26, "_ob) ZALLOC(sizeof(struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Statem239123203 = { {STR_tag}, 26, "Statement to be inserted:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } Violat32627033 = { {STR_tag}, 26, "Violation of postcondition" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } WEIGHT586643509 = { {STR_tag}, 26, "WEIGHT CALCULATION: self=\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } access1838956836 = { {STR_tag}, 26, "access_release_single_lock" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } access753243259 = { {STR_tag}, 26, "access_acquire_single_lock" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } obnZAL437024763 = { {STR_tag}, 26, "  obn=ZALLOC(sizeof(*ob));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } unboun1814884135 = { {STR_tag}, 26, "(*(unbound_arg0->funcptr))" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[27];
  } voidvo530295945 = { {STR_tag}, 26, "void(void) is not allowed." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } AT_ATT601389983 = { {STR_tag}, 27, "AT_ATTACH_START(at_attach);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } LM_SEL917075861 = { {STR_tag}, 27, "LM_SELECT_DISJUNCTIVE_LOCKS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } Oofite361277457 = { {STR_tag}, 27, "O: # of iters not optimized" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } Oofpre1246140551 = { {STR_tag}, 27, "O: # of prefetch statements" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } Refere2000259175 = { {STR_tag}, 27, ". (Reference objects only.)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } case1gotostate1 = { {STR_tag}, 27, "     case 1: goto state1;\n}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } insert806313390 = { {STR_tag}, 27, "insert_after_mark starting\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } isnota1029505609 = { {STR_tag}, 27, " is not a supertype of INT." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } isnota1029526908 = { {STR_tag}, 27, " is not a supertype of FLT." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } making525906926 = { {STR_tag}, 27, "making local calls if args " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } risund1670463801 = { {STR_tag}, 27, "@r is undefined for macro (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } risund16704638011 = { {STR_tag}, 27, "%r is undefined for macro (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[28];
  } srcat_1636109666 = { {STR_tag}, 27, "(src,at_attach_ptr,__res);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } LM_ACQ1821457010 = { {STR_tag}, 28, "LM_ACQUIRE_CONJUNCTIVE_LOCKS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } LM_REL1415431597 = { {STR_tag}, 28, "LM_RELEASE_DISJUNCTIVE_LOCKS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } LM_REL1765693927 = { {STR_tag}, 28, "LM_RELEASE_CONJUNCTIVE_LOCKS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } Lastst301842070 = { {STR_tag}, 28, "Last statement wasn\'t return" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } Ranoff158073728 = { {STR_tag}, 28, "Ran off unbound arg list 2nd" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } isnota1913826900 = { {STR_tag}, 28, " is not a supertype of FLTD." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } isnota1913826995 = { {STR_tag}, 28, " is not a supertype of FLTI." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } isnota1913827280 = { {STR_tag}, 28, " is not a supertype of FLTX." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } isnota1914231676 = { {STR_tag}, 28, " is not a supertype of INTI." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } main_a652001093 = { {STR_tag}, 28, " main_args->arr_part[i] = s;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } obboun2027030443 = { {STR_tag}, 28, "(*(ob->bound_arg0->funcptr))" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } ofthis353885841 = { {STR_tag}, 28, " of this bound iter call is " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } prefet1360873088 = { {STR_tag}, 28, "prefetch of expression from " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[29];
  } waitfo829569008 = { {STR_tag}, 28, "waitfor out of sync in func " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } L = { {STR_tag}, 1, "L" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } S01 = { {STR_tag}, 1, "0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } S1 = { {STR_tag}, 1, "S" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } S11 = { {STR_tag}, 1, "1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } d41 = { {STR_tag}, 1, "d" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } i21 = { {STR_tag}, 1, "i" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } name1111 = { {STR_tag}, 1, "#" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } name123 = { {STR_tag}, 1, "-" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } name152 = { {STR_tag}, 1, "|" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } name92 = { {STR_tag}, 1, "&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[2];
  } x11 = { {STR_tag}, 1, "x" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } HAS_IT200349984 = { {STR_tag}, 29, "HAS_ITER_CALL: has iter call\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } RESULT1809061834 = { {STR_tag}, 29, "RESULT OF LOCAL ANALYSIS for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } Thetyp857837218 = { {STR_tag}, 29, "The type of the destination: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } isnota2002974803 = { {STR_tag}, 29, " is not a supertype of FLTDX." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } sather1651745304 = { {STR_tag}, 29, "sather_main(main_ob,main_args" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[30];
  } workin1782648620 = { {STR_tag}, 29, "working on assign statement [" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Bofbui1943088302 = { {STR_tag}, 30, "B: # of builtin function calls" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Defini1520955740 = { {STR_tag}, 30, "Definition of main (generated)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } Noappl834681518 = { {STR_tag}, 30, "No applicable type in typecase" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } STARTI1374838211 = { {STR_tag}, 30, "STARTING PREFETCH FUNCTION on\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[31];
  } depend1236761341 = { {STR_tag}, 30, "dependency before flattening:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Fastfo578173173 = { {STR_tag}, 31, "Fast fork statement encountered" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Nomatc1537033921 = { {STR_tag}, 31, "No match for the internal call " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } OPT_LO1160315301 = { {STR_tag}, 31, "OPT_LOCAL: nothing to optimize\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } RECVFO1329722496 = { {STR_tag}, 31, ")RECVFOB((FOB)at_attach.result," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } Tempor1361444261 = { {STR_tag}, 31, "Temporary for alternate returns" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } sizeF_133617273 = { {STR_tag}, 31, "->size=F_CHARACTER_RETURN_SIZE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[32];
  } workin412447767 = { {STR_tag}, 31, "working on prefetch statement [" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } Oofbui2034740200 = { {STR_tag}, 32, "O # of builtins with near access" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } anyINT40680072 = { {STR_tag}, 32, "\'any\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } argume2028503200 = { {STR_tag}, 32, " arguments of bound closure (2)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } argume2028503219 = { {STR_tag}, 32, " arguments of bound closure (1)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } farINT1183426989 = { {STR_tag}, 32, "\'far\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } framee1759213052 = { {STR_tag}, 32, "frame->ex = GET_EXCEPTION_STACK;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } ifobnb2144963834 = { {STR_tag}, 32, " if(obn!=NULL) SYSDESTROY(obn);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } should96003773 = { {STR_tag}, 32, "should be inside of a `protect\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[33];
  } unknow2080262473 = { {STR_tag}, 32, "unknown \'@\' macro in CONFIG for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } Compil1678608282 = { {STR_tag}, 33, "Compiler error, no type for void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } FLTXli793215367 = { {STR_tag}, 33, "FLTX literals not implemented yet" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } INTIli1551441099 = { {STR_tag}, 33, "INTI literals not implemented yet" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } TAGfit851270275 = { {STR_tag}, 33, "[TAG(f->iter_frame->self)].iter)(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } Thetyp1357282984 = { {STR_tag}, 33, "The type of this local variable: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } ZALLOC946595004 = { {STR_tag}, 33, "=ZALLOC(sizeof(F_STRING_struct));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } dncann144395436 = { {STR_tag}, 33, "$n cannot be used in the string \'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } functi806347010 = { {STR_tag}, 33, "function code after prefetching:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } get_me375728054 = { {STR_tag}, 33, "  get_memory(obn,ob,sizeof(*ob));" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } hereIN194332449 = { {STR_tag}, 33, "\'here\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } ncanno888628440 = { {STR_tag}, 33, "^n cannot be used in the string \'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } nearIN473408029 = { {STR_tag}, 33, "\'near\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[34];
  } sasize845310572 = { {STR_tag}, 33, " s->asize=F_ASIZE(STR,EXCEPTION);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Compil1359403594 = { {STR_tag}, 34, "Compiler Err: PTRANS create helper" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Compil862630323 = { {STR_tag}, 34, "Compiler Error: PTRANS:link_helper" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Distri728606362 = { {STR_tag}, 34, "Distributed biters still at large." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } FLTDXl660293550 = { {STR_tag}, 34, "FLTDX literals not implemented yet" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } FLTIco1160577432 = { {STR_tag}, 34, "FLTI constants not implemented yet" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Itisil1956015094 = { {STR_tag}, 34, "It is illegal to assign to `self\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Itisil1956473526 = { {STR_tag}, 34, "It is illegal to assign to `void\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } LM_END629067339 = { {STR_tag}, 34, "LM_END_DISJUNCTIVE_LOCKS_SELECTION" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Simple1040713615 = { {STR_tag}, 34, "\n/* Simple @ expr fork handler */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } Thetyp249438757 = { {STR_tag}, 34, "The type of the `new\' expression: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } fiter_612822643 = { {STR_tag}, 34, "f->iter_frame->self = f->oncearg0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } foundc1834005908 = { {STR_tag}, 34, "found complex iter to be inlined: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } hasthe1782842755 = { {STR_tag}, 34, " has the wrong number of args for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } hereZO804547672 = { {STR_tag}, 34, "\'here\' (ZONE) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } undefI419930828 = { {STR_tag}, 34, "\n#undef IS_ITER\n#define IS_ITER 0\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } undefI419930847 = { {STR_tag}, 34, "\n#undef IS_ITER\n#define IS_ITER 1\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[35];
  } whereI1277205312 = { {STR_tag}, 34, "\'where\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } Badtyp271192016 = { {STR_tag}, 35, "Bad type of lock in lock statement." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } CompEr1768856194 = { {STR_tag}, 35, "Comp. Err:PTRANS::transf_cohort_xpr" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } Insert191735456 = { {STR_tag}, 35, "Inserting a prefetch statement for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } Theexp1372574939 = { {STR_tag}, 35, "The expression for argument number " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } intmai1559884463 = { {STR_tag}, 35, "\nint main(int argc, char *argv[]) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } localf930677623 = { {STR_tag}, 35, "local for array creation expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } ressat1960690682 = { {STR_tag}, 35, "res = sather_main(main_ob,main_args" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } struct1342450411 = { {STR_tag}, 35, " struct _func_frame *pFF=&FF_NULL;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[36];
  } whereZ1301005922 = { {STR_tag}, 35, "\'where\' (ZONE) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } BR_clu1677588829 = { {STR_tag}, 36, "(BR_cluster_t src, BR_word_t par_ptr" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } Compil532310248 = { {STR_tag}, 36, "Compiler error, ELT::impl with sig: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } Expens1195807252 = { {STR_tag}, 36, "Expensive fork statement encountered" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } Noretu151888568 = { {STR_tag}, 36, "/* No return value from iter call */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } String1518861743 = { {STR_tag}, 36, "String literals are not subtypes of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } fiter_907730610 = { {STR_tag}, 36, "f->iter_frame->self = f->bound_arg0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } global307478914 = { {STR_tag}, 36, "\'global\' (ZONE) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[37];
  } voidon2114174781 = { {STR_tag}, 36, "void() on array creation expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Boolea1401042582 = { {STR_tag}, 37, "Boolean literals are not subtypes of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } CURSOR1135707328 = { {STR_tag}, 37, "CURSOR: insert_stmt_after, inserting " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Initia354244688 = { {STR_tag}, 37, "Initialize once arguments of call to " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Oloops168062639 = { {STR_tag}, 37, "O: loops having a loop index variable" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } RESULT1262569080 = { {STR_tag}, 37, "RESULT OF LOCAL ANALYSIS for builtin " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Simple287998794 = { {STR_tag}, 37, "\n/* Simple @ expr request handler */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Thetyp1424918217 = { {STR_tag}, 37, "The type of the creation expression: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Thetyp849634942 = { {STR_tag}, 37, "The type of the `result\' expression: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } Unreco1110335422 = { {STR_tag}, 37, "Unrecognized Sather bound iter name: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } cluste1966397776 = { {STR_tag}, 37, "\'clusters\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } localf217231907 = { {STR_tag}, 37, "local for value type array assignment" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } struct167654362 = { {STR_tag}, 37, ", struct _func_frame *prev_func_frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[38];
  } struct340882784 = { {STR_tag}, 37, " struct LOCAL_MEM_struct local_mem;\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } ErrorS66315690 = { {STR_tag}, 38, "Error: Simple fork body is not a call\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Locals698775910 = { {STR_tag}, 38, ". (Locals, arguments and \'self\' only.)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Noappl1426784379 = { {STR_tag}, 38, "No applicable target in case statement" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Nocall611387935 = { {STR_tag}, 38, "No calls into partial classes allowed." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Pmustb23117515 = { {STR_tag}, 38, "@P must be followd by a digit, not by " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Pmustb231175151 = { {STR_tag}, 38, "%P must be followd by a digit, not by " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } Thetyp1640230519 = { {STR_tag}, 38, "The type of the exception expression: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } cohort228120 = { {STR_tag}, 38, "\'cohort\' (ATTACH) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[39];
  } nonato642173603 = { {STR_tag}, 38, "non atomic argument in simple at expr " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } AM = { {STR_tag}, 2, "AM" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } P = { {STR_tag}, 2, "@P" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } P1 = { {STR_tag}, 2, "%P" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S05 = { {STR_tag}, 2, ",0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S06 = { {STR_tag}, 2, ".0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S12 = { {STR_tag}, 2, "+1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S1L = { {STR_tag}, 2, "1L" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } S_1 = { {STR_tag}, 2, ":_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } b21 = { {STR_tag}, 2, "\\b" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } c311 = { {STR_tag}, 2, "c=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } dx = { {STR_tag}, 2, "dx" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } f4 = { {STR_tag}, 2, "\\f" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } f_ = { {STR_tag}, 2, "f_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } i_ = { {STR_tag}, 2, "i_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } n21 = { {STR_tag}, 2, "\\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name100 = { {STR_tag}, 2, ",&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name101 = { {STR_tag}, 2, "=(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name102 = { {STR_tag}, 2, ";}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name103 = { {STR_tag}, 2, ")?" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name108 = { {STR_tag}, 2, "*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name110 = { {STR_tag}, 2, "==" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name114 = { {STR_tag}, 2, "\\\\" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name115 = { {STR_tag}, 2, "\\\'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name116 = { {STR_tag}, 2, "\\\"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name125 = { {STR_tag}, 2, ")*" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name133 = { {STR_tag}, 2, " \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name138 = { {STR_tag}, 2, ":;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name139 = { {STR_tag}, 2, " }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name146 = { {STR_tag}, 2, "\"," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name150 = { {STR_tag}, 2, ":?" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name155 = { {STR_tag}, 2, ":=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name163 = { {STR_tag}, 2, "\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name65 = { {STR_tag}, 2, "]\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name71 = { {STR_tag}, 2, " \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name73 = { {STR_tag}, 2, "\"\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name75 = { {STR_tag}, 2, "((" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name76 = { {STR_tag}, 2, ");" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name77 = { {STR_tag}, 2, "* " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name81 = { {STR_tag}, 2, "&(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name85 = { {STR_tag}, 2, "(*" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name86 = { {STR_tag}, 2, "];" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name87 = { {STR_tag}, 2, "))" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name88 = { {STR_tag}, 2, "&&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name90 = { {STR_tag}, 2, "])" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name93 = { {STR_tag}, 2, "/*" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name94 = { {STR_tag}, 2, "*/" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name97 = { {STR_tag}, 2, " *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name98 = { {STR_tag}, 2, "{\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } name99 = { {STR_tag}, 2, "){" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } r311 = { {STR_tag}, 2, "\\r" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } t31 = { {STR_tag}, 2, "\\t" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[3];
  } v211 = { {STR_tag}, 2, "\\v" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } Charac693849448 = { {STR_tag}, 39, "Character literals are not subtypes of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } Compil163952783 = { {STR_tag}, 39, "Compiler Err:PTRANS::add_helper_to_tbls" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } Errori170665162 = { {STR_tag}, 39, "Error in Bnd iter obj (sig not found): " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } Partia2053653834 = { {STR_tag}, 39, "Partial classes cannot be instantiated." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } hasare2086446435 = { {STR_tag}, 39, " has a return value, but it isn\'t used." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[40];
  } struct682057613 = { {STR_tag}, 39, "\n         struct sather_attribute attr[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } Compil151053839 = { {STR_tag}, 40, "Compiler Error: PTRANS: transform_par: 0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } Compil1510538391 = { {STR_tag}, 40, "Compiler Error: PTRANS: transform_pa:r 0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } Oofloc443792109 = { {STR_tag}, 40, "O: # of locks not put on exception stack" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } hasnor813490359 = { {STR_tag}, 40, " has no return value, but one is needed." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } initia1858005947 = { {STR_tag}, 40, "`initial\' expressions may not be nested." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } mustno562683704 = { {STR_tag}, 40, "\' must not be a `!\' argument to an iter." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[41];
  } struct1585435604 = { {STR_tag}, 40, "\n struct _func_frame FF={ 0,NULL,NULL };" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } Callsm1564266979 = { {STR_tag}, 41, "Calls may not be made directly on `void\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } Compil1402030697 = { {STR_tag}, 41, "Compiler Error: PTRANS: transform_fork: 0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } Compil2017674967 = { {STR_tag}, 41, "Compiler error, TRANS::call_fix_iter, ir[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } Fastpa760727994 = { {STR_tag}, 41, "Fast par or parloop statement encountered" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } Inemit721671903 = { {STR_tag}, 41, "In emit_call, unclassifiable signature : " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[42];
  } voidon651986877 = { {STR_tag}, 41, "void() on create expression without type." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } BR_SET165350689 = { {STR_tag}, 42, " BR_SET_THREAD_LOCAL((void *)&local_mem);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[43];
  } nonato57333472 = { {STR_tag}, 42, "non atomic self in a simple fork statement" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Boundi1405270156 = { {STR_tag}, 43, "Bound iters must be formed from iter calls." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Callsm1636158155 = { {STR_tag}, 43, "Calls may not be made on array expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Compil608171113 = { {STR_tag}, 43, "Compiler Error: PTRANS: transform_attach: 0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Emitti760829686 = { {STR_tag}, 43, "Emitting non-psather `@\' on fork or parloop" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } F_R_AR1075403797 = { {STR_tag}, 43, " F_R_ARRAY_OBJECT_NN(STR,CHAR,s,EXCEPTION);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Oofite2134272122 = { {STR_tag}, 43, "O: # of iters not optimized and not inlined" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Thetyp1564090783 = { {STR_tag}, 43, "The type of the bound creation expression: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } Thiscr666336343 = { {STR_tag}, 43, "This creation expression returns the type: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } asizef934973982 = { {STR_tag}, 43, "asize for AVAL has to be a integer literal!" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } cluste1354019123 = { {STR_tag}, 43, "\'clusters_procs\' (INT) is not a subtype of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[44];
  } consta1495253793 = { {STR_tag}, 43, "constant array literals not implemented yet" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } CURSOR1056944728 = { {STR_tag}, 44, "CURSOR: insert_stmt_after, after inserting:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Compil372096537 = { {STR_tag}, 44, "Compiler Err:PTRANS::update_rout_in_tbls - 3" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Compil372096556 = { {STR_tag}, 44, "Compiler Err:PTRANS::update_rout_in_tbls - 2" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Compil372096575 = { {STR_tag}, 44, "Compiler Err:PTRANS::update_rout_in_tbls - 1" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Itersn1426326843 = { {STR_tag}, 44, "Iters need a surrounding loop inside `fork\'," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Itisil1921741040 = { {STR_tag}, 44, "It is illegal to assign to a string literal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Onlyca1297144567 = { {STR_tag}, 44, "Only `call!\' may be applied to a bound iter." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Should515168967 = { {STR_tag}, 44, "/* Should implement unlock checking here! */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } Trying914151009 = { {STR_tag}, 44, "Trying to insert at the following position:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } nonato2046226648 = { {STR_tag}, 44, "non atomic argument in simple fork statement" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[45];
  } return448605028 = { {STR_tag}, 44, "`return\' statements may not appear in iters." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } CURSOR570633082 = { {STR_tag}, 45, "CURSOR: insert_stmt_before, after inserting:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } Compil204150738 = { {STR_tag}, 45, "Compiler error, TRANS::call_tp_of_expr(void)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } Itisil1717990552 = { {STR_tag}, 45, "It is illegal to assign to a boolean literal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } Orexpr1074242364 = { {STR_tag}, 45, "Or expressions return BOOL objects which are " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } quitst2023839416 = { {STR_tag}, 45, "`quit\' statements may not appear in routines." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[46];
  } struct88687480 = { {STR_tag}, 45, " struct _func_frame FF_NULL={ 0,NULL,NULL };\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Andexp1703022664 = { {STR_tag}, 46, "And expressions return BOOL objects which are " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } BR_SET1034520431 = { {STR_tag}, 46, " BR_SET_THREAD_LOCAL((void *)prev_local_mem);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } CURSOR479438165 = { {STR_tag}, 46, "CURSOR: insert_stmt_before, before inserting:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Compil1718354347 = { {STR_tag}, 46, "Compiler error, TRANS::call_fix_bnd_iter, bir[" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Expens429369331 = { {STR_tag}, 46, "Expensive par or parloop statement encountered" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Itisil1021252333 = { {STR_tag}, 46, "It is illegal to assign to a `far\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Itisil1815917697 = { {STR_tag}, 46, "It is illegal to assign to an `at\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Itisil258160423 = { {STR_tag}, 46, "It is illegal to assign to an `or\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Itisil333018310 = { {STR_tag}, 46, "It is illegal to assign to a `new\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Itisil87353010 = { {STR_tag}, 46, "It is illegal to assign to an integer literal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Lhsofi493151537 = { {STR_tag}, 46, "Lhs of \'@\' in an expr statement must be a call" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Markin891501023 = { {STR_tag}, 46, "Marking full because of unknown external call\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } NEARLO1748192338 = { {STR_tag}, 46, "NEAR LOCALS (immutable locals are not shown):\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } Onlyca1521382942 = { {STR_tag}, 46, "Only `call\' may be applied to a bound routine." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } forlen1652118724 = { {STR_tag}, 46, " for (length=0; argv[i][length]!=0; length++);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } inline29312080 = { {STR_tag}, 46, "inlined_C arg something other than STR literal" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[47];
  } yields57855310 = { {STR_tag}, 46, "`yield\' statements may not appear in routines." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Ingene409399871 = { {STR_tag}, 47, "In generate_bnd_iter_stub: something wrong with" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil1048791346 = { {STR_tag}, 47, "It is illegal to assign to a where\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil1097711340 = { {STR_tag}, 47, "It is illegal to assign to a character literal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil1607700999 = { {STR_tag}, 47, "It is illegal to assign to a `here\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil1608792715 = { {STR_tag}, 47, "It is illegal to assign to a `near\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil187901097 = { {STR_tag}, 47, "It is illegal to assign to an `any\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil2136244190 = { {STR_tag}, 47, "It is illegal to assign to an `and\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Itisil578632473 = { {STR_tag}, 47, "It is illegal to assign to an array expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } Therig1406749556 = { {STR_tag}, 47, "The right hand side of `::=\' may not be `void\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } voidpp1939092441 = { {STR_tag}, 47, "{   void* pp_n = (void*)0; void* locked_lock = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[48];
  } yields1884499596 = { {STR_tag}, 47, "`yield\' statements may not appear in `protect\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } Compil1357616012 = { {STR_tag}, 48, "Compiler error, TRANS::add_local, cur_rout=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } Compil321489027 = { {STR_tag}, 48, "Compiler error, TRANS::call_self, self_val=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } Itersm2063802943 = { {STR_tag}, 48, "Iters may only be called within loop statements." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } Itisno2078445089 = { {STR_tag}, 48, "It is not possible to copy bnd iter expressions\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } NULLFA1853100247 = { {STR_tag}, 48, "==NULL) FATAL(\"Unable to allocate more memory\");" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } PAR_DE1693558485 = { {STR_tag}, 48, " PAR_DECREMENT(src,(SIMPLE_PAR_ATTACH)par_ptr);\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } PSATHE898285019 = { {STR_tag}, 48, " PSATHER_THREAD_ID_SET(*((BR_thread_t *)&tid));\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } Therea944368619 = { {STR_tag}, 48, "There are no matching branches in this typecase." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } iffram1733377389 = { {STR_tag}, 48, "if(frame->state) SET_EXCEPTION_STACK(frame->ex);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } switch81042052 = { {STR_tag}, 48, "switch (f->state) { \n     case 0: goto state0; \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[49];
  } unlock1640206679 = { {STR_tag}, 48, "\'unlock\' statement must appear inside of `lock\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } ANY1 = { {STR_tag}, 3, "ANY" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } OB3 = { {STR_tag}, 3, "OB " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S0 = { {STR_tag}, 3, ",0," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S02 = { {STR_tag}, 3, ":0)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S03 = { {STR_tag}, 3, ", 0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S04 = { {STR_tag}, 3, ")0)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S1L1 = { {STR_tag}, 3, "1L;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S___ = { {STR_tag}, 3, "___" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } S_ob1 = { {STR_tag}, 3, "_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } TAG2 = { {STR_tag}, 3, "TAG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } arg21 = { {STR_tag}, 3, "arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } do5 = { {STR_tag}, 3, "do{" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } f7 = { {STR_tag}, 3, "f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } if4 = { {STR_tag}, 3, "if(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } if5 = { {STR_tag}, 3, "if " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } in4 = { {STR_tag}, 3, "in " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name104 = { {STR_tag}, 3, "= *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name106 = { {STR_tag}, 3, "; }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name119 = { {STR_tag}, 3, ", -" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name120 = { {STR_tag}, 3, "}, " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name121 = { {STR_tag}, 3, ", \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name124 = { {STR_tag}, 3, ")+(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name126 = { {STR_tag}, 3, "))," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name127 = { {STR_tag}, 3, ");\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name128 = { {STR_tag}, 3, " []" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name130 = { {STR_tag}, 3, "==(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name134 = { {STR_tag}, 3, "(*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name136 = { {STR_tag}, 3, ": ;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name137 = { {STR_tag}, 3, "); " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name140 = { {STR_tag}, 3, " } " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name141 = { {STR_tag}, 3, "  \"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name142 = { {STR_tag}, 3, "\",\"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name143 = { {STR_tag}, 3, "\",\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name151 = { {STR_tag}, 3, " < " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name153 = { {STR_tag}, 3, " @ " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name154 = { {STR_tag}, 3, ":= " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name158 = { {STR_tag}, 3, ", \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name159 = { {STR_tag}, 3, " > " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name161 = { {STR_tag}, 3, "}\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name162 = { {STR_tag}, 3, ";\n}" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name68 = { {STR_tag}, 3, " : " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name70 = { {STR_tag}, 3, " * " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name79 = { {STR_tag}, 3, " */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name82 = { {STR_tag}, 3, ",((" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name84 = { {STR_tag}, 3, ") {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name89 = { {STR_tag}, 3, "]==" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } name95 = { {STR_tag}, 3, "\");" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[4];
  } xc11 = { {STR_tag}, 3, "x.c" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Boundr2070341545 = { {STR_tag}, 49, "Bound routines must be formed from routine calls." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Compil1396259414 = { {STR_tag}, 49, "Compiler error, TRANS::call_fix_iter, ir[0]=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Illega1957842819 = { {STR_tag}, 49, "Illegal routine is passed as argument to #F_ROUT." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil1353104731 = { {STR_tag}, 49, "It is illegal to assign to a `break!\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil1603144605 = { {STR_tag}, 49, "It is illegal to assign to a `result\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil1885405538 = { {STR_tag}, 49, "It is illegal to assign to a call with arguments." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil325536867 = { {STR_tag}, 49, "It is illegal to assign to a `cohort\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil515121697 = { {STR_tag}, 49, "It is illegal to assign to a `global\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil905536373 = { {STR_tag}, 49, "It is illegal to assign to the typecase variable." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } Itisil9134278 = { {STR_tag}, 49, "It is illegal to assign to a creation expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } aftere1148777451 = { {STR_tag}, 49, "after executing back_to_mark. Current position: \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } fstate886545549 = { {STR_tag}, 49, "f->state = (f->iter_frame->state == -1 ? -1 : 1);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[50];
  } inthis1097895141 = { {STR_tag}, 49, " in this iter call, itself contains an iter call." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Compil1779756918 = { {STR_tag}, 50, "Compiler error, TRANS::call_fix_iter, ir.fun=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Compil807929712 = { {STR_tag}, 50, "Compiler err, TRANS::transform_call_expr, el=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Compil998589335 = { {STR_tag}, 50, "Compiler err, TRANS::transform_call_expr, im=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Itersi136016895 = { {STR_tag}, 50, "Iter signature not found in : generate_bnd_iter : " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Partia838698083 = { {STR_tag}, 50, "Partial classes cannot be used as type parameters." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } Sethot404575151 = { {STR_tag}, 50, "Set hot arguments, call iter and check iter state." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } before1421983347 = { {STR_tag}, 50, "before executing back_to_mark. Current position: \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } creati960341370 = { {STR_tag}, 50, "creation expression is not of the form `ARRAY{T}\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[51];
  } signal1726310935 = { {STR_tag}, 50, "signal(SIGSEGV,(void(*)(int))rt_segfault_handler);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Compil1237838286 = { {STR_tag}, 51, "Compiler error, TRANS::call_expr_get_sig, res size." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Compil564774712 = { {STR_tag}, 51, "Compiler error, TRANS::transform_rout_elt size bug." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Compil816288381 = { {STR_tag}, 51, "Compiler error, TRANS::local_with_name, void local." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Inside1737175246 = { {STR_tag}, 51, "Inside of `par\', `parloop\', or `fork\' this `raise\' " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Itisil1384984645 = { {STR_tag}, 51, "It is illegal to assign to an `initial\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Itisil1456121986 = { {STR_tag}, 51, "It is illegal to pass `void\' as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Itisil162874430 = { {STR_tag}, 51, "It is illegal to pass `self\' as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Itisil173827897 = { {STR_tag}, 51, "It is illegal to assign to an `cluster\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Nostat1064653964 = { {STR_tag}, 51, "No statements may follow `quit\' in a statment list." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } Thislo1121838955 = { {STR_tag}, 51, "This local variable declaration is in the scope of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } becaus2028109384 = { {STR_tag}, 51, " because its init_stmt has not only AM_ASSIGN_STMT\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[52];
  } breakb816053051 = { {STR_tag}, 51, "`break!\' may not appear as a part of an expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } Compil534809471 = { {STR_tag}, 52, "Compiler error, TRANS::call_expr_get_sig, args size." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } Creati2137576634 = { {STR_tag}, 52, "Creation expressions may not specify abstract types." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } Itisil1408694951 = { {STR_tag}, 52, "It is illegal to assign to a `void\' test expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } Itisil598792827 = { {STR_tag}, 52, "It is illegal to assign to a floating point literal." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } Nostat1502650915 = { {STR_tag}, 52, "No statements may follow `raise\' in a statment list." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[53];
  } forj0j563736514 = { {STR_tag}, 52, " for (j=0;j<length;j++) s->arr_part[j] = argv[i][j];" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } BR_wor1065271233 = { {STR_tag}, 53, "(BR_word_t)&at_attach, (BR_word_t)PSATHER_THREAD_ID.t" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Compil483515429 = { {STR_tag}, 53, "Compiler error, TRANS::call_expr_check_local on void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } INTERN350222499 = { {STR_tag}, 53, "INTERNAL COMPILER ERROR. Bad type to unique_file_name" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Itisil2092671248 = { {STR_tag}, 53, "It is illegal to assign to a bound create expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Itisil815302075 = { {STR_tag}, 53, "It is illegal to assign to an `exception\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Nostat2057009120 = { {STR_tag}, 53, "No statements may follow `return\' in a statment list." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Noyiel2003661345 = { {STR_tag}, 53, "No yield value should be provided for the signature: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } Unders445425447 = { {STR_tag}, 53, "Underscore arguments may not appear in this position." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[54];
  } forane106255635 = { {STR_tag}, 53, " for an external C class  has to be a string literal!" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } Boundi238890286 = { {STR_tag}, 54, "Bound iters may only be called inside loop statements." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } Failur531548896 = { {STR_tag}, 54, "Failure to infer the type of the unbound self argument" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } Itersi1831213836 = { {STR_tag}, 54, "Iter signature not found in : emit_am_bnd_iter_create." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } Noretu261930975 = { {STR_tag}, 54, "No return value should be provided for the signature: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } Trying1677464978 = { {STR_tag}, 54, "Trying to insert a statement, but this does not work:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[55];
  } result202247682 = { {STR_tag}, 54, "`result\' expressions can only occur in `post\' clauses." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } Compil1234648972 = { {STR_tag}, 55, "Compiler error, TRANS::tranform_rout_elt given abstract" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } Oofini2113488966 = { {STR_tag}, 55, "O: # of initializations of iters moved outside the loop" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } Youcan1429763738 = { {STR_tag}, 55, "You cannot have more than 15 locks in one \'when\' branch" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } Zonesa266747214 = { {STR_tag}, 55, "Zones are not yet implemented for distributed platforms" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[56];
  } initia552865977 = { {STR_tag}, 55, "`initial\' expressions can only occur in `post\' clauses." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[57];
  } Boundc88095547 = { {STR_tag}, 56, "Bound creation calls may not be made directly on `void\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[57];
  } Itisil1765426125 = { {STR_tag}, 56, "It is illegal to assign to an `cluster_size\' expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } BR_clu1835965243 = { {STR_tag}, 57, "(BR_cluster_t src, BR_word_t at_attach_ptr, BR_word_t tid" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } addres395294157 = { {STR_tag}, 57, "->address=(F_CHARACTER *)ZALLOC(F_CHARACTER_RETURN_SIZE);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } cluste72287095 = { {STR_tag}, 57, "@cluster works currently only for routine calls, not for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } forkst2003855832 = { {STR_tag}, 57, "`fork\' statement must appear inside of `par\' or `parloop\'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } name64 = { {STR_tag}, 57, "********************************************************\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[58];
  } struct328711801 = { {STR_tag}, 57, "\nstruct { void (*cf)();long src;char *sn,*cn;int args,lc;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } Boundc1895589567 = { {STR_tag}, 58, "Bound creation calls may not be made on array expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } Compil29290199 = { {STR_tag}, 58, "Compiler error, TRANS::transform_call_expr, cur_rout=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } Expres1823750894 = { {STR_tag}, 58, "Expressions used as statements may not have return values." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } Itersm1658204009 = { {STR_tag}, 58, "Iters may not be called on the rhs of an attach statement." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } Toofew1923659380 = { {STR_tag}, 58, "Too few unbound arguments in a closure creation expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[59];
  } before837382382 = { {STR_tag}, 58, "before executing back_to_mark_or_loop. Current position: \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } FF2 = { {STR_tag}, 4, ",&FF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } HERE1 = { {STR_tag}, 4, "HERE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } NULL2 = { {STR_tag}, 4, "NULL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } POST = { {STR_tag}, 4, "POST" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S_ob = { {STR_tag}, 4, "_ob " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S_pS_ = { {STR_tag}, 4, "_pS_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S_str = { {STR_tag}, 4, "_str" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } S_tag = { {STR_tag}, 4, "_tag" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } WW = { {STR_tag}, 4, "WW: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } arg4 = { {STR_tag}, 4, " arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } arg_ = { {STR_tag}, 4, "arg_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } argc1 = { {STR_tag}, 4, "argc" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } far4 = { {STR_tag}, 4, "far(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } gate11 = { {STR_tag}, 4, "gate" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } if3 = { {STR_tag}, 4, "if (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } int1 = { {STR_tag}, 4, "int " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name105 = { {STR_tag}, 4, ")) {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name107 = { {STR_tag}, 4, " = *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name129 = { {STR_tag}, 4, "] = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name131 = { {STR_tag}, 4, "]==(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name147 = { {STR_tag}, 4, "\n  }" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name148 = { {STR_tag}, 4, "\n};\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name156 = { {STR_tag}, 4, " :- " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name157 = { {STR_tag}, 4, " -> " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name160 = { {STR_tag}, 4, "\n}\n\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name164 = { {STR_tag}, 4, ") {\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name63 = { {STR_tag}, 4, "---\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name72 = { {STR_tag}, 4, "\"*/\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name74 = { {STR_tag}, 4, "name" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name78 = { {STR_tag}, 4, " /* " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name80 = { {STR_tag}, 4, " = (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name91 = { {STR_tag}, 4, " = &" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } name96 = { {STR_tag}, 4, ")])(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } out3 = { {STR_tag}, 4, "out " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } pFF1 = { {STR_tag}, 4, ",pFF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } pre2 = { {STR_tag}, 4, "pre " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[5];
  } to1 = { {STR_tag}, 4, " to:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } Binary376592169 = { {STR_tag}, 59, "Binary copy of bound iterator object for pot. multiple use." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } Compil1546367601 = { {STR_tag}, 59, "Compiler error, TRANS::transform_assign_dec_stmt, bad type." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } Compil1678661998 = { {STR_tag}, 59, "Compiler error, TRANS::transform_attr_elt, e.sig.args=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } Tooman1400108149 = { {STR_tag}, 59, "Too many unbound arguments in a closure creation expression" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } statem1515885938 = { {STR_tag}, 59, "\':-\' statement must have  $ATTACH or $ATTACH{T} expr as lhs" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[60];
  } thefol726840669 = { {STR_tag}, 59, "the following expression is not hoistable: (need_safe_attr=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } Compil1673539783 = { {STR_tag}, 60, "Compiler Error: PTRANS: transf_with_near_stmt: cur_rout void" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } Itisil419355605 = { {STR_tag}, 60, "It is illegal to pass `or\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } LOCAL_2068279904 = { {STR_tag}, 60, " LOCAL_MEM prev_local_mem=(LOCAL_MEM)BR_GET_THREAD_LOCAL();\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } Passin247568571 = { {STR_tag}, 60, "Passing an attribute as out/inout argument: writer not found" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } Passin645655410 = { {STR_tag}, 60, "Passing array element as out/inout argument: aset not found." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } Thebac1831246981 = { {STR_tag}, 60, "The back-end encountered an improperly translated $AM_STMT: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } aoncea731344550 = { {STR_tag}, 60, "a once argument, but an iter call appears in its expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[61];
  } fprint334871173 = { {STR_tag}, 60, " fprintf(stderr,\"Uncaught STR exception: %s\\n\",s->arr_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[62];
  } Compil1435418303 = { {STR_tag}, 61, "Compiler error, TRANS::transform_shared_elt, e.sig.args=void." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[62];
  } Itisil1213919924 = { {STR_tag}, 61, "It is illegal to pass a string literal as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[62];
  } Itisil842621925 = { {STR_tag}, 61, "It is illegal to pass array expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[62];
  } result1689554699 = { {STR_tag}, 61, "`result\' expressions may not appear in `initial\' expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[62];
  } thefol1791862455 = { {STR_tag}, 61, "/* the following call should be made at some other cluster */" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[63];
  } Itisil1083364558 = { {STR_tag}, 62, "It is illegal to pass a boolean literal as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[64];
  } Itisil1114767838 = { {STR_tag}, 63, "It is illegal to pass an integer literal as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[64];
  } Itisil1936949134 = { {STR_tag}, 63, "It is illegal to pass a `new\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[64];
  } breakb17360986 = { {STR_tag}, 63, "`break!\', `while!\' and `until!\' calls must appear inside loops." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[64];
  } newexp646232754 = { {STR_tag}, 63, "`new\' expressions may only appear in reference partial classes." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[65];
  } Itisil1874953554 = { {STR_tag}, 64, "It is illegal to pass a character literal as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[65];
  } Itisil2145524732 = { {STR_tag}, 64, "It is illegal to pass an `and\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[65];
  } cohort1947905067 = { {STR_tag}, 64, "\'cohort\' must not appear outside of `par\', `parloop\', or `fork\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[65];
  } quitst277111570 = { {STR_tag}, 64, "`quit\' statements may not appear in `par\', `parloop\', or `fork\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[66];
  } Allarg773350235 = { {STR_tag}, 65, "All arguments must be left unbound in F_ROUT creation expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[66];
  } Itisil1159445535 = { {STR_tag}, 65, "It is illegal to pass a result\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[66];
  } yields560152216 = { {STR_tag}, 65, "`yield\' statements may not appear in `par\', `parloop\', or `fork\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[67];
  } Itisil100141197 = { {STR_tag}, 66, "It is illegal to pass a `break!\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[67];
  } Itisil159494944 = { {STR_tag}, 66, "It is illegal to pass a creation expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[67];
  } Itisil401642417 = { {STR_tag}, 66, "It is illegal to pass a `global\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[67];
  } return401915800 = { {STR_tag}, 66, "`return\' statements may not appear in `par\', `parloop\', or `fork\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[68];
  } Itisil205696450 = { {STR_tag}, 67, "It is illegal to pass a `cluster\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[68];
  } Itisil264579487 = { {STR_tag}, 67, "It is illegal to pass a call with arguments as out/inout parameter." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[68];
  } Itisil483697597 = { {STR_tag}, 67, "It is illegal to pass `void\' test expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[68];
  } Itisil781657708 = { {STR_tag}, 67, "It is illegal pass a bound create expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[68];
  } Oncear1787080910 = { {STR_tag}, 67, "Once arguments of iter calls may not themselves contain iter calls." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[69];
  } Itisil27243933 = { {STR_tag}, 68, "It is illegal to pass an `initial\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[69];
  } Voidte1174064897 = { {STR_tag}, 68, "Void test expressions return BOOL objects which are not subtypes of " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } ATTR1 = { {STR_tag}, 5, "ATTR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } BOOL2 = { {STR_tag}, 5, "BOOL " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } FF11 = { {STR_tag}, 5, ", &FF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } FORK_ = { {STR_tag}, 5, "FORK_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } F_TAG2 = { {STR_tag}, 5, "F_TAG" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } HERE2 = { {STR_tag}, 5, ",HERE" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } IAM = { {STR_tag}, 5, "IAM: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } ITER2 = { {STR_tag}, 5, "ITER(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } I_u_I = { {STR_tag}, 5, "I_u_I" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } NULL5 = { {STR_tag}, 5, ",NULL" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } OB1 = { {STR_tag}, 5, "((OB)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } ROUT3 = { {STR_tag}, 5, "ROUT(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } SYNC1 = { {STR_tag}, 5, "SYNC;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } S_zero = { {STR_tag}, 5, "_zero" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } TAG1 = { {STR_tag}, 5, "[TAG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } arg3 = { {STR_tag}, 5, "->arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } arg5 = { {STR_tag}, 5, "  arg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } attr3 = { {STR_tag}, 5, "attr " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } bound = { {STR_tag}, 5, "bound" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } case3 = { {STR_tag}, 5, "case " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } d0 = { {STR_tag}, 5, "$#=0;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } d31 = { {STR_tag}, 5, "$#++;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } dAM1 = { {STR_tag}, 5, "$AM: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } death = { {STR_tag}, 5, "death" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } do3 = { {STR_tag}, 5, "do { " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } else8 = { {STR_tag}, 5, "else\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } f6 = { {STR_tag}, 5, ", f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } for1 = { {STR_tag}, 5, " for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } goto1 = { {STR_tag}, 5, "goto " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } ifb = { {STR_tag}, 5, "if (!" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } is3 = { {STR_tag}, 5, " is \n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } lock3 = { {STR_tag}, 5, "lock\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } loop2 = { {STR_tag}, 5, "loop\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name109 = { {STR_tag}, 5, ") = *" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name113 = { {STR_tag}, 5, " = (*" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name118 = { {STR_tag}, 5, " = { " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name122 = { {STR_tag}, 5, "\" };\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name144 = { {STR_tag}, 5, ",\n  {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name149 = { {STR_tag}, 5, " = *(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name66 = { {STR_tag}, 5, "   - " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name69 = { {STR_tag}, 5, "   : " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } name83 = { {STR_tag}, 5, "\" */\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } near4 = { {STR_tag}, 5, "near(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } nin = { {STR_tag}, 5, "\\nin " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } or2 = { {STR_tag}, 5, " or\n\t" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } other359 = { {STR_tag}, 5, "other" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } pFF = { {STR_tag}, 5, ", pFF" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } post2 = { {STR_tag}, 5, "post " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } s111 = { {STR_tag}, 5, " s = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } self3 = { {STR_tag}, 5, "self=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } self8 = { {STR_tag}, 5, " self" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } state2 = { {STR_tag}, 5, "state" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } type3 = { {STR_tag}, 5, "type " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } void11 = { {STR_tag}, 5, "void(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } void6 = { {STR_tag}, 5, "void " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } when3 = { {STR_tag}, 5, "when " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[6];
  } with3 = { {STR_tag}, 5, "with " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[70];
  } Itisil1672895968 = { {STR_tag}, 69, "It is illegal to pass a `exception\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[70];
  } Itisil676454093 = { {STR_tag}, 69, "It is illegal to pass a floating point literal as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[70];
  } Therig416988915 = { {STR_tag}, 69, "The right hand side of `::=\' may not be an array creation expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[71];
  } Dispat1038375013 = { {STR_tag}, 70, "Dispatch on self for builtin iters should never occur (please report)." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[73];
  } Itisil1998665541 = { {STR_tag}, 72, "It is illegal to pass a `cluster_size\' expression as out/inout argument." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[73];
  } Routin1457291481 = { {STR_tag}, 72, "Routine must terminate with a `return\' statement or a `raise\' statement." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[73];
  } selfma1628479826 = { {STR_tag}, 72, "`self\' may not appear in a shared or constant initialization expression." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[75];
  } Itisil593397214 = { {STR_tag}, 74, "It is illegal to pass elements of immutable arrays as out/inout arguments." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[76];
  } Intern1789924828 = { {STR_tag}, 75, "Internal compiler error:Something wierd is going on with file name mangling" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[76];
  } Iterca2012547742 = { {STR_tag}, 75, "Iter calls may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[76];
  } Theis_1675105926 = { {STR_tag}, 75, "The `is_eq\' routine corresponding to a `case\' branch must return a boolean." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[76];
  } Thetyp1295754401 = { {STR_tag}, 75, "The type of this array creation expression cannot be inferred from context." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[76];
  } fprint1701882405 = { {STR_tag}, 75, " fprintf(stderr,\"Uncaught STR exception: %s\\n\",((STR)EXCEPTION)->arr_part);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[77];
  } Aniter1062309569 = { {STR_tag}, 76, "An iter call may not occur in a protect statement without an enclosing loop." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[77];
  } Theexp628757468 = { {STR_tag}, 76, "The expression for self in an iter call may not itself contain an iter call." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[77];
  } unlock800726374 = { {STR_tag}, 76, "\'unlock\' statement may not not appear inside of `par\', `parloop\', or `fork\'." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[78];
  } cluste1354883291 = { {STR_tag}, 77, "`clusters\' is a key word of pSather and may not be used in sequential Sather\'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[79];
  } Itisil1872847488 = { {STR_tag}, 78, "It is illegal to pass attributes of immutable objects as out/inout parameters." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } ATTRs2 = { {STR_tag}, 6, "ATTRs(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } Const = { {STR_tag}, 6, "Const " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } DONE = { {STR_tag}, 6, " DONE\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } FVOID1 = { {STR_tag}, 6, "FVOID(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } F_tag_ = { {STR_tag}, 6, "F_tag_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } RAISE1 = { {STR_tag}, 6, "RAISE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } SATTR1 = { {STR_tag}, 6, "SATTR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } STRs = { {STR_tag}, 6, "STR s;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_boxed2 = { {STR_tag}, 6, "_boxed" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_frame4 = { {STR_tag}, 6, "_frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_obob = { {STR_tag}, 6, "_ob ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_pS_at = { {STR_tag}, 6, "_pS_at" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_unbox = { {STR_tag}, 6, "_unbox" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } S_zero1 = { {STR_tag}, 6, "_zero;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } TYPE1 = { {STR_tag}, 6, "TYPE: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } ZFREE1 = { {STR_tag}, 6, "ZFREE(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } attach = { {STR_tag}, 6, "attach" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } breakb1 = { {STR_tag}, 6, "break!" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } class3 = { {STR_tag}, 6, "class " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } const3 = { {STR_tag}, 6, "const " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } dep1 = { {STR_tag}, 6, ": dep=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } do4 = { {STR_tag}, 6, "  do{\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } else6 = { {STR_tag}, 6, "else {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } else7 = { {STR_tag}, 6, "} else" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } f3 = { {STR_tag}, 6, "(*(f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } f5 = { {STR_tag}, 6, "   f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } f8 = { {STR_tag}, 6, " = f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } full1 = { {STR_tag}, 6, ", full" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } hotarg1 = { {STR_tag}, 6, "hotarg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } ifb1 = { {STR_tag}, 6, "if (!(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } inout2 = { {STR_tag}, 6, "inout " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } length1 = { {STR_tag}, 6, "length" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } line3 = { {STR_tag}, 6, "#line " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name112 = { {STR_tag}, 6, ") @@; " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name132 = { {STR_tag}, 6, " (*) (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name135 = { {STR_tag}, 6, " = (*(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } name62 = { {STR_tag}, 6, "-----\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } near5 = { {STR_tag}, 6, " near\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } noname1 = { {STR_tag}, 6, "noname" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } or3 = { {STR_tag}, 6, ") or (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } par_cl = { {STR_tag}, 6, "par_cl" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } par_ob1 = { {STR_tag}, 6, "par_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } quit3 = { {STR_tag}, 6, "quit;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } raise3 = { {STR_tag}, 6, "raise " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } self4 = { {STR_tag}, 6, " self;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } sig2 = { {STR_tag}, 6, ", sig " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } size3 = { {STR_tag}, 6, "->size" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } sync3 = { {STR_tag}, 6, "sync;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } then3 = { {STR_tag}, 6, " then\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } where3 = { {STR_tag}, 6, "where(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[7];
  } yield3 = { {STR_tag}, 6, "yield " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[80];
  } Extern977010393 = { {STR_tag}, 79, "External calls may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[80];
  } result1629600363 = { {STR_tag}, 79, "`result\' expressions may not appear in routines or iters without return values." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[82];
  } Boundi122936614 = { {STR_tag}, 81, "Bound iter calls may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[82];
  } Theexp627865128 = { {STR_tag}, 81, "The expression specifying `self\' in this iter call, itself contains an iter call." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[82];
  } cluste448825028 = { {STR_tag}, 81, "`cluster_size\' is a key word of pSather and may not be used in sequential Sather\'" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[83];
  } newexp2094845250 = { {STR_tag}, 82, "`new\' expressions may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[84];
  } Compil628753182 = { {STR_tag}, 83, "Compiler Error: PTRANS: transf_interf_attach: Cannot locate par/fork/attach routine" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[84];
  } Compil832122242 = { {STR_tag}, 83, "Compiler Error: PTRANS: transf_interf_attach: Cannot locate par/fork/attach Routine" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[85];
  } Boundr821212056 = { {STR_tag}, 84, "Bound routine calls may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[85];
  } Creati1591834403 = { {STR_tag}, 84, "Creation expressions on the right hand side of `::=\' must explicitly specify a type." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[86];
  } Creati989582580 = { {STR_tag}, 85, "Creation expressions may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[87];
  } newexp1686257053 = { {STR_tag}, 86, "`new\' expressions only take an argument in classes which have an include path to AREF." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[89];
  } except680562044 = { {STR_tag}, 88, "`exception\' expressions may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } ATTACH1 = { {STR_tag}, 7, "ATTACH(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } ATTRs1 = { {STR_tag}, 7, "=ATTRs(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } FATAL1 = { {STR_tag}, 7, "FATAL(\"" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } F_TAG1 = { {STR_tag}, 7, "[F_TAG(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } LOCKV1 = { {STR_tag}, 7, "LOCKV(&" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } NEAR1 = { {STR_tag}, 7, "+(NEAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } NULL1 = { {STR_tag}, 7, ") NULL)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } OB2 = { {STR_tag}, 7, "  ((OB)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } PARend = { {STR_tag}, 7, "PAR end" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } PS_FAR = { {STR_tag}, 7, "PS_FAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } RECVOB1 = { {STR_tag}, 7, "RECVOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } SENDOB1 = { {STR_tag}, 7, "SENDOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } STRs1 = { {STR_tag}, 7, " STR s;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_IS_EQ = { {STR_tag}, 7, "_IS_EQ(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_asize = { {STR_tag}, 7, "_asize;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_boxed1 = { {STR_tag}, 7, "_boxed," };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_frame = { {STR_tag}, 7, "_frame " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_frame3 = { {STR_tag}, 7, "_frame)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_pS_att = { {STR_tag}, 7, "_pS_att" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } S_pS_par = { {STR_tag}, 7, "_pS_par" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } Shared = { {STR_tag}, 7, "Shared " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } THREAD = { {STR_tag}, 7, "THREAD(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } and5 = { {STR_tag}, 7, ") and (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } assert3 = { {STR_tag}, 7, "assert " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } break3 = { {STR_tag}, 7, ";break;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } break5 = { {STR_tag}, 7, " break;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } else4 = { {STR_tag}, 7, "} else{" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } extern2 = { {STR_tag}, 7, "extern " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } far3 = { {STR_tag}, 7, "\n  far=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } forks = { {STR_tag}, 7, ", forks" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } frame1 = { {STR_tag}, 7, "frame->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } ifFAR = { {STR_tag}, 7, "if(FAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } ifd = { {STR_tag}, 7, "if($#>=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } iff1 = { {STR_tag}, 7, "if (f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } line2 = { {STR_tag}, 7, "\n#line " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } local1 = { {STR_tag}, 7, "local: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } memset1 = { {STR_tag}, 7, "memset(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } name117 = { {STR_tag}, 7, "];\n  } " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } return4 = { {STR_tag}, 7, "return " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } return5 = { {STR_tag}, 7, "return;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } self5 = { {STR_tag}, 7, "self = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } shared3 = { {STR_tag}, 7, "shared " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } size2 = { {STR_tag}, 7, "->size=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } size5 = { {STR_tag}, 7, "->size;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } sizeof1 = { {STR_tag}, 7, "sizeof(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } struct2 = { {STR_tag}, 7, "struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } switch1 = { {STR_tag}, 7, "switch(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } unlock2 = { {STR_tag}, 7, "unlock " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[8];
  } void9 = { {STR_tag}, 7, "(void) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[91];
  } Thiscr2087058747 = { {STR_tag}, 90, "This creation expression does not specify its type and it cannot be inferred from context." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[91];
  } errorI265231944 = { {STR_tag}, 90, "\n#error Internal Sather Compiler error: using hot argument in builtin iter initialization\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[92];
  } Boundc1055142586 = { {STR_tag}, 91, "Bound creation calls must be calls on routines or iters, not references to local variables." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[92];
  } Boundc1335562444 = { {STR_tag}, 91, "Bound creation expressions may not appear in shared or constant initialization expressions." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[93];
  } except1173160488 = { {STR_tag}, 92, "`exception\' expressions may only appear in `then\'and `else\' clauses of `protect\' statements." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[98];
  } Callsm1819579645 = { {STR_tag}, 97, "Calls may not be made on create expressions which don\'t specify the type of object being created." };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } AMfor = { {STR_tag}, 8, "\nAM for " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } BR_FORK_ = { {STR_tag}, 8, "BR_FORK_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } CLUSTERS1 = { {STR_tag}, 8, "CLUSTERS" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } Class = { {STR_tag}, 8, " - Class" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } FFfunc = { {STR_tag}, 8, "FF.func=" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } F_LENGTH1 = { {STR_tag}, 8, "F_LENGTH" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } F_ROUT3 = { {STR_tag}, 8, " F_ROUT " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } LOOP_END1 = { {STR_tag}, 8, "LOOP_END" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } NULL4 = { {STR_tag}, 8, " = NULL;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PARLOOP_ = { {STR_tag}, 8, "PARLOOP_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PAR_DEC = { {STR_tag}, 8, "PAR_DEC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PAR_END = { {STR_tag}, 8, "PAR_END(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PARbody = { {STR_tag}, 8, "PAR body" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PSR_FAR = { {STR_tag}, 8, "PSR_FAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } PS_NEAR = { {STR_tag}, 8, "PS_NEAR(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } RECVFOB2 = { {STR_tag}, 8, "RECVFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } SENDFOB1 = { {STR_tag}, 8, "SENDFOB(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } STR2 = { {STR_tag}, 8, "((STR) &" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } SYSOBEQ1 = { {STR_tag}, 8, "SYSOBEQ(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S__res = { {STR_tag}, 8, " __res;\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S__res1 = { {STR_tag}, 8, " __res =" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_boxed = { {STR_tag}, 8, "_boxed) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_frame1 = { {STR_tag}, 8, "_frame);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_frame2 = { {STR_tag}, 8, "_frame) " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_iter_ob2 = { {STR_tag}, 8, "_iter_ob" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_pS_par_ = { {STR_tag}, 8, "_pS_par_" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_struct = { {STR_tag}, 8, "_struct)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } S_struct1 = { {STR_tag}, 8, "_struct " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } TAGf = { {STR_tag}, 8, "[TAG(f->" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } TRUE3 = { {STR_tag}, 8, " = TRUE;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } VASS_LL1 = { {STR_tag}, 8, "VASS_LL(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } VASS_LP1 = { {STR_tag}, 8, "VASS_LP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } VASS_PL1 = { {STR_tag}, 8, "VASS_PL(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } VASS_PP1 = { {STR_tag}, 8, "VASS_PP(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } WEIGHT = { {STR_tag}, 8, "WEIGHT: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } ZALLOC1 = { {STR_tag}, 8, "=ZALLOC(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } abort1 = { {STR_tag}, 8, "abort();" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } args3 = { {STR_tag}, 8, "   args:" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } break4 = { {STR_tag}, 8, "; break;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } call2 = { {STR_tag}, 8, "->call))" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } clusterb = { {STR_tag}, 8, "cluster!" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } create2 = { {STR_tag}, 8, "::create" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } dummy11 = { {STR_tag}, 8, "dummy = " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } else5 = { {STR_tag}, 8, "} else {" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } endif2 = { {STR_tag}, 8, "\n#endif\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } frame2 = { {STR_tag}, 8, " - frame" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } goto2 = { {STR_tag}, 8, "); goto " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } hotarg = { {STR_tag}, 8, "->hotarg" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } include4 = { {STR_tag}, 8, "include " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } initial3 = { {STR_tag}, 8, "initial(" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } iter3 = { {STR_tag}, 8, ")].iter)" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } locals2 = { {STR_tag}, 8, "locals: " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } oncearg0 = { {STR_tag}, 8, "oncearg0" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } par3 = { {STR_tag}, 8, "-- par:\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } private3 = { {STR_tag}, 8, "private " };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } protect2 = { {STR_tag}, 8, "protect\n" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } size4 = { {STR_tag}, 8, "->size);" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } state02 = { {STR_tag}, 8, "state0:;" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } switch2 = { {STR_tag}, 8, "switch (" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } unsafe = { {STR_tag}, 8, ", unsafe" };
struct {
 OB_HEADER header;
 INT asize;
 CHAR arr_part[9];
  } weight1 = { {STR_tag}, 8, "\nweight=" };

