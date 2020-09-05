#include "sather.h"

/* Layouts */

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

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

typedef struct dELTAB597459657_struct {
 OB_HEADER header;
 } *dELTAB597459657;

typedef struct dELTAM1766315002_struct {
 OB_HEADER header;
 } *dELTAM1766315002;

typedef struct dELTAM1208385872_struct {
 OB_HEADER header;
 } *dELTAM1208385872;

typedef struct dELTTU186609637_struct {
 OB_HEADER header;
 } *dELTTU186609637;

typedef struct dPARSE_struct {
 OB_HEADER header;
 } *dPARSE;

typedef struct dPROG_ERR_struct {
 OB_HEADER header;
 } *dPROG_ERR;

typedef struct dSTR_struct {
 OB_HEADER header;
 } *dSTR;

typedef struct dTP_struct {
 OB_HEADER header;
 } *dTP;

typedef struct ABSTRACT_LAYOUT_struct {/* layout for ABSTRACT_LAYOUT */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct FLISTdLAYOUT_struct *dependencies;
 struct PROG_struct *prog;
 STR str;
 } *ABSTRACT_LAYOUT;

typedef struct ARRAYdTP_struct {/* layout for ARRAY{$TP} */
 OB_HEADER header;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *ARRAYdTP;

typedef struct ARRAYd2147017943_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd2147017943_frame;

typedef struct ARRAYd1730019671_frame_struct {
 INT state;
 ARRAYdTP self;/* Formal argument: self */
 dTP arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1730019671_frame;

typedef struct ARRAYARG_struct {/* layout for ARRAY{ARG} */
 OB_HEADER header;
 INT asize;
 struct ARG_struct *arr_part[1];
 } *ARRAYARG;

typedef struct BFILE_struct {/* layout for BFILE */
 OB_HEADER header;
 EXT_OB fp;
 } *BFILE;

typedef struct ELT_AL1287585232_struct {/* layout for ELT_ALG{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1287585232;

typedef struct ELT_AL1248934552_struct {/* layout for ELT_ALG{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL1248934552;

typedef struct ELT_AL138844101_struct {/* layout for ELT_ALG{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL138844101;

typedef struct ELT_AL629085806_struct {/* layout for ELT_ALG{TUP{TP_CLASS,FSET{TP_CLASS}}} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_AL629085806;

typedef struct ELT_EQ947319992_struct {/* layout for ELT_EQ{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ947319992;

typedef struct ELT_NI1411991819_struct {/* layout for ELT_NIL{AS_CLASS_DEF} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1411991819;

typedef struct ELT_NILTP_CLASS_struct {/* layout for ELT_NIL{TP_CLASS} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILTP_CLASS;

typedef struct ERR1_struct {/* layout for ERR */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ERR1;

typedef struct FILE1_struct {/* layout for FILE */
 OB_HEADER header;
 EXT_OB fp;
 } *FILE1;

typedef struct FIND_GENERICS_struct {/* layout for FIND_GENERICS */
 OB_HEADER header;
 struct FSETTP_CLASS_struct *tp_todo;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *generic_class;
 struct TP_CONTEXT_struct *con;
 } *FIND_GENERICS;

typedef struct FIND_TYPES_struct {/* layout for FIND_TYPES */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CONTEXT_struct *con;
 } *FIND_TYPES;

typedef struct IDENT_struct {/* layout for IDENT */
 STR str;
 } IDENT;
static IDENT IDENT_zero;

typedef struct IDENT_boxed_struct {
 OB_HEADER header;
 IDENT immutable_part;
 } *IDENT_boxed;

typedef struct INT_upbrINT_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I; /* used by builtin iter */
 } *INT_upbrINT_frame;

typedef struct INT_up418511718_frame_struct {
 INT state;
 INT self;/* Formal argument: self */
 INT arg1;/* Formal argument: i */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_I,I_u_Im; /* used by builtin iter */
 } *INT_up418511718_frame;

typedef struct OUT_struct {/* layout for OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *OUT;

typedef struct PROG_AS_TBL_struct {/* layout for PROG_AS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct AS_CLASS_DEF_struct *arr_part[1];
 } *PROG_AS_TBL;

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

typedef struct STR_se405450305_frame_struct {
 INT state;
 STR self;/* Formal argument: self */
 dSTR arg1;/* Formal argument: s */
 STR ret_val1;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 } *STR_se405450305_frame;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct TP_CLASS_struct {/* layout for TP_CLASS */
 OB_HEADER header;
 struct ARRAYdTP_struct *params;
 struct AS_CLASS_DEF_struct *my_as_tree;
 struct IDENT_struct name1;
 struct IFC_struct *my_ifc;
 struct IMPL_struct *my_impl;
 INT kind_cache;
 struct PROG_struct *prog;
 STR sas_cache;
 struct TP_CONTEXT_struct *my_context;
 BOOL is_atomic;
 BOOL is_builtin;
 BOOL is_dummy_class;
 BOOL is_generic;
 } *TP_CLASS;

typedef struct TP_CLASS_TBL_struct {/* layout for TP_CLASS_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_CLASS_TBL;

typedef struct TP_CLA115798516_frame_struct {
 INT state;
 TP_CLASS_TBL self;/* Formal argument: self */
 TP_CLASS ret_val2;
 TP_CLASS_TBL L61;
 TP_CLASS r;
 INT L31;
 TP_CLASS_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_CLA115798516_frame;

typedef struct TP_CONTEXT_struct {/* layout for TP_CONTEXT */
 OB_HEADER header;
 struct ARRAYdTP_struct *ptypes;
 struct ARRAYIDENT_struct *pnames;
 struct PROG_struct *prog;
 struct TP_CLASS_struct *same1;
 BOOL is_abs;
 } *TP_CONTEXT;

typedef struct TP_GEN_TBL_struct {/* layout for TP_GEN_TBL */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *TP_GEN_TBL;

typedef struct TP_GEN1057619759_frame_struct {
 INT state;
 TP_GEN_TBL self;/* Formal argument: self */
 TP_CLASS ret_val2;
 TP_GEN_TBL L61;
 TP_CLASS r;
 INT L31;
 TP_GEN_TBL is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *TP_GEN1057619759_frame;

typedef struct TP_ITER_struct {/* layout for TP_ITER */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct ARRAYARG_struct *args;
 struct ARRAYBOOL_struct *hot;
 struct IFC_struct *my_ifc;
 struct PROG_struct *prog;
 STR sas_cache;
 BOOL is_generic;
 } *TP_ITER;

typedef struct TP_ROUT_struct {/* layout for TP_ROUT */
 OB_HEADER header;
 struct dTP_struct *ret;
 struct ARRAYARG_struct *args;
 struct IFC_struct *my_ifc;
 struct PROG_struct *prog;
 STR sas_cache;
 BOOL is_fortran;
 BOOL is_generic;
 BOOL is_remote;
 } *TP_ROUT;

typedef struct TP_TBL_struct {/* layout for TP_TBL */
 OB_HEADER header;
 struct PROG_struct *prog;
 struct TP_CLASS_TBL_struct *class_tbl;
 struct TP_CLASS_TBL_struct *included_classes;
 struct TP_GEN_TBL_struct *gen_tbl;
 struct TP_ITER_TBL_struct *iter_tbl;
 struct TP_ROUT_TBL_struct *f_rout_tbl;
 struct TP_ROUT_TBL_struct *rout_tbl;
 } *TP_TBL;

typedef struct TUPIDENTINT_struct {/* layout for TUP{IDENT,INT} */
 struct IDENT_struct t1;
 INT t2;
 } TUPIDENTINT;
static TUPIDENTINT TUPIDENTINT_zero;

typedef struct TUPIDENTINT_boxed_struct {
 OB_HEADER header;
 TUPIDENTINT immutable_part;
 } *TUPIDENTINT_boxed;

typedef struct TUPTP_1668454172_struct {/* layout for TUP{TP_CLASS,FSET{TP_CLASS}} */
 struct FSETTP_CLASS_struct *t2;
 struct TP_CLASS_struct *t1;
 } TUPTP_1668454172;
static TUPTP_1668454172 TUPTP_1668454172_zero;

typedef struct TUPTP_1668454172_boxed_struct {
 OB_HEADER header;
 TUPTP_1668454172 immutable_part;
 } *TUPTP_1668454172_boxed;

typedef struct dELTTU963535579_frame_struct {
 INT state;
 dELTTU186609637 self;
 } *dELTTU963535579_frame;

/* Dispatch table entry for $ELT{TUP{TP_CLASS,FSET{TP_CLASS}}}::elt!:TUP{TP_CLASS,FSET{TP_CLASS}} */
typedef struct dELTTU963535579_entry {
const TUPTP_1668454172 (*iter)(dELTTU963535579_frame frame);
const void* (*alloc_frame)();
} dELTTU963535579_entry;

typedef struct AM_ANY_EXPR_struct {/* layout for AM_ANY_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_ANY_EXPR;

typedef struct AM_ATTR_EXPR_struct {/* layout for AM_ATTR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dTP_struct *self_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 BOOL secure1;
 } *AM_ATTR_EXPR;

typedef struct AM_AT_EXPR_struct {/* layout for AM_AT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *e;
 struct SFILE_ID_struct source1;
 } *AM_AT_EXPR;

typedef struct AM_BND1124877163_struct {/* layout for AM_BND_CREATE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct ARRAYINT_struct *bnd_args;
 struct ARRAYINT_struct *unbnd_args;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 STR clst;
 BOOL is_remote;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND1124877163;

typedef struct AM_BND367211769_struct {/* layout for AM_BND_ITER_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *bi;
 struct dAM_STMT_struct *init;
 struct AM_LOOP_STMT_struct *lp;
 struct SFILE_ID_struct source1;
 struct TP_ITER_struct *bi_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND367211769;

typedef struct AM_CLUSTER_EXPR_struct {/* layout for AM_CLUSTER_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLUSTER_EXPR;

typedef struct AM_CLU1286269335_struct {/* layout for AM_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLU1286269335;

typedef struct AM_CONST_struct {/* layout for AM_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_CONST;

typedef struct AM_EXCEPT_EXPR_struct {/* layout for AM_EXCEPT_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_EXCEPT_EXPR;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

typedef struct AM_FLTI_CONST_struct {/* layout for AM_FLTI_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTI_CONST;

typedef struct AM_GLOBAL_EXPR_struct {/* layout for AM_GLOBAL_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_GLOBAL_EXPR;

typedef struct AM_HERE_EXPR_struct {/* layout for AM_HERE_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_HERE_EXPR;

typedef struct AM_IF_EXPR_struct {/* layout for AM_IF_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *if_false;
 struct dAM_EXPR_struct *if_true;
 struct dAM_EXPR_struct *test1;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IF_EXPR;

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

typedef struct AM_LOCAL_EXPR_struct {/* layout for AM_LOCAL_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL check_near;
 BOOL is_always_near;
 BOOL is_hot;
 BOOL is_volatile;
 BOOL needs_init;
 BOOL no_assign;
 BOOL not_in_frame;
 } *AM_LOCAL_EXPR;

typedef struct AM_NEAR_EXPR_struct {/* layout for AM_NEAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_NEAR_EXPR;

typedef struct AM_NEW_EXPR_struct {/* layout for AM_NEW_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *asz;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_NEW_EXPR;

typedef struct AM_SHARED_EXPR_struct {/* layout for AM_SHARED_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *init;
 struct dTP_struct *class_tp;
 struct dTP_struct *tp_at;
 struct AS_TYPE_SPEC_struct *as_type;
 struct IDENT_struct name1;
 struct SFILE_ID_struct source1;
 BOOL is_const;
 } *AM_SHARED_EXPR;

typedef struct AM_STMT_EXPR_struct {/* layout for AM_STMT_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dAM_EXPR_struct *replaced;
 struct dAM_STMT_struct *stmts;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTA725283029_struct *locals1;
 struct SFILE_ID_struct source1;
 } *AM_STMT_EXPR;

typedef struct AM_VAR744470097_struct {/* layout for AM_VARR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ind;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_VAR744470097;

typedef struct AM_VAT319982528_struct {/* layout for AM_VATTR_ASSIGN_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *ob;
 struct dAM_EXPR_struct *val1;
 struct dTP_struct *real_tp;
 struct IDENT_struct at;
 struct SFILE_ID_struct source1;
 } *AM_VAT319982528;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

typedef struct ARG_struct {/* layout for ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct dTP_struct *tp;
 BOOL issame;
 } *ARG;

typedef struct ARRAYIDENT_struct {/* layout for ARRAY{IDENT} */
 OB_HEADER header;
 INT asize;
 struct IDENT_struct arr_part[1];
 } *ARRAYIDENT;

typedef struct ARRAYI1177186037_frame_struct {
 INT state;
 ARRAYIDENT self;/* Formal argument: self */
 IDENT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYI1177186037_frame;

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

typedef struct AS_INC1416917001_struct {/* layout for AS_INCLUDE_CLAUSE */
 OB_HEADER header;
 struct dAS_CLASS_ELT_struct *next;
 struct AS_FEAT_MOD_struct *mods;
 struct AS_TYPE_SPEC_struct *tp;
 struct SFILE_ID_struct source1;
 BOOL is_private;
 } *AS_INC1416917001;

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

typedef struct AS_UNLOCK_STMT_struct {/* layout for AS_UNLOCK_STMT */
 OB_HEADER header;
 struct dAS_EXPR_struct *e;
 struct dAS_STMT_struct *next;
 struct AS_STMT_LIST_struct *surr_stmt_list;
 struct SFILE_ID_struct source1;
 BOOL transformed;
 } *AS_UNLOCK_STMT;

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

typedef struct FLISTA162798263_struct {/* layout for FLIST{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FLISTA162798263;

typedef struct FLISTA1252869158_frame_struct {
 INT state;
 FLISTA162798263 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val1;
 INT i;
 INT sz;
 FLISTA162798263 aget_self;
 INT aget_ind;
 ABSTRACT_LAYOUT ret_val;
 } *FLISTA1252869158_frame;

typedef struct FLISTA1893461511_struct {/* layout for FLIST{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FLISTA1893461511;

typedef struct FLISTA609756551_frame_struct {
 INT state;
 FLISTA1893461511 self;/* Formal argument: self */
 AM_BND1124877163 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA609756551_frame;

typedef struct FLISTA273121893_frame_struct {
 INT state;
 FLISTA1893461511 self;/* Formal argument: self */
 AM_BND1124877163 ret_val1;
 INT i;
 INT sz;
 FLISTA1893461511 aget_self;
 INT aget_ind;
 AM_BND1124877163 ret_val;
 } *FLISTA273121893_frame;

typedef struct FLISTA1409846210_struct {/* layout for FLIST{AM_BND_ITER_CALL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_BND367211769_struct *arr_part[1];
 } *FLISTA1409846210;

typedef struct FLISTA2015944564_frame_struct {
 INT state;
 FLISTA1409846210 self;/* Formal argument: self */
 AM_BND367211769 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FLISTA2015944564_frame;

typedef struct FLISTA1446310868_frame_struct {
 INT state;
 FLISTA1409846210 self;/* Formal argument: self */
 AM_BND367211769 ret_val1;
 INT i;
 INT sz;
 FLISTA1409846210 aget_self;
 INT aget_ind;
 AM_BND367211769 ret_val;
 } *FLISTA1446310868_frame;

typedef struct FLISTA725283029_struct {/* layout for FLIST{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FLISTA725283029;

typedef struct FLISTTUPIDENTINT_struct {/* layout for FLIST{TUP{IDENT,INT}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct TUPIDENTINT_struct arr_part[1];
 } *FLISTTUPIDENTINT;

typedef struct FLISTT97637998_frame_struct {
 INT state;
 FLISTTUPIDENTINT self;/* Formal argument: self */
 TUPIDENTINT ret_val1;
 INT i;
 INT sz;
 FLISTTUPIDENTINT aget_self;
 INT aget_ind;
 TUPIDENTINT ret_val;
 } *FLISTT97637998_frame;

typedef struct FSETdTP_struct {/* layout for FSET{$TP} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct dTP_struct *arr_part[1];
 } *FSETdTP;

typedef struct FSETTP_CLASS_struct {/* layout for FSET{TP_CLASS} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct TP_CLASS_struct *arr_part[1];
 } *FSETTP_CLASS;

typedef struct FSETTP340014621_frame_struct {
 INT state;
 FSETTP_CLASS self;/* Formal argument: self */
 TP_CLASS ret_val2;
 FSETTP_CLASS L61;
 TP_CLASS r;
 INT i;
 INT sz;
 INT L31;
 FSETTP_CLASS is_elt_nil_self;
 TP_CLASS is_elt_nil_e;
 BOOL ret_val;
 ELT_NILTP_CLASS is_elt_nil_self1;
 TP_CLASS is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETTP340014621_frame;

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

typedef struct PROG_A1283019219_frame_struct {
 INT state;
 PROG_AS_TBL self;/* Formal argument: self */
 AS_CLASS_DEF ret_val2;
 PROG_AS_TBL L61;
 AS_CLASS_DEF r;
 INT L31;
 PROG_AS_TBL is_elt_nil_self;
 AS_CLASS_DEF is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1411991819 is_elt_nil_self1;
 AS_CLASS_DEF is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *PROG_A1283019219_frame;

#include "tags.h"

/* Globals */

extern FLISTTUPIDENTINT FIND_G1866376849;
extern FLISTTUPIDENTINT FIND_T1407637984;
extern TP_CLASS TP_BUILTIN_bool;
extern TP_CLASS TP_BUILTIN_int;
extern TP_CLASS TP_BUILTIN_zone;

/* Function declarations */


extern RETURNED_CONST AS_CLASS_DEF (**dPARSE1581792481)(dPARSE, IDENT, INT);

extern RETURNED_CONST BOOL (**dAM_EX1117610980)(dAM_EXPR);

extern RETURNED_CONST BOOL (**dAM_EX2004971809)(dAM_EXPR, OB);

extern RETURNED_CONST BOOL (**dTP_is1334578382)(dTP);

extern RETURNED_CONST BOOL (**dTP_is242312711)(dTP, OB);

extern RETURNED_CONST FLISTA725283029 (**dAM_EX1656723676)(dAM_EXPR);

extern RETURNED_CONST INT (**dAM_EX1001627434)(dAM_EXPR);

extern RETURNED_CONST INT (**dTP_hashrINT)(dTP);

extern RETURNED_CONST STR (**dSTR_strrSTR)(dSTR);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_EXPR (**dAM_EX1830308833)(dAM_EXPR);

extern RETURNED_CONST dAM_STMT (**dAM_ST1032597839)(dAM_STMT);

extern RETURNED_CONST dELTTU963535579_entry *dELTTU963535579;

extern RETURNED_CONST dTP (**dAM_EXPR_tprdTP)(dAM_EXPR);
ABSTRACT_LAYOUT FLISTA1252869158(FLISTA1252869158_frame);
AM_ANY_EXPR AM_ANY934244480(AM_ANY_EXPR);
AM_ATTR_EXPR AM_ATT1705760219(AM_ATTR_EXPR);
AM_AT_EXPR AM_AT_1135400775(AM_AT_EXPR);
AM_BND1124877163 FLISTA1230733197(FLISTA1893461511);
AM_BND1124877163 FLISTA273121893(FLISTA273121893_frame);
AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame);
AM_CLU1286269335 AM_CLU779855221(AM_CLU1286269335);
AM_CLUSTER_EXPR AM_CLU1577744436(AM_CLUSTER_EXPR);
AM_CONST AM_CON582703191(AM_CONST);
AM_EXCEPT_EXPR AM_EXC449573271(AM_EXCEPT_EXPR);
AM_FAR_EXPR AM_FAR755578228(AM_FAR_EXPR);
AM_FLTI_CONST AM_FLT425117946(AM_FLTI_CONST);
AM_GLOBAL_EXPR AM_GLO928972965(AM_GLOBAL_EXPR);
AM_HERE_EXPR AM_HER1301311031(AM_HERE_EXPR);
AM_IF_EXPR AM_IF_1266575443(AM_IF_EXPR);
AM_IS_VOID_EXPR AM_IS_193101840(AM_IS_VOID_EXPR);
AM_LOCAL_EXPR AM_LOC1798202130(AM_LOCAL_EXPR);
AM_NEAR_EXPR AM_NEA1086037759(AM_NEAR_EXPR);
AM_NEW_EXPR AM_NEW1250698656(AM_NEW_EXPR);
AM_SHARED_EXPR AM_SHA1035965785(AM_SHARED_EXPR);
AM_STMT_EXPR AM_STM416210327(AM_STMT_EXPR);
AM_VAR744470097 AM_VAR1906690564(AM_VAR744470097);
AM_VAT319982528 AM_VAT2137026865(AM_VAT319982528);
AM_WHERE_EXPR AM_WHE1264983710(AM_WHERE_EXPR);
AS_CLASS_DEF PROG_A1283019219(PROG_A1283019219_frame);
BOOL AM_ANY1687381320(AM_ANY_EXPR, OB);
BOOL AM_ANY309199231(AM_ANY_EXPR);
BOOL AM_ATT1582125658(AM_ATTR_EXPR);
BOOL AM_ATT1818896075(AM_ATTR_EXPR, OB);
BOOL AM_AT_1010480980(AM_AT_EXPR, OB);
BOOL AM_AT_1425681295(AM_AT_EXPR);
BOOL AM_CLU1159843664(AM_CLU1286269335, OB);
BOOL AM_CLU170440611(AM_CLU1286269335);
BOOL AM_CLU528836536(AM_CLUSTER_EXPR, OB);
BOOL AM_CLU862886673(AM_CLUSTER_EXPR);
BOOL AM_CON174643192(AM_CONST);
BOOL AM_CON357292637(AM_CONST, OB);
BOOL AM_EXC219044661(AM_EXCEPT_EXPR);
BOOL AM_EXC30103808(AM_EXCEPT_EXPR, OB);
BOOL AM_FAR1472604699(AM_FAR_EXPR, OB);
BOOL AM_FAR833547686(AM_FAR_EXPR);
BOOL AM_FLT1011996250(AM_FLTI_CONST, OB);
BOOL AM_FLT840430235(AM_FLTI_CONST);
BOOL AM_GLO1300832959(AM_GLOBAL_EXPR);
BOOL AM_GLO286344550(AM_GLOBAL_EXPR, OB);
BOOL AM_HER1213808373(AM_HERE_EXPR);
BOOL AM_HER1833766706(AM_HERE_EXPR, OB);
BOOL AM_IF_2781110(AM_IF_EXPR, OB);
BOOL AM_IF_792326817(AM_IF_EXPR);
BOOL AM_IS_1001428606(AM_IS_VOID_EXPR);
BOOL AM_IS_997133203(AM_IS_VOID_EXPR, OB);
BOOL AM_LOC2055616863(AM_LOCAL_EXPR);
BOOL AM_LOC783953200(AM_LOCAL_EXPR, OB);
BOOL AM_NEA1492883953(AM_NEAR_EXPR);
BOOL AM_NEA499164018(AM_NEAR_EXPR, OB);
BOOL AM_NEW1485150288(AM_NEW_EXPR, OB);
BOOL AM_NEW952487097(AM_NEW_EXPR);
BOOL AM_SHA1922018909(AM_SHARED_EXPR);
BOOL AM_SHA623484008(AM_SHARED_EXPR, OB);
BOOL AM_STM214668158(AM_STMT_EXPR, OB);
BOOL AM_STM552683579(AM_STMT_EXPR);
BOOL AM_VAR1818998713(AM_VAR744470097, OB);
BOOL AM_VAR394665606(AM_VAR744470097);
BOOL AM_VAT2140838301(AM_VAT319982528, OB);
BOOL AM_VAT239070856(AM_VAT319982528);
BOOL AM_WHE332796028(AM_WHERE_EXPR, OB);
BOOL AM_WHE420295477(AM_WHERE_EXPR);
BOOL FLISTA1522230041(FLISTA1893461511, AM_BND1124877163);
BOOL FLISTT1340999558(FLISTTUPIDENTINT, TUPIDENTINT);
BOOL FSETdT1764379880(FSETdTP, dTP);
BOOL STR_is111530248(STR, STR);
BOOL TP_TBL663685613(TP_TBL, dTP);
FLISTA1409846210 FLISTA1236996208(FLISTA1409846210, FLISTA1409846210);
FLISTA1409846210 FLISTA157313261(FLISTA1409846210, INT);
FLISTA1409846210 FLISTA1715286767(FLISTA1409846210, AM_BND367211769);
FLISTA1409846210 FLISTA681914133(FLISTA1409846210, INT);
FLISTA1893461511 FLISTA1412425582(FLISTA1893461511, AM_BND1124877163);
FLISTA1893461511 FLISTA226859063(FLISTA1893461511, FLISTA1893461511);
FLISTA1893461511 FLISTA359657848(FLISTA1893461511);
FLISTA1893461511 FLISTA80425519(FLISTA1893461511, AM_BND1124877163);
FLISTA725283029 AM_ANY1678367917(AM_ANY_EXPR);
FLISTA725283029 AM_ATT2085324530(AM_ATTR_EXPR);
FLISTA725283029 AM_AT_726422231(AM_AT_EXPR);
FLISTA725283029 AM_CLU1812304013(AM_CLUSTER_EXPR);
FLISTA725283029 AM_CLU39726141(AM_CLU1286269335);
FLISTA725283029 AM_CON19269408(AM_CONST);
FLISTA725283029 AM_EXC1481209187(AM_EXCEPT_EXPR);
FLISTA725283029 AM_FAR276305130(AM_FAR_EXPR);
FLISTA725283029 AM_FLT523420577(AM_FLTI_CONST);
FLISTA725283029 AM_GLO404411817(AM_GLOBAL_EXPR);
FLISTA725283029 AM_HER1483923803(AM_HERE_EXPR);
FLISTA725283029 AM_IF_1267236903(AM_IF_EXPR);
FLISTA725283029 AM_IS_1111682258(AM_IS_VOID_EXPR);
FLISTA725283029 AM_LOC1121251403(AM_LOCAL_EXPR);
FLISTA725283029 AM_NEA1654213825(AM_NEAR_EXPR);
FLISTA725283029 AM_NEW1679302875(AM_NEW_EXPR);
FLISTA725283029 AM_SHA1496045189(AM_SHARED_EXPR);
FLISTA725283029 AM_STM1543836917(AM_STMT_EXPR);
FLISTA725283029 AM_VAR567579326(AM_VAR744470097);
FLISTA725283029 AM_VAT467614272(AM_VAT319982528);
FLISTA725283029 AM_WHE1419632471(AM_WHERE_EXPR);
FLISTA725283029 FLISTA1817671564(FLISTA725283029, AM_LOCAL_EXPR);
FLISTA725283029 FLISTA189055531(FLISTA725283029, FLISTA725283029);
FLISTTUPIDENTINT FLISTT1610849855(FLISTTUPIDENTINT, TUPIDENTINT);
FSETTP_CLASS FSETTP1186813482(FSETTP_CLASS, TP_CLASS);
FSETdTP FSETdT1016814448(FSETdTP, dTP);
FSTR BFILE_fstrrFSTR(BFILE);
FSTR FILE_g1986509982(FILE1);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_c1998740379(FSTR, INT);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR FSTR_p399773021(FSTR, CHAR);
FSTR INT_st367594495(INT, FSTR, INT, INT, CHAR);
IDENT IDENT_1150413730(IDENT, STR);
INT AM_ANY1965823461(AM_ANY_EXPR);
INT AM_ATT178000012(AM_ATTR_EXPR);
INT AM_AT_373259037(AM_AT_EXPR);
INT AM_CLU772455521(AM_CLU1286269335);
INT AM_CLU978087499(AM_CLUSTER_EXPR);
INT AM_CON2051977914(AM_CONST);
INT AM_EXC800301049(AM_EXCEPT_EXPR);
INT AM_FAR2123150608(AM_FAR_EXPR);
INT AM_FLT153804005(AM_FLTI_CONST);
INT AM_GLO57248015(AM_GLOBAL_EXPR);
INT AM_HER774694957(AM_HERE_EXPR);
INT AM_IF_1573308145(AM_IF_EXPR);
INT AM_IS_128338200(AM_IS_VOID_EXPR);
INT AM_LOC505223029(AM_LOCAL_EXPR);
INT AM_NEA1314295025(AM_NEAR_EXPR);
INT AM_NEW1014811437(AM_NEW_EXPR);
INT AM_SHA1840537425(AM_SHARED_EXPR);
INT AM_STM804699229(AM_STMT_EXPR);
INT AM_VAR2089448132(AM_VAR744470097);
INT AM_VAT495261450(AM_VAT319982528);
INT AM_WHE1598364639(AM_WHERE_EXPR);
INT AS_TYP2145685956(AS_TYPE_SPEC);
INT FLISTA2089597941(FLISTA1409846210);
INT FLISTA919540447(FLISTA1893461511);
INT FSTR_sizerINT(FSTR);
INT INT_hashrINT(INT);
INT INT_up418511718(INT_up418511718_frame);
INT INT_upbrINT(INT_upbrINT_frame);
INT STR_lengthrINT(STR);
SFILE_ID AM_ANY312639639(AM_ANY_EXPR);
SFILE_ID AM_ATT1483547331(AM_ATTR_EXPR);
SFILE_ID AM_AT_336435086(AM_AT_EXPR);
SFILE_ID AM_CLU1154173259(AM_CLUSTER_EXPR);
SFILE_ID AM_CLU2049939620(AM_CLU1286269335);
SFILE_ID AM_CON810758495(AM_CONST);
SFILE_ID AM_EXC2040160518(AM_EXCEPT_EXPR);
SFILE_ID AM_FAR526851136(AM_FAR_EXPR);
SFILE_ID AM_FLT564073323(AM_FLTI_CONST);
SFILE_ID AM_GLO537255272(AM_GLOBAL_EXPR);
SFILE_ID AM_HER1201005342(AM_HERE_EXPR);
SFILE_ID AM_IF_1629993260(AM_IF_EXPR);
SFILE_ID AM_IS_82827228(AM_IS_VOID_EXPR);
SFILE_ID AM_LOC327374505(AM_LOCAL_EXPR);
SFILE_ID AM_NEA1717951158(AM_NEAR_EXPR);
SFILE_ID AM_NEW765217327(AM_NEW_EXPR);
SFILE_ID AM_SHA644128690(AM_SHARED_EXPR);
SFILE_ID AM_STM1466562094(AM_STMT_EXPR);
SFILE_ID AM_VAR1884071700(AM_VAR744470097);
SFILE_ID AM_VAT338387515(AM_VAT319982528);
SFILE_ID AM_WHE345322829(AM_WHERE_EXPR);
STR ELT_AL1147639445(ELT_AL1248934552, dELTAM1766315002);
STR ELT_AL1351346800(ELT_AL138844101, dELTAM1208385872);
STR ELT_AL1860341546(ELT_AL629085806, dELTTU186609637);
STR ELT_AL372842071(ELT_AL629085806, OB, INT);
STR ELT_AL723329742(ELT_AL1287585232, dELTAB597459657);
STR FLISTA1264584154(FLISTA162798263);
STR FLISTA389202533(FLISTA1409846210);
STR FLISTA902886330(FLISTA1893461511);
STR IDENT_1415966123(IDENT, INT);
STR STR_ap2004550586(STR, STR);
STR STR_fr1097270334(STR, FSTR);
STR STR_se405450305(STR_se405450305_frame);
STR TP_CLASS_strrSTR(TP_CLASS);
TP_CLASS FSETTP340014621(FSETTP340014621_frame);
TP_CLASS TP_CLA115798516(TP_CLA115798516_frame);
TP_CLASS TP_GEN1057619759(TP_GEN1057619759_frame);
TP_CLASS TP_TBL944008484(TP_TBL, IDENT, ARRAYdTP, BOOL);
TP_CLASS_TBL TP_CLA426779342(TP_CLASS_TBL, TP_CLASS);
TP_CONTEXT TP_CLA821242177(TP_CLASS);
TUPIDENTINT FLISTT108744548(FLISTTUPIDENTINT);
TUPIDENTINT FLISTT97637998(FLISTT97637998_frame);
dTP AM_ANY772124871(AM_ANY_EXPR);
dTP AM_ATT839964902(AM_ATTR_EXPR);
dTP AM_AT_1635564765(AM_AT_EXPR);
dTP AM_CLU2134150433(AM_CLU1286269335);
dTP AM_CLU889770873(AM_CLUSTER_EXPR);
dTP AM_CONST_tprdTP(AM_CONST);
dTP AM_EXC706383345(AM_EXCEPT_EXPR);
dTP AM_FAR24566214(AM_FAR_EXPR);
dTP AM_FLT804205627(AM_FLTI_CONST);
dTP AM_GLO541877835(AM_GLOBAL_EXPR);
dTP AM_HER217252257(AM_HERE_EXPR);
dTP AM_IF_179347781(AM_IF_EXPR);
dTP AM_IS_826521970(AM_IS_VOID_EXPR);
dTP AM_LOC1004636145(AM_LOCAL_EXPR);
dTP AM_NEA176857219(AM_NEAR_EXPR);
dTP AM_NEW710003407(AM_NEW_EXPR);
dTP AM_SHA1973416871(AM_SHARED_EXPR);
dTP AM_STM300451057(AM_STMT_EXPR);
dTP AM_VAR2117196902(AM_VAR744470097);
dTP AM_VAT1812280108(AM_VAT319982528);
dTP AM_WHE843899429(AM_WHERE_EXPR);
dTP ARRAYd2147017943(ARRAYd2147017943_frame);
dTP TP_CON1800432689(TP_CONTEXT, AS_TYPE_SPEC);
dTP TP_CON2076339407(TP_CONTEXT, AS_TYPE_SPEC, BOOL);
void ARRAYI1177186037(ARRAYI1177186037_frame);
void ARRAYd1730019671(ARRAYd1730019671_frame);
void BFILE_plus_FSTR(BFILE, FSTR);
void BFILE_plus_STR(BFILE, STR);
void FILE_plus_FSTR(FILE1, FSTR);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void FIND_G1310533044(FIND_GENERICS, dAS_NODE);
void FIND_G1604486363(FIND_GENERICS, AS_TYPE_SPEC);
void FIND_G1788878638(FIND_GENERICS);
void FIND_G1798674529(FIND_GENERICS, TP_CLASS);
void FIND_G191267046(FIND_GENERICS);
void FIND_G597583556(FIND_GENERICS, dTP);
void FIND_T1058885549(FIND_TYPES, AS_TYPE_SPEC);
void FIND_T194079625(FIND_TYPES);
void FIND_T481437519(FIND_TYPES, dTP);
void FIND_T724224762(FIND_TYPES);
void FIND_T896241716(FIND_TYPES, dAS_NODE);
void FLISTA2015944564(FLISTA2015944564_frame);
void FLISTA609756551(FLISTA609756551_frame);
void PROG_e176405615(PROG, dPROG_ERR);
void PROG_err_STR(PROG, STR);
void TP_TBL1525652988(TP_TBL, IDENT, INT);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

AM_ANY_EXPR AM_ANY934244480(AM_ANY_EXPR self) {
 AM_ANY_EXPR ret_val;
 AM_ANY_EXPR r;
 AM_ANY_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_ANY_EXPR ret_val1;
 AM_ANY_EXPR r1;
 AM_ANY_EXPR L1;
 OB L2;
 if ((self==((AM_ANY_EXPR) NULL))) {
  ret_val = ((AM_ANY_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_ANY_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC_LEAF(sizeof(struct AM_ANY_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct AM_ANY_EXPR_struct));
 ((OB)L2)->header.tag=AM_ANY_EXPR_tag;
 L1 = ((AM_ANY_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_ATTR_EXPR AM_ATT1705760219(AM_ATTR_EXPR self) {
 AM_ATTR_EXPR ret_val;
 AM_ATTR_EXPR r;
 AM_ATTR_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_ATTR_EXPR ret_val1;
 AM_ATTR_EXPR r1;
 AM_ATTR_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_ATTR_EXPR) NULL))) {
  ret_val = ((AM_ATTR_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_ATTR_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_ATTR_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_ATTR_EXPR_tag;
 L1 = ((AM_ATTR_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,ob)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,ob);
  SATTR(r,ob,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 SATTR(r,self_tp,ATTR(self,self_tp));
 SATTR(r,at,ATTR(self,at));
 SATTR(r,tp_at,ATTR(self,tp_at));
 SATTR(r,secure1,ATTR(self,secure1));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_AT_EXPR AM_AT_1135400775(AM_AT_EXPR self) {
 AM_AT_EXPR ret_val;
 AM_AT_EXPR r;
 AM_AT_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_AT_EXPR ret_val1;
 AM_AT_EXPR r1;
 AM_AT_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_AT_EXPR) NULL))) {
  ret_val = ((AM_AT_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_AT_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_AT_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_AT_EXPR_tag;
 L1 = ((AM_AT_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,at)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,at);
  SATTR(r,at,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,e)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,e);
  SATTR(r,e,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_BND1124877163 FLISTA1230733197(FLISTA1893461511 self) {
 AM_BND1124877163 ret_val;
 AM_BND1124877163 r;
 FLISTA1893461511 aget_self;
 INT aget_ind = INT_zero;
 AM_BND1124877163 ret_val1;
 FLISTA1893461511 aset_self;
 INT aset_ind = INT_zero;
 AM_BND1124877163 aset_val;
 INT L1;
 BOOL L21_;
 INT L3;
 INT L41_;
 AM_BND1124877163 L51_;
 INT L6;
 INT L71_;
 INT L9;
 INT L101_;
 L1 = FLISTA919540447(self);
 L21_=(L1)==(0); 
 if (L21_) {
  ret_val = ((AM_BND1124877163) NULL);
  return ret_val;
 }
 aget_self = self;
 L3 = ATTR(self,loc);
 L41_=INTMINUS(L3,1); 
 aget_ind = L41_;
 L51_=(AM_BND1124877163)ARR((FLISTA1893461511)aget_self,aget_ind); 
 ret_val1 = L51_;
 r = ret_val1;
 aset_self = self;
 L6 = ATTR(self,loc);
 L71_=INTMINUS(L6,1); 
 aset_ind = L71_;
 aset_val = ((AM_BND1124877163) NULL);
 SARR((FLISTA1893461511)aset_self,aset_ind,(AM_BND1124877163)aset_val); 
 ;
 L9 = ATTR(self,loc);
 L101_=INTMINUS(L9,1); 
 SATTR(self,loc,L101_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CLU1286269335 AM_CLU779855221(AM_CLU1286269335 self) {
 AM_CLU1286269335 ret_val;
 AM_CLU1286269335 r;
 AM_CLU1286269335 create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_CLU1286269335 ret_val1;
 AM_CLU1286269335 r1;
 AM_CLU1286269335 L1;
 OB L2;
 if ((self==((AM_CLU1286269335) NULL))) {
  ret_val = ((AM_CLU1286269335) NULL);
  return ret_val;
 }
 create_self = ((AM_CLU1286269335) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC_LEAF(sizeof(struct AM_CLU1286269335_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct AM_CLU1286269335_struct));
 ((OB)L2)->header.tag=AM_CLU1286269335_tag;
 L1 = ((AM_CLU1286269335) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CLUSTER_EXPR AM_CLU1577744436(AM_CLUSTER_EXPR self) {
 AM_CLUSTER_EXPR ret_val;
 AM_CLUSTER_EXPR r;
 AM_CLUSTER_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_CLUSTER_EXPR ret_val1;
 AM_CLUSTER_EXPR r1;
 AM_CLUSTER_EXPR L1;
 OB L2;
 if ((self==((AM_CLUSTER_EXPR) NULL))) {
  ret_val = ((AM_CLUSTER_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_CLUSTER_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC_LEAF(sizeof(struct AM_CLUSTER_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct AM_CLUSTER_EXPR_struct));
 ((OB)L2)->header.tag=AM_CLUSTER_EXPR_tag;
 L1 = ((AM_CLUSTER_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_CONST AM_CON582703191(AM_CONST self) {
 AM_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_EXCEPT_EXPR AM_EXC449573271(AM_EXCEPT_EXPR self) {
 AM_EXCEPT_EXPR ret_val;
 AM_EXCEPT_EXPR r;
 AM_EXCEPT_EXPR create_self;
 dTP create_tp;
 AM_EXCEPT_EXPR ret_val1;
 AM_EXCEPT_EXPR r1;
 AM_EXCEPT_EXPR L1;
 OB L2;
 if ((self==((AM_EXCEPT_EXPR) NULL))) {
  ret_val = ((AM_EXCEPT_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_EXCEPT_EXPR) NULL);
 create_tp = ATTR(self,tp_at);
 L2=ZALLOC(sizeof(struct AM_EXCEPT_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_EXCEPT_EXPR_tag;
 L1 = ((AM_EXCEPT_EXPR) L2);
 r1 = L1;
 SATTR(r1,tp_at,create_tp);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FAR_EXPR AM_FAR755578228(AM_FAR_EXPR self) {
 AM_FAR_EXPR ret_val;
 AM_FAR_EXPR r;
 AM_FAR_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_FAR_EXPR ret_val1;
 AM_FAR_EXPR r1;
 AM_FAR_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_FAR_EXPR) NULL))) {
  ret_val = ((AM_FAR_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_FAR_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_FAR_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_FAR_EXPR_tag;
 L1 = ((AM_FAR_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,arg)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,arg);
  SATTR(r,arg,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_FLTI_CONST AM_FLT425117946(AM_FLTI_CONST self) {
 AM_FLTI_CONST ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_GLOBAL_EXPR AM_GLO928972965(AM_GLOBAL_EXPR self) {
 AM_GLOBAL_EXPR ret_val;
 AM_GLOBAL_EXPR r;
 AM_GLOBAL_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_GLOBAL_EXPR ret_val1;
 AM_GLOBAL_EXPR r1;
 AM_GLOBAL_EXPR L1;
 OB L2;
 if ((self==((AM_GLOBAL_EXPR) NULL))) {
  ret_val = ((AM_GLOBAL_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_GLOBAL_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC_LEAF(sizeof(struct AM_GLOBAL_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 memset(L2,0,sizeof(struct AM_GLOBAL_EXPR_struct));
 ((OB)L2)->header.tag=AM_GLOBAL_EXPR_tag;
 L1 = ((AM_GLOBAL_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_HERE_EXPR AM_HER1301311031(AM_HERE_EXPR self) {
 AM_HERE_EXPR ret_val;
 AM_HERE_EXPR r;
 AM_HERE_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_HERE_EXPR ret_val1;
 AM_HERE_EXPR r1;
 AM_HERE_EXPR L1;
 OB L2;
 if ((self==((AM_HERE_EXPR) NULL))) {
  ret_val = ((AM_HERE_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_HERE_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_HERE_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_HERE_EXPR_tag;
 L1 = ((AM_HERE_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_IF_EXPR AM_IF_1266575443(AM_IF_EXPR self) {
 AM_IF_EXPR ret_val;
 AM_IF_EXPR r;
 AM_IF_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IF_EXPR ret_val1;
 AM_IF_EXPR r1;
 AM_IF_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 BOOL L9;
 BOOL L101_;
 dAM_EXPR L11;
 if ((self==((AM_IF_EXPR) NULL))) {
  ret_val = ((AM_IF_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_IF_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_IF_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_IF_EXPR_tag;
 L1 = ((AM_IF_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,test1)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,test1);
  SATTR(r,test1,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,if_true)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,if_true);
  SATTR(r,if_true,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 L9 = (ATTR(self,if_false)==((dAM_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(self,if_false);
  SATTR(r,if_false,(*dAM_EX1830308833[TAG(L11)])(L11));
 }
 SATTR(r,tp_at,ATTR(self,tp_at));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_IS_VOID_EXPR AM_IS_193101840(AM_IS_VOID_EXPR self) {
 AM_IS_VOID_EXPR ret_val;
 AM_IS_VOID_EXPR r;
 AM_IS_VOID_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_IS_VOID_EXPR ret_val1;
 AM_IS_VOID_EXPR r1;
 AM_IS_VOID_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_IS_VOID_EXPR) NULL))) {
  ret_val = ((AM_IS_VOID_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_IS_VOID_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_IS_VOID_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_IS_VOID_EXPR_tag;
 L1 = ((AM_IS_VOID_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,arg)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,arg);
  SATTR(r,arg,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 SATTR(r,tp_at,ATTR(self,tp_at));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_LOCAL_EXPR AM_LOC1798202130(AM_LOCAL_EXPR self) {
 AM_LOCAL_EXPR ret_val;
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_NEAR_EXPR AM_NEA1086037759(AM_NEAR_EXPR self) {
 AM_NEAR_EXPR ret_val;
 AM_NEAR_EXPR r;
 AM_NEAR_EXPR create_self;
 SFILE_ID create_src = SFILE_ID_zero;
 AM_NEAR_EXPR ret_val1;
 AM_NEAR_EXPR r1;
 AM_NEAR_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_NEAR_EXPR) NULL))) {
  ret_val = ((AM_NEAR_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_NEAR_EXPR) NULL);
 create_src = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_NEAR_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_NEAR_EXPR_tag;
 L1 = ((AM_NEAR_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_src);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,arg)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,arg);
  SATTR(r,arg,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_NEW_EXPR AM_NEW1250698656(AM_NEW_EXPR self) {
 AM_NEW_EXPR ret_val;
 AM_NEW_EXPR r;
 AM_NEW_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_NEW_EXPR ret_val1;
 AM_NEW_EXPR r1;
 AM_NEW_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_NEW_EXPR) NULL))) {
  ret_val = ((AM_NEW_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_NEW_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_NEW_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_NEW_EXPR_tag;
 L1 = ((AM_NEW_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,tp_at,ATTR(self,tp_at));
 L3 = (ATTR(self,asz)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,asz);
  SATTR(r,asz,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_SHARED_EXPR AM_SHA1035965785(AM_SHARED_EXPR self) {
 AM_SHARED_EXPR ret_val;
 AM_SHARED_EXPR r;
 AM_SHARED_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_SHARED_EXPR ret_val1;
 AM_SHARED_EXPR r1;
 AM_SHARED_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_SHARED_EXPR) NULL))) {
  ret_val = ((AM_SHARED_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_SHARED_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_SHARED_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_SHARED_EXPR_tag;
 L1 = ((AM_SHARED_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 SATTR(r,tp_at,ATTR(self,tp_at));
 SATTR(r,name1,ATTR(self,name1));
 SATTR(r,class_tp,ATTR(self,class_tp));
 L3 = (ATTR(self,init)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,init);
  SATTR(r,init,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 SATTR(r,is_const,ATTR(self,is_const));
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_STMT_EXPR AM_STM416210327(AM_STMT_EXPR self) {
 AM_STMT_EXPR ret_val;
 AM_STMT_EXPR r;
 AM_STMT_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_STMT_EXPR ret_val1;
 AM_STMT_EXPR r1;
 AM_STMT_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_STMT L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_STMT_EXPR) NULL))) {
  ret_val = ((AM_STMT_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_STMT_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_STMT_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_STMT_EXPR_tag;
 L1 = ((AM_STMT_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,stmts)==((dAM_STMT) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,stmts);
  SATTR(r,stmts,(*dAM_ST1032597839[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,expr)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,expr);
  SATTR(r,expr,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_VAR744470097 AM_VAR1906690564(AM_VAR744470097 self) {
 AM_VAR744470097 ret_val;
 AM_VAR744470097 r;
 AM_VAR744470097 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VAR744470097 ret_val1;
 AM_VAR744470097 r1;
 AM_VAR744470097 L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 BOOL L9;
 BOOL L101_;
 dAM_EXPR L11;
 if ((self==((AM_VAR744470097) NULL))) {
  ret_val = ((AM_VAR744470097) NULL);
  return ret_val;
 }
 create_self = ((AM_VAR744470097) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_VAR744470097_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_VAR744470097_tag;
 L1 = ((AM_VAR744470097) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,ob)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,ob);
  SATTR(r,ob,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 L6 = (ATTR(self,ind)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,ind);
  SATTR(r,ind,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 L9 = (ATTR(self,val1)==((dAM_EXPR) NULL));
 L101_=!(L9); 
 if (L101_) {
  L11 = ATTR(self,val1);
  SATTR(r,val1,(*dAM_EX1830308833[TAG(L11)])(L11));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_VAT319982528 AM_VAT2137026865(AM_VAT319982528 self) {
 AM_VAT319982528 ret_val;
 AM_VAT319982528 r;
 AM_VAT319982528 create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_VAT319982528 ret_val1;
 AM_VAT319982528 r1;
 AM_VAT319982528 L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 dAM_EXPR L8;
 if ((self==((AM_VAT319982528) NULL))) {
  ret_val = ((AM_VAT319982528) NULL);
  return ret_val;
 }
 create_self = ((AM_VAT319982528) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_VAT319982528_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_VAT319982528_tag;
 L1 = ((AM_VAT319982528) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,ob)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,ob);
  SATTR(r,ob,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 SATTR(r,at,ATTR(self,at));
 SATTR(r,real_tp,ATTR(self,real_tp));
 L6 = (ATTR(self,val1)==((dAM_EXPR) NULL));
 L71_=!(L6); 
 if (L71_) {
  L8 = ATTR(self,val1);
  SATTR(r,val1,(*dAM_EX1830308833[TAG(L8)])(L8));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

AM_WHERE_EXPR AM_WHE1264983710(AM_WHERE_EXPR self) {
 AM_WHERE_EXPR ret_val;
 AM_WHERE_EXPR r;
 AM_WHERE_EXPR create_self;
 SFILE_ID create_source = SFILE_ID_zero;
 AM_WHERE_EXPR ret_val1;
 AM_WHERE_EXPR r1;
 AM_WHERE_EXPR L1;
 OB L2;
 BOOL L3;
 BOOL L41_;
 dAM_EXPR L5;
 if ((self==((AM_WHERE_EXPR) NULL))) {
  ret_val = ((AM_WHERE_EXPR) NULL);
  return ret_val;
 }
 create_self = ((AM_WHERE_EXPR) NULL);
 create_source = ATTR(self,source1);
 L2=ZALLOC(sizeof(struct AM_WHERE_EXPR_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=AM_WHERE_EXPR_tag;
 L1 = ((AM_WHERE_EXPR) L2);
 r1 = L1;
 SATTR(r1,source1,create_source);
 ret_val1 = r1;
 r = ret_val1;
 L3 = (ATTR(self,arg)==((dAM_EXPR) NULL));
 L41_=!(L3); 
 if (L41_) {
  L5 = ATTR(self,arg);
  SATTR(r,arg,(*dAM_EX1830308833[TAG(L5)])(L5));
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ANY1687381320(AM_ANY_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 if ((self==((AM_ANY_EXPR) NULL))) {
  L4 = (a==((OB) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L5 = (self==((AM_ANY_EXPR) NULL));
  L61_=!(L5); 
  L3 = L61_;
 }
 if (L3) {
  L7 = (a==((OB) NULL));
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L91_=SYSTP(a); 
  L10 = L91_;
  L111_=SYSTP(self); 
  L12 = L111_;
  L131_=(L10)==(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ANY309199231(AM_ANY_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_ANY_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_int);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ATT1582125658(AM_ATTR_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_ATTR_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_ATT1818896075(AM_ATTR_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 dAM_EXPR L2;
 if (a==NULL) {
  goto other157;
 } else
 switch (TAG(a)) {
  case AM_ATTR_EXPR_tag:
   is_eq_self = ATTR(self,at);
   is_eq_i = ATTR(((AM_ATTR_EXPR) a),at);
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   if (ret_val1) {
    L2 = ATTR(self,ob);
    L1 = (*dAM_EX2004971809[TAG(L2)])(L2, ((OB) ATTR(((AM_ATTR_EXPR) a),ob)));
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  default: ;
  other157: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_AT_1010480980(AM_AT_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 dAM_EXPR L2;
 dAM_EXPR L3;
 if (a==NULL) {
  goto other147;
 } else
 switch (TAG(a)) {
  case AM_AT_EXPR_tag:
   L2 = ATTR(self,e);
   if ((*dAM_EX2004971809[TAG(L2)])(L2, ((OB) ATTR(((AM_AT_EXPR) a),e)))) {
    L3 = ATTR(self,at);
    L1 = (*dAM_EX2004971809[TAG(L3)])(L3, ((OB) ATTR(((AM_AT_EXPR) a),at)));
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  default: ;
  other147: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_AT_1425681295(AM_AT_EXPR self) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 L1 = AM_AT_1635564765(self);
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CLU1159843664(AM_CLU1286269335 self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 if ((self==((AM_CLU1286269335) NULL))) {
  L4 = (a==((OB) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L5 = (self==((AM_CLU1286269335) NULL));
  L61_=!(L5); 
  L3 = L61_;
 }
 if (L3) {
  L7 = (a==((OB) NULL));
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L91_=SYSTP(a); 
  L10 = L91_;
  L111_=SYSTP(self); 
  L12 = L111_;
  L131_=(L10)==(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CLU170440611(AM_CLU1286269335 self) {
 BOOL ret_val = BOOL_zero;
 AM_CLU1286269335 tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_int);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CLU528836536(AM_CLUSTER_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 if ((self==((AM_CLUSTER_EXPR) NULL))) {
  L4 = (a==((OB) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L5 = (self==((AM_CLUSTER_EXPR) NULL));
  L61_=!(L5); 
  L3 = L61_;
 }
 if (L3) {
  L7 = (a==((OB) NULL));
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L91_=SYSTP(a); 
  L10 = L91_;
  L111_=SYSTP(self); 
  L12 = L111_;
  L131_=(L10)==(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CLU862886673(AM_CLUSTER_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_CLUSTER_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_int);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CON174643192(AM_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_CON357292637(AM_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_EXC219044661(AM_EXCEPT_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_EXCEPT_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_EXC30103808(AM_EXCEPT_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FAR1472604699(AM_FAR_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR L1;
 if (a==NULL) {
  goto other148;
 } else
 switch (TAG(a)) {
  case AM_FAR_EXPR_tag:
   L1 = ATTR(self,arg);
   ret_val = (*dAM_EX2004971809[TAG(L1)])(L1, ((OB) ATTR(((AM_FAR_EXPR) a),arg)));
   return ret_val; break;
  default: ;
  other148: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FAR833547686(AM_FAR_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_FAR_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_bool);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT1011996250(AM_FLTI_CONST self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_FLT840430235(AM_FLTI_CONST self) {
 BOOL ret_val = BOOL_zero;
 AM_FLTI_CONST tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_GLO1300832959(AM_GLOBAL_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_GLOBAL_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_zone);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_GLO286344550(AM_GLOBAL_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 if ((self==((AM_GLOBAL_EXPR) NULL))) {
  L4 = (a==((OB) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L5 = (self==((AM_GLOBAL_EXPR) NULL));
  L61_=!(L5); 
  L3 = L61_;
 }
 if (L3) {
  L7 = (a==((OB) NULL));
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L91_=SYSTP(a); 
  L10 = L91_;
  L111_=SYSTP(self); 
  L12 = L111_;
  L131_=(L10)==(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_HER1213808373(AM_HERE_EXPR self) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 L1 = ATTR(self,tp);
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_HER1833766706(AM_HERE_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 if ((self==((AM_HERE_EXPR) NULL))) {
  L4 = (a==((OB) NULL));
 } else {
  L4 = FALSE;
 }
 if (L4) {
  L3 = TRUE;
 } else {
  L5 = (self==((AM_HERE_EXPR) NULL));
  L61_=!(L5); 
  L3 = L61_;
 }
 if (L3) {
  L7 = (a==((OB) NULL));
  L81_=!(L7); 
  L2 = L81_;
 } else {
  L2 = FALSE;
 }
 if (L2) {
  L91_=SYSTP(a); 
  L10 = L91_;
  L111_=SYSTP(self); 
  L12 = L111_;
  L131_=(L10)==(L12); 
  L1 = L131_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_IF_2781110(AM_IF_EXPR self, OB am) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 BOOL L3;
 BOOL L4;
 BOOL L5;
 BOOL L61_;
 BOOL L7;
 BOOL L81_;
 BOOL L9;
 BOOL L10;
 BOOL L111_;
 BOOL L12;
 BOOL L131_;
 BOOL L14;
 BOOL L15;
 BOOL L161_;
 BOOL L17;
 BOOL L181_;
 dTP L19;
 BOOL L20;
 BOOL L211_;
 BOOL L22;
 BOOL L23;
 BOOL L24;
 dAM_EXPR L25;
 BOOL L26;
 dAM_EXPR L27;
 BOOL L28;
 dAM_EXPR L29;
 if (am==NULL) {
  goto other149;
 } else
 switch (TAG(am)) {
  case AM_IF_EXPR_tag:
   L4 = (ATTR(self,test1)==((dAM_EXPR) NULL));
   L5 = (ATTR(((AM_IF_EXPR) am),test1)==((dAM_EXPR) NULL));
   L61_=L4==L5; 
   L7 = L61_;
   L81_=!(L7); 
   if (L81_) {
    L3 = TRUE;
   } else {
    L9 = (ATTR(self,if_true)==((dAM_EXPR) NULL));
    L10 = (ATTR(((AM_IF_EXPR) am),if_true)==((dAM_EXPR) NULL));
    L111_=L9==L10; 
    L12 = L111_;
    L131_=!(L12); 
    L3 = L131_;
   }
   if (L3) {
    L2 = TRUE;
   } else {
    L14 = (ATTR(self,if_false)==((dAM_EXPR) NULL));
    L15 = (ATTR(((AM_IF_EXPR) am),if_false)==((dAM_EXPR) NULL));
    L161_=L14==L15; 
    L17 = L161_;
    L181_=!(L17); 
    L2 = L181_;
   }
   if (L2) {
    L1 = TRUE;
   } else {
    L19 = ATTR(self,tp_at);
    L20 = (*dTP_is242312711[TAG(L19)])(L19, ((OB) ATTR(((AM_IF_EXPR) am),tp_at)));
    L211_=!(L20); 
    L1 = L211_;
   }
   if (L1) {
    ret_val = FALSE;
    return ret_val;
   }
   if ((ATTR(self,test1)==((dAM_EXPR) NULL))) {
    L24 = TRUE;
   } else {
    L25 = ATTR(self,test1);
    L24 = (*dAM_EX2004971809[TAG(L25)])(L25, ((OB) ATTR(((AM_IF_EXPR) am),test1)));
   }
   if (L24) {
    if ((ATTR(self,if_true)==((dAM_EXPR) NULL))) {
     L26 = TRUE;
    } else {
     L27 = ATTR(self,if_true);
     L26 = (*dAM_EX2004971809[TAG(L27)])(L27, ((OB) ATTR(((AM_IF_EXPR) am),if_true)));
    }
    L23 = L26;
   } else {
    L23 = FALSE;
   }
   if (L23) {
    if ((ATTR(self,if_false)==((dAM_EXPR) NULL))) {
     L28 = TRUE;
    } else {
     L29 = ATTR(self,if_false);
     L28 = (*dAM_EX2004971809[TAG(L29)])(L29, ((OB) ATTR(((AM_IF_EXPR) am),if_false)));
    }
    L22 = L28;
   } else {
    L22 = FALSE;
   }
   ret_val = L22;
   return ret_val; break;
  default: ;
  other149: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_IF_792326817(AM_IF_EXPR self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 BOOL L2;
 dAM_EXPR L3;
 BOOL L4;
 dAM_EXPR L5;
 if ((ATTR(self,if_true)==((dAM_EXPR) NULL))) {
  L2 = TRUE;
 } else {
  L3 = ATTR(self,if_true);
  L2 = (*dAM_EX1117610980[TAG(L3)])(L3);
 }
 if (L2) {
  if ((ATTR(self,if_false)==((dAM_EXPR) NULL))) {
   L5 = ATTR(self,if_false);
   L4 = (*dAM_EX1117610980[TAG(L5)])(L5);
  } else {
   L4 = FALSE;
  }
  L1 = L4;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_IS_1001428606(AM_IS_VOID_EXPR self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_IS_997133203(AM_IS_VOID_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR L1;
 if (a==NULL) {
  goto other150;
 } else
 switch (TAG(a)) {
  case AM_IS_VOID_EXPR_tag:
   L1 = ATTR(self,arg);
   ret_val = (*dAM_EX2004971809[TAG(L1)])(L1, ((OB) ATTR(((AM_IS_VOID_EXPR) a),arg)));
   return ret_val; break;
  default: ;
  other150: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_LOC2055616863(AM_LOCAL_EXPR self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 dTP L2;
 if (ATTR(self,is_always_near)) {
  L1 = TRUE;
 } else {
  L2 = ATTR(self,tp_at);
  L1 = (*dTP_is1334578382[TAG(L2)])(L2);
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_LOC783953200(AM_LOCAL_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_NEA1492883953(AM_NEAR_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_NEAR_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ((dTP) TP_BUILTIN_bool);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_NEA499164018(AM_NEAR_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR L1;
 if (a==NULL) {
  goto other151;
 } else
 switch (TAG(a)) {
  case AM_NEAR_EXPR_tag:
   L1 = ATTR(self,arg);
   ret_val = (*dAM_EX2004971809[TAG(L1)])(L1, ((OB) ATTR(((AM_NEAR_EXPR) a),arg)));
   return ret_val; break;
  default: ;
  other151: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_NEW1485150288(AM_NEW_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_NEW952487097(AM_NEW_EXPR self) {
 BOOL ret_val = BOOL_zero;
 ret_val = TRUE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_SHA1922018909(AM_SHARED_EXPR self) {
 BOOL ret_val = BOOL_zero;
 AM_SHARED_EXPR tp_self;
 dTP ret_val1;
 dTP L1;
 tp_self = self;
 ret_val1 = ATTR(tp_self,tp_at);
 L1 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_SHA623484008(AM_SHARED_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 IDENT is_eq_self = IDENT_zero;
 IDENT is_eq_i = IDENT_zero;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 dTP L2;
 if (a==NULL) {
  goto other155;
 } else
 switch (TAG(a)) {
  case AM_SHARED_EXPR_tag:
   is_eq_self = ATTR(self,name1);
   is_eq_i = ATTR(((AM_SHARED_EXPR) a),name1);
   ret_val1 = (is_eq_self.str==is_eq_i.str);
   if (ret_val1) {
    L2 = ATTR(self,class_tp);
    L1 = (*dTP_is242312711[TAG(L2)])(L2, ((OB) ATTR(((AM_SHARED_EXPR) a),class_tp)));
   } else {
    L1 = FALSE;
   }
   ret_val = L1;
   return ret_val; break;
  default: ;
  other155: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_STM214668158(AM_STMT_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_STM552683579(AM_STMT_EXPR self) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR L1;
 L1 = ATTR(self,expr);
 ret_val = (*dAM_EX1117610980[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VAR1818998713(AM_VAR744470097 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VAR394665606(AM_VAR744470097 self) {
 BOOL ret_val = BOOL_zero;
 AM_VAR744470097 tp_self;
 dTP ret_val1;
 dAM_EXPR L1;
 dTP L2;
 tp_self = self;
 L1 = ATTR(tp_self,ob);
 ret_val1 = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 L2 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L2)])(L2);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VAT2140838301(AM_VAT319982528 self, OB a) {
 BOOL ret_val = BOOL_zero;
 ret_val = SYSOBEQ(((OB) self),a);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_VAT239070856(AM_VAT319982528 self) {
 BOOL ret_val = BOOL_zero;
 AM_VAT319982528 tp_self;
 dTP ret_val1;
 dAM_EXPR L1;
 dTP L2;
 tp_self = self;
 L1 = ATTR(tp_self,ob);
 ret_val1 = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 L2 = ret_val1;
 ret_val = (*dTP_is1334578382[TAG(L2)])(L2);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_WHE332796028(AM_WHERE_EXPR self, OB a) {
 BOOL ret_val = BOOL_zero;
 dAM_EXPR L1;
 if (a==NULL) {
  goto other156;
 } else
 switch (TAG(a)) {
  case AM_WHERE_EXPR_tag:
   L1 = ATTR(self,arg);
   ret_val = (*dAM_EX2004971809[TAG(L1)])(L1, ((OB) ATTR(((AM_WHERE_EXPR) a),arg)));
   return ret_val; break;
  default: ;
  other156: ;
   ret_val = FALSE;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

BOOL AM_WHE420295477(AM_WHERE_EXPR self) {
 BOOL ret_val = BOOL_zero;
 dTP L1;
 L1 = ATTR(self,tp);
 ret_val = (*dTP_is1334578382[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FLISTA1522230041(FLISTA1893461511 self, AM_BND1124877163 e) {
 BOOL ret_val = BOOL_zero;
 FLISTA1893461511 L11;
 FLISTA1893461511 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 AM_BND1124877163 L2;
 {
  struct FLISTA273121893_frame_struct other1_0;
FLISTA273121893_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   elt_eq_self = self;
   elt_eq_e1 = e;
   L2 = FLISTA273121893(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   elt_eq_e2 = L2;
   elt_eq_self1 = ((ELT_EQ947319992) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_a = ((OB) elt_eq_e21);
   ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val2 = ret_val3;
   ret_val1 = ret_val2;
   if (ret_val1) {
    ret_val = TRUE;
    return ret_val;
   }
  }
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1409846210 FLISTA1236996208(FLISTA1409846210 self, FLISTA1409846210 l) {
 FLISTA1409846210 ret_val;
 FLISTA1409846210 res;
 INT oldsize = INT_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 INT resi = INT_zero;
 FLISTA1409846210 aget_self;
 INT aget_ind = INT_zero;
 AM_BND367211769 ret_val1;
 FLISTA1409846210 aset_self;
 INT aset_ind = INT_zero;
 AM_BND367211769 aset_val;
 BOOL L1;
 BOOL L21_;
 INT L3;
 INT L4;
 INT L51_;
 BOOL L61_;
 AM_BND367211769 L71_;
 INT L91_;
 INT L101_;
 res = self;
 L1 = (l==((FLISTA1409846210) NULL));
 L21_=!(L1); 
 if (L21_) {
  oldsize = FLISTA2089597941(self);
  L3 = FLISTA2089597941(self);
  L4 = FLISTA2089597941(l);
  L51_=INTPLUS(L3,L4); 
  res = FLISTA157313261(res, L51_);
  i = 0;
  sz = FLISTA2089597941(l);
  resi = oldsize;
  while (1) {
   L61_=(i)==(sz); 
   if (L61_) {
    goto after_loop;
   }
   aset_self = res;
   aset_ind = resi;
   aget_self = l;
   aget_ind = i;
   L71_=(AM_BND367211769)ARR((FLISTA1409846210)aget_self,aget_ind); 
   ret_val1 = L71_;
   aset_val = ret_val1;
   SARR((FLISTA1409846210)aset_self,aset_ind,(AM_BND367211769)aset_val); 
   ;
   L91_=INTPLUS(i,1); 
   i = L91_;
   L101_=INTPLUS(resi,1); 
   resi = L101_;
  }
  after_loop: ;
 }
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1409846210 FLISTA157313261(FLISTA1409846210 self, INT new_size) {
 FLISTA1409846210 ret_val;
 FLISTA1409846210 r = ((FLISTA1409846210) NULL);
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTA1409846210 aget_self;
 INT aget_ind = INT_zero;
 AM_BND367211769 ret_val1;
 FLISTA1409846210 aset_self;
 INT aset_ind = INT_zero;
 AM_BND367211769 aset_val;
 FLISTA1409846210 L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 INT L61_;
 INT L7;
 BOOL L81_;
 BOOL L9;
 BOOL L101_;
 FLISTA1409846210 L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 BOOL L201_;
 AM_BND367211769 L211_;
 INT L231_;
 if ((self==((FLISTA1409846210) NULL))) {
  L31_=(5>new_size)?5:new_size; 
  L2 = L31_;
  L5=(sizeof(struct FLISTA1409846210_struct)-sizeof(AM_BND367211769))+(L2)*sizeof(AM_BND367211769);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA1409846210_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L1 = ((FLISTA1409846210) L4);
  L1->asize = L2;
  r = L1;
 }
 else {
  L61_=ASIZE((FLISTA1409846210)self); 
  L7 = L61_;
  L81_=(L7)<(new_size); 
  L9 = L81_;
  L101_=!(L9); 
  if (L101_) {
   r = self;
  }
  else {
   L131_=ASIZE((FLISTA1409846210)self); 
   L14 = L131_;
   L151_=INTTIMES(2,L14); 
   L16 = L151_;
   L171_=(L16>new_size)?L16:new_size; 
   L12 = L171_;
   L19=(sizeof(struct FLISTA1409846210_struct)-sizeof(AM_BND367211769))+(L12)*sizeof(AM_BND367211769);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1409846210_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L11 = ((FLISTA1409846210) L18);
   L11->asize = L12;
   r = L11;
   SATTR(r,loc,ATTR(self,loc));
   i = 0;
   sz = FLISTA2089597941(self);
   while (1) {
    L201_=(i)==(sz); 
    if (L201_) {
     goto after_loop;
    }
    aset_self = r;
    aset_ind = i;
    aget_self = self;
    aget_ind = i;
    L211_=(AM_BND367211769)ARR((FLISTA1409846210)aget_self,aget_ind); 
    ret_val1 = L211_;
    aset_val = ret_val1;
    SARR((FLISTA1409846210)aset_self,aset_ind,(AM_BND367211769)aset_val); 
    ;
    L231_=INTPLUS(i,1); 
    i = L231_;
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 SATTR(r,loc,new_size);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1409846210 FLISTA1715286767(FLISTA1409846210 self, AM_BND367211769 e) {
 FLISTA1409846210 ret_val;
 FLISTA1409846210 r = ((FLISTA1409846210) NULL);
 FLISTA1409846210 L11;
 FLISTA1409846210 L21;
 INT L31 = INT_zero;
 FLISTA1409846210 aset_self;
 INT aset_ind = INT_zero;
 AM_BND367211769 aset_val;
 FLISTA1409846210 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA1409846210 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_BND367211769 L22;
 AM_BND367211769 L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA1409846210) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA1409846210_struct)-sizeof(AM_BND367211769))+(L5)*sizeof(AM_BND367211769);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA1409846210_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA1409846210) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA1409846210)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA1409846210)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA1409846210_struct)-sizeof(AM_BND367211769))+(L14)*sizeof(AM_BND367211769);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1409846210_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA1409846210) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA1446310868_frame_struct other1_0;
FLISTA1446310868_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA1409846210)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA1446310868(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA1409846210)L21,L31,(AM_BND367211769)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA1409846210)aset_self,aset_ind,(AM_BND367211769)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1409846210 FLISTA681914133(FLISTA1409846210 self, INT n) {
 FLISTA1409846210 ret_val;
 BOOL L11_;
 FLISTA1409846210 L2;
 INT L3;
 OB L4;
 INT L5;
 L11_=(n)==(0); 
 if (L11_) {
  ret_val = ((FLISTA1409846210) NULL);
  return ret_val;
 }
 else {
  L3 = n;
  L5=(sizeof(struct FLISTA1409846210_struct)-sizeof(AM_BND367211769))+(L3)*sizeof(AM_BND367211769);
  L4=ZALLOC_BIG(L5);
  if (L4==NULL) FATAL("Unable to allocate more memory");
  ((OB)L4)->header.tag=FLISTA1409846210_tag;
#ifdef DESTROY_CHK

    ((OB)L4)->header.destroyed=0;
#endif

  L2 = ((FLISTA1409846210) L4);
  L2->asize = L3;
  ret_val = L2;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1893461511 FLISTA1412425582(FLISTA1893461511 self, AM_BND1124877163 e) {
 FLISTA1893461511 ret_val;
 if (FLISTA1522230041(self, e)) {
  ret_val = self;
  return ret_val;
 }
 else {
  ret_val = FLISTA80425519(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1893461511 FLISTA226859063(FLISTA1893461511 self, FLISTA1893461511 l) {
 FLISTA1893461511 ret_val;
 FLISTA1893461511 r;
 FLISTA1893461511 L11;
 AM_BND1124877163 L2;
 r = FLISTA359657848(self);
 {
  struct FLISTA273121893_frame_struct other1_0;
FLISTA273121893_frame noname1 = &other1_0;
  L11 = l;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FLISTA273121893(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FLISTA1412425582(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1893461511 FLISTA359657848(FLISTA1893461511 self) {
 FLISTA1893461511 ret_val;
 FLISTA1893461511 r;
 INT i = INT_zero;
 INT sz = INT_zero;
 FLISTA1893461511 aget_self;
 INT aget_ind = INT_zero;
 AM_BND1124877163 ret_val1;
 FLISTA1893461511 aset_self;
 INT aset_ind = INT_zero;
 AM_BND1124877163 aset_val;
 FLISTA1893461511 L1;
 INT L2;
 INT L31_;
 OB L4;
 INT L5;
 BOOL L61_;
 AM_BND1124877163 L71_;
 INT L91_;
 if ((self==((FLISTA1893461511) NULL))) {
  ret_val = ((FLISTA1893461511) NULL);
  return ret_val;
 }
 L31_=ASIZE((FLISTA1893461511)self); 
 L2 = L31_;
 L5=(sizeof(struct FLISTA1893461511_struct)-sizeof(AM_BND1124877163))+(L2)*sizeof(AM_BND1124877163);
 L4=ZALLOC_BIG(L5);
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=FLISTA1893461511_tag;
#ifdef DESTROY_CHK

   ((OB)L4)->header.destroyed=0;
#endif

 L1 = ((FLISTA1893461511) L4);
 L1->asize = L2;
 r = L1;
 i = 0;
 sz = ATTR(self,loc);
 SATTR(r,loc,ATTR(self,loc));
 while (1) {
  L61_=(i)==(sz); 
  if (L61_) {
   goto after_loop;
  }
  aset_self = r;
  aset_ind = i;
  aget_self = self;
  aget_ind = i;
  L71_=(AM_BND1124877163)ARR((FLISTA1893461511)aget_self,aget_ind); 
  ret_val1 = L71_;
  aset_val = ret_val1;
  SARR((FLISTA1893461511)aset_self,aset_ind,(AM_BND1124877163)aset_val); 
  ;
  L91_=INTPLUS(i,1); 
  i = L91_;
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA1893461511 FLISTA80425519(FLISTA1893461511 self, AM_BND1124877163 e) {
 FLISTA1893461511 ret_val;
 FLISTA1893461511 r = ((FLISTA1893461511) NULL);
 FLISTA1893461511 L11;
 FLISTA1893461511 L21;
 INT L31 = INT_zero;
 FLISTA1893461511 aset_self;
 INT aset_ind = INT_zero;
 AM_BND1124877163 aset_val;
 FLISTA1893461511 L4;
 INT L5;
 OB L6;
 INT L7;
 INT L8;
 INT L91_;
 INT L10;
 BOOL L121_;
 FLISTA1893461511 L13;
 INT L14;
 INT L151_;
 INT L16;
 INT L171_;
 OB L18;
 INT L19;
 INT L201_br;
 AM_BND1124877163 L22;
 AM_BND1124877163 L23;
 INT L241_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 if ((self==((FLISTA1893461511) NULL))) {
  L5 = 5;
  L7=(sizeof(struct FLISTA1893461511_struct)-sizeof(AM_BND1124877163))+(L5)*sizeof(AM_BND1124877163);
  L6=ZALLOC_BIG(L7);
  if (L6==NULL) FATAL("Unable to allocate more memory");
  ((OB)L6)->header.tag=FLISTA1893461511_tag;
#ifdef DESTROY_CHK

    ((OB)L6)->header.destroyed=0;
#endif

  L4 = ((FLISTA1893461511) L6);
  L4->asize = L5;
  r = L4;
 }
 else {
  L8 = ATTR(self,loc);
  L91_=ASIZE((FLISTA1893461511)self); 
  L10 = L91_;
  L121_=(L8)<(L10); 
  if (L121_) {
   r = self;
  }
  else {
   L151_=ASIZE((FLISTA1893461511)self); 
   L16 = L151_;
   L171_=INTTIMES(2,L16); 
   L14 = L171_;
   L19=(sizeof(struct FLISTA1893461511_struct)-sizeof(AM_BND1124877163))+(L14)*sizeof(AM_BND1124877163);
   L18=ZALLOC_BIG(L19);
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FLISTA1893461511_tag;
#ifdef DESTROY_CHK

     ((OB)L18)->header.destroyed=0;
#endif

   L13 = ((FLISTA1893461511) L18);
   L13->asize = L14;
   r = L13;
   SATTR(r,loc,ATTR(self,loc));
   {
    struct FLISTA273121893_frame_struct other1_0;
FLISTA273121893_frame noname1 = &other1_0;
    BOOL f_L201_ = TRUE;
    L11 = self;
    /* loop index variable */
    L31 = 0;
    L21 = r;
    noname1->self = L11;
    noname1->state = 0;
    L201_br=ASIZE((FLISTA1893461511)L21); 
    while (1) {
     if(L31>=L201_br)  goto after_loop; 
     L22 = FLISTA273121893(noname1);
     if (noname1->state == -1) {
      goto after_loop;
     }
     L23 = L22;
     SARR((FLISTA1893461511)L21,L31,(AM_BND1124877163)L23); 
     ;
     L241_=INTPLUS(L31,1); 
     L31 = L241_;
    }
   }
   after_loop: ;
   SYSDESTROY(self); 
   ;
  }
 }
 L26 = ATTR(r,loc);
 L271_=INTPLUS(L26,1); 
 SATTR(r,loc,L271_);
 aset_self = r;
 L28 = ATTR(r,loc);
 L291_=INTMINUS(L28,1); 
 aset_ind = L291_;
 aset_val = e;
 SARR((FLISTA1893461511)aset_self,aset_ind,(AM_BND1124877163)aset_val); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ANY1678367917(AM_ANY_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_ATT2085324530(AM_ATTR_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_AT_726422231(AM_AT_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_CLU1812304013(AM_CLUSTER_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_CLU39726141(AM_CLU1286269335 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_CON19269408(AM_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_EXC1481209187(AM_EXCEPT_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FAR276305130(AM_FAR_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_FLT523420577(AM_FLTI_CONST self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_GLO404411817(AM_GLOBAL_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_HER1483923803(AM_HERE_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_IF_1267236903(AM_IF_EXPR self) {
 FLISTA725283029 ret_val;
 FLISTA725283029 f = ((FLISTA725283029) NULL);
 FLISTA725283029 g;
 BOOL L1;
 BOOL L21_;
 dAM_EXPR L3;
 BOOL L4;
 dAM_EXPR L5;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 dAM_EXPR L10;
 BOOL L11;
 dAM_EXPR L12;
 BOOL L13;
 BOOL L141_;
 L1 = (ATTR(self,if_true)==((dAM_EXPR) NULL));
 L21_=!(L1); 
 if (L21_) {
  L3 = ATTR(self,if_true);
  f = (*dAM_EX1656723676[TAG(L3)])(L3);
  if ((f==((FLISTA725283029) NULL))) {
   L5 = ATTR(self,if_true);
   L6 = (*dAM_EX1117610980[TAG(L5)])(L5);
   L71_=!(L6); 
   L4 = L71_;
  } else {
   L4 = FALSE;
  }
  if (L4) {
   ret_val = ((FLISTA725283029) NULL);
   return ret_val;
  }
 }
 L8 = (ATTR(self,if_false)==((dAM_EXPR) NULL));
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,if_true);
  g = (*dAM_EX1656723676[TAG(L10)])(L10);
  if ((g==((FLISTA725283029) NULL))) {
   L12 = ATTR(self,if_false);
   L13 = (*dAM_EX1117610980[TAG(L12)])(L12);
   L141_=!(L13); 
   L11 = L141_;
  } else {
   L11 = FALSE;
  }
  if (L11) {
   ret_val = ((FLISTA725283029) NULL);
   return ret_val;
  }
  ret_val = FLISTA189055531(f, g);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_IS_1111682258(AM_IS_VOID_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_LOC1121251403(AM_LOCAL_EXPR self) {
 FLISTA725283029 ret_val;
 AM_LOCAL_EXPR is_near_self;
 BOOL ret_val1 = BOOL_zero;
 BOOL L1;
 dTP L2;
 is_near_self = self;
 if (ATTR(is_near_self,is_always_near)) {
  L1 = TRUE;
 } else {
  L2 = ATTR(is_near_self,tp_at);
  L1 = (*dTP_is1334578382[TAG(L2)])(L2);
 }
 ret_val1 = L1;
 if (ret_val1) {
  ret_val = ((FLISTA725283029) NULL);
  return ret_val;
 }
 else {
  ret_val = FLISTA1817671564(((FLISTA725283029) NULL), self);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_NEA1654213825(AM_NEAR_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_NEW1679302875(AM_NEW_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_SHA1496045189(AM_SHARED_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_STM1543836917(AM_STMT_EXPR self) {
 FLISTA725283029 ret_val;
 dAM_EXPR L1;
 L1 = ATTR(self,expr);
 ret_val = (*dAM_EX1656723676[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_VAR567579326(AM_VAR744470097 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_VAT467614272(AM_VAT319982528 self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FLISTA725283029 AM_WHE1419632471(AM_WHERE_EXPR self) {
 FLISTA725283029 ret_val;
 ret_val = ((FLISTA725283029) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR BFILE_fstrrFSTR(BFILE self) {
 FSTR ret_val;
 INT sz = INT_zero;
 FSTR r;
 INT fs = INT_zero;
 BFILE size_self;
 INT ret_val1 = INT_zero;
 INT current1 = INT_zero;
 INT r1 = INT_zero;
 BFILE current_loc_self;
 INT ret_val2 = INT_zero;
 BFILE seek_from_end_se;
 INT seek_from_end_of = INT_zero;
 INT dummy1 = INT_zero;
 BFILE current_loc_self1;
 INT ret_val3 = INT_zero;
 BFILE seek_from_front_;
 INT seek_from_front_1 = INT_zero;
 INT dummy2 = INT_zero;
 BFILE seek_from_front_2;
 INT seek_from_front_3 = INT_zero;
 INT dummy3 = INT_zero;
 BOOL L11_;
 BOOL L21_;
 FSTR L3;
 size_self = self;
 current_loc_self = size_self;
 ret_val2 = ftell(ATTR(current_loc_self,fp));
 current1 = ret_val2;
 seek_from_end_se = size_self;
 seek_from_end_of = 0;
 dummy1 = fseek(ATTR(seek_from_end_se,fp), seek_from_end_of, 2);
 current_loc_self1 = size_self;
 ret_val3 = ftell(ATTR(current_loc_self1,fp));
 r1 = ret_val3;
 seek_from_front_ = size_self;
 seek_from_front_1 = current1;
 dummy2 = fseek(ATTR(seek_from_front_,fp), seek_from_front_1, 0);
 ret_val1 = r1;
 sz = ret_val1;
 L11_=(sz)==(-1); 
 if (L11_) {
  ret_val = ((FSTR) NULL);
  return ret_val;
 }
 L21_=(sz)==(0); 
 if (L21_) {
  ret_val = FSTR_c1998740379(((FSTR) NULL), 1);
  return ret_val;
 }
 r = FSTR_c1998740379(((FSTR) NULL), sz);
 seek_from_front_2 = self;
 seek_from_front_3 = 0;
 dummy3 = fseek(ATTR(seek_from_front_2,fp), seek_from_front_3, 0);
 L3 = r;
 fs = fread(((L3==NULL)?NULL:L3->arr_part), sz, 1, ATTR(self,fp));
 SATTR(r,loc,sz);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSTR FILE_g1986509982(FILE1 self) {
 FSTR ret_val;
 FSTR res;
 CHAR c = CHAR_zero;
 FSTR create_self;
 FSTR ret_val1;
 FILE1 get_char_self;
 CHAR ret_val2 = CHAR_zero;
 FILE1 eof_self;
 BOOL ret_val3 = BOOL_zero;
 FSTR L1;
 INT L2;
 OB L3;
 INT L4;
 BOOL L51_;
 create_self = ((FSTR) NULL);
 L2 = 16;
 L4=(sizeof(struct FSTR_struct)+1-sizeof(CHAR))+(L2)*sizeof(CHAR);
 L3=ZALLOC_LEAF_BIG(L4);
 if (L3==NULL) FATAL("Unable to allocate more memory");
 memset(L3,0,L4);
 ((OB)L3)->header.tag=FSTR_tag;
#ifdef DESTROY_CHK

   ((OB)L3)->header.destroyed=0;
#endif

 L1 = ((FSTR) L3);
 L1->asize = L2;
 ret_val1 = L1;
 res = ret_val1;
 while (1) {
  get_char_self = self;
  ret_val2 = fgetc(ATTR(get_char_self,fp));
  c = ret_val2;
  eof_self = self;
  ret_val3 = feof(ATTR(eof_self,fp));
  if (ret_val3) {
   ret_val = res;
   return ret_val;
  }
  res = FSTR_p399773021(res, c);
  L51_=c=='\n'; 
  if (L51_) {
   goto after_loop;
  }
 }
 after_loop: ;
 ret_val = res;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ANY1965823461(AM_ANY_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_ATT178000012(AM_ATTR_EXPR self) {
 INT ret_val = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 dAM_EXPR L7;
 INT L8;
 INT L91_;
 hash_self = ATTR(self,at);
 L1 = hash_self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val1 = L51_;
 L6 = ret_val1;
 L7 = ATTR(self,ob);
 L8 = (*dAM_EX1001627434[TAG(L7)])(L7);
 L91_=L6^L8; 
 ret_val = L91_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_AT_373259037(AM_AT_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_CLU772455521(AM_CLU1286269335 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_CLU978087499(AM_CLUSTER_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_CON2051977914(AM_CONST self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_EXC800301049(AM_EXCEPT_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FAR2123150608(AM_FAR_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_FLT153804005(AM_FLTI_CONST self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_GLO57248015(AM_GLOBAL_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_HER774694957(AM_HERE_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_IF_1573308145(AM_IF_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_IS_128338200(AM_IS_VOID_EXPR self) {
 INT ret_val = INT_zero;
 dAM_EXPR L1;
 INT L2;
 INT L31_;
 L1 = ATTR(self,arg);
 L2 = (*dAM_EX1001627434[TAG(L1)])(L1);
 L31_=L2^3843; 
 ret_val = L31_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_LOC505223029(AM_LOCAL_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_NEA1314295025(AM_NEAR_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_NEW1014811437(AM_NEW_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_SHA1840537425(AM_SHARED_EXPR self) {
 INT ret_val = INT_zero;
 IDENT hash_self = IDENT_zero;
 INT ret_val1 = INT_zero;
 INT x = INT_zero;
 STR L1;
 INT L21_;
 INT L31_;
 INT L4;
 INT L51_;
 INT L6;
 dTP L7;
 INT L8;
 INT L91_;
 hash_self = ATTR(self,name1);
 L1 = hash_self.str;
 L21_=SYSID(L1); 
 x = L21_;
 L31_=((unsigned)x*(unsigned)1664525); 
 L4 = L31_;
 L51_=((unsigned)L4+(unsigned)1013904223); 
 ret_val1 = L51_;
 L6 = ret_val1;
 L7 = ATTR(self,class_tp);
 L8 = (*dTP_hashrINT[TAG(L7)])(L7);
 L91_=L6^L8; 
 ret_val = L91_;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_STM804699229(AM_STMT_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_VAR2089448132(AM_VAR744470097 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_VAT495261450(AM_VAT319982528 self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT AM_WHE1598364639(AM_WHERE_EXPR self) {
 INT ret_val = INT_zero;
 INT L11_;
 L11_=SYSID(self); 
 ret_val = INT_hashrINT(L11_);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA2089597941(FLISTA1409846210 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA1409846210) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FLISTA919540447(FLISTA1893461511 self) {
 INT ret_val = INT_zero;
 if ((self==((FLISTA1893461511) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,loc);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ANY312639639(AM_ANY_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_ATT1483547331(AM_ATTR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_AT_336435086(AM_AT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CLU1154173259(AM_CLUSTER_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CLU2049939620(AM_CLU1286269335 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_CON810758495(AM_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_EXC2040160518(AM_EXCEPT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FAR526851136(AM_FAR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_FLT564073323(AM_FLTI_CONST self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_GLO537255272(AM_GLOBAL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_HER1201005342(AM_HERE_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_IF_1629993260(AM_IF_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_IS_82827228(AM_IS_VOID_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_LOC327374505(AM_LOCAL_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_NEA1717951158(AM_NEAR_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_NEW765217327(AM_NEW_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_SHA644128690(AM_SHARED_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_STM1466562094(AM_STMT_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_VAR1884071700(AM_VAR744470097 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_VAT338387515(AM_VAT319982528 self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

SFILE_ID AM_WHE345322829(AM_WHERE_EXPR self) {
 return ATTR(self,source1);
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL1860341546(ELT_AL629085806 self, dELTTU186609637 arr) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 dELTTU186609637 L11;
 TUPTP_1668454172 e = TUPTP_1668454172_zero;
 FSTR str_self;
 STR ret_val1;
 extern STR name2;
 dELTTU186609637 L2;
 TUPTP_1668454172 L3;
 BOOL L41_;
 extern STR name3;
 FSTR L5;
 OB L6;
 OB L7;
 OB L8;
 OB L9;
 INT L101_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 {
  
  dELTTU963535579_frame 
  noname1;
  L11 = arr;
  L2 = L11;
  noname1 = (dELTTU963535579_frame) (*dELTTU963535579[TAG(L2)].alloc_frame)();
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = (*dELTTU963535579[TAG(L11)].iter)(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   e = L3;
   L41_=(0)<(i); 
   if (L41_) {
    L5 = FSTR_p1752847026(res, ((STR) &name3));
    L7=ZALLOC(sizeof(struct TUPTP_1668454172_boxed_struct));
    if (L7==NULL) FATAL("Unable to allocate more memory");
    ((OB)L7)->header.tag=TUPTP_1668454172_tag;
    L6 = (OB)((TUPTP_1668454172_boxed) L7);
    ((TUPTP_1668454172_boxed) L6)->immutable_part = e;
    res = FSTR_p1752847026(L5, ELT_AL372842071(self, L6, i));
   }
   else {
    L9=ZALLOC(sizeof(struct TUPTP_1668454172_boxed_struct));
    if (L9==NULL) FATAL("Unable to allocate more memory");
    ((OB)L9)->header.tag=TUPTP_1668454172_tag;
    L8 = (OB)((TUPTP_1668454172_boxed) L9);
    ((TUPTP_1668454172_boxed) L8)->immutable_part = e;
    res = FSTR_p1752847026(res, ELT_AL372842071(self, L8, i));
   }
   L101_=INTPLUS(i,1); 
   i = L101_;
  }
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self = res;
 ret_val1 = STR_fr1097270334(((STR) NULL), str_self);
 ret_val = ret_val1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

STR ELT_AL372842071(ELT_AL629085806 self, OB e, INT i) {
 STR ret_val;
 INT str_self = INT_zero;
 STR ret_val1;
 FSTR buf1 = ((FSTR) NULL);
 FSTR clear_self;
 INT str_in_self = INT_zero;
 FSTR str_in_s;
 FSTR ret_val2;
 FSTR str_self1;
 STR ret_val3;
 STR plus_self;
 STR plus_sarg;
 STR ret_val4;
 dSTR L1;
 extern STR Unprintable;
 BOOL L2;
 BOOL L31_;
 if (e==NULL) {
  goto other89;
 } else
 switch (TAG(e)) {
  case ABSTRA151498155_tag:
  case ABSTRACT_LAYOUT_tag:
  case ARG_LAYOUT_tag:
  case ARRAYdAM_CONST_tag:
  case ARRAYdAM_EXPR_tag:
  case ARRAYdAM_STMT_tag:
  case ARRAYdLAYOUT_tag:
  case ARRAYdTP_tag:
  case ARRAYA1870808380_tag:
  case ARRAYA855692714_tag:
  case ARRAYA132553625_tag:
  case ARRAYAM_CALL_ARG_tag:
  case ARRAYA36280886_tag:
  case ARRAYAM_CURSOR_tag:
  case ARRAYA1347292543_tag:
  case ARRAYA1369744492_tag:
  case ARRAYA1575632848_tag:
  case ARRAYAM_ROUT_DEF_tag:
  case ARRAYA1135548710_tag:
  case ARRAYARG_tag:
  case ARRAYA477251019_tag:
  case ARRAYARRAYBOOL_tag:
  case ARRAYARRAYSTR_tag:
  case ARRAYAS_FEAT_MOD_tag:
  case ARRAYATTRSORT_tag:
  case ARRAYBOOL_tag:
  case ARRAYB1234765325_tag:
  case ARRAYB1925477728_tag:
  case ARRAYB1039347268_tag:
  case ARRAYCALL_ARG_tag:
  case ARRAYCHAR_tag:
  case ARRAYC1438655838_tag:
  case ARRAYCODE_FILE_tag:
  case ARRAYC1788421899_tag:
  case ARRAYE2126408533_tag:
  case ARRAYE406487263_tag:
  case ARRAYF1030817442_tag:
  case ARRAYFLISTSTR_tag:
  case ARRAYFSTR_tag:
  case ARRAYIDENT_tag:
  case ARRAYIFC_tag:
  case ARRAYI1327367692_tag:
  case ARRAYI1991954990_tag:
  case ARRAYINT_tag:
  case ARRAYO570594127_tag:
  case ARRAYSFILE_ID_tag:
  case ARRAYSIDE_EFFECT_tag:
  case ARRAYSIG_tag:
  case ARRAYSTR_tag:
  case ARRAYTP_CLASS_tag:
  case ARRAYTP_LAYOUT_tag:
  case ARRAYT747572380_tag:
  case ARRAYTUPdOBSTR_tag:
  case ARRAYT1418209173_tag:
  case ARRAYT1153101058_tag:
  case ARRAYT1562141251_tag:
  case ARRAYTUPdTPINT_tag:
  case ARRAYT1275404834_tag:
  case ARRAYT1146239647_tag:
  case ARRAYT2065045042_tag:
  case ARRAYT1753333425_tag:
  case ARRAYT1744744076_tag:
  case ARRAYT546647214_tag:
  case ARRAYT1968212795_tag:
  case ARRAYTUPIDENTdTP_tag:
  case ARRAYT263953675_tag:
  case ARRAYTUPIDENTINT_tag:
  case ARRAYT348931282_tag:
  case ARRAYT537394765_tag:
  case ARRAYT1759168384_tag:
  case ARRAYT1754348798_tag:
  case ARRAYTUPSIGINT_tag:
  case ARRAYTUPSIGSIG_tag:
  case ARRAYTUPSIGSTR_tag:
  case ARRAYT61112132_tag:
  case ARRAYT378612147_tag:
  case ARRAYTUPSTRINT_tag:
  case ARRAYTUPSTRSIG_tag:
  case ARRAYTUPSTRSTR_tag:
  case ARRAYT1127505716_tag:
  case ARRAYT1641775270_tag:
  case A_STAC1244966098_tag:
  case BOOL_tag:
  case BOUND_1007407331_tag:
  case BOUND_2140373124_tag:
  case BOUND_1677815802_tag:
  case BOUND_809390774_tag:
  case BUILTIN_LAYOUT_tag:
  case CHAR_tag:
  case CLASS_LAYOUT_tag:
  case CONFIG_DEF_tag:
  case CONFIG_ROUT_tag:
  case CONFIG_TBL_tag:
  case ELT_TBL_tag:
  case EXTERNAL_LAYOUT_tag:
  case FLISTdAM_CONST_tag:
  case FLISTdAM_EXPR_tag:
  case FLISTdAM_STMT_tag:
  case FLISTdLAYOUT_tag:
  case FLISTdTP_tag:
  case FLISTA162798263_tag:
  case FLISTA1893461511_tag:
  case FLISTA1409846210_tag:
  case FLISTAM_CALL_ARG_tag:
  case FLISTA886630705_tag:
  case FLISTAM_CURSOR_tag:
  case FLISTA2097324934_tag:
  case FLISTA1062334999_tag:
  case FLISTA725283029_tag:
  case FLISTAM_ROUT_DEF_tag:
  case FLISTA112326087_tag:
  case FLISTARG_tag:
  case FLISTA1327600838_tag:
  case FLISTARRAYBOOL_tag:
  case FLISTARRAYSTR_tag:
  case FLISTAS_FEAT_MOD_tag:
  case FLISTATTRSORT_tag:
  case FLISTBOOL_tag:
  case FLISTB475591118_tag:
  case FLISTB2062080075_tag:
  case FLISTB16124645_tag:
  case FLISTCALL_ARG_tag:
  case FLISTCHAR_tag:
  case FLISTC1031309271_tag:
  case FLISTCODE_FILE_tag:
  case FLISTC1772242452_tag:
  case FLISTE1761212196_tag:
  case FLISTE1627119380_tag:
  case FLISTF7594819_tag:
  case FLISTFLISTSTR_tag:
  case FLISTFSTR_tag:
  case FLISTIDENT_tag:
  case FLISTIFC_tag:
  case FLISTI2086541899_tag:
  case FLISTI1895665739_tag:
  case FLISTINT_tag:
  case FLISTO1593816750_tag:
  case FLISTSFILE_ID_tag:
  case FLISTSIDE_EFFECT_tag:
  case FLISTSIG_tag:
  case FLISTSTR_tag:
  case FLISTTP_CLASS_tag:
  case FLISTTP_LAYOUT_tag:
  case FLISTT1513788273_tag:
  case FLISTTUPdOBSTR_tag:
  case FLISTT567859354_tag:
  case FLISTT880505585_tag:
  case FLISTT1858637310_tag:
  case FLISTTUPdTPINT_tag:
  case FLISTT1311334615_tag:
  case FLISTT1661203254_tag:
  case FLISTT1145236345_tag:
  case FLISTT2049829484_tag:
  case FLISTT406286251_tag:
  case FLISTT31683607_tag:
  case FLISTT1683165204_tag:
  case FLISTTUPIDENTdTP_tag:
  case FLISTT1023127882_tag:
  case FLISTTUPIDENTINT_tag:
  case FLISTT1075494997_tag:
  case FLISTT844804258_tag:
  case FLISTT1742988937_tag:
  case FLISTT994805427_tag:
  case FLISTTUPSIGINT_tag:
  case FLISTTUPSIGSIG_tag:
  case FLISTTUPSIGSTR_tag:
  case FLISTT789237687_tag:
  case FLISTT471737672_tag:
  case FLISTTUPSTRINT_tag:
  case FLISTTUPSTRSIG_tag:
  case FLISTTUPSTRSTR_tag:
  case FLISTT418307355_tag:
  case FLISTT102367225_tag:
  case FLT_tag:
  case FLTD_tag:
  case FMAPdOBNAMESPACE_tag:
  case FMAPdTPdLAYOUT_tag:
  case FMAPdTPCODE_FILE_tag:
  case FMAPdT953816637_tag:
  case FMAPdTPINT_tag:
  case FMAPAM339652544_tag:
  case FMAPAM1357596931_tag:
  case FMAPAM1145008811_tag:
  case FMAPAM418009930_tag:
  case FMAPAM1244483504_tag:
  case FMAPAM1787628573_tag:
  case FMAPIDENTdTP_tag:
  case FMAPID739086519_tag:
  case FMAPSI2103621588_tag:
  case FMAPSI518162669_tag:
  case FMAPSI1349988702_tag:
  case FMAPSI1121373188_tag:
  case FMAPSIGSTR_tag:
  case FMAPSTRdCONFIG_tag:
  case FMAPSTRFSETSTR_tag:
  case FMAPSTRINT_tag:
  case FMAPSTRSIG_tag:
  case FMAPSTRSTR_tag:
  case FMAPTP291739594_tag:
  case FMAPTP775297600_tag:
  case FMT_ERROR_tag:
  case FRAME_LAYOUT_tag:
  case FSTR_tag:
  case IFC_tag:
  case IMMUTA421759893_tag:
  case INOUT_MODE_tag:
  case INT_tag:
  case INTI1_tag:
  case IN_MODE_tag:
  case NULL_STAT_tag:
  case ONCE_MODE_tag:
  case OUT_MODE_tag:
  case RAT_tag:
  case SFILE_ID_tag:
  case SIG_tag:
  case SIG_TBL_tag:
  case STAT1_tag:
  case STR_tag:
  case TP_CLASS_tag:
  case TP_ITER_tag:
  case TP_LAYOUT_tag:
  case TP_ROUT_tag:
  case TUPdAM_EXPRdTP_tag:
  case TUPdAS268679779_tag:
  case TUPdOBNAMESPACE_tag:
  case TUPdOBSTR_tag:
  case TUPdTPdLAYOUT_tag:
  case TUPdTPCODE_FILE_tag:
  case TUPdTP1710233051_tag:
  case TUPdTPINT_tag:
  case TUPAM_304458649_tag:
  case TUPAM_667892060_tag:
  case TUPAM_927437481_tag:
  case TUPAM_1519040877_tag:
  case TUPAM_504653531_tag:
  case TUPAM_1024994801_tag:
  case TUPAM_653469574_tag:
  case TUPARRAYARGdTP_tag:
  case TUPARR1759987776_tag:
  case TUPBOO1392923241_tag:
  case TUPIDENTdTP_tag:
  case TUPIDE641654124_tag:
  case TUPIDENTARRAYdTP_tag:
  case TUPIDENTINT_tag:
  case TUPINTINT_tag:
  case TUPSIG1754078736_tag:
  case TUPSIG578692189_tag:
  case TUPSIGARG_LAYOUT_tag:
  case TUPSIG2023533247_tag:
  case TUPSIGINT_tag:
  case TUPSIGSIG_tag:
  case TUPSIGSTR_tag:
  case TUPSTRdCONFIG_tag:
  case TUPSTRFSETSTR_tag:
  case TUPSTRINT_tag:
  case TUPSTRSIG_tag:
  case TUPSTRSTR_tag:
  case TUPTP_858321267_tag:
  case TUPTP_1668454172_tag:
   L1 = ((dSTR) e);
   ret_val = (*dSTR_strrSTR[TAG(L1)])(L1);
   return ret_val; break;
  default: ;
  other89: ;
   plus_self = ((STR) &Unprintable);
   str_self = i;
   clear_self = buf1;
   L2 = (clear_self==((FSTR) NULL));
   L31_=!(L2); 
   if (L31_) {
    SATTR(clear_self,loc,0);
   }
   str_in_self = str_self;
   str_in_s = buf1;
   ret_val2 = INT_st367594495(str_in_self, str_in_s, 0, 10, ' ');
   buf1 = ret_val2;
   str_self1 = buf1;
   ret_val3 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val1 = ret_val3;
   plus_sarg = ret_val1;
   ret_val4 = STR_ap2004550586(plus_self, plus_sarg);
   ret_val = ret_val4;
   return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA1264584154(FLISTA162798263 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA162798263) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL723329742(((ELT_AL1287585232) NULL), ((dELTAB597459657) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA389202533(FLISTA1409846210 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA1409846210) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1351346800(((ELT_AL138844101) NULL), ((dELTAM1208385872) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FLISTA902886330(FLISTA1893461511 self) {
 STR ret_val;
 extern STR name5;
 if ((self==((FLISTA1893461511) NULL))) {
  ret_val = ((STR) &name5);
  return ret_val;
 }
 else {
  ret_val = ELT_AL1147639445(((ELT_AL1248934552) NULL), ((dELTAM1766315002) self));
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ANY772124871(AM_ANY_EXPR self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_int);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_ATT839964902(AM_ATTR_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_AT_1635564765(AM_AT_EXPR self) {
 dTP ret_val;
 dAM_EXPR L1;
 L1 = ATTR(self,e);
 ret_val = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_CLU2134150433(AM_CLU1286269335 self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_int);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_CLU889770873(AM_CLUSTER_EXPR self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_int);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_CONST_tprdTP(AM_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_EXC706383345(AM_EXCEPT_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FAR24566214(AM_FAR_EXPR self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_bool);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_FLT804205627(AM_FLTI_CONST self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_GLO541877835(AM_GLOBAL_EXPR self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_zone);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_HER217252257(AM_HERE_EXPR self) {
 return ATTR(self,tp);
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_IF_179347781(AM_IF_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_IS_826521970(AM_IS_VOID_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_LOC1004636145(AM_LOCAL_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_NEA176857219(AM_NEAR_EXPR self) {
 dTP ret_val;
 ret_val = ((dTP) TP_BUILTIN_bool);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_NEW710003407(AM_NEW_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_SHA1973416871(AM_SHARED_EXPR self) {
 dTP ret_val;
 ret_val = ATTR(self,tp_at);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_STM300451057(AM_STMT_EXPR self) {
 dTP ret_val;
 dAM_EXPR L1;
 if ((ATTR(self,expr)==((dAM_EXPR) NULL))) {
  ret_val = ((dTP) NULL);
  return ret_val;
 }
 else {
  L1 = ATTR(self,expr);
  ret_val = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_VAR2117196902(AM_VAR744470097 self) {
 dTP ret_val;
 dAM_EXPR L1;
 L1 = ATTR(self,ob);
 ret_val = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_VAT1812280108(AM_VAT319982528 self) {
 dTP ret_val;
 dAM_EXPR L1;
 L1 = ATTR(self,ob);
 ret_val = (*dAM_EXPR_tprdTP[TAG(L1)])(L1);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dTP AM_WHE843899429(AM_WHERE_EXPR self) {
 return ATTR(self,tp);
}


#undef IS_ITER
#define IS_ITER 0

void BFILE_plus_FSTR(BFILE self, FSTR s) {
 INT fs = INT_zero;
 FSTR length_self;
 INT ret_val = INT_zero;
 INT L1;
 EXT_OB L2;
 INT L31_;
 if ((s==((FSTR) NULL))) {
  return;
 }
 length_self = s;
 ret_val = FSTR_sizerINT(length_self);
 L1 = ret_val;
 L2 = ATTR(self,fp);
 L31_=fwrite(((FSTR)s)->arr_part,L1,1,L2); 
 fs = L31_;
}


#undef IS_ITER
#define IS_ITER 0

void BFILE_plus_STR(BFILE self, STR s) {
 INT fs = INT_zero;
 INT L1;
 EXT_OB L2;
 INT L31_;
 if ((s==((STR) NULL))) {
  return;
 }
 L1 = STR_lengthrINT(s);
 L2 = ATTR(self,fp);
 L31_=fwrite(((STR)s)->arr_part,L1,1,L2); 
 fs = L31_;
}


#undef IS_ITER
#define IS_ITER 0

void FILE_plus_FSTR(FILE1 self, FSTR s) {
 INT fs = INT_zero;
 FSTR length_self;
 INT ret_val = INT_zero;
 INT L1;
 EXT_OB L2;
 INT L31_;
 if ((s==((FSTR) NULL))) {
  return;
 }
 length_self = s;
 ret_val = FSTR_sizerINT(length_self);
 L1 = ret_val;
 L2 = ATTR(self,fp);
 L31_=fwrite(((FSTR)s)->arr_part,L1,1,L2); 
 fs = L31_;
}


#undef IS_ITER
#define IS_ITER 0

void FILE_plus_STR(FILE1 self, STR s) {
 INT fs = INT_zero;
 INT L1;
 EXT_OB L2;
 INT L31_;
 if ((s==((STR) NULL))) {
  return;
 }
 L1 = STR_lengthrINT(s);
 L2 = ATTR(self,fp);
 L31_=fwrite(((STR)s)->arr_part,L1,1,L2); 
 fs = L31_;
}


#undef IS_ITER
#define IS_ITER 0

void FILE_plus_dSTR(FILE1 self, dSTR s) {
 STR ss;
 INT fs = INT_zero;
 dSTR L1;
 INT L2;
 EXT_OB L3;
 INT L41_;
 if ((s==((dSTR) NULL))) {
  return;
 }
 L1 = s;
 ss = (*dSTR_strrSTR[TAG(L1)])(L1);
 L2 = STR_lengthrINT(ss);
 L3 = ATTR(self,fp);
 L41_=fwrite(((STR)ss)->arr_part,L2,1,L3); 
 fs = L41_;
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G1310533044(FIND_GENERICS self, dAS_NODE as) {
 dAS_NODE as_n = ((dAS_NODE) NULL);
 FIND_GENERICS err_loc_self;
 dPROG_ERR err_loc_t;
 while (1) {
  if ((as==((dAS_NODE) NULL))) {
   goto after_loop;
  }
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) as);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  switch (TAG(as)) {
   case AS_CLASS_DEF_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),params)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),under)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),over)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),body)));
    return; break;
   case AS_PARAM_DEC_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PARAM_DEC) as),type_constraint)));
    as_n = ((dAS_NODE) ATTR(((AS_PARAM_DEC) as),next)); break;
   case AS_TYPE_SPEC_tag:
    PROG_e176405615(ATTR(self,prog), ((dPROG_ERR) as));
    FIND_G597583556(self, TP_CON1800432689(ATTR(self,con), ((AS_TYPE_SPEC) as)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPE_SPEC) as),next)); break;
   case AS_INC1416917001_tag:
    FIND_G1604486363(self, ATTR(((AS_INC1416917001) as),tp));
    as_n = ((dAS_NODE) ATTR(((AS_INC1416917001) as),next)); break;
   case AS_CONST_DEF_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CONST_DEF) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CONST_DEF) as),init)));
    as_n = ((dAS_NODE) ATTR(((AS_CONST_DEF) as),next)); break;
   case AS_SHARED_DEF_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),init)));
    as_n = ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),next)); break;
   case AS_ATTR_DEF_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ATTR_DEF) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_ATTR_DEF) as),next)); break;
   case AS_ROUT_DEF_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),args_dec)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),ret_dec)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),pre_e)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),post_e)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),next)); break;
   case AS_STMT_LIST_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_STMT_LIST) as),stmts)));
    return; break;
   case AS_ARG_DEC_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ARG_DEC) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_ARG_DEC) as),next)); break;
   case AS_DEC_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_DEC_STMT) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_DEC_STMT) as),next)); break;
   case AS_ASSIGN_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),lhs_expr)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),rhs)));
    as_n = ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),next)); break;
   case AS_IF_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),test1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),then_part)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_IF_STMT) as),next)); break;
   case AS_LOOP_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOOP_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_LOOP_STMT) as),next)); break;
   case AS_RETURN_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_RETURN_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_RETURN_STMT) as),next)); break;
   case AS_YIELD_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_YIELD_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_YIELD_STMT) as),next)); break;
   case AS_QUIT_STMT_tag:
    as_n = ((dAS_NODE) ATTR(((AS_QUIT_STMT) as),next)); break;
   case AS_CASE_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),test1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),when_part)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_CASE_STMT) as),next)); break;
   case AS_CASE_WHEN_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),val1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),next)); break;
   case AS_TYPECASE_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),when_part)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),next)); break;
   case AS_TYPECASE_WHEN_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),next)); break;
   case AS_ASSERT_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ASSERT_STMT) as),test1)));
    as_n = ((dAS_NODE) ATTR(((AS_ASSERT_STMT) as),next)); break;
   case AS_PROTECT_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),body)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),when_part)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),next)); break;
   case AS_PROTECT_WHEN_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),next)); break;
   case AS_RAISE_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_RAISE_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_RAISE_STMT) as),next)); break;
   case AS_EXPR_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_EXPR_STMT) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_EXPR_STMT) as),next)); break;
   case AS_SELF_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_SELF_EXPR) as),next)); break;
   case AS_CALL_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),ob)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),args)));
    as_n = ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),next)); break;
   case AS_VOID_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_VOID_EXPR) as),next)); break;
   case AS_IS_VOID_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_IS_VOID_EXPR) as),arg)));
    as_n = ((dAS_NODE) ATTR(((AS_IS_VOID_EXPR) as),next)); break;
   case AS_ARRAY_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ARRAY_EXPR) as),elts)));
    as_n = ((dAS_NODE) ATTR(((AS_ARRAY_EXPR) as),next)); break;
   case AS_CREATE_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),tp)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),elts)));
    as_n = ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),next)); break;
   case AS_BOU14269336_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_BOU14269336) as),call1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_BOU14269336) as),ret)));
    as_n = ((dAS_NODE) ATTR(((AS_BOU14269336) as),next)); break;
   case AS_UND152986614_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_UND152986614) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_UND152986614) as),next)); break;
   case AS_AND_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_AND_EXPR) as),e1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_AND_EXPR) as),e2)));
    as_n = ((dAS_NODE) ATTR(((AS_AND_EXPR) as),next)); break;
   case AS_OR_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_OR_EXPR) as),e1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_OR_EXPR) as),e2)));
    as_n = ((dAS_NODE) ATTR(((AS_OR_EXPR) as),next)); break;
   case AS_EXCEPT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_EXCEPT_EXPR) as),next)); break;
   case AS_NEW_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_NEW_EXPR) as),arg)));
    as_n = ((dAS_NODE) ATTR(((AS_NEW_EXPR) as),next)); break;
   case AS_INITIAL_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_INITIAL_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_INITIAL_EXPR) as),next)); break;
   case AS_BREAK_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_BREAK_EXPR) as),next)); break;
   case AS_RESULT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_RESULT_EXPR) as),next)); break;
   case AS_BOOL_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_BOOL_LIT_EXPR) as),next)); break;
   case AS_CHAR_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CHAR_LIT_EXPR) as),next)); break;
   case AS_STR_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_STR_LIT_EXPR) as),next)); break;
   case AS_INT_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_INT_LIT_EXPR) as),next)); break;
   case AS_FLT_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_FLT_LIT_EXPR) as),next)); break;
   case AS_PAR_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_PAR_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_PAR_STMT) as),next)); break;
   case AS_ATTACH_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),lhs)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),rhs)));
    as_n = ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),next)); break;
   case AS_LOCK_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),e_list)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),next)); break;
   case AS_LOCK_IF_WHEN_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),val1)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),e_list)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),next)); break;
   case AS_UNLOCK_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_UNLOCK_STMT) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_UNLOCK_STMT) as),next)); break;
   case AS_WIT1063437351_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_WIT1063437351) as),near_part)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_WIT1063437351) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_WIT1063437351) as),next)); break;
   case AS_FORK_STMT_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_FORK_STMT) as),at)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_FORK_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_FORK_STMT) as),next)); break;
   case AS_INT2031239268_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_INT2031239268) as),at)));
    as_n = ((dAS_NODE) ATTR(((AS_INT2031239268) as),next)); break;
   case AS_SYNC_STMT_tag:
    as_n = ((dAS_NODE) ATTR(((AS_SYNC_STMT) as),next)); break;
   case AS_HERE_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_HERE_EXPR) as),next)); break;
   case AS_WHERE_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_WHERE_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_WHERE_EXPR) as),next)); break;
   case AS_NEAR_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_NEAR_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_NEAR_EXPR) as),next)); break;
   case AS_FAR_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_FAR_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_FAR_EXPR) as),next)); break;
   case AS_AT_EXPR_tag:
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_AT_EXPR) as),e)));
    FIND_G1310533044(self, ((dAS_NODE) ATTR(((AS_AT_EXPR) as),at)));
    as_n = ((dAS_NODE) ATTR(((AS_AT_EXPR) as),next)); break;
   case AS_ANY_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_ANY_EXPR) as),next)); break;
   case AS_CLUSTER_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CLUSTER_EXPR) as),next)); break;
   case AS_CLU553570599_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CLU553570599) as),next)); break;
   case AS_COHORT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_COHORT_EXPR) as),next)); break;
   case AS_GLOBAL_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_GLOBAL_EXPR) as),next)); break;
   default: ;
    FATAL("No applicable type in typecase\nin FIND_GENERICS::do_tps($AS_NODE)\n./Front/find_types.sa:159:20");
  }
  as = as_n;
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G1604486363(FIND_GENERICS self, AS_TYPE_SPEC inc_type) {
 ARRAYdTP params = ((ARRAYdTP) NULL);
 ARRAYIDENT pnames = ((ARRAYIDENT) NULL);
 dTP included_class = ((dTP) NULL);
 INT inc_param_c = INT_zero;
 TUPIDENTINT typeid = TUPIDENTINT_zero;
 AS_CLASS_DEF include_tree;
 AS_TYPE_SPEC inc_param;
 ARRAYdTP L11;
 AS_PARAM_DEC tree_pnames;
 ARRAYIDENT L21;
 TP_CONTEXT old_con;
 TUPIDENTINT dummy1 = TUPIDENTINT_zero;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val = TUPIDENTINT_zero;
 FLISTTUPIDENTINT contains_self;
 TUPIDENTINT contains_e = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 FIND_GENERICS tree_for_self;
 IDENT tree_for_nm = IDENT_zero;
 INT tree_for_num = INT_zero;
 AS_CLASS_DEF ret_val2;
 ARRAYdTP create_self1;
 INT create_n = INT_zero;
 ARRAYdTP ret_val3;
 ARRAYIDENT create_self2;
 INT create_n1 = INT_zero;
 ARRAYIDENT ret_val4;
 TP_CONTEXT create_self3;
 TP_CLASS create_same;
 ARRAYIDENT create_pnames;
 ARRAYdTP create_ptypes;
 PROG create_prog;
 TP_CONTEXT ret_val5;
 TP_CONTEXT r;
 TUPIDENTINT L3;
 TUPIDENTINT L4;
 dPARSE L5;
 BOOL L61_;
 ARRAYdTP L7;
 INT L8;
 OB L9;
 INT L10;
 INT L121_br;
INT i_L121_=0;
 dTP L13;
 ARRAYIDENT L14;
 INT L15;
 OB L16;
 INT L17;
 INT L181_br;
INT i_L181_=0;
 IDENT L19;
 TP_CONTEXT L20;
 OB L22;
 inc_param_c = AS_TYP2145685956(ATTR(inc_type,params));
 create_self = TUPIDENTINT_zero;
 create_at1 = ATTR(inc_type,name1);
 create_at2 = inc_param_c;
 L3 = create_self;
 L3.t1 = create_at1;
 L4 = L3;
 L4.t2 = create_at2;
 ret_val = L4;
 typeid = ret_val;
 contains_self = FIND_G1866376849;
 contains_e = typeid;
 ret_val1 = FLISTT1340999558(contains_self, contains_e);
 if (ret_val1) {
  FIND_G191267046(self);
  return;
 }
 FIND_G1866376849 = FLISTT1610849855(FIND_G1866376849, typeid);
 tree_for_self = self;
 tree_for_nm = ATTR(inc_type,name1);
 tree_for_num = inc_param_c;
 L5 = ATTR(ATTR(tree_for_self,prog),parse);
 ret_val2 = (*dPARSE1581792481[TAG(L5)])(L5, tree_for_nm, tree_for_num);
 include_tree = ret_val2;
 L61_=(inc_param_c)==(0); 
 if (L61_) {
  included_class = TP_CON2076339407(ATTR(self,con), inc_type, FALSE);
 }
 else {
  create_self1 = ((ARRAYdTP) NULL);
  create_n = inc_param_c;
  L8 = create_n;
  L10=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L8)*sizeof(dTP);
  L9=ZALLOC_BIG(L10);
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

    ((OB)L9)->header.destroyed=0;
#endif

  L7 = ((ARRAYdTP) L9);
  L7->asize = L8;
  ret_val3 = L7;
  params = ret_val3;
  inc_param = ATTR(inc_type,params);
  {
   BOOL f_L121_ = TRUE;
   L11 = params;
   L121_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
   i_L121_=0;
   while (1) {
    if ((inc_param==((AS_TYPE_SPEC) NULL))) {
     goto after_loop;
    }
    if(i_L121_>=L121_br)  goto after_loop; 
    L13 = TP_CON1800432689(ATTR(self,con), inc_param);
    SARR((ARRAYdTP)L11,i_L121_,(dTP)L13); i_L121_++;
    ;
    inc_param = ATTR(inc_param,next);
   }
  }
  after_loop: ;
  if ((include_tree==((AS_CLASS_DEF) NULL))) {
   return;
  }
  create_self2 = ((ARRAYIDENT) NULL);
  create_n1 = inc_param_c;
  L15 = create_n1;
  L17=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L15)*sizeof(IDENT);
  L16=ZALLOC_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((ARRAYIDENT) L16);
  L14->asize = L15;
  ret_val4 = L14;
  pnames = ret_val4;
  tree_pnames = ATTR(include_tree,params);
  {
   BOOL f_L181_ = TRUE;
   L21 = pnames;
   L181_br=L21==NULL?0:ASIZE((ARRAYIDENT)L21); 
   i_L181_=0;
   while (1) {
    if ((tree_pnames==((AS_PARAM_DEC) NULL))) {
     goto after_loop1;
    }
    if(i_L181_>=L181_br)  goto after_loop1; 
    L19 = ATTR(tree_pnames,name1);
    SARR((ARRAYIDENT)L21,i_L181_,L19); i_L181_++;
    ;
    tree_pnames = ATTR(tree_pnames,next);
   }
  }
  after_loop1: ;
  included_class = TP_CON2076339407(ATTR(self,con), inc_type, FALSE);
  if (ATTR(ATTR(self,prog),check_generics)) {
   TP_TBL1525652988(ATTR(ATTR(self,prog),tp_tbl), ATTR(inc_type,name1), inc_param_c);
  }
 }
 old_con = ATTR(self,con);
 create_self3 = ((TP_CONTEXT) NULL);
 create_same = ATTR(ATTR(self,con),same1);
 create_pnames = pnames;
 create_ptypes = params;
 create_prog = ATTR(self,prog);
 L22=ZALLOC(sizeof(struct TP_CONTEXT_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=TP_CONTEXT_tag;
 L20 = ((TP_CONTEXT) L22);
 r = L20;
 SATTR(r,same1,create_same);
 SATTR(r,pnames,create_pnames);
 SATTR(r,ptypes,create_ptypes);
 SATTR(r,prog,create_prog);
 ret_val5 = r;
 SATTR(self,con,ret_val5);
 FIND_G1310533044(self, ((dAS_NODE) include_tree));
 SATTR(self,con,old_con);
 dummy1 = FLISTT108744548(FIND_G1866376849);
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G1788878638(FIND_GENERICS self) {
 TP_GEN_TBL L11;
 FSETTP_CLASS tp_doing;
 FSETTP_CLASS L21;
 TP_CLASS current_class;
 AS_CLASS_DEF as_tree;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val;
 TP_CLASS L3;
 TP_CLASS L4;
 BOOL L5;
 BOOL L61_;
 dPARSE L7;
 IDENT L8;
 ARRAYdTP L9;
 INT L101_;
 dPARSE L12;
 BOOL L13;
 BOOL L141_;
 SATTR(ATTR(self,prog),find_generics_st,TRUE);
 {
  struct TP_GEN1057619759_frame_struct other1_0;
TP_GEN1057619759_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(ATTR(self,prog),tp_tbl),gen_tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L3 = TP_GEN1057619759(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   SATTR(self,generic_class,L3);
   SATTR(self,tp_todo,((FSETTP_CLASS) NULL));
   SATTR(self,tp_todo,FSETTP1186813482(ATTR(self,tp_todo), ATTR(self,generic_class)));
   while (1) {
    if ((ATTR(self,tp_todo)==((FSETTP_CLASS) NULL))) {
     goto after_loop1;
    }
    tp_doing = ATTR(self,tp_todo);
    SATTR(self,tp_todo,((FSETTP_CLASS) NULL));
    {
     struct FSETTP340014621_frame_struct other3_0;
FSETTP340014621_frame noname2 = &other3_0;
     L21 = tp_doing;
     noname2->self = L21;
     noname2->state = 0;
     while (1) {
      L4 = FSETTP340014621(noname2);
      if (noname2->state == -1) {
       goto after_loop2;
      }
      current_class = L4;
      SATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl,TP_CLA426779342(ATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl), current_class));
      SATTR(ATTR(self,prog),tp_done,FSETdT1016814448(ATTR(ATTR(self,prog),tp_done), ((dTP) current_class)));
      L5 = ATTR(current_class,is_dummy_class);
      L61_=!(L5); 
      if (L61_) {
       SATTR(self,con,TP_CLA821242177(current_class));
       as_tree_self = current_class;
       if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
        L7 = ATTR(ATTR(as_tree_self,prog),parse);
        L8 = ATTR(as_tree_self,name1);
        L9 = ATTR(as_tree_self,params);
        L101_=(L9)==NULL?0:ASIZE((ARRAYdTP)L9); 
        L12 = L7;
        SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L12)])(L12, L8, L101_));
       }
       ret_val = ATTR(as_tree_self,my_as_tree);
       as_tree = ret_val;
       L13 = (as_tree==((AS_CLASS_DEF) NULL));
       L141_=!(L13); 
       if (L141_) {
        FIND_G1310533044(self, ((dAS_NODE) as_tree));
       }
      }
     }
    }
    after_loop2: ;
   }
   after_loop1: ;
  }
 }
 after_loop: ;
 SATTR(ATTR(self,prog),find_generics_st,FALSE);
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G1798674529(FIND_GENERICS self, TP_CLASS local_class) {
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
 ERR1 ret_val5;
 FILE1 stderr_self2;
 FILE1 ret_val6;
 FILE1 r2;
 ERR1 plus_self3;
 dSTR plus_s3;
 FILE1 stderr_self3;
 FILE1 ret_val7;
 FILE1 r3;
 BOOL L1;
 ARRAYdTP L2;
 INT L31_;
 INT L4;
 BOOL L51_;
 BOOL L6;
 BOOL L71_;
 extern STR adding;
 FILE1 L8;
 OB L9;
 FILE1 L11;
 OB L12;
 extern STR to;
 FILE1 L14;
 OB L15;
 FILE1 L17;
 OB L18;
 L2 = ATTR(local_class,params);
 L31_=(L2)==NULL?0:ASIZE((ARRAYdTP)L2); 
 L4 = L31_;
 L51_=(0)<(L4); 
 if (L51_) {
  L6 = TP_TBL663685613(ATTR(ATTR(self,prog),tp_tbl), ((dTP) local_class));
  L71_=!(L6); 
  L1 = L71_;
 } else {
  L1 = FALSE;
 }
 if (L1) {
  SATTR(self,tp_todo,FSETTP1186813482(ATTR(self,tp_todo), local_class));
  if (ATTR(ATTR(self,prog),trace_generics)) {
   create_self = ((ERR1) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((dSTR) ((STR) &adding));
   stderr_self = ((FILE1) NULL);
   L9=ZALLOC(sizeof(struct FILE1_struct));
   if (L9==NULL) FATAL("Unable to allocate more memory");
   ((OB)L9)->header.tag=FILE1_tag;
   L8 = ((FILE1) L9);
   r = L8;
   
   SATTR(r,fp,stderr);
   ret_val2 = r;
   FILE_plus_dSTR(ret_val2, plus_s);
   ret_val1 = plus_self;
   plus_self1 = ret_val1;
   plus_s1 = ((dSTR) TP_CLASS_strrSTR(local_class));
   stderr_self1 = ((FILE1) NULL);
   L12=ZALLOC(sizeof(struct FILE1_struct));
   if (L12==NULL) FATAL("Unable to allocate more memory");
   ((OB)L12)->header.tag=FILE1_tag;
   L11 = ((FILE1) L12);
   r1 = L11;
   
   SATTR(r1,fp,stderr);
   ret_val4 = r1;
   FILE_plus_dSTR(ret_val4, plus_s1);
   ret_val3 = plus_self1;
   plus_self2 = ret_val3;
   plus_s2 = ((dSTR) ((STR) &to));
   stderr_self2 = ((FILE1) NULL);
   L15=ZALLOC(sizeof(struct FILE1_struct));
   if (L15==NULL) FATAL("Unable to allocate more memory");
   ((OB)L15)->header.tag=FILE1_tag;
   L14 = ((FILE1) L15);
   r2 = L14;
   
   SATTR(r2,fp,stderr);
   ret_val6 = r2;
   FILE_plus_dSTR(ret_val6, plus_s2);
   ret_val5 = plus_self2;
   plus_self3 = ret_val5;
   plus_s3 = ((dSTR) TP_CLASS_strrSTR(ATTR(self,generic_class)));
   stderr_self3 = ((FILE1) NULL);
   L18=ZALLOC(sizeof(struct FILE1_struct));
   if (L18==NULL) FATAL("Unable to allocate more memory");
   ((OB)L18)->header.tag=FILE1_tag;
   L17 = ((FILE1) L18);
   r3 = L17;
   
   SATTR(r3,fp,stderr);
   ret_val7 = r3;
   FILE_plus_dSTR(ret_val7, plus_s3);
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G191267046(FIND_GENERICS self) {
 STR str = ((STR) NULL);
 FLISTTUPIDENTINT L11;
 TUPIDENTINT cur = TUPIDENTINT_zero;
 STR L21;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 extern STR name29;
 TUPIDENTINT L3;
 STR L4;
 extern STR Cyclic1005221544;
 extern STR name18;
 {
  struct FLISTT97637998_frame_struct other1_0;
FLISTT97637998_frame noname1 = &other1_0;
  struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
  L11 = FIND_G1866376849;
  L21 = ((STR) &name29);
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L3 = FLISTT97637998(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   cur = L3;
   plus_self = str;
   noname2->arg1 = ((dSTR) IDENT_1415966123(cur.t1, cur.t2));
   L4 = STR_se405450305(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   plus_sarg = L4;
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   str = ret_val;
  }
 }
 after_loop: ;
 plus_self1 = ((STR) &Cyclic1005221544);
 plus_sarg1 = str;
 ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val1;
 plus_sarg2 = ((STR) &name18);
 ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
 PROG_err_STR(ATTR(self,prog), ret_val2);
}


#undef IS_ITER
#define IS_ITER 0

void FIND_G597583556(FIND_GENERICS self, dTP t) {
 ARRAYdTP L11;
 INT L21 = INT_zero;
 ERR1 create_self;
 ERR1 ret_val;
 ERR1 plus_self;
 dSTR plus_s;
 FILE1 stderr_self;
 FILE1 ret_val1;
 FILE1 r;
 INT L31_br;
 dTP aL31_;
 dTP L4;
 INT L51_;
 extern STR what1;
 FILE1 L6;
 OB L7;
 if ((t==((dTP) NULL))) {
  return;
 }
 if (t==NULL) {
  goto other109;
 } else
 switch (TAG(t)) {
  case TP_CLASS_tag:
   if (ATTR(((TP_CLASS) t),is_generic)) {
    FIND_G1798674529(self, ((TP_CLASS) t));
    {
     BOOL f_L31_ = TRUE;
     /* loop index variable */
     L21 = 0;
     L11 = ATTR(((TP_CLASS) t),params);
     L31_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
     while (1) {
      if(L21>=L31_br)  goto after_loop; 
      aL31_=ARR((ARRAYdTP)L11,L21); 
      FIND_G597583556(self, aL31_);
      L51_=INTPLUS(L21,1); 
      L21 = L51_;
     }
    }
    after_loop: ;
   } break;
  case TP_ROUT_tag: break;
  case TP_ITER_tag: break;
  default: ;
  other109: ;
   create_self = ((ERR1) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((dSTR) ((STR) &what1));
   stderr_self = ((FILE1) NULL);
   L7=ZALLOC(sizeof(struct FILE1_struct));
   if (L7==NULL) FATAL("Unable to allocate more memory");
   ((OB)L7)->header.tag=FILE1_tag;
   L6 = ((FILE1) L7);
   r = L6;
   
   SATTR(r,fp,stderr);
   ret_val1 = r;
   FILE_plus_dSTR(ret_val1, plus_s);
 }
 return;
}


#undef IS_ITER
#define IS_ITER 0

void FIND_T1058885549(FIND_TYPES self, AS_TYPE_SPEC inc_type) {
 ARRAYdTP params = ((ARRAYdTP) NULL);
 ARRAYIDENT pnames = ((ARRAYIDENT) NULL);
 dTP included_class = ((dTP) NULL);
 INT inc_param_c = INT_zero;
 TUPIDENTINT typeid = TUPIDENTINT_zero;
 AS_CLASS_DEF include_tree;
 AS_TYPE_SPEC inc_param;
 ARRAYdTP L11;
 AS_PARAM_DEC tree_pnames;
 ARRAYIDENT L21;
 TP_CONTEXT old_con;
 TUPIDENTINT dummy1 = TUPIDENTINT_zero;
 TUPIDENTINT create_self = TUPIDENTINT_zero;
 IDENT create_at1 = IDENT_zero;
 INT create_at2 = INT_zero;
 TUPIDENTINT ret_val = TUPIDENTINT_zero;
 FLISTTUPIDENTINT contains_self;
 TUPIDENTINT contains_e = TUPIDENTINT_zero;
 BOOL ret_val1 = BOOL_zero;
 FIND_TYPES tree_for_self;
 IDENT tree_for_nm = IDENT_zero;
 INT tree_for_num = INT_zero;
 AS_CLASS_DEF ret_val2;
 ARRAYdTP create_self1;
 INT create_n = INT_zero;
 ARRAYdTP ret_val3;
 ARRAYIDENT create_self2;
 INT create_n1 = INT_zero;
 ARRAYIDENT ret_val4;
 TP_CONTEXT create_self3;
 TP_CLASS create_same;
 ARRAYIDENT create_pnames;
 ARRAYdTP create_ptypes;
 PROG create_prog;
 TP_CONTEXT ret_val5;
 TP_CONTEXT r;
 TUPIDENTINT L3;
 TUPIDENTINT L4;
 dPARSE L5;
 BOOL L61_;
 ARRAYdTP L7;
 INT L8;
 OB L9;
 INT L10;
 INT L121_br;
INT i_L121_=0;
 dTP L13;
 ARRAYIDENT L14;
 INT L15;
 OB L16;
 INT L17;
 INT L181_br;
INT i_L181_=0;
 IDENT L19;
 TP_CONTEXT L20;
 OB L22;
 inc_param_c = AS_TYP2145685956(ATTR(inc_type,params));
 create_self = TUPIDENTINT_zero;
 create_at1 = ATTR(inc_type,name1);
 create_at2 = inc_param_c;
 L3 = create_self;
 L3.t1 = create_at1;
 L4 = L3;
 L4.t2 = create_at2;
 ret_val = L4;
 typeid = ret_val;
 contains_self = FIND_T1407637984;
 contains_e = typeid;
 ret_val1 = FLISTT1340999558(contains_self, contains_e);
 if (ret_val1) {
  FIND_T724224762(self);
  return;
 }
 FIND_T1407637984 = FLISTT1610849855(FIND_T1407637984, typeid);
 tree_for_self = self;
 tree_for_nm = ATTR(inc_type,name1);
 tree_for_num = inc_param_c;
 L5 = ATTR(ATTR(tree_for_self,prog),parse);
 ret_val2 = (*dPARSE1581792481[TAG(L5)])(L5, tree_for_nm, tree_for_num);
 include_tree = ret_val2;
 L61_=(inc_param_c)==(0); 
 if (L61_) {
  included_class = TP_CON2076339407(ATTR(self,con), inc_type, FALSE);
 }
 else {
  create_self1 = ((ARRAYdTP) NULL);
  create_n = inc_param_c;
  L8 = create_n;
  L10=(sizeof(struct ARRAYdTP_struct)-sizeof(dTP))+(L8)*sizeof(dTP);
  L9=ZALLOC_BIG(L10);
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=ARRAYdTP_tag;
#ifdef DESTROY_CHK

    ((OB)L9)->header.destroyed=0;
#endif

  L7 = ((ARRAYdTP) L9);
  L7->asize = L8;
  ret_val3 = L7;
  params = ret_val3;
  inc_param = ATTR(inc_type,params);
  {
   BOOL f_L121_ = TRUE;
   L11 = params;
   L121_br=L11==NULL?0:ASIZE((ARRAYdTP)L11); 
   i_L121_=0;
   while (1) {
    if ((inc_param==((AS_TYPE_SPEC) NULL))) {
     goto after_loop;
    }
    if(i_L121_>=L121_br)  goto after_loop; 
    L13 = TP_CON1800432689(ATTR(self,con), inc_param);
    SARR((ARRAYdTP)L11,i_L121_,(dTP)L13); i_L121_++;
    ;
    inc_param = ATTR(inc_param,next);
   }
  }
  after_loop: ;
  if ((include_tree==((AS_CLASS_DEF) NULL))) {
   return;
  }
  create_self2 = ((ARRAYIDENT) NULL);
  create_n1 = inc_param_c;
  L15 = create_n1;
  L17=(sizeof(struct ARRAYIDENT_struct)-sizeof(IDENT))+(L15)*sizeof(IDENT);
  L16=ZALLOC_BIG(L17);
  if (L16==NULL) FATAL("Unable to allocate more memory");
  ((OB)L16)->header.tag=ARRAYIDENT_tag;
#ifdef DESTROY_CHK

    ((OB)L16)->header.destroyed=0;
#endif

  L14 = ((ARRAYIDENT) L16);
  L14->asize = L15;
  ret_val4 = L14;
  pnames = ret_val4;
  tree_pnames = ATTR(include_tree,params);
  {
   BOOL f_L181_ = TRUE;
   L21 = pnames;
   L181_br=L21==NULL?0:ASIZE((ARRAYIDENT)L21); 
   i_L181_=0;
   while (1) {
    if ((tree_pnames==((AS_PARAM_DEC) NULL))) {
     goto after_loop1;
    }
    if(i_L181_>=L181_br)  goto after_loop1; 
    L19 = ATTR(tree_pnames,name1);
    SARR((ARRAYIDENT)L21,i_L181_,L19); i_L181_++;
    ;
    tree_pnames = ATTR(tree_pnames,next);
   }
  }
  after_loop1: ;
  included_class = TP_CON2076339407(ATTR(self,con), inc_type, FALSE);
  if (ATTR(ATTR(self,prog),check_generics)) {
   TP_TBL1525652988(ATTR(ATTR(self,prog),tp_tbl), ATTR(inc_type,name1), inc_param_c);
  }
 }
 old_con = ATTR(self,con);
 create_self3 = ((TP_CONTEXT) NULL);
 create_same = ATTR(ATTR(self,con),same1);
 create_pnames = pnames;
 create_ptypes = params;
 create_prog = ATTR(self,prog);
 L22=ZALLOC(sizeof(struct TP_CONTEXT_struct));
 if (L22==NULL) FATAL("Unable to allocate more memory");
 ((OB)L22)->header.tag=TP_CONTEXT_tag;
 L20 = ((TP_CONTEXT) L22);
 r = L20;
 SATTR(r,same1,create_same);
 SATTR(r,pnames,create_pnames);
 SATTR(r,ptypes,create_ptypes);
 SATTR(r,prog,create_prog);
 ret_val5 = r;
 SATTR(self,con,ret_val5);
 FIND_T896241716(self, ((dAS_NODE) include_tree));
 SATTR(self,con,old_con);
 dummy1 = FLISTT108744548(FIND_T1407637984);
}


#undef IS_ITER
#define IS_ITER 0

void FIND_T194079625(FIND_TYPES self) {
 AS_CLASS_DEF dummy_tree;
 TP_CLASS dummy1;
 PROG_AS_TBL L11;
 AS_CLASS_DEF nonlib_class;
 FSETTP_CLASS tp_doing;
 TP_CLASS tp = ((TP_CLASS) NULL);
 TP_CLASS_TBL L21;
 FSETTP_CLASS L31;
 AS_CLASS_DEF as_tree;
 INT L41 = INT_zero;
 FIND_TYPES err_loc_self;
 dPROG_ERR err_loc_t;
 FIND_TYPES tree_for_self;
 IDENT tree_for_nm = IDENT_zero;
 INT tree_for_num = INT_zero;
 AS_CLASS_DEF ret_val;
 TP_TBL tp_class_for_sel;
 IDENT tp_class_for_nam = IDENT_zero;
 ARRAYdTP tp_class_for_par;
 TP_CLASS ret_val1;
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
 OUT ret_val7;
 FILE1 stdout_self2;
 FILE1 ret_val8;
 FILE1 r2;
 OUT flush_self;
 FILE1 stdout_self3;
 FILE1 ret_val9;
 FILE1 r3;
 FILE1 flush_self1;
 TP_TBL tp_class_for_sel1;
 IDENT tp_class_for_nam1 = IDENT_zero;
 ARRAYdTP tp_class_for_par1;
 TP_CLASS ret_val10;
 FSETTP_CLASS create_self1;
 FSETTP_CLASS ret_val11;
 OUT create_self2;
 OUT ret_val12;
 OUT plus_self3;
 STR plus_s3;
 OUT ret_val13;
 FILE1 stdout_self4;
 FILE1 ret_val14;
 FILE1 r4;
 OUT plus_self4;
 STR plus_s4;
 OUT ret_val15;
 FILE1 stdout_self5;
 FILE1 ret_val16;
 FILE1 r5;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val17;
 FILE1 stdout_self6;
 FILE1 ret_val18;
 FILE1 r6;
 OUT flush_self2;
 FILE1 stdout_self7;
 FILE1 ret_val19;
 FILE1 r7;
 FILE1 flush_self3;
 OUT create_self3;
 OUT ret_val20;
 OUT plus_self6;
 STR plus_s6;
 OUT ret_val21;
 FILE1 stdout_self8;
 FILE1 ret_val22;
 FILE1 r8;
 OUT plus_self7;
 STR plus_s7;
 OUT ret_val23;
 FILE1 stdout_self9;
 FILE1 ret_val24;
 FILE1 r9;
 OUT plus_self8;
 STR plus_s8;
 OUT ret_val25;
 FILE1 stdout_self10;
 FILE1 ret_val26;
 FILE1 r10;
 OUT flush_self4;
 FILE1 stdout_self11;
 FILE1 ret_val27;
 FILE1 r11;
 FILE1 flush_self5;
 ERR1 create_self4;
 ERR1 ret_val28;
 ERR1 plus_self9;
 dSTR plus_s9;
 ERR1 ret_val29;
 FILE1 stderr_self;
 FILE1 ret_val30;
 FILE1 r12;
 ERR1 plus_self10;
 dSTR plus_s10;
 FILE1 stderr_self1;
 FILE1 ret_val31;
 FILE1 r13;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val32;
 STR plus_self11;
 STR plus_sarg;
 STR ret_val33;
 dPARSE L5;
 AS_CLASS_DEF L6;
 extern STR Adding1215795603;
 FILE1 L7;
 OB L8;
 FILE1 L10;
 OB L12;
 extern STR name9;
 FILE1 L14;
 OB L15;
 FILE1 L17;
 OB L18;
 INT L201_;
 TP_CLASS L22;
 BOOL L23;
 BOOL L241_;
 extern STR Adding1462271024;
 FILE1 L25;
 OB L26;
 FILE1 L28;
 OB L29;
 extern STR name9;
 FILE1 L32;
 OB L33;
 FILE1 L35;
 OB L36;
 TP_CLASS L38;
 extern STR findingtypesin;
 FILE1 L39;
 OB L40;
 FILE1 L43;
 OB L44;
 extern STR name9;
 FILE1 L46;
 OB L47;
 FILE1 L49;
 OB L50;
 BOOL L52;
 ARRAYdTP L53;
 INT L541_;
 INT L55;
 BOOL L561_;
 extern STR Finding;
 FILE1 L57;
 OB L58;
 IDENT L60;
 ARRAYdTP L61;
 INT L621_;
 FILE1 L63;
 OB L64;
 TP_TBL L66;
 IDENT L67;
 ARRAYdTP L68;
 INT L691_;
 dPARSE L70;
 IDENT L71;
 ARRAYdTP L72;
 INT L731_;
 dPARSE L74;
 BOOL L75;
 BOOL L761_;
 INT L77;
 INT L78;
 BOOL L791_;
 extern STR Probab986537740;
 err_loc_self = self;
 err_loc_t = ((dPROG_ERR) NULL);
 PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
 tree_for_self = self;
 tree_for_nm = IDENT_1150413730(IDENT_zero, ATTR(ATTR(self,prog),main_class));
 tree_for_num = 0;
 L5 = ATTR(ATTR(tree_for_self,prog),parse);
 ret_val = (*dPARSE1581792481[TAG(L5)])(L5, tree_for_nm, tree_for_num);
 dummy_tree = ret_val;
 tp_class_for_sel = ATTR(ATTR(self,prog),tp_tbl);
 tp_class_for_nam = IDENT_1150413730(IDENT_zero, ATTR(ATTR(self,prog),main_class));
 tp_class_for_par = ((ARRAYdTP) NULL);
 ret_val1 = TP_TBL944008484(tp_class_for_sel, tp_class_for_nam, tp_class_for_par, TRUE);
 dummy1 = ret_val1;
 {
  struct PROG_A1283019219_frame_struct other1_0;
PROG_A1283019219_frame noname1 = &other1_0;
  L11 = ATTR(ATTR(self,prog),as_tbl);
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L6 = PROG_A1283019219(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   nonlib_class = L6;
   if (ATTR(ATTR(self,prog),show_types)) {
    create_self = ((OUT) NULL);
    ret_val2 = create_self;
    plus_self = ret_val2;
    plus_s = ((STR) &Adding1215795603);
    stdout_self = ((FILE1) NULL);
    L8=ZALLOC(sizeof(struct FILE1_struct));
    if (L8==NULL) FATAL("Unable to allocate more memory");
    ((OB)L8)->header.tag=FILE1_tag;
    L7 = ((FILE1) L8);
    r = L7;
    
    SATTR(r,fp,stdout);
    ret_val4 = r;
    FILE_plus_STR(ret_val4, plus_s);
    ret_val3 = plus_self;
    plus_self1 = ret_val3;
    plus_s1 = ATTR(nonlib_class,name1).str;
    stdout_self1 = ((FILE1) NULL);
    L12=ZALLOC(sizeof(struct FILE1_struct));
    if (L12==NULL) FATAL("Unable to allocate more memory");
    ((OB)L12)->header.tag=FILE1_tag;
    L10 = ((FILE1) L12);
    r1 = L10;
    
    SATTR(r1,fp,stdout);
    ret_val6 = r1;
    FILE_plus_STR(ret_val6, plus_s1);
    ret_val5 = plus_self1;
    plus_self2 = ret_val5;
    plus_s2 = ((STR) &name9);
    stdout_self2 = ((FILE1) NULL);
    L15=ZALLOC(sizeof(struct FILE1_struct));
    if (L15==NULL) FATAL("Unable to allocate more memory");
    ((OB)L15)->header.tag=FILE1_tag;
    L14 = ((FILE1) L15);
    r2 = L14;
    
    SATTR(r2,fp,stdout);
    ret_val8 = r2;
    FILE_plus_STR(ret_val8, plus_s2);
    ret_val7 = plus_self2;
    flush_self = ret_val7;
    stdout_self3 = ((FILE1) NULL);
    L18=ZALLOC(sizeof(struct FILE1_struct));
    if (L18==NULL) FATAL("Unable to allocate more memory");
    ((OB)L18)->header.tag=FILE1_tag;
    L17 = ((FILE1) L18);
    r3 = L17;
    
    SATTR(r3,fp,stdout);
    ret_val9 = r3;
    flush_self1 = ret_val9;
    fflush(ATTR(flush_self1,fp));
   }
   if ((ATTR(nonlib_class,params)==((AS_PARAM_DEC) NULL))) {
    tp_class_for_sel1 = ATTR(ATTR(self,prog),tp_tbl);
    tp_class_for_nam1 = ATTR(nonlib_class,name1);
    tp_class_for_par1 = ((ARRAYdTP) NULL);
    ret_val10 = TP_TBL944008484(tp_class_for_sel1, tp_class_for_nam1, tp_class_for_par1, TRUE);
    dummy1 = ret_val10;
   }
  }
 }
 after_loop: ;
 {
  BOOL f_L201_ = TRUE;
  L41 = 0;
  L201_=L41-1; 
  while (1) {
   create_self1 = ((FSETTP_CLASS) NULL);
   ret_val11 = ((FSETTP_CLASS) NULL);
   tp_doing = ret_val11;
   {
    struct TP_CLA115798516_frame_struct other3_0;
TP_CLA115798516_frame noname2 = &other3_0;
    L21 = ATTR(ATTR(ATTR(self,prog),tp_tbl),class_tbl);
    noname2->self = L21;
    noname2->state = 0;
    while (1) {
     L22 = TP_CLA115798516(noname2);
     if (noname2->state == -1) {
      goto after_loop2;
     }
     tp = L22;
     L23 = FSETdT1764379880(ATTR(ATTR(self,prog),tp_done), ((dTP) tp));
     L241_=!(L23); 
     if (L241_) {
      if (ATTR(ATTR(self,prog),show_types)) {
       create_self2 = ((OUT) NULL);
       ret_val12 = create_self2;
       plus_self3 = ret_val12;
       plus_s3 = ((STR) &Adding1462271024);
       stdout_self4 = ((FILE1) NULL);
       L26=ZALLOC(sizeof(struct FILE1_struct));
       if (L26==NULL) FATAL("Unable to allocate more memory");
       ((OB)L26)->header.tag=FILE1_tag;
       L25 = ((FILE1) L26);
       r4 = L25;
       
       SATTR(r4,fp,stdout);
       ret_val14 = r4;
       FILE_plus_STR(ret_val14, plus_s3);
       ret_val13 = plus_self3;
       plus_self4 = ret_val13;
       plus_s4 = TP_CLASS_strrSTR(tp);
       stdout_self5 = ((FILE1) NULL);
       L29=ZALLOC(sizeof(struct FILE1_struct));
       if (L29==NULL) FATAL("Unable to allocate more memory");
       ((OB)L29)->header.tag=FILE1_tag;
       L28 = ((FILE1) L29);
       r5 = L28;
       
       SATTR(r5,fp,stdout);
       ret_val16 = r5;
       FILE_plus_STR(ret_val16, plus_s4);
       ret_val15 = plus_self4;
       plus_self5 = ret_val15;
       plus_s5 = ((STR) &name9);
       stdout_self6 = ((FILE1) NULL);
       L33=ZALLOC(sizeof(struct FILE1_struct));
       if (L33==NULL) FATAL("Unable to allocate more memory");
       ((OB)L33)->header.tag=FILE1_tag;
       L32 = ((FILE1) L33);
       r6 = L32;
       
       SATTR(r6,fp,stdout);
       ret_val18 = r6;
       FILE_plus_STR(ret_val18, plus_s5);
       ret_val17 = plus_self5;
       flush_self2 = ret_val17;
       stdout_self7 = ((FILE1) NULL);
       L36=ZALLOC(sizeof(struct FILE1_struct));
       if (L36==NULL) FATAL("Unable to allocate more memory");
       ((OB)L36)->header.tag=FILE1_tag;
       L35 = ((FILE1) L36);
       r7 = L35;
       
       SATTR(r7,fp,stdout);
       ret_val19 = r7;
       flush_self3 = ret_val19;
       fflush(ATTR(flush_self3,fp));
      }
      tp_doing = FSETTP1186813482(tp_doing, tp);
     }
    }
   }
   after_loop2: ;
   if ((tp_doing==((FSETTP_CLASS) NULL))) {
    goto after_loop1;
   }
   {
    struct FSETTP340014621_frame_struct other4_0;
FSETTP340014621_frame noname3 = &other4_0;
    L31 = tp_doing;
    noname3->self = L31;
    noname3->state = 0;
    while (1) {
     L38 = FSETTP340014621(noname3);
     if (noname3->state == -1) {
      goto after_loop3;
     }
     tp = L38;
     if (ATTR(ATTR(self,prog),show_types)) {
      create_self3 = ((OUT) NULL);
      ret_val20 = create_self3;
      plus_self6 = ret_val20;
      plus_s6 = ((STR) &findingtypesin);
      stdout_self8 = ((FILE1) NULL);
      L40=ZALLOC(sizeof(struct FILE1_struct));
      if (L40==NULL) FATAL("Unable to allocate more memory");
      ((OB)L40)->header.tag=FILE1_tag;
      L39 = ((FILE1) L40);
      r8 = L39;
      
      SATTR(r8,fp,stdout);
      ret_val22 = r8;
      FILE_plus_STR(ret_val22, plus_s6);
      ret_val21 = plus_self6;
      plus_self7 = ret_val21;
      plus_s7 = TP_CLASS_strrSTR(tp);
      stdout_self9 = ((FILE1) NULL);
      L44=ZALLOC(sizeof(struct FILE1_struct));
      if (L44==NULL) FATAL("Unable to allocate more memory");
      ((OB)L44)->header.tag=FILE1_tag;
      L43 = ((FILE1) L44);
      r9 = L43;
      
      SATTR(r9,fp,stdout);
      ret_val24 = r9;
      FILE_plus_STR(ret_val24, plus_s7);
      ret_val23 = plus_self7;
      plus_self8 = ret_val23;
      plus_s8 = ((STR) &name9);
      stdout_self10 = ((FILE1) NULL);
      L47=ZALLOC(sizeof(struct FILE1_struct));
      if (L47==NULL) FATAL("Unable to allocate more memory");
      ((OB)L47)->header.tag=FILE1_tag;
      L46 = ((FILE1) L47);
      r10 = L46;
      
      SATTR(r10,fp,stdout);
      ret_val26 = r10;
      FILE_plus_STR(ret_val26, plus_s8);
      ret_val25 = plus_self8;
      flush_self4 = ret_val25;
      stdout_self11 = ((FILE1) NULL);
      L50=ZALLOC(sizeof(struct FILE1_struct));
      if (L50==NULL) FATAL("Unable to allocate more memory");
      ((OB)L50)->header.tag=FILE1_tag;
      L49 = ((FILE1) L50);
      r11 = L49;
      
      SATTR(r11,fp,stdout);
      ret_val27 = r11;
      flush_self5 = ret_val27;
      fflush(ATTR(flush_self5,fp));
     }
     if (ATTR(ATTR(self,prog),check_generics)) {
      L53 = ATTR(tp,params);
      L541_=(L53)==NULL?0:ASIZE((ARRAYdTP)L53); 
      L55 = L541_;
      L561_=(0)<(L55); 
      L52 = L561_;
     } else {
      L52 = FALSE;
     }
     if (L52) {
      if (ATTR(ATTR(self,prog),trace_generics)) {
       create_self4 = ((ERR1) NULL);
       ret_val28 = create_self4;
       plus_self9 = ret_val28;
       plus_s9 = ((dSTR) ((STR) &Finding));
       stderr_self = ((FILE1) NULL);
       L58=ZALLOC(sizeof(struct FILE1_struct));
       if (L58==NULL) FATAL("Unable to allocate more memory");
       ((OB)L58)->header.tag=FILE1_tag;
       L57 = ((FILE1) L58);
       r12 = L57;
       
       SATTR(r12,fp,stderr);
       ret_val30 = r12;
       FILE_plus_dSTR(ret_val30, plus_s9);
       ret_val29 = plus_self9;
       plus_self10 = ret_val29;
       L60 = ATTR(tp,name1);
       L61 = ATTR(tp,params);
       L621_=(L61)==NULL?0:ASIZE((ARRAYdTP)L61); 
       plus_s10 = ((dSTR) IDENT_1415966123(L60, L621_));
       stderr_self1 = ((FILE1) NULL);
       L64=ZALLOC(sizeof(struct FILE1_struct));
       if (L64==NULL) FATAL("Unable to allocate more memory");
       ((OB)L64)->header.tag=FILE1_tag;
       L63 = ((FILE1) L64);
       r13 = L63;
       
       SATTR(r13,fp,stderr);
       ret_val31 = r13;
       FILE_plus_dSTR(ret_val31, plus_s10);
      }
      L66 = ATTR(ATTR(self,prog),tp_tbl);
      L67 = ATTR(tp,name1);
      L68 = ATTR(tp,params);
      L691_=(L68)==NULL?0:ASIZE((ARRAYdTP)L68); 
      TP_TBL1525652988(L66, L67, L691_);
     }
     SATTR(ATTR(self,prog),tp_done,FSETdT1016814448(ATTR(ATTR(self,prog),tp_done), ((dTP) tp)));
     as_tree_self = tp;
     if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
      L70 = ATTR(ATTR(as_tree_self,prog),parse);
      L71 = ATTR(as_tree_self,name1);
      L72 = ATTR(as_tree_self,params);
      L731_=(L72)==NULL?0:ASIZE((ARRAYdTP)L72); 
      L74 = L70;
      SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L74)])(L74, L71, L731_));
     }
     ret_val32 = ATTR(as_tree_self,my_as_tree);
     as_tree = ret_val32;
     L75 = (as_tree==((AS_CLASS_DEF) NULL));
     L761_=!(L75); 
     if (L761_) {
      SATTR(self,con,TP_CLA821242177(tp));
      FIND_T896241716(self, ((dAS_NODE) as_tree));
     }
    }
   }
   after_loop3: ;
   L201_++; 
   L78 = L201_;
   L791_=(100)<(L78); 
   if (L791_) {
    plus_self11 = ((STR) &Probab986537740);
    plus_sarg = TP_CLASS_strrSTR(tp);
    ret_val33 = STR_ap2004550586(plus_self11, plus_sarg);
    PROG_err_STR(ATTR(self,prog), ret_val33);
    goto after_loop1;
   }
  }
 }
 after_loop1: ;
}


#undef IS_ITER
#define IS_ITER 0

void FIND_T481437519(FIND_TYPES self, dTP t) {
 IDENT nm = IDENT_zero;
 INT pnum = INT_zero;
 INT L11 = INT_zero;
 INT L21 = INT_zero;
 INT L31 = INT_zero;
 INT L41 = INT_zero;
 INT L51 = INT_zero;
 INT L61 = INT_zero;
 OUT create_self;
 OUT ret_val;
 OUT plus_self;
 STR plus_s;
 OUT ret_val1;
 FILE1 stdout_self;
 FILE1 ret_val2;
 FILE1 r;
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
 OUT flush_self;
 FILE1 stdout_self3;
 FILE1 ret_val7;
 FILE1 r3;
 FILE1 flush_self1;
 ERR1 create_self1;
 ERR1 ret_val8;
 ERR1 plus_self3;
 dSTR plus_s3;
 ERR1 ret_val9;
 FILE1 stderr_self;
 FILE1 ret_val10;
 FILE1 r4;
 ERR1 plus_self4;
 dSTR plus_s4;
 ERR1 ret_val11;
 FILE1 stderr_self1;
 FILE1 ret_val12;
 FILE1 r5;
 ERR1 plus_self5;
 dSTR plus_s5;
 FILE1 stderr_self2;
 FILE1 ret_val13;
 FILE1 r6;
 TP_CLASS as_tree_self;
 AS_CLASS_DEF ret_val14;
 BOOL L7;
 BOOL L8;
 BOOL L9;
 BOOL L10;
 BOOL L12;
 dTP L13;
 STR L14;
 extern STR STR1;
 BOOL L15;
 BOOL L161_;
 dTP L17;
 STR L18;
 extern STR INT1;
 BOOL L19;
 BOOL L201_;
 dTP L22;
 STR L23;
 extern STR FLT1;
 BOOL L24;
 BOOL L251_;
 dTP L26;
 STR L27;
 extern STR BOOL1;
 BOOL L28;
 BOOL L291_;
 dTP L30;
 STR L32;
 extern STR FLTD1;
 BOOL L33;
 BOOL L341_;
 dTP L35;
 STR L36;
 extern STR OUT1;
 BOOL L37;
 BOOL L381_;
 extern STR name30;
 FILE1 L39;
 OB L40;
 dTP L43;
 FILE1 L44;
 OB L45;
 extern STR name7;
 FILE1 L47;
 OB L48;
 FILE1 L50;
 OB L52;
 ARRAYdTP L54;
 INT L551_;
 extern STR process_tp;
 FILE1 L56;
 OB L57;
 FILE1 L59;
 OB L60;
 extern STR name31;
 FILE1 L63;
 OB L64;
 BOOL L66;
 dPARSE L67;
 IDENT L68;
 ARRAYdTP L69;
 INT L701_;
 dPARSE L71;
 BOOL L72;
 BOOL L731_;
 BOOL L741_;
 INT L761_;
 INT L751_,L751_m;
 ARRAYdTP L77;
 INT L78;
 INT L79;
 dTP L801_;
 BOOL L81;
 BOOL L821_;
 ARRAYARG L84;
 INT L851_;
 INT L86;
 INT L871_;
 INT L831_,L831_m;
 ARRAYARG L88;
 INT L89;
 INT L90;
 ARG L911_;
 ARG L92;
 BOOL L93;
 BOOL L941_;
 BOOL L95;
 BOOL L961_;
 ARRAYARG L98;
 INT L991_;
 INT L100;
 INT L1011_;
 INT L971_,L971_m;
 ARRAYARG L102;
 INT L103;
 INT L104;
 ARG L1051_;
 ARG L106;
 BOOL L107;
 BOOL L1081_;
 if ((t==((dTP) NULL))) {
  return;
 }
 if (FSETdT1764379880(ATTR(ATTR(self,prog),tp_done), t)) {
  return;
 }
 if (ATTR(ATTR(self,prog),show_types)) {
  L13 = t;
  L14 = (*dTP_strrSTR[TAG(L13)])(L13);
  L15 = STR_is111530248(L14, ((STR) &STR1));
  L161_=!(L15); 
  if (L161_) {
   L17 = t;
   L18 = (*dTP_strrSTR[TAG(L17)])(L17);
   L19 = STR_is111530248(L18, ((STR) &INT1));
   L201_=!(L19); 
   L12 = L201_;
  } else {
   L12 = FALSE;
  }
  if (L12) {
   L22 = t;
   L23 = (*dTP_strrSTR[TAG(L22)])(L22);
   L24 = STR_is111530248(L23, ((STR) &FLT1));
   L251_=!(L24); 
   L10 = L251_;
  } else {
   L10 = FALSE;
  }
  if (L10) {
   L26 = t;
   L27 = (*dTP_strrSTR[TAG(L26)])(L26);
   L28 = STR_is111530248(L27, ((STR) &BOOL1));
   L291_=!(L28); 
   L9 = L291_;
  } else {
   L9 = FALSE;
  }
  if (L9) {
   L30 = t;
   L32 = (*dTP_strrSTR[TAG(L30)])(L30);
   L33 = STR_is111530248(L32, ((STR) &FLTD1));
   L341_=!(L33); 
   L8 = L341_;
  } else {
   L8 = FALSE;
  }
  if (L8) {
   L35 = t;
   L36 = (*dTP_strrSTR[TAG(L35)])(L35);
   L37 = STR_is111530248(L36, ((STR) &OUT1));
   L381_=!(L37); 
   L7 = L381_;
  } else {
   L7 = FALSE;
  }
  if (L7) {
   create_self = ((OUT) NULL);
   ret_val = create_self;
   plus_self = ret_val;
   plus_s = ((STR) &name30);
   stdout_self = ((FILE1) NULL);
   L40=ZALLOC(sizeof(struct FILE1_struct));
   if (L40==NULL) FATAL("Unable to allocate more memory");
   ((OB)L40)->header.tag=FILE1_tag;
   L39 = ((FILE1) L40);
   r = L39;
   
   SATTR(r,fp,stdout);
   ret_val2 = r;
   FILE_plus_STR(ret_val2, plus_s);
   ret_val1 = plus_self;
   plus_self1 = ret_val1;
   L43 = t;
   plus_s1 = (*dTP_strrSTR[TAG(L43)])(L43);
   stdout_self1 = ((FILE1) NULL);
   L45=ZALLOC(sizeof(struct FILE1_struct));
   if (L45==NULL) FATAL("Unable to allocate more memory");
   ((OB)L45)->header.tag=FILE1_tag;
   L44 = ((FILE1) L45);
   r1 = L44;
   
   SATTR(r1,fp,stdout);
   ret_val4 = r1;
   FILE_plus_STR(ret_val4, plus_s1);
   ret_val3 = plus_self1;
   plus_self2 = ret_val3;
   plus_s2 = ((STR) &name7);
   stdout_self2 = ((FILE1) NULL);
   L48=ZALLOC(sizeof(struct FILE1_struct));
   if (L48==NULL) FATAL("Unable to allocate more memory");
   ((OB)L48)->header.tag=FILE1_tag;
   L47 = ((FILE1) L48);
   r2 = L47;
   
   SATTR(r2,fp,stdout);
   ret_val6 = r2;
   FILE_plus_STR(ret_val6, plus_s2);
   ret_val5 = plus_self2;
   flush_self = ret_val5;
   stdout_self3 = ((FILE1) NULL);
   L52=ZALLOC(sizeof(struct FILE1_struct));
   if (L52==NULL) FATAL("Unable to allocate more memory");
   ((OB)L52)->header.tag=FILE1_tag;
   L50 = ((FILE1) L52);
   r3 = L50;
   
   SATTR(r3,fp,stdout);
   ret_val7 = r3;
   flush_self1 = ret_val7;
   fflush(ATTR(flush_self1,fp));
  }
 }
 switch (TAG(t)) {
  case TP_CLASS_tag:
   nm = ATTR(((TP_CLASS) t),name1);
   L54 = ATTR(((TP_CLASS) t),params);
   L551_=(L54)==NULL?0:ASIZE((ARRAYdTP)L54); 
   pnum = L551_;
   if (ATTR(((TP_CLASS) t),is_generic)) {
    create_self1 = ((ERR1) NULL);
    ret_val8 = create_self1;
    plus_self3 = ret_val8;
    plus_s3 = ((dSTR) ((STR) &process_tp));
    stderr_self = ((FILE1) NULL);
    L57=ZALLOC(sizeof(struct FILE1_struct));
    if (L57==NULL) FATAL("Unable to allocate more memory");
    ((OB)L57)->header.tag=FILE1_tag;
    L56 = ((FILE1) L57);
    r4 = L56;
    
    SATTR(r4,fp,stderr);
    ret_val10 = r4;
    FILE_plus_dSTR(ret_val10, plus_s3);
    ret_val9 = plus_self3;
    plus_self4 = ret_val9;
    plus_s4 = ((dSTR) TP_CLASS_strrSTR(((TP_CLASS) t)));
    stderr_self1 = ((FILE1) NULL);
    L60=ZALLOC(sizeof(struct FILE1_struct));
    if (L60==NULL) FATAL("Unable to allocate more memory");
    ((OB)L60)->header.tag=FILE1_tag;
    L59 = ((FILE1) L60);
    r5 = L59;
    
    SATTR(r5,fp,stderr);
    ret_val12 = r5;
    FILE_plus_dSTR(ret_val12, plus_s4);
    ret_val11 = plus_self4;
    plus_self5 = ret_val11;
    plus_s5 = ((dSTR) ((STR) &name31));
    stderr_self2 = ((FILE1) NULL);
    L64=ZALLOC(sizeof(struct FILE1_struct));
    if (L64==NULL) FATAL("Unable to allocate more memory");
    ((OB)L64)->header.tag=FILE1_tag;
    L63 = ((FILE1) L64);
    r6 = L63;
    
    SATTR(r6,fp,stderr);
    ret_val13 = r6;
    FILE_plus_dSTR(ret_val13, plus_s5);
   }
   as_tree_self = ((TP_CLASS) t);
   if ((ATTR(as_tree_self,my_as_tree)==((AS_CLASS_DEF) NULL))) {
    L67 = ATTR(ATTR(as_tree_self,prog),parse);
    L68 = ATTR(as_tree_self,name1);
    L69 = ATTR(as_tree_self,params);
    L701_=(L69)==NULL?0:ASIZE((ARRAYdTP)L69); 
    L71 = L67;
    SATTR(as_tree_self,my_as_tree,(*dPARSE1581792481[TAG(L71)])(L71, L68, L701_));
   }
   ret_val14 = ATTR(as_tree_self,my_as_tree);
   L72 = (ret_val14==((AS_CLASS_DEF) NULL));
   L731_=!(L72); 
   if (L731_) {
    L741_=(0)<(pnum); 
    L66 = L741_;
   } else {
    L66 = FALSE;
   }
   if (L66) {
    {
     BOOL f_L751_ = TRUE;
     L11 = 0;
     L761_=INTMINUS(pnum,1); 
     L21 = L761_;
     L751_=L11-1;L751_m=L21; 
     while (1) {
      L77 = ATTR(((TP_CLASS) t),params);
      if(L751_++>=L751_m)  goto after_loop; 
      L79 = L751_;
      L801_=(dTP)ARR((ARRAYdTP)L77,L79); 
      FIND_T481437519(self, L801_);
     }
    }
    after_loop: ;
   } break;
  case TP_ROUT_tag:
   SATTR(ATTR(self,prog),tp_done,FSETdT1016814448(ATTR(ATTR(self,prog),tp_done), t));
   L81 = (ATTR(((TP_ROUT) t),args)==((ARRAYARG) NULL));
   L821_=!(L81); 
   if (L821_) {
    {
     BOOL f_L831_ = TRUE;
     L31 = 0;
     L84 = ATTR(((TP_ROUT) t),args);
     L851_=(L84)==NULL?0:ASIZE((ARRAYARG)L84); 
     L86 = L851_;
     L871_=INTMINUS(L86,1); 
     L41 = L871_;
     L831_=L31-1;L831_m=L41; 
     while (1) {
      L88 = ATTR(((TP_ROUT) t),args);
      if(L831_++>=L831_m)  goto after_loop1; 
      L90 = L831_;
      L911_=(ARG)ARR((ARRAYARG)L88,L90); 
      L92=L911_;
      FIND_T481437519(self, ATTR(L92,tp));
     }
    }
    after_loop1: ;
   }
   L93 = (ATTR(((TP_ROUT) t),ret)==((dTP) NULL));
   L941_=!(L93); 
   if (L941_) {
    FIND_T481437519(self, ATTR(((TP_ROUT) t),ret));
   } break;
  case TP_ITER_tag:
   SATTR(ATTR(self,prog),tp_done,FSETdT1016814448(ATTR(ATTR(self,prog),tp_done), t));
   L95 = (ATTR(((TP_ITER) t),args)==((ARRAYARG) NULL));
   L961_=!(L95); 
   if (L961_) {
    {
     BOOL f_L971_ = TRUE;
     L51 = 0;
     L98 = ATTR(((TP_ITER) t),args);
     L991_=(L98)==NULL?0:ASIZE((ARRAYARG)L98); 
     L100 = L991_;
     L1011_=INTMINUS(L100,1); 
     L61 = L1011_;
     L971_=L51-1;L971_m=L61; 
     while (1) {
      L102 = ATTR(((TP_ITER) t),args);
      if(L971_++>=L971_m)  goto after_loop2; 
      L104 = L971_;
      L1051_=(ARG)ARR((ARRAYARG)L102,L104); 
      L106=L1051_;
      FIND_T481437519(self, ATTR(L106,tp));
     }
    }
    after_loop2: ;
   }
   L107 = (ATTR(((TP_ITER) t),ret)==((dTP) NULL));
   L1081_=!(L107); 
   if (L1081_) {
    FIND_T481437519(self, ATTR(((TP_ITER) t),ret));
   } break;
  default: ;
   FATAL("No applicable type in typecase\nin FIND_TYPES::process_tp($TP)\n./Front/find_types.sa:124:16");
 }
}


#undef IS_ITER
#define IS_ITER 0

void FIND_T724224762(FIND_TYPES self) {
 STR str = ((STR) NULL);
 FLISTTUPIDENTINT L11;
 TUPIDENTINT cur = TUPIDENTINT_zero;
 STR L21;
 STR plus_self;
 STR plus_sarg;
 STR ret_val;
 STR plus_self1;
 STR plus_sarg1;
 STR ret_val1;
 STR plus_self2;
 STR plus_sarg2;
 STR ret_val2;
 extern STR name29;
 TUPIDENTINT L3;
 STR L4;
 extern STR Cyclic1005221544;
 extern STR name18;
 {
  struct FLISTT97637998_frame_struct other1_0;
FLISTT97637998_frame noname1 = &other1_0;
  struct STR_se405450305_frame_struct other1_1;
STR_se405450305_frame noname2 = &other1_1;
  L11 = FIND_T1407637984;
  L21 = ((STR) &name29);
  noname1->self = L11;
  noname1->state = 0;
  noname2->self = L21;
  noname2->state = 0;
  while (1) {
   L3 = FLISTT97637998(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   cur = L3;
   plus_self = str;
   noname2->arg1 = ((dSTR) IDENT_1415966123(cur.t1, cur.t2));
   L4 = STR_se405450305(noname2);
   if (noname2->state == -1) {
    goto after_loop;
   }
   plus_sarg = L4;
   ret_val = STR_ap2004550586(plus_self, plus_sarg);
   str = ret_val;
  }
 }
 after_loop: ;
 plus_self1 = ((STR) &Cyclic1005221544);
 plus_sarg1 = str;
 ret_val1 = STR_ap2004550586(plus_self1, plus_sarg1);
 plus_self2 = ret_val1;
 plus_sarg2 = ((STR) &name18);
 ret_val2 = STR_ap2004550586(plus_self2, plus_sarg2);
 PROG_err_STR(ATTR(self,prog), ret_val2);
}


#undef IS_ITER
#define IS_ITER 0

void FIND_T896241716(FIND_TYPES self, dAS_NODE as) {
 dAS_NODE as_n = ((dAS_NODE) NULL);
 FIND_TYPES err_loc_self;
 dPROG_ERR err_loc_t;
 while (1) {
  if ((as==((dAS_NODE) NULL))) {
   goto after_loop;
  }
  err_loc_self = self;
  err_loc_t = ((dPROG_ERR) as);
  PROG_e176405615(ATTR(err_loc_self,prog), err_loc_t);
  switch (TAG(as)) {
   case AS_CLASS_DEF_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),params)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),under)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),over)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CLASS_DEF) as),body)));
    return; break;
   case AS_PARAM_DEC_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PARAM_DEC) as),type_constraint)));
    as_n = ((dAS_NODE) ATTR(((AS_PARAM_DEC) as),next)); break;
   case AS_TYPE_SPEC_tag:
    PROG_e176405615(ATTR(self,prog), ((dPROG_ERR) as));
    FIND_T481437519(self, TP_CON1800432689(ATTR(self,con), ((AS_TYPE_SPEC) as)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPE_SPEC) as),next)); break;
   case AS_INC1416917001_tag:
    FIND_T1058885549(self, ATTR(((AS_INC1416917001) as),tp));
    as_n = ((dAS_NODE) ATTR(((AS_INC1416917001) as),next)); break;
   case AS_CONST_DEF_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CONST_DEF) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CONST_DEF) as),init)));
    as_n = ((dAS_NODE) ATTR(((AS_CONST_DEF) as),next)); break;
   case AS_SHARED_DEF_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),init)));
    as_n = ((dAS_NODE) ATTR(((AS_SHARED_DEF) as),next)); break;
   case AS_ATTR_DEF_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ATTR_DEF) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_ATTR_DEF) as),next)); break;
   case AS_ROUT_DEF_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),args_dec)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),ret_dec)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),pre_e)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),post_e)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_ROUT_DEF) as),next)); break;
   case AS_STMT_LIST_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_STMT_LIST) as),stmts)));
    return; break;
   case AS_ARG_DEC_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ARG_DEC) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_ARG_DEC) as),next)); break;
   case AS_DEC_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_DEC_STMT) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_DEC_STMT) as),next)); break;
   case AS_ASSIGN_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),lhs_expr)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),rhs)));
    as_n = ((dAS_NODE) ATTR(((AS_ASSIGN_STMT) as),next)); break;
   case AS_IF_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),test1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),then_part)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_IF_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_IF_STMT) as),next)); break;
   case AS_LOOP_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOOP_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_LOOP_STMT) as),next)); break;
   case AS_RETURN_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_RETURN_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_RETURN_STMT) as),next)); break;
   case AS_YIELD_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_YIELD_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_YIELD_STMT) as),next)); break;
   case AS_QUIT_STMT_tag:
    as_n = ((dAS_NODE) ATTR(((AS_QUIT_STMT) as),next)); break;
   case AS_CASE_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),test1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),when_part)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CASE_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_CASE_STMT) as),next)); break;
   case AS_CASE_WHEN_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),val1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_CASE_WHEN) as),next)); break;
   case AS_TYPECASE_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),when_part)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPECASE_STMT) as),next)); break;
   case AS_TYPECASE_WHEN_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_TYPECASE_WHEN) as),next)); break;
   case AS_ASSERT_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ASSERT_STMT) as),test1)));
    as_n = ((dAS_NODE) ATTR(((AS_ASSERT_STMT) as),next)); break;
   case AS_PROTECT_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),body)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),when_part)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_PROTECT_STMT) as),next)); break;
   case AS_PROTECT_WHEN_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_PROTECT_WHEN) as),next)); break;
   case AS_RAISE_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_RAISE_STMT) as),val1)));
    as_n = ((dAS_NODE) ATTR(((AS_RAISE_STMT) as),next)); break;
   case AS_EXPR_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_EXPR_STMT) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_EXPR_STMT) as),next)); break;
   case AS_SELF_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_SELF_EXPR) as),next)); break;
   case AS_CALL_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),ob)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),args)));
    as_n = ((dAS_NODE) ATTR(((AS_CALL_EXPR) as),next)); break;
   case AS_VOID_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_VOID_EXPR) as),next)); break;
   case AS_IS_VOID_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_IS_VOID_EXPR) as),arg)));
    as_n = ((dAS_NODE) ATTR(((AS_IS_VOID_EXPR) as),next)); break;
   case AS_ARRAY_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ARRAY_EXPR) as),elts)));
    as_n = ((dAS_NODE) ATTR(((AS_ARRAY_EXPR) as),next)); break;
   case AS_CREATE_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),tp)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),elts)));
    as_n = ((dAS_NODE) ATTR(((AS_CREATE_EXPR) as),next)); break;
   case AS_BOU14269336_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_BOU14269336) as),call1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_BOU14269336) as),ret)));
    as_n = ((dAS_NODE) ATTR(((AS_BOU14269336) as),next)); break;
   case AS_UND152986614_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_UND152986614) as),tp)));
    as_n = ((dAS_NODE) ATTR(((AS_UND152986614) as),next)); break;
   case AS_AND_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_AND_EXPR) as),e1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_AND_EXPR) as),e2)));
    as_n = ((dAS_NODE) ATTR(((AS_AND_EXPR) as),next)); break;
   case AS_OR_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_OR_EXPR) as),e1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_OR_EXPR) as),e2)));
    as_n = ((dAS_NODE) ATTR(((AS_OR_EXPR) as),next)); break;
   case AS_EXCEPT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_EXCEPT_EXPR) as),next)); break;
   case AS_NEW_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_NEW_EXPR) as),arg)));
    as_n = ((dAS_NODE) ATTR(((AS_NEW_EXPR) as),next)); break;
   case AS_INITIAL_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_INITIAL_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_INITIAL_EXPR) as),next)); break;
   case AS_BREAK_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_BREAK_EXPR) as),next)); break;
   case AS_RESULT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_RESULT_EXPR) as),next)); break;
   case AS_BOOL_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_BOOL_LIT_EXPR) as),next)); break;
   case AS_CHAR_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CHAR_LIT_EXPR) as),next)); break;
   case AS_STR_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_STR_LIT_EXPR) as),next)); break;
   case AS_INT_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_INT_LIT_EXPR) as),next)); break;
   case AS_FLT_LIT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_FLT_LIT_EXPR) as),next)); break;
   case AS_PAR_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_PAR_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_PAR_STMT) as),next)); break;
   case AS_ATTACH_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),lhs)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),rhs)));
    as_n = ((dAS_NODE) ATTR(((AS_ATTACH_STMT) as),next)); break;
   case AS_LOCK_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),e_list)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_LOCK_STMT) as),next)); break;
   case AS_LOCK_IF_WHEN_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),val1)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),e_list)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),then_part)));
    as_n = ((dAS_NODE) ATTR(((AS_LOCK_IF_WHEN) as),next)); break;
   case AS_UNLOCK_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_UNLOCK_STMT) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_UNLOCK_STMT) as),next)); break;
   case AS_WIT1063437351_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_WIT1063437351) as),near_part)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_WIT1063437351) as),else_part)));
    as_n = ((dAS_NODE) ATTR(((AS_WIT1063437351) as),next)); break;
   case AS_FORK_STMT_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_FORK_STMT) as),at)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_FORK_STMT) as),body)));
    as_n = ((dAS_NODE) ATTR(((AS_FORK_STMT) as),next)); break;
   case AS_INT2031239268_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_INT2031239268) as),at)));
    as_n = ((dAS_NODE) ATTR(((AS_INT2031239268) as),next)); break;
   case AS_SYNC_STMT_tag:
    as_n = ((dAS_NODE) ATTR(((AS_SYNC_STMT) as),next)); break;
   case AS_HERE_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_HERE_EXPR) as),next)); break;
   case AS_WHERE_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_WHERE_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_WHERE_EXPR) as),next)); break;
   case AS_NEAR_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_NEAR_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_NEAR_EXPR) as),next)); break;
   case AS_FAR_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_FAR_EXPR) as),e)));
    as_n = ((dAS_NODE) ATTR(((AS_FAR_EXPR) as),next)); break;
   case AS_AT_EXPR_tag:
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_AT_EXPR) as),e)));
    FIND_T896241716(self, ((dAS_NODE) ATTR(((AS_AT_EXPR) as),at)));
    as_n = ((dAS_NODE) ATTR(((AS_AT_EXPR) as),next)); break;
   case AS_ANY_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_ANY_EXPR) as),next)); break;
   case AS_CLUSTER_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CLUSTER_EXPR) as),next)); break;
   case AS_CLU553570599_tag:
    as_n = ((dAS_NODE) ATTR(((AS_CLU553570599) as),next)); break;
   case AS_COHORT_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_COHORT_EXPR) as),next)); break;
   case AS_GLOBAL_EXPR_tag:
    as_n = ((dAS_NODE) ATTR(((AS_GLOBAL_EXPR) as),next)); break;
   default: ;
    FATAL("No applicable type in typecase\nin FIND_TYPES::do_tps($AS_NODE)\n./Front/find_types.sa:159:20");
  }
  as = as_n;
 }
 after_loop: ;
}


#undef IS_ITER
#define IS_ITER 1

ABSTRACT_LAYOUT FLISTA1252869158(FLISTA1252869158_frame frame) {
 ABSTRACT_LAYOUT dummy = ((ABSTRACT_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 ABSTRACT_LAYOUT L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA162798263) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(ABSTRACT_LAYOUT)ARR((FLISTA162798263)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_BND1124877163 FLISTA273121893(FLISTA273121893_frame frame) {
 AM_BND1124877163 dummy = ((AM_BND1124877163) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_BND1124877163 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA1893461511) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_BND1124877163)ARR((FLISTA1893461511)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_BND367211769 FLISTA1446310868(FLISTA1446310868_frame frame) {
 AM_BND367211769 dummy = ((AM_BND367211769) NULL);
 BOOL L1;
 BOOL L21_;
 BOOL L31_;
 AM_BND367211769 L41_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FLISTA1409846210) NULL));
 L21_=!(L1); 
 if (L21_) {
  frame->i = 0;
  frame->sz = ATTR(frame->self,loc);
  while (1) {
   L31_=(frame->i)==(frame->sz); 
   if (L31_) {
    goto after_loop;
   }
   frame->state = 1;
   frame->aget_self = frame->self;
   frame->aget_ind = frame->i;
   L41_=(AM_BND367211769)ARR((FLISTA1409846210)frame->aget_self,frame->aget_ind); 
   frame->ret_val = L41_;
   return frame->ret_val;
   state1:;
   L51_=INTPLUS(frame->i,1); 
   frame->i = L51_;
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void FLISTA2015944564(FLISTA2015944564_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA1409846210)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA1409846210)frame->self,frame->i_I_u_I,(AM_BND367211769)frame->arg1); frame->i_I_u_I++;
   ;
   frame->state = 1;
   return;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return;
}


#undef IS_ITER
#define IS_ITER 1

void FLISTA609756551(FLISTA609756551_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FLISTA1893461511)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FLISTA1893461511)frame->self,frame->i_I_u_I,(AM_BND1124877163)frame->arg1); frame->i_I_u_I++;
   ;
   frame->state = 1;
   return;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return;
}
