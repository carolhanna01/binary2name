#include "sather.h"

/* Layouts */

typedef struct dAM_struct {
 OB_HEADER header;
 } *dAM;

typedef struct dAM_EXPR_struct {
 OB_HEADER header;
 } *dAM_EXPR;

typedef struct dAM_STMT_struct {
 OB_HEADER header;
 } *dAM_STMT;

typedef struct dCONFIG_struct {
 OB_HEADER header;
 } *dCONFIG;

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

typedef struct AM_CALL_ARG_struct {/* layout for AM_CALL_ARG */
 OB_HEADER header;
 struct dAM_EXPR_struct *expr;
 struct dMODE_struct *mode1;
 } *AM_CALL_ARG;

typedef struct AM_FORMAL_ARG_struct {/* layout for AM_FORMAL_ARG */
 OB_HEADER header;
 struct dMODE_struct *mode1;
 struct AM_LOCAL_EXPR_struct *expr;
 } *AM_FORMAL_ARG;

typedef struct AM_OUT_struct {/* layout for AM_OUT */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *AM_OUT;

typedef struct AREFFL1698233817_struct {/* layout for AREF{FLIST{AM_LOCAL_EXPR}} */
 OB_HEADER header;
 INT asize;
 struct FLISTA725283029_struct *arr_part[1];
 } *AREFFL1698233817;

typedef struct ARRAYdAM_EXPR_struct {/* layout for ARRAY{$AM_EXPR} */
 OB_HEADER header;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *ARRAYdAM_EXPR;

typedef struct ARRAYd1413132360_frame_struct {
 INT state;
 ARRAYdAM_EXPR self;/* Formal argument: self */
 dAM_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAYd1413132360_frame;

typedef struct ELT_EQ981370187_struct {/* layout for ELT_EQ{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ981370187;

typedef struct ELT_EQ947319992_struct {/* layout for ELT_EQ{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ947319992;

typedef struct ELT_EQ1704040611_struct {/* layout for ELT_EQ{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQ1704040611;

typedef struct ELT_EQSTR_struct {/* layout for ELT_EQ{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_EQSTR;

typedef struct ELT_HASH_struct {/* layout for ELT_HASH */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_HASH;

typedef struct ELT_NI1559244239_struct {/* layout for ELT_NIL{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1559244239;

typedef struct ELT_NI1564018771_struct {/* layout for ELT_NIL{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI1564018771;

typedef struct ELT_NI19483997_struct {/* layout for ELT_NIL{AM_LOCAL_EXPR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NI19483997;

typedef struct ELT_NILSTR_struct {/* layout for ELT_NIL{STR} */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *ELT_NILSTR;

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

typedef struct SFILE_ID_struct {/* layout for SFILE_ID */
 INT loc;
 } SFILE_ID;
static SFILE_ID SFILE_ID_zero;

typedef struct SFILE_ID_boxed_struct {
 OB_HEADER header;
 SFILE_ID immutable_part;
 } *SFILE_ID_boxed;

typedef struct SYS_struct {/* layout for SYS */
 OB_HEADER header;
 CHAR field_so_not_empty;
 } *SYS;

typedef struct TUPSTRdCONFIG_struct {/* layout for TUP{STR,$CONFIG} */
 struct dCONFIG_struct *t2;
 STR t1;
 } TUPSTRdCONFIG;
static TUPSTRdCONFIG TUPSTRdCONFIG_zero;

typedef struct TUPSTRdCONFIG_boxed_struct {
 OB_HEADER header;
 TUPSTRdCONFIG immutable_part;
 } *TUPSTRdCONFIG_boxed;

typedef struct AM_ANY_EXPR_struct {/* layout for AM_ANY_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_ANY_EXPR;

typedef struct AM_ARRAY_EXPR_struct {/* layout for AM_ARRAY_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *AM_ARRAY_EXPR;

typedef struct AM_ARR_CONST_struct {/* layout for AM_ARR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct ARRAYdAM_CONST_struct *elts;
 struct SFILE_ID_struct source1;
 struct TP_CLASS_struct *elt_tp;
 } *AM_ARR_CONST;

typedef struct AM_ASSERT_STMT_struct {/* layout for AM_ASSERT_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_ASSERT_STMT;

typedef struct AM_ASSIGN_STMT_struct {/* layout for AM_ASSIGN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_ASSIGN_STMT;

typedef struct AM_ATTACH_STMT_struct {/* layout for AM_ATTACH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *gate;
 struct AM_LOCAL_EXPR_struct *helper;
 struct SFILE_ID_struct source1;
 struct SIG_struct *rout;
 } *AM_ATTACH_STMT;

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

typedef struct AM_BND260301329_struct {/* layout for AM_BND_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *br;
 struct SFILE_ID_struct source1;
 struct TP_ROUT_struct *br_tp;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_BND260301329;

typedef struct AM_BOOL_CONST_struct {/* layout for AM_BOOL_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 BOOL val1;
 } *AM_BOOL_CONST;

typedef struct AM_BREAK_STMT_struct {/* layout for AM_BREAK_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_BREAK_STMT;

typedef struct AM_CASE_STMT_struct {/* layout for AM_CASE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTF7594819_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 } *AM_CASE_STMT;

typedef struct AM_CHAR_CONST_struct {/* layout for AM_CHAR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 CHAR bval;
 } *AM_CHAR_CONST;

typedef struct AM_CLUSTER_EXPR_struct {/* layout for AM_CLUSTER_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLUSTER_EXPR;

typedef struct AM_CLU1286269335_struct {/* layout for AM_CLUSTER_SIZE_EXPR */
 OB_HEADER header;
 struct SFILE_ID_struct source1;
 } *AM_CLU1286269335;

typedef struct AM_COMMENT_STMT_struct {/* layout for AM_COMMENT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_COMMENT_STMT;

typedef struct AM_EXCEPT_EXPR_struct {/* layout for AM_EXCEPT_EXPR */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_EXCEPT_EXPR;

typedef struct AM_EXPR_STMT_struct {/* layout for AM_EXPR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_EXPR_struct *expr;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_EXPR_STMT;

typedef struct AM_EXT_CALL_EXPR_struct {/* layout for AM_EXT_CALL_EXPR */
 OB_HEADER header;
 struct IDENT_struct nm;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_EXT_CALL_EXPR;

typedef struct AM_FAR_EXPR_struct {/* layout for AM_FAR_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct SFILE_ID_struct source1;
 } *AM_FAR_EXPR;

typedef struct AM_FLTDX_CONST_struct {/* layout for AM_FLTDX_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTDX_CONST;

typedef struct AM_FLTD_CONST_struct {/* layout for AM_FLTD_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTD_CONST;

typedef struct AM_FLTI_CONST_struct {/* layout for AM_FLTI_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTI_CONST;

typedef struct AM_FLTX_CONST_struct {/* layout for AM_FLTX_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLTX_CONST;

typedef struct AM_FLT_CONST_struct {/* layout for AM_FLT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct RAT_struct val1;
 struct SFILE_ID_struct source1;
 } *AM_FLT_CONST;

typedef struct AM_FORK_STMT_struct {/* layout for AM_FORK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *at;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct AM_PAR_STMT_struct *par_stmt;
 struct SFILE_ID_struct source1;
 } *AM_FORK_STMT;

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

typedef struct AM_IF_STMT_struct {/* layout for AM_IF_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *if_false;
 struct dAM_STMT_struct *if_true;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_IF_STMT;

typedef struct AM_INITIAL_STMT_struct {/* layout for AM_INITIAL_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct dAM_STMT_struct *stmts;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_INITIAL_STMT;

typedef struct AM_INTI_CONST_struct {/* layout for AM_INTI_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INTI_CONST;

typedef struct AM_INT_CONST_struct {/* layout for AM_INT_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct INTI1_struct *val1;
 struct SFILE_ID_struct source1;
 } *AM_INT_CONST;

typedef struct AM_INV1167837230_struct {/* layout for AM_INVARIANT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 } *AM_INV1167837230;

typedef struct AM_IS_VOID_EXPR_struct {/* layout for AM_IS_VOID_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_IS_VOID_EXPR;

typedef struct AM_ITE1809135850_struct {/* layout for AM_ITER_CALL_EXPR */
 OB_HEADER header;
 struct dAM_STMT_struct *init;
 struct AM_LOOP_STMT_struct *lp;
 struct ARRAYSTR_struct *arg_list;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 STR uniq;
 BOOL init_before_loop;
 BOOL use_loop_index;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ITE1809135850;

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

typedef struct AM_LOCK_STMT_struct {/* layout for AM_LOCK_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_EXPR_struct *guards;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTA1327600838_struct *locks1;
 struct SFILE_ID_struct source1;
 BOOL manual_unlock;
 } *AM_LOCK_STMT;

typedef struct AM_LOOP_STMT_struct {/* layout for AM_LOOP_STMT */
 OB_HEADER header;
 struct dAM_struct *first_while_move;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *init;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *loop_index_var;
 struct FLISTA1409846210_struct *bits1;
 struct FLISTA1062334999_struct *its1;
 struct FLISTA725283029_struct *firsts;
 struct FLISTE1761212196_struct *hoisted;
 struct FLISTSTR_struct *ar_max_expr;
 INT breaks;
 struct SFILE_ID_struct source1;
 STR loop_index;
 BOOL has_init_stmt;
 BOOL has_yield;
 BOOL no_begin_loop;
 } *AM_LOOP_STMT;

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

typedef struct AM_OB_DEF_struct {/* layout for AM_OB_DEF */
 OB_HEADER header;
 struct dTP_struct *arr;
 struct dTP_struct *tp;
 struct ARRAYIDENT_struct *sorted_at;
 struct FMAPIDENTdTP_struct *at;
 INT asize;
 struct SFILE_ID_struct source1;
 } *AM_OB_DEF;

typedef struct AM_PAR_STMT_struct {/* layout for AM_PAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_PAR_STMT;

typedef struct AM_POST_STMT_struct {/* layout for AM_POST_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_POST_STMT;

typedef struct AM_PREFETCH_STMT_struct {/* layout for AM_PREFETCH_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 STR comment1;
 } *AM_PREFETCH_STMT;

typedef struct AM_PRE_STMT_struct {/* layout for AM_PRE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *test1;
 struct dAM_STMT_struct *next;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_PRE_STMT;

typedef struct AM_PROTECT_STMT_struct {/* layout for AM_PROTECT_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *body;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTdTP_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL no_else;
 } *AM_PROTECT_STMT;

typedef struct AM_RAISE_STMT_struct {/* layout for AM_RAISE_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RAISE_STMT;

typedef struct AM_RETURN_STMT_struct {/* layout for AM_RETURN_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_RETURN_STMT;

typedef struct AM_ROU1916046290_struct {/* layout for AM_ROUT_CALL_EXPR */
 OB_HEADER header;
 struct AS_TYPE_SPEC_struct *as_type;
 struct SFILE_ID_struct source1;
 struct SIG_struct *fun;
 INT asize;
 struct AM_CALL_ARG_struct *arr_part[1];
 } *AM_ROU1916046290;

typedef struct AM_ROUT_DEF_struct {/* layout for AM_ROUT_DEF */
 OB_HEADER header;
 struct dAM_STMT_struct *code1;
 struct AM_LOCAL_EXPR_struct *rres;
 struct AM_LOCAL_EXPR_struct *specul_prefetch;
 struct FLISTdAM_EXPR_struct *calls1;
 struct FLISTA725283029_struct *locals1;
 INT num_yields;
 struct SFILE_ID_struct source1;
 struct SIG_struct *sig1;
 struct SIG_struct *srcsig;
 BOOL is_abstract;
 BOOL is_attach_routin;
 BOOL is_clean;
 BOOL is_external;
 BOOL is_fork_routine;
 BOOL is_par_routine;
 BOOL n_export_call;
 BOOL n_export_locals;
 BOOL n_import_call;
 BOOL n_import_locals;
 INT asize;
 struct AM_FORMAL_ARG_struct *arr_part[1];
 } *AM_ROUT_DEF;

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

typedef struct AM_STR_CONST_struct {/* layout for AM_STR_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 STR bval;
 } *AM_STR_CONST;

typedef struct AM_SYNC_STMT_struct {/* layout for AM_SYNC_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_SYNC_STMT;

typedef struct AM_TYPECASE_STMT_struct {/* layout for AM_TYPECASE_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_stmts;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *test1;
 struct FLISTdAM_STMT_struct *stmts;
 struct FLISTdTP_struct *tgts;
 struct SFILE_ID_struct source1;
 BOOL has_void_stmts;
 BOOL no_else;
 } *AM_TYPECASE_STMT;

typedef struct AM_UNLOCK_STMT_struct {/* layout for AM_UNLOCK_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *lock_ob;
 struct dAM_STMT_struct *next;
 struct SFILE_ID_struct source1;
 } *AM_UNLOCK_STMT;

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

typedef struct AM_VOID_CONST_struct {/* layout for AM_VOID_CONST */
 OB_HEADER header;
 struct dTP_struct *tp_at;
 struct SFILE_ID_struct source1;
 } *AM_VOID_CONST;

typedef struct AM_WAITFOR_STMT_struct {/* layout for AM_WAITFOR_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *dest;
 struct dAM_EXPR_struct *src1;
 struct dAM_STMT_struct *next;
 struct AM_LOCAL_EXPR_struct *prefetch;
 struct SFILE_ID_struct source1;
 } *AM_WAITFOR_STMT;

typedef struct AM_WHERE_EXPR_struct {/* layout for AM_WHERE_EXPR */
 OB_HEADER header;
 struct dAM_EXPR_struct *arg;
 struct dTP_struct *tp;
 struct SFILE_ID_struct source1;
 } *AM_WHERE_EXPR;

typedef struct AM_WIT1996971603_struct {/* layout for AM_WITH_NEAR_STMT */
 OB_HEADER header;
 struct dAM_STMT_struct *else_part;
 struct dAM_STMT_struct *near_part;
 struct dAM_STMT_struct *next;
 struct ARRAYdAM_EXPR_struct *objects1;
 struct SFILE_ID_struct source1;
 } *AM_WIT1996971603;

typedef struct AM_YIELD_STMT_struct {/* layout for AM_YIELD_STMT */
 OB_HEADER header;
 struct dAM_EXPR_struct *val1;
 struct dAM_STMT_struct *next;
 INT ret;
 struct SFILE_ID_struct source1;
 } *AM_YIELD_STMT;

typedef struct AREFFL1603407436_frame_struct {
 INT state;
 AREFFL1698233817 self;/* Formal argument: self */
 INT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *AREFFL1603407436_frame;

typedef struct ARRAY2BOOL_struct {/* layout for ARRAY2{BOOL} */
 OB_HEADER header;
 INT size1;
 INT size2;
 INT asize;
 BOOL arr_part[1];
 } *ARRAY2BOOL;

typedef struct ARRAY21774950588_frame_struct {
 INT state;
 ARRAY2BOOL self;/* Formal argument: self */
 BOOL ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *ARRAY21774950588_frame;

typedef struct ARRAY21272457581_frame_struct {
 INT state;
 ARRAY2BOOL self;/* Formal argument: self */
 BOOL ret_val;
 ARRAY2BOOL L41;
 INT L11;
 BOOL f_L21_; /* used by builtin iter */
 INT L21_br; /* used by builtin iter */
 INT i_L21_; /* used by builtin iter */
 } *ARRAY21272457581_frame;

typedef struct FLISTdAM_EXPR_struct {/* layout for FLIST{$AM_EXPR} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_EXPR_struct *arr_part[1];
 } *FLISTdAM_EXPR;

typedef struct FLISTdAM_STMT_struct {/* layout for FLIST{$AM_STMT} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct dAM_STMT_struct *arr_part[1];
 } *FLISTdAM_STMT;

typedef struct FLISTA1327600838_struct {/* layout for FLIST{ARRAY{$AM_EXPR}} */
 OB_HEADER header;
 INT loc;
 INT asize;
 struct ARRAYdAM_EXPR_struct *arr_part[1];
 } *FLISTA1327600838;

typedef struct FMAPSTRdCONFIG_struct {/* layout for FMAP{STR,$CONFIG} */
 OB_HEADER header;
 INT hsize;
 INT asize;
 struct TUPSTRdCONFIG_struct arr_part[1];
 } *FMAPSTRdCONFIG;

typedef struct FMAPST204994337_frame_struct {
 INT state;
 FMAPSTRdCONFIG self;/* Formal argument: self */
 TUPSTRdCONFIG ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST204994337_frame;

typedef struct FMAPST1202054525_frame_struct {
 INT state;
 FMAPSTRdCONFIG self;/* Formal argument: self */
 TUPSTRdCONFIG arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FMAPST1202054525_frame;

typedef struct FMAPST1565675296_frame_struct {
 INT state;
 FMAPSTRdCONFIG self;/* Formal argument: self */
 TUPSTRdCONFIG ret_val2;
 FMAPSTRdCONFIG L61;
 TUPSTRdCONFIG r;
 INT L31;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1565675296_frame;

typedef struct FMAPST1785271125_frame_struct {
 INT state;
 FMAPSTRdCONFIG self;/* Formal argument: self */
 dCONFIG ret_val2;
 FMAPSTRdCONFIG L61;
 TUPSTRdCONFIG e;
 INT L31;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FMAPST1785271125_frame;

typedef struct FSETAB1569698808_struct {/* layout for FSET{ABSTRACT_LAYOUT} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct ABSTRACT_LAYOUT_struct *arr_part[1];
 } *FSETAB1569698808;

typedef struct FSETAB1139214763_frame_struct {
 INT state;
 FSETAB1569698808 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAB1139214763_frame;

typedef struct FSETAB716509_frame_struct {
 INT state;
 FSETAB1569698808 self;/* Formal argument: self */
 ABSTRACT_LAYOUT arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAB716509_frame;

typedef struct FSETAB1942662686_frame_struct {
 INT state;
 FSETAB1569698808 self;/* Formal argument: self */
 ABSTRACT_LAYOUT ret_val2;
 FSETAB1569698808 L61;
 ABSTRACT_LAYOUT r;
 INT i;
 INT sz;
 INT L31;
 FSETAB1569698808 is_elt_nil_self;
 ABSTRACT_LAYOUT is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1559244239 is_elt_nil_self1;
 ABSTRACT_LAYOUT is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAB1942662686_frame;

typedef struct FSETAM1032778315_struct {/* layout for FSET{AM_BND_CREATE_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_BND1124877163_struct *arr_part[1];
 } *FSETAM1032778315;

typedef struct FSETAM1934223114_frame_struct {
 INT state;
 FSETAM1032778315 self;/* Formal argument: self */
 AM_BND1124877163 ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM1934223114_frame;

typedef struct FSETAM1225243056_frame_struct {
 INT state;
 FSETAM1032778315 self;/* Formal argument: self */
 AM_BND1124877163 arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM1225243056_frame;

typedef struct FSETAM595751729_frame_struct {
 INT state;
 FSETAM1032778315 self;/* Formal argument: self */
 AM_BND1124877163 ret_val2;
 FSETAM1032778315 L61;
 AM_BND1124877163 r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1032778315 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM595751729_frame;

typedef struct FSETAM1158405984_struct {/* layout for FSET{AM_LOCAL_EXPR} */
 OB_HEADER header;
 INT hsize;
 BOOL use_map;
 INT asize;
 struct AM_LOCAL_EXPR_struct *arr_part[1];
 } *FSETAM1158405984;

typedef struct FSETAM1413017669_frame_struct {
 INT state;
 FSETAM1158405984 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val;
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM1413017669_frame;

typedef struct FSETAM1474900765_frame_struct {
 INT state;
 FSETAM1158405984 self;/* Formal argument: self */
 AM_LOCAL_EXPR arg1;/* Formal argument: val */
 BOOL f_I_u_I; /* used by builtin iter */
 INT I_u_Ibr; /* used by builtin iter */
 INT i_I_u_I; /* used by builtin iter */
 } *FSETAM1474900765_frame;

typedef struct FSETAM441739032_frame_struct {
 INT state;
 FSETAM1158405984 self;/* Formal argument: self */
 AM_LOCAL_EXPR ret_val2;
 FSETAM1158405984 L61;
 AM_LOCAL_EXPR r;
 INT i;
 INT sz;
 INT L31;
 FSETAM1158405984 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val1;
 BOOL f_L41_; /* used by builtin iter */
 INT L41_br; /* used by builtin iter */
 INT i_L41_; /* used by builtin iter */
 } *FSETAM441739032_frame;

typedef struct FSTR_struct {/* layout for FSTR */
 OB_HEADER header;
 INT loc;
 INT asize;
 CHAR arr_part[1];
 } *FSTR;

#include "tags.h"

/* Globals */

extern BOOL FSETAB1854241901;
extern BOOL FSETAB582100884;
extern BOOL FSETAM1441852029;
extern BOOL FSETAM1817646066;
extern BOOL FSETAM40978445;
extern BOOL FSETAM994490756;
extern FSTR INTI_buf;
extern INT FMAPST641873122;
extern INT FSETAB246578603;
extern INT FSETAB478854058;
extern INT FSETAM2014782744;
extern INT FSETAM34126943;
extern INT FSETAM584174453;
extern INT FSETAM687490432;

/* Function declarations */


extern RETURNED_CONST STR (**dCONFIG_strrSTR)(dCONFIG);

extern RETURNED_CONST STR (**dTP_strrSTR)(dTP);

extern RETURNED_CONST dAM_STMT (**dAM_ST775224435)(dAM_STMT);
ABSTRACT_LAYOUT FSETAB1139214763(FSETAB1139214763_frame);
ABSTRACT_LAYOUT FSETAB1942662686(FSETAB1942662686_frame);
AM_BND1124877163 FSETAM1934223114(FSETAM1934223114_frame);
AM_BND1124877163 FSETAM595751729(FSETAM595751729_frame);
AM_LOCAL_EXPR FSETAM1413017669(FSETAM1413017669_frame);
AM_LOCAL_EXPR FSETAM441739032(FSETAM441739032_frame);
BOOL ARRAY21272457581(ARRAY21272457581_frame);
BOOL ARRAY21774950588(ARRAY21774950588_frame);
BOOL FSETAM1417167536(FSETAM1158405984);
BOOL FSETAM1609599145(FSETAM1158405984, AM_LOCAL_EXPR);
BOOL FSETAM2142988046(FSETAM1158405984, AM_LOCAL_EXPR);
BOOL FSETAM215631726(FSETAM1158405984, AM_LOCAL_EXPR);
BOOL STR_is111530248(STR, STR);
FMAPSTRdCONFIG FMAPST1013830433(FMAPSTRdCONFIG, INT);
FMAPSTRdCONFIG FMAPST1027320702(FMAPSTRdCONFIG);
FMAPSTRdCONFIG FMAPST2004940399(FMAPSTRdCONFIG, STR, dCONFIG);
FSETAB1569698808 FSETAB1294173876(FSETAB1569698808, FSETAB1569698808, ABSTRACT_LAYOUT);
FSETAB1569698808 FSETAB1823289229(FSETAB1569698808, ABSTRACT_LAYOUT);
FSETAB1569698808 FSETAB1862720980(FSETAB1569698808, INT);
FSETAB1569698808 FSETAB2001021748(FSETAB1569698808, FSETAB1569698808, ABSTRACT_LAYOUT);
FSETAB1569698808 FSETAB285451608(FSETAB1569698808);
FSETAB1569698808 FSETAB478050731(FSETAB1569698808);
FSETAM1032778315 FSETAM1135369366(FSETAM1032778315, FSETAM1032778315, AM_BND1124877163);
FSETAM1032778315 FSETAM1587889927(FSETAM1032778315, INT);
FSETAM1032778315 FSETAM1990484536(FSETAM1032778315, AM_BND1124877163);
FSETAM1032778315 FSETAM246540798(FSETAM1032778315);
FSETAM1032778315 FSETAM381650379(FSETAM1032778315, FSETAM1032778315);
FSETAM1032778315 FSETAM736229142(FSETAM1032778315, FSETAM1032778315, AM_BND1124877163);
FSETAM1032778315 FSETAM74171957(FSETAM1032778315);
FSETAM1032778315 FSETAM990065885(FSETAM1032778315);
FSETAM1158405984 FSETAM1195182488(FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM2136437804(FSETAM1158405984, FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM306848596(FSETAM1158405984, FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM430316904(FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM460903522(FSETAM1158405984);
FSETAM1158405984 FSETAM483153547(FSETAM1158405984, AM_LOCAL_EXPR);
FSETAM1158405984 FSETAM556323666(FSETAM1158405984, INT);
FSETAM1158405984 FSETAM7866998(FSETAM1158405984);
FSETAM1158405984 FSETAM792944763(FSETAM1158405984);
FSETAM1158405984 FSETAM83361925(FSETAM1158405984, AM_LOCAL_EXPR);
FSTR FSTR_c1307336863(FSTR, STR);
FSTR FSTR_p1752847026(FSTR, STR);
FSTR INTI_s96356965(INTI1, FSTR);
INT AREFFL1603407436(AREFFL1603407436_frame);
INT ELT_HA1612720024(ELT_HASH, OB);
INT FLISTd1406591968(FLISTdAM_EXPR);
INT FSETAM1190305782(FSETAM1032778315);
STR FMAPST512400025(FMAPSTRdCONFIG);
STR RAT_strrSTR(RAT);
STR STR_fr1097270334(STR, FSTR);
TUPSTRdCONFIG FMAPST1565675296(FMAPST1565675296_frame);
TUPSTRdCONFIG FMAPST204994337(FMAPST204994337_frame);
dAM_EXPR ARRAYd1413132360(ARRAYd1413132360_frame);
dCONFIG FMAPST1785271125(FMAPST1785271125_frame);
dCONFIG FMAPST558704553(FMAPSTRdCONFIG, STR);
void AM_OUT101602186(AM_OUT, AM_LOOP_STMT, INT, INT);
void AM_OUT1191840136(AM_OUT, AM_ASSERT_STMT, INT, INT);
void AM_OUT1219015152(AM_OUT, AM_UNLOCK_STMT, INT, INT);
void AM_OUT1250601520(AM_OUT, AM_IF_STMT, INT, INT);
void AM_OUT1286972646(AM_OUT, AM_HERE_EXPR, INT, INT);
void AM_OUT1341727046(AM_OUT, AM_POST_STMT, INT, INT);
void AM_OUT1365214533(AM_OUT, AM_FORMAL_ARG, INT, INT);
void AM_OUT1399375684(AM_OUT, AM_ASSIGN_STMT, INT, INT);
void AM_OUT1439666796(AM_OUT, dAM);
void AM_OUT1468881370(AM_OUT, AM_STMT_EXPR, INT, INT);
void AM_OUT1496755235(AM_OUT, AM_YIELD_STMT, INT, INT);
void AM_OUT1521024147(AM_OUT, AM_PAR_STMT, INT, INT);
void AM_OUT1533629362(AM_OUT, AM_FORK_STMT, INT, INT);
void AM_OUT1555944202(AM_OUT, AM_CASE_STMT, INT, INT);
void AM_OUT1614972293(AM_OUT, AM_BND260301329, INT, INT);
void AM_OUT1623210178(AM_OUT, AM_TYPECASE_STMT, INT, INT);
void AM_OUT1677535099(AM_OUT, AM_LOCAL_EXPR, INT, INT);
void AM_OUT1813430155(AM_OUT, AM_BND367211769, INT, INT);
void AM_OUT1830495166(AM_OUT, AM_PREFETCH_STMT, INT, INT);
void AM_OUT1924306007(AM_OUT, AM_ARRAY_EXPR, INT, INT);
void AM_OUT1997333258(AM_OUT, AM_ATTR_EXPR, INT, INT);
void AM_OUT2087454067(AM_OUT, INT);
void AM_OUT244025178(AM_OUT, AM_VAT319982528, INT, INT);
void AM_OUT275623249(AM_OUT, AM_ANY_EXPR, INT, INT);
void AM_OUT347621831(AM_OUT, AM_VAR744470097, INT, INT);
void AM_OUT359547257(AM_OUT, AM_PRE_STMT, INT, INT);
void AM_OUT376898766(AM_OUT, AM_EXT_CALL_EXPR, INT, INT);
void AM_OUT391785759(AM_OUT, AM_OB_DEF, INT, INT);
void AM_OUT41050653(AM_OUT, AM_ROU1916046290, INT, INT);
void AM_OUT435235740(AM_OUT, AM_ATTACH_STMT, INT, INT);
void AM_OUT438015553(AM_OUT, AM_WIT1996971603, INT, INT);
void AM_OUT523288876(AM_OUT, AM_BND1124877163, INT, INT);
void AM_OUT600226455(AM_OUT, AM_CALL_ARG, INT, INT);
void AM_OUT628733655(AM_OUT, AM_CLUSTER_EXPR, INT, INT);
void AM_OUT671570827(AM_OUT, AM_PROTECT_STMT, INT, INT);
void AM_OUT726258064(AM_OUT, AM_SHARED_EXPR, INT, INT);
void AM_OUT732629843(AM_OUT, AM_ITE1809135850, INT, INT);
void AM_OUT786574336(AM_OUT, AM_RETURN_STMT, INT, INT);
void AM_OUT796864830(AM_OUT, AM_ARR_CONST, INT, INT);
void AM_OUT797583920(AM_OUT, AM_EXCEPT_EXPR, INT, INT);
void AM_OUT860401336(AM_OUT, dAM, INT, INT);
void AM_OUT930898334(AM_OUT, AM_EXPR_STMT, INT, INT);
void AM_OUT957177270(AM_OUT, AM_LOCK_STMT, INT, INT);
void AM_OUT959054340(AM_OUT, AM_IF_EXPR, INT, INT);
void AM_OUT991874317(AM_OUT, AM_ROUT_DEF, INT, INT);
void FILE_plus_STR(FILE1, STR);
void FILE_plus_dSTR(FILE1, dSTR);
void FMAPST1202054525(FMAPST1202054525_frame);
void FSETAB716509(FSETAB716509_frame);
void FSETAM1225243056(FSETAM1225243056_frame);
void FSETAM1474900765(FSETAM1474900765_frame);

/* Bound rout stubs decls */


/* Bound iter stubs decls */


/* Unbox decls */


/* External calls */

#include <stdio.h>

/* Code */




#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM1417167536(FSETAM1158405984 self) {
 BOOL ret_val = BOOL_zero;
 BOOL L1;
 INT L21_;
 INT L3;
 BOOL L41_;
 BOOL L5;
 BOOL L61_;
 INT L7;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 INT L12;
 INT L131_;
 INT L14;
 INT L151_;
 INT L16;
 BOOL L171_;
 L21_=ASIZE((FSETAM1158405984)self); 
 L3 = L21_;
 L41_=(L3)<(33); 
 L5 = L41_;
 L61_=!(L5); 
 if (L61_) {
  L7 = ATTR(self,hsize);
  L81_=ASIZE((FSETAM1158405984)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,1); 
  L11 = L101_;
  L12 = FSETAM584174453;
  L131_=INTTIMES(L12,2); 
  L14 = L131_;
  L151_=INTDIV(L11,L14); 
  L16 = L151_;
  L171_=(L7)<(L16); 
  L1 = L171_;
 } else {
  L1 = FALSE;
 }
 ret_val = L1;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM1609599145(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 BOOL ret_val = BOOL_zero;
 INT h = INT_zero;
 AM_LOCAL_EXPR te;
 AM_LOCAL_EXPR te1;
 FSETAM1158405984 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM1158405984 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FSETAM1158405984 is_elt_nil_self2;
 AM_LOCAL_EXPR is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self3;
 AM_LOCAL_EXPR is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETAM1158405984 elt_eq_self2;
 AM_LOCAL_EXPR elt_eq_e12;
 AM_LOCAL_EXPR elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self3;
 AM_LOCAL_EXPR elt_eq_e13;
 AM_LOCAL_EXPR elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 AM_LOCAL_EXPR L71_;
 INT L81_;
 INT L91_;
 INT L10;
 INT L111_;
 INT L12;
 BOOL L131_;
 AM_LOCAL_EXPR L141_;
 INT L151_;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETAM1158405984)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI19483997) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1704040611) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_a = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    ret_val = TRUE;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=ASIZE((FSETAM1158405984)self); 
 L10 = L91_;
 L111_=INTMINUS(L10,1); 
 L12 = L111_;
 L131_=(h)==(L12); 
 if (L131_) {
  h = 0;
  while (1) {
   L141_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,h); 
   te1 = L141_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI19483997) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((AM_LOCAL_EXPR) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1704040611) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_a1 = ((OB) elt_eq_e23);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     ret_val = TRUE;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM2142988046(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 BOOL ret_val = BOOL_zero;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 BOOL L11_;
 AM_LOCAL_EXPR L21_;
 INT L31_;
 i = 0;
 sz = ATTR(self,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1704040611) NULL);
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
  L31_=INTPLUS(i,1); 
  i = L31_;
 }
 after_loop: ;
 ret_val = FALSE;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

BOOL FSETAM215631726(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 BOOL ret_val = BOOL_zero;
 if ((self==((FSETAM1158405984) NULL))) {
  ret_val = FALSE;
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETAM1609599145(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM2142988046(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRdCONFIG FMAPST1013830433(FMAPSTRdCONFIG self, INT n) {
 FMAPSTRdCONFIG ret_val;
 FMAPSTRdCONFIG r;
 FMAPSTRdCONFIG L11;
 FMAPSTRdCONFIG key_nil_self;
 STR ret_val1;
 ELT_NILSTR elt_nil_self;
 STR ret_val2;
 STR e = ((STR) NULL);
 FMAPSTRdCONFIG key_nil_self1;
 STR ret_val3;
 ELT_NILSTR elt_nil_self1;
 STR ret_val4;
 STR e1 = ((STR) NULL);
 TUPSTRdCONFIG create_self = TUPSTRdCONFIG_zero;
 STR create_at1;
 dCONFIG create_at2;
 TUPSTRdCONFIG ret_val5 = TUPSTRdCONFIG_zero;
 INT L21 = INT_zero;
 FMAPSTRdCONFIG L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 TUPSTRdCONFIG L10;
 TUPSTRdCONFIG L12;
 TUPSTRdCONFIG L13;
 INT L141_;
 L4 = n;
 L6=(sizeof(struct FMAPSTRdCONFIG_struct)-sizeof(TUPSTRdCONFIG))+(L4)*sizeof(TUPSTRdCONFIG);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FMAPSTRdCONFIG_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FMAPSTRdCONFIG) L5);
 L3->asize = L4;
 r = L3;
 key_nil_self = self;
 elt_nil_self = ((ELT_NILSTR) NULL);
 ret_val2 = ((STR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((STR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FMAPSTRdCONFIG)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    create_self = TUPSTRdCONFIG_zero;
    key_nil_self1 = self;
    elt_nil_self1 = ((ELT_NILSTR) NULL);
    ret_val4 = ((STR) NULL);
    ret_val3 = ret_val4;
    create_at1 = ret_val3;
    create_at2 = ((dCONFIG) NULL);
    L10 = create_self;
    L10.t1 = create_at1;
    L12 = L10;
    L12.t2 = create_at2;
    ret_val5 = L12;
    L13 = ret_val5;
    SARR((FMAPSTRdCONFIG)L11,L21,L13); 
    ;
    L141_=INTPLUS(L21,1); 
    L21 = L141_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRdCONFIG FMAPST1027320702(FMAPSTRdCONFIG self) {
 FMAPSTRdCONFIG ret_val;
 INT ns = INT_zero;
 FMAPSTRdCONFIG r;
 FMAPSTRdCONFIG L11;
 FMAPSTRdCONFIG insert_pair_self;
 TUPSTRdCONFIG insert_pair_p = TUPSTRdCONFIG_zero;
 FMAPSTRdCONFIG ret_val1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 TUPSTRdCONFIG L9;
 L21_=ASIZE((FMAPSTRdCONFIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 ns = L81_;
 r = FMAPST1013830433(self, ns);
 {
  struct FMAPST1565675296_frame_struct other1_0;
FMAPST1565675296_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   insert_pair_self = r;
   L9 = FMAPST1565675296(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   insert_pair_p = L9;
   ret_val1 = FMAPST2004940399(insert_pair_self, insert_pair_p.t1, insert_pair_p.t2);
   r = ret_val1;
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FMAPSTRdCONFIG FMAPST2004940399(FMAPSTRdCONFIG self, STR k, dCONFIG t) {
 FMAPSTRdCONFIG ret_val;
 FMAPSTRdCONFIG r;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 INT asm1 = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRdCONFIG should_grow_self;
 BOOL ret_val1 = BOOL_zero;
 FMAPSTRdCONFIG key_hash_self;
 OB key_hash_e;
 INT ret_val2 = INT_zero;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val3 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val4 = BOOL_zero;
 FMAPSTRdCONFIG key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val6 = BOOL_zero;
 TUPSTRdCONFIG create_self = TUPSTRdCONFIG_zero;
 STR create_at1;
 dCONFIG create_at2;
 TUPSTRdCONFIG ret_val7 = TUPSTRdCONFIG_zero;
 FMAPSTRdCONFIG is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val8 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val9 = BOOL_zero;
 FMAPSTRdCONFIG key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val10 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val11 = BOOL_zero;
 TUPSTRdCONFIG create_self1 = TUPSTRdCONFIG_zero;
 STR create_at11;
 dCONFIG create_at21;
 TUPSTRdCONFIG ret_val12 = TUPSTRdCONFIG_zero;
 TUPSTRdCONFIG create_self2 = TUPSTRdCONFIG_zero;
 STR create_at12;
 dCONFIG create_at22;
 TUPSTRdCONFIG ret_val13 = TUPSTRdCONFIG_zero;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 INT L161_;
 INT L171_;
 INT L18;
 INT L191_;
 TUPSTRdCONFIG L201_;
 TUPSTRdCONFIG L21;
 TUPSTRdCONFIG L22;
 TUPSTRdCONFIG L23;
 TUPSTRdCONFIG L24;
 INT L261_;
 BOOL L271_;
 TUPSTRdCONFIG L281_;
 TUPSTRdCONFIG L29;
 TUPSTRdCONFIG L30;
 TUPSTRdCONFIG L31;
 TUPSTRdCONFIG L32;
 INT L341_;
 TUPSTRdCONFIG L35;
 TUPSTRdCONFIG L36;
 TUPSTRdCONFIG L37;
 INT L39;
 INT L401_;
 r = self;
 if ((r==((FMAPSTRdCONFIG) NULL))) {
  r = FMAPST1013830433(self, 5);
 }
 else {
  should_grow_self = self;
  L11_=ASIZE((FMAPSTRdCONFIG)should_grow_self); 
  L2 = L11_;
  L3 = ATTR(should_grow_self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FMAPST641873122;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  ret_val1 = L91_;
  if (ret_val1) {
   r = FMAPST1027320702(self);
  }
 }
 key_hash_self = r;
 key_hash_e = ((OB) k);
 ret_val2 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L10 = ret_val2;
 L121_=ASIZE((FMAPSTRdCONFIG)r); 
 L13 = L121_;
 L141_=INTMINUS(L13,2); 
 L15 = L141_;
 L161_=L10&L15; 
 orig_h = L161_;
 h = orig_h;
 L171_=ASIZE((FMAPSTRdCONFIG)r); 
 L18 = L171_;
 L191_=INTMINUS(L18,1); 
 asm1 = L191_;
 while (1) {
  L201_=ARR((FMAPSTRdCONFIG)r,h); 
  L21=L201_;
  tk = L21.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val4 = (is_elt_nil_e==((STR) NULL));
  ret_val3 = ret_val4;
  if (ret_val3) {
   goto after_loop;
  }
  key_eq_self = self;
  key_eq_e1 = tk;
  key_eq_e2 = k;
  elt_eq_self = ((ELT_EQSTR) NULL);
  elt_eq_e1 = key_eq_e1;
  elt_eq_e2 = key_eq_e2;
  ret_val6 = STR_is111530248(elt_eq_e1, elt_eq_e2);
  ret_val5 = ret_val6;
  if (ret_val5) {
   create_self = TUPSTRdCONFIG_zero;
   create_at1 = k;
   create_at2 = t;
   L22 = create_self;
   L22.t1 = create_at1;
   L23 = L22;
   L23.t2 = create_at2;
   ret_val7 = L23;
   L24 = ret_val7;
   SARR((FMAPSTRdCONFIG)r,h,L24); 
   ;
   ret_val = r;
   return ret_val;
  }
  L261_=INTPLUS(h,1); 
  h = L261_;
 }
 after_loop: ;
 L271_=(h)==(asm1); 
 if (L271_) {
  h = 0;
  while (1) {
   L281_=ARR((FMAPSTRdCONFIG)r,h); 
   L29=L281_;
   tk1 = L29.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val9 = (is_elt_nil_e1==((STR) NULL));
   ret_val8 = ret_val9;
   if (ret_val8) {
    goto after_loop1;
   }
   key_eq_self1 = self;
   key_eq_e11 = tk1;
   key_eq_e21 = k;
   elt_eq_self1 = ((ELT_EQSTR) NULL);
   elt_eq_e11 = key_eq_e11;
   elt_eq_e21 = key_eq_e21;
   ret_val11 = STR_is111530248(elt_eq_e11, elt_eq_e21);
   ret_val10 = ret_val11;
   if (ret_val10) {
    create_self1 = TUPSTRdCONFIG_zero;
    create_at11 = k;
    create_at21 = t;
    L30 = create_self1;
    L30.t1 = create_at11;
    L31 = L30;
    L31.t2 = create_at21;
    ret_val12 = L31;
    L32 = ret_val12;
    SARR((FMAPSTRdCONFIG)r,h,L32); 
    ;
    ret_val = r;
    return ret_val;
   }
   L341_=INTPLUS(h,1); 
   h = L341_;
  }
  after_loop1: ;
 }
 create_self2 = TUPSTRdCONFIG_zero;
 create_at12 = k;
 create_at22 = t;
 L35 = create_self2;
 L35.t1 = create_at12;
 L36 = L35;
 L36.t2 = create_at22;
 ret_val13 = L36;
 L37 = ret_val13;
 SARR((FMAPSTRdCONFIG)r,h,L37); 
 ;
 L39 = ATTR(r,hsize);
 L401_=INTPLUS(L39,1); 
 SATTR(r,hsize,L401_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB1294173876(FSETAB1569698808 self, FSETAB1569698808 r, ABSTRACT_LAYOUT e) {
 FSETAB1569698808 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAB1569698808 elt_eq_self;
 ABSTRACT_LAYOUT elt_eq_e1;
 ABSTRACT_LAYOUT elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ981370187 elt_eq_self1;
 ABSTRACT_LAYOUT elt_eq_e11;
 ABSTRACT_LAYOUT elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 BOOL L11_;
 ABSTRACT_LAYOUT L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(ABSTRACT_LAYOUT)ARR((FSETAB1569698808)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ981370187) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  ret_val2 = (elt_eq_e11==elt_eq_e21);
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAB1569698808)r,i,(ABSTRACT_LAYOUT)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAB1569698808)r,L5,(ABSTRACT_LAYOUT)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB1823289229(FSETAB1569698808 self, ABSTRACT_LAYOUT e) {
 FSETAB1569698808 ret_val;
 FSETAB1569698808 r;
 FSETAB1569698808 set_initial_stru;
 r = self;
 if ((r==((FSETAB1569698808) NULL))) {
  r = FSETAB1862720980(self, FSETAB478854058);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAB582100884);
 }
 else {
  r = FSETAB285451608(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAB2001021748(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAB1294173876(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB1862720980(FSETAB1569698808 self, INT n) {
 FSETAB1569698808 ret_val;
 FSETAB1569698808 r;
 FSETAB1569698808 L11;
 FSETAB1569698808 elt_nil_self;
 ABSTRACT_LAYOUT ret_val1;
 ELT_NI1559244239 elt_nil_self1;
 ABSTRACT_LAYOUT ret_val2;
 ABSTRACT_LAYOUT e = ((ABSTRACT_LAYOUT) NULL);
 FSETAB1569698808 elt_nil_self2;
 ABSTRACT_LAYOUT ret_val3;
 ELT_NI1559244239 elt_nil_self3;
 ABSTRACT_LAYOUT ret_val4;
 ABSTRACT_LAYOUT e1 = ((ABSTRACT_LAYOUT) NULL);
 INT L21 = INT_zero;
 FSETAB1569698808 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 ABSTRACT_LAYOUT L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAB1569698808_struct)-sizeof(ABSTRACT_LAYOUT))+(L4)*sizeof(ABSTRACT_LAYOUT);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAB1569698808_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAB1569698808) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1559244239) NULL);
 ret_val2 = ((ABSTRACT_LAYOUT) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((ABSTRACT_LAYOUT) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAB1569698808)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1559244239) NULL);
    ret_val4 = ((ABSTRACT_LAYOUT) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAB1569698808)L11,L21,(ABSTRACT_LAYOUT)L10); 
    ;
    L121_=INTPLUS(L21,1); 
    L21 = L121_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB2001021748(FSETAB1569698808 self, FSETAB1569698808 r, ABSTRACT_LAYOUT e) {
 FSETAB1569698808 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 ABSTRACT_LAYOUT te;
 ABSTRACT_LAYOUT te1;
 FSETAB1569698808 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAB1569698808 is_elt_nil_self;
 ABSTRACT_LAYOUT is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1559244239 is_elt_nil_self1;
 ABSTRACT_LAYOUT is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAB1569698808 elt_eq_self;
 ABSTRACT_LAYOUT elt_eq_e1;
 ABSTRACT_LAYOUT elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ981370187 elt_eq_self1;
 ABSTRACT_LAYOUT elt_eq_e11;
 ABSTRACT_LAYOUT elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 FSETAB1569698808 is_elt_nil_self2;
 ABSTRACT_LAYOUT is_elt_nil_e2;
 BOOL ret_val6 = BOOL_zero;
 ELT_NI1559244239 is_elt_nil_self3;
 ABSTRACT_LAYOUT is_elt_nil_e3;
 BOOL ret_val7 = BOOL_zero;
 FSETAB1569698808 elt_eq_self2;
 ABSTRACT_LAYOUT elt_eq_e12;
 ABSTRACT_LAYOUT elt_eq_e22;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQ981370187 elt_eq_self3;
 ABSTRACT_LAYOUT elt_eq_e13;
 ABSTRACT_LAYOUT elt_eq_e23;
 BOOL ret_val9 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 ABSTRACT_LAYOUT L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 ABSTRACT_LAYOUT L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAB1569698808)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(ABSTRACT_LAYOUT)ARR((FSETAB1569698808)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1559244239) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((ABSTRACT_LAYOUT) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ981370187) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   ret_val5 = (elt_eq_e11==elt_eq_e21);
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAB1569698808)r,h,(ABSTRACT_LAYOUT)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(ABSTRACT_LAYOUT)ARR((FSETAB1569698808)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1559244239) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val7 = (is_elt_nil_e3==((ABSTRACT_LAYOUT) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ981370187) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    ret_val9 = (elt_eq_e13==elt_eq_e23);
    ret_val8 = ret_val9;
    if (ret_val8) {
     SARR((FSETAB1569698808)r,h,(ABSTRACT_LAYOUT)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETAB1569698808)r,h,(ABSTRACT_LAYOUT)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB285451608(FSETAB1569698808 self) {
 FSETAB1569698808 ret_val;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETAB1569698808)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAB246578603;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAB478050731(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAB1569698808)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAB478050731(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAB1569698808 FSETAB478050731(FSETAB1569698808 self) {
 FSETAB1569698808 ret_val;
 FSETAB1569698808 r;
 FSETAB1569698808 L11;
 FSETAB1569698808 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 ABSTRACT_LAYOUT L9;
 L21_=ASIZE((FSETAB1569698808)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAB1862720980(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAB1854241901) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAB1942662686_frame_struct other1_0;
FSETAB1942662686_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAB1942662686(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAB1823289229(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM1135369366(FSETAM1032778315 self, FSETAM1032778315 r, AM_BND1124877163 e) {
 FSETAM1032778315 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM1032778315 elt_eq_self;
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
 BOOL L11_;
 AM_BND1124877163 L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(AM_BND1124877163)ARR((FSETAM1032778315)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ947319992) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_a = ((OB) elt_eq_e21);
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAM1032778315)r,i,(AM_BND1124877163)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAM1032778315)r,L5,(AM_BND1124877163)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM1587889927(FSETAM1032778315 self, INT n) {
 FSETAM1032778315 ret_val;
 FSETAM1032778315 r;
 FSETAM1032778315 L11;
 FSETAM1032778315 elt_nil_self;
 AM_BND1124877163 ret_val1;
 ELT_NI1564018771 elt_nil_self1;
 AM_BND1124877163 ret_val2;
 AM_BND1124877163 e = ((AM_BND1124877163) NULL);
 FSETAM1032778315 elt_nil_self2;
 AM_BND1124877163 ret_val3;
 ELT_NI1564018771 elt_nil_self3;
 AM_BND1124877163 ret_val4;
 AM_BND1124877163 e1 = ((AM_BND1124877163) NULL);
 INT L21 = INT_zero;
 FSETAM1032778315 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AM_BND1124877163 L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAM1032778315_struct)-sizeof(AM_BND1124877163))+(L4)*sizeof(AM_BND1124877163);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAM1032778315_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAM1032778315) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI1564018771) NULL);
 ret_val2 = ((AM_BND1124877163) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_BND1124877163) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAM1032778315)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI1564018771) NULL);
    ret_val4 = ((AM_BND1124877163) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAM1032778315)L11,L21,(AM_BND1124877163)L10); 
    ;
    L121_=INTPLUS(L21,1); 
    L21 = L121_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM1990484536(FSETAM1032778315 self, AM_BND1124877163 e) {
 FSETAM1032778315 ret_val;
 FSETAM1032778315 r;
 FSETAM1032778315 set_initial_stru;
 r = self;
 if ((r==((FSETAM1032778315) NULL))) {
  r = FSETAM1587889927(self, FSETAM687490432);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAM40978445);
 }
 else {
  r = FSETAM990065885(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAM736229142(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM1135369366(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM246540798(FSETAM1032778315 self) {
 FSETAM1032778315 ret_val;
 FSETAM1032778315 r;
 FSETAM1032778315 L11;
 FSETAM1032778315 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_BND1124877163 L9;
 L21_=ASIZE((FSETAM1032778315)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM1587889927(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM1817646066) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM595751729_frame_struct other1_0;
FSETAM595751729_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM595751729(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM1990484536(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM381650379(FSETAM1032778315 self, FSETAM1032778315 s) {
 FSETAM1032778315 ret_val;
 FSETAM1032778315 r;
 FSETAM1032778315 L11;
 AM_BND1124877163 L2;
 r = self;
 {
  struct FSETAM595751729_frame_struct other1_0;
FSETAM595751729_frame noname1 = &other1_0;
  L11 = s;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETAM595751729(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM1990484536(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM736229142(FSETAM1032778315 self, FSETAM1032778315 r, AM_BND1124877163 e) {
 FSETAM1032778315 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AM_BND1124877163 te;
 AM_BND1124877163 te1;
 FSETAM1032778315 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM1032778315 is_elt_nil_self;
 AM_BND1124877163 is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self1;
 AM_BND1124877163 is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM1032778315 elt_eq_self;
 AM_BND1124877163 elt_eq_e1;
 AM_BND1124877163 elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self1;
 AM_BND1124877163 elt_eq_e11;
 AM_BND1124877163 elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 AM_BND1124877163 is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FSETAM1032778315 is_elt_nil_self2;
 AM_BND1124877163 is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI1564018771 is_elt_nil_self3;
 AM_BND1124877163 is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETAM1032778315 elt_eq_self2;
 AM_BND1124877163 elt_eq_e12;
 AM_BND1124877163 elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ947319992 elt_eq_self3;
 AM_BND1124877163 elt_eq_e13;
 AM_BND1124877163 elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 AM_BND1124877163 is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 AM_BND1124877163 L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AM_BND1124877163 L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAM1032778315)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(AM_BND1124877163)ARR((FSETAM1032778315)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_BND1124877163) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ947319992) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_a = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAM1032778315)r,h,(AM_BND1124877163)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(AM_BND1124877163)ARR((FSETAM1032778315)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI1564018771) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((AM_BND1124877163) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ947319992) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_a1 = ((OB) elt_eq_e23);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     SARR((FSETAM1032778315)r,h,(AM_BND1124877163)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETAM1032778315)r,h,(AM_BND1124877163)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM74171957(FSETAM1032778315 self) {
 FSETAM1032778315 ret_val;
 FSETAM1032778315 r = ((FSETAM1032778315) NULL);
 FSETAM1032778315 L11;
 AM_BND1124877163 L2;
 {
  struct FSETAM595751729_frame_struct other1_0;
FSETAM595751729_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L2 = FSETAM595751729(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM1990484536(r, L2);
  }
 }
 after_loop: ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1032778315 FSETAM990065885(FSETAM1032778315 self) {
 FSETAM1032778315 ret_val;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETAM1032778315)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAM34126943;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAM246540798(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAM1032778315)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAM246540798(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM1195182488(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 INT h = INT_zero;
 AM_LOCAL_EXPR te;
 INT i = INT_zero;
 AM_LOCAL_EXPR te1;
 INT hsh = INT_zero;
 FSETAM1158405984 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM1158405984 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FSETAM1158405984 elt_nil_self;
 AM_LOCAL_EXPR ret_val7;
 ELT_NI19483997 elt_nil_self1;
 AM_LOCAL_EXPR ret_val8;
 AM_LOCAL_EXPR e1 = ((AM_LOCAL_EXPR) NULL);
 FSETAM1158405984 is_elt_nil_self2;
 AM_LOCAL_EXPR is_elt_nil_e2;
 BOOL ret_val9 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self3;
 AM_LOCAL_EXPR is_elt_nil_e3;
 BOOL ret_val10 = BOOL_zero;
 FSETAM1158405984 elt_hash_self1;
 OB elt_hash_e1;
 INT ret_val11 = INT_zero;
 FSETAM1158405984 elt_nil_self2;
 AM_LOCAL_EXPR ret_val12;
 ELT_NI19483997 elt_nil_self3;
 AM_LOCAL_EXPR ret_val13;
 AM_LOCAL_EXPR e2 = ((AM_LOCAL_EXPR) NULL);
 FSETAM1158405984 elt_nil_self4;
 AM_LOCAL_EXPR ret_val14;
 ELT_NI19483997 elt_nil_self5;
 AM_LOCAL_EXPR ret_val15;
 AM_LOCAL_EXPR e3 = ((AM_LOCAL_EXPR) NULL);
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 AM_LOCAL_EXPR L71_;
 INT L81_;
 INT L9;
 INT L101_;
 INT L11;
 BOOL L121_;
 INT L131_;
 AM_LOCAL_EXPR L14;
 INT L16;
 INT L171_;
 INT L181_;
 INT L19;
 INT L201_;
 INT L22;
 BOOL L231_;
 INT L241_;
 AM_LOCAL_EXPR L251_;
 INT L26;
 INT L271_;
 INT L28;
 INT L291_;
 INT L30;
 INT L311_;
 BOOL L321_;
 BOOL L33;
 BOOL L341_;
 BOOL L35;
 BOOL L361_;
 BOOL L371_;
 BOOL L38;
 BOOL L391_;
 AM_LOCAL_EXPR L401_;
 AM_LOCAL_EXPR L42;
 AM_LOCAL_EXPR L44;
 BOOL L46;
 BOOL L471_;
 BOOL L48;
 BOOL L491_;
 BOOL L501_;
 AM_LOCAL_EXPR L511_;
 AM_LOCAL_EXPR L52;
 AM_LOCAL_EXPR L54;
 elt_hash_self = self;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FSETAM1158405984)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,h); 
  te = L71_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI19483997) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   ret_val = self;
   return ret_val;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1704040611) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_a = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    goto after_loop;
   }
  }
  L81_=ASIZE((FSETAM1158405984)self); 
  L9 = L81_;
  L101_=INTMINUS(L9,2); 
  L11 = L101_;
  L121_=(h)==(L11); 
  if (L121_) {
   h = 0;
  }
  else {
   L131_=INTPLUS(h,1); 
   h = L131_;
  }
 }
 after_loop: ;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI19483997) NULL);
 ret_val8 = ((AM_LOCAL_EXPR) NULL);
 ret_val7 = ret_val8;
 L14 = ret_val7;
 SARR((FSETAM1158405984)self,h,(AM_LOCAL_EXPR)L14); 
 ;
 L16 = ATTR(self,hsize);
 L171_=INTMINUS(L16,1); 
 SATTR(self,hsize,L171_);
 i = h;
 while (1) {
  L181_=ASIZE((FSETAM1158405984)self); 
  L19 = L181_;
  L201_=INTMINUS(L19,2); 
  L22 = L201_;
  L231_=(i)==(L22); 
  if (L231_) {
   i = 0;
  }
  else {
   L241_=INTPLUS(i,1); 
   i = L241_;
  }
  L251_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,i); 
  te1 = L251_;
  is_elt_nil_self2 = self;
  is_elt_nil_e2 = te1;
  is_elt_nil_self3 = ((ELT_NI19483997) NULL);
  is_elt_nil_e3 = is_elt_nil_e2;
  ret_val10 = (is_elt_nil_e3==((AM_LOCAL_EXPR) NULL));
  ret_val9 = ret_val10;
  if (ret_val9) {
   goto after_loop1;
  }
  elt_hash_self1 = self;
  elt_hash_e1 = ((OB) te1);
  ret_val11 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e1);
  L26 = ret_val11;
  L271_=ASIZE((FSETAM1158405984)self); 
  L28 = L271_;
  L291_=INTMINUS(L28,2); 
  L30 = L291_;
  L311_=L26&L30; 
  hsh = L311_;
  L321_=(i)<(hsh); 
  L33 = L321_;
  L341_=!(L33); 
  if (L341_) {
   L361_=(h)<(i); 
   if (L361_) {
    L371_=(h)<(hsh); 
    L38 = L371_;
    L391_=!(L38); 
    L35 = L391_;
   } else {
    L35 = FALSE;
   }
   if (L35) {
    L401_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,i); 
    L42 = L401_;
    SARR((FSETAM1158405984)self,h,(AM_LOCAL_EXPR)L42); 
    ;
    h = i;
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI19483997) NULL);
    ret_val13 = ((AM_LOCAL_EXPR) NULL);
    ret_val12 = ret_val13;
    L44 = ret_val12;
    SARR((FSETAM1158405984)self,i,(AM_LOCAL_EXPR)L44); 
    ;
   }
  }
  else {
   L471_=(h)<(hsh); 
   L48 = L471_;
   L491_=!(L48); 
   if (L491_) {
    L46 = TRUE;
   } else {
    L501_=(h)<(i); 
    L46 = L501_;
   }
   if (L46) {
    L511_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,i); 
    L52 = L511_;
    SARR((FSETAM1158405984)self,h,(AM_LOCAL_EXPR)L52); 
    ;
    h = i;
    elt_nil_self4 = self;
    elt_nil_self5 = ((ELT_NI19483997) NULL);
    ret_val15 = ((AM_LOCAL_EXPR) NULL);
    ret_val14 = ret_val15;
    L54 = ret_val14;
    SARR((FSETAM1158405984)self,i,(AM_LOCAL_EXPR)L54); 
    ;
   }
  }
 }
 after_loop1: ;
 if (FSETAM1417167536(self)) {
  ret_val = FSETAM7866998(self);
  return ret_val;
 }
 else {
  ret_val = self;
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM2136437804(FSETAM1158405984 self, FSETAM1158405984 r, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 INT asz = INT_zero;
 INT orig_h = INT_zero;
 INT h = INT_zero;
 AM_LOCAL_EXPR te;
 AM_LOCAL_EXPR te1;
 FSETAM1158405984 elt_hash_self;
 OB elt_hash_e;
 INT ret_val1 = INT_zero;
 FSETAM1158405984 is_elt_nil_self;
 AM_LOCAL_EXPR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self1;
 AM_LOCAL_EXPR is_elt_nil_e1;
 BOOL ret_val3 = BOOL_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val5 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val6 = BOOL_zero;
 FSETAM1158405984 is_elt_nil_self2;
 AM_LOCAL_EXPR is_elt_nil_e2;
 BOOL ret_val7 = BOOL_zero;
 ELT_NI19483997 is_elt_nil_self3;
 AM_LOCAL_EXPR is_elt_nil_e3;
 BOOL ret_val8 = BOOL_zero;
 FSETAM1158405984 elt_eq_self2;
 AM_LOCAL_EXPR elt_eq_e12;
 AM_LOCAL_EXPR elt_eq_e22;
 BOOL ret_val9 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self3;
 AM_LOCAL_EXPR elt_eq_e13;
 AM_LOCAL_EXPR elt_eq_e23;
 BOOL ret_val10 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self1;
 OB is_eq_a1;
 BOOL ret_val11 = BOOL_zero;
 INT L11_;
 INT L2;
 INT L31_;
 INT L4;
 INT L51_;
 AM_LOCAL_EXPR L61_;
 INT L81_;
 INT L91_;
 INT L10;
 BOOL L121_;
 AM_LOCAL_EXPR L131_;
 INT L151_;
 INT L17;
 INT L181_;
 L11_=ASIZE((FSETAM1158405984)r); 
 asz = L11_;
 elt_hash_self = r;
 elt_hash_e = ((OB) e);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), elt_hash_e);
 L2 = ret_val1;
 L31_=INTMINUS(asz,2); 
 L4 = L31_;
 L51_=L2&L4; 
 orig_h = L51_;
 h = orig_h;
 while (1) {
  L61_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)r,h); 
  te = L61_;
  is_elt_nil_self = self;
  is_elt_nil_e = te;
  is_elt_nil_self1 = ((ELT_NI19483997) NULL);
  is_elt_nil_e1 = is_elt_nil_e;
  ret_val3 = (is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   elt_eq_self = self;
   elt_eq_e1 = te;
   elt_eq_e2 = e;
   elt_eq_self1 = ((ELT_EQ1704040611) NULL);
   elt_eq_e11 = elt_eq_e1;
   elt_eq_e21 = elt_eq_e2;
   is_eq_self = elt_eq_e11;
   is_eq_a = ((OB) elt_eq_e21);
   ret_val6 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
   ret_val5 = ret_val6;
   ret_val4 = ret_val5;
   if (ret_val4) {
    SARR((FSETAM1158405984)r,h,(AM_LOCAL_EXPR)e); 
    ;
    ret_val = r;
    return ret_val;
   }
  }
  L81_=INTPLUS(h,1); 
  h = L81_;
 }
 after_loop: ;
 L91_=INTMINUS(asz,1); 
 L10 = L91_;
 L121_=(h)==(L10); 
 if (L121_) {
  h = 0;
  while (1) {
   L131_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)r,h); 
   te1 = L131_;
   is_elt_nil_self2 = self;
   is_elt_nil_e2 = te1;
   is_elt_nil_self3 = ((ELT_NI19483997) NULL);
   is_elt_nil_e3 = is_elt_nil_e2;
   ret_val8 = (is_elt_nil_e3==((AM_LOCAL_EXPR) NULL));
   ret_val7 = ret_val8;
   if (ret_val7) {
    goto after_loop1;
   }
   else {
    elt_eq_self2 = self;
    elt_eq_e12 = te1;
    elt_eq_e22 = e;
    elt_eq_self3 = ((ELT_EQ1704040611) NULL);
    elt_eq_e13 = elt_eq_e12;
    elt_eq_e23 = elt_eq_e22;
    is_eq_self1 = elt_eq_e13;
    is_eq_a1 = ((OB) elt_eq_e23);
    ret_val11 = SYSOBEQ(((OB) is_eq_self1),is_eq_a1);
    ret_val10 = ret_val11;
    ret_val9 = ret_val10;
    if (ret_val9) {
     SARR((FSETAM1158405984)r,h,(AM_LOCAL_EXPR)e); 
     ;
     ret_val = r;
     return ret_val;
    }
   }
   L151_=INTPLUS(h,1); 
   h = L151_;
  }
  after_loop1: ;
 }
 SARR((FSETAM1158405984)r,h,(AM_LOCAL_EXPR)e); 
 ;
 L17 = ATTR(r,hsize);
 L181_=INTPLUS(L17,1); 
 SATTR(r,hsize,L181_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM306848596(FSETAM1158405984 self, FSETAM1158405984 r, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 INT i = INT_zero;
 INT sz = INT_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 BOOL L11_;
 AM_LOCAL_EXPR L21_;
 INT L41_;
 INT L5;
 INT L7;
 INT L81_;
 i = 0;
 sz = ATTR(r,hsize);
 while (1) {
  L11_=(i)==(sz); 
  if (L11_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L21_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)r,i); 
  elt_eq_e2 = L21_;
  elt_eq_self1 = ((ELT_EQ1704040611) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_a = ((OB) elt_eq_e21);
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   SARR((FSETAM1158405984)r,i,(AM_LOCAL_EXPR)e); 
   ;
   ret_val = r;
   return ret_val;
  }
  L41_=INTPLUS(i,1); 
  i = L41_;
 }
 after_loop: ;
 L5 = ATTR(r,hsize);
 SARR((FSETAM1158405984)r,L5,(AM_LOCAL_EXPR)e); 
 ;
 L7 = ATTR(r,hsize);
 L81_=INTPLUS(L7,1); 
 SATTR(r,hsize,L81_);
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM430316904(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 INT delete_elt_ind = INT_zero;
 INT hash_table_size = INT_zero;
 INT i = INT_zero;
 INT empty_loc = INT_zero;
 INT second_to_last_i = INT_zero;
 INT next = INT_zero;
 FSETAM1158405984 elt_eq_self;
 AM_LOCAL_EXPR elt_eq_e1;
 AM_LOCAL_EXPR elt_eq_e2;
 BOOL ret_val1 = BOOL_zero;
 ELT_EQ1704040611 elt_eq_self1;
 AM_LOCAL_EXPR elt_eq_e11;
 AM_LOCAL_EXPR elt_eq_e21;
 BOOL ret_val2 = BOOL_zero;
 AM_LOCAL_EXPR is_eq_self;
 OB is_eq_a;
 BOOL ret_val3 = BOOL_zero;
 BOOL L11_;
 BOOL L2;
 BOOL L31_;
 AM_LOCAL_EXPR L41_;
 INT L51_;
 BOOL L6;
 BOOL L71_;
 BOOL L8;
 BOOL L91_;
 INT L10;
 BOOL L121_;
 INT L13;
 INT L141_;
 BOOL L151_;
 INT L161_;
 AM_LOCAL_EXPR L171_;
 AM_LOCAL_EXPR L18;
 INT L20;
 INT L211_;
 delete_elt_ind = -1;
 hash_table_size = ATTR(self,hsize);
 i = 0;
 while (1) {
  L11_=(i)<(hash_table_size); 
  L2 = L11_;
  L31_=!(L2); 
  if (L31_) {
   goto after_loop;
  }
  elt_eq_self = self;
  elt_eq_e1 = e;
  L41_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,i); 
  elt_eq_e2 = L41_;
  elt_eq_self1 = ((ELT_EQ1704040611) NULL);
  elt_eq_e11 = elt_eq_e1;
  elt_eq_e21 = elt_eq_e2;
  is_eq_self = elt_eq_e11;
  is_eq_a = ((OB) elt_eq_e21);
  ret_val3 = SYSOBEQ(((OB) is_eq_self),is_eq_a);
  ret_val2 = ret_val3;
  ret_val1 = ret_val2;
  if (ret_val1) {
   delete_elt_ind = i;
   goto after_loop;
  }
  L51_=INTPLUS(i,1); 
  i = L51_;
 }
 after_loop: ;
 L71_=(delete_elt_ind)<(0); 
 L8 = L71_;
 L91_=!(L8); 
 if (L91_) {
  L10 = ATTR(self,hsize);
  L121_=(delete_elt_ind)<(L10); 
  L6 = L121_;
 } else {
  L6 = FALSE;
 }
 if (L6) {
  empty_loc = delete_elt_ind;
  L13 = ATTR(self,hsize);
  L141_=INTMINUS(L13,2); 
  second_to_last_i = L141_;
  while (1) {
   L151_=(second_to_last_i)<(empty_loc); 
   if (L151_) {
    goto after_loop1;
   }
   L161_=INTPLUS(empty_loc,1); 
   next = L161_;
   L171_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)self,next); 
   L18 = L171_;
   SARR((FSETAM1158405984)self,empty_loc,(AM_LOCAL_EXPR)L18); 
   ;
   empty_loc = next;
  }
  after_loop1: ;
  L20 = ATTR(self,hsize);
  L211_=INTMINUS(L20,1); 
  SATTR(self,hsize,L211_);
 }
 ret_val = self;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM460903522(FSETAM1158405984 self) {
 FSETAM1158405984 ret_val;
 INT L11_;
 INT L2;
 INT L3;
 INT L41_;
 INT L5;
 INT L6;
 INT L71_;
 INT L8;
 BOOL L91_;
 INT L10;
 INT L121_;
 INT L13;
 BOOL L141_;
 BOOL L15;
 BOOL L161_;
 if (ATTR(self,use_map)) {
  L11_=ASIZE((FSETAM1158405984)self); 
  L2 = L11_;
  L3 = ATTR(self,hsize);
  L41_=INTPLUS(L3,1); 
  L5 = L41_;
  L6 = FSETAM584174453;
  L71_=INTTIMES(L5,L6); 
  L8 = L71_;
  L91_=(L2)<(L8); 
  if (L91_) {
   ret_val = FSETAM792944763(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
 else {
  L10 = ATTR(self,hsize);
  L121_=ASIZE((FSETAM1158405984)self); 
  L13 = L121_;
  L141_=(L10)<(L13); 
  L15 = L141_;
  L161_=!(L15); 
  if (L161_) {
   ret_val = FSETAM792944763(self);
   return ret_val;
  }
  else {
   ret_val = self;
   return ret_val;
  }
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM483153547(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 if ((self==((FSETAM1158405984) NULL))) {
  ret_val = ((FSETAM1158405984) NULL);
  return ret_val;
 }
 if (ATTR(self,use_map)) {
  ret_val = FSETAM1195182488(self, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM430316904(self, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM556323666(FSETAM1158405984 self, INT n) {
 FSETAM1158405984 ret_val;
 FSETAM1158405984 r;
 FSETAM1158405984 L11;
 FSETAM1158405984 elt_nil_self;
 AM_LOCAL_EXPR ret_val1;
 ELT_NI19483997 elt_nil_self1;
 AM_LOCAL_EXPR ret_val2;
 AM_LOCAL_EXPR e = ((AM_LOCAL_EXPR) NULL);
 FSETAM1158405984 elt_nil_self2;
 AM_LOCAL_EXPR ret_val3;
 ELT_NI19483997 elt_nil_self3;
 AM_LOCAL_EXPR ret_val4;
 AM_LOCAL_EXPR e1 = ((AM_LOCAL_EXPR) NULL);
 INT L21 = INT_zero;
 FSETAM1158405984 L3;
 INT L4;
 OB L5;
 INT L6;
 BOOL L7;
 BOOL L81_;
 INT L91_br;
 AM_LOCAL_EXPR L10;
 INT L121_;
 L4 = n;
 L6=(sizeof(struct FSETAM1158405984_struct)-sizeof(AM_LOCAL_EXPR))+(L4)*sizeof(AM_LOCAL_EXPR);
 L5=ZALLOC_BIG(L6);
 if (L5==NULL) FATAL("Unable to allocate more memory");
 ((OB)L5)->header.tag=FSETAM1158405984_tag;
#ifdef DESTROY_CHK

   ((OB)L5)->header.destroyed=0;
#endif

 L3 = ((FSETAM1158405984) L5);
 L3->asize = L4;
 r = L3;
 elt_nil_self = self;
 elt_nil_self1 = ((ELT_NI19483997) NULL);
 ret_val2 = ((AM_LOCAL_EXPR) NULL);
 ret_val1 = ret_val2;
 L7 = (ret_val1==((AM_LOCAL_EXPR) NULL));
 L81_=!(L7); 
 if (L81_) {
  {
   BOOL f_L91_ = TRUE;
   /* loop index variable */
   L21 = 0;
   L11 = r;
   L91_br=ASIZE((FSETAM1158405984)L11); 
   while (1) {
    if(L21>=L91_br)  goto after_loop; 
    elt_nil_self2 = self;
    elt_nil_self3 = ((ELT_NI19483997) NULL);
    ret_val4 = ((AM_LOCAL_EXPR) NULL);
    ret_val3 = ret_val4;
    L10 = ret_val3;
    SARR((FSETAM1158405984)L11,L21,(AM_LOCAL_EXPR)L10); 
    ;
    L121_=INTPLUS(L21,1); 
    L21 = L121_;
   }
  }
  after_loop: ;
 }
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM7866998(FSETAM1158405984 self) {
 FSETAM1158405984 ret_val;
 FSETAM1158405984 r;
 FSETAM1158405984 L11;
 FSETAM1158405984 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_LOCAL_EXPR L9;
 L21_=ASIZE((FSETAM1158405984)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTDIV(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM556323666(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM1441852029) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM441739032_frame_struct other1_0;
FSETAM441739032_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM441739032(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM83361925(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM792944763(FSETAM1158405984 self) {
 FSETAM1158405984 ret_val;
 FSETAM1158405984 r;
 FSETAM1158405984 L11;
 FSETAM1158405984 switch_structure;
 BOOL switch_structure1 = BOOL_zero;
 BOOL switch_structure2 = BOOL_zero;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 INT L7;
 INT L81_;
 AM_LOCAL_EXPR L9;
 L21_=ASIZE((FSETAM1158405984)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,1); 
 L5 = L41_;
 L61_=INTTIMES(L5,2); 
 L7 = L61_;
 L81_=INTPLUS(L7,1); 
 r = FSETAM556323666(self, L81_);
 switch_structure = r;
 switch_structure1 = ATTR(self,use_map);
 switch_structure2 = TRUE;
 if (FSETAM1441852029) {
  SATTR(switch_structure,use_map,switch_structure2);
 }
 else {
  SATTR(switch_structure,use_map,switch_structure1);
 }
 {
  struct FSETAM441739032_frame_struct other1_0;
FSETAM441739032_frame noname1 = &other1_0;
  L11 = self;
  noname1->self = L11;
  noname1->state = 0;
  while (1) {
   L9 = FSETAM441739032(noname1);
   if (noname1->state == -1) {
    goto after_loop;
   }
   r = FSETAM83361925(r, L9);
  }
 }
 after_loop: ;
 SYSDESTROY(self); 
 ;
 ret_val = r;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

FSETAM1158405984 FSETAM83361925(FSETAM1158405984 self, AM_LOCAL_EXPR e) {
 FSETAM1158405984 ret_val;
 FSETAM1158405984 r;
 FSETAM1158405984 set_initial_stru;
 r = self;
 if ((r==((FSETAM1158405984) NULL))) {
  r = FSETAM556323666(self, FSETAM2014782744);
  set_initial_stru = r;
  SATTR(set_initial_stru,use_map,FSETAM994490756);
 }
 else {
  r = FSETAM460903522(self);
 }
 if (ATTR(r,use_map)) {
  ret_val = FSETAM2136437804(self, r, e);
  return ret_val;
 }
 else {
  ret_val = FSETAM306848596(self, r, e);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

INT FSETAM1190305782(FSETAM1032778315 self) {
 INT ret_val = INT_zero;
 if ((self==((FSETAM1032778315) NULL))) {
  ret_val = 0;
  return ret_val;
 }
 else {
  ret_val = ATTR(self,hsize);
  return ret_val;
 }
}


#undef IS_ITER
#define IS_ITER 0

STR FMAPST512400025(FMAPSTRdCONFIG self) {
 STR ret_val;
 FSTR res;
 INT i = INT_zero;
 TUPSTRdCONFIG p = TUPSTRdCONFIG_zero;
 STR k;
 dCONFIG e;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val1 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val2 = BOOL_zero;
 STR str_self;
 STR ret_val3;
 FSTR str_self1;
 STR ret_val4;
 extern STR name2;
 INT L11_;
 INT L2;
 BOOL L31_;
 TUPSTRdCONFIG L41_;
 BOOL L5;
 BOOL L61_;
 extern STR name14;
 FSTR L7;
 FSTR L8;
 extern STR name15;
 extern STR name10;
 FSTR L9;
 dCONFIG L10;
 FSTR L12;
 extern STR name10;
 extern STR name10;
 INT L131_;
 extern STR name4;
 res = FSTR_c1307336863(((FSTR) NULL), ((STR) &name2));
 i = 0;
 while (1) {
  L11_=ASIZE((FMAPSTRdCONFIG)self); 
  L2 = L11_;
  L31_=(i)==(L2); 
  if (L31_) {
   goto after_loop;
  }
  L41_=ARR((FMAPSTRdCONFIG)self,i); 
  p = L41_;
  is_key_nil_self = self;
  is_key_nil_e = p.t1;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val2 = (is_elt_nil_e==((STR) NULL));
  ret_val1 = ret_val2;
  L5 = ret_val1;
  L61_=!(L5); 
  if (L61_) {
   k = p.t1;
   e = p.t2;
   L7 = FSTR_p1752847026(res, ((STR) &name14));
   str_self = k;
   ret_val3 = str_self;
   L8 = FSTR_p1752847026(L7, ret_val3);
   res = FSTR_p1752847026(L8, ((STR) &name15));
   L9 = FSTR_p1752847026(res, ((STR) &name10));
   L10 = e;
   L12 = FSTR_p1752847026(L9, (*dCONFIG_strrSTR[TAG(L10)])(L10));
   res = FSTR_p1752847026(L12, ((STR) &name10));
   res = FSTR_p1752847026(res, ((STR) &name10));
  }
  L131_=INTPLUS(i,1); 
  i = L131_;
 }
 after_loop: ;
 res = FSTR_p1752847026(res, ((STR) &name4));
 str_self1 = res;
 ret_val4 = STR_fr1097270334(((STR) NULL), str_self1);
 ret_val = ret_val4;
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

dCONFIG FMAPST558704553(FMAPSTRdCONFIG self, STR k) {
 dCONFIG ret_val;
 INT h = INT_zero;
 STR tk;
 STR tk1;
 FMAPSTRdCONFIG key_hash_self;
 OB key_hash_e;
 INT ret_val1 = INT_zero;
 FMAPSTRdCONFIG is_key_nil_self;
 STR is_key_nil_e;
 BOOL ret_val2 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self;
 STR is_elt_nil_e;
 BOOL ret_val3 = BOOL_zero;
 FMAPSTRdCONFIG key_eq_self;
 STR key_eq_e1;
 STR key_eq_e2;
 BOOL ret_val4 = BOOL_zero;
 ELT_EQSTR elt_eq_self;
 STR elt_eq_e1;
 STR elt_eq_e2;
 BOOL ret_val5 = BOOL_zero;
 FMAPSTRdCONFIG is_key_nil_self1;
 STR is_key_nil_e1;
 BOOL ret_val6 = BOOL_zero;
 ELT_NILSTR is_elt_nil_self1;
 STR is_elt_nil_e1;
 BOOL ret_val7 = BOOL_zero;
 FMAPSTRdCONFIG key_eq_self1;
 STR key_eq_e11;
 STR key_eq_e21;
 BOOL ret_val8 = BOOL_zero;
 ELT_EQSTR elt_eq_self1;
 STR elt_eq_e11;
 STR elt_eq_e21;
 BOOL ret_val9 = BOOL_zero;
 INT L1;
 INT L21_;
 INT L3;
 INT L41_;
 INT L5;
 INT L61_;
 TUPSTRdCONFIG L71_;
 TUPSTRdCONFIG L8;
 TUPSTRdCONFIG L91_;
 TUPSTRdCONFIG L10;
 INT L111_;
 INT L121_;
 INT L13;
 INT L141_;
 INT L15;
 BOOL L161_;
 TUPSTRdCONFIG L171_;
 TUPSTRdCONFIG L18;
 TUPSTRdCONFIG L191_;
 TUPSTRdCONFIG L20;
 INT L221_;
 if ((self==((FMAPSTRdCONFIG) NULL))) {
  ret_val = ((dCONFIG) NULL);
  return ret_val;
 }
 key_hash_self = self;
 key_hash_e = ((OB) k);
 ret_val1 = ELT_HA1612720024(((ELT_HASH) NULL), key_hash_e);
 L1 = ret_val1;
 L21_=ASIZE((FMAPSTRdCONFIG)self); 
 L3 = L21_;
 L41_=INTMINUS(L3,2); 
 L5 = L41_;
 L61_=L1&L5; 
 h = L61_;
 while (1) {
  L71_=ARR((FMAPSTRdCONFIG)self,h); 
  L8=L71_;
  tk = L8.t1;
  is_key_nil_self = self;
  is_key_nil_e = tk;
  is_elt_nil_self = ((ELT_NILSTR) NULL);
  is_elt_nil_e = is_key_nil_e;
  ret_val3 = (is_elt_nil_e==((STR) NULL));
  ret_val2 = ret_val3;
  if (ret_val2) {
   goto after_loop;
  }
  else {
   key_eq_self = self;
   key_eq_e1 = tk;
   key_eq_e2 = k;
   elt_eq_self = ((ELT_EQSTR) NULL);
   elt_eq_e1 = key_eq_e1;
   elt_eq_e2 = key_eq_e2;
   ret_val5 = STR_is111530248(elt_eq_e1, elt_eq_e2);
   ret_val4 = ret_val5;
   if (ret_val4) {
    L91_=ARR((FMAPSTRdCONFIG)self,h); 
    L10=L91_;
    ret_val = L10.t2;
    return ret_val;
   }
  }
  L111_=INTPLUS(h,1); 
  h = L111_;
 }
 after_loop: ;
 L121_=ASIZE((FMAPSTRdCONFIG)self); 
 L13 = L121_;
 L141_=INTMINUS(L13,1); 
 L15 = L141_;
 L161_=(h)==(L15); 
 if (L161_) {
  h = 0;
  while (1) {
   L171_=ARR((FMAPSTRdCONFIG)self,h); 
   L18=L171_;
   tk1 = L18.t1;
   is_key_nil_self1 = self;
   is_key_nil_e1 = tk1;
   is_elt_nil_self1 = ((ELT_NILSTR) NULL);
   is_elt_nil_e1 = is_key_nil_e1;
   ret_val7 = (is_elt_nil_e1==((STR) NULL));
   ret_val6 = ret_val7;
   if (ret_val6) {
    goto after_loop1;
   }
   else {
    key_eq_self1 = self;
    key_eq_e11 = tk1;
    key_eq_e21 = k;
    elt_eq_self1 = ((ELT_EQSTR) NULL);
    elt_eq_e11 = key_eq_e11;
    elt_eq_e21 = key_eq_e21;
    ret_val9 = STR_is111530248(elt_eq_e11, elt_eq_e21);
    ret_val8 = ret_val9;
    if (ret_val8) {
     L191_=ARR((FMAPSTRdCONFIG)self,h); 
     L20=L191_;
     ret_val = L20.t2;
     return ret_val;
    }
   }
   L221_=INTPLUS(h,1); 
   h = L221_;
  }
  after_loop1: ;
 }
 ret_val = ((dCONFIG) NULL);
 return ret_val;
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1219015152(AM_OUT self, AM_UNLOCK_STMT a, INT first1, INT indent11) {
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1286972646(AM_OUT self, AM_HERE_EXPR a, INT first1, INT indent11) {
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1439666796(AM_OUT self, dAM a) {
 AM_OUT860401336(self, a, 0, 0);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1521024147(AM_OUT self, AM_PAR_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR body;
 FILE1 L5;
 OB L6;
 dAM_STMT L8;
 INT L91_;
 INT L10;
 INT L111_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &body);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,body);
 L91_=INTPLUS(indent11,3); 
 L10 = L91_;
 L111_=INTPLUS(indent11,3); 
 AM_OUT860401336(self, ((dAM) L8), L10, L111_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1533629362(AM_OUT self, AM_FORK_STMT a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR at2;
 FILE1 L5;
 OB L6;
 extern STR any3;
 FILE1 L8;
 OB L9;
 dAM_EXPR L11;
 INT L121_;
 extern STR body1;
 FILE1 L13;
 OB L14;
 dAM_STMT L16;
 INT L171_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &at2);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 if ((ATTR(a,at)==((dAM_EXPR) NULL))) {
  create_self2 = ((OUT) NULL);
  ret_val4 = create_self2;
  plus_self2 = ret_val4;
  plus_s2 = ((STR) &any3);
  stdout_self2 = ((FILE1) NULL);
  L9=ZALLOC(sizeof(struct FILE1_struct));
  if (L9==NULL) FATAL("Unable to allocate more memory");
  ((OB)L9)->header.tag=FILE1_tag;
  L8 = ((FILE1) L9);
  r2 = L8;
  
  SATTR(r2,fp,stdout);
  ret_val5 = r2;
  FILE_plus_STR(ret_val5, plus_s2);
 }
 else {
  L11 = ATTR(a,at);
  L121_=INTPLUS(indent11,11); 
  AM_OUT860401336(self, ((dAM) L11), 0, L121_);
 }
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &body1);
 stdout_self3 = ((FILE1) NULL);
 L14=ZALLOC(sizeof(struct FILE1_struct));
 if (L14==NULL) FATAL("Unable to allocate more memory");
 ((OB)L14)->header.tag=FILE1_tag;
 L13 = ((FILE1) L14);
 r3 = L13;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L16 = ATTR(a,body);
 L171_=INTPLUS(indent11,11); 
 AM_OUT860401336(self, ((dAM) L16), 0, L171_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT1830495166(AM_OUT self, AM_PREFETCH_STMT a, INT first1, INT indent11) {
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
 extern STR prefetch1;
 FILE1 L1;
 OB L2;
 AM_LOCAL_EXPR L4;
 INT L51_;
 extern STR name9;
 FILE1 L6;
 OB L7;
 INT L91_;
 extern STR dest;
 FILE1 L10;
 OB L11;
 dAM_EXPR L13;
 INT L141_;
 extern STR name9;
 FILE1 L15;
 OB L16;
 INT L181_;
 extern STR src1;
 FILE1 L19;
 OB L20;
 dAM_EXPR L22;
 INT L231_;
 extern STR name9;
 FILE1 L24;
 OB L25;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &prefetch1);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L4 = ATTR(a,prefetch);
 L51_=INTPLUS(indent11,29); 
 AM_OUT860401336(self, ((dAM) L4), 0, L51_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &name9);
 stdout_self1 = ((FILE1) NULL);
 L7=ZALLOC(sizeof(struct FILE1_struct));
 if (L7==NULL) FATAL("Unable to allocate more memory");
 ((OB)L7)->header.tag=FILE1_tag;
 L6 = ((FILE1) L7);
 r1 = L6;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L91_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L91_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &dest);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L13 = ATTR(a,dest);
 L141_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L13), 0, L141_);
 create_self3 = ((OUT) NULL);
 ret_val6 = create_self3;
 plus_self3 = ret_val6;
 plus_s3 = ((STR) &name9);
 stdout_self3 = ((FILE1) NULL);
 L16=ZALLOC(sizeof(struct FILE1_struct));
 if (L16==NULL) FATAL("Unable to allocate more memory");
 ((OB)L16)->header.tag=FILE1_tag;
 L15 = ((FILE1) L16);
 r3 = L15;
 
 SATTR(r3,fp,stdout);
 ret_val7 = r3;
 FILE_plus_STR(ret_val7, plus_s3);
 L181_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L181_);
 create_self4 = ((OUT) NULL);
 ret_val8 = create_self4;
 plus_self4 = ret_val8;
 plus_s4 = ((STR) &src1);
 stdout_self4 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r4 = L19;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 L22 = ATTR(a,src1);
 L231_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L22), 0, L231_);
 create_self5 = ((OUT) NULL);
 ret_val10 = create_self5;
 plus_self5 = ret_val10;
 plus_s5 = ((STR) &name9);
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

void AM_OUT275623249(AM_OUT self, AM_ANY_EXPR a, INT first1, INT indent11) {
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT438015553(AM_OUT self, AM_WIT1996971603 a, INT first1, INT indent11) {
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
 extern STR name9;
 FILE1 L1;
 OB L2;
 INT L41_;
 extern STR near3;
 FILE1 L5;
 OB L6;
 dAM_STMT L8;
 INT L91_;
 INT L101_;
 extern STR else3;
 FILE1 L11;
 OB L12;
 dAM_STMT L14;
 INT L151_;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L2=ZALLOC(sizeof(struct FILE1_struct));
 if (L2==NULL) FATAL("Unable to allocate more memory");
 ((OB)L2)->header.tag=FILE1_tag;
 L1 = ((FILE1) L2);
 r = L1;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L41_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L41_);
 create_self1 = ((OUT) NULL);
 ret_val2 = create_self1;
 plus_self1 = ret_val2;
 plus_s1 = ((STR) &near3);
 stdout_self1 = ((FILE1) NULL);
 L6=ZALLOC(sizeof(struct FILE1_struct));
 if (L6==NULL) FATAL("Unable to allocate more memory");
 ((OB)L6)->header.tag=FILE1_tag;
 L5 = ((FILE1) L6);
 r1 = L5;
 
 SATTR(r1,fp,stdout);
 ret_val3 = r1;
 FILE_plus_STR(ret_val3, plus_s1);
 L8 = ATTR(a,near_part);
 L91_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L8), 0, L91_);
 L101_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L101_);
 create_self2 = ((OUT) NULL);
 ret_val4 = create_self2;
 plus_self2 = ret_val4;
 plus_s2 = ((STR) &else3);
 stdout_self2 = ((FILE1) NULL);
 L12=ZALLOC(sizeof(struct FILE1_struct));
 if (L12==NULL) FATAL("Unable to allocate more memory");
 ((OB)L12)->header.tag=FILE1_tag;
 L11 = ((FILE1) L12);
 r2 = L11;
 
 SATTR(r2,fp,stdout);
 ret_val5 = r2;
 FILE_plus_STR(ret_val5, plus_s2);
 L14 = ATTR(a,else_part);
 L151_=INTPLUS(indent11,8); 
 AM_OUT860401336(self, ((dAM) L14), 0, L151_);
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT628733655(AM_OUT self, AM_CLUSTER_EXPR a, INT first1, INT indent11) {
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT860401336(AM_OUT self, dAM a, INT first1, INT indent11) {
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
 dSTR plus_s3;
 OUT ret_val7;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val9;
 FILE1 r4;
 AM_OUT AM_COMMENT_STMT_;
 AM_COMMENT_STMT AM_COMMENT_STMT_1;
 INT AM_COMMENT_STMT_2 = INT_zero;
 INT AM_COMMENT_STMT_3 = INT_zero;
 OUT create_self2;
 OUT ret_val10;
 OUT plus_self5;
 STR plus_s5;
 OUT ret_val11;
 FILE1 stdout_self5;
 FILE1 ret_val12;
 FILE1 r5;
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
 AM_OUT AM_BREAK_STMT_ou;
 AM_BREAK_STMT AM_BREAK_STMT_ou1;
 INT AM_BREAK_STMT_ou2 = INT_zero;
 INT AM_BREAK_STMT_ou3 = INT_zero;
 OUT create_self3;
 OUT ret_val16;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val17;
 FILE1 r8;
 AM_OUT AM_INITIAL_STMT_;
 AM_INITIAL_STMT AM_INITIAL_STMT_1;
 INT AM_INITIAL_STMT_2 = INT_zero;
 INT AM_INITIAL_STMT_3 = INT_zero;
 OUT create_self4;
 OUT ret_val18;
 OUT plus_self9;
 STR plus_s9;
 FILE1 stdout_self9;
 FILE1 ret_val19;
 FILE1 r9;
 OUT create_self5;
 OUT ret_val20;
 OUT plus_self10;
 STR plus_s10;
 FILE1 stdout_self10;
 FILE1 ret_val21;
 FILE1 r10;
 AM_OUT AM_INVARIANT_STM;
 AM_INV1167837230 AM_INVARIANT_STM1;
 INT AM_INVARIANT_STM2 = INT_zero;
 INT AM_INVARIANT_STM3 = INT_zero;
 OUT create_self6;
 OUT ret_val22;
 OUT plus_self11;
 STR plus_s11;
 FILE1 stdout_self11;
 FILE1 ret_val23;
 FILE1 r11;
 AM_OUT AM_RAISE_STMT_ou;
 AM_RAISE_STMT AM_RAISE_STMT_ou1;
 INT AM_RAISE_STMT_ou2 = INT_zero;
 INT AM_RAISE_STMT_ou3 = INT_zero;
 OUT create_self7;
 OUT ret_val24;
 OUT plus_self12;
 STR plus_s12;
 FILE1 stdout_self12;
 FILE1 ret_val25;
 FILE1 r12;
 OUT create_self8;
 OUT ret_val26;
 OUT plus_self13;
 STR plus_s13;
 FILE1 stdout_self13;
 FILE1 ret_val27;
 FILE1 r13;
 AM_OUT AM_NEW_EXPR_out_;
 AM_NEW_EXPR AM_NEW_EXPR_out_1;
 INT AM_NEW_EXPR_out_2 = INT_zero;
 INT AM_NEW_EXPR_out_3 = INT_zero;
 OUT create_self9;
 OUT ret_val28;
 OUT plus_self14;
 STR plus_s14;
 FILE1 stdout_self14;
 FILE1 ret_val29;
 FILE1 r14;
 AM_OUT AM_IS_VOID_EXPR_;
 AM_IS_VOID_EXPR AM_IS_VOID_EXPR_1;
 INT AM_IS_VOID_EXPR_2 = INT_zero;
 INT AM_IS_VOID_EXPR_3 = INT_zero;
 OUT create_self10;
 OUT ret_val30;
 OUT plus_self15;
 STR plus_s15;
 FILE1 stdout_self15;
 FILE1 ret_val31;
 FILE1 r15;
 AM_OUT AM_VOID_CONST_ou;
 AM_VOID_CONST AM_VOID_CONST_ou1;
 INT AM_VOID_CONST_ou2 = INT_zero;
 INT AM_VOID_CONST_ou3 = INT_zero;
 OUT create_self11;
 OUT ret_val32;
 OUT plus_self16;
 STR plus_s16;
 FILE1 stdout_self16;
 FILE1 ret_val33;
 FILE1 r16;
 OUT create_self12;
 OUT ret_val34;
 OUT plus_self17;
 STR plus_s17;
 FILE1 stdout_self17;
 FILE1 ret_val35;
 FILE1 r17;
 AM_OUT AM_BOOL_CONST_ou;
 AM_BOOL_CONST AM_BOOL_CONST_ou1;
 INT AM_BOOL_CONST_ou2 = INT_zero;
 INT AM_BOOL_CONST_ou3 = INT_zero;
 OUT create_self13;
 OUT ret_val36;
 OUT plus_self18;
 STR plus_s18;
 FILE1 stdout_self18;
 FILE1 ret_val37;
 FILE1 r18;
 OUT create_self14;
 OUT ret_val38;
 OUT plus_self19;
 STR plus_s19;
 FILE1 stdout_self19;
 FILE1 ret_val39;
 FILE1 r19;
 AM_OUT AM_CHAR_CONST_ou;
 AM_CHAR_CONST AM_CHAR_CONST_ou1;
 INT AM_CHAR_CONST_ou2 = INT_zero;
 INT AM_CHAR_CONST_ou3 = INT_zero;
 OUT create_self15;
 OUT ret_val40;
 OUT plus_self20;
 STR plus_s20;
 OUT ret_val41;
 FILE1 stdout_self20;
 FILE1 ret_val42;
 FILE1 r20;
 OUT plus_self21;
 dSTR plus_s21;
 OUT ret_val43;
 FILE1 stdout_self21;
 FILE1 ret_val44;
 FILE1 r21;
 OUT plus_self22;
 STR plus_s22;
 FILE1 stdout_self22;
 FILE1 ret_val45;
 FILE1 r22;
 AM_OUT AM_STR_CONST_out;
 AM_STR_CONST AM_STR_CONST_out1;
 INT AM_STR_CONST_out2 = INT_zero;
 INT AM_STR_CONST_out3 = INT_zero;
 OUT create_self16;
 OUT ret_val46;
 OUT plus_self23;
 STR plus_s23;
 OUT ret_val47;
 FILE1 stdout_self23;
 FILE1 ret_val48;
 FILE1 r23;
 OUT plus_self24;
 STR plus_s24;
 FILE1 stdout_self24;
 FILE1 ret_val49;
 FILE1 r24;
 AM_OUT AM_INT_CONST_out;
 AM_INT_CONST AM_INT_CONST_out1;
 INT AM_INT_CONST_out2 = INT_zero;
 INT AM_INT_CONST_out3 = INT_zero;
 OUT create_self17;
 OUT ret_val50;
 OUT plus_self25;
 STR plus_s25;
 OUT ret_val51;
 FILE1 stdout_self25;
 FILE1 ret_val52;
 FILE1 r25;
 OUT plus_self26;
 dSTR plus_s26;
 FILE1 stdout_self26;
 FILE1 ret_val53;
 FILE1 r26;
 AM_OUT AM_INTI_CONST_ou;
 AM_INTI_CONST AM_INTI_CONST_ou1;
 INT AM_INTI_CONST_ou2 = INT_zero;
 INT AM_INTI_CONST_ou3 = INT_zero;
 OUT create_self18;
 OUT ret_val54;
 OUT plus_self27;
 STR plus_s27;
 OUT ret_val55;
 FILE1 stdout_self27;
 FILE1 ret_val56;
 FILE1 r27;
 INTI1 str_self;
 STR ret_val57;
 FSTR clear_self;
 FSTR str_self1;
 STR ret_val58;
 OUT plus_self28;
 STR plus_s28;
 FILE1 stdout_self28;
 FILE1 ret_val59;
 FILE1 r28;
 AM_OUT AM_FLT_CONST_out;
 AM_FLT_CONST AM_FLT_CONST_out1;
 INT AM_FLT_CONST_out2 = INT_zero;
 INT AM_FLT_CONST_out3 = INT_zero;
 OUT create_self19;
 OUT ret_val60;
 OUT plus_self29;
 STR plus_s29;
 OUT ret_val61;
 FILE1 stdout_self29;
 FILE1 ret_val62;
 FILE1 r29;
 OUT plus_self30;
 STR plus_s30;
 FILE1 stdout_self30;
 FILE1 ret_val63;
 FILE1 r30;
 AM_OUT AM_FLTD_CONST_ou;
 AM_FLTD_CONST AM_FLTD_CONST_ou1;
 INT AM_FLTD_CONST_ou2 = INT_zero;
 INT AM_FLTD_CONST_ou3 = INT_zero;
 OUT create_self20;
 OUT ret_val64;
 OUT plus_self31;
 STR plus_s31;
 OUT ret_val65;
 FILE1 stdout_self31;
 FILE1 ret_val66;
 FILE1 r31;
 OUT plus_self32;
 STR plus_s32;
 FILE1 stdout_self32;
 FILE1 ret_val67;
 FILE1 r32;
 AM_OUT AM_FLTX_CONST_ou;
 AM_FLTX_CONST AM_FLTX_CONST_ou1;
 INT AM_FLTX_CONST_ou2 = INT_zero;
 INT AM_FLTX_CONST_ou3 = INT_zero;
 OUT create_self21;
 OUT ret_val68;
 OUT plus_self33;
 STR plus_s33;
 OUT ret_val69;
 FILE1 stdout_self33;
 FILE1 ret_val70;
 FILE1 r33;
 OUT plus_self34;
 STR plus_s34;
 FILE1 stdout_self34;
 FILE1 ret_val71;
 FILE1 r34;
 AM_OUT AM_FLTDX_CONST_o;
 AM_FLTDX_CONST AM_FLTDX_CONST_o1;
 INT AM_FLTDX_CONST_o2 = INT_zero;
 INT AM_FLTDX_CONST_o3 = INT_zero;
 OUT create_self22;
 OUT ret_val72;
 OUT plus_self35;
 STR plus_s35;
 OUT ret_val73;
 FILE1 stdout_self35;
 FILE1 ret_val74;
 FILE1 r35;
 OUT plus_self36;
 STR plus_s36;
 FILE1 stdout_self36;
 FILE1 ret_val75;
 FILE1 r36;
 AM_OUT AM_FLTI_CONST_ou;
 AM_FLTI_CONST AM_FLTI_CONST_ou1;
 INT AM_FLTI_CONST_ou2 = INT_zero;
 INT AM_FLTI_CONST_ou3 = INT_zero;
 OUT create_self23;
 OUT ret_val76;
 OUT plus_self37;
 STR plus_s37;
 OUT ret_val77;
 FILE1 stdout_self37;
 FILE1 ret_val78;
 FILE1 r37;
 OUT plus_self38;
 STR plus_s38;
 FILE1 stdout_self38;
 FILE1 ret_val79;
 FILE1 r38;
 AM_OUT AM_SYNC_STMT_out;
 AM_SYNC_STMT AM_SYNC_STMT_out1;
 INT AM_SYNC_STMT_out2 = INT_zero;
 INT AM_SYNC_STMT_out3 = INT_zero;
 OUT create_self24;
 OUT ret_val80;
 OUT plus_self39;
 STR plus_s39;
 FILE1 stdout_self39;
 FILE1 ret_val81;
 FILE1 r39;
 AM_OUT AM_WAITFOR_STMT_;
 AM_WAITFOR_STMT AM_WAITFOR_STMT_1;
 INT AM_WAITFOR_STMT_2 = INT_zero;
 INT AM_WAITFOR_STMT_3 = INT_zero;
 OUT create_self25;
 OUT ret_val82;
 OUT plus_self40;
 STR plus_s40;
 FILE1 stdout_self40;
 FILE1 ret_val83;
 FILE1 r40;
 OUT create_self26;
 OUT ret_val84;
 OUT plus_self41;
 STR plus_s41;
 FILE1 stdout_self41;
 FILE1 ret_val85;
 FILE1 r41;
 AM_OUT AM_WHERE_EXPR_ou;
 AM_WHERE_EXPR AM_WHERE_EXPR_ou1;
 INT AM_WHERE_EXPR_ou2 = INT_zero;
 INT AM_WHERE_EXPR_ou3 = INT_zero;
 OUT create_self27;
 OUT ret_val86;
 OUT plus_self42;
 STR plus_s42;
 FILE1 stdout_self42;
 FILE1 ret_val87;
 FILE1 r42;
 AM_OUT AM_NEAR_EXPR_out;
 AM_NEAR_EXPR AM_NEAR_EXPR_out1;
 INT AM_NEAR_EXPR_out2 = INT_zero;
 INT AM_NEAR_EXPR_out3 = INT_zero;
 OUT create_self28;
 OUT ret_val88;
 OUT plus_self43;
 STR plus_s43;
 FILE1 stdout_self43;
 FILE1 ret_val89;
 FILE1 r43;
 AM_OUT AM_FAR_EXPR_out_;
 AM_FAR_EXPR AM_FAR_EXPR_out_1;
 INT AM_FAR_EXPR_out_2 = INT_zero;
 INT AM_FAR_EXPR_out_3 = INT_zero;
 OUT create_self29;
 OUT ret_val90;
 OUT plus_self44;
 STR plus_s44;
 FILE1 stdout_self44;
 FILE1 ret_val91;
 FILE1 r44;
 AM_OUT AM_AT_EXPR_out_s;
 AM_AT_EXPR AM_AT_EXPR_out_a;
 INT AM_AT_EXPR_out_f = INT_zero;
 INT AM_AT_EXPR_out_i = INT_zero;
 OUT create_self30;
 OUT ret_val92;
 OUT plus_self45;
 STR plus_s45;
 FILE1 stdout_self45;
 FILE1 ret_val93;
 FILE1 r45;
 OUT create_self31;
 OUT ret_val94;
 OUT plus_self46;
 STR plus_s46;
 FILE1 stdout_self46;
 FILE1 ret_val95;
 FILE1 r46;
 AM_OUT AM_CLUSTER_SIZE_;
 AM_CLU1286269335 AM_CLUSTER_SIZE_1;
 INT AM_CLUSTER_SIZE_2 = INT_zero;
 INT AM_CLUSTER_SIZE_3 = INT_zero;
 OUT create_self32;
 OUT ret_val96;
 OUT plus_self47;
 STR plus_s47;
 FILE1 stdout_self47;
 FILE1 ret_val97;
 FILE1 r47;
 extern STR voiddAM;
 FILE1 L1;
 OB L2;
 INT L41_;
 INT L5;
 STR L61_;
 FILE1 L7;
 OB L8;
 extern STR name14;
 FILE1 L10;
 OB L11;
 INT L131_;
 dSTR L14;
 OB L15;
 FILE1 L16;
 OB L17;
 extern STR name53;
 FILE1 L19;
 OB L20;
 extern STR name54;
 FILE1 L22;
 OB L23;
 FILE1 L25;
 OB L26;
 extern STR name55;
 FILE1 L28;
 OB L29;
 extern STR name9;
 FILE1 L32;
 OB L33;
 extern STR name9;
 FILE1 L35;
 OB L36;
 INT L381_;
 extern STR stmts1;
 FILE1 L39;
 OB L40;
 dAM_STMT L43;
 INT L441_;
 extern STR name9;
 FILE1 L45;
 OB L46;
 extern STR val2;
 FILE1 L48;
 OB L49;
 dAM_EXPR L51;
 INT L521_;
 extern STR name9;
 FILE1 L53;
 OB L54;
 extern STR asz1;
 FILE1 L56;
 OB L57;
 dAM_EXPR L59;
 INT L601_;
 extern STR arg1;
 FILE1 L62;
 OB L63;
 dAM_EXPR L65;
 INT L661_;
 extern STR void1;
 FILE1 L67;
 OB L68;
 dTP L70;
 FILE1 L71;
 OB L72;
 extern STR valtrue;
 FILE1 L74;
 OB L75;
 extern STR valfalse;
 FILE1 L77;
 OB L78;
 extern STR bval;
 FILE1 L80;
 OB L81;
 dSTR L83;
 OB L84;
 FILE1 L85;
 OB L86;
 extern STR name42;
 FILE1 L88;
 OB L89;
 extern STR bval1;
 FILE1 L92;
 OB L93;
 FILE1 L95;
 OB L96;
 extern STR val1;
 FILE1 L98;
 OB L99;
 FILE1 L101;
 OB L102;
 extern STR val1;
 FILE1 L104;
 OB L105;
 BOOL L107;
 BOOL L1081_;
 FILE1 L109;
 OB L110;
 extern STR val1;
 FILE1 L112;
 OB L113;
 FILE1 L115;
 OB L116;
 extern STR val1;
 FILE1 L118;
 OB L119;
 FILE1 L122;
 OB L123;
 extern STR val1;
 FILE1 L125;
 OB L126;
 FILE1 L128;
 OB L129;
 extern STR val1;
 FILE1 L132;
 OB L133;
 FILE1 L135;
 OB L136;
 extern STR val1;
 FILE1 L138;
 OB L139;
 FILE1 L141;
 OB L142;
 extern STR name9;
 FILE1 L144;
 OB L145;
 extern STR prefetch1;
 FILE1 L147;
 OB L148;
 AM_LOCAL_EXPR L150;
 INT L1511_;
 extern STR name9;
 FILE1 L152;
 OB L153;
 extern STR expr2;
 FILE1 L155;
 OB L156;
 dAM_EXPR L158;
 INT L1591_;
 extern STR expr2;
 FILE1 L160;
 OB L161;
 dAM_EXPR L163;
 INT L1641_;
 extern STR expr2;
 FILE1 L165;
 OB L166;
 extern STR expr2;
 FILE1 L168;
 OB L169;
 dAM_EXPR L171;
 INT L1721_;
 extern STR at1;
 FILE1 L173;
 OB L174;
 extern STR cluster_size;
 FILE1 L176;
 OB L177;
 dAM_STMT L179;
 BOOL L180;
 BOOL L1821_;
 dAM_STMT L183;
 dAM_STMT L184;
 AM_OUT2087454067(self, first1);
 if ((a==((dAM) NULL))) {
  create_self = ((OUT) NULL);
  ret_val = create_self;
  plus_self = ret_val;
  plus_s = ((STR) &voiddAM);
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
 L41_=SYSTP(a); 
 L5 = L41_;
 L61_=SYSSTRFORTP(L5); 
 plus_s1 = L61_;
 stdout_self1 = ((FILE1) NULL);
 L8=ZALLOC(sizeof(struct FILE1_struct));
 if (L8==NULL) FATAL("Unable to allocate more memory");
 ((OB)L8)->header.tag=FILE1_tag;
 L7 = ((FILE1) L8);
 r1 = L7;
 
 SATTR(r1,fp,stdout);
 ret_val4 = r1;
 FILE_plus_STR(ret_val4, plus_s1);
 ret_val3 = plus_self1;
 plus_self2 = ret_val3;
 plus_s2 = ((STR) &name14);
 stdout_self2 = ((FILE1) NULL);
 L11=ZALLOC(sizeof(struct FILE1_struct));
 if (L11==NULL) FATAL("Unable to allocate more memory");
 ((OB)L11)->header.tag=FILE1_tag;
 L10 = ((FILE1) L11);
 r2 = L10;
 
 SATTR(r2,fp,stdout);
 ret_val6 = r2;
 FILE_plus_STR(ret_val6, plus_s2);
 ret_val5 = plus_self2;
 plus_self3 = ret_val5;
 L131_=SYSID(a); 
 L15=ZALLOC_LEAF(sizeof(struct INT_boxed_struct));
 if (L15==NULL) FATAL("Unable to allocate more memory");
 memset(L15,0,sizeof(struct INT_boxed_struct));
 ((OB)L15)->header.tag=INT_tag;
 L14 = (dSTR)((INT_boxed) L15);
 ((INT_boxed) L14)->immutable_part = L131_;
 plus_s3 = L14;
 stdout_self3 = ((FILE1) NULL);
 L17=ZALLOC(sizeof(struct FILE1_struct));
 if (L17==NULL) FATAL("Unable to allocate more memory");
 ((OB)L17)->header.tag=FILE1_tag;
 L16 = ((FILE1) L17);
 r3 = L16;
 
 SATTR(r3,fp,stdout);
 ret_val8 = r3;
 FILE_plus_dSTR(ret_val8, plus_s3);
 ret_val7 = plus_self3;
 plus_self4 = ret_val7;
 plus_s4 = ((STR) &name53);
 stdout_self4 = ((FILE1) NULL);
 L20=ZALLOC(sizeof(struct FILE1_struct));
 if (L20==NULL) FATAL("Unable to allocate more memory");
 ((OB)L20)->header.tag=FILE1_tag;
 L19 = ((FILE1) L20);
 r4 = L19;
 
 SATTR(r4,fp,stdout);
 ret_val9 = r4;
 FILE_plus_STR(ret_val9, plus_s4);
 switch (TAG(a)) {
  case AM_OB_DEF_tag:
   AM_OUT391785759(self, ((AM_OB_DEF) a), 0, indent11); break;
  case AM_ROUT_DEF_tag:
   AM_OUT991874317(self, ((AM_ROUT_DEF) a), 0, indent11); break;
  case AM_ASSIGN_STMT_tag:
   AM_OUT1399375684(self, ((AM_ASSIGN_STMT) a), 0, indent11); break;
  case AM_COMMENT_STMT_tag:
   AM_COMMENT_STMT_ = self;
   AM_COMMENT_STMT_1 = ((AM_COMMENT_STMT) a);
   AM_COMMENT_STMT_2 = 0;
   AM_COMMENT_STMT_3 = indent11;
   create_self2 = ((OUT) NULL);
   ret_val10 = create_self2;
   plus_self5 = ret_val10;
   plus_s5 = ((STR) &name54);
   stdout_self5 = ((FILE1) NULL);
   L23=ZALLOC(sizeof(struct FILE1_struct));
   if (L23==NULL) FATAL("Unable to allocate more memory");
   ((OB)L23)->header.tag=FILE1_tag;
   L22 = ((FILE1) L23);
   r5 = L22;
   
   SATTR(r5,fp,stdout);
   ret_val12 = r5;
   FILE_plus_STR(ret_val12, plus_s5);
   ret_val11 = plus_self5;
   plus_self6 = ret_val11;
   plus_s6 = ATTR(AM_COMMENT_STMT_1,comment1);
   stdout_self6 = ((FILE1) NULL);
   L26=ZALLOC(sizeof(struct FILE1_struct));
   if (L26==NULL) FATAL("Unable to allocate more memory");
   ((OB)L26)->header.tag=FILE1_tag;
   L25 = ((FILE1) L26);
   r6 = L25;
   
   SATTR(r6,fp,stdout);
   ret_val14 = r6;
   FILE_plus_STR(ret_val14, plus_s6);
   ret_val13 = plus_self6;
   plus_self7 = ret_val13;
   plus_s7 = ((STR) &name55);
   stdout_self7 = ((FILE1) NULL);
   L29=ZALLOC(sizeof(struct FILE1_struct));
   if (L29==NULL) FATAL("Unable to allocate more memory");
   ((OB)L29)->header.tag=FILE1_tag;
   L28 = ((FILE1) L29);
   r7 = L28;
   
   SATTR(r7,fp,stdout);
   ret_val15 = r7;
   FILE_plus_STR(ret_val15, plus_s7); break;
  case AM_IF_STMT_tag:
   AM_OUT1250601520(self, ((AM_IF_STMT) a), 0, indent11); break;
  case AM_LOOP_STMT_tag:
   AM_OUT101602186(self, ((AM_LOOP_STMT) a), 0, indent11); break;
  case AM_BREAK_STMT_tag:
   AM_BREAK_STMT_ou = self;
   AM_BREAK_STMT_ou1 = ((AM_BREAK_STMT) a);
   AM_BREAK_STMT_ou2 = 0;
   AM_BREAK_STMT_ou3 = indent11;
   create_self3 = ((OUT) NULL);
   ret_val16 = create_self3;
   plus_self8 = ret_val16;
   plus_s8 = ((STR) &name9);
   stdout_self8 = ((FILE1) NULL);
   L33=ZALLOC(sizeof(struct FILE1_struct));
   if (L33==NULL) FATAL("Unable to allocate more memory");
   ((OB)L33)->header.tag=FILE1_tag;
   L32 = ((FILE1) L33);
   r8 = L32;
   
   SATTR(r8,fp,stdout);
   ret_val17 = r8;
   FILE_plus_STR(ret_val17, plus_s8); break;
  case AM_RETURN_STMT_tag:
   AM_OUT786574336(self, ((AM_RETURN_STMT) a), 0, indent11); break;
  case AM_YIELD_STMT_tag:
   AM_OUT1496755235(self, ((AM_YIELD_STMT) a), 0, indent11); break;
  case AM_CASE_STMT_tag:
   AM_OUT1555944202(self, ((AM_CASE_STMT) a), 0, indent11); break;
  case AM_TYPECASE_STMT_tag:
   AM_OUT1623210178(self, ((AM_TYPECASE_STMT) a), 0, indent11); break;
  case AM_PRE_STMT_tag:
   AM_OUT359547257(self, ((AM_PRE_STMT) a), 0, indent11); break;
  case AM_POST_STMT_tag:
   AM_OUT1341727046(self, ((AM_POST_STMT) a), 0, indent11); break;
  case AM_INITIAL_STMT_tag:
   AM_INITIAL_STMT_ = self;
   AM_INITIAL_STMT_1 = ((AM_INITIAL_STMT) a);
   AM_INITIAL_STMT_2 = 0;
   AM_INITIAL_STMT_3 = indent11;
   create_self4 = ((OUT) NULL);
   ret_val18 = create_self4;
   plus_self9 = ret_val18;
   plus_s9 = ((STR) &name9);
   stdout_self9 = ((FILE1) NULL);
   L36=ZALLOC(sizeof(struct FILE1_struct));
   if (L36==NULL) FATAL("Unable to allocate more memory");
   ((OB)L36)->header.tag=FILE1_tag;
   L35 = ((FILE1) L36);
   r9 = L35;
   
   SATTR(r9,fp,stdout);
   ret_val19 = r9;
   FILE_plus_STR(ret_val19, plus_s9);
   L381_=INTPLUS(AM_INITIAL_STMT_3,3); 
   AM_OUT2087454067(AM_INITIAL_STMT_, L381_);
   create_self5 = ((OUT) NULL);
   ret_val20 = create_self5;
   plus_self10 = ret_val20;
   plus_s10 = ((STR) &stmts1);
   stdout_self10 = ((FILE1) NULL);
   L40=ZALLOC(sizeof(struct FILE1_struct));
   if (L40==NULL) FATAL("Unable to allocate more memory");
   ((OB)L40)->header.tag=FILE1_tag;
   L39 = ((FILE1) L40);
   r10 = L39;
   
   SATTR(r10,fp,stdout);
   ret_val21 = r10;
   FILE_plus_STR(ret_val21, plus_s10);
   L43 = ATTR(AM_INITIAL_STMT_1,stmts);
   L441_=INTPLUS(AM_INITIAL_STMT_3,9); 
   AM_OUT860401336(AM_INITIAL_STMT_, ((dAM) L43), 0, L441_); break;
  case AM_ASSERT_STMT_tag:
   AM_OUT1191840136(self, ((AM_ASSERT_STMT) a), 0, indent11); break;
  case AM_INV1167837230_tag:
   AM_INVARIANT_STM = self;
   AM_INVARIANT_STM1 = ((AM_INV1167837230) a);
   AM_INVARIANT_STM2 = 0;
   AM_INVARIANT_STM3 = indent11;
   create_self6 = ((OUT) NULL);
   ret_val22 = create_self6;
   plus_self11 = ret_val22;
   plus_s11 = ((STR) &name9);
   stdout_self11 = ((FILE1) NULL);
   L46=ZALLOC(sizeof(struct FILE1_struct));
   if (L46==NULL) FATAL("Unable to allocate more memory");
   ((OB)L46)->header.tag=FILE1_tag;
   L45 = ((FILE1) L46);
   r11 = L45;
   
   SATTR(r11,fp,stdout);
   ret_val23 = r11;
   FILE_plus_STR(ret_val23, plus_s11); break;
  case AM_PROTECT_STMT_tag:
   AM_OUT671570827(self, ((AM_PROTECT_STMT) a), 0, indent11); break;
  case AM_RAISE_STMT_tag:
   AM_RAISE_STMT_ou = self;
   AM_RAISE_STMT_ou1 = ((AM_RAISE_STMT) a);
   AM_RAISE_STMT_ou2 = 0;
   AM_RAISE_STMT_ou3 = indent11;
   create_self7 = ((OUT) NULL);
   ret_val24 = create_self7;
   plus_self12 = ret_val24;
   plus_s12 = ((STR) &val2);
   stdout_self12 = ((FILE1) NULL);
   L49=ZALLOC(sizeof(struct FILE1_struct));
   if (L49==NULL) FATAL("Unable to allocate more memory");
   ((OB)L49)->header.tag=FILE1_tag;
   L48 = ((FILE1) L49);
   r12 = L48;
   
   SATTR(r12,fp,stdout);
   ret_val25 = r12;
   FILE_plus_STR(ret_val25, plus_s12);
   L51 = ATTR(AM_RAISE_STMT_ou1,val1);
   L521_=INTPLUS(AM_RAISE_STMT_ou3,18); 
   AM_OUT860401336(AM_RAISE_STMT_ou, ((dAM) L51), 0, L521_);
   create_self8 = ((OUT) NULL);
   ret_val26 = create_self8;
   plus_self13 = ret_val26;
   plus_s13 = ((STR) &name9);
   stdout_self13 = ((FILE1) NULL);
   L54=ZALLOC(sizeof(struct FILE1_struct));
   if (L54==NULL) FATAL("Unable to allocate more memory");
   ((OB)L54)->header.tag=FILE1_tag;
   L53 = ((FILE1) L54);
   r13 = L53;
   
   SATTR(r13,fp,stdout);
   ret_val27 = r13;
   FILE_plus_STR(ret_val27, plus_s13); break;
  case AM_EXPR_STMT_tag:
   AM_OUT930898334(self, ((AM_EXPR_STMT) a), 0, indent11); break;
  case AM_LOCAL_EXPR_tag:
   AM_OUT1677535099(self, ((AM_LOCAL_EXPR) a), 0, indent11); break;
  case AM_ARRAY_EXPR_tag:
   AM_OUT1924306007(self, ((AM_ARRAY_EXPR) a), 0, indent11); break;
  case AM_BND1124877163_tag:
   AM_OUT523288876(self, ((AM_BND1124877163) a), 0, indent11); break;
  case AM_IF_EXPR_tag:
   AM_OUT959054340(self, ((AM_IF_EXPR) a), 0, indent11); break;
  case AM_NEW_EXPR_tag:
   AM_NEW_EXPR_out_ = self;
   AM_NEW_EXPR_out_1 = ((AM_NEW_EXPR) a);
   AM_NEW_EXPR_out_2 = 0;
   AM_NEW_EXPR_out_3 = indent11;
   create_self9 = ((OUT) NULL);
   ret_val28 = create_self9;
   plus_self14 = ret_val28;
   plus_s14 = ((STR) &asz1);
   stdout_self14 = ((FILE1) NULL);
   L57=ZALLOC(sizeof(struct FILE1_struct));
   if (L57==NULL) FATAL("Unable to allocate more memory");
   ((OB)L57)->header.tag=FILE1_tag;
   L56 = ((FILE1) L57);
   r14 = L56;
   
   SATTR(r14,fp,stdout);
   ret_val29 = r14;
   FILE_plus_STR(ret_val29, plus_s14);
   L59 = ATTR(AM_NEW_EXPR_out_1,asz);
   L601_=INTPLUS(AM_NEW_EXPR_out_3,16); 
   AM_OUT860401336(AM_NEW_EXPR_out_, ((dAM) L59), 0, L601_); break;
  case AM_IS_VOID_EXPR_tag:
   AM_IS_VOID_EXPR_ = self;
   AM_IS_VOID_EXPR_1 = ((AM_IS_VOID_EXPR) a);
   AM_IS_VOID_EXPR_2 = 0;
   AM_IS_VOID_EXPR_3 = indent11;
   create_self10 = ((OUT) NULL);
   ret_val30 = create_self10;
   plus_self15 = ret_val30;
   plus_s15 = ((STR) &arg1);
   stdout_self15 = ((FILE1) NULL);
   L63=ZALLOC(sizeof(struct FILE1_struct));
   if (L63==NULL) FATAL("Unable to allocate more memory");
   ((OB)L63)->header.tag=FILE1_tag;
   L62 = ((FILE1) L63);
   r15 = L62;
   
   SATTR(r15,fp,stdout);
   ret_val31 = r15;
   FILE_plus_STR(ret_val31, plus_s15);
   L65 = ATTR(AM_IS_VOID_EXPR_1,arg);
   L661_=INTPLUS(AM_IS_VOID_EXPR_3,20); 
   AM_OUT860401336(AM_IS_VOID_EXPR_, ((dAM) L65), 0, L661_); break;
  case AM_SHARED_EXPR_tag:
   AM_OUT726258064(self, ((AM_SHARED_EXPR) a), 0, indent11); break;
  case AM_ATTR_EXPR_tag:
   AM_OUT1997333258(self, ((AM_ATTR_EXPR) a), 0, indent11); break;
  case AM_VAT319982528_tag:
   AM_OUT244025178(self, ((AM_VAT319982528) a), 0, indent11); break;
  case AM_VAR744470097_tag:
   AM_OUT347621831(self, ((AM_VAR744470097) a), 0, indent11); break;
  case AM_EXCEPT_EXPR_tag:
   AM_OUT797583920(self, ((AM_EXCEPT_EXPR) a), 0, indent11); break;
  case AM_STMT_EXPR_tag:
   AM_OUT1468881370(self, ((AM_STMT_EXPR) a), 0, indent11); break;
  case AM_CALL_ARG_tag:
   AM_OUT600226455(self, ((AM_CALL_ARG) a), 0, indent11); break;
  case AM_FORMAL_ARG_tag:
   AM_OUT1365214533(self, ((AM_FORMAL_ARG) a), 0, indent11); break;
  case AM_ROU1916046290_tag:
   AM_OUT41050653(self, ((AM_ROU1916046290) a), 0, indent11); break;
  case AM_ITE1809135850_tag:
   AM_OUT732629843(self, ((AM_ITE1809135850) a), 0, indent11); break;
  case AM_BND260301329_tag:
   AM_OUT1614972293(self, ((AM_BND260301329) a), 0, indent11); break;
  case AM_BND367211769_tag:
   AM_OUT1813430155(self, ((AM_BND367211769) a), 0, indent11); break;
  case AM_EXT_CALL_EXPR_tag:
   AM_OUT376898766(self, ((AM_EXT_CALL_EXPR) a), 0, indent11); break;
  case AM_VOID_CONST_tag:
   AM_VOID_CONST_ou = self;
   AM_VOID_CONST_ou1 = ((AM_VOID_CONST) a);
   AM_VOID_CONST_ou2 = 0;
   AM_VOID_CONST_ou3 = indent11;
   if ((ATTR(AM_VOID_CONST_ou1,tp_at)==((dTP) NULL))) {
    create_self11 = ((OUT) NULL);
    ret_val32 = create_self11;
    plus_self16 = ret_val32;
    plus_s16 = ((STR) &void1);
    stdout_self16 = ((FILE1) NULL);
    L68=ZALLOC(sizeof(struct FILE1_struct));
    if (L68==NULL) FATAL("Unable to allocate more memory");
    ((OB)L68)->header.tag=FILE1_tag;
    L67 = ((FILE1) L68);
    r16 = L67;
    
    SATTR(r16,fp,stdout);
    ret_val33 = r16;
    FILE_plus_STR(ret_val33, plus_s16);
   }
   else {
    create_self12 = ((OUT) NULL);
    ret_val34 = create_self12;
    plus_self17 = ret_val34;
    L70 = ATTR(AM_VOID_CONST_ou1,tp_at);
    plus_s17 = (*dTP_strrSTR[TAG(L70)])(L70);
    stdout_self17 = ((FILE1) NULL);
    L72=ZALLOC(sizeof(struct FILE1_struct));
    if (L72==NULL) FATAL("Unable to allocate more memory");
    ((OB)L72)->header.tag=FILE1_tag;
    L71 = ((FILE1) L72);
    r17 = L71;
    
    SATTR(r17,fp,stdout);
    ret_val35 = r17;
    FILE_plus_STR(ret_val35, plus_s17);
   } break;
  case AM_ARR_CONST_tag:
   AM_OUT796864830(self, ((AM_ARR_CONST) a), 0, indent11); break;
  case AM_BOOL_CONST_tag:
   AM_BOOL_CONST_ou = self;
   AM_BOOL_CONST_ou1 = ((AM_BOOL_CONST) a);
   AM_BOOL_CONST_ou2 = 0;
   AM_BOOL_CONST_ou3 = indent11;
   if (ATTR(AM_BOOL_CONST_ou1,val1)) {
    create_self13 = ((OUT) NULL);
    ret_val36 = create_self13;
    plus_self18 = ret_val36;
    plus_s18 = ((STR) &valtrue);
    stdout_self18 = ((FILE1) NULL);
    L75=ZALLOC(sizeof(struct FILE1_struct));
    if (L75==NULL) FATAL("Unable to allocate more memory");
    ((OB)L75)->header.tag=FILE1_tag;
    L74 = ((FILE1) L75);
    r18 = L74;
    
    SATTR(r18,fp,stdout);
    ret_val37 = r18;
    FILE_plus_STR(ret_val37, plus_s18);
   }
   else {
    create_self14 = ((OUT) NULL);
    ret_val38 = create_self14;
    plus_self19 = ret_val38;
    plus_s19 = ((STR) &valfalse);
    stdout_self19 = ((FILE1) NULL);
    L78=ZALLOC(sizeof(struct FILE1_struct));
    if (L78==NULL) FATAL("Unable to allocate more memory");
    ((OB)L78)->header.tag=FILE1_tag;
    L77 = ((FILE1) L78);
    r19 = L77;
    
    SATTR(r19,fp,stdout);
    ret_val39 = r19;
    FILE_plus_STR(ret_val39, plus_s19);
   } break;
  case AM_CHAR_CONST_tag:
   AM_CHAR_CONST_ou = self;
   AM_CHAR_CONST_ou1 = ((AM_CHAR_CONST) a);
   AM_CHAR_CONST_ou2 = 0;
   AM_CHAR_CONST_ou3 = indent11;
   create_self15 = ((OUT) NULL);
   ret_val40 = create_self15;
   plus_self20 = ret_val40;
   plus_s20 = ((STR) &bval);
   stdout_self20 = ((FILE1) NULL);
   L81=ZALLOC(sizeof(struct FILE1_struct));
   if (L81==NULL) FATAL("Unable to allocate more memory");
   ((OB)L81)->header.tag=FILE1_tag;
   L80 = ((FILE1) L81);
   r20 = L80;
   
   SATTR(r20,fp,stdout);
   ret_val42 = r20;
   FILE_plus_STR(ret_val42, plus_s20);
   ret_val41 = plus_self20;
   plus_self21 = ret_val41;
   L84=ZALLOC_LEAF(sizeof(struct CHAR_boxed_struct));
   if (L84==NULL) FATAL("Unable to allocate more memory");
   memset(L84,0,sizeof(struct CHAR_boxed_struct));
   ((OB)L84)->header.tag=CHAR_tag;
   L83 = (dSTR)((CHAR_boxed) L84);
   ((CHAR_boxed) L83)->immutable_part = ATTR(AM_CHAR_CONST_ou1,bval);
   plus_s21 = L83;
   stdout_self21 = ((FILE1) NULL);
   L86=ZALLOC(sizeof(struct FILE1_struct));
   if (L86==NULL) FATAL("Unable to allocate more memory");
   ((OB)L86)->header.tag=FILE1_tag;
   L85 = ((FILE1) L86);
   r21 = L85;
   
   SATTR(r21,fp,stdout);
   ret_val44 = r21;
   FILE_plus_dSTR(ret_val44, plus_s21);
   ret_val43 = plus_self21;
   plus_self22 = ret_val43;
   plus_s22 = ((STR) &name42);
   stdout_self22 = ((FILE1) NULL);
   L89=ZALLOC(sizeof(struct FILE1_struct));
   if (L89==NULL) FATAL("Unable to allocate more memory");
   ((OB)L89)->header.tag=FILE1_tag;
   L88 = ((FILE1) L89);
   r22 = L88;
   
   SATTR(r22,fp,stdout);
   ret_val45 = r22;
   FILE_plus_STR(ret_val45, plus_s22); break;
  case AM_STR_CONST_tag:
   AM_STR_CONST_out = self;
   AM_STR_CONST_out1 = ((AM_STR_CONST) a);
   AM_STR_CONST_out2 = 0;
   AM_STR_CONST_out3 = indent11;
   create_self16 = ((OUT) NULL);
   ret_val46 = create_self16;
   plus_self23 = ret_val46;
   plus_s23 = ((STR) &bval1);
   stdout_self23 = ((FILE1) NULL);
   L93=ZALLOC(sizeof(struct FILE1_struct));
   if (L93==NULL) FATAL("Unable to allocate more memory");
   ((OB)L93)->header.tag=FILE1_tag;
   L92 = ((FILE1) L93);
   r23 = L92;
   
   SATTR(r23,fp,stdout);
   ret_val48 = r23;
   FILE_plus_STR(ret_val48, plus_s23);
   ret_val47 = plus_self23;
   plus_self24 = ret_val47;
   plus_s24 = ATTR(AM_STR_CONST_out1,bval);
   stdout_self24 = ((FILE1) NULL);
   L96=ZALLOC(sizeof(struct FILE1_struct));
   if (L96==NULL) FATAL("Unable to allocate more memory");
   ((OB)L96)->header.tag=FILE1_tag;
   L95 = ((FILE1) L96);
   r24 = L95;
   
   SATTR(r24,fp,stdout);
   ret_val49 = r24;
   FILE_plus_STR(ret_val49, plus_s24); break;
  case AM_INT_CONST_tag:
   AM_INT_CONST_out = self;
   AM_INT_CONST_out1 = ((AM_INT_CONST) a);
   AM_INT_CONST_out2 = 0;
   AM_INT_CONST_out3 = indent11;
   create_self17 = ((OUT) NULL);
   ret_val50 = create_self17;
   plus_self25 = ret_val50;
   plus_s25 = ((STR) &val1);
   stdout_self25 = ((FILE1) NULL);
   L99=ZALLOC(sizeof(struct FILE1_struct));
   if (L99==NULL) FATAL("Unable to allocate more memory");
   ((OB)L99)->header.tag=FILE1_tag;
   L98 = ((FILE1) L99);
   r25 = L98;
   
   SATTR(r25,fp,stdout);
   ret_val52 = r25;
   FILE_plus_STR(ret_val52, plus_s25);
   ret_val51 = plus_self25;
   plus_self26 = ret_val51;
   plus_s26 = ((dSTR) ATTR(AM_INT_CONST_out1,val1));
   stdout_self26 = ((FILE1) NULL);
   L102=ZALLOC(sizeof(struct FILE1_struct));
   if (L102==NULL) FATAL("Unable to allocate more memory");
   ((OB)L102)->header.tag=FILE1_tag;
   L101 = ((FILE1) L102);
   r26 = L101;
   
   SATTR(r26,fp,stdout);
   ret_val53 = r26;
   FILE_plus_dSTR(ret_val53, plus_s26); break;
  case AM_INTI_CONST_tag:
   AM_INTI_CONST_ou = self;
   AM_INTI_CONST_ou1 = ((AM_INTI_CONST) a);
   AM_INTI_CONST_ou2 = 0;
   AM_INTI_CONST_ou3 = indent11;
   create_self18 = ((OUT) NULL);
   ret_val54 = create_self18;
   plus_self27 = ret_val54;
   plus_s27 = ((STR) &val1);
   stdout_self27 = ((FILE1) NULL);
   L105=ZALLOC(sizeof(struct FILE1_struct));
   if (L105==NULL) FATAL("Unable to allocate more memory");
   ((OB)L105)->header.tag=FILE1_tag;
   L104 = ((FILE1) L105);
   r27 = L104;
   
   SATTR(r27,fp,stdout);
   ret_val56 = r27;
   FILE_plus_STR(ret_val56, plus_s27);
   ret_val55 = plus_self27;
   plus_self28 = ret_val55;
   str_self = ATTR(AM_INTI_CONST_ou1,val1);
   clear_self = INTI_buf;
   L107 = (clear_self==((FSTR) NULL));
   L1081_=!(L107); 
   if (L1081_) {
    SATTR(clear_self,loc,0);
   }
   INTI_buf = INTI_s96356965(str_self, INTI_buf);
   str_self1 = INTI_buf;
   ret_val58 = STR_fr1097270334(((STR) NULL), str_self1);
   ret_val57 = ret_val58;
   plus_s28 = ret_val57;
   stdout_self28 = ((FILE1) NULL);
   L110=ZALLOC(sizeof(struct FILE1_struct));
   if (L110==NULL) FATAL("Unable to allocate more memory");
   ((OB)L110)->header.tag=FILE1_tag;
   L109 = ((FILE1) L110);
   r28 = L109;
   
   SATTR(r28,fp,stdout);
   ret_val59 = r28;
   FILE_plus_STR(ret_val59, plus_s28); break;
  case AM_FLT_CONST_tag:
   AM_FLT_CONST_out = self;
   AM_FLT_CONST_out1 = ((AM_FLT_CONST) a);
   AM_FLT_CONST_out2 = 0;
   AM_FLT_CONST_out3 = indent11;
   create_self19 = ((OUT) NULL);
   ret_val60 = create_self19;
   plus_self29 = ret_val60;
   plus_s29 = ((STR) &val1);
   stdout_self29 = ((FILE1) NULL);
   L113=ZALLOC(sizeof(struct FILE1_struct));
   if (L113==NULL) FATAL("Unable to allocate more memory");
   ((OB)L113)->header.tag=FILE1_tag;
   L112 = ((FILE1) L113);
   r29 = L112;
   
   SATTR(r29,fp,stdout);
   ret_val62 = r29;
   FILE_plus_STR(ret_val62, plus_s29);
   ret_val61 = plus_self29;
   plus_self30 = ret_val61;
   plus_s30 = RAT_strrSTR(ATTR(AM_FLT_CONST_out1,val1));
   stdout_self30 = ((FILE1) NULL);
   L116=ZALLOC(sizeof(struct FILE1_struct));
   if (L116==NULL) FATAL("Unable to allocate more memory");
   ((OB)L116)->header.tag=FILE1_tag;
   L115 = ((FILE1) L116);
   r30 = L115;
   
   SATTR(r30,fp,stdout);
   ret_val63 = r30;
   FILE_plus_STR(ret_val63, plus_s30); break;
  case AM_FLTD_CONST_tag:
   AM_FLTD_CONST_ou = self;
   AM_FLTD_CONST_ou1 = ((AM_FLTD_CONST) a);
   AM_FLTD_CONST_ou2 = 0;
   AM_FLTD_CONST_ou3 = indent11;
   create_self20 = ((OUT) NULL);
   ret_val64 = create_self20;
   plus_self31 = ret_val64;
   plus_s31 = ((STR) &val1);
   stdout_self31 = ((FILE1) NULL);
   L119=ZALLOC(sizeof(struct FILE1_struct));
   if (L119==NULL) FATAL("Unable to allocate more memory");
   ((OB)L119)->header.tag=FILE1_tag;
   L118 = ((FILE1) L119);
   r31 = L118;
   
   SATTR(r31,fp,stdout);
   ret_val66 = r31;
   FILE_plus_STR(ret_val66, plus_s31);
   ret_val65 = plus_self31;
   plus_self32 = ret_val65;
   plus_s32 = RAT_strrSTR(ATTR(AM_FLTD_CONST_ou1,val1));
   stdout_self32 = ((FILE1) NULL);
   L123=ZALLOC(sizeof(struct FILE1_struct));
   if (L123==NULL) FATAL("Unable to allocate more memory");
   ((OB)L123)->header.tag=FILE1_tag;
   L122 = ((FILE1) L123);
   r32 = L122;
   
   SATTR(r32,fp,stdout);
   ret_val67 = r32;
   FILE_plus_STR(ret_val67, plus_s32); break;
  case AM_FLTX_CONST_tag:
   AM_FLTX_CONST_ou = self;
   AM_FLTX_CONST_ou1 = ((AM_FLTX_CONST) a);
   AM_FLTX_CONST_ou2 = 0;
   AM_FLTX_CONST_ou3 = indent11;
   create_self21 = ((OUT) NULL);
   ret_val68 = create_self21;
   plus_self33 = ret_val68;
   plus_s33 = ((STR) &val1);
   stdout_self33 = ((FILE1) NULL);
   L126=ZALLOC(sizeof(struct FILE1_struct));
   if (L126==NULL) FATAL("Unable to allocate more memory");
   ((OB)L126)->header.tag=FILE1_tag;
   L125 = ((FILE1) L126);
   r33 = L125;
   
   SATTR(r33,fp,stdout);
   ret_val70 = r33;
   FILE_plus_STR(ret_val70, plus_s33);
   ret_val69 = plus_self33;
   plus_self34 = ret_val69;
   plus_s34 = RAT_strrSTR(ATTR(AM_FLTX_CONST_ou1,val1));
   stdout_self34 = ((FILE1) NULL);
   L129=ZALLOC(sizeof(struct FILE1_struct));
   if (L129==NULL) FATAL("Unable to allocate more memory");
   ((OB)L129)->header.tag=FILE1_tag;
   L128 = ((FILE1) L129);
   r34 = L128;
   
   SATTR(r34,fp,stdout);
   ret_val71 = r34;
   FILE_plus_STR(ret_val71, plus_s34); break;
  case AM_FLTDX_CONST_tag:
   AM_FLTDX_CONST_o = self;
   AM_FLTDX_CONST_o1 = ((AM_FLTDX_CONST) a);
   AM_FLTDX_CONST_o2 = 0;
   AM_FLTDX_CONST_o3 = indent11;
   create_self22 = ((OUT) NULL);
   ret_val72 = create_self22;
   plus_self35 = ret_val72;
   plus_s35 = ((STR) &val1);
   stdout_self35 = ((FILE1) NULL);
   L133=ZALLOC(sizeof(struct FILE1_struct));
   if (L133==NULL) FATAL("Unable to allocate more memory");
   ((OB)L133)->header.tag=FILE1_tag;
   L132 = ((FILE1) L133);
   r35 = L132;
   
   SATTR(r35,fp,stdout);
   ret_val74 = r35;
   FILE_plus_STR(ret_val74, plus_s35);
   ret_val73 = plus_self35;
   plus_self36 = ret_val73;
   plus_s36 = RAT_strrSTR(ATTR(AM_FLTDX_CONST_o1,val1));
   stdout_self36 = ((FILE1) NULL);
   L136=ZALLOC(sizeof(struct FILE1_struct));
   if (L136==NULL) FATAL("Unable to allocate more memory");
   ((OB)L136)->header.tag=FILE1_tag;
   L135 = ((FILE1) L136);
   r36 = L135;
   
   SATTR(r36,fp,stdout);
   ret_val75 = r36;
   FILE_plus_STR(ret_val75, plus_s36); break;
  case AM_FLTI_CONST_tag:
   AM_FLTI_CONST_ou = self;
   AM_FLTI_CONST_ou1 = ((AM_FLTI_CONST) a);
   AM_FLTI_CONST_ou2 = 0;
   AM_FLTI_CONST_ou3 = indent11;
   create_self23 = ((OUT) NULL);
   ret_val76 = create_self23;
   plus_self37 = ret_val76;
   plus_s37 = ((STR) &val1);
   stdout_self37 = ((FILE1) NULL);
   L139=ZALLOC(sizeof(struct FILE1_struct));
   if (L139==NULL) FATAL("Unable to allocate more memory");
   ((OB)L139)->header.tag=FILE1_tag;
   L138 = ((FILE1) L139);
   r37 = L138;
   
   SATTR(r37,fp,stdout);
   ret_val78 = r37;
   FILE_plus_STR(ret_val78, plus_s37);
   ret_val77 = plus_self37;
   plus_self38 = ret_val77;
   plus_s38 = RAT_strrSTR(ATTR(AM_FLTI_CONST_ou1,val1));
   stdout_self38 = ((FILE1) NULL);
   L142=ZALLOC(sizeof(struct FILE1_struct));
   if (L142==NULL) FATAL("Unable to allocate more memory");
   ((OB)L142)->header.tag=FILE1_tag;
   L141 = ((FILE1) L142);
   r38 = L141;
   
   SATTR(r38,fp,stdout);
   ret_val79 = r38;
   FILE_plus_STR(ret_val79, plus_s38); break;
  case AM_ATTACH_STMT_tag:
   AM_OUT435235740(self, ((AM_ATTACH_STMT) a), 0, indent11); break;
  case AM_FORK_STMT_tag:
   AM_OUT1533629362(self, ((AM_FORK_STMT) a), 0, indent11); break;
  case AM_PAR_STMT_tag:
   AM_OUT1521024147(self, ((AM_PAR_STMT) a), 0, indent11); break;
  case AM_LOCK_STMT_tag:
   AM_OUT957177270(self, ((AM_LOCK_STMT) a), 0, indent11); break;
  case AM_UNLOCK_STMT_tag:
   AM_OUT1219015152(self, ((AM_UNLOCK_STMT) a), 0, indent11); break;
  case AM_WIT1996971603_tag:
   AM_OUT438015553(self, ((AM_WIT1996971603) a), 0, indent11); break;
  case AM_SYNC_STMT_tag:
   AM_SYNC_STMT_out = self;
   AM_SYNC_STMT_out1 = ((AM_SYNC_STMT) a);
   AM_SYNC_STMT_out2 = 0;
   AM_SYNC_STMT_out3 = indent11;
   create_self24 = ((OUT) NULL);
   ret_val80 = create_self24;
   plus_self39 = ret_val80;
   plus_s39 = ((STR) &name9);
   stdout_self39 = ((FILE1) NULL);
   L145=ZALLOC(sizeof(struct FILE1_struct));
   if (L145==NULL) FATAL("Unable to allocate more memory");
   ((OB)L145)->header.tag=FILE1_tag;
   L144 = ((FILE1) L145);
   r39 = L144;
   
   SATTR(r39,fp,stdout);
   ret_val81 = r39;
   FILE_plus_STR(ret_val81, plus_s39); break;
  case AM_PREFETCH_STMT_tag:
   AM_OUT1830495166(self, ((AM_PREFETCH_STMT) a), 0, indent11); break;
  case AM_WAITFOR_STMT_tag:
   AM_WAITFOR_STMT_ = self;
   AM_WAITFOR_STMT_1 = ((AM_WAITFOR_STMT) a);
   AM_WAITFOR_STMT_2 = 0;
   AM_WAITFOR_STMT_3 = indent11;
   create_self25 = ((OUT) NULL);
   ret_val82 = create_self25;
   plus_self40 = ret_val82;
   plus_s40 = ((STR) &prefetch1);
   stdout_self40 = ((FILE1) NULL);
   L148=ZALLOC(sizeof(struct FILE1_struct));
   if (L148==NULL) FATAL("Unable to allocate more memory");
   ((OB)L148)->header.tag=FILE1_tag;
   L147 = ((FILE1) L148);
   r40 = L147;
   
   SATTR(r40,fp,stdout);
   ret_val83 = r40;
   FILE_plus_STR(ret_val83, plus_s40);
   L150 = ATTR(AM_WAITFOR_STMT_1,prefetch);
   L1511_=INTPLUS(AM_WAITFOR_STMT_3,29); 
   AM_OUT860401336(AM_WAITFOR_STMT_, ((dAM) L150), 0, L1511_);
   create_self26 = ((OUT) NULL);
   ret_val84 = create_self26;
   plus_self41 = ret_val84;
   plus_s41 = ((STR) &name9);
   stdout_self41 = ((FILE1) NULL);
   L153=ZALLOC(sizeof(struct FILE1_struct));
   if (L153==NULL) FATAL("Unable to allocate more memory");
   ((OB)L153)->header.tag=FILE1_tag;
   L152 = ((FILE1) L153);
   r41 = L152;
   
   SATTR(r41,fp,stdout);
   ret_val85 = r41;
   FILE_plus_STR(ret_val85, plus_s41); break;
  case AM_HERE_EXPR_tag:
   AM_OUT1286972646(self, ((AM_HERE_EXPR) a), 0, indent11); break;
  case AM_ANY_EXPR_tag:
   AM_OUT275623249(self, ((AM_ANY_EXPR) a), 0, indent11); break;
  case AM_WHERE_EXPR_tag:
   AM_WHERE_EXPR_ou = self;
   AM_WHERE_EXPR_ou1 = ((AM_WHERE_EXPR) a);
   AM_WHERE_EXPR_ou2 = 0;
   AM_WHERE_EXPR_ou3 = indent11;
   create_self27 = ((OUT) NULL);
   ret_val86 = create_self27;
   plus_self42 = ret_val86;
   plus_s42 = ((STR) &expr2);
   stdout_self42 = ((FILE1) NULL);
   L156=ZALLOC(sizeof(struct FILE1_struct));
   if (L156==NULL) FATAL("Unable to allocate more memory");
   ((OB)L156)->header.tag=FILE1_tag;
   L155 = ((FILE1) L156);
   r42 = L155;
   
   SATTR(r42,fp,stdout);
   ret_val87 = r42;
   FILE_plus_STR(ret_val87, plus_s42);
   L158 = ATTR(AM_WHERE_EXPR_ou1,arg);
   L1591_=INTPLUS(AM_WHERE_EXPR_ou3,19); 
   AM_OUT860401336(AM_WHERE_EXPR_ou, ((dAM) L158), 0, L1591_); break;
  case AM_NEAR_EXPR_tag:
   AM_NEAR_EXPR_out = self;
   AM_NEAR_EXPR_out1 = ((AM_NEAR_EXPR) a);
   AM_NEAR_EXPR_out2 = 0;
   AM_NEAR_EXPR_out3 = indent11;
   create_self28 = ((OUT) NULL);
   ret_val88 = create_self28;
   plus_self43 = ret_val88;
   plus_s43 = ((STR) &expr2);
   stdout_self43 = ((FILE1) NULL);
   L161=ZALLOC(sizeof(struct FILE1_struct));
   if (L161==NULL) FATAL("Unable to allocate more memory");
   ((OB)L161)->header.tag=FILE1_tag;
   L160 = ((FILE1) L161);
   r43 = L160;
   
   SATTR(r43,fp,stdout);
   ret_val89 = r43;
   FILE_plus_STR(ret_val89, plus_s43);
   L163 = ATTR(AM_NEAR_EXPR_out1,arg);
   L1641_=INTPLUS(AM_NEAR_EXPR_out3,18); 
   AM_OUT860401336(AM_NEAR_EXPR_out, ((dAM) L163), 0, L1641_); break;
  case AM_FAR_EXPR_tag:
   AM_FAR_EXPR_out_ = self;
   AM_FAR_EXPR_out_1 = ((AM_FAR_EXPR) a);
   AM_FAR_EXPR_out_2 = 0;
   AM_FAR_EXPR_out_3 = indent11;
   create_self29 = ((OUT) NULL);
   ret_val90 = create_self29;
   plus_self44 = ret_val90;
   plus_s44 = ((STR) &expr2);
   stdout_self44 = ((FILE1) NULL);
   L166=ZALLOC(sizeof(struct FILE1_struct));
   if (L166==NULL) FATAL("Unable to allocate more memory");
   ((OB)L166)->header.tag=FILE1_tag;
   L165 = ((FILE1) L166);
   r44 = L165;
   
   SATTR(r44,fp,stdout);
   ret_val91 = r44;
   FILE_plus_STR(ret_val91, plus_s44);
   AM_OUT860401336(AM_FAR_EXPR_out_, ((dAM) ATTR(AM_FAR_EXPR_out_1,arg)), 0, 17); break;
  case AM_AT_EXPR_tag:
   AM_AT_EXPR_out_s = self;
   AM_AT_EXPR_out_a = ((AM_AT_EXPR) a);
   AM_AT_EXPR_out_f = 0;
   AM_AT_EXPR_out_i = indent11;
   create_self30 = ((OUT) NULL);
   ret_val92 = create_self30;
   plus_self45 = ret_val92;
   plus_s45 = ((STR) &expr2);
   stdout_self45 = ((FILE1) NULL);
   L169=ZALLOC(sizeof(struct FILE1_struct));
   if (L169==NULL) FATAL("Unable to allocate more memory");
   ((OB)L169)->header.tag=FILE1_tag;
   L168 = ((FILE1) L169);
   r45 = L168;
   
   SATTR(r45,fp,stdout);
   ret_val93 = r45;
   FILE_plus_STR(ret_val93, plus_s45);
   L171 = ATTR(AM_AT_EXPR_out_a,e);
   L1721_=INTPLUS(AM_AT_EXPR_out_i,15); 
   AM_OUT860401336(AM_AT_EXPR_out_s, ((dAM) L171), 0, L1721_);
   create_self31 = ((OUT) NULL);
   ret_val94 = create_self31;
   plus_self46 = ret_val94;
   plus_s46 = ((STR) &at1);
   stdout_self46 = ((FILE1) NULL);
   L174=ZALLOC(sizeof(struct FILE1_struct));
   if (L174==NULL) FATAL("Unable to allocate more memory");
   ((OB)L174)->header.tag=FILE1_tag;
   L173 = ((FILE1) L174);
   r46 = L173;
   
   SATTR(r46,fp,stdout);
   ret_val95 = r46;
   FILE_plus_STR(ret_val95, plus_s46);
   AM_OUT860401336(AM_AT_EXPR_out_s, ((dAM) ATTR(AM_AT_EXPR_out_a,at)), 0, 0); break;
  case AM_CLUSTER_EXPR_tag:
   AM_OUT628733655(self, ((AM_CLUSTER_EXPR) a), 0, indent11); break;
  case AM_CLU1286269335_tag:
   AM_CLUSTER_SIZE_ = self;
   AM_CLUSTER_SIZE_1 = ((AM_CLU1286269335) a);
   AM_CLUSTER_SIZE_2 = 0;
   AM_CLUSTER_SIZE_3 = indent11;
   create_self32 = ((OUT) NULL);
   ret_val96 = create_self32;
   plus_self47 = ret_val96;
   plus_s47 = ((STR) &cluster_size);
   stdout_self47 = ((FILE1) NULL);
   L177=ZALLOC(sizeof(struct FILE1_struct));
   if (L177==NULL) FATAL("Unable to allocate more memory");
   ((OB)L177)->header.tag=FILE1_tag;
   L176 = ((FILE1) L177);
   r47 = L176;
   
   SATTR(r47,fp,stdout);
   ret_val97 = r47;
   FILE_plus_STR(ret_val97, plus_s47); break;
  default: ;
   FATAL("No applicable type in typecase\nin AM_OUT::AM_out($AM,INT,INT)\n./Abstract_Machine/am_out.sa:36:14");
 }
 if (a==NULL) {
  goto other221;
 } else
 switch (TAG(a)) {
  case AM_ASSERT_STMT_tag:
  case AM_ASSIGN_STMT_tag:
  case AM_ATTACH_STMT_tag:
  case AM_BREAK_STMT_tag:
  case AM_CASE_STMT_tag:
  case AM_COMMENT_STMT_tag:
  case AM_EXPR_STMT_tag:
  case AM_FORK_STMT_tag:
  case AM_IF_STMT_tag:
  case AM_INITIAL_STMT_tag:
  case AM_INV1167837230_tag:
  case AM_LOCK_STMT_tag:
  case AM_LOOP_STMT_tag:
  case AM_PAR_STMT_tag:
  case AM_POST_STMT_tag:
  case AM_PREFETCH_STMT_tag:
  case AM_PRE_STMT_tag:
  case AM_PROTECT_STMT_tag:
  case AM_RAISE_STMT_tag:
  case AM_RETURN_STMT_tag:
  case AM_SYNC_STMT_tag:
  case AM_TYPECASE_STMT_tag:
  case AM_UNLOCK_STMT_tag:
  case AM_WAITFOR_STMT_tag:
  case AM_WIT1996971603_tag:
  case AM_YIELD_STMT_tag:
   L179 = ((dAM_STMT) a);
   L180 = ((*dAM_ST775224435[TAG(L179)])(L179)==((dAM_STMT) NULL));
   L1821_=!(L180); 
   if (L1821_) {
    L183 = ((dAM_STMT) a);
    L184 = (*dAM_ST775224435[TAG(L183)])(L183);
    AM_OUT860401336(self, ((dAM) L184), indent11, indent11);
   } break;
  default: ;
  other221: ;
 }
}


#undef IS_ITER
#define IS_ITER 0

void AM_OUT957177270(AM_OUT self, AM_LOCK_STMT a, INT first1, INT indent11) {
 INT i = INT_zero;
 ARRAYdAM_EXPR L11;
 dAM_EXPR lck1;
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
 FLISTdAM_EXPR aget_self;
 INT aget_ind = INT_zero;
 dAM_EXPR ret_val4;
 OUT create_self2;
 OUT ret_val5;
 OUT plus_self2;
 STR plus_s2;
 FILE1 stdout_self2;
 FILE1 ret_val6;
 FILE1 r2;
 OUT create_self3;
 OUT ret_val7;
 OUT plus_self3;
 STR plus_s3;
 FILE1 stdout_self3;
 FILE1 ret_val8;
 FILE1 r3;
 FLISTA1327600838 aget_self1;
 INT aget_ind1 = INT_zero;
 ARRAYdAM_EXPR ret_val9;
 INT L21 = INT_zero;
 OUT create_self4;
 OUT ret_val10;
 OUT plus_self4;
 STR plus_s4;
 FILE1 stdout_self4;
 FILE1 ret_val11;
 FILE1 r4;
 OUT create_self5;
 OUT ret_val12;
 OUT plus_self5;
 STR plus_s5;
 FILE1 stdout_self5;
 FILE1 ret_val13;
 FILE1 r5;
 OUT create_self6;
 OUT ret_val14;
 OUT plus_self6;
 STR plus_s6;
 FILE1 stdout_self6;
 FILE1 ret_val15;
 FILE1 r6;
 FLISTdAM_STMT aget_self2;
 INT aget_ind2 = INT_zero;
 dAM_STMT ret_val16;
 OUT create_self7;
 OUT ret_val17;
 OUT plus_self7;
 STR plus_s7;
 FILE1 stdout_self7;
 FILE1 ret_val18;
 FILE1 r7;
 OUT create_self8;
 OUT ret_val19;
 OUT plus_self8;
 STR plus_s8;
 FILE1 stdout_self8;
 FILE1 ret_val20;
 FILE1 r8;
 extern STR name9;
 FILE1 L3;
 OB L4;
 INT L61_;
 INT L7;
 BOOL L81_;
 extern STR GUARD;
 FILE1 L9;
 OB L10;
 dAM_EXPR L131_;
 extern STR name9;
 FILE1 L14;
 OB L15;
 INT L171_;
 extern STR LOCK11;
 FILE1 L18;
 OB L19;
 dAM_EXPR L23;
 ARRAYdAM_EXPR L241_;
 INT L221_br;
 dAM_EXPR aL221_;
 INT L251_;
 extern STR name29;
 FILE1 L26;
 OB L27;
 INT L291_;
 extern STR name9;
 FILE1 L30;
 OB L31;
 INT L331_;
 extern STR THEN;
 FILE1 L34;
 OB L35;
 dAM_STMT L371_;
 dAM_STMT L38;
 INT L391_;
 extern STR name9;
 FILE1 L40;
 OB L41;
 extern STR ELSE_STMT;
 FILE1 L43;
 OB L44;
 create_self = ((OUT) NULL);
 ret_val = create_self;
 plus_self = ret_val;
 plus_s = ((STR) &name9);
 stdout_self = ((FILE1) NULL);
 L4=ZALLOC(sizeof(struct FILE1_struct));
 if (L4==NULL) FATAL("Unable to allocate more memory");
 ((OB)L4)->header.tag=FILE1_tag;
 L3 = ((FILE1) L4);
 r = L3;
 
 SATTR(r,fp,stdout);
 ret_val1 = r;
 FILE_plus_STR(ret_val1, plus_s);
 L61_=INTPLUS(indent11,3); 
 AM_OUT2087454067(self, L61_);
 i = 0;
 while (1) {
  L7 = FLISTd1406591968(ATTR(a,guards));
  L81_=(i)<(L7); 
  if (L81_) {
  }
  else {
   goto after_loop;
  }
  create_self1 = ((OUT) NULL);
  ret_val2 = create_self1;
  plus_self1 = ret_val2;
  plus_s1 = ((STR) &GUARD);
  stdout_self1 = ((FILE1) NULL);
  L10=ZALLOC(sizeof(struct FILE1_struct));
  if (L10==NULL) FATAL("Unable to allocate more memory");
  ((OB)L10)->header.tag=FILE1_tag;
  L9 = ((FILE1) L10);
  r1 = L9;
  
  SATTR(r1,fp,stdout);
  ret_val3 = r1;
  FILE_plus_STR(ret_val3, plus_s1);
  aget_self = ATTR(a,guards);
  aget_ind = i;
  L131_=(dAM_EXPR)ARR((FLISTdAM_EXPR)aget_self,aget_ind); 
  ret_val4 = L131_;
  AM_OUT860401336(self, ((dAM) ret_val4), 0, 9);
  create_self2 = ((OUT) NULL);
  ret_val5 = create_self2;
  plus_self2 = ret_val5;
  plus_s2 = ((STR) &name9);
  stdout_self2 = ((FILE1) NULL);
  L15=ZALLOC(sizeof(struct FILE1_struct));
  if (L15==NULL) FATAL("Unable to allocate more memory");
  ((OB)L15)->header.tag=FILE1_tag;
  L14 = ((FILE1) L15);
  r2 = L14;
  
  SATTR(r2,fp,stdout);
  ret_val6 = r2;
  FILE_plus_STR(ret_val6, plus_s2);
  L171_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L171_);
  create_self3 = ((OUT) NULL);
  ret_val7 = create_self3;
  plus_self3 = ret_val7;
  plus_s3 = ((STR) &LOCK11);
  stdout_self3 = ((FILE1) NULL);
  L19=ZALLOC(sizeof(struct FILE1_struct));
  if (L19==NULL) FATAL("Unable to allocate more memory");
  ((OB)L19)->header.tag=FILE1_tag;
  L18 = ((FILE1) L19);
  r3 = L18;
  
  SATTR(r3,fp,stdout);
  ret_val8 = r3;
  FILE_plus_STR(ret_val8, plus_s3);
  {
   BOOL f_L221_ = TRUE;
   /* loop index variable */
   L21 = 0;
   while (1) {
    if (f_L221_) {
     f_L221_ = FALSE;
     aget_self1 = ATTR(a,locks1);
     aget_ind1 = i;
     L241_=(ARRAYdAM_EXPR)ARR((FLISTA1327600838)aget_self1,aget_ind1); 
     ret_val9 = L241_;
     L11 = ret_val9;
     L221_br=L11==NULL?0:ASIZE((ARRAYdAM_EXPR)L11); 
    }
    if(L21>=L221_br)  goto after_loop1; 
    aL221_=ARR((ARRAYdAM_EXPR)L11,L21); 
    lck1 = aL221_;
    L251_=INTPLUS(indent11,9); 
    AM_OUT860401336(self, ((dAM) lck1), 0, L251_);
    create_self4 = ((OUT) NULL);
    ret_val10 = create_self4;
    plus_self4 = ret_val10;
    plus_s4 = ((STR) &name29);
    stdout_self4 = ((FILE1) NULL);
    L27=ZALLOC(sizeof(struct FILE1_struct));
    if (L27==NULL) FATAL("Unable to allocate more memory");
    ((OB)L27)->header.tag=FILE1_tag;
    L26 = ((FILE1) L27);
    r4 = L26;
    
    SATTR(r4,fp,stdout);
    ret_val11 = r4;
    FILE_plus_STR(ret_val11, plus_s4);
    L291_=INTPLUS(L21,1); 
    L21 = L291_;
   }
  }
  after_loop1: ;
  create_self5 = ((OUT) NULL);
  ret_val12 = create_self5;
  plus_self5 = ret_val12;
  plus_s5 = ((STR) &name9);
  stdout_self5 = ((FILE1) NULL);
  L31=ZALLOC(sizeof(struct FILE1_struct));
  if (L31==NULL) FATAL("Unable to allocate more memory");
  ((OB)L31)->header.tag=FILE1_tag;
  L30 = ((FILE1) L31);
  r5 = L30;
  
  SATTR(r5,fp,stdout);
  ret_val13 = r5;
  FILE_plus_STR(ret_val13, plus_s5);
  L331_=INTPLUS(indent11,3); 
  AM_OUT2087454067(self, L331_);
  create_self6 = ((OUT) NULL);
  ret_val14 = create_self6;
  plus_self6 = ret_val14;
  plus_s6 = ((STR) &THEN);
  stdout_self6 = ((FILE1) NULL);
  L35=ZALLOC(sizeof(struct FILE1_struct));
  if (L35==NULL) FATAL("Unable to allocate more memory");
  ((OB)L35)->header.tag=FILE1_tag;
  L34 = ((FILE1) L35);
  r6 = L34;
  
  SATTR(r6,fp,stdout);
  ret_val15 = r6;
  FILE_plus_STR(ret_val15, plus_s6);
  aget_self2 = ATTR(a,stmts);
  aget_ind2 = i;
  L371_=(dAM_STMT)ARR((FLISTdAM_STMT)aget_self2,aget_ind2); 
  ret_val16 = L371_;
  L38 = ret_val16;
  L391_=INTPLUS(indent11,9); 
  AM_OUT860401336(self, ((dAM) L38), 0, L391_);
  create_self7 = ((OUT) NULL);
  ret_val17 = create_self7;
  plus_self7 = ret_val17;
  plus_s7 = ((STR) &name9);
  stdout_self7 = ((FILE1) NULL);
  L41=ZALLOC(sizeof(struct FILE1_struct));
  if (L41==NULL) FATAL("Unable to allocate more memory");
  ((OB)L41)->header.tag=FILE1_tag;
  L40 = ((FILE1) L41);
  r7 = L40;
  
  SATTR(r7,fp,stdout);
  ret_val18 = r7;
  FILE_plus_STR(ret_val18, plus_s7);
  i = 2;
 }
 after_loop: ;
 create_self8 = ((OUT) NULL);
 ret_val19 = create_self8;
 plus_self8 = ret_val19;
 plus_s8 = ((STR) &ELSE_STMT);
 stdout_self8 = ((FILE1) NULL);
 L44=ZALLOC(sizeof(struct FILE1_struct));
 if (L44==NULL) FATAL("Unable to allocate more memory");
 ((OB)L44)->header.tag=FILE1_tag;
 L43 = ((FILE1) L44);
 r8 = L43;
 
 SATTR(r8,fp,stdout);
 ret_val20 = r8;
 FILE_plus_STR(ret_val20, plus_s8);
 AM_OUT860401336(self, ((dAM) ATTR(a,else_stmts)), 0, 13);
}


#undef IS_ITER
#define IS_ITER 1

ABSTRACT_LAYOUT FSETAB1139214763(FSETAB1139214763_frame frame) {
 ABSTRACT_LAYOUT dummy = ((ABSTRACT_LAYOUT) NULL);
 ABSTRACT_LAYOUT aI_u_I;
 ABSTRACT_LAYOUT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAB1569698808)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAB1569698808)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

ABSTRACT_LAYOUT FSETAB1942662686(FSETAB1942662686_frame frame) {
 ABSTRACT_LAYOUT dummy = ((ABSTRACT_LAYOUT) NULL);
 BOOL L1;
 BOOL L21_;
 ABSTRACT_LAYOUT L5;
 ABSTRACT_LAYOUT aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 ABSTRACT_LAYOUT L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAB1569698808) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETAB1569698808)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAB1569698808)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1559244239) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((ABSTRACT_LAYOUT) NULL));
     frame->ret_val = frame->ret_val1;
     L7 = frame->ret_val;
     L81_=!(L7); 
     if (L81_) {
      frame->state = 1;
      return frame->r;
      state1:;
     }
     L91_=INTPLUS(frame->L31,1); 
     frame->L31 = L91_;
    }
   }
   after_loop: ;
  }
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(ABSTRACT_LAYOUT)ARR((FSETAB1569698808)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_BND1124877163 FSETAM1934223114(FSETAM1934223114_frame frame) {
 AM_BND1124877163 dummy = ((AM_BND1124877163) NULL);
 AM_BND1124877163 aI_u_I;
 AM_BND1124877163 L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1032778315)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAM1032778315)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_BND1124877163 FSETAM595751729(FSETAM595751729_frame frame) {
 AM_BND1124877163 dummy = ((AM_BND1124877163) NULL);
 BOOL L1;
 BOOL L21_;
 AM_BND1124877163 L5;
 AM_BND1124877163 aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 AM_BND1124877163 L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAM1032778315) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETAM1032778315)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAM1032778315)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI1564018771) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((AM_BND1124877163) NULL));
     frame->ret_val = frame->ret_val1;
     L7 = frame->ret_val;
     L81_=!(L7); 
     if (L81_) {
      frame->state = 1;
      return frame->r;
      state1:;
     }
     L91_=INTPLUS(frame->L31,1); 
     frame->L31 = L91_;
    }
   }
   after_loop: ;
  }
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(AM_BND1124877163)ARR((FSETAM1032778315)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_LOCAL_EXPR FSETAM1413017669(FSETAM1413017669_frame frame) {
 AM_LOCAL_EXPR dummy = ((AM_LOCAL_EXPR) NULL);
 AM_LOCAL_EXPR aI_u_I;
 AM_LOCAL_EXPR L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1158405984)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FSETAM1158405984)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

AM_LOCAL_EXPR FSETAM441739032(FSETAM441739032_frame frame) {
 AM_LOCAL_EXPR dummy = ((AM_LOCAL_EXPR) NULL);
 BOOL L1;
 BOOL L21_;
 AM_LOCAL_EXPR L5;
 AM_LOCAL_EXPR aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 INT L10;
 BOOL L111_;
 AM_LOCAL_EXPR L121_;
 INT L131_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  case 2: goto state2;
  }
 state0:;
 L1 = (frame->self==((FSETAM1158405984) NULL));
 L21_=!(L1); 
 if (L21_) {
  if (ATTR(frame->self,use_map)) {
   {
    /* loop index variable */
    frame->L31 = 0;
    frame->f_L41_ = TRUE;
    while (1) {
     if (frame->f_L41_) {
      frame->f_L41_ = FALSE;
      frame->L61 = frame->self;
      frame->L41_br=ASIZE((FSETAM1158405984)frame->L61); 
     }
     if(frame->L31>=frame->L41_br)  goto after_loop; 
     aL41_=ARR((FSETAM1158405984)frame->L61,frame->L31); 
     frame->r = aL41_;
     frame->is_elt_nil_self = frame->self;
     frame->is_elt_nil_e = frame->r;
     frame->is_elt_nil_self1 = ((ELT_NI19483997) NULL);
     frame->is_elt_nil_e1 = frame->is_elt_nil_e;
     frame->ret_val1 = (frame->is_elt_nil_e1==((AM_LOCAL_EXPR) NULL));
     frame->ret_val = frame->ret_val1;
     L7 = frame->ret_val;
     L81_=!(L7); 
     if (L81_) {
      frame->state = 1;
      return frame->r;
      state1:;
     }
     L91_=INTPLUS(frame->L31,1); 
     frame->L31 = L91_;
    }
   }
   after_loop: ;
  }
  else {
   frame->i = 0;
   frame->sz = ATTR(frame->self,hsize);
   while (1) {
    L10 = ATTR(frame->self,hsize);
    L111_=(frame->i)==(L10); 
    if (L111_) {
     goto after_loop1;
    }
    frame->state = 2;
    L121_=(AM_LOCAL_EXPR)ARR((FSETAM1158405984)frame->self,frame->i); 
    return L121_;
    state2:;
    L131_=INTPLUS(frame->i,1); 
    frame->i = L131_;
   }
   after_loop1: ;
  }
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BOOL ARRAY21272457581(ARRAY21272457581_frame frame) {
 BOOL dummy = BOOL_zero;
 BOOL L3;
 BOOL aL21_;
 INT L51_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  /* loop index variable */
  frame->L11 = 0;
  frame->f_L21_ = TRUE;
  while (1) {
   frame->state = 1;
   if (frame->f_L21_) {
    frame->f_L21_ = FALSE;
    frame->L41 = frame->self;
    frame->L21_br=ASIZE((ARRAY2BOOL)frame->L41); 
   }
   if(frame->L11>=frame->L21_br)  goto after_loop; 
   aL21_=ARR((ARRAY2BOOL)frame->L41,frame->L11); 
   return aL21_;
   state1:;
   L51_=INTPLUS(frame->L11,1); 
   frame->L11 = L51_;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

BOOL ARRAY21774950588(ARRAY21774950588_frame frame) {
 BOOL dummy = BOOL_zero;
 BOOL aI_u_I;
 BOOL L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((ARRAY2BOOL)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((ARRAY2BOOL)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

INT AREFFL1603407436(AREFFL1603407436_frame frame) {
 INT dummy = INT_zero;
 INT rI_u_I;
 INT L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((AREFFL1698233817)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   rI_u_I=frame->i_I_u_I; frame->i_I_u_I++;
   return rI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TUPSTRdCONFIG FMAPST1565675296(FMAPST1565675296_frame frame) {
 TUPSTRdCONFIG dummy = TUPSTRdCONFIG_zero;
 BOOL L1;
 BOOL L21_;
 TUPSTRdCONFIG L5;
 TUPSTRdCONFIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRdCONFIG) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   /* loop index variable */
   frame->L31 = 0;
   frame->f_L41_ = TRUE;
   while (1) {
    if (frame->f_L41_) {
     frame->f_L41_ = FALSE;
     frame->L61 = frame->self;
     frame->L41_br=ASIZE((FMAPSTRdCONFIG)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRdCONFIG)frame->L61,frame->L31); 
    frame->r = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->r.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->r;
     state1:;
    }
    L91_=INTPLUS(frame->L31,1); 
    frame->L31 = L91_;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

TUPSTRdCONFIG FMAPST204994337(FMAPST204994337_frame frame) {
 TUPSTRdCONFIG dummy = TUPSTRdCONFIG_zero;
 TUPSTRdCONFIG aI_u_I;
 TUPSTRdCONFIG L1;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRdCONFIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   frame->state = 1;
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   aI_u_I=ARR((FMAPSTRdCONFIG)frame->self,frame->i_I_u_I); frame->i_I_u_I++;
   return aI_u_I;
   state1:;
  }
 }
 after_loop: ;
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

dCONFIG FMAPST1785271125(FMAPST1785271125_frame frame) {
 dCONFIG dummy = ((dCONFIG) NULL);
 BOOL L1;
 BOOL L21_;
 TUPSTRdCONFIG L5;
 TUPSTRdCONFIG aL41_;
 BOOL L7;
 BOOL L81_;
 INT L91_;
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 L1 = (frame->self==((FMAPSTRdCONFIG) NULL));
 L21_=!(L1); 
 if (L21_) {
  {
   /* loop index variable */
   frame->L31 = 0;
   frame->f_L41_ = TRUE;
   while (1) {
    if (frame->f_L41_) {
     frame->f_L41_ = FALSE;
     frame->L61 = frame->self;
     frame->L41_br=ASIZE((FMAPSTRdCONFIG)frame->L61); 
    }
    if(frame->L31>=frame->L41_br)  goto after_loop; 
    aL41_=ARR((FMAPSTRdCONFIG)frame->L61,frame->L31); 
    frame->e = aL41_;
    frame->is_key_nil_self = frame->self;
    frame->is_key_nil_e = frame->e.t1;
    frame->is_elt_nil_self = ((ELT_NILSTR) NULL);
    frame->is_elt_nil_e = frame->is_key_nil_e;
    frame->ret_val1 = (frame->is_elt_nil_e==((STR) NULL));
    frame->ret_val = frame->ret_val1;
    L7 = frame->ret_val;
    L81_=!(L7); 
    if (L81_) {
     frame->state = 1;
     return frame->e.t2;
     state1:;
    }
    L91_=INTPLUS(frame->L31,1); 
    frame->L31 = L91_;
   }
  }
  after_loop: ;
 }
 frame->state = -1;
 return dummy;
}


#undef IS_ITER
#define IS_ITER 1

void FMAPST1202054525(FMAPST1202054525_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FMAPSTRdCONFIG)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FMAPSTRdCONFIG)frame->self,frame->i_I_u_I,frame->arg1); frame->i_I_u_I++;
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

void FSETAB716509(FSETAB716509_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAB1569698808)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAB1569698808)frame->self,frame->i_I_u_I,(ABSTRACT_LAYOUT)frame->arg1); frame->i_I_u_I++;
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

void FSETAM1225243056(FSETAM1225243056_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1032778315)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAM1032778315)frame->self,frame->i_I_u_I,(AM_BND1124877163)frame->arg1); frame->i_I_u_I++;
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

void FSETAM1474900765(FSETAM1474900765_frame frame) {
 switch (frame->state) {
  case 0: goto state0;
  case 1: goto state1;
  }
 state0:;
 {
  frame->I_u_Ibr=ASIZE((FSETAM1158405984)frame->self); 
  frame->i_I_u_I=0;
  frame->f_I_u_I = TRUE;
  while (1) {
   if(frame->i_I_u_I>=frame->I_u_Ibr)  goto after_loop; 
   SARR((FSETAM1158405984)frame->self,frame->i_I_u_I,(AM_LOCAL_EXPR)frame->arg1); frame->i_I_u_I++;
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
